;;; test-niwa-view.el -- Tests for niwa-view -*- lexical-binding: t -*-

;;; Commentary:

;; Contract tests for chat-view coordination and shared surface behavior.

;;; Code:

(require 'niwa-session-test-support
         (file-name-concat (file-name-directory
                            (or buffer-file-name load-file-name byte-compile-current-file))
                           "niwa-session-test-support"))

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name
               load-file-name
               byte-compile-current-file))
          "helpers"))
(require 'niwa-agent-control)
(require 'niwa-agents)
(require 'niwa-claude-code-backend)
(require 'niwa-compact)
(require 'niwa-execution)
(require 'niwa-execution-target)
(require 'niwa-background-list)
(require 'niwa-gptel-stream-bridge)
(require 'niwa-view)
(require 'niwa-view-render)
(require 'niwa-view-segments)
(require 'niwa-view-stream)
(require 'niwa-menu)
(require 'niwa-transport)
(require 'niwa-structs)
(require 'niwa-skills-core)
(require 'niwa-workspace)
(require 'niwa-plan-mode)
(require 'niwa-session-persistence)
(require 'niwa-session-artifacts)
(require 'niwa-session-publication)
(require 'niwa-session-recovery)
(require 'niwa-permission-queue)

(defvar org-mode-hook)
(declare-function gptel-menu "ext:gptel-transient" ())
(declare-function org-entry-put "org" (pom property value))


;;
;;; Activation

(defun niwa-view-test--publish (session path body)
  "Commit archived fixture BODY at PATH under SESSION's real lease."
  (niwa-session-publication-publish
   session
   (list (list :path path :content body)
         (list :path (file-name-concat (niwa-session-save-path session) "session.meta.el")
               :content (niwa-session-artifacts-printed-value
                         (niwa-session-codec-serialize session))
               :commit-marker t))))

(niwa-deftest niwa-view--status-recovery-body ()
  ,test
  (test)
  :doc "blocking notices do not offer dismissal"
  (should (equal "niwa [workspace unavailable]: Sign in"
                 (niwa-view--status-recovery-body
                  (niwa-session--create)
                  '(:message "Sign in" :blocking t))))

  :doc "dismissal through the rendered control preserves the draft and agent result"
  (niwa-view-test--with-buffers
    (let* ((record (niwa-agent-record--create
                    :path "/root/explore" :settled-outcome 'errored
                    :settled-result "Provider failed"))
           (session (niwa-session--create
                     :name "main" :root-buffer data-buf
                     :agent-registry (list (cons "/root/explore" record)))))
      (with-current-buffer data-buf
        (setq-local niwa--session session))
      (with-current-buffer view-buf
        (setq-local niwa--session session)
        (niwa-view-test--insert-composer-draft
         "> first line\nsecond line" (- (length "> first line\nsecond line") 4))
        (let ((offset (niwa-view-test--composer-point-offset)))
          (niwa-recovery-report session "agent:a" 'request "Agent failed" nil)
          (should (= offset (niwa-view-test--composer-point-offset)))
          (should (equal "> first line\nsecond line" (niwa-view-composer-draft-text))))
        (goto-char (point-min))
        (search-forward "[Dismiss]")
        (backward-char 2)
        (should (get-text-property (point) 'read-only))
        (should (button-at (point)))
        (let ((last-command-event ?\r))
          (call-interactively (key-binding (kbd "RET"))))
        (should-not (niwa-session-recovery-issues session))
        (should-not (string-search "Agent failed" (buffer-string)))
        (should (equal "> first line\nsecond line" (niwa-view-composer-draft-text))))
      (should (eq 'errored (niwa-agent-record-settled-outcome record)))
      (should (equal "Provider failed" (niwa-agent-record-settled-result record))))))

(niwa-deftest niwa-view-activate-at-point
  (:doc "keyboard activation keeps current buffer; mouse uses clicked window")
  (progn
    ;; Keyboard events must not consult the selected window.  A caller can
    ;; activate a rendered control in another current buffer, as async views and
    ;; batch callers do.  `eventp' also accepts these character events.
    (niwa-view-test--with-buffers
      (with-current-buffer view-buf
        (let ((inhibit-read-only t) activated)
          (goto-char (point-min))
          (insert (propertize
                   "Action\n" 'keymap niwa-view--display-map
                   'niwa-view-zone-activate (lambda () (setq activated t))))
          (dolist (event '(13 11 nil))
            (goto-char (point-min))
            (setq activated nil)
            (let ((last-nonmenu-event event))
              (call-interactively (key-binding (kbd "RET"))))
            (should (eq (current-buffer) view-buf))
            (should activated))
          (remove-text-properties
           (point-min) (+ (point-min) 7) '(niwa-view-zone-activate nil))
          (dolist (event '(13 11 nil))
            (let ((last-nonmenu-event event))
              (should-error (call-interactively (key-binding (kbd "RET")))
                            :type 'user-error)))
          ;; An unadorned display click remains harmless.
          (save-window-excursion
            (switch-to-buffer view-buf)
            (niwa-view-activate-at-point
             (list 'mouse-1 (list (selected-window) (point-min) '(0 . 0) 0)))))))
    (let ((target (generate-new-buffer " *test-click-target*"))
          opened)
      (unwind-protect
          (progn
            (with-current-buffer target
              (insert "xagent")
              (put-text-property
               2 7 'niwa-view-agent-path "/root/test"))
            (cl-letf
                (((symbol-function 'niwa-view--event-position)
                  (lambda (&optional _event) 2))
                 ((symbol-function 'mouse-set-point)
                  (lambda (_event)
                    (set-buffer target)
                    (goto-char 2)))
                 ((symbol-function 'niwa-view--position-in-input-region-p)
                  (lambda (_position) nil))
                 ((symbol-function 'niwa-view-open-agent-transcript-at-point)
                  (lambda (&optional _event)
                    (setq opened
                          (get-text-property
                           (point) 'niwa-view-agent-path)))))
              (with-temp-buffer
                (niwa-view-activate-at-point 'mouse)))
            (should (equal "/root/test" opened)))
        (when (buffer-live-p target)
          (kill-buffer target))))))


;;
;;; Rendering

;; `niwa-gptel-stream-bridge'
(defvar niwa-gptel-stream-bridge-flushing)

(niwa-deftest niwa-view--schedule-render
  ()
  ,test
  (test)
  :doc "coalesces stream, tool-boundary, and full requests into one refresh"
  (niwa-view-test--with-buffers
    (let ((niwa-view-stream-render-delay 1)
          (niwa-view-tool-boundary-render-delay 1)
          (niwa-view-rerender-debounce 1)
          (scheduled 0) callback args fake-timers
          (incremental-count 0)
          (full-count 0))
      (with-current-buffer view-buf
        (setq niwa-view--in-flight-turn-start
              (copy-marker (niwa-view-transcript-tail-marker)))
        (setq niwa-view--data-turn-start
              (with-current-buffer data-buf (copy-marker (point-min)))))
      (unwind-protect
          ;; The mock must arm a real far-future timer: scheduling now
          ;; tests presence on `timer-list', so a bare placeholder value
          ;; would read as a dropped timer and defeat the coalescing.
          (cl-letf* ((real-run-at-time (symbol-function 'run-at-time))
                     ((symbol-function 'run-at-time)
                      (lambda (_delay _repeat function &rest function-args)
                        (cl-incf scheduled)
                        (setq callback function
                              args function-args)
                        (car (push (apply real-run-at-time 3600 nil
                                          function function-args)
                                   fake-timers))))
                     ((symbol-function 'niwa-view--render-stream-update)
                      (lambda (_data-buffer) (cl-incf incremental-count)))
                     ((symbol-function 'niwa-view-render-batched-full)
                      (lambda () (cl-incf full-count))))
            (with-current-buffer data-buf
              (niwa-view-stream-schedule))
            (with-current-buffer view-buf
              (niwa-view--schedule-tool-boundary-render data-buf))
            (niwa-view-rerender view-buf)
            (should (= 1 scheduled))
            (with-current-buffer view-buf
              (should (eq 'full niwa-view--pending-render-kind)))
            (apply callback args)
            (should (= 1 full-count))
            (should (= 0 incremental-count))
            (with-current-buffer view-buf
              (should-not niwa-view--render-timer)
              (should-not niwa-view--pending-render-kind))
            (setq callback nil args nil)
            (with-current-buffer data-buf
              (niwa-view-stream-schedule))
            (with-current-buffer view-buf
              (niwa-view--schedule-tool-boundary-render data-buf))
            (should (= 2 scheduled))
            (apply callback args)
            (should (= 1 full-count))
            (should (= 1 incremental-count)))
        (mapc #'cancel-timer fake-timers))))

  :doc "renders a flushed stream batch in its own timer's pass"
  (niwa-view-test--with-buffers
    (let ((niwa-view-stream-render-delay 1)
          (incremental-count 0)
          delays callback)
      (with-current-buffer view-buf
        (setq niwa-view--in-flight-turn-start
              (copy-marker (niwa-view-transcript-tail-marker)))
        (setq niwa-view--data-turn-start
              (with-current-buffer data-buf (copy-marker (point-min)))))
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (delay _repeat function &rest _)
                   (push delay delays)
                   (setq callback function)
                   'timer))
                ((symbol-function 'niwa-view--render-stream-update)
                 (lambda (_data-buffer) (cl-incf incremental-count))))
        ;; In the batch's own timer, the render joins that timer's pass.
        (let ((after (list nil)))
          (with-current-buffer data-buf
            (let ((niwa-gptel-stream-bridge-flushing t)
                  (niwa-gptel-stream-bridge--after-flush after))
              (niwa-view-stream-schedule)))
          (should-not delays)
          (should (= 1 (length (car after))))
          (funcall (caar after))
          (should (= 1 incremental-count)))
        ;; A flush elsewhere renders from a zero-delay timer.
        (with-current-buffer data-buf
          (let ((niwa-gptel-stream-bridge-flushing t))
            (niwa-view-stream-schedule)))
        (should (equal delays '(0)))
        (should (= 1 incremental-count))
        (funcall callback)
        (should (= 2 incremental-count)))))

  :doc "defers timer flushes while a remote operation is already in flight"
  (niwa-view-test--with-buffers
    (let ((niwa-view-rerender-debounce 1)
          callback args fake-timers
          (scheduled 0)
          (full-count 0))
      (unwind-protect
          (cl-letf* ((real-run-at-time (symbol-function 'run-at-time))
                     ((symbol-function 'run-at-time)
                      (lambda (_delay _repeat function &rest function-args)
                        (cl-incf scheduled)
                        (setq callback function
                              args function-args)
                        (car (push (apply real-run-at-time 3600 nil
                                          function function-args)
                                   fake-timers))))
                     ((symbol-function 'niwa-view-render-batched-full)
                      (lambda () (cl-incf full-count))))
            (niwa-view-rerender view-buf)
            ;; A real handler frame, because that is what a render timer lands
            ;; inside.  It must not test `tramp-current-connection': that stays
            ;; set for the life of the process once any remote file is touched,
            ;; which postponed every remote render forever.
            (niwa-transport--handler-advice
             (lambda (&rest _) (apply callback args))
             'file-exists-p "/ssh:user@host:/srv/x")
            (should (= 2 scheduled))
            (should (= 0 full-count))
            (with-current-buffer view-buf
              (should (eq 'full niwa-view--pending-render-kind)))
            (apply callback args)
            (should (= 1 full-count)))
        (mapc #'cancel-timer fake-timers))))

  :doc "re-arms when the recorded timer is no longer on `timer-list'"
  (niwa-view-test--with-buffers
    (let ((niwa-view-rerender-debounce 1)
          (scheduled 0)
          lost)
      ;; Arm a timer on a discarded `timer-list' binding: the shape TRAMP's
      ;; suspended-timers window leaves behind when a stream or tool hook
      ;; schedules a render inside it.
      (let (timer-list)
        (setq lost (run-at-time 3600 nil #'ignore)))
      (should (timerp lost))
      (with-current-buffer view-buf
        (setq niwa-view--render-timer lost))
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (&rest _) (cl-incf scheduled) 'scheduled)))
        (niwa-view-rerender view-buf))
      (should (= 1 scheduled)))))

(niwa-deftest niwa-view--flush-scheduled-render ()
  ,test
  (test)
  :doc "keeps historical projection fixed while refreshing live chrome"
  (niwa-view-test--with-buffers
    (let ((full-count 0)
          (incremental-count 0)
          (chrome-count 0))
      (with-current-buffer view-buf
        (setq-local niwa-view--pending-render-kind 'full
                    niwa-view--pending-render-data-buffer data-buf))
      (cl-letf
          (((symbol-function 'niwa-view-historical-segment-p)
            (lambda () t))
           ((symbol-function 'niwa-view-render-batched-full)
            (lambda (&rest _) (cl-incf full-count)))
           ((symbol-function 'niwa-view--render-stream-update)
            (lambda (&rest _) (cl-incf incremental-count)))
           ((symbol-function 'niwa-view--render-status)
            (lambda (&rest _) (cl-incf chrome-count)))
           ((symbol-function 'niwa-view--interaction-rebuild)
            (lambda () (cl-incf chrome-count)))
           ((symbol-function 'niwa-view--ensure-request-progress)
            (lambda (&rest _) (cl-incf chrome-count))))
        (niwa-view--flush-scheduled-render view-buf))
      (with-current-buffer view-buf
        (should-not niwa-view--pending-render-kind))
      (should (= 0 full-count))
      (should (= 0 incremental-count))
      (should (= 3 chrome-count))))

  :doc "an unattended view keeps its pending render until focus returns"
  (niwa-view-test--with-buffers
    (let ((full-count 0)
          (unattended t)
          delay scheduled)
      (with-current-buffer view-buf
        (setq-local niwa-view--pending-render-kind 'full
                    niwa-view--pending-render-data-buffer data-buf))
      (unwind-protect
          (cl-letf* ((real-run-at-time (symbol-function 'run-at-time))
                     ((symbol-function 'niwa-view--unattended-p)
                      (lambda (&rest _) unattended))
                     ((symbol-function 'run-at-time)
                      (lambda (seconds _repeat fn &rest args)
                        (setq delay seconds
                              scheduled (lambda () (apply fn args)))
                        (apply real-run-at-time 3600 nil fn args)))
                     ((symbol-function 'niwa-view-render-batched-full)
                      (lambda (&rest _) (cl-incf full-count))))
            (niwa-view--flush-scheduled-render view-buf)
            (with-current-buffer view-buf
              (should (= 0 full-count))
              (should (eq 'full niwa-view--pending-render-kind))
              (should (eq data-buf niwa-view--pending-render-data-buffer))
              (should-not niwa-view--render-timer))
            ;; Focus returns: work is already coalesced, but cannot run
            ;; inside the focus/redisplay hook itself.
            (setq unattended nil)
            (niwa-view--resume-attended-views)
            (with-current-buffer view-buf
              (should (= 0 full-count))
              (should (equal 0 delay))
              (should (niwa--timer-pending-p niwa-view--render-timer))
              (let ((timer niwa-view--render-timer))
                (niwa-view--resume-render-if-attended view-buf)
                (should (eq timer niwa-view--render-timer))
                (cancel-timer timer))
              (funcall scheduled))
            (with-current-buffer view-buf
              (should (= 1 full-count))
              (should-not niwa-view--pending-render-kind)
              (should-not niwa-view--render-timer)))
        (with-current-buffer view-buf
          (niwa-view--cancel-scheduled-render))))))

(niwa-deftest niwa-view--flush-scheduled-render/diagnostic-scope ()
  (niwa-view-test--with-buffers
    (with-current-buffer data-buf
      (setq-local niwa--workspace
                  (niwa-workspace--create :root "/project A/same/")))
    (with-current-buffer view-buf
      (niwa-view--schedule-render 'full data-buf 10)
      (cancel-timer niwa-view--render-timer)
      (setq niwa-view--render-timer nil))
    (with-current-buffer data-buf
      (let ((kill-buffer-hook nil)) (kill-buffer data-buf)))
    (with-temp-buffer
      (setq-local niwa--workspace
                  (niwa-workspace--create :root "/project B/same/"))
      (cl-letf (((symbol-function 'niwa-view-historical-segment-p) (lambda () nil))
                ((symbol-function 'niwa-view--unattended-p) (lambda (&rest _) nil))
                ((symbol-function 'niwa-view--full-rerender)
                 (lambda () (error "Injected render failure"))))
        (let (messages)
          (niwa-test--with-captured-messages messages
            (niwa-view--flush-scheduled-render view-buf t))
          (should (equal "niwa [/project A/same/]: Injected render failure (View refresh failed)\n"
                         messages)))))))

(niwa-deftest niwa-view--flush-scheduled-render/tool-rows
  (:doc "coalesced full rendering subsumes rows while incremental retains them")
  (dolist (kind '(tools incremental full))
    (niwa-view-test--with-buffers
      (let ((full 0) (incremental 0) refreshed)
        (with-current-buffer view-buf
          (niwa-view-test--insert-composer-draft "> quoted\nsecond line" 3)
          (setq niwa-view--pending-render-kind kind
                niwa-view--pending-render-data-buffer data-buf
                niwa-view--pending-tool-rows '("older" "current")))
        (cl-letf (((symbol-function 'niwa-view-render-batched-full)
                   (lambda (&rest _) (cl-incf full)))
                  ((symbol-function 'niwa-view--render-stream-update)
                   (lambda (&rest _) (cl-incf incremental)))
                  ((symbol-function 'niwa-view--refresh-tool-row)
                   (lambda (_buffer id) (push id refreshed) t)))
          (niwa-view--flush-scheduled-render view-buf))
        (should (= full (if (eq kind 'full) 1 0)))
        (should (= incremental (if (eq kind 'incremental) 1 0)))
        (should (equal refreshed (unless (eq kind 'full) '("current" "older"))))
        (with-current-buffer view-buf
          (should-not niwa-view--pending-tool-rows)
          (should-not niwa-view--pending-render-kind)
          (should (equal "> quoted\nsecond line" (niwa-view-composer-draft-text)))
          (should (= 3 (niwa-view-test--composer-point-offset))))))))

(niwa-deftest niwa-view--schedule-render/tool-rows
  (:doc "tool updates never downgrade pending projection; cancelling can keep rows")
  (niwa-view-test--with-buffers
    (unwind-protect
        (with-current-buffer view-buf
          (cl-loop for kind in '(tools incremental tools full tools incremental)
                   for expected in '(tools incremental incremental full full full)
                   do (niwa-view--schedule-render kind data-buf 10)
                   (should (eq expected niwa-view--pending-render-kind)))
          (setq niwa-view--pending-tool-rows '("stale"))
          (niwa-view--cancel-scheduled-render)
          (should-not niwa-view--pending-tool-rows)
          ;; A settling turn retires streamed work but keeps retained
          ;; rows as their own render.
          (niwa-view--schedule-render 'incremental data-buf 10)
          (setq niwa-view--pending-tool-rows '("kept"))
          (niwa-view--cancel-scheduled-render t)
          (should (equal '("kept") niwa-view--pending-tool-rows))
          (should (eq 'tools niwa-view--pending-render-kind))
          ;; A pending full render outlives the settling turn.
          (niwa-view--schedule-render 'full data-buf 10)
          (let ((timer niwa-view--render-timer))
            (niwa-view--cancel-scheduled-render t)
            (should (eq 'full niwa-view--pending-render-kind))
            (should (eq timer niwa-view--render-timer))
            (should (niwa--timer-pending-p timer))))
      (with-current-buffer view-buf (niwa-view--cancel-scheduled-render)))))

(niwa-deftest niwa-view--unattended-p ()
  ,test
  (test)
  :doc "a hidden view is unattended in an interactive Emacs"
  (niwa-view-test--with-buffers
    (let ((noninteractive nil))
      (should (niwa-view--unattended-p view-buf))))

  :doc "a view without a window in batch, or on a terminal frame, is attended"
  (niwa-view-test--with-buffers
    (should-not (niwa-view--unattended-p view-buf))
    (let ((window (selected-window))
          (original (window-buffer (selected-window))))
      (unwind-protect
          (progn
            (set-window-buffer window view-buf)
            ;; The batch frame is visible, non-graphic, and reports no
            ;; focus; a terminal cannot tell, so it counts as attended.
            (should-not (niwa-view--unattended-p view-buf))
            (with-current-buffer view-buf
              (should-not (niwa-view--unattended-p))))
        (set-window-buffer window original))))

  :doc "a graphical frame without focus or visibility is unattended"
  (niwa-view-test--with-buffers
    (let ((window (selected-window))
          (original (window-buffer (selected-window)))
          (focused nil)
          (visible t))
      (unwind-protect
          (cl-letf (((symbol-function 'display-graphic-p)
                     (lambda (&rest _) t))
                    ((symbol-function 'frame-focus-state)
                     (lambda (&rest _) focused))
                    ((symbol-function 'frame-visible-p)
                     (lambda (&rest _) visible)))
            (set-window-buffer window view-buf)
            (should (niwa-view--unattended-p view-buf))
            (setq focused t)
            (should-not (niwa-view--unattended-p view-buf))
            (setq visible 'icon)
            (should (niwa-view--unattended-p view-buf))
            (setq visible nil)
            (should (niwa-view--unattended-p view-buf)))
        (set-window-buffer window original))))

  :doc "a child frame accepts its own focus or its top-level ancestor's"
  (niwa-view-test--with-buffers
    (let ((window (selected-window))
          (original (window-buffer (selected-window)))
          (parent-focused nil)
          (child-focused nil)
          asked)
      (unwind-protect
          (cl-letf (((symbol-function 'frame-parent)
                     (lambda (frame) (and (not (eq frame 'parent)) 'parent)))
                    ((symbol-function 'display-graphic-p)
                     (lambda (&rest _) t))
                    ((symbol-function 'frame-focus-state)
                     (lambda (&optional frame)
                       (push frame asked)
                       (if (eq frame 'parent) parent-focused child-focused))))
            (set-window-buffer window view-buf)
            (should (niwa-view--unattended-p view-buf))
            (should (memq 'parent asked))
            (should (memq (window-frame window) asked))
            (setq child-focused t)
            (should-not (niwa-view--unattended-p view-buf))
            (setq child-focused nil)
            (setq parent-focused t)
            (should-not (niwa-view--unattended-p view-buf)))
        (set-window-buffer window original)))))

(niwa-deftest niwa-view--schedule-render/unattended ()
  ,test
  (test)
  :doc "an unattended view records the render without arming a timer"
  (niwa-view-test--with-buffers
    (with-current-buffer view-buf
      (cl-letf (((symbol-function 'niwa-view--unattended-p) (lambda (&rest _) t)))
        (niwa-view--schedule-render 'incremental data-buf 0.4)
        (should-not niwa-view--render-timer)
        (should (eq 'incremental niwa-view--pending-render-kind))
        (should (eq data-buf niwa-view--pending-render-data-buffer))))))

(niwa-deftest niwa-view--resume-attended-views ()
  ,test
  (test)
  :doc "re-arms only attended views that still owe a render"
  (niwa-view-test--with-buffers
    (let ((unattended nil))
      (unwind-protect
          (cl-letf (((symbol-function 'niwa-view--unattended-p)
                     (lambda (&rest _) unattended)))
            ;; Nothing pending: nothing armed.
            (niwa-view--resume-attended-views)
            (with-current-buffer view-buf
              (should-not niwa-view--render-timer)
              (setq-local niwa-view--pending-render-kind 'incremental
                          niwa-view--pending-render-data-buffer data-buf))
            ;; Still unattended: keep waiting.
            (setq unattended t)
            (niwa-view--resume-attended-views)
            (with-current-buffer view-buf
              (should-not niwa-view--render-timer))
            ;; Attended again: the stored kind is rescheduled once, and a
            ;; second focus change does not arm a second timer.
            (setq unattended nil)
            (niwa-view--resume-attended-views)
            (with-current-buffer view-buf
              (should (niwa--timer-pending-p niwa-view--render-timer))
              (should (eq 'incremental niwa-view--pending-render-kind))
              (let ((timer niwa-view--render-timer))
                (niwa-view--resume-attended-views)
                (should (eq timer niwa-view--render-timer)))))
        (with-current-buffer view-buf
          (niwa-view--cancel-scheduled-render)))))

  :doc "skips a pending render whose data buffer died"
  (niwa-view-test--with-buffers
    (let ((dead (generate-new-buffer " *dead-data*")))
      (kill-buffer dead)
      (with-current-buffer view-buf
        (setq-local niwa-view--pending-render-kind 'full
                    niwa-view--pending-render-data-buffer dead))
      (cl-letf (((symbol-function 'niwa-view--unattended-p)
                 (lambda (&rest _) nil)))
        (niwa-view--resume-attended-views))
      (with-current-buffer view-buf
        (should-not niwa-view--render-timer)
        (niwa-view--cancel-scheduled-render)))))

(niwa-deftest niwa-view--status-strip-button ()
  ,test
  (test)
  :doc "status strip button routes clicks to the requested cockpit area"
  (let ((button (niwa-view--status-strip-button
                 "Mode" 'mode "Open mode cockpit"))
        called)
    (cl-letf (((symbol-function 'niwa-menu-open)
               (lambda (area) (setq called area))))
      (let* ((map (get-text-property 0 'local-map button))
             (command (lookup-key map [tab-line mouse-1])))
        (should (eq (get-text-property 0 'niwa-view-cockpit-area button)
                    'mode))
        (should (string= button "Mode"))
        (should command)
        (funcall command nil)
        (should (eq called 'mode))))))

(niwa-deftest niwa-view--pinned-prompt ()
  ,test
  (test)
  :doc "matches a backward property walk at every window start"
  (save-window-excursion
    (niwa-view-test--with-buffers
      (dolist (prompt '("One" "Two" "Three"))
        (niwa-view-test--insert-data data-buf (format "%s\n" prompt) nil)
        (niwa-view-test--insert-data
         data-buf (apply #'concat (make-list 5 "answer\n")) 'response))
      (with-current-buffer view-buf
        (niwa-view--full-rerender)
        (set-window-buffer (selected-window) view-buf)
        (let ((oracle
               (lambda (start top)
                 ;; The last prompt header run that ends at or before TOP.
                 (let ((pos (point-min)) found)
                   (while (< pos start)
                     (let ((next (next-single-property-change
                                  pos 'niwa-view-prompt-preview nil (point-max)))
                           (preview (get-text-property pos 'niwa-view-prompt-preview)))
                       (when (and preview (<= next top))
                         (setq found (cons pos preview)))
                       (setq pos next)))
                   found))))
          (dolist (start (number-sequence (point-min) (point-max)))
            (set-window-start nil start t)
            (unless (get-text-property start 'niwa-view-prompt-preview)
              (should (equal (funcall oracle start
                                      (min start (niwa-view-transcript-tail)))
                             (niwa-view--pinned-prompt (selected-window))))))))))

  :doc "header follows the prompt above the top edge, not point or the newest prompt"
  (save-window-excursion
    (niwa-view-test--with-buffers
      (dolist (prompt '("First question" "Second question"))
        (niwa-view-test--insert-data data-buf
                                        (format "%s\n" prompt) nil)
        (niwa-view-test--insert-data data-buf
                                        (apply #'concat (make-list 100 "response line\n"))
                                        'response))
      (with-current-buffer view-buf
        (switch-to-buffer view-buf)
        (niwa-view--full-rerender)
        (set-window-buffer (selected-window) view-buf)
        (goto-char (point-min))
        (search-forward "First question")
        (let ((first (line-beginning-position)))
          (search-forward "Second question")
          (let ((second (line-beginning-position)))
            (set-window-start nil (point-min) t)
            (should-not (niwa-view--sticky-prompt-line))
            (goto-char second)
            (set-window-start nil first t)
            (should (get-text-property
                     (save-excursion (goto-char first)
                                     (search-backward "You") (point))
                     'niwa-view-prompt-preview))
            (should (equal first (window-start)))
            (should (niwa-view--pinned-prompt (selected-window)))
            (should (string-search "First question"
                                   (niwa-view--sticky-prompt-line)))
            (should-not (string-search "First question"
                                       (niwa-view--status-strip)))
            (should (string-search "ask · idle"
                                   (niwa-view--status-strip)))
            (set-window-start
             nil (save-excursion (goto-char second)
                                 (search-backward "You\n") (point)) t)
            (should-not (niwa-view--pinned-prompt (selected-window)))
            (set-window-start nil second t)
            (should (string-search "Second question"
                                   (niwa-view--sticky-prompt-line))))))))

  :doc "two windows and a status-strip cache follow their own top edges"
  (save-window-excursion
    (niwa-view-test--with-buffers
      (dolist (prompt '("Alpha question" "Beta question"))
        (niwa-view-test--insert-data data-buf (format "%s\n" prompt) nil)
        (niwa-view-test--insert-data
         data-buf (apply #'concat (make-list 60 "answer\n")) 'response))
      (with-current-buffer view-buf
        (niwa-view--full-rerender)
        (let ((first (save-excursion
                       (goto-char (point-min))
                       (search-forward "Alpha question")
                       (line-beginning-position)))
              (second (save-excursion
                        (goto-char (point-min))
                        (search-forward "Beta question")
                        (line-beginning-position)))
              (left (selected-window))
              right)
          (set-window-buffer left view-buf)
          (setq right (split-window-below))
          (set-window-buffer right view-buf)
          (set-window-start left first t)
          (set-window-start right second t)
          (with-selected-window left
            (should (string-search "Alpha question" (niwa-view--sticky-prompt-line))))
          (with-selected-window right
            (should (string-search "Beta question" (niwa-view--sticky-prompt-line))))
          (with-selected-window left
            (should (string-search "Alpha question" (niwa-view--sticky-prompt-line))))))))

  :doc "a folded summary pins as soon as only its blank separator remains"
  (save-window-excursion
    (niwa-view-test--with-buffers
      (niwa-view-test--insert-data data-buf
                                      "First question\nMore text\n" nil)
      (niwa-view-test--insert-data
       data-buf (apply #'concat (make-list 100 "answer\n")) 'response)
      (with-current-buffer view-buf
        (niwa-view--full-rerender)
        (set-window-buffer (selected-window) view-buf)
        (goto-char (point-min))
        (search-forward "You\n")
        (let ((header (match-beginning 0)))
          (goto-char header)
          (niwa-view--collapse-turn)
          (set-window-start nil header t)
          (should-not (niwa-view--pinned-prompt (selected-window)))
          (let ((separator (save-excursion
                             (goto-char header)
                             (forward-line 1)
                             (point))))
            (set-window-start nil separator t)
            (should (= separator (window-start)))
            (should (equal (cons header "First question More text")
                           (niwa-view--pinned-prompt
                            (selected-window))))))))))

(niwa-deftest niwa-view--sticky-prompt-line ()
  ,test
  (test)
  :doc "caps a long preview at the configured width, or the window when nil"
  (save-window-excursion
    (niwa-view-test--with-buffers
      (niwa-view-test--insert-data
       data-buf (concat (mapconcat #'identity (make-list 40 "long") " ") "\n")
       nil)
      (niwa-view-test--insert-data
       data-buf (apply #'concat (make-list 100 "answer\n")) 'response)
      (with-current-buffer view-buf
        (niwa-view--full-rerender)
        (set-window-buffer (selected-window) view-buf)
        (goto-char (point-min))
        (search-forward "answer")
        (set-window-start nil (line-beginning-position) t)
        (let ((niwa-view-sticky-prompt-max-width 20))
          (let ((line (niwa-view--sticky-prompt-line)))
            (should (string-prefix-p "Prompt  long" line))
            (should (string-suffix-p "…" line))
            (should (= (string-width line) (+ 8 20)))))
        (let ((niwa-view-sticky-prompt-max-width nil))
          (should (= (string-width (niwa-view--sticky-prompt-line))
                     (1- (window-body-width)))))
        (let ((niwa-view-sticky-prompt-max-width 10000))
          (should (= (string-width (niwa-view--sticky-prompt-line))
                     (1- (window-body-width))))))))

  :doc "graphical redisplay measures archived prompt visibility without recursion"
  (let ((process-environment (copy-sequence process-environment)))
    (skip-unless (niwa-test--wayland-available-p))
    (setenv "NIWA_TEST_LOAD_PATH" (prin1-to-string load-path))
    (pcase-let ((`(,observed ,status . ,_trace)
                 (niwa-test--run-wayland-fixture "view-header-wayland.el" 20)))
      (ert-info ((format "Observed: %S" observed))
        (should (= status 0))
        (should (= (plist-get observed :depth) 1))
        (should-not (plist-get observed :visible))
        (should (string-match-p "Archived prompt" (plist-get observed :hidden)))
        (should (equal (plist-get observed :draft) "> draft\nsecond line"))
        (should (plist-get observed :header-restored)))))

  :doc "ignores a selected window displaying a different buffer"
  (save-window-excursion
    (niwa-view-test--with-buffers
      (with-temp-buffer
        (insert (make-string 1000 ?x))
        (set-window-buffer (selected-window) (current-buffer))
        (set-window-start (selected-window) 900 t)
        (with-current-buffer view-buf
          (setq-local niwa-view--continuation-prompt-cache
                      '(nil nil nil nil (:segment 1 :pos 1 :preview "Archived prompt")))
          (should (< (point-max) (window-start (selected-window))))
          (should-not (niwa-view--sticky-prompt-line))))))

  :doc "two windows share stable rows but compute different prompt text"
  (save-window-excursion
    (niwa-view-test--with-buffers
      (niwa-view-test--insert-data data-buf "First question\n" nil)
      (niwa-view-test--insert-data
       data-buf (apply #'concat (make-list 100 "answer\n")) 'response)
      (with-current-buffer view-buf
        (niwa-view--full-rerender)
        (let ((window (selected-window))
              (other (split-window-right)))
          (set-window-buffer window view-buf)
          (set-window-buffer other view-buf)
          (goto-char (point-min))
          (search-forward "answer")
          (set-window-start window (line-beginning-position) t)
          (set-window-start other (point-min) t)
          (should (equal header-line-format
                         '(:eval (niwa-view--sticky-prompt-line))))
          (should (equal tab-line-format
                         '(:eval (niwa-view--status-strip))))
          (with-selected-window window
            (should (string-search "First question"
                                   (niwa-view--sticky-prompt-line))))
          (with-selected-window other
            (should-not (niwa-view--sticky-prompt-line)))
          (set-window-start window (point-min) t)
          (should-not (niwa-view--sticky-prompt-line))
          (set-window-buffer window data-buf)
          (with-current-buffer data-buf
            (should-not (local-variable-p 'tab-line-format))))))))

(niwa-deftest niwa-view--continuation-prompt ()
  ,test
  (test)
  :doc "a copied two-prompt tail cannot pin its later prompt over earlier content"
  (let* ((directory (make-temp-file "niwa-sticky-tail-" t))
         (session (test-niwa-session-persistence--leased-session directory
                   :name "tail"
                   :current-segment 2))
         (path (niwa-session-artifacts-segment-path directory 1)))
    (unwind-protect
        (progn
          (with-temp-buffer
            (org-mode)
            (insert ":PROPERTIES:\n:GPTEL_BOUNDS: nil\n:END:\n\n")
            (insert "Before tail\n")
            (insert (propertize "Answer before tail\n" 'gptel 'response))
            (insert "First copied prompt\n")
            (insert (propertize "Answer in tail\n" 'gptel 'response))
            (insert "Second copied prompt\n")
            (dotimes (_ 8)
              (let (bounds)
                (goto-char (point-min))
                (dolist (answer '("Answer before tail" "Answer in tail"))
                  (search-forward answer)
                  (push (list 'response
                              (list (match-beginning 0) (match-end 0)))
                        bounds))
                (org-entry-put (point-min) "GPTEL_BOUNDS"
                               (prin1-to-string (nreverse bounds)))))
            (setf (niwa-session-prompt-index session)
                  (list (cons 1 (niwa-session-artifacts-collect-prompts
                                 (current-buffer)))))
            (niwa-view-test--publish session path (buffer-string)))
          (save-window-excursion
            (niwa-view-test--with-buffers
              (with-current-buffer data-buf
                (setq-local niwa--session session)
                (insert ":PROPERTIES:\n:NIWA_SEGMENT_TAIL_PROMPTS: 2\n:END:\n\n")
                (insert (niwa-session-artifacts-summary-block "summary"))
                (insert (propertize
                         (apply #'concat (make-list 80 "Continuation answer\n"))
                         'gptel 'response))
                (insert "First copied prompt\n")
                (insert (propertize "Answer in tail\n" 'gptel 'response))
                (insert "Second copied prompt\n")
                (insert (propertize
                         (apply #'concat (make-list 80 "More answer\n"))
                         'gptel 'response)))
              (with-current-buffer view-buf
                (switch-to-buffer view-buf)
                (niwa-view--full-rerender)
                (set-window-point nil (point-min))
                (set-window-start nil (point-min) t)
                (should (string-search "Before tail"
                                       (niwa-view--sticky-prompt-line)))
                (should-not (string-search "Second copied prompt"
                                           (niwa-view--sticky-prompt-line)))
                (goto-char (point-min))
                (search-forward "First copied prompt")
                (search-backward "You\n")
                (set-window-start nil (point) t)
                (should-not (niwa-view--sticky-prompt-line))
                (goto-char (point-min))
                (search-forward "More answer")
                (set-window-start nil (line-beginning-position) t)
                (should (string-search "Second copied prompt"
                                       (niwa-view--sticky-prompt-line)))))))
      (niwa-session-persistence-lock-release directory session)
      (niwa-workspace-clear-registry)
      (delete-directory directory t)))

  :doc "a compacted continuation pins its source prompt, but clear does not"
  (let* ((directory (make-temp-file "niwa-sticky-continuation-" t))
         (path (niwa-session-artifacts-segment-path directory 1))
         (session (test-niwa-session-persistence--leased-session directory
                   :name "continuation"
                   :current-segment 2))
         prompt-position indexed)
    (unwind-protect
        (progn
          (with-temp-buffer
            (org-mode)
            (insert ":PROPERTIES:\n:GPTEL_BOUNDS: nil\n:END:\n\n")
            (setq prompt-position (point))
            (insert "Original prompt with a second line\nMore details\n")
            (setq indexed (car (niwa-session-artifacts-collect-prompts
                                (current-buffer))))
            (niwa-view-test--publish session path (buffer-string)))
          (setf (niwa-session-prompt-index session)
                (list (cons 1 (list (append indexed '(:cum-turn 1))))))
          (save-window-excursion
            (niwa-view-test--with-buffers
              (with-current-buffer data-buf
                (setq-local niwa--session session)
                (insert (niwa-session-artifacts-summary-block "summary"))
                (insert (propertize "Continuation answer\n" 'gptel 'response)))
              (with-current-buffer view-buf
                (switch-to-buffer view-buf)
                (niwa-view--full-rerender)
                (should (string-search
                         "Original prompt with a second line More details"
                         (niwa-view--sticky-prompt-line)))
                (should (equal (list :segment 1 :pos prompt-position)
                               (cl-subseq (niwa-view--continuation-prompt) 0 4)))
                (niwa-view-segments-jump-to-prompt
                 1 prompt-position (selected-window))
                (should (= 1 (niwa-view-segments-current-number)))
                (should (looking-at-p "You"))
                (should-not (niwa-view--continuation-prompt))
                (niwa-view-return-to-latest-segment)
                (should (string-search "Original prompt with a second line"
                                       (niwa-view--sticky-prompt-line)))
                ;; A second compacted segment has no indexed prompt of its
                ;; own, but still belongs to the original conversation.
                (with-current-buffer data-buf
                  (niwa-view-test--publish
                   session (niwa-session-artifacts-segment-path directory 2)
                   (buffer-string))
                  (setf (niwa-session-current-segment session) 3)
                  (let ((inhibit-read-only t))
                    (erase-buffer)
                    (insert (niwa-session-artifacts-summary-block "again"))))
                (niwa-view--full-rerender)
                (should (string-search "Original prompt with a second line"
                                       (niwa-view--sticky-prompt-line)))
                (with-current-buffer data-buf
                  (insert "New local prompt\n")
                  (insert (propertize
                           (apply #'concat (make-list 80 "answer line\n"))
                           'gptel 'response)))
                (niwa-view--full-rerender)
                (let ((header (save-excursion
                                (goto-char (point-min))
                                (search-forward "New local prompt")
                                (search-backward "You\n")
                                (point)))
                      (answer (save-excursion
                                (goto-char (point-min))
                                (search-forward "answer line")
                                (line-beginning-position))))
                  (set-window-point nil header)
                  (set-window-start
                   nil (save-excursion (goto-char header)
                                       (forward-line -1) (point)) t)
                  (should (< (window-start) header))
                  (should (< header
                             (save-excursion
                               (goto-char (window-start))
                               (vertical-motion (window-body-height))
                               (point))))
                  (should-not (invisible-p header))
                  (should-not (niwa-view--sticky-prompt-line))
                  (set-window-start nil header t)
                  (should-not (niwa-view--sticky-prompt-line))
                  (set-window-start nil answer t)
                  (should (string-search "New local prompt"
                                         (niwa-view--sticky-prompt-line))))
                ;; An empty fresh segment is a /clear boundary, even if
                ;; it is subsequently compacted into a new live segment.
                (with-temp-buffer
                  (niwa-view-test--publish
                   session (niwa-session-artifacts-segment-path directory 2)
                   (buffer-string)))
                (with-current-buffer data-buf
                  (setf (niwa-session-current-segment session) 4)
                  (niwa-view-test--publish
                   session (niwa-session-artifacts-segment-path directory 3)
                   (buffer-string)))
                (niwa-view--full-rerender)
                (should-not (niwa-view--continuation-prompt))
                (with-current-buffer data-buf
                  (let ((inhibit-read-only t))
                    (erase-buffer)
                    (insert "Fresh after clear\n")))
                (niwa-view--full-rerender)
                (should-not (niwa-view--sticky-prompt-line))))))
      (niwa-session-persistence-lock-release directory session)
      (niwa-workspace-clear-registry)
      (delete-directory directory t))))

(niwa-deftest niwa-view--prompt-header-runs ()
  ,test
  (test)
  :doc "indexes header edits without rescanning body edits or fontification"
  (niwa-view-test--with-buffers
    (with-current-buffer view-buf
      (let ((inhibit-read-only t))
        (niwa-view--insert-user-message "First question")
        (let* ((runs (niwa-view--prompt-header-runs))
               (header (aref runs 0))
               (start (marker-position (car header))))
          (goto-char (point-min))
          (insert "Prefix\n")
          (goto-char (point-max))
          (insert "Draft")
          (should (eq runs (niwa-view--prompt-header-runs)))
          (should (= (+ start (length "Prefix\n")) (car header)))
          (put-text-property (1- (point-max)) (point-max) 'face 'bold)
          (should (eq runs (niwa-view--prompt-header-runs)))
          (niwa-view--insert-user-message "Second question")
          (should (= 2 (length (niwa-view--prompt-header-runs))))
          (delete-region (car header) (cadr header))
          (let ((remaining (niwa-view--prompt-header-runs)))
            (should (= 1 (length remaining)))
            (should (equal "Second question" (nth 2 (aref remaining 0))))))))))

(niwa-deftest niwa-view--pinned-prompt-button ()
  ,test
  (test)
  :doc "a narrow preview preserves controls and escapes literal percent signs"
  (let* ((button (niwa-view--pinned-prompt-button "Unicode ∑ 100% complete" 4 18))
         (map (get-text-property 0 'local-map button)))
    (should (<= (string-width
                 (replace-regexp-in-string "%%" "%" button t t)) 18))
    (should (string-match-p "%%" button))
    (should (string-suffix-p "…" button))
    (should (lookup-key map [header-line mouse-1])))

  :doc "exhausted preview budgets omit the prompt entirely"
  (dolist (width '(-10 0))
    (should (equal "" (niwa-view--pinned-prompt-button
                       "Long prompt that must yield to controls" 4 width))))

  (save-window-excursion
    (niwa-view-test--with-buffers
      (niwa-view-test--insert-data data-buf "Very long prompt\n" nil)
      (niwa-view-test--insert-data data-buf "Answer.\n" 'response)
      (with-current-buffer view-buf
        (switch-to-buffer view-buf)
        (niwa-view--full-rerender)
        (goto-char (point-min))
        (search-forward "Answer.")
        (set-window-start nil (line-beginning-position) t)
        (cl-letf (((symbol-function 'niwa-view--status-strip-width)
                   (lambda () 1)))
          (let ((header (niwa-view--status-strip)))
            (should (string-match-p "ask · idle" header))
            (should-not (string-match-p "Very long prompt" header))
            (should (string-search "Very long prompt"
                                   (niwa-view--sticky-prompt-line))))))))

  :doc "previews share one map and route clicks by the clicked text's target"
  (let* ((archived (niwa-view--pinned-prompt-button "Archived prompt" 42 30 3))
         (local (niwa-view--pinned-prompt-button "Local prompt" 7 30))
         (line (concat (propertize "Prompt  " 'face 'shadow) archived))
         (window (selected-window))
         calls)
    (should (eq (get-text-property 0 'local-map archived)
                (get-text-property 0 'local-map local)))
    (cl-letf (((symbol-function 'niwa-view-segments-jump-to-prompt)
               (lambda (&rest args) (push args calls))))
      (let ((command (lookup-key (get-text-property 0 'local-map archived)
                                 [header-line mouse-1])))
        (funcall command (list 'mouse-1 (list window 'header-line '(10 . 0) 0
                                              (cons line 10))))
        (should (equal (list (list 3 42 window)) calls))
        ;; The label carries no target.
        (funcall command (list 'mouse-1 (list window 'header-line '(1 . 0) 0
                                              (cons line 1))))
        (should (= 1 (length calls)))))))

(niwa-deftest niwa-view--jump-to-pinned-prompt ()
  ,test
  (test)
  :doc "clicking a pinned folded prompt expands it and brings it to the top"
  (save-window-excursion
    (niwa-view-test--with-buffers
      (niwa-view-test--insert-data
       data-buf (concat "Long question\n"
                        (apply #'concat (make-list 18 "additional detail\n"))) nil)
      (niwa-view-test--insert-data
       data-buf (apply #'concat (make-list 80 "answer\n")) 'response)
      (with-current-buffer view-buf
        (niwa-view--full-rerender)
        (set-window-buffer (selected-window) view-buf)
        (goto-char (point-min))
        (search-forward "You\n")
        (let ((header (match-beginning 0)))
          (should (get-text-property (point) 'niwa-view-collapsed))
          (niwa-view-render-toggle-user-input)
          (goto-char header)
          (niwa-view--collapse-turn)
          (should (string-prefix-p
                   "Long question additional detail"
                   (get-text-property header 'niwa-view-prompt-preview)))
          (niwa-view--jump-to-pinned-prompt header)
          (should (= header (window-start)))
          (should-not (get-text-property header 'niwa-view-collapsed))
          (forward-line 1)
          (should-not (get-text-property (point) 'niwa-view-collapsed))))))

  :doc "jump expands an auto-folded input without changing the composer draft"
  (save-window-excursion
    (niwa-view-test--with-buffers
      (niwa-view-test--insert-data
       data-buf (concat "Multi line prompt\n"
                        (apply #'concat (make-list 20 "body line\n"))) nil)
      (niwa-view-test--insert-data data-buf "Answer.\n" 'response)
      (with-current-buffer view-buf
        (niwa-view--full-rerender)
        (set-window-buffer (selected-window) view-buf)
        (niwa-view-test--insert-composer-draft "> draft\nsecond line")
        (goto-char (point-min))
        (search-forward "You\n")
        (let ((header (match-beginning 0)))
          (should (get-text-property (point) 'niwa-view-collapsed))
          (niwa-view--jump-to-pinned-prompt header)
          (should (= header (window-start)))
          (forward-line 1)
          (should (looking-at-p "Multi line prompt"))
          (should-not (get-text-property (point) 'niwa-view-collapsed))
          (should (string= "> draft\nsecond line"
                           (niwa-view-composer-draft-text nil t)))))))

  :doc "a preview click targets its window when another buffer is selected"
  (save-window-excursion
    (niwa-view-test--with-buffers
      (niwa-view-test--insert-data data-buf "First question\n" nil)
      (niwa-view-test--insert-data
       data-buf (apply #'concat (make-list 100 "answer\n")) 'response)
      (with-current-buffer view-buf
        (niwa-view--full-rerender)
        (niwa-view-test--insert-composer-draft "> draft\nsecond line"))
      (let ((left (selected-window))
            (right (split-window-right)))
        (set-window-buffer left data-buf)
        (set-window-buffer right view-buf)
        (with-current-buffer view-buf
          (goto-char (point-min))
          (search-forward "You\n")
          (let* ((header (match-beginning 0))
                 (response (progn (search-forward "answer")
                                  (line-beginning-position)))
                 (button (niwa-view--pinned-prompt-button
                          "First question" header 30))
                 (command (lookup-key
                           (get-text-property 0 'local-map button)
                           [header-line mouse-1]))
                 (event (list 'mouse-1
                              (list right 'header-line '(10 . 0) 0
                                    (cons button 0)))))
            (set-window-start right response t)
            (set-window-point right response)
            (select-window left)
            (should (eq (current-buffer) data-buf))
            (funcall command event)
            (should (eq (selected-window) left))
            (should (= header (window-start right)))
            (should (equal "> draft\nsecond line"
                           (with-current-buffer view-buf
                             (niwa-view-composer-draft-text))))))))))

(niwa-deftest niwa-view-toggle-auto-compaction ()
  ,test
  (test)
  :doc "rendered toggle changes admission without touching the draft or another window"
  (save-window-excursion
    (niwa-view-test--with-buffers
      (let* ((root (make-temp-file "niwa-toggle-" t))
             (session (niwa-session--create
                       :name "toggle" :save-path root :current-segment 1))
             (left (selected-window))
             (right (split-window-right)))
        (unwind-protect
            (progn
              (with-current-buffer data-buf
                (setq-local niwa--session session
                            buffer-file-name
                            (niwa-session-artifacts-segment-path root 1)
                            niwa-compact-auto t)
                (should (niwa--compact-auto-eligible-p)))
              (set-window-buffer left data-buf)
              (set-window-buffer right view-buf)
              (with-current-buffer view-buf
                (niwa-view-test--insert-composer-draft "> draft\nsecond line" 4)
                (let* ((before (buffer-string))
                       (position (point))
                       (start (window-start right))
                       (line (niwa-view--status-strip))
                       (index (string-search "compact on" line))
                       (command (and index (lookup-key
                                            (get-text-property index 'local-map line)
                                            [tab-line mouse-1])))
                       (event (list 'mouse-1
                                    (list right 'tab-line '(10 . 0) 0))))
                  (should index)
                  (should (commandp command))
                  (should (eq line (niwa-view--status-strip)))
                  (with-current-buffer data-buf (funcall command event))
                  (should (eq left (selected-window)))
                  (should (equal before (buffer-string)))
                  (should (= position (point)))
                  (should (= start (window-start right)))
                  (should (string-search "compact off" (niwa-view--status-strip)))
                  (with-current-buffer data-buf
                    (should-not (niwa--compact-auto-eligible-p)))
                  (funcall command event)
                  (should (string-search "compact on" (niwa-view--status-strip)))
                  (with-current-buffer data-buf
                    (should (niwa--compact-auto-eligible-p)))
                  (with-temp-buffer (should niwa-compact-auto)))))
          (with-current-buffer data-buf
            (setq buffer-file-name nil niwa--session nil))
          (delete-directory root t)))))

  :doc "explicit re-enable clears the failure latch but preserves the global default"
  (let ((niwa-compact-auto nil))
    (niwa-view-test--with-buffers
      (with-current-buffer data-buf
        (setq-local niwa--session (niwa-session--create :name "toggle")
                    niwa--compact-auto-disabled t
                    niwa-compact-run-failure-count 3))
      (with-current-buffer view-buf
        (should (string-search "compact off" (niwa-view--status-strip)))
        (niwa-view-toggle-auto-compaction)
        (should (string-search "compact on" (niwa-view--status-strip))))
    (should-not niwa-compact-auto)))

  :doc "provider-managed compaction has no clickable control and refuses direct toggling"
  (niwa-view-test--with-buffers
    (with-current-buffer data-buf
      (setq-local niwa--session (niwa-session--create :name "native")
                  gptel-backend (niwa-claude-code--make-backend :name "Claude")))
    (with-current-buffer view-buf
      (let* ((line (niwa-view--status-strip))
             (index (string-search "compact provider" line)))
        (should index)
        (should-not (get-text-property index 'local-map line))
        (should (string-search "provider" (get-text-property index 'help-echo line)))
        (should-error (niwa-view-toggle-auto-compaction) :type 'user-error)))))

(niwa-deftest niwa-view--status-strip ()
  ,test
  (test)
  :doc "status strip root label truncates to the workspace tail, then disappears"
  (let ((root "~/Projekte/niwa/"))
    (should (equal root
                   (niwa-view--status-strip-root-label root 24)))
    (should (equal "…/niwa/"
                   (niwa-view--status-strip-root-label root 7)))
    (should (equal ""
                   (niwa-view--status-strip-root-label root 6))))

  :doc "a long session name yields to controls; the prompt has its own row"
  (save-window-excursion
    (niwa-view-test--with-buffers
      (niwa-view-test--insert-data data-buf "Short prompt\n" nil)
      (niwa-view-test--insert-data
       data-buf (apply #'concat (make-list 100 "answer\n")) 'response)
      (with-current-buffer data-buf
        (setq-local niwa--session
                    (niwa-session--create :name (make-string 60 ?S))
                    gptel-model nil))
      (with-current-buffer view-buf
        (niwa-view--full-rerender)
        (set-window-buffer (selected-window) view-buf)
        (goto-char (point-min))
        (search-forward "answer")
        (set-window-start nil (line-beginning-position) t)
        (dolist (width '(48 63 93))
          (cl-letf (((symbol-function 'niwa-view--status-strip-width)
                     (lambda () width)))
            (let ((line (niwa-view--status-strip)))
              (should (<= (string-width line) width))
              (should (string-match-p
                       (regexp-quote "ask · idle · model none · 0 tools")
                       line))
              (if (= width 48)
                  (should-not (string-search "S" line))
                (should (string-match-p "\\`S+…" line))
                (should (text-property-any
                         0 (length line) 'niwa-view-cockpit-area 'top line)))
              (should-not (string-search "Short prompt" line))
              (should (string-search "Short prompt"
                                     (niwa-view--sticky-prompt-line))))))
        (set-window-start nil (point-min) t)
        (cl-letf (((symbol-function 'niwa-view--status-strip-width)
                   (lambda () 120)))
          (let ((line (niwa-view--status-strip)))
            (should (string-prefix-p (make-string 60 ?S) line))
            (should (string-search "0 tools" line)))))))

  :doc "status strip shows niwa-owned session orientation instead of the data header"
  (let* ((root (make-temp-file "niwa-status-root-" t))
         (workspace (niwa-workspace-get-or-create
                     'project (format "status-%s" root) root "niwa"))
         (session (niwa-session-create "main" workspace)))
    (setf (niwa-session-permission-mode session) 'ask)
    (unwind-protect
        (niwa-view-test--with-buffers
          (with-current-buffer data-buf
            (setq-local default-directory (file-name-as-directory root))
            (setq-local header-line-format "GPTEL HEADER")
            (setq-local niwa--session session)
            (setq-local gptel-model 'gpt-5.5)
            (setq-local gptel-tools '(read edit)))
          (with-current-buffer view-buf
            (let ((line (cl-letf (((symbol-function 'niwa-view--status-strip-width)
                                  (lambda () 120)))
                          (niwa-view--status-strip))))
              (should (string-prefix-p "main  " line))
              (should (string-match-p
                       (regexp-quote
                        (file-name-nondirectory
                         (directory-file-name root)))
                       line))
              (should (string-match-p
                        (regexp-quote "ask · idle · gpt-5.5 · 2 tools")
                       line))
              (should-not (string-match-p "niwa:" line))
              (should-not (string-match-p "\\[gpt-5\\.5\\]" line))
              (should-not (string-match-p "\\[2 tools\\]" line))
              (should-not (string-match-p "GPTEL HEADER" line)))))
	  (when (file-directory-p root)
	    (delete-directory root t))))

  :doc "status strip preserves the model none label"
  (niwa-view-test--with-buffers
    ;; The label describes an unconfigured gptel, so the data buffer must
    ;; not inherit the harness default model.
    (with-current-buffer data-buf
      (setq-local gptel-model nil))
    (with-current-buffer view-buf
      (let ((line (niwa-view--status-strip)))
        (should (string-match-p
                 (regexp-quote "ask · idle · model none · 0 tools")
                 line)))))

  :doc "status strip identifies the session execution target"
  (niwa-view-test--with-buffers
    (let* ((target (niwa-execution-target-create
                    "/ssh:user@host:/srv/project/"))
           (remote-workspace
            (niwa-workspace--create
             :type 'project :id "remote" :root "/ssh:user@host:/srv/project/"
             :name "remote"))
           (remote-session
            (niwa-session--create
             :name "remote" :workspace remote-workspace
             :working-directory "/ssh:user@host:/srv/project/"
             :execution-target target :permission-mode 'ask)))
      (with-current-buffer data-buf
        (setq-local niwa--session remote-session))
      (with-current-buffer view-buf
        (let ((line (niwa-view--status-strip)))
          (should (string-match-p "ssh:user@host" line))
          (should (text-property-any
                   0 (length line) 'niwa-view-cockpit-area 'top line))))
      (with-current-buffer data-buf
        (setq-local niwa--session nil))))

  :doc "target, readiness, lease, and publication refreshes preserve a multiline leading-> composer draft"
  (niwa-view-test--with-buffers
    (let* ((target (niwa-execution-target-create
                    "/ssh:user@host:/srv/project/"))
           (workspace (niwa-workspace--create
                       :type 'project :id "remote-status"
                       :root "/ssh:user@host:/srv/project/"
                       :name "remote-status"))
           (session (niwa-session--create
                     :name "remote-status" :workspace workspace
                     :working-directory "/ssh:user@host:/srv/project/"
                     :execution-target target
                     :permission-mode 'ask))
           (draft "> quoted\nsecond line")
           (point-offset 4))
      (unwind-protect
          (progn
            (with-current-buffer data-buf
              (setq-local niwa--session session))
            (with-current-buffer view-buf
              (niwa-view-interaction-initialize))
            (with-current-buffer view-buf
              (niwa-view-test--insert-composer-draft draft point-offset)
              (should (string-match-p "ssh:user@host"
                                      (niwa-view--status-strip))))
            (setf (niwa-session-lease session)
                  '(:state lost :unsettled-mutation nil)
                  (niwa-session-pending-publication session)
                  '(:reason "remote write failed")
                  (niwa-execution-target-readiness target)
                  '(:status ready
                    :sandbox-mode best-effort
                    :sandbox-status bubblewrap))
            (niwa-session-recovery-refresh-session-buffers session)
            (with-current-buffer view-buf
              (force-mode-line-update t)
              (let ((line (niwa-view--status-strip))
                    (header (niwa-menu--header)))
                (should (string-match-p "ssh:user@host" line))
                (should (string-match-p "lease lost" line))
                (should (string-match-p "publication pending" line))
                ;; Nominal readiness stays out of the cockpit header; the
                ;; lost lease and pending publication earn its alert line.
                (should (string-match-p "lease lost" header))
                (should (string-match-p "publication pending" header))
                (should-not (string-match-p "sandbox bubblewrap" header))
                (should (string= draft (niwa-view-composer-draft-text)))
                (should (= point-offset (niwa-view-test--composer-point-offset))))))
        (setf (niwa-session-pending-publication session) nil)
        (when (buffer-live-p data-buf)
          (with-current-buffer data-buf
            (setq-local niwa--session nil))))))

  :doc "status strip reuses unchanged output and rebuilds after state changes"
  (niwa-view-test--with-buffers
    (with-current-buffer view-buf
      (let ((first (niwa-view--status-strip))
            second changed)
        (setq second (niwa-view--status-strip))
        (should (eq first second))
        (with-current-buffer data-buf
          (setq-local gptel-tools '(read)))
        (setq changed (niwa-view--status-strip))
        (should-not (eq second changed))
        (should (string-match-p "1 tool" changed)))))

  :doc "status strip reflects each input changed in place in one view"
  (niwa-view-test--with-buffers
    (let* ((workspace (niwa-workspace--create
                       :type 'project :id "strip-inputs"
                       :root "/tmp/strip-inputs/" :name "strip-inputs"))
           (session (niwa-session--create
                     :name "main" :workspace workspace
                     :permission-mode 'ask))
           (goal (niwa-goal--create :status 'active :turns-run 1))
           (width 120))
      (with-current-buffer data-buf
        (setq-local niwa--session session
                    gptel-model 'gpt-5.5
                    gptel-tools nil))
      (with-current-buffer view-buf
        (cl-letf (((symbol-function 'niwa-view--status-strip-width)
                   (lambda () width)))
          (let ((line (niwa-view--status-strip)))
            (cl-flet ((changes (pattern mutate)
                        (funcall mutate)
                        (let ((next (niwa-view--status-strip)))
                          (should-not (equal line next))
                          (should (string-match-p pattern next))
                          (setq line next))))
              (should (eq line (niwa-view--status-strip)))
              (changes "\\`renamed"
                       (lambda () (setf (niwa-session-name session) "renamed")))
              (changes "strip-moved/"
                       (lambda () (setf (niwa-workspace-root workspace)
                                        "/tmp/strip-moved/")))
              ;; A session's target is immutable; the view's data buffer
              ;; can still change sessions.
              (changes "ssh:user@host"
                       (lambda ()
                         (setq session
                               (niwa-session--create
                                :name "renamed" :workspace workspace
                                :permission-mode 'ask
                                :execution-target
                                (niwa-execution-target-create
                                 "/ssh:user@host:/srv/project/")))
                         (with-current-buffer data-buf
                           (setq-local niwa--session session))))
              (changes "publication pending"
                       (lambda ()
                         (setf (niwa-session-pending-publication session)
                               '(:reason "remote write failed"))))
              (changes "lease lost"
                       (lambda ()
                         (setf (niwa-session-lease session) '(:state lost))))
              (changes "edits ·"
                       (lambda ()
                         (setf (niwa-session-permission-mode session) 'edits)))
              (changes "Plan/edits"
                       (lambda () (setf (niwa-session-plan-mode session) t)))
              (changes "· running ·"
                       (lambda ()
                         (with-current-buffer data-buf
                           (setq-local niwa--current-request
                                       (niwa-request--create :id "strip"
                                                                :session session)))))
              (changes "· settling ·"
                       (lambda ()
                         (with-current-buffer data-buf
                           (setq-local niwa--current-request nil
                                       niwa--turn-settlements-pending '(fsm)))))
              (changes "gpt-5.6-sol"
                       (lambda ()
                         (with-current-buffer data-buf
                           (setq-local gptel-model 'gpt-5.6-sol))))
              (changes "active · 1 turns · gpt-5.6-sol"
                       (lambda () (setf (niwa-session-goal session) goal)))
              (changes "complete · 1 turns"
                       (lambda () (setf (niwa-goal-status goal) 'complete)))
              (changes "complete · 2 turns"
                       (lambda () (setf (niwa-goal-turns-run goal) 2)))
              (changes "preset team"
                       (lambda () (setf (niwa-session-preset-name session) 'team)))
              (changes "1 tool\\'"
                       (lambda ()
                         (with-current-buffer data-buf
                           (setq-local gptel-tools '(read)))))
              (changes "\\`renamed  /srv/project/"
                       (lambda () (setq width 400)))
              (with-current-buffer data-buf
                (setq-local niwa--turn-settlements-pending nil))))))
      (with-current-buffer data-buf
        (setq-local niwa--session nil))))

  :doc "status strip shows Plan together with its permission policy"
  (niwa-view-test--with-buffers
    (let ((session (niwa-session--create
                    :name "main" :permission-mode 'full-auto :plan-mode t)))
      (with-current-buffer data-buf
        (setq-local niwa--session session))
      (with-current-buffer view-buf
        (should (string-match-p
                 (regexp-quote "Plan/full-auto · idle")
                 (niwa-view--status-strip))))))

  :doc "status strip shows phase-free Goal status and turn accounting"
  (niwa-view-test--with-buffers
    (let* ((goal (niwa-goal--create
                  :status 'active :turns-run 5))
           (session (niwa-session--create
                     :name "main" :goal goal :preset-name 'team)))
      (with-current-buffer data-buf
        (setq-local niwa--session session))
      (with-current-buffer view-buf
        (let ((line (niwa-view--status-strip)))
          (should (string-match-p "active · 5 turns" line))
          (should (string-match-p "preset team" line))
          (dolist (area '(goal model preset))
            (should (text-property-any
                     0 (length line) 'niwa-view-cockpit-area area line)))))))

  :doc "status strip shows completion and the restored session model"
  (niwa-view-test--with-buffers
    (let* ((goal (niwa-goal--create
                  :status 'complete :turns-run 3))
           (session (niwa-session--create :name "main" :goal goal)))
      (with-current-buffer data-buf
        (setq-local niwa--session session
                    gptel-model 'gpt-5.6-sol))
      (with-current-buffer view-buf
        (let ((line (niwa-view--status-strip)))
          (should (string-match-p "complete · 3 turns · gpt-5.6-sol" line))))))

  :doc "status strip routes click targets to cockpit surfaces"
  (niwa-view-test--with-buffers
    (with-current-buffer view-buf
      (let ((line (niwa-view--status-strip))
            called)
        (cl-letf (((symbol-function 'niwa-menu-open)
                   (lambda (area) (setq called area))))
          (dolist (area '(top mode model tools))
            (let* ((pos (text-property-any
                         0 (length line)
                         'niwa-view-cockpit-area area line))
                   (map (and pos (get-text-property pos 'local-map line)))
                   (command (and map
                                 (lookup-key map [tab-line mouse-1]))))
              (should pos)
              (should command)
              (setq called nil)
              (funcall command nil)
              (should (eq called area))))))))

  :doc "model and Goal labels keep separate click destinations with an active or finished Goal"
  (niwa-view-test--with-buffers
    (let* ((goal (niwa-goal--create :status 'active :turns-run 2))
           (session (niwa-session--create :name "main" :goal goal))
           called)
      (with-current-buffer data-buf
        (setq-local niwa--session session gptel-model 'gpt-5.6-sol))
      (with-current-buffer view-buf
        (cl-letf (((symbol-function 'niwa-menu-open)
                   (lambda (area) (setq called area))))
          (dolist (status '(active complete))
            (setf (niwa-goal-status goal) status)
            (let ((line (niwa-view--status-strip)))
              (dolist (label '(("Goal " . goal) ("gpt-5.6-sol" . model)))
                (let* ((pos (string-search (car label) line))
                       (map (and pos (get-text-property pos 'local-map line))))
                  (should pos)
                  (should (eq (cdr label)
                              (get-text-property pos 'niwa-view-cockpit-area line)))
                  (setq called nil)
                  (funcall (lookup-key map [tab-line mouse-1]) nil)
                  (should (eq called (cdr label)))))))))))

  :doc "status strip clicks do not call gptel transients directly"
  (niwa-view-test--with-buffers
    (with-current-buffer view-buf
      (let* ((line (niwa-view--status-strip))
             (pos (text-property-any
                   0 (length line)
                   'niwa-view-cockpit-area 'tools line))
             (map (get-text-property pos 'local-map line))
             (command (lookup-key map [tab-line mouse-1]))
             (gptel-called nil))
        (cl-letf (((symbol-function 'gptel-menu)
                   (lambda ()
                     (interactive)
                     (setq gptel-called t)))
                  ((symbol-function 'niwa-menu-open) #'ignore))
          (funcall command nil)
          (should-not gptel-called)))))

  :doc "status strip keeps the raw data buffer header line"
  (let ((data-buf (generate-new-buffer " *status-data*"))
        (view-buf (generate-new-buffer " *status-view*")))
    (unwind-protect
        (progn
          (with-current-buffer data-buf
            (org-mode)
            (setq-local header-line-format "GPTEL HEADER"))
          (niwa-view--setup view-buf data-buf)
          (with-current-buffer data-buf
            (should (equal header-line-format "GPTEL HEADER")))
          (with-current-buffer view-buf
            (should (equal header-line-format
                           '(:eval (niwa-view--sticky-prompt-line))))
            (should (equal tab-line-format
                           '(:eval (niwa-view--status-strip))))))
      (when (buffer-live-p view-buf) (kill-buffer view-buf))
      (when (buffer-live-p data-buf) (kill-buffer data-buf)))))

(niwa-deftest niwa-view--status-fragments ()
  ,test
  (test)
  :doc "status zone contains tasks, executions, and agents but no sandbox row"
  (let ((session (niwa-session--create :name "status")))
    (cl-letf (((symbol-function 'niwa-execution-count-user)
             (lambda (seen-session)
               (should (eq session seen-session))
               3))
            ((symbol-function 'niwa-execution-list-user)
             (lambda (_)
               '((:execution-id "e1" :owner "/root" :yielded t
                  :command "make test")
                 (:execution-id "e2" :owner "/root/child" :yielded t
                  :command "make lint")
                 (:execution-id "e3" :owner "/root" :yielded nil
                  :command "ls"))))
            ((symbol-function 'niwa-view--agent-status-collect)
             (lambda ()
               '((:path "/root/child" :status running :depth 0)))))
    (let* ((fragments
            (niwa-view--status-fragments
             (list :session session :task-body "tasks\n")))
           (sandbox (seq-find
                     (lambda (fragment)
                       (eq (plist-get fragment :id) 'sandbox))
                     fragments))
           (tasks (seq-find
                   (lambda (fragment)
                     (eq (plist-get fragment :id) 'tasks))
                     fragments))
           (executions (seq-find
                        (lambda (fragment)
                          (eq (plist-get fragment :id) 'background))
                        fragments)))
      (should-not sandbox)
      (should (> (plist-get tasks :priority)
                 (plist-get executions :priority)))
      (should-not (seq-find (lambda (fragment)
                              (eq (plist-get fragment :id) 'agents))
                            fragments))
      (should (string-match-p
               (concat "\\` ─── background work · 1 agent · 2 commands \\[-\\] ─+\n"
                       "  ▸ Running   agent    /root/child\n"
                       "  ▸ Running   bash     make test\n"
                       "  ▸ Running   bash     make lint · /root/child\n"
                       "  \\[Manage\\]\n\\'")
               (substring-no-properties
                (plist-get executions :body))))
      ;; The panel needs a blank line under it or its last row reads as a
      ;; caption for what follows.  A zone trims `:body' to one newline,
      ;; so the blank has to travel as a suffix.
      (should (equal "\n" (plist-get executions :body-suffix)))))))

(niwa-deftest niwa-view--status-task-body ()
  ,test
  (test)
  :doc "header clicks toggle while task row clicks only place point"
  (niwa-view-test--with-buffers
    (switch-to-buffer view-buf)
    (let ((session (niwa-session--create
                    :name "task-mouse"
                    :tasks (list (niwa-task--create
                                  :id 1 :subject "Active task" :status 'pending)
                                 (niwa-task--create
                                  :id 2 :subject "Done task" :status 'completed)))))
      (with-current-buffer data-buf
        (setq-local niwa--session session))
      (niwa-view-test--insert-composer-draft "> draft\nsecond line" 2)
      (niwa-view--render-status data-buf)
      (dolist (expanded '(t nil))
        (let* ((header (plist-get (niwa-view-zone-fragment-bounds 'status 'tasks)
                                  :start))
               (command (lookup-key (get-text-property header 'keymap) [mouse-1])))
          (should (eq (get-text-property header 'mouse-face) 'highlight))
          (should (get-text-property header 'follow-link))
          (should (stringp (get-text-property header 'help-echo)))
          (funcall command
                   (list 'mouse-1 (list (selected-window) header '(0 . 0) 0))))
        (should (eq (niwa-view--status-task-show-completed-p) expanded))
        (should (eq (not (null (string-search "Done task" (buffer-string))))
                    expanded))
        (let* ((header (plist-get (niwa-view-zone-fragment-bounds 'status 'tasks)
                                  :start))
               (row (save-excursion (goto-char header) (forward-line 1) (point)))
               (command (lookup-key (get-text-property row 'keymap) [mouse-1])))
          (should-not (get-text-property row 'mouse-face))
          (should-not (get-text-property row 'follow-link))
          (funcall command
                   (list 'mouse-1 (list (selected-window) row '(0 . 1) 0)))
          (should (= (point) row))
          (should (eq (niwa-view--status-task-show-completed-p) expanded)))
        (should (equal (niwa-view-composer-draft-text) "> draft\nsecond line"))
        (should (= 2 (niwa-view-test--composer-point-offset)))))))

(niwa-deftest niwa-view-execution-state-changed-integration ()
  ,test
  (test)
  :doc "live-execution redraw preserves a multiline leading-> composer draft"
  (niwa-view-test--with-buffers
    (let ((session (niwa-session--create :name "execution-status"))
          (draft "> quoted\nsecond line"))
      (with-current-buffer data-buf
        (setq-local niwa--session session))
      (with-current-buffer view-buf
        (niwa-view-test--insert-composer-draft draft 4))
      (cl-letf (((symbol-function 'niwa-execution-count-user)
                 (lambda (seen-session)
                   (should (eq session seen-session))
                   1))
                ((symbol-function 'niwa-execution-list-user)
                 (lambda (_) '((:execution-id "e1" :owner "/root" :yielded t
                                :command "make test"))))
                ((symbol-function 'niwa-view--agent-status-collect)
                 #'ignore))
        (niwa-view-execution--state-changed session data-buf))
      (with-current-buffer view-buf
        (should (string= draft (niwa-view-composer-draft-text)))
        (should (= 4 (niwa-view-test--composer-point-offset)))
        (should (string-match-p
                 "Running   bash     make test"
                 (buffer-substring-no-properties
                  (point-min) (niwa-view-transcript-tail)))))))
  :doc "routes an agent execution update to its parent session view"
  (niwa-view-test--with-buffers
    (let ((session (niwa-session--create :name "agent-execution-status"))
          (agent-data (generate-new-buffer " *niwa-agent-execution*")))
      (unwind-protect
          (progn
            (with-current-buffer data-buf
              (setq-local niwa--session session))
            (with-current-buffer agent-data
              (setq-local
               niwa--agent-invocation
               (niwa-agent-invocation--create
                :parent-data-buffer data-buf)))
            (cl-letf (((symbol-function 'niwa-execution-count-user)
                       (lambda (seen-session)
                         (should (eq session seen-session))
                         1))
                      ((symbol-function 'niwa-execution-list-user)
                       (lambda (_) '((:execution-id "e1" :owner "/root" :yielded t
                                      :command "make test"))))
                      ((symbol-function 'niwa-view--agent-status-collect)
                       #'ignore))
              (niwa-view-execution--state-changed session agent-data))
            (with-current-buffer view-buf
              (should (string-match-p
                       "Running   bash     make test"
                       (buffer-substring-no-properties
                        (point-min) (niwa-view-transcript-tail))))))
        (kill-buffer agent-data)))))

(niwa-deftest niwa-view-execution-fragment-integration ()
  ,test
  (test)
  :doc "caps listed commands, expands locally, and offers explicit navigation and overflow management"
  (niwa-view-test--with-buffers
    (let ((session (niwa-session--create :name "status-executions"))
          shown opened)
      (with-current-buffer data-buf (setq-local niwa--session session))
      (cl-letf (((symbol-function 'niwa-execution-list-user)
                 (lambda (_)
                   (cl-loop for n from 1 to 6 collect
                            (list :execution-id (format "e%d" n)
                                  :owner "/root" :yielded t
                                  :tool-use-id (format "bash-%d" n)
                                  :output-tail "sample output" :state 'running
                                  :command (format "command %d" n)))))
                ((symbol-function 'niwa-execution-count-user) (lambda (_) 6))
                ((symbol-function 'niwa-view--agent-status-collect) #'ignore)
                ((symbol-function 'niwa-background-list-open)
                 (lambda (&optional _context) (setq opened t)))
                ((symbol-function 'niwa-view-audit-show-control-result)
                 (lambda (_) (error "Live activation scanned transcript history")))
                ((symbol-function 'niwa-view-audit-show-result)
                 (lambda (record)
                   (setq shown (list (plist-get record :execution-id)
                                     (plist-get record :tool-use-id))))))
        (with-current-buffer view-buf
          (niwa-view--render-status)
          (should-not (string-search "command 6" (buffer-string)))
          (goto-char (point-min))
          (search-forward "command 1")
          (funcall (get-text-property (1- (point)) 'niwa-view-zone-activate))
          (should (string-search "sample output" (buffer-string)))
          (should-not shown)
          (should-not opened)
          (goto-char (point-min))
          (search-forward "Show in conversation")
          (funcall (get-text-property (1- (point)) 'niwa-view-zone-activate))
          (should (equal '("e1" "bash-1") shown))
          (goto-char (point-min))
          (search-forward "[Show all 6]")
          (funcall (get-text-property (1- (point)) 'niwa-view-zone-activate))
          (should (string-search "command 6" (buffer-string)))
          (should-not opened)
          (goto-char (point-min))
          (search-forward "[Show fewer]")
          (funcall (get-text-property (1- (point)) 'niwa-view-zone-activate))
          (should-not (string-search "command 6" (buffer-string)))
          (goto-char (point-min))
          (search-forward "[Manage]")
          (funcall (get-text-property (1- (point)) 'niwa-view-zone-activate))
          (should opened))))))

(niwa-deftest niwa-view-execution-show-in-conversation-integration ()
  ,test
  (test)
  :doc "agent navigation supplies owner and captured nested tool identity to the navigator"
  (let (shown)
    (cl-letf (((symbol-function 'niwa-view-open-agent-transcript)
               (lambda (_) (ert-fail "Caller bypassed owner-aware navigation")))
              ((symbol-function 'niwa-view-audit-show-control-result)
               (lambda (_) (ert-fail "Live activation scanned transcript history")))
              ((symbol-function 'niwa-view-audit-show-result)
               (lambda (record) (setq shown record))))
      (niwa-view-execution-show-in-conversation
       '(:execution-id "exec-child" :tool-use-id "outer/1/1"
         :owner "/root/worker"))
      (should (equal (plist-get shown :owner) "/root/worker"))
      (should (equal (plist-get shown :tool-use-id) "outer/1/1"))
      (should (equal (plist-get shown :execution-id) "exec-child")))))

(niwa-deftest niwa-view-execution-manage-integration ()
  ,test
  (test)
  :doc "Manage opens the background-work cockpit"
  (niwa-view-test--with-buffers
    (let ((session (niwa-session--create :name "all")) opened)
      (with-current-buffer data-buf (setq-local niwa--session session))
      (cl-letf (((symbol-function 'niwa-background-list-open)
                 (lambda (&optional _context) (setq opened t)))
                ((symbol-function 'niwa-execution-count-user) (lambda (_) 1))
                ((symbol-function 'niwa-execution-list-user)
                 (lambda (_) '((:execution-id "e" :owner "/root" :yielded t)))))
        (with-current-buffer view-buf
          (niwa-view--render-status)
          (goto-char (point-min))
          (search-forward "[Manage]")
          (funcall (get-text-property (1- (point)) 'niwa-view-zone-activate))))
      (should opened))))

;;
;;; View lifecycle

(niwa-deftest niwa-view--on-view-killed
  (:doc "view kill hook cleans up queued interactions")
  ,test
  (test)

  :doc "killing the view aborts both queues and kills the data buffer"
  (let ((data-buf (generate-new-buffer " *test-data-kill-view*"))
        (view-buf (generate-new-buffer " *test-view-kill-view*"))
        (session (niwa-session-create
                  "main"
                  (niwa-workspace--create
                   :type 'project :id "/tmp/kill-view/"
                   :root "/tmp/kill-view/" :name "kill-view")))
        (outcomes nil))
    (unwind-protect
        (progn
          (with-current-buffer data-buf
            (org-mode)
            (setq-local niwa--session session))
          (niwa-view--setup view-buf data-buf)
          (setf (niwa-session-permission-queue session)
                (list (list :kind 'generic
                            :tool-name "Read"
                            :session session
                            :callback
                            (lambda (outcome)
                              (push (cons 'permission outcome) outcomes)))))
          (setf (niwa-session-pending-plan-approval session)
                (list :body "# Plan"
                            :chat-buffer data-buf
                            :session session
                            :callback
                            (lambda (outcome)
                              (push (cons 'plan outcome) outcomes))))
          (kill-buffer view-buf)
          (should-not (buffer-live-p view-buf))
          (should-not (buffer-live-p data-buf))
          (should (null (niwa-session-permission-queue session)))
          (should (null (niwa-session-pending-plan-approval session)))
          (should (equal '((plan . aborted) (permission . aborted))
                         outcomes)))
      (when (buffer-live-p view-buf) (kill-buffer view-buf))
      (when (buffer-live-p data-buf) (kill-buffer data-buf))))

  :doc "stops transfer polling before sealing without losing root registration"
  (niwa-view-test--with-buffers
    (let ((session (niwa-session--create :name "kill-transfer"))
          timer sealed)
      (with-current-buffer data-buf
        (setq-local niwa--session session))
      (with-current-buffer view-buf
        (niwa-view-control-transfer-initialize #'ignore #'ignore)
        (setq timer niwa-view--control-transfer-timer))
      (cl-letf (((symbol-function 'niwa-journal-capture-seal-and-schedule)
                 (lambda (owner data trigger &optional _captures)
                   (should (eq session owner))
                   (should (eq data-buf data))
                   (should (eq 'session-end trigger))
                   (should (eq data-buf
                               (niwa-session-control-transfer-root-buffer
                                session)))
                   (should-not (memq timer timer-list))
                   (with-current-buffer view-buf
                     (niwa-view--control-transfer-schedule view-buf)
                     (should-not niwa-view--control-transfer-timer))
                   (setq sealed t))))
        (kill-buffer view-buf))
      (should sealed)
      (should-not (buffer-live-p data-buf))
      (should-not (niwa-session-control-transfer-root-buffer session))))

  :doc "killing the data buffer does not re-enter its kill via the view"
  (niwa-view-test--with-buffers
    (let ((kill (symbol-function 'kill-buffer))
          (data-kills 0))
      (cl-letf (((symbol-function 'kill-buffer)
                 (lambda (&optional buffer)
                   (when (eq (get-buffer (or buffer (current-buffer))) data-buf)
                     (cl-incf data-kills))
                   (funcall kill buffer))))
        (kill-buffer data-buf))
      (should (= 1 data-kills))
      (should-not (buffer-live-p data-buf))
      (should-not (buffer-live-p view-buf))))

  :doc "killing an agent view detaches its observer from retained data"
  (let ((data-buf (generate-new-buffer " *test-agent-data-kill-view*"))
        (view-buf (generate-new-buffer " *test-agent-view-kill-view*"))
        (parent-view (generate-new-buffer " *test-parent-kill-view*"))
        (change-hook #'ignore))
    (unwind-protect
        (progn
          (with-current-buffer data-buf
            (org-mode)
            (setq-local niwa--view-buffer parent-view))
          (niwa-view--setup
           view-buf data-buf
           (list :agent-transcript-p t
                 :preserve-data-view-buffer t
                 :parent-view parent-view
                 :transcript-info '(:live-buffer t)))
          (with-current-buffer view-buf
            (setq-local niwa-view--live-source-change-hook change-hook)
            (setq-local niwa-view--live-data-tail-start
                        (with-current-buffer data-buf
                          (copy-marker (point-min))))
            (setq-local niwa-view--live-view-tail-start
                        (copy-marker (point-min))))
          (with-current-buffer data-buf
            (add-hook 'before-change-functions change-hook nil t))
          (kill-buffer view-buf)
          (should (buffer-live-p data-buf))
          (with-current-buffer data-buf
            (should-not (memq change-hook before-change-functions))))
      (when (buffer-live-p view-buf) (kill-buffer view-buf))
      (when (buffer-live-p data-buf) (kill-buffer data-buf))
      (when (buffer-live-p parent-view) (kill-buffer parent-view)))))

(niwa-deftest niwa-view--on-data-killed
  (:doc "data kill hook cleans up queued interactions")
  ,test
  (test)

  :doc "data teardown stops all executions in the session"
  (let ((session (niwa-session--create :name "kill"))
        (agent-buffer (generate-new-buffer " *test-data-kill-agent*"))
        stopped)
    (setf (niwa-session-agent-registry session)
          (list
           (cons "/root/worker"
                 (niwa-agent-record--create
                  :path "/root/worker"
                  :conversation-buffer agent-buffer))))
    (unwind-protect
        (progn
          (with-temp-buffer
            (setq-local niwa--session session)
            (cl-letf
                (((symbol-function 'niwa-view--abort-data-buffer) #'ignore)
                 ((symbol-function 'niwa-execution-teardown-session)
                  (lambda (owner-session)
                    (setq stopped owner-session))))
              (niwa-view--on-data-killed)))
          (should (eq session stopped))
          (should-not (buffer-live-p agent-buffer)))
      (when (buffer-live-p agent-buffer)
        (kill-buffer agent-buffer))))

  :doc "killing the data buffer aborts queued and direct interactions"
  (let ((data-buf (generate-new-buffer " *test-data-kill-data*"))
        (view-buf (generate-new-buffer " *test-view-kill-data*"))
        (session (niwa-session-create
                  "main"
                  (niwa-workspace--create
                   :type 'project :id "/tmp/kill-data/"
                   :root "/tmp/kill-data/" :name "kill-data")))
        (outcomes nil))
    (unwind-protect
        (progn
          (with-current-buffer data-buf
            (org-mode)
            (setq-local niwa--session session))
          (niwa-view--setup view-buf data-buf)
          (require 'niwa-interaction-prompt)
          (with-current-buffer data-buf
            (niwa--prompt-user-with-overlay
             "Confirm" "Direct request" "Proceed?" nil
             (lambda (outcome)
               (push (cons 'direct outcome) outcomes))))
          (setf (niwa-session-permission-queue session)
                (list (list :kind 'generic
                            :tool-name "Read"
                            :session session
                            :callback
                            (lambda (outcome)
                              (push (cons 'permission outcome) outcomes)))))
          (setf (niwa-session-pending-plan-approval session)
                (list :body "# Plan"
                            :chat-buffer data-buf
                            :session session
                            :callback
                            (lambda (outcome)
                              (push (cons 'plan outcome) outcomes))))
          (kill-buffer data-buf)
          (should-not (buffer-live-p data-buf))
          (should-not (buffer-live-p view-buf))
          (should (null (niwa-session-permission-queue session)))
          (should (null (niwa-session-pending-plan-approval session)))
          (should (equal '((direct . aborted)
                           (plan . aborted)
                           (permission . aborted))
                         outcomes)))
      (when (buffer-live-p view-buf) (kill-buffer view-buf))
      (when (buffer-live-p data-buf) (kill-buffer data-buf)))))

(niwa-deftest niwa-view--allow-session-close-p (:quiet t)
  ,test
  (test)
  :doc "pending publication blocks closing the data buffer"
  (let ((data-buf (generate-new-buffer " *test-pending-data*"))
        (view-buf (generate-new-buffer " *test-pending-data-view*"))
        (session (niwa-session-create
                  "main"
                  (niwa-workspace--create
                   :type 'project :id "/tmp/pending-data/"
                   :root "/tmp/pending-data/" :name "pending-data"))))
    (unwind-protect
        (progn
          (with-current-buffer data-buf
            (org-mode)
            (setq-local niwa--session session))
          (niwa-view--setup view-buf data-buf)
          (setf (niwa-session-pending-publication session)
                '(:reason "target unavailable"))
          (should-not (kill-buffer data-buf))
          (should (buffer-live-p data-buf))
          (should (buffer-live-p view-buf))
          (setf (niwa-session-pending-publication session) nil)
          (should (kill-buffer data-buf))
          (should-not (buffer-live-p view-buf)))
      (setf (niwa-session-pending-publication session) nil)
      (when (buffer-live-p view-buf) (kill-buffer view-buf))
      (when (buffer-live-p data-buf) (kill-buffer data-buf))))

  :doc "pending publication blocks closing the paired view"
  (let ((data-buf (generate-new-buffer " *test-pending-view-data*"))
        (view-buf (generate-new-buffer " *test-pending-view*"))
        (session (niwa-session-create
                  "main"
                  (niwa-workspace--create
                   :type 'project :id "/tmp/pending-view/"
                   :root "/tmp/pending-view/" :name "pending-view"))))
    (unwind-protect
        (progn
          (with-current-buffer data-buf
            (org-mode)
            (setq-local niwa--session session))
          (niwa-view--setup view-buf data-buf)
          (setf (niwa-session-pending-publication session)
                '(:reason "target unavailable"))
          (should-not (kill-buffer view-buf))
          (should (buffer-live-p view-buf))
          (should (buffer-live-p data-buf))
          (setf (niwa-session-pending-publication session) nil)
          (should (kill-buffer view-buf))
          (should-not (buffer-live-p data-buf)))
      (setf (niwa-session-pending-publication session) nil)
      (when (buffer-live-p view-buf) (kill-buffer view-buf))
      (when (buffer-live-p data-buf) (kill-buffer data-buf))))

  :doc "an unsettled remote mutation blocks close until acknowledgement"
  (let ((data-buf (generate-new-buffer " *test-unsettled-data*"))
        (view-buf (generate-new-buffer " *test-unsettled-view*"))
        (session (niwa-session-create
                  "main"
                  (niwa-workspace--create
                   :type 'project :id "/tmp/unsettled/"
                   :root "/tmp/unsettled/"
                   :name "unsettled"))))
    (unwind-protect
        (progn
          (with-current-buffer data-buf
            (org-mode)
            (setq-local niwa--session session))
          (niwa-view--setup view-buf data-buf)
          (setf (niwa-session-lease session)
                '(:state owned :unsettled-mutation t))
          (cl-letf (((symbol-function
                      'niwa-execution-unsettled-mutation-p)
                     (lambda (_session) t)))
            (should-not (kill-buffer data-buf)))
          (should (buffer-live-p data-buf))
          (should (buffer-live-p view-buf))
          (setf (niwa-session-lease session)
                '(:state owned :unsettled-mutation nil))
          (should (kill-buffer data-buf))
          (should-not (buffer-live-p view-buf)))
      (when (buffer-live-p view-buf) (kill-buffer view-buf))
      (when (buffer-live-p data-buf) (kill-buffer data-buf))))

  :doc "a foreign read-only inspector can close with a durable mutation latch"
  (let* ((root "/tmp/foreign-close/")
         (data-buf (generate-new-buffer " *test-foreign-close-data*"))
         (view-buf (generate-new-buffer " *test-foreign-close-view*"))
         (session
          (niwa-session-create
           "main"
           (niwa-workspace--create
            :type 'project :id root :root root :name "foreign-close"))))
    (unwind-protect
        (progn
          (with-current-buffer data-buf
            (org-mode)
            (setq-local niwa--session session))
          (niwa-view--setup view-buf data-buf)
          (setf (niwa-session-lease session)
                '(:state foreign :unsettled-mutation t))
          (with-current-buffer data-buf
            (setq-local niwa-session--read-only-mode t)
            (setq buffer-read-only t))
          (cl-letf (((symbol-function
                      'niwa-execution-unsettled-mutation-p)
                     (lambda (_session) t)))
            (should (kill-buffer data-buf)))
          (should-not (buffer-live-p view-buf))
          (should
           (plist-get (niwa-session-lease session)
                      :unsettled-mutation)))
      (when (buffer-live-p view-buf) (kill-buffer view-buf))
      (when (buffer-live-p data-buf)
        (with-current-buffer data-buf
          (setq buffer-read-only nil))
        (kill-buffer data-buf)))))

;;
;;; View command wiring

(niwa-deftest niwa-view-mode-map ()
  ,test
  (test)
  :doc "view mode binds the cockpit command"
  (should (eq (lookup-key niwa-view-mode-map (kbd "C-c C-o"))
              #'niwa-menu))
  (should-not (eq (lookup-key niwa-view-mode-map (kbd "C-c C-m"))
                  #'niwa-menu)))



(niwa-deftest niwa-view--prompt-on-screen-p ()
  ,test
  (test)
  :doc "keeps its answer until the buffer or the window start changes"
  (save-window-excursion
    (with-temp-buffer
      (insert "Answer\n"
              (propertize "Prompt\n" 'niwa-view-prompt-preview "Prompt")
              "More\n")
      (set-window-buffer (selected-window) (current-buffer))
      (set-window-start nil (point-min) t)
      (let* ((calls 0)
             (real (symbol-function 'vertical-motion))
             (window (selected-window)))
        (cl-letf (((symbol-function 'vertical-motion)
                   (lambda (&rest args) (cl-incf calls) (apply real args))))
          (should (niwa-view--prompt-on-screen-p window))
          ;; Header lines re-evaluate on every redisplay; nothing changed.
          (should (niwa-view--prompt-on-screen-p window))
          (should (= 1 calls))
          ;; An animation frame rewrites a display property only.
          (niwa-view--put-decorative-display 1 3 "An")
          (niwa-view--put-decorative-display 1 3 "AN")
          (should (niwa-view--prompt-on-screen-p window))
          (should (= 1 calls))
          ;; Hiding the prompt changes the buffer and the answer.
          (put-text-property (point-min) (point-max) 'invisible t)
          (should-not (niwa-view--prompt-on-screen-p window))
          (should (= 2 calls))
          (set-window-start nil (1+ (point-min)) t)
          (niwa-view--prompt-on-screen-p window)
          (should (= 3 calls))))))

  :doc "answers again when wrapping, text scale or line spacing changes"
  (save-window-excursion
    (with-temp-buffer
      (insert (make-string (* 4 (window-body-width) (window-body-height)) ?a)
              "\n"
              (propertize "Prompt\n" 'niwa-view-prompt-preview "Prompt"))
      (set-window-buffer (selected-window) (current-buffer))
      (set-window-start nil (point-min) t)
      (let ((window (selected-window)))
        ;; The wrapped line fills the window and hides the prompt.
        (setq-local truncate-lines nil)
        (should-not (niwa-view--prompt-on-screen-p window))
        (setq-local truncate-lines t)
        (should (niwa-view--prompt-on-screen-p window))
        (let* ((calls 0)
               (real (symbol-function 'vertical-motion)))
          (cl-letf (((symbol-function 'vertical-motion)
                     (lambda (&rest args) (cl-incf calls) (apply real args))))
            (setq-local line-spacing 2)
            (niwa-view--prompt-on-screen-p window)
            (should (= 1 calls))
            (text-scale-set 1)
            (niwa-view--prompt-on-screen-p window)
            (should (= 2 calls))
            ;; Rescaling edits the remapping list in place.
            (text-scale-set 2)
            (niwa-view--prompt-on-screen-p window)
            (should (= 3 calls))
            (niwa-view--prompt-on-screen-p window)
            (should (= 3 calls)))))))

  :doc "graphical visibility follows pixel layout, not canonical row counts"
  (save-window-excursion
    (with-temp-buffer
      (insert "Answer\n"
              (propertize "Prompt\n" 'niwa-view-prompt-preview "Prompt"))
      (set-window-buffer (selected-window) (current-buffer))
      (set-window-start nil (point-min) t)
      (let ((visible '(0 20)) (calls 0))
        (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t))
                  ((symbol-function 'pos-visible-in-window-p)
                   (lambda (_position _window partially)
                     (should partially)
                     (cl-incf calls)
                     visible))
                  ((symbol-function 'vertical-motion)
                   (lambda (&rest _) (ert-fail "Counted canonical rows"))))
          (should (niwa-view--prompt-on-screen-p (selected-window)))
          (should (niwa-view--prompt-on-screen-p (selected-window)))
          (should (= calls 1))
          ;; A larger font puts the prompt below the bottom of the window.
          (setq visible nil)
          (text-scale-set 5)
          (should-not (niwa-view--prompt-on-screen-p (selected-window)))
          (should (= calls 2))
          (setq visible '(0 20))
          (text-scale-set 0)
          (should (niwa-view--prompt-on-screen-p (selected-window)))
          (should (= calls 3))))))

  :doc "skips the on-screen walk without a compacted continuation prompt"
  (save-window-excursion
    (with-temp-buffer
      (insert "Answer\n")
      (set-window-buffer (selected-window) (current-buffer))
      (cl-letf (((symbol-function 'niwa-view--prompt-on-screen-p)
                 (lambda (_) (error "Walked the window")))
                ((symbol-function 'niwa-view--continuation-prompt) #'ignore))
        (should-not (niwa-view--sticky-prompt-line))))))

(niwa-deftest niwa-view--status-strip-abbreviated
  (:doc "Reuses a root abbreviation and replaces it when the root changes.")
  (with-temp-buffer
    (let ((calls 0))
      (cl-letf (((symbol-function 'abbreviate-file-name)
                 (lambda (path) (cl-incf calls) (concat "short:" path))))
        (should (equal "short:/one/"
                       (niwa-view--status-strip-abbreviated "/one")))
        (should (equal "short:/one/"
                       (niwa-view--status-strip-abbreviated "/one")))
        (should (= calls 1))
        (should (equal "short:/two/"
                       (niwa-view--status-strip-abbreviated "/two")))
        (should (= calls 2))))))

(provide 'test-niwa-view)

;;; test-niwa-view.el ends here
