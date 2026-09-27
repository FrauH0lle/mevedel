;;; test-mevedel-view.el -- Tests for mevedel-view -*- lexical-binding: t -*-

;;; Commentary:

;; Contract tests for chat-view coordination and shared surface behavior.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name
               load-file-name
               byte-compile-current-file))
          "helpers"))
(require 'mevedel-agent-control)
(require 'mevedel-agents)
(require 'mevedel-execution)
(require 'mevedel-execution-target)
(require 'mevedel-executions-list)
(require 'mevedel-view)
(require 'mevedel-view-segments)
(require 'mevedel-view-stream)
(require 'mevedel-menu)
(require 'mevedel-transport)
(require 'mevedel-structs)
(require 'mevedel-skills-core)
(require 'mevedel-workspace)
(require 'mevedel-plan-mode)
(require 'mevedel-session-persistence)
(require 'mevedel-session-artifacts)
(require 'mevedel-session-publication)
(require 'mevedel-session-recovery)
(require 'mevedel-permission-queue)

(defvar org-mode-hook)
(declare-function gptel-menu "ext:gptel-transient" ())
(declare-function org-entry-put "org" (pom property value))


;;
;;; Activation

(mevedel-deftest mevedel-view-activate-at-point
  (:doc "mouse activation reads properties from the clicked window")
  (let ((target (generate-new-buffer " *test-click-target*"))
        opened)
    (unwind-protect
        (progn
          (with-current-buffer target
            (insert "xagent")
            (put-text-property
             2 7 'mevedel-view-agent-path "/root/test"))
          (cl-letf
              (((symbol-function 'mevedel-view--event-position)
                (lambda (&optional _event) 2))
               ((symbol-function 'mouse-set-point)
                (lambda (_event)
                  (set-buffer target)
                  (goto-char 2)))
               ((symbol-function 'mevedel-view--position-in-input-region-p)
                (lambda (_position) nil))
               ((symbol-function 'mevedel-view-open-agent-transcript-at-point)
                (lambda (&optional _event)
                  (setq opened
                        (get-text-property
                         (point) 'mevedel-view-agent-path)))))
            (with-temp-buffer
              (mevedel-view-activate-at-point 'mouse)))
          (should (equal "/root/test" opened)))
      (when (buffer-live-p target)
        (kill-buffer target)))))


;;
;;; Rendering

(mevedel-deftest mevedel-view--schedule-render
  ()
  ,test
  (test)
  :doc "coalesces stream, tool-boundary, and full requests into one refresh"
  (mevedel-view-test--with-buffers
    (let ((mevedel-view-stream-render-delay 1)
          (mevedel-view-tool-boundary-render-delay 1)
          (mevedel-view-rerender-debounce 1)
          (scheduled 0) callback args fake-timers
          (incremental-count 0)
          (full-count 0))
      (with-current-buffer view-buf
        (setq mevedel-view--in-flight-turn-start
              (copy-marker mevedel-view--input-marker))
        (setq mevedel-view--data-turn-start
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
                     ((symbol-function 'mevedel-view--render-stream-update)
                      (lambda (_data-buffer) (cl-incf incremental-count)))
                     ((symbol-function 'mevedel-view-render-batched-full)
                      (lambda () (cl-incf full-count))))
            (with-current-buffer data-buf
              (mevedel-view-stream-schedule))
            (with-current-buffer view-buf
              (mevedel-view--schedule-tool-boundary-render data-buf))
            (mevedel-view-rerender view-buf)
            (should (= 1 scheduled))
            (with-current-buffer view-buf
              (should (eq 'full mevedel-view--pending-render-kind)))
            (apply callback args)
            (should (= 1 full-count))
            (should (= 0 incremental-count))
            (with-current-buffer view-buf
              (should-not mevedel-view--render-timer)
              (should-not mevedel-view--pending-render-kind))
            (setq callback nil args nil)
            (with-current-buffer data-buf
              (mevedel-view-stream-schedule))
            (with-current-buffer view-buf
              (mevedel-view--schedule-tool-boundary-render data-buf))
            (should (= 2 scheduled))
            (apply callback args)
            (should (= 1 full-count))
            (should (= 1 incremental-count)))
        (mapc #'cancel-timer fake-timers))))

  :doc "defers timer flushes while a remote operation is already in flight"
  (mevedel-view-test--with-buffers
    (let ((mevedel-view-rerender-debounce 1)
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
                     ((symbol-function 'mevedel-view-render-batched-full)
                      (lambda () (cl-incf full-count))))
            (mevedel-view-rerender view-buf)
            ;; A real handler frame, because that is what a render timer lands
            ;; inside.  It must not test `tramp-current-connection': that stays
            ;; set for the life of the process once any remote file is touched,
            ;; which postponed every remote render forever.
            (mevedel-transport--handler-advice
             (lambda (&rest _) (apply callback args))
             'file-exists-p "/ssh:user@host:/srv/x")
            (should (= 2 scheduled))
            (should (= 0 full-count))
            (with-current-buffer view-buf
              (should (eq 'full mevedel-view--pending-render-kind)))
            (apply callback args)
            (should (= 1 full-count)))
        (mapc #'cancel-timer fake-timers))))

  :doc "re-arms when the recorded timer is no longer on `timer-list'"
  (mevedel-view-test--with-buffers
    (let ((mevedel-view-rerender-debounce 1)
          (scheduled 0)
          lost)
      ;; Arm a timer on a discarded `timer-list' binding: the shape TRAMP's
      ;; suspended-timers window leaves behind when a stream or tool hook
      ;; schedules a render inside it.
      (let (timer-list)
        (setq lost (run-at-time 3600 nil #'ignore)))
      (should (timerp lost))
      (with-current-buffer view-buf
        (setq mevedel-view--render-timer lost))
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (&rest _) (cl-incf scheduled) 'scheduled)))
        (mevedel-view-rerender view-buf))
      (should (= 1 scheduled)))))

(mevedel-deftest mevedel-view--flush-scheduled-render ()
  ,test
  (test)
  :doc "keeps historical projection fixed while refreshing live chrome"
  (mevedel-view-test--with-buffers
    (let ((full-count 0)
          (incremental-count 0)
          (chrome-count 0))
      (with-current-buffer view-buf
        (setq-local mevedel-view--pending-render-kind 'full
                    mevedel-view--pending-render-data-buffer data-buf))
      (cl-letf
          (((symbol-function 'mevedel-view-historical-segment-p)
            (lambda () t))
           ((symbol-function 'mevedel-view-render-batched-full)
            (lambda (&rest _) (cl-incf full-count)))
           ((symbol-function 'mevedel-view--render-stream-update)
            (lambda (&rest _) (cl-incf incremental-count)))
           ((symbol-function 'mevedel-view--render-status)
            (lambda (&rest _) (cl-incf chrome-count)))
           ((symbol-function 'mevedel-view--interaction-rebuild)
            (lambda () (cl-incf chrome-count)))
           ((symbol-function 'mevedel-view--ensure-request-progress)
            (lambda (&rest _) (cl-incf chrome-count))))
        (mevedel-view--flush-scheduled-render view-buf))
      (with-current-buffer view-buf
        (should-not mevedel-view--pending-render-kind))
      (should (= 0 full-count))
      (should (= 0 incremental-count))
      (should (= 3 chrome-count))))

  :doc "an unattended view keeps its pending render until focus returns"
  (mevedel-view-test--with-buffers
    (let ((full-count 0)
          (unattended t))
      (with-current-buffer view-buf
        (setq-local mevedel-view--pending-render-kind 'full
                    mevedel-view--pending-render-data-buffer data-buf))
      (unwind-protect
          (cl-letf (((symbol-function 'mevedel-view--unattended-p)
                     (lambda (&rest _) unattended))
                    ((symbol-function 'mevedel-view--full-rerender)
                     (lambda (&rest _) (cl-incf full-count))))
            (mevedel-view--flush-scheduled-render view-buf)
            (with-current-buffer view-buf
              (should (= 0 full-count))
              (should (eq 'full mevedel-view--pending-render-kind))
              (should (eq data-buf mevedel-view--pending-render-data-buffer))
              (should-not mevedel-view--render-timer))
            ;; Focus returns: the resume hook re-arms the same render,
            ;; and the flush then runs it exactly once.
            (setq unattended nil)
            (mevedel-view--resume-attended-views)
            (with-current-buffer view-buf
              (should (mevedel--timer-pending-p mevedel-view--render-timer))
              (mevedel-view--schedule-render 'full data-buf 0))
            (with-current-buffer view-buf
              (should (= 1 full-count))
              (should-not mevedel-view--pending-render-kind)
              (should-not mevedel-view--render-timer)))
        (with-current-buffer view-buf
          (mevedel-view--cancel-scheduled-render))))))

(mevedel-deftest mevedel-view--flush-scheduled-render/tool-rows
  (:doc "coalesced full rendering subsumes rows while incremental retains them")
  (dolist (kind '(tools incremental full))
    (mevedel-view-test--with-buffers
      (let ((full 0) (incremental 0) refreshed)
        (with-current-buffer view-buf
          (mevedel-view-test--insert-composer-draft "> quoted\nsecond line" 3)
          (setq mevedel-view--pending-render-kind kind
                mevedel-view--pending-render-data-buffer data-buf
                mevedel-view--pending-tool-rows '("older" "current")))
        (cl-letf (((symbol-function 'mevedel-view-render-batched-full)
                   (lambda (&rest _) (cl-incf full)))
                  ((symbol-function 'mevedel-view--render-stream-update)
                   (lambda (&rest _) (cl-incf incremental)))
                  ((symbol-function 'mevedel-view--refresh-tool-row)
                   (lambda (_buffer id) (push id refreshed) t)))
          (mevedel-view--flush-scheduled-render view-buf))
        (should (= full (if (eq kind 'full) 1 0)))
        (should (= incremental (if (eq kind 'incremental) 1 0)))
        (should (equal refreshed (unless (eq kind 'full) '("current" "older"))))
        (with-current-buffer view-buf
          (should-not mevedel-view--pending-tool-rows)
          (should-not mevedel-view--pending-render-kind)
          (should (equal "> quoted\nsecond line" (mevedel-view--input-text)))
          (should (= 3 (- (point) (mevedel-view--input-start)))))))))

(mevedel-deftest mevedel-view--schedule-render/tool-rows
  (:doc "tool updates never downgrade pending transcript projection")
  (mevedel-view-test--with-buffers
    (unwind-protect
        (with-current-buffer view-buf
          (cl-loop for kind in '(tools incremental tools full tools incremental)
                   for expected in '(tools incremental incremental full full full)
                   do (mevedel-view--schedule-render kind data-buf 10)
                   (should (eq expected mevedel-view--pending-render-kind)))
          (setq mevedel-view--pending-tool-rows '("stale"))
          (mevedel-view--cancel-scheduled-render)
          (should-not mevedel-view--pending-tool-rows))
      (with-current-buffer view-buf (mevedel-view--cancel-scheduled-render)))))

(mevedel-deftest mevedel-view--unattended-p ()
  ,test
  (test)
  :doc "a view without a window, or on a terminal frame, is attended"
  (mevedel-view-test--with-buffers
    (should-not (mevedel-view--unattended-p view-buf))
    (let ((window (selected-window))
          (original (window-buffer (selected-window))))
      (unwind-protect
          (progn
            (set-window-buffer window view-buf)
            ;; The batch frame is visible, non-graphic, and reports no
            ;; focus; a terminal cannot tell, so it counts as attended.
            (should-not (mevedel-view--unattended-p view-buf))
            (with-current-buffer view-buf
              (should-not (mevedel-view--unattended-p))))
        (set-window-buffer window original))))

  :doc "a graphical frame without focus or visibility is unattended"
  (mevedel-view-test--with-buffers
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
            (should (mevedel-view--unattended-p view-buf))
            (setq focused t)
            (should-not (mevedel-view--unattended-p view-buf))
            (setq visible 'icon)
            (should (mevedel-view--unattended-p view-buf))
            (setq visible nil)
            (should (mevedel-view--unattended-p view-buf)))
        (set-window-buffer window original))))

  :doc "a child frame reports the focus of its top-level ancestor"
  (mevedel-view-test--with-buffers
    (let ((window (selected-window))
          (original (window-buffer (selected-window)))
          (parent-focused nil)
          asked)
      (unwind-protect
          (cl-letf (((symbol-function 'frame-parent)
                     (lambda (frame) (and (not (eq frame 'parent)) 'parent)))
                    ((symbol-function 'display-graphic-p)
                     (lambda (&rest _) t))
                    ((symbol-function 'frame-focus-state)
                     (lambda (&optional frame)
                       (push frame asked)
                       (and (eq frame 'parent) parent-focused))))
            (set-window-buffer window view-buf)
            (should (mevedel-view--unattended-p view-buf))
            (should (equal '(parent) (delete-dups asked)))
            (setq parent-focused t)
            (should-not (mevedel-view--unattended-p view-buf)))
        (set-window-buffer window original)))))

(mevedel-deftest mevedel-view--resume-attended-views ()
  ,test
  (test)
  :doc "re-arms only attended views that still owe a render"
  (mevedel-view-test--with-buffers
    (let ((unattended nil))
      (unwind-protect
          (cl-letf (((symbol-function 'mevedel-view--unattended-p)
                     (lambda (&rest _) unattended)))
            ;; Nothing pending: nothing armed.
            (mevedel-view--resume-attended-views)
            (with-current-buffer view-buf
              (should-not mevedel-view--render-timer)
              (setq-local mevedel-view--pending-render-kind 'incremental
                          mevedel-view--pending-render-data-buffer data-buf))
            ;; Still unattended: keep waiting.
            (setq unattended t)
            (mevedel-view--resume-attended-views)
            (with-current-buffer view-buf
              (should-not mevedel-view--render-timer))
            ;; Attended again: the stored kind is rescheduled once, and a
            ;; second focus change does not arm a second timer.
            (setq unattended nil)
            (mevedel-view--resume-attended-views)
            (with-current-buffer view-buf
              (should (mevedel--timer-pending-p mevedel-view--render-timer))
              (should (eq 'incremental mevedel-view--pending-render-kind))
              (let ((timer mevedel-view--render-timer))
                (mevedel-view--resume-attended-views)
                (should (eq timer mevedel-view--render-timer)))))
        (with-current-buffer view-buf
          (mevedel-view--cancel-scheduled-render)))))

  :doc "skips a pending render whose data buffer died"
  (mevedel-view-test--with-buffers
    (let ((dead (generate-new-buffer " *dead-data*")))
      (kill-buffer dead)
      (with-current-buffer view-buf
        (setq-local mevedel-view--pending-render-kind 'full
                    mevedel-view--pending-render-data-buffer dead))
      (cl-letf (((symbol-function 'mevedel-view--unattended-p)
                 (lambda (&rest _) nil)))
        (mevedel-view--resume-attended-views))
      (with-current-buffer view-buf
        (should-not mevedel-view--render-timer)
        (mevedel-view--cancel-scheduled-render)))))

(mevedel-deftest mevedel-view--status-strip-button ()
  ,test
  (test)
  :doc "status strip button routes clicks to the requested cockpit area"
  (let ((button (mevedel-view--status-strip-button
                 "Mode" 'mode "Open mode cockpit"))
        called)
    (cl-letf (((symbol-function 'mevedel-menu-open)
               (lambda (area) (setq called area))))
      (let* ((map (get-text-property 0 'local-map button))
             (command (lookup-key map [tab-line mouse-1])))
        (should (eq (get-text-property 0 'mevedel-view-cockpit-area button)
                    'mode))
        (should (string= button "Mode"))
        (should command)
        (funcall command nil)
        (should (eq called 'mode))))))

(mevedel-deftest mevedel-view--pinned-prompt ()
  ,test
  (test)
  :doc "header follows the prompt above the top edge, not point or the newest prompt"
  (save-window-excursion
    (mevedel-view-test--with-buffers
      (dolist (prompt '("First question" "Second question"))
        (mevedel-view-test--insert-data data-buf
                                        (format "*** %s\n" prompt) nil)
        (mevedel-view-test--insert-data data-buf
                                        (apply #'concat (make-list 100 "response line\n"))
                                        'response))
      (with-current-buffer view-buf
        (switch-to-buffer view-buf)
        (mevedel-view--full-rerender)
        (set-window-buffer (selected-window) view-buf)
        (goto-char (point-min))
        (search-forward "First question")
        (let ((first (line-beginning-position)))
          (search-forward "Second question")
          (let ((second (line-beginning-position)))
            (set-window-start nil (point-min) t)
            (should-not (mevedel-view--sticky-prompt-line))
            (goto-char second)
            (set-window-start nil first t)
            (should (get-text-property
                     (save-excursion (goto-char first)
                                     (search-backward "You") (point))
                     'mevedel-view-prompt-preview))
            (should (equal first (window-start)))
            (should (mevedel-view--pinned-prompt (selected-window)))
            (should (string-search "First question"
                                   (mevedel-view--sticky-prompt-line)))
            (should-not (string-search "First question"
                                       (mevedel-view--status-strip)))
            (should (string-search "mevedel/"
                                   (mevedel-view--status-strip)))
            (set-window-start
             nil (save-excursion (goto-char second)
                                 (search-backward "You\n") (point)) t)
            (should-not (mevedel-view--pinned-prompt (selected-window)))
            (set-window-start nil second t)
            (should (string-search "Second question"
                                   (mevedel-view--sticky-prompt-line))))))))

  :doc "two windows and a status-strip cache follow their own top edges"
  (save-window-excursion
    (mevedel-view-test--with-buffers
      (dolist (prompt '("Alpha question" "Beta question"))
        (mevedel-view-test--insert-data data-buf (format "*** %s\n" prompt) nil)
        (mevedel-view-test--insert-data
         data-buf (apply #'concat (make-list 60 "answer\n")) 'response))
      (with-current-buffer view-buf
        (mevedel-view--full-rerender)
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
            (should (string-search "Alpha question" (mevedel-view--sticky-prompt-line))))
          (with-selected-window right
            (should (string-search "Beta question" (mevedel-view--sticky-prompt-line))))
          (with-selected-window left
            (should (string-search "Alpha question" (mevedel-view--sticky-prompt-line))))))))

  :doc "a folded summary pins as soon as only its blank separator remains"
  (save-window-excursion
    (mevedel-view-test--with-buffers
      (mevedel-view-test--insert-data data-buf
                                      "*** First question\nMore text\n" nil)
      (mevedel-view-test--insert-data
       data-buf (apply #'concat (make-list 100 "answer\n")) 'response)
      (with-current-buffer view-buf
        (mevedel-view--full-rerender)
        (set-window-buffer (selected-window) view-buf)
        (goto-char (point-min))
        (search-forward "You\n")
        (let ((header (match-beginning 0)))
          (goto-char header)
          (mevedel-view--collapse-turn)
          (set-window-start nil header t)
          (should-not (mevedel-view--pinned-prompt (selected-window)))
          (let ((separator (save-excursion
                             (goto-char header)
                             (forward-line 1)
                             (point))))
            (set-window-start nil separator t)
            (should (= separator (window-start)))
            (should (equal (cons header "First question More text")
                           (mevedel-view--pinned-prompt
                            (selected-window))))))))))

(mevedel-deftest mevedel-view--sticky-prompt-line ()
  ,test
  (test)
  :doc "two windows share stable rows but compute different prompt text"
  (save-window-excursion
    (mevedel-view-test--with-buffers
      (mevedel-view-test--insert-data data-buf "*** First question\n" nil)
      (mevedel-view-test--insert-data
       data-buf (apply #'concat (make-list 100 "answer\n")) 'response)
      (with-current-buffer view-buf
        (mevedel-view--full-rerender)
        (let ((window (selected-window))
              (other (split-window-right)))
          (set-window-buffer window view-buf)
          (set-window-buffer other view-buf)
          (goto-char (point-min))
          (search-forward "answer")
          (set-window-start window (line-beginning-position) t)
          (set-window-start other (point-min) t)
          (should (equal header-line-format
                         '(:eval (mevedel-view--sticky-prompt-line))))
          (should (equal tab-line-format
                         '(:eval (mevedel-view--status-strip))))
          (with-selected-window window
            (should (string-search "First question"
                                   (mevedel-view--sticky-prompt-line))))
          (with-selected-window other
            (should-not (mevedel-view--sticky-prompt-line)))
          (set-window-start window (point-min) t)
          (should-not (mevedel-view--sticky-prompt-line))
          (set-window-buffer window data-buf)
          (with-current-buffer data-buf
            (should-not (local-variable-p 'tab-line-format))))))))

(mevedel-deftest mevedel-view--continuation-prompt ()
  ,test
  (test)
  :doc "a copied two-prompt tail cannot pin its later prompt over earlier content"
  (let* ((directory (make-temp-file "mevedel-sticky-tail-" t))
         (session (mevedel-session--create
                   :authority-mode 'pid-lock :name "tail"
                   :save-path (file-name-as-directory directory)
                   :current-segment 2))
         (path (mevedel-session-artifacts-segment-path directory 1)))
    (unwind-protect
        (progn
          (with-temp-buffer
            (org-mode)
            (insert ":PROPERTIES:\n:GPTEL_BOUNDS: nil\n:END:\n\n")
            (insert "*** Before tail\n")
            (insert (propertize "Answer before tail\n" 'gptel 'response))
            (insert "*** First copied prompt\n")
            (insert (propertize "Answer in tail\n" 'gptel 'response))
            (insert "*** Second copied prompt\n")
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
            (setf (mevedel-session-prompt-index session)
                  (list (cons 1 (mevedel-session-artifacts-collect-prompts
                                 (current-buffer)))))
            (write-region (point-min) (point-max) path nil 'silent))
          (save-window-excursion
            (mevedel-view-test--with-buffers
              (with-current-buffer data-buf
                (setq-local mevedel--session session)
                (insert ":PROPERTIES:\n:MEVEDEL_SEGMENT_TAIL_PROMPTS: 2\n:END:\n\n")
                (insert (mevedel-session-artifacts-summary-block "summary"))
                (insert (propertize
                         (apply #'concat (make-list 80 "Continuation answer\n"))
                         'gptel 'response))
                (insert "*** First copied prompt\n")
                (insert (propertize "Answer in tail\n" 'gptel 'response))
                (insert "*** Second copied prompt\n")
                (insert (propertize
                         (apply #'concat (make-list 80 "More answer\n"))
                         'gptel 'response)))
              (with-current-buffer view-buf
                (switch-to-buffer view-buf)
                (mevedel-view--full-rerender)
                (set-window-point nil (point-min))
                (set-window-start nil (point-min) t)
                (should (string-search "Before tail"
                                       (mevedel-view--sticky-prompt-line)))
                (should-not (string-search "Second copied prompt"
                                           (mevedel-view--sticky-prompt-line)))
                (goto-char (point-min))
                (search-forward "First copied prompt")
                (search-backward "You\n")
                (set-window-start nil (point) t)
                (should-not (mevedel-view--sticky-prompt-line))
                (goto-char (point-min))
                (search-forward "More answer")
                (set-window-start nil (line-beginning-position) t)
                (should (string-search "Second copied prompt"
                                       (mevedel-view--sticky-prompt-line)))))))
      (delete-directory directory t)))

  :doc "a compacted continuation pins its source prompt, but clear does not"
  (let* ((directory (make-temp-file "mevedel-sticky-continuation-" t))
         (path (mevedel-session-artifacts-segment-path directory 1))
         (session (mevedel-session--create
                   :authority-mode 'pid-lock
                   :name "continuation"
                   :save-path (file-name-as-directory directory)
                   :current-segment 2))
         prompt-position indexed)
    (unwind-protect
        (progn
          (with-temp-buffer
            (org-mode)
            (insert ":PROPERTIES:\n:GPTEL_BOUNDS: nil\n:END:\n\n")
            (setq prompt-position (point))
            (insert "*** Original prompt with a second line\nMore details\n")
            (setq indexed (car (mevedel-session-artifacts-collect-prompts
                                (current-buffer))))
            (write-region (point-min) (point-max) path nil 'silent))
          (setf (mevedel-session-prompt-index session)
                (list (cons 1 (list (append indexed '(:cum-turn 1))))))
          (save-window-excursion
            (mevedel-view-test--with-buffers
              (with-current-buffer data-buf
                (setq-local mevedel--session session)
                (insert (mevedel-session-artifacts-summary-block "summary"))
                (insert (propertize "Continuation answer\n" 'gptel 'response)))
              (with-current-buffer view-buf
                (switch-to-buffer view-buf)
                (mevedel-view--full-rerender)
                (should (string-search
                         "Original prompt with a second line More details"
                         (mevedel-view--sticky-prompt-line)))
                (should (equal (list :segment 1 :pos prompt-position)
                               (cl-subseq (mevedel-view--continuation-prompt) 0 4)))
                (mevedel-view-segments-jump-to-prompt
                 1 prompt-position (selected-window))
                (should (= 1 (mevedel-view-segments-current-number)))
                (should (looking-at-p "You"))
                (should-not (mevedel-view--continuation-prompt))
                (mevedel-view-return-to-latest-segment)
                (should (string-search "Original prompt with a second line"
                                       (mevedel-view--sticky-prompt-line)))
                ;; A second compacted segment has no indexed prompt of its
                ;; own, but still belongs to the original conversation.
                (with-current-buffer data-buf
                  (write-region
                   (point-min) (point-max)
                   (mevedel-session-artifacts-segment-path directory 2)
                   nil 'silent)
                  (setf (mevedel-session-current-segment session) 3)
                  (let ((inhibit-read-only t))
                    (erase-buffer)
                    (insert (mevedel-session-artifacts-summary-block "again"))))
                (mevedel-view--full-rerender)
                (should (string-search "Original prompt with a second line"
                                       (mevedel-view--sticky-prompt-line)))
                (with-current-buffer data-buf
                  (insert "*** New local prompt\n")
                  (insert (propertize
                           (apply #'concat (make-list 80 "answer line\n"))
                           'gptel 'response)))
                (mevedel-view--full-rerender)
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
                  (should-not (mevedel-view--sticky-prompt-line))
                  (set-window-start nil header t)
                  (should-not (mevedel-view--sticky-prompt-line))
                  (set-window-start nil answer t)
                  (should (string-search "New local prompt"
                                         (mevedel-view--sticky-prompt-line))))
                ;; An empty fresh segment is a /clear boundary, even if
                ;; it is subsequently compacted into a new live segment.
                (with-temp-buffer
                  (write-region (point-min) (point-max)
                                (mevedel-session-artifacts-segment-path
                                 directory 2) nil 'silent))
                (with-current-buffer data-buf
                  (write-region
                   (point-min) (point-max)
                   (mevedel-session-artifacts-segment-path directory 3)
                   nil 'silent)
                  (setf (mevedel-session-current-segment session) 4))
                (mevedel-view--full-rerender)
                (should-not (mevedel-view--continuation-prompt))
                (with-current-buffer data-buf
                  (let ((inhibit-read-only t))
                    (erase-buffer)
                    (insert "*** Fresh after clear\n")))
                (mevedel-view--full-rerender)
                (should-not (mevedel-view--sticky-prompt-line))))))
      (delete-directory directory t))))

(mevedel-deftest mevedel-view--pinned-prompt-button ()
  ,test
  (test)
  :doc "a narrow preview preserves controls and escapes literal percent signs"
  (let* ((button (mevedel-view--pinned-prompt-button "Unicode ∑ 100% complete" 4 18))
         (map (get-text-property 0 'local-map button)))
    (should (<= (string-width
                 (replace-regexp-in-string "%%" "%" button t t)) 18))
    (should (string-match-p "%%" button))
    (should (string-suffix-p "…" button))
    (should (lookup-key map [header-line mouse-1])))

  :doc "exhausted preview budgets omit the prompt entirely"
  (dolist (width '(-10 0))
    (should (equal "" (mevedel-view--pinned-prompt-button
                       "Long prompt that must yield to controls" 4 width))))
  (save-window-excursion
    (mevedel-view-test--with-buffers
      (mevedel-view-test--insert-data data-buf "*** Very long prompt\n" nil)
      (mevedel-view-test--insert-data data-buf "Answer.\n" 'response)
      (with-current-buffer view-buf
        (switch-to-buffer view-buf)
        (mevedel-view--full-rerender)
        (goto-char (point-min))
        (search-forward "Answer.")
        (set-window-start nil (line-beginning-position) t)
        (cl-letf (((symbol-function 'mevedel-view--status-strip-width)
                   (lambda () 1)))
          (let ((header (mevedel-view--status-strip)))
            (should (string-match-p "ask · idle" header))
            (should-not (string-match-p "Very long prompt" header))
            (should (string-search "Very long prompt"
                                   (mevedel-view--sticky-prompt-line)))))))))

(mevedel-deftest mevedel-view--jump-to-pinned-prompt ()
  ,test
  (test)
  :doc "clicking a pinned folded prompt expands it and brings it to the top"
  (save-window-excursion
    (mevedel-view-test--with-buffers
      (mevedel-view-test--insert-data
       data-buf (concat "*** Long question\n"
                        (apply #'concat (make-list 18 "additional detail\n"))) nil)
      (mevedel-view-test--insert-data
       data-buf (apply #'concat (make-list 80 "answer\n")) 'response)
      (with-current-buffer view-buf
        (mevedel-view--full-rerender)
        (set-window-buffer (selected-window) view-buf)
        (goto-char (point-min))
        (search-forward "You\n")
        (let ((header (match-beginning 0)))
          (should (get-text-property (point) 'mevedel-view-collapsed))
          (mevedel-view-render-toggle-user-input)
          (goto-char header)
          (mevedel-view--collapse-turn)
          (should (string-prefix-p
                   "Long question additional detail"
                   (get-text-property header 'mevedel-view-prompt-preview)))
          (mevedel-view--jump-to-pinned-prompt header)
          (should (= header (window-start)))
          (should-not (get-text-property header 'mevedel-view-collapsed))
          (forward-line 1)
          (should-not (get-text-property (point) 'mevedel-view-collapsed))))))

  :doc "jump expands an auto-folded input without changing the composer draft"
  (save-window-excursion
    (mevedel-view-test--with-buffers
      (mevedel-view-test--insert-data
       data-buf (concat "*** Multi line prompt\n"
                        (apply #'concat (make-list 20 "body line\n"))) nil)
      (mevedel-view-test--insert-data data-buf "Answer.\n" 'response)
      (with-current-buffer view-buf
        (mevedel-view--full-rerender)
        (set-window-buffer (selected-window) view-buf)
        (mevedel-view-test--insert-composer-draft "> draft\nsecond line")
        (goto-char (point-min))
        (search-forward "You\n")
        (let ((header (match-beginning 0)))
          (should (get-text-property (point) 'mevedel-view-collapsed))
          (mevedel-view--jump-to-pinned-prompt header)
          (should (= header (window-start)))
          (forward-line 1)
          (should (looking-at-p "Multi line prompt"))
          (should-not (get-text-property (point) 'mevedel-view-collapsed))
          (should (string= "> draft\nsecond line"
                           (buffer-substring-no-properties
                            (mevedel-view--input-start) (point-max))))))))

  :doc "a preview click targets its window when another buffer is selected"
  (save-window-excursion
    (mevedel-view-test--with-buffers
      (mevedel-view-test--insert-data data-buf "*** First question\n" nil)
      (mevedel-view-test--insert-data
       data-buf (apply #'concat (make-list 100 "answer\n")) 'response)
      (with-current-buffer view-buf
        (mevedel-view--full-rerender)
        (mevedel-view-test--insert-composer-draft "> draft\nsecond line"))
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
                 (button (mevedel-view--pinned-prompt-button
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
                             (mevedel-view--input-text))))))))))

(mevedel-deftest mevedel-view--status-strip ()
  ,test
  (test)
  :doc "status strip root label truncates to the workspace tail, then disappears"
  (let ((root "~/Projekte/mevedel/"))
    (should (equal root
                   (mevedel-view--status-strip-root-label root 24)))
    (should (equal "…/mevedel/"
                   (mevedel-view--status-strip-root-label root 10)))
    (should (equal ""
                   (mevedel-view--status-strip-root-label root 9))))

  :doc "a long session name yields to controls; the prompt has its own row"
  (save-window-excursion
    (mevedel-view-test--with-buffers
      (mevedel-view-test--insert-data data-buf "*** Short prompt\n" nil)
      (mevedel-view-test--insert-data
       data-buf (apply #'concat (make-list 100 "answer\n")) 'response)
      (with-current-buffer data-buf
        (setq-local mevedel--session
                    (mevedel-session--create :name (make-string 60 ?S))
                    gptel-model nil))
      (with-current-buffer view-buf
        (mevedel-view--full-rerender)
        (set-window-buffer (selected-window) view-buf)
        (goto-char (point-min))
        (search-forward "answer")
        (set-window-start nil (line-beginning-position) t)
        (dolist (width '(35 50 80))
          (cl-letf (((symbol-function 'mevedel-view--status-strip-width)
                     (lambda () width)))
            (let ((line (mevedel-view--status-strip)))
              (should (<= (string-width line) width))
              (should (string-match-p
                       (regexp-quote "ask · idle · model none · 0 tools")
                       line))
              (if (= width 35)
                  (should-not (string-search "S" line))
                (should (string-match-p "\\`S+…" line))
                (should (text-property-any
                         0 (length line) 'mevedel-view-cockpit-area 'top line)))
              (should-not (string-search "Short prompt" line))
              (should (string-search "Short prompt"
                                     (mevedel-view--sticky-prompt-line))))))
        (set-window-start nil (point-min) t)
        (cl-letf (((symbol-function 'mevedel-view--status-strip-width)
                   (lambda () 120)))
          (let ((line (mevedel-view--status-strip)))
            (should (string-prefix-p (make-string 60 ?S) line))
            (should (string-search "0 tools" line)))))))

  :doc "status strip shows mevedel-owned session orientation instead of the data header"
  (let* ((root (make-temp-file "mevedel-status-root-" t))
         (workspace (mevedel-workspace-get-or-create
                     'project (format "status-%s" root) root "mevedel"))
         (session (mevedel-session-create "main" workspace)))
    (setf (mevedel-session-permission-mode session) 'ask)
    (unwind-protect
        (mevedel-view-test--with-buffers
          (with-current-buffer data-buf
            (setq-local default-directory (file-name-as-directory root))
            (setq-local header-line-format "GPTEL HEADER")
            (setq-local mevedel--session session)
            (setq-local gptel-model 'gpt-5.5)
            (setq-local gptel-tools '(read edit)))
          (with-current-buffer view-buf
            (let ((line (mevedel-view--status-strip)))
              (should (string-prefix-p "main  " line))
              (should (string-match-p
                       (regexp-quote
                        (file-name-nondirectory
                         (directory-file-name root)))
                       line))
              (should (string-match-p
                        (regexp-quote "ask · idle · gpt-5.5 · 2 tools")
                       line))
              (should-not (string-match-p "mevedel:" line))
              (should-not (string-match-p "\\[gpt-5\\.5\\]" line))
              (should-not (string-match-p "\\[2 tools\\]" line))
              (should-not (string-match-p "GPTEL HEADER" line)))))
	  (when (file-directory-p root)
	    (delete-directory root t))))

  :doc "status strip preserves the model none label"
  (mevedel-view-test--with-buffers
    ;; The label describes an unconfigured gptel, so the data buffer must
    ;; not inherit the harness default model.
    (with-current-buffer data-buf
      (setq-local gptel-model nil))
    (with-current-buffer view-buf
      (let ((line (mevedel-view--status-strip)))
        (should (string-match-p
                 (regexp-quote "ask · idle · model none · 0 tools")
                 line)))))

  :doc "status strip identifies the session execution target"
  (mevedel-view-test--with-buffers
    (let* ((target (mevedel-execution-target-create
                    "/ssh:user@host:/srv/project/"))
           (remote-workspace
            (mevedel-workspace--create
             :type 'project :id "remote" :root "/ssh:user@host:/srv/project/"
             :name "remote"))
           (remote-session
            (mevedel-session--create
             :name "remote" :workspace remote-workspace
             :working-directory "/ssh:user@host:/srv/project/"
             :execution-target target :permission-mode 'ask)))
      (with-current-buffer data-buf
        (setq-local mevedel--session remote-session))
      (with-current-buffer view-buf
        (let ((line (mevedel-view--status-strip)))
          (should (string-match-p "ssh:user@host" line))
          (should (text-property-any
                   0 (length line) 'mevedel-view-cockpit-area 'top line))))
      (with-current-buffer data-buf
        (setq-local mevedel--session nil))))

  :doc "target, readiness, lease, and publication refreshes preserve a multiline leading-> composer draft"
  (mevedel-view-test--with-buffers
    (let* ((target (mevedel-execution-target-create
                    "/ssh:user@host:/srv/project/"))
           (workspace (mevedel-workspace--create
                       :type 'project :id "remote-status"
                       :root "/ssh:user@host:/srv/project/"
                       :name "remote-status"))
           (session (mevedel-session--create
                     :name "remote-status" :workspace workspace
                     :working-directory "/ssh:user@host:/srv/project/"
                     :execution-target target
                     :permission-mode 'ask))
           (draft "> quoted\nsecond line")
           (point-offset 4))
      (unwind-protect
          (progn
            (with-current-buffer data-buf
              (setq-local mevedel--session session))
            (with-current-buffer view-buf
              (mevedel-view-interaction-initialize))
            (with-current-buffer view-buf
              (mevedel-view-test--insert-composer-draft draft point-offset)
              (should (string-match-p "ssh:user@host"
                                      (mevedel-view--status-strip))))
            (setf (mevedel-session-lease session)
                  '(:state lost :unsettled-mutation nil)
                  (mevedel-session-pending-publication session)
                  '(:reason "remote write failed")
                  (mevedel-execution-target-readiness target)
                  '(:status ready
                    :sandbox-mode best-effort
                    :sandbox-status bubblewrap))
            (mevedel-session-recovery-refresh-session-buffers session)
            (with-current-buffer view-buf
              (force-mode-line-update t)
              (let ((line (mevedel-view--status-strip))
                    (header (mevedel-menu--header)))
                (should (string-match-p "ssh:user@host" line))
                (should (string-match-p "lease lost" line))
                (should (string-match-p "publication pending" line))
                ;; Nominal readiness stays out of the cockpit header; the
                ;; lost lease and pending publication earn its alert line.
                (should (string-match-p "lease lost" header))
                (should (string-match-p "publication pending" header))
                (should-not (string-match-p "sandbox bubblewrap" header))
                (should (string= draft (mevedel-view--input-text)))
                (should (= (point)
                           (+ (mevedel-view--input-start) point-offset))))))
        (setf (mevedel-session-pending-publication session) nil)
        (when (buffer-live-p data-buf)
          (with-current-buffer data-buf
            (setq-local mevedel--session nil))))))

  :doc "status strip reuses unchanged output and rebuilds after state changes"
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (let ((first (mevedel-view--status-strip))
            second changed)
        (setq second (mevedel-view--status-strip))
        (should (eq first second))
        (with-current-buffer data-buf
          (setq-local gptel-tools '(read)))
        (setq changed (mevedel-view--status-strip))
        (should-not (eq second changed))
        (should (string-match-p "1 tool" changed)))))

  :doc "status strip shows Plan together with its permission policy"
  (mevedel-view-test--with-buffers
    (let ((session (mevedel-session--create
                    :name "main" :permission-mode 'full-auto :plan-mode t)))
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (with-current-buffer view-buf
        (should (string-match-p
                 (regexp-quote "Plan/full-auto · idle")
                 (mevedel-view--status-strip))))))

  :doc "status strip shows phase-free Goal status and turn accounting"
  (mevedel-view-test--with-buffers
    (let* ((goal (mevedel-goal--create
                  :status 'active :turns-run 5))
           (session (mevedel-session--create
                     :name "main" :goal goal :preset-name 'team)))
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (with-current-buffer view-buf
        (let ((line (mevedel-view--status-strip)))
          (should (string-match-p "active · 5 turns" line))
          (should (string-match-p "preset team" line))
          (dolist (area '(goal preset))
            (should (text-property-any
                     0 (length line) 'mevedel-view-cockpit-area area line)))))))

  :doc "status strip shows completion and the restored session model"
  (mevedel-view-test--with-buffers
    (let* ((goal (mevedel-goal--create
                  :status 'complete :turns-run 3))
           (session (mevedel-session--create :name "main" :goal goal)))
      (with-current-buffer data-buf
        (setq-local mevedel--session session
                    gptel-model 'gpt-5.6-sol))
      (with-current-buffer view-buf
        (let ((line (mevedel-view--status-strip)))
          (should (string-match-p "complete · 3 turns · gpt-5.6-sol" line))))))

  :doc "status strip routes click targets to cockpit surfaces"
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (let ((line (mevedel-view--status-strip))
            called)
        (cl-letf (((symbol-function 'mevedel-menu-open)
                   (lambda (area) (setq called area))))
          (dolist (area '(top mode model tools))
            (let* ((pos (text-property-any
                         0 (length line)
                         'mevedel-view-cockpit-area area line))
                   (map (and pos (get-text-property pos 'local-map line)))
                   (command (and map
                                 (lookup-key map [tab-line mouse-1]))))
              (should pos)
              (should command)
              (setq called nil)
              (funcall command nil)
              (should (eq called area))))))))

  :doc "status strip clicks do not call gptel transients directly"
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (let* ((line (mevedel-view--status-strip))
             (pos (text-property-any
                   0 (length line)
                   'mevedel-view-cockpit-area 'tools line))
             (map (get-text-property pos 'local-map line))
             (command (lookup-key map [tab-line mouse-1]))
             (gptel-called nil))
        (cl-letf (((symbol-function 'gptel-menu)
                   (lambda ()
                     (interactive)
                     (setq gptel-called t)))
                  ((symbol-function 'mevedel-menu-open) #'ignore))
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
          (mevedel-view--setup view-buf data-buf)
          (with-current-buffer data-buf
            (should (equal header-line-format "GPTEL HEADER")))
          (with-current-buffer view-buf
            (should (equal header-line-format
                           '(:eval (mevedel-view--sticky-prompt-line))))
            (should (equal tab-line-format
                           '(:eval (mevedel-view--status-strip))))))
      (when (buffer-live-p view-buf) (kill-buffer view-buf))
      (when (buffer-live-p data-buf) (kill-buffer data-buf)))))

(mevedel-deftest mevedel-view--status-fragments ()
  ,test
  (test)
  :doc "status zone contains tasks, executions, and agents but no sandbox row"
  (let ((session (mevedel-session--create :name "status")))
    (cl-letf (((symbol-function 'mevedel-execution-count-user)
             (lambda (seen-session)
               (should (eq session seen-session))
               2))
            ((symbol-function 'mevedel-view-agent-status-fragment)
             (lambda ()
               '(:namespace status :id agents :priority 0
                 :body "agents\n"))))
    (let* ((fragments
            (mevedel-view--status-fragments
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
                          (eq (plist-get fragment :id) 'executions))
                        fragments))
           (agents (seq-find
                    (lambda (fragment)
                     (eq (plist-get fragment :id) 'agents))
                     fragments)))
      (should-not sandbox)
      (should (> (plist-get tasks :priority)
                 (plist-get executions :priority)))
      (should (> (plist-get executions :priority)
                 (plist-get agents :priority)))
      (should (string-match-p "Executions: 2 live"
                              (plist-get executions :body)))
      ;; The row needs a blank line under it or it reads as a caption
      ;; for the agents separator that follows.  A zone trims `:body'
      ;; to one newline, so the blank has to travel as a suffix.
      (should (equal "\n" (plist-get executions :body-suffix)))))))

(mevedel-deftest mevedel-view--execution-state-changed ()
  ,test
  (test)
  :doc "live-count redraw preserves a multiline leading-> composer draft"
  (mevedel-view-test--with-buffers
    (let ((session (mevedel-session--create :name "execution-status"))
          (draft "> quoted\nsecond line"))
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (with-current-buffer view-buf
        (goto-char (mevedel-view--input-start))
        (insert draft)
        (goto-char (+ (mevedel-view--input-start) 4)))
      (cl-letf (((symbol-function 'mevedel-execution-count-user)
                 (lambda (seen-session)
                   (should (eq session seen-session))
                   1))
                ((symbol-function 'mevedel-view-agent-status-fragment)
                 #'ignore))
        (mevedel-view--execution-state-changed session data-buf))
      (with-current-buffer view-buf
        (should (string= draft (mevedel-view--input-text)))
        (should (= (point) (+ (mevedel-view--input-start) 4)))
        (should (string-match-p
                 "Executions: 1 live"
                 (buffer-substring-no-properties
                  (point-min) (mevedel-view--input-start)))))))
  :doc "routes an agent execution update to its parent session view"
  (mevedel-view-test--with-buffers
    (let ((session (mevedel-session--create :name "agent-execution-status"))
          (agent-data (generate-new-buffer " *mevedel-agent-execution*")))
      (unwind-protect
          (progn
            (with-current-buffer data-buf
              (setq-local mevedel--session session))
            (with-current-buffer agent-data
              (setq-local
               mevedel--agent-invocation
               (mevedel-agent-invocation--create
                :parent-data-buffer data-buf)))
            (cl-letf (((symbol-function 'mevedel-execution-count-user)
                       (lambda (seen-session)
                         (should (eq session seen-session))
                         1))
                      ((symbol-function 'mevedel-view-agent-status-fragment)
                       #'ignore))
              (mevedel-view--execution-state-changed session agent-data))
            (with-current-buffer view-buf
              (should (string-match-p
                       "Executions: 1 live"
                       (buffer-substring-no-properties
                        (point-min) (mevedel-view--input-start))))))
        (kill-buffer agent-data)))))

(mevedel-deftest mevedel-view-open-executions ()
  ,test
  (test)
  :doc "opens the live execution cockpit"
  (let (opened)
    (cl-letf (((symbol-function 'mevedel-executions-list-open)
               (lambda (&optional _context) (setq opened t))))
      (mevedel-view-open-executions))
    (should opened)))

;;
;;; View lifecycle

(mevedel-deftest mevedel-view--on-view-killed
  (:doc "view kill hook cleans up queued interactions")
  ,test
  (test)

  :doc "killing the view aborts both queues and kills the data buffer"
  (let ((data-buf (generate-new-buffer " *test-data-kill-view*"))
        (view-buf (generate-new-buffer " *test-view-kill-view*"))
        (session (mevedel-session-create
                  "main"
                  (mevedel-workspace--create
                   :type 'project :id "/tmp/kill-view/"
                   :root "/tmp/kill-view/" :name "kill-view")))
        (outcomes nil))
    (unwind-protect
        (progn
          (with-current-buffer data-buf
            (org-mode)
            (setq-local mevedel--session session))
          (mevedel-view--setup view-buf data-buf)
          (setf (mevedel-session-permission-queue session)
                (list (list :kind 'generic
                            :tool-name "Read"
                            :session session
                            :callback
                            (lambda (outcome)
                              (push (cons 'permission outcome) outcomes)))))
          (setf (mevedel-session-pending-plan-approval session)
                (list :body "# Plan"
                            :chat-buffer data-buf
                            :session session
                            :callback
                            (lambda (outcome)
                              (push (cons 'plan outcome) outcomes))))
          (kill-buffer view-buf)
          (should-not (buffer-live-p view-buf))
          (should-not (buffer-live-p data-buf))
          (should (null (mevedel-session-permission-queue session)))
          (should (null (mevedel-session-pending-plan-approval session)))
          (should (equal '((plan . aborted) (permission . aborted))
                         outcomes)))
      (when (buffer-live-p view-buf) (kill-buffer view-buf))
      (when (buffer-live-p data-buf) (kill-buffer data-buf))))

  :doc "stops transfer polling before sealing without losing root registration"
  (mevedel-view-test--with-buffers
    (let ((session (mevedel-session--create :name "kill-transfer"))
          timer sealed)
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (with-current-buffer view-buf
        (mevedel-view-control-transfer-initialize #'ignore #'ignore)
        (setq timer mevedel-view--control-transfer-timer))
      (cl-letf (((symbol-function 'mevedel-journal-capture-seal-and-schedule)
                 (lambda (owner data trigger &optional _captures)
                   (should (eq session owner))
                   (should (eq data-buf data))
                   (should (eq 'session-end trigger))
                   (should (eq data-buf
                               (mevedel-session-control-transfer-root-buffer
                                session)))
                   (should-not (memq timer timer-list))
                   (with-current-buffer view-buf
                     (mevedel-view--control-transfer-schedule view-buf)
                     (should-not mevedel-view--control-transfer-timer))
                   (setq sealed t))))
        (kill-buffer view-buf))
      (should sealed)
      (should-not (buffer-live-p data-buf))
      (should-not (mevedel-session-control-transfer-root-buffer session))))

  :doc "killing an agent view detaches its observer from retained data"
  (let ((data-buf (generate-new-buffer " *test-agent-data-kill-view*"))
        (view-buf (generate-new-buffer " *test-agent-view-kill-view*"))
        (parent-view (generate-new-buffer " *test-parent-kill-view*"))
        (change-hook #'ignore))
    (unwind-protect
        (progn
          (with-current-buffer data-buf
            (org-mode)
            (setq-local mevedel--view-buffer parent-view))
          (mevedel-view--setup
           view-buf data-buf
           (list :agent-transcript-p t
                 :preserve-data-view-buffer t
                 :parent-view parent-view
                 :transcript-info '(:live-buffer t)))
          (with-current-buffer view-buf
            (setq-local mevedel-view--live-source-change-hook change-hook)
            (setq-local mevedel-view--live-data-tail-start
                        (with-current-buffer data-buf
                          (copy-marker (point-min))))
            (setq-local mevedel-view--live-view-tail-start
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

(mevedel-deftest mevedel-view--on-data-killed
  (:doc "data kill hook cleans up queued interactions")
  ,test
  (test)

  :doc "data teardown stops all executions in the session"
  (let ((session (mevedel-session--create :name "kill"))
        (agent-buffer (generate-new-buffer " *test-data-kill-agent*"))
        stopped)
    (setf (mevedel-session-agent-registry session)
          (list
           (cons "/root/worker"
                 (mevedel-agent-record--create
                  :path "/root/worker"
                  :conversation-buffer agent-buffer))))
    (unwind-protect
        (progn
          (with-temp-buffer
            (setq-local mevedel--session session)
            (cl-letf
                (((symbol-function 'mevedel-view--abort-data-buffer) #'ignore)
                 ((symbol-function 'mevedel-execution-teardown-session)
                  (lambda (owner-session)
                    (setq stopped owner-session))))
              (mevedel-view--on-data-killed)))
          (should (eq session stopped))
          (should-not (buffer-live-p agent-buffer)))
      (when (buffer-live-p agent-buffer)
        (kill-buffer agent-buffer))))

  :doc "killing the data buffer aborts queued and direct interactions"
  (let ((data-buf (generate-new-buffer " *test-data-kill-data*"))
        (view-buf (generate-new-buffer " *test-view-kill-data*"))
        (session (mevedel-session-create
                  "main"
                  (mevedel-workspace--create
                   :type 'project :id "/tmp/kill-data/"
                   :root "/tmp/kill-data/" :name "kill-data")))
        (outcomes nil))
    (unwind-protect
        (progn
          (with-current-buffer data-buf
            (org-mode)
            (setq-local mevedel--session session))
          (mevedel-view--setup view-buf data-buf)
          (require 'mevedel-interaction-prompt)
          (with-current-buffer data-buf
            (mevedel--prompt-user-with-overlay
             "Confirm" "Direct request" "Proceed?" nil
             (lambda (outcome)
               (push (cons 'direct outcome) outcomes))))
          (setf (mevedel-session-permission-queue session)
                (list (list :kind 'generic
                            :tool-name "Read"
                            :session session
                            :callback
                            (lambda (outcome)
                              (push (cons 'permission outcome) outcomes)))))
          (setf (mevedel-session-pending-plan-approval session)
                (list :body "# Plan"
                            :chat-buffer data-buf
                            :session session
                            :callback
                            (lambda (outcome)
                              (push (cons 'plan outcome) outcomes))))
          (kill-buffer data-buf)
          (should-not (buffer-live-p data-buf))
          (should-not (buffer-live-p view-buf))
          (should (null (mevedel-session-permission-queue session)))
          (should (null (mevedel-session-pending-plan-approval session)))
          (should (equal '((direct . aborted)
                           (plan . aborted)
                           (permission . aborted))
                         outcomes)))
      (when (buffer-live-p view-buf) (kill-buffer view-buf))
      (when (buffer-live-p data-buf) (kill-buffer data-buf)))))

(mevedel-deftest mevedel-view--allow-session-close-p (:quiet t)
  ,test
  (test)
  :doc "pending publication blocks closing the data buffer"
  (let ((data-buf (generate-new-buffer " *test-pending-data*"))
        (view-buf (generate-new-buffer " *test-pending-data-view*"))
        (session (mevedel-session-create
                  "main"
                  (mevedel-workspace--create
                   :type 'project :id "/tmp/pending-data/"
                   :root "/tmp/pending-data/" :name "pending-data"))))
    (unwind-protect
        (progn
          (with-current-buffer data-buf
            (org-mode)
            (setq-local mevedel--session session))
          (mevedel-view--setup view-buf data-buf)
          (setf (mevedel-session-pending-publication session)
                '(:reason "target unavailable"))
          (should-not (kill-buffer data-buf))
          (should (buffer-live-p data-buf))
          (should (buffer-live-p view-buf))
          (setf (mevedel-session-pending-publication session) nil)
          (should (kill-buffer data-buf))
          (should-not (buffer-live-p view-buf)))
      (setf (mevedel-session-pending-publication session) nil)
      (when (buffer-live-p view-buf) (kill-buffer view-buf))
      (when (buffer-live-p data-buf) (kill-buffer data-buf))))

  :doc "pending publication blocks closing the paired view"
  (let ((data-buf (generate-new-buffer " *test-pending-view-data*"))
        (view-buf (generate-new-buffer " *test-pending-view*"))
        (session (mevedel-session-create
                  "main"
                  (mevedel-workspace--create
                   :type 'project :id "/tmp/pending-view/"
                   :root "/tmp/pending-view/" :name "pending-view"))))
    (unwind-protect
        (progn
          (with-current-buffer data-buf
            (org-mode)
            (setq-local mevedel--session session))
          (mevedel-view--setup view-buf data-buf)
          (setf (mevedel-session-pending-publication session)
                '(:reason "target unavailable"))
          (should-not (kill-buffer view-buf))
          (should (buffer-live-p view-buf))
          (should (buffer-live-p data-buf))
          (setf (mevedel-session-pending-publication session) nil)
          (should (kill-buffer view-buf))
          (should-not (buffer-live-p data-buf)))
      (setf (mevedel-session-pending-publication session) nil)
      (when (buffer-live-p view-buf) (kill-buffer view-buf))
      (when (buffer-live-p data-buf) (kill-buffer data-buf))))

  :doc "an unsettled remote mutation blocks close until acknowledgement"
  (let ((data-buf (generate-new-buffer " *test-unsettled-data*"))
        (view-buf (generate-new-buffer " *test-unsettled-view*"))
        (session (mevedel-session-create
                  "main"
                  (mevedel-workspace--create
                   :type 'project :id "/tmp/unsettled/"
                   :root "/tmp/unsettled/"
                   :name "unsettled"))))
    (unwind-protect
        (progn
          (with-current-buffer data-buf
            (org-mode)
            (setq-local mevedel--session session))
          (mevedel-view--setup view-buf data-buf)
          (setf (mevedel-session-lease session)
                '(:state owned :unsettled-mutation t))
          (cl-letf (((symbol-function
                      'mevedel-execution-unsettled-mutation-p)
                     (lambda (_session) t)))
            (should-not (kill-buffer data-buf)))
          (should (buffer-live-p data-buf))
          (should (buffer-live-p view-buf))
          (setf (mevedel-session-lease session)
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
          (mevedel-session-create
           "main"
           (mevedel-workspace--create
            :type 'project :id root :root root :name "foreign-close"))))
    (unwind-protect
        (progn
          (with-current-buffer data-buf
            (org-mode)
            (setq-local mevedel--session session))
          (mevedel-view--setup view-buf data-buf)
          (setf (mevedel-session-lease session)
                '(:state foreign :unsettled-mutation t))
          (with-current-buffer data-buf
            (setq-local mevedel-session--read-only-mode t)
            (setq buffer-read-only t))
          (cl-letf (((symbol-function
                      'mevedel-execution-unsettled-mutation-p)
                     (lambda (_session) t)))
            (should (kill-buffer data-buf)))
          (should-not (buffer-live-p view-buf))
          (should
           (plist-get (mevedel-session-lease session)
                      :unsettled-mutation)))
      (when (buffer-live-p view-buf) (kill-buffer view-buf))
      (when (buffer-live-p data-buf)
        (with-current-buffer data-buf
          (setq buffer-read-only nil))
        (kill-buffer data-buf)))))

;;
;;; View command wiring

(mevedel-deftest mevedel-view-mode-map ()
  ,test
  (test)
  :doc "view mode binds the cockpit command"
  (should (eq (lookup-key mevedel-view-mode-map (kbd "C-c C-o"))
              #'mevedel-menu))
  (should-not (eq (lookup-key mevedel-view-mode-map (kbd "C-c C-m"))
                  #'mevedel-menu)))



(provide 'test-mevedel-view)

;;; test-mevedel-view.el ends here
