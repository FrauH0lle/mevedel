;;; test-niwa-view-render-reentry.el --- Reentrant projection tests -*- lexical-binding: t -*-

;;; Commentary:
;; Exercise projection entry points while fontification delivers another event.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'niwa-view)
(require 'niwa-view-render)
(require 'niwa-view-stream)

(niwa-deftest niwa-view-render--mouse-gesture ()
  ,test
  (test)
  :doc "macro-based tracking holds for any non-nil tracking value"
  (with-temp-buffer
    (dolist (tracking '(t dragging dropping))
      (let ((track-mouse tracking)
            (last-command-event (list 'down-mouse-1)))
        (should (eq last-command-event (niwa-view-render--mouse-gesture)))
        (should (niwa-view-render--mouse-held-p)))))
  :doc "macro expiry blocks only that gesture even with a stale terminal start"
  (niwa-view-test--with-buffers
    (with-current-buffer view-buf
      (let* ((track-mouse t)
             (first (list 'down-mouse-1))
             (second (list 'down-mouse-1))
             (last-command-event first)
             (terminal-start (list 'down-mouse-1))
             events)
        (cl-letf (((symbol-function 'terminal-parameter)
                   (lambda (&rest _) terminal-start)))
          (niwa-view-render-mutate 'live (lambda () (push 'first events)))
          (niwa-view-render--resume-after-mouse view-buf t)
          (should (equal '(first) events))
          (should (eq first niwa-view-render--expired-mouse-gesture))
          (should-not (niwa-view-render--mouse-held-p))
          (niwa-view-render-mutate 'live (lambda () (push 'continued events)))
          (should (equal '(continued first) events))
          (should-not niwa-view-render--mouse-resume-hook)
          (setq last-command-event second)
          (should (niwa-view-render--mouse-held-p))
          (niwa-view-render-mutate 'live (lambda () (push 'second events)))
          (should (equal '(continued first) events))
          (let ((track-mouse nil))
            (funcall niwa-view-render--mouse-resume-hook))
          (should (equal '(second continued first) events))))))
  :doc "built-in identity survives movement commands and changes for a new drag"
  (with-temp-buffer
    (let* ((track-mouse 'drag-tracking)
           (first (list 'down-mouse-1))
           (terminal-start first)
           (last-command-event (list 'mouse-movement)))
      (cl-letf (((symbol-function 'terminal-parameter)
                 (lambda (&rest _) terminal-start)))
        (should (eq first (niwa-view-render--mouse-gesture)))
        (setq niwa-view-render--expired-mouse-gesture first)
        (should-not (niwa-view-render--mouse-held-p))
        (setq last-command-event (list 'mouse-movement))
        (should-not (niwa-view-render--mouse-held-p))
        (setq terminal-start (list 'down-mouse-1))
        (should (niwa-view-render--mouse-held-p))))))

(niwa-deftest niwa-view-render--queued-mouse-release-p ()
  (let ((unread-command-events ,events))
    (should (eq (not (null (niwa-view-render--queued-mouse-release-p))) ,expected)))
  (events expected)
  :doc "ordinary input does not hold a released gesture"
  '(97 (down-mouse-1 (nil 5)) (mouse-movement (nil 5))) nil
  :doc "queued click retains the gesture"
  '((mouse-1 (nil 5))) t
  :doc "queued drag release retains the gesture"
  '((drag-mouse-1 (nil 5) (nil 8))) t
  :doc "queued double click retains the gesture"
  '((double-mouse-1 (nil 5))) t
  :doc "recorded event wrapper retains the gesture"
  '((t mouse-1 (nil 5))) t
  :doc "unrecorded event wrapper retains the gesture"
  '((no-record mouse-1 (nil 5))) t)

(niwa-deftest niwa-view-render--wait-for-mouse ()
  ,test
  (test)
  :doc "a macro-requeued release keeps writes held until its command finishes"
  (niwa-view-test--with-buffers
    (with-current-buffer view-buf
      (let ((last-command-event (list 'down-mouse-1))
            events hook)
        (let ((track-mouse t))
          (niwa-view-render-mutate 'live (lambda () (push 'release events)))
          (setq hook niwa-view-render--mouse-resume-hook))
        (let ((track-mouse nil)
              (unread-command-events '((mouse-1 (nil 5)))))
          (funcall hook)
          (should-not events)
          (should (eq hook niwa-view-render--mouse-resume-hook))
          ;; A later command hook may request another projection before the
          ;; queued release runs.  It must join the same hold, not drain it.
          (niwa-view-render-mutate 'status (lambda () (push 'status events)))
          (should-not events))
        (let ((track-mouse nil) (unread-command-events nil)) (funcall hook))
        (should (equal '(status release) events))
        (should-not niwa-view-render--mouse-resume-hook))))
  :doc "the deadline still drains a queued release that never runs"
  (niwa-view-test--with-buffers
    (with-current-buffer view-buf
      (let ((last-command-event (list 'down-mouse-1)) events)
        (let ((track-mouse t))
          (niwa-view-render-mutate 'live (lambda () (push 'deadline events))))
        (let ((track-mouse nil)
              (unread-command-events '((mouse-1 (nil 5)))))
          (niwa-view-render--resume-after-mouse view-buf t)
          (should (equal '(deadline) events)))
        (should-not niwa-view-render--mouse-resume-hook)
        (should-not niwa-view-render--mouse-resume-timer))))
  :doc "nested command hooks cannot drain a macro gesture before tracking ends"
  (niwa-view-test--with-buffers
    (with-current-buffer view-buf
      (let ((track-mouse t)
            (last-command-event (list 'down-mouse-1))
            events hook)
        (niwa-view-render-mutate 'live (lambda () (push 'release events)))
        (setq hook niwa-view-render--mouse-resume-hook)
        (should hook)
        (run-hooks 'post-command-hook)
        (should-not events)
        (should (eq hook niwa-view-render--mouse-resume-hook))
        (let ((track-mouse nil)) (funcall hook))
        (should (equal '(release) events))
        (should-not niwa-view-render--mouse-resume-hook))))
  :doc "mouse-held writes coalesce and release in another buffer drains them once"
  (niwa-view-test--with-buffers
    (let (events hook)
      (with-current-buffer view-buf
        (let ((track-mouse 'drag-tracking))
          (niwa-view-render-mutate 'live (lambda () (push 'obsolete events)))
          (setq hook niwa-view-render--mouse-resume-hook)
          (niwa-view-render-mutate 'live (lambda () (push 'latest events)))
          (should (eq hook niwa-view-render--mouse-resume-hook))
          (should (memq hook (default-value 'post-command-hook)))
          (funcall hook)
          (should-not events)))
      (with-current-buffer data-buf
        (let ((track-mouse nil)) (funcall hook)))
      (should (equal '(latest) events))
      (should-not (memq hook (default-value 'post-command-hook)))
      (with-current-buffer view-buf
        (should-not niwa-view-render--mouse-resume-hook)
        (should-not niwa-view-render--pending)
        (should-not niwa-view-render--owner))))
  :doc "view teardown removes the temporary global mouse-release hook"
  (niwa-view-test--with-buffers
    (let (hook)
      (with-current-buffer view-buf
        (let ((track-mouse 'drag-tracking))
          (niwa-view-render-mutate 'live (lambda () (ert-fail "Killed view wrote")))
          (setq hook niwa-view-render--mouse-resume-hook)
          (should (memq hook (default-value 'post-command-hook)))))
      (kill-buffer view-buf)
      (should-not (memq hook (default-value 'post-command-hook)))))
  :doc "lost release drains by deadline and the same stale gesture cannot rearm"
  (niwa-view-test--with-buffers
    (with-current-buffer view-buf
      (let ((track-mouse 'drag-tracking)
            events hook)
        (niwa-view-render-mutate 'live (lambda () (push 'deadline events)))
        (setq hook niwa-view-render--mouse-resume-hook)
        (should (timerp niwa-view-render--mouse-resume-timer))
        (let ((timer niwa-view-render--mouse-resume-timer)
              (deadline (+ (float-time) 3)))
          (while (and (not events) (< (float-time) deadline))
            (accept-process-output nil 0.02))
          (ert-info ((format "deadline=%S now=%S active=%S expired=%S hook=%S"
                             (timer--time timer) (current-time)
                             (memq timer timer-list)
                             niwa-view-render--expired-mouse-gesture
                             (not (null niwa-view-render--mouse-resume-hook))))
            (should (equal '(deadline) events))))
        (should-not (niwa-view-render--mouse-held-p))
        (should-not niwa-view-render--mouse-resume-timer)
        (should-not (memq hook (default-value 'post-command-hook)))
        (niwa-view-render-mutate 'live (lambda () (push 'continued events)))
        (should (equal '(continued deadline) events))
        (should-not niwa-view-render--mouse-resume-hook))))
  :doc "a deferred writer error still removes both wakeups and writer ownership"
  (niwa-view-test--with-buffers
    (with-current-buffer view-buf
      (let (hook)
        (let ((track-mouse 'drag-tracking))
          (niwa-view-render-mutate 'live (lambda () (error "Deferred failure")))
          (setq hook niwa-view-render--mouse-resume-hook))
        (let ((track-mouse nil))
          (should-error (funcall hook) :type 'error))
        (should-not (memq hook (default-value 'post-command-hook)))
        (should-not niwa-view-render--mouse-resume-timer)
        (should-not niwa-view-render--mouse-resume-hook)
        (should-not niwa-view-render--pending)
        (should-not niwa-view-render--owner))))
  :doc "two held views own independent hooks and drain their own writers"
  (niwa-view-test--with-buffers
    (let ((first view-buf)
          first-hook second-hook events)
      (niwa-view-test--with-buffers
        (let ((track-mouse 'drag-tracking))
          (with-current-buffer first
            (niwa-view-render-mutate 'live (lambda () (push 'first events)))
            (setq first-hook niwa-view-render--mouse-resume-hook))
          (with-current-buffer view-buf
            (niwa-view-render-mutate 'live (lambda () (push 'second events)))
            (setq second-hook niwa-view-render--mouse-resume-hook)))
        (let ((track-mouse nil))
          (funcall first-hook)
          (should (equal '(first) events))
          (should-not (memq first-hook (default-value 'post-command-hook)))
          (should (memq second-hook (default-value 'post-command-hook)))
          (funcall second-hook))
        (should (equal '(second first) events))
        (should-not (memq second-hook (default-value 'post-command-hook)))))))

(niwa-deftest niwa-view-render-live-update/reentry ()
  ,test
  (test)

  :doc "terminal settlement during fontification leaves one response and no live tail"
  (dolist (terminal '(direct observer stop
                     (full . direct) (full . observer) (full . stop)))
    (niwa-view-test--with-buffers
      (niwa-view-test--insert-data
       data-buf
       "Conclusion.\n\nThe appropriate fix:\n\n```html\n<meta charset=\"utf-8\">\n```\n\nNo files changed.\n"
       'response)
      (with-current-buffer view-buf
        (setq niwa-view--data-turn-start
              (with-current-buffer data-buf (copy-marker (point-min))))
        (setq niwa-view--in-flight-turn-start
              (copy-marker niwa-view--status-marker nil))
        (niwa-view-test--insert-composer-draft "> draft\nsecond line" 4)
        (let ((original (symbol-function 'niwa-view--fontify-response))
              entered)
          (cl-letf (((symbol-function 'niwa-view--fontify-response)
                     (lambda (&rest args)
                       (prog1 (apply original args)
                         (unless entered
                           (setq entered t)
                           (pcase (if (consp terminal) (cdr terminal) terminal)
                             ('direct
                              (niwa-view-render-settle
                               data-buf 1 (with-current-buffer data-buf (point-max))))
                             ('observer
                              (with-current-buffer data-buf
                                (niwa-view-stream-render-response 1 (point-max))))
                             ('stop (niwa-view-stream-stop))))))))
            (if (consp terminal)
                (niwa-view--full-rerender)
              (niwa-view-render-live-update data-buf)))
          (should entered))
        (should-not niwa-view--live-data-tail-start)
        (should (= 1 (niwa-view-test--count-substring
                      "<meta charset=" (buffer-string))))
        (should (equal "> draft\nsecond line" (niwa-view-composer-draft-text nil t)))
        (should (= (+ (niwa-view-composer-draft-start) 4)
                   (with-current-buffer (niwa-view-composer-buffer) (point))))))))

(niwa-deftest niwa-view-render/chrome-reentry ()
  (niwa-view-test--with-buffers
    (let ((session (niwa-session--create :name "chrome-reentry")))
      (with-current-buffer data-buf
        (setq-local niwa--session session))
      (niwa-view-test--insert-data data-buf "Unique response body\n" 'response)
      (with-current-buffer view-buf
        (setq-local niwa--session session)
        (setq niwa-view--data-turn-start
              (with-current-buffer data-buf (copy-marker (point-min))))
        (setq niwa-view--in-flight-turn-start
              (copy-marker niwa-view--status-marker nil))
        (niwa-view-test--insert-composer-draft ,draft (min 4 (length ,draft)))
        (let ((original (symbol-function 'niwa-view--fontify-response))
              (composer (niwa-view-composer-buffer))
              (start (niwa-view-composer-draft-start))
              prompt
              entered)
          (should-not (eq composer view-buf))
          (with-current-buffer composer
            (setq prompt (buffer-substring-no-properties (point-min) start))
            (set-mark start)
            (setq mark-active t))
          (cl-letf (((symbol-function 'niwa-view--fontify-response)
                     (lambda (&rest args)
                       (prog1 (apply original args)
                         (unless entered
                           (setq entered t)
                           (should niwa-view-render--owner)
                           (let ((before (buffer-string)))
                             (pcase ',zone
                               ('status
                                (setf (niwa-session-recovery-issues session)
                                      '((:message "Nonempty reentrant status" :blocking t)))
                                (niwa-view--render-status data-buf))
                               ('interaction
                                (let ((overlay
                                       (niwa-view--interaction-register
                                        '(:id chrome-reentry :kind request
                                          :body "Nonempty reentrant interaction"))))
                                  (should (eq overlay
                                              (gethash 'chrome-reentry
                                                       niwa-view--interaction-overlays)))
                                  (should (niwa-view-interaction-blocking-p))))
                               ('progress
                                (niwa-view--update-spinner
                                 "Nonempty reentrant progress" 'request)))
                             (should (equal-including-properties before (buffer-string)))
                             (if (eq ',zone 'progress)
                                 (should (string-search "Nonempty reentrant progress"
                                                        (niwa-view-test--rail-text)))
                               (should-not (niwa-view-zone-region ',zone))
                               (should (assoc ',zone niwa-view-render--pending)))))))))
            (pcase ',render
              ('full (niwa-view--full-rerender))
              ('incremental (niwa-view-render-live-update data-buf))))
          (should entered)
          (if (eq ',zone 'progress)
              (should (string-search "Nonempty reentrant progress"
                                     (niwa-view-test--rail-text)))
            (should (niwa-view-zone-region ',zone)))
          (should (= 1 (niwa-view-test--count-substring
                        "Unique response body" (buffer-string))))
          (let ((end (niwa-view-composer-draft-end))
                (tail (niwa-view-transcript-tail)))
            (should (eq composer (niwa-view-composer-buffer)))
            (should (= start (niwa-view-composer-draft-start)))
            (should (equal ,draft (niwa-view-composer-draft-text nil t)))
            (with-current-buffer composer
              (should (= (point) (+ start (min 4 (length ,draft)))))
              (should (= (mark) start))
              (should mark-active)
              (let ((position start))
                (while (< position end)
                  (should-not (get-char-property position 'read-only))
                  (should-not (invisible-p position))
                  (setq position (1+ position))))
              (should (equal prompt (buffer-substring-no-properties (point-min) start)))
              (should (= start (overlay-start niwa-view--composer-display-overlay)))
              (should (= end (overlay-end niwa-view--composer-display-overlay)))
              (let ((position (+ (point-min) 4)))
                (while (< position start)
                  (should (get-text-property position 'niwa-view-prompt))
                  (should-not (invisible-p position))
                  (should-not (get-char-property position 'display))
                  (should-not (get-char-property position 'line-prefix))
                  (should-not (get-char-property position 'wrap-prefix))
                  (setq position (1+ position)))))
            (let ((position (point-min)))
              (while (< position tail)
                (should-not (get-char-property position 'line-prefix))
                (should-not (get-char-property position 'wrap-prefix))
                (setq position (1+ position))))
            (dolist (name '(status interaction))
              (when-let* ((region (niwa-view-zone-region name)))
                (should (<= niwa-view--status-marker
                            (overlay-start region) (overlay-end region) tail))))
            (save-excursion
              (goto-char (point-min))
              (search-forward "Unique response body")
              (should (<= (point) niwa-view--status-marker))
              (should (eq 'response (get-text-property (1- (point)) 'niwa-view-type)))))))))
  (render zone draft)
  :doc "full render keeps an empty composer after a nonempty status redraw"
  full status ""
  :doc "full render keeps a multiline composer after a nonempty status redraw"
  full status "> draft\nsecond line\n"
  :doc "incremental render keeps an empty composer after a nonempty status redraw"
  incremental status ""
  :doc "incremental render keeps a multiline composer after a nonempty status redraw"
  incremental status "> draft\nsecond line\n"
  :doc "full render keeps an empty composer after a nonempty interaction redraw"
  full interaction ""
  :doc "full render keeps a multiline composer after a nonempty interaction redraw"
  full interaction "> draft\nsecond line\n"
  :doc "incremental render keeps an empty composer after a nonempty interaction redraw"
  incremental interaction ""
  :doc "incremental render keeps a multiline composer after a nonempty interaction redraw"
  incremental interaction "> draft\nsecond line\n"
  :doc "full render keeps an empty composer after a nonempty progress redraw"
  full progress ""
  :doc "full render keeps a multiline composer after a nonempty progress redraw"
  full progress "> draft\nsecond line\n"
  :doc "incremental render keeps an empty composer after a nonempty progress redraw"
  incremental progress ""
  :doc "incremental render keeps a multiline composer after a nonempty progress redraw"
  incremental progress "> draft\nsecond line\n")

(niwa-deftest niwa-view--render-status-now ()
  ,test
  (test)
  :doc "owned status materialization renders live state and preserves the draft"
  (niwa-view-test--with-buffers
    (with-current-buffer view-buf
      (setq-local niwa--session
                  (niwa-session--create
                   :name "status-owned"
                   :recovery-issues '((:message "Owned status" :blocking t))))
      (niwa-view-test--insert-composer-draft "> draft\nsecond line" 4)
      (niwa-view--render-status-now)
      (should (niwa-view-zone-fragment-bounds 'status 'recovery))
      (should (string-search "Owned status" (buffer-string)))
      (should (equal "> draft\nsecond line" (niwa-view-composer-draft-text nil t)))
      (should (= (+ (niwa-view-composer-draft-start) 4)
                 (with-current-buffer (niwa-view-composer-buffer) (point)))))))

(niwa-deftest niwa-view--interaction-render-now ()
  ,test
  (test)
  :doc "owned interaction materialization retains the immediate callback handle"
  (niwa-view-test--with-buffers
    (with-current-buffer view-buf
      (niwa-view-test--insert-composer-draft "> draft\nsecond line" 4)
      (let* ((callback #'ignore)
             (niwa-view--interaction-render-suppressed t)
             (overlay
              (niwa-view--interaction-register
               (list :id 'owned :kind 'request :body "Owned interaction"
                     :activate callback))))
        (let ((niwa-view--interaction-render-suppressed nil))
          (niwa-view--interaction-render-now))
        (should (niwa-view-zone-fragment-bounds 'interaction 'owned))
        (should (eq overlay (gethash 'owned niwa-view--interaction-overlays)))
        (should (eq callback (overlay-get overlay 'niwa-view-interaction-activate)))
        (should (equal "> draft\nsecond line" (niwa-view-composer-draft-text nil t)))
        (should (= (+ (niwa-view-composer-draft-start) 4)
                   (with-current-buffer (niwa-view-composer-buffer) (point))))))))

(niwa-deftest niwa-view-render/chrome-coalescing ()
  ,test
  (test)
  :doc "repeated status and interaction requests materialize once with latest state"
  (niwa-view-test--with-buffers
    (with-current-buffer view-buf
      (setq-local niwa--session (niwa-session--create :name "chrome-coalescing"))
      (let ((status-now (symbol-function 'niwa-view--render-status-now))
            (interaction-now (symbol-function 'niwa-view--interaction-render-now))
            (status-count 0)
            (interaction-count 0)
            (after-count 0)
            overlay)
        (cl-letf (((symbol-function 'niwa-view--render-status-now)
                   (lambda (&rest args)
                     (cl-incf status-count)
                     (apply status-now args)))
                  ((symbol-function 'niwa-view--interaction-render-now)
                   (lambda ()
                     (cl-incf interaction-count)
                     (funcall interaction-now))))
          (niwa-view-render-mutate
           'outer
           (lambda ()
             (let ((before (buffer-string)))
               (dolist (label '("Old chrome" "Latest chrome"))
                 (setf (niwa-session-recovery-issues niwa--session)
                       (list (list :message label :blocking t)))
                 (niwa-view--render-status)
                 (let ((registered
                        (niwa-view--interaction-register
                         (list :id 'coalesced :kind 'request :body label
                               :after-render (lambda () (cl-incf after-count))))))
                   (when overlay (should (eq overlay registered)))
                   (setq overlay registered))
                 (should (niwa-view-interaction-blocking-p))
                 (should (equal-including-properties before (buffer-string))))
               (should (= 2 (length niwa-view-render--pending)))
               (should (= 0 status-count interaction-count after-count))))))
        (should (= 1 status-count interaction-count after-count))
        (should-not (string-search "Old chrome" (buffer-string)))
        (should (= 2 (niwa-view-test--count-substring "Latest chrome" (buffer-string))))
        (should (eq overlay (gethash 'coalesced niwa-view--interaction-overlays))))))

  :doc "isolated non-view interaction rendering does not acquire transcript ownership"
  (with-temp-buffer
    (insert "Isolated source\n")
    (setq-local niwa-view-render--owner t)
    (cl-letf (((symbol-function 'niwa-view-render-mutate)
               (lambda (&rest _) (error "Unexpected transcript ownership"))))
      (let ((overlay (niwa-view--interaction-register
                      '(:id isolated :kind request :body "Isolated interaction"))))
        (should (overlay-buffer overlay))
        (should (niwa-view-zone-fragment-bounds 'interaction 'isolated))
        (should (string-search "Isolated interaction" (buffer-string)))
        (should (string-prefix-p "Isolated source\n" (buffer-string)))))))

(niwa-deftest niwa-view-render-mutate/ownership ()
  ,test
  (test)

  :doc "nested mutations coalesce without running inside the active writer"
  (niwa-view-test--with-buffers
    (with-current-buffer view-buf
      (let (events)
        (niwa-view-render-mutate
         'outer
         (lambda ()
           (push 'start events)
           (niwa-view-render-mutate 'refresh (lambda () (push 'obsolete events)))
           (niwa-view-render-mutate 'refresh (lambda () (push 'latest events)))
           (push 'end events)))
        (should (equal (nreverse events) '(start end latest)))
        (should-not niwa-view-render--owner)
        (should-not niwa-view-render--pending))))

  :doc "replacement invalidates queued old work and errors release ownership"
  (niwa-view-test--with-buffers
    (with-current-buffer view-buf
      (let (events)
        (should-error
         (niwa-view-render-mutate
          'outer
          (lambda ()
            (niwa-view-render-mutate 'old (lambda () (push 'old events)))
            (niwa-view-render-mutate
             'replacement (lambda () (push 'replacement events)) t)
            (error "Injected render failure"))))
        (should (equal events '(replacement)))
        (should-not niwa-view-render--owner)
        (niwa-view-render-mutate 'next (lambda () (push 'next events)))
        (should (equal events '(next replacement)))))))

(niwa-deftest niwa-view-disclosure/reentry ()
  ,test
  (test)

  :doc "full rendering during disclosure expansion preserves exactly one expanded row"
  (niwa-view-test--with-buffers
    (niwa-tool-register
     (niwa-tool--create
      :name "ReentryTool" :category "niwa"
      :renderer (lambda (_name _args result _data)
                  (list :header "ReentryTool: details" :body result
                        :body-mode 'text-mode :vtype 'tool-summary
                        :initially-collapsed-p t))))
    (niwa-view-test--insert-data
     data-buf "(:name \"ReentryTool\" :args nil)\n\nUnique tool body\n"
     '(tool . "reentry-tool"))
    (with-current-buffer view-buf
      (niwa-view--full-rerender)
      (niwa-view-test--insert-composer-draft "> draft\nsecond line" 4)
      (let ((original (symbol-function 'niwa-view-render-insert-expanded-disclosure))
            entered)
        (cl-letf (((symbol-function 'niwa-view-render-insert-expanded-disclosure)
                   (lambda (&rest args)
                     (unless entered
                       (setq entered t)
                       (niwa-view--full-rerender))
                     (apply original args))))
          (save-excursion
            (goto-char (point-min))
            (search-forward "ReentryTool: details")
            (niwa-view-toggle-section)))
        (should entered))
      (should (= 1 (niwa-view-test--count-substring
                    "Unique tool body" (buffer-string))))
      (should (= 1 (niwa-view-test--count-substring
                    "ReentryTool: details" (buffer-string))))
      (should (equal "> draft\nsecond line" (niwa-view-composer-draft-text nil t)))
      (should (= (+ (niwa-view-composer-draft-start) 4)
                 (with-current-buffer (niwa-view-composer-buffer) (point))))))

  :doc "full rendering during disclosure collapse preserves the requested fold"
  (niwa-view-test--with-buffers
    (niwa-view-test--insert-data
     data-buf "<system-reminder>\nCollapsed reminder body\n</system-reminder>\n" nil)
    (niwa-view-test--insert-data data-buf "Adjacent response\n" 'response)
    (with-current-buffer view-buf
      (niwa-view--full-rerender)
      (goto-char (point-min))
      (search-forward "System reminder")
      (niwa-view-toggle-section)
      (niwa-view-test--insert-composer-draft "> draft\nsecond line" 4)
      (let ((original (symbol-function 'niwa-view-render-collapsed-disclosure)) entered)
        (cl-letf (((symbol-function 'niwa-view-render-collapsed-disclosure)
                   (lambda (&rest args)
                     (unless entered
                       (setq entered t)
                       (niwa-view--full-rerender))
                     (apply original args))))
          (save-excursion
            (goto-char (point-min))
            (search-forward "System reminder")
            (niwa-view-toggle-section)))
        (should entered))
      (should-not (string-search "Collapsed reminder body" (buffer-string)))
      (should (= 1 (niwa-view-test--count-substring
                    "System reminder" (buffer-string))))
      (should (= 1 (niwa-view-test--count-substring
                    "Adjacent response" (buffer-string))))
      (should (equal "> draft\nsecond line" (niwa-view-composer-draft-text nil t)))
      (should (= (+ (niwa-view-composer-draft-start) 4)
                 (with-current-buffer (niwa-view-composer-buffer) (point)))))))

(niwa-deftest niwa-view-stream/replacement ()
  ,test
  (test)

  :doc "queued old terminal cleanup cannot clear a replacement turn"
  (niwa-view-test--with-buffers
    (niwa-view-test--insert-data data-buf "Old response\n" 'response)
    (with-current-buffer view-buf
      (let ((start (with-current-buffer data-buf (copy-marker (point-max)))))
        (niwa-view-render-mutate
         'outer
         (lambda ()
           (niwa-view-stream-stop)
           (niwa-view-stream-begin-turn niwa-view--status-marker start)))
        (should (marker-position niwa-view--data-turn-start))
        (should (= start niwa-view--data-turn-start))
        (should-not niwa-view-render--terminal-p)))))

(niwa-deftest niwa-view-agent/refresh-reentry ()
  ,test
  (test)
  :doc "multiple agent handles are rediscovered after nested full rendering"
  (niwa-view-test--with-buffers
    (niwa-tool-register
     (niwa-tool--create
      :name "ReentryAgent" :category "niwa"
      :renderer (lambda (_name _args result _data)
                  (list :header "Agent: owned worker" :body result
                        :body-mode 'text-mode :vtype 'agent-handle
                        :agent-path "/root/worker" :agent-status 'running
                        :initially-collapsed-p t))))
    (dotimes (n 2)
      (niwa-view-test--insert-data
       data-buf "(:name \"ReentryAgent\" :args nil)\n\nAgent detail\n"
       (cons 'tool (format "reentry-agent-%d" n)))
      (niwa-view-test--insert-data data-buf "Between calls\n" 'response))
    (with-current-buffer view-buf
      (niwa-view--full-rerender)
      (niwa-view-test--insert-composer-draft "> draft\nsecond line" 4)
      (let ((original (symbol-function 'niwa-view--segment-rendering))
            entered)
        (cl-letf (((symbol-function 'niwa-view--segment-rendering)
                   (lambda (&rest args)
                     (prog1 (apply original args)
                       (unless entered
                         (setq entered t)
                         (with-current-buffer data-buf
                           (goto-char (point-min))
                           (insert "*** Added authoritative prompt\n"))
                         (niwa-view--full-rerender))))))
          (niwa-view--refresh-agent-rendering-now "/root/worker"))
        (should entered))
      (should (= 2 (length (car (niwa-view--agent-handle-refresh-points
                                "/root/worker")))))
      (should (= 2 (niwa-view-test--count-substring
                    "Agent: owned worker" (buffer-string))))
      (should (= 1 (niwa-view-test--count-substring
                    "Added authoritative prompt" (buffer-string))))
      (should (equal "> draft\nsecond line" (niwa-view-composer-draft-text nil t)))
      (should (= (+ (niwa-view-composer-draft-start) 4)
                 (with-current-buffer (niwa-view-composer-buffer) (point)))))))

(niwa-deftest niwa-view-render-mutate/terminal-cleanup ()
  ,test
  (test)
  :doc "a failing queued disclosure cannot discard later mandatory terminal cleanup"
  (niwa-view-test--with-buffers
    (niwa-view-test--insert-data data-buf "Final response\n" 'response)
    (with-current-buffer view-buf
      (niwa-view-stream-begin-turn
       niwa-view--status-marker
       (with-current-buffer data-buf (copy-marker (point-min))))
      (should-error
       (niwa-view-render-mutate
        'outer
        (lambda ()
          (niwa-view-render-mutate 'bad-disclosure
                                      (lambda () (error "Queued failure")))
          (niwa-view-stream-stop))))
      (should-not niwa-view--data-turn-start)
      (should-not niwa-view--in-flight-turn-start)
      (should-not niwa-view--spinner-timer)
      (should-not niwa-view-render--owner)))

  :doc "source replacement cannot discard a queued terminal's required release"
  (dolist (terminal '(stop observer))
    (niwa-view-test--with-buffers
      (niwa-view-test--insert-data data-buf "Final response\n" 'response)
      (with-current-buffer view-buf
        (niwa-view-stream-begin-turn
         niwa-view--status-marker
         (with-current-buffer data-buf (copy-marker (point-min))))
        (niwa-view--schedule-render 'incremental data-buf 60)
        (should (timerp niwa-view--render-timer))
        (niwa-view-render-mutate
         'outer
         (lambda ()
           (if (eq terminal 'stop)
               (niwa-view-stream-stop)
             (with-current-buffer data-buf
               (niwa-view-stream-render-response 1 (point-max))))
           (niwa-view--full-rerender data-buf t)))
        (should-not niwa-view--data-turn-start)
        (should-not niwa-view--in-flight-turn-start)
        (should-not niwa-view--spinner-timer)
        (should-not niwa-view--pending-render-kind)
        (should-not niwa-view--render-timer)
        (should (= 1 (niwa-view-test--count-substring
                      "Final response" (buffer-string))))))))

(niwa-deftest niwa-view-agent-live-transcript/queued-error ()
  ,test
  (test)
  :doc "deferred observer failures warn locally without failing the outer writer"
  (dolist (operation '(niwa-view-agent-live-transcript-start
                       niwa-view-agent-live-transcript-finalize))
    (niwa-view-test--with-buffers
      (niwa-view-test--insert-data data-buf "Last good agent projection\n" 'response)
      (let ((invocation (niwa-agent-invocation--create
                         :buffer data-buf :transcript-status 'completed))
            warnings completed before)
        (with-current-buffer view-buf
          (setq niwa-view--agent-transcript-p t)
          (niwa-view--full-rerender)
          (setq before (buffer-string))
          (cl-letf (((symbol-function 'niwa-view--fontify-response)
                     (lambda (&rest _) (error "Injected observer failure")))
                    ((symbol-function 'niwa--warn-once)
                     (lambda (&rest args) (push args warnings))))
            (niwa-view-render-mutate
             'outer
             (lambda ()
               (funcall operation invocation)
               (niwa-view-render-mutate
                'after-observer (lambda () (setq completed t))))))
          (should completed)
          (should (equal before (buffer-string)))
          (should (= 1 (length warnings)))
          (should (string-match-p "Injected observer failure"
                                  (car (last (car warnings)))))
          (should-not niwa-view-render--owner)
          (should-not niwa-view-render--pending)
          (should-not niwa-view--spinner-timer)
          (niwa-view-render-mutate 'retry (lambda () (setq completed 'retry)))
          (should (eq completed 'retry)))))))

(niwa-deftest niwa-view-render-settle/reentry ()
  ,test
  (test)
  :doc "live and full renders requested during settlement cannot revive its tail"
  (dolist (nested '(live full))
    (niwa-view-test--with-buffers
      (niwa-view-test--insert-data data-buf "Only settled response\n" 'response)
      (with-current-buffer view-buf
        (niwa-view-stream-begin-turn
         niwa-view--status-marker
         (with-current-buffer data-buf (copy-marker (point-min))))
        (niwa-view-test--insert-composer-draft "> draft\nsecond line" 4)
        (let ((original (symbol-function 'niwa-view--fontify-response)) entered)
          (cl-letf (((symbol-function 'niwa-view--fontify-response)
                     (lambda (&rest args)
                       (prog1 (apply original args)
                         (unless entered
                           (setq entered t)
                           (if (eq nested 'live)
                               (niwa-view-render-live-update data-buf)
                             (niwa-view--full-rerender)))))))
            (with-current-buffer data-buf
              (niwa-view-stream-render-response 1 (point-max))))
          (should entered))
        (should (= 1 (niwa-view-test--count-substring
                      "Only settled response" (buffer-string))))
        (should-not niwa-view--live-data-tail-start)
        (should-not niwa-view--data-turn-start)
        (should-not niwa-view--in-flight-turn-start)
        (should-not niwa-view--spinner-timer)
        (should (equal "> draft\nsecond line" (niwa-view-composer-draft-text nil t)))
        (should (= (+ (niwa-view-composer-draft-start) 4)
                   (with-current-buffer (niwa-view-composer-buffer) (point))))))))

(niwa-deftest niwa-view-agent-live-transcript-finalize/reentry ()
  ,test
  (test)
  :doc "agent settlement during live rendering retains the source and reader"
  (niwa-view-test--with-buffers
    (niwa-view-test--insert-data data-buf "Agent final answer\n" 'response)
    (let ((invocation (niwa-agent-invocation--create
                       :buffer data-buf :transcript-status 'completed :call-count 2)))
      (with-current-buffer view-buf
        (setq niwa-view--agent-transcript-p t)
        (niwa-view-stream-begin-turn
         niwa-view--status-marker
         (with-current-buffer data-buf (copy-marker (point-min))))
        (niwa-view--full-rerender)
        (goto-char (point-min))
        (search-forward "Agent final")
        (let ((reader (buffer-substring-no-properties
                       (line-beginning-position) (point)))
              (reader-point (point))
              (original (symbol-function 'niwa-view--fontify-response)) entered)
          (cl-letf (((symbol-function 'niwa-view--fontify-response)
                     (lambda (&rest args)
                       (prog1 (apply original args)
                         (unless entered
                           (setq entered t)
                           (niwa-view-agent-live-transcript-finalize invocation))))))
            (niwa-view-render-live-update data-buf))
          (should entered)
          (should (= reader-point (point)))
          (should (equal reader (buffer-substring-no-properties
                                 (line-beginning-position) (point)))))
        (should (eq niwa--data-buffer data-buf))
        (should (eq 'completed (plist-get niwa-view--agent-transcript-info :status)))
        (should-not (plist-get niwa-view--agent-transcript-info :live-buffer))
        (should (= 1 (niwa-view-test--count-substring
                      "Agent final answer" (buffer-string))))
        (should-not niwa-view--data-turn-start)
        (should-not niwa-view--in-flight-turn-start)
        (should-not niwa-view--live-data-tail-start)
        (should-not niwa-view-render--owner)))))

(niwa-deftest niwa-view-disclosure/desired-state ()
  ,test
  (test)
  :doc "queued expansion and collapse apply desired state, not two old toggles"
  (dolist (collapse '(nil t))
    (niwa-view-test--with-buffers
      (niwa-view-test--insert-data
       data-buf "<system-reminder>\nUnique reminder body\n</system-reminder>\n" nil)
      (with-current-buffer view-buf
        (niwa-view--full-rerender)
        (when collapse
          (goto-char (point-min))
          (search-forward "System reminder")
          (niwa-view-toggle-section))
        (niwa-view-test--insert-composer-draft "> draft\nsecond line" 4)
        (niwa-view-render-mutate
         'outer
         (lambda ()
           (dotimes (_ 2)
             (save-excursion
               (goto-char (point-min))
               (search-forward "System reminder")
               (niwa-view-toggle-section)))
           (niwa-view--full-rerender)))
        (save-excursion
          (goto-char (point-min))
          (search-forward "System reminder")
          (should (eq collapse (and (get-text-property
                                    (1- (point)) 'niwa-view-collapsed) t))))
        (should (= (if collapse 0 1)
                   (niwa-view-test--count-substring
                    "Unique reminder body" (buffer-string))))
        (should (equal "> draft\nsecond line" (niwa-view-composer-draft-text nil t)))
        (should (= (+ (niwa-view-composer-draft-start) 4)
                   (with-current-buffer (niwa-view-composer-buffer) (point))))))))

(niwa-deftest niwa-view-agent/adjacent-reentry ()
  ,test
  (test)
  :doc "both refresh nesting orders preserve two handles and an expanded reminder"
  (dolist (order '(full-inside-refresh refresh-inside-full))
    (dolist (reader '(nil t))
      (niwa-view-test--with-buffers
        (niwa-tool-register
         (niwa-tool--create
          :name "AdjacentAgent" :category "niwa"
          :renderer (lambda (_name _args result _data)
                      (list :header "Agent: adjacent worker" :body result
                            :body-mode 'text-mode :vtype 'agent-handle
                            :agent-path "/root/worker" :agent-status 'running
                            :initially-collapsed-p t))))
        (dotimes (n 2)
          (niwa-view-test--insert-data
           data-buf "(:name \"AdjacentAgent\" :args nil)\n\nAgent detail\n"
           (cons 'tool (format "adjacent-agent-%d" n)))
          (niwa-view-test--insert-data data-buf "Between calls\n" 'response))
        (niwa-view-test--insert-data
         data-buf "<system-reminder>\nAdjacent expanded reminder\n</system-reminder>\n" nil)
        (with-current-buffer view-buf
          (niwa-view--full-rerender)
          (goto-char (point-min))
          (search-forward "System reminder")
          (niwa-view-toggle-section)
          (niwa-view-test--insert-composer-draft "> draft\nsecond line" 4)
          (when reader
            (goto-char (point-min))
            (search-forward "Between"))
          (let ((reader-point (point))
                (original (symbol-function 'niwa-view--segment-rendering)) entered)
            (cl-letf (((symbol-function 'niwa-view--segment-rendering)
                       (lambda (&rest args)
                         (prog1 (apply original args)
                           (unless entered
                             (setq entered t)
                             (if (eq order 'full-inside-refresh)
                                 (niwa-view--full-rerender)
                               (niwa-view--refresh-agent-rendering-now "/root/worker")))))))
              (if (eq order 'full-inside-refresh)
                  (niwa-view--refresh-agent-rendering-now "/root/worker")
                (niwa-view--full-rerender)))
            (should entered)
            (should (if reader
                        (= (point) reader-point)
                      (= (+ (niwa-view-composer-draft-start) 4)
                         (with-current-buffer (niwa-view-composer-buffer) (point))))))
          (should (= 2 (length (car (niwa-view--agent-handle-refresh-points
                                    "/root/worker")))))
          (should (= 2 (niwa-view-test--count-substring
                        "Agent: adjacent worker" (buffer-string))))
          (should (= 1 (niwa-view-test--count-substring
                        "Adjacent expanded reminder" (buffer-string))))
          (should (equal "> draft\nsecond line" (niwa-view-composer-draft-text nil t)))
          (let ((pos (point-min)))
            (while (< pos niwa-view--status-marker)
              (when-let* ((source (get-text-property pos 'niwa-view-source)))
                (should (eq data-buf (marker-buffer (car source))))
                (should (<= 1 (car source) (cdr source)
                            (with-current-buffer data-buf (point-max)))))
              (setq pos (1+ pos)))))))))

(niwa-deftest niwa-view-disclosure/source-replacement ()
  ,test
  (test)
  :doc "a queued disclosure cannot expand a replacement source at the same offset"
  (with-temp-buffer
    (org-mode)
    (insert "<system-reminder>\nNew source body\n</system-reminder>\n")
    (let ((replacement (current-buffer)))
      (niwa-view-test--with-buffers
        (niwa-view-test--insert-data
         data-buf "<system-reminder>\nOld source body\n</system-reminder>\n" nil)
        (with-current-buffer replacement
          (setq-local niwa--view-buffer view-buf))
        (with-current-buffer view-buf
          (niwa-view--full-rerender)
          (niwa-view-test--insert-composer-draft "> draft\nsecond line" 4)
          (niwa-view-render-mutate
           'outer
           (lambda ()
             (save-excursion
               (goto-char (point-min))
               (search-forward "System reminder")
               (niwa-view-toggle-section))
             (setq niwa--data-buffer replacement)
             (niwa-view--full-rerender replacement t)))
          (should-not (string-search "Old source body" (buffer-string)))
          (should-not (string-search "New source body" (buffer-string)))
          (save-excursion
            (goto-char (point-min))
            (search-forward "System reminder")
            (should (get-text-property (1- (point)) 'niwa-view-collapsed))
            (should (eq replacement
                        (marker-buffer (car (get-text-property
                                             (1- (point)) 'niwa-view-source)))))
            (niwa-view-toggle-section)
            (should (string-search "New source body" (buffer-string))))
          (should (equal "> draft\nsecond line" (niwa-view-composer-draft-text nil t)))
          (should (= (+ (niwa-view-composer-draft-start) 4)
                     (with-current-buffer (niwa-view-composer-buffer) (point)))))))))

(provide 'test-niwa-view-render-reentry)
;;; test-niwa-view-render-reentry.el ends here
