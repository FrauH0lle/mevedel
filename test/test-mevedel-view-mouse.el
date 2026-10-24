;;; test-mevedel-view-mouse.el --- Live view mouse interleavings -*- lexical-binding: t -*-

;;; Commentary:

;; Deliver mouse events through the graphical command loop, not direct command
;; calls.  A post-command hook imposes a live update between down and release.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-execution)
(require 'mevedel-background-list)
(require 'mevedel-view)
(require 'mevedel-view-agent)
(require 'mevedel-view-execution)
(require 'mevedel-view-render)
(require 'mevedel-view-stream)

(defun mevedel-view-mouse-test--click (position update redraw queued)
  "Click POSITION with UPDATE between down and release.
With REDRAW, redisplay after UPDATE.  With QUEUED, resolve the release before
UPDATE, like input already queued by the display backend."
  (let* ((window (selected-window))
         (start (posn-at-point position window))
         (xy (cons (car (posn-x-y start))
                   (+ (cdr (posn-x-y start))
                      (window-header-line-height window)
                      (window-tab-line-height window))))
         (release (list 'mouse-1 (copy-tree start)))
         (phase 'waiting)
         failure commands timer exit-timer)
    (should start)
    (ert-info ((format "Start %S; lookup %S; header %S; edges %S"
                       start (posn-at-x-y (car xy) (cdr xy) window)
                       header-line-format (window-inside-pixel-edges window)))
	      (should (= position (posn-point (posn-at-x-y (car xy) (cdr xy) window)))))
    (let ((before
           (lambda ()
             (pcase (car-safe last-command-event)
               ('down-mouse-1 (setq phase 'down))
               ('mouse-1 (setq phase 'up)))))
          (hook
           (lambda ()
             (when (memq phase '(down up)) (push this-command commands))
             (condition-case err
                 (pcase phase
                   ('down
                    (setq phase 'waiting)
                    (should (= position (point)))
                    (funcall update)
                    (when redraw (redisplay t))
                    (setq unread-command-events
                          (list (if queued release
                                  (list 'mouse-1
                                        (posn-at-x-y (car xy) (cdr xy) window))))))
                   ('up
                    (setq phase 'done)
                    ;; Let the remaining production post-command hooks drain
                    ;; deferred projection writes before leaving the loop.
                    (setq exit-timer (run-at-time 0 nil #'exit-recursive-edit))))
               (error (setq failure err) (exit-recursive-edit))))))
      (unwind-protect
          (progn
            (add-hook 'pre-command-hook before nil t)
            (add-hook 'post-command-hook hook nil t)
            (setq timer (run-at-time 3 nil
                                     (lambda ()
                                       (setq failure '(error "Mouse loop timed out"))
                                       (exit-recursive-edit))))
            (setq unread-command-events (list (list 'down-mouse-1 start)))
            (recursive-edit))
        (remove-hook 'pre-command-hook before t)
        (remove-hook 'post-command-hook hook t)
        (when timer (cancel-timer timer))
        (when exit-timer (cancel-timer exit-timer))
        (setq unread-command-events nil)))
    (when failure (signal (car failure) (cdr failure)))
    (should (= 2 (length commands)))
    (should (cl-some (lambda (command)
                       (memq command '(mouse-drag-region
                                       mevedel-view-execution--mouse-press)))
                     commands))))

(mevedel-deftest mevedel-view-mouse-live-update ()
  (progn
    (skip-unless (display-graphic-p))
    (save-window-excursion
      (delete-other-windows)
      (mevedel-view-test--with-buffers
	(mevedel-view-test--insert-data data-buf "Mouse prompt\n\n" nil)
	(mevedel-view-test--insert-data
	 data-buf "An assistant response.\n\n" 'response)
	(mevedel-view-test--insert-data
	 data-buf
	 "#+begin_tool\n(:name \"Read\" :args (:file_path \"mouse-tool-target.el\"))\n\ncontent\n#+end_tool\n"
	 '(tool . "mouse-tool"))
	(switch-to-buffer view-buf)
	(let* ((session (mevedel-session--create :name "mouse"))
               (clock 1000.0)
               (original-float-time (symbol-function 'float-time))
               (executions
		(list (list :execution-id "mouse-exec" :owner "/root" :yielded t
                            :state 'running :description "Click background target"
                            :started-at 941.0)))
               (draft "> click composer target\nsecond line")
               (needle ,needle)
               (redraw ,redraw)
               (queued ,queued)
               (operation ,operation))
          (setq-local mevedel--session session)
          (with-current-buffer data-buf (setq-local mevedel--session session))
          (mevedel-view-test--insert-composer-draft draft)
          (cl-letf (((symbol-function 'float-time)
                     (lambda (&optional time)
                       (if time (funcall original-float-time time) clock)))
                    ((symbol-function 'frame-focus-state) (lambda (&optional _) t))
                    ((symbol-function 'mevedel-execution-count-user) (lambda (_) 1))
                    ((symbol-function 'mevedel-execution-list-user) (lambda (_) executions))
                    ((symbol-function 'mevedel-view--agent-status-collect) (lambda () nil)))
            (mevedel-view--full-rerender)
            (mevedel-view-stream-begin-turn
             (mevedel-view--history-insertion-marker)
             (with-current-buffer data-buf (point-max)))
            (mevedel-view-test--insert-data data-buf "Initial stream text." 'response)
            (mevedel-view--render-stream-update data-buf)
            (goto-char (point-min))
            (search-forward needle)
            (let ((clicked (- (point) 3)))
              (goto-char (point-min))
              (set-window-start nil (point-min))
              (redisplay t)
              (should mevedel-view-execution--elapsed-targets)
              (mevedel-view-mouse-test--click
               clicked
               (lambda ()
		 (if (eq operation 'tick)
                     (progn
                       (setq clock 1001.0)
                       (mevedel-view--spinner-tick))
                   (mevedel-view-test--insert-data data-buf " Additional chunk." 'response)
                   (mevedel-view--render-stream-update data-buf)))
               redraw queued)
              (if (eq operation 'tick)
                  (should (string-search "1m 00s" (buffer-string)))
		(should (string-search "Additional chunk." (buffer-string))))
              (let ((actual (point))
                    (expected (save-excursion
				(goto-char (point-min))
				(search-forward needle)
				(- (point) 3))))
		(should (equal draft (mevedel-view--input-text)))
		(should (= expected actual)))))))))
  (needle redraw queued operation)
  :doc "composer click, stale geometry, release resolved after update"
  "click composer target" nil nil 'tick
  :doc "composer click, fresh geometry, release resolved after update"
  "click composer target" t nil 'tick
  :doc "composer click, stale geometry, release already queued"
  "click composer target" nil t 'tick
  :doc "composer click, fresh geometry, release already queued"
  "click composer target" t t 'tick
  :doc "background click, stale geometry, release resolved after update"
  "Click background target" nil nil 'tick
  :doc "background click, fresh geometry, release resolved after update"
  "Click background target" t nil 'tick
  :doc "background click, stale geometry, release already queued"
  "Click background target" nil t 'tick
  :doc "background click, fresh geometry, release already queued"
  "Click background target" t t 'tick
  :doc "tool click, stale geometry, queued release across elapsed tick"
  "mouse-tool-target.el" nil t 'tick
  :doc "tool click, fresh geometry, queued release across elapsed tick"
  "mouse-tool-target.el" t t 'tick
  :doc "composer click, stale geometry, release resolved after stream chunk"
  "click composer target" nil nil 'stream
  :doc "composer click, fresh geometry, release resolved after stream chunk"
  "click composer target" t nil 'stream
  :doc "composer click, stale geometry, queued release across stream chunk"
  "click composer target" nil t 'stream
  :doc "composer click, fresh geometry, queued release across stream chunk"
  "click composer target" t t 'stream
  :doc "background click, stale geometry, queued release across stream chunk"
  "Click background target" nil t 'stream
  :doc "background click, fresh geometry, queued release across stream chunk"
  "Click background target" t t 'stream
  :doc "tool click, stale geometry, queued release across stream chunk"
  "mouse-tool-target.el" nil t 'stream
  :doc "tool click, fresh geometry, queued release across stream chunk"
  "mouse-tool-target.el" t t 'stream)

(defun mevedel-view-mouse-test--wait (predicate)
  "Run real timers until PREDICATE succeeds, with a bounded deadline."
  (let ((deadline (+ (float-time) 1.0)))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil 0.01))
    (should (funcall predicate))))

(defvar mevedel-view-mouse-test--macro-resume nil)
(defvar mevedel-view-mouse-test--macro-observations nil)
(defvar mevedel-view-mouse-test--macro-requeue nil)

(defun mevedel-view-mouse-test--macro-press (event)
  "Track EVENT's release inside one command, like macro-based mouse clients."
  (interactive "e")
  (mouse-set-point event)
  (let (release)
    (track-mouse
      (funcall mevedel-view-mouse-test--macro-resume 'schedule)
      (push (list 'before track-mouse (car-safe last-command-event)
                  (eq last-command-event event)
                  (not (null (terminal-parameter nil 'mouse-drag-start)))
                  (buffer-modified-tick)
                  (string-search "Deferred response 5" (buffer-string)))
            mevedel-view-mouse-test--macro-observations)
      (setq release (read-key))
      (push (list 'after track-mouse (car-safe last-command-event)
                  (eq last-command-event event)
                  (not (null (terminal-parameter nil 'mouse-drag-start)))
                  (buffer-modified-tick)
                  (string-search "Deferred response 5" (buffer-string)))
            mevedel-view-mouse-test--macro-observations))
    (if mevedel-view-mouse-test--macro-requeue
        (push release unread-command-events)
      (mouse-set-point release))))

(defun mevedel-view-mouse-test--macro-click (position resume &optional requeue)
  "Click POSITION while RESUME runs timers inside macro-based tracking.
With REQUEUE, hand the release back to the ordinary command loop."
  (let* ((start (posn-at-point position))
         (release (list 'mouse-1 (copy-tree start)))
         (mevedel-view-mouse-test--macro-resume resume)
         (mevedel-view-mouse-test--macro-requeue requeue)
         (map (copy-keymap (current-local-map)))
         timer exit-timer deadline)
    ;; Preserve the real release binding from the clicked fragment.  The
    ;; override is only needed to replace its press handler with this fixture.
    (define-key map [mouse-1] (key-binding [mouse-1]))
    (define-key map [down-mouse-1] #'mevedel-view-mouse-test--macro-press)
    (let ((overriding-local-map map)
          (old-map (current-local-map))
          (after (lambda ()
                   (when (and (= 2 (length mevedel-view-mouse-test--macro-observations))
                              (not (cl-some #'mouse-event-p unread-command-events)))
                     (setq exit-timer (run-at-time 0 nil #'exit-recursive-edit))))))
      (unwind-protect
          (progn
            (use-local-map map)
            (add-hook 'post-command-hook after nil t)
            ;; The release arrives while `read-key' runs real production timers.
            (setq timer (run-at-time 0.65 nil
                                     (lambda ()
                                       (setq unread-command-events (list release)))))
            (setq deadline (run-at-time 1.5 nil #'exit-recursive-edit))
            (setq unread-command-events (list (list 'down-mouse-1 start)))
            (recursive-edit)
            (should (= 2 (length mevedel-view-mouse-test--macro-observations))))
        (use-local-map old-map)
        (remove-hook 'post-command-hook after t)
        (when timer (cancel-timer timer))
        (when deadline (cancel-timer deadline))
        (when exit-timer (cancel-timer exit-timer))
        (setq unread-command-events nil)))))

(defun mevedel-view-mouse-test--focus-click (position ordering resume)
  "Click POSITION across focus RESUME with the specified ORDERING."
  (let* ((start (posn-at-point position))
         (release (list 'mouse-1 (copy-tree start)))
         (phase nil)
         (commands 0)
         timer exit-timer failure)
    (should start)
    (when (eq ordering 'before-fresh)
      (funcall resume t)
      (setq start (posn-at-point (point))
            release (list 'mouse-1 (copy-tree start))))
    (let ((before
           (lambda ()
             (pcase (car-safe last-command-event)
               ('down-mouse-1
                (setq phase 'down))
               ('mouse-1 (setq phase 'up)))))
          (after
           (lambda ()
             (condition-case err
                 (pcase phase
                   ('down
                    (cl-incf commands)
                    (setq phase nil)
                    (when (eq ordering 'between) (funcall resume nil))
                    (setq unread-command-events (list release)))
                   ('up
                    (cl-incf commands)
                    (setq phase nil)
                    ;; Leave the loop only after production release hooks run.
                    (setq exit-timer (run-at-time 0 nil #'exit-recursive-edit))))
               (error (setq failure err) (exit-recursive-edit))))))
      (unwind-protect
          (progn
            (add-hook 'pre-command-hook before nil t)
            (add-hook 'post-command-hook after nil t)
            (setq timer (run-at-time 3 nil
                                     (lambda ()
                                       (setq failure '(error "Focus click timed out"))
                                       (exit-recursive-edit))))
            (setq unread-command-events (list (list 'down-mouse-1 start)))
            (recursive-edit))
        (remove-hook 'pre-command-hook before t)
        (remove-hook 'post-command-hook after t)
        (when timer (cancel-timer timer))
        (when exit-timer (cancel-timer exit-timer))
        (setq unread-command-events nil)))
    (when failure (signal (car failure) (cdr failure)))
    (should (= 2 commands))))

(mevedel-deftest mevedel-view-mouse-focus-resume ()
  (progn
    (skip-unless (display-graphic-p))
    (save-window-excursion
      (delete-other-windows)
      (mevedel-view-test--with-buffers
        (mevedel-view-test--insert-data data-buf "Mouse prompt\n\n" nil)
        (mevedel-view-test--insert-data data-buf "Initial response.\n\n" 'response)
        (switch-to-buffer view-buf)
        (let* ((session (mevedel-session--create :name "focus-mouse"))
               (focused t)
               (kind ,kind)
               (ordering ,ordering)
               (mevedel-view-mouse-test--macro-observations nil)
               managed
               (draft "> focus composer target\nsecond line")
               (needle ,needle)
               (executions
                (list (list :execution-id "focus-exec" :owner "/root" :yielded t
                            :state 'running :description "Focus background target"
                            :started-at (float-time))))
               (mevedel-view-rerender-debounce 0.015))
          (setq-local mevedel--session session)
          (with-current-buffer data-buf (setq-local mevedel--session session))
          (mevedel-view-test--insert-composer-draft draft)
          ;; Only the focus scalar is simulated.  The real unattended predicate,
          ;; scheduling/resume hooks, debounce timer and projection writers run.
          (cl-letf (((symbol-function 'frame-focus-state) (lambda (&optional _) focused))
                    ((symbol-function 'mevedel-execution-count-user) (lambda (_) 1))
                    ((symbol-function 'mevedel-execution-list-user) (lambda (_) executions))
                    ((symbol-function 'mevedel-background-list-open) (lambda () (setq managed t)))
                    ((symbol-function 'mevedel-view--agent-status-collect) (lambda () nil)))
            (mevedel-view--full-rerender)
            (unless (eq kind 'batch)
              (mevedel-view-stream-begin-turn
               (mevedel-view--history-insertion-marker)
               (with-current-buffer data-buf (point-max)))
              (mevedel-view-test--insert-data data-buf "Live initial." 'response)
              (mevedel-view--render-stream-update data-buf))
            (goto-char (point-min))
            (search-forward needle)
            (backward-char 3)
            (let ((clicked (point))
                  (old-text (buffer-string)))
              (set-window-start nil (point-min))
              (redisplay t)
              (setq focused nil)
              (should (mevedel-view--unattended-p))
              (dotimes (index 6)
                (when (eq kind 'batch)
                  (mevedel-view-test--insert-data data-buf (format "Deferred prompt %d\n\n" index) nil))
                (mevedel-view-test--insert-data
                 data-buf (format "Deferred response %d: new transcript content.\n\n" index) 'response)
                (mevedel-view--schedule-render
                 (if (eq kind 'incremental) 'incremental 'full)
                 data-buf mevedel-view-rerender-debounce))
              (should (equal old-text (buffer-string)))
              (should-not mevedel-view--render-timer)
              (should mevedel-view--pending-render-kind)
              (funcall
               (if (memq ordering '(macro-between macro-requeue))
                   (lambda (pos _order callback)
                     (mevedel-view-mouse-test--macro-click
                      pos callback (eq ordering 'macro-requeue)))
                 #'mevedel-view-mouse-test--focus-click)
               clicked ordering
               (lambda (complete)
                 (setq focused t)
                 (mevedel-view--resume-attended-views)
                 (should mevedel-view--render-timer)
                 ;; The actual scheduled timer must fire, not a direct flush.
                 (unless (eq complete 'schedule)
                   (mevedel-view-mouse-test--wait
                    (lambda () (null mevedel-view--render-timer))))
                 (when (eq complete t)
                   (mevedel-view-mouse-test--wait
                    (lambda () (null mevedel-view-render--batch))))))
              (mevedel-view-mouse-test--wait
               (lambda () (null mevedel-view-render--batch)))
              (should (string-search "Deferred response 5" (buffer-string)))
              (should (equal draft (mevedel-view--input-text)))
              (when (equal needle "[Manage]") (should managed))
              (let ((actual (point))
                    (expected (save-excursion
                                (goto-char (point-min))
                                (search-forward needle)
                                (- (point) 3))))
                (ert-info ((format "Kind %S ordering %S target %S: old=%d actual=%d expected=%d; macro=%S"
                                   kind ordering needle clicked actual expected
                                   mevedel-view-mouse-test--macro-observations))
                  (should (= expected actual))))))))))
  (kind ordering needle)
  :doc "full catch-up between press and release"
  'full 'between "focus composer target"
  :doc "incremental catch-up between press and release"
  'incremental 'between "focus composer target"
  :doc "batched catch-up between press and release"
  'batch 'between "focus composer target"
  :doc "full catch-up before freshly resolved press"
  'full 'before-fresh "focus composer target"
  :doc "incremental catch-up before freshly resolved press"
  'incremental 'before-fresh "focus composer target"
  :doc "batched catch-up before freshly resolved press"
  'batch 'before-fresh "focus composer target"
  :doc "full background catch-up between press and release"
  'full 'between "Focus background target"
  :doc "incremental background catch-up between press and release"
  'incremental 'between "Focus background target"
  :doc "batched background catch-up between press and release"
  'batch 'between "Focus background target"
  :doc "full background catch-up before freshly resolved press"
  'full 'before-fresh "Focus background target"
  :doc "incremental background catch-up before freshly resolved press"
  'incremental 'before-fresh "Focus background target"
  :doc "batched background catch-up before freshly resolved press"
  'batch 'before-fresh "Focus background target"
  :doc "full catch-up during macro-based tracking"
  'full 'macro-between "focus composer target"
  :doc "incremental catch-up during macro-based tracking"
  'incremental 'macro-between "focus composer target"
  :doc "batched catch-up during macro-based tracking"
  'batch 'macro-between "focus composer target"
  :doc "full background catch-up during macro-based tracking"
  'full 'macro-between "Focus background target"
  :doc "incremental background catch-up during macro-based tracking"
  'incremental 'macro-between "Focus background target"
  :doc "batched background catch-up during macro-based tracking"
  'batch 'macro-between "Focus background target"
  :doc "full catch-up with macro-requeued release"
  'full 'macro-requeue "focus composer target"
  :doc "incremental catch-up with macro-requeued release"
  'incremental 'macro-requeue "focus composer target"
  :doc "batched catch-up with macro-requeued release"
  'batch 'macro-requeue "focus composer target"
  :doc "full background catch-up with macro-requeued release"
  'full 'macro-requeue "Focus background target"
  :doc "incremental background catch-up with macro-requeued release"
  'incremental 'macro-requeue "Focus background target"
  :doc "batched background catch-up with macro-requeued release"
  'batch 'macro-requeue "Focus background target"
  :doc "full plain transcript click with macro-requeued release"
  'full 'macro-requeue "Initial response"
  :doc "incremental plain transcript click with macro-requeued release"
  'incremental 'macro-requeue "Initial response"
  :doc "batched plain transcript click with macro-requeued release"
  'batch 'macro-requeue "Initial response"
  :doc "full Manage button click with macro-requeued release"
  'full 'macro-requeue "[Manage]"
  :doc "incremental Manage button click with macro-requeued release"
  'incremental 'macro-requeue "[Manage]"
  :doc "batched Manage button click with macro-requeued release"
  'batch 'macro-requeue "[Manage]")

(provide 'test-mevedel-view-mouse)
;;; test-mevedel-view-mouse.el ends here
