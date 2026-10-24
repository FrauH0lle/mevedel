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

(provide 'test-mevedel-view-mouse)
;;; test-mevedel-view-mouse.el ends here
