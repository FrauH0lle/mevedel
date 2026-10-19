;;; test-mevedel-plan-handoff-remote.el --- Browser Worktree continuation -*- lexical-binding: t; -*-

;;; Commentary:

;; Native handoff delegates room continuation before closing the source buffers.

;;; Code:

(require 'mevedel)
(require 'mevedel-plan-handoff)
(require 'mevedel-collaboration)
(require 'mevedel-view)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(mevedel-deftest mevedel-plan-handoff--hand-over
  (:doc "publishes continuation before deferred close without changing native continuity")
  (dolist (shared '(nil t))
    (mevedel-view-test--with-buffers
      (let ((fork (generate-new-buffer " *handoff-fork*"))
            (fork-view (generate-new-buffer " *handoff-fork-view*"))
            close events)
        (unwind-protect
            (save-window-excursion
              (set-window-buffer (selected-window) view-buf)
              (with-current-buffer fork (setq-local mevedel--view-buffer fork-view))
              (cl-letf (((symbol-function 'mevedel-collaboration-hand-off)
                         (lambda (source target)
                           (should (eq source data-buf))
                           (should (eq target fork))
                           (should (buffer-live-p source))
                           (push 'handoff events)
                           (and shared 'target-room)))
                        ((symbol-function 'run-at-time)
                         (lambda (_time _repeat fn &rest _args)
                           (push 'timer events)
                           (setq close fn))))
                (mevedel-plan-handoff--hand-over data-buf fork)
                (should (equal '(timer handoff) events))
                (should (eq fork-view (window-buffer (selected-window))))
                (should (buffer-live-p data-buf))
                (should (buffer-live-p view-buf))
                (funcall close)
                (should-not (buffer-live-p data-buf))
                (should-not (buffer-live-p view-buf))
                (should (buffer-live-p fork))
                (should (buffer-live-p fork-view))))
          (when (buffer-live-p fork-view) (kill-buffer fork-view))
          (when (buffer-live-p fork) (kill-buffer fork)))))))

(provide 'test-mevedel-plan-handoff-remote)
;;; test-mevedel-plan-handoff-remote.el ends here
