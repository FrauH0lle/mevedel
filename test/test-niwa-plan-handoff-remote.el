;;; test-niwa-plan-handoff-remote.el --- Browser Worktree continuation -*- lexical-binding: t; -*-

;;; Commentary:

;; Native handoff delegates room continuation before closing the source buffers.

;;; Code:

(require 'niwa)
(require 'niwa-plan-handoff)
(require 'niwa-collaboration)
(require 'niwa-view)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(niwa-deftest niwa-plan-handoff--hand-over
  (:doc "publishes continuation before deferred close without changing native continuity")
  (dolist (shared '(nil t))
    (niwa-view-test--with-buffers
      (let ((fork (generate-new-buffer " *handoff-fork*"))
            (fork-view (generate-new-buffer " *handoff-fork-view*"))
            close events)
        (unwind-protect
            (save-window-excursion
              (set-window-buffer (selected-window) view-buf)
              (with-current-buffer fork (setq-local niwa--view-buffer fork-view))
              (cl-letf (((symbol-function 'niwa-collaboration-hand-off)
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
                (niwa-plan-handoff--hand-over data-buf fork)
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

(provide 'test-niwa-plan-handoff-remote)
;;; test-niwa-plan-handoff-remote.el ends here
