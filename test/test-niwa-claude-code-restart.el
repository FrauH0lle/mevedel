;;; test-niwa-claude-code-restart.el --- Cold subscription resume -*- lexical-binding: t -*-

;;; Commentary:
;; Two fresh Emacs processes share only published files and an ACP fixture.

;;; Code:

(require 'helpers
         (file-name-concat (file-name-directory (or load-file-name buffer-file-name)) "helpers"))

(defconst niwa-claude-restart-test--fixtures
  (file-name-concat (file-name-directory (or load-file-name buffer-file-name)) "fixtures"))

(niwa-deftest niwa-session-persistence-restore/claude-restart ()
  (let ((root (make-temp-file "niwa-claude-restart-" t))
        (emacs (expand-file-name invocation-name invocation-directory)))
    (unwind-protect
        (dolist (phase '(1 2))
          (with-temp-buffer
            (let* ((form `(progn
                            (setq load-path ',load-path)
                            (load ,(file-name-concat niwa-claude-restart-test--fixtures "claude-restart.el") nil t)
                            (niwa-test-claude-restart
                             ,root ,phase ,niwa-test--acp-peer)))
                   (status (call-process emacs nil t nil "--batch" "-Q" "--eval" (prin1-to-string form))))
              (ert-info ((format "Restart phase %d: %s" phase (buffer-string)))
                (should (equal 0 status))
                (should (string-empty-p (buffer-string)))))))
      (delete-directory root t))))

(provide 'test-niwa-claude-code-restart)
;;; test-niwa-claude-code-restart.el ends here
