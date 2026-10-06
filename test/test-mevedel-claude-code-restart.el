;;; test-mevedel-claude-code-restart.el --- Cold subscription resume -*- lexical-binding: t -*-

;;; Commentary:
;; Two fresh Emacs processes share only published files and an ACP fixture.

;;; Code:

(require 'helpers
         (file-name-concat (file-name-directory (or load-file-name buffer-file-name)) "helpers"))

(defconst mevedel-claude-restart-test--fixtures
  (file-name-concat (file-name-directory (or load-file-name buffer-file-name)) "fixtures"))

(mevedel-deftest mevedel-session-persistence-restore/claude-restart ()
  (let ((root (make-temp-file "mevedel-claude-restart-" t))
        (emacs (expand-file-name invocation-name invocation-directory)))
    (unwind-protect
        (dolist (phase '(1 2))
          (with-temp-buffer
            (let* ((form `(progn
                            (setq load-path ',load-path)
                            (load ,(file-name-concat mevedel-claude-restart-test--fixtures "claude-restart.el") nil t)
                            (mevedel-test-claude-restart
                             ,root ,phase ,(file-name-concat mevedel-claude-restart-test--fixtures "acp-agent.py"))))
                   (status (call-process emacs nil t nil "--batch" "-Q" "--eval" (prin1-to-string form))))
              (ert-info ((format "Restart phase %d: %s" phase (buffer-string)))
                (should (equal 0 status))
                (should (string-empty-p (buffer-string)))))))
      (delete-directory root t))))

(provide 'test-mevedel-claude-code-restart)
;;; test-mevedel-claude-code-restart.el ends here
