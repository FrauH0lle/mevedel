;;; test-niwa-agent-directory.el --- Agent cwd boundary tests -*- lexical-binding: t -*-

;;; Commentary:

;; Exercise canonical containment and real Git worktree identity.

;;; Code:

(require 'niwa-agent-directory)
(require 'niwa-worktree)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(niwa-deftest niwa-agent-directory-resolve
  (:doc "resolves parent-relative directories, rejects symlink escapes and foreign targets")
  (let* ((base (make-temp-file "niwa-agent-cwd-" t))
         (root (file-name-as-directory (file-name-concat base "workspace")))
         (child (file-name-as-directory (file-name-concat root "child")))
         (nested (file-name-as-directory (file-name-concat child "nested")))
         (outside (file-name-concat base "outside"))
         (workspace (niwa-workspace--create :root root))
         (session (niwa-session--create :workspace workspace :working-directory root)))
    (unwind-protect
        (with-temp-buffer
          (make-directory nested t)
          (make-directory outside)
          (make-symbolic-link outside (file-name-concat root "escape"))
          (setq-local niwa--session session)
          (should (equal child (niwa-agent-directory-resolve session "child")))
          (setq-local niwa--conversation-working-directory child)
          (should (equal nested (niwa-agent-directory-resolve session "nested")))
          (should (equal root (niwa-agent-directory-resolve session "..")))
          (dolist (bad (list "" 12 "absent" outside
                            (file-name-concat root "escape")
                            "/ssh:unrelated.invalid:/tmp/"))
            (should-error (niwa-agent-directory-resolve session bad)))
          (with-temp-file (file-name-concat root "file") (insert "not a directory"))
          (should-error
           (niwa-agent-directory-resolve session (file-name-concat root "file"))))
      (delete-directory base t))))

(provide 'test-niwa-agent-directory)

;;; test-niwa-agent-directory.el ends here
