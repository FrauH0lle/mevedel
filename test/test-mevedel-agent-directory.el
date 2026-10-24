;;; test-mevedel-agent-directory.el --- Agent cwd boundary tests -*- lexical-binding: t -*-

;;; Commentary:

;; Exercise canonical containment and real Git worktree identity.

;;; Code:

(require 'mevedel-agent-directory)
(require 'mevedel-worktree)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(mevedel-deftest mevedel-agent-directory-resolve
  (:doc "resolves parent-relative directories, rejects symlink escapes and foreign targets")
  (let* ((base (make-temp-file "mevedel-agent-cwd-" t))
         (root (file-name-as-directory (file-name-concat base "workspace")))
         (child (file-name-as-directory (file-name-concat root "child")))
         (nested (file-name-as-directory (file-name-concat child "nested")))
         (outside (file-name-concat base "outside"))
         (workspace (mevedel-workspace--create :root root))
         (session (mevedel-session--create :workspace workspace :working-directory root)))
    (unwind-protect
        (with-temp-buffer
          (make-directory nested t)
          (make-directory outside)
          (make-symbolic-link outside (file-name-concat root "escape"))
          (setq-local mevedel--session session)
          (should (equal child (mevedel-agent-directory-resolve session "child")))
          (setq-local mevedel--conversation-working-directory child)
          (should (equal nested (mevedel-agent-directory-resolve session "nested")))
          (should (equal root (mevedel-agent-directory-resolve session "..")))
          (dolist (bad (list "" 12 "absent" outside
                            (file-name-concat root "escape")
                            "/ssh:unrelated.invalid:/tmp/"))
            (should-error (mevedel-agent-directory-resolve session bad)))
          (with-temp-file (file-name-concat root "file") (insert "not a directory"))
          (should-error
           (mevedel-agent-directory-resolve session (file-name-concat root "file"))))
      (delete-directory base t))))

(provide 'test-mevedel-agent-directory)

;;; test-mevedel-agent-directory.el ends here
