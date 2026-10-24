;;; mevedel-agent-directory.el --- Agent execution directory boundary -*- lexical-binding: t -*-

;;; Commentary:

;; Resolves a retained agent's optional execution directory on the session's
;; existing target.  Directory membership is not filesystem authority: shared
;; permission rules, resource grants and sandbox roots still decide access.

;;; Code:

(require 'mevedel-structs)
(require 'mevedel-execution-target)

;; `mevedel-worktree'
(declare-function mevedel-worktree-related-directory-p
                  "mevedel-worktree" (directory workspace-root))
(autoload 'mevedel-worktree-related-directory-p "mevedel-worktree")

(defun mevedel-agent-directory-resolve (session directory)
  "Resolve and validate agent DIRECTORY in SESSION.
Relative names start in the caller's conversation directory.  Accept existing
directories within the canonical workspace or a registered worktree of that
workspace's repository, on the same execution target.  Return a canonical
absolute directory.  This function grants no resource or execution authority."
  (unless (and (stringp directory) (not (equal directory "")))
    (user-error "Agent working_directory must be a non-empty directory path"))
  (let* ((workspace (mevedel-session-workspace session))
         (root (mevedel-workspace-root workspace))
         (target (or (mevedel-session-execution-target session)
                     (mevedel-execution-target-create root)))
         (path (mevedel-execution-target-expand-path
                target (substring-no-properties directory)
                (mevedel-conversation-working-directory session))))
    (unless (file-directory-p path)
      (user-error "Agent working_directory does not exist or is not a directory: %s"
                  path))
    (let ((canonical (file-name-as-directory (file-truename path)))
          (root (file-name-as-directory (file-truename root))))
      (unless (or (equal canonical root)
                  (file-in-directory-p canonical root)
                  (mevedel-worktree-related-directory-p canonical root))
        (user-error "Agent working_directory is outside the workspace and its registered Git worktrees: %s"
                    path))
      canonical)))

(provide 'mevedel-agent-directory)

;;; mevedel-agent-directory.el ends here
