;;; niwa-agent-directory.el --- Agent execution directory boundary -*- lexical-binding: t -*-

;;; Commentary:

;; Resolves a retained agent's optional execution directory on the session's
;; existing target.  Directory membership is not filesystem authority: shared
;; permission rules, resource grants and sandbox roots still decide access.

;;; Code:

(require 'niwa-structs)
(require 'niwa-execution-target)

;; `niwa-worktree'
(declare-function niwa-worktree-related-directory-p
                  "niwa-worktree" (directory workspace-root))
(autoload 'niwa-worktree-related-directory-p "niwa-worktree")

(defun niwa-agent-directory-resolve (session directory)
  "Resolve and validate agent DIRECTORY in SESSION.
Relative names start in the caller's conversation directory.  Accept existing
directories within the canonical workspace or a registered worktree of that
workspace's repository, on the same execution target.  Return a canonical
absolute directory.  This function grants no resource or execution authority."
  (unless (and (stringp directory) (not (equal directory "")))
    (user-error "Agent working_directory must be a non-empty directory path"))
  (let* ((workspace (niwa-session-workspace session))
         (root (niwa-workspace-root workspace))
         (target (or (niwa-session-execution-target session)
                     (niwa-execution-target-create root)))
         (path (niwa-execution-target-expand-path
                target (substring-no-properties directory)
                (niwa-conversation-working-directory session))))
    (unless (file-directory-p path)
      (user-error "Agent working_directory does not exist or is not a directory: %s"
                  path))
    (let ((canonical (file-name-as-directory (file-truename path)))
          (root (file-name-as-directory (file-truename root))))
      (unless (or (equal canonical root)
                  (file-in-directory-p canonical root)
                  (niwa-worktree-related-directory-p canonical root))
        (user-error "Agent working_directory is outside the workspace and its registered Git worktrees: %s"
                    path))
      canonical)))

(provide 'niwa-agent-directory)

;;; niwa-agent-directory.el ends here
