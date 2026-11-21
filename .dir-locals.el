;; These command-executing variables deliberately require Emacs local-variable
;; approval.  Never mark them safe merely because their types are valid.
((niwa-worktree-setup-mode
  . ((niwa-worktree-setup-enabled . t)
     (niwa-worktree-setup-command
      . ("python3" "scripts/bootstrap-worktree.py")))))
