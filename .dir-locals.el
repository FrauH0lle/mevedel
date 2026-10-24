;; These command-executing variables deliberately require Emacs local-variable
;; approval.  Never mark them safe merely because their types are valid.
((mevedel-worktree-setup-mode
  . ((mevedel-worktree-setup-enabled . t)
     (mevedel-worktree-setup-command
      . ("python3" "scripts/bootstrap-worktree.py")))))
