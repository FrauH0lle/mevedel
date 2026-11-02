;; Mevedel persistent permissions
;; Auto-generated, safe to edit

(:rules
 (("Bash" :pattern "npx @emacs-eask/cli *" :file-system
   ((:path "~/.npm" :access write :recursive t)
    (:path "~/.eask" :access write :recursive t))
   :action allow)
  ("Bash" :pattern "npx --offline @emacs-eask/cli *" :file-system
   ((:path "~/.npm" :access write :recursive t)
    (:path "~/.eask" :access write :recursive t))
   :action allow)
  ("Bash" :pattern "git add:*" :file-system
   ((:path "./.git" :access write :recursive t)) :action allow)
  ("Bash" :pattern "git commit:*" :file-system
   ((:path "./.git" :access write :recursive t)) :action allow)
  ("Bash" :pattern "git worktree add:*" :file-system
   ((:path "./.git" :access write :recursive t)) :action allow)
  ("Bash" :pattern "git diff:*" :action allow)
  ("Bash" :pattern "git status:*" :action allow)
  ("Bash" :pattern "git log:*" :action allow)
  ("Bash" :pattern "git fetch:*" :network t :file-system
   ((:path "~/.ssh" :access read :recursive t)
    (:path "./.git" :access write :recursive t))
   :action allow)
  ("Bash" :pattern "git -C .scratch/upstream/gptel pull --ff-only" :network t
   :file-system
   ((:path "./.scratch/upstream/gptel" :access write :recursive t)
    (:path "./.scratch/upstream/gptel/.git" :access write :recursive t))
   :action allow))
 :resource-grants
 ((:path "~/.mevedel/skills" :access read)
  (:path "~/.agents/skills" :access read)
  (:path "~/ccs" :access read :recursive t))
 :safe-env-vars ("CHECK_BASE"))
