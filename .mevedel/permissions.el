;; Mevedel persistent permissions
;; Auto-generated, safe to edit

(:rules
 (("Bash" :pattern "npx @emacs-eask/cli *" :network t :file-system
   ((:path "~/.npm" :access write)) :action allow)
  ("Bash" :pattern "git add:*" :action allow)
  ("Bash" :pattern "git diff:*" :action allow)
  ("Bash" :pattern "git status:*" :action allow)
  ("Bash" :pattern "git log:*" :action allow))
 :resource-grants
 ((:path "~/.npm" :access write)
  (:path "~/.mevedel/skills" :access read)
  (:path "~/.agents/skills" :access read)))
