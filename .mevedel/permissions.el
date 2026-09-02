;; Mevedel persistent permissions
;; Auto-generated, safe to edit

(:rules
 (("Bash" :pattern "npx @emacs-eask/cli *" :network t :file-system
   ((:path "/home/roland/.npm" :access write)) :action allow)
  ("Bash" :pattern "npx @emacs-eask/cli clean elc" :action allow)
  ("Bash" :pattern
   "npx @emacs-eask/cli test ert test/test-mevedel-skills-prompt.el test/test-mevedel-skills-ui.el test/test-mevedel-compact-evidence.el"
   :action allow)
  ("Bash" :pattern
   "npx @emacs-eask/cli clean elc && npx @emacs-eask/cli test ert test/test-mevedel-skills-prompt.el test/test-mevedel-skills-ui.el test/test-mevedel-compact-evidence.el"
   :file-system ((:path "/home/roland/.npm" :access write)) :action allow)
  ("Bash" :pattern "git add:*" :action allow)
  ("Bash" :pattern "git diff:*" :action allow)
  ("Bash" :pattern "git status:*" :action allow)
  ("Bash" :pattern "git log:*" :action allow))
 :resource-grants
 ((:path "/home/roland/.npm" :access write)
  (:path "/home/roland/.mevedel/skills" :access read)
  (:path "/home/roland/.agents/skills" :access read)))
