;; Mevedel persistent permissions
;; Auto-generated, safe to edit

(:rules
 (("Bash" :pattern "npx @emacs-eask/cli *" :network t :file-system
   ((:path "~/.npm" :access write :recursive t)) :action allow)
  ("Bash" :pattern "git add:*" :action allow)
  ("Bash" :pattern "git diff:*" :action allow)
  ("Bash" :pattern "git status:*" :action allow)
  ("Bash" :pattern "git log:*" :action allow)
  ("Bash" :pattern "npx --offline @emacs-eask/cli compile --help"
   :sandbox-permissions require-escalated :action allow)
  ("Bash" :pattern
   "npx --offline @emacs-eask/cli compile mevedel-view-fontify.el mevedel-transport.el && npx --offline @emacs-eask/cli clean elc"
   :sandbox-permissions require-escalated :action allow)
  ("Bash" :pattern
   "npx @emacs-eask/cli clean elc && npx @emacs-eask/cli test ert test/test-mevedel-turn-ownership.el && npx @emacs-eask/cli test ert test/test-mevedel-turn.el && npx @emacs-eask/cli test ert test/test-mevedel-presets.el && npx @emacs-eask/cli test ert test/test-mevedel-chat.el && npx @emacs-eask/cli test ert test/test-mevedel-directive-request.el"
   :sandbox-permissions require-escalated :action allow)
  ("Bash" :pattern
   "npx @emacs-eask/cli clean elc && npx @emacs-eask/cli test ert test/test-mevedel-view-render-reentry.el test/test-mevedel-view-render.el test/test-mevedel-view-stream.el test/test-mevedel-view-agent-reminders.el test/test-mevedel-view-segments.el"
   :sandbox-permissions require-escalated :action allow))
 :resource-grants
 ((:path "~/.mevedel/skills" :access read)
  (:path "~/.agents/skills" :access read)
  (:path "~/ccs" :access read :recursive t)))
