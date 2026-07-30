;; Mevedel persistent permissions
;; Auto-generated, safe to edit

(:rules
 (("Bash" :pattern "npx @emacs-eask/cli *"
   :network t
   :file-system ((:path "~/.npm" :access write))
   :action allow))
 :resource-grants
 ((:path "~/.npm" :access write)))
