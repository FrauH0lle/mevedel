;;; mevedel-tool-exec-registration.el --- Exec tool catalog -*- lexical-binding: t -*-

;;; Commentary:

;; Complete discovery metadata; implementation loads at first use.

;;; Code:

(require 'mevedel-tool-registry)

(autoload 'mevedel-tool-exec--bash "mevedel-tool-exec")
(autoload 'mevedel-tool-exec--eval "mevedel-tool-exec")
(autoload 'mevedel-tool-exec--list-executions "mevedel-tool-exec")
(autoload 'mevedel-tool-exec--render-bash "mevedel-tool-exec")
(autoload 'mevedel-tool-exec--render-eval "mevedel-tool-exec")
(autoload 'mevedel-tool-exec--stop-execution "mevedel-tool-exec")
(autoload 'mevedel-tool-exec--write-stdin "mevedel-tool-exec")
(autoload 'mevedel-tool-exec-permission-check-bash-async "mevedel-tool-exec-permission")
(autoload 'mevedel-tool-exec-permission-check-eval-async "mevedel-tool-exec-permission")

(defun mevedel-tool-exec--register ()
  "Register Bash and Eval tools."
  (mevedel-define-tool
   :name "Bash"
   :description "Execute Bash commands."
   :prompt-file "prompts/tools/bash.md"
   :handler #'mevedel-tool-exec--bash
   :args ((command string :required
                   "The Bash command to execute from the session working directory. Can include pipes and standard shell operators.")
          (yield_time_ms integer :optional
                         "Milliseconds to wait before yielding a still-running command. Defaults to 10000; range 250-30000."
                         :minimum 250 :maximum 30000)
          (tty boolean :optional
               "Allocate a PTY and retain stdin for prompts or REPL input. Defaults to false.")
          (sandbox_permissions string :optional
                               "Child-execution authority: use_default, with_additional_permissions, or require_escalated for a complete confinement bypass."
                               :enum ["use_default"
                                      "with_additional_permissions"
                                      "require_escalated"])
          (additional_permissions object :optional
                                  "Capabilities requested in addition to the default confinement profile."
                                  :properties
                                  (:network
                                   (:type boolean
                                          :description "Allow network access for this invocation.")
                                   :file_system
                                   (:type object
                                          :description "Exact filesystem paths to reopen inside confinement."
                                          :properties
                                          (:read
                                           (:type array
                                                  :items (:type string)
                                                  :description "Absolute paths requiring read access.")
                                           :write
                                           (:type array
                                                  :items (:type string)
                                                  :description "Absolute paths requiring write access.")))))
          (justification string :optional
                         "Concise user-facing reason for a non-default permission request."))
   :async-p t
   :max-result-size 30000
   :groups (eval)
   :check-permission-async #'mevedel-tool-exec-permission-check-bash-async
   :get-pattern (lambda (input) (plist-get input :command))
   :renderer #'mevedel-tool-exec--render-bash)

  (mevedel-define-tool
   :name "WriteStdin"
   :description "Poll unread output or send input to a yielded Bash execution."
   :prompt-file "prompts/tools/writestdin.md"
   :handler #'mevedel-tool-exec--write-stdin
   :args ((execution_id string :required
                        "Opaque execution ID returned by Bash.")
          (chars string :optional
                 "Input to send. Omit or use an empty string to poll. Ordinary input requires a PTY; a single Ctrl-C character interrupts either mode.")
          (yield_time_ms integer :optional
                         "Wait before returning: polls default to 5000ms and clamp positive shorter waits to 5000ms (maximum 300000); input defaults to 250ms (250-30000)."
                         :minimum 250 :maximum 300000))
   :async-p t
   :max-result-size 30000
   :groups (eval)
   :check-permission
   (lambda (_tool args)
     (let ((chars (plist-get args :chars)))
       (if (or (null chars) (equal chars "") (equal chars "\C-c"))
           '(:outcome allow :raw-outcome allow :via execution-control)
         'allow)))
   :renderer #'mevedel-tool-exec--render-bash)

  (mevedel-define-tool
   :name "ListExecutions"
   :description "List yielded Bash executions owned by this agent."
   :prompt-file "prompts/tools/listexecutions.md"
   :handler #'mevedel-tool-exec--list-executions
   :args ()
   :read-only-p t
   :groups (eval))

  (mevedel-define-tool
   :name "StopExecution"
   :description "Stop one yielded Bash execution owned by this agent."
   :prompt-file "prompts/tools/stopexecution.md"
   :handler #'mevedel-tool-exec--stop-execution
   :args ((execution_id string :required
                        "Opaque execution ID returned by Bash."))
   :async-p t
   :max-result-size 30000
   :groups (eval)
   :check-permission
   (lambda (_tool _args)
     '(:outcome allow :raw-outcome allow :via execution-control))
   :renderer #'mevedel-tool-exec--render-bash)

  (mevedel-define-tool
   :name "Eval"
   :description "Evaluate an Elisp expression and return the result."
   :prompt-file "prompts/tools/eval.md"
   :handler #'mevedel-tool-exec--eval
   :args ((expression string :required "A single elisp sexp to evaluate with default-directory set to the session working directory.")
          (mode string :optional "Execution mode: live (default) evaluates in the current Emacs; batch evaluates in a child emacs --batch process."
                :enum ["live" "batch"])
          (preserve_ui boolean :optional "In live mode, restore the current window configuration after evaluation. Defaults to true.")
          (sandbox_permissions string :optional
                               "Batch child-execution authority: use_default, with_additional_permissions, or require_escalated for a complete confinement bypass."
                               :enum ["use_default"
                                      "with_additional_permissions"
                                      "require_escalated"])
          (additional_permissions object :optional
                                  "Capabilities requested in addition to default batch confinement."
                                  :properties
                                  (:network
                                   (:type boolean
                                          :description "Allow network access for this batch invocation.")
                                   :file_system
                                   (:type object
                                          :description "Exact filesystem paths to reopen inside confinement."
                                          :properties
                                          (:read
                                           (:type array
                                                  :items (:type string)
                                                  :description "Absolute paths requiring read access.")
                                           :write
                                           (:type array
                                                  :items (:type string)
                                                  :description "Absolute paths requiring write access.")))))
          (justification string :optional
                         "Concise user-facing reason for a non-default batch permission request."))
   :async-p t
   :max-result-size 30000
   :groups (eval)
   :check-permission-async #'mevedel-tool-exec-permission-check-eval-async
   :renderer '((success . mevedel-tool-exec--render-eval))))

(provide 'mevedel-tool-exec-registration)
;;; mevedel-tool-exec-registration.el ends here
