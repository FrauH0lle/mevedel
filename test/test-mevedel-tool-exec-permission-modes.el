;;; test-mevedel-tool-exec-permission-modes.el --- Mode authority -*- lexical-binding: t -*-

;;; Commentary:

;; Exercise mode authority through the asynchronous Bash and Eval entry points.

;;; Code:

(require 'mevedel-tool-exec-permission)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(mevedel-deftest mevedel-tool-exec-permission-check-bash-async-modes ()
  ,test
  (test)
  :doc "confined edits accepts arbitrary syntax without admitting a card"
  (let ((mevedel-permission-rules nil)
        (mevedel-permission-reviewer 'user))
    (dolist (command '("unknown-test-runner --check"
                        "git status | cat"
                        "for file in *.el; do wc -l \"$file\"; done"
                        "make test > test.log 2>&1"
                        "cat /usr/share/emacs/manual"))
      (let (result prompts)
        (mevedel-tool-exec-permission-check-bash-async
         nil
         (list :command command
               :permission-context
               (list :mode 'edits
                     :permission-request
                     (lambda (&rest _) (setq prompts t))))
         (lambda (value) (setq result value)))
        (should-not prompts)
        (should (eq 'allow result)))))
  :doc "full-auto bypasses asks and approval review for every authority level"
  (let ((mevedel-permission-rules '(("Bash" :action ask)))
        (mevedel-permission-reviewer 'auto))
    (dolist (extra '(nil
                     (:sandbox_permissions "require_escalated"
                      :justification "Requested full access")
                     (:sandbox_permissions "with_additional_permissions"
                      :justification "Requested full access"
                      :additional_permissions
                      (:network t :file_system
                       (:write ["/outside/cache"])))))
      (let (result prompts)
        (mevedel-tool-exec-permission-check-bash-async
         nil
         (append (list :command "cat ~/.ssh/key"
                       :permission-context
                       (list :mode 'full-auto
                             :permission-request
                             (lambda (&rest _) (setq prompts t))))
                 extra)
         (lambda (value) (setq result value)))
        (should-not prompts)
        (should (eq 'allow result)))))
  :doc "full-auto retains explicit operation, network and escalation denies"
  (dolist (case
           '(((("Bash" :pattern "make *" :action deny)) nil)
             ((("Bash" :network t :action deny))
              (:sandbox_permissions "with_additional_permissions"
               :additional_permissions (:network t) :justification "Test"))
             ((("Bash" :sandbox-permissions require-escalated :action deny))
              (:sandbox_permissions "require_escalated" :justification "Test"))))
    (let ((mevedel-permission-rules (car case))
          result prompts)
      (mevedel-tool-exec-permission-check-bash-async
       nil
       (append (list :command "make test"
                     :permission-context
                     (list :mode 'full-auto :permission-request
                           (lambda (&rest _) (setq prompts t))))
               (cadr case))
       (lambda (value) (setq result value)))
      (should-not prompts)
      (should (eq 'deny result)))))

(mevedel-deftest mevedel-tool-exec-permission-check-eval-async-modes ()
  ,test
  (test)
  :doc "edits automates batch Eval but live Eval still requests authority"
  (let ((mevedel-permission-rules nil))
    (dolist (eval-mode '("batch" "live"))
      (let (result prompts)
        (mevedel-tool-exec-permission-check-eval-async
         nil
         (list :expression "(+ 1 2)" :mode eval-mode
               :permission-context
               (list :mode 'edits
                     :permission-request
                     (lambda (&rest _) (setq prompts t))))
         (lambda (value) (setq result value)))
        (if (equal eval-mode "batch")
            (progn (should-not prompts) (should (eq 'allow result)))
          (should prompts)
          (should-not result))))))

(provide 'test-mevedel-tool-exec-permission-modes)
;;; test-mevedel-tool-exec-permission-modes.el ends here
