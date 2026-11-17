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

(mevedel-deftest mevedel-tool-exec-permission-eval-mode ()
  ,test
  (test)
  :doc "accepts fresh and rejects the former mode name without an alias"
  (should (eq 'fresh (mevedel-tool-exec-permission-eval-mode '(:mode "fresh"))))
  (let ((err (should-error
              (mevedel-tool-exec-permission-eval-mode '(:mode "batch")))))
    (should (string-match-p "Unknown Eval mode: batch" (error-message-string err))))
  :doc "sandbox validates reset and accepts yielding like fresh"
  (progn
    (dolist (reset '(t nil :json-false))
      (should (eq 'sandbox (mevedel-tool-exec-permission-eval-mode
                            (list :mode "sandbox" :reset reset)))))
    (dolist (mode '("live" "fresh" "sandbox"))
      (should (eq (intern mode) (mevedel-tool-exec-permission-eval-mode
                                (list :mode mode :reset :json-false))))
      (should-error (mevedel-tool-exec-permission-eval-mode (list :mode mode :reset "true"))
                    :type 'mevedel-execution-policy-error))
    (dolist (args '((:mode "live" :reset t) (:mode "fresh" :reset t)))
      (should-error (mevedel-tool-exec-permission-eval-mode args)
                    :type 'mevedel-execution-policy-error))
    (dolist (mode '("fresh" "sandbox"))
      (should (eq (intern mode) (mevedel-tool-exec-permission-eval-mode
                                (list :mode mode :yield_time_ms 1000)))))))

(mevedel-deftest mevedel-tool-exec-permission-check-eval-async-modes ()
  ,test
  (test)
  :doc "edits automates fresh Eval but live Eval still requests authority"
  (let ((mevedel-permission-rules nil))
    (dolist (eval-mode '("fresh" "live"))
      (let (result prompts)
        (mevedel-tool-exec-permission-check-eval-async
         nil
         (list :expression "(+ 1 2)" :mode eval-mode
               :permission-context
               (list :mode 'edits
                     :permission-request
                     (lambda (&rest _) (setq prompts t))))
         (lambda (value) (setq result value)))
        (if (equal eval-mode "fresh")
            (progn (should-not prompts) (should (eq 'allow result)))
          (should prompts)
          (should-not result)))))
  :doc "sandbox is automatic in Ask Edits and Full Access; explicit deny remains"
  (dolist (mode '(ask edits full-auto))
    (dolist (deny '(nil t))
      (let ((mevedel-permission-rules (and deny '(("Eval" :action deny)))) result)
        (mevedel-tool-exec-permission-check-eval-async
         nil (list :expression "(+ 1 2)" :mode "sandbox"
                   :permission-decision-metadata t :permission-context (list :mode mode))
         (lambda (value) (setq result value)))
        (should (eq (if deny 'deny 'allow) (plist-get result :outcome)))
        (should (eq 'sandbox (plist-get result :eval-mode))))))
  :doc "remote sandbox refuses before authorization"
  (let* ((session (mevedel-session--create
                   :execution-target (mevedel-execution-target-create "/ssh:invalid:/project/")
                   :working-directory temporary-file-directory)) result)
    (mevedel-tool-exec-permission-check-eval-async
     nil (list :expression "t" :mode "sandbox" :permission-context (list :session session))
     (lambda (value) (setq result value)))
    (should (eq 'deny (car result)))
    (should (string-match-p "Sandbox Eval is unavailable for remote sessions" (cdr result)))))

(mevedel-deftest mevedel-tool-exec-permission-effective-sandbox-request/sandbox ()
  ,test
  (test)
  :doc "sandbox ignores remembered and invocation grants and refuses raw additions"
  (let ((mevedel-permission-rules '(("Eval" :expression "t" :network t :action allow)))
        (mevedel-tool-exec-permission-approved-resources '((:path "/outside" :access write))))
    (should (equal '(:level use-default :additional-permissions nil :eval-mode sandbox)
                   (mevedel-tool-exec-permission-effective-sandbox-request
                    '(:mode "sandbox" :additional_permissions nil :reset nil :yield_time_ms nil)
                    "Eval" "t" 'sandbox)))
    (should (eq 'fresh (mevedel-tool-exec-permission-eval-mode
                        '(:mode "fresh" :reset nil :yield_time_ms nil))))
    (dolist (extra '((:sandbox_permissions "require_escalated")
                     (:sandbox_permissions "with_additional_permissions")
                     (:additional_permissions (:file_system (:write ["relative"])))
                     (:sandbox_permissions "use_default" :additional_permissions (:network :json-false))))
      (should (string-match-p
               "use fresh Eval"
               (error-message-string
                (should-error (mevedel-tool-exec-permission-effective-sandbox-request
                               (append '(:mode "sandbox") extra) "Eval" "t" 'sandbox)
                              :type 'mevedel-execution-policy-error)))))))

(provide 'test-mevedel-tool-exec-permission-modes)
;;; test-mevedel-tool-exec-permission-modes.el ends here
