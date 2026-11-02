;;; test-mevedel-tool-permission-errors.el --- Permission failure integration tests -*- lexical-binding: t -*-

;;; Commentary:

;; Exercise execution-policy failures through the production tool pipeline.

;;; Code:

(require 'cl-lib)
(require 'mevedel-bash-analysis)
(require 'mevedel-execution)
(require 'mevedel-execution-target)
(require 'mevedel-hooks)
(require 'mevedel-pipeline)
(require 'mevedel-permissions)
(require 'mevedel-permission-queue)
(require 'mevedel-tool-exec-permission)
(require 'mevedel-tool-exec-registration)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(mevedel-deftest mevedel-pipeline-run-tool-outcome/permission-errors ()
  ,test
  (test)
  :doc "an analyzer fault is a pipeline error without permission hooks or launch"
  (progn
    (mevedel-tool-exec--register)
    (let* ((root (file-name-as-directory (make-temp-file "permission-error-" t)))
           (workspace (mevedel-workspace--create :type 'test :id root :root root))
           (session (mevedel-session--create :workspace workspace
                                             :working-directory root
                                             :permission-mode 'ask))
           (tool (mevedel-tool-get "Bash"))
           (count 0) (analyzed 0) events outcome)
      (unwind-protect
          (with-temp-buffer
            (setq-local default-directory root mevedel--session session)
            (cl-letf (((symbol-function 'mevedel-bash-analysis-analyze)
                       (lambda (_source)
                         (cl-incf analyzed)
                         (error "Injected analyzer fault")))
                      ((symbol-function 'mevedel-hooks-run-event)
                       (lambda (event _payload callback &rest _)
                         (push event events)
                         (funcall callback nil))))
              (mevedel-pipeline-run-tool-outcome
               tool (lambda (value) (cl-incf count) (setq outcome value))
               '(:command "printf must-not-launch > launched" :description "Check failure isolation"))
              (should (> analyzed 0))
              (should (= 1 count))
              (should (eq 'error (plist-get outcome :status)))
              (should (eq 'pipeline-error (plist-get outcome :reason)))
              (should (string-match-p "Injected analyzer fault" (plist-get outcome :result)))
              (should-not (memq 'PermissionRequest events))
              (should-not (memq 'PermissionDenied events))
              (should-not (mevedel-permission-queue--get session))
              (should-not (mevedel-execution-list-user session))
              (should (equal '("." "..") (directory-files root)))))
        (delete-directory root t)))))

(mevedel-deftest mevedel-pipeline-run-tool-outcome/deferred-permission-errors ()
  ,test
  (test)
  :doc "deferred errors settle once; typed sandbox refusals retain denial semantics"
  (progn
    (mevedel-tool-exec--register)
    (dolist (condition '(error mevedel-sandbox-policy-error))
      (let* ((root (file-name-as-directory (make-temp-file "permission-deferred-" t)))
             (workspace (mevedel-workspace--create :type 'test :id root :root root))
             (session (mevedel-session--create :workspace workspace
                                               :working-directory root
                                               :permission-mode 'ask))
             (analyze (symbol-function 'mevedel-bash-analysis-analyze))
             (validate (symbol-function 'mevedel-permission-queue-validate-approval))
             (count 0) resume broken events outcome)
        (unwind-protect
            (with-temp-buffer
              (setq-local default-directory root mevedel--session session)
              (cl-letf (((symbol-function 'mevedel-bash-analysis-analyze)
                         (lambda (source)
                           (if (and broken (eq condition 'error))
                               (error "Deferred analyzer fault")
                             (funcall analyze source))))
                        ((symbol-function 'mevedel-permission-queue-validate-approval)
                         (lambda (entry answer)
                           (if (and broken (not (eq condition 'error)))
                               (signal condition '("Deferred analyzer fault"))
                             (funcall validate entry answer))))
                        ((symbol-function 'mevedel-hooks-run-event)
                         (lambda (event _payload callback &rest _)
                           (push event events)
                           (if (eq event 'PermissionRequest) (setq resume callback)
                             (funcall callback nil)))))
                (mevedel-pipeline-run-tool-outcome
                 (mevedel-tool-get "Bash")
                 (lambda (value) (cl-incf count) (setq outcome value))
                 '(:command "printf must-not-launch > launched" :description "Check deferred failure"))
                (unless (functionp resume) (ert-fail (list :no-approval outcome events)))
                (should (= 0 count))
                (setq broken t)
                (funcall resume '(:permission-decision allow))
                (should (= 1 count))
                (should (eq (if (eq condition 'error) 'pipeline-error 'permission-denied)
                            (plist-get outcome :reason)))
                (should (string-match-p "Deferred analyzer fault" (plist-get outcome :result)))
                (funcall resume '(:permission-decision allow))
                (should (= 1 count))
                (should (= (if (eq condition 'error) 0 1)
                           (cl-count 'PermissionDenied events)))
                (should-not (mevedel-permission-queue--get session))
                (should-not (mevedel-execution-list-user session))
                (should (equal '("." "..") (directory-files root)))))
          (delete-directory root t))))))

(mevedel-deftest mevedel-pipeline-run-tool-outcome/sync-permission-errors ()
  ,test
  (test)
  :doc "a synchronous checker fault cannot fall through to Full Access allowance"
  (let* ((tool (mevedel-tool--create
                :name "PolicyFault" :read-only-p t
                :check-permission (lambda (&rest _) (error "Sync checker fault"))
                :handler (lambda (_) (ert-fail "Faulty checker authorized execution"))))
         (session (mevedel-session--create :working-directory default-directory
                                           :permission-mode 'full-auto))
         (count 0) outcome)
    (with-temp-buffer
      (setq-local mevedel--session session)
      (mevedel-pipeline-run-tool-outcome
       tool (lambda (value) (cl-incf count) (setq outcome value)) nil)
      (should (= 1 count))
      (should (eq 'pipeline-error (plist-get outcome :reason)))
      (should (string-match-p "Sync checker fault" (plist-get outcome :result))))))

(mevedel-deftest mevedel-pipeline-run-tool-outcome/permission-refusals ()
  ,test
  (test)
  :doc "explicit permission and invalid sandbox requests remain denials without launch"
  (progn
    (mevedel-tool-exec--register)
    (dolist (sandbox-p '(nil t))
      (let* ((root (file-name-as-directory (make-temp-file "permission-denial-" t)))
             (workspace (mevedel-workspace--create :type 'test :id root :root root))
             (session (mevedel-session--create
                       :workspace workspace :working-directory root :permission-mode 'ask
                       :permission-rules (unless sandbox-p '(("Bash" :action deny)))))
             (count 0) events outcome)
        (unwind-protect
            (with-temp-buffer
              (setq-local default-directory root mevedel--session session)
              (cl-letf (((symbol-function 'mevedel-hooks-run-event)
                         (lambda (event _payload callback &rest _)
                           (push event events)
                           (funcall callback nil))))
                (mevedel-pipeline-run-tool-outcome
                 (mevedel-tool-get "Bash")
                 (lambda (value) (cl-incf count) (setq outcome value))
                 (append '(:command "printf must-not-launch > launched"
                                    :description "Check actual refusal")
                         (when sandbox-p
                           '(:sandbox_permissions "with_additional_permissions"
                                                  :additional_permissions (:network t)))))
                (should (= 1 count))
                (should (eq 'error (plist-get outcome :status)))
                (should (eq 'permission-denied (plist-get outcome :reason)))
                (when sandbox-p
                  (should (string-match-p "require a justification"
                                          (plist-get outcome :result))))
                (should (= 1 (cl-count 'PermissionDenied events)))
                (should-not (memq 'PermissionRequest events))
                (should-not (mevedel-permission-queue--get session))
                (should-not (mevedel-execution-list-user session))
                (should (equal '("." "..") (directory-files root)))))
          (delete-directory root t))))))

(mevedel-deftest mevedel-check-permission-async-with-metadata/callback-errors ()
  ,test
  (test)
  :doc "a throwing terminal callback cannot escape or receive a second decision"
  (let ((tool (mevedel-tool--create
               :name "CallbackFault"
               :check-permission-async
               (lambda (&rest _) (error "Checker failed before callback"))))
        (count 0) diagnostic)
    (mevedel-test--with-captured-diagnostics diagnostic
      (mevedel-check-permission-async-with-metadata
       "CallbackFault"
       (lambda (_decision) (cl-incf count) (error "Terminal callback fault"))
       :tool tool :mode 'ask))
    (should (= count 1))
    (should (string-match-p "Terminal callback fault" diagnostic))))

(mevedel-deftest mevedel-pipeline-run-tool-outcome/request-path-refusals ()
  ,test
  (test)
  :doc "requested path refusals are denials; target expansion faults remain errors"
  (progn
    (mevedel-tool-exec--register)
    (dolist (case '(("/ssh:foreign.invalid:/tmp/requested" "another execution target")
                    ("/:/tmp/requested" "Quoted paths are not allowed")
                    ("/tmp/requested" "Injected target expansion failure" t)))
      (let* ((root (file-name-as-directory (make-temp-file "permission-path-" t)))
             (workspace (mevedel-workspace--create :type 'test :id root :root root))
             (session (mevedel-session--create
                       :workspace workspace :working-directory root :permission-mode 'ask
                       :execution-target (mevedel-execution-target-create root)))
             (expand (symbol-function 'mevedel-execution-target-expand-path))
             (count 0) events outcome)
        (unwind-protect
            (with-temp-buffer
              (setq-local default-directory root mevedel--session session)
              (cl-letf (((symbol-function 'mevedel-execution-target-expand-path)
                         (lambda (&rest args)
                           (if (nth 2 case)
                               (signal 'mevedel-execution-target-error
                                       '("Injected target expansion failure"))
                             (apply expand args))))
                        ((symbol-function 'mevedel-hooks-run-event)
                         (lambda (event _payload callback &rest _)
                           (push event events)
                           (funcall callback nil))))
                (mevedel-pipeline-run-tool-outcome
                 (mevedel-tool-get "Bash")
                 (lambda (value) (cl-incf count) (setq outcome value))
                 (list :command "printf must-not-launch > launched"
                       :description "Check requested path refusal"
                       :sandbox_permissions "with_additional_permissions"
                       :justification "Inspect the requested path"
                       :additional_permissions
                       (list :file_system (list :read (vector (car case))))))
                (should (= 1 count))
                (should (eq 'error (plist-get outcome :status)))
                (should (eq (if (nth 2 case) 'pipeline-error 'permission-denied)
                            (plist-get outcome :reason)))
                (should (string-match-p (cadr case) (plist-get outcome :result)))
                (should (= (if (nth 2 case) 0 1) (cl-count 'PermissionDenied events)))
                (should-not (memq 'PermissionRequest events))
                (should-not (mevedel-permission-queue--get session))
                (should-not (mevedel-execution-list-user session))
                (should (equal '("." "..") (directory-files root)))))
          (delete-directory root t))))))

(provide 'test-mevedel-tool-permission-errors)
;;; test-mevedel-tool-permission-errors.el ends here
