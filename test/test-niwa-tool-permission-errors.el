;;; test-niwa-tool-permission-errors.el --- Permission failure integration tests -*- lexical-binding: t -*-

;;; Commentary:

;; Exercise execution-policy failures through the production tool pipeline.

;;; Code:

(require 'cl-lib)
(require 'niwa-bash-analysis)
(require 'niwa-execution)
(require 'niwa-execution-target)
(require 'niwa-hooks)
(require 'niwa-pipeline)
(require 'niwa-permissions)
(require 'niwa-permission-queue)
(require 'niwa-tool-exec-permission)
(require 'niwa-tool-exec-registration)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(niwa-deftest niwa-pipeline-run-tool-outcome/permission-errors ()
  ,test
  (test)
  :doc "an analyzer fault is a pipeline error without permission hooks or launch"
  (progn
    (niwa-tool-exec--register)
    (let* ((root (file-name-as-directory (make-temp-file "permission-error-" t)))
           (workspace (niwa-workspace--create :type 'test :id root :root root))
           (session (niwa-session--create :workspace workspace
                                             :working-directory root
                                             :permission-mode 'ask))
           (tool (niwa-tool-get "Bash"))
           (count 0) (analyzed 0) events outcome)
      (unwind-protect
          (with-temp-buffer
            (setq-local default-directory root niwa--session session)
            (cl-letf (((symbol-function 'niwa-bash-analysis-analyze)
                       (lambda (_source)
                         (cl-incf analyzed)
                         (error "Injected analyzer fault")))
                      ((symbol-function 'niwa-hooks-run-event)
                       (lambda (event _payload callback &rest _)
                         (push event events)
                         (funcall callback nil))))
              (niwa-pipeline-run-tool-outcome
               tool (lambda (value) (cl-incf count) (setq outcome value))
               '(:command "printf must-not-launch > launched" :description "Check failure isolation"))
              (should (> analyzed 0))
              (should (= 1 count))
              (should (eq 'error (plist-get outcome :status)))
              (should (eq 'pipeline-error (plist-get outcome :reason)))
              (should (string-match-p "Injected analyzer fault" (plist-get outcome :result)))
              (should-not (memq 'PermissionRequest events))
              (should-not (memq 'PermissionDenied events))
              (should-not (niwa-permission-queue--get session))
              (should-not (niwa-execution-list-user session))
              (should (equal '("." "..") (directory-files root)))))
        (delete-directory root t)))))

(niwa-deftest niwa-pipeline-run-tool-outcome/deferred-permission-errors ()
  ,test
  (test)
  :doc "deferred errors settle once; typed sandbox refusals retain denial semantics"
  (progn
    (niwa-tool-exec--register)
    (dolist (condition '(error niwa-sandbox-policy-error))
      (let* ((root (file-name-as-directory (make-temp-file "permission-deferred-" t)))
             (workspace (niwa-workspace--create :type 'test :id root :root root))
             (session (niwa-session--create :workspace workspace
                                               :working-directory root
                                               :permission-mode 'ask))
             (analyze (symbol-function 'niwa-bash-analysis-analyze))
             (validate (symbol-function 'niwa-permission-queue-validate-approval))
             (count 0) resume broken events outcome)
        (unwind-protect
            (with-temp-buffer
              (setq-local default-directory root niwa--session session)
              (cl-letf (((symbol-function 'niwa-bash-analysis-analyze)
                         (lambda (source)
                           (if (and broken (eq condition 'error))
                               (error "Deferred analyzer fault")
                             (funcall analyze source))))
                        ((symbol-function 'niwa-permission-queue-validate-approval)
                         (lambda (entry answer)
                           (if (and broken (not (eq condition 'error)))
                               (signal condition '("Deferred analyzer fault"))
                             (funcall validate entry answer))))
                        ((symbol-function 'niwa-hooks-run-event)
                         (lambda (event _payload callback &rest _)
                           (push event events)
                           (if (eq event 'PermissionRequest) (setq resume callback)
                             (funcall callback nil)))))
                (niwa-pipeline-run-tool-outcome
                 (niwa-tool-get "Bash")
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
                (should-not (niwa-permission-queue--get session))
                (should-not (niwa-execution-list-user session))
                (should (equal '("." "..") (directory-files root)))))
          (delete-directory root t))))))

(niwa-deftest niwa-pipeline-run-tool-outcome/sync-permission-errors ()
  ,test
  (test)
  :doc "a synchronous checker fault cannot fall through to Full Access allowance"
  (let* ((tool (niwa-tool--create
                :name "PolicyFault" :read-only-p t
                :check-permission (lambda (&rest _) (error "Sync checker fault"))
                :handler (lambda (_) (ert-fail "Faulty checker authorized execution"))))
         (session (niwa-session--create :working-directory default-directory
                                           :permission-mode 'full-auto))
         (count 0) outcome)
    (with-temp-buffer
      (setq-local niwa--session session)
      (niwa-pipeline-run-tool-outcome
       tool (lambda (value) (cl-incf count) (setq outcome value)) nil)
      (should (= 1 count))
      (should (eq 'pipeline-error (plist-get outcome :reason)))
      (should (string-match-p "Sync checker fault" (plist-get outcome :result))))))

(niwa-deftest niwa-pipeline-run-tool-outcome/permission-refusals ()
  ,test
  (test)
  :doc "explicit permission and invalid sandbox requests remain denials without launch"
  (progn
    (niwa-tool-exec--register)
    (dolist (sandbox-p '(nil t))
      (let* ((root (file-name-as-directory (make-temp-file "permission-denial-" t)))
             (workspace (niwa-workspace--create :type 'test :id root :root root))
             (session (niwa-session--create
                       :workspace workspace :working-directory root :permission-mode 'ask
                       :permission-rules (unless sandbox-p '(("Bash" :action deny)))))
             (count 0) events outcome)
        (unwind-protect
            (with-temp-buffer
              (setq-local default-directory root niwa--session session)
              (cl-letf (((symbol-function 'niwa-hooks-run-event)
                         (lambda (event _payload callback &rest _)
                           (push event events)
                           (funcall callback nil))))
                (niwa-pipeline-run-tool-outcome
                 (niwa-tool-get "Bash")
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
                (should-not (niwa-permission-queue--get session))
                (should-not (niwa-execution-list-user session))
                (should (equal '("." "..") (directory-files root)))))
          (delete-directory root t))))))

(niwa-deftest niwa-check-permission-async-with-metadata/callback-errors ()
  ,test
  (test)
  :doc "a throwing terminal callback cannot escape or receive a second decision"
  (let ((tool (niwa-tool--create
               :name "CallbackFault"
               :check-permission-async
               (lambda (&rest _) (error "Checker failed before callback"))))
        (count 0) diagnostic)
    (niwa-test--with-captured-diagnostics diagnostic
      (niwa-check-permission-async-with-metadata
       "CallbackFault"
       (lambda (_decision) (cl-incf count) (error "Terminal callback fault"))
       :tool tool :mode 'ask))
    (should (= count 1))
    (should (string-match-p "Terminal callback fault" diagnostic))))

(niwa-deftest niwa-pipeline-run-tool-outcome/request-path-refusals ()
  ,test
  (test)
  :doc "requested path refusals are denials; target expansion faults remain errors"
  (progn
    (niwa-tool-exec--register)
    (dolist (case '(("/ssh:foreign.invalid:/tmp/requested" "another execution target")
                    ("/:/tmp/requested" "Quoted paths are not allowed")
                    ("/tmp/requested" "Injected target expansion failure" t)))
      (let* ((root (file-name-as-directory (make-temp-file "permission-path-" t)))
             (workspace (niwa-workspace--create :type 'test :id root :root root))
             (session (niwa-session--create
                       :workspace workspace :working-directory root :permission-mode 'ask
                       :execution-target (niwa-execution-target-create root)))
             (expand (symbol-function 'niwa-execution-target-expand-path))
             (count 0) events outcome)
        (unwind-protect
            (with-temp-buffer
              (setq-local default-directory root niwa--session session)
              (cl-letf (((symbol-function 'niwa-execution-target-expand-path)
                         (lambda (&rest args)
                           (if (nth 2 case)
                               (signal 'niwa-execution-target-error
                                       '("Injected target expansion failure"))
                             (apply expand args))))
                        ((symbol-function 'niwa-hooks-run-event)
                         (lambda (event _payload callback &rest _)
                           (push event events)
                           (funcall callback nil))))
                (niwa-pipeline-run-tool-outcome
                 (niwa-tool-get "Bash")
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
                (should-not (niwa-permission-queue--get session))
                (should-not (niwa-execution-list-user session))
                (should (equal '("." "..") (directory-files root)))))
          (delete-directory root t))))))

(provide 'test-niwa-tool-permission-errors)
;;; test-niwa-tool-permission-errors.el ends here
