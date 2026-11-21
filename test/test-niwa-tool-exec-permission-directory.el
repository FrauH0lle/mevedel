;;; test-niwa-tool-exec-permission-directory.el --- Execution cwd agreement -*- lexical-binding: t -*-

;;; Commentary:

;; Execution and permission adapters share the conversation directory while
;; resource roots remain independent policy facts.

;;; Code:

(require 'cl-lib)
(require 'niwa-tool-exec-permission)
(require 'niwa-bash-policy)
(require 'niwa-execution)
(require 'niwa-permission-queue)
(require 'niwa-permission-queue-test-support
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "niwa-permission-queue-test-support"))
(require 'niwa-pipeline)
(require 'niwa-structs)
(require 'niwa-tools)
(require 'niwa-workspace)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(niwa-deftest niwa-tool-exec-permission-default-directory ()
  ,test
  (test)
  :doc "projected agent cwd wins session cwd and all ordinary fallbacks remain"
  (let* ((root (file-name-as-directory (make-temp-file "niwa-exec-root-" t)))
         (child (file-name-concat root "child/"))
         (workspace (niwa-workspace--create :root root))
         (session (niwa-session--create :workspace workspace
                                           :working-directory root)))
    (unwind-protect
        (with-temp-buffer
          (make-directory child)
          (setq-local niwa--session session
                      niwa--conversation-working-directory child)
          (should (equal child (niwa-tool-exec-permission-default-directory)))
          (setq niwa--conversation-working-directory nil)
          (should (equal root (niwa-tool-exec-permission-default-directory)))
          (setf (niwa-session-working-directory session) nil)
          (should (equal root (niwa-tool-exec-permission-default-directory)))
          (setq niwa--session nil)
          (should (equal default-directory
                         (niwa-tool-exec-permission-default-directory))))
      (delete-directory root t))))

(niwa-deftest niwa-tool-exec-permission-current-context/directory ()
  ,test
  (test)
  :doc "Bash and Eval capture execution cwd without adding it to resource roots"
  (let* ((root (file-name-as-directory (make-temp-file "niwa-exec-owner-" t)))
         (child (file-name-as-directory (make-temp-file "niwa-exec-child-" t)))
         (session (niwa-session--create :working-directory root)))
    (unwind-protect
        (with-temp-buffer
          (setq-local niwa--session session
                      niwa--conversation-working-directory child)
          (dolist (tool '("Bash" "Eval"))
            (let* ((context (niwa-tool-exec-permission-current-context
                             tool nil session))
                   (input (niwa-tool-exec-permission--capture-permission-origin
                           (list :permission-context context))))
              (should (equal child (plist-get context :execution-directory)))
              (should (equal child (plist-get (plist-get input :permission-context)
                                              :execution-directory)))
              (should-not (member child (plist-get context :allowed-roots)))
              (should-not (plist-get context :resource-grants))
              (should (equal root (niwa-session-working-directory session))))))
      (delete-directory child t)
      (delete-directory root t))))

(niwa-deftest niwa-tool-exec-permission--sandbox-request/directory ()
  ,test
  (test)
  :doc "Bash and fresh Eval normalize capabilities at captured cwd before live cwd"
  (let* ((root (file-name-as-directory (make-temp-file "niwa-exec-session-" t)))
         (child (file-name-concat root "child/"))
         (captured (file-name-concat root "captured/"))
         (session (niwa-session--create :working-directory root))
         (normalize (symbol-function
                     'niwa-tool-exec-permission--normalize-additional-permissions))
         seen)
    (unwind-protect
        (with-temp-buffer
          (make-directory child)
          (make-directory captured)
          (setq-local niwa--session session
                      niwa--conversation-working-directory child)
          (cl-letf (((symbol-function
                     'niwa-tool-exec-permission--normalize-additional-permissions)
                    (lambda (additional target directory)
                      (push directory seen)
                      (funcall normalize additional target directory))))
            (dolist (tool '(bash eval))
              (dolist (directory (list child captured))
                (let* ((context (list :session session))
                       (_ (when (equal directory captured)
                            (setq context (plist-put context :execution-directory
                                                     captured))))
                       (request
                        (niwa-tool-exec-permission--sandbox-request
                         `(:sandbox_permissions "with_additional_permissions"
                           :additional_permissions (:file_system (:read [,child]))
                           :justification "Inspect the selected directory")
                         tool 'fresh context)))
                  (should (equal directory (pop seen)))
                  (should (equal (list (list :path child :access 'read))
                                 (plist-get (plist-get request :additional-permissions)
                                            :file-system))))))))
      (delete-directory root t))))

(niwa-deftest niwa-tool-exec-permission--decide/directory ()
  ,test
  (test)
  :doc "a command allow checks relative resources at child cwd, not session cwd"
  (let* ((root (file-name-as-directory (make-temp-file "niwa-exec-root-" t)))
         (child (file-name-as-directory (make-temp-file "niwa-exec-child-" t)))
         (session (niwa-session--create :working-directory root))
         (niwa-permission-rules nil)
         (context
          (plist-put
           (niwa-permission--invocation-context
            :tool-name "Bash" :session session :mode 'ask :allowed-roots (list child)
            :session-rules '(("Bash" :pattern "cat *" :action allow)))
           :execution-directory child)))
    (unwind-protect
        (progn
          (should
           (eq 'allow
               (plist-get (niwa-tool-exec-permission--decide
                           "Bash" "cat ./marker" '(:level use-default) context)
                          :outcome)))
          (setq context (plist-put context :allowed-roots (list root)))
          (let ((decision (niwa-tool-exec-permission--decide
                           "Bash" "cat ./marker" '(:level use-default) context)))
            (should (eq 'ask (plist-get decision :outcome)))
            (should (eq 'resources (plist-get decision :needs)))
            (should (equal (list (file-name-concat child "marker"))
                           (plist-get decision :paths)))))
      (delete-directory child t)
      (delete-directory root t))))

(niwa-deftest niwa-tool-exec-permission-current-context/registered-bash
  (:quiet t :before-each (niwa-tools-register))
  ,test
  (test)
  :doc "registered Bash command allow admits child resources at the initial permission step"
  (let* ((root (file-name-as-directory (make-temp-file "niwa-exec-parent-" t)))
         (child (file-name-as-directory (make-temp-file "niwa-exec-policy-" t)))
         (workspace (niwa-workspace--create :type 'file :id child :root child))
         (session (niwa-session-create "cwd" workspace root))
         (niwa-permission-rules nil)
         (niwa-permission-log-enabled t)
         done result)
    (unwind-protect
        (with-temp-buffer
          ;; Only the child's directory is an automatic resource root.  The
          ;; session cwd is outside it, so resolving there would prompt.
          (setq-local niwa--session session niwa--workspace workspace
                      niwa--conversation-working-directory child
                      default-directory child)
          (let ((temporary-file-directory child))
            (setf (niwa-session-permission-mode session) 'ask
                  (niwa-session-sandbox-mode session) 'off
                  (niwa-session-permission-rules session)
                  '(("Bash" :pattern "cat *" :action allow)))
            (write-region "child-admission-marker\n" nil
                          (file-name-concat child "marker") nil 'silent)
            (should-not
             (niwa-permission-rules-path-in-allowed-roots-p
              (file-name-concat root "marker")
              (niwa--all-allowed-roots (current-buffer))))
            (niwa-pipeline-run-tool
             (niwa-tool-ensure "Bash")
             (lambda (value) (setq result value done t))
             '(:command "cat ./marker" :description "Read child admission marker"))
            (with-timeout (10 (ert-fail "Bash did not settle"))
              (while (not done) (accept-process-output nil 0.05)))
            (should-not (niwa-pq-test--requested-p session))
            (should (string-prefix-p "child-admission-marker\n" result))
            (should (equal root (niwa-session-working-directory session)))))
      (niwa-execution-teardown-session session)
      (delete-directory child t)
      (delete-directory root t))))

(provide 'test-niwa-tool-exec-permission-directory)
;;; test-niwa-tool-exec-permission-directory.el ends here
