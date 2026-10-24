;;; test-mevedel-tool-exec-permission-directory.el --- Execution cwd agreement -*- lexical-binding: t -*-

;;; Commentary:

;; Execution and permission adapters share the conversation directory while
;; resource roots remain independent policy facts.

;;; Code:

(require 'cl-lib)
(require 'mevedel-tool-exec-permission)
(require 'mevedel-bash-policy)
(require 'mevedel-execution)
(require 'mevedel-permission-queue)
(require 'mevedel-pipeline)
(require 'mevedel-structs)
(require 'mevedel-tools)
(require 'mevedel-workspace)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(mevedel-deftest mevedel-tool-exec-permission-default-directory ()
  ,test
  (test)
  :doc "projected agent cwd wins session cwd and all ordinary fallbacks remain"
  (let* ((root (file-name-as-directory (make-temp-file "mevedel-exec-root-" t)))
         (child (file-name-concat root "child/"))
         (workspace (mevedel-workspace--create :root root))
         (session (mevedel-session--create :workspace workspace
                                           :working-directory root)))
    (unwind-protect
        (with-temp-buffer
          (make-directory child)
          (setq-local mevedel--session session
                      mevedel--conversation-working-directory child)
          (should (equal child (mevedel-tool-exec-permission-default-directory)))
          (setq mevedel--conversation-working-directory nil)
          (should (equal root (mevedel-tool-exec-permission-default-directory)))
          (setf (mevedel-session-working-directory session) nil)
          (should (equal root (mevedel-tool-exec-permission-default-directory)))
          (setq mevedel--session nil)
          (should (equal default-directory
                         (mevedel-tool-exec-permission-default-directory))))
      (delete-directory root t))))

(mevedel-deftest mevedel-tool-exec-permission-current-context/directory ()
  ,test
  (test)
  :doc "Bash and Eval capture execution cwd without adding it to resource roots"
  (let* ((root (file-name-as-directory (make-temp-file "mevedel-exec-owner-" t)))
         (child (file-name-as-directory (make-temp-file "mevedel-exec-child-" t)))
         (session (mevedel-session--create :working-directory root)))
    (unwind-protect
        (with-temp-buffer
          (setq-local mevedel--session session
                      mevedel--conversation-working-directory child)
          (dolist (tool '("Bash" "Eval"))
            (let* ((context (mevedel-tool-exec-permission-current-context
                             tool nil session))
                   (input (mevedel-tool-exec-permission--capture-permission-origin
                           (list :permission-context context))))
              (should (equal child (plist-get context :execution-directory)))
              (should (equal child (plist-get (plist-get input :permission-context)
                                              :execution-directory)))
              (should-not (member child (plist-get context :allowed-roots)))
              (should-not (plist-get context :resource-grants))
              (should (equal root (mevedel-session-working-directory session))))))
      (delete-directory child t)
      (delete-directory root t))))

(mevedel-deftest mevedel-tool-exec-permission--sandbox-request/directory ()
  ,test
  (test)
  :doc "Bash and batch Eval normalize capabilities at captured cwd before live cwd"
  (let* ((root (file-name-as-directory (make-temp-file "mevedel-exec-session-" t)))
         (child (file-name-concat root "child/"))
         (captured (file-name-concat root "captured/"))
         (session (mevedel-session--create :working-directory root))
         (normalize (symbol-function
                     'mevedel-tool-exec-permission--normalize-additional-permissions))
         seen)
    (unwind-protect
        (with-temp-buffer
          (make-directory child)
          (make-directory captured)
          (setq-local mevedel--session session
                      mevedel--conversation-working-directory child)
          (cl-letf (((symbol-function
                     'mevedel-tool-exec-permission--normalize-additional-permissions)
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
                        (mevedel-tool-exec-permission--sandbox-request
                         `(:sandbox_permissions "with_additional_permissions"
                           :additional_permissions (:file_system (:read [,child]))
                           :justification "Inspect the selected directory")
                         tool 'batch context)))
                  (should (equal directory (pop seen)))
                  (should (equal (list (list :path child :access 'read))
                                 (plist-get (plist-get request :additional-permissions)
                                            :file-system))))))))
      (delete-directory root t))))

(mevedel-deftest mevedel-tool-exec-permission--decide/directory ()
  ,test
  (test)
  :doc "a command allow checks relative resources at child cwd, not session cwd"
  (let* ((root (file-name-as-directory (make-temp-file "mevedel-exec-root-" t)))
         (child (file-name-as-directory (make-temp-file "mevedel-exec-child-" t)))
         (session (mevedel-session--create :working-directory root))
         (mevedel-permission-rules nil)
         (context
          (plist-put
           (mevedel-permission--invocation-context
            :tool-name "Bash" :session session :mode 'ask :allowed-roots (list child)
            :session-rules '(("Bash" :pattern "cat *" :action allow)))
           :execution-directory child)))
    (unwind-protect
        (progn
          (should
           (eq 'allow
               (plist-get (mevedel-tool-exec-permission--decide
                           "Bash" "cat ./marker" '(:level use-default) context)
                          :outcome)))
          (setq context (plist-put context :allowed-roots (list root)))
          (let ((decision (mevedel-tool-exec-permission--decide
                           "Bash" "cat ./marker" '(:level use-default) context)))
            (should (eq 'ask (plist-get decision :outcome)))
            (should (eq 'resources (plist-get decision :needs)))
            (should (equal (list (file-name-concat child "marker"))
                           (plist-get decision :paths)))))
      (delete-directory child t)
      (delete-directory root t))))

(mevedel-deftest mevedel-tool-exec-permission-current-context/registered-bash
  (:quiet t :before-each (mevedel-tools-register))
  ,test
  (test)
  :doc "registered Bash command allow admits child resources at the initial permission step"
  (let* ((root (file-name-as-directory (make-temp-file "mevedel-exec-parent-" t)))
         (child (file-name-as-directory (make-temp-file "mevedel-exec-policy-" t)))
         (workspace (mevedel-workspace--create :type 'file :id child :root child))
         (session (mevedel-session-create "cwd" workspace root))
         (mevedel-permission-rules nil)
         prompts done result)
    (unwind-protect
        (with-temp-buffer
          ;; Only the child's directory is an automatic resource root.  The
          ;; session cwd is outside it, so resolving there would prompt.
          (setq-local mevedel--session session mevedel--workspace workspace
                      mevedel--conversation-working-directory child
                      default-directory child)
          (let ((temporary-file-directory child))
            (setf (mevedel-session-permission-mode session) 'ask
                  (mevedel-session-sandbox-mode session) 'off
                  (mevedel-session-permission-rules session)
                  '(("Bash" :pattern "cat *" :action allow)))
            (write-region "child-admission-marker\n" nil
                          (file-name-concat child "marker") nil 'silent)
            (should-not
             (mevedel-permission-rules-path-in-allowed-roots-p
              (file-name-concat root "marker")
              (mevedel--all-allowed-roots (current-buffer))))
            (cl-letf (((symbol-function 'mevedel-permission--enqueue)
                       (lambda (entry &optional _session)
                         (push entry prompts)
                         (funcall (plist-get entry :callback) 'deny))))
              (mevedel-pipeline-run-tool
               (mevedel-tool-ensure "Bash")
               (lambda (value) (setq result value done t))
               '(:command "cat ./marker" :description "Read child admission marker"))
              (with-timeout (10 (ert-fail "Bash did not settle"))
                (while (not done) (accept-process-output nil 0.05))))
            (should-not prompts)
            (should (string-prefix-p "child-admission-marker\n" result))
            (should (equal root (mevedel-session-working-directory session)))))
      (mevedel-execution-teardown-session session)
      (delete-directory child t)
      (delete-directory root t))))

(provide 'test-mevedel-tool-exec-permission-directory)
;;; test-mevedel-tool-exec-permission-directory.el ends here
