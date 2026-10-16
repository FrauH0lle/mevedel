;;; test-mevedel-permissions-request-scope.el -- Scoped authority -*- lexical-binding: t -*-

;;; Commentary:
;; Actual decision and delayed queue checks across implementation identities.

;;; Code:

(require 'mevedel-agents)
(require 'mevedel-bash-policy)
(require 'mevedel-execution)
(require 'mevedel-permissions)
(require 'mevedel-permission-queue)
(require 'mevedel-reminders)
(require 'mevedel-tool-registry)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(mevedel-deftest mevedel-permission-request-scoped-decisions ()
  ,test
  (test)
  :doc "native mutation uses scoped mode without changing baseline or rules"
  (with-temp-buffer
    (let* ((session (mevedel-session--create :permission-mode 'ask))
           (tool (mevedel-tool--create :name "ScopedEdit" :groups '(edit)))
           (mevedel-permission-rules nil)
           (request (mevedel-request--create
                     :session session :permission-mode-override 'edits)))
      (setq-local mevedel--session session mevedel--current-request request)
      (should (eq 'allow (mevedel-check-permission
                         "ScopedEdit" :tool-struct tool :session session :request request)))
      (should (eq 'deny (mevedel-check-permission
                        "ScopedEdit" :tool-struct tool :session session :request request
                        :session-rules '(("ScopedEdit" :action deny)))))
      (setf (mevedel-request-plan-read-only request) t)
      (should (eq 'deny (mevedel-check-permission
                        "ScopedEdit" :tool-struct tool :session session :request request)))
      (should (eq 'ask (mevedel-session-permission-mode session)))))

  :doc "explicit Ask overrides Full Access for decisions and reminders"
  (with-temp-buffer
    (let* ((session (mevedel-session--create :permission-mode 'full-auto))
           (tool (mevedel-tool--create :name "ScopedEdit" :groups '(edit)))
           (mevedel-permission-rules nil)
           (request (mevedel-request--create
                     :session session :permission-mode-override 'ask)))
      (setq-local mevedel--session session mevedel--current-request request)
      (should (eq 'ask (mevedel-check-permission
                       "ScopedEdit" :tool-struct tool :session session :request request)))
      (should (eq 'ask (mevedel-reminders--session-mode session)))
      (should (eq 'full-auto (mevedel-session-permission-mode session)))))

  :doc "delayed queue rechecks keep old request identity and live hard denies"
  (with-temp-buffer
    (let* ((session (mevedel-session--create :permission-mode 'ask))
           (tool (mevedel-tool--create :name "ScopedEdit" :category 'edit :groups '(edit)))
           (mevedel-permission-rules nil)
           (old (mevedel-request--create :session session))
           (implementation (mevedel-request--create
                            :session session :permission-mode-override 'full-auto))
           (entry (list :kind 'generic :tool-name "ScopedEdit" :origin "/root"
                        :session session :data-buffer (current-buffer) :request old)))
      (mevedel-tool-register tool)
      (setq-local mevedel--session session mevedel--current-request implementation)
      (should (eq 'ask (mevedel-permission-queue--reevaluate entry)))
      (setq entry (plist-put entry :request implementation))
      (setq mevedel--current-request old)
      (should (eq 'allow (mevedel-permission-queue--reevaluate entry)))
      (setf (mevedel-session-permission-rules session) '(("ScopedEdit" :action deny)))
      (should (eq 'deny (mevedel-permission-queue--reevaluate entry)))
      (should (eq 'ask (mevedel-session-permission-mode session)))))

  :doc "Bash confinement and pending discovery retain captured authority"
  (with-temp-buffer
    (let* ((session (mevedel-session--create :permission-mode 'ask :sandbox-mode 'best-effort))
           (request (mevedel-request--create
                     :session session :permission-mode-override 'full-auto))
           (context (list :session session :buffer (current-buffer) :request request))
           snapshot)
      (setq-local mevedel--session session mevedel--current-request request)
      (should (eq 'full-auto (mevedel-bash-policy-effective-permission-mode context)))
      (should (eq 'off (mevedel-bash-policy-effective-sandbox-mode context)))
      (setq snapshot (mevedel-execution--preparation-authority
                      session (list temporary-file-directory) request))
      (setq mevedel--current-request (mevedel-request--create :session session))
      (should (equal snapshot (mevedel-execution--preparation-authority
                               session (list temporary-file-directory) request)))
      ;; Rules sharing structure must not make the snapshot depend on the
      ;; caller's printer state: admission and settlement bind it differently.
      (let* ((tail (list :recursive t))
             (rules (list (list "Bash" :file-system
                                (list (append (list :path "/a" :access 'write) tail))
                                :action 'allow)
                          (list "Read" :file-system
                                (list (append (list :path "/b" :access 'read) tail))
                                :action 'allow))))
        (setf (mevedel-session-permission-rules session) rules)
        (setq snapshot (let ((print-circle t))
                         (mevedel-execution--preparation-authority
                          session (list temporary-file-directory) request)))
        (should (equal snapshot
                       (let ((print-circle nil))
                         (mevedel-execution--preparation-authority
                          session (list temporary-file-directory) request))))
        ;; An in-place change to live authority is still detected.
        (setcar (cdr tail) nil)
        (should-not (equal snapshot (mevedel-execution--preparation-authority
                                     session (list temporary-file-directory) request))))
      ;; Strings are mutable too: copying conses alone would let a live path
      ;; change rewrite the captured snapshot and escape the launch recheck.
      (let ((path (copy-sequence "/allowed")))
        (setf (mevedel-session-resource-grants session)
              (make-list 2000 (list :path path :access 'write)))
        (setq snapshot (mevedel-execution--preparation-authority
                        session (list temporary-file-directory) request))
        (aset path 1 ?x)
        (should-not (equal snapshot (mevedel-execution--preparation-authority
                                     session (list temporary-file-directory) request))))
      (setf (mevedel-session-permission-rules session) '(("Bash" :action deny)))
      (should-not (equal snapshot (mevedel-execution--preparation-authority
                                   session (list temporary-file-directory) request)))))

  :doc "new child and nested invocations inherit only scoped permission"
  (with-temp-buffer
    (let* ((session (mevedel-session--create :permission-mode 'ask))
           (agent (mevedel-agent--create :name "scope-child"))
           (old (mevedel-agent-invocation-create agent)))
      (setq-local mevedel--session session
                  mevedel--current-request
                  (mevedel-request--create :session session :permission-mode-override 'full-auto))
      (let ((child (mevedel-agent-invocation-create agent)))
        (should-not (mevedel-agent-invocation-permission-mode-override old))
        (should (eq 'full-auto (mevedel-agent-invocation-permission-mode-override child)))
        (should-not (mevedel-agent-invocation-skill-model-override child))
        (setq mevedel--current-request nil mevedel--agent-invocation child)
        (let ((nested (mevedel-agent-invocation-create agent)))
          (should (eq 'full-auto (mevedel-agent-invocation-permission-mode-override nested)))))
      (setq mevedel--agent-invocation nil)
      (should-not (mevedel-agent-invocation-permission-mode-override
                   (mevedel-agent-invocation-create agent))))))

(provide 'test-mevedel-permissions-request-scope)
;;; test-mevedel-permissions-request-scope.el ends here
