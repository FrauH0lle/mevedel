;;; test-niwa-permissions-request-scope.el -- Scoped authority -*- lexical-binding: t -*-

;;; Commentary:
;; Actual decision and delayed queue checks across implementation identities.

;;; Code:

(require 'niwa-agents)
(require 'niwa-bash-policy)
(require 'niwa-execution)
(require 'niwa-permissions)
(require 'niwa-permission-queue)
(require 'niwa-reminders)
(require 'niwa-tool-registry)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(niwa-deftest niwa-permission-request-scoped-decisions ()
  ,test
  (test)
  :doc "native mutation uses scoped mode without changing baseline or rules"
  (with-temp-buffer
    (let* ((session (niwa-session--create :permission-mode 'ask))
           (tool (niwa-tool--create :name "ScopedEdit" :groups '(edit)))
           (niwa-permission-rules nil)
           (request (niwa-request--create
                     :session session :permission-mode-override 'edits)))
      (setq-local niwa--session session niwa--current-request request)
      (should (eq 'allow (niwa-check-permission
                         "ScopedEdit" :tool-struct tool :session session :request request)))
      (should (eq 'deny (niwa-check-permission
                        "ScopedEdit" :tool-struct tool :session session :request request
                        :session-rules '(("ScopedEdit" :action deny)))))
      (setf (niwa-request-plan-read-only request) t)
      (should (eq 'deny (niwa-check-permission
                        "ScopedEdit" :tool-struct tool :session session :request request)))
      (should (eq 'ask (niwa-session-permission-mode session)))))

  :doc "explicit Ask overrides Full Access for decisions and reminders"
  (with-temp-buffer
    (let* ((session (niwa-session--create :permission-mode 'full-auto))
           (tool (niwa-tool--create :name "ScopedEdit" :groups '(edit)))
           (niwa-permission-rules nil)
           (request (niwa-request--create
                     :session session :permission-mode-override 'ask)))
      (setq-local niwa--session session niwa--current-request request)
      (should (eq 'ask (niwa-check-permission
                       "ScopedEdit" :tool-struct tool :session session :request request)))
      (should (eq 'ask (niwa-reminders--session-mode session)))
      (should (eq 'full-auto (niwa-session-permission-mode session)))))

  :doc "delayed queue rechecks keep old request identity and live hard denies"
  (with-temp-buffer
    (let* ((session (niwa-session--create :permission-mode 'ask))
           (tool (niwa-tool--create :name "ScopedEdit" :category 'edit :groups '(edit)))
           (niwa-permission-rules nil)
           (old (niwa-request--create :session session))
           (implementation (niwa-request--create
                            :session session :permission-mode-override 'full-auto))
           (entry (list :kind 'generic :tool-name "ScopedEdit" :origin "/root"
                        :session session :data-buffer (current-buffer) :request old)))
      (niwa-tool-register tool)
      (setq-local niwa--session session niwa--current-request implementation)
      (should (eq 'ask (niwa-permission-queue--reevaluate entry)))
      (setq entry (plist-put entry :request implementation))
      (setq niwa--current-request old)
      (should (eq 'allow (niwa-permission-queue--reevaluate entry)))
      (setf (niwa-session-permission-rules session) '(("ScopedEdit" :action deny)))
      (should (eq 'deny (niwa-permission-queue--reevaluate entry)))
      (should (eq 'ask (niwa-session-permission-mode session)))))

  :doc "Bash confinement and pending discovery retain captured authority"
  (with-temp-buffer
    (let* ((session (niwa-session--create :permission-mode 'ask :sandbox-mode 'best-effort))
           (request (niwa-request--create
                     :session session :permission-mode-override 'full-auto))
           (context (list :session session :buffer (current-buffer) :request request))
           snapshot)
      (setq-local niwa--session session niwa--current-request request)
      (should (eq 'full-auto (niwa-bash-policy-effective-permission-mode context)))
      (should (eq 'off (niwa-bash-policy-effective-sandbox-mode context)))
      (setq snapshot (niwa-execution--preparation-authority
                      session (list temporary-file-directory) request))
      (setq niwa--current-request (niwa-request--create :session session))
      (should (equal snapshot (niwa-execution--preparation-authority
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
        (setf (niwa-session-permission-rules session) rules)
        (setq snapshot (let ((print-circle t))
                         (niwa-execution--preparation-authority
                          session (list temporary-file-directory) request)))
        (should (equal snapshot
                       (let ((print-circle nil))
                         (niwa-execution--preparation-authority
                          session (list temporary-file-directory) request))))
        ;; An in-place change to live authority is still detected.
        (setcar (cdr tail) nil)
        (should-not (equal snapshot (niwa-execution--preparation-authority
                                     session (list temporary-file-directory) request))))
      ;; Strings are mutable too: copying conses alone would let a live path
      ;; change rewrite the captured snapshot and escape the launch recheck.
      (let ((path (copy-sequence "/allowed")))
        (setf (niwa-session-resource-grants session)
              (make-list 2000 (list :path path :access 'write)))
        (setq snapshot (niwa-execution--preparation-authority
                        session (list temporary-file-directory) request))
        (aset path 1 ?x)
        (should-not (equal snapshot (niwa-execution--preparation-authority
                                     session (list temporary-file-directory) request))))
      (setf (niwa-session-permission-rules session) '(("Bash" :action deny)))
      (should-not (equal snapshot (niwa-execution--preparation-authority
                                   session (list temporary-file-directory) request)))))

  :doc "new child and nested invocations inherit only scoped permission"
  (with-temp-buffer
    (let* ((session (niwa-session--create :permission-mode 'ask))
           (agent (niwa-agent--create :name "scope-child"))
           (old (niwa-agent-invocation-create agent)))
      (setq-local niwa--session session
                  niwa--current-request
                  (niwa-request--create :session session :permission-mode-override 'full-auto))
      (let ((child (niwa-agent-invocation-create agent)))
        (should-not (niwa-agent-invocation-permission-mode-override old))
        (should (eq 'full-auto (niwa-agent-invocation-permission-mode-override child)))
        (should-not (niwa-agent-invocation-skill-model-override child))
        (setq niwa--current-request nil niwa--agent-invocation child)
        (let ((nested (niwa-agent-invocation-create agent)))
          (should (eq 'full-auto (niwa-agent-invocation-permission-mode-override nested)))))
      (setq niwa--agent-invocation nil)
      (should-not (niwa-agent-invocation-permission-mode-override
                   (niwa-agent-invocation-create agent))))))

(provide 'test-niwa-permissions-request-scope)
;;; test-niwa-permissions-request-scope.el ends here
