;;; test-mevedel-permissions.el --- Tests for permission system -*- lexical-binding: t -*-

;;; Commentary:

;; Tests permission preflight, decision composition, and prompt-result dispatch.

;;; Code:

(require 'mevedel-permissions)
(require 'mevedel-execution-target)
(require 'mevedel-permission-mode)
(require 'mevedel-permission-rules)
(require 'mevedel-session-control-fs)
(require 'mevedel-session-durability)
(require 'mevedel-structs)
(require 'mevedel-tool-registry)
(require 'mevedel-agents)
(require 'mevedel-reminders)
(require 'mevedel-plan)
(require 'mevedel-plan-mode)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name
               load-file-name
               byte-compile-current-file))
          "helpers"))

(mevedel-deftest mevedel-permission--plan-mode-p ()
  ,test
  (test)
  :doc "uses an explicit root session"
  (let ((session (mevedel-session--create :name "main" :plan-mode t)))
    (should (mevedel-permission--plan-mode-p session)))

  :doc "keeps a directive planning request read-only after its phase advances"
  (let ((session (mevedel-session--create :name "main"))
        (mevedel--current-request
         (mevedel-request--create :plan-read-only t)))
    (should (mevedel-permission--plan-mode-p session))
    (setf (mevedel-session-directive-planning session)
          '(:directive-id "d1" :phase implementation))
    (should (mevedel-permission--plan-mode-p session)))

  :doc "uses a retained agent's parent session"
  (let ((session (mevedel-session--create :name "main" :plan-mode t)))
    (with-temp-buffer
      (setq-local mevedel--agent-invocation
                  (mevedel-agent-invocation--create
                   :parent-session session))
      (should (mevedel-permission--plan-mode-p))))

  :doc "uses immutable directive planning authority on a retained agent"
  (let ((session (mevedel-session--create :name "main")))
    (with-temp-buffer
      (setq-local mevedel--agent-invocation
                  (mevedel-agent-invocation--create
                   :parent-session session :plan-read-only t))
      (should (mevedel-permission--plan-mode-p)))))


;;
;;; Full decision chain

(mevedel-deftest mevedel-permission--preflight ()
  ,test
  (test)
  :doc "normalizes extracted specifiers and shared decision facts"
  (let* ((get-path-calls 0)
         (tool (mevedel-tool--create
                :name "Edit"
                :get-path (lambda (input)
                            (cl-incf get-path-calls)
                            (plist-get input :file_path))
                :read-only-p nil))
         (context
          (let ((mevedel-permission-rules nil)
                (mevedel-protected-paths nil))
            (mevedel-permission--preflight
             "Edit"
             :tool-struct tool
             :content '(:file_path "/project/file.el")
             :session-rules
             '(("Edit" :path "/project/*" :action allow))
             :mode 'ask
             :workspace-root "/project"))))
    (should (= 1 get-path-calls))
    (should (equal "/project/file.el" (plist-get context :path)))
    (should-not (plist-get context :read-only-p))
    (should (equal '("/project") (plist-get context :allowed-roots)))
    (should (eq 'ask (plist-get context :mode)))
    (should-not (plist-get context :early-decision)))

  :doc "returns an absolute deny with its winning bucket"
  (let ((mevedel-permission-rules nil)
        (mevedel-protected-paths nil))
    (let* ((context
            (mevedel-permission--preflight
             "Edit"
             :session-rules '(("Edit" :action deny))
             :mode 'full-auto))
           (decision (plist-get context :early-decision)))
      (should (eq 'deny
                  (mevedel-permission-decision-raw-outcome decision)))
      (should (eq 'deny-rule (plist-get decision :via)))
      (should (eq :session (plist-get decision :bucket)))))

  :doc "allows read-only access to installed manuals without granting edits"
  (let* ((mevedel-tool-registry--source-dir "/package/")
         (path "/package/docs/ptc-dialect.md")
         (read-context
          (mevedel-permission--preflight
           "Read"
           :tool-struct (mevedel-tool--create :name "Read" :read-only-p t)
           :path path :allowed-roots '("/project")))
         (edit-context
          (mevedel-permission--preflight
           "Edit"
           :tool-struct (mevedel-tool--create :name "Edit" :read-only-p nil)
           :path path :allowed-roots '("/project"))))
    (should-not (plist-get read-context :workspace-boundary-p))
    (should (member "/package/" (plist-get read-context :allowed-roots)))
    (should (plist-get edit-context :workspace-boundary-p))
    (should-not (member "/package/" (plist-get edit-context :allowed-roots))))

  :doc "derives protected target-home facts from the owning session"
  (let* ((target (mevedel-execution-target-create
                  "/ssh:user@host:/srv/project/"))
         (session (mevedel-session--create
                   :name "remote" :execution-target target))
         (mevedel-protected-paths '(("~/.ssh/**" . inaccessible))))
    (setf (mevedel-execution-target-environment target)
          '(("HOME" . "/home/user")))
    (should
     (plist-get
      (mevedel-permission--preflight
       "Read" :path "/ssh:user@host:/home/user/.ssh/id_rsa"
       :session session :mode 'full-auto)
      :protected-path-p))))

(mevedel-deftest mevedel-check-permission-async-with-metadata ()
  ,test
  (test)
  :doc "sync and async entry points return identical non-async decisions"
  (let* ((read-tool (mevedel-tool--create :name "Read" :read-only-p t))
         (edit-tool (mevedel-tool--create :name "Edit" :read-only-p nil))
         (deny-tool
          (mevedel-tool--create
           :name "Custom"
           :read-only-p nil
           :check-permission
           (lambda (_tool _input)
             (signal 'mevedel-permission-denied '("custom reason")))))
         (cases
          `(("Edit" :tool-struct ,edit-tool
             :session-rules (("Edit" :action deny)) :mode full-auto)
            ("Read" :tool-struct ,read-tool
             :path "/repo/.git/config" :mode full-auto)
            ("Edit" :tool-struct ,edit-tool
             :path "/repo/.git/config" :mode ask)
            ("Read" :tool-struct ,read-tool
             :path "/project/file.el" :allowed-roots ("/project")
             :mode ask)
            ("Read" :tool-struct ,read-tool
             :path "/drop/file.el" :exact-allowed-paths ("/drop/file.el")
             :mode ask)
            ("Read" :tool-struct ,read-tool
             :path "/outside/file.el" :allowed-roots ("/project")
             :mode ask)
            ("Edit" :tool-struct ,edit-tool
             :request-rules (("Edit" :action ask)) :mode edits)
            ("Custom" :tool-struct ,deny-tool :mode full-auto))))
    (let ((mevedel-permission-rules nil)
          (mevedel-protected-paths '(("**/.git/**" . read-only))))
      (dolist (case cases)
        (let* ((tool-name (car case))
               (args (cdr case))
               (sync (apply #'mevedel-check-permission-with-metadata
                            tool-name args))
               async)
          (apply #'mevedel-check-permission-async-with-metadata
                 tool-name (lambda (decision) (setq async decision)) args)
          (should (equal sync async))))))

  :doc "an absolute deny is resolved once and skips both tool slots"
  (let* ((deny-checks 0)
         (sync-slot-called nil)
         (async-slot-called nil)
         (original (symbol-function
                    'mevedel-permission-rules-first-deny-bucket))
         (tool
          (mevedel-tool--create
           :name "Custom"
           :read-only-p nil
           :check-permission
           (lambda (_tool _input) (setq sync-slot-called t) 'allow)
           :check-permission-async
           (lambda (_tool _input cont)
             (setq async-slot-called t)
             (funcall cont 'allow))))
         decision)
    (let ((mevedel-permission-rules nil)
          (mevedel-protected-paths '(("**/.git/**" . read-only))))
      (cl-letf (((symbol-function 'mevedel-permission-rules-first-deny-bucket)
                 (lambda (&rest args)
                   (cl-incf deny-checks)
                   (apply original args))))
        (mevedel-check-permission-async-with-metadata
         "Custom" (lambda (result) (setq decision result))
         :tool-struct tool
         :path "/repo/.git/config"
         :invocation-rules '(("Custom" :action allow))
         :session-rules '(("Custom" :action deny))
         :mode 'full-auto)))
    (should (= 1 deny-checks))
    (should-not sync-slot-called)
    (should-not async-slot-called)
    (should (equal '(:outcome deny :raw-outcome deny
                     :via deny-rule :bucket :session)
                   decision))))

(mevedel-deftest mevedel-check-permission ()
  ,test
  (test)
  :doc "deny rule overrides everything"
  (let ((mevedel-permission-rules '(("Edit" :action deny)))
        (mevedel-protected-paths nil))
    (should (eq (mevedel-check-permission "Edit" :mode 'full-auto) 'deny)))
  :doc "protected path forces ask even in full-auto"
  (let ((mevedel-permission-rules nil)
        (mevedel-protected-paths '(("**/.git/**" . read-only))))
    (should (eq (mevedel-check-permission "Edit"
                  :path "/repo/.git/config"
                  :mode 'full-auto)
                'ask)))
  :doc "tool check-permission returning allow is respected"
  (let ((mevedel-permission-rules nil)
        (mevedel-protected-paths nil)
        (mock-tool (mevedel-tool--create
                    :name "MockTool"
                    :check-permission (lambda (_ts _input) 'allow)
                    :read-only-p nil)))
    (should (eq (mevedel-check-permission "MockTool"
                  :tool-struct mock-tool
                  :mode 'ask)
                'allow)))
  :doc "one-shot policy overrides a generic tool-slot allow"
  (let ((mevedel-permission-rules nil)
        (mevedel-protected-paths nil)
        (mock-tool (mevedel-tool--create
                    :name "MockTool"
                    :check-permission (lambda (_ts _input) 'allow)
                    :read-only-p nil)))
    (should
     (eq 'ask
         (mevedel-check-permission
          "MockTool" :tool-struct mock-tool :mode 'full-auto
          :one-shot-mutations-p t))))
  :doc "one-shot policy keeps emergency controls automatic"
  (let ((mevedel-permission-rules nil)
        (mevedel-protected-paths nil)
        (control-tool (mevedel-tool--create
                       :name "StopExecution"
                       :check-permission
                       (lambda (_ts _input)
                         '(:outcome allow :raw-outcome allow
                           :via execution-control))
                       :read-only-p nil)))
    (should
     (eq 'allow
         (mevedel-check-permission
          "StopExecution" :tool-struct control-tool :mode 'full-auto
          :one-shot-mutations-p t)))
    (should
     (eq 'deny
         (mevedel-check-permission
          "StopExecution" :tool-struct control-tool :mode 'full-auto
          :session-rules '(("StopExecution" :action deny))
          :one-shot-mutations-p t))))
  :doc "tool check-permission returning deny is respected"
  (let ((mevedel-permission-rules nil)
        (mevedel-protected-paths nil)
        (mock-tool (mevedel-tool--create
                    :name "MockTool"
                    :check-permission (lambda (_ts _input) 'deny)
                    :read-only-p nil)))
    (should (eq (mevedel-check-permission "MockTool"
                  :tool-struct mock-tool
                  :mode 'full-auto)
                'deny)))
  :doc "tool check-permission returning nil falls through to allow rule"
  (let ((mevedel-permission-rules '(("MockTool" :action allow)))
        (mevedel-protected-paths nil)
        (mock-tool (mevedel-tool--create
                    :name "MockTool"
                    :check-permission (lambda (_ts _input) nil)
                    :read-only-p nil)))
    (should (eq (mevedel-check-permission "MockTool"
                  :tool-struct mock-tool
                  :mode 'ask)
                'allow)))
  :doc "allow rule allows when no deny or protection"
  (let ((mevedel-permission-rules '(("Read" :action allow)))
        (mevedel-protected-paths nil))
    (should (eq (mevedel-check-permission "Read" :mode 'ask) 'allow)))
  :doc "mode decision when no rules match - default asks for non-read-only"
  (let ((mevedel-permission-rules nil)
        (mevedel-protected-paths nil)
        (mock-tool (mevedel-tool--create :name "Edit" :read-only-p nil)))
    (should (eq (mevedel-check-permission "Edit"
                  :tool-struct mock-tool
                  :mode 'ask)
                'ask)))
  :doc "reviewed edits use review inside roots and retain outside path rules"
  (let ((mevedel-protected-paths nil)
        (tool (mevedel-tool--create
               :name "ApplyPatch" :groups '(edit reviewed-edit))))
    (let ((mevedel-permission-rules nil))
      (should (eq (mevedel-check-permission
                   "ApplyPatch" :tool-struct tool :path "/project/a"
                   :mode 'ask :workspace-root "/project")
                  'allow))
      (should (eq (mevedel-check-permission
                   "ApplyPatch" :tool-struct tool :path "/outside/a"
                   :mode 'ask :workspace-root "/project")
                  'ask)))
    (let ((mevedel-permission-rules
           '(("ApplyPatch" :path "/outside/a" :action allow))))
      (should (eq (mevedel-check-permission
                   "ApplyPatch" :tool-struct tool :path "/outside/a"
                   :mode 'ask :workspace-root "/project")
                  'allow)))
    (let ((mevedel-permission-rules
           '(("ApplyPatch" :action ask))))
      (dolist (mode '(ask edits full-auto))
        (should (eq (mevedel-check-permission
                     "ApplyPatch" :tool-struct tool :path "/project/a"
                     :mode mode :workspace-root "/project")
                    'ask)))))
  :doc "one-shot reviewed edits avoid a duplicate prompt without bypassing policy"
  (let ((mevedel-permission-rules nil)
        (mevedel-protected-paths nil)
        (tool (mevedel-tool--create
               :name "ApplyPatch" :groups '(edit reviewed-edit))))
    (should
     (eq 'allow
         (mevedel-check-permission
          "ApplyPatch" :tool-struct tool :path "/project/a"
          :mode 'full-auto :workspace-root "/project"
          :one-shot-mutations-p t)))
    (should
     (eq 'ask
         (mevedel-check-permission
          "ApplyPatch" :tool-struct tool :path "/outside/a"
          :mode 'full-auto :workspace-root "/project"
          :one-shot-mutations-p t)))
    (should
     (eq 'deny
         (mevedel-check-permission
          "ApplyPatch" :tool-struct tool :path "/project/a"
          :session-rules '(("ApplyPatch" :action deny))
          :mode 'full-auto :workspace-root "/project"
          :one-shot-mutations-p t))))
  :doc "Plan mode denies edits and Eval across modes and explicit allows"
  (let* ((mevedel-permission-rules nil)
         (mevedel-protected-paths nil)
         (session (mevedel-session--create :name "plan" :plan-mode t)))
    (dolist (mode '(ask edits full-auto))
      (dolist (name '("Write" "Edit" "MkDir" "Eval"))
        (let ((tool (mevedel-tool--create
                     :name name :read-only-p nil
                     :groups (if (equal name "Eval") '(eval) '(edit)))))
          (should
           (eq
            (mevedel-check-permission
             name
             :tool-struct tool
             :session session
             :session-rules `((,name :action allow))
             :mode mode)
            'deny))))))
  :doc "Plan boundary follows retained agents across modes and explicit allows"
  (let* ((mevedel-permission-rules nil)
         (mevedel-protected-paths nil)
         (session (mevedel-session--create :name "plan" :plan-mode t))
         (invocation (mevedel-agent-invocation--create
                      :parent-session session)))
    (with-temp-buffer
      (setq-local mevedel--agent-invocation invocation)
      (dolist (mode '(ask edits full-auto))
        (dolist (name '("Write" "Edit" "MkDir" "Eval"))
          (let ((tool (mevedel-tool--create
                       :name name :read-only-p nil
                       :groups (if (equal name "Eval") '(eval) '(edit)))))
            (should
             (eq
              (mevedel-check-permission
               name
               :tool-struct tool
               :session-rules `((,name :action allow))
               :mode mode)
              'deny)))))))
  :doc "Plan allows only an already-prepared all-local ApplyPatch proposal"
  (let* ((mevedel-permission-rules nil)
         (mevedel-protected-paths nil)
         (session (mevedel-session--create :name "plan" :plan-mode t))
         (tool (mevedel-tool--create
                :name "ApplyPatch" :read-only-p nil
                :groups '(edit reviewed-edit))))
    (dolist (mode '(ask edits full-auto))
      (should
       (eq 'allow
           (mevedel-check-permission
            "ApplyPatch" :tool-struct tool :session session :mode mode
            :patch-local-only-p t)))
      (should
       (eq 'deny
           (mevedel-check-permission
            "ApplyPatch" :tool-struct tool :session session :mode mode
            :patch-local-only-p nil))))
    (should
     (eq 'deny
         (mevedel-check-permission
          "ApplyPatch" :tool-struct tool :session session :mode 'full-auto
          :patch-local-only-p t
          :session-rules '(("ApplyPatch" :action deny))))))
  :doc "directive planning keeps all-local ApplyPatch denied"
  (let* ((mevedel-permission-rules nil)
         (mevedel-protected-paths nil)
         (session (mevedel-session--create
                   :name "directive"
                   :directive-planning
                   '(:directive-id "d1" :phase approval)))
         (request (mevedel-request--create
                   :plan-read-only t :directive-uuid "d1"))
         (tool (mevedel-tool--create
                :name "ApplyPatch" :read-only-p nil
                :groups '(edit reviewed-edit)))
         (mevedel--current-request request))
    (should
     (eq 'deny
         (mevedel-check-permission
          "ApplyPatch" :tool-struct tool :session session :mode 'full-auto
          :patch-local-only-p t))))
  :doc "read-only tool allowed in ask mode"
  (let ((mevedel-permission-rules nil)
        (mevedel-protected-paths nil)
        (mock-tool (mevedel-tool--create :name "Read" :read-only-p t)))
    (should (eq (mevedel-check-permission "Read"
                  :tool-struct mock-tool
                  :mode 'ask)
                'allow)))
  :doc "one-shot mutation policy overrides full-auto and inherited allows"
  (let ((mevedel-permission-rules nil)
        (mevedel-protected-paths nil)
        (edit-tool (mevedel-tool--create :name "Edit" :read-only-p nil))
        (read-tool (mevedel-tool--create :name "Read" :read-only-p t)))
    (should (eq (mevedel-check-permission
                 "Edit" :tool-struct edit-tool
                 :session-rules '(("Edit" :action allow))
                 :mode 'full-auto :one-shot-mutations-p t)
                'ask))
    (should (eq (mevedel-check-permission
                 "Read" :tool-struct read-tool
                 :session-rules '(("Read" :action allow))
                 :mode 'full-auto :one-shot-mutations-p t)
                'allow)))
  :doc "session rules work alongside defcustom rules"
  (let ((mevedel-permission-rules '(("Edit" :action ask)))
        (mevedel-protected-paths nil)
        (session-rules '(("Edit" :path "/allowed/*" :action allow))))
    (should (eq (mevedel-check-permission "Edit"
                  :path "/allowed/file.el"
                  :session-rules session-rules
                  :mode 'ask)
                'allow)))
  :doc "unknown tool (no struct) defaults to ask"
  (let ((mevedel-permission-rules nil)
        (mevedel-protected-paths nil))
    (should (eq (mevedel-check-permission "UnknownTool" :mode 'ask) 'ask)))
  :doc "get-path extracts path from content"
  (let ((mevedel-permission-rules nil)
        (mevedel-protected-paths '(("**/.git/**" . read-only)))
        (mock-tool (mevedel-tool--create
                    :name "Edit"
                    :get-path (lambda (input) (plist-get input :file_path))
                    :read-only-p nil)))
    (should (eq (mevedel-check-permission "Edit"
                  :tool-struct mock-tool
                  :content '(:file_path "/repo/.git/config")
                  :mode 'full-auto)
                'ask)))
  :doc "path inside workspace root is implicitly allowed"
  (let ((mevedel-permission-rules nil)
        (mevedel-protected-paths nil)
        (mock-tool (mevedel-tool--create :name "Read" :read-only-p t)))
    (should (eq (mevedel-check-permission "Read"
                  :tool-struct mock-tool
                  :path "/project/src/file.el"
                  :mode 'ask
                  :workspace-root "/project")
                'allow)))
  :doc "path outside workspace root asks even for read-only tools"
  (let ((mevedel-permission-rules nil)
        (mevedel-protected-paths nil)
        (mock-tool (mevedel-tool--create :name "Read" :read-only-p t)))
    (should (eq (mevedel-check-permission "Read"
                  :tool-struct mock-tool
                  :path "/etc/passwd"
                  :mode 'ask
                  :workspace-root "/project")
                'ask)))
  :doc "exact read resource grant allows Read outside workspace"
  (let ((mevedel-permission-rules nil)
        (mevedel-protected-paths nil)
        (mock-tool (mevedel-tool--create :name "Read" :read-only-p t)))
    (should (eq (mevedel-check-permission
                 "Read" :tool-struct mock-tool
                 :path "/etc/passwd" :mode 'ask
                 :workspace-root "/project"
                 :resource-grants '((:path "/etc/passwd" :access read)))
                'allow)))
  :doc "recursive resource grant allows descendant reads outside workspace"
  (let ((mevedel-permission-rules nil)
        (mevedel-protected-paths nil)
        (mock-tool (mevedel-tool--create :name "Read" :read-only-p t))
        (grants '((:path "/usr/share/emacs" :access read :recursive t))))
    (should (eq (mevedel-check-permission
                 "Read" :tool-struct mock-tool
                 :path "/usr/share/emacs/31.1/lisp/simple.el" :mode 'ask
                 :workspace-root "/project"
                 :resource-grants grants)
                'allow))
    (should (eq (mevedel-check-permission
                 "Read" :tool-struct mock-tool
                 :path "/usr/share/emacs-old/file" :mode 'ask
                 :workspace-root "/project"
                 :resource-grants grants)
                'ask)))
  :doc "explicit deny and protected paths stay final over a recursive grant"
  (let ((mock-tool (mevedel-tool--create :name "Read" :read-only-p t))
        (grants '((:path "/outside" :access write :recursive t))))
    (let ((mevedel-permission-rules
           '(("Read" :path "/outside/**" :action deny)))
          (mevedel-protected-paths nil))
      (should (eq (mevedel-check-permission
                   "Read" :tool-struct mock-tool
                   :path "/outside/denied.el" :mode 'ask
                   :workspace-root "/project"
                   :resource-grants grants)
                  'deny)))
    ;; A recursive grant covering a protected path satisfies its gate,
    ;; exactly as an exact grant on the protected path does today.
    (let ((mevedel-permission-rules nil)
          (mevedel-protected-paths '(("/outside/.ssh/**" . inaccessible))))
      (should (eq (mevedel-check-permission
                   "Read" :tool-struct mock-tool
                   :path "/outside/.ssh/config" :mode 'ask
                   :workspace-root "/project"
                   :resource-grants nil)
                  'ask))
      (should (eq (mevedel-check-permission
                   "Read" :tool-struct mock-tool
                   :path "/outside/.ssh/config" :mode 'ask
                   :workspace-root "/project"
                   :resource-grants grants)
                  'allow))))
  :doc "active Goal request grants only exact accepted-plan reads"
  (let* ((mevedel-permission-rules nil)
         (mevedel-protected-paths nil)
         (request (mevedel-request--create
                   :goal-plan-read-path "/sessions/accepted-plan.md"))
         (read (mevedel-tool--create :name "Read" :read-only-p t))
         (write (mevedel-tool--create :name "Write" :read-only-p nil))
         (check
          (lambda (tool path)
            (let ((context
                   (mevedel-permission--invocation-context
                    :tool tool :request request :path path
                    :workspace-root "/project" :mode 'ask)))
              (apply #'mevedel-check-permission
                     (mevedel-tool-name tool)
                     (mevedel-permission--checker-args context))))))
    (should (eq 'allow
                (funcall check read "/sessions/accepted-plan.md")))
    (should (eq 'ask
                (funcall check read "/sessions/sibling.md")))
    (should (eq 'ask
                (funcall check write "/sessions/accepted-plan.md"))))
  :doc "resource grant has distinct decision metadata"
  (let* ((mevedel-permission-rules nil)
         (mevedel-protected-paths nil)
         (mock-tool (mevedel-tool--create :name "Read" :read-only-p t))
         (decision
          (mevedel-check-permission-with-metadata
           "Read" :tool-struct mock-tool
           :path "/etc/passwd" :mode 'ask
           :workspace-root "/project"
           :resource-grants '((:path "/etc/passwd" :access read)))))
    (should (eq 'allow
                (mevedel-permission-decision-raw-outcome decision)))
    (should (eq 'resource-grant (plist-get decision :via))))
  :doc "read resource grant does not allow Write"
  (let ((mevedel-permission-rules nil)
        (mevedel-protected-paths nil)
        (mock-tool (mevedel-tool--create :name "Write" :read-only-p nil)))
    (should (eq (mevedel-check-permission
                 "Write" :tool-struct mock-tool
                 :path "/etc/passwd" :mode 'ask
                 :workspace-root "/project"
                 :resource-grants '((:path "/etc/passwd" :access read)))
                'ask)))
  :doc "write resource grants satisfy only the resource gate"
  (let ((mevedel-permission-rules nil)
        (mevedel-protected-paths nil)
        (mock-tool (mevedel-tool--create
                    :name "Write" :groups '(edit) :read-only-p nil))
        (grants '((:path "/outside/target.el" :access write))))
    (should (eq (mevedel-check-permission
                 "Write" :tool-struct mock-tool
                 :path "/outside/target.el" :mode 'ask
                 :workspace-root "/project" :resource-grants grants)
                'ask))
    (should (eq (mevedel-check-permission
                 "Write" :tool-struct mock-tool
                 :path "/outside/target.el" :mode 'edits
                 :workspace-root "/project" :resource-grants grants)
                'allow))
    (should (eq (mevedel-check-permission
                 "Write" :tool-struct mock-tool
                 :path "/outside/sibling.el" :mode 'edits
                 :workspace-root "/project" :resource-grants grants)
                'ask)))
  :doc "resource grant does not override a command-specific ask"
  (let ((mevedel-permission-rules nil)
        (mevedel-protected-paths nil)
        (mock-tool
         (mevedel-tool--create
          :name "Bash" :read-only-p nil
          :check-permission (lambda (_tool _content) 'ask))))
    (should (eq (mevedel-check-permission
                 "Bash" :tool-struct mock-tool
                 :content '(:command "curl https://example.com")
                 :path "/outside/file" :mode 'ask
                 :workspace-root "/project"
                 :resource-grants '((:path "/outside/file" :access write)))
                'ask)))
  :doc "command-specific allow does not override missing resource authority"
  (let ((mevedel-permission-rules nil)
        (mevedel-protected-paths nil)
        (mock-tool
         (mevedel-tool--create
          :name "Bash" :read-only-p nil
          :check-permission (lambda (_tool _content) 'allow))))
    (should (eq (mevedel-check-permission
                 "Bash" :tool-struct mock-tool
                 :content '(:command "cat /outside/file")
                 :path "/outside/file" :mode 'ask
                 :workspace-root "/project")
                'ask))
    (should (eq (mevedel-check-permission
                 "Bash" :tool-struct mock-tool
                 :content '(:command "cat /outside/file")
                 :path "/outside/file" :mode 'ask
                 :workspace-root "/project"
                 :resource-grants '((:path "/outside/file" :access write)))
                'allow)))
  :doc "resource grant does not authorize a command slot that declines"
  (let ((mevedel-permission-rules nil)
        (mevedel-protected-paths nil)
        (mock-tool
         (mevedel-tool--create
          :name "Bash" :read-only-p nil
          :check-permission (lambda (_tool _content) nil))))
    (should (eq (mevedel-check-permission
                 "Bash" :tool-struct mock-tool
                 :content '(:command "unknown")
                 :path "/outside/file" :mode 'ask
                 :workspace-root "/project"
                 :resource-grants '((:path "/outside/file" :access write)))
                'ask)))
  :doc "explicit allow rule overrides workspace boundary"
  (let ((mevedel-permission-rules '(("Read" :path "/etc/*" :action allow)))
        (mevedel-protected-paths nil)
        (mock-tool (mevedel-tool--create :name "Read" :read-only-p t)))
    (should (eq (mevedel-check-permission "Read"
                  :tool-struct mock-tool
                  :path "/etc/hosts"
                  :mode 'ask
                  :workspace-root "/project")
                'allow)))
  :doc "wildcard allow rule covers paths outside workspace"
  (let ((mevedel-permission-rules '(("*" :path "/shared/**" :action allow)))
        (mevedel-protected-paths nil))
    (should (eq (mevedel-check-permission "Edit"
                  :path "/shared/lib/util.el"
                  :mode 'ask
                  :workspace-root "/project")
                'allow)))
  :doc "no workspace root falls through to mode for non-path tools"
  (let ((mevedel-permission-rules nil)
        (mevedel-protected-paths nil)
        (mock-tool (mevedel-tool--create :name "Read" :read-only-p t)))
    (should (eq (mevedel-check-permission "Read"
                  :tool-struct mock-tool
                  :mode 'ask)
                'allow)))
  :doc "no workspace root with path falls through to ask"
  (let ((mevedel-permission-rules nil)
        (mevedel-protected-paths nil))
    (should (eq (mevedel-check-permission "Read"
                  :path "/some/file.el"
                  :mode 'ask)
                'ask)))
  :doc "get-pattern extracts command string for pattern rule match"
  (let ((mevedel-permission-rules
         '(("Bash" :pattern "echo*" :action allow)))
        (mevedel-protected-paths nil)
        (mock-tool (mevedel-tool--create
                    :name "Bash"
                    :get-pattern (lambda (input) (plist-get input :command))
                    :read-only-p nil)))
    (should (eq (mevedel-check-permission "Bash"
                  :tool-struct mock-tool
                  :content '(:command "echo hello")
                  :mode 'ask)
                'allow)))
  :doc "get-domain extracts host for domain rule match"
  (let ((mevedel-permission-rules
         '(("WebFetch" :domain "*.example.com" :action allow)))
        (mevedel-protected-paths nil)
        (mock-tool (mevedel-tool--create
                    :name "WebFetch"
                    :get-domain (lambda (input) (plist-get input :host))
                    :read-only-p t)))
    (should (eq (mevedel-check-permission "WebFetch"
                  :tool-struct mock-tool
                  :content '(:host "api.example.com")
                  :mode 'ask)
                'allow)))
  :doc "get-name extracts name for name rule match"
  (let ((mevedel-permission-rules
         '(("Agent" :name "explorer" :action allow)))
        (mevedel-protected-paths nil)
        (mock-tool (mevedel-tool--create
                    :name "Agent"
                    :get-name (lambda (input) (plist-get input :task_name))
                    :read-only-p nil)))
    (should (eq (mevedel-check-permission "Agent"
                  :tool-struct mock-tool
                  :content '(:task_name "explorer")
                  :mode 'ask)
                'allow))))


;;
;;; Session rule storage

(mevedel-deftest mevedel-permission--add-session-rule ()
  ,test
  (test)
  :doc "adds rule to session"
  (let ((session (mevedel-session--create :name "test")))
    (mevedel-permission--add-session-rule session "Edit" 'allow)
    (should (equal (mevedel-session-permission-rules session)
                   '(("Edit" :action allow)))))
  :doc "adds path-scoped rule"
  (let ((session (mevedel-session--create :name "test")))
    (mevedel-permission--add-session-rule session "Edit" 'allow "/foo/*")
    (should (equal (mevedel-session-permission-rules session)
                   '(("Edit" :path "/foo/*" :action allow)))))
  :doc "appends multiple rules"
  (let ((session (mevedel-session--create :name "test")))
    (mevedel-permission--add-session-rule session "Read" 'allow)
    (mevedel-permission--add-session-rule session "Edit" 'deny)
    (should (= (length (mevedel-session-permission-rules session)) 2)))
  :doc "deduplicates exact session rules"
  (let ((session (mevedel-session--create :name "test")))
    (mevedel-permission--add-session-rule session "Read" 'allow)
    (mevedel-permission--add-session-rule session "Read" 'allow)
    (mevedel-permission--add-session-rule session "Edit" 'allow "/foo/*")
    (mevedel-permission--add-session-rule session "Edit" 'allow "/foo/*")
    (mevedel-permission--add-session-rule
     session "Bash" 'allow nil
     :spec-key :pattern :spec-value "git diff:*")
    (mevedel-permission--add-session-rule
     session "Bash" 'allow nil
     :spec-key :pattern :spec-value "git diff:*")
    (should (equal (mevedel-session-permission-rules session)
                   '(("Read" :action allow)
                     ("Edit" :path "/foo/*" :action allow)
                     ("Bash" :pattern "git diff:*" :action allow)))))
  :doc "preserves distinct session rules"
  (let ((session (mevedel-session--create :name "test")))
    (mevedel-permission--add-session-rule session "Bash" 'allow nil
     :spec-key :pattern :spec-value "git diff:*")
    (mevedel-permission--add-session-rule session "Bash" 'deny nil
     :spec-key :pattern :spec-value "git diff:*")
    (mevedel-permission--add-session-rule session "Bash" 'allow nil
     :spec-key :pattern :spec-value "git status:*")
    (should (equal (mevedel-session-permission-rules session)
                   '(("Bash" :pattern "git diff:*" :action allow)
                     ("Bash" :pattern "git diff:*" :action deny)
                     ("Bash" :pattern "git status:*" :action allow)))))

  :doc "writes through to the same struct shared by aliases (by-reference)"
  ;; Pins the sub-agent permission-propagation contract: agent buffers
  ;; carry the parent session struct buffer-locally by reference, so a
  ;; rule recorded inside any agent that resolves to the same struct
  ;; appears on the parent's slot immediately.
  (let* ((parent-session (mevedel-session--create :name "parent"))
         (sub-agent-session-alias parent-session))
    (mevedel-permission--add-session-rule
     sub-agent-session-alias "Bash" 'allow nil
     :spec-key :pattern :spec-value "ls")
    (should (equal (mevedel-session-permission-rules parent-session)
                   '(("Bash" :pattern "ls" :action allow))))
    (should (eq (mevedel-session-permission-rules parent-session)
                (mevedel-session-permission-rules sub-agent-session-alias)))))


(mevedel-deftest mevedel-permission-remove-session-resource-grant ()
  ,test
  (test)
  :doc "revocation restores the underlying protected-path decision"
  (let* ((path "/repo/.git/config")
         (session (mevedel-session--create :name "test"))
         (tool (mevedel-tool--create :name "Read" :read-only-p t))
         (mevedel-permission-rules nil)
         (mevedel-protected-paths '(("**/.git/**" . read-only))))
    (mevedel-permission-add-session-resource-grant session path 'read)
    (should (eq 'allow
                (mevedel-check-permission
                 "Read" :tool-struct tool :path path :mode 'ask
                 :workspace-root "/repo"
                 :resource-grants
                 (mevedel-session-resource-grants session))))
    (mevedel-permission-remove-session-resource-grant session path 'read)
    (should-not (mevedel-session-resource-grants session))
    (should (eq 'ask
                (mevedel-check-permission
                 "Read" :tool-struct tool :path path :mode 'ask
                 :workspace-root "/repo"
                 :resource-grants
                 (mevedel-session-resource-grants session)))))
  :doc "revocation does not mutate another session's shared list"
  (let* ((first (list :path (expand-file-name
                             "first" temporary-file-directory)
                      :access 'read))
         (second (list :path (expand-file-name
                              "second" temporary-file-directory)
                       :access 'read))
         (shared (list first second))
         (session (mevedel-session--create
                   :name "child" :resource-grants shared))
         (other (mevedel-session--create
                 :name "parent" :resource-grants shared)))
    (mevedel-permission-remove-session-resource-grant
     session (plist-get second :path) 'read)
    (should (equal (list first)
                   (mevedel-session-resource-grants session)))
    (should (equal (list first second)
                   (mevedel-session-resource-grants other)))))

(mevedel-deftest mevedel-permission-add-session-resource-grant ()
  ,test
  (test)
  :doc "write authority promotes an exact read grant"
  (let* ((path (expand-file-name "outside" temporary-file-directory))
         (session (mevedel-session--create :name "test")))
    (mevedel-permission-add-session-resource-grant session path 'read)
    (mevedel-permission-add-session-resource-grant session path 'write)
    (should
     (equal `((:path ,path :access write))
            (mevedel-session-resource-grants session))))
  :doc "recursive grants keep their own identity next to exact ones"
  (let* ((path (expand-file-name "tree" temporary-file-directory))
         (session (mevedel-session--create :name "test")))
    (mevedel-permission-add-session-resource-grant session path 'read)
    (mevedel-permission-add-session-resource-grant session path 'read t)
    (should
     (equal `((:path ,path :access read)
              (:path ,path :access read :recursive t))
            (mevedel-session-resource-grants session)))
    (mevedel-permission-remove-session-resource-grant session path 'read t)
    (should
     (equal `((:path ,path :access read))
            (mevedel-session-resource-grants session)))))


(mevedel-deftest mevedel-permission-invalidate-target-grants ()
  ,test
  (test)
  :doc "revokes exact session, frozen, and dropped grants but not the workspace store"
  (let* ((tmp-dir (file-name-as-directory
                   (make-temp-file "mevedel-test-" t)))
         (mevedel-user-dir (file-name-concat tmp-dir "global/"))
         (path (file-name-concat tmp-dir "outside.el"))
         (workspace (mevedel-workspace--create
                     :type 'project :id "test" :root tmp-dir
                     :name "test" :file-cache nil))
         (session (mevedel-session-create "main" workspace)))
    (unwind-protect
        (with-temp-buffer
          (setq-local mevedel--session session
                      mevedel-permission--context-frozen-p t
                      mevedel-permission--frozen-resource-grants
                      `((:path ,path :access read)))
          (setf (mevedel-session-permission-rules session)
                '(("Bash" :pattern "git status:*" :action allow))
                (mevedel-session-resource-grants session)
                `((:path ,path :access read))
                (mevedel-session-dropped-file-grants session)
                (list path)
                (mevedel-session-active-dropped-file-grants session)
                (list path))
          (mevedel-permission-persistence-save-rule
           workspace "Read" 'allow)
          (mevedel-permission-persistence-save-resource-grant
           workspace path 'read)
          (should (mevedel-permission-invalidate-target-grants session))
          (should-not (mevedel-session-resource-grants session))
          (should-not (mevedel-session-dropped-file-grants session))
          (should-not
           (mevedel-session-active-dropped-file-grants session))
          (should-not mevedel-permission--frozen-resource-grants)
          (should
           (equal '(("Bash" :pattern "git status:*" :action allow))
                  (mevedel-session-permission-rules session)))
          ;; The workspace store is shared configuration, not authority
          ;; bound to this target's incarnation: it keeps its grants.
          (should
           (equal `((:path ,path :access read))
                  (mevedel-permission-persistence-load-resource-grants
                   workspace)))
          (should (equal '(("Read" :action allow))
                         (mevedel-permission-persistence-load-rules
                          workspace))))
      (delete-directory tmp-dir t))))


;;
;;; Prompt result dispatch

(mevedel-deftest mevedel-permission--apply-prompt-result ()
  ,test
  (test)
  :doc "allow-once returns allow without storage"
  (let ((session (mevedel-session--create :name "test")))
    (should (eq (mevedel-permission--apply-prompt-result
                 'allow-once "Edit" session)
                'allow))
    (should-not (mevedel-session-permission-rules session)))
  :doc "allow-session stores rule and returns allow"
  (let ((session (mevedel-session--create :name "test")))
    (should (eq (mevedel-permission--apply-prompt-result
                 'allow-session "Edit" session)
                'allow))
    (should (= (length (mevedel-session-permission-rules session)) 1)))
  :doc "allow-session stores an execution-level-qualified pattern rule"
  (let ((session (mevedel-session--create :name "test")))
    (should
     (eq 'allow
         (mevedel-permission--apply-prompt-result
          'allow-session "Bash" session nil nil
          :spec-key :pattern :spec-value "emacs --batch *"
          :sandbox-permissions 'require-escalated)))
    (should
     (equal
      '(("Bash" :pattern "emacs --batch *"
                :sandbox-permissions require-escalated
                :action allow))
      (mevedel-session-permission-rules session))))
  :doc "allow-session stores network authority independently"
  (let ((session (mevedel-session--create :name "test")))
    (mevedel-permission--apply-prompt-result
     'allow-session "Bash" session nil nil
     :spec-key :pattern :spec-value "npx test")
    (mevedel-permission--apply-prompt-result
     'allow-session "Bash" session nil nil
     :spec-key :pattern :spec-value "npx test" :network t)
    (should
     (equal
      '(("Bash" :pattern "npx test" :action allow)
        ("Bash" :pattern "npx test" :network t :action allow))
      (mevedel-session-permission-rules session))))
  :doc "allow-session stores exact resource authority separately from rules"
  (let* ((session (mevedel-session--create :name "test"))
         (path (expand-file-name "/outside/file.el")))
    (should (eq (mevedel-permission--apply-prompt-result
                 'allow-session "Read" session nil path
                 :resource-access 'read)
                'allow))
    (should (equal (list (list :path path :access 'read))
                   (mevedel-session-resource-grants session)))
    (should-not (mevedel-session-permission-rules session)))
  :doc "always-allow stores persistent and session rules"
  (let* ((tmp-dir (make-temp-file "mevedel-test-" t))
         (mevedel-user-dir (file-name-concat tmp-dir "global/"))
         (ws (mevedel-workspace--create
              :type 'project :id "test" :root tmp-dir
              :name "test"
              :file-cache (make-hash-table :test #'equal)))
         (session (mevedel-session--create :name "test")))
    (unwind-protect
        (progn
          (should (eq (mevedel-permission--apply-prompt-result
                       'always-allow "Read" session ws)
                      'allow))
          (should (= (length (mevedel-session-permission-rules session)) 1))
          (should (= (length (mevedel-permission-persistence-load-rules ws)) 1)))
      (delete-directory tmp-dir t)))
  :doc "always-allow stores persistent authority without a session duplicate"
  (let* ((tmp-dir (make-temp-file "mevedel-test-" t))
         (mevedel-user-dir (file-name-concat tmp-dir "global/"))
         (path (file-name-concat tmp-dir "outside.el"))
         (ws (mevedel-workspace--create
              :type 'project :id "test" :root tmp-dir
              :name "test" :file-cache nil))
         (session (mevedel-session--create :name "test")))
    (unwind-protect
        (progn
          (should (eq (mevedel-permission--apply-prompt-result
                       'always-allow "Write" session ws path
                       :resource-access 'write)
                      'allow))
          (should-not (mevedel-session-resource-grants session))
          (should (equal (list (list :path path :access 'write))
                         (mevedel-permission-persistence-load-resource-grants
                          ws)))
          (should-not (mevedel-session-permission-rules session)))
      (delete-directory tmp-dir t)))
  :doc "deny-once returns deny without storage"
  (let ((session (mevedel-session--create :name "test")))
    (should (eq (mevedel-permission--apply-prompt-result
                 'deny-once "Edit" session)
                'deny))
    (should-not (mevedel-session-permission-rules session)))
  :doc "deny-session stores rule and returns deny"
  (let ((session (mevedel-session--create :name "test")))
    (should (eq (mevedel-permission--apply-prompt-result
                 'deny-session "Edit" session)
                'deny))
    (should (equal (plist-get (cdar (mevedel-session-permission-rules session))
                              :action)
                   'deny)))
  :doc "unknown result defaults to deny"
  (should (eq (mevedel-permission--apply-prompt-result 'bogus "Edit") 'deny)))


(mevedel-deftest mevedel-permission--invocation-context ()
  ,test
  (test)
  :doc "resolves a named built-in tool before deriving capability facts"
  (let ((tool (mevedel-tool--create :name "Read" :read-only-p t)))
    (cl-letf (((symbol-function 'mevedel-tool-ensure)
               (lambda (name)
                 (and (equal name "Read") tool))))
      (let ((context (mevedel-permission--invocation-context
                      :tool-name "Read"
                      :path "/outside/file.el"
                      :workspace-root "/workspace")))
        (should (eq tool (plist-get context :tool)))
        (should (eq 'read (plist-get context :resource-access))))))

  :doc "extracts checker facts and prompt rule facts for an outside path"
  (let* ((root (file-name-as-directory
                (make-temp-file "mevedel-perm-root-" t)))
         (outside (file-name-as-directory
                   (make-temp-file "mevedel-perm-outside-" t)))
         (path (file-name-concat outside "secret.txt"))
         (workspace (mevedel-workspace--create
                     :type 'file :id "root" :root root
                     :name "root" :file-cache nil))
         (session (mevedel-session--create
                   :name "test" :workspace workspace
                   :permission-mode 'ask
                   :permission-rules '(("Read" :action ask))))
         (tool (mevedel-tool--create
                :name "Read" :read-only-p t
                :get-path (lambda (args) (plist-get args :file_path)))))
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel--all-allowed-roots)
                   (lambda (&optional _buffer) (list root))))
          (let ((context (mevedel-permission--invocation-context
                          :tool tool
                          :args (list :file_path path)
                          :session session
                          :workspace workspace)))
            (should (equal path (plist-get context :path)))
            (should (equal root (plist-get context :workspace-root)))
            (should (equal (list (file-name-as-directory
                                  (expand-file-name
                                   mevedel-tool-registry--source-dir))
                                 root)
                           (plist-get context :allowed-roots)))
            (should (eq :path (plist-get context :specifier-key)))
            (should (equal path (plist-get context :specifier-value)))
            (should (equal "*" (plist-get context :rule-tool)))
            (should (eq :path (plist-get context :rule-key)))
            (should (equal path (plist-get context :rule-value)))
            (should (eq 'read (plist-get context :resource-access)))
            (should (plist-get context :include-always))))
      (delete-directory root t)
      (delete-directory outside t)))

  :doc "explicit specifiers override tool getter extraction"
  (let* ((tool (mevedel-tool--create
                :name "Bash" :read-only-p nil
                :get-pattern (lambda (_args) "getter-pattern")))
         (context (mevedel-permission--invocation-context
                   :tool tool
                   :args '(:command "getter-pattern")
                   :pattern "explicit-pattern")))
    (should (equal "explicit-pattern" (plist-get context :pattern)))
    (should (eq :pattern (plist-get context :specifier-key)))
    (should (equal "explicit-pattern"
                   (plist-get context :specifier-value))))

  :doc "retains the pipeline permission-request boundary"
  (let* ((request #'ignore)
         (tool (mevedel-tool--create :name "Bash" :read-only-p nil))
         (context (mevedel-permission--invocation-context
                   :tool tool :permission-request request)))
    (should (eq request (plist-get context :permission-request))))

  :doc "inherits one-shot mutation policy from the request"
  (let* ((tool (mevedel-tool--create :name "Edit" :read-only-p nil))
         (request (mevedel-request--create :one-shot-mutations-p t))
         (context (mevedel-permission--invocation-context
                   :tool tool :request request)))
    (should (plist-get context :one-shot-mutations-p)))

  :doc "uses a frozen persistent permission snapshot without reloading it"
  (let* ((root (file-name-as-directory
                (make-temp-file "mevedel-perm-frozen-" t)))
         (workspace (mevedel-workspace--create
                     :type 'file :id root :root root :name "frozen"
                     :file-cache nil))
         (session (mevedel-session--create
                   :name "frozen" :workspace workspace
                   :permission-rules '(("Read" :action ask))))
         (tool (mevedel-tool--create :name "Read" :read-only-p t)))
    (unwind-protect
        (with-temp-buffer
          (setq-local mevedel-permission--context-frozen-p t
                      mevedel-permission--frozen-persistent-rules
                      '(("Read" :action deny))
                      mevedel-permission--frozen-resource-grants
                      `((:path ,root :access read)))
          (cl-letf (((symbol-function
                      'mevedel-permission-persistence-load-rules)
                     (lambda (_workspace)
                       (ert-fail "reloaded persistent rules")))
                    ((symbol-function
                      'mevedel-permission-persistence-load-resource-grants)
                     (lambda (_workspace)
                       (ert-fail "reloaded persistent grants"))))
            (let* ((context (mevedel-permission--invocation-context
                             :tool tool :session session
                             :workspace workspace))
                   (buckets (plist-get context :buckets)))
              (should (equal '(("Read" :action deny))
                             (alist-get :persistent buckets)))
              (should (equal `((:path ,root :access read))
                             (plist-get context :resource-grants))))))
      (delete-directory root t))))

(mevedel-deftest mevedel-permission--one-shot-prompt-entry
  (:doc "captures the side buffer without caching parent activity")
  (let ((side-buffer (generate-new-buffer " *mevedel-side-permission*")))
    (unwind-protect
        (let ((entry (mevedel-permission--one-shot-prompt-entry
                      '(:kind permission :mutation-p t) side-buffer)))
          (should (eq side-buffer (plist-get entry :data-buffer)))
          (should (plist-get entry :once-only))
          (should-not (plist-member entry :shared-workspace-active-p)))
      (kill-buffer side-buffer))))

(mevedel-deftest mevedel-permission--checker-args ()
  ,test
  (test)
  :doc "passes the normalized invocation preflight without rebuilding it"
  (let* ((tool (mevedel-tool--create :name "Read" :read-only-p t))
         (context
          (mevedel-permission--invocation-context
           :tool tool
           :path "/tmp/file.txt"
           :session-rules '(("Read" :action allow))
           :mode 'ask
           :workspace-root "/tmp/"
           :allowed-roots '("/tmp/")
           :exact-allowed-paths '("/tmp/file.txt")))
         (args (mevedel-permission--checker-args context)))
    (should (equal :normalized-context (car args)))
    (should (= 2 (length args)))
    (setq args (plist-get args :normalized-context))
    (should (eq tool (plist-get args :tool)))
    (should (equal "/tmp/file.txt" (plist-get args :path)))
    (should (eq 'ask (plist-get args :mode)))
    (should (equal '("/tmp/file.txt")
                   (plist-get args :exact-allowed-paths)))))

(mevedel-deftest mevedel-permission--invocation-preflight-reuse ()
  ,test
  (test)
  :doc "extracts specifiers and collects rule buckets once per invocation"
  (let* ((getter-calls 0)
         (bucket-calls 0)
         (boundary-calls 0)
         (original-collect (symbol-function
                            'mevedel-permission-rules-collect-buckets))
         (original-boundary (symbol-function
                             'mevedel-permission-rules-path-in-allowed-roots-p))
         (tool (mevedel-tool--create
                :name "Read"
                :read-only-p t
                :get-path (lambda (args)
                            (cl-incf getter-calls)
                            (plist-get args :path))))
         context)
    (cl-letf (((symbol-function 'mevedel-permission-rules-collect-buckets)
               (lambda (&rest buckets)
                 (cl-incf bucket-calls)
                 (apply original-collect buckets)))
              ((symbol-function
                'mevedel-permission-rules-path-in-allowed-roots-p)
               (lambda (path roots)
                 (cl-incf boundary-calls)
                 (funcall original-boundary path roots))))
      (setq context
            (mevedel-permission--invocation-context
             :tool tool
             :args '(:path "/project/file.el")
             :workspace-root "/project"
             :mode 'ask))
      (should
       (eq 'allow
           (apply #'mevedel-check-permission
                  "Read"
                  (mevedel-permission--checker-args context)))))
    (should (= 1 getter-calls))
    (should (= 1 bucket-calls))
    (should (= 1 boundary-calls))))


(mevedel-deftest mevedel-check-permission/bucket-precedence ()
  ,test
  (test)
  :doc "session deny beats invocation allow (pass 1 absolute)"
  (let ((mevedel-permission-rules nil))
    (should (eq 'deny
                (mevedel-check-permission
                 "Bash"
                 :pattern "rm /tmp/foo"
                 :invocation-rules '(("Bash" :action allow))
                 :session-rules
                 '(("Bash" :pattern "rm *" :action deny))))))

  :doc "innermost (invocation) allow beats session ask"
  (let ((mevedel-permission-rules nil))
    (should (eq 'allow
                (mevedel-check-permission
                 "Bash"
                 :pattern "echo hi"
                 :invocation-rules
                 '(("Bash" :pattern "echo *" :action allow))
                 :session-rules '(("Bash" :action ask))))))

  :doc "request rules outrank session rules"
  (let ((mevedel-permission-rules nil))
    (should (eq 'allow
                (mevedel-check-permission
                 "Bash"
                 :pattern "ls"
                 :request-rules '(("Bash" :pattern "ls" :action allow))
                 :session-rules '(("Bash" :pattern "ls" :action ask))))))

  :doc "no skill rules -> session rules apply normally"
  (let ((mevedel-permission-rules nil))
    (should (eq 'ask
                (mevedel-check-permission
                 "Bash"
                 :pattern "rm /tmp/foo"
                 :session-rules
                 '(("Bash" :pattern "rm *" :action ask)))))))

(mevedel-deftest mevedel-check-permission/workspace-root ()
  ,test
  (test)
  :doc "workspace root itself is treated as inside the workspace"
  (let* ((root (file-name-as-directory
                (make-temp-file "mevedel-workspace-root-" t)))
         (root-without-slash (directory-file-name root))
         (mock-tool (mevedel-tool--create :name "Grep" :read-only-p t))
         (mevedel-permission-rules nil)
         (mevedel-protected-paths nil))
    (unwind-protect
        (should (eq 'allow
                    (mevedel-check-permission
                     "Grep"
                     :tool-struct mock-tool
                     :path root-without-slash
                     :workspace-root root
                     :mode 'ask)))
      (delete-directory root t)))

  :doc "workspace children are still treated as inside the workspace"
  (let* ((root (file-name-as-directory
                (make-temp-file "mevedel-workspace-child-" t)))
         (child (file-name-concat root "file.el"))
         (mock-tool (mevedel-tool--create :name "Read" :read-only-p t))
         (mevedel-permission-rules nil)
         (mevedel-protected-paths nil))
    (unwind-protect
        (should (eq 'allow
                    (mevedel-check-permission
                     "Read"
                     :tool-struct mock-tool
                     :path child
                     :workspace-root root
                     :mode 'ask)))
      (delete-directory root t)))

  :doc "additional allowed roots are treated as inside the workspace boundary"
  (let* ((root (file-name-as-directory
                (make-temp-file "mevedel-workspace-allowed-root-" t)))
         (extra (file-name-as-directory
                 (make-temp-file "mevedel-workspace-extra-root-" t)))
         (child (file-name-concat extra "file.el"))
         (mock-tool (mevedel-tool--create :name "Read" :read-only-p t))
         (mevedel-permission-rules nil)
         (mevedel-protected-paths nil))
    (unwind-protect
        (should (eq 'allow
                    (mevedel-check-permission
                     "Read"
                     :tool-struct mock-tool
                     :path child
                     :workspace-root root
                     :allowed-roots (list root extra)
                     :mode 'ask)))
      (delete-directory root t)
      (delete-directory extra t)))

  :doc "sibling directories are outside the workspace"
  (let* ((parent (make-temp-file "mevedel-workspace-parent-" t))
         (root (file-name-as-directory
                (file-name-concat parent "project")))
         (sibling (file-name-concat parent "project-other"))
         (mevedel-permission-rules nil)
         (mevedel-protected-paths nil))
    (unwind-protect
        (progn
          (make-directory root)
          (make-directory sibling)
          (should (eq 'ask
                      (mevedel-check-permission
                       "Grep"
                       :path sibling
                       :workspace-root root
                       :mode 'ask))))
      (delete-directory parent t))))

(provide 'test-mevedel-permissions)
;;; test-mevedel-permissions.el ends here
