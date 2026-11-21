;;; test-niwa-permissions.el --- Tests for permission system -*- lexical-binding: t -*-

;;; Commentary:

;; Tests permission preflight, decision composition, and prompt-result dispatch.

;;; Code:

(require 'niwa-permissions)
(require 'niwa-execution-target)
(require 'niwa-permission-mode)
(require 'niwa-permission-persistence)
(require 'niwa-permission-rules)
(require 'niwa-session-control-fs)
(require 'niwa-session-durability)
(require 'niwa-structs)
(require 'niwa-tool-registry)
(require 'niwa-agents)
(require 'niwa-reminders)
(require 'niwa-plan)
(require 'niwa-plan-mode)
(require 'niwa-workspace)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name
               load-file-name
               byte-compile-current-file))
          "helpers"))


;;
;;; Full decision chain

(niwa-deftest niwa-permission--preflight ()
  ,test
  (test)
  :doc "omitted owners use ambient Plan stamps but explicit nil owners do not"
  (let* ((niwa-permission-rules nil)
         (session (niwa-session--create)))
    (dolist (owner '(request invocation))
      (let ((niwa--current-request
             (and (eq owner 'request)
                  (niwa-request--create :session session :plan-read-only t)))
            (niwa--agent-invocation
             (and (eq owner 'invocation)
                  (niwa-agent-invocation--create
                   :path "/root/ambient" :parent-session session
                   :plan-read-only t))))
        (should (eq 'plan-mode
                    (plist-get
                     (plist-get (niwa-permission--preflight
                                 "Eval" :session session :mode 'full-auto)
                                :early-decision)
                     :via)))
        (should-not
         (plist-get (niwa-permission--preflight
                     "Eval" :session session :mode 'full-auto
                     :request nil :invocation nil)
                    :early-decision)))))

  :doc "normalizes extracted specifiers and shared decision facts"
  (let* ((get-path-calls 0)
         (tool (niwa-tool--create
                :name "Edit"
                :get-path (lambda (input)
                            (cl-incf get-path-calls)
                            (plist-get input :file_path))
                :read-only-p nil))
         (context
          (let ((niwa-permission-rules nil)
                (niwa-protected-paths nil))
            (niwa-permission--preflight
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
  (let ((niwa-permission-rules nil)
        (niwa-protected-paths nil))
    (let* ((context
            (niwa-permission--preflight
             "Edit"
             :session-rules '(("Edit" :action deny))
             :mode 'full-auto))
           (decision (plist-get context :early-decision)))
      (should (eq 'deny
                  (niwa-permission-decision-raw-outcome decision)))
      (should (eq 'deny-rule (plist-get decision :via)))
      (should (eq :session (plist-get decision :bucket)))))

  :doc "allows read-only access to installed manuals without granting edits"
  (let* ((niwa-tool-registry--source-dir "/package/")
         (path "/package/docs/ptc-dialect.md")
         (read-context
          (niwa-permission--preflight
           "Read"
           :tool-struct (niwa-tool--create :name "Read" :read-only-p t)
           :path path :allowed-roots '("/project")))
         (edit-context
          (niwa-permission--preflight
           "Edit"
           :tool-struct (niwa-tool--create :name "Edit" :read-only-p nil)
           :path path :allowed-roots '("/project"))))
    (should-not (plist-get read-context :workspace-boundary-p))
    (should (member "/package/" (plist-get read-context :allowed-roots)))
    (should (plist-get edit-context :workspace-boundary-p))
    (should-not (member "/package/" (plist-get edit-context :allowed-roots))))

  :doc "derives protected target-home facts from the owning session"
  (let* ((target (niwa-execution-target-create
                  "/ssh:user@host:/srv/project/"))
         (session (niwa-session--create
                   :name "remote" :execution-target target))
         (niwa-protected-paths '(("~/.ssh/**" . inaccessible))))
    (setf (niwa-execution-target-environment target)
          '(("HOME" . "/home/user")))
    (should
     (plist-get
      (niwa-permission--preflight
       "Read" :path "/ssh:user@host:/home/user/.ssh/id_rsa"
       :session session :mode 'full-auto)
      :protected-path-p))))

(niwa-deftest niwa-check-permission-async-with-metadata ()
  ,test
  (test)
  :doc "bare async slot results retain the tool's policy provenance"
  (let ((niwa-permission-rules nil)
        (niwa--current-request nil)
        (niwa--agent-invocation nil))
    (dolist (case '(("Monitor" . bash-classifier)
                    ("Bash" . bash-classifier)
                    ("Eval" . eval-policy)
                    ("Custom" . tool-slot)))
      (let* ((tool (niwa-tool--create
                    :name (car case)
                    :check-permission-async
                    (lambda (_tool _input cont) (funcall cont 'deny))))
             decision)
        (niwa-check-permission-async-with-metadata
         (car case) (lambda (result) (setq decision result))
         :tool-struct tool :mode 'full-auto)
        (should (eq 'deny (niwa-permission-decision-raw-outcome decision)))
        (should (eq (cdr case) (plist-get decision :via))))))

  :doc "sync and async entry points return identical non-async decisions"
  (let* ((read-tool (niwa-tool--create :name "Read" :read-only-p t))
         (edit-tool (niwa-tool--create :name "Edit" :read-only-p nil))
         (deny-tool
          (niwa-tool--create
           :name "Custom"
           :read-only-p nil
           :check-permission
           (lambda (_tool _input)
             (signal 'niwa-permission-denied '("custom reason")))))
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
    (let ((niwa-permission-rules nil)
          (niwa-protected-paths '(("**/.git/**" . read-only))))
      (dolist (case cases)
        (let* ((tool-name (car case))
               (args (cdr case))
               (sync (apply #'niwa-check-permission-with-metadata
                            tool-name args))
               async)
          (apply #'niwa-check-permission-async-with-metadata
                 tool-name (lambda (decision) (setq async decision)) args)
          (should (equal sync async))
          ;; A slot's denial keeps its reason.
          (when (equal tool-name "Custom")
            (should (equal '(deny . "custom reason")
                           (niwa-permission-decision-raw-outcome sync))))))))

  :doc "an absolute deny is resolved once and skips both tool slots"
  (let* ((deny-checks 0)
         (sync-slot-called nil)
         (async-slot-called nil)
         (original (symbol-function
                    'niwa-permission-rules-first-deny-bucket))
         (tool
          (niwa-tool--create
           :name "Custom"
           :read-only-p nil
           :check-permission
           (lambda (_tool _input) (setq sync-slot-called t) 'allow)
           :check-permission-async
           (lambda (_tool _input cont)
             (setq async-slot-called t)
             (funcall cont 'allow))))
         decision)
    (let ((niwa-permission-rules nil)
          (niwa-protected-paths '(("**/.git/**" . read-only))))
      (cl-letf (((symbol-function 'niwa-permission-rules-first-deny-bucket)
                 (lambda (&rest args)
                   (cl-incf deny-checks)
                   (apply original args))))
        (niwa-check-permission-async-with-metadata
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

(niwa-deftest niwa-check-permission ()
  ,test
  (test)
  :doc "deny rule overrides everything"
  (let ((niwa-permission-rules '(("Edit" :action deny)))
        (niwa-protected-paths nil))
    (should (eq (niwa-check-permission "Edit" :mode 'full-auto) 'deny)))
  :doc "full-auto authorizes protected paths without resource grants"
  (let ((niwa-permission-rules nil)
        (niwa-protected-paths '(("**/.git/**" . read-only))))
    (should (eq (niwa-check-permission "Edit"
                  :path "/repo/.git/config"
                  :mode 'full-auto)
                'allow)))
  :doc "full-auto ignores ordinary ask rules and tool-slot asks"
  (let ((niwa-permission-rules '(("Read" :action ask)))
        (niwa-protected-paths '(("**/.ssh/**" . inaccessible)))
        (tool (niwa-tool--create
               :name "Read" :read-only-p t
               :check-permission (lambda (_tool _input) 'ask))))
    (should (eq 'allow
                (niwa-check-permission
                 "Read" :tool-struct tool :mode 'full-auto
                 :path "/outside/.ssh/key" :workspace-root "/project"))))
  :doc "edits reads share the sandbox boundary while protected writes ask"
  (let ((niwa-permission-rules nil)
        (niwa-protected-paths '(("**/.git/**" . read-only)
                                   ("**/.ssh/**" . inaccessible)))
        (read-tool (niwa-tool--create :name "Read" :read-only-p t))
        (edit-tool (niwa-tool--create :name "Edit" :groups '(edit))))
    (dolist (path '("/outside/manual.el" "/project/.git/config"))
      (should (eq 'allow
                  (niwa-check-permission
                   "Read" :tool-struct read-tool :mode 'edits
                   :path path :workspace-root "/project"))))
    (should (eq 'ask
                (niwa-check-permission
                 "Read" :tool-struct read-tool :mode 'edits
                 :path "/outside/.ssh/key" :workspace-root "/project")))
    (dolist (path '("/outside/manual.el" "/project/.git/config"))
      (should (eq 'ask
                  (niwa-check-permission
                   "Edit" :tool-struct edit-tool :mode 'edits
                   :path path :workspace-root "/project"))))
    (should (eq 'deny
                (niwa-check-permission
                 "Read" :tool-struct read-tool :mode 'edits
                 :path "/outside/manual.el" :workspace-root "/project"
                 :session-rules '(("Read" :path "/outside/**" :action deny)))))
    (should (eq 'ask
                (niwa-check-permission
                 "Read" :tool-struct read-tool :mode 'edits
                 :path "/outside/manual.el" :workspace-root "/project"
                 :session-rules '(("Read" :action ask))))))
  :doc "tool check-permission returning allow is respected"
  (let ((niwa-permission-rules nil)
        (niwa-protected-paths nil)
        (mock-tool (niwa-tool--create
                    :name "MockTool"
                    :check-permission (lambda (_ts _input) 'allow)
                    :read-only-p nil)))
    (should (eq (niwa-check-permission "MockTool"
                  :tool-struct mock-tool
                  :mode 'ask)
                'allow)))
  :doc "one-shot policy overrides a generic tool-slot allow"
  (let ((niwa-permission-rules nil)
        (niwa-protected-paths nil)
        (mock-tool (niwa-tool--create
                    :name "MockTool"
                    :check-permission (lambda (_ts _input) 'allow)
                    :read-only-p nil)))
    (should
     (eq 'ask
         (niwa-check-permission
          "MockTool" :tool-struct mock-tool :mode 'edits
          :one-shot-mutations-p t))))
  :doc "one-shot policy keeps emergency controls automatic"
  (let ((niwa-permission-rules nil)
        (niwa-protected-paths nil)
        (control-tool (niwa-tool--create
                       :name "StopExecution"
                       :check-permission
                       (lambda (_ts _input)
                         '(:outcome allow :raw-outcome allow
                           :via execution-control))
                       :read-only-p nil)))
    (should
     (eq 'allow
         (niwa-check-permission
          "StopExecution" :tool-struct control-tool :mode 'full-auto
          :one-shot-mutations-p t)))
    (should
     (eq 'deny
         (niwa-check-permission
          "StopExecution" :tool-struct control-tool :mode 'full-auto
          :session-rules '(("StopExecution" :action deny))
          :one-shot-mutations-p t))))
  :doc "tool check-permission returning deny is respected"
  (let ((niwa-permission-rules nil)
        (niwa-protected-paths nil)
        (mock-tool (niwa-tool--create
                    :name "MockTool"
                    :check-permission (lambda (_ts _input) 'deny)
                    :read-only-p nil)))
    (should (eq (niwa-check-permission "MockTool"
                  :tool-struct mock-tool
                  :mode 'full-auto)
                'deny)))
  :doc "tool check-permission returning nil falls through to allow rule"
  (let ((niwa-permission-rules '(("MockTool" :action allow)))
        (niwa-protected-paths nil)
        (mock-tool (niwa-tool--create
                    :name "MockTool"
                    :check-permission (lambda (_ts _input) nil)
                    :read-only-p nil)))
    (should (eq (niwa-check-permission "MockTool"
                  :tool-struct mock-tool
                  :mode 'ask)
                'allow)))
  :doc "allow rule allows when no deny or protection"
  (let ((niwa-permission-rules '(("Read" :action allow)))
        (niwa-protected-paths nil))
    (should (eq (niwa-check-permission "Read" :mode 'ask) 'allow)))
  :doc "mode decision when no rules match - default asks for non-read-only"
  (let ((niwa-permission-rules nil)
        (niwa-protected-paths nil)
        (mock-tool (niwa-tool--create :name "Edit" :read-only-p nil)))
    (should (eq (niwa-check-permission "Edit"
                  :tool-struct mock-tool
                  :mode 'ask)
                'ask)))
  :doc "reviewed edits use review inside roots and retain outside path rules"
  (let ((niwa-protected-paths nil)
        (tool (niwa-tool--create
               :name "ApplyPatch" :groups '(edit reviewed-edit))))
    (let ((niwa-permission-rules nil))
      (should (eq (niwa-check-permission
                   "ApplyPatch" :tool-struct tool :path "/project/a"
                   :mode 'ask :workspace-root "/project")
                  'allow))
      (should (eq (niwa-check-permission
                   "ApplyPatch" :tool-struct tool :path "/outside/a"
                   :mode 'ask :workspace-root "/project")
                  'ask)))
    (let ((niwa-permission-rules
           '(("ApplyPatch" :path "/outside/a" :action allow))))
      (should (eq (niwa-check-permission
                   "ApplyPatch" :tool-struct tool :path "/outside/a"
                   :mode 'ask :workspace-root "/project")
                  'allow)))
    (let ((niwa-permission-rules
           '(("ApplyPatch" :action ask))))
      (dolist (mode '(ask edits))
        (should (eq (niwa-check-permission
                     "ApplyPatch" :tool-struct tool :path "/project/a"
                     :mode mode :workspace-root "/project")
                    'ask)))))
  :doc "full-auto reviewed edits bypass resource prompts but retain hard denies"
  (let ((niwa-permission-rules nil)
        (niwa-protected-paths nil)
        (tool (niwa-tool--create
               :name "ApplyPatch" :groups '(edit reviewed-edit))))
    (should
     (eq 'allow
         (niwa-check-permission
          "ApplyPatch" :tool-struct tool :path "/project/a"
          :mode 'full-auto :workspace-root "/project"
          :one-shot-mutations-p t)))
    (should
     (eq 'allow
         (niwa-check-permission
          "ApplyPatch" :tool-struct tool :path "/outside/a"
          :mode 'full-auto :workspace-root "/project"
          :one-shot-mutations-p t)))
    (should
     (eq 'deny
         (niwa-check-permission
          "ApplyPatch" :tool-struct tool :path "/project/a"
          :session-rules '(("ApplyPatch" :action deny))
          :mode 'full-auto :workspace-root "/project"
          :one-shot-mutations-p t))))
  :doc "Plan mode denies edits and Eval across modes and explicit allows"
  (let* ((niwa-permission-rules nil)
         (niwa-protected-paths nil)
         (session (niwa-session--create :name "plan" :plan-mode t)))
    (dolist (mode '(ask edits full-auto))
      (dolist (name '("Write" "Edit" "MkDir" "Eval"))
        (let ((tool (niwa-tool--create
                     :name name :read-only-p nil
                     :groups (if (equal name "Eval") '(eval) '(edit)))))
          (should
           (eq
            (niwa-check-permission
             name
             :tool-struct tool
             :session session
             :session-rules `((,name :action allow))
             :mode mode)
            'deny))))))
  :doc "Plan boundary follows retained agents across modes and explicit allows"
  (let* ((niwa-permission-rules nil)
         (niwa-protected-paths nil)
         (session (niwa-session--create :name "plan" :plan-mode t))
         (invocation (niwa-agent-invocation--create
                      :parent-session session)))
    (with-temp-buffer
      (setq-local niwa--agent-invocation invocation)
      (dolist (mode '(ask edits full-auto))
        (dolist (name '("Write" "Edit" "MkDir" "Eval"))
          (let ((tool (niwa-tool--create
                       :name name :read-only-p nil
                       :groups (if (equal name "Eval") '(eval) '(edit)))))
            (should
             (eq
              (niwa-check-permission
               name
               :tool-struct tool
               :session-rules `((,name :action allow))
               :mode mode)
              'deny)))))))
  :doc "Plan allows only a prepared ApplyPatch over session-owned work descendants"
  (let* ((niwa-permission-rules nil)
         (niwa-protected-paths nil)
         (session (niwa-session--create :name "plan" :plan-mode t))
         (tool (niwa-tool--create
                :name "ApplyPatch" :read-only-p nil
                :groups '(edit reviewed-edit))))
    (dolist (mode '(ask edits full-auto))
      (should
       (eq 'allow
           (niwa-check-permission
            "ApplyPatch" :tool-struct tool :session session :mode mode
            :patch-session-only-p t)))
      (should
       (eq 'deny
           (niwa-check-permission
            "ApplyPatch" :tool-struct tool :session session :mode mode
            :patch-session-only-p nil))))
    (should
     (eq 'deny
         (niwa-check-permission
          "ApplyPatch" :tool-struct tool :session session :mode 'full-auto
          :patch-session-only-p t
          :session-rules '(("ApplyPatch" :action deny))))))
  :doc "read-only tool allowed in ask mode"
  (let ((niwa-permission-rules nil)
        (niwa-protected-paths nil)
        (mock-tool (niwa-tool--create :name "Read" :read-only-p t)))
    (should (eq (niwa-check-permission "Read"
                  :tool-struct mock-tool
                  :mode 'ask)
                'allow)))
  :doc "full-auto authorizes one-shot mutations despite inherited ask rules"
  (let ((niwa-permission-rules nil)
        (niwa-protected-paths nil)
        (edit-tool (niwa-tool--create :name "Edit" :read-only-p nil))
        (read-tool (niwa-tool--create :name "Read" :read-only-p t)))
    (should (eq (niwa-check-permission
                 "Edit" :tool-struct edit-tool
                 :session-rules '(("Edit" :action ask))
                 :mode 'full-auto :one-shot-mutations-p t)
                'allow))
    (should (eq (niwa-check-permission
                 "Read" :tool-struct read-tool
                 :session-rules '(("Read" :action allow))
                 :mode 'full-auto :one-shot-mutations-p t)
                'allow)))
  :doc "session rules work alongside defcustom rules"
  (let ((niwa-permission-rules '(("Edit" :action ask)))
        (niwa-protected-paths nil)
        (session-rules '(("Edit" :path "/allowed/*" :action allow))))
    (should (eq (niwa-check-permission "Edit"
                  :path "/allowed/file.el"
                  :session-rules session-rules
                  :mode 'ask)
                'allow)))
  :doc "unknown tool (no struct) defaults to ask"
  (let ((niwa-permission-rules nil)
        (niwa-protected-paths nil))
    (should (eq (niwa-check-permission "UnknownTool" :mode 'ask) 'ask)))
  :doc "get-path extracts path from content"
  (let ((niwa-permission-rules nil)
        (niwa-protected-paths '(("**/.git/**" . read-only)))
        (mock-tool (niwa-tool--create
                    :name "Edit"
                    :get-path (lambda (input) (plist-get input :file_path))
                    :read-only-p nil)))
    (should (eq (niwa-check-permission "Edit"
                  :tool-struct mock-tool
                  :content '(:file_path "/repo/.git/config")
                  :mode 'edits)
                'ask)))
  :doc "path inside workspace root is implicitly allowed"
  (let ((niwa-permission-rules nil)
        (niwa-protected-paths nil)
        (mock-tool (niwa-tool--create :name "Read" :read-only-p t)))
    (should (eq (niwa-check-permission "Read"
                  :tool-struct mock-tool
                  :path "/project/src/file.el"
                  :mode 'ask
                  :workspace-root "/project")
                'allow)))
  :doc "path outside workspace root asks even for read-only tools"
  (let ((niwa-permission-rules nil)
        (niwa-protected-paths nil)
        (mock-tool (niwa-tool--create :name "Read" :read-only-p t)))
    (should (eq (niwa-check-permission "Read"
                  :tool-struct mock-tool
                  :path "/etc/passwd"
                  :mode 'ask
                  :workspace-root "/project")
                'ask)))
  :doc "exact read resource grant allows Read outside workspace"
  (let ((niwa-permission-rules nil)
        (niwa-protected-paths nil)
        (mock-tool (niwa-tool--create :name "Read" :read-only-p t)))
    (should (eq (niwa-check-permission
                 "Read" :tool-struct mock-tool
                 :path "/etc/passwd" :mode 'ask
                 :workspace-root "/project"
                 :resource-grants '((:path "/etc/passwd" :access read)))
                'allow)))
  :doc "recursive resource grant allows descendant reads outside workspace"
  (let ((niwa-permission-rules nil)
        (niwa-protected-paths nil)
        (mock-tool (niwa-tool--create :name "Read" :read-only-p t))
        (grants '((:path "/usr/share/emacs" :access read :recursive t))))
    (should (eq (niwa-check-permission
                 "Read" :tool-struct mock-tool
                 :path "/usr/share/emacs/31.1/lisp/simple.el" :mode 'ask
                 :workspace-root "/project"
                 :resource-grants grants)
                'allow))
    (should (eq (niwa-check-permission
                 "Read" :tool-struct mock-tool
                 :path "/usr/share/emacs-old/file" :mode 'ask
                 :workspace-root "/project"
                 :resource-grants grants)
                'ask)))
  :doc "explicit deny and protected paths stay final over a recursive grant"
  (let ((mock-tool (niwa-tool--create :name "Read" :read-only-p t))
        (grants '((:path "/outside" :access write :recursive t))))
    (let ((niwa-permission-rules
           '(("Read" :path "/outside/**" :action deny)))
          (niwa-protected-paths nil))
      (should (eq (niwa-check-permission
                   "Read" :tool-struct mock-tool
                   :path "/outside/denied.el" :mode 'ask
                   :workspace-root "/project"
                   :resource-grants grants)
                  'deny)))
    ;; A recursive grant covering a protected path satisfies its gate,
    ;; exactly as an exact grant on the protected path does today.
    (let ((niwa-permission-rules nil)
          (niwa-protected-paths '(("/outside/.ssh/**" . inaccessible))))
      (should (eq (niwa-check-permission
                   "Read" :tool-struct mock-tool
                   :path "/outside/.ssh/config" :mode 'ask
                   :workspace-root "/project"
                   :resource-grants nil)
                  'ask))
      (should (eq (niwa-check-permission
                   "Read" :tool-struct mock-tool
                   :path "/outside/.ssh/config" :mode 'ask
                   :workspace-root "/project"
                   :resource-grants grants)
                  'allow))))
  :doc "active Goal request grants only exact accepted-plan reads"
  (let* ((niwa-permission-rules nil)
         (niwa-protected-paths nil)
         (request (niwa-request--create
                   :goal-plan-read-path "/sessions/accepted-plan.md"))
         (read (niwa-tool--create :name "Read" :read-only-p t))
         (write (niwa-tool--create :name "Write" :read-only-p nil))
         (check
          (lambda (tool path)
            (let ((context
                   (niwa-permission--invocation-context
                    :tool tool :request request :path path
                    :workspace-root "/project" :mode 'ask)))
              (apply #'niwa-check-permission
                     (niwa-tool-name tool)
                     (niwa-permission--checker-args context))))))
    (should (eq 'allow
                (funcall check read "/sessions/accepted-plan.md")))
    (should (eq 'ask
                (funcall check read "/sessions/sibling.md")))
    (should (eq 'ask
                (funcall check write "/sessions/accepted-plan.md"))))
  :doc "resource grant has distinct decision metadata"
  (let* ((niwa-permission-rules nil)
         (niwa-protected-paths nil)
         (mock-tool (niwa-tool--create :name "Read" :read-only-p t))
         (decision
          (niwa-check-permission-with-metadata
           "Read" :tool-struct mock-tool
           :path "/etc/passwd" :mode 'ask
           :workspace-root "/project"
           :resource-grants '((:path "/etc/passwd" :access read)))))
    (should (eq 'allow
                (niwa-permission-decision-raw-outcome decision)))
    (should (eq 'resource-grant (plist-get decision :via))))
  :doc "read resource grant does not allow Write"
  (let ((niwa-permission-rules nil)
        (niwa-protected-paths nil)
        (mock-tool (niwa-tool--create :name "Write" :read-only-p nil)))
    (should (eq (niwa-check-permission
                 "Write" :tool-struct mock-tool
                 :path "/etc/passwd" :mode 'ask
                 :workspace-root "/project"
                 :resource-grants '((:path "/etc/passwd" :access read)))
                'ask)))
  :doc "write resource grants satisfy only the resource gate"
  (let ((niwa-permission-rules nil)
        (niwa-protected-paths nil)
        (mock-tool (niwa-tool--create
                    :name "Write" :groups '(edit) :read-only-p nil))
        (grants '((:path "/outside/target.el" :access write))))
    (should (eq (niwa-check-permission
                 "Write" :tool-struct mock-tool
                 :path "/outside/target.el" :mode 'ask
                 :workspace-root "/project" :resource-grants grants)
                'ask))
    (should (eq (niwa-check-permission
                 "Write" :tool-struct mock-tool
                 :path "/outside/target.el" :mode 'edits
                 :workspace-root "/project" :resource-grants grants)
                'allow))
    (should (eq (niwa-check-permission
                 "Write" :tool-struct mock-tool
                 :path "/outside/sibling.el" :mode 'edits
                 :workspace-root "/project" :resource-grants grants)
                'ask)))
  :doc "resource grant does not override a command-specific ask"
  (let ((niwa-permission-rules nil)
        (niwa-protected-paths nil)
        (mock-tool
         (niwa-tool--create
          :name "Bash" :read-only-p nil
          :check-permission (lambda (_tool _content) 'ask))))
    (should (eq (niwa-check-permission
                 "Bash" :tool-struct mock-tool
                 :content '(:command "curl https://example.com")
                 :path "/outside/file" :mode 'ask
                 :workspace-root "/project"
                 :resource-grants '((:path "/outside/file" :access write)))
                'ask)))
  :doc "command-specific allow does not override missing resource authority"
  (let ((niwa-permission-rules nil)
        (niwa-protected-paths nil)
        (mock-tool
         (niwa-tool--create
          :name "Bash" :read-only-p nil
          :check-permission (lambda (_tool _content) 'allow))))
    (should (eq (niwa-check-permission
                 "Bash" :tool-struct mock-tool
                 :content '(:command "cat /outside/file")
                 :path "/outside/file" :mode 'ask
                 :workspace-root "/project")
                'ask))
    (should (eq (niwa-check-permission
                 "Bash" :tool-struct mock-tool
                 :content '(:command "cat /outside/file")
                 :path "/outside/file" :mode 'ask
                 :workspace-root "/project"
                 :resource-grants '((:path "/outside/file" :access write)))
                'allow)))
  :doc "resource grant does not authorize a command slot that declines"
  (let ((niwa-permission-rules nil)
        (niwa-protected-paths nil)
        (mock-tool
         (niwa-tool--create
          :name "Bash" :read-only-p nil
          :check-permission (lambda (_tool _content) nil))))
    (should (eq (niwa-check-permission
                 "Bash" :tool-struct mock-tool
                 :content '(:command "unknown")
                 :path "/outside/file" :mode 'ask
                 :workspace-root "/project"
                 :resource-grants '((:path "/outside/file" :access write)))
                'ask)))
  :doc "explicit allow rule overrides workspace boundary"
  (let ((niwa-permission-rules '(("Read" :path "/etc/*" :action allow)))
        (niwa-protected-paths nil)
        (mock-tool (niwa-tool--create :name "Read" :read-only-p t)))
    (should (eq (niwa-check-permission "Read"
                  :tool-struct mock-tool
                  :path "/etc/hosts"
                  :mode 'ask
                  :workspace-root "/project")
                'allow)))
  :doc "wildcard allow rule covers paths outside workspace"
  (let ((niwa-permission-rules '(("*" :path "/shared/**" :action allow)))
        (niwa-protected-paths nil))
    (should (eq (niwa-check-permission "Edit"
                  :path "/shared/lib/util.el"
                  :mode 'ask
                  :workspace-root "/project")
                'allow)))
  :doc "no workspace root falls through to mode for non-path tools"
  (let ((niwa-permission-rules nil)
        (niwa-protected-paths nil)
        (mock-tool (niwa-tool--create :name "Read" :read-only-p t)))
    (should (eq (niwa-check-permission "Read"
                  :tool-struct mock-tool
                  :mode 'ask)
                'allow)))
  :doc "no workspace root with path falls through to ask"
  (let ((niwa-permission-rules nil)
        (niwa-protected-paths nil))
    (should (eq (niwa-check-permission "Read"
                  :path "/some/file.el"
                  :mode 'ask)
                'ask)))
  :doc "get-pattern extracts command string for pattern rule match"
  (let ((niwa-permission-rules
         '(("Bash" :pattern "echo*" :action allow)))
        (niwa-protected-paths nil)
        (mock-tool (niwa-tool--create
                    :name "Bash"
                    :get-pattern (lambda (input) (plist-get input :command))
                    :read-only-p nil)))
    (should (eq (niwa-check-permission "Bash"
                  :tool-struct mock-tool
                  :content '(:command "echo hello")
                  :mode 'ask)
                'allow)))
  :doc "get-domain extracts host for domain rule match"
  (let ((niwa-permission-rules
         '(("WebFetch" :domain "*.example.com" :action allow)))
        (niwa-protected-paths nil)
        (mock-tool (niwa-tool--create
                    :name "WebFetch"
                    :get-domain (lambda (input) (plist-get input :host))
                    :read-only-p t)))
    (should (eq (niwa-check-permission "WebFetch"
                  :tool-struct mock-tool
                  :content '(:host "api.example.com")
                  :mode 'ask)
                'allow)))
  :doc "get-name extracts name for name rule match"
  (let ((niwa-permission-rules
         '(("Agent" :name "explorer" :action allow)))
        (niwa-protected-paths nil)
        (mock-tool (niwa-tool--create
                    :name "Agent"
                    :get-name (lambda (input) (plist-get input :task_name))
                    :read-only-p nil)))
    (should (eq (niwa-check-permission "Agent"
                  :tool-struct mock-tool
                  :content '(:task_name "explorer")
                  :mode 'ask)
                'allow))))


;;
;;; Session rule storage

(niwa-deftest niwa-permission--add-session-rule ()
  ,test
  (test)
  :doc "adds rule to session"
  (let ((session (niwa-session--create :name "test")))
    (niwa-permission--add-session-rule
     session '("Edit" :action allow))
    (should (equal (niwa-session-permission-rules session)
                   '(("Edit" :action allow)))))
  :doc "adds path-scoped rule"
  (let ((session (niwa-session--create :name "test")))
    (niwa-permission--add-session-rule
     session '("Edit" :path "/foo/*" :action allow))
    (should (equal (niwa-session-permission-rules session)
                   '(("Edit" :path "/foo/*" :action allow)))))
  :doc "appends multiple rules"
  (let ((session (niwa-session--create :name "test")))
    (niwa-permission--add-session-rule
     session '("Read" :action allow))
    (niwa-permission--add-session-rule
     session '("Edit" :action deny))
    (should (= (length (niwa-session-permission-rules session)) 2)))
  :doc "deduplicates exact session rules"
  (let ((session (niwa-session--create :name "test")))
    (niwa-permission--add-session-rule
     session '("Read" :action allow))
    (niwa-permission--add-session-rule
     session '("Read" :action allow))
    (niwa-permission--add-session-rule
     session '("Edit" :path "/foo/*" :action allow))
    (niwa-permission--add-session-rule
     session '("Edit" :path "/foo/*" :action allow))
    (niwa-permission--add-session-rule
     session '("Bash" :pattern "git diff:*" :action allow))
    (niwa-permission--add-session-rule
     session '("Bash" :pattern "git diff:*" :action allow))
    (should (equal (niwa-session-permission-rules session)
                   '(("Read" :action allow)
                     ("Edit" :path "/foo/*" :action allow)
                     ("Bash" :pattern "git diff:*" :action allow)))))
  :doc "preserves distinct session rules"
  (let ((session (niwa-session--create :name "test")))
    (niwa-permission--add-session-rule
     session '("Bash" :pattern "git diff:*" :action allow))
    (niwa-permission--add-session-rule
     session '("Bash" :pattern "git diff:*" :action deny))
    (niwa-permission--add-session-rule
     session '("Bash" :pattern "git status:*" :action allow))
    (should (equal (niwa-session-permission-rules session)
                   '(("Bash" :pattern "git diff:*" :action allow)
                     ("Bash" :pattern "git diff:*" :action deny)
                     ("Bash" :pattern "git status:*" :action allow)))))

  :doc "writes through to the same struct shared by aliases (by-reference)"
  ;; Pins the sub-agent permission-propagation contract: agent buffers
  ;; carry the parent session struct buffer-locally by reference, so a
  ;; rule recorded inside any agent that resolves to the same struct
  ;; appears on the parent's slot immediately.
  (let* ((parent-session (niwa-session--create :name "parent"))
         (sub-agent-session-alias parent-session))
    (niwa-permission--add-session-rule
     sub-agent-session-alias '("Bash" :pattern "ls" :action allow))
    (should (equal (niwa-session-permission-rules parent-session)
                   '(("Bash" :pattern "ls" :action allow))))
    (should (eq (niwa-session-permission-rules parent-session)
                (niwa-session-permission-rules sub-agent-session-alias)))))


(niwa-deftest niwa-permission-remove-session-resource-grant ()
  ,test
  (test)
  :doc "revocation restores the underlying protected-path decision"
  (let* ((path "/repo/.git/config")
         (session (niwa-session--create :name "test"))
         (tool (niwa-tool--create :name "Read" :read-only-p t))
         (niwa-permission-rules nil)
         (niwa-protected-paths '(("**/.git/**" . read-only))))
    (niwa-permission-add-session-resource-grant session path 'read)
    (should (eq 'allow
                (niwa-check-permission
                 "Read" :tool-struct tool :path path :mode 'ask
                 :workspace-root "/repo"
                 :resource-grants
                 (niwa-session-resource-grants session))))
    (niwa-permission-remove-session-resource-grant session path 'read)
    (should-not (niwa-session-resource-grants session))
    (should (eq 'ask
                (niwa-check-permission
                 "Read" :tool-struct tool :path path :mode 'ask
                 :workspace-root "/repo"
                 :resource-grants
                 (niwa-session-resource-grants session)))))
  :doc "revocation does not mutate another session's shared list"
  (let* ((first (list :path (expand-file-name
                             "first" temporary-file-directory)
                      :access 'read))
         (second (list :path (expand-file-name
                              "second" temporary-file-directory)
                       :access 'read))
         (shared (list first second))
         (session (niwa-session--create
                   :name "child" :resource-grants shared))
         (other (niwa-session--create
                 :name "parent" :resource-grants shared)))
    (niwa-permission-remove-session-resource-grant
     session (plist-get second :path) 'read)
    (should (equal (list first)
                   (niwa-session-resource-grants session)))
    (should (equal (list first second)
                   (niwa-session-resource-grants other)))))

(niwa-deftest niwa-permission-add-session-resource-grant ()
  ,test
  (test)
  :doc "write authority promotes an exact read grant"
  (let* ((path (expand-file-name "outside" temporary-file-directory))
         (session (niwa-session--create :name "test")))
    (niwa-permission-add-session-resource-grant session path 'read)
    (niwa-permission-add-session-resource-grant session path 'write)
    (should
     (equal `((:path ,path :access write))
            (niwa-session-resource-grants session))))
  :doc "recursive grants keep their own identity next to exact ones"
  (let* ((path (expand-file-name "tree" temporary-file-directory))
         (session (niwa-session--create :name "test")))
    (niwa-permission-add-session-resource-grant session path 'read)
    (niwa-permission-add-session-resource-grant session path 'read t)
    (should
     (equal `((:path ,path :access read)
              (:path ,path :access read :recursive t))
            (niwa-session-resource-grants session)))
    (niwa-permission-remove-session-resource-grant session path 'read t)
    (should
     (equal `((:path ,path :access read))
            (niwa-session-resource-grants session)))))


(niwa-deftest niwa-permission-invalidate-target-grants ()
  ,test
  (test)
  :doc "revokes exact session, frozen, and dropped grants but not the workspace store"
  (let* ((tmp-dir (file-name-as-directory
                   (make-temp-file "niwa-test-" t)))
         (niwa-user-dir (file-name-concat tmp-dir "global/"))
         (path (file-name-concat tmp-dir "outside.el"))
         (workspace (niwa-workspace--create
                     :type 'project :id "test" :root tmp-dir
                     :name "test" :file-cache nil))
         (session (niwa-session-create "main" workspace)))
    (unwind-protect
        (with-temp-buffer
          (setq-local niwa--session session
                      niwa-permission--context-frozen-p t
                      niwa-permission--frozen-persistent-rules
                      `(("Bash" :pattern "frozen:*" :network t
                         :file-system ((:path ,path :access read)) :action allow))
                      niwa-permission--frozen-resource-grants
                      `((:path ,path :access read)))
          (setf (niwa-session-permission-rules session)
                `(("Bash" :pattern "git status:*" :network t
                   :file-system ((:path ,path :access read)) :action allow))
                (niwa-session-resource-grants session)
                `((:path ,path :access read))
                (niwa-session-dropped-file-grants session)
                (list path)
                (niwa-session-active-dropped-file-grants session)
                (list path))
          (niwa-permission-persistence-save-rule
           workspace '("Read" :action allow))
          (niwa-permission-persistence-save-resource-grant
           workspace path 'read)
          (should (niwa-permission-invalidate-target-grants session))
          (should-not (niwa-session-resource-grants session))
          (should-not (niwa-session-dropped-file-grants session))
          (should-not
           (niwa-session-active-dropped-file-grants session))
          (should-not niwa-permission--frozen-resource-grants)
          (should (equal '(("Bash" :pattern "frozen:*" :network t :action allow))
                         niwa-permission--frozen-persistent-rules))
          (should
           (equal '(("Bash" :pattern "git status:*" :network t :action allow))
                  (niwa-session-permission-rules session)))
          ;; The workspace store is shared configuration, not authority
          ;; bound to this target's incarnation: it keeps its grants.
          (should
           (equal `((:path ,path :access read))
                  (niwa-permission-persistence-load-resource-grants
                   workspace)))
          (should (equal '(("Read" :action allow))
                         (niwa-permission-persistence-load-rules
                          workspace))))
      (delete-directory tmp-dir t))))


;;
;;; Prompt result dispatch

(niwa-deftest niwa-permission--apply-prompt-result ()
  ,test
  (test)
  :doc "allow-once returns allow without storage"
  (let ((session (niwa-session--create :name "test")))
    (should (eq (niwa-permission--apply-prompt-result
                 'allow-once "Edit" session)
                'allow))
    (should-not (niwa-session-permission-rules session)))
  :doc "allow-session stores rule and returns allow"
  (let ((session (niwa-session--create :name "test")))
    (should (eq (niwa-permission--apply-prompt-result
                 'allow-session "Edit" session)
                'allow))
    (should (= (length (niwa-session-permission-rules session)) 1)))
  :doc "allow-session stores an execution-level-qualified pattern rule"
  (let ((session (niwa-session--create :name "test")))
    (should
     (eq 'allow
         (niwa-permission--apply-prompt-result
          'allow-session "Bash" session nil nil
          :spec-key :pattern :spec-value "emacs --batch *"
          :sandbox-permissions 'require-escalated)))
    (should
     (equal
      '(("Bash" :pattern "emacs --batch *"
                :sandbox-permissions require-escalated
                :action allow))
      (niwa-session-permission-rules session))))
  :doc "allow-session stores network authority independently"
  (let ((session (niwa-session--create :name "test")))
    (niwa-permission--apply-prompt-result
     'allow-session "Bash" session nil nil
     :spec-key :pattern :spec-value "npx test")
    (niwa-permission--apply-prompt-result
     'allow-session "Bash" session nil nil
     :spec-key :pattern :spec-value "npx test" :network t)
    (should
     (equal
      '(("Bash" :pattern "npx test" :action allow)
        ("Bash" :pattern "npx test" :network t :action allow))
      (niwa-session-permission-rules session))))
  :doc "allow-session stores exact resource authority separately from rules"
  (let* ((session (niwa-session--create :name "test"))
         (path (expand-file-name "/outside/file.el")))
    (should (eq (niwa-permission--apply-prompt-result
                 'allow-session "Read" session nil path
                 :resource-access 'read)
                'allow))
    (should (equal (list (list :path path :access 'read))
                   (niwa-session-resource-grants session)))
    (should-not (niwa-session-permission-rules session)))
  :doc "always-allow stores persistent and session rules"
  (let* ((tmp-dir (make-temp-file "niwa-test-" t))
         (niwa-user-dir (file-name-concat tmp-dir "global/"))
         (ws (niwa-workspace--create
              :type 'project :id "test" :root tmp-dir
              :name "test"
              :file-cache (make-hash-table :test #'equal)))
         (session (niwa-session--create :name "test")))
    (unwind-protect
        (progn
          (should (eq (niwa-permission--apply-prompt-result
                       'always-allow "Read" session ws)
                      'allow))
          (should (= (length (niwa-session-permission-rules session)) 1))
          (should (= (length (niwa-permission-persistence-load-rules ws)) 1)))
      (delete-directory tmp-dir t)))
  :doc "always-allow stores persistent authority without a session duplicate"
  (let* ((tmp-dir (make-temp-file "niwa-test-" t))
         (niwa-user-dir (file-name-concat tmp-dir "global/"))
         (path (file-name-concat tmp-dir "outside.el"))
         (ws (niwa-workspace--create
              :type 'project :id "test" :root tmp-dir
              :name "test" :file-cache nil))
         (session (niwa-session--create :name "test")))
    (unwind-protect
        (progn
          (should (eq (niwa-permission--apply-prompt-result
                       'always-allow "Write" session ws path
                       :resource-access 'write)
                      'allow))
          (should-not (niwa-session-resource-grants session))
          (should (equal (list (list :path path :access 'write))
                         (niwa-permission-persistence-load-resource-grants
                          ws)))
          (should-not (niwa-session-permission-rules session)))
      (delete-directory tmp-dir t)))
  :doc "deny-once returns deny without storage"
  (let ((session (niwa-session--create :name "test")))
    (should (eq (niwa-permission--apply-prompt-result
                 'deny-once "Edit" session)
                'deny))
    (should-not (niwa-session-permission-rules session)))
  :doc "deny-session stores rule and returns deny"
  (let ((session (niwa-session--create :name "test")))
    (should (eq (niwa-permission--apply-prompt-result
                 'deny-session "Edit" session)
                'deny))
    (should (equal (plist-get (cdar (niwa-session-permission-rules session))
                              :action)
                   'deny)))
  :doc "unknown result defaults to deny"
  (should (eq (niwa-permission--apply-prompt-result 'bogus "Edit") 'deny))

  :doc "remembers the same complete operation profile in session and workspace"
  (dolist (specifier '((nil nil) (:path "/outside/*")
                       (:pattern "build *") (:domain "*.example.com")
                       (:name "worker")))
    (let* ((root (make-temp-file "niwa-rule-profile-" t))
           (niwa-user-dir (file-name-concat root "global"))
           (workspace (niwa-workspace--create :type 'project :root root))
           (session (niwa-session--create :name "test"))
           (grants (list (list :path (file-name-concat root "cache")
                               :access 'write :recursive t)))
           (expected (append (list "Custom")
                             (and (car specifier) specifier)
                             (list :network t :file-system grants :action 'allow))))
      (unwind-protect
          (progn
            (should
             (eq 'allow
                 (niwa-permission--apply-prompt-result
                  'always-allow "Custom" session workspace nil
                  :spec-key (car specifier) :spec-value (cadr specifier)
                  :network t :file-system grants)))
            (should (equal (list expected)
                           (niwa-session-permission-rules session)))
            (should (equal (list expected)
                           (niwa-permission-persistence-load-rules workspace)))
            (should-not (niwa-session-resource-grants session))
            (should-not (niwa-permission-persistence-load-resource-grants workspace)))
        (delete-directory root t))))

  :doc "selected resource scope and original deny rule remain independent"
  (let ((session (niwa-session--create :name "test")))
    (niwa-permission--apply-prompt-result
     'allow-session "Read" session nil "/selected/tree"
     :spec-key :path :spec-value "/original/file"
     :resource-access 'read :resource-recursive t)
    (should (equal '((:path "/selected/tree" :access read :recursive t))
                   (niwa-session-resource-grants session)))
    (should-not (niwa-session-permission-rules session))
    (niwa-permission--apply-prompt-result
     'deny-session "Read" session nil "/selected/tree"
     :spec-key :path :spec-value "/original/file"
     :resource-access 'read :resource-recursive t)
    (should (equal '(("Read" :path "/original/file" :action deny))
                   (niwa-session-permission-rules session))))

  :doc "failed persistent approval does not install a session rule"
  (let* ((root (make-temp-file "niwa-rule-failure-" t))
         (workspace (niwa-workspace--create :type 'project :root root))
         (session (niwa-session--create :name "test"))
         (file (niwa-permission-persistence-file workspace 'workspace)))
    (unwind-protect
        (progn
          (make-directory (file-name-directory file) t)
          (write-region "invalid" nil file nil 'silent)
          (should-error (niwa-permission--apply-prompt-result
                         'always-allow "Read" session workspace)
                        :type 'user-error)
          (should-not (niwa-session-permission-rules session)))
      (delete-directory root t))))


(niwa-deftest niwa-permission--invocation-context ()
  ,test
  (test)
  :doc "captures a selected conversation directory without changing shared policy"
  (let* ((session (niwa-session--create :working-directory "/tmp/parent/"))
         (tool (niwa-tool--create :name "Read" :read-only-p t))
         (niwa--session session)
         (niwa--conversation-working-directory "/tmp/child/")
         (context (niwa-permission--invocation-context
                   :tool tool :session session :mode 'ask)))
    (should (equal "/tmp/child/" (plist-get context :execution-directory)))
    (should (equal "/tmp/parent/" (niwa-session-working-directory session)))
    (let ((captured (niwa-permission--invocation-context
                     :tool tool :session session :mode 'ask
                     :execution-directory "/tmp/captured/")))
      (should (equal "/tmp/captured/"
                     (plist-get captured :execution-directory)))))

  :doc "captured request and invocation Plan stamps survive ambient drift"
  (let* ((niwa-permission-rules nil)
         (session (niwa-session--create))
         (niwa--current-request (niwa-request--create :session session))
         (niwa--agent-invocation
          (niwa-agent-invocation--create
           :path "/root/unrelated" :parent-session session)))
    (dolist (owner '(request invocation))
      (dolist (name '("Eval" "Edit" "ApplyPatch"))
        (dolist (mode '(ask edits full-auto))
          (let* ((tool (niwa-tool--create
                        :name name :groups (unless (equal name "Eval") '(edit))))
                 (context
                  (niwa-permission--invocation-context
                   :tool tool :session session :mode mode
                   :request (and (eq owner 'request)
                                 (niwa-request--create
                                  :session session :plan-read-only t))
                   :invocation (and (eq owner 'invocation)
                                    (niwa-agent-invocation--create
                                     :path "/root/captured"
                                     :parent-session session
                                     :plan-read-only t))))
                 (decision (plist-get context :early-decision)))
            (should (eq 'deny (niwa-permission-decision-raw-outcome decision)))
            (should (eq 'plan-mode (plist-get decision :via))))))))

  :doc "captured non-Plan and nil stamps do not borrow ambient Plan authority"
  (let* ((niwa-permission-rules nil)
         (session (niwa-session--create))
         (niwa--current-request
          (niwa-request--create :session session :plan-read-only t))
         (niwa--agent-invocation
          (niwa-agent-invocation--create
           :path "/root/unrelated" :parent-session session :plan-read-only t)))
    (dolist (nil-stamps '(nil t))
      (dolist (name '("Eval" "Edit" "ApplyPatch"))
        (dolist (mode '(ask edits full-auto))
          (let ((context
                 (niwa-permission--invocation-context
                  :tool (niwa-tool--create
                         :name name :groups (unless (equal name "Eval") '(edit)))
                  :session session :mode mode
                  :request (unless nil-stamps
                             (niwa-request--create :session session))
                  :invocation (unless nil-stamps
                                (niwa-agent-invocation--create
                                 :path "/root/captured" :parent-session session)))))
            (should-not (plist-get context :early-decision)))))))

  :doc "captured nil stamps still enforce the owning session's live Plan flag"
  (let* ((niwa-permission-rules nil)
         (session (niwa-session--create))
         (niwa--current-request nil)
         (niwa--agent-invocation nil)
         (tool (niwa-tool--create :name "Eval")))
    (should-not
     (plist-get (niwa-permission--invocation-context
                 :tool tool :session session :request nil :invocation nil
                 :mode 'full-auto)
                :early-decision))
    (setf (niwa-session-plan-mode session) t)
    (let ((decision
           (plist-get (niwa-permission--invocation-context
                       :tool tool :session session :request nil :invocation nil
                       :mode 'full-auto)
                      :early-decision)))
      (should (eq 'deny (niwa-permission-decision-raw-outcome decision)))
      (should (eq 'plan-mode (plist-get decision :via)))))

  :doc "resolves a named built-in tool before deriving capability facts"
  (let ((tool (niwa-tool--create :name "Read" :read-only-p t)))
    (cl-letf (((symbol-function 'niwa-tool-ensure)
               (lambda (name)
                 (and (equal name "Read") tool))))
      (let ((context (niwa-permission--invocation-context
                      :tool-name "Read"
                      :path "/outside/file.el"
                      :workspace-root "/workspace")))
        (should (eq tool (plist-get context :tool)))
        (should (eq 'read (plist-get context :resource-access))))))

  :doc "extracts checker facts and prompt rule facts for an outside path"
  (let* ((root (file-name-as-directory
                (make-temp-file "niwa-perm-root-" t)))
         (outside (file-name-as-directory
                   (make-temp-file "niwa-perm-outside-" t)))
         (path (file-name-concat outside "secret.txt"))
         (workspace (niwa-workspace--create
                     :type 'file :id "root" :root root
                     :name "root" :file-cache nil))
         (session (niwa-session--create
                   :name "test" :workspace workspace
                   :permission-mode 'ask
                   :permission-rules '(("Read" :action ask))))
         (tool (niwa-tool--create
                :name "Read" :read-only-p t
                :get-path (lambda (args) (plist-get args :file_path)))))
    (unwind-protect
        (cl-letf (((symbol-function 'niwa--all-allowed-roots)
                   (lambda (&optional _buffer) (list root))))
          (let ((context (niwa-permission--invocation-context
                          :tool tool
                          :args (list :file_path path)
                          :session session
                          :workspace workspace)))
            (should (equal path (plist-get context :path)))
            (should (equal root (plist-get context :workspace-root)))
            (should (equal (list (file-name-as-directory
                                  (expand-file-name
                                   niwa-tool-registry--source-dir))
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
  (let* ((tool (niwa-tool--create
                :name "Bash" :read-only-p nil
                :get-pattern (lambda (_args) "getter-pattern")))
         (context (niwa-permission--invocation-context
                   :tool tool
                   :args '(:command "getter-pattern")
                   :pattern "explicit-pattern")))
    (should (equal "explicit-pattern" (plist-get context :pattern)))
    (should (eq :pattern (plist-get context :specifier-key)))
    (should (equal "explicit-pattern"
                   (plist-get context :specifier-value))))

  :doc "retains the pipeline permission-request boundary"
  (let* ((request #'ignore)
         (tool (niwa-tool--create :name "Bash" :read-only-p nil))
         (context (niwa-permission--invocation-context
                   :tool tool :permission-request request)))
    (should (eq request (plist-get context :permission-request))))

  :doc "inherits one-shot mutation policy from the request"
  (let* ((tool (niwa-tool--create :name "Edit" :read-only-p nil))
         (request (niwa-request--create :one-shot-mutations-p t))
         (context (niwa-permission--invocation-context
                   :tool tool :request request)))
    (should (plist-get context :one-shot-mutations-p)))

  :doc "uses a frozen persistent permission snapshot without reloading it"
  (let* ((root (file-name-as-directory
                (make-temp-file "niwa-perm-frozen-" t)))
         (workspace (niwa-workspace--create
                     :type 'file :id root :root root :name "frozen"
                     :file-cache nil))
         (session (niwa-session--create
                   :name "frozen" :workspace workspace
                   :permission-rules '(("Read" :action ask))))
         (tool (niwa-tool--create :name "Read" :read-only-p t)))
    (unwind-protect
        (with-temp-buffer
          (setq-local niwa-permission--context-frozen-p t
                      niwa-permission--frozen-persistent-rules
                      '(("Read" :action deny))
                      niwa-permission--frozen-resource-grants
                      `((:path ,root :access read)))
          (cl-letf (((symbol-function
                      'niwa-permission-persistence-load-rules)
                     (lambda (_workspace)
                       (ert-fail "reloaded persistent rules")))
                    ((symbol-function
                      'niwa-permission-persistence-load-resource-grants)
                     (lambda (_workspace)
                       (ert-fail "reloaded persistent grants"))))
            (let* ((context (niwa-permission--invocation-context
                             :tool tool :session session
                             :workspace workspace))
                   (buckets (plist-get context :buckets)))
              (should (equal '(("Read" :action deny))
                             (alist-get :persistent buckets)))
              (should (equal `((:path ,root :access read))
                             (plist-get context :resource-grants))))))
      (delete-directory root t))))

(niwa-deftest niwa-permission--one-shot-prompt-entry
  (:doc "captures the side buffer without caching parent activity")
  (let ((side-buffer (generate-new-buffer " *niwa-side-permission*")))
    (unwind-protect
        (let ((entry (niwa-permission--one-shot-prompt-entry
                      '(:kind permission :mutation-p t) side-buffer)))
          (should (eq side-buffer (plist-get entry :data-buffer)))
          (should (plist-get entry :once-only))
          (should-not (plist-member entry :shared-workspace-active-p)))
      (kill-buffer side-buffer))))

(niwa-deftest niwa-permission--checker-args ()
  ,test
  (test)
  :doc "passes the normalized invocation preflight without rebuilding it"
  (let* ((tool (niwa-tool--create :name "Read" :read-only-p t))
         (context
          (niwa-permission--invocation-context
           :tool tool
           :path "/tmp/file.txt"
           :session-rules '(("Read" :action allow))
           :mode 'ask
           :workspace-root "/tmp/"
           :allowed-roots '("/tmp/")
           :exact-allowed-paths '("/tmp/file.txt")))
         (args (niwa-permission--checker-args context)))
    (should (equal :normalized-context (car args)))
    (should (= 2 (length args)))
    (setq args (plist-get args :normalized-context))
    (should (eq tool (plist-get args :tool)))
    (should (equal "/tmp/file.txt" (plist-get args :path)))
    (should (eq 'ask (plist-get args :mode)))
    (should (equal '("/tmp/file.txt")
                   (plist-get args :exact-allowed-paths)))))

(niwa-deftest niwa-permission--invocation-preflight-reuse ()
  ,test
  (test)
  :doc "extracts specifiers and collects rule buckets once per invocation"
  (let* ((getter-calls 0)
         (bucket-calls 0)
         (boundary-calls 0)
         (original-collect (symbol-function
                            'niwa-permission-rules-collect-buckets))
         (original-boundary (symbol-function
                             'niwa-permission-rules-path-in-allowed-roots-p))
         (tool (niwa-tool--create
                :name "Read"
                :read-only-p t
                :get-path (lambda (args)
                            (cl-incf getter-calls)
                            (plist-get args :path))))
         context)
    (cl-letf (((symbol-function 'niwa-permission-rules-collect-buckets)
               (lambda (&rest buckets)
                 (cl-incf bucket-calls)
                 (apply original-collect buckets)))
              ((symbol-function
                'niwa-permission-rules-path-in-allowed-roots-p)
               (lambda (path roots)
                 (cl-incf boundary-calls)
                 (funcall original-boundary path roots))))
      (setq context
            (niwa-permission--invocation-context
             :tool tool
             :args '(:path "/project/file.el")
             :workspace-root "/project"
             :mode 'ask))
      (should
       (eq 'allow
           (apply #'niwa-check-permission
                  "Read"
                  (niwa-permission--checker-args context)))))
    (should (= 1 getter-calls))
    (should (= 1 bucket-calls))
    (should (= 1 boundary-calls))))


(niwa-deftest niwa-check-permission/bucket-precedence ()
  ,test
  (test)
  :doc "session deny beats invocation allow (pass 1 absolute)"
  (let ((niwa-permission-rules nil))
    (should (eq 'deny
                (niwa-check-permission
                 "Bash"
                 :pattern "rm /tmp/foo"
                 :invocation-rules '(("Bash" :action allow))
                 :session-rules
                 '(("Bash" :pattern "rm *" :action deny))))))

  :doc "innermost (invocation) allow beats session ask"
  (let ((niwa-permission-rules nil))
    (should (eq 'allow
                (niwa-check-permission
                 "Bash"
                 :pattern "echo hi"
                 :invocation-rules
                 '(("Bash" :pattern "echo *" :action allow))
                 :session-rules '(("Bash" :action ask))))))

  :doc "request rules outrank session rules"
  (let ((niwa-permission-rules nil))
    (should (eq 'allow
                (niwa-check-permission
                 "Bash"
                 :pattern "ls"
                 :request-rules '(("Bash" :pattern "ls" :action allow))
                 :session-rules '(("Bash" :pattern "ls" :action ask))))))

  :doc "no skill rules -> session rules apply normally"
  (let ((niwa-permission-rules nil))
    (should (eq 'ask
                (niwa-check-permission
                 "Bash"
                 :pattern "rm /tmp/foo"
                 :session-rules
                 '(("Bash" :pattern "rm *" :action ask)))))))

(niwa-deftest niwa-check-permission/workspace-root ()
  ,test
  (test)
  :doc "workspace root itself is treated as inside the workspace"
  (let* ((root (file-name-as-directory
                (make-temp-file "niwa-workspace-root-" t)))
         (root-without-slash (directory-file-name root))
         (mock-tool (niwa-tool--create :name "Grep" :read-only-p t))
         (niwa-permission-rules nil)
         (niwa-protected-paths nil))
    (unwind-protect
        (should (eq 'allow
                    (niwa-check-permission
                     "Grep"
                     :tool-struct mock-tool
                     :path root-without-slash
                     :workspace-root root
                     :mode 'ask)))
      (delete-directory root t)))

  :doc "workspace children are still treated as inside the workspace"
  (let* ((root (file-name-as-directory
                (make-temp-file "niwa-workspace-child-" t)))
         (child (file-name-concat root "file.el"))
         (mock-tool (niwa-tool--create :name "Read" :read-only-p t))
         (niwa-permission-rules nil)
         (niwa-protected-paths nil))
    (unwind-protect
        (should (eq 'allow
                    (niwa-check-permission
                     "Read"
                     :tool-struct mock-tool
                     :path child
                     :workspace-root root
                     :mode 'ask)))
      (delete-directory root t)))

  :doc "additional allowed roots are treated as inside the workspace boundary"
  (let* ((root (file-name-as-directory
                (make-temp-file "niwa-workspace-allowed-root-" t)))
         (extra (file-name-as-directory
                 (make-temp-file "niwa-workspace-extra-root-" t)))
         (child (file-name-concat extra "file.el"))
         (mock-tool (niwa-tool--create :name "Read" :read-only-p t))
         (niwa-permission-rules nil)
         (niwa-protected-paths nil))
    (unwind-protect
        (should (eq 'allow
                    (niwa-check-permission
                     "Read"
                     :tool-struct mock-tool
                     :path child
                     :workspace-root root
                     :allowed-roots (list root extra)
                     :mode 'ask)))
      (delete-directory root t)
      (delete-directory extra t)))

  :doc "sibling directories are outside the workspace"
  (let* ((parent (make-temp-file "niwa-workspace-parent-" t))
         (root (file-name-as-directory
                (file-name-concat parent "project")))
         (sibling (file-name-concat parent "project-other"))
         (niwa-permission-rules nil)
         (niwa-protected-paths nil))
    (unwind-protect
        (progn
          (make-directory root)
          (make-directory sibling)
          (should (eq 'ask
                      (niwa-check-permission
                       "Grep"
                       :path sibling
                       :workspace-root root
                       :mode 'ask))))
      (delete-directory parent t))))

(provide 'test-niwa-permissions)
;;; test-niwa-permissions.el ends here
