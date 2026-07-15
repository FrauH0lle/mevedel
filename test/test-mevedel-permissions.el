;;; test-mevedel-permissions.el --- Tests for permission system -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'mevedel-permissions)
(require 'mevedel-structs)
(require 'mevedel-tool-registry)
(require 'mevedel-reminders)
(require 'mevedel-goal)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name
               load-file-name
               byte-compile-current-file))
          "helpers"))

(defmacro test-mevedel-permissions--with-goal-phase (phase &rest body)
  "Run BODY with an active Goal in PHASE as the current session."
  (declare (indent 1))
  `(let* ((mevedel--session
           (mevedel-session--create :name "goal" :permission-mode 'full-auto))
          (goal (mevedel-goal--create
                 :objective "test" :status 'active :phase ,phase)))
     (setf (mevedel-session-goal mevedel--session) goal)
     ,@body))

(mevedel-deftest mevedel-permission--goal-read-only-phase-p ()
  ,test
  (test)
  :doc "uses the explicit owning session when enforcing Goal phase policy"
  (let ((session (mevedel-session--create :name "main"))
        received)
    (cl-letf (((symbol-function 'mevedel-goal-read-only-phase-p)
               (lambda (candidate)
                 (setq received candidate)
                 t)))
      (should (mevedel-permission--goal-read-only-phase-p session)))
    (should (eq session received))))


;;
;;; Path pattern matching

(mevedel-deftest mevedel-permission--match-path-pattern ()
  ,test
  (test)
  :doc "single star matches files in directory"
  (should (mevedel-permission--match-path-pattern
           "/home/user/projects/foo.el" "/home/user/projects/*"))
  :doc "single star does not match across directories"
  (should-not (mevedel-permission--match-path-pattern
               "/home/user/projects/sub/foo.el" "/home/user/projects/*"))
  :doc "double star matches across directories"
  (should (mevedel-permission--match-path-pattern
           "/home/user/projects/sub/foo.el" "/home/user/projects/**"))
  :doc "double star matches nested paths"
  (should (mevedel-permission--match-path-pattern
           "/home/user/projects/a/b/c/foo.el" "/home/user/projects/**"))
  :doc "trailing double star also matches the directory itself"
  (should (mevedel-permission--match-path-pattern
           "/home/user/projects" "/home/user/projects/**"))
  :doc "trailing double star also matches the directory itself with slash"
  (should (mevedel-permission--match-path-pattern
           "/home/user/projects/" "/home/user/projects/**"))
  :doc "double star matches across directories in interior"
  (should (mevedel-permission--match-path-pattern
           "/repo/.git/config" "**/.git/**"))
  :doc "tilde expansion in pattern"
  (should (mevedel-permission--match-path-pattern
           (expand-file-name "~/.ssh/id_rsa") "~/.ssh/*"))
  :doc "question mark matches single character"
  (should (mevedel-permission--match-path-pattern
           "/home/user/file.el" "/home/user/fil?.el"))
  :doc "nil path returns nil"
  (should-not (mevedel-permission--match-path-pattern nil "/some/pattern"))
  :doc "nil pattern returns nil"
  (should-not (mevedel-permission--match-path-pattern "/some/path" nil))
  :doc "non-matching path returns nil"
  (should-not (mevedel-permission--match-path-pattern
               "/other/path/file.el" "/home/user/*")))


;;
;;; Rule matching

(mevedel-deftest mevedel-permission--find-rules ()
  ,test
  (test)
  :doc "exact tool name match"
  (let ((rules '(("Read" :action allow))))
    (should (equal (length (mevedel-permission--find-rules rules "Read")) 1))
    (should (null (mevedel-permission--find-rules rules "Write"))))
  :doc "wildcard tool matches all"
  (let ((rules '(("*" :action ask))))
    (should (equal (length (mevedel-permission--find-rules rules "Read")) 1))
    (should (equal (length (mevedel-permission--find-rules rules "Write")) 1)))
  :doc "path rule matches only matching paths"
  (let ((rules '(("Edit" :path "/home/user/projects/*" :action allow))))
    (should (equal (length (mevedel-permission--find-rules
                            rules "Edit" :path "/home/user/projects/foo.el"))
                   1))
    (should (null (mevedel-permission--find-rules
                   rules "Edit" :path "/other/path/foo.el"))))
  :doc "non-path rule matches regardless of path"
  (let ((rules '(("Read" :action allow))))
    (should (equal (length (mevedel-permission--find-rules
                            rules "Read" :path "/any/path"))
                   1)))
  :doc "pattern rule matches only matching command strings"
  (let ((rules '(("Bash" :pattern "git log*" :action allow))))
    (should (equal (length (mevedel-permission--find-rules
                            rules "Bash" :pattern "git log --oneline"))
                   1))
    (should (null (mevedel-permission--find-rules
                   rules "Bash" :pattern "rm -rf"))))
  :doc "pattern prefix rule matches bare command and command with arguments"
  (let ((rules '(("Bash" :pattern "git log:*" :action allow))))
    (should (equal (length (mevedel-permission--find-rules
                            rules "Bash" :pattern "git log"))
                   1))
    (should (equal (length (mevedel-permission--find-rules
                            rules "Bash" :pattern "git log --oneline"))
                   1))
    (should (null (mevedel-permission--find-rules
                   rules "Bash" :pattern "git lollipop"))))
  :doc "domain rule matches only matching hosts"
  (let ((rules '(("WebFetch" :domain "*.example.com" :action allow))))
    (should (equal (length (mevedel-permission--find-rules
                            rules "WebFetch" :domain "api.example.com"))
                   1))
    (should (null (mevedel-permission--find-rules
                   rules "WebFetch" :domain "evil.org"))))
  :doc "name rule matches only matching names"
  (let ((rules '(("Agent" :name "explorer" :action allow))))
    (should (equal (length (mevedel-permission--find-rules
                            rules "Agent" :name "explorer"))
                   1))
    (should (null (mevedel-permission--find-rules
                   rules "Agent" :name "verifier")))))

(mevedel-deftest mevedel-permission--rules-action ()
  ,test
  (test)
  :doc "deny takes precedence over allow"
  (let ((rules '(("Edit" :action allow)
                 ("Edit" :action deny))))
    (should (eq (mevedel-permission--rules-action rules "Edit") 'deny)))
  :doc "ask takes precedence over allow"
  (let ((rules '(("Edit" :action allow)
                 ("Edit" :action ask))))
    (should (eq (mevedel-permission--rules-action rules "Edit") 'ask)))
  :doc "allow alone"
  (let ((rules '(("Read" :action allow))))
    (should (eq (mevedel-permission--rules-action rules "Read") 'allow)))
  :doc "no matching rules returns nil"
  (let ((rules '(("Read" :action allow))))
    (should-not (mevedel-permission--rules-action rules "Write")))
  :doc "specifier rule overrides generic rule of opposite action"
  ;; Generic deny should lose to specific allow for matching pattern
  (let ((rules '(("Bash" :action deny)
                 ("Bash" :pattern "echo*" :action allow))))
    (should (eq (mevedel-permission--rules-action
                 rules "Bash" :pattern "echo hello")
                'allow)))
  :doc "generic rule applies when specifier does not match"
  (let ((rules '(("Bash" :action deny)
                 ("Bash" :pattern "echo*" :action allow))))
    (should (eq (mevedel-permission--rules-action
                 rules "Bash" :pattern "rm -rf")
                'deny)))
  :doc "pattern specifier: deny wins over allow within specifier group"
  (let ((rules '(("Bash" :pattern "git*" :action allow)
                 ("Bash" :pattern "git push*" :action deny))))
    (should (eq (mevedel-permission--rules-action
                 rules "Bash" :pattern "git push origin")
                'deny)))
  :doc "domain specifier match allows"
  (let ((rules '(("WebFetch" :domain "*.example.com" :action allow))))
    (should (eq (mevedel-permission--rules-action
                 rules "WebFetch" :domain "api.example.com")
                'allow)))
  :doc "name specifier match allows"
  (let ((rules '(("Agent" :name "explorer" :action allow))))
    (should (eq (mevedel-permission--rules-action
                 rules "Agent" :name "explorer")
                'allow))))


;;
;;; Protected paths

(mevedel-deftest mevedel-permission--path-protected-p ()
  ,test
  (test)
  :doc "git directory is protected"
  (let ((mevedel-protected-paths '("**/.git/**")))
    (should (mevedel-permission--path-protected-p "/repo/.git/config")))
  :doc "ssh directory is protected"
  (let ((mevedel-protected-paths '("~/.ssh/*")))
    (should (mevedel-permission--path-protected-p
             (expand-file-name "~/.ssh/id_rsa"))))
  :doc "normal path is not protected"
  (let ((mevedel-protected-paths '("**/.git/**" "~/.ssh/*")))
    (should-not (mevedel-permission--path-protected-p "/home/user/projects/foo.el")))
  :doc "nil path is not protected"
  (let ((mevedel-protected-paths '("**/.git/**")))
    (should-not (mevedel-permission--path-protected-p nil))))


;;
;;; Mode decisions

(mevedel-deftest mevedel-permission-mode-normalize ()
  ,test
  (test)
  :doc "accepts only canonical permission modes"
  (dolist (mode '(ask auto full-auto))
    (should (eq mode (mevedel-permission-mode-normalize mode)))
    (should (eq mode
                (mevedel-permission-mode-normalize (symbol-name mode)))))
  :doc "rejects retired modes and the user-facing edit alias"
  (dolist (mode '(default accept-edits trust-all edit edits))
    (should-error (mevedel-permission-mode-normalize mode)
                  :type 'user-error)))

(mevedel-deftest mevedel-permission-mode-parse-user-input ()
  ,test
  (test)
  :doc "accepts canonical mode names and maps edit to auto"
  (progn
    (dolist (mode '(ask auto full-auto))
      (should (eq mode
                  (mevedel-permission-mode-parse-user-input
                   (symbol-name mode)))))
    (should (eq 'auto (mevedel-permission-mode-parse-user-input "edit"))))
  :doc "rejects retired persisted vocabulary"
  (dolist (mode '(default accept-edits trust-all edits))
    (should-error (mevedel-permission-mode-parse-user-input mode)
                  :type 'user-error)))

(mevedel-deftest mevedel-permission--mode-decision ()
  ,test
  (test)
  :doc "full-auto allows everything"
  (progn
    (should (eq (mevedel-permission--mode-decision 'full-auto nil) 'allow))
    (should (eq (mevedel-permission--mode-decision 'full-auto t) 'allow)))
  :doc "auto allows read-only and native edits but asks for other writes"
  (progn
    (should (eq (mevedel-permission--mode-decision 'auto t) 'allow))
    (should (eq (mevedel-permission--mode-decision 'auto nil t) 'allow))
    (should (eq (mevedel-permission--mode-decision 'auto nil nil) 'ask)))
  :doc "ask allows read-only and asks for writes"
  (progn
    (should (eq (mevedel-permission--mode-decision 'ask t) 'allow))
    (should (eq (mevedel-permission--mode-decision 'ask nil) 'ask))))


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

  :doc "read-only Goal phase denies mutation before protected-path prompting"
  (test-mevedel-permissions--with-goal-phase 'planning
    (let ((tool (mevedel-tool--create :name "Edit" :read-only-p nil))
          (mevedel-permission-rules nil)
          (mevedel-protected-paths '("**/.git/**")))
      (let* ((context
              (mevedel-permission--preflight
               "Edit" :tool-struct tool :path "/repo/.git/config"
               :mode 'full-auto))
             (decision (plist-get context :early-decision)))
        (should (eq 'deny
                    (mevedel-permission-decision-raw-outcome decision)))
        (should (eq 'goal-phase (plist-get decision :via)))))))

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
             :request-rules (("Edit" :action ask)) :mode auto)
            ("Custom" :tool-struct ,deny-tool :mode full-auto))))
    (let ((mevedel-permission-rules nil)
          (mevedel-protected-paths '("**/.git/**")))
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
                    'mevedel-permission--first-deny-bucket))
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
          (mevedel-protected-paths '("**/.git/**")))
      (cl-letf (((symbol-function 'mevedel-permission--first-deny-bucket)
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
        (mevedel-protected-paths '("**/.git/**")))
    (should (eq (mevedel-check-permission "Edit"
                  :path "/repo/.git/config"
                  :mode 'full-auto)
                'ask)))
  :doc "read-only Goal phase denies non-read-only protected paths"
  (test-mevedel-permissions--with-goal-phase 'reviewing
    (let ((mevedel-permission-rules nil)
          (mevedel-protected-paths '("**/.git/**"))
          (mock-tool (mevedel-tool--create :name "Edit" :read-only-p nil)))
      (should (eq (mevedel-check-permission "Edit"
                    :tool-struct mock-tool
                    :path "/repo/.git/config"
                    :mode 'full-auto)
                  'deny))))
  :doc "read-only Goal phase keeps read-only protected paths as ask"
  (test-mevedel-permissions--with-goal-phase 'planning
    (let ((mevedel-permission-rules nil)
          (mevedel-protected-paths '("**/.git/**"))
          (mock-tool (mevedel-tool--create :name "Read" :read-only-p t)))
      (should (eq (mevedel-check-permission "Read"
                    :tool-struct mock-tool
                    :path "/repo/.git/config"
                    :mode 'full-auto)
                  'ask))))
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
  :doc "Goal planning denies mutation even under full-auto"
  (test-mevedel-permissions--with-goal-phase 'planning
    (let ((mevedel-permission-rules nil)
          (mevedel-protected-paths nil)
          (mock-tool (mevedel-tool--create :name "Edit" :read-only-p nil)))
      (should (eq (mevedel-check-permission "Edit"
                    :tool-struct mock-tool
                    :mode 'full-auto)
                  'deny))))
  :doc "read-only tool allowed in ask mode"
  (let ((mevedel-permission-rules nil)
        (mevedel-protected-paths nil)
        (mock-tool (mevedel-tool--create :name "Read" :read-only-p t)))
    (should (eq (mevedel-check-permission "Read"
                  :tool-struct mock-tool
                  :mode 'ask)
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
        (mevedel-protected-paths '("**/.git/**"))
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
                 :path "/outside/target.el" :mode 'auto
                 :workspace-root "/project" :resource-grants grants)
                'allow))
    (should (eq (mevedel-check-permission
                 "Write" :tool-struct mock-tool
                 :path "/outside/sibling.el" :mode 'auto
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
                    :get-name (lambda (input) (plist-get input :subagent_type))
                    :read-only-p nil)))
    (should (eq (mevedel-check-permission "Agent"
                  :tool-struct mock-tool
                  :content '(:subagent_type "explorer")
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


;;
;;; Persistent rule storage

(mevedel-deftest mevedel-permission--save-and-load-persistent-rules ()
  ,test
  (test)
  :doc "round-trip save and load"
  (let* ((tmp-dir (make-temp-file "mevedel-test-" t))
         (mevedel-user-dir (file-name-concat tmp-dir "global/"))
         (ws (mevedel-workspace--create
              :type 'project :id "test" :root tmp-dir
              :name "test"
              :file-cache (make-hash-table :test #'equal))))
    (unwind-protect
        (progn
          (mevedel-permission--save-persistent-rule ws "Read" 'allow)
          (mevedel-permission--save-persistent-rule ws "Edit" 'deny "/secret/*")
          (let ((rules (mevedel-permission--load-persistent-rules ws)))
            (should (= (length rules) 2))
            (should (equal (car rules) '("Read" :action allow)))
            (should (equal (cadr rules)
                           '("Edit" :path "/secret/*" :action deny)))))
      (delete-directory tmp-dir t)))
  :doc "load returns nil for missing file"
  (let* ((tmp-dir (make-temp-file "mevedel-test-" t))
         (mevedel-user-dir (file-name-concat tmp-dir "global/"))
         (ws (mevedel-workspace--create
              :type 'project :id "test" :root tmp-dir
              :name "test"
              :file-cache (make-hash-table :test #'equal))))
    (unwind-protect
        (should-not (mevedel-permission--load-persistent-rules ws))
      (delete-directory tmp-dir t)))
  :doc "merges global and project rules, project rules last"
  (let* ((tmp-dir (make-temp-file "mevedel-test-" t))
         (global-dir (file-name-concat tmp-dir "global/"))
         (project-dir (file-name-concat tmp-dir "project/"))
         (mevedel-user-dir global-dir)
         (ws (mevedel-workspace--create
              :type 'project :id "test" :root project-dir
              :name "test"
              :file-cache (make-hash-table :test #'equal))))
    (unwind-protect
        (progn
          ;; Write global rules
          (make-directory global-dir t)
          (with-temp-file (file-name-concat global-dir "permissions.el")
            (pp '(:rules (("Read" :action allow)
                          ("*" :path "/tmp/*" :action allow))
                  :resource-grants nil)
                (current-buffer)))
          ;; Write project rules
          (mevedel-permission--save-persistent-rule ws "Edit" 'allow "~/proj/*")
          (let ((rules (mevedel-permission--load-persistent-rules ws)))
            ;; Global rules first, then project
            (should (= (length rules) 3))
            (should (equal (nth 0 rules) '("Read" :action allow)))
            (should (equal (nth 1 rules) '("*" :path "/tmp/*" :action allow)))
            (should (equal (nth 2 rules) '("Edit" :path "~/proj/*" :action allow)))))
      (delete-directory tmp-dir t)))
  :doc "deduplicates exact persistent rules"
  (let* ((tmp-dir (make-temp-file "mevedel-test-" t))
         (mevedel-user-dir (file-name-concat tmp-dir "global/"))
         (ws (mevedel-workspace--create
              :type 'project :id "test" :root tmp-dir
              :name "test"
              :file-cache (make-hash-table :test #'equal))))
    (unwind-protect
        (progn
          (mevedel-permission--save-persistent-rule
           ws "Bash" 'allow nil
           :spec-key :pattern :spec-value "git diff:*")
          (mevedel-permission--save-persistent-rule
           ws "Bash" 'allow nil
           :spec-key :pattern :spec-value "git diff:*")
          (let ((rules (mevedel-permission--load-persistent-rules ws)))
            (should (equal rules
                           '(("Bash" :pattern "git diff:*"
                              :action allow))))))
      (delete-directory tmp-dir t)))
  :doc "preserves distinct persistent rules"
  (let* ((tmp-dir (make-temp-file "mevedel-test-" t))
         (mevedel-user-dir (file-name-concat tmp-dir "global/"))
         (ws (mevedel-workspace--create
              :type 'project :id "test" :root tmp-dir
              :name "test"
              :file-cache (make-hash-table :test #'equal))))
    (unwind-protect
        (progn
          (mevedel-permission--save-persistent-rule
           ws "Bash" 'allow nil
           :spec-key :pattern :spec-value "git diff:*")
          (mevedel-permission--save-persistent-rule
           ws "Bash" 'deny nil
           :spec-key :pattern :spec-value "git diff:*")
          (mevedel-permission--save-persistent-rule
           ws "Bash" 'allow nil
           :spec-key :pattern :spec-value "git status:*")
          (let ((rules (mevedel-permission--load-persistent-rules ws)))
            (should (equal rules
                           '(("Bash" :pattern "git diff:*"
                              :action allow)
                             ("Bash" :pattern "git diff:*"
                              :action deny)
                             ("Bash" :pattern "git status:*"
                              :action allow))))))
      (delete-directory tmp-dir t))))

(mevedel-deftest mevedel-permission-remove-session-resource-grant ()
  ,test
  (test)
  :doc "revocation restores the underlying protected-path decision"
  (let* ((path "/repo/.git/config")
         (session (mevedel-session--create :name "test"))
         (tool (mevedel-tool--create :name "Read" :read-only-p t))
         (mevedel-permission-rules nil)
         (mevedel-protected-paths '("**/.git/**")))
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
  (let* ((first '(:path "/tmp/first" :access read))
         (second '(:path "/tmp/second" :access read))
         (shared (list first second))
         (session (mevedel-session--create
                   :name "child" :resource-grants shared))
         (other (mevedel-session--create
                 :name "parent" :resource-grants shared)))
    (mevedel-permission-remove-session-resource-grant
     session "/tmp/second" 'read)
    (should (equal (list first)
                   (mevedel-session-resource-grants session)))
    (should (equal (list first second)
                   (mevedel-session-resource-grants other)))))

(mevedel-deftest mevedel-permission-remove-persistent-resource-grant ()
  ,test
  (test)
  :doc "revokes one persistent grant without changing command rules"
  (let* ((tmp-dir (make-temp-file "mevedel-test-" t))
         (mevedel-user-dir (file-name-concat tmp-dir "global/"))
         (path (file-name-concat tmp-dir "outside.el"))
         (ws (mevedel-workspace--create
              :type 'project :id "test" :root tmp-dir
              :name "test" :file-cache nil)))
    (unwind-protect
        (progn
          (mevedel-permission--save-persistent-rule ws "Read" 'allow)
          (mevedel-permission--save-persistent-resource-grant ws path 'read)
          (should (eq 'allow
                      (mevedel-check-permission
                       "Read"
                       :tool-struct (mevedel-tool--create
                                     :name "Read" :read-only-p t)
                       :path path :mode 'ask
                       :workspace-root "/different"
                       :resource-grants
                       (mevedel-permission--load-persistent-resource-grants
                        ws))))
          (mevedel-permission-remove-persistent-resource-grant ws path 'read)
          (should-not
           (mevedel-permission--load-persistent-resource-grants ws))
          (should (equal '(("Read" :action allow))
                         (mevedel-permission--load-persistent-rules ws)))
          (should (eq 'ask
                      (mevedel-check-permission
                       "Read"
                       :tool-struct (mevedel-tool--create
                                     :name "Read" :read-only-p t)
                       :path path :mode 'ask
                       :workspace-root "/different"
                       :resource-grants
                       (mevedel-permission--load-persistent-resource-grants
                        ws)))))
      (delete-directory tmp-dir t))))

(mevedel-deftest mevedel-permission--load-persistent-resource-grants ()
  ,test
  (test)
  :doc "drops malformed and relative grants from an editable store"
  (let* ((tmp-dir (make-temp-file "mevedel-test-" t))
         (ws (mevedel-workspace--create
              :type 'project :id "test" :root tmp-dir
              :name "test" :file-cache nil))
         (file (mevedel-permission--persistent-file ws)))
    (unwind-protect
        (progn
          (make-directory (file-name-directory file) t)
          (with-temp-file file
            (pp '(:rules nil
                  :resource-grants
                  ((:path "/tmp/read" :access read)
                   (:path "relative" :access write)
                   (:path "/tmp/execute" :access execute)
                   malformed))
                (current-buffer)))
          (should (equal '((:path "/tmp/read" :access read))
                         (mevedel-permission--load-persistent-resource-grants
                          ws))))
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
          (should (= (length (mevedel-permission--load-persistent-rules ws)) 1)))
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
                         (mevedel-permission--load-persistent-resource-grants
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


;;
;;; Permission-mode setter

(defmacro mevedel-test--with-saved-permission-mode (&rest body)
  "Execute BODY with the global `mevedel-permission-mode' preserved.
The setter writes to the global default, so tests that exercise it
must restore the prior value to avoid cross-test pollution."
  (declare (indent 0) (debug t))
  `(let ((mevedel-test--saved-mode (default-toplevel-value 'mevedel-permission-mode)))
     (unwind-protect (progn ,@body)
       (set-default-toplevel-value 'mevedel-permission-mode
                                   mevedel-test--saved-mode))))

(mevedel-deftest mevedel-permission-mode-effective ()
  ,test
  (test)
  :doc "uses session mode before data-buffer and global fallbacks"
  (mevedel-test--with-saved-permission-mode
    (let ((data-buf (generate-new-buffer " *mev-effective-data*")))
      (unwind-protect
          (let ((session (mevedel-session--create
                          :name "test" :permission-mode 'ask)))
            (set-default-toplevel-value 'mevedel-permission-mode 'full-auto)
            (with-current-buffer data-buf
              (setq-local mevedel--session
                          (mevedel-session--create
                           :name "data" :permission-mode 'auto)))
            (should (eq (mevedel-permission-mode-effective session data-buf)
                        'ask)))
        (when (buffer-live-p data-buf) (kill-buffer data-buf)))))

  :doc "uses data-buffer session when no session is passed"
  (let ((data-buf (generate-new-buffer " *mev-effective-data*")))
    (unwind-protect
        (with-current-buffer data-buf
          (setq-local mevedel--session
                      (mevedel-session--create
                       :name "data" :permission-mode 'auto))
          (should (eq (mevedel-permission-mode-effective nil data-buf)
                      'auto)))
      (when (buffer-live-p data-buf) (kill-buffer data-buf))))

  :doc "uses data-buffer local mode before global fallback"
  (mevedel-test--with-saved-permission-mode
    (let ((data-buf (generate-new-buffer " *mev-effective-data*")))
      (unwind-protect
          (progn
            (set-default-toplevel-value 'mevedel-permission-mode 'ask)
            (with-current-buffer data-buf
              (setq-local mevedel-permission-mode 'full-auto))
            (should (eq (mevedel-permission-mode-effective nil data-buf)
                        'full-auto)))
        (when (buffer-live-p data-buf) (kill-buffer data-buf)))))

  :doc "uses explicit surface buffer local mode before data-buffer fallback"
  (let ((data-buf (generate-new-buffer " *mev-effective-data*"))
        (surface-buf (generate-new-buffer " *mev-effective-surface*")))
    (unwind-protect
        (progn
          (with-current-buffer data-buf
            (setq-local mevedel-permission-mode 'ask))
          (with-current-buffer surface-buf
            (setq-local mevedel-permission-mode 'full-auto))
          (should (eq (mevedel-permission-mode-effective
                       nil data-buf surface-buf)
                      'full-auto)))
      (when (buffer-live-p data-buf) (kill-buffer data-buf))
      (when (buffer-live-p surface-buf) (kill-buffer surface-buf))))

  :doc "ignores caller local mode when a data-buffer is explicit"
  (let ((data-buf (generate-new-buffer " *mev-effective-data*")))
    (unwind-protect
        (with-temp-buffer
          (with-current-buffer data-buf
            (setq-local mevedel-permission-mode 'ask))
          (setq-local mevedel-permission-mode 'full-auto)
          (should (eq (mevedel-permission-mode-effective nil data-buf)
                      'ask)))
      (when (buffer-live-p data-buf) (kill-buffer data-buf))))

  :doc "falls back to the global mode"
  (mevedel-test--with-saved-permission-mode
    (set-default-toplevel-value 'mevedel-permission-mode 'auto)
    (should (eq (mevedel-permission-mode-effective) 'auto))))

(mevedel-deftest mevedel-permission-mode-label ()
  ,test
  (test)
  :doc "renders compact labels for permission modes"
  (should (equal "ask" (mevedel-permission-mode-label 'ask)))
  (should (equal "auto" (mevedel-permission-mode-label 'auto)))
  (should (equal "full-auto" (mevedel-permission-mode-label 'full-auto)))
  (should (equal "ask" (mevedel-permission-mode-label 'unknown))))

(mevedel-deftest mevedel-permission-mode--set ()
  ,test
  (test)
  :doc "no session context: updates the global default only"
  (mevedel-test--with-saved-permission-mode
    (set-default-toplevel-value 'mevedel-permission-mode 'ask)
    (with-temp-buffer
      (mevedel-permission-mode--set 'mevedel-permission-mode 'full-auto))
    (should (eq (default-toplevel-value 'mevedel-permission-mode) 'full-auto)))

  :doc "from data buffer: updates session slot and its buffer-local, leaves default untouched"
  (mevedel-test--with-saved-permission-mode
    (let ((data-buf (generate-new-buffer " *mev-test-data*")))
      (unwind-protect
          (let ((session (mevedel-session--create
                          :name "test" :permission-mode 'ask)))
            (with-current-buffer data-buf
              (setq-local mevedel--session session))
            (set-default-toplevel-value 'mevedel-permission-mode 'ask)
            (with-current-buffer data-buf
              (mevedel-permission-mode--set 'mevedel-permission-mode 'auto))
            (should (eq (mevedel-session-permission-mode session) 'auto))
            (should (eq (buffer-local-value 'mevedel-permission-mode data-buf)
                        'auto))
            ;; Global default is NOT touched.
            (should (eq (default-toplevel-value 'mevedel-permission-mode)
                        'ask)))
        (when (buffer-live-p data-buf) (kill-buffer data-buf)))))

  :doc "from view buffer: back-pointer resolves to the session; data + view locals both update"
  (mevedel-test--with-saved-permission-mode
    (let ((data-buf (generate-new-buffer " *mev-test-data*"))
          (view-buf (generate-new-buffer " *mev-test-view*")))
      (unwind-protect
          (let ((session (mevedel-session--create
                          :name "test" :permission-mode 'ask)))
            (with-current-buffer data-buf
              (setq-local mevedel--session session)
              (setq-local mevedel--view-buffer view-buf))
            (with-current-buffer view-buf
              (setq-local mevedel--data-buffer data-buf))
            (set-default-toplevel-value 'mevedel-permission-mode 'ask)
            (with-current-buffer view-buf
              (mevedel-permission-mode--set
               'mevedel-permission-mode 'auto))
            (should (eq (mevedel-session-permission-mode session) 'auto))
            (should (eq (buffer-local-value 'mevedel-permission-mode data-buf)
                        'auto))
            (should (eq (buffer-local-value 'mevedel-permission-mode view-buf)
                        'auto))
            (should (eq (default-toplevel-value 'mevedel-permission-mode)
                        'ask)))
        (when (buffer-live-p data-buf) (kill-buffer data-buf))
        (when (buffer-live-p view-buf) (kill-buffer view-buf)))))

  :doc "interactive aliases are rejected below the user-input boundary"
  (mevedel-test--with-saved-permission-mode
    (let ((data-buf (generate-new-buffer " *mev-test-data*")))
      (unwind-protect
          (let ((session (mevedel-session--create
                          :name "test" :permission-mode 'ask)))
            (with-current-buffer data-buf
              (setq-local mevedel--session session)
              (should-error
               (mevedel-permission-mode--set
                'mevedel-permission-mode 'edit)
               :type 'user-error))
            (should (eq (mevedel-session-permission-mode session)
                        'ask)))
        (when (buffer-live-p data-buf) (kill-buffer data-buf)))))

  :doc "multiple sessions: only the current session is modified"
  (mevedel-test--with-saved-permission-mode
    (let ((data-a (generate-new-buffer " *mev-test-a*"))
          (data-b (generate-new-buffer " *mev-test-b*")))
      (unwind-protect
          (let ((sess-a (mevedel-session--create
                         :name "a" :permission-mode 'ask))
                (sess-b (mevedel-session--create
                         :name "b" :permission-mode 'auto)))
            (with-current-buffer data-a
              (setq-local mevedel--session sess-a))
            (with-current-buffer data-b
              (setq-local mevedel--session sess-b))
            (set-default-toplevel-value 'mevedel-permission-mode 'ask)
            (with-current-buffer data-a
              (mevedel-permission-mode--set 'mevedel-permission-mode 'full-auto))
            (should (eq (mevedel-session-permission-mode sess-a) 'full-auto))
            (should (memq 'full-auto-mode
                          (mapcar #'mevedel-reminder-type
                                  (mevedel-session-reminders sess-a))))
            ;; Session B and the global default are untouched.
            (should (eq (mevedel-session-permission-mode sess-b) 'auto))
            (should (eq (default-toplevel-value 'mevedel-permission-mode)
                        'ask))
            ;; Buffer-local was set in data-a, not in data-b.
            (should (eq (buffer-local-value 'mevedel-permission-mode data-a)
                        'full-auto))
            (should-not (local-variable-p 'mevedel-permission-mode data-b)))
        (when (buffer-live-p data-a) (kill-buffer data-a))
        (when (buffer-live-p data-b) (kill-buffer data-b)))))

  :doc "setq-local bypasses setter: session slot stays stale"
  (mevedel-test--with-saved-permission-mode
    (let ((data-buf (generate-new-buffer " *mev-test-data*")))
      (unwind-protect
          (let ((session (mevedel-session--create
                          :name "test" :permission-mode 'ask)))
            (with-current-buffer data-buf
              (setq-local mevedel--session session)
              (setq-local mevedel-permission-mode 'full-auto))
            (should (eq (buffer-local-value 'mevedel-permission-mode data-buf)
                        'full-auto))
            (should (eq (mevedel-session-permission-mode session) 'ask)))
        (when (buffer-live-p data-buf) (kill-buffer data-buf))))))

(mevedel-deftest mevedel-permission--set-session-scoped ()
  ,test
  (test)
  :doc "generic helper routes to slot-setter when a session is current"
  (mevedel-test--with-saved-permission-mode
    (let ((data-buf (generate-new-buffer " *mev-test-data*"))
          (calls nil))
      (unwind-protect
          (let ((session (mevedel-session--create
                          :name "test" :permission-mode 'ask)))
            (with-current-buffer data-buf
              (setq-local mevedel--session session))
            (with-current-buffer data-buf
              (mevedel-permission--set-session-scoped
               'mevedel-permission-mode 'auto
               (lambda (s v) (push (cons s v) calls)
                 (setf (mevedel-session-permission-mode s) v))))
            (should (= (length calls) 1))
            (should (eq (cdar calls) 'auto))
            (should (eq (car (car calls)) session))
            (should (eq (mevedel-session-permission-mode session) 'auto)))
        (when (buffer-live-p data-buf) (kill-buffer data-buf)))))

  :doc "generic helper updates global default when no session is current"
  (mevedel-test--with-saved-permission-mode
    (let ((calls nil))
      (set-default-toplevel-value 'mevedel-permission-mode 'ask)
      (with-temp-buffer
        (mevedel-permission--set-session-scoped
         'mevedel-permission-mode 'auto
         (lambda (s v) (push (cons s v) calls))))
      (should-not calls)
      (should (eq (default-toplevel-value 'mevedel-permission-mode)
                  'auto)))))

(mevedel-deftest mevedel-permission-mode--get ()
  ,test
  (test)
  :doc "no session context: returns the global default"
  (mevedel-test--with-saved-permission-mode
    (set-default-toplevel-value 'mevedel-permission-mode 'full-auto)
    (with-temp-buffer
      (should (eq (mevedel-permission-mode--get 'mevedel-permission-mode)
                  'full-auto))))

  :doc "from data buffer: returns the session slot, not the global default"
  (mevedel-test--with-saved-permission-mode
    (let ((data-buf (generate-new-buffer " *mev-test-data*")))
      (unwind-protect
          (let ((session (mevedel-session--create
                          :name "test" :permission-mode 'auto)))
            (with-current-buffer data-buf
              (setq-local mevedel--session session))
            (set-default-toplevel-value 'mevedel-permission-mode 'ask)
            (with-current-buffer data-buf
              (should (eq (mevedel-permission-mode--get 'mevedel-permission-mode)
                          'auto))))
        (when (buffer-live-p data-buf) (kill-buffer data-buf)))))

  :doc "from view buffer: back-pointer resolves to the session slot"
  (mevedel-test--with-saved-permission-mode
    (let ((data-buf (generate-new-buffer " *mev-test-data*"))
          (view-buf (generate-new-buffer " *mev-test-view*")))
      (unwind-protect
          (let ((session (mevedel-session--create
                          :name "test" :permission-mode 'auto)))
            (with-current-buffer data-buf
              (setq-local mevedel--session session))
            (with-current-buffer view-buf
              (setq-local mevedel--data-buffer data-buf))
            (set-default-toplevel-value 'mevedel-permission-mode 'ask)
            (with-current-buffer view-buf
              (should (eq (mevedel-permission-mode--get 'mevedel-permission-mode)
                          'auto))))
        (when (buffer-live-p data-buf) (kill-buffer data-buf))
        (when (buffer-live-p view-buf) (kill-buffer view-buf))))))

(mevedel-deftest mevedel-permission-mode ()
  ,test
  (test)
  :doc ":local 'permanent makes variable auto-buffer-local"
  (should (local-variable-if-set-p 'mevedel-permission-mode))
  :doc ":local 'permanent sets permanent-local property"
  (should (get 'mevedel-permission-mode 'permanent-local))
  :doc "buffer-local binding survives kill-all-local-variables"
  (let ((buf (generate-new-buffer " *mev-test-permanent*")))
    (unwind-protect
        (with-current-buffer buf
          (setq-local mevedel-permission-mode 'full-auto)
          (kill-all-local-variables)
          (should (local-variable-p 'mevedel-permission-mode))
          (should (eq mevedel-permission-mode 'full-auto)))
      (when (buffer-live-p buf) (kill-buffer buf)))))


;;
;;; Allowed-tools parser

(defvar mevedel-permissions-test--fake-tools
  ;; Skip the real registry; build minimal fake tool structs whose
  ;; only relevant slots are the get-* getters that drive the
  ;; specifier-key inference.  Letting the parser tests run
  ;; independent of tool-registration order avoids the existing
  ;; load-order coupling.
  `(("Read"     . ,(mevedel-tool--create
                    :name "Read" :handler #'ignore
                    :get-path (lambda (_) "")))
    ("Edit"     . ,(mevedel-tool--create
                    :name "Edit" :handler #'ignore
                    :get-path (lambda (_) "")))
    ("Bash"     . ,(mevedel-tool--create
                    :name "Bash" :handler #'ignore
                    :get-pattern (lambda (_) "")))
    ("WebFetch" . ,(mevedel-tool--create
                    :name "WebFetch" :handler #'ignore
                    :get-domain (lambda (_) "")))
    ("Agent"    . ,(mevedel-tool--create
                    :name "Agent" :handler #'ignore
                    :get-name (lambda (_) "")))
    ("Ask"      . ,(mevedel-tool--create
                    :name "Ask" :handler #'ignore))))

(defmacro mevedel-permissions-test--with-fake-tools (&rest body)
  "Run BODY with `mevedel-tool-get' answering from the fake-tool table."
  (declare (indent 0))
  `(cl-letf (((symbol-function 'mevedel-tool-get)
              (lambda (name &optional _category)
                (cdr (assoc name mevedel-permissions-test--fake-tools)))))
     ,@body))

(mevedel-deftest mevedel-permission--tool-specifier-key ()
  ,test
  (test)
  :doc "Bash uses :pattern, WebFetch uses :domain"
  (mevedel-permissions-test--with-fake-tools
    (should (eq :pattern (mevedel-permission--tool-specifier-key "Bash")))
    (should (eq :domain  (mevedel-permission--tool-specifier-key "WebFetch"))))

  :doc "Read uses :path, Agent uses :name"
  (mevedel-permissions-test--with-fake-tools
    (should (eq :path (mevedel-permission--tool-specifier-key "Read")))
    (should (eq :name (mevedel-permission--tool-specifier-key "Agent"))))

  :doc "Unknown tool returns nil"
  (mevedel-permissions-test--with-fake-tools
    (should (null (mevedel-permission--tool-specifier-key "NonExistent"))))

  :doc "Tool with no specifier getter returns nil"
  (mevedel-permissions-test--with-fake-tools
    (should (null (mevedel-permission--tool-specifier-key "Ask")))))

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
                     :type 'test :id "root" :root root
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
            (should (equal (list root)
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
                   (plist-get context :specifier-value)))))

(mevedel-deftest mevedel-permission--checker-args ()
  ,test
  (test)
  :doc "returns the existing checker keyword interface from context"
  (let* ((tool (mevedel-tool--create :name "Read" :read-only-p t))
         (context (list :tool tool
                        :path "/tmp/file.txt"
                        :session-rules '(("Read" :action allow))
                        :mode 'ask
                        :workspace-root "/tmp/"
                        :allowed-roots '("/tmp/")
                        :exact-allowed-paths '("/tmp/file.txt")))
         (args (mevedel-permission--checker-args context)))
    (should (eq tool (plist-get args :tool-struct)))
    (should (equal "/tmp/file.txt" (plist-get args :path)))
    (should (equal '(("Read" :action allow))
                   (plist-get args :session-rules)))
    (should (eq 'ask (plist-get args :mode)))
    (should (equal '("/tmp/file.txt")
                   (plist-get args :exact-allowed-paths)))))

(mevedel-deftest mevedel-permission--parse-rule-string ()
  ,test
  (test)
  :doc "bare tool name -> unqualified allow rule"
  (mevedel-permissions-test--with-fake-tools
    (should (equal '("Read" :action allow)
                   (mevedel-permission--parse-rule-string "Read"))))

  :doc "qualified by exact pattern (Bash)"
  (mevedel-permissions-test--with-fake-tools
    (should (equal '("Bash" :pattern "git status" :action allow)
                   (mevedel-permission--parse-rule-string
                    "Bash(git status)"))))

  :doc "qualified by glob pattern (Bash)"
  (mevedel-permissions-test--with-fake-tools
    (should (equal '("Bash" :pattern "git status *" :action allow)
                   (mevedel-permission--parse-rule-string
                    "Bash(git status *)"))))

  :doc "qualified by domain (WebFetch)"
  (mevedel-permissions-test--with-fake-tools
    (should (equal '("WebFetch" :domain "example.com" :action allow)
                   (mevedel-permission--parse-rule-string
                    "WebFetch(example.com)"))))

  :doc "qualified by path (Edit)"
  (mevedel-permissions-test--with-fake-tools
    (should (equal '("Edit" :path "src/**" :action allow)
                   (mevedel-permission--parse-rule-string "Edit(src/**)"))))

  :doc "qualified by sub-agent name (Agent)"
  (mevedel-permissions-test--with-fake-tools
    (should (equal '("Agent" :name "verifier" :action allow)
                   (mevedel-permission--parse-rule-string "Agent(verifier)"))))

  :doc "malformed: no closing paren rejected"
  (mevedel-permissions-test--with-fake-tools
    (should-error (mevedel-permission--parse-rule-string "Bash(foo")
                  :type 'user-error))

  :doc "malformed: lowercase first char or empty rejected"
  (mevedel-permissions-test--with-fake-tools
    (should-error (mevedel-permission--parse-rule-string "bash(foo)")
                  :type 'user-error)
    (should-error (mevedel-permission--parse-rule-string "")
                  :type 'user-error))

  :doc "unknown tool name rejected"
  (mevedel-permissions-test--with-fake-tools
    (should-error (mevedel-permission--parse-rule-string "NonExistent")
                  :type 'user-error))

  :doc "qualifier on a tool without a specifier slot rejected"
  (mevedel-permissions-test--with-fake-tools
    (should-error (mevedel-permission--parse-rule-string "Ask(foo)")
                  :type 'user-error))

  :doc "non-string input rejected"
  (mevedel-permissions-test--with-fake-tools
    (should-error (mevedel-permission--parse-rule-string nil)
                  :type 'user-error)
    (should-error (mevedel-permission--parse-rule-string 42)
                  :type 'user-error)))


;;
;;; Bucket-aware permission resolution

(mevedel-deftest mevedel-permission--collect-buckets ()
  ,test
  (test)
  :doc "buckets returned in innermost-first order"
  ;; Pass 2 consumes buckets in innermost-first order.
  (let ((buckets (mevedel-permission--collect-buckets
                  '(:invocation-rule)
                  '(:request-rule)
                  '(:session-rule)
                  '(:persistent-rule))))
    (should (equal '(:invocation :request :session :persistent :defcustom)
                   (mapcar #'car buckets)))))

(mevedel-deftest mevedel-permission--any-deny ()
  ,test
  (test)
  :doc "deny in any bucket short-circuits pass 1"
  (let ((buckets-with-deny
         (mevedel-permission--collect-buckets
          nil nil
          '(("Bash" :pattern "rm *" :action deny))
          nil)))
    (should (mevedel-permission--any-deny
             buckets-with-deny "Bash" nil "rm /etc" nil nil)))

  :doc "no deny anywhere returns nil"
  (let ((buckets-no-deny
         (mevedel-permission--collect-buckets
          '(("Bash" :pattern "rm *" :action allow))
          nil nil nil)))
    (should-not (mevedel-permission--any-deny
                 buckets-no-deny "Bash" nil "rm /etc" nil nil))))

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
                 '(("Bash" :pattern "rm *" :action ask))))))

  :doc "Goal planning denies mutation despite explicit allow buckets"
  (test-mevedel-permissions--with-goal-phase 'planning
    (let ((mevedel-permission-rules nil)
          (bash (mevedel-tool--create :name "Bash" :read-only-p nil)))
      (should (eq 'deny
                  (mevedel-check-permission
                   "Bash"
                   :tool-struct bash
                   :pattern "echo hi"
                   :invocation-rules '(("Bash" :action allow))
                   :session-rules '(("Bash" :action allow))
                   :mode 'full-auto))))))

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

  :doc "Goal review denies non-read-only tools inside the workspace"
  (let* ((root (file-name-as-directory
                (make-temp-file "mevedel-workspace-plan-" t)))
         (child (file-name-concat root "file.el"))
         (mevedel-permission-rules nil)
         (mevedel-protected-paths nil)
         (mock-tool (mevedel-tool--create :name "Edit" :read-only-p nil)))
    (unwind-protect
        (test-mevedel-permissions--with-goal-phase 'reviewing
          (should (eq 'deny
                      (mevedel-check-permission
                       "Edit"
                       :tool-struct mock-tool
                       :path child
                       :workspace-root root
                       :mode 'full-auto))))
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
