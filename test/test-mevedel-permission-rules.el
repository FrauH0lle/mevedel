;;; test-mevedel-permission-rules.el -- Permission rule tests -*- lexical-binding: t -*-

;;; Commentary:

;; Tests permission rule parsing, matching, precedence buckets, and protected
;; path policy.

;;; Code:

(require 'mevedel-execution-target)
(require 'mevedel-permission-rules)
(require 'mevedel-tool-registry)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(defvar mevedel-permission-rules-test--fake-tools
  `(("Read" . ,(mevedel-tool--create
                 :name "Read" :handler #'ignore
                 :get-path (lambda (_) "")))
    ("Edit" . ,(mevedel-tool--create
                 :name "Edit" :handler #'ignore
                 :get-path (lambda (_) "")))
    ("Bash" . ,(mevedel-tool--create
                 :name "Bash" :handler #'ignore
                 :get-pattern (lambda (_) "")))
    ("WebFetch" . ,(mevedel-tool--create
                     :name "WebFetch" :handler #'ignore
                     :get-domain (lambda (_) "")))
    ("Agent" . ,(mevedel-tool--create
                  :name "Agent" :handler #'ignore
                  :get-name (lambda (_) "")))
    ("Ask" . ,(mevedel-tool--create
                :name "Ask" :handler #'ignore))))

(defmacro mevedel-permission-rules-test--with-fake-tools (&rest body)
  "Run BODY with `mevedel-tool-get' answering from the fake-tool table."
  (declare (indent 0) (debug t))
  `(cl-letf (((symbol-function 'mevedel-tool-get)
              (lambda (name &optional _category)
                (cdr (assoc name mevedel-permission-rules-test--fake-tools)))))
     ,@body))


;;
;;; Path pattern matching

(mevedel-deftest mevedel-permission-rules-match-path-p ()
  ,test
  (test)
  :doc "single star matches files in directory"
  (should (mevedel-permission-rules-match-path-p
           "/home/user/projects/foo.el" "/home/user/projects/*"))
  :doc "single star does not match across directories"
  (should-not (mevedel-permission-rules-match-path-p
               "/home/user/projects/sub/foo.el" "/home/user/projects/*"))
  :doc "double star matches across directories"
  (should (mevedel-permission-rules-match-path-p
           "/home/user/projects/sub/foo.el" "/home/user/projects/**"))
  :doc "double star matches nested paths"
  (should (mevedel-permission-rules-match-path-p
           "/home/user/projects/a/b/c/foo.el" "/home/user/projects/**"))
  :doc "trailing double star also matches the directory itself"
  (should (mevedel-permission-rules-match-path-p
           "/home/user/projects" "/home/user/projects/**"))
  :doc "trailing double star also matches the directory itself with slash"
  (should (mevedel-permission-rules-match-path-p
           "/home/user/projects/" "/home/user/projects/**"))
  :doc "double star matches across directories in interior"
  (should (mevedel-permission-rules-match-path-p
           "/repo/.git/config" "**/.git/**"))
  :doc "relative trailing globstar matches its directory root"
  (should (mevedel-permission-rules-match-path-p
           "/repo/.git" "**/.git/**"))
  :doc "tilde expansion in pattern"
  (should (mevedel-permission-rules-match-path-p
           (expand-file-name "~/.ssh/id_rsa") "~/.ssh/*"))
  :doc "home-relative trailing globstar matches its directory root"
  (should (mevedel-permission-rules-match-path-p
           (expand-file-name "~/.ssh") "~/.ssh/**"))
  :doc "tilde expansion uses the remote target home"
  (let ((target (mevedel-execution-target-create
                 "/ssh:user@host:/srv/project/")))
    (setf (mevedel-execution-target-environment target)
          '(("HOME" . "/home/user")))
    (should (mevedel-permission-rules-match-path-p
             "/ssh:user@host:/home/user/.ssh/id_rsa"
             "~/.ssh/*" target)))
  :doc "client absolute patterns stay client-only on remote targets"
  (let ((target (mevedel-execution-target-create
                 "/ssh:user@host:/srv/project/")))
    (setf (mevedel-execution-target-environment target)
          '(("HOME" . "/home/user")))
    (should-not (mevedel-permission-rules-match-path-p
                 "/ssh:user@host:/home/roland/private/key"
                 "/home/roland/private/**" target)))
  :doc "question mark matches single character"
  (should (mevedel-permission-rules-match-path-p
           "/home/user/file.el" "/home/user/fil?.el"))
  :doc "nil path returns nil"
  (should-not (mevedel-permission-rules-match-path-p nil "/some/pattern"))
  :doc "nil pattern returns nil"
  (should-not (mevedel-permission-rules-match-path-p "/some/path" nil))
  :doc "non-matching path returns nil"
  (should-not (mevedel-permission-rules-match-path-p
               "/other/path/file.el" "/home/user/*")))


;;
;;; Rule matching

(mevedel-deftest mevedel-permission-rules-find ()
  ,test
  (test)
  :doc "exact tool name match"
  (let ((rules '(("Read" :action allow))))
    (should (equal (length (mevedel-permission-rules-find rules "Read")) 1))
    (should (null (mevedel-permission-rules-find rules "Write"))))
  :doc "wildcard tool matches all"
  (let ((rules '(("*" :action ask))))
    (should (equal (length (mevedel-permission-rules-find rules "Read")) 1))
    (should (equal (length (mevedel-permission-rules-find rules "Write")) 1)))
  :doc "path rule matches only matching paths"
  (let ((rules '(("Edit" :path "/home/user/projects/*" :action allow))))
    (should (equal (length (mevedel-permission-rules-find
                            rules "Edit" :path "/home/user/projects/foo.el"))
                   1))
    (should (null (mevedel-permission-rules-find
                   rules "Edit" :path "/other/path/foo.el"))))
  :doc "non-path rule matches regardless of path"
  (let ((rules '(("Read" :action allow))))
    (should (equal (length (mevedel-permission-rules-find
                            rules "Read" :path "/any/path"))
                   1)))
  :doc "pattern rule matches only matching command strings"
  (let ((rules '(("Bash" :pattern "git log*" :action allow))))
    (should (equal (length (mevedel-permission-rules-find
                            rules "Bash" :pattern "git log --oneline"))
                   1))
    (should (null (mevedel-permission-rules-find
                   rules "Bash" :pattern "rm -rf"))))
  :doc "pattern prefix rule matches bare command and command with arguments"
  (let ((rules '(("Bash" :pattern "git log:*" :action allow))))
    (should (equal (length (mevedel-permission-rules-find
                            rules "Bash" :pattern "git log"))
                   1))
    (should (equal (length (mevedel-permission-rules-find
                            rules "Bash" :pattern "git log --oneline"))
                   1))
    (should (null (mevedel-permission-rules-find
                   rules "Bash" :pattern "git lollipop"))))
  :doc "domain rule matches only matching hosts"
  (let ((rules '(("WebFetch" :domain "*.example.com" :action allow))))
    (should (equal (length (mevedel-permission-rules-find
                            rules "WebFetch" :domain "api.example.com"))
                   1))
    (should (null (mevedel-permission-rules-find
                   rules "WebFetch" :domain "evil.org"))))
  :doc "name rule matches only matching names"
  (let ((rules '(("Agent" :name "explorer" :action allow))))
    (should (equal (length (mevedel-permission-rules-find
                            rules "Agent" :name "explorer"))
                   1))
    (should (null (mevedel-permission-rules-find
                   rules "Agent" :name "verifier")))))

(mevedel-deftest mevedel-permission-rules-action ()
  ,test
  (test)
  :doc "deny takes precedence over allow"
  (let ((rules '(("Edit" :action allow)
                 ("Edit" :action deny))))
    (should (eq (mevedel-permission-rules-action rules "Edit") 'deny)))
  :doc "ask takes precedence over allow"
  (let ((rules '(("Edit" :action allow)
                 ("Edit" :action ask))))
    (should (eq (mevedel-permission-rules-action rules "Edit") 'ask)))
  :doc "allow alone"
  (let ((rules '(("Read" :action allow))))
    (should (eq (mevedel-permission-rules-action rules "Read") 'allow)))
  :doc "no matching rules returns nil"
  (let ((rules '(("Read" :action allow))))
    (should-not (mevedel-permission-rules-action rules "Write")))
  :doc "specifier rule overrides generic rule of opposite action"
  ;; Generic deny should lose to specific allow for matching pattern
  (let ((rules '(("Bash" :action deny)
                 ("Bash" :pattern "echo*" :action allow))))
    (should (eq (mevedel-permission-rules-action
                 rules "Bash" :pattern "echo hello")
                'allow)))
  :doc "generic rule applies when specifier does not match"
  (let ((rules '(("Bash" :action deny)
                 ("Bash" :pattern "echo*" :action allow))))
    (should (eq (mevedel-permission-rules-action
                 rules "Bash" :pattern "rm -rf")
                'deny)))
  :doc "pattern specifier: deny wins over allow within specifier group"
  (let ((rules '(("Bash" :pattern "git*" :action allow)
                 ("Bash" :pattern "git push*" :action deny))))
    (should (eq (mevedel-permission-rules-action
                 rules "Bash" :pattern "git push origin")
                'deny)))
  :doc "domain specifier match allows"
  (let ((rules '(("WebFetch" :domain "*.example.com" :action allow))))
    (should (eq (mevedel-permission-rules-action
                 rules "WebFetch" :domain "api.example.com")
                'allow)))
  :doc "name specifier match allows"
  (let ((rules '(("Agent" :name "explorer" :action allow))))
    (should (eq (mevedel-permission-rules-action
                 rules "Agent" :name "explorer")
                'allow)))
  :doc "execution-level qualifiers never match ordinary tool resolution"
  (let ((rules '(("Bash" :sandbox-permissions require-escalated
                         :action allow))))
    (should-not (mevedel-permission-rules-action rules "Bash"))))


;;
;;; Protected paths

(mevedel-deftest mevedel-permission-rules-path-protected-p ()
  ,test
  (test)
  :doc "git directory is protected"
  (let ((mevedel-protected-paths '(("**/.git/**" . read-only))))
    (should (mevedel-permission-rules-path-protected-p "/repo/.git"))
    (should (mevedel-permission-rules-path-protected-p "/repo/.git/config")))
  :doc "home-relative trailing globstar protects its directory root"
  (let ((mevedel-protected-paths '(("~/.ssh/**" . inaccessible))))
    (should (mevedel-permission-rules-path-protected-p
             (expand-file-name "~/.ssh"))))
  :doc "ssh directory is protected"
  (let ((mevedel-protected-paths '(("~/.ssh/*" . inaccessible))))
    (should (mevedel-permission-rules-path-protected-p
             (expand-file-name "~/.ssh/id_rsa"))))
  :doc "remote ssh directory is protected from the target home"
  (let ((mevedel-protected-paths '(("~/.ssh/*" . inaccessible)))
        (target (mevedel-execution-target-create
                 "/ssh:user@host:/srv/project/")))
    (setf (mevedel-execution-target-environment target)
          '(("HOME" . "/home/user")))
    (should (mevedel-permission-rules-path-protected-p
             "/ssh:user@host:/home/user/.ssh/id_rsa" target)))
  :doc "default cloud credentials are protected from the target home"
  (let ((target (mevedel-execution-target-create
                 "/ssh:user@host:/srv/project/")))
    (setf (mevedel-execution-target-environment target)
          '(("HOME" . "/home/user")))
    (dolist (path '(".aws/credentials"
                    ".azure/accessTokens.json"
                    ".config/gcloud/credentials.db"
                    ".kube/config"))
      (should
       (mevedel-permission-rules-path-protected-p
        (concat "/ssh:user@host:/home/user/" path) target))))
  :doc "normal path is not protected"
  (let ((mevedel-protected-paths
         '(("**/.git/**" . read-only) ("~/.ssh/*" . inaccessible))))
    (should-not (mevedel-permission-rules-path-protected-p "/home/user/projects/foo.el")))
  :doc "nil path is not protected"
  (let ((mevedel-protected-paths '(("**/.git/**" . read-only))))
    (should-not (mevedel-permission-rules-path-protected-p nil))))

(mevedel-deftest mevedel-permission-protected-path-policy ()
  ,test
  (test)
  :doc "valid protected path modes are returned unchanged"
  (let ((mevedel-protected-paths
         '(("**/.git/**" . read-only)
           ("~/.ssh/**" . inaccessible))))
    (should (eq (mevedel-permission-protected-path-policy)
                mevedel-protected-paths)))
  :doc "the superseded string-only shape is rejected"
  (let ((mevedel-protected-paths '("**/.git/**")))
    (should-error (mevedel-permission-protected-path-policy) :type 'error))
  :doc "unknown protected path modes are rejected"
  (let ((mevedel-protected-paths '(("**/.git/**" . writable))))
    (should-error (mevedel-permission-protected-path-policy) :type 'error)))

(mevedel-deftest mevedel-permission--tool-specifier-key ()
  ,test
  (test)
  :doc "Bash uses :pattern, WebFetch uses :domain"
  (mevedel-permission-rules-test--with-fake-tools
    (should (eq :pattern (mevedel-permission--tool-specifier-key "Bash")))
    (should (eq :domain  (mevedel-permission--tool-specifier-key "WebFetch"))))

  :doc "Read uses :path, Agent uses :name"
  (mevedel-permission-rules-test--with-fake-tools
    (should (eq :path (mevedel-permission--tool-specifier-key "Read")))
    (should (eq :name (mevedel-permission--tool-specifier-key "Agent"))))

  :doc "Unknown tool returns nil"
  (mevedel-permission-rules-test--with-fake-tools
    (should (null (mevedel-permission--tool-specifier-key "NonExistent"))))

  :doc "Tool with no specifier getter returns nil"
  (mevedel-permission-rules-test--with-fake-tools
    (should (null (mevedel-permission--tool-specifier-key "Ask")))))

(mevedel-deftest mevedel-permission-rules-parse ()
  ,test
  (test)
  :doc "bare tool name -> unqualified allow rule"
  (mevedel-permission-rules-test--with-fake-tools
    (should (equal '("Read" :action allow)
                   (mevedel-permission-rules-parse "Read"))))

  :doc "qualified by exact pattern (Bash)"
  (mevedel-permission-rules-test--with-fake-tools
    (should (equal '("Bash" :pattern "git status" :action allow)
                   (mevedel-permission-rules-parse
                    "Bash(git status)"))))

  :doc "qualified by glob pattern (Bash)"
  (mevedel-permission-rules-test--with-fake-tools
    (should (equal '("Bash" :pattern "git status *" :action allow)
                   (mevedel-permission-rules-parse
                    "Bash(git status *)"))))

  :doc "qualified by domain (WebFetch)"
  (mevedel-permission-rules-test--with-fake-tools
    (should (equal '("WebFetch" :domain "example.com" :action allow)
                   (mevedel-permission-rules-parse
                    "WebFetch(example.com)"))))

  :doc "qualified by path (Edit)"
  (mevedel-permission-rules-test--with-fake-tools
    (should (equal '("Edit" :path "src/**" :action allow)
                   (mevedel-permission-rules-parse "Edit(src/**)"))))

  :doc "qualified by sub-agent name (Agent)"
  (mevedel-permission-rules-test--with-fake-tools
    (should (equal '("Agent" :name "verifier" :action allow)
                   (mevedel-permission-rules-parse "Agent(verifier)"))))

  :doc "malformed: no closing paren rejected"
  (mevedel-permission-rules-test--with-fake-tools
    (should-error (mevedel-permission-rules-parse "Bash(foo")
                  :type 'user-error))

  :doc "malformed: lowercase first char or empty rejected"
  (mevedel-permission-rules-test--with-fake-tools
    (should-error (mevedel-permission-rules-parse "bash(foo)")
                  :type 'user-error)
    (should-error (mevedel-permission-rules-parse "")
                  :type 'user-error))

  :doc "unknown tool name rejected"
  (mevedel-permission-rules-test--with-fake-tools
    (should-error (mevedel-permission-rules-parse "NonExistent")
                  :type 'user-error))

  :doc "qualifier on a tool without a specifier slot rejected"
  (mevedel-permission-rules-test--with-fake-tools
    (should-error (mevedel-permission-rules-parse "Ask(foo)")
                  :type 'user-error))

  :doc "non-string input rejected"
  (mevedel-permission-rules-test--with-fake-tools
    (should-error (mevedel-permission-rules-parse nil)
                  :type 'user-error)
    (should-error (mevedel-permission-rules-parse 42)
                  :type 'user-error)))


;;
;;; Bucket-aware permission resolution

(mevedel-deftest mevedel-permission-rules-collect-buckets ()
  ,test
  (test)
  :doc "buckets returned in innermost-first order"
  ;; Pass 2 consumes buckets in innermost-first order.
  (let ((buckets (mevedel-permission-rules-collect-buckets
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
         (mevedel-permission-rules-collect-buckets
          nil nil
          '(("Bash" :pattern "rm *" :action deny))
          nil)))
    (should (mevedel-permission--any-deny
             buckets-with-deny "Bash" nil "rm /etc" nil nil)))

  :doc "no deny anywhere returns nil"
  (let ((buckets-no-deny
         (mevedel-permission-rules-collect-buckets
          '(("Bash" :pattern "rm *" :action allow))
          nil nil nil)))
    (should-not (mevedel-permission--any-deny
                 buckets-no-deny "Bash" nil "rm /etc" nil nil))))

(mevedel-deftest mevedel-permission-rules-bucket-decision ()
  ,test
  (test)
  :doc "an outer deny remains absolute over an inner allow"
  (let ((mevedel-permission-rules nil)
        (buckets
         (mevedel-permission-rules-collect-buckets
          '(("Bash" :path "/secret" :action allow))
          nil
          '(("Bash" :path "/secret" :action deny))
          nil)))
    (should (eq 'deny
                (mevedel-permission-rules-bucket-decision
                 buckets "Bash" "/secret" nil nil nil))))

  :doc "without a deny the innermost matching action wins"
  (let ((mevedel-permission-rules nil)
        (buckets
         (mevedel-permission-rules-collect-buckets
          '(("Bash" :path "/secret" :action ask))
          nil
          '(("Bash" :path "/secret" :action allow))
          nil)))
    (should (eq 'ask
                (mevedel-permission-rules-bucket-decision
                 buckets "Bash" "/secret" nil nil nil)))))

(mevedel-deftest mevedel-permission-rules-execution-level-decision ()
  ,test
  (test)
  :doc "direct scoped rule authorizes only its requested execution level"
  (let ((mevedel-permission-rules nil)
        (buckets
         (mevedel-permission-rules-collect-buckets
          nil nil
          '(("Bash" :pattern "emacs --batch *"
                    :sandbox-permissions require-escalated
                    :action allow))
          nil)))
    (should
     (eq 'allow
         (mevedel-permission-rules-execution-level-decision
          buckets "Bash" 'require-escalated "emacs --batch -Q")))
    (should-not
     (mevedel-permission-rules-execution-level-decision
      buckets "Bash" 'require-escalated "curl example.test")))

  :doc "deliberately broad direct rule may authorize full escalation"
  (let ((mevedel-permission-rules
         '(("Eval" :sandbox-permissions require-escalated :action allow))))
    (should
     (eq 'allow
         (mevedel-permission-rules-execution-level-decision
          (mevedel-permission-rules-collect-buckets nil nil nil nil)
          "Eval" 'require-escalated "(message \"hello\")"))))

  :doc "delegated allow cannot grant full escalation"
  (let ((mevedel-permission-rules nil)
        (buckets
         (mevedel-permission-rules-collect-buckets
          '(("Bash" :sandbox-permissions require-escalated :action allow))
          nil nil nil)))
    (should-not
     (mevedel-permission-rules-execution-level-decision
      buckets "Bash" 'require-escalated "pwd")))

  :doc "delegated deny remains final over a direct allow"
  (let ((mevedel-permission-rules
         '(("Bash" :sandbox-permissions require-escalated :action allow)))
        (buckets
         (mevedel-permission-rules-collect-buckets
          '(("Bash" :sandbox-permissions require-escalated :action deny))
          nil nil nil)))
    (should
     (eq 'deny
         (mevedel-permission-rules-execution-level-decision
          buckets "Bash" 'require-escalated "pwd")))))

(mevedel-deftest mevedel-permission-rules-qualified-buckets ()
  ,test
  (test)
  :doc "retains direct qualified authority and delegated qualified denies"
  (should
   (equal
    '((:invocation
       ("Bash" :pattern "rm *" :action deny))
      (:request)
      (:session
       ("Bash" :pattern "pwd" :action allow)))
    (mevedel-permission-rules-qualified-buckets
     '((:invocation
        ("Bash" :pattern "pwd"
                :sandbox-permissions require-escalated :action allow)
        ("Bash" :pattern "rm *"
                :sandbox-permissions require-escalated :action deny))
       (:request
        ("Bash" :pattern "pwd"
                :sandbox-permissions use-default :action deny))
       (:session
        ("Bash" :pattern "pwd"
                :sandbox-permissions require-escalated :action allow)))
     :sandbox-permissions 'require-escalated))))

(provide 'test-mevedel-permission-rules)
;;; test-mevedel-permission-rules.el ends here
