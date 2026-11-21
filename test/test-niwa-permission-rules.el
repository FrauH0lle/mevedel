;;; test-niwa-permission-rules.el -- Permission rule tests -*- lexical-binding: t -*-

;;; Commentary:

;; Tests permission rule parsing, matching, precedence buckets, and protected
;; path policy.

;;; Code:

(require 'niwa-execution-target)
(require 'niwa-permission-rules)
(require 'niwa-tool-registry)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(defvar niwa-permission-rules-test--fake-tools
  `(("Read" . ,(niwa-tool--create
                 :name "Read" :handler #'ignore
                 :get-path (lambda (_) "")))
    ("Edit" . ,(niwa-tool--create
                 :name "Edit" :handler #'ignore
                 :get-path (lambda (_) "")))
    ("Bash" . ,(niwa-tool--create
                 :name "Bash" :handler #'ignore
                 :get-pattern (lambda (_) "")))
    ("WebFetch" . ,(niwa-tool--create
                     :name "WebFetch" :handler #'ignore
                     :get-domain (lambda (_) "")))
    ("Agent" . ,(niwa-tool--create
                  :name "Agent" :handler #'ignore
                  :get-name (lambda (_) "")))
    ("Ask" . ,(niwa-tool--create
                :name "Ask" :handler #'ignore))))

(defmacro niwa-permission-rules-test--with-fake-tools (&rest body)
  "Run BODY with `niwa-tool-get' answering from the fake-tool table."
  (declare (indent 0) (debug t))
  `(cl-letf (((symbol-function 'niwa-tool-get)
              (lambda (name &optional _category)
                (cdr (assoc name niwa-permission-rules-test--fake-tools)))))
     ,@body))


;;
;;; Path pattern matching

(niwa-deftest niwa-permission-rules-match-path-p ()
  ,test
  (test)
  :doc "single star matches files in directory"
  (should (niwa-permission-rules-match-path-p
           "/home/user/projects/foo.el" "/home/user/projects/*"))
  :doc "single star does not match across directories"
  (should-not (niwa-permission-rules-match-path-p
               "/home/user/projects/sub/foo.el" "/home/user/projects/*"))
  :doc "double star matches across directories"
  (should (niwa-permission-rules-match-path-p
           "/home/user/projects/sub/foo.el" "/home/user/projects/**"))
  :doc "double star matches nested paths"
  (should (niwa-permission-rules-match-path-p
           "/home/user/projects/a/b/c/foo.el" "/home/user/projects/**"))
  :doc "trailing double star also matches the directory itself"
  (should (niwa-permission-rules-match-path-p
           "/home/user/projects" "/home/user/projects/**"))
  :doc "trailing double star also matches the directory itself with slash"
  (should (niwa-permission-rules-match-path-p
           "/home/user/projects/" "/home/user/projects/**"))
  :doc "double star matches across directories in interior"
  (should (niwa-permission-rules-match-path-p
           "/repo/.git/config" "**/.git/**"))
  :doc "relative trailing globstar matches its directory root"
  (should (niwa-permission-rules-match-path-p
           "/repo/.git" "**/.git/**"))
  :doc "tilde expansion in pattern"
  (should (niwa-permission-rules-match-path-p
           (expand-file-name "~/.ssh/id_rsa") "~/.ssh/*"))
  :doc "home-relative trailing globstar matches its directory root"
  (should (niwa-permission-rules-match-path-p
           (expand-file-name "~/.ssh") "~/.ssh/**"))
  :doc "tilde expansion uses the remote target home"
  (let ((target (niwa-execution-target-create
                 "/ssh:user@host:/srv/project/")))
    (setf (niwa-execution-target-environment target)
          '(("HOME" . "/home/user")))
    (should (niwa-permission-rules-match-path-p
             "/ssh:user@host:/home/user/.ssh/id_rsa"
             "~/.ssh/*" target)))
  :doc "client absolute patterns stay client-only on remote targets"
  (let ((target (niwa-execution-target-create
                 "/ssh:user@host:/srv/project/")))
    (setf (niwa-execution-target-environment target)
          '(("HOME" . "/home/user")))
    (should-not (niwa-permission-rules-match-path-p
                 "/ssh:user@host:/home/roland/private/key"
                 "/home/roland/private/**" target)))
  :doc "question mark matches single character"
  (should (niwa-permission-rules-match-path-p
           "/home/user/file.el" "/home/user/fil?.el"))
  :doc "nil path returns nil"
  (should-not (niwa-permission-rules-match-path-p nil "/some/pattern"))
  :doc "nil pattern returns nil"
  (should-not (niwa-permission-rules-match-path-p "/some/path" nil))
  :doc "non-matching path returns nil"
  (should-not (niwa-permission-rules-match-path-p
               "/other/path/file.el" "/home/user/*")))


;;
;;; Rule matching

(niwa-deftest niwa-permission--match-command-grant ()
  ,test
  (test)
  :doc "wildcards cover ordinary arguments, never syntax that runs another command"
  (pcase-dolist (`(,pattern ,command ,expected)
                 '(("npx a *" "npx a test" t)
                   ("npx a *" "npx a eval '(load \"x\"); (y)'" t)
                   ("npx a *" "npx a $'x;y'" t)
                   ("npx a *" "npx a test \\; rm b" t)
                   ("npx a *" "npx a test 2>&1" t)
                   ("npx a *" "npx a test >&2" t)
                   ("npx a *" "npx a test 2>&-" t)
                   ("npx a *" "npx a test >&2-" t)
                   ("npx a *" "npx a test 2>&1 >&2" t)
                   ("npx a *" "npx a test >&1foo" nil)
                   ("npx a *" "npx a test >&2\"q\"" nil)
                   ("npx a *" "npx a test >&0z" nil)
                   ("git status:*" "git status >&1foo" nil)
                   ("npx a *" "npx a test 2>&1 | cat" nil)
                   ("npx a *" "npx a test &>log" nil)
                   ("npx a *" "npx a test > out" nil)
                   ("npx a *" "npx a test >> out" nil)
                   ("npx a *" "npx a test 2> out" nil)
                   ("npx a *" "npx a test < in" nil)
                   ("npx a *" "npx a test >&out" nil)
                   ("npx a *" "npx a test '>' x \">\" y \\> z" t)
                   ("git status:*" "git status" t)
                   ("git status:*" "git status 2>&1" t)
                   ("git status:*" "git status > /home/u/.bashrc" nil)
                   ("git status:*" "git status > src/main.el" nil)
                   ("git status:*" "git status >> ~/.profile" nil)
                   ("git status > *" "git status > out" t)
                   ("git status > out" "git status > out" t)
                   ("npx a *" "npx a test && rm b" nil)
                   ("npx a *" "npx a test; rm b" nil)
                   ("npx a *" "npx a test | sh" nil)
                   ("npx a *" "npx a test\nrm b" nil)
                   ("npx a *" "npx a test & rm b" nil)
                   ("npx a *" "npx a $(rm b)" nil)
                   ("npx a *" "npx a \"$(rm b)\"" nil)
                   ("npx a *" "npx a `rm b`" nil)
                   ("npx a *" "npx a (rm b)" nil)
                   ("npx a *" "npx a $'\\''; rm b '" nil)
                   ("npx a *" "npx a \\>& rm b" nil)
                   ("npx a *" "NPX a test" nil)
                   ("git add:*" "git add" t)
                   ("git add:*" "git add x" t)
                   ("git add:*" "git addx" nil)
                   ("git add:*" "git add x; rm y" nil)
                   ("make -j$(nproc) *" "make -j$(nproc) all" t)
                   ("cd * && make" "cd sub && make" t)
                   ("cd * && make" "cd $(rm b) && make" nil)))
    (should (eq expected
                (and (niwa-permission--match-command-grant pattern command) t)))))

(niwa-deftest niwa-permission-rules-strip-safe-assignments ()
  ,test
  (test)
  :doc "removes plain leading assignments to listed names from each segment"
  (pcase-dolist (`(,command ,expected)
                 '(("A=1 cmd" "cmd")
                   ("A=1 A=2 cmd x" "cmd x")
                   ("A='a b' cmd" "cmd")
                   ("A=\"a b\" cmd" "cmd")
                   ("cd x && A=1 cmd" "cd x && cmd")
                   ("A=1 B=2 cmd" "B=2 cmd")
                   ("B=2 A=1 cmd" "B=2 A=1 cmd")
                   ("a=1 cmd" "a=1 cmd")
                   ("A=$(x) cmd" "A=$(x) cmd")
                   ("A=\"$(x)\" cmd" "A=\"$(x)\" cmd")
                   ("A=$HOME cmd" "A=$HOME cmd")
                   ("A=1" "A=1")
                   ("echo A=1" "echo A=1")))
    (should (equal expected
                   (niwa-permission-rules-strip-safe-assignments
                    command '("A")))))
  :doc "leaves commands unchanged without names or a string"
  (should (equal "A=1 cmd" (niwa-permission-rules-strip-safe-assignments "A=1 cmd" nil)))
  (should-not (niwa-permission-rules-strip-safe-assignments nil '("A")))
  :doc "scans a long assignment ending at a separator in linear time"
  (let* ((command (concat "S=/" (make-string 24 ?a) "; ls"))
         (start (float-time))
         (result (niwa-permission-rules-strip-safe-assignments command '("A"))))
    (should (equal command result))
    (should (< (- (float-time) start) 0.2))))

(niwa-deftest niwa-permission-rules-find ()
  ,test
  (test)
  :doc "literal Eval expressions stay distinct from deliberately authored pattern rules"
  (let* ((literal '("Eval" :expression "(message \"*\")" :action allow))
         (pattern '("Eval" :pattern "(message \"*\")" :action ask))
         (rules (list literal pattern)))
    (should (equal rules (niwa-permission-rules-find
                          rules "Eval" :pattern "(message \"*\")")))
    (should (equal (list pattern) (niwa-permission-rules-find
                                  rules "Eval" :pattern "(message \"different\")")))
    (should-not (niwa-permission-rules-find
                 '(("*" :expression "(message \"*\")" :action allow))
                 "Bash" :pattern "(message \"*\")")))
  :doc "path allow rules are case-sensitive; deny and ask rules still fold"
  (let ((allow '(("Write" :path "/home/u/proj/**" :action allow)))
        (deny '(("Write" :path "/home/u/proj/**" :action deny)))
        (ask '(("Write" :path "/home/u/proj/**" :action ask))))
    (should (null (niwa-permission-rules-find
                   allow "Write" :path "/home/u/PROJ/x")))
    (should (null (niwa-permission-rules-action
                   allow "Write" :path "/home/u/PROJ/x")))
    (should (eq 'allow (niwa-permission-rules-action
                        allow "Write" :path "/home/u/proj/x")))
    (should (= 1 (length (niwa-permission-rules-find
                          deny "Write" :path "/home/u/PROJ/x"))))
    (should (= 1 (length (niwa-permission-rules-find
                          ask "Write" :path "/home/u/PROJ/x")))))
  :doc "exact tool name match"
  (let ((rules '(("Read" :action allow))))
    (should (equal (length (niwa-permission-rules-find rules "Read")) 1))
    (should (null (niwa-permission-rules-find rules "Write"))))
  :doc "wildcard tool matches all"
  (let ((rules '(("*" :action ask))))
    (should (equal (length (niwa-permission-rules-find rules "Read")) 1))
    (should (equal (length (niwa-permission-rules-find rules "Write")) 1)))
  :doc "path rule matches only matching paths"
  (let ((rules '(("Edit" :path "/home/user/projects/*" :action allow))))
    (should (equal (length (niwa-permission-rules-find
                            rules "Edit" :path "/home/user/projects/foo.el"))
                   1))
    (should (null (niwa-permission-rules-find
                   rules "Edit" :path "/other/path/foo.el"))))
  :doc "non-path rule matches regardless of path"
  (let ((rules '(("Read" :action allow))))
    (should (equal (length (niwa-permission-rules-find
                            rules "Read" :path "/any/path"))
                   1)))
  :doc "pattern rule matches only matching command strings"
  (let ((rules '(("Bash" :pattern "git log*" :action allow))))
    (should (equal (length (niwa-permission-rules-find
                            rules "Bash" :pattern "git log --oneline"))
                   1))
    (should (null (niwa-permission-rules-find
                   rules "Bash" :pattern "rm -rf"))))
  :doc "pattern prefix rule matches bare command and command with arguments"
  (let ((rules '(("Bash" :pattern "git log:*" :action allow))))
    (should (equal (length (niwa-permission-rules-find
                            rules "Bash" :pattern "git log"))
                   1))
    (should (equal (length (niwa-permission-rules-find
                            rules "Bash" :pattern "git log --oneline"))
                   1))
    (should (null (niwa-permission-rules-find
                   rules "Bash" :pattern "git lollipop"))))
  :doc "domain rule matches only matching hosts"
  (let ((rules '(("WebFetch" :domain "*.example.com" :action allow))))
    (should (equal (length (niwa-permission-rules-find
                            rules "WebFetch" :domain "api.example.com"))
                   1))
    (should (null (niwa-permission-rules-find
                   rules "WebFetch" :domain "evil.org"))))
  :doc "name rule matches only matching names"
  (let ((rules '(("Agent" :name "explorer" :action allow))))
    (should (equal (length (niwa-permission-rules-find
                            rules "Agent" :name "explorer"))
                   1))
    (should (null (niwa-permission-rules-find
                   rules "Agent" :name "verifier")))))

(niwa-deftest niwa-permission-rules-action ()
  ,test
  (test)
  :doc "deny takes precedence over allow"
  (let ((rules '(("Edit" :action allow)
                 ("Edit" :action deny))))
    (should (eq (niwa-permission-rules-action rules "Edit") 'deny)))
  :doc "ask takes precedence over allow"
  (let ((rules '(("Edit" :action allow)
                 ("Edit" :action ask))))
    (should (eq (niwa-permission-rules-action rules "Edit") 'ask)))
  :doc "allow alone"
  (let ((rules '(("Read" :action allow))))
    (should (eq (niwa-permission-rules-action rules "Read") 'allow)))
  :doc "no matching rules returns nil"
  (let ((rules '(("Read" :action allow))))
    (should-not (niwa-permission-rules-action rules "Write")))
  :doc "specifier rule overrides generic rule of opposite action"
  ;; Generic deny should lose to specific allow for matching pattern
  (let ((rules '(("Bash" :action deny)
                 ("Bash" :pattern "echo*" :action allow))))
    (should (eq (niwa-permission-rules-action
                 rules "Bash" :pattern "echo hello")
                'allow)))
  :doc "generic rule applies when specifier does not match"
  (let ((rules '(("Bash" :action deny)
                 ("Bash" :pattern "echo*" :action allow))))
    (should (eq (niwa-permission-rules-action
                 rules "Bash" :pattern "rm -rf")
                'deny)))
  :doc "pattern specifier: deny wins over allow within specifier group"
  (let ((rules '(("Bash" :pattern "git*" :action allow)
                 ("Bash" :pattern "git push*" :action deny))))
    (should (eq (niwa-permission-rules-action
                 rules "Bash" :pattern "git push origin")
                'deny)))
  :doc "domain specifier match allows"
  (let ((rules '(("WebFetch" :domain "*.example.com" :action allow))))
    (should (eq (niwa-permission-rules-action
                 rules "WebFetch" :domain "api.example.com")
                'allow)))
  :doc "name specifier match allows"
  (let ((rules '(("Agent" :name "explorer" :action allow))))
    (should (eq (niwa-permission-rules-action
                 rules "Agent" :name "explorer")
                'allow)))
  :doc "execution-level qualifiers never match ordinary tool resolution"
  (let ((rules '(("Bash" :sandbox-permissions require-escalated
                         :action allow))))
    (should-not (niwa-permission-rules-action rules "Bash"))))


;;
;;; Protected paths

(niwa-deftest niwa-permission-rules--path-contained-p ()
  ,test
  (test)
  :doc "matches the root and descendants without matching prefixed siblings"
  (should (niwa-permission-rules--path-contained-p "/srv/tree" "/srv/tree"))
  (should (niwa-permission-rules--path-contained-p
           "/srv/tree/sub/file" "/srv/tree"))
  (should-not (niwa-permission-rules--path-contained-p
               "/srv/tree-old/file" "/srv/tree"))
  :doc "a doubled leading slash names the same tree on either side"
  (should (niwa-permission-rules--path-contained-p "//srv/tree/file" "/srv/tree"))
  (should (niwa-permission-rules--path-contained-p "/srv/tree/file" "//srv/tree"))
  (should (niwa-permission-rules-path-in-allowed-roots-p
           "//srv/tree/file" '("/srv/tree/"))))

(niwa-deftest niwa-permission-rules-path-protected-p ()
  ,test
  (test)
  :doc "git directory is protected"
  (let ((niwa-protected-paths '(("**/.git/**" . read-only))))
    (should (niwa-permission-rules-path-protected-p "/repo/.git"))
    (should (niwa-permission-rules-path-protected-p "/repo/.git/config")))
  :doc "home-relative trailing globstar protects its directory root"
  (let ((niwa-protected-paths '(("~/.ssh/**" . inaccessible))))
    (should (niwa-permission-rules-path-protected-p
             (expand-file-name "~/.ssh"))))
  :doc "a doubled leading slash cannot escape an inaccessible home rule"
  (let ((niwa-protected-paths '(("~/.ssh/**" . inaccessible)
                                   ("/srv/secret/**" . inaccessible))))
    (should (niwa-permission-rules-path-protected-p
             (concat "/" (expand-file-name "~/.ssh/id_ed25519")) nil 'read))
    (should (niwa-permission-rules-path-protected-p "//srv/secret/key" nil 'read)))
  :doc "a workspace symlink or quoted name reaching a protected tree is protected"
  (let* ((top (file-truename (make-temp-file "niwa-protected-link-" t)))
         (outside (file-name-concat top "outside"))
         (ws (file-name-concat top "ws"))
         (niwa-protected-paths `((,(concat outside "/.ssh/**") . inaccessible))))
    (unwind-protect
        (progn
          (make-directory (file-name-concat outside ".ssh") t)
          (make-directory ws)
          (write-region "DECOY" nil (file-name-concat outside ".ssh/key") nil 'silent)
          (make-symbolic-link (file-name-concat outside ".ssh") (file-name-concat ws "s"))
          (should (niwa-permission-rules-path-protected-p
                   (file-name-concat ws "s/key") nil 'read))
          ;; A missing tail below the link is still inside the tree.
          (should (niwa-permission-rules-path-protected-p
                   (file-name-concat ws "s/new") nil 'read))
          (should (niwa-permission-rules-path-protected-p
                   (concat "/:" outside "/.ssh/key") nil 'read))
          ;; Control: an ordinary workspace path stays unprotected.
          (should-not (niwa-permission-rules-path-protected-p
                       (file-name-concat ws "plain") nil 'read)))
      (delete-directory top t)))
  :doc "read access respects the protection level"
  (let ((niwa-protected-paths '(("**/.git/**" . read-only)
                                   ("**/.ssh/**" . inaccessible))))
    (should-not (niwa-permission-rules-path-protected-p
                 "/repo/.git/config" nil 'read))
    (should (niwa-permission-rules-path-protected-p
             "/repo/.git/config" nil 'write))
    (should (niwa-permission-rules-path-protected-p
             "/home/user/.ssh/key" nil 'read)))
  :doc "ssh directory is protected"
  (let ((niwa-protected-paths '(("~/.ssh/*" . inaccessible))))
    (should (niwa-permission-rules-path-protected-p
             (expand-file-name "~/.ssh/id_rsa"))))
  :doc "remote ssh directory is protected from the target home"
  (let ((niwa-protected-paths '(("~/.ssh/*" . inaccessible)))
        (target (niwa-execution-target-create
                 "/ssh:user@host:/srv/project/")))
    (setf (niwa-execution-target-environment target)
          '(("HOME" . "/home/user")))
    (should (niwa-permission-rules-path-protected-p
             "/ssh:user@host:/home/user/.ssh/id_rsa" target)))
  :doc "default cloud credentials are protected from the target home"
  (let ((target (niwa-execution-target-create
                 "/ssh:user@host:/srv/project/")))
    (setf (niwa-execution-target-environment target)
          '(("HOME" . "/home/user")))
    (dolist (path '(".aws/credentials"
                    ".azure/accessTokens.json"
                    ".config/gcloud/credentials.db"
                    ".kube/config"))
      (should
       (niwa-permission-rules-path-protected-p
        (concat "/ssh:user@host:/home/user/" path) target))))
  :doc "normal path is not protected"
  (let ((niwa-protected-paths
         '(("**/.git/**" . read-only) ("~/.ssh/*" . inaccessible))))
    (should-not (niwa-permission-rules-path-protected-p "/home/user/projects/foo.el")))
  :doc "nil path is not protected"
  (let ((niwa-protected-paths '(("**/.git/**" . read-only))))
    (should-not (niwa-permission-rules-path-protected-p nil))))

(niwa-deftest niwa-permission-protected-path-policy ()
  ,test
  (test)
  :doc "valid protected path modes are returned unchanged"
  (let ((niwa-protected-paths
         '(("**/.git/**" . read-only)
           ("~/.ssh/**" . inaccessible))))
    (should (eq (niwa-permission-protected-path-policy)
                niwa-protected-paths)))
  :doc "the superseded string-only shape is rejected"
  (let ((niwa-protected-paths '("**/.git/**")))
    (should-error (niwa-permission-protected-path-policy) :type 'error))
  :doc "unknown protected path modes are rejected"
  (let ((niwa-protected-paths '(("**/.git/**" . writable))))
    (should-error (niwa-permission-protected-path-policy) :type 'error)))

(niwa-deftest niwa-permission--tool-specifier-key ()
  ,test
  (test)
  :doc "Bash uses :pattern, WebFetch uses :domain"
  (niwa-permission-rules-test--with-fake-tools
    (should (eq :pattern (niwa-permission--tool-specifier-key "Bash")))
    (should (eq :domain  (niwa-permission--tool-specifier-key "WebFetch"))))

  :doc "Read uses :path, Agent uses :name"
  (niwa-permission-rules-test--with-fake-tools
    (should (eq :path (niwa-permission--tool-specifier-key "Read")))
    (should (eq :name (niwa-permission--tool-specifier-key "Agent"))))

  :doc "Unknown tool returns nil"
  (niwa-permission-rules-test--with-fake-tools
    (should (null (niwa-permission--tool-specifier-key "NonExistent"))))

  :doc "Tool with no specifier getter returns nil"
  (niwa-permission-rules-test--with-fake-tools
    (should (null (niwa-permission--tool-specifier-key "Ask")))))

(niwa-deftest niwa-permission-rules-parse ()
  ,test
  (test)
  :doc "bare tool name -> unqualified allow rule"
  (niwa-permission-rules-test--with-fake-tools
    (should (equal '("Read" :action allow)
                   (niwa-permission-rules-parse "Read"))))

  :doc "qualified by exact pattern (Bash)"
  (niwa-permission-rules-test--with-fake-tools
    (should (equal '("Bash" :pattern "git status" :action allow)
                   (niwa-permission-rules-parse
                    "Bash(git status)"))))

  :doc "qualified by glob pattern (Bash)"
  (niwa-permission-rules-test--with-fake-tools
    (should (equal '("Bash" :pattern "git status *" :action allow)
                   (niwa-permission-rules-parse
                    "Bash(git status *)"))))

  :doc "qualified by domain (WebFetch)"
  (niwa-permission-rules-test--with-fake-tools
    (should (equal '("WebFetch" :domain "example.com" :action allow)
                   (niwa-permission-rules-parse
                    "WebFetch(example.com)"))))

  :doc "qualified by path (Edit)"
  (niwa-permission-rules-test--with-fake-tools
    (should (equal '("Edit" :path "src/**" :action allow)
                   (niwa-permission-rules-parse "Edit(src/**)"))))

  :doc "qualified by sub-agent name (Agent)"
  (niwa-permission-rules-test--with-fake-tools
    (should (equal '("Agent" :name "verifier" :action allow)
                   (niwa-permission-rules-parse "Agent(verifier)"))))

  :doc "malformed: no closing paren rejected"
  (niwa-permission-rules-test--with-fake-tools
    (should-error (niwa-permission-rules-parse "Bash(foo")
                  :type 'user-error))

  :doc "malformed: lowercase first char or empty rejected"
  (niwa-permission-rules-test--with-fake-tools
    (should-error (niwa-permission-rules-parse "bash(foo)")
                  :type 'user-error)
    (should-error (niwa-permission-rules-parse "")
                  :type 'user-error))

  :doc "unknown tool name rejected"
  (niwa-permission-rules-test--with-fake-tools
    (should-error (niwa-permission-rules-parse "NonExistent")
                  :type 'user-error))

  :doc "qualifier on a tool without a specifier slot rejected"
  (niwa-permission-rules-test--with-fake-tools
    (should-error (niwa-permission-rules-parse "Ask(foo)")
                  :type 'user-error))

  :doc "non-string input rejected"
  (niwa-permission-rules-test--with-fake-tools
    (should-error (niwa-permission-rules-parse nil)
                  :type 'user-error)
    (should-error (niwa-permission-rules-parse 42)
                  :type 'user-error)))


;;
;;; Bucket-aware permission resolution

(niwa-deftest niwa-permission-rules-collect-buckets ()
  ,test
  (test)
  :doc "buckets returned in innermost-first order"
  ;; Pass 2 consumes buckets in innermost-first order.
  (let ((buckets (niwa-permission-rules-collect-buckets
                  '(:invocation-rule)
                  '(:request-rule)
                  '(:session-rule)
                  '(:persistent-rule))))
    (should (equal '(:invocation :request :session :persistent :defcustom)
                   (mapcar #'car buckets)))))

(niwa-deftest niwa-permission--any-deny ()
  ,test
  (test)
  :doc "deny in any bucket short-circuits pass 1"
  (let ((buckets-with-deny
         (niwa-permission-rules-collect-buckets
          nil nil
          '(("Bash" :pattern "rm *" :action deny))
          nil)))
    (should (niwa-permission--any-deny
             buckets-with-deny "Bash" nil "rm /etc" nil nil)))

  :doc "no deny anywhere returns nil"
  (let ((buckets-no-deny
         (niwa-permission-rules-collect-buckets
          '(("Bash" :pattern "rm *" :action allow))
          nil nil nil)))
    (should-not (niwa-permission--any-deny
                 buckets-no-deny "Bash" nil "rm /etc" nil nil))))

(niwa-deftest niwa-permission-rules-bucket-decision ()
  ,test
  (test)
  :doc "an outer deny remains absolute over an inner allow"
  (let ((niwa-permission-rules nil)
        (buckets
         (niwa-permission-rules-collect-buckets
          '(("Bash" :path "/secret" :action allow))
          nil
          '(("Bash" :path "/secret" :action deny))
          nil)))
    (should (eq 'deny
                (niwa-permission-rules-bucket-decision
                 buckets "Bash" "/secret" nil nil nil))))

  :doc "without a deny the innermost matching action wins"
  (let ((niwa-permission-rules nil)
        (buckets
         (niwa-permission-rules-collect-buckets
          '(("Bash" :path "/secret" :action ask))
          nil
          '(("Bash" :path "/secret" :action allow))
          nil)))
    (should (eq 'ask
                (niwa-permission-rules-bucket-decision
                 buckets "Bash" "/secret" nil nil nil)))))

(niwa-deftest niwa-permission-rules-execution-level-decision ()
  ,test
  (test)
  :doc "literal Eval grants preserve execution-level and direct-authority boundaries"
  (let* ((expression "(message \"*\")")
         (allow (list "Eval" :expression expression
                      :sandbox-permissions 'require-escalated :action 'allow))
         (deny (list "Eval" :expression expression
                     :sandbox-permissions 'require-escalated :action 'deny)))
    (dolist (bucket '(:session :persistent :defcustom))
      (should (eq 'allow (niwa-permission-rules-execution-level-decision
                          (list (cons bucket (list allow)))
                          "Eval" 'require-escalated expression))))
    (should-not (niwa-permission-rules-execution-level-decision
                 (list (cons :skill (list allow))) "Eval" 'require-escalated expression))
    (should (eq 'deny (niwa-permission-rules-execution-level-decision
                       (list (cons :session (list allow)) (cons :skill (list deny)))
                       "Eval" 'require-escalated expression)))
    (should-not (niwa-permission-rules-action (list allow) "Eval" :pattern expression)))
  :doc "direct scoped rule authorizes only its requested execution level"
  (let ((niwa-permission-rules nil)
        (buckets
         (niwa-permission-rules-collect-buckets
          nil nil
          '(("Bash" :pattern "emacs --batch *"
                    :sandbox-permissions require-escalated
                    :action allow))
          nil)))
    (should
     (eq 'allow
         (niwa-permission-rules-execution-level-decision
          buckets "Bash" 'require-escalated "emacs --batch -Q")))
    (should-not
     (niwa-permission-rules-execution-level-decision
      buckets "Bash" 'require-escalated "curl example.test")))

  :doc "deliberately broad direct rule may authorize full escalation"
  (let ((niwa-permission-rules
         '(("Eval" :sandbox-permissions require-escalated :action allow))))
    (should
     (eq 'allow
         (niwa-permission-rules-execution-level-decision
          (niwa-permission-rules-collect-buckets nil nil nil nil)
          "Eval" 'require-escalated "(message \"hello\")"))))

  :doc "delegated allow cannot grant full escalation"
  (let ((niwa-permission-rules nil)
        (buckets
         (niwa-permission-rules-collect-buckets
          '(("Bash" :sandbox-permissions require-escalated :action allow))
          nil nil nil)))
    (should-not
     (niwa-permission-rules-execution-level-decision
      buckets "Bash" 'require-escalated "pwd")))

  :doc "delegated deny remains final over a direct allow"
  (let ((niwa-permission-rules
         '(("Bash" :sandbox-permissions require-escalated :action allow)))
        (buckets
         (niwa-permission-rules-collect-buckets
          '(("Bash" :sandbox-permissions require-escalated :action deny))
          nil nil nil)))
    (should
     (eq 'deny
         (niwa-permission-rules-execution-level-decision
          buckets "Bash" 'require-escalated "pwd")))))

(niwa-deftest niwa-permission-rules-qualified-buckets ()
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
    (niwa-permission-rules-qualified-buckets
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


;;
;;; Resource grants

(niwa-deftest niwa-permission-rules-resource-grant ()
  ,test
  (test)
  :doc "exact grant shape carries only path and access"
  (should (equal '(:path "/etc/passwd" :access read)
                 (niwa-permission-rules-resource-grant
                  "/etc/passwd" 'read)))
  :doc "recursive grant appends the recursive flag"
  (should (equal '(:path "/usr/share/emacs" :access read :recursive t)
                 (niwa-permission-rules-resource-grant
                  "/usr/share/emacs" 'read t)))
  :doc "non-boolean recursive value errors"
  (should-error (niwa-permission-rules-resource-grant
                 "/usr/share/emacs" 'read 'yes)))

(niwa-deftest niwa-permission-rules-resource-granted-p ()
  ,test
  (test)
  :doc "exact grant matches only its exact path"
  (let ((grants '((:path "/etc/passwd" :access read))))
    (should (niwa-permission-rules-resource-granted-p
             "/etc/passwd" 'read grants))
    (should-not (niwa-permission-rules-resource-granted-p
                 "/etc" 'read grants))
    (should-not (niwa-permission-rules-resource-granted-p
                 "/etc/passwd2" 'read grants)))
  :doc "recursive grant matches the directory and descendants"
  (let ((grants '((:path "/usr/share/emacs" :access read :recursive t))))
    (should (niwa-permission-rules-resource-granted-p
             "/usr/share/emacs" 'read grants))
    (should (niwa-permission-rules-resource-granted-p
             "/usr/share/emacs/31.1/lisp/simple.el" 'read grants)))
  :doc "similarly prefixed sibling does not match a recursive grant"
  (should-not (niwa-permission-rules-resource-granted-p
               "/usr/share/emacs-old" 'read
               '((:path "/usr/share/emacs" :access read :recursive t))))
  :doc "write grant implies read in both scopes"
  (should (niwa-permission-rules-resource-granted-p
           "/srv/out" 'read '((:path "/srv/out" :access write))))
  (should (niwa-permission-rules-resource-granted-p
           "/srv/out/log" 'read
           '((:path "/srv/out" :access write :recursive t))))
  :doc "read grant does not imply write"
  (should-not (niwa-permission-rules-resource-granted-p
               "/srv/out/log" 'write
               '((:path "/srv/out" :access read :recursive t))))
  :doc "recursive request is never satisfied by an exact grant"
  (should-not (niwa-permission-rules-resource-granted-p
               "/usr/share/emacs" 'read
               '((:path "/usr/share/emacs" :access read))
               t))
  :doc "recursive request is satisfied by a recursive ancestor grant"
  (should (niwa-permission-rules-resource-granted-p
           "/usr/share/emacs/31.1" 'read
           '((:path "/usr/share" :access read :recursive t))
           t)))

(niwa-deftest niwa-permission-rules-merge-resource-grant ()
  ,test
  (test)
  :doc "write promotes an exact read grant of the same identity"
  (should (equal '((:path "/srv/out" :access write))
                 (niwa-permission-rules-merge-resource-grant
                  '((:path "/srv/out" :access read)) "/srv/out" 'write)))
  :doc "same-identity coverage short-circuits"
  (let ((grants '((:path "/srv/out" :access write :recursive t))))
    (should (eq grants
                (niwa-permission-rules-merge-resource-grant
                 grants "/srv/out" 'read t))))
  :doc "exact grant is recorded even under a covering recursive ancestor"
  (should (equal '((:path "/srv" :access read :recursive t)
                   (:path "/srv/out" :access read))
                 (niwa-permission-rules-merge-resource-grant
                  '((:path "/srv" :access read :recursive t))
                  "/srv/out" 'read)))
  :doc "promotion does not cross between exact and recursive identities"
  (should (equal '((:path "/srv/out" :access read)
                   (:path "/srv/out" :access write :recursive t))
                 (niwa-permission-rules-merge-resource-grant
                  '((:path "/srv/out" :access read))
                  "/srv/out" 'write t))))

(provide 'test-niwa-permission-rules)
;;; test-niwa-permission-rules.el ends here
