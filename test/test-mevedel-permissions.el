;;; test-mevedel-permissions.el --- Tests for permission system -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'mevedel-permissions)
(require 'mevedel-structs)
(require 'mevedel-tool-registry)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name
               load-file-name
               byte-compile-current-file))
          "helpers"))


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
                            rules "Edit" "/home/user/projects/foo.el"))
                   1))
    (should (null (mevedel-permission--find-rules
                   rules "Edit" "/other/path/foo.el"))))
  :doc "non-path rule matches regardless of path"
  (let ((rules '(("Read" :action allow))))
    (should (equal (length (mevedel-permission--find-rules
                            rules "Read" "/any/path"))
                   1))))

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
    (should-not (mevedel-permission--rules-action rules "Write"))))


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

(mevedel-deftest mevedel-permission--mode-decision ()
  ,test
  (test)
  :doc "trust-all allows everything"
  (progn
    (should (eq (mevedel-permission--mode-decision 'trust-all nil) 'allow))
    (should (eq (mevedel-permission--mode-decision 'trust-all t) 'allow)))
  :doc "plan allows read-only, denies write"
  (progn
    (should (eq (mevedel-permission--mode-decision 'plan t) 'allow))
    (should (eq (mevedel-permission--mode-decision 'plan nil) 'deny)))
  :doc "accept-edits allows everything"
  (progn
    (should (eq (mevedel-permission--mode-decision 'accept-edits nil) 'allow))
    (should (eq (mevedel-permission--mode-decision 'accept-edits t) 'allow)))
  :doc "default allows read-only, asks for write"
  (progn
    (should (eq (mevedel-permission--mode-decision 'default t) 'allow))
    (should (eq (mevedel-permission--mode-decision 'default nil) 'ask))))


;;
;;; Full decision chain

(mevedel-deftest mevedel-check-permission ()
  ,test
  (test)
  :doc "deny rule overrides everything"
  (let ((mevedel-permission-rules '(("Edit" :action deny)))
        (mevedel-protected-paths nil))
    (should (eq (mevedel-check-permission "Edit" :mode 'trust-all) 'deny)))
  :doc "protected path forces ask even in trust-all"
  (let ((mevedel-permission-rules nil)
        (mevedel-protected-paths '("**/.git/**")))
    (should (eq (mevedel-check-permission "Edit"
                  :path "/repo/.git/config"
                  :mode 'trust-all)
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
                  :mode 'default)
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
                  :mode 'trust-all)
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
                  :mode 'default)
                'allow)))
  :doc "allow rule allows when no deny or protection"
  (let ((mevedel-permission-rules '(("Read" :action allow)))
        (mevedel-protected-paths nil))
    (should (eq (mevedel-check-permission "Read" :mode 'default) 'allow)))
  :doc "mode decision when no rules match - default asks for non-read-only"
  (let ((mevedel-permission-rules nil)
        (mevedel-protected-paths nil)
        (mock-tool (mevedel-tool--create :name "Edit" :read-only-p nil)))
    (should (eq (mevedel-check-permission "Edit"
                  :tool-struct mock-tool
                  :mode 'default)
                'ask)))
  :doc "mode decision - plan denies non-read-only"
  (let ((mevedel-permission-rules nil)
        (mevedel-protected-paths nil)
        (mock-tool (mevedel-tool--create :name "Edit" :read-only-p nil)))
    (should (eq (mevedel-check-permission "Edit"
                  :tool-struct mock-tool
                  :mode 'plan)
                'deny)))
  :doc "read-only tool allowed in default mode"
  (let ((mevedel-permission-rules nil)
        (mevedel-protected-paths nil)
        (mock-tool (mevedel-tool--create :name "Read" :read-only-p t)))
    (should (eq (mevedel-check-permission "Read"
                  :tool-struct mock-tool
                  :mode 'default)
                'allow)))
  :doc "session rules work alongside defcustom rules"
  (let ((mevedel-permission-rules '(("Edit" :action ask)))
        (mevedel-protected-paths nil)
        (session-rules '(("Edit" :path "/allowed/*" :action allow))))
    (should (eq (mevedel-check-permission "Edit"
                  :path "/allowed/file.el"
                  :session-rules session-rules
                  :mode 'default)
                'allow)))
  :doc "unknown tool (no struct) defaults to ask"
  (let ((mevedel-permission-rules nil)
        (mevedel-protected-paths nil))
    (should (eq (mevedel-check-permission "UnknownTool" :mode 'default) 'ask)))
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
                  :mode 'trust-all)
                'ask))))


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
    (should (= (length (mevedel-session-permission-rules session)) 2))))


;;
;;; Persistent rule storage

(mevedel-deftest mevedel-permission--save-and-load-persistent-rules ()
  ,test
  (test)
  :doc "round-trip save and load"
  (let* ((tmp-dir (make-temp-file "mevedel-test-" t))
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
         (ws (mevedel-workspace--create
              :type 'project :id "test" :root tmp-dir
              :name "test"
              :file-cache (make-hash-table :test #'equal))))
    (unwind-protect
        (should-not (mevedel-permission--load-persistent-rules ws))
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
  :doc "always-allow stores persistent and session rules"
  (let* ((tmp-dir (make-temp-file "mevedel-test-" t))
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

(provide 'test-mevedel-permissions)
;;; test-mevedel-permissions.el ends here
