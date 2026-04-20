;;; test-mevedel-tool-registry.el --- Tests for mevedel-tool-registry.el -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'mevedel-tool-registry)
(require 'gptel-request)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name
               load-file-name
               byte-compile-current-file))
          "helpers"))


;;
;;; Tool struct

(mevedel-deftest mevedel-tool--create
  (:doc "`mevedel-tool--create' creates tool with all slots")
  (let ((tool (mevedel-tool--create
               :name "TestTool"
               :handler #'ignore
               :description "A test tool"
               :category "mevedel"
               :read-only-p t
               :groups '(read test))))
    (should (equal "TestTool" (mevedel-tool-name tool)))
    (should (eq #'ignore (mevedel-tool-handler tool)))
    (should (equal "A test tool" (mevedel-tool-description tool)))
    (should (equal "mevedel" (mevedel-tool-category tool)))
    (should (eq t (mevedel-tool-read-only-p tool)))
    (should (equal '(read test) (mevedel-tool-groups tool)))
    (should (null (mevedel-tool-destructive-p tool)))
    (should (null (mevedel-tool-async-p tool)))
    (should (null (mevedel-tool-check-permission tool)))
    (should (null (mevedel-tool-get-path tool)))))


;;
;;; Registry

(mevedel-deftest mevedel-tool-register
  (:before-each (mevedel-tool-clear-registry)
   :after-each (mevedel-tool-clear-registry))
  ,test
  (test)
  :doc "registers and retrieves by category+name"
  (let ((tool (mevedel-tool--create :name "Read" :category "mevedel")))
    (mevedel-tool-register tool)
    (should (eq tool (mevedel-tool-get "Read" "mevedel"))))

  :doc "retrieves by name only (first match)"
  (let ((tool (mevedel-tool--create :name "Read" :category "mevedel")))
    (mevedel-tool-register tool)
    (should (eq tool (mevedel-tool-get "Read"))))

  :doc "returns nil for unknown tool"
  (should (null (mevedel-tool-get "Nonexistent")))

  :doc "overwrites existing entry"
  (let ((tool1 (mevedel-tool--create :name "Read" :category "mevedel"
                                     :description "v1"))
        (tool2 (mevedel-tool--create :name "Read" :category "mevedel"
                                     :description "v2")))
    (mevedel-tool-register tool1)
    (mevedel-tool-register tool2)
    (should (equal "v2" (mevedel-tool-description
                         (mevedel-tool-get "Read" "mevedel"))))))

(mevedel-deftest mevedel-tool-all
  (:before-each (mevedel-tool-clear-registry)
   :after-each (mevedel-tool-clear-registry))
  ,test
  (test)
  :doc "returns empty list when registry is empty"
  (should (null (mevedel-tool-all)))

  :doc "returns all registered tools"
  (progn
    (mevedel-tool-register (mevedel-tool--create :name "A" :category "mevedel"))
    (mevedel-tool-register (mevedel-tool--create :name "B" :category "mevedel"))
    (should (= 2 (length (mevedel-tool-all))))))


;;
;;; Group resolution

(mevedel-deftest mevedel-tool-for-groups
  (:before-each (mevedel-tool-clear-registry)
   :after-each (mevedel-tool-clear-registry))
  ,test
  (test)
  :doc "finds tools by group"
  (progn
    (mevedel-tool-register (mevedel-tool--create :name "Read" :category "mevedel"
                                                 :groups '(read)))
    (mevedel-tool-register (mevedel-tool--create :name "Grep" :category "mevedel"
                                                 :groups '(read)))
    (mevedel-tool-register (mevedel-tool--create :name "Edit" :category "mevedel"
                                                 :groups '(edit)))
    (let ((read-tools (mevedel-tool-for-groups '(read))))
      (should (= 2 (length read-tools)))
      (should (cl-every (lambda (t) (memq 'read (mevedel-tool-groups t)))
                        read-tools))))

  :doc "returns empty for unknown group"
  (should (null (mevedel-tool-for-groups '(nonexistent))))

  :doc "tool in multiple groups found via any"
  (progn
    (mevedel-tool-register (mevedel-tool--create :name "WebFetch" :category "mevedel"
                                                 :groups '(read web)))
    (should (= 1 (length (mevedel-tool-for-groups '(read)))))
    (should (= 1 (length (mevedel-tool-for-groups '(web)))))))

(mevedel-deftest mevedel-tool-group-exists-p
  (:before-each (mevedel-tool-clear-registry)
   :after-each (mevedel-tool-clear-registry))
  ,test
  (test)
  :doc "returns non-nil for existing group"
  (progn
    (mevedel-tool-register (mevedel-tool--create :name "Read" :category "mevedel"
                                                 :groups '(read)))
    (should (mevedel-tool-group-exists-p 'read)))

  :doc "returns nil for nonexistent group"
  (should (null (mevedel-tool-group-exists-p 'nonexistent))))


;;
;;; Unified tools list resolver

(mevedel-deftest mevedel-tool-resolve
  (:before-each
   (progn
     (mevedel-tool-clear-registry)
     (mevedel-tool-register (mevedel-tool--create :name "Read" :category "mevedel"
                                                  :groups '(read)))
     (mevedel-tool-register (mevedel-tool--create :name "Grep" :category "mevedel"
                                                  :groups '(read)))
     (mevedel-tool-register (mevedel-tool--create :name "Edit" :category "mevedel"
                                                  :groups '(edit)))
     (mevedel-tool-register (mevedel-tool--create :name "Bash" :category "mevedel"
                                                  :groups '(eval))))
   :after-each (mevedel-tool-clear-registry))
  ,test
  (test)
  :doc "bare symbol resolves as group"
  (let ((result (mevedel-tool-resolve '(read))))
    (should (= 2 (length (plist-get result :active))))
    (should (null (plist-get result :deferred))))

  :doc "bare symbol falls back to tool name"
  (let ((result (mevedel-tool-resolve '(Bash))))
    (should (= 1 (length (plist-get result :active))))
    (should (equal "Bash" (mevedel-tool-name (car (plist-get result :active))))))

  :doc "(:group X) explicit group expansion"
  (let ((result (mevedel-tool-resolve '((:group read)))))
    (should (= 2 (length (plist-get result :active)))))

  :doc "(:tool X) explicit tool lookup by name"
  (let ((result (mevedel-tool-resolve '((:tool "Edit")))))
    (should (= 1 (length (plist-get result :active))))
    (should (equal "Edit" (mevedel-tool-name (car (plist-get result :active))))))

  :doc "(:tool (CAT NAME)) explicit tool lookup by path"
  (let ((result (mevedel-tool-resolve '((:tool ("mevedel" "Edit"))))))
    (should (= 1 (length (plist-get result :active))))
    (should (equal "Edit" (mevedel-tool-name (car (plist-get result :active))))))

  :doc "(:deferred X) collects into deferred list"
  (let ((result (mevedel-tool-resolve '(read (:deferred edit)))))
    (should (= 2 (length (plist-get result :active))))
    (should (= 1 (length (plist-get result :deferred))))
    (should (equal "Edit" (mevedel-tool-name (car (plist-get result :deferred))))))

  :doc "mixed list resolves correctly"
  (let ((result (mevedel-tool-resolve '(read (:tool "Bash") (:deferred edit)))))
    (should (= 3 (length (plist-get result :active))))
    (should (= 1 (length (plist-get result :deferred)))))

  :doc "unknown bare symbol signals error"
  (should-error (mevedel-tool-resolve '(nonexistent)) :type 'error)

  :doc "unknown (:tool X) signals error"
  (should-error (mevedel-tool-resolve '((:tool "Nonexistent"))) :type 'error)

  :doc "unknown (:group X) signals error"
  (should-error (mevedel-tool-resolve '((:group nonexistent))) :type 'error))


;;
;;; Args conversion

(mevedel-deftest mevedel-tool--args-to-gptel
  (:doc "`mevedel-tool--args-to-gptel' converts mevedel args to gptel format")
  ,test
  (test)
  :doc "converts required arg"
  (let ((result (mevedel-tool--args-to-gptel
                 '((path string :required "File path")))))
    (should (= 1 (length result)))
    (should (equal "path" (plist-get (car result) :name)))
    (should (equal 'string (plist-get (car result) :type)))
    (should (equal "File path" (plist-get (car result) :description)))
    (should (null (plist-get (car result) :optional))))

  :doc "converts optional arg"
  (let ((result (mevedel-tool--args-to-gptel
                 '((offset integer :optional "Line offset")))))
    (should (eq t (plist-get (car result) :optional))))

  :doc "converts multiple args"
  (let ((result (mevedel-tool--args-to-gptel
                 '((path string :required "File path")
                   (offset integer :optional "Line offset")
                   (limit integer :optional "Max lines")))))
    (should (= 3 (length result))))

  :doc "passes through :items for arrays (required for strict schema)"
  (let ((result (mevedel-tool--args-to-gptel
                 '((ids array :optional "list of ids" :items (:type integer))))))
    (should (equal '(:type integer) (plist-get (car result) :items))))

  :doc "passes through :properties for objects"
  (let ((result (mevedel-tool--args-to-gptel
                 '((config object :required "config"
                           :properties (:host (:type string)))))))
    (should (equal '(:host (:type string))
                   (plist-get (car result) :properties))))

  :doc "round-trips :items through args-from-gptel then args-to-gptel"
  (let* ((gptel-args '((:name "ids" :type array :description "ids"
                              :optional t :items (:type integer))))
         (mevedel-args (mevedel-tool--args-from-gptel gptel-args "t"))
         (roundtrip (mevedel-tool--args-to-gptel mevedel-args)))
    (should (equal '(:type integer) (plist-get (car roundtrip) :items)))))


;;
;;; Validation

(mevedel-deftest mevedel-tool--validate-args
  (:doc "`mevedel-tool--validate-args' validates tool arguments")
  ,test
  (test)
  :doc "passes valid required args"
  (should (null (mevedel-tool--validate-args
                 "Read"
                 '(:path "/tmp/test")
                 '((path string :required "File path")))))

  :doc "fails missing required arg"
  (should (stringp (mevedel-tool--validate-args
                    "Read"
                    '()
                    '((path string :required "File path")))))

  :doc "passes when optional arg is nil"
  (should (null (mevedel-tool--validate-args
                 "Read"
                 '(:path "/tmp/test")
                 '((path string :required "File path")
                   (offset integer :optional "Line offset")))))

  :doc "fails wrong type"
  (should (stringp (mevedel-tool--validate-args
                    "Read"
                    '(:path 42)
                    '((path string :required "File path")))))

  :doc "validates boolean (t and :json-false)"
  (progn
    (should (null (mevedel-tool--validate-args
                   "Test"
                   '(:flag t)
                   '((flag boolean :required "A flag")))))
    (should (null (mevedel-tool--validate-args
                   "Test"
                   '(:flag :json-false)
                   '((flag boolean :required "A flag")))))))


;;
;;; JSON boolean truthiness

(mevedel-deftest mevedel-tool-truthy-p
  ()
  ,test
  (test)

  :doc "JSON false parses to `:json-false', must be treated as nil"
  (should (eq nil (mevedel-tool-truthy-p :json-false)))

  :doc "nil remains nil"
  (should (eq nil (mevedel-tool-truthy-p nil)))

  :doc "t is truthy"
  (should (mevedel-tool-truthy-p t))

  :doc "non-nil string is truthy"
  (should (mevedel-tool-truthy-p "yes"))

  :doc "non-zero number is truthy"
  (should (mevedel-tool-truthy-p 1)))


;;
;;; Prompt resolution

(mevedel-deftest mevedel-tool--resolve-prompt
  ()
  ,test
  (test)
  :doc "returns string unchanged"
  (should (equal "hello" (mevedel-tool--resolve-prompt "hello")))

  :doc "calls function and returns its string result"
  (should (equal "dynamic"
                 (mevedel-tool--resolve-prompt (lambda () "dynamic"))))

  :doc "calls symbol function and returns its string result"
  (let ((fn (lambda () "from-symbol")))
    (should (equal "from-symbol" (mevedel-tool--resolve-prompt fn))))

  :doc "signals error when function returns non-string"
  (should-error (mevedel-tool--resolve-prompt (lambda () 42))
                :type 'error)

  :doc "signals error for non-string non-function"
  (should-error (mevedel-tool--resolve-prompt 42) :type 'error)

  :doc "signals error for nil"
  (should-error (mevedel-tool--resolve-prompt nil) :type 'error))


;;
;;; Registration macro

(mevedel-deftest mevedel-define-tool
  (:before-each (mevedel-tool-clear-registry)
   :after-each (mevedel-tool-clear-registry))
  ,test
  (test)
  :doc "registers tool in registry with correct slots"
  (progn
    (mevedel-define-tool
     :name "TestRead"
     :handler #'ignore
     :description "Read a file"
     :args ((path string :required "File path"))
     :category "mevedel"
     :groups (read test)
     :read-only-p t
     :get-path (lambda (input) (plist-get input :path)))
    (let ((tool (mevedel-tool-get "TestRead" "mevedel")))
      (should tool)
      (should (equal "TestRead" (mevedel-tool-name tool)))
      (should (equal "Read a file" (mevedel-tool-description tool)))
      (should (eq t (mevedel-tool-read-only-p tool)))
      (should (equal '(read test) (mevedel-tool-groups tool)))
      (should (mevedel-tool-gptel-tool tool))))

  :doc "creates gptel-tool with correct name"
  (progn
    (mevedel-define-tool
     :name "TestEdit"
     :handler #'ignore
     :description "Edit a file"
     :category "mevedel")
    (let* ((tool (mevedel-tool-get "TestEdit" "mevedel"))
           (gt (mevedel-tool-gptel-tool tool)))
      (should (equal "TestEdit" (gptel-tool-name gt)))
      (should (equal "mevedel" (gptel-tool-category gt)))))

  :doc "defaults category to mevedel"
  (progn
    (mevedel-define-tool
     :name "TestDefault"
     :handler #'ignore
     :description "Default category")
    (let ((tool (mevedel-tool-get "TestDefault" "mevedel")))
      (should tool)
      (should (equal "mevedel" (mevedel-tool-category tool)))))

  :doc "converts args to gptel format"
  (progn
    (mevedel-define-tool
     :name "TestArgs"
     :handler #'ignore
     :description "Tool with args"
     :args ((name string :required "Name")
            (count integer :optional "Count")))
    (let* ((tool (mevedel-tool-get "TestArgs" "mevedel"))
           (gt (mevedel-tool-gptel-tool tool))
           (args (gptel-tool-args gt)))
      (should (= 2 (length args)))
      (should (equal "name" (plist-get (car args) :name)))))

  :doc "function-valued :prompt resolves to string at register time"
  (progn
    (mevedel-define-tool
     :name "TestFnPrompt"
     :handler #'ignore
     :description "Short description"
     :prompt (lambda () "Dynamic detailed prompt"))
    (let* ((tool (mevedel-tool-get "TestFnPrompt" "mevedel"))
           (gt (mevedel-tool-gptel-tool tool)))
      (should (equal "Dynamic detailed prompt" (mevedel-tool-prompt tool)))
      (should (equal "Dynamic detailed prompt" (gptel-tool-description gt)))))

  :doc "string :prompt stored unchanged"
  (progn
    (mevedel-define-tool
     :name "TestStrPrompt"
     :handler #'ignore
     :description "Short description"
     :prompt "Static detailed prompt")
    (let ((tool (mevedel-tool-get "TestStrPrompt" "mevedel")))
      (should (equal "Static detailed prompt" (mevedel-tool-prompt tool)))))

  :doc "omitted :prompt falls back to :description"
  (progn
    (mevedel-define-tool
     :name "TestNoPrompt"
     :handler #'ignore
     :description "Just a description")
    (let ((tool (mevedel-tool-get "TestNoPrompt" "mevedel")))
      (should (equal "Just a description" (mevedel-tool-prompt tool))))))


;;
;;; Display string

(mevedel-deftest mevedel-tool-display-string ()
  ,test
  (test)
  :doc "registered tool uses first required arg by default"
  (mevedel-tool-clear-registry)
  (unwind-protect
      (progn
        (mevedel-tool-register
         (mevedel-tool--create :name "MyTool" :category "mevedel"
                               :args '((target string :required "Target")
                                       (flag boolean :optional "Flag"))))
        (should (equal "hello"
                       (mevedel-tool-display-string "MyTool"
                                                    '(:target "hello" :flag t)))))
    (mevedel-tool-clear-registry))

  :doc "registered tool uses :display-arg keyword"
  (mevedel-tool-clear-registry)
  (unwind-protect
      (progn
        (mevedel-tool-register
         (mevedel-tool--create :name "MyTool" :category "mevedel"
                               :display-arg :flag
                               :args '((target string :required "Target")
                                       (flag string :optional "Flag"))))
        (should (equal "yes"
                       (mevedel-tool-display-string "MyTool"
                                                    '(:target "hello" :flag "yes")))))
    (mevedel-tool-clear-registry))

  :doc "registered tool uses :display-arg function"
  (mevedel-tool-clear-registry)
  (unwind-protect
      (progn
        (mevedel-tool-register
         (mevedel-tool--create :name "MyTool" :category "mevedel"
                               :display-arg (lambda (args)
                                              (upcase (plist-get args :name)))
                               :args '((name string :required "Name"))))
        (should (equal "HELLO"
                       (mevedel-tool-display-string "MyTool"
                                                    '(:name "hello")))))
    (mevedel-tool-clear-registry))

  :doc "unregistered tool falls back to first plist value"
  (should (equal "world"
                 (mevedel-tool-display-string "Unknown" '(:foo "world"))))

  :doc "path values are abbreviated"
  (should (equal ".../myapp/src/main.el"
                 (mevedel-tool-display-string "Unknown"
                                              '(:path "/home/user/projects/myapp/src/main.el"))))

  :doc "nil when no args"
  (should (null (mevedel-tool-display-string "Unknown" nil))))

(provide 'test-mevedel-tool-registry)
;;; test-mevedel-tool-registry.el ends here
