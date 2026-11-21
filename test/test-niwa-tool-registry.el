;;; test-niwa-tool-registry.el --- Tests for niwa-tool-registry.el -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'bytecomp)

(require 'niwa-structs)
(require 'niwa-tool-registry)
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

(niwa-deftest niwa-tool--create
  (:doc "`niwa-tool--create' creates tool with all slots")
  (let ((tool (niwa-tool--create
               :name "TestTool"
               :handler #'ignore
               :description "A test tool"
               :category "niwa"
               :read-only-p t
               :groups '(read test))))
    (should (equal "TestTool" (niwa-tool-name tool)))
    (should (eq #'ignore (niwa-tool-handler tool)))
    (should (equal "A test tool" (niwa-tool-description tool)))
    (should (equal "niwa" (niwa-tool-category tool)))
    (should (eq t (niwa-tool-read-only-p tool)))
    (should (equal '(read test) (niwa-tool-groups tool)))
    (should (null (niwa-tool-destructive-p tool)))
    (should (null (niwa-tool-async-p tool)))
    (should (null (niwa-tool-check-permission tool)))
    (should (null (niwa-tool-get-path tool)))))


;;
;;; Registry

(niwa-deftest niwa-tool-register
  (:before-each (niwa-tool-clear-registry)
   :after-each (niwa-tool-clear-registry))
  ,test
  (test)
  :doc "registers and retrieves by category+name"
  (let ((tool (niwa-tool--create :name "Read" :category "niwa")))
    (niwa-tool-register tool)
    (should (eq tool (niwa-tool-get "Read" "niwa"))))

  :doc "retrieves by name only (first match)"
  (let ((tool (niwa-tool--create :name "Read" :category "niwa")))
    (niwa-tool-register tool)
    (should (eq tool (niwa-tool-get "Read"))))

  :doc "returns nil for unknown tool"
  (should (null (niwa-tool-get "Nonexistent")))

  :doc "overwrites existing entry"
  (let ((tool1 (niwa-tool--create :name "Read" :category "niwa"
                                     :description "v1"))
        (tool2 (niwa-tool--create :name "Read" :category "niwa"
                                     :description "v2")))
    (niwa-tool-register tool1)
    (niwa-tool-register tool2)
    (should (equal "v2" (niwa-tool-description
                         (niwa-tool-get "Read" "niwa"))))))

(niwa-deftest niwa-tool-ensure
  (:before-each (niwa-tool-clear-registry)
   :after-each (niwa-tool-clear-registry))
  ,test
  (test)
  :doc "returns an existing tool without loading its built-in owner"
  (let ((tool (niwa-tool--create :name "Bash" :category "custom"))
        (require-called nil))
    (niwa-tool-register tool)
    (cl-letf (((symbol-function 'require)
               (lambda (&rest _args)
                 (setq require-called t))))
      (should (eq tool (niwa-tool-ensure "Bash")))
      (should-not require-called)))

  :doc "loads only Bash's owner and returns the requested tool"
  (let ((tool (niwa-tool-ensure "Bash")))
    (should (equal "Bash" (niwa-tool-name tool)))
    (should (equal '("Bash" "Eval" "ListExecutions" "Monitor"
                     "StopExecution" "WriteStdin")
                   (sort (mapcar #'niwa-tool-name
                                 (niwa-tool-all))
                         #'string<))))

  :doc "Skill and ListSkills each lazily register only their shared owner"
  (dolist (name '("Skill" "ListSkills"))
    (niwa-tool-clear-registry)
    (should (equal name (niwa-tool-name (niwa-tool-ensure name))))
    (should (equal '("ListSkills" "Skill")
                   (sort (mapcar #'niwa-tool-name
                                 (niwa-tool-all))
                         #'string<))))

  :doc "Ask lazily registers only its direct owner"
  (progn
    (niwa-tool-clear-registry)
    (should (equal "Ask" (niwa-tool-name (niwa-tool-ensure "Ask"))))
    (should (equal '("Ask")
                   (mapcar #'niwa-tool-name (niwa-tool-all)))))

  :doc "returns nil without registering anything for an unknown name"
  (progn
    (should-not (niwa-tool-ensure "DoesNotExist"))
    (should-not (niwa-tool-all))))

(niwa-deftest niwa-tool-all
  (:before-each (niwa-tool-clear-registry)
   :after-each (niwa-tool-clear-registry))
  ,test
  (test)
  :doc "returns empty list when registry is empty"
  (should (null (niwa-tool-all)))

  :doc "returns all registered tools"
  (progn
    (niwa-tool-register (niwa-tool--create :name "A" :category "niwa"))
    (niwa-tool-register (niwa-tool--create :name "B" :category "niwa"))
    (should (= 2 (length (niwa-tool-all))))))


;;
;;; Group resolution

(niwa-deftest niwa-tool-for-groups
  (:before-each (niwa-tool-clear-registry)
   :after-each (niwa-tool-clear-registry))
  ,test
  (test)
  :doc "finds tools by group"
  (progn
    (niwa-tool-register (niwa-tool--create :name "Read" :category "niwa"
                                                 :groups '(read)))
    (niwa-tool-register (niwa-tool--create :name "Grep" :category "niwa"
                                                 :groups '(read)))
    (niwa-tool-register (niwa-tool--create :name "Edit" :category "niwa"
                                                 :groups '(edit)))
    (let ((read-tools (niwa-tool-for-groups '(read))))
      (should (= 2 (length read-tools)))
      (should (cl-every (lambda (tool) (memq 'read (niwa-tool-groups tool)))
                        read-tools))))

  :doc "returns empty for unknown group"
  (should (null (niwa-tool-for-groups '(nonexistent))))

  :doc "tool in multiple groups found via any"
  (progn
    (niwa-tool-register (niwa-tool--create :name "WebFetch" :category "niwa"
                                                 :groups '(read web)))
    (should (= 1 (length (niwa-tool-for-groups '(read)))))
    (should (= 1 (length (niwa-tool-for-groups '(web)))))))

(niwa-deftest niwa-tool-group-exists-p
  (:before-each (niwa-tool-clear-registry)
   :after-each (niwa-tool-clear-registry))
  ,test
  (test)
  :doc "returns non-nil for existing group"
  (progn
    (niwa-tool-register (niwa-tool--create :name "Read" :category "niwa"
                                                 :groups '(read)))
    (should (niwa-tool-group-exists-p 'read)))

  :doc "returns nil for nonexistent group"
  (should (null (niwa-tool-group-exists-p 'nonexistent))))


;;
;;; Unified tools list resolver

(niwa-deftest niwa-tool-resolve
  (:before-each
   (progn
     (niwa-tool-clear-registry)
     (niwa-tool-register (niwa-tool--create :name "Read" :category "niwa"
                                                  :groups '(read)))
     (niwa-tool-register (niwa-tool--create :name "Grep" :category "niwa"
                                                  :groups '(read)))
     (niwa-tool-register (niwa-tool--create :name "Edit" :category "niwa"
                                                  :groups '(edit)))
     (niwa-tool-register (niwa-tool--create :name "Bash" :category "niwa"
                                                  :groups '(eval))))
   :after-each (niwa-tool-clear-registry))
  ,test
  (test)
  :doc "bare symbol resolves as group"
  (let ((result (niwa-tool-resolve '(read))))
    (should (= 2 (length (plist-get result :active))))
    (should (null (plist-get result :discoverable))))

  :doc "bare symbol falls back to tool name"
  (let ((result (niwa-tool-resolve '(Bash))))
    (should (= 1 (length (plist-get result :active))))
    (should (equal "Bash" (niwa-tool-name (car (plist-get result :active))))))

  :doc "(:group X) explicit group expansion"
  (let ((result (niwa-tool-resolve '((:group read)))))
    (should (= 2 (length (plist-get result :active)))))

  :doc "(:tool X) explicit tool lookup by name"
  (let ((result (niwa-tool-resolve '((:tool "Edit")))))
    (should (= 1 (length (plist-get result :active))))
    (should (equal "Edit" (niwa-tool-name (car (plist-get result :active))))))

  :doc "(:tool (CAT NAME)) explicit tool lookup by path"
  (let ((result (niwa-tool-resolve '((:tool ("niwa" "Edit"))))))
    (should (= 1 (length (plist-get result :active))))
    (should (equal "Edit" (niwa-tool-name (car (plist-get result :active))))))

  :doc "(:discoverable X) collects into deferred list"
  (let ((result (niwa-tool-resolve '(read (:discoverable edit)))))
    (should (= 2 (length (plist-get result :active))))
    (should (= 1 (length (plist-get result :discoverable))))
    (should (equal "Edit" (niwa-tool-name (car (plist-get result :discoverable))))))

  :doc "mixed list resolves correctly"
  (let ((result (niwa-tool-resolve '(read (:tool "Bash") (:discoverable edit)))))
    (should (= 3 (length (plist-get result :active))))
    (should (= 1 (length (plist-get result :discoverable)))))

  :doc "rejects uncallable discoverable names after native precedence"
  (progn
   (dolist (name '("ToolCall" "ToolSearch" "list" "bad name"))
    (niwa-tool-register (niwa-tool--create :name name :category "niwa"))
    (should-error (niwa-tool-resolve `((:discoverable (:tool ,name))))))
  (let ((resolved (niwa-tool-resolve '((:tool "ToolCall")
                                         (:discoverable (:tool "ToolCall"))))))
    (should (= 1 (length (plist-get resolved :active))))
    (should-not (plist-get resolved :discoverable))))

  :doc "rejects ambiguous category separators instead of dispatching another identity"
  (progn
    (niwa-tool-register (niwa-tool--create :name "Probe" :category "server/team"))
    (niwa-tool-register (niwa-tool--create :name "team/Probe" :category "server"))
    (should-error
     (niwa-tool-resolve '((:discoverable (:tool ("server/team" "Probe")))))))

  :doc "unknown bare symbol signals error"
  (should-error (niwa-tool-resolve '(nonexistent)) :type 'error)

  :doc "unknown (:tool X) signals error"
  (should-error (niwa-tool-resolve '((:tool "Nonexistent"))) :type 'error)

  :doc "unknown (:group X) signals error"
  (should-error (niwa-tool-resolve '((:group nonexistent))) :type 'error))


;;
;;; Args conversion

(niwa-deftest niwa-tool--args-to-gptel
  (:doc "`niwa-tool--args-to-gptel' converts niwa args to gptel format")
  ,test
  (test)
  :doc "converts required arg"
  (let ((result (niwa-tool--args-to-gptel
                 '((path string :required "File path")))))
    (should (= 1 (length result)))
    (should (equal "path" (plist-get (car result) :name)))
    (should (equal 'string (plist-get (car result) :type)))
    (should (equal "File path" (plist-get (car result) :description)))
    (should (null (plist-get (car result) :optional))))

  :doc "converts optional arg"
  (let ((result (niwa-tool--args-to-gptel
                 '((offset integer :optional "Line offset")))))
    (should (eq t (plist-get (car result) :optional))))

  :doc "converts multiple args"
  (let ((result (niwa-tool--args-to-gptel
                 '((path string :required "File path")
                   (offset integer :optional "Line offset")
                   (limit integer :optional "Max lines")))))
    (should (= 3 (length result))))

  :doc "lowers semantic paths to provider strings with one raw-path hint"
  (let* ((description "File path")
         (args (list (list 'file_path 'path :required description)))
         (result (niwa-tool--args-to-gptel args))
         (provider-arg (car result))
         (provider-description (plist-get provider-arg :description)))
    (should (eq 'string (plist-get provider-arg :type)))
    (should (equal description (nth 3 (car args))))
    (should
     (equal
      "File path Pass a raw filesystem path, not Markdown or a web URL."
      provider-description))
    (should-not
     (string-match-p
      (concat (regexp-quote niwa-tool--path-description-suffix) ".*"
              (regexp-quote niwa-tool--path-description-suffix))
      provider-description)))

  :doc "keeps ordinary path guidance filesystem-only"
  (let* ((result
          (niwa-tool--args-to-gptel
           '((path path :required "File path"))))
         (description (plist-get (car result) :description)))
    (should (equal
             "File path Pass a raw filesystem path, not Markdown or a web URL."
             description))
    (should-not (string-search "resource address" description)))

  :doc "adds resource guidance to path-or-resource arguments"
  (let* ((result
          (niwa-tool--args-to-gptel
           '((path path-or-resource :required "File path"))))
         (provider-arg (car result))
         (description (plist-get (car result) :description)))
    (should (eq 'string (plist-get provider-arg :type)))
    (should (string-suffix-p
             niwa-tool--path-or-resource-description-suffix
             description))
    (should (string-search "canonical resource address" description)))

  :doc "does not add path semantics to wrapped string arguments"
  (let* ((wrapped
          '((:name "file_path" :type string :description "File path")))
         (niwa-args (niwa-tool--args-from-gptel wrapped "wrapped"))
         (roundtrip (niwa-tool--args-to-gptel niwa-args)))
    (should (eq 'string (cadar niwa-args)))
    (should (equal "File path" (plist-get (car roundtrip) :description))))

  :doc "appends the exact path hint when authored text differs only by case"
  (let* ((description
          "pass a raw filesystem path, not markdown or a url.")
         (result
          (niwa-tool--args-to-gptel
           (list (list 'file_path 'path :required description))))
         (provider-description (plist-get (car result) :description)))
    (should
     (string-suffix-p niwa-tool--path-description-suffix
                      provider-description)))

  :doc "keeps the niwa-only :access key out of the provider schema"
  (let ((schema (car (niwa-tool--args-to-gptel
                      '((source path-or-resource :optional "S" :access read :enum ["a"]))))))
    (should-not (plist-member schema :access))
    (should (equal ["a"] (plist-get schema :enum)))
    (should (plist-get schema :optional)))

  :doc "passes through :items for arrays (required for strict schema)"
  (let ((result (niwa-tool--args-to-gptel
                 '((ids array :optional "list of ids" :items (:type integer))))))
    (should (equal '(:type integer) (plist-get (car result) :items))))

  :doc "does not let gptel mutate nested niwa schemas"
  (let* ((args '((tasks array :required "Tasks" :items (:type object))))
         (provider-args (niwa-tool--args-to-gptel args)))
    (gptel-make-tool :name "SchemaIsolation" :function #'ignore
                     :args provider-args)
    (should (eq 'object
                (plist-get (plist-get (car args) :items) :type))))

  :doc "does not share vector-valued extras with gptel"
  (let* ((args '((mode string :required "Mode" :enum ["fast" "safe"])))
         (provider-args (niwa-tool--args-to-gptel args)))
    (should-not (eq (plist-get (car args) :enum)
                    (plist-get (car provider-args) :enum))))

  :doc "passes through numeric :minimum/:maximum bounds"
  (let ((result (niwa-tool--args-to-gptel
                 '((timeout_ms integer :optional "Timeout"
                               :minimum 10000 :maximum 3600000)))))
    (should (equal 10000 (plist-get (car result) :minimum)))
    (should (equal 3600000 (plist-get (car result) :maximum))))

  :doc "passes through :properties for objects"
  (let ((result (niwa-tool--args-to-gptel
                 '((config object :required "config"
                           :properties (:host (:type string)))))))
    (should (equal '(:host (:type string))
                   (plist-get (car result) :properties))))

  :doc "round-trips :items through args-from-gptel then args-to-gptel"
  (let* ((gptel-args '((:name "ids" :type array :description "ids"
                              :optional t :items (:type integer))))
         (niwa-args (niwa-tool--args-from-gptel gptel-args "t"))
         (roundtrip (niwa-tool--args-to-gptel niwa-args)))
    (should (equal '(:type integer) (plist-get (car roundtrip) :items))))

  :doc "normalizes nested string types imported from wrapped tools"
  (let* ((gptel-args
          '((:name "tasks" :type "array" :description "tasks"
                   :items (:type "object"
                           :properties (:subject (:type "string"))))))
         (niwa-args (niwa-tool--args-from-gptel gptel-args "wrapped"))
         (items (plist-get (car niwa-args) :items)))
    (should (eq 'object (plist-get items :type)))
    (should (eq 'string
                (plist-get (plist-get (plist-get items :properties) :subject)
                           :type))))

  :doc "does not confuse an object property named type with schema type"
  (let* ((gptel-args
          '((:name "config" :type "object" :description "config"
                   :properties (:type (:type "string")))))
         (niwa-args (niwa-tool--args-from-gptel gptel-args "wrapped"))
         (properties (plist-get (car niwa-args) :properties)))
    (should (eq 'string (plist-get (plist-get properties :type) :type)))))

(niwa-deftest niwa-tool--positional-to-plist ()
  ,test
  (test)
  :doc "converts positional args to plist"
  (let ((specs '((name string :required "Name")
                 (count integer :optional "Count")))
        (values '("hello" 42)))
    (should (equal (niwa-tool--positional-to-plist values specs)
                   '(:name "hello" :count 42))))
  :doc "handles empty args"
  (should (null (niwa-tool--positional-to-plist nil nil)))
  :doc "handles fewer values than specs"
  (let ((specs '((a string :required "A")
                 (b string :required "B")))
        (values '("only-one")))
    (should (equal (niwa-tool--positional-to-plist values specs)
                   '(:a "only-one")))))

(niwa-deftest niwa-tool-args-key ()
  ,test
  (test)
  :doc "the model's plist and the pipeline's spec-order plist share a key"
  (let ((specs '((file_path string) (offset integer) (limit integer))))
    (should (equal (niwa-tool-args-key '(:limit 5 :file_path "a"))
                   (niwa-tool-args-key
                    (niwa-tool--positional-to-plist '("a" nil 5) specs))))
    (should (equal '((:file_path . "a") (:limit . 5))
                   (niwa-tool-args-key '(:limit 5 :file_path "a")))))
  :doc "different and non-nil false values distinguish calls"
  (should-not (equal (niwa-tool-args-key '(:file_path "a"))
                     (niwa-tool-args-key '(:file_path "b"))))
  (should-not (equal (niwa-tool-args-key '(:pattern "x" :multiline :json-false))
                     (niwa-tool-args-key '(:pattern "x"))))
  :doc "nested values compare structurally, beyond any printer limit"
  (should (equal (niwa-tool-args-key `(:edits [(:old "a" :new "b")]))
                 (niwa-tool-args-key `(:edits [(:old "a" :new "b")]))))
  (should-not (equal (niwa-tool-args-key `(:edits ,(append (make-list 40 1) '(a))))
                     (niwa-tool-args-key `(:edits ,(append (make-list 40 1) '(b))))))
  :doc "shares values without copying and leaves ARGS unchanged"
  (let* ((body (make-string 100000 ?x))
         (args (list :content body :file_path "a.el"))
         (before (copy-tree args))
         (key (niwa-tool-args-key args)))
    (should (eq body (cdr (assq :content key))))
    (should (equal before args))
    (should (eq body (plist-get args :content))))
  :doc "non-plist arguments are returned unchanged"
  (should (null (niwa-tool-args-key nil)))
  (should (equal "raw" (niwa-tool-args-key "raw")))
  (should (equal '(:odd) (niwa-tool-args-key '(:odd)))))


;;
;;; JSON boolean truthiness

(niwa-deftest niwa-tool-truthy-p
  ()
  ,test
  (test)

  :doc "JSON false parses to `:json-false', must be treated as nil"
  (should (eq nil (niwa-tool-truthy-p :json-false)))

  :doc "nil remains nil"
  (should (eq nil (niwa-tool-truthy-p nil)))

  :doc "t is truthy"
  (should (niwa-tool-truthy-p t))

  :doc "non-nil string is truthy"
  (should (niwa-tool-truthy-p "yes"))

  :doc "non-zero number is truthy"
  (should (niwa-tool-truthy-p 1)))

(niwa-deftest niwa-tool-string-arg
  ()
  ,test
  (test)

  :doc "returns the string when key is a non-empty string"
  (should (equal "foo" (niwa-tool-string-arg '(:key "foo") :key)))

  :doc "returns default when key is missing"
  (should (equal "def" (niwa-tool-string-arg '() :key "def")))

  :doc "returns nil default when key is missing and none supplied"
  (should (null (niwa-tool-string-arg '() :key)))

  :doc "empty string returns default, not the empty string"
  (should (equal "def" (niwa-tool-string-arg '(:key "") :key "def")))

  :doc ":json-false returns default"
  (should (equal "def" (niwa-tool-string-arg '(:key :json-false) :key "def")))

  :doc "non-string value returns default"
  (should (equal "def" (niwa-tool-string-arg '(:key 42) :key "def"))))

(niwa-deftest niwa-tool-integer-arg
  ()
  ,test
  (test)

  :doc "returns the integer when key is an integer"
  (should (= 5 (niwa-tool-integer-arg '(:key 5) :key)))

  :doc "zero is a valid integer"
  (should (= 0 (niwa-tool-integer-arg '(:key 0) :key)))

  :doc "returns default when key is missing"
  (should (= 3 (niwa-tool-integer-arg '() :key 3)))

  :doc "returns nil default when key is missing and none supplied"
  (should (null (niwa-tool-integer-arg '() :key)))

  :doc ":json-false returns default"
  (should (null (niwa-tool-integer-arg '(:key :json-false) :key)))

  :doc "non-integer string returns default"
  (should (null (niwa-tool-integer-arg '(:key "5") :key)))

  :doc "float returns default"
  (should (null (niwa-tool-integer-arg '(:key 3.14) :key))))


;;
;;; Prompt resolution

(niwa-deftest niwa-tool--resolve-prompt
  ()
  ,test
  (test)
  :doc "returns string unchanged"
  (should (equal "hello" (niwa-tool--resolve-prompt "hello")))

  :doc "calls function and returns its string result"
  (should (equal "dynamic"
                 (niwa-tool--resolve-prompt (lambda () "dynamic"))))

  :doc "calls symbol function and returns its string result"
  (let ((fn (lambda () "from-symbol")))
    (should (equal "from-symbol" (niwa-tool--resolve-prompt fn))))

  :doc "signals error when function returns non-string"
  (should-error (niwa-tool--resolve-prompt (lambda () 42))
                :type 'error)

  :doc "signals error for non-string non-function"
  (should-error (niwa-tool--resolve-prompt 42) :type 'error)

  :doc "signals error for nil"
  (should-error (niwa-tool--resolve-prompt nil) :type 'error))


;;
;;; Registration macro

(niwa-deftest niwa-define-tool
  (:before-each (niwa-tool-clear-registry)
   :after-each (niwa-tool-clear-registry))
  ,test
  (test)
  :doc "registers tool in registry with correct slots"
  (progn
    (niwa-define-tool
     :name "TestRead"
     :handler #'ignore
     :description "Read a file"
     :args ((path string :required "File path"))
     :category "niwa"
     :groups (read test)
     :read-only-p t
     :get-path (lambda (input) (plist-get input :path)))
    (let ((tool (niwa-tool-get "TestRead" "niwa")))
      (should tool)
      (should (equal "TestRead" (niwa-tool-name tool)))
      (should (equal "Read a file" (niwa-tool-description tool)))
      (should (eq t (niwa-tool-read-only-p tool)))
      (should (equal '(read test) (niwa-tool-groups tool)))
      (should (niwa-tool-gptel-tool tool))))

  :doc "creates gptel-tool with correct name"
  (progn
    (niwa-define-tool
     :name "TestEdit"
     :handler #'ignore
     :description "Edit a file"
     :category "niwa")
    (let* ((tool (niwa-tool-get "TestEdit" "niwa"))
           (gt (niwa-tool-gptel-tool tool)))
      (should (equal "TestEdit" (gptel-tool-name gt)))
      (should (equal "niwa" (gptel-tool-category gt)))))

  :doc "rejects unknown properties in native and wrapped forms"
  (dolist (form '((niwa-define-tool
                    :name "Typo" :description "Typo" :get-pths ignore)
                  (niwa-define-tool
                    :wrap source :get-pths ignore)))
    (should-error (macroexpand form) :type 'error))

  :doc "defaults category to niwa"
  (progn
    (niwa-define-tool
     :name "TestDefault"
     :handler #'ignore
     :description "Default category")
    (let ((tool (niwa-tool-get "TestDefault" "niwa")))
      (should tool)
      (should (equal "niwa" (niwa-tool-category tool)))))

  :doc "converts args to gptel format"
  (progn
    (niwa-define-tool
     :name "TestArgs"
     :handler #'ignore
     :description "Tool with args"
     :args ((name string :required "Name")
            (count integer :optional "Count")))
    (let* ((tool (niwa-tool-get "TestArgs" "niwa"))
           (gt (niwa-tool-gptel-tool tool))
           (args (gptel-tool-args gt)))
      (should (= 2 (length args)))
      (should (equal "name" (plist-get (car args) :name)))))

  :doc "each evaluation of one declaration owns its provider schema"
  ;; Per-request updates such as the Agent tool's role and model enums edit
  ;; the registered provider schema in place.  Re-registering the same
  ;; expanded declaration must not inherit them.
  (let ((gptel--known-tools nil)
        (declaration
         (macroexpand-all
          '(niwa-define-tool
            :name "TestSchemaOwnership"
            :handler #'ignore
            :description "Tool with nested schema"
            :args ((mode string :optional "Mode" :enum [])
                   (tasks array :required "Tasks"
                          :items (:type object :required ["id"])))))))
    (cl-flet ((provider-args ()
                (gptel-tool-args
                 (niwa-tool-gptel-tool
                  (niwa-tool-get "TestSchemaOwnership" "niwa")))))
      (eval declaration t)
      (let* ((first (provider-args))
             (mode (car first))
             (items (plist-get (cadr first) :items)))
        (setf (plist-get mode :enum) ["fast" "safe"]
              (plist-get mode :description) "Changed")
        (aset (plist-get items :required) 0 "changed")
        (setf (plist-get items :type) "changed")
        (eval declaration t)
        (let ((second (provider-args)))
          (should-not (eq first second))
          (should (equal [] (plist-get (car second) :enum)))
          (should (equal "Mode" (plist-get (car second) :description)))
          (should (equal '(:type "object" :required ["id"])
                         (plist-get (cadr second) :items)))
          ;; The authored niwa schema stays as declared too.
          (should (equal '(mode string :optional "Mode" :enum [])
                         (assq 'mode (niwa-tool-args
                                      (niwa-tool-get "TestSchemaOwnership"
                                                        "niwa")))))))))

  :doc "function-valued :prompt resolves to string at register time"
  (progn
    (niwa-define-tool
     :name "TestFnPrompt"
     :handler #'ignore
     :description "Short description"
     :prompt (lambda () "Dynamic detailed prompt"))
    (let* ((tool (niwa-tool-get "TestFnPrompt" "niwa"))
           (gt (niwa-tool-gptel-tool tool)))
      (should (equal "Dynamic detailed prompt" (niwa-tool-prompt tool)))
      (should (equal "Dynamic detailed prompt" (gptel-tool-description gt)))))

  :doc "string :prompt stored unchanged"
  (progn
    (niwa-define-tool
     :name "TestStrPrompt"
     :handler #'ignore
     :description "Short description"
     :prompt "Static detailed prompt")
    (let ((tool (niwa-tool-get "TestStrPrompt" "niwa")))
      (should (equal "Static detailed prompt" (niwa-tool-prompt tool)))))

  :doc "omitted :prompt falls back to :description"
  (progn
    (niwa-define-tool
     :name "TestNoPrompt"
     :handler #'ignore
     :description "Just a description")
    (let ((tool (niwa-tool-get "TestNoPrompt" "niwa")))
      (should (equal "Just a description" (niwa-tool-prompt tool)))))

  :doc ":renderer is stored on the niwa-tool struct"
  (progn
    (niwa-define-tool
     :name "TestRenderer"
     :handler #'ignore
     :description "Tool with a renderer"
     :renderer (lambda (_name _args _result _data)
                 (list :header "hi")))
    (let ((tool (niwa-tool-get "TestRenderer" "niwa")))
      (should (functionp (niwa-tool-renderer tool)))
      (should (equal '(:header "hi")
                     (funcall (niwa-tool-renderer tool) nil nil nil nil)))))

  :doc ":render-transform is stored on the niwa-tool struct"
  (progn
    (niwa-define-tool
     :name "TestRenderTransform"
     :handler #'ignore
     :description "Tool with a render transform"
     :render-transform (lambda (_name _args result)
                         (list :chars (length result))))
    (let ((tool (niwa-tool-get "TestRenderTransform" "niwa")))
      (should (functionp (niwa-tool-render-transform tool)))
      (should (equal '(:chars 3)
                     (funcall (niwa-tool-render-transform tool)
                              "TestRenderTransform" nil "abc")))))

  :doc ":render-transform is stored on wrapped tools"
  (let ((source (gptel-make-tool
                 :name "WrappedRenderTransform"
                 :function #'ignore
                 :description "Wrapped source"
                 :args nil
                 :category "test-render-transform")))
    (niwa-define-tool
     :wrap source
     :render-transform (lambda (_name _args result)
                         (list :chars (length result))))
    (let ((tool (niwa-tool-get "WrappedRenderTransform"
                                  "niwa-test-render-transform")))
      (should (functionp (niwa-tool-render-transform tool)))
      (should (equal '(:chars 4)
                     (funcall (niwa-tool-render-transform tool)
                              "WrappedRenderTransform" nil "abcd")))))

  :doc "semantic repair extension is absent"
  (should-not (fboundp 'niwa-tool-repair-input))

  :doc "wrapped tool dispatch suppresses interactive file-visit side effects"
  (let (captured result)
    (gptel-make-tool
     :name "WrappedQuietFileVisit"
     :function (lambda ()
                 (setq captured
                       (list enable-local-variables
                             find-file-hook
                             hack-local-variables-hook))
                 "ok")
     :description "Wrapped source"
     :args nil
     :category "test-quiet-file-visit")
    (niwa-define-tool
     :wrap (gptel-get-tool '("test-quiet-file-visit" "WrappedQuietFileVisit")))
    (let* ((tool (niwa-tool-get "WrappedQuietFileVisit"
                                   "niwa-test-quiet-file-visit"))
           (handler (niwa-tool-handler tool))
           (enable-local-variables t)
           (find-file-hook '(sentinel-find-file-hook))
           (hack-local-variables-hook '(sentinel-local-variables-hook)))
      (funcall handler (lambda (value) (setq result value)) nil)
      (should (equal '(:result "ok") result))
      (should (equal '(:safe nil nil) captured))))

  :doc "wrapped tool rejects replacement schema drift before dispatch"
  (let ((gptel--known-tools nil)
        called result)
    (niwa-define-tool
     :wrap
     (gptel-make-tool
      :name "WrappedSchemaDrift"
      :function #'ignore
      :description "Wrapped source"
      :args '((:name "left" :type string :description "Left")
              (:name "right" :type string :description "Right"))
      :category "test-schema-drift"))
    (gptel-make-tool
     :name "WrappedSchemaDrift"
     :function (lambda (&rest _args) (setq called t))
     :description "Wrapped replacement"
     :args '((:name "right" :type string :description "Right")
             (:name "left" :type string :description "Left"))
     :category "test-schema-drift")
    (funcall
     (niwa-tool-handler
      (niwa-tool-get "WrappedSchemaDrift"
                        "niwa-test-schema-drift"))
     (lambda (value) (setq result value))
     '(:left "L" :right "R"))
    (should-not called)
    (should (string-match-p "contract changed; re-wrap"
                            (plist-get result :result))))

  :doc "wrapped tool rejects replacement async drift before dispatch"
  (let ((gptel--known-tools nil)
        called result)
    (niwa-define-tool
     :wrap
     (gptel-make-tool
      :name "WrappedAsyncDrift"
      :function #'ignore
      :description "Wrapped source"
      :args nil
      :category "test-async-drift"))
    (gptel-make-tool
     :name "WrappedAsyncDrift"
     :function (lambda (_callback) (setq called t))
     :description "Wrapped replacement"
     :args nil
     :async t
     :category "test-async-drift")
    (funcall
     (niwa-tool-handler
      (niwa-tool-get "WrappedAsyncDrift"
                        "niwa-test-async-drift"))
     (lambda (value) (setq result value))
     nil)
    (should-not called)
    (should (string-match-p "contract changed; re-wrap"
                            (plist-get result :result))))

  :doc ":summary keyword reaches the niwa-tool struct"
  (progn
    (niwa-define-tool
     :name "TestSummary"
     :handler #'ignore
     :description "Long winded description that should not bloat the deferred-tools roster reminder."
     :summary "Concise one-liner.")
    (let ((tool (niwa-tool-get "TestSummary" "niwa")))
      (should (equal "Concise one-liner." (niwa-tool-summary tool)))))

  :doc "omitted :summary defaults to the description's first sentence"
  (progn
    (niwa-define-tool
     :name "TestNoSummary"
     :handler #'ignore
     :description "Tool without a summary. Further detail stays out.\nSecond line.")
    (let ((tool (niwa-tool-get "TestNoSummary" "niwa")))
      (should (equal "Tool without a summary." (niwa-tool-summary tool)))))

  :doc "a wrapped tool without :summary uses its source description"
  (progn
    (niwa-define-tool
     :wrap (gptel-make-tool
            :name "WrappedNoSummary"
            :function #'ignore
            :description "Read the wrapped thing. More words."
            :args nil
            :category "test-summary"))
    (should (equal "Read the wrapped thing."
                   (niwa-tool-summary
                    (niwa-tool-get "WrappedNoSummary"
                                      "niwa-test-summary")))))

  :doc "omitted :renderer leaves the slot nil"
  (progn
    (niwa-define-tool
     :name "TestNoRenderer"
     :handler #'ignore
     :description "Tool without a renderer")
    (let ((tool (niwa-tool-get "TestNoRenderer" "niwa")))
      (should (null (niwa-tool-renderer tool))))))


;;
;;; Display string

(niwa-deftest niwa-tool-display-string ()
  ,test
  (test)
  :doc "registered tool uses first required arg by default"
  (niwa-tool-clear-registry)
  (unwind-protect
      (progn
        (niwa-tool-register
         (niwa-tool--create :name "MyTool" :category "niwa"
                               :args '((target string :required "Target")
                                       (flag boolean :optional "Flag"))))
        (should (equal "hello"
                       (niwa-tool-display-string "MyTool"
                                                    '(:target "hello" :flag t)))))
    (niwa-tool-clear-registry))

  :doc "registered tool uses :display-arg keyword"
  (niwa-tool-clear-registry)
  (unwind-protect
      (progn
        (niwa-tool-register
         (niwa-tool--create :name "MyTool" :category "niwa"
                               :display-arg :flag
                               :args '((target string :required "Target")
                                       (flag string :optional "Flag"))))
        (should (equal "yes"
                       (niwa-tool-display-string "MyTool"
                                                    '(:target "hello" :flag "yes")))))
    (niwa-tool-clear-registry))

  :doc "registered tool uses :display-arg function"
  (niwa-tool-clear-registry)
  (unwind-protect
      (progn
        (niwa-tool-register
         (niwa-tool--create :name "MyTool" :category "niwa"
                               :display-arg (lambda (args)
                                              (upcase (plist-get args :name)))
                               :args '((name string :required "Name"))))
        (should (equal "HELLO"
                       (niwa-tool-display-string "MyTool"
                                                    '(:name "hello")))))
    (niwa-tool-clear-registry))

  :doc "unregistered tool falls back to first plist value"
  (should (equal "world"
                 (niwa-tool-display-string "Unknown" '(:foo "world"))))

  :doc "path values are abbreviated"
  (should (equal ".../myapp/src/main.el"
                 (niwa-tool-display-string "Unknown"
                                              '(:path "/home/user/projects/myapp/src/main.el"))))

  :doc "nil when no args"
  (should (null (niwa-tool-display-string "Unknown" nil)))

  :doc "a signalling display-arg loses the label, never the turn"
  ;; Reached from `gptel--handle-pre-tool' inside gptel's curl sentinel,
  ;; and gptel transitions the request outside its own error guard: an
  ;; escape here strands the turn waiting on a tool result forever.
  (unwind-protect
      (let (captured)
        (niwa-tool-register
         (niwa-tool--create
          :name "Exploding" :read-only-p t
          :args '((patch string :required "Patch"))
          :display-arg (lambda (_args) (aref "1 file" 6))))
        (niwa-test--with-captured-diagnostics captured
          (should-not
           (niwa-tool-display-string "Exploding" '(:patch "x"))))
        (should (string-match-p "omitting label" captured)))
    (niwa-tool-clear-registry))

  :doc "a well-behaved display-arg is unaffected by the guard"
  (unwind-protect
      (progn
        (niwa-tool-register
         (niwa-tool--create
          :name "Calm" :read-only-p t
          :args '((patch string :required "Patch"))
          :display-arg (lambda (_args) "1 file")))
        (should (equal "1 file"
                       (niwa-tool-display-string "Calm" '(:patch "x")))))
    (niwa-tool-clear-registry)))

(niwa-deftest niwa-tool-callable-p ()
  ,test
  (test)
  :doc "native-only exceptions cannot enter ToolCall through a colliding identity"
  (let ((niwa-tool--registry (make-hash-table :test #'equal)))
    (dolist (spec '(("Good" "server" t) ("Probe" "server/team" nil)
                    ("team/Probe" "server" t) ("list" "niwa" nil)))
      (let ((tool (niwa-tool--create :name (car spec) :category (cadr spec))))
        (niwa-tool-register tool)
        (should (eq (not (null (niwa-tool-callable-p tool))) (nth 2 spec)))))))

(niwa-deftest niwa-define-tool/cold-dispatch ()
  ,test
  (test)
  :doc "native and wrapped tools dispatch from fresh source and compiled registries"
  (let* ((root (file-name-directory (locate-library "niwa")))
         (compiled-root (make-temp-file "niwa-cold-registry-" t))
         (emacs (expand-file-name invocation-name invocation-directory))
         (source (file-name-concat compiled-root "niwa-tool-registry.el")))
    (unwind-protect
        (progn
          (copy-file (file-name-concat root "niwa-tool-registry.el") source)
          (let ((byte-compile-verbose nil))
            (should (byte-compile-file source)))
          (dolist (library (list (file-name-concat root "niwa-tool-registry.el")
                                (concat source "c")))
            (dolist (wrapped '(nil t))
              (with-temp-buffer
                (ert-info ((format "registry: %s, wrapped: %s" library wrapped))
                  (let ((status
                         (call-process
                          emacs nil t nil "--batch" "-Q" "-L" root "--eval"
                          (prin1-to-string
                           `(progn
                              (load ,library nil t)
                              (require 'niwa-structs)
                              (setq-local niwa--session
                                          (niwa-session--create
                                           :name "cold-registry"
                                           :permission-mode 'ask))
                              (defvar cold-registry-calls 0)
                              ,(if wrapped
                                   '(niwa-define-tool
                                     :wrap
                                     (gptel-make-tool
                                      :name "ColdProbe" :category "source"
                                      :description "Cold wrapped probe"
                                      :args '((:name "text" :type string
                                               :description "Text"))
                                      :async t
                                      :function
                                      (lambda (callback text)
                                        (cl-incf cold-registry-calls)
                                        (funcall callback text)))
                                     :read-only-p t)
                                 '(niwa-define-tool
                                   :name "ColdProbe" :description "Cold native probe"
                                   :args ((text string :required "Text"))
                                   :read-only-p t
                                   :handler
                                   (lambda (args)
                                     (cl-incf cold-registry-calls)
                                     (list :result (plist-get args :text)))))
                              (when (featurep 'niwa-pipeline)
                                (error "Registration loaded execution pipeline"))
                              (let* ((tool (niwa-tool-get "ColdProbe"))
                                     (dispatch (gptel-tool-function
                                                (niwa-tool-gptel-tool tool)))
                                     (deliveries 0)
                                     result)
                                (funcall dispatch
                                         (lambda (value)
                                           (cl-incf deliveries)
                                           (setq result value))
                                         "cold result")
                                (unless (and (equal "cold result" result)
                                             (= 1 deliveries)
                                             (= 1 cold-registry-calls))
                                  (error "Cold dispatch failed: %S" result))
                                (funcall dispatch
                                         (lambda (value)
                                           (cl-incf deliveries)
                                           (setq result value))
                                         '(invalid argument))
                                (unless (and (string-prefix-p "Error:" result)
                                             (= 2 deliveries)
                                             (= 1 cold-registry-calls))
                                  (error "Invalid input reached handler: %S" result)))
                              (when (featurep 'niwa)
                                (error "Registry dispatch loaded umbrella")))))))
                    (ert-info ((buffer-string))
                      (should (= 0 status))
                      (should (string-empty-p (string-trim (buffer-string)))))))))))
      (delete-directory compiled-root t))))

(niwa-deftest niwa-tool--with-quiet-file-visit
  (:doc "visits a file without any question, even past the large-file size")
  (let ((file (make-temp-file "niwa-quiet-visit-" nil ".txt" "0123456789"))
        (large-file-warning-threshold 1)
        buffer)
    (unwind-protect
        (progn
          (let ((inhibit-interaction t))
            (setq buffer (niwa-tool--with-quiet-file-visit
                           (find-file-noselect file))))
          (should (equal "0123456789" (with-current-buffer buffer (buffer-string)))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (delete-file file))))

(niwa-deftest niwa-tool-arg-access
  (:doc "`niwa-tool-arg-access' returns the access an argument spec declares")
  ,test
  (test)
  :doc "returns the declared access"
  (should (eq 'read (niwa-tool-arg-access
                     '(source path-or-resource :required "S" :access read))))
  :doc "returns nil without a declaration"
  (progn
    (should-not (niwa-tool-arg-access '(source path-or-resource :required "S")))
    (should-not (niwa-tool-arg-access
                 '(source path-or-resource :optional "S" :enum ["a"])))))

(niwa-deftest niwa-tool-for-gptel ()
  ,test
  (test)
  :doc "resolves the registered provider and its request-local copies only"
  (progn
    (niwa-define-tool
     :name "TestProvider"
     :handler #'ignore
     :description "Provider identity"
     :args ((mode string :optional "Mode" :enum [])))
    (let* ((tool (niwa-tool-get "TestProvider" "niwa"))
           (registered (niwa-tool-gptel-tool tool))
           (copy (gptel--copy-tool registered))
           (foreign (gptel--copy-tool registered))
           (elsewhere (gptel--copy-tool registered))
           (uncategorized (gptel--copy-tool registered)))
      (setf (gptel-tool-args copy) nil
            (gptel-tool-function foreign) #'ignore
            (gptel-tool-category elsewhere) "other"
            (gptel-tool-category uncategorized) nil)
      (should (eq tool (niwa-tool-for-gptel registered)))
      (should (eq tool (niwa-tool-for-gptel copy)))
      (should-not (niwa-tool-for-gptel foreign))
      (should-not (niwa-tool-for-gptel elsewhere))
      (should-not (niwa-tool-for-gptel uncategorized))
      (should-not (niwa-tool-for-gptel "TestProvider"))
      (should-not (niwa-tool-for-gptel nil)))))

(niwa-deftest niwa-tool--default-summary ()
  ,test
  (test)
  :doc "keeps only the first sentence of the first line"
  (should (equal "Run it."
                 (niwa-tool--default-summary "  Run it. Then more.\nOther line.")))
  :doc "keeps a first line without sentence punctuation whole"
  (should (equal "Outline of a file"
                 (niwa-tool--default-summary "Outline of a file\nMore.")))
  :doc "does not split inside a dotted word"
  (should (equal "Read gptel.el source."
                 (niwa-tool--default-summary "Read gptel.el source. Rest.")))
  :doc "bounds a long sentence"
  (should (= 160 (length (niwa-tool--default-summary (make-string 300 ?a)))))
  :doc "returns nil without a description"
  (progn
    (should-not (niwa-tool--default-summary nil))
    (should-not (niwa-tool--default-summary "  \n"))))

(niwa-deftest niwa-tool-contextual-contract ()
  ,test
  (test)
  :doc "returns the registered arguments and summary without a contextualizer"
  (let ((tool (niwa-tool--create
               :name "Plain" :summary "Plain summary"
               :args '((x string :required "x")))))
    (should (equal '(((x string :required "x")) . "Plain summary")
                   (niwa-tool-contextual-contract tool))))

  :doc "lets the tool resolve its arguments and summary for the current buffer"
  (let* ((static '((x string :required "static")))
         (tool (niwa-tool--create
                :name "Contextual" :summary "Static" :args static
                :contextualize
                (lambda (args summary)
                  (cons (list (list (car (car args)) 'string :required
                                    (buffer-name)))
                        (concat summary " here"))))))
    (with-temp-buffer
      (rename-buffer "contextual-buffer" t)
      (let ((resolved (niwa-tool-contextual-contract tool)))
        (should (string-prefix-p "contextual-buffer"
                                 (nth 3 (car (car resolved)))))
        (should (equal "Static here" (cdr resolved)))))
    ;; The registered specification is never edited.
    (should (equal '((x string :required "static")) (niwa-tool-args tool)))))

(provide 'test-niwa-tool-registry)
;;; test-niwa-tool-registry.el ends here
