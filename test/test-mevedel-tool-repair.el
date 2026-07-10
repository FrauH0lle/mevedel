;;; test-mevedel-tool-repair.el --- Tests for mevedel-tool-repair.el -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'mevedel-tool-registry)
(require 'mevedel-tool-repair)
(require 'mevedel-agents)
(require 'mevedel-structs)
(require 'mevedel-utilities)
(require 'gptel)
(require 'gptel-openai)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))


;;
;;; Structured validation

(mevedel-deftest mevedel-tool-repair-validate
  ()
  ,test
  (test)

  :doc "accepts valid scalar, enum, array, and nested object values"
  (let ((tool
         (mevedel-tool--create
          :name "Configure"
          :args '((name string :required "Name")
                  (mode string :optional "Mode" :enum ["fast" "safe"])
                  (ids array :optional "IDs"
                       :items (:type integer) :minItems 1 :maxItems 2)
                  (config object :optional "Configuration"
                          :properties (:host (:type string)
                                       :port (:type integer))
                          :required ["host"])))))
    (should-not
     (mevedel-tool-repair-validate
      tool
      '(:name "demo" :mode "safe" :ids [1 2]
              :config (:host "localhost" :port 8080)))))

  :doc "returns ordered issues for invalid declared values"
  (let* ((tool
          (mevedel-tool--create
           :name "Configure"
           :args '((name string :required "Name")
                   (mode string :optional "Mode" :enum ["fast" "safe"])
                   (ids array :optional "IDs"
                        :items (:type integer) :minItems 3 :maxItems 3)
                   (config object :optional "Configuration"
                           :properties (:host (:type string)
                                        :port (:type integer))
                           :required ["host"]))))
         (issues
          (mevedel-tool-repair-validate
           tool
           '(:mode "turbo" :ids [1 "two"]
                   :config (:port "8080") :extra t))))
    (should
     (equal
      '((:path (name) :kind missing-required :expected string :actual missing
         :schema (:type string))
        (:path (mode) :kind invalid-enum :expected ["fast" "safe"]
         :actual string :schema (:type string :mevedel-optional t
                                 :enum ["fast" "safe"]))
        (:path (ids) :kind too-few-items :expected 3 :actual 2
         :schema (:type array :mevedel-optional t :items (:type integer)
                  :minItems 3 :maxItems 3))
        (:path (ids 1) :kind wrong-type :expected integer :actual string
         :schema (:type integer))
        (:path (config host) :kind missing-required :expected string
         :actual missing :schema (:type string))
        (:path (config port) :kind wrong-type :expected integer
         :actual string :schema (:mevedel-optional t :type integer))
        (:path (extra) :kind unexpected-property :expected absent
         :actual boolean :schema nil))
      issues)))

  :doc "accepts absent optional values and both JSON booleans"
  (let ((tool
         (mevedel-tool--create
          :name "Toggle"
          :args '((enabled boolean :required "Whether enabled")
                  (note string :optional "Note")))))
    (should-not (mevedel-tool-repair-validate tool '(:enabled t)))
    (should-not
     (mevedel-tool-repair-validate tool '(:enabled :json-false :note nil))))

  :doc "distinguishes array and object shapes"
  (let ((tool
         (mevedel-tool--create
          :name "Shapes"
          :args '((values array :required "Values")
                  (options object :required "Options")))))
    (should
     (equal '(wrong-type wrong-type)
            (mapcar
             (lambda (issue) (plist-get issue :kind))
             (mevedel-tool-repair-validate
              tool '(:values (:one 1) :options [1 2]))))))

  :doc "fails closed for unsupported schema types"
  (let ((tool
         (mevedel-tool--create
          :name "BrokenSchema"
          :args '((value typo :required "Value")))))
    (should-error
     (mevedel-tool-repair-validate tool '(:value "anything"))
     :type 'error))

  :doc "preflights unsupported nested types even when values are absent"
  (dolist
      (spec '((items array :optional "Items" :items (:type typo))
              (config object :optional "Config"
                      :properties (:child (:type typo)))))
    (let ((tool
           (mevedel-tool--create :name "BrokenNestedSchema"
                                 :args (list spec))))
      (should-error (mevedel-tool-repair-validate tool nil) :type 'error)
      (should-error (mevedel-tool-repair-validate tool 42) :type 'error))))


;;
;;; Safe model feedback

(mevedel-deftest mevedel-tool-repair-format-issues
  ()
  ,test
  (test)

  :doc "formats bounded actionable feedback without sensitive values"
  (let* ((tool
          (mevedel-tool--create
           :name "Read"
           :args '((file_path path :required "Path")
                   (command string :optional "Command")
                   (count integer :optional "Count"))))
         (issues
          (mevedel-tool-repair-validate
           tool
           '(:file_path 42
             :command ["rm -rf /private"]
             :count "12\nsecret")))
         (message (mevedel-tool-repair-format-issues tool issues)))
    (should (string-match-p "Read input is invalid" message))
    (should (string-match-p "file_path.*expected path" message))
    (should (string-match-p "command" message))
    (should (string-match-p "count" message))
    (should-not (string-match-p "private\|secret\|rm -rf" message))
    (should (< (length message) 1200)))

  :doc "includes applicable array constraints in wrong-type feedback"
  (let* ((tool
          (mevedel-tool--create
           :name "Collect"
           :args '((names array :required "Names"
                         :items (:type string) :minItems 1))))
         (message
          (mevedel-tool-repair-format-issues
           tool (mevedel-tool-repair-validate tool '(:names "alice")))))
    (should
     (string-match-p "array of string items, at least 1 item" message)))

  :doc "bounds long and multiline enum choices"
  (let* ((long (make-string 500 ?x))
         (tool
          (mevedel-tool--create
           :name "Choose"
           :args (list (list 'mode 'string :required "Mode"
                             :enum (vector long "line\nsentinel-secret")))))
         (message
          (mevedel-tool-repair-format-issues
           tool (mevedel-tool-repair-validate tool '(:mode "other")))))
    (should (< (length message) 1200))
    (should-not (string-match-p (regexp-quote long) message))
    (should-not (string-match-p "sentinel-secret" message)))

  :doc "bounds non-string enum choices and unexpected property names"
  (let* ((huge-number (read (make-string 500 ?9)))
         (secret-name (concat (make-string 5000 ?k)
                              "\nsentinel-secret"))
         (secret-key (intern (concat ":" secret-name)))
         (tool
          (mevedel-tool--create
           :name "Bounded"
           :args (list (list 'count 'integer :required "Count"
                             :enum (vector huge-number)))))
         (message
          (mevedel-tool-repair-format-issues
           tool
           (mevedel-tool-repair-validate
            tool (list :count 1 secret-key t)))))
    (should (< (length message) 1200))
    (should-not (string-match-p (regexp-quote (format "%s" huge-number))
                                message))
    (should-not (string-match-p "sentinel-secret" message)))

  :doc "limits the number of rendered issues"
  (let* ((specs (mapcar (lambda (number)
                          (list (intern (format "field%d" number))
                                'string :required "Field"))
                        (number-sequence 1 20)))
         (tool (mevedel-tool--create :name "Large" :args specs))
         (message
          (mevedel-tool-repair-format-issues
           tool (mevedel-tool-repair-validate tool nil))))
    (should (string-match-p "additional issues" message))
    (should (< (length message) 1200))))


;;
;;; Atomic repair

(mevedel-deftest mevedel-tool-repair--with-lossless-json
  ()
  ,test
  (test)

  :doc "scopes lossless parsing to mevedel response buffers"
  (with-temp-buffer
    (setq-local mevedel--session t)
    (should
     (mevedel-tool-repair--with-lossless-json
      (lambda (&rest _) mevedel-tool-repair--parsing-response)
      'backend nil (list :buffer (current-buffer)))))

  :doc "leaves unrelated gptel response buffers unchanged"
  (with-temp-buffer
    (should-not
     (mevedel-tool-repair--with-lossless-json
      (lambda (&rest _) mevedel-tool-repair--parsing-response)
      'backend nil (list :buffer (current-buffer))))))

(mevedel-deftest mevedel-tool-repair--json-parse-string
  ()
  ,test
  (test)

  :doc "preserves null while gptel parses a response"
  (let ((mevedel-tool-repair--parsing-response t))
    (should
     (equal '(:empty nil :null :null)
            (mevedel-tool-repair--json-parse-string
             #'json-parse-string
             "{\"empty\":{},\"null\":null}"
             :object-type 'plist :null-object nil
             :false-object :json-false))))

  :doc "leaves unrelated JSON parsing unchanged"
  (should
   (equal '(:null nil)
          (mevedel-tool-repair--json-parse-string
           #'json-parse-string "{\"null\":null}"
           :object-type 'plist :null-object nil
           :false-object :json-false))))

(mevedel-deftest mevedel-tool-repair--restore-argument-shapes
  ()
  ,test
  (test)

  :doc "distinguishes decoded empty objects from nulls before repair"
  (let* ((tool
          (mevedel-tool--create
           :name "TaskUpdate"
           :args '((id integer :required "ID")
                   (blocks array :optional "Blocks"
                           :items (:type integer)))))
         (empty-call '(:id "empty" :name "TaskUpdate"
                       :args (:id 3 :blocks nil)))
         (null-call '(:id "null" :name "TaskUpdate"
                      :args (:id 4 :blocks :null)))
         (info (list :tool-use (list empty-call null-call)))
         (fsm (gptel-make-fsm :info info)))
    (cl-letf (((symbol-function 'mevedel-tool-get)
               (lambda (&rest _) tool)))
      (mevedel-tool-repair--restore-argument-shapes fsm))
    (let* ((empty-args (plist-get empty-call :args))
           (null-args (plist-get null-call :args))
           (empty-outcome (mevedel-tool-repair-attempt tool empty-args))
           (null-outcome (mevedel-tool-repair-attempt tool null-args)))
      (should (hash-table-p (plist-get empty-args :blocks)))
      (should (eq :null (plist-get null-args :blocks)))
      (should (equal [] (plist-get (plist-get empty-outcome :args) :blocks)))
      (should-not (plist-member (plist-get null-outcome :args) :blocks))
      (should (eq 'empty-array-placeholder
                  (plist-get (car (plist-get empty-outcome :repairs)) :rule)))
      (should (eq 'omit-optional-null
                  (plist-get (car (plist-get null-outcome :repairs)) :rule))))))

(mevedel-deftest mevedel-tool-repair-attempt
  ()
  ,test
  (test)

  :doc "repairs optional nulls and JSON-encoded arrays in rule order"
  (let* ((tool
          (mevedel-tool--create
           :name "Collect"
           :args '((note string :optional "Note")
                   (names array :required "Names" :items (:type string)))))
         (input '(:note nil :names "[\"a\",\"b\"]"))
         (outcome (mevedel-tool-repair-attempt tool input)))
    (should (eq 'repaired (plist-get outcome :status)))
    (should (equal '(:names ["a" "b"]) (plist-get outcome :args)))
    (should
     (equal '(omit-optional-null parse-json-value)
            (mapcar (lambda (record) (plist-get record :rule))
                    (plist-get outcome :repairs))))
    (should (equal '(:note nil :names "[\"a\",\"b\"]") input)))

  :doc "parses an encoded array before considering singleton wrapping"
  (let* ((tool
          (mevedel-tool--create
           :name "Collect"
           :args '((names array :required "Names" :items (:type string)))))
         (outcome
          (mevedel-tool-repair-attempt tool '(:names "[\"a\",\"b\"]"))))
    (should (equal ["a" "b"] (plist-get (plist-get outcome :args) :names)))
    (should
     (equal 'parse-json-value
            (plist-get (car (plist-get outcome :repairs)) :rule))))

  :doc "wraps a schema-compatible singleton as an array"
  (let* ((tool
          (mevedel-tool--create
           :name "Collect"
           :args '((names array :required "Names" :items (:type string)))))
         (outcome (mevedel-tool-repair-attempt tool '(:names "alice"))))
    (should (equal ["alice"] (plist-get (plist-get outcome :args) :names)))
    (should
     (equal 'wrap-array-singleton
            (plist-get (car (plist-get outcome :repairs)) :rule))))

  :doc "never parses JSON-shaped content declared as a string"
  (let* ((tool
          (mevedel-tool--create
           :name "Write"
           :args '((content string :required "Content"))))
         (input '(:content "[\"keep\",\"verbatim\"]"))
         (outcome (mevedel-tool-repair-attempt tool input)))
    (should (eq 'valid (plist-get outcome :status)))
    (should (equal input (plist-get outcome :args)))
    (should-not (plist-get outcome :repairs)))

  :doc "parses exact JSON scalars but never guesses coercions"
  (let* ((tool
          (mevedel-tool--create
           :name "Typed"
           :args '((count integer :required "Count")
                   (enabled boolean :required "Enabled"))))
         (outcome
          (mevedel-tool-repair-attempt
           tool '(:count "12" :enabled "true"))))
    (should (eq 'repaired (plist-get outcome :status)))
    (should (equal '(:count 12 :enabled t) (plist-get outcome :args)))
    (should
     (equal '(parse-json-value parse-json-value)
            (mapcar (lambda (record) (plist-get record :rule))
                    (plist-get outcome :repairs))))
    (should
     (eq 'invalid
         (plist-get
          (mevedel-tool-repair-attempt
           tool '(:count "twelve" :enabled "yes"))
          :status))))

  :doc "repairs both supported optional JSON-null representations"
  (let ((tool
         (mevedel-tool--create
          :name "Optional"
          :args '((note string :optional "Note")))))
    (dolist (null-value '(nil :null))
      (let ((outcome
             (mevedel-tool-repair-attempt
              tool (list :note null-value))))
        (should (eq 'repaired (plist-get outcome :status)))
        (should-not (plist-member (plist-get outcome :args) :note)))))

  :doc "does not turn required null into an empty array"
  (let* ((tool
          (mevedel-tool--create
           :name "Collect"
           :args '((names array :required "Names"))))
         (outcome (mevedel-tool-repair-attempt tool '(:names nil))))
    (should (eq 'invalid (plist-get outcome :status)))
    (should-not (plist-member outcome :args)))

  :doc "turns an optional empty object placeholder into a permitted empty array"
  (let* ((tool
          (mevedel-tool--create
           :name "Collect"
           :args '((names array :optional "Names" :items (:type string)))))
         (placeholder (make-hash-table))
         (outcome
          (mevedel-tool-repair-attempt tool (list :names placeholder))))
    (should (equal [] (plist-get (plist-get outcome :args) :names)))
    (should
     (eq 'empty-array-placeholder
         (plist-get (car (plist-get outcome :repairs)) :rule))))

  :doc "abandons all tentative changes when final validation fails"
  (let* ((tool
          (mevedel-tool--create
           :name "Collect"
           :args '((names array :required "Names" :items (:type string))
                   (count integer :required "Count"))))
         (input '(:names "alice" :count "not-an-integer"))
         (outcome (mevedel-tool-repair-attempt tool input)))
    (should (eq 'invalid (plist-get outcome :status)))
    (should-not (plist-member outcome :args))
    (should-not (plist-get outcome :repairs))
    (should (equal '(wrap-array-singleton)
                   (mapcar (lambda (record) (plist-get record :rule))
                           (plist-get outcome :abandoned-repairs))))
    (should (equal '(:names "alice" :count "not-an-integer") input)))

  :doc "the global switch disables repair but retains validation"
  (let* ((mevedel-tool-input-repair-enabled nil)
         (tool
          (mevedel-tool--create
           :name "Collect"
           :args '((names array :required "Names" :items (:type string)))))
         (outcome
          (mevedel-tool-repair-attempt tool '(:names "[\"alice\"]"))))
    (should (eq 'invalid (plist-get outcome :status)))
    (should-not (plist-member outcome :args)))

  :doc "runs one semantic callback and repairs its output in a second pass"
  (let* ((calls 0)
         (tool
         (mevedel-tool--create
          :name "Semantic"
          :args '((ids array :required "IDs" :items (:type integer)))
          :repair-input
          (lambda (_args _issues)
            (setq calls (1+ calls))
            '(:args (:ids "[1,2]")
              :repairs ((:rule supply-ids
                         :paths ((ids))
                         :before object
                         :after string
                         :summary "Supplied the default ID list.")))))))
    (let ((outcome
           (mevedel-tool-repair-attempt tool '(:ids (:placeholder t)))))
      (should (= 1 calls))
      (should (eq 'repaired (plist-get outcome :status)))
      (should (equal [1 2] (plist-get (plist-get outcome :args) :ids)))
      (should
       (equal '(supply-ids parse-json-value)
              (mapcar (lambda (record) (plist-get record :rule))
                      (plist-get outcome :repairs))))))

  :doc "runs semantic defaults even when each individual field is valid"
  (let ((tool
         (mevedel-tool--create
          :name "Read"
          :args '((offset integer :optional "Offset")
                  (limit integer :optional "Limit"))
          :repair-input
          (lambda (args _issues)
            (when (and (plist-member args :limit)
                       (not (plist-member args :offset)))
              (list
               :args (plist-put (copy-sequence args) :offset 0)
               :repairs
               '((:rule default-offset
                  :paths ((offset))
                  :before missing
                  :after integer
                  :summary "Defaulted the missing offset to zero."))))))))
    (let ((outcome (mevedel-tool-repair-attempt tool '(:limit 30))))
      (should (equal '(:limit 30 :offset 0) (plist-get outcome :args)))
      (should (eq 'default-offset
                  (plist-get (car (plist-get outcome :repairs)) :rule)))))

  :doc "rejects callback changes not covered by value-free records"
  (let ((tool
         (mevedel-tool--create
          :name "Broken"
          :args '((count integer :required "Count"))
          :repair-input
          (lambda (_args _issues)
            '(:args (:count 1) :repairs nil)))))
    (should-error
     (mevedel-tool-repair-attempt tool '(:count "one"))
     :type 'error))

  :doc "detects callback reversals across both generic passes"
  (let ((tool
         (mevedel-tool--create
          :name "Cycle"
          :args '((names array :required "Names" :items (:type string))
                  (count integer :required "Count"))
          :repair-input
          (lambda (args _issues)
            (list
             :args (plist-put (copy-sequence args) :names "alice")
             :repairs
             '((:rule reverse-array-repair
                :paths ((names))
                :before array
                :after string
                :summary "Reversed the array repair.")))))))
    (should-error
     (mevedel-tool-repair-attempt
     tool '(:names "alice" :count "not-json"))
     :type 'error))

  :doc "fails closed when the shared repair-attempt bound is exhausted"
  (let (specs args)
    (dotimes (index 33)
      (let ((name (intern (format "field%d" index))))
        (push (list name 'string :optional "Field") specs)
        (setq args (append args (list (intern (format ":%s" name)) nil)))))
    (let ((tool
           (mevedel-tool--create :name "TooManyRepairs"
                                 :args (nreverse specs))))
      (should-error (mevedel-tool-repair-attempt tool args) :type 'error)))

  :doc "rejects invalid callback identifiers and false schema paths separately"
  (dolist
      (record '((:rule nil :paths ((count)) :before string :after integer
                       :summary "Changed count.")
                (:rule unsafe-path :paths ((count "sentinel-secret"))
                       :before string :after integer :summary "Changed count.")
                (:rule nil-shape :paths ((count)) :before nil :after integer
                       :summary "Changed count.")
                (:rule secret-shape :paths ((count)) :before alice :after integer
                       :summary "Changed count.")
                (:rule false-path :paths ((count bogus))
                       :before string :after integer :summary "Changed count.")
                (:rule wrong-container :paths ((count 0))
                       :before string :after integer :summary "Changed count.")))
    (let ((tool
           (mevedel-tool--create
            :name "UnsafeRecord"
            :args '((count integer :required "Count"))
            :repair-input
            (lambda (_args _issues)
              (list :args '(:count 1) :repairs (list record))))))
      (should-error
       (mevedel-tool-repair-attempt tool '(:count "one"))
       :type 'error)))

  :doc "unwraps exact HTTP and HTTPS auto-links in the final path component"
  (let ((tool
         (mevedel-tool--create
          :name "Read"
          :args '((file_path path :required "Path")))))
    (dolist (case '(("[notes.md](http://notes.md)" . "notes.md")
                    ("/tmp/[notes.md](https://notes.md)" . "/tmp/notes.md")))
      (let ((outcome
             (mevedel-tool-repair-attempt
              tool (list :file_path (car case)))))
        (should (eq 'repaired (plist-get outcome :status)))
        (should (equal (cdr case)
                       (plist-get (plist-get outcome :args) :file_path)))
        (should
         (eq 'unwrap-path-autolink
             (plist-get (car (plist-get outcome :repairs)) :rule))))))

  :doc "leaves non-degenerate Markdown and near misses unchanged"
  (let ((tool
         (mevedel-tool--create
          :name "Read"
          :args '((file_path path :required "Path")))))
    (dolist (path '("[click](https://example.com)"
                    "[notes.md](ftp://notes.md)"
                    "[notes.md](HTTPS://notes.md)"
                    "[Notes.md](https://notes.md)"
                    "[notes.md](https://notes.md)/suffix"
                    "[notes md](https://notes md)"
                    "[dir/notes.md](https://dir/notes.md)"
                    "prefix[notes.md](https://notes.md)"))
      (let ((outcome
             (mevedel-tool-repair-attempt tool (list :file_path path))))
        (should (eq 'valid (plist-get outcome :status)))
        (should (equal path
                       (plist-get (plist-get outcome :args) :file_path))))))

  :doc "never applies path repair to an ordinary string contract"
  (let* ((tool
          (mevedel-tool--create
           :name "Wrapped"
           :args '((file_path string :required "Path-like string"))))
         (value "[notes.md](https://notes.md)")
         (outcome
          (mevedel-tool-repair-attempt tool (list :file_path value))))
    (should (eq 'valid (plist-get outcome :status)))
    (should (equal value
                   (plist-get (plist-get outcome :args) :file_path)))))

(mevedel-deftest mevedel-tool-repair-normalize-audit-record
  ()
  ,test
  (test)

  :doc "retains only bounded value-free repair audit fields"
  (let* ((record
          '(:type tool-input-repair :state committed
                  :repairs
                  ((:rule wrap-array-singleton :source generic
                          :paths ((names) (config ids 0))
                          :before string :after array
                          :summary "Contains sentinel-secret"))))
         (normalized (mevedel-tool-repair-normalize-audit-record record)))
    (should
     (equal
      '(:type tool-input-repair :state committed
              :repairs
              ((:rule wrap-array-singleton :source generic
                      :paths ((names) (config ids 0))
                      :before string :after array)))
      normalized))
    (should-not (string-match-p "sentinel" (prin1-to-string normalized))))

  :doc "rejects malformed states, identifiers, paths, and value fields"
  (dolist
      (record
       '((:type tool-input-repair :state unknown :repairs nil)
         (:type tool-input-repair :state committed
                :repairs ((:rule "secret" :source generic
                                :paths ((names)) :before string :after array)))
         (:type tool-input-repair :state committed
                :repairs ((:rule rule :source generic
                                :paths ((/private/path))
                                :before string :after array)))
         (:type tool-input-repair :state committed
                :repairs ((:rule rule :source generic
                                :paths ((names)) :before "value" :after array)))
         (:type tool-input-repair :state committed
                :repairs ((:rule rule :source generic
                                :paths ((names))
                                :before secret123 :after array)))))
    (should-not (mevedel-tool-repair-normalize-audit-record record)))
  (let ((cyclic-path (list 'names)))
    (setcdr cyclic-path cyclic-path)
    (should-not
     (mevedel-tool-repair-normalize-audit-record
      (list :type 'tool-input-repair :state 'committed
            :repairs
            (list (list :rule 'rule :source 'generic
                        :paths (list cyclic-path)
                        :before 'string :after 'array)))))))

(mevedel-deftest mevedel-tool-repair-audit-record
  ()
  ,test
  (test)

  :doc "builds the normalized committed or abandoned audit envelope"
  (dolist (state '(committed abandoned))
    (let ((record
           (mevedel-tool-repair-audit-record
            state
            '((:rule parse-json-value :source generic
                    :paths ((count)) :before string :after integer)))))
      (should (eq state (plist-get record :state)))
      (should (eq 'tool-input-repair (plist-get record :type))))))

(mevedel-deftest mevedel-tool-repair-format-audit-block
  ()
  ,test
  (test)

  :doc "formats a hidden ignored side channel without summaries or values"
  (let* ((block
          (mevedel-tool-repair-format-audit-block
           'committed
           '((:rule wrap-array-singleton :source generic
                   :paths ((names)) :before string :after array
                   :summary "sentinel-secret"))))
         record)
    (should (string-match-p mevedel--hook-audit-open block))
    (should (eq 'ignore (get-text-property 0 'gptel block)))
    (with-temp-buffer
      (insert block)
      (goto-char (point-min))
      (search-forward mevedel--hook-audit-open)
      (let ((start (point)))
        (search-forward mevedel--hook-audit-close)
        (setq record
              (mevedel--read-hook-audit-record
               (buffer-substring-no-properties start (match-beginning 0))))))
    (should (eq 'committed (plist-get record :state)))
    (should-not (string-match-p "sentinel" (prin1-to-string record))))

  :doc "malformed records fail open to an empty block"
  (should
   (equal ""
          (mevedel-tool-repair-format-audit-block
           'committed '((:rule "bad" :paths ((names))))))))

(mevedel-deftest mevedel-tool-repair-format-path
  ()
  ,test
  (test)

  :doc "formats nested object and array schema paths"
  (should (equal "config.items[2].name"
                 (mevedel-tool-repair-format-path
                  '(config items 2 name)))))


;;
;;; Raw gptel adapter and dispatch ledger

(mevedel-deftest mevedel-tool-repair-pre-tool-call
  (:before-each (mevedel-tool-clear-registry)
   :after-each (mevedel-tool-clear-registry))
  ,test
  (test)

  :doc "returns repaired args and records the call for pipeline dispatch"
  (let ((tool
         (mevedel-tool--create
          :name "Collect"
          :category "mevedel"
          :args '((names array :required "Names" :items (:type string))))))
    (mevedel-tool-register tool)
    (with-temp-buffer
      (let ((result
             (mevedel-tool-repair-pre-tool-call
              '(:name "Collect" :args (:names "alice")
                :backend test-backend :model test-model))))
        (should (equal '(:names ["alice"]) (plist-get result :args)))
        (let ((entry
               (mevedel-tool-repair-consume-ledger-entry
                tool '(:names ["alice"]))))
          (should (eq 'repaired (plist-get entry :status)))
          (should (eq 'wrap-array-singleton
                      (plist-get (car (plist-get entry :repairs)) :rule)))))))

  :doc "settles invalid input with a direct leading Error result"
  (let ((tool
         (mevedel-tool--create
          :name "Count"
          :category "mevedel"
          :args '((count integer :required "Count")))))
    (mevedel-tool-register tool)
    (with-temp-buffer
      (let ((result
             (mevedel-tool-repair-pre-tool-call
              '(:name "Count" :args (:count "twelve")))))
        (should (string-prefix-p "Error:" (plist-get result :result)))
        (should-not (plist-member result :block)))))

  :doc "marks a partially repaired invalid call as abandoned and auditable"
  (let* ((session (mevedel-session--create :name "main"))
         (tool
          (mevedel-tool--create
           :name "CollectCount" :category "mevedel"
           :args '((names array :required "Names" :items (:type string))
                   (count integer :required "Count"))))
         (info '(:name "CollectCount"
                       :args (:names "alice" :count "not-a-number"))))
    (mevedel-tool-register tool)
    (with-temp-buffer
      (setq-local mevedel--session session)
      (let* ((adapted (mevedel-tool-repair-pre-tool-call info))
             (result (plist-get adapted :result)))
        (should (string-prefix-p "Error:" result))
        (should (string-match-p mevedel--hook-audit-open result))
        (mevedel-tool-repair-post-tool-call
         (append info (list :result result)))))
    (let ((event (car (mevedel-session-repair-log session))))
      (should (eq 'abandoned (plist-get event :outcome)))
      (should (equal '(wrap-array-singleton) (plist-get event :rules)))
      (should (equal '((names) (count)) (plist-get event :paths)))))

  :doc "settles internal callback errors instead of leaking them to gptel"
  (let ((session (mevedel-session--create :name "main"))
        (tool
         (mevedel-tool--create
          :name "BrokenRepair"
          :category "mevedel"
          :args '((count integer :required "Count"))
          :repair-input
          (lambda (&rest _)
            (error "Repair exploded with /private/sentinel-secret")))))
    (mevedel-tool-register tool)
    (with-temp-buffer
      (setq-local mevedel--session session)
      (let (warning)
        (cl-letf (((symbol-function 'display-warning)
                   (lambda (_type message &rest _)
                     (setq warning message))))
          (let* ((info '(:name "BrokenRepair" :args (:count "twelve")))
                 (result
                 (mevedel-tool-repair-pre-tool-call
                  info)))
            (should (string-prefix-p "Error:" (plist-get result :result)))
            (should (string-match-p "internally" (plist-get result :result)))
            (mevedel-tool-repair-post-tool-call
             (append info (list :result (plist-get result :result))))))
        (should warning)
        (should-not (string-match-p "private\|sentinel-secret" warning))))
    (let ((event (car (mevedel-session-repair-log session))))
      (should (eq 'internal-repair-error
                  (plist-get event :failure-class)))
      (should (equal '(internal-error) (plist-get event :issue-kinds)))))

  :doc "consumes identical final args in original call order"
  (let ((tool
         (mevedel-tool--create
          :name "Collect"
          :category "mevedel"
          :args '((note string :optional "Note")
                  (names array :required "Names" :items (:type string))))))
    (mevedel-tool-register tool)
    (with-temp-buffer
      (mevedel-tool-repair-pre-tool-call
       '(:name "Collect" :args (:note nil :names "alice")))
      (mevedel-tool-repair-pre-tool-call
       '(:name "Collect" :args (:names ["alice"])))
      (let ((first
             (mevedel-tool-repair-consume-ledger-entry
              tool '(:note nil :names ["alice"])))
            (second
             (mevedel-tool-repair-consume-ledger-entry
              tool '(:note nil :names ["alice"]))))
        (should (eq 'repaired (plist-get first :status)))
        (should (= 2 (length (plist-get first :repairs))))
        (should (eq 'valid (plist-get second :status)))
        (should-not (plist-get second :repairs)))))

  :doc "turn cleanup prevents stale entries from matching a later call"
  (let ((tool
         (mevedel-tool--create
          :name "Collect"
          :category "mevedel"
          :args '((names array :required "Names" :items (:type string))))))
    (mevedel-tool-register tool)
    (with-temp-buffer
      (mevedel-tool-repair-pre-tool-call
       '(:name "Collect" :args (:names "alice")))
      (mevedel-tool-repair-clear-ledger)
      (should-not
       (mevedel-tool-repair-consume-ledger-entry
        tool '(:names ["alice"]))))))


;;
;;; Repair telemetry

(mevedel-deftest mevedel-tool-repair-log-event
  ()
  ,test
  (test)

  :doc "bounds memory and appends events after a save path is present"
  (let* ((dir (file-name-as-directory
               (make-temp-file "mevedel-repair-log-" t)))
         (session (mevedel-session--create :name "main"))
         (mevedel-tool-repair-log-limit 2)
         (event '(:time "now" :origin "main" :backend backend
                        :model model :tool "Read" :outcome valid
                        :repair-enabled t :rules nil :paths nil
                        :issue-kinds nil :execution executed :result success)))
    (unwind-protect
        (progn
          (mevedel-tool-repair-log-event session event)
          (setf (mevedel-session-save-path session) dir)
          (mevedel-tool-repair-log-event
           session (plist-put (copy-sequence event) :outcome 'repaired))
          (mevedel-tool-repair-log-event
           session (plist-put (copy-sequence event) :outcome 'invalid))
          (should (= 2 (length (mevedel-session-repair-log session))))
          (with-temp-buffer
            (insert-file-contents
             (mevedel-tool-repair-log-path session))
            (goto-char (point-min))
            (should (= 2 (count-lines (point-min) (point-max))))))
      (delete-directory dir t)))

  :doc "logging failures never escape into validated execution"
  (let* ((dir (file-name-as-directory
               (make-temp-file "mevedel-repair-log-fail-" t)))
         (session (mevedel-session--create :name "main" :save-path dir))
         warned)
    (unwind-protect
        (cl-letf (((symbol-function 'append-to-file)
                   (lambda (&rest _) (error "private persistence path")))
                  ((symbol-function 'display-warning)
                   (lambda (_type message &rest _)
                     (setq warned message))))
          (should
           (mevedel-tool-repair-log-event
            session
            '(:time "now" :origin "main" :backend backend
                    :model model :tool "Read" :outcome valid
                    :repair-enabled t :rules nil :paths nil
                    :issue-kinds nil :execution executed :result success)))
          (should warned)
          (should-not (string-match-p "private" warned)))
      (delete-directory dir t)))

  :doc "normalizes backend objects by name without collapsing providers"
  (let* ((gptel--known-backends nil)
         (first (gptel-make-openai
                 "DeepSeek" :key "test" :models '(deepseek-model)))
         (second (gptel-make-openai
                  "OpenRouter" :key "test" :models '(router-model)))
         (session (mevedel-session--create :name "main"))
         (base '(:time "now" :origin "main" :model model :tool "Read"
                       :outcome valid :repair-enabled t :rules nil :paths nil
                       :issue-kinds nil :execution executed :result success)))
    (mevedel-tool-repair-log-event
     session (plist-put (copy-sequence base) :backend first))
    (mevedel-tool-repair-log-event
     session (plist-put (copy-sequence base) :backend second))
    (should (equal '("DeepSeek" "OpenRouter")
                   (mapcar (lambda (event) (plist-get event :backend))
                           (mevedel-session-repair-log session)))))

  :doc "bounds collections, redacts hostile paths, and validates categories"
  (let (rules paths issue-kinds)
    (dotimes (index 25)
      (push (intern (format "rule-%d" index)) rules)
      (push (list (intern (if (= index 0)
                              "/private/sentinel-key"
                            (format "field-%d" index))))
            paths)
      (push (intern (format "issue-%d" index)) issue-kinds))
    (setq rules (nreverse rules)
          paths (nreverse paths)
          issue-kinds (nreverse issue-kinds))
    (setcar paths
            (cons (intern "/private/sentinel-key")
                  (make-list 20 'nested)))
    (let* ((session (mevedel-session--create :name "main"))
           (event
            (list :time "now" :origin "main" :backend 'backend :model 'model
                  :tool "Read" :outcome "sentinel-outcome"
                  :repair-enabled t :rules rules :paths paths
                  :issue-kinds issue-kinds :execution "sentinel-execution"
                  :result "sentinel-result"
                  :failure-class 'sentinel-failure)))
      (mevedel-tool-repair-log-event session event)
      (let* ((safe (car (mevedel-session-repair-log session)))
             (printed (prin1-to-string safe)))
        (should (= 16 (length (plist-get safe :rules))))
        (should (= 9 (plist-get safe :rules-omitted)))
        (should (= 16 (length (plist-get safe :paths))))
        (should (= 9 (plist-get safe :paths-omitted)))
        (should (= 5 (plist-get safe :path-components-omitted)))
        (should (eq 'redacted (car (car (plist-get safe :paths)))))
        (should (= 16 (length (plist-get safe :issue-kinds))))
        (should (= 9 (plist-get safe :issue-kinds-omitted)))
        (should (eq 'unknown (plist-get safe :outcome)))
        (should (eq 'unknown (plist-get safe :execution)))
        (should (eq 'unknown (plist-get safe :result)))
        (should-not (plist-get safe :failure-class))
        (should-not (string-match-p "sentinel" printed))))))

(mevedel-deftest mevedel-tool-repair-record-result
  ()
  ,test
  (test)

  :doc "records redacted child metadata and result classification exactly once"
  (let* ((session (mevedel-session--create :name "main"))
         (entry
          (list :tool "Write" :status 'repaired
                :args '(:file_path "/private/sentinel-path")
                :repairs
                '((:rule unwrap-path-autolink :source generic
                         :paths ((file_path)) :before markdown-link :after path
                         :summary "Unwrapped a secret path."))
                :issues nil :backend 'child-backend :model 'child-model
                :session session :origin "explorer--stable"
                :repair-enabled t :execution 'executed
                :telemetry-recorded nil)))
    (mevedel-tool-repair-record-result
     entry "Error: sentinel-result /private/sentinel-path")
    (mevedel-tool-repair-record-result entry "second result")
    (let* ((events (mevedel-session-repair-log session))
           (event (car events))
           (printed (prin1-to-string event)))
      (should (= 1 (length events)))
      (should (equal "explorer--stable" (plist-get event :origin)))
      (should (eq 'child-backend (plist-get event :backend)))
      (should (eq 'child-model (plist-get event :model)))
      (should (eq 'repaired (plist-get event :outcome)))
      (should (eq 'executed (plist-get event :execution)))
      (should (eq 'error (plist-get event :result)))
      (should (equal '(unwrap-path-autolink) (plist-get event :rules)))
      (should (equal '((file_path)) (plist-get event :paths)))
      (dolist (secret '("sentinel-path" "sentinel-result" "private"
                        "Unwrapped a secret path"))
        (should-not (string-match-p secret printed)))))

  :doc "records disabled repair and invalid issue kinds without values"
  (let* ((session (mevedel-session--create :name "main"))
         (secret-path (intern "file\nsentinel-secret"))
         (entry
          (list :tool "Read" :status 'invalid :repairs nil
                :issues
                (list (list :path (list secret-path) :kind 'wrong-type
                            :expected 'path :actual 'string
                            :schema '(:type path)))
                :backend 'backend :model 'model :session session
                :origin "main" :repair-enabled nil
                :execution 'not-executed :telemetry-recorded nil)))
    (mevedel-tool-repair-record-result entry "Error: private value")
    (let ((event (car (mevedel-session-repair-log session))))
      (should (plist-member event :repair-enabled))
      (should-not (plist-get event :repair-enabled))
      (should (eq 'invalid (plist-get event :outcome)))
      (should (equal '(wrong-type) (plist-get event :issue-kinds)))
      (should (eq 'not-executed (plist-get event :execution)))
      (should-not
       (string-match-p "sentinel-secret" (prin1-to-string event))))))

(mevedel-deftest mevedel-tool-repair-post-tool-call
  (:before-each (mevedel-tool-clear-registry)
   :after-each (mevedel-tool-clear-registry))
  ,test
  (test)

  :doc "does not settle a later batched call before it has a result"
  (let ((tool
         (mevedel-tool--create
          :name "Count" :category "mevedel"
          :args '((count integer :required "Count")))))
    (mevedel-tool-register tool)
    (with-temp-buffer
      (mevedel-tool-repair-pre-tool-call
       '(:name "Count" :args (:count 1)
         :backend backend :model model))
      (mevedel-tool-repair-post-tool-call
       '(:name "Count" :args (:count 1) :result nil))
      (should
       (mevedel-tool-repair-consume-ledger-entry tool '(:count 1)))))

  :doc "settles invalid raw calls into the parent session telemetry"
  (let* ((dir (file-name-as-directory
               (make-temp-file "mevedel-child-repair-log-" t)))
         (session (mevedel-session--create :name "main" :save-path dir))
         (invocation
          (mevedel-agent-invocation--create
           :agent-id "reviewer--stable" :parent-session session))
         (tool
          (mevedel-tool--create
           :name "Count" :category "mevedel"
           :args '((count integer :required "Count")))))
    (unwind-protect
        (progn
          (mevedel-tool-register tool)
          (with-temp-buffer
            (setq-local mevedel--session session)
            (setq-local mevedel--agent-invocation invocation)
            (let* ((info '(:name "Count"
                                  :args (:count "sentinel-secret")
                                  :backend child-backend :model child-model))
                   (adapted (mevedel-tool-repair-pre-tool-call info)))
              (mevedel-tool-repair-post-tool-call
               (append info (list :result (plist-get adapted :result))))))
          (let ((event (car (mevedel-session-repair-log session))))
            (should (eq 'invalid (plist-get event :outcome)))
            (should (equal "reviewer--stable" (plist-get event :origin)))
            (should (eq 'child-backend (plist-get event :backend)))
            (should (eq 'child-model (plist-get event :model)))
            (should-not
             (string-match-p "sentinel-secret" (prin1-to-string event))))
          (with-temp-buffer
            (insert-file-contents (mevedel-tool-repair-log-path session))
            (should (string-match-p "reviewer--stable" (buffer-string)))
            (should-not (string-match-p "sentinel-secret" (buffer-string)))))
      (delete-directory dir t))))

(mevedel-deftest mevedel-tool-repair-clear-ledger
  (:before-each (mevedel-tool-clear-registry)
   :after-each (mevedel-tool-clear-registry))
  ,test
  (test)

  :doc "records unconsumed calls as abandoned and clears only this buffer"
  (let* ((session (mevedel-session--create :name "main"))
         (tool
          (mevedel-tool--create
           :name "Collect" :category "mevedel"
           :args '((names array :required "Names" :items (:type string)))))
         other-buffer)
    (mevedel-tool-register tool)
    (unwind-protect
        (with-temp-buffer
          (setq-local mevedel--session session)
          (mevedel-tool-repair-pre-tool-call
           '(:name "Collect" :args (:names "alice")
             :backend backend :model model))
          (should
           (mevedel-tool-repair-consume-ledger-entry
            tool '(:names ["alice"])))
          (should-not mevedel-tool-repair--ledger)
          (should (= 1 (length mevedel-tool-repair--in-flight)))
          (setq other-buffer (generate-new-buffer " *repair-other*"))
          (with-current-buffer other-buffer
            (setq-local mevedel-tool-repair--ledger '(:other)))
          (mevedel-tool-repair-clear-ledger)
          (should-not mevedel-tool-repair--ledger)
          (should-not mevedel-tool-repair--in-flight)
          (with-current-buffer other-buffer
            (should mevedel-tool-repair--ledger)))
      (when (buffer-live-p other-buffer) (kill-buffer other-buffer)))
    (let ((event (car (mevedel-session-repair-log session))))
      (should (eq 'abandoned (plist-get event :outcome)))
      (should (eq 'not-executed (plist-get event :execution)))
      (should (eq 'none (plist-get event :result))))))

(provide 'test-mevedel-tool-repair)
;;; test-mevedel-tool-repair.el ends here
