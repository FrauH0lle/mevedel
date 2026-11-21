;;; test-niwa-tool-repair.el --- Tests for niwa-tool-repair.el -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'niwa-tool-registry)
(require 'niwa-tool-repair)
(require 'niwa-tool-repair-diagnostics)
(require 'niwa-agents)
(require 'niwa-execution-target)
(require 'niwa-session-durability)
(require 'niwa-session-publication)
(require 'niwa-structs)
(require 'niwa-utilities)
(require 'gptel)
(require 'gptel-openai)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))


;;
;;; Structured validation

(niwa-deftest niwa-tool-repair-validate
  ()
  ,test
  (test)

  :doc "accepts valid scalar, enum, array, and nested object values"
  (let ((tool
         (niwa-tool--create
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
     (niwa-tool-repair-validate
      tool
      '(:name "demo" :mode "safe" :ids [1 2]
              :config (:host "localhost" :port 8080)))))

  :doc "accepts resource addresses only for path-or-resource contracts"
  (let ((resource-tool
         (niwa-tool--create
          :name "ResourceRead"
          :args '((file_path path-or-resource :required "Path"))))
        (path-tool
         (niwa-tool--create
          :name "FilesystemRead"
          :args '((file_path path :required "Path")))))
    (should-not
     (niwa-tool-repair-validate
      resource-tool '(:file_path "work://notes.md")))
    (let ((issues
           (niwa-tool-repair-validate
            path-tool '(:file_path "work://notes.md"))))
      (should (equal '(resource-address)
                     (mapcar (lambda (issue) (plist-get issue :kind))
                             issues)))
      (should (string-match-p
               "filesystem-only path arguments do not accept resource addresses"
               (niwa-tool-repair-format-issues path-tool issues)))))

  :doc "rejects Markdown autolinks in path-or-resource contracts"
  (let* ((tool
          (niwa-tool--create
           :name "ResourceRead"
           :args '((file_path path-or-resource :required "Path"))))
         (issues
          (niwa-tool-repair-validate
           tool '(:file_path "[notes.md](https://notes.md)"))))
    (should (equal '(path-autolink)
                   (mapcar (lambda (issue) (plist-get issue :kind))
                           issues))))

  :doc "returns ordered issues for invalid declared values"
  (let* ((tool
          (niwa-tool--create
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
          (niwa-tool-repair-validate
           tool
           '(:mode "turbo" :ids [1 "two"]
                   :config (:port "8080") :extra t))))
    (should
     (equal
      '((:path (name) :kind missing-required :expected string :actual missing
         :schema (:type string))
        (:path (mode) :kind invalid-enum :expected ["fast" "safe"]
         :actual string :schema (:type string :niwa-optional t
                                 :enum ["fast" "safe"]))
        (:path (ids) :kind too-few-items :expected 3 :actual 2
         :schema (:type array :niwa-optional t :items (:type integer)
                  :minItems 3 :maxItems 3))
        (:path (ids 1) :kind wrong-type :expected integer :actual string
         :schema (:type integer))
        (:path (config host) :kind missing-required :expected string
         :actual missing :schema (:type string))
        (:path (config port) :kind wrong-type :expected integer
         :actual string :schema (:niwa-optional t :type integer))
        (:path (extra) :kind unexpected-property :expected absent
         :actual boolean :schema nil))
      issues)))

  :doc "accepts absent optional values and both JSON booleans"
  (let ((tool
         (niwa-tool--create
          :name "Toggle"
          :args '((enabled boolean :required "Whether enabled")
                  (note string :optional "Note")))))
    (should-not (niwa-tool-repair-validate tool '(:enabled t)))
    (should-not
     (niwa-tool-repair-validate tool '(:enabled :json-false :note nil))))

  :doc "distinguishes array and object shapes"
  (let ((tool
         (niwa-tool--create
          :name "Shapes"
          :args '((values array :required "Values")
                  (options object :required "Options")))))
    (should
     (equal '(wrong-type wrong-type)
            (mapcar
             (lambda (issue) (plist-get issue :kind))
             (niwa-tool-repair-validate
              tool '(:values (:one 1) :options [1 2]))))))

  :doc "flags numbers outside declared minimum/maximum bounds"
  (let ((tool
         (niwa-tool--create
          :name "Wait"
          :args '((timeout_ms integer :optional "Timeout"
                              :minimum 10000 :maximum 3600000)
                  (count integer :optional "Count")))))
    (should-not (niwa-tool-repair-validate tool '(:timeout_ms 10000)))
    (should-not (niwa-tool-repair-validate tool '(:timeout_ms 3600000)))
    (should-not (niwa-tool-repair-validate tool '(:count -5)))
    (should
     (equal
      '((:path (timeout_ms) :kind below-minimum :expected 10000
         :actual integer
         :schema (:type integer :niwa-optional t
                  :minimum 10000 :maximum 3600000)))
      (niwa-tool-repair-validate tool '(:timeout_ms 9999))))
    (let ((issues (niwa-tool-repair-validate tool '(:timeout_ms 3600001))))
      (should (equal '(above-maximum)
                     (mapcar (lambda (issue) (plist-get issue :kind))
                             issues)))
      (should (string-match-p
               "value must be at most 3600000"
               (niwa-tool-repair-format-issues tool issues)))))

  :doc "preserves presence and null rules at root and nested properties"
  (dolist (nested '(nil t))
    (dolist (required '(nil t))
      (dolist (raw '(nil t))
        (let* ((niwa-tool-repair--raw-input-p raw)
               (tool
                (niwa-tool--create
                 :name "Presence"
                 :args (if nested
                           `((config object :required "Config"
                                     :properties (:value (:type string))
                                     :required ,(if required ["value"] [])))
                         `((value string ,(if required :required :optional)
                                  "Value")))))
               (path (if nested '(config value) '(value))))
          ;; Columns: input, required issue, optional issue, raw optional issue.
          (dolist (entry '((nil missing-required nil nil)
                           ((:value nil) missing-required nil optional-null)
                           ((:value :null) missing-required wrong-type optional-null)
                           ((:value "ok") nil nil nil)
                           ((:value 42) wrong-type wrong-type wrong-type)
                           ((:value []) wrong-type wrong-type wrong-type)))
            (let* ((args (car entry))
                   (expected (nth (cond (required 1) (raw 3) (t 2)) entry))
                   (issues
                    (niwa-tool-repair-validate
                     tool (if nested
                              (list :config (append args '(:ignored t)))
                            args))))
              (should (equal (and expected (list expected))
                             (mapcar (lambda (issue) (plist-get issue :kind))
                                     issues)))
              (when expected
                (should (equal path (plist-get (car issues) :path))))))))))

  :doc "fails closed for unsupported schema types"
  (let ((tool
         (niwa-tool--create
          :name "BrokenSchema"
          :args '((value typo :required "Value")))))
    (should-error
     (niwa-tool-repair-validate tool '(:value "anything"))
     :type 'error))

  :doc "preflights unsupported nested types even when values are absent"
  (dolist
      (spec '((items array :optional "Items" :items (:type typo))
              (config object :optional "Config"
                      :properties (:child (:type typo)))))
    (let ((tool
           (niwa-tool--create :name "BrokenNestedSchema"
                                 :args (list spec))))
      (should-error (niwa-tool-repair-validate tool nil) :type 'error)
      (should-error (niwa-tool-repair-validate tool 42) :type 'error))))


;;
;;; Safe model feedback

(niwa-deftest niwa-tool-repair-format-issues
  ()
  ,test
  (test)

  :doc "formats bounded actionable feedback without sensitive values"
  (let* ((tool
          (niwa-tool--create
           :name "Read"
           :args '((file_path path :required "Path")
                   (command string :optional "Command")
                   (count integer :optional "Count"))))
         (issues
          (niwa-tool-repair-validate
           tool
           '(:file_path 42
             :command ["rm -rf /private"]
             :count "12\nsecret")))
         (message (niwa-tool-repair-format-issues tool issues)))
    (should (string-match-p "Read input is invalid" message))
    (should (string-match-p "file_path.*expected path" message))
    (should (string-match-p "command" message))
    (should (string-match-p "count" message))
    (should-not (string-match-p "private\|secret\|rm -rf" message))
    (should (< (length message) 1200)))

  :doc "includes applicable array constraints in wrong-type feedback"
  (let* ((tool
          (niwa-tool--create
           :name "Collect"
           :args '((names array :required "Names"
                         :items (:type string) :minItems 1))))
         (message
          (niwa-tool-repair-format-issues
           tool (niwa-tool-repair-validate tool '(:names "alice")))))
    (should
     (string-match-p "array of string items, at least 1 item" message)))

  :doc "bounds long and multiline enum choices"
  (let* ((long (make-string 500 ?x))
         (tool
          (niwa-tool--create
           :name "Choose"
           :args (list (list 'mode 'string :required "Mode"
                             :enum (vector long "line\nsentinel-secret")))))
         (message
          (niwa-tool-repair-format-issues
           tool (niwa-tool-repair-validate tool '(:mode "other")))))
    (should (< (length message) 1200))
    (should-not (string-match-p (regexp-quote long) message))
    (should-not (string-match-p "sentinel-secret" message)))

  :doc "bounds non-string enum choices and unexpected property names"
  (let* ((huge-number (read (make-string 500 ?9)))
         (secret-name (concat (make-string 5000 ?k)
                              "\nsentinel-secret"))
         (secret-key (intern (concat ":" secret-name)))
         (tool
          (niwa-tool--create
           :name "Bounded"
           :args (list (list 'count 'integer :required "Count"
                             :enum (vector huge-number)))))
         (message
          (niwa-tool-repair-format-issues
           tool
           (niwa-tool-repair-validate
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
         (tool (niwa-tool--create :name "Large" :args specs))
         (message
          (niwa-tool-repair-format-issues
           tool (niwa-tool-repair-validate tool nil))))
    (should (string-match-p "additional issues" message))
    (should (< (length message) 1200))))


;;
;;; Atomic repair

(niwa-deftest niwa-tool-repair--with-lossless-json
  ()
  ,test
  (test)

  :doc "scopes lossless parsing to niwa response buffers"
  (with-temp-buffer
    (setq-local niwa--session t)
    (should
     (niwa-tool-repair--with-lossless-json
      (lambda (&rest _) niwa-tool-repair--parsing-response)
      'backend nil (list :buffer (current-buffer)))))

  :doc "leaves unrelated gptel response buffers unchanged"
  (with-temp-buffer
    (should-not
     (niwa-tool-repair--with-lossless-json
      (lambda (&rest _) niwa-tool-repair--parsing-response)
      'backend nil (list :buffer (current-buffer))))))

(niwa-deftest niwa-tool-repair--json-parse-string
  ()
  ,test
  (test)

  :doc "preserves null while gptel parses a response"
  (let ((niwa-tool-repair--parsing-response t))
    (should
     (equal '(:empty nil :null :null)
            (niwa-tool-repair--json-parse-string
             #'json-parse-string
             "{\"empty\":{},\"null\":null}"
             :object-type 'plist :null-object nil
             :false-object :json-false))))

  :doc "leaves unrelated JSON parsing unchanged"
  (should
   (equal '(:null nil)
          (niwa-tool-repair--json-parse-string
           #'json-parse-string "{\"null\":null}"
           :object-type 'plist :null-object nil
           :false-object :json-false))))

(niwa-deftest niwa-tool-repair--restore-argument-shapes
  ()
  ,test
  (test)

  :doc "distinguishes decoded empty objects from nulls before repair"
  (let* ((tool
          (niwa-tool--create
           :name "TaskUpdate"
           :args '((id integer :required "ID")
                   (blockedBy array :optional "Blocked by"
                              :items (:type integer)))))
         (empty-call '(:id "empty" :name "TaskUpdate"
                       :args (:id 3 :blockedBy nil)))
         (null-call '(:id "null" :name "TaskUpdate"
                      :args (:id 4 :blockedBy :null)))
         (info (list :tool-use (list empty-call null-call)))
         (fsm (gptel-make-fsm :info info)))
    (cl-letf (((symbol-function 'niwa-tool-get)
               (lambda (&rest _) tool)))
      (niwa-tool-repair--restore-argument-shapes fsm))
    (let* ((empty-args (plist-get empty-call :args))
           (null-args (plist-get null-call :args))
           (empty-outcome (niwa-tool-repair-attempt tool empty-args))
           (null-outcome (niwa-tool-repair-attempt tool null-args)))
      (should (hash-table-p (plist-get empty-args :blockedBy)))
      (should (eq :null (plist-get null-args :blockedBy)))
      (should (equal [] (plist-get (plist-get empty-outcome :args) :blockedBy)))
      (should-not (plist-member (plist-get null-outcome :args) :blockedBy))
      (should (eq 'empty-array-placeholder
                  (plist-get (car (plist-get empty-outcome :repairs)) :rule)))
      (should (eq 'omit-optional-null
                  (plist-get (car (plist-get null-outcome :repairs)) :rule))))))

(niwa-deftest niwa-tool-repair-attempt
  ()
  ,test
  (test)

  :doc "repairs optional nulls and JSON-encoded arrays in rule order"
  (let* ((tool
          (niwa-tool--create
           :name "Collect"
           :args '((note string :optional "Note")
                   (names array :required "Names" :items (:type string)))))
         (input '(:note nil :names "[\"a\",\"b\"]"))
         (outcome (niwa-tool-repair-attempt tool input)))
    (should (eq 'repaired (plist-get outcome :status)))
    (should (equal '(:names ["a" "b"]) (plist-get outcome :args)))
    (should
     (equal '(omit-optional-null parse-json-value)
            (mapcar (lambda (record) (plist-get record :rule))
                    (plist-get outcome :repairs))))
    (should (equal '(:note nil :names "[\"a\",\"b\"]") input)))

  :doc "parses an encoded array before considering singleton wrapping"
  (let* ((tool
          (niwa-tool--create
           :name "Collect"
           :args '((names array :required "Names" :items (:type string)))))
         (outcome
          (niwa-tool-repair-attempt tool '(:names "[\"a\",\"b\"]"))))
    (should (equal ["a" "b"] (plist-get (plist-get outcome :args) :names)))
    (should
     (equal 'parse-json-value
            (plist-get (car (plist-get outcome :repairs)) :rule))))

  :doc "wraps a schema-compatible singleton as an array"
  (let* ((tool
          (niwa-tool--create
           :name "Collect"
           :args '((names array :required "Names" :items (:type string)))))
         (outcome (niwa-tool-repair-attempt tool '(:names "alice"))))
    (should (equal ["alice"] (plist-get (plist-get outcome :args) :names)))
    (should
     (equal 'wrap-array-singleton
            (plist-get (car (plist-get outcome :repairs)) :rule))))

  :doc "never parses JSON-shaped content declared as a string"
  (let* ((tool
          (niwa-tool--create
           :name "Write"
           :args '((content string :required "Content"))))
         (input '(:content "[\"keep\",\"verbatim\"]"))
         (outcome (niwa-tool-repair-attempt tool input)))
    (should (eq 'valid (plist-get outcome :status)))
    (should (equal input (plist-get outcome :args)))
    (should-not (plist-get outcome :repairs)))

  :doc "parses exact JSON scalars but never guesses coercions"
  (let* ((tool
          (niwa-tool--create
           :name "Typed"
           :args '((count integer :required "Count")
                   (enabled boolean :required "Enabled"))))
         (outcome
          (niwa-tool-repair-attempt
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
          (niwa-tool-repair-attempt
           tool '(:count "twelve" :enabled "yes"))
          :status))))

  :doc "repairs both supported optional JSON-null representations"
  (let ((tool
         (niwa-tool--create
          :name "Optional"
          :args '((note string :optional "Note")))))
    (dolist (null-value '(nil :null))
      (let ((outcome
             (niwa-tool-repair-attempt
              tool (list :note null-value))))
        (should (eq 'repaired (plist-get outcome :status)))
        (should-not (plist-member (plist-get outcome :args) :note)))))

  :doc "does not turn required null into an empty array"
  (let* ((tool
          (niwa-tool--create
           :name "Collect"
           :args '((names array :required "Names"))))
         (outcome (niwa-tool-repair-attempt tool '(:names nil))))
    (should (eq 'invalid (plist-get outcome :status)))
    (should-not (plist-member outcome :args)))

  :doc "turns an optional empty object placeholder into a permitted empty array"
  (let* ((tool
          (niwa-tool--create
           :name "Collect"
           :args '((names array :optional "Names" :items (:type string)))))
         (placeholder (make-hash-table))
         (outcome
          (niwa-tool-repair-attempt tool (list :names placeholder))))
    (should (equal [] (plist-get (plist-get outcome :args) :names)))
    (should
     (eq 'empty-array-placeholder
         (plist-get (car (plist-get outcome :repairs)) :rule))))

  :doc "clamps numbers to the declared range and reports the repair"
  (let* ((tool
          (niwa-tool--create
           :name "Wait"
           :args '((timeout_ms integer :optional "Timeout"
                               :minimum 10000 :maximum 3600000))))
         (input (list :timeout_ms 5000))
         (outcome (niwa-tool-repair-attempt tool input)))
    (should (eq 'repaired (plist-get outcome :status)))
    (should (equal '(:timeout_ms 10000) (plist-get outcome :args)))
    (should
     (equal 'clamp-range
            (plist-get (car (plist-get outcome :repairs)) :rule)))
    (should (string-match-p
             "declared minimum 10000"
             (plist-get (car (plist-get outcome :repairs)) :summary)))
    (should (equal '(:timeout_ms 5000) input))
    (let ((outcome (niwa-tool-repair-attempt tool '(:timeout_ms 9999999))))
      (should (equal '(:timeout_ms 3600000) (plist-get outcome :args)))
      (should (string-match-p
               "declared maximum 3600000"
               (plist-get (car (plist-get outcome :repairs)) :summary))))
    (should (eq 'valid
                (plist-get (niwa-tool-repair-attempt
                            tool '(:timeout_ms 30000))
                           :status))))

  :doc "parses a numeric string and then clamps it in the same pass"
  (let* ((tool
          (niwa-tool--create
           :name "Bash"
           :args '((yield_time_ms integer :optional "Yield"
                                  :minimum 250 :maximum 30000))))
         (outcome
          (niwa-tool-repair-attempt tool '(:yield_time_ms "999999"))))
    (should (eq 'repaired (plist-get outcome :status)))
    (should (equal '(:yield_time_ms 30000) (plist-get outcome :args)))
    (should
     (equal '(parse-json-value clamp-range)
            (mapcar (lambda (record) (plist-get record :rule))
                    (plist-get outcome :repairs)))))

  :doc "abandons all tentative changes when final validation fails"
  (let* ((tool
          (niwa-tool--create
           :name "Collect"
           :args '((names array :required "Names" :items (:type string))
                   (count integer :required "Count"))))
         (input '(:names "alice" :count "not-an-integer"))
         (outcome (niwa-tool-repair-attempt tool input)))
    (should (eq 'invalid (plist-get outcome :status)))
    (should-not (plist-member outcome :args))
    (should-not (plist-get outcome :repairs))
    (should (equal '(wrap-array-singleton)
                   (mapcar (lambda (record) (plist-get record :rule))
                           (plist-get outcome :abandoned-repairs))))
    (should (equal '(:names "alice" :count "not-an-integer") input)))

  :doc "the global switch disables repair but retains validation"
  (let* ((niwa-tool-input-repair-enabled nil)
         (tool
          (niwa-tool--create
           :name "Collect"
           :args '((names array :required "Names" :items (:type string)))))
         (outcome
          (niwa-tool-repair-attempt tool '(:names "[\"alice\"]"))))
    (should (eq 'invalid (plist-get outcome :status)))
    (should-not (plist-member outcome :args)))

  :doc "fails closed when the generic repair-attempt bound is exhausted"
  (let (specs args)
    (dotimes (index 33)
      (let ((name (intern (format "field%d" index))))
        (push (list name 'string :optional "Field") specs)
        (setq args (append args (list (intern (format ":%s" name)) nil)))))
    (let ((tool
           (niwa-tool--create :name "TooManyRepairs"
                                 :args (nreverse specs))))
      (should-error (niwa-tool-repair-attempt tool args) :type 'error)))

  :doc "unwraps exact HTTP and HTTPS auto-links in the final path component"
  (let ((tool
         (niwa-tool--create
          :name "Read"
          :args '((file_path path :required "Path")))))
    (dolist (case '(("[notes.md](http://notes.md)" . "notes.md")
                    ("/tmp/[notes.md](https://notes.md)" . "/tmp/notes.md")))
      (let ((outcome
             (niwa-tool-repair-attempt
              tool (list :file_path (car case)))))
        (should (eq 'repaired (plist-get outcome :status)))
        (should (equal (cdr case)
                       (plist-get (plist-get outcome :args) :file_path)))
        (should
         (eq 'unwrap-path-autolink
             (plist-get (car (plist-get outcome :repairs)) :rule))))))

  :doc "leaves non-degenerate Markdown and near misses unchanged"
  (let ((tool
         (niwa-tool--create
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
             (niwa-tool-repair-attempt tool (list :file_path path))))
        (should (eq 'valid (plist-get outcome :status)))
        (should (equal path
                       (plist-get (plist-get outcome :args) :file_path))))))

  :doc "never applies path repair to an ordinary string contract"
  (let* ((tool
          (niwa-tool--create
           :name "Wrapped"
           :args '((file_path string :required "Path-like string"))))
         (value "[notes.md](https://notes.md)")
         (outcome
          (niwa-tool-repair-attempt tool (list :file_path value))))
    (should (eq 'valid (plist-get outcome :status)))
    (should (equal value
                   (plist-get (plist-get outcome :args) :file_path)))))

(niwa-deftest niwa-tool-repair-normalize-audit-record
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
         (normalized (niwa-tool-repair-normalize-audit-record record)))
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
    (should-not (niwa-tool-repair-normalize-audit-record record)))
  (let ((cyclic-path (list 'names)))
    (setcdr cyclic-path cyclic-path)
    (should-not
     (niwa-tool-repair-normalize-audit-record
      (list :type 'tool-input-repair :state 'committed
            :repairs
            (list (list :rule 'rule :source 'generic
                        :paths (list cyclic-path)
                        :before 'string :after 'array)))))))

(niwa-deftest niwa-tool-repair-audit-record
  ()
  ,test
  (test)

  :doc "builds the normalized committed or abandoned audit envelope"
  (dolist (state '(committed abandoned))
    (let ((record
           (niwa-tool-repair-audit-record
            state
            '((:rule parse-json-value :source generic
                    :paths ((count)) :before string :after integer)))))
      (should (eq state (plist-get record :state)))
      (should (eq 'tool-input-repair (plist-get record :type))))))

(niwa-deftest niwa-tool-repair-format-audit-block
  ()
  ,test
  (test)

  :doc "formats a producer-owned hidden side channel without values"
  (let* ((block
          (niwa-tool-repair-format-audit-block
           'committed
           '((:rule wrap-array-singleton :source generic
                   :paths ((names)) :before string :after array
                   :summary "sentinel-secret"))))
         record)
    (should (string-match-p niwa--hook-audit-open block))
    (should (eq 'niwa-hook-audit (get-text-property 0 'gptel block)))
    (should (eq t (get-text-property 0 'niwa-hook-audit block)))
    (with-temp-buffer
      (insert block)
      (goto-char (point-min))
      (search-forward niwa--hook-audit-open)
      (let ((start (point)))
        (search-forward niwa--hook-audit-close)
        (setq record
              (niwa--read-hook-audit-record
               (buffer-substring-no-properties start (match-beginning 0))))))
    (should (eq 'committed (plist-get record :state)))
    (should-not (string-match-p "sentinel" (prin1-to-string record))))

  :doc "malformed records fail open to an empty block"
  (should
   (equal ""
          (niwa-tool-repair-format-audit-block
           'committed '((:rule "bad" :paths ((names))))))))

(niwa-deftest niwa-tool-repair-format-path
  ()
  ,test
  (test)

  :doc "formats nested object and array schema paths"
  (should (equal "config.items[2].name"
                 (niwa-tool-repair-format-path
                  '(config items 2 name)))))


;;
;;; Raw gptel adapter and dispatch ledger

(niwa-deftest niwa-tool-repair-pre-tool-call
  (:before-each (niwa-tool-clear-registry)
   :after-each (niwa-tool-clear-registry))
  ,test
  (test)

  :doc "returns repaired args and records the call for pipeline dispatch"
  (let ((tool
         (niwa-tool--create
          :name "Collect"
          :category "niwa"
          :args '((names array :required "Names" :items (:type string))))))
    (niwa-tool-register tool)
    (with-temp-buffer
      (let ((result
             (niwa-tool-repair-pre-tool-call
              '(:name "Collect" :args (:names "alice")
                :backend test-backend :model test-model))))
        (should (equal '(:names ["alice"]) (plist-get result :args)))
        (let ((entry
               (niwa-tool-repair-consume-ledger-entry
                tool '(:names ["alice"]))))
          (should (eq 'repaired (plist-get entry :status)))
          (should (eq 'wrap-array-singleton
                      (plist-get (car (plist-get entry :repairs)) :rule)))))))

  :doc "records one settled telemetry event only for interesting outcomes"
  (let ((tool
         (niwa-tool--create
          :name "Collect"
          :category "niwa"
          :args '((names array :required "Names" :items (:type string)))))
        (session (niwa-session--create :name "main"))
        captured)
    (niwa-tool-register tool)
    (cl-letf (((symbol-function 'niwa-telemetry-detailed-p)
               (lambda (_session) t))
              ((symbol-function 'niwa-telemetry-record)
               (lambda (_session event &rest props)
                 (push (cons event props) captured))))
      (with-temp-buffer
        (setq-local niwa--session session)
        (niwa-tool-repair-pre-tool-call
         '(:name "Collect" :args (:names ["alice"])))
        ;; Valid no-op input: nothing recorded even in a detailed run.
        (should-not captured)
        (niwa-tool-repair-pre-tool-call
         '(:name "Collect" :args (:names "alice")))
        (should (= 1 (length captured)))
        (let ((props (cdr (car captured))))
          (should (eq 'tool-input-validation-repair (car (car captured))))
          (should (eq 'settled (plist-get props :stage)))
          (should (eq 'repaired (plist-get props :outcome)))
          (should (= 1 (plist-get props :repair-count)))
          (should (numberp (plist-get props :duration-ms)))))))

  :doc "settles invalid input with a direct leading Error result"
  (let ((tool
         (niwa-tool--create
          :name "Count"
          :category "niwa"
          :args '((count integer :required "Count")))))
    (niwa-tool-register tool)
    (with-temp-buffer
      (let ((result
             (niwa-tool-repair-pre-tool-call
              '(:name "Count" :args (:count "twelve")))))
        (should (string-prefix-p "Error:" (plist-get result :result)))
        (should-not (plist-member result :block)))))

  :doc "marks a partially repaired invalid call as abandoned and auditable"
  (let* ((session (niwa-session--create :name "main"))
         (tool
          (niwa-tool--create
           :name "CollectCount" :category "niwa"
           :args '((names array :required "Names" :items (:type string))
                   (count integer :required "Count"))))
         (info '(:name "CollectCount"
                       :args (:names "alice" :count "not-a-number"))))
    (niwa-tool-register tool)
    (with-temp-buffer
      (setq-local niwa--session session)
      (let* ((adapted (niwa-tool-repair-pre-tool-call info))
             (result (plist-get adapted :result)))
        (should (string-prefix-p "Error:" result))
        (should (string-match-p niwa--hook-audit-open result))
        (niwa-tool-repair-post-tool-call
         (append info (list :result result)))))
    (let ((event (car (niwa-session-repair-log session))))
      (should (eq 'abandoned (plist-get event :outcome)))
      (should (equal '(wrap-array-singleton) (plist-get event :rules)))
      (should (equal '((names) (count)) (plist-get event :paths)))))

  :doc "omits validation events for normal sessions"
  (let ((session (niwa-session--create :name "main"))
        (tool (niwa-tool--create
               :name "NormalValidation" :category "niwa"
               :args '((name string :required "Name"))))
        recorded)
    (niwa-tool-register tool)
    (with-temp-buffer
      (setq-local niwa--session session)
      (cl-letf (((symbol-function 'niwa-telemetry-record)
                 (lambda (&rest _) (setq recorded t))))
        (niwa-tool-repair-pre-tool-call
         '(:name "NormalValidation" :args (:name "ok")))))
    (should-not recorded))

  :doc "consumes identical final args in original call order"
  (let ((tool
         (niwa-tool--create
          :name "Collect"
          :category "niwa"
          :args '((note string :optional "Note")
                  (names array :required "Names" :items (:type string))))))
    (niwa-tool-register tool)
    (with-temp-buffer
      (niwa-tool-repair-pre-tool-call
       '(:name "Collect" :args (:note nil :names "alice")))
      (niwa-tool-repair-pre-tool-call
       '(:name "Collect" :args (:names ["alice"])))
      (let ((first
             (niwa-tool-repair-consume-ledger-entry
              tool '(:note nil :names ["alice"])))
            (second
             (niwa-tool-repair-consume-ledger-entry
              tool '(:note nil :names ["alice"]))))
        (should (eq 'repaired (plist-get first :status)))
        (should (= 2 (length (plist-get first :repairs))))
        (should (eq 'valid (plist-get second :status)))
        (should-not (plist-get second :repairs)))))

  :doc "turn cleanup prevents stale entries from matching a later call"
  (let ((tool
         (niwa-tool--create
          :name "Collect"
          :category "niwa"
          :args '((names array :required "Names" :items (:type string))))))
    (niwa-tool-register tool)
    (with-temp-buffer
      (niwa-tool-repair-pre-tool-call
       '(:name "Collect" :args (:names "alice")))
      (niwa-tool-repair-clear-ledger)
      (should-not
       (niwa-tool-repair-consume-ledger-entry
        tool '(:names ["alice"]))))))


;;
;;; Repair telemetry

(niwa-deftest niwa-tool-repair--persist-content
  (:doc "creates missing parents once and appends UTF-8 independent of the caller")
  (let* ((root (make-temp-file "niwa-repair-append-" t))
         (session (niwa-session--create
                   :save-path (file-name-concat root "nested" "session")))
         (niwa-tool-repair-persist-log t)
         (content "(:model \"\u2603\")\n"))
    (unwind-protect
        (let ((coding-system-for-write 'utf-16le))
          (should (niwa-tool-repair--persist-content session content))
          (cl-letf (((symbol-function 'make-directory)
                     (lambda (&rest _) (ert-fail "Recreated existing directory"))))
            (should (niwa-tool-repair--persist-content session content)))
          (with-temp-buffer
            (set-buffer-multibyte nil)
            (insert-file-contents-literally (niwa-tool-repair-log-path session))
            (should (equal (encode-coding-string (concat content content) 'utf-8-unix)
                           (buffer-string)))))
      (delete-directory root t))))

(niwa-deftest niwa-tool-repair-log-event
  ()
  ,test
  (test)

  :doc "bounds memory and appends events after a save path is present"
  (let* ((dir (file-name-as-directory
               (make-temp-file "niwa-repair-log-" t)))
         (session (niwa-session--create :name "main"))
         (niwa-tool-repair-log-limit 2)
         (event '(:time "now" :origin "/root" :backend backend
                        :model model :tool "Read" :outcome invalid
                        :repair-enabled t :rules nil :paths nil
                        :issue-kinds nil :execution executed :result success)))
    (unwind-protect
        (progn
          (niwa-tool-repair-log-event session event)
          (setf (niwa-session-save-path session) dir)
          (niwa-tool-repair-log-event
           session (plist-put (copy-sequence event) :outcome 'repaired))
          (niwa-tool-repair-log-event
           session (plist-put (copy-sequence event) :outcome 'invalid))
          (should (= 2 (length (niwa-session-repair-log session))))
          (niwa-tool-repair-flush-log session)
          (with-temp-buffer
            (insert-file-contents
             (niwa-tool-repair-log-path session))
            (goto-char (point-min))
            (should (= 3 (count-lines (point-min) (point-max))))))
      (delete-directory dir t)))

  :doc "forwards value-free repair outcomes into the unified telemetry stream"
  (let ((session (niwa-session--create :name "main"))
        captured)
    (cl-letf (((symbol-function 'niwa-telemetry-record)
               (lambda (_session event &rest props)
                 (setq captured (cons event props)))))
      (niwa-tool-repair-log-event
       session
       '(:time "now" :origin "/root" :backend backend
               :model model :tool "Read" :outcome repaired
               :repair-enabled t :rules (array-to-list) :paths ((names))
               :issue-kinds (wrong-shape) :execution executed
               :result success)))
    (should (eq 'tool-input-repair (car captured)))
    (should (eq 'settled (plist-get (cdr captured) :stage)))
    (should (= 1 (plist-get (cdr captured) :repair-count)))
    (should (= 1 (plist-get (cdr captured) :issue-count))))

  :doc "side repair outcomes use the durable audit target"
  (let* ((parent (niwa-session--create :name "parent"))
         (side (niwa-session--create
                :name "side" :audit-session parent))
         captured)
    (cl-letf (((symbol-function 'niwa-telemetry-record)
               (lambda (session event &rest props)
                 (setq captured (list session event props)))))
      (niwa-tool-repair-log-event
       side
       '(:time "now" :origin "/root" :backend backend
               :model model :tool "Read" :outcome repaired
               :repair-enabled t :rules (array-to-list) :paths ((names))
               :issue-kinds (wrong-shape) :execution executed
               :result success)))
    (should (eq parent (car captured)))
    (should (eq 'tool-input-repair (cadr captured)))
    (should (eq 'btw
                (plist-get (nth 2 captured) :conversation-scope))))

  :doc "skips unchanged valid inputs before redaction, queueing, and persistence"
  (let* ((root (make-temp-file "niwa-noop-repair-" t))
         (session (niwa-session--create :name "main"))
         (event '(:time "now" :origin "/root" :backend backend
                        :model model :tool "Read" :outcome valid
                        :repair-enabled t :rules nil :paths nil
                        :issue-kinds nil :execution executed :result success))
         (niwa-tool-repair-persist-log t)
         (original-safe-event (symbol-function 'niwa-tool-repair--safe-event))
         (original-audit (symbol-function 'niwa-telemetry-record-audit))
         (redactions 0)
         (audits 0))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'niwa-tool-repair--safe-event)
                     (lambda (&rest args)
                       (cl-incf redactions)
                       (apply original-safe-event args)))
                    ((symbol-function 'niwa-telemetry-record-audit)
                     (lambda (&rest args)
                       (cl-incf audits)
                       (apply original-audit args))))
            (dolist (path (list nil root))
              (setf (niwa-session-save-path session) path)
              (niwa-tool-repair-log-event session event)))
          (should (= 0 redactions))
          (should (= 0 audits))
          (should-not (niwa-session-repair-log session))
          (should-not (niwa-session-repair-log-pending session))
          (should-not (file-exists-p
                       (niwa-tool-repair-log-path session))))
      (delete-directory root t)))

  :doc "logging failures never escape into validated execution"
  (let* ((dir (file-name-as-directory
               (make-temp-file "niwa-repair-log-fail-" t)))
         (session (niwa-session--create :name "main" :save-path dir))
         (niwa-telemetry-enabled nil)
         warned)
    (unwind-protect
        (cl-letf (((symbol-function 'write-region)
                   (lambda (&rest _) (error "private persistence path")))
                  ((symbol-function 'display-warning)
                   (lambda (_type message &rest _)
                     (setq warned message))))
          (should
           (niwa-tool-repair-log-event
            session
            '(:time "now" :origin "/root" :backend backend
                    :model model :tool "Read" :outcome repaired
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
         (session (niwa-session--create :name "main"))
         (base '(:time "now" :origin "/root" :model model :tool "Read"
                       :outcome repaired :repair-enabled t :rules nil :paths nil
                       :issue-kinds nil :execution executed :result success)))
    (niwa-tool-repair-log-event
     session (plist-put (copy-sequence base) :backend first))
    (niwa-tool-repair-log-event
     session (plist-put (copy-sequence base) :backend second))
    (should (equal '("DeepSeek" "OpenRouter")
                   (mapcar (lambda (event) (plist-get event :backend))
                           (niwa-session-repair-log session)))))

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
    (let* ((session (niwa-session--create :name "main"))
           (event
            (list :time "now" :origin "/root" :backend 'backend :model 'model
                  :tool "Read" :outcome "sentinel-outcome"
                  :repair-enabled t :rules rules :paths paths
                  :issue-kinds issue-kinds :execution "sentinel-execution"
                  :result "sentinel-result"
                  :failure-class 'sentinel-failure)))
      (niwa-tool-repair-log-event session event)
      (let* ((safe (car (niwa-session-repair-log session)))
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
        (should-not (string-match-p "sentinel" printed)))))

  :doc "defers a materialized remote append until session settlement"
  (let* ((root (make-temp-file "niwa-repair-log-remote-" t))
         (target
          (niwa-execution-target-create "/ssh:repair-host:/workspace/"))
         (session
          (niwa-session--create
           :name "main" :execution-target target :save-path root))
         (event '(:time "now" :origin "/root" :backend backend
                        :model model :tool "Read" :outcome repaired
                        :repair-enabled t :rules (array-to-list)
                        :paths ((names)) :issue-kinds (wrong-shape)
                        :execution executed :result success))
         (later-event
          '(:time "later" :origin "/root" :backend backend
                  :model model :tool "Read" :outcome invalid
                  :repair-enabled t :rules nil :paths ((names))
                  :issue-kinds (wrong-shape)
                  :execution not-executed :result none))
         (niwa-tool-repair-persist-log t)
         calls)
    (unwind-protect
        (progn
          (cl-letf
              (((symbol-function
                 'niwa-session-publication-append-diagnostic)
                (lambda (_session path content)
                  (push (list path content) calls)
                  t))
               ((symbol-function 'niwa-telemetry-record-audit) #'ignore))
            (niwa-tool-repair-log-event session event)
            (niwa-tool-repair-log-event session later-event)
            (should (= 2 (length
                          (niwa-session-repair-log-pending session))))
            (should-not calls)
            (should-not
             (file-exists-p (niwa-tool-repair-log-path session)))
            (niwa-tool-repair-flush-log session))
          (should-not (niwa-session-repair-log-pending session))
          (pcase-let ((`((,path ,content)) calls))
            (should (equal (niwa-tool-repair-log-path session) path))
            (with-temp-buffer
              (insert content)
              (goto-char (point-min))
              (should (eq 'repaired
                          (plist-get (read (current-buffer)) :outcome)))
              (should (eq 'invalid
                          (plist-get (read (current-buffer)) :outcome))))))
      (delete-directory root t))))

(niwa-deftest niwa-tool-repair-flush-log
  (:quiet t :doc "retains a failed append and persists it on a later flush")
  (let* ((root (make-temp-file "niwa-repair-log-retry-" t))
         (blocked (file-name-concat root "blocked"))
         (restored (file-name-concat root "restored"))
         (session (niwa-session--create :name "main"))
         (event '(:time "now" :origin "/root" :backend backend
                        :model model :tool "Read" :outcome repaired
                        :repair-enabled t :rules (array-to-list)
                        :paths ((names)) :issue-kinds (wrong-shape)
                        :execution executed :result success))
         (later-event '(:time "later" :origin "/root" :backend backend
                              :model model :tool "Read" :outcome invalid
                              :repair-enabled t :rules nil :paths ((names))
                              :issue-kinds (wrong-shape)
                              :execution not-executed :result none))
         (niwa-tool-repair-persist-log t))
    (unwind-protect
        (progn
          (write-region "not a directory" nil blocked nil 'silent)
          (setf (niwa-session-save-path session) blocked)
          (niwa-tool-repair-log-event session event)
          (should (= 1 (length
                        (niwa-session-repair-log-pending session))))
          (setf (niwa-session-save-path session) restored)
          (niwa-tool-repair-log-event session later-event)
          (should (= 2 (length
                        (niwa-session-repair-log-pending session))))
          (niwa-tool-repair-flush-log session)
          (should-not (niwa-session-repair-log-pending session))
          (with-temp-buffer
            (insert-file-contents (niwa-tool-repair-log-path session))
            (let ((first (read (current-buffer)))
                  (second (read (current-buffer))))
              (should (eq 'repaired (plist-get first :outcome)))
              (should (equal '(array-to-list)
                             (plist-get first :rules)))
              (should (eq 'invalid (plist-get second :outcome))))))
      (delete-directory root t))))

(niwa-deftest niwa-tool-repair-record-result
  ()
  ,test
  (test)

  :doc "records redacted child metadata and result classification exactly once"
  (let* ((session (niwa-session--create :name "main"))
         (entry
          (list :tool "Write" :status 'repaired
                :args '(:file_path "/private/sentinel-path")
                :repairs
                '((:rule unwrap-path-autolink :source generic
                         :paths ((file_path)) :before markdown-link :after path
                         :summary "Unwrapped a secret path."))
                :issues nil :backend 'child-backend :model 'child-model
                :session session :origin "/root/explorer"
                :repair-enabled t :execution 'executed
                :telemetry-recorded nil)))
    (niwa-tool-repair-record-result
     entry "Error: sentinel-result /private/sentinel-path")
    (niwa-tool-repair-record-result entry "second result")
    (let* ((events (niwa-session-repair-log session))
           (event (car events))
           (printed (prin1-to-string event)))
      (should (= 1 (length events)))
      (should (equal "/root/explorer" (plist-get event :origin)))
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

  :doc "settles a valid call without constructing diagnostics and releases tracking"
  (let ((session (niwa-session--create :name "main"))
        (original-event (symbol-function 'niwa-tool-repair--event))
        (constructed 0))
    (with-temp-buffer
      (let ((entry (list :tool "Read" :status 'valid :repairs nil :issues nil
                         :session session :buffer (current-buffer)
                         :execution 'executed :telemetry-recorded nil)))
        (setq-local niwa-tool-repair--in-flight (list entry))
        (cl-letf (((symbol-function 'niwa-tool-repair--event)
                   (lambda (&rest args)
                     (cl-incf constructed)
                     (apply original-event args))))
          (niwa-tool-repair-record-result entry "ok")
          (niwa-tool-repair-record-result entry "duplicate"))
        (should (= 0 constructed))
        (should (plist-get entry :telemetry-recorded))
        (should-not niwa-tool-repair--in-flight)
        (should-not (niwa-session-repair-log session)))))

  :doc "records disabled repair and invalid issue kinds without values"
  (let* ((session (niwa-session--create :name "main"))
         (secret-path (intern "file\nsentinel-secret"))
         (entry
          (list :tool "Read" :status 'invalid :repairs nil
                :issues
                (list (list :path (list secret-path) :kind 'wrong-type
                            :expected 'path :actual 'string
                            :schema '(:type path)))
                :backend 'backend :model 'model :session session
                :origin "/root" :repair-enabled nil
                :execution 'not-executed :telemetry-recorded nil)))
    (niwa-tool-repair-record-result entry "Error: private value")
    (let ((event (car (niwa-session-repair-log session))))
      (should (plist-member event :repair-enabled))
      (should-not (plist-get event :repair-enabled))
      (should (eq 'invalid (plist-get event :outcome)))
      (should (equal '(wrong-type) (plist-get event :issue-kinds)))
      (should (eq 'not-executed (plist-get event :execution)))
      (should-not
       (string-match-p "sentinel-secret" (prin1-to-string event))))))

(niwa-deftest niwa-tool-repair-post-tool-call
  (:before-each (niwa-tool-clear-registry)
   :after-each (niwa-tool-clear-registry))
  ,test
  (test)

  :doc "does not settle a later batched call before it has a result"
  (let ((tool
         (niwa-tool--create
          :name "Count" :category "niwa"
          :args '((count integer :required "Count")))))
    (niwa-tool-register tool)
    (with-temp-buffer
      (niwa-tool-repair-pre-tool-call
       '(:name "Count" :args (:count 1)
         :backend backend :model model))
      (niwa-tool-repair-post-tool-call
       '(:name "Count" :args (:count 1) :result nil))
      (should
       (niwa-tool-repair-consume-ledger-entry tool '(:count 1)))))

  :doc "settles invalid raw calls into the parent session telemetry"
  (let* ((dir (file-name-as-directory
               (make-temp-file "niwa-child-repair-log-" t)))
         (session (niwa-session--create :name "main" :save-path dir))
         (invocation
         (niwa-agent-invocation--create
           :agent-id "reviewer--stable" :path "/root/reviewer"
           :parent-session session))
         (tool
          (niwa-tool--create
           :name "Count" :category "niwa"
           :args '((count integer :required "Count")))))
    (unwind-protect
        (progn
          (niwa-tool-register tool)
          (with-temp-buffer
            (setq-local niwa--session session)
            (setq-local niwa--agent-invocation invocation)
            (let* ((info '(:name "Count"
                                  :args (:count "sentinel-secret")
                                  :backend child-backend :model child-model))
                   (adapted (niwa-tool-repair-pre-tool-call info)))
              (niwa-tool-repair-post-tool-call
               (append info (list :result (plist-get adapted :result))))))
          (let ((event (car (niwa-session-repair-log session))))
            (should (eq 'invalid (plist-get event :outcome)))
            (should (equal "/root/reviewer" (plist-get event :origin)))
            (should (eq 'child-backend (plist-get event :backend)))
            (should (eq 'child-model (plist-get event :model)))
            (should-not
             (string-match-p "sentinel-secret" (prin1-to-string event))))
          (with-temp-buffer
            (insert-file-contents (niwa-tool-repair-log-path session))
            (should (string-match-p "/root/reviewer" (buffer-string)))
            (should-not (string-match-p "sentinel-secret" (buffer-string)))))
      (delete-directory dir t))))

(niwa-deftest niwa-tool-repair-clear-ledger
  (:before-each (niwa-tool-clear-registry)
   :after-each (niwa-tool-clear-registry))
  ,test
  (test)

  :doc "records unconsumed calls as abandoned and clears only this buffer"
  (let* ((session (niwa-session--create :name "main"))
         (tool
          (niwa-tool--create
           :name "Collect" :category "niwa"
           :args '((names array :required "Names" :items (:type string)))))
         other-buffer)
    (niwa-tool-register tool)
    (unwind-protect
        (with-temp-buffer
          (setq-local niwa--session session)
          (niwa-tool-repair-pre-tool-call
           '(:name "Collect" :args (:names "alice")
             :backend backend :model model))
          (should
           (niwa-tool-repair-consume-ledger-entry
            tool '(:names ["alice"])))
          (should-not niwa-tool-repair--ledger)
          (should (= 1 (length niwa-tool-repair--in-flight)))
          (setq other-buffer (generate-new-buffer " *repair-other*"))
          (with-current-buffer other-buffer
            (setq-local niwa-tool-repair--ledger '(:other)))
          (niwa-tool-repair-clear-ledger)
          (should-not niwa-tool-repair--ledger)
          (should-not niwa-tool-repair--in-flight)
          (with-current-buffer other-buffer
            (should niwa-tool-repair--ledger)))
      (when (buffer-live-p other-buffer) (kill-buffer other-buffer)))
    (let ((event (car (niwa-session-repair-log session))))
      (should (eq 'abandoned (plist-get event :outcome)))
      (should (eq 'not-executed (plist-get event :execution)))
      (should (eq 'none (plist-get event :result))))))

(provide 'test-niwa-tool-repair)
;;; test-niwa-tool-repair.el ends here
