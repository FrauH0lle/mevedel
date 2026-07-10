;;; mevedel-tool-repair.el --- Tool input validation and repair -*- lexical-binding: t -*-

;;; Commentary:

;; Owns structured tool-input validation, atomic schema-directed repair,
;; model-facing value-free feedback, raw gptel hook adaptation, dispatch
;; tracking, and redacted per-session repair telemetry.

;;; Code:

(eval-when-compile
  (require 'mevedel-structs))

;; `gptel-request'
(declare-function gptel-backend-name "ext:gptel-request" (cl-x) t)
(declare-function gptel-backend-p "ext:gptel-request" (cl-x))
(declare-function gptel-fsm-info "ext:gptel-request" (cl-x) t)

;; `mevedel-agents'
(declare-function mevedel-agent-invocation-agent-id
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-p "mevedel-agents" (cl-x))
(declare-function mevedel-agent-invocation-parent-session
                  "mevedel-agents" (cl-x) t)

;; `mevedel-tool-registry'
(declare-function mevedel-tool-args "mevedel-tool-registry" (cl-x) t)
(declare-function mevedel-tool-get "mevedel-tool-registry"
                  (name &optional category))
(declare-function mevedel-tool-name "mevedel-tool-registry" (cl-x) t)
(declare-function mevedel-tool-repair-input "mevedel-tool-registry" (cl-x) t)

;; `mevedel-utilities'
(declare-function mevedel--format-hook-audit-record
                  "mevedel-utilities" (record))

(defvar mevedel--agent-invocation)
(defvar mevedel--session)

(defcustom mevedel-tool-input-repair-enabled t
  "When non-nil, repair recoverable model-produced tool input."
  :type 'boolean
  :group 'mevedel)

(defcustom mevedel-tool-repair-log-limit 200
  "Maximum repair telemetry events retained on a session."
  :type 'integer
  :group 'mevedel)

(defcustom mevedel-tool-repair-persist-log t
  "When non-nil, append repair telemetry to materialized sessions."
  :type 'boolean
  :group 'mevedel)

(defconst mevedel-tool-repair--max-rendered-issues 8
  "Maximum validation issues included in model-facing feedback.")

(defconst mevedel-tool-repair--max-ledger-entries 256
  "Maximum pending raw tool-call records retained in one buffer.")

(defconst mevedel-tool-repair--max-telemetry-items 16
  "Maximum entries retained in one telemetry collection.")

(defconst mevedel-tool-repair--max-telemetry-path-depth 16
  "Maximum schema path components retained in one telemetry event.")

(defconst mevedel-tool-repair--max-audit-records 32
  "Maximum repair records retained in one view audit block.")

(defconst mevedel-tool-repair--shape-identifiers
  '(absent array boolean defaulted empty integer markdown-link missing null
    number object path placeholder present singleton string symbol unknown)
  "Value-free vocabulary permitted in repair before/after descriptors.")

(defvar-local mevedel-tool-repair--ledger nil
  "Ordered raw tool calls awaiting pipeline dispatch in this buffer.")

(defvar-local mevedel-tool-repair--in-flight nil
  "Ordered pipeline calls awaiting completion in this buffer.")

(defvar mevedel-tool-repair--parsing-response nil
  "Non-nil while gptel is decoding a provider response.")

(defvar mevedel-tool-repair--raw-input-p nil
  "Non-nil while validating undecoded model-produced argument presence.")

(autoload 'mevedel-tool-repair--current-session
  "mevedel-tool-repair-diagnostics")
(autoload 'mevedel-tool-repair--current-origin
  "mevedel-tool-repair-diagnostics")
(autoload 'mevedel-tool-repair-record-result
  "mevedel-tool-repair-diagnostics")
(autoload 'mevedel-tool-repair-mark-executed
  "mevedel-tool-repair-diagnostics")
(autoload 'mevedel-tool-repair-format-audit-block
  "mevedel-tool-repair-diagnostics")
(autoload 'mevedel-tool-repair-audit-record
  "mevedel-tool-repair-diagnostics")
(autoload 'mevedel-tool-repair-normalize-audit-record
  "mevedel-tool-repair-diagnostics")
(autoload 'mevedel-tool-repair-log-event
  "mevedel-tool-repair-diagnostics")
(autoload 'mevedel-tool-repair-log-path
  "mevedel-tool-repair-diagnostics")
(autoload 'mevedel-tool-repair--persist-event
  "mevedel-tool-repair-diagnostics")
(autoload 'mevedel-tool-repair--preserve-empty-objects
  "mevedel-tool-repair-gptel")
(autoload 'mevedel-tool-repair--with-lossless-json
  "mevedel-tool-repair-gptel")
(autoload 'mevedel-tool-repair--json-parse-string
  "mevedel-tool-repair-gptel")
(autoload 'mevedel-tool-repair--restore-argument-shapes
  "mevedel-tool-repair-gptel")
(autoload 'mevedel-tool-repair-install-shape-adapter
  "mevedel-tool-repair-gptel")
(autoload 'mevedel-tool-repair-uninstall-shape-adapter
  "mevedel-tool-repair-gptel")


;;
;;; Structured validation

(defun mevedel-tool-repair--name (value)
  "Return VALUE as an unqualified symbol."
  (cond
   ((keywordp value) (intern (substring (symbol-name value) 1)))
   ((symbolp value) value)
   ((stringp value) (intern value))))

(defun mevedel-tool-repair--key (name)
  "Return the plist keyword for NAME."
  (intern (concat ":" (symbol-name (mevedel-tool-repair--name name)))))

(defun mevedel-tool-repair--null-p (value)
  "Return non-nil for supported decoded JSON null representations."
  (or (null value) (eq value :null)))

(defun mevedel-tool-repair--actual-type (value)
  "Return a value-free type name describing VALUE."
  (cond
   ((mevedel-tool-repair--null-p value) 'null)
   ((or (eq value t) (eq value :json-false)) 'boolean)
   ((stringp value) 'string)
   ((integerp value) 'integer)
   ((numberp value) 'number)
   ((vectorp value) 'array)
   ((hash-table-p value) 'object)
   ((listp value) 'object)
   ((symbolp value) 'symbol)
   (t 'unknown)))

(defun mevedel-tool-repair--type-p (type value)
  "Return non-nil when VALUE satisfies schema TYPE."
  (pcase type
    ((or 'string 'path) (stringp value))
    ('integer (integerp value))
    ('number (numberp value))
    ('boolean (or (eq value t) (eq value :json-false)))
    ('array (vectorp value))
    ('object (or (listp value)
                 (and (hash-table-p value)
                      (= 0 (hash-table-count value)))))
    (_ (error "Unsupported tool schema type: %S" type))))

(defun mevedel-tool-repair--issue (path kind expected actual &optional schema)
  "Return a structured validation issue."
  (list :path path :kind kind :expected expected :actual actual
        :schema (copy-tree schema t)))

(defun mevedel-tool-repair--required-names (schema)
  "Return normalized required property names from SCHEMA."
  (mapcar #'mevedel-tool-repair--name
          (append (plist-get schema :required) nil)))

(defun mevedel-tool-repair--assert-schema-supported (schema)
  "Signal when SCHEMA contains an unsupported declared type."
  (let ((type (plist-get schema :type)))
    (mevedel-tool-repair--type-p type nil)
    (pcase type
      ('array
       (when-let* ((items (plist-get schema :items)))
         (mevedel-tool-repair--assert-schema-supported items)))
      ('object
       (let ((properties (plist-get schema :properties)))
         (while properties
           (pop properties)
           (mevedel-tool-repair--assert-schema-supported
            (pop properties))))))))

(defun mevedel-tool-repair--validate-properties (schema value path)
  "Validate declared object properties in VALUE at PATH against SCHEMA."
  (let ((properties (plist-get schema :properties))
        (required (mevedel-tool-repair--required-names schema))
        issues)
    (while properties
      (let* ((name (mevedel-tool-repair--name (pop properties)))
             (property-schema
              (append (and (not (memq name required))
                           (list :mevedel-optional t))
                      (pop properties)))
             (key (mevedel-tool-repair--key name))
             (present (and (listp value) (plist-member value key)))
             (property-value (and present (plist-get value key)))
             (property-path (append path (list name))))
        (cond
         ((and (memq name required)
               (mevedel-tool-repair--null-p property-value))
          (setq issues
                (append issues
                        (list
                         (mevedel-tool-repair--issue
                          property-path 'missing-required
                          (plist-get property-schema :type) 'missing
                          property-schema)))))
         ((and mevedel-tool-repair--raw-input-p present
               (mevedel-tool-repair--null-p property-value))
          (setq issues
                (append issues
                        (list
                         (mevedel-tool-repair--issue
                          property-path 'optional-null
                          (plist-get property-schema :type) 'null
                          property-schema)))))
         ((and present property-value)
          (setq issues
                (append issues
                        (mevedel-tool-repair--validate-schema
                         property-schema property-value property-path)))))))
    issues))

(defun mevedel-tool-repair--validate-array (schema value path)
  "Validate array VALUE at PATH against SCHEMA constraints."
  (let ((length (length value))
        (minimum (plist-get schema :minItems))
        (maximum (plist-get schema :maxItems))
        (item-schema (plist-get schema :items))
        issues)
    (when (and minimum (< length minimum))
      (push (mevedel-tool-repair--issue
             path 'too-few-items minimum length schema)
            issues))
    (when (and maximum (> length maximum))
      (push (mevedel-tool-repair--issue
             path 'too-many-items maximum length schema)
            issues))
    (setq issues (nreverse issues))
    (when item-schema
      (dotimes (index length)
        (setq issues
              (append issues
                      (mevedel-tool-repair--validate-schema
                       item-schema (aref value index)
                       (append path (list index)))))))
    issues))

(defun mevedel-tool-repair--validate-schema (schema value path)
  "Validate VALUE at PATH against SCHEMA and return ordered issues."
  (let ((type (plist-get schema :type))
        (enum (plist-get schema :enum))
        issues)
    (if (not (mevedel-tool-repair--type-p type value))
        (list (mevedel-tool-repair--issue
               path 'wrong-type type
               (mevedel-tool-repair--actual-type value) schema))
      (when (and enum (not (member value (append enum nil))))
        (push (mevedel-tool-repair--issue
               path 'invalid-enum enum
               (mevedel-tool-repair--actual-type value) schema)
              issues))
      (when (and mevedel-tool-repair--raw-input-p
                 (eq type 'path)
                 (mevedel-tool-repair--unwrap-path-autolink value))
        (push (mevedel-tool-repair--issue
               path 'path-autolink 'path 'string schema)
              issues))
      (setq issues (nreverse issues))
      (cond
       ((eq type 'array)
        (setq issues
              (append issues
                      (mevedel-tool-repair--validate-array
                       schema value path))))
       ((eq type 'object)
        (setq issues
              (append issues
                      (mevedel-tool-repair--validate-properties
                       schema value path)))))
      issues)))

(defun mevedel-tool-repair--unexpected-issues (args known-names)
  "Return issues for top-level ARGS keys absent from KNOWN-NAMES."
  (let ((tail args)
        issues)
    (while (consp tail)
      (let* ((key (pop tail))
             (value (pop tail))
             (name (mevedel-tool-repair--name key)))
        (unless (memq name known-names)
          (push (mevedel-tool-repair--issue
                 (list name) 'unexpected-property 'absent
                 (mevedel-tool-repair--actual-type value) nil)
                issues))))
    (nreverse issues)))

(defun mevedel-tool-repair-validate (tool args)
  "Return ordered validation issues for TOOL ARGS, or nil when valid.

Only declared object properties are validated recursively.  Unexpected
properties are rejected at the top-level tool contract, where mevedel owns
the complete schema.  Optional nil values remain compatible with gptel's
positional dispatch, which represents omitted optional arguments as nil."
  (dolist (spec (mevedel-tool-args tool))
    (mevedel-tool-repair--assert-schema-supported
     (append (list :type (cadr spec)) (nthcdr 4 spec))))
  (if (not (listp args))
      (list (mevedel-tool-repair--issue
             nil 'wrong-type 'object
             (mevedel-tool-repair--actual-type args) '(:type object)))
    (let (issues known-names)
      (dolist (spec (mevedel-tool-args tool))
        (let* ((name (car spec))
               (type (cadr spec))
               (required (eq :required (nth 2 spec)))
               (key (mevedel-tool-repair--key name))
               (present (plist-member args key))
               (value (and present (plist-get args key)))
               (schema (append (list :type type)
                               (and (not required)
                                    (list :mevedel-optional t))
                               (nthcdr 4 spec))))
          (push name known-names)
          (cond
           ((and required (mevedel-tool-repair--null-p value))
            (setq issues
                  (append issues
                          (list
                           (mevedel-tool-repair--issue
                            (list name) 'missing-required type 'missing
                            schema)))))
           ((and mevedel-tool-repair--raw-input-p present
                 (mevedel-tool-repair--null-p value))
            (setq issues
                  (append issues
                          (list
                           (mevedel-tool-repair--issue
                            (list name) 'optional-null type 'null schema)))))
           ((and present value)
            (setq issues
                  (append issues
                          (mevedel-tool-repair--validate-schema
                           schema value (list name))))))))
      (append issues
              (mevedel-tool-repair--unexpected-issues
               args (nreverse known-names))))))


;;
;;; Atomic repair

(defun mevedel-tool-repair--unwrap-path-autolink (path)
  "Return PATH with an exact final-component HTTP(S) auto-link unwrapped."
  (let ((case-fold-search nil))
    (when (and
           (stringp path)
           (string-match
            "\\`\\(\\(?:.*[/\\\\]\\)?\\)\\[\\([^][()/\\\\[:space:]]+\\)\\](https?://\\2)\\'"
            path))
      (concat (match-string 1 path) (match-string 2 path)))))

(defun mevedel-tool-repair--validate-raw (tool args)
  "Return validation issues for raw model-produced TOOL ARGS."
  (let ((mevedel-tool-repair--raw-input-p t))
    (mevedel-tool-repair-validate tool args)))

(defun mevedel-tool-repair--declared-path-p (tool path)
  "Return non-nil when PATH names a declared schema location in TOOL."
  (let* ((spec (assq (car path) (mevedel-tool-args tool)))
         (schema (and spec (append (list :type (cadr spec))
                                   (nthcdr 4 spec)))))
    (dolist (part (cdr path))
      (setq schema
            (cond
             ((and (integerp part) (eq 'array (plist-get schema :type)))
              (plist-get schema :items))
             ((and (symbolp part) (eq 'object (plist-get schema :type)))
              (plist-get (plist-get schema :properties)
                         (mevedel-tool-repair--key part))))))
    (not (null schema))))

(defun mevedel-tool-repair--value-at-path (value path)
  "Return the value below VALUE at PATH."
  (dolist (part path value)
    (setq value
          (if (integerp part)
              (and (vectorp value) (< part (length value)) (aref value part))
            (and (listp value)
                 (plist-get value (mevedel-tool-repair--key part)))))))

(defun mevedel-tool-repair--set-at-path (value path replacement)
  "Return a copy of VALUE with PATH set to REPLACEMENT."
  (let ((part (car path))
        (rest (cdr path)))
    (if (integerp part)
        (let ((copy (copy-sequence value)))
          (aset copy part
                (if rest
                    (mevedel-tool-repair--set-at-path
                     (aref copy part) rest replacement)
                  replacement))
          copy)
      (let* ((key (mevedel-tool-repair--key part))
             (child (and rest (plist-get value key))))
        (plist-put (copy-sequence value) key
                   (if rest
                       (mevedel-tool-repair--set-at-path
                        child rest replacement)
                     replacement))))))

(defun mevedel-tool-repair--plist-delete (plist key)
  "Return PLIST without KEY."
  (let (result)
    (while plist
      (let ((current-key (pop plist))
            (current-value (pop plist)))
        (unless (eq current-key key)
          (push current-key result)
          (push current-value result))))
    (nreverse result)))

(defun mevedel-tool-repair--delete-at-path (value path)
  "Return a copy of VALUE with the property at PATH removed."
  (let ((part (car path))
        (rest (cdr path)))
    (if rest
        (mevedel-tool-repair--set-at-path
         value (list part)
         (mevedel-tool-repair--delete-at-path
          (mevedel-tool-repair--value-at-path value (list part)) rest))
      (mevedel-tool-repair--plist-delete
       value (mevedel-tool-repair--key part)))))

(defun mevedel-tool-repair--record (rule path before after summary)
  "Return one value-free generic repair record."
  (list :rule rule :source 'generic :paths (list path)
        :before before :after after :summary summary))

(defun mevedel-tool-repair--parse-json-value (string)
  "Parse exact JSON STRING, or return a private failure marker."
  (condition-case nil
      (mevedel-tool-repair--preserve-empty-objects
       (json-parse-string string
                          :object-type 'plist
                          :array-type 'array
                          :null-object :null
                          :false-object :json-false))
    (error :mevedel-json-parse-failed)))

(defun mevedel-tool-repair--apply-rule (_tool args issues rule)
  "Apply the first RULE repair supported by ARGS and canonical ISSUES."
  (catch 'applied
    (dolist (issue issues)
      (let* ((path (plist-get issue :path))
             (kind (plist-get issue :kind))
             (expected (plist-get issue :expected))
             (schema (plist-get issue :schema)))
        (cond
         ((and (eq rule 'omit-optional-null)
               (eq kind 'optional-null))
          (throw
           'applied
           (list
            :args (mevedel-tool-repair--delete-at-path args path)
            :record
            (mevedel-tool-repair--record
             rule path 'null 'missing
             (format "Omitted optional null at `%s`."
                     (mevedel-tool-repair-format-path path))))))
         ((and (eq rule 'parse-json-value)
               (eq kind 'wrong-type)
               (memq expected '(integer number boolean array object))
               (stringp (mevedel-tool-repair--value-at-path args path)))
          (let ((parsed
                 (mevedel-tool-repair--parse-json-value
                  (mevedel-tool-repair--value-at-path args path))))
            (when (and (not (eq parsed :mevedel-json-parse-failed))
                       (null (mevedel-tool-repair--validate-schema
                              schema parsed path)))
              (throw
               'applied
               (list
                :args (mevedel-tool-repair--set-at-path args path parsed)
                :record
                (mevedel-tool-repair--record
                 rule path 'string expected
                 (format "Parsed JSON %s at `%s`."
                         expected
                         (mevedel-tool-repair-format-path path))))))))
         ((and (eq rule 'wrap-array-singleton)
               (eq kind 'wrong-type)
               (eq expected 'array)
               (let ((value (mevedel-tool-repair--value-at-path args path)))
                 (and value
                      (not (vectorp value))
                      (not (and (hash-table-p value)
                                (= 0 (hash-table-count value))))
                      (let ((item-schema (plist-get schema :items)))
                        (or (null item-schema)
                            (null (mevedel-tool-repair--validate-schema
                                   item-schema value path)))))))
          (let ((value (mevedel-tool-repair--value-at-path args path)))
            (throw
             'applied
             (list
              :args (mevedel-tool-repair--set-at-path
                     args path (vector value))
              :record
              (mevedel-tool-repair--record
               rule path (mevedel-tool-repair--actual-type value) 'array
               (format "Wrapped the singleton at `%s` as an array."
                       (mevedel-tool-repair-format-path path)))))))
         ((and (eq rule 'empty-array-placeholder)
               (eq kind 'wrong-type)
               (eq expected 'array)
               (plist-get schema :mevedel-optional)
               (or (null (plist-get schema :minItems))
                   (= 0 (plist-get schema :minItems)))
               (hash-table-p (mevedel-tool-repair--value-at-path args path))
               (= 0 (hash-table-count
                     (mevedel-tool-repair--value-at-path args path))))
          (throw
           'applied
           (list
            :args (mevedel-tool-repair--set-at-path args path [])
            :record
            (mevedel-tool-repair--record
             rule path 'object 'array
             (format "Replaced the empty placeholder at `%s` with an array."
                     (mevedel-tool-repair-format-path path))))))
         ((and (eq rule 'unwrap-path-autolink)
               (eq kind 'path-autolink)
               (mevedel-tool-repair--unwrap-path-autolink
                (mevedel-tool-repair--value-at-path args path)))
          (let ((unwrapped
                 (mevedel-tool-repair--unwrap-path-autolink
                  (mevedel-tool-repair--value-at-path args path))))
            (throw
             'applied
             (list
              :args (mevedel-tool-repair--set-at-path args path unwrapped)
              :record
              (mevedel-tool-repair--record
               rule path 'markdown-link 'path
               (format "Unwrapped the filesystem path at `%s`."
                       (mevedel-tool-repair-format-path path))))))))))
    nil))

(defun mevedel-tool-repair--generic-pass (tool args &optional seen remaining)
  "Apply generic repairs to TOOL ARGS using shared SEEN and REMAINING state."
  (let ((candidate args)
        records
        (seen seen)
        (remaining (or remaining 32))
        done)
    (while (not done)
      (let* ((issues (mevedel-tool-repair--validate-raw tool candidate))
             (change
              (and issues
                   (or (mevedel-tool-repair--apply-rule
                        tool candidate issues 'omit-optional-null)
                       (mevedel-tool-repair--apply-rule
                        tool candidate issues 'parse-json-value)
                       (mevedel-tool-repair--apply-rule
                        tool candidate issues 'wrap-array-singleton)
                       (mevedel-tool-repair--apply-rule
                        tool candidate issues 'empty-array-placeholder)
                       (mevedel-tool-repair--apply-rule
                        tool candidate issues 'unwrap-path-autolink)))))
        (if (null change)
            (setq done t)
          (let ((updated (plist-get change :args)))
            (when (or (equal updated candidate)
                      (member updated (cons candidate seen)))
              (error "Tool input repair did not make monotonic progress"))
            (when (= remaining 0)
              (error "Tool input repair exceeded its progress bound"))
            (push candidate seen)
            (setq candidate updated)
            (setq remaining (1- remaining))
            (setq records
                  (append records (list (plist-get change :record))))))))
    (list :args candidate
          :repairs records
          :issues (mevedel-tool-repair--validate-raw tool candidate)
          :seen seen
          :remaining remaining)))

(defun mevedel-tool-repair--plist-keys (plist)
  "Return keys in PLIST."
  (let (keys)
    (while plist
      (push (pop plist) keys)
      (pop plist))
    (nreverse keys)))

(defun mevedel-tool-repair--changed-keys (before after)
  "Return top-level keys whose presence or values differ in BEFORE and AFTER."
  (let (changed)
    (dolist (key (delete-dups
                  (append (mevedel-tool-repair--plist-keys before)
                          (mevedel-tool-repair--plist-keys after))))
      (unless (and (eq (not (null (plist-member before key)))
                       (not (null (plist-member after key))))
                   (equal (plist-get before key) (plist-get after key)))
        (push (mevedel-tool-repair--name key) changed)))
    (nreverse changed)))

(defun mevedel-tool-repair--validate-callback-record (tool record)
  "Validate and normalize one TOOL-owned repair RECORD."
  (let ((rule (plist-get record :rule))
        (paths (plist-get record :paths))
        (before (plist-get record :before))
        (after (plist-get record :after))
        (summary (plist-get record :summary)))
    (unless (and rule (symbolp rule)
                 (listp paths) paths
                 (seq-every-p
                  (lambda (path)
                    (and (listp path)
                         path
                         (symbolp (car path))
                         (car path)
                         (seq-every-p
                         (lambda (part)
                            (or (and (symbolp part) part)
                                (and (integerp part) (>= part 0))))
                          path)
                         (mevedel-tool-repair--declared-path-p tool path)))
                  paths)
                 (memq before mevedel-tool-repair--shape-identifiers)
                 (memq after mevedel-tool-repair--shape-identifiers)
                 (stringp summary)
                 (<= (length summary) 160)
                 (not (string-match-p "[\n\r]" summary)))
      (error "Tool repair callback returned an invalid repair record"))
    (plist-put (copy-sequence record) :source 'tool)))

(defun mevedel-tool-repair--apply-callback (tool args issues)
  "Apply and validate TOOL's semantic repair callback to ARGS and ISSUES."
  (when-let* ((callback (mevedel-tool-repair-input tool))
              (result (funcall callback
                               (copy-tree args t)
                               (copy-tree issues t))))
    (unless (and (listp result) (plist-member result :args))
      (error "Tool repair callback must return :args and :repairs"))
    (let* ((updated (plist-get result :args))
           (records
            (mapcar
             (lambda (record)
               (mevedel-tool-repair--validate-callback-record tool record))
             (plist-get result :repairs)))
           (changed (mevedel-tool-repair--changed-keys args updated))
           (covered
            (delete-dups
             (mapcan (lambda (record)
                       (mapcar #'car (plist-get record :paths)))
                     records))))
      (unless (and (listp updated) changed
                   (seq-every-p (lambda (key) (memq key covered)) changed))
        (error "Tool repair callback did not describe every changed argument"))
      (list :args (copy-tree updated t) :repairs records))))

(defun mevedel-tool-repair-attempt (tool args)
  "Validate and atomically repair raw model-produced TOOL ARGS.

Return a plist whose `:status' is `valid', `repaired', or `invalid'.
Invalid outcomes intentionally omit tentative args and repair records."
  (if (not mevedel-tool-input-repair-enabled)
      (if-let* ((issues (mevedel-tool-repair--validate-raw tool args)))
          (list :status 'invalid :issues issues)
        (list :status 'valid :args args :repairs nil))
    (let* ((first (mevedel-tool-repair--generic-pass
                   tool (copy-tree args t)))
           (candidate (plist-get first :args))
           (records (plist-get first :repairs))
           (seen (cons candidate (plist-get first :seen)))
           (remaining (plist-get first :remaining))
           (callback
            (mevedel-tool-repair--apply-callback
             tool candidate (plist-get first :issues))))
      (when callback
        (let ((updated (plist-get callback :args)))
          (when (member updated seen)
            (error "Tool input repair revisited a previous state"))
          (when (= remaining 0)
            (error "Tool input repair exceeded its progress bound"))
          (setq candidate updated)
          (push candidate seen)
          (setq remaining (1- remaining)))
        (setq records (append records (plist-get callback :repairs))))
      (let* ((second (mevedel-tool-repair--generic-pass
                      tool candidate seen remaining))
             (issues (plist-get second :issues)))
        (setq records (append records (plist-get second :repairs)))
        (if issues
            (append (list :status 'invalid :issues issues)
                    (when records (list :abandoned-repairs records)))
          (list :status (if records 'repaired 'valid)
                :args (plist-get second :args)
                :repairs records))))))


;;
;;; Raw gptel adapter and dispatch ledger

(defun mevedel-tool-repair--record-call (entry)
  "Append value-carrying dispatch ENTRY to the buffer-local ledger."
  (when (>= (length mevedel-tool-repair--ledger)
            mevedel-tool-repair--max-ledger-entries)
    (mevedel-tool-repair-record-result
     (car mevedel-tool-repair--ledger) nil 'abandoned 'none)
    (setq mevedel-tool-repair--ledger
          (cdr mevedel-tool-repair--ledger)))
  (setq mevedel-tool-repair--ledger
        (append mevedel-tool-repair--ledger (list entry))))

(defun mevedel-tool-repair--make-entry
    (name status raw-args final-args repairs issues info
          &optional failure-class abandoned-repairs)
  "Build a buffer-local dispatch entry from one raw tool-call INFO."
  (list :tool name :status status
        :raw-args raw-args :args final-args
        :repairs repairs :issues issues
        :backend (plist-get info :backend)
        :model (plist-get info :model)
        :session (mevedel-tool-repair--current-session)
        :origin (mevedel-tool-repair--current-origin)
        :repair-enabled (and mevedel-tool-input-repair-enabled t)
        :execution 'not-executed
        :failure-class failure-class
        :abandoned-repairs (and abandoned-repairs t)
        :buffer (current-buffer)
        :telemetry-recorded nil))

(defun mevedel-tool-repair--args-equivalent-p (left right)
  "Return non-nil when LEFT and RIGHT plists differ only by omitted nils."
  (if (and (listp left) (listp right))
      (seq-every-p
       (lambda (key) (equal (plist-get left key) (plist-get right key)))
       (delete-dups
        (append (mevedel-tool-repair--plist-keys left)
                (mevedel-tool-repair--plist-keys right))))
    (equal left right)))

(defun mevedel-tool-repair--take-ledger-entry (name args statuses)
  "Take the first NAME entry matching ARGS and one of STATUSES."
  (let (found)
    (catch 'matched
      (dolist (entry mevedel-tool-repair--ledger)
        (when (and (memq (plist-get entry :status) statuses)
                   (equal name (plist-get entry :tool))
                   (or (mevedel-tool-repair--args-equivalent-p
                        args (plist-get entry :args))
                       (mevedel-tool-repair--args-equivalent-p
                        args (plist-get entry :raw-args))))
          (setq found entry)
          (throw 'matched t))))
    (when found
      (setq mevedel-tool-repair--ledger
            (delq found mevedel-tool-repair--ledger)))
    found))

(defun mevedel-tool-repair-consume-ledger-entry (tool args)
  "Consume and return the first ledger entry matching dispatched TOOL ARGS."
  (when-let* ((entry
               (mevedel-tool-repair--take-ledger-entry
                (mevedel-tool-name tool) args '(valid repaired))))
    (when (>= (length mevedel-tool-repair--in-flight)
              mevedel-tool-repair--max-ledger-entries)
      (mevedel-tool-repair-record-result
       (car mevedel-tool-repair--in-flight) nil 'abandoned 'none))
    (setq mevedel-tool-repair--in-flight
          (append mevedel-tool-repair--in-flight (list entry)))
    entry))

(defun mevedel-tool-repair-clear-ledger (&rest _)
  "Record pending calls as abandoned, then clear current buffer tracking."
  (dolist (entry (append mevedel-tool-repair--ledger
                         mevedel-tool-repair--in-flight))
    (mevedel-tool-repair-record-result entry nil 'abandoned 'none))
  (setq mevedel-tool-repair--ledger nil
        mevedel-tool-repair--in-flight nil))

(defun mevedel-tool-repair-post-tool-call (info)
  "Finish telemetry for non-pipeline tool-call INFO."
  (when-let* ((result (plist-get info :result))
              (entry
               (mevedel-tool-repair--take-ledger-entry
                (plist-get info :name) (plist-get info :args)
                '(valid repaired invalid internal-error))))
    (mevedel-tool-repair-record-result entry result))
  nil)

(defun mevedel-tool-repair-pre-tool-call (info)
  "Repair raw gptel tool-call INFO before dispatch.

Invalid or internally failed repairs return a synthetic `:result' beginning
with `Error:' so gptel settles the call without invoking its handler."
  (let* ((name (plist-get info :name))
         (args (plist-get info :args))
         (tool (and name (mevedel-tool-get name))))
    (when tool
      (condition-case err
          (let* ((outcome (mevedel-tool-repair-attempt tool args))
                 (status (plist-get outcome :status))
                 (final-args (plist-get outcome :args))
                 (abandoned (plist-get outcome :abandoned-repairs)))
            (mevedel-tool-repair--record-call
             (mevedel-tool-repair--make-entry
              name status args final-args
              (or (plist-get outcome :repairs) abandoned)
              (plist-get outcome :issues) info nil abandoned))
            (pcase status
              ('valid nil)
              ('repaired (list :args final-args))
              ('invalid
               (list
                :result
                (concat
                 "Error: "
                 (mevedel-tool-repair-format-issues
                  tool (plist-get outcome :issues))
                 (and abandoned
                      (mevedel-tool-repair-format-audit-block
                       'abandoned abandoned)))))))
        (error
         (mevedel-tool-repair--record-call
          (mevedel-tool-repair--make-entry
           name 'internal-error args nil nil
           '((:path nil :kind internal-error)) info
           'internal-repair-error))
         (display-warning
          'mevedel
          (format "Tool input repair failed for %s (%s)"
                  name (car-safe err))
          :warning)
         (list :result
               "Error: Tool input repair failed internally; the tool was not executed."))))))

(defun mevedel-tool-repair-format-reminder (records)
  "Return one compact corrective reminder for committed repair RECORDS."
  (let ((shown nil)
        (tail records)
        (count 0))
    (while (and tail (< count 4))
      (push (plist-get (pop tail) :summary) shown)
      (setq count (1+ count)))
    (concat
     "Note: Repaired tool input: "
     (mapconcat #'identity (nreverse shown) " ")
     (when tail (format " %d additional repairs were applied." (length tail)))
     " Please use the corrected argument shape in later calls.")))

(defun mevedel-tool-repair-format-path (path)
  "Return model-readable dotted notation for issue PATH."
  (if (null path)
      "input"
    (let ((result ""))
      (dolist (part path)
        (setq result
              (concat result
                      (if (integerp part)
                          (format "[%d]" part)
                        (concat (unless (string-empty-p result) ".")
                                (let ((name (symbol-name part)))
                                  (cond
                                   ((string-match-p "[\n\r]" name)
                                    "<multiline property>")
                                   ((> (length name) 48)
                                    (format "<property, %d chars>"
                                            (length name)))
                                   (t name))))))))
      result)))

(defun mevedel-tool-repair--format-enum-choice (choice)
  "Return one bounded model-facing representation of enum CHOICE."
  (cond
   ((stringp choice)
    (cond
     ((string-match-p "[\n\r]" choice) "<multiline string>")
     ((> (length choice) 32)
      (format "<string, %d chars>" (length choice)))
     (t (format "%S" choice))))
   ((or (numberp choice) (symbolp choice))
    (let ((rendered (format "%S" choice)))
      (if (> (length rendered) 48)
          (format "<%s, %d chars>"
                  (mevedel-tool-repair--actual-type choice)
                  (length rendered))
        rendered)))
   (t (format "<%s>" (mevedel-tool-repair--actual-type choice)))))

(defun mevedel-tool-repair--format-enum (values)
  "Return a bounded model-facing representation of enum VALUES."
  (let ((remaining (append values nil))
        shown
        (count 0))
    (while (and remaining (< count 4))
      (push (mevedel-tool-repair--format-enum-choice (pop remaining)) shown)
      (setq count (1+ count)))
    (concat "[" (mapconcat #'identity (nreverse shown) ", ")
            (when remaining
              (format ", ... %d more" (length remaining)))
            "]")))

(defun mevedel-tool-repair--format-issue (issue)
  "Return one value-free model-facing sentence for ISSUE."
  (let ((path (mevedel-tool-repair-format-path
               (plist-get issue :path)))
        (kind (plist-get issue :kind))
        (expected (plist-get issue :expected))
        (actual (plist-get issue :actual)))
    (pcase kind
      ('missing-required
       (format "`%s`: required %s value is missing." path expected))
      ('unexpected-property
       (format "`%s`: this property is not accepted." path))
      ('optional-null
       (format "`%s`: omit this optional property instead of sending null."
               path))
      ('path-autolink
       (format "`%s`: pass a raw filesystem path, not Markdown or a URL."
               path))
      ('wrong-type
       (let* ((schema (plist-get issue :schema))
              (minimum (plist-get schema :minItems))
              (maximum (plist-get schema :maxItems))
              (items (plist-get schema :items))
              (detail
               (concat
                (format "%s" expected)
                (when (and (eq expected 'array) items)
                  (format " of %s items" (plist-get items :type)))
                (when minimum
                  (format ", at least %d item%s"
                          minimum (if (= minimum 1) "" "s")))
                (when maximum
                  (format ", at most %d item%s"
                          maximum (if (= maximum 1) "" "s"))))))
         (format "`%s`: expected %s; received %s." path detail actual)))
      ('invalid-enum
       (format "`%s`: expected one of %s; received %s."
               path (mevedel-tool-repair--format-enum expected) actual))
      ('too-few-items
       (format "`%s`: expected at least %d items; received %d."
               path expected actual))
      ('too-many-items
       (format "`%s`: expected at most %d items; received %d."
               path expected actual))
      (_ (format "`%s`: value does not satisfy the tool contract." path)))))

(defun mevedel-tool-repair-format-issues (tool issues)
  "Format TOOL validation ISSUES as bounded, value-free model feedback."
  (let* ((limit mevedel-tool-repair--max-rendered-issues)
         (shown nil)
         (tail issues)
         (count 0))
    (while (and tail (< count limit))
      (push (mevedel-tool-repair--format-issue (pop tail)) shown)
      (setq count (1+ count)))
    (concat
     (format "%s input is invalid; the tool was not executed.\n\n"
             (mevedel-tool-name tool))
     (mapconcat (lambda (line) (concat "- " line))
                (nreverse shown) "\n")
     (when tail
       (format "\n- %d additional issues were omitted." (length tail)))
     "\n\nNo tentative repairs were applied.")))

(provide 'mevedel-tool-repair)
;;; mevedel-tool-repair.el ends here
