;;; niwa-tool-registry.el -- Tool structs and registry -*- lexical-binding: t -*-

;;; Commentary:

;; Declarative tool registration system.  Each tool is a niwa-tool struct with
;; behavioral metadata (read-only, destructive, permissions, groups).  The
;; niwa-define-tool macro creates both the niwa-tool and the underlying
;; gptel-tool in one declaration.

;;; Code:

(require 'cl-lib)

(require 'gptel-request)
(require 'niwa-utilities)

;; `niwa-pipeline'
(autoload 'niwa-pipeline-run-tool "niwa-pipeline")

;; `niwa-ptc-interpreter'
(autoload 'niwa-ptc-tool-name-p "niwa-ptc-interpreter")

;; `niwa-utilities'
(declare-function niwa--truncate-display
                  "niwa-utilities" (text width &optional ellipsis))
(declare-function niwa--warn-once
                  "niwa-utilities" (scope key format &rest args))
(declare-function niwa-library-source-directory "niwa-utilities" (file))
(autoload 'niwa--truncate-display "niwa-utilities")
(autoload 'niwa--warn-once "niwa-utilities")
(autoload 'niwa-library-source-directory "niwa-utilities")


;;
;;; Error conditions
;;
;; Part of the tool contract: a permission slot may signal a denial with its
;; reason, and the pipeline reports failed steps as these conditions.

(define-error 'niwa-pipeline-error "Pipeline error")
(define-error 'niwa-permission-denied "Permission denied"
              'niwa-pipeline-error)
(define-error 'niwa-validation-error "Validation error"
              'niwa-pipeline-error)


;;
;;; Source directory
;;
;; Captured at load time so that `niwa-define-tool' can resolve
;; :prompt-file paths at compile time, including symlinked installations.

(defvar niwa-tool-registry--source-dir
  (niwa-library-source-directory (or load-file-name buffer-file-name))
  "Directory containing the niwa source files.
Resolved through symlinks so data files (prompts/tools/, etc.) are reachable.")


;;
;;; Tool struct

(defvar niwa-tool-render-summary-only nil
  "Non-nil while the view requests an initially collapsed tool rendering.
Renderers may omit body and child-row construction in this context, but must
preserve the complete header, status, visibility and disclosure decisions.
Renderers that expand initially must still supply their body.  Expansion and
other consumers invoke renderers with this variable nil.")

(cl-defstruct (niwa-tool (:constructor niwa-tool--create))
  "A niwa tool with behavioral metadata.

Parallel struct alongside `gptel-tool'.  `niwa-tool' holds behavioral
metadata (permissions, groups, read-only status).  The `gptel-tool' is
created as a side effect of registration and handles serialization."
  name              ; string: "Read", "ApplyPatch", "Bash"
  handler           ; function: the actual tool implementation
  description       ; string: short LLM-facing description (for schema, "ToolSearch")
  summary           ; string: ultra-short one-liner that ToolSearch matches
                    ;   and the specialist tool listing shows.  Defaults
                    ;   to the description's first sentence.
  prompt            ; string or function: detailed instructions
  prompt-source     ; plist describing the prompt's registration provenance
  args              ; arg spec list in niwa format
  category          ; string: "niwa" (default)
  ;; Behavioral declarations
  read-only-p       ; t if tool never modifies state
  snapshot-p        ; t if the touched file participates in the final patch
  destructive-p     ; t or function: needs extra confirmation
  async-p           ; t if handler takes a callback
  ;; Permission integration
  check-permission  ; function or nil: (tool-struct input) -> allow|deny|ask|nil,
                    ;   may signal `niwa-permission-denied' with a REASON
  check-permission-async ; function or nil: (tool-struct input cont) where cont
                    ;   receives one of: 'allow, 'deny, (deny . REASON),
                    ;   (feedback . TEXT), 'ask, 'aborted, nil.  Preferred over
                    ;   the sync slot; `niwa-check-permission-async' adapts
                    ;   the sync slot when only it is present.
  get-path          ; function or nil: (input) -> path this tool touches
  get-paths         ; function or nil: (input) -> every path this tool touches
  get-pattern       ; function or nil: (input) -> command string for :pattern rules
  get-domain        ; function or nil: (input) -> host string for :domain rules
  get-name          ; function or nil: (input) -> match name for :name rules
  ;; Groups
  groups            ; list of symbols: (read util tasks agents code edit eval ...)
  ;; Output size management
  max-result-size   ; integer or nil: char limit before persisting result to disk
  ;; Display
  display-arg       ; keyword, function, or nil: what to show in spinners/one-liners
                    ;   keyword (e.g. :file_path) - extract that arg value
                    ;   function (args -> string|nil) - custom formatting
                    ;   nil - use first required arg value
  render-transform  ; function or nil: consumes normalized string result before
                    ;   persistence and render-data attachment. Called with
                    ;   (name args result), returns bounded render-data or nil.
  renderer          ; function, alist, or nil: consumes render-data plist,
                    ;   returns rendering plist (:header :body :body-mode
                    ;   :initially-collapsed-p) for view-buffer display.
                    ;   Called with (name args result render-data) and must be pure.
  ;; Back-reference
  gptel-tool        ; the `gptel-tool' struct created during registration
  ;; Appended last so existing slot indices, which compiled callers inline,
  ;; stay stable.
  contextualize)    ; function or nil: (args summary) -> (args . summary)
                    ;   resolved for the current owner buffer; see
                    ;   `niwa-tool-contextual-contract'


;;
;;; Registry

(defvar niwa-tool--registry (make-hash-table :test #'equal)
  "Hash-table mapping (CATEGORY NAME) lists to `niwa-tool' structs.")

(defconst niwa-tool--builtin-registrars
  '(("Agent" "InterruptAgent" "ListAgents" "SendMessage"
     "ToolSearch" "WaitAgent"
     niwa-tool-ui niwa-tool-ui--register)
    ("Ask" niwa-tool-ask niwa-tool-ask-register)
    ("Bash" "Eval" "ListExecutions" "Monitor" "StopExecution" "WriteStdin"
     niwa-tool-exec-registration niwa-tool-exec--register)
    ("EmacsDescribe" "EmacsFind" "EmacsManual"
     niwa-tool-introspect niwa-tool-introspect--register)
    ("Glob" "Grep" "Read"
     niwa-tool-fs niwa-tool-fs--register)
    ("ApplyPatch" niwa-tool-patch-registration niwa-tool-patch-register)
    ("SharedCreate" "SharedEdit" "SharedSaveImage"
     niwa-tool-editing niwa-tool-editing--register)
    ("ToolCall" niwa-tool-ptc niwa-tool-ptc--register)
    ("CreateGoal" "GetGoal" "UpdateGoal" niwa-tool-goal niwa-tool-goal--register)
    ("Schedule" niwa-tool-schedule niwa-tool-schedule--register)
    ("Imenu" "Treesitter" "XrefDefinitions" "XrefReferences"
     niwa-tool-code niwa-tool-code--register)
    ("ListSkills" "Skill" niwa-tool-skills niwa-tool-skills--register)
    ("SubscriptionUsage" niwa-tool-usage niwa-tool-usage--register)
    ("TaskList" "TaskWrite"
     niwa-tool-task niwa-tool-task--register)
    ("NoteWrite" niwa-tool-note niwa-tool-note-register)
    ("Notify" niwa-tool-notify niwa-tool-notify-register)
    ("WebFetch" "WebSearch"
     niwa-tool-web-registration niwa-tool-web--register))
  "Built-in tool names followed by their feature and registrar.")

(defun niwa-tool-get (name &optional category)
  "Look up a niwa-tool by NAME, optionally scoped to CATEGORY.

If CATEGORY is provided, look up by exact (CATEGORY NAME) key.  If
CATEGORY is nil, search all entries for the first matching NAME."
  (if category
      (gethash (list category name) niwa-tool--registry)
    (let (found)
      (maphash (lambda (key tool)
                 (when (and (not found) (equal name (cadr key)))
                   (setq found tool)))
               niwa-tool--registry)
      found)))

(defun niwa-tool-call-name (tool)
  "Return the unambiguous expression name for TOOL."
  (if (equal (niwa-tool-category tool) "niwa")
      (niwa-tool-name tool)
    (concat (niwa-tool-category tool) "/" (niwa-tool-name tool))))

(defun niwa-tool-for-call (name)
  "Resolve expression NAME without ambiguous cross-category lookup."
  (let ((separator (string-search "/" name)))
    (if separator
        (niwa-tool-get (substring name (1+ separator))
                          (substring name 0 separator))
      (niwa-tool-get name "niwa"))))

(defun niwa-tool-contextual-contract (tool)
  "Return (ARGS . SUMMARY) of TOOL as the current buffer should see them.
A tool without a `contextualize' function shows its registered
specification.  The result may share structure with TOOL, so do not
edit it."
  (let ((args (niwa-tool-args tool))
        (summary (niwa-tool-summary tool))
        (fn (niwa-tool-contextualize tool)))
    (if fn (funcall fn args summary) (cons args summary))))

(defun niwa-tool-ensure (name)
  "Return tool NAME, loading its built-in registrar when necessary."
  (or (niwa-tool-get name)
      (when-let* ((entry (cl-find-if (lambda (candidate)
                                       (member name (butlast candidate 2)))
                                     niwa-tool--builtin-registrars))
                  (feature (nth (- (length entry) 2) entry))
                  (registrar (car (last entry))))
        (require feature)
        (funcall registrar)
        (niwa-tool-get name))))

(defun niwa-tool-callable-p (tool)
  "Return non-nil if TOOL has an unambiguous route through ToolCall."
  (let ((name (niwa-tool-call-name tool)))
    (and (niwa-ptc-tool-name-p name)
         (not (string-search "/" (niwa-tool-category tool)))
         (eq tool (niwa-tool-for-call name)))))

(defun niwa-tool-register (tool)
  "Register TOOL in the niwa tool registry.

Keyed by (CATEGORY NAME).  Overwrites any existing entry."
  (puthash (list (niwa-tool-category tool)
                 (niwa-tool-name tool))
           tool
           niwa-tool--registry)
  (niwa-tool--invalidate-resolve-cache)
  tool)

(defun niwa-tool-all ()
  "Return a list of all registered `niwa-tool' structs."
  (let (tools)
    (maphash (lambda (_key tool) (push tool tools))
             niwa-tool--registry)
    tools))

(defun niwa-tool-for-gptel (provider)
  "Return the registered `niwa-tool' that gptel PROVIDER implements, or nil.
PROVIDER is the registered provider tool itself or a copy carrying a
request-local schema: the same category and name and the same provider
function, which runs the registered tool through the pipeline.  Any
other implementation under a registered name is foreign."
  (when-let* (((gptel-tool-p provider))
              ;; An exact key: without a category the lookup would search
              ;; every category.
              (category (gptel-tool-category provider))
              (tool (niwa-tool-get (gptel-tool-name provider) category))
              (registered (niwa-tool-gptel-tool tool)))
    (and (or (eq provider registered)
             (and (gptel-tool-function provider)
                  (eq (gptel-tool-function provider)
                      (gptel-tool-function registered))))
         tool)))

(defun niwa-tool-clear-registry ()
  "Remove all tools from the registry.

Intended for testing and cleanup."
  (clrhash niwa-tool--registry)
  (niwa-tool--invalidate-resolve-cache))


;;
;;; Display

(defun niwa-tool--first-required-arg (tool)
  "Return the keyword for TOOL's first required arg, or nil."
  (let ((args (niwa-tool-args tool)))
    (cl-loop for arg in args
             when (eq (nth 2 arg) :required)
             return (intern (format ":%s" (car arg))))))

(defun niwa-tool-display-string (tool-name args &optional scope)
  "Return a display string for TOOL-NAME with ARGS.
SCOPE identifies the originating workspace when label generation fails.
Looks up the tool in the registry and uses its `display-arg' slot.
Returns nil when no meaningful display string can be produced.

When the raw value looks like a file path (contains `/'), it is
abbreviated to the last 3 components automatically.

Never signals.  This is reached from `gptel--handle-pre-tool', which runs
inside gptel's curl process sentinel, and gptel calls
`gptel--fsm-transition' outside its own `with-demoted-errors' guard, so an
error escaping here kills the sentinel before it advances the request and
the turn then waits forever for a tool result that is never dispatched.  A
`display-arg' slot is caller-supplied and parses model output -- ApplyPatch
parses the patch itself -- which is exactly the kind of code that signals on
input nobody anticipated.  Losing the label is an acceptable outcome, and
one this function already documents; losing the turn is not."
  (condition-case error
      (niwa-tool--display-string tool-name args)
    (error
     (niwa--warn-once
      (niwa-diagnostic-scope scope)
      (list 'tool-display-string tool-name)
      "Tool display string for %s signaled, omitting label: %S"
      tool-name error)
     nil)))

(defun niwa-tool--display-string (tool-name args)
  "Return the display string for TOOL-NAME with ARGS, or nil."
  (let* ((tool (niwa-tool-get tool-name))
         (display-arg (and tool (niwa-tool-display-arg tool)))
         (value
          (cond
           ((functionp display-arg)
            (funcall display-arg args))
           ((keywordp display-arg)
            (plist-get args display-arg))
           ;; Default: first required arg
           (tool
            (when-let* ((key (niwa-tool--first-required-arg tool)))
              (plist-get args key)))
           ;; Tool not in registry: try first plist value
           (t (cadr args)))))
    (when (and value (stringp value) (not (string-empty-p value)))
      ;; Abbreviate file paths
      (when (string-match-p "/" value)
        (let ((parts (split-string value "/" t)))
          (when (> (length parts) 3)
            (setq value (concat ".../" (mapconcat #'identity
                                                  (last parts 3) "/"))))))
      (niwa--truncate-display value 60 "..."))))


;;
;;; Group resolution

(defun niwa-tool-for-groups (groups)
  "Return `niwa-tool' structs whose groups intersect GROUPS.

GROUPS is a list of symbols."
  (let (result)
    (maphash (lambda (_key tool)
               (when (cl-intersection groups (niwa-tool-groups tool))
                 (push tool result)))
             niwa-tool--registry)
    result))

(defun niwa-tool-group-exists-p (group)
  "Return non-nil if any registered tool belongs to GROUP."
  (let (found)
    (maphash (lambda (_key tool)
               (when (and (not found)
                          (memq group (niwa-tool-groups tool)))
                 (setq found t)))
             niwa-tool--registry)
    found))


;;
;;; Unified tools list resolver

(defvar niwa-tool--resolve-cache (make-hash-table :test #'equal)
  "Memo table for `niwa-tool-resolve' keyed on SPECS `equal'.
Invalidated by `niwa-tool--invalidate-resolve-cache' on every
registration or clear so a stale resolution cannot leak across a
tool being registered or removed.")

(defun niwa-tool--invalidate-resolve-cache ()
  "Clear the `niwa-tool-resolve' memo table."
  (clrhash niwa-tool--resolve-cache))

(defun niwa-tool-resolve (specs)
  "Resolve a mixed tools list SPECS into `niwa-tool' structs.

SPECS is a list where each element is one of:
  - SYMBOL          bare symbol, try group first then tool name
  - (:group GROUP)  expand group from registry
  - (:tool NAME)    look up tool by name string or (CATEGORY NAME)
                    path
  - (:discoverable X)   expose X through ToolSearch/ToolCall

Returns a plist (:active TOOLS :discoverable TOOLS) where each TOOLS is a
list of niwa-tool structs.  Result is memoized on SPECS `equal' --
resolution is a hot path (every sub-agent spawn re-resolves) and the
tool registry is stable between register/clear events.  The cache is
invalidated at registration boundaries via `niwa-tool-register'
and `niwa-tool-clear-registry'."
  (let ((cached (gethash specs niwa-tool--resolve-cache 'miss)))
    (if (not (eq cached 'miss))
        cached
      (let (active deferred)
        (dolist (spec specs)
          (cond
           ;; (:discoverable X) -> resolve X, add to deferred list
           ((and (listp spec) (eq :discoverable (car spec)))
            (let ((inner (cadr spec)))
              (dolist (tool (niwa-tool--resolve-one inner))
                (push tool deferred))))
           ;; Any other form -> resolve and add to active list
           (t
            (dolist (tool (niwa-tool--resolve-one spec))
              (push tool active)))))
        (setq active (delete-dups (nreverse active))
              deferred (cl-remove-if (lambda (tool) (memq tool active))
                                     (delete-dups (nreverse deferred))))
        (dolist (tool deferred)
          (unless (niwa-tool-callable-p tool)
            (error "Tool %s cannot be discovered through ToolCall; expose it natively"
                   (niwa-tool-call-name tool))))
        (let ((result (list :active active :discoverable deferred)))
          (puthash specs result niwa-tool--resolve-cache)
          result)))))

(defun niwa-tool--resolve-one (spec)
  "Resolve a single SPEC into a list of niwa-tool structs.

SPEC is one of:
  - SYMBOL          bare symbol, try group first then tool name
  - (:group GROUP)  expand group
  - (:tool NAME)    look up by name or path"
  (cond
   ;; (:group GROUP) -> expand group
   ((and (listp spec) (eq :group (car spec)))
    (let ((group (cadr spec)))
      (or (niwa-tool-for-groups (list group))
          (error "Unknown tool group: %s" group))))
   ;; (:tool NAME-OR-PATH) -> look up single tool
   ((and (listp spec) (eq :tool (car spec)))
    (let* ((name-or-path (cadr spec))
           (tool (if (listp name-or-path)
                     ;; (CATEGORY NAME) path
                     (niwa-tool-get (cadr name-or-path) (car name-or-path))
                   ;; Plain name string
                   (niwa-tool-get (if (symbolp name-or-path)
                                         (symbol-name name-or-path)
                                       name-or-path)))))
      (unless tool
        (error "Unknown tool: %s" name-or-path))
      (list tool)))
   ;; Bare symbol -> group-first-then-tool heuristic
   ((symbolp spec)
    (if (niwa-tool-group-exists-p spec)
        (niwa-tool-for-groups (list spec))
      (let ((tool (niwa-tool-get (symbol-name spec))))
        (if tool
            (list tool)
          (error "Unknown group or tool: %s" spec)))))
   (t (error "Invalid tool spec: %S" spec))))

(defun niwa-tool-resolve-gptel (specs)
  "Resolve SPECS and return `gptel-tool' structs.

Convenience wrapper around `niwa-tool-resolve' that extracts the
`gptel-tool' back-references.  Returns a plist (:active GPTEL-TOOLS
:discoverable GPTEL-TOOLS)."
  (let ((resolved (niwa-tool-resolve specs)))
    (list :active (mapcar #'niwa-tool-gptel-tool (plist-get resolved :active))
          :discoverable (mapcar #'niwa-tool-gptel-tool (plist-get resolved :discoverable)))))


;;
;;; Args conversion

(defun niwa-tool--positional-to-plist (arg-values arg-specs)
  "Convert positional ARG-VALUES to a keyword plist using ARG-SPECS.

ARG-SPECS is the niwa args format: ((name type ...) ...).
ARG-VALUES is a list of values in the same order.
Returns a plist like (:name1 val1 :name2 val2 ...)."
  (let ((plist nil))
    (cl-loop for spec in arg-specs
             for val in arg-values
             do (push (intern (format ":%s" (car spec))) plist)
             (push val plist))
    (nreverse plist)))

(defun niwa-tool-args-key (args)
  "Return tool-call ARGS as a key that identifies the call under `equal'.
gptel's tool hooks see the model's argument plist, while the pipeline
receives `niwa-tool--positional-to-plist' output: every specified
argument in spec order, omitted ones as nil.  The key lists ARGS' non-nil
top-level arguments as (KEY . VALUE) pairs ordered by key, so both forms
of one call agree.  The pairs are fresh and share ARGS' values: ARGS is
not modified and large values are neither copied nor truncated.  Nested
values compare structurally under `equal'.  ARGS that is not a plist is
returned unchanged."
  (if (and (consp args) (plistp args))
      (let (pairs)
        (while args
          (when (cadr args)
            (push (cons (car args) (cadr args)) pairs))
          (setq args (cddr args)))
        (sort pairs :key #'car :in-place t))
    args))

(defconst niwa-tool--path-description-suffix
  "Pass a raw filesystem path, not Markdown or a web URL."
  "Provider-facing guidance appended to filesystem-only path arguments.")

(defconst niwa-tool--path-or-resource-description-suffix
  "Pass a raw filesystem path or canonical resource address, not Markdown or a web URL."
  "Provider-facing guidance appended to path-or-resource arguments.")

(defun niwa-tool--provider-arg-description (type description)
  "Return provider-facing DESCRIPTION for argument TYPE."
  (let ((description (or description "")))
    (let ((suffix (pcase type
                    ('path niwa-tool--path-description-suffix)
                    ('path-or-resource
                     niwa-tool--path-or-resource-description-suffix))))
      (if (and suffix (not (string-search suffix description)))
          (concat description
                  (unless (string-empty-p description) " ")
                  suffix)
        description))))

(defun niwa-tool--args-to-gptel (args)
  "Convert niwa ARGS spec to gptel plist format.

Niwa format:
  ((name type :required \"desc\" [extras...]) ...)
  ((name type :optional \"desc\" [extras...]) ...)

Gptel format:
  ((:name \"name\" :type type :description \"desc\" [extras...]) ...)
  with :optional t for optional args.

Any trailing plist keys after the description (e.g. `:items',
`:enum', `:properties') are passed through verbatim to gptel, which
propagates them into the JSON schema.  Required for array/object
types under strict function-schema validation.  The niwa-only
`:access' key (see `niwa-tool-arg-access') stays out of the schema."
  (mapcar
   (lambda (arg-spec)
     (let* ((name (car arg-spec))
            (type (cadr arg-spec))
            (rest (cddr arg-spec))
            (required (eq :required (car rest)))
            (desc (cadr rest))
            (extras (cl-loop for (key value) on (cddr rest) by #'cddr
                             unless (eq key :access)
                             append (list key value)))
            (result (list :name (symbol-name name)
                          :type (if (memq type '(path path-or-resource))
                                    'string type)
                          :description
                          (niwa-tool--provider-arg-description
                           type desc))))
       (unless required
         (setq result (append result (list :optional t))))
       (when extras
         (setq result (append result (copy-tree extras t))))
       result))
   args))

(defun niwa-tool-arg-access (arg-spec)
  "Return the filesystem access ARG-SPEC declares for its path value, or nil.
ARG-SPEC is (NAME TYPE :required|:optional DESCRIPTION [KEY VALUE]...).
`:access read' on a `path-or-resource' argument of a tool that otherwise
mutates says the tool only reads the file or resource the argument names,
so that argument is authorized as a read and a resource address is
prepared for reading."
  (plist-get (nthcdr 4 arg-spec) :access))

(defconst niwa-tool--canonical-types
  '(string number integer boolean array object)
  "Canonical niwa arg types.")

(defun niwa-tool--normalize-type (raw source-name)
  "Normalize RAW arg type value to a canonical symbol.

RAW may be a symbol (`string'), a quoted symbol (`'string'), or a
string (`\"string\"').  SOURCE-NAME is attached to the error when
the type is unrecognized."
  (let* ((value (cond
                 ((and (consp raw) (eq 'quote (car raw)))
                  (cadr raw))
                 ((stringp raw) (intern raw))
                 ((symbolp raw) raw)
                 (t (error "Wrapped tool %S has unsupported :type %S"
                           source-name raw)))))
    (unless (memq value niwa-tool--canonical-types)
      (error "Wrapped tool %S has unknown :type %S (expected one of %S)"
             source-name raw niwa-tool--canonical-types))
    value))

(defun niwa-tool--normalize-schema (value source-name)
  "Copy VALUE while normalizing nested schema types for SOURCE-NAME."
  (cond
   ((vectorp value)
    (vconcat (mapcar (lambda (item)
                       (niwa-tool--normalize-schema item source-name))
                     value)))
   ((consp value)
    (let ((copy (mapcar (lambda (item)
                          (niwa-tool--normalize-schema item source-name))
                        value)))
      (when-let* (((keywordp (car-safe copy)))
                  (raw-type (plist-get copy :type))
                  ((or (symbolp raw-type)
                       (stringp raw-type)
                       (and (consp raw-type) (eq 'quote (car raw-type))))))
        (plist-put copy :type
                   (niwa-tool--normalize-type raw-type source-name)))
      copy))
   (t value)))

(defun niwa-tool--args-from-gptel (gptel-args source-name)
  "Convert GPTEL-ARGS plist list to niwa arg spec format.

SOURCE-NAME is used in error messages when normalisation fails.

Gptel format:
  ((:name \"name\" :type TYPE :description \"...\" [:optional t] ...) ...)
Niwa format: ((name TYPE :required \"desc\" ...) ...)

Extra keys (`:items', `:enum', `:properties', ...) are preserved as
a trailing plist tail on the niwa spec element."
  (mapcar
   (lambda (arg)
     (let* ((raw-name (plist-get arg :name))
            (name (cond
                   ((symbolp raw-name) raw-name)
                   ((stringp raw-name) (intern raw-name))
                   (t (error "Wrapped tool %S has unsupported :name %S"
                             source-name raw-name))))
            (type (niwa-tool--normalize-type
                   (plist-get arg :type) source-name))
            (required (not (plist-get arg :optional)))
            (desc (or (plist-get arg :description) ""))
            (extras (cl-loop for (k v) on arg by #'cddr
                             unless (memq k '(:name :type :description :optional))
                             append
                             (list k (niwa-tool--normalize-schema
                                      v source-name)))))
       (append (list name type (if required :required :optional) desc)
               extras)))
   gptel-args))

(defmacro niwa-tool--with-quiet-file-visit (&rest body)
  "Run BODY while suppressing interactive file-visit side effects.
Tools visit files during turns that nobody in Emacs may be there to
answer, so visiting never asks: unsafe local variables are skipped, a
large file opens without the size warning, and a version-controlled
symlink is followed."
  (declare (indent 0) (debug t))
  `(let ((enable-local-variables :safe)
         (large-file-warning-threshold nil)
         (vc-follow-symlinks t)
         (find-file-hook nil)
         (hack-local-variables-hook nil))
     ,@body))

(defun niwa-tool--call-wrapped-handler (source)
  "Return a handler that dispatches to the wrapped source tool.

SOURCE is the `gptel-tool' whose identity, argument schema, positional
order, and calling convention are frozen.  The returned function has
niwa handler shape -- `(callback args-plist)' -- and is called by
`niwa-pipeline--step-handler'.

The dispatcher does a fresh `gptel-get-tool' lookup on every call
so MCP reconnects (where mcp.el rebuilds the source struct with a
fresh `:function') propagate automatically when the source contract
is unchanged."
  (let* ((source-category (gptel-tool-category source))
         (source-name (gptel-tool-name source))
         (source-args (copy-tree (gptel-tool-args source) t))
         (niwa-args (niwa-tool--args-from-gptel
                        source-args source-name))
         (async-p (and (gptel-tool-async source) t)))
    (lambda (callback args)
      (let ((return (lambda (result)
                      (funcall callback (list :result result)))))
        (condition-case err
            (let* ((source (condition-case _
                               (gptel-get-tool
                                (list source-category source-name))
                             (error nil))))
              (cond
               ((null source)
                (funcall return
                         (format "Error: wrapped tool %S has been unregistered; reconnect and re-wrap if needed"
                                 source-name)))
               ((not (gptel-tool-p source))
                (funcall return
                         (format "Error: wrapped tool %S resolved to a non-tool value"
                                 source-name)))
               ((or (not (equal source-args (gptel-tool-args source)))
                    (not (eq async-p (and (gptel-tool-async source) t))))
                (funcall return
                         (format "Error: wrapped tool %S contract changed; re-wrap it"
                                 source-name)))
               (t
                (let* ((fn (gptel-tool-function source))
                       (positional
                        (cl-loop for spec in niwa-args
                                 collect (plist-get
                                          args
                                          (intern
                                           (format ":%s" (car spec)))))))
                  (niwa-tool--with-quiet-file-visit
                    (if async-p
                        (apply fn return positional)
                      (funcall return (apply fn positional))))))))
          (error
           (funcall return
                    (format "Error: %s" (error-message-string err)))))))))

(defun niwa-tool-truthy-p (value)
  "Return non-nil if VALUE is a truthy LLM-supplied boolean.

gptel parses JSON false to `:json-false', which is non-nil in Emacs
Lisp.  Use this on every `plist-get' whose key is declared boolean in
a tool schema, otherwise `(if (plist-get args :flag) ...)' wrongly
treats `false' as true."
  (and value (not (eq value :json-false))))

(defun niwa-tool-string-arg (args key &optional default)
  "Return the string value at KEY in plist ARGS, or DEFAULT.

Returns DEFAULT (nil if omitted) whenever the value is missing,
`:json-false', non-string, or the empty string.  Use this on every
optional string argument fetched from LLM-supplied ARGS to avoid
passing `\"\"' / `:json-false' downstream to shell tools, path
expansion, etc.  Required string arguments are validated upstream
by `niwa-tool-repair-validate' and do not need this helper."
  (let ((v (plist-get args key)))
    (if (and (stringp v) (not (string-empty-p v)))
        v
      default)))

(defun niwa-tool-integer-arg (args key &optional default)
  "Return the integer value at KEY in plist ARGS, or DEFAULT.

Returns DEFAULT (nil if omitted) whenever the value is missing,
`:json-false', or not an integer.  Use this on every optional
numeric argument fetched from LLM-supplied ARGS before passing it
to `format' or arithmetic, otherwise a mistyped value crashes the
handler."
  (let ((v (plist-get args key)))
    (if (integerp v) v default)))


;;
;;; Prompt resolution

(defun niwa-tool--resolve-prompt (prompt)
  "Resolve PROMPT to a string.

If PROMPT is a string, return it unchanged.  If PROMPT is a function,
call it with no arguments and return the result, which must be a string.
Signals an error for any other type."
  (cond
   ((stringp prompt) prompt)
   ((functionp prompt)
    (let ((result (funcall prompt)))
      (unless (stringp result)
        (error "Tool prompt function must return a string, got %S" result))
      result))
   (t (error "Tool prompt must be a string or function, got %S" prompt))))

(defun niwa-tool--default-summary (description)
  "Return DESCRIPTION's first sentence as a one-line tool summary, or nil."
  (when-let* (((stringp description))
              (line (car (split-string description "[\n\r]+" t "[ \t]+"))))
    (truncate-string-to-width
     (if (string-match "[.!?]\\(?: \\|\\'\\)" line)
         (substring line 0 (1+ (match-beginning 0)))
       line)
     160 nil nil "...")))


;;
;;; Registration macro

(defmacro niwa-define-tool (&rest props)
  "Define and register a niwa tool.

PROPS is a plist with the following keys:

Native form (no :wrap):

Required:
  :name         STRING   Tool name
  :description  STRING   Short LLM-facing description

  :summary      STRING   Optional ultra-short one-liner that ToolSearch
                         matches and the specialist tool listing shows.
                         Defaults to the description's first sentence;
                         full descriptions are not searched.

Wrap form (:wrap EXPR):

  :wrap EXPR evaluates at runtime to a `gptel-tool' struct.  The
  wrapped tool's name, args, :async flag, and handler function are
  derived from the source.  Supplying :name, :args, :async-p, or
  :handler alongside :wrap is an error (prevents drift).  :category,
  :description, :prompt, and :prompt-file remain overridable.
  :category defaults to \"niwa-<source-category>\".

Optional (both forms):
  :handler          FUNCTION     Tool implementation returning a plist with
                                 required `:result' (native only)
  :prompt           STRING-OR-FN Detailed instructions (defaults to
                                 description)
  :prompt-file      STRING       Load prompt from file (relative to niwa
                                 source dir)
  :args             LIST         Arg specs: ((name type :required \"desc\") ...)
                                 A `path-or-resource' spec may add `:access
                                 read' when the tool only reads that file or
                                 resource; see `niwa-tool-arg-access'.
  :contextualize    FN           Native only.  Called in the owner's buffer as
                                 (fn args summary) with the registered
                                 specification; returns (ARGS . SUMMARY)
                                 for what ToolSearch and the specialist
                                 listing show.  Must not edit its inputs.
  :category         STRING       Tool category (default \"niwa\")
  :groups           LIST         Group symbols: (read edit tasks agents ...)
  :read-only-p      BOOL         Tool never modifies state
  :snapshot-p       BOOL         Snapshot the touched file for final patching
  :destructive-p    BOOL-OR-FN   Needs extra confirmation
  :async-p          BOOL         Handler takes a callback as first arg
                                 (native only)
  :check-permission FN           Custom permission check; sync form returning
                                 `allow'/`deny'/`ask'/nil, may signal
                                 `niwa-permission-denied' with a reason
                                 string.  Kept for tools whose check is pure
                                 data (introspection, Eval, Bash pattern
                                 matching after sync validation).
  :check-permission-async FN     Async permission check; signature
                                 (tool-struct input cont) where cont receives
                                 `allow', `deny', (deny . REASON),
                                 (feedback . TEXT), `ask', `aborted', or nil
                                 (fall through).  Preferred over the sync slot;
                                 tools that prompt the user via their own
                                 overlay UI (Bash, Eval) define this.
  :get-path         FN           Extract path from input for `:path' rules
  :get-paths        FN           Extract all paths from aggregate file input
  :get-pattern      FN           Extract command string from input for
                                 `:pattern' rules (Bash and similar)
  :get-domain       FN           Extract host from input for `:domain' rules
                                 (WebFetch, WebSearch)
  :get-name         FN           Extract match name from input for `:name'
                                 rules (Agent subagent types, etc.)
  :max-result-size  INTEGER      Char limit before persisting result to disk
                                 (nil = self-bounded, no persistence)
  :display-arg      KEY-OR-FN   What to show in spinners and one-liners.
                                 Keyword: extract that arg value.
                                 Function: (args) -> string or nil.
                                 Nil: use first required arg value.
  :render-transform FN          Pure function called as
                                 (fn name args result), where RESULT is the
                                 normalized string result before persistence
                                 and side-channel attachment. Returns bounded
                                 render-data metadata or nil.
  :renderer         FN-OR-ALIST Pure renderer for the view buffer. Function
                                 form is called as
                                 (fn name args result render-data). Alist form
                                 dispatches on result status, e.g.
                                 ((success . FN) (error . FN)
                                  (default . FN)). Must return a plist with
                                 keys :header :body :body-mode
                                 :initially-collapsed-p, or nil to use the
                                 generic renderer fallback.

The macro creates a `niwa-tool' struct, registers it, and calls
`gptel-make-tool' to create the underlying gptel-tool."
  (declare (indent 0) (debug t))
  (cl-loop for (key _value) on props by #'cddr
           unless (memq key
                        '(:args :async-p :category :check-permission
                          :check-permission-async :description :destructive-p
                          :display-arg :get-domain :get-name :get-path
                          :get-paths :get-pattern :groups :handler
                          :max-result-size :name :prompt :prompt-file
                          :read-only-p :renderer :render-transform :snapshot-p
                          :summary :wrap :contextualize))
           do (error "Unknown tool property: %S" key))
  (let* ((wrap (plist-get props :wrap)))
    (if wrap
        (niwa-tool--expand-wrap props)
      (niwa-tool--expand-native props))))

(defun niwa-tool--expand-native (props)
  "Expand `niwa-define-tool' PROPS for the native registration form."
  (let* ((name (plist-get props :name))
         (handler (plist-get props :handler))
         (description (plist-get props :description))
         (summary (plist-get props :summary))
         (prompt (plist-get props :prompt))
         (prompt-file (plist-get props :prompt-file))
         (prompt-source
          (cond
           (prompt-file
            (list :kind 'file
                  :path (expand-file-name
                         prompt-file niwa-tool-registry--source-dir)))
           ((plist-member props :prompt) '(:kind inline))
           (t '(:kind description))))
         (args (plist-get props :args))
         (contextualize (plist-get props :contextualize))
         (category (or (plist-get props :category) "niwa"))
         (groups (plist-get props :groups))
         (read-only-p (plist-get props :read-only-p))
         (snapshot-p (plist-get props :snapshot-p))
         (destructive-p (plist-get props :destructive-p))
         (async-p (plist-get props :async-p))
         (check-permission (plist-get props :check-permission))
         (check-permission-async (plist-get props :check-permission-async))
         (get-path (plist-get props :get-path))
         (get-paths (plist-get props :get-paths))
         (get-pattern (plist-get props :get-pattern))
         (get-domain (plist-get props :get-domain))
         (get-name (plist-get props :get-name))
         (max-result-size (plist-get props :max-result-size))
         (display-arg (plist-get props :display-arg))
         (render-transform (plist-get props :render-transform))
         (renderer (plist-get props :renderer)))
    (unless name (error "Tool :name is required"))
    (unless description (error "Tool :description is required"))
    (when prompt-file
      (let ((path (expand-file-name prompt-file
                                    niwa-tool-registry--source-dir)))
        (if (file-exists-p path)
            (setq prompt (with-temp-buffer
                           (insert-file-contents path)
                           (buffer-string)))
          (error "Prompt file not found: %s" path))))
    `(let* ((resolved-prompt (niwa-tool--resolve-prompt
                              ,(or prompt description)))
            (mtool
             (niwa-tool--create
              :name ,name
              :handler ,handler
              :description ,description
              :summary (or ,summary (niwa-tool--default-summary ,description))
              :prompt resolved-prompt
              :prompt-source ',prompt-source
              :args ',args
              :contextualize ,contextualize
              :category ,category
              :read-only-p ,read-only-p
              :snapshot-p ,snapshot-p
              :destructive-p ,destructive-p
              :async-p ,async-p
              :check-permission ,check-permission
              :check-permission-async ,check-permission-async
              :get-path ,get-path
              :get-paths ,get-paths
              :get-pattern ,get-pattern
              :get-domain ,get-domain
              :get-name ,get-name
              :groups ',groups
              :max-result-size ,max-result-size
              :display-arg ,display-arg
              :render-transform ,render-transform
              :renderer ,renderer))
            (gptel-tool
             (gptel-make-tool
              :name ,name
              :function (lambda (callback &rest raw-args)
                          (niwa-pipeline-run-tool
                           mtool callback
                           (niwa-tool--positional-to-plist
                            raw-args ',(or args nil))))
              :description resolved-prompt
              ;; Each registration owns its provider schema.  The expansion's
              ;; quoted schema is shared by every evaluation, while gptel
              ;; preprocesses it destructively and per-request updates edit
              ;; it in place.
              :args (copy-tree ',(when args (niwa-tool--args-to-gptel args)) t)
              ;; Always async: the pipeline wrapper uses continuations
              ;; (permission prompts, async handlers).  The handler's
              ;; own async nature is tracked in niwa-tool :async-p
              ;; and honored by `niwa-pipeline--step-handler'.
              :async t
              :category ,category)))
       (setf (niwa-tool-gptel-tool mtool) gptel-tool)
       (niwa-tool-register mtool))))

(defun niwa-tool--expand-wrap (props)
  "Expand `niwa-define-tool' PROPS for the :wrap registration form."
  (let* ((wrap-form (plist-get props :wrap))
         (category-override (plist-get props :category))
         (description-override (plist-get props :description))
         (summary (plist-get props :summary))
         (prompt-override (plist-get props :prompt))
         (prompt-file (plist-get props :prompt-file))
         (prompt-source
          (cond
           (prompt-file
            (list :kind 'file
                  :path (expand-file-name
                         prompt-file niwa-tool-registry--source-dir)))
           ((plist-member props :prompt) '(:kind inline))
           ((plist-member props :description) '(:kind description))
           (t nil)))
         (groups (plist-get props :groups))
         (read-only-p (plist-get props :read-only-p))
         (snapshot-p (plist-get props :snapshot-p))
         (destructive-p (plist-get props :destructive-p))
         (check-permission (plist-get props :check-permission))
         (check-permission-async (plist-get props :check-permission-async))
         (get-path (plist-get props :get-path))
         (get-paths (plist-get props :get-paths))
         (get-pattern (plist-get props :get-pattern))
         (get-domain (plist-get props :get-domain))
         (get-name (plist-get props :get-name))
         (max-result-size (plist-get props :max-result-size))
         (display-arg (plist-get props :display-arg))
         (render-transform (plist-get props :render-transform))
         (renderer (plist-get props :renderer)))
    (dolist (k '(:name :args :async-p :handler))
      (when (plist-member props k)
        (error "niwa-define-tool: %s is derived from :wrap, do not supply"
               k)))
    (when (plist-member props :contextualize)
      (error "Tool property :contextualize is not supported with :wrap"))
    (when prompt-file
      (let ((path (expand-file-name prompt-file
                                    niwa-tool-registry--source-dir)))
        (if (file-exists-p path)
            (setq prompt-override
                  (with-temp-buffer
                    (insert-file-contents path)
                    (buffer-string)))
          (error "Prompt file not found: %s" path))))
    `(niwa-tool--register-wrap
      :source ,wrap-form
      :category-override ,category-override
      :description-override ,description-override
      :summary ,summary
      :prompt-override ,prompt-override
      :prompt-source ',prompt-source
      :groups ',groups
      :read-only-p ,read-only-p
      :snapshot-p ,snapshot-p
      :destructive-p ,destructive-p
      :check-permission ,check-permission
      :check-permission-async ,check-permission-async
      :get-path ,get-path
      :get-paths ,get-paths
      :get-pattern ,get-pattern
      :get-domain ,get-domain
      :get-name ,get-name
      :max-result-size ,max-result-size
      :display-arg ,display-arg
      :render-transform ,render-transform
      :renderer ,renderer)))

(cl-defun niwa-tool--register-wrap
    (&key source category-override description-override summary
          prompt-override prompt-source groups read-only-p
          snapshot-p destructive-p
          check-permission check-permission-async
          get-path get-paths get-pattern get-domain get-name
          max-result-size display-arg render-transform renderer)
  "Runtime helper: build and register a wrapped tool from SOURCE.

SOURCE must be a `gptel-tool' struct.  CATEGORY-OVERRIDE,
DESCRIPTION-OVERRIDE, SUMMARY, PROMPT-OVERRIDE, PROMPT-SOURCE, GROUPS,
READ-ONLY-P, SNAPSHOT-P, DESTRUCTIVE-P, CHECK-PERMISSION,
CHECK-PERMISSION-ASYNC, GET-PATH,
GET-PATHS, GET-PATTERN, GET-DOMAIN, GET-NAME, MAX-RESULT-SIZE, DISPLAY-ARG,
RENDER-TRANSFORM, and RENDERER mirror `niwa-define-tool'."
  (unless (gptel-tool-p source)
    (error "`niwa-define-tool :wrap' expects a gptel-tool, got %S" source))
  (let* ((source-name (gptel-tool-name source))
         (source-category (gptel-tool-category source))
         (target-category (or category-override
                              (format "niwa-%s" source-category)))
         (source-args (copy-tree (gptel-tool-args source) t))
         (niwa-args (niwa-tool--args-from-gptel
                        source-args source-name)))
    (let* ((source-description (gptel-tool-description source))
           (description (or description-override source-description))
           (resolved-prompt (niwa-tool--resolve-prompt
                             (or prompt-override description)))
           (handler (niwa-tool--call-wrapped-handler source))
           (mtool
            (niwa-tool--create
             :name source-name
             :handler handler
             :description description
             :summary (or summary (niwa-tool--default-summary description))
             :prompt resolved-prompt
             :prompt-source
             (or prompt-source
                 (list :kind 'wrapped
                       :category source-category
                       :name source-name))
             :args niwa-args
             :category target-category
             :read-only-p read-only-p
             :snapshot-p snapshot-p
             :destructive-p destructive-p
             :async-p t
             :check-permission check-permission
             :check-permission-async check-permission-async
             :get-path get-path
             :get-paths get-paths
             :get-pattern get-pattern
             :get-domain get-domain
             :get-name get-name
             :groups groups
             :max-result-size max-result-size
             :display-arg display-arg
             :render-transform render-transform
             :renderer renderer))
           (gptel-tool
            (gptel-make-tool
             :name source-name
             :function (lambda (callback &rest raw-args)
                         (niwa-pipeline-run-tool
                          mtool callback
                          (niwa-tool--positional-to-plist
                           raw-args niwa-args)))
             :description resolved-prompt
             :args (niwa-tool--args-to-gptel niwa-args)
             :async t
             :include (gptel-tool-include source)
             :category target-category)))
      (setf (niwa-tool-gptel-tool mtool) gptel-tool)
      (niwa-tool-register mtool))))

;;
;;; Validation macros

(defmacro niwa-tools--validate-params (callback function-name &rest param-specs)
  "Validate parameters for tool functions.

CALLBACK is the callback function to call with error messages (can be
nil for sync functions).
FUNCTION-NAME is the name of the function being validated (symbol, can
be nil for lambdas).
PARAM-SPECS is a list of (VAR TYPE-SPEC) or (VAR TYPE-SPEC REQUIRED)
forms where:

  - VAR is the parameter variable name (symbol)
  - TYPE-SPEC is either:
    - A predicate function symbol (e.g., stringp, integerp)
      Special: booleanp handles both t and :json-false automatically
    - A cons (PRED . TYPE-NAME) for custom type names
      e.g., (vectorp . \"array\") checks with vectorp, reports \"array\"
  - REQUIRED is optional, defaults to t.  If nil, skip validation when
    VAR is nil.

Examples:
  (niwa-tools--validate-params callback my-func
    (name stringp)                    ; Required string
    (enabled booleanp)                ; Boolean (handles :json-false)
    (count integerp nil)              ; Optional integer
    (items (vectorp . \"array\")))    ; Vector reported as \"array\"

Returns validation code that uses `cl-return-from' if CALLBACK is
non-nil, otherwise `error', to exit early."
  (declare (indent defun) (debug t))
  (let ((clauses nil))
    (dolist (spec param-specs)
      (cl-destructuring-bind (var type-spec &optional (required t)) spec
        (let* ((var-name (symbol-name var))
               ;; Handle plain symbols and cons cells with custom names.
               (type-pred (cond
                           ;; Pred with custom name: (pred . "type")
                           ((consp type-spec) (car type-spec))
                           ;; Plain predicate symbol
                           (t type-spec)))
               (type-name (cond
                           ;; Custom type name in cdr
                           ((and (consp type-spec) (stringp (cdr type-spec)))
                            (cdr type-spec))
                           ;; Plain predicate - derive from name
                           (t (replace-regexp-in-string
                               "p$" "" (symbol-name type-pred)))))
               ;; Build the type check form
               (type-check-form
                (cond
                 ;; Special case: booleanp handles both t and :json-false
                 ((eq type-pred 'booleanp)
                  `(or (eq ,var t) (eq ,var :json-false)))
                 ;; Regular predicate function
                 (t `(,type-pred ,var)))))

          ;; Add required check
          (when required
            (push `((not ,var)
                    ,(if callback
                         `(cl-return-from ,function-name
                            (funcall ,callback
                                     ,(format "Error: '%s' parameter is required" var-name)))
                       `(error ,(format "'%s' parameter is required" var-name))))
                  clauses))

          ;; Add type check
          (let ((err-msg `(format ,(format "%s'%s' must be %s%%s, received %%s: %%S"
                                           (if callback "Error: " "")
                                           var-name
                                           (if (string-match-p "^[aeiou]" (downcase type-name))
                                               (concat "an " type-name)
                                             (concat "a " type-name)))
                           ,(if required "" " (when provided)")
                           (type-of ,var) ,var)))
            (push `(,(if required
                         `(not ,type-check-form)
                       `(and ,var (not ,type-check-form)))
                    ,(if callback
                         `(cl-return-from ,function-name
                            (funcall ,callback ,err-msg))
                       `(error "%s" ,err-msg)))
                  clauses)))))
    `(cond ,@(nreverse clauses))))

(provide 'niwa-tool-registry)
;;; niwa-tool-registry.el ends here
