;;; mevedel-tool-registry.el -- Tool structs and registry -*- lexical-binding: t -*-

;;; Commentary:

;; Declarative tool registration system.  Each tool is a mevedel-tool struct with
;; behavioral metadata (read-only, destructive, permissions, groups).  The
;; mevedel-define-tool macro creates both the mevedel-tool and the underlying
;; gptel-tool in one declaration.

;;; Code:

(require 'cl-lib)

(require 'mevedel-pipeline)

;; `gptel-request'
(declare-function gptel-make-tool "ext:gptel-request" (&rest slots))
(declare-function gptel-get-tool "ext:gptel-request" (path))
(declare-function gptel-tool-p "ext:gptel-request" (object))
(declare-function gptel-tool-name "ext:gptel-request" (tool))
(declare-function gptel-tool-description "ext:gptel-request" (tool))
(declare-function gptel-tool-args "ext:gptel-request" (tool))
(declare-function gptel-tool-async "ext:gptel-request" (tool))
(declare-function gptel-tool-category "ext:gptel-request" (tool))
(declare-function gptel-tool-function "ext:gptel-request" (tool))
(declare-function gptel-tool-include "ext:gptel-request" (tool))
(defvar gptel--known-tools)


;;
;;; Source directory
;;
;; Captured at load time so that `mevedel-define-tool' can resolve
;; :prompt-file paths at compile time.  Straight.el symlinks .el files
;; into build/ but not data directories, so we resolve symlinks to find
;; the real repo root where tools/ lives.

(defvar mevedel-tool-registry--source-dir
  (let* ((lib (or load-file-name buffer-file-name))
         ;; .elc is a real file in build/; the corresponding .el is a
         ;; symlink back to the repo -- resolve that.
         (el-file (if (string-suffix-p ".elc" lib)
                      (substring lib 0 -1)
                    lib)))
    (file-name-directory (file-truename el-file)))
  "Directory containing the mevedel source files.
Resolved through symlinks so data files (tools/, etc.) are reachable.")


;;
;;; Tool struct

(cl-defstruct (mevedel-tool (:constructor mevedel-tool--create))
  "A mevedel tool with behavioral metadata.

Parallel struct alongside `gptel-tool'.  `mevedel-tool' holds behavioral
metadata (permissions, groups, read-only status).  The `gptel-tool' is
created as a side effect of registration and handles serialization."
  name              ; string: "Read", "Edit", "Bash"
  handler           ; function: the actual tool implementation
  description       ; string: short LLM-facing description (for schema, "ToolSearch")
  summary           ; string or nil: ultra-short one-liner for the
                    ;   deferred-tools roster reminder.  When nil, the
                    ;   roster lists just the tool name (some
                    ;   third-party / wrapped tools have multi-line
                    ;   descriptions that bloat the system reminder).
  prompt            ; string or function: detailed instructions
  args              ; arg spec list in mevedel format
  repair-input      ; function or nil: tool-owned semantic input repair
  category          ; string: "mevedel" (default)
  ;; Behavioral declarations
  read-only-p       ; t if tool never modifies state
  destructive-p     ; t or function: needs extra confirmation
  async-p           ; t if handler takes a callback
  ;; Permission integration
  check-permission  ; function or nil: (tool-struct input) -> allow|deny|ask|nil,
                    ;   may signal `mevedel-permission-denied' with a REASON
  check-permission-async ; function or nil: (tool-struct input cont) where cont
                    ;   receives one of: 'allow, 'deny, (deny . REASON),
                    ;   (feedback . TEXT), 'ask, 'aborted, nil.  Preferred over
                    ;   the sync slot; `mevedel-check-permission-async' adapts
                    ;   the sync slot when only it is present.
  get-path          ; function or nil: (input) -> path this tool touches
  get-pattern       ; function or nil: (input) -> command string for :pattern rules
  get-domain        ; function or nil: (input) -> host string for :domain rules
  get-name          ; function or nil: (input) -> match name for :name rules
  ;; Groups
  groups            ; list of symbols: (read util code edit eval ...)
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
  gptel-tool)       ; the `gptel-tool' struct created during registration


;;
;;; Registry

(defvar mevedel-tool--registry (make-hash-table :test #'equal)
  "Hash-table mapping (CATEGORY NAME) lists to `mevedel-tool' structs.")

(defun mevedel-tool-get (name &optional category)
  "Look up a mevedel-tool by NAME, optionally scoped to CATEGORY.

If CATEGORY is provided, look up by exact (CATEGORY NAME) key.  If
CATEGORY is nil, search all entries for the first matching NAME."
  (if category
      (gethash (list category name) mevedel-tool--registry)
    (let (found)
      (maphash (lambda (key tool)
                 (when (and (not found) (equal name (cadr key)))
                   (setq found tool)))
               mevedel-tool--registry)
      found)))

(defun mevedel-tool-register (tool)
  "Register TOOL in the mevedel tool registry.

Keyed by (CATEGORY NAME).  Overwrites any existing entry."
  (puthash (list (mevedel-tool-category tool)
                 (mevedel-tool-name tool))
           tool
           mevedel-tool--registry)
  (mevedel-tool--invalidate-resolve-cache)
  tool)

(defun mevedel-tool-all ()
  "Return a list of all registered `mevedel-tool' structs."
  (let (tools)
    (maphash (lambda (_key tool) (push tool tools))
             mevedel-tool--registry)
    tools))

(defun mevedel-tool-clear-registry ()
  "Remove all tools from the registry.

Intended for testing and cleanup."
  (clrhash mevedel-tool--registry)
  (mevedel-tool--invalidate-resolve-cache))


;;
;;; Display

(defun mevedel-tool--first-required-arg (tool)
  "Return the keyword for TOOL's first required arg, or nil."
  (let ((args (mevedel-tool-args tool)))
    (cl-loop for arg in args
             when (eq (nth 2 arg) :required)
             return (intern (format ":%s" (car arg))))))

(defun mevedel-tool-display-string (tool-name args)
  "Return a display string for TOOL-NAME with ARGS.
Looks up the tool in the registry and uses its `display-arg' slot.
Returns nil when no meaningful display string can be produced.

When the raw value looks like a file path (contains `/'), it is
abbreviated to the last 3 components automatically."
  (let* ((tool (mevedel-tool-get tool-name))
         (display-arg (and tool (mevedel-tool-display-arg tool)))
         (value
          (cond
           ((functionp display-arg)
            (funcall display-arg args))
           ((keywordp display-arg)
            (plist-get args display-arg))
           ;; Default: first required arg
           (tool
            (when-let* ((key (mevedel-tool--first-required-arg tool)))
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
      (truncate-string-to-width value 60 nil nil "..."))))


;;
;;; Group resolution

(defun mevedel-tool-for-groups (groups)
  "Return `mevedel-tool' structs whose groups intersect GROUPS.

GROUPS is a list of symbols."
  (let (result)
    (maphash (lambda (_key tool)
               (when (cl-intersection groups (mevedel-tool-groups tool))
                 (push tool result)))
             mevedel-tool--registry)
    result))

(defun mevedel-tool-group-exists-p (group)
  "Return non-nil if any registered tool belongs to GROUP."
  (let (found)
    (maphash (lambda (_key tool)
               (when (and (not found)
                          (memq group (mevedel-tool-groups tool)))
                 (setq found t)))
             mevedel-tool--registry)
    found))


;;
;;; Unified tools list resolver

(defvar mevedel-tool--resolve-cache (make-hash-table :test #'equal)
  "Memo table for `mevedel-tool-resolve' keyed on SPECS `equal'.
Invalidated by `mevedel-tool--invalidate-resolve-cache' on every
registration or clear so a stale resolution cannot leak across a
tool being registered or removed.")

(defun mevedel-tool--invalidate-resolve-cache ()
  "Clear the `mevedel-tool-resolve' memo table."
  (clrhash mevedel-tool--resolve-cache))

(defun mevedel-tool-resolve (specs)
  "Resolve a mixed tools list SPECS into `mevedel-tool' structs.

SPECS is a list where each element is one of:
  - SYMBOL          bare symbol, try group first then tool name
  - (:group GROUP)  expand group from registry
  - (:tool NAME)    look up tool by name string or (CATEGORY NAME)
                    path
  - (:deferred X)   mark X as deferred (returns nil, collects into
                    second value)

Returns a plist (:active TOOLS :deferred TOOLS) where each TOOLS is a
list of mevedel-tool structs.  Result is memoized on SPECS `equal' --
resolution is a hot path (every sub-agent spawn re-resolves) and the
tool registry is stable between register/clear events.  The cache is
invalidated at registration boundaries via `mevedel-tool-register'
and `mevedel-tool-clear-registry'."
  (let ((cached (gethash specs mevedel-tool--resolve-cache 'miss)))
    (if (not (eq cached 'miss))
        cached
      (let (active deferred)
        (dolist (spec specs)
          (cond
           ;; (:deferred X) -> resolve X, add to deferred list
           ((and (listp spec) (eq :deferred (car spec)))
            (let ((inner (cadr spec)))
              (dolist (tool (mevedel-tool--resolve-one inner))
                (push tool deferred))))
           ;; Any other form -> resolve and add to active list
           (t
            (dolist (tool (mevedel-tool--resolve-one spec))
              (push tool active)))))
        (let ((result (list :active (nreverse active)
                            :deferred (nreverse deferred))))
          (puthash specs result mevedel-tool--resolve-cache)
          result)))))

(defun mevedel-tool--resolve-one (spec)
  "Resolve a single SPEC into a list of mevedel-tool structs.

SPEC is one of:
  - SYMBOL          bare symbol, try group first then tool name
  - (:group GROUP)  expand group
  - (:tool NAME)    look up by name or path"
  (cond
   ;; (:group GROUP) -> expand group
   ((and (listp spec) (eq :group (car spec)))
    (let ((group (cadr spec)))
      (or (mevedel-tool-for-groups (list group))
          (error "Unknown tool group: %s" group))))
   ;; (:tool NAME-OR-PATH) -> look up single tool
   ((and (listp spec) (eq :tool (car spec)))
    (let* ((name-or-path (cadr spec))
           (tool (if (listp name-or-path)
                     ;; (CATEGORY NAME) path
                     (mevedel-tool-get (cadr name-or-path) (car name-or-path))
                   ;; Plain name string
                   (mevedel-tool-get (if (symbolp name-or-path)
                                         (symbol-name name-or-path)
                                       name-or-path)))))
      (unless tool
        (error "Unknown tool: %s" name-or-path))
      (list tool)))
   ;; Bare symbol -> group-first-then-tool heuristic
   ((symbolp spec)
    (if (mevedel-tool-group-exists-p spec)
        (mevedel-tool-for-groups (list spec))
      (let ((tool (mevedel-tool-get (symbol-name spec))))
        (if tool
            (list tool)
          (error "Unknown group or tool: %s" spec)))))
   (t (error "Invalid tool spec: %S" spec))))

(defun mevedel-tool-resolve-gptel (specs)
  "Resolve SPECS and return `gptel-tool' structs.

Convenience wrapper around `mevedel-tool-resolve' that extracts the
`gptel-tool' back-references.  Returns a plist (:active GPTEL-TOOLS
:deferred GPTEL-TOOLS)."
  (let ((resolved (mevedel-tool-resolve specs)))
    (list :active (mapcar #'mevedel-tool-gptel-tool (plist-get resolved :active))
          :deferred (mapcar #'mevedel-tool-gptel-tool (plist-get resolved :deferred)))))


;;
;;; Args conversion

(defconst mevedel-tool--path-description-suffix
  "Pass a raw filesystem path, not Markdown or a URL."
  "Provider-facing guidance appended to semantic path arguments.")

(defun mevedel-tool--provider-arg-description (type description)
  "Return provider-facing DESCRIPTION for argument TYPE."
  (let ((description (or description "")))
    (if (and (eq type 'path)
             (not (string-search mevedel-tool--path-description-suffix
                                 description)))
        (concat description
                (unless (string-empty-p description) " ")
                mevedel-tool--path-description-suffix)
      description)))

(defun mevedel-tool--args-to-gptel (args)
  "Convert mevedel ARGS spec to gptel plist format.

Mevedel format:
  ((name type :required \"desc\" [extras...]) ...)
  ((name type :optional \"desc\" [extras...]) ...)

Gptel format:
  ((:name \"name\" :type type :description \"desc\" [extras...]) ...)
  with :optional t for optional args.

Any trailing plist keys after the description (e.g. `:items',
`:enum', `:properties') are passed through verbatim to gptel, which
propagates them into the JSON schema.  Required for array/object
types under strict function-schema validation."
  (mapcar
   (lambda (arg-spec)
     (let* ((name (car arg-spec))
            (type (cadr arg-spec))
            (rest (cddr arg-spec))
            (required (eq :required (car rest)))
            (desc (cadr rest))
            (extras (cddr rest))
            (result (list :name (symbol-name name)
                          :type (if (eq type 'path) 'string type)
                          :description
                          (mevedel-tool--provider-arg-description
                           type desc))))
       (unless required
         (setq result (append result (list :optional t))))
       (when extras
         (setq result (append result (copy-tree extras t))))
       result))
   args))

(defconst mevedel-tool--canonical-types
  '(string number integer boolean array object)
  "Canonical mevedel arg types.")

(defun mevedel-tool--normalize-type (raw source-name)
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
    (unless (memq value mevedel-tool--canonical-types)
      (error "Wrapped tool %S has unknown :type %S (expected one of %S)"
             source-name raw mevedel-tool--canonical-types))
    value))

(defun mevedel-tool--normalize-schema (value source-name)
  "Copy VALUE while normalizing nested schema types for SOURCE-NAME."
  (cond
   ((vectorp value)
    (vconcat (mapcar (lambda (item)
                       (mevedel-tool--normalize-schema item source-name))
                     value)))
   ((consp value)
    (let ((copy (mapcar (lambda (item)
                          (mevedel-tool--normalize-schema item source-name))
                        value)))
      (when-let* (((keywordp (car-safe copy)))
                  (raw-type (plist-get copy :type))
                  ((or (symbolp raw-type)
                       (stringp raw-type)
                       (and (consp raw-type) (eq 'quote (car raw-type))))))
        (plist-put copy :type
                   (mevedel-tool--normalize-type raw-type source-name)))
      copy))
   (t value)))

(defun mevedel-tool--args-from-gptel (gptel-args source-name)
  "Convert GPTEL-ARGS plist list to mevedel arg spec format.

SOURCE-NAME is used in error messages when normalisation fails.

Gptel format:
  ((:name \"name\" :type TYPE :description \"...\" [:optional t] ...) ...)
Mevedel format: ((name TYPE :required \"desc\" ...) ...)

Extra keys (`:items', `:enum', `:properties', ...) are preserved as
a trailing plist tail on the mevedel spec element."
  (mapcar
   (lambda (arg)
     (let* ((raw-name (plist-get arg :name))
            (name (cond
                   ((symbolp raw-name) raw-name)
                   ((stringp raw-name) (intern raw-name))
                   (t (error "Wrapped tool %S has unsupported :name %S"
                             source-name raw-name))))
            (type (mevedel-tool--normalize-type
                   (plist-get arg :type) source-name))
            (required (not (plist-get arg :optional)))
            (desc (or (plist-get arg :description) ""))
            (extras (cl-loop for (k v) on arg by #'cddr
                             unless (memq k '(:name :type :description :optional))
                             append
                             (list k (mevedel-tool--normalize-schema
                                      v source-name)))))
       (append (list name type (if required :required :optional) desc)
               extras)))
   gptel-args))

(defmacro mevedel-tool--with-quiet-file-visit (&rest body)
  "Run BODY while suppressing interactive file-visit side effects."
  (declare (indent 0) (debug t))
  `(let ((enable-local-variables :safe)
         (find-file-hook nil)
         (hack-local-variables-hook nil))
     ,@body))

(defun mevedel-tool--call-wrapped-handler (source-category source-name async-p)
  "Return a handler that dispatches to the wrapped source tool.

SOURCE-CATEGORY and SOURCE-NAME identify the source `gptel-tool'.
ASYNC-P reflects the source's :async flag.  The returned function
has mevedel handler shape -- `(callback args-plist)' -- and is
called by `mevedel-pipeline--step-handler'.

The dispatcher does a fresh `gptel-get-tool' lookup on every call
so MCP reconnects (where mcp.el rebuilds the source struct with a
fresh `:function') propagate automatically."
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
             (t
              (let* ((fn (gptel-tool-function source))
                     (arg-specs (mevedel-tool--args-from-gptel
                                 (gptel-tool-args source) source-name))
                     (positional
                      (cl-loop for spec in arg-specs
                               collect (plist-get
                                        args
                                        (intern
                                         (format ":%s" (car spec)))))))
                (mevedel-tool--with-quiet-file-visit
                  (if async-p
                      (apply fn return positional)
                    (funcall return (apply fn positional))))))))
        (error
         (funcall return
                  (format "Error: %s" (error-message-string err))))))))

(defun mevedel-tool-truthy-p (value)
  "Return non-nil if VALUE is a truthy LLM-supplied boolean.

gptel parses JSON false to `:json-false', which is non-nil in Emacs
Lisp.  Use this on every `plist-get' whose key is declared boolean in
a tool schema, otherwise `(if (plist-get args :flag) ...)' wrongly
treats `false' as true."
  (and value (not (eq value :json-false))))

(defun mevedel-tool-string-arg (args key &optional default)
  "Return the string value at KEY in plist ARGS, or DEFAULT.

Returns DEFAULT (nil if omitted) whenever the value is missing,
`:json-false', non-string, or the empty string.  Use this on every
optional string argument fetched from LLM-supplied ARGS to avoid
passing `\"\"' / `:json-false' downstream to shell tools, path
expansion, etc.  Required string arguments are validated upstream
by `mevedel-tool-repair-validate' and do not need this helper."
  (let ((v (plist-get args key)))
    (if (and (stringp v) (not (string-empty-p v)))
        v
      default)))

(defun mevedel-tool-integer-arg (args key &optional default)
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

(defun mevedel-tool--resolve-prompt (prompt)
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


;;
;;; Registration macro

(defmacro mevedel-define-tool (&rest props)
  "Define and register a mevedel tool.

PROPS is a plist with the following keys:

Native form (no :wrap):

Required:
  :name         STRING   Tool name
  :description  STRING   Short LLM-facing description

  :summary      STRING   Optional ultra-short one-liner used by the
                         deferred-tools roster reminder (the roster
                         falls back to listing just the tool name when
                         this is omitted; full descriptions are too
                         long for that listing).

Wrap form (:wrap EXPR):

  :wrap EXPR evaluates at runtime to a `gptel-tool' struct.  The
  wrapped tool's name, args, :async flag, and handler function are
  derived from the source.  Supplying :name, :args, :async-p, or
  :handler alongside :wrap is an error (prevents drift).  :category,
  :description, :prompt, and :prompt-file remain overridable.
  :category defaults to \"mevedel-<source-category>\".

Optional (both forms):
  :handler          FUNCTION     Tool implementation returning a plist with
                                 required `:result' (native only)
  :prompt           STRING-OR-FN Detailed instructions (defaults to
                                 description)
  :prompt-file      STRING       Load prompt from file (relative to mevedel
                                 source dir)
  :args             LIST         Arg specs: ((name type :required \"desc\") ...)
  :repair-input     FN           Pure semantic repair callback receiving
                                 (args-copy validation-issues)
  :category         STRING       Tool category (default \"mevedel\")
  :groups           LIST         Group symbols: (read edit util ...)
  :read-only-p      BOOL         Tool never modifies state
  :destructive-p    BOOL-OR-FN   Needs extra confirmation
  :async-p          BOOL         Handler takes a callback as first arg
                                 (native only)
  :check-permission FN           Custom permission check; sync form returning
                                 `allow'/`deny'/`ask'/nil, may signal
                                 `mevedel-permission-denied' with a reason
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
  :get-pattern      FN           Extract command string from input for
                                 `:pattern' rules (Bash and similar)
  :get-domain       FN           Extract host from input for `:domain' rules
                                 (WebFetch, WebSearch, YouTube)
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

The macro creates a `mevedel-tool' struct, registers it, and calls
`gptel-make-tool' to create the underlying gptel-tool."
  (declare (indent 0) (debug t))
  (let* ((wrap (plist-get props :wrap)))
    (if wrap
        (mevedel-tool--expand-wrap props)
      (mevedel-tool--expand-native props))))

(defun mevedel-tool--expand-native (props)
  "Expand `mevedel-define-tool' PROPS for the native registration form."
  (let* ((name (plist-get props :name))
         (handler (plist-get props :handler))
         (description (plist-get props :description))
         (summary (plist-get props :summary))
         (prompt (plist-get props :prompt))
         (prompt-file (plist-get props :prompt-file))
         (args (plist-get props :args))
         (repair-input (plist-get props :repair-input))
         (category (or (plist-get props :category) "mevedel"))
         (groups (plist-get props :groups))
         (read-only-p (plist-get props :read-only-p))
         (destructive-p (plist-get props :destructive-p))
         (async-p (plist-get props :async-p))
         (check-permission (plist-get props :check-permission))
         (check-permission-async (plist-get props :check-permission-async))
         (get-path (plist-get props :get-path))
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
                                    mevedel-tool-registry--source-dir)))
        (if (file-exists-p path)
            (setq prompt (with-temp-buffer
                           (insert-file-contents path)
                           (buffer-string)))
          (error "Prompt file not found: %s" path))))
    `(let* ((resolved-prompt (mevedel-tool--resolve-prompt
                              ,(or prompt description)))
            (mtool
             (mevedel-tool--create
              :name ,name
              :handler ,handler
              :description ,description
              :summary ,summary
              :prompt resolved-prompt
              :args ',args
              :repair-input ,repair-input
              :category ,category
              :read-only-p ,read-only-p
              :destructive-p ,destructive-p
              :async-p ,async-p
              :check-permission ,check-permission
              :check-permission-async ,check-permission-async
              :get-path ,get-path
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
                          (mevedel-pipeline-run-tool
                           mtool callback
                           (mevedel-pipeline--positional-to-plist
                            raw-args ',(or args nil))))
              :description resolved-prompt
              :args ',(when args (mevedel-tool--args-to-gptel args))
              ;; Always async: the pipeline wrapper uses continuations
              ;; (permission prompts, async handlers).  The handler's
              ;; own async nature is tracked in mevedel-tool :async-p
              ;; and honored by `mevedel-pipeline--step-handler'.
              :async t
              :category ,category)))
       (setf (mevedel-tool-gptel-tool mtool) gptel-tool)
       (mevedel-tool-register mtool))))

(defun mevedel-tool--expand-wrap (props)
  "Expand `mevedel-define-tool' PROPS for the :wrap registration form."
  (let* ((wrap-form (plist-get props :wrap))
         (category-override (plist-get props :category))
         (description-override (plist-get props :description))
         (summary (plist-get props :summary))
         (prompt-override (plist-get props :prompt))
         (prompt-file (plist-get props :prompt-file))
         (groups (plist-get props :groups))
         (repair-input (plist-get props :repair-input))
         (read-only-p (plist-get props :read-only-p))
         (destructive-p (plist-get props :destructive-p))
         (check-permission (plist-get props :check-permission))
         (check-permission-async (plist-get props :check-permission-async))
         (get-path (plist-get props :get-path))
         (get-pattern (plist-get props :get-pattern))
         (get-domain (plist-get props :get-domain))
         (get-name (plist-get props :get-name))
         (max-result-size (plist-get props :max-result-size))
         (display-arg (plist-get props :display-arg))
         (render-transform (plist-get props :render-transform))
         (renderer (plist-get props :renderer)))
    (dolist (k '(:name :args :async-p :handler))
      (when (plist-member props k)
        (error "mevedel-define-tool: %s is derived from :wrap, do not supply"
               k)))
    (when prompt-file
      (let ((path (expand-file-name prompt-file
                                    mevedel-tool-registry--source-dir)))
        (if (file-exists-p path)
            (setq prompt-override
                  (with-temp-buffer
                    (insert-file-contents path)
                    (buffer-string)))
          (error "Prompt file not found: %s" path))))
    `(mevedel-tool--register-wrap
      :source ,wrap-form
      :category-override ,category-override
      :description-override ,description-override
      :summary ,summary
      :prompt-override ,prompt-override
      :groups ',groups
      :repair-input ,repair-input
      :read-only-p ,read-only-p
      :destructive-p ,destructive-p
      :check-permission ,check-permission
      :check-permission-async ,check-permission-async
      :get-path ,get-path
      :get-pattern ,get-pattern
      :get-domain ,get-domain
      :get-name ,get-name
      :max-result-size ,max-result-size
      :display-arg ,display-arg
      :render-transform ,render-transform
      :renderer ,renderer)))

(cl-defun mevedel-tool--register-wrap
    (&key source category-override description-override summary
          prompt-override groups repair-input read-only-p destructive-p
          check-permission check-permission-async
          get-path get-pattern get-domain get-name
          max-result-size display-arg render-transform renderer)
  "Runtime helper: build and register a wrapped tool from SOURCE.

SOURCE must be a `gptel-tool' struct.  CATEGORY-OVERRIDE,
DESCRIPTION-OVERRIDE, SUMMARY, PROMPT-OVERRIDE, GROUPS, REPAIR-INPUT,
READ-ONLY-P, DESTRUCTIVE-P, CHECK-PERMISSION, CHECK-PERMISSION-ASYNC, GET-PATH,
GET-PATTERN, GET-DOMAIN, GET-NAME, MAX-RESULT-SIZE, DISPLAY-ARG,
RENDER-TRANSFORM, and RENDERER mirror `mevedel-define-tool'."
  (unless (gptel-tool-p source)
    (error "`mevedel-define-tool :wrap' expects a gptel-tool, got %S" source))
  (let* ((source-name (gptel-tool-name source))
         (source-category (gptel-tool-category source))
         (target-category (or category-override
                              (format "mevedel-%s" source-category)))
         (mevedel-args (mevedel-tool--args-from-gptel
                        (gptel-tool-args source) source-name))
         (source-async-p (and (gptel-tool-async source) t)))
    (let* ((source-description (gptel-tool-description source))
           (description (or description-override source-description))
           (resolved-prompt (mevedel-tool--resolve-prompt
                             (or prompt-override description)))
           (handler (mevedel-tool--call-wrapped-handler
                     source-category source-name source-async-p))
           (mtool
            (mevedel-tool--create
             :name source-name
             :handler handler
             :description description
             :summary summary
             :prompt resolved-prompt
             :args mevedel-args
             :repair-input repair-input
             :category target-category
             :read-only-p read-only-p
             :destructive-p destructive-p
             :async-p t
             :check-permission check-permission
             :check-permission-async check-permission-async
             :get-path get-path
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
                         (mevedel-pipeline-run-tool
                          mtool callback
                          (mevedel-pipeline--positional-to-plist
                           raw-args mevedel-args)))
             :description resolved-prompt
             :args (mevedel-tool--args-to-gptel mevedel-args)
             :async t
             :include (gptel-tool-include source)
             :category target-category)))
      (setf (mevedel-tool-gptel-tool mtool) gptel-tool)
      (mevedel-tool-register mtool))))

;;
;;; Validation macros

(defmacro mevedel-tools--validate-params (callback function-name &rest param-specs)
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
  (mevedel-tools--validate-params callback my-func
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

(provide 'mevedel-tool-registry)
;;; mevedel-tool-registry.el ends here
