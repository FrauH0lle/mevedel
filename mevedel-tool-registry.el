;;; mevedel-tool-registry.el -- Tool structs and registry -*- lexical-binding: t -*-

;;; Commentary:

;; Declarative tool registration system. Each tool is a mevedel-tool struct with
;; behavioral metadata (read-only, destructive, permissions, groups). The
;; mevedel-define-tool macro creates both the mevedel-tool and the underlying
;; gptel-tool in one declaration.

;;; Code:

(require 'cl-lib)

;; `gptel-request'
(declare-function gptel-make-tool "ext:gptel-request" (&rest slots))
(declare-function gptel-get-tool "ext:gptel-request" (name-or-path &optional noerror))
(declare-function gptel-tool-name "ext:gptel-request" (cl-x) t)
(declare-function gptel-tool-category "ext:gptel-request" (cl-x) t)
(declare-function gptel-tool-function "ext:gptel-request" (cl-x) t)


;;
;;; Tool struct

(cl-defstruct (mevedel-tool (:constructor mevedel-tool--create))
  "A mevedel tool with behavioral metadata.

Parallel struct alongside `gptel-tool'. `mevedel-tool' holds behavioral
metadata (permissions, groups, read-only status). The `gptel-tool' is
created as a side effect of registration and handles serialization."
  name              ; string: "Read", "Edit", "Bash"
  handler           ; function: the actual tool implementation
  description       ; string: short LLM-facing description (for schema, "ToolSearch")
  prompt            ; string or function: detailed instructions
  args              ; arg spec list in mevedel format
  category          ; string: "mevedel" (default)
  ;; Behavioral declarations
  read-only-p       ; t if tool never modifies state
  destructive-p     ; t or function: needs extra confirmation
  async-p           ; t if handler takes a callback
  ;; Permission integration
  check-permission  ; function or nil: (tool-struct input) -> allow|deny|ask
  get-path          ; function or nil: (input) -> path this tool touches
  ;; Groups
  groups            ; list of symbols: (read util code edit eval ...)
  ;; Back-reference
  gptel-tool)       ; the `gptel-tool' struct created during registration


;;
;;; Registry

(defvar mevedel-tool--registry (make-hash-table :test #'equal)
  "Hash-table mapping (CATEGORY NAME) lists to `mevedel-tool' structs.")

(defun mevedel-tool-get (name &optional category)
  "Look up a mevedel-tool by NAME, optionally scoped to CATEGORY.

If CATEGORY is provided, look up by exact (CATEGORY NAME) key. If
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
  (clrhash mevedel-tool--registry))


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
list of mevedel-tool structs."
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
    (list :active (nreverse active)
          :deferred (nreverse deferred))))

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
`gptel-tool' back-references. Returns a plist (:active GPTEL-TOOLS
:deferred GPTEL-TOOLS)."
  (let ((resolved (mevedel-tool-resolve specs)))
    (list :active (mapcar #'mevedel-tool-gptel-tool (plist-get resolved :active))
          :deferred (mapcar #'mevedel-tool-gptel-tool (plist-get resolved :deferred)))))


;;
;;; Args conversion

(defun mevedel-tool--args-to-gptel (args)
  "Convert mevedel ARGS spec to gptel plist format.

Mevedel format:  ((name type :required \"desc\") ...)
                  ((name type :optional \"desc\") ...)

Gptel format:    ((:name \"name\" :type type :description \"desc\") ...)
                  with :optional t for optional args."
  (mapcar
   (lambda (arg-spec)
     (let* ((name (car arg-spec))
            (type (cadr arg-spec))
            (rest (cddr arg-spec))
            (required (eq :required (car rest)))
            (desc (cadr rest))
            (result (list :name (symbol-name name)
                          :type type
                          :description (or desc ""))))
       (unless required
         (setq result (append result (list :optional t))))
       result))
   args))

(defconst mevedel-tool--type-predicates
  '((string  . stringp)
    (integer . integerp)
    (number  . numberp)
    (boolean . mevedel-tool--boolean-p)
    (array   . vectorp)
    (object  . listp))
  "Mapping from JSON schema types to Elisp predicates.")

(defun mevedel-tool--boolean-p (value)
  "Return non-nil if VALUE is a JSON boolean (t or :json-false)."
  (or (eq value t) (eq value :json-false)))

(defun mevedel-tool--validate-args (tool-name args arg-specs)
  "Validate ARGS against ARG-SPECS for TOOL-NAME.

ARGS is a plist of argument values from the LLM. ARG-SPECS is the
mevedel args format list. Returns nil on success, or an error string on
failure."
  (catch 'validation-error
    (dolist (spec arg-specs)
      (let* ((name (car spec))
             (type (cadr spec))
             (rest (cddr spec))
             (required (eq :required (car rest)))
             (value (plist-get args (intern (format ":%s" name))))
             (pred (alist-get type mevedel-tool--type-predicates)))
        (when (and required (null value))
          (throw 'validation-error
                 (format "%s: missing required parameter '%s'" tool-name name)))
        (when (and value pred (not (funcall pred value)))
          (throw 'validation-error
                 (format "%s: parameter '%s' must be %s, got %S"
                         tool-name name type value)))))
    nil))


;;
;;; Prompt resolution

(defun mevedel-tool--resolve-prompt (prompt)
  "Resolve PROMPT to a string.

If PROMPT is a string, return it. If it is a function, call it."
  (cond
   ((stringp prompt) prompt)
   ((functionp prompt) (funcall prompt))
   (t (error "Tool prompt must be a string or function, got %S" prompt))))


;;
;;; Registration macro

(defmacro mevedel-define-tool (&rest props)
  "Define and register a mevedel tool.

PROPS is a plist with the following keys:

Required:
  :name         STRING   Tool name
  :description  STRING   Short LLM-facing description

Optional:
  :handler      FUNCTION  Tool implementation (nil for MCP wrappers)
  :prompt       STRING-OR-FN  Detailed instructions (defaults to description)
  :prompt-file  STRING   Load prompt from file (relative to mevedel source dir)
  :args         LIST     Arg specs: ((name type :required \"desc\") ...)
  :category     STRING   Tool category (default \"mevedel\")
  :groups       LIST     Group symbols: (read edit util ...)
  :read-only-p  BOOL     Tool never modifies state
  :destructive-p BOOL-OR-FN  Needs extra confirmation
  :async-p      BOOL     Handler takes a callback as first arg
  :check-permission FN   Custom permission check
  :get-path     FN       Extract path from input

The macro creates a `mevedel-tool' struct, registers it, and calls
`gptel-make-tool' to create the underlying gptel-tool."
  (declare (indent 0) (debug t))
  (let* ((name (plist-get props :name))
         (handler (plist-get props :handler))
         (description (plist-get props :description))
         (prompt (plist-get props :prompt))
         (prompt-file (plist-get props :prompt-file))
         (args (plist-get props :args))
         (category (or (plist-get props :category) "mevedel"))
         (groups (plist-get props :groups))
         (read-only-p (plist-get props :read-only-p))
         (destructive-p (plist-get props :destructive-p))
         (async-p (plist-get props :async-p))
         (check-permission (plist-get props :check-permission))
         (get-path (plist-get props :get-path)))
    ;; Validate required fields at compile time
    (unless name (error "Tool :name is required"))
    (unless description (error "Tool :description is required"))
    ;; Resolve prompt-file at compile time
    (when prompt-file
      (let ((path (expand-file-name
                   prompt-file
                   (file-name-directory
                    (or byte-compile-current-file load-file-name
                        buffer-file-name)))))
        (if (file-exists-p path)
            (setq prompt (with-temp-buffer
                           (insert-file-contents path)
                           (buffer-string)))
          (error "Prompt file not found: %s" path))))
    `(let* ((gptel-tool
             (gptel-make-tool
              :name ,name
              :function ,handler
              :description ,(or prompt description)
              :args ',(when args (mevedel-tool--args-to-gptel args))
              :async ,async-p
              :category ,category))
            (mtool
             (mevedel-tool--create
              :name ,name
              :handler ,handler
              :description ,description
              :prompt ,(or prompt description)
              :args ',args
              :category ,category
              :read-only-p ,read-only-p
              :destructive-p ,destructive-p
              :async-p ,async-p
              :check-permission ,check-permission
              :get-path ,get-path
              :groups ',groups
              :gptel-tool gptel-tool)))
       (mevedel-tool-register mtool))))

(provide 'mevedel-tool-registry)
;;; mevedel-tool-registry.el ends here
