;;; mevedel-skills.el -- Skills infrastructure -*- lexical-binding: t -*-

;;; Commentary:

;; Skills are reusable, named prompt recipes loaded from SKILL.md files.
;; Each skill bundles task-scoped instructions, tool restrictions, and
;; execution context.  This module owns the data model (discovery,
;; loading, invocation, and the `Skill' tool) while delegating YAML
;; frontmatter parsing to `gptel-agent-read-file'.

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(require 'gptel-agent)
(require 'mevedel-structs)
(require 'mevedel-reminders)
(require 'mevedel-tool-registry)
(require 'mevedel-permissions)
(require 'mevedel-models)

;; `project'
(declare-function project-current "project" (&optional maybe-prompt directory))
(declare-function project-root "project" (project))

;; `text-property-search'
(declare-function text-property-search-backward "text-property-search"
                  (property &optional value predicate not-current))
(declare-function prop-match-end "text-property-search" (match))

;; `mevedel-workspace'
(declare-function mevedel-workspace-root "mevedel-workspace" (workspace) t)

;; `mevedel-tool-ui'
;; Use `t' for the arglist: cl-defun with &key keywords confuses the
;; byte-compiler's arity check (it counts each keyword as one arg
;; rather than a pair), producing spurious "called with N args, accepts
;; only M" warnings.
(declare-function mevedel-tools--task "mevedel-tool-ui" t t)

;; `mevedel-pipeline'
(declare-function mevedel-pipeline-run-tool "mevedel-pipeline"
                  (tool callback args))

;; `mevedel-tool-exec'
(declare-function mevedel-tool-exec--register "mevedel-tool-exec" ())

;; `mevedel-tool-registry'
(declare-function mevedel-tool-get "mevedel-tool-registry" (name &optional category))
(declare-function mevedel-tool-get-path "mevedel-tool-registry" (cl-x) t)

;; `gptel'
(defvar gptel-post-tool-call-functions)

;; `mevedel-compact'
(declare-function mevedel-compact "mevedel-compact" ())
(declare-function mevedel--estimate-tokens "mevedel-compact" ())
(defvar mevedel-compact-context-limit)

;; `gptel'
(declare-function gptel-send "ext:gptel" (&optional arg))
(declare-function gptel--update-status "ext:gptel" (msg &optional face))
(defvar gptel-backend)
(defvar gptel-prompt-prefix-alist)
(defvar gptel-model)
(defvar gptel-post-response-functions)
(defvar gptel-response-separator)

;; `mevedel-models'
(declare-function mevedel-model-parse-selector "mevedel-models" (value))
(declare-function mevedel-model-resolve-selector
                  "mevedel-models" (selector &optional noerror))
(declare-function mevedel-model-apply-provider-to-info
                  "mevedel-models" (info provider))

;; `mevedel-permissions'
(defvar mevedel-permission-mode)

;; `mevedel-structs'
(declare-function mevedel-request-begin "mevedel-structs"
                  (session &optional directive-uuid))
(declare-function mevedel-request-end "mevedel-structs" ())
(declare-function mevedel-session-turn-count "mevedel-structs" (cl-x) t)
(defvar mevedel--session)
(defvar mevedel--current-request)
(defvar mevedel--current-directive-uuid)

;; `mevedel-session-persistence'
(declare-function mevedel-session-persistence-save
                  "mevedel-session-persistence" (session buffer))
(defvar mevedel-session-persistence)
(defvar mevedel-session--read-only-mode)
(defvar mevedel-session--save-failed)


;;
;;; Customization

(defcustom mevedel-skill-dirs
  '("~/.claude/skills/"
    ".claude/skills/"
    ".mevedel/skills/")
  "Directories scanned for SKILL.md files.

Absolute paths and `~'-prefixed paths are scanned as-is and classified
as `user' skills.  Relative paths are resolved against the current
workspace root and classified as `project' skills.  Earlier directories
take precedence when two skills share a name."
  :type '(repeat directory)
  :group 'mevedel)

(defcustom mevedel-skills-include-bundled t
  "When non-nil, `mevedel-skills-scan' also scans the bundled directory.
Bundled skills (e.g. the coordinator skill) ship with mevedel and are
discovered alongside user skills by default.  Set to nil to exclude
them from skill enumeration; tests also let-bind this to nil to assert
on exact user-skill contents without bundled skills leaking in."
  :type 'boolean
  :group 'mevedel)

(defconst mevedel-skills--bundled-dir
  (expand-file-name "skills/" mevedel-tool-registry--source-dir)
  "Absolute path to mevedel's bundled skills directory.
Scanned last by `mevedel-skills-scan' so user-provided skills with
the same name take precedence.")


;;
;;; Skill struct

(cl-defstruct (mevedel-skill (:constructor mevedel-skill--create))
  "A single skill loaded from a SKILL.md file.

NAME is the invocation identifier (the string typed after `/' and
matched by `Skill(name=...)').  Resolution: frontmatter `name' if
present and valid, otherwise the skill directory name.  Must match
`[a-z0-9-]+' and be 1-64 chars.  Invalid skills are skipped at scan
time with a warning.  DISPLAY-NAME is the human-friendly label used
in completion annotations and listing output; defaults to NAME when
the frontmatter omits `display-name'.  DESCRIPTION is the listing
line shown to the model; falls back to the first non-empty
paragraph/header of the body when frontmatter omits `description'.
WHEN-TO-USE is an optional longer trigger description (accepts both
`when_to_use' and `when-to-use', preferring the underscore form on
conflict).  BODY is the skill prompt text; populated lazily on first
invocation.  SOURCE-FILE and SOURCE-DIR point at the SKILL.md and
its containing directory.  SOURCE is a symbol tagging the origin
\\=(`user', `project', `managed', `plugin', `bundled').
USER-INVOCABLE-P and MODEL-INVOCABLE-P gate the `/name' menu and
the skills listing reminder respectively.  CONTEXT is `inline'
\\=(default) or `fork'.  AGENT names the agent type for fork
execution; if omitted, the fork inherits from the immediate parent
invocation.  ALLOWED-TOOLS holds the raw
  frontmatter strings; ALLOWED-TOOL-RULES holds the parsed mevedel
  permission rules.  MODEL names a tier or BACKEND:MODEL provider for
  the request scope.  EFFORT overrides reasoning effort
\\=(parsed and stored; currently inert pending gptel support).
ARGUMENT-HINT annotates completion UI.  ARGUMENT-NAMES holds the
parsed `arguments' frontmatter as a list of names with numeric-only
entries filtered out.  PATH-PATTERNS contains globs that trigger
conditional activation.  SHELL is the shell symbol for body shell
expansion (`bash' default, `powershell' parsed but unsupported).
HOOKS is the raw frontmatter `hooks' value, stored as-is.
ACTIVE-P records the current activation state for path-scoped
skills."
  name
  display-name
  description
  when-to-use
  body
  source-file
  source-dir
  source
  (user-invocable-p t)
  (model-invocable-p t)
  (context 'inline)
  agent
  allowed-tools
  allowed-tool-rules
  model
  effort
  argument-hint
  argument-names
  path-patterns
  shell
  hooks
  active-p)


;;
;;; Plist -> struct construction

(defun mevedel-skills--coerce-bool (val default)
  "Coerce VAL to a boolean, returning DEFAULT when VAL is nil or missing.

Accepts t, nil, strings (\"true\"/\"false\"/\"yes\"/\"no\"), and the
yaml.el false sentinel :false.  Anything else is treated as t."
  (cond
   ((null val) default)
   ((eq val :false) nil)
   ((eq val t) t)
   ((stringp val)
    (pcase (downcase val)
      ((or "false" "no" "off" "nil") nil)
      (_ t)))
   (t t)))

(defun mevedel-skills--coerce-list (val)
  "Return VAL as a list.  Handles nil, strings, and existing lists."
  (cond
   ((null val) nil)
   ((listp val) val)
   ((stringp val) (list val))
   (t (list val))))

(defun mevedel-skills--coerce-context (val)
  "Return VAL as a context symbol (`inline' or `fork')."
  (pcase val
    ((or 'fork "fork" :fork) 'fork)
    (_ 'inline)))

(defconst mevedel-skills--valid-efforts '(low medium high xhigh max)
  "Effort values accepted on the `effort' frontmatter field.")

(defconst mevedel-skills--name-regexp "\\`[a-z0-9-]+\\'"
  "Regexp matching valid skill invocation identifiers.")

(defconst mevedel-skills--name-max-length 64
  "Maximum allowed length for a skill invocation identifier.")

(defun mevedel-skills--valid-name-p (name)
  "Return non-nil when NAME is a valid skill invocation identifier.
spec Data Model: must match `[a-z0-9-]+' and be 1-64 chars.
Case-sensitive -- `Bad' is rejected."
  (and (stringp name)
       (<= 1 (length name) mevedel-skills--name-max-length)
       (let ((case-fold-search nil))
         (string-match-p mevedel-skills--name-regexp name))))

(defun mevedel-skills--parse-argument-names (val)
  "Parse VAL into a list of argument names per spec
Accepts a space-separated string or a YAML list.  Numeric-only
entries are filtered out so they cannot shadow `$0'/`$1'/etc."
  (let ((names (cond
                ((null val) nil)
                ((listp val) (cl-remove-if-not #'stringp val))
                ((stringp val) (split-string val "[ \t]+" t))
                (t nil))))
    (cl-remove-if (lambda (n)
                    (or (string-empty-p n)
                        (string-match-p "\\`[0-9]+\\'" n)))
                  names)))

(defun mevedel-skills--validate-effort (val source-file)
  "Validate effort VAL.  Return symbol or nil.
Emits a warning and returns nil for unrecognized values.
SOURCE-FILE identifies the offending skill in the warning."
  (when val
    (let ((sym (cond ((symbolp val) val)
                     ((stringp val) (intern val))
                     (t nil))))
      (cond
       ((memq sym mevedel-skills--valid-efforts) sym)
       (t
        (display-warning
         'mevedel
         (format "Skill at %s declares unknown effort %S; ignoring"
                 source-file val)
         :warning)
        nil)))))

(defun mevedel-skills--validate-shell (val source-file)
  "Validate shell VAL.  Returns `bash', `powershell', or `bash' on error.
Defaults to `bash' when VAL is nil.  SOURCE-FILE identifies the
offending skill in the warning."
  (let ((sym (cond ((null val) 'bash)
                   ((symbolp val) val)
                   ((stringp val) (intern val))
                   (t nil))))
    (cond
     ((memq sym '(bash powershell)) sym)
     (t
      (display-warning
       'mevedel
       (format "Skill at %s declares unknown shell %S; using bash"
               source-file val)
       :warning)
      'bash))))

(declare-function mevedel-permission--parse-rule-string
                  "mevedel-permissions" (entry))

(defun mevedel-skills--parse-allowed-tool-rules (entries source-file)
  "Map each ENTRY through `mevedel-permission--parse-rule-string'.

ENTRIES is the raw `allowed-tools' frontmatter list.  Returns a
list of parsed mevedel permission rules.  Per the detail-spec
section \"Validation at skill load\", a malformed entry aborts the whole
skill load: this function signals `user-error' on the first bad
entry, and `mevedel-skills--build-skill''s `condition-case' skips
the offending skill with a warning naming SOURCE-FILE."
  (mapcar (lambda (entry)
            (condition-case err
                (mevedel-permission--parse-rule-string entry)
              (user-error
               (user-error
                "Malformed allowed-tools entry %S in %s: %s"
                entry source-file (error-message-string err)))))
          entries))

(defun mevedel-skills--first-paragraph (body)
  "Return the first non-empty paragraph or header from BODY, or nil.
Used as the description fallback per spec Failure Modes
\\='Missing description'.  Strips leading `#' header characters and
surrounding whitespace.  Returns nil when BODY is nil or contains
no non-blank text."
  (when (stringp body)
    (with-temp-buffer
      (insert body)
      (goto-char (point-min))
      ;; Skip leading blank lines.
      (while (and (not (eobp))
                  (looking-at-p "[ \t]*$"))
        (forward-line 1))
      (when (not (eobp))
        (let ((start (point)))
          (while (and (not (eobp))
                      (not (looking-at-p "[ \t]*$")))
            (forward-line 1))
          (let* ((para (buffer-substring-no-properties start (point)))
                 (stripped (replace-regexp-in-string
                            "\\`[ \t#]+" "" para))
                 (trimmed (string-trim stripped)))
            (and (not (string-empty-p trimmed)) trimmed)))))))

(defun mevedel-skills--directory-name (skill-file)
  "Return the directory name of SKILL-FILE.
For `/path/to/grill-me/SKILL.md' returns `grill-me'."
  (file-name-nondirectory
   (directory-file-name (file-name-directory skill-file))))

(defun mevedel-skills--parse-frontmatter (skill-file)
  "Parse SKILL-FILE's YAML frontmatter and return a plist.
Unlike `gptel-agent-read-file', preserves the `:name' key in the
plist so callers can decide whether to honor it or fall back to
the directory name.  Returns nil on read/parse failure; emits a
warning when YAML parsing fails per spec Failure Modes
\\='Invalid YAML'."
  (when (and (file-readable-p skill-file)
             (file-regular-p skill-file))
    (condition-case err
        (gptel-agent-parse-markdown-frontmatter skill-file nil nil t)
      (error
       (display-warning
        'mevedel
        (format "Skill at %s has invalid YAML: %s; skipping"
                skill-file (error-message-string err))
        :warning)
       nil))))

(defun mevedel-skills--from-plist (name plist source-file source)
  "Build a `mevedel-skill' from NAME and PLIST parsed from SOURCE-FILE.
SOURCE is the origin tag symbol.  NAME is the resolved invocation
identifier (already validated by the caller).  Description fallback
from the body is the caller's responsibility -- anything in PLIST's
`:description' wins over the fallback."
  (let* ((description (plist-get plist :description))
         (display-name (plist-get plist :display-name))
         ;; spec Data Model: prefer when_to_use over when-to-use
         (when-to-use (or (plist-get plist :when_to_use)
                          (plist-get plist :when-to-use)))
         (disable-model (plist-get plist :disable-model-invocation))
         (user-invocable (plist-get plist :user-invocable))
         (context (plist-get plist :context))
         (allowed-tools (plist-get plist :allowed-tools))
         (agent (plist-get plist :agent))
         (model (plist-get plist :model))
         (effort (plist-get plist :effort))
         (argument-hint (plist-get plist :argument-hint))
         (arguments (plist-get plist :arguments))
         (paths (plist-get plist :paths))
         (shell (plist-get plist :shell))
         (hooks (plist-get plist :hooks)))
    (mevedel-skill--create
     :name name
     :display-name (or (and (stringp display-name) display-name) name)
     :description (and (stringp description) description)
     :when-to-use (and (stringp when-to-use) when-to-use)
     :source-file source-file
     :source-dir (file-name-directory source-file)
     :source source
     :user-invocable-p (mevedel-skills--coerce-bool user-invocable t)
     :model-invocable-p (not (mevedel-skills--coerce-bool disable-model nil))
     :context (mevedel-skills--coerce-context context)
     :agent (and (stringp agent) agent)
     :allowed-tools (mevedel-skills--coerce-list allowed-tools)
     :allowed-tool-rules
     (mevedel-skills--parse-allowed-tool-rules
      (mevedel-skills--coerce-list allowed-tools) source-file)
     :model (and (stringp model) model)
     :effort (mevedel-skills--validate-effort effort source-file)
     :argument-hint (and (stringp argument-hint) argument-hint)
     :argument-names (mevedel-skills--parse-argument-names arguments)
     :path-patterns (mevedel-skills--coerce-list paths)
     :shell (mevedel-skills--validate-shell shell source-file)
     :hooks hooks
     :active-p (null paths))))


;;
;;; Discovery

(defun mevedel-skills--resolve-dir (dir workspace-root)
  "Resolve DIR against WORKSPACE-ROOT, returning (ABSOLUTE . SOURCE).
SOURCE is `user' for absolute/`~'-prefixed paths and `project' for
relative paths resolved against WORKSPACE-ROOT.  Returns nil if DIR is
relative and WORKSPACE-ROOT is nil."
  (cond
   ((file-name-absolute-p dir)
    (cons (file-name-as-directory (expand-file-name dir)) 'user))
   (workspace-root
    (cons (file-name-as-directory
           (expand-file-name dir workspace-root))
          'project))))

(defun mevedel-skills--load-body-string (skill-file)
  "Return the markdown body of SKILL-FILE as a string, or nil.
Used during scan to compute the description fallback when frontmatter
omits `description'."
  (let ((parsed (ignore-errors (gptel-agent-read-file skill-file))))
    (plist-get (cdr parsed) :system)))

(defun mevedel-skills--build-skill (skill-file source)
  "Build a `mevedel-skill' for SKILL-FILE with SOURCE origin tag.
Performs name resolution (frontmatter `:name' > directory name) and
validation per spec  Returns nil with a warning when the skill
is invalid (bad name, etc.).  Computes description fallback from the
body when frontmatter omits `description'."
  (let ((plist (mevedel-skills--parse-frontmatter skill-file)))
    (when plist
      (let* ((dir-name (mevedel-skills--directory-name skill-file))
             (frontmatter-name (plist-get plist :name))
             (raw-name (or (and (stringp frontmatter-name) frontmatter-name)
                           dir-name)))
        (cond
         ((not (mevedel-skills--valid-name-p raw-name))
          (display-warning
           'mevedel
           (format "Skill at %s has invalid name %S; skipping (must match %s, max %d chars)"
                   skill-file raw-name
                   mevedel-skills--name-regexp
                   mevedel-skills--name-max-length)
           :warning)
          nil)
         (t
          ;; Description fallback: read body if frontmatter omits it.
          (unless (and (stringp (plist-get plist :description))
                       (not (string-empty-p (plist-get plist :description))))
            (when-let* ((body (mevedel-skills--load-body-string skill-file))
                        (fallback (mevedel-skills--first-paragraph body)))
              (setq plist (plist-put plist :description fallback))))
          (mevedel-skills--from-plist raw-name plist skill-file source)))))))

(defun mevedel-skills--scan-dir (dir source)
  "Return a list of `mevedel-skill' structs found under DIR.
SOURCE is the origin tag applied to every skill scanned from DIR.
Each SKILL.md is wrapped in `condition-case' so a single bad skill
does not abort the whole scan."
  (when (file-directory-p dir)
    (let (result)
      (dolist (skill-file (directory-files-recursively
                           dir "\\`SKILL\\.md\\'" nil nil t))
        (let ((skill (condition-case err
                         (mevedel-skills--build-skill skill-file source)
                       (error
                        (display-warning
                         'mevedel
                         (format "Skill at %s failed to load: %s"
                                 skill-file (error-message-string err))
                         :warning)
                        nil))))
          (when skill
            (push skill result))))
      (nreverse result))))

(defun mevedel-skills-scan (&optional workspace-root dirs)
  "Scan skill directories and return a list of `mevedel-skill' structs.

DIRS defaults to `mevedel-skill-dirs'.  WORKSPACE-ROOT is used to
resolve relative entries; when nil, relative entries are skipped.
Earlier directories take precedence when two skills share a name.
After scanning user/project directories, mevedel's bundled skills
directory is scanned last so user skills can shadow bundled ones by
name."
  (let ((dirs (or dirs mevedel-skill-dirs))
        (seen (make-hash-table :test #'equal))
        result)
    (dolist (raw dirs)
      (when-let* ((resolved (mevedel-skills--resolve-dir raw workspace-root))
                  (dir (car resolved))
                  (source (cdr resolved)))
        (dolist (skill (mevedel-skills--scan-dir dir source))
          (let ((name (mevedel-skill-name skill)))
            (unless (gethash name seen)
              (puthash name t seen)
              (push skill result))))))
    (when (and mevedel-skills-include-bundled
               (file-directory-p mevedel-skills--bundled-dir))
      (dolist (skill (mevedel-skills--scan-dir
                      mevedel-skills--bundled-dir 'bundled))
        (let ((name (mevedel-skill-name skill)))
          (unless (gethash name seen)
            (puthash name t seen)
            (push skill result)))))
    (nreverse result)))


;;
;;; Lazy body loading

(defun mevedel-skill-load-body (skill)
  "Populate SKILL's body slot from its SKILL.md and return the body.

Reads the full markdown body (frontmatter stripped) on first call via
`gptel-agent-read-file' and caches the result on the struct.  Subsequent
calls return the cached string without touching the filesystem.  Returns
nil if the file has no body text or cannot be read."
  (or (mevedel-skill-body skill)
      (when-let* ((file (mevedel-skill-source-file skill))
                  (parsed (ignore-errors (gptel-agent-read-file file)))
                  (body (plist-get (cdr parsed) :system))
                  ((not (string-blank-p body))))
        (setf (mevedel-skill-body skill) body)
        body)))


;;
;;; Session installation

(defun mevedel-skills-install (session)
  "Populate SESSION's skills slot by scanning `mevedel-skill-dirs'.

Uses SESSION's workspace root to resolve relative directory entries.
Idempotent: existing entries on SESSION's skills slot are replaced."
  (let* ((ws (mevedel-session-workspace session))
         (root (and ws (mevedel-workspace-root ws))))
    (setf (mevedel-session-skills session)
          (mevedel-skills-scan root))
    session))

(defun mevedel-session-get-skill (session name)
  "Return the skill named NAME from SESSION, or nil if not found."
  (cl-find name (mevedel-session-skills session)
           :key #'mevedel-skill-name :test #'equal))


;;
;;; Request-scoped skill context

(defvar-local mevedel-skills--pending-request-context nil
  "Buffer-local pending request context for the next mevedel-request.

A plist of the form
  (:permission-rules RULES :model MODEL :effort EFFORT
   :invoked-skills SKILLS)

populated by slash-dispatched skill invocation before `gptel-send'
fires.  Drained into the new `mevedel-request' slots by the
WAIT-state begin handler in `mevedel-presets.el' (see also
`mevedel-skills--drain-pending-context').

Cleared on drain.  Cleared by an `unwind-protect' in the slash
dispatch path if `gptel-send' aborts before request creation.")

(put 'mevedel-skills--pending-request-context 'permanent-local t)

(defcustom mevedel-skills-max-recursion-depth 4
  "Maximum nesting depth for skill invocations.
A skill body that invokes another skill increments the depth;
exceeding this limit fails the inner invocation with an error
outcome.  Default of 4 allows skill A -> B -> C -> D before failing.
spec."
  :type 'integer
  :group 'mevedel)

(defvar mevedel-skills--invoke-depth 0
  "Dynamic depth of nested `mevedel-skills-invoke' calls.
Let-bound around each invocation so the depth naturally pops on
control-flow exit (return, error, throw, abort).  spec")

(declare-function mevedel-request-skill-permission-rules
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-request-skill-model-override
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-request-skill-effort-override
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-invoked-skills
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-agent-invocation-skill-permission-rules
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-skill-model-override
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-skill-effort-override
                  "mevedel-agents" (cl-x) t)
(defvar mevedel--current-request)
(defvar mevedel--agent-invocation)

(defun mevedel-skills--current-invocation ()
  "Return the active sub-agent invocation, or nil.
Reads the buffer-local `mevedel--agent-invocation' set by
`mevedel-agent-exec--allocate-agent-buffer' on agent buffers;
returns nil when called outside any sub-agent."
  (and (boundp 'mevedel--agent-invocation)
       mevedel--agent-invocation))

(defun mevedel-skills--current-request ()
  "Return the active request struct, or nil."
  (and (boundp 'mevedel--current-request)
       mevedel--current-request))

(defun mevedel-skills--current-model-override ()
  "Return the active skill model override, or nil.
Checks the active sub-agent invocation first (innermost wins), then
the request struct.  Used by the WAIT-state apply handler to swap
`info :backend' and `info :model' on the next gptel iteration."
  (or (when-let* ((inv (mevedel-skills--current-invocation)))
        (mevedel-agent-invocation-skill-model-override inv))
      (when-let* ((req (mevedel-skills--current-request)))
        (mevedel-request-skill-model-override req))))

(defun mevedel-skills--pre-realize-model-override ()
  "Return the model selector visible before gptel realizes request data.

Checks active invocation/request overrides first, then the pending
slash/inline skill stash that has not yet been drained into a request."
  (or (mevedel-skills--current-model-override)
      (plist-get mevedel-skills--pending-request-context :model)))

(defun mevedel-skills--model-selector (skill)
  "Return SKILL's parsed model selector, or nil."
  (when-let* ((model (mevedel-skill-model skill)))
    (mevedel-model-parse-selector model)))

(defun mevedel-skills--drain-pending-context (request)
  "Drain `mevedel-skills--pending-request-context' (buffer-local) into REQUEST.

After this call the buffer-local stash is nil.  No-op when no stash
is present.

The stash plist keys map onto the request slots:

- :permission-rules -> `mevedel-request-skill-permission-rules'
- :model            -> `mevedel-request-skill-model-override'
- :effort           -> `mevedel-request-skill-effort-override'
- :invoked-skills   -> appended to `mevedel-session-invoked-skills'
                       on the request's session"
  (when-let* ((ctx mevedel-skills--pending-request-context))
    (when-let* ((rules (plist-get ctx :permission-rules)))
      (setf (mevedel-request-skill-permission-rules request) rules))
    (when-let* ((model (plist-get ctx :model)))
      (setf (mevedel-request-skill-model-override request) model))
    (when-let* ((effort (plist-get ctx :effort)))
      (setf (mevedel-request-skill-effort-override request) effort))
    (when-let* ((skills (plist-get ctx :invoked-skills))
                (session mevedel--session))
      (setf (mevedel-session-invoked-skills session)
            (append (mevedel-session-invoked-skills session) skills)))
    (setq-local mevedel-skills--pending-request-context nil)))

(declare-function gptel-fsm-info "ext:gptel-request" (cl-x) t)

(defun mevedel-skills--transform-apply-model-override (fsm)
  "Pre-realize transform: apply skill model overrides to prompt locals.

gptel realizes request payloads from the temp prompt buffer's
buffer-local `gptel-backend' and `gptel-model'.  Applying the override
here lets cross-backend skill pins build backend-correct request data.
Post-realize model-side overrides remain handled by
`mevedel-skills--apply-overrides-handler'."
  (let* ((info (gptel-fsm-info fsm))
         (chat-buffer (plist-get info :buffer)))
    (when (and chat-buffer (buffer-live-p chat-buffer))
      (when-let* ((override (with-current-buffer chat-buffer
                              (mevedel-skills--pre-realize-model-override)))
                  (provider (mevedel-model-resolve-selector override t)))
        (setq-local gptel-backend (plist-get provider :backend))
        (setq-local gptel-model (plist-get provider :model))))))

(defun mevedel-skills--apply-overrides-handler (fsm)
  "WAIT-state handler: apply post-realize skill model overrides to FSM info.

Reads the active model override (from sub-agent invocation or
request) and mutates `info :backend' and `info :model' so the
upcoming gptel-request fires with the override.  No-op when no
override is in effect.

This is a post-realize safety rail for model-side skill invocations
that arrive during an already-running tool loop.  Same-backend model
swaps are allowed; cross-backend swaps are rejected by
`mevedel-model-apply-provider-to-info' because `info' :data is already
backend-specific.

spec.  Effort is parsed and
stored on the same slot but not applied here -- gptel does not yet
expose an effort knob.  When it does, this handler will mutate the
corresponding info key the same way."
  (let* ((info (gptel-fsm-info fsm))
         (chat-buffer (plist-get info :buffer)))
    (when (and chat-buffer (buffer-live-p chat-buffer))
      (with-current-buffer chat-buffer
        (when-let* ((override (mevedel-skills--current-model-override))
                    (provider (mevedel-model-resolve-selector override t)))
          (setf (gptel-fsm-info fsm)
                (mevedel-model-apply-provider-to-info info provider)))))))


;;
;;; Argument tokenization

(defun mevedel-skills--parse-arguments (arguments)
  "Parse ARGUMENTS into a list of tokens, shell-style.
Returns nil when ARGUMENTS is nil or blank.  Falls back to
whitespace splitting when shell parsing fails (unbalanced quotes
etc.).  Empty tokens that can fall out of leading/trailing
whitespace are filtered.  Ports the parsing half of ccs's
argumentSubstitution.ts."
  (cond
   ((null arguments) nil)
   ((not (stringp arguments)) nil)
   ((string-blank-p arguments) nil)
   (t
    (cl-remove-if #'string-empty-p
                  (condition-case nil
                      (split-string-and-unquote arguments)
                    (error
                     (split-string arguments "[ \t\n]+" t)))))))

;;
;;; Variable substitution

(defun mevedel-skills--word-char-p (ch)
  "Return non-nil when CH is a word character (`[A-Za-z0-9_]')."
  (and ch
       (or (and (>= ch ?a) (<= ch ?z))
           (and (>= ch ?A) (<= ch ?Z))
           (and (>= ch ?0) (<= ch ?9))
           (eq ch ?_))))

(defun mevedel-skills--substitute-named (text name value)
  "Replace `$NAME' with VALUE in TEXT, strict word-boundary matching.

Skips `$NAME[...]' (indexed access form) and `$NAMEident' (longer
identifier).  Emulates ccs's `\\=$NAME(?![\\=[\\=w])' regex.
Case-sensitive."
  (let ((case-fold-search nil))
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (let ((target (concat "$" name)))
        (while (search-forward target nil t)
          (let ((next (char-after)))
            (cond
             ;; Followed by [ -> indexed-access form, skip
             ((eq next ?\[) nil)
             ;; Followed by word char -> longer identifier, skip
             ((mevedel-skills--word-char-p next) nil)
             (t
              (replace-match value t t))))))
      (buffer-string))))

(defun mevedel-skills--substitute-shorthand (text parsed-args)
  "Replace `$N' shorthand with PARSED-ARGS[N] (zero-based) in TEXT.

Strict word-boundary: `$1' followed by a word char (e.g. `$1foo') is
not substituted.  `$ARGUMENTS' starts with `A' (a word char following
the `$') so this regex naturally skips it.  Indices out of range are
substituted with the empty string.  Case-sensitive."
  (let ((case-fold-search nil))
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (while (re-search-forward "\\$\\([0-9]+\\)" nil t)
        (let ((next (char-after)))
          (unless (mevedel-skills--word-char-p next)
            (let* ((idx (string-to-number (match-string 1)))
                   (val (or (nth idx parsed-args) "")))
              (replace-match val t t)))))
      (buffer-string))))

(defun mevedel-skills--substitute-vars (text arguments session skill)
  "Return TEXT with skill placeholders expanded.

Algorithm ports ccs's `argumentSubstitution.ts'.  Substitution
order (zero-based throughout):

1. Named arguments from SKILL's `argument-names' slot, mapping
   ARGUMENT-NAMES[i] -> PARSED-ARGS[i].
2. `$ARGUMENTS[N]'.
3. `$N' shorthand.
4. `$ARGUMENTS' (the raw argument string).
5. `${CLAUDE_SESSION_ID}' / `${CLAUDE_SKILL_DIR}' (mevedel-specific,
   not part of the ccs algorithm; substituted after the
   placeholder-substituted check below).

If ARGUMENTS is non-empty AND none of steps 1-4 substituted
anything, append `\\nARGUMENTS: <raw>' so the body still receives
the user's input.

Named-argument matching uses strict word-boundary semantics so
`$foo' does not match `$foo[0]' or `$foobar'.  Numeric-only
argument names are filtered out at scan time
\\=(see `mevedel-skills--parse-argument-names') so they cannot
shadow `$0'/`$1' shorthand."
  (let* ((session-id (and session (mevedel-session-name session)))
         (skill-dir (and skill (mevedel-skill-source-dir skill)))
         (argument-names (and skill (mevedel-skill-argument-names skill)))
         (raw-args arguments)
         (parsed-args (mevedel-skills--parse-arguments raw-args))
         (full (or raw-args ""))
         (original text)
         (result text))
    ;; 1. Named arguments.
    (cl-loop for name in argument-names
             for i from 0
             for value = (or (nth i parsed-args) "")
             do (setq result
                      (mevedel-skills--substitute-named result name value)))
    ;; 2. $ARGUMENTS[N].
    (setq result (replace-regexp-in-string
                  "\\$ARGUMENTS\\[\\([0-9]+\\)\\]"
                  (lambda (m)
                    (or (nth (string-to-number (match-string 1 m))
                             parsed-args)
                        ""))
                  result t))
    ;; 3. $N shorthand.
    (setq result (mevedel-skills--substitute-shorthand result parsed-args))
    ;; 4. $ARGUMENTS (full).
    (setq result (replace-regexp-in-string
                  "\\$ARGUMENTS" full result t t))
    ;; Decide append-fallback BEFORE the mevedel-specific ${...} subs
    ;; so they don't influence the "no placeholder substituted" check.
    (let ((args-substituted (not (string= result original))))
      ;; 5. ${CLAUDE_SESSION_ID} / ${CLAUDE_SKILL_DIR}.
      (setq result (replace-regexp-in-string
                    (regexp-quote "${CLAUDE_SESSION_ID}")
                    (or session-id "") result t t))
      (setq result (replace-regexp-in-string
                    (regexp-quote "${CLAUDE_SKILL_DIR}")
                    (or skill-dir "") result t t))
      ;; 6. Append-fallback: only when args were supplied AND non-empty
      ;; AND nothing was substituted.
      (when (and (not args-substituted)
                 (stringp raw-args)
                 (not (string-empty-p raw-args)))
        (setq result (concat result "\n\nARGUMENTS: " raw-args))))
    result))


;;
;;; Shell injection

(declare-function mevedel-tools--check-bash-permission "mevedel-tool-exec"
                  (command &key trust-literal-p))

(define-error 'mevedel-skills-shell-abort
  "Skill body shell expansion failed; skill must abort.")

(defun mevedel-skills--shell-outcome-error-p (result)
  "Return non-nil when Bash pipeline RESULT means shell expansion failed."
  (and (stringp result)
       (or (string-prefix-p "Error:" result)
           (string-prefix-p "Command failed with exit code" result)
           (string-prefix-p "Failed to start process:" result))))

(defun mevedel-skills--run-shell-command-async (command marker callback)
  "Run COMMAND through the Bash tool pipeline, then call CALLBACK.

CALLBACK receives either (:status ok :output STRING) or
(:status error :reason SYMBOL :message STRING).  MARKER is the
original shell-injection marker used in diagnostics."
  (let ((tool (or (ignore-errors (mevedel-tool-get "Bash"))
                  (progn
                    (require 'mevedel-tool-exec)
                    (mevedel-tool-exec--register)
                    (ignore-errors (mevedel-tool-get "Bash"))))))
    (cond
     ((null tool)
      (funcall callback
               `(:status error :reason shell-failure
                         :message "Bash tool is not registered.")))
     (t
      (condition-case err
          (progn
            (unless (fboundp 'mevedel-tools--current-deferred-context)
              (require 'mevedel-tools))
            (mevedel-pipeline-run-tool
             tool
             (lambda (result)
               (cond
                ((and (stringp result)
                      (string-prefix-p "Error: Permission denied" result))
                 (funcall callback
                          `(:status error :reason permission-denied
                                    :message ,(format "Shell expansion %s denied: %s"
                                                      marker result))))
                ((mevedel-skills--shell-outcome-error-p result)
                 (funcall callback
                          `(:status error :reason shell-failure
                                    :message ,(format "Shell expansion %s failed: %s"
                                                      marker result))))
                (t
                 (funcall callback
                          `(:status ok :output ,(string-trim-right
                                                 (or result "")))))))
             (list :command command :trust-literal-p t)))
        (error
         (funcall callback
                  `(:status error :reason shell-failure
                            :message ,(format "Shell expansion %s errored: %s"
                                              marker
                                              (error-message-string err))))))))))

(defun mevedel-skills--shell-match (text)
  "Return the next shell-injection match in TEXT.

The return value is a plist with :start, :end, :command, and
:marker, or nil when TEXT contains no shell-injection marker."
  (let ((fenced nil)
        (inline nil))
    (when (string-match "\\(^\\|\n\\)```!\n\\(\\(?:.\\|\n\\)*?\\)\n```\\(\n\\|\\'\\)" text)
      (setq fenced
            (list :start (match-beginning 0)
                  :end (match-end 0)
                  :command (match-string 2 text)
                  :marker "(fenced block)"
                  :prefix (match-string 1 text)
                  :suffix (match-string 3 text))))
    (when (string-match "!`\\([^`\n]*\\)`" text)
      (setq inline
            (list :start (match-beginning 0)
                  :end (match-end 0)
                  :command (match-string 1 text)
                  :marker (format "!`%s`" (match-string 1 text)))))
    (cond
     ((and fenced inline)
      (if (< (plist-get fenced :start) (plist-get inline :start))
          fenced
        inline))
     (fenced fenced)
     (inline inline))))

(defun mevedel-skills--run-shell-injections-async (text callback)
  "Replace shell-injection markers in TEXT, then call CALLBACK.

CALLBACK receives either (:status ok :body STRING) or
(:status error :reason SYMBOL :message STRING).

Supported markers:
- !`COMMAND`          inline: run COMMAND, substitute stdout
- ```!\\nSCRIPT\\n``` fenced block: run SCRIPT as a shell script

Each command goes through the Bash tool pipeline with
`:trust-literal-p t', so permission checking, process execution, and
oversized-result persistence stay aligned with normal Bash tool
  execution."
  (if-let* ((match (mevedel-skills--shell-match text)))
      (let ((start (plist-get match :start))
            (end (plist-get match :end))
            (command (plist-get match :command))
            (marker (plist-get match :marker))
            (prefix (or (plist-get match :prefix) ""))
            (suffix (or (plist-get match :suffix) ""))
            (origin-buffer (current-buffer)))
        (mevedel-skills--run-shell-command-async
         command marker
         (lambda (outcome)
           (if (not (buffer-live-p origin-buffer))
               (funcall callback
                        `(:status error :reason aborted
                                  :message "Skill buffer was killed during shell expansion."))
             (with-current-buffer origin-buffer
               (pcase (plist-get outcome :status)
                 ('ok
                  (mevedel-skills--run-shell-injections-async
                   (concat (substring text 0 start)
                           prefix
                           (plist-get outcome :output)
                           suffix
                           (substring text end))
                   callback))
                 (_
                  (funcall callback outcome))))))))
    (funcall callback `(:status ok :body ,text))))



;;
;;; Unified skill invocation API

(defun mevedel-skills--display-event (display-callback event)
  "Funcall DISPLAY-CALLBACK with EVENT, ignoring errors.
DISPLAY-CALLBACK may be nil; EVENT is a lifecycle event plist
\\=."
  (when display-callback
    (condition-case err
        (funcall display-callback event)
      (error
       (display-warning
        'mevedel
        (format "Skill display-callback error: %s"
                (error-message-string err))
        :warning)))))

(defun mevedel-skills--invoke-error (skill reason message
                                           callback display-callback)
  "Fire the error-outcome path: emit error event, deliver outcome plist."
  (let ((skill-name (and skill (mevedel-skill-name skill))))
    (mevedel-skills--display-event
     display-callback
     `(:event error :skill ,skill-name
              :reason ,reason :message ,message))
    (funcall callback
             `(:status error :reason ,reason :message ,message))))

(defun mevedel-skills--invoke-done (skill outcome callback display-callback)
  "Fire the success path: emit done event, deliver outcome plist."
  (let ((skill-name (and skill (mevedel-skill-name skill))))
    (mevedel-skills--display-event
     display-callback
     `(:event done :skill ,skill-name))
    (funcall callback outcome)))

(cl-defun mevedel-skills--activate-context
    (trigger &key permission-rules model effort invoked-skill)
  "Apply skill-scoped overrides to the active context.

TRIGGER selects the install path:

- `user-slash': append onto the buffer-local pending stash
  (`mevedel-skills--pending-request-context'); drained at request
  begin by the WAIT-state begin handler in `mevedel-presets.el'.
  Used because slash dispatch fires before the `mevedel-request'
  has been created.
- `model-skill' / `internal': mutate the active sub-agent
  invocation (innermost) or request directly.

PERMISSION-RULES is a list of parsed mevedel rules to append.
MODEL is a selector plist or nil.  EFFORT is a symbol or nil (currently
inert).  INVOKED-SKILL is a `mevedel-skill-invocation-record' to
record on the session for compaction/replay.

Emits one-time per-invocation `display-warning' notices when MODEL
or EFFORT is set so skill authors know:
- model: the override is being installed; verify the provider or tier is
  configured with gptel.
- effort: stored but currently has no observable effect (gptel
  does not yet expose an effort knob)."
  (when model
    (display-warning
     'mevedel
     (format "Skill model override %S installed; verify the provider or tier is configured with gptel."
             model)
     :debug))
  (when effort
    (display-warning
     'mevedel
     (format "Skill effort override %S stored but currently inert (gptel does not yet expose an effort knob)."
             effort)
     :debug))
  (cond
   ((eq trigger 'user-slash)
    (let ((existing mevedel-skills--pending-request-context))
      (when permission-rules
        (setq existing
              (plist-put existing :permission-rules
                         (append (plist-get existing :permission-rules)
                                 permission-rules))))
      (when model
        (setq existing (plist-put existing :model model)))
      (when effort
        (setq existing (plist-put existing :effort effort)))
      (when invoked-skill
        (setq existing
              (plist-put existing :invoked-skills
                         (append (plist-get existing :invoked-skills)
                                 (list invoked-skill)))))
      (setq-local mevedel-skills--pending-request-context existing)))
   (t
    (let ((req (mevedel-skills--current-request))
          (inv (mevedel-skills--current-invocation)))
      ;; Permission rules accumulate on the innermost slot.
      (when permission-rules
        (cond
         (inv
          (setf (mevedel-agent-invocation-skill-permission-rules inv)
                (append (mevedel-agent-invocation-skill-permission-rules inv)
                        permission-rules)))
         (req
          (setf (mevedel-request-skill-permission-rules req)
                (append (mevedel-request-skill-permission-rules req)
                        permission-rules)))))
      ;; Model and effort overwrite (last-writer-wins).
      (when model
        (cond
         (inv (setf (mevedel-agent-invocation-skill-model-override inv) model))
         (req (setf (mevedel-request-skill-model-override req) model))))
      (when effort
        (cond
         (inv (setf (mevedel-agent-invocation-skill-effort-override inv) effort))
         (req (setf (mevedel-request-skill-effort-override req) effort))))
      ;; Record on the session.
      (when invoked-skill
        (when-let* ((session (and (boundp 'mevedel--session) mevedel--session)))
          (setf (mevedel-session-invoked-skills session)
                (append (mevedel-session-invoked-skills session)
                        (list invoked-skill)))))))))

(cl-defun mevedel-skills--invoke-inline
    (skill arguments callback &key trigger display-callback)
  "Inline-context invocation.

Preparation order matches section \"Shell Injection\":
  1. Load body
  2. Substitute variables
  3. Activate skill-scoped permission rules (so allowed-tools is
     in effect during shell expansion)
  4. Expand shell injections
  5. Build invocation record
  6. Activate model/effort + record"
  (let* ((skill-name (mevedel-skill-name skill))
         (session (and (boundp 'mevedel--session) mevedel--session))
         (body (mevedel-skill-load-body skill)))
    (cond
     ((null body)
      (mevedel-skills--invoke-error
       skill 'load-failure
       (format "Skill %s could not be loaded: %s"
               skill-name
               (or (mevedel-skill-source-file skill) "unknown source"))
       callback display-callback))
     (t
      (let* ((substituted (mevedel-skills--substitute-vars
                           body arguments session skill))
             (rules (mevedel-skill-allowed-tool-rules skill))
             (model (mevedel-skills--model-selector skill))
             (effort (mevedel-skill-effort skill))
             (temporary-request-p nil))
        ;; Step 3: activate permission rules before shell expansion.
        (mevedel-skills--activate-context
         trigger :permission-rules rules)
        ;; Slash expansion happens before the real request exists.
        ;; Install a short-lived request so Bash pipeline permission
        ;; checks can see this skill's allowed-tools while shell
        ;; injection is being prepared.
        (when (and (eq trigger 'user-slash)
                   (not (bound-and-true-p mevedel--current-request)))
          (setq temporary-request-p t)
          (setq-local mevedel--current-request
                      (mevedel-request--create
                       :session session
                       :file-snapshots (make-hash-table :test #'equal)
                       :skill-permission-rules rules)))
        (mevedel-skills--run-shell-injections-async
         substituted
         (lambda (shell-outcome)
           (when temporary-request-p
             (setq-local mevedel--current-request nil))
           (pcase (plist-get shell-outcome :status)
             ('ok
              (let* ((expanded (plist-get shell-outcome :body))
                     (record
                      (mevedel-skill-invocation-record--create
                       :name skill-name
                       :args arguments
                       :trigger trigger
                       :turn (and session (mevedel-session-turn-count session))
                       :source-path (mevedel-skill-source-file skill)
                       :prepared-body expanded))
                     (ctx (list :permission-rules rules
                                :model model
                                :effort effort
                                :invoked-skills (list record))))
                ;; Rules already activated above; do not append them twice.
                (mevedel-skills--activate-context
                 trigger :model model :effort effort :invoked-skill record)
                (mevedel-skills--invoke-done
                 skill
                 `(:status ok :kind inline
                           :body ,expanded
                           :request-context ,ctx)
                 callback display-callback)))
             (_
              (mevedel-skills--invoke-error
               skill
               (plist-get shell-outcome :reason)
               (plist-get shell-outcome :message)
               callback display-callback))))))))))

(declare-function mevedel-agent-get "mevedel-agents" (name))
(declare-function mevedel-agent-to-gptel-spec "mevedel-agents" (agent))
(defvar mevedel-agent-exec--agents)
(defvar gptel--system-message)

(defun mevedel-skills--build-parent-inherited-agent (skill)
  "Build a synthetic `mevedel-agent' for SKILL with no `agent' field.

Captures the calling buffer's current gptel state at spawn time
and returns a `mevedel-agent' struct named `skill:<skill-name>'.
The agent inherits the parent's system prompt directly; tools are
inherited via the request-locals snapshot captured by
`mevedel-agent-exec--run' at dispatch time, which carries the
calling buffer's `gptel-tools' through to the spawned agent
buffer.

Side effect: the synthetic agent is also registered (or refreshed)
in the buffer-local `mevedel-agent-exec--agents' alist so the
spawn path can resolve it the same way it resolves named agents.
Registration is keyed on the `skill:<skill-name>' identifier."
  (let* ((skill-name (mevedel-skill-name skill))
         (agent-name (concat "skill:" skill-name))
         (parent-system (and (boundp 'gptel--system-message)
                             gptel--system-message))
         (agent
          (mevedel-agent--create
           :name agent-name
           :description (or (mevedel-skill-description skill)
                            (format "Parent-inherited fork of skill %s"
                                    skill-name))
           :tools nil
           :system-prompt (or parent-system "")
           :max-turns nil
           :reminders nil)))
    (let ((spec (mevedel-agent-to-gptel-spec agent))
          (existing (and (boundp 'mevedel-agent-exec--agents)
                         mevedel-agent-exec--agents)))
      (setq-local mevedel-agent-exec--agents
                  (cons spec
                        (cl-remove agent-name existing
                                   :key #'car :test #'equal)))
      ;; Some tests dynamically bind this special variable; keep the
      ;; dynamic binding in sync with the buffer-local value.
      (setq mevedel-agent-exec--agents
            (buffer-local-value 'mevedel-agent-exec--agents
                                (current-buffer))))
    agent))

(defun mevedel-skills--build-fork-agent (skill)
  "Return a `mevedel-agent' struct to use for SKILL's fork dispatch.

If SKILL declares an `agent' field, look it up in the registry
and return that agent.  Returns nil for unknown agent names so
the caller can produce an `unknown-agent' outcome.

If SKILL does not declare an `agent' field, build a synthetic
parent-inherited agent via
`mevedel-skills--build-parent-inherited-agent'.  The synthetic
agent's name is `skill:<skill-name>'; system prompt is
snapshotted from the calling buffer's `gptel--system-message';
tools propagate through the spawn path's request-locals capture."
  (let ((agent-name (mevedel-skill-agent skill)))
    (cond
     ((and (stringp agent-name) (not (string-empty-p agent-name)))
      (mevedel-agent-get agent-name))
     (t
      (mevedel-skills--build-parent-inherited-agent skill)))))

(cl-defun mevedel-skills--invoke-fork
    (skill arguments callback &key trigger display-callback)
  "Fork dispatch dispatcher.

All triggers use direct foreground dispatch through
`mevedel-tools--task'.  Slash callers must treat CALLBACK as the
continuation and suppress the original `gptel-send' until/unless an
inline body is produced."
  (mevedel-skills--invoke-fork-direct
   skill arguments callback
   :trigger trigger :display-callback display-callback))

(cl-defun mevedel-skills--invoke-fork-direct
    (skill arguments callback &key trigger display-callback)
  "Direct fork dispatch via `mevedel-tools--task'.  Async outcome.

Builds the target `mevedel-agent' via
`mevedel-skills--build-fork-agent', then dispatches the substituted
skill body as the agent's task prompt.  The agent runs foreground;
the parent FSM parks until the agent returns.

The outcome callback fires when the agent completes (potentially
much later than this function returns).  Suitable for callers
that already operate async (e.g., the `Skill' tool handler)."
  (let* ((skill-name (mevedel-skill-name skill))
         (session (and (boundp 'mevedel--session) mevedel--session))
         (agent (mevedel-skills--build-fork-agent skill)))
    (cond
     ((null agent)
      ;; --build-fork-agent only returns nil for an unknown named
      ;; agent; the parent-inherited path always synthesizes a
      ;; struct.
      (mevedel-skills--invoke-error
       skill 'unknown-agent
       (format "Skill '%s' references unknown agent '%s'"
               skill-name (mevedel-skill-agent skill))
       callback display-callback))
     (t
      (let* ((body (or (mevedel-skill-load-body skill) ""))
             (substituted (mevedel-skills--substitute-vars
                           body arguments session skill))
             (description (or (mevedel-skill-description skill) skill-name))
             (rules (mevedel-skill-allowed-tool-rules skill))
             (model (mevedel-skills--model-selector skill))
             (effort (mevedel-skill-effort skill))
             (temporary-request-p nil))
        (unless (eq trigger 'user-slash)
          (mevedel-skills--activate-context
           trigger :permission-rules rules))
        (when (and (eq trigger 'user-slash)
                   (not (bound-and-true-p mevedel--current-request)))
          (setq temporary-request-p t)
          (setq-local mevedel--current-request
                      (mevedel-request--create
                       :session session
                       :file-snapshots (make-hash-table :test #'equal)
                       :skill-permission-rules rules)))
        (mevedel-skills--run-shell-injections-async
         substituted
         (lambda (shell-outcome)
           (when temporary-request-p
             (setq-local mevedel--current-request nil))
           (pcase (plist-get shell-outcome :status)
             ('ok
              (let* ((prepared (plist-get shell-outcome :body))
                     (record
                      (mevedel-skill-invocation-record--create
                       :name skill-name
                       :args arguments
                       :trigger trigger
                       :turn (and session (mevedel-session-turn-count session))
                       :source-path (mevedel-skill-source-file skill)
                       :prepared-body prepared)))
                (when session
                  (setf (mevedel-session-invoked-skills session)
                        (append (mevedel-session-invoked-skills session)
                                (list record))))
                (unless (fboundp 'mevedel-tools--task)
                  (require 'mevedel-tool-ui))
                (mevedel-tools--task
                 (lambda (response)
                   ;; `mevedel-tools--task' may deliver either a bare string
                   ;; or a plist `(:result STR :render-data (:kind
                   ;; agent-transcript :agent-id ID ...))' when transcript
                   ;; metadata is present (see
                   ;; `mevedel-tools--task--wrap-foreground-response').
                   ;; Destructure so the outcome shape gets the
                   ;; right :result string and forwards :render-data plus
                   ;; the unique invocation agent-id.
                   (let* ((wrapped-p (and (listp response)
                                          (plist-member response :result)))
                          (result-str (if wrapped-p
                                          (plist-get response :result)
                                        response))
                          (render-data (and wrapped-p
                                            (plist-get response :render-data)))
                          (transcript-agent-id
                           (and wrapped-p
                                (plist-get render-data :agent-id))))
                     (mevedel-skills--invoke-done
                      skill
                      `(:status ok :kind fork
                                :result ,result-str
                                :agent-id ,(or transcript-agent-id
                                               (mevedel-agent-name agent))
                                :render-data ,render-data)
                      callback display-callback)))
                 agent description prepared
                 :skill-permission-rules rules
                 :skill-model-override model
                 :skill-effort-override effort)))
             (_
              (mevedel-skills--invoke-error
               skill
               (plist-get shell-outcome :reason)
               (plist-get shell-outcome :message)
               callback display-callback))))))))))

(cl-defun mevedel-skills-invoke
    (skill arguments callback &key trigger display-callback)
  "Invoke SKILL with ARGUMENTS through the unified skill API.

CALLBACK is invoked with a normalized invocation outcome plist:

  (:status ok    :kind inline :body BODY :request-context CTX)
  (:status ok    :kind fork   :result RESULT :agent-id ID
                  :render-data DATA)
  (:status error :reason REASON :message MESSAGE)

TRIGGER is `user-slash', `model-skill', or `internal' and
determines the blocking model implicitly: `user-slash' blocks
chat input; `model-skill' blocks the parent tool call.

DISPLAY-CALLBACK is an optional lifecycle event sink that
receives `agent-progress' (fork only), `done', and `error'
events.

Recursion depth is tracked via the dynamic let-bound
`mevedel-skills--invoke-depth'.
Crossing `mevedel-skills-max-recursion-depth' yields a
`recursion-limit-exceeded' error outcome.

Inline and fork contexts are callback-driven.  Inline invocation
calls CALLBACK with a prepared body; fork invocation dispatches a
foreground agent and calls CALLBACK when that agent returns."
  (let ((skill-name (and skill (mevedel-skill-name skill))))
    (cond
     ((not (mevedel-skill-p skill))
      (mevedel-skills--invoke-error
       skill 'unknown-skill
       "Invalid skill struct"
       callback display-callback))
     ;; Recursion guard.
     ((>= mevedel-skills--invoke-depth mevedel-skills-max-recursion-depth)
      (mevedel-skills--invoke-error
       skill 'recursion-limit-exceeded
       (format "Skill recursion limit exceeded (max %d) invoking '%s'"
               mevedel-skills-max-recursion-depth skill-name)
       callback display-callback))
     ;; User-slash gating.
     ((and (eq trigger 'user-slash)
           (not (mevedel-skill-user-invocable-p skill)))
      (mevedel-skills--invoke-error
       skill 'disabled
       (format "Skill '%s' is not user-invocable" skill-name)
       callback display-callback))
     ;; Model-side gating.
     ((and (eq trigger 'model-skill)
           (not (mevedel-skill-model-invocable-p skill)))
      (mevedel-skills--invoke-error
       skill 'disabled
       (format "Skill '%s' is not model-invocable" skill-name)
       callback display-callback))
     (t
      (let ((mevedel-skills--invoke-depth
             (1+ mevedel-skills--invoke-depth)))
        (pcase (mevedel-skill-context skill)
          ('inline
           (mevedel-skills--invoke-inline
            skill arguments callback
            :trigger trigger :display-callback display-callback))
          ('fork
           (mevedel-skills--invoke-fork
            skill arguments callback
            :trigger trigger :display-callback display-callback))
          (other
           (mevedel-skills--invoke-error
            skill 'unknown-skill
            (format "Skill '%s' has unsupported context: %S"
                    skill-name other)
            callback display-callback))))))))


;;
;;; Skill tool handler

(defun mevedel-skills--invoke-handler (callback args)
  "Pipeline handler for the `Skill' tool.

CALLBACK is the async tool callback.  ARGS is a plist with :name
and optional :arguments.

Routes through `mevedel-skills-invoke' with `model-skill' trigger
per spec.  The outcome plist is projected
to a tool-result string: success returns the body; error returns
a `Error: ' prefixed message."
  (let* ((name (plist-get args :name))
         (arguments (plist-get args :arguments))
         (session (and (boundp 'mevedel--session) mevedel--session))
         (skill (and session (mevedel-session-get-skill session name))))
    (cond
     ((not (stringp name))
      (funcall callback "Error: Skill name is required."))
     ((not session)
      (funcall callback "Error: No active mevedel session."))
     ((not skill)
      (funcall callback (format "Error: Unknown skill '%s'." name)))
     (t
      (mevedel-skills-invoke
       skill arguments
       (lambda (outcome)
         (pcase (plist-get outcome :status)
           ('ok
            (funcall callback
                     (or (plist-get outcome :body)
                         (plist-get outcome :result)
                         (format "Skill '%s' produced no body." name))))
           ('error
            (funcall callback
                     (format "Error: %s"
                             (or (plist-get outcome :message)
                                 "skill invocation failed"))))))
       :trigger 'model-skill)))))


;;
;;; Tool registration

;;;###autoload
(defun mevedel-skills--register ()
  "Register the `Skill' tool with the mevedel tool registry.

spec: `:get-name' lets permission rules
qualify by skill name, e.g.
  `(\"Skill\" :name \"commit\" :action ask)'
will match a `Skill(name=\"commit\")' invocation.  The `:name'
specifier here means *the skill name* (invocation identifier),
distinct from `Agent :name' which matches subagent_type."
  (mevedel-define-tool
    :name "Skill"
    :description "Invoke a reusable prompt recipe (skill) by name."
    :handler #'mevedel-skills--invoke-handler
    :args ((name string :required
                 "The skill name (as shown in the skills listing).")
           (arguments string :optional
                      "Optional argument string passed to the skill."))
    :async-p t
    :read-only-p t
    :get-name (lambda (args) (plist-get args :name))
    :groups (util)))


;;
;;; Local slash commands

(defvar mevedel-slash-commands)

(defun mevedel-cmd--tokens (_args)
  "Print the estimated token usage of the current chat buffer."
  (message "Estimated tokens in this buffer: %d"
           (mevedel--estimate-tokens)))

(defun mevedel-cmd--model (args)
  "Show or set the gptel model for the current chat buffer.
With a non-empty ARGS string, set `gptel-model' to the interned symbol."
  (if (and args (not (string-blank-p args)))
      (let ((model (intern (string-trim args))))
        (setq-local gptel-model model)
        (message "Model set to %s" model))
    (message "Current model: %s" gptel-model)))

(defun mevedel-cmd--compact (_args)
  "Run `mevedel-compact' on the current chat buffer."
  (mevedel-compact))

(defun mevedel-cmd--mode (args)
  "Show or set `mevedel-permission-mode' for the current chat buffer.
Recognized modes: default, accept-edits, plan, trust-all.

Routes through `setopt' so `mevedel-permission-mode--set' fires and
updates the session slot + both buffer-locals in one pass; a plain
`setq-local' would only touch whichever buffer the slash command ran
in, leaving the session slot and the other buffer to drift."
  (if (and args (not (string-blank-p args)))
      (let ((mode (intern (string-trim args))))
        (unless (memq mode '(default accept-edits plan trust-all))
          (user-error "Unknown permission mode: %s" mode))
        (setopt mevedel-permission-mode mode)
        (message "Permission mode set to %s" mode))
    (message "Current permission mode: %s" mevedel-permission-mode)))

(defun mevedel-cmd--clear (_args)
  "Erase the current chat buffer and reinsert the prompt prefix."
  (when (yes-or-no-p "Clear all chat buffer content? ")
    (let ((inhibit-read-only t)
          (prefix (or (alist-get major-mode gptel-prompt-prefix-alist) "")))
      (erase-buffer)
      (insert prefix)
      (goto-char (point-max)))))

(defun mevedel-cmd--help (_args)
  "Show the list of local slash commands and available skills."
  (let* ((locals (mapconcat
                  (lambda (cell) (format "/%s" (car cell)))
                  mevedel-slash-commands
                  "  "))
         (skills (and mevedel--session
                      (mapconcat #'mevedel-skill-name
                                 (mevedel-session-skills mevedel--session)
                                 "  "))))
    (message "Commands: %s%s"
             locals
             (if (and skills (not (string-empty-p skills)))
                 (format "\nSkills: %s" skills)
               ""))))

(defvar mevedel-slash-commands
  '(("tokens"  . mevedel-cmd--tokens)
    ("model"   . mevedel-cmd--model)
    ("compact" . mevedel-cmd--compact)
    ("mode"    . mevedel-cmd--mode)
    ("clear"   . mevedel-cmd--clear)
    ("help"    . mevedel-cmd--help))
  "Alist of local slash commands.
Each entry is a (NAME . HANDLER) pair.  HANDLER is a function
accepting a single ARGS string (the text after the command name,
trimmed), and is expected to execute immediately and return nil.
Handlers have access to the buffer-local `mevedel--session'.")


;;
;;; Slash-command dispatch

(defun mevedel-skills--current-prompt-region ()
  "Return (START . END) of the pending prompt text in the chat buffer.

Locates the start of the current user prompt by preferring gptel's
`gptel' text property -- which marks prior LLM responses even when the
user has disabled the prompt prefix -- and falling back to the last
occurrence of the configured prompt prefix.  If neither boundary is
present the whole buffer is treated as the pending prompt.  END is
always `point-max'.  Returns nil only for an empty buffer."
  (save-excursion
    (goto-char (point-max))
    (let* ((match (text-property-search-backward 'gptel nil nil t))
           (prefix (alist-get major-mode gptel-prompt-prefix-alist))
           (has-prefix (and prefix (not (string-empty-p prefix))))
           start)
      (cond
       (match (setq start (prop-match-end match)))
       ((and has-prefix
             (progn (goto-char (point-max))
                    (search-backward prefix nil t)))
        (setq start (+ (point) (length prefix))))
       ((< (point-min) (point-max))
        (setq start (point-min))))
      (when (and start has-prefix)
        (save-excursion
          (goto-char start)
          (when (looking-at-p (regexp-quote prefix))
            (setq start (+ start (length prefix))))))
      (when start
        (cons start (point-max))))))

(defun mevedel-skills--parse-slash-line (text)
  "Parse TEXT for a leading `/command [args]' line.
Returns (NAME ARGS OFFSET) when TEXT starts (after optional leading
whitespace) with `/' followed by an identifier, or nil otherwise.
NAME is the command name; ARGS is the rest of TEXT after the
command name (the remainder of the first line plus every
subsequent line, joined and trimmed); OFFSET is the 0-based
character position of `/' within TEXT.

Skill commands (`/coordinator', `/grill-me', etc.) take the user's
prompt body as ARGS and the body is naturally multi-line.  Local
commands (`/model', `/mode', etc.) parse only the first whitespace-
separated token from ARGS and ignore the rest, so extending ARGS
to include subsequent lines does not change their behavior."
  (let* ((trimmed (string-trim-left text))
         (offset (- (length text) (length trimmed))))
    (when (and (> (length trimmed) 1) (eq (aref trimmed 0) ?/))
      (let* ((line-end (or (string-match "\n" trimmed) (length trimmed)))
             (line (substring trimmed 1 line-end))
             (rest (substring trimmed line-end))
             (space (string-match "[ \t]" line))
             (name (if space (substring line 0 space) line))
             (first-line-args (if space (substring line space) ""))
             (args (string-trim (concat first-line-args rest))))
        (when (string-match-p "\\`[A-Za-z0-9_-]+\\'" name)
          (list name args offset))))))

(defun mevedel-skills--ensure-fresh-line ()
  "Leave point at the start of an empty line with a blank line above.
Called after deleting a slash-command region so the next insertion or
cursor rest position is visually separated from the preceding response.
Skipped when the slash command was preceded solely by the prompt prefix,
since in that case the prefix should remain on its own line and the body
should inline with it."
  (unless (bolp) (insert "\n"))
  (unless (save-excursion
            (forward-line -1)
              (looking-at-p "^[ \t]*$"))
    (insert "\n")))

(defun mevedel-skills--insert-fork-result (outcome)
  "Insert fork skill OUTCOME as an assistant response in the data buffer.

The current buffer must be the data buffer.  This path is used when a
slash fork skill suppresses the main `gptel-send'; it records the
foreground agent's final result as the assistant side of that turn and
runs the normal post-response hooks so the view and persistence layers
observe the completed response."
  (let ((result (or (plist-get outcome :result)
                    "Fork skill produced no result.")))
    (unless (bound-and-true-p mevedel--current-request)
      (when (bound-and-true-p mevedel--session)
        (mevedel-request-begin mevedel--session
                               (and (boundp 'mevedel--current-directive-uuid)
                                    mevedel--current-directive-uuid))))
    (goto-char (point-max))
    (unless (bolp) (insert "\n"))
    (insert gptel-response-separator)
    (let ((start (point)))
      (insert result)
      (unless (eq (char-before) ?\n)
        (insert "\n"))
      (let ((end (point)))
        (add-text-properties start end '(gptel response))
        (run-hook-with-args 'gptel-post-response-functions start end)
        (mevedel-skills--finalize-fork-turn)))))

(defun mevedel-skills--finalize-fork-turn ()
  "Run completed-turn bookkeeping for a direct slash fork turn.

Direct fork skills suppress the parent `gptel-send' request, so the
main FSM's DONE handlers never run.  Mirror the subset of that
terminal path that belongs to a successful completed turn: bump the
session turn count, save the session sidecar/segment, clear the
active request, and reset gptel's status indicator."
  (unwind-protect
      (when (bound-and-true-p mevedel--session)
        (cl-incf (mevedel-session-turn-count mevedel--session))
        (require 'mevedel-session-persistence)
        (when (and (bound-and-true-p mevedel-session-persistence)
                   (not (bound-and-true-p mevedel-session--read-only-mode)))
          (condition-case err
              (progn
                (mevedel-session-persistence-save mevedel--session
                                                  (current-buffer))
                (when (bound-and-true-p mevedel-session--save-failed)
                  (setq-local mevedel-session--save-failed nil)
                  (force-mode-line-update)))
            (error
             (display-warning 'mevedel
                              (format "Session auto-save failed: %s" err)
                              :warning)
             (setq-local mevedel-session--save-failed t)
             (force-mode-line-update)))))
    (when (bound-and-true-p mevedel--current-request)
      (mevedel-request-end))
    (gptel--update-status " Ready" 'success)))

(defun mevedel-skills--handle-slash-outcome
    (skill outcome delete-start region-end after-prefix continue-fn)
  "Apply slash skill OUTCOME in the current data buffer.

CONTINUE-FN, when non-nil, resumes the original `gptel-send' after an
inline body has been inserted.  Fork outcomes suppress that send and
insert their result when the foreground agent finishes."
  (pcase (plist-get outcome :status)
    ('ok
     (pcase (plist-get outcome :kind)
       ('inline
        (delete-region delete-start region-end)
        (unless after-prefix
          (mevedel-skills--ensure-fresh-line))
        (insert (or (plist-get outcome :body)
                    (format "Skill '%s' produced no body."
                            (mevedel-skill-name skill))))
        (when continue-fn
          (funcall continue-fn))
        'skill)
       ('fork
        (message "Skill '%s' dispatched; waiting for agent result..."
                 (mevedel-skill-name skill))
        (mevedel-skills--insert-fork-result outcome)
        'skill)
       (_
        (message "Skill '%s' returned an unsupported outcome: %S"
                 (mevedel-skill-name skill) outcome)
        'unknown)))
    (_
     (message "Skill '%s' failed: %s"
              (mevedel-skill-name skill)
              (plist-get outcome :message))
     'unknown)))

(defun mevedel-skills--dispatch-slash-command (&optional continue-fn)
  "Parse and dispatch a `/command' in the current chat buffer.

Returns:
- `local'   a local command ran; caller should abort the send.
- `skill'   a skill was expanded into the prompt region; caller
            should proceed with the send if CONTINUE-FN was nil.
- `unknown' a `/' line was present but matched nothing; caller
            should abort the send.
- nil       no `/command' present; caller should proceed as usual."
  (when-let* ((region (mevedel-skills--current-prompt-region))
              (text (buffer-substring-no-properties (car region) (cdr region)))
              (parsed (mevedel-skills--parse-slash-line text)))
    (let* ((name (nth 0 parsed))
           (args (nth 1 parsed))
           (slash-pos (+ (car region) (nth 2 parsed)))
           (local (assoc name mevedel-slash-commands))
           (skill (and mevedel--session
                       (mevedel-session-get-skill mevedel--session name)))
           (prefix (alist-get major-mode gptel-prompt-prefix-alist))
           (has-prefix (and prefix (not (string-empty-p prefix))))
           (line-start (save-excursion
                         (goto-char slash-pos)
                         (line-beginning-position)))
           (before-slash (buffer-substring-no-properties line-start slash-pos))
           (after-prefix (and has-prefix (equal before-slash prefix)))
           (delete-start
            (cond
             (after-prefix slash-pos)
             ((string-match-p "\\`[ \t]*\\'" before-slash) line-start)
             (t slash-pos))))
      (cond
       (local
        (delete-region delete-start (cdr region))
        (unless after-prefix
          (mevedel-skills--ensure-fresh-line))
       (funcall (cdr local) args)
       'local)
       (skill
        (let ((buffer (current-buffer))
              (region-end (cdr region))
              (dispatch-result 'skill))
          (mevedel-skills-invoke
           skill args
           (lambda (outcome)
             (when (buffer-live-p buffer)
               (with-current-buffer buffer
                 (setq dispatch-result
                       (mevedel-skills--handle-slash-outcome
                        skill outcome delete-start region-end
                        after-prefix continue-fn)))))
           :trigger 'user-slash)
          dispatch-result))
       (t
        (message "Unknown slash command: /%s" name)
        'unknown)))))

(defun mevedel-skills--gptel-send-advice (orig-fn &rest args)
  "`:around' advice on `gptel-send' for slash-command dispatch.

Dispatches the leading `/command' on the prompt region first.
- Local commands and unknown slashes abort the send (do not call
  ORIG-FN).
- Inline skills install body + pending-stash, then ORIG-FN is called
  from the invocation callback.
- Fork skills dispatch an agent directly and never call ORIG-FN for
  the slash command.
- No `/command' present -> proceed unchanged.

Pending-stash cleanup is tied to the continuation that actually resumes
ORIG-FN so async shell preparation does not clear the stash before the
request begin handler can drain it."
  (if (not (bound-and-true-p mevedel--session))
      (apply orig-fn args)
    (let ((decision
           (mevedel-skills--dispatch-slash-command
            (lambda ()
              (unwind-protect
                  (apply orig-fn args)
                (when (and (boundp 'mevedel-skills--pending-request-context)
                           mevedel-skills--pending-request-context)
                  (setq-local mevedel-skills--pending-request-context nil)))))))
      (pcase decision
        ('local nil)
        ('unknown nil)
        ('skill nil)
        (_
         (unwind-protect
             (apply orig-fn args)
           (when (and (boundp 'mevedel-skills--pending-request-context)
                      mevedel-skills--pending-request-context)
             (setq-local mevedel-skills--pending-request-context nil))))))))


;;
;;; Completion at point

(defun mevedel-skills--progressive-argument-hint (skill)
  "Return SKILL's argument-hint string for completion annotation.

If `argument-hint' is set, return it.  Otherwise generate from
`argument-names': `[name1] [name2] ...'.  Returns nil when neither
is available."
  (let ((hint (mevedel-skill-argument-hint skill))
        (names (mevedel-skill-argument-names skill)))
    (cond
     ((and (stringp hint) (not (string-empty-p hint))) hint)
     (names
      (mapconcat (lambda (n) (format "[%s]" n)) names " "))
     (t nil))))

(defun mevedel-slash-capf ()
  "Completion-at-point for `/command' and `/skill-name' prefixes.
Active only in a mevedel chat buffer when point sits just after a
`/' that begins its line (after an optional prompt prefix).

spec:
- Local commands annotated as ` [command]'.
- User-invocable skills annotated as ` [skill]'.
- Path-scoped skills not yet active append ` [dormant]' so the
  user sees the skill exists but understands it is not in the
  current model listing.
- Argument hints (from `argument-hint' or generated from
  `arguments') are appended to skill annotations when available.
- Skills with `user-invocable: false' are omitted entirely."
  (when (and (bound-and-true-p mevedel--session)
             (save-excursion
               (skip-chars-backward "A-Za-z0-9_-")
               (and (eq (char-before) ?/)
                    (let ((prefix (alist-get major-mode
                                             gptel-prompt-prefix-alist)))
                      (save-excursion
                        (backward-char)
                        (or (bolp)
                            (and prefix
                                 (looking-back (regexp-quote prefix)
                                               (line-beginning-position)))))))))
    (let* ((end (point))
           (start (save-excursion
                    (skip-chars-backward "A-Za-z0-9_-")
                    (point)))
           ;; spec: omit user-invocable: false skills from completion.
           (visible-skills
            (cl-remove-if-not
             #'mevedel-skill-user-invocable-p
             (mevedel-session-skills mevedel--session)))
           (skill-by-name (mapcar (lambda (s)
                                    (cons (mevedel-skill-name s) s))
                                  visible-skills))
           (skill-names (mapcar #'car skill-by-name))
           (local-names (mapcar #'car mevedel-slash-commands))
           (candidates (append local-names skill-names)))
      (list start end candidates
            :exclusive 'no
            :annotation-function
            (lambda (name)
              (cond
               ((assoc name mevedel-slash-commands) " [command]")
               (t
                (let* ((skill (cdr (assoc name skill-by-name)))
                       (annotation " [skill]")
                       (dormant (and skill
                                     (mevedel-skill-path-patterns skill)
                                     (not (mevedel-skill-active-p skill))))
                       (hint (and skill
                                  (mevedel-skills--progressive-argument-hint
                                   skill))))
                  (concat annotation
                          (when dormant " [dormant]")
                          (when hint (concat " " hint)))))))))))


;;
;;; Installation

;;;###autoload
(defun mevedel-skills-install-slash-commands ()
  "Install the slash-command advice on `gptel-send'.

`:around' so the advice can wrap the call in `unwind-protect' for
pending-stash cleanup."
  (advice-add 'gptel-send :around
              #'mevedel-skills--gptel-send-advice))

(defun mevedel-skills-uninstall-slash-commands ()
  "Remove the slash-command advice from `gptel-send'."
  (advice-remove 'gptel-send
                 #'mevedel-skills--gptel-send-advice))


;;
;;; Skills listing reminder

(defcustom mevedel-skills-listing-budget 0.01
  "Fraction of the context window allotted to the skills-listing reminder.

The listing reminder enumerates active, model-invocable skills so the
model can call them by name via the `Skill' tool.  This fraction of
`mevedel-compact-context-limit' (converted to characters at four chars
per token) caps the total payload so the listing cannot crowd out the
user's conversation on long sessions."
  :type 'float
  :group 'mevedel)

(defcustom mevedel-skills-listing-max-entry-chars 1536
  "Maximum characters per skill entry in the skills-listing reminder.

Entries longer than this are truncated with an ellipsis so a single
verbose description cannot starve the rest of the listing.  spec
section \"Skill Listing\" pins the cap at 1,536 chars across description +
when_to_use combined."
  :type 'integer
  :group 'mevedel)

(defconst mevedel-skills--source-priority
  '(user project bundled managed plugin)
  "Source-tag priority for skills-listing reminder ordering.
spec: user > project > bundled > plugin/managed.
Matches the discovery/shadowing precedence so a skill the user
installed is more likely to appear when budget pressure drops
trailing entries.")

(defconst mevedel-skills--dormant-note
  "Additional path-scoped skills may exist for this session and \
can be invoked directly by name via `Skill(name=...)' even when not \
listed here."
  "Fixed text appended to the listing reminder.
Tells the model that direct-by-name invocation works for skills
not currently in the listing (e.g. dormant path-scoped skills).")

(defun mevedel-skills--listing-budget-chars ()
  "Return the character budget for the skills-listing reminder.
Derived from `mevedel-skills-listing-budget' and the compact context
limit; assumes ~4 characters per token."
  (let ((limit (or (and (boundp 'mevedel-compact-context-limit)
                        mevedel-compact-context-limit)
                   200000)))
    (max 0 (floor (* mevedel-skills-listing-budget limit 4)))))

(defun mevedel-skills--listing-describe (skill)
  "Return a one-line entry for SKILL.

Format:
  - name: description - when_to_use

`when_to_use' (and the surrounding ` - ') is omitted when SKILL
has no `when-to-use'.  Capped at
`mevedel-skills-listing-max-entry-chars' (1,536 by default
per spec) by truncation with an ellipsis so a single verbose
skill cannot starve the rest of the listing."
  (let* ((max-chars mevedel-skills-listing-max-entry-chars)
         (name (mevedel-skill-name skill))
         (desc (or (mevedel-skill-description skill) ""))
         (when-to-use (mevedel-skill-when-to-use skill))
         (line (if (and (stringp when-to-use)
                        (not (string-empty-p when-to-use)))
                   (format "- %s: %s - %s" name desc when-to-use)
                 (format "- %s: %s" name desc))))
    (if (> (length line) max-chars)
        (concat (substring line 0 (max 1 (- max-chars 3))) "...")
      line)))

(defun mevedel-skills--listing-candidates (session)
  "Return SESSION's model-invocable, currently active skills.

Sorted by `mevedel-skills--source-priority' (user > project >
bundled > managed > plugin) so budget pressure drops bundled
entries before user entries."
  (let ((candidates
         (cl-remove-if-not
          (lambda (s)
            (and (mevedel-skill-model-invocable-p s)
                 (mevedel-skill-active-p s)))
          (mevedel-session-skills session))))
    (cl-sort (copy-sequence candidates)
             (lambda (a b)
               (let ((ai (or (cl-position
                              a mevedel-skills--source-priority
                              :test (lambda (sk tag)
                                      (eq (mevedel-skill-source sk) tag)))
                             most-positive-fixnum))
                     (bi (or (cl-position
                              b mevedel-skills--source-priority
                              :test (lambda (sk tag)
                                      (eq (mevedel-skill-source sk) tag)))
                             most-positive-fixnum)))
                 (< ai bi))))))

(defun mevedel-skills--session-has-dormant-skills-p (session)
  "Return non-nil when SESSION has model-invocable but inactive skills.
Used to decide whether the listing reminder appends the dormant-
skill note."
  (cl-some (lambda (s)
             (and (mevedel-skill-model-invocable-p s)
                  (mevedel-skill-path-patterns s)
                  (not (mevedel-skill-active-p s))))
           (mevedel-session-skills session)))

(defun mevedel-skills--format-listing (skills &optional include-dormant-note)
  "Format SKILLS as the body of the skills-listing reminder.

Budget-capped at `mevedel-skills--listing-budget-chars'; entries
past the budget are dropped (rather than truncating names) per
spec.

When INCLUDE-DORMANT-NOTE is non-nil and the budget allows,
`mevedel-skills--dormant-note' is appended after the entries to
tell the model that direct-by-name invocation works for skills
that are not currently listed."
  (let* ((budget (mevedel-skills--listing-budget-chars))
         (header "The following skills are available for use with the Skill tool:")
         (note (and include-dormant-note mevedel-skills--dormant-note))
         (used (+ (length header) 2))
         (lines nil))
    (catch 'done
      (dolist (skill skills)
        (let* ((entry (mevedel-skills--listing-describe skill))
               (cost (1+ (length entry))))
          (when (> (+ used cost) budget)
            (throw 'done nil))
          (push entry lines)
          (cl-incf used cost))))
    (let ((body (concat header "\n\n"
                        (mapconcat #'identity (nreverse lines) "\n"))))
      (if (and note (<= (+ used 2 (length note)) budget))
          (concat body "\n\n" note)
        body))))

(defun mevedel-reminders-make-skills-listing ()
  "Create the `skills-listing' reminder.

Fires every turn the session has at least one model-invocable
skill (active OR dormant -- a session with only dormant skills
still needs the dormant-skill note so the model knows it can
invoke them by name; spec).

The listing enumerates active skills so the model can call them
via the `Skill' tool; budget-capped by `mevedel-skills-listing-budget'.
When dormant path-scoped skills exist on the session, a fixed
note is appended telling the model that direct-by-name
invocation works for skills not in the listing."
  (mevedel-reminder-create
   :type 'skills-listing
   :trigger (lambda (session)
              (or (mevedel-skills--listing-candidates session)
                  (mevedel-skills--session-has-dormant-skills-p session)))
   :content (lambda (session)
              (mevedel-skills--format-listing
               (mevedel-skills--listing-candidates session)
               (mevedel-skills--session-has-dormant-skills-p session)))
   :interval nil))


;;
;;; Conditional activation

(defun mevedel-skills--glob-matches-p (pattern path)
  "Return non-nil if glob PATTERN matches PATH or its basename.
Uses `wildcard-to-regexp' for glob translation."
  (let ((re (wildcard-to-regexp pattern)))
    (or (string-match-p re path)
        (string-match-p re (file-name-nondirectory path)))))

(defun mevedel-skills--path-matches-p (path patterns)
  "Return non-nil if PATH matches any glob in PATTERNS."
  (and path patterns
       (cl-some (lambda (pat) (mevedel-skills--glob-matches-p pat path))
                patterns)))

(defun mevedel-skills--maybe-activate (session path)
  "Activate dormant SESSION skills whose path-patterns match PATH.

Idempotent: already-active skills are left alone.  Returns the list of
skills newly activated by this call."
  (let (activated)
    (dolist (skill (mevedel-session-skills session))
      (when (and (not (mevedel-skill-active-p skill))
                 (mevedel-skill-path-patterns skill)
                 (mevedel-skills--path-matches-p
                  path (mevedel-skill-path-patterns skill)))
        (setf (mevedel-skill-active-p skill) t)
        (push skill activated)))
    (nreverse activated)))

(defun mevedel-skills--post-tool-activate (info)
  "Post-tool-call hook: activate conditional skills based on tool path.

INFO is the plist passed by `gptel-post-tool-call-functions'.  Extracts
the touched path via the tool struct's `get-path' slot and forwards it
to `mevedel-skills--maybe-activate'."
  (when-let* ((session (bound-and-true-p mevedel--session))
              (tool-name (plist-get info :name))
              (args (plist-get info :args))
              (tool (mevedel-tool-get tool-name))
              (get-path-fn (mevedel-tool-get-path tool)))
    (when-let* ((path (ignore-errors (funcall get-path-fn args))))
      (mevedel-skills--maybe-activate session path)))
  nil)

;;;###autoload
(defun mevedel-skills-install-activation-hook ()
  "Install the buffer-local post-tool-call activation hook."
  (add-hook 'gptel-post-tool-call-functions
            #'mevedel-skills--post-tool-activate nil t))

(defun mevedel-skills-uninstall-activation-hook ()
  "Remove the buffer-local post-tool-call activation hook."
  (remove-hook 'gptel-post-tool-call-functions
               #'mevedel-skills--post-tool-activate t))


;;
;;; Reminder installation

;;;###autoload
(defun mevedel-skills-install-reminder (session)
  "Add the `skills-listing' reminder to SESSION if not already present.
Idempotent."
  (unless (cl-find 'skills-listing
                   (mevedel-session-reminders session)
                   :key #'mevedel-reminder-type)
    (mevedel-session-add-reminder
     session (mevedel-reminders-make-skills-listing)))
  session)


(provide 'mevedel-skills)

;;; mevedel-skills.el ends here
