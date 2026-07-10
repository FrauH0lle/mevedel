;;; mevedel-skills.el -- Skills infrastructure -*- lexical-binding: t -*-

;;; Commentary:

;; Skills are reusable, named prompt recipes loaded from SKILL.md files.
;; Each skill bundles task-scoped instructions, tool restrictions, and
;; execution context.  This module owns the data model (discovery,
;; loading, invocation, and the `Skill' tool) while delegating YAML
;; frontmatter parsing to `gptel-agent-read-file'.

;;; Code:

(require 'cl-lib)

(eval-when-compile
  (require 'mevedel-agents)
  (require 'tabulated-list))

(require 'gptel-agent)
(require 'mevedel-structs)
(require 'mevedel-reminders)
(require 'mevedel-tool-registry)
(require 'mevedel-permissions)
(require 'mevedel-models)

;; `gptel'
(defvar gptel-post-tool-call-functions)

;; `mevedel-agent-exec'
(defvar mevedel--agent-invocation)

;; `mevedel-agents'
(declare-function mevedel-agent--create "mevedel-agents" (&rest args))
(declare-function mevedel-agent-get "mevedel-agents" (name))
(declare-function mevedel-agent-invocation-hook-rules
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-parent-session
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-skill-effort-override
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-skill-model-override
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-skill-permission-rules
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-name "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-to-gptel-spec "mevedel-agents" (agent))

;; `mevedel-cockpit'
(declare-function mevedel-cockpit-context-data-buffer
                  "mevedel-cockpit" (&optional context))
(declare-function mevedel-cockpit-context-session
                  "mevedel-cockpit" (&optional context))
(declare-function mevedel-cockpit-current-context
                  "mevedel-cockpit" ())
(declare-function mevedel-cockpit-open-surface
                  "mevedel-cockpit" (surface &optional context))
(declare-function mevedel-cockpit-quit "mevedel-cockpit" (&optional label))
(declare-function mevedel-cockpit-setup-tabulated-surface
                  "mevedel-cockpit" (surface))
(declare-function mevedel-cockpit-show-help
                  "mevedel-cockpit" (buffer text))
(declare-function mevedel-cockpit-surface-context
                  "mevedel-cockpit" (&optional surface))
(declare-function mevedel-cockpit-surface-details
                  "mevedel-cockpit" ())
(declare-function mevedel-cockpit-surface-help
                  "mevedel-cockpit" ())
(declare-function mevedel-cockpit-surface-key-help-text
                  "mevedel-cockpit" (&optional surface))
(declare-function mevedel-cockpit-surface-quit
                  "mevedel-cockpit" ())
(declare-function mevedel-cockpit-surface-refresh
                  "mevedel-cockpit" (&optional selected-id))
(declare-function mevedel-cockpit-surface-selected
                  "mevedel-cockpit" (&optional no-error))

;; `mevedel-mentions'
(declare-function mevedel-mentions--valid-mention-context-p
                  "mevedel-mentions" (match-beg))
(declare-function mevedel-mentions-replace-with-placeholder
                  "mevedel-mentions" (start end placeholder))

;; `mevedel-pipeline'
(declare-function mevedel-pipeline--format-render-data-block
                  "mevedel-pipeline" (render-data))
(declare-function mevedel-pipeline--positional-to-plist
                  "mevedel-pipeline" (arg-values arg-specs))
(declare-function mevedel-pipeline-run-tool "mevedel-pipeline"
                  (tool callback args))

;; `mevedel-reminders'
(defvar mevedel-reminders--current-chat-buffer)

;; `mevedel-review'
(declare-function mevedel-review-transform-outcome
                  "mevedel-review" (skill-name outcome))

;; `mevedel-structs'
(declare-function mevedel-request-begin "mevedel-structs"
                  (session &optional directive-uuid))
(declare-function mevedel-request-end "mevedel-structs" ())
(declare-function mevedel-request-hook-rules
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-request-skill-effort-override
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-request-skill-model-override
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-request-skill-permission-rules
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-invoked-skills
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-save-path "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-session-id "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-turn-count "mevedel-structs" (cl-x) t)
(defvar mevedel--current-directive-uuid)
(defvar mevedel--current-request)
(defvar mevedel--data-buffer)
(defvar mevedel--session)
(defvar mevedel--view-buffer)

;; `mevedel-tool-exec'
(declare-function mevedel-tool-exec--register "mevedel-tool-exec" ())

;; `mevedel-tool-plan'
(declare-function mevedel-plan-mode-enter "mevedel-tool-plan"
                  (&optional prompt))
(declare-function mevedel-plan-mode-exit "mevedel-tool-plan"
                  (&optional target-mode))

;; `mevedel-tool-registry'
(declare-function mevedel-tool-get "mevedel-tool-registry" (name &optional category))
(declare-function mevedel-tool-get-path "mevedel-tool-registry" (cl-x) t)

;; `mevedel-tool-ui'
;; Use `t' for the arglist: cl-defun with &key keywords confuses the
;; byte-compiler's arity check (it counts each keyword as one arg
;; rather than a pair), producing spurious "called with N args, accepts
;; only M" warnings.
(declare-function mevedel-tools--task "mevedel-tool-ui" t t)

;; `mevedel-tools'
(declare-function mevedel-tools--register-builtins "mevedel-tools" ())

;; `mevedel-transcript'
(declare-function mevedel-transcript-prompt-transform-start
                  "mevedel-transcript" ())

;; `mevedel-utilities'
(declare-function mevedel--clear-user-turn-gptel-properties
                  "mevedel-utilities" (start end))
(declare-function mevedel--format-hook-audit-record
                  "mevedel-utilities" (record))
(declare-function mevedel--hook-prompt-rewrite-audit-record
                  "mevedel-utilities"
                  (event original submitted &optional reason))
(declare-function mevedel--restore-render-data-gptel-properties
                  "mevedel-utilities" (start end))

;; `mevedel-view'
(declare-function mevedel-view-refresh-input-prompt "mevedel-view" ())

;; `mevedel-workspace'
(declare-function mevedel-workspace-root "mevedel-workspace" (workspace) t)

;; `project'
(declare-function project-current "project" (&optional maybe-prompt directory))
(declare-function project-root "project" (project))

;; `subr'
(defvar read-eval)

;; `tabulated-list'
(declare-function tabulated-list-get-id "tabulated-list" ())
(declare-function tabulated-list-init-header "tabulated-list" ())
(declare-function tabulated-list-mode "tabulated-list" ())
(defvar tabulated-list-entries)
(defvar tabulated-list-format)
(defvar tabulated-list-padding)
(defvar tabulated-list-sort-key)

;; `text-property-search'
(declare-function prop-match-end "text-property-search" (match))
(declare-function text-property-search-backward "text-property-search"
                  (property &optional value predicate not-current))

;; `mevedel-compact'
(declare-function mevedel-compact "mevedel-compact"
                  (&optional aggressive instructions))
(declare-function mevedel--estimate-tokens "mevedel-compact" ())
(defvar mevedel-compact-context-limit)

;; `gptel'
(declare-function gptel-send "ext:gptel" (&optional arg))
(declare-function gptel-make-fsm "ext:gptel-request" (&rest args))
(declare-function gptel-backend-models "ext:gptel-request" (cl-x) t)
(declare-function gptel--update-status "ext:gptel" (msg &optional face))
(declare-function gptel--model-name "ext:gptel" (model))
(defvar gptel-backend)
(defvar gptel-prompt-prefix-alist)
(defvar gptel-model)
(defvar gptel-post-response-functions)
(defvar gptel-response-separator)

;; `mevedel-models'
(declare-function mevedel-model-apply-provider-to-info
                  "mevedel-models" (info provider))
(declare-function mevedel-model-parse-selector "mevedel-models" (value))
(declare-function mevedel-model-resolve-provider
                  "mevedel-models" (spec &optional noerror))
(declare-function mevedel-model-resolve-selector
                  "mevedel-models" (selector &optional noerror))

;; `mevedel-menu'
(declare-function mevedel-menu-open "mevedel-menu" (area))

;; `mevedel-permissions'
(defvar mevedel-permission-mode)

;; `mevedel-plugins'
(declare-function mevedel-plugin-name "mevedel-plugins" (cl-x) t)
(declare-function mevedel-plugin-skills-dir "mevedel-plugins" (cl-x) t)
(declare-function mevedel-plugins-enabled "mevedel-plugins"
                  (&optional workspace))
(declare-function mevedel-plugins-list "mevedel-plugins" ())
(declare-function mevedel-plugins-skill-dirs "mevedel-plugins"
                  (&optional workspace))
(declare-function mevedel-plugins-slash-command "mevedel-plugins" (args))
(autoload 'mevedel-plugins-slash-command "mevedel-plugins" nil nil)

;; `mevedel-session-persistence'
(declare-function mevedel-session-persistence--refresh-visited-file-modtime-or-error
                  "mevedel-session-persistence" (&optional expected-texts))
(declare-function mevedel-session-persistence-save
                  "mevedel-session-persistence" (session buffer))
(declare-function mevedel-session-persistence-start-fresh-segment
                  "mevedel-session-persistence" (session buffer &rest args))
(defvar mevedel-session-persistence)
(defvar mevedel-session--read-only-mode)
(defvar mevedel-session--save-failed)

;; `mevedel-presets'
(declare-function mevedel--run-turn-terminal-hook
                  "mevedel-presets" (fsm event status))


;;
;;; Customization

(defcustom mevedel-skill-dirs
  '(".mevedel/skills/"
    ".agents/skills/"
    "~/.mevedel/skills/"
    "~/.agents/skills/")
  "Directories scanned for SKILL.md files.

Absolute paths and `~'-prefixed paths are scanned as-is and classified
as `user' skills.  Relative paths are resolved against the current
workspace root and classified as `project' skills.  Default directories
scan local mevedel resources, local shared agent resources, global
mevedel resources, then global shared agent resources."
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
present and valid, otherwise the skill directory name.  Raw skill names
must match `[a-z0-9-]+' and be 1-64 chars.  Discovery may later prefix
conflicting local names with `source:name'.  Plugin skills are
namespaced after loading as `plugin-name:skill-name'.  Invalid skills
are skipped at scan time with a warning.  DISPLAY-NAME is the
human-friendly label used in completion annotations and listing output;
defaults to NAME when the frontmatter omits `display-name'.  DESCRIPTION
is the listing line shown to the model; falls back to the first non-empty
paragraph/header of the body when frontmatter omits `description'.  BODY
is the skill prompt text; populated lazily on first
invocation.  SOURCE-FILE and SOURCE-DIR point at the SKILL.md and
its containing directory.  SOURCE is a symbol tagging the origin
scope/type \\=(`user', `project', `managed', `plugin', `bundled').
SOURCE-FAMILY is `mevedel', `agents', or nil and distinguishes ordinary
resource roots without changing SOURCE's existing scope meaning.
USER-INVOCABLE-P and MODEL-INVOCABLE-P gate `$name' dispatch and
the model-facing skills roster respectively.  CONTEXT is `inline'
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
HOOKS is the normalized frontmatter `hooks' value.
ACTIVE-P records the current activation state for path-scoped
skills."
  name
  display-name
  description
  body
  source-file
  source-dir
  source
  source-family
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
;;; Skill state

(defconst mevedel-skills--state-file-name "skills-state.el"
  "Global user skill state file under `mevedel-user-dir'.")

(defun mevedel-skills--state-file ()
  "Return the global skill state file path."
  (file-name-concat mevedel-user-dir mevedel-skills--state-file-name))

(defun mevedel-skills--read-state ()
  "Read global skill state, returning a plist."
  (let ((file (mevedel-skills--state-file)))
    (if (not (file-readable-p file))
        nil
      (condition-case nil
          (with-temp-buffer
            (let ((read-eval nil))
              (insert-file-contents file)
              (let ((state (read (current-buffer))))
                (and (listp state) state))))
        (error nil)))))

(defun mevedel-skills--write-state (state)
  "Write global skill STATE plist."
  (let* ((file (mevedel-skills--state-file))
         (dir (file-name-directory file)))
    (unless (file-directory-p dir)
      (make-directory dir t))
    (with-temp-file file
      (insert ";; Mevedel skill state\n")
      (insert ";; Auto-generated, safe to edit\n\n")
      (pp state (current-buffer)))))

(defun mevedel-skills--disabled-names ()
  "Return persisted disabled skill names."
  (cl-remove-if-not
   #'stringp
   (plist-get (mevedel-skills--read-state) :disabled)))

(defun mevedel-skills--disabled-keys ()
  "Return persisted disabled stable skill keys."
  (cl-remove-if-not
   #'stringp
   (plist-get (mevedel-skills--read-state) :disabled-keys)))

(defun mevedel-skills--state-key (skill)
  "Return stable persisted state key for SKILL."
  (when-let* ((file (and (mevedel-skill-p skill)
                         (mevedel-skill-source-file skill))))
    (concat "file:" (or (ignore-errors (file-truename file))
                        (expand-file-name file)))))

(defun mevedel-skills--state-name-variants (skill name)
  "Return persisted disabled-name variants for SKILL named NAME."
  (let ((names (and (stringp name) (list name))))
    (when (and skill
               (stringp name)
               (not (eq (mevedel-skill-source skill) 'plugin))
               (string-match "\\`[^:]+:\\(.+\\)\\'" name))
      (push (match-string 1 name) names))
    (delete-dups names)))

(defun mevedel-skills--set-enabled (skill-or-name enabled)
  "Persist SKILL-OR-NAME as enabled or disabled according to ENABLED."
  (let* ((skill (and (mevedel-skill-p skill-or-name) skill-or-name))
         (name (if skill
                   (mevedel-skill-name skill)
                 skill-or-name))
         (key (and skill (mevedel-skills--state-key skill))))
    (unless (and (stringp name) (not (string-empty-p name)))
      (user-error "Skill name is required"))
    (let* ((state (or (mevedel-skills--read-state) nil))
           (disabled (mevedel-skills--disabled-names))
           (disabled-keys (if key
                              (cl-remove key
                                         (mevedel-skills--disabled-keys)
                                         :test #'equal)
                            (mevedel-skills--disabled-keys))))
      (dolist (variant (mevedel-skills--state-name-variants skill name))
        (setq disabled (cl-remove variant disabled :test #'equal)))
      (unless enabled
        (if key
            (push key disabled-keys)
          (push name disabled)))
      (setq disabled (sort (delete-dups disabled) #'string<))
      (setq disabled-keys (sort (delete-dups disabled-keys) #'string<))
      (setq state (plist-put state :disabled disabled))
      (setq state (plist-put state :disabled-keys disabled-keys))
      (mevedel-skills--write-state state))))

(defun mevedel-skills--skill-enabled-p (skill-or-name)
  "Return non-nil when SKILL-OR-NAME is not user-disabled."
  (let* ((skill (and (mevedel-skill-p skill-or-name) skill-or-name))
         (name (if skill
                   (mevedel-skill-name skill)
                 skill-or-name))
         (key (and skill (mevedel-skills--state-key skill)))
         (disabled (mevedel-skills--disabled-names))
         (disabled-keys (mevedel-skills--disabled-keys)))
    (not (or (cl-some
              (lambda (variant)
                (member variant disabled))
              (mevedel-skills--state-name-variants skill name))
             (and key (member key disabled-keys))))))

(defun mevedel-skills--session-skill-or-name (session name)
  "Return SESSION skill named NAME, or NAME when it is not loaded."
  (or (and session (mevedel-session-get-skill session name))
      name))


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
    (`(,single) (mevedel-skills--coerce-context single))
    (_ 'inline)))

(defconst mevedel-skills--valid-efforts '(low medium high xhigh max)
  "Effort values accepted on the `effort' frontmatter field.")

(defconst mevedel-skills--name-regexp "\\`[a-z0-9-]+\\'"
  "Regexp matching valid skill invocation identifiers.")

(defconst mevedel-skills--name-max-length 64
  "Maximum allowed length for a skill invocation identifier.")

(defun mevedel-skills--valid-name-p (name)
  "Return non-nil when NAME is a valid skill invocation identifier.
Names must match `[a-z0-9-]+' and be 1-64 chars.
Case-sensitive -- `Bad' is rejected."
  (and (stringp name)
       (<= 1 (length name) mevedel-skills--name-max-length)
       (let ((case-fold-search nil))
         (string-match-p mevedel-skills--name-regexp name))))

(defun mevedel-skills--parse-argument-names (val)
  "Parse VAL into a list of argument names.
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
  "Validate shell VAL.  Return `bash', `powershell', or `bash' on error.
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
(declare-function mevedel-hooks-normalize-rules
                  "mevedel-hooks" (rules &optional scope))
(declare-function mevedel-hooks-decision-reason
                  "mevedel-hooks" (decision))
(declare-function mevedel-hooks-run-event "mevedel-hooks"
                  (event event-plist callback
                         &optional session workspace request invocation))
(declare-function mevedel-hooks-event-plist "mevedel-hooks"
                  (event &optional session workspace &rest extra))
(declare-function mevedel-hooks-additional-context-string
                  "mevedel-hooks" (decision &optional event))

(defun mevedel-skills--parse-allowed-tool-rules (entries source-file)
  "Map each ENTRY through `mevedel-permission--parse-rule-string'.

ENTRIES is the raw `allowed-tools' frontmatter list.  Returns a
list of parsed mevedel permission rules.  A malformed entry aborts the whole
skill load: this function signals `user-error' on the first bad
entry, and `mevedel-skills--build-skill''s `condition-case' skips
the offending skill with a warning naming SOURCE-FILE."
  (when entries
    ;; Skill discovery can happen before `mevedel-install' fills the
    ;; registry, but `allowed-tools' parsing validates tool names there.
    (require 'mevedel-tools)
    (mevedel-tools--register-builtins)
    (mevedel-skills--register))
  (mapcar (lambda (entry)
            (condition-case err
                (mevedel-permission--parse-rule-string entry)
              (user-error
               (user-error
                "Malformed allowed-tools entry %S in %s: %s"
                entry source-file (error-message-string err)))))
          entries))

(defun mevedel-skills--hooks-plist-to-rules (hooks)
  "Convert plist-shaped HOOKS frontmatter to hook rule alists.
YAML mappings naturally parse as `(:Event GROUPS ...)'; the hook
subsystem consumes `((Event . GROUPS) ...)'.  Already-alist values are
returned unchanged."
  (cond
   ((not (and (listp hooks) (keywordp (car-safe hooks))))
    hooks)
   (t
    (let (rules)
      (while hooks
        (let ((key (pop hooks))
              (groups (pop hooks)))
          (when (keywordp key)
            (push (cons (intern (substring (symbol-name key) 1))
                        groups)
                  rules))))
      (nreverse rules)))))

(defun mevedel-skills--normalize-hooks (hooks &optional scope)
  "Normalize SKILL.md HOOKS frontmatter for SCOPE request propagation."
  (when hooks
    (require 'mevedel-hooks)
    (mevedel-hooks-normalize-rules
     (mevedel-skills--hooks-plist-to-rules hooks)
     scope)))

(defun mevedel-skills--activate-request-context (request rules hooks)
  "Append skill-scoped RULES and HOOKS to REQUEST."
  (when rules
    (setf (mevedel-request-skill-permission-rules request)
          (append (mevedel-request-skill-permission-rules request)
                  rules)))
  (when hooks
    (setf (mevedel-request-hook-rules request)
          (append (mevedel-request-hook-rules request)
                  hooks))))

(defun mevedel-skills--first-paragraph (body)
  "Return the first non-empty paragraph or header from BODY, or nil.
Used as the description fallback when frontmatter omits a description.
Strips leading `#' header characters and
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
warning when YAML parsing fails."
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

(defun mevedel-skills--from-plist
    (name plist source-file source &optional source-family)
  "Build a `mevedel-skill' from NAME and PLIST parsed from SOURCE-FILE.
SOURCE is the origin tag symbol.  SOURCE-FAMILY is `mevedel',
`agents', or nil.  NAME is the resolved invocation identifier (already
validated by the caller).  Description fallback from the body is the
caller's responsibility -- anything in PLIST's `:description' wins over
the fallback."
  (let* ((description (plist-get plist :description))
         (display-name (plist-get plist :display-name))
         (disable-model (plist-get plist :disable-model-invocation))
         (user-invocable (plist-get plist :user-invocable))
         (context (mevedel-skills--coerce-context (plist-get plist :context)))
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
     :source-file source-file
     :source-dir (file-name-directory source-file)
     :source source
     :source-family source-family
     :user-invocable-p (mevedel-skills--coerce-bool user-invocable t)
     :model-invocable-p (not (mevedel-skills--coerce-bool disable-model nil))
     :context context
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
     :hooks (mevedel-skills--normalize-hooks
             hooks (and (eq context 'fork) 'skill-fork))
     :active-p (null paths))))


;;
;;; Discovery

(defun mevedel-skills--source-family-from-dir (dir)
  "Return the resource family implied by DIR, or nil."
  (when (stringp dir)
    (let ((path (directory-file-name
                 (expand-file-name (substitute-in-file-name dir)))))
      (cond
       ((string-match-p "\\(?:\\`\\|/\\)\\.mevedel/skills\\'" path)
        'mevedel)
       ((string-match-p "\\(?:\\`\\|/\\)\\.agents/skills\\'" path)
        'agents)))))

(defun mevedel-skills--resolve-dir (dir workspace-root)
  "Resolve DIR against WORKSPACE-ROOT, returning (ABSOLUTE . SOURCE).
SOURCE is `user' for absolute/`~'-prefixed paths and `project' for
relative paths resolved against WORKSPACE-ROOT.  Returns nil if DIR is
relative and WORKSPACE-ROOT is nil."
  (cond
   ((consp dir)
    (when-let* ((resolved (mevedel-skills--resolve-dir
                           (car dir) workspace-root)))
      (cons (car resolved) (cdr dir))))
   ((file-name-absolute-p dir)
    (let ((family (mevedel-skills--source-family-from-dir dir)))
      (cons (file-name-as-directory (expand-file-name dir))
            (if family (cons 'user family) 'user))))
   (workspace-root
    (let ((family (mevedel-skills--source-family-from-dir dir)))
      (cons (file-name-as-directory
             (expand-file-name dir workspace-root))
            (if family (cons 'project family) 'project))))))

(defun mevedel-skills--scan-resolved-dir (dir source)
  "Scan DIR with SOURCE, applying plugin namespacing when requested."
  (let* ((plugin-name (and (consp source)
                           (eq (car source) 'plugin)
                           (cdr source)))
         (source-tag (if (consp source) (car source) source))
         (source-family (and (consp source)
                             (memq (cdr source) '(mevedel agents))
                             (cdr source)))
         skills)
    (dolist (skill (mevedel-skills--scan-dir
                    dir source-tag source-family)
                   (nreverse skills))
      (push (if plugin-name
                (mevedel-skills--namespace-plugin-skill plugin-name skill)
              skill)
            skills))))

(defun mevedel-skills--plugin-skill-dir-entries (&optional workspace)
  "Return (PLUGIN-NAME . DIR) entries from plugins enabled in WORKSPACE."
  (require 'mevedel-plugins)
  (mapcar (lambda (entry)
            (cons (cdr (cdr entry)) (car entry)))
          (mevedel-plugins-skill-dirs workspace)))

(defun mevedel-skills--plugin-skill-dirs (&optional workspace)
  "Return existing skill directories from plugins enabled in WORKSPACE."
  (mapcar #'cdr (mevedel-skills--plugin-skill-dir-entries workspace)))

(defun mevedel-skills--namespace-plugin-skill (plugin-name skill)
  "Prefix plugin SKILL with PLUGIN-NAME for user-facing invocation."
  (let ((name (format "%s:%s" plugin-name (mevedel-skill-name skill))))
    (setf (mevedel-skill-name skill) name)
    (setf (mevedel-skill-display-name skill) name)
    skill))

(defun mevedel-skills--load-body-string (skill-file)
  "Return the markdown body of SKILL-FILE as a string, or nil.
Used during scan to compute the description fallback when frontmatter
omits `description'."
  (let ((parsed (ignore-errors (gptel-agent-read-file skill-file))))
    (plist-get (cdr parsed) :system)))

(defun mevedel-skills--build-skill
    (skill-file source &optional source-family)
  "Build a `mevedel-skill' for SKILL-FILE with SOURCE origin tag.
SOURCE-FAMILY distinguishes `.mevedel' and `.agents' resource roots.
Performs name resolution (frontmatter `:name' > directory name) and
validation.  Returns nil with a warning when the skill is invalid
\\(bad name, etc.).  Computes description fallback from the body when
frontmatter omits `description'."
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
          (mevedel-skills--from-plist
           raw-name plist skill-file source source-family)))))))

(defun mevedel-skills--scan-dir (dir source &optional source-family)
  "Return a list of `mevedel-skill' structs found under DIR.
SOURCE is the origin tag applied to every skill scanned from DIR.
SOURCE-FAMILY distinguishes `.mevedel' and `.agents' resource roots.
Each SKILL.md is wrapped in `condition-case' so a single bad skill
does not abort the whole scan."
  (when (file-directory-p dir)
    (let (result)
      (dolist (skill-file (directory-files-recursively
                           dir "\\`SKILL\\.md\\'" nil nil t))
        (let ((skill (condition-case err
                         (mevedel-skills--build-skill
                          skill-file source source-family)
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

(defconst mevedel-skills--source-prefixes
  '((project . "local")
    (user . "global")
    (bundled . "bundled")
    (managed . "managed"))
  "Source prefixes used when non-plugin skill names collide.")

(defun mevedel-skills--source-prefix (skill)
  "Return the fallback conflict prefix for SKILL, or nil for plugin skills."
  (let ((source (mevedel-skill-source skill)))
    (cond
     ((eq source 'plugin) nil)
     ((alist-get source mevedel-skills--source-prefixes))
     ((symbolp source) (symbol-name source)))))

(defun mevedel-skills--ordinary-skill-p (skill)
  "Return non-nil for user/project resource-root SKILL."
  (memq (mevedel-skill-source skill) '(user project)))

(defun mevedel-skills--conflict-prefix (skill group)
  "Return the visible conflict prefix for SKILL in GROUP."
  (cond
   ((eq (mevedel-skill-source skill) 'plugin) nil)
   ((not (mevedel-skills--ordinary-skill-p skill))
    (mevedel-skills--source-prefix skill))
   (t
    (let* ((ordinary (cl-remove-if-not
                      #'mevedel-skills--ordinary-skill-p group))
           (scope-key (lambda (s)
                        (pcase (mevedel-skill-source s)
                          ('project "local")
                          ('user "global"))))
           (family-key (lambda (s)
                         (pcase (mevedel-skill-source-family s)
                           ('mevedel "mevedel")
                           ('agents "agents"))))
           (scope (funcall scope-key skill))
           (family (funcall family-key skill)))
      (or (and family
               (cl-every family-key ordinary)
               (= 1 (cl-count family ordinary
                              :key family-key :test #'equal))
               family)
          (and scope
               (= 1 (cl-count scope ordinary
                              :key scope-key :test #'equal))
               scope)
          (and scope family (format "%s-%s" scope family))
          (mevedel-skills--source-prefix skill))))))

(defun mevedel-skills--qualified-name-p (name)
  "Return non-nil when NAME already has a visible prefix."
  (and (stringp name) (string-search ":" name)))

(defun mevedel-skills--qualify-conflicting-names (skills)
  "Return SKILLS with deterministic visible names.

Unique non-plugin names stay unqualified.  Same-origin duplicates keep
the first entry.  Cross-origin non-plugin conflicts are exposed with the
shortest deterministic unique prefix.  Already-qualified names,
including plugin skills, are left untouched."
  (let ((seen (make-hash-table :test #'equal))
        (groups (make-hash-table :test #'equal))
        deduped)
    (dolist (skill skills)
      (let ((key (list (mevedel-skill-source skill)
                       (mevedel-skill-source-family skill)
                       (mevedel-skill-name skill))))
        (unless (gethash key seen)
          (puthash key t seen)
          (push skill deduped))))
    (setq deduped (nreverse deduped))
    (dolist (skill deduped)
      (let ((name (mevedel-skill-name skill)))
        (unless (or (eq (mevedel-skill-source skill) 'plugin)
                    (mevedel-skills--qualified-name-p name))
          (puthash name (cons skill (gethash name groups)) groups))))
    (maphash
     (lambda (name group)
       (when (> (length group) 1)
         (dolist (skill group)
           (when-let* ((prefix (mevedel-skills--conflict-prefix
                                skill group)))
             (setf (mevedel-skill-name skill)
                   (format "%s:%s" prefix name))))))
     groups)
    deduped))

(defun mevedel-skills-scan (&optional workspace-root dirs workspace)
  "Scan skill directories and return a list of `mevedel-skill' structs.

DIRS defaults to `mevedel-skill-dirs'.  WORKSPACE-ROOT is used to
resolve relative entries; when nil, relative entries are skipped.
Unique names remain unqualified.  When user/project/bundled/managed
sources collide, all colliding entries are exposed as `source:name'.
Same-source duplicates keep the first entry.  After scanning
user/project directories, mevedel's bundled skills directory is
scanned, then plugins enabled in WORKSPACE are scanned last under
`plugin-name:skill-name' names.
Plugin skills are scanned only when DIRS is omitted, preserving exact
explicit scans for tests and internal callers."
  (let ((include-plugins (null dirs))
        (dirs (or dirs mevedel-skill-dirs))
        result)
    (dolist (raw dirs)
      (when-let* ((resolved (mevedel-skills--resolve-dir raw workspace-root))
                  (dir (car resolved))
                  (source (cdr resolved)))
        (dolist (skill (mevedel-skills--scan-resolved-dir dir source))
          (push skill result))))
    (when (and mevedel-skills-include-bundled
               (file-directory-p mevedel-skills--bundled-dir))
      (dolist (skill (mevedel-skills--scan-dir
                      mevedel-skills--bundled-dir 'bundled))
        (push skill result)))
    (when include-plugins
      (require 'mevedel-plugins)
      (dolist (entry (mevedel-plugins-skill-dirs workspace))
        (when-let* ((resolved (mevedel-skills--resolve-dir
                               entry workspace-root)))
          (dolist (skill (mevedel-skills--scan-resolved-dir
                          (car resolved) (cdr resolved)))
            (push skill result)))))
    (mevedel-skills--qualify-conflicting-names (nreverse result))))


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

;; The dirty-buffer set is defined further down with the rest of the
;; modification-detection state.  Forward-declare it here so the
;; install function's `remhash' call compiles cleanly.
(defvar mevedel-skills--dirty-buffers)

(defun mevedel-skills--preserve-active-skills (skills old-skills)
  "Copy runtime path-scoped activation from OLD-SKILLS into SKILLS.
Path-scoped skills start dormant from frontmatter, but can become
active during a session after a matching file touch.  Rescans rebuild
skill structs from disk, so preserve that runtime state for skills
with the same name and source file."
  (let ((active-keys (make-hash-table :test #'equal)))
    (dolist (skill old-skills)
      (when (and (mevedel-skill-path-patterns skill)
                 (mevedel-skill-active-p skill))
        (puthash (cons (mevedel-skill-name skill)
                       (mevedel-skill-source-file skill))
                 t active-keys)))
    (dolist (skill skills)
      (when (and (mevedel-skill-path-patterns skill)
                 (gethash (cons (mevedel-skill-name skill)
                                (mevedel-skill-source-file skill))
                          active-keys))
        (setf (mevedel-skill-active-p skill) t)))
    skills))

(defun mevedel-skills-install (session &optional buffer)
  "Populate SESSION's skills slot by scanning `mevedel-skill-dirs'.

Uses SESSION's workspace root to resolve relative directory entries.
Idempotent: existing entries on SESSION's skills slot are replaced.

BUFFER is the chat buffer that consumes this session's skills; it
defaults to the current buffer.  When non-nil it is registered as a
consumer of every resolved skill directory so that hot-reload events
can mark the right sessions dirty (see
`mevedel-skills-check-for-modifications').  Watcher and mtime caches
are refreshed to match the freshly scanned skill set."
  (let* ((ws (mevedel-session-workspace session))
         (root (and ws (mevedel-workspace-root ws)))
         (buffer (or buffer (current-buffer)))
         (old-skills (mevedel-session-skills session))
         (skills (mevedel-skills--preserve-active-skills
                  (mevedel-skills-scan root nil ws) old-skills))
         (dirs (mevedel-skills--collect-roots root skills ws)))
    (setf (mevedel-session-skills session) skills)
    (when (buffer-live-p buffer)
      (mevedel-skills--register-buffer buffer dirs))
    (mevedel-skills--refresh-mtime-cache skills buffer)
    (remhash buffer mevedel-skills--dirty-buffers)
    session))

(defun mevedel-session-get-skill (session name)
  "Return the skill named NAME from SESSION, or nil if not found.
When called inside a chat buffer, run a hot-reload pull-check first so
external skill changes are picked up before lookup."
  (when (buffer-live-p (current-buffer))
    (mevedel-skills--ensure-fresh (current-buffer) session))
  (cl-find name (mevedel-session-skills session)
           :key #'mevedel-skill-name :test #'equal))


;;
;;; Modification detection (hot reload)

(defun mevedel-skills--filenotify-supported-p ()
  "Return non-nil when this Emacs has filesystem-notification support.
Probes for any of the backend `*-add-watch' primitives Emacs uses to
implement `file-notify-add-watch'.  Avoids depending on internals
\\(`file-notify--library') which moved between Emacs versions."
  (and (require 'filenotify nil 'noerror)
       (or (fboundp 'inotify-add-watch)
           (fboundp 'kqueue-add-watch)
           (fboundp 'gfile-add-watch)
           (fboundp 'w32notify-add-watch))))

(defun mevedel-skills--default-modification-checking ()
  "Return the default value for `mevedel-skills-check-for-modifications'.
Always includes `check-on-save' (zero external deps).  Adds
`watch-files' when this Emacs has filesystem-notification support."
  (let ((strategies (list 'check-on-save)))
    (when (mevedel-skills--filenotify-supported-p)
      (push 'watch-files strategies))
    (nreverse strategies)))

(defcustom mevedel-skills-check-for-modifications
  (mevedel-skills--default-modification-checking)
  "When and how to detect changes to skill files on disk.

A list of symbols.  Strategies compose; each one independently flips
a per-chat-buffer dirty flag that is consumed by the next pull-check
in `mevedel-slash-capf', `mevedel-session-get-skill', the dynamic
skills prompt section, and skill-change reminders.

Recognized strategies:

`check-on-save'      Hook `before-save-hook' globally; saving a
                     SKILL.md inside a registered skill directory
                     marks every consumer of that directory dirty.
                     Catches in-Emacs edits with zero external deps.

`watch-files'        Use `file-notify-add-watch' on each registered
                     top-level skill directory plus every leaf
                     directory currently containing a SKILL.md.
                     Catches creates, deletes, renames and external
                     edits to existing skills.  Requires
                     filesystem-notification support in Emacs.

`stat-when-checking' At pull-check time, stat each known SKILL.md
                     against a cached modtime.  Catches external
                     edits even on remote/networked filesystems
                     where `file-notify' is unreliable.  O(N) stats
                     per pull-check where N is the skill count, so
                     enable selectively.

The default selects `check-on-save' plus `watch-files' on platforms
that support filesystem notifications.  Set to nil to disable
hot-reload entirely; users can then run \\[mevedel-skills-rescan]
manually."
  :type '(set (const :tag "Detect in-Emacs saves via `before-save-hook'"
                     check-on-save)
              (const :tag "Watch skill directories with `file-notify'"
                     watch-files)
              (const :tag "Stat skill files at pull-check time"
                     stat-when-checking))
  :group 'mevedel
  :set (lambda (sym val)
         (set-default sym val)
         (when (fboundp 'mevedel-skills--reconcile-strategies)
           (mevedel-skills--reconcile-strategies))))

(defun mevedel-skills--strategy-active-p (sym)
  "Return non-nil when SYM is in `mevedel-skills-check-for-modifications'."
  (memq sym mevedel-skills-check-for-modifications))


;;;; State

(defvar mevedel-skills--watchers (make-hash-table :test #'equal)
  "Live `file-notify' watchers keyed by absolute directory path.
Values are descriptors returned by `file-notify-add-watch'.  Entries
are added by `mevedel-skills--ensure-watcher' and removed by
`mevedel-skills--remove-watcher-if-unused' when no buffer consumes
them anymore.")

(defvar mevedel-skills--dir-buffers (make-hash-table :test #'equal)
  "Map of absolute directory path to list of consumer chat buffers.
A buffer joins the list for every directory its session resolves
\\(top-level + existing subdirectories + any leaf containing a SKILL.md)
so that watcher events on that directory can identify which sessions
to mark dirty.")

(defvar mevedel-skills--dirty-buffers (make-hash-table :test #'eq)
  "Hash set of chat buffers awaiting a skills rescan.
Membership is added by save-hook, watcher callback, or stat recheck
and cleared by `mevedel-skills-install' (which performs the rescan).")

(defvar mevedel-skills--mtime-cache (make-hash-table :test #'equal)
  "Map of (BUFFER . SKILL.md absolute path) to last-known mtime.")


;;;; Buffer registry

(defun mevedel-skills--nearest-existing-directory (dir)
  "Return the nearest existing ancestor directory of DIR, or nil."
  (let ((dir (file-name-as-directory (expand-file-name dir)))
        parent)
    (catch 'found
      (while dir
        (when (file-directory-p dir)
          (throw 'found dir))
        (setq parent (file-name-directory (directory-file-name dir)))
        (if (or (null parent) (equal parent dir))
            (throw 'found nil)
          (setq dir (file-name-as-directory parent)))))))

(defun mevedel-skills--existing-subdirectories (dir)
  "Return every existing subdirectory under DIR, including DIR.
Directory file notifications are not recursive, so `watch-files'
needs to watch pre-existing subdirectories as well as the configured
skill root."
  (let ((dir (file-name-as-directory (expand-file-name dir)))
        result)
    (when (file-directory-p dir)
      (push dir result)
      (dolist (entry (directory-files dir t directory-files-no-dot-files-regexp))
        (when (and (file-directory-p entry)
                   (not (file-symlink-p entry)))
          (setq result
                (append (mevedel-skills--existing-subdirectories entry)
                        result)))))
    result))

(defun mevedel-skills--collect-roots (workspace-root skills &optional workspace)
  "Return the list of directories to watch for SKILLS.
Includes every resolved entry of `mevedel-skill-dirs' plus its
existing subdirectories, enabled plugin skill directories, the nearest
existing parent for roots that do not yet exist, and every directory
that currently contains one of SKILLS' source files (leaves).  Bundled
skills are intentionally excluded -- they only change when mevedel
itself updates and would waste a watcher slot.  WORKSPACE-ROOT is used
to resolve relative entries; relative entries are skipped when it is
nil.  Plugin roots are resolved from plugins enabled in WORKSPACE."
  (let ((dirs (make-hash-table :test #'equal)))
    (dolist (raw mevedel-skill-dirs)
      (when-let* ((resolved (mevedel-skills--resolve-dir raw workspace-root))
                  (dir (car resolved)))
        (puthash dir t dirs)
        (if (file-directory-p dir)
            (dolist (subdir (mevedel-skills--existing-subdirectories dir))
              (puthash subdir t dirs))
          (when-let* ((parent (mevedel-skills--nearest-existing-directory
                               dir)))
            (puthash parent t dirs)))))
    (dolist (dir (mevedel-skills--plugin-skill-dirs workspace))
      (puthash dir t dirs)
      (dolist (subdir (mevedel-skills--existing-subdirectories dir))
        (puthash subdir t dirs)))
    (dolist (skill skills)
      (unless (eq (mevedel-skill-source skill) 'bundled)
        (when-let* ((file (mevedel-skill-source-file skill))
                    (leaf (file-name-directory file)))
          (puthash (file-name-as-directory leaf) t dirs))))
    (let (result)
      (maphash (lambda (k _v) (push k result)) dirs)
      result)))

(defun mevedel-skills--register-buffer (buffer dirs)
  "Register BUFFER as a consumer of every directory in DIRS.
Drops BUFFER's prior registrations to keep the registry consistent
when the resolved directory set changes (e.g. a leaf directory was
deleted), then installs watchers on the new set when `watch-files' is
active.  Removes now-orphan watchers."
  (mevedel-skills--unregister-buffer buffer)
  (dolist (dir dirs)
    (let ((entry (gethash dir mevedel-skills--dir-buffers)))
      (puthash dir (cons buffer entry) mevedel-skills--dir-buffers)))
  (when (mevedel-skills--strategy-active-p 'watch-files)
    (dolist (dir dirs)
      (mevedel-skills--ensure-watcher dir)))
  (mevedel-skills--gc-watchers))

(defun mevedel-skills--unregister-buffer (buffer)
  "Drop BUFFER from every directory's consumer list.
Watchers whose consumer list becomes empty are torn down."
  (let (remove update touched)
    (maphash (lambda (dir consumers)
               (when (memq buffer consumers)
                 (let ((rest (delq buffer (copy-sequence consumers))))
                   (if rest
                       (push (cons dir rest) update)
                     (push dir remove)
                     (push dir touched)))))
             mevedel-skills--dir-buffers)
    (dolist (dir remove)
      (remhash dir mevedel-skills--dir-buffers))
    (dolist (entry update)
      (puthash (car entry) (cdr entry) mevedel-skills--dir-buffers))
    (dolist (dir touched)
      (mevedel-skills--remove-watcher-if-unused dir)))
  (remhash buffer mevedel-skills--dirty-buffers)
  (mevedel-skills--clear-mtime-cache-for-buffer buffer))

(defun mevedel-skills--gc-watchers ()
  "Remove watchers whose directories no longer have any consumer."
  (let (orphans)
    (maphash (lambda (dir _desc)
               (unless (gethash dir mevedel-skills--dir-buffers)
                 (push dir orphans)))
             mevedel-skills--watchers)
    (dolist (dir orphans)
      (mevedel-skills--remove-watcher-if-unused dir))))

(defun mevedel-skills--release-on-kill ()
  "Buffer-local `kill-buffer-hook' that drops this buffer from the registry."
  (mevedel-skills--unregister-buffer (current-buffer)))


;;;; Marking dirty

(defun mevedel-skills--mark-dir-dirty (dir)
  "Mark every chat buffer consuming DIR as needing a rescan."
  (let ((dir (file-name-as-directory (expand-file-name dir))))
    (dolist (buffer (gethash dir mevedel-skills--dir-buffers))
      (when (buffer-live-p buffer)
        (puthash buffer t mevedel-skills--dirty-buffers)))))

(defun mevedel-skills--mark-buffer-dirty (buffer)
  "Mark BUFFER as needing a rescan."
  (when (buffer-live-p buffer)
    (puthash buffer t mevedel-skills--dirty-buffers)))


;;;; check-on-save strategy

(defun mevedel-skills--file-under-watched-dirs (file)
  "Return registered directories containing FILE.
Walks the entries of `mevedel-skills--dir-buffers' so this only
returns directories that have at least one consumer."
  (when (and (stringp file) (not (string-empty-p file)))
    (let* ((file (expand-file-name file))
           hits)
      (maphash (lambda (dir _consumers)
                 (when (string-prefix-p dir file)
                   (push dir hits)))
               mevedel-skills--dir-buffers)
      hits)))

(defun mevedel-skills--before-save-hook ()
  "Mark consumers dirty when saving a SKILL.md under a registered dir."
  (when-let* ((file buffer-file-name)
              ((string-equal "SKILL.md" (file-name-nondirectory file))))
    (dolist (dir (mevedel-skills--file-under-watched-dirs file))
      (mevedel-skills--mark-dir-dirty dir))))


;;;; watch-files strategy

(declare-function file-notify-add-watch "filenotify"
                  (file flags callback))
(declare-function file-notify-rm-watch "filenotify" (descriptor))
(declare-function file-notify-valid-p "filenotify" (descriptor))

(defun mevedel-skills--watch-callback (event)
  "Mark the firing directory dirty for every consumer.
EVENT is `(DESCRIPTOR ACTION FILE [FILE2])'.  Filtering on action is
deliberately permissive: a `created'/`changed'/`deleted'/`renamed'/
`attribute-changed' event under a watched directory always triggers
a rescan, which discovers new leaves, dropped leaves, and edited
SKILL.md files in one walk.  The `stopped' action is ignored."
  (let ((descriptor (nth 0 event))
        (action (nth 1 event)))
    (unless (eq action 'stopped)
      (maphash (lambda (dir desc)
                 (when (equal desc descriptor)
                   (mevedel-skills--mark-dir-dirty dir)))
               mevedel-skills--watchers))))

(defun mevedel-skills--ensure-watcher (dir)
  "Install a `file-notify' watcher on DIR if none is live yet.
Silently no-ops on platforms without filenotify support, or when DIR
does not exist.  Errors during watcher installation are downgraded to
warnings -- a missing watcher only degrades hot-reload, it never
breaks normal operation."
  (let ((dir (file-name-as-directory (expand-file-name dir))))
    (when (file-directory-p dir)
      (let ((existing (gethash dir mevedel-skills--watchers)))
        (when (and existing
                   (not (ignore-errors (file-notify-valid-p existing))))
          (remhash dir mevedel-skills--watchers)
          (setq existing nil))
        (unless existing
          (condition-case err
              (let ((desc (file-notify-add-watch
                           dir '(change attribute-change)
                           #'mevedel-skills--watch-callback)))
                (puthash dir desc mevedel-skills--watchers))
            (error
             (display-warning
              'mevedel
              (format "Skill watcher failed for %s: %s"
                      dir (error-message-string err))
              :warning))))))))

(defun mevedel-skills--remove-watcher-if-unused (dir)
  "Tear down the watcher for DIR when no buffer consumes it."
  (let ((dir (file-name-as-directory (expand-file-name dir))))
    (unless (gethash dir mevedel-skills--dir-buffers)
      (when-let ((desc (gethash dir mevedel-skills--watchers)))
        (ignore-errors (file-notify-rm-watch desc))
        (remhash dir mevedel-skills--watchers)))))

(defun mevedel-skills--teardown-all-watchers ()
  "Tear down every live watcher.  Used by strategy reconciliation."
  (maphash (lambda (_dir desc)
             (ignore-errors (file-notify-rm-watch desc)))
           mevedel-skills--watchers)
  (clrhash mevedel-skills--watchers))


;;;; stat-when-checking strategy

(defun mevedel-skills--mtime-cache-key (buffer file)
  "Return the stat-cache key for BUFFER and FILE."
  (cons buffer file))

(defun mevedel-skills--file-mtime (file)
  "Return the modification time of FILE as a Lisp time, or nil."
  (when (and (stringp file)
             (file-readable-p file))
    (file-attribute-modification-time (file-attributes file))))

(defun mevedel-skills--clear-mtime-cache-for-buffer (buffer)
  "Remove every stat-cache entry for BUFFER."
  (let (keys)
    (maphash (lambda (key _mtime)
               (when (eq (car-safe key) buffer)
                 (push key keys)))
             mevedel-skills--mtime-cache)
    (dolist (key keys)
      (remhash key mevedel-skills--mtime-cache))))

(defun mevedel-skills--refresh-mtime-cache (skills buffer)
  "Rebuild BUFFER's `mevedel-skills--mtime-cache' entries from SKILLS.
Stale entries for BUFFER (skill file no longer present in SKILLS) are
dropped so the cache cannot grow unboundedly across rescans."
  (let ((seen (make-hash-table :test #'equal)))
    (dolist (skill skills)
      (when-let* ((file (mevedel-skill-source-file skill))
                  (mtime (mevedel-skills--file-mtime file)))
        (puthash (mevedel-skills--mtime-cache-key buffer file)
                 mtime
                 mevedel-skills--mtime-cache)
        (puthash file t seen)))
    (let (orphans)
      (maphash (lambda (key _mtime)
                 (when (eq (car-safe key) buffer)
                   (let ((file (cdr key)))
                     (unless (gethash file seen)
                       (push key orphans)))))
               mevedel-skills--mtime-cache)
      (dolist (key orphans)
        (remhash key mevedel-skills--mtime-cache)))))

(defun mevedel-skills--stat-recheck (session buffer)
  "Mark BUFFER dirty when any of SESSION's skill files has changed mtime.
Cheap when the skill set is small; one `file-attributes' call per
skill.  The mtime cache is per consumer buffer so one session's rescan
does not mask a pending change from another live session."
  (catch 'dirty
    (dolist (skill (mevedel-session-skills session))
      (when-let* ((file (mevedel-skill-source-file skill)))
        (let ((cached (gethash (mevedel-skills--mtime-cache-key buffer file)
                               mevedel-skills--mtime-cache))
              (current (mevedel-skills--file-mtime file)))
          (unless (and cached (equal cached current))
            (mevedel-skills--mark-buffer-dirty buffer)
            (throw 'dirty t)))))))


;;;; Pull-check

(defun mevedel-skills--ensure-fresh (buffer session)
  "Rescan SESSION's skills if BUFFER is marked dirty.
Optional `stat-when-checking' strategy first stats every known skill
file and may flip the dirty flag.  When the flag is set, calls
`mevedel-skills-install' which clears it.  No-op when BUFFER is
unregistered (e.g. tests that bypass chat-buffer setup)."
  (when (and (buffer-live-p buffer) session)
    (when (mevedel-skills--strategy-active-p 'stat-when-checking)
      (mevedel-skills--stat-recheck session buffer))
    (when (gethash buffer mevedel-skills--dirty-buffers)
      (with-current-buffer buffer
        (mevedel-skills-install session buffer)))))

(defun mevedel-skills--buffer-for-session (session)
  "Return a live buffer whose `mevedel--session' is SESSION, or nil."
  (catch 'found
    (dolist (buffer (buffer-list))
      (when (and (buffer-live-p buffer)
                 (local-variable-p 'mevedel--session buffer)
                 (eq (buffer-local-value 'mevedel--session buffer) session))
        (throw 'found buffer)))))

(defun mevedel-skills--current-reminder-buffer (session)
  "Return the chat buffer whose skills should be refreshed for SESSION."
  (let ((buffer mevedel-reminders--current-chat-buffer))
    (if (and (buffer-live-p buffer)
             (local-variable-p 'mevedel--session buffer)
             (eq (buffer-local-value 'mevedel--session buffer) session))
        buffer
      (mevedel-skills--buffer-for-session session))))


;;;; Strategy reconciliation

(defun mevedel-skills--reconcile-strategies ()
  "Add or remove global hooks/watchers to match the active strategy set.
Called by `mevedel-skills-check-for-modifications''s `:set' and at
load time so toggling the defcustom at runtime takes effect without
restarting Emacs."
  (cond
   ((mevedel-skills--strategy-active-p 'check-on-save)
    (add-hook 'before-save-hook #'mevedel-skills--before-save-hook))
   (t
    (remove-hook 'before-save-hook #'mevedel-skills--before-save-hook)))
  (cond
   ((mevedel-skills--strategy-active-p 'watch-files)
    (maphash (lambda (dir _consumers)
               (mevedel-skills--ensure-watcher dir))
             mevedel-skills--dir-buffers))
   (t
    (mevedel-skills--teardown-all-watchers))))

(defun mevedel-skills-install-hot-reload ()
  "Install global skill hot-reload hooks/watchers for active strategies."
  (mevedel-skills--reconcile-strategies))

(defun mevedel-skills-uninstall-hot-reload ()
  "Remove all global skill hot-reload hooks/watchers and registry state."
  (remove-hook 'before-save-hook #'mevedel-skills--before-save-hook)
  (mevedel-skills--teardown-all-watchers)
  (maphash (lambda (_dir buffers)
             (dolist (buffer buffers)
               (when (buffer-live-p buffer)
                 (with-current-buffer buffer
                   (remove-hook 'kill-buffer-hook
                                #'mevedel-skills--release-on-kill
                                t)))))
           mevedel-skills--dir-buffers)
  (clrhash mevedel-skills--dir-buffers)
  (clrhash mevedel-skills--dirty-buffers)
  (clrhash mevedel-skills--mtime-cache))

(mevedel-skills--reconcile-strategies)


;;;; Manual rescan

;;;###autoload
(defun mevedel-skills-rescan ()
  "Rescan skill directories for the current chat buffer's session.
Use this when a hot-reload strategy missed an event (e.g. on remote
filesystems where `file-notify' is silently unreliable, or when
`mevedel-skills-check-for-modifications' is nil)."
  (interactive)
  (let ((session (and (boundp 'mevedel--session) mevedel--session)))
    (unless session
      (user-error "No mevedel session in this buffer"))
    (mevedel-skills--mark-buffer-dirty (current-buffer))
    (mevedel-skills--ensure-fresh (current-buffer) session)
    (message "Rescanned %d skills"
             (length (mevedel-session-skills session)))))


;;
;;; Request-scoped skill context

(defvar-local mevedel-skills--pending-request-context nil
  "Buffer-local pending request context for the next mevedel-request.

A plist of the form
  (:permission-rules RULES :model MODEL :effort EFFORT
   :hook-rules HOOKS :invoked-skills SKILLS)

populated by user-dispatched skill invocation before `gptel-send'
fires.  Drained into the new `mevedel-request' slots by the
WAIT-state begin handler in `mevedel-presets.el' (see also
`mevedel-skills--drain-pending-context').

Cleared on drain.  Cleared by an `unwind-protect' in the slash
or skill dispatch path if `gptel-send' aborts before request creation.")

(put 'mevedel-skills--pending-request-context 'permanent-local t)

(defvar-local mevedel-skills--pending-inline-attachments nil
  "Prepared inline `$skill' attachments for the next prompt transform.

Each entry is a plist with :name, :body, :skill, and :arguments.
The stash lives on the chat/data buffer.  It is populated before
`gptel-send' and consumed by
`mevedel-skills--transform-expand-inline-attachments' in gptel's
temporary prompt buffer.")

(put 'mevedel-skills--pending-inline-attachments 'permanent-local t)

(defcustom mevedel-skills-max-recursion-depth 4
  "Maximum nesting depth for skill invocations.
A skill body that invokes another skill increments the depth;
exceeding this limit fails the inner invocation with an error
outcome.  Default of 4 allows skill A -> B -> C -> D before failing."
  :type 'integer
  :group 'mevedel)

(defvar mevedel-skills--invoke-depth 0
  "Dynamic depth of nested `mevedel-skills-invoke' calls.
Let-bound around each invocation so the depth naturally pops on
control-flow exit (return, error, throw, abort).")

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
- :hook-rules       -> `mevedel-request-hook-rules'
- :invoked-skills   -> appended to `mevedel-session-invoked-skills'
                       on the request's session"
  (when-let* ((ctx mevedel-skills--pending-request-context))
    (when-let* ((rules (plist-get ctx :permission-rules)))
      (setf (mevedel-request-skill-permission-rules request) rules))
    (when-let* ((model (plist-get ctx :model)))
      (setf (mevedel-request-skill-model-override request) model))
    (when-let* ((effort (plist-get ctx :effort)))
      (setf (mevedel-request-skill-effort-override request) effort))
    (when-let* ((hooks (plist-get ctx :hook-rules)))
      (setf (mevedel-request-hook-rules request) hooks))
    (when-let* ((skills (plist-get ctx :invoked-skills))
                (session mevedel--session))
      (setf (mevedel-session-invoked-skills session)
            (append (mevedel-session-invoked-skills session) skills)))
    (setq-local mevedel-skills--pending-request-context nil)))

(declare-function gptel-fsm-info "ext:gptel-request" (cl-x) t)

(defun mevedel-skills--transform-apply-model-override (fsm)
  "Pre-realize transform: apply skill model overrides to prompt locals.

FSM is the active gptel request state machine.

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

  Effort is parsed and stored on the same slot but not applied here
  -- gptel does not yet expose an effort knob.  When it does, this
  handler will mutate the corresponding info key the same way."
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

(defconst mevedel-skills--non-author-text-property
  'mevedel-skills-non-author-text
  "Text property set on content not written literally in SKILL.md.")

(defconst mevedel-skills--literal-placeholder-property
  'mevedel-skills-literal-placeholder
  "Text property set on escaped placeholders that must stay literal.")

(defvar mevedel-skills--substitution-made-p nil
  "Non-nil when skill variable substitution replaced text.")

(defun mevedel-skills--word-char-p (ch)
  "Return non-nil when CH is a word character (`[A-Za-z0-9_]')."
  (and ch
       (or (and (>= ch ?a) (<= ch ?z))
           (and (>= ch ?A) (<= ch ?Z))
           (and (>= ch ?0) (<= ch ?9))
           (eq ch ?_))))

(defun mevedel-skills--mark-non-author-text (value)
  "Return VALUE marked as text not written literally in SKILL.md."
  (let ((copy (copy-sequence (or value ""))))
    (add-text-properties
     0 (length copy)
     (list mevedel-skills--non-author-text-property t)
     copy)
    copy))

(defun mevedel-skills--property-range-p (text start end property)
  "Return non-nil when TEXT has PROPERTY anywhere from START to END."
  (let ((pos start)
        found)
    (while (and (< pos end) (not found))
      (if (get-text-property pos property text)
          (setq found t)
        (setq pos (or (next-single-property-change pos property text end)
                      end))))
    found))

(defun mevedel-skills--non-author-range-p (text start end)
  "Return non-nil when TEXT has any non-author content from START to END."
  (mevedel-skills--property-range-p
   text start end mevedel-skills--non-author-text-property))

(defun mevedel-skills--literal-placeholder-range-p (text start end)
  "Return non-nil when TEXT has literal placeholder content from START to END."
  (mevedel-skills--property-range-p
   text start end mevedel-skills--literal-placeholder-property))

(defun mevedel-skills--protected-substitution-range-p (text start end)
  "Return non-nil when TEXT from START to END must not be substituted."
  (or (mevedel-skills--non-author-range-p text start end)
      (mevedel-skills--literal-placeholder-range-p text start end)))

(defun mevedel-skills--author-ranges-p (text &rest ranges)
  "Return non-nil when every range in RANGES is author-written in TEXT.
RANGES is a flat list of START/END pairs."
  (let ((author-p t))
    (while (and ranges author-p)
      (let ((start (pop ranges))
            (end (pop ranges)))
        (when (mevedel-skills--non-author-range-p text start end)
          (setq author-p nil))))
    author-p))

(defun mevedel-skills--replace-match-with-non-author (value)
  "Replace the current match with VALUE marked as non-author text."
  (let ((start (match-beginning 0))
        (end (match-end 0)))
    (setq mevedel-skills--substitution-made-p t)
    (delete-region start end)
    (goto-char start)
    (insert (mevedel-skills--mark-non-author-text value))))

(defconst mevedel-skills--literal-variable-placeholders
  '("${CLAUDE_SESSION_ID}"
    "${CLAUDE_SKILL_DIR}"
    "${CLAUDE_EFFORT}"
    "${MEVEDEL_SESSION_ID}"
    "${MEVEDEL_SKILL_DIR}"
    "${MEVEDEL_EFFORT}")
  "Literal skill variable placeholders supported by substitution.")

(defun mevedel-skills--placeholder-end-at-point (argument-names)
  "Return placeholder end at point, or nil when point is not at one.
ARGUMENT-NAMES is the list of named skill arguments."
  (or (cl-loop for placeholder in mevedel-skills--literal-variable-placeholders
               when (looking-at (regexp-quote placeholder))
               return (match-end 0))
      (when (looking-at "\\$ARGUMENTS\\(\\[[0-9]+\\]\\)?")
        (match-end 0))
      (when (looking-at "\\$\\([0-9]+\\)")
        (let ((end (match-end 0)))
          (unless (mevedel-skills--word-char-p (char-after end))
            end)))
      (cl-loop for name in argument-names
               for target = (concat "$" name)
               for end = (+ (point) (length target))
               when (and (looking-at (regexp-quote target))
                         (not (eq (char-after end) ?\[))
                         (not (mevedel-skills--word-char-p
                               (char-after end))))
               return end)))

(defun mevedel-skills--protect-escaped-placeholders (text argument-names)
  "Return TEXT with escaped placeholders made literal.
ARGUMENT-NAMES is the list of named skill arguments.
A backslash before a recognized placeholder, such as `\\$ARGUMENTS',
is removed and the placeholder is protected from substitution."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (while (search-forward "\\$" nil t)
      (let ((slash (match-beginning 0))
            (dollar (1- (point))))
        (goto-char dollar)
        (let ((end (mevedel-skills--placeholder-end-at-point argument-names)))
          (if end
              (progn
                (delete-region slash (1+ slash))
                (add-text-properties
                 slash (1- end)
                 (list mevedel-skills--literal-placeholder-property t))
                (goto-char (1- end)))
            (goto-char (1+ dollar))))))
    (buffer-string)))

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
             ((mevedel-skills--protected-substitution-range-p
               (buffer-string) (match-beginning 0) (match-end 0))
              nil)
             (t
              (mevedel-skills--replace-match-with-non-author value))))))
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
          (unless (or (mevedel-skills--word-char-p next)
                      (mevedel-skills--protected-substitution-range-p
                       (buffer-string) (match-beginning 0) (match-end 0)))
            (let* ((idx (string-to-number (match-string 1)))
                   (val (or (nth idx parsed-args) "")))
              (mevedel-skills--replace-match-with-non-author val)))))
      (buffer-string))))

(defun mevedel-skills--substitute-regexp
    (text regexp replacement-fn &optional author-only-p)
  "Replace REGEXP matches in TEXT with non-author replacement text.
REPLACEMENT-FN is called before the match is deleted, so it may use
`match-string' to inspect subgroups in the current buffer.
When AUTHOR-ONLY-P is non-nil, skip matches that overlap non-author text."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (while (re-search-forward regexp nil t)
      (unless (or (mevedel-skills--literal-placeholder-range-p
                   (buffer-string) (match-beginning 0) (match-end 0))
                  (and author-only-p
                       (mevedel-skills--non-author-range-p
                        (buffer-string) (match-beginning 0) (match-end 0))))
        (mevedel-skills--replace-match-with-non-author
         (funcall replacement-fn))))
    (buffer-string)))

(defun mevedel-skills--substitute-vars (text arguments session skill)
  "Return TEXT with skill placeholders expanded.

Algorithm follows ccs's `argumentSubstitution.ts' for unescaped
placeholders, with the mevedel extension that a backslash before a
recognized placeholder keeps it literal.  Substitution order
\=(zero-based throughout):

1. Named arguments from SKILL's `argument-names' slot, mapping
   ARGUMENT-NAMES[i] -> PARSED-ARGS[i].
2. `$ARGUMENTS[N]'.
3. `$N' shorthand.
4. `$ARGUMENTS' (the raw argument string).
5. `${CLAUDE_SESSION_ID}', `${CLAUDE_SKILL_DIR}', `${CLAUDE_EFFORT}',
   and their `${MEVEDEL_*}' aliases.  These are substituted after the
   placeholder-substituted check below.

If ARGUMENTS is non-empty AND none of steps 1-4 substituted
anything, append `\\nARGUMENTS: <raw>' so the body still receives
the user's input.

Named-argument matching uses strict word-boundary semantics so
`$foo' does not match `$foo[0]' or `$foobar'.  Numeric-only
argument names are filtered out at scan time
\\=(see `mevedel-skills--parse-argument-names') so they cannot
shadow `$0'/`$1' shorthand."
  (let* ((session-id (or (and session (mevedel-session-session-id session))
                         (and session (mevedel-session-name session))
                         ""))
         (skill-dir (or (and skill (mevedel-skill-source-dir skill)) ""))
         (effort (or (and skill
                          (mevedel-skill-effort skill)
                          (symbol-name (mevedel-skill-effort skill)))
                     ""))
         (argument-names (and skill (mevedel-skill-argument-names skill)))
         (raw-args arguments)
         (parsed-args (mevedel-skills--parse-arguments raw-args))
         (full (or raw-args ""))
         (mevedel-skills--substitution-made-p nil)
         (result (mevedel-skills--protect-escaped-placeholders
                  text argument-names)))
    ;; 1. Named arguments.
    (cl-loop for name in argument-names
             for i from 0
             for value = (or (nth i parsed-args) "")
             do (setq result
                      (mevedel-skills--substitute-named result name value)))
    ;; 2. $ARGUMENTS[N].
    (setq result
          (mevedel-skills--substitute-regexp
           result
           "\\$ARGUMENTS\\[\\([0-9]+\\)\\]"
           (lambda ()
             (or (nth (string-to-number (match-string 1))
                      parsed-args)
                 ""))))
    ;; 3. $N shorthand.
    (setq result (mevedel-skills--substitute-shorthand result parsed-args))
    ;; 4. $ARGUMENTS (full).
    (setq result
          (mevedel-skills--substitute-regexp
           result "\\$ARGUMENTS" (lambda () full)))
    ;; Decide append-fallback BEFORE the mevedel-specific ${...} subs
    ;; so they don't influence the "no placeholder substituted" check.
    (let ((args-substituted mevedel-skills--substitution-made-p))
      ;; 5. Claude-compatible and mevedel-native literal variables.
      (dolist (var `(("${CLAUDE_SESSION_ID}" . ,session-id)
                     ("${CLAUDE_SKILL_DIR}" . ,skill-dir)
                     ("${CLAUDE_EFFORT}" . ,effort)
                     ("${MEVEDEL_SESSION_ID}" . ,session-id)
                     ("${MEVEDEL_SKILL_DIR}" . ,skill-dir)
                     ("${MEVEDEL_EFFORT}" . ,effort)))
        (setq result
              (mevedel-skills--substitute-regexp
               result (regexp-quote (car var))
               (lambda () (cdr var)) t)))
      ;; 6. Append-fallback: only when args were supplied AND non-empty
      ;; AND nothing was substituted.
      (when (and (not args-substituted)
                 (stringp raw-args)
                 (not (string-empty-p raw-args)))
        (setq result
              (concat result "\n\nARGUMENTS: "
                      (mevedel-skills--mark-non-author-text raw-args)))))
    result))


;;
;;; Body injections

(declare-function mevedel-tools--check-bash-permission "mevedel-tool-exec"
                  (command &key trust-literal-p))
(declare-function mevedel-tools--check-eval-permission "mevedel-tool-exec"
                  (&key trust-literal-p))

(define-error 'mevedel-skills-shell-abort
  "Skill body shell expansion failed; skill must abort.")

(defun mevedel-skills--injection-outcome-error-p (result)
  "Return non-nil when pipeline RESULT means body injection failed."
  (and (stringp result)
       (or (string-prefix-p "Error:" result)
           (string-prefix-p "Command failed with exit code" result)
           (string-prefix-p "Failed to start process:" result))))

(defun mevedel-skills--run-shell-command-async (command marker callback)
  "Run COMMAND through the Bash tool pipeline, then call CALLBACK.

CALLBACK receives either \\=(:status ok :output STRING) or
\\=(:status error :reason SYMBOL :message STRING).  MARKER is the
original shell-injection marker used in diagnostics."
  (let ((tool (or (ignore-errors (mevedel-tool-get "Bash"))
                  (progn
                    (require 'mevedel-tool-exec)
                    (mevedel-tool-exec--register)
                    (ignore-errors (mevedel-tool-get "Bash"))))))
    (when tool
      (setq tool (copy-mevedel-tool tool))
      (setf (mevedel-tool-args tool)
            (append (mevedel-tool-args tool)
                    '((trust-literal-p boolean :optional
                                       "Internal trusted skill input.")))))
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
                ((mevedel-skills--injection-outcome-error-p result)
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

(defun mevedel-skills--run-elisp-expression-async (expression marker callback)
  "Run EXPRESSION through the Eval tool pipeline, then call CALLBACK.

CALLBACK receives either \\=(:status ok :output STRING) or
\\=(:status error :reason SYMBOL :message STRING).  MARKER is the
original elisp-injection marker used in diagnostics."
  (let ((tool (or (ignore-errors (mevedel-tool-get "Eval"))
                  (progn
                    (require 'mevedel-tool-exec)
                    (mevedel-tool-exec--register)
                    (ignore-errors (mevedel-tool-get "Eval"))))))
    (when tool
      (setq tool (copy-mevedel-tool tool))
      (setf (mevedel-tool-args tool)
            (append
             (mevedel-tool-args tool)
             '((trust-literal-p boolean :optional
                                "Internal trusted skill input.")
               (result-format string :optional
                              "Internal skill result format.")))))
    (cond
     ((null tool)
      (funcall callback
               `(:status error :reason elisp-failure
                         :message "Eval tool is not registered.")))
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
                                    :message ,(format "Elisp expansion %s denied: %s"
                                                      marker result))))
                ((mevedel-skills--injection-outcome-error-p result)
                 (funcall callback
                          `(:status error :reason elisp-failure
                                    :message ,(format "Elisp expansion %s failed: %s"
                                                      marker result))))
                (t
                 (funcall callback
                          `(:status ok :output ,(string-trim-right
                                                 (or result "")))))))
             (list :expression expression
                   :trust-literal-p t
                   :result-format "injection")))
        (error
         (funcall callback
                  `(:status error :reason elisp-failure
                            :message ,(format "Elisp expansion %s errored: %s"
                                              marker
                                              (error-message-string err))))))))))

(defun mevedel-skills--ranges-overlap-p (ranges start end)
  "Return non-nil when any range in RANGES overlaps START to END."
  (let (found)
    (while (and ranges (not found))
      (let ((range (pop ranges)))
        (when (and (< start (cdr range))
                   (< (car range) end))
          (setq found t))))
    found))

(defun mevedel-skills--markdown-injection-fence-opener-p (line marker)
  "Return non-nil when LINE and MARKER open a body-injection fence."
  (and (string= marker "```")
       (or (string= line "```!")
           (string-match-p "\\````!el[ \t]*\\'" line))))

(defun mevedel-skills--markdown-authored-fence-close-end (text close-re start)
  "Return end of the next CLOSE-RE match in TEXT after START."
  (let ((search start)
        close-end)
    (while (and (not close-end)
                (string-match close-re text search))
      (if (mevedel-skills--author-ranges-p
           text (match-beginning 0) (match-end 0))
          (setq close-end (match-end 0))
        (setq search (match-end 0))))
    close-end))

(defun mevedel-skills--markdown-code-fence-ranges (text)
  "Return ordinary Markdown code-fence ranges in TEXT.
Body-injection fences are deliberately excluded so they remain
active skill syntax."
  (let ((ranges nil)
        (pos 0)
        (len (length text)))
    (while (and (< pos len)
                (string-match "\\(^\\|\n\\)\\(```+\\)[^\n]*\\(\n\\|\\'\\)"
                              text pos))
      (let* ((line-start (+ (match-beginning 0)
                            (length (match-string 1 text))))
             (marker (match-string 2 text))
             (line-end (if (string= (match-string 3 text) "\n")
                           (1- (match-end 0))
                         (match-end 0)))
             (line (substring text line-start line-end))
             (body-start (match-end 0))
             (close-re (concat "\\(^\\|\n\\)"
                               (regexp-quote marker)
                               "\\(\n\\|\\'\\)"))
             (close-end (mevedel-skills--markdown-authored-fence-close-end
                         text close-re body-start)))
        (if (mevedel-skills--markdown-injection-fence-opener-p line marker)
            (setq pos (or close-end len))
          (if close-end
              (progn
                (push (cons line-start close-end) ranges)
                (setq pos close-end))
            (push (cons line-start len) ranges)
            (setq pos len)))))
    (nreverse ranges)))

(defun mevedel-skills--injection-inline-marker-start (text position)
  "Return the injection marker start before TEXT's backtick at POSITION."
  (cond
   ((and (> position 0)
         (= (aref text (1- position)) ?!))
    (1- position))
   ((and (>= position 3)
         (string= (substring text (- position 3) position) "!el"))
    (- position 3))))

(defun mevedel-skills--injection-inline-span-end (text position line-end)
  "Return end of authored inline injection at POSITION, or nil.
POSITION must point at the opening backtick.  LINE-END is the
exclusive end of the current line."
  (when-let* ((marker-start
               (mevedel-skills--injection-inline-marker-start text position))
              ((mevedel-skills--author-ranges-p
                text marker-start (1+ position))))
    (let ((search (1+ position))
          span-end)
      (while (and (not span-end)
                  (string-match "`" text search)
                  (<= (match-end 0) line-end))
        (let ((close-start (match-beginning 0))
              (close-end (match-end 0)))
          (if (mevedel-skills--author-ranges-p text close-start close-end)
              (setq span-end close-end)
            (setq search close-end))))
      span-end)))

(defun mevedel-skills--markdown-inline-code-ranges (text fence-ranges)
  "Return Markdown inline code-span ranges in TEXT outside FENCE-RANGES."
  (let ((ranges nil)
        (line-start 0)
        (len (length text)))
    (while (< line-start len)
      (let* ((line-end (or (string-match "\n" text line-start) len))
             (pos line-start))
        (while (and (< pos line-end)
                    (string-match "`+" text pos))
          (let* ((run-start (match-beginning 0))
                 (run-end (match-end 0))
                 (run (match-string 0 text)))
            (cond
             ((or (>= run-start line-end)
                  (mevedel-skills--ranges-overlap-p
                   fence-ranges run-start run-end))
              (setq pos run-end))
             ((if-let* ((injection-end
                         (mevedel-skills--injection-inline-span-end
                          text run-start line-end)))
                  (setq pos injection-end)))
             ((and (string-match (regexp-quote run) text run-end)
                   (<= (match-end 0) line-end))
              (push (cons run-start (match-end 0)) ranges)
              (setq pos (match-end 0)))
             (t
              (setq pos run-end)))))
        (setq line-start (if (< line-end len) (1+ line-end) len))))
    (nreverse ranges)))

(defun mevedel-skills--markdown-code-ranges (text)
  "Return Markdown code ranges in TEXT that should not run as injections."
  (let* ((fence-ranges (mevedel-skills--markdown-code-fence-ranges text))
         (inline-ranges (mevedel-skills--markdown-inline-code-ranges
                         text fence-ranges)))
    (sort (append fence-ranges inline-ranges)
          (lambda (a b) (< (car a) (car b))))))

(defun mevedel-skills--injection-match (text)
  "Return the next body-injection match in TEXT.

The return value is a plist with :start, :end, :command, and
:marker, or nil when TEXT contains no injection marker."
  (let ((matches nil)
        (markdown-code-ranges (mevedel-skills--markdown-code-ranges text)))
    (cl-labels
        ((scan-inline
          (opener kind payload-key)
          (let ((pos 0)
                (opener-re (regexp-quote opener))
                (len (length text)))
            (while (string-match opener-re text pos)
              (let* ((start (match-beginning 0))
                     (body-start (match-end 0))
                     (search body-start)
                     (done nil))
                (when (and (mevedel-skills--author-ranges-p
                            text start body-start)
                           (not (mevedel-skills--ranges-overlap-p
                                 markdown-code-ranges start body-start)))
                  (while (and (not done)
                              (string-match "`" text search))
                    (let ((close-start (match-beginning 0))
                          (close-end (match-end 0)))
                      (cond
                       ((string-match-p
                         "\n" (substring text body-start close-start))
                        (setq done t))
                       ((and (mevedel-skills--author-ranges-p
                              text close-start close-end)
                             (not (mevedel-skills--ranges-overlap-p
                                   markdown-code-ranges close-start close-end)))
                        (let ((payload (substring text body-start
                                                  close-start)))
                          (push (list :kind kind
                                      :start start
                                      :end close-end
                                      payload-key payload
                                      :marker (format "%s%s`" opener payload))
                                matches))
                        (setq done t))
                       (t
                        (setq search close-end))))))
                (setq pos (min len (max (1+ start) body-start)))))))
         (scan-fenced
          (opener-regexp kind payload-key marker)
          (let ((pos 0)
                (opener-re (concat "\\(^\\|\n\\)" opener-regexp)))
            (while (string-match opener-re text pos)
              (let* ((start (match-beginning 0))
                     (prefix-start (match-beginning 1))
                     (prefix-end (match-end 1))
                     (prefix (match-string 1 text))
                     (marker-start (+ start (length prefix)))
                     (body-start (match-end 0))
                     (search body-start)
                     (done nil))
                (when (and (mevedel-skills--author-ranges-p
                            text prefix-start prefix-end marker-start body-start)
                           (not (mevedel-skills--ranges-overlap-p
                                 markdown-code-ranges marker-start body-start)))
                  (while (and (not done)
                              (string-match "\n```\\(\n\\|\\'\\)"
                                            text search))
                    (let ((close-start (match-beginning 0))
                          (close-end (match-beginning 1))
                          (suffix-start (match-beginning 1))
                          (suffix-end (match-end 1)))
                      (if (and (mevedel-skills--author-ranges-p
                                text close-start close-end suffix-start suffix-end)
                               (not (mevedel-skills--ranges-overlap-p
                                     markdown-code-ranges close-start close-end)))
                          (let ((payload (substring text body-start
                                                    close-start)))
                            (push (list :kind kind
                                        :start start
                                        :end (match-end 0)
                                        payload-key payload
                                        :marker marker
                                        :prefix prefix
                                        :suffix (match-string 1 text))
                                  matches)
                            (setq done t))
                        (setq search (match-end 0))))))
                (setq pos (max (1+ start) body-start)))))))
      (scan-fenced (regexp-quote "```!\n")
                   'shell :command "(fenced shell block)")
      (scan-fenced "```!el[ \t]*\n"
                   'elisp :expression "(fenced elisp block)")
      (scan-inline "!`" 'shell :command)
      (scan-inline "!el`" 'elisp :expression)
      (car (sort matches
                 (lambda (a b)
                   (< (plist-get a :start)
                      (plist-get b :start))))))))

(defun mevedel-skills--run-body-injections-async (text callback)
  "Replace skill body injection markers in TEXT, then call CALLBACK.

CALLBACK receives either \\=(:status ok :body STRING) or
\\=(:status error :reason SYMBOL :message STRING).

Supported markers:
- !`COMMAND`          inline: run COMMAND, substitute stdout
- ```!\\nSCRIPT\\n``` fenced block: run SCRIPT as a shell script
- !el`EXPRESSION`     inline: evaluate EXPRESSION, substitute result
- ```!el\\nEXPR\\n``` fenced block: evaluate EXPR, substitute result

Each command/expression goes through its normal tool pipeline with
`:trust-literal-p t', so permission checking, execution, and
oversized-result persistence stay aligned with normal tool
execution."
  (if-let* ((match (mevedel-skills--injection-match text)))
      (let ((start (plist-get match :start))
            (end (plist-get match :end))
            (kind (plist-get match :kind))
            (marker (plist-get match :marker))
            (prefix (or (plist-get match :prefix) ""))
            (suffix (or (plist-get match :suffix) ""))
            (origin-buffer (current-buffer)))
        (funcall
         (pcase kind
           ('shell #'mevedel-skills--run-shell-command-async)
           ('elisp #'mevedel-skills--run-elisp-expression-async))
         (or (plist-get match :command)
             (plist-get match :expression))
         marker
         (lambda (outcome)
           (if (not (buffer-live-p origin-buffer))
               (funcall callback
                        `(:status error :reason aborted
                                  :message "Skill buffer was killed during body injection expansion."))
             (with-current-buffer origin-buffer
               (pcase (plist-get outcome :status)
                 ('ok
                  (mevedel-skills--run-body-injections-async
                   (concat (substring text 0 start)
                           prefix
                           (mevedel-skills--mark-non-author-text
                            (plist-get outcome :output))
                           suffix
                           (substring text end))
                   callback))
                 (_
                  (funcall callback outcome))))))))
    (funcall callback `(:status ok :body ,(substring-no-properties text)))))

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
  "Emit SKILL error event, then deliver REASON and MESSAGE to CALLBACK.
DISPLAY-CALLBACK receives the lifecycle event when non-nil."
  (let ((skill-name (and skill (mevedel-skill-name skill))))
    (mevedel-skills--display-event
     display-callback
     `(:event error :skill ,skill-name
              :reason ,reason :message ,message))
    (funcall callback
             `(:status error :reason ,reason :message ,message))))

(defun mevedel-skills--run-expansion-hook
    (skill arguments prompt trigger session callback)
  "Run `UserPromptExpansion' for a user SKILL expansion.
ARGUMENTS is the raw user argument string.  SESSION supplies workspace
context.  CALLBACK receives (PROMPT DECISION).  Non-user TRIGGER values
skip the hook and call CALLBACK with PROMPT and nil."
  (if (not (eq trigger 'user-skill))
      (funcall callback prompt nil)
    (require 'mevedel-hooks)
    (let* ((workspace (and session (mevedel-session-workspace session)))
           (request (and (boundp 'mevedel--current-request)
                         mevedel--current-request)))
      (mevedel-hooks-run-event
       'UserPromptExpansion
       (mevedel-hooks-event-plist
        'UserPromptExpansion session workspace
        :skill-name (mevedel-skill-name skill)
        :arguments arguments
        :prompt prompt)
       (lambda (decision)
         (let* ((updated (plist-get decision :updated-input))
                (context (mevedel-hooks-additional-context-string
                          decision 'UserPromptExpansion))
                (prompt (if (stringp updated) updated prompt)))
           (when (and context (not (string-empty-p context)))
             (setq prompt (concat prompt "\n\n" context)))
           (funcall callback prompt decision)))
       session workspace request nil))))

(defun mevedel-skills--invoke-done (skill outcome callback display-callback)
  "Emit SKILL done event, then deliver OUTCOME to CALLBACK.
DISPLAY-CALLBACK receives the lifecycle event when non-nil."
  (let ((skill-name (and skill (mevedel-skill-name skill))))
    (mevedel-skills--display-event
     display-callback
     `(:event done :skill ,skill-name))
    (funcall callback outcome)))

(defun mevedel-skills--safe-hook-decision (event decision)
  "Return plist-shaped hook DECISION for EVENT, or nil.

Hook runners normally sanitize their callback value, but callers can
stub them in tests and older compiled code may still deliver malformed
values.  Skill dispatch reads prompt decisions synchronously, so keep
that boundary defensive."
  (if (and (listp decision)
           (or (null decision)
               (keywordp (car-safe decision))))
      decision
    (display-warning
     'mevedel
     (format "Ignoring malformed %s hook decision: %S" event decision)
     :warning)
    nil))

(defun mevedel-skills--prompt-rewrite-audit-record (original decision)
  "Return a `UserPromptExpansion' rewrite audit record, or nil."
  (when-let* ((updated (plist-get decision :updated-input))
              ((stringp updated)))
    (mevedel--hook-prompt-rewrite-audit-record
     'UserPromptExpansion original updated
     (mevedel-hooks-decision-reason decision))))

(defun mevedel-skills-inline-display-text (name arguments)
  "Return the compact view text for inline skill NAME and ARGUMENTS."
  (if (and arguments (not (string-empty-p arguments)))
      (format "$%s %s" name arguments)
    (format "$%s" name)))

(defun mevedel-skills-format-inline-render-data (skill arguments)
  "Return hidden render-data for an expanded inline user SKILL.

ARGUMENTS is the raw user argument string.

The block is ignored by gptel and consumed by `mevedel-view' so the
data buffer keeps the expanded prompt while the view can show the
original `$skill' invocation compactly."
  (let* ((name (mevedel-skill-name skill))
         (data (list :kind 'inline-skill
                     :name name
                     :arguments arguments
                     :display-text
                     (mevedel-skills-inline-display-text
                      name arguments)))
         (block (propertize
                 (progn
                   (require 'mevedel-pipeline)
                   (mevedel-pipeline--format-render-data-block data))
                 'gptel 'ignore)))
    block))

(defun mevedel-skills--insert-inline-user-skill-render-data
    (skill arguments)
  "Insert hidden render-data for SKILL with raw user ARGUMENTS."
  (insert (mevedel-skills-format-inline-render-data skill arguments)))

(defun mevedel-skills-format-inline-attachment-render-data (attachments)
  "Return hidden render-data for inline skill ATTACHMENTS."
  (let ((data (list :kind 'inline-skill-attachments
                    :skills (mapcar
                             (lambda (attachment)
                               (list :name (plist-get attachment :name)))
                             attachments))))
    (propertize
     (progn
       (require 'mevedel-pipeline)
       (mevedel-pipeline--format-render-data-block data))
     'gptel 'ignore)))

(defun mevedel-skills--inline-attachment-reminder (attachment)
  "Return system-reminder body for prepared inline skill ATTACHMENT."
  (format "Skill `$%s` was attached by an inline user mention. Follow these instructions for this turn:\n\n%s"
          (plist-get attachment :name)
          (or (plist-get attachment :body) "")))

(defun mevedel-skills--replace-inline-attachment-mentions
    (attachments start end)
  "Replace prompt mentions for prepared ATTACHMENTS between START and END.
Return attachments that were actually referenced, preserving first use."
  (let* ((text (buffer-substring-no-properties start end))
         (first-nonspace (or (string-match-p "\\S-" text) -1))
         (code-ranges (mevedel-skills--markdown-code-ranges text))
         (by-name (make-hash-table :test #'equal))
         (seen (make-hash-table :test #'equal))
         replacements
         used)
    (dolist (attachment attachments)
      (puthash (plist-get attachment :name) attachment by-name))
    (let ((pos 0))
      (while (string-match "\\$\\([A-Za-z0-9_.:-]+\\)" text pos)
        (let* ((rel-start (match-beginning 0))
               (raw-name (match-string 1 text))
               (resolved
                (mevedel-skills--resolve-inline-skill-name
                 raw-name
                 (lambda (candidate)
                   (gethash candidate by-name))))
               (name (car-safe resolved))
               (rel-end (and name (+ rel-start 1 (length name))))
               (attachment (cdr-safe resolved)))
          (setq pos (match-end 0))
          (when (and attachment
                     (mevedel-skills--inline-skill-live-p
                      text rel-start rel-end first-nonspace code-ranges))
            (push (list (+ start rel-start)
                        (+ start rel-end)
                        (format "[skill:%s -- attached]" name))
                  replacements)
            (unless (gethash name seen)
              (puthash name t seen)
              (push attachment used))))))
    (require 'mevedel-mentions)
    (dolist (replacement (sort replacements
                               (lambda (a b) (> (car a) (car b)))))
      (mevedel-mentions-replace-with-placeholder
       (nth 0 replacement) (nth 1 replacement) (nth 2 replacement)))
    (nreverse used)))

(defun mevedel-skills--transform-expand-inline-attachments (fsm)
  "Expand prepared inline `$skill' attachments into prompt context."
  (when-let* ((chat-buffer (and fsm (plist-get (gptel-fsm-info fsm) :buffer)))
              ((buffer-live-p chat-buffer))
              (attachments
               (buffer-local-value
                'mevedel-skills--pending-inline-attachments chat-buffer)))
    (with-current-buffer chat-buffer
      (setq-local mevedel-skills--pending-inline-attachments nil))
    (require 'mevedel-transcript)
    (let* ((start (copy-marker (mevedel-transcript-prompt-transform-start) nil))
           (used (mevedel-skills--replace-inline-attachment-mentions
                  attachments (marker-position start) (point-max))))
      (when used
        (goto-char start)
        (dolist (attachment used)
          (let ((insert-start (point)))
            (insert "<system-reminder>\n"
                    (mevedel-skills--inline-attachment-reminder attachment)
                    "\n</system-reminder>\n\n")
            (remove-text-properties
             insert-start (point)
             '(gptel nil response nil invisible nil front-sticky nil))))))))

(cl-defun mevedel-skills--activate-context
    (trigger &key permission-rules model effort hook-rules invoked-skill)
  "Apply skill-scoped overrides to the active context.

TRIGGER selects the install path:

- `user-skill': append onto the buffer-local pending stash
  (`mevedel-skills--pending-request-context'); drained at request
  begin by the WAIT-state begin handler in `mevedel-presets.el'.
  Used because user skill dispatch fires before the `mevedel-request'
  has been created.
- `model-skill' / `internal': mutate the active sub-agent
  invocation (innermost) or request directly.

PERMISSION-RULES is a list of parsed mevedel rules to append.
MODEL is a selector plist or nil.  EFFORT is a symbol or nil (currently
inert).  HOOK-RULES is a list of normalized hook rules.  INVOKED-SKILL
is a `mevedel-skill-invocation-record' to record on the session for
compaction/replay.

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
   ((eq trigger 'user-skill)
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
      (when hook-rules
        (setq existing
              (plist-put existing :hook-rules
                         (append (plist-get existing :hook-rules)
                                 hook-rules))))
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
          (mevedel-skills--activate-request-context
           req permission-rules nil))))
      ;; Model and effort overwrite (last-writer-wins).
      (when model
        (cond
         (inv (setf (mevedel-agent-invocation-skill-model-override inv) model))
         (req (setf (mevedel-request-skill-model-override req) model))))
      (when effort
        (cond
         (inv (setf (mevedel-agent-invocation-skill-effort-override inv) effort))
         (req (setf (mevedel-request-skill-effort-override req) effort))))
      (when hook-rules
        (cond
         (inv
          (setf (mevedel-agent-invocation-hook-rules inv)
                (append (mevedel-agent-invocation-hook-rules inv)
                        hook-rules)))
	 (req
	  (mevedel-skills--activate-request-context
	   req nil hook-rules))))
      ;; Record on the session.
      (when invoked-skill
        (when-let* ((session (and (boundp 'mevedel--session) mevedel--session)))
          (setf (mevedel-session-invoked-skills session)
                (append (mevedel-session-invoked-skills session)
                        (list invoked-skill)))))))))

(cl-defun mevedel-skills--invoke-inline
    (skill arguments callback &key trigger display-callback)
  "Inline-context invocation.

SKILL is the skill to run.  ARGUMENTS is the raw user/model argument
string.  CALLBACK receives the final outcome.  TRIGGER records the
invocation source.  DISPLAY-CALLBACK receives lifecycle events.

Preparation order matches the body-injection section:
  1. Load body
  2. Substitute variables
  3. Activate skill-scoped permission rules (so allowed-tools is
     in effect during body injection expansion)
  4. Expand shell/elisp injections
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
             (hooks (mevedel-skill-hooks skill))
             (temporary-request-p nil))
        ;; Step 3: activate permission rules before body injection expansion.
        (mevedel-skills--activate-context
         trigger :permission-rules rules :hook-rules hooks)
        ;; Slash expansion happens before the real request exists.
        ;; Install a short-lived request so tool pipeline permission
        ;; checks can see this skill's allowed-tools while body
        ;; injections are being prepared.
        (when (and (eq trigger 'user-skill)
                   (not (bound-and-true-p mevedel--current-request)))
          (setq temporary-request-p t)
          (setq-local mevedel--current-request
                      (mevedel-request--create
                       :session session
                       :file-snapshots (make-hash-table :test #'equal)
                       :skill-permission-rules rules
                       :hook-rules hooks)))
	(mevedel-skills--run-body-injections-async
	 substituted
	 (lambda (injection-outcome)
	   (pcase (plist-get injection-outcome :status)
	     ('ok
	      (mevedel-skills--run-expansion-hook
	       skill arguments (plist-get injection-outcome :body)
	       trigger session
	       (lambda (expanded decision)
                 (setq decision
                       (mevedel-skills--safe-hook-decision
                        'UserPromptExpansion decision))
	         (when temporary-request-p
	           (setq-local mevedel--current-request nil))
	         (if (and (plist-member decision :continue)
	                  (not (plist-get decision :continue)))
	             (progn
	               (when (eq trigger 'user-skill)
	                 (setq-local mevedel-skills--pending-request-context nil))
	               (mevedel-skills--invoke-error
	                skill 'hook-blocked
	                (or (plist-get decision :stop-reason)
	                    "UserPromptExpansion hook stopped skill")
	                callback display-callback))
                   (let* ((hook-audits
                           (delq nil
                                 (list
                                  (mevedel-skills--prompt-rewrite-audit-record
                                   (plist-get injection-outcome :body)
                                   decision))))
                          (record
                           (mevedel-skill-invocation-record--create
                            :name skill-name
                            :args arguments
                            :trigger trigger
                            :turn (and session
                                       (mevedel-session-turn-count session))
                            :source-path (mevedel-skill-source-file skill)
                            :prepared-body expanded))
                          (ctx (list :permission-rules rules
                                     :model model
                                     :effort effort
                                     :hook-rules hooks
                                     :invoked-skills (list record))))
                     ;; Rules already activated above; do not append them twice.
                     (mevedel-skills--activate-context
                      trigger :model model :effort effort
                      :invoked-skill record)
                     (mevedel-skills--invoke-done
                      skill
                      `(:status ok :kind inline
                                :body ,expanded
                                :arguments ,arguments
                                :hook-audits ,hook-audits
                                :request-context ,ctx)
                      callback display-callback))))))
	     (_
	      (when (eq trigger 'user-skill)
	        (setq-local mevedel-skills--pending-request-context nil))
	      (when temporary-request-p
	        (setq-local mevedel--current-request nil))
	      (mevedel-skills--invoke-error
	       skill
	       (plist-get injection-outcome :reason)
	       (plist-get injection-outcome :message)
	       callback display-callback))))))))))

(defvar mevedel-agent-exec--agents)
(defvar gptel-system-prompt)

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
         (parent-system (and (boundp 'gptel-system-prompt)
                             gptel-system-prompt))
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
snapshotted from the calling buffer's `gptel-system-prompt';
tools propagate through the spawn path's request-locals capture."
  (let ((agent-name (mevedel-skill-agent skill)))
    (cond
     ((and (stringp agent-name) (not (string-empty-p agent-name)))
      (mevedel-agent-get agent-name))
     (t
      (mevedel-skills--build-parent-inherited-agent skill)))))

(cl-defun mevedel-skills--invoke-fork-direct
    (skill arguments callback &key trigger display-callback
           additional-context description on-invocation)
  "Direct fork dispatch via `mevedel-tools--task'.  Async outcome.

SKILL is the skill to run.  ARGUMENTS is the raw user/model argument
string.  CALLBACK receives the final outcome.  TRIGGER records the
invocation source.  DISPLAY-CALLBACK receives lifecycle events.
ADDITIONAL-CONTEXT and DESCRIPTION are passed through to the spawned
agent task.  ON-INVOCATION is called with the agent invocation object.

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
    (if (null agent)
        ;; --build-fork-agent only returns nil for an unknown named
        ;; agent; the parent-inherited path always synthesizes a
        ;; struct.
        (mevedel-skills--invoke-error
         skill 'unknown-agent
         (format "Skill '%s' references unknown agent '%s'"
                 skill-name (mevedel-skill-agent skill))
         callback display-callback)
      (let* ((body (or (mevedel-skill-load-body skill) ""))
             (substituted (mevedel-skills--substitute-vars
                           body arguments session skill))
             (description (or description
                              (mevedel-skill-description skill)
                              skill-name))
             (rules (mevedel-skill-allowed-tool-rules skill))
             (model (mevedel-skills--model-selector skill))
             (effort (mevedel-skill-effort skill))
             (hooks (mevedel-skill-hooks skill))
             (temporary-request-p nil))
        (cond
         ((not (eq trigger 'user-skill))
          (mevedel-skills--activate-context
           trigger :permission-rules rules :hook-rules hooks))
         ((bound-and-true-p mevedel--current-request)
          (mevedel-skills--activate-request-context
           mevedel--current-request rules hooks)))
        (when (and (eq trigger 'user-skill)
                   (not (bound-and-true-p mevedel--current-request)))
          (setq temporary-request-p t)
          (setq-local mevedel--current-request
                      (mevedel-request--create
                       :session session
                       :file-snapshots (make-hash-table :test #'equal)
                       :skill-permission-rules rules
                       :hook-rules hooks)))
        (mevedel-skills--run-body-injections-async
         substituted
         (lambda (injection-outcome)
           (pcase (plist-get injection-outcome :status)
             ('ok
              (mevedel-skills--run-expansion-hook
               skill arguments (plist-get injection-outcome :body)
               trigger session
               (lambda (prepared decision)
                 (setq decision
                       (mevedel-skills--safe-hook-decision
                        'UserPromptExpansion decision))
                 (when temporary-request-p
                   (setq-local mevedel--current-request nil))
                 (if (and (plist-member decision :continue)
                          (not (plist-get decision :continue)))
                     (mevedel-skills--invoke-error
                      skill 'hook-blocked
                      (or (plist-get decision :stop-reason)
                          "UserPromptExpansion hook stopped skill")
                      callback display-callback)
                   (let* ((hook-audits
                           (delq nil
                                 (list
                                  (mevedel-skills--prompt-rewrite-audit-record
                                   (plist-get injection-outcome :body)
                                   decision))))
                          (prepared
                           (if (and (stringp additional-context)
                                    (not (string-empty-p additional-context)))
                               (concat prepared "\n\n" additional-context)
                             prepared))
                          (record
                           (mevedel-skill-invocation-record--create
                            :name skill-name
                            :args arguments
                            :trigger trigger
                            :turn (and session
                                       (mevedel-session-turn-count session))
                            :source-path (mevedel-skill-source-file skill)
                            :prepared-body prepared)))
                     (when session
                       (setf (mevedel-session-invoked-skills session)
                             (append (mevedel-session-invoked-skills session)
                                     (list record))))
                     (unless (fboundp 'mevedel-tools--task)
                       (require 'mevedel-tool-ui))
                     (condition-case err
                         (mevedel-tools--task
                          (lambda (response)
                            ;; `mevedel-tools--task' may deliver either a bare
                            ;; string or a plist with transcript render data.
                            (let* ((wrapped-p
                                    (and (listp response)
                                         (plist-member response :result)))
                                   (result-str (if wrapped-p
                                                   (plist-get response :result)
                                                 response))
                                   (render-data
                                    (and wrapped-p
                                         (plist-get response :render-data)))
                                   (transcript-agent-id
                                    (and wrapped-p
                                         (plist-get render-data :agent-id))))
                              (mevedel-skills--invoke-done
                               skill
                               `(:status ok :kind fork
                                         :result ,result-str
                                         :agent-id
                                         ,(or transcript-agent-id
                                              (mevedel-agent-name agent))
                                         :hook-audits ,hook-audits
                                         :render-data ,render-data)
                               callback display-callback)))
                          agent description prepared
                          :skill-permission-rules rules
                          :skill-model-override model
                          :skill-effort-override effort
                          :skill-hook-rules hooks
                          :on-invocation on-invocation)
                       (error
                        (mevedel-skills--invoke-error
                         skill 'agent-dispatch-failed
                         (error-message-string err)
                         callback display-callback))))))))
             (_
              (when temporary-request-p
                (setq-local mevedel--current-request nil))
              (mevedel-skills--invoke-error
               skill
               (plist-get injection-outcome :reason)
               (plist-get injection-outcome :message)
               callback display-callback)))))))))

(cl-defun mevedel-skills-invoke
    (skill arguments callback &key trigger display-callback
           additional-context description on-invocation skip-gates)
  "Invoke SKILL with ARGUMENTS through the unified skill API.

CALLBACK is invoked with a normalized invocation outcome plist:

  (:status ok    :kind inline :body BODY :request-context CTX)
  (:status ok    :kind fork   :result RESULT :agent-id ID
                  :render-data DATA)
  (:status error :reason REASON :message MESSAGE)

TRIGGER is `user-skill', `model-skill', or `internal' and
determines the blocking model implicitly: `user-skill' blocks
chat input; `model-skill' blocks the parent tool call.

DISPLAY-CALLBACK is an optional lifecycle event sink that
receives `agent-progress' (fork only), `done', and `error'
events.

ADDITIONAL-CONTEXT is appended to fork-skill agent prompts after body
injections have prepared the prompt.

DESCRIPTION overrides the task description for fork skills.
ON-INVOCATION is forwarded to `mevedel-tools--task' for fork skills.
SKIP-GATES bypasses user-disabled/user-invocable/model-invocable gates
for first-class local commands that own their dispatch semantics.

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
     ;; User-disabled skill gating.
     ((and (not skip-gates)
           (not (mevedel-skills--skill-enabled-p skill)))
      (mevedel-skills--invoke-error
       skill 'disabled
       (if (eq trigger 'user-skill)
           (format "Skill $%s is disabled. Enable it with /skills enable %s or escape it as \\$%s."
                   skill-name skill-name skill-name)
         (format "Skill '%s' is disabled" skill-name))
       callback display-callback))
     ;; User-slash gating.
     ((and (not skip-gates)
           (eq trigger 'user-skill)
           (not (mevedel-skill-user-invocable-p skill)))
      (mevedel-skills--invoke-error
       skill 'disabled
       (format "Skill '%s' is not user-invocable" skill-name)
       callback display-callback))
     ;; Model-side gating.
     ((and (not skip-gates)
           (eq trigger 'model-skill)
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
           (mevedel-skills--invoke-fork-direct
            skill arguments callback
            :trigger trigger :display-callback display-callback
            :additional-context additional-context
            :description description
            :on-invocation on-invocation))
          (other
           (mevedel-skills--invoke-error
            skill 'unknown-skill
            (format "Skill '%s' has unsupported context: %S"
                    skill-name other)
            callback display-callback))))))))


;;
;;; Skill tool handler

(defun mevedel-skills--render-skill-tool (name args result _render-data)
  "Return rendering plist for NAME, ARGS, and RESULT from the Skill tool."
  (when (stringp result)
    (let* ((skill-name (or (plist-get args :name) "?"))
           (lines (length (split-string result "\n" t))))
      (list :header (format "%s: %s (%d %s)"
                            (or name "Skill")
                            skill-name
                            lines
                            (if (= lines 1) "line" "lines"))
            :body result
            :body-mode 'markdown-mode
            :status (and (string-prefix-p "Error:" result) 'error)
            :initially-collapsed-p t))))

(defun mevedel-skills--invoke-handler (callback args)
  "Pipeline handler for the `Skill' tool.

CALLBACK is the async tool callback.  ARGS is a plist with :name
and optional :arguments.

Routes through `mevedel-skills-invoke' with `model-skill' trigger
and projects the outcome plist to a tool-result string: success
returns the body; error returns a `Error: ' prefixed message."
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

(defconst mevedel-skills--list-tool-limit 25
  "Maximum entries returned by the ListSkills tool without narrowing.")

(defun mevedel-skills--model-visible-p (skill &optional active-only)
  "Return non-nil when SKILL may be shown to the model.
When ACTIVE-ONLY is non-nil, dormant path-scoped skills are excluded."
  (and (mevedel-skill-model-invocable-p skill)
       (mevedel-skills--skill-enabled-p skill)
       (or (not active-only)
           (mevedel-skill-active-p skill))))

(defun mevedel-skills--skill-matches-query-p (skill query)
  "Return non-nil when SKILL matches QUERY."
  (or (not (and (stringp query) (not (string-empty-p query))))
      (let ((case-fold-search t)
            (needle (regexp-quote query)))
        (cl-some
         (lambda (value)
           (and (stringp value) (string-match-p needle value)))
         (list (mevedel-skill-name skill)
               (mevedel-skill-display-name skill)
               (mevedel-skill-description skill))))))

(defun mevedel-skills--format-list-tool-result
    (skills omitted &optional mark-dormant)
  "Format SKILLS for the ListSkills tool, noting OMITTED entries."
  (if (null skills)
      "No model-invocable skills match."
    (let ((body
           (mapconcat (lambda (skill)
                        (mevedel-skills--listing-describe
                         skill
                         (and mark-dormant
                              (mevedel-skills--model-visible-p skill)
                              (mevedel-skill-path-patterns skill)
                              (not (mevedel-skill-active-p skill)))))
                      skills "\n")))
      (if (> omitted 0)
          (concat body
                  (format "\n\n%d more skill(s) omitted; use query to narrow."
                          omitted))
        body))))

(defun mevedel-skills--list-handler (callback args)
  "Pipeline handler for the `ListSkills' tool.
CALLBACK is the async tool callback.  ARGS is a plist with optional :query."
  (let* ((query (plist-get args :query))
         (session (and (boundp 'mevedel--session) mevedel--session)))
    (cond
     ((not session)
      (funcall callback "Error: No active mevedel session."))
     ((and query (not (stringp query)))
      (funcall callback "Error: query must be a string."))
     (t
      (when (buffer-live-p (current-buffer))
        (mevedel-skills--ensure-fresh (current-buffer) session))
      (let* ((narrowed (and (stringp query)
                            (not (string-empty-p (string-trim query)))))
             (pool (if narrowed
                       (cl-remove-if-not
                        #'mevedel-skills--model-visible-p
                        (mevedel-session-skills session))
                     (mevedel-skills--listing-candidates session)))
             (matches
              (cl-remove-if-not
               (lambda (skill)
                 (mevedel-skills--skill-matches-query-p skill query))
               pool))
             (shown (cl-subseq matches 0
                                (min (length matches)
                                     mevedel-skills--list-tool-limit)))
             (omitted (max 0 (- (length matches) (length shown)))))
        (funcall callback
                 (mevedel-skills--format-list-tool-result
                  shown omitted narrowed)))))))


;;
;;; Tool registration

;;;###autoload
(defun mevedel-skills--register ()
  "Register skill tools with the mevedel tool registry.

`:get-name' lets permission rules qualify by skill name, e.g.
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
    :groups (util)
    :renderer #'mevedel-skills--render-skill-tool)
  (mevedel-define-tool
    :name "ListSkills"
    :description "List active model-invocable skills, optionally filtered by query."
    :handler #'mevedel-skills--list-handler
    :args ((query string :optional
                  "Optional case-insensitive search over skill name and description."))
    :async-p t
    :read-only-p t
    :groups (util)))


;;
;;; Local slash commands

(defvar mevedel-slash-commands)

(defconst mevedel-skills--mode-command-candidates
  '(("default" . " ask before write tools")
    ("accept-edits" . " auto-apply edit previews")
    ("plan" . " read-only planning mode")
    ("trust-all" . " auto-allow tools")
    ("edit" . " alias for accept-edits")
    ("edits" . " alias for accept-edits")
    ("auto" . " alias for trust-all"))
  "Completion candidates and annotations for `/mode'.")

(defconst mevedel-skills--validation-target-command-candidates
  '(("current" . " current changes")
    ("uncommitted" . " current changes")
    ("HEAD" . " last commit")
    ("last" . " last commit")
    ("branch:" . " base branch")
    ("base:" . " base branch")
    ("commit:" . " specific commit"))
  "Completion candidates and annotations for `/review' and `/verify'.")

(defconst mevedel-skills--plugin-command-candidates
  '(("list" . " installed plugins")
    ("enable" . " enable plugin")
    ("disable" . " disable plugin")
    ("hooks" . " manage plugin hooks")
    ("install" . " install GitHub plugin")
    ("update" . " update installed plugin")
    ("remove" . " remove installed plugin")
    ("uninstall" . " remove installed plugin")
    ("reload" . " rescan current session plugin skills"))
  "Completion candidates and annotations for `/plugin'.")

(defconst mevedel-skills--skills-command-candidates
  '(("list" . " list available skills")
    ("help" . " show help for a skill")
    ("enable" . " enable a skill")
    ("disable" . " disable a skill"))
  "Completion candidates and annotations for `/skills'.")

(defconst mevedel-skills--slash-command-annotations
  '(("tokens" . " [command] no args; estimate tokens")
    ("model" . " [command] model name")
    ("compact" . " [command] optional summary guidance")
    ("plan" . " [command] optional first Plan-mode prompt")
    ("mode" . " [command] default | accept-edits | plan | trust-all")
    ("skills" . " [command] list | help NAME | enable NAME | disable NAME")
    ("tools" . " [command] list")
    ("auto" . " [command] no args; toggle auto mode")
    ("clear" . " [command] no args; start a fresh segment")
    ("help" . " [command] no args; list commands and skills")
    ("init" . " [command] optional repository bootstrap focus")
    ("plugin" . " [command] list | enable | disable | hooks | install | update | remove | uninstall | reload")
    ("review" . " [command] picker; target args or custom instructions")
    ("verify" . " [command] picker; target args or custom instructions")
    ("worktree" . " [command] status | create"))
  "Root completion annotations for included slash commands.")

(defun mevedel-cmd--tokens (_args)
  "Print the estimated token usage of the current chat buffer."
  (message "Estimated tokens in this buffer: %d"
           (mevedel--estimate-tokens)))

(defun mevedel-skills--open-menu-or-message (area format-string &rest args)
  "Open cockpit AREA, or message FORMAT-STRING with ARGS."
  (if (fboundp 'mevedel-menu-open)
      (condition-case nil
          (mevedel-menu-open area)
        (user-error
         (apply #'message format-string args)))
    (apply #'message format-string args)))

(defun mevedel-cmd--model (args)
  "Show or set the gptel model for the current chat buffer.
With a non-empty ARGS string, set `gptel-model' to the interned symbol.
A BACKEND:MODEL argument sets both buffer-local `gptel-backend' and
`gptel-model'.  With no ARGS, open the model cockpit surface when the
current buffer belongs to a live session pair."
  (if (and args (not (string-blank-p args)))
      (let ((trimmed (string-trim args)))
        (if-let* ((provider (mevedel-model-resolve-provider trimmed t)))
            (let ((backend (plist-get provider :backend))
                  (model (plist-get provider :model)))
              (setq-local gptel-backend backend)
              (setq-local gptel-model model)
              (message "Model set to %s" trimmed))
          (let ((model (intern trimmed)))
            (setq-local gptel-model model)
            (message "Model set to %s" model))))
    (mevedel-skills--open-menu-or-message
     'model "Current model: %s" gptel-model)))

(defun mevedel-cmd--compact (args)
  "Run `mevedel-compact' on the current chat buffer with ARGS."
  (mevedel-compact nil args))

(defun mevedel-skills--refresh-view-input-prompt ()
  "Refresh the associated view prompt when it is available."
  (let ((view-buf (cond
                   ((and (boundp 'mevedel--view-buffer)
                         (buffer-live-p mevedel--view-buffer))
                    mevedel--view-buffer)
                   ((and (boundp 'mevedel--data-buffer)
                         (buffer-live-p mevedel--data-buffer))
                    (buffer-local-value 'mevedel--view-buffer
                                        mevedel--data-buffer)))))
    (when (and view-buf
               (fboundp 'mevedel-view-refresh-input-prompt))
      (with-current-buffer view-buf
        (mevedel-view-refresh-input-prompt)))))

(defun mevedel-cmd--mode (args)
  "Show or set `mevedel-permission-mode' for the current chat buffer.
ARGS is the raw slash-command argument string.
Recognized modes: default, accept-edits, plan, trust-all, and UI aliases.

Routes through the lifecycle-aware permission transition path."
  (if (and args (not (string-blank-p args)))
      (let ((mode (mevedel-permission-mode-normalize args)))
        (mevedel-permission-mode-transition mode)
        (message "Permission mode set to %s" mode))
    (mevedel-skills--open-menu-or-message
     'mode "Current permission mode: %s" mevedel-permission-mode)))

(defun mevedel-cmd--plan (args)
  "Enter Plan mode, optionally sending ARGS as the first prompt."
  (mevedel-permission-mode-transition 'plan args))

(defun mevedel-cmd--auto (_args)
  "Toggle trust-all auto mode for the current session."
  (unless (bound-and-true-p mevedel--session)
    (user-error "No mevedel session in this buffer"))
  (let* ((current (or (mevedel-session-permission-mode mevedel--session)
                      mevedel-permission-mode
                      'default))
         (auto-on-p (eq current 'trust-all)))
    (mevedel-permission-mode-transition
     (if auto-on-p 'default 'trust-all))
    (if auto-on-p
        (message "mevedel: auto mode off")
      (message "mevedel: auto mode on"))))

(defun mevedel-cmd--clear-trim-bare-prefix (prefix)
  "Delete PREFIX when it is the only text on the pending prompt line."
  (when (and prefix (not (string-empty-p prefix)))
    (let* ((end (point-max))
           (start (- end (length prefix))))
      (when (and (<= (point-min) start)
                 (equal prefix
                        (buffer-substring-no-properties start end))
                 (save-excursion
                   (goto-char start)
                   (= start (line-beginning-position))))
        (delete-region start end)))))

(defun mevedel-cmd--clear (_args)
  "Start a new, empty chat segment."
  (let ((prefix (or (alist-get major-mode gptel-prompt-prefix-alist) "")))
    (cond
     ((bound-and-true-p mevedel-session--read-only-mode)
      (user-error "Session is read-only"))
     ((and (bound-and-true-p mevedel-session-persistence)
           (bound-and-true-p mevedel--session)
           (mevedel-session-save-path mevedel--session)
           buffer-file-name)
      (require 'mevedel-session-persistence)
      (let ((inhibit-read-only t))
        (mevedel-cmd--clear-trim-bare-prefix prefix))
      (mevedel-session-persistence-start-fresh-segment
       mevedel--session (current-buffer)
       :initial-text prefix)
      (message "mevedel: started a fresh chat segment"))
     (t
      (when (yes-or-no-p "Clear all chat buffer content? ")
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert prefix)
          (goto-char (point-max)))
        (message "mevedel: cleared chat buffer"))))))

(defun mevedel-cmd--help (_args)
  "Show the list of local slash commands and available `$' skills."
  (let* ((locals (mapconcat
                  (lambda (cell) (format "/%s" (car cell)))
                  mevedel-slash-commands
                  "  "))
         (skills (and mevedel--session
                      (mapconcat
                       (lambda (skill)
                         (format "$%s" (mevedel-skill-name skill)))
                       (mevedel-skills--user-visible-skills
                        mevedel--session)
                       "  "))))
    (mevedel-skills--open-menu-or-message
     'help
     "Commands: %s%s"
     locals
     (if (and skills (not (string-empty-p skills)))
         (format "\nSkills: %s" skills)
       ""))))

(defun mevedel-cmd--skills--require-name (name action)
  "Return NAME or signal a usage error for ACTION."
  (if (and (stringp name) (not (string-empty-p name)))
      name
    (user-error "Usage: /skills %s NAME" action)))

(defconst mevedel-skills-list-buffer-name "*mevedel skills*"
  "Name of the skill listing buffer.")

(defconst mevedel-skills-help-buffer-name "*mevedel skills help*"
  "Name of the skills cockpit help buffer.")

(defun mevedel-skills--skill-status-label (skill)
  "Return user-facing enabled status for SKILL."
  (if (mevedel-skills--skill-enabled-p skill) "enabled" "disabled"))

(defun mevedel-skills--skill-source-label (skill)
  "Return user-facing source label for SKILL."
  (let ((source (or (mevedel-skill-source skill) 'unknown))
        (family (mevedel-skill-source-family skill)))
    (if family
        (format "%s/%s" source family)
      (symbol-name source))))

(defun mevedel-skills-count-label (session)
  "Return enabled/total skill count label for SESSION."
  (let ((enabled 0)
        (total 0))
    (dolist (skill (and session (mevedel-session-skills session)))
      (setq total (1+ total))
      (when (mevedel-skills--skill-enabled-p skill)
        (setq enabled (1+ enabled))))
    (format "%d/%d" enabled total)))

(defun mevedel-skills-list--item-id (skill)
  "Return stable tabulated row id for SKILL."
  (mevedel-skill-name skill))

(defun mevedel-skills-list--status-cell (skill)
  "Return the propertized table status cell for SKILL."
  (let ((status (mevedel-skills--skill-status-label skill)))
    (propertize status
                'face
                (if (mevedel-skills--skill-enabled-p skill)
                    'success
                  'shadow))))

(defun mevedel-skills-list--entry (skill &optional _context)
  "Return a `tabulated-list-mode' row for SKILL."
  (list
   (mevedel-skills-list--item-id skill)
   (vector
    (mevedel-skills-list--status-cell skill)
    (mevedel-skill-name skill)
    (mevedel-skills--skill-source-label skill)
    (or (mevedel-skill-description skill) ""))))

(defun mevedel-skills--skill-detail-text (skill &optional _context)
  "Return detail text for SKILL."
  (format
   "Skill %s [%s]\nSource: %s\nDescription: %s%s"
   (mevedel-skill-name skill)
   (mevedel-skills--skill-status-label skill)
   (mevedel-skills--skill-source-label skill)
   (or (mevedel-skill-description skill) "")
   (if-let* ((file (mevedel-skill-source-file skill)))
       (format "\nFile: %s" file)
     "")))

(defun mevedel-skills-list--session-label (&optional context)
  "Return CONTEXT's skills cockpit session label."
  (if-let* ((session (and context
                          (mevedel-cockpit-context-session context))))
      (mevedel-session-name session)
    "unknown"))

(defun mevedel-skills-list--header-line (&optional items context)
  "Return the skills cockpit header line for ITEMS and CONTEXT."
  (let* ((total (length items))
         (enabled (cl-count-if #'mevedel-skills--skill-enabled-p
                               items)))
    (format "%s  %s  %d/%d enabled    RET details  e toggle  o source  g refresh  ? help  q back"
            (propertize "mevedel: skills"
                        'face 'font-lock-function-name-face)
            (mevedel-skills-list--session-label context)
            enabled
            total)))

(defun mevedel-skills-list--collect (context)
  "Return skill cockpit items for CONTEXT."
  (or (mevedel-cockpit-context-session context)
      (user-error "No mevedel session in this buffer"))
  (mevedel-session-skills (mevedel-cockpit-context-session context)))

(defun mevedel-skills-list-refresh ()
  "Refresh the current skill listing buffer."
  (interactive)
  (require 'mevedel-cockpit)
  (mevedel-cockpit-surface-refresh))

(defun mevedel-skills-list--skill-at-point ()
  "Return the skill at point in a skill listing buffer."
  (require 'mevedel-cockpit)
  (mevedel-cockpit-surface-selected))

(defun mevedel-skills-list-details ()
  "Show details for the skill at point."
  (interactive)
  (require 'mevedel-cockpit)
  (mevedel-cockpit-surface-details))

(defun mevedel-skills-list-toggle-enabled ()
  "Toggle enabled state for the skill at point."
  (interactive)
  (let* ((skill (mevedel-skills-list--skill-at-point))
         (name (mevedel-skill-name skill))
         (enable (not (mevedel-skills--skill-enabled-p skill))))
    (mevedel-skills--set-enabled skill enable)
    (when-let* ((data-buffer (mevedel-cockpit-context-data-buffer
                              (mevedel-cockpit-surface-context))))
      (with-current-buffer data-buffer
        (mevedel-skills--refresh-view-input-prompt)))
    (mevedel-skills-list-refresh)
    (message "mevedel: skill %s %s"
             name
             (if enable "enabled" "disabled"))))

(defun mevedel-skills-list-open-source ()
  "Open the selected skill's source file or directory."
  (interactive)
  (let* ((skill (mevedel-skills-list--skill-at-point))
         (file (mevedel-skill-source-file skill))
         (dir (mevedel-skill-source-dir skill))
         (target (cond
                  ((and file (file-readable-p file)) file)
                  ((and dir (file-directory-p dir)) dir))))
    (unless target
      (user-error "Skill source is not readable: %s"
                  (or file dir "unknown")))
    (find-file target)))

(defconst mevedel-skills-list--surface
  `(:buffer-name ,mevedel-skills-list-buffer-name
    :label "skills cockpit"
    :row-label "skill"
    :mode mevedel-skills-list-mode
    :format [("State" 9 t)
             ("Name" 24 t)
             ("Source" 18 t)
             ("Description" 0 t)]
    :sort-key ("Name" . nil)
    :require-session t
    :collect mevedel-skills-list--collect
    :entry mevedel-skills-list--entry
    :header mevedel-skills-list--header-line
    :details mevedel-skills--skill-detail-text
    :details-buffer "*mevedel skill details*"
    :help-buffer ,mevedel-skills-help-buffer-name
    :help-function mevedel-skills-list--help-text
    :keys (("e" "Enable or disable selected skill"
            mevedel-skills-list-toggle-enabled)
           ("o" "Open selected skill source"
            mevedel-skills-list-open-source)))
  "Cockpit surface spec for the skill list.")

(defun mevedel-skills-list--help-text (&optional _context)
  "Return help text for the skills cockpit."
  (concat
   "mevedel skills cockpit\n\n"
   "Keys\n"
   (mevedel-cockpit-surface-key-help-text mevedel-skills-list--surface)
   "\n\n"
   "Slash equivalents\n"
   "/skills help NAME\n"
   "/skills enable NAME, /skills disable NAME\n"))

(defun mevedel-skills-list-help ()
  "Open skills cockpit help."
  (interactive)
  (require 'mevedel-cockpit)
  (mevedel-cockpit-show-help
   mevedel-skills-help-buffer-name
   (mevedel-skills-list--help-text)))

(defun mevedel-skills-list-quit ()
  "Quit the skills cockpit and return to the main session cockpit."
  (interactive)
  (require 'mevedel-cockpit)
  (mevedel-cockpit-quit "skills cockpit"))

(define-derived-mode mevedel-skills-list-mode tabulated-list-mode
  "mevedel-skills"
  "Major mode for managing mevedel skills."
  (require 'mevedel-cockpit)
  (mevedel-cockpit-setup-tabulated-surface
   mevedel-skills-list--surface))

(defun mevedel-skills-list-open (&optional context)
  "Open the skill listing buffer for CONTEXT."
  (require 'mevedel-cockpit)
  (let ((context (or context (mevedel-cockpit-current-context))))
    (unless (mevedel-cockpit-context-session context)
      (user-error "No mevedel session in this buffer"))
    (mevedel-cockpit-open-surface mevedel-skills-list--surface context)))

(defun mevedel-skills--list-message-text (session)
  "Return fallback `/skills list' message text for SESSION."
  (concat
   (format "mevedel skills for %s\n\n"
           (mevedel-session-name session))
   (if-let* ((skills (mevedel-session-skills session)))
       (mapconcat
        (lambda (skill)
          (format "%s [%s] source:%s%s"
                  (mevedel-skill-name skill)
                  (mevedel-skills--skill-status-label skill)
                  (mevedel-skills--skill-source-label skill)
                  (if-let* ((desc (mevedel-skill-description skill)))
                      (if (string-empty-p desc)
                          ""
                        (format " - %s" desc))
                    "")))
        skills
        "\n")
     "No skills available.")))

(defun mevedel-cmd--skills--help (session name)
  "Message help for skill NAME in SESSION."
  (if-let* ((skill (mevedel-session-get-skill session name)))
      (message "%s" (mevedel-skills--skill-detail-text skill))
    (message "Unknown skill: %s" name)))

(defun mevedel-cmd--skills (args)
  "List, describe, enable, or disable skills using ARGS."
  (unless (bound-and-true-p mevedel--session)
    (user-error "No mevedel session in this buffer"))
  (let* ((parts (split-string (or args "") "[ \t\n]+" t))
         (action (or (car parts) "list"))
         (name (cadr parts)))
    (pcase action
      ("list"
       (mevedel-skills--open-menu-or-message
        'skills "%s" (mevedel-skills--list-message-text mevedel--session)))
      ("help"
       (mevedel-cmd--skills--help
        mevedel--session
        (mevedel-cmd--skills--require-name name "help")))
      ("enable"
       (setq name (mevedel-cmd--skills--require-name name "enable"))
       (mevedel-skills--set-enabled
        (mevedel-skills--session-skill-or-name mevedel--session name)
        t)
       (mevedel-skills--refresh-view-input-prompt)
       (message "Skill %s enabled" name))
      ("disable"
       (setq name (mevedel-cmd--skills--require-name name "disable"))
       (mevedel-skills--set-enabled
        (mevedel-skills--session-skill-or-name mevedel--session name)
        nil)
       (mevedel-skills--refresh-view-input-prompt)
       (message "Skill %s disabled" name))
      (_
       (message "Usage: /skills [list|help NAME|enable NAME|disable NAME]")))))

(defun mevedel-cmd--tools (args)
  "Open the tools surface using ARGS."
  (let ((args (string-trim (or args ""))))
    (if (member args '("" "list"))
        (if (fboundp 'mevedel-menu-open)
            (mevedel-menu-open 'tools)
          (user-error "No mevedel session cockpit here"))
      (message "Usage: /tools [list]"))))

(defvar mevedel-slash-commands
  '(("tokens"  . mevedel-cmd--tokens)
    ("model"   . mevedel-cmd--model)
    ("compact" . mevedel-cmd--compact)
    ("plan"    . mevedel-cmd--plan)
    ("mode"    . mevedel-cmd--mode)
    ("skills"  . mevedel-cmd--skills)
    ("tools"   . mevedel-cmd--tools)
    ("auto"    . mevedel-cmd--auto)
    ("clear"   . mevedel-cmd--clear)
    ("help"    . mevedel-cmd--help)
    ("plugin"  . mevedel-plugins-slash-command))
  "Alist of local slash commands.
Each entry is a (NAME . HANDLER) pair.  HANDLER is a function
accepting a single ARGS string (the text after the command name,
trimmed), and is expected to execute immediately and report its own
result.  If a handler returns a string, the dispatch path shows it with
`message'.
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

(defun mevedel-skills--parse-prefixed-line (text prefix)
  "Parse TEXT for a leading PREFIX command line.
Returns (NAME ARGS OFFSET) when TEXT starts (after optional leading
whitespace) with PREFIX followed by an identifier, or nil otherwise.
NAME is the command name; ARGS is the rest of TEXT after the
command name (the remainder of the first line plus every
subsequent line, joined and trimmed); OFFSET is the 0-based
character position of PREFIX within TEXT."
  (let* ((trimmed (string-trim-left text))
         (offset (- (length text) (length trimmed))))
    (when (and (> (length trimmed) 1) (eq (aref trimmed 0) prefix))
      (let* ((line-end (or (string-match "\n" trimmed) (length trimmed)))
             (line (substring trimmed 1 line-end))
             (rest (substring trimmed line-end))
             (space (string-match "[ \t]" line))
             (name (if space (substring line 0 space) line))
             (first-line-args (if space (substring line space) ""))
             (args (string-trim (concat first-line-args rest))))
        (when (string-match-p "\\`[A-Za-z0-9_.:-]+\\'" name)
          (list name args offset))))))

(defun mevedel-skills--parse-slash-line (text)
  "Parse TEXT for a leading `/command [args]' line.

Local slash commands (`/model', `/mode', etc.) parse only the first
whitespace-separated token from ARGS and ignore the rest, so extending
ARGS to include subsequent lines does not change their behavior."
  (mevedel-skills--parse-prefixed-line text ?/))

(defun mevedel-skills--parse-skill-line (text)
  "Parse TEXT for a leading `$skill [args]' line.

User skill invocations take the user's prompt body as ARGS, and prompt
bodies are naturally multi-line."
  (mevedel-skills--parse-prefixed-line text ?$))

(defun mevedel-skills--escaped-position-p (text pos)
  "Return non-nil when POS in TEXT is escaped by an odd backslash run."
  (let ((i (1- pos))
        (count 0))
    (while (and (>= i 0)
                (eq (aref text i) ?\\))
      (cl-incf count)
      (cl-decf i))
    (cl-oddp count)))

(defun mevedel-skills--inline-skill-boundary-p (text pos)
  "Return non-nil when a `$skill' mention may start at POS in TEXT."
  (or (= pos 0)
      (not (string-match-p
            "[[:alnum:]_]"
            (char-to-string (aref text (1- pos)))))))

(defun mevedel-skills--quote-delimiter-p (text pos)
  "Return non-nil when POS is a quote delimiter in TEXT."
  (and (not (mevedel-skills--escaped-position-p text pos))
       (let ((quote (aref text pos)))
         (and (memq quote '(?\" ?\'))
              (or (eq quote ?\")
                  (not (and (mevedel-skills--word-char-p
                             (and (> pos 0) (aref text (1- pos))))
                            (mevedel-skills--word-char-p
                             (and (< (1+ pos) (length text))
                                  (aref text (1+ pos)))))))))))

(defun mevedel-skills--quoted-inline-skill-p (text start end)
  "Return non-nil when TEXT START..END is inside single/double quotes."
  (let ((line-start (or (and (> start 0)
                             (string-match-p "\n"
                                             (substring text 0 start))
                             (1+ (cl-position ?\n text
                                               :end start
                                               :from-end t)))
                        0))
        (line-end (or (cl-position ?\n text :start end)
                      (length text)))
        quoted)
    (dolist (quote '(?\" ?\') quoted)
      (let ((open nil)
            (pos (1- start)))
        (while (and (not open) (>= pos line-start))
          (when (and (eq (aref text pos) quote)
                     (mevedel-skills--quote-delimiter-p text pos))
            (setq open pos))
          (cl-decf pos))
        (when open
          (setq pos end)
          (while (and (not quoted) (< pos line-end))
            (when (and (eq (aref text pos) quote)
                       (mevedel-skills--quote-delimiter-p text pos))
              (setq quoted t))
            (cl-incf pos)))))))

(defun mevedel-skills--inline-skill-live-p
    (text start end first-nonspace code-ranges)
  "Return non-nil when TEXT START..END is a live inline `$skill' mention."
  (and (not (= start first-nonspace))
       (mevedel-skills--inline-skill-boundary-p text start)
       (not (mevedel-skills--escaped-position-p text start))
       (not (mevedel-skills--quoted-inline-skill-p text start end))
       (not (mevedel-skills--ranges-overlap-p
             code-ranges start end))))

(defun mevedel-skills--resolve-inline-skill-name (raw-name lookup)
  "Resolve RAW-NAME with LOOKUP, trimming trailing punctuation if needed.
Return (NAME . VALUE), preferring an exact match.  Only punctuation
that is also allowed in skill names is trimmed, so `$foobar' does not
fall back to `$foo'."
  (let ((name raw-name)
        found)
    (while (and (not found) (> (length name) 0))
      (when-let* ((value (funcall lookup name)))
        (setq found (cons name value)))
      (unless found
        (let ((last (aref name (1- (length name)))))
          (if (memq last '(?. ?: ?-))
              (setq name (substring name 0 -1))
            (setq name "")))))
    found))

(defun mevedel-skills--inline-skill-mentions (text session)
  "Return live inline `$skill' mentions in TEXT for SESSION.

Unknown `$foo' text is ignored.  Known but unavailable skills return
a single plist with :error and :message so callers can block the send."
  (let ((pos 0)
        (first-nonspace (or (string-match-p "\\S-" text) -1))
        (code-ranges (mevedel-skills--markdown-code-ranges text))
        (seen (make-hash-table :test #'equal))
        mentions
        problem)
    (while (and (not problem)
                (string-match "\\$\\([A-Za-z0-9_.:-]+\\)" text pos))
      (let* ((start (match-beginning 0))
             (raw-name (match-string 1 text))
             (resolved
              (and session
                   (mevedel-skills--resolve-inline-skill-name
                    raw-name
                    (lambda (candidate)
                      (mevedel-session-get-skill session candidate)))))
             (name (car-safe resolved))
             (end (and name (+ start 1 (length name))))
             (skill (cdr-safe resolved)))
        (setq pos (match-end 0))
        (when (and skill
                   (mevedel-skills--inline-skill-live-p
                    text start end first-nonspace code-ranges))
          (cond
           ((not (mevedel-skills--skill-enabled-p skill))
            (setq problem
                  (list :error 'disabled
                        :message
                        (format "Skill $%s is disabled. Enable it with /skills enable %s or escape it as \\$%s."
                                name name name))))
           ((not (mevedel-skill-user-invocable-p skill))
            (setq problem
                  (list :error 'not-user-invocable
                        :message
                        (format "Skill $%s is not user-invocable. Escape it as \\$%s to send it literally."
                                name name))))
           ((eq (mevedel-skill-context skill) 'fork)
            (setq problem
                  (list :error 'fork-inline
                        :message
                        (format "Fork skill $%s must be invoked at the start of the prompt. Use $%s ..., or escape, quote, or code-span it to send literal text."
                                name name))))
           ((not (gethash (mevedel-skill-name skill) seen))
            (puthash (mevedel-skill-name skill) t seen)
            (push (list :name (mevedel-skill-name skill)
                        :start start
                        :end end
                        :skill skill)
                  mentions))))))
    (or problem (nreverse mentions))))

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
fork skill suppresses the main `gptel-send'; it records the
foreground agent's final result as the assistant side of that turn and
runs the normal post-response hooks so the view and persistence layers
observe the completed response."
  (require 'mevedel-utilities)
  (let* ((render-data (plist-get outcome :render-data))
         (hook-audits (plist-get outcome :hook-audits))
         (result (or (plist-get outcome :result)
                     "Fork skill produced no result."))
         (result (if render-data
                     (progn
                       (require 'mevedel-pipeline)
                       (concat result
                               (mevedel-pipeline--format-render-data-block
                                render-data)))
                   result)))
    (unless (bound-and-true-p mevedel--current-request)
      (when (bound-and-true-p mevedel--session)
        (mevedel-request-begin mevedel--session
                               (and (boundp 'mevedel--current-directive-uuid)
                                    mevedel--current-directive-uuid))))
    (goto-char (point-max))
    (when-let* ((synthetic (plist-get outcome :synthetic-user-message)))
      (let ((user-turn-start (point)))
        (unless (bolp) (insert "\n"))
        (insert synthetic)
        (unless (bolp) (insert "\n"))
        (mevedel--clear-user-turn-gptel-properties
         user-turn-start (point))))
    (when hook-audits
      (insert (mapconcat #'mevedel--format-hook-audit-record
                         hook-audits "")))
    (unless (bolp) (insert "\n"))
    (insert gptel-response-separator)
    (let ((start (point)))
      (insert result)
      (unless (eq (char-before) ?\n)
        (insert "\n"))
      (let ((end (point)))
        (add-text-properties start end '(gptel response))
        (mevedel--restore-render-data-gptel-properties start end)
        (run-hook-with-args 'gptel-post-response-functions start end)
        (mevedel--restore-render-data-gptel-properties start end)
        (mevedel-skills--finalize-fork-turn)))))

(defun mevedel-skills--finalize-fork-turn ()
  "Run completed-turn bookkeeping for a direct fork skill turn.

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
             (force-mode-line-update))))
        (require 'mevedel-presets)
        (mevedel--run-turn-terminal-hook
         (gptel-make-fsm :info (list :buffer (current-buffer)))
         'Stop 'completed))
    (when (bound-and-true-p mevedel--current-request)
      (mevedel-request-end))
    (gptel--update-status " Ready" 'success)))

(defun mevedel-skills--text-after-local-command-delete
    (delete-start region-end after-prefix)
  "Return buffer text after deleting a local slash command region.
DELETE-START and REGION-END bound the command text.  AFTER-PREFIX means
the deleted command followed the prompt prefix."
  (let ((text (buffer-substring-no-properties (point-min) (point-max))))
    (with-temp-buffer
      (insert text)
      (delete-region delete-start region-end)
      (unless after-prefix
        (mevedel-skills--ensure-fresh-line))
      (buffer-string))))

(defun mevedel-skills--refresh-visited-file-before-local-edit
    (delete-start region-end after-prefix)
  "Refresh stale visited-file metadata before slash command edits.
DELETE-START and REGION-END bound the command text.  AFTER-PREFIX means
the deleted command followed the prompt prefix."
  (when (and buffer-file-name
             (bound-and-true-p mevedel-session-persistence)
             (bound-and-true-p mevedel--session)
             (mevedel-session-save-path mevedel--session))
    (require 'mevedel-session-persistence)
    (mevedel-session-persistence--refresh-visited-file-modtime-or-error
     (mevedel-skills--text-after-local-command-delete
      delete-start region-end after-prefix))))

(defun mevedel-skills--command-delete-context (command-pos)
  "Return deletion context for a command starting at COMMAND-POS.
The result is a plist with :delete-start and :after-prefix."
  (let* ((prefix (alist-get major-mode gptel-prompt-prefix-alist))
         (has-prefix (and prefix (not (string-empty-p prefix))))
         (line-start (save-excursion
                       (goto-char command-pos)
                       (line-beginning-position)))
         (before-command
          (buffer-substring-no-properties line-start command-pos))
         (after-prefix (and has-prefix (equal before-command prefix)))
         (delete-start
          (cond
           (after-prefix command-pos)
           ((string-match-p "\\`[ \t]*\\'" before-command) line-start)
           (t command-pos))))
    (list :delete-start delete-start :after-prefix after-prefix)))

(defun mevedel-skills--handle-user-skill-outcome
    (skill outcome delete-start region-end after-prefix continue-fn)
  "Apply user skill OUTCOME in the current data buffer.

SKILL is the invoked user skill.  DELETE-START and REGION-END bound
the original `$skill' text.  AFTER-PREFIX means the command followed
the prompt prefix.

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
        (let ((body (or (plist-get outcome :body)
                        (format "Skill '%s' produced no body."
                                (mevedel-skill-name skill)))))
          (insert body)
          (mevedel-skills--insert-inline-user-skill-render-data
           skill (plist-get outcome :arguments)))
        (when continue-fn
          (funcall continue-fn))
        'skill)
       ('fork
        (when (fboundp 'mevedel-review-transform-outcome)
          (setq outcome
                (mevedel-review-transform-outcome
                 (mevedel-skill-name skill) outcome)))
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

(defun mevedel-skills--dispatch-slash-command ()
  "Parse and dispatch a local `/command' in the current chat buffer.

Returns:
- `local'   a local command ran; caller should abort the send.
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
           (delete-context
            (mevedel-skills--command-delete-context slash-pos))
           (delete-start (plist-get delete-context :delete-start))
           (after-prefix (plist-get delete-context :after-prefix)))
      (cond
       (local
        (mevedel-skills--refresh-visited-file-before-local-edit
         delete-start (cdr region) after-prefix)
        (delete-region delete-start (cdr region))
        (unless after-prefix
          (mevedel-skills--ensure-fresh-line))
        (when-let* ((result (funcall (cdr local) args))
                    ((stringp result)))
          (message "%s" result))
        'local)
       (t
        (message "Unknown slash command: /%s" name)
        'unknown)))))

(defun mevedel-skills--dispatch-skill-command (&optional continue-fn)
  "Parse and dispatch a `$skill' invocation in the current chat buffer.

Returns:
- `skill'   a skill was expanded into the prompt region; caller
            should proceed with the send if CONTINUE-FN was nil.
- `unknown' a named skill existed but could not be invoked; caller
            should abort the send.
- nil       no known `$skill' present; caller should proceed as usual."
  (when-let* ((region (mevedel-skills--current-prompt-region))
              (text (buffer-substring-no-properties (car region) (cdr region)))
              (parsed (mevedel-skills--parse-skill-line text)))
    (let* ((name (nth 0 parsed))
           (args (nth 1 parsed))
           (skill-pos (+ (car region) (nth 2 parsed)))
           (skill (and mevedel--session
                       (mevedel-session-get-skill mevedel--session name))))
      (when skill
        (let* ((delete-context
                (mevedel-skills--command-delete-context skill-pos))
               (delete-start (plist-get delete-context :delete-start))
               (after-prefix (plist-get delete-context :after-prefix))
               (buffer (current-buffer))
               (region-end (cdr region))
               (dispatch-result 'skill))
          (mevedel-skills-invoke
           skill args
           (lambda (outcome)
             (when (buffer-live-p buffer)
               (with-current-buffer buffer
                 (setq dispatch-result
                       (mevedel-skills--handle-user-skill-outcome
                        skill outcome delete-start region-end
                        after-prefix continue-fn)))))
           :trigger 'user-skill)
          dispatch-result)))))

(defun mevedel-skills--prepare-inline-attachments
    (mentions callback &optional prepared)
  "Prepare inline skill MENTIONS, then call CALLBACK.
CALLBACK receives (:status ok :attachments LIST) or an error plist.
PREPARED is the accumulator used for sequential async preparation."
  (if (null mentions)
      (funcall callback
               (list :status 'ok :attachments (nreverse prepared)))
    (let* ((mention (car mentions))
           (skill (plist-get mention :skill))
           (name (plist-get mention :name)))
      (mevedel-skills-invoke
       skill ""
       (lambda (outcome)
         (pcase (plist-get outcome :status)
           ('ok
            (if (eq (plist-get outcome :kind) 'inline)
                (mevedel-skills--prepare-inline-attachments
                 (cdr mentions) callback
                 (cons (list :name name
                             :skill skill
                             :arguments ""
                             :body (plist-get outcome :body))
                       prepared))
              (funcall callback
                       (list :status 'error
                             :reason 'unsupported-context
                             :message
                             (format "Skill $%s cannot be attached inline."
                                     name)))))
           (_
            (funcall callback outcome))))
       :trigger 'user-skill))))

(defun mevedel-skills--prepare-inline-attachments-for-text
    (text session callback)
  "Prepare inline `$skill' attachments in TEXT for SESSION.
CALLBACK receives the prepared outcome.  Return `skill' when async
preparation took ownership, `unknown' when a known skill blocks the
send, or nil when TEXT has no inline attachments."
  (when-let* ((mentions (mevedel-skills--inline-skill-mentions
                         text session)))
    (cond
     ((plist-get mentions :error)
      (message "%s" (plist-get mentions :message))
      'unknown)
     (t
      (mevedel-skills--prepare-inline-attachments mentions callback)
      'skill))))

(defun mevedel-skills--stage-inline-attachments (attachments)
  "Store prepared inline ATTACHMENTS and return their render-data block."
  (setq-local mevedel-skills--pending-inline-attachments attachments)
  (mevedel-skills-format-inline-attachment-render-data attachments))

(defun mevedel-skills--clear-pending-inline-attachments ()
  "Clear pending inline attachment request state in the current buffer."
  (setq-local mevedel-skills--pending-request-context nil)
  (setq-local mevedel-skills--pending-inline-attachments nil))

(defun mevedel-skills--dispatch-inline-attachments (&optional continue-fn)
  "Prepare inline `$skill' attachments in the current prompt.
Return `skill' when preparation took ownership of continuing the send,
`unknown' when a known skill blocks the send, or nil when no inline
attachments exist."
  (when-let* ((region (mevedel-skills--current-prompt-region))
              (session (and (bound-and-true-p mevedel--session)
                            mevedel--session))
              ((not (bound-and-true-p
                     mevedel-skills--pending-inline-attachments)))
              (text (buffer-substring-no-properties (car region) (cdr region))))
    (let ((buffer (current-buffer)))
      (mevedel-skills--prepare-inline-attachments-for-text
       text session
       (lambda (outcome)
         (when (buffer-live-p buffer)
           (with-current-buffer buffer
             (pcase (plist-get outcome :status)
               ('ok
                (goto-char (point-max))
                (insert
                 (mevedel-skills--stage-inline-attachments
                  (plist-get outcome :attachments)))
                (when continue-fn
                  (funcall continue-fn)))
               (_
                (mevedel-skills--clear-pending-inline-attachments)
                (message "Inline skill failed: %s"
                         (or (plist-get outcome :message)
                             "unknown error")))))))))))

(defun mevedel-skills--gptel-send-advice (orig-fn &rest args)
  "`:around' advice on `gptel-send' for command dispatch.

ORIG-FN and ARGS are the original `gptel-send' function and arguments.

Dispatches the leading `/command' or `$skill' on the prompt region first.
- Local commands and unknown slashes abort the send (do not call
  ORIG-FN).
- Inline skills install body + pending-stash, then ORIG-FN is called
  from the invocation callback.
- Fork skills dispatch an agent directly and never call ORIG-FN for
  the `$skill' command.
- No command present -> proceed unchanged.

Pending-stash cleanup is tied to the continuation that actually resumes
ORIG-FN so async shell preparation does not clear the stash before the
request begin handler can drain it."
  (if (not (bound-and-true-p mevedel--session))
      (apply orig-fn args)
    (cl-labels
        ((continue ()
           (unwind-protect
               (apply orig-fn args)
             (mevedel-skills--clear-pending-inline-attachments))))
      (pcase (mevedel-skills--dispatch-slash-command)
        ((or 'local 'unknown) nil)
        (_
         (pcase (mevedel-skills--dispatch-skill-command #'continue)
           ((or 'unknown 'skill) nil)
           (_
            (pcase (mevedel-skills--dispatch-inline-attachments #'continue)
              ((or 'unknown 'skill) nil)
              (_ (continue))))))))))


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

(defun mevedel-skills--remaining-argument-hint (skill arguments)
  "Return display-only remaining argument hint for SKILL and ARGUMENTS.

Explicit `argument-hint' text is shown only before the user starts
typing arguments.  Named `arguments' frontmatter is shown as the
remaining positional slots after shell-style tokenization."
  (let ((hint (mevedel-skill-argument-hint skill))
        (names (mevedel-skill-argument-names skill))
        (tokens (mevedel-skills--parse-arguments arguments)))
    (cond
     ((and (stringp hint)
           (not (string-empty-p hint))
           (null tokens))
      hint)
     (names
      (let ((remaining (nthcdr (length tokens) names)))
        (and remaining
             (mapconcat (lambda (n) (format "[%s]" n))
                        remaining " "))))
     (t nil))))

(defun mevedel-skills--user-visible-skills (session &optional inline-only)
  "Return user-invocable skills visible in `$' completion for SESSION.
When INLINE-ONLY is non-nil, return only inline-context skills."
  (when session
    (cl-remove-if-not
     (lambda (skill)
       (and (mevedel-skill-user-invocable-p skill)
            (mevedel-skills--skill-enabled-p skill)
            (or (not inline-only)
                (eq (mevedel-skill-context skill) 'inline))))
     (mevedel-session-skills session))))

(defun mevedel-skills--slash-candidates (buffer session local-commands)
  "Return fresh slash command completion candidates.
BUFFER and SESSION are accepted for call-site symmetry with skill
completion.  LOCAL-COMMANDS is the slash command alist captured when
the CAPF table was created."
  (ignore buffer session)
  (mapcar #'car local-commands))

(defun mevedel-skills--skill-candidates
    (buffer session &optional inline-only)
  "Return fresh `$' skill completion candidates for BUFFER and SESSION.
When INLINE-ONLY is non-nil, include only inline-context skills."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (mevedel-skills--ensure-fresh buffer session)))
  (mapcar #'mevedel-skill-name
          (mevedel-skills--user-visible-skills session inline-only)))

(defun mevedel-skills--skill-by-name
    (buffer session name &optional inline-only)
  "Return fresh `$' completion skill named NAME for BUFFER and SESSION.
When INLINE-ONLY is non-nil, include only inline-context skills."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (mevedel-skills--ensure-fresh buffer session)))
  (cl-find name (mevedel-skills--user-visible-skills session inline-only)
           :key #'mevedel-skill-name
           :test #'equal))

(defun mevedel-skills--slash-completion-table
    (buffer session local-commands)
  "Return dynamic completion table for BUFFER, SESSION, and LOCAL-COMMANDS."
  (lambda (string pred action)
    (complete-with-action
     action
     (mevedel-skills--slash-candidates buffer session local-commands)
     string pred)))

(defun mevedel-skills--skill-completion-table
    (buffer session &optional inline-only)
  "Return dynamic `$' skill completion table for BUFFER and SESSION.
When INLINE-ONLY is non-nil, include only inline-context skills."
  (lambda (string pred action)
    (complete-with-action
     action
     (mevedel-skills--skill-candidates buffer session inline-only)
     string pred)))

(defun mevedel-skills--slash-command-annotation (name)
  "Return root completion annotation for local slash command NAME."
  (or (cdr (assoc name mevedel-skills--slash-command-annotations))
      " [command]"))

(defun mevedel-skills--slash-annotation
    (name _buffer _session local-commands)
  "Return completion annotation for slash command NAME.
LOCAL-COMMANDS is the slash-command alist."
  (and (assoc name local-commands)
       (mevedel-skills--slash-command-annotation name)))

(defun mevedel-skills--skill-annotation
    (name buffer session &optional inline-only)
  "Return completion annotation for `$' skill candidate NAME.
BUFFER and SESSION identify the current chat."
  (let* ((skill (mevedel-skills--skill-by-name
                 buffer session name inline-only))
         (annotation " [skill]")
         (dormant (and skill
                       (mevedel-skill-path-patterns skill)
                       (not (mevedel-skill-active-p skill))))
         (hint (and skill
                    (mevedel-skills--progressive-argument-hint skill))))
    (concat annotation
            (when dormant " [dormant]")
            (when hint (concat " " hint)))))

(defun mevedel-skills--model-command-candidates ()
  "Return current-backend model names suitable for `/model' completion."
  (let ((models (and (bound-and-true-p gptel-backend)
                     (fboundp 'gptel-backend-models)
                     (gptel-backend-models gptel-backend)))
        result)
    (dolist (model models)
      (let ((name (cond
                   ((fboundp 'gptel--model-name)
                    (gptel--model-name model))
                   ((symbolp model) (symbol-name model))
                   ((stringp model) model)
                   (t nil))))
        (when (and (stringp name) (not (string-empty-p name)))
          (push name result))))
    (when (bound-and-true-p gptel-model)
      (let ((name (if (symbolp gptel-model)
                      (symbol-name gptel-model)
                    (format "%s" gptel-model))))
        (unless (string-empty-p name)
          (push name result))))
    (sort (delete-dups result) #'string<)))

(defun mevedel-skills--plugin-name-candidates ()
  "Return installed plugin names for slash command argument completion."
  (require 'mevedel-plugins)
  (sort (delete-dups
         (mapcar #'mevedel-plugin-name (mevedel-plugins-list)))
        #'string<))

(defun mevedel-skills--plugin-command-argument-candidates (args arg-index)
  "Return `/plugin' completion candidates for ARGS at ARG-INDEX.
ARGS is the list of completed arguments before the argument being completed."
  (pcase arg-index
    (0 (mapcar #'car mevedel-skills--plugin-command-candidates))
    (1 (pcase args
         ((or `("enable") `("disable") `("update")
              `("remove") `("uninstall"))
          (mevedel-skills--plugin-name-candidates))
         (`("hooks")
          (delete-dups
           (append '("enable" "disable")
                   (mevedel-skills--plugin-name-candidates))))
         (_ nil)))
    (2 (pcase args
         (`("hooks" ,action)
          (if (member action '("enable" "disable"))
              (mevedel-skills--plugin-name-candidates)
            '("on" "off")))
         (_ nil)))
    (_ nil)))

(defun mevedel-skills--slash-command-argument-candidates
    (name &optional args arg-index)
  "Return completion candidates for slash command NAME.
ARGS is the list of completed command arguments, and ARG-INDEX is the zero-based
index of the argument being completed."
  (let ((arg-index (or arg-index 0)))
    (pcase name
      ("mode" (and (zerop arg-index)
                   (mapcar #'car mevedel-skills--mode-command-candidates)))
      ("skills" (and (zerop arg-index)
                     (mapcar #'car mevedel-skills--skills-command-candidates)))
      ("model" (and (zerop arg-index)
                    (mevedel-skills--model-command-candidates)))
      ("plugin" (mevedel-skills--plugin-command-argument-candidates
                 args arg-index))
      ("worktree" (and (zerop arg-index) '("status" "create")))
      ((or "review" "verify")
       (and (zerop arg-index)
            (mapcar #'car mevedel-skills--validation-target-command-candidates)))
      (_ nil))))

(defun mevedel-skills--plugin-command-argument-annotation
    (candidate args arg-index)
  "Return `/plugin' argument annotation for CANDIDATE.
ARGS is the list of completed arguments before ARG-INDEX."
  (pcase arg-index
    (0 (cdr (assoc candidate mevedel-skills--plugin-command-candidates)))
    (1 (pcase args
         ((or `("enable") `("disable") `("update")
              `("remove") `("uninstall"))
          " installed plugin")
         (`("hooks")
          (if (member candidate '("enable" "disable"))
              " hook command"
            " installed plugin"))
         (_ nil)))
    (2 (pcase args
         (`("hooks" ,action)
          (if (member action '("enable" "disable"))
              " installed plugin"
            " hook state"))
         (_ nil)))
    (_ nil)))

(defun mevedel-skills--slash-command-argument-annotation
    (name candidate &optional args arg-index)
  "Return annotation for slash command NAME argument CANDIDATE.
ARGS is the list of completed command arguments, and ARG-INDEX is the zero-based
index of the argument being completed."
  (let ((arg-index (or arg-index 0)))
    (pcase name
      ("mode" (and (zerop arg-index)
                   (cdr (assoc candidate
                               mevedel-skills--mode-command-candidates))))
      ("skills" (and (zerop arg-index)
                     (cdr (assoc candidate
                                 mevedel-skills--skills-command-candidates))))
      ("model" (and (zerop arg-index) " model"))
      ("plugin" (mevedel-skills--plugin-command-argument-annotation
                 candidate args arg-index))
      ("worktree" (and (zerop arg-index) " command"))
      ((or "review" "verify")
       (and (zerop arg-index)
            (cdr (assoc candidate
                        mevedel-skills--validation-target-command-candidates))))
      (_ nil))))

(defun mevedel-skills--slash-command-argument-table (name args arg-index)
  "Return dynamic completion table for slash command NAME.
ARGS and ARG-INDEX describe the argument position being completed."
  (lambda (string pred action)
    (complete-with-action
     action
     (mevedel-skills--slash-command-argument-candidates
      name args arg-index)
     string pred)))

(defun mevedel-skills--slash-root-exit-function (_candidate status)
  "Insert a real separator after completing a root command candidate.
STATUS is the completion exit status.  Ghost argument hints render after point,
so without an inserted space a user can visually see `$skill [arg]' while the
buffer still contains `$skill'."
  (when (and (memq status '(finished exact sole))
             (or (eobp)
                 (not (memq (char-after) '(?\s ?\t ?\n)))))
    (insert " ")))

(defun mevedel-skills--slash-command-start (&optional input-start)
  "Return slash command start position on this line, or nil.

INPUT-START, when non-nil, is the first editable input position in a
view buffer.  Without INPUT-START, a leading gptel prompt prefix on the
current line is skipped."
  (let ((line-start (line-beginning-position)))
    (cond
     (input-start
      (when (and (<= input-start (point))
                 (= line-start
                    (save-excursion
                      (goto-char input-start)
                      (line-beginning-position))))
        input-start))
     (t
      (let ((prefix (alist-get major-mode gptel-prompt-prefix-alist)))
        (if (and prefix
                 (not (string-empty-p prefix))
                 (save-excursion
                   (goto-char line-start)
                   (looking-at-p (regexp-quote prefix))))
            (+ line-start (length prefix))
          line-start))))))

(defun mevedel-skills--slash-capf-context (&optional input-start)
  "Return slash completion context at point.

INPUT-START constrains completion to the first view-composer line.  The return
value is a plist with :kind `root' or `argument' plus :start, :end, and
command-specific fields.  Argument contexts include :args and :arg-index for the
completed arguments before point."
  (catch 'context
    (let* ((slash-start (mevedel-skills--slash-command-start input-start))
           (line-end (line-end-position)))
      (when (and slash-start
                 (<= slash-start (point))
                 (< slash-start line-end)
                 (eq (char-after slash-start) ?/))
        (let* ((name-start (1+ slash-start))
               (name-end (save-excursion
                           (goto-char name-start)
                           (skip-chars-forward "A-Za-z0-9_.:-" line-end)
                           (point))))
          (when (and (<= name-start (point))
                     (<= (point) name-end))
            (throw 'context
                   (list :kind 'root :start name-start :end name-end)))
          (when (and (< name-start name-end)
                     (< name-end line-end)
                     (memq (char-after name-end) '(?\s ?\t))
                     (> (point) name-end))
            (let* ((name (buffer-substring-no-properties
                          name-start name-end))
                   (args-start (save-excursion
                                 (goto-char name-end)
                                 (skip-chars-forward " \t" line-end)
                                 (point))))
              (when (<= args-start (point))
                (let* ((arg-start (save-excursion
                                    (skip-chars-backward "^ \t\n" args-start)
                                    (point)))
                       (args-before
                        (split-string
                         (buffer-substring-no-properties args-start arg-start)
                         "[ \t]+" t)))
                  (throw 'context
                         (list :kind 'argument
                               :name name
                               :args args-before
                               :arg-index (length args-before)
                               :start arg-start
                               :end (point))))))))))))

(defun mevedel-skills--skill-capf-context (&optional input-start)
  "Return `$' skill completion context at point.

INPUT-START constrains completion to the first view-composer line.  The
return value is a plist with :kind `root', :start, :end, and
:inline-only."
  (catch 'context
    (let* ((command-start (mevedel-skills--slash-command-start input-start))
           (line-start (line-beginning-position))
           (line-end (line-end-position)))
      (when (and command-start
                 (<= command-start (point))
                 (< command-start line-end)
                 (eq (char-after command-start) ?$))
        (let* ((name-start (1+ command-start))
               (name-end (save-excursion
                           (goto-char name-start)
                           (skip-chars-forward "A-Za-z0-9_.:-" line-end)
                           (point))))
          (when (and (<= name-start (point))
                     (<= (point) name-end))
            (throw 'context
                   (list :kind 'root
                         :start name-start
                         :end name-end
                         :inline-only nil)))))
      (when (and (or (not input-start)
                     (<= input-start (point)))
                 (not (and command-start
                           (= command-start (point)))))
        (let* ((limit (max line-start (or input-start line-start)))
               (name-start (save-excursion
                             (skip-chars-backward
                              "A-Za-z0-9_.:-" limit)
                             (point)))
               (dollar-pos (1- name-start)))
          (when (and (<= limit dollar-pos)
                     (eq (char-after dollar-pos) ?$))
            (let* ((line-text (buffer-substring-no-properties
                               line-start line-end))
                   (relative-dollar (- dollar-pos line-start)))
              (when (and (mevedel-skills--inline-skill-boundary-p
                          line-text relative-dollar)
                         (not (mevedel-skills--escaped-position-p
                               line-text relative-dollar)))
                (throw 'context
                       (list :kind 'root
                             :start name-start
                             :end (point)
                             :inline-only t))))))))))

(defun mevedel-skills--slash-capf
    (buffer session local-commands &optional input-start)
  "Return command and skill CAPF for the current buffer.

Slash completion offers local slash commands and command arguments.
Dollar completion offers user-invocable skills.  BUFFER and SESSION are
used for skill discovery.  LOCAL-COMMANDS is the current slash command
alist.  INPUT-START constrains completion to the first view-composer
line when called from the view buffer."
  (when session
    (let ((slash-context (mevedel-skills--slash-capf-context input-start))
          (skill-context (mevedel-skills--skill-capf-context input-start)))
      (pcase (plist-get slash-context :kind)
        ('root
         (list (plist-get slash-context :start)
               (plist-get slash-context :end)
               (mevedel-skills--slash-completion-table
                buffer session local-commands)
               :exclusive 'no
               :annotation-function
               (lambda (name)
                 (mevedel-skills--slash-annotation
                  name buffer session local-commands))
               :exit-function
               #'mevedel-skills--slash-root-exit-function))
        ('argument
         (let* ((name (plist-get slash-context :name))
                (args (plist-get slash-context :args))
                (arg-index (plist-get slash-context :arg-index))
                (candidates
                 (and (assoc name local-commands)
                      (mevedel-skills--slash-command-argument-candidates
                       name args arg-index))))
           (when candidates
             (list (plist-get slash-context :start)
                   (plist-get slash-context :end)
                   (mevedel-skills--slash-command-argument-table
                    name args arg-index)
                   :exclusive 'no
                   :annotation-function
                   (lambda (candidate)
                     (mevedel-skills--slash-command-argument-annotation
                      name candidate args arg-index))))))
        (_
         (pcase (plist-get skill-context :kind)
           ('root
            (mevedel-skills--ensure-fresh buffer session)
            (let ((inline-only (plist-get skill-context :inline-only)))
              (list (plist-get skill-context :start)
                    (plist-get skill-context :end)
                    (mevedel-skills--skill-completion-table
                     buffer session inline-only)
                    :exclusive 'no
                    :annotation-function
                    (lambda (name)
                      (mevedel-skills--skill-annotation
                       name buffer session inline-only))
                    :exit-function
                    #'mevedel-skills--slash-root-exit-function)))))))))

(defun mevedel-slash-capf ()
  "Completion-at-point for slash commands, `$' skills, and options.

Root completion is active at the line-start `/name' or `$name' position,
after an optional gptel prompt prefix.  Inline `$name' completion is
active in prose for inline-context skills.  Command option completion
is active for slash commands that declare finite argument choices."
  (when (bound-and-true-p mevedel--session)
    (mevedel-skills--slash-capf
     (current-buffer) mevedel--session mevedel-slash-commands)))


;;
;;; Font-lock

(defun mevedel-skills--fontify-dollar-keyword (end)
  "Find a known `$skill' mention before END for font-lock."
  (let (found)
    (while (and (not found)
                (re-search-forward "\\$\\([A-Za-z0-9_.:-]+\\)" end t))
      (when-let* ((session (and (bound-and-true-p mevedel--session)
                                mevedel--session))
                  (start (match-beginning 0))
                  (resolved
                   (mevedel-skills--resolve-inline-skill-name
                    (match-string-no-properties 1)
                    (lambda (name)
                      (mevedel-session-get-skill session name))))
                  ((mevedel-mentions--valid-mention-context-p start)))
        (set-match-data
         (list start (+ start 1 (length (car resolved)))))
        (setq found t)))
    found))

(defun mevedel-skills-install-font-lock ()
  "Install font-lock support for known `$skill' mentions."
  (font-lock-add-keywords
   nil
   '((mevedel-skills--fontify-dollar-keyword
      0 font-lock-keyword-face prepend))
   t))


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
;;; Skills prompt roster and reminders

(defcustom mevedel-skills-listing-budget 0.02
  "Fraction of the context window allotted to the model-facing skills roster.

The dynamic prompt roster enumerates active, model-invocable skills so
the model can call them by name via the `Skill' tool.  This fraction of
`mevedel-compact-context-limit' (converted to characters at four chars
per token) caps the roster so it cannot crowd out the user's
conversation on long sessions."
  :type 'float
  :group 'mevedel)

(defcustom mevedel-skills-listing-max-entry-chars 1536
  "Maximum characters per skill entry in the model-facing skills roster.

Entries longer than this are truncated with an ellipsis so a single
verbose description cannot starve the rest of the listing.  The
default cap is 1,536 chars."
  :type 'integer
  :group 'mevedel)

(defconst mevedel-skills--source-priority
  '((project . mevedel)
    (project . agents)
    (user . mevedel)
    (user . agents)
    bundled
    managed
    plugin)
  "Source priority for model-facing skills roster ordering.")

(defun mevedel-skills--source-priority-key (skill)
  "Return ordering key for SKILL in model-facing rosters."
  (or (cl-position
       (cond
        ((and (mevedel-skills--ordinary-skill-p skill)
              (mevedel-skill-source-family skill))
         (cons (mevedel-skill-source skill)
               (mevedel-skill-source-family skill)))
        ((eq (mevedel-skill-source skill) 'project)
         '(project . mevedel))
        ((eq (mevedel-skill-source skill) 'user)
         '(user . mevedel))
        (t
         (mevedel-skill-source skill)))
       mevedel-skills--source-priority
       :test #'equal)
      most-positive-fixnum))

(defun mevedel-skills--listing-budget-chars ()
  "Return the character budget for the model-facing skills roster.
Derived from `mevedel-skills-listing-budget' and the compact context
limit; assumes ~4 characters per token."
  (let ((limit (or (and (boundp 'mevedel-compact-context-limit)
                        mevedel-compact-context-limit)
                   200000)))
    (max 0 (floor (* mevedel-skills-listing-budget limit 4)))))

(defun mevedel-skills--truncate-text (text limit)
  "Return TEXT truncated to LIMIT characters, using `...' when possible."
  (let ((text (or text "")))
    (cond
     ((<= limit 0) "")
     ((<= (length text) limit) text)
     ((<= limit 3) (substring text 0 limit))
     (t (concat (substring text 0 (- limit 3)) "...")))))

(defun mevedel-skills--entry-base (skill &optional dormant)
  "Return the roster line prefix for SKILL.
When DORMANT is non-nil, mark the skill as dormant path-scoped."
  (format "- %s%s:"
          (mevedel-skill-name skill)
          (if dormant " [dormant path-scoped]" "")))

(defun mevedel-skills--entry-description (skill &optional dormant)
  "Return SKILL's description capped for a single roster entry."
  (let* ((base (mevedel-skills--entry-base skill dormant))
         (limit (max 0 (- mevedel-skills-listing-max-entry-chars
                          (length base)
                          1))))
    (mevedel-skills--truncate-text
     (or (mevedel-skill-description skill) "")
     limit)))

(defun mevedel-skills--listing-describe (skill &optional dormant)
  "Return a one-line entry for SKILL.

Format:
  - name: description

`mevedel-skills-listing-max-entry-chars' (1,536 by default) caps entries by
truncation with an ellipsis so a single verbose skill cannot starve
the rest of the listing.  When DORMANT is non-nil, mark the skill as
dormant path-scoped for `ListSkills(query)' output."
  (concat (mevedel-skills--entry-base skill dormant)
          " "
          (mevedel-skills--entry-description skill dormant)))

(defun mevedel-skills--listing-candidates (session)
  "Return SESSION's model-invocable, currently active skills.

Sorted by configured resource precedence so budget pressure drops
global/bundled/plugin entries before local resource entries."
  (let ((candidates
         (cl-remove-if-not
          (lambda (s)
            (mevedel-skills--model-visible-p s t))
          (mevedel-session-skills session))))
    (cl-sort (copy-sequence candidates)
             (lambda (a b)
               (< (mevedel-skills--source-priority-key a)
                  (mevedel-skills--source-priority-key b))))))

(defun mevedel-skills--format-listing-result (skills)
  "Return structured budgeted active roster data for SKILLS.

Descriptions are shortened before names disappear.  Whole entries are
omitted only when name-only entries cannot fit in
`mevedel-skills--listing-budget-chars'.  The returned plist contains
:text and :status, where :status is nil, `truncated', or `omitted'."
  (let* ((budget (mevedel-skills--listing-budget-chars))
         (header "### Available skills")
         (full-lines (mapcar #'mevedel-skills--listing-describe skills)))
    (cl-labels
        ((body (lines &optional notes)
           (string-join
            (append (list header "")
                    lines
                    (when notes
                      (cons "" notes)))
            "\n"))
         (fits-p (text)
           (<= (length text) budget))
         (omit-note (count)
           (format "%d skills omitted; ListSkills(query)." count)))
      (let* ((full-body (body full-lines))
             (name-lines
              (mapcar (lambda (skill)
                        (concat (mevedel-skills--entry-base skill) " "))
                      skills))
             (name-body (body name-lines))
             (short-note
              "Skill descriptions were shortened; ListSkills(query).")
             (short-note-fits (fits-p (body name-lines (list short-note)))))
        (cond
         ((fits-p full-body)
          (list :text full-body :status nil))
         ((fits-p name-body)
          (let ((lines (copy-sequence name-lines))
                (used (length (body name-lines
                                    (and short-note-fits
                                         (list short-note)))))
                (index 0)
                (shortened nil))
            (dolist (skill skills)
              (let* ((desc (mevedel-skills--entry-description skill))
                     (remaining (- budget used)))
                (cond
                 ((string-empty-p desc))
                 ((<= (length desc) remaining)
                  (setf (nth index lines)
                        (concat (nth index lines) desc))
                  (cl-incf used (length desc)))
                 ((> remaining 0)
                  (setf (nth index lines)
                        (concat (nth index lines)
                                (mevedel-skills--truncate-text
                                 desc remaining)))
                  (setq used budget
                        shortened t))
                 (t
                  (setq shortened t))))
              (cl-incf index))
            (list
             :text (body lines
                         (and shortened short-note-fits
                              (list short-note)))
             :status (and shortened 'truncated))))
         (t
          (let ((lines nil)
                (omitted 0))
            (dolist (line name-lines)
              (let ((candidate (append lines (list line))))
                (if (fits-p (body candidate))
                    (setq lines candidate)
                  (cl-incf omitted))))
            (let ((note (omit-note omitted)))
              (while (and lines (not (fits-p (body lines (list note)))))
                (setq lines (butlast lines))
                (cl-incf omitted)
                (setq note (omit-note omitted)))
              (list
               :text (body lines
                           (and (fits-p (body lines (list note)))
                                (list note)))
               :status (and (> omitted 0) 'omitted)
               :omitted omitted)))))))))

(defun mevedel-skills--format-listing (skills)
  "Format SKILLS as the budgeted active roster."
  (plist-get (mevedel-skills--format-listing-result skills) :text))

(defun mevedel-skills--listing-budget-status (skills)
  "Return budget status for SKILLS, or nil when the full roster fits."
  (plist-get (mevedel-skills--format-listing-result skills) :status))

(defconst mevedel-skills--prompt-contract
  "### How to use skills
- Trigger rules: If the user names a listed skill with unquoted `$SkillName` syntax or plain text, or the task clearly matches a listed skill description, call `Skill(name=...)` before proceeding. Quoted, escaped, or Markdown-code `$SkillName` text is literal and must not trigger a skill.
- Discovery: If unsure which skill applies, call `ListSkills(query)` with a short search term.
- Coordination: Choose the minimal applicable skill set. When multiple skills apply, state the order you will use them.
- Scope: Do not carry skill decisions across turns unless re-mentioned or newly matched."
  "Concise model-facing instructions for using mevedel skills.")

(defun mevedel-skills-prompt-section (session &optional buffer)
  "Return the dynamic skills prompt section for SESSION.
BUFFER, when non-nil, is used to refresh dirty skill roots before
rendering."
  (when (and buffer (buffer-live-p buffer))
    (mevedel-skills--ensure-fresh buffer session))
  (when-let* ((skills (mevedel-skills--listing-candidates session)))
    (concat "## Skills\n"
            "A skill is a reusable prompt recipe available through the `Skill` tool. The active skills for this session are listed below by canonical invocation name.\n\n"
            (mevedel-skills--format-listing skills)
            "\n\n"
            mevedel-skills--prompt-contract)))

(defun mevedel-skills--skill-snapshot (session)
  "Return SESSION's current active model-visible skill snapshot."
  (mapcar (lambda (skill)
            (cons (mevedel-skill-name skill)
                  (or (mevedel-skill-description skill) "")))
          (mevedel-skills--listing-candidates session)))

(defun mevedel-skills--roster-budget-status (session)
  "Return SESSION's current prompt-roster budget status."
  (when-let* ((skills (mevedel-skills--listing-candidates session)))
    (mevedel-skills--listing-budget-status skills)))

(defun mevedel-skills--format-roster-budget-reminder (status)
  "Return reminder text for roster budget STATUS."
  (pcase status
    ('omitted
     "The skills roster in the system prompt omitted some active skills because of budget. Use `ListSkills(query)` to search the full enabled model-invocable skill set.")
    ('truncated
     "The skills roster in the system prompt shortened some descriptions because of budget. Use `ListSkills(query)` to search for complete skill details.")
    (_ nil)))

(defun mevedel-skills--format-delta-list (entries with-descriptions)
  "Return a capped bullet list for ENTRIES.
When WITH-DESCRIPTIONS is non-nil, include descriptions from ENTRIES."
  (mapconcat
   (lambda (entry)
     (if with-descriptions
         (format "  - %s: %s" (car entry) (cdr entry))
       (format "  - %s" (car entry))))
   (cl-subseq entries 0 (min 10 (length entries)))
   "\n"))

(defun mevedel-skills--format-delta (added removed)
  "Return a model reminder for ADDED and REMOVED skill snapshot entries."
  (let ((parts (list "Available skills changed.")))
    (when added
      (push (concat "Added skills:\n"
                    (mevedel-skills--format-delta-list added t))
            parts)
      (when (> (length added) 10)
        (push (format "and %d more; use ListSkills(query)"
                      (- (length added) 10))
              parts)))
    (when removed
      (push (concat "Removed skills:\n"
                    (mevedel-skills--format-delta-list removed nil))
            parts)
      (when (> (length removed) 10)
        (push (format "and %d more; use ListSkills(query)"
                      (- (length removed) 10))
              parts)))
    (string-join (nreverse parts) "\n\n")))

(defun mevedel-reminders-make-skills-delta ()
  "Create the `skills-delta' reminder.

The first snapshot is silent.  Later active model-visible skill changes
are reported once and become the new snapshot."
  (let (delta)
    (mevedel-reminder-create
     :type 'skills-delta
     :trigger (lambda (session)
                (setq delta nil)
                (when-let* ((buffer (mevedel-skills--current-reminder-buffer
                                     session)))
                  (mevedel-skills--ensure-fresh buffer session))
                (let* ((current (mevedel-skills--skill-snapshot session))
                       (previous (mevedel-session-skills-snapshot session)))
                  (if (eq previous :uninitialized)
                      (progn
                        (setf (mevedel-session-skills-snapshot session)
                              current)
                        nil)
                    (let ((added (cl-remove-if
                                  (lambda (entry)
                                    (assoc (car entry) previous))
                                  current))
                          (removed (cl-remove-if
                                    (lambda (entry)
                                      (assoc (car entry) current))
                                    previous)))
                      (when (or added removed)
                        (setq delta (list :added added :removed removed))
                        t)))))
     :content (lambda (session)
                (prog1
                    (mevedel-skills--format-delta
                     (plist-get delta :added)
                     (plist-get delta :removed))
                  (setf (mevedel-session-skills-snapshot session)
                        (mevedel-skills--skill-snapshot session))
                  (setq delta nil)))
     :interval nil)))

(defun mevedel-reminders-make-skills-roster-budget ()
  "Create the `skills-roster-budget' reminder.

Emits once when the active prompt roster is truncated or omits entries,
and again if that budget status changes."
  (let ((last-status :uninitialized)
        status)
    (mevedel-reminder-create
     :type 'skills-roster-budget
     :trigger (lambda (session)
                (when-let* ((buffer (mevedel-skills--current-reminder-buffer
                                     session)))
                  (mevedel-skills--ensure-fresh buffer session))
                (setq status (mevedel-skills--roster-budget-status session))
                (let ((changed (not (eq status last-status))))
                  (setq last-status status)
                  (and status changed)))
     :content (lambda (_session)
                (mevedel-skills--format-roster-budget-reminder status))
     :interval nil)))


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
                 (mevedel-skills--skill-enabled-p skill)
                 (mevedel-skill-path-patterns skill)
                 (mevedel-skills--path-matches-p
                  path (mevedel-skill-path-patterns skill)))
        (setf (mevedel-skill-active-p skill) t)
        (push skill activated)))
    (nreverse activated)))

(defun mevedel-skills--activation-reminder (path skills)
  "Return a compact reminder that SKILLS activated because of PATH."
  (let* ((names (mapcar #'mevedel-skill-name skills))
         (shown (cl-subseq names 0 (min 5 (length names))))
         (more (- (length names) (length shown))))
    (format
     "Dormant path-scoped skills became relevant after `%s`: %s%s. They are now in the active skills roster. Use `Skill(name=...)` if applicable; use `ListSkills(query)` to search."
     path
     (mapconcat #'identity shown ", ")
     (if (> more 0)
         (format ", and %d more" more)
       ""))))

(defun mevedel-skills--queue-activation-reminder (session path skills)
  "Queue a path activation reminder for SESSION and SKILLS."
  (when-let* ((visible (cl-remove-if-not
                        (lambda (skill)
                          (mevedel-skills--model-visible-p skill t))
                        skills)))
    (mevedel-session-enqueue-pending-reminder
     session (mevedel-skills--activation-reminder path visible))
    (setf (mevedel-session-skills-snapshot session)
          (mevedel-skills--skill-snapshot session))))

(defun mevedel-skills--post-tool-activate (info)
  "Post-tool-call hook: activate conditional skills based on tool path.

INFO is the plist passed by `gptel-post-tool-call-functions'.  Extracts
the touched path via the tool struct's `get-path' slot and forwards it
to `mevedel-skills--maybe-activate'."
  (when-let* ((session (or (and (boundp 'mevedel--session) mevedel--session)
                           (when-let* ((inv (mevedel-skills--current-invocation)))
                             (mevedel-agent-invocation-parent-session inv))))
              (tool-name (plist-get info :name))
              (args (plist-get info :args))
              (tool (mevedel-tool-get tool-name))
              (get-path-fn (mevedel-tool-get-path tool)))
    (when-let* ((path (ignore-errors (funcall get-path-fn args))))
      (mevedel-skills--queue-activation-reminder
       session path
       (mevedel-skills--maybe-activate session path))))
  nil)

;;;###autoload
(defun mevedel-skills-install-activation-hook ()
  "Install the buffer-local post-tool-call activation hook."
  (add-hook 'gptel-post-tool-call-functions
            #'mevedel-skills--post-tool-activate nil t))

;;
;;; Reminder installation

;;;###autoload
(defun mevedel-skills-install-reminder (session)
  "Add the skills event reminders to SESSION if not already present.
Idempotent."
  (unless (cl-find 'skills-roster-budget
                   (mevedel-session-reminders session)
                   :key #'mevedel-reminder-type)
    (mevedel-session-add-reminder
     session (mevedel-reminders-make-skills-roster-budget)))
  (unless (cl-find 'skills-delta
                   (mevedel-session-reminders session)
                   :key #'mevedel-reminder-type)
    (mevedel-session-add-reminder
     session (mevedel-reminders-make-skills-delta)))
  session)


(provide 'mevedel-skills)

;;; mevedel-skills.el ends here
