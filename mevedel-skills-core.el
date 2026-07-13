;;; mevedel-skills-core.el --- Skill model, discovery, and reload -*- lexical-binding: t -*-

;;; Commentary:

;; Owns the canonical skill model, frontmatter validation, discovery and source
;; precedence, persisted enablement, session installation, lazy body loading,
;; path-scoped activation state, and modification detection.

;;; Code:

(require 'cl-lib)
(require 'mevedel-structs)
(require 'mevedel-tool-registry)

;; `filenotify'
(declare-function file-notify-add-watch "filenotify"
                  (file flags callback))
(declare-function file-notify-rm-watch "filenotify" (descriptor))
(declare-function file-notify-valid-p "filenotify" (descriptor))

;; `gptel-agent'
(declare-function gptel-agent-parse-markdown-frontmatter
                  "gptel-agent"
                  (file-path &optional validator templates metadata-only))
(declare-function gptel-agent-read-file
                  "gptel-agent" (agent-file &optional templates metadata-only))

;; `mevedel-hooks'
(declare-function mevedel-hooks-normalize-rules
                  "mevedel-hooks" (rules &optional scope))

;; `mevedel-permissions'
(declare-function mevedel-permission--parse-rule-string
                  "mevedel-permissions" (entry))

;; `mevedel-plugins'
(declare-function mevedel-plugins-skill-dirs "mevedel-plugins"
                  (&optional workspace))

;; `subr'
(defvar read-eval)


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
  a request-owning invocation.  EFFORT selects its reasoning effort.
ARGUMENT-HINT annotates completion UI.  ARGUMENT-NAMES holds the
parsed `arguments' frontmatter as a list of names with numeric-only
entries filtered out.  PATH-PATTERNS contains globs that trigger
conditional activation.  SHELL is the shell symbol for body shell
expansion (`bash' default, `powershell' parsed but unsupported).
HOOKS is the normalized frontmatter `hooks' value.
WARNINGS holds configuration diagnostics exposed by skill inspection.
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
  warnings
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
      (condition-case err
          (with-temp-buffer
            (let ((read-eval nil))
              (insert-file-contents file)
              (let ((state (read (current-buffer))))
                (unless (and (proper-list-p state)
                             (= (length state) 2)
                             (eq (car state) :disabled-keys)
                             (listp (cadr state))
                             (cl-every #'stringp (cadr state)))
                  (error "Unsupported skill state format"))
                state)))
        (error
         (error "Could not read skill state %s: %s"
                file (error-message-string err)))))))

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

(defun mevedel-skills--disabled-keys ()
  "Return persisted disabled stable skill keys."
  (plist-get (mevedel-skills--read-state) :disabled-keys))

(defun mevedel-skills--source-key (source-file)
  "Return the stable identity key for SOURCE-FILE."
  (when source-file
    (concat "file:" (or (ignore-errors (file-truename source-file))
                        (expand-file-name source-file)))))

(defun mevedel-skills--state-key (skill)
  "Return stable persisted state key for SKILL."
  (and (mevedel-skill-p skill)
       (mevedel-skills--source-key (mevedel-skill-source-file skill))))

(defun mevedel-skills--set-enabled (skill enabled)
  "Persist file-backed SKILL as enabled or disabled according to ENABLED."
  (unless (mevedel-skill-p skill)
    (user-error "Loaded skill is required"))
  (let ((key (mevedel-skills--state-key skill)))
    (unless key
      (user-error "Skill has no stable source file: %s"
                  (mevedel-skill-name skill)))
    (let* ((state (or (mevedel-skills--read-state)
                      '(:disabled-keys nil)))
           (disabled-keys
            (cl-remove key (plist-get state :disabled-keys) :test #'equal)))
      (unless enabled
        (push key disabled-keys))
      (setf (plist-get state :disabled-keys)
            (sort (delete-dups disabled-keys) #'string<))
      (mevedel-skills--write-state state))))

(defun mevedel-skills--skill-enabled-p (skill)
  "Return non-nil when SKILL is not user-disabled."
  (let ((key (mevedel-skills--state-key skill)))
    (not (and key (member key (mevedel-skills--disabled-keys))))))


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

(defun mevedel-skills--parse-allowed-tool-rules (entries source-file)
  "Map each ENTRY through `mevedel-permission--parse-rule-string'.

ENTRIES is the raw `allowed-tools' frontmatter list.  Returns a
list of parsed mevedel permission rules.  A malformed entry aborts the whole
skill load: this function signals `user-error' on the first bad
entry, and `mevedel-skills--build-skill''s `condition-case' skips
  the offending skill with a warning naming SOURCE-FILE."
  (when entries
    (require 'mevedel-permissions))
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
  (require 'gptel-agent)
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
         (hooks (plist-get plist :hooks))
         (warnings
          (when (and (eq context 'inline)
                     (stringp agent)
                     (not (string-blank-p agent)))
            (list
             (format "Agent %s is ignored for inline skills; use context: fork to select an agent"
                     agent)))))
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
     ;; gptel-agent normalizes Markdown `model' values to symbols.
     :model (and model
                 (cond
                  ((stringp model) model)
                  ((symbolp model) (symbol-name model))))
     :effort (if (stringp effort) (intern effort) effort)
     :argument-hint (and (stringp argument-hint) argument-hint)
     :argument-names (mevedel-skills--parse-argument-names arguments)
     :path-patterns (mevedel-skills--coerce-list paths)
     :shell (mevedel-skills--validate-shell shell source-file)
     :hooks (mevedel-skills--normalize-hooks
             hooks (and (eq context 'fork) 'skill-fork))
     :warnings warnings
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
  (require 'gptel-agent)
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
  (require 'gptel-agent)
  (or (mevedel-skill-body skill)
      (when-let* ((file (mevedel-skill-source-file skill))
                  (parsed (ignore-errors (gptel-agent-read-file file)))
                  (body (plist-get (cdr parsed) :system))
                  ((not (string-blank-p body))))
        (setf (mevedel-skill-body skill) body)
        body)))


;;
;;; Session installation

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

(defun mevedel-session-get-skill-by-source (session source-file)
  "Return SESSION skill discovered from exact SOURCE-FILE, or nil.
No name fallback is attempted."
  (when (buffer-live-p (current-buffer))
    (mevedel-skills--ensure-fresh (current-buffer) session))
  (when-let* ((key (mevedel-skills--source-key source-file)))
    (cl-find key (mevedel-session-skills session)
             :key #'mevedel-skills--state-key :test #'equal)))


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

(provide 'mevedel-skills-core)

;;; mevedel-skills-core.el ends here
