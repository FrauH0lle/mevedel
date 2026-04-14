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
(declare-function mevedel-tools--task "mevedel-tool-ui"
                  (main-cb agent-type description prompt))

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
(defvar gptel-prompt-prefix-alist)
(defvar gptel-model)

;; `mevedel-permissions'
(defvar mevedel-permission-mode)


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

(defvar mevedel-skills--include-bundled t
  "When non-nil, `mevedel-skills-scan' also scans the bundled directory.
Let-bound to nil by tests that assert on exact user-skill contents
without the repo's own coordinator skill leaking in.")

(defconst mevedel-skills--bundled-dir
  (expand-file-name "skills/" mevedel-tool-registry--source-dir)
  "Absolute path to mevedel's bundled skills directory.
Scanned last by `mevedel-skills-scan' so user-provided skills with
the same name take precedence.")


;;
;;; Skill struct

(cl-defstruct (mevedel-skill (:constructor mevedel-skill--create))
  "A single skill loaded from a SKILL.md file.

NAME is the skill identifier the user types after `/'.  DESCRIPTION is
the short line shown in listings and sent to the model for matching.
WHEN-TO-USE is an optional longer trigger description.  BODY is the
skill prompt text; populated lazily on first invocation.  SOURCE-FILE
and SOURCE-DIR point at the SKILL.md and its containing directory.
SOURCE is a symbol tagging the origin (`user', `project', `managed',
`plugin', `bundled').  USER-INVOCABLE-P and MODEL-INVOCABLE-P gate the
`/name' menu and the skills listing reminder respectively.  CONTEXT is
`inline' (default) or `fork' -- fork skills dispatch via the Agent tool.
AGENT names the agent type for fork execution.  ALLOWED-TOOLS filters
the active tool set during skill execution.  MODEL overrides the gptel
model.  ARGUMENT-HINT annotates completion UI.  PATH-PATTERNS contains
globs that trigger conditional activation.  HOOKS is the raw ccs hooks
plist (parsed, not executed).  ACTIVE-P records the current activation
state for conditional skills."
  name
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
  model
  argument-hint
  path-patterns
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

(defun mevedel-skills--from-plist (name plist source-file source)
  "Build a `mevedel-skill' from NAME and PLIST parsed from SOURCE-FILE.
SOURCE is the origin tag symbol."
  (let* ((description (plist-get plist :description))
         (when-to-use (plist-get plist :when-to-use))
         (disable-model (plist-get plist :disable-model-invocation))
         (user-invocable (plist-get plist :user-invocable))
         (context (plist-get plist :context))
         (allowed-tools (plist-get plist :allowed-tools))
         (agent (plist-get plist :agent))
         (model (plist-get plist :model))
         (argument-hint (plist-get plist :argument-hint))
         (paths (plist-get plist :paths))
         (hooks (plist-get plist :hooks)))
    (mevedel-skill--create
     :name name
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
     :model (and (stringp model) model)
     :argument-hint (and (stringp argument-hint) argument-hint)
     :path-patterns (mevedel-skills--coerce-list paths)
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

(defun mevedel-skills--read-metadata (file)
  "Return the frontmatter plist parsed from SKILL.md at FILE.

Calls `gptel-agent-read-file' in metadata-only mode so the heavier
`:system' slot is not populated during discovery."
  (when (and (file-readable-p file) (file-regular-p file))
    (ignore-errors (gptel-agent-read-file file nil t))))

(defun mevedel-skills--scan-dir (dir source)
  "Return a list of `mevedel-skill' structs found under DIR.
SOURCE is the origin tag applied to every skill scanned from DIR."
  (when (file-directory-p dir)
    (let (result)
      (dolist (skill-file (directory-files-recursively
                           dir "\\`SKILL\\.md\\'" nil nil t))
        (pcase-let* ((`(,name . ,plist)
                      (mevedel-skills--read-metadata skill-file)))
          (when (and name (plist-get plist :description))
            (push (mevedel-skills--from-plist name plist skill-file source)
                  result))))
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
    (when (and mevedel-skills--include-bundled
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
;;; Argument tokenization

(defun mevedel-skills--split-arguments (arguments)
  "Return ARGUMENTS split into whitespace-separated tokens.
Returns nil when ARGUMENTS is nil or blank."
  (and arguments
       (not (string-blank-p arguments))
       (split-string arguments nil t)))


;;
;;; Variable substitution

(defun mevedel-skills--substitute-vars (text arguments session skill)
  "Return TEXT with skill placeholders expanded.

Supported placeholders:
- $ARGUMENTS              -- the full argument string (empty when nil)
- $ARGUMENTS[N] / $N      -- the Nth positional token, 1-indexed
- ${CLAUDE_SESSION_ID}    -- the session name
- ${CLAUDE_SKILL_DIR}     -- the directory holding SKILL.md"
  (let* ((tokens (mevedel-skills--split-arguments arguments))
         (full (or arguments ""))
         (session-id (and session (mevedel-session-name session)))
         (skill-dir (and skill (mevedel-skill-source-dir skill)))
         (pick (lambda (n)
                 (or (nth (1- n) tokens) "")))
         (result text))
    (setq result (replace-regexp-in-string
                  (regexp-quote "${CLAUDE_SESSION_ID}")
                  (or session-id "") result t t))
    (setq result (replace-regexp-in-string
                  (regexp-quote "${CLAUDE_SKILL_DIR}")
                  (or skill-dir "") result t t))
    (setq result (replace-regexp-in-string
                  "\\$ARGUMENTS\\[\\([0-9]+\\)\\]"
                  (lambda (m)
                    (funcall pick (string-to-number (match-string 1 m))))
                  result t))
    (setq result (replace-regexp-in-string
                  "\\$ARGUMENTS\\b" full result t t))
    (setq result (replace-regexp-in-string
                  "\\$\\([1-9][0-9]*\\)"
                  (lambda (m)
                    (funcall pick (string-to-number (match-string 1 m))))
                  result t))
    result))


;;
;;; Shell injection

(defun mevedel-skills--run-command (command)
  "Execute COMMAND via the shell and return its stdout.
Stdout is right-trimmed.  Non-zero exit codes are prefixed with
`[exit N] ' so the LLM sees the failure in context."
  (with-temp-buffer
    (let ((exit (call-process-shell-command command nil t nil)))
      (let ((out (string-trim-right (buffer-string))))
        (if (zerop exit)
            out
          (format "[exit %d] %s" exit out))))))

(defun mevedel-skills--run-shell-injections (text)
  "Return TEXT with shell-injection markers replaced by command output.

Supported markers:
- !`COMMAND`          inline: run COMMAND, substitute stdout
- ```!\\nSCRIPT\\n``` fenced block: run SCRIPT as a shell script"
  (let ((result text))
    (setq result
          (replace-regexp-in-string
           "^```!\n\\(\\(?:.\\|\n\\)*?\\)\n```$"
           (lambda (m)
             (mevedel-skills--run-command (match-string 1 m)))
           result t t))
    (setq result
          (replace-regexp-in-string
           "!`\\([^`\n]*\\)`"
           (lambda (m)
             (mevedel-skills--run-command (match-string 1 m)))
           result t t))
    result))


;;
;;; Body preparation

(defun mevedel-skills--prepare-body (skill arguments session)
  "Return SKILL's body with variables and shell injections expanded.
ARGUMENTS is the raw argument string passed to the skill (or nil).
SESSION supplies the `CLAUDE_SESSION_ID' substitution.  Returns nil
when the skill has no body."
  (when-let* ((body (mevedel-skill-load-body skill)))
    (mevedel-skills--run-shell-injections
     (mevedel-skills--substitute-vars body arguments session skill))))


;;
;;; Execution dispatchers

(defun mevedel-skills--execute-inline (skill arguments callback session)
  "Invoke SKILL inline: send its prepared body to CALLBACK.
ARGUMENTS is the raw argument string.  SESSION is the current session."
  (let ((body (mevedel-skills--prepare-body skill arguments session)))
    (funcall callback
             (or body
                 (format "Skill '%s' has no body."
                         (mevedel-skill-name skill))))))

(defun mevedel-skills--execute-fork (skill arguments callback session)
  "Invoke SKILL by dispatching a sub-agent via `mevedel-tools--task'.
The skill's `agent' slot (default `general-purpose') names the agent
type.  The skill name is used as the task description."
  (let* ((prompt (or (mevedel-skills--prepare-body skill arguments session) ""))
         (agent-type (or (mevedel-skill-agent skill) "general-purpose"))
         (description (mevedel-skill-name skill)))
    (mevedel-tools--task callback agent-type description prompt)))


;;
;;; Skill tool handler

(defun mevedel-skills--invoke-handler (callback args)
  "Pipeline handler for the `Skill' tool.
CALLBACK is the async tool callback.  ARGS is a plist with :name and
optional :arguments."
  (let* ((name (plist-get args :name))
         (arguments (plist-get args :arguments))
         (session mevedel--session)
         (skill (and session (mevedel-session-get-skill session name))))
    (cond
     ((not (stringp name))
      (funcall callback "Error: skill name is required."))
     ((not session)
      (funcall callback "Error: no active mevedel session."))
     ((not skill)
      (funcall callback (format "Error: unknown skill '%s'." name)))
     (t
      (pcase (mevedel-skill-context skill)
        ('fork  (mevedel-skills--execute-fork skill arguments callback session))
        (_      (mevedel-skills--execute-inline skill arguments callback session)))))))


;;
;;; Tool registration

;;;###autoload
(defun mevedel-skills--register ()
  "Register the `Skill' tool with the mevedel tool registry."
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
Recognized modes: default, accept-edits, plan, trust-all."
  (if (and args (not (string-blank-p args)))
      (let ((mode (intern (string-trim args))))
        (unless (memq mode '(default accept-edits plan trust-all))
          (user-error "Unknown permission mode: %s" mode))
        (setq-local mevedel-permission-mode mode)
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
NAME is the command name, ARGS is the remainder of the first line
trimmed, and OFFSET is the 0-based character position of `/' within
TEXT.  Additional lines in TEXT are ignored."
  (let* ((trimmed (string-trim-left text))
         (offset (- (length text) (length trimmed))))
    (when (and (> (length trimmed) 1) (eq (aref trimmed 0) ?/))
      (let* ((line-end (or (string-match "\n" trimmed) (length trimmed)))
             (line (substring trimmed 1 line-end))
             (space (string-match "[ \t]" line))
             (name (if space (substring line 0 space) line))
             (args (if space (string-trim (substring line space)) "")))
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

(defun mevedel-skills--dispatch-slash-command ()
  "Parse and dispatch a `/command' in the current chat buffer.

Returns:
- `local'   a local command ran; caller should abort the send.
- `skill'   a skill was expanded into the prompt region; caller
            should proceed with the send.
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
        (let ((body (mevedel-skills--prepare-body
                     skill args mevedel--session)))
          (delete-region delete-start (cdr region))
          (unless after-prefix
            (mevedel-skills--ensure-fresh-line))
          (insert (or body
                      (format "Skill '%s' has no body."
                              (mevedel-skill-name skill))))
          'skill))
       (t
        (message "Unknown slash command: /%s" name)
        'unknown)))))

(defun mevedel-skills--gptel-send-advice (&rest _args)
  "`:before-while' advice on `gptel-send' for slash-command dispatch.
Returns non-nil to let `gptel-send' proceed, nil to abort it."
  (if (bound-and-true-p mevedel--session)
      (pcase (mevedel-skills--dispatch-slash-command)
        ('local nil)
        ('unknown nil)
        (_ t))
    t))


;;
;;; Completion at point

(defun mevedel-slash-capf ()
  "Completion-at-point for `/command' and `/skill-name' prefixes.
Active only in a mevedel chat buffer when point sits just after a
`/' that begins its line (after an optional prompt prefix)."
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
           (skill-names
            (mapcar #'mevedel-skill-name
                    (mevedel-session-skills mevedel--session)))
           (local-names (mapcar #'car mevedel-slash-commands))
           (candidates (append local-names skill-names)))
      (list start end candidates
            :exclusive 'no
            :annotation-function
            (lambda (name)
              (if (assoc name mevedel-slash-commands)
                  " [command]"
                " [skill]"))))))


;;
;;; Installation

;;;###autoload
(defun mevedel-skills-install-slash-commands ()
  "Install the slash-command advice on `gptel-send'."
  (advice-add 'gptel-send :before-while
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

(defcustom mevedel-skills-listing-max-entry-chars 250
  "Maximum characters per skill entry in the skills-listing reminder.

Entries longer than this are truncated with an ellipsis so a single
verbose description cannot starve the rest of the listing."
  :type 'integer
  :group 'mevedel)

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
Capped at `mevedel-skills-listing-max-entry-chars'."
  (let* ((max-chars mevedel-skills-listing-max-entry-chars)
         (name (mevedel-skill-name skill))
         (desc (or (mevedel-skill-description skill) ""))
         (line (format "- %s: %s" name desc)))
    (if (> (length line) max-chars)
        (concat (substring line 0 (max 1 (- max-chars 3))) "...")
      line)))

(defun mevedel-skills--listing-candidates (session)
  "Return SESSION's model-invocable, currently active skills."
  (cl-remove-if-not
   (lambda (s)
     (and (mevedel-skill-model-invocable-p s)
          (mevedel-skill-active-p s)))
   (mevedel-session-skills session)))

(defun mevedel-skills--format-listing (skills)
  "Format SKILLS as the body of the skills-listing reminder.
Budget-capped at `mevedel-skills--listing-budget-chars'; entries past
the budget are dropped."
  (let* ((budget (mevedel-skills--listing-budget-chars))
         (header "The following skills are available for use with the Skill tool:")
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
    (concat header "\n\n"
            (mapconcat #'identity (nreverse lines) "\n"))))

(defun mevedel-reminders-make-skills-listing ()
  "Create the `skills-listing' reminder.

Fires every turn the session has at least one model-invocable, active
skill.  The listing enumerates those skills so the model can call them
via the `Skill' tool, and is budget-capped by
`mevedel-skills-listing-budget'."
  (mevedel-reminder-create
   :type 'skills-listing
   :trigger (lambda (session)
              (and (mevedel-skills--listing-candidates session) t))
   :content (lambda (session)
              (mevedel-skills--format-listing
               (mevedel-skills--listing-candidates session)))
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
