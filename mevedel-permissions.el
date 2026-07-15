;;; mevedel-permissions.el -- Unified permission system -*- lexical-binding: t -*-

;;; Commentary:

;; Unified permission decision function for all mevedel tools.  Replaces
;; scattered per-tool permission checks with one decision chain: extract
;; context -> absolute denies -> tool policy -> permission rules -> mode ->
;; independent filesystem resource authority -> mode/default ask.

;;; Code:

(eval-when-compile (require 'cl-lib))

;; `mevedel-agents'
(declare-function mevedel-agent-invocation-parent-session
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-skill-permission-rules
                  "mevedel-agents" (cl-x) t)
(defvar mevedel--agent-invocation)

;; `mevedel-goal'
(declare-function mevedel-goal-read-only-phase-p
                  "mevedel-goal" (&optional session))

;; `mevedel-reminders'
(declare-function mevedel-session-ensure-reminder
                  "mevedel-reminders" (session reminder))
(declare-function mevedel-session-remove-reminder
                  "mevedel-reminders" (session type))
(declare-function mevedel-reminders-make-full-auto-mode
                  "mevedel-reminders" ())
(declare-function mevedel-reminders-make-full-auto-mode-exit
                  "mevedel-reminders" ())

;; `mevedel-skills-ui'
(declare-function mevedel-skills--refresh-view-input-prompt
                  "mevedel-skills-ui" ())

;; `mevedel-structs'
(declare-function mevedel-request-skill-permission-rules
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-active-dropped-file-grants
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-permission-mode "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-permission-rules "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-resource-grants "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-workspace "mevedel-structs" (cl-x) t)
(declare-function mevedel-workspace-root "mevedel-structs" (cl-x) t)
(declare-function mevedel-workspace-state-dir "mevedel-structs" (workspace))
(defvar mevedel--session)
(defvar mevedel--view-buffer)
(defvar mevedel-user-dir)

;; setf expander for session struct
(eval-when-compile
  (require 'mevedel-structs))

;; `mevedel-tool-registry'
(declare-function mevedel-tool-check-permission "mevedel-tool-registry" (cl-x) t)
(declare-function mevedel-tool-check-permission-async "mevedel-tool-registry" (cl-x) t)
(declare-function mevedel-tool-ensure "mevedel-tool-registry" (name))
(declare-function mevedel-tool-get "mevedel-tool-registry" (name &optional category))
(declare-function mevedel-tool-get-domain "mevedel-tool-registry" (cl-x) t)
(declare-function mevedel-tool-get-name "mevedel-tool-registry" (cl-x) t)
(declare-function mevedel-tool-get-path "mevedel-tool-registry" (cl-x) t)
(declare-function mevedel-tool-get-pattern "mevedel-tool-registry" (cl-x) t)
(declare-function mevedel-tool-groups "mevedel-tool-registry" (cl-x) t)
(declare-function mevedel-tool-name "mevedel-tool-registry" (cl-x) t)
(declare-function mevedel-tool-read-only-p "mevedel-tool-registry" (cl-x) t)

;; `mevedel-workspace'
(declare-function mevedel--all-allowed-roots
                  "mevedel-workspace" (&optional buffer))


;;
;;; Customization

(defcustom mevedel-permission-rules
  '(;; File inspection
    ("Bash" :pattern "ls"         :action allow)
    ("Bash" :pattern "ls *"       :action allow)
    ("Bash" :pattern "cat *"      :action allow)
    ("Bash" :pattern "head"       :action allow)
    ("Bash" :pattern "head *"     :action allow)
    ("Bash" :pattern "tail"       :action allow)
    ("Bash" :pattern "tail *"     :action allow)
    ("Bash" :pattern "less *"     :action allow)
    ("Bash" :pattern "more *"     :action allow)
    ("Bash" :pattern "file *"     :action allow)
    ("Bash" :pattern "stat *"     :action allow)
    ("Bash" :pattern "wc"         :action allow)
    ("Bash" :pattern "wc *"       :action allow)
    ("Bash" :pattern "du"         :action allow)
    ("Bash" :pattern "du *"       :action allow)
    ("Bash" :pattern "df"         :action allow)
    ("Bash" :pattern "df *"       :action allow)

    ;; Directory operations
    ("Bash" :pattern "pwd"        :action allow)
    ("Bash" :pattern "cd"         :action allow)
    ("Bash" :pattern "cd *"       :action allow)

    ;; Text processing
    ("Bash" :pattern "grep *"     :action allow)
    ("Bash" :pattern "egrep *"    :action allow)
    ("Bash" :pattern "fgrep *"    :action allow)
    ("Bash" :pattern "rg *"       :action allow)
    ("Bash" :pattern "ag *"       :action allow)
    ("Bash" :pattern "awk *"      :action allow)
    ("Bash" :pattern "cut *"      :action allow)
    ("Bash" :pattern "sort"       :action allow)
    ("Bash" :pattern "sort *"     :action allow)
    ("Bash" :pattern "uniq"       :action allow)
    ("Bash" :pattern "uniq *"     :action allow)
    ("Bash" :pattern "tr *"       :action allow)
    ("Bash" :pattern "diff *"     :action allow)

    ;; File search
    ("Bash" :pattern "find *"     :action allow)
    ("Bash" :pattern "which *"    :action allow)
    ("Bash" :pattern "whereis *"  :action allow)
    ("Bash" :pattern "type *"     :action allow)

    ;; Version control (read operations)
    ("Bash" :pattern "git status"         :action allow)
    ("Bash" :pattern "git status *"       :action allow)
    ("Bash" :pattern "git log"            :action allow)
    ("Bash" :pattern "git log *"          :action allow)
    ("Bash" :pattern "git diff"           :action allow)
    ("Bash" :pattern "git diff *"         :action allow)
    ("Bash" :pattern "git show"           :action allow)
    ("Bash" :pattern "git show *"         :action allow)
    ("Bash" :pattern "git branch"         :action allow)
    ("Bash" :pattern "git branch *"       :action allow)
    ("Bash" :pattern "git tag"            :action allow)
    ("Bash" :pattern "git tag *"          :action allow)
    ("Bash" :pattern "git remote"         :action allow)
    ("Bash" :pattern "git remote *"       :action allow)
    ("Bash" :pattern "git ls-files"       :action allow)
    ("Bash" :pattern "git ls-files *"     :action allow)
    ("Bash" :pattern "git config --get *" :action allow)
    ("Bash" :pattern "git config --list"  :action allow)
    ("Bash" :pattern "git config --list *" :action allow)

    ;; Process inspection
    ("Bash" :pattern "ps"         :action allow)
    ("Bash" :pattern "ps *"       :action allow)
    ("Bash" :pattern "pgrep *"    :action allow)

    ;; System information
    ("Bash" :pattern "uname"      :action allow)
    ("Bash" :pattern "uname *"    :action allow)
    ("Bash" :pattern "hostname"   :action allow)
    ("Bash" :pattern "hostname *" :action allow)
    ("Bash" :pattern "whoami"     :action allow)
    ("Bash" :pattern "id"         :action allow)
    ("Bash" :pattern "id *"       :action allow)
    ("Bash" :pattern "date"       :action allow)
    ("Bash" :pattern "date *"     :action allow)
    ("Bash" :pattern "uptime"     :action allow)
    ("Bash" :pattern "uptime *"   :action allow)
    ("Bash" :pattern "printenv"   :action allow)
    ("Bash" :pattern "printenv *" :action allow)

    ;; Echo / output
    ("Bash" :pattern "echo"       :action allow)
    ("Bash" :pattern "echo *"     :action allow)
    ("Bash" :pattern "printf"     :action allow)
    ("Bash" :pattern "printf *"   :action allow))
  "Permission rules for tools.

Each entry is a list: (TOOL-NAME &key SPECIFIER VALUE :action ACTION)

TOOL-NAME is a string matching a tool name (e.g., \"Read\", \"Edit\"),
or \"*\" to match all tools.

SPECIFIER is optional and selects what aspect of the invocation the
rule matches against.  At most one specifier is allowed per rule:

  :path    GLOB  - filesystem path (supports *, **, ?, ~)
                   Used by Read, Edit, Write, Glob, Grep, MkDir, Bash
                   when it resolves a bare path.
  :pattern GLOB  - command string (supports *, plus Bash-style PREFIX:*)
                   Used by Bash (e.g., \"ls *\", \"git log:*\").
  :domain  GLOB  - host name (supports *)
                   Used by WebFetch, WebSearch, YouTube.
  :name    GLOB  - match name (supports *)
                   Used by Agent (subagent_type).

Rules without a specifier match the tool regardless of context.

ACTION is one of: `allow', `deny', or `ask'.

Precedence within matching rules:
  1. Specifier-carrying rules > unqualified (generic) rules.
  2. Within each group: deny > ask > allow.

Example:
  ((\"Read\" :action allow)
   (\"Edit\" :path \"~/projects/**\" :action allow)
   (\"Write\" :path \"~/.ssh/**\" :action deny)
   (\"Bash\" :pattern \"ls *\" :action allow)
   (\"Bash\" :pattern \"git log:*\" :action allow)
   (\"Bash\" :pattern \"rm *\" :action deny)
   (\"WebFetch\" :domain \"*.example.com\" :action allow)
   (\"Agent\" :name \"explorer\" :action allow))"
  :type '(repeat sexp)
  :group 'mevedel)

(defcustom mevedel-protected-paths
  '("**/.git/**" "~/.ssh/**" "~/.gnupg/**")
  "Path patterns that require exact resource authority.

Even `full-auto' mode prompts when a matching path lacks an exact resource
grant.  Each entry is a glob pattern matched against the full expanded path."
  :type '(repeat string)
  :group 'mevedel)

(defun mevedel-permission--current-data-buffer ()
  "Return the session data buffer reachable from `current-buffer', or nil.
The current buffer itself qualifies when it carries a live
`mevedel--session'; otherwise follow its `mevedel--data-buffer'
back-pointer (set on view buffers and derived buffers) to find the
authoritative data buffer.  Returns nil for any buffer not tied to a
session -- Customize UI, `*scratch*', init-file load, etc."
  (let ((cur (current-buffer)))
    (cond
     ((and (boundp 'mevedel--session)
           (buffer-local-value 'mevedel--session cur))
      cur)
     ((let ((db (and (boundp 'mevedel--data-buffer)
                     (buffer-local-value 'mevedel--data-buffer cur))))
        (and db (buffer-live-p db)
             (boundp 'mevedel--session)
             (buffer-local-value 'mevedel--session db)
             db))))))

(defvar mevedel-permission-mode--raw-set nil
  "Non-nil while setting permission mode without transition lifecycle.")

(defun mevedel-permission-mode-normalize (mode)
  "Return canonical permission MODE.
Only values valid in configuration and persisted state are accepted."
  (let ((mode (cond
               ((symbolp mode) mode)
               ((stringp mode) (intern (string-trim mode)))
               (t mode))))
    (if (memq mode '(ask auto full-auto))
        mode
      (user-error "Unknown permission mode: %s" mode))))

(defun mevedel-permission-mode-parse-user-input (mode)
  "Return canonical permission MODE from user-facing input.
The `edit' alias names `auto' only at this interactive boundary."
  (let ((mode (cond
               ((symbolp mode) mode)
               ((stringp mode) (intern (string-trim mode)))
               (t mode))))
    (mevedel-permission-mode-normalize
     (if (eq mode 'edit) 'auto mode))))

(defun mevedel-permission-mode-set-raw (mode)
  "Set permission MODE in the current scope without transition lifecycle.
This is for lifecycle helpers that already know which mode side effects
they are responsible for."
  (let ((mode (mevedel-permission-mode-normalize mode))
        (mevedel-permission-mode--raw-set t))
    (setopt mevedel-permission-mode mode)
    mode))

(defun mevedel-permission-mode--effective-session-mode (session)
  "Return SESSION's effective permission mode."
  (or (mevedel-session-permission-mode session)
      (and (boundp 'mevedel-permission-mode) mevedel-permission-mode)
      'ask))

(defun mevedel-permission-mode-effective
    (&optional session data-buffer surface-buffer)
  "Return the effective permission mode for SESSION and DATA-BUFFER.
SURFACE-BUFFER is the UI buffer whose local mode may override the data
buffer's fallback mode.  When omitted, DATA-BUFFER is used; without a
DATA-BUFFER, the current buffer is used."
  (let* ((surface-buffer (or surface-buffer
                             (and (buffer-live-p data-buffer) data-buffer)
                             (current-buffer)))
         (surface-mode
          (and (buffer-live-p surface-buffer)
               (buffer-local-value 'mevedel-permission-mode
                                   surface-buffer)))
         (surface-local
          (and (buffer-live-p surface-buffer)
               (local-variable-p 'mevedel-permission-mode surface-buffer)))
         (global-mode (and (boundp 'mevedel-permission-mode)
                           (default-toplevel-value
                            'mevedel-permission-mode))))
    (or (and session (mevedel-session-permission-mode session))
        (and (buffer-live-p data-buffer)
             (with-current-buffer data-buffer
               (and (boundp 'mevedel--session)
                    mevedel--session
                    (mevedel-session-permission-mode mevedel--session))))
        (and surface-local surface-mode)
        (and (buffer-live-p data-buffer)
             (with-current-buffer data-buffer
               (and (boundp 'mevedel-permission-mode)
                    mevedel-permission-mode)))
        global-mode
        'ask)))

(defun mevedel-permission-mode-label (&optional mode)
  "Return the compact user-facing label for permission MODE."
  (symbol-name (if (memq mode '(ask auto full-auto)) mode 'ask)))

(defun mevedel-permission-mode-apply-full-auto-lifecycle
    (previous-mode target-mode &optional session)
  "Synchronize full-auto reminders for PREVIOUS-MODE -> TARGET-MODE.
SESSION defaults to the current data buffer's session."
  (let* ((previous-mode (mevedel-permission-mode-normalize previous-mode))
         (target-mode (mevedel-permission-mode-normalize target-mode))
         (data-buf (mevedel-permission--current-data-buffer))
         (session (or session
                      (and data-buf
                           (buffer-local-value 'mevedel--session data-buf)))))
    (when session
      (require 'mevedel-reminders)
      (cond
       ((eq target-mode 'full-auto)
        (mevedel-session-remove-reminder session 'full-auto-mode-exit)
        (mevedel-session-ensure-reminder
         session (mevedel-reminders-make-full-auto-mode)))
       (t
        (mevedel-session-remove-reminder session 'full-auto-mode)
        (when (eq previous-mode 'full-auto)
          (mevedel-session-ensure-reminder
           session (mevedel-reminders-make-full-auto-mode-exit))))))))

(defun mevedel-permission-mode-transition (mode)
  "Transition the current session to permission MODE.
Runs mode-specific lifecycle hooks."
  (let* ((target (mevedel-permission-mode-normalize mode))
         (data-buf (mevedel-permission--current-data-buffer))
         (session (and data-buf
                       (buffer-local-value 'mevedel--session data-buf))))
    (if (not session)
        (set-default-toplevel-value 'mevedel-permission-mode target)
      (with-current-buffer data-buf
        (let ((previous (mevedel-permission-mode--effective-session-mode
                         session)))
          (mevedel-permission-mode-set-raw target)
          (mevedel-permission-mode-apply-full-auto-lifecycle
           previous target session)
          (when (fboundp 'mevedel-skills--refresh-view-input-prompt)
            (mevedel-skills--refresh-view-input-prompt)))))
    target))

(defun mevedel-permission--set-session-scoped (sym val slot-setter)
  "Scoped `:set' helper for session-backed customizations.

Generic setter body for a defcustom that shadows a `mevedel-session'
slot: when the change is made from inside a session, the session slot is
updated and the defcustom's global default is left alone; when made from
anywhere else, the global default is updated so subsequent sessions pick
it up.

SYM is the defcustom symbol.  VAL is the new value.  SLOT-SETTER is a
function `(SESSION VAL) -> _' that writes VAL into the appropriate
session struct slot via `setf'; it is the only per-variable knob, making
this helper reusable across any session-backed setting.

Scope resolution:
  - `current-buffer' carries a session (data buffer), or its
    `mevedel--data-buffer' back-pointer reaches one (view buffer): only
    that session is touched -- SLOT-SETTER updates the slot, SYM is set
    buffer-locally in the data buffer and its view buffer so
    `describe-variable' reports the same value in either buffer.  Other
    sessions and the global default remain unchanged.
  - Otherwise (Customize UI, `use-package :custom', `setopt' from a
    non-session buffer): `set-default-toplevel-value' installs the new
    default for future sessions; no sessions are touched.

Fires on `setopt', `customize-set-variable', `custom-set-variables',
`use-package :custom', and the Customize UI.  Plain `setq' and
`setq-local' bypass this setter entirely."
  (let* ((data-buf (mevedel-permission--current-data-buffer))
         (session (and data-buf
                       (buffer-local-value 'mevedel--session data-buf))))
    (if session
        (progn
          (funcall slot-setter session val)
          (with-current-buffer data-buf
            (set (make-local-variable sym) val))
          (when-let* ((vb (buffer-local-value 'mevedel--view-buffer data-buf))
                      ((buffer-live-p vb)))
            (with-current-buffer vb
              (set (make-local-variable sym) val))))
      (set-default-toplevel-value sym val))))

(defun mevedel-permission-mode--set (sym val)
  "Set SYM to VAL for `mevedel-permission-mode'.

Thin wrapper around `mevedel-permission--set-session-scoped' that
targets the session struct's `permission-mode' slot.  See that helper's
docstring for the full scoping contract."
  (setq val (mevedel-permission-mode-normalize val))
  (if mevedel-permission-mode--raw-set
      (mevedel-permission--set-session-scoped
       sym val
       (lambda (session v)
         (setf (mevedel-session-permission-mode session) v)))
    (let ((data-buf (mevedel-permission--current-data-buffer)))
      (if data-buf
          (with-current-buffer data-buf
            (mevedel-permission-mode-transition val))
        (set-default-toplevel-value sym val)))))

(defun mevedel-permission--get-session-scoped (sym slot-getter)
  "Scoped `:get' helper symmetric with `mevedel-permission--set-session-scoped'.

When `current-buffer' reaches a session (directly or via
`mevedel--data-buffer' back-pointer), returns the value produced by
SLOT-GETTER called on that session -- so Customize widgets and tooling
that consult `:get' reflect the session-scoped value.  Otherwise returns
the global default for SYM.

SLOT-GETTER is a function `(SESSION) -> VALUE' reading the relevant
session struct slot."
  (let* ((data-buf (mevedel-permission--current-data-buffer))
         (session (and data-buf
                       (buffer-local-value 'mevedel--session data-buf))))
    (if session
        (funcall slot-getter session)
      (default-toplevel-value sym))))

(defun mevedel-permission-mode--get (sym)
  "Return SYM's session-scoped `mevedel-permission-mode' value.

Returns the current session's `permission-mode' slot when the call is
made from inside a session; otherwise returns the global default."
  (mevedel-permission--get-session-scoped
   sym #'mevedel-session-permission-mode))

(defcustom mevedel-permission-mode 'ask
  "Current permission mode.

Controls the default permission behavior when no explicit rules match.

  `ask'       - Allow recognized inspection and prompt for edits,
                uncertain Bash, and Eval.
  `auto'      - Apply native edits inside allowed roots automatically;
                Bash and Eval retain their normal checks.
  `full-auto' - Skip heuristic Bash and Eval prompts and run live Eval
                automatically.  Explicit denies and missing protected
                resource authority remain effective.

At the generic permission layer, `auto' authorizes tools in the native
`edit' group after their resource boundary is satisfied.  It does not
authorize Bash, Eval, or unrelated mutating tools.  Native edit previews
also apply without an interactive overlay in `auto'; `ask' prompts.

To change this mode at runtime, use `setopt' from the relevant buffer:
when called from inside a session buffer (a data buffer or its view
buffer) the change is scoped to that session only -- other open
sessions keep their current mode and the global default is left
untouched.  When called from any other buffer, the global default is
updated so future sessions pick it up.

The Customize UI is a global-write path in Emacs by design: opening
`customize-variable' or `customize-option' from a session buffer
switches into a dedicated `*Customize ...*' buffer, so at commit time
`current-buffer' is the Customize buffer and no session is in scope.
Changes made through the Customize UI therefore always update the
global default, never the current session.  Use `setopt' (or the
interactive workflow in `mevedel-preview-mode') for session-scoped
changes.

Plain `setq' / `setq-local' bypass this path entirely and tool
execution reads the session slot first, so it would keep using the
old value.  See `mevedel-permission-mode--set' and
`mevedel-permission--set-session-scoped'."
  :type '(choice
          (const :tag "Ask -- prompt for edits and uncertain execution" ask)
          (const :tag "Auto -- apply native edits, check Bash and Eval" auto)
          (const :tag "Full Auto -- skip heuristic execution prompts" full-auto))
  :set #'mevedel-permission-mode--set
  :get #'mevedel-permission-mode--get
  :local 'permanent
  :group 'mevedel)


;;
;;; allowed-tools parsing

(defun mevedel-permission--tool-specifier-key (tool-name)
  "Return the specifier keyword for TOOL-NAME, or nil if absent.

Looks the tool up in the registry and returns `:pattern', `:domain',
`:name', or `:path' based on which `get-*' slot is populated.  Returns
nil when TOOL-NAME is unknown or declares no getter.

Note: specifier keyword semantics are per-tool, not per-keyword.
Both `Skill' and `Agent' use `:name', but `Skill :name' matches the
skill name (invocation identifier) while `Agent :name' matches the
subagent_type.  The keyword is a syntactic slot; the matching
semantics are owned by the tool's `get-name' getter.  Authors of
permission rules should consult each tool's documentation rather
than assume cross-tool uniformity for the same keyword."
  (when-let* ((tool (mevedel-tool-get tool-name)))
    (cond ((mevedel-tool-get-pattern tool) :pattern)
          ((mevedel-tool-get-domain  tool) :domain)
          ((mevedel-tool-get-name    tool) :name)
          ((mevedel-tool-get-path    tool) :path))))

(defun mevedel-permission--parse-rule-string (entry)
  "Parse ENTRY (an `allowed-tools' string) into a rule.

Returns a rule plist of the form
\\=`(TOOL-NAME &key SPECIFIER VALUE :action allow)' suitable for
`mevedel-permission-rules', or signals `user-error' on bad input.

Recognised forms:

- `\"Read\"'              bare tool name
- `\"Edit(src/**)\"'      qualified by path
- `\"Bash(git status)\"'  qualified by exact pattern
- `\"Bash(git status *)\"' qualified by glob pattern
- `\"WebFetch(example.com)\"' qualified by domain
- `\"Agent(verifier)\"'   qualified by sub-agent name

Specifier inference is registry-driven via
`mevedel-permission--tool-specifier-key', so a new tool that ships
with `:get-domain' is automatically qualifiable in skill frontmatter
without editing the permission layer.

Failure modes:

- malformed syntax (no balanced parens, unrecognized shape) ->
  `user-error \"Malformed allowed-tools entry: ENTRY\"'
- unknown tool name -> `user-error'
- qualifier on a tool with no specifier slot (e.g. `\"Ask(foo)\"') ->
  `user-error'"
  (unless (stringp entry)
    (user-error "Malformed allowed-tools entry: %S (must be a string)" entry))
  (let ((case-fold-search nil))
    (cond
     ;; Bare name: ^Tool$
     ((string-match "\\`\\([A-Za-z][A-Za-z0-9]*\\)\\'" entry)
      (let ((tool-name (match-string 1 entry)))
        (unless (mevedel-tool-ensure tool-name)
          (user-error "Unknown tool in allowed-tools: %s" tool-name))
        (list tool-name :action 'allow)))
     ;; Qualified: ^Tool(VALUE)$
     ((string-match
       "\\`\\([A-Za-z][A-Za-z0-9]*\\)(\\(.*\\))\\'" entry)
      (let* ((tool-name (match-string 1 entry))
             (raw-value (match-string 2 entry))
             (value raw-value))
        (unless (mevedel-tool-ensure tool-name)
          (user-error "Unknown tool in allowed-tools: %s" tool-name))
        (let ((spec-key (mevedel-permission--tool-specifier-key tool-name)))
          (unless spec-key
            (user-error "Tool %s does not support qualifiers" tool-name))
          (list tool-name spec-key value :action 'allow))))
     (t
      (user-error "Malformed allowed-tools entry: %s" entry)))))

(defun mevedel-permission--parse-rule-strings (entries)
  "Map `mevedel-permission--parse-rule-string' over ENTRIES.
ENTRIES is a list of strings.  Returns the list of parsed rule
plists.  Signals `user-error' on the first malformed entry."
  (mapcar #'mevedel-permission--parse-rule-string entries))


;;
;;; Rule matching

(defun mevedel-permission--match-path-pattern (path pattern)
  "Check if PATH matches glob PATTERN.

PATTERN supports:
  *   - matches any sequence of characters except /
  **  - matches any sequence of characters including /
  ?   - matches any single character
  ~   - expanded to home directory at pattern start

Returns non-nil if PATH matches PATTERN."
  (when (and path pattern)
    (let* ((expanded (if (or (string-prefix-p "~" pattern)
                             (file-name-absolute-p pattern))
                         (expand-file-name pattern)
                       pattern))
           (expanded-path (expand-file-name path))
           (directory-glob-root
            (when (string-suffix-p "/**" expanded)
              (substring expanded 0 -3)))
           (i 0)
           (len (length expanded))
           (parts (list "\\`")))
      (or (and directory-glob-root
               (string= (directory-file-name expanded-path)
                        (directory-file-name
                         (expand-file-name directory-glob-root))))
          (progn
            (while (< i len)
              (let ((ch (aref expanded i)))
                (cond
                 ;; ** globstar - matches across directories
                 ((and (eq ch ?*)
                       (< (1+ i) len)
                       (eq (aref expanded (1+ i)) ?*))
                  (push ".*" parts)
                  (setq i (+ i 2)))
                 ;; * - matches within a single directory
                 ((eq ch ?*)
                  (push "[^/]*" parts)
                  (setq i (1+ i)))
                 ;; ? - matches single character
                 ((eq ch ??)
                  (push "." parts)
                  (setq i (1+ i)))
                 ;; Literal character
                 (t
                  (push (regexp-quote (char-to-string ch)) parts)
                  (setq i (1+ i))))))
            (push "\\'" parts)
            (string-match-p (apply #'concat (nreverse parts))
                            expanded-path))))))

(defconst mevedel-permission--specifier-keys
  '(:path :pattern :domain :name)
  "Keys recognised as rule specifiers.
A rule may carry at most one of these.  The first match wins.")

(defun mevedel-permission--rule-specifier (rule)
  "Return (KEY . VALUE) for RULE's specifier, or (nil . nil) if unqualified."
  (let ((plist (cdr rule))
        result)
    (cl-loop for key in mevedel-permission--specifier-keys
             when (plist-member plist key)
             do (setq result (cons key (plist-get plist key)))
             and return nil)
    (or result (cons nil nil))))

(defun mevedel-permission--match-specifier (kind pattern value)
  "Return non-nil if VALUE matches PATTERN under specifier KIND."
  (when (and pattern value)
    (pcase kind
      (:path (mevedel-permission--match-path-pattern value pattern))
      (:pattern
       (if (string-suffix-p ":*" pattern)
           (let ((prefix (substring pattern 0 -2)))
             (or (string= value prefix)
                 (string-prefix-p (concat prefix " ") value)))
         (string-match-p (wildcard-to-regexp pattern) value)))
      ((or :domain :name)
       (string-match-p (wildcard-to-regexp pattern) value)))))

(cl-defun mevedel-permission--find-rules
    (rules tool-name &key path pattern domain name)
  "Find all matching rules in RULES for TOOL-NAME under the given values.

RULES is a list in the format of `mevedel-permission-rules'.  Rule
matches are determined by the rule's specifier (one of `:path',
`:pattern', `:domain', `:name') against PATH, PATTERN, DOMAIN, or NAME.
Unqualified rules match unconditionally.  Return a list of
matching rules in order (later entries = higher priority)."
  (let ((matches nil)
        (values `((:path    . ,path)
                  (:pattern . ,pattern)
                  (:domain  . ,domain)
                  (:name    . ,name))))
    (dolist (rule rules)
      (let ((rule-tool (car rule)))
        (when (or (equal rule-tool "*")
                  (equal rule-tool tool-name))
          (let* ((spec (mevedel-permission--rule-specifier rule))
                 (kind (car spec))
                 (rule-value (cdr spec)))
            (cond
             ;; No specifier: matches unconditionally
             ((null kind)
              (push rule matches))
             ;; Specifier present: compare against the corresponding value
             ((mevedel-permission--match-specifier
               kind rule-value (alist-get kind values))
              (push rule matches)))))))
    (nreverse matches)))

(cl-defun mevedel-permission--rules-action
    (rules tool-name &key path pattern domain name)
  "Determine the effective action from RULES for TOOL-NAME and specifiers.

Rules that carry a specifier (any of `:path', `:pattern', `:domain',
`:name') match PATH, PATTERN, DOMAIN, or NAME and take precedence over
unqualified rules.  Within each group, deny > ask > allow.  Return
`allow', `deny', `ask', or nil if no rules match."
  (let ((matching (mevedel-permission--find-rules
                   rules tool-name
                   :path path :pattern pattern
                   :domain domain :name name))
        (spec-deny nil) (spec-ask nil) (spec-allow nil)
        (gen-deny nil) (gen-ask nil) (gen-allow nil))
    (dolist (rule matching)
      (let ((has-spec (car (mevedel-permission--rule-specifier rule))))
        (pcase (plist-get (cdr rule) :action)
          ('deny (if has-spec (setq spec-deny t) (setq gen-deny t)))
          ('ask (if has-spec (setq spec-ask t) (setq gen-ask t)))
          ('allow (if has-spec (setq spec-allow t) (setq gen-allow t))))))
    (cond
     ;; Specifier-carrying rules take precedence
     (spec-deny 'deny)
     (spec-ask 'ask)
     (spec-allow 'allow)
     ;; Generic rules
     (gen-deny 'deny)
     (gen-ask 'ask)
     (gen-allow 'allow)
     (t nil))))


;;
;;; Protected paths

(defun mevedel-permission--path-protected-p (path)
  "Check if PATH matches any pattern in `mevedel-protected-paths'.

Returns non-nil if the path is protected."
  (when path
    (let ((expanded (expand-file-name path)))
      (cl-loop for pattern in mevedel-protected-paths
               thereis
               (mevedel-permission--match-path-pattern expanded pattern)))))

(defun mevedel-permission--path-in-workspace-p (path workspace-root)
  "Return non-nil when PATH is WORKSPACE-ROOT or is contained by it."
  (when (and path workspace-root)
    (let ((abs-path (expand-file-name path))
          (abs-root (expand-file-name workspace-root)))
      (or (string= (directory-file-name abs-path)
                   (directory-file-name abs-root))
          (string-prefix-p (file-name-as-directory abs-root)
                           abs-path)))))

(defun mevedel-permission--path-in-allowed-roots-p (path roots)
  "Return non-nil when PATH is contained by any root in ROOTS."
  (when path
    (cl-loop for root in roots
             thereis (mevedel-permission--path-in-workspace-p path root))))

(defun mevedel-permission--path-in-exact-allowed-paths-p (path allowed-paths)
  "Return non-nil when PATH exactly matches one of ALLOWED-PATHS."
  (when path
    (let ((expanded (expand-file-name path)))
      (cl-loop for allowed in allowed-paths
               thereis (string= expanded (expand-file-name allowed))))))

(defun mevedel-permission--resource-granted-p (path access grants)
  "Return non-nil when GRANTS authorize ACCESS to exact PATH."
  (when (and path (memq access '(read write)))
    (let ((expanded (expand-file-name path)))
      (cl-loop for grant in grants
               when (proper-list-p grant)
               thereis
               (and (stringp (plist-get grant :path))
                    (string= expanded
                             (expand-file-name (plist-get grant :path)))
                    (or (eq (plist-get grant :access) 'write)
                        (eq (plist-get grant :access) access)))))))


;;
;;; Mode decisions

(defun mevedel-permission--mode-decision
    (mode read-only-p &optional native-edit-p)
  "Determine permission from MODE and the tool's capability flags.
READ-ONLY-P identifies inspection tools.  NATIVE-EDIT-P identifies tools in
the native `edit' group; it never applies to Bash or Eval.

Returns `allow', `deny', or `ask'."
  (pcase mode
    ('full-auto 'allow)
    ('auto (if (or read-only-p native-edit-p) 'allow 'ask))
    ('ask (if read-only-p 'allow 'ask))
    ;; Unknown mode: fall through to ask
    (_ 'ask)))


;;
;;; Decision chain -- bucket-aware

(defun mevedel-permission--collect-buckets
    (invocation-rules request-rules session-rules persistent-rules)
  "Return permission-rule buckets from all rule layers.

INVOCATION-RULES, REQUEST-RULES, SESSION-RULES, and PERSISTENT-RULES are
the dynamic rule layers.

Buckets are listed innermost-first; pass 2 (allow/ask) walks them
in order and returns the first non-nil decision.  Pass 1 (deny)
is order-insensitive but reuses the same alist."
  `((:invocation . ,invocation-rules)
    (:request    . ,request-rules)
    (:session    . ,session-rules)
    (:persistent . ,persistent-rules)
    (:defcustom  . ,mevedel-permission-rules)))

(defun mevedel-permission--bucket-action
    (bucket-rules tool-name path pattern domain name)
  "Resolve BUCKET-RULES action for TOOL-NAME, PATH, PATTERN, DOMAIN, and NAME."
  (mevedel-permission--rules-action
   bucket-rules tool-name
   :path path :pattern pattern :domain domain :name name))

(defun mevedel-permission--first-deny-bucket
    (buckets tool-name path pattern domain name)
  "Return first BUCKETS key denying TOOL-NAME for PATH, PATTERN, DOMAIN, or NAME."
  (cl-loop for (key . rules) in buckets
           when (eq (mevedel-permission--bucket-action
                     rules tool-name path pattern domain name)
                    'deny)
           return key))

(defun mevedel-permission--any-deny
    (buckets tool-name path pattern domain name)
  "Return non-nil if BUCKETS deny TOOL-NAME for PATH, PATTERN, DOMAIN, or NAME.
BUCKETS is the alist from `mevedel-permission--collect-buckets'."
  (not (null (mevedel-permission--first-deny-bucket
              buckets tool-name path pattern domain name))))

(defun mevedel-permission--first-non-nil-action-with-bucket
    (buckets tool-name path pattern domain name)
  "Walk BUCKETS for TOOL-NAME and return (ACTION . BUCKET).

PATH, PATTERN, DOMAIN, and NAME are the specifier values."
  (cl-loop for (key . rules) in buckets
           for action = (mevedel-permission--bucket-action
                         rules tool-name path pattern domain name)
           when action return (cons action key)))

(defun mevedel-permission--first-non-nil-action
    (buckets tool-name path pattern domain name)
  "Walk BUCKETS for TOOL-NAME and return first non-nil bucket action.

PATH, PATTERN, DOMAIN, and NAME are the specifier values."
  (car (mevedel-permission--first-non-nil-action-with-bucket
        buckets tool-name path pattern domain name)))

(defun mevedel-permission--normalize-outcome (outcome)
  "Return the log-safe decision symbol for permission OUTCOME."
  (pcase outcome
    (`(deny . ,_) 'deny)
    (`(feedback . ,_) 'deny)
    (_ outcome)))

(defun mevedel-permission--decision
    (outcome via &rest props)
  "Return permission decision metadata for OUTCOME through VIA with PROPS."
  (let ((normalized (mevedel-permission--normalize-outcome outcome)))
    (append (list :outcome normalized
                  :raw-outcome outcome
                  :via via)
            props)))

(defun mevedel-permission-decision-raw-outcome (decision)
  "Return the pipeline outcome represented by DECISION."
  (if (and (listp decision) (plist-member decision :outcome))
      (if (plist-member decision :raw-outcome)
          (plist-get decision :raw-outcome)
        (plist-get decision :outcome))
    decision))

(defun mevedel-permission-decision-metadata-p (value)
  "Return non-nil when VALUE is permission decision metadata."
  (and (listp value)
       (keywordp (car-safe value))
       (plist-member value :outcome)
       (plist-member value :via)))

(defun mevedel-permission--metadata-content (content)
  "Return CONTENT marked for permission metadata collection when possible."
  (if (and (listp content) (keywordp (car-safe content)))
      (plist-put (copy-sequence content) :permission-decision-metadata t)
    content))

(cl-defun mevedel-permission--invocation-context
    (&key tool tool-name args session workspace request invocation buffer
          path pattern domain name mode workspace-root allowed-roots
          exact-allowed-paths invocation-rules request-rules session-rules
          persistent-rules resource-grants warn-no-session-p)
  "Return normalized permission invocation context.

The context concentrates facts shared by the permission decision
chain, prompt queue, and Bash/Eval adapters.

TOOL, TOOL-NAME, ARGS, SESSION, WORKSPACE, REQUEST, INVOCATION, BUFFER,
PATH, PATTERN, DOMAIN, NAME, MODE, WORKSPACE-ROOT, ALLOWED-ROOTS,
EXACT-ALLOWED-PATHS, INVOCATION-RULES, REQUEST-RULES, SESSION-RULES,
PERSISTENT-RULES, RESOURCE-GRANTS, and WARN-NO-SESSION-P provide the
context facts."
  (setq tool (or tool
                 (and tool-name (mevedel-tool-ensure tool-name)))
        tool-name (or tool-name (and tool (mevedel-tool-name tool))))
  (when (and tool args)
    (cl-flet ((extract (getter current)
                (or current
                    (when-let* ((fn (funcall getter tool)))
                      (ignore-errors (funcall fn args))))))
      (setq path    (extract #'mevedel-tool-get-path    path)
            pattern (extract #'mevedel-tool-get-pattern pattern)
            domain  (extract #'mevedel-tool-get-domain  domain)
            name    (extract #'mevedel-tool-get-name    name))))
  (setq workspace (or workspace
                      (and session (mevedel-session-workspace session)))
        workspace-root
        (or workspace-root
            (and workspace
                 (ignore-errors (mevedel-workspace-root workspace))))
        allowed-roots
        (or allowed-roots
            (when (and workspace (fboundp 'mevedel--all-allowed-roots))
              (ignore-errors (mevedel--all-allowed-roots buffer))))
        exact-allowed-paths
        (or exact-allowed-paths
            (and (equal tool-name "Read")
                 session
                 (mevedel-session-active-dropped-file-grants session)))
        invocation-rules
        (or invocation-rules
            (and invocation
                 (mevedel-agent-invocation-skill-permission-rules
                  invocation)))
        request-rules
        (or request-rules
            (and request (mevedel-request-skill-permission-rules request)))
        session-rules
        (or session-rules
            (and session (mevedel-session-permission-rules session)))
        persistent-rules
        (or persistent-rules
            (and workspace
                 (mevedel-permission--load-persistent-rules workspace)))
        resource-grants
        (or resource-grants
            (append (and session (mevedel-session-resource-grants session))
                    (and workspace
                         (mevedel-permission--load-persistent-resource-grants
                          workspace))))
        mode (or mode (and session (mevedel-session-permission-mode session))))
  (when (and warn-no-session-p (not session))
    (display-warning
     'mevedel
     (format "Permission step for %s ran with no session in \
context; falling back to defcustom defaults.  Session-scoped \
rules and the active permission mode are not being consulted.  \
This usually means the tool was dispatched from a buffer whose \
`mevedel--session' was not set; in production that should not \
happen for a non-read-only tool."
             tool-name)
     :warning))
  (let* ((specifier-key (cond (pattern :pattern)
                              (domain :domain)
                              (name :name)
                              (path :path)))
         (specifier-value (or pattern domain name path))
         (workspace-boundary-p
          (and path
               (not (mevedel-permission--path-in-allowed-roots-p
                     path (or allowed-roots
                              (and workspace-root
                                   (list workspace-root)))))))
         (resource-access (and path
                               (if (and tool (mevedel-tool-read-only-p tool))
                                   'read
                                 'write)))
         (resource-request-p
          (and resource-access
               (or workspace-boundary-p
                   (mevedel-permission--path-protected-p path))))
         (rule-key (if workspace-boundary-p :path specifier-key))
         (rule-value (if workspace-boundary-p
                         (expand-file-name path)
                       specifier-value))
         (buckets (mevedel-permission--collect-buckets
                   invocation-rules request-rules
                   session-rules persistent-rules)))
    (list :tool tool
          :tool-name tool-name
          :args args
          :session session
          :workspace workspace
          :request request
          :invocation invocation
          :buffer buffer
          :path path
          :pattern pattern
          :domain domain
          :name name
          :specifier-key specifier-key
          :specifier-value specifier-value
          :protected-path (and path (mevedel-permission--path-protected-p path))
          :workspace-root workspace-root
          :allowed-roots allowed-roots
          :exact-allowed-paths exact-allowed-paths
          :resource-access (and resource-request-p resource-access)
          :resource-grants resource-grants
          :rule-tool (if workspace-boundary-p "*" tool-name)
          :rule-key rule-key
          :rule-value rule-value
          :include-always (not (null workspace))
          :invocation-rules invocation-rules
          :request-rules request-rules
          :session-rules session-rules
          :persistent-rules persistent-rules
          :buckets buckets
          :mode mode)))

(defun mevedel-permission--checker-args (context)
  "Return `mevedel-check-permission' keyword args for CONTEXT."
  (let ((content (plist-get context :args))
        (tool-name (plist-get context :tool-name)))
    (when (and (member tool-name '("Bash" "Eval"))
               (listp content)
               (keywordp (car-safe content)))
      (setq content (plist-put (copy-sequence content)
                               :permission-context context)))
    (list :tool-struct (plist-get context :tool)
          :path (plist-get context :path)
          :pattern (plist-get context :pattern)
          :domain (plist-get context :domain)
          :name (plist-get context :name)
          :content content
          :invocation-rules (plist-get context :invocation-rules)
          :request-rules (plist-get context :request-rules)
          :session-rules (plist-get context :session-rules)
          :persistent-rules (plist-get context :persistent-rules)
          :mode (plist-get context :mode)
          :session (plist-get context :session)
          :workspace-root (plist-get context :workspace-root)
          :allowed-roots (plist-get context :allowed-roots)
          :exact-allowed-paths
          (plist-get context :exact-allowed-paths)
          :resource-access (plist-get context :resource-access)
          :resource-grants (plist-get context :resource-grants))))

(defun mevedel-permission--goal-read-only-phase-p (&optional session)
  "Return non-nil when the owning session's Goal phase is read-only."
  (when (fboundp 'mevedel-goal-read-only-phase-p)
    (mevedel-goal-read-only-phase-p
     (or session
         (and (boundp 'mevedel--session) mevedel--session)
         (and (boundp 'mevedel--agent-invocation)
              mevedel--agent-invocation
              (mevedel-agent-invocation-parent-session
               mevedel--agent-invocation))))))

(cl-defun mevedel-permission--preflight
    (tool-name &key tool-struct path pattern domain name content
               invocation-rules request-rules session-rules persistent-rules
               mode session workspace-root allowed-roots exact-allowed-paths
               resource-access resource-grants)
  "Return normalized permission facts and any decision before the tool slot.

This pure preflight owns specifier extraction, rule buckets, absolute
denials, protected-path facts, and allowed-root normalization.  The returned
plist's `:early-decision' is nil when the tool-owned permission slot and
the remaining decision chain still need to run.

TOOL-NAME identifies the tool.  TOOL-STRUCT and CONTENT supply its policy
and input.  PATH, PATTERN, DOMAIN, and NAME may supply pre-extracted
specifiers.  INVOCATION-RULES, REQUEST-RULES, SESSION-RULES, and
PERSISTENT-RULES are the ordered rule sources.  MODE selects the permission
mode.  WORKSPACE-ROOT, ALLOWED-ROOTS, EXACT-ALLOWED-PATHS, and
RESOURCE-GRANTS define the filesystem boundary."
  (setq mode (or mode mevedel-permission-mode))
  (when (and tool-struct content)
    (cl-flet ((extract (getter current)
                (or current
                    (when-let* ((fn (funcall getter tool-struct)))
                      (ignore-errors (funcall fn content))))))
      (setq path (extract #'mevedel-tool-get-path path)
            pattern (extract #'mevedel-tool-get-pattern pattern)
            domain (extract #'mevedel-tool-get-domain domain)
            name (extract #'mevedel-tool-get-name name))))
  (let* ((read-only-p
          (when tool-struct (mevedel-tool-read-only-p tool-struct)))
         (resource-access (or resource-access
                              (if read-only-p 'read 'write)))
         (resource-granted-p
          (mevedel-permission--resource-granted-p
           path resource-access resource-grants))
         (buckets
          (mevedel-permission--collect-buckets
           invocation-rules request-rules session-rules persistent-rules))
         (deny-bucket
          (mevedel-permission--first-deny-bucket
           buckets tool-name path pattern domain name))
         (early-decision
          (cond
           (deny-bucket
            (mevedel-permission--decision
             'deny 'deny-rule :bucket deny-bucket))
           ((and (not read-only-p)
                 (mevedel-permission--goal-read-only-phase-p session))
            (mevedel-permission--decision 'deny 'goal-phase)))))
    (list :tool-name tool-name
          :tool tool-struct
          :content content
          :path path
          :pattern pattern
          :domain domain
          :name name
          :mode mode
          :read-only-p read-only-p
          :buckets buckets
          :allowed-roots (or allowed-roots
                             (and workspace-root (list workspace-root)))
          :exact-allowed-paths exact-allowed-paths
          :resource-access resource-access
          :resource-granted-p resource-granted-p
          :protected-path-p (mevedel-permission--path-protected-p path)
          :early-decision early-decision)))

(defun mevedel-permission--sync-tool-decision (context)
  "Return the synchronous tool-slot decision for preflight CONTEXT.

Return nil when the tool has no synchronous slot or its slot declines to
decide.  Permission denials retain their reason as decision metadata;
other slot errors are reported and treated as no decision."
  (when-let* ((tool (plist-get context :tool))
              (check-fn (mevedel-tool-check-permission tool)))
    (let ((result
           (condition-case err
               (funcall check-fn tool (plist-get context :content))
             (mevedel-permission-denied
              (mevedel-permission--decision
               (cons 'deny (cadr err)) 'tool-slot))
             (error
              (message "mevedel: check-permission error: %S" err)
              nil))))
      (when result
        (if (mevedel-permission-decision-metadata-p result)
            result
          (mevedel-permission--decision result 'tool-slot))))))

(defun mevedel-check-permission-with-metadata (tool-name &rest args)
  "Check permission for TOOL-NAME using keyword ARGS.

ARGS are the inputs documented by `mevedel-permission--preflight'.

Returns a plist describing an `allow', `deny', or `ask' decision.

The decision chain:
  1. Extract specifier values via tool-struct getters when missing
  2. Resolve absolute decisions across all buckets:
       any bucket yields `deny' -> deny;
       read-only Goal phase + mutating tool -> deny
  3. Call the tool checker, when present, to decide command authority
  4. Resolve allow/ask rules innermost-first:
       invocation -> request -> session -> persistent -> defcustom.
  5. Apply mode hard-deny
  6. For a path, independently require allowed-root, exact-path, or exact
     resource-grant authority; otherwise ask
  7. Apply the mode/default decision

For tools with a checker, both command authority and resource authority must
allow.  Neither can substitute for the other."
  (let* ((context (apply #'mevedel-permission--preflight tool-name args))
         (tool (plist-get context :tool))
         (early-decision (plist-get context :early-decision)))
    (or early-decision
        (if (and tool (mevedel-tool-check-permission tool))
            (mevedel-permission--finish-tool-decision
             context (mevedel-permission--sync-tool-decision context))
          (mevedel-check-permission--tail-decision context)))))

(defun mevedel-check-permission (tool-name &rest args)
  "Check permission for TOOL-NAME with ARGS.

Return `allow', `deny', or `ask'."
  (mevedel-permission-decision-raw-outcome
   (apply #'mevedel-check-permission-with-metadata tool-name args)))


;;
;;; Async decision chain

(defun mevedel-check-permission-async-with-metadata
    (tool-name cont &rest args)
  "Async permission decision for TOOL-NAME using keyword ARGS.

Invokes CONT with permission decision metadata.  The original pipeline
outcome is available through `mevedel-permission-decision-raw-outcome'.
ARGS are the inputs documented by `mevedel-permission--preflight'.

Normalization, absolute policy, rules, modes, and resource authority run
synchronously just like `mevedel-check-permission'.  The tool command-policy
slot may run async when it defines `:check-permission-async'; the sync-slot
adapter preserves the denial REASON captured from a
`mevedel-permission-denied' signal so `(deny . REASON)' reaches CONT.

Bucket-aware; see `mevedel-check-permission' for the keyword-arg
semantics.  EXACT-ALLOWED-PATHS is passed to the shared tail as an
exact-match in-bounds path list."
  (let* ((context (apply #'mevedel-permission--preflight tool-name args))
         (tool (plist-get context :tool))
         (early-decision (plist-get context :early-decision))
         (finish
          (lambda (slot-decision)
            (funcall cont
                     (mevedel-permission--finish-tool-decision
                      context slot-decision)))))
    (cond
     (early-decision (funcall cont early-decision))
     ((and tool (mevedel-tool-check-permission-async tool))
      (funcall
       (mevedel-tool-check-permission-async tool)
       tool
       (mevedel-permission--metadata-content (plist-get context :content))
       (lambda (slot-result)
         (funcall
          finish
          (and slot-result
               (if (mevedel-permission-decision-metadata-p slot-result)
                   slot-result
                 (mevedel-permission--decision
                  slot-result
                  (cond
                   ((equal tool-name "Bash") 'bash-classifier)
                   ((equal tool-name "Eval") 'eval-policy)
                   (t 'tool-slot)))))))))
     (t
      (if (and tool (mevedel-tool-check-permission tool))
          (funcall finish
                   (mevedel-permission--sync-tool-decision context))
        (funcall cont
                 (mevedel-check-permission--tail-decision context)))))))

(defun mevedel-check-permission-async (tool-name cont &rest args)
  "Check TOOL-NAME permission with ARGS, then call CONT asynchronously."
  (apply #'mevedel-check-permission-async-with-metadata
         tool-name
         (lambda (decision)
           (funcall cont
                    (mevedel-permission-decision-raw-outcome decision)))
         args))

(defun mevedel-permission--resource-decision (context)
  "Return CONTEXT's independent filesystem resource decision, or nil."
  (when-let* ((path (plist-get context :path)))
    (let ((granted-p (plist-get context :resource-granted-p)))
      (cond
       ((and (plist-get context :protected-path-p) granted-p)
        (mevedel-permission--decision 'allow 'resource-grant))
       ((plist-get context :protected-path-p)
        (mevedel-permission--decision 'ask 'protected-path))
       ((mevedel-permission--path-in-allowed-roots-p
         path (plist-get context :allowed-roots))
        (mevedel-permission--decision 'allow 'allowed-root))
       ((mevedel-permission--path-in-exact-allowed-paths-p
         path (plist-get context :exact-allowed-paths))
        (mevedel-permission--decision 'allow 'exact-path))
       (granted-p
        (mevedel-permission--decision 'allow 'resource-grant))
       (t
        (mevedel-permission--decision 'ask 'workspace-boundary))))))

(defun mevedel-permission--finish-tool-decision (context slot-decision)
  "Combine CONTEXT's command SLOT-DECISION with resource authority."
  (let* ((command-context
          (plist-put (copy-sequence context) :skip-resource-boundary-p t))
         (policy-decision
          (or slot-decision
              (mevedel-check-permission--tail-decision command-context)))
         (policy-outcome
          (mevedel-permission-decision-raw-outcome policy-decision)))
    (if (eq policy-outcome 'allow)
        (or (mevedel-permission--resource-decision context)
            policy-decision)
      policy-decision)))

(defun mevedel-check-permission--tail-decision (context)
  "Return decision metadata for preflight CONTEXT's permission-chain tail.

The preflight owns normalization and absolute denials.  Callers own the
tool-specific command-policy slot; this function covers the shared rule,
mode, and native-resource tail."
  (let* ((tool-name (plist-get context :tool-name))
         (buckets (plist-get context :buckets))
         (path (plist-get context :path))
         (pattern (plist-get context :pattern))
         (domain (plist-get context :domain))
         (name (plist-get context :name))
         (skip-resource-boundary-p
          (plist-get context :skip-resource-boundary-p))
         (mode (plist-get context :mode))
         (read-only-p (plist-get context :read-only-p))
         (tool (plist-get context :tool))
         (native-edit-p
          (and tool (memq 'edit (mevedel-tool-groups tool))))
         (resource-decision
          (and (not skip-resource-boundary-p)
               (mevedel-permission--resource-decision context))))
    (cond
     ;; Protected resources require exact grants even when a path rule allows.
     ((and (not skip-resource-boundary-p)
           (plist-get context :protected-path-p)
           (not (plist-get context :resource-granted-p)))
      (mevedel-permission--decision 'ask 'protected-path))
     ;; Step 5: pass 2 -- allow/ask innermost-first across buckets.
     ((when-let* ((action-bucket
                   (mevedel-permission--first-non-nil-action-with-bucket
                    buckets tool-name path pattern domain name)))
        (let ((action (car action-bucket))
              (bucket (cdr action-bucket)))
          (cond
           ((eq action 'allow)
            (mevedel-permission--decision 'allow 'rule :bucket bucket))
           ((eq action 'ask)
            (if (eq (mevedel-permission--mode-decision
                     mode read-only-p native-edit-p)
                    'deny)
                (mevedel-permission--decision 'deny 'mode :bucket bucket)
              (mevedel-permission--decision 'ask 'rule :bucket bucket)))))))
     ;; Step 6: mode hard-deny.
     ((eq (mevedel-permission--mode-decision
           mode read-only-p native-edit-p)
          'deny)
      (mevedel-permission--decision 'deny 'mode))
     ;; Steps 7-8: missing native resource authority forces a prompt.  An
     ;; allowed root or exact grant satisfies only the resource half; the mode
     ;; still decides whether the operation itself is automatic.
     ((and resource-decision
           (eq (mevedel-permission-decision-raw-outcome resource-decision)
               'ask))
      resource-decision)
     ;; Step 9: mode/default decision.
     (t (let ((mode-result (mevedel-permission--mode-decision
                            mode read-only-p native-edit-p)))
          (if (and (eq mode-result 'allow)
                   resource-decision
                   (eq (mevedel-permission-decision-raw-outcome
                        resource-decision)
                       'allow))
              resource-decision
            (mevedel-permission--decision mode-result 'mode)))))))

;;
;;; Session rule storage

(defun mevedel-permission--resource-grant (path access)
  "Return normalized exact PATH ACCESS resource grant."
  (unless (and (stringp path) (not (string-empty-p path)))
    (error "Invalid resource path: %S" path))
  (unless (memq access '(read write))
    (error "Invalid resource access: %s" access))
  (list :path (expand-file-name path) :access access))

(defun mevedel-permission-add-session-resource-grant (session path access)
  "Grant SESSION exact PATH access at READ or WRITE level."
  (let* ((grant (mevedel-permission--resource-grant path access))
         (grants (mevedel-session-resource-grants session)))
    (unless (member grant grants)
      (setf (mevedel-session-resource-grants session)
            (append grants (list grant))))
    grant))

(defun mevedel-permission-remove-session-resource-grant
    (session path access)
  "Revoke SESSION's exact PATH ACCESS resource grant."
  (let ((grant (mevedel-permission--resource-grant path access)))
    (setf (mevedel-session-resource-grants session)
          (cl-remove grant (mevedel-session-resource-grants session)
                     :test #'equal))))

(defun mevedel-permission--build-rule (tool-name action spec-key spec-value)
  "Build a permission rule list from the given components.

TOOL-NAME is the tool name string or \"*\".  ACTION is `allow', `deny',
or `ask'.  SPEC-KEY is one of `:path', `:pattern', `:domain', `:name',
or nil for an unqualified rule.  SPEC-VALUE is the glob associated with
SPEC-KEY (ignored when SPEC-KEY is nil)."
  (if (and spec-key spec-value)
      (list tool-name spec-key spec-value :action action)
    (list tool-name :action action)))

(cl-defun mevedel-permission--add-session-rule
    (session tool-name action &optional path &key spec-key spec-value)
  "Add a permission rule to SESSION's rule list.

TOOL-NAME is the tool name string.  ACTION is `allow' or `deny'.

Positional PATH is retained for existing call sites; when supplied it is
equivalent to SPEC-KEY `:path' with that value.  Callers specifying
another specifier should pass SPEC-KEY (e.g. `:pattern') and SPEC-VALUE
instead, leaving PATH nil.

Mutates SESSION's `permission-rules' slot via `setf' -- this is a
**by-reference** write.  Sub-agents share the parent session by
reference (see `mevedel-agent-exec--allocate-agent-buffer'), so a
rule recorded inside any sub-agent's permission prompt, such as
\"allow-session\" or \"deny-session\", immediately applies to the parent
and to every other live sub-agent sharing the same session struct.  This
is a deliberate contract, not an accident of the buffer-local plumbing."
  (let* ((key (or spec-key (and path :path)))
         (value (or spec-value path))
         (rule (mevedel-permission--build-rule tool-name action key value))
         (rules (mevedel-session-permission-rules session)))
    (unless (member rule rules)
      (setf (mevedel-session-permission-rules session)
            (append rules (list rule))))))


;;
;;; Persistent rule storage

(defun mevedel-permission--persistent-file (workspace)
  "Return the path to WORKSPACE's persistent permission rules file."
  (file-name-concat (mevedel-workspace-state-dir workspace)
                     "permissions.el"))

(defun mevedel-permission--read-store-file (file)
  "Read the permission store plist from FILE, or nil when invalid."
  (when (file-readable-p file)
    (condition-case nil
        (with-temp-buffer
          (insert-file-contents file)
          (let ((store (read (current-buffer))))
            (and (proper-list-p store)
                 (plist-member store :rules)
                 (plist-member store :resource-grants)
                 store)))
      (error nil))))

(defun mevedel-permission--write-store-file (file store)
  "Write permission STORE plist to FILE."
  (make-directory (file-name-directory file) t)
  (with-temp-file file
    (insert ";; Mevedel persistent permissions\n")
    (insert ";; Auto-generated, safe to edit\n\n")
    (pp store (current-buffer))))

(defun mevedel-permission--load-persistent-rules (workspace)
  "Load persistent permission rules for WORKSPACE.

Loads rules from both the global directory (`mevedel-user-dir') and the
project directory (WORKSPACE's .mevedel/).  Global rules are loaded
first, project rules appended after so they take precedence.

Returns a merged list in `mevedel-permission-rules' format."
  (let ((global-file (file-name-concat mevedel-user-dir "permissions.el"))
        (project-file (mevedel-permission--persistent-file workspace)))
    (append (plist-get (mevedel-permission--read-store-file global-file) :rules)
            (plist-get (mevedel-permission--read-store-file project-file)
                       :rules))))

(defun mevedel-permission--load-persistent-resource-grants (workspace)
  "Load exact resource grants persisted for WORKSPACE."
  (cl-remove-if-not
   (lambda (grant)
     (and (proper-list-p grant)
          (stringp (plist-get grant :path))
          (file-name-absolute-p (plist-get grant :path))
          (memq (plist-get grant :access) '(read write))))
   (plist-get (mevedel-permission--read-store-file
               (mevedel-permission--persistent-file workspace))
              :resource-grants)))

(cl-defun mevedel-permission--save-persistent-rule
    (workspace tool-name action &optional path &key spec-key spec-value)
  "Append a permission rule to WORKSPACE's persistent rules file.

TOOL-NAME and ACTION define the rule.  Positional PATH is equivalent
to SPEC-KEY `:path'.  SPEC-KEY/SPEC-VALUE let callers store rules
qualified by any specifier (`:path', `:pattern', `:domain', `:name').
The file is created if it does not exist."
  (let* ((file (mevedel-permission--persistent-file workspace))
         (store (or (mevedel-permission--read-store-file file)
                    (list :rules nil :resource-grants nil)))
         (existing (plist-get store :rules))
         (key (or spec-key (and path :path)))
         (value (or spec-value path))
         (rule (mevedel-permission--build-rule tool-name action key value))
         (updated (if (member rule existing)
                      existing
                    (append existing (list rule)))))
    (mevedel-permission--write-store-file
     file (plist-put store :rules updated))))

(defun mevedel-permission--save-persistent-resource-grant
    (workspace path access)
  "Persist exact PATH ACCESS for WORKSPACE."
  (let* ((file (mevedel-permission--persistent-file workspace))
         (store (or (mevedel-permission--read-store-file file)
                    (list :rules nil :resource-grants nil)))
         (grant (mevedel-permission--resource-grant path access))
         (grants (plist-get store :resource-grants)))
    (unless (member grant grants)
      (setq grants (append grants (list grant))))
    (mevedel-permission--write-store-file
     file (plist-put store :resource-grants grants))
    grant))

(defun mevedel-permission-remove-persistent-resource-grant
    (workspace path access)
  "Revoke WORKSPACE's exact PATH ACCESS resource grant."
  (let* ((file (mevedel-permission--persistent-file workspace))
         (store (mevedel-permission--read-store-file file))
         (grant (mevedel-permission--resource-grant path access)))
    (when store
      (mevedel-permission--write-store-file
       file
       (plist-put store :resource-grants
                  (cl-remove grant (plist-get store :resource-grants)
                             :test #'equal))))))


;;
;;; Prompt result dispatch

(cl-defun mevedel-permission--apply-prompt-result
    (result tool-name &optional session workspace path
            &key spec-key spec-value resource-access)
  "Dispatch a permission prompt RESULT to the correct storage.

RESULT is one of:
  `allow-once'    -- return `allow', no storage
  `allow-session' -- add a session rule or resource grant, return `allow'
  `always-allow'  -- save a persistent rule or resource grant, return `allow'
  `deny-once'     -- return `deny', no storage
  `deny-session'  -- add session deny rule, return `deny'

TOOL-NAME is the tool being permitted.  SESSION and WORKSPACE are used
for storage.  Positional PATH scopes the authority to a file path (kept
for call sites that already pass it).  SPEC-KEY/SPEC-VALUE allow rule
scoping by any other specifier (`:pattern', `:domain', `:name').
RESOURCE-ACCESS stores exact path authority separately from rules."
  (cl-flet ((session-rule (action)
              (when session
                (mevedel-permission--add-session-rule
                 session tool-name action path
                 :spec-key spec-key :spec-value spec-value)))
            (session-resource-grant ()
              (when (and session path resource-access)
                (mevedel-permission-add-session-resource-grant
                 session path resource-access)))
            (persistent-resource-grant ()
              (when (and workspace path resource-access)
                (mevedel-permission--save-persistent-resource-grant
                 workspace path resource-access)))
            (persistent-rule (action)
              (cond
               (workspace
                (mevedel-permission--save-persistent-rule
                 workspace tool-name action path
                 :spec-key spec-key :spec-value spec-value))
               (t
                ;; User clicked always-allow but no workspace is
                ;; in scope (gone since enqueue, or session not
                ;; bound to one).  Silently dropping the persistent
                ;; rule write produces a session-only rule and
                ;; surprises the user on next Emacs start.  Warn so
                ;; the gap is at least diagnosable.
                (display-warning
                 'mevedel
                 (format "Persistent rule for %s skipped: no workspace in context"
                         tool-name)
                 :warning)))))
    (pcase result
      ('allow-once 'allow)
      ('allow-session
       (if resource-access
           (session-resource-grant)
         (session-rule 'allow))
       'allow)
      ('always-allow
       (if resource-access
           (persistent-resource-grant)
         (persistent-rule 'allow)
         (session-rule 'allow))
       'allow)
      ('deny-once 'deny)
      ('deny-session (session-rule 'deny) 'deny)
      (_ 'deny))))

(provide 'mevedel-permissions)
;;; mevedel-permissions.el ends here
