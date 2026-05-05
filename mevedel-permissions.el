;;; mevedel-permissions.el -- Unified permission system -*- lexical-binding: t -*-

;;; Commentary:

;; Unified permission decision function for all mevedel tools. Replaces
;; scattered per-tool permission checks with a single 9-step decision
;; chain: extract context -> deny rules -> protected paths -> tool
;; check-permission -> allow rules -> workspace root -> outside workspace
;; -> mode -> default ask.

;;; Code:

(eval-when-compile (require 'cl-lib))

;; `mevedel-structs'
(declare-function mevedel-session-permission-rules "mevedel-structs" (cl-x) t)
(declare-function mevedel-workspace-state-dir "mevedel-structs" (workspace))
(defvar mevedel-user-dir)

;; setf expander for session struct
(eval-when-compile
  (require 'mevedel-structs))

;; `mevedel-tool-registry'
(declare-function mevedel-tool-check-permission "mevedel-tool-registry" (cl-x) t)
(declare-function mevedel-tool-check-permission-async "mevedel-tool-registry" (cl-x) t)
(declare-function mevedel-tool-get-path "mevedel-tool-registry" (cl-x) t)
(declare-function mevedel-tool-get-pattern "mevedel-tool-registry" (cl-x) t)
(declare-function mevedel-tool-get-domain "mevedel-tool-registry" (cl-x) t)
(declare-function mevedel-tool-get-name "mevedel-tool-registry" (cl-x) t)
(declare-function mevedel-tool-read-only-p "mevedel-tool-registry" (cl-x) t)


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
rule matches against. At most one specifier is allowed per rule:

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
  "Path patterns that always require confirmation.

Even `trust-all' mode prompts for these paths. Each entry is a glob
pattern matched against the full expanded path."
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
  "Custom setter for `mevedel-permission-mode'.
Thin wrapper around `mevedel-permission--set-session-scoped' that
targets the session struct's `permission-mode' slot.  See that helper's
docstring for the full scoping contract."
  (mevedel-permission--set-session-scoped
   sym val
   (lambda (session v) (setf (mevedel-session-permission-mode session) v))))

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
  "Custom getter for `mevedel-permission-mode'.
Returns the current session's `permission-mode' slot when the call is
made from inside a session; otherwise returns the global default."
  (mevedel-permission--get-session-scoped
   sym #'mevedel-session-permission-mode))

(defcustom mevedel-permission-mode 'default
  "Current permission mode.

Controls the default permission behavior when no explicit rules match.

  `default'      - Prompt for non-read-only tools; inline diff
                   previews require interactive approval.
  `accept-edits' - Same permission semantics as `default', but inline
                   diff previews from Write/Edit/MkDir are auto-applied
                   without an interactive overlay.
  `plan'         - Deny non-read-only tools.
  `trust-all'    - Skip all prompts (except protected paths and
                   dangerous commands).

Note: at the permission layer `default' and `accept-edits' behave the
same.  The difference lives in `mevedel-preview-mode': under
`accept-edits' the preview step auto-applies the change, under
`default' it shows an interactive overlay.

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
          (const :tag "Default -- prompt, interactive diff preview" default)
          (const :tag "Accept Edits -- auto-apply diff previews" accept-edits)
          (const :tag "Plan -- read-only tools only" plan)
          (const :tag "Trust All -- skip all prompts (except dangerous)" trust-all))
  :set #'mevedel-permission-mode--set
  :get #'mevedel-permission-mode--get
  :local 'permanent
  :group 'mevedel)


;;
;;; allowed-tools parsing

(declare-function mevedel-tool-get "mevedel-tool-registry" (name &optional category))

(defun mevedel-permission--tool-specifier-key (tool-name)
  "Return the specifier keyword for TOOL-NAME, or nil when none applies.
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
        (unless (mevedel-tool-get tool-name)
          (user-error "Unknown tool in allowed-tools: %s" tool-name))
        (list tool-name :action 'allow)))
     ;; Qualified: ^Tool(VALUE)$
     ((string-match
       "\\`\\([A-Za-z][A-Za-z0-9]*\\)(\\(.*\\))\\'" entry)
      (let* ((tool-name (match-string 1 entry))
             (raw-value (match-string 2 entry))
             (value raw-value))
        (unless (mevedel-tool-get tool-name)
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
    (let* ((expanded (if (string-prefix-p "~" pattern)
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
`:pattern', `:domain', `:name') against the corresponding keyword
value.  Unqualified rules match unconditionally.  Returns a list of
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
`:name') take precedence over unqualified rules.  Within each group,
deny > ask > allow.  Returns `allow', `deny', `ask', or nil if no
rules match."
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


;;
;;; Mode decisions

(defun mevedel-permission--mode-decision (mode read-only-p)
  "Determine permission based on MODE and whether the tool is READ-ONLY-P.

Returns `allow', `deny', or `ask'."
  (pcase mode
    ('trust-all 'allow)
    ('accept-edits (if read-only-p 'allow 'ask))
    ('plan (if read-only-p 'allow 'deny))
    ('default (if read-only-p 'allow 'ask))
    ;; Unknown mode: fall through to ask
    (_ 'ask)))


;;
;;; Decision chain -- bucket-aware

(defun mevedel-permission--collect-buckets
    (invocation-rules request-rules session-rules persistent-rules)
  "Return the bucket alist used by the bucket-aware resolver.
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
  "Resolve a single BUCKET-RULES list to its action for the given context."
  (mevedel-permission--rules-action
   bucket-rules tool-name
   :path path :pattern pattern :domain domain :name name))

(defun mevedel-permission--any-deny
    (buckets tool-name path pattern domain name)
  "Return non-nil if any bucket in BUCKETS yields a `deny' action.
Buckets is the alist from `mevedel-permission--collect-buckets'."
  (cl-loop for entry in buckets
           thereis
           (eq (mevedel-permission--bucket-action
                (cdr entry) tool-name path pattern domain name)
               'deny)))

(defun mevedel-permission--first-non-nil-action
    (buckets tool-name path pattern domain name skip-keys)
  "Walk BUCKETS innermost-first, return first non-nil bucket action.
SKIP-KEYS is a list of bucket-key symbols to skip during the walk
\\=(used for the plan-mode skill-bucket suppression)."
  (cl-loop for (key . rules) in buckets
           unless (memq key skip-keys)
           for action = (mevedel-permission--bucket-action
                         rules tool-name path pattern domain name)
           when action return action))

(defun mevedel-permission--plan-mode-skip-keys (mode read-only-p)
  "Return bucket keys to suppress for the allow/ask pass under MODE.
spec: plan-mode + non-read-only tool -> skip skill buckets.
For read-only tools the plan-mode preview/permission paths converge
on allow anyway, so the suppression has no effect there."
  (when (and (eq mode 'plan)
             (not read-only-p))
    '(:invocation :request)))

(cl-defun mevedel-check-permission (tool-name
                                    &key tool-struct path pattern domain name
                                    content
                                    invocation-rules request-rules
                                    session-rules persistent-rules
                                    mode workspace-root)
  "Check permission for TOOL-NAME to operate on PATH with CONTENT.

TOOL-STRUCT is the `mevedel-tool' struct (nil for unknown tools).
PATH is the file path being accessed (nil for non-path tools).
PATTERN is a command string to match against `:pattern' rules.
DOMAIN is a host string to match against `:domain' rules.
NAME is a match name to test against `:name' rules.
CONTENT is tool-specific content (e.g., bash command string).
INVOCATION-RULES, REQUEST-RULES, SESSION-RULES, PERSISTENT-RULES are
the bucket lists.  All
default to nil for backward compatibility.
MODE is the permission mode (defaults to `mevedel-permission-mode').
WORKSPACE-ROOT is the workspace root directory (nil if unknown).

Returns `allow', `deny', or `ask'.

The 9-step decision chain:
  1. Extract specifier values via tool-struct getters when missing
  2. Pass 1 -- absolute decisions across all buckets:
       any bucket yields `deny' -> deny;
       protected path -> ask
  3. (folded into pass 1)
  4. Call tool check-permission if present -> use result or continue
  5. Pass 2 -- allow/ask resolution innermost-first:
       invocation -> request -> session -> persistent -> defcustom.
       Plan-mode + non-read-only tool: skill buckets suppressed.
  6. Workspace root -> implicit allow for paths inside
  7. Outside workspace root -> ask (workspace boundary)
  8. Mode decision
  9. Default: ask"
  (let* ((mode (or mode mevedel-permission-mode))
         (read-only-p (when tool-struct (mevedel-tool-read-only-p tool-struct)))
         (buckets (mevedel-permission--collect-buckets
                   invocation-rules request-rules
                   session-rules persistent-rules)))

    ;; Step 1: Extract specifier values via tool-struct getters
    (when (and tool-struct content)
      (cl-flet ((extract (getter current)
                  (or current
                      (when-let* ((fn (funcall getter tool-struct)))
                        (ignore-errors (funcall fn content))))))
        (setq path    (extract #'mevedel-tool-get-path    path)
              pattern (extract #'mevedel-tool-get-pattern pattern)
              domain  (extract #'mevedel-tool-get-domain  domain)
              name    (extract #'mevedel-tool-get-name    name))))

    ;; Step 2: Pass 1 -- any bucket says deny.
    (when (mevedel-permission--any-deny
           buckets tool-name path pattern domain name)
      (cl-return-from mevedel-check-permission 'deny))

    ;; Step 3: Protected paths.
    (when (mevedel-permission--path-protected-p path)
      (cl-return-from mevedel-check-permission 'ask))

    ;; Step 4: Tool's check-permission slot.
    (when tool-struct
      (when-let* ((check-fn (mevedel-tool-check-permission tool-struct)))
        (let ((result (condition-case err
                          (funcall check-fn tool-struct content)
                        (mevedel-pipeline-error (signal (car err) (cdr err)))
                        (error
                         (message "mevedel: check-permission error: %S" err)
                         nil))))
          (when result
            (cl-return-from mevedel-check-permission result)))))

    ;; Steps 5-9 share one tail with `mevedel-check-permission-async'.
    (mevedel-check-permission--tail
     tool-name buckets path pattern domain name
     workspace-root mode read-only-p)))


;;
;;; Async decision chain

(cl-defun mevedel-check-permission-async (tool-name cont
                                                    &key tool-struct path
                                                    pattern domain name
                                                    content
                                                    invocation-rules
                                                    request-rules
                                                    session-rules
                                                    persistent-rules
                                                    mode workspace-root)
  "Async variant of `mevedel-check-permission'.

Invokes CONT with one of:
  `allow' / `deny' / `ask'      -- final decision from the chain
  (deny . REASON)               -- slot-emitted or sync-slot-adapter denial
  (feedback . TEXT)             -- slot-emitted feedback denial
  `aborted'                     -- slot teardown
  nil                           -- slot abstained; caller resumes step 5+

Steps 1-3 and 5-9 run synchronously just like `mevedel-check-permission'.
Step 4 may run async when the tool defines `:check-permission-async'; the
sync-slot adapter preserves the denial REASON captured from a
`mevedel-permission-denied' signal so `(deny . REASON)' reaches CONT.

Bucket-aware per spec; see `mevedel-check-permission' for the
keyword-arg semantics."
  (let* ((mode (or mode mevedel-permission-mode))
         (read-only-p (when tool-struct
                        (mevedel-tool-read-only-p tool-struct)))
         (buckets (mevedel-permission--collect-buckets
                   invocation-rules request-rules
                   session-rules persistent-rules)))
    ;; Step 1: extract specifier values via tool-struct getters.
    (when (and tool-struct content)
      (cl-flet ((extract (getter current)
                  (or current
                      (when-let* ((fn (funcall getter tool-struct)))
                        (ignore-errors (funcall fn content))))))
        (setq path    (extract #'mevedel-tool-get-path    path)
              pattern (extract #'mevedel-tool-get-pattern pattern)
              domain  (extract #'mevedel-tool-get-domain  domain)
              name    (extract #'mevedel-tool-get-name    name))))
    (let ((resume-from-5
           (lambda ()
             ;; Step 5-9 are pure sync; funnel them through the existing
             ;; chain by running the tail of `mevedel-check-permission'
             ;; with the already-extracted specifier values.
             (funcall
              cont
              (mevedel-check-permission--tail
               tool-name buckets path pattern domain name
               workspace-root mode read-only-p)))))
      (cond
       ;; Step 2: any bucket says deny.
       ((mevedel-permission--any-deny
         buckets tool-name path pattern domain name)
        (funcall cont 'deny))
       ;; Step 3: protected path.
       ((mevedel-permission--path-protected-p path)
        (funcall cont 'ask))
       ;; Step 4: tool slot (async preferred, sync fallback).
       ((and tool-struct
             (mevedel-tool-check-permission-async tool-struct))
        (funcall (mevedel-tool-check-permission-async tool-struct)
                 tool-struct content
                 (lambda (slot-result)
                   (if (null slot-result)
                       (funcall resume-from-5)
                     (funcall cont slot-result)))))
       ((and tool-struct
             (mevedel-tool-check-permission tool-struct))
        (let ((slot-result
               (condition-case err
                   (funcall (mevedel-tool-check-permission tool-struct)
                            tool-struct content)
                 (mevedel-permission-denied
                  (cons 'deny (cadr err)))
                 (error
                  (message "mevedel: check-permission error: %S" err)
                  nil))))
          (if (null slot-result)
              (funcall resume-from-5)
            (funcall cont slot-result))))
       (t (funcall resume-from-5))))))

(defun mevedel-check-permission--tail
    (tool-name buckets path pattern domain name
               workspace-root mode read-only-p)
  "Run steps 5-9 of the permission chain and return the decision.

Factored out so both the sync and async entry points can share the
tail.  Specifier extraction (step 1), the deny / protected-path
branches (steps 2-3), and the tool-slot branch (step 4) are handled
by the callers -- this function presumes they already ran.  BUCKETS
is the bucket alist from `mevedel-permission--collect-buckets'."
  (let ((skip-keys
         (mevedel-permission--plan-mode-skip-keys mode read-only-p)))
    (cond
     ;; Step 5: pass 2 -- allow/ask innermost-first across buckets.
     ((let ((action (mevedel-permission--first-non-nil-action
                     buckets tool-name path pattern domain name skip-keys)))
        (cond
         ((eq action 'allow) 'allow)
         ((eq action 'ask) 'ask))))
     ;; Step 6: workspace root implicit allow.
     ((mevedel-permission--path-in-workspace-p path workspace-root)
      'allow)
     ;; Step 7: path outside workspace with no covering rule.
     (path 'ask)
     ;; Step 8: mode decision.
     (t (let ((mode-result (mevedel-permission--mode-decision
                            mode read-only-p)))
          (if (eq mode-result 'ask)
              ;; Step 9: default.
              'ask
            mode-result))))))


;;
;;; Session rule storage

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

TOOL-NAME is the tool name string. ACTION is `allow' or `deny'.

Positional PATH is retained for existing call sites; when supplied it is
equivalent to SPEC-KEY `:path' with that value.  Callers specifying
another specifier should pass SPEC-KEY (e.g. `:pattern') and SPEC-VALUE
instead, leaving PATH nil.

Mutates SESSION's `permission-rules' slot via `setf' -- this is a
**by-reference** write.  Sub-agents share the parent session by
reference (see `mevedel-agent-exec--allocate-agent-buffer'), so a
rule recorded inside any sub-agent's permission prompt
(\"allow-session\", \"deny-session\") immediately applies to the
parent and to every other live sub-agent sharing the same session
struct.  This is a deliberate contract, not an accident of the
buffer-local plumbing."
  (let* ((key (or spec-key (and path :path)))
         (value (or spec-value path))
         (rule (mevedel-permission--build-rule tool-name action key value)))
    (setf (mevedel-session-permission-rules session)
          (append (mevedel-session-permission-rules session)
                  (list rule)))))


;;
;;; Persistent rule storage

(defun mevedel-permission--persistent-file (workspace)
  "Return the path to WORKSPACE's persistent permission rules file."
  (file-name-concat (mevedel-workspace-state-dir workspace)
                     "permissions.el"))

(defun mevedel-permission--read-rules-file (file)
  "Read permission rules from FILE.

Returns a list in `mevedel-permission-rules' format, or nil if FILE
does not exist or is unreadable."
  (when (file-readable-p file)
    (condition-case nil
        (with-temp-buffer
          (insert-file-contents file)
          (read (current-buffer)))
      (error nil))))

(defun mevedel-permission--load-persistent-rules (workspace)
  "Load persistent permission rules for WORKSPACE.

Loads rules from both the global directory (`mevedel-user-dir') and the
project directory (WORKSPACE's .mevedel/).  Global rules are loaded
first, project rules appended after so they take precedence.

Returns a merged list in `mevedel-permission-rules' format."
  (let ((global-file (file-name-concat mevedel-user-dir "permissions.el"))
        (project-file (mevedel-permission--persistent-file workspace)))
    (append (mevedel-permission--read-rules-file global-file)
            (mevedel-permission--read-rules-file project-file))))

(cl-defun mevedel-permission--save-persistent-rule
    (workspace tool-name action &optional path &key spec-key spec-value)
  "Append a permission rule to WORKSPACE's persistent rules file.

TOOL-NAME and ACTION define the rule.  Positional PATH is equivalent
to SPEC-KEY `:path'.  SPEC-KEY/SPEC-VALUE let callers store rules
qualified by any specifier (`:path', `:pattern', `:domain', `:name').
The file is created if it does not exist."
  (let* ((file (mevedel-permission--persistent-file workspace))
         (existing (mevedel-permission--read-rules-file file))
         (key (or spec-key (and path :path)))
         (value (or spec-value path))
         (rule (mevedel-permission--build-rule tool-name action key value))
         (updated (append existing (list rule)))
         (dir (file-name-directory file)))
    (unless (file-directory-p dir)
      (make-directory dir t))
    (with-temp-file file
      (insert ";; Mevedel persistent permission rules\n")
      (insert ";; Auto-generated, safe to edit\n\n")
      (pp updated (current-buffer)))))


;;
;;; Prompt result dispatch

(cl-defun mevedel-permission--apply-prompt-result
    (result tool-name &optional session workspace path
            &key spec-key spec-value)
  "Dispatch a permission prompt RESULT to the correct storage.

RESULT is one of:
  `allow-once'    -- return `allow', no storage
  `allow-session' -- add session allow rule, return `allow'
  `always-allow'  -- save persistent allow rule, return `allow'
  `deny-once'     -- return `deny', no storage
  `deny-session'  -- add session deny rule, return `deny'

TOOL-NAME is the tool being permitted.  SESSION and WORKSPACE are used
for rule storage.  Positional PATH scopes the rule to a file path (kept
for call sites that already pass it).  SPEC-KEY/SPEC-VALUE allow
scoping by any other specifier (`:pattern', `:domain', `:name')."
  (cl-flet ((session-rule (action)
              (when session
                (mevedel-permission--add-session-rule
                 session tool-name action path
                 :spec-key spec-key :spec-value spec-value)))
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
      ('allow-session (session-rule 'allow) 'allow)
      ('always-allow
       (persistent-rule 'allow)
       (session-rule 'allow)
       'allow)
      ('deny-once 'deny)
      ('deny-session (session-rule 'deny) 'deny)
      (_ 'deny))))

(provide 'mevedel-permissions)
;;; mevedel-permissions.el ends here
