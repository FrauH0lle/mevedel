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
  :pattern GLOB  - command string (supports *)
                   Used by Bash (e.g., \"ls *\", \"git log*\").
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
   (\"Bash\" :pattern \"rm *\" :action deny)
   (\"WebFetch\" :domain \"*.example.com\" :action allow)
   (\"Agent\" :name \"explore\" :action allow))"
  :type '(repeat sexp)
  :group 'mevedel)

(defcustom mevedel-protected-paths
  '("**/.git/**" "~/.ssh/**" "~/.gnupg/**")
  "Path patterns that always require confirmation.

Even `trust-all' mode prompts for these paths. Each entry is a glob
pattern matched against the full expanded path."
  :type '(repeat string)
  :group 'mevedel)

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
`default' it shows an interactive overlay."
  :type '(choice
          (const :tag "Default -- prompt, interactive diff preview" default)
          (const :tag "Accept Edits -- auto-apply diff previews" accept-edits)
          (const :tag "Plan -- read-only tools only" plan)
          (const :tag "Trust All -- skip all prompts (except dangerous)" trust-all))
  :group 'mevedel)


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
           (i 0)
           (len (length expanded))
           (parts (list "\\`")))
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
                       (expand-file-name path)))))

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
      ((or :pattern :domain :name)
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
      (cl-some (lambda (pattern)
                 (mevedel-permission--match-path-pattern expanded pattern))
               mevedel-protected-paths))))


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
;;; Decision chain

(cl-defun mevedel-check-permission (tool-name
                                    &key tool-struct path pattern domain name
                                    content session-rules mode
                                    workspace-root)
  "Check permission for TOOL-NAME to operate on PATH with CONTENT.

TOOL-STRUCT is the `mevedel-tool' struct (nil for unknown tools).
PATH is the file path being accessed (nil for non-path tools).
PATTERN is a command string to match against `:pattern' rules.
DOMAIN is a host string to match against `:domain' rules.
NAME is a match name to test against `:name' rules.
CONTENT is tool-specific content (e.g., bash command string).
SESSION-RULES is a list of session-scoped permission rules.
MODE is the permission mode (defaults to `mevedel-permission-mode').
WORKSPACE-ROOT is the workspace root directory (nil if unknown).

Returns `allow', `deny', or `ask'.

The 9-step decision chain:
  1. Extract specifier values via tool-struct getters when missing
  2. Check deny rules (defcustom + session) -> deny
  3. Check protected paths -> ask
  4. Call tool check-permission if present -> use result or continue
  5. Check allow rules (defcustom + session) -> allow
  6. Check workspace root (implicit allow for paths inside) -> allow
  7. If path is set and outside workspace root -> ask (workspace boundary)
  8. Check mode -> allow/ask/deny per mode
  9. Default: ask"
  (let ((mode (or mode mevedel-permission-mode))
        (all-rules (append mevedel-permission-rules session-rules))
        (read-only-p (when tool-struct (mevedel-tool-read-only-p tool-struct))))

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

    ;; Step 2: Check deny rules
    (when (eq (mevedel-permission--rules-action
               all-rules tool-name
               :path path :pattern pattern :domain domain :name name)
              'deny)
      (cl-return-from mevedel-check-permission 'deny))

    ;; Step 3: Check protected paths
    (when (mevedel-permission--path-protected-p path)
      (cl-return-from mevedel-check-permission 'ask))

    ;; Step 4: Call tool's check-permission if present.
    ;; Let mevedel-pipeline-error (incl. permission-denied) propagate so
    ;; the pipeline runner can relay feedback to the LLM.  Only swallow
    ;; unexpected errors from check-permission functions.
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

    ;; Step 5: Check allow rules
    (when (eq (mevedel-permission--rules-action
               all-rules tool-name
               :path path :pattern pattern :domain domain :name name)
              'allow)
      (cl-return-from mevedel-check-permission 'allow))

    ;; Step 6: Workspace root acts as implicit allow for all tools
    (when (and path workspace-root)
      (let ((abs-path (expand-file-name path))
            (abs-root (file-name-as-directory (expand-file-name workspace-root))))
        (when (string-prefix-p abs-root abs-path)
          (cl-return-from mevedel-check-permission 'allow))))

    ;; Step 7: Path outside workspace root with no covering rule -> ask
    ;; This ensures read-only tools outside the workspace still prompt.
    (when path
      (cl-return-from mevedel-check-permission 'ask))

    ;; Step 8: Check mode (for non-path tools)
    (let ((mode-result (mevedel-permission--mode-decision mode read-only-p)))
      (unless (eq mode-result 'ask)
        (cl-return-from mevedel-check-permission mode-result)))

    ;; Step 9: Default
    'ask))


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
instead, leaving PATH nil."
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
              (when workspace
                (mevedel-permission--save-persistent-rule
                 workspace tool-name action path
                 :spec-key spec-key :spec-value spec-value))))
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
