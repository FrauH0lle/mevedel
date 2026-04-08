;;; mevedel-permissions.el -- Unified permission system -*- lexical-binding: t -*-

;;; Commentary:

;; Unified permission decision function for all mevedel tools. Replaces
;; scattered per-tool permission checks with a single 7-step decision chain:
;; deny rules -> protected paths -> tool check-permission -> allow rules -> mode
;; -> default ask.

;;; Code:

(eval-when-compile (require 'cl-lib))

;; `mevedel-structs'
(declare-function mevedel-session-permission-rules "mevedel-structs" (cl-x) t)
(declare-function mevedel-session--create "mevedel-structs" (&rest args) t)
(declare-function mevedel-workspace-state-dir "mevedel-structs" (workspace))

;; setf expander for session struct
(eval-when-compile
  (require 'mevedel-structs))

;; `mevedel-tool-registry'
(declare-function mevedel-tool-check-permission "mevedel-tool-registry" (cl-x) t)
(declare-function mevedel-tool-get-path "mevedel-tool-registry" (cl-x) t)
(declare-function mevedel-tool-read-only-p "mevedel-tool-registry" (cl-x) t)


;;
;;; Customization

(defcustom mevedel-permission-rules nil
  "Path-based permission rules for tools.

Each entry is a list: (TOOL-NAME &key :path PATTERN :action ACTION)

TOOL-NAME is a string matching a tool name (e.g., \"Read\", \"Edit\"),
or \"*\" to match all tools.

PATTERN is a file glob pattern (optional). When omitted, the rule
matches the tool regardless of path.

ACTION is one of: `allow', `deny', or `ask'.

Rules are checked in order. Later entries override earlier ones for the
same tool/path combination. Deny rules always take precedence over allow
rules.

Example:
  ((\"Read\" :action allow)
   (\"Edit\" :path \"~/projects/*\" :action allow)
   (\"Write\" :path \"~/.ssh/*\" :action deny))"
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

  `default'      - Prompt for edits and bash commands
  `accept-edits' - Auto-approve file changes, prompt for bash
  `plan'         - Deny non-read-only tools
  `trust-all'    - Skip all prompts (except protected paths and
                   dangerous commands)"
  :type '(choice
          (const :tag "Default -- prompt for edits and bash" default)
          (const :tag "Accept Edits -- auto-approve file changes" accept-edits)
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

(defun mevedel-permission--find-rules (rules tool-name &optional path)
  "Find all matching rules in RULES for TOOL-NAME and optional PATH.

RULES is a list in the format of `mevedel-permission-rules'. Returns a
list of matching rules in order (later entries = higher priority)."
  (let ((matches nil))
    (dolist (rule rules)
      (let ((rule-tool (car rule))
            (rule-plist (cdr rule)))
        (when (or (equal rule-tool "*")
                  (equal rule-tool tool-name))
          (let ((rule-path (plist-get rule-plist :path)))
            (if rule-path
                ;; Path rule: must match
                (when (and path (mevedel-permission--match-path-pattern path rule-path))
                  (push rule matches))
              ;; No path: matches unconditionally
              (push rule matches))))))
    (nreverse matches)))

(defun mevedel-permission--rules-action (rules tool-name &optional path)
  "Determine the effective action from RULES for TOOL-NAME and PATH.

Path-specific rules take precedence over general rules. Within each
group, deny > ask > allow. Returns `allow', `deny', `ask', or nil if no
rules match."
  (let ((matching (mevedel-permission--find-rules rules tool-name path))
        (path-deny nil) (path-ask nil) (path-allow nil)
        (gen-deny nil) (gen-ask nil) (gen-allow nil))
    (dolist (rule matching)
      (let ((has-path (plist-get (cdr rule) :path)))
        (pcase (plist-get (cdr rule) :action)
          ('deny (if has-path (setq path-deny t) (setq gen-deny t)))
          ('ask (if has-path (setq path-ask t) (setq gen-ask t)))
          ('allow (if has-path (setq path-allow t) (setq gen-allow t))))))
    (cond
     ;; Path-specific rules take precedence
     (path-deny 'deny)
     (path-ask 'ask)
     (path-allow 'allow)
     ;; General rules
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
    ('accept-edits (if read-only-p 'allow 'allow))
    ('plan (if read-only-p 'allow 'deny))
    ('default (if read-only-p 'allow 'ask))
    ;; Unknown mode: fall through to ask
    (_ 'ask)))


;;
;;; Decision chain

(cl-defun mevedel-check-permission (tool-name
                                    &key tool-struct path content
                                    session-rules mode)
  "Check permission for TOOL-NAME to operate on PATH with CONTENT.

TOOL-STRUCT is the `mevedel-tool' struct (nil for unknown tools).
PATH is the file path being accessed (nil for non-path tools).
CONTENT is tool-specific content (e.g., bash command string).
SESSION-RULES is a list of session-scoped permission rules.
MODE is the permission mode (defaults to `mevedel-permission-mode').

Returns `allow', `deny', or `ask'.

The 7-step decision chain:
  1. Extract path via get-path if tool-struct provided and path is nil
  2. Check deny rules (defcustom + session) -> deny
  3. Check protected paths -> ask
  4. Call tool check-permission if present -> use result or continue
  5. Check allow rules (defcustom + session) -> allow
  6. Check mode -> allow/ask/deny per mode
  7. Default: ask"
  (let ((mode (or mode mevedel-permission-mode))
        (all-rules (append mevedel-permission-rules session-rules))
        (read-only-p (when tool-struct (mevedel-tool-read-only-p tool-struct))))

    ;; Step 1: Extract path from tool-struct if not provided
    (when (and (not path) tool-struct)
      (when-let* ((get-path-fn (mevedel-tool-get-path tool-struct)))
        (when content
          (setq path (ignore-errors (funcall get-path-fn content))))))

    ;; Step 2: Check deny rules
    (when (eq (mevedel-permission--rules-action all-rules tool-name path) 'deny)
      (cl-return-from mevedel-check-permission 'deny))

    ;; Step 3: Check protected paths
    (when (mevedel-permission--path-protected-p path)
      (cl-return-from mevedel-check-permission 'ask))

    ;; Step 4: Call tool's check-permission if present
    (when tool-struct
      (when-let* ((check-fn (mevedel-tool-check-permission tool-struct)))
        (let ((result (ignore-errors (funcall check-fn tool-struct content))))
          (when result
            (cl-return-from mevedel-check-permission result)))))

    ;; Step 5: Check allow rules
    (when (eq (mevedel-permission--rules-action all-rules tool-name path) 'allow)
      (cl-return-from mevedel-check-permission 'allow))

    ;; Step 6: Check mode
    (let ((mode-result (mevedel-permission--mode-decision mode read-only-p)))
      (unless (eq mode-result 'ask)
        (cl-return-from mevedel-check-permission mode-result)))

    ;; Step 7: Default
    'ask))


;;
;;; Session rule storage

(defun mevedel-permission--add-session-rule (session tool-name action &optional path)
  "Add a permission rule to SESSION's rule list.

TOOL-NAME is the tool name string. ACTION is `allow' or `deny'. Optional
PATH scopes the rule to a file pattern."
  (let ((rule (if path
                  (list tool-name :path path :action action)
                (list tool-name :action action))))
    (setf (mevedel-session-permission-rules session)
          (append (mevedel-session-permission-rules session)
                  (list rule)))))


;;
;;; Persistent rule storage

(defun mevedel-permission--persistent-file (workspace)
  "Return the path to WORKSPACE's persistent permission rules file."
  (file-name-concat (mevedel-workspace-state-dir workspace)
                     "permissions.el"))

(defun mevedel-permission--load-persistent-rules (workspace)
  "Load persistent permission rules for WORKSPACE.

Reads the rules file and returns a list in `mevedel-permission-rules'
format. Returns nil if the file does not exist or is unreadable."
  (let ((file (mevedel-permission--persistent-file workspace)))
    (when (file-readable-p file)
      (condition-case nil
          (with-temp-buffer
            (insert-file-contents file)
            (read (current-buffer)))
        (error nil)))))

(defun mevedel-permission--save-persistent-rule (workspace tool-name action
                                                           &optional path)
  "Append a permission rule to WORKSPACE's persistent rules file.

TOOL-NAME, ACTION, and optional PATH define the rule. The file is
created if it does not exist."
  (let* ((file (mevedel-permission--persistent-file workspace))
         (existing (mevedel-permission--load-persistent-rules workspace))
         (rule (if path
                   (list tool-name :path path :action action)
                 (list tool-name :action action)))
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

(defun mevedel-permission--apply-prompt-result (result tool-name
                                                       &optional session
                                                       workspace path)
  "Dispatch a permission prompt RESULT to the correct storage.

RESULT is one of:
  `allow-once'    -- return `allow', no storage
  `allow-session' -- add session allow rule, return `allow'
  `always-allow'  -- save persistent allow rule, return `allow'
  `deny-once'     -- return `deny', no storage
  `deny-session'  -- add session deny rule, return `deny'

TOOL-NAME is the tool being permitted. SESSION and WORKSPACE are used
for rule storage. PATH optionally scopes the rule."
  (pcase result
    ('allow-once 'allow)
    ('allow-session
     (when session
       (mevedel-permission--add-session-rule session tool-name 'allow path))
     'allow)
    ('always-allow
     (when workspace
       (mevedel-permission--save-persistent-rule workspace tool-name 'allow path))
     (when session
       (mevedel-permission--add-session-rule session tool-name 'allow path))
     'allow)
    ('deny-once 'deny)
    ('deny-session
     (when session
       (mevedel-permission--add-session-rule session tool-name 'deny path))
     'deny)
    (_ 'deny)))

(provide 'mevedel-permissions)
;;; mevedel-permissions.el ends here
