;;; mevedel-hooks.el -- Project hook execution -*- lexical-binding: t -*-

;;; Commentary:

;; Hook execution subsystem for mevedel lifecycle and tool events.
;;
;; Hooks are configured as Lisp data or JSON in user / project files,
;; plus Emacs-native abnormal hook variables for package integration.
;; Command hooks receive JSON on stdin and return structured decisions
;; on stdout; Elisp hooks receive the event plist directly.

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(require 'json)
(require 'subr-x)
(require 'mevedel-structs)

(declare-function mevedel-version "mevedel" (&optional here message))
(declare-function mevedel-tool-name "mevedel-tool-registry" (cl-x) t)
(declare-function mevedel-agent-invocation-hook-rules
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-p "mevedel-agents" (cl-x))
(declare-function mevedel-agent-invocation-agent-id
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-session-working-directory "mevedel-structs" (cl-x) t)
(declare-function mevedel-view--update-spinner "mevedel-view" (status))
(declare-function mevedel-view--spinner-active-p "mevedel-view" ())
(defvar gptel-model)
(defvar read-eval)
(defvar mevedel--view-buffer)


;;
;;; Customization

(defcustom mevedel-hook-rules nil
  "User-level declarative hook rules.

Rules use the same shape as `hooks.el' files:

  ((PreToolUse
    ((:matcher \"Bash\"
      :hooks ((:type command :command \"./hook.sh\"))))))

This defcustom is trusted user configuration and is always loaded."
  :type 'sexp
  :group 'mevedel)

(defcustom mevedel-hooks-command-timeout 30
  "Default timeout in seconds for command hooks."
  :type 'number
  :group 'mevedel)

(defcustom mevedel-hooks-command-timeout-max 120
  "Maximum timeout in seconds for command hooks.

Per-hook `:timeout' values are clamped to this value.  Set to nil to
disable the cap."
  :type '(choice (number :tag "Seconds")
                 (const :tag "No cap" nil))
  :group 'mevedel)

(defcustom mevedel-hooks-require-project-trust t
  "When non-nil, ignore project hook files until explicitly trusted.

User files under `mevedel-user-dir' and `mevedel-hook-rules' are always
trusted.  Project files are trusted by file hash via
`mevedel-hooks-trust-project'."
  :type 'boolean
  :group 'mevedel)

(defcustom mevedel-hooks-log-limit 200
  "Maximum number of hook log entries kept on a session.
Nil keeps all entries."
  :type '(choice (integer :tag "Entries")
                 (const :tag "Unlimited" nil))
  :group 'mevedel)

(defcustom mevedel-hooks-persist-log t
  "When non-nil, append hook log entries to persisted session directories."
  :type 'boolean
  :group 'mevedel)

(defcustom mevedel-hooks-slow-threshold 1.5
  "Seconds before a running hook event is surfaced to the user.
Nil disables slow-hook surfacing."
  :type '(choice (number :tag "Seconds")
                 (const :tag "Disabled" nil))
  :group 'mevedel)


;;
;;; Emacs API hooks

(defvar mevedel-session-start-hook nil
  "Normal hook run after a mevedel session starts.
Runs in the chat data buffer.")

(defvar mevedel-session-end-hook nil
  "Normal hook run when a mevedel session ends.
Runs in the chat data buffer.")

(defvar mevedel-user-prompt-submit-functions nil
  "Abnormal hook functions run before a user prompt is sent.
Each function receives one event plist and may return a hook decision
plist.")

(defvar mevedel-user-prompt-expansion-functions nil
  "Abnormal hook functions run after a user slash prompt expands.
Each function receives one event plist and may return a hook decision
plist.")

(defvar mevedel-pre-tool-use-functions nil
  "Abnormal hook functions run before a tool executes.
Each function receives one event plist and may return a hook decision
plist.")

(defvar mevedel-permission-request-functions nil
  "Abnormal hook functions run before a permission prompt is shown.
Each function receives one event plist and may return a hook decision
plist.")

(defvar mevedel-permission-denied-functions nil
  "Abnormal hook functions run after a permission denial.
Each function receives one event plist and may return a hook decision
plist.  Denial hooks are observational; they cannot undo the denial.")

(defvar mevedel-post-tool-use-functions nil
  "Abnormal hook functions run after a tool produces its final result.
Each function receives one event plist and may return a hook decision
plist.")

(defvar mevedel-pre-compact-functions nil
  "Abnormal hook functions run before conversation compaction.
Each function receives one event plist and may return a hook decision
plist.")

(defvar mevedel-post-compact-functions nil
  "Abnormal hook functions run after conversation compaction.
Each function receives one event plist and may return a hook decision
plist.")

(defvar mevedel-subagent-start-functions nil
  "Abnormal hook functions run before a sub-agent starts.
Each function receives one event plist and may return a hook decision
plist.")

(defvar mevedel-subagent-stop-functions nil
  "Abnormal hook functions run after a sub-agent reaches terminal state.
Each function receives one event plist and may return a hook decision
plist.")

(defvar mevedel-stop-functions nil
  "Abnormal hook functions run after a top-level turn completes.
Each function receives one event plist and may return a hook decision
plist.")

(defvar mevedel-stop-failure-functions nil
  "Abnormal hook functions run after a top-level turn fails.
Each function receives one event plist and may return a hook decision
plist.")


;;
;;; Constants

(defconst mevedel-hooks--events
  '(SessionStart UserPromptSubmit UserPromptExpansion
                 PreToolUse PermissionRequest PermissionDenied
                 PostToolUse PostToolUseFailure PreCompact PostCompact
                 SubagentStart SubagentStop Stop StopFailure SessionEnd)
  "Known hook event names.")

(defconst mevedel-hooks--function-hook-alist
  '((UserPromptSubmit . mevedel-user-prompt-submit-functions)
    (UserPromptExpansion . mevedel-user-prompt-expansion-functions)
    (PreToolUse . mevedel-pre-tool-use-functions)
    (PermissionRequest . mevedel-permission-request-functions)
    (PermissionDenied . mevedel-permission-denied-functions)
    (PostToolUse . mevedel-post-tool-use-functions)
    (PostToolUseFailure . mevedel-post-tool-use-functions)
    (PreCompact . mevedel-pre-compact-functions)
    (PostCompact . mevedel-post-compact-functions)
    (SubagentStart . mevedel-subagent-start-functions)
    (SubagentStop . mevedel-subagent-stop-functions)
    (Stop . mevedel-stop-functions)
    (StopFailure . mevedel-stop-failure-functions))
  "Mapping from hook event names to Emacs abnormal hook variables.")


;;
;;; Utility helpers

(defun mevedel-hooks--event-symbol (event)
  "Return canonical symbol for EVENT, or nil when unknown."
  (let ((sym (cond
              ((symbolp event) event)
              ((stringp event) (intern event))
              (t nil))))
    (and (memq sym mevedel-hooks--events) sym)))

(defun mevedel-hooks--scope-event (event scope)
  "Return EVENT adjusted for hook declaration SCOPE."
  (if (and (memq scope '(agent skill-fork))
           (eq event 'Stop))
      'SubagentStop
    event))

(defun mevedel-hooks--alist-get-string (key alist)
  "Return string KEY's value in ALIST using `equal'."
  (or (cdr (assoc key alist))
      (cdr (assq (intern key) alist))))

(defun mevedel-hooks--truthy-p (value)
  "Return non-nil when VALUE is logically true for hook config."
  (and value (not (eq value :json-false))))

(defun mevedel-hooks--keyword-for-json-key (key)
  "Convert JSON object KEY to a Lisp keyword."
  (let ((key (if (symbolp key) (symbol-name key) key))
        (case-fold-search nil))
    (intern (concat ":"
                    (downcase
                     (replace-regexp-in-string
                      "\\([a-z0-9]\\)\\([A-Z]\\)" "\\1-\\2"
                      (replace-regexp-in-string "_" "-" key)))))))

(defun mevedel-hooks--json-value-to-lisp (value)
  "Convert parsed JSON VALUE into Lisp hook data."
  (cond
   ((vectorp value)
    (mapcar #'mevedel-hooks--json-value-to-lisp (append value nil)))
   ((and (listp value)
         (consp (car-safe value))
         (or (stringp (caar value))
             (symbolp (caar value))))
    (let (plist)
      (dolist (entry value plist)
        (setq plist
              (plist-put plist
                         (mevedel-hooks--keyword-for-json-key (car entry))
                         (mevedel-hooks--json-value-to-lisp
                          (cdr entry)))))))
   ((listp value)
    (mapcar #'mevedel-hooks--json-value-to-lisp value))
   ((eq value :json-false) nil)
   (t value)))

(defun mevedel-hooks--json-tool-input-to-lisp (value)
  "Convert JSON tool input VALUE while preserving arg-key underscores."
  (cond
   ((vectorp value)
    (mapcar #'mevedel-hooks--json-tool-input-to-lisp (append value nil)))
   ((and (listp value)
         (consp (car-safe value))
         (or (stringp (caar value))
             (symbolp (caar value))))
    (let (plist)
      (dolist (entry value plist)
        (let ((key (if (symbolp (car entry))
                       (symbol-name (car entry))
                     (car entry))))
          (setq plist
                (plist-put plist
                           (intern (concat ":" key))
                           (mevedel-hooks--json-tool-input-to-lisp
                            (cdr entry))))))))
   ((listp value)
    (mapcar #'mevedel-hooks--json-tool-input-to-lisp value))
   ((eq value :json-false) nil)
   (t value)))

(defun mevedel-hooks--coerce-symbol (value)
  "Coerce VALUE to a symbol when possible."
  (cond
   ((symbolp value) value)
   ((stringp value) (intern value))
   (t value)))

(defun mevedel-hooks--normalize-permission-decision (value)
  "Normalize VALUE to `allow', `ask', `deny', or nil."
  (let ((sym (mevedel-hooks--coerce-symbol value)))
    (and (memq sym '(allow ask deny)) sym)))

(defun mevedel-hooks--permission-priority (decision)
  "Return merge priority for permission DECISION."
  (pcase decision
    ('deny 3)
    ('ask 2)
    ('allow 1)
    (_ 0)))

(defun mevedel-hooks--command-timeout (handler)
  "Return effective timeout for command HANDLER."
  (let ((timeout (plist-get handler :timeout)))
    (unless (numberp timeout)
      (setq timeout mevedel-hooks-command-timeout))
    (when (numberp timeout)
      (if mevedel-hooks-command-timeout-max
          (max 1 (min timeout mevedel-hooks-command-timeout-max))
        (max 1 timeout)))))

(defun mevedel-hooks--plist-put-append (plist key values)
  "Append VALUES to list at KEY in PLIST."
  (plist-put plist key (append (plist-get plist key) values)))


;;
;;; Normalization

(defun mevedel-hooks--normalize-handler (handler)
  "Normalize one hook HANDLER plist.
Returns a plist or nil when HANDLER is invalid."
  (when (and (listp handler) (keywordp (car-safe handler)))
    (let* ((type (mevedel-hooks--coerce-symbol (plist-get handler :type)))
           (normalized (copy-sequence handler)))
      (setq normalized (plist-put normalized :type type))
      (when (plist-member normalized :fail-closed)
        (setq normalized
              (plist-put normalized :fail-closed
                         (mevedel-hooks--truthy-p
                          (plist-get normalized :fail-closed)))))
      (pcase type
        ('command
         (and (stringp (plist-get normalized :command)) normalized))
        ('elisp
         (let ((fn (plist-get normalized :function)))
           (cond
            ((symbolp fn) normalized)
            ((stringp fn) (plist-put normalized :function (intern fn)))
            (t nil))))
        (_ nil)))))

(defun mevedel-hooks--normalize-group (group)
  "Normalize one matcher GROUP plist."
  (when (and (listp group) (keywordp (car-safe group)))
    (let* ((hooks (plist-get group :hooks))
           (hooks-list (if (and (listp hooks)
                                (or (null hooks)
                                    (listp (car-safe hooks))))
                           hooks
                         (and hooks (list hooks))))
           (normalized-hooks
            (delq nil (mapcar #'mevedel-hooks--normalize-handler
                              hooks-list))))
      (when normalized-hooks
        (list :matcher (plist-get group :matcher)
              :hooks normalized-hooks)))))

(defun mevedel-hooks-normalize-rules (rules &optional scope)
  "Normalize hook RULES into canonical event alist form.
SCOPE may be `agent' or `skill-fork', where local `Stop' hooks are
treated as `SubagentStop'."
  (let (normalized)
    (dolist (entry rules (nreverse normalized))
      (when (and (consp entry)
                 (mevedel-hooks--event-symbol (car entry)))
        (let* ((event (mevedel-hooks--scope-event
                       (mevedel-hooks--event-symbol (car entry))
                       scope))
               (groups (cdr entry))
               (groups (if (and (= (length groups) 1)
                                (listp (car groups))
                                (not (keywordp (caar groups))))
                           (car groups)
                         groups))
               (normalized-groups
                (delq nil (mapcar #'mevedel-hooks--normalize-group groups))))
          (when normalized-groups
            (push (cons event normalized-groups) normalized)))))))

(defun mevedel-hooks--json-rules-to-lisp (json)
  "Convert parsed JSON hook config JSON to Lisp rule shape."
  (let ((root (or (mevedel-hooks--alist-get-string "hooks" json) json))
        rules)
    (dolist (event-entry root (nreverse rules))
      (let ((event (car event-entry))
            (groups (cdr event-entry))
            lisp-groups)
        (when (vectorp groups)
          (setq groups (append groups nil)))
        (dolist (group groups)
          (push (mevedel-hooks--json-value-to-lisp group) lisp-groups))
        (push (cons event (nreverse lisp-groups)) rules)))))


;;
;;; Config file loading and trust

(defun mevedel-hooks--read-lisp-file (file)
  "Read hook rules from Lisp FILE."
  (when (file-readable-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (let ((read-eval nil))
        (read (current-buffer))))))

(defun mevedel-hooks--read-json-file (file)
  "Read hook rules from JSON FILE."
  (when (file-readable-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (mevedel-hooks--json-rules-to-lisp
       (json-parse-buffer
        :object-type 'alist
        :array-type 'array
        :null-object nil
        :false-object :json-false)))))

(defun mevedel-hooks--read-config-file (file)
  "Read and normalize hook config FILE."
  (condition-case err
      (pcase (file-name-extension file)
        ("el" (mevedel-hooks-normalize-rules
               (mevedel-hooks--read-lisp-file file)))
        ("json" (mevedel-hooks-normalize-rules
                 (mevedel-hooks--read-json-file file)))
        (_ nil))
    (error
     (display-warning
      'mevedel
      (format "Could not read hook config %s: %s"
              file (error-message-string err))
      :warning)
     nil)))

(defun mevedel-hooks--config-files-in-dir (dir)
  "Return existing hook config files in DIR in deterministic order."
  (let ((el (file-name-concat dir "hooks.el"))
        (json (file-name-concat dir "hooks.json"))
        files)
    (when (file-exists-p el) (push el files))
    (when (file-exists-p json) (push json files))
    (nreverse files)))

(defun mevedel-hooks--trust-file ()
  "Return the user hook trust database path."
  (file-name-concat mevedel-user-dir "trusted-hooks.el"))

(defun mevedel-hooks--read-trust-db ()
  "Read the trusted hook hash database."
  (let ((file (mevedel-hooks--trust-file)))
    (if (file-readable-p file)
        (condition-case nil
            (mevedel-hooks--read-lisp-file file)
          (error nil))
      nil)))

(defun mevedel-hooks--write-trust-db (db)
  "Write trusted hook hash database DB."
  (let* ((file (mevedel-hooks--trust-file))
         (dir (file-name-directory file)))
    (make-directory dir t)
    (with-temp-file file
      (let ((print-length nil)
            (print-level nil)
            (print-quoted t))
        (prin1 db (current-buffer))))))

(defun mevedel-hooks--file-hash (file)
  "Return SHA256 hash for FILE contents."
  (with-temp-buffer
    (insert-file-contents-literally file)
    (secure-hash 'sha256 (current-buffer))))

(defun mevedel-hooks--workspace-id (workspace)
  "Return stable trust id for WORKSPACE."
  (when workspace
    (format "%S:%s"
            (mevedel-workspace-type workspace)
            (mevedel-workspace-id workspace))))

(defun mevedel-hooks--project-file-trusted-p (workspace file)
  "Return non-nil when project hook FILE is trusted for WORKSPACE."
  (or (not mevedel-hooks-require-project-trust)
      (let ((workspace-id (mevedel-hooks--workspace-id workspace))
            (hash (and (file-readable-p file)
                       (mevedel-hooks--file-hash file)))
            (db (mevedel-hooks--read-trust-db))
            trusted)
        (dolist (entry db trusted)
          (when (and (equal (plist-get entry :workspace-id) workspace-id)
                     (equal (plist-get entry :path)
                            (expand-file-name file))
                     (equal (plist-get entry :hash) hash))
            (setq trusted t))))))

(defun mevedel-hooks--project-config-files (workspace)
  "Return trusted project hook files for WORKSPACE."
  (when workspace
    (let ((dir (mevedel-workspace-state-dir workspace))
          files)
      (dolist (file (mevedel-hooks--config-files-in-dir dir)
                    (nreverse files))
        (if (mevedel-hooks--project-file-trusted-p workspace file)
            (push file files)
          (display-warning
           'mevedel
           (format "Project hook config %s is not trusted; ignoring" file)
           :warning))))))

(defun mevedel-hooks--user-config-files ()
  "Return user hook config files."
  (mevedel-hooks--config-files-in-dir mevedel-user-dir))

(defun mevedel-hooks--append-rule-layer (rules layer)
  "Append normalized hook rule LAYER to RULES."
  (append rules (mevedel-hooks-normalize-rules layer)))

(defun mevedel-hooks--annotate-rules-source (rules source)
  "Return RULES with every handler annotated with SOURCE."
  (mapcar
   (lambda (entry)
     (cons
      (car entry)
      (mapcar
       (lambda (group)
         (let ((copy (copy-sequence group)))
           (when-let* ((hooks (plist-get copy :hooks)))
             (setq copy
                   (plist-put
                    copy :hooks
                    (mapcar
                     (lambda (handler)
                       (plist-put (copy-sequence handler) :source source))
                     hooks))))
           copy))
       (cdr entry))))
   rules))

(defun mevedel-hooks-effective-rules
    (&optional session workspace request invocation)
  "Return effective hook rules for SESSION / WORKSPACE context."
  (let* ((workspace (or workspace
                        (and session (mevedel-session-workspace session))))
         rules)
    (setq rules (mevedel-hooks--append-rule-layer
                 rules mevedel-hook-rules))
    (dolist (file (mevedel-hooks--user-config-files))
      (setq rules
            (append rules
                    (mevedel-hooks--annotate-rules-source
                     (mevedel-hooks--read-config-file file)
                     'user-file))))
    (dolist (file (mevedel-hooks--project-config-files workspace))
      (setq rules
            (append rules
                    (mevedel-hooks--annotate-rules-source
                     (mevedel-hooks--read-config-file file)
                     'project-file))))
    (when session
      (setq rules (mevedel-hooks--append-rule-layer
                   rules (mevedel-session-hook-rules session))))
    (when request
      (setq rules (mevedel-hooks--append-rule-layer
                   rules (mevedel-request-hook-rules request))))
    (when invocation
      (setq rules (mevedel-hooks--append-rule-layer
                   rules (mevedel-agent-invocation-hook-rules invocation))))
    rules))

;;;###autoload
(defun mevedel-hooks-trust-project (&optional workspace)
  "Trust the current project's hook config files.

Interactively, uses `mevedel--session' or `mevedel--workspace' in the
current buffer.  Trust is keyed by workspace id, path, and file hash."
  (interactive)
  (let* ((workspace
          (or workspace
              (and (boundp 'mevedel--session)
                   mevedel--session
                   (mevedel-session-workspace mevedel--session))
              (and (boundp 'mevedel--workspace) mevedel--workspace)))
         (workspace-id (mevedel-hooks--workspace-id workspace)))
    (unless workspace
      (user-error "No mevedel workspace in current buffer"))
    (let ((db (mevedel-hooks--read-trust-db))
          (count 0))
      (dolist (file (mevedel-hooks--config-files-in-dir
                     (mevedel-workspace-state-dir workspace)))
        (let ((entry (list :workspace-id workspace-id
                           :path (expand-file-name file)
                           :hash (mevedel-hooks--file-hash file))))
          (setq db
                (cl-remove-if
                 (lambda (old)
                   (and (equal (plist-get old :workspace-id) workspace-id)
                        (equal (plist-get old :path)
                               (expand-file-name file))))
                 db))
          (push entry db)
          (cl-incf count)))
      (mevedel-hooks--write-trust-db (nreverse db))
      (message "mevedel: trusted %d project hook file(s)" count))))


;;
;;; Matching

(defun mevedel-hooks--matcher-target (event event-plist)
  "Return matcher target for EVENT from EVENT-PLIST."
  (pcase event
    ((or 'PreToolUse 'PermissionRequest 'PermissionDenied
         'PostToolUse 'PostToolUseFailure)
     (plist-get event-plist :tool-name))
    ((or 'SubagentStart 'SubagentStop)
     (plist-get event-plist :agent-type))
    ((or 'PreCompact 'PostCompact)
     (plist-get event-plist :trigger))
    ('SessionStart
     (plist-get event-plist :source))
    ('SessionEnd
     (plist-get event-plist :reason))
    (_ nil)))

(defun mevedel-hooks-matcher-matches-p (matcher target)
  "Return non-nil when MATCHER matches TARGET."
  (let ((target (or target "")))
    (cond
     ((or (null matcher)
          (and (stringp matcher) (string-empty-p matcher))
          (equal matcher "*"))
      t)
     ((symbolp matcher)
      (equal (symbol-name matcher) target))
     ((stringp matcher)
      (if (string-match-p "\\`[[:alnum:]_-]+\\(?:|[[:alnum:]_-]+\\)*\\'"
                          matcher)
          (member target (split-string matcher "|" t))
        (string-match-p matcher target)))
     (t nil))))

(defun mevedel-hooks--matching-handlers (event event-plist rules)
  "Return declarative handlers for EVENT and EVENT-PLIST from RULES."
  (let ((target (mevedel-hooks--matcher-target event event-plist))
        handlers)
    (dolist (entry rules (nreverse handlers))
      (when (eq (car entry) event)
        (dolist (group (cdr entry))
          (when (mevedel-hooks-matcher-matches-p
                 (plist-get group :matcher) target)
            (setq handlers
                  (append (reverse (plist-get group :hooks)) handlers))))))))

(defun mevedel-hooks--native-functions-for-event (event)
  "Return native Emacs hook functions configured for EVENT."
  (when-let* ((hook-var (alist-get event mevedel-hooks--function-hook-alist))
              (_ (boundp hook-var)))
    (let ((value (symbol-value hook-var)))
      (cond
       ((null value) nil)
       ((functionp value) (list value))
       ((listp value) value)
       (t nil)))))


;;
;;; Decision parsing and merging

(defun mevedel-hooks--normalize-decision (decision)
  "Return normalized hook DECISION plist, or nil."
  (when (and (listp decision)
             (keywordp (car-safe decision)))
    (let ((out nil))
      (if (plist-member decision :suppress-output)
          '(:hook-error "Unsupported hook decision field: :suppress-output")
        (when (plist-member decision :continue)
          (setq out (plist-put out :continue
                               (mevedel-hooks--truthy-p
                                (plist-get decision :continue)))))
        (dolist (key '(:stop-reason :system-message :permission-reason
				    :updated-input :updated-result))
          (when (plist-member decision key)
            (setq out (plist-put out key (plist-get decision key)))))
        (when (plist-member decision :additional-context)
          (let ((ctx (plist-get decision :additional-context)))
            (setq out
                  (plist-put out :additional-context
                             (cond
                              ((null ctx) nil)
                              ((listp ctx) ctx)
                              (t (list ctx)))))))
        (when-let* ((perm (mevedel-hooks--normalize-permission-decision
                           (plist-get decision :permission-decision))))
          (setq out (plist-put out :permission-decision perm)))
        (when out out)))))

(defun mevedel-hooks--decision-from-json-alist (alist)
  "Return hook decision plist from parsed JSON ALIST."
  (let* ((specific (or (mevedel-hooks--alist-get-string "hookSpecificOutput"
                                                        alist)
                       (mevedel-hooks--alist-get-string "hook_specific_output"
                                                        alist)))
         (merged (append specific alist))
         decision)
    (cl-labels
        ((value (&rest keys)
           (cl-loop for key in keys
                    for cell = (or (assoc key merged)
                                   (assq (intern key) merged))
                    when cell return (cdr cell))))
      (when (mevedel-hooks--truthy-p
             (value "suppressOutput" "suppress_output"))
        (setq decision
              '(:hook-error
                "Unsupported hook decision field: suppressOutput")))
      (let ((continue-cell (or (assoc "continue" merged)
                               (assq 'continue merged)
                               (assoc "Continue" merged)
                               (assq 'Continue merged))))
        (when (and continue-cell
                   (not (plist-get decision :hook-error)))
          (setq decision
                (plist-put decision :continue
                           (mevedel-hooks--truthy-p
                            (cdr continue-cell))))))
      (dolist (spec '(("stopReason" "stop_reason" :stop-reason)
                      ("systemMessage" "system_message" :system-message)
                      ("additionalContext" "additional_context"
                       :additional-context)
                      ("permissionReason" "permission_reason"
                       "permissionDecisionReason"
                       "permission_decision_reason" :permission-reason)
                      ("updatedResult" "updated_result" :updated-result)))
        (let ((val (apply #'value (butlast spec))))
          (when (and val
                     (not (plist-get decision :hook-error)))
            (setq decision (plist-put decision (car (last spec))
                                      (mevedel-hooks--json-value-to-lisp
                                       val))))))
      (let ((updated-input (value "updatedInput" "updated_input")))
        (when (and updated-input
                   (not (plist-get decision :hook-error)))
          (setq decision
                (plist-put decision :updated-input
                           (mevedel-hooks--json-tool-input-to-lisp
                            updated-input)))))
      (let ((perm (or (value "permissionDecision" "permission_decision")
                      (value "decision"))))
        (when (and perm
                   (not (plist-get decision :hook-error)))
          (setq decision (plist-put decision :permission-decision perm)))))
    (if (plist-get decision :hook-error)
        decision
      (mevedel-hooks--normalize-decision decision))))

(defun mevedel-hooks--parse-command-decision (stdout)
  "Parse command hook STDOUT into a decision plist."
  (let ((trimmed (string-trim stdout)))
    (unless (string-empty-p trimmed)
      (condition-case err
          (mevedel-hooks--decision-from-json-alist
           (json-parse-string trimmed
                              :object-type 'alist
                              :array-type 'array
                              :null-object nil
                              :false-object :json-false))
        (error
         (list :hook-error
               (format "Could not parse hook JSON: %s"
                       (error-message-string err))))))))

(defun mevedel-hooks-merge-decisions (base next)
  "Merge hook decision NEXT into BASE and return the new decision."
  (let ((base (copy-sequence (or base nil)))
        (next (mevedel-hooks--normalize-decision next)))
    (when next
      (when (plist-member next :continue)
        (setq base (plist-put base :continue (plist-get next :continue))))
      (dolist (key '(:stop-reason :system-message :permission-reason
				  :updated-input :updated-result))
        (when (plist-member next key)
          (setq base (plist-put base key (plist-get next key)))))
      (when (plist-member next :additional-context)
        (setq base
              (mevedel-hooks--plist-put-append
               base :additional-context
               (plist-get next :additional-context))))
      (when-let* ((perm (plist-get next :permission-decision)))
        (let ((existing (plist-get base :permission-decision)))
          (when (> (mevedel-hooks--permission-priority perm)
                   (mevedel-hooks--permission-priority existing))
            (setq base (plist-put base :permission-decision perm))))))
    base))

(defun mevedel-hooks-terminal-decision-p (decision &optional event)
  "Return non-nil when DECISION should stop later hooks for EVENT."
  (or (and (memq event '(UserPromptSubmit UserPromptExpansion
                                          PreToolUse PermissionRequest
                                          PreCompact SubagentStart))
           (plist-member decision :continue)
           (not (plist-get decision :continue)))
      (and (memq event '(PreToolUse PermissionRequest))
           (eq (plist-get decision :permission-decision) 'deny))))

(defun mevedel-hooks-record-session-context (session decision)
  "Append DECISION's additional context to SESSION's next prompt."
  (when-let* ((additional (plist-get decision :additional-context)))
    (setf (mevedel-session-hook-context-pending session)
          (append (mevedel-session-hook-context-pending session)
                  additional))))


;;
;;; Event payloads and logging

(defun mevedel-hooks-event-plist (event &optional session workspace &rest extra)
  "Build a generic hook plist for EVENT, SESSION, WORKSPACE, and EXTRA."
  (let* ((workspace (or workspace
                        (and session (mevedel-session-workspace session))))
         (workspace-root (and workspace (mevedel-workspace-root workspace)))
         (cwd (or (and session (mevedel-session-working-directory session))
                  workspace-root
                  default-directory))
         (plist (list :hook-event-name event
                      :session-id (and session
                                       (mevedel-session-session-id session))
                      :transcript-path (and session
                                            (mevedel-session-save-path session))
                      :cwd cwd
                      :workspace-root workspace-root
                      :model (and (boundp 'gptel-model) gptel-model)
                      :turn-id (and session
                                    (1+ (or (mevedel-session-turn-count session)
                                            0)))
                      :origin "main")))
    (while extra
      (setq plist (plist-put plist (pop extra) (pop extra))))
    plist))

(defun mevedel-hooks-tool-event-plist (event context &rest extra)
  "Build tool hook plist for EVENT from pipeline CONTEXT and EXTRA."
  (let* ((tool (plist-get context :tool))
         (session (plist-get context :session))
         (workspace (plist-get context :workspace))
         (invocation (plist-get context :invocation))
         (base (mevedel-hooks-event-plist
                event session workspace
                :cwd (plist-get context :default-directory)
                :origin (or (plist-get context :origin)
                            (and invocation
                                 (fboundp 'mevedel-agent-invocation-p)
                                 (mevedel-agent-invocation-p invocation)
                                 (mevedel-agent-invocation-agent-id
                                  invocation))
                            "main"))))
    (append
     base
     (list :tool-name (and tool (mevedel-tool-name tool))
           :tool-use-id (plist-get context :tool-use-id)
           :tool-input (plist-get context :args))
     extra)))

(defun mevedel-hooks-additional-context-string (decision)
  "Return model-visible additional context from hook DECISION, or nil."
  (when-let* ((additional (plist-get decision :additional-context)))
    (concat "<hook-context>\n"
            (mapconcat (lambda (item) (format "%s" item))
                       additional
                       "\n")
            "\n</hook-context>")))

(defun mevedel-hooks-log-path (session)
  "Return the persistent hook log path for SESSION, or nil."
  (when-let* ((save-path (and session (mevedel-session-save-path session))))
    (file-name-concat save-path "hook-log.el")))

(defun mevedel-hooks--printable-value (value)
  "Return a disk-log-safe representation of VALUE."
  (cond
   ((or (null value)
        (keywordp value)
        (symbolp value)
        (stringp value)
        (numberp value))
    value)
   ((and (listp value) (keywordp (car-safe value)))
    (let (out)
      (while value
        (let ((key (pop value))
              (val (pop value)))
          (setq out
                (plist-put out key
                           (mevedel-hooks--printable-value val)))))
      out))
   ((consp value)
    (cons (mevedel-hooks--printable-value (car value))
          (mevedel-hooks--printable-value (cdr value))))
   ((vectorp value)
    (vconcat (mapcar #'mevedel-hooks--printable-value
                     (append value nil))))
   (t
    (format "%S" value))))

(defun mevedel-hooks--persist-log-entry (session entry)
  "Append sanitized hook log ENTRY to SESSION's persistent hook log."
  (when-let* ((file (and mevedel-hooks-persist-log
                         (mevedel-hooks-log-path session))))
    (condition-case err
        (let ((print-length nil)
              (print-level nil)
              (print-quoted t))
          (make-directory (file-name-directory file) t)
          (with-temp-buffer
            (prin1 (mevedel-hooks--printable-value entry) (current-buffer))
            (insert "\n")
            (append-to-file (point-min) (point-max) file)))
      (error
       (message "mevedel: hook log persistence failed: %s"
                (error-message-string err))))))

(defun mevedel-hooks--log (session entry)
  "Append hook log ENTRY to SESSION."
  (when session
    (let ((log (append (mevedel-session-hook-log session)
                       (list entry))))
      (when (and mevedel-hooks-log-limit
                 (> (length log) mevedel-hooks-log-limit))
        (setq log (last log mevedel-hooks-log-limit)))
      (setf (mevedel-session-hook-log session) log))
    (mevedel-hooks--persist-log-entry session entry)))

(defun mevedel-hooks--log-entry (event handler status &rest props)
  "Build a hook log entry for EVENT, HANDLER, STATUS, and PROPS."
  (append (list :event event
                :handler handler
                :status status
                :time (format-time-string "%FT%T%z"))
          props))

(defun mevedel-hooks--event-display-name (event)
  "Return a human-readable display name for hook EVENT."
  (if (symbolp event) (symbol-name event) (format "%s" event)))

(defun mevedel-hooks--surface (session text &optional spinner-text)
  "Surface hook TEXT for SESSION and optionally update SPINNER-TEXT."
  (ignore session)
  (message "mevedel: %s" text)
  (when-let* ((view-buffer (and (boundp 'mevedel--view-buffer)
                                mevedel--view-buffer))
              (_ (buffer-live-p view-buffer))
              (_ spinner-text)
              (_ (fboundp 'mevedel-view--update-spinner))
              (_ (fboundp 'mevedel-view--spinner-active-p)))
    (with-current-buffer view-buffer
      (when (ignore-errors (mevedel-view--spinner-active-p))
        (ignore-errors
          (mevedel-view--update-spinner spinner-text))))))

(defun mevedel-hooks--decision-blocking-p (decision)
  "Return non-nil when DECISION blocks the triggering operation."
  (or (and (plist-member decision :continue)
           (not (plist-get decision :continue)))
      (eq (plist-get decision :permission-decision) 'deny)))

(defun mevedel-hooks--decision-reason (decision)
  "Return a user-facing reason string from DECISION, or nil."
  (or (plist-get decision :stop-reason)
      (plist-get decision :permission-reason)
      (plist-get decision :system-message)))

(defun mevedel-hooks--surface-final-decision (event session decision)
  "Surface user-visible fields from hook DECISION for EVENT."
  (cond
   ((and (not (memq event '(PostToolUse PostToolUseFailure
                                        PostCompact SubagentStop
                                        Stop StopFailure SessionEnd)))
         (mevedel-hooks--decision-blocking-p decision))
    (mevedel-hooks--surface
     session
     (format "%s hook blocked: %s"
             (mevedel-hooks--event-display-name event)
             (or (mevedel-hooks--decision-reason decision)
                 "no reason provided"))))
   ((plist-get decision :system-message)
    (mevedel-hooks--surface
     session
     (format "%s hook: %s"
             (mevedel-hooks--event-display-name event)
             (plist-get decision :system-message))))))


;;
;;; Command execution

(defun mevedel-hooks--keyword-to-json-key (keyword)
  "Convert Lisp KEYWORD to snake_case JSON key string."
  (replace-regexp-in-string "-" "_"
                            (substring (symbol-name keyword) 1)))

(defun mevedel-hooks--json-encode-value (value)
  "Convert Lisp VALUE to a shape suitable for `json-serialize'."
  (cond
   ((eq value t) t)
   ((null value) :null)
   ((memq value '(:json-false :false)) :false)
   ((eq value :null) :null)
   ((hash-table-p value)
    (let ((table (make-hash-table :test (hash-table-test value))))
      (maphash
       (lambda (key val)
         (puthash key (mevedel-hooks--json-encode-value val) table))
       value)
      table))
   ((and (listp value) (keywordp (car-safe value)))
    (let ((table (make-hash-table :test #'equal)))
      (while value
        (let* ((key (pop value))
               (raw (pop value))
               (encoded (if (and (null raw)
                                  (memq key '(:background :aggressive
                                                       :read-only)))
                             :false
                           (mevedel-hooks--json-encode-value raw))))
          (puthash (mevedel-hooks--keyword-to-json-key key)
                   encoded
                   table)))
      table))
   ((listp value)
    (mapcar #'mevedel-hooks--json-encode-value value))
   ((symbolp value)
    (symbol-name value))
   (t value)))

(defun mevedel-hooks--event-json (event-plist)
  "Return JSON string for EVENT-PLIST."
  (json-serialize (mevedel-hooks--json-encode-value event-plist)))

(defun mevedel-hooks--failure-decision (handler reason)
  "Return failure decision for HANDLER and REASON."
  (if (plist-get handler :fail-closed)
      (list :continue nil :stop-reason reason)
    nil))

(defun mevedel-hooks--block-decision (event reason)
  "Return an event-appropriate blocking decision for EVENT and REASON."
  (let ((reason (if (and (stringp reason)
                         (not (string-empty-p reason)))
                    reason
                  "Hook blocked operation")))
    (pcase event
      ((or 'PreToolUse 'PermissionRequest)
       (list :permission-decision 'deny
             :permission-reason reason))
      ((or 'UserPromptSubmit 'UserPromptExpansion
           'PreCompact 'SubagentStart)
       (list :continue nil :stop-reason reason))
      (_
       (list :system-message reason)))))

(defun mevedel-hooks--apply-decision-to-event-plist
    (event event-plist decision)
  "Return EVENT-PLIST updated with mutating fields from DECISION."
  (let ((payload (copy-sequence event-plist)))
    (when (plist-member decision :updated-input)
      (pcase event
        ('PreToolUse
         (setq payload
               (plist-put payload :tool-input
                          (plist-get decision :updated-input))))
        ((or 'UserPromptSubmit 'UserPromptExpansion)
         (setq payload
               (plist-put payload :prompt
                          (plist-get decision :updated-input))))))
    (when (and (plist-member decision :updated-result)
               (memq event '(PostToolUse PostToolUseFailure)))
      (setq payload
            (plist-put payload :result
                       (plist-get decision :updated-result))))
    (when (and (plist-member decision :updated-result)
               (memq event '(PostToolUse PostToolUseFailure)))
      (setq payload
            (plist-put payload :tool-response
                       (plist-get decision :updated-result))))
    payload))

(defun mevedel-hooks--run-command-handler
    (event handler event-plist session callback)
  "Run command HANDLER for EVENT and call CALLBACK with decision."
  (let* ((command (plist-get handler :command))
         (timeout (mevedel-hooks--command-timeout handler))
         (default-directory (file-name-as-directory
                             (or (and (eq (plist-get handler :source)
                                          'project-file)
                                      (plist-get event-plist
                                                 :workspace-root))
                                 (plist-get event-plist :cwd)
                                 (plist-get event-plist :workspace-root)
                                 default-directory)))
         (stdout-buffer (generate-new-buffer " *mevedel-hook-stdout*"))
         (stderr-buffer (generate-new-buffer " *mevedel-hook-stderr*"))
         (start-time (float-time))
         (settled nil)
         process timer)
    (cl-labels
        ((buffer-string-safe (buffer)
           (if (buffer-live-p buffer)
               (with-current-buffer buffer
                 (buffer-substring-no-properties (point-min) (point-max)))
             ""))
         (finish (status reason)
           (unless settled
             (setq settled t)
             (when timer (cancel-timer timer))
             (let* ((stdout (buffer-string-safe stdout-buffer))
                    (stderr (buffer-string-safe stderr-buffer))
                    (elapsed (- (float-time) start-time))
                    decision log-status)
               (pcase status
                 ('ok
                  (setq decision (mevedel-hooks--parse-command-decision
                                  stdout))
                  (if (plist-get decision :hook-error)
                      (progn
                        (setq log-status 'parse-error)
                        (setq decision
                              (mevedel-hooks--failure-decision
                               handler
                               (plist-get decision :hook-error))))
                    (setq log-status 'ok)))
                 ('block
                  (setq log-status 'block)
                  (setq decision
                        (mevedel-hooks--block-decision
                         event
                         (string-trim
                          (if (string-empty-p stderr) stdout stderr)))))
                 ('timeout
                  (setq log-status 'timeout)
                  (setq decision
                        (mevedel-hooks--failure-decision
                         handler
                         (format "Hook timed out after %s seconds"
                                 timeout))))
                 (_
                  (setq log-status 'error)
                  (setq decision
                        (mevedel-hooks--failure-decision
                         handler
                         (or reason
                             (string-trim
                              (if (string-empty-p stderr) stdout stderr))
                             "Hook command failed")))))
               (mevedel-hooks--log
                session
                (mevedel-hooks--log-entry
                 event handler log-status
                 :elapsed elapsed
                 :exit-status status
                 :stdout-preview (substring stdout 0 (min 1000 (length stdout)))
                 :stderr-preview (substring stderr 0 (min 1000 (length stderr)))
                 :decision decision))
               (when (buffer-live-p stdout-buffer) (kill-buffer stdout-buffer))
               (when (buffer-live-p stderr-buffer) (kill-buffer stderr-buffer))
               (funcall callback decision)))))
      (condition-case err
          (progn
            (setq process
                  (make-process
                   :name "mevedel-hook"
                   :buffer stdout-buffer
                   :stderr stderr-buffer
                   :command (list shell-file-name shell-command-switch command)
                   :connection-type 'pipe
                   :noquery t
                   :sentinel
                   (lambda (proc _event)
                     (when (memq (process-status proc) '(exit signal))
                       (let ((code (process-exit-status proc)))
                         (cond
                          ((= code 0) (finish 'ok nil))
                          ((= code 2) (finish 'block nil))
                          (t (finish 'error
                                     (format "Hook exited with status %s"
                                             code)))))))))
            (process-send-string process
                                 (concat (mevedel-hooks--event-json event-plist)
                                         "\n"))
            (process-send-eof process)
            (when timeout
              (setq timer
                    (run-at-time
                     timeout nil
                     (lambda ()
                       (when (and process
                                  (process-live-p process))
                         (delete-process process))
                       (finish 'timeout nil))))))
        (error
         (finish 'error (error-message-string err)))))))


;;
;;; Handler runner

(defun mevedel-hooks--run-native-functions
    (event event-plist session decision)
  "Run Emacs native hook functions for EVENT.
Returns (DECISION . EVENT-PLIST) after serial mutations."
  (if-let* ((hook-var (alist-get event mevedel-hooks--function-hook-alist)))
      (let ((merged decision)
            (payload event-plist))
        (run-hook-wrapped
         hook-var
         (lambda (fn)
           (let ((handler (list :type 'elisp :function fn)))
             (condition-case err
                 (let* ((result (funcall fn payload))
                        (normalized
                         (mevedel-hooks--normalize-decision result)))
                   (if-let* ((reason (plist-get normalized :hook-error)))
                       (mevedel-hooks--log
                        session
                        (mevedel-hooks--log-entry
                         event handler 'error
                         :decision result
                         :error reason))
                     (setq merged
                           (mevedel-hooks-merge-decisions merged result))
                     (setq payload
                           (mevedel-hooks--apply-decision-to-event-plist
                            event payload merged))
                     (mevedel-hooks--log
                      session
                      (mevedel-hooks--log-entry
                       event handler 'ok :decision result))))
	       (error
	        (mevedel-hooks--log
	         session
                 (mevedel-hooks--log-entry
                  event handler 'error
                  :error (error-message-string err))))))
	   (mevedel-hooks-terminal-decision-p merged event)))
        (cons merged payload))
    (cons decision event-plist)))

(defun mevedel-hooks--run-elisp-handler
    (event handler event-plist session)
  "Run Elisp HANDLER for EVENT and return its decision."
  (let ((fn (plist-get handler :function)))
    (condition-case err
        (if (functionp fn)
            (let* ((decision (funcall fn event-plist))
                   (normalized (mevedel-hooks--normalize-decision
                                decision)))
              (mevedel-hooks--log
               session
               (mevedel-hooks--log-entry
                event handler 'ok :decision decision))
              (if-let* ((reason (plist-get normalized :hook-error)))
                  (progn
                    (mevedel-hooks--log
                     session
                     (mevedel-hooks--log-entry
                      event handler 'error :error reason))
                    (mevedel-hooks--failure-decision handler reason))
                decision))
          (let ((reason (format "Unknown hook function: %S" fn)))
            (mevedel-hooks--log
             session
             (mevedel-hooks--log-entry
              event handler 'error :error reason))
            (mevedel-hooks--failure-decision handler reason)))
      (error
       (let ((reason (error-message-string err)))
         (mevedel-hooks--log
          session
          (mevedel-hooks--log-entry event handler 'error :error reason))
         (mevedel-hooks--failure-decision handler reason))))))

(defun mevedel-hooks--run-handlers
    (event handlers event-plist session decision callback &optional dispatch-buffer)
  "Run declarative HANDLERS for EVENT, then call CALLBACK."
  (if (and dispatch-buffer
           (buffer-live-p dispatch-buffer)
           (not (eq (current-buffer) dispatch-buffer)))
      (with-current-buffer dispatch-buffer
        (mevedel-hooks--run-handlers
         event handlers event-plist session decision callback dispatch-buffer))
    (if (or (null handlers)
            (mevedel-hooks-terminal-decision-p decision event))
        (funcall callback decision)
      (let ((handler (car handlers)))
        (pcase (plist-get handler :type)
          ('elisp
           (let* ((handler-decision
                   (mevedel-hooks--run-elisp-handler
                    event handler event-plist session))
                  (next (mevedel-hooks-merge-decisions
                         decision handler-decision))
                  (next-plist
                   (mevedel-hooks--apply-decision-to-event-plist
                    event event-plist next)))
             (mevedel-hooks--run-handlers
              event (cdr handlers) next-plist session next callback
              dispatch-buffer)))
          ('command
           (mevedel-hooks--run-command-handler
            event handler event-plist session
            (lambda (handler-decision)
              (let* ((next (mevedel-hooks-merge-decisions
                            decision handler-decision))
                     (next-plist
                      (mevedel-hooks--apply-decision-to-event-plist
                       event event-plist next)))
                (mevedel-hooks--run-handlers
                 event (cdr handlers) next-plist session next callback
                 dispatch-buffer)))))
          (_
           (mevedel-hooks--run-handlers
            event (cdr handlers) event-plist session decision callback
            dispatch-buffer)))))))

(defun mevedel-hooks-run-event
    (event event-plist callback &optional session workspace request invocation)
  "Run hooks for EVENT and call CALLBACK with merged decision.

EVENT-PLIST is the stable mevedel event payload passed to Elisp hooks
and serialized to JSON for command hooks.  CALLBACK receives nil or a
decision plist."
  (let* ((event (mevedel-hooks--event-symbol event))
         (session (or session
                      (and (boundp 'mevedel--session) mevedel--session)))
         (workspace (or workspace
                        (and session (mevedel-session-workspace session))))
         (request (or request
                      (and (boundp 'mevedel--current-request)
                           mevedel--current-request)))
         (invocation (or invocation
                         (and (boundp 'mevedel--agent-invocation)
                              mevedel--agent-invocation)))
         (dispatch-buffer (current-buffer)))
    (if (null event)
        (funcall callback nil)
      (let* ((payload (plist-put (copy-sequence event-plist)
                                 :hook-event-name event))
             (rules (mevedel-hooks-effective-rules
                     session workspace request invocation))
             (native-functions (mevedel-hooks--native-functions-for-event
                                event))
             (handlers (mevedel-hooks--matching-handlers
                        event payload rules))
             (settled nil)
             slow-timer)
        (cl-labels
            ((finish (decision)
               (unless settled
                 (setq settled t)
	                 (when (timerp slow-timer)
	                   (cancel-timer slow-timer))
                         (if (buffer-live-p dispatch-buffer)
                             (with-current-buffer dispatch-buffer
	                       (mevedel-hooks--surface-final-decision
	                        event session decision)
	                       (funcall callback decision))
                           (mevedel-hooks--surface-final-decision
                            event session decision)
                           (funcall callback decision)))))
          (when (and mevedel-hooks-slow-threshold
                     (or native-functions handlers))
            (setq slow-timer
                  (run-at-time
                   mevedel-hooks-slow-threshold nil
                   (lambda ()
                     (unless settled
                       (mevedel-hooks--surface
                        session
                        (format "%s hook still running..."
                                (mevedel-hooks--event-display-name event))
                        (format "Running %s hook..."
                                (mevedel-hooks--event-display-name
                                 event))))))))
          (pcase-let ((`(,decision . ,payload)
                       (mevedel-hooks--run-native-functions
                        event payload session nil)))
	            (mevedel-hooks--run-handlers
	             event handlers payload session decision #'finish
                     dispatch-buffer)))))))

(defun mevedel-hooks--target-key-for-event (event)
  "Return the primary matcher payload key for EVENT, or nil."
  (pcase event
    ((or 'PreToolUse 'PermissionRequest 'PermissionDenied
         'PostToolUse 'PostToolUseFailure)
     :tool-name)
    ((or 'SubagentStart 'SubagentStop)
     :agent-type)
    ((or 'PreCompact 'PostCompact)
     :trigger)
    ('SessionStart :source)
    ('SessionEnd :reason)
    (_ nil)))

;;;###autoload
(defun mevedel-hooks-run-dry
    (&optional event event-plist session workspace request invocation)
  "Return a dry-run description of hooks that would run for EVENT.

EVENT-PLIST is matched exactly like `mevedel-hooks-run-event', but no
native function, Elisp handler, or command handler is executed.

Interactively, prompt for an event and optional matcher target, then
display the dry-run result."
  (interactive)
  (let* ((interactive-p (called-interactively-p 'interactive))
         (event
          (or (mevedel-hooks--event-symbol event)
              (intern
               (completing-read
                "Hook event: "
                (mapcar #'symbol-name mevedel-hooks--events)
                nil t))))
         (target-key (mevedel-hooks--target-key-for-event event))
         (target
          (and interactive-p
               target-key
               (read-string
                (format "%s matcher target (empty for all): "
                        (substring (symbol-name target-key) 1)))))
         (event-plist
          (copy-sequence
           (or event-plist
               (and target-key
                    target
                    (not (string-empty-p target))
                    (list target-key target))
               nil)))
         (session (or session
                      (and (boundp 'mevedel--session) mevedel--session)))
         (workspace (or workspace
                        (and session (mevedel-session-workspace session))))
         (request (or request
                      (and (boundp 'mevedel--current-request)
                           mevedel--current-request)))
         (invocation (or invocation
                         (and (boundp 'mevedel--agent-invocation)
                              mevedel--agent-invocation)))
         (payload (plist-put event-plist :hook-event-name event))
         (rules (mevedel-hooks-effective-rules
                 session workspace request invocation))
         (native-functions (mevedel-hooks--native-functions-for-event event))
         (handlers (mevedel-hooks--matching-handlers event payload rules))
         (result
          (list :event event
                :payload payload
                :matcher-target (mevedel-hooks--matcher-target event payload)
                :native-functions
                (mapcar #'mevedel-hooks--printable-value native-functions)
                :handlers (mevedel-hooks--printable-value handlers)
                :handler-count (length handlers))))
    (when interactive-p
      (with-current-buffer (get-buffer-create "*mevedel hook dry-run*")
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert "mevedel hook dry-run\n\n")
          (pp result (current-buffer))
          (goto-char (point-min))
          (view-mode 1))
        (display-buffer (current-buffer))))
    result))

;;;###autoload
(defun mevedel-hooks-list (&optional session)
  "Display effective hooks for SESSION or current buffer."
  (interactive)
  (let* ((session (or session
                      (and (boundp 'mevedel--session) mevedel--session)))
         (workspace (and session (mevedel-session-workspace session)))
         (rules (mevedel-hooks-effective-rules session workspace)))
    (with-current-buffer (get-buffer-create "*mevedel hooks*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "Effective mevedel hooks\n\n")
        (pp rules (current-buffer))
        (goto-char (point-min))
        (view-mode 1))
      (display-buffer (current-buffer)))))

(provide 'mevedel-hooks)
;;; mevedel-hooks.el ends here
