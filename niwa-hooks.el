;;; niwa-hooks.el -- Project hook execution -*- lexical-binding: t -*-

;;; Commentary:

;; Hook execution subsystem for niwa lifecycle and tool events.
;;
;; Hooks are configured as Lisp data or JSON in user / project files, skill
;; manifests, and agent definitions, plus Emacs-native abnormal hook variables
;; for package integration.
;; Command hooks receive JSON on stdin and return structured decisions
;; on stdout; Elisp hooks receive the event plist directly.

;;; Code:

(require 'niwa-report)

(require 'cl-lib)

(require 'json)
(require 'subr-x)
(require 'niwa-structs)
(require 'niwa-telemetry)
(require 'niwa-transport)
(require 'niwa-utilities)

;; `niwa-utilities'
(declare-function niwa-version "niwa-utilities" (&optional here message))

;; `niwa-agents'
(declare-function niwa-agent-invocation-hook-rules
                  "niwa-agents" (cl-x) t)
(declare-function niwa-agent-invocation-p "niwa-agents" (cl-x))
(declare-function niwa-agent-invocation-path
                  "niwa-agents" (cl-x) t)

;; `niwa-execution-target'
(declare-function niwa-execution-target-identity
                  "niwa-execution-target" (cl-x) t)
(declare-function niwa-execution-target-native-path
                  "niwa-execution-target" (target path))
(declare-function niwa-execution-target-remote-p
                  "niwa-execution-target" (target))
(declare-function niwa-execution-target-same-path-domain-p
                  "niwa-execution-target" (left right))
(autoload 'niwa-execution-target-identity "niwa-execution-target")
(autoload 'niwa-execution-target-native-path "niwa-execution-target")
(autoload 'niwa-execution-target-remote-p "niwa-execution-target")
(autoload 'niwa-execution-target-same-path-domain-p "niwa-execution-target")

;; `niwa-plugin-registry'
(declare-function niwa-plugin-hooks "niwa-plugin-registry" (cl-x) t)
(declare-function niwa-plugin-hooks-file
                  "niwa-plugin-registry" (cl-x) t)
(declare-function niwa-plugin-name "niwa-plugin-registry" (cl-x) t)
(declare-function niwa-plugin-root "niwa-plugin-registry" (cl-x) t)
(declare-function niwa-plugins-hooks-enabled-p "niwa-plugin-registry"
                  (plugin &optional workspace))
(declare-function niwa-plugins-enabled "niwa-plugin-registry"
                  (&optional workspace))
(declare-function niwa-plugins-plugin-data-dir "niwa-plugin-registry"
                  (plugin-name &optional workspace))

;; `niwa-reminders'
(declare-function niwa-reminders-queue-turn-event
                  "niwa-reminders" (buffer key body &optional commit))
(autoload 'niwa-reminders-queue-turn-event "niwa-reminders")

;; `niwa-session-publication'
(declare-function niwa-session-publication-append-diagnostic
                  "niwa-session-publication" (session path content))
(autoload 'niwa-session-publication-append-diagnostic
  "niwa-session-publication")

;; `niwa-skills-core'
(declare-function niwa-skills-project-files
                  "niwa-skills-core" (workspace-root))

;; `niwa-structs'
(declare-function niwa-session-working-directory "niwa-structs" (cl-x) t)

;; `niwa-telemetry'
(declare-function niwa-telemetry-record
                  "niwa-telemetry" (session event &rest props))
(declare-function niwa-telemetry-record-audit
                  "niwa-telemetry" (session event &rest props))
(declare-function niwa-telemetry-start
                  "niwa-telemetry" (session event &rest props))
(autoload 'niwa-telemetry-forwarded-audit-p "niwa-telemetry")
(autoload 'niwa-telemetry-record-audit "niwa-telemetry")

;; `niwa-tool-registry'
(declare-function niwa-tool-name "niwa-tool-registry" (cl-x) t)
(declare-function niwa-tool-category "niwa-tool-registry" (cl-x) t)

;; `niwa-turn'
(declare-function niwa-current-turn "niwa-turn" (session))
(declare-function niwa-request-push-canceller
                  "niwa-turn" (request canceller))
(autoload 'niwa-current-turn "niwa-turn")
(autoload 'niwa-request-push-canceller "niwa-turn")

;; `niwa-view-stream'
(declare-function niwa-view-stream-lease-progress
                  "niwa-view-stream" (view))
(declare-function niwa-view-stream-claim-leased-progress
                  "niwa-view-stream" (view lease status owner))
(declare-function niwa-view-stream-release-progress
                  "niwa-view-stream" (view token))
(autoload 'niwa-view-stream-lease-progress "niwa-view-stream")
(autoload 'niwa-view-stream-claim-leased-progress "niwa-view-stream")
(autoload 'niwa-view-stream-release-progress "niwa-view-stream")

;; `niwa-workspace'
(declare-function niwa-workspace-state-dir
                  "niwa-workspace" (workspace))
(autoload 'niwa-workspace-state-dir "niwa-workspace")

(defvar gptel-model)
(defvar read-eval)
(defvar niwa--view-buffer)


;;
;;; Customization

(defcustom niwa-hook-rules nil
  "User-level declarative hook rules.

Rules use the same shape as `hooks.el' files:

  ((PreToolUse
    ((:matcher \"Bash\"
      :hooks ((:type command :command \"./hook.sh\"))))))

This defcustom is trusted user configuration and is always loaded."
  :type 'sexp
  :group 'niwa)

(defcustom niwa-hooks-command-timeout 30
  "Default timeout in seconds for command hooks."
  :type 'number
  :group 'niwa)

(defcustom niwa-hooks-command-timeout-max 120
  "Maximum timeout in seconds for command hooks.

Per-hook `:timeout' values are clamped to this value.  Set to nil to
disable the cap."
  :type '(choice (number :tag "Seconds")
                 (const :tag "No cap" nil))
  :group 'niwa)

(defcustom niwa-hooks-command-output-max-chars (* 64 1024)
  "Maximum stdout or stderr characters retained from one command hook."
  :type '(choice (integer :tag "Characters")
                 (const :tag "No cap" nil))
  :group 'niwa)

(defcustom niwa-hooks-require-project-trust t
  "When non-nil, ignore project hook sources until explicitly trusted.

User files under `niwa-user-dir' and `niwa-hook-rules' are always
trusted.  Project hook configs and skill manifests are trusted by file hash via
`niwa-hooks-trust-project'."
  :type 'boolean
  :group 'niwa)

(defcustom niwa-hooks-log-limit 200
  "Maximum number of hook log entries kept on a session.
Nil keeps all entries."
  :type '(choice (integer :tag "Entries")
                 (const :tag "Unlimited" nil))
  :group 'niwa)

(defvar-local niwa-hooks-context-frozen-p nil
  "Non-nil when this buffer's session hook rules are the complete rule set.")

(defcustom niwa-hooks-persist-log t
  "When non-nil, append hook log entries to persisted session directories."
  :type 'boolean
  :group 'niwa)

(defcustom niwa-hooks-slow-threshold 1.5
  "Seconds before a running hook event is surfaced to the user.
Nil disables slow-hook surfacing."
  :type '(choice (number :tag "Seconds")
                 (const :tag "Disabled" nil))
  :group 'niwa)


;;
;;; Emacs API hooks

(defvar niwa-session-start-hook nil
  "Normal hook run after a niwa session starts.
Runs in the chat data buffer.")

(defvar niwa-session-end-hook nil
  "Normal hook run when a niwa session ends.
Runs in the chat data buffer, with `niwa-session-end-reason' saying
why.")

(defvar niwa-session-end-reason nil
  "Why the session is ending, while `niwa-session-end-hook' runs.
\"kill-buffer\" when its data buffer is killed, \"exit\" when Emacs
exits.  The same value is the `SessionEnd' event's `:reason'.")

(defvar niwa-user-prompt-submit-functions nil
  "Abnormal hook functions run before a user prompt is sent.
Each function receives one event plist and may return a hook decision
plist.")

(defvar niwa-user-prompt-expansion-functions nil
  "Abnormal hook functions run after a user slash prompt expands.
Each function receives one event plist and may return a hook decision
plist.")

(defvar niwa-pre-tool-use-functions nil
  "Abnormal hook functions run before a tool executes.
Each function receives one event plist and may return a hook decision
plist.")

(defvar niwa-permission-request-functions nil
  "Abnormal hook functions run before a permission prompt is shown.
Each function receives one event plist and may return a hook decision
plist.")

(defvar niwa-permission-denied-functions nil
  "Abnormal hook functions run after a permission denial.
Each function receives one event plist and may return a hook decision
plist.  Denial hooks are observational; they cannot undo the denial.")

(defvar niwa-post-tool-use-functions nil
  "Abnormal hook functions run after a tool produces its final result.
Each function receives one event plist and may return a hook decision
plist.")

(defvar niwa-pre-compact-functions nil
  "Abnormal hook functions run before conversation compaction.
Each function receives one event plist and may return a hook decision
plist.")

(defvar niwa-post-compact-functions nil
  "Abnormal hook functions run after conversation compaction.
Each function receives one event plist and may return a hook decision
plist.")

(defvar niwa-subagent-start-functions nil
  "Abnormal hook functions run before a sub-agent starts.
Each function receives one event plist and may return a hook decision
plist.")

(defvar niwa-subagent-stop-functions nil
  "Abnormal hook functions run after a sub-agent reaches terminal state.
Each function receives one event plist and may return a hook decision
plist.")

(defvar niwa-stop-functions nil
  "Abnormal hook functions run after a top-level turn completes.
Each function receives one event plist and may return a hook decision
plist.")

(defvar niwa-stop-failure-functions nil
  "Abnormal hook functions run after a top-level turn fails.
Each function receives one event plist and may return a hook decision
plist.")

(defvar niwa-hooks--context-handler-table
  (make-hash-table :test #'eq :weakness 'key)
  "Internal handler attribution keyed by final hook decisions.")


;;
;;; Constants

(defconst niwa-hooks--events
  '(SessionStart UserPromptSubmit UserPromptExpansion
                 PreToolUse PermissionRequest PermissionDenied
                 PostToolUse PostToolUseFailure PreCompact PostCompact
                 SubagentStart SubagentStop Stop StopFailure SessionEnd)
  "Known hook event names.")

(defconst niwa-hooks-tool-events
  '(PreToolUse PermissionRequest PermissionDenied
               PostToolUse PostToolUseFailure)
  "Hook events fired by the tool pipeline for a single tool call.")

(defconst niwa-hooks--function-hook-alist
  '((UserPromptSubmit . niwa-user-prompt-submit-functions)
    (UserPromptExpansion . niwa-user-prompt-expansion-functions)
    (PreToolUse . niwa-pre-tool-use-functions)
    (PermissionRequest . niwa-permission-request-functions)
    (PermissionDenied . niwa-permission-denied-functions)
    (PostToolUse . niwa-post-tool-use-functions)
    (PostToolUseFailure . niwa-post-tool-use-functions)
    (PreCompact . niwa-pre-compact-functions)
    (PostCompact . niwa-post-compact-functions)
    (SubagentStart . niwa-subagent-start-functions)
    (SubagentStop . niwa-subagent-stop-functions)
    (Stop . niwa-stop-functions)
    (StopFailure . niwa-stop-failure-functions))
  "Mapping from hook event names to Emacs abnormal hook variables.")


;;
;;; Utility helpers

(defun niwa-hooks--event-symbol (event)
  "Return canonical symbol for EVENT, or nil when unknown."
  (let ((sym (cond
              ((symbolp event) event)
              ((stringp event) (intern event))
              (t nil))))
    (and (memq sym niwa-hooks--events) sym)))

(defun niwa-hooks--scope-event (event scope)
  "Return EVENT adjusted for hook declaration SCOPE."
  (if (and (memq scope '(agent skill-fork))
           (eq event 'Stop))
      'SubagentStop
    event))

(defun niwa-hooks--alist-get-string (key alist)
  "Return string KEY's value in ALIST using `equal'."
  (or (cdr (assoc key alist))
      (cdr (assq (intern key) alist))))

(defun niwa-hooks--truthy-p (value)
  "Return non-nil when VALUE is logically true for hook config."
  (and value (not (eq value :json-false))))

(defun niwa-hooks--keyword-for-json-key (key)
  "Convert JSON object KEY to a Lisp keyword."
  (let ((key (if (symbolp key) (symbol-name key) key))
        (case-fold-search nil))
    (intern (concat ":"
                    (downcase
                     (replace-regexp-in-string
                      "\\([a-z0-9]\\)\\([A-Z]\\)" "\\1-\\2"
                      (replace-regexp-in-string "_" "-" key)))))))

(defun niwa-hooks--json-value-to-lisp-with-keys (value key-fn)
  "Convert parsed JSON VALUE into Lisp hook data using KEY-FN for object keys."
  (cond
   ((vectorp value)
    (mapcar (lambda (item)
              (niwa-hooks--json-value-to-lisp-with-keys item key-fn))
            (append value nil)))
   ((and (listp value)
         (consp (car-safe value))
         (or (stringp (caar value))
             (symbolp (caar value))))
    (let (plist)
      (dolist (entry value plist)
        (setq plist
              (plist-put plist
                         (funcall key-fn (car entry))
                         (niwa-hooks--json-value-to-lisp-with-keys
                          (cdr entry) key-fn))))))
   ((listp value)
    (mapcar (lambda (item)
              (niwa-hooks--json-value-to-lisp-with-keys item key-fn))
            value))
   ((eq value :json-false) nil)
   (t value)))

(defun niwa-hooks--json-value-to-lisp (value)
  "Convert parsed JSON VALUE into Lisp hook data."
  (niwa-hooks--json-value-to-lisp-with-keys
   value #'niwa-hooks--keyword-for-json-key))

(defun niwa-hooks--json-tool-input-to-lisp (value)
  "Convert JSON tool input VALUE while preserving arg-key underscores."
  (niwa-hooks--json-value-to-lisp-with-keys
   value
   (lambda (key)
     (intern (concat ":" (if (symbolp key) (symbol-name key) key))))))

(defun niwa-hooks--coerce-symbol (value)
  "Coerce VALUE to a symbol when possible."
  (cond
   ((symbolp value) value)
   ((stringp value) (intern value))
   (t value)))

(defun niwa-hooks--normalize-permission-decision (value)
  "Normalize VALUE to `allow', `ask', `deny', or nil."
  (let ((sym (niwa-hooks--coerce-symbol value)))
    (and (memq sym '(allow ask deny)) sym)))

(defun niwa-hooks--permission-priority (decision)
  "Return merge priority for permission DECISION."
  (pcase decision
    ('deny 3)
    ('ask 2)
    ('allow 1)
    (_ 0)))

(defun niwa-hooks--command-timeout (handler)
  "Return effective timeout for command HANDLER."
  (let ((timeout (plist-get handler :timeout)))
    (unless (numberp timeout)
      (setq timeout niwa-hooks-command-timeout))
    (when (numberp timeout)
      (if niwa-hooks-command-timeout-max
          (max 1 (min timeout niwa-hooks-command-timeout-max))
        (max 1 timeout)))))

(defun niwa-hooks--plist-put-append (plist key values)
  "Append VALUES to list at KEY in PLIST."
  (plist-put plist key (append (plist-get plist key) values)))


;;
;;; Normalization

(defun niwa-hooks--normalize-handler (handler)
  "Normalize one hook HANDLER plist.
Returns a plist or nil when HANDLER is invalid."
  (when (and (listp handler) (keywordp (car-safe handler)))
    (let* ((type (niwa-hooks--coerce-symbol (plist-get handler :type)))
           (normalized (copy-sequence handler)))
      (setq normalized (plist-put normalized :type type))
      (when (plist-member normalized :fail-closed)
        (setq normalized
              (plist-put normalized :fail-closed
                         (niwa-hooks--truthy-p
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

(defvar niwa-hooks--normalizing-source nil
  "Source description for the rules being normalized, when known.
Bound around normalization so an unusable matcher can be reported with
the file or layer that configured it.")

(defvar niwa-hooks--normalizing-scope 'global
  "Explicit diagnostic scope bound while normalizing a hook source.")

(defun niwa-hooks--usable-matcher-p (matcher event)
  "Return non-nil when MATCHER can be matched, reporting it for EVENT if not.
Only a regexp matcher can fail; symbols are compared literally, and every
string in the exact-name grammar is also a valid regexp.  The live
defcustom and programmatic layers are re-normalized on every lifecycle
event, so each unusable matcher is warned once per source and demoted
to a *Messages* line after that."
  (or (not (stringp matcher))
      (condition-case err
          (progn (string-match-p matcher "") t)
        (invalid-regexp
         (niwa--warn-once
          niwa-hooks--normalizing-scope
          (list 'hooks-bad-matcher
                niwa-hooks--normalizing-source event matcher)
          "Ignoring %s hook group%s: matcher %S is not a valid regexp: %s"
          event
          (if niwa-hooks--normalizing-source
              (format " from %s" niwa-hooks--normalizing-source)
            "")
          matcher (error-message-string err))
         nil))))

(defun niwa-hooks--normalize-group (group event)
  "Normalize one matcher GROUP plist.
EVENT names the lifecycle event the group was configured under, so an
unusable matcher can be reported where a reader can find it."
  (when (and (listp group) (keywordp (car-safe group)))
    (let* ((matcher (plist-get group :matcher))
           (matcher (if (or (null matcher)
                            (and (stringp matcher)
                                 (string-empty-p matcher)))
                        "*"
                      matcher))
           (hooks (plist-get group :hooks))
           (hooks-list (if (and (listp hooks)
                                (or (null hooks)
                                    (listp (car-safe hooks))))
                           hooks
                         (and hooks (list hooks))))
           (normalized-hooks
            (delq nil (mapcar #'niwa-hooks--normalize-handler
                              hooks-list))))
      (when (and normalized-hooks
                 (niwa-hooks--usable-matcher-p matcher event))
        (list :matcher matcher
              :hooks normalized-hooks)))))

(defun niwa-hooks-normalize-rules (rules &optional scope)
  "Normalize hook RULES into canonical event alist form.
SCOPE may be `agent' or `skill-fork', where local `Stop' hooks are
treated as `SubagentStop'."
  (let (normalized)
    (when (listp rules)
      (dolist (entry rules)
        (when (and (consp entry)
                   (niwa-hooks--event-symbol (car entry)))
          (let* ((event (niwa-hooks--scope-event
                         (niwa-hooks--event-symbol (car entry))
                         scope))
                 (groups (cdr entry))
                 (groups (if (and (= (length groups) 1)
                                  (listp (car groups))
                                  (not (keywordp (caar groups))))
                             (car groups)
                           groups))
                 (normalized-groups
                  (and (listp groups)
                       (delq nil
                             (mapcar (lambda (group)
                                       (niwa-hooks--normalize-group
                                        group event))
                                     groups)))))
            (when normalized-groups
              (push (cons event normalized-groups) normalized))))))
    (nreverse normalized)))

(defun niwa-hooks--json-rules-to-lisp (json)
  "Convert parsed JSON hook config JSON to Lisp rule shape."
  (let ((root (or (niwa-hooks--alist-get-string "hooks" json) json))
        rules)
    (dolist (event-entry root (nreverse rules))
      (let ((event (car event-entry))
            (groups (cdr event-entry))
            lisp-groups)
        (when (vectorp groups)
          (setq groups (append groups nil)))
        (dolist (group groups)
          (push (niwa-hooks--json-value-to-lisp group) lisp-groups))
        (push (cons event (nreverse lisp-groups)) rules)))))


;;
;;; Config file loading and trust

(defun niwa-hooks--read-lisp-file (file &optional content)
  "Read hook rules from Lisp FILE or snapshotted CONTENT."
  (when (or content (file-readable-p file))
    (with-temp-buffer
      (if content
          (insert content)
        (insert-file-contents file))
      (goto-char (point-min))
      (let ((read-eval nil))
        (read (current-buffer))))))

(defun niwa-hooks--read-json-file (file &optional content)
  "Read hook rules from JSON FILE or snapshotted CONTENT."
  (when (or content (file-readable-p file))
    (with-temp-buffer
      (if content
          (insert content)
        (insert-file-contents file))
      (goto-char (point-min))
      (niwa-hooks--json-rules-to-lisp
       (json-parse-buffer
        :object-type 'alist
        :array-type 'array
        :null-object nil
        :false-object :json-false)))))

(defun niwa-hooks-read-config-file (file &optional content scope)
  "Read hook config FILE or snapshotted CONTENT for diagnostic SCOPE.
Without SCOPE the source is user-wide configuration."
  (condition-case err
      (let ((niwa-hooks--normalizing-source file)
            (niwa-hooks--normalizing-scope (or scope 'global)))
        (pcase (file-name-extension file)
          ("el" (niwa-hooks-normalize-rules
                 (niwa-hooks--read-lisp-file file content)))
          ("json" (niwa-hooks-normalize-rules
                   (niwa-hooks--read-json-file file content)))
          (_ nil)))
    (error
     (niwa--warn-once
      (or scope 'global)
      (list 'hooks-config-unreadable file)
      "Could not read hook config %s: %s"
      file (error-message-string err))
     nil)))

(defun niwa-hooks--config-files-in-dir (dir)
  "Return existing hook config files in DIR in deterministic order."
  (let ((el (file-name-concat dir "hooks.el"))
        (json (file-name-concat dir "hooks.json"))
        files)
    (when (file-exists-p el) (push el files))
    (when (file-exists-p json) (push json files))
    (nreverse files)))

(defun niwa-hooks--trust-file ()
  "Return the user hook trust database path."
  (file-name-concat niwa-user-dir "trusted-hooks.el"))

(defun niwa-hooks--read-trust-db ()
  "Read the trusted hook hash database."
  (let ((file (niwa-hooks--trust-file)))
    (if (file-readable-p file)
        (condition-case nil
            (niwa-hooks--read-lisp-file file)
          (error nil))
      nil)))

(defun niwa-hooks--write-trust-db (db)
  "Write trusted hook hash database DB."
  (let* ((file (niwa-hooks--trust-file))
         (dir (file-name-directory file)))
    (make-directory dir t)
    (with-temp-file file
      (let ((print-length nil)
            (print-level nil)
            (print-quoted t))
        (prin1 db (current-buffer))))))

(defun niwa-hooks--config-snapshot (file)
  "Return FILE's path, SHA256 hash, and decoded content from one read."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file)
    (let ((hash (secure-hash 'sha256 (current-buffer))))
      (set-buffer-multibyte t)
      (decode-coding-inserted-region (point-min) (point-max) file)
      (list :path (expand-file-name file)
            :hash hash
            :content (buffer-string)))))

(defun niwa-hooks--workspace-id (workspace)
  "Return stable trust id for WORKSPACE."
  (when workspace
    (format "%S:%s"
            (niwa-workspace-type workspace)
            (niwa-workspace-id workspace))))

(defun niwa-hooks--project-snapshot-trusted-p (workspace snapshot)
  "Return non-nil when project hook SNAPSHOT is trusted for WORKSPACE."
  (or (not niwa-hooks-require-project-trust)
      (let ((workspace-id (niwa-hooks--workspace-id workspace))
            (path (plist-get snapshot :path))
            (hash (plist-get snapshot :hash))
            (db (niwa-hooks--read-trust-db))
            trusted)
        (dolist (entry db trusted)
          (when (and (listp entry)
                     (keywordp (car-safe entry))
                     (equal (plist-get entry :workspace-id) workspace-id)
                     (equal (plist-get entry :path) path)
                     (equal (plist-get entry :hash) hash))
            (setq trusted t))))))

(defun niwa-hooks-project-file-snapshot (workspace file)
  "Return FILE's one-read trust snapshot for WORKSPACE.
The returned plist includes `:path', `:hash', `:content', and
`:trusted-p'."
  (let ((snapshot (niwa-hooks--config-snapshot file)))
    (plist-put snapshot :trusted-p
               (niwa-hooks--project-snapshot-trusted-p
                workspace snapshot))))

(defun niwa-hooks--project-config-candidates (workspace)
  "Return existing project hook files for WORKSPACE in load order."
  (when workspace
    (let (files)
      (dolist (dir (list (file-name-concat
                          (niwa-workspace-root workspace) ".agents/")
                         (niwa-workspace-state-dir workspace))
                    (nreverse files))
        (dolist (file (niwa-hooks--config-files-in-dir dir))
          (push file files))))))

(defun niwa-hooks--project-config-snapshots (workspace)
  "Return trusted project hook snapshots for WORKSPACE."
  (when workspace
    (let (snapshots)
      (dolist (file (niwa-hooks--project-config-candidates workspace)
                    (nreverse snapshots))
        (let ((snapshot (and (file-readable-p file)
                             (niwa-hooks--config-snapshot file))))
          (if (and snapshot
                   (niwa-hooks--project-snapshot-trusted-p
                    workspace snapshot))
              (push snapshot snapshots)
            (niwa--warn-once
             (niwa-diagnostic-scope workspace)
             (list 'hooks-config-untrusted file)
             "Project hook config %s is not trusted; ignoring" file)))))))

(defun niwa-hooks--user-config-files ()
  "Return user hook config files."
  (append (niwa-hooks--config-files-in-dir
           (expand-file-name "~/.agents/"))
          (niwa-hooks--config-files-in-dir niwa-user-dir)))

(defun niwa-hooks--append-rule-layer (rules layer)
  "Append normalized hook rule LAYER to RULES."
  (append rules (niwa-hooks-normalize-rules layer)))

(defun niwa-hooks--annotate-handlers (rules properties)
  "Return a copy of RULES with PROPERTIES applied to every handler.
Copy rule containers and handler plists; retain their nested values."
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
                       (let ((handler (copy-sequence handler))
                             (properties properties))
                         (while properties
                           (setq handler (plist-put handler
                                                    (pop properties)
                                                    (pop properties))))
                         handler))
                     hooks))))
           copy))
       (cdr entry))))
   rules))

(defun niwa-hooks-annotate-rules-source
    (rules source &optional source-file source-root)
  "Annotate RULES with SOURCE, SOURCE-FILE, and SOURCE-ROOT provenance."
  (niwa-hooks--annotate-handlers
   rules (append (list :source source)
                 (when source-file (list :source-file source-file))
                 (when source-root (list :source-root source-root)))))

(defun niwa-hooks--annotate-plugin-rules (rules plugin &optional workspace)
  "Return RULES with every handler annotated for PLUGIN execution.
Plugin runtime data is scoped to WORKSPACE when provided."
  (niwa-hooks--annotate-handlers
   rules (list :source 'plugin
               :plugin-name (niwa-plugin-name plugin)
               :plugin-root (niwa-plugin-root plugin)
               :plugin-data (niwa-plugins-plugin-data-dir
                             (niwa-plugin-name plugin) workspace))))

(defun niwa-hooks--plugin-manifest-rules (plugin &optional workspace)
  "Return normalized manifest rules for PLUGIN in WORKSPACE."
  (let ((entries (or (and (fboundp 'niwa-plugin-hooks)
                          (niwa-plugin-hooks plugin))
                     (and (niwa-plugin-hooks-file plugin)
                          (list (list :file
                                      (niwa-plugin-hooks-file
                                       plugin))))))
        rules)
    (dolist (entry entries rules)
      (setq rules
            (append
             rules
             (cond
              ((plist-get entry :file)
               (let ((file (plist-get entry :file)))
                 (niwa-hooks-annotate-rules-source
                  (niwa-hooks-read-config-file file nil (niwa-diagnostic-scope workspace))
                  'plugin file (niwa-plugin-root plugin))))))))))

(defun niwa-hooks--plugin-config-rules (&optional workspace)
  "Return normalized hook rules from plugins with enabled hooks in WORKSPACE."
  (when (require 'niwa-plugin-registry nil t)
    (let (rules)
      (dolist (plugin (niwa-plugins-enabled workspace) rules)
        (when (niwa-plugins-hooks-enabled-p plugin workspace)
          (when-let* ((manifest-rules (niwa-hooks--plugin-manifest-rules
                                        plugin workspace)))
            (setq rules
                  (append rules
                          (niwa-hooks--annotate-plugin-rules
                           manifest-rules
                           plugin
                           workspace)))))))))

(defvar niwa-hooks--config-rules-cache (make-hash-table :test #'equal)
  "Configured hook rules memoized per workspace root and user directory.

Resolving the user, plugin, and project layers re-reads and re-hashes
every config file, which on a remote workspace is several target round
trips -- and hook events fire many times per turn.  The answer is
remembered for the process, so edits to hook files need
`niwa-hooks-reload' (or trusting the project, which re-reads anyway)
to be picked up; the same bargain the workspace ignore-file memo makes.")

(defun niwa-hooks--config-rules (workspace)
  "Return the configured user, plugin, and project rules for WORKSPACE.

Memoized in `niwa-hooks--config-rules-cache'; the returned list is a
fresh copy, so a caller appending dynamic layers cannot corrupt the
memo."
  (let* ((key (list (and workspace (niwa-workspace-root workspace))
                    niwa-user-dir))
         (cached (gethash key niwa-hooks--config-rules-cache 'missing)))
    (copy-tree
     (if (not (eq cached 'missing))
         cached
       (let (rules)
         (dolist (file (niwa-hooks--user-config-files))
           (setq rules
                 (append rules
                         (niwa-hooks-annotate-rules-source
                          (niwa-hooks-read-config-file file)
                          'user-file file (file-name-directory file)))))
         (setq rules
               (append rules
                       (niwa-hooks--plugin-config-rules workspace)))
         (dolist (snapshot
                  (niwa-hooks--project-config-snapshots workspace))
           (let ((file (plist-get snapshot :path)))
             (setq rules
                   (append rules
                           (niwa-hooks-annotate-rules-source
                            (niwa-hooks-read-config-file
                             file (plist-get snapshot :content) (niwa-diagnostic-scope workspace))
                            'project-file file
                            (file-name-as-directory
                             (niwa-workspace-root workspace)))))))
         (puthash key rules niwa-hooks--config-rules-cache))))))

(defun niwa-hooks-invalidate-config ()
  "Forget every memoized hook configuration so the next event re-reads it."
  (clrhash niwa-hooks--config-rules-cache)
  ;; This command invalidates configuration for every workspace, unlike
  ;; recovery of one workspace's failure.
  (let (scopes)
    (maphash (lambda (key _) (cl-pushnew (car key) scopes :test #'equal))
             niwa--warn-once-table)
    (dolist (scope scopes) (niwa--warn-once-reset-site scope 'hooks-bad-matcher))))

;;;###autoload
(defun niwa-hooks-reload ()
  "Re-read hook configuration on the next event and say so."
  (interactive)
  (niwa-hooks-invalidate-config)
  (message "niwa: hook configuration will be re-read"))

(defun niwa-hooks-effective-rules
    (&optional session workspace request invocation)
  "Return effective hook rules for SESSION, WORKSPACE, REQUEST, and INVOCATION."
  (if niwa-hooks-context-frozen-p
      (copy-tree (and session (niwa-session-hook-rules session)))
    (let* ((workspace (or workspace
                          (and session (niwa-session-workspace session))))
           rules)
      ;; The defcustom layer stays live: it is pure memory, and tests as
      ;; well as users rebind it per context.
      (setq rules
            (append rules
                    (niwa-hooks-annotate-rules-source
                     (let ((niwa-hooks--normalizing-source
                            "niwa-hook-rules"))
                       (niwa-hooks-normalize-rules niwa-hook-rules))
                     'user nil user-emacs-directory)))
      (setq rules (append rules (niwa-hooks--config-rules workspace)))
      (when session
        (setq rules (niwa-hooks--append-rule-layer
                     rules (niwa-session-hook-rules session))))
      (when (niwa-request-p request)
        (setq rules (niwa-hooks--append-rule-layer
                     rules (niwa-request-hook-rules request))))
      (when (and (fboundp 'niwa-agent-invocation-p)
                 (niwa-agent-invocation-p invocation))
        (setq rules (niwa-hooks--append-rule-layer
                     rules (niwa-agent-invocation-hook-rules invocation))))
      rules)))

;;;###autoload
(defun niwa-hooks-trust-project (&optional workspace)
  "Trust WORKSPACE's hook config files and skill manifests.

Interactively, uses `niwa--session' or `niwa--workspace' in the
current buffer.  Trust is keyed by workspace id, path, and file hash."
  (interactive)
  (let* ((workspace
          (or workspace
              (and (boundp 'niwa--session)
                   niwa--session
                   (niwa-session-workspace niwa--session))
              (and (boundp 'niwa--workspace) niwa--workspace)))
         (workspace-id (niwa-hooks--workspace-id workspace)))
    (unless workspace
      (niwa-diagnostic-user-error 'unavailable "No niwa workspace in current buffer"))
    (require 'niwa-skills-core)
    (let ((db (cl-remove-if
               (lambda (old)
                 (and (listp old)
                      (keywordp (car-safe old))
                      (equal (plist-get old :workspace-id) workspace-id)))
               (niwa-hooks--read-trust-db)))
          (count 0))
      (dolist (file
               (delete-dups
                (append
                 (niwa-hooks--project-config-candidates workspace)
                 (niwa-skills-project-files
                  (niwa-workspace-root workspace)))))
        (let* ((snapshot (niwa-hooks--config-snapshot file))
               (entry (list :workspace-id workspace-id
                            :path (plist-get snapshot :path)
                            :hash (plist-get snapshot :hash))))
          (push entry db)
          (cl-incf count)))
      (niwa-hooks--write-trust-db (nreverse db))
      ;; Trust changes which project files resolve, so the memoized
      ;; configuration is stale from here.
      (clrhash niwa-hooks--config-rules-cache)
      (message "niwa: trusted %d project file(s)" count))))


;;
;;; Matching

(defun niwa-hooks--target-key-for-event (event)
  "Return the primary matcher payload key for EVENT, or nil."
  (pcase event
    ((guard (memq event niwa-hooks-tool-events))
     :tool-name)
    ((or 'SubagentStart 'SubagentStop)
     :role)
    ((or 'PreCompact 'PostCompact)
     :trigger)
    ('SessionStart :source)
    ('SessionEnd :reason)
    (_ nil)))

(defun niwa-hooks--matcher-target (event event-plist)
  "Return matcher target for EVENT from EVENT-PLIST."
  (when-let* ((key (niwa-hooks--target-key-for-event event)))
    (plist-get event-plist key)))

(defun niwa-hooks-matcher-matches-p (matcher target)
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
        ;; A matcher that cannot compile matches nothing.  Normalization
        ;; drops and reports configured ones; a hand-built rule must not
        ;; abort the event around it.
        (condition-case nil
            (string-match-p matcher target)
          (invalid-regexp nil))))
     (t nil))))

(defun niwa-hooks--matching-handlers (event event-plist rules)
  "Return declarative handlers for EVENT and EVENT-PLIST from RULES."
  (let ((target (niwa-hooks--matcher-target event event-plist))
        handlers)
    (dolist (entry rules (nreverse handlers))
      (when (and (consp entry)
                 (eq (car entry) event))
        (when (listp (cdr entry))
          (dolist (group (cdr entry))
            (when (and (listp group)
                       (keywordp (car-safe group))
                       (niwa-hooks-matcher-matches-p
                        (plist-get group :matcher) target))
              (when-let* ((hooks (and (listp (plist-get group :hooks))
                                      (delq nil
                                            (mapcar
                                             #'niwa-hooks--normalize-handler
                                             (plist-get group :hooks))))))
                (setq handlers
                      (append (reverse hooks) handlers))))))))))

(defun niwa-hooks--native-handlers-for-event (event)
  "Return native handlers for EVENT in effective Emacs hook order."
  (when-let* ((hook-var (alist-get event niwa-hooks--function-hook-alist))
              (_ (boundp hook-var)))
    (let (handlers)
      (run-hook-wrapped
       hook-var
       (lambda (fn)
         (push (list :type 'elisp
                     :function fn
                     :source 'native)
               handlers)
         nil))
      (nreverse handlers))))

(defun niwa-hooks--handlers-for-event (event event-plist rules)
  "Return all handlers for EVENT and EVENT-PLIST from RULES."
  (append (niwa-hooks--native-handlers-for-event event)
          (niwa-hooks--matching-handlers event event-plist rules)))


;;
;;; Decision parsing and merging

(defun niwa-hooks--normalize-decision (decision)
  "Return normalized hook DECISION plist, or nil."
  (when (and (listp decision)
             (keywordp (car-safe decision)))
    (let ((out nil))
      (if (plist-member decision :suppress-output)
          '(:hook-error "Unsupported hook decision field: :suppress-output")
        (when (plist-member decision :continue)
          (setq out (plist-put out :continue
                               (niwa-hooks--truthy-p
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
                              ((and (listp ctx)
                                    (keywordp (car-safe ctx)))
                               nil)
                              ((listp ctx) ctx)
                              (t (list ctx)))))))
        (when-let* ((perm (niwa-hooks--normalize-permission-decision
                           (plist-get decision :permission-decision))))
          (setq out (plist-put out :permission-decision perm)))
        (when out out)))))

(defun niwa-hooks--decision-plist-p (decision)
  "Return non-nil when DECISION is nil or a plist-shaped value."
  (and (listp decision)
       (or (null decision)
           (keywordp (car-safe decision)))))

(defun niwa-hooks--safe-decision (decision)
  "Return DECISION when it is plist-shaped, otherwise nil."
  (and (niwa-hooks--decision-plist-p decision) decision))

(defun niwa-hooks-sanitize-final-decision (event decision &optional scope)
  "Return a callback-safe DECISION for EVENT, or nil with a warning.

SCOPE identifies the originating operation.  Hook runners sanitize their
own callback value, but prompt-decision consumers read hook results
synchronously and can be handed values from stubs or user code, so the
boundary stays defensive at one owner."
  (if (niwa-hooks--decision-plist-p decision)
      decision
    (niwa--warn-once
     (or scope 'unavailable)
     (list 'hooks-malformed-decision event)
     "Ignoring malformed %s hook decision: %S"
     (niwa-hooks-event-display-name event)
     decision)
    nil))

(defun niwa-hooks--decision-from-json-alist (alist)
  "Return hook decision plist from parsed JSON ALIST."
  (let* ((specific (or (niwa-hooks--alist-get-string "hookSpecificOutput"
                                                        alist)
                       (niwa-hooks--alist-get-string "hook_specific_output"
                                                        alist)))
         (merged (append specific alist))
         decision)
    (cl-labels
        ((value (&rest keys)
           (cl-loop for key in keys
                    for cell = (or (assoc key merged)
                                   (assq (intern key) merged))
                    when cell return (cdr cell))))
      (when (niwa-hooks--truthy-p
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
                           (niwa-hooks--truthy-p
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
                                      (niwa-hooks--json-value-to-lisp
                                       val))))))
      (let ((updated-input (value "updatedInput" "updated_input")))
        (when (and updated-input
                   (not (plist-get decision :hook-error)))
          (setq decision
                (plist-put decision :updated-input
                           (niwa-hooks--json-tool-input-to-lisp
                            updated-input)))))
      (let ((perm (or (value "permissionDecision" "permission_decision")
                      (value "decision"))))
        (when (and perm
                   (not (plist-get decision :hook-error)))
          (setq decision (plist-put decision :permission-decision perm)))))
    (if (plist-get decision :hook-error)
        decision
      (niwa-hooks--normalize-decision decision))))

(defun niwa-hooks--parse-command-decision (stdout)
  "Parse command hook STDOUT into a decision plist."
  (let ((trimmed (string-trim stdout)))
    (unless (string-empty-p trimmed)
      (condition-case err
          (niwa-hooks--decision-from-json-alist
           (json-parse-string trimmed
                              :object-type 'alist
                              :array-type 'array
                              :null-object nil
                              :false-object :json-false))
        (error
         (list :hook-error
               (format "Could not parse hook JSON: %s"
                       (error-message-string err))))))))

(defun niwa-hooks-merge-decisions (base next)
  "Merge hook decision NEXT into BASE and return the new decision."
  (let ((base (copy-sequence
               (or (niwa-hooks--safe-decision base) nil)))
        (next (niwa-hooks--normalize-decision next)))
    (when next
      ;; Any handler's stop wins, as in Claude Code: a later
      ;; `continue: true' must not cancel it.
      (when (and (plist-member next :continue)
                 (not (and (plist-member base :continue)
                           (not (plist-get base :continue)))))
        (setq base (plist-put base :continue (plist-get next :continue))))
      (dolist (key '(:stop-reason :system-message :permission-reason
				  :updated-input :updated-result))
        (when (plist-member next key)
          (setq base (plist-put base key (plist-get next key)))))
      (when (plist-member next :additional-context)
        (setq base
              (niwa-hooks--plist-put-append
               base :additional-context
               (plist-get next :additional-context))))
      (when-let* ((perm (plist-get next :permission-decision)))
        (let ((existing (plist-get base :permission-decision)))
          (when (> (niwa-hooks--permission-priority perm)
                   (niwa-hooks--permission-priority existing))
            (setq base (plist-put base :permission-decision perm))))))
    base))

(defun niwa-hooks-terminal-decision-p (decision &optional event)
  "Return non-nil when DECISION should stop later hooks for EVENT."
  (let ((decision (niwa-hooks--safe-decision decision)))
    (or (and (memq event '(UserPromptSubmit UserPromptExpansion
                                            PreToolUse PermissionRequest
                                            PreCompact SubagentStart))
             (plist-member decision :continue)
             (not (plist-get decision :continue)))
        (and (memq event '(PreToolUse PermissionRequest))
             (eq (plist-get decision :permission-decision) 'deny)))))

(defun niwa-hooks-event-display-name (event)
  "Return a human-readable display name for hook EVENT."
  (if (symbolp event) (symbol-name event) (format "%s" event)))

(defun niwa-hooks-context-entries (decision event)
  "Return DECISION's added context for EVENT with known host provenance.
Attribution comes from the runner's side table, never from handler-returned
metadata.  Unattributed decisions retain the event label alone."
  (when-let* ((decision (niwa-hooks--safe-decision decision))
              (additional (plist-get decision :additional-context)))
    (let ((sources
           (cl-loop for contribution in
                    (gethash decision niwa-hooks--context-handler-table)
                    append
                    (mapcar (lambda (body) (cons body contribution))
                            (plist-get contribution :contexts)))))
      ;; A caller can change a decision after the runner returns.  Do not
      ;; attach old attribution to replacement or reordered context.
      (unless (equal additional (mapcar #'car sources))
        (setq sources nil))
      (delq nil
            (mapcar
             (lambda (item)
               (let ((source (cdr (pop sources))))
                 (when (and item
                            (not (and (listp item)
                                      (keywordp (car-safe item)))))
                   (let ((entry
                          (list :event (niwa-hooks-event-display-name event)
                                :body (format "%s" item))))
                     (dolist (key '(:source :source-file :plugin-name))
                       (when (plist-member source key)
                         (setq entry (plist-put entry key (plist-get source key)))))
                     entry))))
             additional)))))

(defun niwa-hooks--context-xml-escape (text &optional attribute)
  "Escape TEXT for hook context XML-ish content.
When ATTRIBUTE is non-nil, also escape double quotes."
  (let ((text
         (replace-regexp-in-string
          ">" "&gt;"
          (replace-regexp-in-string
           "<" "&lt;"
           (replace-regexp-in-string
            "&" "&amp;" (or text "") t t)
           t t)
          t t)))
    (if attribute
        (replace-regexp-in-string "\"" "&quot;" text t t)
      text)))

(defun niwa-hooks-format-context (entries)
  "Return model-visible hook context with known source attribution for ENTRIES."
  (let ((entries (seq-filter (lambda (entry) (plist-get entry :body)) entries)))
    (when entries
      (concat
       "<hook-context>\n"
       (mapconcat
        (lambda (entry)
          (format
           "<hook-event name=\"%s\"%s>\n%s\n</hook-event>"
           (niwa-hooks--context-xml-escape
            (format "%s" (plist-get entry :event)) t)
           (mapconcat
            (lambda (field)
              (when-let* ((value (plist-get entry (car field))))
                (format " %s=\"%s\"" (cdr field)
                        (niwa-hooks--context-xml-escape
                         (format "%s" value) t))))
            '((:source . "source") (:source-file . "file")
              (:plugin-name . "plugin"))
            "")
           (niwa-hooks--context-xml-escape
            (format "%s" (plist-get entry :body)))))
        entries
        "\n")
       "\n</hook-context>"))))

(defun niwa-hooks-record-tool-context (context decision &optional event)
  "Append DECISION's additional hook context to CONTEXT.
EVENT labels generated hook event blocks."
  (if-let* ((entries (niwa-hooks-context-entries
                     decision (or event 'PreToolUse))))
      (plist-put context :hook-additional-context
                 (append (plist-get context :hook-additional-context)
                         entries))
    context))

(defun niwa-hooks-record-session-context (session decision &optional event)
  "Append DECISION's additional context for EVENT to SESSION's next prompt."
  (when-let* ((decision (niwa-hooks--safe-decision decision))
              (entries (niwa-hooks-context-entries
                        decision (or event 'SessionStart))))
    (setf (niwa-session-hook-context-pending session)
          (append (niwa-session-hook-context-pending session)
                  entries))))

(defun niwa-hooks-take-session-context (session)
  "Return and clear SESSION's pending hook context as model-visible XML."
  (when-let* ((session session)
              (entries (niwa-session-hook-context-pending session))
              (context (niwa-hooks-format-context entries)))
    (setf (niwa-session-hook-context-pending session) nil)
    context))

(defun niwa-hooks-consume-session-context (session entries)
  "Consume ENTRIES when they still prefix SESSION's pending context.
Context appended after ENTRIES remains pending.  A prefix mismatch leaves the
slot unchanged and returns nil."
  (let ((expected entries)
        (remaining (niwa-session-hook-context-pending session)))
    (while (and expected remaining
                (equal (car expected) (car remaining)))
      (setq expected (cdr expected)
            remaining (cdr remaining)))
    (when (null expected)
      (setf (niwa-session-hook-context-pending session) remaining)
      t)))

(defun niwa-hooks-record-session-reminder (event session decision)
  "Queue model-visible hook guidance for EVENT, SESSION, and DECISION."
  (when-let* ((session session)
              (decision (niwa-hooks--safe-decision decision)))
    (when (niwa-hooks--decision-blocking-p decision)
      (niwa-reminders-queue-turn-event
       (current-buffer) (list 'hook event)
       (format "%s hook blocked the previous operation: %s. Adapt your next action to respect this hook decision instead of retrying the same blocked operation unchanged."
               event
               (or (niwa-hooks-decision-reason decision)
                   "no reason provided"))))))


;;
;;; Event payloads and logging

(defun niwa-hooks-event-plist (event &optional session workspace &rest extra)
  "Build a generic hook plist for EVENT, SESSION, WORKSPACE, and EXTRA."
  (let* ((workspace (or workspace
                        (and session (niwa-session-workspace session))))
         (workspace-root (and workspace (niwa-workspace-root workspace)))
         (cwd (or (and session (niwa-session-working-directory session))
                  workspace-root
                  default-directory))
         (plist (list :hook-event-name event
                      :session-id (and session
                                       (niwa-session-session-id session))
                      :transcript-path (and session
                                            (niwa-session-save-path session))
                      :cwd cwd
                      :workspace-root workspace-root
                      :model (and (boundp 'gptel-model) gptel-model)
                      :turn-id (and session
                                    (niwa-current-turn session))
                      :origin "/root")))
    (while extra
      (setq plist (plist-put plist (pop extra) (pop extra))))
    plist))

(defun niwa-hooks-tool-event-plist (event context &rest extra)
  "Build tool hook plist for EVENT from pipeline CONTEXT and EXTRA."
  (let* ((tool (plist-get context :tool))
         (session (plist-get context :session))
         (workspace (plist-get context :workspace))
         (invocation (plist-get context :invocation))
         (base (niwa-hooks-event-plist
                event session workspace
                :cwd (plist-get context :default-directory)
                :origin (or (plist-get context :origin)
                            (and invocation
                                 (fboundp 'niwa-agent-invocation-p)
                                 (niwa-agent-invocation-p invocation)
                                 (niwa-agent-invocation-path
                                  invocation))
                            "/root"))))
    (append
     base
     (list :tool-name (and tool (niwa-tool-name tool))
           :tool-category (and tool (niwa-tool-category tool))
           :tool-use-id (plist-get context :tool-use-id)
           :parent-tool-use-id (plist-get context :parent-tool-use-id)
           :call-source (plist-get context :call-source)
           :tool-input (plist-get context :args))
     extra)))

(defun niwa-hooks-additional-context-string (decision &optional event)
  "Return model-visible additional context from hook DECISION, or nil.
EVENT labels each generated hook event block."
  (niwa-hooks-format-context
   (niwa-hooks-context-entries decision (or event 'UserPromptSubmit))))

(defun niwa-hooks-log-path (session)
  "Return the persistent hook log path for SESSION, or nil."
  (when-let* ((save-path (and session (niwa-session-save-path session))))
    (file-name-concat save-path "hook-log.el")))

(defun niwa-hooks--persist-log-content (session content)
  "Append serialized hook log CONTENT for SESSION."
  (when-let* ((file (and niwa-hooks-persist-log
                         (niwa-hooks-log-path session))))
    (condition-case err
        (let ((target (niwa-session-execution-target session)))
          (if (and target
                   (niwa-execution-target-remote-p target))
              (progn
                (niwa-session-publication-append-diagnostic
                 session file content))
            (let ((coding-system-for-write 'utf-8-unix))
              (condition-case nil
                  (write-region content nil file t 'silent)
                (file-missing
                 (make-directory (file-name-directory file) t)
                 (write-region content nil file t 'silent))))
            t))
      (error
       (niwa--warn-once
        (niwa-diagnostic-scope session)
        'hooks-log-persistence
        "Hook log persistence failed: %s"
        (error-message-string err))
       nil))))

(defun niwa-hooks--persist-log-entry (session entry)
  "Append sanitized hook log ENTRY to SESSION's persistent hook log."
  (niwa-hooks--persist-log-content
   session (niwa--diagnostic-entry-text entry)))

(defun niwa-hooks-flush-log (session)
  "Persist SESSION's queued hook diagnostics, retaining failures."
  (when session
    (let* ((pending (niwa-session-hook-log-pending session))
           (target (niwa-session-execution-target session))
           (remote-p
            (and target
                 (niwa-execution-target-remote-p target))))
      (if (and pending remote-p)
          (when (niwa-hooks--persist-log-content
                 session (mapconcat #'niwa--diagnostic-entry-text
                                    pending ""))
            (setf (niwa-session-hook-log-pending session) nil))
        (while (and pending
                    (niwa-hooks--persist-log-entry session (car pending)))
          (setq pending (cdr pending)))
        (setf (niwa-session-hook-log-pending session) pending)))))

(defun niwa-hooks--log (session entry)
  "Append hook log ENTRY to SESSION."
  (when session
    (let ((log (append (niwa-session-hook-log session)
                       (list entry))))
      (when (and niwa-hooks-log-limit
                 (> (length log) niwa-hooks-log-limit))
        (setq log (last log niwa-hooks-log-limit)))
      (setf (niwa-session-hook-log session) log))
    (when niwa-hooks-persist-log
      (let* ((pending (niwa-session-hook-log-pending session))
             (target (niwa-session-execution-target session))
             (remote-p
              (and target
                   (niwa-execution-target-remote-p target))))
        (when (or pending remote-p
                  (not (niwa-hooks--persist-log-entry session entry)))
          (setf (niwa-session-hook-log-pending session)
                (append pending (list entry))))))
    ;; A persisted hook log already keeps every handler run of a durable
    ;; session.  Telemetry repeats it only during a profiler run, or as the
    ;; value-free audit of a side conversation, whose own log is transient.
    (when (or (not niwa-hooks-persist-log)
              (niwa-telemetry-forwarded-audit-p session)
              (niwa-telemetry-detailed-p session))
      (let ((handler (plist-get entry :handler)))
        (niwa-telemetry-record-audit
         session 'hook-handler
         :hook-event (plist-get entry :event)
         :handler-id (and handler
                          (niwa-hooks--telemetry-handler-id handler))
         :handler-type (and (listp handler) (plist-get handler :type))
         :handler-source (and (listp handler)
                              (plist-get handler :source))
         :status (plist-get entry :status)
         :duration-ms (and (numberp (plist-get entry :elapsed))
                           (round (* 1000 (plist-get entry :elapsed))))
         :exit-status (plist-get entry :exit-status))))))

(defun niwa-hooks--telemetry-handler-id (handler)
  "Return a stable non-payload identifier for declarative HANDLER."
  (substring
   (secure-hash
    'sha256
    (prin1-to-string
     (mapcar (lambda (key) (cons key (plist-get handler key)))
             '(:type :source :plugin-name :function :command))))
   0 20))

(defun niwa-hooks--log-entry (event event-plist handler status &rest props)
  "Build a log entry for EVENT, EVENT-PLIST, HANDLER, STATUS, and PROPS."
  (append (list :event event
                :handler handler
                :status status
                :time (format-time-string "%FT%T%z"))
          (when (and (eq event 'SessionStart)
                     (plist-get event-plist :source))
            (list :event-source (plist-get event-plist :source)))
          props))

(defun niwa-hooks--surface (text)
  "Surface informational hook TEXT."
  (message "niwa: %s" text))

(defun niwa-hooks--progress-snapshot ()
  "Return the leased view progress and its buffer, or nil."
  (when-let* ((view-buffer (and (boundp 'niwa--view-buffer)
                                niwa--view-buffer))
              (_ (buffer-live-p view-buffer))
              (lease (niwa-view-stream-lease-progress view-buffer)))
    (list :buffer view-buffer :status lease)))

(defun niwa-hooks--claim-progress (lease text owner)
  "Replace LEASE with hook progress TEXT owned by OWNER.
Return a release token, or nil when LEASE no longer applies."
  (when-let* ((view-buffer (plist-get lease :buffer)))
    (ignore-errors
      (niwa-view-stream-claim-leased-progress
       view-buffer (plist-get lease :status) text owner))))

(defun niwa-hooks--restore-progress (lease token)
  "Release TOKEN into LEASE's view while its owner still owns the status."
  (when-let* ((view-buffer (plist-get lease :buffer)))
    (ignore-errors
      (niwa-view-stream-release-progress view-buffer token))))

(defun niwa-hooks--decision-blocking-p (decision)
  "Return non-nil when DECISION blocks the triggering operation."
  (let ((decision (niwa-hooks--safe-decision decision)))
    (or (and (plist-member decision :continue)
             (not (plist-get decision :continue)))
        (eq (plist-get decision :permission-decision) 'deny))))

(defun niwa-hooks-decision-reason (decision)
  "Return a user-facing reason string from DECISION, or nil."
  (let ((decision (niwa-hooks--safe-decision decision)))
    (or (plist-get decision :stop-reason)
        (plist-get decision :permission-reason)
        (plist-get decision :system-message))))

(defun niwa-hooks--context-contribution (handler decision)
  "Return HANDLER attribution for DECISION's added context."
  (when-let* ((decision (niwa-hooks--normalize-decision decision))
              (contexts (plist-get decision :additional-context)))
    (let ((contribution
           (list :contexts
                 (mapcar (lambda (value)
                           (if (stringp value) (copy-sequence value)
                             (copy-tree value t)))
                         contexts))))
      (dolist (key '(:source :source-file :plugin-name :description :function :command))
        (when (plist-member handler key)
          (setq contribution
                (plist-put contribution key (plist-get handler key)))))
      (when-let* ((reason (niwa-hooks-decision-reason decision)))
        (setq contribution (plist-put contribution :reason reason)))
      contribution)))

(defun niwa-hooks-record-tool-audit (context records)
  "Append hook audit RECORDS to CONTEXT."
  (let ((records (if (and (listp records)
                          (keywordp (car-safe records)))
                     (list records)
                   records)))
    (if records
        (plist-put context :hook-audit-records
                   (append (plist-get context :hook-audit-records)
                           records))
      context)))

(defun niwa-hooks-tool-permission-audit-record
    (event outcome decision &optional reason)
  "Return a permission audit record for hook EVENT and OUTCOME."
  (append
   (list :type 'tool-permission
         :event (niwa-hooks-event-display-name event)
         :outcome (format "%s" outcome))
   (when-let* ((reason (or reason
                           (niwa-hooks-decision-reason decision))))
     (list :reason reason))))

(defun niwa-hooks-context-audit-records
    (decision event type &optional omit-context)
  "Return hook audit records for DECISION context at EVENT.
TYPE is stored as the audit record type.  When OMIT-CONTEXT is non-nil,
record only that context was added, without duplicating the body."
  (when-let* ((handlers (gethash decision
                                 niwa-hooks--context-handler-table)))
      (list
       (list :type type
             :event (niwa-hooks-event-display-name event)
             :handlers
             (mapcar
              (lambda (contribution)
                (let (handler)
                  (dolist (key '(:source :source-file :plugin-name :description :function
                                :command :reason))
                    (when (plist-member contribution key)
                      (setq handler
                            (plist-put handler key
                                       (plist-get contribution key)))))
                  (unless omit-context
                    (setq handler
                          (plist-put handler :contexts
                                     (plist-get contribution :contexts))))
                  handler))
              handlers)))))

(defun niwa-hooks--surface-final-decision (event session decision)
  "Surface user-visible fields from hook DECISION for EVENT and SESSION."
  (let ((decision (niwa-hooks--safe-decision decision)))
    (niwa-hooks-record-session-reminder event session decision)
    (cond
     ((and (not (memq event '(PostToolUse PostToolUseFailure
                                          PostCompact SubagentStop
                                          Stop StopFailure SessionEnd)))
           (niwa-hooks--decision-blocking-p decision))
      (niwa-diagnostic-message
       (niwa-diagnostic-scope session)
       "%s hook blocked: %s"
               (niwa-hooks-event-display-name event)
               (or (niwa-hooks-decision-reason decision)
                   "no reason provided")))
     ((plist-get decision :system-message)
      (niwa-hooks--surface
       (format "%s hook: %s"
               (niwa-hooks-event-display-name event)
               (plist-get decision :system-message)))))))


;;
;;; Command execution

(defun niwa-hooks--keyword-to-json-key (keyword)
  "Convert Lisp KEYWORD to snake_case JSON key string."
  (replace-regexp-in-string "-" "_"
                            (substring (symbol-name keyword) 1)))

(defun niwa-hooks--json-encode-value (value)
  "Convert Lisp VALUE to a shape suitable for `json-serialize'."
  (cond
   ((eq value t) t)
   ((null value) :null)
   ((memq value '(:json-false :false)) :false)
   ((eq value :null) :null)
   ((stringp value)
    (niwa--normalize-message-text (substring-no-properties value)))
   ((numberp value)
    value)
   ((hash-table-p value)
    (let ((table (make-hash-table :test (hash-table-test value))))
      (maphash
       (lambda (key val)
         (puthash key (niwa-hooks--json-encode-value val) table))
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
                           (niwa-hooks--json-encode-value raw))))
          (puthash (niwa-hooks--keyword-to-json-key key)
                   encoded
                   table)))
      table))
   ((listp value)
    (mapcar #'niwa-hooks--json-encode-value value))
   ((symbolp value)
    (symbol-name value))
   (t
    (format "%S" value))))

(defun niwa-hooks--event-json (event-plist)
  "Return JSON string for EVENT-PLIST."
  (json-serialize (niwa-hooks--json-encode-value event-plist)))

(defun niwa-hooks--failure-decision (handler reason)
  "Return failure decision for HANDLER and REASON."
  (if (plist-get handler :fail-closed)
      (list :continue nil :stop-reason reason)
    nil))

(defun niwa-hooks--block-decision (event reason)
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

(defun niwa-hooks--apply-decision-to-event-plist
    (event event-plist decision)
  "Return EVENT-PLIST for EVENT updated with mutating fields from DECISION."
  (let ((payload (copy-sequence event-plist))
        (decision (niwa-hooks--safe-decision decision)))
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
                       (plist-get decision :updated-result)))
      (setq payload
            (plist-put payload :tool-response
                       (plist-get decision :updated-result))))
    payload))

(defun niwa-hooks--command-process-environment (handler session)
  "Return process environment for command HANDLER in SESSION."
  (let* ((source (plist-get handler :source))
         (root (and (eq source 'plugin)
                    (plist-get handler :plugin-root)))
         (data (and (eq source 'plugin)
                    (plist-get handler :plugin-data)))
         (target (and session
                      (niwa-session-execution-target session)))
         (target-command-p
          (and target
               (niwa-execution-target-remote-p target)
               (or (eq source 'project-file)
                   (and (eq source 'plugin)
                        (stringp root)
                        (file-remote-p root)))))
         (base-environment (unless target-command-p process-environment)))
    (if (not (eq source 'plugin))
        base-environment
      (if (and (stringp root) (stringp data))
          (let ((env-root root)
                (env-data data))
            (cond
             ((file-remote-p root)
              (unless (and target (file-remote-p data))
                (signal 'niwa-execution-target-error
                        (list "Target plugin data is not on the plugin target")))
              (setq env-root
                    (niwa-execution-target-native-path target root)
                    env-data
                    (niwa-execution-target-native-path target data)))
             ((file-remote-p data)
              ;; A client-local command cannot consume a TRAMP file name.
              ;; Structured hook input still carries the target workspace.
              (setq env-data nil)))
            (when env-data
              (make-directory data t))
            (append
             (list (concat "PLUGIN_ROOT=" env-root)
                   (concat "CLAUDE_PLUGIN_ROOT=" env-root)
                   (concat "NIWA_PLUGIN_ROOT=" env-root))
             (when env-data
               (list (concat "PLUGIN_DATA=" env-data)
                     (concat "CLAUDE_PLUGIN_DATA=" env-data)
                     (concat "NIWA_PLUGIN_DATA=" env-data)))
             base-environment))
        base-environment))))

(defun niwa-hooks--command-default-directory
    (handler event-plist session)
  "Return HANDLER's execution directory for EVENT-PLIST and SESSION."
  (let* ((source (plist-get handler :source))
         (plugin-root (and (eq source 'plugin)
                           (or (plist-get handler :plugin-root)
                               (plist-get handler :source-root))))
         (directory
          (file-name-as-directory
           (or (pcase source
                 ('project-file (plist-get event-plist :workspace-root))
                 ('user (plist-get handler :source-root))
                 ('user-file (plist-get handler :source-root))
                 ('plugin
                  (cl-find-if
                   (lambda (candidate)
                     (and candidate
                          (or (not plugin-root)
                              (niwa-execution-target-same-path-domain-p
                               plugin-root candidate))))
                   (list (plist-get event-plist :cwd)
                         (and session (niwa-session-working-directory session))
                         (plist-get event-plist :workspace-root)
                         plugin-root))))
               (plist-get event-plist :cwd)
               (plist-get event-plist :workspace-root)
               default-directory)))
         (target (and session (niwa-session-execution-target session))))
    (unless (memq source '(project-file user user-file plugin))
      (signal 'niwa-execution-target-error
              (list "Command hook has no trusted execution origin")))
    (when target
      ;; Check the code's origin even when it runs from the conversation cwd.
      (when (and plugin-root (file-remote-p plugin-root))
        (niwa-execution-target-native-path target plugin-root))
      (cond
       ((file-remote-p directory)
        (niwa-execution-target-native-path target directory))
       ((and (eq source 'project-file)
             (niwa-execution-target-remote-p target))
        (signal 'niwa-execution-target-error
                (list (format "Project hook is not on the session execution target: %s"
                              directory))))))
    directory))

(defun niwa-hooks--command-event-plist (event-plist session)
  "Return EVENT-PLIST in SESSION's target-native command-hook domain."
  (if-let* ((target (and session
                         (niwa-session-execution-target session))))
      (progn
        (cl-labels
            ((native-input (value)
               (cond
                ((and (stringp value) (file-remote-p value))
                 (niwa-execution-target-native-path target value))
                ((consp value) (mapcar #'native-input value))
                (t value))))
          (let ((payload (copy-sequence event-plist)))
            (dolist (key '(:cwd :workspace-root :transcript-path))
              (when-let* ((path (plist-get payload key)))
                (setq payload
                      (plist-put
                       payload key
                       (niwa-execution-target-native-path target path)))))
            (when (plist-member payload :tool-input)
              (setq payload
                    (plist-put payload :tool-input
                               (native-input
                                (plist-get payload :tool-input)))))
            (plist-put payload :execution-target
                       (niwa-execution-target-identity target)))))
    event-plist))

(defun niwa-hooks--command-process-command (command remote)
  "Return the shell argv for hook COMMAND, accounting for REMOTE transport."
  ;; Read the event before launching a command that may immediately close stdin.
  (list "bash" "-c"
        (concat (when remote "printf '\\000'; ")
                "IFS= read -r niwa_hook_input; "
                "printf '%s\\n' \"$niwa_hook_input\" | \"$@\"")
        "niwa-hook"
        (if remote "bash" shell-file-name)
        (if remote "-c" shell-command-switch)
        command))

(defun niwa-hooks--run-command-handler
    (event handler event-plist session request callback)
  "Run command HANDLER for EVENT and call CALLBACK with decision."
  (let* ((command (plist-get handler :command))
         (scope (or (plist-get event-plist :workspace-root) (niwa-diagnostic-scope session)))
         (timeout (niwa-hooks--command-timeout handler))
         (default-directory
          (niwa-hooks--command-default-directory
           handler event-plist session))
         (remote (file-remote-p default-directory))
         (stdout-buffer (generate-new-buffer " *niwa-hook-stdout*"))
         (stderr-buffer (generate-new-buffer " *niwa-hook-stderr*"))
         (completion-key (list 'hook-completion stdout-buffer))
         (completion-path default-directory)
         (start-time (float-time))
         (settled nil)
         (completion-requested nil)
         (stdout-truncated nil)
         (stderr-truncated nil)
         (stdin-ready (not remote))
         input process stderr-cap-timer stderr-process timer)
    (cl-labels
        ((truncation-marker ()
           (format "\n... Hook output truncated at %d character limit."
                   niwa-hooks-command-output-max-chars))
         (append-buffer-output (buffer chunk stream)
           (when (buffer-live-p buffer)
             (with-current-buffer buffer
               (let ((max niwa-hooks-command-output-max-chars))
                 (cond
                  ((null max)
                   (goto-char (point-max))
                   (insert chunk))
                  ((>= (buffer-size) max)
                   (unless (if (eq stream 'stdout)
                               stdout-truncated
                             stderr-truncated)
                     (goto-char (point-max))
                     (insert (truncation-marker))
                     (if (eq stream 'stdout)
                         (setq stdout-truncated t)
                       (setq stderr-truncated t))))
                  (t
                   (let ((remaining (- max (buffer-size))))
                     (goto-char (point-max))
                     (insert (substring chunk 0 (min remaining (length chunk))))
                     (when (> (length chunk) remaining)
                       (insert (truncation-marker))
                       (if (eq stream 'stdout)
                           (setq stdout-truncated t)
                         (setq stderr-truncated t))))))))))
         (truncate-remote-stderr ()
           (setq stderr-cap-timer nil)
           (when (and (buffer-live-p stderr-buffer)
                      niwa-hooks-command-output-max-chars)
             (with-current-buffer stderr-buffer
               (when (> (buffer-size)
                        niwa-hooks-command-output-max-chars)
                 (let ((prefix
                        (buffer-substring-no-properties
                         (point-min)
                         (+ (point-min)
                            niwa-hooks-command-output-max-chars)))
                       (inhibit-modification-hooks t))
                   (erase-buffer)
                   (insert prefix (truncation-marker))
                   (setq stderr-truncated t))))))
         (cap-remote-stderr (&rest _)
           (unless (timerp stderr-cap-timer)
             (setq stderr-cap-timer
                   (run-at-time 0 nil #'truncate-remote-stderr))))
         (buffer-string-safe (buffer)
           (if (buffer-live-p buffer)
               (with-current-buffer buffer
                 (buffer-substring-no-properties (point-min) (point-max)))
             ""))
         (release-children ()
           ;; Cancel the timers and kill the child explicitly: killing its
           ;; buffer would collect it, but only while this code still owns
           ;; that buffer.
           (when (timerp timer) (cancel-timer timer))
           (when (timerp stderr-cap-timer)
             (cancel-timer stderr-cap-timer))
           (when (and process (process-live-p process))
             (delete-process process))
           (when (and stderr-process (process-live-p stderr-process))
             (delete-process stderr-process)))
         (cancel ()
           ;; A request teardown drains this while the child runs.  The
           ;; handler still owes one log entry and its settlement: without
           ;; them the event's telemetry span stays open and the slow
           ;; notice later reports a dead child as still running.
           (unless settled
             (setq settled t)
             (niwa-transport-cancel-pending completion-key)
             (condition-case-unless-debug teardown-error (release-children)
               (error (niwa-diagnostic-message scope "Hook teardown failed: %S" teardown-error)))
             (when (buffer-live-p stdout-buffer) (kill-buffer stdout-buffer))
             (when (buffer-live-p stderr-buffer) (kill-buffer stderr-buffer))
             (niwa-hooks--log
              session
              (niwa-hooks--log-entry
               event event-plist handler 'cancelled
               :elapsed (- (float-time) start-time)
               :exit-status 'cancelled))
             (funcall callback 'cancelled)))
         (complete (status reason)
           (unless settled
             (setq settled t)
             (truncate-remote-stderr)
             (let* ((stdout (buffer-string-safe stdout-buffer))
                    (stderr (buffer-string-safe stderr-buffer))
                    (elapsed (- (float-time) start-time))
                    decision log-status)
               (pcase status
                 ('ok
                  (setq decision (niwa-hooks--parse-command-decision
                                  stdout))
                  (if (plist-get (niwa-hooks--safe-decision decision)
                                 :hook-error)
                      (progn
                        (setq log-status 'parse-error)
                        (setq decision
                              (niwa-hooks--failure-decision
                               handler
                               (plist-get decision :hook-error))))
                    (setq log-status 'ok)))
                 ('block
                  (setq log-status 'block)
                  (setq decision
                        (niwa-hooks--block-decision
                         event
                         (string-trim
                          (if (string-empty-p stderr) stdout stderr)))))
                 ('timeout
                  (setq log-status 'timeout)
                  (setq decision
                        (niwa-hooks--failure-decision
                         handler
                         (format "Hook timed out after %s seconds"
                                 timeout))))
                 (_
                  (setq log-status 'error)
                  (setq decision
                        (niwa-hooks--failure-decision
                         handler
                         (or reason
                             (string-trim
                              (if (string-empty-p stderr) stdout stderr))
                             "Hook command failed")))))
               (niwa-hooks--log
                session
                (niwa-hooks--log-entry
                 event event-plist handler log-status
                 :elapsed elapsed
                 :exit-status status
                 :stdout-preview (substring stdout 0 (min 1000 (length stdout)))
                 :stderr-preview (substring stderr 0 (min 1000 (length stderr)))
                 :decision decision))
               (when (buffer-live-p stdout-buffer) (kill-buffer stdout-buffer))
               (when (buffer-live-p stderr-buffer) (kill-buffer stderr-buffer))
               (funcall callback decision))))
         (send-input ()
           ;; Readiness can arrive before make-process returns.  Do not send
           ;; until the returned handle and its deadline belong to this runner.
           (when (and stdin-ready input process (or (null timeout) timer)
                      (not settled) (not completion-requested))
             (let ((payload input))
               ;; Claim the write before yielding to a filter or cancellation.
               (setq input nil)
               (condition-case err
                   (progn
                     (process-send-string process payload)
                     (unless (or remote settled completion-requested)
                       (process-send-eof process)))
                 (error
                  (if (memq (process-status process) '(exit signal))
                      (process-exited process "")
                    (finish 'error (error-message-string err))))))))
         (receive-output (_proc chunk)
           (unless (or settled completion-requested (string-empty-p chunk))
             (if stdin-ready
                 (append-buffer-output stdout-buffer chunk 'stdout)
               ;; The exec'd wrapper acknowledges with one NUL byte.  Sending
               ;; earlier lets TRAMP's preceding shell read ahead into JSON
               ;; and discard it on exec, leaving the wrapper waiting forever.
               ;; A one-byte prefix cannot split across filter deliveries.
               (if (= (aref chunk 0) 0)
                   (progn
                     (setq stdin-ready t)
                     (append-buffer-output stdout-buffer (substring chunk 1)
                                           'stdout)
                     (send-input))
                 (append-buffer-output stdout-buffer chunk 'stdout)
                 (finish 'error "Remote hook did not acknowledge stdin")))))
         (continue-startup ()
           (when (or settled completion-requested)
             (throw 'hook-startup-done nil)))
         (process-exited (proc _event)
           (when (memq (process-status proc) '(exit signal))
             ;; A local stderr pipe is independent of the main process.  Its
             ;; final bytes may still be pending when this sentinel runs.
             ;; A surviving writer must not monopolize this sentinel, even
             ;; when the user disabled output truncation.
             (let ((deadline (+ (float-time) 0.05)))
               (while (and stderr-process (not stderr-truncated)
                           (not settled) (not completion-requested)
                           (< (float-time) deadline)
                           (accept-process-output stderr-process 0 nil t))))
             (let ((code (process-exit-status proc)))
               (cond
                ((= code 0) (finish 'ok nil))
                ((= code 2) (finish 'block nil))
                (t (finish 'error
                           (format "Hook exited with status %s" code)))))))
         (finish (status reason)
           (unless (or settled completion-requested)
             ;; Keep the first terminal result, including while deferred.
             (setq completion-requested t)
             (condition-case-unless-debug teardown-error (release-children)
               (error (niwa-diagnostic-message scope "Hook teardown failed: %S" teardown-error)))
             ;; A local child's sentinel can run inside unrelated TRAMP I/O.
             ;; The next handler may be remote; keep cancellation live until
             ;; the transport owner can safely deliver this continuation.
             (if (niwa-transport-busy-p completion-path)
                 (unless (niwa-transport-run-when-idle
                          completion-key completion-path
                          (lambda () (complete status reason)) #'cancel)
                   (cancel))
               (complete status reason)))))
      (when request
        (niwa-request-push-canceller request #'cancel))
      (unwind-protect
          (catch 'hook-startup-done
            (continue-startup)
            (condition-case err
                (let ((process-environment
                       (niwa-hooks--command-process-environment
                        handler session))
                      process-command)
                  (continue-startup)
                  (setq input (concat (niwa-hooks--event-json
                                       (niwa-hooks--command-event-plist
                                        event-plist session)) "\n")
                        process-command
                        (niwa-hooks--command-process-command command remote))
                  (continue-startup)
                  (if remote
                      (with-current-buffer stderr-buffer
                        (add-hook 'after-change-functions
                                  #'cap-remote-stderr nil t))
                    (setq stderr-process
                          (make-pipe-process
                           :name "niwa-hook-stderr"
                           :buffer nil
                           :filter (lambda (_proc chunk)
                                     (append-buffer-output
                                      stderr-buffer chunk 'stderr))
                           :noquery t)))
                  (continue-startup)
                  (setq process
                        (make-process
                         :name "niwa-hook"
                         :buffer stdout-buffer
                         :filter #'receive-output
                         :stderr (or stderr-process stderr-buffer)
                         :command process-command
                         :connection-type 'pipe
                         :file-handler t
                         :noquery t
                         :sentinel (if remote #'ignore #'process-exited)))
                  (when remote
                    ;; TRAMP drains stderr after the supplied sentinel.
                    (add-function :after (process-sentinel process)
                                  #'process-exited)
                    (when (memq (process-status process) '(exit signal))
                      (funcall (process-sentinel process) process "")))
                  (continue-startup)
                  ;; Bound stdin writes once the child is running.
                  (when timeout
                    (setq timer
                          (run-at-time
                           timeout nil
                           (lambda () (finish 'timeout nil)))))
                  (continue-startup)
                  ;; TRAMP sends exec before installing our filter.  A yielding
                  ;; send can leave the acknowledgment in the default buffer.
                  (when (and remote (not stdin-ready))
                    (let ((pending (buffer-string-safe stdout-buffer)))
                      (unless (string-empty-p pending)
                        (with-current-buffer stdout-buffer (erase-buffer))
                        (receive-output process pending))))
                  (send-input))
              (error
               (finish 'error (error-message-string err)))))
        ;; Acquisition can yield before assigning the returned handle.  Teardown
        ;; may already have run; collect those late resources without redelivery.
        (when (or settled completion-requested)
          (condition-case-unless-debug teardown-error (release-children)
            (error (niwa-diagnostic-message scope "Hook teardown failed: %S" teardown-error))))))))


;;
;;; Handler runner

(defun niwa-hooks--elisp-event-plist (event-plist handler)
  "Return EVENT-PLIST prepared for Elisp HANDLER.
Declarative handlers receive handler metadata; native hooks retain the
stable public event payload."
  (if (eq (plist-get handler :source) 'native)
      event-plist
    (plist-put (copy-sequence event-plist) :hook-handler handler)))

(defun niwa-hooks--run-elisp-handler
    (event handler event-plist session)
  "Run Elisp HANDLER for EVENT and return its decision."
  (let ((fn (plist-get handler :function)))
    (condition-case err
        (if (functionp fn)
            (let* ((decision (funcall fn
                                      (niwa-hooks--elisp-event-plist
                                       event-plist handler)))
                   (normalized (niwa-hooks--normalize-decision
                                decision)))
              (if-let* ((reason (plist-get normalized :hook-error)))
                  (progn
                    (niwa-hooks--log
                     session
                     (niwa-hooks--log-entry
                      event event-plist handler 'error :error reason))
                    (niwa-hooks--failure-decision handler reason))
                (niwa-hooks--log
                 session
                 (niwa-hooks--log-entry
                  event event-plist handler 'ok :decision decision))
                decision))
          (let ((reason (format "Unknown hook function: %S" fn)))
            (niwa-hooks--log
             session
             (niwa-hooks--log-entry
              event event-plist handler 'error :error reason))
            (niwa-hooks--failure-decision handler reason)))
      (error
       (let ((reason (error-message-string err)))
         (niwa-hooks--log
          session
          (niwa-hooks--log-entry
           event event-plist handler 'error :error reason))
         (niwa-hooks--failure-decision handler reason))))))

(defun niwa-hooks--run-handlers
    (event handlers event-plist session request decision callback
           &optional dispatch-buffer context-handlers)
  "Run HANDLERS for EVENT serially, then call CALLBACK."
  (if (and dispatch-buffer
           (buffer-live-p dispatch-buffer)
           (not (eq (current-buffer) dispatch-buffer)))
      (with-current-buffer dispatch-buffer
        (niwa-hooks--run-handlers
         event handlers event-plist session request decision callback
         dispatch-buffer context-handlers))
    (if (or (null handlers)
            (niwa-hooks-terminal-decision-p decision event))
        (progn
          (when (and decision context-handlers)
            (puthash decision context-handlers
                     niwa-hooks--context-handler-table))
          (funcall callback decision))
      (let* ((handler (car handlers))
             (handler-span
              (and session
                   (niwa-telemetry-detailed-p session)
                   (niwa-telemetry-start
                    session 'hook-handler-lifecycle
                    :hook-event event
                    :handler-id (niwa-hooks--telemetry-handler-id handler)
                    :handler-type (plist-get handler :type)
                    :handler-source (plist-get handler :source)))))
        (cl-labels
            ((advance (handler-decision)
               (if (eq handler-decision 'cancelled)
                   ;; The request is being torn down: settle the spans and
                   ;; the event, but run no further handler.
                   (progn
                     (when handler-span
                       (niwa-telemetry-finish
                        handler-span :outcome 'cancelled))
                     (funcall callback 'cancelled))
               (let* ((context
                       (niwa-hooks-additional-context-string
                        handler-decision event))
                      (contribution
                       (niwa-hooks--context-contribution
                        handler handler-decision))
                      (next (niwa-hooks-merge-decisions
                             decision handler-decision))
                      (next-context-handlers
                       (if contribution
                           (append context-handlers (list contribution))
                         context-handlers))
                      (next-plist
                       (niwa-hooks--apply-decision-to-event-plist
                        event event-plist next)))
                 (when handler-span
                   (niwa-telemetry-finish
                    handler-span
                    :outcome
                    (if (niwa-hooks--decision-blocking-p handler-decision)
                        'blocked
                      'continued)
                    :context-chars (and context (length context))
                    :context-deduplicated nil))
                 (niwa-hooks--run-handlers
                  event (cdr handlers) next-plist session request next callback
                  dispatch-buffer next-context-handlers)))))
          (pcase (plist-get handler :type)
            ('elisp
             (advance
              (niwa-hooks--run-elisp-handler
               event handler event-plist session)))
            ('command
             (condition-case err
                 (niwa-hooks--run-command-handler
                  event handler event-plist session request #'advance)
               (niwa-execution-target-error
                (let ((reason (error-message-string err)))
                  (niwa-hooks--log
                   session
                   (niwa-hooks--log-entry
                    event event-plist handler 'error :error reason))
                  (advance (niwa-hooks--block-decision event reason))))))
            (_ (advance nil))))))))

(defun niwa-hooks-run-tool-event
    (event event-plist callback context session workspace request invocation)
  "Run hook EVENT with EVENT-PLIST in CONTEXT's live dispatch buffer.

CALLBACK, SESSION, WORKSPACE, REQUEST, and INVOCATION are forwarded to
the hook runner."
  (let ((buffer (plist-get context :buffer)))
    (if (buffer-live-p buffer)
        (with-current-buffer buffer
          (niwa-hooks-run-event
           event event-plist callback session workspace request invocation))
      (niwa-hooks-run-event
       event event-plist callback session workspace request invocation))))

(defun niwa-hooks-run-event
    (event event-plist callback &optional session workspace request invocation)
  "Run hooks for EVENT and call CALLBACK with merged decision.

EVENT-PLIST is the stable niwa event payload passed to Elisp hooks
and serialized to JSON for command hooks.  CALLBACK receives nil or a
decision plist."
  (let* ((event (niwa-hooks--event-symbol event))
         (session (or session
                      (and (boundp 'niwa--session) niwa--session)))
         (workspace (or workspace
                        (and session (niwa-session-workspace session))))
         (request (or request
                      (and (boundp 'niwa--current-request)
                           niwa--current-request)))
         (invocation (or invocation
                         niwa--agent-invocation))
         (dispatch-buffer (current-buffer)))
    (if (null event)
        (funcall callback nil)
      (let* ((payload (plist-put (copy-sequence event-plist)
                                 :hook-event-name event))
             (rules (niwa-hooks-effective-rules
                     session workspace request invocation))
             (handlers (niwa-hooks--handlers-for-event
                        event payload rules))
             (telemetry-span
              (when (and session
                         (or handlers
                             (niwa-telemetry-detailed-p session)))
                (niwa-telemetry-start
                 session 'hook-event
                 :hook-event event
                 :handler-count (length handlers))))
             (settled nil)
             (slow-surfaced nil)
             (slow-echoed nil)
             (progress-lease (and handlers
                                  (niwa-hooks--progress-snapshot)))
             (progress-owner (gensym "hook-progress-"))
             slow-timer)
        (cl-labels
            ((finish (decision)
               (unless settled
                 (setq settled t)
                 (unless (eq decision 'cancelled)
                   (setq decision
                         (niwa-hooks-sanitize-final-decision
                          event decision (niwa-diagnostic-scope session))))
	         (when telemetry-span
	           (niwa-telemetry-finish
	            telemetry-span
	            :outcome
	            (cond
	             ((eq decision 'cancelled) 'cancelled)
	             ((niwa-hooks--decision-blocking-p decision)
	              'blocked)
	             (t 'continued))
	            :context-chars
	            (when-let* (((not (eq decision 'cancelled)))
	                        (context
	                         (niwa-hooks-additional-context-string
	                          decision event)))
	              (length context))))
	         (when (timerp slow-timer)
	           (cancel-timer slow-timer))
                 ;; The slow notice is a bare echo message that
                 ;; would otherwise linger indefinitely and read
                 ;; as a hang after the hook completes.
                 (when slow-echoed
                   (niwa-hooks--surface
                    (format "%s hook finished"
                            (niwa-hooks-event-display-name
                             event))))
                 (when slow-surfaced
                   (let ((restored
                          (niwa-hooks--restore-progress
                           progress-lease slow-surfaced)))
                     (when session
                       (niwa-telemetry-record
                        session 'hook-slow-status-released
                        :hook-event event
                        :handler-count (length handlers)
                        :restored (and restored t)))))
                 ;; A cancelled event belongs to a request being
                 ;; torn down: nothing to surface, and the caller's
                 ;; continuation must not run.
                 (cond
                  ((eq decision 'cancelled) nil)
                  ((buffer-live-p dispatch-buffer)
                   (with-current-buffer dispatch-buffer
	             (niwa-hooks--surface-final-decision
	              event session decision)
	             (funcall callback decision)))
                  (t
                   (niwa-hooks--surface-final-decision
                    event session decision)
                   (funcall callback decision))))))
          (when (and niwa-hooks-slow-threshold
                     handlers)
            (setq slow-timer
                  (run-at-time
                   niwa-hooks-slow-threshold nil
                   (lambda ()
                     (unless settled
                       (setq slow-echoed t)
                       (niwa-hooks--surface
                        (format "%s hook still running..."
                                (niwa-hooks-event-display-name event)))
                       (when-let* ((token
                                    (niwa-hooks--claim-progress
                                     progress-lease
                                     (format "Running %s hook..."
                                             (niwa-hooks-event-display-name
                                              event))
                                     progress-owner)))
                         (setq slow-surfaced token)
                         (when session
                           (niwa-telemetry-record
                            session 'hook-slow-status-acquired
                            :hook-event event
                            :handler-count (length handlers)
                            :threshold-ms
                            (round (* 1000 niwa-hooks-slow-threshold))))))))))
          (niwa-hooks--run-handlers
           event handlers payload session
           ;; Terminal hooks retain the request's rule layer above, but
           ;; outlive its teardown and are bounded by their own timeout.
           (unless (memq event '(Stop StopFailure)) request)
           nil #'finish
           dispatch-buffer))))))

;;;###autoload
(defun niwa-hooks-run-dry
    (&optional event event-plist session workspace request invocation)
  "Return a dry-run description of hooks that would run for EVENT.

EVENT-PLIST is matched exactly like `niwa-hooks-run-event', but no
native function, Elisp handler, or command handler is executed.

SESSION, WORKSPACE, REQUEST, and INVOCATION supply the context under
inspection.

Interactively, prompt for an event and optional matcher target, then
display the dry-run result."
  (interactive)
  (let* ((interactive-p (called-interactively-p 'interactive))
         (event
          (or (niwa-hooks--event-symbol event)
              (intern
               (completing-read
                "Hook event: "
                (mapcar #'symbol-name niwa-hooks--events)
                nil t))))
         (target-key (niwa-hooks--target-key-for-event event))
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
                      (and (boundp 'niwa--session) niwa--session)))
         (workspace (or workspace
                        (and session (niwa-session-workspace session))))
         (request (or request
                      (and (boundp 'niwa--current-request)
                           niwa--current-request)))
         (invocation (or invocation
                         niwa--agent-invocation))
         (payload (plist-put event-plist :hook-event-name event))
         (rules (niwa-hooks-effective-rules
                 session workspace request invocation))
         (handlers (niwa-hooks--handlers-for-event event payload rules))
         (result
          (list :event event
                :payload payload
                :matcher-target (niwa-hooks--matcher-target event payload)
                :handlers (niwa--diagnostic-value handlers)
                :handler-count (length handlers))))
    (when interactive-p
      (niwa-report-show
       "*niwa hook dry-run*"
       (list :title "Hook dry-run" :identity session
             :sections
             (list (list :id 'match :title "Match"
                         :body (niwa-report-fields
                                (list "Event" event)
                                (list "Matcher target" (plist-get result :matcher-target))
                                (list "Handlers" (plist-get result :handler-count))))
                   (list :id 'record :title "Exact dry-run record"
                         :body (pp-to-string result) :mode 'emacs-lisp-mode)))))
    result))

;;;###autoload
(defun niwa-hooks-list (&optional session)
  "Display effective hooks for SESSION or current buffer."
  (interactive)
  (let* ((session (or session (and (boundp 'niwa--session) niwa--session)))
         (workspace (and session (niwa-session-workspace session)))
         (rules (niwa-hooks-effective-rules session workspace)))
    (niwa-report-show
     "*niwa hooks*"
     (list :title "Effective hooks" :identity session
           :sections (list (list :id 'rules :title "Effective rule records"
                                 :body (pp-to-string rules) :mode 'emacs-lisp-mode))))))

(provide 'niwa-hooks)
;;; niwa-hooks.el ends here
