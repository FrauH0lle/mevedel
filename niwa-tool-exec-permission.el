;;; niwa-tool-exec-permission.el -- Bash/Eval permission adapters -*- lexical-binding: t -*-

;;; Commentary:

;; Owns execution-specific authority normalization, persistence, permission
;; queue adapters, and Bash/Eval permission prompt orchestration.  One pure
;; decision orders operation policy, full escalation, and
;; child capabilities; admission settles what it names through prompts or
;; review and decides again, while queued rechecks read the same decision.

;;; Code:

(eval-when-compile
  (require 'niwa-tool-registry))

(require 'cl-lib)
(require 'niwa-bash-analysis)
(require 'niwa-bash-policy)
(require 'niwa-execution-target)
(require 'niwa-execution-grants)
(require 'niwa-permission-log)
(require 'niwa-permission-persistence)
(require 'niwa-permission-rules)
(require 'niwa-permissions)
(require 'niwa-structs)
(require 'niwa-turn)
(require 'niwa-utilities)
(require 'seq)
(require 'subr-x)

;; `niwa-agents'
(defvar niwa--agent-invocation)

;; `niwa-bash-analysis'
(declare-function niwa-bash-analysis-analyze
                  "niwa-bash-analysis" (source))

;; `niwa-bash-policy'
(declare-function niwa-bash-policy-allow-patterns
                  "niwa-bash-policy" (command &optional permission-context))
(declare-function niwa-bash-policy-buckets
                  "niwa-bash-policy" (&optional permission-context))
(declare-function niwa-bash-policy-check-permission
                  "niwa-bash-policy" (command &rest keys))
(declare-function niwa-bash-policy-command-names
                  "niwa-bash-policy" (analysis))
(declare-function niwa-bash-policy-commands-summary
                  "niwa-bash-policy" (commands))
(declare-function niwa-bash-policy-decision-specifier-value
                  "niwa-bash-policy" (command))
(declare-function niwa-bash-policy-effective-permission-mode
                  "niwa-bash-policy" (&optional permission-context))
(declare-function niwa-bash-policy-effective-sandbox-mode
                  "niwa-bash-policy" (&optional permission-context))
(declare-function niwa-bash-policy-explicit-deny-p
                  "niwa-bash-policy"
                  (buckets command &optional analysis))
(declare-function niwa-bash-policy-missing-resource-paths
                  "niwa-bash-policy"
                  (command permission-context request))
(declare-function niwa-bash-policy-profile-segments
                  "niwa-bash-policy" (command direct-buckets))
(declare-function niwa-bash-policy-reusable-operation-p
                  "niwa-bash-policy" (command))
(declare-function niwa-bash-policy-rule-command
                  "niwa-bash-policy" (command &optional permission-context))

;; `niwa-execution-target'
(declare-function niwa-execution-target-create
                  "niwa-execution-target" (workspace-root))
(declare-function niwa-execution-target-expand-path
                  "niwa-execution-target" (target path &optional directory))
(declare-function niwa-execution-target-remote-p
                  "niwa-execution-target" (target))

;; `niwa-permission-log'
(declare-function niwa-permission-log
                  "niwa-permission-log" (session event &rest props))

;; `niwa-permission-queue'
(declare-function niwa-permission-request "niwa-permission-queue"
                  (entry &optional session))
(autoload 'niwa-permission-request "niwa-permission-queue")

;; `niwa-permission-persistence'
(declare-function niwa-permission-persistence-load-resource-grants
                  "niwa-permission-persistence" (workspace))

;; `niwa-permission-rules'
(declare-function niwa-permission-rules-bucket-decision
                  "niwa-permission-rules"
                  (buckets tool-name path pattern domain name))
(declare-function niwa-permission-rules-execution-level-decision
                  "niwa-permission-rules"
                  (buckets tool-name level pattern))
(declare-function niwa-permission-rules-find
                  "niwa-permission-rules"
                  (rules tool-name &rest keys))
(declare-function niwa-permission-rules-merge-resource-grant
                  "niwa-permission-rules"
                  (grants path access &optional recursive origin))
(declare-function niwa-permission-rules-network-decision
                  "niwa-permission-rules" (buckets tool-name pattern))
(declare-function niwa-permission-rules-qualified-buckets
                  "niwa-permission-rules" (buckets qualifier value))

;; `niwa-permissions'
(declare-function niwa-permission--apply-prompt-result
                  "niwa-permissions"
                  (result tool-name &optional session workspace path
                          &rest keys))
(declare-function niwa-permission--invocation-context
                  "niwa-permissions" (&rest keys))
(declare-function niwa-permission--normalize-outcome
                  "niwa-permissions" (outcome))
(declare-function niwa-permission--one-shot-prompt-entry
                  "niwa-permissions" (entry &optional data-buffer))
(declare-function niwa-permission--one-shot-prompt-outcome
                  "niwa-permissions" (outcome))
(declare-function niwa-permission-decision-raw-outcome
                  "niwa-permissions" (decision))

;; `niwa-queue'
(declare-function niwa-queue--current-session "niwa-queue" ())
(autoload 'niwa-queue--current-session "niwa-queue")

;; `niwa-structs'
(declare-function niwa-session-execution-target
                  "niwa-structs" (cl-x) t)
(declare-function niwa-session-p "niwa-structs" (cl-x))
(declare-function niwa-session-resource-grants "niwa-structs" (cl-x) t)
(declare-function niwa-session-workspace "niwa-structs" (cl-x) t)
(declare-function niwa-session-working-directory "niwa-structs" (cl-x) t)
(declare-function niwa-workspace-root "niwa-structs" (cl-x) t)
(defvar niwa--current-request)
(defvar niwa--session)
(defvar niwa--workspace)

;; `niwa-telemetry'
(declare-function niwa-telemetry-forwarded-audit-p
                  "niwa-telemetry" (session))
(declare-function niwa-telemetry-record-audit
                  "niwa-telemetry" (session event &rest props))

;; `niwa-turn'
(declare-function niwa-current-origin "niwa-turn" ())

;; `niwa-utilities'
(declare-function niwa--warn-once
                  "niwa-utilities" (scope key format &rest args))


;;
;;; Permission queue helpers

(defvar niwa-tool-exec-permission-approved-resources nil
  "Prompt-selected resources bound only while the authorized handler starts.
The pipeline supplies these invocation-only capabilities, never model input.")

(defun niwa-tool-exec-permission-current-context
    (tool-name args &optional session)
  "Return TOOL-NAME's direct execution context for ARGS and SESSION."
  (niwa-permission--invocation-context
   :tool-name tool-name
   :args args
   :session session
   :workspace (and session (niwa-session-workspace session))
   :request niwa--current-request
   :invocation niwa--agent-invocation
   :buffer (current-buffer)))

(defun niwa-tool-exec-permission-default-directory ()
  "Return the working directory for Bash and Eval."
  (niwa-conversation-working-directory))

(defun niwa-tool-exec-permission--capture-permission-origin (input)
  "Return INPUT with its permission owner and session captured."
  (let* ((copy (copy-sequence input))
         (context (copy-sequence (plist-get copy :permission-context))))
    (unless (plist-get context :origin)
      (setq context
            (plist-put context :origin
                       (niwa-current-origin))))
    (unless (plist-member context :session)
      (setq context
            (plist-put context :session
                       (niwa-queue--current-session))))
    (unless (plist-member context :execution-directory)
      (setq context
            (plist-put context :execution-directory
                       (niwa-conversation-working-directory
                        (plist-get context :session)
                        (plist-get context :buffer)))))
    (plist-put copy :permission-context context)))

(defun niwa-tool-exec-permission--permission-origin (permission-context)
  "Return the captured owner from PERMISSION-CONTEXT."
  (or (plist-get permission-context :origin)
      (niwa-current-origin)))

(defun niwa-tool-exec-permission--command-permission-input (input request)
  "Return INPUT carrying REQUEST facts for command authorization."
  (let* ((copy (copy-sequence input))
         (context (copy-sequence (plist-get copy :permission-context))))
    (setq context (plist-put context :sandbox-request request))
    (setq context (plist-put context :eval-mode (plist-get request :eval-mode)))
    (when (plist-get (plist-get request :additional-permissions) :file-system)
      (setq context
            (plist-put context :resource-authority-separated-p t)))
    (plist-put copy :permission-context context)))

(defun niwa-tool-exec-permission--mutation-p (tool-name detail)
  "Return non-nil unless TOOL-NAME runs DETAIL as read-only Bash."
  (or (not (equal tool-name "Bash"))
      (not (eq 'read-only
               (plist-get (niwa-bash-analysis-analyze detail) :class)))))

(defun niwa-tool-exec-permission--operation-decision
    (tool-name detail permission-context)
  "Return operation decision metadata for TOOL-NAME's DETAIL.
PERMISSION-CONTEXT supplies rules and the effective permission mode."
  (if (equal tool-name "Bash")
      (niwa-bash-policy-check-permission
       detail :permission-context permission-context :metadata-p t)
    (niwa-tool-exec-permission--check-eval-permission
     :expression detail :permission-context permission-context)))

(defun niwa-tool-exec-permission--request-permission
    (entry permission-context &optional session)
  "Submit ENTRY through PERMISSION-CONTEXT's request boundary.
Fall back to direct queue admission for callers outside the tool pipeline."
  (when-let* ((tool-name (plist-get permission-context :tool-name)))
    (setq entry (plist-put entry :tool-name tool-name)))
  (setq entry
        (plist-put entry :permission-mode-effective
                   (niwa-bash-policy-effective-permission-mode
                    permission-context)))
  (dolist (pair '((:buffer . :data-buffer) (:request . :request)
                 (:invocation . :invocation) (:args . :args)))
    (when (plist-member permission-context (car pair))
      (setq entry (plist-put entry (cdr pair)
                             (plist-get permission-context (car pair))))))
  (when-let* ((request (plist-get entry :request)))
    (setq entry (plist-put entry :request-id (niwa-request-id request))))
  (when (plist-member permission-context :execution-directory)
    (setq entry
          (plist-put (copy-sequence entry) :execution-directory
                     (plist-get permission-context :execution-directory))))
  (when-let* ((request (plist-get permission-context :sandbox-request))
              (state (plist-get request :authority-state))
              (requested (plist-get state :requested))
              ((memq (plist-get entry :kind) '(bash eval))))
    (setq entry
          (plist-put entry :requested-additional-permissions requested))
    (setq entry
          (plist-put entry :missing-additional-permissions
                     (plist-get state :missing)))
    (setq entry
          (plist-put entry :granted-additional-permissions
                     (plist-get state :granted)))
    (setq entry (plist-put entry :available-resource-grants
                           (plist-get state :available-file-system)))
    (setq entry
          (plist-put entry :resource-selection-cell
                     (plist-get request :resource-selection-cell)))
    (setq entry
          (plist-put entry :resource-originals
                     (copy-tree (plist-get requested :file-system))))
    (setq entry (plist-put entry :show-operation-authority t))
    (setq entry (plist-put entry :operation-pending-p t))
    (setq entry
          (plist-put entry :justification
                     (plist-get request :justification)))
    (let ((callback (plist-get entry :callback))
          (approval-cell (plist-get request :approval-cell))
          (remember-cell (plist-get request :remember-cell)))
      (setq entry
            (plist-put entry :reusable-operation-p
                       (plist-get request :reusable-operation-p)))
      (setq entry
            (plist-put entry :remember-authority-cell remember-cell))
      (setq entry
            (plist-put
             entry :callback
             (lambda (outcome)
               (when (memq outcome
                           '(allow allow-once allow-session always-allow))
                 (setcar approval-cell outcome))
               (funcall
                callback
                (if (memq outcome '(allow-session always-allow))
                    'allow-once
                  outcome)))))))
  (when (plist-get permission-context :one-shot-mutations-p)
    (let ((callback (plist-get entry :callback)))
      (setq entry
            (niwa-permission--one-shot-prompt-entry
             entry (plist-get permission-context :buffer)))
      (setq entry
            (plist-put
             entry :callback
             (lambda (outcome)
               (funcall callback
                        (niwa-permission--one-shot-prompt-outcome
                         outcome)))))))
  (when-let* ((guard (plist-get permission-context :async-guard)))
    (setq entry (plist-put entry :callback
                           (funcall guard (plist-get entry :callback)))))
  (if-let* ((request (plist-get permission-context :permission-request)))
      (funcall request entry session (plist-get entry :callback))
    (if session
        (niwa-permission-request entry session)
      (niwa-permission-request entry))))

(defun niwa-tool-exec-permission--permission-decision-result
    (metadata-p outcome via &rest props)
  "Return OUTCOME, or metadata when METADATA-P is non-nil."
  (if metadata-p
      (append (list :outcome (niwa-permission--normalize-outcome outcome)
                    :raw-outcome outcome
                    :via via
                    :logged t)
              props)
    outcome))

(defun niwa-tool-exec-permission--log-permission-decision
    (tool-name outcome via permission-context &rest props)
  "Persist TOOL-NAME OUTCOME via VIA from PERMISSION-CONTEXT and PROPS."
  (when-let* ((eval-mode (plist-get permission-context :eval-mode)))
    (setq props (plist-put props :eval-mode eval-mode)))
  (when-let* ((session
               (or (plist-get permission-context :session)
                   (niwa-queue--current-session))))
    (let* ((origin (niwa-tool-exec-permission--permission-origin permission-context))
           (mode (niwa-bash-policy-effective-permission-mode
                  permission-context))
           (outcome (niwa-permission--normalize-outcome outcome)))
      (apply #'niwa-permission-log
             session 'permission-decision
             (append (list :tool-name tool-name :origin origin :mode mode
                           :outcome outcome :via via)
                     props))
      (when (niwa-telemetry-forwarded-audit-p session)
        (let ((safe (list :tool-name tool-name :origin origin :mode mode
                          :outcome outcome :via via)))
          (when-let* ((key (plist-get props :specifier-key))
                      ((memq key '(:path :pattern :expression :domain :name))))
            (setq safe (plist-put safe :specifier-key key)))
          (when (plist-member props :protected-path)
            (setq safe
                  (plist-put safe :protected-path
                             (and (plist-get props :protected-path) t))))
          (when-let* ((access (plist-get props :resource-access))
                      ((memq access '(read write))))
            (setq safe (plist-put safe :resource-access access)))
          (dolist (key '(:sandbox-permissions :bucket :command-class :eval-mode))
            (when-let* ((value (plist-get props key))
                        ((symbolp value)))
              (setq safe (plist-put safe key value))))
          (apply #'niwa-telemetry-record-audit
                 session 'permission-decision safe))))))

(define-error 'niwa-execution-policy-error "Invalid execution permission request")

(defun niwa-tool-exec-permission--policy-error (format-string &rest args)
  "Reject an invalid policy request with FORMAT-STRING and ARGS."
  (signal 'niwa-execution-policy-error
          (list (apply #'format format-string args))))

(defun niwa-tool-exec-permission--validate-additional-plist (value allowed label)
  "Return VALUE after validating its keys against ALLOWED for LABEL."
  (unless (and (listp value)
               (proper-list-p value)
               (zerop (% (length value) 2)))
    (niwa-tool-exec-permission--policy-error "%s must be an object" label))
  (let ((tail value))
    (while tail
      (let ((key (pop tail)))
        (pop tail)
        (unless (memq key allowed)
          (niwa-tool-exec-permission--policy-error
           "Unknown %s field: %s" label key)))))
  value)

(defun niwa-tool-exec-permission--filesystem-paths
    (value access &optional target directory)
  "Return exact filesystem permission entries from VALUE at ACCESS.
TARGET and DIRECTORY bind target-native paths to their execution target."
  (let ((paths
         (cond
          ((or (null value) (eq value :json-false)) nil)
          ((vectorp value) (append value nil))
          ((proper-list-p value) value)
          (t (niwa-tool-exec-permission--policy-error
              "Filesystem %s permission must be an array" access)))))
    (mapcar
     (lambda (path)
       (unless (and (stringp path)
                    (not (string-empty-p path))
                    (file-name-absolute-p path))
         (niwa-tool-exec-permission--policy-error
          "Filesystem permission path must be absolute: %S" path))
       ;; Only validation of the authored operand is a policy refusal.
       ;; Expansion below also consumes target and directory state; failures
       ;; there must remain internal errors rather than becoming denials.
       (when target
         (condition-case err
             (niwa-execution-target-native-path target path)
           (niwa-execution-target-error
            (niwa-tool-exec-permission--policy-error
             "%s" (error-message-string err)))))
       ;; Without a target nothing above refuses a quoted name, which would
       ;; escape the spelling-based protection checks.
       (when (file-name-quoted-p path)
         (niwa-tool-exec-permission--policy-error
          "Quoted paths are not allowed: %s" path))
       (list :path (if target
                       (niwa-execution-target-expand-path
                        target path directory)
                     (niwa-expand-file-name path))
             :access access))
     paths)))

(defun niwa-tool-exec-permission--normalize-additional-permissions
    (additional &optional target directory)
  "Return validated ADDITIONAL permissions, or nil when none are requested.
TARGET and DIRECTORY bind target-native filesystem paths."
  (niwa-tool-exec-permission--validate-additional-plist
   additional '(:network :file_system) "Additional permissions")
  (let ((network (plist-get additional :network))
        (file-system (plist-get additional :file_system))
        grants profile)
    (unless (memq network '(nil t :json-false))
      (niwa-tool-exec-permission--policy-error
       "Network permission must be true or false"))
    (when (and file-system (not (eq file-system :json-false)))
      (niwa-tool-exec-permission--validate-additional-plist
       file-system '(:read :write) "Filesystem permissions")
      (let ((reads (niwa-tool-exec-permission--filesystem-paths
                    (plist-get file-system :read) 'read target directory))
            (writes (niwa-tool-exec-permission--filesystem-paths
                     (plist-get file-system :write) 'write target directory)))
        (dolist (grant (append reads writes))
          (let* ((path (plist-get grant :path))
                 (existing
                  (cl-find path grants
                           :key (lambda (item) (plist-get item :path))
                           :test #'string-equal)))
            (if existing
                (when (eq (plist-get grant :access) 'write)
                  (plist-put existing :access 'write))
              (setq grants (append grants (list grant))))))))
    (when (eq network t)
      (setq profile (plist-put profile :network t)))
    (when grants
      (setq profile (plist-put profile :file-system grants)))
    profile))

(defun niwa-tool-exec-permission--sandbox-request
    (args tool &optional eval-mode permission-context)
  "Return the validated child-execution request from ARGS.
TOOL is `bash' or `eval'.  EVAL-MODE selects live, fresh or sandbox Eval.
PERMISSION-CONTEXT supplies the session execution target."
  ;; Refuse before normalization can inspect filesystem or target state.
  (when (and (eq tool 'eval) (eq eval-mode 'sandbox)
             (or (plist-get args :additional_permissions)
                 (member (plist-get args :sandbox_permissions)
                         '("with_additional_permissions" "require_escalated"))))
    (niwa-tool-exec-permission--policy-error
     "Sandbox Eval cannot request additional permissions or escalation; use fresh Eval"))
  (let* ((session (or (plist-get permission-context :session)
                      (and (boundp 'niwa--session) niwa--session)))
         (directory
          (or (plist-get permission-context :execution-directory)
              (niwa-conversation-working-directory
               session (plist-get permission-context :buffer))))
         (target (or (and session
                          (niwa-session-execution-target session))
                     (and (file-remote-p directory)
                          (niwa-execution-target-create directory))))
         (raw-level (plist-get args :sandbox_permissions))
         (level
          (cond
           ((or (null raw-level)
                (eq raw-level :json-false)
                (and (stringp raw-level) (string-empty-p raw-level))
                (equal raw-level "use_default"))
            'use-default)
           ((equal raw-level "with_additional_permissions") 'additive)
           ((equal raw-level "require_escalated") 'escalated)
           (t (niwa-tool-exec-permission--policy-error
               "Unknown sandbox permission level: %s" raw-level))))
         (additional (plist-get args :additional_permissions))
         (normalized-additional
          (and additional
               (not (eq additional :json-false))
               (niwa-tool-exec-permission--normalize-additional-permissions
                additional target directory)))
         (justification (plist-get args :justification)))
    (pcase level
      ('use-default
       (when normalized-additional
         (niwa-tool-exec-permission--policy-error
          "Default sandbox execution cannot include escalation arguments"))
       (list :level 'use-default :additional-permissions nil))
      ('additive
       (unless (and (stringp justification)
                    (not (string-empty-p (string-trim justification))))
         (niwa-tool-exec-permission--policy-error
          "Additional permissions require a justification"))
       (when (and (eq tool 'eval) (not (eq eval-mode 'fresh)))
         (niwa-tool-exec-permission--policy-error
          "Additional permissions are available only to fresh Eval"))
       (list :level 'additive
             :additional-permissions
             (or normalized-additional
                 (niwa-tool-exec-permission--policy-error
                  "Additional permissions must contain a non-empty capability"))
             :justification (string-trim justification)))
      ('escalated
       (unless (and (stringp justification)
                    (not (string-empty-p (string-trim justification))))
         (niwa-tool-exec-permission--policy-error
          "Full sandbox escalation requires a justification"))
       (when normalized-additional
         (niwa-tool-exec-permission--policy-error
          "Full sandbox escalation cannot include additional permissions"))
       (when (and (eq tool 'eval) (not (eq eval-mode 'fresh)))
         (niwa-tool-exec-permission--policy-error
          "Full sandbox escalation is available only to fresh Eval"))
       (list :level 'escalated
             :sandbox-permissions 'require-escalated
             :additional-permissions nil
             :justification (string-trim justification))))))

(defun niwa-tool-exec-permission--permission-allow-p (outcome)
  "Return non-nil when permission OUTCOME authorizes execution."
  (eq 'allow
      (if (and (consp outcome)
               (keywordp (car outcome))
               (plist-member outcome :outcome))
          (plist-get outcome :outcome)
        outcome)))

(defun niwa-tool-exec-permission--additional-denial
    (metadata-p via &optional feedback)
  "Return an additional-authority denial through VIA for METADATA-P."
  (niwa-tool-exec-permission--permission-decision-result
   metadata-p
   (if feedback
       (cons 'deny
             (format "Additional permission denied. Feedback: %s" feedback))
     'deny)
   via))

(defun niwa-tool-exec-permission--filesystem-resource-rule-action
    (tool-name grant permission-context)
  "Return the authoritative `deny' or `ask' rule for TOOL-NAME's GRANT."
  (let ((buckets (niwa-bash-policy-buckets permission-context))
        (path (plist-get grant :path)))
    (let ((action (niwa-permission-rules-bucket-decision
                   buckets tool-name path nil nil nil)))
      (and (memq action '(deny ask)) action))))

(defun niwa-tool-exec-permission--additional-profile (network grants)
  "Return an additive authority profile for NETWORK and GRANTS."
  (let (profile)
    (when network
      (setq profile (plist-put profile :network t)))
    (when grants
      (setq profile (plist-put profile :file-system grants)))
    profile))

(defun niwa-tool-exec-permission--merge-additional-profiles (&rest profiles)
  "Return the normalized union of additive PROFILES."
  (let (network grants)
    (dolist (profile profiles)
      (when (plist-get profile :network)
        (setq network t))
      (dolist (grant (plist-get profile :file-system))
        (when (and (stringp (plist-get grant :path))
                   (file-name-absolute-p (plist-get grant :path))
                   (memq (plist-get grant :access) '(read write))
                   (memq (plist-get grant :recursive) '(t nil)))
          (setq grants
                (niwa-permission-rules-merge-resource-grant
                 grants
                 (plist-get grant :path)
                 (plist-get grant :access)
                 (plist-get grant :recursive)
                 (plist-get grant :workspace-origin))))))
    (niwa-tool-exec-permission--additional-profile network grants)))

(defun niwa-tool-exec-permission--direct-resource-grants (permission-context)
  "Return direct user resource grants from PERMISSION-CONTEXT."
  (if (plist-member permission-context :resource-grants)
      (plist-get permission-context :resource-grants)
    (let* ((session (plist-get permission-context :session))
           (workspace
            (or (plist-get permission-context :workspace)
                (and session (niwa-session-workspace session)))))
      (append
       (and session (niwa-session-resource-grants session))
       (and workspace
            (niwa-permission-persistence-load-resource-grants
             workspace))))))

(defun niwa-tool-exec-permission--remembered-additional-profile
    (tool-name operation permission-context)
  "Return direct remembered authority for TOOL-NAME and OPERATION.
A compound Bash command also receives its segments' direct profiles when
`niwa-bash-policy-profile-segments' allows that."
  (let* ((buckets (niwa-bash-policy-buckets permission-context))
         (direct-buckets
          (seq-filter
           (lambda (entry)
             (memq (car entry) '(:session :persistent :defcustom)))
           buckets))
         (operations
          (if (equal tool-name "Bash")
              (let ((command (niwa-bash-policy-rule-command
                              operation permission-context)))
                (cons command (niwa-bash-policy-profile-segments
                               command direct-buckets)))
            (list operation)))
         (network-decisions
          (mapcar (lambda (item)
                    (niwa-permission-rules-network-decision
                     buckets tool-name item))
                  operations))
         (network
          (and (memq 'allow network-decisions)
               (not (memq 'deny network-decisions))))
         candidates)
    (dolist (item operations)
      (dolist (entry direct-buckets)
        (dolist (rule
                 (niwa-permission-rules-find
                  (cdr entry) tool-name :pattern item))
          (when (eq 'allow (plist-get (cdr rule) :action))
            (setq candidates
                  (append candidates
                          (plist-get (cdr rule) :file-system)))))))
    (niwa-tool-exec-permission--merge-additional-profiles
     (niwa-tool-exec-permission--additional-profile
      network (niwa-permission-origin-usable-grants candidates)))))

(defun niwa-tool-exec-permission-compound-profile-misses
    (command analysis request permission-context)
  "Return bounded diagnostic facts for unattached COMMAND segment profiles.
ANALYSIS describes the authored shell source; REQUEST is its resolved child
authority.  PERMISSION-CONTEXT supplies direct rules and resource policy.
These facts never contribute authority.  Complex syntax is left unexplained."
  (when (and (> (length (plist-get analysis :segments)) 1)
             (not (plist-get analysis :complex-p))
             (not (eq (plist-get request :level) 'escalated)))
    (let* ((buckets (niwa-bash-policy-buckets permission-context))
           (direct (seq-filter
                    (lambda (entry)
                      (memq (car entry) '(:session :persistent :defcustom)))
                    buckets))
           (session (plist-get permission-context :session))
           (target (and session (niwa-session-execution-target session)))
           (grants (plist-get (plist-get request :additional-permissions) :file-system))
           misses)
      (unless (niwa-bash-policy-explicit-deny-p buckets command analysis)
        (dolist (segment (plist-get analysis :segments))
          (when (eq 'allow (car (niwa-permission-rules-first-action-with-bucket
                                direct "Bash" nil segment nil nil)))
            (dolist (entry direct)
              (dolist (rule (niwa-permission-rules-find
                            (cdr entry) "Bash" :pattern segment))
                (when (and (< (length misses) 8)
                           (eq (plist-get (cdr rule) :action) 'allow)
                           (plist-get (cdr rule) :pattern)
                           (not (niwa-permission-rules-find
                                 (list rule) "Bash" :pattern command)))
                  (dolist (grant (niwa-permission-origin-usable-grants
                                 (plist-get (cdr rule) :file-system)))
                    (when (and (< (length misses) 8)
                               (not (niwa-tool-exec-permission--filesystem-resource-rule-action
                                     "Bash" grant permission-context))
                               (not (niwa-execution-grants-covering-grant
                                     (plist-get grant :path) (plist-get grant :access)
                                     grants (plist-get grant :recursive) target)))
                      (push (list :segment (copy-sequence segment)
                                  :pattern (copy-sequence (plist-get (cdr rule) :pattern))
                                  :bucket (car entry)
                                  :path (copy-sequence (plist-get grant :path)))
                            misses)))))))))
      (nreverse misses))))

(defun niwa-tool-exec-permission-effective-sandbox-request
    (args tool-name operation &optional eval-mode permission-context)
  "Resolve ARGS into TOOL-NAME's effective child authority for OPERATION.
EVAL-MODE selects live, fresh or sandbox Eval.  PERMISSION-CONTEXT supplies
remembered direct user authority."
  (let ((request
         (niwa-tool-exec-permission--sandbox-request
          args (if (equal tool-name "Bash") 'bash 'eval) eval-mode
          permission-context)))
    (when eval-mode
      (setq request (plist-put request :eval-mode eval-mode)))
    (if (or (eq eval-mode 'sandbox)
            (eq (plist-get request :level) 'escalated))
        request
      (let* ((remembered
              (and (or (equal tool-name "Bash") (eq eval-mode 'fresh))
                   (niwa-tool-exec-permission--remembered-additional-profile
                    tool-name operation permission-context)))
             (authority
              (append niwa-tool-exec-permission-approved-resources
                      (niwa-tool-exec-permission--direct-resource-grants
                       permission-context)
                      (plist-get remembered :file-system)))
             (target (and (plist-get permission-context :session)
                          (niwa-session-execution-target
                           (plist-get permission-context :session))))
             (profile
              (niwa-tool-exec-permission--merge-additional-profiles
               (plist-get request :additional-permissions)
               (and niwa-tool-exec-permission-approved-resources
                    (list :file-system niwa-tool-exec-permission-approved-resources))
               remembered)))
        ;; Reuse an already approved tree at the requested directory, never an
        ;; unapproved protected exception or an unrepresentable exact mount.
        (when (plist-get profile :file-system)
          (setq profile
                (plist-put profile :file-system
                           (mapcar
                            (lambda (grant)
                              (let* ((path (plist-get grant :path))
                                     (access (plist-get grant :access))
                                     (tree (and (file-directory-p path)
                                                (niwa-execution-grants-covering-grant
                                                 path access authority t target)))
                                     (cover (or tree (niwa-execution-grants-covering-grant
                                                      path access authority
                                                      (plist-get grant :recursive) target))))
                                (if cover
                                    (niwa-permission-origin-derive
                                     cover path access (or (and tree t)
                                                           (plist-get grant :recursive)))
                                  grant)))
                            (plist-get profile :file-system)))))
        ;; Mint launch proof only from original exact authority, never a path
        ;; derived from a covering tree.  A regular file replaced by a socket
        ;; after this point has no proof and cannot enter the socket allow-list.
        (let ((authority
               (seq-remove (lambda (grant) (plist-get grant :recursive)) authority)))
          (when (plist-get profile :file-system)
            (setq profile
                  (plist-put
                   profile :file-system
                   (mapcar
                    (lambda (grant)
                      (if (and (eq (plist-get grant :access) 'write)
                               (not (plist-get grant :recursive))
                               (niwa-execution-grants-socket-p (plist-get grant :path))
                               (niwa-execution-grants-covering-grant
                                (plist-get grant :path) 'write authority nil target))
                          (plist-put grant :socket-authorized t)
                        grant))
                    (plist-get profile :file-system))))))
        (if profile
            (let ((copy (copy-sequence request)))
              (setq copy (plist-put copy :level 'additive))
              (plist-put copy :additional-permissions profile))
          request)))))

(defun niwa-tool-exec-permission--additional-authority-state
    (tool-name request permission-context)
  "Classify TOOL-NAME's additive REQUEST under PERMISSION-CONTEXT."
  (let* ((requested (plist-get request :additional-permissions))
         ;; Only stored direct profiles contribute authority.  The requested
         ;; additions themselves are untrusted until the user approves them.
         (profile
          (and (stringp (plist-get request :operation-pattern))
               (niwa-tool-exec-permission--remembered-additional-profile
                tool-name (plist-get request :operation-pattern)
                permission-context)))
         (grants
          (append (niwa-tool-exec-permission--direct-resource-grants
                   permission-context)
                  (plist-get profile :file-system)))
         (network (eq t (plist-get requested :network)))
         (network-action
          (and network
               (niwa-permission-rules-network-decision
                (niwa-bash-policy-buckets permission-context)
                tool-name
                (plist-get request :operation-pattern))))
         (network-granted
          (and network
               (not (eq network-action 'ask))
               (or (eq network-action 'allow)
                   (eq (niwa-bash-policy-effective-sandbox-mode
                        permission-context)
                       'off)
                   (eq (niwa-bash-policy-effective-permission-mode
                        permission-context)
                       'full-auto))))
         missing-grants
         granted-grants
         (deny-via (and (eq network-action 'deny) 'sandbox-network)))
    (dolist (grant (plist-get requested :file-system))
      (let* ((action
             (niwa-tool-exec-permission--filesystem-resource-rule-action
              tool-name grant permission-context))
             (cover
              (and (not (memq action '(deny ask)))
                   (niwa-execution-grants-covering-grant
                    (plist-get grant :path) (plist-get grant :access) grants
                    (plist-get grant :recursive)
                    (and (plist-get permission-context :session)
                         (niwa-session-execution-target
                          (plist-get permission-context :session)))))))
        (cond
         ((eq action 'deny)
          (setq deny-via 'sandbox-filesystem))
         (cover
          (push (niwa-permission-origin-derive
                 cover (plist-get grant :path) (plist-get grant :access)
                 (plist-get grant :recursive))
                granted-grants))
         (t (push grant missing-grants)))))
    (list
     :requested requested
     :available-file-system grants
     :missing
     (niwa-tool-exec-permission--additional-profile
      (and network (not network-granted))
      (nreverse missing-grants))
     :granted
     (niwa-tool-exec-permission--additional-profile
      network-granted
      (nreverse granted-grants))
     :deny-via deny-via)))

(defun niwa-tool-exec-permission--prepare-additional-authority-request
    (tool-name request permission-context &optional operation-pattern)
  "Attach TOOL-NAME's additive authority state to REQUEST.
OPERATION-PATTERN is the exact Bash command or Eval expression."
  (if (not (eq (plist-get request :level) 'additive))
      request
    (let ((copy (copy-sequence request)))
      (setq copy
            (plist-put copy :operation-pattern operation-pattern))
      (let ((reusable
             (and (stringp operation-pattern)
                  (if (equal tool-name "Bash")
                      (niwa-bash-policy-reusable-operation-p
                       operation-pattern)
                    t))))
        (setq copy (plist-put copy :reusable-operation-p reusable))
        (setq copy
              (plist-put
               copy :remember-patterns
               (and reusable
                    (if (equal tool-name "Bash")
                        (niwa-bash-policy-allow-patterns
                         operation-pattern permission-context)
                      (list operation-pattern)))))
        (setq copy
              (plist-put copy :remember-cell
                         (list (and reusable '(:operation t))))))
      (let* ((state
              (niwa-tool-exec-permission--additional-authority-state
               tool-name copy permission-context))
             (requested (plist-get state :requested))
             (selection
              (append
               (and (plist-get copy :reusable-operation-p)
                    '(:operation t))
               (and (plist-get copy :reusable-operation-p)
                    (plist-get requested :network)
                    '(:network t))
               (and (plist-get copy :reusable-operation-p)
                    (plist-get requested :file-system)
                    (list :file-system
                          (copy-tree
                           (plist-get requested :file-system)))))))
        (setq copy (plist-put copy :authority-state state))
        (setq copy
              (plist-put copy :resource-selection-cell
                         (and (plist-get requested :file-system)
                              (list (copy-tree
                                     (plist-get requested :file-system))))))
        (setcar (plist-get copy :remember-cell) selection))
      (plist-put copy :approval-cell (list nil)))))

(defun niwa-tool-exec-permission--apply-remembered-authority
    (outcome tool-name request session workspace)
  "Store OUTCOME authority selected in REQUEST for TOOL-NAME."
  (when (memq outcome '(allow-session always-allow))
    (let* ((selection
            (car (plist-get request :remember-cell)))
           (spec-key (if (equal tool-name "Eval") :expression :pattern))
           (patterns (plist-get request :remember-patterns))
           (profile
            (niwa-tool-exec-permission--additional-profile
             (plist-get selection :network)
             (plist-get selection :file-system)))
           (profile-patterns
            (and (plist-get selection :operation)
                 profile
                 (if (and (equal tool-name "Bash")
                          (cdr (plist-get
                                (niwa-bash-analysis-analyze
                                 (plist-get request :operation-pattern))
                                :segments)))
                     (list (plist-get request :operation-pattern))
                   patterns))))
      (dolist (pattern patterns)
        (when (and (plist-get selection :operation)
                   (not (member pattern profile-patterns)))
          (niwa-permission--apply-prompt-result
           outcome tool-name session workspace nil
           :spec-key spec-key :spec-value pattern)))
      (dolist (pattern profile-patterns)
        (niwa-permission--apply-prompt-result
         outcome tool-name session workspace nil
         :spec-key spec-key :spec-value pattern
         :network (plist-get profile :network)
         :file-system (plist-get profile :file-system)))
      (dolist (grant (plist-get selection :resource-grants))
        (let ((path (plist-get grant :path))
              (access (plist-get grant :access)))
          (niwa-permission--apply-prompt-result
           outcome tool-name session workspace path
           :spec-key :path :spec-value path
           :resource-access access
           :resource-recursive (plist-get grant :recursive)
           :resource-origin (plist-get grant :workspace-origin)))))))

(defun niwa-tool-exec-permission--log-additional-authority
    (tool-name state permission-context metadata-p)
  "Log resolved additive STATE for TOOL-NAME under PERMISSION-CONTEXT."
  (when metadata-p
    (when (plist-get (plist-get state :requested) :network)
      (niwa-tool-exec-permission--log-permission-decision
       tool-name 'allow 'sandbox-network permission-context
       :sandbox-permissions 'additive
       :additional-permissions '(:network t)))
    (dolist (grant
             (plist-get (plist-get state :requested) :file-system))
      (niwa-tool-exec-permission--log-permission-decision
       tool-name 'allow 'sandbox-filesystem permission-context
       :sandbox-permissions 'additive
       :specifier-key :path
       :specifier-value (plist-get grant :path)
       :resource-access (plist-get grant :access)))))

(defun niwa-tool-exec-permission--capabilities-prompt-async
    (tool-name detail input request state cont)
  "Prompt for the capabilities REQUEST still needs for TOOL-NAME's DETAIL.
INPUT supplies permission context and metadata mode; STATE is the additive
authority state.  Call CONT once: with the approving prompt outcome so the
caller stores and logs that authority, or with the denial result."
  (let* ((permission-context (plist-get input :permission-context))
         (metadata-p (plist-get input :permission-decision-metadata))
         (missing (plist-get state :missing))
         (missing-grants (plist-get missing :file-system))
         (first-grant (car missing-grants))
         (session (plist-get permission-context :session))
         (workspace (or (plist-get permission-context :workspace)
                        (and session
                             (niwa-session-workspace session))))
         (via (if (plist-get missing :network)
                  'sandbox-network
                'sandbox-filesystem)))
    (niwa-tool-exec-permission--request-permission
     (list
      :kind 'sandbox
      :permission-via via
      :tool-name tool-name
      :detail detail
      :mutation-p (niwa-tool-exec-permission--mutation-p tool-name detail)
      :sandbox-permissions 'additive
      :additional-permissions (plist-get state :requested)
      :requested-additional-permissions (plist-get state :requested)
      :missing-additional-permissions missing
      :granted-additional-permissions (plist-get state :granted)
      :available-resource-grants (plist-get state :available-file-system)
      :resource-selection-cell (plist-get request :resource-selection-cell)
      :resource-originals
      (copy-tree (plist-get (plist-get state :requested) :file-system))
      :show-operation-authority t
      :operation-pending-p nil
      :reusable-operation-p
      (plist-get request :reusable-operation-p)
      :remember-authority-cell
      (and (or (plist-get request :reusable-operation-p)
               missing-grants)
           (plist-get request :remember-cell))
      :justification (plist-get request :justification)
      :specifier-key (and first-grant :path)
      :specifier-value (and first-grant (plist-get first-grant :path))
      :resource-path (and first-grant (plist-get first-grant :path))
      :resource-access (and first-grant (plist-get first-grant :access))
      :include-always
      (and (or (plist-get request :reusable-operation-p)
               missing-grants)
           (not (null workspace)))
      :workspace workspace
      :origin (niwa-tool-exec-permission--permission-origin permission-context)
      :callback
      (lambda (outcome)
        (pcase outcome
          ((or 'allow 'allow-once 'allow-session 'always-allow)
           (funcall cont outcome))
          ('deny-session
           (dolist (grant missing-grants)
             (let ((path (plist-get grant :path))
                   (access (plist-get grant :access)))
               (niwa-permission--apply-prompt-result
                outcome tool-name session workspace path
                :spec-key :path :spec-value path
                :resource-access access)))
           (funcall cont
                    (niwa-tool-exec-permission--additional-denial
                     metadata-p via)))
          (`(deny . ,reason)
           (funcall
            cont
            (niwa-tool-exec-permission--permission-decision-result
             metadata-p (cons 'deny reason) via)))
          (`(feedback . ,text)
           (funcall cont
                    (niwa-tool-exec-permission--additional-denial
                     metadata-p via text)))
          ('aborted
           (funcall
            cont
            (niwa-tool-exec-permission--permission-decision-result
             metadata-p 'aborted via)))
          (_
           (funcall cont
                    (niwa-tool-exec-permission--additional-denial
                     metadata-p via))))))
     permission-context session)))

(defun niwa-tool-exec-permission--full-escalation-explicit-deny-p
    (tool-name detail buckets)
  "Return non-nil when ordinary rules deny TOOL-NAME and DETAIL."
  (if (equal tool-name "Bash")
      (niwa-bash-policy-explicit-deny-p buckets detail)
    (eq 'deny
        (niwa-permission-rules-bucket-decision
         buckets tool-name nil detail nil nil))))

(defun niwa-tool-exec-permission-full-escalation-rule-decision
    (tool-name detail buckets level)
  "Return the full-escalation rule decision for TOOL-NAME and DETAIL.
BUCKETS supplies ordinary and execution-level rules for LEVEL."
  (if (or (niwa-tool-exec-permission--full-escalation-explicit-deny-p
           tool-name detail buckets)
          (and (equal tool-name "Bash")
               (niwa-bash-policy-explicit-deny-p
                (niwa-permission-rules-qualified-buckets
                 buckets :sandbox-permissions level)
                detail)))
      'deny
    (niwa-permission-rules-execution-level-decision
     buckets tool-name level detail)))

(defun niwa-tool-exec-permission--full-escalation-reusable-rule-p
    (tool-name detail)
  "Return non-nil when a prompt may offer reusable authority for DETAIL."
  (and (stringp detail)
       (not (string-empty-p (string-trim detail)))
       (if (equal tool-name "Bash")
           (niwa-bash-policy-reusable-operation-p detail)
         (equal tool-name "Eval"))))

(defun niwa-tool-exec-permission--full-escalation-denial
    (metadata-p &optional feedback)
  "Return a full-escalation denial for METADATA-P and FEEDBACK."
  (niwa-tool-exec-permission--permission-decision-result
   metadata-p
   (if feedback
       (cons 'deny
             (format "Full execution escalation denied. Feedback: %s"
                     feedback))
     'deny)
   'sandbox-full-escalation
   :sandbox-permissions 'require-escalated))

(defun niwa-tool-exec-permission--apply-full-escalation-prompt-result
    (outcome tool-name detail level session workspace metadata-p)
  "Apply full-escalation prompt OUTCOME and return its permission result."
  (pcase outcome
    ((or 'allow 'allow-once)
     (niwa-tool-exec-permission--permission-decision-result
      metadata-p 'allow 'sandbox-full-escalation
      :sandbox-permissions level))
    ((or 'allow-session 'always-allow)
     (niwa-permission--apply-prompt-result
      outcome tool-name session workspace nil
      :spec-key (if (equal tool-name "Eval") :expression :pattern)
      :spec-value detail
      :sandbox-permissions level)
     (niwa-tool-exec-permission--permission-decision-result
      metadata-p 'allow 'sandbox-full-escalation
      :sandbox-permissions level))
    ('deny-session
     (niwa-permission--apply-prompt-result
      outcome tool-name session workspace nil
      :spec-key (if (equal tool-name "Eval") :expression :pattern)
      :spec-value detail
      :sandbox-permissions level)
     (niwa-tool-exec-permission--full-escalation-denial metadata-p))
    (`(feedback . ,text)
     (niwa-tool-exec-permission--full-escalation-denial metadata-p text))
    (`(deny . ,reason)
     (niwa-tool-exec-permission--permission-decision-result
      metadata-p (cons 'deny reason) 'sandbox-full-escalation
      :sandbox-permissions level))
    ('aborted
     (niwa-tool-exec-permission--permission-decision-result
      metadata-p 'aborted 'sandbox-full-escalation
      :sandbox-permissions level))
    (_ (niwa-tool-exec-permission--full-escalation-denial metadata-p))))

(defun niwa-tool-exec-permission--escalation-prompt-async
    (tool-name detail input request via cont)
  "Prompt to run TOOL-NAME's DETAIL without confinement for REQUEST.
VIA is the cause the card shows: an explicit rule ask or the escalation
itself.  INPUT supplies permission context and metadata mode.  Call CONT
once with the applied prompt result."
  (let* ((permission-context (plist-get input :permission-context))
         (metadata-p (plist-get input :permission-decision-metadata))
         (level (plist-get request :sandbox-permissions))
         (session (plist-get permission-context :session))
         (workspace (or (plist-get permission-context :workspace)
                        (and session (niwa-session-workspace session)))))
    (when metadata-p
      (niwa-tool-exec-permission--log-permission-decision
       tool-name 'ask via permission-context
       :sandbox-permissions level
       :specifier-key (if (equal tool-name "Eval") :expression :pattern)
       :specifier-value detail))
    (niwa-tool-exec-permission--request-permission
     (list
      :kind 'sandbox
      :permission-via via
      :tool-name tool-name
      :detail detail
      :mutation-p (niwa-tool-exec-permission--mutation-p tool-name detail)
      :sandbox-permissions level
      :justification (plist-get request :justification)
      :specifier-key (if (equal tool-name "Eval") :expression :pattern)
      :specifier-value detail
      :include-always
      (niwa-tool-exec-permission--full-escalation-reusable-rule-p
       tool-name detail)
      :workspace workspace
      :origin (niwa-tool-exec-permission--permission-origin permission-context)
      :callback
      (lambda (outcome)
        (funcall
         cont
         (niwa-tool-exec-permission--apply-full-escalation-prompt-result
          outcome tool-name detail level session workspace metadata-p))))
     permission-context session)))

;;
;;; Eval permission adapter

(defun niwa-tool-exec-permission-eval-mode (args)
  "Return the Eval mode from ARGS, validating mode-specific arguments."
  (let ((mode (plist-get args :mode)))
    (unless (memq (plist-get args :reset) '(nil t :json-false))
      (niwa-tool-exec-permission--policy-error "Reset must be true or false"))
    (when (and (eq (plist-get args :reset) t) (not (equal mode "sandbox")))
      (niwa-tool-exec-permission--policy-error
       "Reset is available only to sandbox Eval"))
    (cond
     ((or (null mode)
          (eq mode :json-false)
          (and (stringp mode) (string-empty-p mode))
          (equal mode "live"))
      'live)
     ((equal mode "fresh") 'fresh)
     ((equal mode "sandbox") 'sandbox)
     (t (niwa-tool-exec-permission--policy-error "Unknown Eval mode: %s" mode)))))

(defun niwa-tool-exec-permission-eval-preserve-ui-p (args)
  "Return non-nil when ARGS request restoring window state."
  (not (eq (plist-get args :preserve_ui) :json-false)))

(cl-defun niwa-tool-exec-permission--check-eval-permission
    (&key expression permission-context)
  "Return Eval decision metadata for EXPRESSION under PERMISSION-CONTEXT.

Live Eval asks unless a rule settles it or the mode is `full-auto'.
Fresh Eval runs automatically in Edits under required confinement.
Sandbox Eval runs automatically in Ask and Edits, always confined.
Deny rules remain final; Full Access bypasses ask rules.
The metadata
identifies rule or mode authority."
  (let* ((buckets (niwa-bash-policy-buckets permission-context))
         (mode (niwa-bash-policy-effective-permission-mode
                permission-context))
         (action (niwa-permission-rules-bucket-decision
                  buckets "Eval" nil expression nil nil)))
    (cond
     ((eq action 'deny) (niwa-permission--decision 'deny 'rule))
     ((eq mode 'full-auto) (niwa-permission--decision 'allow 'mode))
     (action (niwa-permission--decision action 'rule))
     ((eq (plist-get permission-context :eval-mode) 'sandbox)
      (niwa-permission--decision 'allow 'mode))
     ((and (eq mode 'edits)
           (eq (plist-get permission-context :eval-mode) 'fresh))
      (niwa-permission--decision 'allow 'mode))
     (t (niwa-permission--decision 'ask 'mode)))))

(defun niwa-tool-exec-permission--normalize-prompt-result
    (outcome apply-stored feedback-prefix)
  "Normalize prompt OUTCOME using APPLY-STORED and FEEDBACK-PREFIX."
  (pcase outcome
    ((or 'allow 'allow-once) 'allow)
    ((or 'allow-session 'always-allow 'deny-session)
     (funcall apply-stored outcome))
    ((or 'deny 'deny-once) 'deny)
    (`(deny . ,reason) (cons 'deny reason))
    (`(feedback . ,text)
     (cons 'deny (format "%s%s" feedback-prefix text)))
    ('aborted 'aborted)
    (_ 'deny)))

(defun niwa-tool-exec-permission--eval-prompt-result
    (outcome session workspace expression metadata-p)
  "Apply Eval prompt OUTCOME and return its canonical permission result."
  (let ((result
         (niwa-tool-exec-permission--normalize-prompt-result
          outcome
          (lambda (stored-outcome)
            (niwa-permission--apply-prompt-result
             stored-outcome "Eval" session workspace nil
             :spec-key :expression
             :spec-value expression))
          "Eval cancelled by user. Feedback: ")))
    (niwa-tool-exec-permission--permission-decision-result
     metadata-p result 'eval-policy)))

(defun niwa-tool-exec-permission--eval-operation-prompt-async
    (expression input mode via cont)
  "Queue the Eval card for EXPRESSION in MODE and call CONT with its result.
VIA is the operation decision's cause.  INPUT supplies permission context,
metadata mode, and the window-restoration flag.  The queue's head renderer
dispatches to the Eval UI; CONT receives `allow', `deny', `(deny . REASON)',
or `aborted', with feedback shaped as \"Eval cancelled by user.
Feedback: TEXT\"."
  (let* ((permission-context (plist-get input :permission-context))
         (metadata-p (plist-get input :permission-decision-metadata))
         (session (plist-get permission-context :session))
         (workspace (or (plist-get permission-context :workspace)
                        (and session (niwa-session-workspace session)))))
    (when metadata-p
      (niwa-tool-exec-permission--log-permission-decision
       "Eval" 'ask 'eval-policy permission-context))
    (niwa-tool-exec-permission--request-permission
     (list :kind 'eval
           :permission-via via
           :expression expression
           :mode (symbol-name mode)
           :eval-mode mode
           :reset (plist-get input :reset)
           :preserve-ui (niwa-tool-exec-permission-eval-preserve-ui-p input)
           :reusable-operation-p t
           :remember-authority-cell (list '(:operation t))
           :include-always (not (null workspace))
           :workspace workspace
           :specifier-key :expression
           :specifier-value expression
           :origin
           (niwa-tool-exec-permission--permission-origin permission-context)
           :callback
           (lambda (outcome)
             (funcall
              cont
              (niwa-tool-exec-permission--eval-prompt-result
               outcome session workspace expression metadata-p))))
     permission-context session)))

;;

;;; Bash Prompt UI

(defun niwa-tool-exec-permission--apply-bash-prompt-result
    (outcome session workspace command allow-patterns)
  "Apply Bash prompt OUTCOME for SESSION, WORKSPACE, and COMMAND.

Session/permanent allow outcomes store ALLOW-PATTERNS as Bash
`:pattern' rules instead of saving COMMAND verbatim.  Deny-session
stays exact to avoid broad negative rules from a single rejection."
  (pcase outcome
    ('allow-once 'allow)
    ((or 'allow-session 'always-allow)
     (dolist (pattern (or allow-patterns (list command)))
       (niwa-permission--apply-prompt-result
        outcome "Bash" session workspace nil
        :spec-key :pattern
        :spec-value pattern))
     'allow)
    ('deny-once 'deny)
    ('deny-session
     (niwa-permission--apply-prompt-result
      outcome "Bash" session workspace nil
      :spec-key :pattern
      :spec-value command)
     'deny)
    (_ 'deny)))

(defun niwa-tool-exec-permission--bash-prompt-result
    (outcome session workspace command allow-patterns metadata-p)
  "Apply Bash prompt OUTCOME and return its canonical permission result."
  (let ((specifier
         (niwa-bash-policy-decision-specifier-value command))
        (result
         (niwa-tool-exec-permission--normalize-prompt-result
          outcome
          (lambda (stored-outcome)
            (niwa-tool-exec-permission--apply-bash-prompt-result
             stored-outcome session workspace command allow-patterns))
          "Command cancelled by user. Feedback: ")))
    (niwa-tool-exec-permission--permission-decision-result
     metadata-p result 'bash-classifier
     :specifier-key :pattern
     :specifier-value specifier)))

(defun niwa-tool-exec-permission--bash-operation-prompt-async
    (command input via cont)
  "Request complete Bash COMMAND authority from INPUT, then call CONT.
VIA identifies why approval is required.  Shared permission admission owns
optional automatic review before any human card is displayed."
  (let* ((context (plist-get input :permission-context))
         (metadata-p (plist-get input :permission-decision-metadata))
         (session (or (plist-get context :session) (niwa-queue--current-session)))
         (workspace (or (plist-get context :workspace)
                        (and session (niwa-session-workspace session))))
         (analysis (niwa-bash-analysis-analyze command))
         (class (plist-get analysis :class))
         (commands (niwa-bash-policy-command-names analysis))
         (patterns (niwa-bash-policy-allow-patterns command context))
         (reusable (niwa-bash-policy-reusable-operation-p command)))
    (niwa-tool-exec-permission--request-permission
     (list :kind 'bash :permission-via via :command command
           :mutation-p (not (eq class 'read-only))
           :specifier-key :pattern :specifier-value command
           :analysis analysis :command-class class :commands commands
           :commands-summary (niwa-bash-policy-commands-summary commands)
           :unparseable (eq class 'complex) :allow-patterns patterns
           :reusable-operation-p reusable :workspace workspace
           :include-always (and reusable (not (null workspace)))
           :origin (niwa-tool-exec-permission--permission-origin context)
           :callback (lambda (outcome)
                       (funcall cont
                                (niwa-tool-exec-permission--bash-prompt-result
                                 outcome session workspace command patterns metadata-p))))
     context session)))

;;

;;; Execution authorization

(defun niwa-tool-exec-permission--escalation-request-p (request context)
  "Return non-nil when REQUEST runs without the confinement CONTEXT applies."
  (and (eq (plist-get request :sandbox-permissions) 'require-escalated)
       (not (eq (niwa-bash-policy-effective-sandbox-mode context) 'off))))

(defun niwa-tool-exec-permission--decide
    (tool-name detail request context &optional settled)
  "Return the policy decision for TOOL-NAME running DETAIL under REQUEST.

CONTEXT is the permission invocation context.  REQUEST carries the validated
child-authority request: `:sandbox-permissions', `:additional-permissions',
`:operation-pattern', and the operation card's `:approval-cell'.  SETTLED
lists facts this invocation already established: `:operation' once the user
or delegated reviewer approved the operation.

Return decision metadata.  An `ask' names what still stands in the way in
`:needs': `operation', `escalation', `capabilities' with the
additive `:state', or `resources' with the command `:paths' lacking
authority.  Every result carries the `:operation' decision it rests on.
Admission and queued rechecks share this one ordering."
  (let* ((bash-p (equal tool-name "Bash"))
         (requested (plist-get request :additional-permissions))
         (escalation-p
          (niwa-tool-exec-permission--escalation-request-p request context))
         (context (plist-put (copy-sequence context)
                             :resource-authority-separated-p
                             (and (or escalation-p
                                      (plist-get requested :file-system))
                                  t)))
         (context (plist-put context :eval-mode (plist-get request :eval-mode)))
         (full-auto-p
          (eq (niwa-bash-policy-effective-permission-mode context) 'full-auto))
         (escalation-rule
          (and (eq (plist-get request :sandbox-permissions) 'require-escalated)
               (niwa-tool-exec-permission-full-escalation-rule-decision
                tool-name detail (niwa-bash-policy-buckets context)
                'require-escalated)))
         (operation (niwa-tool-exec-permission--operation-decision
                     tool-name detail context))
         (outcome (plist-get operation :outcome))
         (explicit-ask-p (and (eq outcome 'ask)
                              (eq (plist-get operation :via) 'rule))))
    (cl-flet ((decide (outcome via &rest props)
                (apply #'niwa-permission--decision outcome via
                       :operation operation props)))
      (cond
       ((eq outcome 'deny) (decide 'deny (plist-get operation :via)))
       ((eq escalation-rule 'deny) (decide 'deny 'sandbox-full-escalation))
       (full-auto-p
        (let ((state
               (and requested
                    (niwa-tool-exec-permission--additional-authority-state
                     tool-name request context))))
          (if (plist-get state :deny-via)
              (decide 'deny (plist-get state :deny-via))
            (decide 'allow 'mode :state state))))
       (escalation-p
        (cond
         (explicit-ask-p (decide 'ask 'rule :needs 'escalation))
         ((and (eq escalation-rule 'allow)
               (not (plist-get context :one-shot-mutations-p)))
          (decide 'allow 'sandbox-full-escalation))
         (t (decide 'ask 'sandbox-full-escalation :needs 'escalation))))
       ((and (eq outcome 'ask) (not (plist-get settled :operation)))
        (decide 'ask (plist-get operation :via) :needs 'operation))
       (t
        (let* ((state
                (and requested
                     (niwa-tool-exec-permission--additional-authority-state
                      tool-name request context)))
               (missing (plist-get state :missing))
               (paths (and bash-p
                           (niwa-bash-policy-missing-resource-paths
                            detail context request))))
          (cond
           ((plist-get state :deny-via)
            (decide 'deny (plist-get state :deny-via)))
           (paths (decide 'ask 'workspace-boundary :needs 'resources :paths paths))
           ((and missing (not (car (plist-get request :approval-cell))))
            (decide 'ask (if (plist-get missing :network)
                             'sandbox-network
                           'sandbox-filesystem)
                    :needs 'capabilities :state state))
           (t (decide 'allow (plist-get operation :via) :state state)))))))))

(defun niwa-tool-exec-permission-reevaluate (entry context)
  "Recheck queued execution ENTRY against fresh permission CONTEXT.
Return `allow', `deny', or `ask' without prompting or executing.  Already
granted capabilities are checked again; admission's missing subset does not
describe the complete authority needed at settlement."
  (let ((tool-name (niwa-permission-policy-tool-name
                    (plist-get context :tool-name)))
        (detail (or (plist-get entry :command)
                    (plist-get entry :expression)
                    (plist-get entry :detail)
                    (plist-get context :pattern)))
        (early (plist-get context :early-decision)))
    (cond
     (early (niwa-permission-decision-raw-outcome early))
     ((not (stringp detail)) 'ask)
     (t
      (plist-get
       (niwa-tool-exec-permission--decide
        tool-name detail
        (plist-put
         (if (eq (plist-get entry :kind) 'generic)
             (niwa-tool-exec-permission-effective-sandbox-request
              (plist-get entry :args) tool-name detail
              (and (equal tool-name "Eval")
                   (niwa-tool-exec-permission-eval-mode (plist-get entry :args)))
              context)
           (list :sandbox-permissions (plist-get entry :sandbox-permissions)
                 :eval-mode (and (equal tool-name "Eval")
                                 (niwa-tool-exec-permission-eval-mode
                                  (or (plist-get entry :args)
                                      (list :mode (plist-get entry :mode)))))
                 :additional-permissions
                 (if-let* ((selected (car (plist-get entry :resource-selection-cell))))
                     (plist-put (copy-sequence
                                 (plist-get entry :requested-additional-permissions))
                                :file-system selected)
                   (plist-get entry :requested-additional-permissions))))
         :operation-pattern detail)
        context)
       :outcome)))))

(defun niwa-tool-exec-permission--authorize-async
    (tool-name detail input request cont guard)
  "Authorize TOOL-NAME's DETAIL for REQUEST and call CONT once.
INPUT supplies permission context, delegated trust, and metadata mode.
GUARD protects callbacks that resume authorization after an approval.
`niwa-tool-exec-permission--decide' names what still stands in the way;
this loop settles one need at a time through the shared approval boundary,
then decides again until the invocation is allowed or denied."
  (let* ((input (plist-put (copy-sequence input) :permission-context
                           (plist-put (copy-sequence
                                       (plist-get input :permission-context))
                                      :async-guard guard)))
         (input (niwa-tool-exec-permission--command-permission-input
                 input request))
         (permission-context (plist-get input :permission-context))
         (metadata-p (plist-get input :permission-decision-metadata))
         (trust-literal-p (plist-get input :trust-literal-p))
         (bash-p (equal tool-name "Bash"))
         (session (plist-get permission-context :session))
         (workspace (or (plist-get permission-context :workspace)
                        (and session (niwa-session-workspace session))))
         (level (plist-get request :sandbox-permissions))
         (escalation-p (niwa-tool-exec-permission--escalation-request-p
                        request permission-context))
         (operation-via (if bash-p 'bash-classifier 'eval-policy))
         (specifier-props
          (if bash-p
              (list :specifier-key :pattern
                    :specifier-value
                    (niwa-bash-policy-decision-specifier-value detail))
            (list :eval-mode (plist-get request :eval-mode))))
         (escalation-props
          (list :sandbox-permissions level
                :specifier-key (if bash-p :pattern :expression)
                :specifier-value detail))
         settled operation-logged operation-result)
    (cl-labels
     ((record (outcome via &rest props)
        (when metadata-p
          (apply #'niwa-tool-exec-permission--log-permission-decision
                 tool-name outcome via permission-context props)))
      (result (outcome via &rest props)
        (apply #'niwa-tool-exec-permission--permission-decision-result
               metadata-p outcome via props))
      (finish-allow (state &optional approval)
        (when (plist-get request :additional-permissions)
          (niwa-tool-exec-permission--apply-remembered-authority
           (or approval (car (plist-get request :approval-cell)))
           tool-name request session workspace)
          (niwa-tool-exec-permission--log-additional-authority
           tool-name state permission-context metadata-p))
        (funcall cont (or operation-result
                          (apply #'result 'allow operation-via
                                 specifier-props))))
      (after-operation (outcome)
        (if (niwa-tool-exec-permission--permission-allow-p outcome)
            (progn
              (setq settled (plist-put settled :operation t)
                    operation-result outcome)
              (step))
          (funcall cont outcome)))
      (step ()
        (when session
          (setq permission-context
                (plist-put permission-context :mode
                           (niwa-permission-mode-for-context permission-context))))
        (let* ((decision (niwa-tool-exec-permission--decide
                          tool-name detail request permission-context
                          settled))
               (outcome (plist-get decision :outcome))
               (via (plist-get decision :via))
               (needs (plist-get decision :needs))
               (operation (plist-get decision :operation)))
          ;; Record the classifier's own allow once, before anything the
          ;; operation still depends on is resolved.
          (when (and (not escalation-p)
                     (not operation-logged)
                     (not (plist-get settled :operation))
                     (eq (plist-get operation :outcome) 'allow))
            (setq operation-logged t)
            (apply #'record 'allow operation-via specifier-props))
          (cond
           ((eq outcome 'deny)
            (pcase via
              ((or 'sandbox-network 'sandbox-filesystem)
               (funcall cont (niwa-tool-exec-permission--additional-denial
                              metadata-p via)))
              ('sandbox-full-escalation
               (apply #'record 'deny via escalation-props)
               (funcall cont (niwa-tool-exec-permission--full-escalation-denial
                              metadata-p)))
              (_
               (let ((props (if escalation-p escalation-props specifier-props))
                     (via (if escalation-p via operation-via)))
                 (apply #'record 'deny via props)
                 (funcall cont (apply #'result 'deny via props))))))
           ((eq needs 'operation)
            (cond
             (trust-literal-p
              (let ((denial
                     (cons 'deny
                           (if bash-p
                               "Shell expansion requires a pre-approved Bash rule; no prompt is shown while preparing skill bodies."
                             "Elisp expansion requires a pre-approved Eval rule; no prompt is shown while preparing skill bodies."))))
                (apply #'record denial operation-via specifier-props)
                (funcall cont (apply #'result denial operation-via
                                     specifier-props))))
             (bash-p
              (niwa-tool-exec-permission--bash-operation-prompt-async
               detail input via (funcall guard #'after-operation)))
             (t
              (niwa-tool-exec-permission--eval-operation-prompt-async
               detail input (niwa-tool-exec-permission-eval-mode input)
               via (funcall guard #'after-operation)))))
           ((and trust-literal-p escalation-p)
            (funcall cont
                     (result (cons 'deny "Delegated expansion cannot request full execution escalation")
                             'sandbox-policy :sandbox-permissions level)))
           ((and trust-literal-p (eq (plist-get request :level) 'additive))
            (funcall cont
                     (result (cons 'deny "Delegated expansion cannot request additional sandbox authority")
                             'sandbox-policy)))
           ((eq outcome 'allow)
            (if escalation-p
                (progn
                  (apply #'record 'allow via escalation-props)
                  (funcall cont (apply #'result 'allow via escalation-props)))
              (finish-allow (plist-get decision :state))))
           ((eq needs 'resources)
            (funcall
             cont
             (result
              (cons 'deny
                    (format
                     (concat
                      "Filesystem authority required for Bash resource: %s. "
                      "Retry with sandbox_permissions=\"with_additional_permissions\" "
                      "and additional_permissions.file_system.read containing "
                      "the exact absolute path.")
                     (mapconcat #'identity (plist-get decision :paths) ", ")))
              'workspace-boundary)))
           ((eq needs 'capabilities)
            (niwa-tool-exec-permission--capabilities-prompt-async
             tool-name detail input request (plist-get decision :state)
             (funcall guard (lambda (outcome)
			      (if (memq outcome '(allow allow-once allow-session always-allow))
				  (finish-allow (plist-get decision :state) outcome)
				(funcall cont outcome))))))
           (t
            (niwa-tool-exec-permission--escalation-prompt-async
             tool-name detail input request via cont))))))
     (step))))

(defun niwa-tool-exec-permission-check-bash-async
    (_tool-struct input cont)
  "Authorize Bash INPUT, then layer any requested child authority.
CONT follows the async permission slot contract."
  (niwa-permission--run-async
   (niwa-diagnostic-scope
    (or (plist-get (plist-get input :permission-context) :session)
        (niwa-queue--current-session)))
   (lambda (cont guard)
     (let* ((input (niwa-tool-exec-permission--capture-permission-origin input))
            (command (plist-get input :command))
            (permission-context (plist-get input :permission-context))
            (request
             (niwa-tool-exec-permission--prepare-additional-authority-request
              "Bash"
              (niwa-tool-exec-permission-effective-sandbox-request
               input "Bash" command nil permission-context)
              permission-context command)))
       (if (null command)
           (funcall cont nil)
         (niwa-tool-exec-permission--authorize-async
          "Bash" command input request cont guard))))
   (lambda (decision)
     (funcall cont
              (if (or (plist-get input :permission-decision-metadata)
                      (and (niwa-permission-decision-metadata-p decision)
                           (eq (plist-get decision :outcome) 'internal-error)))
                  decision
                (niwa-permission-decision-raw-outcome decision))))))

(defun niwa-tool-exec-permission-check-eval-async
    (_tool-struct input cont)
  "Authorize Eval INPUT, then layer any requested child authority.
CONT follows the async permission slot contract."
  (niwa-permission--run-async
   (niwa-diagnostic-scope
    (or (plist-get (plist-get input :permission-context) :session)
        (niwa-queue--current-session)))
   (lambda (cont guard)
     (let* ((input (niwa-tool-exec-permission--capture-permission-origin input))
            (mode (niwa-tool-exec-permission-eval-mode input))
            (expression (plist-get input :expression))
            (permission-context (plist-get input :permission-context))
            (metadata-p (plist-get input :permission-decision-metadata))
            (session (or (plist-get permission-context :session)
                         (niwa-queue--current-session)))
            (target (and session
                         (niwa-session-execution-target session))))
       (cond
        ((and (memq mode '(fresh sandbox))
              target
              (niwa-execution-target-remote-p target))
         (funcall
          cont
          (niwa-tool-exec-permission--permission-decision-result
           metadata-p
           (cons 'deny
                 (format "%s Eval is unavailable for remote sessions; use Live Eval or Bash on the execution target"
                         (capitalize (symbol-name mode))))
           'eval-policy)))
        (t
         (let ((request
                (niwa-tool-exec-permission--prepare-additional-authority-request
                 "Eval"
                 (niwa-tool-exec-permission-effective-sandbox-request
                  input "Eval" expression mode permission-context)
                 permission-context expression)))
           (if (null expression)
               (progn
                 (when metadata-p
                   (niwa-tool-exec-permission--log-permission-decision
                    "Eval" 'deny 'eval-policy permission-context))
                 (funcall cont
                          (niwa-tool-exec-permission--permission-decision-result
                           metadata-p 'deny 'eval-policy)))
             (niwa-tool-exec-permission--authorize-async
              "Eval" expression input request cont guard)))))))
   (lambda (decision)
     (funcall cont
              (if (or (plist-get input :permission-decision-metadata)
                      (and (niwa-permission-decision-metadata-p decision)
                           (eq (plist-get decision :outcome) 'internal-error)))
                  decision
                (niwa-permission-decision-raw-outcome decision))))))

(provide 'niwa-tool-exec-permission)

;;; niwa-tool-exec-permission.el ends here
