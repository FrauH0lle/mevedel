;;; mevedel-tool-exec-permission.el -- Bash/Eval permission adapters -*- lexical-binding: t -*-

;;; Commentary:

;; Owns execution-specific authority normalization, persistence, permission
;; queue adapters, and Bash/Eval permission prompt orchestration.

;;; Code:

(eval-when-compile
  (require 'mevedel-tool-registry))

(require 'cl-lib)
(require 'mevedel-bash-analysis)
(require 'mevedel-bash-policy)
(require 'mevedel-execution-target)
(require 'mevedel-permission-log)
(require 'mevedel-permission-persistence)
(require 'mevedel-permission-rules)
(require 'mevedel-permissions)
(require 'mevedel-structs)
(require 'mevedel-turn)
(require 'seq)
(require 'subr-x)

;; `mevedel-agents'
(defvar mevedel--agent-invocation)

;; `mevedel-bash-analysis'
(declare-function mevedel-bash-analysis-analyze
                  "mevedel-bash-analysis" (source))

;; `mevedel-bash-policy'
(declare-function mevedel-bash-policy-allow-patterns
                  "mevedel-bash-policy" (command))
(declare-function mevedel-bash-policy-buckets
                  "mevedel-bash-policy" (&optional permission-context))
(declare-function mevedel-bash-policy-check-permission
                  "mevedel-bash-policy" (command &rest keys))
(declare-function mevedel-bash-policy-command-names
                  "mevedel-bash-policy" (analysis))
(declare-function mevedel-bash-policy-commands-summary
                  "mevedel-bash-policy" (commands))
(declare-function mevedel-bash-policy-decision-specifier-value
                  "mevedel-bash-policy" (command))
(declare-function mevedel-bash-policy-effective-permission-mode
                  "mevedel-bash-policy" (&optional permission-context))
(declare-function mevedel-bash-policy-effective-sandbox-mode
                  "mevedel-bash-policy" (&optional permission-context))
(declare-function mevedel-bash-policy-explicit-deny-p
                  "mevedel-bash-policy"
                  (buckets command &optional analysis))
(declare-function mevedel-bash-policy-full-auto-guardian-needed-p
                  "mevedel-bash-policy"
                  (command &optional permission-context))
(declare-function mevedel-bash-policy-guardian-classify-async
                  "mevedel-bash-policy" (command context callback))
(declare-function mevedel-bash-policy-guardian-context
                  "mevedel-bash-policy"
                  (command &optional permission-context))
(declare-function mevedel-bash-policy-missing-resource-paths
                  "mevedel-bash-policy"
                  (command permission-context request))
(declare-function mevedel-bash-policy-reusable-operation-p
                  "mevedel-bash-policy" (command))
(defvar mevedel-permission-guardian)

;; `mevedel-execution-target'
(declare-function mevedel-execution-target-create
                  "mevedel-execution-target" (workspace-root))
(declare-function mevedel-execution-target-expand-path
                  "mevedel-execution-target" (target path &optional directory))
(declare-function mevedel-execution-target-remote-p
                  "mevedel-execution-target" (target))

;; `mevedel-interaction-prompt'
(declare-function mevedel--prompt-attribution-line
                  "mevedel-interaction-prompt" (origin))

(autoload 'mevedel--prompt-attribution-line "mevedel-interaction-prompt")

;; `mevedel-permission-log'
(declare-function mevedel-permission-log
                  "mevedel-permission-log" (session event &rest props))

;; `mevedel-permission-prompt'
(declare-function mevedel-permission--format-authority-capabilities
                  "mevedel-permission-prompt" (entry))
(declare-function mevedel-permission--elide
                  "mevedel-permission-prompt"
                  (text entry &optional line-limit char-limit))
(declare-function mevedel-permission--format-remember-authority
                  "mevedel-permission-prompt" (entry))
(declare-function mevedel-permission--prompt-async-eval
                  "mevedel-permission-prompt"
                  (content cont &optional count entry))

;; `mevedel-permission-queue'
(declare-function mevedel-permission--enqueue "mevedel-permission-queue"
                  (entry &optional session))
(declare-function mevedel-permission-queue--current-session
                  "mevedel-permission-queue" ())
(declare-function mevedel-permission-queue--render-head
                  "mevedel-permission-queue" (&optional session))

;; `mevedel-permission-persistence'
(declare-function mevedel-permission-persistence-load-resource-grants
                  "mevedel-permission-persistence" (workspace))

;; `mevedel-permission-rules'
(declare-function mevedel-permission-rules-bucket-decision
                  "mevedel-permission-rules"
                  (buckets tool-name path pattern domain name))
(declare-function mevedel-permission-rules-execution-level-decision
                  "mevedel-permission-rules"
                  (buckets tool-name level pattern))
(declare-function mevedel-permission-rules-find
                  "mevedel-permission-rules"
                  (rules tool-name &rest keys))
(declare-function mevedel-permission-rules-merge-resource-grant
                  "mevedel-permission-rules"
                  (grants path access &optional recursive))
(declare-function mevedel-permission-rules-network-decision
                  "mevedel-permission-rules" (buckets tool-name pattern))
(declare-function mevedel-permission-rules-qualified-buckets
                  "mevedel-permission-rules" (buckets qualifier value))
(declare-function mevedel-permission-rules-resource-granted-p
                  "mevedel-permission-rules"
                  (path access grants &optional recursive))

;; `mevedel-permissions'
(declare-function mevedel-permission--apply-prompt-result
                  "mevedel-permissions"
                  (result tool-name &optional session workspace path
                          &rest keys))
(declare-function mevedel-permission--invocation-context
                  "mevedel-permissions" (&rest keys))
(declare-function mevedel-permission--normalize-outcome
                  "mevedel-permissions" (outcome))
(declare-function mevedel-permission--one-shot-prompt-entry
                  "mevedel-permissions" (entry &optional data-buffer))
(declare-function mevedel-permission--one-shot-prompt-outcome
                  "mevedel-permissions" (outcome))

;; `mevedel-queue'
(declare-function mevedel-queue--entry-metadata-put
                  "mevedel-queue" (entry key value))
(autoload 'mevedel-queue--entry-metadata-put "mevedel-queue")

;; `mevedel-structs'
(declare-function mevedel-request-p "mevedel-structs" (cl-x))
(declare-function mevedel-session-execution-target
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-p "mevedel-structs" (cl-x))
(declare-function mevedel-session-resource-grants "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-workspace "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-working-directory "mevedel-structs" (cl-x) t)
(declare-function mevedel-workspace-root "mevedel-structs" (cl-x) t)
(defvar mevedel--current-request)
(defvar mevedel--session)
(defvar mevedel--workspace)

;; `mevedel-telemetry'
(declare-function mevedel-telemetry-forwarded-audit-p
                  "mevedel-telemetry" (session))
(declare-function mevedel-telemetry-record-audit
                  "mevedel-telemetry" (session event &rest props))

;; `mevedel-turn'
(declare-function mevedel-current-origin "mevedel-turn" ())
(declare-function mevedel-request-push-canceller
                  "mevedel-turn" (request canceller))

;; `mevedel-utilities'
(declare-function mevedel--warn-once
                  "mevedel-utilities" (key format &rest args))


;;
;;; Permission queue helpers

(defun mevedel-tool-exec-permission-session ()
  "Return the session visible to a Bash/Eval permission adapter."
  (require 'mevedel-permission-queue)
  (or (and (boundp 'mevedel--session) mevedel--session)
      (mevedel-permission-queue--current-session)))

(defun mevedel-tool-exec-permission-current-context
    (tool-name args &optional session)
  "Return TOOL-NAME's direct execution context for ARGS and SESSION."
  (mevedel-permission--invocation-context
   :tool-name tool-name
   :args args
   :session session
   :workspace (and session (mevedel-session-workspace session))
   :request mevedel--current-request
   :invocation (and (boundp 'mevedel--agent-invocation)
                    mevedel--agent-invocation)
   :buffer (current-buffer)))

(defun mevedel-tool-exec-permission-default-directory ()
  "Return the working directory for Bash and Eval."
  (let* ((session (and (boundp 'mevedel--session) mevedel--session))
         (workspace (or (and session (mevedel-session-workspace session))
                        (and (boundp 'mevedel--workspace)
                             mevedel--workspace)))
         (session-dir (and session
                           (ignore-errors
                             (mevedel-session-working-directory session))))
         (root (and workspace
                    (ignore-errors (mevedel-workspace-root workspace)))))
    (file-name-as-directory (or session-dir root default-directory))))

(defun mevedel-tool-exec-permission--capture-permission-origin (input)
  "Return INPUT with its permission owner and session captured."
  (let* ((copy (copy-sequence input))
         (context (copy-sequence (plist-get copy :permission-context))))
    (unless (plist-get context :origin)
      (setq context
            (plist-put context :origin
                       (mevedel-current-origin))))
    (unless (plist-member context :session)
      (setq context
            (plist-put context :session
                       (mevedel-tool-exec-permission-session))))
    (unless (plist-member context :execution-directory)
      (setq context
            (plist-put context :execution-directory
                       (mevedel-tool-exec-permission-default-directory))))
    (plist-put copy :permission-context context)))

(defun mevedel-tool-exec-permission--permission-origin (permission-context)
  "Return the captured owner from PERMISSION-CONTEXT."
  (or (plist-get permission-context :origin)
      (mevedel-current-origin)))

(defun mevedel-tool-exec-permission--command-permission-input (input request)
  "Return INPUT carrying REQUEST facts for command authorization."
  (let* ((copy (copy-sequence input))
         (context (copy-sequence (plist-get copy :permission-context))))
    (setq context (plist-put context :sandbox-request request))
    (when (plist-get (plist-get request :additional-permissions) :file-system)
      (setq context
            (plist-put context :resource-authority-separated-p t)))
    (plist-put copy :permission-context context)))

(defun mevedel-tool-exec-permission--mutation-p (tool-name detail)
  "Return non-nil unless TOOL-NAME runs DETAIL as read-only Bash."
  (or (not (equal tool-name "Bash"))
      (not (eq 'read-only
               (plist-get (mevedel-bash-analysis-analyze detail) :class)))))

(defun mevedel-tool-exec-permission--request-permission
    (entry permission-context &optional session)
  "Submit ENTRY through PERMISSION-CONTEXT's request boundary.
Fall back to direct queue admission for callers outside the tool pipeline."
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
            (mevedel-permission--one-shot-prompt-entry
             entry (plist-get permission-context :buffer)))
      (setq entry
            (plist-put
             entry :callback
             (lambda (outcome)
               (funcall callback
                        (mevedel-permission--one-shot-prompt-outcome
                         outcome)))))))
  (if-let* ((request (plist-get permission-context :permission-request)))
      (funcall request entry session (plist-get entry :callback))
    (if session
        (mevedel-permission--enqueue entry session)
      (mevedel-permission--enqueue entry))))

(defun mevedel-tool-exec-permission--permission-decision-result
    (metadata-p outcome via &rest props)
  "Return OUTCOME, or metadata when METADATA-P is non-nil."
  (if metadata-p
      (append (list :outcome (mevedel-permission--normalize-outcome outcome)
                    :raw-outcome outcome
                    :via via
                    :logged t)
              props)
    outcome))

(defun mevedel-tool-exec-permission--log-permission-decision
    (tool-name outcome via permission-context &rest props)
  "Persist TOOL-NAME OUTCOME via VIA from PERMISSION-CONTEXT and PROPS."
  (when-let* ((session
               (or (plist-get permission-context :session)
                   (mevedel-tool-exec-permission-session))))
    (let* ((origin (mevedel-tool-exec-permission--permission-origin permission-context))
           (mode (mevedel-bash-policy-effective-permission-mode
                  permission-context))
           (outcome (mevedel-permission--normalize-outcome outcome)))
      (apply #'mevedel-permission-log
             session 'permission-decision
             (append (list :tool-name tool-name :origin origin :mode mode
                           :outcome outcome :via via)
                     props))
      (when (mevedel-telemetry-forwarded-audit-p session)
        (let ((safe (list :tool-name tool-name :origin origin :mode mode
                          :outcome outcome :via via)))
          (when-let* ((key (plist-get props :specifier-key))
                      ((memq key '(:path :pattern :domain :name))))
            (setq safe (plist-put safe :specifier-key key)))
          (when (plist-member props :protected-path)
            (setq safe
                  (plist-put safe :protected-path
                             (and (plist-get props :protected-path) t))))
          (when-let* ((access (plist-get props :resource-access))
                      ((memq access '(read write))))
            (setq safe (plist-put safe :resource-access access)))
          (dolist (key '(:sandbox-permissions :bucket :command-class))
            (when-let* ((value (plist-get props key))
                        ((symbolp value)))
              (setq safe (plist-put safe key value))))
          (apply #'mevedel-telemetry-record-audit
                 session 'permission-decision safe))))))

(defun mevedel-tool-exec-permission--validate-additional-plist (value allowed label)
  "Return VALUE after validating its keys against ALLOWED for LABEL."
  (unless (and (listp value)
               (proper-list-p value)
               (zerop (% (length value) 2)))
    (error "%s must be an object" label))
  (let ((tail value))
    (while tail
      (let ((key (pop tail)))
        (pop tail)
        (unless (memq key allowed)
          (error "Unknown %s field: %s" label key)))))
  value)

(defun mevedel-tool-exec-permission--filesystem-paths
    (value access &optional target directory)
  "Return exact filesystem permission entries from VALUE at ACCESS.
TARGET and DIRECTORY bind target-native paths to their execution target."
  (let ((paths
         (cond
          ((or (null value) (eq value :json-false)) nil)
          ((vectorp value) (append value nil))
          ((proper-list-p value) value)
          (t (error "Filesystem %s permission must be an array" access)))))
    (mapcar
     (lambda (path)
       (unless (and (stringp path)
                    (not (string-empty-p path))
                    (file-name-absolute-p path))
         (error "Filesystem permission path must be absolute: %S" path))
       (list :path (if target
                       (mevedel-execution-target-expand-path
                        target path directory)
                     (expand-file-name path))
             :access access))
     paths)))

(defun mevedel-tool-exec-permission--normalize-additional-permissions
    (additional &optional target directory)
  "Return validated ADDITIONAL permissions, or nil when none are requested.
TARGET and DIRECTORY bind target-native filesystem paths."
  (mevedel-tool-exec-permission--validate-additional-plist
   additional '(:network :file_system) "Additional permissions")
  (let ((network (plist-get additional :network))
        (file-system (plist-get additional :file_system))
        grants profile)
    (unless (memq network '(nil t :json-false))
      (error "Network permission must be true or false"))
    (when (and file-system (not (eq file-system :json-false)))
      (mevedel-tool-exec-permission--validate-additional-plist
       file-system '(:read :write) "Filesystem permissions")
      (let ((reads (mevedel-tool-exec-permission--filesystem-paths
                    (plist-get file-system :read) 'read target directory))
            (writes (mevedel-tool-exec-permission--filesystem-paths
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

(defun mevedel-tool-exec-permission--sandbox-request
    (args tool &optional eval-mode permission-context)
  "Return the validated child-execution request from ARGS.
TOOL is `bash' or `eval'.  EVAL-MODE distinguishes live from batch Eval.
PERMISSION-CONTEXT supplies the session execution target."
  (let* ((session (or (plist-get permission-context :session)
                      (and (boundp 'mevedel--session) mevedel--session)))
         (directory
          (or (and session (mevedel-session-working-directory session))
              (mevedel-tool-exec-permission-default-directory)))
         (target (or (and session
                          (mevedel-session-execution-target session))
                     (and (file-remote-p directory)
                          (mevedel-execution-target-create directory))))
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
           (t (error "Unknown sandbox permission level: %s" raw-level))))
         (additional (plist-get args :additional_permissions))
         (normalized-additional
          (and additional
               (not (eq additional :json-false))
               (mevedel-tool-exec-permission--normalize-additional-permissions
                additional target directory)))
         (justification (plist-get args :justification)))
    (pcase level
      ('use-default
       (when normalized-additional
         (error "Default sandbox execution cannot include escalation arguments"))
       '(:level use-default :additional-permissions nil))
      ('additive
       (unless (and (stringp justification)
                    (not (string-empty-p (string-trim justification))))
         (error "Additional permissions require a justification"))
       (when (and (eq tool 'eval) (not (eq eval-mode 'batch)))
         (error "Additional permissions are available only to batch Eval"))
       (list :level 'additive
             :additional-permissions
             (or normalized-additional
                 (error
                  "Additional permissions must contain a non-empty capability"))
             :justification (string-trim justification)))
      ('escalated
       (unless (and (stringp justification)
                    (not (string-empty-p (string-trim justification))))
         (error "Full sandbox escalation requires a justification"))
       (when normalized-additional
         (error "Full sandbox escalation cannot include additional permissions"))
       (when (and (eq tool 'eval) (not (eq eval-mode 'batch)))
         (error "Full sandbox escalation is available only to batch Eval"))
       (list :level 'escalated
             :sandbox-permissions 'require-escalated
             :additional-permissions nil
             :justification (string-trim justification))))))

(defun mevedel-tool-exec-permission--permission-allow-p (outcome)
  "Return non-nil when permission OUTCOME authorizes execution."
  (eq 'allow
      (if (and (consp outcome)
               (keywordp (car outcome))
               (plist-member outcome :outcome))
          (plist-get outcome :outcome)
        outcome)))

(defun mevedel-tool-exec-permission--additional-denial
    (metadata-p via &optional feedback)
  "Return an additional-authority denial through VIA for METADATA-P."
  (mevedel-tool-exec-permission--permission-decision-result
   metadata-p
   (if feedback
       (cons 'deny
             (format "Additional permission denied. Feedback: %s" feedback))
     'deny)
   via))

(defun mevedel-tool-exec-permission--filesystem-resource-granted-p
    (grant permission-context)
  "Return non-nil when PERMISSION-CONTEXT already authorizes GRANT."
  (let* ((session (plist-get permission-context :session))
         (grants
          (append (plist-get permission-context :resource-grants)
                  (and session (mevedel-session-resource-grants session)))))
    (mevedel-permission-rules-resource-granted-p
     (plist-get grant :path) (plist-get grant :access) grants
     (plist-get grant :recursive))))

(defun mevedel-tool-exec-permission--filesystem-resource-rule-action
    (tool-name grant permission-context)
  "Return the authoritative `deny' or `ask' rule for TOOL-NAME's GRANT."
  (let ((buckets (mevedel-bash-policy-buckets permission-context))
        (path (plist-get grant :path)))
    (let ((action (mevedel-permission-rules-bucket-decision
                   buckets tool-name path nil nil nil)))
      (and (memq action '(deny ask)) action))))

(defun mevedel-tool-exec-permission--additional-profile (network grants)
  "Return an additive authority profile for NETWORK and GRANTS."
  (let (profile)
    (when network
      (setq profile (plist-put profile :network t)))
    (when grants
      (setq profile (plist-put profile :file-system grants)))
    profile))

(defun mevedel-tool-exec-permission--merge-additional-profiles (&rest profiles)
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
                (mevedel-permission-rules-merge-resource-grant
                 grants
                 (plist-get grant :path)
                 (plist-get grant :access)
                 (plist-get grant :recursive))))))
    (mevedel-tool-exec-permission--additional-profile network grants)))

(defun mevedel-tool-exec-permission--direct-resource-grants (permission-context)
  "Return direct user resource grants from PERMISSION-CONTEXT."
  (if (plist-member permission-context :resource-grants)
      (plist-get permission-context :resource-grants)
    (let* ((session (plist-get permission-context :session))
           (workspace
            (or (plist-get permission-context :workspace)
                (and session (mevedel-session-workspace session)))))
      (append
       (and session (mevedel-session-resource-grants session))
       (and workspace
            (mevedel-permission-persistence-load-resource-grants
             workspace))))))

(defun mevedel-tool-exec-permission--remembered-additional-profile
    (tool-name operation permission-context)
  "Return direct remembered authority for TOOL-NAME and OPERATION."
  (let* ((buckets (mevedel-bash-policy-buckets permission-context))
         (direct-buckets
          (seq-filter
           (lambda (entry)
             (memq (car entry) '(:session :persistent :defcustom)))
           buckets))
         (network
          (eq 'allow
              (mevedel-permission-rules-network-decision
               buckets tool-name operation)))
         candidates)
    (dolist (entry direct-buckets)
      (dolist (rule
               (mevedel-permission-rules-find
                (cdr entry) tool-name :pattern operation))
        (when (eq 'allow (plist-get (cdr rule) :action))
          (setq candidates
                (append candidates
                        (plist-get (cdr rule) :file-system))))))
    (let ((grants
           (mevedel-tool-exec-permission--direct-resource-grants permission-context)))
      (mevedel-tool-exec-permission--additional-profile
       network
       (cl-remove-if-not
        (lambda (candidate)
          (mevedel-permission-rules-resource-granted-p
           (plist-get candidate :path)
           (plist-get candidate :access)
           grants
           (plist-get candidate :recursive)))
        (plist-get
         (mevedel-tool-exec-permission--merge-additional-profiles
          (list :file-system candidates))
         :file-system))))))

(defun mevedel-tool-exec-permission-effective-sandbox-request
    (args tool-name operation &optional eval-mode permission-context)
  "Resolve ARGS into TOOL-NAME's effective child authority for OPERATION.
EVAL-MODE distinguishes live from batch Eval.  PERMISSION-CONTEXT supplies
remembered direct user authority."
  (let ((request
         (mevedel-tool-exec-permission--sandbox-request
          args (if (equal tool-name "Bash") 'bash 'eval) eval-mode
          permission-context)))
    (if (eq (plist-get request :level) 'escalated)
        request
      (let ((profile
             (mevedel-tool-exec-permission--merge-additional-profiles
              (plist-get request :additional-permissions)
              (and (or (equal tool-name "Bash")
                       (eq eval-mode 'batch))
                   (mevedel-tool-exec-permission--remembered-additional-profile
                    tool-name operation permission-context)))))
        (if profile
            (let ((copy (copy-sequence request)))
              (setq copy (plist-put copy :level 'additive))
              (plist-put copy :additional-permissions profile))
          request)))))

(defun mevedel-tool-exec-permission--additional-authority-state
    (tool-name request permission-context)
  "Classify TOOL-NAME's additive REQUEST under PERMISSION-CONTEXT."
  (let* ((requested (plist-get request :additional-permissions))
         (network (eq t (plist-get requested :network)))
         (network-action
          (and network
               (mevedel-permission-rules-network-decision
                (mevedel-bash-policy-buckets permission-context)
                tool-name
                (plist-get request :operation-pattern))))
         (network-granted
          (and network
               (not (eq network-action 'ask))
               (or (eq network-action 'allow)
                   (eq (mevedel-bash-policy-effective-sandbox-mode
                        permission-context)
                       'off)
                   (eq (mevedel-bash-policy-effective-permission-mode
                        permission-context)
                       'full-auto))))
         missing-grants
         granted-grants
         (deny-via (and (eq network-action 'deny) 'sandbox-network)))
    (dolist (grant (plist-get requested :file-system))
      (let ((action
             (mevedel-tool-exec-permission--filesystem-resource-rule-action
              tool-name grant permission-context)))
        (cond
         ((eq action 'deny)
          (setq deny-via 'sandbox-filesystem))
         ((and (not (eq action 'ask))
               (mevedel-tool-exec-permission--filesystem-resource-granted-p
                grant permission-context))
          (push grant granted-grants))
         (t (push grant missing-grants)))))
    (list
     :requested requested
     :missing
     (mevedel-tool-exec-permission--additional-profile
      (and network (not network-granted))
      (nreverse missing-grants))
     :granted
     (mevedel-tool-exec-permission--additional-profile
      network-granted
      (nreverse granted-grants))
     :deny-via deny-via)))

(defun mevedel-tool-exec-permission--prepare-additional-authority-request
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
                      (mevedel-bash-policy-reusable-operation-p
                       operation-pattern)
                    t))))
        (setq copy (plist-put copy :reusable-operation-p reusable))
        (setq copy
              (plist-put
               copy :remember-patterns
               (and reusable
                    (if (equal tool-name "Bash")
                        (mevedel-bash-policy-allow-patterns
                         operation-pattern)
                      (list operation-pattern)))))
        (setq copy
              (plist-put copy :remember-cell
                         (list (and reusable '(:operation t))))))
      (let* ((state
              (mevedel-tool-exec-permission--additional-authority-state
               tool-name copy permission-context))
             (missing (plist-get state :missing))
             (selection
              (append
               (and (plist-get copy :reusable-operation-p)
                    '(:operation t))
               (and (plist-get copy :reusable-operation-p)
                    (plist-get missing :network)
                    '(:network t))
               (and (plist-get missing :file-system)
                    (list :file-system
                          (copy-tree
                           (plist-get missing :file-system)))))))
        (setq copy (plist-put copy :authority-state state))
        (setcar (plist-get copy :remember-cell) selection))
      (plist-put copy :approval-cell (list nil)))))

(defun mevedel-tool-exec-permission--apply-remembered-authority
    (outcome tool-name request session workspace)
  "Store OUTCOME authority selected in REQUEST for TOOL-NAME."
  (when (memq outcome '(allow-session always-allow))
    (let* ((selection
            (car (plist-get request :remember-cell)))
           (patterns (plist-get request :remember-patterns))
           (profile
            (mevedel-tool-exec-permission--additional-profile
             (plist-get selection :network)
             (plist-get selection :file-system)))
           (profile-patterns
            (and (plist-get selection :operation)
                 profile
                 (if (and (equal tool-name "Bash")
                          (cdr (plist-get
                                (mevedel-bash-analysis-analyze
                                 (plist-get request :operation-pattern))
                                :segments)))
                     (list (plist-get request :operation-pattern))
                   patterns))))
      (dolist (pattern patterns)
        (when (and (plist-get selection :operation)
                   (not (member pattern profile-patterns)))
          (mevedel-permission--apply-prompt-result
           outcome tool-name session workspace nil
           :spec-key :pattern :spec-value pattern)))
      (dolist (pattern profile-patterns)
        (mevedel-permission--apply-prompt-result
         outcome tool-name session workspace nil
         :spec-key :pattern :spec-value pattern
         :network (plist-get profile :network)
         :file-system (plist-get profile :file-system)))
      (dolist (grant (plist-get selection :file-system))
        (let ((path (plist-get grant :path))
              (access (plist-get grant :access)))
          (mevedel-permission--apply-prompt-result
           outcome tool-name session workspace path
           :spec-key :path :spec-value path
           :resource-access access
           :resource-recursive (plist-get grant :recursive)))))))

(defun mevedel-tool-exec-permission--log-additional-authority
    (tool-name state permission-context metadata-p)
  "Log resolved additive STATE for TOOL-NAME under PERMISSION-CONTEXT."
  (when metadata-p
    (when (plist-get (plist-get state :requested) :network)
      (mevedel-tool-exec-permission--log-permission-decision
       tool-name 'allow 'sandbox-network permission-context
       :sandbox-permissions 'additive
       :additional-permissions '(:network t)))
    (dolist (grant
             (plist-get (plist-get state :requested) :file-system))
      (mevedel-tool-exec-permission--log-permission-decision
       tool-name 'allow 'sandbox-filesystem permission-context
       :sandbox-permissions 'additive
       :specifier-key :path
       :specifier-value (plist-get grant :path)
       :resource-access (plist-get grant :access)))))

(defun mevedel-tool-exec-permission--check-additional-permission-async
    (tool-name detail input request command-outcome cont)
  "Layer REQUEST authority for TOOL-NAME and DETAIL over COMMAND-OUTCOME.
INPUT supplies permission context and delegated trust.  Call CONT once."
  (if (or (not (mevedel-tool-exec-permission--permission-allow-p command-outcome))
          (eq (plist-get request :level) 'use-default))
      (funcall cont command-outcome)
    (let* ((permission-context (plist-get input :permission-context))
           (metadata-p (plist-get input :permission-decision-metadata))
           (trust-literal-p (plist-get input :trust-literal-p))
           (state
            (or (plist-get request :authority-state)
                (mevedel-tool-exec-permission--additional-authority-state
                 tool-name request permission-context)))
           (missing (plist-get state :missing))
           (missing-grants (plist-get missing :file-system))
           (first-grant (car missing-grants))
           (session (plist-get permission-context :session))
           (workspace (or (plist-get permission-context :workspace)
                          (and session
                               (mevedel-session-workspace session))))
           (via (if (plist-get missing :network)
                    'sandbox-network
                  'sandbox-filesystem)))
      (cond
       (trust-literal-p
        (funcall
         cont
         (mevedel-tool-exec-permission--permission-decision-result
          metadata-p
          (cons 'deny
                "Delegated expansion cannot request additional sandbox authority")
          'sandbox-policy)))
       ((plist-get state :deny-via)
        (funcall cont
                 (mevedel-tool-exec-permission--additional-denial
                  metadata-p (plist-get state :deny-via))))
       ((or (null missing)
            (car (plist-get request :approval-cell)))
        (mevedel-tool-exec-permission--apply-remembered-authority
         (car (plist-get request :approval-cell))
         tool-name request session workspace)
        (mevedel-tool-exec-permission--log-additional-authority
         tool-name state permission-context metadata-p)
        (funcall cont command-outcome))
       (t
        (mevedel-tool-exec-permission--request-permission
         (list
          :kind 'sandbox
          :tool-name tool-name
          :detail detail
          :mutation-p (mevedel-tool-exec-permission--mutation-p tool-name detail)
          :sandbox-permissions 'additive
          :additional-permissions (plist-get state :requested)
          :requested-additional-permissions (plist-get state :requested)
          :missing-additional-permissions missing
          :granted-additional-permissions (plist-get state :granted)
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
          :origin (mevedel-tool-exec-permission--permission-origin permission-context)
          :callback
          (lambda (outcome)
            (pcase outcome
              ((or 'allow 'allow-once 'allow-session 'always-allow)
               (mevedel-tool-exec-permission--apply-remembered-authority
                outcome tool-name request session workspace)
               (mevedel-tool-exec-permission--log-additional-authority
                tool-name state permission-context metadata-p)
               (funcall cont command-outcome))
              ('deny-session
               (dolist (grant missing-grants)
                 (let ((path (plist-get grant :path))
                       (access (plist-get grant :access)))
                   (mevedel-permission--apply-prompt-result
                    outcome tool-name session workspace path
                    :spec-key :path :spec-value path
                    :resource-access access)))
               (funcall cont
                        (mevedel-tool-exec-permission--additional-denial
                         metadata-p via)))
              (`(deny . ,reason)
               (funcall
                cont
                (mevedel-tool-exec-permission--permission-decision-result
                 metadata-p (cons 'deny reason) via)))
              (`(feedback . ,text)
               (funcall cont
                        (mevedel-tool-exec-permission--additional-denial
                         metadata-p via text)))
              ('aborted
               (funcall
                cont
                (mevedel-tool-exec-permission--permission-decision-result
                 metadata-p 'aborted via)))
              (_
               (funcall cont
                        (mevedel-tool-exec-permission--additional-denial
                         metadata-p via))))))
         permission-context session))))))

(defun mevedel-tool-exec-permission--full-escalation-explicit-deny-p
    (tool-name detail buckets)
  "Return non-nil when ordinary rules deny TOOL-NAME and DETAIL."
  (if (equal tool-name "Bash")
      (mevedel-bash-policy-explicit-deny-p buckets detail)
    (eq 'deny
        (mevedel-permission-rules-bucket-decision
         buckets tool-name nil detail nil nil))))

(defun mevedel-tool-exec-permission-full-escalation-rule-decision
    (tool-name detail buckets level)
  "Return the full-escalation rule decision for TOOL-NAME and DETAIL.
BUCKETS supplies ordinary and execution-level rules for LEVEL."
  (if (or (mevedel-tool-exec-permission--full-escalation-explicit-deny-p
           tool-name detail buckets)
          (and (equal tool-name "Bash")
               (mevedel-bash-policy-explicit-deny-p
                (mevedel-permission-rules-qualified-buckets
                 buckets :sandbox-permissions level)
                detail)))
      'deny
    (mevedel-permission-rules-execution-level-decision
     buckets tool-name level detail)))

(defun mevedel-tool-exec-permission--full-escalation-reusable-rule-p
    (tool-name detail)
  "Return non-nil when a prompt may offer reusable authority for DETAIL."
  (and (stringp detail)
       (not (string-empty-p (string-trim detail)))
       (if (equal tool-name "Bash")
           (mevedel-bash-policy-reusable-operation-p detail)
         (equal tool-name "Eval"))))

(defun mevedel-tool-exec-permission--full-escalation-denial
    (metadata-p &optional feedback)
  "Return a full-escalation denial for METADATA-P and FEEDBACK."
  (mevedel-tool-exec-permission--permission-decision-result
   metadata-p
   (if feedback
       (cons 'deny
             (format "Full execution escalation denied. Feedback: %s"
                     feedback))
     'deny)
   'sandbox-full-escalation
   :sandbox-permissions 'require-escalated))

(defun mevedel-tool-exec-permission--apply-full-escalation-prompt-result
    (outcome tool-name detail level session workspace metadata-p)
  "Apply full-escalation prompt OUTCOME and return its permission result."
  (pcase outcome
    ((or 'allow 'allow-once)
     (mevedel-tool-exec-permission--permission-decision-result
      metadata-p 'allow 'sandbox-full-escalation
      :sandbox-permissions level))
    ((or 'allow-session 'always-allow)
     (mevedel-permission--apply-prompt-result
      outcome tool-name session workspace nil
      :spec-key :pattern :spec-value detail
      :sandbox-permissions level)
     (mevedel-tool-exec-permission--permission-decision-result
      metadata-p 'allow 'sandbox-full-escalation
      :sandbox-permissions level))
    ('deny-session
     (mevedel-permission--apply-prompt-result
      outcome tool-name session workspace nil
      :spec-key :pattern :spec-value detail
      :sandbox-permissions level)
     (mevedel-tool-exec-permission--full-escalation-denial metadata-p))
    (`(feedback . ,text)
     (mevedel-tool-exec-permission--full-escalation-denial metadata-p text))
    (`(deny . ,reason)
     (mevedel-tool-exec-permission--permission-decision-result
      metadata-p (cons 'deny reason) 'sandbox-full-escalation
      :sandbox-permissions level))
    ('aborted
     (mevedel-tool-exec-permission--permission-decision-result
      metadata-p 'aborted 'sandbox-full-escalation
      :sandbox-permissions level))
    (_ (mevedel-tool-exec-permission--full-escalation-denial metadata-p))))

(defun mevedel-tool-exec-permission--check-full-escalation-async
    (tool-name detail input request cont)
  "Authorize REQUEST to run TOOL-NAME and DETAIL without confinement.
Only direct, user-authored rules qualified with `require-escalated' may skip
the prompt.  Delegated expansion never prompts for or grants this authority."
  (let* ((permission-context (plist-get input :permission-context))
         (metadata-p (plist-get input :permission-decision-metadata))
         (trust-literal-p (plist-get input :trust-literal-p))
         (buckets (mevedel-bash-policy-buckets permission-context))
         (level (plist-get request :sandbox-permissions))
         (decision
          (mevedel-tool-exec-permission-full-escalation-rule-decision
           tool-name detail buckets level))
         (session (or (plist-get permission-context :session)
                      (and (boundp 'mevedel--session) mevedel--session)))
         (workspace (or (plist-get permission-context :workspace)
                        (and session (mevedel-session-workspace session)))))
    (cond
     ((eq decision 'deny)
      (when metadata-p
        (mevedel-tool-exec-permission--log-permission-decision
         tool-name 'deny 'sandbox-full-escalation permission-context
         :sandbox-permissions level
         :specifier-key :pattern :specifier-value detail))
      (funcall cont
               (mevedel-tool-exec-permission--full-escalation-denial metadata-p)))
     (trust-literal-p
      (funcall
       cont
       (mevedel-tool-exec-permission--permission-decision-result
        metadata-p
        (cons 'deny
              "Delegated expansion cannot request full execution escalation")
        'sandbox-policy
        :sandbox-permissions level)))
     ((and (eq decision 'allow)
           (not (plist-get permission-context :one-shot-mutations-p)))
      (when metadata-p
        (mevedel-tool-exec-permission--log-permission-decision
         tool-name 'allow 'sandbox-full-escalation permission-context
         :sandbox-permissions level
         :specifier-key :pattern :specifier-value detail))
      (funcall
       cont
       (mevedel-tool-exec-permission--permission-decision-result
        metadata-p 'allow 'sandbox-full-escalation
        :sandbox-permissions level
        :specifier-key :pattern :specifier-value detail)))
     (t
      (when metadata-p
        (mevedel-tool-exec-permission--log-permission-decision
         tool-name 'ask 'sandbox-full-escalation permission-context
         :sandbox-permissions level
         :specifier-key :pattern :specifier-value detail))
      (mevedel-tool-exec-permission--request-permission
       (list
        :kind 'sandbox
        :tool-name tool-name
        :detail detail
        :mutation-p (mevedel-tool-exec-permission--mutation-p tool-name detail)
        :sandbox-permissions level
        :justification (plist-get request :justification)
        :specifier-key :pattern
        :specifier-value detail
        :include-always
        (mevedel-tool-exec-permission--full-escalation-reusable-rule-p
         tool-name detail)
        :workspace workspace
        :origin (mevedel-tool-exec-permission--permission-origin permission-context)
        :callback
        (lambda (outcome)
          (funcall
           cont
           (mevedel-tool-exec-permission--apply-full-escalation-prompt-result
            outcome tool-name detail level session workspace metadata-p))))
       permission-context session)))))

;;

;;; Eval Prompt UI

(defcustom mevedel-eval-expression-display-limit 20
  "Maximum number of lines to show inline in the Eval permission prompt.
Expressions longer than this are truncated with a toggle to expand."
  :type 'integer
  :group 'mevedel)

(defun mevedel-tool-exec-permission-prompt-eval
    (expression callback &optional origin count entry mode preserve-ui)
  "Display Eval permission overlay for EXPRESSION and CALLBACK.

CALLBACK is invoked once with `allow-once', `deny-once', a feedback cons,
or `aborted'.  Long expressions are elided in the display and toggled
with TAB.  ORIGIN, when non-main,
renders the same attribution line used by generic and Bash permission
prompts.  COUNT is the permission queue depth for the composite
interaction-zone counter.  ENTRY identifies the queued prompt.  MODE and
PRESERVE-UI describe the requested execution scope."
  (unless (fboundp 'mevedel-permission--prompt-async-eval)
    (require 'mevedel-permission-prompt))
  (let* ((faced-expr (propertize expression
                                'font-lock-face 'font-lock-string-face))
         ;; Built twice: once elided for the prompt, once whole for the
         ;; remote descriptor, whose reader has no TAB to expand with.
         (build
          (lambda (display-expr)
            (concat
             "The LLM is requesting permission to evaluate elisp.\n\n"
             (mevedel--prompt-attribution-line origin)
             (propertize "Mode: " 'font-lock-face 'font-lock-escape-face)
             (format "%s" (or mode "live"))
             (when (equal (or mode "live") "live")
               (format " (inherently unconfined; preserve_ui: %s)"
                       (if preserve-ui "true" "false")))
             "\n"
             (when entry
               (concat
                (mevedel-permission--format-authority-capabilities entry)
                (mevedel-permission--format-remember-authority entry)))
             "\n"
             (propertize "Expression:\n"
                         'font-lock-face 'font-lock-escape-face)
             display-expr
             "\n\n")))
         (content
          (funcall build
                   (mevedel-permission--elide
                    faced-expr entry
                    mevedel-eval-expression-display-limit))))
    (when entry
      (mevedel-queue--entry-metadata-put
       entry :remote-body
       (substring-no-properties (funcall build faced-expr))))
    (if (fboundp 'mevedel-permission--prompt-async-eval)
        (mevedel-permission--prompt-async-eval content callback count entry)
      (mevedel--warn-once 'eval-permission-ui
                          "Eval permission UI unavailable")
      (funcall callback 'aborted))))


;;
;;; Eval permission adapter

(defun mevedel-tool-exec-permission-eval-mode (args)
  "Return the requested Eval execution mode from ARGS."
  (let ((mode (plist-get args :mode)))
    (cond
     ((or (null mode)
          (eq mode :json-false)
          (and (stringp mode) (string-empty-p mode))
          (equal mode "live"))
      'live)
     ((equal mode "batch") 'batch)
     (t (error "Unknown Eval mode: %s" mode)))))

(defun mevedel-tool-exec-permission-eval-preserve-ui-p (args)
  "Return non-nil when ARGS request restoring window state."
  (not (eq (plist-get args :preserve_ui) :json-false)))

(cl-defun mevedel-tool-exec-permission--check-eval-permission
    (&key expression trust-literal-p permission-context)
  "Decide Eval permission for TRUST-LITERAL-P and PERMISSION-CONTEXT.

Normal model-requested Eval asks unless a rule settles it or the effective
permission mode is `full-auto'.  Deny and ask rules remain final in every
mode.  TRUST-LITERAL-P identifies author-written skill body injections."
  (let* ((buckets (mevedel-bash-policy-buckets permission-context))
         (mode (mevedel-bash-policy-effective-permission-mode
                permission-context))
         (action (mevedel-permission-rules-bucket-decision
                  buckets "Eval" nil expression nil nil)))
    (cond
     ((eq action 'deny) 'deny)
     ((eq action 'ask) 'ask)
     ((eq action 'allow) 'allow)
     ((eq mode 'full-auto)
      'allow)
     (trust-literal-p
      (or action 'ask))
     (t 'ask))))

(defun mevedel-tool-exec-permission--normalize-prompt-result
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

(defun mevedel-tool-exec-permission--eval-prompt-result
    (outcome session workspace expression metadata-p)
  "Apply Eval prompt OUTCOME and return its canonical permission result."
  (let ((result
         (mevedel-tool-exec-permission--normalize-prompt-result
          outcome
          (lambda (stored-outcome)
            (mevedel-permission--apply-prompt-result
             stored-outcome "Eval" session workspace nil
             :spec-key :pattern
             :spec-value expression))
          "Eval cancelled by user. Feedback: ")))
    (mevedel-tool-exec-permission--permission-decision-result
     metadata-p result 'eval-policy)))

(defun mevedel-tool-exec-permission--eval-check-command-permission-async
    (_tool-struct input cont)
  "Async permission check for Eval tool INPUT.

Routes the prompt through the session permission queue rather
than calling `mevedel-tool-exec-permission-prompt-eval' directly.  The
queue's render-head dispatches to the specialized Eval UI via
`mevedel-permission-queue--render-eval'.  CONT receives the same
slot vocabulary as before: `allow', `deny', `(deny . REASON)',
`aborted' -- feedback text shaped into the existing
\"Eval cancelled by user. Feedback: TEXT\" form so LLM-visible
denial parity with the sync slot is preserved."
  (let* ((expression (plist-get input :expression))
         (trust-literal-p (plist-get input :trust-literal-p))
         (permission-context (plist-get input :permission-context))
         (metadata-p (plist-get input :permission-decision-metadata))
         mode
         mode-error
         (preserve-ui (mevedel-tool-exec-permission-eval-preserve-ui-p input)))
    (condition-case err
        (setq mode (mevedel-tool-exec-permission-eval-mode input))
      (error (setq mode-error (error-message-string err))))
    (cond
     (mode-error
      (when metadata-p
        (mevedel-tool-exec-permission--log-permission-decision
         "Eval" (cons 'deny mode-error) 'eval-policy permission-context))
      (funcall cont
               (mevedel-tool-exec-permission--permission-decision-result
                metadata-p (cons 'deny mode-error) 'eval-policy)))
     ((null expression)
      (when metadata-p
        (mevedel-tool-exec-permission--log-permission-decision
         "Eval" 'deny 'eval-policy permission-context))
      (funcall cont
               (mevedel-tool-exec-permission--permission-decision-result
                metadata-p 'deny 'eval-policy)))
     (t
      (pcase (mevedel-tool-exec-permission--check-eval-permission
              :expression expression
              :trust-literal-p trust-literal-p
              :permission-context permission-context)
        ('allow
         (when metadata-p
           (mevedel-tool-exec-permission--log-permission-decision
            "Eval" 'allow 'eval-policy permission-context))
         (funcall cont
                  (mevedel-tool-exec-permission--permission-decision-result
                   metadata-p 'allow 'eval-policy)))
        ('deny
         (when metadata-p
           (mevedel-tool-exec-permission--log-permission-decision
            "Eval" 'deny 'eval-policy permission-context))
         (funcall cont
                  (mevedel-tool-exec-permission--permission-decision-result
                   metadata-p 'deny 'eval-policy)))
        (_
         (if trust-literal-p
             (let ((outcome
                    (cons 'deny
                          "Elisp expansion requires a pre-approved Eval rule; no prompt is shown while preparing skill bodies.")))
               (when metadata-p
                 (mevedel-tool-exec-permission--log-permission-decision
                  "Eval" outcome 'eval-policy permission-context))
               (funcall
                cont
                (mevedel-tool-exec-permission--permission-decision-result
                 metadata-p outcome 'eval-policy)))
           (when metadata-p
             (mevedel-tool-exec-permission--log-permission-decision
              "Eval" 'ask 'eval-policy permission-context))
           (mevedel-tool-exec-permission--request-permission
            (let* ((session (plist-get permission-context :session))
                   (workspace
                    (or (plist-get permission-context :workspace)
                        (and session
                             (mevedel-session-workspace session)))))
              (list :kind 'eval
                    :expression expression
                    :mode (symbol-name mode)
                    :preserve-ui preserve-ui
                    :reusable-operation-p t
                    :remember-authority-cell (list '(:operation t))
                    :include-always (not (null workspace))
                    :workspace workspace
                    :specifier-key :pattern
                    :specifier-value expression
                    :origin
                    (mevedel-tool-exec-permission--permission-origin
                     permission-context)
                    :callback
                    (lambda (outcome)
                      (funcall
                       cont
                       (mevedel-tool-exec-permission--eval-prompt-result
                        outcome session workspace expression metadata-p)))))
            permission-context
            (plist-get permission-context :session)))))))))

(defun mevedel-tool-exec-permission-check-eval-async
    (tool-struct input cont)
  "Authorize Eval INPUT, then layer any requested child authority.
TOOL-STRUCT and CONT follow the async permission slot contract."
  (condition-case err
      (let* ((input (mevedel-tool-exec-permission--capture-permission-origin input))
             (mode (mevedel-tool-exec-permission-eval-mode input))
             (permission-context (plist-get input :permission-context))
             (session (or (plist-get permission-context :session)
                          (mevedel-tool-exec-permission-session)))
             (target (and session
                          (mevedel-session-execution-target session))))
        (if (and (eq mode 'batch)
                 target
                 (mevedel-execution-target-remote-p target))
            (funcall
             cont
             (mevedel-tool-exec-permission--permission-decision-result
              (plist-get input :permission-decision-metadata)
              (cons 'deny
                    "Batch Eval is unavailable for remote sessions; use Live Eval or Bash on the execution target")
              'eval-policy))
          (let* ((request
                  (mevedel-tool-exec-permission--prepare-additional-authority-request
                   "Eval"
                   (mevedel-tool-exec-permission-effective-sandbox-request
                    input "Eval" (plist-get input :expression) mode
                    permission-context)
                   permission-context
                   (plist-get input :expression)))
                 (sandbox-mode
                  (mevedel-bash-policy-effective-sandbox-mode
                   permission-context))
                 (command-input
                  (mevedel-tool-exec-permission--command-permission-input
                   input request)))
            (if (and (eq (plist-get request :level) 'escalated)
                     (not (eq sandbox-mode 'off)))
                (mevedel-tool-exec-permission--check-full-escalation-async
                 "Eval" (plist-get input :expression) input request cont)
              (mevedel-tool-exec-permission--eval-check-command-permission-async
               tool-struct command-input
               (lambda (outcome)
                 (mevedel-tool-exec-permission--check-additional-permission-async
                  "Eval" (plist-get input :expression) input request outcome
                  cont)))))))
    (error
     (funcall
      cont
      (mevedel-tool-exec-permission--permission-decision-result
       (plist-get input :permission-decision-metadata)
       (cons 'deny (error-message-string err)) 'sandbox-policy)))))

;;

;;; Bash Prompt UI

(defun mevedel-tool-exec-permission--bash-deny-only-guardian-async
    (command cont &optional metadata-p permission-context)
  "Run deny-only guardian review for COMMAND, then call CONT.
METADATA-P controls decision metadata.  PERMISSION-CONTEXT supplies the
pending child-confinement request.  Guardian deny recommendations become
`deny'; unavailable or non-deny guidance allows by default."
  (let ((active t)
        (request (plist-get permission-context :request)))
    (when (mevedel-request-p request)
      (mevedel-request-push-canceller
       request (lambda () (setq active nil))))
    (mevedel-bash-policy-guardian-classify-async
     command
     (mevedel-bash-policy-guardian-context command permission-context)
     (lambda (guardian)
       (when active
         (setq active nil)
         (let ((outcome
                (if (eq (plist-get guardian :recommendation) 'deny)
                    'deny
                  'allow)))
           (when metadata-p
             (mevedel-tool-exec-permission--log-permission-decision
              "Bash" outcome 'bash-guardian permission-context
              :specifier-key :pattern
              :specifier-value
              (mevedel-bash-policy-decision-specifier-value command)))
           (funcall
            cont
            (mevedel-tool-exec-permission--permission-decision-result
             metadata-p outcome 'bash-guardian
             :specifier-key :pattern
             :specifier-value
             (mevedel-bash-policy-decision-specifier-value command)))))))))

(defun mevedel-tool-exec-permission--apply-bash-prompt-result
    (outcome session workspace command allow-patterns)
  "Apply Bash prompt OUTCOME for SESSION, WORKSPACE, and COMMAND.

Session/permanent allow outcomes store ALLOW-PATTERNS as Bash
`:pattern' rules instead of saving COMMAND verbatim.  Deny-session
stays exact to avoid broad negative rules from a single rejection."
  (pcase outcome
    ('allow-once 'allow)
    ((or 'allow-session 'always-allow)
     (dolist (pattern (or allow-patterns (list command)))
       (mevedel-permission--apply-prompt-result
        outcome "Bash" session workspace nil
        :spec-key :pattern
        :spec-value pattern))
     'allow)
    ('deny-once 'deny)
    ('deny-session
     (mevedel-permission--apply-prompt-result
      outcome "Bash" session workspace nil
      :spec-key :pattern
      :spec-value command)
     'deny)
    (_ 'deny)))

(defun mevedel-tool-exec-permission--bash-prompt-result
    (outcome session workspace command allow-patterns metadata-p)
  "Apply Bash prompt OUTCOME and return its canonical permission result."
  (let ((specifier
         (mevedel-bash-policy-decision-specifier-value command))
        (result
         (mevedel-tool-exec-permission--normalize-prompt-result
          outcome
          (lambda (stored-outcome)
            (condition-case err
                (mevedel-tool-exec-permission--apply-bash-prompt-result
                 stored-outcome session workspace command allow-patterns)
              (error
               (format "Error: Bash rule write failed: %S" err))))
          "Command cancelled by user. Feedback: ")))
    (mevedel-tool-exec-permission--permission-decision-result
     metadata-p result 'bash-classifier
     :specifier-key :pattern
     :specifier-value specifier)))

(defun mevedel-tool-exec-permission--check-command-permission-async
    (_tool-struct input cont)
  "Async permission check for Bash tool INPUT.

Pattern matching first: when `mevedel-bash-policy-check-permission'
yields a final decision the slot returns it directly.  Trust-literal
shell-expansion path also returns directly (no prompt).  When the
classifier yields `ask' the request enters the session permission
queue; the queue's render-head dispatches to the Bash-specific
overlay via `mevedel-permission-queue--render-bash' when the
entry becomes the head.  CONT receives the same slot vocabulary
as before: `allow' / `deny' / `(deny . REASON)' / `aborted'.
Feedback is shaped into the existing
\"Command cancelled by user. Feedback: TEXT\" form for LLM-visible
parity with the sync slot."
  (let ((command (plist-get input :command))
        (trust-literal-p (plist-get input :trust-literal-p))
        (permission-context (plist-get input :permission-context))
        (metadata-p (plist-get input :permission-decision-metadata)))
    (if (null command)
        (funcall cont nil)
      (let ((decision (mevedel-bash-policy-check-permission
                       command :trust-literal-p trust-literal-p
                       :permission-context permission-context)))
        (cond
         ((not (eq decision 'ask))
          (if (and (eq decision 'allow)
                   (mevedel-bash-policy-full-auto-guardian-needed-p
                    command permission-context))
              (mevedel-tool-exec-permission--bash-deny-only-guardian-async
               command cont metadata-p permission-context)
            (when metadata-p
              (mevedel-tool-exec-permission--log-permission-decision
               "Bash" decision 'bash-classifier permission-context
               :specifier-key :pattern
               :specifier-value (mevedel-bash-policy-decision-specifier-value
                                 command)))
            (funcall
             cont
             (mevedel-tool-exec-permission--permission-decision-result
              metadata-p decision 'bash-classifier
              :specifier-key :pattern
              :specifier-value (mevedel-bash-policy-decision-specifier-value
                                command)))))
         (trust-literal-p
          (let ((outcome
                 (cons 'deny
                       "Shell expansion requires a pre-approved Bash rule; no prompt is shown while preparing skill bodies.")))
            (when metadata-p
              (mevedel-tool-exec-permission--log-permission-decision
               "Bash" outcome 'bash-classifier permission-context
               :specifier-key :pattern
               :specifier-value (mevedel-bash-policy-decision-specifier-value
                                 command)))
            (funcall
             cont
             (mevedel-tool-exec-permission--permission-decision-result
              metadata-p outcome 'bash-classifier
              :specifier-key :pattern
              :specifier-value (mevedel-bash-policy-decision-specifier-value
                                command)))))
         (t
          (let* ((source-buffer (current-buffer))
                 (session (or (plist-get permission-context :session)
                              (and (boundp 'mevedel--session)
                                   mevedel--session)
                              (mevedel-permission-queue--current-session)))
                 (guardian-pending t)
                 (workspace (or (plist-get permission-context :workspace)
                                (and session
                                     (mevedel-session-workspace session))))
                 (guardian-context
                  (and mevedel-permission-guardian
                       (mevedel-bash-policy-guardian-context
                        command permission-context)))
                 (analysis
                  (or (plist-get guardian-context :analysis)
                      (mevedel-bash-analysis-analyze command)))
                 (command-class (plist-get analysis :class))
                 (commands (mevedel-bash-policy-command-names analysis))
                 (commands-summary
                  (mevedel-bash-policy-commands-summary commands))
                 (unparseable (eq command-class 'complex))
                 (allow-patterns
                  (or (plist-get guardian-context :allow-patterns)
                      (mevedel-bash-policy-allow-patterns command)))
                 (rule-creating-p
                  (mevedel-bash-policy-reusable-operation-p command))
                 (guardian-cell
                  (list nil (and mevedel-permission-guardian 'pending)))
                 (entry
                  (list :kind 'bash
                        :command command
                        :mutation-p (not (eq command-class 'read-only))
                        :specifier-key :pattern
                        :specifier-value command
                        :analysis analysis
                        :command-class command-class
                        :commands commands
                        :commands-summary commands-summary
                        :unparseable unparseable
                        :allow-patterns allow-patterns
                        :reusable-operation-p rule-creating-p
                        :guardian-cell guardian-cell
                        :workspace workspace
                        :include-always (and rule-creating-p
                                             (not (null workspace)))
                        :origin
                        (mevedel-tool-exec-permission--permission-origin
                         permission-context)
                        :callback
                        (lambda (outcome)
                          (setq guardian-pending nil)
                          (funcall
                           cont
                           (mevedel-tool-exec-permission--bash-prompt-result
                            outcome session workspace command allow-patterns
                            metadata-p))))))
            (when metadata-p
              (mevedel-tool-exec-permission--log-permission-decision
               "Bash" 'ask 'bash-classifier permission-context
               :specifier-key :pattern
               :specifier-value (mevedel-bash-policy-decision-specifier-value
                                 command)))
            (if (buffer-live-p source-buffer)
                (with-current-buffer source-buffer
                  (mevedel-tool-exec-permission--request-permission
                   entry permission-context session))
              (mevedel-tool-exec-permission--request-permission
               entry permission-context session))
            (when mevedel-permission-guardian
              (setq guardian-context
                    (plist-put guardian-context :workspace workspace))
              (mevedel-bash-policy-guardian-classify-async
               command guardian-context
               (lambda (guardian)
                 (when guardian-pending
                   (let ((was-pending (eq (cadr guardian-cell) 'pending)))
                     (setcar guardian-cell guardian)
                     (when was-pending
                       (setcar (cdr guardian-cell)
                               (if guardian 'done 'unavailable)))
                     (when (or guardian was-pending)
                       ;; Replace the pending placeholder in-place with
                       ;; either guidance or an unavailable note.
                       (when (buffer-live-p source-buffer)
                         (with-current-buffer source-buffer
                           (mevedel-permission-queue--render-head
                            session))))))))))))))))

(defun mevedel-tool-exec-permission-check-bash-async
    (tool-struct input cont)
  "Authorize Bash INPUT, then layer any requested child authority.
TOOL-STRUCT and CONT follow the async permission slot contract."
  (condition-case err
      (let* ((input (mevedel-tool-exec-permission--capture-permission-origin input))
             (request
              (mevedel-tool-exec-permission--prepare-additional-authority-request
               "Bash"
               (mevedel-tool-exec-permission-effective-sandbox-request
                input "Bash" (plist-get input :command) nil
                (plist-get input :permission-context))
               (plist-get input :permission-context)
               (plist-get input :command)))
             (sandbox-mode
              (mevedel-bash-policy-effective-sandbox-mode
               (plist-get input :permission-context)))
             (missing-resources
              (unless (and (eq (plist-get request :level) 'escalated)
                           (not (eq sandbox-mode 'off)))
                (mevedel-bash-policy-missing-resource-paths
                 (plist-get input :command)
                 (plist-get input :permission-context)
                 request)))
             (command-input
              (mevedel-tool-exec-permission--command-permission-input
               input request)))
        (cond
         ((and (eq (plist-get request :level) 'escalated)
               (not (eq sandbox-mode 'off)))
          (mevedel-tool-exec-permission--check-full-escalation-async
           "Bash" (plist-get input :command) input request cont))
         (t
          (mevedel-tool-exec-permission--check-command-permission-async
           tool-struct command-input
           (lambda (outcome)
             (cond
              ((not (mevedel-tool-exec-permission--permission-allow-p outcome))
               (funcall cont outcome))
              (missing-resources
               (funcall
                cont
                (mevedel-tool-exec-permission--permission-decision-result
                 (plist-get input :permission-decision-metadata)
                 (cons
                  'deny
                  (format
                   (concat
                    "Filesystem authority required for Bash resource: %s. "
                    "Retry with sandbox_permissions=\"with_additional_permissions\" "
                    "and additional_permissions.file_system.read containing "
                    "the exact absolute path.")
                   (mapconcat #'identity missing-resources ", ")))
                 'workspace-boundary)))
              (t
               (mevedel-tool-exec-permission--check-additional-permission-async
                "Bash" (plist-get input :command) input request outcome
                cont))))))))
    (error
     (funcall
      cont
      (mevedel-tool-exec-permission--permission-decision-result
       (plist-get input :permission-decision-metadata)
       (cons 'deny (error-message-string err)) 'sandbox-policy)))))

(provide 'mevedel-tool-exec-permission)

;;; mevedel-tool-exec-permission.el ends here
