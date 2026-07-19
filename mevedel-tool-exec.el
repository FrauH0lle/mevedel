;;; mevedel-tool-exec.el --- Bash and Eval tool adapters -*- lexical-binding: t -*-

;;; Commentary:

;; Bash and Eval schemas, permissions, validation, result formatting, and
;; rendering.  `mevedel-execution' owns their operating-system children.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'mevedel-tool-registry))

(require 'subr-x)
(require 'mevedel-permission-log)

;; `cl-extra'
(declare-function cl-some "cl-extra" (cl-pred cl-seq &rest cl-rest))

;; `cl-seq'
(declare-function cl-count "cl-seq" (cl-item cl-seq &rest cl-keys))

;; `gptel'
(declare-function gptel-request "ext:gptel-request" (&optional prompt &rest args))
(defvar gptel-backend)
(defvar gptel-model)
(defvar gptel-reasoning-effort)
(defvar gptel-stream)
(defvar gptel-tools)
(defvar gptel-use-context)
(defvar gptel-use-tools)
(defvar read-eval)

;; `mevedel-agent-control'
(declare-function mevedel-agent-control-enqueue-execution-result
                  "mevedel-agent-control" (session owner body))

;; `mevedel-agent-runtime'
(declare-function mevedel-agent-runtime-queue-execution-completion
                  "mevedel-agent-runtime"
                  (context owner body))

;; `mevedel-agents'
(declare-function mevedel-agent-invocation-skill-permission-rules
                  "mevedel-agents" (cl-x) t)
(defvar mevedel--agent-invocation)

;; `mevedel-bash-analysis'
(declare-function mevedel-bash-analysis-analyze
                  "mevedel-bash-analysis" (source))
(defvar mevedel-bash-dangerous-commands)

;; `mevedel-execution'
(declare-function mevedel-execution-list
                  "mevedel-execution" (session owner))
(declare-function mevedel-execution-observe
                  "mevedel-execution"
                  (session owner execution-id callback &rest keys))
(declare-function mevedel-execution-start-bash
                  "mevedel-execution" (callback &rest keys))
(declare-function mevedel-execution-start-one-shot
                  "mevedel-execution" (callback &rest keys))
(declare-function mevedel-execution-stop
                  "mevedel-execution"
                  (session owner execution-id callback))

;; `mevedel-interaction-prompt'
(declare-function mevedel--prompt-attribution-line
                  "mevedel-interaction-prompt" (origin))

(autoload 'mevedel--prompt-attribution-line "mevedel-interaction-prompt")

;; `mevedel-models'
(declare-function mevedel-model-resolve-workload
                  "mevedel-models"
                  (workload &optional explicit-selector explicit-effort))

;; `mevedel-permission-prompt'
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

;; `mevedel-permissions'
(declare-function mevedel-permission--apply-prompt-result
                  "mevedel-permissions" t t)
(declare-function mevedel-permission--bucket-decision
                  "mevedel-permissions"
                  (buckets tool-name path pattern domain name))
(declare-function mevedel-permission--collect-buckets
                  "mevedel-permissions"
                  (invocation-rules request-rules
                                    session-rules persistent-rules))
(declare-function mevedel-permission--execution-level-buckets
                  "mevedel-permissions" (buckets level))
(declare-function mevedel-permission--execution-level-decision
                  "mevedel-permissions"
                  (buckets tool-name level pattern))
(declare-function mevedel-permission--find-rules
                  "mevedel-permissions"
                  (rules tool-name &rest keys))
(declare-function mevedel-permission--first-non-nil-action-with-bucket
                  "mevedel-permissions"
                  (buckets tool-name path pattern domain name))
(declare-function mevedel-permission--load-persistent-rules "mevedel-permissions"
                  (workspace))
(declare-function mevedel-permission--normalize-outcome
                  "mevedel-permissions" (outcome))
(declare-function mevedel-permission--path-in-allowed-roots-p
                  "mevedel-permissions" (path roots))
(declare-function mevedel-permission--path-protected-p
                  "mevedel-permissions" (path))
(declare-function mevedel-permission--resource-granted-p
                  "mevedel-permissions" (path access grants))
(declare-function mevedel-permission--rules-action "mevedel-permissions"
                  (rules tool-name &rest keys))
(declare-function mevedel-permission-protected-path-policy
                  "mevedel-permissions" ())
(defvar mevedel-permission-mode)
(defvar mevedel-permission-rules)

;; `mevedel-pipeline'
(declare-function mevedel-pipeline--tool-results-dir
                  "mevedel-pipeline" (session buffer))
(declare-function mevedel-pipeline-active-tool-use-id
                  "mevedel-pipeline" ())

;; `mevedel-sandbox'
(declare-function mevedel-sandbox-pending-facts
                  "mevedel-sandbox"
                  (&optional additional-permissions sandbox-permissions))
(declare-function mevedel-sandbox-status-text "mevedel-sandbox" (facts))
(defvar mevedel-sandbox-intrinsic-paths)

;; `mevedel-structs'
(declare-function mevedel-current-origin "mevedel-structs" ())
(declare-function mevedel-request-p "mevedel-structs" (cl-x))
(declare-function mevedel-request-push-canceller
                  "mevedel-structs" (request canceller))
(declare-function mevedel-request-skill-permission-rules
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-p "mevedel-structs" (cl-x))
(declare-function mevedel-session-permission-mode "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-permission-rules "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-resource-grants "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-workspace "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-working-directory "mevedel-structs" (cl-x) t)
(declare-function mevedel-workspace-root "mevedel-structs" (cl-x) t)
(defvar mevedel--current-request)
(defvar mevedel--data-buffer)
(defvar mevedel--session)
(defvar mevedel--workspace)

;; `mevedel-system'
(declare-function mevedel-system-render-prompt-file
                  "mevedel-system" (relative-path &optional replacements))

;; `mevedel-view'
(declare-function mevedel-view-collapse-by-height-p "mevedel-view" (body))

;; `mevedel-workspace'
(declare-function mevedel--all-allowed-roots
                  "mevedel-workspace" (&optional buffer))

;; `xml'
(declare-function xml-escape-string "xml" (string))


;;
;;; Permission queue helpers

(defun mevedel-tool-exec--permission-log-session ()
  "Return the session visible to a Bash/Eval permission adapter."
  (or (and (boundp 'mevedel--session) mevedel--session)
      (mevedel-permission-queue--current-session)))

(defun mevedel-tool-exec--capture-permission-origin (input)
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
                       (mevedel-tool-exec--permission-log-session))))
    (plist-put copy :permission-context context)))

(defun mevedel-tool-exec--permission-origin (permission-context)
  "Return the captured owner from PERMISSION-CONTEXT."
  (or (plist-get permission-context :origin)
      (mevedel-current-origin)))

(defun mevedel-tool-exec--permission-decision-result
    (metadata-p outcome via &rest props)
  "Return OUTCOME, or metadata when METADATA-P is non-nil."
  (if metadata-p
      (append (list :outcome (mevedel-permission--normalize-outcome outcome)
                    :raw-outcome outcome
                    :via via
                    :logged t)
              props)
    outcome))

(defun mevedel-tool-exec--log-permission-decision
    (tool-name outcome via permission-context &rest props)
  "Persist TOOL-NAME OUTCOME via VIA from PERMISSION-CONTEXT and PROPS."
  (when-let* ((session
               (or (plist-get permission-context :session)
                   (mevedel-tool-exec--permission-log-session))))
    (apply #'mevedel-permission-log
           session 'permission-decision
           (append (list :tool-name tool-name
                         :origin
                         (mevedel-tool-exec--permission-origin
                          permission-context)
                         :mode
                         (mevedel-tool-exec--effective-permission-mode
                          permission-context)
                         :outcome (mevedel-permission--normalize-outcome outcome)
                         :via via)
                   props))))

(defun mevedel-tool-exec--validate-additional-plist (value allowed label)
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

(defun mevedel-tool-exec--filesystem-paths (value access)
  "Return exact filesystem permission entries from VALUE at ACCESS."
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
       (list :path (expand-file-name path) :access access))
     paths)))

(defun mevedel-tool-exec--normalize-additional-permissions (additional)
  "Return validated ADDITIONAL permissions, or nil when none are requested."
  (mevedel-tool-exec--validate-additional-plist
   additional '(:network :file_system) "Additional permissions")
  (let ((network (plist-get additional :network))
        (file-system (plist-get additional :file_system))
        grants profile)
    (unless (memq network '(nil t :json-false))
      (error "Network permission must be true or false"))
    (when (and file-system (not (eq file-system :json-false)))
      (mevedel-tool-exec--validate-additional-plist
       file-system '(:read :write) "Filesystem permissions")
      (let ((reads (mevedel-tool-exec--filesystem-paths
                    (plist-get file-system :read) 'read))
            (writes (mevedel-tool-exec--filesystem-paths
                     (plist-get file-system :write) 'write)))
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

(defun mevedel-tool-exec--sandbox-request
    (args tool &optional eval-mode)
  "Return the validated child-execution request from ARGS.
TOOL is `bash' or `eval'.  EVAL-MODE distinguishes live from batch Eval."
  (let* ((raw-level (plist-get args :sandbox_permissions))
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
               (mevedel-tool-exec--normalize-additional-permissions
                additional)))
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

(defun mevedel-tool-exec--permission-allow-p (outcome)
  "Return non-nil when permission OUTCOME authorizes execution."
  (eq 'allow
      (if (and (consp outcome)
               (keywordp (car outcome))
               (plist-member outcome :outcome))
          (plist-get outcome :outcome)
        outcome)))

(defun mevedel-tool-exec--additional-denial
    (metadata-p via &optional feedback)
  "Return an additional-authority denial through VIA for METADATA-P."
  (mevedel-tool-exec--permission-decision-result
   metadata-p
   (if feedback
       (cons 'deny
             (format "Additional permission denied. Feedback: %s" feedback))
     'deny)
   via))

(defun mevedel-tool-exec--check-network-permission-async
    (tool-name detail request command-outcome permission-context metadata-p cont)
  "Check REQUEST's network authority, then call CONT with COMMAND-OUTCOME."
  (if (not (eq t (plist-get (plist-get request :additional-permissions)
                            :network)))
      (funcall cont command-outcome)
    (if (eq (mevedel-tool-exec--effective-permission-mode permission-context)
            'full-auto)
        (progn
          (when metadata-p
            (mevedel-tool-exec--log-permission-decision
             tool-name 'allow 'sandbox-network permission-context
             :sandbox-permissions 'additive
             :additional-permissions '(:network t)))
          (funcall cont command-outcome))
      (apply
       #'mevedel-permission--enqueue
       (list
        :kind 'sandbox
        :tool-name tool-name
        :detail detail
        :sandbox-permissions 'additive
        :additional-permissions '(:network t)
        :justification (plist-get request :justification)
        :origin (mevedel-tool-exec--permission-origin permission-context)
        :callback
        (lambda (outcome)
          (pcase outcome
            ('allow-once (funcall cont command-outcome))
            (`(feedback . ,text)
             (funcall cont (mevedel-tool-exec--additional-denial
                            metadata-p 'sandbox-network text)))
            ('aborted
             (funcall
              cont
              (mevedel-tool-exec--permission-decision-result
               metadata-p 'aborted 'sandbox-network)))
            (_ (funcall cont (mevedel-tool-exec--additional-denial
                              metadata-p 'sandbox-network))))))
       (and (plist-get permission-context :session)
            (list (plist-get permission-context :session)))))))

(defun mevedel-tool-exec--filesystem-resource-granted-p
    (grant permission-context)
  "Return non-nil when PERMISSION-CONTEXT already authorizes GRANT."
  (let* ((session (plist-get permission-context :session))
         (grants
          (append (plist-get permission-context :resource-grants)
                  (and session (mevedel-session-resource-grants session)))))
    (mevedel-permission--resource-granted-p
     (plist-get grant :path) (plist-get grant :access) grants)))

(defun mevedel-tool-exec--filesystem-resource-rule-action
    (tool-name grant permission-context)
  "Return the authoritative `deny' or `ask' rule for TOOL-NAME's GRANT."
  (let ((buckets (mevedel-tools--bash-buckets permission-context))
        (path (plist-get grant :path)))
    (let ((action (mevedel-permission--bucket-decision
                   buckets tool-name path nil nil nil)))
      (and (memq action '(deny ask)) action))))

(defun mevedel-tool-exec--check-filesystem-permissions-async
    (tool-name detail grants request command-outcome permission-context
               metadata-p cont)
  "Authorize exact filesystem GRANTS, then call CONT with COMMAND-OUTCOME."
  (if (not grants)
      (funcall cont command-outcome)
    (let* ((grant (car grants))
           (path (plist-get grant :path))
           (access (plist-get grant :access))
           (rule-action
            (mevedel-tool-exec--filesystem-resource-rule-action
             tool-name grant permission-context))
           (session (plist-get permission-context :session))
           (workspace (or (plist-get permission-context :workspace)
                          (and session (mevedel-session-workspace session))))
           (continue
            (lambda ()
              (when metadata-p
                (mevedel-tool-exec--log-permission-decision
                 tool-name 'allow 'sandbox-filesystem permission-context
                 :sandbox-permissions 'additive
                 :specifier-key :path :specifier-value path
                 :resource-access access))
              (mevedel-tool-exec--check-filesystem-permissions-async
               tool-name detail (cdr grants) request command-outcome
               permission-context metadata-p cont))))
      (cond
       ((eq rule-action 'deny)
        (funcall cont (mevedel-tool-exec--additional-denial
                       metadata-p 'sandbox-filesystem)))
       ((and (not (eq rule-action 'ask))
             (mevedel-tool-exec--filesystem-resource-granted-p
              grant permission-context))
        (funcall continue))
       (t
        (apply
         #'mevedel-permission--enqueue
         (list
          :kind 'sandbox
          :tool-name tool-name
          :detail detail
          :sandbox-permissions 'additive
          :additional-permissions (list :file-system (list grant))
          :justification (plist-get request :justification)
          :specifier-key :path
          :specifier-value path
          :resource-path path
          :resource-access access
          :include-always (not (null workspace))
          :workspace workspace
          :origin (mevedel-tool-exec--permission-origin permission-context)
          :callback
          (lambda (outcome)
            (pcase outcome
              ((or 'allow 'allow-once 'allow-session 'always-allow)
               (when (memq outcome '(allow-session always-allow))
                 (mevedel-permission--apply-prompt-result
                  outcome tool-name session workspace path
                  :spec-key :path :spec-value path
                  :resource-access access))
               (funcall continue))
              ('deny-session
               (mevedel-permission--apply-prompt-result
                outcome tool-name session workspace path
                :spec-key :path :spec-value path
                :resource-access access)
               (funcall cont (mevedel-tool-exec--additional-denial
                              metadata-p 'sandbox-filesystem)))
              (`(feedback . ,text)
               (funcall cont (mevedel-tool-exec--additional-denial
                              metadata-p 'sandbox-filesystem text)))
              ('aborted
               (funcall
                cont
                (mevedel-tool-exec--permission-decision-result
                 metadata-p 'aborted 'sandbox-filesystem)))
              (_ (funcall cont (mevedel-tool-exec--additional-denial
                                metadata-p 'sandbox-filesystem))))))
         (and session (list session))))))))

(defun mevedel-tool-exec--check-additional-permission-async
    (tool-name detail input request command-outcome cont)
  "Layer REQUEST authority for TOOL-NAME and DETAIL over COMMAND-OUTCOME.
INPUT supplies permission context and delegated trust.  Call CONT once."
  (if (or (not (mevedel-tool-exec--permission-allow-p command-outcome))
          (eq (plist-get request :level) 'use-default))
      (funcall cont command-outcome)
    (let* ((permission-context (plist-get input :permission-context))
           (metadata-p (plist-get input :permission-decision-metadata))
           (trust-literal-p (plist-get input :trust-literal-p))
           (profile (plist-get request :additional-permissions)))
      (if trust-literal-p
          (funcall
           cont
           (mevedel-tool-exec--permission-decision-result
            metadata-p
            (cons 'deny
                  "Delegated expansion cannot request additional sandbox authority")
            'sandbox-policy))
        (mevedel-tool-exec--check-network-permission-async
         tool-name detail request command-outcome permission-context metadata-p
         (lambda (network-outcome)
           (if (not (mevedel-tool-exec--permission-allow-p network-outcome))
               (funcall cont network-outcome)
             (mevedel-tool-exec--check-filesystem-permissions-async
              tool-name detail (plist-get profile :file-system) request
             command-outcome permission-context metadata-p cont))))))))

(defun mevedel-tool-exec--full-escalation-explicit-deny-p
    (tool-name detail buckets)
  "Return non-nil when ordinary rules deny TOOL-NAME and DETAIL."
  (if (equal tool-name "Bash")
      (mevedel-tool-exec--bash-explicit-deny-p buckets detail)
    (eq 'deny
        (mevedel-permission--bucket-decision
         buckets tool-name nil detail nil nil))))

(defun mevedel-tool-exec--full-escalation-rule-decision
    (tool-name detail buckets level)
  "Return the full-escalation rule decision for TOOL-NAME and DETAIL.
BUCKETS supplies ordinary and execution-level rules for LEVEL."
  (if (or (mevedel-tool-exec--full-escalation-explicit-deny-p
           tool-name detail buckets)
          (and (equal tool-name "Bash")
               (mevedel-tool-exec--bash-explicit-deny-p
                (mevedel-permission--execution-level-buckets buckets level)
                detail)))
      'deny
    (mevedel-permission--execution-level-decision
     buckets tool-name level detail)))

(defun mevedel-tool-exec--full-escalation-reusable-rule-p
    (tool-name detail)
  "Return non-nil when a prompt may offer reusable authority for DETAIL."
  (and (equal tool-name "Bash")
       (not (memq (plist-get (mevedel-tool-exec--analyze-bash detail) :class)
                  '(dangerous complex)))
       (not (string-match-p "\\(?:\\*\\|\\?\\|\\[\\)" detail))))

(defun mevedel-tool-exec--full-escalation-denial
    (metadata-p &optional feedback)
  "Return a full-escalation denial for METADATA-P and FEEDBACK."
  (mevedel-tool-exec--permission-decision-result
   metadata-p
   (if feedback
       (cons 'deny
             (format "Full execution escalation denied. Feedback: %s"
                     feedback))
     'deny)
   'sandbox-full-escalation
   :sandbox-permissions 'require-escalated))

(defun mevedel-tool-exec--apply-full-escalation-prompt-result
    (outcome tool-name detail level session workspace metadata-p)
  "Apply full-escalation prompt OUTCOME and return its permission result."
  (pcase outcome
    ('allow-once
     (mevedel-tool-exec--permission-decision-result
      metadata-p 'allow 'sandbox-full-escalation
      :sandbox-permissions level))
    ((or 'allow-session 'always-allow)
     (mevedel-permission--apply-prompt-result
      outcome tool-name session workspace nil
      :spec-key :pattern :spec-value detail
      :sandbox-permissions level)
     (mevedel-tool-exec--permission-decision-result
      metadata-p 'allow 'sandbox-full-escalation
      :sandbox-permissions level))
    ('deny-session
     (mevedel-permission--apply-prompt-result
      outcome tool-name session workspace nil
      :spec-key :pattern :spec-value detail
      :sandbox-permissions level)
     (mevedel-tool-exec--full-escalation-denial metadata-p))
    (`(feedback . ,text)
     (mevedel-tool-exec--full-escalation-denial metadata-p text))
    ('aborted
     (mevedel-tool-exec--permission-decision-result
      metadata-p 'aborted 'sandbox-full-escalation
      :sandbox-permissions level))
    (_ (mevedel-tool-exec--full-escalation-denial metadata-p))))

(defun mevedel-tool-exec--check-full-escalation-async
    (tool-name detail input request cont)
  "Authorize REQUEST to run TOOL-NAME and DETAIL without confinement.
Only direct, user-authored rules qualified with `require-escalated' may skip
the prompt.  Delegated expansion never prompts for or grants this authority."
  (let* ((permission-context (plist-get input :permission-context))
         (metadata-p (plist-get input :permission-decision-metadata))
         (trust-literal-p (plist-get input :trust-literal-p))
         (buckets (mevedel-tools--bash-buckets permission-context))
         (level (plist-get request :sandbox-permissions))
         (decision
          (mevedel-tool-exec--full-escalation-rule-decision
           tool-name detail buckets level))
         (session (or (plist-get permission-context :session)
                      (and (boundp 'mevedel--session) mevedel--session)))
         (workspace (or (plist-get permission-context :workspace)
                        (and session (mevedel-session-workspace session)))))
    (cond
     ((eq decision 'deny)
      (when metadata-p
        (mevedel-tool-exec--log-permission-decision
         tool-name 'deny 'sandbox-full-escalation permission-context
         :sandbox-permissions level
         :specifier-key :pattern :specifier-value detail))
      (funcall cont
               (mevedel-tool-exec--full-escalation-denial metadata-p)))
     (trust-literal-p
      (funcall
       cont
       (mevedel-tool-exec--permission-decision-result
        metadata-p
        (cons 'deny
              "Delegated expansion cannot request full execution escalation")
        'sandbox-policy
        :sandbox-permissions level)))
     ((eq decision 'allow)
      (when metadata-p
        (mevedel-tool-exec--log-permission-decision
         tool-name 'allow 'sandbox-full-escalation permission-context
         :sandbox-permissions level
         :specifier-key :pattern :specifier-value detail))
      (funcall
       cont
       (mevedel-tool-exec--permission-decision-result
        metadata-p 'allow 'sandbox-full-escalation
        :sandbox-permissions level
        :specifier-key :pattern :specifier-value detail)))
     (t
      (when metadata-p
        (mevedel-tool-exec--log-permission-decision
         tool-name 'ask 'sandbox-full-escalation permission-context
         :sandbox-permissions level
         :specifier-key :pattern :specifier-value detail))
      (apply
       #'mevedel-permission--enqueue
       (list
        :kind 'sandbox
        :tool-name tool-name
        :detail detail
        :sandbox-permissions level
        :justification (plist-get request :justification)
        :specifier-key :pattern
        :specifier-value detail
        :include-always
        (mevedel-tool-exec--full-escalation-reusable-rule-p
         tool-name detail)
        :workspace workspace
        :origin (mevedel-tool-exec--permission-origin permission-context)
        :callback
        (lambda (outcome)
          (funcall
           cont
           (mevedel-tool-exec--apply-full-escalation-prompt-result
            outcome tool-name detail level session workspace metadata-p))))
       (and session (list session)))))))

(defun mevedel-tool-exec--command-permission-input (input request)
  "Return INPUT carrying REQUEST facts for command authorization."
  (let* ((copy (copy-sequence input))
         (context (copy-sequence (plist-get copy :permission-context))))
    (setq context (plist-put context :sandbox-request request))
    (when (plist-get (plist-get request :additional-permissions) :file-system)
      (setq context
            (plist-put context :resource-authority-separated-p t)))
    (plist-put copy :permission-context context)))

(defun mevedel-tool-exec--analyze-bash (command)
  "Return normalized Bash analysis for COMMAND."
  (require 'mevedel-bash-analysis)
  (mevedel-bash-analysis-analyze command))

(defun mevedel-tool-exec--bash-missing-resource-paths
    (command permission-context request)
  "Return COMMAND resources lacking authority under PERMISSION-CONTEXT.
REQUEST may supply exact additive filesystem grants for this invocation."
  (require 'mevedel-sandbox)
  (let* ((base (mevedel-tool-exec--default-directory))
         (roots (or (plist-get permission-context :allowed-roots)
                    (and-let* ((root (plist-get permission-context
                                                :workspace-root)))
                      (list root))
                    (list base temporary-file-directory)))
         (grants
          (append
           (plist-get permission-context :resource-grants)
           (plist-get (plist-get request :additional-permissions)
                      :file-system)))
         missing)
    (dolist (resource
             (plist-get (mevedel-tool-exec--analyze-bash command) :resources))
      (let ((path (expand-file-name resource base)))
        (unless (or (member path mevedel-sandbox-intrinsic-paths)
                    (mevedel-permission--path-in-allowed-roots-p path roots)
                    (mevedel-permission--resource-granted-p
                     path 'read grants))
          (push path missing))))
    (delete-dups (nreverse missing))))


;;
;;; Command Execution

(defcustom mevedel-bash-timeout 120
  "Maximum seconds a Bash command may run before it is terminated.

The timeout starts once the process is spawned.  Bash tool calls may
override this default with `timeout_seconds'.  Set to nil to disable
Bash command timeouts."
  :type '(choice (integer :tag "Timeout in seconds")
                 (const :tag "Disabled" nil))
  :group 'mevedel)

(defcustom mevedel-permission-guardian nil
  "Whether to annotate Bash permission prompts with risk guidance.

When nil, permission prompts are rendered without guardian guidance.
When t, mevedel asks the current gptel model for advisory-only Bash
risk classification while an `ask' prompt is pending.

A function value is useful for custom classifiers and tests.  It is
called as (FUNCTION COMMAND CONTEXT CALLBACK), where CONTEXT contains
normalized analysis and pending confinement facts, and CALLBACK accepts
either nil or a plist:

  (:risk low|medium|high|critical
   :recommendation proceed|ask|deny
   :reason \"short explanation\")

The result never grants authority or changes deterministic Bash analysis.
Explicit denies, native-edit Goal restrictions, protected-path policy, and
the user's decision remain authoritative."
  :type '(choice (const :tag "Disabled" nil)
                 (const :tag "Use gptel reviewer" t)
                 function)
  :group 'mevedel)

(defcustom mevedel-permission-guardian-timeout 20
  "Seconds to wait before giving up on Bash guardian guidance."
  :type 'number
  :group 'mevedel)

(defconst mevedel-tool-exec--bash-safe-env-vars
  '("GOEXPERIMENT" "GOOS" "GOARCH" "CGO_ENABLED" "GO111MODULE"
    "RUST_BACKTRACE" "RUST_LOG"
    "NODE_ENV"
    "PYTHONUNBUFFERED" "PYTHONDONTWRITEBYTECODE"
    "PYTEST_DISABLE_PLUGIN_AUTOLOAD" "PYTEST_DEBUG"
    "LANG" "LANGUAGE" "LC_ALL" "LC_CTYPE" "LC_TIME" "CHARSET"
    "TERM" "COLORTERM" "NO_COLOR" "FORCE_COLOR" "TZ"
    "LS_COLORS" "LSCOLORS" "GREP_COLOR" "GREP_COLORS" "GCC_COLORS"
    "TIME_STYLE" "BLOCK_SIZE" "BLOCKSIZE")
  "Environment variables safe to skip before suggesting Bash prefix rules.")

(defconst mevedel-tool-exec--bash-never-prefix-commands
  '("sh" "bash" "zsh" "fish" "csh" "tcsh" "ksh" "dash"
    "env" "xargs"
    "nice" "stdbuf" "nohup" "timeout" "time"
    "doas" "pkexec" "su")
  "Shells and wrappers that must not be generalized to prefix rules.")


(defun mevedel-tool-exec--dedupe-strings (strings)
  "Return STRINGS without duplicates, preserving first occurrence order."
  (let (seen result)
    (dolist (s strings)
      (when (and (stringp s)
                 (not (string-empty-p s))
                 (not (member s seen)))
        (push s seen)
        (push s result)))
    (nreverse result)))

(defun mevedel-tool-exec--bash-commands-summary (commands)
  "Return a counted, first-seen summary string for COMMANDS."
  (when-let* ((unique (mevedel-tool-exec--dedupe-strings commands)))
    (string-join
     (mapcar
      (lambda (command)
        (let ((count (cl-count command commands :test #'equal)))
          (if (> count 1)
              (format "%s (%d)" command count)
            command)))
      unique)
     ", ")))

(defun mevedel-tool-exec--bash-decision-specifier-value (command)
  "Return sanitized Bash specifier metadata for COMMAND."
  (or (mevedel-tool-exec--bash-commands-summary
       (mevedel-tool-exec--bash-command-names
        (mevedel-tool-exec--analyze-bash command)))
      "unparseable shell command"))

(defun mevedel-tool-exec--bash-command-names (analysis)
  "Return executable names from normalized Bash ANALYSIS."
  (mapcar (lambda (argv) (file-name-nondirectory (car argv)))
          (plist-get analysis :commands)))

(defun mevedel-tool-exec--bash-segment-words (segment)
  "Return shell words parsed from SEGMENT, or nil when parsing fails."
  (condition-case nil
      (split-string-and-unquote segment)
    (error nil)))

(defun mevedel-tool-exec--bash-env-assignment-p (word)
  "Return non-nil when WORD is a leading shell env assignment."
  (and (stringp word)
       (string-match-p "\\`[A-Za-z_][A-Za-z0-9_]*=" word)))

(defun mevedel-tool-exec--bash-safe-env-assignment-p (word)
  "Return non-nil when WORD is a safe env assignment for prefix matching."
  (and (mevedel-tool-exec--bash-env-assignment-p word)
       (member (car (split-string word "=" t))
               mevedel-tool-exec--bash-safe-env-vars)))

(defun mevedel-tool-exec--bash-strip-safe-env-assignments (words)
  "Strip safe leading env assignments from WORDS.

Return nil if a leading env assignment is not known safe.  This
avoids saving prefix rules that will not match later permission
checks, and avoids hiding environment-controlled behavior behind a
general rule."
  (catch 'unsafe
    (while (and words
                (mevedel-tool-exec--bash-env-assignment-p (car words)))
      (unless (mevedel-tool-exec--bash-safe-env-assignment-p (car words))
        (throw 'unsafe nil))
      (setq words (cdr words)))
    words))

(defun mevedel-tool-exec--bash-subcommand-token-p (word)
  "Return non-nil for stable shell subcommand WORD."
  (and (stringp word)
       (string-match-p
        "\\`[[:lower:]][[:lower:][:digit:]]*\\(?:-[[:lower:][:digit:]]+\\)*\\'"
        word)))

(defun mevedel-tool-exec--bash-command-never-prefix-p (command)
  "Return non-nil when COMMAND should not get a broad prefix rule."
  (member command
          (append mevedel-bash-dangerous-commands
                  mevedel-tool-exec--bash-never-prefix-commands)))

(defun mevedel-tool-exec--bash-prefix-for-segment (segment)
  "Return a stable command prefix for Bash SEGMENT, or nil.

The heuristic follows Claude Code's low-maintenance shape: derive
`command subcommand' generically, only when the second token looks
like a subcommand rather than a flag, path, file name, or number.
Dangerous commands and shell/wrapper commands are not generalized."
  (let* ((words (mevedel-tool-exec--bash-segment-words segment))
         (words (and words
                     (mevedel-tool-exec--bash-strip-safe-env-assignments
                      words)))
         (command (car words))
         (subcommand (cadr words)))
    (when (and command
               subcommand
               (not (mevedel-tool-exec--bash-command-never-prefix-p command))
               (mevedel-tool-exec--bash-subcommand-token-p subcommand))
      (string-join (list command subcommand) " "))))

(defun mevedel-tool-exec--bash-allow-pattern-for-segment (segment)
  "Return the reusable allow pattern suggested for Bash SEGMENT.

Simple `command subcommand ...' invocations are generalized to
Claude Code-style prefix rules such as `git log:*'.  Segments that
do not have a stable subcommand, or that start with a dangerous
command/wrapper, stay exact."
  (let* ((trimmed (string-trim segment))
         (prefix (mevedel-tool-exec--bash-prefix-for-segment trimmed)))
    (if prefix
        (concat prefix ":*")
      trimmed)))

(defun mevedel-tool-exec--bash-allow-patterns (command)
  "Return reusable allow patterns to store when approving COMMAND.

Compound commands produce one pattern per command segment.  This
avoids saving a brittle whole-chain string such as
`pwd && git log --oneline' when the useful reusable rule is
`git log:*'."
  (mevedel-tool-exec--dedupe-strings
   (mapcar #'mevedel-tool-exec--bash-allow-pattern-for-segment
           (plist-get (mevedel-tool-exec--analyze-bash command) :segments))))

(defun mevedel-tool-exec--effective-permission-mode
    (&optional permission-context)
  "Return effective permission mode for PERMISSION-CONTEXT."
  (let ((session (and (boundp 'mevedel--session) mevedel--session)))
    (or (and permission-context
             (plist-get permission-context :mode))
        (and session (mevedel-session-permission-mode session))
        mevedel-permission-mode)))

(defun mevedel-tool-exec--bash-literal-path-tokens (command &optional analysis)
  "Return literal path resources identified in COMMAND.
Dynamic expansions remain complex and are not evaluated.  Reuse ANALYSIS when
the caller already analyzed COMMAND."
  (plist-get (or analysis (mevedel-tool-exec--analyze-bash command))
             :resources))

(defun mevedel-tool-exec--bash-protected-path-p (command &optional analysis)
  "Return non-nil if COMMAND has an obvious protected path in ANALYSIS."
  (cl-some
   (lambda (path)
     (or (mevedel-permission--path-protected-p path)
         ;; Directory roots such as `.git' may be protected by a
         ;; `**/.git/**' policy even when the literal token has no child.
         (mevedel-permission--path-protected-p
          (file-name-as-directory path))
         (cl-some
          (lambda (name)
            (and (cl-some (lambda (pattern)
                            (string-match-p
                             (concat "\\." (regexp-quote name)
                                     "\\(?:/\\|\\'\\)")
                             pattern))
                          (mapcar #'car
                                  (mevedel-permission-protected-path-policy)))
                 (string-match-p
                  (concat "\\(?:\\`\\|/\\)\\." (regexp-quote name)
                          "\\(?:/\\|\\'\\)")
                  path)))
          '("git" "ssh" "gnupg"))))
   (mevedel-tool-exec--bash-literal-path-tokens command analysis)))

(defun mevedel-tool-exec--bash-deny-candidates (command &optional analysis)
  "Return Bash strings explicit deny rules should check for COMMAND.
Includes the whole command, recognized command-chain segments, and extracted
command names.  Dangerous-name harvesting remains independent, so unsupported
syntax cannot hide a command in `mevedel-bash-dangerous-commands'.  Reuse
ANALYSIS when supplied."
  (let ((analysis (or analysis (mevedel-tool-exec--analyze-bash command))))
    (mevedel-tool-exec--dedupe-strings
     (append (list command)
             (plist-get analysis :candidates)
             (mevedel-tool-exec--bash-command-names analysis)))))

(defun mevedel-tool-exec--bash-deny-match-p
    (buckets candidates &optional pattern-only-p)
  "Return non-nil when BUCKETS deny one of CANDIDATES.
When PATTERN-ONLY-P is non-nil, ignore generic fallback rules."
  (cl-some
   (lambda (candidate)
     (cl-some
      (lambda (entry)
        (let ((rules (if pattern-only-p
                         (seq-filter
                          (lambda (rule)
                            (plist-member (cdr rule) :pattern))
                          (cdr entry))
                       (cdr entry))))
          (eq (mevedel-permission--rules-action
               rules "Bash" :pattern candidate)
              'deny)))
      buckets))
   candidates))

(defun mevedel-tool-exec--bash-explicit-deny-p
    (buckets command &optional analysis)
  "Return non-nil when an effective Bash deny covers COMMAND.
Generic fallback denies are evaluated against the original command and its
recognized top-level segments.  Harvested nested candidates use only pattern
rules, so a generic fallback cannot defeat a specific allow for the containing
command.  ANALYSIS is the normalized result for COMMAND when already known."
  (let* ((analysis (or analysis (mevedel-tool-exec--analyze-bash command)))
         (top-level (cons command (plist-get analysis :segments)))
         (harvested (mevedel-tool-exec--bash-deny-candidates command analysis)))
    (or (mevedel-tool-exec--bash-deny-match-p buckets top-level)
        (mevedel-tool-exec--bash-deny-match-p buckets harvested t))))

(defun mevedel-tools--bash-buckets (&optional permission-context)
  "Return Bash buckets for PERMISSION-CONTEXT, innermost-first.

Includes the request-scoped skill rule buckets so a skill's
`allowed-tools: [Bash(...)]' grants are honored by the Bash
permission check; without this, skill rules silently failed for
the Bash tool path because Bash had its own flattened resolver."
  (or (and permission-context (plist-get permission-context :buckets))
      (let* ((session (and (boundp 'mevedel--session) mevedel--session))
             (workspace (cond
                         (session (mevedel-session-workspace session))
                         ((and (boundp 'mevedel--workspace)
                               mevedel--workspace))))
             (request (and (boundp 'mevedel--current-request)
                           mevedel--current-request))
             (invocation (and (boundp 'mevedel--agent-invocation)
                              mevedel--agent-invocation))
             (invocation-rules
              (and invocation
                   (mevedel-agent-invocation-skill-permission-rules
                    invocation)))
             (request-rules
              (and request
                   (mevedel-request-skill-permission-rules request)))
             (session-rules (when session
                              (mevedel-session-permission-rules session)))
             (persistent (when workspace
                           (mevedel-permission--load-persistent-rules
                            workspace))))
        (mevedel-permission--collect-buckets
         invocation-rules request-rules session-rules persistent))))

(defun mevedel-tool-exec--bash-bucket-match (buckets command)
  "Return the first non-deny (ACTION . BUCKET) matching COMMAND in BUCKETS."
  (mevedel-permission--first-non-nil-action-with-bucket
   buckets "Bash" nil command nil nil))

(defun mevedel-tool-exec--bash-direct-match (buckets command)
  "Return direct user authority matching COMMAND in BUCKETS."
  (mevedel-tool-exec--bash-bucket-match
   (seq-filter
    (lambda (entry) (memq (car entry) '(:session :persistent :defcustom)))
    buckets)
   command))


(cl-defun mevedel-tools--check-bash-permission
    (command &key trust-literal-p ignore-effective-trust-p
             permission-context)
  "Decide Bash permission for COMMAND and PERMISSION-CONTEXT.

Rules come from invocation, request, session, persistent, and
defcustom buckets (in that innermost-first order) and are
matched via `:pattern'.

Normalized Bash analysis supplies read-only, dangerous, complex, or unknown
classification.  Read-only commands run without a matching rule.  Unknown
commands need matching authority.  Dangerous and complex commands require
direct user authority rather than invocation- or request-delegated rules.
TRUST-LITERAL-P identifies a delegated skill-body call and grants no extra
authority over dangerous or complex syntax.

In `full-auto' mode, explicit deny rules and protected path tokens still
win, then unknown, dangerous, and complex Bash invocations are allowed.
When IGNORE-EFFECTIVE-TRUST-P is non-nil, `full-auto' is ignored; this
is used by the guardian to decide whether a command would have been
suspicious under the normal classifier.

Bucket-aware: delegated invocation and request rules may authorize ordinary
unknown commands, but only session, persistent, and global user rules may
authorize dangerous or complex syntax."
  (ignore trust-literal-p)
  (let* ((analysis (mevedel-tool-exec--analyze-bash command))
         (class (plist-get analysis :class))
         (segments (plist-get analysis :segments))
         (buckets (mevedel-tools--bash-buckets permission-context))
         (mode (mevedel-tool-exec--effective-permission-mode
                permission-context))
         (full-auto-p (and (not ignore-effective-trust-p)
                           (eq mode 'full-auto)))
         (full-match (mevedel-tool-exec--bash-bucket-match buckets command))
         (direct-match (mevedel-tool-exec--bash-direct-match buckets command))
         (segment-matches
          (mapcar (lambda (segment)
                    (mevedel-tool-exec--bash-bucket-match buckets segment))
                  segments))
         (segment-actions (mapcar #'car segment-matches))
         (direct-segment-actions
          (mapcar
           (lambda (segment)
             (car (mevedel-tool-exec--bash-direct-match buckets segment)))
           segments))
         (segment-classes
          (mapcar
           (lambda (segment)
             (plist-get (mevedel-tool-exec--analyze-bash segment) :class))
           segments)))
    (when (mevedel-tool-exec--bash-explicit-deny-p buckets command analysis)
      (cl-return-from mevedel-tools--check-bash-permission 'deny))

    (when (and (not (plist-get permission-context
                               :resource-authority-separated-p))
               (mevedel-tool-exec--bash-protected-path-p command analysis))
      (cl-return-from mevedel-tools--check-bash-permission 'ask))

    (cond
     ((eq (car full-match) 'ask) 'ask)
     ((memq 'deny segment-actions) 'deny)
     ((memq 'ask segment-actions) 'ask)
     ((and (memq class '(dangerous complex))
           (eq (car direct-match) 'allow))
      'allow)
     ((and (eq class 'dangerous)
           segments
           (cl-loop for action in direct-segment-actions
                    for segment-class in segment-classes
                    always (or (eq action 'allow)
                               (eq segment-class 'read-only))))
      'allow)
     ((memq class '(dangerous complex))
      (if full-auto-p 'allow 'ask))
     ((and segments (cl-every (lambda (action) (eq action 'allow))
                              segment-actions))
      'allow)
     ((eq class 'read-only)
      (if (memq 'ask segment-actions) 'ask 'allow))
     ((eq (car full-match) 'allow) 'allow)
     ((eq (car full-match) 'deny) 'deny)
     (full-auto-p 'allow)
     (t 'ask))))


;;
;;; Bash guardian guidance

(defun mevedel-tool-exec--bash-guardian-symbol (value allowed)
  "Return VALUE as a normalized symbol when it is in ALLOWED."
  (let* ((string (cond
                  ((symbolp value) (symbol-name value))
                  ((stringp value) value)))
         (symbol (and string
                      (intern
                       (replace-regexp-in-string
                        "_" "-"
                        (downcase (string-trim string)))))))
    (and (memq symbol allowed) symbol)))

(defun mevedel-tool-exec--bash-guardian-truncate (string limit)
  "Return STRING capped at LIMIT characters."
  (let ((string (string-trim (or string ""))))
    (if (> (length string) limit)
        (concat (substring string 0 limit) "...")
      string)))

(defun mevedel-tool-exec--bash-guardian-normalize (guidance)
  "Return normalized Bash guardian GUIDANCE plist, or nil."
  (when (listp guidance)
    (let* ((risk (mevedel-tool-exec--bash-guardian-symbol
                  (plist-get guidance :risk)
                  '(low medium high critical)))
           (recommendation (mevedel-tool-exec--bash-guardian-symbol
                            (plist-get guidance :recommendation)
                            '(proceed ask deny)))
           (reason (plist-get guidance :reason)))
      (when (and risk recommendation (stringp reason)
                 (not (string-empty-p (string-trim reason))))
        (list :risk risk
              :recommendation recommendation
              :reason (mevedel-tool-exec--bash-guardian-truncate reason 240))))))

(defun mevedel-tool-exec--bash-guardian-json-range (text)
  "Return the first likely JSON object substring in TEXT, or nil."
  (when-let* ((start (string-match "{" text)))
    (let ((i (1- (length text)))
          end)
      (while (and (>= i start) (not end))
        (when (eq (aref text i) ?\})
          (setq end i))
        (setq i (1- i)))
      (and end (substring text start (1+ end))))))

(defun mevedel-tool-exec--bash-guardian-parse (response)
  "Parse guardian RESPONSE into normalized guidance, or nil."
  (when (stringp response)
    (when-let* ((json (mevedel-tool-exec--bash-guardian-json-range response)))
      (condition-case nil
          (mevedel-tool-exec--bash-guardian-normalize
           (progn
             (require 'json)
             (json-parse-string json
                                :object-type 'plist
                                :array-type 'list
                                :null-object nil
                                :false-object nil)))
        (error nil)))))

(defun mevedel-tool-exec--bash-guardian-context-string (context)
  "Return CONTEXT formatted for the Bash guardian prompt."
  (string-join
   (delq nil
         (list
          (when-let* ((class (plist-get context :class)))
            (format "Command class: %s" class))
          (when-let* ((parser (plist-get context :parser)))
            (format "Parser: %s" parser))
          (format "Dangerous command detected: %s"
                  (if (plist-get context :dangerous) "yes" "no"))
          (format "Complex or unparseable syntax: %s"
                  (if (plist-get context :unparseable) "yes" "no"))
          (when-let* ((reasons (plist-get context :reasons)))
            (format "Analysis reasons: %s"
                    (if (cl-every #'stringp reasons)
                        (string-join reasons "; ")
                      (prin1-to-string reasons))))
          (when-let* ((resources (plist-get context :resources)))
            (format "Identified resources: %s"
                    (if (and (listp resources)
                             (cl-every #'stringp resources))
                        (string-join resources ", ")
                      (prin1-to-string resources))))
          (when-let* ((commands (or (plist-get context :commands-summary)
                                    (and-let* ((commands (plist-get context :commands)))
                                      (string-join commands ", ")))))
            (format "Detected commands: %s" commands))
          (when-let* ((level (plist-get context :sandbox-permissions)))
            (format "Requested sandbox permissions: %s" level))
          (when-let* ((additional
                       (plist-get context :additional-permissions)))
            (format "Requested additional permissions: %S" additional))
          (when-let* ((patterns
                       (plist-get context :matching-allow-patterns)))
            (format "Matching explicit allow patterns: %s"
                    (string-join patterns ", ")))
          (when-let* ((facts (plist-get context :sandbox-facts)))
            (require 'mevedel-sandbox)
            (format "Confinement: %s"
                    (mevedel-sandbox-status-text facts)))))
   "\n"))

(defun mevedel-tool-exec--bash-guardian-model-async (command context callback)
  "Ask gptel for advisory-only Bash risk guidance about COMMAND.
CONTEXT describes the classifier inputs.  CALLBACK receives normalized
guidance or nil."
  (if (not (require 'gptel nil t))
      (funcall callback nil)
    (let ((done nil)
          chunks
          timer)
      (cl-labels
          ((finish (guidance)
             (unless done
               (setq done t)
               (when timer
                 (cancel-timer timer))
               (funcall callback guidance))))
        (setq timer
              (run-at-time
               mevedel-permission-guardian-timeout nil
               (lambda ()
                 (finish nil))))
        (condition-case nil
            (let* ((policy
                    (progn
                      (require 'mevedel-models)
                      (mevedel-model-resolve-workload 'guardian)))
                   (gptel-use-tools nil)
                   (gptel-tools nil)
                   (gptel-use-context nil)
                   (system-prompt
                    (progn
                      (require 'mevedel-system)
                      (mevedel-system-render-prompt-file
                       "prompts/permissions/bash-guardian-system.md")))
                   (prompt
                    (format
                     "Bash source:\n```bash\n%s\n```\n\nDeterministic analysis and confinement evidence:\n```text\n%s\n```"
                     command
                     (mevedel-tool-exec--bash-guardian-context-string
                      context)))
                   (request-fn
                    (lambda ()
                      (gptel-request
                        prompt
                        :buffer (current-buffer)
                        :stream gptel-stream
                        :system system-prompt
                        :transforms nil
                        :callback
                        (lambda (response info)
                          (cond
                           ((and (consp response)
                                 (eq (car response) 'reasoning)))
                           ((and (plist-get info :stream)
                                 (stringp response))
                            (push response chunks))
                           ((eq response t)
                            (finish
                             (mevedel-tool-exec--bash-guardian-parse
                              (apply #'concat (nreverse chunks)))))
                           ((stringp response)
                            (finish
                             (mevedel-tool-exec--bash-guardian-parse response)))
                           ((or (null response) (eq response 'abort))
                            (finish nil))))))))
              (let ((gptel-backend (plist-get policy :backend))
                    (gptel-model (plist-get policy :model))
                    (gptel-reasoning-effort (plist-get policy :effort)))
                (funcall request-fn)))
          (error
           (finish nil)))))))

(defun mevedel-tool-exec--bash-guardian-classify-async
    (command context callback)
  "Return optional guardian guidance for COMMAND and CONTEXT.
CALLBACK receives nil or a normalized guidance plist."
  (cond
   ((null mevedel-permission-guardian)
    (funcall callback nil))
   ((functionp mevedel-permission-guardian)
    (let ((done nil)
          timer)
      (cl-labels
          ((finish (guidance)
             (unless done
               (setq done t)
               (when timer
                 (cancel-timer timer))
               (funcall callback
                        (mevedel-tool-exec--bash-guardian-normalize
                         guidance)))))
        (setq timer
              (run-at-time
               mevedel-permission-guardian-timeout nil
               (lambda ()
                 (finish nil))))
        (condition-case nil
            (funcall mevedel-permission-guardian command context #'finish)
          (error
           (finish nil))))))
   (t
    (mevedel-tool-exec--bash-guardian-model-async
     command context callback))))

(defun mevedel-tool-exec--bash-full-auto-guardian-needed-p
    (command &optional permission-context)
  "Return non-nil when COMMAND and PERMISSION-CONTEXT need guardian review.
This is only for `full-auto' mode.  The guardian is consulted when the
normal classifier would have asked, avoiding latency for routine allowed
commands while still giving the optional guardian a chance to veto
suspicious Bash."
  (and mevedel-permission-guardian
       (eq (mevedel-tool-exec--effective-permission-mode
            permission-context)
           'full-auto)
       (eq (mevedel-tools--check-bash-permission
            command :ignore-effective-trust-p t
            :permission-context permission-context)
           'ask)))

(defun mevedel-tool-exec--bash-guardian-context
    (command &optional permission-context)
  "Return guardian context for COMMAND and PERMISSION-CONTEXT."
  (let* ((analysis (mevedel-tool-exec--analyze-bash command))
         (commands (mevedel-tool-exec--bash-command-names analysis))
         (buckets (mevedel-tools--bash-buckets permission-context))
         (request (plist-get permission-context :sandbox-request))
         (additional-permissions
          (plist-get request :additional-permissions))
         (sandbox-permissions
          (plist-get request :sandbox-permissions))
         (rule-buckets
          (if sandbox-permissions
              (append
               buckets
               (mevedel-permission--execution-level-buckets
                buckets sandbox-permissions))
            buckets))
         (matching-allow-patterns
          (mevedel-tool-exec--dedupe-strings
           (cl-loop
            for (_bucket . rules) in rule-buckets
            append
            (cl-loop
             for rule in
             (mevedel-permission--find-rules
              rules "Bash" :pattern command)
             for pattern = (plist-get (cdr rule) :pattern)
             when (and pattern
                       (eq (plist-get (cdr rule) :action) 'allow))
             collect pattern)))))
    (require 'mevedel-sandbox)
    (list :analysis analysis
          :class (plist-get analysis :class)
          :dangerous (eq (plist-get analysis :class) 'dangerous)
          :commands commands
          :commands-summary (mevedel-tool-exec--bash-commands-summary commands)
          :parser (plist-get analysis :parser)
          :reasons (plist-get analysis :reasons)
          :resources (plist-get analysis :resources)
          :unparseable (eq (plist-get analysis :class) 'complex)
          :allow-patterns (mevedel-tool-exec--bash-allow-patterns command)
          :matching-allow-patterns matching-allow-patterns
          :additional-permissions additional-permissions
          :sandbox-permissions sandbox-permissions
          :sandbox-facts
          (mevedel-sandbox-pending-facts
           additional-permissions sandbox-permissions))))

(defun mevedel-tool-exec--bash-deny-only-guardian-async
    (command cont &optional metadata-p permission-context)
  "Run deny-only guardian review for COMMAND, then call CONT.
METADATA-P controls decision metadata.  PERMISSION-CONTEXT supplies the
pending child-confinement request.
Guardian deny recommendations become `deny'.  Timeout, failure, invalid
output, and non-deny recommendations allow by default."
  (let ((active t)
        (request (plist-get permission-context :request)))
    (when (mevedel-request-p request)
      (mevedel-request-push-canceller
       request (lambda () (setq active nil))))
    (mevedel-tool-exec--bash-guardian-classify-async
     command
     (mevedel-tool-exec--bash-guardian-context command permission-context)
     (lambda (guardian)
       (when active
         (setq active nil)
         (let ((outcome
                (if (eq (plist-get guardian :recommendation) 'deny)
                    'deny
                  'allow)))
           (when metadata-p
             (mevedel-tool-exec--log-permission-decision
              "Bash" outcome 'bash-guardian permission-context
              :specifier-key :pattern
              :specifier-value
              (mevedel-tool-exec--bash-decision-specifier-value command)))
           (funcall
            cont
            (mevedel-tool-exec--permission-decision-result
             metadata-p outcome 'bash-guardian
             :specifier-key :pattern
             :specifier-value
             (mevedel-tool-exec--bash-decision-specifier-value
              command)))))))))


;;
;;; Eval Prompt UI

(defcustom mevedel-eval-expression-display-limit 20
  "Maximum number of lines to show inline in the Eval permission prompt.
Expressions longer than this are truncated with a toggle to expand."
  :type 'integer
  :group 'mevedel)

(defun mevedel--prompt-user-for-eval
    (expression callback &optional origin count entry mode preserve-ui)
  "Display Eval permission overlay for EXPRESSION and CALLBACK.

CALLBACK is invoked once with `allow-once', `deny-once', a feedback cons,
or `aborted'.  Long expressions are truncated in
the display and can be toggled with TAB.  ORIGIN, when non-main,
renders the same attribution line used by generic and Bash permission
prompts.  COUNT is the permission queue depth for the composite
interaction-zone counter.  ENTRY identifies the queued prompt.  MODE and
PRESERVE-UI describe the requested execution scope."
  (unless (fboundp 'mevedel-permission--prompt-async-eval)
    (require 'mevedel-permission-prompt))
  (let* ((lines (split-string expression "\n"))
         (long-p (> (length lines) mevedel-eval-expression-display-limit))
         (display-expr (if long-p
                           (concat
                            (string-join
                             (seq-take lines mevedel-eval-expression-display-limit)
                             "\n")
                            "\n"
                            (propertize
                             (format "... (%d more lines, press TAB to expand)"
                                     (- (length lines) mevedel-eval-expression-display-limit))
                             'font-lock-face 'shadow))
                         expression))
         (content (concat
                   "The LLM is requesting permission to evaluate elisp.\n\n"
                   (mevedel--prompt-attribution-line origin)
                   (propertize "Mode: " 'font-lock-face 'font-lock-escape-face)
                   (format "%s" (or mode "live"))
                   (when (equal (or mode "live") "live")
                     (format " (preserve_ui: %s)"
                             (if preserve-ui "true" "false")))
                   "\n\n"
                   (propertize "Expression:\n" 'font-lock-face 'font-lock-escape-face)
                   (propertize (format "%s\n\n" display-expr)
                               'font-lock-face 'font-lock-string-face))))
    (if (fboundp 'mevedel-permission--prompt-async-eval)
        (mevedel-permission--prompt-async-eval content callback count entry)
      (display-warning
       'mevedel
       "Eval permission UI unavailable"
       :warning)
      (funcall callback 'aborted))))


;;
;;; Eval permission adapter

(cl-defun mevedel-tools--check-eval-permission
    (&key trust-literal-p permission-context)
  "Decide Eval permission for TRUST-LITERAL-P and PERMISSION-CONTEXT.

Normal model-requested Eval asks unless an explicit deny rule applies
or the effective permission mode is `full-auto'.  When TRUST-LITERAL-P
is non-nil, as with author-written skill body injections, an active
allow rule for Eval may bypass the prompt.  Deny rules still win
absolutely."
  (let* ((buckets (mevedel-tools--bash-buckets permission-context))
         (mode (mevedel-tool-exec--effective-permission-mode
                permission-context))
         (action (mevedel-permission--bucket-decision
                  buckets "Eval" nil nil nil nil)))
    (cond
     ((eq action 'deny) 'deny)
     ((eq mode 'full-auto)
      'allow)
     (trust-literal-p
      (or action 'ask))
     (t 'ask))))

(defun mevedel-tool-exec--eval-check-command-permission-async
    (_tool-struct input cont)
  "Async permission check for Eval tool INPUT.

Routes the prompt through the session permission queue rather
than calling `mevedel--prompt-user-for-eval' directly.  The
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
         (preserve-ui (mevedel-tool-exec--eval-preserve-ui-p input)))
    (condition-case err
        (setq mode (mevedel-tool-exec--eval-mode input))
      (error (setq mode-error (error-message-string err))))
    (cond
     (mode-error
      (when metadata-p
        (mevedel-tool-exec--log-permission-decision
         "Eval" (cons 'deny mode-error) 'eval-policy permission-context))
      (funcall cont
               (mevedel-tool-exec--permission-decision-result
                metadata-p (cons 'deny mode-error) 'eval-policy)))
     ((null expression)
      (when metadata-p
        (mevedel-tool-exec--log-permission-decision
         "Eval" 'deny 'eval-policy permission-context))
      (funcall cont
               (mevedel-tool-exec--permission-decision-result
                metadata-p 'deny 'eval-policy)))
     (t
      (pcase (mevedel-tools--check-eval-permission
              :trust-literal-p trust-literal-p
              :permission-context permission-context)
        ('allow
         (when metadata-p
           (mevedel-tool-exec--log-permission-decision
            "Eval" 'allow 'eval-policy permission-context))
         (funcall cont
                  (mevedel-tool-exec--permission-decision-result
                   metadata-p 'allow 'eval-policy)))
        ('deny
         (when metadata-p
           (mevedel-tool-exec--log-permission-decision
            "Eval" 'deny 'eval-policy permission-context))
         (funcall cont
                  (mevedel-tool-exec--permission-decision-result
                   metadata-p 'deny 'eval-policy)))
        (_
         (if trust-literal-p
             (let ((outcome
                    (cons 'deny
                          "Elisp expansion requires a pre-approved Eval rule; no prompt is shown while preparing skill bodies.")))
               (when metadata-p
                 (mevedel-tool-exec--log-permission-decision
                  "Eval" outcome 'eval-policy permission-context))
               (funcall
                cont
                (mevedel-tool-exec--permission-decision-result
                 metadata-p outcome 'eval-policy)))
           (when metadata-p
             (mevedel-tool-exec--log-permission-decision
              "Eval" 'ask 'eval-policy permission-context))
           (apply #'mevedel-permission--enqueue
            (list :kind 'eval
                  :expression expression
                  :mode (symbol-name mode)
                  :preserve-ui preserve-ui
                  :origin
                  (mevedel-tool-exec--permission-origin permission-context)
                  :callback
                  (lambda (outcome)
                    (pcase outcome
                      ('allow-once
                       (funcall
                        cont
                        (mevedel-tool-exec--permission-decision-result
                         metadata-p 'allow 'eval-policy)))
                      ('deny-once
                       (funcall
                        cont
                        (mevedel-tool-exec--permission-decision-result
                         metadata-p 'deny 'eval-policy)))
                      (`(feedback . ,text)
                       (funcall cont
                                (mevedel-tool-exec--permission-decision-result
                                 metadata-p
                                 (cons 'deny
                                       (format "Eval cancelled by user. Feedback: %s"
                                               text))
                                 'eval-policy)))
                      ('aborted
                       (funcall
                        cont
                        (mevedel-tool-exec--permission-decision-result
                         metadata-p 'aborted 'eval-policy)))
                      (_
                       (funcall
                        cont
                        (mevedel-tool-exec--permission-decision-result
                         metadata-p 'deny 'eval-policy))))))
            (and (plist-get permission-context :session)
                 (list (plist-get permission-context :session)))))))))))

(defun mevedel-tool-exec--eval-check-permission-async
    (tool-struct input cont)
  "Authorize Eval INPUT, then layer any requested child authority.
TOOL-STRUCT and CONT follow the async permission slot contract."
  (condition-case err
      (let* ((input (mevedel-tool-exec--capture-permission-origin input))
             (mode (mevedel-tool-exec--eval-mode input))
             (request (mevedel-tool-exec--sandbox-request
                       input 'eval mode))
             (command-input
              (mevedel-tool-exec--command-permission-input
               input request)))
        (if (eq (plist-get request :level) 'escalated)
            (mevedel-tool-exec--check-full-escalation-async
             "Eval" (plist-get input :expression) input request cont)
          (mevedel-tool-exec--eval-check-command-permission-async
           tool-struct command-input
           (lambda (outcome)
             (mevedel-tool-exec--check-additional-permission-async
              "Eval" (plist-get input :expression) input request outcome cont)))))
    (error
     (funcall
      cont
      (mevedel-tool-exec--permission-decision-result
       (plist-get input :permission-decision-metadata)
       (cons 'deny (error-message-string err)) 'sandbox-policy)))))


;;
;;; Bash Prompt UI

(defun mevedel-tool-exec--check-command-permission-async
    (_tool-struct input cont)
  "Async permission check for Bash tool INPUT.

Pattern matching first: when `mevedel-tools--check-bash-permission'
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
      (let ((decision (mevedel-tools--check-bash-permission
                       command :trust-literal-p trust-literal-p
                       :permission-context permission-context)))
        (cond
         ((not (eq decision 'ask))
          (if (and (eq decision 'allow)
                   (mevedel-tool-exec--bash-full-auto-guardian-needed-p
                    command permission-context))
              (mevedel-tool-exec--bash-deny-only-guardian-async
               command cont metadata-p permission-context)
            (when metadata-p
              (mevedel-tool-exec--log-permission-decision
               "Bash" decision 'bash-classifier permission-context
               :specifier-key :pattern
               :specifier-value (mevedel-tool-exec--bash-decision-specifier-value
                                 command)))
            (funcall
             cont
             (mevedel-tool-exec--permission-decision-result
              metadata-p decision 'bash-classifier
              :specifier-key :pattern
              :specifier-value (mevedel-tool-exec--bash-decision-specifier-value
                                 command)))))
         (trust-literal-p
          (let ((outcome
                 (cons 'deny
                       "Shell expansion requires a pre-approved Bash rule; no prompt is shown while preparing skill bodies.")))
            (when metadata-p
              (mevedel-tool-exec--log-permission-decision
               "Bash" outcome 'bash-classifier permission-context
               :specifier-key :pattern
               :specifier-value (mevedel-tool-exec--bash-decision-specifier-value
                                 command)))
            (funcall
             cont
             (mevedel-tool-exec--permission-decision-result
              metadata-p outcome 'bash-classifier
              :specifier-key :pattern
              :specifier-value (mevedel-tool-exec--bash-decision-specifier-value
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
                       (mevedel-tool-exec--bash-guardian-context
                        command permission-context)))
                 (analysis
                  (or (plist-get guardian-context :analysis)
                      (mevedel-tool-exec--analyze-bash command)))
                 (command-class (plist-get analysis :class))
                 (commands (mevedel-tool-exec--bash-command-names analysis))
                 (commands-summary
                  (mevedel-tool-exec--bash-commands-summary commands))
                 (unparseable (eq command-class 'complex))
                 (allow-patterns
                  (or (plist-get guardian-context :allow-patterns)
                      (mevedel-tool-exec--bash-allow-patterns command)))
                 (rule-creating-p
                  (not (memq command-class '(dangerous complex))))
                 (guardian-cell
                  (list nil (and mevedel-permission-guardian 'pending)))
                 (entry
                  (list :kind 'bash
                        :command command
                        :analysis analysis
                        :command-class command-class
                        :commands commands
                        :commands-summary commands-summary
                        :unparseable unparseable
                        :allow-patterns allow-patterns
                        :guardian-cell guardian-cell
                        :workspace workspace
                        :include-always (and rule-creating-p
                                             (not (null workspace)))
                        :origin
                        (mevedel-tool-exec--permission-origin
                         permission-context)
                        :callback
                        (lambda (outcome)
                          (setq guardian-pending nil)
                          (pcase outcome
                            ;; Route 5-button outcomes through
                            ;; --apply-bash-prompt-result so allow-session /
                            ;; always-allow create the suggested pattern rule
                            ;; before we settle the slot.  The function
                            ;; collapses each outcome to 'allow / 'deny.
                            ((or 'allow-once 'allow-session 'always-allow
                                 'deny-once 'deny-session)
                             (condition-case err
                                 (let ((collapsed
                                        (mevedel-tool-exec--apply-bash-prompt-result
                                         outcome session workspace command
                                         allow-patterns)))
                                   (funcall
                                    cont
                                    (mevedel-tool-exec--permission-decision-result
                                     metadata-p collapsed 'bash-classifier
                                     :specifier-key :pattern
                                     :specifier-value (mevedel-tool-exec--bash-decision-specifier-value
                                 command))))
                               (error
                                (funcall
                                 cont
                                 (mevedel-tool-exec--permission-decision-result
                                  metadata-p
                                  (format "Error: Bash rule write failed: %S" err)
                                  'bash-classifier
                                  :specifier-key :pattern
                                  :specifier-value (mevedel-tool-exec--bash-decision-specifier-value
                                 command))))))
                            ('allow
                             (funcall
                              cont
                              (mevedel-tool-exec--permission-decision-result
                               metadata-p 'allow 'bash-classifier
                               :specifier-key :pattern
                               :specifier-value (mevedel-tool-exec--bash-decision-specifier-value
                                 command))))
                            ('deny
                             (funcall
                              cont
                              (mevedel-tool-exec--permission-decision-result
                               metadata-p 'deny 'bash-classifier
                               :specifier-key :pattern
                               :specifier-value (mevedel-tool-exec--bash-decision-specifier-value
                                 command))))
                            (`(deny . ,reason)
                             (funcall
                              cont
                              (mevedel-tool-exec--permission-decision-result
                               metadata-p (cons 'deny reason) 'bash-classifier
                               :specifier-key :pattern
                               :specifier-value (mevedel-tool-exec--bash-decision-specifier-value
                                 command))))
                            (`(feedback . ,text)
                             (funcall
                              cont
                              (mevedel-tool-exec--permission-decision-result
                               metadata-p
                               (cons 'deny
                                     (format "Command cancelled by user. Feedback: %s"
                                             text))
                               'bash-classifier
                               :specifier-key :pattern
                               :specifier-value (mevedel-tool-exec--bash-decision-specifier-value
                                 command))))
                            ('aborted
                             (funcall
                              cont
                              (mevedel-tool-exec--permission-decision-result
                               metadata-p 'aborted 'bash-classifier
                               :specifier-key :pattern
                               :specifier-value (mevedel-tool-exec--bash-decision-specifier-value
                                 command))))
                            (_ (funcall
                                cont
                                (mevedel-tool-exec--permission-decision-result
                                 metadata-p 'deny 'bash-classifier
                                 :specifier-key :pattern
                                 :specifier-value (mevedel-tool-exec--bash-decision-specifier-value
                                 command)))))))))
            (when metadata-p
              (mevedel-tool-exec--log-permission-decision
               "Bash" 'ask 'bash-classifier permission-context
               :specifier-key :pattern
               :specifier-value (mevedel-tool-exec--bash-decision-specifier-value
                                 command)))
            (if (buffer-live-p source-buffer)
                (with-current-buffer source-buffer
                  (mevedel-permission--enqueue entry session))
              (mevedel-permission--enqueue entry session))
            (when mevedel-permission-guardian
              (setq guardian-context
                    (plist-put guardian-context :workspace workspace))
              (mevedel-tool-exec--bash-guardian-classify-async
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

(defun mevedel-tool-exec--check-permission-async
    (tool-struct input cont)
  "Authorize Bash INPUT, then layer any requested child authority.
TOOL-STRUCT and CONT follow the async permission slot contract."
  (condition-case err
      (let* ((input (mevedel-tool-exec--capture-permission-origin input))
             (request (mevedel-tool-exec--sandbox-request input 'bash))
             (missing-resources
              (unless (eq (plist-get request :level) 'escalated)
                (mevedel-tool-exec--bash-missing-resource-paths
                 (plist-get input :command)
                 (plist-get input :permission-context)
                 request)))
             (command-input
              (mevedel-tool-exec--command-permission-input
               input request)))
        (cond
         ((eq (plist-get request :level) 'escalated)
          (mevedel-tool-exec--check-full-escalation-async
           "Bash" (plist-get input :command) input request cont))
         (t
          (mevedel-tool-exec--check-command-permission-async
           tool-struct command-input
           (lambda (outcome)
             (cond
              ((not (mevedel-tool-exec--permission-allow-p outcome))
               (funcall cont outcome))
              (missing-resources
               (funcall
                cont
                (mevedel-tool-exec--permission-decision-result
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
               (mevedel-tool-exec--check-additional-permission-async
                "Bash" (plist-get input :command) input request outcome
                cont))))))))
    (error
     (funcall
      cont
      (mevedel-tool-exec--permission-decision-result
       (plist-get input :permission-decision-metadata)
       (cons 'deny (error-message-string err)) 'sandbox-policy)))))

(defun mevedel-tool-exec--apply-bash-prompt-result
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


;;
;;; Bash

(defun mevedel-tool-exec--default-directory ()
  "Return the cwd Bash/Eval handlers should use.

Prefer the active session working directory when available, then the
workspace root.  Fall back to the caller's `default-directory' for
direct non-workspace uses."
  (let* ((session (and (boundp 'mevedel--session) mevedel--session))
         (workspace (cond
                     (session (mevedel-session-workspace session))
                     ((and (boundp 'mevedel--workspace) mevedel--workspace))))
         (session-dir (and session
                           (ignore-errors
                             (mevedel-session-working-directory session))))
         (root (and workspace
                    (ignore-errors
                      (mevedel-workspace-root workspace)))))
    (file-name-as-directory (or session-dir root default-directory))))

(defun mevedel-tool-exec--bash-timeout-seconds (args)
  "Return the effective Bash timeout in seconds for ARGS."
  (let ((override (plist-get args :timeout_seconds)))
    (cond
     ((and override (not (integerp override)))
      (error "Parameter timeout_seconds must be an integer"))
     ((and (integerp override) (<= override 0))
      (error "Parameter timeout_seconds must be positive"))
     ((null mevedel-bash-timeout) nil)
     ((integerp override) override)
     ((and (integerp mevedel-bash-timeout)
           (> mevedel-bash-timeout 0))
      mevedel-bash-timeout)
     (t
      (error "Variable mevedel-bash-timeout must be nil or a positive integer")))))

(defun mevedel-tool-exec--bash-yield-time-ms (input)
  "Return the validated Bash yield time in milliseconds from INPUT."
  (let* ((milliseconds
          (if (plist-member input :yield-time_ms)
              (plist-get input :yield-time_ms)
            10000)))
    (unless (and (integerp milliseconds)
                 (<= 250 milliseconds)
                 (<= milliseconds 30000))
      (error "Parameter yield_time_ms must be an integer from 250 to 30000"))
    milliseconds))

(defun mevedel-tool-exec--write-wait-time-ms (input chars)
  "Return the validated observation wait from INPUT and CHARS."
  (let* ((input-p (and (stringp chars) (not (string-empty-p chars))))
         (milliseconds
          (if (plist-member input :yield-time_ms)
              (plist-get input :yield-time_ms)
            (if input-p 250 5000)))
         (minimum (if input-p 250 5000))
         (maximum (if input-p 30000 300000)))
    (unless (and (integerp milliseconds)
                 (<= minimum milliseconds)
                 (<= milliseconds maximum))
      (error "Parameter yield_time_ms must be an integer from %d to %d"
             minimum maximum))
    milliseconds))

(defun mevedel-tool-exec--execution-artifact-directory (session)
  "Return SESSION's retained execution artifact directory, if available."
  (when-let* ((root
              (mevedel-pipeline--tool-results-dir
               session
               (or (and (boundp 'mevedel--data-buffer)
                        mevedel--data-buffer)
                   (current-buffer)))))
    (file-name-concat root "executions")))

(defun mevedel-tool-exec--sandbox-writable-roots (workdir)
  "Return writable child-confinement roots for WORKDIR."
  (let ((roots
         (condition-case nil
             (progn
               (require 'mevedel-workspace)
               (mevedel--all-allowed-roots (current-buffer)))
           (error nil))))
    (delete-dups
     (append (or roots
                 (list (file-name-as-directory (expand-file-name workdir))))
             (list (file-name-as-directory
                    (expand-file-name temporary-file-directory)))))))

(defun mevedel-tool-exec--sandbox-disclosure
    (text child-result &optional suppress-p)
  "Append confinement facts from CHILD-RESULT to TEXT unless SUPPRESS-P."
  (let ((facts (plist-get child-result :sandbox-facts)))
    (cond
     ((not facts) text)
     (suppress-p
      (when (eq (plist-get facts :filesystem) 'unrestricted)
        (require 'mevedel-sandbox)
        (display-warning
         'mevedel
         (concat "Skill shell expansion ran without confinement: "
                 (mevedel-sandbox-status-text facts))
         :warning))
      text)
     (t
      (require 'mevedel-sandbox)
      (concat text
              (unless (string-empty-p (or text "")) "\n\n")
              "[" (mevedel-sandbox-status-text facts) "]")))))

(defun mevedel-tool-exec--execution-facts-xml (facts)
  "Return compact model-visible XML derived from canonical FACTS."
  (require 'xml)
  (let (attributes)
    (dolist (entry '((:execution-id . "execution_id")
                     (:command . "command")
                     (:state . "state")
                     (:termination . "termination")
                     (:exit-code . "exit_code")
                     (:outcome . "outcome")
                     (:wall-time-seconds . "wall_time_seconds")
                     (:output-bytes . "output_bytes")
                     (:output-lines . "output_lines")
                     (:omitted-output-bytes . "omitted_output_bytes")
                     (:tty . "tty")
                     (:output-path . "output_path")))
      (let ((value (plist-get facts (car entry))))
        (when (or value (eq (car entry) :tty))
          (push
           (format "%s=\"%s\"" (cdr entry)
                   (xml-escape-string
                    (cond
                     ((eq value t) "true")
                     ((null value) "false")
                     ((floatp value) (format "%.3f" value))
                     (t (format "%s" value)))))
           attributes))))
    (format "<bash-execution %s/>" (string-join (nreverse attributes) " "))))

(defun mevedel-tool-exec-format-execution-metadata (facts timeout-seconds)
  "Return compact UI metadata for execution FACTS and TIMEOUT-SECONDS."
  (string-join
   (delq nil
         (list
          (when-let* ((state (plist-get facts :state)))
            (symbol-name state))
          (when (plist-member facts :wall-time-seconds)
            (format "%.1fs" (or (plist-get facts :wall-time-seconds) 0)))
          (when (plist-member facts :output-lines)
            (format "%d lines" (or (plist-get facts :output-lines) 0)))
          (when (plist-member facts :output-bytes)
            (format "%d bytes" (or (plist-get facts :output-bytes) 0)))
          (and timeout-seconds (format "timeout %ss" timeout-seconds))
          (plist-get facts :execution-id)))
   " · "))

(defun mevedel-tool-exec--bash-outcome (analysis exit-code termination)
  "Derive a canonical outcome from ANALYSIS, EXIT-CODE, and TERMINATION."
  (let* ((commands (plist-get analysis :commands))
         (command
          (and (= (length commands) 1)
               (not (memq (plist-get analysis :class) '(complex dangerous)))
               (caar commands)))
         (exit-one-outcome
          (pcase command
            ((or "grep" "rg") 'no-match)
            ("diff" 'different)
            ((or "test" "[") 'false))))
    (cond
     ((not (eq termination 'exited)) 'failure)
     ((and (integerp exit-code) (zerop exit-code)) 'success)
     ((and (equal exit-code 1) exit-one-outcome) exit-one-outcome)
     (t 'failure))))

(defun mevedel-tool-exec--observation-envelope
    (observation &optional suppress-sandbox-disclosure-p force-success-p)
  "Return a handler envelope for managed OBSERVATION.

SUPPRESS-SANDBOX-DISCLOSURE-P preserves trusted internal expansion behavior.
FORCE-SUCCESS-P makes control-tool completion successful independently of the
stopped command's outcome."
  (let* ((facts (plist-get observation :facts))
         (output (or (plist-get observation :output) ""))
         (error-data (plist-get observation :error))
         (body (if error-data
                   (format "Failed to start process: %s" error-data)
                 output))
         (with-sandbox
          (mevedel-tool-exec--sandbox-disclosure
           body observation suppress-sandbox-disclosure-p))
         (result
          (if suppress-sandbox-disclosure-p
              with-sandbox
            (concat with-sandbox
                    (unless (string-empty-p with-sandbox) "\n\n")
                    (mevedel-tool-exec--execution-facts-xml facts))))
         (status
          (if (or force-success-p
                  (not (eq (plist-get facts :state) 'completed))
                  (memq (plist-get facts :outcome)
                        '(success no-match different false)))
              'success
            'error)))
    (list :result result
          :status status
          :render-data
          (append (copy-sequence facts)
                  (list :sandbox-facts
                        (plist-get observation :sandbox-facts))))))

(defun mevedel-tool-exec-handle-execution-event (event owner-context)
  "Secure an independently completed Bash EVENT in its owner mailbox."
  (when (and (eq (plist-get event :type) 'terminal)
             (eq (plist-get event :delivery) 'mailbox))
    (let* ((args (plist-get event :tool-args))
           (observation (plist-get event :observation))
           (envelope
            (mevedel-tool-exec--observation-envelope
             observation
             (plist-get args :suppress-sandbox-disclosure-p))))
      (if (mevedel-session-p owner-context)
          (progn
            (require 'mevedel-agent-control)
            (mevedel-agent-control-enqueue-execution-result
             owner-context
             (plist-get event :owner)
             (plist-get envelope :result)))
        (require 'mevedel-agent-runtime)
        (mevedel-agent-runtime-queue-execution-completion
         owner-context
         (plist-get event :owner)
         (plist-get envelope :result))))))

(defun mevedel-tool-exec--bash (callback args)
  "Execute a Bash command and return its output.
CALLBACK receives the result envelope.  ARGS is a plist with :command
and optional :timeout_seconds."
  (let ((command (plist-get args :command))
        (tty (plist-get args :tty)))
    (unless (stringp command)
      (error "Parameter command is required"))
    (unless (memq tty '(nil t :json-false))
      (error "Parameter tty must be a boolean"))
    (let* ((analysis (mevedel-tool-exec--analyze-bash command))
           (_ (when (plist-get analysis :background-p)
                (error "Shell-native background execution is not supported; use yield_time_ms")))
           (sandbox-request (mevedel-tool-exec--sandbox-request args 'bash))
           (session (mevedel-tool-exec--permission-log-session))
           (invocation
            (and (boundp 'mevedel--agent-invocation)
                 mevedel--agent-invocation))
           (owner (mevedel-current-origin))
           (timeout (mevedel-tool-exec--bash-timeout-seconds args))
           (yield-time-ms
            (unless (plist-get args :wait-for-completion-p)
              (mevedel-tool-exec--bash-yield-time-ms args)))
           (workdir (mevedel-tool-exec--default-directory)))
      (unless session
        (error "Bash requires an active session"))
      (require 'mevedel-execution)
      (mevedel-execution-start-bash
       (lambda (observation)
         (funcall
          callback
          (mevedel-tool-exec--observation-envelope
           observation (plist-get args :suppress-sandbox-disclosure-p))))
       :session session :data-buffer (current-buffer)
       :owner owner :request mevedel--current-request
       :owner-context (or invocation session)
       :tool-args args
       :tool-use-id (mevedel-pipeline-active-tool-use-id)
       :command (list "bash" "-lc" command)
       :workdir workdir
       :writable-roots (mevedel-tool-exec--sandbox-writable-roots workdir)
       :timeout timeout
       :outcome-function
       (lambda (exit-code termination)
         (mevedel-tool-exec--bash-outcome analysis exit-code termination))
       :read-only-p (eq (plist-get analysis :class) 'read-only)
       :tty (eq tty t)
       :yield-time-ms yield-time-ms
       :artifact-directory
       (mevedel-tool-exec--execution-artifact-directory session)
       :additional-permissions
       (plist-get sandbox-request :additional-permissions)
       :sandbox-permissions
       (plist-get sandbox-request :sandbox-permissions)))))

(defun mevedel-tool-exec--write-stdin (callback args)
  "Poll or write to one owner-scoped yielded execution from ARGS."
  (let* ((execution-id (plist-get args :execution_id))
         (chars (or (plist-get args :chars) ""))
         (session (mevedel-tool-exec--permission-log-session))
         (owner (mevedel-current-origin))
         (wait-ms (mevedel-tool-exec--write-wait-time-ms args chars)))
    (unless (and (stringp execution-id) (not (string-empty-p execution-id)))
      (error "Parameter execution_id is required"))
    (unless (stringp chars)
      (error "Parameter chars must be a string"))
    (unless session
      (error "WriteStdin requires an active session"))
    (require 'mevedel-execution)
    (mevedel-execution-observe
     session owner execution-id
     (lambda (observation)
       (funcall callback
                (mevedel-tool-exec--observation-envelope observation)))
     :chars chars :wait-ms wait-ms :request mevedel--current-request)))

(defun mevedel-tool-exec--list-executions (_args)
  "Return yielded executions visible to the current model owner."
  (let ((session (mevedel-tool-exec--permission-log-session))
        (owner (mevedel-current-origin)))
    (unless session
      (error "ListExecutions requires an active session"))
    (require 'mevedel-execution)
    (let* ((facts (mevedel-execution-list session owner))
           (result
            (if facts
                (mapconcat #'mevedel-tool-exec--execution-facts-xml facts "\n")
              "No yielded executions.")))
      (list :result result :status 'success :render-data facts))))

(defun mevedel-tool-exec--stop-execution (callback args)
  "Stop one owner-scoped yielded execution named by ARGS."
  (let ((execution-id (plist-get args :execution_id))
        (session (mevedel-tool-exec--permission-log-session))
        (owner (mevedel-current-origin)))
    (unless (and (stringp execution-id) (not (string-empty-p execution-id)))
      (error "Parameter execution_id is required"))
    (unless session
      (error "StopExecution requires an active session"))
    (require 'mevedel-execution)
    (mevedel-execution-stop
     session owner execution-id
     (lambda (observation)
       (funcall callback
                (mevedel-tool-exec--observation-envelope
                 observation nil t))))))


;;
;;; Eval

(defun mevedel-tool-exec--eval-mode (args)
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

(defun mevedel-tool-exec--eval-preserve-ui-p (args)
  "Return non-nil when ARGS request restoring window state."
  (not (eq (plist-get args :preserve_ui) :json-false)))

(defun mevedel-tool-exec--eval-format-result
    (result output result-format)
  "Format Eval RESULT and captured OUTPUT for RESULT-FORMAT."
  (if (equal result-format "injection")
      (concat
       (format "%S" result)
       (and (not (string-empty-p (or output "")))
            (format "\n\nSTDOUT:\n%s" output)))
    (concat
     (format "Result:\n%S" result)
     (and (not (string-empty-p (or output "")))
          (format "\n\nSTDOUT:\n%s" output)))))

(defun mevedel-tool-exec--eval-format-error (err output)
  "Format Eval error ERR and captured OUTPUT."
  (concat
   (format "Error: Eval failed with error %S: %S"
           (car err) (cdr err))
   (and (not (string-empty-p (or output "")))
        (format "\n\nSTDOUT:\n%s" output))))

(defun mevedel-tool-exec--eval-live (callback expression result-format preserve-ui)
  "Evaluate EXPRESSION in the live Emacs process.
CALLBACK receives the result envelope.  RESULT-FORMAT controls
the model-facing shape.  PRESERVE-UI restores the selected frame's
window configuration after evaluation."
  (let ((standard-output (generate-new-buffer " *mevedel-eval-elisp*"))
        (window-configuration (and preserve-ui
                                   (current-window-configuration)))
        (result nil) (output nil) response)
    (unwind-protect
        (condition-case err
            (let ((default-directory
                    (mevedel-tool-exec--default-directory)))
              (setq result (eval (read expression) t))
              (when (> (buffer-size standard-output) 0)
                (setq output (with-current-buffer standard-output
                               (buffer-string))))
              (setq response
                    (mevedel-tool-exec--eval-format-result
                     result output result-format)))
          ((error user-error)
           (when (> (buffer-size standard-output) 0)
             (setq output (with-current-buffer standard-output
                            (buffer-string))))
           (setq response
                 (mevedel-tool-exec--eval-format-error err output))))
      (when (window-configuration-p window-configuration)
        (ignore-errors
          (set-window-configuration window-configuration)))
      (kill-buffer standard-output))
    (funcall callback (list :result response))))

(defun mevedel-tool-exec--eval-batch-script
    (expression result-file workdir load-path-value result-format)
  "Return batch Eval source for EXPRESSION writing RESULT-FILE.
WORKDIR, LOAD-PATH-VALUE, and RESULT-FORMAT configure the child Emacs."
  (concat
   ";;; -*- lexical-binding: t -*-\n"
   (prin1-to-string
    `(let ((load-path ',load-path-value)
           (default-directory ,workdir)
           (expression ,expression)
           (result-file ,result-file)
           (result-format ',result-format)
           (stdout-buffer (generate-new-buffer " *mevedel-eval-batch-stdout*"))
           result output)
       (unwind-protect
           (let ((standard-output stdout-buffer))
             (condition-case err
                 (progn
                   (setq result (eval (read expression) t))
                   (setq output
                         (with-current-buffer stdout-buffer
                           (buffer-string)))
                   (with-temp-file result-file
                     (prin1 (list :status 'ok
                                  :text
                                  (if (equal result-format "injection")
                                      (concat
                                       (format "%S" result)
                                       (and (> (length (or output "")) 0)
                                            (format "\n\nSTDOUT:\n%s" output)))
                                    (concat
                                     (format "Result:\n%S" result)
                                     (and (> (length (or output "")) 0)
                                          (format "\n\nSTDOUT:\n%s" output)))))
                            (current-buffer))))
               ((error user-error)
                (setq output
                      (and (buffer-live-p stdout-buffer)
                           (with-current-buffer stdout-buffer
                             (buffer-string))))
                (with-temp-file result-file
                  (prin1 (list :status 'error
                               :text
                               (concat
                                (format "Error: Eval failed with error %S: %S"
                                        (car err) (cdr err))
                                (and (> (length (or output "")) 0)
                                     (format "\n\nSTDOUT:\n%s" output))))
                         (current-buffer))))))
         (when (buffer-live-p stdout-buffer)
           (kill-buffer stdout-buffer)))))))

(defun mevedel-tool-exec--eval-read-batch-result (result-file)
  "Read the batch Eval result plist from RESULT-FILE."
  (when (file-exists-p result-file)
    (with-temp-buffer
      (insert-file-contents result-file)
      (let ((read-eval nil))
        (read (current-buffer))))))

(defun mevedel-tool-exec--eval-batch
    (callback expression result-format additional-permissions
              &optional sandbox-permissions)
  "Evaluate EXPRESSION in a child process and call CALLBACK.
ADDITIONAL-PERMISSIONS is the validated additive execution profile.
SANDBOX-PERMISSIONS may be `require-escalated' after authorization."
  (let* ((workdir (mevedel-tool-exec--default-directory))
         (session (mevedel-tool-exec--permission-log-session))
         (owner (mevedel-current-origin))
         (script-file (make-temp-file "mevedel-eval-batch-" nil ".el"))
         (result-file (make-temp-file "mevedel-eval-result-" nil ".el"))
         (script (mevedel-tool-exec--eval-batch-script
                  expression result-file workdir load-path result-format))
         cleaned
         (cleanup
          (lambda ()
            (unless cleaned
              (setq cleaned t)
              (ignore-errors (delete-file script-file))
              (ignore-errors (delete-file result-file))))))
    (condition-case err
        (progn
          (with-temp-file script-file
            (insert script))
          (require 'mevedel-execution)
          (mevedel-execution-start-one-shot
           (lambda (child-result)
             (let* ((exit-code (plist-get child-result :exit-code))
                    (diagnostics (plist-get child-result :output))
                    (payload
                     (condition-case nil
                         (mevedel-tool-exec--eval-read-batch-result
                          result-file)
                       (error nil))))
               (unwind-protect
                   (funcall
                    callback
                    (list
                     :result
                     (mevedel-tool-exec--sandbox-disclosure
                      (cond
                       ((eq (plist-get payload :status) 'ok)
                        (or (plist-get payload :text) ""))
                       ((eq (plist-get payload :status) 'error)
                        (or (plist-get payload :text) "Error: Eval failed"))
                       ((plist-get child-result :error)
                        (format "Failed to start Eval batch process: %s"
                                (plist-get child-result :error)))
                       (t
                        (format
                         "Error: Eval batch process failed with exit code %d%s"
                         exit-code
                         (if (string-empty-p (or diagnostics ""))
                             ""
                           (format ":\n%s" diagnostics)))))
                      child-result)))
                 (funcall cleanup))))
           :name "mevedel-eval-batch"
           :command
           (list (expand-file-name invocation-name invocation-directory)
                 "-Q" "--batch" "-l" script-file)
           :workdir workdir
           :writable-roots (mevedel-tool-exec--sandbox-writable-roots workdir)
           :additional-permissions additional-permissions
           :sandbox-permissions sandbox-permissions
           :session session
           :owner owner
           :teardown-function cleanup))
      (error
       (funcall cleanup)
       (funcall callback
                (list :result
                      (format "Failed to start Eval batch process: %s" err)))
       nil))))

(defun mevedel-tool-exec--eval (callback args)
  "Evaluate an Elisp expression and return the result.
CALLBACK receives the result envelope.  ARGS is a plist with :expression."
  (let ((expression (plist-get args :expression))
        (result-format (plist-get args :result-format))
        (mode (mevedel-tool-exec--eval-mode args)))
    (unless (stringp expression)
      (error "Parameter expression is required"))
    (let ((request (mevedel-tool-exec--sandbox-request args 'eval mode)))
      (pcase mode
        ('live
         (mevedel-tool-exec--eval-live
          callback expression result-format
          (mevedel-tool-exec--eval-preserve-ui-p args)))
        ('batch
         (mevedel-tool-exec--eval-batch
          callback expression result-format
          (plist-get request :additional-permissions)
          (plist-get request :sandbox-permissions)))))))


;;
;;; Renderers

(defun mevedel-tool-exec--render-bash (name args result render-data)
  "Rendering plist for the Bash tool.
NAME is \"Bash\".  ARGS carries `:command'.  RESULT is stdout/stderr.
Header shows a truncated first line of the command; body fontifies as
`sh-mode'."
  (when (stringp result)
    (let* ((cmd (or (plist-get args :command) ""))
           (first-line (car (split-string cmd "\n")))
           (body
            (replace-regexp-in-string
             "\n*<bash-execution [^\n]*/>[ \t\r\n]*\\'" "" result))
           (status (plist-get render-data :status))
           (state (plist-get render-data :state))
           (metadata
            (and state
                 (mevedel-tool-exec-format-execution-metadata
                  render-data (plist-get render-data :timeout-seconds)))))
      (list :header (concat (format "%s: %s" (or name "Bash") first-line)
                            (and metadata (format " (%s)" metadata)))
            :body body
            :body-mode 'sh-mode
            :status status
            :force-expanded-p
            (and (plist-get render-data :live-execution-p) t)
            :initially-collapsed-p
            (not (plist-get render-data :live-execution-p))))))

(defun mevedel-tool-exec--render-eval (name args result _render-data)
  "Return rendering plist for Eval NAME with ARGS and RESULT."
  (when (stringp result)
    (let* ((expression (or (plist-get args :expression) ""))
           (first-line (car (split-string expression "\n")))
           (mode (let ((raw (plist-get args :mode)))
                   (if (or (null raw)
                           (eq raw :json-false)
                           (and (stringp raw) (string-empty-p raw)))
                       "live"
                     raw)))
           (status (and (string-prefix-p "Error:" result) 'error)))
      (list :header (format "%s: %s %s"
                            (or name "Eval")
                            mode
                            (truncate-string-to-width
                             first-line 60 nil nil "..."))
            :body result
            :body-mode 'emacs-lisp-mode
            :status status
            :initially-collapsed-p t))))


;;
;;; Tool registration

(defun mevedel-tool-exec--register ()
  "Register Bash and Eval tools."

  (mevedel-define-tool
    :name "Bash"
    :description "Execute Bash commands."
    :prompt-file "tools/bash.md"
    :handler #'mevedel-tool-exec--bash
    :args ((command string :required
                   "The Bash command to execute from the session working directory. Can include pipes and standard shell operators.")
           (timeout_seconds integer :optional
                            "Optional timeout in seconds. Defaults to `mevedel-bash-timeout' (120 seconds). Must be positive.")
           (yield_time_ms integer :optional
                          "Milliseconds to wait before yielding a still-running command. Defaults to 10000; range 250-30000.")
           (tty boolean :optional
                "Allocate a PTY and retain stdin for prompts or REPL input. Defaults to false.")
           (sandbox_permissions string :optional
                                "Child-execution authority: use_default, with_additional_permissions, or require_escalated for a complete confinement bypass."
                                :enum ["use_default"
                                       "with_additional_permissions"
                                       "require_escalated"])
           (additional_permissions object :optional
                                   "Capabilities requested in addition to the default confinement profile."
                                   :properties
                                   (:network
                                    (:type boolean
                                     :description "Allow network access for this invocation.")
                                    :file_system
                                    (:type object
                                     :description "Exact filesystem paths to reopen inside confinement."
                                     :properties
                                     (:read
                                      (:type array
                                       :items (:type string)
                                       :description "Absolute paths requiring read access.")
                                      :write
                                      (:type array
                                       :items (:type string)
                                       :description "Absolute paths requiring write access.")))))
           (justification string :optional
                          "Concise user-facing reason for a non-default permission request."))
    :async-p t
    :max-result-size 30000
    :groups (eval)
    :check-permission-async #'mevedel-tool-exec--check-permission-async
    :get-pattern (lambda (input) (plist-get input :command))
    :renderer #'mevedel-tool-exec--render-bash)

  (mevedel-define-tool
    :name "WriteStdin"
    :description "Poll unread output or send input to a yielded Bash execution."
    :handler #'mevedel-tool-exec--write-stdin
    :args ((execution_id string :required
                         "Opaque execution ID returned by Bash.")
           (chars string :optional
                  "Input to send. Omit or use an empty string to poll. Ordinary input requires a PTY; a single Ctrl-C character interrupts either mode.")
           (yield_time_ms integer :optional
                          "Wait before returning: polls default to 5000ms (5000-300000); input defaults to 250ms (250-30000)."))
    :async-p t
    :max-result-size 30000
    :groups (eval)
    :renderer #'mevedel-tool-exec--render-bash)

  (mevedel-define-tool
    :name "ListExecutions"
    :description "List yielded Bash executions owned by this agent."
    :handler #'mevedel-tool-exec--list-executions
    :args ()
    :read-only-p t
    :groups (eval))

  (mevedel-define-tool
    :name "StopExecution"
    :description "Stop one yielded Bash execution owned by this agent."
    :handler #'mevedel-tool-exec--stop-execution
    :args ((execution_id string :required
                         "Opaque execution ID returned by Bash."))
    :async-p t
    :max-result-size 30000
    :groups (eval)
    :renderer #'mevedel-tool-exec--render-bash)

  (mevedel-define-tool
    :name "Eval"
    :description "Evaluate an Elisp expression and return the result."
    :prompt-file "tools/eval.md"
    :handler #'mevedel-tool-exec--eval
    :args ((expression string :required "A single elisp sexp to evaluate with default-directory set to the session working directory.")
           (mode string :optional "Execution mode: live (default) evaluates in the current Emacs; batch evaluates in a child emacs --batch process."
                 :enum ["live" "batch"])
           (preserve_ui boolean :optional "In live mode, restore the current window configuration after evaluation. Defaults to true.")
           (sandbox_permissions string :optional
                                "Batch child-execution authority: use_default, with_additional_permissions, or require_escalated for a complete confinement bypass."
                                :enum ["use_default"
                                       "with_additional_permissions"
                                       "require_escalated"])
           (additional_permissions object :optional
                                   "Capabilities requested in addition to default batch confinement."
                                   :properties
                                   (:network
                                    (:type boolean
                                     :description "Allow network access for this batch invocation.")
                                    :file_system
                                    (:type object
                                     :description "Exact filesystem paths to reopen inside confinement."
                                     :properties
                                     (:read
                                      (:type array
                                       :items (:type string)
                                       :description "Absolute paths requiring read access.")
                                      :write
                                      (:type array
                                       :items (:type string)
                                       :description "Absolute paths requiring write access.")))))
           (justification string :optional
                          "Concise user-facing reason for a non-default batch permission request."))
    :async-p t
    :max-result-size 30000
    :groups (eval)
    :check-permission-async #'mevedel-tool-exec--eval-check-permission-async
    :renderer #'mevedel-tool-exec--render-eval))

(provide 'mevedel-tool-exec)
;;; mevedel-tool-exec.el ends here
