;;; mevedel-compact.el -- Compaction command and gptel gate -*- lexical-binding: t -*-

;;; Commentary:

;; Public compaction command and gptel request gate.  Estimation, transcript
;; evidence, target mutation, and asynchronous settlement have separate owners.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'gptel))
(require 'mevedel-compact-estimation)
(require 'mevedel-compact-evidence)
(require 'mevedel-compact-run)
(require 'mevedel-compact-target)
(require 'mevedel-transcript)

;; `gptel'
(declare-function gptel--update-status "ext:gptel" (msg &optional face))
(declare-function gptel-backend-name "ext:gptel" (backend))
(declare-function gptel-mode "ext:gptel" (&optional arg))
(defvar gptel-mode)

;; `gptel-request'
(declare-function gptel--create-prompt-buffer
                  "ext:gptel-request" (&optional prompt-end))
(declare-function gptel--fsm-transition
                  "ext:gptel-request" (machine &optional new-state))
(declare-function gptel--handle-wait "ext:gptel-request" (fsm))
(declare-function gptel--realize-query "ext:gptel-request" (fsm))
(declare-function gptel-fsm-info "ext:gptel-request" (cl-x) t)
(defvar gptel--request-params)
(defvar gptel-backend)
(defvar gptel-max-tokens)
(defvar gptel-model)
(defvar gptel-reasoning-effort)
(defvar gptel-tools)
(defvar gptel-use-tools)

;; `mevedel-agent-conversation'
(declare-function mevedel-agent-conversation-record-activity
                  "mevedel-agent-conversation"
                  (invocation item &optional suppress-rerender))

;; `mevedel-agent-exec'
(declare-function mevedel-agent-exec-request-snapshot
                  "mevedel-agent-exec" (policy))
(autoload 'mevedel-agent-exec-request-snapshot "mevedel-agent-exec")

;; `mevedel-agents'
(declare-function mevedel-agent-invocation-p "mevedel-agents" (cl-x))

;; `mevedel-chat'
(declare-function mevedel--active-chat-buffer
                  "mevedel-chat" (&optional workspace))

;; `mevedel-compact-estimation'
(declare-function mevedel-compact-estimation-admission
                  "mevedel-compact-estimation" (estimate target-policy))
(declare-function mevedel-compact-estimation-baseline-source
                  "mevedel-compact-estimation" ())
(declare-function mevedel-compact-estimation-estimate-data-tokens
                  "mevedel-compact-estimation" (data))
(declare-function mevedel-compact-estimation-estimate-tokens
                  "mevedel-compact-estimation" ())
(declare-function mevedel-compact-estimation-estimate-transformed-request-tokens
                  "mevedel-compact-estimation" (source-buffer prompt-buffer))
(declare-function mevedel-compact-estimation-policy-threshold-tokens
                  "mevedel-compact-estimation" (policy))
(declare-function mevedel-compact-estimation-summary-request-p
                  "mevedel-compact-estimation" (info))
(declare-function mevedel-compact-estimation-target-policy
                  "mevedel-compact-estimation" ())
(declare-function mevedel-compact-estimation-telemetry-inputs
                  "mevedel-compact-estimation" (estimate target-policy))

;; `mevedel-compact-evidence'
(declare-function mevedel-compact-evidence-current-tool-batch-start
                  "mevedel-compact-evidence" (info body-start))
(declare-function mevedel-compact-evidence-find-boundary
                  "mevedel-compact-evidence" ())
(declare-function mevedel-compact-evidence-insert-current-request-reminder
                  "mevedel-compact-evidence" (body))
(declare-function mevedel-compact-evidence-rebuild-prompt-buffer
                  "mevedel-compact-evidence"
                  (prompt-buffer source-buffer source-pending-text
                                 prompt-history-start prompt-pending-start))

;; `mevedel-compact-run'
(declare-function mevedel-compact-run-start
                  "mevedel-compact-run" (&rest keys))
(defvar mevedel-compact-run-cancel)
(defvar mevedel-compact-run-failure-count)
(defvar mevedel-compact-run-in-flight)

;; `mevedel-compact-target'
(declare-function mevedel-compact-target-agent-target
                  "mevedel-compact-target" (invocation))
(declare-function mevedel-compact-target-call
                  "mevedel-compact-target" (target operation &rest args))
(declare-function mevedel-compact-target-current-persisted-p
                  "mevedel-compact-target" ())
(declare-function mevedel-compact-target-main-target
                  "mevedel-compact-target" ())
(defvar mevedel-compact-target-current-request-hook-context)
(defvar mevedel-compact-target-current-request-reminder)

;; `mevedel-goal'
(declare-function mevedel-goal-pause-runtime-failure
                  "mevedel-goal" (buffer reason))

;; `mevedel-session-persistence'
(defvar mevedel-session--read-only-mode)

;; `mevedel-structs'
(declare-function mevedel-request-id "mevedel-structs" (cl-x))
(declare-function mevedel-session-save-path "mevedel-structs" (cl-x) t)
(defvar mevedel--data-buffer)
(defvar mevedel--session)
(defvar mevedel--view-buffer)

;; `mevedel-telemetry'
(declare-function mevedel-telemetry-record
                  "mevedel-telemetry" (session event &rest props))

;; `mevedel-tools'
(declare-function mevedel-tools--handle-steering-inject
                  "mevedel-tools" (fsm &optional skip-compaction-gate))
(autoload 'mevedel-tools--handle-steering-inject "mevedel-tools")

;; `mevedel-utilities'
(declare-function mevedel--warn-once
                  "mevedel-utilities" (key format &rest args))

;; `mevedel-view-composer'
(declare-function mevedel-view--assert-live-tip
                  "mevedel-view-composer" (&optional allow-armed-fork))
(autoload 'mevedel-view--assert-live-tip "mevedel-view-composer")

;; `mevedel-view-stream'
(declare-function mevedel-view--stop-request-progress
                  "mevedel-view-stream" ())
(declare-function mevedel-view--stop-spinner "mevedel-view-stream" ())
(declare-function mevedel-view--update-spinner
                  "mevedel-view-stream" (status &optional owner))

;; `mevedel-workspace'
(defvar mevedel--workspace)


(defun mevedel--compact-provider-wait (fsm)
  "Record and dispatch the provider request represented by FSM."
  (let* ((info (and fsm (gptel-fsm-info fsm)))
         (chat-buffer (and (listp info) (plist-get info :buffer))))
    (when (and (buffer-live-p chat-buffer)
               (fboundp 'mevedel-telemetry-record))
      (with-current-buffer chat-buffer
        (when (bound-and-true-p mevedel--session)
          (mevedel-telemetry-record
           mevedel--session 'provider-dispatch
           :request-id (plist-get info :mevedel-request-id)
           :backend (let ((backend (plist-get info :backend)))
                      (or (and backend
                               (ignore-errors
                                 (gptel-backend-name backend)))
                          (and (boundp 'gptel-backend)
                               (ignore-errors
                                 (gptel-backend-name gptel-backend)))))
           :model (or (plist-get info :model)
                      (and (boundp 'gptel-model) gptel-model))
           :effort (or (plist-get info :reasoning-effort)
                       (and (boundp 'gptel-reasoning-effort)
                            gptel-reasoning-effort))
           :continuation (and (mevedel--compact-continuation-wait-p fsm)
                              t)))))
    (gptel--handle-wait fsm)))


(defcustom mevedel-compact-auto t
  "Whether mevedel automatically compacts persisted sessions."
  :type 'boolean
  :group 'mevedel)

(defvar-local mevedel--compact-auto-disabled nil
  "Non-nil when auto-compaction is disabled for this session.")


(defun mevedel--compact-auto-eligible-p ()
  "Return non-nil when automatic compaction may run in this buffer."
  (and mevedel-compact-auto
       (not mevedel--compact-auto-disabled)
       (not mevedel-compact-run-in-flight)
       (not (bound-and-true-p mevedel-session--read-only-mode))
       (mevedel-compact-target-current-persisted-p)))

(defun mevedel--compact-auto-ineligible-reason ()
  "Return a short reason automatic compaction cannot run, or nil."
  (cond
   ((not mevedel-compact-auto) "auto-compaction is disabled")
   (mevedel--compact-auto-disabled "auto-compaction is disabled after repeated failures")
   (mevedel-compact-run-in-flight "compaction is already in progress")
   ((bound-and-true-p mevedel-session--read-only-mode) "session is read-only")
   ((not (and (boundp 'mevedel--session)
              mevedel--session
              (mevedel-session-save-path mevedel--session)))
    "session is not materialized on disk")
    ((not (mevedel-compact-target-current-persisted-p))
     "current buffer is not the active persisted segment")))

(defun mevedel--compact-should-compact-p (&optional token-estimate)
  "Return automatic compaction admission for TOKEN-ESTIMATE, or nil."
  (let* ((estimate (or token-estimate (mevedel-compact-estimation-estimate-tokens)))
         (target-policy (mevedel-compact-estimation-target-policy))
         (admission
          (mevedel-compact-estimation-admission estimate target-policy))
         (eligible (and admission (mevedel--compact-auto-eligible-p)))
         (ineligible-reason (and admission
                                 (not eligible)
                                 (mevedel--compact-auto-ineligible-reason)))
         (decision
          (cond
           ((not admission) nil)
           (eligible admission)
           (t
            (mevedel--warn-once
             (list 'compact-auto-ineligible (buffer-name))
             "Auto-compaction skipped: %s"
             (or ineligible-reason "session is not eligible"))
            nil))))
    (when (and (bound-and-true-p mevedel--session)
               (fboundp 'mevedel-telemetry-record))
      (apply #'mevedel-telemetry-record
             mevedel--session 'compaction-threshold-evaluated
             :estimate estimate
             :estimate-source
             (mevedel-compact-estimation-baseline-source)
             :target-threshold
             (mevedel-compact-estimation-policy-threshold-tokens target-policy)
             :summary-threshold
             (and admission
                  (mevedel-compact-estimation-policy-threshold-tokens
                   (plist-get admission :summary-policy)))
             :admitted (and decision t)
             :target-pressure (and admission
                                   (plist-get admission :target-pressure))
             :ineligible-reason ineligible-reason
             (mevedel-compact-estimation-telemetry-inputs estimate target-policy)))
    decision))

;;;###autoload
(defun mevedel-compact (&optional aggressive instructions)
  "Compact the current mevedel chat buffer.
With prefix argument AGGRESSIVE, compact without preserving a recent
  tail.  INSTRUCTIONS is an optional string of manual summary guidance."
  (interactive "P")
  (when (bound-and-true-p mevedel--data-buffer)
    (mevedel-view--assert-live-tip))
  (let* ((chat-buffer
          (cond
           ((and (bound-and-true-p gptel-mode) (bound-and-true-p mevedel--workspace))
            (current-buffer))
           (t (mevedel--active-chat-buffer)))))
    (unless (and chat-buffer (buffer-live-p chat-buffer))
      (user-error "No mevedel chat buffer found"))
    (with-current-buffer chat-buffer
      (mevedel-compact-run-start
       :aggressive aggressive
       :instructions instructions))))

(defun mevedel--compact-auto-failure (chat-buffer err)
  "Surface automatic compaction failure ERR for CHAT-BUFFER."
  (when (buffer-live-p chat-buffer)
    (with-current-buffer chat-buffer
      (when (>= mevedel-compact-run-failure-count 3)
        (setq mevedel--compact-auto-disabled t))
      (display-warning
       'mevedel
       (format
        (if mevedel--compact-auto-disabled
            "Auto-compaction disabled after repeated failures; request not sent: %s"
          "Auto-compaction failed; request not sent: %s")
        err)
       :warning)
      (when (fboundp 'gptel--update-status)
        (gptel--update-status " Compaction failed" 'error))
      (when-let* ((vb mevedel--view-buffer)
                  (_ (buffer-live-p vb)))
        (with-current-buffer vb
          (if (fboundp 'mevedel-view--stop-request-progress)
              (mevedel-view--stop-request-progress)
            (mevedel-view--stop-spinner)))))))

(defun mevedel--compact-continuation-wait-p (fsm)
  "Return non-nil when FSM is entering WAIT for a tool continuation."
  (when-let* ((info (and fsm (gptel-fsm-info fsm))))
    (and (not (mevedel-compact-estimation-summary-request-p info))
         (or (eq (car (plist-get info :history)) 'TRET)
             (plist-get info :tool-result)))))

(defun mevedel--compact-rebuild-info-data-from-buffer (fsm chat-buffer)
  "Rebuild realized request data for FSM from CHAT-BUFFER.

The rebuilt data keeps the effective backend, model, and active tool
set already stored on FSM's info plist."
  (let* ((info (gptel-fsm-info fsm))
         (old-data (plist-get info :data))
         (had-dry-run (plist-member info :dry-run))
         (old-dry-run (plist-get info :dry-run))
         (backend (plist-get info :backend))
         (model (plist-get info :model))
         (tools (plist-get info :tools))
         (request-reminder
          (buffer-local-value 'mevedel-compact-target-current-request-reminder
                              chat-buffer))
         (prompt-buffer nil))
    (condition-case err
        (unwind-protect
            (progn
              (with-current-buffer chat-buffer
                (save-excursion
                  (goto-char (point-max))
                  (let ((mark-active nil))
                    (setq prompt-buffer
                          (gptel--create-prompt-buffer (point))))))
              (with-current-buffer prompt-buffer
                (mevedel-compact-evidence-insert-current-request-reminder
                 request-reminder)
                (when backend
                  (setq-local gptel-backend backend))
                (when model
                  (setq-local gptel-model model))
                (when (plist-member info :tools)
                  (setq-local gptel-tools tools)
                  (setq-local gptel-use-tools (and tools t))))
              (plist-put info :data prompt-buffer)
              (plist-put info :dry-run t)
              (gptel--realize-query fsm))
          (when (buffer-live-p prompt-buffer)
            (kill-buffer prompt-buffer))
          (when (buffer-live-p chat-buffer)
            (with-current-buffer chat-buffer
              (setq mevedel-compact-target-current-request-reminder nil
                    mevedel-compact-target-current-request-hook-context nil)))
          (if had-dry-run
              (plist-put info :dry-run old-dry-run)
            (cl-remf info :dry-run)))
      (error
       (plist-put info :data old-data)
       (signal (car err) (cdr err))))))

(defun mevedel--compact-main-resume-status (target)
  "Show ordinary request progress after compacting main TARGET."
  (let ((chat-buffer (plist-get target :buffer)))
    (when (buffer-live-p chat-buffer)
      (with-current-buffer chat-buffer
        (when-let* ((view-buffer mevedel--view-buffer)
                    (_ (buffer-live-p view-buffer)))
          (with-current-buffer view-buffer
            (mevedel-view--update-spinner "Thinking...")))))))

(defun mevedel--compact-target-resume (target fsm)
  "Rebuild FSM from compacted TARGET and resume its continuation once."
  (let ((buffer (plist-get target :buffer))
        (info (gptel-fsm-info fsm)))
    (unless (buffer-live-p buffer)
      (error "Compaction target buffer is no longer live"))
    (with-current-buffer buffer
      (when-let* ((marker (plist-get info :position)))
        (set-marker marker (point-max) buffer))
      (when-let* ((status-function (plist-get target :resume-status)))
        (funcall status-function target)))
    (mevedel--compact-rebuild-info-data-from-buffer fsm buffer)
    (mevedel--compact-target-provider-wait target fsm)))

(defun mevedel--compact-target-provider-wait (target fsm)
  "Dispatch FSM after TARGET compaction, including deferred root steering."
  (if (plist-get target :invocation)
      (mevedel--compact-provider-wait fsm)
    (mevedel-tools--handle-steering-inject fsm t)
    (unless (plist-get (gptel-fsm-info fsm) :mevedel-pending-input-hold)
      (mevedel--compact-provider-wait fsm))))

(defun mevedel--compact-agent-terminal-failure (target fsm err)
  "Terminate agent FSM with compaction failure ERR."
  (let* ((info (gptel-fsm-info fsm))
         (invocation
          (or (plist-get target :invocation)
              (plist-get info :mevedel-agent-invocation))))
    (when (mevedel-agent-invocation-p invocation)
      (mevedel-agent-conversation-record-activity
       invocation '(:type status :summary "error")))
    (gptel--update-status " Agent failed" 'error)
    (plist-put info :status (format "Compaction failed: %s" err))
    (plist-put info :error
               (list :type "compaction_error" :message (format "%s" err)))
    (gptel--fsm-transition fsm 'ERRS)))

(defun mevedel--compact-main-failure (target _fsm err)
  "Surface automatic main TARGET compaction failure ERR."
  (let ((buffer (plist-get target :buffer)))
    (mevedel-goal-pause-runtime-failure
     buffer (format "Compaction failed: %s" err))
    (mevedel--compact-auto-failure buffer err)))

(defun mevedel--compact-handle-target-wait
    (fsm target admission &optional pending-start)
  "Gate FSM continuation through TARGET using precomputed ADMISSION.
PENDING-START, when non-nil, begins the continuation batch that must survive."
  (if (not admission)
      (mevedel--compact-provider-wait fsm)
    (let ((pending-start (or pending-start
                             (mevedel-compact-evidence-find-boundary))))
      (if (not pending-start)
          (if (plist-get admission :target-pressure)
              (mevedel-compact-target-call
               target :fail fsm
               "No compactable history remains at target pressure")
            (mevedel--compact-target-provider-wait target fsm))
        (mevedel-compact-run-start
         :pending-start pending-start
         :auto t
         :admission admission
         :target target
         :callback
         (lambda (err)
           (cond
            ((eq err :skip)
             (mevedel--compact-target-provider-wait target fsm))
            (err
             (mevedel-compact-target-call target :fail fsm err))
            (t
             (condition-case rebuild-err
                 (mevedel-compact-target-call target :resume fsm)
               (error
                (mevedel-compact-target-call
                 target :fail fsm
                 (error-message-string rebuild-err))))))))))))

(defun mevedel--compact-handle-agent-wait (fsm)
  "Run persisted-agent compaction before a continuation request in FSM."
  (let* ((info (and fsm (gptel-fsm-info fsm)))
         (agent-buffer (and (listp info) (plist-get info :buffer)))
         (invocation
          (and (listp info) (plist-get info :mevedel-agent-invocation))))
    (if (or (not (mevedel--compact-continuation-wait-p fsm))
            (not (buffer-live-p agent-buffer))
            (not (mevedel-agent-invocation-p invocation)))
        (mevedel--compact-provider-wait fsm)
      (with-current-buffer agent-buffer
        (let* ((target (mevedel-compact-target-agent-target invocation))
               (target-policy
                (or (plist-get info :mevedel-compaction-target-policy)
                    (mevedel-compact-estimation-target-policy)))
               (estimate
                (mevedel-compact-estimation-estimate-data-tokens
                 (plist-get info :data)))
               (auto-ready
                (and mevedel-compact-auto
                     (not mevedel--compact-auto-disabled)
                     (not mevedel-compact-run-in-flight)))
               (admission
                (and target
                     auto-ready
                     (mevedel-compact-estimation-admission
                      estimate target-policy))))
          (cond
           (target
            (mevedel--compact-handle-target-wait
             fsm target admission
             (mevedel-compact-evidence-current-tool-batch-start
              info (plist-get target :body-start))))
           ((and auto-ready
                 (>= estimate
                     (mevedel-compact-estimation-policy-threshold-tokens
                      target-policy)))
            (mevedel--compact-agent-terminal-failure
             nil fsm
             "Agent transcript is not eligible for compaction at target pressure"))
           (t
            (mevedel--compact-provider-wait fsm))))))))

(defun mevedel--compact-main-wait-decision (fsm)
  "Return the main continuation compaction decision for FSM, or nil."
  (let* ((info (and fsm (gptel-fsm-info fsm)))
         (chat-buffer (and (listp info) (plist-get info :buffer))))
    (when (and (mevedel--compact-continuation-wait-p fsm)
               (buffer-live-p chat-buffer))
      (with-current-buffer chat-buffer
        (let* ((target-policy
                (or (plist-get info :mevedel-compaction-target-policy)
                    (let ((gptel-backend
                           (or (plist-get info :backend) gptel-backend))
                          (gptel-model
                           (or (plist-get info :model) gptel-model)))
                      (mevedel-compact-estimation-target-policy))))
               (token-estimate
                (mevedel-compact-estimation-estimate-data-tokens
                 (plist-get info :data)))
               (gptel-backend (plist-get target-policy :backend))
               (gptel-model (plist-get target-policy :model))
               (gptel-reasoning-effort
                (plist-get target-policy :effort))
               (gptel-max-tokens (plist-get target-policy :max-tokens))
               (gptel--request-params
                (plist-get target-policy :request-params)))
          (list :admission
                (mevedel--compact-should-compact-p token-estimate)
                :target-policy target-policy))))))

(defun mevedel--compact-defer-steering-p (fsm)
  "Cache FSM's compaction decision and return non-nil when steering must wait."
  (when-let* ((decision (mevedel--compact-main-wait-decision fsm)))
    (setf (gptel-fsm-info fsm)
          (plist-put (gptel-fsm-info fsm)
                     :mevedel-compaction-wait-decision decision))
    (plist-get decision :admission)))

(defun mevedel--compact-handle-wait (fsm)
  "Run continuation auto-compaction for FSM before `gptel--handle-wait'."
  (let* ((info (and fsm (gptel-fsm-info fsm)))
         (cached (and (listp info)
                      (plist-get info :mevedel-compaction-wait-decision))))
    (if (plist-get info :mevedel-pending-input-hold)
        nil
      (let ((decision
             (or cached (mevedel--compact-main-wait-decision fsm))))
        (when cached
          (cl-remf info :mevedel-compaction-wait-decision)
          (setf (gptel-fsm-info fsm) info))
        (if (not decision)
            (mevedel--compact-provider-wait fsm)
          (let* ((chat-buffer (plist-get info :buffer))
                 (target-policy (plist-get decision :target-policy)))
            (with-current-buffer chat-buffer
              (let ((gptel-backend (plist-get target-policy :backend))
                    (gptel-model (plist-get target-policy :model))
                    (gptel-reasoning-effort
                     (plist-get target-policy :effort))
                    (gptel-max-tokens
                     (plist-get target-policy :max-tokens))
                    (gptel--request-params
                     (plist-get target-policy :request-params)))
                (mevedel--compact-handle-target-wait
                 fsm (mevedel-compact-target-main-target)
                 (plist-get decision :admission))))))))))

(defun mevedel--compact-transform-auto (continue fsm)
  "Run auto-compaction before request realization.
CONTINUE is gptel's async transform continuation.  FSM is the request
state machine."
  (let* ((info (and fsm (gptel-fsm-info fsm)))
         (source-buffer (and (listp info) (plist-get info :buffer)))
         (prompt-buffer (current-buffer))
         (effective-backend gptel-backend)
         (effective-model gptel-model)
         (effective-max-tokens gptel-max-tokens)
         (effective-request-params gptel--request-params)
         (context (and (listp info) (plist-get info :context)))
         (late-tail-start (copy-marker (point-max) nil)))
    (when (listp info)
      (plist-put
       info :mevedel-request-locals
       (mevedel-agent-exec-request-snapshot
        (list :backend gptel-backend
              :model gptel-model
              :effort (and (boundp 'gptel-reasoning-effort)
                           gptel-reasoning-effort)))))
    (cl-labels
        ((continue-with-snapshot
           (&optional context-tail-end)
           (when (and (listp info) (buffer-live-p prompt-buffer))
             (with-current-buffer prompt-buffer
               (let ((tail-start (marker-position late-tail-start))
                     (tail-end
                      (if (and (markerp context-tail-end)
                               (marker-buffer context-tail-end))
                          (marker-position context-tail-end)
                        (point-max))))
                 (plist-put
                  info :mevedel-model-context
                  (if (and tail-start (<= tail-start tail-end))
                      (concat (buffer-substring (point-min) tail-start)
                              (buffer-substring tail-end (point-max)))
                    (buffer-string))))))
           (unwind-protect
               (funcall continue)
             (set-marker late-tail-start nil))))
      (if (or (and (listp context)
                   (plist-get context :mevedel-context-summary))
              (not (buffer-live-p source-buffer)))
          (continue-with-snapshot)
        (with-current-buffer source-buffer
          (let ((target-policy
                 (let ((gptel-backend effective-backend)
                       (gptel-model effective-model)
                       (gptel-max-tokens effective-max-tokens)
                       (gptel--request-params effective-request-params))
                   (mevedel-compact-estimation-target-policy)))
                (admission
                 (unless mevedel-compact-run-in-flight
                   (let ((gptel-backend effective-backend)
                         (gptel-model effective-model)
                         (gptel-max-tokens effective-max-tokens)
                         (gptel--request-params effective-request-params))
                     (mevedel--compact-should-compact-p
                      (mevedel-compact-estimation-estimate-transformed-request-tokens
                       source-buffer prompt-buffer))))))
            (plist-put info :mevedel-compaction-target-policy target-policy)
            (cond
             (mevedel-compact-run-in-flight
              (user-error "Compaction already in progress"))
             ((not admission)
              (continue-with-snapshot))
             (t
              (let ((pending-start (mevedel-compact-evidence-find-boundary)))
                (if (not pending-start)
                    (if (plist-get admission :target-pressure)
                        (mevedel--compact-auto-failure
                         source-buffer
                         "No compactable history remains at target pressure")
                      (continue-with-snapshot))
                  (let ((source-pending-text
                         (buffer-substring pending-start (point-max)))
                        (prompt-history-start
                         (when (buffer-live-p prompt-buffer)
                           (with-current-buffer prompt-buffer
                             (copy-marker (point-min) t))))
                        (prompt-pending-start
                         (when (buffer-live-p prompt-buffer)
                           (with-current-buffer prompt-buffer
                             (when-let* ((start (mevedel-compact-evidence-find-boundary)))
                               (copy-marker start nil))))))
                    (mevedel-compact-run-start
                     :pending-start pending-start
                     :auto t
                     :admission admission
                     :callback
                     (lambda (err)
                       (unwind-protect
                           (cond
                            ((eq err :skip)
                             (continue-with-snapshot))
                            (err
                             (mevedel--compact-auto-failure source-buffer err))
                            (t
                             (let (context-tail-end)
                               (when (and (buffer-live-p prompt-buffer)
                                          (buffer-live-p source-buffer))
                                 (with-current-buffer source-buffer
                                   (when-let* ((marker (plist-get info :position)))
                                     (set-marker marker (point-max) source-buffer)))
                                 (when-let* ((vb mevedel--view-buffer)
                                             (_ (buffer-live-p vb)))
                                   (with-current-buffer vb
                                     (mevedel-view--update-spinner "Thinking...")))
                                 (mevedel-compact-evidence-rebuild-prompt-buffer
                                  prompt-buffer source-buffer source-pending-text
                                  prompt-history-start prompt-pending-start)
                                 (setq context-tail-end
                                       (with-current-buffer prompt-buffer
                                         (copy-marker (point-max) nil)))
                                 (when-let* ((hook-context
                                              (buffer-local-value
                                               'mevedel-compact-target-current-request-hook-context
                                               source-buffer)))
                                   (with-current-buffer prompt-buffer
                                     (goto-char (point-max))
                                     (unless (bolp) (insert "\n"))
                                     (insert "\n" hook-context "\n"))
                                   (with-current-buffer source-buffer
                                     (setq mevedel-compact-target-current-request-hook-context
                                           nil)))
                                 (when-let* ((reminder
                                              (buffer-local-value
                                               'mevedel-compact-target-current-request-reminder
                                               source-buffer)))
                                   (with-current-buffer prompt-buffer
                                     (mevedel-compact-evidence-insert-current-request-reminder
                                      reminder))
                                   (with-current-buffer source-buffer
                                     (setq mevedel-compact-target-current-request-reminder
                                           nil))))
                               (unwind-protect
                                   (continue-with-snapshot context-tail-end)
                                 (when (markerp context-tail-end)
                                   (set-marker context-tail-end nil))))))
                         (when (markerp prompt-history-start)
                           (set-marker prompt-history-start nil))
                         (when (markerp prompt-pending-start)
                           (set-marker prompt-pending-start nil))))))))))))))))

(provide 'mevedel-compact)
;;; mevedel-compact.el ends here
