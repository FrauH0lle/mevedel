;;; niwa-compact.el -- Compaction command and gptel gate -*- lexical-binding: t -*-

;;; Commentary:

;; Public compaction command and gptel request gate.  Estimation, transcript
;; evidence, target mutation, and asynchronous settlement have separate owners.

;;; Code:

(require 'niwa-telemetry)
(require 'niwa-utilities)

(require 'niwa-engine)

(require 'niwa-context-delivery)

(eval-when-compile
  (require 'cl-lib)
  (require 'gptel))
(require 'niwa-compact-estimation)
(require 'niwa-compact-evidence)
(require 'niwa-compact-run)
(require 'niwa-compact-target)
(require 'niwa-history)
(require 'niwa-transcript)

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
;; `niwa-auth'
(declare-function gptel-fsm-info "ext:gptel-request" (cl-x) t)
(defvar gptel--request-params)
(defvar gptel-backend)
(defvar gptel-max-tokens)
(defvar gptel-model)
(defvar gptel-reasoning-effort)
(defvar gptel-tools)
(defvar gptel-use-tools)

;; `niwa-agent-conversation'
(declare-function niwa-agent-conversation-record-activity
                  "niwa-agent-conversation"
                  (invocation item &optional suppress-rerender))

;; `niwa-agent-exec'
(declare-function niwa-agent-exec-request-snapshot
                  "niwa-agent-exec" (policy))
(autoload 'niwa-agent-exec-request-snapshot "niwa-agent-exec")

;; `niwa-agents'
(declare-function niwa-agent-invocation-p "niwa-agents" (cl-x))

;; `niwa-chat'
(declare-function niwa--active-chat-buffer
                  "niwa-chat" (&optional workspace))

;; `niwa-goal'
(declare-function niwa-goal-pause-runtime-failure
                  "niwa-goal" (buffer reason))

;; `niwa-gptel-retry'
(declare-function niwa-gptel-retry-dispatch "niwa-gptel-retry" (fsm))
(autoload 'niwa-gptel-retry-dispatch "niwa-gptel-retry")

;; `niwa-reminders'
(declare-function niwa-reminders--handle-inject
                  "niwa-reminders" (fsm))
(declare-function niwa-reminders-stage-entry
                  "niwa-reminders" (fsm type body &optional commit))
(declare-function niwa-reminders-stage-plan-mode
                  "niwa-reminders" (fsm))
(declare-function niwa-reminders-prepare-edited-files
                  "niwa-reminders" (fsm continuation))
(autoload 'niwa-reminders--handle-inject "niwa-reminders")
(autoload 'niwa-reminders-prepare-edited-files "niwa-reminders")
(autoload 'niwa-reminders-stage-entry "niwa-reminders")
(autoload 'niwa-reminders-stage-plan-mode "niwa-reminders")

;; `niwa-session-persistence'
(defvar niwa-session--read-only-mode)

;; `niwa-turn'
(defvar niwa--current-request)
(declare-function niwa-request-push-canceller
                  "niwa-turn" (request canceller))
(autoload 'niwa-request-push-canceller "niwa-turn")

;; `niwa-structs'
(declare-function niwa-request-id "niwa-structs" (cl-x))
(declare-function niwa-session-save-path "niwa-structs" (cl-x) t)
(defvar niwa--data-buffer)
(defvar niwa--session)
(defvar niwa--view-buffer)

;; `niwa-telemetry'
(declare-function niwa-telemetry-record
                  "niwa-telemetry" (session event &rest props))

;; `niwa-tools'
(declare-function niwa-tools--handle-steering-inject
                  "niwa-tools" (fsm &optional skip-compaction-gate))
(autoload 'niwa-tools--handle-steering-inject "niwa-tools")

;; `niwa-utilities'
(declare-function niwa--warn-once
                  "niwa-utilities" (scope key format &rest args))

;; `niwa-view-composer'
(declare-function niwa-view--assert-live-tip
                  "niwa-view-composer" (&optional allow-armed-fork))
(autoload 'niwa-view--assert-live-tip "niwa-view-composer")

;; `niwa-view-stream'
(declare-function niwa-view-stream-end-progress
                  "niwa-view-stream" (view))
(declare-function niwa-view-stream-show-progress
                  "niwa-view-stream" (view status &optional owner))
(autoload 'niwa-view-stream-end-progress "niwa-view-stream")
(autoload 'niwa-view-stream-show-progress "niwa-view-stream")

;; `niwa-workspace'
(defvar niwa--workspace)


(defun niwa--compact-provider-wait (fsm)
  "Inject reminders, record, and dispatch FSM's provider request."
  (let* ((info (and fsm (gptel-fsm-info fsm)))
         (chat-buffer (and (listp info) (plist-get info :buffer))))
    (when (buffer-live-p chat-buffer)
      (with-current-buffer chat-buffer
        (when (bound-and-true-p niwa--session)
          (niwa-telemetry-record
           niwa--session 'provider-dispatch
           :request-id (plist-get info :niwa-request-id)
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
           :continuation (and (niwa--compact-continuation-wait-p fsm)
                              t)))))
    ;; Edited-file diffs are helper processes, prepared before staging so
    ;; this transition never waits on one.  A cancelled request stops a
    ;; pending preparation and ends the turn, since nothing else would move
    ;; a machine that has not yet sent its request out of WAIT.
    (let ((cancel (niwa-reminders-prepare-edited-files
                   fsm #'niwa--compact-provider-dispatch))
          (request (and (listp info)
                        (or (plist-get info :niwa-request)
                            (and (buffer-live-p chat-buffer)
                                 (buffer-local-value 'niwa--current-request
                                                     chat-buffer))))))
      (when (and cancel request)
        (niwa-request-push-canceller
         request
         (lambda ()
           (when (funcall cancel)
             (gptel--fsm-transition fsm 'ABRT))))))))

(defun niwa--compact-provider-dispatch (fsm)
  "Stage context and reminders into FSM's payload, then send it."
  ;; This is the final dispatch seam for both ordinary and rebuilt
  ;; continuation payloads.  Committing reminders any earlier lets
  ;; successful auto-compaction discard their injected message.
  (niwa-context-delivery-stage fsm)
  (niwa-reminders--handle-inject fsm)
  (niwa-history-note-dispatch (gptel-fsm-info fsm))
  (niwa-gptel-retry-dispatch fsm))


(defcustom niwa-compact-auto t
  "Whether niwa automatically compacts persisted sessions."
  :type 'boolean
  :group 'niwa)

(defvar-local niwa--compact-auto-disabled nil
  "Non-nil when auto-compaction is disabled for this session.")


(defun niwa--compact-auto-eligible-p ()
  "Return non-nil when automatic compaction may run in this buffer."
  (and niwa-compact-auto
       (not niwa--compact-auto-disabled)
       (not niwa-compact-run-in-flight)
       (not (bound-and-true-p niwa-session--read-only-mode))
       (niwa-compact-target-current-persisted-p)))

(defun niwa--compact-auto-ineligible-reason ()
  "Return a short reason automatic compaction cannot run, or nil."
  (cond
   ((not niwa-compact-auto) "auto-compaction is disabled")
   (niwa--compact-auto-disabled "auto-compaction is disabled after repeated failures")
   (niwa-compact-run-in-flight "compaction is already in progress")
   ((bound-and-true-p niwa-session--read-only-mode) "session is read-only")
   ((not (and (boundp 'niwa--session)
              niwa--session
              (niwa-session-save-path niwa--session)))
    "session is not materialized on disk")
    ((not (niwa-compact-target-current-persisted-p))
     "current buffer is not the active persisted segment")))

(defun niwa--compact-should-compact-p (&optional token-estimate)
  "Return automatic compaction admission for TOKEN-ESTIMATE, or nil."
  (let* ((estimate (or token-estimate (niwa-compact-estimation-estimate-tokens)))
         (target-policy (niwa-compact-estimation-target-policy))
         (admission
          (niwa-compact-estimation-admission estimate target-policy))
         (eligible (and admission (niwa--compact-auto-eligible-p)))
         (ineligible-reason (and admission
                                 (not eligible)
                                 (niwa--compact-auto-ineligible-reason)))
         (decision
          (cond
           ((not admission) nil)
           (eligible admission)
           (t
            (niwa--warn-once
             (niwa-diagnostic-scope (current-buffer))
             (list 'compact-auto-ineligible (buffer-name))
             "Auto-compaction skipped: %s"
             (or ineligible-reason "session is not eligible"))
            nil))))
    (when (bound-and-true-p niwa--session)
      (apply #'niwa-telemetry-record
             niwa--session 'compaction-threshold-evaluated
             :estimate estimate
             :estimate-source
             (niwa-compact-estimation-baseline-source)
             :admitted (and decision t)
             :target-pressure (and admission
                                   (plist-get admission :target-pressure))
             :ineligible-reason ineligible-reason
             (niwa-compact-estimation-telemetry-inputs estimate target-policy)))
    decision))

;;;###autoload
(defun niwa-compact (&optional aggressive instructions)
  "Compact the current niwa chat buffer.
With prefix argument AGGRESSIVE, compact without preserving a recent
  tail.  INSTRUCTIONS is an optional string of manual summary guidance."
  (interactive "P")
  (when (bound-and-true-p niwa--data-buffer)
    (niwa-view--assert-live-tip))
  (let* ((chat-buffer
          (cond
           ((and (bound-and-true-p gptel-mode) (bound-and-true-p niwa--workspace))
            (current-buffer))
           (t (niwa--active-chat-buffer)))))
    (unless (and chat-buffer (buffer-live-p chat-buffer))
      (niwa-diagnostic-user-error (niwa-diagnostic-scope (current-buffer))
                                    "No niwa chat buffer found"))
    (with-current-buffer chat-buffer
      (niwa-with-diagnostic-errors (niwa-diagnostic-scope chat-buffer)
        (niwa-engine-assert-local-history niwa--session "Manual compaction" gptel-backend t)
        (niwa-compact-run-start
         :aggressive aggressive
         :instructions instructions)))))

(defun niwa--compact-terminate-request (fsm status err &optional aborted)
  "Move FSM to its error state after compaction failure ERR, or ABRT if ABORTED.

STATUS is the gptel status shown before the terminal transition.  gptel
settles a turn from its terminal states, so a compaction that refuses to
send the request has to move the machine there itself.  Left in WAIT the
turn never settles: the view goes on reporting a running request, the
Stop hooks and the autosave never run, and queued pending inputs wait on
a request that will never reach them."
  (when-let* ((info (and fsm (gptel-fsm-info fsm))))
    ;; Before realization :data is a temporary prompt buffer.  Terminal
    ;; gptel handlers expect a provider payload plist instead.
    (when-let* ((data (plist-get info :data))
                ((bufferp data)))
      (when (buffer-live-p data)
        (kill-buffer data))
      (plist-put info :data nil))
    (gptel--update-status status 'error)
    (unless aborted
      (plist-put info :status (format "Compaction failed: %s" err))
      (plist-put info :error
                 (list :type "compaction_error" :message (format "%s" err))))
    (setf (gptel-fsm-info fsm) info)
    (gptel--fsm-transition fsm (if aborted 'ABRT 'ERRS))))

(defun niwa--compact-auto-failure (fsm chat-buffer err)
  "Surface automatic compaction failure ERR for CHAT-BUFFER and end FSM."
  (when (buffer-live-p chat-buffer)
    (with-current-buffer chat-buffer
      (when (>= niwa-compact-run-failure-count 3)
        (setq niwa--compact-auto-disabled t))
      (niwa-diagnostic-warning (niwa-diagnostic-scope chat-buffer)
       'niwa
       (format
        (if niwa--compact-auto-disabled
            "%s (auto-compaction disabled after repeated failures; request not sent)"
          "%s (auto-compaction failed; request not sent)")
        err)
       :warning)
      (niwa-view-stream-end-progress niwa--view-buffer)
      (niwa--compact-terminate-request fsm " Compaction failed" err))))

(defun niwa--compact-continuation-wait-p (fsm)
  "Return non-nil when FSM is entering WAIT for a tool continuation."
  (when-let* ((info (and fsm (gptel-fsm-info fsm))))
    (and (not (niwa-compact-estimation-summary-request-p info))
         (or (eq (car (plist-get info :history)) 'TRET)
             (plist-get info :tool-result)))))

(defun niwa--compact-rebuild-info-data-from-buffer (fsm chat-buffer)
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
          (buffer-local-value 'niwa-compact-target-current-request-reminder
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
              (when request-reminder
                (niwa-reminders-stage-entry
                 fsm 'compact-file-references request-reminder))
              (niwa-reminders-stage-plan-mode fsm)
              (with-current-buffer prompt-buffer
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
              (setq niwa-compact-target-current-request-reminder nil
                    niwa-compact-target-current-request-hook-context nil)))
          (if had-dry-run
              (plist-put info :dry-run old-dry-run)
            (cl-remf info :dry-run)))
      (error
       (plist-put info :data old-data)
       (signal (car err) (cdr err))))))

(defun niwa--compact-main-resume-status (target)
  "Show ordinary request progress after compacting main TARGET."
  (let ((chat-buffer (plist-get target :buffer)))
    (when (buffer-live-p chat-buffer)
      (with-current-buffer chat-buffer
        (niwa-view-stream-show-progress
         niwa--view-buffer "Thinking...")))))

(defun niwa--compact-target-resume (target fsm)
  "Rebuild FSM from compacted TARGET and resume its continuation once."
  (let ((buffer (plist-get target :buffer))
        (info (gptel-fsm-info fsm)))
    (unless (buffer-live-p buffer)
      (error "Compaction target buffer is no longer live"))
    (with-current-buffer buffer
      (when-let* ((marker (plist-get info :position)))
        (set-marker marker (point-max) buffer))
      ;; Rotation collapses the old stream markers to the buffer start.
      ;; Let gptel initialize fresh insertion state after the compacted text.
      (dolist (key '(:tracking-marker :reasoning-marker :tool-marker
                     :reasoning-block))
        (plist-put info key nil))
      (when-let* ((status-function (plist-get target :resume-status)))
        (funcall status-function target)))
    (niwa--compact-rebuild-info-data-from-buffer fsm buffer)
    (niwa--compact-target-provider-wait target fsm)))

(defun niwa--compact-target-provider-wait (target fsm)
  "Dispatch FSM after TARGET compaction, including deferred root steering."
  (unless (plist-get target :invocation)
    (niwa-tools--handle-steering-inject fsm t))
  (unless (plist-get (gptel-fsm-info fsm) :niwa-pending-input-hold)
    (niwa--compact-provider-wait fsm)))

(defun niwa--compact-agent-terminal-failure (target fsm err)
  "Terminate agent FSM with compaction failure ERR."
  (let* ((info (gptel-fsm-info fsm))
         (invocation
          (or (plist-get target :invocation)
              (plist-get info :niwa-agent-invocation))))
    (when (niwa-agent-invocation-p invocation)
      (niwa-agent-conversation-record-activity
       invocation '(:type status :summary "error")))
    (niwa--compact-terminate-request fsm " Agent failed" err)))

(defun niwa--compact-main-failure (target fsm err)
  "Surface automatic main TARGET compaction failure ERR and end FSM."
  (let ((buffer (plist-get target :buffer)))
    (niwa-goal-pause-runtime-failure
     buffer (format "Compaction failed: %s" err))
    (niwa--compact-auto-failure fsm buffer err)))

(defun niwa--compact-handle-target-wait
    (fsm target admission &optional pending-start)
  "Gate FSM continuation through TARGET using precomputed ADMISSION.
PENDING-START, when non-nil, begins the continuation batch that must survive."
  (if (not admission)
      (niwa--compact-provider-wait fsm)
    (let ((pending-start (or pending-start
                             (niwa-compact-evidence-find-boundary))))
      (if (not pending-start)
          (if (plist-get admission :target-pressure)
              (niwa-compact-target-call
               target :fail fsm
               "No compactable history remains at target pressure")
            (niwa--compact-target-provider-wait target fsm))
        (niwa-compact-run-start
         :pending-start pending-start
         :auto t
         :admission admission
         :target target
         :callback
         (lambda (err)
           (cond
            ((eq err :skip)
             (niwa--compact-target-provider-wait target fsm))
            (err
             (niwa-compact-target-call target :fail fsm err))
            (t
             (condition-case rebuild-err
                 (niwa-compact-target-call target :resume fsm)
               (error
                (niwa-compact-target-call
                 target :fail fsm
                 (error-message-string rebuild-err))))))))))))

(defun niwa--compact-handle-agent-wait (fsm)
  "Run persisted-agent compaction before a continuation request in FSM."
  (let* ((info (and fsm (gptel-fsm-info fsm)))
         (agent-buffer (and (listp info) (plist-get info :buffer)))
         (invocation
          (and (listp info) (plist-get info :niwa-agent-invocation))))
    (if (or (not (niwa--compact-continuation-wait-p fsm))
            (not (buffer-live-p agent-buffer))
            (not (niwa-agent-invocation-p invocation)))
        (niwa--compact-provider-wait fsm)
      (with-current-buffer agent-buffer
        (let* ((target (niwa-compact-target-agent-target invocation))
               (target-policy
                (or (plist-get info :niwa-compaction-target-policy)
                    (niwa-compact-estimation-target-policy)))
               (estimate
                (niwa-compact-estimation-estimate-data-tokens
                 (plist-get info :data)))
               (auto-ready
                (and niwa-compact-auto
                     (not niwa--compact-auto-disabled)
                     (not niwa-compact-run-in-flight)))
               (admission
                (and target
                     auto-ready
                     (niwa-compact-estimation-admission
                      estimate target-policy))))
          (cond
           (target
            (niwa--compact-handle-target-wait
             fsm target admission
             (niwa-compact-evidence-current-tool-batch-start
              info (plist-get target :body-start))))
           ((and auto-ready
                 (>= estimate
                     (niwa-compact-estimation-policy-threshold-tokens
                      target-policy)))
            (niwa--compact-agent-terminal-failure
             nil fsm
             "Agent transcript is not eligible for compaction at target pressure"))
           (t
            (niwa--compact-provider-wait fsm))))))))

(defun niwa--compact-main-wait-decision (fsm)
  "Return the main continuation compaction decision for FSM, or nil."
  (let* ((info (and fsm (gptel-fsm-info fsm)))
         (chat-buffer (and (listp info) (plist-get info :buffer))))
    (when (and (niwa--compact-continuation-wait-p fsm)
               (buffer-live-p chat-buffer))
      (with-current-buffer chat-buffer
        (let* ((target-policy
                (or (plist-get info :niwa-compaction-target-policy)
                    (let ((gptel-backend
                           (or (plist-get info :backend) gptel-backend))
                          (gptel-model
                           (or (plist-get info :model) gptel-model)))
                      (niwa-compact-estimation-target-policy))))
               (token-estimate
                (niwa-compact-estimation-estimate-data-tokens
                 (plist-get info :data)))
               (gptel-backend (plist-get target-policy :backend))
               (gptel-model (plist-get target-policy :model))
               (gptel-reasoning-effort
                (plist-get target-policy :effort))
               (gptel-max-tokens (plist-get target-policy :max-tokens))
               (gptel--request-params
                (plist-get target-policy :request-params)))
          (list :admission
                (niwa--compact-should-compact-p token-estimate)
                :target-policy target-policy))))))

(defun niwa--compact-defer-steering-p (fsm)
  "Cache FSM's compaction decision and return non-nil when steering must wait."
  (when-let* ((decision (niwa--compact-main-wait-decision fsm)))
    (setf (gptel-fsm-info fsm)
          (plist-put (gptel-fsm-info fsm)
                     :niwa-compaction-wait-decision decision))
    (plist-get decision :admission)))

(defun niwa--compact-handle-wait (fsm)
  "Run continuation auto-compaction for FSM before `gptel--handle-wait'."
  (let* ((info (and fsm (gptel-fsm-info fsm)))
         (cached (and (listp info)
                      (plist-get info :niwa-compaction-wait-decision))))
    (if (plist-get info :niwa-pending-input-hold)
        nil
      (let ((decision
             (or cached (niwa--compact-main-wait-decision fsm))))
        (when cached
          (cl-remf info :niwa-compaction-wait-decision)
          (setf (gptel-fsm-info fsm) info))
        (if (not decision)
            (niwa--compact-provider-wait fsm)
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
                (niwa--compact-handle-target-wait
                 fsm (niwa-compact-target-main-target)
                 (plist-get decision :admission))))))))))

(defun niwa--compact-transform-auto (continue fsm)
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
       info :niwa-request-locals
       (niwa-agent-exec-request-snapshot
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
                  info :niwa-model-context
                  (if (and tail-start (<= tail-start tail-end))
                      (concat (buffer-substring (point-min) tail-start)
                              (buffer-substring tail-end (point-max)))
                    (buffer-string))))))
           (unwind-protect
               (funcall continue)
             (set-marker late-tail-start nil))))
      (if (or (plist-get info :niwa-shared-item)
              (and (listp context)
                   (plist-get context :niwa-context-summary))
              (not (buffer-live-p source-buffer)))
          (continue-with-snapshot)
        (with-current-buffer source-buffer
          (let ((target-policy
                 (let ((gptel-backend effective-backend)
                       (gptel-model effective-model)
                       (gptel-max-tokens effective-max-tokens)
                       (gptel--request-params effective-request-params))
                   (niwa-compact-estimation-target-policy)))
                (admission
                 (unless niwa-compact-run-in-flight
                   (let ((gptel-backend effective-backend)
                         (gptel-model effective-model)
                         (gptel-max-tokens effective-max-tokens)
                         (gptel--request-params effective-request-params))
                     (niwa--compact-should-compact-p
                      (niwa-compact-estimation-estimate-transformed-request-tokens
                       source-buffer prompt-buffer))))))
            (plist-put info :niwa-compaction-target-policy target-policy)
            (cond
             (niwa-compact-run-in-flight
              (user-error "Compaction already in progress"))
             ((not admission)
              (continue-with-snapshot))
             (t
              (let ((pending-start (niwa-compact-evidence-find-boundary)))
                (if (not pending-start)
                    (if (plist-get admission :target-pressure)
                        (niwa--compact-auto-failure
                         fsm source-buffer
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
                             (when-let* ((start (niwa-compact-evidence-find-boundary)))
                               (copy-marker start nil))))))
                    (niwa-compact-run-start
                     :pending-start pending-start
                     :auto t
                     :admission admission
                     :callback
                     (lambda (err)
                       (unwind-protect
                           (cond
                            ((eq err :skip)
                             (continue-with-snapshot))
                            ((and niwa-compact-run-callback-aborted-p
                                  (not (plist-get info :niwa-request-id)))
                             (niwa--compact-terminate-request fsm " Abort" err t))
                            (err
                             (niwa--compact-auto-failure
                              fsm source-buffer err))
                            (t
                             (let (context-tail-end)
                               (when (and (buffer-live-p prompt-buffer)
                                          (buffer-live-p source-buffer))
                                 (with-current-buffer source-buffer
                                   (when-let* ((marker (plist-get info :position)))
                                     (set-marker marker (point-max) source-buffer)))
                                 (niwa-view-stream-show-progress
                                  niwa--view-buffer "Thinking...")
                                 (niwa-compact-evidence-rebuild-prompt-buffer
                                  prompt-buffer source-buffer source-pending-text
                                  prompt-history-start prompt-pending-start)
                                 (setq context-tail-end
                                       (with-current-buffer prompt-buffer
                                         (copy-marker (point-max) nil)))
                                 (when-let* ((hook-context
                                              (buffer-local-value
                                               'niwa-compact-target-current-request-hook-context
                                               source-buffer)))
                                   (with-current-buffer prompt-buffer
                                     (goto-char (point-max))
                                     (unless (bolp) (insert "\n"))
                                     (insert "\n" hook-context "\n"))
                                   (with-current-buffer source-buffer
                                     (setq niwa-compact-target-current-request-hook-context
                                           nil)))
                                 (when-let* ((reminder
                                              (buffer-local-value
                                               'niwa-compact-target-current-request-reminder
                                               source-buffer)))
                                   (niwa-reminders-stage-entry
                                    fsm 'compact-file-references reminder)
                                   (with-current-buffer source-buffer
                                     (setq niwa-compact-target-current-request-reminder
                                           nil))))
                               (unwind-protect
                                   (continue-with-snapshot context-tail-end)
                                 (when (markerp context-tail-end)
                                   (set-marker context-tail-end nil))))))
                         (when (markerp prompt-history-start)
                           (set-marker prompt-history-start nil))
                         (when (markerp prompt-pending-start)
                           (set-marker prompt-pending-start nil))))))))))))))))

(provide 'niwa-compact)
;;; niwa-compact.el ends here
