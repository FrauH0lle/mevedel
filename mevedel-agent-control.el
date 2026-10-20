;;; mevedel-agent-control.el -- Durable agent-tree control -*- lexical-binding: t -*-

;;; Commentary:

;; Owns the path-addressed, session-scoped agent registry and the admission and
;; settlement transaction for asynchronous agent turns.  Provider execution
;; remains in `mevedel-agent-runtime'; this module gives it durable identities,
;; tree-wide capacity, canonical RESULT/MAIL/USER records, retained mailboxes,
;; turn-local interruption, and the WaitAgent suspension and wake lifecycle.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x)
  ;; Struct setters need their expanders during standalone package compilation.
  (require 'mevedel-agents))

;; `gptel'
(declare-function gptel-backend-name "ext:gptel" (backend))

;; `mevedel-agent-conversation'
(declare-function mevedel-agent-conversation-final-response
                  "mevedel-agent-conversation" (invocation))
(declare-function mevedel-agent-conversation-save
                  "mevedel-agent-conversation" (invocation &optional deferred))
(autoload 'mevedel-agent-conversation-final-response
  "mevedel-agent-conversation")
(autoload 'mevedel-agent-conversation-save "mevedel-agent-conversation")
(defvar mevedel--agent-invocation)

;; `mevedel-agent-persistence'
(declare-function mevedel-agent-persistence-ensure-conversation
                  "mevedel-agent-persistence"
                  (session record root-buffer &optional readonly-p))
(autoload 'mevedel-agent-persistence-ensure-conversation "mevedel-agent-persistence")

;; `mevedel-agent-runtime'
(declare-function mevedel-agent-runtime-dispatch
                  "mevedel-agent-runtime" t t)
(declare-function mevedel-agent-runtime-abandon-persistence
                  "mevedel-agent-runtime" (invocation))
(declare-function mevedel-agent-runtime-interrupt
                  "mevedel-agent-runtime" (invocation reason))
(declare-function mevedel-agent-runtime-prepare-task
                  "mevedel-agent-runtime" t t)
(declare-function mevedel-agent-runtime-task-background
                  "mevedel-agent-runtime" (summary))
(autoload 'mevedel-agent-runtime-abandon-persistence
  "mevedel-agent-runtime")
(autoload 'mevedel-agent-runtime-dispatch "mevedel-agent-runtime")
(autoload 'mevedel-agent-runtime-interrupt "mevedel-agent-runtime")
(autoload 'mevedel-agent-runtime-prepare-task "mevedel-agent-runtime")
(autoload 'mevedel-agent-runtime-task-background "mevedel-agent-runtime")

;; `mevedel-agents'
(declare-function copy-mevedel-agent "mevedel-agents" (cl-x))
(declare-function mevedel-agent-configuration-model-metadata
                  "mevedel-agents" (configuration))
(declare-function mevedel-agent-configuration-p
                  "mevedel-agents" (cl-x))
(declare-function mevedel-agent-hook-rules "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-agent-id
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-buffer
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-frozen-configuration
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-parent-session
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-path
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-plan-read-only
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-terminal-reason
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-transcript-relative-path
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-transcript-status
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-name "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-resolve-role "mevedel-agents" (role))
(declare-function mevedel-agents-specs "mevedel-agents" (&optional buffer))
(declare-function mevedel-plan-read-only-p "mevedel-agents"
                  (&optional session))
(autoload 'copy-mevedel-agent "mevedel-agents")
(autoload 'mevedel-agent-configuration-model-metadata "mevedel-agents")
(autoload 'mevedel-agent-configuration-p "mevedel-agents")
(autoload 'mevedel-agent-hook-rules "mevedel-agents")
(autoload 'mevedel-agent-invocation-agent-id "mevedel-agents")
(autoload 'mevedel-agent-invocation-buffer "mevedel-agents")
(autoload 'mevedel-agent-invocation-frozen-configuration "mevedel-agents")
(autoload 'mevedel-agent-invocation-parent-session "mevedel-agents")
(autoload 'mevedel-agent-invocation-path "mevedel-agents")
(autoload 'mevedel-agent-invocation-plan-read-only "mevedel-agents")
(autoload 'mevedel-agent-invocation-terminal-reason "mevedel-agents")
(autoload 'mevedel-agent-invocation-transcript-relative-path "mevedel-agents")
(autoload 'mevedel-agent-invocation-transcript-status "mevedel-agents")
(autoload 'mevedel-agent-name "mevedel-agents")
(autoload 'mevedel-agent-resolve-role "mevedel-agents")
(autoload 'mevedel-plan-read-only-p "mevedel-agents")
(autoload 'mevedel-agents-specs "mevedel-agents")

;; `mevedel-compact-evidence'
(declare-function mevedel-compact-evidence-context-snapshot
                  "mevedel-compact-evidence" (context))
(declare-function mevedel-compact-evidence-summary-context-evidence
                  "mevedel-compact-evidence" (tool-use-id))
(autoload 'mevedel-compact-evidence-context-snapshot
  "mevedel-compact-evidence")
(autoload 'mevedel-compact-evidence-summary-context-evidence
  "mevedel-compact-evidence")

;; `mevedel-context-summary'
(declare-function mevedel-context-summary-generate
                  "mevedel-context-summary" t t)
(autoload 'mevedel-context-summary-generate "mevedel-context-summary")

;; `mevedel-engine'
(declare-function mevedel-engine-info "mevedel-engine" (owner))

;; `mevedel-models'
(declare-function mevedel-model-parse-effort "mevedel-models" (value))
(declare-function mevedel-model-parse-selector "mevedel-models" (value))
(declare-function mevedel-model-resolve-workload
                  "mevedel-models"
                  (workload &optional explicit-selector explicit-effort))
(autoload 'mevedel-model-parse-effort "mevedel-models")
(autoload 'mevedel-model-parse-selector "mevedel-models")
(autoload 'mevedel-model-resolve-workload "mevedel-models")

;; `mevedel-pending-inputs'
(declare-function mevedel-pending-inputs-offer-root-event
                  "mevedel-pending-inputs" (session &optional data-buffer))
(autoload 'mevedel-pending-inputs-offer-root-event "mevedel-pending-inputs")

;; `mevedel-session-artifacts'
(declare-function mevedel-session-artifacts--sidecar-artifact
                  "mevedel-session-artifacts" (session buffer &optional checkpoint))
(declare-function mevedel-session-artifacts-assert-mutation-authority
                  "mevedel-session-artifacts" (session &optional buffer))
(autoload 'mevedel-session-artifacts-assert-mutation-authority
  "mevedel-session-artifacts")

;; `mevedel-session-checkpoint'
(declare-function mevedel-session-checkpoint-consume
                  "mevedel-session-checkpoint" (session conversation records))
(declare-function mevedel-session-checkpoint-pending-message-p
                  "mevedel-session-checkpoint" (session conversation id))
(autoload 'mevedel-session-checkpoint-consume "mevedel-session-checkpoint")
(autoload 'mevedel-session-checkpoint-pending-message-p "mevedel-session-checkpoint")

;; `mevedel-session-durability'
(defvar mevedel-session-durability--asserted-directories)
(defvar mevedel-session-durability--commit-riders)
(defvar mevedel-session-durability--final-commit)
(defvar mevedel-session-durability--transaction-clock)
(defvar mevedel-session-durability-async-process-function)

;; `mevedel-session-persistence'
(declare-function mevedel-session-persistence-cancel-deferred-agent-save
                  "mevedel-session-persistence" (session))
(declare-function mevedel-session-persistence-save-agent-state
                  "mevedel-session-persistence" (session &optional token))
(declare-function mevedel-session-persistence-save-agent-state-soon
                  "mevedel-session-persistence" (session))
(autoload 'mevedel-session-persistence-cancel-deferred-agent-save
  "mevedel-session-persistence")
(autoload 'mevedel-session-persistence-save-agent-state
  "mevedel-session-persistence")
(autoload 'mevedel-session-persistence-save-agent-state-soon
  "mevedel-session-persistence")

;; `mevedel-session-publication'
(declare-function mevedel-session-publication-async-eligible-p "mevedel-session-publication" (session))
(declare-function mevedel-session-publication-barrier "mevedel-session-publication" (session &optional token))
(declare-function mevedel-session-publication-busy-p "mevedel-session-publication" (session))
(declare-function mevedel-session-publication-defer "mevedel-session-publication" (session preparation &optional failure token))
(declare-function mevedel-session-publication-submit "mevedel-session-publication" (session prepare acknowledge &optional mail-ids))
(autoload 'mevedel-session-publication-async-eligible-p "mevedel-session-publication")
(autoload 'mevedel-session-publication-barrier "mevedel-session-publication")
(autoload 'mevedel-session-publication-busy-p "mevedel-session-publication")
(autoload 'mevedel-session-publication-defer "mevedel-session-publication")
(autoload 'mevedel-session-publication-submit "mevedel-session-publication")

;; `mevedel-session-recovery'
(defvar mevedel-session-recovery--mutation-cache)

;; `mevedel-structs'
(declare-function mevedel-session-root-buffer "mevedel-structs" (cl-x) t)
(declare-function mevedel-agent-path-p "mevedel-structs" (path))
(declare-function mevedel-session--set-agent-registry
                  "mevedel-structs" (session registry))
(declare-function mevedel-session--set-agent-reservations
                  "mevedel-structs" (session reservations))
(declare-function mevedel-session--set-agent-root-waiter
                  "mevedel-structs" (session waiter))
(declare-function mevedel-session--set-messages
                  "mevedel-structs" (session messages))
(declare-function mevedel-session-agent-registry
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-agent-reservations
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-agent-root-activity
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-agent-root-waiter
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-agent-turn-capacity
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-messages
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-p "mevedel-structs" (object))
(declare-function mevedel-session-save-path
                  "mevedel-structs" (cl-x) t)

;; `mevedel-telemetry'
(declare-function mevedel-telemetry-record
                  "mevedel-telemetry" (session event &rest props))

;; `mevedel-transcript-audit'
(declare-function mevedel--format-hook-audit-record
                  "mevedel-transcript-audit" (record))
(autoload 'mevedel--format-hook-audit-record "mevedel-transcript-audit")

;; `mevedel-transport'
(declare-function mevedel-transport-busy-p "mevedel-transport" (&optional path))
(declare-function mevedel-transport-cancel-pending
                  "mevedel-transport" (&optional key))
(declare-function mevedel-transport-run-at-time
                  "mevedel-transport" (seconds function &rest args))
(declare-function mevedel-transport-run-when-idle
                  "mevedel-transport"
                  (key path thunk &optional on-cancel delay))
(autoload 'mevedel-transport-busy-p "mevedel-transport")
(autoload 'mevedel-transport-cancel-pending "mevedel-transport")
(autoload 'mevedel-transport-run-at-time "mevedel-transport")
(autoload 'mevedel-transport-run-when-idle "mevedel-transport")

;; `mevedel-utilities'
(declare-function mevedel--clamped-integer
                  "mevedel-utilities" (value default minimum maximum))
(declare-function mevedel--head-tail-preview-parts
                  "mevedel-utilities"
                  (head tail total-length &optional preview-size))
(declare-function mevedel--warn-once
                  "mevedel-utilities" (key format &rest args))

(defconst mevedel-agent-control--result-limit (* 32 1024)
  "Maximum number of characters in an inline RESULT payload.")

(defconst mevedel-agent-control--wait-default-ms 30000
  "Default WaitAgent timeout in milliseconds.")

(defconst mevedel-agent-control--wait-min-ms 10000
  "Minimum WaitAgent timeout in milliseconds.")

(defconst mevedel-agent-control--wait-max-ms 3600000
  "Maximum WaitAgent timeout in milliseconds.")

(defconst mevedel-agent-control--active-activities
  '(starting running waiting permission-blocked interaction-blocked)
  "Persisted agent activities that own one active-turn slot.")

(defun mevedel-agent-control-active-activity-p (activity)
  "Return non-nil when ACTIVITY owns one active-turn slot."
  (memq activity mevedel-agent-control--active-activities))

(defvar mevedel-agent-control-suppress-persistence nil
  "Non-nil while a multi-step registry repair must be persisted atomically.")

(defvar mevedel-agent-control-settling nil
  "Agent invocations whose settlement is running, innermost first.
The runtime binds this around each settlement.  A turn that started
meanwhile could race the staged terminal state: its capacity release,
rollback, publication, or release of the finished invocation.  Code nested
inside a settlement, timers included, sees the binding.")

(cl-defstruct (mevedel-agent-waiter
               (:constructor mevedel-agent-waiter--create))
  "One suspended ordinary asynchronous WaitAgent tool call."
  callback
  release
  started-at
  timer)

(cl-defstruct (mevedel-agent-record
               (:constructor mevedel-agent-record--create))
  "One retained non-root agent identity in a root session."
  id
  path
  parent-path
  role
  configuration
  activity
  conversation-location
  conversation-buffer
  invocation
  mailbox
  hook-context-pending
  blockers
  waiter
  result-handler
  settled-result
  settled-outcome
  ;; Display-only provider metadata of the latest turn, as
  ;; `mevedel-agent-configuration-model-metadata' returns it.  Runtime
  ;; provider recovery never rewrites CONFIGURATION, so this retains what
  ;; actually served the agent across settlement and cold restore.
  model-metadata)

(defun mevedel-agent-control--capture-model-metadata (record)
  "Retain the provider metadata of RECORD's current invocation, if any."
  (when-let* ((invocation (mevedel-agent-record-invocation record))
              (metadata (mevedel-agent-configuration-model-metadata
                         (mevedel-agent-invocation-frozen-configuration
                          invocation))))
    (setf (mevedel-agent-record-model-metadata record) metadata)))

(defun mevedel-agent-control-record-model-metadata (record)
  "Return the provider metadata currently describing retained RECORD, or nil.
A running turn reports its invocation's effective configuration, including
provider recovery and external acknowledgement.  Otherwise report the latest
turn's retained metadata, then the frozen configuration.  The value has the
shape of `mevedel-agent-configuration-model-metadata'.  No buffer or file is
read, so redraws may call this per record."
  (or (when-let* ((invocation (mevedel-agent-record-invocation record)))
        (mevedel-agent-configuration-model-metadata
         (mevedel-agent-invocation-frozen-configuration invocation)))
      (mevedel-agent-record-model-metadata record)
      (mevedel-agent-configuration-model-metadata
       (mevedel-agent-record-configuration record))))

(defun mevedel-agent-control-model-metadata (session path)
  "Return provider metadata for SESSION's retained agent at PATH, or nil.
See `mevedel-agent-control-record-model-metadata'."
  (when-let* ((record (cdr (assoc path (mevedel-session-agent-registry
                                        session)))))
    (mevedel-agent-control-record-model-metadata record)))

(defun mevedel-agent-control--active-p (record)
  "Return non-nil when RECORD owns one active-turn slot."
  (mevedel-agent-control-active-activity-p
   (mevedel-agent-record-activity record)))

(defun mevedel-agent-control--assert-plan-steering (session path record)
  "Signal unless the caller may message PATH's RECORD in SESSION.
A caller under Plan read-only authority may address /root, an idle agent,
whose next turn it stamps read-only, or a turn that is itself Plan
read-only.  Mail for any other running agent would act under that agent's
own authority."
  (when (and (mevedel-plan-read-only-p session)
             record
             (mevedel-agent-control--active-p record)
             (not (when-let* ((invocation
                               (mevedel-agent-record-invocation record)))
                    (mevedel-agent-invocation-plan-read-only invocation))))
    (user-error
     "SendMessage cannot reach %s from Plan: its running turn is not bound \
by Plan limits.  Message it once it is idle to start a read-only turn"
     path)))

(defun mevedel-agent-control-settled-result (record)
  "Return RECORD's settled result plist when it is idle, or nil.

The returned plist contains the terminal `:outcome' and the complete
unbounded `:payload'.  Active turns deliberately expose no previous result."
  (when (and (mevedel-agent-record-p record)
             (eq (mevedel-agent-record-activity record) 'idle)
             (stringp (mevedel-agent-record-settled-result record))
             (memq (mevedel-agent-record-settled-outcome record)
                   '(completed errored interrupted)))
    (list :outcome (mevedel-agent-record-settled-outcome record)
          :payload (copy-sequence
                    (mevedel-agent-record-settled-result record)))))

(defun mevedel-agent-control--record-settled-result
    (record outcome payload)
  "Store complete PAYLOAD and OUTCOME as RECORD's settled result.
Return the normalized payload stored in the record."
  (let ((payload (if (stringp payload) payload (format "%S" payload))))
    (unless (memq outcome '(completed errored interrupted))
      (error "Invalid agent result outcome: %s" outcome))
    (setf (mevedel-agent-record-settled-result record) payload
          (mevedel-agent-record-settled-outcome record) outcome)
    payload))

(defun mevedel-agent-control-commit-session (session &optional token)
  "Commit SESSION's materialized agent state or signal an error."
  (unless mevedel-agent-control-suppress-persistence
    (when (mevedel-session-save-path session)
      (unless (mevedel-session-persistence-save-agent-state session token)
        (error "Agent state could not be persisted"))))
  t)

(defun mevedel-agent-control--persist-session (session)
  "Best-effort persist SESSION's changed agent state.

Observational transitions -- blocked/waiting activity flavors, explicit
mailbox discards -- debounce into one non-forced save instead of a full
synchronous publication each; the next acknowledged commit absorbs a
pending one.  Recovery treats every active activity identically, so a
crash inside the window costs at most a stale activity flavor or a
repeated discard.  Delivered mail uses a transcript checkpoint instead."
  (unless mevedel-agent-control-suppress-persistence
    (condition-case nil
        (mevedel-session-persistence-save-agent-state-soon session)
      (error nil))))

(defun mevedel-agent-control-block-turn (session path activity)
  "Block PATH's current turn in SESSION with durable ACTIVITY.

ACTIVITY must be `waiting', `permission-blocked', or
`interaction-blocked'.  Return an idempotent release closure bound to the
current invocation, or nil when PATH has no active retained turn.  Overlapping
blockers compose and stale releases cannot alter a later follow-up."
  (unless (memq activity '(waiting permission-blocked interaction-blocked))
    (error "Invalid blocked agent activity: %s" activity))
  (when-let* (((not (equal path "/root")))
              (entry (assoc path (mevedel-session-agent-registry session)))
              (record (cdr entry))
              ((mevedel-agent-control--active-p record))
              (invocation (mevedel-agent-record-invocation record)))
    (let ((token (list activity)))
      (push token (mevedel-agent-record-blockers record))
      (unless (eq activity (mevedel-agent-record-activity record))
        (setf (mevedel-agent-record-activity record) activity)
        (mevedel-agent-control--persist-session session))
      (lambda ()
        (when (and (eq record
                       (cdr (assoc path
                                   (mevedel-session-agent-registry session))))
                   (eq invocation (mevedel-agent-record-invocation record))
                   (memq token (mevedel-agent-record-blockers record)))
          (setf (mevedel-agent-record-blockers record)
                (delq token (mevedel-agent-record-blockers record)))
          (let ((next (or (caar (mevedel-agent-record-blockers record))
                          'running)))
            (unless (eq next (mevedel-agent-record-activity record))
              (setf (mevedel-agent-record-activity record) next)
              (mevedel-agent-control--persist-session session))))))))

(defun mevedel-agent-control--active-count (session)
  "Return the number of active non-root turns in SESSION."
  (+ (length (mevedel-session-agent-reservations session))
     (cl-count-if
      (lambda (entry)
        (mevedel-agent-control--active-p (cdr entry)))
      (mevedel-session-agent-registry session))))

(defun mevedel-agent-control-active-turn-p (session)
  "Return non-nil when SESSION has an active non-root agent turn."
  (> (mevedel-agent-control--active-count session) 0))

(defun mevedel-agent-control-retained-buffer-p (session buffer)
  "Return non-nil when SESSION retains BUFFER as an agent conversation."
  (and (mevedel-session-p session)
       (buffer-live-p buffer)
       (cl-some
        (lambda (entry)
          (eq buffer
              (mevedel-agent-record-conversation-buffer (cdr entry))))
        (mevedel-session-agent-registry session))))

(defun mevedel-agent-control-teardown-session (session)
  "Kill every retained conversation buffer owned by SESSION."
  (when (mevedel-session-p session)
    ;; The caller owns final persistence; a debounced save firing into
    ;; killed buffers would only race it.
    (mevedel-session-persistence-cancel-deferred-agent-save session)
    (mevedel-agent-control--cancel-notifications session)
    (mevedel-transport-cancel-pending (list 'agent-wake session))
    (mevedel-session--set-agent-reservations session nil)
    (mevedel-agent-control-cancel-wait session "/root")
    (dolist (entry (mevedel-session-agent-registry session))
      (mevedel-agent-control-cancel-wait session (car entry))
      (when-let* ((buffer
                   (mevedel-agent-record-conversation-buffer (cdr entry)))
                  ((buffer-live-p buffer)))
        (let ((saved-p
               (with-current-buffer buffer
                 (or (not (buffer-modified-p))
                     (when mevedel--agent-invocation
                       (mevedel-agent-conversation-save
                        mevedel--agent-invocation))))))
          (if saved-p
              (kill-buffer buffer)
            (display-warning
             'mevedel
             (format
              "Retained agent transcript could not be saved during session teardown: %s"
              (car entry))
             :warning)))))))

(defun mevedel-agent-control--path-for-invocation (session invocation)
  "Return INVOCATION's retained canonical path in SESSION."
  (let ((path (mevedel-agent-invocation-path invocation)))
    (and (mevedel-agent-path-p path)
         (assoc path (mevedel-session-agent-registry session))
         path)))

(defun mevedel-agent-control-current-path (session)
  "Return the current caller's canonical path in SESSION."
  (let ((invocation mevedel--agent-invocation))
    (if (not invocation)
        "/root"
      (or (mevedel-agent-control--path-for-invocation session invocation)
          (error "Current agent is not registered: %s"
                 (mevedel-agent-invocation-path invocation))))))

(defun mevedel-agent-control-resolve-path (session caller-path target)
  "Resolve TARGET from CALLER-PATH to an addressable path in SESSION."
  (unless (mevedel-agent-path-p caller-path)
    (error "Invalid caller agent path: %s" caller-path))
  (unless (and (stringp target) (not (string-empty-p target)))
    (user-error "Agent target must be a non-empty path"))
  (let ((path
         (if (string-prefix-p "/" target)
             target
           (concat caller-path "/" target))))
    (unless (mevedel-agent-path-p path)
      (user-error "Invalid agent target: %s" target))
    (unless (or (equal path "/root")
                (assoc path (mevedel-session-agent-registry session)))
      (user-error "Unknown agent target: %s" target))
    path))

(defun mevedel-agent-control--record-at-path (session path)
  "Return SESSION's retained record at PATH, or signal an error."
  (or (cdr (assoc path (mevedel-session-agent-registry session)))
      (error "Unknown agent path: %s" path)))

(defun mevedel-agent-control--mailbox-queue (session path)
  "Return PATH's reverse-order unread queue in SESSION."
  (if (equal path "/root")
      (mevedel-session-messages session)
    (mevedel-agent-record-mailbox
     (mevedel-agent-control--record-at-path session path))))

(defun mevedel-agent-control--set-mailbox-queue (session path queue)
  "Set PATH's reverse-order unread QUEUE in SESSION."
  (if (equal path "/root")
      (mevedel-session--set-messages session queue)
    (setf (mevedel-agent-record-mailbox
           (mevedel-agent-control--record-at-path session path))
          queue)))

(defun mevedel-agent-control--mailbox (session path)
  "Return PATH's committed unread records in SESSION in FIFO order.
Producer publication can yield to a receiving WAIT.  Tentative records stay
serializable for that publication, but cannot enter a receiving transcript."
  (cl-remove-if (lambda (message) (plist-get message :uncommitted))
                (reverse (mevedel-agent-control--mailbox-queue session path))))

(defun mevedel-agent-control-context-mailbox (context)
  "Return CONTEXT's retained unread records in FIFO order."
  (if (mevedel-session-p context)
      (mevedel-agent-control--mailbox context "/root")
    (let* ((session (mevedel-agent-invocation-parent-session context))
           (path (and session
                      (mevedel-agent-control--path-for-invocation
                       session context))))
      (and path (mevedel-agent-control--mailbox session path)))))

(defun mevedel-agent-control-clear-context-mailbox (context)
  "Explicitly discard retained unread records for CONTEXT.
An empty mailbox persists nothing.  This does not acknowledge consumption or
discard delivered records awaiting their receiving transcript checkpoint."
  (let* ((session (if (mevedel-session-p context)
                      context
                    (mevedel-agent-invocation-parent-session context)))
         (path (cond
                ((mevedel-session-p context) "/root")
                (session (mevedel-agent-control--path-for-invocation
                          session context)))))
    (when (and path (mevedel-agent-control--mailbox-queue session path))
      (mevedel-agent-control--set-mailbox-queue session path nil)
      (mevedel-agent-control--persist-session session))))

(defun mevedel-agent-control-acknowledge-mail (context messages)
  "Consume only captured MESSAGES from CONTEXT's retained mailbox.
Call after inserting their delivery blocks into the receiving transcript.
The checkpoint owner retains the removed records in durable projections until
that receiving transcript commits.  No save or hook runs here.  Messages queued
since capture remain unread."
  (let* ((session (if (mevedel-session-p context) context
                    (mevedel-agent-invocation-parent-session context)))
         (path (mevedel-agent-control-context-path context)))
    (when path
      (let* ((previous (mevedel-agent-control--mailbox-queue session path))
             (ids (mapcar (lambda (message) (plist-get message :id)) messages))
             (removed (cl-remove-if-not
                       (lambda (message) (member (plist-get message :id) ids))
                       previous))
             (remaining (cl-remove-if (lambda (message) (memq message removed))
                                      previous)))
        (unless (equal previous remaining)
          ;; Snapshot before removal: even an error cannot lose live mail.
          (mevedel-session-checkpoint-consume session path removed)
          (mevedel-agent-control--set-mailbox-queue session path remaining))))))

(defun mevedel-agent-control--waiter (session path)
  "Return PATH's active waiter in SESSION, or nil."
  (if (equal path "/root")
      (mevedel-session-agent-root-waiter session)
    (when-let* ((record
                 (cdr (assoc path
                             (mevedel-session-agent-registry session)))))
      (mevedel-agent-record-waiter record))))

(defun mevedel-agent-control--set-waiter (session path waiter)
  "Set PATH's active WAITER in SESSION."
  (if (equal path "/root")
      (mevedel-session--set-agent-root-waiter session waiter)
    (setf (mevedel-agent-record-waiter
           (mevedel-agent-control--record-at-path session path))
          waiter)))

(defun mevedel-agent-control-cancel-wait (session path &optional reason)
  "Cancel PATH's active wait in SESSION without invoking its callback.
REASON is the wake outcome, or `cancelled' when omitted."
  (when-let* ((waiter (mevedel-agent-control--waiter session path)))
    (mevedel-agent-control--set-waiter session path nil)
    (when-let* ((timer (mevedel-agent-waiter-timer waiter))
                ((timerp timer)))
      (cancel-timer timer))
    (when-let* ((release (mevedel-agent-waiter-release waiter)))
      (funcall release))
    (when (fboundp 'mevedel-telemetry-record)
      (mevedel-telemetry-record
       session 'agent-wait-ended
       :agent-path path
       :outcome (or reason 'cancelled)
       :duration-ms
       (and (numberp (mevedel-agent-waiter-started-at waiter))
            (round (* 1000.0
                      (- (float-time)
                         (mevedel-agent-waiter-started-at waiter)))))))
    t))

(defun mevedel-agent-control--wake (session path reason)
  "Wake PATH's waiter in SESSION with REASON exactly once."
  (when-let* ((waiter (mevedel-agent-control--waiter session path)))
    (let ((callback (mevedel-agent-waiter-callback waiter)))
      (funcall callback reason)
      (mevedel-agent-control-cancel-wait session path reason)
      t)))

(defun mevedel-agent-control--wait-timeout (session path waiter)
  "Wake PATH for timeout if WAITER is still current in SESSION."
  (when (eq waiter (mevedel-agent-control--waiter session path))
    (mevedel-agent-control--wake session path 'timeout)))

(defun mevedel-agent-control-wait (session callback &optional timeout-ms)
  "Suspend SESSION's current caller until mail, steering, or TIMEOUT-MS.
CALLBACK receives one of `mailbox', `steering', `user', or `timeout'.
Return the caller path when suspended, and nil after an immediate release."
  (let ((timeout (mevedel--clamped-integer
                  timeout-ms
                  mevedel-agent-control--wait-default-ms
                  mevedel-agent-control--wait-min-ms
                  mevedel-agent-control--wait-max-ms)))
    (let ((path (mevedel-agent-control-current-path session)))
      (cond
       ((mevedel-agent-control--mailbox session path)
        (when (fboundp 'mevedel-telemetry-record)
          (mevedel-telemetry-record
           session 'agent-wait-immediate
           :agent-path path :outcome 'mailbox))
        (funcall callback 'mailbox)
        nil)
       ((mevedel-agent-control--waiter session path)
        (error "Agent is already waiting: %s" path))
       (t
        (let ((waiter (mevedel-agent-waiter--create
                       :callback callback :started-at (float-time))))
          (mevedel-agent-control--set-waiter session path waiter)
          (when (fboundp 'mevedel-telemetry-record)
            (mevedel-telemetry-record
             session 'agent-wait-started
             :agent-path path :timeout-ms timeout))
          (setf (mevedel-agent-waiter-release waiter)
                (mevedel-agent-control-block-turn session path 'waiting))
          (condition-case err
              (setf (mevedel-agent-waiter-timer waiter)
                    (mevedel-transport-run-at-time
                     (/ timeout 1000.0)
                     #'mevedel-agent-control--wait-timeout
                     session path waiter))
            (error
             (mevedel-agent-control-cancel-wait session path)
             (signal (car err) (cdr err))))
          path))))))

(defun mevedel-agent-control-root-waiting-p (session)
  "Return non-nil when SESSION's root has an active WaitAgent callback."
  (and (mevedel-agent-control--waiter session "/root") t))

(defun mevedel-agent-control-wake-root-user (session)
  "Wake SESSION's root WaitAgent for pending user steering."
  (mevedel-agent-control--wake session "/root" 'user))

(defun mevedel-agent-control--notification-blocked-p (session)
  "Return non-nil while SESSION cannot safely run arbitrary waiter callbacks."
  (or (mevedel-session-publication-busy-p session)
      (mevedel-session-publication-active-p session)
      (mevedel-transport-busy-p (mevedel-session-save-path session))))

(defun mevedel-agent-control--cancel-notifications (session)
  "Cancel SESSION's notification timer and discard its transient offers."
  (when-let* ((timer (mevedel-session-mail-notification-timer session)))
    (cancel-timer timer))
  (setf (mevedel-session-mail-notification-timer session) nil
        (mevedel-session-mail-notifications session) nil
        (mevedel-session-mail-notification-delay session) nil))

(defun mevedel-agent-control--queue-notification (session key function)
  "Queue FUNCTION under KEY in SESSION's coalesced FIFO notification set.
Notifications never enter the mutation preparation queue.  One timer retries
the whole set, backing off from 50 ms to 500 ms while publication stays busy."
  (unless (assoc key (mevedel-session-mail-notifications session))
    (setf (mevedel-session-mail-notifications session)
          (nconc (mevedel-session-mail-notifications session)
                 (list (cons key function)))))
  (unless (or (mevedel-session-publication-stopping-p session)
              (mevedel-session-mail-notification-timer session))
    (let ((delay (or (mevedel-session-mail-notification-delay session) 0.05)))
      (setf (mevedel-session-mail-notification-delay session) delay
            (mevedel-session-mail-notification-timer session)
            (mevedel-transport-run-at-time
             delay #'mevedel-agent-control--flush-notifications session))))
  t)

(defun mevedel-agent-control--flush-notifications (session)
  "Offer SESSION's pending notifications in order outside the mutation gate."
  (setf (mevedel-session-mail-notification-timer session) nil)
  (if (not (buffer-live-p (mevedel-session-root-buffer session)))
      (mevedel-agent-control--cancel-notifications session)
    (while (and (mevedel-session-mail-notifications session)
                (buffer-live-p (mevedel-session-root-buffer session))
                (not (mevedel-session-publication-stopping-p session))
                (not (mevedel-agent-control--notification-blocked-p session)))
      (let ((entry (pop (mevedel-session-mail-notifications session))))
        (condition-case err
            ;; Timers can run inside another session's advancing transaction.
            ;; Arbitrary waiter code must open a fresh transaction of its own.
            (let ((mevedel-session-durability--transaction-clock nil)
                  (mevedel-session-durability--asserted-directories nil)
                  (mevedel-session-durability--commit-riders nil)
                  (mevedel-session-durability--final-commit nil)
                  (mevedel-session-durability-async-process-function nil)
                  (mevedel-session-recovery--mutation-cache nil))
              (funcall (cdr entry)))
          (error
           (display-warning
            'mevedel (format "Mailbox notification failed: %s"
                             (error-message-string err)) :warning)))))
    (cond
     ((not (buffer-live-p (mevedel-session-root-buffer session)))
      (mevedel-agent-control--cancel-notifications session))
     ;; A temporary close query may still be vetoed.  Keep the actual offers,
     ;; without a timer; reopening admissions re-arms them, teardown drops them.
     ((mevedel-session-publication-stopping-p session) nil)
     ((mevedel-session-mail-notifications session)
      (let ((entry (car (mevedel-session-mail-notifications session))))
        (setf (mevedel-session-mail-notification-delay session)
              (min 0.5 (* 2 (or (mevedel-session-mail-notification-delay session) 0.05))))
        (mevedel-agent-control--queue-notification session (car entry) (cdr entry))))
     (t (mevedel-agent-control--cancel-notifications session)))))

(defun mevedel-agent-control-notify (session key function)
  "Run FUNCTION outside SESSION's mutation gate, or queue it under KEY.
Pending notifications are coalesced by KEY and offered in FIFO order.  They
never become mutation preparations or inherit transaction observations.
Stopping sessions retain offers without a timer, for a vetoed close to reoffer
or teardown to discard.  A callback failure is diagnostic only."
  (if (or (mevedel-session-publication-stopping-p session)
          (mevedel-agent-control--notification-blocked-p session)
          (mevedel-session-mail-notifications session))
      (mevedel-agent-control--queue-notification session key function)
    (let ((mevedel-session-durability--transaction-clock nil)
          (mevedel-session-durability--asserted-directories nil)
          (mevedel-session-durability--commit-riders nil)
          (mevedel-session-durability--final-commit nil)
          (mevedel-session-durability-async-process-function nil)
          (mevedel-session-recovery--mutation-cache nil))
      (condition-case err
          (funcall function)
        (error
         (display-warning
          'mevedel (format "Mailbox notification failed: %s"
                           (error-message-string err)) :warning))))))

(defun mevedel-agent-control-reoffer-mail (session)
  "Re-arm SESSION's held notifications after a vetoed close reopens admissions.
No callback runs inline and an idle session creates no timer.  Teardown drops
the retained offers if the close proceeds instead."
  (when (and (buffer-live-p (mevedel-session-root-buffer session))
             (not (mevedel-session-publication-stopping-p session)))
    (when-let* ((entry (car (mevedel-session-mail-notifications session))))
      (mevedel-agent-control--queue-notification session (car entry) (cdr entry)))))

(defun mevedel-agent-control--deliver-mail
    (session recipient record &optional wake-reason)
  "Notify RECIPIENT of committed RECORD in SESSION.
MAIL and RESULT from another sender are marked to wake their recipient: an
idle root gets a background-event turn and an idle agent its next turn.
Mark before releasing a waiter: its callback can synchronously consume mail.
Recovery repairs neither mark records nor release waits."
  (unless mevedel-agent-control-suppress-persistence
    (let ((notify
           (lambda ()
             (let ((waking
                    (and (memq (plist-get record :type) '(MAIL RESULT))
                         (not (equal (plist-get record :sender) recipient))
                         (memq record (mevedel-agent-control--mailbox-queue session recipient)))))
               (when waking (plist-put record :wake t))
               (mevedel-agent-control--wake session recipient (or wake-reason 'mailbox))
               (when (and waking
                          (memq record (mevedel-agent-control--mailbox-queue session recipient)))
                 (cond
                  ((equal recipient "/root")
                   (mevedel-pending-inputs-offer-root-event session))
                  ;; An active recipient reads it at its next boundary, or its
                  ;; settlement offers the wake.
                  ((not (mevedel-agent-control--active-p
                         (mevedel-agent-control--record-at-path session recipient)))
                   (mevedel-agent-control--schedule-agent-wakes session))))))))
      (mevedel-agent-control-notify
       session (list recipient (plist-get record :id) wake-reason) notify))))

(defun mevedel-agent-control--identify-record (record)
  "Assign RECORD a durable message identity once, at its producer boundary."
  (unless (plist-get record :id)
    (plist-put record :id
               (md5 (format "%s:%s:%s" (emacs-pid) (current-time) (random)))))
  (unless (and (stringp (plist-get record :id))
               (not (string-empty-p (plist-get record :id))))
    (error "Invalid mailbox message identity"))
  record)

(defun mevedel-agent-control--enqueue-records
    (session recipient records &optional wake-reason acknowledge)
  "Durably queue RECORDS (FIFO) for RECIPIENT with one commit, then wake.
All records are secured or none: a failed commit restores the queue.
ACKNOWLEDGE, when supplied, receives (SUCCESS ERROR) after authoritative
adoption or rollback.  Local warm sessions may acknowledge asynchronously."
  (when (mevedel-session-publication-stopping-p session)
    (user-error "Session publication is stopping"))
  ;; All producers enter here, including recovery RESULTs and batched monitor
  ;; records.  Assign once before committing; a retry of these same records
  ;; retains their identities.  Readers never manufacture missing identities.
  (if (and acknowledge
           (not mevedel-agent-control-suppress-persistence)
           (mevedel-session-publication-async-eligible-p session))
      (let (ids)
        (dolist (record records)
          (mevedel-agent-control--identify-record record))
        (mevedel-session-publication-submit
         session
         (lambda ()
           (dolist (record records)
             (mevedel-agent-control--identify-record record))
           (let ((present (mapcar (lambda (message) (plist-get message :id))
                                  (mevedel-agent-control--mailbox-queue session recipient))))
             (setq records (cl-remove-if
                            (lambda (record)
                              (or (member (plist-get record :id) present)
                                  (mevedel-session-checkpoint-pending-message-p
                                   session recipient (plist-get record :id)))) records)))
           (dolist (record records) (plist-put record :uncommitted t))
           (setq ids (mapcar (lambda (record) (plist-get record :id)) records))
           (mevedel-agent-control--set-mailbox-queue
            session recipient
            (append (reverse records)
                    (mevedel-agent-control--mailbox-queue session recipient)))
           (list (mevedel-session-artifacts--sidecar-artifact
                  session (mevedel-session-root-buffer session))))
         (lambda (success err)
           (if success
               (dolist (record records) (cl-remf record :uncommitted))
             (mevedel-agent-control--set-mailbox-queue
              session recipient
              (cl-remove-if (lambda (message) (member (plist-get message :id) ids))
                            (mevedel-agent-control--mailbox-queue session recipient))))
           (funcall acknowledge success err)
           (when (and success records)
             (mevedel-agent-control--deliver-mail
              session recipient (car (last records)) wake-reason)))
         (mapcar (lambda (record) (plist-get record :id)) records))
        'pending)
    (mevedel-session-publication-barrier session)
  (dolist (record records)
    (mevedel-agent-control--identify-record record)
    (plist-put record :uncommitted t))
  (let ((previous
         (mevedel-agent-control--mailbox-queue session recipient))
        (ids (mapcar (lambda (record) (plist-get record :id)) records))
        failure)
    (mevedel-agent-control--set-mailbox-queue
     session recipient (append (reverse records) previous))
    (condition-case err
        (mevedel-agent-control-commit-session session)
      (error
       (mevedel-agent-control--set-mailbox-queue
        session recipient
        (cl-remove-if
         (lambda (message)
           (member (plist-get message :id) ids))
         (mevedel-agent-control--mailbox-queue session recipient)))
       (setq failure err)))
    (if failure
        (if acknowledge (funcall acknowledge nil failure)
          (signal (car failure) (cdr failure)))
      (dolist (record records) (cl-remf record :uncommitted))
      (when acknowledge (funcall acknowledge t nil))
      (when records
        (mevedel-agent-control--deliver-mail
         session recipient (car (last records)) wake-reason))
      records))))

(defun mevedel-agent-control--enqueue
    (session recipient record &optional wake-reason)
  "Durably queue RECORD for RECIPIENT, then wake a matching wait."
  (mevedel-agent-control--enqueue-records
   session recipient (list record) wake-reason)
  record)

(defun mevedel-agent-control-steer-user
    (session message &optional before-wake metadata)
  "Deliver MESSAGE when SESSION's root is explicitly waiting.
Call BEFORE-WAKE before releasing the wait.  Return non-nil only when the
input became steering for the current turn.  METADATA may carry the separate
transcript payload and hook audits."
  (when (mevedel-agent-control--waiter session "/root")
    (when before-wake
      (funcall before-wake))
    (mevedel-agent-control--enqueue
     session "/root"
     (list :type 'USER
           :sender "user"
           :recipient "/root"
           :payload message
           :transcript-payload (plist-get metadata :transcript-payload)
           :hook-audits (plist-get metadata :hook-audits)
           :timestamp (current-time))
     'user)
    t))

(cl-defun mevedel-agent-control-send-message
    (session target message &key parent-tool-use-id)
  "Deliver MESSAGE to TARGET in SESSION and report how.
An idle agent starts its turn with MESSAGE as the prompt when the tree has
capacity.  Otherwise MESSAGE is queued as canonical MAIL: an active agent
reads it at its next boundary, the root may wake for it, and an idle agent
starts once a slot frees.  PARENT-TOOL-USE-ID identifies the calling tool
row of a started turn.  Return (:path PATH :delivery DELIVERY), DELIVERY
being `started', `queued', or `waiting' for a slot."
  (unless (and (stringp message) (not (string-empty-p message)))
    (user-error "SendMessage message must be non-empty plain text"))
  (let* ((sender (mevedel-agent-control-current-path session))
         (recipient
          (mevedel-agent-control-resolve-path session sender target))
         (record (cdr (assoc recipient (mevedel-session-agent-registry session))))
         (idle (and record (not (mevedel-agent-control--active-p record)))))
    (mevedel-agent-control--assert-plan-steering session recipient record)
    (if (and idle
             (mevedel-agent-control--startable-p session record)
             (mevedel-agent-control--capacity-p session))
        (progn
          (mevedel-agent-control--activate
           session record message parent-tool-use-id)
          (list :path recipient :delivery 'started))
      (mevedel-agent-control--enqueue
       session recipient
       (append (list :type 'MAIL
                     :sender sender
                     :recipient recipient
                     :payload message
                     :timestamp (current-time))
               ;; An agent turn this mail wakes keeps the sender's Plan limits.
               (and record
                    (mevedel-plan-read-only-p session)
                    (list :plan-read-only t))))
      (list :path recipient :delivery (if idle 'waiting 'queued)))))

(defun mevedel-agent-control-enqueue-execution-result
    (session owner body &optional execution-id wake meta acknowledge)
  "Queue yielded execution BODY for its OWNER in SESSION.
The owner reads it at its next provider request, like the root does.
EXECUTION-ID correlates the record with its execution while Emacs runs.
WAKE marks a root record that should start a turn when the model is idle;
the mark is runtime state and never persisted.  META is a plist of
structural facts (strings and integers) that the delivery block carries
as opening-tag attributes.  ACKNOWLEDGE receives (SUCCESS ERROR) exactly once
when delivery is secured or definitively fails; output ownership stays with
the producer until this acknowledgement."
  (unless (and (mevedel-agent-path-p owner) (stringp body))
    (error "Invalid execution result"))
  (mevedel-agent-control--enqueue-records
   session owner
   (list (append
    (list :id (and execution-id (secure-hash 'sha256 (concat owner ":" execution-id)))
          :type 'EXECUTION
          :sender owner
          :recipient owner
          :payload body
          :timestamp (current-time))
    (and execution-id (list :execution-id execution-id))
    (and meta (list :meta meta))
    (and wake (list :wake t))))
   nil acknowledge))

(defun mevedel-agent-control-enqueue-monitor-events
    (session execution-id description entries wake &optional acknowledge)
  "Queue Monitor ENTRIES for SESSION's root with one durable commit.
Each entry is (BODY META TERMINAL): BODY is the record text, META a plist of
structural facts carried as opening-tag attributes, and TERMINAL makes the
record the monitor's `MONITOR_TERMINAL' notice instead of an event.
ACKNOWLEDGE receives (SUCCESS ERROR) once after adoption or rollback.
EXECUTION-ID and DESCRIPTION identify the monitor.  WAKE marks every record
as one that starts a turn when the model is idle.  The records are secured
together or not at all, so a retry cannot duplicate a partial batch."
  (unless (and (stringp execution-id) (stringp description)
               (cl-every (lambda (entry) (stringp (car entry))) entries))
    (error "Invalid monitor event"))
  (if entries
    (mevedel-agent-control--enqueue-records
     session "/root"
     (mapcar
      (lambda (entry)
        (append
         (list :type (if (nth 2 entry) 'MONITOR_TERMINAL 'MONITOR)
               :sender "/root"
               :recipient "/root"
               :execution-id execution-id
               :description description
               :payload (car entry)
               :timestamp (current-time))
         (and (nth 1 entry) (list :meta (nth 1 entry)))
         (and wake (list :wake t))))
      entries)
     nil acknowledge)
    (when acknowledge (funcall acknowledge t nil))
    t))

(defun mevedel-agent-control-wake-pending-p (session)
  "Return non-nil while SESSION's root mailbox holds an unread waking record.
Reading the mailbox consumes the record, so a result the model already saw
never wakes it."
  (and session
       (cl-some (lambda (record) (plist-get record :wake))
                (mevedel-agent-control--mailbox session "/root"))
       t))

(defun mevedel-agent-control-clear-wake (session owner execution-id)
  "Stop OWNER's unread records for EXECUTION-ID in SESSION from waking.
The model already received that result through another path."
  (when (and session (equal owner "/root") execution-id)
    (dolist (record (mevedel-session-messages session))
      (when (and (plist-get record :wake)
                 (equal (plist-get record :execution-id) execution-id))
        (plist-put record :wake nil)))))

(defun mevedel-agent-control-monitor-unread-bytes (session execution-id)
  "Return the payload bytes of EXECUTION-ID's unread Monitor records in SESSION."
  (let ((bytes 0))
    (dolist (record (and session (mevedel-session-messages session)))
      (when (and (eq (plist-get record :type) 'MONITOR)
                 (equal (plist-get record :execution-id) execution-id))
        (cl-incf bytes (string-bytes (plist-get record :payload)))))
    bytes))

(defun mevedel-agent-control-list-agents (session &optional path-prefix)
  "Return SESSION's minimal roster, optionally below PATH-PREFIX."
  (when (and path-prefix
             (not (mevedel-agent-path-p path-prefix)))
    (user-error "Invalid agent path prefix: %s" path-prefix))
  (let* ((records
          (mapcar
           (lambda (entry)
             (let ((record (cdr entry)))
               (list :path (mevedel-agent-record-path record)
                     :role (mevedel-agent-record-role record)
                     :activity
                     (symbol-name
                      (mevedel-agent-record-activity record)))))
           (mevedel-session-agent-registry session)))
         (roster
          (cons (list :path "/root" :role "default"
                      :activity
                      (symbol-name
                       (mevedel-session-agent-root-activity session)))
                records))
         (filtered
          (if path-prefix
              (cl-remove-if-not
               (lambda (entry)
                 (let ((path (plist-get entry :path)))
                   (or (equal path path-prefix)
                       (string-prefix-p (concat path-prefix "/") path))))
               roster)
            roster)))
    (sort filtered
          (lambda (a b)
            (string-lessp (plist-get a :path) (plist-get b :path))))))

(defun mevedel-agent-control-context-path (context)
  "Return CONTEXT's canonical path in its root session.

Return nil when CONTEXT is an agent invocation that is not yet published
in its parent session's retained registry.  A freshly spawned child's
first WAIT-state handlers run before its invocation is recorded, so an
unregistered child is a normal transient state, not an error."
  (if (mevedel-session-p context)
      "/root"
    (let ((session (mevedel-agent-invocation-parent-session context)))
      (and session
           (mevedel-agent-control--path-for-invocation session context)))))

(defun mevedel-agent-control-direct-children (session parent-path)
  "Return sorted path and role references below PARENT-PATH in SESSION."
  (sort
   (cl-loop for (_path . record) in (mevedel-session-agent-registry session)
            when (equal parent-path
                        (mevedel-agent-record-parent-path record))
            collect (list :path (mevedel-agent-record-path record)
                          :role (mevedel-agent-record-role record)))
   (lambda (a b)
     (string-lessp (plist-get a :path) (plist-get b :path)))))

(defun mevedel-agent-control--recovery-result-text (record)
  "Return RECORD's complete interrupted recovery payload."
  (let* ((buffer (mevedel-agent-record-conversation-buffer record))
         (identity
          (and (buffer-live-p buffer)
               (buffer-local-value 'mevedel--agent-invocation buffer)))
         (partial
          (and identity
               (ignore-errors
                 (mevedel-agent-conversation-final-response identity))))
         (location (mevedel-agent-record-conversation-location record)))
    (concat
     "Agent turn was interrupted by session recovery."
      "\n\nReason: the persisted turn had no live provider "
      "request after resume."
      (when partial
        (format "\n\nPartial response:\n\n%s" partial))
      (when location
        (format "\n\nTranscript: %s" location)))))

(defun mevedel-agent-control-recover-interrupted (session)
  "Recover every persisted active turn in SESSION as an interrupted RESULT.

Return the number of repaired records.  The caller owns the atomic sidecar
rewrite after the complete registry has been repaired."
  (let ((mevedel-agent-control-suppress-persistence t)
        (count 0))
    (dolist (entry (mevedel-session-agent-registry session))
      (let ((record (cdr entry)))
        (when (mevedel-agent-control--active-p record)
          (let ((payload
                 (mevedel-agent-control--recovery-result-text record)))
            (mevedel-agent-control--record-settled-result
             record 'interrupted payload)
            (mevedel-agent-control--capture-model-metadata record)
            (setf (mevedel-agent-record-activity record) 'idle)
            (setf (mevedel-agent-record-invocation record) nil)
            (setf (mevedel-agent-record-blockers record) nil)
            (mevedel-agent-control--enqueue
             session (mevedel-agent-record-parent-path record)
             (list :type 'RESULT
                   :sender (mevedel-agent-record-path record)
                   :recipient (mevedel-agent-record-parent-path record)
                   :outcome 'interrupted
                   :payload
                   (mevedel-agent-control--bounded-result record payload)
                   :timestamp (current-time)))
            (cl-incf count)))))
    count))

(defun mevedel-agent-control--validate-spawn (session task-name message)
  "Validate SESSION, TASK-NAME, and MESSAGE for a new child."
  (unless (mevedel-session-p session)
    (error "Agent requires an active mevedel session"))
  (unless (and (stringp task-name)
               (let ((case-fold-search nil))
                 (string-match-p "\\`[a-z0-9_-]+\\'" task-name))
               (not (equal task-name "root")))
    (user-error
     "Agent task_name must match [a-z0-9_-]+ and must not be root"))
  (unless (and (stringp message) (not (string-empty-p message)))
    (user-error "Agent message must be non-empty plain text")))

(defun mevedel-agent-control--normalize-context (value)
  "Normalize model-facing parent context VALUE.

Nil defaults to `none'.  The only explicit forms are \"all\", \"none\", and
positive decimal strings, or \"summary\"."
  (cond
   ((or (null value) (equal value "none")) 'none)
   ((equal value "all") 'all)
   ((equal value "summary") 'summary)
   ((and (stringp value)
         (string-match-p "\\`[1-9][0-9]*\\'" value))
    (string-to-number value))
   (t (user-error
       "Agent context must be all, none, summary, or a positive integer string"))))

(defun mevedel-agent-control--reserve
    (session parent-path task-name role)
  "Atomically reserve TASK-NAME below PARENT-PATH in SESSION for ROLE."
  (mevedel-session-publication-barrier session)
  (let* ((path (concat parent-path "/" task-name))
         (registry (mevedel-session-agent-registry session))
         (reservations (mevedel-session-agent-reservations session)))
    (when (or (assoc path registry) (assoc path reservations))
      (user-error "Agent path is already reserved: %s" path))
    (when (>= (mevedel-agent-control--active-count session)
              (mevedel-session-agent-turn-capacity session))
      (user-error "Agent tree is at its active-turn capacity"))
    (let ((record
           (mevedel-agent-record--create
            :path path
            :parent-path parent-path
            :role role
            :configuration (list :role role)
            :activity 'starting)))
      (mevedel-session--set-agent-reservations
       session (cons (cons path record) reservations))
      record)))

(defun mevedel-agent-control--rollback (session record)
  "Remove unpublished RECORD from SESSION and release its slot."
  (mevedel-session--set-agent-reservations
   session
   (assoc-delete-all (mevedel-agent-record-path record)
                     (mevedel-session-agent-reservations session)))
  (when-let* ((invocation (mevedel-agent-record-invocation record)))
    (mevedel-agent-runtime-abandon-persistence invocation)
    (when-let* ((buffer (mevedel-agent-invocation-buffer invocation))
                ((buffer-live-p buffer)))
      (with-current-buffer buffer
        (set-buffer-modified-p nil))
      (kill-buffer buffer)))
  (mevedel-agent-control--persist-session session)
  ;; The freed slot may let an agent waiting for one start.
  (mevedel-agent-control--schedule-agent-wakes session))

(defun mevedel-agent-control--bounded-result (record response)
  "Return RECORD's bounded terminal payload for RESPONSE."
  (let ((text (if (stringp response) response (format "%S" response))))
    (if (<= (length text) mevedel-agent-control--result-limit)
        text
      (let* ((location (mevedel-agent-record-conversation-location record))
             (suffix (format "\n\nFull transcript: %s"
                             (or location "unavailable")))
             (preview-size
              (max 2 (- mevedel-agent-control--result-limit
                        (length suffix) 128)))
             (preview
              (plist-get
               (mevedel--head-tail-preview-parts
                text text (length text) preview-size)
               :text)))
        (concat preview suffix)))))

(defun mevedel-agent-control-message-receipt (message)
  "Return hidden transcript metadata acknowledging MESSAGE's durable identity."
  (mevedel--format-hook-audit-record
   (list :type 'mailbox-receipt :id (plist-get message :id))))

(defun mevedel-agent-control--publish-result
    (session record result &optional staged)
  "Publish RECORD's canonical RESULT in SESSION.

Every turn first enqueues RESULT for the spawn parent.  A workflow may install
a one-shot result handler when it owns the parent interaction.  Successful
workflow delivery consumes that exact queued record; a failed handler leaves
it queued for ordinary parent delivery.  When STAGED is non-nil, the caller
already committed RESULT."
  (let* ((recipient (mevedel-agent-record-parent-path record))
         (handler (mevedel-agent-record-result-handler record)))
    (unless staged
      (mevedel-agent-control--identify-record result)
      (plist-put result :uncommitted t)
      (let ((previous
             (mevedel-agent-control--mailbox-queue session recipient)))
        (mevedel-agent-control--set-mailbox-queue
         session recipient (cons result previous))
        (condition-case err
            (mevedel-agent-control-commit-session session)
          (error
           (mevedel-agent-control--set-mailbox-queue
            session recipient
            (delq result (copy-sequence
                          (mevedel-agent-control--mailbox-queue session recipient))))
           (signal (car err) (cdr err)))))
      (cl-remf result :uncommitted))
    (if (not handler)
        (mevedel-agent-control--deliver-mail session recipient result)
      (let ((buffer (if (equal recipient "/root")
                        (mevedel-session-root-buffer session)
                      (mevedel-agent-record-conversation-buffer
                       (mevedel-agent-control--record-at-path session recipient)))))
        (condition-case err
            (progn
              (funcall handler result)
              ;; A workflow may save inside its callback.  Only acknowledge
              ;; after success; a prior callback save leaves durable redelivery
              ;; until a later checkpoint includes this receipt as well.
              (if (buffer-live-p buffer)
                  (with-current-buffer buffer
                    (let ((inhibit-read-only t))
                      (save-excursion
                        (goto-char (point-max))
                        (insert (mevedel-agent-control-message-receipt result)))))
                (when (mevedel-session-save-path session)
                  (error "Result recipient has no receiving transcript")))
              (mevedel-session-checkpoint-consume session recipient (list result))
              (mevedel-agent-control--set-mailbox-queue
               session recipient
               (cl-remove (plist-get result :id)
                          (mevedel-agent-control--mailbox-queue session recipient)
                          :key (lambda (message) (plist-get message :id))
                          :test #'equal))
              (setf (mevedel-agent-record-result-handler record) nil))
          (error
           (setf (mevedel-agent-record-result-handler record) nil)
           (display-warning
            'mevedel
            (format "Agent result handler failed for %s: %s"
                    (mevedel-agent-record-path record)
                    (error-message-string err))
            :warning)
           (mevedel-agent-control--deliver-mail session recipient result)))))))

(defun mevedel-agent-control--settlement-mail-id (invocation key)
  "Return INVOCATION's outgoing message ID for logical completion KEY.
Keep IDs through failed settlement reconstruction, not only publisher retry.
The delivery closure releases this bounded producer state on success."
  (let* ((info (mevedel-engine-info invocation))
         (ids (plist-get info :mevedel-settlement-mail-ids)))
    (or (cdr (assq key ids))
        (let ((id (plist-get (mevedel-agent-control--identify-record (list :id nil)) :id)))
          (setf (mevedel-engine-info invocation)
                (plist-put info :mevedel-settlement-mail-ids (cons (cons key id) ids)))
          id))))

(defun mevedel-agent-control--settle
    (session record invocation response &optional event token)
  "Stage RECORD's terminal RESPONSE and EVENT in SESSION.
Return rollback and post-commit delivery closures for INVOCATION."
  (mevedel-session-publication-barrier session token)
  (when (and (eq invocation (mevedel-agent-record-invocation record))
             (mevedel-agent-control--active-p record)
             (progn
               ;; Target replacement invalidates grants and may publish a
               ;; sidecar.  Do this before exposing tentative terminal state
               ;; or capturing the rollback snapshot, never under its guard.
               (mevedel-session-artifacts-assert-mutation-authority
                session (mevedel-session-root-buffer session))
               ;; The authority fence can yield to another terminal callback.
               (and (eq invocation (mevedel-agent-record-invocation record))
                    (mevedel-agent-control--active-p record))))
    (let* ((outcome
            (pcase (mevedel-agent-invocation-transcript-status invocation)
              ('error 'errored)
              ('aborted 'interrupted)
              (_ (if (and (stringp response)
                          (string-prefix-p "Error:" response))
                     'errored
                   'completed))))
           (payload
            (if (eq outcome 'errored)
                (or (and (listp event) (plist-get event :error-details))
                    (mevedel-agent-invocation-terminal-reason invocation)
                    "Agent turn failed")
              response))
           (recipient (mevedel-agent-record-parent-path record))
           (previous-queue
            (mevedel-agent-control--mailbox-queue session recipient))
           (previous-activity (mevedel-agent-record-activity record))
           (previous-invocation (mevedel-agent-record-invocation record))
           (previous-blockers (mevedel-agent-record-blockers record))
           (previous-result (mevedel-agent-record-settled-result record))
           (previous-outcome (mevedel-agent-record-settled-outcome record))
           (previous-metadata (mevedel-agent-record-model-metadata record))
           ;; Completions of the agent's own Bash it never read go to the
           ;; caller with the result.
           (own-path (mevedel-agent-record-path record))
           (own-queue (mevedel-agent-record-mailbox record))
           (unread-executions          ; newest first, like the queue
            (cl-remove-if-not
             (lambda (item)
               (and (eq (plist-get item :type) 'EXECUTION)
                    (equal (plist-get item :sender) own-path)))
             own-queue))
           ;; Normalized input plus output, excluding cached input, while the
           ;; child engine context is still attached.  Callers may charge it elsewhere.
           (usage
            (when-let* ((tokens (plist-get (mevedel-engine-info invocation) :tokens-full)))
              (+ (or (plist-get tokens :input) 0)
                 (or (plist-get tokens :output) 0))))
           result staged-records turn-closed)
      (cl-labels
          ((rollback ()
             (setf (mevedel-agent-record-activity record) previous-activity
                   (mevedel-agent-record-invocation record)
                   previous-invocation
                   (mevedel-agent-record-blockers record) previous-blockers
                   (mevedel-agent-record-settled-result record)
                   previous-result
                   (mevedel-agent-record-settled-outcome record)
                   previous-outcome
                   (mevedel-agent-record-model-metadata record)
                   previous-metadata)
             (mevedel-agent-control--set-mailbox-queue
              session recipient
              (cl-remove-if (lambda (message) (memq message staged-records))
                            (mevedel-agent-control--mailbox-queue session recipient)))
             (setf (mevedel-agent-record-mailbox record)
                   (append (cl-set-difference
                            (mevedel-agent-record-mailbox record) own-queue :test #'eq)
                           own-queue)))
           (deliver ()
             (dolist (message staged-records) (cl-remf message :uncommitted))
             ;; Close the finished turn once.  A delivery retry can run after
             ;; the agent's next turn started; that turn's wait and wake
             ;; marks are its own.
             (unless turn-closed
               (mevedel-agent-control-cancel-wait session own-path)
               ;; A failed turn does not restart itself for mail it missed.
               (when (eq outcome 'errored)
                 (mevedel-agent-control-clear-agent-wakes session own-path))
               (setq turn-closed t))
             (mevedel-agent-control--publish-result
              session record result t)
             (setf (mevedel-engine-info invocation)
                   (plist-put (mevedel-engine-info invocation)
                              :mevedel-settlement-mail-ids nil))))
        (setq payload
              (mevedel-agent-control--record-settled-result
               record outcome payload))
        (mevedel-agent-control--capture-model-metadata record)
        (setf (mevedel-agent-record-activity record) 'idle
              (mevedel-agent-record-invocation record) nil
              (mevedel-agent-record-blockers record) nil)
        (setq result
              (mevedel-agent-control--identify-record
               (list :id (mevedel-agent-control--settlement-mail-id invocation 'result)
                     :type 'RESULT
                     :sender (mevedel-agent-record-path record)
                     :recipient recipient
                     :outcome outcome
                     :payload
                     (mevedel-agent-control--bounded-result record payload)
                     :timestamp (current-time))))
        (when usage
          (setq result (append result (list :usage usage))))
        (setq staged-records
              (append
               (mapcar
                (lambda (entry)
                  (mevedel-agent-control--identify-record
                   (append
                    (list :id (mevedel-agent-control--settlement-mail-id invocation entry)
                          :type 'EXECUTION
                          :sender (mevedel-agent-record-path record)
                          :recipient recipient :payload (car entry)
                          :timestamp (current-time))
                    (and (cdr entry) (list :meta (cdr entry))))))
                ;; Completions after the answer are newer.  Runtime entries have
                ;; no identity yet; queued completions retain theirs.
                (mevedel-agent-invocation-runtime-execution-results invocation))
               (mapcar (lambda (item)
                         (plist-put (copy-sequence item) :recipient recipient))
                       unread-executions)
               (list result)))
        (dolist (message staged-records) (plist-put message :uncommitted t))
        (mevedel-agent-control--set-mailbox-queue
         session recipient (append staged-records previous-queue))
        (when unread-executions
          (setf (mevedel-agent-record-mailbox record)
                (cl-remove-if (lambda (item) (memq item unread-executions))
                              own-queue)))
        (cons #'rollback #'deliver)))))

(defun mevedel-agent-control--record-invocation
    (session record invocation)
  "Publish RECORD with INVOCATION in SESSION's retained registry."
  (mevedel-session-publication-barrier session)
  (unless (equal (mevedel-agent-invocation-path invocation)
                 (mevedel-agent-record-path record))
    (error "Agent invocation path mismatch: %s"
           (mevedel-agent-invocation-path invocation)))
  (let* ((path (mevedel-agent-record-path record))
         (reservation
          (assoc path (mevedel-session-agent-reservations session)))
         (published (assoc path (mevedel-session-agent-registry session)))
         (previous-reservations
          (mevedel-session-agent-reservations session))
         (previous-registry (mevedel-session-agent-registry session))
         (previous
          (list (mevedel-agent-record-id record)
                (mevedel-agent-record-configuration record)
                (mevedel-agent-record-activity record)
                (mevedel-agent-record-conversation-location record)
                (mevedel-agent-record-conversation-buffer record)
                (mevedel-agent-record-invocation record)
                (mevedel-agent-record-blockers record)
                (mevedel-agent-record-settled-result record)
                (mevedel-agent-record-settled-outcome record)
                (mevedel-agent-record-model-metadata record))))
    (unless (or reservation (eq (cdr published) record))
      (error "Agent record is not reserved or published: %s" path))
    (setf (mevedel-agent-record-id record)
          (mevedel-agent-invocation-agent-id invocation)
          (mevedel-agent-record-configuration record)
          (mevedel-agent-invocation-frozen-configuration invocation)
          (mevedel-agent-record-activity record) 'running
          (mevedel-agent-record-conversation-location record)
          (mevedel-agent-invocation-transcript-relative-path invocation)
          (mevedel-agent-record-conversation-buffer record)
          (mevedel-agent-invocation-buffer invocation)
          (mevedel-agent-record-invocation record) invocation
          (mevedel-agent-record-blockers record) nil
          (mevedel-agent-record-settled-result record) nil
          (mevedel-agent-record-settled-outcome record) nil)
    (mevedel-agent-control--capture-model-metadata record)
    (when reservation
      (mevedel-session--set-agent-reservations
       session (assoc-delete-all path previous-reservations))
      (mevedel-session--set-agent-registry
       session (cons (cons path record) previous-registry)))
    (condition-case err
        (mevedel-agent-control-commit-session session)
      (error
       (mevedel-session--set-agent-reservations
        session previous-reservations)
       (mevedel-session--set-agent-registry session previous-registry)
       (setf (mevedel-agent-record-id record) (nth 0 previous)
             (mevedel-agent-record-configuration record) (nth 1 previous)
             (mevedel-agent-record-activity record) (nth 2 previous)
             (mevedel-agent-record-conversation-location record)
             (nth 3 previous)
             (mevedel-agent-record-conversation-buffer record)
             (nth 4 previous)
             (mevedel-agent-record-invocation record) (nth 5 previous)
             (mevedel-agent-record-blockers record) (nth 6 previous)
             (mevedel-agent-record-settled-result record) (nth 7 previous)
             (mevedel-agent-record-settled-outcome record) (nth 8 previous)
             (mevedel-agent-record-model-metadata record) (nth 9 previous))
       (signal (car err) (cdr err))))))

(defun mevedel-agent-control--set-hook-context (record entries)
  "Replace RECORD's pending hook context with a copy of ENTRIES."
  (setf (mevedel-agent-record-hook-context-pending record)
        (copy-tree entries)))

(defun mevedel-agent-control--dispatch-followup
    (session record message &optional parent-tool-use-id plan-read-only)
  "Dispatch MESSAGE as RECORD's next provider turn in SESSION.
PARENT-TOOL-USE-ID and PLAN-READ-ONLY are as in
`mevedel-agent-control--activate'."
  (let ((buffer (mevedel-agent-persistence-ensure-conversation
                 session record (mevedel-session-root-buffer session))))
    (let ((configuration (mevedel-agent-record-configuration record)))
      (unless (mevedel-agent-configuration-p configuration)
        (error "Agent has no frozen configuration: %s"
               (mevedel-agent-record-path record)))
      (unless
          (mevedel-agent-runtime-dispatch
           nil (file-name-nondirectory (mevedel-agent-record-path record))
           message
           :path (mevedel-agent-record-path record)
           :frozen-configuration configuration
           :retained-id (mevedel-agent-record-id record)
           :retained-buffer buffer
           :retained-transcript
           (mevedel-agent-record-conversation-location record)
           :pending-hook-context
           (mevedel-agent-record-hook-context-pending record)
           :parent-tool-use-id parent-tool-use-id
           :plan-read-only plan-read-only
           :on-hook-context
           (apply-partially
            #'mevedel-agent-control--set-hook-context record)
           :on-invocation
           (apply-partially
            #'mevedel-agent-control--record-invocation session record)
           :on-settle
           (apply-partially
            #'mevedel-agent-control--settle session record))
        (error "Agent provider request did not start")))
    record))

(defun mevedel-agent-control--capacity-p (session)
  "Return non-nil when SESSION's tree can admit one more active turn."
  (< (mevedel-agent-control--active-count session)
     (mevedel-session-agent-turn-capacity session)))

(defun mevedel-agent-control--startable-p (session record)
  "Return non-nil when idle RECORD in SESSION may start its next turn.
Settlement marks a record idle before its RESULT commits and before the
runtime has released the finished invocation.  Meanwhile a rollback may
restore that turn and its publication rejects nested commits, so no turn
starts while a settlement in SESSION runs.  Another session's settlement
re-offers only its own session, so it does not hold this one.
Waiting mail gets the offer `mevedel-agent-control-settlement-finished'
makes."
  (and (eq (mevedel-agent-record-activity record) 'idle)
       (not (cl-some (lambda (invocation)
                       (eq session (mevedel-agent-invocation-parent-session
                                    invocation)))
                     mevedel-agent-control-settling))))

(defun mevedel-agent-control-settlement-finished (session)
  "Offer SESSION's idle agents turns now that a settlement call has ended.
The runtime calls this whether the settlement committed, rolled back, or
failed.  Mail that arrived during a final answer or while waiting for a
slot gets its offer."
  (mevedel-agent-control--schedule-agent-wakes session))

(defun mevedel-agent-control--activate
    (session record message &optional parent-tool-use-id plan-read-only)
  "Start idle RECORD's next turn in SESSION with MESSAGE as its prompt.
PARENT-TOOL-USE-ID identifies the calling tool row.  PLAN-READ-ONLY, or
unread mail from a Plan read-only sender, stamps the turn read-only even
without an ambient Plan caller: the turn reads that mail."
  (mevedel-session-publication-barrier session)
  (let ((previous-result (mevedel-agent-record-settled-result record))
        (previous-outcome (mevedel-agent-record-settled-outcome record))
        (plan-read-only
         (or plan-read-only
             (cl-some (lambda (message) (plist-get message :plan-read-only))
                      (mevedel-agent-record-mailbox record)))))
    ;; Provider startup can yield to another owner's mailbox save.
    ;; Keep the published registry valid throughout that interval.
    (setf (mevedel-agent-record-activity record) 'starting
          (mevedel-agent-record-settled-result record) nil
          (mevedel-agent-record-settled-outcome record) nil)
    (condition-case err
        (mevedel-agent-control--dispatch-followup
         session record message parent-tool-use-id plan-read-only)
      ((error quit)
       (setf (mevedel-agent-record-activity record) 'idle
             (mevedel-agent-record-invocation record) nil
             (mevedel-agent-record-settled-result record) previous-result
             (mevedel-agent-record-settled-outcome record) previous-outcome)
       (mevedel-agent-control--persist-session session)
       ;; Another agent may have queued for the slot this start held.
       (mevedel-agent-control--schedule-agent-wakes session)
       (signal (car err) (cdr err))))))


;;;; Waking idle agents

(defconst mevedel-agent-control--wake-prompt
  (concat "Messages arrived while you were idle; they follow. "
          "They come from other agents, not from the user.")
  "Fixed prompt of an agent turn started for queued mail.
The mail itself reaches the model as mailbox records at the turn's first
boundary, never as this prompt, so its text cannot pose as the user.")

(defun mevedel-agent-control--waking-records (record)
  "Return RECORD's committed unread records that should wake it."
  (cl-remove-if-not
   (lambda (message)
     (and (plist-get message :wake)
          (not (plist-get message :uncommitted))))
   (mevedel-agent-record-mailbox record)))

(defun mevedel-agent-control-clear-agent-wakes (session path)
  "Stop PATH's unread mail in SESSION from starting another turn.
The mail stays queued for the agent's next turn.  Mail sent later wakes it
again, so stopping an agent does not make it deaf."
  (when-let* ((record (cdr (assoc path (mevedel-session-agent-registry
                                        session)))))
    (dolist (message (mevedel-agent-record-mailbox record))
      (when (plist-get message :wake)
        (plist-put message :wake nil)))))

(defun mevedel-agent-control--schedule-agent-wakes (session)
  "Offer SESSION's idle agents turns for their waking mail.
The offer runs from a timer once the transport is idle, after the current
callback (often a settlement) has unwound.  Repeated offers coalesce."
  (when-let* ((buffer (mevedel-session-root-buffer session))
              ((buffer-live-p buffer)))
    (mevedel-transport-run-when-idle
     (list 'agent-wake session)
     (buffer-local-value 'default-directory buffer)
     (lambda () (mevedel-agent-control--offer-agent-wakes session))
     nil 0)))

(defun mevedel-agent-control--offer-agent-wakes (session)
  "Start idle agents' turns in SESSION for their waking mail.
Agents whose oldest waking mail is oldest go first, while the tree has
capacity; the rest wait for a later settlement to free a slot.  A turn that
cannot start leaves its mail unread for a later turn without retrying."
  (when-let* ((buffer (mevedel-session-root-buffer session))
              ((buffer-live-p buffer))
              ((eq session (buffer-local-value 'mevedel--session buffer))))
    (with-current-buffer buffer
      (let (candidates)
        (dolist (entry (mevedel-session-agent-registry session))
          (let ((record (cdr entry)))
            (when-let* (((eq (mevedel-agent-record-activity record) 'idle))
                        (waking (mevedel-agent-control--waking-records record)))
              ;; The queue is newest first, so its last waking record is oldest.
              (push (list (plist-get (car (last waking)) :timestamp) record)
                    candidates))))
        (dolist (candidate (sort candidates
                                 (lambda (a b) (time-less-p (car a) (car b)))))
          (let ((record (cadr candidate)))
            ;; An earlier start can yield: re-check this agent's state now.
            (when (and (mevedel-agent-control--waking-records record)
                       (mevedel-agent-control--startable-p session record)
                       (mevedel-agent-control--capacity-p session))
              (condition-case err
                  (mevedel-agent-control--activate
                   session record mevedel-agent-control--wake-prompt)
                ((error quit)
                 ;; Neither a failure nor a quit restarts the agent by itself.
                 (mevedel-agent-control-clear-agent-wakes
                  session (mevedel-agent-record-path record))
                 (message "mevedel: could not start %s for its mail: %s"
                          (mevedel-agent-record-path record)
                          (if (eq (car err) 'quit)
                              "quit"
                            (error-message-string err))))))))))))

(defun mevedel-agent-control-interrupt (session target)
  "Interrupt TARGET's current turn in SESSION and return its prior activity."
  (let* ((caller-path (mevedel-agent-control-current-path session))
         (path (mevedel-agent-control-resolve-path
                session caller-path target)))
    (when (equal path "/root")
      (user-error "InterruptAgent cannot target /root"))
    (when (equal path caller-path)
      (user-error "InterruptAgent cannot target the caller"))
    (let* ((record (mevedel-agent-control--record-at-path session path))
           (activity (mevedel-agent-record-activity record)))
      ;; An idle agent waiting for a slot stops waiting too.
      (mevedel-agent-control-clear-agent-wakes session path)
      (when (mevedel-agent-control--active-p record)
        (mevedel-agent-runtime-interrupt
         (mevedel-agent-record-invocation record)
         (format "interrupted by %s" caller-path)))
      (list :path path :previous-activity activity))))

(defun mevedel-agent-control-interrupt-user (session path)
  "Interrupt PATH's current turn in SESSION on the user's behalf.
Return non-nil when an active turn was interrupted; an idle agent is left
alone.  The agent stays retained, as after `InterruptAgent'."
  (when (equal path "/root")
    (user-error "Use the session's own cancel command for /root"))
  (let ((record (or (cdr (assoc path (mevedel-session-agent-registry session)))
                    (user-error "Agent %s is no longer available" path))))
    (mevedel-agent-control-clear-agent-wakes session path)
    (when (mevedel-agent-control--active-p record)
      (mevedel-agent-runtime-interrupt
       (mevedel-agent-record-invocation record) "interrupted by user")
      t)))

(defun mevedel-agent-control--summary-metadata (result)
  "Return disclosable provider metadata from context-summary RESULT."
  (list :backend
        (when-let* ((backend (plist-get result :backend)))
          (or (ignore-errors (gptel-backend-name backend))
              (format "%s" backend)))
        :model (plist-get result :model)
        :effort (plist-get result :effort)))

(cl-defun mevedel-agent-control-spawn
    (session task-name message callback
             &key role agent context model effort model-policy
             description on-invocation result-handler
             skill-permission-rules skill-hook-rules parent-tool-use-id)
  "Prepare and spawn TASK-NAME with MESSAGE asynchronously in SESSION.
CALLBACK receives a success, error, or aborted outcome plist.  Return an
idempotent cancellation thunk for the unpublished preparation transaction."
  (mevedel-agent-control--validate-spawn session task-name message)
  (unless (functionp callback)
    (error "Agent spawn callback must be a function"))
  (when (and role (null agent)
             (mevedel-agents-specs)
             (not (assoc-string role (mevedel-agents-specs))))
    (user-error "Agent role is not available to this session: %s" role))
  (let* ((resolved-agent
          (or agent (mevedel-agent-resolve-role role)))
         (resolved-agent
          (if skill-hook-rules
              (let ((copy (copy-mevedel-agent resolved-agent)))
                (setf (mevedel-agent-hook-rules copy)
                      (append (mevedel-agent-hook-rules copy)
                              skill-hook-rules))
                copy)
            resolved-agent))
         (role-name (mevedel-agent-name resolved-agent))
         (context (mevedel-agent-control--normalize-context context))
         (model-selector
          (mevedel-model-parse-selector model))
         (effort (mevedel-model-parse-effort effort))
         (model-policy
          (or model-policy
              (mevedel-model-resolve-workload
               (intern role-name) model-selector effort)))
         (caller-path (mevedel-agent-control-current-path session))
         (record (mevedel-agent-control--reserve
                  session caller-path task-name role-name))
         context-snapshot
         summary-evidence
         summary-metadata
         summary-cancel
         committed
         cancelled
         settled)
    (setf (mevedel-agent-record-result-handler record) result-handler)
    (cl-labels
        ((settle (outcome)
           (unless settled
             (setq settled t)
             (funcall callback outcome)))
         (fail (message)
           (unless settled
             (unless committed
               (mevedel-agent-control--rollback session record))
             (settle (list :outcome 'error :error message))))
         (start-provider (prepared)
           (unless (or settled cancelled)
             (if (not (eq (plist-get prepared :outcome) 'success))
                 (fail (or (plist-get prepared :error)
                           "Agent task preparation failed"))
               (condition-case err
                   (progn
                     (unless
                         (mevedel-agent-runtime-dispatch
                          resolved-agent (or description task-name) message
                          :context-snapshot context-snapshot
                          :model-policy model-policy
                          :skill-permission-rules skill-permission-rules
                          :prepared-turn (plist-get prepared :turn)
                          :start-hook-audits
                          (plist-get prepared :start-hook-audits)
                          :parent-tool-use-id parent-tool-use-id
                          :path (mevedel-agent-record-path record)
                          :on-invocation
                          (lambda (invocation)
                            (mevedel-agent-control--record-invocation
                             session record invocation)
                            (setq committed t)
                            (when on-invocation
                              (condition-case err
                                  (funcall on-invocation invocation)
                                (error
                                 (mevedel--warn-once
                                  'agent-invocation-observer
                                  "Agent invocation observer failed: %s"
                                  (error-message-string err))))))
                          :on-settle
                          (apply-partially
                           #'mevedel-agent-control--settle session record))
                       (error "Agent provider request did not start"))
                     (unless (mevedel-agent-record-conversation-location
                              record)
                       (error "Agent conversation could not be persisted"))
                     (settle (list :outcome 'success
                                   :record record
                                   ;; Launch snapshot: resolved before any
                                   ;; asynchronous provider acknowledgement.
                                   :model-metadata
                                   (mevedel-agent-control-record-model-metadata
                                    record)
                                   :summary-metadata summary-metadata)))
                 (error (fail (error-message-string err)))))))
         (summarize-then-start (prepared)
           ;; The hook-accepted task is the only focus input, so generation
           ;; can only start once preparation has produced it.
           (condition-case err
               (progn
                 (setq
                  summary-cancel
                  (mevedel-context-summary-generate
                   summary-evidence 'handoff
                   (lambda (result)
                     (unless (or settled cancelled)
                       (pcase (plist-get result :outcome)
                         ('success
                          (setq context-snapshot
                                (mevedel-agent-runtime-task-background
                                 (plist-get result :summary))
                                summary-metadata
                                (mevedel-agent-control--summary-metadata
                                 result))
                          (start-provider prepared))
                         ('aborted
                          (mevedel-agent-control--rollback session record)
                          (settle '(:outcome aborted)))
                         (_
                          (fail (or (plist-get result :error)
                                    "Agent context summary failed"))))))
                   :session session
                   :focus (plist-get (plist-get prepared :turn) :prompt))))
             (error (fail (error-message-string err)))))
         (prepared (result)
           (unless (or settled cancelled)
             (if (and (eq context 'summary)
                      (eq (plist-get result :outcome) 'success))
                 (summarize-then-start result)
               (start-provider result)))))
      (condition-case err
          (progn
            (if (eq context 'summary)
                (setq summary-evidence
                      (mevedel-compact-evidence-summary-context-evidence
                       parent-tool-use-id))
              (setq context-snapshot
                    (mevedel-compact-evidence-context-snapshot context)))
            (mevedel-agent-runtime-prepare-task
             resolved-agent (or description task-name) message
             (mevedel-agent-record-path record) #'prepared
             :skill-permission-rules skill-permission-rules
             :cancelled-p (lambda () cancelled)))
        (error (fail (error-message-string err))))
      (lambda ()
        (unless settled
          (setq cancelled t)
          (when summary-cancel
            (funcall summary-cancel))
          (mevedel-agent-control--rollback session record)
          (settle '(:outcome aborted)))))))

(provide 'mevedel-agent-control)

;;; mevedel-agent-control.el ends here
