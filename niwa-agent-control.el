;;; niwa-agent-control.el -- Durable agent-tree control -*- lexical-binding: t -*-

;;; Commentary:

;; Owns the path-addressed, session-scoped agent registry and the admission and
;; settlement transaction for asynchronous agent turns.  Provider execution
;; remains in `niwa-agent-runtime'; this module gives it durable identities,
;; tree-wide capacity, canonical RESULT/MAIL/USER records, retained mailboxes,
;; turn-local interruption, and the WaitAgent suspension and wake lifecycle.

;;; Code:

(require 'niwa-telemetry)
(require 'niwa-utilities)

(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x)
  ;; Struct setters need their expanders during standalone package compilation.
  (require 'niwa-agents))

;; `gptel'
(declare-function gptel-backend-name "ext:gptel" (backend))

;; `niwa-agent-conversation'
(declare-function niwa-agent-conversation-fenced-p
                  "niwa-agent-conversation" (invocation))
(declare-function niwa-agent-conversation-final-response
                  "niwa-agent-conversation" (invocation))
(declare-function niwa-agent-conversation-save
                  "niwa-agent-conversation" (invocation &optional deferred))
(autoload 'niwa-agent-conversation-fenced-p "niwa-agent-conversation")
(autoload 'niwa-agent-conversation-final-response
  "niwa-agent-conversation")
(autoload 'niwa-agent-conversation-save "niwa-agent-conversation")
(defvar niwa--agent-invocation)

;; `niwa-agent-directory'
(declare-function niwa-agent-directory-resolve
                  "niwa-agent-directory" (session directory))
(autoload 'niwa-agent-directory-resolve "niwa-agent-directory")

;; `niwa-agent-persistence'
(declare-function niwa-agent-persistence-ensure-conversation
                  "niwa-agent-persistence"
                  (session record root-buffer &optional readonly-p))
(autoload 'niwa-agent-persistence-ensure-conversation "niwa-agent-persistence")

;; `niwa-agent-runtime'
(declare-function niwa-agent-runtime-dispatch
                  "niwa-agent-runtime" t t)
(declare-function niwa-agent-runtime-abandon-persistence
                  "niwa-agent-runtime" (invocation))
(declare-function niwa-agent-runtime-interrupt
                  "niwa-agent-runtime" (invocation reason))
(declare-function niwa-agent-runtime-prepare-task
                  "niwa-agent-runtime" t t)
(declare-function niwa-agent-runtime-task-background
                  "niwa-agent-runtime" (summary))
(autoload 'niwa-agent-runtime-abandon-persistence
  "niwa-agent-runtime")
(autoload 'niwa-agent-runtime-dispatch "niwa-agent-runtime")
(autoload 'niwa-agent-runtime-interrupt "niwa-agent-runtime")
(autoload 'niwa-agent-runtime-prepare-task "niwa-agent-runtime")
(autoload 'niwa-agent-runtime-task-background "niwa-agent-runtime")

;; `niwa-agents'
(declare-function copy-niwa-agent "niwa-agents" (cl-x))
(declare-function niwa-agent-configuration-model-metadata
                  "niwa-agents" (configuration))
(declare-function niwa-agent-configuration-p
                  "niwa-agents" (cl-x))
(declare-function niwa-agent-hook-rules "niwa-agents" (cl-x) t)
(declare-function niwa-agent-invocation-agent-id
                  "niwa-agents" (cl-x) t)
(declare-function niwa-agent-invocation-buffer
                  "niwa-agents" (cl-x) t)
(declare-function niwa-agent-invocation-frozen-configuration
                  "niwa-agents" (cl-x) t)
(declare-function niwa-agent-invocation-parent-session
                  "niwa-agents" (cl-x) t)
(declare-function niwa-agent-invocation-path
                  "niwa-agents" (cl-x) t)
(declare-function niwa-agent-invocation-plan-read-only
                  "niwa-agents" (cl-x) t)
(declare-function niwa-agent-invocation-terminal-reason
                  "niwa-agents" (cl-x) t)
(declare-function niwa-agent-invocation-transcript-relative-path
                  "niwa-agents" (cl-x) t)
(declare-function niwa-agent-invocation-transcript-status
                  "niwa-agents" (cl-x) t)
(declare-function niwa-agent-name "niwa-agents" (cl-x) t)
(declare-function niwa-agent-resolve-role "niwa-agents" (role))
(declare-function niwa-agents-specs "niwa-agents" (&optional buffer))
(declare-function niwa-plan-read-only-p "niwa-agents"
                  (&optional session))
(autoload 'copy-niwa-agent "niwa-agents")
(autoload 'niwa-agent-configuration-model-metadata "niwa-agents")
(autoload 'niwa-agent-configuration-p "niwa-agents")
(autoload 'niwa-agent-hook-rules "niwa-agents")
(autoload 'niwa-agent-invocation-agent-id "niwa-agents")
(autoload 'niwa-agent-invocation-buffer "niwa-agents")
(autoload 'niwa-agent-invocation-frozen-configuration "niwa-agents")
(autoload 'niwa-agent-invocation-parent-session "niwa-agents")
(autoload 'niwa-agent-invocation-path "niwa-agents")
(autoload 'niwa-agent-invocation-plan-read-only "niwa-agents")
(autoload 'niwa-agent-invocation-terminal-reason "niwa-agents")
(autoload 'niwa-agent-invocation-transcript-relative-path "niwa-agents")
(autoload 'niwa-agent-invocation-transcript-status "niwa-agents")
(autoload 'niwa-agent-name "niwa-agents")
(autoload 'niwa-agent-resolve-role "niwa-agents")
(autoload 'niwa-plan-read-only-p "niwa-agents")
(autoload 'niwa-agents-specs "niwa-agents")

;; `niwa-compact-evidence'
(declare-function niwa-compact-evidence-context-snapshot
                  "niwa-compact-evidence" (context))
(declare-function niwa-compact-evidence-summary-context-evidence
                  "niwa-compact-evidence" (tool-use-id))
(autoload 'niwa-compact-evidence-context-snapshot
  "niwa-compact-evidence")
(autoload 'niwa-compact-evidence-summary-context-evidence
  "niwa-compact-evidence")

;; `niwa-context-summary'
(declare-function niwa-context-summary-generate
                  "niwa-context-summary" t t)
(autoload 'niwa-context-summary-generate "niwa-context-summary")

;; `niwa-engine'
(declare-function niwa-engine-info "niwa-engine" (owner))

;; `niwa-models'
(declare-function niwa-model-parse-effort "niwa-models" (value))
(declare-function niwa-model-parse-selector "niwa-models" (value))
(declare-function niwa-model-resolve-workload
                  "niwa-models"
                  (workload &optional explicit-selector explicit-effort))
(autoload 'niwa-model-parse-effort "niwa-models")
(autoload 'niwa-model-parse-selector "niwa-models")
(autoload 'niwa-model-resolve-workload "niwa-models")

;; `niwa-pending-inputs'
(declare-function niwa-pending-inputs-offer-root-event
                  "niwa-pending-inputs" (session &optional data-buffer))
(autoload 'niwa-pending-inputs-offer-root-event "niwa-pending-inputs")

;; `niwa-session-artifacts'
(declare-function niwa-session-artifacts--sidecar-artifact
                  "niwa-session-artifacts" (session buffer &optional checkpoint))
(declare-function niwa-session-artifacts-assert-mutation-authority
                  "niwa-session-artifacts" (session &optional buffer))
(autoload 'niwa-session-artifacts-assert-mutation-authority
  "niwa-session-artifacts")

;; `niwa-session-checkpoint'
(declare-function niwa-session-checkpoint-consume
                  "niwa-session-checkpoint" (session conversation records))
(declare-function niwa-session-checkpoint-pending-message-p
                  "niwa-session-checkpoint" (session conversation id))
(autoload 'niwa-session-checkpoint-consume "niwa-session-checkpoint")
(autoload 'niwa-session-checkpoint-pending-message-p "niwa-session-checkpoint")

;; `niwa-session-durability'
(declare-function niwa-session-durability-call-outside-transaction
                  "niwa-session-durability" (function))
(autoload 'niwa-session-durability-call-outside-transaction
  "niwa-session-durability")

;; `niwa-session-persistence'
(declare-function niwa-session-persistence-cancel-deferred-agent-save
                  "niwa-session-persistence" (session))
(declare-function niwa-session-persistence-save-agent-state
                  "niwa-session-persistence" (session &optional token))
(declare-function niwa-session-persistence-save-agent-state-soon
                  "niwa-session-persistence" (session))
(autoload 'niwa-session-persistence-cancel-deferred-agent-save
  "niwa-session-persistence")
(autoload 'niwa-session-persistence-save-agent-state
  "niwa-session-persistence")
(autoload 'niwa-session-persistence-save-agent-state-soon
  "niwa-session-persistence")

;; `niwa-session-publication'
(declare-function niwa-session-publication-async-eligible-p "niwa-session-publication" (session))
(declare-function niwa-session-publication-barrier "niwa-session-publication" (session &optional token))
(declare-function niwa-session-publication-busy-p "niwa-session-publication" (session))
(declare-function niwa-session-publication-defer "niwa-session-publication" (session preparation &optional failure token))
(declare-function niwa-session-publication-enqueue "niwa-session-publication" (session preparation &optional failure))
(declare-function niwa-session-publication-submit "niwa-session-publication" (session prepare acknowledge &optional mail-ids retain-failure select-staged superseded))
(autoload 'niwa-session-publication-async-eligible-p "niwa-session-publication")
(autoload 'niwa-session-publication-barrier "niwa-session-publication")
(autoload 'niwa-session-publication-busy-p "niwa-session-publication")
(autoload 'niwa-session-publication-defer "niwa-session-publication")
(autoload 'niwa-session-publication-enqueue "niwa-session-publication")
(autoload 'niwa-session-publication-submit "niwa-session-publication")

;; `niwa-structs'
(declare-function niwa-session-root-buffer "niwa-structs" (cl-x) t)
(declare-function niwa-agent-path-p "niwa-structs" (path))
(declare-function niwa-session--set-agent-registry
                  "niwa-structs" (session registry))
(declare-function niwa-session--set-agent-reservations
                  "niwa-structs" (session reservations))
(declare-function niwa-session--set-agent-root-waiter
                  "niwa-structs" (session waiter))
(declare-function niwa-session--set-messages
                  "niwa-structs" (session messages))
(declare-function niwa-session-agent-registry
                  "niwa-structs" (cl-x) t)
(declare-function niwa-session-agent-reservations
                  "niwa-structs" (cl-x) t)
(declare-function niwa-session-agent-root-activity
                  "niwa-structs" (cl-x) t)
(declare-function niwa-session-agent-root-waiter
                  "niwa-structs" (cl-x) t)
(declare-function niwa-session-agent-turn-capacity
                  "niwa-structs" (cl-x) t)
(declare-function niwa-session-messages
                  "niwa-structs" (cl-x) t)
(declare-function niwa-session-p "niwa-structs" (object))
(declare-function niwa-session-save-path
                  "niwa-structs" (cl-x) t)

;; `niwa-telemetry'
(declare-function niwa-telemetry-record
                  "niwa-telemetry" (session event &rest props))

;; `niwa-transcript-audit'
(declare-function niwa--format-hook-audit-record
                  "niwa-transcript-audit" (record))
(autoload 'niwa--format-hook-audit-record "niwa-transcript-audit")

;; `niwa-transport'
(declare-function niwa-transport-busy-p "niwa-transport" (&optional path))
(declare-function niwa-transport-cancel-pending
                  "niwa-transport" (&optional key))
(declare-function niwa-transport-run-at-time
                  "niwa-transport" (seconds function &rest args))
(declare-function niwa-transport-run-when-idle
                  "niwa-transport"
                  (key path thunk &optional on-cancel delay))
(autoload 'niwa-transport-busy-p "niwa-transport")
(autoload 'niwa-transport-cancel-pending "niwa-transport")
(autoload 'niwa-transport-run-at-time "niwa-transport")
(autoload 'niwa-transport-run-when-idle "niwa-transport")

;; `niwa-utilities'
(declare-function niwa--clamped-integer
                  "niwa-utilities" (value default minimum maximum))
(declare-function niwa--head-tail-preview-parts
                  "niwa-utilities"
                  (head tail total-length &optional preview-size))
(declare-function niwa--warn-once
                  "niwa-utilities" (scope key format &rest args))

(defconst niwa-agent-control--result-limit (* 32 1024)
  "Maximum number of characters in an inline RESULT payload.")

(defconst niwa-agent-control--wait-default-ms 30000
  "Default WaitAgent timeout in milliseconds.")

(defconst niwa-agent-control--wait-min-ms 10000
  "Minimum WaitAgent timeout in milliseconds.")

(defconst niwa-agent-control--wait-max-ms 3600000
  "Maximum WaitAgent timeout in milliseconds.")

(defconst niwa-agent-control--active-activities
  '(starting running waiting permission-blocked interaction-blocked)
  "Persisted agent activities that own one active-turn slot.")

(defun niwa-agent-control-active-activity-p (activity)
  "Return non-nil when ACTIVITY owns one active-turn slot."
  (memq activity niwa-agent-control--active-activities))

(defvar niwa-agent-control-suppress-persistence nil
  "Non-nil while a multi-step registry repair must be persisted atomically.")

(defvar niwa-agent-control-settling nil
  "Agent invocations whose settlement is running, innermost first.
The runtime binds this around each settlement.  A turn that started
meanwhile could race the staged terminal state: its capacity release,
rollback, publication, or release of the finished invocation.  Code nested
inside a settlement, timers included, sees the binding.")

(cl-defstruct (niwa-agent-waiter
               (:constructor niwa-agent-waiter--create))
  "One suspended ordinary asynchronous WaitAgent tool call."
  callback
  release
  started-at
  timer)

(cl-defstruct (niwa-agent-record
               (:constructor niwa-agent-record--create))
  "One retained non-root agent identity in a root session."
  id
  path
  parent-path
  role
  working-directory
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
  ;; `niwa-agent-configuration-model-metadata' returns it.  Runtime
  ;; provider recovery never rewrites CONFIGURATION, so this retains what
  ;; actually served the agent across settlement and cold restore.
  model-metadata)

(defun niwa-agent-control--capture-model-metadata (record)
  "Retain the provider metadata of RECORD's current invocation, if any."
  (when-let* ((invocation (niwa-agent-record-invocation record))
              (metadata (niwa-agent-configuration-model-metadata
                         (niwa-agent-invocation-frozen-configuration
                          invocation))))
    (setf (niwa-agent-record-model-metadata record) metadata)))

(defun niwa-agent-control-record-model-metadata (record)
  "Return the provider metadata currently describing retained RECORD, or nil.
A running turn reports its invocation's effective configuration, including
provider recovery and external acknowledgement.  Otherwise report the latest
turn's retained metadata, then the frozen configuration.  The value has the
shape of `niwa-agent-configuration-model-metadata'.  No buffer or file is
read, so redraws may call this per record."
  (or (when-let* ((invocation (niwa-agent-record-invocation record)))
        (niwa-agent-configuration-model-metadata
         (niwa-agent-invocation-frozen-configuration invocation)))
      (niwa-agent-record-model-metadata record)
      (niwa-agent-configuration-model-metadata
       (niwa-agent-record-configuration record))))

(defun niwa-agent-control-model-metadata (session path)
  "Return provider metadata for SESSION's retained agent at PATH, or nil.
See `niwa-agent-control-record-model-metadata'."
  (when-let* ((record (cdr (assoc path (niwa-session-agent-registry
                                        session)))))
    (niwa-agent-control-record-model-metadata record)))

(defun niwa-agent-control--active-p (record)
  "Return non-nil when RECORD owns one active-turn slot."
  (niwa-agent-control-active-activity-p
   (niwa-agent-record-activity record)))

(defun niwa-agent-control--assert-plan-steering (session path record)
  "Signal unless the caller may message PATH's RECORD in SESSION.
A caller under Plan read-only authority may address /root, an idle agent,
whose next turn it stamps read-only, or a turn that is itself Plan
read-only.  Mail for any other running agent would act under that agent's
own authority."
  (when (and (niwa-plan-read-only-p session)
             record
             (niwa-agent-control--active-p record)
             (not (when-let* ((invocation
                               (niwa-agent-record-invocation record)))
                    (niwa-agent-invocation-plan-read-only invocation))))
    (user-error
     "SendMessage cannot reach %s from Plan: its running turn is not bound \
by Plan limits.  Message it once it is idle to start a read-only turn"
     path)))

(defun niwa-agent-control-settled-result (record)
  "Return RECORD's settled result plist when it is idle, or nil.

The returned plist contains the terminal `:outcome' and the complete
unbounded `:payload'.  Active turns deliberately expose no previous result."
  (when (and (niwa-agent-record-p record)
             (eq (niwa-agent-record-activity record) 'idle)
             (stringp (niwa-agent-record-settled-result record))
             (memq (niwa-agent-record-settled-outcome record)
                   '(completed errored interrupted)))
    (list :outcome (niwa-agent-record-settled-outcome record)
          :payload (copy-sequence
                    (niwa-agent-record-settled-result record)))))

(defun niwa-agent-control--record-settled-result
    (record outcome payload)
  "Store complete PAYLOAD and OUTCOME as RECORD's settled result.
Return the normalized payload stored in the record."
  (let ((payload (if (stringp payload) payload (format "%S" payload))))
    (unless (memq outcome '(completed errored interrupted))
      (error "Invalid agent result outcome: %s" outcome))
    (setf (niwa-agent-record-settled-result record) payload
          (niwa-agent-record-settled-outcome record) outcome)
    payload))

(defun niwa-agent-control-commit-session (session &optional token)
  "Commit SESSION's materialized agent state or signal an error."
  (unless niwa-agent-control-suppress-persistence
    (when (niwa-session-save-path session)
      (unless (niwa-session-persistence-save-agent-state session token)
        (error "Agent state could not be persisted"))))
  t)

(defun niwa-agent-control--persist-session (session)
  "Best-effort persist SESSION's changed agent state.

Observational transitions -- blocked/waiting activity flavors, explicit
mailbox discards -- debounce into one non-forced save instead of a full
synchronous publication each; the next acknowledged commit absorbs a
pending one.  Recovery treats every active activity identically, so a
crash inside the window costs at most a stale activity flavor or a
repeated discard.  Delivered mail uses a transcript checkpoint instead."
  (unless niwa-agent-control-suppress-persistence
    (condition-case nil
        (niwa-session-persistence-save-agent-state-soon session)
      (error nil))))

(defun niwa-agent-control-block-turn (session path activity)
  "Block PATH's current turn in SESSION with durable ACTIVITY.

ACTIVITY must be `waiting', `permission-blocked', or
`interaction-blocked'.  Return an idempotent release closure bound to the
current invocation, or nil when PATH has no active retained turn.  Overlapping
blockers compose and stale releases cannot alter a later follow-up."
  (unless (memq activity '(waiting permission-blocked interaction-blocked))
    (error "Invalid blocked agent activity: %s" activity))
  (when-let* (((not (equal path "/root")))
              (entry (assoc path (niwa-session-agent-registry session)))
              (record (cdr entry))
              ((niwa-agent-control--active-p record))
              (invocation (niwa-agent-record-invocation record)))
    (let ((token (list activity)))
      (push token (niwa-agent-record-blockers record))
      (unless (eq activity (niwa-agent-record-activity record))
        (setf (niwa-agent-record-activity record) activity)
        (niwa-agent-control--persist-session session))
      (lambda ()
        (when (and (eq record
                       (cdr (assoc path
                                   (niwa-session-agent-registry session))))
                   (eq invocation (niwa-agent-record-invocation record))
                   (memq token (niwa-agent-record-blockers record)))
          (setf (niwa-agent-record-blockers record)
                (delq token (niwa-agent-record-blockers record)))
          (let ((next (or (caar (niwa-agent-record-blockers record))
                          'running)))
            (unless (eq next (niwa-agent-record-activity record))
              (setf (niwa-agent-record-activity record) next)
              (niwa-agent-control--persist-session session))))))))

(defun niwa-agent-control--active-count (session)
  "Return the number of active non-root turns in SESSION."
  (+ (length (niwa-session-agent-reservations session))
     (cl-count-if
      (lambda (entry)
        (niwa-agent-control--active-p (cdr entry)))
      (niwa-session-agent-registry session))))

(defun niwa-agent-control-active-turn-p (session)
  "Return non-nil when SESSION has an active non-root agent turn."
  (> (niwa-agent-control--active-count session) 0))

(defun niwa-agent-control-retained-buffer-p (session buffer)
  "Return non-nil when SESSION retains BUFFER as an agent conversation."
  (and (niwa-session-p session)
       (buffer-live-p buffer)
       (cl-some
        (lambda (entry)
          (eq buffer
              (niwa-agent-record-conversation-buffer (cdr entry))))
        (niwa-session-agent-registry session))))

(defun niwa-agent-control-teardown-session (session)
  "Kill every retained conversation buffer owned by SESSION."
  (when (niwa-session-p session)
    ;; The caller owns final persistence; a debounced save firing into
    ;; killed buffers would only race it.
    (niwa-session-persistence-cancel-deferred-agent-save session)
    (niwa-agent-control--cancel-notifications session)
    (niwa-transport-cancel-pending (list 'agent-wake session))
    (niwa-session--set-agent-reservations session nil)
    (niwa-agent-control-cancel-wait session "/root")
    (dolist (entry (niwa-session-agent-registry session))
      (niwa-agent-control-cancel-wait session (car entry))
      (when-let* ((buffer
                   (niwa-agent-record-conversation-buffer (cdr entry)))
                  ((buffer-live-p buffer)))
        (let ((saved-p
               (with-current-buffer buffer
                 (or (not (buffer-modified-p))
                     (when niwa--agent-invocation
                       (niwa-agent-conversation-save
                        niwa--agent-invocation))))))
          (if saved-p
              (kill-buffer buffer)
            (niwa-diagnostic-warning
             (niwa-diagnostic-scope session) 'niwa
             (format
              "Retained agent transcript could not be saved during session teardown: %s"
              (car entry))
             :warning)))))))

(defun niwa-agent-control--path-for-invocation (session invocation)
  "Return INVOCATION's retained canonical path in SESSION."
  (let ((path (niwa-agent-invocation-path invocation)))
    (and (niwa-agent-path-p path)
         (assoc path (niwa-session-agent-registry session))
         path)))

(defun niwa-agent-control-current-path (session)
  "Return the current caller's canonical path in SESSION."
  (let ((invocation niwa--agent-invocation))
    (if (not invocation)
        "/root"
      (or (niwa-agent-control--path-for-invocation session invocation)
          (error "Current agent is not registered: %s"
                 (niwa-agent-invocation-path invocation))))))

(defun niwa-agent-control-resolve-path (session caller-path target)
  "Resolve TARGET from CALLER-PATH to an addressable path in SESSION."
  (unless (niwa-agent-path-p caller-path)
    (error "Invalid caller agent path: %s" caller-path))
  (unless (and (stringp target) (not (string-empty-p target)))
    (user-error "Agent target must be a non-empty path"))
  (let ((path
         (if (string-prefix-p "/" target)
             target
           (concat caller-path "/" target))))
    (unless (niwa-agent-path-p path)
      (user-error "Invalid agent target: %s" target))
    (unless (or (equal path "/root")
                (assoc path (niwa-session-agent-registry session)))
      (user-error "Unknown agent target: %s" target))
    path))

(defun niwa-agent-control--record-at-path (session path)
  "Return SESSION's retained record at PATH, or signal an error."
  (or (cdr (assoc path (niwa-session-agent-registry session)))
      (error "Unknown agent path: %s" path)))

(defun niwa-agent-control--mailbox-queue (session path)
  "Return PATH's reverse-order unread queue in SESSION."
  (if (equal path "/root")
      (niwa-session-messages session)
    (niwa-agent-record-mailbox
     (niwa-agent-control--record-at-path session path))))

(defun niwa-agent-control--set-mailbox-queue (session path queue)
  "Set PATH's reverse-order unread QUEUE in SESSION."
  (if (equal path "/root")
      (niwa-session--set-messages session queue)
    (setf (niwa-agent-record-mailbox
           (niwa-agent-control--record-at-path session path))
          queue)))

(defun niwa-agent-control--mailbox (session path)
  "Return PATH's committed unread records in SESSION in FIFO order.
Producer publication can yield to a receiving WAIT.  Tentative records stay
serializable for that publication, but cannot enter a receiving transcript."
  (cl-remove-if (lambda (message) (plist-get message :uncommitted))
                (reverse (niwa-agent-control--mailbox-queue session path))))

(defun niwa-agent-control-context-mailbox (context)
  "Return CONTEXT's retained unread records in FIFO order."
  (if (niwa-session-p context)
      (niwa-agent-control--mailbox context "/root")
    (let* ((session (niwa-agent-invocation-parent-session context))
           (path (and session
                      (niwa-agent-control--path-for-invocation
                       session context))))
      (and path (niwa-agent-control--mailbox session path)))))

(defun niwa-agent-control-clear-context-mailbox (context)
  "Explicitly discard retained unread records for CONTEXT.
An empty mailbox persists nothing.  This does not acknowledge consumption or
discard delivered records awaiting their receiving transcript checkpoint."
  (let* ((session (if (niwa-session-p context)
                      context
                    (niwa-agent-invocation-parent-session context)))
         (path (cond
                ((niwa-session-p context) "/root")
                (session (niwa-agent-control--path-for-invocation
                          session context)))))
    (when (and path (niwa-agent-control--mailbox-queue session path))
      (niwa-agent-control--set-mailbox-queue session path nil)
      (niwa-agent-control--persist-session session))))

(defun niwa-agent-control-acknowledge-mail (context messages)
  "Consume only captured MESSAGES from CONTEXT's retained mailbox.
Call after inserting their delivery blocks into the receiving transcript.
The checkpoint owner retains the removed records in durable projections until
that receiving transcript commits.  No save or hook runs here.  Messages queued
since capture remain unread."
  (let* ((session (if (niwa-session-p context) context
                    (niwa-agent-invocation-parent-session context)))
         (path (niwa-agent-control-context-path context)))
    (when path
      (let* ((previous (niwa-agent-control--mailbox-queue session path))
             (ids (mapcar (lambda (message) (plist-get message :id)) messages))
             (removed (cl-remove-if-not
                       (lambda (message) (member (plist-get message :id) ids))
                       previous))
             (remaining (cl-remove-if (lambda (message) (memq message removed))
                                      previous)))
        (unless (equal previous remaining)
          ;; Snapshot before removal: even an error cannot lose live mail.
          (niwa-session-checkpoint-consume session path removed)
          (niwa-agent-control--set-mailbox-queue session path remaining))))))

(defun niwa-agent-control--waiter (session path)
  "Return PATH's active waiter in SESSION, or nil."
  (if (equal path "/root")
      (niwa-session-agent-root-waiter session)
    (when-let* ((record
                 (cdr (assoc path
                             (niwa-session-agent-registry session)))))
      (niwa-agent-record-waiter record))))

(defun niwa-agent-control--set-waiter (session path waiter)
  "Set PATH's active WAITER in SESSION."
  (if (equal path "/root")
      (niwa-session--set-agent-root-waiter session waiter)
    (setf (niwa-agent-record-waiter
           (niwa-agent-control--record-at-path session path))
          waiter)))

(defun niwa-agent-control-cancel-wait (session path &optional reason)
  "Cancel PATH's active wait in SESSION without invoking its callback.
REASON is the wake outcome, or `cancelled' when omitted."
  (when-let* ((waiter (niwa-agent-control--waiter session path)))
    (niwa-agent-control--set-waiter session path nil)
    (when-let* ((timer (niwa-agent-waiter-timer waiter))
                ((timerp timer)))
      (cancel-timer timer))
    (when-let* ((release (niwa-agent-waiter-release waiter)))
      (funcall release))
    (niwa-telemetry-record
     session 'agent-wait-ended
     :agent-path path
     :outcome (or reason 'cancelled)
     :duration-ms
     (and (numberp (niwa-agent-waiter-started-at waiter))
          (round (* 1000.0
                    (- (float-time)
                       (niwa-agent-waiter-started-at waiter))))))
    t))

(defun niwa-agent-control--wake (session path reason)
  "Wake PATH's waiter in SESSION with REASON exactly once."
  (when-let* ((waiter (niwa-agent-control--waiter session path)))
    (let ((callback (niwa-agent-waiter-callback waiter)))
      ;; A callback can throw after reconnect or synchronously start another
      ;; wait.  Retire this registration before entering arbitrary tool code;
      ;; unread mail remains owned by the durable mailbox, not this callback.
      (niwa-agent-control-cancel-wait session path reason)
      (funcall callback reason)
      t)))

(defun niwa-agent-control--wait-timeout (session path waiter)
  "Wake PATH for timeout if WAITER is still current in SESSION."
  (when (eq waiter (niwa-agent-control--waiter session path))
    (niwa-agent-control--wake session path 'timeout)))

(defun niwa-agent-control-wait (session callback &optional timeout-ms)
  "Suspend SESSION's current caller until mail, steering, or TIMEOUT-MS.
CALLBACK receives one of `mailbox', `steering', `user', or `timeout'.
Return the caller path when suspended, and nil after an immediate release."
  (let ((timeout (niwa--clamped-integer
                  timeout-ms
                  niwa-agent-control--wait-default-ms
                  niwa-agent-control--wait-min-ms
                  niwa-agent-control--wait-max-ms)))
    (let ((path (niwa-agent-control-current-path session)))
      (cond
       ((niwa-agent-control--mailbox session path)
        (niwa-telemetry-record
         session 'agent-wait-immediate
         :agent-path path :outcome 'mailbox)
        (funcall callback 'mailbox)
        nil)
       ((niwa-agent-control--waiter session path)
        (error "Agent is already waiting: %s" path))
       (t
        (let ((waiter (niwa-agent-waiter--create
                       :callback callback :started-at (float-time))))
          (niwa-agent-control--set-waiter session path waiter)
          (niwa-telemetry-record
           session 'agent-wait-started
           :agent-path path :timeout-ms timeout)
          (setf (niwa-agent-waiter-release waiter)
                (niwa-agent-control-block-turn session path 'waiting))
          (condition-case err
              (setf (niwa-agent-waiter-timer waiter)
                    (niwa-transport-run-at-time
                     (/ timeout 1000.0)
                     #'niwa-agent-control--wait-timeout
                     session path waiter))
            (error
             (niwa-agent-control-cancel-wait session path)
             (signal (car err) (cdr err))))
          path))))))

(defun niwa-agent-control-root-waiting-p (session)
  "Return non-nil when SESSION's root has an active WaitAgent callback."
  (and (niwa-agent-control--waiter session "/root") t))

(defun niwa-agent-control-wake-root-user (session)
  "Wake SESSION's root WaitAgent for pending user steering."
  (niwa-agent-control--wake session "/root" 'user))

(defun niwa-agent-control--notification-blocked-p (session)
  "Return non-nil while SESSION cannot safely run arbitrary waiter callbacks.
Only work in flight blocks: a running or finishing job or preparation, an active
synchronous publication, or a busy transport.  Queued preparations do not:
a callback's own mutation queues behind them, and sustained producers would
otherwise keep the queue non-empty and starve every wake."
  (or (niwa-session-publication-job session)
      (niwa-session-publication-finishing-p session)
      (niwa-session-publication-preparation-token session)
      (niwa-session-publication-active-p session)
      (niwa-transport-busy-p (niwa-session-save-path session))))

(defun niwa-agent-control--cancel-notifications (session)
  "Cancel SESSION's notification timer and discard its transient offers."
  (when-let* ((timer (niwa-session-mail-notification-timer session)))
    (cancel-timer timer))
  (setf (niwa-session-mail-notification-timer session) nil
        (niwa-session-mail-notifications session) nil
        (niwa-session-mail-notification-delay session) nil))

(defun niwa-agent-control--queue-notification (session key function)
  "Queue FUNCTION under KEY in SESSION's coalesced FIFO notification set.
Notifications never enter the mutation preparation queue.  One timer retries
the whole set, backing off from 50 ms to 500 ms while publication stays busy."
  (unless (assoc key (niwa-session-mail-notifications session))
    (setf (niwa-session-mail-notifications session)
          (nconc (niwa-session-mail-notifications session)
                 (list (cons key function)))))
  (unless (or (niwa-session-publication-stopping-p session)
              (niwa-session-mail-notification-timer session))
    (let ((delay (or (niwa-session-mail-notification-delay session) 0.05)))
      (setf (niwa-session-mail-notification-delay session) delay
            (niwa-session-mail-notification-timer session)
            (niwa-transport-run-at-time
             delay #'niwa-agent-control--flush-notifications session))))
  t)

(defun niwa-agent-control--flush-notifications (session)
  "Offer SESSION's pending notifications in order outside the mutation gate."
  (setf (niwa-session-mail-notification-timer session) nil)
  (if (not (buffer-live-p (niwa-session-root-buffer session)))
      (niwa-agent-control--cancel-notifications session)
    (while (and (niwa-session-mail-notifications session)
                (buffer-live-p (niwa-session-root-buffer session))
                (not (niwa-session-publication-stopping-p session))
                (not (niwa-agent-control--notification-blocked-p session)))
      (let ((entry (pop (niwa-session-mail-notifications session))))
        (condition-case err
            ;; Timers can run inside another session's advancing transaction.
            ;; Arbitrary waiter code must open a fresh transaction of its own.
            (niwa-session-durability-call-outside-transaction (cdr entry))
          (error
           (niwa-diagnostic-warning
            (niwa-diagnostic-scope session) 'niwa (format "Mailbox notification failed: %s"
                             (error-message-string err)) :warning)))))
    (cond
     ((not (buffer-live-p (niwa-session-root-buffer session)))
      (niwa-agent-control--cancel-notifications session))
     ;; A temporary close query may still be vetoed.  Keep the actual offers,
     ;; without a timer; reopening admissions re-arms them, teardown drops them.
     ((niwa-session-publication-stopping-p session) nil)
     ((niwa-session-mail-notifications session)
      (let ((entry (car (niwa-session-mail-notifications session))))
        (setf (niwa-session-mail-notification-delay session)
              (min 0.5 (* 2 (or (niwa-session-mail-notification-delay session) 0.05))))
        (niwa-agent-control--queue-notification session (car entry) (cdr entry))))
     (t (niwa-agent-control--cancel-notifications session)))))

(defun niwa-agent-control-notify (session key function)
  "Run FUNCTION outside SESSION's mutation gate, or queue it under KEY.
Pending notifications are coalesced by KEY and offered in FIFO order.  They
never become mutation preparations or inherit transaction observations.
Stopping sessions retain offers without a timer, for a vetoed close to reoffer
or teardown to discard.  A callback failure is diagnostic only."
  (if (or (niwa-session-publication-stopping-p session)
          ;; Queued work also defers an immediate call: a producer's
          ;; acknowledgement runs inside the completing job, and arbitrary
          ;; callbacks must not drain the queue from there.
          (niwa-session-publication-busy-p session)
          (niwa-agent-control--notification-blocked-p session)
          (niwa-session-mail-notifications session))
      (niwa-agent-control--queue-notification session key function)
    (condition-case err
        (niwa-session-durability-call-outside-transaction function)
      (error
       (niwa-diagnostic-warning
        (niwa-diagnostic-scope session) 'niwa (format "Mailbox notification failed: %s"
                         (error-message-string err)) :warning)))))

(defun niwa-agent-control--offer-at-gate-open (session)
  "Flush SESSION's pending notifications while its gate is open between jobs.
Runs from `niwa-session-publication-gate-open-functions', before the next
queued preparation starts.  A producer that keeps publication busy leaves no
other moment the backoff timer could reliably find, so without this turn its
own mail would never wake an idle model."
  (when (niwa-session-mail-notifications session)
    (when-let* ((timer (niwa-session-mail-notification-timer session)))
      (cancel-timer timer))
    (niwa-agent-control--flush-notifications session)))

(add-hook 'niwa-session-publication-gate-open-functions
          #'niwa-agent-control--offer-at-gate-open)

(defun niwa-agent-control-reoffer-mail (session)
  "Re-arm SESSION's held notifications after a vetoed close reopens admissions.
No callback runs inline and an idle session creates no timer.  Teardown drops
the retained offers if the close proceeds instead."
  (when (and (buffer-live-p (niwa-session-root-buffer session))
             (not (niwa-session-publication-stopping-p session)))
    (when-let* ((entry (car (niwa-session-mail-notifications session))))
      (niwa-agent-control--queue-notification session (car entry) (cdr entry)))))

(defun niwa-agent-control--deliver-mail
    (session recipient record &optional wake-reason)
  "Notify RECIPIENT of committed RECORD in SESSION.
MAIL and RESULT from another sender are marked to wake their recipient: an
idle root gets a background-event turn and an idle agent its next turn.
Mark before releasing a waiter: its callback can synchronously consume mail.
Recovery repairs neither mark records nor release waits."
  (unless niwa-agent-control-suppress-persistence
    (let ((notify
           (lambda ()
             (let ((waking
                    (and (memq (plist-get record :type) '(MAIL RESULT))
                         (not (equal (plist-get record :sender) recipient))
                         (memq record (niwa-agent-control--mailbox-queue session recipient)))))
               (when waking (plist-put record :wake t))
               (niwa-agent-control--wake session recipient (or wake-reason 'mailbox))
               (when (and waking
                          (memq record (niwa-agent-control--mailbox-queue session recipient)))
                 (cond
                  ((equal recipient "/root")
                   (niwa-pending-inputs-offer-root-event session))
                  ;; An active recipient reads it at its next boundary, or its
                  ;; settlement offers the wake.
                  ((not (niwa-agent-control--active-p
                         (niwa-agent-control--record-at-path session recipient)))
                   (niwa-agent-control--schedule-agent-wakes session))))))))
      (niwa-agent-control-notify
       session (list recipient (plist-get record :id) wake-reason) notify))))

(defun niwa-agent-control--identify-record (record)
  "Assign RECORD a durable message identity once, at its producer boundary."
  (unless (plist-get record :id)
    (plist-put record :id
               (md5 (format "%s:%s:%s" (emacs-pid) (current-time) (random)))))
  (unless (and (stringp (plist-get record :id))
               (not (string-empty-p (plist-get record :id))))
    (error "Invalid mailbox message identity"))
  record)

(defun niwa-agent-control--commit-mail-batch (session token &optional failure)
  "Commit SESSION's pending background arrivals together under gate TOKEN.
Until success every record is hidden and every producer retains its output.
Detach before I/O: arrivals during the commit belong to the next preparation.
FAILURE declines a queued batch locally without attempting a commit."
  (let ((batch (nreverse (niwa-session-mail-batch session)))
        added)
    (setf (niwa-session-mail-batch session) nil)
    (unless failure
      (condition-case err
          (progn
            (dolist (entry batch)
              (pcase-let ((`(,recipient ,records ,_wake ,_ack) entry))
                (let ((present (mapcar (lambda (record) (plist-get record :id))
                                      (niwa-agent-control--mailbox-queue session recipient))))
                  (dolist (record records)
                    (unless (or (member (plist-get record :id) present)
                                (niwa-session-checkpoint-pending-message-p
                                 session recipient (plist-get record :id)))
                      (plist-put record :uncommitted t)
                      (push (cons recipient record) added)
                      (push (plist-get record :id) present)
                      (niwa-agent-control--set-mailbox-queue
                       session recipient
                       (cons record (niwa-agent-control--mailbox-queue session recipient))))))))
            (niwa-agent-control-commit-session session token))
        ((error quit) (setq failure err))))
    (dolist (entry added)
      (if failure
          (niwa-agent-control--set-mailbox-queue
           session (car entry)
           (delq (cdr entry) (niwa-agent-control--mailbox-queue session (car entry))))
        (cl-remf (cdr entry) :uncommitted)))
    ;; One failed callback must not strand another producer's acknowledgement.
    ;; Notifications stay outside the preparation token through the normal FIFO.
    (dolist (entry batch)
      (pcase-let ((`(,recipient ,records ,wake ,ack) entry))
        (condition-case err
            (funcall ack (not failure) failure)
          ((error quit)
           (niwa-diagnostic-warning (niwa-diagnostic-scope session) 'niwa
                            (format "Mailbox acknowledgement failed: %s"
                                    (error-message-string err)) :warning)))
        (when (and (not failure) records)
          (niwa-agent-control--deliver-mail
           session recipient (car (last records)) wake))))))

(defun niwa-agent-control--batch-mail (session recipient records wake acknowledge)
  "Queue RECORDS for RECIPIENT in SESSION's next shared background commit.
WAKE and ACKNOWLEDGE retain each producer's delivery and acknowledgement."
  (dolist (record records) (niwa-agent-control--identify-record record))
  (let ((first (null (niwa-session-mail-batch session))))
    (push (list recipient records wake acknowledge) (niwa-session-mail-batch session))
    (when first
      (niwa-session-publication-enqueue
       session (apply-partially #'niwa-agent-control--commit-mail-batch session)
       (lambda (err) (niwa-agent-control--commit-mail-batch session nil err)))))
  'pending)

(defun niwa-agent-control--enqueue-records
    (session recipient records &optional wake-reason acknowledge)
  "Durably queue RECORDS (FIFO) for RECIPIENT with one commit, then wake.
All records are secured or none: a failed commit restores the queue.
ACKNOWLEDGE, when supplied, receives (SUCCESS ERROR) after authoritative
adoption or rollback.  Background arrivals may acknowledge asynchronously."
  (when (niwa-session-publication-stopping-p session)
    (user-error "Session publication is stopping"))
  ;; All producers enter here, including recovery RESULTs and batched monitor
  ;; records.  Assign once before committing; a retry of these same records
  ;; retains their identities.  Readers never manufacture missing identities.
  (cond
   ((and acknowledge
         (not niwa-agent-control-suppress-persistence)
         (niwa-session-save-path session)
         (buffer-live-p (niwa-session-root-buffer session))
         (not (niwa-session-publication-async-eligible-p session)))
    (niwa-agent-control--batch-mail session recipient records wake-reason acknowledge))
   ((and acknowledge
         (not niwa-agent-control-suppress-persistence)
         (niwa-session-publication-async-eligible-p session))
    (let (ids)
      (dolist (record records)
        (niwa-agent-control--identify-record record))
      (niwa-session-publication-submit
       session
       (lambda ()
         (dolist (record records)
           (niwa-agent-control--identify-record record))
         (let ((present (mapcar (lambda (message) (plist-get message :id))
                               (niwa-agent-control--mailbox-queue session recipient))))
           (setq records (cl-remove-if
                          (lambda (record)
                            (or (member (plist-get record :id) present)
                                (niwa-session-checkpoint-pending-message-p
                                 session recipient (plist-get record :id)))) records)))
         (dolist (record records) (plist-put record :uncommitted t))
         (setq ids (mapcar (lambda (record) (plist-get record :id)) records))
         (niwa-agent-control--set-mailbox-queue
          session recipient
          (append (reverse records)
                  (niwa-agent-control--mailbox-queue session recipient)))
         (list (niwa-session-artifacts--sidecar-artifact
                session (niwa-session-root-buffer session))))
       (lambda (success err)
         (if success
             (dolist (record records) (cl-remf record :uncommitted))
           (niwa-agent-control--set-mailbox-queue
            session recipient
            (cl-remove-if (lambda (message) (member (plist-get message :id) ids))
                          (niwa-agent-control--mailbox-queue session recipient))))
         (funcall acknowledge success err)
         (when (and success records)
           (niwa-agent-control--deliver-mail
            session recipient (car (last records)) wake-reason)))
       (mapcar (lambda (record) (plist-get record :id)) records))
      'pending))
   (t
    (niwa-session-publication-barrier session)
    (dolist (record records)
      (niwa-agent-control--identify-record record)
      (plist-put record :uncommitted t))
    (let ((previous
           (niwa-agent-control--mailbox-queue session recipient))
          (ids (mapcar (lambda (record) (plist-get record :id)) records))
          failure)
      (niwa-agent-control--set-mailbox-queue
       session recipient (append (reverse records) previous))
      (condition-case err
          (niwa-agent-control-commit-session session)
        (error
         (niwa-agent-control--set-mailbox-queue
          session recipient
          (cl-remove-if
           (lambda (message)
             (member (plist-get message :id) ids))
           (niwa-agent-control--mailbox-queue session recipient)))
         (setq failure err)))
      (if failure
          (if acknowledge (funcall acknowledge nil failure)
            (signal (car failure) (cdr failure)))
        (dolist (record records) (cl-remf record :uncommitted))
        (when acknowledge (funcall acknowledge t nil))
        (when records
          (niwa-agent-control--deliver-mail
           session recipient (car (last records)) wake-reason))
        records)))))

(defun niwa-agent-control--enqueue
    (session recipient record &optional wake-reason)
  "Durably queue RECORD for RECIPIENT, then wake a matching wait."
  (niwa-agent-control--enqueue-records
   session recipient (list record) wake-reason)
  record)

(defun niwa-agent-control-steer-user
    (session message &optional before-wake metadata)
  "Deliver MESSAGE when SESSION's root is explicitly waiting.
Call BEFORE-WAKE before releasing the wait.  Return non-nil only when the
input became steering for the current turn.  METADATA may carry the separate
transcript payload and hook audits."
  (when (niwa-agent-control--waiter session "/root")
    (when before-wake
      (funcall before-wake))
    (niwa-agent-control--enqueue
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

(cl-defun niwa-agent-control-send-message
    (session target message &key parent-tool-use-id working-directory)
  "Deliver MESSAGE to TARGET in SESSION and report how.
When TARGET is nil, use the caller's immediate parent; root must name a target.
An idle agent starts its turn with MESSAGE as the prompt when the tree has
capacity.  Otherwise MESSAGE is queued as canonical MAIL: an active agent
reads it at its next boundary, the root may wake for it, and an idle agent
starts once a slot frees.  PARENT-TOOL-USE-ID identifies the calling tool
row of a started turn.  WORKING-DIRECTORY moves the retained agent's later
turns to another existing directory, validated as for `Agent'; a running
turn keeps its directory.  Return (:path PATH :delivery DELIVERY), DELIVERY
being `started', `queued', or `waiting' for a slot."
  (unless (and (stringp message) (not (string-empty-p message)))
    (user-error "SendMessage message must be non-empty plain text"))
  (let* ((sender (niwa-agent-control-current-path session))
         (target (or target
                     (if (equal sender "/root")
                         (user-error "Root SendMessage requires a target")
                       (niwa-agent-record-parent-path
                        (niwa-agent-control--record-at-path session sender)))))
         (recipient
          (niwa-agent-control-resolve-path session sender target))
         (record (cdr (assoc recipient (niwa-session-agent-registry session))))
         (idle (and record (not (niwa-agent-control--active-p record))))
         (previous-directory
          (and record (niwa-agent-record-working-directory record))))
    (niwa-agent-control--assert-plan-steering session recipient record)
    (when working-directory
      (unless (and record (not (equal recipient "/root")))
        (user-error "SendMessage working_directory needs a retained agent target"))
      ;; Resolve before any delivery so a rejected directory changes nothing.
      (setf (niwa-agent-record-working-directory record)
            (niwa-agent-directory-resolve session working-directory)))
    (condition-case err
        (if (and idle
                 (niwa-agent-control--startable-p session record)
                 (niwa-agent-control--capacity-p session))
            (progn
              (niwa-agent-control--activate
               session record message parent-tool-use-id)
              (list :path recipient :delivery 'started))
          (niwa-agent-control--enqueue
           session recipient
           (append (list :type 'MAIL
                         :sender sender
                         :recipient recipient
                         :payload message
                         :timestamp (current-time))
                   ;; An agent turn this mail wakes keeps the sender's Plan limits.
                   (and record
                        (niwa-plan-read-only-p session)
                        (list :plan-read-only t))))
          (list :path recipient :delivery (if idle 'waiting 'queued)))
      ((error quit)
       ;; A failed delivery does not move the agent either.
       (when working-directory
         (setf (niwa-agent-record-working-directory record) previous-directory)
         (niwa-agent-control--persist-session session))
       (signal (car err) (cdr err))))))

(defun niwa-agent-control-enqueue-execution-result
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
  (unless (and (niwa-agent-path-p owner) (stringp body))
    (error "Invalid execution result"))
  (niwa-agent-control--enqueue-records
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

(defun niwa-agent-control-enqueue-monitor-events
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
    (niwa-agent-control--enqueue-records
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

(defun niwa-agent-control-enqueue-wake-up (session note meta acknowledge)
  "Queue a due wake-up's NOTE for SESSION's root as one waking WAKE_UP record.
META is a plist of structural facts carried as opening-tag attributes.
ACKNOWLEDGE receives (SUCCESS ERROR) once after adoption or rollback; the
record commits together with the caller's other pending session changes."
  (unless (stringp note)
    (error "Invalid wake-up note"))
  (niwa-agent-control--enqueue-records
   session "/root"
   (list (list :type 'WAKE_UP :sender "/root" :recipient "/root"
               :payload note :timestamp (current-time) :meta meta :wake t))
   nil acknowledge))

(defun niwa-agent-control-restore-lost-monitors (session monitors)
  "Add non-waking terminal mail for lost MONITORS to restored SESSION.
MONITORS contains repaired execution facts.  The restore checkpoint publishes
this mail together with its transcript repairs, not as a separate write."
  (let ((seen (make-hash-table :test #'equal)))
    (dolist (record (niwa-session-messages session))
      (when (eq (plist-get record :type) 'MONITOR_TERMINAL)
        (puthash (plist-get record :execution-id) t seen)))
    (dolist (facts monitors)
      (let ((id (plist-get facts :execution-id)))
        (when (and (stringp id) (not (gethash id seen)))
          (puthash id t seen)
          (let ((record (list :type 'MONITOR_TERMINAL :sender "/root"
                              :recipient "/root" :execution-id id
                              :description (or (plist-get facts :description) "Monitor")
                              :payload (concat "This monitor was lost when the session was restored. "
                                               "It is not running; start it again explicitly if needed.")
                              :timestamp (current-time)
                              :meta '(:termination lost :outcome failure))))
            (niwa-agent-control--identify-record record)
            (push record (niwa-session-messages session))))))))

(defun niwa-agent-control-wake-pending-p (session)
  "Return non-nil while SESSION's root mailbox holds an unread waking record.
Reading the mailbox consumes the record, so a result the model already saw
never wakes it."
  (and session
       (cl-some (lambda (record) (plist-get record :wake))
                (niwa-agent-control--mailbox session "/root"))
       t))

(defun niwa-agent-control-clear-wake (session owner execution-id)
  "Stop OWNER's unread records for EXECUTION-ID in SESSION from waking.
The model already received that result through another path."
  (when (and session (equal owner "/root") execution-id)
    (dolist (record (niwa-session-messages session))
      (when (and (plist-get record :wake)
                 (equal (plist-get record :execution-id) execution-id))
        (plist-put record :wake nil)))))

(defun niwa-agent-control-monitor-unread-bytes (session execution-id)
  "Return the payload bytes of EXECUTION-ID's unread Monitor records in SESSION."
  (let ((bytes 0))
    (dolist (record (and session (niwa-session-messages session)))
      (when (and (eq (plist-get record :type) 'MONITOR)
                 (equal (plist-get record :execution-id) execution-id))
        (cl-incf bytes (string-bytes (plist-get record :payload)))))
    bytes))

(defun niwa-agent-control-list-agents (session &optional path-prefix)
  "Return SESSION's minimal roster, optionally below PATH-PREFIX."
  (when (and path-prefix
             (not (niwa-agent-path-p path-prefix)))
    (user-error "Invalid agent path prefix: %s" path-prefix))
  (let* ((records
          (mapcar
           (lambda (entry)
             (let ((record (cdr entry)))
               (list :path (niwa-agent-record-path record)
                     :role (niwa-agent-record-role record)
                     :activity
                     (symbol-name
                      (niwa-agent-record-activity record)))))
           (niwa-session-agent-registry session)))
         (roster
          (cons (list :path "/root" :role "default"
                      :activity
                      (symbol-name
                       (niwa-session-agent-root-activity session)))
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

(defun niwa-agent-control-context-path (context)
  "Return CONTEXT's canonical path in its root session.

Return nil when CONTEXT is an agent invocation that is not yet published
in its parent session's retained registry.  A freshly spawned child's
first WAIT-state handlers run before its invocation is recorded, so an
unregistered child is a normal transient state, not an error."
  (if (niwa-session-p context)
      "/root"
    (let ((session (niwa-agent-invocation-parent-session context)))
      (and session
           (niwa-agent-control--path-for-invocation session context)))))

(defun niwa-agent-control-direct-children (session parent-path)
  "Return sorted path and role references below PARENT-PATH in SESSION."
  (sort
   (cl-loop for (_path . record) in (niwa-session-agent-registry session)
            when (equal parent-path
                        (niwa-agent-record-parent-path record))
            collect (list :path (niwa-agent-record-path record)
                          :role (niwa-agent-record-role record)))
   (lambda (a b)
     (string-lessp (plist-get a :path) (plist-get b :path)))))

(defun niwa-agent-control--recovery-result-text (record)
  "Return RECORD's complete interrupted recovery payload."
  (let* ((buffer (niwa-agent-record-conversation-buffer record))
         (identity
          (and (buffer-live-p buffer)
               (buffer-local-value 'niwa--agent-invocation buffer)))
         (partial
          (and identity
               (ignore-errors
                 (niwa-agent-conversation-final-response identity))))
         (location (niwa-agent-record-conversation-location record)))
    (concat
     "Agent turn was interrupted by session recovery."
      "\n\nReason: the persisted turn had no live provider "
      "request after resume."
      (when partial
        (format "\n\nPartial response:\n\n%s" partial))
      (when location
        (format "\n\nTranscript: %s" location)))))

(defun niwa-agent-control-recover-interrupted (session)
  "Recover every persisted active turn in SESSION as an interrupted RESULT.

Return the number of repaired records.  The caller owns the atomic sidecar
rewrite after the complete registry has been repaired."
  (let ((niwa-agent-control-suppress-persistence t)
        (count 0))
    (dolist (entry (niwa-session-agent-registry session))
      (let ((record (cdr entry)))
        (when (niwa-agent-control--active-p record)
          (let ((payload
                 (niwa-agent-control--recovery-result-text record)))
            (niwa-agent-control--record-settled-result
             record 'interrupted payload)
            (niwa-agent-control--capture-model-metadata record)
            (setf (niwa-agent-record-activity record) 'idle)
            (setf (niwa-agent-record-invocation record) nil)
            (setf (niwa-agent-record-blockers record) nil)
            (niwa-agent-control--enqueue
             session (niwa-agent-record-parent-path record)
             (list :type 'RESULT
                   :sender (niwa-agent-record-path record)
                   :recipient (niwa-agent-record-parent-path record)
                   :outcome 'interrupted
                   :payload
                   (niwa-agent-control--bounded-result record payload)
                   :timestamp (current-time)))
            (cl-incf count)))))
    count))

(defun niwa-agent-control--validate-spawn (session task-name message)
  "Validate SESSION, TASK-NAME, and MESSAGE for a new child."
  (unless (niwa-session-p session)
    (error "Agent requires an active niwa session"))
  (unless (stringp task-name)
    (user-error "Agent task_name must be plain text"))
  (unless (and (stringp message) (not (string-empty-p message)))
    (user-error "Agent message must be non-empty plain text")))

(defun niwa-agent-control--task-segment (task-name)
  "Derive one canonical child path segment from free-form TASK-NAME.
Lowercase the name, replace runs outside the ASCII path alphabet with a
hyphen, and trim boundary hyphens.  Use agent for an empty result and
agent-root for the reserved root name.  Cap the base at 40 characters,
preferring the last hyphen boundary.  Uniqueness belongs to reservation."
  (let* ((case-fold-search nil)
         (segment (string-trim
                   (replace-regexp-in-string
                    "[^a-z0-9_-]+" "-" (downcase task-name))
                   "-+" "-+")))
    (when (> (length segment) 40)
      (setq segment
            (string-trim-right
             (substring segment 0
                        (or (cl-position ?- segment :from-end t :end 41) 40))
             "-+")))
    (cond ((string-empty-p segment) "agent")
          ((equal segment "root") "agent-root")
          (t segment))))

(defun niwa-agent-control--normalize-context (value)
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

(defun niwa-agent-control--reserve
    (session parent-path task-name role)
  "Atomically reserve a unique TASK-NAME path below PARENT-PATH for ROLE.
SESSION's retained identities and private reservations both occupy paths."
  (niwa-session-publication-barrier session)
  (let* ((base (concat parent-path "/"
                       (niwa-agent-control--task-segment task-name)))
         (path base)
         (suffix 2)
         (registry (niwa-session-agent-registry session))
         (reservations (niwa-session-agent-reservations session)))
    (while (or (assoc path registry) (assoc path reservations))
      (setq path (format "%s-%d" base suffix)
            suffix (1+ suffix)))
    (when (>= (niwa-agent-control--active-count session)
              (niwa-session-agent-turn-capacity session))
      (user-error "Agent tree is at its active-turn capacity"))
    (let ((record
           (niwa-agent-record--create
            :path path
            :parent-path parent-path
            :role role
            :configuration (list :role role)
            :activity 'starting)))
      (niwa-session--set-agent-reservations
       session (cons (cons path record) reservations))
      record)))

(defun niwa-agent-control--rollback (session record)
  "Remove unpublished RECORD from SESSION and release its slot."
  (niwa-session--set-agent-reservations
   session
   (assoc-delete-all (niwa-agent-record-path record)
                     (niwa-session-agent-reservations session)))
  (when-let* ((invocation (niwa-agent-record-invocation record)))
    (niwa-agent-runtime-abandon-persistence invocation)
    (when-let* ((buffer (niwa-agent-invocation-buffer invocation))
                ((buffer-live-p buffer)))
      (with-current-buffer buffer
        (set-buffer-modified-p nil))
      (kill-buffer buffer)))
  (niwa-agent-control--persist-session session)
  ;; The freed slot may let an agent waiting for one start.
  (niwa-agent-control--schedule-agent-wakes session))

(defun niwa-agent-control--bounded-result (record response)
  "Return RECORD's bounded terminal payload for RESPONSE."
  (let ((text (if (stringp response) response (format "%S" response))))
    (if (<= (length text) niwa-agent-control--result-limit)
        text
      (let* ((location (niwa-agent-record-conversation-location record))
             (suffix (format "\n\nFull transcript: %s"
                             (or location "unavailable")))
             (preview-size
              (max 2 (- niwa-agent-control--result-limit
                        (length suffix) 128)))
             (preview
              (plist-get
               (niwa--head-tail-preview-parts
                text text (length text) preview-size)
               :text)))
        (concat preview suffix)))))

(defun niwa-agent-control-message-receipt (message)
  "Return hidden transcript metadata acknowledging MESSAGE's durable identity."
  (niwa--format-hook-audit-record
   (list :type 'mailbox-receipt :id (plist-get message :id))))

(defun niwa-agent-control--publish-result
    (session record result &optional staged)
  "Publish RECORD's canonical RESULT in SESSION.

Every turn first enqueues RESULT for the spawn parent.  A workflow may install
a one-shot result handler when it owns the parent interaction.  Successful
workflow delivery consumes that exact queued record; a failed handler leaves
it queued for ordinary parent delivery.  When STAGED is non-nil, the caller
already committed RESULT."
  (let* ((recipient (niwa-agent-record-parent-path record))
         (handler (niwa-agent-record-result-handler record)))
    (unless staged
      (niwa-agent-control--identify-record result)
      (plist-put result :uncommitted t)
      (let ((previous
             (niwa-agent-control--mailbox-queue session recipient)))
        (niwa-agent-control--set-mailbox-queue
         session recipient (cons result previous))
        (condition-case err
            (niwa-agent-control-commit-session session)
          (error
           (niwa-agent-control--set-mailbox-queue
            session recipient
            (delq result (copy-sequence
                          (niwa-agent-control--mailbox-queue session recipient))))
           (signal (car err) (cdr err)))))
      (cl-remf result :uncommitted))
    (if (not handler)
        (niwa-agent-control--deliver-mail session recipient result)
      (let ((buffer (if (equal recipient "/root")
                        (niwa-session-root-buffer session)
                      (niwa-agent-record-conversation-buffer
                       (niwa-agent-control--record-at-path session recipient)))))
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
                        (insert (niwa-agent-control-message-receipt result)))))
                (when (niwa-session-save-path session)
                  (error "Result recipient has no receiving transcript")))
              (niwa-session-checkpoint-consume session recipient (list result))
              (niwa-agent-control--set-mailbox-queue
               session recipient
               (cl-remove (plist-get result :id)
                          (niwa-agent-control--mailbox-queue session recipient)
                          :key (lambda (message) (plist-get message :id))
                          :test #'equal))
              (setf (niwa-agent-record-result-handler record) nil))
          (error
           (setf (niwa-agent-record-result-handler record) nil)
           (niwa-diagnostic-warning
            (niwa-diagnostic-scope session) 'niwa
            (format "Agent result handler failed for %s: %s"
                    (niwa-agent-record-path record)
                    (error-message-string err))
            :warning)
           (niwa-agent-control--deliver-mail session recipient result)))))))

(defun niwa-agent-control--settlement-mail-id (invocation key)
  "Return INVOCATION's outgoing message ID for logical completion KEY.
Keep IDs through failed settlement reconstruction, not only publisher retry.
The delivery closure releases this bounded producer state on success."
  (let* ((info (niwa-engine-info invocation))
         (ids (plist-get info :niwa-settlement-mail-ids)))
    (or (cdr (assq key ids))
        (let ((id (plist-get (niwa-agent-control--identify-record (list :id nil)) :id)))
          (setf (niwa-engine-info invocation)
                (plist-put info :niwa-settlement-mail-ids (cons (cons key id) ids)))
          id))))

(defun niwa-agent-control--settle
    (session record invocation response &optional event token)
  "Stage RECORD's terminal RESPONSE and EVENT in SESSION.
Return rollback and post-commit delivery closures for INVOCATION."
  (if (niwa-agent-conversation-fenced-p invocation)
      (when (eq invocation (niwa-agent-record-invocation record))
        ;; The successor owns durable state; retire only this local producer.
        (let ((niwa-agent-control-suppress-persistence t))
          (niwa-agent-control-cancel-wait session (niwa-agent-record-path record)))
        (setf (niwa-agent-record-activity record) 'idle
              (niwa-agent-record-invocation record) nil
              (niwa-agent-record-blockers record) nil)
        nil)
    (niwa-session-publication-barrier session token)
    (when (and (eq invocation (niwa-agent-record-invocation record))
               (niwa-agent-control--active-p record)
               (progn
                 ;; Target replacement invalidates grants and may publish a
                 ;; sidecar.  Do this before exposing tentative terminal state
                 ;; or capturing the rollback snapshot, never under its guard.
                 (niwa-session-artifacts-assert-mutation-authority
                  session (niwa-session-root-buffer session))
                 ;; The authority fence can yield to another terminal callback.
                 (if (niwa-agent-conversation-fenced-p invocation)
                     (progn
                       (niwa-agent-control--settle session record invocation response event token)
                       nil)
                   (and (eq invocation (niwa-agent-record-invocation record))
                        (niwa-agent-control--active-p record)))))
      (let* ((outcome
              (pcase (niwa-agent-invocation-transcript-status invocation)
                ('error 'errored)
                ('aborted 'interrupted)
                (_ (if (and (stringp response)
                            (string-prefix-p "Error:" response))
                       'errored
                     'completed))))
             (payload
              (if (eq outcome 'errored)
                  (or (and (listp event) (plist-get event :error-details))
                      (niwa-agent-invocation-terminal-reason invocation)
                      "Agent turn failed")
                response))
             (recipient (niwa-agent-record-parent-path record))
             (previous-queue
              (niwa-agent-control--mailbox-queue session recipient))
             (previous-activity (niwa-agent-record-activity record))
             (previous-invocation (niwa-agent-record-invocation record))
             (previous-blockers (niwa-agent-record-blockers record))
             (previous-result (niwa-agent-record-settled-result record))
             (previous-outcome (niwa-agent-record-settled-outcome record))
             (previous-metadata (niwa-agent-record-model-metadata record))
             ;; Completions of the agent's own Bash it never read go to the
             ;; caller with the result.
             (own-path (niwa-agent-record-path record))
             (own-queue (niwa-agent-record-mailbox record))
             (unread-executions          ; newest first, like the queue
              (cl-remove-if-not
               (lambda (item)
                 (and (eq (plist-get item :type) 'EXECUTION)
                      (equal (plist-get item :sender) own-path)))
               own-queue))
             ;; Normalized input plus output, excluding cached input, while the
             ;; child engine context is still attached.  Callers may charge it elsewhere.
             (usage
              (when-let* ((tokens (plist-get (niwa-engine-info invocation) :tokens-full)))
                (+ (or (plist-get tokens :input) 0)
                   (or (plist-get tokens :output) 0))))
             result staged-records turn-closed)
        (cl-labels
            ((rollback ()
               (setf (niwa-agent-record-activity record) previous-activity
                     (niwa-agent-record-invocation record)
                     previous-invocation
                     (niwa-agent-record-blockers record) previous-blockers
                     (niwa-agent-record-settled-result record)
                     previous-result
                     (niwa-agent-record-settled-outcome record)
                     previous-outcome
                     (niwa-agent-record-model-metadata record)
                     previous-metadata)
               (niwa-agent-control--set-mailbox-queue
                session recipient
                (cl-remove-if (lambda (message) (memq message staged-records))
                              (niwa-agent-control--mailbox-queue session recipient)))
               (setf (niwa-agent-record-mailbox record)
                     (append (cl-set-difference
                              (niwa-agent-record-mailbox record) own-queue :test #'eq)
                             own-queue)))
             (deliver ()
               (dolist (message staged-records) (cl-remf message :uncommitted))
               ;; Close the finished turn once.  A delivery retry can run after
               ;; the agent's next turn started; that turn's wait and wake
               ;; marks are its own.
               (unless turn-closed
                 (niwa-agent-control-cancel-wait session own-path)
                 ;; A failed turn does not restart itself for mail it missed.
                 (when (eq outcome 'errored)
                   (niwa-agent-control-clear-agent-wakes session own-path))
                 (setq turn-closed t))
               (niwa-agent-control--publish-result
                session record result t)
               (setf (niwa-engine-info invocation)
                     (plist-put (niwa-engine-info invocation)
                                :niwa-settlement-mail-ids nil))))
          (setq payload
                (niwa-agent-control--record-settled-result
                 record outcome payload))
          (niwa-agent-control--capture-model-metadata record)
          (setf (niwa-agent-record-activity record) 'idle
                (niwa-agent-record-invocation record) nil
                (niwa-agent-record-blockers record) nil)
          (setq result
                (niwa-agent-control--identify-record
                 (list :id (niwa-agent-control--settlement-mail-id invocation 'result)
                       :type 'RESULT
                       :sender (niwa-agent-record-path record)
                       :recipient recipient
                       :outcome outcome
                       :payload
                       (niwa-agent-control--bounded-result record payload)
                       :timestamp (current-time))))
          (when usage
            (setq result (append result (list :usage usage))))
          (setq staged-records
                (append
                 (mapcar
                  (lambda (entry)
                    (niwa-agent-control--identify-record
                     (append
                      (list :id (niwa-agent-control--settlement-mail-id invocation entry)
                            :type 'EXECUTION
                            :sender (niwa-agent-record-path record)
                            :recipient recipient :payload (car entry)
                            :timestamp (current-time))
                      (and (cdr entry) (list :meta (cdr entry))))))
                  ;; Completions after the answer are newer.  Runtime entries have
                  ;; no identity yet; queued completions retain theirs.
                  (niwa-agent-invocation-runtime-execution-results invocation))
                 (mapcar (lambda (item)
                           (plist-put (copy-sequence item) :recipient recipient))
                         unread-executions)
                 (list result)))
          (dolist (message staged-records) (plist-put message :uncommitted t))
          (niwa-agent-control--set-mailbox-queue
           session recipient (append staged-records previous-queue))
          (when unread-executions
            (setf (niwa-agent-record-mailbox record)
                  (cl-remove-if (lambda (item) (memq item unread-executions))
                                own-queue)))
          (cons #'rollback #'deliver))))))

(defun niwa-agent-control--record-invocation
    (session record invocation)
  "Publish RECORD with INVOCATION in SESSION's retained registry."
  (niwa-session-publication-barrier session)
  (unless (equal (niwa-agent-invocation-path invocation)
                 (niwa-agent-record-path record))
    (error "Agent invocation path mismatch: %s"
           (niwa-agent-invocation-path invocation)))
  (let* ((path (niwa-agent-record-path record))
         (reservation
          (assoc path (niwa-session-agent-reservations session)))
         (published (assoc path (niwa-session-agent-registry session)))
         (previous-reservations
          (niwa-session-agent-reservations session))
         (previous-registry (niwa-session-agent-registry session))
         (previous
          (list (niwa-agent-record-id record)
                (niwa-agent-record-configuration record)
                (niwa-agent-record-activity record)
                (niwa-agent-record-conversation-location record)
                (niwa-agent-record-conversation-buffer record)
                (niwa-agent-record-invocation record)
                (niwa-agent-record-blockers record)
                (niwa-agent-record-settled-result record)
                (niwa-agent-record-settled-outcome record)
                (niwa-agent-record-model-metadata record))))
    (unless (or reservation (eq (cdr published) record))
      (error "Agent record is not reserved or published: %s" path))
    (setf (niwa-agent-record-id record)
          (niwa-agent-invocation-agent-id invocation)
          (niwa-agent-record-configuration record)
          (niwa-agent-invocation-frozen-configuration invocation)
          (niwa-agent-record-activity record) 'running
          (niwa-agent-record-conversation-location record)
          (niwa-agent-invocation-transcript-relative-path invocation)
          (niwa-agent-record-conversation-buffer record)
          (niwa-agent-invocation-buffer invocation)
          (niwa-agent-record-invocation record) invocation
          (niwa-agent-record-blockers record) nil
          (niwa-agent-record-settled-result record) nil
          (niwa-agent-record-settled-outcome record) nil)
    (niwa-agent-control--capture-model-metadata record)
    (when reservation
      (niwa-session--set-agent-reservations
       session (assoc-delete-all path previous-reservations))
      (niwa-session--set-agent-registry
       session (cons (cons path record) previous-registry)))
    (condition-case err
        (niwa-agent-control-commit-session session)
      (error
       (niwa-session--set-agent-reservations
        session previous-reservations)
       (niwa-session--set-agent-registry session previous-registry)
       (setf (niwa-agent-record-id record) (nth 0 previous)
             (niwa-agent-record-configuration record) (nth 1 previous)
             (niwa-agent-record-activity record) (nth 2 previous)
             (niwa-agent-record-conversation-location record)
             (nth 3 previous)
             (niwa-agent-record-conversation-buffer record)
             (nth 4 previous)
             (niwa-agent-record-invocation record) (nth 5 previous)
             (niwa-agent-record-blockers record) (nth 6 previous)
             (niwa-agent-record-settled-result record) (nth 7 previous)
             (niwa-agent-record-settled-outcome record) (nth 8 previous)
             (niwa-agent-record-model-metadata record) (nth 9 previous))
       (signal (car err) (cdr err))))))

(defun niwa-agent-control--set-hook-context (record entries)
  "Replace RECORD's pending hook context with a copy of ENTRIES."
  (setf (niwa-agent-record-hook-context-pending record)
        (copy-tree entries)))

(defun niwa-agent-control--dispatch-followup
    (session record message &optional parent-tool-use-id plan-read-only)
  "Dispatch MESSAGE as RECORD's next provider turn in SESSION.
PARENT-TOOL-USE-ID and PLAN-READ-ONLY are as in
`niwa-agent-control--activate'."
  (when-let* ((directory (niwa-agent-record-working-directory record))
              ((not (file-directory-p directory))))
    (user-error "Agent %s's working directory no longer exists: %s; \
pass SendMessage working_directory to move it"
                (niwa-agent-record-path record) directory))
  (let ((buffer (niwa-agent-persistence-ensure-conversation
                 session record (niwa-session-root-buffer session))))
    (let ((configuration (niwa-agent-record-configuration record)))
      (unless (niwa-agent-configuration-p configuration)
        (error "Agent has no frozen configuration: %s"
               (niwa-agent-record-path record)))
      (unless
          (niwa-agent-runtime-dispatch
           nil (file-name-nondirectory (niwa-agent-record-path record))
           message
           :path (niwa-agent-record-path record)
           :working-directory (niwa-agent-record-working-directory record)
           :frozen-configuration configuration
           :retained-id (niwa-agent-record-id record)
           :retained-buffer buffer
           :retained-transcript
           (niwa-agent-record-conversation-location record)
           :pending-hook-context
           (niwa-agent-record-hook-context-pending record)
           :parent-tool-use-id parent-tool-use-id
           :plan-read-only plan-read-only
           :on-hook-context
           (apply-partially
            #'niwa-agent-control--set-hook-context record)
           :on-invocation
           (apply-partially
            #'niwa-agent-control--record-invocation session record)
           :on-settle
           (apply-partially
            #'niwa-agent-control--settle session record))
        (error "Agent provider request did not start")))
    record))

(defun niwa-agent-control--capacity-p (session)
  "Return non-nil when SESSION's tree can admit one more active turn."
  (< (niwa-agent-control--active-count session)
     (niwa-session-agent-turn-capacity session)))

(defun niwa-agent-control--startable-p (session record)
  "Return non-nil when idle RECORD in SESSION may start its next turn.
Settlement marks a record idle before its RESULT commits and before the
runtime has released the finished invocation.  Meanwhile a rollback may
restore that turn and its publication rejects nested commits, so no turn
starts while a settlement in SESSION runs.  Another session's settlement
re-offers only its own session, so it does not hold this one.
Waiting mail gets the offer `niwa-agent-control-settlement-finished'
makes."
  (and (eq (niwa-agent-record-activity record) 'idle)
       (not (memq session (bound-and-true-p niwa-claude-code-fork--sessions)))
       (not (cl-some (lambda (invocation)
                       (eq session (niwa-agent-invocation-parent-session
                                    invocation)))
                     niwa-agent-control-settling))))

(defun niwa-agent-control-settlement-finished (session)
  "Offer SESSION's idle agents turns now that a settlement call has ended.
The runtime calls this whether the settlement committed, rolled back, or
failed.  Mail that arrived during a final answer or while waiting for a
slot gets its offer."
  (niwa-agent-control--schedule-agent-wakes session))

(defun niwa-agent-control--activate
    (session record message &optional parent-tool-use-id plan-read-only)
  "Start idle RECORD's next turn in SESSION with MESSAGE as its prompt.
PARENT-TOOL-USE-ID identifies the calling tool row.  PLAN-READ-ONLY, or
unread mail from a Plan read-only sender, stamps the turn read-only even
without an ambient Plan caller: the turn reads that mail."
  (niwa-session-publication-barrier session)
  (let ((previous-result (niwa-agent-record-settled-result record))
        (previous-outcome (niwa-agent-record-settled-outcome record))
        (plan-read-only
         (or plan-read-only
             (cl-some (lambda (message) (plist-get message :plan-read-only))
                      (niwa-agent-record-mailbox record)))))
    ;; Provider startup can yield to another owner's mailbox save.
    ;; Keep the published registry valid throughout that interval.
    (setf (niwa-agent-record-activity record) 'starting
          (niwa-agent-record-settled-result record) nil
          (niwa-agent-record-settled-outcome record) nil)
    (condition-case err
        (niwa-agent-control--dispatch-followup
         session record message parent-tool-use-id plan-read-only)
      ((error quit)
       (setf (niwa-agent-record-activity record) 'idle
             (niwa-agent-record-invocation record) nil
             (niwa-agent-record-settled-result record) previous-result
             (niwa-agent-record-settled-outcome record) previous-outcome)
       (niwa-agent-control--persist-session session)
       ;; Another agent may have queued for the slot this start held.
       (niwa-agent-control--schedule-agent-wakes session)
       (signal (car err) (cdr err))))))


;;;; Waking idle agents

(defconst niwa-agent-control--wake-prompt
  (concat "Messages arrived while you were idle; they follow. "
          "They come from other agents, not from the user.")
  "Fixed prompt of an agent turn started for queued mail.
The mail itself reaches the model as mailbox records at the turn's first
boundary, never as this prompt, so its text cannot pose as the user.")

(defun niwa-agent-control--waking-records (record)
  "Return RECORD's committed unread records that should wake it."
  (cl-remove-if-not
   (lambda (message)
     (and (plist-get message :wake)
          (not (plist-get message :uncommitted))))
   (niwa-agent-record-mailbox record)))

(defun niwa-agent-control-clear-agent-wakes (session path)
  "Stop PATH's unread mail in SESSION from starting another turn.
The mail stays queued for the agent's next turn.  Mail sent later wakes it
again, so stopping an agent does not make it deaf."
  (when-let* ((record (cdr (assoc path (niwa-session-agent-registry
                                        session)))))
    (dolist (message (niwa-agent-record-mailbox record))
      (when (plist-get message :wake)
        (plist-put message :wake nil)))))

(defun niwa-agent-control--schedule-agent-wakes (session)
  "Offer SESSION's idle agents turns for their waking mail.
The offer runs from a timer once the transport is idle, after the current
callback (often a settlement) has unwound.  Repeated offers coalesce."
  (when-let* ((buffer (niwa-session-root-buffer session))
              ((buffer-live-p buffer)))
    (niwa-transport-run-when-idle
     (list 'agent-wake session)
     (buffer-local-value 'default-directory buffer)
     (lambda () (niwa-agent-control--offer-agent-wakes session))
     nil 0)))

(defun niwa-agent-control--offer-agent-wakes (session)
  "Start idle agents' turns in SESSION for their waking mail.
Agents whose oldest waking mail is oldest go first, while the tree has
capacity; the rest wait for a later settlement to free a slot.  A turn that
cannot start leaves its mail unread for a later turn without retrying."
  (when-let* ((buffer (niwa-session-root-buffer session))
              ((buffer-live-p buffer))
              ((eq session (buffer-local-value 'niwa--session buffer))))
    (with-current-buffer buffer
      (let (candidates)
        (dolist (entry (niwa-session-agent-registry session))
          (let ((record (cdr entry)))
            (when-let* (((eq (niwa-agent-record-activity record) 'idle))
                        (waking (niwa-agent-control--waking-records record)))
              ;; The queue is newest first, so its last waking record is oldest.
              (push (list (plist-get (car (last waking)) :timestamp) record)
                    candidates))))
        (dolist (candidate (sort candidates
                                 (lambda (a b) (time-less-p (car a) (car b)))))
          (let ((record (cadr candidate)))
            ;; An earlier start can yield: re-check this agent's state now.
            (when (and (niwa-agent-control--waking-records record)
                       (niwa-agent-control--startable-p session record)
                       (niwa-agent-control--capacity-p session))
              (condition-case err
                  (niwa-agent-control--activate
                   session record niwa-agent-control--wake-prompt)
                ((error quit)
                 ;; Neither a failure nor a quit restarts the agent by itself.
                 (niwa-agent-control-clear-agent-wakes
                  session (niwa-agent-record-path record))
                 (niwa-diagnostic-message (niwa-diagnostic-scope session)
                          "Could not start %s for its mail: %s"
                          (niwa-agent-record-path record)
                          (if (eq (car err) 'quit)
                              "quit"
                            (error-message-string err))))))))))))

(defun niwa-agent-control-interrupt (session target)
  "Interrupt TARGET's current turn in SESSION and return its prior activity."
  (let* ((caller-path (niwa-agent-control-current-path session))
         (path (niwa-agent-control-resolve-path
                session caller-path target)))
    (when (equal path "/root")
      (user-error "InterruptAgent cannot target /root"))
    (when (equal path caller-path)
      (user-error "InterruptAgent cannot target the caller"))
    (let* ((record (niwa-agent-control--record-at-path session path))
           (activity (niwa-agent-record-activity record)))
      ;; An idle agent waiting for a slot stops waiting too.
      (niwa-agent-control-clear-agent-wakes session path)
      (when (niwa-agent-control--active-p record)
        (niwa-agent-runtime-interrupt
         (niwa-agent-record-invocation record)
         (format "interrupted by %s" caller-path)))
      (list :path path :previous-activity activity))))

(defun niwa-agent-control-interrupt-user (session path)
  "Interrupt PATH's current turn in SESSION on the user's behalf.
Return non-nil when an active turn was interrupted; an idle agent is left
alone.  The agent stays retained, as after `InterruptAgent'."
  (when (equal path "/root")
    (user-error "Use the session's own cancel command for /root"))
  (let ((record (or (cdr (assoc path (niwa-session-agent-registry session)))
                    (user-error "Agent %s is no longer available" path))))
    (niwa-agent-control-clear-agent-wakes session path)
    (when (niwa-agent-control--active-p record)
      (niwa-agent-runtime-interrupt
       (niwa-agent-record-invocation record) "interrupted by user")
      t)))

(defun niwa-agent-control--summary-metadata (result)
  "Return disclosable provider metadata from context-summary RESULT."
  (list :backend
        (when-let* ((backend (plist-get result :backend)))
          (or (ignore-errors (gptel-backend-name backend))
              (format "%s" backend)))
        :model (plist-get result :model)
        :effort (plist-get result :effort)))

(cl-defun niwa-agent-control-spawn
    (session task-name message callback
             &key role agent context model effort model-policy working-directory
             description on-invocation result-handler
             skill-permission-rules skill-hook-rules parent-tool-use-id)
  "Prepare and spawn TASK-NAME with MESSAGE asynchronously in SESSION.
CALLBACK receives a success, error, or aborted outcome plist.  Return an
idempotent cancellation thunk for the unpublished preparation transaction.
WORKING-DIRECTORY selects an existing execution directory; omission inherits
the caller's optional override, without changing shared resource authority."
  (niwa-agent-control--validate-spawn session task-name message)
  (unless (functionp callback)
    (error "Agent spawn callback must be a function"))
  (when (and role (null agent)
             (niwa-agents-specs)
             (not (assoc-string role (niwa-agents-specs))))
    (user-error "Agent role is not available to this session: %s" role))
  (let* ((resolved-agent
          (or agent (niwa-agent-resolve-role role)))
         (resolved-agent
          (if skill-hook-rules
              (let ((copy (copy-niwa-agent resolved-agent)))
                (setf (niwa-agent-hook-rules copy)
                      (append (niwa-agent-hook-rules copy)
                              skill-hook-rules))
                copy)
            resolved-agent))
         (role-name (niwa-agent-name resolved-agent))
         (context (niwa-agent-control--normalize-context context))
         (model-selector
          (niwa-model-parse-selector model))
         (effort (niwa-model-parse-effort effort))
         (model-policy
          (or model-policy
              (niwa-model-resolve-workload
               (intern role-name) model-selector effort)))
         (directory
          (if working-directory
              (niwa-agent-directory-resolve session working-directory)
            (and (eq session niwa--session)
                 niwa--conversation-working-directory)))
         (caller-path (niwa-agent-control-current-path session))
         (scope (niwa-diagnostic-scope session))
         (record (niwa-agent-control--reserve
                  session caller-path task-name role-name))
         context-snapshot
         summary-evidence
         summary-metadata
         summary-cancel
         committed
         cancelled
         settled)
    (setf (niwa-agent-record-result-handler record) result-handler
          (niwa-agent-record-working-directory record) directory)
    (cl-labels
        ((settle (outcome)
           (unless settled
             (setq settled t)
             (funcall callback outcome)))
         (fail (message)
           (unless settled
             (unless committed
               (niwa-agent-control--rollback session record))
             (settle (list :outcome 'error :error message))))
         (start-provider (prepared)
           (unless (or settled cancelled)
             (if (not (eq (plist-get prepared :outcome) 'success))
                 (fail (or (plist-get prepared :error)
                           "Agent task preparation failed"))
               (condition-case err
                   (progn
                     (unless
                         (niwa-agent-runtime-dispatch
                          resolved-agent (or description task-name) message
                          :context-snapshot context-snapshot
                          :model-policy model-policy
                          :skill-permission-rules skill-permission-rules
                          :prepared-turn (plist-get prepared :turn)
                          :start-hook-audits
                          (plist-get prepared :start-hook-audits)
                          :parent-tool-use-id parent-tool-use-id
                          :path (niwa-agent-record-path record)
                          :working-directory directory
                          :on-invocation
                          (lambda (invocation)
                            (niwa-agent-control--record-invocation
                             session record invocation)
                            (setq committed t)
                            (when on-invocation
                              (condition-case err
                                  (funcall on-invocation invocation)
                                (error
                                 (niwa--warn-once
                                  scope 'agent-invocation-observer
                                  "Agent invocation observer failed: %s"
                                  (error-message-string err))))))
                          :on-settle
                          (apply-partially
                           #'niwa-agent-control--settle session record))
                       (error "Agent provider request did not start"))
                     (unless (niwa-agent-record-conversation-location
                              record)
                       (error "Agent conversation location was not recorded"))
                     (settle (list :outcome 'success
                                   :record record
                                   ;; Launch snapshot: resolved before any
                                   ;; asynchronous provider acknowledgement.
                                   :model-metadata
                                   (niwa-agent-control-record-model-metadata
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
                  (niwa-context-summary-generate
                   summary-evidence 'handoff
                   (lambda (result)
                     (unless (or settled cancelled)
                       (pcase (plist-get result :outcome)
                         ('success
                          (setq context-snapshot
                                (niwa-agent-runtime-task-background
                                 (plist-get result :summary))
                                summary-metadata
                                (niwa-agent-control--summary-metadata
                                 result))
                          (start-provider prepared))
                         ('aborted
                          (niwa-agent-control--rollback session record)
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
                      (niwa-compact-evidence-summary-context-evidence
                       parent-tool-use-id))
              (setq context-snapshot
                    (niwa-compact-evidence-context-snapshot context)))
            (niwa-agent-runtime-prepare-task
             resolved-agent (or description task-name) message
             (niwa-agent-record-path record) #'prepared
             :skill-permission-rules skill-permission-rules
             :cancelled-p (lambda () cancelled)))
        (error (fail (error-message-string err))))
      (lambda ()
        (unless settled
          (setq cancelled t)
          (when summary-cancel
            (funcall summary-cancel))
          (niwa-agent-control--rollback session record)
          (settle '(:outcome aborted)))))))

(provide 'niwa-agent-control)

;;; niwa-agent-control.el ends here
