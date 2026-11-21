;;; niwa-turn.el -- Canonical turn settlement -*- lexical-binding: t -*-

;;; Commentary:

;; Owns request admission, identity, cancellation, and the terminal transaction
;; shared by normal gptel turns and direct skill forks.  Every settlement step
;; is isolated so one failure cannot skip later cleanup, persistence, hook
;; delivery, or queued-message drainage.

;;; Code:

(require 'niwa-telemetry)
(require 'niwa-utilities)

(require 'niwa-recovery)

(require 'cl-lib)
(require 'niwa-engine)
(require 'niwa-models)
(require 'niwa-compact-estimation)
(require 'niwa-structs)
(require 'niwa-transport)

(eval-when-compile
  (require 'gptel-request))

;; `gptel'
(declare-function gptel-backend-name "ext:gptel" (cl-x) t)
(defvar gptel-backend)

;; `gptel-request'

;; `niwa-agent-control'
(declare-function niwa-agent-control-active-turn-p "niwa-agent-control" (session))
(autoload 'niwa-agent-control-active-turn-p "niwa-agent-control")
(declare-function niwa-agent-control-context-mailbox "niwa-agent-control" (context))

;; `niwa-agents'
(declare-function niwa-agent-invocation-p "niwa-agents" (cl-x))
(declare-function niwa-agent-invocation-permission-mode-override
                  "niwa-agents" (cl-x) t)
(declare-function niwa-agent-invocation-plan-read-only
                  "niwa-agents" (cl-x) t)
(declare-function niwa-agent-invocation-require-path
                  "niwa-agents" (invocation))
(declare-function niwa-agent-invocation-runtime-pending-response
                  "niwa-agents" (cl-x) t)
(defvar niwa--agent-invocation)

;; `niwa-artifact-backend'
(declare-function niwa-artifact-backend-save-version
                  "niwa-artifact-backend" (workspace id &optional session-id))
(autoload 'niwa-artifact-backend-save-version "niwa-artifact-backend")

;; `niwa-collaboration'
(declare-function niwa-collaboration-notify-request-changed
                  "niwa-collaboration" (data-buffer &optional completed-trigger))

;; `niwa-compact-estimation'
(declare-function niwa-compact-estimation-record-token-baseline
                  "niwa-compact-estimation" (fsm))

;; `niwa-execution'
(declare-function niwa-execution-session-live-p "niwa-execution" (session))

;; `niwa-execution-target'
(declare-function niwa-execution-target-probe
                  "niwa-execution-target"
                  (target &optional refresh sandbox-mode))
(declare-function niwa-execution-target-readiness-message
                  "niwa-execution-target" (target))
(declare-function niwa-execution-target-ready-p
                  "niwa-execution-target" (target))
(declare-function niwa-execution-target-refresh-incarnation
                  "niwa-execution-target" (target))
(declare-function niwa-execution-target-remote-p
                  "niwa-execution-target" (target))
(autoload 'niwa-execution-target-remote-p "niwa-execution-target")

;; `niwa-goal'
(declare-function niwa-goal-dispatch-after-turn "niwa-goal"
                  (fsm &optional succeeded))
(declare-function niwa-goal-persist-failure "niwa-goal" (fsm))
(declare-function niwa-goal-settle-failure
                  "niwa-goal" (fsm &optional status))
(declare-function niwa-goal-settle-turn "niwa-goal" (fsm))

;; `niwa-gptel-retry'
(declare-function niwa-gptel-retry-record-failure "niwa-gptel-retry" (fsm))
(declare-function niwa-gptel-retry-refresh-availability "niwa-gptel-retry" (buffer))
(autoload 'niwa-gptel-retry-record-failure "niwa-gptel-retry")
(autoload 'niwa-gptel-retry-refresh-availability "niwa-gptel-retry")

;; `niwa-hooks'
(declare-function niwa-hooks-event-plist
                  "niwa-hooks"
                  (event &optional session workspace &rest extra))
(declare-function niwa-hooks-run-event
                  "niwa-hooks"
                  (event event-plist callback
                         &optional session workspace request invocation))
(autoload 'niwa-hooks-event-plist "niwa-hooks")
(autoload 'niwa-hooks-run-event "niwa-hooks")

;; `niwa-implementation-policy'
(declare-function niwa-implementation-policy-assert-preparation
                  "niwa-implementation-policy" (preparation))
(declare-function niwa-implementation-policy-current
                  "niwa-implementation-policy" (&optional session buffer))
(declare-function niwa-implementation-policy-validate
                  "niwa-implementation-policy" (policy))
(defvar niwa-implementation-policy--admission-preparation nil)
(autoload 'niwa-implementation-policy-assert-preparation "niwa-implementation-policy")
(autoload 'niwa-implementation-policy-current "niwa-implementation-policy")
(autoload 'niwa-implementation-policy-validate "niwa-implementation-policy")

;; `niwa-journal-capture'
(declare-function niwa-journal-capture-checkpoint "niwa-journal-capture" (session buffer))
(declare-function niwa-journal-capture-checkpoint-start "niwa-journal-capture" (session buffer callback))
(defvar niwa-journal-enabled)
(autoload 'niwa-journal-capture-checkpoint "niwa-journal-capture")
(autoload 'niwa-journal-capture-checkpoint-start "niwa-journal-capture")

;; `niwa-journal-cleanup'
(declare-function niwa-journal-cleanup-schedule "niwa-journal-cleanup" (workspace &optional force))
(autoload 'niwa-journal-cleanup-schedule "niwa-journal-cleanup")

;; `niwa-journal-idle'
(declare-function niwa-journal-idle-arm-seal "niwa-journal-idle" (session buffer))
(autoload 'niwa-journal-idle-arm-seal "niwa-journal-idle")

;; `niwa-journal-process'
(declare-function niwa-journal-process-schedule "niwa-journal-process" (workspace &optional recover))
(autoload 'niwa-journal-process-schedule "niwa-journal-process")

;; `niwa-memory-decision'
(declare-function niwa-memory-decision-schedule-recovery "niwa-memory-decision" (workspace &optional callback))
(autoload 'niwa-memory-decision-schedule-recovery "niwa-memory-decision")

;; `niwa-memory-pass'
(declare-function niwa-memory-pass-schedule "niwa-memory-pass" (workspace))
(autoload 'niwa-memory-pass-schedule "niwa-memory-pass")

;; `niwa-pending-inputs'
(declare-function niwa-pending-inputs-cockpit-holds-p
                  "niwa-pending-inputs" (session))
(declare-function niwa-pending-inputs-offer-event-after-turn
                  "niwa-pending-inputs" (fsm))
(declare-function niwa-pending-inputs-requeue-steering
                  "niwa-pending-inputs" (session request-id))
(declare-function niwa-pending-inputs-schedule-held-input-delivery
                  "niwa-pending-inputs" (&optional data-buffer))
(declare-function niwa-view--schedule-follow-up-drain
                  "niwa-pending-inputs" (fsm))
(autoload 'niwa-pending-inputs-cockpit-holds-p "niwa-pending-inputs")
(autoload 'niwa-pending-inputs-offer-event-after-turn "niwa-pending-inputs")
(autoload 'niwa-pending-inputs-requeue-steering "niwa-pending-inputs")
(autoload 'niwa-pending-inputs-schedule-held-input-delivery "niwa-pending-inputs")
(autoload 'niwa-view--schedule-follow-up-drain "niwa-pending-inputs")

;; `niwa-permission-mode'
(declare-function niwa-permission-mode-effective
                  "niwa-permission-mode"
                  (&optional session data-buffer surface-buffer request invocation))
(declare-function niwa-permission-mode-normalize
                  "niwa-permission-mode" (mode))
(autoload 'niwa-permission-mode-effective "niwa-permission-mode")
(autoload 'niwa-permission-mode-normalize "niwa-permission-mode")

;; `niwa-permission-queue'
(declare-function niwa-permission-queue-sweep-request
                  "niwa-permission-queue"
                  (request-id &optional session no-render))

;; `niwa-persistence'
(declare-function niwa-instruction-store-owes-p
                  "niwa-persistence" (session))
(declare-function niwa-instruction-store-settle-session
                  "niwa-persistence" (session))
(autoload 'niwa-instruction-store-owes-p "niwa-persistence")
(autoload 'niwa-instruction-store-settle-session "niwa-persistence")

;; `niwa-plan-handoff'
(declare-function niwa-plan-handoff-settle-request
                  "niwa-plan-handoff"
                  (fsm status &optional reason))

;; `niwa-plan-mode'
(declare-function niwa-plan-approval-abort
                  "niwa-plan-mode" (&optional session outcome))

;; `niwa-ptc-checkpoint'
(declare-function niwa-ptc-checkpoint-clear-settled
                  "niwa-ptc-checkpoint" (session &optional conversation buffer))
(autoload 'niwa-ptc-checkpoint-clear-settled "niwa-ptc-checkpoint")

;; `niwa-reminders'
(declare-function niwa-reminders-restore-reserved-context
                  "niwa-reminders" (buffer))
(autoload 'niwa-reminders-restore-reserved-context "niwa-reminders")


;; `niwa-sandbox'
(declare-function niwa-sandbox-mode-effective
                  "niwa-sandbox" (&optional session permission-mode))
(autoload 'niwa-sandbox-mode-effective "niwa-sandbox")

;; `niwa-session-artifacts'
(declare-function niwa-session-artifacts-assert-mutation-authority
                  "niwa-session-artifacts" (session &optional buffer))
(declare-function niwa-session-artifacts-save
                  "niwa-session-artifacts"
                  (session buffer &optional settled force))
(declare-function niwa-session-artifacts-prepare-save
                  "niwa-session-artifacts"
                  (session buffer &optional settled force defer-comparison))
(declare-function niwa-session-artifacts-call-with-checkpoint
                  "niwa-session-artifacts" (session function))
(declare-function niwa-session-artifacts-recover-working-directory
                  "niwa-session-artifacts" (session))
(declare-function niwa-session-artifacts-check-target-incarnation
                  "niwa-session-artifacts" (session &optional buffer))
(autoload 'niwa-session-publication-async-eligible-p "niwa-session-publication")
(autoload 'niwa-session-publication-submit "niwa-session-publication")
(declare-function niwa-session-publication-submit
                  "niwa-session-publication"
                  (session prepare acknowledge &optional mail-ids retain-failure select-staged superseded))
(autoload 'niwa-session-artifacts-assert-mutation-authority
  "niwa-session-artifacts")
(autoload 'niwa-session-artifacts-save "niwa-session-artifacts")

;; `niwa-session-codec'
(declare-function niwa-session-codec-portable-authority-p "niwa-session-codec" (session))

;; `niwa-session-collection'
(declare-function niwa-session-collection-schedule
                  "niwa-session-collection" (session))
(autoload 'niwa-session-collection-schedule "niwa-session-collection")

;; `niwa-session-persistence'
(declare-function niwa-session-persistence-schedule-cleanup
                  "niwa-session-persistence" (workspace))
(declare-function niwa-session-persistence-start-checkpoints
                  "niwa-session-persistence" nil)
(autoload 'niwa-session-persistence-schedule-cleanup
  "niwa-session-persistence")
(autoload 'niwa-session-persistence-start-checkpoints
  "niwa-session-persistence")
(defvar niwa-session--read-only-mode)
(defvar niwa-session--save-failed)

;; `niwa-sleep'
(declare-function niwa-sleep-update "niwa-sleep" (&rest _))
(autoload 'niwa-sleep-update "niwa-sleep")

;; `niwa-structs'
(declare-function niwa-request-edited-artifacts "niwa-structs" (cl-x))
(declare-function niwa-request-id "niwa-structs" (cl-x))
(declare-function niwa-request-origin "niwa-structs" (cl-x))
(declare-function niwa-request-started-at "niwa-structs" (cl-x))
(declare-function niwa-session-pending-publication
                  "niwa-structs" (cl-x) t)
(declare-function niwa-session-pending-steering
                  "niwa-structs" (cl-x) t)
(declare-function niwa-session-ptc-checkpoints "niwa-structs" (cl-x))
(declare-function niwa-session-set-pending-input-failure-paused
                  "niwa-structs" (session paused))
(declare-function niwa-session-set-pending-inputs
                  "niwa-structs" (session category entries))
(defvar niwa--session)
(defvar niwa--view-buffer)

;; `niwa-readiness'
(declare-function niwa-readiness-record-turn "niwa-readiness" (info outcome message))
(autoload 'niwa-readiness-record-turn "niwa-readiness")

;; `niwa-telemetry'
(declare-function niwa-telemetry-record
                  "niwa-telemetry" (session event &rest props))

;; `niwa-transport'
(declare-function niwa-transport-run-when-idle
                  "niwa-transport" (key path thunk &optional on-cancel delay))
(declare-function niwa-transport-cancel-pending "niwa-transport" (&optional key))

;; `niwa-utilities'
(declare-function niwa--gc-hold "niwa-utilities" (key live-p))
(defvar niwa--gc-settlement-grace)
(declare-function niwa--warn-once
                  "niwa-utilities" (scope key format &rest args))

;; `niwa-view-interaction'
(declare-function niwa-view--interaction-rebuild
                  "niwa-view-interaction" ())
(autoload 'niwa-view--interaction-rebuild "niwa-view-interaction")

;; `niwa-view-render'
(declare-function niwa-view--append-request-summary
                  "niwa-view-render"
                  (data-buf search-start &optional extra))
(declare-function niwa-view-render-refresh-latest-turn
                  "niwa-view-render" (data))
(autoload 'niwa-view--append-request-summary "niwa-view-render")
(autoload 'niwa-view-render-refresh-latest-turn "niwa-view-render")

;; `niwa-workspace'
(declare-function niwa-workspace "niwa-workspace" (&optional buffer))


;;
;;; Request lifecycle

(defvar-local niwa--turn-settlements-pending nil
  "FSMs whose terminal settlement is waiting for transport or its next step.")

(defun niwa-request-active-p (&optional buffer)
  "Return non-nil when BUFFER has an active request.
Status lines ask on every redisplay; reading the binding avoids the
buffer switch, which costs microseconds in a buffer with many locals."
  (let ((buffer (or buffer (current-buffer))))
    (and (buffer-live-p buffer)
         (buffer-local-value 'niwa--current-request buffer))))

(defun niwa-turn-busy-p (&optional buffer)
  "Return non-nil when BUFFER has a request or pending settlement."
  (let ((buffer (or buffer (current-buffer))))
    (and (buffer-live-p buffer)
         (or (buffer-local-value 'niwa--current-request buffer)
             (buffer-local-value 'niwa--turn-settlements-pending
                                 buffer)))))

(defun niwa-busy-p ()
  "Return non-nil while any buffer in this Emacs runs or settles a turn.
The sleep inhibitor asks this; a host deciding when to restart asks
`niwa-restart-unsafe-p'."
  (and (cl-some
        (lambda (buffer)
          (with-current-buffer buffer
            (or (niwa-turn-busy-p buffer)
                (and niwa--agent-invocation
                     (niwa-agent-invocation-runtime-pending-response
                      niwa--agent-invocation))
                (and niwa--session
                     (or (niwa-session-agent-reservations niwa--session)
                         (and (niwa-session-agent-registry niwa--session)
                              (niwa-agent-control-active-turn-p niwa--session)))))))
        (buffer-list))
       t))

;;;###autoload
(defun niwa-restart-unsafe-p ()
  "Return non-nil while restarting this Emacs would interrupt niwa work.
Beyond `niwa-busy-p', an idle session counts while it owns a live
command or Monitor, or an active Goal waiting between turns.  For callers
outside niwa, such as a host deciding when to restart."
  (and (or (niwa-busy-p)
           (cl-some
            (lambda (buffer)
              (when-let* ((session (buffer-local-value 'niwa--session buffer)))
                (or (and (featurep 'niwa-execution)
                         (niwa-execution-session-live-p session))
                    (when-let* ((goal (niwa-session-goal session)))
                      (eq (niwa-goal-status goal) 'active)))))
            (buffer-list)))
       t))

(defun niwa-request-state-label (&optional buffer)
  "Return BUFFER's compact request state label."
  (let ((buffer (or buffer (current-buffer))))
    (cond
     ((niwa-request-active-p buffer) "running")
     ((niwa-turn-busy-p buffer) "settling")
     (t "idle"))))


;;
;;; Request cancellers

(defun niwa-request-push-canceller (request canceller)
  "Append CANCELLER (a zero-arg thunk) onto REQUEST's `cancellers' list.

Each canceller is invoked exactly once during teardown via the
drain-then-invoke helper.  Primitives that own pending overlays
register a thunk that drains their own overlays with the
`aborted' sentinel.  A registration after cancellation runs immediately,
without reopening REQUEST or retaining the thunk."
  (when request
    (if (niwa-request-cancelled-p request)
        (ignore-errors (funcall canceller))
      (setf (niwa-request-cancellers request)
            (append (niwa-request-cancellers request)
                    (list canceller))))))

(defun niwa-request-drain-cancellers (request)
  "Atomically clear and invoke every canceller on REQUEST.

Drains the list before invoking, so a recursive drain cannot invoke the
same thunk twice.  Each canceller runs inside `ignore-errors' so a
misbehaving thunk cannot strand the others.  Registration on a cancelled
request invokes the new thunk immediately rather than adding to this list.

Used by `niwa-abort', `niwa-request-end', and the stale-request
replacement path in `niwa-request-begin'."
  (when request
    (let ((cancellers (niwa-request-cancellers request)))
      (setf (niwa-request-cancellers request) nil)
      (dolist (canceller cancellers)
        (ignore-errors (funcall canceller))))))


;;
;;; Ending a turn at a tool boundary

(defun niwa-turn-end-at-boundary (fsm reason)
  "End FSM's turn once its current tool results are recorded.
Instead of sampling the model again, FSM moves to DONE and settles like any
completed turn: accounting, journal capture, the checkpoint, queued input,
and any Goal continuation all run.  Pending steering is delivered first, so
the turn then ends at the following boundary.  REASON is a symbol naming the
caller for telemetry.  Return non-nil when this call made the request."
  (when-let* ((info (and fsm (niwa-engine-info fsm)))
              ((not (plist-get info :niwa-end-turn))))
    (setf (niwa-engine-info fsm) (plist-put info :niwa-end-turn reason))
    (when-let* ((buffer (plist-get info :buffer))
                ((buffer-live-p buffer))
                (session (buffer-local-value 'niwa--session buffer)))
      (niwa-telemetry-record session 'turn-end-requested :reason reason))
    t))

(defun niwa-turn-end-requested-p (info)
  "Return the reason INFO's turn ends at this tool boundary, or nil."
  (plist-get info :niwa-end-turn))


;;
;;; Request identity and admission

(defun niwa-current-origin ()
  "Return the canonical owner for the current execution context."
  (or (and (bound-and-true-p niwa--current-request)
           (niwa-request-p niwa--current-request)
           (niwa-request-origin niwa--current-request))
      (and-let* ((inv niwa--agent-invocation)
                 ((fboundp 'niwa-agent-invocation-p))
                 ((niwa-agent-invocation-p inv)))
        (niwa-agent-invocation-require-path inv))
      "/root"))

(defun niwa-current-turn (session)
  "Return SESSION's active reserved turn or next turn."
  (if (and (niwa-request-p niwa--current-request)
           (eq session (niwa-request-session niwa--current-request)))
      (niwa-request-turn niwa--current-request)
    (1+ (or (niwa-session-turn-count session) 0))))

(defun niwa-request-note-untracked-effect (request source reason)
  "Record one untracked filesystem effect SOURCE and REASON on REQUEST."
  (unless (assoc source (niwa-request-untracked-effects request))
    (push (cons source reason)
          (niwa-request-untracked-effects request)))
  (niwa-request-untracked-effects request))

(defun niwa-request-assert-target-ready (session)
  "Signal a user error when SESSION's execution target is not ready."
  (when-let* ((session)
              (target (niwa-session-execution-target session)))
    (when (niwa-execution-target-remote-p target)
      (niwa-execution-target-probe
       target nil (niwa-sandbox-mode-effective session))
      (unless (niwa-execution-target-ready-p target)
        (user-error "Execution target is not ready: %s"
                    (niwa-execution-target-readiness-message target))))
    (unless (niwa-execution-target-remote-p target)
      (niwa-execution-target-refresh-incarnation target)))
  t)


(defun niwa--turn-resume-pending-input (request)
  "Rebind ordinary detached steering to an admitted root REQUEST.
A fresh root send releases an interruption hold, but never takes input
from another session, a failed turn, or an open Pending Inputs cockpit."
  (let ((session (niwa-request-session request)))
    (when (and session
               (eq session niwa--session)
               (eq (current-buffer) (niwa-session-root-buffer session))
               (not niwa--agent-invocation)
               (not (and (niwa-session-pending-input-paused session)
                         (niwa-pending-inputs-cockpit-holds-p session))))
      (niwa-session-set-pending-inputs
       session 'steering
       (mapcar (lambda (entry)
                 (if (or (plist-get entry :request-id)
                         (eq 'failed-turn (plist-get entry :state)))
                     entry
                   (plist-put (copy-sequence entry) :request-id
                              (niwa-request-id request))))
               (niwa-session-pending-steering session)))
      (niwa-session-set-pending-input-paused session nil))))

(defun niwa-request-begin (session &optional options)
  "Create a new request for SESSION, guarding against stale requests.

If `niwa--current-request' is already set, log a warning and replace
it.  Signal instead if yielding checks or cancellation callbacks transfer
ownership or start deferred settlement.  Return the new request struct.
OPTIONS may explicitly supply `:permission-mode' and `:plan-read-only',
including nil, instead of shared pending/session defaults.  Explicit mode
does not consume pending mode.
OPTIONS `:admitted' is called with the new request as soon as it owns the
buffer, so a caller can still recognize its own admission when a later
admission step signals.

A root request is the user's retry: blocking recovery issues refuse it, and
it clears the previous attempt's informational issues and a failure pause no
failed input still justifies.  A retained agent's admission leaves the root's
recovery state alone."
  (when (memq session (bound-and-true-p niwa-claude-code-fork--sessions))
    (user-error "Wait for native history preparation before starting a turn"))
  (unless niwa--agent-invocation
    (niwa-recovery-assert-ready session))
  (let ((entry-request niwa--current-request))
    (when niwa--turn-settlements-pending
      (user-error "Turn settlement is still pending"))
    (niwa-request-assert-target-ready session)
    (niwa-session-artifacts-assert-mutation-authority
     session (current-buffer))
    ;; A committed Rewind may still owe the workspace instruction store its
    ;; prune.  A new turn reuses the discarded turn numbers, which would
    ;; then look like evidence for the discarded activity, so resolve it
    ;; first and refuse the request while that fails.
    (when (and (not niwa--agent-invocation)
               (niwa-instruction-store-owes-p session))
      (niwa-instruction-store-settle-session session))
    ;; Target checks can yield before stale-request teardown.  A request
    ;; admitted there is not the stale request this caller came to replace.
    (unless (eq entry-request niwa--current-request)
      (user-error "Request ownership changed during admission checks"))
    (when niwa--turn-settlements-pending
      (user-error "Turn settlement is still pending")))
  (when niwa--current-request
    (message "niwa: stale request found, replacing")
    (niwa-request-end t))
  ;; Teardown invokes cancellers, which can admit another request or begin
  ;; deferred terminal work.  The outer admission must not replace either.
  (when niwa--turn-settlements-pending
    (user-error "Turn settlement is still pending"))
  (when niwa--current-request
    (user-error "Another request was admitted during cancellation"))
  (unless niwa--agent-invocation
    (niwa-recovery-clear-informational session)
    (unless (cl-find 'failed-turn
                     (append (niwa-session-pending-steering session)
                             (niwa-session-pending-follow-ups session))
                     :key (lambda (entry) (plist-get entry :state)))
      (niwa-session-set-pending-input-failure-paused session nil)))
  ;; Readiness and recovery checks can yield.  Recheck immediately before
  ;; capture, keeping ordinary policy inspection independent of admission.
  (when (memq session (bound-and-true-p niwa-claude-code-fork--sessions))
    (user-error "Wait for native history preparation before starting a turn"))
  (when niwa-implementation-policy--admission-preparation
    (niwa-implementation-policy-assert-preparation
     niwa-implementation-policy--admission-preparation))
  (let* ((implementation-policy
          (copy-tree (niwa-implementation-policy-validate
                      (niwa-implementation-policy-current session))))
         (permission-mode-override
          (when-let* ((mode
                       (if (plist-member options :permission-mode)
                           (plist-get options :permission-mode)
                         (or niwa--pending-permission-mode
                             (plist-get implementation-policy :permission-mode)
                             (and niwa--agent-invocation
                                  (niwa-agent-invocation-permission-mode-override
                                   niwa--agent-invocation))))))
            (niwa-permission-mode-normalize mode)))
         (origin (niwa-current-origin))
         (id (format "request-%s-%s"
                     (format-time-string "%Y%m%dT%H%M%S")
                     (substring
                      (secure-hash
                       'sha1
                       (format "%s:%s:%s" (emacs-pid) (float-time) origin))
                      0 12)))
         (request (niwa-request--create
                   :id id
                   :session session
                   :implementation-policy implementation-policy
                   :permission-mode-override permission-mode-override
                   :turn (1+ (or (niwa-session-turn-count session) 0))
                   :file-snapshots (make-hash-table :test #'equal)
                   :untracked-effects nil
                   :plan-read-only
                   (if (plist-member options :plan-read-only)
                       (plist-get options :plan-read-only)
                     (and niwa--agent-invocation
                          (niwa-agent-invocation-plan-read-only
                           niwa--agent-invocation)))
                   :started-at (current-time)
                   :origin origin)))
    (setf (niwa-request-context request)
          (list :buffer (current-buffer) :niwa-request-id id
                :niwa-request request))
    (setq niwa--current-request request)
    (when-let* ((admitted (plist-get options :admitted)))
      (funcall admitted request))
    (niwa--turn-resume-pending-input request)
    (setq niwa--pending-implementation-policy nil)
    (unless (plist-member options :permission-mode)
      (setq niwa--pending-permission-mode nil))
    ;; Emacs auto-saves only after input; an unattended turn needs its own
    ;; mid-turn checkpoints.
    (niwa-session-persistence-start-checkpoints)
    ;; System sleep would suspend the turn.
    (niwa-sleep-update)
    (let ((buffer (current-buffer)))
      (niwa--gc-hold
       request (lambda ()
                 (and (buffer-live-p buffer)
                      (eq request (buffer-local-value 'niwa--current-request buffer))))))
    (when (fboundp 'niwa-collaboration-notify-request-changed)
      (niwa-collaboration-notify-request-changed (current-buffer)))
    (when (equal origin "/root")
      (setf (niwa-session-agent-root-activity session) 'running))
    (niwa-telemetry-record
     session 'request-queued :request-id id :origin origin
     :permission-mode (niwa-permission-mode-effective session nil nil request)
     :sandbox-mode (niwa-sandbox-mode-effective session))
    (niwa-telemetry-record
     session 'request-start :request-id id :origin origin
     :permission-mode (niwa-permission-mode-effective session nil nil request)
     :sandbox-mode (niwa-sandbox-mode-effective session))
    request))

(defun niwa-request-call-owned (request buffer function &rest args)
  "Call FUNCTION with ARGS in BUFFER while REQUEST still owns it.
Ignore delivery to a dead buffer, a cancelled request, or a replacement owner."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (and (eq request niwa--current-request)
                 (or (null request)
                     (not (niwa-request-cancelled-p request))))
        (apply function args)))))

(defun niwa-request-cancel (request &optional abort-plan-approval)
  "Cancel REQUEST terminally and drain its owned pending interactions.
Queued permission prompts are swept only for REQUEST's identity.  Plan
approvals normally outlive the request that presented them; when
ABORT-PLAN-APPROVAL is non-nil, abort it too."
  (when request
    (let ((session (niwa-request-session request))
          (request-id (niwa-request-id request)))
      (setf (niwa-request-cancelled-p request) t)
      (niwa-request-drain-cancellers request)
      (when (and request-id
                 (fboundp 'niwa-permission-queue-sweep-request))
        (niwa-permission-queue-sweep-request request-id session))
      (when (and abort-plan-approval
                 (fboundp 'niwa-plan-approval-abort))
        (niwa-plan-approval-abort session)))))

(defun niwa-request-end (&optional abort-plan-approval)
  "Cancel the current request, then clear `niwa--current-request'.

Also returns any hook context the ended request reserved.  Reminder
injection runs ahead of the handler that begins a request, so a delivered
payload has already released its reservation and only an undelivered one
is returned here."
  (niwa-reminders-restore-reserved-context (current-buffer))
  (when niwa--current-request
    (let ((request niwa--current-request))
      ;; The slot is what every caller reads as "a turn is running"; a
      ;; teardown step that signals must not leave it set.
      (unwind-protect
          (unwind-protect
              (niwa-telemetry-record
               (niwa-request-session request) 'request-teardown
               :request-id (niwa-request-id request)
               :origin (niwa-request-origin request)
               :abort-plan-approval (and abort-plan-approval t))
            (niwa-request-cancel request abort-plan-approval))
        ;; Cancelling interactions invokes callbacks; do not erase a request
        ;; installed by one of them while the old teardown was on the stack.
        (when (eq request niwa--current-request)
          (setq niwa--current-request nil)
          (unwind-protect
              (progn
                ;; Versioning can dispatch a new request, whose activity wins.
                (when (equal (niwa-request-origin request) "/root")
                  (setf (niwa-session-agent-root-activity
                         (niwa-request-session request))
                        'idle))
                ;; A turn that edited store artifacts leaves one version of each.
                (pcase-dolist (`(,workspace . ,id) (niwa-request-edited-artifacts request))
                  (condition-case err
                      (niwa-artifact-backend-save-version
                       workspace id (niwa-session-session-id
                                     (niwa-request-session request)))
                    (error (niwa-diagnostic-message (niwa-diagnostic-scope workspace)
                                                       "%s (no version of %s was saved)"
                                                       (error-message-string err) id))))
                (when (equal (niwa-request-origin request) "/root")
                  ;; A long-running Emacs must keep expiring old sessions.
                  (niwa-session-persistence-schedule-cleanup
                   (niwa-session-workspace (niwa-request-session request)))))
            ;; Settlement leaves journal publication and collection behind it,
            ;; which allocate as much as the turn did; keep the threshold a
            ;; little longer rather than collecting repeatedly through them.
            (let ((until (+ (float-time) niwa--gc-settlement-grace)))
              (niwa--gc-hold request (lambda () (< (float-time) until))))
            (when (fboundp 'niwa-collaboration-notify-request-changed)
              (let ((context (plist-get (niwa-engine-info (niwa-engine-owner request)) :context))
                    (session (niwa-request-session request)))
                (niwa-collaboration-notify-request-changed
                 (current-buffer)
                 (and session (eq (current-buffer) (niwa-session-root-buffer session))
                      (or (and (listp context) (plist-get context :turn-trigger))
                          'user)))))))))))


;;
;;; Terminal transaction

(defvar niwa-turn-settled-functions nil
  "Abnormal hook run in a root chat buffer once its turn settles.
Each function receives the terminal outcome symbol, such as `success',
`error' or `abort'.  A failing function is reported and does not stop
settlement.")

(autoload 'niwa-schedule-settle-wake "niwa-schedule")

(defvar niwa-turn-settlement-reason nil
  "Provider's reason for the current terminal outcome, while its hooks run.")

(defun niwa--turn-publish-outcome (info outcome reason)
  "Publish OUTCOME and REASON for INFO's turn in the current root buffer."
  (when (bound-and-true-p niwa--session)
    (setf (niwa-session-last-outcome niwa--session) outcome)
    (let ((context (plist-get info :context)))
      (when (and (listp context) (plist-get context :wake-runs))
        (condition-case err
            (niwa-schedule-settle-wake niwa--session (current-buffer) context outcome reason)
          (error (niwa-diagnostic-message (niwa-diagnostic-scope (current-buffer))
                                             "Wake run settlement failed: %s"
                                             (error-message-string err)))))))
  (let ((niwa-turn-settlement-reason reason))
    (dolist (function niwa-turn-settled-functions)
      (condition-case err
          (funcall function outcome)
        (error (niwa-diagnostic-message (niwa-diagnostic-scope (current-buffer))
                                           "%s (turn settlement step %s failed)"
                                           (error-message-string err) function))))))

(defun niwa--turn-record-settlement (fsm outcome)
  "Record terminal OUTCOME for FSM's active request.
Run `niwa-turn-settled-functions' with OUTCOME in the chat buffer."
  (when-let* ((info (niwa-engine-info fsm))
              (chat-buffer (plist-get info :buffer))
              ((buffer-live-p chat-buffer)))
    (with-current-buffer chat-buffer
      (niwa--turn-publish-outcome info outcome (niwa--fsm-error-message fsm))
      (when (featurep 'niwa-readiness)
        (niwa-readiness-record-turn info outcome (niwa--fsm-error-message fsm)))
      (when (and (bound-and-true-p niwa--session)
                 (bound-and-true-p niwa--current-request))
        (let* ((request niwa--current-request)
               (tokens (or (plist-get info :tokens-full)
                           (plist-get info :tokens)))
               (started-at (niwa-request-started-at request)))
          (niwa-telemetry-record
           niwa--session 'request-settled
           :request-id (niwa-request-id request)
           :origin (niwa-request-origin request)
           :outcome outcome
           :duration-ms (and started-at
                             (round
                              (* 1000.0
                                 (float-time
                                  (time-subtract (current-time)
                                                 started-at)))))
           :provider-status (plist-get info :status)
           :token-source (if (plist-get info :tokens-full)
                             'tokens-full
                           'tokens)
           :input-tokens (and (listp tokens) (plist-get tokens :input))
           :output-tokens (and (listp tokens) (plist-get tokens :output))
           :cached-tokens (and (listp tokens)
                               (or (plist-get tokens :cached)
                                   (plist-get tokens :cache-read)
                                   (plist-get tokens :cache_read)))))))))

(defun niwa--turn-settle-plan-handoff (fsm status)
  "Settle any Direct Plan handoff attached to FSM with STATUS."
  (when (fboundp 'niwa-plan-handoff-settle-request)
    (niwa-plan-handoff-settle-request
     fsm status
     (and (not (eq status 'success))
          (or (niwa--fsm-error-message fsm)
              (if (eq status 'aborted)
                  "Request aborted"
                "Provider request failed"))))))

(defun niwa--fsm-error-message (fsm)
  "Return a compact error message for FSM, or nil."
  (let* ((info (and fsm (niwa-engine-info fsm)))
         (error (plist-get info :error))
         (status (plist-get info :status))
         (error-type (and (listp error) (plist-get error :type)))
         (error-message (and (listp error) (plist-get error :message))))
    (or (and (stringp error) error)
        error-message
        (and error-type status (format "%s: %s" error-type status))
        (and error-type (format "%s" error-type))
        (and status (format "%s" status)))))

(defun niwa--turn-record-request-failure (fsm)
  "Add FSM's provider failure to its ignored request summary."
  (when-let* ((info (niwa-engine-info fsm))
              (chat-buffer (plist-get info :buffer))
              ((buffer-live-p chat-buffer)))
    (with-current-buffer chat-buffer
      (let* ((error-data (plist-get info :error))
             (request (plist-get info :niwa-request))
             (backend (or (plist-get info :backend) gptel-backend))
             (backend-name
              (and backend
                   (condition-case nil
                       (gptel-backend-name backend)
                     (error nil)))))
        (when (bound-and-true-p niwa--session)
          ;; Informational only: the next root request is the retry.  The
          ;; provider's text stays in the transcript summary below.
          (let ((category (niwa-recovery-category
                           (niwa--fsm-error-message fsm)
                           (and (listp error-data) (plist-get error-data :code))
                           (plist-get info :http-status))))
            (niwa-recovery-report
             niwa--session "request" category
             (concat (niwa-recovery-message category) "; see the transcript")
             nil)
            ;; Recover only the ordinary root selection that is still current.
            ;; Accepted implementation choices are request-local, and
            ;; an old failure must not overwrite a newer user selection.
            (when-let* (((eq category 'model))
                         ((not (and request
                                    (niwa-request-implementation-policy request))))
                         ((eq chat-buffer (niwa-session-root-buffer niwa--session)))
                         ((eq backend gptel-backend))
                         ((eq (or (plist-get info :model) gptel-model) gptel-model))
                         (selector (format "%s:%s" backend-name
                                           (gptel--model-name
                                            (or (plist-get info :model) gptel-model))))
                         (provider (niwa-model-recover-provider selector nil t)))
              (niwa-model-set-session-provider niwa--session provider chat-buffer)
              (let ((notice (format "%s is unavailable; using %s" selector
                                    (niwa-model--provider-label provider))))
                (niwa-recovery-report niwa--session "model" 'model notice nil)
                (niwa-diagnostic-message (niwa-diagnostic-scope chat-buffer)
                                            "%s" notice)))))
        (niwa-view--append-request-summary
         chat-buffer
         (plist-get info :position)
         (list :outcome 'error
               :request-id (plist-get info :niwa-request-id)
               :external-history (plist-get info :external-history)
               :backend (or backend-name "Provider")
               :status (plist-get info :status)
               :error-type (and (listp error-data)
                                (plist-get error-data :type))
               :error-code (and (listp error-data)
                                (plist-get error-data :code))
               :error-data error-data
               :message (niwa--fsm-error-message fsm)
               :retry-errors (copy-tree (plist-get info :niwa-retry-errors))
               :retry-exhausted (plist-get info :niwa-retry-exhausted)
               :retry 'manual))))))

(defun niwa--run-turn-terminal-hook (fsm event status)
  "Run top-level turn terminal hook EVENT for FSM with STATUS."
  (when-let* ((info (niwa-engine-info fsm))
              (chat-buffer (plist-get info :buffer))
              ((buffer-live-p chat-buffer)))
    (with-current-buffer chat-buffer
      (when (bound-and-true-p niwa--session)
        (let* ((workspace (niwa-workspace))
               (reason (and (eq event 'StopFailure)
                            (or (and (not (eq status 'lost))
                                     (niwa--fsm-error-message fsm))
                                (symbol-name status)))))
          (niwa-hooks-run-event
           event
           (niwa-hooks-event-plist
            event niwa--session workspace
            :status (symbol-name status)
            :terminal-reason reason)
           #'ignore
           niwa--session workspace niwa--current-request nil))))))


(defun niwa--turn-commit (fsm)
  "Commit FSM's request-reserved turn to its live session.
Signal when the request is missing or its reservation is not the next turn."
  (when-let* ((info (niwa-engine-info fsm))
              (chat-buffer (plist-get info :buffer))
              ((buffer-live-p chat-buffer)))
    (with-current-buffer chat-buffer
      (unless (and niwa--session niwa--current-request)
        (error "Cannot commit turn without an active request"))
      (let* ((reserved (niwa-request-turn niwa--current-request))
             (retry (plist-get info :niwa-retry-of))
             (committed (or (niwa-session-turn-count niwa--session) 0))
             (expected (if retry committed (1+ committed))))
        (unless (equal reserved expected)
          (error "%s turn %S conflicts with committed turn %S"
                 (if retry "Retry reservation for" "Reserved") reserved committed))
        (setf (niwa-session-turn-count niwa--session) reserved)))))

(defvar niwa--turn-resume nil
  "Continuation for a step returning `niwa-turn-pending'.
Call with non-nil argument to cancel, otherwise resume.")

(defun niwa--turn-save (fsm)
  "Publish FSM's completed turn before allowing later settlement steps.
Warm local saves use the publication queue when a continuation is available."
  (unless (plist-get (niwa-engine-info fsm) :niwa-save-state)
    (setf (niwa-engine-info fsm)
          (plist-put (niwa-engine-info fsm) :niwa-turn-saved nil)))
  (when-let* ((chat-buffer (plist-get (niwa-engine-info fsm) :buffer))
              ((buffer-live-p chat-buffer)))
    (with-current-buffer chat-buffer
      (when (and niwa--session
                 (not (bound-and-true-p niwa-session--read-only-mode)))
        (unless (plist-get (niwa-engine-info fsm) :niwa-save-state)
          (let* ((session niwa--session)
                 (resume (and niwa--turn-resume
                              (niwa-session-publication-async-eligible-p session)
                              niwa--turn-resume))
                 prepared retired
                 (finish
                  (lambda (success err)
                    (unwind-protect
                        (when (and (niwa--turn-current-p fsm)
                                   (eq session (buffer-local-value 'niwa--session chat-buffer)))
                          (with-current-buffer chat-buffer
                            (when (and success prepared)
                              (condition-case cleanup
                                  (funcall (plist-get prepared :finish))
                                ((error quit) (setq success nil err cleanup))))
                            (setq niwa-session--save-failed (not success))
                            (setf (niwa-engine-info fsm)
                                  (plist-put (niwa-engine-info fsm) :niwa-turn-saved success))
                            (unless success
                              ;; Other conversations may have checkpointed in the meantime.
                              (dolist (row retired)
                                (unless (seq-find
                                         (lambda (current) (equal (plist-get row :id)
                                                                 (plist-get current :id)))
                                         (niwa-session-ptc-checkpoints session))
                                  (push row (niwa-session-ptc-checkpoints session))))
                              (niwa--warn-once (niwa-diagnostic-scope session)
                                                 'turn-auto-save "%s (Session auto-save failed)"
                                                 (error-message-string err)))
                            (force-mode-line-update)))
                      (when resume
                        (setf (niwa-engine-info fsm)
                              (plist-put (niwa-engine-info fsm) :niwa-save-state 'done))
                        (funcall resume)))))
                 (retire
                  (lambda ()
                    (let ((before (niwa-session-ptc-checkpoints session)))
                      (niwa-ptc-checkpoint-clear-settled session "/root" chat-buffer)
                      (setq retired (cl-set-difference
                                     before (niwa-session-ptc-checkpoints session)
                                     :test #'eq))))))
            (condition-case err
                (if resume
                    (progn
                      (setf (niwa-engine-info fsm)
                            (plist-put (niwa-engine-info fsm) :niwa-save-state 'waiting))
                      (niwa-session-publication-submit
                       session
                       (lambda ()
                         (unless (niwa--turn-current-p fsm)
                           (error "Turn replaced before session save"))
                         (with-current-buffer chat-buffer
                           (niwa-session-artifacts-recover-working-directory session)
                           (niwa-session-artifacts-check-target-incarnation session chat-buffer)
                           (funcall retire)
                           (setq prepared
                                 (niwa-session-artifacts-call-with-checkpoint
                                  session (lambda () (niwa-session-artifacts-prepare-save
                                                      session chat-buffer t nil t))))
                           (prog1 (plist-get prepared :artifacts)
                             (setf (plist-get prepared :artifacts) nil))))
                       finish nil t
                       (lambda (batch) (funcall (plist-get prepared :select-staged) batch))
                       (lambda () (funcall resume t))))
                  (funcall retire)
                  (unless (niwa-session-artifacts-save session chat-buffer t)
                    (error "Session auto-save did not publish"))
                  (funcall finish t nil))
              (error (funcall finish nil err)))))
        (when (eq 'waiting (plist-get (niwa-engine-info fsm) :niwa-save-state))
          'niwa-turn-pending)))))

(defun niwa--turn-checkpoint (fsm)
  "Checkpoint FSM's saved turn, offer maintenance and refresh its view.
The terminal admission hold stays live between publication and this phase."
  (catch 'waiting
    (when-let* ((info (niwa-engine-info fsm))
                ((plist-get info :niwa-turn-saved))
                (chat-buffer (plist-get info :buffer))
                ((buffer-live-p chat-buffer)))
      (with-current-buffer chat-buffer
        (let* ((workspace (niwa-session-workspace niwa--session))
               (root-p (and (eq chat-buffer (niwa-session-root-buffer niwa--session))
                            (not niwa--agent-invocation))))
          (condition-case err
              (progn
                (require 'niwa-journal-capture)
                (if (and niwa--turn-resume root-p niwa-journal-enabled
                         (eq system-type 'gnu/linux)
                         (not (file-remote-p (niwa-workspace-root workspace)))
                         (niwa-session-codec-portable-authority-p niwa--session))
                    (let ((resume niwa--turn-resume))
                      (unless (plist-get info :niwa-checkpoint-state)
                        (setf (niwa-engine-info fsm)
                              (plist-put (niwa-engine-info fsm) :niwa-checkpoint-state 'waiting))
                        (let ((cancel
                               (niwa-journal-capture-checkpoint-start
                                niwa--session chat-buffer
                                (lambda (result)
                                  (let ((current (niwa-engine-info fsm)))
                                    (setq current (plist-put current :niwa-checkpoint-error (plist-get result :error))
                                          current (plist-put current :niwa-checkpoint-state 'done))
                                    (setf (niwa-engine-info fsm) current))
                                  (funcall resume)))))
                          (setf (niwa-engine-info fsm)
                                (plist-put (niwa-engine-info fsm) :niwa-checkpoint-cancel cancel))))
                      (when (eq (plist-get (niwa-engine-info fsm) :niwa-checkpoint-state) 'waiting)
                        (throw 'waiting 'niwa-turn-pending))
                      (when-let* ((failure (plist-get (niwa-engine-info fsm)
                                                      :niwa-checkpoint-error)))
                        (error "%s" failure)))
                  (niwa-journal-capture-checkpoint niwa--session chat-buffer))
                (niwa-journal-process-schedule workspace root-p)
                (when root-p (niwa-journal-idle-arm-seal niwa--session chat-buffer)))
            (error
             (niwa--warn-once (niwa-diagnostic-scope workspace)
                                'journal-capture "%s (Journal capture checkpoint failed)"
                                 (error-message-string err))))
          (when root-p
            (niwa-journal-cleanup-schedule workspace)
            (niwa-memory-decision-schedule-recovery
             workspace (lambda () (niwa-memory-pass-schedule workspace))))
          (niwa-session-collection-schedule niwa--session)
          (when (buffer-live-p niwa--view-buffer)
            ;; The save appended the turn's fork-point record after the
            ;; response; only the newest turn needs its new coordinates.
            (with-current-buffer niwa--view-buffer
              (niwa-view-render-refresh-latest-turn chat-buffer))))))))

(defun niwa--turn-end-request (fsm)
  "End the active niwa request for FSM's request buffer."
  (when-let* ((info (niwa-engine-info fsm))
              (chat-buffer (plist-get info :buffer))
              ((buffer-live-p chat-buffer)))
    (with-current-buffer chat-buffer
      (when (or (plist-get info :error)
                (eq (plist-get info :niwa-terminal-status) 'error))
        (niwa-gptel-retry-record-failure fsm))
      (niwa-request-end))))

(defun niwa--turn-request-id (info)
  "Return the request id of the turn whose context is INFO.
Call in the turn's chat buffer, before the request ends."
  (if niwa--current-request
      (niwa-request-id niwa--current-request)
    (plist-get info :niwa-request-id)))

(defun niwa--turn-requeue-steering (fsm)
  "Requeue steering FSM's successful turn left undelivered as first follow-ups.
A gptel turn cannot complete with its steering pending; an external turn
cannot wait at a held boundary, so its held steering starts the next turns."
  (when-let* ((info (niwa-engine-info fsm))
              (chat-buffer (plist-get info :buffer))
              ((buffer-live-p chat-buffer)))
    (with-current-buffer chat-buffer
      (when (and niwa--session
                 (niwa-pending-inputs-requeue-steering
                  niwa--session (niwa--turn-request-id info))
                 (buffer-live-p niwa--view-buffer))
        (with-current-buffer niwa--view-buffer
          (niwa-view--interaction-rebuild))))))

(defun niwa--turn-fail-pending-input (fsm &optional status)
  "Retain undelivered steering for FSM's dead turn.
An intentional `aborted' STATUS detaches normal pending input and pauses its
delivery; other dead turns leave their steering requiring failure review."
  (when-let* ((info (niwa-engine-info fsm))
              (chat-buffer (plist-get info :buffer))
              ((buffer-live-p chat-buffer)))
    (with-current-buffer chat-buffer
      (when niwa--session
        (let* ((request-id (niwa--turn-request-id info))
               (interrupted (eq status 'aborted))
               (entries (niwa-session-pending-steering niwa--session))
               (failed nil)
               (updated
                (mapcar
                 (lambda (entry)
                   (if (and request-id (equal request-id (plist-get entry :request-id)))
                       (progn
                         (setq failed t)
                         (let ((copy (plist-put (copy-sequence entry) :delivering nil)))
                           (if interrupted
                               (plist-put copy :request-id nil)
                             (plist-put copy :state 'failed-turn))))
                     entry))
                 entries)))
          (when failed
            (niwa-session-set-pending-inputs
             niwa--session 'steering updated)
            (unless interrupted
              (niwa-session-set-pending-input-failure-paused
               niwa--session t)))
          (when (and interrupted (niwa-session-pending-input-p niwa--session))
            (niwa-session-set-pending-input-paused niwa--session t))
          (when (or failed interrupted)
            (when (buffer-live-p niwa--view-buffer)
              (with-current-buffer niwa--view-buffer
                (niwa-view--interaction-rebuild)))))))))

(defun niwa--run-turn-steps (fsm steps)
  "Run FSM through STEPS while it still owns the buffer's terminal work.
Recheck between steps: publication and hooks can dispatch other callbacks."
  (let (result)
    (dolist (step steps result)
      (when (niwa--turn-current-p fsm)
        (setq result (funcall (niwa--safe-fsm-handler step) fsm))))))

(defun niwa--turn-buffer (fsm)
  "Return FSM's live chat buffer, or nil."
  (when-let* ((info (condition-case nil (niwa-engine-info fsm) (error nil)))
              (buffer (plist-get info :buffer))
              ((buffer-live-p buffer)))
    buffer))

(defun niwa--turn-current-p (fsm)
  "Return non-nil unless FSM's buffer or request has been replaced.
An empty request slot permits lost-turn settlement and post-teardown steps.
Sessionless machines have no buffer ownership to check."
  (let* ((info (condition-case nil (niwa-engine-info fsm) (error nil)))
         (buffer (plist-get info :buffer)))
    (or (null buffer)
        (and (buffer-live-p buffer)
             (with-current-buffer buffer
               (and (or (not (plist-member info :niwa-settlement-session))
                        (eq niwa--session
                            (plist-get info :niwa-settlement-session)))
                    (or (null niwa--current-request)
                        (equal (plist-get info :niwa-request-id)
                               (niwa-request-id niwa--current-request)))))))))

(defun niwa--turn-hold (fsm)
  "Fence admission while FSM owns a terminal continuation.
Holds nest across final-patch generation and deferred durable settlement."
  (when-let* ((buffer (niwa--turn-buffer fsm)))
    (with-current-buffer buffer
      (let ((info (niwa-engine-info fsm)))
        (unless (plist-member info :niwa-settlement-session)
          (setq info (plist-put info :niwa-settlement-session niwa--session)))
        (when (and niwa--current-request
                   (equal (plist-get info :niwa-request-id)
                          (niwa-request-id niwa--current-request)))
          (setq info (plist-put info :niwa-request niwa--current-request)))
        (setf (niwa-engine-info fsm)
              (plist-put info :niwa-settlement-holds
                         (1+ (or (plist-get info :niwa-settlement-holds) 0)))))
      (cl-pushnew fsm niwa--turn-settlements-pending :test #'eq))))

(defun niwa--turn-release (fsm)
  "Release one of FSM's terminal continuation holds."
  (let* ((info (niwa-engine-info fsm))
         (remaining (max 0 (1- (or (plist-get info :niwa-settlement-holds) 0)))))
    (setf (niwa-engine-info fsm)
          (plist-put info :niwa-settlement-holds remaining))
    (when (zerop remaining)
      (when-let* ((buffer (niwa--turn-buffer fsm)))
        (with-current-buffer buffer
          (setq niwa--turn-settlements-pending
                (delq fsm niwa--turn-settlements-pending))
          (niwa-gptel-retry-refresh-availability buffer)
          ;; A drain offered before the last hold cleared may have refused.
          (when (plist-get info :niwa-input-delivery-after-release)
            (setf (niwa-engine-info fsm)
                  (plist-put (niwa-engine-info fsm)
                             :niwa-input-delivery-after-release nil))
            (when (and (plist-get info :niwa-turn-settled)
                       (not (niwa-turn-busy-p buffer))
                       (not (niwa--turn-publication-pending-p fsm)))
              (niwa-pending-inputs-schedule-held-input-delivery buffer))))))))

(defun niwa--defer-turn-steps (fsm steps &optional on-cancel)
  "Run FSM through STEPS with transport ownership and event-loop breaks.
Keep admission fenced through publication, even when abort stops the provider.
ON-CANCEL runs on transport cancellation; otherwise perform local teardown.
Cheap steps share five milliseconds; an expensive step ends that callback.
A step returning `niwa-turn-pending' stays at the front and calls
`niwa--turn-resume' when ready.  No timer polls while its child is working."
  (let* ((info (niwa-engine-info fsm))
         (request-id (plist-get info :niwa-request-id))
         (buffer (niwa--turn-buffer fsm))
         finished)
    (unless request-id (error "Cannot defer turn without a request identity"))
    (niwa--turn-hold fsm)
    (cl-labels
     ((cancel-checkpoint ()
        (when-let* ((cancel-checkpoint (plist-get (niwa-engine-info fsm) :niwa-checkpoint-cancel)))
          (setf (niwa-engine-info fsm)
                (plist-put (niwa-engine-info fsm) :niwa-checkpoint-cancel nil))
          (funcall cancel-checkpoint)))
      (release ()
        (cancel-checkpoint)
        (when (buffer-live-p buffer)
          (with-current-buffer buffer (remove-hook 'kill-buffer-hook #'teardown t)))
        (niwa--turn-release fsm))
      (teardown ()
        (unwind-protect
            (niwa-transport-cancel-pending (list 'turn-settlement request-id))
          (cancel)))
      (cancel ()
        (unless finished
          (setq finished t)
          (unwind-protect
              (progn
                (cancel-checkpoint)
                (when (niwa--turn-current-p fsm)
                  (if on-cancel (funcall on-cancel)
                    (niwa--run-turn-steps fsm '(niwa--turn-end-request)))))
            (setf (niwa-engine-info fsm)
                  (plist-put (niwa-engine-info fsm) :niwa-turn-settled nil))
            (release))))
      (advance ()
        (unless finished
          (condition-case err
              (let ((deadline (+ (float-time) .005)) waiting
                    (niwa--turn-resume
                     (lambda (&optional abort)
                       (unless finished (if abort (cancel) (enqueue .001))))))
                (while (and (not finished) (not waiting) steps
                            (niwa--turn-current-p fsm) (< (float-time) deadline))
                  (if (eq (niwa--run-turn-steps fsm (list (car steps))) 'niwa-turn-pending)
                      (setq waiting t)
                    (pop steps)))
                (unless finished
                  (cond
                   ((not (and steps (niwa--turn-current-p fsm)))
                    (setq finished t)
                    (release))
                   ((not waiting) (enqueue .001)))))
            ((error quit) (cancel) (signal (car err) (cdr err))))))
      (enqueue (&optional delay)
        (condition-case err
            (unless (niwa-transport-run-when-idle
                     (list 'turn-settlement request-id)
                     (and (buffer-live-p buffer) (buffer-local-value 'default-directory buffer))
                     #'advance #'cancel delay)
              (cancel))
          ((error quit) (cancel) (signal (car err) (cdr err))))))
     (when (buffer-live-p buffer)
       (with-current-buffer buffer (add-hook 'kill-buffer-hook #'teardown nil t)))
     (enqueue))))

(defun niwa--turn-publication-pending-p (fsm)
  "Return non-nil when FSM's session has failed critical publication."
  (when-let* ((info (condition-case nil
                        (niwa-engine-info fsm)
                      (error nil)))
              (buffer (plist-get info :buffer))
              ((buffer-live-p buffer)))
    (with-current-buffer buffer
      (and niwa--session
           (niwa-session-pending-publication niwa--session)))))

(defun niwa--turn-after-publication (function fsm)
  "Call FUNCTION with FSM unless critical publication is pending."
  (unless (niwa--turn-publication-pending-p fsm)
    (funcall function fsm)))

(defun niwa--turn-stamp-settled (fsm)
  "Mark FSM\='s turn as settled on its own info plist.
The stamp remains set after settlement, so a repeated terminal transition
on the same machine is recognized even after the request slot has been
emptied.  Transport cancellation clears it before releasing the admission
fence so a later terminal transition can retry the lost settlement."
  (if-let* ((info (niwa-engine-info fsm)))
      (if-let* ((stamp (memq :niwa-turn-settled info)))
          (setcar (cdr stamp) t)
        (nconc info (list :niwa-turn-settled t)))
    (error "Cannot stamp an FSM without info")))

(defun niwa--turn-settled-p (fsm)
  "Return non-nil when FSM\='s turn has nothing left to settle.

A terminal transition can arrive twice.  gptel drives settlement from the
curl process sentinel, and a request that already failed -- a dropped
connection when the machine suspends, say -- settles once on the failure
and then again when Emacs reaps the dead process on resume.  The first
settlement stamps FSM, and the stamp rather than an empty request slot
is what marks the repeat: an empty slot with no stamp is a lost turn
(`niwa--turn-lost-p'), not a settled one.  A delayed transition whose
buffer already runs a newer request also settles nothing; that turn was
cancelled with its own teardown when the newer request replaced it.  A
machine without a niwa request identity never began a turn to settle."
  (when-let* ((info (condition-case nil (niwa-engine-info fsm) (error nil)))
              (chat-buffer (plist-get info :buffer))
              ((buffer-live-p chat-buffer)))
    (or (plist-get info :niwa-turn-settled)
        (null (plist-get info :niwa-request-id))
        (with-current-buffer chat-buffer
          (and niwa--current-request
               (not (equal (plist-get info :niwa-request-id)
                           (niwa-request-id niwa--current-request))))))))

(defun niwa--turn-lost-p (fsm)
  "Return non-nil when FSM\='s turn was lost without settling.
FSM carries a request identity and no settled stamp, but its request
slot is empty: something cleared the request without running its
settlement, which is the state a terminal transition lost with its
process leaves behind."
  (when-let* ((info (condition-case nil (niwa-engine-info fsm) (error nil)))
              ((plist-get info :niwa-request-id))
              (chat-buffer (plist-get info :buffer))
              ((buffer-live-p chat-buffer)))
    (and (not (plist-get info :niwa-turn-settled))
         (with-current-buffer chat-buffer
           (null niwa--current-request)))))

(defun niwa--turn-record-lost-settlement (fsm)
  "Record a lost-turn settlement from FSM\='s own request identity.
The request is gone, so unlike `niwa--turn-record-settlement' the
identity comes from the info plist and no duration or token facts are
available."
  (when-let* ((info (niwa-engine-info fsm))
              (chat-buffer (plist-get info :buffer))
              ((buffer-live-p chat-buffer)))
    (with-current-buffer chat-buffer
      (niwa--turn-publish-outcome info 'lost "Turn ownership was lost")
      (when (bound-and-true-p niwa--session)
        (niwa-recovery-report niwa--session "request" 'request
                                 "Turn ownership was lost; review the transcript before continuing" nil))
      (when (bound-and-true-p niwa--session)
        (niwa-telemetry-record
         niwa--session 'request-settled
         :request-id (plist-get info :niwa-request-id)
         :origin "/root"
         :outcome 'lost
         :provider-status (plist-get info :status))))))

(defun niwa--turn-settle-lost (fsm)
  "Run the degraded settlement for FSM\='s lost turn.

The turn's request disappeared without settling, so the full
transaction cannot run: there is no reservation to commit and no
request to end.  What still can run does: the settlement telemetry the
lost teardown never recorded, the autosave that publishes the
transcript, the StopFailure hook, and
idling the root roster that only `niwa-request-end' would otherwise
touch.  The buffer remains busy until this deferred chain finishes."
  (niwa--turn-stamp-settled fsm)
  (niwa--defer-turn-steps
   fsm
   (list #'niwa--turn-record-lost-settlement
         #'niwa--turn-fail-pending-input
         #'niwa--turn-save
         #'niwa--turn-checkpoint
         (lambda (machine)
           (niwa--run-turn-terminal-hook machine 'StopFailure 'lost))
         (lambda (machine)
           (when-let* ((buffer (niwa--turn-buffer machine)))
             (with-current-buffer buffer
               (when (bound-and-true-p niwa--session)
                 (setf (niwa-session-agent-root-activity niwa--session)
                       'idle))))))
   (lambda ()
     (when-let* ((buffer (niwa--turn-buffer fsm)))
       (with-current-buffer buffer
         (when (bound-and-true-p niwa--session)
           (setf (niwa-session-agent-root-activity niwa--session)
                 'idle)))))))

(defun niwa--complete-turn (fsm)
  "Run the canonical successful top-level turn transaction for FSM.

The commit is the single-use reservation fence and runs here, synchronously:
it touches no target and nothing may settle if the reservation is wrong.  The
remaining steps publish, so they run once the transport is idle.  gptel drives
this from its process sentinel, which Emacs may dispatch from inside an
unrelated remote operation; publishing from there desynchronizes the
connection and the transaction commits against answers belonging to another
command.

The chain retains one admission hold across event-loop breaks because its order
is load-bearing: `niwa--turn-end-request\=' follows the autosave, and
inverting them drops the turn's file-history checkpoints."
  (cond
   ((niwa--turn-settled-p fsm)
    (niwa--turn-stamp-settled fsm))
   ((niwa--turn-lost-p fsm)
    (niwa--turn-settle-lost fsm))
   (t
    (niwa--turn-commit fsm)
    (niwa--turn-stamp-settled fsm)
    (niwa--defer-turn-steps
     fsm
     (list (lambda (machine)
             (niwa--turn-record-settlement machine 'success))
           #'niwa--turn-requeue-steering
           (lambda (machine)
             (niwa--turn-settle-plan-handoff machine 'success))
           #'niwa-goal-settle-turn
           #'niwa-compact-estimation-record-token-baseline
           #'niwa--turn-save
           #'niwa--turn-checkpoint
           (lambda (machine)
             (niwa--run-turn-terminal-hook machine 'Stop 'completed))
           #'niwa--turn-end-request
           (lambda (machine)
             (niwa--turn-after-publication
              (lambda (m) (niwa-goal-dispatch-after-turn m t)) machine))
           (lambda (machine)
             (setf (niwa-engine-info machine)
                   (plist-put (niwa-engine-info machine)
                              :niwa-input-delivery-after-release t))
             (niwa--turn-after-publication
              #'niwa-view--schedule-follow-up-drain machine))
           (lambda (machine)
             (niwa--turn-after-publication
              #'niwa-pending-inputs-offer-event-after-turn machine)))))))

(defun niwa--turn-clear-pending-policy (fsm)
  "Clear only the unadmitted submission policy captured in FSM's info.
An older terminal callback must not clear a replacement submission."
  (when-let* ((info (niwa-engine-info fsm))
              ((not (plist-get info :niwa-request-id)))
              (buffer (plist-get info :buffer))
              ((buffer-live-p buffer)))
    (with-current-buffer buffer
      (unless niwa--current-request
        (when (and (plist-member info :niwa-pending-implementation-policy)
                   (eq niwa--pending-implementation-policy
                       (plist-get info :niwa-pending-implementation-policy)))
          (setq niwa--pending-implementation-policy nil))
        (when (and (plist-member info :niwa-pending-permission-mode)
                   (eq niwa--pending-permission-mode
                       (plist-get info :niwa-pending-permission-mode)))
          (setq niwa--pending-permission-mode nil))))))

(defvar niwa-workspace-schedule-run)
(declare-function niwa-workspace-schedule--turn-settled "niwa-workspace-schedule" (outcome))

(defun niwa--turn-record-rejected-schedule (fsm info status)
  "Record FSM's unadmitted STATUS using only the scheduled run captured in INFO."
  (when-let* ((context (plist-get info :context))
              ((listp context))
              (buffer (plist-get info :buffer))
              ((buffer-live-p buffer)))
    (with-current-buffer buffer
      (when (bound-and-true-p niwa--session)
        (let ((niwa-turn-settlement-reason (and (eq status 'error) (niwa--fsm-error-message fsm))))
          (cond
           ((plist-get context :wake-runs)
            ;; A newer turn may already have consumed some of these records.
            (setf (plist-get context :wake-runs)
                  (cl-intersection (plist-get context :wake-runs)
                                   (niwa-agent-control-context-mailbox niwa--session)
                                   :test #'equal :key (lambda (r) (plist-get r :id))))
            (when (plist-get context :wake-runs)
              (niwa-schedule-settle-wake niwa--session buffer context status niwa-turn-settlement-reason)))
           ((and (bound-and-true-p niwa-workspace-schedule-run)
                 (eq niwa-workspace-schedule-run (plist-get context :workspace-schedule-run)))
            (niwa-workspace-schedule--turn-settled status))))))))

(defun niwa--fail-turn (fsm status)
  "Run failure cleanup for FSM with terminal STATUS.

Deferred for the same reason as `niwa--complete-turn\=': the failure chain
also autosaves, and it reaches here from the same process sentinel."
  (niwa--turn-clear-pending-policy fsm)
  (cond
   ((niwa--turn-settled-p fsm)
    (let* ((info (niwa-engine-info fsm))
           (rejected (and (memq status '(error aborted)) (not (plist-get info :niwa-request-id))
                          (not (plist-get info :niwa-turn-settled)))))
      (niwa--turn-stamp-settled fsm)
      (when rejected (niwa--turn-record-rejected-schedule fsm info status))))
   ((niwa--turn-lost-p fsm)
    (niwa--turn-settle-lost fsm))
   (t
    (niwa--turn-commit fsm)
    (niwa--turn-stamp-settled fsm)
    (let ((admission-cleanup
           (list #'niwa--turn-end-request)))
      (niwa--defer-turn-steps
       fsm
       (append
        (list (lambda (machine)
                (niwa--turn-record-settlement machine status))
              (lambda (machine)
                (niwa--turn-settle-plan-handoff machine status))
              #'niwa-compact-estimation-record-token-baseline
              (lambda (machine)
                (niwa-goal-settle-failure machine status)))
        (and (eq status 'error)
             (list #'niwa--turn-record-request-failure))
        (list (lambda (machine)
                (niwa--turn-fail-pending-input machine status))
              #'niwa--turn-save #'niwa--turn-checkpoint)
        (list (lambda (machine)
                (niwa--run-turn-terminal-hook
                 machine 'StopFailure status)))
        admission-cleanup
        (list (lambda (machine)
                (niwa--turn-after-publication
                 #'niwa-goal-persist-failure machine))
              (lambda (machine)
                (niwa--turn-after-publication
                 #'niwa-goal-dispatch-after-turn machine))))
       (lambda ()
         (niwa--run-turn-steps
          fsm (cons (lambda (machine)
                      (niwa--turn-fail-pending-input machine status))
                    admission-cleanup))))))))


(defun niwa--handler-name (handler)
  "Return a compact display name for FSM HANDLER."
  (cond
   ((symbolp handler) (symbol-name handler))
   ((byte-code-function-p handler) "#<byte-code>")
   ((functionp handler) "#<function>")
   (t (format "%S" handler))))

(defun niwa--safe-fsm-handler (handler)
  "Return a wrapper to run FSM HANDLER without aborting sibling handlers."
  (lambda (fsm)
    (condition-case err
        (funcall handler fsm)
      (error
       (let* ((info (niwa-engine-info fsm))
              (request (plist-get info :niwa-request)))
         (niwa--warn-once
          (niwa-diagnostic-scope
           (if request (niwa-request-session request) (plist-get info :buffer)))
          (list 'turn-fsm-handler (niwa--handler-name handler))
          "%s (FSM handler %s failed)"
          (error-message-string err)
          (niwa--handler-name handler)))
       nil))))


(provide 'niwa-turn)
;;; niwa-turn.el ends here
