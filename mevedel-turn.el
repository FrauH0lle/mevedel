;;; mevedel-turn.el -- Canonical turn settlement -*- lexical-binding: t -*-

;;; Commentary:

;; Owns request admission, identity, cancellation, and the terminal transaction
;; shared by normal gptel turns and direct skill forks.  Every settlement step
;; is isolated so one failure cannot skip later cleanup, persistence, hook
;; delivery, or queued-message drainage.

;;; Code:

(require 'cl-lib)
(require 'mevedel-structs)

;; `gptel'
(declare-function gptel-backend-name "ext:gptel" (cl-x) t)
(defvar gptel-backend)

;; `gptel-request'
(declare-function gptel-fsm-info "ext:gptel-request" (cl-x) t)

;; `mevedel-agents'
(declare-function mevedel-agent-invocation-p "mevedel-agents" (cl-x))
(declare-function mevedel-agent-invocation-plan-read-only
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-require-path
                  "mevedel-agents" (invocation))
(defvar mevedel--agent-invocation)

;; `mevedel-chat'
(declare-function mevedel--implementation-permission-mode-restore
                  "mevedel-permission-mode" ())

;; `mevedel-compact-estimation'
(declare-function mevedel-compact-estimation-record-token-baseline
                  "mevedel-compact-estimation" (fsm))

;; `mevedel-execution-target'
(declare-function mevedel-execution-target-probe
                  "mevedel-execution-target"
                  (target &optional refresh sandbox-mode))
(declare-function mevedel-execution-target-readiness-message
                  "mevedel-execution-target" (target))
(declare-function mevedel-execution-target-ready-p
                  "mevedel-execution-target" (target))
(declare-function mevedel-execution-target-refresh-incarnation
                  "mevedel-execution-target" (target))
(declare-function mevedel-execution-target-remote-p
                  "mevedel-execution-target" (target))

;; `mevedel-goal'
(declare-function mevedel-goal-dispatch-after-turn "mevedel-goal" (fsm))
(declare-function mevedel-goal-persist-failure "mevedel-goal" (fsm))
(declare-function mevedel-goal-settle-failure
                  "mevedel-goal" (fsm &optional status))
(declare-function mevedel-goal-settle-turn "mevedel-goal" (fsm))

;; `mevedel-hooks'
(declare-function mevedel-hooks-event-plist
                  "mevedel-hooks"
                  (event &optional session workspace &rest extra))
(declare-function mevedel-hooks-run-event
                  "mevedel-hooks"
                  (event event-plist callback
                         &optional session workspace request invocation))

;; `mevedel-pending-inputs'
(declare-function mevedel-view--schedule-follow-up-drain
                  "mevedel-pending-inputs" (fsm))

;; `mevedel-permission-queue'
(declare-function mevedel-permission-queue-sweep-request
                  "mevedel-permission-queue"
                  (request-id &optional session no-render))

;; `mevedel-plan-handoff'
(declare-function mevedel-plan-handoff-settle-request
                  "mevedel-plan-handoff"
                  (fsm status &optional reason))

;; `mevedel-plan-mode'
(declare-function mevedel-plan-approval-abort
                  "mevedel-plan-mode" (&optional session outcome))

;; `mevedel-reminders'
(declare-function mevedel-reminders-restore-reserved-context
                  "mevedel-reminders" (buffer))

;; `mevedel-session-artifacts'
(declare-function mevedel-session-artifacts-assert-mutation-authority
                  "mevedel-session-artifacts" (session &optional buffer))
(declare-function mevedel-session-artifacts-save
                  "mevedel-session-artifacts"
                  (session buffer &optional settled force))

;; `mevedel-session-persistence'
(defvar mevedel-session--read-only-mode)
(defvar mevedel-session--save-failed)

;; `mevedel-structs'
(declare-function mevedel-request-id "mevedel-structs" (cl-x))
(declare-function mevedel-request-origin "mevedel-structs" (cl-x))
(declare-function mevedel-request-started-at "mevedel-structs" (cl-x))
(declare-function mevedel-session-pending-publication
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-pending-steering
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-set-pending-input-failure-paused
                  "mevedel-structs" (session paused))
(declare-function mevedel-session-set-pending-inputs
                  "mevedel-structs" (session category entries))
(defvar mevedel--session)
(defvar mevedel--view-buffer)

;; `mevedel-telemetry'
(declare-function mevedel-telemetry-record
                  "mevedel-telemetry" (session event &rest props))

;; `mevedel-transport'
(declare-function mevedel-transport-run-when-idle
                  "mevedel-transport" (key path thunk))

;; `mevedel-view'
(declare-function mevedel-view-rerender "mevedel-view" (&optional buffer))

;; `mevedel-view-interaction'
(declare-function mevedel-view--interaction-rebuild
                  "mevedel-view-interaction" ())

;; `mevedel-view-render'
(declare-function mevedel-view--append-request-summary
                  "mevedel-view-render"
                  (data-buf search-start &optional extra))

;; `mevedel-workspace'
(declare-function mevedel-workspace "mevedel-workspace" (&optional buffer))


;;
;;; Request lifecycle

(defun mevedel-request-active-p (&optional buffer)
  "Return non-nil when BUFFER has an active request."
  (let ((buffer (or buffer (current-buffer))))
    (and (buffer-live-p buffer)
         (with-current-buffer buffer
           (bound-and-true-p mevedel--current-request)))))

(defun mevedel-request-state-label (&optional buffer)
  "Return BUFFER's compact request state label."
  (if (mevedel-request-active-p buffer) "running" "idle"))


;;
;;; Request cancellers

(defun mevedel-request-push-canceller (request canceller)
  "Append CANCELLER (a zero-arg thunk) onto REQUEST's `cancellers' list.

Each canceller is invoked exactly once during teardown via the
drain-then-invoke helper.  Primitives that own pending overlays
register a thunk that drains their own overlays with the
`aborted' sentinel."
  (when request
    (setf (mevedel-request-cancellers request)
          (append (mevedel-request-cancellers request)
                  (list canceller)))))

(defun mevedel-request-drain-cancellers (request)
  "Atomically clear and invoke every canceller on REQUEST.

Drains the list before invoking, so a canceller that registers a new
canceller during its run does not re-enter the current drain.  Each
canceller runs inside `ignore-errors' so a misbehaving thunk cannot
strand the others.

Used by `mevedel-abort', `mevedel-request-end', and the stale-request
replacement path in `mevedel-request-begin'.  Together these are the
only call sites that may invoke cancellers."
  (when request
    (let ((cancellers (mevedel-request-cancellers request)))
      (setf (mevedel-request-cancellers request) nil)
      (dolist (canceller cancellers)
        (ignore-errors (funcall canceller))))))


;;
;;; Request identity and admission

(defun mevedel-current-origin ()
  "Return the canonical owner for the current execution context."
  (or (and (bound-and-true-p mevedel--current-request)
           (mevedel-request-p mevedel--current-request)
           (mevedel-request-origin mevedel--current-request))
      (and-let* ((inv (bound-and-true-p mevedel--agent-invocation))
                 ((fboundp 'mevedel-agent-invocation-p))
                 ((mevedel-agent-invocation-p inv)))
        (mevedel-agent-invocation-require-path inv))
      "/root"))

(defun mevedel-current-turn (session)
  "Return SESSION's active reserved turn or next turn."
  (if (and (mevedel-request-p mevedel--current-request)
           (eq session (mevedel-request-session mevedel--current-request)))
      (mevedel-request-turn mevedel--current-request)
    (1+ (or (mevedel-session-turn-count session) 0))))

(defun mevedel-request-note-untracked-effect (request source reason)
  "Record one untracked filesystem effect SOURCE and REASON on REQUEST."
  (unless (assoc source (mevedel-request-untracked-effects request))
    (push (cons source reason)
          (mevedel-request-untracked-effects request)))
  (mevedel-request-untracked-effects request))

(defun mevedel-request-assert-target-ready (session)
  "Signal a user error when SESSION's execution target is not ready."
  (when-let* ((session)
              (target (mevedel-session-execution-target session)))
    (require 'mevedel-execution-target)
    (when (mevedel-execution-target-remote-p target)
      (mevedel-execution-target-probe
       target nil (mevedel-session-sandbox-mode session))
      (unless (mevedel-execution-target-ready-p target)
        (user-error "Execution target is not ready: %s"
                    (mevedel-execution-target-readiness-message target))))
    (unless (mevedel-execution-target-remote-p target)
      (mevedel-execution-target-refresh-incarnation target)))
  t)

(defun mevedel-request-begin (session &optional directive-uuid)
  "Create a new request for SESSION, guarding against stale requests.

If `mevedel--current-request' is already set, log a warning and replace
it.  Optional DIRECTIVE-UUID sets the directive being processed.  Returns
the new request struct."
  (require 'mevedel-session-persistence)
  (require 'mevedel-session-codec)
  (require 'mevedel-session-artifacts)
  (mevedel-request-assert-target-ready session)
  (mevedel-session-artifacts-assert-mutation-authority
   session (current-buffer))
  (when mevedel--current-request
    (message "mevedel: stale request found, replacing")
    (mevedel-request-end t))
  (let* ((origin (mevedel-current-origin))
         (id (format "request-%s-%s"
                     (format-time-string "%Y%m%dT%H%M%S")
                     (substring
                      (secure-hash
                       'sha1
                       (format "%s:%s:%s" (emacs-pid) (float-time) origin))
                      0 12)))
         (request (mevedel-request--create
                   :id id
                   :session session
                   :turn (1+ (or (mevedel-session-turn-count session) 0))
                   :file-snapshots (make-hash-table :test #'equal)
                   :untracked-effects nil
                   :directive-uuid directive-uuid
                   :plan-read-only
                   (or (eq (plist-get
                            (mevedel-session-directive-planning session)
                            :phase)
                           'planning)
                       (and (boundp 'mevedel--agent-invocation)
                            mevedel--agent-invocation
                            (mevedel-agent-invocation-plan-read-only
                             mevedel--agent-invocation)))
                   :started-at (current-time)
                   :origin origin)))
    (setq mevedel--current-request request)
    (when (equal origin "/root")
      (setf (mevedel-session-agent-root-activity session) 'running))
    (when (fboundp 'mevedel-telemetry-record)
      (mevedel-telemetry-record
       session 'request-queued :request-id id :origin origin
       :permission-mode (mevedel-session-permission-mode session)
       :sandbox-mode (mevedel-session-sandbox-mode session))
      (mevedel-telemetry-record
       session 'request-start :request-id id :origin origin
       :permission-mode (mevedel-session-permission-mode session)
       :sandbox-mode (mevedel-session-sandbox-mode session)))
    request))

(defun mevedel-request-cancel (request &optional abort-plan-approval)
  "Cancel REQUEST and its owned pending interactions.
Queued permission prompts are swept only for REQUEST's identity.  Plan
approvals normally outlive the request that presented them; when
ABORT-PLAN-APPROVAL is non-nil, abort it too."
  (when request
    (let ((session (mevedel-request-session request))
          (request-id (mevedel-request-id request)))
      (mevedel-request-drain-cancellers request)
      (when (and request-id
                 (fboundp 'mevedel-permission-queue-sweep-request))
        (mevedel-permission-queue-sweep-request request-id session))
      (when (and abort-plan-approval
                 (fboundp 'mevedel-plan-approval-abort))
        (mevedel-plan-approval-abort session)))))

(defun mevedel-request-end (&optional abort-plan-approval)
  "Cancel the current request, then clear `mevedel--current-request'.

Also returns any hook context the ended request reserved.  Reminder
injection runs ahead of the handler that begins a request, so a delivered
payload has already released its reservation and only an undelivered one
is returned here."
  (require 'mevedel-reminders)
  (mevedel-reminders-restore-reserved-context (current-buffer))
  (when mevedel--current-request
    (let ((request mevedel--current-request))
      (when (fboundp 'mevedel-telemetry-record)
        (mevedel-telemetry-record
         (mevedel-request-session request) 'request-teardown
         :request-id (mevedel-request-id request)
         :origin (mevedel-request-origin request)
         :abort-plan-approval (and abort-plan-approval t)))
      (mevedel-request-cancel request abort-plan-approval)
      (when (equal (mevedel-request-origin request) "/root")
        (setf (mevedel-session-agent-root-activity
               (mevedel-request-session request))
              'idle)))
    (setq mevedel--current-request nil)))


;;
;;; Terminal transaction

(defun mevedel--turn-record-settlement (fsm outcome)
  "Record terminal OUTCOME for FSM's active request."
  (when-let* ((info (gptel-fsm-info fsm))
              (chat-buffer (plist-get info :buffer))
              ((buffer-live-p chat-buffer)))
    (with-current-buffer chat-buffer
      (when (and (bound-and-true-p mevedel--session)
                 (bound-and-true-p mevedel--current-request)
                 (fboundp 'mevedel-telemetry-record))
        (let* ((request mevedel--current-request)
               (tokens (or (plist-get info :tokens-full)
                           (plist-get info :tokens)))
               (started-at (mevedel-request-started-at request)))
          (mevedel-telemetry-record
           mevedel--session 'request-settled
           :request-id (mevedel-request-id request)
           :origin (mevedel-request-origin request)
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

(defun mevedel--turn-settle-plan-handoff (fsm status)
  "Settle any Direct Plan handoff attached to FSM with STATUS."
  (when (fboundp 'mevedel-plan-handoff-settle-request)
    (mevedel-plan-handoff-settle-request
     fsm status
     (and (not (eq status 'success))
          (or (mevedel--fsm-error-message fsm)
              (if (eq status 'aborted)
                  "Request aborted"
                "Provider request failed"))))))

(defun mevedel--fsm-error-message (fsm)
  "Return a compact error message for FSM, or nil."
  (let* ((info (and fsm (gptel-fsm-info fsm)))
         (error (plist-get info :error))
         (status (plist-get info :status))
         (error-type (and (listp error) (plist-get error :type)))
         (error-message (and (listp error) (plist-get error :message))))
    (or (and (stringp error) error)
        error-message
        (and error-type status (format "%s: %s" error-type status))
        (and error-type (format "%s" error-type))
        (and status (format "%s" status)))))

(defun mevedel--turn-record-request-failure (fsm)
  "Add FSM's provider failure to its ignored request summary."
  (when-let* ((info (gptel-fsm-info fsm))
              (chat-buffer (plist-get info :buffer))
              ((buffer-live-p chat-buffer)))
    (with-current-buffer chat-buffer
      (let* ((error-data (plist-get info :error))
             (backend (or (plist-get info :backend) gptel-backend))
             (backend-name
              (and backend
                   (condition-case nil
                       (gptel-backend-name backend)
                     (error nil)))))
        (require 'mevedel-view-render)
        (mevedel-view--append-request-summary
         chat-buffer
         (plist-get info :position)
         (list :outcome 'error
               :backend (or backend-name "Provider")
               :status (plist-get info :status)
               :error-type (and (listp error-data)
                                (plist-get error-data :type))
               :error-code (and (listp error-data)
                                (plist-get error-data :code))
               :error-data error-data
               :message (mevedel--fsm-error-message fsm)
               :retry 'manual))))))

(defun mevedel--run-turn-terminal-hook (fsm event status)
  "Run top-level turn terminal hook EVENT for FSM with STATUS."
  (when-let* ((info (gptel-fsm-info fsm))
              (chat-buffer (plist-get info :buffer))
              ((buffer-live-p chat-buffer)))
    (with-current-buffer chat-buffer
      (when (bound-and-true-p mevedel--session)
        (require 'mevedel-hooks)
        (let* ((workspace (mevedel-workspace))
               (reason (and (eq event 'StopFailure)
                            (or (mevedel--fsm-error-message fsm)
                                (symbol-name status)))))
          ;; Deliberately no request: the turn's own teardown drains the
          ;; request's cancellers two settlement steps later, which would
          ;; kill this hook's process before it can settle.  A terminal
          ;; hook outlives its request and is bounded by its own timeout.
          (mevedel-hooks-run-event
           event
           (mevedel-hooks-event-plist
            event mevedel--session workspace
            :status (symbol-name status)
            :terminal-reason reason)
           #'ignore
           mevedel--session workspace nil nil))))))


(defun mevedel--turn-commit (fsm)
  "Commit FSM's request-reserved turn to its live session.
Signal when the request is missing or its reservation is not the next turn."
  (when-let* ((info (gptel-fsm-info fsm))
              (chat-buffer (plist-get info :buffer))
              ((buffer-live-p chat-buffer)))
    (with-current-buffer chat-buffer
      (unless (and mevedel--session mevedel--current-request)
        (error "Cannot commit turn without an active request"))
      (let ((reserved (mevedel-request-turn mevedel--current-request))
            (expected (1+ (or (mevedel-session-turn-count mevedel--session)
                              0))))
        (unless (equal reserved expected)
          (error "Reserved turn %S does not follow committed turn %S"
                 reserved (mevedel-session-turn-count mevedel--session)))
        (setf (mevedel-session-turn-count mevedel--session) reserved)))))

(defun mevedel--turn-autosave (fsm)
  "Persist the completed turn represented by FSM and refresh its view."
  (require 'mevedel-session-persistence)
  (require 'mevedel-session-codec)
  (require 'mevedel-session-artifacts)
  (when-let* ((info (gptel-fsm-info fsm))
              (chat-buffer (plist-get info :buffer))
              ((buffer-live-p chat-buffer)))
    (with-current-buffer chat-buffer
      (when (and mevedel--session
                 (not (bound-and-true-p mevedel-session--read-only-mode)))
        (let (saved)
          (condition-case err
              (progn
                (mevedel-session-artifacts-save
                 mevedel--session chat-buffer t)
                (when (bound-and-true-p mevedel-session--save-failed)
                  (setq mevedel-session--save-failed nil)
                  (force-mode-line-update))
                (setq saved t))
            (error
             (display-warning 'mevedel
                              (format "Session auto-save failed: %s" err)
                              :warning)
             (setq-local mevedel-session--save-failed t)
             (force-mode-line-update)))
          (when (and saved (buffer-live-p mevedel--view-buffer))
            (require 'mevedel-view)
            (mevedel-view-rerender mevedel--view-buffer)))))))

(defun mevedel--turn-restore-permission-mode (fsm)
  "Restore any temporary permission mode for FSM's request buffer."
  (when-let* ((info (gptel-fsm-info fsm))
              (chat-buffer (plist-get info :buffer))
              ((buffer-live-p chat-buffer)))
    (with-current-buffer chat-buffer
      (mevedel--implementation-permission-mode-restore))))

(defun mevedel--turn-end-request (fsm)
  "End the active mevedel request for FSM's request buffer."
  (when-let* ((info (gptel-fsm-info fsm))
              (chat-buffer (plist-get info :buffer))
              ((buffer-live-p chat-buffer)))
    (with-current-buffer chat-buffer
      (mevedel-request-end))))

(defun mevedel--turn-fail-pending-input (fsm)
  "Mark undelivered steering for FSM's dead turn as requiring review."
  (when-let* ((info (gptel-fsm-info fsm))
              (chat-buffer (plist-get info :buffer))
              ((buffer-live-p chat-buffer)))
    (with-current-buffer chat-buffer
      (when (and mevedel--session mevedel--current-request)
        (let* ((request-id (mevedel-request-id mevedel--current-request))
               (entries (mevedel-session-pending-steering mevedel--session))
               (failed nil)
               (updated
                (mapcar
                 (lambda (entry)
                   (if (equal request-id (plist-get entry :request-id))
                       (progn
                         (setq failed t)
                         (plist-put (copy-sequence entry)
                                    :state 'failed-turn))
                     entry))
                 entries)))
          (when failed
            (mevedel-session-set-pending-inputs
             mevedel--session 'steering updated)
            (mevedel-session-set-pending-input-failure-paused
             mevedel--session t)
            (when (buffer-live-p mevedel--view-buffer)
              (with-current-buffer mevedel--view-buffer
                (mevedel-view--interaction-rebuild)))))))))

(defun mevedel--run-turn-steps (fsm steps)
  "Run FSM through STEPS without allowing one failure to skip the rest."
  (dolist (step steps)
    (funcall (mevedel--safe-fsm-handler step) fsm)))

(defun mevedel--turn-buffer (fsm)
  "Return FSM's live chat buffer, or nil."
  (when-let* ((info (condition-case nil (gptel-fsm-info fsm) (error nil)))
              (buffer (plist-get info :buffer))
              ((buffer-live-p buffer)))
    buffer))

(defun mevedel--defer-turn-steps (fsm steps)
  "Run FSM through STEPS once no remote operation is in flight.

Each step re-derives its own buffer from FSM, so waiting costs the chain no
context.  Settlement keeps the request open until the chain finishes, so a
Goal continuation or a user send still observes the workflow as busy while
this waits."
  (require 'mevedel-transport)
  (let ((buffer (mevedel--turn-buffer fsm)))
    (mevedel-transport-run-when-idle
     (list 'turn-settlement (or buffer fsm))
     (and buffer (buffer-local-value 'default-directory buffer))
     (lambda () (mevedel--run-turn-steps fsm steps)))))

(defun mevedel--turn-publication-pending-p (fsm)
  "Return non-nil when FSM's session has failed critical publication."
  (when-let* ((info (condition-case nil
                        (gptel-fsm-info fsm)
                      (error nil)))
              (buffer (plist-get info :buffer))
              ((buffer-live-p buffer)))
    (with-current-buffer buffer
      (and mevedel--session
           (mevedel-session-pending-publication mevedel--session)))))

(defun mevedel--turn-after-publication (function fsm)
  "Call FUNCTION with FSM unless critical publication is pending."
  (unless (mevedel--turn-publication-pending-p fsm)
    (funcall function fsm)))

(defun mevedel--complete-turn (fsm)
  "Run the canonical successful top-level turn transaction for FSM.

The commit is the single-use reservation fence and runs here, synchronously:
it touches no target and nothing may settle if the reservation is wrong.  The
remaining steps publish, so they run once the transport is idle.  gptel drives
this from its process sentinel, which Emacs may dispatch from inside an
unrelated remote operation; publishing from there desynchronizes the
connection and the transaction commits against answers belonging to another
command.

The whole chain defers as one unit rather than step by step, because its order
is load-bearing: `mevedel--turn-end-request\=' follows the autosave, and
inverting them drops the turn's file-history checkpoints."
  (require 'mevedel-compact-estimation)
  (mevedel--turn-commit fsm)
  (mevedel--defer-turn-steps
   fsm
   (list (lambda (machine)
           (mevedel--turn-record-settlement machine 'success))
         (lambda (machine)
           (mevedel--turn-settle-plan-handoff machine 'success))
         #'mevedel-goal-settle-turn
         #'mevedel-compact-estimation-record-token-baseline
         #'mevedel--turn-autosave
         (lambda (machine)
           (mevedel--run-turn-terminal-hook machine 'Stop 'completed))
         #'mevedel--turn-restore-permission-mode
         #'mevedel--turn-end-request
         (lambda (machine)
           (mevedel--turn-after-publication
            #'mevedel-goal-dispatch-after-turn machine))
         (lambda (machine)
           (require 'mevedel-pending-inputs)
           (mevedel--turn-after-publication
            #'mevedel-view--schedule-follow-up-drain machine)))))

(defun mevedel--fail-turn (fsm status)
  "Run failure cleanup for FSM with terminal STATUS.

Deferred for the same reason as `mevedel--complete-turn\=': the failure chain
also autosaves, and it reaches here from the same process sentinel."
  (require 'mevedel-compact-estimation)
  (mevedel--turn-commit fsm)
  (mevedel--defer-turn-steps
   fsm
   (append
    (list (lambda (machine)
            (mevedel--turn-record-settlement machine status))
          (lambda (machine)
            (mevedel--turn-settle-plan-handoff machine status))
          #'mevedel-compact-estimation-record-token-baseline
          (lambda (machine)
            (mevedel-goal-settle-failure machine status)))
    (and (eq status 'error)
         (list #'mevedel--turn-record-request-failure
               #'mevedel--turn-autosave))
    (list (lambda (machine)
            (mevedel--run-turn-terminal-hook
             machine 'StopFailure status))
          #'mevedel--turn-restore-permission-mode
          #'mevedel--turn-fail-pending-input
          #'mevedel--turn-end-request
          (lambda (machine)
            (mevedel--turn-after-publication
             #'mevedel-goal-persist-failure machine))
          (lambda (machine)
            (mevedel--turn-after-publication
             #'mevedel-goal-dispatch-after-turn machine))))))


(defun mevedel--handler-name (handler)
  "Return a compact display name for FSM HANDLER."
  (cond
   ((symbolp handler) (symbol-name handler))
   ((byte-code-function-p handler) "#<byte-code>")
   ((functionp handler) "#<function>")
   (t (format "%S" handler))))

(defun mevedel--safe-fsm-handler (handler)
  "Return a wrapper to run FSM HANDLER without aborting sibling handlers."
  (lambda (fsm)
    (condition-case err
        (funcall handler fsm)
      (error
       (display-warning
        'mevedel
        (format "FSM handler %s failed: %s"
                (mevedel--handler-name handler)
                (error-message-string err))
        :warning)
       nil))))


(provide 'mevedel-turn)
;;; mevedel-turn.el ends here
