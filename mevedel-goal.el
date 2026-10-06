;;; mevedel-goal.el -- Durable Goal continuation controller -*- lexical-binding: t -*-

;;; Commentary:

;; Owns the phase-free Goal record, request-local context, root-turn
;; attribution, and deterministic idle continuation.  Turn settlement and Goal
;; controls schedule continuation; the pending-input owner re-offers it when an
;; interaction holding an idle session closes.  Planning and review are
;; ordinary conversation work; only UpdateGoal may mark an active Goal
;; blocked, or complete once an independent verifier accepts the claim.

;;; Code:

(require 'cl-lib)
(eval-when-compile
  ;; The FSM slot setter needs its struct expander during compilation.
  (require 'gptel-request)
  (require 'mevedel-structs)
  (require 'subr-x))

;; `gptel-request'
(declare-function gptel-fsm-info "ext:gptel-request" (cl-x) t)

;; `mevedel-agent-control'
(declare-function mevedel-agent-control-steer-user
                  "mevedel-agent-control"
                  (session message &optional before-wake metadata))
(autoload 'mevedel-agent-control-steer-user "mevedel-agent-control")

;; `mevedel-agents'
(declare-function mevedel-agent-invocation-goal-fsm "mevedel-agents" (cl-x) t)

;; `mevedel-chat'
(declare-function mevedel--submit-generated-turn
                  "mevedel-chat" (prompt &optional display-text
                                         prompt-submission))

;; `mevedel-pending-inputs'
(declare-function mevedel-pending-inputs-follow-up-changed
                  "mevedel-pending-inputs" (session))
(declare-function mevedel-view--run-follow-up-drain
                  "mevedel-pending-inputs" (data-buffer))
(autoload 'mevedel-pending-inputs-follow-up-changed
  "mevedel-pending-inputs")
(autoload 'mevedel-view--run-follow-up-drain "mevedel-pending-inputs")

;; `mevedel-plan'
(declare-function mevedel-plan-artifact-path-p "mevedel-plan" (path))
(declare-function mevedel-plan-read-artifact "mevedel-plan"
                  (session artifact))
(declare-function mevedel-plan-resource-address "mevedel-plan"
                  (relative-path))
(autoload 'mevedel-plan-artifact-path-p "mevedel-plan")
(autoload 'mevedel-plan-read-artifact "mevedel-plan")
(autoload 'mevedel-plan-resource-address "mevedel-plan")

;; `mevedel-session-artifacts'
(declare-function mevedel-session-artifacts-artifact-present-p
                  "mevedel-session-artifacts"
                  (session logical &optional committed-only))
(declare-function mevedel-session-artifacts-assert-mutation-authority
                  "mevedel-session-artifacts" (session &optional buffer))
(declare-function mevedel-session-artifacts-save
                  "mevedel-session-artifacts"
                  (session buffer &optional settled force))
(autoload 'mevedel-session-artifacts-artifact-present-p
  "mevedel-session-artifacts")
(autoload 'mevedel-session-artifacts-assert-mutation-authority
  "mevedel-session-artifacts")
(autoload 'mevedel-session-artifacts-save "mevedel-session-artifacts")

;; `mevedel-reminders'
(declare-function mevedel-reminders-queue-turn-event
                  "mevedel-reminders" (buffer key body &optional commit))
(autoload 'mevedel-reminders-queue-turn-event "mevedel-reminders")

;; `mevedel-structs'
(declare-function mevedel-goal--create "mevedel-structs" (&rest slots))
(declare-function mevedel-request-fsm "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-enqueue-pending-input
                  "mevedel-structs" (session category entry))
(declare-function mevedel-session-enqueue-pending-reminder
                  "mevedel-structs" (session reminder))
(declare-function mevedel-session-pending-follow-ups
                  "mevedel-structs" (cl-x) t)
(defvar mevedel--current-request)
(defvar mevedel--session)
(defvar mevedel--view-buffer)

;; `mevedel-system'
(declare-function mevedel-system-render-prompt-file
                  "mevedel-system" (relative-path &optional replacements))
(autoload 'mevedel-system-render-prompt-file "mevedel-system")

;; `mevedel-telemetry'
(declare-function mevedel-telemetry-record
                  "mevedel-telemetry" (session event &rest props))

;; `mevedel-tools'
(declare-function mevedel-tools--buffer-local-agent-invocation
                  "mevedel-tools" (buffer))

;; `mevedel-transport'
(declare-function mevedel-transport-run-when-idle
                  "mevedel-transport" (key path thunk))
(autoload 'mevedel-transport-run-when-idle "mevedel-transport")

;; `mevedel-turn'
(declare-function mevedel-turn-end-at-boundary "mevedel-turn" (fsm reason))
(defvar mevedel--agent-invocation)

;; `mevedel-view-interaction'
(declare-function mevedel-view-interaction-pending-p
                  "mevedel-view-interaction" (&optional view-buffer))


;;
;;; Settings

(defcustom mevedel-goal-token-budget nil
  "Default token budget for new Goals, or nil for no limit."
  :type '(choice (const :tag "Unlimited" nil)
                 (integer :tag "Token budget"))
  :group 'mevedel)

(make-variable-buffer-local 'mevedel-goal-token-budget)

(defconst mevedel-goal--continuation-trigger
  "Continue working toward the active Goal."
  "Ordinary user-role trigger used for initial and automatic Goal turns.")

(defconst mevedel-goal--budget-thresholds '(50 80 100)
  "Percentage crossings that produce Goal budget reminders.")

(defconst mevedel-goal--transient-retry-delays '(15 30 60 120 240)
  "Seconds before each consecutive retry of a transient Goal failure.
Their sum outlasts a short network outage such as a dropped Wi-Fi link.")

(defvar-local mevedel-goal--transient-retries 0
  "Transient Goal failures retried since the last successful turn.")


;;
;;; Durable record

(defun mevedel-goal--assert-mutation-authority (session &optional buffer)
  "Require SESSION mutation authority before changing Goal state.
BUFFER defaults to the current buffer."
  (mevedel-session-artifacts-assert-mutation-authority
   session (or buffer (current-buffer))))

(defun mevedel-goal-new-id ()
  "Return a fresh versioned Goal identifier."
  (format "%s-%06x" (format-time-string "%Y%m%d-%H%M%S")
          (random #x1000000)))

(defun mevedel-goal--validate-objective (objective)
  "Return normalized non-empty OBJECTIVE or signal `user-error'."
  (unless (and (stringp objective) (not (string-blank-p objective)))
    (user-error "Goal objective must not be blank"))
  (string-trim objective))

(defun mevedel-goal--assert-token-budget ()
  "Signal an error unless `mevedel-goal-token-budget' is valid."
  (unless (or (null mevedel-goal-token-budget)
              (and (integerp mevedel-goal-token-budget)
                   (> mevedel-goal-token-budget 0)))
    (error "Goal token budget must be a positive integer or nil")))

(defun mevedel-goal--valid-plan-reference-p (reference)
  "Return non-nil when REFERENCE is a normalized relative path."
  (or (null reference)
      (mevedel-plan-artifact-path-p reference)))

(defun mevedel-goal-create (objective &optional session plan-reference id)
  "Create and persist a lifecycle-neutral Goal for OBJECTIVE.
SESSION defaults to the current session.  PLAN-REFERENCE is an optional
session-relative accepted-plan artifact.  ID may preallocate the Goal identity."
  (setq objective (mevedel-goal--validate-objective objective)
        session (or session mevedel--session))
  (unless session
    (error "No active session for Goal"))
  (unless (mevedel-goal--valid-plan-reference-p plan-reference)
    (error "Invalid accepted-plan reference"))
  (mevedel-goal--assert-token-budget)
  (mevedel-goal--assert-mutation-authority session)
  (let* ((previous (mevedel-session-goal session))
         (now (format-time-string "%FT%T%z"))
         (goal (mevedel-goal--create
                :id (or id (mevedel-goal-new-id))
                :objective objective
                :status 'active
                :token-budget mevedel-goal-token-budget
                :tokens-used 0
                :time-used-seconds 0
                :turns-run 0
                :plan-reference plan-reference
                :created-at now
                :updated-at now)))
    (setf (mevedel-session-goal session) goal)
    (condition-case err
        (mevedel-session-artifacts-save session (current-buffer))
      (error
       (setf (mevedel-session-goal session) previous)
       (signal (car err) (cdr err))))
    (setq mevedel-goal--transient-retries 0)
    (when (fboundp 'mevedel-telemetry-record)
      (mevedel-telemetry-record session 'goal-start :goal-id (mevedel-goal-id goal)))
    goal))

(defun mevedel-goal-ensure (objective session plan-reference id)
  "Return the durable Goal identified by ID and PLAN-REFERENCE in SESSION.
Create it for OBJECTIVE when absent.  A matching Goal restored as paused is
reactivated without scheduling because its caller owns the prepared kickoff."
  (mevedel-goal--assert-mutation-authority session)
  (let ((current (mevedel-session-goal session)))
    (cond
     ((and current
           (equal id (mevedel-goal-id current))
           (equal plan-reference (mevedel-goal-plan-reference current)))
      (unless (memq (mevedel-goal-status current) '(active paused))
        (error "Reserved Goal %s has terminal status %s"
               id (mevedel-goal-status current)))
      (setf (mevedel-goal-status current) 'active
            (mevedel-goal-reason current) nil)
      (mevedel-goal--touch current)
      (mevedel-goal--persist session (current-buffer))
      current)
     ((and current (not (eq (mevedel-goal-status current) 'complete)))
      (error "Target session has unfinished Goal %s; expected reserved Goal %s for %s"
             (mevedel-goal-id current) id plan-reference))
     (t
      (mevedel-goal-create objective session plan-reference id)))))

(defun mevedel-goal--current ()
  "Return the current session Goal or signal `user-error'."
  (unless (bound-and-true-p mevedel--session)
    (user-error "No mevedel session in this buffer"))
  (or (mevedel-session-goal mevedel--session)
      (user-error "No current Goal")))

(defun mevedel-goal--touch (goal)
  "Update GOAL's modification timestamp."
  (setf (mevedel-goal-updated-at goal) (format-time-string "%FT%T%z"))
  goal)

(defun mevedel-goal--persist (session buffer)
  "Persist SESSION from BUFFER when both remain live."
  (when (and session (buffer-live-p buffer))
    (with-current-buffer buffer
      (mevedel-session-artifacts-save session buffer))))


;;
;;; Accepted-plan authority and request context

(defun mevedel-goal--pause-for-integrity (goal session reason)
  "Persistently pause GOAL in SESSION for integrity REASON, then error."
  (setf (mevedel-goal-status goal) 'paused
        (mevedel-goal-reason goal) reason)
  (mevedel-goal--touch goal)
  (mevedel-goal--persist session (current-buffer))
  (error "%s" reason))

(defun mevedel-goal--resolve-plan-reference (goal session)
  "Return GOAL's validated accepted-plan resource address in SESSION, or nil.

The artifact is validated through its private session-owned path, but the
returned value is the model-facing `work://plans/...' address so Goal context
never discloses session storage paths."
  (when-let* ((reference (mevedel-goal-plan-reference goal)))
    (unless (mevedel-goal--valid-plan-reference-p reference)
      (mevedel-goal--pause-for-integrity
       goal session "Accepted-plan reference is invalid"))
    (let* ((metadata (mevedel-session-plan-metadata session))
           (accepted (plist-get metadata :accepted-path))
           (expected-hash (plist-get metadata :accepted-hash))
           (artifact (list :path reference :hash expected-hash)))
      (unless (and (equal reference accepted)
                   (stringp expected-hash))
        (mevedel-goal--pause-for-integrity
         goal session
         "Accepted-plan artifact is missing or no longer owned by this session"))
      (unless (mevedel-session-artifacts-artifact-present-p
               session reference)
        (mevedel-goal--pause-for-integrity
         goal session
         "Accepted-plan artifact is missing or no longer owned by this session"))
      (condition-case nil
          (mevedel-plan-read-artifact session artifact)
        (error
         (mevedel-goal--pause-for-integrity
          goal session
          "Accepted-plan artifact no longer matches its accepted hash")))
      (condition-case nil
          (mevedel-plan-resource-address reference)
        (error
         (mevedel-goal--pause-for-integrity
          goal session
          "Accepted-plan reference is outside managed plan storage"))))))

(defun mevedel-goal-active-context (session)
  "Render request-local active Goal context for SESSION, or nil."
  (when-let* ((goal (mevedel-session-goal session))
              ((eq (mevedel-goal-status goal) 'active)))
    ;; Live usage stays out: it moves with every charge, and each change
    ;; would re-deliver the whole block.  Crossing reminders and GetGoal
    ;; report usage instead.
    (let* ((budget (mevedel-goal-token-budget goal))
           (plan-address (mevedel-goal--resolve-plan-reference goal session)))
      (mevedel-system-render-prompt-file
       "prompts/goals/active-context.md"
       `(("objective" . ,(mevedel-goal-objective goal))
         ("token-budget" . ,(if budget
                                (format "%d tokens" budget)
                              "unbounded"))
         ("turns-run" . ,(number-to-string (mevedel-goal-turns-run goal)))
         ("plan-reference-line" .
          ,(if plan-address
               (format "Accepted plan: %s. Its outcomes, constraints, and achievement criteria are binding except where amended by the current objective; its implementation mechanics are revisable."
                       plan-address)
             "")))))))


;;
;;; Continuation and commands

(defun mevedel-goal--pending-interaction-p (session)
  "Return non-nil when SESSION has a pending user interaction."
  (or (mevedel-session-permission-queue session)
      (mevedel-session-pending-plan-approval session)
      (and (boundp 'mevedel--view-buffer)
           (buffer-live-p mevedel--view-buffer)
           (mevedel-view-interaction-pending-p mevedel--view-buffer))))

(defun mevedel-goal--budget-exhausted-p (goal)
  "Return non-nil when GOAL has reached its finite token budget."
  (when-let* ((budget (mevedel-goal-token-budget goal)))
    (>= (mevedel-goal-tokens-used goal) budget)))

(defun mevedel-goal-continue-if-idle
    (&optional session buffer prompt-submission)
  "Start SESSION's next Goal turn from BUFFER when all gates admit it.
Return `dispatched' on dispatch or the deterministic blocking gate symbol."
  (let* ((session (or session mevedel--session))
         (buffer (or buffer (current-buffer)))
         (goal (and session (mevedel-session-goal session))))
    (cond
     ((or (not (buffer-live-p buffer))
          (not (eq session (buffer-local-value 'mevedel--session buffer))))
      'unavailable)
     ((not goal) 'no-goal)
     ((not (eq (mevedel-goal-status goal) 'active)) 'inactive)
     ((buffer-local-value 'mevedel--current-request buffer) 'request)
     ((mevedel-session-pending-follow-ups session)
      (run-at-time 0 nil #'mevedel-view--run-follow-up-drain buffer)
      'follow-up)
     ((with-current-buffer buffer
        (mevedel-goal--pending-interaction-p session)) 'interaction)
     ((mevedel-goal--budget-exhausted-p goal) 'budget)
     (t
      (with-current-buffer buffer
        (mevedel--submit-generated-turn
         mevedel-goal--continuation-trigger
         mevedel-goal--continuation-trigger
         prompt-submission))
      (when (fboundp 'mevedel-telemetry-record)
        (mevedel-telemetry-record
         session 'goal-continuation :goal-id (mevedel-goal-id goal)))
      'dispatched))))

(defun mevedel-goal--scheduled-continuation (session buffer prompt-submission)
  "Run a scheduled continuation for SESSION in BUFFER.

Continuation admits a new turn, which is target I/O, and it runs from a timer.
A timer fires wherever the main loop is waiting, so it waits for an idle
transport rather than nesting inside a remote operation already in flight."
  (when (and (buffer-live-p buffer)
             (eq session (buffer-local-value 'mevedel--session buffer)))
    (mevedel-transport-run-when-idle
     (list 'goal-continuation buffer)
     (buffer-local-value 'default-directory buffer)
     (lambda ()
       (when (and (buffer-live-p buffer)
                  (eq session (buffer-local-value 'mevedel--session buffer)))
         (with-current-buffer buffer
           (condition-case err
               (mevedel-goal-continue-if-idle session buffer prompt-submission)
             (error
              (mevedel-goal-pause-runtime-failure
               buffer (format "Goal continuation failed: %s" (error-message-string err)))
              (message "mevedel: Goal continuation failed: %s" (error-message-string err))))))))))

(defun mevedel-goal-schedule-continuation
    (&optional session buffer prompt-submission delay)
  "Schedule SESSION's Goal continuation check after the current command.
DELAY is the number of seconds to wait first; nil means none."
  (let ((session (or session mevedel--session))
        (buffer (or buffer (current-buffer))))
    (when (and session (buffer-live-p buffer))
      (run-at-time (or delay 0) nil #'mevedel-goal--scheduled-continuation
                   session buffer prompt-submission))))

(defun mevedel-goal--start-blocker (session)
  "Return why a new Goal cannot start in SESSION now, or nil.
Reads the current buffer's request for request-scoped Plan mode."
  (let ((current (mevedel-session-goal session))
        (metadata (mevedel-session-plan-metadata session)))
    (cond
     ((or (mevedel-session-plan-mode session)
          (and mevedel--current-request
               (mevedel-request-plan-read-only mevedel--current-request)))
      "Leave Plan mode before starting a Goal")
     ((and current (not (eq (mevedel-goal-status current) 'complete)))
      "Finish or clear the current Goal first")
     ((or (plist-get metadata :implementation-retry)
          (plist-get metadata :implementation-goal-id))
      "Finish or cancel the accepted Plan implementation first"))))

(defun mevedel-goal-start (objective &optional prompt-submission)
  "Start a Goal for OBJECTIVE and schedule its first ordinary turn."
  (setq objective (mevedel-goal--validate-objective objective))
  (unless (bound-and-true-p mevedel--session)
    (user-error "No mevedel session in this buffer"))
  (when-let* ((blocker (mevedel-goal--start-blocker mevedel--session)))
    (user-error "%s" blocker))
  ;; Validate before settling a replaced Goal below.
  (mevedel-goal--assert-token-budget)
  (mevedel-goal--assert-mutation-authority mevedel--session)
  (let ((current (mevedel-session-goal mevedel--session)))
    (let* ((fsm (when-let* ((request mevedel--current-request)
                            ((eq mevedel--session (mevedel-request-session request)))
                            ((not (or (mevedel-request-ephemeral-p request)
                                      (mevedel-request-cancelled-p request)
                                      (mevedel-request-directive-uuid request))))
                            (machine (mevedel-request-fsm request))
                            ((eq (current-buffer)
                                 (plist-get (gptel-fsm-info machine) :buffer))))
                  machine))
           (info (and fsm (gptel-fsm-info fsm)))
           (replacing (and current
                           (equal (mevedel-goal-id current)
                                  (plist-get info :mevedel-goal-accounting-id))))
           (baseline (if replacing
                         (+ (or (mevedel-goal--known-token-count info) 0)
                            (or (plist-get info :mevedel-goal-token-baseline) 0))
                       0)))
      ;; Preserve the completed Goal's final charge before replacing it.
      ;; A cleared, unrelated Goal retains its old request attribution.
      (when replacing (mevedel-goal-settle-turn fsm))
      (let ((goal (mevedel-goal-create objective mevedel--session)))
        (when fsm
          (setq info (gptel-fsm-info fsm))
          (when replacing
            (dolist (key '(:mevedel-goal-id :mevedel-goal-accounting-id
                                            :mevedel-goal-accounted :mevedel-goal-budget-warnings))
              (cl-remf info key))
            (setq info (plist-put info :mevedel-goal-token-baseline baseline)))
          (setf (gptel-fsm-info fsm) info)
          (mevedel-goal-capture-request fsm))
        (mevedel-goal-schedule-continuation
         mevedel--session (current-buffer) prompt-submission)
        goal))))

(defun mevedel-goal--end-running-turn (goal reason message)
  "End the current buffer's root turn charged to GOAL at its tool boundary.
The turn settles normally instead of sampling the model again, so a user
control takes effect after the running tools rather than after an arbitrarily
long turn.  A turn waiting in WaitAgent reaches that boundary only when the
wait returns, so MESSAGE is also delivered as steering to wake it.  REASON
names the control for telemetry."
  (when-let* ((request mevedel--current-request)
              (fsm (mevedel-request-fsm request))
              ((equal (mevedel-goal-id goal)
                      (plist-get (gptel-fsm-info fsm)
                                 :mevedel-goal-accounting-id)))
              ((mevedel-turn-end-at-boundary fsm reason)))
    (ignore-errors
      (mevedel-agent-control-steer-user mevedel--session message))
    t))

(defun mevedel-goal-pause ()
  "Pause the current Goal, ending its running turn at the next tool boundary.
Running tools finish and the turn settles; it is not aborted."
  (interactive)
  (let ((goal (mevedel-goal--current)))
    (mevedel-goal--assert-mutation-authority mevedel--session)
    (unless (eq (mevedel-goal-status goal) 'active)
      (user-error "Goal is not active"))
    (setf (mevedel-goal-status goal) 'paused
          (mevedel-goal-reason goal) "paused by user")
    (mevedel-goal--touch goal)
    (mevedel-goal--persist mevedel--session (current-buffer))
    (mevedel-goal--end-running-turn
     goal 'goal-paused
     "The user paused the Goal. Stop here; this turn ends after the current tools.")
    goal))

(defun mevedel-goal-resume (&optional steering)
  "Resume the current Goal, optionally with ordinary STEERING text."
  (interactive)
  (let ((goal (mevedel-goal--current)))
    (mevedel-goal--assert-mutation-authority mevedel--session)
    (when (eq (mevedel-goal-status goal) 'complete)
      (user-error "Completed Goal cannot be resumed"))
    (when (or (eq (mevedel-goal-status goal) 'budget-limited)
              (mevedel-goal--budget-exhausted-p goal))
      (user-error "Raise or remove the Goal budget before resuming"))
    (setq mevedel-goal--transient-retries 0)
    (setf (mevedel-goal-status goal) 'active
          (mevedel-goal-reason goal) nil)
    (when-let* ((metadata (mevedel-session-plan-metadata mevedel--session))
                ((equal (plist-get metadata :implementation-goal-id)
                        (mevedel-goal-id goal))))
      (setq metadata (copy-sequence metadata))
      (cl-remf metadata :implementation-goal-id)
      (setf (mevedel-session-plan-metadata mevedel--session) metadata))
    (mevedel-goal--touch goal)
    (when (and (stringp steering) (not (string-blank-p steering)))
      (mevedel-session-enqueue-pending-input
       mevedel--session 'follow-up
       (list :input (string-trim steering)))
      (mevedel-pending-inputs-follow-up-changed mevedel--session))
    (mevedel-goal--persist mevedel--session (current-buffer))
    (mevedel-goal-schedule-continuation mevedel--session (current-buffer))
    goal))

(defun mevedel-goal-set-budget (value)
  "Set the current Goal token budget from positive integer or string VALUE.
The string `none' removes the limit."
  (interactive "sGoal token budget (positive integer or none): ")
  (let* ((text (and (stringp value) (string-trim value)))
         (budget
          (cond
           ((and text (string-equal (downcase text) "none")) nil)
           ((and text
                 (string-match-p (rx string-start (+ digit) string-end)
                                 text)
                 (> (string-to-number text) 0))
            (string-to-number text))
           ((and (integerp value) (> value 0)) value)
           (t (user-error "Goal budget must be a positive integer or none"))))
         (goal (mevedel-goal--current))
         (old (mevedel-goal-token-budget goal))
         (used (mevedel-goal-tokens-used goal))
         reactivated)
    (mevedel-goal--assert-mutation-authority mevedel--session)
    (setf (mevedel-goal-token-budget goal) budget)
    (cond
     ((and budget (>= used budget)
           (not (memq (mevedel-goal-status goal) '(blocked complete))))
      (setf (mevedel-goal-status goal) 'budget-limited
            (mevedel-goal-reason goal)
            (format "Token budget reached: %d/%d tokens used" used budget))
      (mevedel-goal--end-running-turn
       goal 'goal-budget-limited
       (format "The user lowered the Goal token budget to %d, which is reached. Stop here; this turn ends after the current tools."
               budget)))
     ((and (eq (mevedel-goal-status goal) 'budget-limited)
           (or (null budget) (< used budget)))
      (setq reactivated t)
      (setf (mevedel-goal-status goal) 'active
            (mevedel-goal-reason goal) nil)))
    (mevedel-goal--touch goal)
    (mevedel-session-enqueue-pending-reminder
     mevedel--session
     (format
      "Goal token budget changed from %s to %s; tokens used: %d; remaining: %s; status: %s."
      (or old "unbounded") (or budget "unbounded") used
      (if budget (max 0 (- budget used)) "unbounded")
      (mevedel-goal-status goal)))
    (mevedel-goal--persist mevedel--session (current-buffer))
    (when reactivated
      (setq mevedel-goal--transient-retries 0)
      (mevedel-goal-schedule-continuation mevedel--session (current-buffer)))
    goal))

(defun mevedel-goal-clear ()
  "Clear the current Goal while retaining transcript and artifacts."
  (interactive)
  (mevedel-goal--current)
  (mevedel-goal--assert-mutation-authority mevedel--session)
  (setf (mevedel-session-goal mevedel--session) nil)
  (setq mevedel-goal--transient-retries 0)
  (mevedel-goal--persist mevedel--session (current-buffer))
  nil)

(defun mevedel-goal-edit (objective)
  "Replace the current Goal OBJECTIVE and rotate its identity."
  (interactive "sNew Goal objective: ")
  (setq objective (mevedel-goal--validate-objective objective))
  (let* ((session mevedel--session)
         (goal (mevedel-goal--current))
         (old-id (mevedel-goal-id goal))
         (new-id (mevedel-goal-new-id)))
    (mevedel-goal--assert-mutation-authority session)
    (setf (mevedel-goal-id goal) new-id
          (mevedel-goal-objective goal) objective)
    (dolist (entry (mevedel-session-pending-follow-ups session))
      (when (equal old-id (plist-get entry :queued-at-goal-id))
        (plist-put entry :queued-at-goal-id new-id)))
    (when-let* ((request mevedel--current-request)
                (fsm (mevedel-request-fsm request))
                (info (gptel-fsm-info fsm))
                ((equal old-id
                        (plist-get info :mevedel-goal-accounting-id))))
      (plist-put info :mevedel-goal-accounting-id new-id)
      (setf (gptel-fsm-info fsm) info))
    (mevedel-goal--touch goal)
    (mevedel-session-enqueue-pending-reminder
     session
     (format "Goal objective updated to: %s. The revised objective has highest authority; any accepted plan remains binding only where consistent."
             objective))
    (mevedel-goal--persist session (current-buffer))
    (when (eq (mevedel-goal-status goal) 'active)
      ;; The next turn starts from the revised objective.
      (mevedel-goal--end-running-turn
       goal 'goal-edited (mevedel-goal-active-context session)))
    (when (eq (mevedel-goal-status goal) 'active)
      (mevedel-goal-schedule-continuation session (current-buffer)))
    goal))

(defun mevedel-goal-pause-runtime-failure (buffer reason)
  "Pause BUFFER's active Goal for REASON, such as a runtime failure.
A hook that stops the turn pauses the Goal through here as well."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when-let* ((session mevedel--session)
                  (goal (mevedel-session-goal session))
                  ((eq (mevedel-goal-status goal) 'active)))
        (setf (mevedel-goal-status goal) 'paused
              (mevedel-goal-reason goal) reason)
        (mevedel-goal--touch goal)
        (mevedel-goal--persist session buffer)
        goal))))


;;
;;; Root-turn attribution and settlement

(defun mevedel-goal--context-summary-request-p (info)
  "Return non-nil when INFO describes a context-summary request."
  (when-let* ((context (plist-get info :context)))
    (and (listp context)
         (plist-get context :mevedel-context-summary))))

(defun mevedel-goal-capture-request (fsm)
  "Capture active Goal attribution on root request FSM exactly once."
  (let* ((info (gptel-fsm-info fsm))
         (buffer (plist-get info :buffer)))
    (when (and (not (plist-member info :mevedel-goal-id))
               (not (or (plist-get info :mevedel-agent-invocation)
                        (mevedel-tools--buffer-local-agent-invocation buffer)))
               (not (mevedel-goal--context-summary-request-p info))
               (buffer-live-p buffer))
      (with-current-buffer buffer
        (when-let* ((session mevedel--session)
                    (goal (mevedel-session-goal session))
                    ((eq (mevedel-goal-status goal) 'active)))
          (let ((plan-path (mevedel-goal--resolve-plan-reference goal session)))
            (plist-put info :mevedel-goal-id (mevedel-goal-id goal))
            (plist-put info :mevedel-goal-accounting-id
                       (mevedel-goal-id goal))
            (plist-put info :mevedel-goal-started-at (float-time))
            (plist-put info :mevedel-goal-estimated-tokens
                       (max 1 (/ (+ (length (prin1-to-string
                                             (plist-get info :data))) 3)
                                 4)))
            ;; Also revoke a replaced Goal's plan grant.
            (when mevedel--current-request
              (setf (mevedel-request-goal-plan-read-path
                     mevedel--current-request)
                    plan-path))
            (setf (gptel-fsm-info fsm) info)))))))

(defun mevedel-goal--known-token-count (info)
  "Return known normalized input plus output tokens from request INFO."
  (let ((usage (or (plist-get info :tokens-full)
                   (plist-get info :tokens))))
    (when (and (listp usage)
               (or (plist-member usage :input) (plist-member usage :output)))
      (let ((count (+ (or (plist-get usage :input) 0)
                      (or (plist-get usage :output) 0))))
        (max 0 (- count (or (plist-get info :mevedel-goal-token-baseline) 0)))))))

(defun mevedel-goal--request-token-count (info)
  "Return normalized input plus output tokens for request INFO."
  (or (mevedel-goal--known-token-count info)
      (plist-get info :mevedel-goal-estimated-tokens)
      1))

(defun mevedel-goal--budget-threshold-crossed-p
    (before after budget percentage)
  "Return non-nil when BEFORE..AFTER crosses PERCENTAGE of BUDGET."
  (and (< (* before 100) (* budget percentage))
       (>= (* after 100) (* budget percentage))))

(defun mevedel-goal--budget-crossing-body (percentage used budget)
  "Return the root reminder for crossing PERCENTAGE with USED of BUDGET."
  (format "Goal token budget crossed %d%%: %d/%d tokens used; %d remain. %s"
          percentage used budget (max 0 (- budget used))
          (pcase percentage
            (50 "Prioritize the remaining requirements.")
            (80 "Reassess the remaining work and avoid low-value detours.")
            (_ "Stop new substantive work and wrap up the current response; do not create a separate wrap-up turn."))))

(defun mevedel-goal--budget-event-key (percentage)
  "Return the turn-event key for a PERCENTAGE budget crossing."
  (intern (format "goal-budget-%d" percentage)))

(defun mevedel-goal--emit-budget-crossings (fsm session goal before)
  "Report budget thresholds GOAL newly crossed by a charge to root FSM.
BEFORE is the durable usage before the charge.  While FSM's turn still runs,
a crossing reaches it at its next provider request; otherwise it waits for the
next root request."
  (when-let* (((eq (mevedel-goal-status goal) 'active))
              (budget (mevedel-goal-token-budget goal)))
    (let* ((info (gptel-fsm-info fsm))
           (after (mevedel-goal-tokens-used goal))
           (warned (plist-get info :mevedel-goal-budget-warnings)))
      (dolist (percentage mevedel-goal--budget-thresholds)
        (when (and (not (memq percentage warned))
                   (mevedel-goal--budget-threshold-crossed-p
                    before after budget percentage))
          (push percentage warned)
          (let ((body (mevedel-goal--budget-crossing-body
                       percentage after budget)))
            (unless (and (not (plist-get info :mevedel-goal-accounted))
                         (mevedel-reminders-queue-turn-event
                          (plist-get info :buffer)
                          (mevedel-goal--budget-event-key percentage) body))
              (mevedel-session-enqueue-pending-reminder session body)))))
      (plist-put info :mevedel-goal-budget-warnings warned)
      (setf (gptel-fsm-info fsm) info))))

(defun mevedel-goal--settle-budget (fsm session goal before)
  "Apply post-charge budget policy to GOAL for FSM in SESSION."
  (mevedel-goal--emit-budget-crossings fsm session goal before)
  (when (and (eq (mevedel-goal-status goal) 'active)
             (mevedel-goal--budget-exhausted-p goal))
    (setf (mevedel-goal-status goal) 'budget-limited
          (mevedel-goal-reason goal)
          (format "Token budget reached: %d/%d tokens used"
                  (mevedel-goal-tokens-used goal)
                  (mevedel-goal-token-budget goal)))
    (mevedel-goal--touch goal)))

(defun mevedel-goal-charge-tokens (fsm tokens)
  "Charge TOKENS spent on behalf of root request FSM to its Goal.
The Goal is the one FSM is accounted to, so charges follow an edited Goal and
skip a cleared or replaced one.  Budget crossings are reported now; the root
turn's settlement still applies the budget limit."
  (when-let* (((natnump tokens))
              ((> tokens 0))
              (info (gptel-fsm-info fsm))
              (buffer (plist-get info :buffer))
              ((buffer-live-p buffer))
              (session (buffer-local-value 'mevedel--session buffer))
              (goal (mevedel-session-goal session))
              ((equal (plist-get info :mevedel-goal-accounting-id)
                      (mevedel-goal-id goal))))
    (let ((before (mevedel-goal-tokens-used goal)))
      (cl-incf (mevedel-goal-tokens-used goal) tokens)
      (mevedel-goal--touch goal)
      (mevedel-goal--emit-budget-crossings fsm session goal before)
      goal)))

(defun mevedel-goal-tool-result-budget-warnings (session fsm)
  "Return budget warning entries newly crossed by root FSM's known usage.
SESSION owns FSM's Goal.  Durable usage plus the turn's known provider usage
is compared with the budget at a tool-result boundary, so a long turn hears
about each crossing before it settles.  Each entry holds `:key', `:body', and
a `:commit' that records delivery once the injector reaches the payload."
  (when-let* ((info (gptel-fsm-info fsm))
              (goal (mevedel-session-goal session))
              ((eq (mevedel-goal-status goal) 'active))
              ((not (plist-get info :mevedel-goal-accounted)))
              ((equal (plist-get info :mevedel-goal-accounting-id)
                      (mevedel-goal-id goal)))
              (budget (mevedel-goal-token-budget goal))
              (current (mevedel-goal--known-token-count info)))
    (let* ((before (mevedel-goal-tokens-used goal))
           (after (+ before current)))
      (cl-loop
       for percentage in mevedel-goal--budget-thresholds
       unless (memq percentage (plist-get info :mevedel-goal-budget-warnings))
       when (mevedel-goal--budget-threshold-crossed-p
             before after budget percentage)
       collect
       (let ((percentage percentage))
         (list
          :key (mevedel-goal--budget-event-key percentage)
          :body (mevedel-goal--budget-crossing-body percentage after budget)
          :commit
          (lambda ()
            (let ((current (gptel-fsm-info fsm)))
              (unless (memq percentage
                            (plist-get current :mevedel-goal-budget-warnings))
                (plist-put current :mevedel-goal-budget-warnings
                           (cons percentage
                                 (plist-get current
                                            :mevedel-goal-budget-warnings)))
                (setf (gptel-fsm-info fsm) current))))))))))

(defun mevedel-goal-accounting-fsm (&optional buffer)
  "Return the root request FSM whose Goal pays for work started in BUFFER.
In an agent's buffer this is the FSM its invocation inherited; otherwise it
is BUFFER's running root request when that request is charged to a Goal."
  (with-current-buffer (or buffer (current-buffer))
    (if-let* ((invocation (and (boundp 'mevedel--agent-invocation)
                               mevedel--agent-invocation)))
        (mevedel-agent-invocation-goal-fsm invocation)
      (when-let* ((request (and (boundp 'mevedel--current-request)
                                mevedel--current-request))
                  (fsm (mevedel-request-fsm request))
                  ((plist-get (gptel-fsm-info fsm)
                              :mevedel-goal-accounting-id)))
        fsm))))

(defun mevedel-goal-charge-agent-progress (fsm)
  "Charge agent request FSM's usage since its last charge to its Goal.
Runs after each tool batch and at the end of the request, so the Goal budget
tracks agent work while it happens rather than once it returns."
  (when-let* ((info (gptel-fsm-info fsm))
              (invocation (plist-get info :mevedel-agent-invocation))
              (goal-fsm (mevedel-agent-invocation-goal-fsm invocation))
              (known (mevedel-goal--known-token-count info))
              (delta (- known (or (plist-get info :mevedel-goal-charged) 0)))
              ((> delta 0)))
    (setf (gptel-fsm-info fsm) (plist-put info :mevedel-goal-charged known))
    (mevedel-goal-charge-tokens goal-fsm delta)))

(defun mevedel-goal-agent-budget-notice (fsm)
  "Return a budget notice entry for agent request FSM, or nil.
The notice names the highest threshold its Goal has reached that this request
was not yet told about.  Usage includes the root turn's known in-flight usage.
The entry holds `:body' and a delivery `:commit'."
  (when-let* ((info (gptel-fsm-info fsm))
              (invocation (plist-get info :mevedel-agent-invocation))
              (goal-fsm (mevedel-agent-invocation-goal-fsm invocation))
              (root (gptel-fsm-info goal-fsm))
              (buffer (plist-get root :buffer))
              ((buffer-live-p buffer))
              (session (buffer-local-value 'mevedel--session buffer))
              (goal (mevedel-session-goal session))
              ((eq (mevedel-goal-status goal) 'active))
              ((equal (plist-get root :mevedel-goal-accounting-id)
                      (mevedel-goal-id goal)))
              (budget (mevedel-goal-token-budget goal))
              (used (+ (mevedel-goal-tokens-used goal)
                       (if (plist-get root :mevedel-goal-accounted)
                           0
                         (or (mevedel-goal--known-token-count root) 0))))
              (level (cl-loop for percentage
                              in (reverse mevedel-goal--budget-thresholds)
                              when (>= (* used 100) (* budget percentage))
                              return percentage))
              ((> level (or (plist-get info :mevedel-goal-budget-level) 0))))
    (list
     :body
     (if (>= level 100)
         (format "The Goal this work belongs to has used its whole token budget (%d/%d tokens). Stop new work and return your findings to your caller now."
                 used budget)
       (format "The Goal this work belongs to has used %d%% of its token budget (%d/%d tokens). %s"
               level used budget
               (if (>= level 80)
                   "Keep to the essentials your caller needs and avoid detours."
                 "Prioritize what your caller needs.")))
     :commit
     (lambda ()
       (setf (gptel-fsm-info fsm)
             (plist-put (gptel-fsm-info fsm)
                        :mevedel-goal-budget-level level))))))

(defun mevedel-goal--settle-accounting (fsm)
  "Charge FSM and return its session, Goal, and prior usage, or nil."
  (let* ((info (gptel-fsm-info fsm))
         (captured-id (plist-get info :mevedel-goal-id))
         (accounting-id (plist-get info :mevedel-goal-accounting-id))
         (buffer (plist-get info :buffer)))
    (when (and captured-id accounting-id
               (not (plist-get info :mevedel-goal-accounted))
               (buffer-live-p buffer))
      (with-current-buffer buffer
        (when-let* ((goal (and mevedel--session
                               (mevedel-session-goal mevedel--session)))
                    ((equal accounting-id (mevedel-goal-id goal))))
          (let ((before (mevedel-goal-tokens-used goal)))
            (cl-incf (mevedel-goal-tokens-used goal)
                     (mevedel-goal--request-token-count info))
            (cl-incf (mevedel-goal-time-used-seconds goal)
                     (max 0 (round (- (float-time)
                                      (or (plist-get
                                           info :mevedel-goal-started-at)
                                          (float-time))))))
            (cl-incf (mevedel-goal-turns-run goal))
            (setf (gptel-fsm-info fsm)
                  (plist-put info :mevedel-goal-accounted t))
            (mevedel-goal--touch goal)
            (when (fboundp 'mevedel-telemetry-record)
              (mevedel-telemetry-record
               mevedel--session 'goal-turn-settled
               :captured-goal-id captured-id
               :goal-id (mevedel-goal-id goal)
               :tokens-used (mevedel-goal-tokens-used goal)
               :turns-run (mevedel-goal-turns-run goal)))
            (list mevedel--session goal before)))))))

(defun mevedel-goal--fsm-failure-reason (fsm status)
  "Return a concrete failure reason from FSM and terminal STATUS."
  (let* ((info (gptel-fsm-info fsm))
         (value (plist-get info :error)))
    (string-trim
     (format "%s"
             (cond
              ((stringp value) value)
              ((listp value) (or (plist-get value :message)
                                 (plist-get value :type)))
              (value value)
              ((plist-get info :status) (plist-get info :status))
              (t status))))))

(defun mevedel-goal--transient-failure-p (reason)
  "Return non-nil when REASON describes a retryable transport failure."
  (string-match-p
   (rx (or "timeout" "timed out" "temporar" "connection"
           "network" "unavailable" "502" "503" "504"
           ;; Name resolution, connect, partial transfer, timeout, TLS
           ;; handshake, empty reply, send and receive failures.
           (seq "curl failed with exit code "
                (or "6" "7" "18" "28" "35" "52" "55" "56")
                word-end)))
   (downcase reason)))

(defun mevedel-goal-settle-turn (fsm)
  "Charge successful Goal turn FSM."
  (when-let* ((settled (mevedel-goal--settle-accounting fsm)))
    (let ((session (nth 0 settled))
          (goal (nth 1 settled))
          (before (nth 2 settled)))
      (with-current-buffer (plist-get (gptel-fsm-info fsm) :buffer)
        (setq mevedel-goal--transient-retries 0))
      (mevedel-goal--settle-budget fsm session goal before))))

(defun mevedel-goal-settle-failure (fsm &optional status)
  "Charge failed Goal turn FSM and pause or retain it for one retry."
  (when-let* ((settled (mevedel-goal--settle-accounting fsm)))
    (let ((session (nth 0 settled))
          (goal (nth 1 settled))
          (before (nth 2 settled)))
      (with-current-buffer (plist-get (gptel-fsm-info fsm) :buffer)
        (when (eq (mevedel-goal-status goal) 'active)
          (let ((reason (mevedel-goal--fsm-failure-reason fsm status)))
            (if (and (mevedel-goal--transient-failure-p reason)
                     (< mevedel-goal--transient-retries
                        (length mevedel-goal--transient-retry-delays)))
                (message "mevedel: Goal retrying in %ds after: %s"
                         (nth (cl-incf mevedel-goal--transient-retries)
                              (cons 0 mevedel-goal--transient-retry-delays))
                         reason)
              (setf (mevedel-goal-status goal) 'paused
                    (mevedel-goal-reason goal) reason)
              (mevedel-goal--touch goal)))))
      (mevedel-goal--settle-budget fsm session goal before))))

(defun mevedel-goal-persist-failure (fsm)
  "Persist Goal failure state after FSM teardown steps."
  (when-let* ((info (gptel-fsm-info fsm))
              ((plist-get info :mevedel-goal-id))
              (buffer (plist-get info :buffer))
              ((buffer-live-p buffer)))
    (with-current-buffer buffer
      (mevedel-goal--persist mevedel--session buffer))))

(defun mevedel-goal-dispatch-after-turn (fsm &optional succeeded)
  "Schedule Goal continuation after FSM teardown.
A SUCCEEDED root turn leaves any active Goal free to continue; a failed or
interrupted turn continues only the Goal it was attributed to, after the
backoff delay of a retried transient failure."
  (when-let* ((info (gptel-fsm-info fsm))
              ((or succeeded (plist-get info :mevedel-goal-id)))
              (buffer (plist-get info :buffer)))
    (with-current-buffer buffer
      (mevedel-goal-schedule-continuation
       mevedel--session buffer nil
       (unless succeeded
         (nth mevedel-goal--transient-retries
              (cons 0 mevedel-goal--transient-retry-delays)))))))



(provide 'mevedel-goal)
;;; mevedel-goal.el ends here
