;;; niwa-goal.el -- Durable Goal continuation controller -*- lexical-binding: t -*-

;;; Commentary:

;; Owns the phase-free Goal record, request-local context, root-turn
;; attribution, and deterministic idle continuation.  Turn settlement and Goal
;; controls schedule continuation; the pending-input owner re-offers it when an
;; interaction holding an idle session closes.  Planning and review are
;; ordinary conversation work; only UpdateGoal may mark an active Goal
;; blocked, or complete once an independent verifier accepts the claim.

;;; Code:

(require 'niwa-telemetry)
(require 'niwa-utilities)

(require 'niwa-recovery)

(require 'cl-lib)
(require 'niwa-engine)
(eval-when-compile
  ;; The FSM slot setter needs its struct expander during compilation.
  (require 'gptel-request)
  (require 'niwa-structs)
  (require 'subr-x))

;; `gptel-request'

;; `niwa-agent-control'
(declare-function niwa-agent-control-steer-user
                  "niwa-agent-control"
                  (session message &optional before-wake metadata))
(declare-function niwa-agent-control-wake-pending-p
                  "niwa-agent-control" (session))
(autoload 'niwa-agent-control-steer-user "niwa-agent-control")
(autoload 'niwa-agent-control-wake-pending-p "niwa-agent-control")

;; `niwa-agents'
(declare-function niwa-agent-invocation-goal-owner "niwa-agents" (cl-x) t)

;; `niwa-chat'
(declare-function niwa--submit-generated-turn
                  "niwa-chat" (prompt &optional display-text
                                         prompt-submission prepared-outcome trigger))
(autoload 'niwa--submit-generated-turn "niwa-chat")

;; `niwa-implementation-policy'
(declare-function niwa-implementation-policy-validate
                  "niwa-implementation-policy" (policy))
(autoload 'niwa-implementation-policy-validate
  "niwa-implementation-policy")

;; `niwa-pending-inputs'
(declare-function niwa-pending-inputs--run-event-wake
                  "niwa-pending-inputs" (data-buffer))
(declare-function niwa-pending-inputs-follow-up-changed
                  "niwa-pending-inputs" (session))
(declare-function niwa-view--run-follow-up-drain
                  "niwa-pending-inputs" (data-buffer))
(autoload 'niwa-pending-inputs--run-event-wake "niwa-pending-inputs")
(autoload 'niwa-pending-inputs-follow-up-changed
  "niwa-pending-inputs")
(autoload 'niwa-view--run-follow-up-drain "niwa-pending-inputs")

;; `niwa-permission-queue'
(declare-function niwa-permission-absence-held-p
                  "niwa-permission-queue" (session))
(autoload 'niwa-permission-absence-held-p "niwa-permission-queue")

;; `niwa-plan'
(declare-function niwa-plan-artifact-path-p "niwa-plan" (path))
(declare-function niwa-plan-read-artifact "niwa-plan"
                  (session artifact))
(declare-function niwa-plan-resource-address "niwa-plan"
                  (relative-path))
(autoload 'niwa-plan-artifact-path-p "niwa-plan")
(autoload 'niwa-plan-read-artifact "niwa-plan")
(autoload 'niwa-plan-resource-address "niwa-plan")

;; `niwa-session-artifacts'
(declare-function niwa-session-artifacts-artifact-present-p
                  "niwa-session-artifacts"
                  (session logical &optional committed-only))
(declare-function niwa-session-artifacts-assert-mutation-authority
                  "niwa-session-artifacts" (session &optional buffer))
(declare-function niwa-session-artifacts-save
                  "niwa-session-artifacts"
                  (session buffer &optional settled force))
(autoload 'niwa-session-artifacts-artifact-present-p
  "niwa-session-artifacts")
(autoload 'niwa-session-artifacts-assert-mutation-authority
  "niwa-session-artifacts")
(autoload 'niwa-session-artifacts-save "niwa-session-artifacts")

;; `niwa-reminders'
(declare-function niwa-reminders-queue-turn-event
                  "niwa-reminders" (buffer key body &optional commit))
(autoload 'niwa-reminders-queue-turn-event "niwa-reminders")

;; `niwa-structs'
(declare-function niwa-goal--create "niwa-structs" (&rest slots))
(declare-function niwa-session-enqueue-pending-input
                  "niwa-structs" (session category entry))
(declare-function niwa-session-enqueue-pending-reminder
                  "niwa-structs" (session reminder))
(declare-function niwa-session-pending-follow-ups
                  "niwa-structs" (cl-x) t)
(defvar niwa--current-request)
(defvar niwa--session)
(defvar niwa--view-buffer)

;; `niwa-system'
(declare-function niwa-system-render-prompt-file
                  "niwa-system" (relative-path &optional replacements))
(autoload 'niwa-system-render-prompt-file "niwa-system")

;; `niwa-telemetry'
(declare-function niwa-telemetry-record
                  "niwa-telemetry" (session event &rest props))

;; `niwa-tools'
(declare-function niwa-tools--buffer-local-agent-invocation
                  "niwa-tools" (buffer))

;; `niwa-transport'
(declare-function niwa-transport-run-when-idle
                  "niwa-transport" (key path thunk))
(autoload 'niwa-transport-run-when-idle "niwa-transport")

;; `niwa-turn'
(declare-function niwa-turn-end-at-boundary "niwa-turn" (fsm reason))
(autoload 'niwa-turn-end-at-boundary "niwa-turn")
(defvar niwa--agent-invocation)

;; `niwa-view-interaction'
(declare-function niwa-view-interaction-pending-p
                  "niwa-view-interaction" (&optional view-buffer))
(autoload 'niwa-view-interaction-pending-p "niwa-view-interaction")


;;
;;; Settings

(defcustom niwa-goal-token-budget nil
  "Default token budget for new Goals, or nil for no limit."
  :type '(choice (const :tag "Unlimited" nil)
                 (integer :tag "Token budget"))
  :group 'niwa)

(make-variable-buffer-local 'niwa-goal-token-budget)

(defcustom niwa-goal-max-rejected-completions 3
  "Consecutive rejected completion claims after which a Goal becomes blocked.
A claim is rejected when its independent verification returns FAIL, no
valid verdict, or does not finish.  Resuming the Goal restarts the count."
  :type 'natnum
  :group 'niwa)

(defconst niwa-goal--continuation-trigger
  "Continue working toward the active Goal."
  "Ordinary user-role trigger used for initial and automatic Goal turns.")

(defconst niwa-goal--budget-thresholds '(50 80 100)
  "Percentage crossings that produce Goal budget reminders.")

(defconst niwa-goal--transient-retry-delays '(15 30 60 120 240)
  "Seconds before each consecutive retry of a transient Goal failure.
Their sum outlasts a short network outage such as a dropped Wi-Fi link.")

(defvar-local niwa-goal--transient-retries 0
  "Transient Goal failures retried since the last successful turn.")

(defvar-local niwa-goal--rejected-completions 0
  "Consecutive rejected completion claims since the Goal started or resumed.")

(defun niwa-goal-record-rejected-completion (buffer)
  "Count one rejected completion claim of BUFFER's Goal; return the total."
  (with-current-buffer buffer
    (setq niwa-goal--rejected-completions
          (1+ niwa-goal--rejected-completions))))


;;
;;; Durable record

(defvar niwa-goal--provisional-owners nil
  "Dynamically scoped constructor rollback claims.
Each entry is (GOAL . TOUCHED).  Touching GOAL revokes its claim even if
its durable contents later return to their original values.")

(defun niwa-goal--assert-mutation-authority (session &optional buffer)
  "Require SESSION mutation authority before changing Goal state.
BUFFER defaults to the current buffer."
  (niwa-session-artifacts-assert-mutation-authority
   session (or buffer (current-buffer))))

(defun niwa-goal-new-id ()
  "Return a fresh versioned Goal identifier."
  (format "%s-%06x" (format-time-string "%Y%m%d-%H%M%S")
          (random #x1000000)))

(defun niwa-goal--validate-objective (objective)
  "Return normalized non-empty OBJECTIVE or signal `user-error'."
  (unless (and (stringp objective) (not (string-blank-p objective)))
    (user-error "Goal objective must not be blank"))
  (string-trim objective))

(cl-defun niwa-goal--assert-token-budget
    (&optional (budget niwa-goal-token-budget))
  "Signal an error unless BUDGET is a positive integer or nil.
Omission checks the effective default for new Goals."
  (unless (or (null budget)
              (and (integerp budget) (> budget 0)))
    (error "Goal token budget must be a positive integer or nil")))

(defun niwa-goal--valid-plan-reference-p (reference)
  "Return non-nil when REFERENCE is a normalized relative path."
  (or (null reference)
      (niwa-plan-artifact-path-p reference)))

(cl-defun niwa-goal-create
    (objective &key session plan-reference id implementation-policy
               (token-budget niwa-goal-token-budget))
  "Create and persist a lifecycle-neutral Goal for OBJECTIVE.
SESSION defaults to the current session.  PLAN-REFERENCE is an optional
session-relative accepted-plan artifact.  ID may preallocate the Goal identity.
IMPLEMENTATION-POLICY is an optional durable implementation snapshot.
Omitting TOKEN-BUDGET inherits the effective new-Goal default; explicit nil
means unlimited.  Neither choice changes that default."
  (setq objective (niwa-goal--validate-objective objective)
        session (or session niwa--session))
  (unless session
    (error "No active session for Goal"))
  (unless (niwa-goal--valid-plan-reference-p plan-reference)
    (error "Invalid accepted-plan reference"))
  (niwa-goal--assert-token-budget token-budget)
  (niwa-implementation-policy-validate implementation-policy)
  (niwa-goal--assert-mutation-authority session)
  (let* ((previous (niwa-session-goal session))
         (now (format-time-string "%FT%T%z"))
         (goal (niwa-goal--create
                :id (or id (niwa-goal-new-id))
                :objective objective
                :status 'active
                :implementation-policy (copy-tree implementation-policy)
                :token-budget token-budget
                :tokens-used 0
                :time-used-seconds 0
                :turns-run 0
                :plan-reference plan-reference
                :created-at now
                :updated-at now))
         (provisional-state (copy-tree goal t))
         (provisional-owner (cons goal nil))
         (niwa-goal--provisional-owners
          (cons provisional-owner niwa-goal--provisional-owners)))
    (setf (niwa-session-goal session) goal)
    (condition-case err
        (niwa-session-artifacts-save session (current-buffer))
      (error
       ;; Saving can yield to public controls that replace or mutate GOAL.
       ;; Public mutation revokes ownership even if the original values return.
       (when (and (eq (niwa-session-goal session) goal)
                  (not (cdr provisional-owner))
                  (equal goal provisional-state))
         (setf (niwa-session-goal session) previous))
       (signal (car err) (cdr err))))
    (setq niwa-goal--transient-retries 0
          niwa-goal--rejected-completions 0)
    (niwa-telemetry-record session 'goal-start :goal-id (niwa-goal-id goal))
    goal))

(cl-defun niwa-goal-ensure
    (objective session plan-reference id
               &key implementation-policy
               (token-budget niwa-goal-token-budget))
  "Return the durable Goal identified by ID and PLAN-REFERENCE in SESSION.
Create it for OBJECTIVE when absent.  A matching Goal restored as paused is
reactivated without scheduling because its caller owns the prepared kickoff.
Its stored IMPLEMENTATION-POLICY must match the caller's snapshot.  Existing
accounting and budget remain unchanged.  For creation, omission of TOKEN-BUDGET
inherits the effective default; explicit nil means unlimited."
  (niwa-implementation-policy-validate implementation-policy)
  (niwa-goal--assert-token-budget token-budget)
  (niwa-goal--assert-mutation-authority session)
  (let ((current (niwa-session-goal session)))
    (cond
     ((and current
           (equal id (niwa-goal-id current))
           (equal plan-reference (niwa-goal-plan-reference current)))
      (let ((stored (niwa-goal-implementation-policy current)))
        (niwa-implementation-policy-validate stored)
        (unless (and (eq (null stored) (null implementation-policy))
                     (cl-every
                      (lambda (key)
                        (equal (plist-get stored key)
                               (plist-get implementation-policy key)))
                      '(:model-provider :reasoning-effort :permission-mode)))
          (error "Reserved Goal %s has a conflicting implementation policy" id)))
      (unless (memq (niwa-goal-status current) '(active paused))
        (error "Reserved Goal %s has terminal status %s"
               id (niwa-goal-status current)))
      (setf (niwa-goal-status current) 'active
            (niwa-goal-reason current) nil
            (niwa-goal-reason-diagnostic current) nil)
      (niwa-goal--touch current)
      (niwa-goal--persist session (current-buffer))
      current)
     ((and current (not (eq (niwa-goal-status current) 'complete)))
      (error "Target session has unfinished Goal %s; expected reserved Goal %s for %s"
             (niwa-goal-id current) id plan-reference))
     (t
      (niwa-goal-create
       objective :session session :plan-reference plan-reference :id id
       :implementation-policy implementation-policy :token-budget token-budget)))))

(defun niwa-goal--current ()
  "Return the current session Goal or signal `user-error'."
  (unless (bound-and-true-p niwa--session)
    (user-error "No niwa session in this buffer"))
  (or (niwa-session-goal niwa--session)
      (user-error "No current Goal")))

(defun niwa-goal--touch (goal)
  "Update GOAL's modification timestamp and revoke constructor rollback."
  (dolist (owner niwa-goal--provisional-owners)
    (when (eq (car owner) goal)
      (setcdr owner t)))
  (setf (niwa-goal-updated-at goal) (format-time-string "%FT%T%z"))
  goal)

(defun niwa-goal--persist (session buffer)
  "Persist SESSION from BUFFER when both remain live."
  (when (and session (buffer-live-p buffer))
    (with-current-buffer buffer
      (niwa-session-artifacts-save session buffer))))


;;
;;; Accepted-plan authority and request context

(defun niwa-goal--pause-for-integrity (goal session reason)
  "Persistently pause GOAL in SESSION for integrity REASON, then error."
  (setf (niwa-goal-status goal) 'paused
        (niwa-goal-reason goal) reason
        (niwa-goal-reason-diagnostic goal) t)
  (niwa-goal--touch goal)
  (niwa-goal--persist session (current-buffer))
  (error "%s" reason))

(defun niwa-goal--resolve-plan-reference (goal session)
  "Return GOAL's validated accepted-plan resource address in SESSION, or nil.

The artifact is validated through its private session-owned path, but the
returned value is the model-facing `work://plans/...' address so Goal context
never discloses session storage paths."
  (when-let* ((reference (niwa-goal-plan-reference goal)))
    (unless (niwa-goal--valid-plan-reference-p reference)
      (niwa-goal--pause-for-integrity
       goal session "Accepted-plan reference is invalid"))
    (let* ((metadata (niwa-session-plan-metadata session))
           (accepted (plist-get metadata :accepted-path))
           (expected-hash (plist-get metadata :accepted-hash))
           (artifact (list :path reference :hash expected-hash)))
      (unless (and (equal reference accepted)
                   (stringp expected-hash))
        (niwa-goal--pause-for-integrity
         goal session
         "Accepted-plan artifact is missing or no longer owned by this session"))
      (unless (niwa-session-artifacts-artifact-present-p
               session reference)
        (niwa-goal--pause-for-integrity
         goal session
         "Accepted-plan artifact is missing or no longer owned by this session"))
      (condition-case nil
          (niwa-plan-read-artifact session artifact)
        (error
         (niwa-goal--pause-for-integrity
          goal session
          "Accepted-plan artifact no longer matches its accepted hash")))
      (condition-case nil
          (niwa-plan-resource-address reference)
        (error
         (niwa-goal--pause-for-integrity
          goal session
          "Accepted-plan reference is outside managed plan storage"))))))

(defun niwa-goal-active-context (session)
  "Render request-local active Goal context for SESSION, or nil."
  (when-let* ((goal (niwa-session-goal session))
              ((eq (niwa-goal-status goal) 'active)))
    ;; Live usage and the turn count stay out: they move with every charge
    ;; or turn, and each change would re-deliver the whole block.  Crossing
    ;; reminders report usage; GetGoal reports both usage and turns run.
    (let* ((budget (niwa-goal-token-budget goal))
           (plan-address (niwa-goal--resolve-plan-reference goal session)))
      (niwa-system-render-prompt-file
       "prompts/goals/active-context.md"
       `(("objective" . ,(niwa-goal-objective goal))
         ("token-budget" . ,(if budget
                                (format "%d tokens" budget)
                              "unbounded"))
         ("plan-reference-line" .
          ,(if plan-address
               (format "Accepted plan: %s. Its outcomes, constraints, and achievement criteria are binding except where amended by the current objective; its implementation mechanics are revisable."
                       plan-address)
             "")))))))


;;
;;; Continuation and commands

(defun niwa-goal--pending-interaction-p (session)
  "Return non-nil when SESSION has a pending user interaction."
  (or (niwa-session-permission-queue session)
      (niwa-session-pending-plan-approval session)
      (and (boundp 'niwa--view-buffer)
           (buffer-live-p niwa--view-buffer)
           (niwa-view-interaction-pending-p niwa--view-buffer))))

(defun niwa-goal--budget-exhausted-p (goal)
  "Return non-nil when GOAL has exhausted or cannot measure its budget."
  (when-let* ((budget (niwa-goal-token-budget goal)))
    (or (niwa-goal-tokens-incomplete-p goal)
        (>= (niwa-goal-tokens-used goal) budget))))

(defun niwa-goal-continue-if-idle
    (&optional session buffer prompt-submission)
  "Start SESSION's next Goal turn from BUFFER when all gates admit it.
Return `dispatched' on dispatch or the deterministic blocking gate symbol."
  (let* ((session (or session niwa--session))
         (buffer (or buffer (current-buffer)))
         (goal (and session (niwa-session-goal session))))
    (cond
     ((or (not (buffer-live-p buffer))
          (not (eq session (buffer-local-value 'niwa--session buffer))))
      'unavailable)
     ((not goal) 'no-goal)
     ((not (eq (niwa-goal-status goal) 'active)) 'inactive)
     ((buffer-local-value 'niwa--current-request buffer) 'request)
     ((niwa-permission-absence-held-p session) 'absence)
     ((or (niwa-session-pending-steering session)
          (niwa-session-pending-input-paused session))
      'pending-input)
     ((niwa-session-pending-follow-ups session)
      (run-at-time 0 nil #'niwa-view--run-follow-up-drain buffer)
      'follow-up)
     ;; Background work that reported runs before the Goal does; the turn
     ;; it starts continues the Goal when it settles.
     ((niwa-agent-control-wake-pending-p session)
      (run-at-time 0 nil #'niwa-pending-inputs--run-event-wake buffer)
      'event)
     ((with-current-buffer buffer
        (niwa-goal--pending-interaction-p session)) 'interaction)
     ((niwa-goal--budget-exhausted-p goal) 'budget)
     (t
      (with-current-buffer buffer
        (niwa--submit-generated-turn
         niwa-goal--continuation-trigger
         niwa-goal--continuation-trigger
         prompt-submission nil 'goal-continuation))
      (niwa-telemetry-record
       session 'goal-continuation :goal-id (niwa-goal-id goal))
      'dispatched))))

(defun niwa-goal--scheduled-continuation (session buffer prompt-submission)
  "Run a scheduled continuation for SESSION in BUFFER.

Continuation admits a new turn, which is target I/O, and it runs from a timer.
A timer fires wherever the main loop is waiting, so it waits for an idle
transport rather than nesting inside a remote operation already in flight."
  (when (and (buffer-live-p buffer)
             (eq session (buffer-local-value 'niwa--session buffer)))
    (niwa-transport-run-when-idle
     (list 'goal-continuation buffer)
     (buffer-local-value 'default-directory buffer)
     (lambda ()
       (when (and (buffer-live-p buffer)
                  (eq session (buffer-local-value 'niwa--session buffer)))
         (with-current-buffer buffer
           (condition-case err
               (niwa-goal-continue-if-idle session buffer prompt-submission)
             (error
              (niwa-goal-pause-runtime-failure
               buffer (format "Goal continuation failed: %s" (error-message-string err)))
              (niwa-diagnostic-message (niwa-diagnostic-scope session)
               "%s (Goal continuation failed)" (error-message-string err))))))))))

(defun niwa-goal-schedule-continuation
    (&optional session buffer prompt-submission delay)
  "Schedule SESSION's Goal continuation check after the current command.
DELAY is the number of seconds to wait first; nil means none."
  (let ((session (or session niwa--session))
        (buffer (or buffer (current-buffer))))
    (when (and session (buffer-live-p buffer))
      (run-at-time (or delay 0) nil #'niwa-goal--scheduled-continuation
                   session buffer prompt-submission))))

(defun niwa-goal--start-blocker (session)
  "Return why a new Goal cannot start in SESSION now, or nil.
Reads the current buffer's request for request-scoped Plan mode."
  (let ((current (niwa-session-goal session))
        (metadata (niwa-session-plan-metadata session)))
    (cond
     ((or (niwa-session-plan-mode session)
          (and niwa--current-request
               (niwa-request-plan-read-only niwa--current-request)))
      "Leave Plan mode before starting a Goal")
     ((and current (not (eq (niwa-goal-status current) 'complete)))
      "Finish or clear the current Goal first")
     ((plist-get metadata :implementation-retry)
      "Finish or cancel the accepted Plan implementation first"))))

(defun niwa-goal-start (objective &optional prompt-submission)
  "Start a Goal for OBJECTIVE and schedule its first ordinary turn."
  (setq objective (niwa-goal--validate-objective objective))
  (unless (bound-and-true-p niwa--session)
    (user-error "No niwa session in this buffer"))
  (when-let* ((blocker (niwa-goal--start-blocker niwa--session)))
    (user-error "%s" blocker))
  ;; Validate before settling a replaced Goal below.
  (niwa-goal--assert-token-budget)
  (niwa-goal--assert-mutation-authority niwa--session)
  (let ((current (niwa-session-goal niwa--session)))
    (let* ((fsm (when-let* ((request niwa--current-request)
                            ((eq niwa--session (niwa-request-session request)))
                            ((not (or (niwa-request-ephemeral-p request)
                                      (niwa-request-cancelled-p request))))
                            (machine (niwa-engine-owner request))
                            ((eq (current-buffer)
                                 (plist-get (niwa-engine-info machine) :buffer))))
                  machine))
           (info (and fsm (niwa-engine-info fsm)))
           (replacing (and current
                           (equal (niwa-goal-id current)
                                  (plist-get info :niwa-goal-accounting-id))))
           (baseline (if replacing
                         (+ (or (niwa-goal--known-token-count info) 0)
                            (or (plist-get info :niwa-goal-token-baseline) 0))
                       0)))
      ;; Preserve the completed Goal's final charge before replacing it.
      ;; A cleared, unrelated Goal retains its old request attribution.
      (when replacing (niwa-goal-settle-turn fsm))
      (let ((goal (niwa-goal-create objective :session niwa--session)))
        (when fsm
          (setq info (niwa-engine-info fsm))
          (when replacing
            (dolist (key '(:niwa-goal-id :niwa-goal-accounting-id
                                            :niwa-goal-accounted :niwa-goal-budget-warnings))
              (cl-remf info key))
            (setq info (plist-put info :niwa-goal-token-baseline baseline)))
          (setf (niwa-engine-info fsm) info)
          (niwa-goal-capture-request fsm))
        (niwa-goal-schedule-continuation
         niwa--session (current-buffer) prompt-submission)
        goal))))

(defun niwa-goal--end-running-turn (goal reason message)
  "End the current buffer's root turn charged to GOAL at its tool boundary.
The turn settles normally instead of sampling the model again, so a user
control takes effect after the running tools rather than after an arbitrarily
long turn.  A turn waiting in WaitAgent reaches that boundary only when the
wait returns, so MESSAGE is also delivered as steering to wake it.  REASON
names the control for telemetry."
  (when-let* ((request niwa--current-request)
              (fsm (niwa-engine-owner request))
              ((equal (niwa-goal-id goal)
                      (plist-get (niwa-engine-info fsm)
                                 :niwa-goal-accounting-id)))
              ((niwa-turn-end-at-boundary fsm reason)))
    (ignore-errors
      (niwa-agent-control-steer-user niwa--session message))
    t))

(defun niwa-goal-pause ()
  "Pause the current Goal, ending its running turn at the next tool boundary.
Running tools finish and the turn settles; it is not aborted."
  (interactive)
  (let ((goal (niwa-goal--current)))
    (niwa-goal--assert-mutation-authority niwa--session)
    (unless (eq (niwa-goal-status goal) 'active)
      (user-error "Goal is not active"))
    (setf (niwa-goal-status goal) 'paused
          (niwa-goal-reason goal) "paused by user"
          (niwa-goal-reason-diagnostic goal) nil)
    (niwa-goal--touch goal)
    (niwa-goal--persist niwa--session (current-buffer))
    (niwa-goal--end-running-turn
     goal 'goal-paused
     "The user paused the Goal. Stop here; this turn ends after the current tools.")
    goal))

(defun niwa-goal-resume (&optional steering)
  "Resume the current Goal, optionally with ordinary STEERING text."
  (interactive)
  (let ((goal (niwa-goal--current)))
    (niwa-goal--assert-mutation-authority niwa--session)
    (when (eq (niwa-goal-status goal) 'complete)
      (user-error "Completed Goal cannot be resumed"))
    (when (or (eq (niwa-goal-status goal) 'budget-limited)
              (niwa-goal--budget-exhausted-p goal))
      (user-error "%s" (if (niwa-goal-tokens-incomplete-p goal)
                            "Native usage is incomplete; remove the budget or start a new Goal"
                          "Raise or remove the Goal budget before resuming")))
    (setq niwa-goal--transient-retries 0
          niwa-goal--rejected-completions 0)
    (setf (niwa-goal-status goal) 'active
          (niwa-goal-reason goal) nil
          (niwa-goal-reason-diagnostic goal) nil)
    (niwa-goal--touch goal)
    (when (and (stringp steering) (not (string-blank-p steering)))
      (niwa-session-enqueue-pending-input
       niwa--session 'follow-up
       (list :input (string-trim steering)))
      (niwa-pending-inputs-follow-up-changed niwa--session))
    (niwa-goal--persist niwa--session (current-buffer))
    (niwa-goal-schedule-continuation niwa--session (current-buffer))
    goal))

(defun niwa-goal-set-budget (value)
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
         (goal (niwa-goal--current))
         (old (niwa-goal-token-budget goal))
         (used (niwa-goal-tokens-used goal))
         reactivated)
    (niwa-goal--assert-mutation-authority niwa--session)
    (setf (niwa-goal-token-budget goal) budget)
    (cond
     ((and (niwa-goal--budget-exhausted-p goal)
           (not (memq (niwa-goal-status goal) '(blocked complete))))
      (setf (niwa-goal-status goal) 'budget-limited
            (niwa-goal-reason-diagnostic goal) nil
            (niwa-goal-reason goal)
            (if (niwa-goal-tokens-incomplete-p goal)
                "Native token usage is incomplete; remove the budget or start a new Goal to continue"
              (format "Token budget reached: %d/%d tokens used" used budget)))
      (niwa-goal--end-running-turn
       goal 'goal-budget-limited
       (format "The Goal token budget is %d and further usage cannot be admitted. Stop here; this turn ends after the current tools."
               budget)))
     ((and (eq (niwa-goal-status goal) 'budget-limited)
           (not (niwa-goal--budget-exhausted-p goal)))
      (setq reactivated t)
      (setf (niwa-goal-status goal) 'active
            (niwa-goal-reason goal) nil
            (niwa-goal-reason-diagnostic goal) nil)))
    (niwa-goal--touch goal)
    (niwa-session-enqueue-pending-reminder
     niwa--session
     (format
      "Goal token budget changed from %s to %s; tokens used: %s; remaining: %s; status: %s."
      (or old "unbounded") (or budget "unbounded")
      (if (niwa-goal-tokens-incomplete-p goal) (format "at least %d (incomplete)" used) used)
      (cond ((niwa-goal-tokens-incomplete-p goal) "unknown")
            (budget (max 0 (- budget used))) (t "unbounded"))
      (niwa-goal-status goal)))
    (niwa-goal--persist niwa--session (current-buffer))
    (when reactivated
      (setq niwa-goal--transient-retries 0)
      (niwa-goal-schedule-continuation niwa--session (current-buffer)))
    goal))

(defun niwa-goal-clear ()
  "Clear the current Goal while retaining transcript and artifacts."
  (interactive)
  (niwa-goal--current)
  (niwa-goal--assert-mutation-authority niwa--session)
  (setf (niwa-session-goal niwa--session) nil)
  (setq niwa-goal--transient-retries 0
        niwa-goal--rejected-completions 0)
  (niwa-goal--persist niwa--session (current-buffer))
  nil)

(defun niwa-goal-edit (objective)
  "Replace the current Goal OBJECTIVE and rotate its identity."
  (interactive "sNew Goal objective: ")
  (setq objective (niwa-goal--validate-objective objective))
  (let* ((session niwa--session)
         (goal (niwa-goal--current))
         (old-id (niwa-goal-id goal))
         (new-id (niwa-goal-new-id)))
    (niwa-goal--assert-mutation-authority session)
    (setf (niwa-goal-id goal) new-id
          (niwa-goal-objective goal) objective)
    (dolist (entry (niwa-session-pending-follow-ups session))
      (when (equal old-id (plist-get entry :queued-at-goal-id))
        (plist-put entry :queued-at-goal-id new-id)))
    (when-let* ((request niwa--current-request)
                (fsm (niwa-engine-owner request))
                (info (niwa-engine-info fsm))
                ((equal old-id
                        (plist-get info :niwa-goal-accounting-id))))
      (plist-put info :niwa-goal-accounting-id new-id)
      (setf (niwa-engine-info fsm) info))
    (niwa-goal--touch goal)
    (niwa-session-enqueue-pending-reminder
     session
     (format "Goal objective updated to: %s. The revised objective has highest authority; any accepted plan remains binding only where consistent."
             objective))
    (niwa-goal--persist session (current-buffer))
    (when (eq (niwa-goal-status goal) 'active)
      ;; The next turn starts from the revised objective.
      (niwa-goal--end-running-turn
       goal 'goal-edited (niwa-goal-active-context session)))
    (when (eq (niwa-goal-status goal) 'active)
      (niwa-goal-schedule-continuation session (current-buffer)))
    goal))

(defun niwa-goal-pause-runtime-failure (buffer reason)
  "Pause BUFFER's active Goal for REASON, such as a runtime failure.
A hook that stops the turn pauses the Goal through here as well."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when-let* ((session niwa--session)
                  (goal (niwa-session-goal session))
                  ((eq (niwa-goal-status goal) 'active)))
        (setf (niwa-goal-status goal) 'paused
              (niwa-goal-reason goal) reason
              (niwa-goal-reason-diagnostic goal) t)
        (niwa-goal--touch goal)
        (niwa-goal--persist session buffer)
        goal))))


;;
;;; Root-turn attribution and settlement

(defun niwa-goal--context-summary-request-p (info)
  "Return non-nil when INFO describes a context-summary request."
  (when-let* ((context (plist-get info :context)))
    (and (listp context)
         (plist-get context :niwa-context-summary))))

(defun niwa-goal-capture-request (fsm)
  "Capture active Goal attribution on root request FSM exactly once."
  (let* ((info (niwa-engine-info fsm))
         (context (plist-get info :context))
         (buffer (plist-get info :buffer)))
    (when (and (not (plist-member info :niwa-goal-id))
               (not (and (listp context) (eq (plist-get context :turn-trigger) 'scheduled-wake)))
               (not (or (plist-get info :niwa-agent-invocation)
                        (niwa-tools--buffer-local-agent-invocation buffer)))
               (not (niwa-goal--context-summary-request-p info))
               (buffer-live-p buffer))
      (with-current-buffer buffer
        (when-let* ((session niwa--session)
                    (goal (niwa-session-goal session))
                    ((eq (niwa-goal-status goal) 'active)))
          (let ((plan-path (niwa-goal--resolve-plan-reference goal session)))
            (plist-put info :niwa-goal-id (niwa-goal-id goal))
            (plist-put info :niwa-goal-accounting-id
                       (niwa-goal-id goal))
            (plist-put info :niwa-goal-started-at (float-time))
            (unless (niwa-engine-external-p (plist-get info :backend))
              (plist-put info :niwa-goal-estimated-tokens
                         (max 1 (/ (+ (length (prin1-to-string
                                               (plist-get info :data))) 3)
                                   4))))
            ;; Also revoke a replaced Goal's plan grant.
            (when niwa--current-request
              (setf (niwa-request-goal-plan-read-path
                     niwa--current-request)
                    plan-path))
            (setf (niwa-engine-info fsm) info)))))))

(defun niwa-goal--known-token-count (info)
  "Return known normalized input plus output tokens from request INFO."
  (let ((usage (or (plist-get info :tokens-full)
                   (plist-get info :tokens))))
    (when (and (listp usage)
               (or (plist-member usage :input) (plist-member usage :output)))
      (let ((count (+ (or (plist-get usage :input) 0)
                      (or (plist-get usage :output) 0))))
        (max 0 (- count (or (plist-get info :niwa-goal-token-baseline) 0)))))))

(defun niwa-goal--request-token-count (info)
  "Return accounted tokens for INFO, preserving native unknown usage."
  (or (niwa-goal--known-token-count info)
      (unless (niwa-engine-external-p (plist-get info :backend))
        (or (plist-get info :niwa-goal-estimated-tokens) 1))
      0))

(defun niwa-goal--native-usage-incomplete-p (info)
  "Return non-nil when native INFO lacks complete request usage."
  (and (niwa-engine-external-p (plist-get info :backend))
       (let ((tokens (plist-get info :tokens-full)))
         (or (plist-get info :niwa-usage-incomplete)
             (plist-get info :niwa-usage-pending)
             (not (and (natnump (plist-get tokens :input))
                       (natnump (plist-get tokens :output))))))))

(defun niwa-goal--record-incomplete-usage (fsm info)
  "Record missing native usage from INFO in root owner FSM's Goal.
Known counters remain a lower bound.  A finite budget cannot be enforced once
any submitted native request lacks a complete input and output total."
  (when (and (plist-get info :niwa-acp-prompted)
             (niwa-goal--native-usage-incomplete-p info))
    (let ((root (niwa-engine-info fsm)))
      (when-let* ((buffer (plist-get root :buffer))
                  ((buffer-live-p buffer))
                  (session (buffer-local-value 'niwa--session buffer))
                  (goal (niwa-session-goal session))
                  ((equal (plist-get root :niwa-goal-accounting-id)
                          (niwa-goal-id goal))))
        (setf (niwa-goal-tokens-incomplete-p goal) t)
        (niwa-goal--touch goal)
        goal))))

(defun niwa-goal--budget-threshold-crossed-p
    (before after budget percentage)
  "Return non-nil when BEFORE..AFTER crosses PERCENTAGE of BUDGET."
  (and (< (* before 100) (* budget percentage))
       (>= (* after 100) (* budget percentage))))

(defun niwa-goal--budget-crossing-body (percentage used budget)
  "Return the root reminder for crossing PERCENTAGE with USED of BUDGET."
  (format "Goal token budget crossed %d%%: %d/%d tokens used; %d remain. %s"
          percentage used budget (max 0 (- budget used))
          (pcase percentage
            (50 "Prioritize the remaining requirements.")
            (80 "Reassess the remaining work and avoid low-value detours.")
            (_ "Stop new substantive work and wrap up the current response; do not create a separate wrap-up turn."))))

(defun niwa-goal--budget-event-key (percentage)
  "Return the turn-event key for a PERCENTAGE budget crossing."
  (intern (format "goal-budget-%d" percentage)))

(defun niwa-goal--emit-budget-crossings (fsm session goal before)
  "Report budget thresholds GOAL newly crossed by a charge to root FSM.
BEFORE is the durable usage before the charge.  While FSM's turn still runs,
a crossing reaches it at its next provider request; otherwise it waits for the
next root request."
  (when-let* (((eq (niwa-goal-status goal) 'active))
              (budget (niwa-goal-token-budget goal)))
    (let* ((info (niwa-engine-info fsm))
           (after (niwa-goal-tokens-used goal))
           (warned (plist-get info :niwa-goal-budget-warnings)))
      (dolist (percentage niwa-goal--budget-thresholds)
        (when (and (not (memq percentage warned))
                   (niwa-goal--budget-threshold-crossed-p
                    before after budget percentage))
          (push percentage warned)
          (let ((body (niwa-goal--budget-crossing-body
                       percentage after budget)))
            (unless (and (not (plist-get info :niwa-goal-accounted))
                         (niwa-reminders-queue-turn-event
                          (plist-get info :buffer)
                          (niwa-goal--budget-event-key percentage) body))
              (niwa-session-enqueue-pending-reminder session body)))))
      (plist-put info :niwa-goal-budget-warnings warned)
      (setf (niwa-engine-info fsm) info))))

(defun niwa-goal--settle-budget (fsm session goal before)
  "Apply post-charge budget policy to GOAL for FSM in SESSION."
  (niwa-goal--emit-budget-crossings fsm session goal before)
  (when (and (eq (niwa-goal-status goal) 'active)
             (niwa-goal--budget-exhausted-p goal))
    (setf (niwa-goal-status goal) 'budget-limited
          (niwa-goal-reason-diagnostic goal) nil
          (niwa-goal-reason goal)
          (if (niwa-goal-tokens-incomplete-p goal)
              "Native token usage is incomplete; remove the budget or start a new Goal to continue"
            (format "Token budget reached: %d/%d tokens used"
                    (niwa-goal-tokens-used goal)
                    (niwa-goal-token-budget goal))))
    (niwa-goal--touch goal)))

(defun niwa-goal-charge-tokens (fsm tokens)
  "Charge TOKENS spent on behalf of root request FSM to its Goal.
The Goal is the one FSM is accounted to, so charges follow an edited Goal and
skip a cleared or replaced one.  Budget crossings are reported now; the root
turn's settlement still applies the budget limit."
  (when-let* (((natnump tokens))
              ((> tokens 0))
              (info (niwa-engine-info fsm))
              (buffer (plist-get info :buffer))
              ((buffer-live-p buffer))
              (session (buffer-local-value 'niwa--session buffer))
              (goal (niwa-session-goal session))
              ((equal (plist-get info :niwa-goal-accounting-id)
                      (niwa-goal-id goal))))
    (let ((before (niwa-goal-tokens-used goal)))
      (cl-incf (niwa-goal-tokens-used goal) tokens)
      (niwa-goal--touch goal)
      (niwa-goal--emit-budget-crossings fsm session goal before)
      goal)))

(defun niwa-goal-tool-result-budget-warnings (session fsm)
  "Return budget warning entries newly crossed by root FSM's known usage.
SESSION owns FSM's Goal.  Durable usage plus the turn's known provider usage
is compared with the budget at a tool-result boundary, so a long turn hears
about each crossing before it settles.  Each entry holds `:key', `:body', and
a `:commit' that records delivery once the injector reaches the payload."
  (when-let* ((info (niwa-engine-info fsm))
              (goal (niwa-session-goal session))
              ((eq (niwa-goal-status goal) 'active))
              ((not (plist-get info :niwa-goal-accounted)))
              ((equal (plist-get info :niwa-goal-accounting-id)
                      (niwa-goal-id goal)))
              (budget (niwa-goal-token-budget goal))
              (current (niwa-goal--known-token-count info)))
    (let* ((before (niwa-goal-tokens-used goal))
           (after (+ before current)))
      (cl-loop
       for percentage in niwa-goal--budget-thresholds
       unless (memq percentage (plist-get info :niwa-goal-budget-warnings))
       when (niwa-goal--budget-threshold-crossed-p
             before after budget percentage)
       collect
       (let ((percentage percentage))
         (list
          :key (niwa-goal--budget-event-key percentage)
          :body (niwa-goal--budget-crossing-body percentage after budget)
          :commit
          (lambda ()
            (let ((current (niwa-engine-info fsm)))
              (unless (memq percentage
                            (plist-get current :niwa-goal-budget-warnings))
                (plist-put current :niwa-goal-budget-warnings
                           (cons percentage
                                 (plist-get current
                                            :niwa-goal-budget-warnings)))
                (setf (niwa-engine-info fsm) current))))))))))

(defun niwa-goal-accounting-owner (&optional buffer)
  "Return the root engine owner whose Goal pays for work started in BUFFER.
In an agent's buffer this is the owner its invocation inherited; otherwise it
is BUFFER's running root request when that request is charged to a Goal."
  (with-current-buffer (or buffer (current-buffer))
    (if-let* ((invocation niwa--agent-invocation))
        (niwa-agent-invocation-goal-owner invocation)
      (when-let* ((request (and (boundp 'niwa--current-request)
                                niwa--current-request))
                  (fsm (niwa-engine-owner request))
                  ((plist-get (niwa-engine-info fsm)
                              :niwa-goal-accounting-id)))
        fsm))))

(defun niwa-goal-charge-agent-progress (fsm &optional terminal)
  "Charge agent FSM's usage since its last charge to its Goal.
At TERMINAL settlement also retain missing native counters as unknown."
  (when-let* ((info (niwa-engine-info fsm))
              (invocation (plist-get info :niwa-agent-invocation))
              (goal-owner (niwa-agent-invocation-goal-owner invocation)))
    (when-let* ((known (niwa-goal--known-token-count info))
                (delta (- known (or (plist-get info :niwa-goal-charged) 0)))
                ((> delta 0)))
      (setf (niwa-engine-info fsm) (plist-put info :niwa-goal-charged known))
      (niwa-goal-charge-tokens goal-owner delta))
    (when-let* (((and terminal (niwa-goal--record-incomplete-usage goal-owner info)))
                (root (niwa-engine-info goal-owner))
                (buffer (plist-get root :buffer))
                (session (buffer-local-value 'niwa--session buffer))
                (goal (niwa-session-goal session))
                ((eq 'active (niwa-goal-status goal)))
                ((niwa-goal-token-budget goal)))
      (let* ((current (niwa-goal-accounting-owner buffer))
             (running (and current (niwa-engine-info current))))
        (if (and (equal (plist-get running :niwa-goal-accounting-id) (niwa-goal-id goal))
                 (not (plist-get running :niwa-goal-accounted)))
            (niwa-reminders-queue-turn-event
             buffer 'goal-usage-incomplete
             "Native token usage for this Goal is incomplete, so its remaining budget is unknown. Stop new substantive work and wrap up this response; automatic continuation will stop at settlement.")
          (niwa-goal--settle-budget goal-owner session goal (niwa-goal-tokens-used goal)))))))

(defun niwa-goal-agent-budget-notice (fsm)
  "Return a budget notice entry for agent request FSM, or nil.
The notice names the highest threshold its Goal has reached that this request
was not yet told about.  Usage includes the root turn's known in-flight usage.
The entry holds `:body' and a delivery `:commit'."
  (when-let* ((info (niwa-engine-info fsm))
              (invocation (plist-get info :niwa-agent-invocation))
              (goal-owner (niwa-agent-invocation-goal-owner invocation))
              (root (niwa-engine-info goal-owner))
              (buffer (plist-get root :buffer))
              ((buffer-live-p buffer))
              (session (buffer-local-value 'niwa--session buffer))
              (goal (niwa-session-goal session))
              ((eq (niwa-goal-status goal) 'active))
              ((equal (plist-get root :niwa-goal-accounting-id)
                      (niwa-goal-id goal)))
              (budget (niwa-goal-token-budget goal))
              (used (+ (niwa-goal-tokens-used goal)
                       (if (plist-get root :niwa-goal-accounted)
                           0
                         (or (niwa-goal--known-token-count root) 0))))
              (level (cl-loop for percentage
                              in (reverse niwa-goal--budget-thresholds)
                              when (>= (* used 100) (* budget percentage))
                              return percentage))
              ((> level (or (plist-get info :niwa-goal-budget-level) 0))))
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
       (setf (niwa-engine-info fsm)
             (plist-put (niwa-engine-info fsm)
                        :niwa-goal-budget-level level))))))

(defun niwa-goal--settle-accounting (fsm)
  "Charge FSM and return its session, Goal, and prior usage, or nil."
  (let* ((info (niwa-engine-info fsm))
         (captured-id (plist-get info :niwa-goal-id))
         (accounting-id (plist-get info :niwa-goal-accounting-id))
         (buffer (plist-get info :buffer)))
    (when (and captured-id accounting-id
               (not (plist-get info :niwa-goal-accounted))
               (buffer-live-p buffer))
      (with-current-buffer buffer
        (when-let* ((goal (and niwa--session
                               (niwa-session-goal niwa--session)))
                    ((equal accounting-id (niwa-goal-id goal))))
          (let ((before (niwa-goal-tokens-used goal)))
            (cl-incf (niwa-goal-tokens-used goal)
                     (niwa-goal--request-token-count info))
            (niwa-goal--record-incomplete-usage fsm info)
            (cl-incf (niwa-goal-time-used-seconds goal)
                     (max 0 (round (- (float-time)
                                      (or (plist-get
                                           info :niwa-goal-started-at)
                                          (float-time))))))
            (cl-incf (niwa-goal-turns-run goal))
            (setf (niwa-engine-info fsm)
                  (plist-put info :niwa-goal-accounted t))
            (niwa-goal--touch goal)
            (niwa-telemetry-record
             niwa--session 'goal-turn-settled
             :captured-goal-id captured-id
             :goal-id (niwa-goal-id goal)
             :tokens-used (niwa-goal-tokens-used goal)
             :turns-run (niwa-goal-turns-run goal))
            (list niwa--session goal before)))))))

(defun niwa-goal--fsm-failure-reason (fsm status)
  "Return a concrete failure reason from FSM and terminal STATUS."
  (let* ((info (niwa-engine-info fsm))
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

(defun niwa-goal--transient-failure-p (reason)
  "Return non-nil when REASON describes a retryable transport failure.
Sign-in, runtime, configuration and history failures need the user; a
model reported unavailable in free text is often a transient overload and
classifies as an ordinary request failure, so it is retried."
  (and (eq (niwa-recovery-category reason) 'request)
       (string-match-p
        (rx (or "timeout" "timed out" "temporar" "connection"
                "network" "unavailable" "502" "503" "504"
                ;; Name resolution, connect, partial transfer, timeout, TLS
                ;; handshake, empty reply, send and receive failures.
                (seq "curl failed with exit code "
                     (or "6" "7" "18" "28" "35" "52" "55" "56")
                     word-end)))
        (downcase reason))))

(defun niwa-goal-settle-turn (fsm)
  "Charge successful Goal turn FSM."
  (when-let* ((settled (niwa-goal--settle-accounting fsm)))
    (let ((session (nth 0 settled))
          (goal (nth 1 settled))
          (before (nth 2 settled)))
      (with-current-buffer (plist-get (niwa-engine-info fsm) :buffer)
        (setq niwa-goal--transient-retries 0))
      (niwa-goal--settle-budget fsm session goal before))))

(defun niwa-goal-settle-failure (fsm &optional status)
  "Charge failed Goal turn FSM and pause or retain it for one retry.
Exhausted retries or a permanent request-level error pause the Goal without
another turn retry, regardless of the provider's failure message."
  (when-let* ((settled (niwa-goal--settle-accounting fsm)))
    (let ((session (nth 0 settled))
          (goal (nth 1 settled))
          (before (nth 2 settled)))
      (with-current-buffer (plist-get (niwa-engine-info fsm) :buffer)
        (when (eq (niwa-goal-status goal) 'active)
          (let ((reason (niwa-goal--fsm-failure-reason fsm status)))
            (if (and (not (plist-get (niwa-engine-info fsm)
                                    :niwa-retry-exhausted))
                     (not (plist-get (niwa-engine-info fsm)
                                     :niwa-retry-permanent))
                     (niwa-goal--transient-failure-p reason)
                     (< niwa-goal--transient-retries
                        (length niwa-goal--transient-retry-delays)))
                (niwa-diagnostic-message (niwa-diagnostic-scope session)
                         "Goal retrying in %ds after: %s"
                         (nth (cl-incf niwa-goal--transient-retries)
                              (cons 0 niwa-goal--transient-retry-delays))
                         reason)
              (setf (niwa-goal-status goal) 'paused
                    (niwa-goal-reason goal) reason
                    (niwa-goal-reason-diagnostic goal) t)
              (niwa-goal--touch goal)))))
      (niwa-goal--settle-budget fsm session goal before))))

(defun niwa-goal-persist-failure (fsm)
  "Persist Goal failure state after FSM teardown steps."
  (when-let* ((info (niwa-engine-info fsm))
              ((plist-get info :niwa-goal-id))
              (buffer (plist-get info :buffer))
              ((buffer-live-p buffer)))
    (with-current-buffer buffer
      (niwa-goal--persist niwa--session buffer))))

(defun niwa-goal-dispatch-after-turn (fsm &optional succeeded)
  "Schedule Goal continuation after FSM teardown.
A SUCCEEDED root turn leaves any active Goal free to continue; a failed or
interrupted turn continues only the Goal it was attributed to, after the
backoff delay of a retried transient failure."
  (when-let* ((info (niwa-engine-info fsm))
              ((or succeeded (plist-get info :niwa-goal-id)))
              (buffer (plist-get info :buffer)))
    (with-current-buffer buffer
      (niwa-goal-schedule-continuation
       niwa--session buffer nil
       (unless succeeded
         (nth niwa-goal--transient-retries
              (cons 0 niwa-goal--transient-retry-delays)))))))



(provide 'niwa-goal)
;;; niwa-goal.el ends here
