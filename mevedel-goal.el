;;; mevedel-goal.el -- Durable Goal continuation controller -*- lexical-binding: t -*-

;;; Commentary:

;; Owns the phase-free Goal record, request-local context, root-turn
;; attribution, deterministic idle continuation, and the shared single Plan
;; approval interaction.  Planning and review are ordinary conversation work;
;; only UpdateGoal may mark an active Goal complete or blocked.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(eval-when-compile
  (require 'mevedel-structs))

;; `gptel-request'
(declare-function gptel-fsm-info "ext:gptel-request" (cl-x) t)

;; `mevedel-agent-control'
(declare-function mevedel-agent-control-steer-user
                  "mevedel-agent-control"
                  (session message &optional before-wake metadata))

;; `mevedel-chat'
(declare-function mevedel--submit-generated-turn
                  "mevedel-chat" (prompt &optional display-text
                                          prompt-submission))

;; `mevedel-interaction-prompt'
(declare-function mevedel--prompt--settle
                  "mevedel-interaction-prompt" (overlay outcome))

;; `mevedel-plan'
(declare-function mevedel-plan-hash "mevedel-plan" (plan-markdown))

;; `mevedel-queue'
(declare-function mevedel-queue--current-session "mevedel-queue" ())
(declare-function mevedel-queue--unregister-entry-interaction
                  "mevedel-queue" (entry))

;; `mevedel-session-persistence'
(declare-function mevedel-session-persistence-save
                  "mevedel-session-persistence" (session buffer))

;; `mevedel-system'
(declare-function mevedel-system-render-prompt-file
                  "mevedel-system" (relative-path &optional replacements))

;; `mevedel-telemetry'
(declare-function mevedel-telemetry-record
                  "mevedel-telemetry" (session event &rest props))

;; `mevedel-view-composer'
(declare-function mevedel-view--run-queued-user-message-drain
                  "mevedel-view-composer" (data-buffer))

;; `mevedel-tools'
(declare-function mevedel-tools--buffer-local-agent-invocation
                  "mevedel-tools" (buffer))

;; `mevedel-view-interaction'
(declare-function mevedel-view-interaction-pending-p
                  "mevedel-view-interaction" (&optional view-buffer))
(defvar mevedel--view-buffer)

;; `mevedel-structs'
(defvar mevedel--current-request)
(defvar mevedel--session)


;;
;;; Settings

(defcustom mevedel-goal-token-budget nil
  "Default token budget for new Goals, or nil for no limit."
  :type '(choice (const :tag "Unlimited" nil)
                 (integer :tag "Token budget"))
  :group 'mevedel)

(make-variable-buffer-local 'mevedel-goal-token-budget)

(defcustom mevedel-goal-max-transient-retries 1
  "Number of automatic retries after a transient Goal turn failure."
  :type 'natnum
  :group 'mevedel)

(defconst mevedel-goal--continuation-trigger
  "Continue working toward the active Goal."
  "Ordinary user-role trigger used for initial and automatic Goal turns.")

(defvar-local mevedel-goal--transient-retries 0
  "Transient Goal failures retried since the last successful turn.")


;;
;;; Durable record

(defun mevedel-goal--new-id ()
  "Return a fresh versioned Goal identifier."
  (format "%s-%06x" (format-time-string "%Y%m%d-%H%M%S")
          (random #x1000000)))

(defun mevedel-goal--validate-objective (objective)
  "Return normalized non-empty OBJECTIVE or signal `user-error'."
  (unless (and (stringp objective) (not (string-blank-p objective)))
    (user-error "Goal objective must not be blank"))
  (string-trim objective))

(defun mevedel-goal--valid-plan-reference-p (reference)
  "Return non-nil when REFERENCE is a normalized relative path."
  (or (null reference)
      (and (stringp reference)
           (not (string-empty-p reference))
           (not (file-name-absolute-p reference))
           (equal reference
                  (file-relative-name (expand-file-name reference "/") "/"))
           (not (string-prefix-p "../" reference))
           (not (equal reference "..")))))

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
  (unless (and (or (null mevedel-goal-token-budget)
                   (and (integerp mevedel-goal-token-budget)
                        (> mevedel-goal-token-budget 0))))
    (error "Goal token budget must be a positive integer or nil"))
  (let* ((now (format-time-string "%FT%T%z"))
         (goal (mevedel-goal--create
                :id (or id (mevedel-goal--new-id))
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
    (require 'mevedel-session-persistence)
    (mevedel-session-persistence-save session (current-buffer))
    (when (fboundp 'mevedel-telemetry-record)
      (mevedel-telemetry-record session 'goal-start :goal-id (mevedel-goal-id goal)))
    goal))

(defun mevedel-goal--current ()
  "Return the current session Goal or signal `user-error'."
  (unless (bound-and-true-p mevedel--session)
    (user-error "No mevedel session in this buffer"))
  (or (mevedel-session-goal mevedel--session)
      (user-error "No current Goal")))

(defun mevedel-goal-unfinished-p (&optional goal)
  "Return non-nil when GOAL is not complete."
  (and (setq goal (or goal
                      (and (bound-and-true-p mevedel--session)
                           (mevedel-session-goal mevedel--session))))
       (not (eq (mevedel-goal-status goal) 'complete))))

(defun mevedel-goal-description (&optional goal)
  "Return a compact user-facing description of GOAL."
  (let ((goal (or goal (mevedel-goal--current))))
    (format "%s — %s; %d turns; %d/%s tokens"
            (mevedel-goal-objective goal)
            (mevedel-goal-status goal)
            (mevedel-goal-turns-run goal)
            (mevedel-goal-tokens-used goal)
            (or (mevedel-goal-token-budget goal) "unbounded"))))

(defun mevedel-goal--touch (goal)
  "Update GOAL's modification timestamp."
  (setf (mevedel-goal-updated-at goal) (format-time-string "%FT%T%z"))
  goal)

(defun mevedel-goal--persist (session buffer)
  "Persist SESSION from BUFFER when both remain live."
  (when (and session (buffer-live-p buffer))
    (with-current-buffer buffer
      (require 'mevedel-session-persistence)
      (mevedel-session-persistence-save session buffer))))


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
  "Return GOAL's validated accepted-plan path in SESSION, or nil."
  (when-let* ((reference (mevedel-goal-plan-reference goal)))
    (unless (mevedel-goal--valid-plan-reference-p reference)
      (mevedel-goal--pause-for-integrity
       goal session "Accepted-plan reference is invalid"))
    (let* ((save-path (mevedel-session-save-path session))
           (metadata (mevedel-session-plan-metadata session))
           (accepted (plist-get metadata :accepted-path))
           (expected-hash (plist-get metadata :accepted-hash))
           (path (and save-path (expand-file-name reference save-path))))
      (unless (and save-path
                   (equal reference accepted)
                   (stringp expected-hash)
                   path
                   (file-in-directory-p path save-path)
                   (file-regular-p path))
        (mevedel-goal--pause-for-integrity
         goal session
         "Accepted-plan artifact is missing or no longer owned by this session"))
      (with-temp-buffer
        (insert-file-contents path)
        (unless (equal expected-hash (mevedel-plan-hash (buffer-string)))
          (mevedel-goal--pause-for-integrity
           goal session
           "Accepted-plan artifact no longer matches its accepted hash")))
      path)))

(defun mevedel-goal-active-context (session)
  "Render request-local active Goal context for SESSION, or nil."
  (when-let* ((goal (mevedel-session-goal session))
              ((eq (mevedel-goal-status goal) 'active)))
    (let* ((budget (mevedel-goal-token-budget goal))
           (used (mevedel-goal-tokens-used goal))
           (plan-path (mevedel-goal--resolve-plan-reference goal session)))
      (require 'mevedel-system)
      (mevedel-system-render-prompt-file
       "prompts/goals/active-context.md"
       `(("objective" . ,(mevedel-goal-objective goal))
         ("tokens-used" . ,(number-to-string used))
         ("token-budget" . ,(if budget (number-to-string budget) "none"))
         ("tokens-remaining" . ,(if budget
                                     (number-to-string (max 0 (- budget used)))
                                   "unbounded"))
         ("turns-run" . ,(number-to-string (mevedel-goal-turns-run goal)))
         ("plan-reference-line" .
          ,(if plan-path
               (format "Accepted plan: %s. Its outcomes, constraints, and achievement criteria are binding except where amended by the current objective; its implementation mechanics are revisable."
                       plan-path)
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
     ((not goal) 'no-goal)
     ((not (eq (mevedel-goal-status goal) 'active)) 'inactive)
     ((and (buffer-live-p buffer)
           (buffer-local-value 'mevedel--current-request buffer))
      'request)
     ((mevedel-session-queued-user-messages session)
      (run-at-time 0 nil #'mevedel-view--run-queued-user-message-drain buffer)
      'queued-user-message)
     ((mevedel-goal--pending-interaction-p session) 'interaction)
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
  "Run a scheduled continuation for SESSION in BUFFER."
  (when (and (buffer-live-p buffer)
             (eq session (buffer-local-value 'mevedel--session buffer)))
    (with-current-buffer buffer
      (mevedel-goal-continue-if-idle session buffer prompt-submission))))

(defun mevedel-goal--schedule-continuation
    (&optional session buffer prompt-submission)
  "Schedule SESSION's Goal continuation check after the current command."
  (let ((session (or session mevedel--session))
        (buffer (or buffer (current-buffer))))
    (when (and session (buffer-live-p buffer))
      (run-at-time 0 nil #'mevedel-goal--scheduled-continuation
                   session buffer prompt-submission))))

(defun mevedel-goal-start (objective &optional prompt-submission)
  "Start a Goal for OBJECTIVE and schedule its first ordinary turn."
  (let ((current (and (bound-and-true-p mevedel--session)
                      (mevedel-session-goal mevedel--session))))
    (unless (bound-and-true-p mevedel--session)
      (user-error "No mevedel session in this buffer"))
    (when (and current (not (eq (mevedel-goal-status current) 'complete)))
      (user-error "Finish or clear the current Goal first"))
    (when (plist-get (mevedel-session-plan-metadata mevedel--session)
                     :implementation-retry)
      (user-error "Finish or cancel the accepted Plan implementation first"))
    (let ((goal (mevedel-goal-create objective mevedel--session)))
      (mevedel-goal--schedule-continuation
       mevedel--session (current-buffer) prompt-submission)
      goal)))

(defun mevedel-goal-pause ()
  "Pause the current Goal without interrupting an in-flight request."
  (interactive)
  (let ((goal (mevedel-goal--current)))
    (unless (eq (mevedel-goal-status goal) 'active)
      (user-error "Goal is not active"))
    (setf (mevedel-goal-status goal) 'paused
          (mevedel-goal-reason goal) "paused by user")
    (mevedel-goal--touch goal)
    (mevedel-goal--persist mevedel--session (current-buffer))
    goal))

(defun mevedel-goal-resume (&optional steering)
  "Resume the current Goal, optionally with ordinary STEERING text."
  (interactive)
  (let ((goal (mevedel-goal--current)))
    (when (eq (mevedel-goal-status goal) 'complete)
      (user-error "Completed Goal cannot be resumed"))
    (setf (mevedel-goal-status goal) 'active
          (mevedel-goal-reason goal) nil)
    (mevedel-goal--touch goal)
    (when (and (stringp steering) (not (string-blank-p steering)))
      (setf (mevedel-session-queued-user-messages mevedel--session)
            (append (mevedel-session-queued-user-messages mevedel--session)
                    (list (list :input (string-trim steering))))))
    (mevedel-goal--persist mevedel--session (current-buffer))
    (mevedel-goal--schedule-continuation mevedel--session (current-buffer))
    goal))

(defun mevedel-goal-clear ()
  "Clear the current Goal while retaining transcript and artifacts."
  (interactive)
  (mevedel-goal--current)
  (setf (mevedel-session-goal mevedel--session) nil)
  (setq mevedel-goal--transient-retries 0)
  (mevedel-goal--persist mevedel--session (current-buffer))
  nil)

(defun mevedel-goal-edit (objective)
  "Replace the current Goal OBJECTIVE and rotate its identity."
  (interactive "sNew Goal objective: ")
  (setq objective (mevedel-goal--validate-objective objective))
  (let* ((session mevedel--session)
         (goal (mevedel-goal--current)))
    (setf (mevedel-goal-id goal) (mevedel-goal--new-id)
          (mevedel-goal-objective goal) objective)
    (mevedel-goal--touch goal)
    (mevedel-session-enqueue-pending-reminder
     session
     (format "Goal objective updated to: %s. The revised objective has highest authority; any accepted plan remains binding only where consistent."
             objective))
    (mevedel-goal--persist session (current-buffer))
    (when (and (eq (mevedel-goal-status goal) 'active)
               mevedel--current-request)
      (require 'mevedel-agent-control)
      (ignore-errors
        (mevedel-agent-control-steer-user
         session (mevedel-goal-active-context session))))
    (when (eq (mevedel-goal-status goal) 'active)
      (mevedel-goal--schedule-continuation session (current-buffer)))
    goal))

(defun mevedel-goal-pause-runtime-failure (buffer reason)
  "Pause BUFFER's active Goal with runtime failure REASON."
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

(defun mevedel-goal--compaction-request-p (info)
  "Return non-nil when INFO describes a compaction request."
  (when-let* ((context (plist-get info :context)))
    (and (listp context) (plist-get context :mevedel-compaction))))

(defun mevedel-goal-capture-request (fsm)
  "Capture active Goal attribution on root request FSM exactly once."
  (let* ((info (gptel-fsm-info fsm))
         (buffer (plist-get info :buffer)))
    (when (and (not (plist-member info :mevedel-goal-id))
               (not (or (plist-get info :mevedel-agent-invocation)
                        (mevedel-tools--buffer-local-agent-invocation buffer)))
               (not (mevedel-goal--compaction-request-p info))
               (buffer-live-p buffer))
      (with-current-buffer buffer
        (when-let* ((session mevedel--session)
                    (goal (mevedel-session-goal session))
                    ((eq (mevedel-goal-status goal) 'active)))
          (let ((plan-path (mevedel-goal--resolve-plan-reference goal session)))
            (plist-put info :mevedel-goal-id (mevedel-goal-id goal))
            (plist-put info :mevedel-goal-started-at (float-time))
            (plist-put info :mevedel-goal-estimated-tokens
                       (max 1 (/ (+ (length (prin1-to-string
                                            (plist-get info :data))) 3)
                                 4)))
            (when (and plan-path mevedel--current-request)
              (setf (mevedel-request-goal-plan-read-path
                     mevedel--current-request)
                    plan-path))
            (setf (gptel-fsm-info fsm) info)))))))

(defun mevedel-goal--request-token-count (info)
  "Return normalized input plus output tokens for request INFO."
  (let ((usage (or (plist-get info :tokens-full)
                   (plist-get info :tokens))))
    (or (and (listp usage)
             (let ((count (+ (or (plist-get usage :input) 0)
                             (or (plist-get usage :output) 0))))
               (and (> count 0) count)))
        (plist-get info :mevedel-goal-estimated-tokens)
        1)))

(defun mevedel-goal--settle-accounting (fsm)
  "Charge FSM to its attributed Goal and return that Goal, or nil."
  (let* ((info (gptel-fsm-info fsm))
         (captured-id (plist-get info :mevedel-goal-id))
         (buffer (plist-get info :buffer)))
    (when (and captured-id (buffer-live-p buffer))
      (with-current-buffer buffer
        (when-let* ((goal (and mevedel--session
                               (mevedel-session-goal mevedel--session))))
          (cl-incf (mevedel-goal-tokens-used goal)
                   (mevedel-goal--request-token-count info))
          (cl-incf (mevedel-goal-time-used-seconds goal)
                   (max 0 (round (- (float-time)
                                    (or (plist-get info
                                                   :mevedel-goal-started-at)
                                        (float-time))))))
          (cl-incf (mevedel-goal-turns-run goal))
          (mevedel-goal--touch goal)
          (when (fboundp 'mevedel-telemetry-record)
            (mevedel-telemetry-record
             mevedel--session 'goal-turn-settled
             :captured-goal-id captured-id
             :goal-id (mevedel-goal-id goal)
             :tokens-used (mevedel-goal-tokens-used goal)
             :turns-run (mevedel-goal-turns-run goal)))
          goal)))))

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
           "network" "unavailable" "502" "503" "504"))
   (downcase reason)))

(defun mevedel-goal-settle-turn (fsm)
  "Charge successful Goal turn FSM."
  (when (mevedel-goal--settle-accounting fsm)
    (setq mevedel-goal--transient-retries 0)))

(defun mevedel-goal-settle-failure (fsm &optional status)
  "Charge failed Goal turn FSM and pause or retain it for one retry."
  (when-let* ((goal (mevedel-goal--settle-accounting fsm)))
    (when (eq (mevedel-goal-status goal) 'active)
      (let ((reason (mevedel-goal--fsm-failure-reason fsm status)))
        (if (and (mevedel-goal--transient-failure-p reason)
                 (< mevedel-goal--transient-retries
                    mevedel-goal-max-transient-retries))
            (cl-incf mevedel-goal--transient-retries)
          (setf (mevedel-goal-status goal) 'paused
                (mevedel-goal-reason goal) reason)
          (mevedel-goal--touch goal))))))

(defun mevedel-goal-persist-failure (fsm)
  "Persist Goal failure state after FSM teardown steps."
  (when-let* ((info (gptel-fsm-info fsm))
              ((plist-get info :mevedel-goal-id))
              (buffer (plist-get info :buffer))
              ((buffer-live-p buffer)))
    (with-current-buffer buffer
      (mevedel-goal--persist mevedel--session buffer))))

(defun mevedel-goal-dispatch-after-turn (fsm)
  "Schedule Goal continuation after successful FSM teardown."
  (when-let* ((info (gptel-fsm-info fsm))
              ((plist-get info :mevedel-goal-id))
              (buffer (plist-get info :buffer)))
    (with-current-buffer buffer
      (mevedel-goal--schedule-continuation mevedel--session buffer))))

(defun mevedel-goal-dispatch-after-failure (fsm)
  "Schedule a permitted Goal retry after failed FSM teardown."
  (mevedel-goal-dispatch-after-turn fsm))


;;
;;; Shared single Plan approval interaction

(defun mevedel-plan-approval--current-session ()
  "Resolve the session that owns the pending Plan approval."
  (mevedel-queue--current-session))

(defun mevedel-plan-approval--deliver (entry outcome phase &optional retain)
  "Deliver OUTCOME to ENTRY during PHASE.
When RETAIN is non-nil, keep ENTRY's interaction after a callback error."
  (condition-case err
      (progn
        (when-let* ((callback (plist-get entry :callback)))
          (funcall callback outcome))
        (mevedel-queue--unregister-entry-interaction entry)
        t)
    (error
     (display-warning 'mevedel
                      (format "Plan approval %s callback error: %S" phase err)
                      :warning)
     (unless retain (mevedel-queue--unregister-entry-interaction entry))
     nil)))

(defun mevedel-plan-approval-present (entry &optional session)
  "Replace SESSION's pending Plan approval with ENTRY and render it."
  (let ((session (or session (mevedel-plan-approval--current-session))))
    (if (not session)
        (mevedel-plan-approval--deliver entry 'aborted "no-session")
      (setq entry (plist-put (copy-sequence entry) :session session))
      (when-let* ((previous (mevedel-session-pending-plan-approval session)))
        (setf (mevedel-session-pending-plan-approval session) nil)
        (mevedel-plan-approval--deliver previous 'superseded "supersede"))
      (setf (mevedel-session-pending-plan-approval session) entry)
      (mevedel-plan-approval-render session))))

(defun mevedel-plan-approval-render (&optional session)
  "Render SESSION's pending Plan approval."
  (when-let* ((session (or session (mevedel-plan-approval--current-session)))
              (entry (mevedel-session-pending-plan-approval session)))
    (condition-case err
        (if-let* ((renderer (plist-get entry :renderer)))
            (funcall renderer entry)
          (error "Plan approval has no renderer"))
      (error
       (display-warning 'mevedel
                        (format "Plan approval render error: %S" err)
                        :warning)
       (mevedel-plan-approval-abort session)))))

(defun mevedel-plan-approval-settle (entry outcome)
  "Settle pending Plan approval ENTRY with OUTCOME."
  (let* ((session (plist-get entry :session))
         (pending (and session
                       (mevedel-session-pending-plan-approval session))))
    (when (and (proper-list-p outcome)
               (plist-get outcome :accept)
               (mevedel-session-queued-user-messages session))
      (user-error "Resolve queued messages before implementing the plan"))
    (if (not (eq entry pending))
        (display-warning 'mevedel
                         "Plan approval: stale settlement ignored" :warning)
      (when (mevedel-plan-approval--deliver entry outcome "settle" t)
        (when (eq entry (mevedel-session-pending-plan-approval session))
          (setf (mevedel-session-pending-plan-approval session) nil))))))

(defun mevedel-plan-approval-abort (&optional session outcome)
  "Settle SESSION's pending Plan approval with OUTCOME or `aborted'."
  (when-let* ((session (or session (mevedel-plan-approval--current-session)))
              (entry (mevedel-session-pending-plan-approval session)))
    (setf (mevedel-session-pending-plan-approval session) nil)
    (mevedel-plan-approval--deliver entry (or outcome 'aborted) "abort")))


(provide 'mevedel-goal)
;;; mevedel-goal.el ends here
