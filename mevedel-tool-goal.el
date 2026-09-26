;;; mevedel-tool-goal.el -- Goal control tool -*- lexical-binding: t -*-

;;; Commentary:

;; Create, inspect, and complete session Goals through the ordinary tool
;; pipeline.  Only the owning root request can control its Goal, and a
;; completion claim is accepted only after an independent verifier passes it.

;;; Code:

(eval-when-compile
  (require 'mevedel-tool-registry)
  (require 'mevedel-structs)
  (require 'subr-x))

(require 'mevedel-goal)

;; `gptel-request'
(declare-function gptel-fsm-info "ext:gptel-request" (cl-x) t)

;; `mevedel-pipeline'
(defvar mevedel-pipeline--handler-active-p)
(defvar mevedel-pipeline--handler-commit)

;; `mevedel-plan'
(declare-function mevedel-plan-read-artifact "mevedel-plan" (session artifact))
(declare-function mevedel-plan-resource-address "mevedel-plan" (relative-path))

;; `mevedel-review'
(declare-function mevedel-review-verify "mevedel-review"
                  (prompt callback &optional workload))
(autoload 'mevedel-review-verify "mevedel-review")

;; `mevedel-structs'
(defvar mevedel--current-request)
(defvar mevedel--session)

;; `mevedel-system'
(declare-function mevedel-system-render-prompt-file
                  "mevedel-system" (relative-path &optional replacements))

;; `mevedel-tools'
(declare-function mevedel-tools--context-for "mevedel-tools" (fsm))
(defvar mevedel-tools--current-fsm)

(defconst mevedel-tool-goal-names '("CreateGoal" "GetGoal" "UpdateGoal")
  "Goal tools whose visibility follows the owning root request.")

(defun mevedel-tool-goal-available-p (name &optional fsm)
  "Return whether Goal tool NAME is available to root request FSM.
Other tool names are unaffected.  Apply this to native and discovered tools."
  (or (not (member name mevedel-tool-goal-names))
      (when-let* ((fsm (or fsm (bound-and-true-p mevedel-tools--current-fsm)))
                  (info (gptel-fsm-info fsm))
                  (session (mevedel-tools--context-for fsm))
                  ((mevedel-session-p session))
                  ((not (mevedel-goal--context-summary-request-p info)))
                  (buffer (plist-get info :buffer))
                  ((buffer-live-p buffer)))
        (with-current-buffer buffer
          (let ((goal (mevedel-session-goal session)))
            (and (not (and mevedel--current-request
                           (or (mevedel-request-ephemeral-p mevedel--current-request)
                               (mevedel-request-cancelled-p mevedel--current-request))))
                 (pcase name
                   ("GetGoal" t)
                   ("CreateGoal"
                    (not (or (and mevedel--current-request
                                  (mevedel-request-directive-uuid mevedel--current-request))
                             (mevedel-goal--start-blocker session))))
                   ("UpdateGoal"
                    (and goal (eq (mevedel-goal-status goal) 'active)
                         (equal (plist-get info :mevedel-goal-id)
                                (mevedel-goal-id goal)))))))))))

(defun mevedel-tool-goal--request (name)
  "Return the current owning root FSM authorized to call NAME."
  (let ((fsm (bound-and-true-p mevedel-tools--current-fsm)))
    (unless (and fsm
                 (eq (plist-get (gptel-fsm-info fsm) :buffer) (current-buffer))
                 mevedel--current-request
                 (eq fsm (mevedel-request-fsm mevedel--current-request))
                 (eq mevedel--session
                     (mevedel-request-session mevedel--current-request))
                 (mevedel-tool-goal-available-p name fsm))
      (error "%s is unavailable outside its owning root request or current Goal state" name))
    fsm))

(defun mevedel-tool-goal--handle-get (_args)
  "Return the current Goal and known usage without changing its state."
  (let* ((fsm (mevedel-tool-goal--request "GetGoal"))
         (info (gptel-fsm-info fsm))
         (goal (mevedel-session-goal mevedel--session))
         plan-error
         (plan-address
          (when-let* ((path (and goal (mevedel-goal-plan-reference goal))))
            (condition-case err
                (mevedel-plan-resource-address path)
              (error (setq plan-error (error-message-string err)) nil))))
         (in-flight (and goal
                         (not (plist-get info :mevedel-goal-accounted))
                         (equal (mevedel-goal-id goal)
                                (plist-get info :mevedel-goal-accounting-id))))
         (used (and goal (+ (mevedel-goal-tokens-used goal)
                            (if in-flight
                                (or (mevedel-goal--known-token-count info) 0)
                              0))))
         (budget (and goal (mevedel-goal-token-budget goal))))
    (list :result
          (json-serialize
           (list :goal
                 (when goal
                   (list :id (mevedel-goal-id goal)
                         :objective (mevedel-goal-objective goal)
                         :status (symbol-name (mevedel-goal-status goal))
                         :reason (mevedel-goal-reason goal)
                         :token_budget budget :tokens_used used
                         :remaining_tokens (and budget (max 0 (- budget used)))
                         :time_used_seconds
                         (+ (mevedel-goal-time-used-seconds goal)
                            (if in-flight
                                (max 0 (round (- (float-time)
                                                 (or (plist-get info :mevedel-goal-started-at)
                                                     (float-time)))))
                              0))
                         :turns_run (mevedel-goal-turns-run goal)
                         :plan_reference plan-address
                         :plan_reference_error plan-error)))
           :null-object nil :false-object :false))))

(defun mevedel-tool-goal--handle-create (args)
  "Create the explicitly requested Goal in ARGS and attribute this turn."
  (mevedel-tool-goal--request "CreateGoal")
  (let ((mevedel-goal-token-budget
         (or (plist-get args :token_budget) mevedel-goal-token-budget)))
    (mevedel-goal-start (plist-get args :objective))
    (mevedel-tool-goal--handle-get nil)))

(defun mevedel-tool-goal-update
    (status summary &optional session captured-id)
  "Set SESSION's active Goal to terminal STATUS.
SUMMARY is required for `blocked'.  CAPTURED-ID must match its Goal identity."
  (setq session (or session mevedel--session)
        status (if (stringp status) (intern (downcase status)) status))
  (let ((goal (and session (mevedel-session-goal session))))
    (unless goal (error "No goal to update"))
    (unless (eq (mevedel-goal-status goal) 'active)
      (error "Goal is %s, not active" (mevedel-goal-status goal)))
    (unless (memq status '(complete blocked))
      (error "Invalid Goal status: %s" status))
    (unless (equal captured-id (mevedel-goal-id goal))
      (error "Goal identity is stale"))
    (when (and (eq status 'blocked)
               (or (not (stringp summary)) (string-blank-p summary)))
      (error "Blocked Goal requires a nonblank summary"))
    (setf (mevedel-goal-status goal) status
          (mevedel-goal-reason goal)
          (and (eq status 'blocked) (string-trim summary))
          (mevedel-goal-updated-at goal) (format-time-string "%FT%T%z"))
    (format "Goal status changed to %s" status)))

(defun mevedel-tool-goal--verification-prompt (session goal)
  "Return the independent completion check for GOAL in SESSION.
GOAL's accepted plan, when it has one, is included verbatim."
  (let ((plan
         (when-let* ((reference (mevedel-goal-plan-reference goal)))
           (condition-case err
               (mevedel-plan-read-artifact
                session
                (list :path reference
                      :hash (plist-get (mevedel-session-plan-metadata session)
                                       :accepted-hash)))
             (error
              (error "Cannot verify completion: accepted plan is unreadable (%s)"
                     (error-message-string err)))))))
    ;; The plan is substituted first so an objective cannot expand into it.
    (mevedel-system-render-prompt-file
     "prompts/goals/completion-verification.md"
     `(("plan-section" . ,(if plan
                              (format "\n<accepted-plan>\n%s\n</accepted-plan>\n" plan)
                            ""))
       ("objective" . ,(mevedel-goal-objective goal))))))

(defun mevedel-tool-goal--verification-failure (outcome)
  "Return the tool result explaining why OUTCOME did not complete the Goal."
  (list :status 'error
        :result
        (format "Goal remains active: completion verification %s. Address the findings, then call UpdateGoal again.\n\n%s"
                (pcase (plist-get outcome :verdict)
                  ('fail "returned FAIL")
                  ('partial "returned PARTIAL")
                  (_ (if (plist-get outcome :verification-rejected)
                         "produced no valid verdict"
                       "did not finish")))
                (or (plist-get outcome :result) (plist-get outcome :message) ""))))

(defun mevedel-tool-goal--handle-update (callback args)
  "Handle UpdateGoal ARGS from the current root request through CALLBACK.
Blocked settles at once.  Completion is accepted only when an independent
verifier passes it; any other verdict returns its report and keeps the Goal
active.  The verifier's token usage is charged to the Goal."
  (let* ((fsm (mevedel-tool-goal--request "UpdateGoal"))
         (session mevedel--session)
         (goal-id (plist-get (gptel-fsm-info fsm) :mevedel-goal-id))
         (status (plist-get args :status))
         ;; Captured now: the pipeline binds these only while this runs.
         (active-p (or mevedel-pipeline--handler-active-p #'always))
         (commit (or mevedel-pipeline--handler-commit #'funcall)))
    (mevedel-goal--assert-mutation-authority session)
    (if (not (equal status "complete"))
        (funcall callback
                 (list :result (mevedel-tool-goal-update
                                status (plist-get args :summary)
                                session goal-id)))
      ;; ponytail: tokens a verifier spent before a user abort are not charged;
      ;; read its FSM from a request canceller if abort accounting matters.
      (mevedel-review-verify
       (mevedel-tool-goal--verification-prompt
        session (mevedel-session-goal session))
       (lambda (outcome)
         (let ((usage (plist-get outcome :usage)))
           (cond
            ((not (funcall active-p))
             (mevedel-goal-charge-tokens fsm usage))
            ((eq (plist-get outcome :verdict) 'pass)
             (funcall
              commit
              (lambda ()
                (let ((reply
                       (condition-case err
                           (list :result (mevedel-tool-goal-update
                                          'complete nil session goal-id))
                         (error
                          (list :status 'error
                                :result (format "Error: %s"
                                                (error-message-string err)))))))
                  (mevedel-goal-charge-tokens fsm usage)
                  (funcall callback reply)))))
            (t
             (mevedel-goal-charge-tokens fsm usage)
             (funcall callback
                      (mevedel-tool-goal--verification-failure outcome))))))
       'goal-review))))

(defun mevedel-tool-goal--register ()
  "Register Goal creation, inspection, and completion tools."
  (mevedel-define-tool
   :name "CreateGoal"
   :description "Create a persistent goal only when explicitly requested."
   :prompt-file "prompts/tools/creategoal.md"
   :handler #'mevedel-tool-goal--handle-create
   :args ((objective string :required "The full concrete objective to pursue.")
          (token_budget integer :optional "Positive token limit, only when explicitly requested."))
   :read-only-p t
   :groups (util))
  (mevedel-define-tool
   :name "GetGoal"
   :description "Inspect the current goal, status, budget, and known usage without resuming it."
   :prompt-file "prompts/tools/getgoal.md"
   :handler #'mevedel-tool-goal--handle-get
   :args nil
   :read-only-p t
   :groups (util))
  (mevedel-define-tool
   :name "UpdateGoal"
   :description "Mark the active goal blocked, or request independently verified completion."
   :prompt-file "prompts/tools/updategoal.md"
   :handler #'mevedel-tool-goal--handle-update
   :async-p t
   :args ((status string :required
                  "Terminal goal status."
                  :enum ["complete" "blocked"])
          (summary string :optional
                   "Required for blocked: name the recurring condition and exact input or external change needed."))
   :read-only-p t
   :groups (util)))

(provide 'mevedel-tool-goal)
;;; mevedel-tool-goal.el ends here
