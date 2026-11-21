;;; niwa-tool-goal.el -- Goal control tool -*- lexical-binding: t -*-

;;; Commentary:

;; Create, inspect, and complete session Goals through the ordinary tool
;; pipeline.  Only the owning root request can control its Goal, and a
;; completion claim is accepted only after an independent verifier passes it,
;; or passes everything it could check (PARTIAL).  Repeated rejected claims
;; block the Goal instead of looping.

;;; Code:

(require 'cl-lib)
(require 'niwa-engine)
(require 'niwa-tool-registry)

(eval-when-compile
  (require 'niwa-structs)
  (require 'subr-x))

(require 'niwa-goal)
(require 'niwa-notify)

;; `gptel-request'

;; `niwa-pipeline'
(defvar niwa-pipeline--handler-active-p)
(defvar niwa-pipeline--handler-commit)

;; `niwa-plan'
(declare-function niwa-plan-read-artifact "niwa-plan" (session artifact))
(declare-function niwa-plan-resource-address "niwa-plan" (relative-path))

;; `niwa-review'
(declare-function niwa-review-verify "niwa-review"
                  (prompt callback &optional workload))
(autoload 'niwa-review-verify "niwa-review")

;; `niwa-structs'
(defvar niwa--current-request)
(defvar niwa--session)

;; `niwa-system'
(declare-function niwa-system-render-prompt-file
                  "niwa-system" (relative-path &optional replacements))

;; `niwa-tools'
(declare-function niwa-tools--context-for "niwa-tools" (fsm))
(defvar niwa-tools--current-engine)

;; `niwa-turn'
(declare-function niwa-turn-end-at-boundary "niwa-turn" (fsm reason))

(defconst niwa-tool-goal-names '("CreateGoal" "GetGoal" "UpdateGoal")
  "Goal tools whose visibility follows the owning root request.")

(defun niwa-tool-goal-available-p (name &optional fsm)
  "Return whether Goal tool NAME is available to root request FSM.
Other tool names are unaffected.  Apply this to native and discovered tools."
  (or (not (member name niwa-tool-goal-names))
      (when-let* ((fsm (or fsm (bound-and-true-p niwa-tools--current-engine)))
                  (info (niwa-engine-info fsm))
                  (session (niwa-tools--context-for fsm))
                  ((niwa-session-p session))
                  ((not (niwa-goal--context-summary-request-p info)))
                  (buffer (plist-get info :buffer))
                  ((buffer-live-p buffer)))
        (with-current-buffer buffer
          (let ((goal (niwa-session-goal session)))
            (and (not (and niwa--current-request
                           (or (niwa-request-ephemeral-p niwa--current-request)
                               (niwa-request-cancelled-p niwa--current-request))))
                 (pcase name
                   ("GetGoal" t)
                   ("CreateGoal"
                    (not (niwa-goal--start-blocker session)))
                   ("UpdateGoal"
                    (and goal (eq (niwa-goal-status goal) 'active)
                         (equal (plist-get info :niwa-goal-id)
                                (niwa-goal-id goal)))))))))))

(defun niwa-tool-goal--request (name)
  "Return the current owning root FSM authorized to call NAME."
  (let ((fsm (bound-and-true-p niwa-tools--current-engine)))
    (unless (and fsm
                 (eq (plist-get (niwa-engine-info fsm) :buffer) (current-buffer))
                 niwa--current-request
                 (eq fsm (niwa-engine-owner niwa--current-request))
                 (eq niwa--session
                     (niwa-request-session niwa--current-request))
                 (niwa-tool-goal-available-p name fsm))
      (error "%s is unavailable outside its owning root request or current Goal state" name))
    fsm))

(defun niwa-tool-goal--handle-get (_args)
  "Return the current Goal and known usage without changing its state."
  (let* ((fsm (niwa-tool-goal--request "GetGoal"))
         (info (niwa-engine-info fsm))
         (goal (niwa-session-goal niwa--session))
         plan-error
         (plan-address
          (when-let* ((path (and goal (niwa-goal-plan-reference goal))))
            (condition-case err
                (niwa-plan-resource-address path)
              (error (setq plan-error (error-message-string err)) nil))))
         (in-flight (and goal
                         (not (plist-get info :niwa-goal-accounted))
                         (equal (niwa-goal-id goal)
                                (plist-get info :niwa-goal-accounting-id))))
         (used (and goal (+ (niwa-goal-tokens-used goal)
                            (if in-flight
                                (or (niwa-goal--known-token-count info) 0)
                              0))))
         (incomplete (and goal
                          (or (niwa-goal-tokens-incomplete-p goal)
                              (and in-flight (niwa-goal--native-usage-incomplete-p info)))))
         (budget (and goal (niwa-goal-token-budget goal))))
    (list :result
          (json-serialize
           (list :goal
                 (when goal
                   (nconc
                    (list :id (niwa-goal-id goal)
                          :objective (niwa-goal-objective goal)
                          :status (symbol-name (niwa-goal-status goal))
                          :reason (niwa-goal-reason goal)
                          :token_budget budget :tokens_used (unless incomplete used))
                    ;; Complete usage has no separate lower bound to report.
                    (when incomplete (list :known_tokens_used used))
                    (list :remaining_tokens (and budget (not incomplete) (max 0 (- budget used)))
                          :time_used_seconds
                          (+ (niwa-goal-time-used-seconds goal)
                             (if in-flight
                                 (max 0 (round (- (float-time)
                                                  (or (plist-get info :niwa-goal-started-at)
                                                      (float-time)))))
                               0))
                          :turns_run (niwa-goal-turns-run goal)
                          :plan_reference plan-address
                          :plan_reference_error plan-error))))
           :null-object nil :false-object :false))))

(defun niwa-tool-goal--handle-create (args)
  "Create the explicitly requested Goal in ARGS and attribute this turn."
  (niwa-tool-goal--request "CreateGoal")
  (let ((niwa-goal-token-budget
         (or (plist-get args :token_budget) niwa-goal-token-budget)))
    (niwa-goal-start (plist-get args :objective))
    (niwa-tool-goal--handle-get nil)))

(defun niwa-tool-goal-update
    (status summary &optional session captured-id)
  "Set SESSION's active Goal to terminal STATUS.
SUMMARY is required for `blocked' and an optional note for `complete'.
CAPTURED-ID must match its Goal identity."
  (setq session (or session niwa--session)
        status (if (stringp status) (intern (downcase status)) status))
  (let ((goal (and session (niwa-session-goal session))))
    (unless goal (error "No goal to update"))
    (unless (eq (niwa-goal-status goal) 'active)
      (error "Goal is %s, not active" (niwa-goal-status goal)))
    (unless (memq status '(complete blocked))
      (error "Invalid Goal status: %s" status))
    (unless (equal captured-id (niwa-goal-id goal))
      (error "Goal identity is stale"))
    (when (and (eq status 'blocked)
               (or (not (stringp summary)) (string-blank-p summary)))
      (error "Blocked Goal requires a nonblank summary"))
    (setf (niwa-goal-status goal) status
          (niwa-goal-reason-diagnostic goal) nil
          (niwa-goal-reason goal)
          (and (stringp summary) (not (string-blank-p summary))
               (string-trim summary))
          (niwa-goal-updated-at goal) (format-time-string "%FT%T%z"))
    (niwa-notify session 'goal
                    (format "Goal %s: %s" status (niwa-goal-objective goal)))
    (format "Goal status changed to %s" status)))

(defun niwa-tool-goal--verification-prompt (session goal)
  "Return the independent completion check for GOAL in SESSION.
GOAL's accepted plan, when it has one, is included verbatim."
  (let ((plan
         (when-let* ((reference (niwa-goal-plan-reference goal)))
           (condition-case err
               (niwa-plan-read-artifact
                session
                (list :path reference
                      :hash (plist-get (niwa-session-plan-metadata session)
                                       :accepted-hash)))
             (error
              (error "Cannot verify completion: accepted plan is unreadable (%s)"
                     (error-message-string err)))))))
    ;; The plan is substituted first so an objective cannot expand into it.
    (niwa-system-render-prompt-file
     "prompts/goals/completion-verification.md"
     `(("plan-section" . ,(if plan
                              (format "\n<accepted-plan>\n%s\n</accepted-plan>\n" plan)
                            ""))
       ("objective" . ,(niwa-goal-objective goal))))))

(defun niwa-tool-goal--verification-failure (outcome)
  "Return the tool result explaining why OUTCOME did not complete the Goal."
  (list :status 'error
        :result
        (format "Goal remains active: completion verification %s. This turn ends here; the next Goal turn addresses the findings, then calls UpdateGoal again.\n\n%s"
                (niwa-tool-goal--rejection-label outcome)
                (niwa-tool-goal--report outcome))))

(defun niwa-tool-goal--rejection-label (outcome)
  "Return how rejected verification OUTCOME ended, for a tool result."
  (cond ((eq (plist-get outcome :verdict) 'fail) "returned FAIL")
        ((plist-get outcome :verification-rejected) "produced no valid verdict")
        (t "did not finish")))

(defun niwa-tool-goal--report (outcome)
  "Return verification OUTCOME's report text."
  (or (plist-get outcome :result) (plist-get outcome :message) ""))

(defun niwa-tool-goal--handle-update (callback args)
  "Handle UpdateGoal ARGS from the current root request through CALLBACK.
Blocked settles at once.  Completion is accepted when an independent
verifier passes it, or passes all it could check (PARTIAL).  Any other
outcome rejects the claim: it returns the report, keeps the Goal active, and
ends the turn at this tool boundary so the rejected attempt settles as its
own Goal turn before continuation.  The rejection that reaches
`niwa-goal-max-rejected-completions' blocks the Goal instead."
  (let* ((fsm (niwa-tool-goal--request "UpdateGoal"))
         (session niwa--session)
         (buffer (current-buffer))
         (goal-id (plist-get (niwa-engine-info fsm) :niwa-goal-id))
         (status (plist-get args :status))
         ;; Captured now: the pipeline binds these only while this runs.
         (active-p (or niwa-pipeline--handler-active-p #'always))
         (commit (or niwa-pipeline--handler-commit #'funcall)))
    (niwa-goal--assert-mutation-authority session)
    (if (not (equal status "complete"))
        (funcall callback
                 (list :result (niwa-tool-goal-update
                                status (plist-get args :summary)
                                session goal-id)))
      ;; The verifier charges its own usage to the Goal as it runs, like
      ;; any agent started from a Goal turn.
      (niwa-review-verify
       (niwa-tool-goal--verification-prompt
        session (niwa-session-goal session))
       (lambda (outcome)
         (cl-labels
             ((settle (status summary reply)
                ;; REPLY formats the result once the status change succeeded.
                (funcall
                 commit
                 (lambda ()
                   (funcall
                    callback
                    (condition-case err
                        (funcall reply (niwa-tool-goal-update
                                        status summary session goal-id))
                      (error
                       (list :status 'error
                             :result (format "Error: %s"
                                             (error-message-string err))))))))))
           (when (funcall active-p)
             (pcase (plist-get outcome :verdict)
               ('pass
                (settle 'complete nil (lambda (changed) (list :result changed))))
               ;; PARTIAL means every requirement the verifier could check
               ;; holds; only an environmental limitation stopped the rest,
               ;; which another attempt cannot change.
               ('partial
                (settle 'complete
                        (concat "Completed with PARTIAL verification: an "
                                "environmental limitation kept the verifier "
                                "from checking some requirements; its report "
                                "is in the transcript.")
                        (lambda (changed)
                          (list :result
                                (format "%s.\n\n%s" changed
                                        (niwa-tool-goal--report outcome))))))
               (_
                (niwa-turn-end-at-boundary fsm 'goal-verification-failed)
                (let ((count (niwa-goal-record-rejected-completion buffer)))
                  (if (< count niwa-goal-max-rejected-completions)
                      (funcall callback
                               (niwa-tool-goal--verification-failure outcome))
                    (let ((summary
                           (format "Completion verification rejected %d consecutive claims (last %s)."
                                   count (niwa-tool-goal--rejection-label outcome))))
                      (settle 'blocked summary
                              (lambda (_changed)
                                (list :status 'error
                                      :result
                                      (format "Goal blocked: %s The user decides how to continue.\n\n%s"
                                              summary
                                              (niwa-tool-goal--report outcome)))))))))))))
       'goal-review))))

(defun niwa-tool-goal--register ()
  "Register Goal creation, inspection, and completion tools."
  (niwa-define-tool
   :name "CreateGoal"
   :description "Create a persistent goal only when explicitly requested."
   :prompt-file "prompts/tools/creategoal.md"
   :handler #'niwa-tool-goal--handle-create
   :args ((objective string :required "The full concrete objective to pursue.")
          (token_budget integer :optional "Positive token limit, only when explicitly requested."))
   :read-only-p t
   :groups (util))
  (niwa-define-tool
   :name "GetGoal"
   :description "Inspect the current goal, status, budget, and known usage without resuming it."
   :prompt-file "prompts/tools/getgoal.md"
   :handler #'niwa-tool-goal--handle-get
   :args nil
   :read-only-p t
   :groups (util))
  (niwa-define-tool
   :name "UpdateGoal"
   :description "Mark the active goal blocked, or request independently verified completion."
   :prompt-file "prompts/tools/updategoal.md"
   :handler #'niwa-tool-goal--handle-update
   :async-p t
   :args ((status string :required
                  "Terminal goal status."
                  :enum ["complete" "blocked"])
          (summary string :optional
                   "Required for blocked: name the recurring condition and exact input or external change needed."))
   :read-only-p t
   :groups (util)))

(provide 'niwa-tool-goal)
;;; niwa-tool-goal.el ends here
