;;; mevedel-goal.el -- Goal workflow controller -*- lexical-binding: t -*-

;;; Commentary:

;; A session-owned Goal drives supervised or guardian-approved planning,
;; implementation, and evidence-review cycles.  Plans use the lifecycle-neutral
;; artifact operations in `mevedel-plan'.

;;; Code:

(require 'cl-lib)

(eval-when-compile
  (require 'gptel-request)
  (require 'mevedel-plan)
  (require 'mevedel-tool-registry))
(require 'mevedel-queue)
(require 'mevedel-utilities)

;; `gptel'
(declare-function gptel--copy-tool "ext:gptel-request" (cl-x) t)
(declare-function gptel--display-tool-calls "ext:gptel-request"
		  (calls info))
(declare-function gptel--model-name "ext:gptel" (model))
(declare-function gptel--reject-tool-calls "ext:gptel"
		  (&optional calls overlay))
(declare-function gptel-abort "ext:gptel-request" (buffer))
(declare-function gptel-backend-name "ext:gptel" (backend))
(declare-function gptel-fsm-info "ext:gptel-request" (cl-x) t)
(declare-function gptel-fsm-state "ext:gptel-request" (cl-x) t)
(declare-function gptel-request "ext:gptel" (prompt &rest args))
(declare-function gptel-tool-async "ext:gptel-request" (cl-x) t)
(declare-function gptel-tool-function "ext:gptel-request" (cl-x) t)
(declare-function gptel-tool-name "ext:gptel-request" (cl-x) t)
(defvar gptel-backend)
(defvar gptel-model)
(defvar gptel-prompt-transform-functions)
(defvar gptel-reasoning-effort)
(defvar gptel-system-prompt)
(defvar gptel-tools)
(defvar gptel-use-context)
(defvar gptel-use-tools)

;; `mevedel-chat'
(declare-function mevedel--gptel-send-request "mevedel-chat"
		  (&optional model-input))
(declare-function mevedel--implement-plan "mevedel-chat"
		  (action-plist))
(declare-function mevedel--insert-local-user-turn "mevedel-chat"
		  (prompt &optional display-text kind hook-context
			  no-spinner))
(declare-function mevedel--submit-generated-turn "mevedel-chat"
		  (prompt &optional display-text prompt-submission))
(defvar mevedel--current-request)
(defvar mevedel--session)

;; `mevedel-compact'
(declare-function mevedel--compact-token-usage-count "mevedel-compact"
		  (tokens))
(declare-function mevedel--estimate-tokens "mevedel-compact" nil)

;; `mevedel-interaction-prompt'
(declare-function mevedel--prompt--settle "mevedel-interaction-prompt"
		  (overlay outcome))

;; `mevedel-models'
(declare-function mevedel-model-resolve-workload "mevedel-models"
		  (workload &optional explicit-selector
			    explicit-effort))

;; `mevedel-plan-mode'
(declare-function mevedel-plan-mode-exit "mevedel-plan-mode"
		  (&optional session))

;; `mevedel-presets'
(declare-function mevedel-preset-restore-session "mevedel-presets"
		  (session &optional buffer))

;; `mevedel-prompt-submission'
(declare-function mevedel-prompt-submission-commit
		  "mevedel-prompt-submission" (submission))
(declare-function mevedel-prompt-submission-context
		  "mevedel-prompt-submission" (cl-x) t)

;; `mevedel-queue'
(declare-function mevedel-queue--entry-metadata-get "mevedel-queue"
		  (entry key))
(declare-function mevedel-queue--entry-metadata-put "mevedel-queue"
		  (entry key value))
(declare-function mevedel-queue--unregister-entry-interaction
		  "mevedel-queue" (entry))

;; `mevedel-reminders'
(declare-function mevedel-reminders-make-plan-reference
		  "mevedel-reminders" nil)
(declare-function mevedel-session-ensure-reminder "mevedel-reminders"
		  (session reminder))
(declare-function mevedel-session-remove-reminder "mevedel-reminders"
		  (session type))

;; `mevedel-session-persistence'
(declare-function mevedel-session-persistence-ensure-files
		  "mevedel-session-persistence" (session buffer))
(declare-function mevedel-session-persistence-save
		  "mevedel-session-persistence" (session buffer))
(declare-function mevedel-session-persistence-write
		  "mevedel-session-persistence" (path plist))

;; `mevedel-structs'
(declare-function mevedel-goal--create "mevedel-structs" (&rest args))
(declare-function mevedel-goal-approval-policy "mevedel-structs"
		  (cl-x) t)
(declare-function mevedel-goal-checkpoint "mevedel-structs" (cl-x) t)
(declare-function mevedel-goal-continuation-key "mevedel-structs"
		  (cl-x) t)
(declare-function mevedel-goal-current-plan "mevedel-structs" (cl-x) t)
(declare-function mevedel-goal-cycle "mevedel-structs" (cl-x) t)
(declare-function mevedel-goal-cycles "mevedel-structs" (cl-x) t)
(declare-function mevedel-goal-execution-home "mevedel-structs" (cl-x)
		  t)
(declare-function mevedel-goal-id "mevedel-structs" (cl-x) t)
(declare-function mevedel-goal-implementation-context
		  "mevedel-structs" (cl-x) t)
(declare-function mevedel-goal-objective "mevedel-structs" (cl-x) t)
(declare-function mevedel-goal-pause-requested "mevedel-structs"
		  (cl-x) t)
(declare-function mevedel-goal-phase "mevedel-structs" (cl-x) t)
(declare-function mevedel-goal-reason "mevedel-structs" (cl-x) t)
(declare-function mevedel-goal-review-findings "mevedel-structs"
		  (cl-x) t)
(declare-function mevedel-goal-review-summary "mevedel-structs" (cl-x)
		  t)
(declare-function mevedel-goal-status "mevedel-structs" (cl-x) t)
(declare-function mevedel-goal-token-budget "mevedel-structs" (cl-x) t)
(declare-function mevedel-goal-token-usage "mevedel-structs" (cl-x) t)
(declare-function mevedel-request--create "mevedel-structs"
		  (&rest args))
(declare-function mevedel-request-cancel "mevedel-structs"
		  (request &optional abort-plan-approval))
(declare-function mevedel-session-enqueue-pending-reminder
		  "mevedel-structs" (session body))
(declare-function mevedel-session-goal "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-goal-handoff "mevedel-structs"
		  (cl-x) t)
(declare-function mevedel-session-name "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-pending-plan-approval
		  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-pending-reminders "mevedel-structs"
		  (cl-x) t)
(declare-function mevedel-session-permission-mode "mevedel-structs"
		  (cl-x) t)
(declare-function mevedel-session-permission-queue "mevedel-structs"
		  (cl-x) t)
(declare-function mevedel-session-permission-rules "mevedel-structs"
		  (cl-x) t)
(declare-function mevedel-session-plan-metadata "mevedel-structs"
		  (cl-x) t)
(declare-function mevedel-session-preset-name "mevedel-structs" (cl-x)
		  t)
(declare-function mevedel-session-preset-settings "mevedel-structs"
		  (cl-x) t)
(declare-function mevedel-session-queued-user-messages
		  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-save-path "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-session-id "mevedel-structs" (cl-x)
		  t)
(declare-function mevedel-session-working-directory "mevedel-structs"
		  (cl-x) t)

;; `mevedel-system'
(declare-function mevedel-system-render-prompt-file "mevedel-system"
		  (relative-path &optional replacements))

;; `mevedel-telemetry'
(declare-function mevedel-telemetry-finish "mevedel-telemetry"
		  (span &rest props))
(declare-function mevedel-telemetry-record "mevedel-telemetry"
		  (session event &rest props))
(declare-function mevedel-telemetry-start "mevedel-telemetry"
		  (session event &rest props))

;; `mevedel-transcript'
(declare-function mevedel-transcript-segments "mevedel-transcript"
		  (start end))

;; `mevedel-transcript-audit'
(declare-function mevedel--format-hook-audit-record
		  "mevedel-transcript-audit" (record))

;; `mevedel-view'
(declare-function mevedel-view--fontify-as "mevedel-view" (text mode))
(declare-function mevedel-view-rerender "mevedel-view"
		  (&optional buffer))
(defvar mevedel--view-buffer)

;; `mevedel-view-composer'
(declare-function mevedel-view--clear-input "mevedel-view-composer"
		  nil)
(declare-function mevedel-view--input-start "mevedel-view-composer"
		  nil)

;; `mevedel-view-interaction'
(declare-function mevedel-view--interaction-anchor
		  "mevedel-view-interaction" nil)
(declare-function mevedel-view--interaction-register
		  "mevedel-view-interaction" (descriptor))
(declare-function mevedel-view--interaction-target-buffer
		  "mevedel-view-interaction" (&optional data-buffer))
(declare-function mevedel-view-interaction-pending-p
		  "mevedel-view-interaction" (&optional view-buffer))

;; `mevedel-worktree'
(declare-function mevedel-worktree--git-result "mevedel-worktree"
		  (directory &rest args))
(declare-function mevedel-worktree-create-session "mevedel-worktree"
		  (&optional branch purpose clean))

;;
;;; Lifecycle

(defvar mevedel-goal-dispatch-function #'mevedel-goal--dispatch-gptel
  "Function called with PHASE, PROMPT, DISPLAY-TEXT, and prompt submission.")

(defcustom mevedel-goal-guardian-timeout 60
  "Seconds before an automatic Goal guardian request escalates to the user."
  :type 'number
  :group 'mevedel)

(defcustom mevedel-goal-planner-revision-timeout 120
  "Seconds before an automatic planner revision escalates to the user."
  :type 'number
  :group 'mevedel)

(defcustom mevedel-goal-investigation-time-budget 120
  "Seconds allowed for a subagent started during Goal planning or review.
The maximum leaves 30 seconds for terminal delivery within one `WaitAgent'."
  :type '(restricted-sexp
          :tag "Seconds from 1 through 3570"
          :match-alternatives
          ((lambda (value)
             (and (integerp value) (<= 1 value 3570)))))
  :group 'mevedel)

(defcustom mevedel-goal-max-transient-retries 1
  "Maximum automatic retries for a failed read-only Goal request."
  :type 'natnum
  :group 'mevedel)

(defcustom mevedel-goal-token-budget nil
  "Default aggregate token budget for a newly started Goal.
Nil means no budget.  Presets may set this buffer-locally."
  :type '(choice (const :tag "Unlimited" nil) pos-integer)
  :group 'mevedel)

(make-variable-buffer-local 'mevedel-goal-token-budget)

(defconst mevedel-goal--plan-guidance
  "Treat the Goal objective and achievement criteria as the completion contract, followed by authoritative referenced requirements. The accepted plan is an implementation approach, not a competing completion contract. Investigate the current repository state and propose a concise outcome-first plan.

Use this structure when applicable:

## Goal
- State the desired outcome and important non-goals.

## Achievement Criteria
- List observable conditions that prove the Goal is achieved.
- Reference an authoritative PRD, specification, or ticket when it provides the criteria.

## Approach
- Describe the intended method by subsystem or behavior.
- Include implementation details only when needed to resolve ambiguity, material risk, public interfaces, data shape, or architecture.

## Regression Coverage
- List user-visible flows, edge cases, failure scenarios, and intentionally unchanged behavior that tests should cover.

## Validation
- List exact focused test/build commands and other evidence to collect.

## Assumptions
- Record defaults, constraints, compatibility expectations, and unresolved inferences.

Ordinary plans should use the full structure, but omit genuinely inapplicable sections. A concise plan may delegate detail to a clear authoritative PRD, specification, or ticket reference without repeating it."
  "Outcome-first guidance shared by initial and user-requested Goal planning.")

(defcustom mevedel-goal-execution-home 'current
  "Execution home selected for a newly started Goal.
`current' uses the current session checkout.  `worktree' creates one Goal-owned
worktree before implementation."
  :type '(choice (const current) (const worktree))
  :group 'mevedel)

(make-variable-buffer-local 'mevedel-goal-execution-home)

(defcustom mevedel-goal-implementation-context 'full
  "Implementation context selected for a newly started Goal.
`full' preserves the conversation.  `focused' sends only authoritative Goal
and plan context.  Worktree Goals begin focused; supervised approval may
explicitly select full context."
  :type '(choice (const full) (const focused))
  :group 'mevedel)

(make-variable-buffer-local 'mevedel-goal-implementation-context)

(defvar mevedel-goal-guardian-function #'mevedel-goal--guardian-request
  "Function called with GOAL, PLAN, CHAT-BUFFER, and CALLBACK.
CALLBACK receives a normalized guardian decision plist.")

(defvar mevedel-goal-planner-revision-function
  #'mevedel-goal--planner-revision-request
  "Function called with GOAL, PLAN, DECISION, CHAT-BUFFER, and CALLBACK.
CALLBACK receives a plist containing either `:plan' or `:error'.")

(defconst mevedel-goal--review-open-tag "<goal_review>"
  "Opening tag for structured Goal review results.")

(defconst mevedel-goal--review-close-tag "</goal_review>"
  "Closing tag for structured Goal review results.")

(defconst mevedel-goal--guardian-open-tag "<goal_guardian>"
  "Opening tag for structured Goal guardian results.")

(defconst mevedel-goal--guardian-close-tag "</goal_guardian>"
  "Closing tag for structured Goal guardian results.")

(defconst mevedel-goal--max-automatic-revisions 2
  "Maximum planner corrections within one automatic approval boundary.")

(defun mevedel-goal--relative-dir (goal)
  "Return GOAL's artifact directory relative to its session."
  (file-name-concat "goals" (mevedel-goal-id goal)))

(defun mevedel-goal--current-plan-relative-path (goal)
  "Return GOAL's mutable current-plan relative path."
  (file-name-concat (mevedel-goal--relative-dir goal) "current-plan.md"))

(defun mevedel-goal--revision-plan-relative-path (goal revision)
  "Return GOAL's immutable candidate path for REVISION."
  (file-name-concat
   (mevedel-goal--relative-dir goal)
   (format "cycle-%03d-revision-%03d-plan.md"
           (mevedel-goal-cycle goal) revision)))

(defun mevedel-goal--cycle-plan-relative-path (goal)
  "Return GOAL's current immutable cycle-plan relative path."
  (file-name-concat
   (mevedel-goal--relative-dir goal)
   (format "cycle-%03d-plan.md" (mevedel-goal-cycle goal))))

(defun mevedel-goal-cycle-record (goal)
  "Return GOAL's current cycle record."
  (cl-find (mevedel-goal-cycle goal) (mevedel-goal-cycles goal)
           :key (lambda (record) (plist-get record :cycle))))

(defun mevedel-goal-latest-provider (goal workload)
  "Return GOAL's latest recorded provider policy for WORKLOAD."
  (when-let* ((cycle
               (cl-find-if
                (lambda (record)
                  (alist-get workload (plist-get record :providers)))
                (mevedel-goal-cycles goal) :from-end t)))
    (alist-get workload (plist-get cycle :providers))))

(defun mevedel-goal--cycle-put (goal key value)
  "Set KEY to VALUE in GOAL's current cycle record."
  (let* ((number (mevedel-goal-cycle goal))
         (records (copy-tree (or (mevedel-goal-cycles goal) nil)))
         (record (or (cl-find number records
                              :key (lambda (item) (plist-get item :cycle)))
                     (list :cycle number
                           :started-at (format-time-string "%FT%T%z")))))
    (setq record (plist-put record key value))
    (setq records
          (cons record
                (cl-remove number records
                           :key (lambda (item) (plist-get item :cycle)))))
    (setf (mevedel-goal-cycles goal)
           (sort records (lambda (a b)
                          (< (plist-get a :cycle) (plist-get b :cycle)))))
    record))

(defun mevedel-goal--record-plan-revision
    (goal session chat-buffer revision &rest properties)
  "Update REVISION audit PROPERTIES for GOAL in SESSION and CHAT-BUFFER."
  (let* ((cycle (mevedel-goal-cycle-record goal))
         (records (copy-tree (plist-get cycle :plan-revisions)))
         (record (or (cl-find revision records
                              :key (lambda (item)
                                     (plist-get item :revision)))
                     (list :revision revision))))
    (while properties
      (setq record
            (plist-put record (pop properties) (pop properties))))
    (setq records
          (sort
           (append
            (cl-remove revision records
                       :key (lambda (item)
                              (plist-get item :revision)))
            (list record))
           (lambda (left right)
             (< (plist-get left :revision)
                (plist-get right :revision)))))
    (mevedel-goal--cycle-put goal :plan-revisions records)
    (mevedel-goal--persist-cycle-index goal session chat-buffer)
    (require 'mevedel-session-persistence)
    (mevedel-session-persistence-save session chat-buffer)
    record))

(defun mevedel-goal--persist-cycle-index (goal session buffer)
  "Persist GOAL's lightweight cycle index for SESSION from BUFFER."
  (require 'mevedel-session-persistence)
  (let* ((save-path (or (mevedel-session-save-path session)
                        (mevedel-session-persistence-ensure-files
                         session buffer)))
         (path (file-name-concat save-path
                                 (mevedel-goal--relative-dir goal)
                                 "cycles.el")))
    (make-directory (file-name-directory path) t)
    (mevedel-session-persistence-write path (mevedel-goal-cycles goal))
    path))

(defun mevedel-goal--record-phase-policy (workload policy)
  "Record resolved POLICY for WORKLOAD in the active Goal cycle."
  (when-let* ((session (and (bound-and-true-p mevedel--session)
                            mevedel--session))
              (goal (mevedel-session-goal session))
              ((eq (mevedel-goal-status goal) 'active)))
    (let* ((record (mevedel-goal-cycle-record goal))
           (providers (copy-tree (plist-get record :providers)))
           (backend (plist-get policy :backend))
           (model (plist-get policy :model)))
      (setf (alist-get workload providers)
            (list :provider
                  (if backend
                      (format "%s:%s" (gptel-backend-name backend)
                              (gptel--model-name model))
                    (format "%s" model))
                  :effort (plist-get policy :effort)
                  :at (format-time-string "%FT%T%z")))
      (mevedel-goal--cycle-put goal :providers providers)
      (mevedel-goal--persist-cycle-index goal session (current-buffer)))))

(defun mevedel-goal--plan-metadata-put (session key value)
  "Set plan artifact metadata KEY to VALUE in SESSION."
  (let ((metadata (copy-sequence (or (mevedel-session-plan-metadata session)
                                     nil))))
    (setq metadata (plist-put metadata key value))
    (setf (mevedel-session-plan-metadata session) metadata)
    metadata))

(defun mevedel-goal--mark-rejected (session)
  "Mark SESSION's current plan as rejected."
  (mevedel-goal--plan-metadata-put session :status 'rejected)
  (mevedel-goal--plan-metadata-put session :verification-pending nil))

(defun mevedel-goal--save-session-state (session buffer)
  "Persist SESSION state from BUFFER."
  (require 'mevedel-session-persistence)
  (condition-case err
      (mevedel-session-persistence-save session buffer)
    (error
     (display-warning
      'mevedel
      (format "Could not persist plan state: %S" err)
      :warning))))

(defun mevedel-goal--persist-checkpoint (session buffer)
  "Persist SESSION's Goal checkpoint from BUFFER or signal an error."
  (require 'mevedel-session-persistence)
  (mevedel-session-persistence-save session buffer))

(defun mevedel-goal-owned-by-session-p (goal session)
  "Return non-nil when GOAL is exclusively owned by SESSION."
  (let ((session-id (mevedel-session-session-id session))
        (home (mevedel-goal-execution-home goal)))
    (and (stringp session-id)
         (null (mevedel-session-goal-handoff session))
         (equal session-id (mevedel-goal-owner-session goal))
         (equal session-id (plist-get home :session-id)))))

(defun mevedel-goal--assert-execution-home (goal session)
  "Signal unless GOAL may execute from SESSION's recorded home."
  (unless (mevedel-goal-owned-by-session-p goal session)
    (error "Goal is owned by another session"))
  (let ((expected (file-name-as-directory
                   (expand-file-name
                    (plist-get (mevedel-goal-execution-home goal)
                               :directory))))
        (actual (file-name-as-directory
                 (expand-file-name
                  (or (mevedel-session-working-directory session)
                      default-directory)))))
    (unless (equal expected actual)
      (error "Goal execution home is %s, not %s" expected actual))
    expected))

(defun mevedel-goal-context-fragment (goal &optional session snapshot)
  "Return authoritative model context generated from persisted GOAL state.
SESSION supplies the cycle-index path when it is materialized.  When SNAPSHOT
is non-nil, label the fragment as a compaction-time snapshot instead."
  (let* ((home (mevedel-goal-execution-home goal))
         (cycles (mevedel-goal-cycles goal))
         (plan-cycle
          (cl-find-if (lambda (record) (plist-get record :plan))
                      cycles :from-end t))
         (review-cycle
          (cl-find-if (lambda (record) (plist-get record :review))
                      cycles :from-end t))
         (plan (or (mevedel-goal-current-plan goal)
                   (and plan-cycle
                        (list :path (plist-get plan-cycle :plan)))))
         (review (mevedel-goal-review-summary goal))
         (review-text
          (cond
           (review
            (format "%s - %s"
                    (or (plist-get review :verdict) "unknown")
                    (or (plist-get review :summary) "no summary")))
           ((mevedel-goal-review-findings goal)
            (format "continue - %s" (mevedel-goal-review-findings goal)))
           ((plist-get review-cycle :review)
            (format "%s - summary hash %s"
                    (or (plist-get (plist-get review-cycle :review) :verdict)
                        "unknown")
                    (or (plist-get (plist-get review-cycle :review)
                                   :summary-hash)
                        "unavailable")))
           (t "none")))
         (budget (mevedel-goal-token-budget goal))
         (save-path (and session (mevedel-session-save-path session)))
         (cycle-index
          (if save-path
              (file-name-concat save-path (mevedel-goal--relative-dir goal)
                                "cycles.el")
            (file-name-concat (mevedel-goal--relative-dir goal) "cycles.el"))))
    (format
     (concat
      "<goal-context authority=\"%s\">\n"
      "Goal ID: %s\nObjective: %s\nStatus: %s\nPhase: %s\nCycle: %d\n"
      "Approval policy: %s\nAccepted plan: %s\nCycle index: %s\n"
      "Latest review: %s\nBudget: %s\nExecution home: %s\n"
      "</goal-context>")
     (if snapshot "compaction-snapshot" "session-sidecar")
     (mevedel-goal-id goal)
     (mevedel-goal-objective goal)
     (mevedel-goal-status goal)
     (mevedel-goal-phase goal)
     (mevedel-goal-cycle goal)
     (mevedel-goal-approval-policy goal)
     (or (plist-get plan :absolute-path) (plist-get plan :path) "none")
     cycle-index
     review-text
     (if budget
         (format "%d/%d tokens"
                 (or (mevedel-goal-token-usage goal) 0) budget)
       (format "%d tokens used; unlimited"
               (or (mevedel-goal-token-usage goal) 0)))
     (or (plist-get home :directory) "unavailable"))))

(defun mevedel-goal--refresh-request-context (goal input)
  "Return checkpoint INPUT with current persisted context for GOAL."
  (let ((context (mevedel-goal-context-fragment goal mevedel--session)))
    (if (stringp input)
        (let* ((start (string-match "<goal-context[ >]" input))
               (close (and start (string-match "</goal-context>" input start)))
               (end (and close (+ close (length "</goal-context>")))))
          (if end
              (concat (substring input 0 start) context (substring input end))
            (concat context "\n\n" input)))
      (plist-put (copy-tree input) :goal-context context))))

(defun mevedel-goal--enqueue-event-reminder (session event)
  "Queue sparse lifecycle EVENT for SESSION without changing Goal state."
  (mevedel-session-enqueue-pending-reminder
   session (format "Goal lifecycle event: %s" event)))

(defun mevedel-goal--policy-label (policy)
  "Return a stable provider/model label for resolved POLICY."
  (let ((backend (plist-get policy :backend))
        (model (plist-get policy :model)))
    (if backend
        (condition-case nil
            (format "%s:%s" (gptel-backend-name backend)
                    (gptel--model-name model))
          (error (format "%s:%s" backend model)))
      (format "%s" model))))

(defun mevedel-goal--checkpoint-prepare (phase input workload policy)
  "Durably prepare PHASE checkpoint for INPUT, WORKLOAD, and POLICY."
  (let* ((session mevedel--session)
         (goal (mevedel-session-goal session))
         (prior (mevedel-goal-checkpoint goal))
         (same-boundary
          (and (eq phase (plist-get prior :phase))
               (= (mevedel-goal-cycle goal)
                  (or (plist-get prior :cycle) -1))))
         (attempt (if same-boundary
                      (1+ (plist-get prior :attempt))
                    1))
         (attempt-id (format "%s/%d/%s/%d"
                             (mevedel-goal-id goal)
                             (mevedel-goal-cycle goal) phase attempt))
         (last-settled
          (if (eq (plist-get prior :dispatch-state) 'settled)
              (list :phase (plist-get prior :phase)
                    :cycle (plist-get prior :cycle)
                    :attempt-id (plist-get prior :attempt-id)
                    :settled-at (plist-get prior :settled-at))
            (plist-get prior :last-settled-boundary)))
         (checkpoint
          (list :phase phase
                :cycle (mevedel-goal-cycle goal)
                :input (copy-tree input)
                :workload workload
                :provider (mevedel-goal--policy-label policy)
                :effort (plist-get policy :effort)
                :plan-reference
                (plist-get (mevedel-goal-current-plan goal) :path)
                :attempt attempt
                :attempt-id attempt-id
                :retry-count (if same-boundary
                                 (or (plist-get prior :retry-count) 0)
                               0)
                :estimated-input-tokens
                (mevedel-goal--estimate-input-tokens input)
                :token-baseline
                (when (fboundp 'mevedel--estimate-tokens)
                  (mevedel--estimate-tokens))
                :usage-recorded nil
                :dispatch-state 'prepared
                :request-started nil
                :last-settled-boundary last-settled
                :prepared-at (format-time-string "%FT%T%z"))))
    (setf (mevedel-goal-checkpoint goal) checkpoint)
    (mevedel-goal--persist-checkpoint session (current-buffer))
    checkpoint))

(defun mevedel-goal--checkpoint-state (state &rest properties)
  "Set current Goal checkpoint STATE and append PROPERTIES durably."
  (let* ((session mevedel--session)
         (goal (mevedel-session-goal session))
         (checkpoint (copy-tree (mevedel-goal-checkpoint goal))))
    (setq checkpoint (plist-put checkpoint :dispatch-state state))
    (while properties
      (setq checkpoint (plist-put checkpoint (pop properties)
                                  (pop properties))))
    (setf (mevedel-goal-checkpoint goal) checkpoint)
    (mevedel-goal--persist-checkpoint session (current-buffer))
    checkpoint))

(defun mevedel-goal--checkpoint-settle (fsm state &optional reason)
  "Set FSM's matching checkpoint to STATE, optionally recording REASON."
  (when-let* ((goal (and (bound-and-true-p mevedel--session)
                         (mevedel-session-goal mevedel--session)))
              (checkpoint (mevedel-goal-checkpoint goal))
              (info (gptel-fsm-info fsm))
              (attempt-id (plist-get info :mevedel-goal-attempt-id))
              ((equal attempt-id (plist-get checkpoint :attempt-id))))
    (setq checkpoint (copy-tree checkpoint))
    (setq checkpoint (plist-put checkpoint :dispatch-state state))
    (setq checkpoint (plist-put checkpoint
                                (if (eq state 'settled)
                                    :settled-at :failed-at)
                                (format-time-string "%FT%T%z")))
    (when reason
      (setq checkpoint (plist-put checkpoint :error reason)))
    (setf (mevedel-goal-checkpoint goal) checkpoint)
    checkpoint))

(defun mevedel-goal--fsm-failure-reason (fsm status)
  "Return an actionable failure reason from FSM and terminal STATUS."
  (let* ((info (gptel-fsm-info fsm))
         (error-value (plist-get info :error))
         (detail (cond
                  ((stringp error-value) error-value)
                  ((listp error-value)
                   (or (plist-get error-value :message)
                       (plist-get error-value :type)))
                  (error-value (format "%s" error-value))
                  ((plist-get info :status)
                   (format "%s" (plist-get info :status)))
                  (t (symbol-name status)))))
    (string-trim (format "%s" detail))))

(defun mevedel-goal--transient-failure-p (reason)
  "Return non-nil when REASON describes a retryable transport failure."
  (string-match-p
   (rx (or "timeout" "timed out" "temporar" "connection"
           "network" "unavailable" "502" "503" "504"))
   (downcase reason)))

(defun mevedel-goal--terminal-provider-failure-p (reason)
  "Return non-nil when REASON requires user or provider intervention."
  (string-match-p
   (rx (or "quota" "credit" "billing" "authentication" "unauthorized"
           "forbidden" "api key" "unsupported" "rate limit" "429"))
   (downcase reason)))

(defun mevedel-goal--estimate-input-tokens (input)
  "Estimate token count for exact Goal request INPUT."
  (max 1 (/ (+ (length (if (stringp input)
                           input
                         (prin1-to-string input)))
               3)
            4)))

(defun mevedel-goal--estimate-request-input-tokens (prompt system-prompt)
  "Estimate tokens for request PROMPT and resolved SYSTEM-PROMPT."
  (+ (mevedel-goal--estimate-input-tokens prompt)
     (cond
      ((null system-prompt) 0)
      ((stringp system-prompt)
       (mevedel-goal--estimate-input-tokens system-prompt))
      ((listp system-prompt)
       (cl-loop for item in system-prompt
                when (stringp item)
                sum (mevedel-goal--estimate-input-tokens item)))
      (t (mevedel-goal--estimate-input-tokens system-prompt)))))

(defun mevedel-goal--charge-token-usage
    (goal info &optional response usage-context)
  "Charge GOAL for one request INFO, RESPONSE, and USAGE-CONTEXT.
USAGE-CONTEXT supplies request-local `:estimated-input-tokens' and
`:token-baseline' values.  When nil, use the current Goal checkpoint."
  (require 'mevedel-compact)
  (let* ((estimate-source
          (or usage-context (mevedel-goal-checkpoint goal)))
         (reported
          (mevedel--compact-token-usage-count
           (or (plist-get info :tokens) (plist-get info :tokens-full))))
         (baseline (plist-get estimate-source :token-baseline))
         (estimated
          (if (and (numberp baseline) (not response))
              (max (or (plist-get estimate-source
                                  :estimated-input-tokens)
                       1)
                   (- (mevedel--estimate-tokens) baseline))
            (+ (or (plist-get estimate-source :estimated-input-tokens) 1)
               (if (stringp response)
                   (mevedel-goal--estimate-input-tokens response)
                 0))))
         (count (or (and reported (> reported 0) reported) estimated)))
    (setf (mevedel-goal-token-usage goal)
          (+ (or (mevedel-goal-token-usage goal) 0) count))
    count))

(cl-defstruct
    (mevedel-goal--request-leg
     (:constructor mevedel-goal--request-leg-create (goal buffer)))
  "Request-local usage state for one possibly continued model request."
  goal
  buffer
  context
  (active t)
  chunks
  (reported-total 0)
  fallback-since-reported)

(defun mevedel-goal--request-leg-begin (leg)
  "Mark LEG as a newly active model-request leg."
  (setf (mevedel-goal--request-leg-active leg) t
        (mevedel-goal--request-leg-chunks leg) nil))

(defun mevedel-goal--request-leg-append (leg chunk)
  "Record streamed response CHUNK for active request LEG."
  (setf (mevedel-goal--request-leg-active leg) t)
  (push chunk (mevedel-goal--request-leg-chunks leg)))

(defun mevedel-goal--request-leg-charge (leg info &optional response)
  "Charge active request LEG once using request-local INFO and RESPONSE."
  (when (mevedel-goal--request-leg-active leg)
    (setf (mevedel-goal--request-leg-active leg) nil)
    (require 'mevedel-compact)
    (let* ((tokens (and info (plist-get info :tokens)))
           (tokens-full (and info (plist-get info :tokens-full)))
           (tokens-count
            (mevedel--compact-token-usage-count tokens))
           (full-count
            (mevedel--compact-token-usage-count tokens-full))
           (reported-count
            (cond
             ((and tokens-count (> tokens-count 0))
              (cl-incf
               (mevedel-goal--request-leg-reported-total leg)
               tokens-count)
              tokens-count)
             ((and full-count (> full-count 0))
              (prog1
                  (unless
                      (mevedel-goal--request-leg-fallback-since-reported leg)
                    (max
                     0
                     (- full-count
                        (mevedel-goal--request-leg-reported-total leg))))
                (setf
                 (mevedel-goal--request-leg-reported-total leg) full-count
                 (mevedel-goal--request-leg-fallback-since-reported leg) nil)))))
           (leg-response
            (or response
                (when-let* ((chunks
                             (mevedel-goal--request-leg-chunks leg)))
                  (apply #'concat (nreverse chunks)))))
           (buffer (mevedel-goal--request-leg-buffer leg)))
      (setf (mevedel-goal--request-leg-chunks leg) nil)
      (unless (and reported-count (> reported-count 0))
        (setf (mevedel-goal--request-leg-fallback-since-reported leg) t))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (mevedel-goal--charge-token-usage
           (mevedel-goal--request-leg-goal leg)
           (and reported-count
                (> reported-count 0)
                (list :tokens (list :input reported-count)))
           leg-response
           (mevedel-goal--request-leg-context leg)))))))

(defun mevedel-goal--record-token-usage (goal info &optional response)
  "Charge GOAL once for its checkpoint request INFO and RESPONSE."
  (let ((checkpoint (copy-tree (mevedel-goal-checkpoint goal))))
    (unless (plist-get checkpoint :usage-recorded)
      (let ((count (mevedel-goal--charge-token-usage goal info response)))
        (setq checkpoint (plist-put checkpoint :usage-recorded t))
        (setf (mevedel-goal-checkpoint goal) checkpoint)
        count))))

(defun mevedel-goal--append-guardian-audit (record chat-buffer)
  "Persist RECORD as a hidden, model-ignored disclosure in CHAT-BUFFER."
  (when (buffer-live-p chat-buffer)
    (with-current-buffer chat-buffer
      (require 'mevedel-transcript-audit)
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert (mevedel--format-hook-audit-record record)))
      (when (and (boundp 'mevedel--view-buffer)
                 (buffer-live-p mevedel--view-buffer))
        (mevedel-view-rerender mevedel--view-buffer)))))

(defun mevedel-goal--record-guardian-decision
    (goal decision plan-hash session chat-buffer)
  "Record GOAL guardian DECISION for PLAN-HASH in SESSION and CHAT-BUFFER."
  (let* ((record (append
                  (list :type 'goal-guardian
                        :verdict (plist-get decision :verdict)
                        :reason (plist-get decision :reason)
                        :feedback (copy-sequence
                                   (plist-get decision :feedback))
                        :plan-hash plan-hash
                        :provider (or (plist-get decision :provider) "unavailable")
                        :effort (plist-get decision :effort)
                        :at (format-time-string "%FT%T%z"))))
         (cycle (mevedel-goal-cycle-record goal))
         (audits (append (plist-get cycle :guardian-audits) (list record))))
    (mevedel-goal--cycle-put goal :guardian-audits audits)
    (mevedel-goal--persist-cycle-index goal session chat-buffer)
    (mevedel-goal--append-guardian-audit record chat-buffer)
    (mevedel-goal--save-session-state session chat-buffer)
    record))

(defun mevedel-goal--ensure-reference-reminder (session)
  "Install the accepted-plan reference reminder for SESSION."
  (require 'mevedel-reminders)
  (mevedel-session-remove-reminder session 'plan-reference)
  (mevedel-session-ensure-reminder
   session (mevedel-reminders-make-plan-reference)))

(defun mevedel-goal-read-only-phase-p (&optional session)
  "Return non-nil when SESSION's active Goal phase must be read-only."
  (when-let* ((session (or session mevedel--session))
              (goal (mevedel-session-goal session)))
    (and (eq (mevedel-goal-status goal) 'active)
         (or (memq (mevedel-goal-phase goal) '(planning reviewing))
             (plist-get (mevedel-session-plan-metadata session)
                        :revision-pending)))))

(defun mevedel-goal--investigation-budget ()
  "Return the valid Goal investigation budget in seconds."
  (unless (and (integerp mevedel-goal-investigation-time-budget)
               (<= 1 mevedel-goal-investigation-time-budget 3570))
    (error "Goal investigation time budget must be from 1 through 3570 seconds"))
  mevedel-goal-investigation-time-budget)

(defun mevedel-goal-investigation-contract (message &optional session)
  "Return Goal investigation metadata for MESSAGE in SESSION, or nil."
  (when-let* (((stringp message))
              ((not (string-empty-p message)))
              (session (or session mevedel--session))
              (goal (mevedel-session-goal session))
              ((eq (mevedel-goal-status goal) 'active))
              (phase (mevedel-goal-phase goal))
              ((memq phase '(planning reviewing)))
              (seconds (mevedel-goal--investigation-budget)))
    (list
     :seconds seconds
     :message
     (format
      "Goal %s investigation\nEvidence target (stay strictly within this scope): %s\nCompletion budget: %d seconds. Return concise evidence before the deadline; useful partial output is preserved if the budget expires."
      phase message seconds))))

(defun mevedel-goal--investigation-guidance ()
  "Return bounded subagent guidance for a read-only Goal phase."
  (let ((seconds (mevedel-goal--investigation-budget)))
    (format
     "If independent investigation is necessary, delegate one narrow evidence target at a time. Each Agent task has a %d-second completion budget. Await it with one event-driven WaitAgent(timeout_ms=%d) call; do not poll or repeatedly wait."
     seconds (* 1000 (+ seconds 30)))))

(defun mevedel-goal--planning-prompt (goal)
  "Return the planning request for GOAL."
  (format
   "%s\n\nPlanning instructions:\n%s%s\n\n%s\n\nThis phase is read-only. End with exactly one line-oriented <proposed_plan>...</proposed_plan> block."
   (mevedel-goal-context-fragment goal mevedel--session)
   (if-let* ((findings (mevedel-goal-review-findings goal)))
       (format "Prior review findings to resolve:\n%s\n\n" findings)
     "")
   mevedel-goal--plan-guidance
   (mevedel-goal--investigation-guidance)))

(defun mevedel-goal--review-prompt (goal)
  "Return the one-cycle completion review request for GOAL."
  (format
   "%s\n\nReview instructions:\nReview the implementation against current repository evidence using this authority order: Goal objective and achievement criteria, authoritative referenced requirements, then the accepted plan as an implementation approach. Require observable evidence that the desired outcome is achieved. Completing every plan step is insufficient when the Goal remains unmet. Reasonable divergence from plan details is acceptable when the authoritative outcomes are proven.\n\n%s\n\nThis phase is read-only. Do not make changes. Return exactly one structured result with a single verdict and evidence:\n<goal_review>\nverdict: complete|continue|blocked\nsummary: evidence, remaining work, or blocker\n</goal_review>\nUse complete only when the whole Goal and its achievement criteria are proven complete; use continue when another implementation cycle can make progress; use blocked only for a concrete external or decision blocker."
   (mevedel-goal-context-fragment goal mevedel--session)
   (mevedel-goal--investigation-guidance)))

(defun mevedel-goal--recovery-prompt (goal checkpoint)
  "Return a read-only recovery audit request for GOAL and CHECKPOINT."
  (format
   "%s\n\nRecovery instructions:\nInterrupted implementation attempt: %s\nDispatch state: %s\n\nThe implementation request may already have changed files. Do not replay it and do not make changes. Inspect the actual repository, diff, tests, and artifacts against the objective and accepted plan. Choose the next safe lifecycle boundary. Return exactly one structured result:\n<goal_review>\nverdict: complete|continue|blocked\nsummary: evidence and the safe next step\n</goal_review>\nUse complete only when repository evidence proves the whole objective; continue when a new plan should address remaining work; blocked when external input is required."
   (mevedel-goal-context-fragment goal mevedel--session)
   (or (plist-get checkpoint :attempt-id) "unknown")
   (or (plist-get checkpoint :dispatch-state) 'unknown)))

(defun mevedel-goal--guardian-reference-paths (plan)
  "Return explicit PRD, specification, or ticket paths mentioned in PLAN."
  (let ((case-fold-search t)
        paths)
    (with-temp-buffer
      (insert plan)
      (goto-char (point-min))
      (while (not (eobp))
        (let* ((start (line-beginning-position))
               (end (line-end-position))
               (reference-line
                (string-match-p
                 "\\_<\\(?:prd\\|specification\\|spec\\|ticket\\)\\_>"
                 (buffer-substring-no-properties start end))))
          (while (re-search-forward
                  "\\(?:^\\|[^[:alnum:]_./-]\\)\\([[:alnum:]_./-]+\\.md\\)"
                  end t)
            (let ((path (match-string-no-properties 1)))
              (when (or reference-line
                        (string-match-p
                         "\\(?:prd\\|spec\\|ticket\\)"
                         (file-name-nondirectory path)))
                (push path paths))))
          (forward-line 1))))
    (delete-dups (nreverse paths))))

(defun mevedel-goal--guardian-reference-evidence (plan)
  "Return deterministic local reference evidence for PLAN."
  (let ((base (or (and (bound-and-true-p mevedel--session)
                       (mevedel-session-working-directory mevedel--session))
                  default-directory)))
    (if-let* ((paths (mevedel-goal--guardian-reference-paths plan)))
        (string-join
         (mapcar
          (lambda (path)
            (let ((absolute (expand-file-name path base)))
              (format "%s: %s"
                      path
                      (cond
                       ((not (file-exists-p absolute)) "missing")
                       ((not (file-readable-p absolute)) "exists but is unreadable")
                       (t "exists and is readable")))))
          paths)
         "\n")
      "No explicit local PRD, specification, or ticket path detected.")))

(defun mevedel-goal--guardian-review-position (&optional session)
  "Return the current guardian review position for SESSION."
  (let ((count (or (plist-get
                    (and session (mevedel-session-plan-metadata session))
                    :revision-count)
                   0)))
    (cond
     ((zerop count) 'initial)
     ((>= count mevedel-goal--max-automatic-revisions) 'final)
     (t (intern (format "revision-%d" count))))))

(defun mevedel-goal-approval-status (&optional session)
  "Return hidden plan-approval status and workload for SESSION."
  (when-let* ((session (or session
                           (and (bound-and-true-p mevedel--session)
                                mevedel--session)))
              (metadata (mevedel-session-plan-metadata session)))
    (let ((count (or (plist-get metadata :revision-count) 0)))
      (cond
       ((plist-get metadata :revision-pending)
        (list
         :label
         (format "revising plan %d/%d"
                 (min (1+ count) mevedel-goal--max-automatic-revisions)
                 mevedel-goal--max-automatic-revisions)
         :workload 'planning))
       ((plist-get metadata :guardian-pending)
        (list
         :label
         (if (zerop count)
             "guardian reviewing initial plan"
           (format "guardian reviewing revision %d/%d"
                   count mevedel-goal--max-automatic-revisions))
         :workload 'goal-guardian))))))

(defun mevedel-goal--guardian-prompt
    (goal plan &optional review-position remaining-revisions prior-feedback)
  "Return untrusted guardian evidence for GOAL and PLAN.
REVIEW-POSITION and REMAINING-REVISIONS describe the current automatic
approval boundary.  PRIOR-FEEDBACK contains the preceding guardian corrections."
  (format
   "Goal context:\n```text\n%s\n```\n\nReview position: %s\nAutomatic revisions remaining: %d\n\nPrior guardian feedback:\n%s\n\nReference validation:\n```text\n%s\n```\n\nProposed plan:\n```text\n%s\n```"
   (mevedel-goal-context-fragment goal mevedel--session)
   (or review-position 'initial)
   (or remaining-revisions mevedel-goal--max-automatic-revisions)
   (if prior-feedback
       (mapconcat (lambda (item) (concat "- " item))
                  prior-feedback "\n")
     "None.")
   (mevedel-goal--guardian-reference-evidence plan)
   plan))

(defun mevedel-goal--parse-guardian (text)
  "Return a validated Goal guardian decision parsed from TEXT, or nil."
  (when (stringp text)
    (let* ((lines (split-string (string-trim text) "\n" nil))
           (count (length lines)))
      (when (and (>= count 5)
                 (equal (nth 0 lines) mevedel-goal--guardian-open-tag)
                 (equal (car (last lines))
                        mevedel-goal--guardian-close-tag)
                 (string-match
                  "\\`verdict:[ \t]*\\(approve\\|revise\\|ask\\)[ \t]*\\'"
                  (nth 1 lines))
                 (string-match
                  "\\`reason:[ \t]*\\(.+?\\)[ \t]*\\'" (nth 2 lines))
                 (equal (nth 3 lines) "feedback:"))
        (let* ((verdict
                (intern
                 (string-trim
                  (substring (nth 1 lines) (length "verdict:")))))
               (reason
                (progn
                  (string-match
                   "\\`reason:[ \t]*\\(.+?\\)[ \t]*\\'" (nth 2 lines))
                  (string-trim (match-string 1 (nth 2 lines)))))
               (feedback-lines (cl-subseq lines 4 (1- count)))
               (feedback
                (mapcar
                 (lambda (line)
                   (and (string-match "\\`-[ \t]+\\(.+\\)\\'" line)
                        (string-trim (match-string 1 line))))
                 feedback-lines)))
          (when (and (not (string-blank-p reason))
                     (not (memq nil feedback))
                     (pcase verdict
                       ('approve (null feedback))
                       ((or 'revise 'ask) feedback)))
            (list :verdict verdict :reason reason :feedback feedback)))))))

(defun mevedel-goal--guardian-failure-decision (reason &rest properties)
  "Return a structurally valid internal ask decision for REASON.
Append PROPERTIES to the decision."
  (append
   (list :verdict 'ask
         :reason reason
         :feedback
         '("Review the latest valid plan manually before implementation."))
   properties))

(defun mevedel-goal--normalize-guardian-decision (decision)
  "Return validated guardian DECISION or a fail-closed ask decision."
  (let ((verdict (plist-get decision :verdict))
        (reason (plist-get decision :reason))
        (feedback (plist-get decision :feedback)))
    (if (and (memq verdict '(approve revise ask))
             (stringp reason)
             (not (string-blank-p reason))
             (or (null feedback)
                 (and (proper-list-p feedback)
                      (cl-every
                       (lambda (item)
                         (and (stringp item)
                              (not (string-blank-p item))))
                       feedback)))
             (pcase verdict
               ('approve (null feedback))
               ((or 'revise 'ask) feedback)))
        decision
      (mevedel-goal--guardian-failure-decision
       "Goal guardian returned malformed output"))))

(defun mevedel-goal--guardian-provider-label (policy)
  "Return a stable provider/model label for resolved guardian POLICY."
  (mevedel-goal--policy-label policy))

(defun mevedel-goal--guardian-request (goal plan chat-buffer callback)
  "Review PLAN for GOAL internally, then call CALLBACK in CHAT-BUFFER.
The request has no tools or conversational transcript insertion."
  (if (not (require 'gptel nil t))
      (funcall callback
               (mevedel-goal--guardian-failure-decision
                "Goal guardian is unavailable"))
    (let* ((done nil)
          (session (buffer-local-value 'mevedel--session chat-buffer))
          (telemetry-span
           (and session
                (fboundp 'mevedel-telemetry-start)
                (mevedel-telemetry-start
                 session 'guardian-request
                 :goal-id (mevedel-goal-id goal))))
          (first-response nil)
          (stream (buffer-local-value 'gptel-stream chat-buffer))
          chunks
          policy
          timer
          usage-context)
      (cl-labels
          ((record-first-response ()
             (unless first-response
               (setq first-response t)
               (when (and session (fboundp 'mevedel-telemetry-record))
                 (mevedel-telemetry-record
                  session 'guardian-first-response
                  :provider (and policy
                                 (mevedel-goal--guardian-provider-label
                                  policy))
                  :effort (and policy (plist-get policy :effort))))))
           (finish (decision)
             (unless done
               (setq done t)
               (when timer (cancel-timer timer))
               (when telemetry-span
                 (mevedel-telemetry-finish
                  telemetry-span
                  :outcome (or (plist-get decision :verdict) 'error)
                  :provider (and policy
                                 (mevedel-goal--guardian-provider-label
                                  policy))
                  :effort (and policy (plist-get policy :effort))))
               (when (buffer-live-p chat-buffer)
                 (with-current-buffer chat-buffer
                   (funcall callback decision)))))
           (settle-response (response info)
             (record-first-response)
             (let ((decision (mevedel-goal--parse-guardian response)))
               (when (buffer-live-p chat-buffer)
                 (with-current-buffer chat-buffer
                   (mevedel-goal--charge-token-usage
                    goal info response usage-context)))
               (finish
                (append
                 (or decision
                     (mevedel-goal--guardian-failure-decision
                      "Goal guardian returned malformed output"))
                 (list
                  :provider
                  (mevedel-goal--guardian-provider-label policy)
                  :effort (plist-get policy :effort)))))))
        (setq timer
              (run-at-time
               mevedel-goal-guardian-timeout nil
               (lambda ()
                 (unless done
                   (when (buffer-live-p chat-buffer)
                     (with-current-buffer chat-buffer
                       (mevedel-goal--charge-token-usage
                        goal nil
                        (and chunks (apply #'concat (nreverse chunks)))
                        usage-context)))
                   (finish
                    (append
                     (mevedel-goal--guardian-failure-decision
                      "Goal guardian timed out"
                      :checkpoint-state 'failed)
                     (when policy
                       (list :provider
                             (mevedel-goal--guardian-provider-label policy)
                             :effort (plist-get policy :effort)))))))))
        (condition-case err
            (progn
              (let* ((count
                      (or (plist-get
                           (and session
                                (mevedel-session-plan-metadata session))
                           :revision-count)
                          0))
                     (prior-feedback
                      (when-let* ((record
                                   (car
                                    (last
                                     (plist-get
                                      (mevedel-goal-cycle-record goal)
                                      :plan-revisions)))))
                        (plist-get record :feedback)))
                     (system-prompt
                      (progn
                        (require 'mevedel-system)
                        (mevedel-system-render-prompt-file
                         "prompts/goals/goal-guardian-system.md")))
                     (prompt
                      (mevedel-goal--guardian-prompt
                       goal plan
                       (mevedel-goal--guardian-review-position session)
                       (max 0
                            (- mevedel-goal--max-automatic-revisions
                               count))
                       prior-feedback)))
                (setq usage-context
                      (list
                       :estimated-input-tokens
                       (mevedel-goal--estimate-request-input-tokens
                        prompt system-prompt)))
                (mevedel-goal--call-with-workload
                 'goal-guardian
                 (lambda ()
                   (setq policy
                         (list :backend gptel-backend
                               :model gptel-model
                               :effort gptel-reasoning-effort))
                   (let ((gptel-use-tools nil)
                         (gptel-tools nil)
                         (gptel-use-context nil))
                     (gptel-request
                      prompt
                      :buffer chat-buffer
                      :stream stream
                      :system system-prompt
                      :transforms nil
                      :callback
                      (lambda (response info)
                        (cond
                         (done nil)
                         ((and (consp response)
                               (eq (car response) 'reasoning)))
                         ((and (plist-get info :stream)
                               (stringp response))
                          (record-first-response)
                          (push response chunks))
                         ((eq response t)
                          (settle-response
                           (apply #'concat (nreverse chunks)) info))
                         (t (settle-response response info)))))))
                 'guardian prompt)))
          (error
           (finish
            (append
             (mevedel-goal--guardian-failure-decision
              (format "Goal guardian failed: %s"
                      (error-message-string err))
              :checkpoint-state 'failed)
             (when policy
               (list :provider (mevedel-goal--guardian-provider-label policy)
                     :effort (plist-get policy :effort)))))))))))

(defun mevedel-goal--planner-revision-prompt
    (goal plan decision revision-number)
  "Return the planner correction prompt for GOAL, PLAN, and DECISION.
REVISION-NUMBER identifies the correction round."
  (format
   "%s\n\nAutomatic plan revision:\nRevision: %d\nAutomatic revisions remaining after this correction: %d\nGuardian reason: %s\nGuardian feedback:\n%s\n\nCurrent proposed plan:\n%s\n\n%s\n\nThis phase is read-only. Address all feedback together and return one complete replacement plan in exactly one line-oriented <proposed_plan>...</proposed_plan> block. Do not return a point-by-point reply."
   (mevedel-goal-context-fragment goal mevedel--session)
   revision-number
   (max 0 (- mevedel-goal--max-automatic-revisions revision-number))
   (plist-get decision :reason)
   (mapconcat (lambda (item) (concat "- " item))
              (plist-get decision :feedback) "\n")
   plan
   mevedel-goal--plan-guidance))

(defun mevedel-goal--guard-planner-tool-calls (calls info live-p)
  "Return CALLS and INFO tool specs guarded by LIVE-P after settlement."
  (let* ((guarded-tools nil)
         (guarded-calls
          (mapcar
           (pcase-lambda (`(,tool ,args ,result-callback))
             (let* ((function (gptel-tool-function tool))
                    (async (gptel-tool-async tool))
                    (guarded-tool (gptel--copy-tool tool)))
               (setf
                (gptel-tool-function guarded-tool)
                (if async
                    (lambda (callback &rest function-args)
                      (when (funcall live-p)
                        (apply function callback function-args)))
                  (lambda (&rest function-args)
                    (when (funcall live-p)
                      (apply function function-args)))))
               (push (cons (gptel-tool-name tool) guarded-tool)
                     guarded-tools)
               (list
                guarded-tool
                args
                (lambda (result)
                  (when (funcall live-p)
                    (funcall result-callback result))))))
           calls)))
    (when-let* ((tools (plist-get info :tools)))
      (plist-put
       info :tools
       (mapcar
        (lambda (tool)
          (or (alist-get (gptel-tool-name tool)
                         guarded-tools nil nil #'equal)
              tool))
        tools)))
    guarded-calls))

(defun mevedel-goal--tool-confirmation-overlay (display info)
  "Return the dispatch overlay associated with DISPLAY and INFO."
  (when-let* ((buffer
               (or (and (overlayp display) (overlay-buffer display))
                   (and-let* ((position (plist-get info :position)))
                     (marker-buffer position)))))
    (with-current-buffer buffer
      (cl-find-if
       (lambda (overlay)
         (and (eq (overlay-get overlay 'info) info)
              (overlay-get overlay 'gptel-tool)))
       (overlays-in (point-min) (point-max))))))

(defun mevedel-goal--stop-planner-request
    (fsm chat-buffer request tool-overlay)
  "Cancel REQUEST, its TOOL-OVERLAY, and active FSM in CHAT-BUFFER."
  (mevedel-request-cancel request)
  (when (and (overlayp tool-overlay) (overlay-buffer tool-overlay))
    (gptel--reject-tool-calls nil tool-overlay))
  (when fsm
    (let ((info (gptel-fsm-info fsm)))
      (when info
        (plist-put info :callback #'ignore)))
    (setf (gptel-fsm-state fsm) 'ABRT)
    (gptel-abort chat-buffer)))

(defun mevedel-goal--planner-revision-request
    (goal plan decision chat-buffer callback)
  "Request one automatic replacement for PLAN, then call CALLBACK."
  (if (not (require 'gptel nil t))
      (funcall callback '(:error "Planner is unavailable"))
    (let* ((session (buffer-local-value 'mevedel--session chat-buffer))
           (done nil)
           (stream (buffer-local-value 'gptel-stream chat-buffer))
           chunks
           (previous-request
            (buffer-local-value 'mevedel--current-request chat-buffer))
           policy
           (planner-request
            (mevedel-request--create
             :session session
             :origin "/root"))
           request-fsm
           tool-confirmation-overlay
           timer
           (usage-leg
            (mevedel-goal--request-leg-create goal chat-buffer))
           (revision-number
            (1+ (or (plist-get
                     (and session
                          (mevedel-session-plan-metadata session))
                     :revision-count)
                    0))))
      (cl-labels
          ((finish (result &optional stop-request)
             (unless done
               (setq done t)
               (when timer
                 (cancel-timer timer))
               (when stop-request
                 (mevedel-goal--stop-planner-request
                  request-fsm chat-buffer planner-request
                  tool-confirmation-overlay))
               (when (buffer-live-p chat-buffer)
                 (with-current-buffer chat-buffer
                   (when (eq mevedel--current-request planner-request)
                     (setq-local mevedel--current-request previous-request))
                   (funcall callback result)))))
           (settle-response (response info usage-response)
             (mevedel-goal--request-leg-charge
              usage-leg info usage-response)
             (let ((replacement
                    (and (stringp response)
                         (mevedel-plan-extract-proposed response))))
               (finish
                (append
                 (if replacement
                     (list :plan replacement)
                   (list
                    :error "Planner returned no complete replacement plan"))
                 (when policy
                   (list :provider (mevedel-goal--policy-label policy)
                         :effort (plist-get policy :effort))))))))
        (setq timer
              (run-at-time
               mevedel-goal-planner-revision-timeout nil
               (lambda ()
                 (unless done
                   (mevedel-goal--request-leg-charge usage-leg nil)
                   (finish
                    (append
                     (list :error "Planner revision timed out")
                     (when policy
                       (list :provider (mevedel-goal--policy-label policy)
                             :effort (plist-get policy :effort))))
                    t)))))
        (condition-case err
            (let* ((prompt
                    (with-current-buffer chat-buffer
                      (mevedel-goal--planner-revision-prompt
                       goal plan decision revision-number)))
                   (system-prompt
                    (with-current-buffer chat-buffer
                      (if (functionp gptel-system-prompt)
                          (funcall gptel-system-prompt)
                        gptel-system-prompt))))
              (setf
               (mevedel-goal--request-leg-context usage-leg)
               (list
                :estimated-input-tokens
                (mevedel-goal--estimate-request-input-tokens
                 prompt system-prompt)))
              (with-current-buffer chat-buffer
                (mevedel-goal--call-with-workload
                 'planning
                 (lambda ()
                   (setq policy
                         (list :backend gptel-backend
                               :model gptel-model
                               :effort gptel-reasoning-effort))
                   (setq
                    request-fsm
                    (progn
                      (setq-local mevedel--current-request planner-request)
                      (gptel-request
                       prompt
                       :buffer chat-buffer
                       :stream stream
                       :system system-prompt
                       :transforms nil
                       :callback
                       (lambda (response info)
                         (cond
                          (done nil)
                          ((and (consp response)
                                (eq (car response) 'reasoning)))
                          ((and (consp response)
                                (eq (car response) 'tool-call))
                           (when (or (plist-get info :tokens)
                                     (plist-get info :tokens-full))
                             (mevedel-goal--request-leg-charge usage-leg info))
                           (setq
                            tool-confirmation-overlay
                            (mevedel-goal--tool-confirmation-overlay
                             (gptel--display-tool-calls
                              (mevedel-goal--guard-planner-tool-calls
                               (cdr response)
                               info
                               (lambda () (not done)))
                              info)
                             info)))
                          ((and (consp response)
                                (eq (car response) 'tool-result))
                           (mevedel-goal--request-leg-charge usage-leg info)
                           (mevedel-goal--request-leg-begin usage-leg))
                          ((and (plist-get info :stream)
                                (stringp response))
                           (push response chunks)
                           (mevedel-goal--request-leg-append
                            usage-leg response))
                          ((eq response t)
                           (if (plist-get info :tool-use)
                               (mevedel-goal--request-leg-charge
                                usage-leg info)
                             (settle-response
                              (apply #'concat (nreverse chunks))
                              info nil)))
                          ((stringp response)
                           (if (plist-get info :tool-use)
                               (progn
                                 (push response chunks)
                                 (mevedel-goal--request-leg-charge
                                  usage-leg info response))
                             (settle-response
                              (apply #'concat
                                     (nreverse (cons response chunks)))
                              info response)))
                          ((or (null response) (eq response 'abort))
                           (mevedel-goal--request-leg-charge usage-leg info)
                           (finish
                            (list
                             :error
                             "Planner revision request failed"))))))))))))
          (error
           (finish
            (list :error
                  (format "Planner revision failed: %s"
                          (error-message-string err)))
            t)))))))

(defun mevedel-goal--revision-escalation-reason
    (decision result &optional limit-reached)
  "Return escalation text for DECISION and planner RESULT.
When LIMIT-REACHED is non-nil, disclose the automatic correction ceiling."
  (string-join
   (delq nil
         (list
          (plist-get decision :reason)
          (when-let* ((feedback (plist-get decision :feedback)))
            (concat "Requested corrections:\n"
                    (mapconcat (lambda (item) (concat "- " item))
                               feedback "\n")))
          (plist-get result :error)
          (when limit-reached
            (format "Automatic plan revision limit of %d reached."
                    mevedel-goal--max-automatic-revisions))))
   "\n\n"))

(defun mevedel-goal--complete-plan-revision-record-p (record)
  "Return non-nil when RECORD is a complete settled revision audit."
  (and (integerp (plist-get record :revision))
       (stringp (plist-get record :input-plan-hash))
       (stringp (plist-get record :replacement-plan-hash))
       (eq (plist-get record :verdict) 'revise)
       (when-let* ((reason (plist-get record :reason)))
         (and (stringp reason) (not (string-blank-p reason))))
       (when-let* ((feedback (plist-get record :feedback)))
         (and (consp feedback)
              (cl-every (lambda (item)
                          (and (stringp item)
                               (not (string-blank-p item))))
                        feedback)))
       (plist-member record :guardian-provider)
       (plist-member record :guardian-effort)
       (plist-member record :planner-provider)
       (plist-member record :planner-effort)
       (eq (plist-get record :settlement-state) 'settled)
       (stringp (plist-get record :started-at))
       (stringp (plist-get record :settled-at))))

(defun mevedel-goal--plan-revision-recovery-action
    (metadata record plan-hash guardian-audit)
  "Classify revision recovery from durable state.
METADATA is the session plan metadata, RECORD is the latest revision audit,
PLAN-HASH identifies the recorded current candidate, and GUARDIAN-AUDIT is
the matching guardian result when one exists."
  (cond
   ((and (plist-get metadata :revision-pending)
         (null record))
    'present-previous)
   ((and (mevedel-goal--complete-plan-revision-record-p record)
         (equal plan-hash (plist-get metadata :hash))
         (equal plan-hash (plist-get record :replacement-plan-hash))
         (not (equal plan-hash (plist-get record :input-plan-hash)))
         (null guardian-audit))
    'review-replacement)
   ((and record
         (memq (plist-get record :settlement-state)
               '(started failed)))
    'present-previous)))

(defun mevedel-goal--recover-plan-revision
    (goal session chat-buffer)
  "Resume GOAL from its durable automatic revision boundary.
Return non-nil when revision recovery handled continuation for SESSION in
CHAT-BUFFER."
  (let* ((metadata (mevedel-session-plan-metadata session))
         (record
          (car
           (last
            (plist-get (mevedel-goal-cycle-record goal)
                       :plan-revisions))))
         (plan (mevedel-plan-current-body session))
         (plan-hash (and plan (mevedel-plan-hash plan)))
         (audit (and plan
                     (mevedel-goal--guardian-decision-for-plan goal plan)))
         (action
          (and plan
               (mevedel-goal--plan-revision-recovery-action
                metadata record plan-hash audit))))
    (pcase action
      ('review-replacement
       (let ((revision (plist-get record :revision)))
         (mevedel-goal--plan-metadata-put
          session :revision-count
          (max revision
               (or (plist-get metadata :revision-count) 0)))
         (mevedel-goal--plan-metadata-put session :revision-pending nil)
         (mevedel-goal--plan-metadata-put session :guardian-pending nil)
         (mevedel-goal--record-plan-revision
          goal session chat-buffer revision
          :recovered-at (format-time-string "%FT%T%z"))
         (mevedel-goal--guard-current-plan goal chat-buffer)
         t))
      ('present-previous
       (mevedel-goal--plan-metadata-put session :revision-pending nil)
       (mevedel-goal--plan-metadata-put session :guardian-pending nil)
       (if record
           (let ((failure
                  (or (plist-get record :failure)
                      "Planner revision was interrupted before a replacement plan was persisted")))
             (mevedel-goal--record-plan-revision
              goal session chat-buffer (plist-get record :revision)
              :settlement-state 'failed
              :failure failure
              :failed-at (or (plist-get record :failed-at)
                             (format-time-string "%FT%T%z")))
             (mevedel-goal-present-plan
              plan chat-buffer
              (mevedel-goal--revision-escalation-reason
               (list :reason (plist-get record :reason)
                     :feedback (plist-get record :feedback))
               (list :error failure))))
         (require 'mevedel-session-persistence)
         (mevedel-session-persistence-save session chat-buffer)
         (mevedel-goal-present-plan
          plan chat-buffer
          "Automatic plan revision was interrupted before its audit record was persisted; explicit approval is required."))
       t))))

(defun mevedel-goal--planner-revision-finished
    (goal-id revision input-plan input-hash decision chat-buffer result)
  "Apply planner revision RESULT for GOAL-ID and INPUT-PLAN.
REVISION identifies the correction round.  INPUT-HASH identifies the
still-current candidate reviewed in CHAT-BUFFER."
  (when (buffer-live-p chat-buffer)
    (with-current-buffer chat-buffer
      (when-let* ((session (and (bound-and-true-p mevedel--session)
                                mevedel--session))
                  (goal (mevedel-session-goal session))
                  ((equal goal-id (mevedel-goal-id goal))))
        (mevedel-goal--plan-metadata-put session :revision-pending nil)
        (mevedel-goal--save-session-state session chat-buffer)
        (when-let* (((eq (mevedel-goal-status goal) 'active))
                    ((eq (mevedel-goal-phase goal) 'awaiting-approval))
                    (current-plan (mevedel-plan-current-body session))
                    ((equal input-hash (mevedel-plan-hash current-plan))))
          (let* ((replacement (plist-get result :plan))
                 (replacement-hash
                  (and (stringp replacement)
                       (not (string-blank-p replacement))
                       (mevedel-plan-hash replacement)))
                 (previous-metadata
                  (copy-tree (mevedel-session-plan-metadata session)))
                 persisted)
            (cl-labels
                ((present (plan reason)
                   (when-let* ((boundary
                                (mevedel-goal--automatic-request-boundary
                                 goal session t)))
                     (setq reason
                           (format
                            "%s\n\n%s"
                            reason
                            (mevedel-goal--settle-automatic-request-boundary
                             goal session boundary 'planner-revision))))
                   (mevedel-goal-present-plan plan chat-buffer reason)))
              (if (and replacement-hash
                       (not (equal input-hash replacement-hash)))
                  (condition-case err
                      (progn
                        (mevedel-plan-write-current
                         replacement session chat-buffer
                         (mevedel-goal--revision-plan-relative-path
                          goal revision))
                        (mevedel-goal--plan-metadata-put
                         session :revision-count revision)
                        (mevedel-goal--record-plan-revision
                         goal session chat-buffer revision
                         :replacement-plan-hash replacement-hash
                         :planner-provider (plist-get result :provider)
                         :planner-effort (plist-get result :effort)
                         :settlement-state 'settled
                         :settled-at (format-time-string "%FT%T%z"))
                        (setq persisted t)
                        (mevedel-goal--guard-current-plan goal chat-buffer))
                    (error
                     (unless persisted
                       (setf (mevedel-session-plan-metadata session)
                             previous-metadata))
                     (ignore-errors
                       (mevedel-goal--record-plan-revision
                        goal session chat-buffer revision
                        :replacement-plan-hash replacement-hash
                        :planner-provider (plist-get result :provider)
                        :planner-effort (plist-get result :effort)
                        :settlement-state 'failed
                        :failure (error-message-string err)
                        :failed-at (format-time-string "%FT%T%z")))
                     (present
                      (if persisted replacement input-plan)
                      (mevedel-goal--revision-escalation-reason
                       decision
                       (list
                        :error
                        (format
                         "Could not persist automatic plan revision: %s"
                         (error-message-string err)))))))
                (let ((failure
                       (if (and replacement-hash
                                (equal input-hash replacement-hash))
                           (plist-put
                            (copy-sequence result) :error
                            "Planner returned the same plan after guardian feedback")
                         (if (plist-get result :error)
                             result
                           (plist-put
                            (copy-sequence result) :error
                            "Planner returned no complete replacement plan")))))
                  (condition-case err
                      (mevedel-goal--record-plan-revision
                       goal session chat-buffer revision
                       :replacement-plan-hash replacement-hash
                       :planner-provider (plist-get result :provider)
                       :planner-effort (plist-get result :effort)
                       :settlement-state 'failed
                       :failure (plist-get failure :error)
                       :failed-at (format-time-string "%FT%T%z"))
                    (error
                     (setq failure
                           (plist-put
                            (copy-sequence failure) :error
                            (format "%s; audit persistence failed: %s"
                                    (plist-get failure :error)
                                    (error-message-string err))))))
                  (present
                   input-plan
                   (mevedel-goal--revision-escalation-reason
                    decision failure)))))))))))

(defun mevedel-goal--start-plan-revision
    (goal plan decision chat-buffer)
  "Start one planner correction for PLAN after guardian DECISION."
  (let* ((session (buffer-local-value 'mevedel--session chat-buffer))
         (goal-id (mevedel-goal-id goal))
         (plan-hash (mevedel-plan-hash plan))
         (revision
          (1+ (or (plist-get (mevedel-session-plan-metadata session)
                             :revision-count)
                  0))))
    (if-let* ((boundary
               (mevedel-goal--automatic-request-boundary
                goal session t)))
        (let ((blocker
               (mevedel-goal--settle-automatic-request-boundary
                goal session boundary 'planner-revision)))
          (mevedel-goal--enqueue-event-reminder
           session "automatic plan revision stopped at the user boundary")
          (mevedel-goal-present-plan
           plan chat-buffer
           (mevedel-goal--revision-escalation-reason
            decision (list :error blocker))))
      (mevedel-goal--plan-metadata-put session :revision-pending t)
      (mevedel-goal--save-session-state session chat-buffer)
      (condition-case err
          (progn
            (mevedel-goal--record-plan-revision
             goal session chat-buffer revision
             :input-plan-hash plan-hash
             :verdict (plist-get decision :verdict)
             :reason (plist-get decision :reason)
             :feedback (copy-sequence (plist-get decision :feedback))
             :guardian-provider (plist-get decision :provider)
             :guardian-effort (plist-get decision :effort)
             :settlement-state 'started
             :started-at (format-time-string "%FT%T%z"))
            (funcall
             mevedel-goal-planner-revision-function
             goal plan decision chat-buffer
             (lambda (result)
               (mevedel-goal--planner-revision-finished
                goal-id revision plan plan-hash decision chat-buffer result))))
        (error
         (mevedel-goal--plan-metadata-put session :revision-pending nil)
         (mevedel-goal--save-session-state session chat-buffer)
         (ignore-errors
           (mevedel-goal--record-plan-revision
            goal session chat-buffer revision
            :settlement-state 'failed
            :failure (error-message-string err)
            :failed-at (format-time-string "%FT%T%z")))
         (mevedel-goal-present-plan
          plan chat-buffer
          (mevedel-goal--revision-escalation-reason
           decision
           (list :error
                 (format "Planner revision failed: %s"
                         (error-message-string err))))))))))

(defun mevedel-goal--new-id ()
  "Return a fresh compact Goal identifier."
  (format "%s-%06x" (format-time-string "%Y%m%d-%H%M%S")
          (random #x1000000)))

(defun mevedel-goal--validate-objective (objective)
  "Return normalized non-empty OBJECTIVE or signal `user-error'."
  (unless (and (stringp objective)
               (not (string-blank-p objective)))
    (user-error "Goal objective must not be blank"))
  (string-trim objective))

(defun mevedel-goal--current ()
  "Return the current session Goal or signal `user-error'."
  (unless (bound-and-true-p mevedel--session)
    (user-error "No mevedel session in this buffer"))
  (or (mevedel-session-goal mevedel--session)
      (user-error "No current Goal")))

(defun mevedel-goal-description (&optional goal)
  "Return a compact user-facing description of GOAL."
  (let ((goal (or goal (mevedel-goal--current))))
    (format "Goal %s [%s/%s, cycle %s]: %s%s"
            (mevedel-goal-id goal)
            (mevedel-goal-status goal)
            (mevedel-goal-phase goal)
            (mevedel-goal-cycle goal)
            (mevedel-goal-objective goal)
            (if-let* ((reason (mevedel-goal-reason goal)))
                (format " (%s)" reason)
              ""))))

(defun mevedel-goal-pause ()
  "Pause the current Goal after any active request settles."
  (let ((goal (mevedel-goal--current)))
    (when (eq (mevedel-goal-status goal) 'complete)
      (user-error "Completed Goal cannot be paused"))
    (if (or (bound-and-true-p mevedel--current-request)
            (mevedel-goal-approval-request-pending-p mevedel--session))
        (progn
          (setf (mevedel-goal-pause-requested goal) t)
          (mevedel-goal--enqueue-event-reminder
           mevedel--session "pause requested; stop after the active request"))
      (setf (mevedel-goal-status goal) 'paused
            (mevedel-goal-pause-requested goal) nil
            (mevedel-goal-reason goal) "Paused by user")
      (mevedel-goal--enqueue-event-reminder
       mevedel--session "paused by the user")
      (mevedel-goal--save-session-state mevedel--session (current-buffer)))
    goal))

(defun mevedel-goal-edit (objective)
  "Replace the current Goal OBJECTIVE while preserving its identity and cycles."
  (let ((goal (mevedel-goal--current)))
    (setf (mevedel-goal-objective goal)
          (mevedel-goal--validate-objective objective)
          (mevedel-goal-status goal) 'paused
          (mevedel-goal-phase goal) 'planning
          (mevedel-goal-current-plan goal) nil
          (mevedel-goal-pause-requested goal) nil
          (mevedel-goal-reason goal)
          "Goal objective edited; use /goal resume to replan")
    (mevedel-plan-approval-abort mevedel--session)
    (mevedel-goal--enqueue-event-reminder
     mevedel--session "objective edited; replan before implementation")
    (mevedel-goal--save-session-state mevedel--session (current-buffer))
    goal))

(defun mevedel-goal-set-token-budget (budget)
  "Set current Goal aggregate token BUDGET, or nil for unlimited."
  (interactive
   (list (let ((text (read-string "Goal token budget (blank = unlimited): ")))
           (unless (string-blank-p text)
             (string-to-number text)))))
  (unless (or (null budget) (and (integerp budget) (> budget 0)))
    (user-error "Goal token budget must be a positive integer or nil"))
  (let ((goal (mevedel-goal--current)))
    (setf (mevedel-goal-token-budget goal) budget)
    (setq-local mevedel-goal-token-budget budget)
    (mevedel-goal--save-session-state mevedel--session (current-buffer))
    goal))

(defun mevedel-goal--apply-automatic-approval-policy (goal chat-buffer)
  "Apply GOAL's automatic policy at its pending boundary in CHAT-BUFFER."
  (when-let* ((plan (mevedel-plan-current-body mevedel--session)))
    (let ((audit (mevedel-goal--guardian-decision-for-plan goal plan)))
      (pcase (plist-get audit :verdict)
        ('approve
         (if (mevedel-goal-continuation-ready-p
              mevedel--session 'guardian 'implementing)
             (mevedel-goal--approval-callback
              plan chat-buffer
              (list :context (mevedel-goal-implementation-context goal)))
           (mevedel-goal-present-plan
            plan chat-buffer
            "Automatic approval deferred because user input or an interaction is pending.")))
        ('ask
         (mevedel-goal-present-plan
          plan chat-buffer (plist-get audit :reason)))
        (_
         (mevedel-goal--guard-current-plan goal chat-buffer))))))

(defun mevedel-goal-set-approval-policy (policy)
  "Set the current Goal approval POLICY to `supervised' or `automatic'."
  (interactive
   (list (intern (completing-read "Goal approval policy: "
                                  '("supervised" "automatic") nil t))))
  (unless (memq policy '(supervised automatic))
    (user-error "Unknown Goal approval policy: %S" policy))
  (let ((goal (mevedel-goal--current)))
    (unless (mevedel-goal-owned-by-session-p goal mevedel--session)
      (user-error "Goal continuation belongs to its handoff target"))
    (when (eq (mevedel-goal-status goal) 'complete)
      (user-error "Completed Goal has no approval policy to change"))
    (unless (eq policy (mevedel-goal-approval-policy goal))
      (let ((prefix "Goal lifecycle event: approval policy changed to "))
        (setf (mevedel-goal-approval-policy goal) policy
              (mevedel-session-pending-reminders mevedel--session)
              (append
               (cl-remove-if
                (lambda (body) (string-prefix-p prefix body))
                (mevedel-session-pending-reminders mevedel--session))
               (list (concat prefix (symbol-name policy))))))
      (mevedel-goal--save-session-state mevedel--session (current-buffer))
      (when (and (eq policy 'automatic)
                 (eq (mevedel-goal-status goal) 'active)
                 (eq (mevedel-goal-phase goal) 'awaiting-approval))
        (mevedel-plan-approval-abort mevedel--session 'policy-changed)
        (unless (mevedel-goal-approval-request-pending-p mevedel--session)
          (mevedel-goal--apply-automatic-approval-policy
           goal (current-buffer)))))
    (message "mevedel: Goal approval policy is %s" policy)
    goal))

(defun mevedel-goal-clear ()
  "Remove current Goal state while preserving transcript, artifacts, and work."
  (mevedel-goal--current)
  (when (bound-and-true-p mevedel--current-request)
    (user-error "Wait for or abort the active request before clearing Goal"))
  (mevedel-plan-approval-abort mevedel--session)
  (mevedel-goal--enqueue-event-reminder
   mevedel--session "cleared; transcript and artifacts were preserved")
  (setf (mevedel-session-goal mevedel--session) nil
        (mevedel-session-plan-metadata mevedel--session) nil)
  (mevedel-goal--save-session-state mevedel--session (current-buffer))
  nil)

(defun mevedel-goal-resume (&optional input)
  "Resume the current paused or blocked Goal, incorporating optional INPUT."
  (let ((goal (mevedel-goal--current)))
    (when (bound-and-true-p mevedel--current-request)
      (user-error "A request is already active"))
    (unless (mevedel-goal-owned-by-session-p goal mevedel--session)
      (user-error "Goal continuation belongs to its handoff target"))
    (unless (memq (mevedel-goal-status goal) '(paused blocked))
      (user-error "Goal is not paused or blocked"))
    (when (local-variable-p 'mevedel-goal-token-budget)
      (setf (mevedel-goal-token-budget goal) mevedel-goal-token-budget))
    (unless (mevedel-goal--budget-available-p goal mevedel--session)
      (user-error "%s" (mevedel-goal-reason goal)))
    (when-let* ((reason (mevedel-goal-reason goal)))
      (setf (mevedel-goal-review-findings goal)
            (string-join
             (delq nil
                   (list (mevedel-goal-review-findings goal)
                         (format "Prior stop reason: %s" reason)
                         (and input (not (string-blank-p input))
                              (format "Resume input: %s" input))))
             "\n")))
    (when (eq (mevedel-goal-status goal) 'blocked)
      (setf (mevedel-goal-phase goal) 'planning))
    (setf (mevedel-goal-status goal) 'active
          (mevedel-goal-pause-requested goal) nil
          (mevedel-goal-reason goal) nil)
    (mevedel-goal--enqueue-event-reminder
     mevedel--session "resumed from persisted phase and checkpoint")
    (condition-case err
        (let ((checkpoint (mevedel-goal-checkpoint goal)))
          (pcase (mevedel-goal-phase goal)
            ('planning
             (mevedel-goal--dispatch-phase
              'planning
              (if (and (eq (plist-get checkpoint :phase) 'planning)
                       (memq (plist-get checkpoint :dispatch-state)
                             '(started unknown failed)))
                  (plist-get checkpoint :input)
                (mevedel-goal--planning-prompt goal))
              (format "Resume Goal %s" (mevedel-goal-id goal))))
          ('awaiting-approval
           (unless
               (and (eq (mevedel-goal-approval-policy goal) 'automatic)
                    (mevedel-goal--recover-plan-revision
                     goal mevedel--session (current-buffer)))
             (cond
              ((not (eq (mevedel-goal-approval-policy goal) 'automatic))
               (mevedel-goal-restore-pending-approval
                mevedel--session (current-buffer)))
              ((and (eq (plist-get checkpoint :phase) 'planning)
                    (eq (plist-get checkpoint :dispatch-state) 'settled)
                    (mevedel-goal-continuation-ready-p
                     mevedel--session 'planning 'guardian))
               (mevedel-goal--guard-current-plan goal (current-buffer)))
              ((and (eq (plist-get checkpoint :phase) 'guardian)
                    (memq (plist-get checkpoint :dispatch-state)
                          '(started unknown failed)))
               (mevedel-goal--guard-current-plan goal (current-buffer)))
              ((and (eq (plist-get checkpoint :phase) 'guardian)
                    (eq (plist-get checkpoint :dispatch-state) 'settled)
                    (mevedel-goal--guardian-approved-p goal)
                    (mevedel-goal-continuation-ready-p
                     mevedel--session 'guardian 'implementing))
               (when-let* ((plan
                            (mevedel-plan-current-body mevedel--session)))
                 (mevedel-goal--approval-callback
                  plan (current-buffer)
                  (list :context
                        (mevedel-goal-implementation-context goal)))))
              (t
               (mevedel-goal-restore-pending-approval
                mevedel--session (current-buffer))))))
          ('implementing
           (if (and (memq (plist-get checkpoint :dispatch-state)
                          '(prepared failed))
                    (not (plist-get checkpoint :request-started)))
               (let ((fsm
                      (mevedel-goal--call-with-workload
                       'implementation
                       (lambda ()
                         (mevedel--implement-plan
                          (mevedel-goal--refresh-request-context
                           goal (plist-get checkpoint :input))))
                       'implementing
                       (mevedel-goal--refresh-request-context
                        goal (plist-get checkpoint :input)))))
                 (when fsm
                   (setf (gptel-fsm-info fsm)
                         (plist-put (gptel-fsm-info fsm)
                                    :mevedel-goal-phase 'implementing))))
             (setf (mevedel-goal-phase goal) 'reviewing)
             (mevedel-goal--dispatch-phase
              'reviewing (mevedel-goal--recovery-prompt goal checkpoint)
              "Audit interrupted Goal implementation")))
            ('reviewing
             (mevedel-goal--dispatch-phase
              'reviewing
              (if (and (eq (plist-get checkpoint :phase) 'reviewing)
                       (memq (plist-get checkpoint :dispatch-state)
                             '(started unknown failed)))
                  (plist-get checkpoint :input)
                (mevedel-goal--review-prompt goal))
              "Resume Goal review"))))
      (error
       (setf (mevedel-goal-status goal) 'paused
             (mevedel-goal-reason goal) (error-message-string err))
       (mevedel-goal--save-session-state mevedel--session (current-buffer))
       (signal (car err) (cdr err))))
    goal))

(defun mevedel-goal-start
    (objective &optional display-text approval-policy prompt-submission)
  "Start a Goal for OBJECTIVE in the current session.
DISPLAY-TEXT is the user-facing form of the planning turn.
APPROVAL-POLICY is `supervised' by default or explicitly `automatic'.
PROMPT-SUBMISSION owns accepted hook context for the initial planning turn."
  (unless (bound-and-true-p mevedel--session)
    (user-error "No mevedel session in this buffer"))
  (when-let* ((existing (mevedel-session-goal mevedel--session)))
    (when (bound-and-true-p mevedel--current-request)
      (user-error "A request is already active"))
    (when (and (not (eq (mevedel-goal-status existing) 'complete))
               (not (yes-or-no-p "Replace the unfinished Goal? ")))
      (user-error "Goal replacement aborted")))
  (setq approval-policy (or approval-policy 'supervised))
  (unless (memq approval-policy '(supervised automatic))
    (error "Unknown Goal approval policy: %s" approval-policy))
  (when (fboundp 'mevedel-plan-mode-exit)
    (mevedel-plan-mode-exit mevedel--session))
  (mevedel-session-persistence-ensure-files
   mevedel--session (current-buffer))
  (let ((previous-goal (mevedel-session-goal mevedel--session))
        (previous-plan-metadata
         (mevedel-session-plan-metadata mevedel--session))
        (goal
         (mevedel-goal--create
          :id (mevedel-goal--new-id)
          :objective (mevedel-goal--validate-objective objective)
          :status 'active
          :phase 'planning
          :approval-policy approval-policy
          :owner-session (mevedel-session-session-id mevedel--session)
          :execution-home
          (list :kind mevedel-goal-execution-home
                :directory
                (file-name-as-directory
                 (expand-file-name
                  (or (mevedel-session-working-directory mevedel--session)
                      default-directory)))
                :session-id (mevedel-session-session-id mevedel--session)
                :locked nil)
          :implementation-context
          (if (eq mevedel-goal-execution-home 'worktree)
              'focused
            mevedel-goal-implementation-context)
          :token-budget mevedel-goal-token-budget
          :token-usage 0
          :cycle 1
          :cycles (list (list :cycle 1
                              :started-at (format-time-string "%FT%T%z"))))))
    (setf (mevedel-session-goal mevedel--session) goal
          (mevedel-session-plan-metadata mevedel--session) nil)
    (when (fboundp 'mevedel-telemetry-record)
      (mevedel-telemetry-record
       mevedel--session 'goal-start
       :approval-policy approval-policy
       :implementation-context (mevedel-goal-implementation-context goal)
       :execution-home (plist-get (mevedel-goal-execution-home goal) :kind)
       :token-budget (mevedel-goal-token-budget goal)))
    (mevedel-goal--enqueue-event-reminder
     mevedel--session "started; authoritative Goal context is attached")
    (condition-case err
        (mevedel-goal--dispatch-phase
         'planning (mevedel-goal--planning-prompt goal)
         (or display-text (mevedel-goal-objective goal)) prompt-submission)
      (error
       (if (mevedel-goal-checkpoint goal)
           (message "mevedel: goal paused before planning dispatch: %s"
                    (error-message-string err))
         (setf (mevedel-session-goal mevedel--session) previous-goal
               (mevedel-session-plan-metadata mevedel--session)
               previous-plan-metadata)
         (signal (car err) (cdr err)))))
    (when previous-goal
      (setf (mevedel-session-goal mevedel--session) previous-goal
            (mevedel-session-plan-metadata mevedel--session)
            previous-plan-metadata)
      (mevedel-plan-approval-abort mevedel--session)
      (setf (mevedel-session-goal mevedel--session) goal
            (mevedel-session-plan-metadata mevedel--session) nil))
    goal))

(defun mevedel-goal--dispatch-gptel
    (phase prompt display-text &optional prompt-submission)
  "Dispatch PHASE with PROMPT, showing DISPLAY-TEXT in the transcript."
  (let ((fsm (mevedel--submit-generated-turn
              prompt display-text prompt-submission)))
    (when fsm
      (setf (gptel-fsm-info fsm)
            (plist-put (gptel-fsm-info fsm)
                       :mevedel-goal-phase phase)))
    fsm))

(defun mevedel-goal--call-with-workload
    (workload function &optional checkpoint-phase checkpoint-input)
  "Call FUNCTION with WORKLOAD's resolved request policy.
When CHECKPOINT-PHASE is non-nil, durably record CHECKPOINT-INPUT before
dispatch and attach the attempt identity to the returned FSM."
  (require 'mevedel-models)
  (let* ((checkpoint-enabled
          (and checkpoint-phase
               (bound-and-true-p mevedel--session)
               (mevedel-session-goal mevedel--session)))
         (old-backend gptel-backend)
         (old-model gptel-model)
         (old-effort (and (boundp 'gptel-reasoning-effort)
                          gptel-reasoning-effort))
         policy
         dispatch-started)
    (unwind-protect
        (condition-case err
            (progn
              (when (and checkpoint-enabled
                         (memq checkpoint-phase '(implementing reviewing)))
                (mevedel-goal--assert-execution-home
                 (mevedel-session-goal mevedel--session)
                 mevedel--session))
              (setq policy (mevedel-model-resolve-workload workload))
              (mevedel-goal--record-phase-policy workload policy)
              (when checkpoint-enabled
                (mevedel-goal--checkpoint-prepare
                 checkpoint-phase checkpoint-input workload policy))
              (when (and checkpoint-enabled
                         (fboundp 'mevedel-telemetry-record))
                (mevedel-telemetry-record
                 mevedel--session 'goal-phase-dispatch
                 :phase checkpoint-phase
                 :workload workload
                 :attempt-id
                 (plist-get
                  (mevedel-goal-checkpoint
                   (mevedel-session-goal mevedel--session))
                  :attempt-id)
                 :backend (ignore-errors
                            (gptel-backend-name
                             (plist-get policy :backend)))
                 :model (plist-get policy :model)
                 :effort (plist-get policy :effort)))
              (setq-local gptel-backend (plist-get policy :backend)
                          gptel-model (plist-get policy :model)
                          gptel-reasoning-effort (plist-get policy :effort))
              (when checkpoint-enabled
                (setq dispatch-started t)
                (mevedel-goal--checkpoint-state
                 'started :request-started t
                 :started-at (format-time-string "%FT%T%z")))
              (let* ((workload-transform
                      (lambda (_fsm)
                        (setq-local
                         gptel-backend (plist-get policy :backend)
                         gptel-model (plist-get policy :model)
                         gptel-reasoning-effort
                         (plist-get policy :effort))))
                     (gptel-prompt-transform-functions
                      (cons workload-transform
                            gptel-prompt-transform-functions))
                     (fsm (funcall function)))
                (when (and checkpoint-enabled fsm)
                  (condition-case nil
                      (setf (gptel-fsm-info fsm)
                            (plist-put
                             (gptel-fsm-info fsm)
                             :mevedel-goal-attempt-id
                             (plist-get
                              (mevedel-goal-checkpoint
                               (mevedel-session-goal mevedel--session))
                              :attempt-id)))
                    (wrong-type-argument nil)))
                fsm))
          (error
           (when checkpoint-enabled
             (let* ((goal (mevedel-session-goal mevedel--session))
                    (checkpoint (mevedel-goal-checkpoint goal))
                    (reason
                     (format "Request startup failed; switch provider or preset, then resume: %s"
                             (error-message-string err))))
               (setf (mevedel-goal-status goal) 'paused
                     (mevedel-goal-reason goal) reason)
               (unless (and checkpoint
                            (eq checkpoint-phase
                                (plist-get checkpoint :phase)))
                 (mevedel-goal--checkpoint-prepare
                  checkpoint-phase checkpoint-input workload
                  (list :backend old-backend :model old-model
                        :effort old-effort)))
               (mevedel-goal--checkpoint-state
                (if dispatch-started 'unknown 'failed)
                :request-started dispatch-started
                :error (error-message-string err)
                :failed-at (format-time-string "%FT%T%z"))))
           (signal (car err) (cdr err))))
      (setq-local gptel-backend old-backend
                  gptel-model old-model
                  gptel-reasoning-effort old-effort))))

(defun mevedel-goal--dispatch-phase
    (phase prompt display-text &optional prompt-submission)
  "Dispatch Goal PHASE with PROMPT and DISPLAY-TEXT."
  (unless (memq phase '(planning reviewing))
    (error "Goal phase cannot dispatch: %s" phase))
  (let* ((goal (mevedel-session-goal mevedel--session))
         (prompt (mevedel-goal--refresh-request-context goal prompt)))
    (mevedel-goal--call-with-workload
     (if (eq phase 'planning) 'planning 'review)
     (lambda ()
       (funcall mevedel-goal-dispatch-function
                phase prompt display-text prompt-submission))
     phase prompt)))

(defun mevedel-plan-approval--current-session ()
  "Resolve the session struct that owns the pending Plan approval."
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
     (display-warning
      'mevedel
      (format "plan approval: %s callback error: %S" phase err)
      :warning)
     (unless retain
       (mevedel-queue--unregister-entry-interaction entry))
     nil)))

(defun mevedel-plan-approval-present (entry &optional session)
  "Replace SESSION's pending Plan approval with ENTRY and render it."
  (let ((session (or session (mevedel-plan-approval--current-session))))
    (if (not session)
        (progn
          (display-warning 'mevedel "Plan approval presented with no session"
                           :warning)
          (mevedel-plan-approval--deliver entry 'aborted "no-session"))
      (let ((entry (plist-put (copy-sequence entry) :session session)))
        (when-let* ((previous
                    (mevedel-session-pending-plan-approval session)))
          (setf (mevedel-session-pending-plan-approval session) nil)
          (mevedel-plan-approval--deliver
           previous 'superseded "supersede"))
        (setf (mevedel-session-pending-plan-approval session) entry)
        (mevedel-plan-approval-render session)))))

(defun mevedel-plan-approval-render (&optional session)
  "Render SESSION's pending Plan approval."
  (when-let* ((session (or session
                           (mevedel-plan-approval--current-session)))
              (entry (mevedel-session-pending-plan-approval session)))
    (condition-case err
        (if-let* ((renderer (plist-get entry :renderer)))
            (funcall renderer entry)
          (mevedel-plan-approval--render-entry entry))
      (error
       (display-warning
        'mevedel (format "plan approval: render error: %S" err) :warning)
       (mevedel-plan-approval-abort session)))))

(defun mevedel-goal--ensure-implementation-allowed (entry outcome)
  "Signal when ENTRY may not be implemented with OUTCOME yet."
  (when (and (or (mevedel-goal--approval-outcome-p outcome)
                 (and (proper-list-p outcome)
                      (plist-get outcome :accept)))
             (mevedel-session-queued-user-messages
              (plist-get entry :session)))
    (user-error
     "Resolve queued messages before implementing the plan (edit or clear the queued message batch)")))

(defun mevedel-plan-approval-settle (entry outcome)
  "Settle pending Plan approval ENTRY with OUTCOME."
  (mevedel-goal--ensure-implementation-allowed entry outcome)
  (let* ((session (plist-get entry :session))
         (pending (and session
                       (mevedel-session-pending-plan-approval session))))
    (if (not (eq entry pending))
        (display-warning
         'mevedel "Plan approval: stale interaction settlement ignored"
         :warning)
      (when (mevedel-plan-approval--deliver entry outcome "settle" t)
        (when (eq entry (mevedel-session-pending-plan-approval session))
          (setf (mevedel-session-pending-plan-approval session) nil))))))

(defun mevedel-plan-approval-abort (&optional session outcome)
  "Settle SESSION's pending Plan approval with OUTCOME or `aborted'."
  (when-let* ((session (or session
                           (mevedel-plan-approval--current-session)))
              (entry (mevedel-session-pending-plan-approval session)))
    (setf (mevedel-session-pending-plan-approval session) nil)
    (mevedel-plan-approval--deliver
     entry (or outcome 'aborted) "abort")))

(defconst mevedel-goal--implementation-modes
  '(ask auto full-auto)
  "Plan implementation permission modes cycled by the approval prompt.")

(defun mevedel-goal--implementation-mode-label (mode)
  "Return compact display label for implementation MODE."
  (pcase mode
    ('auto "auto")
    ('full-auto "full-auto")
    (_ "ask")))

(defun mevedel-goal--next-implementation-mode (mode)
  "Return the next implementation mode after MODE."
  (let* ((modes mevedel-goal--implementation-modes)
         (tail (memq mode modes)))
    (or (cadr tail) (car modes))))

(defun mevedel-plan-approval--entry-implementation-mode (entry)
  "Return ENTRY's selected implementation permission mode."
  (or (mevedel-queue--entry-metadata-get entry :implementation-mode)
      (when-let* ((session (plist-get entry :session)))
        (mevedel-session-permission-mode session))
      'ask))

(defun mevedel-plan-approval--cycle-entry-implementation-mode (entry)
  "Cycle ENTRY's implementation mode and rerender the plan prompt."
  (mevedel-queue--entry-metadata-put
   entry :implementation-mode
   (mevedel-goal--next-implementation-mode
    (mevedel-plan-approval--entry-implementation-mode entry)))
  (mevedel-plan-approval--render-entry entry))

(defun mevedel-plan-approval--entry-execution-home (entry)
  "Return ENTRY's selected Goal execution-home kind."
  (or (mevedel-queue--entry-metadata-get entry :execution-home)
      (when-let* ((session (plist-get entry :session))
                  (goal (mevedel-session-goal session)))
        (plist-get (mevedel-goal-execution-home goal) :kind))
      'current))

(defun mevedel-plan-approval--cycle-entry-execution-home (entry)
  "Toggle ENTRY's implementation home and rerender the plan prompt."
  (unless (mevedel-plan-approval--entry-execution-home-mutable-p entry)
    (user-error "Goal execution home is locked after first approval"))
  (mevedel-queue--entry-metadata-put
   entry :execution-home
   (if (eq (mevedel-plan-approval--entry-execution-home entry) 'current)
       'worktree
     'current))
  (mevedel-plan-approval--render-entry entry))

(defun mevedel-plan-approval--entry-execution-home-mutable-p (entry)
  "Return non-nil when ENTRY may select the Goal execution home."
  (when-let* ((session (plist-get entry :session))
              (goal (mevedel-session-goal session)))
    (not (plist-get (mevedel-goal-execution-home goal) :locked))))

(defun mevedel-plan-approval--keys-line
    (implementation-mode execution-home home-mutable-p)
  "Return plan key help for IMPLEMENTATION-MODE and EXECUTION-HOME.
HOME-MUTABLE-P means the first approval may still select another home."
  (concat
   (propertize "Keys: " 'font-lock-face 'help-key-binding)
   (propertize "RET" 'font-lock-face 'help-key-binding)
   " implement (full)  "
   (propertize "I" 'font-lock-face 'help-key-binding)
   " implement (focused)  "
   (propertize "TAB" 'font-lock-face 'help-key-binding)
   "/"
   (propertize "m" 'font-lock-face 'help-key-binding)
   (format " mode: %s  "
           (mevedel-goal--implementation-mode-label
            implementation-mode))
   (if home-mutable-p
       (concat (propertize "w" 'font-lock-face 'help-key-binding)
               (format " home: %s  " execution-home))
     (format "home: %s (locked)  " execution-home))
   (propertize "f" 'font-lock-face 'help-key-binding)
   " feedback draft  "
   (propertize "q" 'font-lock-face 'help-key-binding)
   " cancel\n"))

(defun mevedel-plan-approval--display-body (plan-markdown)
  "Return PLAN-MARKDOWN fontified for the plan approval interaction zone."
  (if (fboundp 'mevedel-view--fontify-as)
      (mevedel-view--fontify-as plan-markdown 'markdown-mode)
    plan-markdown))

(defun mevedel-plan-approval--render-entry (entry)
  "Render Plan approval ENTRY in the interaction zone."
  (require 'mevedel-interaction-prompt)
  (let ((plan-markdown (plist-get entry :body))
        (chat-buffer (plist-get entry :chat-buffer))
        overlay)
    (cl-labels
        ((settle (sym)
           (when overlay
             (mevedel--prompt--settle overlay sym)))
         (implementation-outcome (context)
           (list :context context
                 :execution-home
                 (mevedel-plan-approval--entry-execution-home entry)
                 :mode (mevedel-plan-approval--entry-implementation-mode
                        entry)))
         (implement-plan ()
           (interactive)
           (let ((outcome (implementation-outcome 'full)))
             (mevedel-goal--ensure-implementation-allowed entry outcome)
             (settle outcome)))
         (implement-plan-clear ()
           (interactive)
           (let ((outcome (implementation-outcome 'focused)))
             (mevedel-goal--ensure-implementation-allowed entry outcome)
             (settle outcome)))
         (cycle-implementation-mode ()
           (interactive)
           (mevedel-plan-approval--cycle-entry-implementation-mode entry))
         (cycle-execution-home ()
           (interactive)
           (mevedel-plan-approval--cycle-entry-execution-home entry))
         (reject-plan-feedback ()
           (interactive)
           (settle 'feedback-draft))
         (abort-plan ()
           (interactive) (settle 'aborted)))
      (let ((target-buf
             (if (fboundp 'mevedel-view--interaction-target-buffer)
                 (mevedel-view--interaction-target-buffer chat-buffer)
               (error "No live view for queued prompt"))))
        (mevedel-queue--entry-metadata-put entry :view-buffer target-buf)
        (with-current-buffer target-buf
          (let* ((keymap (make-sparse-keymap))
                 (interaction-id (or (mevedel-queue--entry-metadata-get
                                      entry :interaction-id)
                                     (let ((id (list :plan (gensym "plan-"))))
                                       (mevedel-queue--entry-metadata-put
                                        entry :interaction-id id)
                                       id)))
                 (body
                  (concat
                   "\n"
                   (when-let* ((reason (plist-get entry :guardian-reason)))
                     (concat
                      (propertize "Automatic approval escalated\n"
                                  'font-lock-face 'warning)
                      reason "\n\n"))
                   (mevedel-plan-approval--display-body plan-markdown)
                   "\n\n"
                   (mevedel-plan-approval--keys-line
                    (mevedel-plan-approval--entry-implementation-mode entry)
                    (mevedel-plan-approval--entry-execution-home entry)
                    (mevedel-plan-approval--entry-execution-home-mutable-p
                     entry))
                   (propertize
                    "\n" 'font-lock-face
                    '(:inherit font-lock-string-face :underline t :extend t)))))
            (define-key keymap (kbd "RET") #'implement-plan)
            (define-key keymap (kbd "<return>") #'implement-plan)
            (define-key keymap (kbd "i") #'implement-plan)
            (define-key keymap (kbd "C-c C-c") #'implement-plan)
            (define-key keymap (kbd "I") #'implement-plan-clear)
            (define-key keymap (kbd "TAB") #'cycle-implementation-mode)
            (define-key keymap (kbd "<tab>") #'cycle-implementation-mode)
            (define-key keymap (kbd "m") #'cycle-implementation-mode)
            (define-key keymap (kbd "w") #'cycle-execution-home)
            (define-key keymap (kbd "f") #'reject-plan-feedback)
            (define-key keymap (kbd "q") #'abort-plan)
            (define-key keymap (kbd "C-c C-k") #'abort-plan)
            (define-key keymap (kbd "C-g") #'abort-plan)
            (setq overlay
                  (mevedel-view--interaction-register
                   (list :kind 'plan
                         :id interaction-id
                         :count 1
                         :body body
                         :priority 200
                         :keymap keymap
                         :help-echo nil
                         :entry entry
                         :activate
                         (lambda (outcome)
                           (mevedel-plan-approval-settle
                            entry outcome)))))
            (overlay-put overlay 'mevedel-plan t)
            (overlay-put overlay 'mevedel-user-request t)
            (overlay-put overlay 'mevedel--callback
                         (lambda (outcome)
                           (mevedel-plan-approval-settle
                            entry outcome)))
            (overlay-put overlay 'keymap keymap)
            (when (and (overlay-buffer overlay)
                       (= (point) (overlay-start overlay)))
              (when-let* ((buf-win (get-buffer-window target-buf)))
                (with-selected-window buf-win
                  (recenter-top-bottom 1))))))))))

(defun mevedel-goal--feedback-draft-text (plan-path &optional feedback)
  "Return editable Plan feedback draft text referencing PLAN-PATH.
When FEEDBACK is non-nil, prefill it in the feedback section."
  (format
   "Plan feedback:\n\n%s\n\nRevise the proposed plan to address the feedback. Treat the current plan artifact as reference-only: read it if needed, but do not edit it.\n\n%s\n\nEmit exactly one full replacement <proposed_plan> block.\n\nCurrent plan artifact: %s"
   (or feedback "")
   mevedel-goal--plan-guidance
   (or plan-path "latest plan artifact")))

(defun mevedel-goal--insert-feedback-draft
    (chat-buffer plan-path &optional feedback)
  "Insert an editable Plan feedback draft for PLAN-PATH in CHAT-BUFFER's view.
When FEEDBACK is non-nil, prefill it in the feedback section."
  (let ((target-buffer
         (if (fboundp 'mevedel-view--interaction-target-buffer)
             (mevedel-view--interaction-target-buffer chat-buffer)
           (error "No live view for Plan feedback draft"))))
    (unless (buffer-live-p target-buffer)
      (error "No live view for Plan feedback draft"))
    (with-current-buffer target-buffer
      (mevedel-view--clear-input)
      (goto-char (mevedel-view--input-start))
      (let ((start (point)))
        (insert (mevedel-goal--feedback-draft-text plan-path feedback))
        (goto-char start))
      (when (search-forward "Plan feedback:\n\n" nil t)
        (goto-char (match-end 0)))
      (when-let* ((window (get-buffer-window target-buffer)))
        (select-window window)))))

(defun mevedel-goal--approval-outcome-p (outcome)
  "Return non-nil when OUTCOME requests implementation."
  (and (proper-list-p outcome)
       (memq (plist-get outcome :context) '(full focused))))

(defun mevedel-goal--approval-implementation-mode (outcome)
  "Return implementation permission mode represented by OUTCOME."
  (let ((mode (and (consp outcome) (plist-get outcome :mode))))
    (if (memq mode '(ask auto full-auto))
        mode
      (or (mevedel-session-permission-mode mevedel--session) 'ask))))

(defun mevedel-goal--approval-execution-home (goal outcome)
  "Return execution-home kind selected for GOAL by OUTCOME."
  (let* ((recorded (plist-get (mevedel-goal-execution-home goal) :kind))
         (requested (and (consp outcome)
                         (plist-get outcome :execution-home)))
         (kind (if (memq requested '(current worktree))
                   requested
                 recorded)))
    (when (and (plist-get (mevedel-goal-execution-home goal) :locked)
               (not (eq kind recorded)))
      (user-error "Goal execution home is locked after first approval"))
    kind))

(defun mevedel-goal--approval-context (goal outcome)
  "Return implementation context selected for GOAL by OUTCOME."
  (let ((context (and (consp outcome) (plist-get outcome :context))))
    (if (memq context '(full focused))
        context
      (mevedel-goal-implementation-context goal))))

(defun mevedel-goal--accept-plan (plan-markdown session buffer)
  "Accept PLAN-MARKDOWN for SESSION in BUFFER and persist Goal references."
  (require 'mevedel-plan)
  (let* ((goal (mevedel-session-goal session))
         (artifacts
          (mevedel-plan-accept
           plan-markdown session buffer nil
           (mevedel-goal--current-plan-relative-path goal)
           (mevedel-goal--cycle-plan-relative-path goal)))
         (accepted (plist-get artifacts :accepted))
         (plan-hash (mevedel-plan-hash plan-markdown)))
    (mevedel-goal--ensure-reference-reminder session)
    (setq accepted (plist-put accepted :hash plan-hash))
    (setf (mevedel-goal-current-plan goal) accepted
          (mevedel-goal-phase goal) 'implementing)
    (mevedel-goal--enqueue-event-reminder
     session "accepted plan; implementation started")
    (mevedel-goal--cycle-put goal :plan (plist-get accepted :path))
    (mevedel-goal--cycle-put goal :plan-hash plan-hash)
    (mevedel-goal--cycle-put
     goal :accepted-at (format-time-string "%FT%T%z"))
    (mevedel-goal--persist-cycle-index goal session buffer)
    (mevedel-goal--persist-checkpoint session buffer)
    accepted))

(defun mevedel-goal--transfer-to-worktree (goal source-buffer plan-markdown)
  "Transfer GOAL and accepted PLAN-MARKDOWN from SOURCE-BUFFER.
Return `(:buffer BUFFER :accepted ARTIFACT)' for the sole target owner."
  (require 'mevedel-worktree)
  (with-current-buffer source-buffer
    (let* ((source-session mevedel--session)
           (source-plan-metadata
            (copy-tree (mevedel-session-plan-metadata source-session)))
           (branch (format "goal/%s" (mevedel-goal-id goal)))
           (result (mevedel-worktree-create-session
                    branch (mevedel-goal-objective goal) t))
           (target-buffer (plist-get result :buffer))
           (target-directory (plist-get result :directory))
           (target-session
            (buffer-local-value 'mevedel--session target-buffer))
           (_target-files
            (with-current-buffer target-buffer
              (mevedel-session-persistence-ensure-files
               target-session target-buffer)))
           (target-goal (copy-mevedel-goal goal))
           (target-id (mevedel-session-session-id target-session))
           (handoff (list :goal-id (mevedel-goal-id goal)
                          :target-session-id target-id
                          :target-directory target-directory
                          :state 'prepared))
           accepted)
      (setf (mevedel-session-preset-name target-session)
            (mevedel-session-preset-name source-session)
            (mevedel-session-preset-settings target-session)
            (copy-tree (mevedel-session-preset-settings source-session))
            (mevedel-session-permission-mode target-session)
            (mevedel-session-permission-mode source-session)
            (mevedel-session-permission-rules target-session)
            (copy-tree (mevedel-session-permission-rules source-session))
            (mevedel-goal-owner-session target-goal) target-id
            (mevedel-goal-execution-home target-goal)
            (list :kind 'worktree
                  :directory target-directory
                  :session-id target-id
                  :branch (plist-get result :branch)
                  :locked t)
            (mevedel-session-goal target-session) target-goal
            (mevedel-session-plan-metadata target-session) nil)
      (condition-case err
          (progn
            ;; Persist the handoff intent first.  A forceful stop can then
            ;; never leave the source capable of continuing independently.
            (setf (mevedel-session-goal-handoff source-session) handoff)
            (mevedel-goal--persist-checkpoint source-session source-buffer)
            (with-current-buffer target-buffer
              (require 'mevedel-presets)
              (mevedel-preset-restore-session target-session target-buffer)
              (setq accepted
                    (mevedel-goal--accept-plan
                     plan-markdown target-session target-buffer)))
            (setf (mevedel-session-goal source-session) nil
                  (plist-get handoff :state) 'complete
                  (mevedel-session-plan-metadata source-session) nil)
            (mevedel-goal--enqueue-event-reminder
             target-session "ownership transferred to this Goal worktree")
            (mevedel-goal--persist-checkpoint source-session source-buffer)
            (list :buffer target-buffer :accepted accepted))
        (error
         (setf (mevedel-session-goal source-session) goal
               (mevedel-session-goal-handoff source-session) nil
               (mevedel-session-plan-metadata source-session)
               source-plan-metadata)
         (let ((source-restored-p nil))
           (condition-case rollback-err
               (progn
               ;; Do not destroy the target until restored source ownership is
               ;; durable.  If this write fails, the prepared handoff remains
               ;; the source-side gate and the target is preserved for recovery.
                 (mevedel-goal--persist-checkpoint
                  source-session source-buffer)
                 (setq source-restored-p t)
                 (setf (mevedel-session-goal target-session) nil)
                 (with-current-buffer target-buffer
                   (mevedel-goal--persist-checkpoint
                    target-session target-buffer))
                 (when (buffer-live-p target-buffer)
                   (with-current-buffer target-buffer
                     (set-buffer-modified-p nil))
                   (kill-buffer target-buffer))
                 (let ((removed
                        (mevedel-worktree--git-result
                         (mevedel-session-working-directory source-session)
                         "worktree" "remove" "--force" target-directory)))
                   (if (eq 0 (plist-get removed :exit))
                       (let ((deleted
                              (mevedel-worktree--git-result
                               (mevedel-session-working-directory source-session)
                               "branch" "-D" branch)))
                         (unless (eq 0 (plist-get deleted :exit))
                           (display-warning
                            'mevedel
                            (format "Could not remove failed Goal branch %s: %s"
                                    branch (plist-get deleted :output))
                            :warning)))
                     (display-warning
                      'mevedel
                      (format "Could not remove failed Goal worktree %s: %s"
                              target-directory (plist-get removed :output))
                      :warning)))
                 nil)
             (error
              (setf (mevedel-session-goal-handoff source-session) handoff
                    (plist-get handoff :state) 'prepared)
              (if source-restored-p
                  (condition-case regate-err
                      (progn
                        (mevedel-goal--persist-checkpoint
                         source-session source-buffer)
                        (setf (mevedel-session-goal target-session) target-goal)
                        (with-current-buffer target-buffer
                          (mevedel-goal--persist-checkpoint
                           target-session target-buffer)))
                    (error
                     ;; The target remains non-runnable in memory.  Do not
                     ;; write a second owner when the source cannot be gated.
                     (error
                      "Goal transfer failed (%s); target cleanup failed (%s); source re-gating failed (%s)"
                      (error-message-string err)
                      (error-message-string rollback-err)
                      (error-message-string regate-err))))
                ;; Source restoration never reached disk, so its original
                ;; prepared handoff remains the durable gate.
                (setf (mevedel-session-goal target-session) target-goal)
                (ignore-errors
                  (with-current-buffer target-buffer
                    (mevedel-goal--persist-checkpoint
                     target-session target-buffer))))
              (error
               "Goal transfer failed (%s); rollback failed (%s); target preserved at %s"
               (error-message-string err)
               (error-message-string rollback-err)
               target-directory))))
         (signal (car err) (cdr err)))))))

(defun mevedel-goal--approval-callback
    (plan-markdown chat-buffer outcome)
  "Handle OUTCOME for a proposed PLAN-MARKDOWN in CHAT-BUFFER."
  (require 'mevedel-plan)
  (setq plan-markdown (mevedel--normalize-message-text plan-markdown))
  (when (buffer-live-p chat-buffer)
    (with-current-buffer chat-buffer
      (if (mevedel-goal--approval-outcome-p outcome)
          (let* ((goal (mevedel-session-goal mevedel--session)))
            (unless (and goal
                         (eq (mevedel-goal-status goal) 'active)
                         (eq (mevedel-goal-phase goal) 'awaiting-approval))
              (user-error "Goal is not awaiting plan approval"))
            (let* ((home-kind
                    (mevedel-goal--approval-execution-home goal outcome))
                   (context (mevedel-goal--approval-context goal outcome))
                   (home (copy-tree (mevedel-goal-execution-home goal)))
                   result)
              (setf (plist-get home :kind) home-kind
                    (plist-get home :locked) t
                    (mevedel-goal-execution-home goal) home
                    (mevedel-goal-implementation-context goal) context)
              (setq result
                    (if (and (eq home-kind 'worktree)
                             (null (plist-get home :branch)))
                        (mevedel-goal--transfer-to-worktree
                         goal chat-buffer plan-markdown)
                      (list :buffer chat-buffer
                            :accepted
                            (mevedel-goal--accept-plan
                             plan-markdown mevedel--session chat-buffer))))
              (let ((implementation-buffer (plist-get result :buffer))
                    (accepted-artifact (plist-get result :accepted)))
              (with-current-buffer implementation-buffer
                (let* ((goal (mevedel-session-goal mevedel--session))
                       (context (mevedel-goal-implementation-context goal))
                       (implementation-mode
                        (mevedel-goal--approval-implementation-mode outcome)))
                  (let* ((implementation-input
                          (mevedel-plan-implementation-input
                           context accepted-artifact implementation-mode
                           (mevedel-goal-context-fragment
                            goal mevedel--session)))
                         (fsm
                          (mevedel-goal--call-with-workload
                           'implementation
                           (lambda ()
                             (mevedel--implement-plan implementation-input))
                           'implementing implementation-input)))
                    (when fsm
                      (setf (gptel-fsm-info fsm)
                            (plist-put (gptel-fsm-info fsm)
                                       :mevedel-goal-phase
                                       'implementing)))))))))
        (pcase outcome
          (`(feedback . ,text)
           (let ((path (mevedel-plan-current-path
                        mevedel--session chat-buffer)))
             (mevedel-goal--mark-rejected mevedel--session)
             (setf (mevedel-goal-phase
                    (mevedel-session-goal mevedel--session)) 'planning)
             (mevedel-goal--save-session-state
              mevedel--session chat-buffer)
             (mevedel-goal--insert-feedback-draft chat-buffer path text)))
          ('feedback-draft
           (let ((path (mevedel-plan-current-path
                        mevedel--session chat-buffer)))
             (mevedel-goal--mark-rejected mevedel--session)
             (setf (mevedel-goal-phase
                    (mevedel-session-goal mevedel--session)) 'planning)
             (mevedel-goal--save-session-state
              mevedel--session chat-buffer)
             (mevedel-goal--insert-feedback-draft chat-buffer path)))
          ('aborted
           (setf (mevedel-goal-status
                  (mevedel-session-goal mevedel--session)) 'paused)
           (mevedel-goal--plan-metadata-put
            mevedel--session :verification-pending nil)
           (mevedel-goal--save-session-state
            mevedel--session chat-buffer)
           (message "mevedel: goal paused at plan approval"))
          ((or 'policy-changed 'superseded) nil)
          (_
           (message "mevedel: unknown plan outcome %S" outcome)))))))

(defun mevedel-goal--approval-entry
    (plan-markdown chat-buffer session &optional guardian-reason)
  "Return a plan approval entry for PLAN-MARKDOWN in CHAT-BUFFER SESSION."
  (list :body plan-markdown
        :chat-buffer chat-buffer
        :guardian-reason guardian-reason
        :origin "/root"
        :session session
        :callback
        (lambda (outcome)
          (mevedel-goal--approval-callback
           plan-markdown chat-buffer outcome))))

(defun mevedel-goal-present-plan
    (plan-markdown &optional chat-buffer guardian-reason)
  "Present PLAN-MARKDOWN for approval in CHAT-BUFFER.
The latest presented plan is persisted to the session-local plan
artifact before the approval prompt is displayed.  GUARDIAN-REASON,
when non-nil, explains why automatic approval escalated."
  (require 'mevedel-plan)
  (setq plan-markdown (mevedel--normalize-message-text plan-markdown))
  (let ((chat-buffer (or chat-buffer (current-buffer))))
    (with-current-buffer chat-buffer
      (mevedel-tools--validate-params
          nil mevedel-goal-present-plan
        (plan-markdown (stringp . "string")))
      (unless (string-blank-p plan-markdown)
        (mevedel-plan-write-current
         plan-markdown mevedel--session chat-buffer
         (when-let* ((goal (mevedel-session-goal mevedel--session)))
           (mevedel-goal--current-plan-relative-path goal)))
        (mevedel-plan-approval-present
         (mevedel-goal--approval-entry
          plan-markdown chat-buffer mevedel--session guardian-reason))))))

(defun mevedel-goal-restore-pending-approval
    (&optional session chat-buffer)
  "Restore pending approval for SESSION in CHAT-BUFFER if needed."
  (require 'mevedel-plan)
  (let* ((chat-buffer (or chat-buffer (current-buffer)))
         (session (or session (and (boundp 'mevedel--session)
                                   mevedel--session)))
         (metadata (and session (mevedel-session-plan-metadata session))))
    (when (and session
               (when-let* ((goal (mevedel-session-goal session)))
                 (and (eq (mevedel-goal-status goal) 'active)
                      (eq (mevedel-goal-phase goal) 'awaiting-approval)))
               (eq (plist-get metadata :status) 'presented)
               (null (mevedel-session-pending-plan-approval session)))
      (when-let* ((plan-markdown
                   (mevedel-plan-current-body session))
                  ((not (string-blank-p plan-markdown))))
        (mevedel-plan-approval-present
         (mevedel-goal--approval-entry
          plan-markdown chat-buffer session
          (when (plist-get metadata :guardian-pending)
            "Automatic guardian review was interrupted; explicit approval is required.")))))))

(defun mevedel-goal--pending-interaction-p (session)
  "Return non-nil when SESSION has queued or visible user interaction."
  (or (mevedel-session-queued-user-messages session)
      (mevedel-session-permission-queue session)
      (mevedel-session-pending-plan-approval session)
      (and (boundp 'mevedel--view-buffer)
           (buffer-live-p mevedel--view-buffer)
           (mevedel-view-interaction-pending-p mevedel--view-buffer))))

(defun mevedel-goal--budget-available-p (goal session &optional source)
  "Return non-nil when GOAL has budget for another request in SESSION.
When exhausted, pause durably with progress and blocker context after SOURCE."
  (let ((budget (mevedel-goal-token-budget goal))
        (usage (or (mevedel-goal-token-usage goal) 0)))
    (if (or (null budget) (< usage budget))
        t
      (let* ((finished
              (cl-count-if
               (lambda (cycle) (plist-get cycle :review))
               (mevedel-goal-cycles goal)))
             (latest (or (mevedel-goal-review-findings goal)
                         (and (mevedel-goal-review-summary goal)
                              (plist-get (mevedel-goal-review-summary goal)
                                         :summary))
                         "No review evidence recorded")))
        (setf (mevedel-goal-status goal) 'paused
              (mevedel-goal-reason goal)
              (format
               "Goal token budget exhausted (%d/%d)%s. Progress: %d completed review cycle%s; latest evidence: %s. Blocker: raise or remove the budget, then resume"
               usage budget (if source (format " after %s" source) "")
               finished (if (= finished 1) "" "s") latest))
        (mevedel-goal--enqueue-event-reminder
         session "token budget exhausted; raise or remove it before resume")
        (mevedel-goal--persist-checkpoint session (current-buffer))
        nil))))

(defun mevedel-goal--budget-exhausted-p (goal)
  "Return non-nil when GOAL has exhausted its finite token budget."
  (when-let* ((budget (mevedel-goal-token-budget goal)))
    (>= (or (mevedel-goal-token-usage goal) 0) budget)))

(defun mevedel-goal--automatic-request-boundary
    (goal session &optional approval-boundary)
  "Classify why GOAL should not start another automatic request.
SESSION supplies interaction state.  When APPROVAL-BOUNDARY is non-nil,
also require the automatic awaiting-approval state.  This function does
not mutate GOAL or SESSION."
  (cond
   ((not (eq (mevedel-goal-status goal) 'active))
    'inactive)
   ((and approval-boundary
         (not (eq (mevedel-goal-phase goal) 'awaiting-approval)))
    'phase)
   ((and approval-boundary
         (not (eq (mevedel-goal-approval-policy goal) 'automatic)))
    'supervised)
   ((mevedel-goal-pause-requested goal)
    'pause)
   ((bound-and-true-p mevedel--current-request)
    'request)
   ((mevedel-goal--pending-interaction-p session)
    'interaction)
   ((mevedel-goal--budget-exhausted-p goal)
    'budget)))

(defun mevedel-goal--settle-automatic-request-boundary
    (goal session boundary source)
  "Settle GOAL's classified BOUNDARY after SOURCE and return its reason."
  (pcase boundary
    ('inactive
     (or (mevedel-goal-reason goal) "Goal is not active"))
    ('phase "Goal is no longer awaiting plan approval")
    ('supervised "Goal approval policy changed to supervised")
    ('pause
     (setf (mevedel-goal-status goal) 'paused
           (mevedel-goal-pause-requested goal) nil
           (mevedel-goal-reason goal) "Paused by user")
     (mevedel-goal--save-session-state session (current-buffer))
     (mevedel-goal-reason goal))
    ('request "Automatic continuation stopped because a request is active")
    ('interaction
     "Automatic continuation stopped because user input or an interaction is pending")
    ('budget
     (mevedel-goal--budget-available-p goal session source)
     (mevedel-goal-reason goal))))

(defun mevedel-goal--guardian-decision-for-plan (goal plan)
  "Return GOAL's latest guardian decision matching PLAN."
  (when (and (stringp plan) (not (string-blank-p plan)))
    (let ((plan-hash (mevedel-plan-hash plan)))
      (cl-find-if
       (lambda (audit)
         (equal plan-hash (plist-get audit :plan-hash)))
       (plist-get (mevedel-goal-cycle-record goal) :guardian-audits)
       :from-end t))))

(defun mevedel-goal--guardian-approved-p (goal &optional session)
  "Return non-nil when GOAL's current plan in SESSION has guardian approval."
  (when-let* ((session (or session
                           (and (bound-and-true-p mevedel--session)
                                mevedel--session)))
              (plan (mevedel-plan-current-body session))
              (audit (mevedel-goal--guardian-decision-for-plan goal plan)))
    (eq (plist-get audit :verdict) 'approve)))

(defun mevedel-goal--continuation-key (goal source target)
  "Return stable key for GOAL continuation from SOURCE to TARGET."
  (secure-hash
   'sha256
   (prin1-to-string
    (list (mevedel-goal-id goal) source target (mevedel-goal-cycle goal)
          (plist-get (mevedel-goal-checkpoint goal) :attempt-id)
          (plist-get (mevedel-goal-cycle-record goal) :plan-hash)
          (mevedel-goal-phase goal)))))

(defun mevedel-goal-continuation-ready-p
    (session source target &optional required-state)
  "Admit one automatic SESSION continuation from SOURCE to TARGET.
REQUIRED-STATE defaults to `settled'.  Admission requires a durable matching
checkpoint, an idle interaction surface, any required guardian approval, and
remaining budget.  Equivalent duplicate admissions pause instead of spin."
  (when-let* ((goal (mevedel-session-goal session)))
    (if-let* ((boundary
               (mevedel-goal--automatic-request-boundary goal session)))
        (progn
          (mevedel-goal--settle-automatic-request-boundary
           goal session boundary source)
          nil)
      (mevedel-goal--continuation-checkpoint-ready-p
       goal session source target required-state))))

(defun mevedel-goal--continuation-checkpoint-ready-p
    (goal session source target &optional required-state)
  "Admit GOAL continuation after automatic request boundaries were checked."
  (let ((checkpoint (mevedel-goal-checkpoint goal)))
    (cond
     ((or (not (mevedel-goal-owned-by-session-p goal session))
          (not (eq source (plist-get checkpoint :phase)))
          (not (eq (or required-state 'settled)
                   (plist-get checkpoint :dispatch-state)))
          (and (eq target 'implementing)
               (eq (mevedel-goal-approval-policy goal) 'automatic)
               (not (mevedel-goal--guardian-approved-p goal session))))
      nil)
     (t
      (let ((key (mevedel-goal--continuation-key goal source target)))
        (if (equal key (mevedel-goal-continuation-key goal))
            (progn
              (setf (mevedel-goal-status goal) 'paused
                    (mevedel-goal-reason goal)
                    "Goal paused because an equivalent continuation was already admitted")
              (mevedel-goal--persist-checkpoint session (current-buffer))
              nil)
          (setf (mevedel-goal-continuation-key goal) key)
          (mevedel-goal--persist-checkpoint session (current-buffer))
          t))))))

(defun mevedel-goal-approval-request-pending-p (&optional session)
  "Return non-nil while SESSION runs hidden plan-approval model work."
  (and (mevedel-goal-approval-status session) t))

(defun mevedel-goal--guardian-finished
    (goal-id plan plan-hash chat-buffer decision)
  "Apply guardian DECISION for PLAN and GOAL-ID in CHAT-BUFFER."
  (when (buffer-live-p chat-buffer)
    (with-current-buffer chat-buffer
      (when-let* ((session (and (bound-and-true-p mevedel--session)
                                mevedel--session))
                  (goal (mevedel-session-goal session))
                  ((equal goal-id (mevedel-goal-id goal)))
                  ((eq (mevedel-goal-status goal) 'active))
                  ((eq (mevedel-goal-phase goal) 'awaiting-approval))
                  (current-plan (mevedel-plan-current-body session))
                  ((equal plan-hash (mevedel-plan-hash current-plan))))
        (mevedel-goal--plan-metadata-put session :guardian-pending nil)
        (when (eq (plist-get (mevedel-goal-checkpoint goal) :phase)
                  'guardian)
          (mevedel-goal--checkpoint-state
           (or (plist-get decision :checkpoint-state) 'settled)
           (if (eq (plist-get decision :checkpoint-state) 'failed)
               :failed-at :settled-at)
           (format-time-string "%FT%T%z")))
        (condition-case err
            (mevedel-goal--record-guardian-decision
             goal decision plan-hash session chat-buffer)
          (error
           (setq decision
                 (mevedel-goal--guardian-failure-decision
                  (format "Could not persist Goal guardian audit: %s"
                          (error-message-string err))))))
        (pcase (plist-get decision :verdict)
          ('approve
           ;; gptel invokes callbacks before removing the completed request
           ;; from its registry.  Continue on the next event-loop boundary to
           ;; avoid nested request dispatch and recheck user intervention.
           (run-at-time
            0 nil
            (lambda ()
              (when (buffer-live-p chat-buffer)
                (with-current-buffer chat-buffer
                  (when-let* ((current-session
                               (and (bound-and-true-p mevedel--session)
                                    mevedel--session))
                              (current-goal
                               (mevedel-session-goal current-session))
                              ((equal goal-id (mevedel-goal-id current-goal)))
                              ((eq (mevedel-goal-status current-goal) 'active))
                              ((eq (mevedel-goal-phase current-goal)
                                   'awaiting-approval)))
                    (if (eq (mevedel-goal-approval-policy current-goal)
                            'automatic)
                        (if-let* ((boundary
                                  (mevedel-goal--automatic-request-boundary
                                   current-goal current-session t)))
                            (mevedel-goal-present-plan
                             plan chat-buffer
                             (mevedel-goal--settle-automatic-request-boundary
                              current-goal current-session boundary
                              'implementation))
                          (if (mevedel-goal--continuation-checkpoint-ready-p
                               current-goal current-session
                               'guardian 'implementing)
                              (mevedel-goal--approval-callback
                               plan chat-buffer
                               (list
                                :context
                                (mevedel-goal-implementation-context
                                 current-goal)))
                            (mevedel-goal-present-plan
                             plan chat-buffer
                             "Automatic approval deferred because the durable continuation boundary was not ready.")))
                      (mevedel-goal-present-plan plan chat-buffer))))))))
          ('revise
           (if (< (or (plist-get
                       (mevedel-session-plan-metadata session)
                       :revision-count)
                      0)
                  mevedel-goal--max-automatic-revisions)
               (mevedel-goal--start-plan-revision
                goal plan decision chat-buffer)
             (mevedel-goal--enqueue-event-reminder
              session "guardian revision requires user approval")
             (mevedel-goal-present-plan
              plan chat-buffer
              (mevedel-goal--revision-escalation-reason
               decision nil
               (>= (or (plist-get
                        (mevedel-session-plan-metadata session)
                        :revision-count)
                       0)
                   mevedel-goal--max-automatic-revisions)))))
          (_
           (when (mevedel-goal-pause-requested goal)
             (mevedel-goal--settle-automatic-request-boundary
              goal session 'pause 'guardian-review))
           (mevedel-goal--enqueue-event-reminder
            session "guardian escalated plan approval to the user")
           (mevedel-goal-present-plan
            plan chat-buffer
            (mevedel-goal--revision-escalation-reason
             decision nil
             (>= (or (plist-get
                      (mevedel-session-plan-metadata session)
                      :revision-count)
                     0)
                 mevedel-goal--max-automatic-revisions)))))))))

(defun mevedel-goal--guard-current-plan (goal chat-buffer)
  "Run the mandatory automatic guardian for GOAL's current plan."
  (require 'mevedel-plan)
  (let ((session (buffer-local-value 'mevedel--session chat-buffer)))
    (when-let* ((plan (mevedel-plan-current-body session))
              ((not (string-blank-p plan))))
      (if-let* ((boundary
                 (mevedel-goal--automatic-request-boundary
                  goal session t)))
          (let ((blocker
                 (mevedel-goal--settle-automatic-request-boundary
                  goal session boundary 'guardian-review)))
            (mevedel-goal--plan-metadata-put
             session :guardian-pending nil)
            (mevedel-goal--enqueue-event-reminder
             session "automatic guardian review stopped at the user boundary")
            (mevedel-goal-present-plan plan chat-buffer blocker))
        (let ((goal-id (mevedel-goal-id goal))
              (plan-hash (mevedel-plan-hash plan)))
          (mevedel-goal--plan-metadata-put session :guardian-pending t)
          (condition-case err
              (funcall
               mevedel-goal-guardian-function goal plan chat-buffer
               (lambda (decision)
                 (mevedel-goal--guardian-finished
                  goal-id plan plan-hash chat-buffer
                  (mevedel-goal--normalize-guardian-decision decision))))
            (error
             (mevedel-goal--guardian-finished
              goal-id plan plan-hash chat-buffer
              (mevedel-goal--guardian-failure-decision
               (format "Goal guardian failed: %s"
                       (error-message-string err))
               :checkpoint-state 'failed)))))))))

(defun mevedel-goal--response-text (start end)
  "Return response text between START and END in the current buffer."
  (mevedel--normalize-message-text
   (buffer-substring-no-properties start end)))

(defun mevedel-goal--review-response-text (start end)
  "Return assistant review prose between START and END."
  (require 'mevedel-transcript)
  (mevedel--normalize-message-text
   (mapconcat
    (lambda (segment)
      (if (memq (car segment) '(response user))
          (buffer-substring-no-properties (cadr segment) (caddr segment))
        ""))
    (mevedel-transcript-segments start end)
    "\n")))

(defun mevedel-goal--parse-review (text)
  "Return validated structured Goal review from TEXT, or nil."
  (let ((case-fold-search nil)
        (text (string-trim text))
        (tag-regexp
         (regexp-opt (list mevedel-goal--review-open-tag
                           mevedel-goal--review-close-tag)))
        (regexp
         (concat "^" (regexp-quote mevedel-goal--review-open-tag)
                 "[ \t]*\n"
                 "verdict:[ \t]*\\(complete\\|continue\\|blocked\\)"
                 "[ \t]*\n"
                 "summary:[ \t]*\\(\\(?:.\\|\n\\)*?\\)[ \t\n]*"
                 (regexp-quote mevedel-goal--review-close-tag) "\\'")))
    (when (string-match regexp text)
      (let ((prefix (substring text 0 (match-beginning 0)))
            (summary (string-trim (match-string 2 text)))
            (verdict (intern (match-string 1 text))))
        (unless (or (string-blank-p summary)
                    (string-match-p tag-regexp prefix)
                    (string-match-p tag-regexp summary))
          (list :verdict verdict :summary summary))))))

(defun mevedel-goal--post-response (start end)
  "Capture phase output between START and END without settling the turn."
  (require 'mevedel-plan)
  (when-let* ((session (and (bound-and-true-p mevedel--session)
                            mevedel--session))
              (goal (mevedel-session-goal session))
              ((eq (mevedel-goal-status goal) 'active)))
    (let ((response (mevedel-goal--response-text start end)))
      (pcase (mevedel-goal-phase goal)
        ('planning
         (if-let* ((plan (mevedel-plan-extract-proposed response)))
             (let* ((hash (mevedel-plan-hash plan))
                    (previous (car (last (butlast
                                          (mevedel-goal-cycles goal))))))
               (if (and previous
                        (equal hash (plist-get previous :plan-hash)))
                   (progn
                     (setf (mevedel-goal-status goal) 'paused
                           (mevedel-goal-reason goal)
                           "Planner repeated the previous accepted plan")
                     (mevedel-goal--save-session-state session (current-buffer))
                     (message "mevedel: goal paused after duplicate plan"))
                 (setf (mevedel-goal-phase goal) 'awaiting-approval)
                 (mevedel-goal--plan-metadata-put
                  session :revision-count 0)
                 (mevedel-goal--plan-metadata-put
                  session :revision-pending nil)
                 (mevedel-goal--enqueue-event-reminder
                  session "planning finished; plan awaits approval")
                 (if (eq (mevedel-goal-approval-policy goal) 'automatic)
                     (progn
                       (mevedel-plan-write-current
                        plan session (current-buffer)
                        (mevedel-goal--current-plan-relative-path goal))
                       (mevedel-goal--save-session-state
                        session (current-buffer)))
                   (mevedel-goal-present-plan plan (current-buffer)))))
           (setf (mevedel-goal-status goal) 'paused
                 (mevedel-goal-reason goal)
                 "Planner returned no proposed plan; resume to retry planning")
           (mevedel-goal--save-session-state session (current-buffer))
           (message "mevedel: goal paused because planning produced no plan")))
        ('reviewing
         (setf (mevedel-goal-review-summary goal)
               (mevedel-goal--parse-review
                (mevedel-goal--review-response-text start end))))))))

(defun mevedel-goal-settle-turn (fsm)
  "Advance Goal state after FSM reaches a successful terminal boundary."
  (when-let* ((info (gptel-fsm-info fsm))
              (phase (plist-get info :mevedel-goal-phase))
              (chat-buffer (plist-get info :buffer))
              ((buffer-live-p chat-buffer)))
    (with-current-buffer chat-buffer
      (when-let* ((goal (and (bound-and-true-p mevedel--session)
                             (mevedel-session-goal mevedel--session))))
        (let ((settled-cycle (mevedel-goal-cycle goal))
              (review-verdict
               (and (mevedel-goal-review-summary goal)
                    (plist-get (mevedel-goal-review-summary goal) :verdict))))
          (mevedel-goal--record-token-usage goal info)
          (mevedel-goal--checkpoint-settle fsm 'settled)
          (when (eq (mevedel-goal-status goal) 'active)
          (when (eq phase (mevedel-goal-phase goal))
            (pcase phase
              ('implementing
               (setf (mevedel-goal-phase goal) 'reviewing)
               (mevedel-goal--enqueue-event-reminder
                mevedel--session "implementation settled; review started"))
              ('reviewing
               (if-let* ((review (mevedel-goal-review-summary goal)))
                   (progn
                     (mevedel-goal--cycle-put
                      goal :review
                      (list :verdict (plist-get review :verdict)
                            :summary-hash
                            (secure-hash 'sha256 (plist-get review :summary))
                            :at (format-time-string "%FT%T%z")))
                     (mevedel-goal--persist-cycle-index
                      goal mevedel--session chat-buffer)
                     (pcase (plist-get review :verdict)
                       ('complete
                        (setf (mevedel-goal-status goal) 'complete
                              (mevedel-goal-reason goal) nil)
                        (mevedel-goal--enqueue-event-reminder
                         mevedel--session "review proved the Goal complete"))
                       ('continue
                        (setf (mevedel-goal-cycle goal)
                              (1+ (mevedel-goal-cycle goal))
                              (mevedel-goal-phase goal) 'planning
                              (mevedel-goal-current-plan goal) nil
                              (mevedel-goal-review-findings goal)
                              (plist-get review :summary)
                              (mevedel-goal-review-summary goal) nil
                              (mevedel-goal-reason goal) nil)
                        (mevedel-goal--cycle-put
                         goal :started-at (format-time-string "%FT%T%z"))
                        (mevedel-goal--persist-cycle-index
                         goal mevedel--session chat-buffer)
                        (mevedel-goal--enqueue-event-reminder
                         mevedel--session
                         "review found remaining work; next planning cycle started"))
                       ('blocked
                        (setf (mevedel-goal-status goal) 'blocked
                              (mevedel-goal-reason goal)
                              (plist-get review :summary))
                        (mevedel-goal--enqueue-event-reminder
                         mevedel--session
                         "review found a blocker; user input is required"))))
                   (setf (mevedel-goal-status goal) 'paused
                       (mevedel-goal-reason goal)
                       "Goal review returned malformed structured output")))))
          (when (and (mevedel-goal-pause-requested goal)
                     (eq (mevedel-goal-status goal) 'active))
            (setf (mevedel-goal-status goal) 'paused
                  (mevedel-goal-pause-requested goal) nil
                  (mevedel-goal-reason goal) "Paused by user"))
          (when (fboundp 'mevedel-telemetry-record)
            (when review-verdict
              (mevedel-telemetry-record
               mevedel--session 'goal-review-verdict-persisted
               :settled-cycle settled-cycle
               :verdict review-verdict
               :resulting-status (mevedel-goal-status goal)))
            (when (not (equal settled-cycle (mevedel-goal-cycle goal)))
              (mevedel-telemetry-record
               mevedel--session 'goal-cycle-changed
               :previous-cycle settled-cycle
               :new-cycle (mevedel-goal-cycle goal)))
            (mevedel-telemetry-record
             mevedel--session 'goal-phase-settled
             :settled-phase phase
             :attempt-id (plist-get info :mevedel-goal-attempt-id)
             :resulting-phase (mevedel-goal-phase goal)
             :resulting-status (mevedel-goal-status goal)
             :review-verdict review-verdict)
            (when (memq (mevedel-goal-status goal) '(complete blocked))
              (mevedel-telemetry-record
               mevedel--session 'goal-terminal-settlement
               :settled-phase phase
               :result (mevedel-goal-status goal)
               :review-verdict review-verdict)))))))))

(defun mevedel-goal-settle-failure (fsm &optional status)
  "Settle Goal failure for FSM with terminal STATUS in memory.
The post-teardown failure transaction persists this state."
  (when-let* ((info (gptel-fsm-info fsm))
              (phase (plist-get info :mevedel-goal-phase))
              (chat-buffer (plist-get info :buffer))
              ((buffer-live-p chat-buffer)))
    (with-current-buffer chat-buffer
      (when-let* ((goal (and (bound-and-true-p mevedel--session)
                             (mevedel-session-goal mevedel--session))))
        (mevedel-goal--record-token-usage goal info)
        (let* ((status (or status 'error))
               (reason (mevedel-goal--fsm-failure-reason fsm status))
               (checkpoint (mevedel-goal--checkpoint-settle
                            fsm 'failed reason))
               (retry-count (1+ (or (plist-get checkpoint :retry-count) 0)))
               (read-only (memq phase '(planning reviewing)))
               (retry (and read-only
                           (not (eq status 'aborted))
                           (mevedel-goal--transient-failure-p reason)
                           (< (or (plist-get checkpoint :retry-count) 0)
                              mevedel-goal-max-transient-retries))))
          (when checkpoint
            (setf (mevedel-goal-checkpoint goal)
                  (plist-put checkpoint :retry-count retry-count)))
          (cond
           ((mevedel-goal-pause-requested goal)
            (setf (mevedel-goal-status goal) 'paused
                  (mevedel-goal-reason goal) "Paused by user"))
           (retry
            (setf (mevedel-goal-status goal) 'active
                  (mevedel-goal-reason goal)
                  (format "Retrying transient %s failure: %s" phase reason)))
           ((eq phase 'implementing)
            (setf (mevedel-goal-status goal) 'paused
                  (mevedel-goal-reason goal)
                  (format "Implementation outcome is unknown; resume to audit actual work: %s"
                          reason)))
           (t
            (setf (mevedel-goal-status goal) 'paused
                  (mevedel-goal-reason goal)
                  (if (eq status 'aborted)
                      (format "Request was forcefully stopped; switch provider if needed, then resume: %s"
                              reason)
                    (format "%s failure; switch provider or preset if needed, then resume: %s"
                            (if (mevedel-goal--terminal-provider-failure-p reason)
                                "Provider" "Request")
                            reason)))))
          (when (eq (mevedel-goal-status goal) 'paused)
            (mevedel-goal--enqueue-event-reminder
             mevedel--session
             (if (eq status 'aborted)
                 "request was forcefully stopped; resume remains available"
               "request failed; Goal paused with a recoverable checkpoint")))
          (setf (mevedel-goal-pause-requested goal) nil)
          (when (fboundp 'mevedel-telemetry-record)
            (mevedel-telemetry-record
             mevedel--session 'goal-phase-settled
             :settled-phase phase
             :attempt-id (plist-get info :mevedel-goal-attempt-id)
             :outcome status
             :retry retry
             :retry-count retry-count
             :resulting-phase (mevedel-goal-phase goal)
             :resulting-status (mevedel-goal-status goal))))))))

(defun mevedel-goal-persist-failure (fsm)
  "Persist FSM's Goal failure after request teardown."
  (when-let* ((info (gptel-fsm-info fsm))
              (chat-buffer (plist-get info :buffer))
              ((buffer-live-p chat-buffer)))
    (with-current-buffer chat-buffer
      (when (and (bound-and-true-p mevedel--session)
                 (mevedel-session-goal mevedel--session))
        (mevedel-goal--persist-checkpoint mevedel--session chat-buffer)))))

(defun mevedel-goal-dispatch-after-failure (fsm)
  "Retry FSM's failed read-only Goal attempt when its bounded policy allows."
  (when-let* ((info (gptel-fsm-info fsm))
              (chat-buffer (plist-get info :buffer))
              ((buffer-live-p chat-buffer)))
    (with-current-buffer chat-buffer
      (when-let* ((goal (and (bound-and-true-p mevedel--session)
                             (mevedel-session-goal mevedel--session)))
                  ((eq (mevedel-goal-status goal) 'active))
                  (checkpoint (mevedel-goal-checkpoint goal))
                  ((eq (plist-get checkpoint :dispatch-state) 'failed))
                  (phase (plist-get checkpoint :phase))
                  ((memq phase '(planning reviewing))))
        (when (mevedel-goal-continuation-ready-p
               mevedel--session phase phase 'failed)
          (setf (mevedel-goal-reason goal) nil)
          (condition-case err
              (mevedel-goal--dispatch-phase
               phase (copy-tree (plist-get checkpoint :input))
               (format "Retry Goal %s after transient failure"
                       (mevedel-goal-id goal)))
            (error
             (setf (mevedel-goal-status goal) 'paused
                   (mevedel-goal-reason goal)
                   (format "Automatic retry could not start; switch provider or preset, then resume: %s"
                           (error-message-string err)))
             (mevedel-goal--persist-checkpoint
              mevedel--session chat-buffer))))))))

(defun mevedel-goal-dispatch-after-turn (fsm)
  "Dispatch the next Goal phase after FSM has settled."
  (when-let* ((info (gptel-fsm-info fsm))
              (settled-phase (plist-get info :mevedel-goal-phase))
              (chat-buffer (plist-get info :buffer))
              ((buffer-live-p chat-buffer)))
    (with-current-buffer chat-buffer
      (when-let* ((goal (and (bound-and-true-p mevedel--session)
                             (mevedel-session-goal mevedel--session)))
                  ((eq (mevedel-goal-status goal) 'active)))
        (pcase (cons settled-phase (mevedel-goal-phase goal))
          (`(planning . awaiting-approval)
           (when (and (eq (mevedel-goal-approval-policy goal) 'automatic)
                      (mevedel-goal-continuation-ready-p
                       mevedel--session 'planning 'guardian))
             (mevedel-goal--guard-current-plan goal chat-buffer)))
          (`(implementing . reviewing)
           (when (mevedel-goal-continuation-ready-p
                  mevedel--session 'implementing 'reviewing)
             (mevedel-goal--dispatch-phase
              'reviewing (mevedel-goal--review-prompt goal)
              "Review Goal implementation")))
          (`(reviewing . planning)
           (when (mevedel-goal-continuation-ready-p
                  mevedel--session 'reviewing 'planning)
             (mevedel-goal--dispatch-phase
              'planning (mevedel-goal--planning-prompt goal)
              (format "Continue Goal with cycle %d"
                      (mevedel-goal-cycle goal))))))))))


(provide 'mevedel-goal)
;;; mevedel-goal.el ends here
