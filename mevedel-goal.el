;;; mevedel-goal.el -- Goal workflow controller -*- lexical-binding: t -*-

;;; Commentary:

;; A session-owned Goal drives supervised or guardian-approved planning,
;; implementation, and evidence-review cycles.  Plans use the lifecycle-neutral
;; artifact operations in `mevedel-plan'.

;;; Code:

(require 'cl-lib)

(eval-when-compile
  (require 'mevedel-plan)
  (require 'mevedel-tool-registry))
(require 'mevedel-queue)
(require 'mevedel-utilities)

;; `gptel'
(declare-function gptel--model-name "ext:gptel" (model))
(declare-function gptel-backend-name "ext:gptel" (backend))
(declare-function gptel-fsm-info "ext:gptel-request" (cl-x) t)
(declare-function gptel-request "ext:gptel" (prompt &rest args))
(declare-function gptel-send "ext:gptel" (&optional arg))
(defvar gptel-backend)
(defvar gptel-model)
(defvar gptel-prompt-prefix-alist)
(defvar gptel-reasoning-effort)
(defvar gptel-response-separator)
(defvar gptel-tools)
(defvar gptel-use-context)
(defvar gptel-use-tools)

;; `mevedel-chat'
(declare-function mevedel--implement-plan "mevedel-chat" (action-plist))
(defvar mevedel--current-request)
(defvar mevedel--session)

;; `mevedel-compact'
(declare-function mevedel--compact-token-usage-count "mevedel-compact" (tokens))
(declare-function mevedel--estimate-tokens "mevedel-compact" ())

;; `mevedel-interaction-prompt'
(declare-function mevedel--prompt--settle
                  "mevedel-interaction-prompt" (overlay outcome))

;; `mevedel-models'
(declare-function mevedel-model-resolve-workload
                  "mevedel-models"
                  (workload &optional explicit-selector explicit-effort))

;; `mevedel-plan'
(defvar mevedel-plan-queue--spec)

;; `mevedel-presets'
(declare-function mevedel-preset-restore-session
                  "mevedel-presets" (session &optional buffer))

;; `mevedel-queue'
(declare-function mevedel-queue--entry-metadata-get "mevedel-queue"
                  (entry key))
(declare-function mevedel-queue--entry-metadata-put "mevedel-queue"
                  (entry key value))

;; `mevedel-reminders'
(declare-function mevedel-reminders-make-plan-reference
                  "mevedel-reminders" ())
(declare-function mevedel-session-ensure-reminder
                  "mevedel-reminders" (session reminder))
(declare-function mevedel-session-remove-reminder
                  "mevedel-reminders" (session type))

;; `mevedel-session-persistence'
(declare-function mevedel-session-persistence-ensure-files
                  "mevedel-session-persistence" (session buffer))
(declare-function mevedel-session-persistence-save
                  "mevedel-session-persistence" (session buffer))
(declare-function mevedel-session-persistence-write
                  "mevedel-session-persistence" (path plist))

;; `mevedel-structs'
(declare-function mevedel-goal--create "mevedel-structs" (&rest args))
(declare-function mevedel-goal-approval-policy "mevedel-structs" (cl-x) t)
(declare-function mevedel-goal-checkpoint "mevedel-structs" (cl-x) t)
(declare-function mevedel-goal-continuation-key "mevedel-structs" (cl-x) t)
(declare-function mevedel-goal-current-plan "mevedel-structs" (cl-x) t)
(declare-function mevedel-goal-cycle "mevedel-structs" (cl-x) t)
(declare-function mevedel-goal-cycles "mevedel-structs" (cl-x) t)
(declare-function mevedel-goal-execution-home "mevedel-structs" (cl-x) t)
(declare-function mevedel-goal-id "mevedel-structs" (cl-x) t)
(declare-function mevedel-goal-implementation-context "mevedel-structs" (cl-x) t)
(declare-function mevedel-goal-objective "mevedel-structs" (cl-x) t)
(declare-function mevedel-goal-pause-requested "mevedel-structs" (cl-x) t)
(declare-function mevedel-goal-phase "mevedel-structs" (cl-x) t)
(declare-function mevedel-goal-reason "mevedel-structs" (cl-x) t)
(declare-function mevedel-goal-review-findings "mevedel-structs" (cl-x) t)
(declare-function mevedel-goal-review-summary "mevedel-structs" (cl-x) t)
(declare-function mevedel-goal-status "mevedel-structs" (cl-x) t)
(declare-function mevedel-goal-token-budget "mevedel-structs" (cl-x) t)
(declare-function mevedel-goal-token-usage "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-enqueue-pending-reminder
                  "mevedel-structs" (session body))
(declare-function mevedel-session-goal "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-goal-handoff "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-name "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-permission-mode "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-permission-queue "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-permission-rules "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-pending-reminders
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-plan-metadata "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-plan-queue "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-preset-name "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-preset-settings "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-queued-user-messages
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-save-path "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-session-id "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-working-directory "mevedel-structs" (cl-x) t)

;; `mevedel-transcript-audit'
(declare-function mevedel--format-hook-audit-record
                  "mevedel-transcript-audit" (record))

;; `mevedel-view'
(declare-function mevedel-view--fontify-as "mevedel-view" (text mode))
(declare-function mevedel-view-rerender "mevedel-view" (&optional buffer))
(defvar mevedel--view-buffer)

;; `mevedel-view-composer'
(declare-function mevedel-view--begin-external-turn
                  "mevedel-view-composer"
                  (display-text data-turn-start
                                &optional kind hook-context no-spinner))
(declare-function mevedel-view--clear-input "mevedel-view-composer" ())
(declare-function mevedel-view--input-start "mevedel-view-composer" ())

;; `mevedel-view-interaction'
(declare-function mevedel-view--interaction-anchor
                  "mevedel-view-interaction" ())
(declare-function mevedel-view--interaction-register
                  "mevedel-view-interaction"
                  (descriptor))
(declare-function mevedel-view--interaction-target-buffer
                  "mevedel-view-interaction"
                  (&optional data-buffer))
(declare-function mevedel-view-interaction-pending-p
                  "mevedel-view-interaction" (&optional view-buffer))

;; `mevedel-worktree'
(declare-function mevedel-worktree-create-session
                  "mevedel-worktree" (&optional branch purpose clean))
(declare-function mevedel-worktree--git-result
                  "mevedel-worktree" (directory &rest args))

;;
;;; Lifecycle

(defvar mevedel-goal-dispatch-function #'mevedel-goal--dispatch-gptel
  "Function called with PHASE, PROMPT, and DISPLAY-TEXT for a Goal request.")

(defcustom mevedel-goal-guardian-timeout 60
  "Seconds before an automatic Goal guardian request escalates to the user."
  :type 'number
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

(defconst mevedel-goal--review-open-tag "<goal_review>"
  "Opening tag for structured Goal review results.")

(defconst mevedel-goal--review-close-tag "</goal_review>"
  "Closing tag for structured Goal review results.")

(defconst mevedel-goal--guardian-open-tag "<goal_guardian>"
  "Opening tag for structured Goal guardian results.")

(defconst mevedel-goal--guardian-close-tag "</goal_guardian>"
  "Closing tag for structured Goal guardian results.")

(defun mevedel-goal--relative-dir (goal)
  "Return GOAL's artifact directory relative to its session."
  (file-name-concat "goals" (mevedel-goal-id goal)))

(defun mevedel-goal--current-plan-relative-path (goal)
  "Return GOAL's mutable current-plan relative path."
  (file-name-concat (mevedel-goal--relative-dir goal) "current-plan.md"))

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

(defun mevedel-goal--record-token-usage (goal info &optional response)
  "Charge GOAL once for request INFO, estimating from RESPONSE if needed."
  (let ((checkpoint (copy-tree (mevedel-goal-checkpoint goal))))
    (unless (plist-get checkpoint :usage-recorded)
      (require 'mevedel-compact)
      (let* ((reported
              (mevedel--compact-token-usage-count
               (or (plist-get info :tokens) (plist-get info :tokens-full))))
             (baseline (plist-get checkpoint :token-baseline))
             (estimated
              (if (and (numberp baseline) (not response))
                  (max (or (plist-get checkpoint :estimated-input-tokens) 1)
                       (- (mevedel--estimate-tokens) baseline))
                (+ (or (plist-get checkpoint :estimated-input-tokens) 1)
                   (if (stringp response)
                       (mevedel-goal--estimate-input-tokens response)
                     0))))
             (count (or (and reported (> reported 0) reported) estimated)))
        (setf (mevedel-goal-token-usage goal)
              (+ (or (mevedel-goal-token-usage goal) 0) count))
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
         (memq (mevedel-goal-phase goal) '(planning reviewing)))))

(defun mevedel-goal--planning-prompt (goal)
  "Return the planning request for GOAL."
  (format
   "%s\n\nPlanning instructions:\n%sInvestigate the current repository state and propose the next decision-complete implementation plan. This phase is read-only. End with exactly one line-oriented <proposed_plan>...</proposed_plan> block."
   (mevedel-goal-context-fragment goal mevedel--session)
   (if-let* ((findings (mevedel-goal-review-findings goal)))
       (format "Prior review findings to resolve:\n%s\n\n" findings)
     "")))

(defun mevedel-goal--review-prompt (goal)
  "Return the one-cycle completion review request for GOAL."
  (format
   "%s\n\nReview instructions:\nReview the implementation against the whole objective and current repository evidence. This phase is read-only. Do not make changes. Return exactly one structured result with a single verdict and evidence:\n<goal_review>\nverdict: complete|continue|blocked\nsummary: evidence, remaining work, or blocker\n</goal_review>\nUse complete only when the whole objective is proven complete; use continue when another implementation cycle can make progress; use blocked only for a concrete external or decision blocker."
   (mevedel-goal-context-fragment goal mevedel--session)))

(defun mevedel-goal--recovery-prompt (goal checkpoint)
  "Return a read-only recovery audit request for GOAL and CHECKPOINT."
  (format
   "%s\n\nRecovery instructions:\nInterrupted implementation attempt: %s\nDispatch state: %s\n\nThe implementation request may already have changed files. Do not replay it and do not make changes. Inspect the actual repository, diff, tests, and artifacts against the objective and accepted plan. Choose the next safe lifecycle boundary. Return exactly one structured result:\n<goal_review>\nverdict: complete|continue|blocked\nsummary: evidence and the safe next step\n</goal_review>\nUse complete only when repository evidence proves the whole objective; continue when a new plan should address remaining work; blocked when external input is required."
   (mevedel-goal-context-fragment goal mevedel--session)
   (or (plist-get checkpoint :attempt-id) "unknown")
   (or (plist-get checkpoint :dispatch-state) 'unknown)))

(defun mevedel-goal--guardian-prompt (goal plan)
  "Return the tool-free automatic approval request for GOAL and PLAN."
  (format
   "%s\n\nGuardian instructions:\nProposed plan:\n%s\n\nJudge only whether this plan is safe and credible to implement automatically. Check objective alignment, scope boundaries, dangerous or irreversible effects, ambiguity, and verification quality. Do not rewrite the plan. Return exactly:\n<goal_guardian>\nverdict: approve|ask\nreason: concise reason\n</goal_guardian>\nUse ask whenever user judgment, missing constraints, or clarification is required."
   (mevedel-goal-context-fragment goal mevedel--session)
   plan))

(defun mevedel-goal--parse-guardian (text)
  "Return a validated Goal guardian decision parsed from TEXT, or nil."
  (let ((case-fold-search nil)
        (text (and (stringp text) (string-trim text))))
    (when (and text
               (string-match
                (concat "\\`" (regexp-quote mevedel-goal--guardian-open-tag)
                        "[ \t]*\n"
                        "verdict:[ \t]*\\(approve\\|ask\\)[ \t]*\n"
                        "reason:[ \t]*\\(\\(?:.\\|\n\\)*?\\)[ \t\n]*"
                        (regexp-quote mevedel-goal--guardian-close-tag)
                        "\\'")
                text))
      (let ((reason (string-trim (match-string 2 text))))
        (unless (string-blank-p reason)
          (list :verdict (intern (match-string 1 text))
                :reason reason))))))

(defun mevedel-goal--normalize-guardian-decision (decision)
  "Return validated guardian DECISION or a fail-closed ask decision."
  (let ((verdict (plist-get decision :verdict))
        (reason (plist-get decision :reason)))
    (if (and (memq verdict '(approve ask))
             (stringp reason)
             (not (string-blank-p reason)))
        decision
      (list :verdict 'ask
            :reason "Goal guardian returned malformed output"))))

(defun mevedel-goal--guardian-provider-label (policy)
  "Return a stable provider/model label for resolved guardian POLICY."
  (mevedel-goal--policy-label policy))

(defun mevedel-goal--guardian-request (goal plan chat-buffer callback)
  "Review PLAN for GOAL internally, then call CALLBACK in CHAT-BUFFER.
The request has no tools or conversational transcript insertion."
  (if (not (require 'gptel nil t))
      (funcall callback
               (list :verdict 'ask :reason "Goal guardian is unavailable"))
    (let ((done nil)
          policy
          timer)
      (cl-labels
          ((finish (decision)
             (unless done
               (setq done t)
               (when timer (cancel-timer timer))
               (when (buffer-live-p chat-buffer)
                 (with-current-buffer chat-buffer
                   (funcall callback decision))))))
        (setq timer
              (run-at-time
               mevedel-goal-guardian-timeout nil
               (lambda ()
                 (when (buffer-live-p chat-buffer)
                   (with-current-buffer chat-buffer
                     (mevedel-goal--record-token-usage goal nil)))
                 (finish
                  (append
                   (list :verdict 'ask
                         :reason "Goal guardian timed out"
                         :checkpoint-state 'failed)
                   (when policy
                     (list :provider
                           (mevedel-goal--guardian-provider-label policy)
                           :effort (plist-get policy :effort))))))))
        (condition-case err
            (progn
              (let ((prompt (mevedel-goal--guardian-prompt goal plan)))
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
                      :stream nil
                      :transforms nil
                      :callback
                      (lambda (response info)
                        (unless (and (consp response)
                                     (eq (car response) 'reasoning))
                          (let ((decision
                                 (mevedel-goal--parse-guardian response)))
                            (when (buffer-live-p chat-buffer)
                              (with-current-buffer chat-buffer
                                (mevedel-goal--record-token-usage
                                 goal info response)))
                            (finish
                             (append
                              (or decision
                                  (list
                                   :verdict 'ask
                                   :reason
                                   "Goal guardian returned malformed output"))
                              (list
                               :provider
                               (mevedel-goal--guardian-provider-label policy)
                               :effort (plist-get policy :effort)))))))))))
                 'guardian prompt))
          (error
           (finish
            (append
             (list :verdict 'ask
                   :reason (format "Goal guardian failed: %s"
                                   (error-message-string err))
                   :checkpoint-state 'failed)
             (when policy
               (list :provider (mevedel-goal--guardian-provider-label policy)
                     :effort (plist-get policy :effort)))))))))))

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
    (if (bound-and-true-p mevedel--current-request)
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
    (mevedel-plan-queue-abort-all mevedel--session)
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
        (when (mevedel-session-plan-queue mevedel--session)
          (mevedel-queue--abort-all
           mevedel-plan-queue--spec 'policy-changed mevedel--session))
        (unless (mevedel-goal-guardian-pending-p mevedel--session)
          (mevedel-goal--apply-automatic-approval-policy
           goal (current-buffer)))))
    (message "mevedel: Goal approval policy is %s" policy)
    goal))

(defun mevedel-goal-clear ()
  "Remove current Goal state while preserving transcript, artifacts, and work."
  (mevedel-goal--current)
  (when (bound-and-true-p mevedel--current-request)
    (user-error "Wait for or abort the active request before clearing Goal"))
  (mevedel-plan-queue-abort-all mevedel--session)
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
             (when-let* ((plan (mevedel-plan-current-body mevedel--session)))
               (mevedel-goal--approval-callback
                plan (current-buffer)
                (list :context (mevedel-goal-implementation-context goal)))))
            (t
             (mevedel-goal-restore-pending-approval
              mevedel--session (current-buffer)))))
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

(defun mevedel-goal-start (objective &optional display-text approval-policy)
  "Start a Goal for OBJECTIVE in the current session.
DISPLAY-TEXT is the user-facing form of the planning turn.
APPROVAL-POLICY is `supervised' by default or explicitly `automatic'."
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
    (mevedel-goal--enqueue-event-reminder
     mevedel--session "started; authoritative Goal context is attached")
    (condition-case err
        (mevedel-goal--dispatch-phase
         'planning (mevedel-goal--planning-prompt goal)
         (or display-text (mevedel-goal-objective goal)))
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
      (mevedel-plan-queue-abort-all mevedel--session)
      (setf (mevedel-session-goal mevedel--session) goal
            (mevedel-session-plan-metadata mevedel--session) nil))
    goal))

(defun mevedel-goal--dispatch-gptel (phase prompt display-text)
  "Dispatch PHASE with PROMPT, showing DISPLAY-TEXT in the transcript."
  (let ((fsm (mevedel-goal--insert-and-send prompt display-text)))
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
              (setq-local gptel-backend (plist-get policy :backend)
                          gptel-model (plist-get policy :model)
                          gptel-reasoning-effort (plist-get policy :effort))
              (when checkpoint-enabled
                (setq dispatch-started t)
                (mevedel-goal--checkpoint-state
                 'started :request-started t
                 :started-at (format-time-string "%FT%T%z")))
              (let ((fsm (funcall function)))
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

(defun mevedel-goal--dispatch-phase (phase prompt display-text)
  "Dispatch Goal PHASE with PROMPT and DISPLAY-TEXT."
  (unless (memq phase '(planning reviewing))
    (error "Goal phase cannot dispatch: %s" phase))
  (let* ((goal (mevedel-session-goal mevedel--session))
         (prompt (mevedel-goal--refresh-request-context goal prompt)))
    (mevedel-goal--call-with-workload
     (if (eq phase 'planning) 'planning 'review)
     (lambda ()
       (funcall mevedel-goal-dispatch-function phase prompt display-text))
     phase prompt)))

(defun mevedel-goal--insert-and-send (prompt &optional display-text hook-context)
  "Insert PROMPT as a user turn in the current data buffer and send it.
DISPLAY-TEXT is shown in the view instead of PROMPT.  HOOK-CONTEXT is
shown as a collapsed hook-context disclosure."
  (goto-char (point-max))
  (let ((user-turn-start (point)))
    (insert gptel-response-separator)
    (when-let* ((prefix (alist-get major-mode gptel-prompt-prefix-alist)))
      (unless (and (>= (point) (+ (point-min) (length prefix)))
                   (string= (buffer-substring-no-properties
                             (- (point) (length prefix)) (point))
                            prefix))
        (unless (bolp) (insert "\n"))
        (insert prefix)))
    (insert prompt "\n")
    (mevedel--clear-user-turn-gptel-properties user-turn-start (point)))
  (let ((data-turn-start (copy-marker (point) nil)))
    (when-let* ((view (and (boundp 'mevedel--view-buffer)
                           mevedel--view-buffer))
                ((buffer-live-p view))
                ((fboundp 'mevedel-view--begin-external-turn)))
      (with-current-buffer view
        (mevedel-view--begin-external-turn
         (or display-text prompt) data-turn-start nil hook-context))))
  (gptel-send))

(defun mevedel-plan-queue--current-session ()
  "Resolve the session struct that owns the plan approval FIFO."
  (mevedel-queue--current-session))

(defvar mevedel-plan-queue--spec
  (mevedel-queue-spec--create
   :name 'plan-queue
   :get (lambda (session) (mevedel-session-plan-queue session))
   :set (lambda (session queue)
          (setf (mevedel-session-plan-queue session) queue))
   :render #'mevedel-plan-queue--render-entry
   :settle (lambda (entry outcome)
             (when-let* ((callback (plist-get entry :callback)))
               (funcall callback outcome)))
   :retain-on-settle-error t
   :entry-origin (lambda (entry) (plist-get entry :origin)))
  "Shared FIFO spec for plan approval confirmations.")

(defun mevedel-plan-queue--get (&optional session)
  "Return SESSION's plan queue."
  (when-let* ((sess (or session (mevedel-plan-queue--current-session))))
    (mevedel-queue--get mevedel-plan-queue--spec sess)))

(defun mevedel-plan-queue--enqueue (entry)
  "Append plan approval ENTRY to the session FIFO and render the head."
  (mevedel-queue--enqueue mevedel-plan-queue--spec entry))

(defun mevedel-plan-queue--render-head (&optional session)
  "Render the current head of SESSION's plan approval FIFO."
  (mevedel-queue--render-head mevedel-plan-queue--spec
                              (or session
                                  (mevedel-plan-queue--current-session))))

(defun mevedel-goal--ensure-implementation-allowed (entry outcome)
  "Signal when ENTRY may not be implemented with OUTCOME yet."
  (when (and (mevedel-goal--approval-outcome-p outcome)
             (mevedel-session-queued-user-messages
              (plist-get entry :session)))
    (user-error
     "Resolve queued messages before implementing the plan (edit or clear the queued message batch)")))

(defun mevedel-plan-queue--on-head-outcome (entry outcome)
  "Settle plan approval ENTRY with OUTCOME and render the next head."
  (mevedel-goal--ensure-implementation-allowed entry outcome)
  (mevedel-queue--pop mevedel-plan-queue--spec entry outcome))

(defun mevedel-plan-queue-abort-all (&optional session)
  "Flush SESSION's plan FIFO, firing `aborted' on every entry."
  (mevedel-queue--abort-all mevedel-plan-queue--spec 'aborted session))

(defun mevedel-plan-queue-sweep-agent (origin &optional session)
  "Abort queued SESSION plans whose `:origin' matches ORIGIN."
  (mevedel-queue--sweep-origin
   mevedel-plan-queue--spec origin 'aborted session))

(defconst mevedel-goal--implementation-modes
  '(default accept-edits trust-all)
  "Plan implementation permission modes cycled by the approval prompt.")

(defun mevedel-goal--implementation-mode-label (mode)
  "Return compact display label for implementation MODE."
  (pcase mode
    ('accept-edits "edit")
    ('trust-all "auto")
    (_ "default")))

(defun mevedel-goal--next-implementation-mode (mode)
  "Return the next implementation mode after MODE."
  (let* ((modes mevedel-goal--implementation-modes)
         (tail (memq mode modes)))
    (or (cadr tail) (car modes))))

(defun mevedel-plan-queue--entry-implementation-mode (entry)
  "Return ENTRY's selected implementation permission mode."
  (or (mevedel-queue--entry-metadata-get entry :implementation-mode)
      (when-let* ((session (plist-get entry :session)))
        (mevedel-session-permission-mode session))
      'default))

(defun mevedel-plan-queue--cycle-entry-implementation-mode (entry)
  "Cycle ENTRY's implementation mode and rerender the plan prompt."
  (mevedel-queue--entry-metadata-put
   entry :implementation-mode
   (mevedel-goal--next-implementation-mode
    (mevedel-plan-queue--entry-implementation-mode entry)))
  (mevedel-plan-queue--render-entry entry))

(defun mevedel-plan-queue--entry-execution-home (entry)
  "Return ENTRY's selected Goal execution-home kind."
  (or (mevedel-queue--entry-metadata-get entry :execution-home)
      (when-let* ((session (plist-get entry :session))
                  (goal (mevedel-session-goal session)))
        (plist-get (mevedel-goal-execution-home goal) :kind))
      'current))

(defun mevedel-plan-queue--cycle-entry-execution-home (entry)
  "Toggle ENTRY's implementation home and rerender the plan prompt."
  (unless (mevedel-plan-queue--entry-execution-home-mutable-p entry)
    (user-error "Goal execution home is locked after first approval"))
  (mevedel-queue--entry-metadata-put
   entry :execution-home
   (if (eq (mevedel-plan-queue--entry-execution-home entry) 'current)
       'worktree
     'current))
  (mevedel-plan-queue--render-entry entry))

(defun mevedel-plan-queue--entry-execution-home-mutable-p (entry)
  "Return non-nil when ENTRY may select the Goal execution home."
  (when-let* ((session (plist-get entry :session))
              (goal (mevedel-session-goal session)))
    (not (plist-get (mevedel-goal-execution-home goal) :locked))))

(defun mevedel-plan-queue--keys-line
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

(defun mevedel-plan-queue--display-body (plan-markdown)
  "Return PLAN-MARKDOWN fontified for the plan approval interaction zone."
  (if (fboundp 'mevedel-view--fontify-as)
      (mevedel-view--fontify-as plan-markdown 'markdown-mode)
    plan-markdown))

(defun mevedel-plan-queue--render-entry (entry)
  "Render plan approval queue ENTRY in the interaction zone."
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
                 (mevedel-plan-queue--entry-execution-home entry)
                 :mode (mevedel-plan-queue--entry-implementation-mode
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
           (mevedel-plan-queue--cycle-entry-implementation-mode entry))
         (cycle-execution-home ()
           (interactive)
           (mevedel-plan-queue--cycle-entry-execution-home entry))
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
                   (mevedel-plan-queue--display-body plan-markdown)
                   "\n\n"
                   (mevedel-plan-queue--keys-line
                    (mevedel-plan-queue--entry-implementation-mode entry)
                    (mevedel-plan-queue--entry-execution-home entry)
                    (mevedel-plan-queue--entry-execution-home-mutable-p
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
                         :count (length (mevedel-plan-queue--get
                                         (plist-get entry :session)))
                         :body body
                         :priority 200
                         :keymap keymap
                         :help-echo nil
                         :entry entry
                         :activate
                         (lambda (outcome)
                           (mevedel-plan-queue--on-head-outcome
                            entry outcome)))))
            (overlay-put overlay 'mevedel-plan t)
            (overlay-put overlay 'mevedel-user-request t)
            (overlay-put overlay 'mevedel--callback
                         (lambda (outcome)
                           (mevedel-plan-queue--on-head-outcome
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
   "Plan feedback:\n\n%s\n\nRevise the proposed plan to address the feedback. Treat the current plan artifact as reference-only: read it if needed, but do not edit it. When the revised plan is decision-complete, emit exactly one full replacement <proposed_plan> block.\n\nCurrent plan artifact: %s"
   (or feedback "")
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
    (if (memq mode '(default accept-edits trust-all))
        mode
      (or (mevedel-session-permission-mode mevedel--session) 'default))))

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
          ('policy-changed nil)
          (_
           (message "mevedel: unknown plan outcome %S" outcome)))))))

(defun mevedel-goal--approval-entry
    (plan-markdown chat-buffer session &optional guardian-reason)
  "Return a plan approval queue entry for PLAN-MARKDOWN in CHAT-BUFFER SESSION."
  (list :body plan-markdown
        :chat-buffer chat-buffer
        :guardian-reason guardian-reason
        :origin "main"
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
        (mevedel-plan-queue--enqueue
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
               (null (mevedel-session-plan-queue session)))
      (when-let* ((plan-markdown
                   (mevedel-plan-current-body session))
                  ((not (string-blank-p plan-markdown))))
        (mevedel-plan-queue--enqueue
         (mevedel-goal--approval-entry
          plan-markdown chat-buffer session
          (when (plist-get metadata :guardian-pending)
            "Automatic guardian review was interrupted; explicit approval is required.")))))))

(defun mevedel-goal--pending-interaction-p (session)
  "Return non-nil when SESSION has queued or visible user interaction."
  (or (mevedel-session-queued-user-messages session)
      (mevedel-session-permission-queue session)
      (mevedel-session-plan-queue session)
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
  (let* ((goal (mevedel-session-goal session))
         (checkpoint (and goal (mevedel-goal-checkpoint goal))))
    (cond
     ((or (null goal)
          (not (mevedel-goal-owned-by-session-p goal session))
          (not (eq (mevedel-goal-status goal) 'active))
          (bound-and-true-p mevedel--current-request)
          (mevedel-goal--pending-interaction-p session)
          (not (eq source (plist-get checkpoint :phase)))
          (not (eq (or required-state 'settled)
                   (plist-get checkpoint :dispatch-state)))
          (and (eq target 'implementing)
               (eq (mevedel-goal-approval-policy goal) 'automatic)
               (not (mevedel-goal--guardian-approved-p goal session))))
      nil)
     ((not (mevedel-goal--budget-available-p goal session source))
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

(defun mevedel-goal-guardian-pending-p (&optional session)
  "Return non-nil when SESSION is waiting for an internal guardian result."
  (when-let* ((session (or session
                           (and (bound-and-true-p mevedel--session)
                                mevedel--session))))
    (plist-get (mevedel-session-plan-metadata session) :guardian-pending)))

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
                 (list :verdict 'ask
                       :reason
                       (format "Could not persist Goal guardian audit: %s"
                               (error-message-string err))))))
        (if (eq (plist-get decision :verdict) 'approve)
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
                     (if (and
                          (eq (mevedel-goal-approval-policy current-goal)
                              'automatic)
                          (mevedel-goal-continuation-ready-p
                           current-session 'guardian 'implementing))
                         (mevedel-goal--approval-callback
                          plan chat-buffer
                          (list :context
                                (mevedel-goal-implementation-context
                                 current-goal)))
                       (mevedel-goal-present-plan
                        plan chat-buffer
                        (when (eq (mevedel-goal-approval-policy current-goal)
                                  'automatic)
                          "Automatic approval deferred because user input or an interaction is pending."))))))))
          (mevedel-goal--enqueue-event-reminder
           session "guardian escalated plan approval to the user")
          (mevedel-goal-present-plan
           plan chat-buffer (plist-get decision :reason)))))))

(defun mevedel-goal--guard-current-plan (goal chat-buffer)
  "Run the mandatory automatic guardian for GOAL's current plan."
  (require 'mevedel-plan)
  (when-let* ((plan (mevedel-plan-current-body mevedel--session))
              ((not (string-blank-p plan))))
    (let ((goal-id (mevedel-goal-id goal))
          (plan-hash (mevedel-plan-hash plan)))
      (mevedel-goal--plan-metadata-put mevedel--session :guardian-pending t)
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
          (list :verdict 'ask
                :reason (format "Goal guardian failed: %s"
                                (error-message-string err)))))))))

(defun mevedel-goal--response-text (start end)
  "Return response text between START and END in the current buffer."
  (mevedel--normalize-message-text
   (buffer-substring-no-properties start end)))

(defun mevedel-goal--parse-review (text)
  "Return validated structured Goal review from TEXT, or nil."
  (let ((case-fold-search nil)
        (text (string-trim text)))
    (when (string-match
           (concat "\\`" (regexp-quote mevedel-goal--review-open-tag)
                   "[ \t]*\n"
                   "verdict:[ \t]*\\(complete\\|continue\\|blocked\\)"
                   "[ \t]*\n"
                   "summary:[ \t]*\\(\\(?:.\\|\n\\)*?\\)[ \t\n]*"
                   (regexp-quote mevedel-goal--review-close-tag) "\\'")
           text)
      (let ((summary (string-trim (match-string 2 text))))
        (unless (string-blank-p summary)
          (list :verdict (intern (match-string 1 text))
                :summary summary))))))

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
               (mevedel-goal--parse-review response)))))))

(defun mevedel-goal-settle-turn (fsm)
  "Advance Goal state after FSM reaches a successful terminal boundary."
  (when-let* ((info (gptel-fsm-info fsm))
              (phase (plist-get info :mevedel-goal-phase))
              (chat-buffer (plist-get info :buffer))
              ((buffer-live-p chat-buffer)))
    (with-current-buffer chat-buffer
      (when-let* ((goal (and (bound-and-true-p mevedel--session)
                             (mevedel-session-goal mevedel--session))))
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
                  (mevedel-goal-reason goal) "Paused by user")))))))

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
          (setf (mevedel-goal-pause-requested goal) nil))))))

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
