;;; mevedel-goal.el -- Supervised goal workflow -*- lexical-binding: t -*-

;;; Commentary:

;; A session-owned Goal drives supervised planning, approval, implementation,
;; and evidence-review cycles.  Plans use the lifecycle-neutral
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
(declare-function gptel-send "ext:gptel" (&optional arg))
(defvar gptel-backend)
(defvar gptel-model)
(defvar gptel-prompt-prefix-alist)
(defvar gptel-reasoning-effort)
(defvar gptel-response-separator)

;; `mevedel-chat'
(declare-function mevedel--implement-plan "mevedel-chat" (action-plist))
(defvar mevedel--current-request)
(defvar mevedel--session)

;; `mevedel-interaction-prompt'
(declare-function mevedel--prompt--settle
                  "mevedel-interaction-prompt" (overlay outcome))

;; `mevedel-models'
(declare-function mevedel-model-resolve-workload
                  "mevedel-models"
                  (workload &optional explicit-selector explicit-effort))

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
(declare-function mevedel-goal-current-plan "mevedel-structs" (cl-x) t)
(declare-function mevedel-goal-cycle "mevedel-structs" (cl-x) t)
(declare-function mevedel-goal-cycles "mevedel-structs" (cl-x) t)
(declare-function mevedel-goal-id "mevedel-structs" (cl-x) t)
(declare-function mevedel-goal-objective "mevedel-structs" (cl-x) t)
(declare-function mevedel-goal-pause-requested "mevedel-structs" (cl-x) t)
(declare-function mevedel-goal-phase "mevedel-structs" (cl-x) t)
(declare-function mevedel-goal-reason "mevedel-structs" (cl-x) t)
(declare-function mevedel-goal-review-findings "mevedel-structs" (cl-x) t)
(declare-function mevedel-goal-review-summary "mevedel-structs" (cl-x) t)
(declare-function mevedel-goal-status "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-goal "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-name "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-permission-mode "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-plan-metadata "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-plan-queue "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-queued-user-messages
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-save-path "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-session-id "mevedel-structs" (cl-x) t)

;; `mevedel-view'
(declare-function mevedel-view--fontify-as "mevedel-view" (text mode))
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

;;
;;; Lifecycle

(defvar mevedel-goal-dispatch-function #'mevedel-goal--dispatch-gptel
  "Function called with PHASE, PROMPT, and DISPLAY-TEXT for a Goal request.")

(defconst mevedel-goal--review-open-tag "<goal_review>"
  "Opening tag for structured Goal review results.")

(defconst mevedel-goal--review-close-tag "</goal_review>"
  "Closing tag for structured Goal review results.")

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

(defun mevedel-goal--cycle-record (goal)
  "Return GOAL's current cycle record."
  (cl-find (mevedel-goal-cycle goal) (mevedel-goal-cycles goal)
           :key (lambda (record) (plist-get record :cycle))))

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
    (let* ((record (mevedel-goal--cycle-record goal))
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
   "Goal objective:\n%s\n\nCycle: %d\n%sInvestigate the current repository state and propose the next decision-complete implementation plan. This phase is read-only. End with exactly one line-oriented <proposed_plan>...</proposed_plan> block."
   (mevedel-goal-objective goal)
   (mevedel-goal-cycle goal)
   (if-let* ((findings (mevedel-goal-review-findings goal)))
       (format "Prior review findings to resolve:\n%s\n\n" findings)
     "")))

(defun mevedel-goal--review-prompt (goal)
  "Return the one-cycle completion review request for GOAL."
  (format
   "Goal objective:\n%s\n\nCycle: %d\nAccepted plan: %s\n\nReview the implementation against the whole objective and current repository evidence. This phase is read-only. Do not make changes. Return exactly one structured result with a single verdict and evidence:\n<goal_review>\nverdict: complete|continue|blocked\nsummary: evidence, remaining work, or blocker\n</goal_review>\nUse complete only when the whole objective is proven complete; use continue when another implementation cycle can make progress; use blocked only for a concrete external or decision blocker."
   (mevedel-goal-objective goal)
   (mevedel-goal-cycle goal)
   (or (plist-get (mevedel-goal-current-plan goal) :absolute-path)
       "unavailable")))

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
        (setf (mevedel-goal-pause-requested goal) t)
      (setf (mevedel-goal-status goal) 'paused
            (mevedel-goal-pause-requested goal) nil
            (mevedel-goal-reason goal) "Paused by user")
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
    (mevedel-goal--save-session-state mevedel--session (current-buffer))
    goal))

(defun mevedel-goal-clear ()
  "Remove current Goal state while preserving transcript, artifacts, and work."
  (mevedel-goal--current)
  (when (bound-and-true-p mevedel--current-request)
    (user-error "Wait for or abort the active request before clearing Goal"))
  (mevedel-plan-queue-abort-all mevedel--session)
  (setf (mevedel-session-goal mevedel--session) nil
        (mevedel-session-plan-metadata mevedel--session) nil)
  (mevedel-goal--save-session-state mevedel--session (current-buffer))
  nil)

(defun mevedel-goal-resume (&optional input)
  "Resume the current paused or blocked Goal, incorporating optional INPUT."
  (let ((goal (mevedel-goal--current)))
    (when (bound-and-true-p mevedel--current-request)
      (user-error "A request is already active"))
    (unless (memq (mevedel-goal-status goal) '(paused blocked))
      (user-error "Goal is not paused or blocked"))
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
    (condition-case err
        (pcase (mevedel-goal-phase goal)
          ('planning
           (mevedel-goal--dispatch-phase
            'planning (mevedel-goal--planning-prompt goal)
            (format "Resume Goal %s" (mevedel-goal-id goal))))
          ('awaiting-approval
           (mevedel-goal-restore-pending-approval
            mevedel--session (current-buffer)))
          ('implementing
           (setf (mevedel-goal-phase goal) 'reviewing)
           (mevedel-goal--dispatch-phase
            'reviewing (mevedel-goal--review-prompt goal)
            "Review interrupted Goal implementation"))
          ('reviewing
           (mevedel-goal--dispatch-phase
            'reviewing (mevedel-goal--review-prompt goal)
            "Resume Goal review")))
      (error
       (setf (mevedel-goal-status goal) 'paused
             (mevedel-goal-reason goal) (error-message-string err))
       (mevedel-goal--save-session-state mevedel--session (current-buffer))
       (signal (car err) (cdr err))))
    goal))

(defun mevedel-goal-start (objective &optional display-text)
  "Start a supervised Goal for OBJECTIVE in the current session.
DISPLAY-TEXT is the user-facing form of the planning turn."
  (unless (bound-and-true-p mevedel--session)
    (user-error "No mevedel session in this buffer"))
  (when-let* ((existing (mevedel-session-goal mevedel--session)))
    (when (bound-and-true-p mevedel--current-request)
      (user-error "A request is already active"))
    (when (and (not (eq (mevedel-goal-status existing) 'complete))
               (not (yes-or-no-p "Replace the unfinished Goal? ")))
      (user-error "Goal replacement aborted")))
  (let ((previous-goal (mevedel-session-goal mevedel--session))
        (previous-plan-metadata
         (mevedel-session-plan-metadata mevedel--session))
        (goal
         (mevedel-goal--create
          :id (mevedel-goal--new-id)
          :objective (mevedel-goal--validate-objective objective)
          :status 'active
          :phase 'planning
          :approval-policy 'supervised
          :cycle 1
          :cycles (list (list :cycle 1
                              :started-at (format-time-string "%FT%T%z")))
          :owner-session (or (mevedel-session-session-id mevedel--session)
                             (mevedel-session-name mevedel--session)))))
    (setf (mevedel-session-goal mevedel--session) goal
          (mevedel-session-plan-metadata mevedel--session) nil)
    (condition-case err
        (mevedel-goal--dispatch-phase
         'planning (mevedel-goal--planning-prompt goal)
         (or display-text (mevedel-goal-objective goal)))
      (error
       (setf (mevedel-session-goal mevedel--session) previous-goal
             (mevedel-session-plan-metadata mevedel--session)
             previous-plan-metadata)
       (signal (car err) (cdr err))))
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
  (let ((workload (pcase phase
                    ('planning 'planning)
                    ('reviewing 'review)
                    (_ (error "Goal phase cannot dispatch: %s" phase)))))
    (mevedel-goal--call-with-workload
     workload
     (lambda ()
       (let ((fsm (mevedel-goal--insert-and-send prompt display-text)))
         (when fsm
           (setf (gptel-fsm-info fsm)
                 (plist-put (gptel-fsm-info fsm)
                            :mevedel-goal-phase phase)))
         fsm)))))

(defun mevedel-goal--call-with-workload (workload function)
  "Call FUNCTION with WORKLOAD's resolved request policy."
  (require 'mevedel-models)
  (let* ((policy (mevedel-model-resolve-workload workload))
         (old-backend gptel-backend)
         (old-model gptel-model)
         (old-effort (and (boundp 'gptel-reasoning-effort)
                          gptel-reasoning-effort)))
    (mevedel-goal--record-phase-policy workload policy)
    (unwind-protect
        (progn
          (setq-local gptel-backend (plist-get policy :backend)
                      gptel-model (plist-get policy :model)
                      gptel-reasoning-effort (plist-get policy :effort))
          (funcall function))
      (setq-local gptel-backend old-backend
                  gptel-model old-model
                  gptel-reasoning-effort old-effort))))

(defun mevedel-goal--dispatch-phase (phase prompt display-text)
  "Dispatch Goal PHASE with PROMPT and DISPLAY-TEXT."
  (unless (memq phase '(planning reviewing))
    (error "Goal phase cannot dispatch: %s" phase))
  (funcall mevedel-goal-dispatch-function phase prompt display-text))

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
  (when (and (mevedel-goal--approval-action outcome)
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

(defun mevedel-plan-queue--keys-line (implementation-mode)
  "Return the plan approval key help line for IMPLEMENTATION-MODE."
  (concat
   (propertize "Keys: " 'font-lock-face 'help-key-binding)
   (propertize "RET" 'font-lock-face 'help-key-binding)
   " implement  "
   (propertize "I" 'font-lock-face 'help-key-binding)
   " implement (clear context)  "
   (propertize "TAB" 'font-lock-face 'help-key-binding)
   "/"
   (propertize "m" 'font-lock-face 'help-key-binding)
   (format " mode: %s  "
           (mevedel-goal--implementation-mode-label
            implementation-mode))
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
         (implementation-outcome (action)
           (list :action action
                 :mode (mevedel-plan-queue--entry-implementation-mode
                        entry)))
         (implement-plan ()
           (interactive)
           (let ((outcome (implementation-outcome 'implement)))
             (mevedel-goal--ensure-implementation-allowed entry outcome)
             (settle outcome)))
         (implement-plan-clear ()
           (interactive)
           (let ((outcome (implementation-outcome 'implement-clear)))
             (mevedel-goal--ensure-implementation-allowed entry outcome)
             (settle outcome)))
         (cycle-implementation-mode ()
           (interactive)
           (mevedel-plan-queue--cycle-entry-implementation-mode entry))
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
                   (mevedel-plan-queue--display-body plan-markdown)
                   "\n\n"
                   (mevedel-plan-queue--keys-line
                    (mevedel-plan-queue--entry-implementation-mode entry))
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

(defun mevedel-goal--approval-action (outcome)
  "Return implementation action represented by OUTCOME, or nil."
  (cond
   ((memq outcome '(implement implement-clear)) outcome)
   ((and (consp outcome)
         (memq (plist-get outcome :action)
               '(implement implement-clear)))
    (plist-get outcome :action))))

(defun mevedel-goal--approval-implementation-mode (outcome)
  "Return implementation permission mode represented by OUTCOME."
  (let ((mode (and (consp outcome) (plist-get outcome :mode))))
    (if (memq mode '(default accept-edits trust-all))
        mode
      (or (mevedel-session-permission-mode mevedel--session) 'default))))

(defun mevedel-goal--approval-callback
    (plan-markdown chat-buffer outcome)
  "Handle OUTCOME for a proposed PLAN-MARKDOWN in CHAT-BUFFER."
  (require 'mevedel-plan)
  (setq plan-markdown (mevedel--normalize-message-text plan-markdown))
  (when (buffer-live-p chat-buffer)
    (with-current-buffer chat-buffer
      (if-let* ((action (mevedel-goal--approval-action outcome)))
          (let* ((goal (mevedel-session-goal mevedel--session)))
            (unless (and goal
                         (eq (mevedel-goal-status goal) 'active)
                         (eq (mevedel-goal-phase goal) 'awaiting-approval))
              (user-error "Goal is not awaiting plan approval"))
            (let* ((artifacts (mevedel-plan-accept
                               plan-markdown mevedel--session chat-buffer nil
                               (mevedel-goal--current-plan-relative-path goal)
                               (mevedel-goal--cycle-plan-relative-path goal)))
                   (accepted-artifact (plist-get artifacts :accepted))
                   (plan-hash (mevedel-plan-hash plan-markdown))
                   (implementation-mode
                    (mevedel-goal--approval-implementation-mode outcome)))
              (mevedel-goal--ensure-reference-reminder mevedel--session)
              (setq accepted-artifact
                    (plist-put accepted-artifact :hash plan-hash))
              (setf (mevedel-goal-current-plan goal) accepted-artifact
                    (mevedel-goal-phase goal) 'implementing)
              (mevedel-goal--cycle-put goal :plan
                                       (plist-get accepted-artifact :path))
              (mevedel-goal--cycle-put goal :plan-hash plan-hash)
              (mevedel-goal--cycle-put goal :accepted-at
                                       (format-time-string "%FT%T%z"))
              (mevedel-goal--persist-cycle-index
               goal mevedel--session chat-buffer)
              (mevedel-goal--save-session-state mevedel--session chat-buffer)
              (let ((fsm
                     (mevedel-goal--call-with-workload
                      'implementation
                      (lambda ()
                        (mevedel--implement-plan
                         (mevedel-plan-implementation-input
                          action accepted-artifact implementation-mode))))))
                (when fsm
                  (setf (gptel-fsm-info fsm)
                        (plist-put (gptel-fsm-info fsm)
                                   :mevedel-goal-phase 'implementing))))))
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
        (_
         (message "mevedel: unknown plan outcome %S" outcome)))))))

(defun mevedel-goal--approval-entry
    (plan-markdown chat-buffer session)
  "Return a plan approval queue entry for PLAN-MARKDOWN in CHAT-BUFFER SESSION."
  (list :body plan-markdown
        :chat-buffer chat-buffer
        :origin "main"
        :session session
        :callback
        (lambda (outcome)
          (mevedel-goal--approval-callback
           plan-markdown chat-buffer outcome))))

(defun mevedel-goal-present-plan (plan-markdown &optional chat-buffer)
  "Present PLAN-MARKDOWN for approval in CHAT-BUFFER.
The latest presented plan is persisted to the session-local plan
artifact before the approval prompt is displayed."
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
          plan-markdown chat-buffer mevedel--session))))))

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
          plan-markdown chat-buffer session))))))

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
         (when-let* ((plan (mevedel-plan-extract-proposed response)))
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
               (mevedel-goal-present-plan plan (current-buffer))
               (setf (mevedel-goal-phase goal) 'awaiting-approval)))))
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
                             (mevedel-session-goal mevedel--session)))
                  ((eq (mevedel-goal-status goal) 'active)))
        (when (eq phase (mevedel-goal-phase goal))
          (pcase phase
            ('implementing
             (setf (mevedel-goal-phase goal) 'reviewing))
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
                          (mevedel-goal-reason goal) nil))
                   ('continue
                    (setf (mevedel-goal-cycle goal)
                          (1+ (mevedel-goal-cycle goal))
                          (mevedel-goal-phase goal) 'planning
                          (mevedel-goal-current-plan goal) nil
                          (mevedel-goal-review-findings goal)
                          (plist-get review :summary)
                          (mevedel-goal-review-summary goal) nil
                          (mevedel-goal-reason goal) nil)
                    (mevedel-goal--cycle-put goal :started-at
                                             (format-time-string "%FT%T%z"))
                    (mevedel-goal--persist-cycle-index
                     goal mevedel--session chat-buffer))
                   ('blocked
                    (setf (mevedel-goal-status goal) 'blocked
                          (mevedel-goal-reason goal)
                          (plist-get review :summary)))))
               (setf (mevedel-goal-status goal) 'paused
                     (mevedel-goal-reason goal)
                     "Goal review returned malformed structured output")))))
        (when (and (mevedel-goal-pause-requested goal)
                   (eq (mevedel-goal-status goal) 'active))
          (setf (mevedel-goal-status goal) 'paused
                (mevedel-goal-pause-requested goal) nil
                (mevedel-goal-reason goal) "Paused by user"))))))

(defun mevedel-goal-settle-failure (fsm)
  "Apply deferred Goal pause at FSM's failed terminal boundary.
Persistence belongs to the caller's post-teardown abort or error path."
  (when-let* ((info (gptel-fsm-info fsm))
              (chat-buffer (plist-get info :buffer))
              ((buffer-live-p chat-buffer)))
    (with-current-buffer chat-buffer
      (when-let* ((goal (and (bound-and-true-p mevedel--session)
                             (mevedel-session-goal mevedel--session)))
                  ((mevedel-goal-pause-requested goal)))
        (setf (mevedel-goal-status goal) 'paused
              (mevedel-goal-pause-requested goal) nil
              (mevedel-goal-reason goal) "Paused by user")))))

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
          (`(implementing . reviewing)
           (mevedel-goal--dispatch-phase
            'reviewing (mevedel-goal--review-prompt goal)
            "Review Goal implementation"))
          (`(reviewing . planning)
           (mevedel-goal--dispatch-phase
            'planning (mevedel-goal--planning-prompt goal)
            (format "Continue Goal with cycle %d"
                    (mevedel-goal-cycle goal)))))))))


(provide 'mevedel-goal)
;;; mevedel-goal.el ends here
