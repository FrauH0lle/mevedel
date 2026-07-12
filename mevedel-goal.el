;;; mevedel-goal.el -- Supervised goal workflow -*- lexical-binding: t -*-

;;; Commentary:

;; A session-owned Goal drives one supervised planning, approval,
;; implementation, and review cycle.  Plans use the lifecycle-neutral
;; artifact operations in `mevedel-plan'.

;;; Code:

(require 'cl-lib)

(eval-when-compile
  (require 'mevedel-plan)
  (require 'mevedel-tool-registry))
(require 'mevedel-queue)
(require 'mevedel-utilities)

;; `gptel'
(declare-function gptel-fsm-info "ext:gptel-request" (cl-x) t)
(declare-function gptel-send "ext:gptel" (&optional arg))
(defvar gptel-backend)
(defvar gptel-model)
(defvar gptel-prompt-prefix-alist)
(defvar gptel-reasoning-effort)
(defvar gptel-response-separator)

;; `mevedel-chat'
(declare-function mevedel--implement-plan "mevedel-chat" (action-plist))
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

;; `mevedel-structs'
(declare-function mevedel-goal--create "mevedel-structs" (&rest args))
(declare-function mevedel-goal-approval-policy "mevedel-structs" (cl-x) t)
(declare-function mevedel-goal-current-plan "mevedel-structs" (cl-x) t)
(declare-function mevedel-goal-id "mevedel-structs" (cl-x) t)
(declare-function mevedel-goal-objective "mevedel-structs" (cl-x) t)
(declare-function mevedel-goal-phase "mevedel-structs" (cl-x) t)
(declare-function mevedel-goal-review-summary "mevedel-structs" (cl-x) t)
(declare-function mevedel-goal-status "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-goal "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-name "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-permission-mode "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-plan-metadata "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-plan-queue "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-queued-user-messages
                  "mevedel-structs" (cl-x) t)
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
   "Goal objective:\n%s\n\nInvestigate the current repository state and propose a decision-complete implementation plan. This phase is read-only. End with exactly one line-oriented <proposed_plan>...</proposed_plan> block."
   (mevedel-goal-objective goal)))

(defun mevedel-goal--review-prompt (goal)
  "Return the one-cycle completion review request for GOAL."
  (format
   "Goal objective:\n%s\n\nAccepted plan: %s\n\nReview the implementation against the whole objective and current repository evidence. This phase is read-only. Do not make changes. State whether the objective is fully complete and explain the evidence."
   (mevedel-goal-objective goal)
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

(defun mevedel-goal-start (objective &optional display-text)
  "Start a supervised Goal for OBJECTIVE in the current session.
DISPLAY-TEXT is the user-facing form of the planning turn."
  (unless (bound-and-true-p mevedel--session)
    (user-error "No mevedel session in this buffer"))
  (when-let* ((existing (mevedel-session-goal mevedel--session)))
    (unless (eq (mevedel-goal-status existing) 'complete)
      (user-error "A non-complete Goal already owns this session")))
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
                               plan-markdown mevedel--session chat-buffer))
                   (current-artifact (plist-get artifacts :current))
                   (implementation-mode
                    (mevedel-goal--approval-implementation-mode outcome)))
              (mevedel-goal--ensure-reference-reminder mevedel--session)
              (setf (mevedel-goal-current-plan goal) current-artifact
                    (mevedel-goal-phase goal) 'implementing)
              (mevedel-goal--save-session-state mevedel--session chat-buffer)
              (let ((fsm
                     (mevedel-goal--call-with-workload
                      'implementation
                      (lambda ()
                        (mevedel--implement-plan
                         (mevedel-plan-implementation-input
                          action current-artifact implementation-mode))))))
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
         plan-markdown mevedel--session chat-buffer)
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
           (mevedel-goal-present-plan plan (current-buffer))
           (setf (mevedel-goal-phase goal) 'awaiting-approval)))
        ('reviewing
         (setf (mevedel-goal-review-summary goal) response))))))

(defun mevedel-goal-settle-turn (fsm)
  "Advance Goal state after FSM reaches a successful terminal boundary."
  (when-let* ((info (gptel-fsm-info fsm))
              (phase (plist-get info :mevedel-goal-phase))
              (chat-buffer (plist-get info :buffer))
              ((buffer-live-p chat-buffer)))
    (with-current-buffer chat-buffer
      (when-let* ((goal (and (bound-and-true-p mevedel--session)
                             (mevedel-session-goal mevedel--session)))
                  ((eq (mevedel-goal-status goal) 'active))
                  ((eq phase (mevedel-goal-phase goal))))
        (pcase phase
          ('implementing
           (setf (mevedel-goal-phase goal) 'reviewing))
          ('reviewing
           (when (and (stringp (mevedel-goal-review-summary goal))
                      (not (string-blank-p
                            (mevedel-goal-review-summary goal))))
             (setf (mevedel-goal-status goal) 'complete))))))))

(defun mevedel-goal-dispatch-after-turn (fsm)
  "Dispatch Goal review after FSM's implementation turn has settled."
  (when-let* ((info (gptel-fsm-info fsm))
              ((eq (plist-get info :mevedel-goal-phase) 'implementing))
              (chat-buffer (plist-get info :buffer))
              ((buffer-live-p chat-buffer)))
    (with-current-buffer chat-buffer
      (when-let* ((goal (and (bound-and-true-p mevedel--session)
                             (mevedel-session-goal mevedel--session)))
                  ((eq (mevedel-goal-status goal) 'active))
                  ((eq (mevedel-goal-phase goal) 'reviewing)))
        (mevedel-goal--dispatch-phase
         'reviewing (mevedel-goal--review-prompt goal)
         "Review Goal implementation")))))


(provide 'mevedel-goal)
;;; mevedel-goal.el ends here
