;;; mevedel-plan-mode.el -- Plan conversation and proposal UI -*- lexical-binding: t -*-

;;; Commentary:

;; Sticky Plan conversations and proposal approval interaction.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'mevedel-structs))

;; `gptel'
(defvar gptel-reasoning-effort)

;; `mevedel-interaction-prompt'
(declare-function mevedel--prompt-announce "mevedel-interaction-prompt"
                  (overlay))
(declare-function mevedel--prompt--settle "mevedel-interaction-prompt"
		  (overlay outcome))
(declare-function mevedel--prompt-framed-body
                  "mevedel-interaction-prompt" (content face))
(declare-function mevedel--prompt-key
                  "mevedel-interaction-prompt" (key))

;; `mevedel-menu'
(declare-function mevedel-menu-open-model-selection "mevedel-menu"
                  (&rest options))

;; `mevedel-models'
(declare-function mevedel-model-current-provider-label
                  "mevedel-models" (&optional buffer))
(declare-function mevedel-model-resolve-provider
                  "mevedel-models" (spec &optional noerror))

;; `mevedel-plan'
(declare-function mevedel-plan-accept "mevedel-plan"
		  (plan-markdown session buffer &optional skip-verification
				 current-relative-path accepted-relative-path))
(declare-function mevedel-plan-current-body "mevedel-plan"
		  (&optional session))
(declare-function mevedel-plan-hash "mevedel-plan" (plan-markdown))
(declare-function mevedel-plan-resource-address "mevedel-plan"
		  (relative-path))
(defvar mevedel-plan--relative-current-path)

;; `mevedel-plan-handoff'
(declare-function mevedel-plan-handoff-selection-valid-p
                  "mevedel-plan-handoff" (selection))
(declare-function mevedel-plan-handoff-start "mevedel-plan-handoff"
                  (session chat-buffer selection accepted))
(defvar mevedel-plan-handoff-implementation-modes)

;; `mevedel-queue'
(declare-function mevedel-queue--current-session "mevedel-queue" ())
(declare-function mevedel-queue--entry-metadata-put
                  "mevedel-queue" (entry key value))
(declare-function mevedel-queue--unregister-entry-interaction
                  "mevedel-queue" (entry))

;; `mevedel-skills-core'
(declare-function mevedel-skill-name "mevedel-skills-core" (cl-x) t)
(declare-function mevedel-skill-source-file "mevedel-skills-core" (cl-x) t)

;; `mevedel-skills-invoke'
(declare-function mevedel-skills-prepare-user-input
                  "mevedel-skills-invoke" (text session))

;; `mevedel-skills-ui'
(declare-function mevedel-skills--refresh-view-input-prompt
		  "mevedel-skills-ui" nil)
(declare-function mevedel-skills--user-visible-skills
                  "mevedel-skills-ui" (session &optional inline-only))

;; `mevedel-structs'
(declare-function mevedel-goal-status "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-directive-planning
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-goal "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-pending-follow-ups
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-pending-plan-approval
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-pending-steering
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-permission-mode "mevedel-structs"
		  (cl-x) t)
(declare-function mevedel-session-plan-metadata "mevedel-structs"
		  (cl-x) t)
(declare-function mevedel-session-plan-mode "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-persistence-assert-new-mutation-authority
                  "mevedel-session-persistence" (session))
(declare-function mevedel-session-preset-name "mevedel-structs" (cl-x)
		  t)
(declare-function mevedel-session-preset-settings "mevedel-structs"
		  (cl-x) t)
(declare-function mevedel-session-save-path "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-session-id "mevedel-structs" (cl-x)
		  t)
(declare-function mevedel-session-turn-count "mevedel-structs" (cl-x)
		  t)
(declare-function mevedel-session-working-directory "mevedel-structs"
		  (cl-x) t)
(defvar mevedel--data-buffer)
(defvar mevedel--session)
(defvar mevedel-goal-token-budget)

;; `mevedel-transcript'
(declare-function mevedel-transcript-segments "mevedel-transcript"
		  (start end))

;; `mevedel-utilities'
(declare-function mevedel--normalize-message-text "mevedel-utilities"
		  (text))

;; `mevedel-view-composer'
(declare-function mevedel-view-enqueue-external-follow-up
                  "mevedel-view-composer" (data-buffer text &rest keys))
(declare-function mevedel-view--clear-input "mevedel-view-composer"
		  nil)
(declare-function mevedel-view--input-start "mevedel-view-composer"
		  nil)

;; `mevedel-view-interaction'
(declare-function mevedel-view--interaction-register
		  "mevedel-view-interaction" (descriptor))
(declare-function mevedel-view--interaction-target-buffer
		  "mevedel-view-interaction" (chat-buffer))
(declare-function mevedel-view--interaction-unregister
                  "mevedel-view-interaction" (id))

;; `mevedel-view-markdown'
(declare-function mevedel-view--fontify-as "mevedel-view-markdown"
		  (text mode))

;; `mevedel-worktree'
(declare-function mevedel-worktree--collect-status "mevedel-worktree"
		  (&optional context))
(declare-function mevedel-worktree--default-branch-name
		  "mevedel-worktree" (session purpose))
(declare-function mevedel-worktree--validate-branch-name
		  "mevedel-worktree" (name &optional directory))

;;
;;; Plan conversation mode

(defun mevedel-plan-mode--current-session (&optional session)
  "Return SESSION or the session reachable from the current buffer."
  (or session
      (and (boundp 'mevedel--session) mevedel--session)
      (and (boundp 'mevedel--data-buffer)
           (buffer-live-p mevedel--data-buffer)
           (buffer-local-value 'mevedel--session mevedel--data-buffer))))

(defun mevedel-plan-mode-active-p (&optional session)
  "Return non-nil when SESSION is in a Plan conversation."
  (when-let* ((session (mevedel-plan-mode--current-session session)))
    (mevedel-session-plan-mode session)))

(defun mevedel-plan-mode-enter (&optional session)
  "Enter a sticky Plan conversation for SESSION."
  (interactive)
  (let ((session (mevedel-plan-mode--current-session session)))
    (unless session
      (user-error "No mevedel session for Plan mode"))
    (require 'mevedel-session-persistence)
    (mevedel-session-persistence-assert-new-mutation-authority session)
    (when (mevedel-session-directive-planning session)
      (user-error "Finish or cancel directive planning before entering Plan"))
    (when-let* ((goal (mevedel-session-goal session)))
      (unless (eq (mevedel-goal-status goal) 'complete)
        (user-error "Finish or clear the current Goal before entering Plan")))
    (unless (mevedel-session-plan-mode session)
      (let ((metadata (copy-sequence
                       (or (mevedel-session-plan-metadata session) nil))))
        (cl-remf metadata :selection)
        (setf (mevedel-session-plan-metadata session) metadata)))
    (setf (mevedel-session-plan-mode session) t)
    (when (fboundp 'mevedel-skills--refresh-view-input-prompt)
      (mevedel-skills--refresh-view-input-prompt))
    (force-mode-line-update t)
    t))

(defun mevedel-plan-mode-exit (&optional session)
  "Leave the Plan conversation for SESSION."
  (interactive)
  (let ((session (mevedel-plan-mode--current-session session)))
    (when (and session (mevedel-session-plan-mode session))
      (mevedel-plan-mode--demote-proposal session t)
      (when (mevedel-session-pending-plan-approval session)
        (require 'mevedel-goal)
        (mevedel-plan-approval-abort session 'plan-exit))
      (mevedel-plan-mode--deactivate session))
    nil))

;;; Plan proposals

(defun mevedel-plan-mode--deactivate (session)
  "Leave Plan in SESSION without changing proposal metadata."
  (setf (mevedel-session-plan-mode session) nil)
  (when (fboundp 'mevedel-skills--refresh-view-input-prompt)
    (mevedel-skills--refresh-view-input-prompt))
  (force-mode-line-update t))

(defun mevedel-plan-mode--demote-proposal (session discard-selection)
  "Make SESSION's proposal a draft.
When DISCARD-SELECTION is non-nil, discard its approval selection too."
  (let ((metadata (copy-sequence
                   (or (mevedel-session-plan-metadata session) nil))))
    (when (eq (plist-get metadata :status) 'proposed)
      (setq metadata (plist-put metadata :status 'draft)))
    (cl-remf metadata :proposal-id)
    (when discard-selection
      (cl-remf metadata :selection))
    (setf (mevedel-session-plan-metadata session) metadata)
    metadata))

(defun mevedel-plan-mode--invalidate-proposal (&optional session)
  "Demote and dismiss SESSION's actionable proposal, preserving selection."
  (when-let* ((session (mevedel-plan-mode--current-session session))
              ((mevedel-session-plan-mode session))
              ((eq (plist-get (mevedel-session-plan-metadata session) :status)
                   'proposed)))
    (mevedel-plan-mode--demote-proposal session nil)
    (when (mevedel-session-pending-plan-approval session)
      (mevedel-plan-approval-abort session 'invalidated))
    t))

(defun mevedel-plan-mode--assistant-prose (start end)
  "Return root-assistant prose in START..END, excluding tool evidence."
  (require 'mevedel-transcript)
  (mevedel--normalize-message-text
   (mapconcat
    (lambda (segment)
      (if (eq (car segment) 'response)
          (buffer-substring-no-properties (cadr segment) (caddr segment))
        ""))
    (mevedel-transcript-segments start end)
    "\n")))

(defun mevedel-plan-mode--default-selection (session)
  "Return the default Direct implementation selection for SESSION."
  (list :location 'here
        :context 'current
        :execution 'direct
        :mode (or (mevedel-session-permission-mode session) 'ask)
        :model-provider
        (progn
          (require 'mevedel-models)
          (mevedel-model-current-provider-label))
        :reasoning-effort
        (and (boundp 'gptel-reasoning-effort) gptel-reasoning-effort)
        :goal-token-budget
        (mevedel-plan-mode--effective-goal-budget (current-buffer))
        :skills nil
        :instructions nil))

(defun mevedel-plan-mode--next-mode (mode)
  "Return the Plan implementation mode after MODE."
  (require 'mevedel-plan-handoff)
  (or (cadr (memq mode mevedel-plan-handoff-implementation-modes))
      (car mevedel-plan-handoff-implementation-modes)))

(defun mevedel-plan-mode--next-execution (execution)
  "Return the Plan execution choice after EXECUTION."
  (if (eq execution 'direct) 'goal 'direct))

(defun mevedel-plan-mode--next-context (location context)
  "Return the implementation context after CONTEXT at LOCATION."
  (if (eq location 'worktree)
      (if (eq context 'fresh) 'summary 'fresh)
    (or (cadr (memq context '(current fresh summary))) 'current)))

(defun mevedel-plan-mode--next-location (selection)
  "Toggle SELECTION's location while preserving a valid context."
  (if (eq (plist-get selection :location) 'here)
      (progn
        (plist-put selection :location 'worktree)
        (when (eq (plist-get selection :context) 'current)
          (plist-put selection :context 'fresh)))
    (plist-put selection :location 'here))
  selection)

(defun mevedel-plan-mode--context-description (context)
  "Return the compact UI description for CONTEXT."
  (pcase context
    ('current "full planning transcript")
    ('fresh "setup context + accepted plan")
    ('summary "compact handoff + accepted plan (additional model request)")))

(defun mevedel-plan-mode--execution-description (execution)
  "Return the compact UI description for EXECUTION."
  (if (eq execution 'goal)
      "continue until complete, blocked, paused, or budget-limited"
    "one implementation turn"))

(defun mevedel-plan-mode--effective-goal-budget (buffer)
  "Return the Goal budget effective in BUFFER, or nil when unbounded."
  (and (boundp 'mevedel-goal-token-budget)
       (buffer-local-value 'mevedel-goal-token-budget buffer)))


(defun mevedel-plan-mode--accept
    (plan-markdown chat-buffer session selection)
  "Accept PLAN-MARKDOWN and dispatch SELECTION from CHAT-BUFFER SESSION."
  (require 'mevedel-plan-handoff)
  (unless (mevedel-plan-handoff-selection-valid-p selection)
    (error "Unsupported Plan implementation selection: %S" selection))
  (let* ((artifacts (mevedel-plan-accept
                     plan-markdown session chat-buffer t))
         (accepted (plist-get artifacts :accepted)))
    (mevedel-plan-mode--deactivate session)
    (mevedel-plan-handoff-start
     session chat-buffer selection accepted)))

(defun mevedel-plan-mode--feedback-draft (chat-buffer)
  "Insert an editable replacement-plan request for CHAT-BUFFER."
  (let ((target (mevedel-view--interaction-target-buffer chat-buffer)))
    (with-current-buffer target
      (mevedel-view--clear-input)
      (goto-char (mevedel-view--input-start))
      (let ((start (point)))
        (insert
         (format
          "Plan feedback:\n\n\n\nRevise the proposal to address this feedback. Emit one complete replacement <proposed_plan> block; the current draft is reference-only.\n\nCurrent plan artifact: %s"
          (mevedel-plan-resource-address
           mevedel-plan--relative-current-path)))
        (goto-char start)
        (forward-line 2)))))

(defun mevedel-plan-mode--remote-feedback (chat-buffer text)
  "Queue TEXT as a revision request for CHAT-BUFFER's demoted proposal.
The remote counterpart of the Emacs feedback draft: the same request
template, submitted immediately as a queued follow-up instead of
opening an editable draft."
  (require 'mevedel-view-composer)
  (mevedel-view-enqueue-external-follow-up
   chat-buffer
   (format
    "Plan feedback:\n\n%s\n\nRevise the proposal to address this feedback. Emit one complete replacement <proposed_plan> block; the current draft is reference-only.\n\nCurrent plan artifact: %s"
    text
    (with-current-buffer chat-buffer
      (mevedel-plan-resource-address mevedel-plan--relative-current-path)))
   :guest-name (and (boundp 'mevedel-collaboration-remote-guest)
                    mevedel-collaboration-remote-guest)))

(defun mevedel-plan-mode--approval-callback
    (plan-markdown chat-buffer session outcome)
  "Handle Plan proposal OUTCOME for PLAN-MARKDOWN in CHAT-BUFFER SESSION."
  (cond
   ((and (proper-list-p outcome) (plist-get outcome :accept))
    (mevedel-plan-mode--accept
     plan-markdown chat-buffer session (plist-get outcome :selection)))
   ((and (proper-list-p outcome) (plist-get outcome :remote-feedback))
    (mevedel-plan-mode--demote-proposal session nil)
    (mevedel-plan-mode--remote-feedback
     chat-buffer (plist-get outcome :remote-feedback)))
   ((eq outcome 'feedback-draft)
    (mevedel-plan-mode--demote-proposal session nil)
    (mevedel-plan-mode--feedback-draft chat-buffer))
   ((eq outcome 'aborted)
    (mevedel-plan-mode--demote-proposal session t))
   ((memq outcome '(invalidated plan-exit superseded)) nil)
   (t (message "mevedel: unknown Plan proposal outcome %S" outcome))))

(defun mevedel-plan-mode--approval-entry
    (plan-markdown chat-buffer session selection)
  "Return a Plan-mode approval entry for PLAN-MARKDOWN and SELECTION."
  (list :body plan-markdown
        :chat-buffer chat-buffer
        :origin "/root"
        :session session
        :selection selection
        :interaction-id (list :plan-mode (gensym "plan-"))
        :renderer #'mevedel-plan-mode--render-approval
        :callback
        (lambda (outcome)
          (mevedel-plan-mode--approval-callback
           plan-markdown chat-buffer session outcome))))

(defun mevedel-plan-mode--read-worktree-branch (entry)
  "Read and validate a Worktree branch for approval ENTRY."
  (require 'mevedel-worktree)
  (let* ((session (plist-get entry :session))
         (directory (mevedel-session-working-directory session))
         (default (mevedel-worktree--default-branch-name
                   session "accepted-plan"))
         (branch (read-string "Worktree branch name: " nil nil default)))
    (mevedel-worktree--validate-branch-name branch directory)
    branch))

(defun mevedel-plan-mode--worktree-warning (entry)
  "Return ENTRY's dirty-source warning when Worktree is selected."
  (when (eq (plist-get (plist-get entry :selection) :location) 'worktree)
    (require 'mevedel-worktree)
    (with-current-buffer (plist-get entry :chat-buffer)
      (when (plist-get (mevedel-worktree--collect-status) :dirty-p)
        "Worktree starts at HEAD; uncommitted changes are not included."))))

(defvar-local mevedel-plan-mode--instructions-entry nil
  "Plan approval entry edited by the current instructions buffer.")

(defun mevedel-plan-mode--selection-changed (entry)
  "Persist ENTRY's changed selection and redraw its approval."
  (let ((selection (plist-get entry :selection))
        (session (plist-get entry :session)))
    (if-let* ((callback (plist-get entry :selection-changed)))
        (funcall callback selection)
      (mevedel-plan--metadata-put session :selection selection))
    (mevedel-plan-approval-render session)))

(defun mevedel-plan-mode--save-instructions ()
  "Save the current Plan implementation instructions and close the editor."
  (interactive)
  (unless mevedel-plan-mode--instructions-entry
    (user-error "No Plan implementation instructions to save"))
  (require 'subr-x)
  (let* ((entry mevedel-plan-mode--instructions-entry)
         (session (plist-get entry :session))
         (selection (plist-get entry :selection))
         (instructions (string-trim
                        (buffer-substring-no-properties
                         (point-min) (point-max))))
         (instructions
          (unless (string-empty-p instructions)
            (with-current-buffer (plist-get entry :chat-buffer)
              (require 'mevedel-skills-invoke)
              (mevedel-skills-prepare-user-input
               instructions session)))))
    (plist-put selection :instructions
               instructions)
    (kill-buffer (current-buffer))
    (mevedel-plan-mode--selection-changed entry)))

(defun mevedel-plan-mode--cancel-instructions ()
  "Close the current Plan instructions editor without saving."
  (interactive)
  (kill-buffer (current-buffer)))

(defun mevedel-plan-mode--edit-instructions (entry)
  "Edit implementation instructions for Plan approval ENTRY."
  (let* ((selection (plist-get entry :selection))
         (buffer (generate-new-buffer "*mevedel Plan instructions*")))
    (with-current-buffer buffer
      (text-mode)
      (use-local-map (copy-keymap text-mode-map))
      (local-set-key (kbd "C-c C-c")
                     #'mevedel-plan-mode--save-instructions)
      (local-set-key (kbd "C-c C-k")
                     #'mevedel-plan-mode--cancel-instructions)
      (setq-local mevedel-plan-mode--instructions-entry entry)
      (insert (or (plist-get selection :instructions) ""))
      (goto-char (point-max)))
    (pop-to-buffer buffer)))

(defun mevedel-plan-mode--toggle-skill (entry)
  "Toggle one user-invocable implementation skill for approval ENTRY."
  (require 'mevedel-skills-ui)
  (let* ((selection (plist-get entry :selection))
         (skills
          (mevedel-skills--user-visible-skills
           (plist-get entry :session)))
         (candidates
          (mapcar
           (lambda (skill)
             (cons (mevedel-skill-name skill) skill))
           skills)))
    (unless candidates
      (user-error "No user-invocable skills available"))
    (let* ((choice (completing-read "Toggle implementation skill: "
                                    candidates nil t))
           (skill (cdr (assoc choice candidates)))
           (source-file (mevedel-skill-source-file skill))
           (selected (plist-get selection :skills))
           (existing
            (cl-find source-file selected
                     :key (lambda (item) (plist-get item :source-file))
                     :test #'equal)))
      (plist-put
       selection :skills
       (if existing
           (delete existing selected)
         (append selected
                 (list (list :name (mevedel-skill-name skill)
                             :source-file source-file)))))
      (mevedel-plan-mode--selection-changed entry))))

(defun mevedel-plan-mode--skills-label (selection)
  "Return the compact selected-skills label for SELECTION."
  (if-let* ((skills (plist-get selection :skills)))
      (mapconcat (lambda (skill) (plist-get skill :name)) skills ", ")
    "None"))

(defun mevedel-plan-mode--instructions-label (selection)
  "Return the compact instructions label for SELECTION."
  (if-let* ((instructions (plist-get selection :instructions)))
      (format "%d line%s"
              (length (string-lines instructions))
              (if (string-match-p "\n" instructions) "s" ""))
    "None"))

(defun mevedel-plan-mode--render-approval (entry)
  "Render standalone Plan approval ENTRY in the interaction zone."
  (require 'mevedel-interaction-prompt)
  (let ((chat-buffer (plist-get entry :chat-buffer))
        (selection (plist-get entry :selection))
        (directive-p (plist-get entry :directive))
        overlay)
    (cl-labels
        ((deliver (outcome)
           (mevedel-plan-approval-settle entry outcome))
         (settle (outcome)
           (when overlay (mevedel--prompt--settle overlay outcome)))
         (accept ()
           (interactive)
           (require 'mevedel-models)
           (let ((accepted (copy-tree selection))
                 (provider (plist-get selection :model-provider)))
             (when (eq (plist-get accepted :location) 'worktree)
               (plist-put accepted :branch
                          (mevedel-plan-mode--read-worktree-branch entry)))
             (unless (string-match-p "\\`[^:]+:.+\\'" provider)
               (user-error "Select a registered model before implementing"))
             (unless (mevedel-model-resolve-provider provider t)
               (user-error "Select a registered model before implementing"))
             (settle (list :accept t :selection accepted))))
         (cycle-mode ()
           (interactive)
           (plist-put selection :mode
                      (mevedel-plan-mode--next-mode
                       (plist-get selection :mode)))
           (mevedel-plan-mode--selection-changed entry))
         (cycle-context ()
           (interactive)
           (plist-put selection :context
                      (mevedel-plan-mode--next-context
                       (plist-get selection :location)
                       (plist-get selection :context)))
           (mevedel-plan-mode--selection-changed entry))
         (cycle-execution ()
           (interactive)
           (plist-put selection :execution
                      (mevedel-plan-mode--next-execution
                       (plist-get selection :execution)))
           (mevedel-plan-mode--selection-changed entry))
         (cycle-location ()
           (interactive)
           (mevedel-plan-mode--next-location selection)
           (mevedel-plan-mode--selection-changed entry))
         (edit-budget ()
           (interactive)
           (let* ((current (plist-get selection :goal-token-budget))
                  (input
                   (read-string
                    "Goal token budget (empty for Unlimited): "
                    (and current (number-to-string current))))
                  (budget
                   (cond
                    ((string-match-p "\\`[[:space:]]*\\'" input) nil)
                    ((string-match
                      "\\`[[:space:]]*\\([1-9][0-9]*\\)[[:space:]]*\\'"
                      input)
                     (string-to-number (match-string 1 input)))
                    (t
                     (user-error
                      "Goal token budget must be a positive integer or empty")))))
             (plist-put selection :goal-token-budget budget)
             (mevedel-plan-mode--selection-changed entry)))
         (open-model ()
           (interactive)
           (require 'mevedel-menu)
           (mevedel-menu-open-model-selection
            :title "Implementation model"
            :provider (plist-get selection :model-provider)
            :effort (plist-get selection :reasoning-effort)
            :update
            (lambda (provider effort)
              (plist-put selection :model-provider provider)
              (plist-put selection :reasoning-effort effort)
              (mevedel-plan-mode--selection-changed entry))))
         (toggle-skill ()
           (interactive)
           (mevedel-plan-mode--toggle-skill entry))
         (edit-instructions ()
           (interactive)
           (mevedel-plan-mode--edit-instructions entry))
         (feedback () (interactive) (settle 'feedback-draft))
         (hide ()
           (interactive)
           (plist-put entry :hidden t)
           (mevedel-view--interaction-unregister
            (plist-get entry :interaction-id))
           (when-let* ((position
                        (text-property-any
                         (point-min) (point-max)
                         'mevedel-view-pending-plan t)))
             (goto-char position)))
         (cancel () (interactive) (settle 'aborted)))
      (let ((target (mevedel-view--interaction-target-buffer chat-buffer)))
        (mevedel-queue--entry-metadata-put
         entry :interaction-id (plist-get entry :interaction-id))
        (mevedel-queue--entry-metadata-put entry :view-buffer target)
        (with-current-buffer target
          (let* ((keymap (make-sparse-keymap))
                 (warning (and (not directive-p)
                               (mevedel-plan-mode--worktree-warning entry)))
                 (execution (plist-get selection :execution))
                 (budget (plist-get selection :goal-token-budget))
                 (body
                  (mevedel--prompt-framed-body
                   (concat
                    (if (fboundp 'mevedel-view--fontify-as)
                        (mevedel-view--fontify-as
                         (plist-get entry :body) 'markdown-mode)
                      (plist-get entry :body))
                    "\n\n"
                    (propertize (if directive-p
                                    "Directive implementation"
                                  "Implementation")
                                'font-lock-face 'mevedel-view-plan-mode)
                    "\n\n"
                    (unless directive-p
                      (concat
                       (mevedel--prompt-key "l")
                       "  Location    "
                       (propertize
                        (capitalize
                         (symbol-name (plist-get selection :location)))
                        'font-lock-face 'bold)
                       "\n"
                       (mevedel--prompt-key "c")
                       "  Context     "
                       (propertize
                        (capitalize
                         (symbol-name (plist-get selection :context)))
                        'font-lock-face 'bold)
                       " — "
                       (mevedel-plan-mode--context-description
                        (plist-get selection :context))
                       "\n"
                       (mevedel--prompt-key "e")
                       "  Execution   "
                       (propertize
                        (capitalize (symbol-name execution))
                        'font-lock-face 'bold)
                       " — "
                       (mevedel-plan-mode--execution-description execution)
                       "\n"
                       (when (eq execution 'goal)
                         (concat
                          (mevedel--prompt-key "b")
                          "  Budget      "
                          (propertize
                           (if budget
                               (format "%d tokens" budget)
                             "Unlimited")
                           'font-lock-face 'bold)
                          "\n"))))
                    (mevedel--prompt-key "m")
                    "  Mode        "
                    (propertize
                     (capitalize
                      (symbol-name (plist-get selection :mode)))
                     'font-lock-face 'bold)
                    "\n"
                    (mevedel--prompt-key "M")
                    "  Model       "
                    (propertize
                     (format
                      "%s · effort %s"
                      (plist-get selection :model-provider)
                      (or (plist-get selection :reasoning-effort)
                          "default"))
                     'font-lock-face 'bold)
                    "\n"
                    (mevedel--prompt-key "s")
                    "  Skills      "
                    (propertize
                     (mevedel-plan-mode--skills-label selection)
                     'font-lock-face 'bold)
                    "\n"
                    (mevedel--prompt-key "i")
                    "  Instructions "
                    (propertize
                     (mevedel-plan-mode--instructions-label selection)
                     'font-lock-face 'bold)
                    "\n"
                    (when warning
                      (concat "\n"
                              (propertize warning
                                          'font-lock-face 'warning)
                              "\n"))
                    "\n"
                    (mevedel--prompt-key "RET")
                    " implement    "
                    (mevedel--prompt-key "f")
                    " feedback    "
                    (unless directive-p
                      (concat (mevedel--prompt-key "q") " hide    "))
                    (mevedel--prompt-key "C-g")
                    " cancel\n")
                   'mevedel-view-plan-mode)))
            (define-key keymap (kbd "RET") #'accept)
            (define-key keymap (kbd "<return>") #'accept)
            (define-key keymap (kbd "C-c C-c") #'accept)
            (define-key keymap (kbd "TAB") #'cycle-mode)
            (define-key keymap (kbd "<tab>") #'cycle-mode)
            (define-key keymap (kbd "m") #'cycle-mode)
            (define-key keymap (kbd "M") #'open-model)
            (define-key keymap (kbd "s") #'toggle-skill)
            (define-key keymap (kbd "i") #'edit-instructions)
            (unless directive-p
              (define-key keymap (kbd "e") #'cycle-execution)
              (define-key keymap (kbd "l") #'cycle-location)
              (define-key keymap (kbd "c") #'cycle-context)
              (when (eq execution 'goal)
                (define-key keymap (kbd "b") #'edit-budget)))
            (define-key keymap (kbd "f") #'feedback)
            (unless directive-p
              (define-key keymap (kbd "q") #'hide))
            (define-key keymap (kbd "C-g") #'cancel)
            (setq overlay
                  (mevedel-view--interaction-register
                   (list :kind 'plan
                         :id (plist-get entry :interaction-id)
                         :count 1
                         :body body
                         :priority 200
                         :keymap keymap
                         :entry entry
                         :activate #'deliver)))
            (overlay-put overlay 'mevedel-plan t)
            (overlay-put overlay 'mevedel-user-request t)
            (overlay-put overlay 'mevedel--callback #'deliver)
            (overlay-put overlay 'keymap keymap)
            ;; Remote acceptance uses the host-configured axes verbatim;
            ;; axis editing and Worktree acceptance (which prompts for a
            ;; branch) stay in Emacs, so a Worktree proposal offers no
            ;; remote accept at all instead of a dead button.  Remote
            ;; feedback demotes the proposal and queues the same revision
            ;; request the Emacs feedback draft composes.
            (overlay-put overlay 'mevedel--remote
                         (append
                          (list :body (substring-no-properties body)
                                :feedback
                                (lambda (text)
                                  (settle (list :remote-feedback text))))
                          (unless (eq (plist-get selection :location)
                                      'worktree)
                            (list :options
                                  (list (cons (lambda () (accept))
                                              "Accept plan"))))))
            (mevedel--prompt-announce overlay)))))))

(defun mevedel-plan-mode--post-response (start end)
  "Present a complete root-assistant proposal from START..END once."
  (when-let* ((session (mevedel-plan-mode--current-session))
              ((mevedel-session-plan-mode session))
              (plan (mevedel-plan-extract-proposed
                     (mevedel-plan-mode--assistant-prose start end)))
              (hash (mevedel-plan-hash plan)))
    (let* ((proposal-id (list start end hash))
           (metadata (mevedel-session-plan-metadata session)))
      (unless (equal proposal-id (plist-get metadata :proposal-id))
        (let ((selection (or (plist-get metadata :selection)
                             (mevedel-plan-mode--default-selection session))))
          (mevedel-plan-write-current plan session (current-buffer))
          (mevedel-plan--metadata-put session :status 'proposed)
          (mevedel-plan--metadata-put session :proposal-id proposal-id)
          (mevedel-plan--metadata-put session :selection selection)
          (mevedel-plan-approval-present
           (mevedel-plan-mode--approval-entry
            plan (current-buffer) session selection)
           session))))))

(defun mevedel-plan-mode-restore-pending-approval
    (&optional session chat-buffer)
  "Restore SESSION's genuine pending Plan proposal in CHAT-BUFFER."
  (require 'mevedel-plan-handoff)
  (let* ((session (mevedel-plan-mode--current-session session))
         (chat-buffer (or chat-buffer (current-buffer)))
         (metadata (and session (mevedel-session-plan-metadata session)))
         (proposal-id (plist-get metadata :proposal-id))
         (selection (plist-get metadata :selection))
         (hash (plist-get metadata :hash))
         (plan (and session
                    (ignore-errors (mevedel-plan-current-body session))))
         (valid
          (and session
               (mevedel-session-plan-mode session)
               (eq (plist-get metadata :status) 'proposed)
               (proper-list-p proposal-id)
               (= (length proposal-id) 3)
               (integerp (nth 0 proposal-id))
               (integerp (nth 1 proposal-id))
               (stringp (nth 2 proposal-id))
               (equal hash (nth 2 proposal-id))
               (mevedel-plan-handoff-selection-valid-p selection)
               (stringp (plist-get selection :model-provider))
               (plist-member selection :reasoning-effort)
               (stringp plan)
               (equal hash (mevedel-plan-hash plan)))))
    (cond
     ((and valid (not (mevedel-session-pending-plan-approval session)))
      (mevedel-plan-approval-present
       (mevedel-plan-mode--approval-entry
        plan chat-buffer session selection)
       session)
      t)
     ((and session
           (mevedel-session-plan-mode session)
           (eq (plist-get metadata :status) 'proposed)
           (not valid))
      (mevedel-plan-mode--demote-proposal session t)
      nil))))

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
              (entry (mevedel-session-pending-plan-approval session))
              ((not (plist-get entry :hidden))))
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
               (mevedel-session-pending-steering session))
      (user-error "Resolve pending steering before implementing the plan"))
    (when (and (plist-get entry :directive)
               (proper-list-p outcome)
               (plist-get outcome :accept)
               (cl-some
                (lambda (queued)
                  (eq (plist-get (plist-get queued :scope) :action) 'plan))
                (mevedel-session-pending-follow-ups session)))
      (user-error "Resolve pending planning follow-ups before implementing"))
    (if (not (eq entry pending))
        (display-warning 'mevedel
                         "Plan approval: stale settlement ignored" :warning)
      (setf (mevedel-session-pending-plan-approval session) nil)
      (unless (mevedel-plan-approval--deliver entry outcome "settle" t)
        (unless (mevedel-session-pending-plan-approval session)
          (setf (mevedel-session-pending-plan-approval session) entry))
        (when (eq entry (mevedel-session-pending-plan-approval session))
          (mevedel-plan-approval-render session))))))

(defun mevedel-plan-approval-abort (&optional session outcome)
  "Settle SESSION's pending Plan approval with OUTCOME or `aborted'."
  (when-let* ((session (or session (mevedel-plan-approval--current-session)))
              (entry (mevedel-session-pending-plan-approval session)))
    (setf (mevedel-session-pending-plan-approval session) nil)
    (mevedel-plan-approval--deliver entry (or outcome 'aborted) "abort")))


(provide 'mevedel-plan-mode)
;;; mevedel-plan-mode.el ends here
