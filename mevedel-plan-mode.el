;;; mevedel-plan-mode.el -- Plan conversation and proposal UI -*- lexical-binding: t -*-

;;; Commentary:

;; Sticky Plan conversations and proposal approval interaction.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'mevedel-structs))

;; `mevedel-interaction-prompt'
(declare-function mevedel--prompt--settle "mevedel-interaction-prompt"
		  (overlay outcome))

;; `mevedel-plan'
(declare-function mevedel-plan-accept "mevedel-plan"
		  (plan-markdown session buffer &optional
				 skip-verification
				 current-relative-path
				 accepted-relative-path))
(declare-function mevedel-plan-current-body "mevedel-plan"
		  (&optional session))
(declare-function mevedel-plan-hash "mevedel-plan" (plan-markdown))

;; `mevedel-plan-handoff'
(declare-function mevedel-plan-handoff-selection-valid-p
                  "mevedel-plan-handoff" (selection))
(declare-function mevedel-plan-handoff-start "mevedel-plan-handoff"
                  (session chat-buffer selection accepted goal-token-budget))
(defvar mevedel-plan-handoff-implementation-modes)

;; `mevedel-queue'
(declare-function mevedel-queue--current-session "mevedel-queue" ())
(declare-function mevedel-queue--unregister-entry-interaction
                  "mevedel-queue" (entry))

;; `mevedel-skills-ui'
(declare-function mevedel-skills--refresh-view-input-prompt
		  "mevedel-skills-ui" nil)

;; `mevedel-structs'
(declare-function mevedel-goal-status "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-goal "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-pending-plan-approval
		  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-permission-mode "mevedel-structs"
		  (cl-x) t)
(declare-function mevedel-session-plan-metadata "mevedel-structs"
		  (cl-x) t)
(declare-function mevedel-session-plan-mode "mevedel-structs" (cl-x) t)
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
(declare-function mevedel-view--clear-input "mevedel-view-composer"
		  nil)
(declare-function mevedel-view--input-start "mevedel-view-composer"
		  nil)

;; `mevedel-view-interaction'
(declare-function mevedel-view--interaction-register
		  "mevedel-view-interaction" (descriptor))
(declare-function mevedel-view--interaction-target-buffer
		  "mevedel-view-interaction" (chat-buffer))

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
        :mode (or (mevedel-session-permission-mode session) 'ask)))

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
    ('current "keep the complete planning transcript")
    ('fresh "start with setup context and the accepted plan")
    ('summary "generate a compact handoff, then start with it and the accepted plan (additional model request)")))

(defun mevedel-plan-mode--execution-description (execution)
  "Return the compact UI description for EXECUTION."
  (if (eq execution 'goal)
      "continue automatically until complete, genuinely blocked, paused, or budget-limited"
    "one ordinary implementation turn"))

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
     session chat-buffer selection accepted
     (mevedel-plan-mode--effective-goal-budget chat-buffer))))

(defun mevedel-plan-mode--feedback-draft (chat-buffer session)
  "Insert an editable replacement-plan request for CHAT-BUFFER SESSION."
  (let ((target (mevedel-view--interaction-target-buffer chat-buffer))
        (path (plist-get (mevedel-session-plan-metadata session) :path)))
    (with-current-buffer target
      (mevedel-view--clear-input)
      (goto-char (mevedel-view--input-start))
      (let ((start (point)))
        (insert
         (format
          "Plan feedback:\n\n\n\nRevise the proposal to address this feedback. Emit one complete replacement <proposed_plan> block; the current draft is reference-only.\n\nCurrent plan artifact: %s"
          (or path "plans/current.md")))
        (goto-char start)
        (forward-line 2)))))

(defun mevedel-plan-mode--approval-callback
    (plan-markdown chat-buffer session outcome)
  "Handle Plan proposal OUTCOME for PLAN-MARKDOWN in CHAT-BUFFER SESSION."
  (cond
   ((and (proper-list-p outcome) (plist-get outcome :accept))
    (mevedel-plan-mode--accept
     plan-markdown chat-buffer session (plist-get outcome :selection)))
   ((eq outcome 'feedback-draft)
    (mevedel-plan-mode--demote-proposal session nil)
    (mevedel-plan-mode--feedback-draft chat-buffer session))
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

(defun mevedel-plan-mode--render-approval (entry)
  "Render standalone Plan approval ENTRY in the interaction zone."
  (require 'mevedel-interaction-prompt)
  (let ((chat-buffer (plist-get entry :chat-buffer))
        (selection (plist-get entry :selection))
        overlay)
    (cl-labels
        ((settle (outcome)
           (when overlay (mevedel--prompt--settle overlay outcome)))
         (accept ()
           (interactive)
           (let ((accepted (copy-sequence selection)))
             (when (eq (plist-get accepted :location) 'worktree)
               (plist-put accepted :branch
                          (mevedel-plan-mode--read-worktree-branch entry)))
             (settle (list :accept t :selection accepted))))
         (cycle-mode ()
           (interactive)
           (plist-put selection :mode
                      (mevedel-plan-mode--next-mode
                       (plist-get selection :mode)))
           (mevedel-plan--metadata-put
            (plist-get entry :session) :selection selection)
           (mevedel-plan-approval-render (plist-get entry :session)))
         (cycle-context ()
           (interactive)
           (plist-put selection :context
                      (mevedel-plan-mode--next-context
                       (plist-get selection :location)
                       (plist-get selection :context)))
           (mevedel-plan--metadata-put
            (plist-get entry :session) :selection selection)
           (mevedel-plan-approval-render (plist-get entry :session)))
         (cycle-execution ()
           (interactive)
           (plist-put selection :execution
                      (mevedel-plan-mode--next-execution
                       (plist-get selection :execution)))
           (mevedel-plan--metadata-put
            (plist-get entry :session) :selection selection)
           (mevedel-plan-approval-render (plist-get entry :session)))
         (cycle-location ()
           (interactive)
           (mevedel-plan-mode--next-location selection)
           (mevedel-plan--metadata-put
            (plist-get entry :session) :selection selection)
           (mevedel-plan-approval-render (plist-get entry :session)))
         (feedback () (interactive) (settle 'feedback-draft))
         (cancel () (interactive) (settle 'aborted)))
      (let ((target (mevedel-view--interaction-target-buffer chat-buffer)))
        (with-current-buffer target
          (let* ((keymap (make-sparse-keymap))
                 (warning (mevedel-plan-mode--worktree-warning entry))
                 (execution (plist-get selection :execution))
                 (budget
                  (mevedel-plan-mode--effective-goal-budget chat-buffer))
                 (body
                  (format
                   "\n%s\n\nLocation   %s\nContext    %s — %s\nExecution  %s — %s\n%sMode       %s\n%s\n%s\n"
                   (if (fboundp 'mevedel-view--fontify-as)
                       (mevedel-view--fontify-as
                        (plist-get entry :body) 'markdown-mode)
                     (plist-get entry :body))
                   (capitalize (symbol-name (plist-get selection :location)))
                   (capitalize (symbol-name (plist-get selection :context)))
                   (mevedel-plan-mode--context-description
                    (plist-get selection :context))
                   (capitalize (symbol-name execution))
                   (mevedel-plan-mode--execution-description execution)
                   (if (eq execution 'goal)
                       (format "Goal budget %s\n"
                               (if budget
                                   (format "%d tokens" budget)
                                 "Unlimited"))
                     "")
                   (plist-get selection :mode)
                   (if warning (concat "\n" warning "\n") "")
                   (concat
                    "Keys: RET implement  l location  c context  e execution  TAB/m mode  "
                    "f feedback draft  q cancel"))))
            (define-key keymap (kbd "RET") #'accept)
            (define-key keymap (kbd "<return>") #'accept)
            (define-key keymap (kbd "C-c C-c") #'accept)
            (define-key keymap (kbd "TAB") #'cycle-mode)
            (define-key keymap (kbd "<tab>") #'cycle-mode)
            (define-key keymap (kbd "m") #'cycle-mode)
            (define-key keymap (kbd "e") #'cycle-execution)
            (define-key keymap (kbd "l") #'cycle-location)
            (define-key keymap (kbd "c") #'cycle-context)
            (define-key keymap (kbd "f") #'feedback)
            (define-key keymap (kbd "q") #'cancel)
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
                         :activate
                         (lambda (outcome)
                           (mevedel-plan-approval-settle entry outcome)))))
            (overlay-put overlay 'mevedel-plan t)
            (overlay-put overlay 'mevedel-user-request t)
            (overlay-put overlay 'keymap keymap)))))))

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
      (mevedel-plan-mode--demote-proposal session nil)
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


(provide 'mevedel-plan-mode)
;;; mevedel-plan-mode.el ends here
