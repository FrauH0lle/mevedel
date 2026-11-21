;;; niwa-plan-mode.el -- Plan conversation and proposal UI -*- lexical-binding: t -*-

;;; Commentary:

;; Sticky Plan conversations and proposal approval interaction.

;;; Code:

(require 'niwa-utilities)

(require 'cl-lib)
(require 'subr-x)
(eval-when-compile (require 'niwa-structs))
(require 'niwa-plan)

;; `niwa-interaction-prompt'
(declare-function niwa--prompt-announce "niwa-interaction-prompt"
                  (overlay))
(declare-function niwa--prompt--settle "niwa-interaction-prompt"
		  (overlay outcome))
(declare-function niwa--prompt-framed-body
                  "niwa-interaction-prompt" (content face))
(declare-function niwa--prompt-key
                  "niwa-interaction-prompt" (key))
(autoload 'niwa--prompt-announce "niwa-interaction-prompt")
(autoload 'niwa--prompt--settle "niwa-interaction-prompt")
(autoload 'niwa--prompt-framed-body "niwa-interaction-prompt")
(autoload 'niwa--prompt-key "niwa-interaction-prompt")

;; `niwa-menu'
(declare-function niwa-menu-open-model-selection "niwa-menu"
                  (&rest options))
(autoload 'niwa-menu-open-model-selection "niwa-menu")

;; `niwa-models'
(declare-function niwa-model--provider-label
                  "niwa-models" (provider))
(declare-function niwa-model-resolve-provider
                  "niwa-models" (spec &optional noerror))
(declare-function niwa-model-resolve-workload
                  "niwa-models"
                  (workload &optional explicit-selector explicit-effort))
(autoload 'niwa-model-resolve-provider "niwa-models")
(autoload 'niwa-model-resolve-workload "niwa-models")

;; `niwa-pending-inputs'
(declare-function niwa-view-enqueue-external-follow-up
                  "niwa-pending-inputs" (data-buffer text &rest keys))
(autoload 'niwa-view-enqueue-external-follow-up "niwa-pending-inputs")

;; `niwa-plan'
(declare-function niwa-plan-accept "niwa-plan"
		  (plan-markdown session buffer &optional skip-verification
				 current-relative-path accepted-relative-path))
(declare-function niwa-plan-current-body "niwa-plan"
		  (&optional session))
(declare-function niwa-plan-hash "niwa-plan" (plan-markdown))
(declare-function niwa-plan-resource-address "niwa-plan"
		  (relative-path))
(defvar niwa-plan--relative-current-path)

;; `niwa-plan-handoff'
(declare-function niwa-plan-handoff-assert-worktree-ready
                  "niwa-plan-handoff" (session buffer))
(declare-function niwa-plan-handoff-selection-valid-p
                  "niwa-plan-handoff" (selection))
(declare-function niwa-plan-handoff-start "niwa-plan-handoff"
                  (session chat-buffer selection accepted))
(defvar niwa-plan-handoff-implementation-modes)
(autoload 'niwa-plan-handoff-assert-worktree-ready "niwa-plan-handoff")
(autoload 'niwa-plan-handoff-selection-valid-p "niwa-plan-handoff")
(autoload 'niwa-plan-handoff-start "niwa-plan-handoff")

;; `niwa-queue'
(declare-function niwa-queue--current-session "niwa-queue" ())
(declare-function niwa-queue--entry-metadata-put
                  "niwa-queue" (entry key value))
(declare-function niwa-queue--unregister-entry-interaction
                  "niwa-queue" (entry))
(autoload 'niwa-queue--current-session "niwa-queue")
(autoload 'niwa-queue--entry-metadata-put "niwa-queue")
(autoload 'niwa-queue--unregister-entry-interaction "niwa-queue")

;; `niwa-reminders'
(declare-function niwa-reminders-rearm-plan-mode "niwa-reminders" (session))
(autoload 'niwa-reminders-rearm-plan-mode "niwa-reminders")

;; `niwa-session-artifacts'
(declare-function niwa-session-artifacts-assert-new-mutation-authority
                  "niwa-session-artifacts" (session))
(autoload 'niwa-session-artifacts-assert-new-mutation-authority
  "niwa-session-artifacts")

;; `niwa-skills-core'
(declare-function niwa-skill-name "niwa-skills-core" (cl-x) t)
(declare-function niwa-skill-source-file "niwa-skills-core" (cl-x) t)
(autoload 'niwa-skill-name "niwa-skills-core")
(autoload 'niwa-skill-source-file "niwa-skills-core")

;; `niwa-skills-input'
(declare-function niwa-skills-input-prepare-user-input
                  "niwa-skills-input" (text session))
(autoload 'niwa-skills-input-prepare-user-input "niwa-skills-input")

;; `niwa-skills-ui'
(declare-function niwa-view-refresh-associated-input-prompt
                  "niwa-view-composer" ())
(declare-function niwa-skills-user-visible-skills
                  "niwa-skills-ui" (session &optional inline-only))
(autoload 'niwa-skills-user-visible-skills "niwa-skills-ui")

;; `niwa-structs'
(declare-function niwa-goal-status "niwa-structs" (cl-x) t)
(declare-function niwa-session-goal "niwa-structs" (cl-x) t)
(declare-function niwa-session-pending-follow-ups
                  "niwa-structs" (cl-x) t)
(declare-function niwa-session-pending-plan-approval
                  "niwa-structs" (cl-x) t)
(declare-function niwa-session-pending-steering
                  "niwa-structs" (cl-x) t)
(declare-function niwa-session-permission-mode "niwa-structs"
		  (cl-x) t)
(declare-function niwa-session-plan-metadata "niwa-structs"
		  (cl-x) t)
(declare-function niwa-session-plan-mode "niwa-structs" (cl-x) t)
(declare-function niwa-session-working-directory "niwa-structs"
		  (cl-x) t)
(defvar niwa--data-buffer)
(defvar niwa--session)
(defvar niwa-goal-token-budget)

;; `niwa-transcript'
(declare-function niwa-transcript-segments "niwa-transcript"
		  (start end))
(autoload 'niwa-transcript-segments "niwa-transcript")

;; `niwa-utilities'
(declare-function niwa--normalize-message-text "niwa-utilities"
		  (text))
(declare-function niwa--warn-once
                  "niwa-utilities" (scope key format &rest args))

;; `niwa-view-composer'
(declare-function niwa-view--clear-input "niwa-view-composer"
		  nil)
(declare-function niwa-view-composer-buffer "niwa-view-composer"
                  (&optional view))
(declare-function niwa-view-composer-draft-start "niwa-view-composer"
                  (&optional view))
(declare-function niwa-view-composer-draft-text "niwa-view-composer"
                  (&optional view without-properties))
(declare-function niwa-view-composer-insert "niwa-view-composer"
                  (text &optional position view))

;; `niwa-view-fontify'
(declare-function niwa-view--fontify-as "niwa-view-fontify"
                  (text mode))

;; `niwa-view-history'
(declare-function niwa-view-history-add "niwa-view-history" (input))
(autoload 'niwa-view-history-add "niwa-view-history")

;; `niwa-view-interaction'
(declare-function niwa-view--interaction-register
		  "niwa-view-interaction" (descriptor))
(declare-function niwa-view--interaction-target-buffer
		  "niwa-view-interaction" (chat-buffer))
(declare-function niwa-view--interaction-unregister
                  "niwa-view-interaction" (id))

;; `niwa-view-zone'
(declare-function niwa-view-zone-width "niwa-view-zone" ())
(autoload 'niwa-view-zone-width "niwa-view-zone")

;; `niwa-worktree'
(declare-function niwa-worktree-collect-status "niwa-worktree"
		  (&optional context))
(declare-function niwa-worktree-default-branch-name
		  "niwa-worktree" (session purpose))
(declare-function niwa-worktree-validate-branch-name
		  "niwa-worktree" (name &optional directory))
(autoload 'niwa-worktree-collect-status "niwa-worktree")
(autoload 'niwa-worktree-default-branch-name "niwa-worktree")
(autoload 'niwa-worktree-validate-branch-name "niwa-worktree")

;;
;;; Plan conversation mode

(defun niwa-plan-mode--current-session (&optional session)
  "Return SESSION or the session reachable from the current buffer."
  (or session
      (and (boundp 'niwa--session) niwa--session)
      (and (boundp 'niwa--data-buffer)
           (buffer-live-p niwa--data-buffer)
           (buffer-local-value 'niwa--session niwa--data-buffer))))

(defun niwa-plan-mode-active-p (&optional session)
  "Return non-nil when SESSION is in a Plan conversation."
  (when-let* ((session (niwa-plan-mode--current-session session)))
    (niwa-session-plan-mode session)))

(defun niwa-plan-mode-enter (&optional session)
  "Enter a sticky Plan conversation for SESSION."
  (interactive)
  (let ((session (niwa-plan-mode--current-session session)))
    (unless session
      (niwa-diagnostic-user-error (niwa-diagnostic-scope session)
                                    "No niwa session for Plan mode"))
    (niwa-session-artifacts-assert-new-mutation-authority session)
    (when-let* ((goal (niwa-session-goal session)))
      (unless (eq (niwa-goal-status goal) 'complete)
        (niwa-diagnostic-user-error (niwa-diagnostic-scope session)
                                      "Finish or clear the current Goal before entering Plan")))
    (unless (niwa-session-plan-mode session)
      (niwa-reminders-rearm-plan-mode session)
      (let ((metadata (copy-sequence
                       (or (niwa-session-plan-metadata session) nil))))
        (cl-remf metadata :selection)
        (setf (niwa-session-plan-metadata session) metadata)))
    (setf (niwa-session-plan-mode session) t)
    (niwa-view-refresh-associated-input-prompt)
    (force-mode-line-update t)
    t))

(defun niwa-plan-mode-exit (&optional session)
  "Leave the Plan conversation for SESSION."
  (interactive)
  (let ((session (niwa-plan-mode--current-session session)))
    (when (and session (niwa-session-plan-mode session))
      (niwa-plan-mode--demote-proposal session t)
      (when (niwa-session-pending-plan-approval session)
        (niwa-plan-approval-abort session 'plan-exit))
      (niwa-plan-mode--deactivate session))
    nil))

;;; Plan proposals

(defun niwa-plan-mode--deactivate (session)
  "Leave Plan in SESSION without changing proposal metadata."
  (setf (niwa-session-plan-mode session) nil)
  (niwa-view-refresh-associated-input-prompt)
  (force-mode-line-update t))

(defun niwa-plan-mode--demote-proposal (session discard-selection)
  "Make SESSION's proposal a draft.
When DISCARD-SELECTION is non-nil, discard its approval selection too."
  (let ((metadata (copy-sequence
                   (or (niwa-session-plan-metadata session) nil))))
    (when (eq (plist-get metadata :status) 'proposed)
      (setq metadata (plist-put metadata :status 'draft)))
    (cl-remf metadata :proposal-id)
    (when discard-selection
      (cl-remf metadata :selection))
    (setf (niwa-session-plan-metadata session) metadata)
    metadata))

(defun niwa-plan-mode--invalidate-proposal (&optional session)
  "Demote and dismiss SESSION's actionable proposal, preserving selection."
  (when-let* ((session (niwa-plan-mode--current-session session))
              ((niwa-session-plan-mode session))
              ((eq (plist-get (niwa-session-plan-metadata session) :status)
                   'proposed)))
    (niwa-plan-mode--demote-proposal session nil)
    (when (niwa-session-pending-plan-approval session)
      (niwa-plan-approval-abort session 'invalidated))
    t))

(defun niwa-plan-mode--assistant-prose (start end)
  "Return root-assistant prose in START..END, excluding tool evidence."
  (niwa--normalize-message-text
   (mapconcat
    (lambda (segment)
      (if (eq (car segment) 'response)
          (buffer-substring-no-properties (cadr segment) (caddr segment))
        ""))
    (niwa-transcript-segments start end)
    "\n")))

(defun niwa-plan-mode--default-selection (session)
  "Return SESSION's initial selection using its implementation workload.
Call in the owning chat buffer, where the session's preset policy lives."
  (let ((policy (niwa-model-resolve-workload 'plan-implementation)))
    (list :location 'here
          :context 'current
          :execution 'direct
          :mode (or (niwa-session-permission-mode session) 'ask)
          :model-provider (niwa-model--provider-label policy)
          :reasoning-effort (plist-get policy :effort)
          :goal-token-budget
          (niwa-plan-mode--effective-goal-budget (current-buffer))
          :skills nil
          :instructions nil)))

(defun niwa-plan-mode--next-mode (mode)
  "Return the Plan implementation mode after MODE."
  (or (cadr (memq mode niwa-plan-handoff-implementation-modes))
      (car niwa-plan-handoff-implementation-modes)))

(defun niwa-plan-mode--next-execution (execution)
  "Return the Plan execution choice after EXECUTION."
  (or (cadr (memq execution '(direct goal buddy))) 'direct))

(defun niwa-plan-mode--next-context (context)
  "Return the implementation context after CONTEXT.
Every context is available at either location: a Worktree implementation
forks the planning session."
  (or (cadr (memq context '(current fresh summary))) 'current))

(defun niwa-plan-mode--next-location (selection)
  "Toggle SELECTION's location, keeping its context."
  (plist-put selection :location
             (if (eq (plist-get selection :location) 'here) 'worktree 'here))
  selection)

(defun niwa-plan-mode--context-description (context)
  "Return the compact UI description for CONTEXT."
  (pcase context
    ('current "full planning transcript")
    ('fresh "setup context + accepted plan")
    ('summary "compact handoff + accepted plan (additional model request)")))

(defun niwa-plan-mode--execution-description (execution)
  "Return the compact UI description for EXECUTION."
  (pcase execution
    ('goal "continue until complete, blocked, paused, or budget-limited")
    ('buddy "Implement myself with notes")
    (_ "one implementation turn")))

(defun niwa-plan-mode--effective-goal-budget (buffer)
  "Return the Goal budget effective in BUFFER, or nil when unbounded."
  (and (boundp 'niwa-goal-token-budget)
       (buffer-local-value 'niwa-goal-token-budget buffer)))


(defun niwa-plan-mode--accept
    (plan-markdown chat-buffer session selection)
  "Accept PLAN-MARKDOWN and dispatch SELECTION from CHAT-BUFFER SESSION."
  (unless (niwa-plan-handoff-selection-valid-p selection)
    (error "Unsupported Plan implementation selection: %S" selection))
  (let* ((artifacts (niwa-plan-accept
                     plan-markdown session chat-buffer t))
         (accepted (plist-get artifacts :accepted)))
    (niwa-plan-mode--deactivate session)
    (niwa-plan-handoff-start
     session chat-buffer selection accepted)))

(defun niwa-plan-mode--feedback-draft (chat-buffer)
  "Insert an editable replacement-plan request for CHAT-BUFFER.
A draft already in the composer is stashed into the input history
before the template replaces it, and the user is told how to recall
it -- the feedback template must not silently destroy unsubmitted
text."
  (let ((target (niwa-view--interaction-target-buffer chat-buffer)))
    (with-current-buffer target
      (let ((draft (string-trim (niwa-view-composer-draft-text target t))))
        (unless (string-empty-p draft)
          (niwa-view-history-add draft)
          (message
           "niwa: composer draft stashed in input history, not deleted; M-p recalls it")))
      (niwa-view--clear-input)
      (let ((start (niwa-view-composer-draft-start target)))
        (niwa-view-composer-insert
         (format
          "Plan feedback:\n\n\n\nRevise the proposal to address this feedback. Emit one complete replacement <proposed_plan> block; the current draft is reference-only.\n\nCurrent plan artifact: %s"
          (niwa-plan-resource-address
           niwa-plan--relative-current-path))
         start target)
        (with-current-buffer (niwa-view-composer-buffer target)
          (goto-char start)
          (forward-line 2))))))

(defun niwa-plan-mode--remote-feedback (chat-buffer text)
  "Queue TEXT as a revision request for CHAT-BUFFER's demoted proposal.
The remote counterpart of the Emacs feedback draft: the same request
template, submitted immediately as a queued follow-up instead of
opening an editable draft."
  (niwa-view-enqueue-external-follow-up
   chat-buffer
   (format
    "Plan feedback:\n\n%s\n\nRevise the proposal to address this feedback. Emit one complete replacement <proposed_plan> block; the current draft is reference-only.\n\nCurrent plan artifact: %s"
    text
    (with-current-buffer chat-buffer
      (niwa-plan-resource-address niwa-plan--relative-current-path)))
   :guest-name (and (boundp 'niwa-collaboration-remote-guest)
                    niwa-collaboration-remote-guest)))

(defun niwa-plan-mode--approval-callback
    (plan-markdown chat-buffer session outcome)
  "Handle Plan proposal OUTCOME for PLAN-MARKDOWN in CHAT-BUFFER SESSION."
  (cond
   ((and (proper-list-p outcome) (plist-get outcome :accept))
    (niwa-plan-mode--accept
     plan-markdown chat-buffer session (plist-get outcome :selection)))
   ((and (proper-list-p outcome) (plist-get outcome :remote-feedback))
    (niwa-plan-mode--demote-proposal session nil)
    (niwa-plan-mode--remote-feedback
     chat-buffer (plist-get outcome :remote-feedback)))
   ((eq outcome 'feedback-draft)
    (niwa-plan-mode--demote-proposal session nil)
    (niwa-plan-mode--feedback-draft chat-buffer))
   ((eq outcome 'aborted)
    (niwa-plan-mode--demote-proposal session t))
   ((memq outcome '(invalidated plan-exit superseded render-error)) nil)
   (t (niwa-diagnostic-message (niwa-diagnostic-scope session)
                                  "Unknown Plan proposal outcome %S" outcome))))

(defun niwa-plan-mode--approval-entry
    (plan-markdown chat-buffer session selection)
  "Return a Plan-mode approval entry for PLAN-MARKDOWN and SELECTION."
  (list :body plan-markdown
        :chat-buffer chat-buffer
        :origin "/root"
        :session session
        :selection selection
        :interaction-id (list :plan-mode (gensym "plan-"))
        :renderer #'niwa-plan-mode--render-approval
        :callback
        (lambda (outcome)
          (niwa-plan-mode--approval-callback
           plan-markdown chat-buffer session outcome))))

(defun niwa-plan-mode--read-worktree-branch (entry)
  "Read and validate a Worktree branch for approval ENTRY."
  (let* ((session (plist-get entry :session))
         (directory (niwa-session-working-directory session))
         (default (niwa-worktree-default-branch-name
                   session "accepted-plan"))
         (branch (read-string "Worktree branch name: " nil nil default)))
    (niwa-worktree-validate-branch-name branch directory)
    branch))

(defun niwa-plan-mode--worktree-dirty-p (entry)
  "Return non-nil when ENTRY's source checkout has uncommitted changes.
The check runs several git processes, about 40 ms, so its result is kept
on ENTRY for every redraw until the selection changes; foreign rebuilds
such as queue events and the control-transfer poll redraw the card too.
Acceptance runs its own check and never relies on this one."
  (car (or (plist-get entry :worktree-dirty)
           (let ((dirty (list (with-current-buffer (plist-get entry :chat-buffer)
                                (and (plist-get (niwa-worktree-collect-status)
                                                :dirty-p)
                                     t)))))
             (plist-put entry :worktree-dirty dirty)
             dirty))))

(defun niwa-plan-mode--worktree-warning (entry)
  "Return ENTRY's Worktree notice when Worktree is selected, or nil.
It names what would refuse acceptance now, from the same check acceptance
runs, and warns that uncommitted source changes stay behind."
  (when (eq (plist-get (plist-get entry :selection) :location) 'worktree)
    (let* ((blocked
            (when-let* ((session (plist-get entry :session)))
              (condition-case err
                  (progn (niwa-plan-handoff-assert-worktree-ready
                          session (plist-get entry :chat-buffer))
                         nil)
                (error (error-message-string err)))))
           (dirty (niwa-plan-mode--worktree-dirty-p entry)))
      (when (or blocked dirty)
        (string-join
         (delq nil
               (list (and blocked (concat blocked "."))
                     (and dirty
                          "Worktree starts at HEAD; uncommitted changes are not included.")))
         "\n")))))

(defvar-local niwa-plan-mode--instructions-entry nil
  "Plan approval entry edited by the current instructions buffer.")

(defun niwa-plan-mode--selection-changed (entry)
  "Persist ENTRY's changed selection and redraw its approval.
An explicit change also refreshes the cached source checkout state."
  (let ((selection (plist-get entry :selection))
        (session (plist-get entry :session)))
    (plist-put entry :worktree-dirty nil)
    (if-let* ((callback (plist-get entry :selection-changed)))
        (funcall callback selection)
      (niwa-plan--metadata-put session :selection selection))
    (niwa-plan-approval-render session)))

(defun niwa-plan-mode--save-instructions ()
  "Save the current Plan implementation instructions and close the editor."
  (interactive)
  (unless niwa-plan-mode--instructions-entry
    (niwa-diagnostic-user-error (niwa-diagnostic-scope (plist-get niwa-plan-mode--instructions-entry :session)) "No Plan implementation instructions to save"))
  (let* ((entry niwa-plan-mode--instructions-entry)
         (session (plist-get entry :session))
         (selection (plist-get entry :selection))
         (instructions (string-trim
                        (buffer-substring-no-properties
                         (point-min) (point-max))))
         (instructions
          (unless (string-empty-p instructions)
            (with-current-buffer (plist-get entry :chat-buffer)
              (niwa-skills-input-prepare-user-input
               instructions session)))))
    (plist-put selection :instructions
               instructions)
    (kill-buffer (current-buffer))
    (niwa-plan-mode--selection-changed entry)))

(defun niwa-plan-mode--cancel-instructions ()
  "Close the current Plan instructions editor without saving."
  (interactive)
  (kill-buffer (current-buffer)))

(defun niwa-plan-mode--edit-instructions (entry)
  "Edit implementation instructions for Plan approval ENTRY."
  (let* ((selection (plist-get entry :selection))
         (buffer (generate-new-buffer "*niwa Plan instructions*")))
    (with-current-buffer buffer
      (text-mode)
      (use-local-map (copy-keymap text-mode-map))
      (local-set-key (kbd "C-c C-c")
                     #'niwa-plan-mode--save-instructions)
      (local-set-key (kbd "C-c C-k")
                     #'niwa-plan-mode--cancel-instructions)
      (setq-local niwa-plan-mode--instructions-entry entry)
      (insert (or (plist-get selection :instructions) ""))
      (goto-char (point-max)))
    (pop-to-buffer buffer)))

(defun niwa-plan-mode--toggle-skill (entry)
  "Toggle one user-invocable implementation skill for approval ENTRY."
  (let* ((selection (plist-get entry :selection))
         (skills
          (niwa-skills-user-visible-skills
           (plist-get entry :session)))
         (candidates
          (mapcar
           (lambda (skill)
             (cons (niwa-skill-name skill) skill))
           skills)))
    (unless candidates
      (niwa-diagnostic-user-error (niwa-diagnostic-scope (plist-get entry :session)) "No user-invocable skills available"))
    (let* ((choice (completing-read "Toggle implementation skill: "
                                    candidates nil t))
           (skill (cdr (assoc choice candidates)))
           (source-file (niwa-skill-source-file skill))
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
                 (list (list :name (niwa-skill-name skill)
                             :source-file source-file)))))
      (niwa-plan-mode--selection-changed entry))))

(defun niwa-plan-mode--skills-label (selection)
  "Return the compact selected-skills label for SELECTION."
  (if-let* ((skills (plist-get selection :skills)))
      (mapconcat (lambda (skill) (plist-get skill :name)) skills ", ")
    "None"))

(defun niwa-plan-mode--instructions-label (selection)
  "Return the compact instructions label for SELECTION."
  (if-let* ((instructions (plist-get selection :instructions)))
      (format "%d line%s"
              (length (string-lines instructions))
              (if (string-match-p "\n" instructions) "s" ""))
    "None"))

(defun niwa-plan-mode--fit-row (row)
  "Return ROW cut to one screen row of the current view.
A cut row ends in an ellipsis and keeps its full text in the help echo.
The last column is left for a continuation glyph, which a terminal
window draws there."
  (let ((width (1- (niwa-view-zone-width))))
    (if (<= (string-width row) width)
        row
      (let ((cut (truncate-string-to-width row width nil nil "…")))
        (add-text-properties 0 (length cut)
                             (list 'help-echo (substring-no-properties row))
                             cut)
        cut))))

(defun niwa-plan-mode--remote-body (entry warning)
  "Return read-only Markdown for approval ENTRY, including WARNING.
Unlike the compact native card this discloses all implementation instructions."
  (let* ((selection (plist-get entry :selection))
         (execution (plist-get selection :execution))
         (worktree (eq (plist-get selection :location) 'worktree)))
    (concat
     (plist-get entry :body)
     (format
      (concat "\n\n## Implementation\n\n"
              "- **Location:** %s\n- **Context:** %s — %s\n"
              "- **Execution:** %s — %s\n- **Budget:** %s\n")
      (capitalize (symbol-name (plist-get selection :location)))
      (capitalize (symbol-name (plist-get selection :context)))
      (niwa-plan-mode--context-description (plist-get selection :context))
      (capitalize (symbol-name execution))
      (niwa-plan-mode--execution-description execution)
      (if (eq execution 'goal)
          (if-let* ((budget (plist-get selection :goal-token-budget)))
              (format "%d tokens" budget) "Unlimited")
        "Goal execution only"))
     (format "- **Mode:** %s\n- **Model:** %s · effort %s\n- **Skills:** %s\n"
             (capitalize (symbol-name (plist-get selection :mode)))
             (plist-get selection :model-provider)
             (or (plist-get selection :reasoning-effort) "default")
             (niwa-plan-mode--skills-label selection))
     "\n### Instructions\n\n"
     (or (plist-get selection :instructions) "None")
     "\n\nSettings are read-only here; change them in Emacs.\n"
     (when worktree
       "\nWorktree acceptance requires an owner link. Full-link guests can send feedback but cannot accept this Worktree plan.\n")
     (when warning (concat "\n" warning "\n")))))

(defun niwa-plan-mode--render-approval (entry)
  "Render standalone Plan approval ENTRY in the interaction zone."
  (let ((chat-buffer (plist-get entry :chat-buffer))
        (selection (plist-get entry :selection))
        overlay)
    (cl-labels
        ((deliver (outcome)
           (niwa-plan-approval-settle entry outcome))
         (settle (outcome)
           (when overlay (niwa--prompt--settle overlay outcome)))
         (accept (&optional branch remote-p)
           (interactive)
           (if remote-p
               (accept-selection branch remote-p)
             (niwa-with-diagnostic-errors
                 (niwa-diagnostic-scope (plist-get entry :session))
               (accept-selection branch remote-p))))
         (accept-selection (branch remote-p)
           (let ((accepted (copy-tree selection))
                 (provider (plist-get selection :model-provider)))
             (when (eq (plist-get accepted :location) 'worktree)
               (niwa-plan-handoff-assert-worktree-ready
                (plist-get entry :session) chat-buffer)
               (plist-put accepted :branch
                          (if remote-p
                              (progn
                                (niwa-worktree-validate-branch-name
                                 branch (niwa-session-working-directory
                                         (plist-get entry :session)))
                                branch)
                            (niwa-plan-mode--read-worktree-branch entry))))
             (unless (string-match-p "\\`[^:]+:.+\\'" provider)
               (user-error "Select a registered model before implementing"))
             (unless (niwa-model-resolve-provider provider t)
               (user-error "Select a registered model before implementing"))
             (settle (list :accept t :selection accepted))))
         (cycle-mode ()
           (interactive)
           (plist-put selection :mode
                      (niwa-plan-mode--next-mode
                       (plist-get selection :mode)))
           (niwa-plan-mode--selection-changed entry))
         (cycle-context ()
           (interactive)
           (plist-put selection :context
                      (niwa-plan-mode--next-context
                       (plist-get selection :context)))
           (niwa-plan-mode--selection-changed entry))
         (cycle-execution ()
           (interactive)
           (plist-put selection :execution
                      (niwa-plan-mode--next-execution
                       (plist-get selection :execution)))
           (niwa-plan-mode--selection-changed entry))
         (cycle-location ()
           (interactive)
           (niwa-plan-mode--next-location selection)
           (niwa-plan-mode--selection-changed entry))
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
                     (niwa-diagnostic-user-error
                      (niwa-diagnostic-scope (plist-get entry :session))
                      "Goal token budget must be a positive integer or empty")))))
             (plist-put selection :goal-token-budget budget)
             (niwa-plan-mode--selection-changed entry)))
         (open-model ()
           (interactive)
           (niwa-menu-open-model-selection
            :title "Implementation model"
            :provider (plist-get selection :model-provider)
            :effort (plist-get selection :reasoning-effort)
            :update
            (lambda (provider effort)
              (plist-put selection :model-provider provider)
              (plist-put selection :reasoning-effort effort)
              (niwa-plan-mode--selection-changed entry))))
         (toggle-skill ()
           (interactive)
           (niwa-plan-mode--toggle-skill entry))
         (edit-instructions ()
           (interactive)
           (niwa-plan-mode--edit-instructions entry))
         (feedback () (interactive) (settle 'feedback-draft))
         (hide ()
           (interactive)
           (plist-put entry :hidden t)
           (niwa-view--interaction-unregister
            (plist-get entry :interaction-id))
           (when-let* ((position
                        (text-property-any
                         (point-min) (point-max)
                         'niwa-view-pending-plan t)))
             (goto-char position)))
         (cancel () (interactive) (settle 'aborted)))
      (let ((target (niwa-view--interaction-target-buffer chat-buffer)))
        (niwa-queue--entry-metadata-put
         entry :interaction-id (plist-get entry :interaction-id))
        (niwa-queue--entry-metadata-put entry :view-buffer target)
        (with-current-buffer target
          (let* ((keymap (make-sparse-keymap))
                 (warning (niwa-plan-mode--worktree-warning entry))
                 (execution (plist-get selection :execution))
                 (budget (plist-get selection :goal-token-budget))
                 ;; Native rows keep a stable one-screen-row geometry.
                 ;; The remote body discloses settings without keyboard hints.
                 (rows
                  (append
                   (list
                    (concat
                     (niwa--prompt-key "l")
                     "  Location    "
                     (propertize
                      (capitalize
                       (symbol-name (plist-get selection :location)))
                      'font-lock-face 'bold))
                    (concat
                     (niwa--prompt-key "c")
                     "  Context     "
                     (propertize
                      (capitalize
                       (symbol-name (plist-get selection :context)))
                      'font-lock-face 'bold)
                     " — "
                     (niwa-plan-mode--context-description
                      (plist-get selection :context)))
                    (concat
                     (niwa--prompt-key "e")
                     "  Execution   "
                     (propertize
                      (capitalize (symbol-name execution))
                      'font-lock-face 'bold)
                     " — "
                     (niwa-plan-mode--execution-description execution))
                    ;; The row stays when Execution is not Goal.
                    (if (eq execution 'goal)
                        (concat
                         (niwa--prompt-key "b")
                         "  Budget      "
                         (propertize
                          (if budget
                              (format "%d tokens" budget)
                            "Unlimited")
                          'font-lock-face 'bold))
                      (propertize "   Budget      Goal execution only"
                                  'font-lock-face 'shadow)))
                   (list
                    (concat
                     (niwa--prompt-key "m")
                     "  Mode        "
                     (propertize
                      (capitalize
                       (symbol-name (plist-get selection :mode)))
                      'font-lock-face 'bold))
                    (concat
                     (niwa--prompt-key "M")
                     "  Model       "
                     (propertize
                      (format
                       "%s · effort %s"
                       (plist-get selection :model-provider)
                       (or (plist-get selection :reasoning-effort)
                           "default"))
                      'font-lock-face 'bold))
                    (concat
                     (niwa--prompt-key "s")
                     "  Skills      "
                     (propertize
                      (niwa-plan-mode--skills-label selection)
                      'font-lock-face 'bold))
                    (concat
                     (niwa--prompt-key "i")
                     "  Instructions "
                     (propertize
                      (niwa-plan-mode--instructions-label selection)
                      'font-lock-face 'bold))
                    ;; The blank row above the actions holds the notice.
                    (if warning
                        (propertize
                         (replace-regexp-in-string "\n" " " warning)
                         'font-lock-face 'warning)
                      ""))))
                 (plan-text
                  (if (fboundp 'niwa-view--fontify-as)
                      (niwa-view--fontify-as
                       (plist-get entry :body) 'markdown-mode)
                    (plist-get entry :body)))
                 (frame
                  (lambda (fit)
                    (niwa--prompt-framed-body
                     (concat
                      plan-text
                      "\n\n"
                      (propertize "Implementation"
                                  'font-lock-face 'niwa-view-plan-mode)
                      "\n\n"
                      ;; Every setting and the notice slot keep one screen
                      ;; row locally, so changing a setting or Location
                      ;; never changes the card's height.
                      (mapconcat (lambda (row) (concat (funcall fit row) "\n"))
                                 rows "")
                      (niwa--prompt-key "RET")
                      " implement    "
                      (niwa--prompt-key "f")
                      " feedback    "
                      (niwa--prompt-key "q") " hide    "
                      (niwa--prompt-key "C-g")
                      " cancel\n")
                     'niwa-view-plan-mode)))
                 (body (funcall frame #'niwa-plan-mode--fit-row))
                 (remote-body (niwa-plan-mode--remote-body entry warning))
                 ;; Full disclosure is a condition of browser acceptance.
                 ;; Include JSON escaping and UTF-8 in the sealed-frame budget.
                 (remote-oversized
                  (> (string-bytes (json-serialize (vector remote-body))) 57000)))
            (define-key keymap (kbd "RET") #'accept)
            (define-key keymap (kbd "<return>") #'accept)
            (define-key keymap (kbd "C-c C-c") #'accept)
            (define-key keymap (kbd "TAB") #'cycle-mode)
            (define-key keymap (kbd "<tab>") #'cycle-mode)
            (define-key keymap (kbd "m") #'cycle-mode)
            (define-key keymap (kbd "M") #'open-model)
            (define-key keymap (kbd "s") #'toggle-skill)
            (define-key keymap (kbd "i") #'edit-instructions)
            (define-key keymap (kbd "e") #'cycle-execution)
            (define-key keymap (kbd "l") #'cycle-location)
            (define-key keymap (kbd "c") #'cycle-context)
            (when (eq execution 'goal)
              (define-key keymap (kbd "b") #'edit-budget))
            (define-key keymap (kbd "f") #'feedback)
            (define-key keymap (kbd "q") #'hide)
            (define-key keymap (kbd "C-g") #'cancel)
            (setq overlay
                  (niwa-view--interaction-register
                   (list :kind 'plan
                         :id (plist-get entry :interaction-id)
                         :count 1
                         :body body
                         :keymap keymap
                         :entry entry
                         :activate #'deliver)))
            (overlay-put overlay 'niwa-plan t)
            (overlay-put overlay 'niwa-user-request t)
            (overlay-put overlay 'niwa--callback #'deliver)
            (overlay-put overlay 'keymap keymap)
            ;; Remote acceptance uses the host-configured axes verbatim.
            ;; Worktree accepts a branch input only from owner links; the
            ;; dispatcher enforces that authority before invoking the closure.
            (overlay-put overlay 'niwa--remote
                         (append
                          (list :body
                                (if remote-oversized
                                    (concat "## Full review in Emacs\n\n"
                                            "The complete plan and implementation instructions exceed the browser card limit. "
                                            "Nothing has been approved. Review and accept the full plan in Emacs; "
                                            "you can still send feedback here.")
                                  (substring-no-properties remote-body))
                                :body-kind "markdown"
                                :options
                                (unless remote-oversized
                                  (list (cons (lambda (&optional branch)
                                                (accept branch t))
                                              "Accept plan")))
                                :feedback
                                (lambda (text)
                                  (settle (list :remote-feedback text))))
                          (when (and (not remote-oversized)
                                     (eq (plist-get selection :location) 'worktree))
                            (list :options-owner-only t
                                  :option-input
                                  (list :label "Worktree branch"
                                        :value
                                        (niwa-worktree-default-branch-name
                                         (plist-get entry :session)
                                         "accepted-plan"))))))
            (niwa--prompt-announce overlay)))))))

(defun niwa-plan-mode--post-response (start end)
  "Present a complete root-assistant proposal from START..END once.
Warn when a standalone delimiter is present but no nonblank plan is recognized."
  (when-let* ((session (niwa-plan-mode--current-session))
              ((niwa-session-plan-mode session)))
    (let* ((text (niwa-plan-mode--assistant-prose start end))
           (plan (niwa-plan-extract-proposed text))
           (case-fold-search nil))
      (if (and plan (not (string-blank-p plan)))
          (let* ((proposal-id (list start end (niwa-plan-hash plan)))
                 (metadata (niwa-session-plan-metadata session)))
            (unless (equal proposal-id (plist-get metadata :proposal-id))
              (let ((selection
                     (or (plist-get metadata :selection)
                         (niwa-plan-mode--default-selection session))))
                (niwa-plan-write-current plan session (current-buffer))
                (niwa-plan--metadata-put session :status 'proposed)
                (niwa-plan--metadata-put session :proposal-id proposal-id)
                (niwa-plan--metadata-put session :selection selection)
                (niwa-plan-approval-present
                 (niwa-plan-mode--approval-entry
                  plan (current-buffer) session selection)
                 session))))
        ;; One standalone delimiter signals an attempted block.
        ;; An inline mention alone is not enough to diagnose a proposal.
        (when (string-match-p "^[ \t]*</?proposed_plan>[ \t]*$" text)
          (niwa-diagnostic-warning (niwa-diagnostic-scope session)
           'niwa
           (concat "Plan approval unavailable: no complete, nonblank proposal "
                   "was recognized. Ask the assistant to resend the full plan "
                   "with <proposed_plan> and </proposed_plan> each on its own "
                   "line, without indentation. Plan mode remains active.")
           :warning))))))

(defun niwa-plan-mode-restore-pending-approval
    (&optional session chat-buffer)
  "Restore SESSION's genuine pending Plan proposal in CHAT-BUFFER."
  (let* ((session (niwa-plan-mode--current-session session))
         (chat-buffer (or chat-buffer (current-buffer)))
         (metadata (and session (niwa-session-plan-metadata session)))
         (proposal-id (plist-get metadata :proposal-id))
         (selection (plist-get metadata :selection))
         (hash (plist-get metadata :hash))
         (plan (and session
                    (ignore-errors (niwa-plan-current-body session))))
         (valid
          (and session
               (niwa-session-plan-mode session)
               (eq (plist-get metadata :status) 'proposed)
               (proper-list-p proposal-id)
               (= (length proposal-id) 3)
               (integerp (nth 0 proposal-id))
               (integerp (nth 1 proposal-id))
               (stringp (nth 2 proposal-id))
               (equal hash (nth 2 proposal-id))
               (niwa-plan-handoff-selection-valid-p selection)
               (stringp (plist-get selection :model-provider))
               (plist-member selection :reasoning-effort)
               (stringp plan)
               (equal hash (niwa-plan-hash plan)))))
    (cond
     ((and valid (not (niwa-session-pending-plan-approval session)))
      (niwa-plan-approval-present
       (niwa-plan-mode--approval-entry
        plan chat-buffer session selection)
       session)
      t)
     ((and session
           (niwa-session-plan-mode session)
           (eq (plist-get metadata :status) 'proposed)
           (not valid))
      (niwa-plan-mode--demote-proposal session t)
      nil))))

;;
;;; Shared single Plan approval interaction

(defun niwa-plan-approval--deliver (entry outcome phase &optional retain)
  "Deliver OUTCOME to ENTRY during PHASE.
When RETAIN is non-nil, keep ENTRY's interaction after a callback error."
  (condition-case err
      (progn
        (when-let* ((callback (plist-get entry :callback)))
          (funcall callback outcome))
        (niwa-queue--unregister-entry-interaction entry)
        t)
    (error
     (niwa-diagnostic-warning
      (niwa-diagnostic-scope (plist-get entry :session)) 'niwa
                      (format "%s (Plan approval %s callback error)"
                              (error-message-string err) phase)
                      :warning)
     (unless retain (niwa-queue--unregister-entry-interaction entry))
     nil)))

(defun niwa-plan-approval-present (entry &optional session)
  "Replace SESSION's pending Plan approval with ENTRY and render it."
  ;; The single installer for a pending approval: its renderer and every
  ;; later settlement reach `niwa-queue\=' helpers.  Session resume
  ;; presents an approval before any other interaction has loaded them.
  (let ((session (or session (niwa-queue--current-session))))
    (if (not session)
        (niwa-plan-approval--deliver entry 'aborted "no-session")
      (setq entry (plist-put (copy-sequence entry) :session session))
      (when-let* ((previous (niwa-session-pending-plan-approval session)))
        (setf (niwa-session-pending-plan-approval session) nil)
        (niwa-plan-approval--deliver previous 'superseded "supersede"))
      (setf (niwa-session-pending-plan-approval session) entry)
      (niwa-plan-approval-render session))))

(defun niwa-plan-approval-render (&optional session)
  "Render SESSION's pending Plan approval."
  (when-let* ((session (or session (niwa-queue--current-session)))
              (entry (niwa-session-pending-plan-approval session))
              ((not (plist-get entry :hidden))))
    (condition-case err
        (if-let* ((renderer (plist-get entry :renderer)))
            (funcall renderer entry)
          (error "Plan approval has no renderer"))
      (error
       (niwa-diagnostic-warning (niwa-diagnostic-scope session) 'niwa
                        (format "%s (Plan approval render error)"
                                (error-message-string err))
                        :warning)
       ;; A rendering bug must not discard the proposal: settling it as
       ;; aborted would drop the selection a later resume needs.
       (niwa-plan-approval-abort session 'render-error)))))

(defun niwa-plan-approval-settle (entry outcome)
  "Settle pending Plan approval ENTRY with OUTCOME."
  (let* ((session (plist-get entry :session))
         (pending (and session
                       (niwa-session-pending-plan-approval session))))
    (when (and (proper-list-p outcome)
               (plist-get outcome :accept)
               (niwa-session-pending-steering session))
      (user-error "Resolve pending steering before implementing the plan"))
    (if (not (eq entry pending))
        (niwa--warn-once (niwa-diagnostic-scope session) 'plan-approval-stale
                            "Plan approval: stale settlement ignored")
      (setf (niwa-session-pending-plan-approval session) nil)
      (unless (niwa-plan-approval--deliver entry outcome "settle" t)
        (unless (niwa-session-pending-plan-approval session)
          (setf (niwa-session-pending-plan-approval session) entry))
        (when (eq entry (niwa-session-pending-plan-approval session))
          (niwa-plan-approval-render session))))))

(defun niwa-plan-approval-abort (&optional session outcome)
  "Settle SESSION's pending Plan approval with OUTCOME or `aborted'."
  (when-let* ((session (or session (niwa-queue--current-session)))
              (entry (niwa-session-pending-plan-approval session)))
    (setf (niwa-session-pending-plan-approval session) nil)
    (niwa-plan-approval--deliver entry (or outcome 'aborted) "abort")))


(provide 'niwa-plan-mode)
;;; niwa-plan-mode.el ends here
