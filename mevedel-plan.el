;;; mevedel-plan.el -- Plan data and artifacts -*- lexical-binding: t -*-

;;; Commentary:

;; Lifecycle-neutral plan parsing, artifact persistence, acceptance metadata,
;; and implementation inputs.  Goal workflows choose storage paths without
;; duplicating these operations.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'mevedel-structs))

;; `gptel'
(defvar gptel-prompt-prefix-alist)

;; `mevedel-chat'
(declare-function mevedel--implement-plan "mevedel-chat" (action-plist))
(declare-function mevedel--run-session-start-hooks "mevedel-chat" (source))

;; `mevedel-compact'
(declare-function mevedel--compact-main-target "mevedel-compact" ())
(declare-function mevedel--compact-previous-summary "mevedel-compact" ())
(declare-function mevedel--compact-run "mevedel-compact" (&rest args))

;; `mevedel-goal'
(declare-function mevedel-plan-approval-present
                  "mevedel-goal" (entry &optional session))
(declare-function mevedel-plan-approval-render
                  "mevedel-goal" (&optional session))
(declare-function mevedel-plan-approval-abort
                  "mevedel-goal" (&optional session outcome))
(declare-function mevedel-plan-approval-settle
                  "mevedel-goal" (entry outcome))

;; `mevedel-interaction-prompt'
(declare-function mevedel--prompt--settle
                  "mevedel-interaction-prompt" (overlay outcome))

;; `mevedel-permissions'
(declare-function mevedel-permission-mode-transition
                  "mevedel-permissions" (mode))

;; `mevedel-presets'
(declare-function mevedel-preset-restore-session
                  "mevedel-presets" (session &optional buffer))

;; `mevedel-session-persistence'
(declare-function mevedel-session-persistence-ensure-files
                  "mevedel-session-persistence" (session buffer))
(declare-function mevedel-session-persistence-save
                  "mevedel-session-persistence" (session buffer))
(declare-function mevedel-session-persistence-restore
                  "mevedel-session-persistence" (session-dir))
(declare-function mevedel-session-persistence-start-fresh-segment
                  "mevedel-session-persistence" (session buffer &rest args))

;; `mevedel-skills-ui'
(declare-function mevedel-skills--refresh-view-input-prompt
                  "mevedel-skills-ui" ())

;; `mevedel-structs'
(declare-function mevedel-goal-status "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-goal "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-pending-plan-approval
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-permission-mode "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-plan-metadata "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-plan-mode "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-preset-name "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-preset-settings "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-save-path "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-session-id "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-turn-count "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-working-directory
                  "mevedel-structs" (cl-x) t)
(defvar mevedel--data-buffer)
(defvar mevedel--session)

;; `mevedel-transcript'
(declare-function mevedel-transcript-segments
                  "mevedel-transcript" (start end))

;; `mevedel-utilities'
(declare-function mevedel--normalize-message-text
                  "mevedel-utilities" (text))

;; `mevedel-view-composer'
(declare-function mevedel-view--clear-input "mevedel-view-composer" ())
(declare-function mevedel-view--input-start "mevedel-view-composer" ())
(declare-function mevedel-view--run-prompt-submit-hook
                  "mevedel-view-composer"
                  (input display-text callback
                         &optional blocked-callback prior-context))

;; `mevedel-view-interaction'
(declare-function mevedel-view--interaction-register
                  "mevedel-view-interaction" (descriptor))
(declare-function mevedel-view--interaction-target-buffer
                  "mevedel-view-interaction" (chat-buffer))

;; `mevedel-view-markdown'
(declare-function mevedel-view--fontify-as
                  "mevedel-view-markdown" (text mode))

;; `mevedel-worktree'
(declare-function mevedel-worktree--collect-status
                  "mevedel-worktree" (&optional context))
(declare-function mevedel-worktree--default-branch-name
                  "mevedel-worktree" (session purpose))
(declare-function mevedel-worktree--validate-branch-name
                  "mevedel-worktree" (name &optional directory))
(declare-function mevedel-worktree-create-session
                  "mevedel-worktree" (&optional branch purpose clean))

(defconst mevedel-plan--open-tag "<proposed_plan>"
  "Opening tag for proposed plans.")

(defconst mevedel-plan--close-tag "</proposed_plan>"
  "Closing tag for proposed plans.")

(defconst mevedel-plan--relative-current-path
  (file-name-concat "plans" "current.md")
  "Relative path of the mutable current plan under a session directory.")

(defconst mevedel-plan-mode--implementation-modes '(ask auto full-auto)
  "Permission modes selectable for accepted Plan implementation.")


;;
;;; Plan conversation mode

(defun mevedel-plan--current-session (&optional session)
  "Return SESSION or the session reachable from the current buffer."
  (or session
      (and (boundp 'mevedel--session) mevedel--session)
      (and (boundp 'mevedel--data-buffer)
           (buffer-live-p mevedel--data-buffer)
           (buffer-local-value 'mevedel--session mevedel--data-buffer))))

(defun mevedel-plan-mode-active-p (&optional session)
  "Return non-nil when SESSION is in a Plan conversation."
  (when-let* ((session (mevedel-plan--current-session session)))
    (mevedel-session-plan-mode session)))

(defun mevedel-plan-mode-enter (&optional session)
  "Enter a sticky Plan conversation for SESSION."
  (interactive)
  (let ((session (mevedel-plan--current-session session)))
    (unless session
      (user-error "No mevedel session for Plan mode"))
    (when-let* ((goal (mevedel-session-goal session)))
      (unless (eq (mevedel-goal-status goal) 'complete)
        (user-error "Finish or clear the current Goal before entering Plan")))
    (setf (mevedel-session-plan-mode session) t)
    (when (fboundp 'mevedel-skills--refresh-view-input-prompt)
      (mevedel-skills--refresh-view-input-prompt))
    (force-mode-line-update t)
    t))

(defun mevedel-plan-mode-exit (&optional session)
  "Leave the Plan conversation for SESSION."
  (interactive)
  (let ((session (mevedel-plan--current-session session)))
    (when (and session (mevedel-session-plan-mode session))
      (mevedel-plan-mode--demote-proposal session t)
      (when (mevedel-session-pending-plan-approval session)
        (require 'mevedel-goal)
        (mevedel-plan-approval-abort session 'plan-exit))
      (mevedel-plan-mode--deactivate session))
    nil))

(defun mevedel-plan-validate (plan-markdown)
  "Return normalized PLAN-MARKDOWN or signal when it is invalid."
  (unless (stringp plan-markdown)
    (error "Plan must be a string"))
  (require 'mevedel-utilities)
  (let ((plan-markdown (mevedel--normalize-message-text plan-markdown)))
    (when (string-blank-p plan-markdown)
      (error "Plan must not be blank"))
    plan-markdown))

(defun mevedel-plan-extract-proposed (text)
  "Return the last proposed-plan body found in TEXT, or nil.
Only exact line-oriented `<proposed_plan>' blocks are recognized."
  (let ((case-fold-search nil)
        found)
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (while (re-search-forward
              (concat "^" (regexp-quote mevedel-plan--open-tag)
                      "[ \t]*\n")
              nil t)
        (let ((body-start (point)))
          (when (re-search-forward
                 (concat "^" (regexp-quote mevedel-plan--close-tag)
                         "[ \t]*$")
                 nil t)
            (setq found
                  (string-trim-right
                   (buffer-substring-no-properties
                    body-start (match-beginning 0)))))))
      found)))

(defun mevedel-plan-strip-proposed (text)
  "Return TEXT with proposed-plan blocks removed."
  (let ((case-fold-search nil))
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (while (re-search-forward
              (concat "^" (regexp-quote mevedel-plan--open-tag)
                      "[ \t]*\n")
              nil t)
        (let ((start (match-beginning 0)))
          (if (re-search-forward
               (concat "^" (regexp-quote mevedel-plan--close-tag)
                       "[ \t]*\n?")
               nil t)
              (delete-region start (match-end 0))
            (delete-region start (point-max)))
          (goto-char start)))
      (string-trim (buffer-string)))))

(defun mevedel-plan--metadata-put (session key value)
  "Set KEY to VALUE in SESSION's plan metadata."
  (require 'mevedel-structs)
  (let ((metadata (copy-sequence (or (mevedel-session-plan-metadata session)
                                     nil))))
    (setq metadata (plist-put metadata key value))
    (setf (mevedel-session-plan-metadata session) metadata)
    metadata))


;;
;;; Plan proposals

(defun mevedel-plan-mode--deactivate (session)
  "Leave Plan in SESSION without changing proposal metadata."
  (setf (mevedel-session-plan-mode session) nil)
  (when (fboundp 'mevedel-skills--refresh-view-input-prompt)
    (mevedel-skills--refresh-view-input-prompt))
  (force-mode-line-update t))

(defun mevedel-plan-mode--selection-valid-p (selection)
  "Return non-nil when SELECTION is a supported Phase 1 selection."
  (and (proper-list-p selection)
       (let ((location (plist-get selection :location))
             (context (plist-get selection :context)))
         (and (or (and (eq location 'here)
                       (memq context '(current fresh summary)))
                  (and (eq location 'worktree)
                       (eq context 'fresh)))
              (eq (plist-get selection :execution) 'direct)
              (memq (plist-get selection :mode)
                    mevedel-plan-mode--implementation-modes)))))

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
  (when-let* ((session (mevedel-plan--current-session session))
              ((mevedel-session-plan-mode session))
              ((eq (plist-get (mevedel-session-plan-metadata session) :status)
                   'proposed)))
    (mevedel-plan-mode--demote-proposal session nil)
    (when (mevedel-session-pending-plan-approval session)
      (require 'mevedel-goal)
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
  (or (cadr (memq mode mevedel-plan-mode--implementation-modes))
      (car mevedel-plan-mode--implementation-modes)))

(defun mevedel-plan-mode--next-context (location context)
  "Return the implementation context after CONTEXT at LOCATION."
  (if (eq location 'worktree)
      'fresh
    (or (cadr (memq context '(current fresh summary))) 'current)))

(defun mevedel-plan-mode--next-location (selection)
  "Toggle SELECTION's location while preserving a valid context."
  (if (eq (plist-get selection :location) 'here)
      (progn
        (plist-put selection :location 'worktree)
        (unless (eq (plist-get selection :context) 'fresh)
          (plist-put selection :context 'fresh)))
    (plist-put selection :location 'here))
  selection)

(defun mevedel-plan-mode--context-description (context)
  "Return the compact UI description for CONTEXT."
  (pcase context
    ('current "keep the complete planning transcript")
    ('fresh "start with setup context and the accepted plan")
    ('summary "generate a compact handoff, then start fresh (another model request)")))

(defun mevedel-plan-mode--implementation-prompt
    (accepted-artifact plan-markdown)
  "Return the Direct prompt for ACCEPTED-ARTIFACT and PLAN-MARKDOWN."
  (format
   "Accepted plan artifact: %s\n\nAccepted plan:\n%s\n\nImplementation instructions:\nImplement the accepted plan against the current repository state. Preserve its stated outcomes and acceptance criteria while using repository evidence to choose the safest effective mechanics."
   (plist-get accepted-artifact :absolute-path)
   plan-markdown))

(defun mevedel-plan-mode--persist (session chat-buffer)
  "Persist SESSION from CHAT-BUFFER."
  (require 'mevedel-session-persistence)
  (mevedel-session-persistence-save session chat-buffer))

(defun mevedel-plan-mode--implementation-record (selection accepted)
  "Return retry state for SELECTION and ACCEPTED artifact."
  (list :step (if (eq (plist-get selection :location) 'worktree)
                  'prepare-worktree
                (pcase (plist-get selection :context)
                  ('fresh 'prepare-context)
                  ('summary 'prepare-summary)
                  (_ 'submit)))
        :selection (copy-tree selection)
        :accepted-path (plist-get accepted :path)
        :accepted-absolute-path (plist-get accepted :absolute-path)
        :accepted-hash (plist-get accepted :hash)))

(defun mevedel-plan-mode--accepted-body (record)
  "Return RECORD's validated immutable accepted-plan body."
  (let ((path (plist-get record :accepted-absolute-path))
        (hash (plist-get record :accepted-hash)))
    (unless (and (stringp path) (file-exists-p path) (stringp hash))
      (error "Accepted plan artifact is unavailable"))
    (with-temp-buffer
      (insert-file-contents path)
      (let ((body (mevedel--normalize-message-text (buffer-string))))
        (unless (equal hash (mevedel-plan-hash body))
          (error "Accepted plan artifact hash does not match"))
        body))))

(defun mevedel-plan-mode--worktree-target-buffer (record)
  "Return RECORD's prepared Worktree target buffer."
  (let ((save-path (plist-get record :target-save-path))
        (session-id (plist-get record :target-session-id)))
    (unless (and (stringp save-path) (file-directory-p save-path)
                 (stringp session-id))
      (error "Prepared Worktree session is unavailable"))
    (require 'mevedel-session-persistence)
    (let* ((buffer (mevedel-session-persistence-restore save-path))
           (session (buffer-local-value 'mevedel--session buffer)))
      (unless (equal session-id (mevedel-session-session-id session))
        (error "Prepared Worktree session identity does not match"))
      (unless (file-equal-p (plist-get record :target-directory)
                            (mevedel-session-working-directory session))
        (error "Prepared Worktree directory does not match"))
      buffer)))

(defun mevedel-plan-mode--prepare-worktree (session chat-buffer record)
  "Create RECORD's Worktree target and persist its identity in SESSION."
  (require 'mevedel-worktree)
  (let* ((selection (plist-get record :selection))
         (branch (plist-get selection :branch))
         (result
          (with-current-buffer chat-buffer
            (mevedel-worktree-create-session
             branch "Accepted Plan implementation" nil)))
         (target-buffer (plist-get result :buffer))
         (target-session
          (buffer-local-value 'mevedel--session target-buffer))
         (target-save-path
          (with-current-buffer target-buffer
            (mevedel-session-persistence-ensure-files
             target-session target-buffer)))
         (prepared (copy-tree record)))
    (unless (equal branch (plist-get result :branch))
      (error "Created Worktree branch does not match the accepted branch"))
    (setq prepared (plist-put prepared :step 'prepare-target))
    (setq prepared (plist-put prepared :target-directory
                              (plist-get result :directory)))
    (setq prepared (plist-put prepared :target-save-path target-save-path))
    (setq prepared
          (plist-put prepared :target-session-id
                     (mevedel-session-session-id target-session)))
    (mevedel-plan--metadata-put session :implementation-retry prepared)
    (mevedel-plan-mode--persist session chat-buffer)
    prepared))

(defun mevedel-plan-mode--prepare-worktree-target
    (session chat-buffer record)
  "Prepare RECORD's target artifact, settings, and Mode for SESSION."
  (let* ((selection (plist-get record :selection))
         (mode (plist-get selection :mode))
         (target-buffer (mevedel-plan-mode--worktree-target-buffer record))
         (target-session
          (buffer-local-value 'mevedel--session target-buffer))
         (_body (mevedel-plan-mode--accepted-body record))
         (source-artifact
          (list :path (plist-get record :accepted-path)
                :absolute-path (plist-get record :accepted-absolute-path)
                :hash (plist-get record :accepted-hash)))
         (accepted
          (mevedel-plan-archive-accepted
           source-artifact target-session
           (file-name-concat "plans" "accepted.md")))
         (prepared (copy-tree record)))
    (setf (mevedel-session-preset-name target-session)
          (mevedel-session-preset-name session)
          (mevedel-session-preset-settings target-session)
          (copy-tree (mevedel-session-preset-settings session))
          (mevedel-session-plan-metadata target-session)
          (list :status 'accepted
                :accepted-path (plist-get accepted :path)
                :accepted-absolute-path (plist-get accepted :absolute-path)
                :accepted-hash (plist-get accepted :hash)))
    (with-current-buffer target-buffer
      (require 'mevedel-presets)
      (require 'mevedel-permissions)
      (mevedel-preset-restore-session target-session target-buffer)
      (mevedel-permission-mode-transition mode)
      (mevedel-plan-mode--persist target-session target-buffer))
    (setq prepared (plist-put prepared :step 'submit))
    (setq prepared (plist-put prepared :target-accepted-path
                              (plist-get accepted :path)))
    (setq prepared (plist-put prepared :target-accepted-absolute-path
                              (plist-get accepted :absolute-path)))
    (setq prepared (plist-put prepared :target-accepted-hash
                              (plist-get accepted :hash)))
    (mevedel-plan--metadata-put session :implementation-retry prepared)
    (mevedel-plan-mode--persist session chat-buffer)
    prepared))

(defun mevedel-plan-mode--summary-instructions ()
  "Return compaction guidance for an implementation handoff."
  (concat
   "Create a portable implementation handoff that preserves requirements, "
   "repository discoveries, rationale, constraints, unresolved risks, and "
   "next steps. Do not reproduce the accepted plan; it will be supplied "
   "separately in full after this summary."))

(defun mevedel-plan-mode--summary-without-plan (summary plan)
  "Return SUMMARY without an exact duplicate of PLAN."
  (string-replace plan
                  "(Accepted plan omitted; supplied separately.)"
                  summary))

(defun mevedel-plan-mode--prepare-summary (session chat-buffer record)
  "Aggressively compact CHAT-BUFFER and cache the result in SESSION RECORD."
  (require 'mevedel-compact)
  (with-current-buffer chat-buffer
    (let* ((plan (mevedel-plan-mode--accepted-body record))
           (target (mevedel--compact-main-target))
           (apply-function (plist-get target :apply)))
      (setq target
            (plist-put
             target :apply
             (lambda (active-target summary &rest args)
               (let ((prepared (copy-tree record)))
                 (setq prepared (plist-put prepared :summary summary))
                 (setq prepared (plist-put prepared :step 'submit))
                 (cl-remf prepared :failure)
                 (mevedel-plan--metadata-put
                  session :implementation-retry prepared)
                 (condition-case apply-error
                     (apply apply-function active-target summary args)
                   (error
                    (setq prepared (plist-put prepared :step 'prepare-summary))
                    (mevedel-plan--metadata-put
                     session :implementation-retry prepared)
                    (signal (car apply-error) (cdr apply-error))))))))
      (mevedel--compact-run
       :aggressive t
       :instructions (mevedel-plan-mode--summary-instructions)
       :prepared-summary (plist-get record :summary)
       :summary-ready
       (lambda (summary)
         (let ((prepared (copy-tree record))
               (summary (mevedel-plan-mode--summary-without-plan
                         summary plan)))
           (setq prepared (plist-put prepared :summary summary))
           (setq prepared (plist-put prepared :step 'prepare-summary))
           (cl-remf prepared :failure)
           (mevedel-plan--metadata-put
            session :implementation-retry prepared)
           (mevedel-plan-mode--persist session chat-buffer)
           summary))
       :target target
       :callback
       (lambda (err)
         (if err
             (mevedel-plan-mode--implementation-failed
              session chat-buffer (format "%s" err))
           (mevedel-plan-mode--dispatch-accepted session chat-buffer)))))))

(defun mevedel-plan-mode--implementation-failed
    (session chat-buffer reason)
  "Keep SESSION retryable after REASON from CHAT-BUFFER."
  (when-let* ((record (copy-tree
                       (plist-get (mevedel-session-plan-metadata session)
                                  :implementation-retry))))
    (setq record (plist-put record :failure reason))
    (mevedel-plan--metadata-put session :implementation-retry record)
    (condition-case err
        (mevedel-plan-mode--persist session chat-buffer)
      (error
       (display-warning
        'mevedel
        (format "Could not persist plan implementation retry: %s"
                (error-message-string err))))))
  (message
   "mevedel: Implementation did not start: %s; retry with M-x mevedel-retry-plan-implementation"
   reason)
  nil)

(defun mevedel-plan-mode--implementation-started (session chat-buffer)
  "Clear SESSION's retry state after request startup from CHAT-BUFFER."
  (let ((metadata (copy-sequence
                   (or (mevedel-session-plan-metadata session) nil))))
    (cl-remf metadata :implementation-retry)
    (setf (mevedel-session-plan-metadata session) metadata)
    (condition-case err
        (mevedel-plan-mode--persist session chat-buffer)
      (error
       (display-warning
        'mevedel
        (format "Could not persist started plan implementation: %s"
                (error-message-string err)))))))

(cl-defun mevedel-plan-mode--dispatch-accepted (session chat-buffer)
  "Prepare and dispatch SESSION's accepted plan from CHAT-BUFFER."
  (condition-case err
      (let* ((record
              (copy-tree
               (or (plist-get (mevedel-session-plan-metadata session)
                              :implementation-retry)
                   (error "No accepted plan implementation to retry"))))
             (selection (plist-get record :selection))
             (location (plist-get selection :location))
             (context (plist-get selection :context)))
        (unless (mevedel-plan-mode--selection-valid-p selection)
          (error "Invalid accepted plan implementation selection"))
        (unless (memq (plist-get record :step)
                      '(prepare-context prepare-summary prepare-worktree
                        prepare-target submit))
          (error "Invalid accepted plan implementation step"))
        (when (eq location 'worktree)
          (let ((branch (plist-get selection :branch)))
            (when (or (not (stringp branch)) (string-empty-p branch))
              (error "Accepted Worktree implementation lacks a branch"))))
        (cl-remf record :failure)
        (mevedel-plan--metadata-put session :implementation-retry record)
        (mevedel-plan-mode--persist session chat-buffer)
        (when (eq (plist-get record :step) 'prepare-worktree)
          (unless (eq location 'worktree)
            (error "Invalid accepted plan Worktree step"))
          (setq record
                (mevedel-plan-mode--prepare-worktree
                 session chat-buffer record)))
        (when (eq (plist-get record :step) 'prepare-target)
          (unless (eq location 'worktree)
            (error "Invalid accepted plan target step"))
          (setq record
                (mevedel-plan-mode--prepare-worktree-target
                 session chat-buffer record)))
        (when (eq (plist-get record :step) 'prepare-summary)
          (unless (eq context 'summary)
            (error "Invalid accepted plan summary step"))
          (cl-return-from mevedel-plan-mode--dispatch-accepted
            (mevedel-plan-mode--prepare-summary
             session chat-buffer record)))
        (when (eq (plist-get record :step) 'prepare-context)
          (unless (and (eq location 'here) (eq context 'fresh))
            (error "Invalid accepted plan preparation step"))
          ;; Persist the next step as part of the segment rotation's sidecar
          ;; transaction so resume cannot rotate the planning segment twice.
          (setq record (plist-put record :step 'submit))
          (mevedel-plan--metadata-put session :implementation-retry record)
          (condition-case rotation-error
              (with-current-buffer chat-buffer
                (require 'mevedel-session-persistence)
                (unless
                    (mevedel-session-persistence-start-fresh-segment
                     session chat-buffer
                     :initial-text (or (and (boundp 'gptel-prompt-prefix-alist)
                                            (alist-get
                                             major-mode
                                             gptel-prompt-prefix-alist))
                                       ""))
                  (error "Could not start a fresh conversation segment"))
                (mevedel--run-session-start-hooks "clear"))
            (error
             (setq record (plist-put record :step 'prepare-context))
             (mevedel-plan--metadata-put
              session :implementation-retry record)
             (signal (car rotation-error) (cdr rotation-error)))))
        (when (and (eq context 'summary)
                   (not (equal (plist-get record :summary)
                               (with-current-buffer chat-buffer
                                 (mevedel--compact-previous-summary)))))
          (error "Prepared plan summary does not match the current segment"))
        (let* ((target-buffer
                (if (eq location 'worktree)
                    (mevedel-plan-mode--worktree-target-buffer record)
                  chat-buffer))
               (target-record
                (if (eq location 'worktree)
                    (let ((target-record (copy-tree record)))
                      (setq target-record
                            (plist-put
                             target-record :accepted-path
                             (plist-get record :target-accepted-path)))
                      (setq target-record
                            (plist-put
                             target-record :accepted-absolute-path
                             (plist-get record
                                        :target-accepted-absolute-path)))
                      (plist-put target-record :accepted-hash
                                 (plist-get record :target-accepted-hash)))
                  record))
               (body (mevedel-plan-mode--accepted-body target-record))
               (accepted
                (list :path (plist-get target-record :accepted-path)
                      :absolute-path
                      (plist-get target-record :accepted-absolute-path)
                      :hash (plist-get target-record :accepted-hash)))
               (prompt (mevedel-plan-mode--implementation-prompt
                        accepted body))
               (view-buffer
                (mevedel-view--interaction-target-buffer target-buffer)))
          (with-current-buffer view-buffer
            (mevedel-view--run-prompt-submit-hook
             prompt "Implement accepted plan"
             (lambda (submission)
               (condition-case dispatch-error
                   (progn
                     (with-current-buffer target-buffer
                       (mevedel--implement-plan
                        (list :context 'current
                              :plan-file
                              (plist-get target-record
                                         :accepted-absolute-path)
                              :permission-mode (plist-get selection :mode)
                              :prompt-submission submission)))
                     (mevedel-plan-mode--implementation-started
                      session chat-buffer))
                 (error
                  (mevedel-plan-mode--implementation-failed
                   session chat-buffer
                   (error-message-string dispatch-error)))))
             (lambda ()
               (mevedel-plan-mode--implementation-failed
                session chat-buffer "Prompt submission was blocked"))))))
    (error
     (mevedel-plan-mode--implementation-failed
      session chat-buffer (error-message-string err)))))

(defun mevedel-retry-plan-implementation (&optional session chat-buffer)
  "Retry SESSION's accepted plan implementation from CHAT-BUFFER."
  (interactive)
  (let* ((session (mevedel-plan--current-session session))
         (chat-buffer
          (or chat-buffer
              (and (boundp 'mevedel--data-buffer)
                   (buffer-live-p mevedel--data-buffer)
                   mevedel--data-buffer)
              (current-buffer))))
    (unless (and session
                 (plist-get (mevedel-session-plan-metadata session)
                            :implementation-retry))
      (user-error "No accepted plan implementation to retry"))
    (mevedel-plan-mode--dispatch-accepted session chat-buffer)))

(defun mevedel-plan-mode--accept
    (plan-markdown chat-buffer session selection)
  "Accept PLAN-MARKDOWN and dispatch SELECTION from CHAT-BUFFER SESSION."
  (require 'mevedel-permissions)
  (require 'mevedel-view-composer)
  (unless (mevedel-plan-mode--selection-valid-p selection)
    (error "Unsupported Plan implementation selection: %S" selection))
  (let* ((mode (plist-get selection :mode))
         (artifacts (mevedel-plan-accept
                     plan-markdown session chat-buffer t))
         (accepted (plist-get artifacts :accepted)))
    (mevedel-plan--metadata-put session :selection selection)
    (mevedel-plan--metadata-put
     session :implementation-retry
     (mevedel-plan-mode--implementation-record selection accepted))
    (mevedel-plan-mode--deactivate session)
    (when (eq (plist-get selection :location) 'here)
      (with-current-buffer chat-buffer
        (mevedel-permission-mode-transition mode)))
    (mevedel-plan-mode--dispatch-accepted session chat-buffer)))

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
                 (body
                  (format
                   "\n%s\n\nLocation   %s\nContext    %s — %s\nExecution  Direct\nMode       %s\n%s\n%s\n"
                   (if (fboundp 'mevedel-view--fontify-as)
                       (mevedel-view--fontify-as
                        (plist-get entry :body) 'markdown-mode)
                     (plist-get entry :body))
                   (capitalize (symbol-name (plist-get selection :location)))
                   (capitalize (symbol-name (plist-get selection :context)))
                   (mevedel-plan-mode--context-description
                    (plist-get selection :context))
                   (plist-get selection :mode)
                   (if warning (concat "\n" warning "\n") "")
                   (concat
                    "Keys: RET implement  l location  c context  TAB/m mode  "
                    "f feedback draft  q cancel"))))
            (define-key keymap (kbd "RET") #'accept)
            (define-key keymap (kbd "<return>") #'accept)
            (define-key keymap (kbd "C-c C-c") #'accept)
            (define-key keymap (kbd "TAB") #'cycle-mode)
            (define-key keymap (kbd "<tab>") #'cycle-mode)
            (define-key keymap (kbd "m") #'cycle-mode)
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
  (when-let* ((session (mevedel-plan--current-session))
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
  (let* ((session (mevedel-plan--current-session session))
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
               (mevedel-plan-mode--selection-valid-p selection)
               (stringp plan)
               (equal hash (mevedel-plan-hash plan)))))
    (cond
     ((and valid (not (mevedel-session-pending-plan-approval session)))
      (require 'mevedel-goal)
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

(defun mevedel-plan-hash (plan-markdown)
  "Return a stable hash for PLAN-MARKDOWN."
  (secure-hash 'sha256 (string-trim-right (or plan-markdown ""))))

(defun mevedel-plan-current-path (&optional session buffer relative-path)
  "Return the session-local current plan path for SESSION.
Materialize the session directory when needed.  BUFFER defaults to the
current data buffer."
  (require 'mevedel-structs)
  (let* ((session (or session mevedel--session))
         (buffer (or buffer (current-buffer)))
         (save-path (or (mevedel-session-save-path session)
                        (progn
                          (require 'mevedel-session-persistence)
                          (mevedel-session-persistence-ensure-files
                           session buffer)))))
    (unless save-path
      (error "Could not materialize session for plan"))
    (file-name-concat save-path
                      (or relative-path mevedel-plan--relative-current-path))))

(defun mevedel-plan--metadata-path (session)
  "Return SESSION's recorded current plan path, when available."
  (require 'mevedel-structs)
  (let ((metadata (mevedel-session-plan-metadata session)))
    (or (when-let* ((save-path (mevedel-session-save-path session))
                    (path (or (plist-get metadata :path)
                              mevedel-plan--relative-current-path)))
          (file-name-concat save-path path))
        (plist-get metadata :absolute-path))))

(defun mevedel-plan-write-current
    (plan-markdown session buffer &optional relative-path)
  "Write PLAN-MARKDOWN to SESSION's current plan artifact for BUFFER.
RELATIVE-PATH overrides the default path below SESSION's save directory.
Return an explicit artifact plist containing `:path', `:absolute-path', and
`:hash'."
  (require 'mevedel-structs)
  (let* ((relative-path (or relative-path
                            mevedel-plan--relative-current-path))
         (path (mevedel-plan-current-path session buffer relative-path))
         (plan-markdown (mevedel-plan-validate plan-markdown))
         (hash (mevedel-plan-hash plan-markdown)))
    (make-directory (file-name-directory path) t)
    (let ((coding-system-for-write 'utf-8-unix))
      (write-region plan-markdown nil path nil 'silent))
    (let ((turn (or (mevedel-session-turn-count session) 0))
          (metadata (copy-sequence (or (mevedel-session-plan-metadata session)
                                       nil))))
      (setq metadata (plist-put metadata :path relative-path))
      (setq metadata (plist-put metadata :absolute-path path))
      (setq metadata (plist-put metadata :hash hash))
      (setq metadata (plist-put metadata :status 'presented))
      (setq metadata (plist-put metadata :updated-turn turn))
      (setq metadata (plist-put metadata :updated-at
                                (format-time-string "%FT%H-%M-%S")))
      (cl-remf metadata :accepted-path)
      (cl-remf metadata :accepted-absolute-path)
      (cl-remf metadata :accepted-hash)
      (setq metadata (plist-put metadata :verification-pending nil))
      (setq metadata (plist-put metadata :accepted-turn nil))
      (setq metadata (plist-put metadata :accepted-at nil))
      (setf (mevedel-session-plan-metadata session) metadata))
    (list :path relative-path
          :absolute-path path
          :hash hash)))

(defun mevedel-plan-archive-accepted
    (current-artifact session &optional relative-path)
  "Archive CURRENT-ARTIFACT as an accepted plan for SESSION.
CURRENT-ARTIFACT is the plist returned by `mevedel-plan-write-current'.
RELATIVE-PATH names a deterministic immutable destination when non-nil.
Return a plist containing `:path', `:absolute-path', and `:hash'."
  (require 'mevedel-structs)
  (let ((plan-path (plist-get current-artifact :absolute-path))
        (plan-hash (plist-get current-artifact :hash)))
    (unless (and plan-path plan-hash (file-exists-p plan-path))
      (error "Accepted plan artifact does not exist"))
    (let* ((save-path (mevedel-session-save-path session))
           (dir (file-name-directory plan-path))
           (timestamp (format-time-string "%Y%m%d-%H%M%S"))
           (archive-path
            (if relative-path
                (file-name-concat save-path relative-path)
              (file-name-concat dir (format "accepted-%s.md" timestamp))))
           (index 1))
      (unless relative-path
        (while (file-exists-p archive-path)
          (setq archive-path
                (file-name-concat dir (format "accepted-%s-%d.md"
                                              timestamp index)))
          (setq index (1+ index))))
      (make-directory (file-name-directory archive-path) t)
      (if (file-exists-p archive-path)
          (with-temp-buffer
            (insert-file-contents archive-path)
            (unless (and relative-path
                         (equal plan-hash
                                (mevedel-plan-hash (buffer-string))))
              (error "Accepted plan artifact already exists with different content")))
        (copy-file plan-path archive-path))
      (list :path (and save-path (file-relative-name archive-path save-path))
            :absolute-path archive-path
            :hash plan-hash))))

(defun mevedel-plan-current-body (&optional session)
  "Return SESSION's current plan artifact contents, or nil."
  (require 'mevedel-utilities)
  (when-let* ((session (or session mevedel--session)))
    (let ((path (mevedel-plan--metadata-path session)))
      (when (and path (file-exists-p path))
        (with-temp-buffer
          (insert-file-contents path)
          (mevedel--normalize-message-text (buffer-string)))))))

(defun mevedel-plan-current-exists-p (&optional session)
  "Return non-nil when SESSION has a current plan artifact on disk."
  (when-let* ((session (or session mevedel--session)))
    (let ((path (mevedel-plan--metadata-path session)))
      (and path (file-exists-p path)))))

(defun mevedel-plan-mark-accepted
    (session current-artifact accepted-artifact &optional skip-verification)
  "Mark SESSION's CURRENT-ARTIFACT as accepted.
ACCEPTED-ARTIFACT identifies the immutable archived plan.  When
SKIP-VERIFICATION is non-nil, do not leave verification pending."
  (require 'mevedel-structs)
  (let ((metadata (copy-sequence (or (mevedel-session-plan-metadata session)
                                     nil))))
    (setq metadata (plist-put metadata :path
                              (plist-get current-artifact :path)))
    (setq metadata (plist-put metadata :status 'accepted))
    (setq metadata (plist-put metadata :accepted-turn
                              (or (mevedel-session-turn-count session) 0)))
    (setq metadata (plist-put metadata :accepted-at
                              (format-time-string "%FT%H-%M-%S")))
    (if skip-verification
        (cl-remf metadata :verification-pending)
      (setq metadata (plist-put metadata :verification-pending t)))
    (setq metadata (plist-put metadata :absolute-path
                              (plist-get current-artifact :absolute-path)))
    (setq metadata (plist-put metadata :accepted-path
                              (plist-get accepted-artifact :path)))
    (setq metadata (plist-put metadata :accepted-absolute-path
                              (plist-get accepted-artifact :absolute-path)))
    (setq metadata (plist-put metadata :accepted-hash
                              (plist-get accepted-artifact :hash)))
    (setf (mevedel-session-plan-metadata session) metadata)
    metadata))

(defun mevedel-plan-accept
    (plan-markdown session buffer &optional skip-verification
                   current-relative-path accepted-relative-path)
  "Persist and accept PLAN-MARKDOWN for SESSION and BUFFER.
CURRENT-RELATIVE-PATH and ACCEPTED-RELATIVE-PATH override artifact locations.
Return `(:current ARTIFACT :accepted ARTIFACT)' for later dispatch."
  (let* ((current (mevedel-plan-write-current
                   plan-markdown session buffer current-relative-path))
         (accepted (mevedel-plan-archive-accepted
                    current session accepted-relative-path)))
    (mevedel-plan-mark-accepted
     session current accepted skip-verification)
    (list :current current :accepted accepted)))

(defun mevedel-plan-implementation-input
    (context current-artifact permission-mode goal-context)
  "Return a validated implementation input plist.
CONTEXT is `full' or `focused', CURRENT-ARTIFACT is returned by
`mevedel-plan-write-current', PERMISSION-MODE is the implementation permission
mode.  GOAL-CONTEXT is the authoritative persisted lifecycle fragment."
  (unless (memq context '(full focused))
    (error "Unknown implementation context: %s" context))
  (unless (memq permission-mode '(ask auto full-auto))
    (error "Unknown implementation permission mode: %s" permission-mode))
  (unless (and (stringp goal-context) (not (string-blank-p goal-context)))
    (error "Implementation requires Goal context"))
  (let ((path (plist-get current-artifact :absolute-path)))
    (unless (and path (file-exists-p path))
      (error "Current plan artifact does not exist"))
    (list :context context
          :plan-file path
          :permission-mode permission-mode
          :goal-context goal-context)))

(provide 'mevedel-plan)
;;; mevedel-plan.el ends here
