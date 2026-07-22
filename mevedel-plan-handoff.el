;;; mevedel-plan-handoff.el -- Durable accepted-Plan handoff -*- lexical-binding: t -*-

;;; Commentary:

;; Owns accepted-plan preparation, crash-safe target recovery, and Direct or
;; Goal kickoff.  Plan conversation and proposal UI remain in
;; `mevedel-plan-mode'.

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
(declare-function mevedel--compact-main-target "mevedel-compact" nil)
(declare-function mevedel--compact-previous-summary "mevedel-compact" nil)
(declare-function mevedel--compact-run "mevedel-compact" (&rest args))

;; `mevedel-goal'
(declare-function mevedel-goal-ensure "mevedel-goal"
                  (objective session plan-reference id))
(declare-function mevedel-goal-new-id "mevedel-goal" nil)
(declare-function mevedel-goal-pause-runtime-failure "mevedel-goal"
                  (buffer reason))

;; `mevedel-permissions'
(declare-function mevedel-permission-mode-transition
                  "mevedel-permissions" (mode))

;; `mevedel-plan'
(declare-function mevedel-plan--metadata-put "mevedel-plan"
                  (session key value))
(declare-function mevedel-plan-archive-accepted "mevedel-plan"
                  (artifact session &optional relative-path))
(declare-function mevedel-plan-hash "mevedel-plan" (plan-markdown))

;; `mevedel-presets'
(declare-function mevedel-preset-restore-session "mevedel-presets"
                  (session &optional buffer))

;; `mevedel-session-persistence'
(declare-function mevedel-session-persistence--summary-block
                  "mevedel-session-persistence" (summary))
(declare-function mevedel-session-persistence-ensure-files
                  "mevedel-session-persistence" (session buffer))
(declare-function mevedel-session-persistence-restore
                  "mevedel-session-persistence" (session-dir))
(declare-function mevedel-session-persistence-save
                  "mevedel-session-persistence" (session buffer))
(declare-function mevedel-session-persistence-start-fresh-segment
                  "mevedel-session-persistence" (session buffer &rest args))

;; `mevedel-structs'
(declare-function mevedel-session-plan-metadata "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-preset-name "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-preset-settings "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-session-id "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-working-directory "mevedel-structs" (cl-x) t)
(defvar mevedel--data-buffer)
(defvar mevedel--session)
(defvar mevedel-goal-token-budget)

;; `mevedel-utilities'
(declare-function mevedel--normalize-message-text "mevedel-utilities" (text))

;; `mevedel-view-composer'
(declare-function mevedel-view--run-prompt-submit-hook
                  "mevedel-view-composer"
                  (input display-text callback &optional blocked-callback
                         prior-context))

;; `mevedel-view-interaction'
(declare-function mevedel-view--interaction-target-buffer
                  "mevedel-view-interaction" (chat-buffer))

;; `mevedel-view-stream'
(declare-function mevedel-view--stop-request-progress
                  "mevedel-view-stream" nil)
(declare-function mevedel-view--update-spinner
                  "mevedel-view-stream" (status &optional owner))

;; `mevedel-worktree'
(declare-function mevedel-worktree-create-session
                  "mevedel-worktree" (&optional branch purpose clean))

(defconst mevedel-plan-handoff--accepted-goal-objective
  (concat
   "Implement the accepted plan referenced by this Goal. Read it before acting. "
   "Treat its stated outcomes, constraints, and acceptance criteria as the "
   "completion contract; its implementation mechanics remain revisable. Use "
   "its named validation commands and artifacts to verify progress, and keep "
   "the Goal active until current evidence proves the full contract. Where the "
   "mechanics are silent or conflict with current repository evidence, preserve "
   "the completion contract and use the safest effective implementation.")
  "Deterministic objective for a Goal created from an accepted Plan.")

(defconst mevedel-plan-handoff-implementation-modes '(ask auto full-auto)
  "Permission modes supported by accepted Plan handoffs.")


;;
;;; Durable handoff

(defun mevedel-plan-handoff--current-session (&optional session)
  "Return SESSION or the session reachable from the current buffer."
  (or session
      (and (boundp 'mevedel--session) mevedel--session)
      (and (boundp 'mevedel--data-buffer)
           (buffer-live-p mevedel--data-buffer)
           (buffer-local-value 'mevedel--session mevedel--data-buffer))))

(defun mevedel-plan-handoff-reserved-goal-id (&optional session)
  "Return SESSION's Goal kickoff reservation, or nil.
A source retry reserves only a Here handoff; a Worktree target carries its own
reservation while its prepared kickoff has not started."
  (when-let* ((session (mevedel-plan-handoff--current-session session)))
    (let* ((metadata (mevedel-session-plan-metadata session))
           (retry (plist-get metadata :implementation-retry))
           (selection (plist-get retry :selection)))
      (or (and (eq (plist-get selection :execution) 'goal)
               (eq (plist-get selection :location) 'here)
               (plist-get retry :goal-id))
          (plist-get metadata :implementation-goal-id)))))

(defun mevedel-plan-handoff-selection-valid-p (selection)
  "Return non-nil when SELECTION is a supported Plan handoff selection."
  (and (proper-list-p selection)
       (let ((location (plist-get selection :location))
             (context (plist-get selection :context)))
         (and (or (and (eq location 'here)
                       (memq context '(current fresh summary)))
                  (and (eq location 'worktree)
                       (memq context '(fresh summary))))
              (memq (plist-get selection :execution) '(direct goal))
              (memq (plist-get selection :mode)
                    mevedel-plan-handoff-implementation-modes)))))

(defun mevedel-plan-handoff--implementation-prompt
    (accepted-artifact plan-markdown)
  "Return the Direct prompt for ACCEPTED-ARTIFACT and PLAN-MARKDOWN."
  (format
   "Accepted plan artifact: %s\n\nAccepted plan:\n%s\n\nImplementation instructions:\nImplement the accepted plan against the current repository state. Preserve its stated outcomes and acceptance criteria while using repository evidence to choose the safest effective mechanics."
   (plist-get accepted-artifact :absolute-path)
   plan-markdown))

(defun mevedel-plan-handoff--goal-kickoff-prompt
    (accepted-artifact plan-markdown)
  "Return the Goal kickoff for ACCEPTED-ARTIFACT and PLAN-MARKDOWN."
  (format
   "Accepted plan artifact: %s\n\nAccepted plan:\n%s\n\nGoal kickoff:\nBegin the active Goal. Read the accepted plan supplied above before acting."
   (plist-get accepted-artifact :absolute-path)
   plan-markdown))

(defun mevedel-plan-handoff--persist (session chat-buffer)
  "Persist SESSION from CHAT-BUFFER."
  (require 'mevedel-session-persistence)
  (mevedel-session-persistence-save session chat-buffer))

(defun mevedel-plan-handoff--implementation-record
    (selection accepted &optional goal-token-budget)
  "Return retry state for SELECTION and ACCEPTED artifact."
  (let ((record
         (list :step (pcase (plist-get selection :context)
                       ('summary 'prepare-summary)
                       ('fresh
                        (if (eq (plist-get selection :location) 'worktree)
                            'prepare-worktree
                          'prepare-context))
                       (_ 'submit))
               :selection (copy-tree selection)
               :accepted (copy-tree accepted)
               :goal-token-budget goal-token-budget)))
    (when (eq (plist-get selection :execution) 'goal)
      (require 'mevedel-goal)
      (setq record (plist-put record :goal-id (mevedel-goal-new-id))))
    record))

(defun mevedel-plan-handoff--accepted-body (artifact)
  "Return ARTIFACT's validated immutable accepted-plan body."
  (let ((path (plist-get artifact :absolute-path))
        (hash (plist-get artifact :hash)))
    (unless (and (stringp path) (file-exists-p path) (stringp hash))
      (error "Accepted plan artifact is unavailable"))
    (with-temp-buffer
      (insert-file-contents path)
      (let ((body (mevedel--normalize-message-text (buffer-string))))
        (unless (equal hash (mevedel-plan-hash body))
          (error "Accepted plan artifact hash does not match"))
        body))))

(defun mevedel-plan-handoff--worktree-target-buffer (record)
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

(defun mevedel-plan-handoff--prepare-worktree (session chat-buffer record)
  "Create RECORD's Worktree target and persist its identity in SESSION."
  (require 'mevedel-worktree)
  (let* ((selection (plist-get record :selection))
         (branch (plist-get selection :branch))
         (result
          (with-current-buffer chat-buffer
            (mevedel-worktree-create-session
             branch "Accepted Plan implementation"
             (eq (plist-get selection :context) 'summary))))
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
    (mevedel-plan-handoff--persist session chat-buffer)
    prepared))

(defun mevedel-plan-handoff--prepare-worktree-target
    (session chat-buffer record)
  "Prepare RECORD's target artifact, settings, and Mode for SESSION."
  (let* ((selection (plist-get record :selection))
         (mode (plist-get selection :mode))
         (target-buffer (mevedel-plan-handoff--worktree-target-buffer record))
         (target-session
          (buffer-local-value 'mevedel--session target-buffer))
         (source-artifact (plist-get record :accepted))
         (_body (mevedel-plan-handoff--accepted-body source-artifact))
         (target-metadata (mevedel-session-plan-metadata target-session))
         (existing
          (and (eq (plist-get target-metadata :status) 'accepted)
               (list :path (plist-get target-metadata :accepted-path)
                     :absolute-path
                     (plist-get target-metadata :accepted-absolute-path)
                     :hash (plist-get target-metadata :accepted-hash))))
         (accepted
          (if existing
              (progn
                (unless (equal (plist-get existing :hash)
                               (plist-get source-artifact :hash))
                  (error "Prepared Worktree plan does not match source"))
                (mevedel-plan-handoff--accepted-body existing)
                existing)
            (mevedel-plan-archive-accepted
             source-artifact target-session
             (file-name-concat "plans" "accepted.md"))))
         (prepared (copy-tree record)))
    (unless existing
      (setf (mevedel-session-preset-name target-session)
            (mevedel-session-preset-name session)
            (mevedel-session-preset-settings target-session)
            (copy-tree (mevedel-session-preset-settings session))
            (mevedel-session-plan-metadata target-session)
            (append
             (list :status 'accepted
                   :accepted-path (plist-get accepted :path)
                   :accepted-absolute-path (plist-get accepted :absolute-path)
                   :accepted-hash (plist-get accepted :hash))
             (and (eq (plist-get selection :execution) 'goal)
                  (list :implementation-goal-id
                        (plist-get record :goal-id)))))
      (with-current-buffer target-buffer
        (when (eq (plist-get selection :execution) 'goal)
          (setq-local mevedel-goal-token-budget
                      (plist-get record :goal-token-budget)))
        (require 'mevedel-presets)
        (require 'mevedel-permissions)
        (mevedel-preset-restore-session target-session target-buffer)
        (mevedel-permission-mode-transition mode)
        (when-let* (((eq (plist-get selection :context) 'summary))
                    (summary (plist-get record :summary)))
          (let ((current (mevedel--compact-previous-summary)))
            (cond
             ((equal current summary))
             (current (error "Prepared Worktree summary does not match target"))
             (t
              (let ((inhibit-read-only t))
                (goto-char (point-max))
                (unless (bolp) (insert "\n"))
                (insert
                 (mevedel-session-persistence--summary-block summary)))))))
        (mevedel-plan-handoff--persist target-session target-buffer)))
    (setq prepared (plist-put prepared :step 'submit))
    (setq prepared (plist-put prepared :target-accepted accepted))
    (mevedel-plan--metadata-put session :implementation-retry prepared)
    (mevedel-plan-handoff--persist session chat-buffer)
    prepared))

(defun mevedel-plan-handoff--summary-instructions (&optional portable-paths)
  "Return compaction guidance for an implementation handoff.
When PORTABLE-PATHS is non-nil, require repository-relative file references."
  (concat
   "Create a portable implementation handoff that preserves requirements, "
   "repository discoveries, rationale, constraints, unresolved risks, and "
   "next steps. Do not reproduce the accepted plan; it will be supplied "
   "separately in full after this summary."
   (when portable-paths
     " Express repository-local file references relative to the repository root.")))

(defun mevedel-plan-handoff--prepare-summary (session chat-buffer record)
  "Aggressively compact CHAT-BUFFER and cache the result in SESSION RECORD."
  (require 'mevedel-compact)
  (with-current-buffer chat-buffer
    (let* ((selection (plist-get record :selection))
           (worktree-p (eq (plist-get selection :location) 'worktree))
           (plan
            (mevedel-plan-handoff--accepted-body (plist-get record :accepted)))
           (target (mevedel--compact-main-target))
           (apply-function (plist-get target :apply)))
      (setq target
            (plist-put
             target :apply
             (lambda (active-target summary &rest args)
               (let ((prepared (copy-tree record)))
                 (setq prepared (plist-put prepared :summary summary))
                 (setq prepared
                       (plist-put prepared :step
                                  (if worktree-p 'prepare-worktree 'submit)))
                 (cl-remf prepared :failure)
                 (mevedel-plan--metadata-put
                  session :implementation-retry prepared)
                 (if worktree-p
                     (mevedel-plan-handoff--persist session chat-buffer)
                   (condition-case apply-error
                       (apply apply-function active-target summary args)
                     (error
                      (setq prepared (plist-put prepared :step 'prepare-summary))
                      (mevedel-plan--metadata-put
                       session :implementation-retry prepared)
                      (signal (car apply-error) (cdr apply-error)))))))))
      (when worktree-p
        (setq target (plist-put target :begin-context-epoch nil))
        (setq target (plist-put target :warn-on-completion nil)))
      (mevedel--compact-run
       :aggressive t
       :instructions (mevedel-plan-handoff--summary-instructions worktree-p)
       :prepared-summary (plist-get record :summary)
       :summary-ready
       (lambda (summary)
         (let* ((prepared (copy-tree record))
                (summary
                 (string-replace
                  plan "(Accepted plan omitted; supplied separately.)"
                  summary))
                (summary
                 (if worktree-p
                     (string-replace
                      (file-name-as-directory
                       (expand-file-name
                        (mevedel-session-working-directory session)))
                      "" summary)
                   summary)))
           (setq prepared (plist-put prepared :summary summary))
           (setq prepared (plist-put prepared :step 'prepare-summary))
           (cl-remf prepared :failure)
           (mevedel-plan--metadata-put
            session :implementation-retry prepared)
           (mevedel-plan-handoff--persist session chat-buffer)
           summary))
       :target target
       :callback
       (lambda (err)
         (if err
             (mevedel-plan-handoff--implementation-failed
              session chat-buffer (format "%s" err))
           (mevedel-plan-handoff--dispatch-accepted session chat-buffer)))))))

(defun mevedel-plan-handoff--implementation-failed
    (session chat-buffer reason)
  "Keep SESSION retryable after REASON from CHAT-BUFFER."
  (when-let* ((view-buffer
               (ignore-errors
                 (mevedel-view--interaction-target-buffer chat-buffer)))
              ((fboundp 'mevedel-view--stop-request-progress)))
    (with-current-buffer view-buffer
      (mevedel-view--stop-request-progress)))
  (when-let* ((record (copy-tree
                       (plist-get (mevedel-session-plan-metadata session)
                                  :implementation-retry))))
    (setq record (plist-put record :failure reason))
    (mevedel-plan--metadata-put session :implementation-retry record)
    (condition-case err
        (mevedel-plan-handoff--persist session chat-buffer)
      (error
       (display-warning
        'mevedel
        (format "Could not persist plan implementation retry: %s"
                (error-message-string err))))))
  (message
   "mevedel: Implementation did not start: %s; retry with M-x mevedel-retry-plan-implementation"
   reason)
  nil)

(defun mevedel-plan-handoff--implementation-started (session chat-buffer)
  "Clear SESSION's retry state after request startup from CHAT-BUFFER."
  (let ((metadata (copy-sequence
                   (or (mevedel-session-plan-metadata session) nil))))
    (cl-remf metadata :implementation-retry)
    (setf (mevedel-session-plan-metadata session) metadata)
    (condition-case err
        (mevedel-plan-handoff--persist session chat-buffer)
      (error
       (display-warning
        'mevedel
        (format "Could not persist started plan implementation: %s"
                (error-message-string err)))))))

(defun mevedel-plan-handoff--goal-handoff-complete (session chat-buffer)
  "Durably clear SESSION's Plan retry before Goal kickoff from CHAT-BUFFER."
  (let* ((old-metadata (mevedel-session-plan-metadata session))
         (metadata (copy-sequence (or old-metadata nil))))
    (cl-remf metadata :implementation-retry)
    (setf (mevedel-session-plan-metadata session) metadata)
    (condition-case err
        (mevedel-plan-handoff--persist session chat-buffer)
      (error
       (setf (mevedel-session-plan-metadata session) old-metadata)
       (signal (car err) (cdr err))))))

(defun mevedel-plan-handoff--clear-target-goal-reservation
    (target-session target-buffer)
  "Clear TARGET-SESSION's Worktree Goal reservation in TARGET-BUFFER."
  (let ((metadata (copy-sequence
                   (or (mevedel-session-plan-metadata target-session) nil))))
    (when (plist-member metadata :implementation-goal-id)
      (cl-remf metadata :implementation-goal-id)
      (setf (mevedel-session-plan-metadata target-session) metadata)
      (mevedel-plan-handoff--persist target-session target-buffer))))

(defun mevedel-plan-handoff--prepare-context (session chat-buffer record)
  "Rotate CHAT-BUFFER once and return RECORD advanced to submission."
  (let ((prepared (plist-put record :step 'submit)))
    ;; The segment transaction persists the advanced retry step, so recovery
    ;; cannot repeat a completed rotation.
    (mevedel-plan--metadata-put session :implementation-retry prepared)
    (condition-case err
        (with-current-buffer chat-buffer
          (require 'mevedel-session-persistence)
          (unless
              (mevedel-session-persistence-start-fresh-segment
               session chat-buffer
               :initial-text
               (or (and (boundp 'gptel-prompt-prefix-alist)
                        (alist-get major-mode gptel-prompt-prefix-alist))
                   ""))
            (error "Could not start a fresh conversation segment"))
          (mevedel--run-session-start-hooks "clear"))
      (error
       (setq prepared (plist-put prepared :step 'prepare-context))
       (mevedel-plan--metadata-put session :implementation-retry prepared)
       (signal (car err) (cdr err))))
    prepared))

(defun mevedel-plan-handoff--start-goal
    (session chat-buffer record accepted target-session target-buffer)
  "Construct RECORD's Goal and durably finish SESSION's Plan handoff."
  (with-current-buffer target-buffer
    (setq-local mevedel-goal-token-budget
                (plist-get record :goal-token-budget))
    (require 'mevedel-goal)
    (mevedel-goal-ensure
     mevedel-plan-handoff--accepted-goal-objective
     target-session
     (plist-get accepted :path)
     (or (plist-get record :goal-id)
         (error "Accepted Goal handoff has no reserved identity"))))
  (mevedel-plan-handoff--goal-handoff-complete session chat-buffer)
  t)

(defun mevedel-plan-handoff--dispatch-submission
    (session chat-buffer record selection accepted target-session
             target-buffer display-text submission)
  "Dispatch one accepted SUBMISSION and settle its durable handoff state."
  (let ((goal-p (eq (plist-get selection :execution) 'goal))
        goal-started)
    (condition-case err
        (progn
          (when goal-p
            (setq goal-started
                  (mevedel-plan-handoff--start-goal
                   session chat-buffer record accepted
                   target-session target-buffer))
            (mevedel-plan-handoff--clear-target-goal-reservation
             target-session target-buffer))
          (with-current-buffer target-buffer
            (mevedel--implement-plan
             (list :permission-mode (plist-get selection :mode)
                   :display-text display-text
                   :prompt-submission submission)))
          (unless goal-p
            (mevedel-plan-handoff--implementation-started
             session chat-buffer)))
      (error
       (let ((reason (error-message-string err)))
         (if goal-started
             (progn
               (mevedel-goal-pause-runtime-failure target-buffer reason)
               (message
                "mevedel: Goal kickoff did not start: %s; resume with /goal resume"
                reason))
           (mevedel-plan-handoff--implementation-failed
            session chat-buffer reason)))))))

(defun mevedel-plan-handoff--submit (session chat-buffer record)
  "Submit prepared implementation RECORD for SESSION from CHAT-BUFFER."
  (let* ((selection (plist-get record :selection))
         (location (plist-get selection :location))
         (context (plist-get selection :context))
         (goal-p (eq (plist-get selection :execution) 'goal)))
    (when (and (eq location 'here)
               (eq context 'summary)
               (not (equal
                     (plist-get record :summary)
                     (with-current-buffer chat-buffer
                       (mevedel--compact-previous-summary)))))
      (error "Prepared plan summary does not match the current segment"))
    (let* ((target-buffer
            (if (eq location 'worktree)
                (mevedel-plan-handoff--worktree-target-buffer record)
              chat-buffer))
           (target-session
            (buffer-local-value 'mevedel--session target-buffer))
           (accepted
            (plist-get record
                       (if (eq location 'worktree)
                           :target-accepted
                         :accepted)))
           (body (mevedel-plan-handoff--accepted-body accepted))
           (prompt
            (if goal-p
                (mevedel-plan-handoff--goal-kickoff-prompt accepted body)
              (mevedel-plan-handoff--implementation-prompt accepted body)))
           (display-text
            (if goal-p
                "Implement accepted plan as Goal"
              "Implement accepted plan"))
           (view-buffer
            (mevedel-view--interaction-target-buffer target-buffer)))
      (with-current-buffer view-buffer
        (mevedel-view--run-prompt-submit-hook
         prompt display-text
         (lambda (submission)
           (mevedel-plan-handoff--dispatch-submission
            session chat-buffer record selection accepted target-session
            target-buffer display-text submission))
         (lambda ()
           (mevedel-plan-handoff--implementation-failed
            session chat-buffer "Prompt submission was blocked")))))))

(defun mevedel-plan-handoff--validate-record (record)
  "Validate durable handoff RECORD and return its selection."
  (let* ((selection (plist-get record :selection))
         (location (plist-get selection :location)))
    (unless (mevedel-plan-handoff-selection-valid-p selection)
      (error "Invalid accepted plan implementation selection"))
    (unless (memq (plist-get record :step)
                  '(prepare-context prepare-summary prepare-worktree
                    prepare-target submit))
      (error "Invalid accepted plan implementation step"))
    (when (eq location 'worktree)
      (let ((branch (plist-get selection :branch)))
        (when (or (not (stringp branch)) (string-empty-p branch))
          (error "Accepted Worktree implementation lacks a branch"))))
    selection))

(defun mevedel-plan-handoff--dispatch-accepted (session chat-buffer)
  "Prepare and dispatch SESSION's accepted plan from CHAT-BUFFER."
  (condition-case err
      (let* ((record
              (copy-tree
               (or (plist-get (mevedel-session-plan-metadata session)
                              :implementation-retry)
                   (error "No accepted plan implementation to retry"))))
             (selection (mevedel-plan-handoff--validate-record record))
             (location (plist-get selection :location))
             (context (plist-get selection :context))
             result)
        (cl-remf record :failure)
        (mevedel-plan--metadata-put session :implementation-retry record)
        (mevedel-plan-handoff--persist session chat-buffer)
        (while record
          (pcase (plist-get record :step)
            ('prepare-worktree
             (unless (eq location 'worktree)
               (error "Invalid accepted plan Worktree step"))
             (setq record
                   (mevedel-plan-handoff--prepare-worktree
                    session chat-buffer record)))
            ('prepare-target
             (unless (eq location 'worktree)
               (error "Invalid accepted plan target step"))
             (setq record
                   (mevedel-plan-handoff--prepare-worktree-target
                    session chat-buffer record)))
            ('prepare-summary
             (unless (eq context 'summary)
               (error "Invalid accepted plan summary step"))
             (setq result
                   (mevedel-plan-handoff--prepare-summary
                    session chat-buffer record)
                   record nil))
            ('prepare-context
             (unless (and (eq location 'here) (eq context 'fresh))
               (error "Invalid accepted plan preparation step"))
             (setq record
                   (mevedel-plan-handoff--prepare-context
                    session chat-buffer record)))
            ('submit
             (setq result
                   (mevedel-plan-handoff--submit
                    session chat-buffer record)
                   record nil))))
        result)
    (error
     (mevedel-plan-handoff--implementation-failed
      session chat-buffer (error-message-string err)))))

(defun mevedel-plan-handoff-start
    (session chat-buffer selection accepted goal-token-budget)
  "Start durable accepted-plan handoff for SESSION from CHAT-BUFFER."
  (unless (mevedel-plan-handoff-selection-valid-p selection)
    (error "Unsupported Plan implementation selection: %S" selection))
  (let ((record
         (mevedel-plan-handoff--implementation-record
          selection accepted goal-token-budget)))
    (mevedel-plan--metadata-put session :selection selection)
    (mevedel-plan--metadata-put session :implementation-retry record)
    (when (eq (plist-get selection :execution) 'goal)
      (mevedel-plan-handoff--persist session chat-buffer))
    (when-let* ((view-buffer
                 (ignore-errors
                   (mevedel-view--interaction-target-buffer chat-buffer)))
                ((fboundp 'mevedel-view--update-spinner)))
      (with-current-buffer view-buffer
        (mevedel-view--update-spinner
         "Preparing implementation..." 'plan-preparation)))
    (when (eq (plist-get selection :location) 'here)
      (with-current-buffer chat-buffer
        (require 'mevedel-permissions)
        (mevedel-permission-mode-transition
         (plist-get selection :mode))))
    (mevedel-plan-handoff--dispatch-accepted session chat-buffer)))

(defun mevedel-retry-plan-implementation (&optional session chat-buffer)
  "Retry SESSION's accepted plan implementation from CHAT-BUFFER."
  (interactive)
  (let* ((session (mevedel-plan-handoff--current-session session))
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
    (mevedel-plan-handoff--dispatch-accepted session chat-buffer)))


(provide 'mevedel-plan-handoff)
;;; mevedel-plan-handoff.el ends here
