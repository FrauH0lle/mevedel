;;; niwa-plan-handoff.el -- Durable accepted-Plan handoff -*- lexical-binding: t -*-

;;; Commentary:

;; Owns accepted-plan preparation, crash-safe recovery, and Direct, Goal or Buddy
;; kickoff.  A Worktree implementation forks the planning session into the
;; worktree and hands it an ordinary Here handoff.  Plan conversation and
;; proposal UI remain in `niwa-plan-mode'.

;;; Code:

(require 'niwa-utilities)

(require 'niwa-engine)

(require 'cl-lib)
(eval-when-compile
  (require 'gptel-request)
  (require 'niwa-structs))
(require 'niwa-plan)
(require 'niwa-implementation-policy)

;; `niwa-chat'
(declare-function niwa--display-chat-buffer "niwa-chat" (chat-buffer))
(declare-function niwa--implement-plan "niwa-chat" (action-plist))
(declare-function niwa--run-session-start-hooks "niwa-chat" (source))
(autoload 'niwa--display-chat-buffer "niwa-chat")

;; `niwa-collaboration'
(declare-function niwa-collaboration-hand-off
                  "niwa-collaboration" (source-data-buffer target-data-buffer))

;; `niwa-compact-evidence'
(declare-function niwa-compact-evidence-previous-summary
                  "niwa-compact-evidence" ())
(autoload 'niwa-compact-evidence-previous-summary "niwa-compact-evidence")

;; `niwa-compact-run'
(declare-function niwa-compact-run-start
                  "niwa-compact-run" (&rest keys))
(defvar niwa-compact-run-cancel)
(autoload 'niwa-compact-run-start "niwa-compact-run")

;; `niwa-compact-target'
(declare-function niwa-compact-target-main-target
                  "niwa-compact-target" ())
(autoload 'niwa-compact-target-main-target "niwa-compact-target")

;; `niwa-goal'
(declare-function niwa-goal-ensure "niwa-goal"
                  (objective session plan-reference id &rest args))
(declare-function niwa-goal-new-id "niwa-goal" nil)
(declare-function niwa-goal-pause-runtime-failure "niwa-goal"
                  (buffer reason))
(autoload 'niwa-goal-ensure "niwa-goal")
(autoload 'niwa-goal-new-id "niwa-goal")
(autoload 'niwa-goal-pause-runtime-failure "niwa-goal")

;; `niwa-mention-bindings'
(declare-function niwa-mention-bindings-ranges
                  "niwa-mention-bindings" (text))
(declare-function niwa-mention-bindings-set
                  "niwa-mention-bindings"
                  (start end binding &optional object))
(autoload 'niwa-mention-bindings-ranges "niwa-mention-bindings")
(autoload 'niwa-mention-bindings-set "niwa-mention-bindings")

;; `niwa-plan'
(declare-function niwa-plan--metadata-put "niwa-plan"
                  (session key value))
(declare-function niwa-plan-archive-accepted "niwa-plan"
                  (artifact session &optional relative-path source-session))
(declare-function niwa-plan-artifact-path-p "niwa-plan" (path))
(declare-function niwa-plan-read-artifact "niwa-plan"
                  (session artifact))
(declare-function niwa-plan-resource-address "niwa-plan"
                  (relative-path))

;; `niwa-presets'
(declare-function niwa-preset-restore-session "niwa-presets"
                  (session &optional buffer))
(autoload 'niwa-preset-restore-session "niwa-presets")

;; `niwa-prompt-submission'
(declare-function niwa-prompt-submission-outcome
                  "niwa-prompt-submission" (cl-x) t)

;; `niwa-execution-target'
(declare-function niwa-execution-target-native-path
                  "niwa-execution-target" (target path))
(autoload 'niwa-execution-target-native-path "niwa-execution-target")

;; `niwa-session-artifacts'
(declare-function niwa-session-artifacts-save
                  "niwa-session-artifacts"
                  (session buffer &optional settled force))
(declare-function niwa-session-artifacts-start-fresh-segment
                  "niwa-session-artifacts"
                  (session buffer &rest keys))
(autoload 'niwa-session-artifacts-save "niwa-session-artifacts")
(autoload 'niwa-session-artifacts-start-fresh-segment
  "niwa-session-artifacts")

;; `niwa-session-fork'
(declare-function niwa-session-fork-create
                  "niwa-session-fork" (buffer target type))
(declare-function niwa-session-fork-retarget-worktree-path
                  "niwa-session-fork" (session path))
(autoload 'niwa-session-fork-create "niwa-session-fork")
(autoload 'niwa-session-fork-retarget-worktree-path "niwa-session-fork")

;; `niwa-session-persistence'
(declare-function niwa-session-persistence-list-sessions
                  "niwa-session-persistence" (workspace &optional cached))
(declare-function niwa-session-persistence-restore
                  "niwa-session-persistence"
                  (session-dir &optional lifecycle-source
                               session-override workspace))
(autoload 'niwa-session-persistence-list-sessions
  "niwa-session-persistence")
(autoload 'niwa-session-persistence-restore "niwa-session-persistence")

;; `niwa-session-rewind'
(declare-function niwa-session-rewind-assert-stable-source
                  "niwa-session-rewind" (session buffer operation))
(autoload 'niwa-session-rewind-assert-stable-source "niwa-session-rewind")

;; `niwa-skills-core'
(declare-function niwa-session-get-skill-by-source
                  "niwa-skills-core" (session source-file))
(declare-function niwa-skill-name "niwa-skills-core" (cl-x) t)
(declare-function niwa-skill-user-invocable-p
                  "niwa-skills-core" (cl-x) t)
(declare-function niwa-skills-skill-enabled-p
                  "niwa-skills-core" (skill))
(autoload 'niwa-session-get-skill-by-source "niwa-skills-core")
(autoload 'niwa-skill-name "niwa-skills-core")
(autoload 'niwa-skill-user-invocable-p "niwa-skills-core")
(autoload 'niwa-skills-skill-enabled-p "niwa-skills-core")

;; `niwa-skills-input'
(declare-function niwa-skills-input-prepare-user-input
                  "niwa-skills-input" (text session))
(autoload 'niwa-skills-input-prepare-user-input "niwa-skills-input")

;; `niwa-structs'
(declare-function niwa-session-current-segment "niwa-structs" (cl-x) t)
(declare-function niwa-session-execution-target
                  "niwa-structs" (cl-x) t)
(declare-function niwa-session-forked-from-session-id
                  "niwa-structs" (cl-x) t)
(declare-function niwa-session-plan-metadata "niwa-structs" (cl-x) t)
(declare-function niwa-session-prompt-index "niwa-structs" (cl-x) t)
(declare-function niwa-session-save-path "niwa-structs" (cl-x) t)
(declare-function niwa-session-session-id "niwa-structs" (cl-x) t)
(declare-function niwa-session-working-directory "niwa-structs" (cl-x) t)
(declare-function niwa-session-workspace "niwa-structs" (cl-x) t)
(defvar niwa--data-buffer)
(defvar niwa--session)

;; `niwa-view-composer'
(declare-function niwa-view--submit-planned-input
                  "niwa-view-composer"
                  (input &optional before-send on-block dispatch after-insert))

;; `niwa-view-interaction'
(declare-function niwa-view--interaction-target-buffer
                  "niwa-view-interaction" (chat-buffer))

;; `niwa-view-stream'
(declare-function niwa-view-stream-end-progress
                  "niwa-view-stream" (view))
(declare-function niwa-view-stream-show-progress
                  "niwa-view-stream" (view status &optional owner))
(autoload 'niwa-view-stream-end-progress "niwa-view-stream")
(autoload 'niwa-view-stream-show-progress "niwa-view-stream")
(defvar niwa-view--spinner-owner)

;; `niwa-worktree'
(declare-function niwa-worktree-fork-reservation
                  "niwa-worktree" (session &optional preflight branch))
(autoload 'niwa-worktree-fork-reservation "niwa-worktree")

(defconst niwa-plan-handoff--accepted-goal-objective
  (concat
   "Implement the accepted plan referenced by this Goal. Read it before acting. "
   "Treat its stated outcomes, constraints, and acceptance criteria as the "
   "completion contract; its implementation mechanics remain revisable. Use "
   "its named validation commands and artifacts to verify progress, and keep "
   "the Goal active until current evidence proves the full contract. Where the "
   "mechanics are silent or conflict with current repository evidence, preserve "
   "the completion contract and use the safest effective implementation.")
  "Deterministic objective for a Goal created from an accepted Plan.")

(defconst niwa-plan-handoff-implementation-modes '(ask edits full-auto)
  "Permission modes supported by accepted Plan handoffs.")


;;
;;; Durable handoff

(defun niwa-plan-handoff--current-session (&optional session)
  "Return SESSION or the session reachable from the current buffer."
  (or session
      (and (boundp 'niwa--session) niwa--session)
      (and (boundp 'niwa--data-buffer)
           (buffer-live-p niwa--data-buffer)
           (buffer-local-value 'niwa--session niwa--data-buffer))))

(defun niwa-plan-handoff-reserved-goal-id (&optional session)
  "Return SESSION's Goal kickoff reservation, or nil.
Only a Here handoff reserves one: a Worktree handoff hands the Goal to the
worktree fork, which runs its own Here handoff."
  (when-let* ((session (niwa-plan-handoff--current-session session)))
    (let* ((retry (plist-get (niwa-session-plan-metadata session)
                             :implementation-retry))
           (selection (plist-get retry :selection)))
      (and (eq (plist-get selection :execution) 'goal)
           (eq (plist-get selection :location) 'here)
           (plist-get retry :goal-id)))))

(defun niwa-plan-handoff-selection-valid-p (selection)
  "Return non-nil when SELECTION is a supported Plan handoff selection."
  (and (proper-list-p selection)
       (let ((location (plist-get selection :location))
             (context (plist-get selection :context))
             (budget (plist-get selection :goal-token-budget))
             (skills (plist-get selection :skills))
             (instructions (plist-get selection :instructions))
             (provider (plist-get selection :model-provider))
             (effort (plist-get selection :reasoning-effort)))
         (and (memq location '(here worktree))
              (memq context '(current fresh summary))
              (memq (plist-get selection :execution) '(direct goal buddy))
              (memq (plist-get selection :mode)
                    niwa-plan-handoff-implementation-modes)
              (or (null budget)
                  (and (integerp budget) (> budget 0)))
              (or (null skills)
                  (and
                   (proper-list-p skills)
                   (cl-every
                    (lambda (skill)
                      (and (proper-list-p skill)
                           (stringp (plist-get skill :name))
                           (file-name-absolute-p
                            (or (plist-get skill :source-file) ""))))
                    skills)))
              (or (null instructions) (stringp instructions))
              (or (null provider) (stringp provider))
              (or (null effort) (symbolp effort))))))

(defun niwa-plan-handoff-append-implementation-input
    (prompt selection)
  "Append SELECTION's skills and instructions to PROMPT."
  (let ((result prompt))
    (when-let* ((skills (plist-get selection :skills)))
      (setq result (concat result "\n\nImplementation skills:\n"))
      (dolist (skill skills)
        (let* ((token (concat "$" (plist-get skill :name)))
               (start (+ (length result) 4)))
          (setq result
                (concat result "Use " token " during implementation.\n"))
          (niwa-mention-bindings-set
           start (+ start (length token))
           (list :kind 'skill :token token
                 :source-file (plist-get skill :source-file))
           result))))
    (when-let* ((instructions (plist-get selection :instructions)))
      (setq result
            (concat result
                    "\n\nAdditional implementation instructions:\n"
                    instructions)))
    result))

(defun niwa-plan-handoff--implementation-prompt
    (accepted-artifact plan-markdown &optional selection)
  "Return the Direct or Buddy prompt for ACCEPTED-ARTIFACT and PLAN-MARKDOWN.
SELECTION chooses execution and supplies implementation-only input."
  (niwa-plan-handoff-append-implementation-input
   (format
    "Accepted plan artifact: %s\n\nAccepted plan:\n%s\n\nImplementation instructions:\n%s"
    (niwa-plan-resource-address
     (plist-get accepted-artifact :path))
    plan-markdown
    (if (eq (plist-get selection :execution) 'buddy)
        (concat
         "The user will implement this plan themselves. Read the accepted plan "
         "and relevant code, then create one note per plan step as concise "
         "instructions for a human, with the step as its text. Anchor each note "
         "to the right file and lines. Use one NoteWrite batch of create "
         "operations so the store syncs once. If a step has no clear code "
         "location, anchor it to the most relevant file you read and explain "
         "that choice in your reply. Leave all notes unsent; the "
         "user decides whether to take, send, edit or delete each one. "
         "Do not edit code. Do not prepare changes; omit prepared_change from "
         "every create operation. Do not send, take or close notes. "
         "After creating the notes, summarize their numbers and corresponding "
         "steps, and tell the user that the notes list shows them.")
      "Implement the accepted plan against the current repository state. Preserve its stated outcomes and acceptance criteria while using repository evidence to choose the safest effective mechanics."))
   selection))

(defun niwa-plan-handoff--goal-kickoff-prompt
    (accepted-artifact plan-markdown &optional selection)
  "Return the Goal kickoff for ACCEPTED-ARTIFACT and PLAN-MARKDOWN."
  (niwa-plan-handoff-append-implementation-input
   (format
    "Accepted plan artifact: %s\n\nAccepted plan:\n%s\n\nGoal kickoff:\nBegin the active Goal. Read the accepted plan supplied above before acting."
    (niwa-plan-resource-address
     (plist-get accepted-artifact :path))
    plan-markdown)
   selection))

(defun niwa-plan-handoff-validate-skill-bindings (prompt session)
  "Signal when an explicit skill binding in PROMPT is unavailable in SESSION."
  (dolist (range (niwa-mention-bindings-ranges prompt))
    (let* ((binding (plist-get range :binding))
           (source (and (eq (plist-get binding :kind) 'skill)
                        (plist-get binding :source-file)))
           (skill (and source
                       (niwa-session-get-skill-by-source session source))))
      (when source
        (unless (and skill
                     (niwa-skills-skill-enabled-p skill)
                     (niwa-skill-user-invocable-p skill))
          (error "Implementation skill %s is unavailable"
                 (plist-get binding :token)))))))

(defun niwa-plan-handoff--implementation-record (selection accepted)
  "Return retry state for SELECTION and ACCEPTED artifact."
  (let ((record
         (list :step (niwa-plan-handoff--initial-step selection)
               :selection (copy-tree selection)
               :accepted
               (list :path (plist-get accepted :path)
                     :hash (plist-get accepted :hash)))))
    (when (eq (plist-get selection :execution) 'goal)
      (setq record (plist-put record :goal-id (niwa-goal-new-id))))
    record))

(defun niwa-plan-handoff--initial-step (selection)
  "Return the first preparation step for SELECTION."
  (if (eq (plist-get selection :location) 'worktree)
      'prepare-worktree
    (pcase (plist-get selection :context)
      ('summary 'prepare-summary)
      ('fresh 'prepare-context)
      (_ 'submit))))

(defun niwa-plan-handoff-assert-worktree-ready (session buffer)
  "Signal unless SESSION in BUFFER can fork into a worktree now.
Checked before acceptance, so a session that cannot fork keeps its proposal."
  (niwa-session-rewind-assert-stable-source
   session buffer "implementing in a worktree"))

(defun niwa-plan-handoff--latest-fork-point-id (session buffer)
  "Return the fork point of SESSION's latest settled reply in BUFFER.
Saving first indexes the fork point recorded when the reply settled."
  (niwa-session-artifacts-save session buffer)
  (or (plist-get
       (car (last (cdr (assoc (or (niwa-session-current-segment session) 1)
                              (niwa-session-prompt-index session)))))
       :fork-point-id)
      (error "The accepted plan's reply has no fork point")))

(defun niwa-plan-handoff--find-fork (session directory)
  "Return the save path of SESSION's unique saved fork at DIRECTORY, or nil."
  (let* ((target (niwa-session-execution-target session))
         (native (directory-file-name
                  (if target
                      (niwa-execution-target-native-path target directory)
                    directory)))
         (matches
          (cl-remove-if-not
           (lambda (entry)
             (let ((summary (plist-get entry :summary)))
               (and (equal (plist-get summary :forked-from-session-id)
                           (niwa-session-session-id session))
                    (stringp (plist-get summary :working-directory))
                    (equal native (directory-file-name
                                   (plist-get summary :working-directory))))))
           (niwa-session-persistence-list-sessions
            (niwa-session-workspace session)))))
    (when (cdr matches)
      (error "Several sessions were forked into %s" directory))
    (plist-get (car matches) :save-path)))

(defun niwa-plan-handoff--open-fork (session directory save-path)
  "Open SESSION's fork saved at SAVE-PATH, which must be rooted at DIRECTORY."
  (let* ((buffer (niwa-session-persistence-restore
                  save-path nil nil (niwa-session-workspace session)))
         (fork (buffer-local-value 'niwa--session buffer)))
    (unless (and (equal (niwa-session-forked-from-session-id fork)
                        (niwa-session-session-id session))
                 (file-equal-p (niwa-session-working-directory fork)
                               directory))
      (error "The prepared worktree session does not belong to this plan"))
    buffer))

(defun niwa-plan-handoff--worktree-fork (session chat-buffer record)
  "Return RECORD's worktree fork of SESSION, creating it at most once.
A fork published before an interruption is found again by its recorded
save path, or, when even that was not saved, by its directory and parent."
  (let* ((reservation (plist-get record :worktree-reservation))
         (directory (plist-get reservation :directory))
         (save-path (or (plist-get record :target-save-path)
                        (niwa-plan-handoff--find-fork session directory))))
    (if save-path
        (niwa-plan-handoff--open-fork session directory save-path)
      (niwa-session-fork-create
       chat-buffer
       (list :fork-point-id (plist-get record :fork-point-id)
             :worktree-reservation reservation)
       'worktree))))

(defun niwa-plan-handoff--install-in-fork (session record fork fork-buffer)
  "Give FORK RECORD's accepted plan and an equivalent Here handoff.
Return the fork's retry record.  A plan installed by an earlier attempt,
recognized by its hash, is not installed again; its retry record is
returned as it now is, nil when the fork already started it."
  (let ((metadata (niwa-session-plan-metadata fork))
        (hash (plist-get (plist-get record :accepted) :hash)))
    (if (equal hash (plist-get metadata :accepted-hash))
        (plist-get metadata :implementation-retry)
      (let* ((accepted (niwa-plan-archive-accepted
                        (plist-get record :accepted) fork nil session))
             (selection (copy-tree (plist-get record :selection)))
             prepared)
        (cl-remf selection :branch)
        (setq selection (plist-put selection :location 'here))
        ;; Repository-local skills live in the fork's own checkout now.
        (setq selection
              (plist-put
               selection :skills
               (mapcar (lambda (skill)
                         (plist-put (copy-sequence skill) :source-file
                                    (niwa-session-fork-retarget-worktree-path
                                     fork (plist-get skill :source-file))))
                       (plist-get selection :skills))))
        (setq prepared
              (append
               (list :step (niwa-plan-handoff--initial-step selection)
                     :selection selection
                     :accepted (list :path (plist-get accepted :path)
                                     :hash (plist-get accepted :hash)))
               (when-let* ((goal-id (plist-get record :goal-id)))
                 (list :goal-id goal-id))))
        (setf (niwa-session-plan-metadata fork)
              (list :status 'accepted
                    :accepted-path (plist-get accepted :path)
                    :accepted-hash (plist-get accepted :hash)
                    :selection selection
                    :implementation-retry prepared))
        (niwa-session-artifacts-save fork fork-buffer)
        prepared))))

(defun niwa-plan-handoff--hand-over (chat-buffer fork-buffer)
  "Show FORK-BUFFER's view where CHAT-BUFFER's was, then close CHAT-BUFFER.
The planning session is saved and finished; its conversation continues in
the fork.  Closing waits for the accepting command to return."
  (let ((source-view (buffer-local-value 'niwa--view-buffer chat-buffer))
        (fork-view (or (buffer-local-value 'niwa--view-buffer fork-buffer)
                       fork-buffer))
        windows)
    (when (buffer-live-p source-view)
      (setq windows (get-buffer-window-list source-view nil t))
      (dolist (window windows)
        (set-window-buffer window fork-view)))
    (unless windows
      (niwa--display-chat-buffer fork-buffer))
    ;; Sharing is optional; a shared source necessarily loaded its owner.
    ;; Deliver fresh continuation credentials before buffer teardown revokes it.
    (when (featurep 'niwa-collaboration)
      (niwa-collaboration-hand-off chat-buffer fork-buffer))
    (run-at-time
     0 nil
     (lambda ()
       ;; Killing a view kills its data buffer; kill both in case either
       ;; outlives the other.
       (dolist (buffer (list source-view chat-buffer))
         (when (buffer-live-p buffer)
           (kill-buffer buffer)))))))

(defun niwa-plan-handoff--prepare-worktree (session chat-buffer record)
  "Fork SESSION into RECORD's worktree and hand the implementation to it.
The fork carries the planning conversation; it receives the accepted plan
and runs the selected context and execution as an ordinary Here handoff.
Each durable step is recorded before the next, so a retry neither forks
twice nor installs the plan twice.  Return nil: the fork continues alone."
  (let ((prepared (copy-tree record)))
    (unless (plist-get prepared :fork-point-id)
      (setq prepared (plist-put prepared :fork-point-id
                                (niwa-plan-handoff--latest-fork-point-id
                                 session chat-buffer))))
    (unless (plist-get prepared :worktree-reservation)
      (setq prepared
            (plist-put prepared :worktree-reservation
                       (with-current-buffer chat-buffer
                         (niwa-worktree-fork-reservation
                          session nil
                          (plist-get (plist-get prepared :selection) :branch)))))
      (niwa-plan--metadata-put session :implementation-retry prepared)
      (niwa-session-artifacts-save session chat-buffer))
    (let* ((fork-buffer (niwa-plan-handoff--worktree-fork
                         session chat-buffer prepared))
           (fork (buffer-local-value 'niwa--session fork-buffer)))
      (unless (plist-get prepared :target-save-path)
        (setq prepared (plist-put prepared :target-save-path
                                  (niwa-session-save-path fork)))
        (niwa-plan--metadata-put session :implementation-retry prepared)
        (niwa-session-artifacts-save session chat-buffer))
      (let ((fork-record (niwa-plan-handoff--install-in-fork
                          session prepared fork fork-buffer))
            (metadata (copy-sequence (niwa-session-plan-metadata session))))
        ;; The fork owns the implementation now; the source keeps its archive.
        (cl-remf metadata :implementation-retry)
        (setf (niwa-session-plan-metadata session) metadata)
        (niwa-session-artifacts-save session chat-buffer)
        (niwa-plan-handoff--hand-over chat-buffer fork-buffer)
        (when fork-record
          (niwa-plan-handoff--begin fork fork-buffer))
        nil))))

(defun niwa-plan-handoff--summary-focus (plan selection)
  "Return exact PLAN and SELECTION instructions as relevance focus."
  (format
   "Accepted plan (authoritative; relevance only):\n%s\n\nImplementation-only instructions (authoritative; relevance only):\n%s"
   plan
   (or (plist-get selection :instructions) "(none)")))

(defun niwa-plan-handoff--summary-source
    (source previous-summary plan)
  "Return SOURCE with PREVIOUS-SUMMARY as evidence and PLAN omitted."
  (string-replace
   plan
   "[niwa accepted-plan omission: supplied separately as authoritative focus]"
   (if previous-summary
       (concat
        "--- evidence item; provenance: prior-continuation-summary ---\n"
        previous-summary
        "\n--- end evidence item ---\n\n"
        source)
     source)))

(defun niwa-plan-handoff--advance-record
    (session record step &optional summary)
  "Store RECORD advanced to STEP as SESSION's retry state and return it.
SUMMARY, when given, becomes the cached handoff summary.  Any recorded
failure is cleared, because reaching STEP supersedes it."
  (let ((prepared (copy-tree record)))
    (when summary
      (setq prepared (plist-put prepared :summary summary)))
    (setq prepared (plist-put prepared :step step))
    (cl-remf prepared :failure)
    (niwa-plan--metadata-put session :implementation-retry prepared)
    prepared))

(defun niwa-plan-handoff--prepare-here-summary
    (session chat-buffer record target focus source-transform)
  "Aggressively compact CHAT-BUFFER into a Here handoff for SESSION RECORD.
TARGET is the active main compaction target; its apply step is wrapped so
each transition is recorded before it runs.  FOCUS carries the
authoritative plan; SOURCE-TRANSFORM filters the projected evidence."
  (let ((apply-function (plist-get target :apply)))
    (setq target
          (plist-put
           target :apply
           (lambda (active-target summary &rest args)
             (niwa-plan-handoff--advance-record
              session record 'submit summary)
             (condition-case apply-error
                 (apply apply-function active-target summary args)
               (error
                (niwa-plan-handoff--advance-record
                 session record 'prepare-summary summary)
                (signal (car apply-error) (cdr apply-error)))))))
    (niwa-compact-run-start
     :aggressive t
     :focus focus
     :prepared-summary (plist-get record :summary)
     :purpose 'handoff
     :source-transform source-transform
     :summary-ready
     (lambda (summary)
       (niwa-plan-handoff--advance-record
        session record 'prepare-summary summary)
       (niwa-session-artifacts-save session chat-buffer)
       summary)
     :target target
     :callback
     (lambda (err)
       (if err
           (niwa-plan-handoff--implementation-failed
            session chat-buffer err)
         (niwa-plan-handoff--dispatch-accepted session chat-buffer))))))

(defun niwa-plan-handoff--prepare-summary (session chat-buffer record)
  "Compact CHAT-BUFFER into the implementation handoff for SESSION RECORD.
Runs under the ordinary compaction retry policy."
  (with-current-buffer chat-buffer
    (let* ((selection (plist-get record :selection))
           (plan
            (niwa-plan-read-artifact
             session (plist-get record :accepted)))
           (target (niwa-compact-target-main-target))
           (previous-summary (plist-get target :previous-summary))
           (focus (niwa-plan-handoff--summary-focus plan selection))
           (source-transform
            (lambda (source)
              (niwa-plan-handoff--summary-source
               source previous-summary plan))))
      (niwa-plan-handoff--prepare-here-summary
       session chat-buffer record target focus source-transform))))

(defconst niwa-plan-handoff--failure-reason-limit 500
  "Maximum characters of a failure reason kept in a durable retry record.")

(defun niwa-plan-handoff--failure-reason (reason)
  "Return REASON, a string or error value, as bounded text.
Error data can carry whole requests or sessions.  Printing them unbounded
would copy transcripts into the retry record and the echo area."
  (let* ((print-level 3)
         (print-length 8)
         (text (cond
                ((stringp reason) reason)
                ((and (consp reason) (symbolp (car reason))
                      (get (car reason) 'error-conditions))
                 (error-message-string reason))
                (t (format "%s" reason)))))
    (truncate-string-to-width
     text niwa-plan-handoff--failure-reason-limit nil nil "...")))

(defun niwa-plan-handoff--implementation-failed
    (session chat-buffer reason)
  "Keep SESSION retryable after REASON from CHAT-BUFFER.
REASON is a string or an error value; it is recorded in bounded form."
  (setq reason (niwa-plan-handoff--failure-reason reason))
  (niwa-view-stream-end-progress
   (ignore-errors
     (niwa-view--interaction-target-buffer chat-buffer)))
  (when-let* ((record (copy-tree
                       (plist-get (niwa-session-plan-metadata session)
                                  :implementation-retry))))
    (setq record (plist-put record :failure reason))
    (niwa-plan--metadata-put session :implementation-retry record)
    (condition-case err
        (niwa-session-artifacts-save session chat-buffer)
      (error
       (niwa-diagnostic-warning (niwa-diagnostic-scope session)
        'niwa
        (format "%s (could not persist plan implementation retry)"
                (niwa-plan-handoff--failure-reason err))))))
  (niwa-diagnostic-message (niwa-diagnostic-scope session)
   "%s (implementation failed); retry with M-x niwa-retry-plan-implementation"
   reason)
  nil)

(defun niwa-plan-handoff--implementation-request-started
    (owner chat-buffer)
  "Attach CHAT-BUFFER's Direct handoff recovery to turn OWNER.
OWNER is a native gptel state machine or an admitted external request."
  (let ((info (copy-sequence (or (niwa-engine-info owner) nil))))
    (setq info (plist-put info :niwa-plan-handoff-diagnostic-scope
                          (niwa-diagnostic-scope chat-buffer)))
    (setf (niwa-engine-info owner)
          (plist-put info :niwa-plan-handoff-source-buffer chat-buffer)))
  owner)

(defun niwa-plan-handoff--implementation-completed
    (session chat-buffer)
  "Durably clear SESSION's Direct retry after terminal success."
  (let* ((old-metadata (niwa-session-plan-metadata session))
         (metadata (copy-sequence (or old-metadata nil))))
    (cl-remf metadata :implementation-retry)
    (setf (niwa-session-plan-metadata session) metadata)
    (condition-case err
        (niwa-session-artifacts-save session chat-buffer)
      (error
       (setf (niwa-session-plan-metadata session) old-metadata)
       (niwa-diagnostic-warning (niwa-diagnostic-scope session)
        'niwa
        (format "%s (could not persist completed plan implementation)"
                (niwa-plan-handoff--failure-reason err)))))))

(defun niwa-plan-handoff-settle-request (fsm status &optional reason)
  "Settle FSM's attached Direct Plan handoff with terminal STATUS.

STATUS is `success', `error', or `aborted'.  Optional REASON supplies the
provider or abort detail retained for a retryable failure."
  (let* ((info (copy-sequence (or (niwa-engine-info fsm) nil)))
         (diagnostic-scope (plist-get info :niwa-plan-handoff-diagnostic-scope))
         (chat-buffer
          (plist-get info :niwa-plan-handoff-source-buffer)))
    (when (plist-member info :niwa-plan-handoff-source-buffer)
      (cl-remf info :niwa-plan-handoff-source-buffer)
      (cl-remf info :niwa-plan-handoff-diagnostic-scope)
      (setf (niwa-engine-info fsm) info)
      (if (not (buffer-live-p chat-buffer))
          (niwa-diagnostic-warning diagnostic-scope
           'niwa
           "Plan implementation settled after its source buffer was killed; \
the durable retry was retained"
           :warning)
        (with-current-buffer chat-buffer
          (when (bound-and-true-p niwa--session)
            (if (eq status 'success)
                (niwa-plan-handoff--implementation-completed
                 niwa--session chat-buffer)
              (niwa-plan-handoff--implementation-failed
               niwa--session chat-buffer
               (or reason
                   (if (eq status 'aborted)
                       "Request aborted"
                     "Provider request failed"))))))))))

(defun niwa-plan-handoff--goal-handoff-complete (session chat-buffer)
  "Durably clear SESSION's Plan retry before Goal kickoff from CHAT-BUFFER."
  (let* ((old-metadata (niwa-session-plan-metadata session))
         (metadata (copy-sequence (or old-metadata nil))))
    (cl-remf metadata :implementation-retry)
    (setf (niwa-session-plan-metadata session) metadata)
    (condition-case err
        (niwa-session-artifacts-save session chat-buffer)
      (error
       (setf (niwa-session-plan-metadata session) old-metadata)
       (signal (car err) (cdr err))))))

(defun niwa-plan-handoff--prepare-context (session chat-buffer record)
  "Rotate CHAT-BUFFER once and return RECORD advanced to submission."
  (let ((prepared (plist-put record :step 'submit)))
    ;; The segment transaction persists the advanced retry step, so recovery
    ;; cannot repeat a completed rotation.
    (niwa-plan--metadata-put session :implementation-retry prepared)
    (condition-case err
        (with-current-buffer chat-buffer
          (unless
              (niwa-session-artifacts-start-fresh-segment
               session chat-buffer)
            (error "Could not start a fresh conversation segment"))
          (niwa--run-session-start-hooks "clear"))
      (error
       (setq prepared (plist-put prepared :step 'prepare-context))
       (niwa-plan--metadata-put session :implementation-retry prepared)
       (signal (car err) (cdr err))))
    prepared))

(defun niwa-plan-handoff--start-goal (record accepted session chat-buffer)
  "Construct RECORD's durable Goal in SESSION and return its exact owner."
  (with-current-buffer chat-buffer
    (niwa-goal-ensure
     niwa-plan-handoff--accepted-goal-objective
     session
     (plist-get accepted :path)
     (or (plist-get record :goal-id)
         (error "Accepted Goal handoff has no reserved identity"))
     :implementation-policy
     (niwa-implementation-policy-from-selection (plist-get record :selection))
     :token-budget (plist-get (plist-get record :selection) :goal-token-budget))))

(defun niwa-plan-handoff--dispatch-submission
    (session chat-buffer record selection accepted display-text submission)
  "Dispatch one accepted SUBMISSION and settle its durable handoff state."
  (let ((goal-p (eq (plist-get selection :execution) 'goal))
        (goal-id (plist-get record :goal-id))
        (policy (copy-tree (niwa-implementation-policy-from-selection selection)))
        goal-started
        request-fsm)
    (condition-case err
        (progn
          ;; Release only our preparation status before transferring
          ;; ownership to the implementation request.
          (when-let* ((view-buffer (buffer-local-value 'niwa--view-buffer
                                                       chat-buffer))
                      ((buffer-live-p view-buffer)))
            (with-current-buffer view-buffer
              ;; Only a loaded stream module can own a spinner at all.
              (when (eq (bound-and-true-p niwa-view--spinner-owner)
                        'plan-preparation)
                (niwa-view--stop-spinner))))
          (when goal-p
            (setq goal-started
                  (niwa-plan-handoff--start-goal
                   record accepted session chat-buffer))
            ;; Keep the exact owner before publication can yield.
            (niwa-plan-handoff--goal-handoff-complete session chat-buffer))
          (with-current-buffer chat-buffer
            (setq request-fsm
                  (niwa--implement-plan
                   (list :implementation-policy
                         policy
                         :implementation-goal goal-started
                         :implementation-goal-id (and goal-p goal-id)
                         :display-text display-text
                         :prompt-submission submission
                         :prepared-outcome
                         (niwa-prompt-submission-outcome submission)))))
          (unless goal-p
            (niwa-plan-handoff--implementation-request-started
             request-fsm chat-buffer)))
      (error
       (let ((reason (niwa-plan-handoff--failure-reason err)))
         (if goal-started
             (progn
               (when (and (not (eq (car err) 'niwa-stale-implementation-preparation))
                          (buffer-live-p chat-buffer)
                          (eq session (buffer-local-value 'niwa--session chat-buffer))
                          (eq goal-started (niwa-session-goal session))
                          (equal goal-id (niwa-goal-id goal-started))
                          (equal policy (niwa-goal-implementation-policy goal-started)))
                 (niwa-goal-pause-runtime-failure chat-buffer reason))
               (niwa-diagnostic-message (niwa-diagnostic-scope session)
                "%s (Goal kickoff did not start); inspect the current Goal before resuming"
                reason))
           (niwa-plan-handoff--implementation-failed
            session chat-buffer reason)))))))

(defun niwa-plan-handoff--submit (session chat-buffer record)
  "Submit prepared implementation RECORD for SESSION from CHAT-BUFFER."
  (let* ((selection (plist-get record :selection))
         (goal-p (eq (plist-get selection :execution) 'goal)))
    (when (and (eq (plist-get selection :context) 'summary)
               (not (equal
                     (plist-get record :summary)
                     (with-current-buffer chat-buffer
                       (niwa-compact-evidence-previous-summary)))))
      (error "Prepared plan summary does not match the current segment"))
    (let* ((accepted (plist-get record :accepted))
           (body (niwa-plan-read-artifact session accepted))
           (prompt
            (if goal-p
                (niwa-plan-handoff--goal-kickoff-prompt
                 accepted body selection)
              (niwa-plan-handoff--implementation-prompt
               accepted body selection)))
           (display-text
            (pcase (plist-get selection :execution)
              ('goal "Implement accepted plan as Goal")
              ('buddy "Implement myself with notes")
              (_ "Implement accepted plan")))
           (view-buffer
            (niwa-view--interaction-target-buffer chat-buffer)))
      (with-current-buffer chat-buffer
        (setq prompt
              (niwa-skills-input-prepare-user-input prompt session))
        (niwa-plan-handoff-validate-skill-bindings prompt session))
      (with-current-buffer view-buffer
        (niwa-view--submit-planned-input
         prompt nil
         (lambda ()
           (niwa-plan-handoff--implementation-failed
            session chat-buffer "Prompt submission was blocked"))
         (lambda (submission)
           (niwa-plan-handoff--dispatch-submission
            session chat-buffer record selection accepted display-text
            submission)))))))

(defun niwa-plan-handoff--validate-record (record)
  "Validate durable handoff RECORD and return its selection."
  (let* ((selection (plist-get record :selection))
         (location (plist-get selection :location))
         (accepted (plist-get record :accepted)))
    (unless (niwa-plan-handoff-selection-valid-p selection)
      (error "Invalid accepted plan implementation selection"))
    (unless (and (stringp (plist-get selection :model-provider))
                 (string-match-p
                  "\\`[^:]+:.+\\'"
                  (plist-get selection :model-provider)))
      (error "Accepted plan implementation lacks a model snapshot"))
    (niwa-implementation-policy-from-selection selection)
    (unless (memq (plist-get record :step)
                  '(prepare-context prepare-summary prepare-worktree submit))
      (error "Invalid accepted plan implementation step"))
    (unless accepted
      (error "Accepted plan implementation lacks an artifact"))
    (unless (and (niwa-plan-artifact-path-p (plist-get accepted :path))
                 (stringp (plist-get accepted :hash))
                 (not (plist-member accepted :absolute-path))
                 (not (plist-member accepted :accepted-absolute-path)))
      (error "Invalid accepted plan artifact"))
    (when (eq location 'worktree)
      (let ((branch (plist-get selection :branch)))
        (when (or (not (stringp branch)) (string-empty-p branch))
          (error "Accepted Worktree implementation lacks a branch"))))
    selection))

(defun niwa-plan-handoff--dispatch-accepted (session chat-buffer)
  "Prepare and dispatch SESSION's accepted plan from CHAT-BUFFER."
  (condition-case err
      (let* ((record
              (copy-tree
               (or (plist-get (niwa-session-plan-metadata session)
                              :implementation-retry)
                   (error "No accepted plan implementation to retry"))))
             (selection (niwa-plan-handoff--validate-record record))
             (location (plist-get selection :location))
             (context (plist-get selection :context))
             result)
        (cl-remf record :failure)
        (niwa-plan--metadata-put session :implementation-retry record)
        (niwa-session-artifacts-save session chat-buffer)
        (while record
          (pcase (plist-get record :step)
            ('prepare-worktree
             (unless (eq location 'worktree)
               (error "Invalid accepted plan Worktree step"))
             (setq record
                   (niwa-plan-handoff--prepare-worktree
                    session chat-buffer record)))
            ('prepare-summary
             (unless (eq context 'summary)
               (error "Invalid accepted plan summary step"))
             (setq result
                   (niwa-plan-handoff--prepare-summary
                    session chat-buffer record)
                   record nil))
            ('prepare-context
             (unless (and (eq location 'here) (eq context 'fresh))
               (error "Invalid accepted plan preparation step"))
             (setq record
                   (niwa-plan-handoff--prepare-context
                    session chat-buffer record)))
            ('submit
             (setq result
                   (niwa-plan-handoff--submit
                    session chat-buffer record)
                   record nil))))
        result)
    (error
     (niwa-plan-handoff--implementation-failed
      session chat-buffer err))))

(defun niwa-plan-handoff-start
    (session chat-buffer selection accepted)
  "Start durable accepted-plan handoff for SESSION from CHAT-BUFFER."
  (unless (niwa-plan-handoff-selection-valid-p selection)
    (error "Unsupported Plan implementation selection: %S" selection))
  (let ((record
         (niwa-plan-handoff--implementation-record
          selection accepted)))
    (niwa-plan--metadata-put session :selection selection)
    (niwa-plan--metadata-put session :implementation-retry record)
    (condition-case err
        (progn
          (when (eq (plist-get selection :execution) 'goal)
            (niwa-session-artifacts-save session chat-buffer))
          (when (eq (plist-get selection :location) 'worktree)
            ;; Fork at the reply that proposed this plan, whatever follows.
            (niwa-plan--metadata-put
             session :implementation-retry
             (plist-put record :fork-point-id
                        (niwa-plan-handoff--latest-fork-point-id
                         session chat-buffer))))
          (niwa-plan-handoff--begin session chat-buffer))
      (error
       (niwa-plan-handoff--implementation-failed
        session chat-buffer err)))))

(defun niwa-plan-handoff--begin (session chat-buffer)
  "Show preparation progress and dispatch SESSION's handoff from CHAT-BUFFER."
  (niwa-view-stream-show-progress
   (ignore-errors
     (niwa-view--interaction-target-buffer chat-buffer))
   "Preparing implementation..." 'plan-preparation)
  (niwa-plan-handoff--dispatch-accepted session chat-buffer))

(defun niwa-retry-plan-implementation (&optional session chat-buffer)
  "Retry SESSION's accepted plan implementation from CHAT-BUFFER."
  (interactive)
  (let* ((session (niwa-plan-handoff--current-session session))
         (chat-buffer
          (or chat-buffer
              (and (boundp 'niwa--data-buffer)
                   (buffer-live-p niwa--data-buffer)
                   niwa--data-buffer)
              (current-buffer))))
    (unless (and session
                 (plist-get (niwa-session-plan-metadata session)
                            :implementation-retry))
      (niwa-diagnostic-user-error (niwa-diagnostic-scope session)
                                    "No accepted plan implementation to retry"))
    (niwa-plan-handoff--dispatch-accepted session chat-buffer)))


(provide 'niwa-plan-handoff)
;;; niwa-plan-handoff.el ends here
