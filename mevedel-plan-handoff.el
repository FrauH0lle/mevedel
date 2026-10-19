;;; mevedel-plan-handoff.el -- Durable accepted-Plan handoff -*- lexical-binding: t -*-

;;; Commentary:

;; Owns accepted-plan preparation, crash-safe recovery, and Direct or Goal
;; kickoff.  A Worktree implementation forks the planning session into the
;; worktree and hands it an ordinary Here handoff.  Plan conversation and
;; proposal UI remain in `mevedel-plan-mode'.

;;; Code:

(require 'mevedel-engine)

(require 'cl-lib)
(eval-when-compile
  (require 'gptel-request)
  (require 'mevedel-structs))
(require 'mevedel-plan)
(require 'mevedel-implementation-policy)

;; `mevedel-chat'
(declare-function mevedel--display-chat-buffer "mevedel-chat" (chat-buffer))
(declare-function mevedel--implement-plan "mevedel-chat" (action-plist))
(declare-function mevedel--run-session-start-hooks "mevedel-chat" (source))
(autoload 'mevedel--display-chat-buffer "mevedel-chat")

;; `mevedel-collaboration'
(declare-function mevedel-collaboration-hand-off
                  "mevedel-collaboration" (source-data-buffer target-data-buffer))

;; `mevedel-compact-evidence'
(declare-function mevedel-compact-evidence-previous-summary
                  "mevedel-compact-evidence" ())
(autoload 'mevedel-compact-evidence-previous-summary "mevedel-compact-evidence")

;; `mevedel-compact-run'
(declare-function mevedel-compact-run-start
                  "mevedel-compact-run" (&rest keys))
(defvar mevedel-compact-run-cancel)
(autoload 'mevedel-compact-run-start "mevedel-compact-run")

;; `mevedel-compact-target'
(declare-function mevedel-compact-target-main-target
                  "mevedel-compact-target" ())
(autoload 'mevedel-compact-target-main-target "mevedel-compact-target")

;; `mevedel-goal'
(declare-function mevedel-goal-ensure "mevedel-goal"
                  (objective session plan-reference id &rest args))
(declare-function mevedel-goal-new-id "mevedel-goal" nil)
(declare-function mevedel-goal-pause-runtime-failure "mevedel-goal"
                  (buffer reason))
(autoload 'mevedel-goal-ensure "mevedel-goal")
(autoload 'mevedel-goal-new-id "mevedel-goal")
(autoload 'mevedel-goal-pause-runtime-failure "mevedel-goal")

;; `mevedel-mention-bindings'
(declare-function mevedel-mention-bindings-ranges
                  "mevedel-mention-bindings" (text))
(declare-function mevedel-mention-bindings-set
                  "mevedel-mention-bindings"
                  (start end binding &optional object))
(autoload 'mevedel-mention-bindings-ranges "mevedel-mention-bindings")
(autoload 'mevedel-mention-bindings-set "mevedel-mention-bindings")

;; `mevedel-plan'
(declare-function mevedel-plan--metadata-put "mevedel-plan"
                  (session key value))
(declare-function mevedel-plan-archive-accepted "mevedel-plan"
                  (artifact session &optional relative-path source-session))
(declare-function mevedel-plan-artifact-path-p "mevedel-plan" (path))
(declare-function mevedel-plan-read-artifact "mevedel-plan"
                  (session artifact))
(declare-function mevedel-plan-resource-address "mevedel-plan"
                  (relative-path))

;; `mevedel-presets'
(declare-function mevedel-preset-restore-session "mevedel-presets"
                  (session &optional buffer))
(autoload 'mevedel-preset-restore-session "mevedel-presets")

;; `mevedel-prompt-submission'
(declare-function mevedel-prompt-submission-outcome
                  "mevedel-prompt-submission" (cl-x) t)

;; `mevedel-execution-target'
(declare-function mevedel-execution-target-native-path
                  "mevedel-execution-target" (target path))
(autoload 'mevedel-execution-target-native-path "mevedel-execution-target")

;; `mevedel-session-artifacts'
(declare-function mevedel-session-artifacts-save
                  "mevedel-session-artifacts"
                  (session buffer &optional settled force))
(declare-function mevedel-session-artifacts-start-fresh-segment
                  "mevedel-session-artifacts"
                  (session buffer &rest keys))
(autoload 'mevedel-session-artifacts-save "mevedel-session-artifacts")
(autoload 'mevedel-session-artifacts-start-fresh-segment
  "mevedel-session-artifacts")

;; `mevedel-session-fork'
(declare-function mevedel-session-fork-create
                  "mevedel-session-fork" (buffer target type))
(declare-function mevedel-session-fork-retarget-worktree-path
                  "mevedel-session-fork" (session path))
(autoload 'mevedel-session-fork-create "mevedel-session-fork")
(autoload 'mevedel-session-fork-retarget-worktree-path "mevedel-session-fork")

;; `mevedel-session-persistence'
(declare-function mevedel-session-persistence-list-sessions
                  "mevedel-session-persistence" (workspace &optional cached))
(declare-function mevedel-session-persistence-restore
                  "mevedel-session-persistence"
                  (session-dir &optional lifecycle-source
                               session-override workspace))
(autoload 'mevedel-session-persistence-list-sessions
  "mevedel-session-persistence")
(autoload 'mevedel-session-persistence-restore "mevedel-session-persistence")

;; `mevedel-session-rewind'
(declare-function mevedel-session-rewind-assert-stable-source
                  "mevedel-session-rewind" (session buffer operation))
(autoload 'mevedel-session-rewind-assert-stable-source "mevedel-session-rewind")

;; `mevedel-skills-core'
(declare-function mevedel-session-get-skill-by-source
                  "mevedel-skills-core" (session source-file))
(declare-function mevedel-skill-name "mevedel-skills-core" (cl-x) t)
(declare-function mevedel-skill-user-invocable-p
                  "mevedel-skills-core" (cl-x) t)
(declare-function mevedel-skills-skill-enabled-p
                  "mevedel-skills-core" (skill))
(autoload 'mevedel-session-get-skill-by-source "mevedel-skills-core")
(autoload 'mevedel-skill-name "mevedel-skills-core")
(autoload 'mevedel-skill-user-invocable-p "mevedel-skills-core")
(autoload 'mevedel-skills-skill-enabled-p "mevedel-skills-core")

;; `mevedel-skills-input'
(declare-function mevedel-skills-input-prepare-user-input
                  "mevedel-skills-input" (text session))
(autoload 'mevedel-skills-input-prepare-user-input "mevedel-skills-input")

;; `mevedel-structs'
(declare-function mevedel-session-current-segment "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-execution-target
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-forked-from-session-id
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-plan-metadata "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-prompt-index "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-save-path "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-session-id "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-working-directory "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-workspace "mevedel-structs" (cl-x) t)
(defvar mevedel--data-buffer)
(defvar mevedel--session)

;; `mevedel-view-composer'
(declare-function mevedel-view--submit-planned-input
                  "mevedel-view-composer"
                  (input &optional before-send on-block dispatch after-insert))

;; `mevedel-view-interaction'
(declare-function mevedel-view--interaction-target-buffer
                  "mevedel-view-interaction" (chat-buffer))

;; `mevedel-view-stream'
(declare-function mevedel-view--stop-request-progress
                  "mevedel-view-stream" nil)
(declare-function mevedel-view--stop-spinner
                  "mevedel-view-stream" nil)
(declare-function mevedel-view--update-spinner
                  "mevedel-view-stream" (status &optional owner))
(defvar mevedel-view--spinner-owner)

;; `mevedel-worktree'
(declare-function mevedel-worktree-fork-reservation
                  "mevedel-worktree" (session &optional preflight branch))
(autoload 'mevedel-worktree-fork-reservation "mevedel-worktree")

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

(defconst mevedel-plan-handoff-implementation-modes '(ask edits full-auto)
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
Only a Here handoff reserves one: a Worktree handoff hands the Goal to the
worktree fork, which runs its own Here handoff."
  (when-let* ((session (mevedel-plan-handoff--current-session session)))
    (let* ((retry (plist-get (mevedel-session-plan-metadata session)
                             :implementation-retry))
           (selection (plist-get retry :selection)))
      (and (eq (plist-get selection :execution) 'goal)
           (eq (plist-get selection :location) 'here)
           (plist-get retry :goal-id)))))

(defun mevedel-plan-handoff-selection-valid-p (selection)
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
              (memq (plist-get selection :execution) '(direct goal))
              (memq (plist-get selection :mode)
                    mevedel-plan-handoff-implementation-modes)
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

(defun mevedel-plan-handoff-append-implementation-input
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
          (mevedel-mention-bindings-set
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

(defun mevedel-plan-handoff--implementation-prompt
    (accepted-artifact plan-markdown &optional selection)
  "Return the Direct prompt for ACCEPTED-ARTIFACT and PLAN-MARKDOWN."
  (mevedel-plan-handoff-append-implementation-input
   (format
    "Accepted plan artifact: %s\n\nAccepted plan:\n%s\n\nImplementation instructions:\nImplement the accepted plan against the current repository state. Preserve its stated outcomes and acceptance criteria while using repository evidence to choose the safest effective mechanics."
    (mevedel-plan-resource-address
     (plist-get accepted-artifact :path))
    plan-markdown)
   selection))

(defun mevedel-plan-handoff--goal-kickoff-prompt
    (accepted-artifact plan-markdown &optional selection)
  "Return the Goal kickoff for ACCEPTED-ARTIFACT and PLAN-MARKDOWN."
  (mevedel-plan-handoff-append-implementation-input
   (format
    "Accepted plan artifact: %s\n\nAccepted plan:\n%s\n\nGoal kickoff:\nBegin the active Goal. Read the accepted plan supplied above before acting."
    (mevedel-plan-resource-address
     (plist-get accepted-artifact :path))
    plan-markdown)
   selection))

(defun mevedel-plan-handoff-validate-skill-bindings (prompt session)
  "Signal when an explicit skill binding in PROMPT is unavailable in SESSION."
  (dolist (range (mevedel-mention-bindings-ranges prompt))
    (let* ((binding (plist-get range :binding))
           (source (and (eq (plist-get binding :kind) 'skill)
                        (plist-get binding :source-file)))
           (skill (and source
                       (mevedel-session-get-skill-by-source session source))))
      (when source
        (unless (and skill
                     (mevedel-skills-skill-enabled-p skill)
                     (mevedel-skill-user-invocable-p skill))
          (error "Implementation skill %s is unavailable"
                 (plist-get binding :token)))))))

(defun mevedel-plan-handoff--implementation-record (selection accepted)
  "Return retry state for SELECTION and ACCEPTED artifact."
  (let ((record
         (list :step (mevedel-plan-handoff--initial-step selection)
               :selection (copy-tree selection)
               :accepted
               (list :path (plist-get accepted :path)
                     :hash (plist-get accepted :hash)))))
    (when (eq (plist-get selection :execution) 'goal)
      (setq record (plist-put record :goal-id (mevedel-goal-new-id))))
    record))

(defun mevedel-plan-handoff--initial-step (selection)
  "Return the first preparation step for SELECTION."
  (if (eq (plist-get selection :location) 'worktree)
      'prepare-worktree
    (pcase (plist-get selection :context)
      ('summary 'prepare-summary)
      ('fresh 'prepare-context)
      (_ 'submit))))

(defun mevedel-plan-handoff-assert-worktree-ready (session buffer)
  "Signal unless SESSION in BUFFER can fork into a worktree now.
Checked before acceptance, so a session that cannot fork keeps its proposal."
  (mevedel-session-rewind-assert-stable-source
   session buffer "implementing in a worktree"))

(defun mevedel-plan-handoff--latest-fork-point-id (session buffer)
  "Return the fork point of SESSION's latest settled reply in BUFFER.
Saving first indexes the fork point recorded when the reply settled."
  (mevedel-session-artifacts-save session buffer)
  (or (plist-get
       (car (last (cdr (assoc (or (mevedel-session-current-segment session) 1)
                              (mevedel-session-prompt-index session)))))
       :fork-point-id)
      (error "The accepted plan's reply has no fork point")))

(defun mevedel-plan-handoff--find-fork (session directory)
  "Return the save path of SESSION's unique saved fork at DIRECTORY, or nil."
  (let* ((target (mevedel-session-execution-target session))
         (native (directory-file-name
                  (if target
                      (mevedel-execution-target-native-path target directory)
                    directory)))
         (matches
          (cl-remove-if-not
           (lambda (entry)
             (let ((summary (plist-get entry :summary)))
               (and (equal (plist-get summary :forked-from-session-id)
                           (mevedel-session-session-id session))
                    (stringp (plist-get summary :working-directory))
                    (equal native (directory-file-name
                                   (plist-get summary :working-directory))))))
           (mevedel-session-persistence-list-sessions
            (mevedel-session-workspace session)))))
    (when (cdr matches)
      (error "Several sessions were forked into %s" directory))
    (plist-get (car matches) :save-path)))

(defun mevedel-plan-handoff--open-fork (session directory save-path)
  "Open SESSION's fork saved at SAVE-PATH, which must be rooted at DIRECTORY."
  (let* ((buffer (mevedel-session-persistence-restore
                  save-path nil nil (mevedel-session-workspace session)))
         (fork (buffer-local-value 'mevedel--session buffer)))
    (unless (and (equal (mevedel-session-forked-from-session-id fork)
                        (mevedel-session-session-id session))
                 (file-equal-p (mevedel-session-working-directory fork)
                               directory))
      (error "The prepared worktree session does not belong to this plan"))
    buffer))

(defun mevedel-plan-handoff--worktree-fork (session chat-buffer record)
  "Return RECORD's worktree fork of SESSION, creating it at most once.
A fork published before an interruption is found again by its recorded
save path, or, when even that was not saved, by its directory and parent."
  (let* ((reservation (plist-get record :worktree-reservation))
         (directory (plist-get reservation :directory))
         (save-path (or (plist-get record :target-save-path)
                        (mevedel-plan-handoff--find-fork session directory))))
    (if save-path
        (mevedel-plan-handoff--open-fork session directory save-path)
      (mevedel-session-fork-create
       chat-buffer
       (list :fork-point-id (plist-get record :fork-point-id)
             :worktree-reservation reservation)
       'worktree))))

(defun mevedel-plan-handoff--install-in-fork (session record fork fork-buffer)
  "Give FORK RECORD's accepted plan and an equivalent Here handoff.
Return the fork's retry record.  A plan installed by an earlier attempt,
recognized by its hash, is not installed again; its retry record is
returned as it now is, nil when the fork already started it."
  (let ((metadata (mevedel-session-plan-metadata fork))
        (hash (plist-get (plist-get record :accepted) :hash)))
    (if (equal hash (plist-get metadata :accepted-hash))
        (plist-get metadata :implementation-retry)
      (let* ((accepted (mevedel-plan-archive-accepted
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
                                    (mevedel-session-fork-retarget-worktree-path
                                     fork (plist-get skill :source-file))))
                       (plist-get selection :skills))))
        (setq prepared
              (append
               (list :step (mevedel-plan-handoff--initial-step selection)
                     :selection selection
                     :accepted (list :path (plist-get accepted :path)
                                     :hash (plist-get accepted :hash)))
               (when-let* ((goal-id (plist-get record :goal-id)))
                 (list :goal-id goal-id))))
        (setf (mevedel-session-plan-metadata fork)
              (list :status 'accepted
                    :accepted-path (plist-get accepted :path)
                    :accepted-hash (plist-get accepted :hash)
                    :selection selection
                    :implementation-retry prepared))
        (mevedel-session-artifacts-save fork fork-buffer)
        prepared))))

(defun mevedel-plan-handoff--hand-over (chat-buffer fork-buffer)
  "Show FORK-BUFFER's view where CHAT-BUFFER's was, then close CHAT-BUFFER.
The planning session is saved and finished; its conversation continues in
the fork.  Closing waits for the accepting command to return."
  (let ((source-view (buffer-local-value 'mevedel--view-buffer chat-buffer))
        (fork-view (or (buffer-local-value 'mevedel--view-buffer fork-buffer)
                       fork-buffer))
        windows)
    (when (buffer-live-p source-view)
      (setq windows (get-buffer-window-list source-view nil t))
      (dolist (window windows)
        (set-window-buffer window fork-view)))
    (unless windows
      (mevedel--display-chat-buffer fork-buffer))
    ;; Sharing is optional; a shared source necessarily loaded its owner.
    ;; Deliver fresh continuation credentials before buffer teardown revokes it.
    (when (featurep 'mevedel-collaboration)
      (mevedel-collaboration-hand-off chat-buffer fork-buffer))
    (run-at-time
     0 nil
     (lambda ()
       ;; Killing a view kills its data buffer; kill both in case either
       ;; outlives the other.
       (dolist (buffer (list source-view chat-buffer))
         (when (buffer-live-p buffer)
           (kill-buffer buffer)))))))

(defun mevedel-plan-handoff--prepare-worktree (session chat-buffer record)
  "Fork SESSION into RECORD's worktree and hand the implementation to it.
The fork carries the planning conversation; it receives the accepted plan
and runs the selected context and execution as an ordinary Here handoff.
Each durable step is recorded before the next, so a retry neither forks
twice nor installs the plan twice.  Return nil: the fork continues alone."
  (let ((prepared (copy-tree record)))
    (unless (plist-get prepared :fork-point-id)
      (setq prepared (plist-put prepared :fork-point-id
                                (mevedel-plan-handoff--latest-fork-point-id
                                 session chat-buffer))))
    (unless (plist-get prepared :worktree-reservation)
      (setq prepared
            (plist-put prepared :worktree-reservation
                       (with-current-buffer chat-buffer
                         (mevedel-worktree-fork-reservation
                          session nil
                          (plist-get (plist-get prepared :selection) :branch)))))
      (mevedel-plan--metadata-put session :implementation-retry prepared)
      (mevedel-session-artifacts-save session chat-buffer))
    (let* ((fork-buffer (mevedel-plan-handoff--worktree-fork
                         session chat-buffer prepared))
           (fork (buffer-local-value 'mevedel--session fork-buffer)))
      (unless (plist-get prepared :target-save-path)
        (setq prepared (plist-put prepared :target-save-path
                                  (mevedel-session-save-path fork)))
        (mevedel-plan--metadata-put session :implementation-retry prepared)
        (mevedel-session-artifacts-save session chat-buffer))
      (let ((fork-record (mevedel-plan-handoff--install-in-fork
                          session prepared fork fork-buffer))
            (metadata (copy-sequence (mevedel-session-plan-metadata session))))
        ;; The fork owns the implementation now; the source keeps its archive.
        (cl-remf metadata :implementation-retry)
        (setf (mevedel-session-plan-metadata session) metadata)
        (mevedel-session-artifacts-save session chat-buffer)
        (mevedel-plan-handoff--hand-over chat-buffer fork-buffer)
        (when fork-record
          (mevedel-plan-handoff--begin fork fork-buffer))
        nil))))

(defun mevedel-plan-handoff--summary-focus (plan selection)
  "Return exact PLAN and SELECTION instructions as relevance focus."
  (format
   "Accepted plan (authoritative; relevance only):\n%s\n\nImplementation-only instructions (authoritative; relevance only):\n%s"
   plan
   (or (plist-get selection :instructions) "(none)")))

(defun mevedel-plan-handoff--summary-source
    (source previous-summary plan)
  "Return SOURCE with PREVIOUS-SUMMARY as evidence and PLAN omitted."
  (string-replace
   plan
   "[mevedel accepted-plan omission: supplied separately as authoritative focus]"
   (if previous-summary
       (concat
        "--- evidence item; provenance: prior-continuation-summary ---\n"
        previous-summary
        "\n--- end evidence item ---\n\n"
        source)
     source)))

(defun mevedel-plan-handoff--advance-record
    (session record step &optional summary)
  "Store RECORD advanced to STEP as SESSION's retry state and return it.
SUMMARY, when given, becomes the cached handoff summary.  Any recorded
failure is cleared, because reaching STEP supersedes it."
  (let ((prepared (copy-tree record)))
    (when summary
      (setq prepared (plist-put prepared :summary summary)))
    (setq prepared (plist-put prepared :step step))
    (cl-remf prepared :failure)
    (mevedel-plan--metadata-put session :implementation-retry prepared)
    prepared))

(defun mevedel-plan-handoff--prepare-here-summary
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
             (mevedel-plan-handoff--advance-record
              session record 'submit summary)
             (condition-case apply-error
                 (apply apply-function active-target summary args)
               (error
                (mevedel-plan-handoff--advance-record
                 session record 'prepare-summary summary)
                (signal (car apply-error) (cdr apply-error)))))))
    (mevedel-compact-run-start
     :aggressive t
     :focus focus
     :prepared-summary (plist-get record :summary)
     :purpose 'handoff
     :source-transform source-transform
     :summary-ready
     (lambda (summary)
       (mevedel-plan-handoff--advance-record
        session record 'prepare-summary summary)
       (mevedel-session-artifacts-save session chat-buffer)
       summary)
     :target target
     :callback
     (lambda (err)
       (if err
           (mevedel-plan-handoff--implementation-failed
            session chat-buffer err)
         (mevedel-plan-handoff--dispatch-accepted session chat-buffer))))))

(defun mevedel-plan-handoff--prepare-summary (session chat-buffer record)
  "Compact CHAT-BUFFER into the implementation handoff for SESSION RECORD.
Runs under the ordinary compaction retry policy."
  (with-current-buffer chat-buffer
    (let* ((selection (plist-get record :selection))
           (plan
            (mevedel-plan-read-artifact
             session (plist-get record :accepted)))
           (target (mevedel-compact-target-main-target))
           (previous-summary (plist-get target :previous-summary))
           (focus (mevedel-plan-handoff--summary-focus plan selection))
           (source-transform
            (lambda (source)
              (mevedel-plan-handoff--summary-source
               source previous-summary plan))))
      (mevedel-plan-handoff--prepare-here-summary
       session chat-buffer record target focus source-transform))))

(defconst mevedel-plan-handoff--failure-reason-limit 500
  "Maximum characters of a failure reason kept in a durable retry record.")

(defun mevedel-plan-handoff--failure-reason (reason)
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
     text mevedel-plan-handoff--failure-reason-limit nil nil "...")))

(defun mevedel-plan-handoff--implementation-failed
    (session chat-buffer reason)
  "Keep SESSION retryable after REASON from CHAT-BUFFER.
REASON is a string or an error value; it is recorded in bounded form."
  (setq reason (mevedel-plan-handoff--failure-reason reason))
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
        (mevedel-session-artifacts-save session chat-buffer)
      (error
       (display-warning
        'mevedel
        (format "Could not persist plan implementation retry: %s"
                (mevedel-plan-handoff--failure-reason err))))))
  (message
   "mevedel: Implementation failed: %s; retry with M-x mevedel-retry-plan-implementation"
   reason)
  nil)

(defun mevedel-plan-handoff--implementation-request-started
    (owner chat-buffer)
  "Attach CHAT-BUFFER's Direct handoff recovery to turn OWNER.
OWNER is a native gptel state machine or an admitted external request."
  (let ((info (copy-sequence (or (mevedel-engine-info owner) nil))))
    (setf (mevedel-engine-info owner)
          (plist-put info :mevedel-plan-handoff-source-buffer chat-buffer)))
  owner)

(defun mevedel-plan-handoff--implementation-completed
    (session chat-buffer)
  "Durably clear SESSION's Direct retry after terminal success."
  (let* ((old-metadata (mevedel-session-plan-metadata session))
         (metadata (copy-sequence (or old-metadata nil))))
    (cl-remf metadata :implementation-retry)
    (setf (mevedel-session-plan-metadata session) metadata)
    (condition-case err
        (mevedel-session-artifacts-save session chat-buffer)
      (error
       (setf (mevedel-session-plan-metadata session) old-metadata)
       (display-warning
        'mevedel
        (format "Could not persist completed plan implementation: %s"
                (mevedel-plan-handoff--failure-reason err)))))))

(defun mevedel-plan-handoff-settle-request (fsm status &optional reason)
  "Settle FSM's attached Direct Plan handoff with terminal STATUS.

STATUS is `success', `error', or `aborted'.  Optional REASON supplies the
provider or abort detail retained for a retryable failure."
  (let* ((info (copy-sequence (or (mevedel-engine-info fsm) nil)))
         (chat-buffer
          (plist-get info :mevedel-plan-handoff-source-buffer)))
    (when (plist-member info :mevedel-plan-handoff-source-buffer)
      (cl-remf info :mevedel-plan-handoff-source-buffer)
      (setf (mevedel-engine-info fsm) info)
      (if (not (buffer-live-p chat-buffer))
          (display-warning
           'mevedel
           "Plan implementation settled after its source buffer was killed; \
the durable retry was retained"
           :warning)
        (with-current-buffer chat-buffer
          (when (bound-and-true-p mevedel--session)
            (if (eq status 'success)
                (mevedel-plan-handoff--implementation-completed
                 mevedel--session chat-buffer)
              (mevedel-plan-handoff--implementation-failed
               mevedel--session chat-buffer
               (or reason
                   (if (eq status 'aborted)
                       "Request aborted"
                     "Provider request failed"))))))))))

(defun mevedel-plan-handoff--goal-handoff-complete (session chat-buffer)
  "Durably clear SESSION's Plan retry before Goal kickoff from CHAT-BUFFER."
  (let* ((old-metadata (mevedel-session-plan-metadata session))
         (metadata (copy-sequence (or old-metadata nil))))
    (cl-remf metadata :implementation-retry)
    (setf (mevedel-session-plan-metadata session) metadata)
    (condition-case err
        (mevedel-session-artifacts-save session chat-buffer)
      (error
       (setf (mevedel-session-plan-metadata session) old-metadata)
       (signal (car err) (cdr err))))))

(defun mevedel-plan-handoff--prepare-context (session chat-buffer record)
  "Rotate CHAT-BUFFER once and return RECORD advanced to submission."
  (let ((prepared (plist-put record :step 'submit)))
    ;; The segment transaction persists the advanced retry step, so recovery
    ;; cannot repeat a completed rotation.
    (mevedel-plan--metadata-put session :implementation-retry prepared)
    (condition-case err
        (with-current-buffer chat-buffer
          (unless
              (mevedel-session-artifacts-start-fresh-segment
               session chat-buffer)
            (error "Could not start a fresh conversation segment"))
          (mevedel--run-session-start-hooks "clear"))
      (error
       (setq prepared (plist-put prepared :step 'prepare-context))
       (mevedel-plan--metadata-put session :implementation-retry prepared)
       (signal (car err) (cdr err))))
    prepared))

(defun mevedel-plan-handoff--start-goal (record accepted session chat-buffer)
  "Construct RECORD's durable Goal in SESSION and return its exact owner."
  (with-current-buffer chat-buffer
    (mevedel-goal-ensure
     mevedel-plan-handoff--accepted-goal-objective
     session
     (plist-get accepted :path)
     (or (plist-get record :goal-id)
         (error "Accepted Goal handoff has no reserved identity"))
     :implementation-policy
     (mevedel-implementation-policy-from-selection (plist-get record :selection))
     :token-budget (plist-get (plist-get record :selection) :goal-token-budget))))

(defun mevedel-plan-handoff--dispatch-submission
    (session chat-buffer record selection accepted display-text submission)
  "Dispatch one accepted SUBMISSION and settle its durable handoff state."
  (let ((goal-p (eq (plist-get selection :execution) 'goal))
        (goal-id (plist-get record :goal-id))
        (policy (copy-tree (mevedel-implementation-policy-from-selection selection)))
        goal-started
        request-fsm)
    (condition-case err
        (progn
          ;; Release only our preparation status before transferring
          ;; ownership to the implementation request.
          (when-let* ((view-buffer (buffer-local-value 'mevedel--view-buffer
                                                       chat-buffer))
                      ((buffer-live-p view-buffer)))
            (with-current-buffer view-buffer
              ;; Only a loaded stream module can own a spinner at all.
              (when (eq (bound-and-true-p mevedel-view--spinner-owner)
                        'plan-preparation)
                (mevedel-view--stop-spinner))))
          (when goal-p
            (setq goal-started
                  (mevedel-plan-handoff--start-goal
                   record accepted session chat-buffer))
            ;; Keep the exact owner before publication can yield.
            (mevedel-plan-handoff--goal-handoff-complete session chat-buffer))
          (with-current-buffer chat-buffer
            (setq request-fsm
                  (mevedel--implement-plan
                   (list :implementation-policy
                         policy
                         :implementation-goal goal-started
                         :implementation-goal-id (and goal-p goal-id)
                         :display-text display-text
                         :prompt-submission submission
                         :prepared-outcome
                         (mevedel-prompt-submission-outcome submission)))))
          (unless goal-p
            (mevedel-plan-handoff--implementation-request-started
             request-fsm chat-buffer)))
      (error
       (let ((reason (mevedel-plan-handoff--failure-reason err)))
         (if goal-started
             (progn
               (when (and (not (eq (car err) 'mevedel-stale-implementation-preparation))
                          (buffer-live-p chat-buffer)
                          (eq session (buffer-local-value 'mevedel--session chat-buffer))
                          (eq goal-started (mevedel-session-goal session))
                          (equal goal-id (mevedel-goal-id goal-started))
                          (equal policy (mevedel-goal-implementation-policy goal-started)))
                 (mevedel-goal-pause-runtime-failure chat-buffer reason))
               (message
                "mevedel: Goal kickoff did not start: %s; inspect the current Goal before resuming"
                reason))
           (mevedel-plan-handoff--implementation-failed
            session chat-buffer reason)))))))

(defun mevedel-plan-handoff--submit (session chat-buffer record)
  "Submit prepared implementation RECORD for SESSION from CHAT-BUFFER."
  (let* ((selection (plist-get record :selection))
         (goal-p (eq (plist-get selection :execution) 'goal)))
    (when (and (eq (plist-get selection :context) 'summary)
               (not (equal
                     (plist-get record :summary)
                     (with-current-buffer chat-buffer
                       (mevedel-compact-evidence-previous-summary)))))
      (error "Prepared plan summary does not match the current segment"))
    (let* ((accepted (plist-get record :accepted))
           (body (mevedel-plan-read-artifact session accepted))
           (prompt
            (if goal-p
                (mevedel-plan-handoff--goal-kickoff-prompt
                 accepted body selection)
              (mevedel-plan-handoff--implementation-prompt
               accepted body selection)))
           (display-text
            (if goal-p
                "Implement accepted plan as Goal"
              "Implement accepted plan"))
           (view-buffer
            (mevedel-view--interaction-target-buffer chat-buffer)))
      (with-current-buffer chat-buffer
        (setq prompt
              (mevedel-skills-input-prepare-user-input prompt session))
        (mevedel-plan-handoff-validate-skill-bindings prompt session))
      (with-current-buffer view-buffer
        (mevedel-view--submit-planned-input
         prompt nil
         (lambda ()
           (mevedel-plan-handoff--implementation-failed
            session chat-buffer "Prompt submission was blocked"))
         (lambda (submission)
           (mevedel-plan-handoff--dispatch-submission
            session chat-buffer record selection accepted display-text
            submission)))))))

(defun mevedel-plan-handoff--validate-record (record)
  "Validate durable handoff RECORD and return its selection."
  (let* ((selection (plist-get record :selection))
         (location (plist-get selection :location))
         (accepted (plist-get record :accepted)))
    (unless (mevedel-plan-handoff-selection-valid-p selection)
      (error "Invalid accepted plan implementation selection"))
    (unless (and (stringp (plist-get selection :model-provider))
                 (string-match-p
                  "\\`[^:]+:.+\\'"
                  (plist-get selection :model-provider)))
      (error "Accepted plan implementation lacks a model snapshot"))
    (mevedel-implementation-policy-from-selection selection)
    (unless (memq (plist-get record :step)
                  '(prepare-context prepare-summary prepare-worktree submit))
      (error "Invalid accepted plan implementation step"))
    (unless accepted
      (error "Accepted plan implementation lacks an artifact"))
    (unless (and (mevedel-plan-artifact-path-p (plist-get accepted :path))
                 (stringp (plist-get accepted :hash))
                 (not (plist-member accepted :absolute-path))
                 (not (plist-member accepted :accepted-absolute-path)))
      (error "Invalid accepted plan artifact"))
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
        (mevedel-session-artifacts-save session chat-buffer)
        (while record
          (pcase (plist-get record :step)
            ('prepare-worktree
             (unless (eq location 'worktree)
               (error "Invalid accepted plan Worktree step"))
             (setq record
                   (mevedel-plan-handoff--prepare-worktree
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
      session chat-buffer err))))

(defun mevedel-plan-handoff-start
    (session chat-buffer selection accepted)
  "Start durable accepted-plan handoff for SESSION from CHAT-BUFFER."
  (unless (mevedel-plan-handoff-selection-valid-p selection)
    (error "Unsupported Plan implementation selection: %S" selection))
  (let ((record
         (mevedel-plan-handoff--implementation-record
          selection accepted)))
    (mevedel-plan--metadata-put session :selection selection)
    (mevedel-plan--metadata-put session :implementation-retry record)
    (condition-case err
        (progn
          (when (eq (plist-get selection :execution) 'goal)
            (mevedel-session-artifacts-save session chat-buffer))
          (when (eq (plist-get selection :location) 'worktree)
            ;; Fork at the reply that proposed this plan, whatever follows.
            (mevedel-plan--metadata-put
             session :implementation-retry
             (plist-put record :fork-point-id
                        (mevedel-plan-handoff--latest-fork-point-id
                         session chat-buffer))))
          (mevedel-plan-handoff--begin session chat-buffer))
      (error
       (mevedel-plan-handoff--implementation-failed
        session chat-buffer err)))))

(defun mevedel-plan-handoff--begin (session chat-buffer)
  "Show preparation progress and dispatch SESSION's handoff from CHAT-BUFFER."
  (when-let* ((view-buffer
               (ignore-errors
                 (mevedel-view--interaction-target-buffer chat-buffer)))
              ((fboundp 'mevedel-view--update-spinner)))
    (with-current-buffer view-buffer
      (mevedel-view--update-spinner
       "Preparing implementation..." 'plan-preparation)))
  (mevedel-plan-handoff--dispatch-accepted session chat-buffer))

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
