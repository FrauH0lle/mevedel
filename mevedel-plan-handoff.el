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

;; `gptel-request'
(declare-function gptel-fsm-info "ext:gptel-request" (cl-x) t)

;; `mevedel-chat'
(declare-function mevedel--implement-plan "mevedel-chat" (action-plist))
(declare-function mevedel--run-session-start-hooks "mevedel-chat" (source))

;; `mevedel-compact-evidence'
(declare-function mevedel-compact-evidence-find-boundary
                  "mevedel-compact-evidence" ())
(declare-function mevedel-compact-evidence-previous-summary
                  "mevedel-compact-evidence" ())
(declare-function mevedel-compact-evidence-select
                  "mevedel-compact-evidence" (target limit aggressive))

;; `mevedel-compact-run'
(declare-function mevedel-compact-run-start
                  "mevedel-compact-run" (&rest keys))
(defvar mevedel-compact-run-cancel)

;; `mevedel-compact-target'
(declare-function mevedel-compact-target-main-target
                  "mevedel-compact-target" ())

;; `mevedel-context-summary'
(declare-function mevedel-context-summary-generate
                  "mevedel-context-summary"
                  (source purpose callback &rest args))

;; `mevedel-goal'
(declare-function mevedel-goal-ensure "mevedel-goal"
                  (objective session plan-reference id))
(declare-function mevedel-goal-new-id "mevedel-goal" nil)
(declare-function mevedel-goal-pause-runtime-failure "mevedel-goal"
                  (buffer reason))

;; `mevedel-mention-bindings'
(declare-function mevedel-mention-bindings-ranges
                  "mevedel-mention-bindings" (text))
(declare-function mevedel-mention-bindings-set
                  "mevedel-mention-bindings"
                  (start end binding &optional object))

;; `mevedel-models'
(declare-function mevedel-model-resolve-provider
                  "mevedel-models" (spec &optional noerror))
(declare-function mevedel-model-set-session-effort
                  "mevedel-models" (session effort &optional buffer))
(declare-function mevedel-model-set-session-provider
                  "mevedel-models" (session provider &optional buffer))

;; `mevedel-permission-mode'
(declare-function mevedel-permission-mode-transition
                  "mevedel-permission-mode" (mode))

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

;; `mevedel-prompt-submission'
(declare-function mevedel-prompt-submission-outcome
                  "mevedel-prompt-submission" (cl-x) t)

;; `mevedel-execution-target'
(declare-function mevedel-execution-target-native-path
                  "mevedel-execution-target" (target path))
(declare-function mevedel-execution-target-remote-p
                  "mevedel-execution-target" (target))

;; `mevedel-session-artifacts'
(declare-function mevedel-session-artifacts-save
                  "mevedel-session-artifacts"
                  (session buffer &optional settled force))
(declare-function mevedel-session-artifacts-start-fresh-segment
                  "mevedel-session-artifacts"
                  (session buffer &rest keys))
(declare-function mevedel-session-artifacts-summary-block
                  "mevedel-session-artifacts" (summary))

;; `mevedel-session-persistence'
(declare-function mevedel-session-persistence-restore
                  "mevedel-session-persistence"
                  (session-dir &optional lifecycle-source
                               session-override workspace))

;; `mevedel-skills-core'
(declare-function mevedel-session-get-skill-by-source
                  "mevedel-skills-core" (session source-file))
(declare-function mevedel-skill-name "mevedel-skills-core" (cl-x) t)
(declare-function mevedel-skill-user-invocable-p
                  "mevedel-skills-core" (cl-x) t)
(declare-function mevedel-skills-skill-enabled-p
                  "mevedel-skills-core" (skill))

;; `mevedel-skills-input'
(declare-function mevedel-skills-input-prepare-user-input
                  "mevedel-skills-input" (text session))

;; `mevedel-structs'
(declare-function mevedel-session-execution-target
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-plan-metadata "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-preset-name "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-save-path "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-session-id "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-working-directory "mevedel-structs" (cl-x) t)
(defvar mevedel--data-buffer)
(defvar mevedel--session)
(defvar mevedel-goal-token-budget)

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
(declare-function mevedel-view--update-spinner
                  "mevedel-view-stream" (status &optional owner))

;; `mevedel-worktree'
(declare-function mevedel-worktree-session-directory
                  "mevedel-worktree" (branch))
(declare-function mevedel-worktree-repository-root
                  "mevedel-worktree" (directory))
(declare-function mevedel-worktree-create-session
                  "mevedel-worktree"
                  (&optional branch purpose clean recovery))

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
             (context (plist-get selection :context))
             (budget (plist-get selection :goal-token-budget))
             (skills (plist-get selection :skills))
             (instructions (plist-get selection :instructions))
             (provider (plist-get selection :model-provider))
             (effort (plist-get selection :reasoning-effort)))
         (and (or (and (eq location 'here)
                       (memq context '(current fresh summary)))
                  (and (eq location 'worktree)
                       (memq context '(fresh summary))))
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
          (require 'mevedel-mention-bindings)
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
    (_session accepted-artifact plan-markdown &optional selection)
  "Return the Direct prompt for ACCEPTED-ARTIFACT and PLAN-MARKDOWN.

The accepted artifact is named by its canonical address, so the prompt
needs no session."
  (mevedel-plan-handoff-append-implementation-input
   (format
    "Accepted plan artifact: %s\n\nAccepted plan:\n%s\n\nImplementation instructions:\nImplement the accepted plan against the current repository state. Preserve its stated outcomes and acceptance criteria while using repository evidence to choose the safest effective mechanics."
    (mevedel-plan-resource-address
     (plist-get accepted-artifact :path))
    plan-markdown)
   selection))

(defun mevedel-plan-handoff--goal-kickoff-prompt
    (_session accepted-artifact plan-markdown &optional selection)
  "Return the Goal kickoff for ACCEPTED-ARTIFACT and PLAN-MARKDOWN.

The accepted artifact is named by its canonical address, so the kickoff
needs no session."
  (mevedel-plan-handoff-append-implementation-input
   (format
    "Accepted plan artifact: %s\n\nAccepted plan:\n%s\n\nGoal kickoff:\nBegin the active Goal. Read the accepted plan supplied above before acting."
    (mevedel-plan-resource-address
     (plist-get accepted-artifact :path))
    plan-markdown)
   selection))

(defun mevedel-plan-handoff--persist (session chat-buffer)
  "Persist SESSION from CHAT-BUFFER."
(require 'mevedel-session-persistence)
(require 'mevedel-session-codec)
(require 'mevedel-session-artifacts)
  (mevedel-session-artifacts-save session chat-buffer))

(defun mevedel-plan-handoff--apply-model-policy
    (selection session buffer)
  "Apply SELECTION's accepted model policy to SESSION in BUFFER."
  (when-let* ((label (plist-get selection :model-provider)))
    (require 'mevedel-models)
    (mevedel-model-set-session-provider
     session (mevedel-model-resolve-provider label) buffer)
    (mevedel-model-set-session-effort
     session (plist-get selection :reasoning-effort) buffer)))

(defun mevedel-plan-handoff-validate-skill-bindings (prompt session)
  "Signal when an explicit skill binding in PROMPT is unavailable in SESSION."
  (require 'mevedel-mention-bindings)
  (require 'mevedel-skills-core)
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
         (list :step (pcase (plist-get selection :context)
                       ('summary 'prepare-summary)
                       ('fresh
                        (if (eq (plist-get selection :location) 'worktree)
                            'prepare-worktree
                          'prepare-context))
                       (_ 'submit))
               :selection (copy-tree selection)
               :accepted
               (list :path (plist-get accepted :path)
                     :hash (plist-get accepted :hash)))))
    (when (eq (plist-get selection :execution) 'goal)
      (require 'mevedel-goal)
      (setq record (plist-put record :goal-id (mevedel-goal-new-id))))
    record))

(defun mevedel-plan-handoff--accepted-body (session artifact)
  "Return SESSION's validated immutable accepted-plan ARTIFACT body."
  (require 'mevedel-plan)
  (mevedel-plan-read-artifact session artifact))

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
         (prepared (copy-tree record))
         (recovery (plist-member prepared :target-directory)))
    (unless recovery
      (setq prepared
            (plist-put
             prepared :target-directory
             (with-current-buffer chat-buffer
               (mevedel-worktree-session-directory branch))))
      (mevedel-plan--metadata-put session :implementation-retry prepared)
      (mevedel-plan-handoff--persist session chat-buffer))
    (let* ((result
            (with-current-buffer chat-buffer
              (mevedel-worktree-create-session
               branch "Accepted Plan implementation"
               (eq (plist-get selection :context) 'summary)
               (and recovery prepared))))
           (target-buffer (plist-get result :buffer))
           (target-session
            (buffer-local-value 'mevedel--session target-buffer)))
      (unless (equal branch (plist-get result :branch))
        (error "Created Worktree branch does not match the accepted branch"))
      (unless (equal (plist-get prepared :target-directory)
                     (plist-get result :directory))
        (error "Created Worktree directory does not match its reservation"))
      (condition-case err
          (mevedel-plan-handoff--persist target-session target-buffer)
        (error
         (when-let* ((save-path (mevedel-session-save-path target-session))
                     (session-id (mevedel-session-session-id target-session)))
           (setq prepared (plist-put prepared :target-save-path save-path))
           (setq prepared (plist-put prepared :target-session-id session-id))
           (mevedel-plan--metadata-put
            session :implementation-retry prepared))
         (signal (car err) (cdr err))))
      (unless (and (stringp (mevedel-session-save-path target-session))
                   (stringp (mevedel-session-session-id target-session)))
        (error "Prepared Worktree session was not persisted"))
      (setq prepared (plist-put prepared :step 'prepare-target))
      (setq prepared
            (plist-put prepared :target-save-path
                       (mevedel-session-save-path target-session)))
      (setq prepared
            (plist-put prepared :target-session-id
                       (mevedel-session-session-id target-session)))
      (mevedel-plan--metadata-put session :implementation-retry prepared)
      (mevedel-plan-handoff--persist session chat-buffer)
      prepared)))

(defun mevedel-plan-handoff--prepare-worktree-target
    (session chat-buffer record)
  "Prepare RECORD's target artifact, settings, and Mode for SESSION."
  (require 'mevedel-session-persistence)
  (require 'mevedel-session-codec)
  (require 'mevedel-session-artifacts)
  (let* ((selection (plist-get record :selection))
         (mode (plist-get selection :mode))
         (target-buffer (mevedel-plan-handoff--worktree-target-buffer record))
         (target-session
          (buffer-local-value 'mevedel--session target-buffer))
         (source-artifact (plist-get record :accepted))
         (_body
          (mevedel-plan-handoff--accepted-body session source-artifact))
         (target-metadata (mevedel-session-plan-metadata target-session))
         (existing
          (and (eq (plist-get target-metadata :status) 'accepted)
               (list :path (plist-get target-metadata :accepted-path)
                     :hash (plist-get target-metadata :accepted-hash))))
         (accepted
          (if existing
              (progn
                (unless (equal (plist-get existing :hash)
                               (plist-get source-artifact :hash))
                  (error "Prepared Worktree plan does not match source"))
                (mevedel-plan-handoff--accepted-body target-session existing)
                existing)
            (mevedel-plan-archive-accepted
             source-artifact target-session
             (file-name-concat "local" "plans" "accepted-handoff.md") session)))
         (prepared (copy-tree record)))
    (unless existing
      (setf (mevedel-session-preset-name target-session)
            (mevedel-session-preset-name session)
            (mevedel-session-plan-metadata target-session)
            (append
             (list :status 'accepted
                   :accepted-path (plist-get accepted :path)
                   :accepted-hash (plist-get accepted :hash))
             (and (eq (plist-get selection :execution) 'goal)
                  (list :implementation-goal-id
                        (plist-get record :goal-id)))))
      (with-current-buffer target-buffer
        (when (eq (plist-get selection :execution) 'goal)
          (setq-local mevedel-goal-token-budget
                      (plist-get selection :goal-token-budget)))
        (require 'mevedel-presets)
        (require 'mevedel-permission-mode)
        (mevedel-preset-restore-session target-session target-buffer)
        (mevedel-plan-handoff--apply-model-policy
         selection target-session target-buffer)
        (mevedel-permission-mode-transition mode)
        (when-let* (((eq (plist-get selection :context) 'summary))
                    (summary (plist-get record :summary)))
          (let ((current (mevedel-compact-evidence-previous-summary)))
            (cond
             ((equal current summary))
             (current (error "Prepared Worktree summary does not match target"))
             (t
              (let ((inhibit-read-only t))
                (goto-char (point-max))
                (unless (bolp) (insert "\n"))
                (insert
                 (mevedel-session-artifacts-summary-block summary)))))))
        (mevedel-plan-handoff--persist target-session target-buffer)))
    (setq prepared (plist-put prepared :step 'submit))
    (setq prepared (plist-put prepared :target-accepted accepted))
    (mevedel-plan--metadata-put session :implementation-retry prepared)
    (mevedel-plan-handoff--persist session chat-buffer)
    prepared))

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

(defun mevedel-plan-handoff--prepare-worktree-summary
    (session chat-buffer record target focus source-transform)
  "Generate one non-mutating Worktree handoff summary for SESSION RECORD.
TARGET is the active main compaction target, read but never mutated.
FOCUS carries the authoritative plan; SOURCE-TRANSFORM filters the
projected evidence.  CHAT-BUFFER is left unchanged."
  (unless (plist-get target :eligible-p)
    (user-error "Current buffer is not the active persisted segment"))
  (let* ((limit (or (mevedel-compact-evidence-find-boundary)
                    (user-error "Not enough conversation content to summarize")))
         (source
          (funcall source-transform
                   (plist-get
                    (mevedel-compact-evidence-select target limit t)
                    :content)))
         settled)
    (when (string-blank-p source)
      (user-error "Not enough conversation content to summarize"))
    (require 'mevedel-context-summary)
    (let ((cancel
           (mevedel-context-summary-generate
            source 'handoff
            (lambda (result)
              (setq settled t)
              (setq-local mevedel-compact-run-cancel nil)
              (pcase (plist-get result :outcome)
                ('success
                 (mevedel-plan-handoff--advance-record
                  session record 'prepare-worktree
                  (mevedel-plan-handoff--portable-paths
                   (plist-get result :summary) session))
                 (mevedel-plan-handoff--persist session chat-buffer)
                 (mevedel-plan-handoff--dispatch-accepted
                  session chat-buffer))
                ('aborted
                 (mevedel-plan-handoff--implementation-failed
                  session chat-buffer "Summary generation aborted"))
                ('error
                 (mevedel-plan-handoff--implementation-failed
                  session chat-buffer
                  (format "Summary generation failed: %s"
                          (plist-get result :error))))))
            :session session
            :focus focus)))
      (unless settled
        (setq-local mevedel-compact-run-cancel cancel)))))

(defun mevedel-plan-handoff--strip-path-prefix (text prefix)
  "Return TEXT with PREFIX removed wherever it begins a path.
Only at a path boundary: a bare replacement also rewrites a longer path
that merely contains PREFIX, so a snapshot or mount point spelled around
the repository root would be corrupted rather than shortened."
  (replace-regexp-in-string
   (concat "\\(\\`\\|[^[:alnum:]._~/-]\\)" (regexp-quote prefix))
   "\\1" text t))

(defun mevedel-plan-handoff--portable-paths (summary session)
  "Return SUMMARY with SESSION's repository root prefix removed.

The repository root, not the working directory: a target session's working
directory is its own worktree top level, so only a repository-relative path
resolves there.  Stripping a subdirectory prefix instead names the wrong
file, and leaves every path outside that subdirectory pointing at the
source checkout.

Both spellings go, because a model-visible path is target-native: on a
remote session the native spelling is the only one the evidence carries."
  (require 'mevedel-worktree)
  (require 'mevedel-execution-target)
  (let* ((directory (file-name-as-directory
                     (expand-file-name
                      (mevedel-session-working-directory session))))
         ;; Outside a repository the working directory is the only base there
         ;; is.  No diagnostic: this runs in the summary callback, and
         ;; emitting one changes the current buffer under a caller that
         ;; requires its own.
         (root (or (mevedel-worktree-repository-root directory) directory))
         (target (mevedel-session-execution-target session))
         (native (and target
                      (mevedel-execution-target-remote-p target)
                      (file-name-as-directory
                       (mevedel-execution-target-native-path target root)))))
    (dolist (prefix (delete-dups (delq nil (list root native))))
      (setq summary (mevedel-plan-handoff--strip-path-prefix summary prefix)))
    summary))

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
       (mevedel-plan-handoff--persist session chat-buffer)
       summary)
     :target target
     :callback
     (lambda (err)
       (if err
           (mevedel-plan-handoff--implementation-failed
            session chat-buffer (format "%s" err))
         (mevedel-plan-handoff--dispatch-accepted session chat-buffer))))))

(defun mevedel-plan-handoff--prepare-summary (session chat-buffer record)
  "Generate the implementation handoff for SESSION RECORD from CHAT-BUFFER.
Worktree keeps CHAT-BUFFER intact and makes one attempt; Here compacts
CHAT-BUFFER under the ordinary compaction retry policy."
  (require 'mevedel-compact)
  (require 'mevedel-compact-evidence)
  (require 'mevedel-compact-run)
  (require 'mevedel-compact-target)
  (with-current-buffer chat-buffer
    (let* ((selection (plist-get record :selection))
           (plan
            (mevedel-plan-handoff--accepted-body
             session (plist-get record :accepted)))
           (target (mevedel-compact-target-main-target))
           (previous-summary (plist-get target :previous-summary))
           (focus (mevedel-plan-handoff--summary-focus plan selection))
           (source-transform
            (lambda (source)
              (mevedel-plan-handoff--summary-source
               source previous-summary plan))))
      (cond
       ((not (eq (plist-get selection :location) 'worktree))
        (mevedel-plan-handoff--prepare-here-summary
         session chat-buffer record target focus source-transform))
       ;; A cached Worktree summary is already portable and validated.
       ((plist-get record :summary)
        (mevedel-plan-handoff--advance-record
         session record 'prepare-worktree)
        (mevedel-plan-handoff--persist session chat-buffer)
        (mevedel-plan-handoff--dispatch-accepted session chat-buffer))
       (t
        (mevedel-plan-handoff--prepare-worktree-summary
         session chat-buffer record target focus source-transform))))))

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
   "mevedel: Implementation failed: %s; retry with M-x mevedel-retry-plan-implementation"
   reason)
  nil)

(defun mevedel-plan-handoff--implementation-request-started
    (fsm chat-buffer)
  "Attach CHAT-BUFFER's Direct handoff recovery to FSM."
  (let ((info (copy-sequence (or (gptel-fsm-info fsm) nil))))
    (setf (gptel-fsm-info fsm)
          (plist-put info :mevedel-plan-handoff-source-buffer chat-buffer)))
  fsm)

(defun mevedel-plan-handoff--implementation-completed
    (session chat-buffer)
  "Durably clear SESSION's Direct retry after terminal success."
  (let* ((old-metadata (mevedel-session-plan-metadata session))
         (metadata (copy-sequence (or old-metadata nil))))
    (cl-remf metadata :implementation-retry)
    (setf (mevedel-session-plan-metadata session) metadata)
    (condition-case err
        (mevedel-plan-handoff--persist session chat-buffer)
      (error
       (setf (mevedel-session-plan-metadata session) old-metadata)
       (display-warning
        'mevedel
        (format "Could not persist completed plan implementation: %s"
                (error-message-string err)))))))

(defun mevedel-plan-handoff-settle-request (fsm status &optional reason)
  "Settle FSM's attached Direct Plan handoff with terminal STATUS.

STATUS is `success', `error', or `aborted'.  Optional REASON supplies the
provider or abort detail retained for a retryable failure."
  (let* ((info (copy-sequence (or (gptel-fsm-info fsm) nil)))
         (chat-buffer
          (plist-get info :mevedel-plan-handoff-source-buffer)))
    (when (plist-member info :mevedel-plan-handoff-source-buffer)
      (cl-remf info :mevedel-plan-handoff-source-buffer)
      (setf (gptel-fsm-info fsm) info)
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
  (require 'mevedel-session-persistence)
  (require 'mevedel-session-codec)
  (require 'mevedel-session-artifacts)
  (let ((prepared (plist-put record :step 'submit)))
    ;; The segment transaction persists the advanced retry step, so recovery
    ;; cannot repeat a completed rotation.
    (mevedel-plan--metadata-put session :implementation-retry prepared)
    (condition-case err
        (with-current-buffer chat-buffer
          (unless
              (mevedel-session-artifacts-start-fresh-segment
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
                (plist-get
                 (plist-get record :selection) :goal-token-budget))
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
        goal-started
        request-fsm)
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
            (setq request-fsm
                  (mevedel--implement-plan
                   (list :permission-mode (plist-get selection :mode)
                         :display-text display-text
                         :prompt-submission submission
                         :prepared-outcome
                         (mevedel-prompt-submission-outcome submission)))))
          (unless goal-p
            (mevedel-plan-handoff--implementation-request-started
             request-fsm chat-buffer)))
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
                       (mevedel-compact-evidence-previous-summary)))))
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
           (body
            (mevedel-plan-handoff--accepted-body target-session accepted))
           (prompt
            (if goal-p
                (mevedel-plan-handoff--goal-kickoff-prompt
                 target-session accepted body selection)
              (mevedel-plan-handoff--implementation-prompt
               target-session accepted body selection)))
           (display-text
            (if goal-p
                "Implement accepted plan as Goal"
              "Implement accepted plan"))
           (view-buffer
            (mevedel-view--interaction-target-buffer target-buffer)))
      (mevedel-plan-handoff--apply-model-policy
       selection target-session target-buffer)
      (with-current-buffer target-buffer
        (require 'mevedel-skills-input)
        (setq prompt
              (mevedel-skills-input-prepare-user-input prompt target-session))
        (mevedel-plan-handoff-validate-skill-bindings
         prompt target-session))
      (with-current-buffer view-buffer
        (mevedel-view--submit-planned-input
         prompt nil
         (lambda ()
           (mevedel-plan-handoff--implementation-failed
            session chat-buffer "Prompt submission was blocked"))
         (lambda (submission)
           (mevedel-plan-handoff--dispatch-submission
            session chat-buffer record selection accepted target-session
            target-buffer display-text submission)))))))

(defun mevedel-plan-handoff--validate-record (record)
  "Validate durable handoff RECORD and return its selection."
  (let* ((selection (plist-get record :selection))
         (location (plist-get selection :location))
         (accepted (plist-get record :accepted))
         (target-accepted (plist-get record :target-accepted)))
    (unless (mevedel-plan-handoff-selection-valid-p selection)
      (error "Invalid accepted plan implementation selection"))
    (unless (and (stringp (plist-get selection :model-provider))
                 (string-match-p
                  "\\`[^:]+:.+\\'"
                  (plist-get selection :model-provider)))
      (error "Accepted plan implementation lacks a model snapshot"))
    (unless (memq (plist-get record :step)
                  '(prepare-context prepare-summary prepare-worktree
                    prepare-target submit))
      (error "Invalid accepted plan implementation step"))
    (unless accepted
      (error "Accepted plan implementation lacks an artifact"))
    (dolist (artifact (delq nil (list accepted target-accepted)))
      (let ((path (plist-get artifact :path)))
        (unless (and (mevedel-plan-artifact-path-p path)
                     (stringp (plist-get artifact :hash))
                     (not (plist-member artifact :absolute-path))
                     (not (plist-member artifact :accepted-absolute-path)))
          (error "Invalid accepted plan artifact"))))
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
        (when (eq location 'here)
          (with-current-buffer chat-buffer
            (require 'mevedel-permission-mode)
            (mevedel-permission-mode-transition
             (plist-get selection :mode))))
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
            (mevedel-plan-handoff--persist session chat-buffer))
          (when-let* ((view-buffer
                       (ignore-errors
                         (mevedel-view--interaction-target-buffer chat-buffer)))
                      ((fboundp 'mevedel-view--update-spinner)))
            (with-current-buffer view-buffer
              (mevedel-view--update-spinner
               "Preparing implementation..." 'plan-preparation)))
          (mevedel-plan-handoff--dispatch-accepted session chat-buffer))
      (error
       (mevedel-plan-handoff--implementation-failed
        session chat-buffer (error-message-string err))))))

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
