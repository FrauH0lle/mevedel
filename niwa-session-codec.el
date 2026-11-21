;;; niwa-session-codec.el --- Session sidecar codec and validation -*- lexical-binding: t -*-

;;; Commentary:

;; Owns the closed session sidecar schema, validation, and literal IO.

;;; Code:

(require 'niwa-recovery)

(require 'cl-lib)

(eval-when-compile
  (require 'niwa-agents)
  (require 'niwa-structs))

;; `niwa-agent-persistence'
(declare-function niwa-agent-persistence-deserialize-registry "niwa-agent-persistence" (raw target))
(declare-function niwa-agent-persistence-sanitize-mailbox "niwa-agent-persistence" (raw recipient))
(declare-function niwa-agent-persistence-serialize-registry "niwa-agent-persistence" (session))
(autoload 'niwa-agent-persistence-deserialize-registry
  "niwa-agent-persistence")
(autoload 'niwa-agent-persistence-sanitize-mailbox
  "niwa-agent-persistence")
(autoload 'niwa-agent-persistence-serialize-registry
  "niwa-agent-persistence")

;; `niwa-execution-target'
(declare-function niwa-execution-target-create "niwa-execution-target" (workspace-root))
(declare-function niwa-execution-target-expand-path "niwa-execution-target" (target path &optional directory))
(declare-function niwa-execution-target-incarnation "niwa-execution-target" (cl-x))
(declare-function niwa-execution-target-native-path "niwa-execution-target" (target path))
(declare-function niwa-execution-target-native-root "niwa-execution-target" (cl-x))
(declare-function niwa-execution-target-restore-incarnation "niwa-execution-target" (target incarnation))
(autoload 'niwa-execution-target-create "niwa-execution-target")
(autoload 'niwa-execution-target-expand-path "niwa-execution-target")
(autoload 'niwa-execution-target-incarnation "niwa-execution-target")
(autoload 'niwa-execution-target-native-path "niwa-execution-target")
(autoload 'niwa-execution-target-native-root "niwa-execution-target")
(autoload 'niwa-execution-target-restore-incarnation
  "niwa-execution-target")

;; `niwa-implementation-policy'
(declare-function niwa-implementation-policy-validate
                  "niwa-implementation-policy" (policy))
(autoload 'niwa-implementation-policy-validate
  "niwa-implementation-policy")

;; `niwa-permission-mode'
(defvar niwa-permission-mode)

;; `niwa-permission-persistence'
(declare-function niwa-permission-deserialize-authority "niwa-permission-persistence" (rules grants target))
(declare-function niwa-permission-serialize-authority "niwa-permission-persistence" (rules grants target))
(autoload 'niwa-permission-deserialize-authority
  "niwa-permission-persistence")
(autoload 'niwa-permission-serialize-authority
  "niwa-permission-persistence")

;; `niwa-sandbox'
(defvar niwa-sandbox-mode)

;; `niwa-schedule'
(declare-function niwa-schedule-entry-p "niwa-schedule" (entry))
(autoload 'niwa-schedule-entry-p "niwa-schedule")

;; `niwa-session-checkpoint'
(declare-function niwa-session-checkpoint-seed "niwa-session-checkpoint" (session sidecar))
(autoload 'niwa-session-checkpoint-seed "niwa-session-checkpoint")

;; `niwa-session-control-fs'
(declare-function niwa-session-control-fs-path-exists-p "niwa-session-control-fs" (path))
(declare-function niwa-session-control-fs-physical-path "niwa-session-control-fs" (path))
(declare-function niwa-session-control-fs-read-file "niwa-session-control-fs" (path &optional coding-system max-bytes))
(declare-function niwa-session-control-fs-write-file "niwa-session-control-fs" (path content))
(autoload 'niwa-session-control-fs-path-exists-p
  "niwa-session-control-fs")
(autoload 'niwa-session-control-fs-physical-path
  "niwa-session-control-fs")
(autoload 'niwa-session-control-fs-read-file "niwa-session-control-fs")
(autoload 'niwa-session-control-fs-write-file "niwa-session-control-fs")

;; `niwa-session-durability'
(declare-function niwa-session-durability--assert-no-pid-lock "niwa-session-durability" (session-dir))
(autoload 'niwa-session-durability--assert-no-pid-lock
  "niwa-session-durability")

;; `niwa-structs'
(declare-function niwa-agent-path-p "niwa-structs" (path))
(declare-function niwa-goal--create "niwa-structs" (&rest slots))
(declare-function niwa-goal-created-at "niwa-structs" (cl-x))
(declare-function niwa-goal-id "niwa-structs" (cl-x))
(declare-function niwa-goal-implementation-policy "niwa-structs" (cl-x))
(declare-function niwa-goal-objective "niwa-structs" (cl-x))
(declare-function niwa-goal-plan-reference "niwa-structs" (cl-x))
(declare-function niwa-goal-reason "niwa-structs" (cl-x))
(declare-function niwa-goal-reason-diagnostic "niwa-structs" (cl-x))
(declare-function niwa-goal-status "niwa-structs" (cl-x))
(declare-function niwa-goal-time-used-seconds "niwa-structs" (cl-x))
(declare-function niwa-goal-token-budget "niwa-structs" (cl-x))
(declare-function niwa-goal-tokens-incomplete-p "niwa-structs" (cl-x))
(declare-function niwa-goal-tokens-used "niwa-structs" (cl-x))
(declare-function niwa-goal-turns-run "niwa-structs" (cl-x))
(declare-function niwa-goal-updated-at "niwa-structs" (cl-x))
(declare-function niwa-session--create "niwa-structs" (&rest slots))
(declare-function niwa-session-agent-turn-capacity "niwa-structs" (cl-x))
(declare-function niwa-session-attached-artifacts "niwa-structs" (cl-x))
(declare-function niwa-session-chapters "niwa-structs" (cl-x))
(declare-function niwa-session-dedicated-artifact "niwa-structs" (cl-x))
(declare-function niwa-session-authority-mode-for-session "niwa-structs" (session))
(declare-function niwa-session-created-at "niwa-structs" (cl-x))
(declare-function niwa-session-current-segment "niwa-structs" (cl-x))
(declare-function niwa-session-execution-target "niwa-structs" (cl-x))
(declare-function niwa-session-file-snapshots "niwa-structs" (cl-x))
(declare-function niwa-session-untracked-effects "niwa-structs" (cl-x))
(declare-function niwa-session-fork-type "niwa-structs" (cl-x))
(declare-function niwa-session-forked-from-fork-point-id "niwa-structs" (cl-x))
(declare-function niwa-session-forked-from-session-id "niwa-structs" (cl-x))
(declare-function niwa-session-forked-from-turn "niwa-structs" (cl-x))
(declare-function niwa-session-goal "niwa-structs" (cl-x))
(declare-function niwa-session-name "niwa-structs" (cl-x))
(declare-function niwa-session-permission-mode "niwa-structs" (cl-x))
(declare-function niwa-session-permission-rules "niwa-structs" (cl-x))
(declare-function niwa-session-plan-mode "niwa-structs" (cl-x))
(declare-function niwa-session-preset-name "niwa-structs" (cl-x))
(declare-function niwa-session-prompt-index "niwa-structs" (cl-x))
(declare-function niwa-session-ptc-checkpoints "niwa-structs" (cl-x))
(declare-function niwa-session-resource-grants "niwa-structs" (cl-x))
(declare-function niwa-session-sandbox-mode "niwa-structs" (cl-x))
(declare-function niwa-session-schedules "niwa-structs" (cl-x))
(declare-function niwa-session-session-id "niwa-structs" (cl-x))
(declare-function niwa-session-task-status-notes "niwa-structs" (cl-x))
(declare-function niwa-session-tasks "niwa-structs" (cl-x))
(declare-function niwa-session-turn-count "niwa-structs" (cl-x))
(declare-function niwa-session-updated-at "niwa-structs" (cl-x))
(declare-function niwa-session-working-directory "niwa-structs" (cl-x))
(declare-function niwa-session-workspace "niwa-structs" (cl-x))
(declare-function niwa-session-worktree-base-commit "niwa-structs" (cl-x))
(declare-function niwa-session-worktree-branch "niwa-structs" (cl-x))
(declare-function niwa-session-worktree-directory "niwa-structs" (cl-x))
(declare-function niwa-session-worktree-source-root "niwa-structs" (cl-x))
(declare-function niwa-task--create "niwa-structs" (&rest slots))
(declare-function niwa-task-blocked-by "niwa-structs" (cl-x))
(declare-function niwa-task-completed-turn "niwa-structs" (cl-x))
(declare-function niwa-task-description "niwa-structs" (cl-x))
(declare-function niwa-task-id "niwa-structs" (cl-x))
(declare-function niwa-task-metadata "niwa-structs" (cl-x))
(declare-function niwa-task-normalize-owner "niwa-structs" (owner agent-registry))
(declare-function niwa-task-owner "niwa-structs" (cl-x))
(declare-function niwa-task-prune-resolved-dependencies "niwa-structs" (tasks))
(declare-function niwa-task-status "niwa-structs" (cl-x))
(declare-function niwa-task-subject "niwa-structs" (cl-x))
(declare-function niwa-workspace-name "niwa-structs" (cl-x))
(declare-function niwa-workspace-root "niwa-structs" (cl-x))
(declare-function niwa-workspace-type "niwa-structs" (cl-x))
(autoload 'niwa-agent-path-p "niwa-structs")

;; `niwa-utilities'
(declare-function niwa--plain-data-p "niwa-utilities" (value))
(autoload 'niwa--plain-data-p "niwa-utilities")

;; `niwa-workspace'
(defvar niwa-workspace-additional-roots)

;; `niwa-workspace-identity'
(declare-function niwa-workspace-identity-read "niwa-workspace-identity" (root))
(autoload 'niwa-workspace-identity-read "niwa-workspace-identity")

;;
;;; Constants

(defconst niwa-session-codec-format-version "v0.6.0"
  "Current on-disk session sidecar format.

The authority profile is part of this format.  Readers accept exactly this
value and intentionally do not migrate older sidecars.")

(defconst niwa-session-codec-naming-input-limit 2000
  "Maximum characters of authored naming input a session retains.")

(defconst niwa-session-codec--allowed-permission-actions
  '(allow deny ask)
  "Permission rule actions recognised by this version.
Rules with other actions are dropped on load (a future version may
add more, and we don't want to act on actions we don't understand).")

(defconst niwa-session-codec--required-sidecar-keys
  '(:version :session-id :session-name :naming-state :naming-input :workspace :working-directory
    :authority-mode :target-incarnation
    :created-at :updated-at :current-segment :total-turn-count
    :last-task-write-turn :task-status-notes :first-user-message
    :latest-user-message :forked-from-session-id :forked-from-turn
    :fork-type :forked-from-fork-point-id
    :worktree-source-root :worktree-directory :worktree-branch
    :worktree-base-commit
    :permission-mode :sandbox-mode :plan-mode :permission-rules :resource-grants
    :preset-name :model-provider :reasoning-effort :external-conversations
	     :recovery-issues :pending-follow-ups :pending-steering :pending-input-next-id
	     :pending-input-paused :pending-input-failure-paused
    :last-observed-date
    :agent-types-snapshot :workspace-instruction-hashes
    :additional-roots :tasks
    :prompt-index :chapters :file-snapshots :ptc-checkpoints :agent-transcripts :agent-registry
    :agent-turn-capacity :plan-metadata :goal :attached-artifacts :dedicated-artifact :messages
    :schedules)
  "Keys required in every current-version session sidecar.")

(defun niwa-session-codec-portable-authority-p (session)
  "Return non-nil when SESSION uses the portable lease authority."
  (eq (niwa-session-authority-mode-for-session session) 'portable))

(defun niwa-session-codec-validate-authority-mode (mode)
  "Validate the sole persisted authority MODE, `portable'."
  (when (eq mode 'pid-lock)
    (error "Unsupported transcript format (before portable leases)"))
  (unless (eq mode 'portable)
    (error "Invalid session authority mode: %S" mode))
  mode)

(defun niwa-session-codec-authority-mode-for-path
    (session-dir &optional session explicit-mode)
  "Return the authority mode for SESSION-DIR.

SESSION or EXPLICIT-MODE is required for a fresh path with no control
artifact.  Existing control artifacts are only a narrow cold-start fallback
for callers that intentionally operate before hydration; contradictory
artifacts fail closed.  A fresh path without a session or explicit profile is
an error rather than an implicit PID-lock fallback."
  (unless (and (stringp session-dir)
               (file-name-absolute-p session-dir))
    (error "Session path is unavailable: %S" session-dir))
  (let* ((lease (file-name-concat session-dir ".lease"))
         ;; A decided profile needs no inference probes, and every control
         ;; probe is one target process.
         (decided (or explicit-mode
                      (and session
                           (niwa-session-authority-mode-for-session
                            session))))
         (mode
          (or decided
              (progn
                (niwa-session-durability--assert-no-pid-lock session-dir)
                (if (niwa-session-control-fs-path-exists-p lease)
                    'portable
                  (let ((sidecar
                         (file-name-concat session-dir "session.meta.el")))
                    (unless (niwa-session-control-fs-path-exists-p sidecar)
                      (error "Session authority profile is unavailable: %s"
                             session-dir))
                    (let ((plist (niwa-session-codec-read sidecar)))
                      (niwa-session-codec-validate-authority-mode
                       (plist-get plist :authority-mode)))))))))
    ;; Preserve the transaction's batched proof of legacy `.lock' absence.
    (when decided
      (niwa-session-codec-validate-authority-mode mode)
      (niwa-session-durability--assert-no-pid-lock session-dir))
    mode))


;;
;;; Workspace serialization

(defun niwa-session-codec--sanitize-workspace-instruction-hashes
    (value)
  "Return valid persisted workspace instruction hashes from VALUE."
  (when (proper-list-p value)
    (cl-loop for entry in value
             for key = (car-safe entry)
             for hash = (cdr-safe entry)
             when (and (proper-list-p key)
                       (= (length key) 2)
                       (stringp (car key))
                       (or (equal (car key) "/root")
                           (niwa-agent-path-p (car key)))
                       (stringp (cadr key))
                       (file-name-absolute-p (cadr key))
                       (or (null hash)
                           (and (stringp hash)
                                (string-match-p "\\`[[:xdigit:]]\\{64\\}\\'" hash))))
             collect (cons (copy-sequence key) hash))))

(defun niwa-session-codec--workspace-to-plist (workspace)
  "Convert WORKSPACE to a plist for sidecar storage.

The project-owned identity and target-native root are portable across
client-specific TRAMP spellings.  The process-local workspace id and file
cache are not persisted."
  (when workspace
    (let* ((root (niwa-workspace-root workspace))
           (identity (niwa-workspace-identity-read root))
           (target (niwa-execution-target-create root)))
      (unless identity
        (error "Workspace identity is not materialized: %s" root))
      (list :type (niwa-workspace-type workspace)
            :workspace-id identity
            :target-native-root
            (niwa-execution-target-native-root target)
            :name (niwa-workspace-name workspace)))))

(defun niwa-session-codec--workspace-from-plist (plist workspace)
  "Bind PLIST to the currently opened WORKSPACE.

The sidecar never supplies a live filesystem authority.  Resume must bind it
to the workspace through which the user opened the session.  Return
`(WORKSPACE . IDENTITY-CHANGED-P)'.  Rebinding a different project-owned
identity requires confirmation so callers can discard copied authority."
  (unless (and (proper-list-p plist) workspace)
    (error "Invalid persisted workspace"))
  (let ((saved-identity (plist-get plist :workspace-id))
        (saved-root (plist-get plist :target-native-root))
        (current-identity
         (niwa-workspace-identity-read
          (niwa-workspace-root workspace))))
    (unless (and (stringp saved-identity)
                 (string-match-p "\\`[0-9a-f]\\{64\\}\\'" saved-identity))
      (error "Invalid persisted workspace identity"))
    (unless (and (stringp saved-root)
                 (file-name-absolute-p saved-root)
                 (not (file-remote-p saved-root)))
      (error "Invalid target-native workspace root: %S" saved-root))
    (unless current-identity
      (error "Workspace identity is missing: %s"
             (niwa-workspace-root workspace)))
    (let ((changed-p (not (equal saved-identity current-identity))))
      (when (and changed-p
                 (not
                  (yes-or-no-p
                   (concat
                    "This session belongs to a different workspace identity. "
                    "Resume against the opened project and discard copied "
                    "session permissions, resource grants, and additional "
                    "roots? "))))
        (user-error "Workspace identity change was not accepted"))
      (cons workspace changed-p))))


;;
;;; Permission rule hygiene

(defun niwa-session-codec--filter-permission-rules (rules)
  "Drop RULES whose `:action' is unrecognised.
A rule is `(TOOL-NAME &rest PLIST)' with `:action SYMBOL'."
  (cl-remove-if-not
   (lambda (rule)
     (and (consp rule)
          (memq (plist-get (cdr rule)
                           :action)
                niwa-session-codec--allowed-permission-actions)))
   rules))

(defun niwa-session-codec--filter-resource-grants (grants)
  "Keep well-formed resource GRANTS.
A grant is `(:path ABSOLUTE-PATH :access read-or-write)', optionally
carrying `:recursive t' for directory-tree scope."
  (cl-remove-if-not
   (lambda (grant)
     (and (proper-list-p grant)
          (stringp (plist-get grant :path))
          (file-name-absolute-p (plist-get grant :path))
          (memq (plist-get grant :access) '(read write))
          (memq (plist-get grant :recursive) '(t nil))))
   grants))


;;
;;; Working directory restore

(defun niwa-session-codec--working-directory-from-plist
    (plist workspace target)
  "Return PLIST's restored working directory for WORKSPACE.

TARGET qualifies the persisted target-native path through the current
client's live filesystem authority.  Paths below a prior target-native root
are mapped below the opened workspace root before containment is checked."
  (let* ((raw (plist-get plist :working-directory))
         (saved-workspace (plist-get plist :workspace))
         (saved-root
          (file-name-as-directory
           (expand-file-name
            (plist-get saved-workspace :target-native-root))))
         (current-native-root
          (niwa-execution-target-native-root target)))
    (unless (and (stringp raw)
                 (file-name-absolute-p raw)
                 (not (file-remote-p raw)))
      (error "Invalid target-native working directory: %S" raw))
    (let* ((native-directory (file-name-as-directory (expand-file-name raw)))
           (mapped
            (cond
             ((string-prefix-p current-native-root native-directory)
              native-directory)
             ((string-prefix-p saved-root native-directory)
              (concat current-native-root
                      (substring native-directory (length saved-root))))
             (t native-directory)))
           (dir
            (file-name-as-directory
             (niwa-execution-target-expand-path target mapped)))
           (root (niwa-workspace-root workspace)))
      (unless (file-in-directory-p dir root)
        (user-error "Working directory must be inside workspace root %s" root))
      dir)))


;;
;;; Goal serialization

(defun niwa-session-codec--goal-to-plist (goal)
  "Serialize GOAL to a sidecar plist."
  (append (when (niwa-goal-reason-diagnostic goal) '(:reason-diagnostic t))
          (list :id (niwa-goal-id goal)
        :objective (niwa-goal-objective goal)
        :status (niwa-goal-status goal)
        :reason (niwa-goal-reason goal)
        :implementation-policy
        (copy-tree
         (niwa-implementation-policy-validate
          (niwa-goal-implementation-policy goal)))
        :token-budget (niwa-goal-token-budget goal)
        :tokens-used (niwa-goal-tokens-used goal)
        :tokens-incomplete-p (niwa-goal-tokens-incomplete-p goal)
        :time-used-seconds (niwa-goal-time-used-seconds goal)
        :turns-run (niwa-goal-turns-run goal)
        :plan-reference (niwa-goal-plan-reference goal)
        :created-at (niwa-goal-created-at goal)
        :updated-at (niwa-goal-updated-at goal))))

(defun niwa-session-codec--goal-from-plist (plist)
  "Reconstruct a `niwa-goal' from PLIST, or nil."
  (when plist
    (let ((keys '(:id :objective :status :reason :implementation-policy
                  :token-budget :tokens-used :tokens-incomplete-p
                  :time-used-seconds :turns-run :plan-reference
                  :created-at :updated-at)))
      (unless
          (and (proper-list-p plist)
               (cl-evenp (length plist))
               (= (length plist) (+ (* 2 (length keys))
                                    (if (plist-member plist :reason-diagnostic) 2 0)))
               (cl-every (lambda (key) (plist-member plist key)) keys)
               (cl-loop for (key _) on plist by #'cddr
                        always (or (memq key keys) (eq key :reason-diagnostic)))
               (stringp (plist-get plist :id))
               (string-match-p "\\`[[:alnum:]_.-]+\\'"
                               (plist-get plist :id))
               (not (member (plist-get plist :id) '("." "..")))
               (stringp (plist-get plist :objective))
               (not (string-blank-p (plist-get plist :objective)))
               (memq (plist-get plist :status)
                     '(active paused blocked budget-limited complete))
               (or (not (plist-member plist :reason-diagnostic))
                   (eq t (plist-get plist :reason-diagnostic)))
               (let ((reason (plist-get plist :reason))
                     (status (plist-get plist :status)))
                 ;; Stopped Goals need a reason; a completed one may carry a
                 ;; note, such as partial verification; an active one has none.
                 (if (null reason)
                     (memq status '(active complete))
                   (and (stringp reason) (not (string-blank-p reason))
                        (not (eq status 'active)))))
               (or (null (plist-get plist :token-budget))
                   (and (integerp (plist-get plist :token-budget))
                        (> (plist-get plist :token-budget) 0)))
               (natnump (plist-get plist :tokens-used))
               (booleanp (plist-get plist :tokens-incomplete-p))
               (natnump (plist-get plist :time-used-seconds))
               (natnump (plist-get plist :turns-run))
               (or (null (plist-get plist :plan-reference))
                   (let ((reference (plist-get plist :plan-reference)))
                     (and (stringp reference)
                          (not (string-empty-p reference))
                          (not (file-name-absolute-p reference))
                          (equal reference
                                 (file-relative-name
                                  (expand-file-name reference "/") "/"))
                          (not (string-prefix-p "../" reference))
                          (not (equal reference "..")))))
               (stringp (plist-get plist :created-at))
               (stringp (plist-get plist :updated-at)))
        (error "Invalid Goal sidecar")))
    (niwa-implementation-policy-validate
     (plist-get plist :implementation-policy))
    (niwa-goal--create
     :id (plist-get plist :id)
     :objective (plist-get plist :objective)
     :status (plist-get plist :status)
     :reason (plist-get plist :reason)
     :reason-diagnostic (plist-get plist :reason-diagnostic)
     :implementation-policy (copy-tree (plist-get plist :implementation-policy))
     :token-budget (plist-get plist :token-budget)
     :tokens-used (plist-get plist :tokens-used)
     :tokens-incomplete-p (plist-get plist :tokens-incomplete-p)
     :time-used-seconds (plist-get plist :time-used-seconds)
     :turns-run (plist-get plist :turns-run)
     :plan-reference (plist-get plist :plan-reference)
     :created-at (plist-get plist :created-at)
     :updated-at (plist-get plist :updated-at))))


;;
;;; Task serialization

(defun niwa-session-codec--task-to-plist (task)
  "Serialize TASK struct to a plist."
  (list :id          (niwa-task-id task)
        :subject     (niwa-task-subject task)
        :description (niwa-task-description task)
        :status      (niwa-task-status task)
        :owner       (niwa-task-owner task)
        :blocked-by  (niwa-task-blocked-by task)
        :completed-turn (niwa-task-completed-turn task)
        :metadata    (niwa-task-metadata task)))

(defun niwa-session-codec--task-from-plist
    (plist &optional agent-registry)
  "Reconstruct a `niwa-task' from PLIST."
  (niwa-task--create
   :id          (plist-get plist :id)
   :subject     (plist-get plist :subject)
   :description (plist-get plist :description)
   :status      (plist-get plist :status)
   :owner       (niwa-task-normalize-owner
                 (plist-get plist :owner) agent-registry)
   :blocked-by  (plist-get plist :blocked-by)
   :completed-turn (plist-get plist :completed-turn)
   :metadata    (plist-get plist :metadata)))


;;
;;; Top-level serialize / deserialize

(cl-defun niwa-session-codec-serialize (session
                                                 &key
                                                 first-user-message
                                                 latest-user-message
                                                 additional-roots)
  "Serialize SESSION to a sidecar plist.

FIRST-USER-MESSAGE is the cached original-request preview.
LATEST-USER-MESSAGE is the cached resume picker preview.
ADDITIONAL-ROOTS is the buffer-local value of
`niwa-workspace-additional-roots' for this session.

The resulting plist is round-trippable via
`niwa-session-codec-deserialize'."
  (let* ((execution-target (niwa-session-execution-target session))
         (authority-mode
          (niwa-session-authority-mode-for-session session))
         (target-incarnation
          (niwa-execution-target-incarnation execution-target))
         (permission-mode
         (or (niwa-session-permission-mode session)
             (and (boundp 'niwa-permission-mode)
                  (default-toplevel-value 'niwa-permission-mode))
             'ask))
        (sandbox-mode
         (or (niwa-session-sandbox-mode session)
             (and (boundp 'niwa-sandbox-mode)
                  (default-toplevel-value 'niwa-sandbox-mode))
             'best-effort))
        (authority
         (or
          (niwa-permission-serialize-authority
           (niwa-session-permission-rules session)
           (niwa-session-resource-grants session)
           execution-target)
          (error "Session permission authority is not portable")))
        (workspace-plist
         (niwa-session-codec--workspace-to-plist
          (niwa-session-workspace session))))
    (niwa-session-codec-validate-authority-mode authority-mode)
    (unless (and (stringp target-incarnation)
                 (string-match-p "\\S-" target-incarnation))
      (error "Target incarnation is not available"))
    (unless (memq permission-mode '(ask edits full-auto))
      (error "Invalid persisted permission mode: %S" permission-mode))
    (unless (memq sandbox-mode '(best-effort required off))
      (error "Invalid persisted sandbox mode: %S" sandbox-mode))
    (niwa-session-codec--check-naming (niwa-session-naming-state session)
                                         (niwa-session-naming-input session))
    (list
   :version                niwa-session-codec-format-version
   :session-id             (niwa-session-session-id session)
   :session-name           (niwa-session-name session)
   :naming-state           (niwa-session-naming-state session)
   :naming-input           (niwa-session-naming-input session)
   :workspace              workspace-plist
   :authority-mode         authority-mode
   :working-directory
   (niwa-execution-target-native-path
    execution-target
    (or (niwa-session-working-directory session)
        (niwa-workspace-root (niwa-session-workspace session))))
   :target-incarnation     target-incarnation
   :created-at             (niwa-session-created-at session)
   :updated-at             (niwa-session-updated-at session)
   :current-segment        (or (niwa-session-current-segment session) 1)
   :total-turn-count       (or (niwa-session-turn-count session) 0)
   :last-task-write-turn   (niwa-session-last-task-write-turn session)
   :task-status-notes      (niwa-session-task-status-notes session)
   :first-user-message     first-user-message
   :latest-user-message    latest-user-message
   :forked-from-session-id (niwa-session-forked-from-session-id session)
   :forked-from-turn       (niwa-session-forked-from-turn session)
   :fork-type              (niwa-session-fork-type session)
   :forked-from-fork-point-id
   (niwa-session-forked-from-fork-point-id session)
   :worktree-source-root (niwa-session-worktree-source-root session)
   :worktree-directory   (niwa-session-worktree-directory session)
   :worktree-branch      (niwa-session-worktree-branch session)
   :worktree-base-commit (niwa-session-worktree-base-commit session)
   :permission-mode        permission-mode
   :sandbox-mode           sandbox-mode
   :plan-mode              (and (niwa-session-plan-mode session) t)
   :permission-rules       (plist-get authority :rules)
   :resource-grants        (plist-get authority :resource-grants)
   :preset-name            (niwa-session-preset-name session)
     :recovery-issues        (copy-tree (niwa-session-recovery-issues session))
     :pending-follow-ups     (niwa-recovery-persist-inputs session)
     :pending-steering       (niwa-recovery-persist-inputs session t)
     :pending-input-next-id  (or (niwa-session-pending-input-next-id session) 0)
     :pending-input-paused   (niwa-session-pending-input-paused session)
     :pending-input-failure-paused (niwa-session-pending-input-failure-paused session)
   :model-provider         (niwa-session-model-provider session)
   :external-conversations (copy-tree (niwa-session-external-conversations session))
   :reasoning-effort       (niwa-session-reasoning-effort session)
   :last-observed-date     (niwa-session-last-observed-date session)
   :agent-types-snapshot   (niwa-session-agent-types-snapshot session)
   :workspace-instruction-hashes
   (copy-tree (niwa-session-workspace-instruction-hashes session) t)
   :additional-roots       additional-roots
   :tasks                  (mapcar #'niwa-session-codec--task-to-plist
                                   (niwa-session-tasks session))
   :prompt-index           (niwa-session-prompt-index session)
   :chapters               (copy-tree (niwa-session-chapters session))
   :file-snapshots         (niwa-session-file-snapshots session)
   :untracked-effects      (niwa-session-untracked-effects session)
   :ptc-checkpoints        (copy-tree (niwa-session-ptc-checkpoints session) t)
   :agent-transcripts      (niwa-session-agent-transcripts session)
   :agent-registry         (niwa-agent-persistence-serialize-registry session)
   :agent-turn-capacity    (niwa-session-agent-turn-capacity session)
   :plan-metadata          (niwa-session-plan-metadata session)
   :goal                   (when-let* ((goal (niwa-session-goal session)))
                             (niwa-session-codec--goal-to-plist goal))
   :attached-artifacts     (niwa-session-attached-artifacts session)
   :dedicated-artifact     (niwa-session-dedicated-artifact session)
   :note-conversation      (niwa-session-note-conversation session)
   :schedules              (copy-tree (niwa-session-schedules session))
   ;; Root's reverse-order unread queue.  Child queues live on their explicit
   ;; registry records and all queues become FIFO only at delivery time.
   :messages
   (niwa-agent-persistence-sanitize-mailbox
    (niwa-session-messages session) "/root"))))

(defun niwa-session-codec-sanitize-agent-transcripts (raw)
  "Sanitize the `:agent-transcripts' alist RAW read from a sidecar.

Coerce unknown status values to `incomplete'.  Deduplicate agent IDs by
keeping the entry with the newest `:updated-at'.  Unknown plist keys survive
the round trip but are ignored when rendered, except the retired
`:activity' history: nothing reads it, and it made up a third of a large
session's sidecar, rewritten by every save."
  (let ((seen (make-hash-table :test #'equal))
        out)
    (dolist (entry (and (listp raw) raw))
      (when (and (consp entry)
                 (stringp (car entry))
                 (listp (cdr entry)))
        (let* ((id (car entry))
               (plist (copy-sequence (cdr entry)))
               (status (plist-get plist :status))
               (existing (gethash id seen)))
          (cl-remf plist :activity)
          (unless (memq status '(running completed error aborted incomplete))
            (setq plist (plist-put plist :status 'incomplete)))
          (cond
           ((null existing)
            (puthash id plist seen)
            (push (cons id plist) out))
           ((let ((new-time (plist-get plist :updated-at))
                  (old-time (plist-get existing :updated-at)))
              (and (stringp new-time)
                   (stringp old-time)
                   (string> new-time old-time)))
            (puthash id plist seen)
            (setf (alist-get id out nil nil #'equal) plist))))))
    (nreverse out)))

(defun niwa-session-codec--sanitize-ptc-checkpoints (raw)
  "Return closed, read-safe ToolCall checkpoints from sidecar value RAW."
  (cl-loop
   for checkpoint in (and (proper-list-p raw) raw)
   for id = (and (proper-list-p checkpoint) (plist-get checkpoint :id))
   for conversation = (and (proper-list-p checkpoint)
                           (plist-get checkpoint :conversation))
   for args = (and (proper-list-p checkpoint) (plist-get checkpoint :args))
   for script = (and (proper-list-p args) (plist-get args :expression))
   for state = (and (proper-list-p checkpoint)
                    (plist-get checkpoint :state))
   for result = (and (proper-list-p checkpoint)
                     (plist-get checkpoint :result))
   for render-data = (and (proper-list-p checkpoint)
                          (plist-get checkpoint :render-data))
   when (and (stringp id) (<= (length id) 256)
             (niwa-agent-path-p conversation)
             (memq state '(running settled))
             (stringp script) (<= (length script) (* 64 1024))
             (or (null result) (stringp result))
             (or (null render-data) (niwa--plain-data-p render-data)))
   collect (list :id id :conversation conversation
                 :args (list :expression script) :state state
                 :result result
                 :render-data (copy-tree render-data t))))

(defun niwa-session-codec--check-naming (state input)
  "Signal unless naming STATE and recorded INPUT form a valid pair.
Only a consumed automatic attempt (`attempted') may retain INPUT: a
nonblank property-free string of at most 2,000 characters, or nil when the
authored prompt was never recorded."
  (unless (memq state '(pending attempted explicit))
    (error "Invalid persisted naming state: %S" state))
  (unless (or (null input)
              (and (eq state 'attempted) (stringp input)
                   (<= (length input) niwa-session-codec-naming-input-limit)
                   (string-match-p "\\S-" input)
                   (null (text-properties-at 0 input))
                   (null (next-property-change 0 input))))
    (error "Invalid persisted naming input for state %S" state)))

(defun niwa-session-codec--chapter-p (chapter)
  "Return non-nil when CHAPTER is a valid closed-chat record.
A record names the last segment of a chat that `/clear' closed, the title
it had then, or nil, and when it closed."
  (and (proper-list-p chapter)
       (natnump (plist-get chapter :segment))
       (> (plist-get chapter :segment) 0)
       (let ((title (plist-get chapter :title)))
         (or (null title) (stringp title)))
       (stringp (plist-get chapter :closed-at))))

(defun niwa-session-codec-validate-current-sidecar (plist)
  "Return PLIST when it contains every current-version sidecar key."
  (unless (proper-list-p plist)
    (error "Invalid session sidecar"))
  (dolist (key niwa-session-codec--required-sidecar-keys)
    (unless (plist-member plist key)
      (error "Missing session sidecar key: %s" key)))
  (let ((histories (plist-get plist :external-conversations)) scopes)
    (unless (proper-list-p histories) (error "Invalid external conversation histories"))
    (dolist (entry histories)
      (unless (and (consp entry) (stringp (car entry))
                   (not (string-empty-p (car entry)))
                   (not (member (car entry) scopes))
                   (proper-list-p (cdr entry))
                   (eq 'claude-code (plist-get (cdr entry) :engine))
                   (if (eq 'unstarted (plist-get (cdr entry) :state))
                       ;; No installed history or effects can exist yet.
                       (and (cl-evenp (length (cdr entry)))
                            (cl-loop for (key _value) on (cdr entry) by #'cddr
                                     always (memq key '(:engine :state :boundaries))))
                     (and
                      (cl-every (lambda (key)
                                  (let ((value (plist-get (cdr entry) key)))
                                    (and (stringp value) (string-match-p "\\S-" value))))
                                '(:id :host :directory))
                      (file-name-absolute-p (plist-get (cdr entry) :directory))
                      (not (file-remote-p (plist-get (cdr entry) :directory)))
                      (memq (plist-get (cdr entry) :state) '(ready in-flight uncertain diverged)))))
        (error "Invalid external conversation history: %S" entry))
      (when-let* ((boundary (plist-get (cdr entry) :input-boundary)))
        (unless (and (consp boundary) (natnump (car boundary)) (natnump (cdr boundary)))
          (error "Invalid external submitted-input boundary")))
      (let ((boundaries (plist-get (cdr entry) :boundaries)) turns)
        (unless (proper-list-p boundaries) (error "Invalid external history boundaries"))
        (dolist (boundary boundaries)
          (unless (and (proper-list-p boundary) (cl-evenp (length boundary))
                       (natnump (plist-get boundary :turn))
                       (not (memq (plist-get boundary :turn) turns))
                       (if (plist-get boundary :detached)
                           (and (eq t (plist-get boundary :detached))
                                (= 4 (length boundary)))
                         (and (cl-every (lambda (key)
                                          (let ((value (plist-get boundary key)))
                                            (and (stringp value) (string-match-p "\\S-" value))))
                                        '(:id :message-id :host :directory))
                              (file-name-absolute-p (plist-get boundary :directory))
                              (not (file-remote-p (plist-get boundary :directory)))
                              (let ((input (plist-get boundary :input-boundary)))
                                (or (null input)
                                    (and (consp input) (natnump (car input)) (natnump (cdr input))))))))
            (error "Invalid external history boundary: %S" boundary))
          (push (plist-get boundary :turn) turns)))
      (push (car entry) scopes)))
  (niwa-session-codec-validate-authority-mode
   (plist-get plist :authority-mode))
  (let ((incarnation (plist-get plist :target-incarnation)))
    (unless (and (stringp incarnation)
                 (string-match-p "\\S-" incarnation))
      (error "Invalid persisted target incarnation: %S" incarnation))
    incarnation)
  (niwa-session-codec--check-naming (plist-get plist :naming-state)
                                       (plist-get plist :naming-input))
  (unless (memq (plist-get plist :permission-mode) '(ask edits full-auto))
    (error "Invalid persisted permission mode: %S"
           (plist-get plist :permission-mode)))
  (unless (memq (plist-get plist :sandbox-mode)
                '(best-effort required off))
    (error "Invalid persisted sandbox mode: %S"
           (plist-get plist :sandbox-mode)))
  (unless (booleanp (plist-get plist :plan-mode))
    (error "Invalid persisted Plan mode: %S" (plist-get plist :plan-mode)))
  (unless (and (proper-list-p (plist-get plist :recovery-issues))
               (proper-list-p (plist-get plist :pending-follow-ups))
               (proper-list-p (plist-get plist :pending-steering))
               (natnump (plist-get plist :pending-input-next-id))
               (booleanp (plist-get plist :pending-input-paused))
               (booleanp (plist-get plist :pending-input-failure-paused)))
    (error "Invalid recovery records"))
  (dolist (issue (plist-get plist :recovery-issues))
    (unless (and (proper-list-p issue) (cl-evenp (length issue))
                 (stringp (plist-get issue :id)) (stringp (plist-get issue :category))
                 (stringp (plist-get issue :message))
                 (memq (plist-get issue :blocking) '(nil t))
                 (cl-loop for (key _) on issue by #'cddr
                          always (memq key '(:id :category :message :blocking))))
      (error "Invalid recovery issue")))
  (dolist (entry (append (plist-get plist :pending-follow-ups)
                         (plist-get plist :pending-steering)))
    (unless (and (proper-list-p entry) (cl-evenp (length entry))
                 (niwa--plain-data-p entry)
                 (stringp (plist-get entry :input))
                 (integerp (plist-get entry :id))
                 (memq (plist-get entry :category) '(follow-up steering))
                 (memq (plist-get entry :guest-role) '(nil full owner))
                 (cl-loop for (key _) on entry by #'cddr
                          always (memq key niwa-recovery--input-keys)))
      (error "Invalid retained input")))
  (unless (or (null (plist-get plist :model-provider))
              (and (stringp (plist-get plist :model-provider))
                   (string-match-p
                    "\\`[^:]+:.+\\'"
                    (plist-get plist :model-provider))))
    (error "Invalid persisted model provider: %S"
           (plist-get plist :model-provider)))
  (unless (or (null (plist-get plist :reasoning-effort))
              (symbolp (plist-get plist :reasoning-effort)))
    (error "Invalid persisted reasoning effort: %S"
           (plist-get plist :reasoning-effort)))
  (unless (and (integerp (plist-get plist :agent-turn-capacity))
               (> (plist-get plist :agent-turn-capacity) 0))
    (error "Invalid persisted agent turn capacity: %S"
           (plist-get plist :agent-turn-capacity)))
  (dolist (segment (plist-get plist :prompt-index))
    (unless (and (consp segment) (integerp (car segment)))
      (error "Invalid session prompt-index segment: %S" segment))
    (dolist (prompt (cdr segment))
      (unless (and (proper-list-p prompt)
                   (cl-every (lambda (key) (plist-member prompt key))
                             '(:turn :file-turn :cum-turn)))
        (error "Invalid session prompt entry: %S" prompt))))
  (let ((chapters (plist-get plist :chapters)))
    (unless (and (proper-list-p chapters)
                 (cl-every #'niwa-session-codec--chapter-p chapters))
      (error "Invalid session chapters: %S" chapters)))
  (let ((ids (plist-get plist :attached-artifacts)))
    (unless (and (proper-list-p ids) (cl-every #'stringp ids))
      (error "Invalid attached artifacts: %S" ids)))
  (let ((id (plist-get plist :dedicated-artifact)))
    (unless (or (null id) (stringp id))
      (error "Invalid dedicated artifact: %S" id)))
  (let ((schedules (plist-get plist :schedules)))
    (unless (and (proper-list-p schedules)
                 (cl-every #'niwa-schedule-entry-p schedules))
      (error "Invalid session schedules: %S" schedules)))
  plist)

(defun niwa-session-codec-deserialize (plist workspace)
  "Reconstruct a session from sidecar PLIST for opened WORKSPACE.

Returns a plist:
  (:session SESSION
   :first-user-message STR-OR-NIL
   :latest-user-message STR-OR-NIL
   :additional-roots ALIST)

Where SESSION is a freshly-created `niwa-session' struct populated
from PLIST.  The auxiliary fields (first-user-message,
latest-user-message, additional-roots) are returned alongside because
they are not on the session struct.

Only the current sidecar version is accepted.  Permission rules with
unknown actions and task state with invalid agent owners are dropped via
their hygiene filters."
  (unless (equal (plist-get plist :version)
                 niwa-session-codec-format-version)
    (error "Unsupported session version: %s"
           (or (plist-get plist :version) "missing")))
  (niwa-session-codec-validate-current-sidecar plist)
  (let* ((workspace-binding
          (niwa-session-codec--workspace-from-plist
           (plist-get plist :workspace) workspace))
         (workspace (car workspace-binding))
         (workspace-identity-changed-p (cdr workspace-binding))
         (execution-target
          (let* ((target
                  (niwa-execution-target-create
                   (niwa-workspace-root workspace)))
                 (incarnation (plist-get plist :target-incarnation)))
            (niwa-execution-target-restore-incarnation
             target incarnation)
            target))
         (working-directory
          (niwa-session-codec--working-directory-from-plist
           plist workspace execution-target))
         (persisted-rules
          (unless workspace-identity-changed-p
            (niwa-session-codec--filter-permission-rules
             (plist-get plist :permission-rules))))
         (persisted-resource-grants
          (unless workspace-identity-changed-p
            (niwa-session-codec--filter-resource-grants
             (plist-get plist :resource-grants))))
         (authority
          (niwa-permission-deserialize-authority
           persisted-rules
           persisted-resource-grants
           execution-target))
         (rules (and authority (plist-get authority :rules)))
         (resource-grants
          (and authority (plist-get authority :resource-grants)))
         (prompt-index (plist-get plist :prompt-index))
         (latest-user-message (plist-get plist :latest-user-message))
         (raw-agent-registry (plist-get plist :agent-registry))
         (agent-registry
          (niwa-agent-persistence-deserialize-registry
           raw-agent-registry execution-target))
         (tasks
          (niwa-task-prune-resolved-dependencies
           (delq
            nil
            (mapcar
             (lambda (task-plist)
               (condition-case nil
                   (niwa-session-codec--task-from-plist
                    task-plist agent-registry)
                 (error nil)))
             (plist-get plist :tasks)))))
         (task-status-notes
          (cl-loop
           for entry in (and (proper-list-p
                              (plist-get plist :task-status-notes))
                             (plist-get plist :task-status-notes))
           when (consp entry)
           for normalized
           = (condition-case nil
                 (cons t
                       (niwa-task-normalize-owner
                        (car entry) agent-registry))
               (error nil))
           when normalized
           collect (cons (cdr normalized) (copy-tree (cdr entry)))))
         (session   (niwa-session--create
                     :name             (plist-get plist :session-name)
                     :naming-state     (plist-get plist :naming-state)
                     :naming-input     (plist-get plist :naming-input)
                     :workspace        workspace
                     :execution-target execution-target
                     :authority-mode  (plist-get plist :authority-mode)
                     :working-directory working-directory
                     :touched-files    (make-hash-table :test #'equal)
                     :mentions-shown   (make-hash-table :test #'equal)
                     :tasks            tasks
                     :permission-rules rules
                     :resource-grants  resource-grants
                     :permission-mode  (plist-get plist :permission-mode)
                     :sandbox-mode     (plist-get plist :sandbox-mode)
                     :plan-mode        (plist-get plist :plan-mode)
                     :preset-name      (if (eq (plist-get plist :preset-name) 'niwa-note)
                                           'niwa-implement
                                         (plist-get plist :preset-name))
                     :note-conversation
                     (and (not (plist-get plist :forked-from-session-id))
                          (or (plist-get plist :note-conversation)
                              (eq (plist-get plist :preset-name) 'niwa-note)))
                     :recovery-issues  (copy-tree (plist-get plist :recovery-issues))
                     :pending-follow-ups (mapcar (lambda (entry)
                                                   (let ((copy (copy-tree entry)))
                                                     (when (eq (plist-get copy :state) 'dispatching)
                                                       (plist-put copy :state 'failed-turn))
                                                     copy))
						 (plist-get plist :pending-follow-ups))
                     :pending-steering (mapcar (lambda (entry)
                                                 (let ((copy (copy-tree entry)))
                                                   (if (plist-get entry :request-id)
                                                       (plist-put copy :state 'failed-turn)
                                                     copy)))
                                               (plist-get plist :pending-steering))
                     :pending-input-next-id (plist-get plist :pending-input-next-id)
                     :pending-input-paused (plist-get plist :pending-input-paused)
                     :pending-input-failure-paused (or (and (cl-find-if
                                                             (lambda (entry) (memq (plist-get entry :state) '(dispatching failed-turn)))
                                                             (plist-get plist :pending-follow-ups)) t)
                                                       (and (memq (plist-get (cdr (assoc "root" (plist-get plist :external-conversations))) :state)
                                                                  '(in-flight uncertain)) t)
                                                       (and (cl-find-if
                                                             (lambda (entry)
                                                               (or (plist-get entry :request-id)
                                                                   (eq 'failed-turn (plist-get entry :state))))
                                                             (plist-get plist :pending-steering)) t)
                                                       (plist-get plist :pending-input-failure-paused))
                     :model-provider   (plist-get plist :model-provider)
                     :external-conversations
                     (mapcar (lambda (entry)
                               (let ((record (copy-tree entry)))
                                 (when (eq 'in-flight (plist-get (cdr record) :state))
                                   (plist-put (cdr record) :state 'uncertain))
                                 record))
                             (plist-get plist :external-conversations))
                     :reasoning-effort (plist-get plist :reasoning-effort)
                     :turn-count       (plist-get plist :total-turn-count)
                     :last-observed-date (plist-get plist :last-observed-date)
                     :agent-types-snapshot
                     (plist-get plist :agent-types-snapshot)
                     :workspace-instruction-hashes
                     (niwa-session-codec--sanitize-workspace-instruction-hashes
                      (plist-get plist :workspace-instruction-hashes))
                     :last-task-write-turn
                     (plist-get plist :last-task-write-turn)
                     :task-status-notes task-status-notes
                     :session-id       (plist-get plist :session-id)
                     :created-at       (plist-get plist :created-at)
                     :updated-at       (plist-get plist :updated-at)
                     :current-segment  (plist-get plist :current-segment)
                     :forked-from-session-id
                     (plist-get plist :forked-from-session-id)
                     :forked-from-turn (plist-get plist :forked-from-turn)
                     :fork-type (plist-get plist :fork-type)
                     :forked-from-fork-point-id
                     (plist-get plist :forked-from-fork-point-id)
                     :worktree-source-root
                     (plist-get plist :worktree-source-root)
                     :worktree-directory
                     (plist-get plist :worktree-directory)
                     :worktree-branch
                     (plist-get plist :worktree-branch)
                     :worktree-base-commit
                     (plist-get plist :worktree-base-commit)
                     :prompt-index     prompt-index
                     :chapters         (copy-tree (plist-get plist :chapters))
                     :file-snapshots   (plist-get plist :file-snapshots)
                     :untracked-effects (plist-get plist :untracked-effects)
                     :ptc-checkpoints
                     (niwa-session-codec--sanitize-ptc-checkpoints
                      (plist-get plist :ptc-checkpoints))
                     :plan-metadata    (plist-get plist :plan-metadata)
                     :goal
                     (condition-case nil
                         (niwa-session-codec--goal-from-plist
                          (plist-get plist :goal))
                       (error nil))
                     :attached-artifacts (plist-get plist :attached-artifacts)
                     :dedicated-artifact (plist-get plist :dedicated-artifact)
                     :agent-transcripts
                     (niwa-session-codec-sanitize-agent-transcripts
                      (plist-get plist :agent-transcripts))
                     :agent-registry agent-registry
                     :agent-turn-capacity
                     (plist-get plist :agent-turn-capacity)
                     :schedules (copy-tree (plist-get plist :schedules))
                     :messages
                     (niwa-agent-persistence-sanitize-mailbox
                      (plist-get plist :messages) "/root"))))
    (niwa-session-checkpoint-seed session plist)
    (when-let* ((goal (niwa-session-goal session))
                ((eq (niwa-goal-status goal) 'active)))
      (setf (niwa-goal-status goal) 'paused
            (niwa-goal-reason goal) "session resumed"
            (niwa-goal-reason-diagnostic goal) nil
            (niwa-goal-updated-at goal)
            (format-time-string "%FT%T%z")))
    (list :session             session
          :first-user-message  (plist-get plist :first-user-message)
          :latest-user-message latest-user-message
          :additional-roots    (unless workspace-identity-changed-p
                                 (plist-get plist :additional-roots))
          :agent-registry-repaired-p
          (not (= (length agent-registry)
                  (length (and (proper-list-p raw-agent-registry)
                               raw-agent-registry)))))))


;;
;;; Sidecar IO

(defun niwa-session-codec-write (path plist)
  "Write sidecar PLIST to PATH atomically.
Uses a temp file created in PATH's own directory so the final
  `rename-file' stays within the same filesystem and is POSIX-atomic
even on setups where the workspace lives on a different mount from
the system temp directory."
  (with-temp-buffer
    (let ((print-length nil)
          (print-level nil)
          (print-circle t)
          (print-quoted t))
      (prin1 plist (current-buffer))
      (niwa-session-control-fs-write-file
       (niwa-session-control-fs-physical-path path)
       (buffer-string)))))

(defun niwa-session-codec-read (path)
  "Read sidecar plist from PATH.
Returns the raw plist.  Caller is responsible for passing it through
`niwa-session-codec-deserialize' for validation and hygiene."
  (with-temp-buffer
    (insert (niwa-session-control-fs-read-file
             (niwa-session-control-fs-physical-path path)))
    (goto-char (point-min))
    (read (current-buffer))))


(provide 'niwa-session-codec)

;;; niwa-session-codec.el ends here
