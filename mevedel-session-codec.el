;;; mevedel-session-codec.el --- Session sidecar codec and validation -*- lexical-binding: t -*-

;;; Commentary:

;; Owns the closed session sidecar schema, validation, and literal IO.

;;; Code:

(require 'cl-lib)

(eval-when-compile
  (require 'mevedel-agents)
  (require 'mevedel-structs))

;; `mevedel-agent-persistence'
(declare-function mevedel-agent-persistence-deserialize-registry "mevedel-agent-persistence" (raw))
(declare-function mevedel-agent-persistence-sanitize-mailbox "mevedel-agent-persistence" (raw recipient))
(declare-function mevedel-agent-persistence-serialize-registry "mevedel-agent-persistence" (session))
(autoload 'mevedel-agent-persistence-deserialize-registry
  "mevedel-agent-persistence")
(autoload 'mevedel-agent-persistence-sanitize-mailbox
  "mevedel-agent-persistence")
(autoload 'mevedel-agent-persistence-serialize-registry
  "mevedel-agent-persistence")

;; `mevedel-execution-target'
(declare-function mevedel-execution-target-create "mevedel-execution-target" (workspace-root))
(declare-function mevedel-execution-target-expand-path "mevedel-execution-target" (target path &optional directory))
(declare-function mevedel-execution-target-incarnation "mevedel-execution-target" (cl-x))
(declare-function mevedel-execution-target-native-path "mevedel-execution-target" (target path))
(declare-function mevedel-execution-target-native-root "mevedel-execution-target" (cl-x))
(declare-function mevedel-execution-target-restore-incarnation "mevedel-execution-target" (target incarnation))
(autoload 'mevedel-execution-target-create "mevedel-execution-target")
(autoload 'mevedel-execution-target-expand-path "mevedel-execution-target")
(autoload 'mevedel-execution-target-incarnation "mevedel-execution-target")
(autoload 'mevedel-execution-target-native-path "mevedel-execution-target")
(autoload 'mevedel-execution-target-native-root "mevedel-execution-target")
(autoload 'mevedel-execution-target-restore-incarnation
  "mevedel-execution-target")

;; `mevedel-permission-mode'
(defvar mevedel-permission-mode)

;; `mevedel-permission-persistence'
(declare-function mevedel-permission-deserialize-authority "mevedel-permission-persistence" (rules grants target))
(declare-function mevedel-permission-serialize-authority "mevedel-permission-persistence" (rules grants target))
(autoload 'mevedel-permission-deserialize-authority
  "mevedel-permission-persistence")
(autoload 'mevedel-permission-serialize-authority
  "mevedel-permission-persistence")

;; `mevedel-sandbox'
(defvar mevedel-sandbox-mode)

;; `mevedel-session-control-fs'
(declare-function mevedel-session-control-fs-path-exists-p "mevedel-session-control-fs" (path))
(declare-function mevedel-session-control-fs-physical-path "mevedel-session-control-fs" (path))
(declare-function mevedel-session-control-fs-read-file "mevedel-session-control-fs" (path))
(declare-function mevedel-session-control-fs-write-file "mevedel-session-control-fs" (path content))
(autoload 'mevedel-session-control-fs-path-exists-p
  "mevedel-session-control-fs")
(autoload 'mevedel-session-control-fs-physical-path
  "mevedel-session-control-fs")
(autoload 'mevedel-session-control-fs-read-file "mevedel-session-control-fs")
(autoload 'mevedel-session-control-fs-write-file "mevedel-session-control-fs")

;; `mevedel-structs'
(declare-function mevedel-agent-path-p "mevedel-structs" (path))
(declare-function mevedel-goal--create "mevedel-structs" (&rest slots))
(declare-function mevedel-goal-created-at "mevedel-structs" (cl-x))
(declare-function mevedel-goal-id "mevedel-structs" (cl-x))
(declare-function mevedel-goal-objective "mevedel-structs" (cl-x))
(declare-function mevedel-goal-plan-reference "mevedel-structs" (cl-x))
(declare-function mevedel-goal-reason "mevedel-structs" (cl-x))
(declare-function mevedel-goal-status "mevedel-structs" (cl-x))
(declare-function mevedel-goal-time-used-seconds "mevedel-structs" (cl-x))
(declare-function mevedel-goal-token-budget "mevedel-structs" (cl-x))
(declare-function mevedel-goal-tokens-used "mevedel-structs" (cl-x))
(declare-function mevedel-goal-turns-run "mevedel-structs" (cl-x))
(declare-function mevedel-goal-updated-at "mevedel-structs" (cl-x))
(declare-function mevedel-session--create "mevedel-structs" (&rest slots))
(declare-function mevedel-session-agent-turn-capacity "mevedel-structs" (cl-x))
(declare-function mevedel-session-authority-mode-for-session "mevedel-structs" (session))
(declare-function mevedel-session-authority-mode-for-workspace "mevedel-structs" (workspace))
(declare-function mevedel-session-created-at "mevedel-structs" (cl-x))
(declare-function mevedel-session-current-segment "mevedel-structs" (cl-x))
(declare-function mevedel-session-execution-target "mevedel-structs" (cl-x))
(declare-function mevedel-session-file-snapshots "mevedel-structs" (cl-x))
(declare-function mevedel-session-fork-type "mevedel-structs" (cl-x))
(declare-function mevedel-session-forked-from-fork-point-id "mevedel-structs" (cl-x))
(declare-function mevedel-session-forked-from-session-id "mevedel-structs" (cl-x))
(declare-function mevedel-session-forked-from-turn "mevedel-structs" (cl-x))
(declare-function mevedel-session-goal "mevedel-structs" (cl-x))
(declare-function mevedel-session-name "mevedel-structs" (cl-x))
(declare-function mevedel-session-permission-mode "mevedel-structs" (cl-x))
(declare-function mevedel-session-permission-rules "mevedel-structs" (cl-x))
(declare-function mevedel-session-plan-mode "mevedel-structs" (cl-x))
(declare-function mevedel-session-preset-name "mevedel-structs" (cl-x))
(declare-function mevedel-session-prompt-index "mevedel-structs" (cl-x))
(declare-function mevedel-session-ptc-checkpoints "mevedel-structs" (cl-x))
(declare-function mevedel-session-resource-grants "mevedel-structs" (cl-x))
(declare-function mevedel-session-sandbox-mode "mevedel-structs" (cl-x))
(declare-function mevedel-session-session-id "mevedel-structs" (cl-x))
(declare-function mevedel-session-task-status-notes "mevedel-structs" (cl-x))
(declare-function mevedel-session-tasks "mevedel-structs" (cl-x))
(declare-function mevedel-session-turn-count "mevedel-structs" (cl-x))
(declare-function mevedel-session-updated-at "mevedel-structs" (cl-x))
(declare-function mevedel-session-working-directory "mevedel-structs" (cl-x))
(declare-function mevedel-session-workspace "mevedel-structs" (cl-x))
(declare-function mevedel-session-worktree-base-commit "mevedel-structs" (cl-x))
(declare-function mevedel-session-worktree-branch "mevedel-structs" (cl-x))
(declare-function mevedel-session-worktree-directory "mevedel-structs" (cl-x))
(declare-function mevedel-session-worktree-source-root "mevedel-structs" (cl-x))
(declare-function mevedel-task--create "mevedel-structs" (&rest slots))
(declare-function mevedel-task-blocked-by "mevedel-structs" (cl-x))
(declare-function mevedel-task-completed-turn "mevedel-structs" (cl-x))
(declare-function mevedel-task-description "mevedel-structs" (cl-x))
(declare-function mevedel-task-id "mevedel-structs" (cl-x))
(declare-function mevedel-task-metadata "mevedel-structs" (cl-x))
(declare-function mevedel-task-normalize-owner "mevedel-structs" (owner agent-registry))
(declare-function mevedel-task-owner "mevedel-structs" (cl-x))
(declare-function mevedel-task-prune-resolved-dependencies "mevedel-structs" (tasks))
(declare-function mevedel-task-status "mevedel-structs" (cl-x))
(declare-function mevedel-task-subject "mevedel-structs" (cl-x))
(declare-function mevedel-workspace-name "mevedel-structs" (cl-x))
(declare-function mevedel-workspace-root "mevedel-structs" (cl-x))
(declare-function mevedel-workspace-type "mevedel-structs" (cl-x))
(autoload 'mevedel-agent-path-p "mevedel-structs")

;; `mevedel-utilities'
(declare-function mevedel--plain-data-p "mevedel-utilities" (value))
(autoload 'mevedel--plain-data-p "mevedel-utilities")

;; `mevedel-workspace'
(defvar mevedel-workspace-additional-roots)

;; `mevedel-workspace-identity'
(declare-function mevedel-workspace-identity-read "mevedel-workspace-identity" (root))
(autoload 'mevedel-workspace-identity-read "mevedel-workspace-identity")

;;
;;; Constants

(defconst mevedel-session-codec-format-version "v0.5.4"
  "Current on-disk session sidecar format.

The authority profile is part of this format.  Readers accept exactly this
value and intentionally do not migrate older sidecars.")

(defconst mevedel-session-codec--allowed-permission-actions
  '(allow deny ask)
  "Permission rule actions recognised by this version.
Rules with other actions are dropped on load (a future version may
add more, and we don't want to act on actions we don't understand).")

(defconst mevedel-session-codec--required-sidecar-keys
  '(:version :session-id :session-name :workspace :working-directory
    :authority-mode :target-incarnation
    :created-at :updated-at :current-segment :total-turn-count
    :last-task-write-turn :task-status-notes :first-user-message
    :latest-user-message :forked-from-session-id :forked-from-turn
    :fork-type :forked-from-fork-point-id
    :worktree-source-root :worktree-directory :worktree-branch
    :worktree-base-commit
    :permission-mode :sandbox-mode :plan-mode :permission-rules :resource-grants
    :preset-name :model-provider :reasoning-effort
    :last-observed-date
    :agent-types-snapshot :skills-snapshot :workspace-instruction-hashes
    :additional-roots :tasks
    :prompt-index :file-snapshots :ptc-checkpoints :agent-transcripts :agent-registry
    :agent-turn-capacity :plan-metadata :goal :messages)
  "Keys required in every current-version session sidecar.")

(defun mevedel-session-codec-workspace-authority-mode (workspace)
  "Return the authority mode required by WORKSPACE's category."
  (mevedel-session-authority-mode-for-workspace workspace))

(defun mevedel-session-codec-authority-mode (session)
  "Return SESSION's explicit authority mode.

Normalize a fresh session's missing mode from its workspace category.  An
explicit mode that contradicts the workspace is rejected."
  (mevedel-session-authority-mode-for-session session))

(defun mevedel-session-codec-portable-authority-p (session)
  "Return non-nil when SESSION uses the portable lease authority."
  (eq (mevedel-session-codec-authority-mode session) 'portable))

(defun mevedel-session-codec-validate-authority-mode
    (mode workspace-plist)
  "Validate MODE against the persisted WORKSPACE-PLIST category."
  (unless (memq mode '(portable pid-lock))
    (error "Invalid session authority mode: %S" mode))
  (let ((type (plist-get workspace-plist :type)))
    (when (and (eq type 'project) (not (eq mode 'portable)))
      (error "Project sessions require portable authority"))
    (when (and (eq type 'file) (not (eq mode 'pid-lock)))
      (error "File sessions require PID-lock authority")))
  mode)

(defun mevedel-session-codec-authority-mode-for-path
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
  (let* ((lock (file-name-concat session-dir ".lock"))
         (lease (file-name-concat session-dir ".lease"))
         ;; A decided profile needs no inference probes, and every control
         ;; probe is one target process.
         (decided (or explicit-mode
                      (and session
                           (mevedel-session-codec-authority-mode
                            session))))
         (mode
          (or decided
              (let ((lock-exists
                     (mevedel-session-control-fs-path-exists-p lock))
                    (lease-exists
                     (mevedel-session-control-fs-path-exists-p lease)))
                (cond
                 ((and lock-exists lease-exists)
                  (error "Session has both PID lock and portable lease: %s"
                         session-dir))
                 (lease-exists 'portable)
                 (lock-exists 'pid-lock)
                 (t
                  (let ((sidecar
                         (file-name-concat session-dir "session.meta.el")))
                    (unless (mevedel-session-control-fs-path-exists-p sidecar)
                      (error "Session authority profile is unavailable: %s"
                             session-dir))
                    (let* ((plist (mevedel-session-codec-read sidecar))
                           (mode (plist-get plist :authority-mode)))
                      (mevedel-session-codec-validate-authority-mode
                       mode (plist-get plist :workspace))))))))))
    ;; Only a decided profile can disagree with the directory: an inferred
    ;; one was derived from these very artifacts and already failed closed on
    ;; a mixed directory.  Probe just the artifact that would contradict it.
    (when decided
      (pcase mode
        ('portable
         (when (mevedel-session-control-fs-path-exists-p lock)
           (error "Portable session has a PID lock: %s" session-dir)))
        ('pid-lock
         (when (mevedel-session-control-fs-path-exists-p lease)
           (error "PID-lock session has a portable lease: %s" session-dir)))))
    mode))


;;
;;; Workspace serialization

(defun mevedel-session-codec--sanitize-workspace-instruction-hashes
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
                           (mevedel-agent-path-p (car key)))
                       (stringp (cadr key))
                       (file-name-absolute-p (cadr key))
                       (stringp hash)
                       (string-match-p "\\`[[:xdigit:]]\\{64\\}\\'" hash))
             collect (cons (copy-sequence key) hash))))

(defun mevedel-session-codec--workspace-to-plist (workspace)
  "Convert WORKSPACE to a plist for sidecar storage.

The project-owned identity and target-native root are portable across
client-specific TRAMP spellings.  The process-local workspace id and file
cache are not persisted."
  (when workspace
    (let* ((root (mevedel-workspace-root workspace))
           (identity (mevedel-workspace-identity-read root))
           (target (mevedel-execution-target-create root)))
      (unless identity
        (error "Workspace identity is not materialized: %s" root))
      (list :type (mevedel-workspace-type workspace)
            :workspace-id identity
            :target-native-root
            (mevedel-execution-target-native-root target)
            :name (mevedel-workspace-name workspace)))))

(defun mevedel-session-codec--workspace-from-plist (plist workspace)
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
         (mevedel-workspace-identity-read
          (mevedel-workspace-root workspace))))
    (unless (and (stringp saved-identity)
                 (string-match-p "\\`[0-9a-f]\\{64\\}\\'" saved-identity))
      (error "Invalid persisted workspace identity"))
    (unless (and (stringp saved-root)
                 (file-name-absolute-p saved-root)
                 (not (file-remote-p saved-root)))
      (error "Invalid target-native workspace root: %S" saved-root))
    (unless current-identity
      (error "Workspace identity is missing: %s"
             (mevedel-workspace-root workspace)))
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

(defun mevedel-session-codec--filter-permission-rules (rules)
  "Drop RULES whose `:action' is unrecognised.
A rule is `(TOOL-NAME &rest PLIST)' with `:action SYMBOL'."
  (cl-remove-if-not
   (lambda (rule)
     (and (consp rule)
          (memq (plist-get (cdr rule)
                           :action)
                mevedel-session-codec--allowed-permission-actions)))
   rules))

(defun mevedel-session-codec--filter-resource-grants (grants)
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

(defun mevedel-session-codec--working-directory-from-plist
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
          (mevedel-execution-target-native-root target)))
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
             (mevedel-execution-target-expand-path target mapped)))
           (root (mevedel-workspace-root workspace)))
      (unless (file-in-directory-p dir root)
        (user-error "Working directory must be inside workspace root %s" root))
      dir)))


;;
;;; Goal serialization

(defun mevedel-session-codec--goal-to-plist (goal)
  "Serialize GOAL to a sidecar plist."
  (list :id (mevedel-goal-id goal)
        :objective (mevedel-goal-objective goal)
        :status (mevedel-goal-status goal)
        :reason (mevedel-goal-reason goal)
        :token-budget (mevedel-goal-token-budget goal)
        :tokens-used (mevedel-goal-tokens-used goal)
        :time-used-seconds (mevedel-goal-time-used-seconds goal)
        :turns-run (mevedel-goal-turns-run goal)
        :plan-reference (mevedel-goal-plan-reference goal)
        :created-at (mevedel-goal-created-at goal)
        :updated-at (mevedel-goal-updated-at goal)))

(defun mevedel-session-codec--goal-from-plist (plist)
  "Reconstruct a `mevedel-goal' from PLIST, or nil."
  (when plist
    (let ((keys '(:id :objective :status :reason :token-budget :tokens-used
                  :time-used-seconds :turns-run :plan-reference
                  :created-at :updated-at)))
      (unless
          (and (proper-list-p plist)
               (cl-evenp (length plist))
               (= (length plist) (* 2 (length keys)))
               (cl-every (lambda (key) (plist-member plist key)) keys)
               (cl-loop for (key _) on plist by #'cddr
                        always (memq key keys))
               (stringp (plist-get plist :id))
               (string-match-p "\\`[[:alnum:]_.-]+\\'"
                               (plist-get plist :id))
               (not (member (plist-get plist :id) '("." "..")))
               (stringp (plist-get plist :objective))
               (not (string-blank-p (plist-get plist :objective)))
               (memq (plist-get plist :status)
                     '(active paused blocked budget-limited complete))
               (let ((reason (plist-get plist :reason))
                     (status (plist-get plist :status)))
                 (if (memq status '(paused blocked budget-limited))
                     (and (stringp reason) (not (string-blank-p reason)))
                   (null reason)))
               (or (null (plist-get plist :token-budget))
                   (and (integerp (plist-get plist :token-budget))
                        (> (plist-get plist :token-budget) 0)))
               (natnump (plist-get plist :tokens-used))
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
    (mevedel-goal--create
     :id (plist-get plist :id)
     :objective (plist-get plist :objective)
     :status (plist-get plist :status)
     :reason (plist-get plist :reason)
     :token-budget (plist-get plist :token-budget)
     :tokens-used (plist-get plist :tokens-used)
     :time-used-seconds (plist-get plist :time-used-seconds)
     :turns-run (plist-get plist :turns-run)
     :plan-reference (plist-get plist :plan-reference)
     :created-at (plist-get plist :created-at)
     :updated-at (plist-get plist :updated-at))))


;;
;;; Task serialization

(defun mevedel-session-codec--task-to-plist (task)
  "Serialize TASK struct to a plist."
  (list :id          (mevedel-task-id task)
        :subject     (mevedel-task-subject task)
        :description (mevedel-task-description task)
        :status      (mevedel-task-status task)
        :owner       (mevedel-task-owner task)
        :blocked-by  (mevedel-task-blocked-by task)
        :completed-turn (mevedel-task-completed-turn task)
        :metadata    (mevedel-task-metadata task)))

(defun mevedel-session-codec--task-from-plist
    (plist &optional agent-registry)
  "Reconstruct a `mevedel-task' from PLIST."
  (mevedel-task--create
   :id          (plist-get plist :id)
   :subject     (plist-get plist :subject)
   :description (plist-get plist :description)
   :status      (plist-get plist :status)
   :owner       (mevedel-task-normalize-owner
                 (plist-get plist :owner) agent-registry)
   :blocked-by  (plist-get plist :blocked-by)
   :completed-turn (plist-get plist :completed-turn)
   :metadata    (plist-get plist :metadata)))


;;
;;; Top-level serialize / deserialize

(cl-defun mevedel-session-codec-serialize (session
                                                 &key
                                                 first-user-message
                                                 latest-user-message
                                                 additional-roots)
  "Serialize SESSION to a sidecar plist.

FIRST-USER-MESSAGE is the cached original-request preview.
LATEST-USER-MESSAGE is the cached resume picker preview.
ADDITIONAL-ROOTS is the buffer-local value of
`mevedel-workspace-additional-roots' for this session.

The resulting plist is round-trippable via
`mevedel-session-codec-deserialize'."
  (let* ((execution-target (mevedel-session-execution-target session))
         (authority-mode
          (mevedel-session-codec-authority-mode session))
         (target-incarnation
          (mevedel-execution-target-incarnation execution-target))
         (permission-mode
         (or (mevedel-session-permission-mode session)
             (and (boundp 'mevedel-permission-mode)
                  (default-toplevel-value 'mevedel-permission-mode))
             'ask))
        (sandbox-mode
         (or (mevedel-session-sandbox-mode session)
             (and (boundp 'mevedel-sandbox-mode)
                  (default-toplevel-value 'mevedel-sandbox-mode))
             'best-effort))
        (authority
         (or
          (mevedel-permission-serialize-authority
           (mevedel-session-permission-rules session)
           (mevedel-session-resource-grants session)
           execution-target)
          (error "Session permission authority is not portable"))))
    (mevedel-session-codec-validate-authority-mode
     authority-mode
     (mevedel-session-codec--workspace-to-plist
      (mevedel-session-workspace session)))
    (unless (and (stringp target-incarnation)
                 (string-match-p "\\S-" target-incarnation))
      (error "Target incarnation is not available"))
    (unless (memq permission-mode '(ask edits full-auto))
      (error "Invalid persisted permission mode: %S" permission-mode))
    (unless (memq sandbox-mode '(best-effort required off))
      (error "Invalid persisted sandbox mode: %S" sandbox-mode))
    (list
   :version                mevedel-session-codec-format-version
   :session-id             (mevedel-session-session-id session)
   :session-name           (mevedel-session-name session)
   :workspace              (mevedel-session-codec--workspace-to-plist
                            (mevedel-session-workspace session))
   :authority-mode         authority-mode
   :working-directory
   (mevedel-execution-target-native-path
    execution-target
    (or (mevedel-session-working-directory session)
        (mevedel-workspace-root (mevedel-session-workspace session))))
   :target-incarnation     target-incarnation
   :created-at             (mevedel-session-created-at session)
   :updated-at             (mevedel-session-updated-at session)
   :current-segment        (or (mevedel-session-current-segment session) 1)
   :total-turn-count       (or (mevedel-session-turn-count session) 0)
   :last-task-write-turn   (mevedel-session-last-task-write-turn session)
   :task-status-notes      (mevedel-session-task-status-notes session)
   :first-user-message     first-user-message
   :latest-user-message    latest-user-message
   :forked-from-session-id (mevedel-session-forked-from-session-id session)
   :forked-from-turn       (mevedel-session-forked-from-turn session)
   :fork-type              (mevedel-session-fork-type session)
   :forked-from-fork-point-id
   (mevedel-session-forked-from-fork-point-id session)
   :worktree-source-root (mevedel-session-worktree-source-root session)
   :worktree-directory   (mevedel-session-worktree-directory session)
   :worktree-branch      (mevedel-session-worktree-branch session)
   :worktree-base-commit (mevedel-session-worktree-base-commit session)
   :permission-mode        permission-mode
   :sandbox-mode           sandbox-mode
   :plan-mode              (and (mevedel-session-plan-mode session) t)
   :permission-rules       (plist-get authority :rules)
   :resource-grants        (plist-get authority :resource-grants)
   :preset-name            (mevedel-session-preset-name session)
   :model-provider         (mevedel-session-model-provider session)
   :reasoning-effort       (mevedel-session-reasoning-effort session)
   :last-observed-date     (mevedel-session-last-observed-date session)
   :agent-types-snapshot   (mevedel-session-agent-types-snapshot session)
   :skills-snapshot        (mevedel-session-skills-snapshot session)
   :workspace-instruction-hashes
   (copy-tree (mevedel-session-workspace-instruction-hashes session) t)
   :additional-roots       additional-roots
   :tasks                  (mapcar #'mevedel-session-codec--task-to-plist
                                   (mevedel-session-tasks session))
   :prompt-index           (mevedel-session-prompt-index session)
   :file-snapshots         (mevedel-session-file-snapshots session)
   :ptc-checkpoints        (copy-tree (mevedel-session-ptc-checkpoints session) t)
   :agent-transcripts      (mevedel-session-agent-transcripts session)
   :agent-registry         (mevedel-agent-persistence-serialize-registry session)
   :agent-turn-capacity    (mevedel-session-agent-turn-capacity session)
   :plan-metadata          (mevedel-session-plan-metadata session)
   :goal                   (when-let* ((goal (mevedel-session-goal session)))
                             (mevedel-session-codec--goal-to-plist goal))
   ;; Root's reverse-order unread queue.  Child queues live on their explicit
   ;; registry records and all queues become FIFO only at delivery time.
   :messages
   (mevedel-agent-persistence-sanitize-mailbox
    (mevedel-session-messages session) "/root"))))

(defun mevedel-session-codec-sanitize-agent-transcripts (raw)
  "Sanitize the `:agent-transcripts' alist RAW read from a sidecar.

Coerce unknown status values to `incomplete'.  Deduplicate agent IDs by
keeping the entry with the newest `:updated-at'.  Unknown plist keys survive
the round trip but are ignored when rendered."
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

(defun mevedel-session-codec--sanitize-ptc-checkpoints (raw)
  "Return closed, read-safe ToolScript checkpoints from sidecar value RAW."
  (cl-loop
   for checkpoint in (and (proper-list-p raw) raw)
   for id = (and (proper-list-p checkpoint) (plist-get checkpoint :id))
   for args = (and (proper-list-p checkpoint) (plist-get checkpoint :args))
   for script = (and (proper-list-p args) (plist-get args :script))
   for state = (and (proper-list-p checkpoint)
                    (plist-get checkpoint :state))
   for result = (and (proper-list-p checkpoint)
                     (plist-get checkpoint :result))
   for render-data = (and (proper-list-p checkpoint)
                          (plist-get checkpoint :render-data))
   when (and (stringp id) (<= (length id) 256)
             (memq state '(running settled))
             (stringp script) (<= (length script) (* 64 1024))
             (or (null result) (stringp result))
             (or (null render-data) (mevedel--plain-data-p render-data)))
   collect (list :id id :args (list :script script) :state state
                 :result result
                 :render-data (copy-tree render-data t))))

(defun mevedel-session-codec-validate-current-sidecar (plist)
  "Return PLIST when it contains every current-version sidecar key."
  (unless (proper-list-p plist)
    (error "Invalid session sidecar"))
  (dolist (key mevedel-session-codec--required-sidecar-keys)
    (unless (plist-member plist key)
      (error "Missing session sidecar key: %s" key)))
  (mevedel-session-codec-validate-authority-mode
   (plist-get plist :authority-mode)
   (plist-get plist :workspace))
  (let ((incarnation (plist-get plist :target-incarnation)))
    (unless (and (stringp incarnation)
                 (string-match-p "\\S-" incarnation))
      (error "Invalid persisted target incarnation: %S" incarnation))
    incarnation)
  (unless (memq (plist-get plist :permission-mode) '(ask edits full-auto))
    (error "Invalid persisted permission mode: %S"
           (plist-get plist :permission-mode)))
  (unless (memq (plist-get plist :sandbox-mode)
                '(best-effort required off))
    (error "Invalid persisted sandbox mode: %S"
           (plist-get plist :sandbox-mode)))
  (unless (booleanp (plist-get plist :plan-mode))
    (error "Invalid persisted Plan mode: %S" (plist-get plist :plan-mode)))
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
  plist)

(defun mevedel-session-codec-deserialize (plist workspace)
  "Reconstruct a session from sidecar PLIST for opened WORKSPACE.

Returns a plist:
  (:session SESSION
   :first-user-message STR-OR-NIL
   :latest-user-message STR-OR-NIL
   :additional-roots ALIST)

Where SESSION is a freshly-created `mevedel-session' struct populated
from PLIST.  The auxiliary fields (first-user-message,
latest-user-message, additional-roots) are returned alongside because
they are not on the session struct.

Only the current sidecar version is accepted.  Permission rules with
unknown actions and task state with invalid agent owners are dropped via
their hygiene filters."
  (unless (equal (plist-get plist :version)
                mevedel-session-codec-format-version)
    (error "Unsupported session version: %s"
           (or (plist-get plist :version) "missing")))
  (mevedel-session-codec-validate-current-sidecar plist)
  (let* ((workspace-binding
          (mevedel-session-codec--workspace-from-plist
           (plist-get plist :workspace) workspace))
         (workspace (car workspace-binding))
         (workspace-identity-changed-p (cdr workspace-binding))
         (execution-target
          (let* ((target
                  (mevedel-execution-target-create
                   (mevedel-workspace-root workspace)))
                 (incarnation (plist-get plist :target-incarnation)))
            (mevedel-execution-target-restore-incarnation
             target incarnation)
            target))
         (working-directory
          (mevedel-session-codec--working-directory-from-plist
           plist workspace execution-target))
         (persisted-rules
          (unless workspace-identity-changed-p
            (mevedel-session-codec--filter-permission-rules
             (plist-get plist :permission-rules))))
         (persisted-resource-grants
          (unless workspace-identity-changed-p
            (mevedel-session-codec--filter-resource-grants
             (plist-get plist :resource-grants))))
         (authority
          (mevedel-permission-deserialize-authority
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
          (mevedel-agent-persistence-deserialize-registry raw-agent-registry))
         (tasks
          (mevedel-task-prune-resolved-dependencies
           (delq
            nil
            (mapcar
             (lambda (task-plist)
               (condition-case nil
                   (mevedel-session-codec--task-from-plist
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
                       (mevedel-task-normalize-owner
                        (car entry) agent-registry))
               (error nil))
           when normalized
           collect (cons (cdr normalized) (copy-tree (cdr entry)))))
         (session   (mevedel-session--create
                     :name             (plist-get plist :session-name)
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
                     :preset-name      (plist-get plist :preset-name)
                     :model-provider   (plist-get plist :model-provider)
                     :reasoning-effort (plist-get plist :reasoning-effort)
                     :turn-count       (plist-get plist :total-turn-count)
                     :last-observed-date (plist-get plist :last-observed-date)
                     :agent-types-snapshot
                     (plist-get plist :agent-types-snapshot)
                     :skills-snapshot (plist-get plist :skills-snapshot)
                     :workspace-instruction-hashes
                     (mevedel-session-codec--sanitize-workspace-instruction-hashes
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
                     :file-snapshots   (plist-get plist :file-snapshots)
                     :ptc-checkpoints
                     (mevedel-session-codec--sanitize-ptc-checkpoints
                      (plist-get plist :ptc-checkpoints))
                     :plan-metadata    (plist-get plist :plan-metadata)
                     :goal
                     (condition-case nil
                         (mevedel-session-codec--goal-from-plist
                          (plist-get plist :goal))
                       (error nil))
                     :agent-transcripts
                     (mevedel-session-codec-sanitize-agent-transcripts
                      (plist-get plist :agent-transcripts))
                     :agent-registry agent-registry
                     :agent-turn-capacity
                     (plist-get plist :agent-turn-capacity)
                     :messages
                     (mevedel-agent-persistence-sanitize-mailbox
                      (plist-get plist :messages) "/root"))))
    (when-let* ((goal (mevedel-session-goal session))
                ((eq (mevedel-goal-status goal) 'active)))
        (setf (mevedel-goal-status goal) 'paused
              (mevedel-goal-reason goal) "session resumed"
              (mevedel-goal-updated-at goal)
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

(defun mevedel-session-codec-write (path plist)
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
      (mevedel-session-control-fs-write-file
       (mevedel-session-control-fs-physical-path path)
       (buffer-string)))))

(defun mevedel-session-codec-read (path)
  "Read sidecar plist from PATH.
Returns the raw plist.  Caller is responsible for passing it through
`mevedel-session-codec-deserialize' for validation and hygiene."
  (with-temp-buffer
    (insert (mevedel-session-control-fs-read-file
             (mevedel-session-control-fs-physical-path path)))
    (goto-char (point-min))
    (read (current-buffer))))


(provide 'mevedel-session-codec)

;;; mevedel-session-codec.el ends here
