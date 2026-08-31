;;; mevedel-session-fork.el --- Conversation and Worktree Fork persistence -*- lexical-binding: t -*-

;;; Commentary:

;; Owns Fork projection, publication, Worktree restoration, and rename.

;;; Code:

(require 'cl-lib)

(eval-when-compile
  (require 'mevedel-agents)
  (require 'mevedel-structs))

;; `mevedel-agent-persistence'
(declare-function mevedel-agent-persistence-transcript-path-p "mevedel-agent-persistence" (path save-path))
(autoload 'mevedel-agent-persistence-transcript-path-p
  "mevedel-agent-persistence")

;; `mevedel-execution'
(declare-function mevedel-execution-relocate-artifacts "mevedel-execution" (session old-root new-root))
(autoload 'mevedel-execution-relocate-artifacts "mevedel-execution")

;; `mevedel-execution-target'
(declare-function mevedel-execution-target-native-path "mevedel-execution-target" (target path))
(autoload 'mevedel-execution-target-native-path "mevedel-execution-target")

;; `mevedel-plan'
(declare-function mevedel-plan-artifact-path-p "mevedel-plan" (path))
(declare-function mevedel-plan-hash "mevedel-plan" (body))
(autoload 'mevedel-plan-artifact-path-p "mevedel-plan")
(autoload 'mevedel-plan-hash "mevedel-plan")

;; `mevedel-reminders'
(declare-function mevedel-reminders-clone-list "mevedel-reminders" (reminders))
(autoload 'mevedel-reminders-clone-list "mevedel-reminders")

;; `mevedel-session-artifacts'
(declare-function mevedel-session-artifacts-artifact-present-p "mevedel-session-artifacts" (session logical &optional committed-only))
(declare-function mevedel-session-artifacts-artifacts-dir "mevedel-session-artifacts" (save-path))
(declare-function mevedel-session-artifacts-assert-mutation-authority "mevedel-session-artifacts" (session &optional buffer))
(declare-function mevedel-session-artifacts-build-sidecar "mevedel-session-artifacts" (session buffer))
(declare-function mevedel-session-artifacts-compute-id "mevedel-session-artifacts" (name))
(declare-function mevedel-session-artifacts-materialize-published-artifacts "mevedel-session-artifacts" (session destination-save-path))
(declare-function mevedel-session-artifacts-printed-value "mevedel-session-artifacts" (value))
(declare-function mevedel-session-artifacts-publish-text "mevedel-session-artifacts" (session path content &optional coding))
(declare-function mevedel-session-artifacts-read-artifact "mevedel-session-artifacts" (session logical &optional committed-only))
(declare-function mevedel-session-artifacts-sanitize "mevedel-session-artifacts" (name))
(declare-function mevedel-session-artifacts-save-buffer-silently "mevedel-session-artifacts" ())
(declare-function mevedel-session-artifacts-save-instructions "mevedel-session-artifacts" (session buffer &optional current-only))
(declare-function mevedel-session-artifacts-segment-path "mevedel-session-artifacts" (save-path n))
(declare-function mevedel-session-artifacts-sessions-dir "mevedel-session-artifacts" (workspace))
(declare-function mevedel-session-artifacts-sidecar-path "mevedel-session-artifacts" (save-path))
(autoload 'mevedel-session-artifacts-artifact-present-p
  "mevedel-session-artifacts")
(autoload 'mevedel-session-artifacts-artifacts-dir
  "mevedel-session-artifacts")
(autoload 'mevedel-session-artifacts-assert-mutation-authority
  "mevedel-session-artifacts")
(autoload 'mevedel-session-artifacts-build-sidecar
  "mevedel-session-artifacts")
(autoload 'mevedel-session-artifacts-compute-id "mevedel-session-artifacts")
(autoload 'mevedel-session-artifacts-materialize-published-artifacts
  "mevedel-session-artifacts")
(autoload 'mevedel-session-artifacts-printed-value
  "mevedel-session-artifacts")
(autoload 'mevedel-session-artifacts-publish-text
  "mevedel-session-artifacts")
(autoload 'mevedel-session-artifacts-read-artifact
  "mevedel-session-artifacts")
(autoload 'mevedel-session-artifacts-sanitize "mevedel-session-artifacts")
(autoload 'mevedel-session-artifacts-save-buffer-silently
  "mevedel-session-artifacts")
(autoload 'mevedel-session-artifacts-save-instructions
  "mevedel-session-artifacts")
(autoload 'mevedel-session-artifacts-segment-path
  "mevedel-session-artifacts")
(autoload 'mevedel-session-artifacts-sessions-dir
  "mevedel-session-artifacts")
(autoload 'mevedel-session-artifacts-sidecar-path
  "mevedel-session-artifacts")

;; `mevedel-session-codec'
(declare-function mevedel-session-codec-deserialize "mevedel-session-codec" (plist workspace))
(declare-function mevedel-session-codec-portable-authority-p "mevedel-session-codec" (session))
(declare-function mevedel-session-codec-read "mevedel-session-codec" (path))
(declare-function mevedel-session-codec-write "mevedel-session-codec" (path plist))
(autoload 'mevedel-session-codec-deserialize "mevedel-session-codec")
(autoload 'mevedel-session-codec-portable-authority-p
  "mevedel-session-codec")
(autoload 'mevedel-session-codec-read "mevedel-session-codec")
(autoload 'mevedel-session-codec-write "mevedel-session-codec")

;; `mevedel-session-durability'
(declare-function mevedel-session-durability-call-with-reserved-lease "mevedel-session-durability" (session function))
(declare-function mevedel-session-durability-forget-removed-session "mevedel-session-durability" (session))
(autoload 'mevedel-session-durability-call-with-reserved-lease
  "mevedel-session-durability")
(autoload 'mevedel-session-durability-forget-removed-session
  "mevedel-session-durability")

;; `mevedel-session-persistence'
(declare-function mevedel-session-persistence-allocate-session-id "mevedel-session-persistence" (name sessions-dir))
(declare-function mevedel-session-persistence-find-live-buffer "mevedel-session-persistence" (session-id buf-name))
(declare-function mevedel-session-persistence-list-sessions "mevedel-session-persistence" (workspace &optional cached))
(declare-function mevedel-session-persistence-lock-acquire "mevedel-session-persistence" (session-dir buffer-name &optional session))
(declare-function mevedel-session-persistence-lock-release "mevedel-session-persistence" (session-dir &optional session))
(declare-function mevedel-session-persistence-notify-session-event "mevedel-session-persistence" (session event &rest args))
(declare-function mevedel-session-persistence-reconcile-lost-execution-segments "mevedel-session-persistence" (session &optional exclude-path artifact-callback))
(declare-function mevedel-session-persistence-restore "mevedel-session-persistence" (session-dir &optional lifecycle-source session-override workspace))
(declare-function mevedel-session-persistence-write-current-buffer-atomically "mevedel-session-persistence" (path))
(autoload 'mevedel-session-persistence-allocate-session-id
  "mevedel-session-persistence")
(autoload 'mevedel-session-persistence-find-live-buffer
  "mevedel-session-persistence")
(autoload 'mevedel-session-persistence-list-sessions
  "mevedel-session-persistence")
(autoload 'mevedel-session-persistence-lock-acquire
  "mevedel-session-persistence")
(autoload 'mevedel-session-persistence-lock-release
  "mevedel-session-persistence")
(autoload 'mevedel-session-persistence-notify-session-event
  "mevedel-session-persistence")
(autoload 'mevedel-session-persistence-reconcile-lost-execution-segments
  "mevedel-session-persistence")
(autoload 'mevedel-session-persistence-restore
  "mevedel-session-persistence")
(autoload 'mevedel-session-persistence-write-current-buffer-atomically
  "mevedel-session-persistence")

;; `mevedel-session-publication'
(declare-function mevedel-session-publication-discard-rolled-back "mevedel-session-publication" (session))
(declare-function mevedel-session-publication-publish "mevedel-session-publication" (session artifacts &optional require-commit))
(declare-function mevedel-session-publication-read "mevedel-session-publication" (session-dir))
(autoload 'mevedel-session-publication-discard-rolled-back
  "mevedel-session-publication")
(autoload 'mevedel-session-publication-publish "mevedel-session-publication")
(autoload 'mevedel-session-publication-read "mevedel-session-publication")

;; `mevedel-session-rewind'
(declare-function mevedel-session-rewind-assert-stable-source "mevedel-session-rewind" (session buffer operation))
(declare-function mevedel-session-rewind-load-rewind-target "mevedel-session-rewind" (session buffer target &optional before-turn))
(declare-function mevedel-session-rewind-read-backup "mevedel-session-rewind" (session backup-name))
(declare-function mevedel-session-rewind-reduce-agent-transcripts "mevedel-session-rewind" (entries picked-cum-turn))
(declare-function mevedel-session-rewind-reduce-file-snapshots "mevedel-session-rewind" (snapshots picked-cum-turn &optional before-turn))
(declare-function mevedel-session-rewind-reduce-prompt-index "mevedel-session-rewind" (index picked-segment picked-cum-turn &optional before-turn))
(declare-function mevedel-session-rewind-resolve-fork-target "mevedel-session-rewind" (session target))
(declare-function mevedel-session-rewind-rewind-publication-artifacts "mevedel-session-rewind" (session buffer staging-path &optional state))
(declare-function mevedel-session-rewind-state-at-turn "mevedel-session-rewind" (session cum-turn &optional before-turn))
(autoload 'mevedel-session-rewind-assert-stable-source
  "mevedel-session-rewind")
(autoload 'mevedel-session-rewind-load-rewind-target
  "mevedel-session-rewind")
(autoload 'mevedel-session-rewind-read-backup "mevedel-session-rewind")
(autoload 'mevedel-session-rewind-reduce-agent-transcripts
  "mevedel-session-rewind")
(autoload 'mevedel-session-rewind-reduce-file-snapshots
  "mevedel-session-rewind")
(autoload 'mevedel-session-rewind-reduce-prompt-index
  "mevedel-session-rewind")
(autoload 'mevedel-session-rewind-resolve-fork-target
  "mevedel-session-rewind")
(autoload 'mevedel-session-rewind-rewind-publication-artifacts
  "mevedel-session-rewind")
(autoload 'mevedel-session-rewind-state-at-turn "mevedel-session-rewind")

;; `mevedel-structs'
(declare-function mevedel-session--create "mevedel-structs" (&rest slots))
(declare-function mevedel-session-agent-turn-capacity "mevedel-structs" (cl-x))
(declare-function mevedel-session-buffer-name "mevedel-structs" (session-name workspace))
(declare-function mevedel-session-created-at "mevedel-structs" (cl-x))
(declare-function mevedel-session-current-segment "mevedel-structs" (cl-x))
(declare-function mevedel-session-execution-target "mevedel-structs" (cl-x))
(declare-function mevedel-session-file-snapshots "mevedel-structs" (cl-x))
(declare-function mevedel-session-fork-type "mevedel-structs" (cl-x))
(declare-function mevedel-session-forked-from-fork-point-id "mevedel-structs" (cl-x))
(declare-function mevedel-session-forked-from-session-id "mevedel-structs" (cl-x))
(declare-function mevedel-session-goal "mevedel-structs" (cl-x))
(declare-function mevedel-session-name "mevedel-structs" (cl-x))
(declare-function mevedel-session-permission-mode "mevedel-structs" (cl-x))
(declare-function mevedel-session-permission-rules "mevedel-structs" (cl-x))
(declare-function mevedel-session-plan-mode "mevedel-structs" (cl-x))
(declare-function mevedel-session-preset-name "mevedel-structs" (cl-x))
(declare-function mevedel-session-prompt-index "mevedel-structs" (cl-x))
(declare-function mevedel-session-publication "mevedel-structs" (cl-x))
(declare-function mevedel-session-resource-grants "mevedel-structs" (cl-x))
(declare-function mevedel-session-sandbox-mode "mevedel-structs" (cl-x))
(declare-function mevedel-session-save-path "mevedel-structs" (cl-x))
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
(defvar mevedel--session)

;; `mevedel-tool-render-data'
(declare-function mevedel-tool-render-data-reconcile-lost-executions
                  "mevedel-tool-render-data"
                  (buffer &optional successor-execution-ids))
(autoload 'mevedel-tool-render-data-reconcile-lost-executions
  "mevedel-tool-render-data")

;; `mevedel-utilities'
(declare-function mevedel--normalize-message-text "mevedel-utilities" (text))
(autoload 'mevedel--normalize-message-text "mevedel-utilities")

;; `mevedel-view'
(defvar mevedel--data-buffer)

;; `mevedel-workspace'
(defvar mevedel-workspace-additional-roots)

;; `mevedel-worktree'
(declare-function mevedel-worktree-fork-create "mevedel-worktree" (reservation))
(declare-function mevedel-worktree-fork-reservation "mevedel-worktree" (session &optional preflight))
(declare-function mevedel-worktree-fork-validate-reservation "mevedel-worktree" (session reservation))
(autoload 'mevedel-worktree-fork-create "mevedel-worktree")
(autoload 'mevedel-worktree-fork-reservation "mevedel-worktree")
(autoload 'mevedel-worktree-fork-validate-reservation "mevedel-worktree")

;; `org'
(defvar org-agenda-file-menu-enabled)

;;
;;; Fork-on-send and rename

(defun mevedel-session-fork--agent-files-for-segments
    (prompt-index agent-transcripts picked-segment picked-cum-turn)
  "Return transcript entries whose `:parent-turn' is in copied ranges.

PROMPT-INDEX is the session sidecar's segment prompt index.
AGENT-TRANSCRIPTS is the sidecar's transcript entry alist.
PICKED-SEGMENT and PICKED-CUM-TURN describe the rewind target.  The
copied transcript set is derived from concrete segment ranges:
predecessor segments are copied whole; the picked segment is copied
only through PICKED-CUM-TURN.  Entries with non-integer
`:parent-turn' are excluded."
  (let ((ranges nil))
    (dolist (seg-entry prompt-index)
      (let ((seg (car seg-entry))
            (prompts (cdr seg-entry)))
        (when (and (integerp seg)
                   (or (< seg picked-segment)
                       (= seg picked-segment)))
          (let* ((turns (cl-loop for p in prompts
                                 for ct = (plist-get p :cum-turn)
                                 when (and (integerp ct)
                                           (or (< seg picked-segment)
                                               (null picked-cum-turn)
                                               (<= ct picked-cum-turn)))
                                 collect ct))
                 (lo (and turns (apply #'min turns)))
                 (hi (and turns (apply #'max turns))))
            (when (and lo hi)
              (push (cons lo hi) ranges))))))
    (setq ranges (nreverse ranges))
    (cl-loop for entry in agent-transcripts
             for parent-turn = (plist-get (cdr entry) :parent-turn)
             when (and (integerp parent-turn)
                       (cl-some (lambda (range)
                                  (and (<= (car range) parent-turn)
                                       (<= parent-turn (cdr range))))
                                ranges))
             collect entry)))

(defconst mevedel-session-fork--clone-slot-names
  '(name workspace execution-target authority-mode working-directory
    tasks task-status-notes last-task-write-turn touched-files
    permission-rules resource-grants permission-mode sandbox-mode plan-mode
    directive-planning preset-name model-provider
    reasoning-effort turn-count reminders last-observed-date
    agent-types-snapshot skills-snapshot pending-reminders
    specialist-nudge-state deferred-set deferred-pending deferred-injected
    deferred-used deferred-expired messages agent-registry agent-reservations
    agent-root-activity agent-root-waiter agent-turn-capacity pending-steering
    pending-follow-ups pending-input-next-id pending-input-paused
    pending-input-failure-paused dropped-file-grants active-dropped-file-grants
    mentions-shown workspace-instruction-hashes skills hook-rules hook-log
    hook-log-pending repair-log
    repair-log-pending permission-log-pending telemetry-pending
    hook-context-pending execution-state audit-session pending-publication
    publication publication-queue publication-uncommitted-batches
    publication-active-p control-transfer control-transfer-drains root-buffer
    lease
    lease-renewal-timer save-path session-id created-at updated-at
    current-segment forked-from-session-id forked-from-turn fork-type
    forked-from-fork-point-id worktree-source-root worktree-directory
    worktree-branch worktree-base-commit prompt-index file-snapshots
    ptc-checkpoints persisted-first-user-message durable-tree-ensured
    agent-transcripts invoked-skills permission-queue pending-plan-approval
    plan-metadata goal)
  "Every `mevedel-session' slot decided by the logical clone constructor.")

(defun mevedel-session-fork--assert-clone-slot-completeness ()
  "Signal when the session clone policy no longer covers the struct."
  (let ((actual
         (mapcar #'car
                 (cdr (cl-struct-slot-info 'mevedel-session))))
        (expected mevedel-session-fork--clone-slot-names))
    (unless (equal actual expected)
      (error "Session clone policy is incomplete: missing %S, extra %S"
             (cl-set-difference actual expected)
             (cl-set-difference expected actual)))
    t))

(cl-defun mevedel-session-fork-clone-session
    (session policy &key name save-path session-id created-at updated-at
             current-segment forked-from-session-id forked-from-turn)
  "Build an explicit independent logical clone of SESSION for POLICY.

POLICY is `fork' or `save-as'.  Workspace and execution-target are shared
because they are immutable authority identities owned outside the session.
Every other slot is decided here: durable logical containers are copied,
fork-only projections are reduced, and runtime/control state starts empty.
The identity and timestamp keywords describe the new materialized child."
  (mevedel-session-fork--assert-clone-slot-completeness)
  (unless (memq policy '(fork save-as))
    (error "Unknown session clone policy: %S" policy))
  (let* ((fork-p (eq policy 'fork))
         (turn (if fork-p
                   forked-from-turn
                 (mevedel-session-turn-count session)))
         (child
          (mevedel-session--create
           :name (if (eq policy 'save-as)
                     name
                   (mevedel-session-name session))
           :workspace (mevedel-session-workspace session)
           :execution-target (mevedel-session-execution-target session)
           :authority-mode (mevedel-session-authority-mode session)
           :working-directory (mevedel-session-working-directory session)
           :tasks (unless fork-p
                    (copy-tree (mevedel-session-tasks session) t))
           :task-status-notes (unless fork-p
                                (copy-tree
                                 (mevedel-session-task-status-notes session)
                                 t))
           :last-task-write-turn (unless fork-p
                                   (mevedel-session-last-task-write-turn
                                    session))
           :touched-files (make-hash-table :test #'equal)
           :permission-rules
           (copy-tree (mevedel-session-permission-rules session) t)
           :resource-grants
           (copy-tree (mevedel-session-resource-grants session) t)
           :permission-mode (mevedel-session-permission-mode session)
           :sandbox-mode (mevedel-session-sandbox-mode session)
           :plan-mode (mevedel-session-plan-mode session)
           :directive-planning nil
           :preset-name (mevedel-session-preset-name session)
           :model-provider (mevedel-session-model-provider session)
           :reasoning-effort (mevedel-session-reasoning-effort session)
           :turn-count turn
           :reminders
           (mevedel-reminders-clone-list
            (mevedel-session-reminders session))
           :last-observed-date (mevedel-session-last-observed-date session)
           :agent-types-snapshot
           (copy-tree (mevedel-session-agent-types-snapshot session) t)
           :skills-snapshot
           (copy-tree (mevedel-session-skills-snapshot session) t)
           :pending-reminders nil
           :specialist-nudge-state nil
           :deferred-set
           (copy-tree (mevedel-session-deferred-set session) t)
           :deferred-pending nil
           :deferred-injected nil
           :deferred-used nil
           :deferred-expired nil
           :messages (unless fork-p
                       (copy-tree (mevedel-session-messages session) t))
           :agent-registry (unless fork-p
                             (copy-tree
                              (mevedel-session-agent-registry session) t))
           :agent-reservations nil
           :agent-root-activity 'idle
           :agent-root-waiter nil
           :agent-turn-capacity
           (mevedel-session-agent-turn-capacity session)
           :pending-steering nil
           :pending-follow-ups nil
           :pending-input-next-id nil
           :pending-input-paused nil
           :pending-input-failure-paused nil
           :dropped-file-grants nil
           :active-dropped-file-grants nil
           :mentions-shown (make-hash-table :test #'equal)
           :workspace-instruction-hashes nil
           :skills (copy-tree (mevedel-session-skills session) t)
           :hook-rules (and fork-p
                            (copy-tree
                             (mevedel-session-hook-rules session) t))
           :hook-log nil
           :hook-log-pending nil
           :repair-log nil
           :repair-log-pending nil
           :permission-log-pending nil
           :telemetry-pending nil
           :hook-context-pending nil
           :execution-state nil
           :audit-session nil
           :pending-publication nil
           :publication nil
           :publication-queue nil
           :publication-uncommitted-batches nil
           :publication-active-p nil
           :control-transfer nil
           :control-transfer-drains nil
           :root-buffer nil
           :lease nil
           :lease-renewal-timer nil
           :save-path save-path
           :session-id session-id
           :created-at (or created-at
                           (mevedel-session-created-at session))
           :updated-at (or updated-at
                           (mevedel-session-updated-at session))
           :current-segment
           (if fork-p
               current-segment
             (mevedel-session-current-segment session))
           :forked-from-session-id forked-from-session-id
           :forked-from-turn turn
           :fork-type (unless fork-p
                        (mevedel-session-fork-type session))
           :forked-from-fork-point-id
           (unless fork-p
             (mevedel-session-forked-from-fork-point-id session))
           :worktree-source-root
           (unless fork-p
             (mevedel-session-worktree-source-root session))
           :worktree-directory
           (unless fork-p
             (mevedel-session-worktree-directory session))
           :worktree-branch
           (unless fork-p
             (mevedel-session-worktree-branch session))
           :worktree-base-commit
           (unless fork-p
             (mevedel-session-worktree-base-commit session))
           :prompt-index
           (copy-tree
            (if fork-p
                (mevedel-session-rewind-reduce-prompt-index
                 (mevedel-session-prompt-index session)
                 current-segment turn)
              (mevedel-session-prompt-index session))
            t)
           :file-snapshots
           (copy-tree
            (if fork-p
                (mevedel-session-rewind-reduce-file-snapshots
                 (mevedel-session-file-snapshots session) turn)
              (mevedel-session-file-snapshots session))
            t)
           :ptc-checkpoints nil
           :agent-transcripts
           (copy-tree (mevedel-session-agent-transcripts session) t)
           :invoked-skills
           (copy-tree
            (if fork-p
                (cl-remove-if
                 (lambda (record)
                   (> (or (mevedel-skill-invocation-record-turn record) 0)
                      (or turn 0)))
                 (mevedel-session-invoked-skills session))
              (mevedel-session-invoked-skills session))
            t)
           :permission-queue nil
           :pending-plan-approval nil
           :plan-metadata (unless fork-p
                            (copy-tree
                             (mevedel-session-plan-metadata session) t))
           :goal (unless fork-p
                   (copy-tree (mevedel-session-goal session) t)))))
    (when (and fork-p turn)
      (setf (mevedel-session-agent-transcripts child)
            (mevedel-session-rewind-reduce-agent-transcripts
             (mevedel-session-agent-transcripts child) turn)))
    child))

(defun mevedel-session-fork--materialize-fork-artifact
    (source logical staging-path &optional required)
  "Copy SOURCE's committed LOGICAL artifact below STAGING-PATH.

Return non-nil when the artifact was materialized.  When REQUIRED is non-nil,
signal if LOGICAL is absent.  Literal resolver bytes are used so remote fixed
caches never become fork authority."
  (if (mevedel-session-artifacts-artifact-present-p source logical t)
      (let ((destination (expand-file-name logical staging-path))
            (content
             (mevedel-session-artifacts-read-artifact source logical t)))
        (make-directory (file-name-directory destination) t)
        (let ((coding-system-for-write 'no-conversion))
          (write-region content nil destination nil 'silent))
        t)
    (when required
      (error "Required fork artifact is not published: %s" logical))))

(defun mevedel-session-fork--stage-fork
    (child buffer staging-buffer parent-save-path staging-path
           picked-segment picked-cum-turn &optional additional-roots)
  "Materialize CHILD under STAGING-PATH using STAGING-BUFFER."
  (let ((source (buffer-local-value 'mevedel--session buffer)))
    (unless source
      (error "Fork source buffer has no session"))
    (make-directory (file-name-concat staging-path "agents") t)
    (make-directory (file-name-concat staging-path "file-history") t)
    (when-let* ((local-source
                (and parent-save-path
                     (file-name-concat parent-save-path "local")))
               ((file-directory-p local-source)))
      (copy-directory local-source
                      (file-name-concat staging-path "local")
                      nil t t)
      ;; `local/plans' is managed plan state, not ordinary local content.
      ;; Re-add only the accepted artifact proven valid at the fork point
      ;; below.
      (let ((plans (file-name-concat staging-path "local" "plans")))
        (when (file-directory-p plans)
          (delete-directory plans t))))
    (if (mevedel-session-codec-portable-authority-p source)
        (mevedel-session-artifacts-materialize-published-artifacts
         source staging-path)
      (when-let* ((artifact-source
                   (and parent-save-path
                        (mevedel-session-artifacts-artifacts-dir
                         parent-save-path)))
                  ((file-directory-p artifact-source)))
        (copy-directory
         artifact-source
         (mevedel-session-artifacts-artifacts-dir staging-path)
         nil t t)))
    (when-let* ((source source)
                (metadata (mevedel-session-plan-metadata source))
                ((eq (plist-get metadata :status) 'accepted))
                (accepted-turn (plist-get metadata :accepted-turn))
                ((and (integerp accepted-turn)
                      (<= accepted-turn picked-cum-turn)))
                (relative-path (plist-get metadata :accepted-path))
                ((stringp relative-path))
                ;; A normalized session-relative path is the whole
                ;; containment rule: it cannot escape the session store or
                ;; name another target.
                ((mevedel-plan-artifact-path-p relative-path)))
      ;; A fork must not inherit an artifact that no longer matches the hash
      ;; the source accepted.  Only committed bytes are authority here; an
      ;; absent artifact is reported by the materialization below.
      (when (mevedel-session-artifacts-artifact-present-p
             source relative-path t)
        (let ((body
               (mevedel--normalize-message-text
                (decode-coding-string
                 (mevedel-session-artifacts-read-artifact
                  source relative-path t)
                 'utf-8-unix))))
          (unless (equal (plist-get metadata :accepted-hash)
                         (mevedel-plan-hash body))
            (error "Accepted plan artifact hash does not match"))))
      (mevedel-session-fork--materialize-fork-artifact
       source relative-path staging-path t))
    (cl-loop for i from 1 below picked-segment do
             (mevedel-session-fork--materialize-fork-artifact
              source
              (file-name-nondirectory
               (mevedel-session-artifacts-segment-path parent-save-path i))
              staging-path))
    (with-current-buffer staging-buffer
      (setq-local mevedel--session child)
      (setq-local mevedel-workspace-additional-roots
                  (copy-tree additional-roots t))
      (setq buffer-file-name
            (mevedel-session-artifacts-segment-path
             staging-path picked-segment)
            buffer-file-truename nil)
      (mevedel-tool-render-data-reconcile-lost-executions staging-buffer)
      (set-buffer-modified-p t)
      (mevedel-session-artifacts-save-buffer-silently))
    (mevedel-session-persistence-reconcile-lost-execution-segments
     child (mevedel-session-artifacts-segment-path
            staging-path picked-segment))
    (when picked-cum-turn
      (dolist (entry
               (mevedel-session-rewind-state-at-turn
                child picked-cum-turn))
        (when-let* ((backup-name (plist-get (cdr entry) :backup-name)))
          (mevedel-session-fork--materialize-fork-artifact
           source (file-name-concat "file-history" backup-name)
           staging-path))))
    (when (and picked-cum-turn parent-save-path)
      (dolist (entry
               (mevedel-session-fork--agent-files-for-segments
                (mevedel-session-prompt-index child)
                (mevedel-session-agent-transcripts child)
                picked-segment picked-cum-turn))
        (when-let* ((rel-path (plist-get (cdr entry) :path))
                    ((mevedel-agent-persistence-transcript-path-p
                      rel-path parent-save-path))
                    ((mevedel-agent-persistence-transcript-path-p
                      rel-path staging-path)))
          (mevedel-session-fork--materialize-fork-artifact
           source rel-path staging-path))))
    (mevedel-session-codec-write
     (mevedel-session-artifacts-sidecar-path staging-path)
     (mevedel-session-artifacts-build-sidecar child staging-buffer))
    (mevedel-session-artifacts-save-instructions child buffer)
    (let* ((saved
            (mevedel-session-codec-read
             (mevedel-session-artifacts-sidecar-path staging-path)))
           (restored
            (mevedel-session-codec-deserialize
             saved (mevedel-session-workspace child))))
      (unless (equal (mevedel-session-session-id
                      (plist-get restored :session))
                     (mevedel-session-session-id child))
        (error "Fork staging validation failed")))))

(defun mevedel-session-fork--publish-fork
    (child buffer staging-buffer parent-save-path staging-path new-save-path
           picked-segment picked-cum-turn additional-roots)
  "Stage, publish, and restore CHILD as one session-artifact transaction."
  (let ((portable-p
         (mevedel-session-codec-portable-authority-p child))
        child-buffer published committed)
    (unwind-protect
        (progn
          (unless (mevedel-session-persistence-lock-acquire
                   staging-path (buffer-name buffer) child)
            (error "Could not acquire fork session lock"))
          (mevedel-session-fork--stage-fork
           child buffer staging-buffer parent-save-path staging-path
           picked-segment picked-cum-turn additional-roots)
          (if portable-p
              (progn
                ;; A fork starts a new publication history.  Publish its
                ;; complete staged snapshot before the owned lease and
                ;; immutable control state move together into discoverability.
                (mevedel-session-publication-publish
                 child
                 (mevedel-session-rewind-rewind-publication-artifacts
                  child staging-buffer staging-path))
                (mevedel-session-durability-call-with-reserved-lease
                 child
                 (lambda ()
                   (rename-file (directory-file-name staging-path)
                                (directory-file-name new-save-path))
                   (setq published t)
                   ;; Reservation finalization must address the moved lease.
                   (setf (mevedel-session-save-path child) new-save-path)))
                (setf
                 (mevedel-session-publication child)
                 (mevedel-session-publication-read new-save-path)))
            (mevedel-session-persistence-lock-release staging-path child)
            (rename-file (directory-file-name staging-path)
                         (directory-file-name new-save-path))
            (setf (mevedel-session-save-path child) new-save-path))
          (setq published t)
          (with-current-buffer staging-buffer
            (set-buffer-modified-p nil)
            (set-visited-file-name nil t)
            (setq-local kill-buffer-hook nil))
          (kill-buffer staging-buffer)
          (setq child-buffer
                (mevedel-session-persistence-restore
                 new-save-path "fork" child)
                committed t)
          child-buffer)
      (unless committed
        (when-let* ((failed-buffer
                     (or child-buffer
                         (mevedel-session-persistence-find-live-buffer
                          (mevedel-session-session-id child)
                          (mevedel-session-buffer-name
                           (mevedel-session-name child)
                           (mevedel-session-workspace child))))))
          (with-current-buffer failed-buffer
            (set-buffer-modified-p nil)
            (setq-local kill-buffer-hook nil
                        kill-buffer-query-functions nil))
          (when (buffer-live-p failed-buffer)
            (kill-buffer failed-buffer)))
        (when published
          (ignore-errors (delete-directory new-save-path t)))
        (when (file-directory-p staging-path)
          (ignore-errors (delete-directory staging-path t)))
        (let ((remaining
               (cond
                ((and published
                      (ignore-errors (file-directory-p new-save-path)))
                 new-save-path)
                ((ignore-errors (file-directory-p staging-path))
                 staging-path))))
          (if remaining
              (ignore-errors
                (mevedel-session-persistence-lock-release remaining child))
            (when portable-p
              (ignore-errors
                (mevedel-session-durability-forget-removed-session child))))))
      (when (buffer-live-p staging-buffer)
        (with-current-buffer staging-buffer
          (set-buffer-modified-p nil)
          (setq-local kill-buffer-hook nil))
        (kill-buffer staging-buffer)))))

(defun mevedel-session-fork--fork-child-name (session fork-type)
  "Return the first unused direct-child name for SESSION and FORK-TYPE."
  (let* ((source-id (mevedel-session-session-id session))
         (source-name (mevedel-session-name session))
         (type-name (symbol-name fork-type))
         (regexp
          (format "\\`%s \u00b7 %s \\([0-9]+\\)\\'"
                  (regexp-quote source-name)
                  (regexp-quote type-name)))
         used)
    (dolist (entry
             (mevedel-session-persistence-list-sessions
              (mevedel-session-workspace session)))
      (let ((summary (plist-get entry :summary)))
        (when (and (equal source-id
                          (plist-get summary :forked-from-session-id))
                   (eq fork-type (plist-get summary :fork-type))
                   (string-match regexp
                                 (or (plist-get summary :session-name) "")))
          (push (string-to-number (match-string 1
                                                (plist-get summary
                                                           :session-name)))
                used))))
    (let ((number 1))
      (while (memq number used)
        (cl-incf number))
      (format "%s · %s %d" source-name type-name number))))

(defun mevedel-session-fork--conversation-fork-disclosure
    (session)
  "Return durable disclosure text for conversation fork SESSION."
  (format
   (concat "\n\n<system-reminder>\n"
           "Conversation Fork\n"
           "Source session: %s\n"
           "Working directory: %s\n"
           "Files were not restored.  This fork uses the current files in "
           "the Source working directory, so they may be newer than the "
           "selected conversation point and changes are shared.\n"
           "</system-reminder>\n")
   (mevedel-session-forked-from-session-id session)
   (mevedel-session-working-directory session)))

(defun mevedel-session-fork-retarget-worktree-path (session path)
  "Retarget absolute PATH from SESSION's Source root into its worktree."
  (unless (and (stringp path) (file-name-absolute-p path))
    (error "Invalid Worktree Fork path: %S" path))
  (let* ((source-root
          (file-name-as-directory
           (expand-file-name
            (or (mevedel-session-worktree-source-root session)
                (error "Worktree Fork has no Source repository root")))))
         (worktree-root
          (file-name-as-directory
           (expand-file-name
            (or (mevedel-session-worktree-directory session)
                (error "Worktree Fork has no worktree directory")))))
         (expanded (expand-file-name path)))
    (if (string-prefix-p source-root expanded)
        (expand-file-name (substring expanded (length source-root))
                          worktree-root)
      expanded)))

(defun mevedel-session-fork--retarget-worktree-roots
    (session roots)
  "Retarget checkout-local paths in session-local ROOTS for SESSION.
Return `(:roots ALIST :dropped LIST)'."
  (let (retargeted dropped)
    (dolist (entry roots)
      (if (and (consp entry)
               (stringp (car entry))
               (file-name-absolute-p (car entry))
               (proper-list-p (cdr entry)))
          (let (paths)
            (dolist (path (cdr entry))
              (if (and (stringp path) (file-name-absolute-p path))
                  (push
                   (mevedel-session-fork-retarget-worktree-path
                    session path)
                   paths)
                (push (format "additional root %S" path) dropped)))
            (push (cons (car entry) (nreverse paths)) retargeted))
        (push (format "additional roots entry %S" entry) dropped)))
    (list :roots (nreverse retargeted)
          :dropped (nreverse dropped))))

(defun mevedel-session-fork--retarget-worktree-state (session)
  "Retarget SESSION's copied repository-local path state.
Return descriptions of malformed grants and rules dropped from the child."
  (let (grants rules dropped)
    (dolist (grant (mevedel-session-resource-grants session))
      (if (and (proper-list-p grant)
               (stringp (plist-get grant :path))
               (file-name-absolute-p (plist-get grant :path)))
          (let ((copy (copy-tree grant t)))
            (plist-put
             copy :path
             (mevedel-session-fork-retarget-worktree-path
              session (plist-get copy :path)))
            (push copy grants))
        (push (format "resource grant %S" grant) dropped)))
    (dolist (rule (mevedel-session-permission-rules session))
      (cond
       ((not (and (consp rule) (proper-list-p (cdr rule))))
        (push (format "permission rule %S" rule) dropped))
       ((not (plist-member (cdr rule) :path))
        (push (copy-tree rule t) rules))
       ((and (stringp (plist-get (cdr rule) :path))
             (file-name-absolute-p (plist-get (cdr rule) :path)))
        (let ((copy (copy-tree rule t)))
          (plist-put
           (cdr copy) :path
           (mevedel-session-fork-retarget-worktree-path
            session (plist-get (cdr copy) :path)))
          (push copy rules)))
       (t
        (push (format "permission rule %S" rule) dropped))))
  (setf
   (mevedel-session-working-directory session)
   (mevedel-session-worktree-directory session)
   (mevedel-session-file-snapshots session)
   (mapcar
    (lambda (turn-entry)
      (cons
       (car turn-entry)
       (mapcar
        (lambda (file-entry)
          (cons
           (mevedel-session-fork-retarget-worktree-path
            session (car file-entry))
           (copy-tree (cdr file-entry) t)))
        (cdr turn-entry))))
    (mevedel-session-file-snapshots session))
   (mevedel-session-resource-grants session)
   (nreverse grants)
   (mevedel-session-permission-rules session)
   (nreverse rules))
    (nreverse dropped)))

(defun mevedel-session-fork--assert-worktree-target
    (worktree-root target)
  "Signal when TARGET could escape WORKTREE-ROOT through a symlink."
  (let* ((root (file-name-as-directory (file-truename worktree-root)))
         (cursor (if (file-exists-p target)
                     target
                   (file-name-directory target))))
    (while (and cursor (not (file-exists-p cursor)))
      (setq cursor
            (file-name-directory
             (directory-file-name cursor))))
    (unless (and cursor
                 (let ((resolved (file-truename cursor)))
                   (or (file-equal-p resolved root)
                       (file-in-directory-p resolved root))))
      (error "Unsafe Worktree Fork target: %s" target))))

(defun mevedel-session-fork--restore-worktree-files
    (source child cum-turn)
  "Restore SOURCE's captured repository files into CHILD at CUM-TURN."
  (let* ((source-root
          (file-name-as-directory
           (expand-file-name
            (mevedel-session-worktree-source-root child))))
         (worktree-root
          (file-name-as-directory
           (expand-file-name
            (mevedel-session-worktree-directory child))))
         (history-dir
          (file-name-concat
           (mevedel-session-save-path source) "file-history"))
         (portable-p
          (mevedel-session-codec-portable-authority-p source))
         (restored 0)
         plan
         unrestored
         external)
    (unless (or portable-p (file-readable-p history-dir))
      (error "Captured file-history store is unreadable: %s" history-dir))
    ;; Validate the complete plan before the first child-worktree write.
    (dolist (entry
             (mevedel-session-rewind-state-at-turn source cum-turn))
      (let ((path (car entry))
            (snapshot (cdr entry)))
        (unless (and (stringp path) (file-name-absolute-p path))
          (error "Invalid captured Worktree Fork path: %S" path))
        (if (not
             (string-prefix-p source-root (expand-file-name path)))
            (push (expand-file-name path) external)
          (let* ((target
                  (mevedel-session-fork-retarget-worktree-path
                   child path))
                 (backup-name (plist-get snapshot :backup-name)))
            (mevedel-session-fork--assert-worktree-target
             worktree-root target)
            (unless
                (or (null backup-name)
                    (and (stringp backup-name)
                         (equal backup-name
                                (file-name-nondirectory backup-name))))
              (error "Invalid captured backup name: %S" backup-name))
            (push (list :path target :backup-name backup-name) plan)))))
    (dolist (item (nreverse plan))
      (let ((target (plist-get item :path))
            (backup-name (plist-get item :backup-name)))
        (condition-case err
            (progn
              (if (null backup-name)
                  (when (file-exists-p target)
                    (delete-file target))
                (make-directory (file-name-directory target) t)
                (with-temp-buffer
                  (set-buffer-multibyte nil)
                  (insert
                   (mevedel-session-rewind-read-backup
                    source backup-name))
                  (mevedel-session-persistence-write-current-buffer-atomically
                   target)))
              (cl-incf restored))
          (error
           (push (list :path target
                       :reason (error-message-string err))
                 unrestored)))))
    (list :restored restored
          :unrestored (nreverse unrestored)
          :external (nreverse external))))

(defun mevedel-session-fork--worktree-fork-disclosure
    (session report)
  "Return durable Worktree Fork disclosure for SESSION and REPORT."
  (let* ((unrestored (plist-get report :unrestored))
         (dropped (plist-get report :dropped))
         (partial (or unrestored dropped)))
    (format
     (concat "\n\n<system-reminder>\n"
           "%s\n"
           "Source session: %s\n"
           "Worktree directory: %s\n"
           "Branch: %s\n"
           "Base commit: %s\n"
           "Captured repository files restored: %d\n"
           "%s"
           "Uncaptured files retain the base commit's contents. "
           "Uncommitted Source changes were not copied.\n"
           "%s"
           "%s"
           "%s"
           "</system-reminder>\n")
     (if partial "Worktree Fork (partial restoration)" "Worktree Fork")
     (mevedel-session-forked-from-session-id session)
     (mevedel-session-fork--target-native-report-path
      session (mevedel-session-worktree-directory session))
     (mevedel-session-worktree-branch session)
     (mevedel-session-worktree-base-commit session)
     (plist-get report :restored)
     (if partial
         (concat
          "WARNING: Restoration was partial; this is not an exact "
          "historical checkout.\n")
       "")
     (if-let* ((external (plist-get report :external)))
         (format
          "External captured paths remain shared and non-isolated: %s\n"
          (string-join
           (mapcar
            (lambda (path)
              (mevedel-session-fork--target-native-report-path
               session path))
            external)
           ", "))
       "")
     (if unrestored
         (format
          "Unrestored captured files:\n%s\n"
          (mapconcat
           (lambda (item)
             (format "- %s: %s"
                     (mevedel-session-fork--target-native-report-path
                      session (plist-get item :path))
                     (plist-get item :reason)))
           unrestored "\n"))
       "")
     (if dropped
         (format
          "Dropped malformed copied path state:\n%s\n"
          (mapconcat (lambda (item) (format "- %s" item))
                     dropped "\n"))
        ""))))

(defun mevedel-session-fork--target-native-report-path (session path)
  "Return PATH without SESSION's client-specific remote prefix."
  (if-let* ((target (mevedel-session-execution-target session))
            ((stringp path)))
      (condition-case _
          (mevedel-execution-target-native-path target path)
        (mevedel-execution-target-error "<path outside session target>"))
    path))

(defun mevedel-session-fork--worktree-fork-retained-error
    (session failure reservation)
  "Return the retained-artifact error for SESSION, FAILURE, and RESERVATION."
  (format
   "%s; Worktree Fork artifacts retained: branch %s, directory %s. Cleanup: %s"
   (error-message-string failure)
   (plist-get reservation :branch)
   (mevedel-session-fork--target-native-report-path
    session (plist-get reservation :directory))
   (plist-get reservation :cleanup-command)))

(defun mevedel-session-fork-conversation-fork (buffer target)
  "Create and open a Conversation Fork of BUFFER at stable TARGET.

The child receives truncated conversation history and the Source working
directory.  Working files are neither restored nor copied.  Return the new
child data buffer without mutating the Source buffer, session, or lock."
  (let* ((session (buffer-local-value 'mevedel--session buffer))
         (_ (unless session
              (user-error "Active buffer has no mevedel session")))
         (_ (mevedel-session-artifacts-assert-mutation-authority
             session buffer))
         (target
          (mevedel-session-rewind-resolve-fork-target session target))
         (_ (mevedel-session-rewind-assert-stable-source
             session buffer "forking"))
         (parent-save-path (mevedel-session-save-path session))
         (_ (unless parent-save-path
              (user-error "Save the session before forking")))
         (picked-segment (plist-get target :segment))
         (picked-cum-turn (plist-get target :cum-turn))
         (sessions-dir
          (mevedel-session-artifacts-sessions-dir
           (mevedel-session-workspace session)))
         (child-name
          (mevedel-session-fork--fork-child-name
           session 'conversation))
         (new-id
          (mevedel-session-persistence-allocate-session-id
           child-name sessions-dir))
         (new-save-path
          (file-name-as-directory (file-name-concat sessions-dir new-id)))
         (staging-path
          (file-name-as-directory
           (make-temp-file
            (expand-file-name ".mevedel-fork-" sessions-dir) t)))
         (staging-buffer
          (generate-new-buffer " *mevedel-conversation-fork*"))
         (additional-roots
          (buffer-local-value 'mevedel-workspace-additional-roots buffer))
         child)
    (unwind-protect
        (progn
          (let ((now (format-time-string "%FT%H-%M-%S")))
            (setq child
                  (mevedel-session-fork-clone-session
                   session 'fork
                   :save-path staging-path
                   :session-id new-id
                   :created-at now
                   :updated-at now
                   :current-segment picked-segment
                   :forked-from-session-id
                   (mevedel-session-session-id session)
                   :forked-from-turn picked-cum-turn)))
          (setf (mevedel-session-name child) child-name
                (mevedel-session-fork-type child) 'conversation
                (mevedel-session-forked-from-fork-point-id child)
                (plist-get target :fork-point-id))
          (with-current-buffer staging-buffer
            (let ((org-agenda-file-menu-enabled nil))
              (org-mode)))
          (mevedel-session-rewind-load-rewind-target
           session staging-buffer target)
          (with-current-buffer staging-buffer
            (goto-char (point-max))
            (let ((start (point)))
              (insert
               (mevedel-session-fork--conversation-fork-disclosure
                child))
              (set-text-properties start (point) nil)))
          (mevedel-session-fork--publish-fork
           child buffer staging-buffer parent-save-path staging-path
           new-save-path picked-segment picked-cum-turn additional-roots))
      (when (file-directory-p staging-path)
        (ignore-errors (delete-directory staging-path t)))
      (when (buffer-live-p staging-buffer)
        (with-current-buffer staging-buffer
          (set-buffer-modified-p nil)
          (setq-local kill-buffer-hook nil))
        (kill-buffer staging-buffer)))))

(defun mevedel-session-fork-worktree-fork (buffer target)
  "Create and open a Worktree Fork of BUFFER at stable TARGET."
  (let* ((session (buffer-local-value 'mevedel--session buffer))
         (_ (unless session
              (user-error "Active buffer has no mevedel session")))
         (_ (mevedel-session-artifacts-assert-mutation-authority
             session buffer))
         (target
          (mevedel-session-rewind-resolve-fork-target session target))
         (_ (mevedel-session-rewind-assert-stable-source
             session buffer "forking"))
         (parent-save-path (mevedel-session-save-path session))
         (_ (unless parent-save-path
              (user-error "Save the session before forking")))
         (picked-segment (plist-get target :segment))
         (picked-cum-turn (plist-get target :cum-turn))
         (sessions-dir
          (mevedel-session-artifacts-sessions-dir
           (mevedel-session-workspace session)))
         (reservation
          (or (plist-get target :worktree-reservation)
              (mevedel-worktree-fork-reservation session)))
         (_ (mevedel-worktree-fork-validate-reservation
             session reservation))
         (child-name
          (mevedel-session-fork--fork-child-name session 'worktree))
         (new-id
          (mevedel-session-persistence-allocate-session-id
           child-name sessions-dir))
         (new-save-path
          (file-name-as-directory (file-name-concat sessions-dir new-id)))
         (staging-path
          (file-name-as-directory
           (make-temp-file
            (expand-file-name ".mevedel-worktree-fork-" sessions-dir) t)))
         (staging-buffer
          (generate-new-buffer " *mevedel-worktree-fork*"))
         (source-roots
          (buffer-local-value 'mevedel-workspace-additional-roots buffer))
         child report dropped additional-roots worktree-created failure result)
    (condition-case err
        (setq result
              (unwind-protect
                  (progn
                    (mevedel-worktree-fork-create reservation)
                    (let ((now (format-time-string "%FT%H-%M-%S")))
                      (setq worktree-created t
                            child
                            (mevedel-session-fork-clone-session
                             session 'fork
                             :save-path staging-path
                             :session-id new-id
                             :created-at now
                             :updated-at now
                             :current-segment picked-segment
                             :forked-from-session-id
                             (mevedel-session-session-id session)
                             :forked-from-turn picked-cum-turn)))
                    (setf
                     (mevedel-session-name child) child-name
                     (mevedel-session-fork-type child) 'worktree
                     (mevedel-session-forked-from-fork-point-id child)
                     (plist-get target :fork-point-id)
                     (mevedel-session-worktree-source-root child)
                     (plist-get reservation :source-root)
                     (mevedel-session-worktree-directory child)
                     (plist-get reservation :directory)
                     (mevedel-session-worktree-branch child)
                     (plist-get reservation :branch)
                     (mevedel-session-worktree-base-commit child)
                     (plist-get reservation :base-commit))
                    (setq dropped
                          (mevedel-session-fork--retarget-worktree-state child))
                    (let ((roots
                           (mevedel-session-fork--retarget-worktree-roots
                            child source-roots)))
                      (setq additional-roots (plist-get roots :roots)
                            dropped
                            (nconc dropped (plist-get roots :dropped))))
                    (setq report
                          (mevedel-session-fork--restore-worktree-files
                           session child picked-cum-turn))
                    (setq report (plist-put report :dropped dropped))
                    (with-current-buffer staging-buffer
                      (let ((org-agenda-file-menu-enabled nil))
                        (org-mode)))
                    (mevedel-session-rewind-load-rewind-target
                     session staging-buffer target)
                    (with-current-buffer staging-buffer
                      (goto-char (point-max))
                      (let ((start (point)))
                        (insert
                         (mevedel-session-fork--worktree-fork-disclosure
                          child report))
                        (set-text-properties start (point) nil)))
                    (mevedel-session-fork--publish-fork
                     child buffer staging-buffer parent-save-path staging-path
                     new-save-path picked-segment picked-cum-turn
                     additional-roots))
                (when (file-directory-p staging-path)
                  (ignore-errors (delete-directory staging-path t)))
                (when (buffer-live-p staging-buffer)
                  (with-current-buffer staging-buffer
                    (set-buffer-modified-p nil)
                    (setq-local kill-buffer-hook nil))
                  (kill-buffer staging-buffer))))
      (error (setq failure err)))
    (if (not failure)
        result
      (if worktree-created
          (error "%s"
                 (mevedel-session-fork--worktree-fork-retained-error
                  session failure reservation))
        (signal (car failure) (cdr failure))))))

(defun mevedel-session-fork--commit-remote-rename
    (session buffer new-name new-id new-save-path)
  "Rename portable project SESSION and commit NEW-NAME through its current
lease head.

BUFFER is SESSION's root data buffer.  NEW-ID and NEW-SAVE-PATH name the moved
session tree.  Return a post-commit lease error, or nil.  A failure before the
sidecar marker CAS rolls the directory and in-memory paths back while the same
lease generation remains owned."
  (let* ((old-save-path (mevedel-session-save-path session))
         (old-id (mevedel-session-session-id session))
         (old-name (mevedel-session-name session))
         (old-publication
          (or (mevedel-session-publication session)
              (mevedel-session-publication-read old-save-path)))
         (head-before (plist-get old-publication :head))
         (old-buffer-file (buffer-local-value 'buffer-file-name buffer))
         (old-buffer-truename
          (buffer-local-value 'buffer-file-truename buffer))
         (new-buffer-file
          (and old-buffer-file
               (file-name-concat
                new-save-path (file-name-nondirectory old-buffer-file))))
         moved committed operation-error rollback-error post-commit-error)
    (unless head-before
      (error "Portable project Rename requires a committed publication"))
    (condition-case err
        (progn
          (mevedel-session-durability-call-with-reserved-lease
           session
           (lambda ()
             (rename-file (directory-file-name old-save-path)
                          (directory-file-name new-save-path))
             (setq moved t)
             ;; Reservation finalization must immediately follow the moved
             ;; lease at its new qualified path.
             (setf (mevedel-session-save-path session) new-save-path
                   (mevedel-session-session-id session) new-id
                   (mevedel-session-name session) new-name)
             (with-current-buffer buffer
               (setq buffer-file-name new-buffer-file
                     buffer-file-truename nil))
             (mevedel-execution-relocate-artifacts
              session old-save-path new-save-path)
             (setf (mevedel-session-publication session)
                   (or
                    (mevedel-session-publication-read new-save-path)
                    (error "Moved session has no committed publication")))))
          (condition-case publish-error
              (mevedel-session-publication-publish
               session
               (list
                (list
                 :path
                 (mevedel-session-artifacts-sidecar-path new-save-path)
                 :content
                 (mevedel-session-artifacts-printed-value
                  (mevedel-session-artifacts-build-sidecar session buffer))
                 :commit-marker t)))
            (error
             (if (not
                  (equal head-before
                         (plist-get (mevedel-session-publication session)
                                    :head)))
                 (setq committed t
                       post-commit-error publish-error)
               (signal (car publish-error) (cdr publish-error)))))
          (unless committed
            (when (equal head-before
                         (plist-get (mevedel-session-publication session)
                                    :head))
              (error "Portable project Rename did not commit a publication head"))
            (setq committed t)))
      (error (setq operation-error err)))
    (when (and operation-error moved (not committed))
      (condition-case err
          (progn
            (mevedel-session-durability-call-with-reserved-lease
             session
             (lambda ()
               (rename-file (directory-file-name new-save-path)
                            (directory-file-name old-save-path))
               (setf (mevedel-session-save-path session) old-save-path
                     (mevedel-session-session-id session) old-id
                     (mevedel-session-name session) old-name
                     (mevedel-session-publication session) old-publication)
               (with-current-buffer buffer
                 (setq buffer-file-name old-buffer-file
                       buffer-file-truename old-buffer-truename))
               (mevedel-execution-relocate-artifacts
                session new-save-path old-save-path)))
            (mevedel-session-publication-discard-rolled-back
             session))
        (error (setq rollback-error err))))
    (when rollback-error
      (error
       "Portable project Rename rollback incomplete after %s: %s"
       (error-message-string operation-error)
       (error-message-string rollback-error)))
    (when operation-error
      (signal (car operation-error) (cdr operation-error)))
    post-commit-error))

;;;###autoload
(defun mevedel-rename-session (new-name)
  "Rename the current session to NEW-NAME.

Updates `:session-name', renames the on-disk session directory (so
its prefix matches the new name), updates `:save-path' / `:session-id'
on the session struct, repoints the buffer's variable `buffer-file-name'
to the renamed directory, rewrites the sidecar, and renames the chat
buffer per `mevedel-session-buffer-name'.

Works from a chat buffer or a view buffer."
  (interactive "sNew session name: ")
  (let* ((data-buf
          (cond
           ((and (boundp 'mevedel--session) mevedel--session) (current-buffer))
           ((and (boundp 'mevedel--data-buffer) mevedel--data-buffer
                 (buffer-live-p mevedel--data-buffer))
            mevedel--data-buffer)
           (t (user-error "Not in a mevedel chat or view buffer"))))
         (session (buffer-local-value 'mevedel--session data-buf)))
    (unless session
      (user-error "Active buffer has no mevedel session"))
    (mevedel-session-artifacts-assert-mutation-authority session data-buf)
    (let ((sanitized (mevedel-session-artifacts-sanitize new-name)))
      (when (string-empty-p sanitized)
        (user-error "Empty session name"))
      ;; Rename the on-disk directory if the session is materialized.
      (let (post-commit-error)
      (when (mevedel-session-save-path session)
        (let* ((old-save-path (mevedel-session-save-path session))
               (parent-dir    (file-name-directory
                               (directory-file-name old-save-path)))
               (old-id        (or (mevedel-session-session-id session)
                                  (file-name-nondirectory
                                   (directory-file-name old-save-path))))
               (old-name-sanitized
                (mevedel-session-artifacts-sanitize
                 (mevedel-session-name session)))
               ;; Derive the suffix by stripping the old sanitized
               ;; name from the start of the id.  This does not
               ;; hard-code the id format beyond "name + dash +
               ;; whatever" -- changes to the timestamp/uuid portion
               ;; do not break rename.  Fallback: rebuild a fresh id
               ;; from scratch.
               (prefix         (concat old-name-sanitized "-"))
               (suffix         (if (and (stringp old-id)
                                        (string-prefix-p prefix old-id))
                                   (substring old-id (length old-name-sanitized))
                                 nil))
               (new-id         (if suffix
                                   (concat sanitized suffix)
                                 (mevedel-session-artifacts-compute-id
                                  sanitized)))
               (new-save-path  (file-name-as-directory
                                (file-name-concat parent-dir new-id))))
          (if (mevedel-session-codec-portable-authority-p session)
              (setq post-commit-error
                    (mevedel-session-fork--commit-remote-rename
                     session data-buf sanitized new-id new-save-path))
            (rename-file (directory-file-name old-save-path)
                         (directory-file-name new-save-path))
            (mevedel-execution-relocate-artifacts
             session old-save-path new-save-path)
            (setf (mevedel-session-save-path session) new-save-path
                  (mevedel-session-session-id session) new-id)
            (with-current-buffer data-buf
              (when buffer-file-name
                (setq buffer-file-name
                      (file-name-concat
                       new-save-path
                       (file-name-nondirectory buffer-file-name))))))))
      ;; PID-lock and unmaterialized sessions retain the direct metadata write.
      (unless (and (mevedel-session-save-path session)
                   (mevedel-session-codec-portable-authority-p session))
        (setf (mevedel-session-name session) sanitized)
        (when (mevedel-session-save-path session)
          (mevedel-session-artifacts-publish-text
           session
           (mevedel-session-artifacts-sidecar-path
            (mevedel-session-save-path session))
           (mevedel-session-artifacts-printed-value
            (mevedel-session-artifacts-build-sidecar session data-buf)))))
      ;; Rename the chat buffer per the convention.  The view observes this
      ;; semantic event and derives its own presentation name.
      (let* ((workspace (mevedel-session-workspace session))
             (new-data-name (mevedel-session-buffer-name sanitized workspace)))
        (with-current-buffer data-buf
          (rename-buffer new-data-name t))
        (mevedel-session-persistence-notify-session-event
         session 'rename new-data-name))
      (message "Session renamed to %s" sanitized)
      (when post-commit-error
        (error "Portable project Rename committed, but lease finalization failed: %s"
               (error-message-string post-commit-error)))))))


(provide 'mevedel-session-fork)

;;; mevedel-session-fork.el ends here
