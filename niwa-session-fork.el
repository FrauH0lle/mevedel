;;; niwa-session-fork.el --- Conversation and Worktree Fork persistence -*- lexical-binding: t -*-

;;; Commentary:

;; Owns Fork projection, publication, Worktree restoration.

;;; Code:

(eval-when-compile (require 'niwa-utilities))
(require 'cl-lib)

(eval-when-compile
  (require 'niwa-agents)
  (require 'niwa-structs))

;; `niwa-agent-persistence'
(declare-function niwa-agent-persistence-transcript-path-p "niwa-agent-persistence" (path save-path))
(autoload 'niwa-agent-persistence-transcript-path-p
  "niwa-agent-persistence")

;; `niwa-engine'
(declare-function niwa-engine-assert-local-history "niwa-engine" (session operation &optional backend root-only))
(autoload 'niwa-engine-assert-local-history "niwa-engine")

;; `niwa-execution'
(declare-function niwa-execution-stop-services "niwa-execution" (session))
(autoload 'niwa-execution-stop-services "niwa-execution")

;; `niwa-execution-target'
(declare-function niwa-execution-target-native-path "niwa-execution-target" (target path))
(autoload 'niwa-execution-target-native-path "niwa-execution-target")

;; `niwa-plan'
(declare-function niwa-plan-artifact-path-p "niwa-plan" (path))
(declare-function niwa-plan-hash "niwa-plan" (body))
(autoload 'niwa-plan-artifact-path-p "niwa-plan")
(autoload 'niwa-plan-hash "niwa-plan")

;; `niwa-reminders'
(declare-function niwa-reminders-clone-list "niwa-reminders" (reminders))
(autoload 'niwa-reminders-clone-list "niwa-reminders")

;; `niwa-session-artifacts'
(declare-function niwa-session-artifacts--live-sidecar "niwa-session-artifacts" (session &optional data-buffer))
(declare-function niwa-session-artifacts-artifact-present-p "niwa-session-artifacts" (session logical &optional committed-only))
(declare-function niwa-session-artifacts-assert-mutation-authority "niwa-session-artifacts" (session &optional buffer))
(declare-function niwa-session-artifacts-read-artifact "niwa-session-artifacts" (session logical &optional committed-only))
(declare-function niwa-session-artifacts-save-buffer-silently "niwa-session-artifacts" ())
(declare-function niwa-session-artifacts-segment-path "niwa-session-artifacts" (save-path n))
(declare-function niwa-session-artifacts-sessions-dir "niwa-session-artifacts" (workspace))
(declare-function niwa-session-artifacts-sidecar-path "niwa-session-artifacts" (save-path))
(declare-function niwa-session-artifacts-stabilize-gptel-bounds "niwa-session-artifacts" ())
(autoload 'niwa-session-artifacts--live-sidecar
  "niwa-session-artifacts")
(autoload 'niwa-session-artifacts-artifact-present-p
  "niwa-session-artifacts")
(autoload 'niwa-session-artifacts-assert-mutation-authority
  "niwa-session-artifacts")
(autoload 'niwa-session-artifacts-read-artifact
  "niwa-session-artifacts")
(autoload 'niwa-session-artifacts-save-buffer-silently
  "niwa-session-artifacts")
(autoload 'niwa-session-artifacts-segment-path
  "niwa-session-artifacts")
(autoload 'niwa-session-artifacts-sessions-dir
  "niwa-session-artifacts")
(autoload 'niwa-session-artifacts-sidecar-path
  "niwa-session-artifacts")
(autoload 'niwa-session-artifacts-stabilize-gptel-bounds
  "niwa-session-artifacts")

;; `niwa-session-codec'
(declare-function niwa-session-codec-deserialize "niwa-session-codec" (plist workspace))
(declare-function niwa-session-codec-read "niwa-session-codec" (path))
(declare-function niwa-session-codec-write "niwa-session-codec" (path plist))
(autoload 'niwa-session-codec-deserialize "niwa-session-codec")
(autoload 'niwa-session-codec-read "niwa-session-codec")
(autoload 'niwa-session-codec-write "niwa-session-codec")

;; `niwa-session-durability'
(declare-function niwa-session-durability-call-with-reserved-lease "niwa-session-durability" (session function))
(declare-function niwa-session-durability-forget-removed-session "niwa-session-durability" (session))
(autoload 'niwa-session-durability-call-with-reserved-lease
  "niwa-session-durability")
(autoload 'niwa-session-durability-forget-removed-session
  "niwa-session-durability")

;; `niwa-session-persistence'
(declare-function niwa-session-persistence--execution-successor-ids "niwa-session-persistence" (text))
(declare-function niwa-session-persistence--reconcile-lost-execution-file "niwa-session-persistence" (path text &optional successor-execution-ids artifact-callback lost-monitor))
(declare-function niwa-session-persistence-allocate-session-id "niwa-session-persistence" (sessions-dir))
(declare-function niwa-session-persistence-find-live-buffer "niwa-session-persistence" (session-id buf-name))
(declare-function niwa-session-persistence-lock-acquire "niwa-session-persistence" (session-dir buffer-name &optional session))
(declare-function niwa-session-persistence-lock-release "niwa-session-persistence" (session-dir &optional session))
(declare-function niwa-session-persistence-restore "niwa-session-persistence" (session-dir &optional lifecycle-source session-override workspace))
(declare-function niwa-session-persistence-write-current-buffer-atomically "niwa-session-persistence" (path))
(autoload 'niwa-session-persistence--execution-successor-ids
  "niwa-session-persistence")
(autoload 'niwa-session-persistence--reconcile-lost-execution-file
  "niwa-session-persistence")
(autoload 'niwa-session-persistence-allocate-session-id
  "niwa-session-persistence")
(autoload 'niwa-session-persistence-find-live-buffer
  "niwa-session-persistence")
(autoload 'niwa-session-persistence-lock-acquire
  "niwa-session-persistence")
(autoload 'niwa-session-persistence-lock-release
  "niwa-session-persistence")
(autoload 'niwa-session-persistence-restore
  "niwa-session-persistence")
(autoload 'niwa-session-persistence-write-current-buffer-atomically
  "niwa-session-persistence")

;; `niwa-session-publication'
(declare-function niwa-session-publication-publish "niwa-session-publication" (session artifacts &optional require-commit))
(declare-function niwa-session-publication-read "niwa-session-publication" (session-dir &optional head names))
(autoload 'niwa-session-publication-publish "niwa-session-publication")
(autoload 'niwa-session-publication-read "niwa-session-publication")

;; `niwa-session-rewind'
(declare-function niwa-session-rewind-assert-stable-source "niwa-session-rewind" (session buffer operation))
(declare-function niwa-session-rewind-load-rewind-target "niwa-session-rewind" (session buffer target &optional before-turn))
(declare-function niwa-session-artifacts-read-backup "niwa-session-artifacts" (session backup-name))
(declare-function niwa-session-rewind-reduce-agent-transcripts "niwa-session-rewind" (entries picked-cum-turn))
(declare-function niwa-session-rewind-reduce-file-snapshots "niwa-session-rewind" (snapshots picked-cum-turn &optional before-turn))
(declare-function niwa-session-rewind-reduce-prompt-index "niwa-session-rewind" (index picked-segment picked-cum-turn &optional before-turn))
(declare-function niwa-session-rewind-resolve-fork-target "niwa-session-rewind" (session target))
(declare-function niwa-session-rewind-rewind-publication-artifacts "niwa-session-rewind" (session buffer staging-path &optional state))
(declare-function niwa-session-rewind-state-at-turn "niwa-session-rewind" (session cum-turn &optional before-turn))
(autoload 'niwa-session-rewind-assert-stable-source
  "niwa-session-rewind")
(autoload 'niwa-session-rewind-load-rewind-target
  "niwa-session-rewind")
(autoload 'niwa-session-artifacts-read-backup "niwa-session-artifacts")
(autoload 'niwa-session-rewind-reduce-agent-transcripts
  "niwa-session-rewind")
(autoload 'niwa-session-rewind-reduce-file-snapshots
  "niwa-session-rewind")
(autoload 'niwa-session-rewind-reduce-prompt-index
  "niwa-session-rewind")
(autoload 'niwa-session-rewind-resolve-fork-target
  "niwa-session-rewind")
(autoload 'niwa-session-rewind-rewind-publication-artifacts
  "niwa-session-rewind")
(autoload 'niwa-session-rewind-state-at-turn "niwa-session-rewind")

;; `niwa-structs'
(declare-function niwa-session--create "niwa-structs" (&rest slots))
(declare-function niwa-session-agent-turn-capacity "niwa-structs" (cl-x))
(declare-function niwa-session-attached-artifacts "niwa-structs" (cl-x))
(declare-function niwa-session-buffer-name "niwa-structs" (session-name workspace))
(declare-function niwa-session-chapters "niwa-structs" (cl-x))
(declare-function niwa-session-created-at "niwa-structs" (cl-x))
(declare-function niwa-session-current-segment "niwa-structs" (cl-x))
(declare-function niwa-session-enqueue-pending-reminder
                  "niwa-structs" (session body))
(declare-function niwa-session-execution-target "niwa-structs" (cl-x))
(declare-function niwa-session-file-snapshots "niwa-structs" (cl-x))
(declare-function niwa-session-untracked-effects "niwa-structs" (cl-x))
(declare-function niwa-session-fork-type "niwa-structs" (cl-x))
(declare-function niwa-session-forked-from-fork-point-id "niwa-structs" (cl-x))
(declare-function niwa-session-forked-from-session-id "niwa-structs" (cl-x))
(declare-function niwa-session-goal "niwa-structs" (cl-x))
(declare-function niwa-session-name "niwa-structs" (cl-x))
(declare-function niwa-session-permission-mode "niwa-structs" (cl-x))
(declare-function niwa-session-permission-rules "niwa-structs" (cl-x))
(declare-function niwa-session-plan-mode "niwa-structs" (cl-x))
(declare-function niwa-session-preset-name "niwa-structs" (cl-x))
(declare-function niwa-session-prompt-index "niwa-structs" (cl-x))
(declare-function niwa-session-publication "niwa-structs" (cl-x))
(declare-function niwa-session-resource-grants "niwa-structs" (cl-x))
(declare-function niwa-session-sandbox-mode "niwa-structs" (cl-x))
(declare-function niwa-session-save-path "niwa-structs" (cl-x))
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
(defvar niwa--session)

;; `niwa-tool-render-data'
(declare-function niwa-tool-render-data-reconcile-lost-executions
                  "niwa-tool-render-data"
                  (buffer &optional successor-execution-ids))
(autoload 'niwa-tool-render-data-reconcile-lost-executions
  "niwa-tool-render-data")

;; `niwa-utilities'
(declare-function niwa--normalize-message-text "niwa-utilities" (text))
(autoload 'niwa--normalize-message-text "niwa-utilities")

;; `niwa-view'
(defvar niwa--data-buffer)

;; `niwa-workspace'
(defvar niwa-workspace-additional-roots)

;; `niwa-worktree'
(declare-function niwa-worktree-fork-create "niwa-worktree" (reservation))
(declare-function niwa-worktree-fork-reservation "niwa-worktree" (session &optional preflight))
(declare-function niwa-worktree-fork-validate-reservation "niwa-worktree" (session reservation))
(autoload 'niwa-worktree-fork-create "niwa-worktree")
(autoload 'niwa-worktree-fork-reservation "niwa-worktree")
(autoload 'niwa-worktree-fork-validate-reservation "niwa-worktree")

;; `org'
(defvar org-agenda-file-menu-enabled)

;;
;;; Fork-on-send

(defun niwa-session-fork--agent-files-for-segments
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

(defconst niwa-session-fork--clone-slot-names
  '(name naming-state naming-input workspace execution-target authority-mode working-directory
    tasks task-status-notes last-task-write-turn touched-files
    permission-rules resource-grants permission-mode sandbox-mode plan-mode
    preset-name model-provider external-conversations
    last-outcome recovery-issues
    reasoning-effort turn-count reminders last-observed-date
    agent-types-snapshot pending-reminders
    tool-catalog
    messages agent-registry agent-reservations
    agent-root-activity agent-root-waiter agent-turn-capacity pending-steering
    pending-follow-ups pending-input-next-id pending-input-paused
    pending-input-failure-paused dropped-file-grants active-dropped-file-grants
    mentions-shown workspace-instruction-hashes skills hook-rules hook-log
    hook-log-pending repair-log
    repair-log-pending permission-log-pending telemetry-pending
    hook-context-pending execution-state audit-session pending-publication
    publication publication-queue publication-uncommitted-batches
    checkpoint-state publication-active-p publication-job publication-preparations
    publication-preparation-token publication-finishing-p publication-stopping-p
    mail-batch mail-notifications
    mail-notification-timer mail-notification-delay control-transfer control-transfer-drains root-buffer
    lease ownership-epoch
    lease-renewal-timer lease-renewal-job save-path session-id created-at updated-at
    current-segment forked-from-session-id forked-from-turn fork-type
    forked-from-fork-point-id worktree-source-root worktree-directory
    worktree-branch worktree-base-commit prompt-index chapters file-snapshots
    untracked-effects
    ptc-checkpoints durable-tree-ensured
    agent-transcripts invoked-skills permission-queue permission-absence
    pending-plan-approval
    plan-metadata goal attached-artifacts dedicated-artifact note-conversation
    schedules)
  "Every `niwa-session' slot decided by the logical clone constructor.")

(defun niwa-session-fork--assert-clone-slot-completeness ()
  "Signal when the session clone policy no longer covers the struct."
  (let ((actual
         (mapcar #'car
                 (cdr (cl-struct-slot-info 'niwa-session))))
        (expected niwa-session-fork--clone-slot-names))
    (unless (equal actual expected)
      (error "Session clone policy is incomplete: missing %S, extra %S"
             (cl-set-difference actual expected)
             (cl-set-difference expected actual)))
    t))

(cl-defun niwa-session-fork-clone-session
    (session policy &key name save-path session-id created-at updated-at
             current-segment forked-from-session-id forked-from-turn)
  "Build an explicit independent logical clone of SESSION for POLICY.

POLICY is `fork' or `save-as'.  Workspace and execution-target are shared
because they are immutable authority identities owned outside the session.
Every other slot is decided here: durable logical containers are copied,
fork-only projections are reduced, and runtime/control state starts empty.
The identity and timestamp keywords describe the new materialized child."
  (niwa-engine-assert-local-history session "Session cloning")
  (niwa-session-fork--assert-clone-slot-completeness)
  (unless (memq policy '(fork save-as))
    (error "Unknown session clone policy: %S" policy))
  (let* ((fork-p (eq policy 'fork))
         (turn (if fork-p
                   forked-from-turn
                 (niwa-session-turn-count session)))
         (child
          (niwa-session--create
           :name (if fork-p session-id name)
           :naming-state (if fork-p 'pending 'explicit)
           ;; A child's title is its own: Fork names from its next prompt, and
           ;; Save As establishes an explicit name.
           :naming-input nil
           :workspace (niwa-session-workspace session)
           :execution-target (niwa-session-execution-target session)
           :authority-mode (niwa-session-authority-mode session)
           :working-directory (niwa-session-working-directory session)
           :tasks (unless fork-p
                    (copy-tree (niwa-session-tasks session) t))
           :task-status-notes (unless fork-p
                                (copy-tree
                                 (niwa-session-task-status-notes session)
                                 t))
           :last-task-write-turn (unless fork-p
                                   (niwa-session-last-task-write-turn
                                    session))
           :touched-files (make-hash-table :test #'equal)
           :permission-rules
           (copy-tree (niwa-session-permission-rules session) t)
           :resource-grants
           (copy-tree (niwa-session-resource-grants session) t)
           :permission-mode (niwa-session-permission-mode session)
           :sandbox-mode (niwa-session-sandbox-mode session)
           :plan-mode (niwa-session-plan-mode session)
           :preset-name (niwa-session-preset-name session)
           :note-conversation (unless fork-p (niwa-session-note-conversation session))
           :model-provider (niwa-session-model-provider session)
           :external-conversations nil
           :last-outcome nil
           :recovery-issues nil
           :reasoning-effort (niwa-session-reasoning-effort session)
           :turn-count turn
           :reminders
           (niwa-reminders-clone-list
            (niwa-session-reminders session))
           :last-observed-date (niwa-session-last-observed-date session)
           :agent-types-snapshot
           (copy-tree (niwa-session-agent-types-snapshot session) t)
           :pending-reminders nil
           :tool-catalog
           (copy-tree (niwa-session-tool-catalog session) t)
           :messages (unless fork-p
                       (copy-tree (niwa-session-messages session) t))
           :agent-registry (unless fork-p
                             (copy-tree
                              (niwa-session-agent-registry session) t))
           :agent-reservations nil
           :agent-root-activity 'idle
           :agent-root-waiter nil
           :agent-turn-capacity
           (niwa-session-agent-turn-capacity session)
           :pending-steering nil
           :pending-follow-ups nil
           :pending-input-next-id nil
           :pending-input-paused nil
           :pending-input-failure-paused nil
           :dropped-file-grants nil
           :active-dropped-file-grants nil
           :mentions-shown (make-hash-table :test #'equal)
           :workspace-instruction-hashes nil
           :skills (copy-tree (niwa-session-skills session) t)
           :hook-rules (and fork-p
                            (copy-tree
                             (niwa-session-hook-rules session) t))
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
           :publication-job nil
           :publication-preparations nil
           :publication-preparation-token nil
           :publication-finishing-p nil
           :publication-stopping-p nil
           :mail-batch nil
           :mail-notifications nil
           :mail-notification-timer nil
           :mail-notification-delay nil
           :control-transfer nil
           :control-transfer-drains nil
           :root-buffer nil
           :lease nil
           :ownership-epoch 0
           :lease-renewal-timer nil
           :lease-renewal-job nil
           :save-path save-path
           :session-id session-id
           :created-at (or created-at
                           (niwa-session-created-at session))
           :updated-at (or updated-at
                           (niwa-session-updated-at session))
           :current-segment
           (if fork-p
               current-segment
             (niwa-session-current-segment session))
           :forked-from-session-id forked-from-session-id
           :forked-from-turn turn
           :fork-type (unless fork-p
                        (niwa-session-fork-type session))
           :forked-from-fork-point-id
           (unless fork-p
             (niwa-session-forked-from-fork-point-id session))
           :worktree-source-root
           (unless fork-p
             (niwa-session-worktree-source-root session))
           :worktree-directory
           (unless fork-p
             (niwa-session-worktree-directory session))
           :worktree-branch
           (unless fork-p
             (niwa-session-worktree-branch session))
           :worktree-base-commit
           (unless fork-p
             (niwa-session-worktree-base-commit session))
           :prompt-index
           (copy-tree
            (if fork-p
                (niwa-session-rewind-reduce-prompt-index
                 (niwa-session-prompt-index session)
                 current-segment turn)
              (niwa-session-prompt-index session))
            t)
           ;; A fork keeps only the chats that ended before its fork segment.
           :chapters
           (copy-tree
            (if fork-p
                (seq-filter (lambda (chapter)
                              (< (plist-get chapter :segment) current-segment))
                            (niwa-session-chapters session))
              (niwa-session-chapters session)))
           :file-snapshots
           (copy-tree
            (if fork-p
                (niwa-session-rewind-reduce-file-snapshots
                 (niwa-session-file-snapshots session) turn)
              (niwa-session-file-snapshots session))
            t)
           :untracked-effects
           (copy-tree
            (if fork-p
                (niwa-session-rewind-reduce-file-snapshots
                 (niwa-session-untracked-effects session) turn)
              (niwa-session-untracked-effects session))
            t)
           :ptc-checkpoints nil
           :checkpoint-state nil
           :agent-transcripts
           (copy-tree (niwa-session-agent-transcripts session) t)
           :invoked-skills
           (copy-tree
            (if fork-p
                (cl-remove-if
                 (lambda (record)
                   (> (or (niwa-skill-invocation-record-turn record) 0)
                      (or turn 0)))
                 (niwa-session-invoked-skills session))
              (niwa-session-invoked-skills session))
            t)
           :permission-queue nil
           :permission-absence nil
           :pending-plan-approval nil
           :plan-metadata (unless fork-p
                            (copy-tree
                             (niwa-session-plan-metadata session) t))
           :goal (unless fork-p
                   (copy-tree (niwa-session-goal session) t))
           ;; Forks share the attached artifacts by reference.
           :attached-artifacts
           (copy-sequence (niwa-session-attached-artifacts session))
           ;; A fork is the user's own chat, never the artifact's conversation.
           :dedicated-artifact nil
           ;; Wake-ups belong to one conversation: a fork would fire them twice.
           :schedules (unless fork-p
                        (copy-tree (niwa-session-schedules session) t)))))
    (when (and fork-p turn)
      (setf (niwa-session-agent-transcripts child)
            (niwa-session-rewind-reduce-agent-transcripts
             (niwa-session-agent-transcripts child) turn)))
    child))

(defun niwa-session-fork--materialize-fork-artifact
    (source logical staging-path &optional required)
  "Copy SOURCE's committed LOGICAL artifact below STAGING-PATH.

Return non-nil when the artifact was materialized.  When REQUIRED is non-nil,
signal if LOGICAL is absent.  Literal resolver bytes are used so remote fixed
caches never become fork authority."
  (if (niwa-session-artifacts-artifact-present-p source logical t)
      (let ((destination (expand-file-name logical staging-path))
            (content
             (niwa-session-artifacts-read-artifact source logical t)))
        (make-directory (file-name-directory destination) t)
        (let ((coding-system-for-write 'no-conversion))
          (write-region content nil destination nil 'silent))
        t)
    (when required
      (error "Required fork artifact is not published: %s" logical))))

(defun niwa-session-fork--stage-fork
    (child buffer staging-buffer parent-save-path staging-path
           picked-segment picked-cum-turn &optional additional-roots)
  "Materialize CHILD under STAGING-PATH using STAGING-BUFFER."
  (let ((source (buffer-local-value 'niwa--session buffer)))
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
    (when-let* ((source source)
                (metadata (niwa-session-plan-metadata source))
                ((eq (plist-get metadata :status) 'accepted))
                (accepted-turn (plist-get metadata :accepted-turn))
                ((and (integerp accepted-turn)
                      (<= accepted-turn picked-cum-turn)))
                (relative-path (plist-get metadata :accepted-path))
                ((stringp relative-path))
                ;; A normalized session-relative path is the whole
                ;; containment rule: it cannot escape the session store or
                ;; name another target.
                ((niwa-plan-artifact-path-p relative-path)))
      ;; A fork must not inherit an artifact that no longer matches the hash
      ;; the source accepted.  Only committed bytes are authority here; an
      ;; absent artifact is reported by the materialization below.
      (when (niwa-session-artifacts-artifact-present-p
             source relative-path t)
        (let ((body
               (niwa--normalize-message-text
                (decode-coding-string
                 (niwa-session-artifacts-read-artifact
                  source relative-path t)
                 'utf-8-unix))))
          (unless (equal (plist-get metadata :accepted-hash)
                         (niwa-plan-hash body))
            (error "Accepted plan artifact hash does not match"))))
      (niwa-session-fork--materialize-fork-artifact
       source relative-path staging-path t))
    (cl-loop for i from 1 below picked-segment do
             (niwa-session-fork--materialize-fork-artifact
              source
              (file-name-nondirectory
               (niwa-session-artifacts-segment-path parent-save-path i))
              staging-path))
    (with-current-buffer staging-buffer
      (setq-local niwa--session child)
      (setq-local niwa-workspace-additional-roots
                  (copy-tree additional-roots t))
      (setq buffer-file-name
            (niwa-session-artifacts-segment-path
             staging-path picked-segment)
            buffer-file-truename nil)
      (when (> (niwa-tool-render-data-reconcile-lost-executions staging-buffer) 0)
        ;; This bare Org staging buffer has no gptel save hook.  Repairs can
        ;; change text length, so serialize its moved properties before saving.
        (niwa-session-artifacts-stabilize-gptel-bounds))
      (set-buffer-modified-p t)
      (niwa-session-artifacts-save-buffer-silently))
    ;; The private child has staged copies, not an immutable publication yet.
    (let ((successor-ids
           (niwa-session-persistence--execution-successor-ids
            (with-current-buffer staging-buffer (buffer-string)))))
      (cl-loop for segment downfrom (1- picked-segment) to 1
               for path = (niwa-session-artifacts-segment-path
                           staging-path segment)
               for text = (when (file-regular-p path)
                            (with-temp-buffer
                              (insert-file-contents path)
                              (buffer-string)))
               when text
               do (niwa-session-persistence--reconcile-lost-execution-file
                   path text successor-ids)
               and do
               (dolist (id (niwa-session-persistence--execution-successor-ids text))
                 (cl-pushnew id successor-ids :test #'equal))))
    (when picked-cum-turn
      (dolist (entry
               (niwa-session-rewind-state-at-turn
                child picked-cum-turn))
        (when-let* ((backup-name (plist-get (cdr entry) :backup-name)))
          (niwa-session-fork--materialize-fork-artifact
           source (file-name-concat "file-history" backup-name)
           staging-path))))
    (when (and picked-cum-turn parent-save-path)
      (dolist (entry
               (niwa-session-fork--agent-files-for-segments
                (niwa-session-prompt-index child)
                (niwa-session-agent-transcripts child)
                picked-segment picked-cum-turn))
        (when-let* ((rel-path (plist-get (cdr entry) :path))
                    ((niwa-agent-persistence-transcript-path-p
                      rel-path parent-save-path))
                    ((niwa-agent-persistence-transcript-path-p
                      rel-path staging-path)))
          (niwa-session-fork--materialize-fork-artifact
           source rel-path staging-path))))
    (niwa-session-codec-write
     (niwa-session-artifacts-sidecar-path staging-path)
     (niwa-session-artifacts--live-sidecar child staging-buffer))
    (let* ((saved
            (niwa-session-codec-read
             (niwa-session-artifacts-sidecar-path staging-path)))
           (restored
            (niwa-session-codec-deserialize
             saved (niwa-session-workspace child))))
      (unless (equal (niwa-session-session-id
                      (plist-get restored :session))
                     (niwa-session-session-id child))
        (error "Fork staging validation failed"))
      (setf (niwa-session-checkpoint-state child)
            (niwa-session-checkpoint-state (plist-get restored :session))))))

(defun niwa-session-fork--publish-fork
    (child buffer staging-buffer parent-save-path staging-path new-save-path
           picked-segment picked-cum-turn additional-roots)
  "Stage, publish, and restore CHILD as one session-artifact transaction."
  (let (child-buffer published committed)
    (unwind-protect
        (progn
          (unless (niwa-session-persistence-lock-acquire
                   staging-path (buffer-name buffer) child)
            (error "Could not acquire fork session lock"))
          (niwa-session-fork--stage-fork
           child buffer staging-buffer parent-save-path staging-path
           picked-segment picked-cum-turn additional-roots)
          ;; A fork starts a new publication history. Publish its complete
          ;; staged snapshot before the lease and control state move together.
          (niwa-session-publication-publish
           child
           (niwa-session-rewind-rewind-publication-artifacts
            child staging-buffer staging-path))
          (niwa-session-durability-call-with-reserved-lease
           child
           (lambda ()
             (rename-file (directory-file-name staging-path)
                          (directory-file-name new-save-path))
             (setq published t)
             ;; Reservation finalization must address the moved lease.
             (setf (niwa-session-save-path child) new-save-path)))
          (setf (niwa-session-publication child)
                (niwa-session-publication-read new-save-path))
          (setq published t)
          (with-current-buffer staging-buffer
            (set-buffer-modified-p nil)
            (set-visited-file-name nil t)
            (setq-local kill-buffer-hook nil))
          (kill-buffer staging-buffer)
          (setq child-buffer
                (niwa-session-persistence-restore
                 new-save-path "fork" child)
                committed t)
          child-buffer)
      (unless committed
        (when-let* ((failed-buffer
                     (or child-buffer
                         (niwa-session-persistence-find-live-buffer
                          (niwa-session-session-id child)
                          (niwa-session-buffer-name
                           (niwa-session-name child)
                           (niwa-session-workspace child))))))
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
                (niwa-session-persistence-lock-release remaining child))
            (ignore-errors
              (niwa-session-durability-forget-removed-session child)))))
      (when (buffer-live-p staging-buffer)
        (with-current-buffer staging-buffer
          (set-buffer-modified-p nil)
          (setq-local kill-buffer-hook nil))
        (kill-buffer staging-buffer)))))

(defun niwa-session-fork-provenance-body (session)
  "Return SESSION's fork provenance reminder body, or nil for a non-fork.
Regenerated from durable session slots by the `fork-provenance'
reminder instead of living as permanent transcript text: the
provenance is session state, true for the fork's whole life."
  (when (niwa-session-forked-from-session-id session)
    (if (eq (niwa-session-fork-type session) 'worktree)
        (format
         (concat "Worktree Fork\n"
                 "Source session: %s\n"
                 "Worktree directory: %s\n"
                 "Branch: %s\n"
                 "Base commit: %s\n"
                 "Uncaptured files retain the base commit's contents. "
                 "Uncommitted Source changes were not copied.")
         (niwa-session-forked-from-session-id session)
         (niwa-session-fork--target-native-report-path
          session (niwa-session-worktree-directory session))
         (niwa-session-worktree-branch session)
         (niwa-session-worktree-base-commit session))
      (format
       (concat "Conversation Fork\n"
               "Source session: %s\n"
               "Working directory: %s\n"
               "Files were not restored.  This fork uses the current files "
               "in the Source working directory, so they may be newer than "
               "the selected conversation point and changes are shared.")
       (niwa-session-forked-from-session-id session)
       (niwa-session-working-directory session)))))

(defun niwa-session-fork-retarget-worktree-path (session path)
  "Retarget absolute PATH from SESSION's Source root into its worktree."
  (unless (and (stringp path) (file-name-absolute-p path))
    (error "Invalid Worktree Fork path: %S" path))
  (let* ((source-root
          (file-name-as-directory
           (expand-file-name
            (or (niwa-session-worktree-source-root session)
                (error "Worktree Fork has no Source repository root")))))
         (worktree-root
          (file-name-as-directory
           (expand-file-name
            (or (niwa-session-worktree-directory session)
                (error "Worktree Fork has no worktree directory")))))
         (expanded (expand-file-name path)))
    (if (string-prefix-p source-root expanded)
        (expand-file-name (substring expanded (length source-root))
                          worktree-root)
      expanded)))

(defun niwa-session-fork--retarget-worktree-roots
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
                   (niwa-session-fork-retarget-worktree-path
                    session path)
                   paths)
                (push (format "additional root %S" path) dropped)))
            (push (cons (car entry) (nreverse paths)) retargeted))
        (push (format "additional roots entry %S" entry) dropped)))
    (list :roots (nreverse retargeted)
          :dropped (nreverse dropped))))

(defun niwa-session-fork--retarget-worktree-state (session)
  "Retarget SESSION's copied repository-local path state.
Return descriptions of malformed grants and rules dropped from the child."
  (let (grants rules dropped)
    (dolist (grant (niwa-session-resource-grants session))
      (if (and (proper-list-p grant)
               (stringp (plist-get grant :path))
               (file-name-absolute-p (plist-get grant :path)))
          (let ((copy (copy-tree grant t)))
            (plist-put
             copy :path
             (niwa-session-fork-retarget-worktree-path
              session (plist-get copy :path)))
            (push copy grants))
        (push (format "resource grant %S" grant) dropped)))
    (dolist (rule (niwa-session-permission-rules session))
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
           (niwa-session-fork-retarget-worktree-path
            session (plist-get (cdr copy) :path)))
          (push copy rules)))
       (t
        (push (format "permission rule %S" rule) dropped))))
  (setf
   (niwa-session-working-directory session)
   (niwa-session-worktree-directory session)
   (niwa-session-file-snapshots session)
   (mapcar
    (lambda (turn-entry)
      (cons
       (car turn-entry)
       (mapcar
        (lambda (file-entry)
          (cons
           (niwa-session-fork-retarget-worktree-path
            session (car file-entry))
           (copy-tree (cdr file-entry) t)))
        (cdr turn-entry))))
    (niwa-session-file-snapshots session))
   (niwa-session-resource-grants session)
   (nreverse grants)
   (niwa-session-permission-rules session)
   (nreverse rules))
    (nreverse dropped)))

(defun niwa-session-fork--assert-worktree-target
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

(defun niwa-session-fork--restore-worktree-files
    (source child cum-turn)
  "Restore SOURCE's captured repository files into CHILD at CUM-TURN."
  (let* ((source-root
          (file-name-as-directory
           (expand-file-name
            (niwa-session-worktree-source-root child))))
         (worktree-root
          (file-name-as-directory
           (expand-file-name
            (niwa-session-worktree-directory child))))
         (restored 0)
         plan
         unrestored
         external)
    ;; Validate the complete plan before the first child-worktree write.
    (dolist (entry
             (niwa-session-rewind-state-at-turn source cum-turn))
      (let ((path (car entry))
            (snapshot (cdr entry)))
        (unless (and (stringp path) (file-name-absolute-p path))
          (error "Invalid captured Worktree Fork path: %S" path))
        (if (not
             (string-prefix-p source-root (expand-file-name path)))
            (push (expand-file-name path) external)
          (let* ((target
                  (niwa-session-fork-retarget-worktree-path
                   child path))
                 (backup-name (plist-get snapshot :backup-name)))
            (niwa-session-fork--assert-worktree-target
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
                   (niwa-session-artifacts-read-backup
                    source backup-name))
                  (niwa-session-persistence-write-current-buffer-atomically
                   target)))
              (cl-incf restored))
          (error
           (push (list :path target
                       :reason (error-message-string err))
                 unrestored)))))
    (list :restored restored
          :unrestored (nreverse unrestored)
          :external (nreverse external))))

(defun niwa-session-fork--worktree-restore-report-body
    (session report)
  "Return the one-shot restore REPORT body for worktree fork SESSION.
Delivered once through the session's pending reminder FIFO on the
fork's first request; the durable provenance facts are carried
separately by `niwa-session-fork-provenance-body'."
  (let* ((unrestored (plist-get report :unrestored))
         (dropped (plist-get report :dropped))
         (partial (or unrestored dropped)))
    (format
     (concat "%s\n"
             "Captured repository files restored: %d\n"
             "%s"
             "%s"
             "%s"
             "%s")
     (if partial "Worktree Fork (partial restoration)" "Worktree Fork")
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
              (niwa-session-fork--target-native-report-path
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
                     (niwa-session-fork--target-native-report-path
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

(defun niwa-session-fork--target-native-report-path (session path)
  "Return PATH without SESSION's client-specific remote prefix."
  (if-let* ((target (niwa-session-execution-target session))
            ((stringp path)))
      (condition-case _
          (niwa-execution-target-native-path target path)
        (niwa-execution-target-error "<path outside session target>"))
    path))

(defun niwa-session-fork--worktree-fork-retained-error
    (session failure reservation)
  "Return the retained-artifact error for SESSION, FAILURE, and RESERVATION."
  (format
   "%s; Worktree Fork artifacts retained: branch %s, directory %s. Cleanup: %s"
   (error-message-string failure)
   (plist-get reservation :branch)
   (niwa-session-fork--target-native-report-path
    session (plist-get reservation :directory))
   (plist-get reservation :cleanup-command)))

(defun niwa-session-fork-create (buffer target type)
  "Create and open a fork of BUFFER at stable TARGET of TYPE.
TYPE is `conversation' or `worktree'.  Both share admission, staging,
publication, and cleanup.  Only a worktree fork creates a checkout and
restores captured files; a conversation fork shares the source directory.
Return the child data buffer, stopping the source's transient services.
The source's durable state and lock remain unchanged."
  (niwa--with-gc-busy
    (unless (memq type '(conversation worktree))
      (error "Unknown session fork type: %S" type))
    (let* ((session (buffer-local-value 'niwa--session buffer))
           (_ (unless session
                (user-error "Active buffer has no niwa session")))
           (_ (niwa-session-artifacts-assert-mutation-authority
               session buffer))
           (target
            (niwa-session-rewind-resolve-fork-target session target))
           (_ (niwa-session-rewind-assert-stable-source
               session buffer "forking"))
           (parent-save-path (niwa-session-save-path session))
           (_ (unless parent-save-path
                (user-error "Save the session before forking")))
           (picked-segment (plist-get target :segment))
           (picked-cum-turn (plist-get target :cum-turn))
           (sessions-dir
            (niwa-session-artifacts-sessions-dir
             (niwa-session-workspace session)))
           (reservation
            (when (eq type 'worktree)
              (let ((reserved (or (plist-get target :worktree-reservation)
                                  (niwa-worktree-fork-reservation session))))
                (niwa-worktree-fork-validate-reservation session reserved)
                reserved)))
           (new-id
            (niwa-session-persistence-allocate-session-id sessions-dir))
           (new-save-path
            (file-name-as-directory (file-name-concat sessions-dir new-id)))
           (staging-path
            (file-name-as-directory
             (make-temp-file
              (expand-file-name (if reservation ".niwa-worktree-fork-"
                                  ".niwa-fork-")
                                sessions-dir) t)))
           (staging-buffer
            (generate-new-buffer (format " *niwa-%s-fork*" type)))
           (additional-roots
            (buffer-local-value 'niwa-workspace-additional-roots buffer))
           child report worktree-created)
      (condition-case err
          (unwind-protect
              (progn
                (when reservation
                  (niwa-worktree-fork-create reservation))
                (let ((now (format-time-string "%FT%H-%M-%S")))
                  (setq worktree-created (and reservation t)
                        child
                        (niwa-session-fork-clone-session
                         session 'fork
                         :save-path staging-path
                         :session-id new-id
                         :created-at now
                         :updated-at now
                         :current-segment picked-segment
                         :forked-from-session-id
                         (niwa-session-session-id session)
                         :forked-from-turn picked-cum-turn)))
                (setf (niwa-session-fork-type child) type
                      (niwa-session-forked-from-fork-point-id child)
                      (plist-get target :fork-point-id))
                (when reservation
                  (setf (niwa-session-worktree-source-root child)
                        (plist-get reservation :source-root)
                        (niwa-session-worktree-directory child)
                        (plist-get reservation :directory)
                        (niwa-session-worktree-branch child)
                        (plist-get reservation :branch)
                        (niwa-session-worktree-base-commit child)
                        (plist-get reservation :base-commit))
                  (let* ((dropped
                          (niwa-session-fork--retarget-worktree-state child))
                         (roots
                          (niwa-session-fork--retarget-worktree-roots
                           child additional-roots)))
                    (setq additional-roots (plist-get roots :roots)
                          report (niwa-session-fork--restore-worktree-files
                                  session child picked-cum-turn))
                    (setq report (plist-put report :dropped
                                            (nconc dropped (plist-get roots :dropped))))))
                (with-current-buffer staging-buffer
                  (let ((org-agenda-file-menu-enabled nil))
                    (org-mode)))
                (niwa-session-rewind-load-rewind-target
                 session staging-buffer target)
                (when reservation
                  ;; The FIFO is transient; durable provenance still survives
                  ;; a restart before the child's first request.
                  (niwa-session-enqueue-pending-reminder
                   child (niwa-session-fork--worktree-restore-report-body
                          child report)))
                (prog1
                    (niwa-session-fork--publish-fork
                     child buffer staging-buffer parent-save-path staging-path
                     new-save-path picked-segment picked-cum-turn additional-roots)
                  ;; Arming or a failed fork must not discard source definitions.
                  (niwa-execution-stop-services session)))
            (when (file-directory-p staging-path)
              (ignore-errors (delete-directory staging-path t)))
            (when (buffer-live-p staging-buffer)
              (with-current-buffer staging-buffer
                (set-buffer-modified-p nil)
                (setq-local kill-buffer-hook nil))
              (kill-buffer staging-buffer)))
        (error
         (if worktree-created
             (error "%s" (niwa-session-fork--worktree-fork-retained-error
                          session err reservation))
           (signal (car err) (cdr err))))))))

(provide 'niwa-session-fork)

;;; niwa-session-fork.el ends here
