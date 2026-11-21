;;; niwa-structs.el -- Core data structures -*- lexical-binding: t -*-

;;; Commentary:

;; Passive workspace, session, request, and task data shapes plus
;; shape-local invariants and constructors.  Workspace and request
;; lifecycle behavior belongs to their focused owners.

;;; Code:

;; `cl-lib' is a runtime dependency, not a compile-time one: these owners
;; call `cl-lib' functions -- `cl-remove', `cl-some', `cl-find' -- and
;; `cl-lib' autoloads none of them.  Loading this module compiled and
;; without the umbrella, as the split-owner contract allows, then hit a
;; void-function; loading it as source only worked because
;; `eval-when-compile' degrades to `progn' in interpreted code.
(require 'cl-lib)

;; `niwa-execution-target'
(declare-function niwa-execution-target-create
                  "niwa-execution-target" (workspace-root))
(declare-function niwa-execution-target-expand-path
                  "niwa-execution-target" (target path &optional directory))
(autoload 'niwa-execution-target-create "niwa-execution-target")
(autoload 'niwa-execution-target-expand-path "niwa-execution-target")

;; `niwa-sandbox'
(defvar niwa-sandbox-mode)

(defun niwa-agent-path-p (path)
  "Return non-nil when PATH is a canonical address in an agent tree."
  (and (stringp path)
       (let ((case-fold-search nil))
         (string-match-p
          "\\`/root\\(?:/[a-z0-9_-]+\\)*\\'" path))))


;;
;;; Customization

(defcustom niwa-user-dir (expand-file-name "~/.niwa/")
  "Global user state directory.

Stores user-wide skills, memory, plugins, global config, and local-target
global permissions.  The defaults of `niwa-skill-dirs' and
`niwa-memory-dirs' derive from it when niwa loads, so set it first.
Remote global permissions live in the target user's ~/.niwa/ instead.
Project-level state goes in PROJECT/.niwa/ instead.  Lookup functions
check project dir first, then global."
  :type 'directory
  :group 'niwa)


;;
;;; Workspace structs

(cl-defstruct (niwa-workspace (:constructor niwa-workspace--create))
  "Project-level shared state.
One workspace per project, shared by all sessions for that project."
  type              ; symbol: project, file, etc.
  id                ; opaque identifier
  root              ; cached absolute path
  name              ; display name
  file-cache        ; niwa-file-cache struct: LRU workspace file cache
  journal-observation ; disposable public journal observation for prompt discovery
  journal-cleanup-at ; client time of the last journal expiry opportunity
  memory-observation ; disposable proposal counts for cockpit display
  memory-schedule)  ; disposable automatic-review timing and setting snapshot


;;
;;; File state structs

(cl-defstruct (niwa-file-state (:constructor niwa-file-state--create))
  "Cached state of a tracked file within a workspace.

Stored inside a `niwa-file-cache' on the workspace; updated by
Read/ApplyPatch tool handlers and polled by external-change detection.

PATH is the absolute filesystem path.  CONTENT is the full file
content as captured at the last read or write, or nil for a
fingerprint-only entry: a file over
`niwa-file-cache-max-file-bytes' is tracked by its timestamps alone
so it can still be seen to move without holding the cache budget.
MTIME is the file's modification time at that same moment (as returned
by `file-attribute-modification-time').  SIZE is the byte length of
CONTENT -- zero when CONTENT is omitted -- cached to avoid repeated
`length' calls during LRU accounting.

CTIME and STAT-SIZE are the inode change time and the size the
filesystem reported at capture, kept for freshness comparison.  They
are deliberately separate from SIZE: for a symbolic link or a virtual
file the reported size and the content length are different
quantities, and comparing one against the other would report a change
that never happened.  A change time cannot be set from userland, so it
moves even when a tool restores the modification time."
  path
  content
  mtime
  size
  ctime
  stat-size)

(cl-defstruct (niwa-file-interaction
               (:constructor niwa-file-interaction--create))
  "Per-session record of interaction with a file.

Stored in the session `touched-files' hash-table, keyed by absolute
path.  READ-TURN is the session turn count when the file was last
read by a tool, or nil if never.  MODIFIED-TURN is the same for tool
modifications.  READ-OFFSET and READ-LIMIT capture the arguments of
the last read call so that repeated Read calls over the same range
can be deduplicated when the on-disk mtime is unchanged."
  read-turn
  modified-turn
  read-offset
  read-limit)

(cl-defstruct (niwa-file-cache (:constructor niwa-file-cache--create))
  "LRU-bounded workspace file cache.

TABLE is a hash-table mapping absolute path to `niwa-file-state'.
ORDER is a list of paths in most-recently-used order (head is MRU).
TOTAL-BYTES is the current sum of cached CONTENT sizes, maintained
incrementally on every put/remove so eviction does not need to walk
the table."
  table
  order
  total-bytes)


;;
;;; Session struct

(cl-defstruct (niwa-session (:constructor niwa-session--create))
  "Per-chat-buffer session state.

Each chat buffer has exactly one session. Multiple sessions can share a
workspace."
  name              ; readable display name, initially the session id
  (naming-state 'explicit) ; `pending', `attempted', or user-owned `explicit'
  naming-input      ; bounded authored prompt of an `attempted' automatic title, or nil
  workspace         ; niwa-workspace struct (shared by reference)
  (execution-target nil :read-only t) ; immutable filesystem/process authority
  authority-mode    ; `portable' for durable workspaces, nil for workspace-free sessions
  working-directory ; absolute directory used for relative tools/prompts
  tasks             ; list of niwa-task structs
  task-status-notes ; alist: owner -> plist with :note/:updated-turn/:updated-at
  last-task-write-turn ; integer or nil: turn of last task tool write
  touched-files     ; hash-table: filepath -> niwa-file-interaction
  permission-rules  ; session-scoped permission rules
  resource-grants   ; session-scoped exact or recursive path/access grants
  permission-mode   ; current permission mode
  sandbox-mode      ; current child-process confinement policy
  plan-mode         ; non-nil during a sticky Plan conversation
  preset-name       ; selected niwa preset symbol
  model-provider    ; exact "BACKEND:MODEL" session selector or nil
  external-conversations ; scope -> native ownership, installed history and turn state
  last-outcome      ; most recent terminal turn outcome for notifications
  recovery-issues   ; durable, credential-free operational issues
  reasoning-effort  ; explicit session effort symbol or nil for default
  turn-count        ; integer: for reminder throttling
  reminders         ; list of active niwa-reminder structs
  last-observed-date ; YYYY-MM-DD string last advertised to the model
  agent-types-snapshot ; alist or :uninitialized: last advertised agents
  pending-reminders ; transient FIFO of model-visible reminder bodies
  tool-catalog      ; alist: (CATEGORY NAME) -> SHORT-DESCRIPTION
  messages          ; list of inbound-message plists queued for next turn
  agent-registry    ; alist: canonical path -> retained `niwa-agent-record'
  agent-reservations ; transient alist of unpublished agent records
  (agent-root-activity 'idle) ; root roster activity: running or idle
  agent-root-waiter ; transient async WaitAgent callback and timer
  (agent-turn-capacity 10) ; maximum active non-root turns in this session tree
  pending-steering ; durable unsent steering; restored for explicit review
  pending-follow-ups ; durable FIFO of prompts awaiting separate root turns
  pending-input-next-id ; next session-local pending-input identity
  pending-input-paused ; non-nil while automatic delivery is user-paused
  pending-input-failure-paused ; non-nil after a failed turn leaves undelivered or uncertain input
  dropped-file-grants ; pending exact-file read grants from drag/drop
  active-dropped-file-grants ; session-scoped exact-file read grants
  mentions-shown    ; hash-table: (KIND . KEY) -> (turn . content-hash) for mention dedup
  workspace-instruction-hashes ; (OWNER PATH) -> SHA-256 hash; nil needs delivery
  skills            ; list of niwa-skill structs available to this session
  hook-rules         ; transient session-scoped declarative hook rules
  hook-log           ; transient per-session hook execution log
  hook-log-pending   ; transient hook diagnostics awaiting persistence
  repair-log         ; transient bounded tool-input repair telemetry
  repair-log-pending ; transient repair diagnostics awaiting persistence
  permission-log-pending ; transient diagnostics awaiting materialization
  telemetry-pending  ; queued telemetry entries, newest first
  hook-context-pending ; transient hook context injected into the next prompt
  execution-state   ; transient opaque state owned by `niwa-execution'
  audit-session     ; transient durable parent for audit and mutation authority
  pending-publication ; transient local recovery record blocking mutation
  publication       ; transient captured immutable publication plist
  publication-queue ; transient FIFO of durability publication batches
  publication-uncommitted-batches ; transient staged session-local writes
  checkpoint-state ; transient committed projection owned by `niwa-session-checkpoint'
  publication-active-p ; non-nil while this session publishes target state
  publication-job ; transient acknowledged local publication owner
  publication-preparations ; transient FIFO of mutation preparation continuations
  publication-preparation-token ; transient explicit owner of the running preparation
  publication-finishing-p ; transient notification/dispatch exclusion during job finalization
  publication-stopping-p ; teardown rejects new jobs and suppresses their wakes
  mail-batch ; transient reversed background arrivals awaiting one gated commit
  mail-notifications ; transient FIFO of coalesced post-publication notifications
  mail-notification-timer ; transient single gate-idle notification retry timer
  mail-notification-delay ; transient bounded retry backoff in seconds
  control-transfer ; transient cooperative lease-transfer state; never persisted
  control-transfer-drains ; transient predicates that block lease handoff
  root-buffer       ; transient authoritative chat data buffer, or nil
  lease             ; transient portable lease record and live ownership state
  (ownership-epoch 0) ; transient local authority epoch, not a lease generation
  lease-renewal-timer ; transient timer renewing remote mutation authority
  lease-renewal-job ; transient single-flight asynchronous heartbeat owner
  ;; Persistence -- nil until lazy materialization.
  save-path         ; absolute path to the session directory under .niwa/sessions/
  session-id        ; string: stable session identifier (matches save-path basename)
  created-at        ; ISO timestamp string of session creation
  updated-at        ; ISO timestamp string of last save
  current-segment   ; integer: highest-numbered segment file (1, 2, ...)
  forked-from-session-id ; string or nil: parent session's id if this is a fork
  forked-from-turn  ; integer or nil: parent's turn number at fork point
  fork-type         ; conversation, worktree, or nil
  forked-from-fork-point-id ; string or nil: stable parent response identity
  worktree-source-root ; original Git worktree root for path retargeting
  worktree-directory ; originally allocated linked-worktree directory
  worktree-branch   ; generated branch name for a Worktree Fork
  worktree-base-commit ; full HEAD commit used to create a Worktree Fork
  prompt-index      ; alist: (segment-number . list of prompt plists) for picker
  chapters          ; closed /clear chats, oldest first: (:segment LAST :title STR-OR-NIL :closed-at ISO)
  file-snapshots    ; alist: (turn-number . file-map alist) for file-history restore
  untracked-effects ; alist: (turn-number . ((tool . reason) ...)) outside file-snapshots
  ptc-checkpoints   ; durable unfinished/settled ToolCall envelopes; never guest state
  ;; Non-nil once this process has ensured the session directory tree and the
  ;; workspace ignore entries.  Re-ensuring them on every save costs target
  ;; round trips for directories that were created by an earlier save in the
  ;; same process.
  durable-tree-ensured
  ;; Sub-agent transcript index.  Alist of
  ;; (AGENT-ID . PLIST) where PLIST has :agent-type, :description,
  ;; :path (relative to save-path), :status (running | completed |
  ;; error | aborted | incomplete), :created-at, :updated-at,
  ;; :parent-turn.  Authoritative in-memory copy; the sidecar
  ;; `:agent-transcripts' value mirrors this slot.
  agent-transcripts
  ;; List of `niwa-skill-invocation-record' structs recording every skill
  ;; invoked during this session.  Used by compaction/replay so the
  ;; expanded body survives even when SKILL.md is rewritten on disk.
  ;; Session-lifetime; see also dynamic let-bound depth counter
  ;; `niwa-skills--invoke-depth' for recursion bookkeeping.
  invoked-skills
  ;; heterogeneous FIFO permission queue.  Entries are
  ;; plists with :kind (`generic' / `bash' / `eval'), :request-id,
  ;; :origin (the canonical requesting agent path), :callback (continuation
  ;; receiving the queue's outcome vocabulary), and kind-specific
  ;; fields.  Transient runtime state -- never persisted to the
  ;; sidecar; empty at every completed-turn boundary because
  ;; pending tool calls are not recoverable.
  permission-queue
  ;; Transient absence-timeout state for `permission-queue': nil, a plist
  ;; (:timer TIMER :deadline TIME) while an unanswered-queue deadline is
  ;; armed, or (:expired-at TIME :count N) while the session holds for a
  ;; user message after that deadline expired.  Never persisted.
  permission-absence
  ;; The single transient Plan approval interaction descriptor, or nil.
  pending-plan-approval
  ;; Plan artifact metadata.  Goal plan paths are recorded here.
  plan-metadata
  ;; The session-owned current `niwa-goal', or nil.
  goal
  ;; Ids of the workspace store artifacts this session is attached to.
  attached-artifacts
  ;; Id of the artifact whose own conversation this session is, or nil.  The
  ;; artifact's metadata names the session back; only when both agree is the
  ;; session hidden from lists, kept from expiry and deleted with it.
  dedicated-artifact
  note-conversation ; non-nil for an isolated note session, independent of preset
  ;; Durable wake-up plists owned by `niwa-schedule', oldest first.
  schedules)

(defun niwa-session-authority-mode-for-workspace (workspace)
  "Return the durable authority mode required by WORKSPACE.

Every workspace type uses the portable lease.  Without a workspace, return
nil rather than claiming a persistence protocol."
  (when workspace
    'portable))

(defun niwa-session-authority-mode-for-session (session)
  "Return and normalize SESSION's explicit durable authority mode.

A session with a durable workspace derives its missing mode once and stores
it on the struct.  Workspace-free sessions may keep a nil mode and do not
materialize durable state.  An obsolete or unknown explicit mode is rejected."
  (let* ((workspace (niwa-session-workspace session))
         (mode (niwa-session-authority-mode session))
         (expected (and workspace
                        (niwa-session-authority-mode-for-workspace
                         workspace))))
    (when (eq mode 'pid-lock)
      (error "Pre-lease session requires migration with scripts/migrate-sessions.el"))
    (unless (memq mode '(nil portable))
      (error "Invalid session authority mode: %S" mode))
    (unless mode
      (setq mode expected))
    (unless (eq mode (niwa-session-authority-mode session))
      (setf (niwa-session-authority-mode session) mode))
    mode))

(defun niwa-session-set-control-transfer-drains (session predicates)
  "Set SESSION's transient control-transfer drain PREDICATES."
  (setf (niwa-session-control-transfer-drains session) predicates))

(defun niwa-session-set-control-transfer (session transfer)
  "Set SESSION's transient cooperative control-transfer state."
  (setf (niwa-session-control-transfer session) transfer))

(defun niwa-session-set-root-buffer (session buffer)
  "Set SESSION's authoritative live root data BUFFER, or nil."
  (setf (niwa-session-root-buffer session)
        (and (buffer-live-p buffer) buffer)))

(defun niwa-session-set-execution-target (session target)
  "Set SESSION's execution TARGET during committed-state adoption."
  (setf (cl-struct-slot-value 'niwa-session 'execution-target session)
        target))

(defun niwa-session-adopt-committed-state
    (session workspace save-path lease ownership-epoch lease-renewal-timer publication
             control-transfer control-transfer-drains root-buffer)
  "Adopt committed durable state into SESSION after a control transfer.

The coordinator supplies the already validated committed values.  Keep this
slot-level update here so a transfer cannot leave runtime state from the old
owner alongside the newly committed session state."
  (setf (niwa-session-workspace session) workspace
        (niwa-session-save-path session) save-path
        (niwa-session-lease session) lease
        (niwa-session-ownership-epoch session) ownership-epoch
        (niwa-session-lease-renewal-timer session) lease-renewal-timer
        (niwa-session-publication session) publication
        (niwa-session-control-transfer session) control-transfer
        (niwa-session-control-transfer-drains session)
        control-transfer-drains)
  (niwa-session-set-root-buffer session root-buffer))


;;
;;; Goal struct

(cl-defstruct (niwa-goal (:constructor niwa-goal--create)
                            (:copier nil))
  "Session-owned durable objective driving automatic continuation."
  id                 ; versioned string identity
  objective          ; non-empty free-form string
  status             ; active, paused, blocked, budget-limited, or complete
  reason             ; non-empty string for paused/blocked/budget-limited;
                     ; optional note for complete
  reason-diagnostic  ; non-nil when reason is private host diagnostic text
  implementation-policy ; exact model/effort/permission snapshot or nil
  token-budget       ; positive integer or nil
  tokens-used        ; non-negative integer; lower bound when incomplete
  tokens-incomplete-p ; missing native usage in an accounted request
  time-used-seconds  ; non-negative integer
  turns-run          ; non-negative integer
  plan-reference     ; normalized relative accepted-plan path or nil
  created-at         ; ISO timestamp
  updated-at)        ; ISO timestamp


;;
;;; Task struct

(cl-defstruct (niwa-task (:constructor niwa-task--create))
  "A single task in a session's task list."
  id                ; integer: unique per session
  subject           ; string: short one-line summary
  description       ; string or nil: detailed notes
  status            ; symbol: pending, in-progress, completed
  owner             ; string or nil: canonical agent path or user bucket
  blocked-by        ; list of task IDs blocking this task
  completed-turn    ; integer or nil: turn when status changed to completed
  metadata)         ; plist or nil: free-form extra data

(defun niwa-task-normalize-owner (owner agent-registry)
  "Normalize OWNER to a canonical path, bucket string, or nil.
Canonical non-root paths must name a retained agent in AGENT-REGISTRY."
  (cond
   ((or (null owner) (equal owner "") (equal owner "/root")) nil)
   ((not (stringp owner))
    (error "Task owner must be a string: %S" owner))
   (t
    (let ((case-fold-search nil))
      (when (string-match-p
             "\\`[^[:space:]]+--[[:xdigit:]]\\{32\\}\\'" owner)
        (error "Opaque agent IDs cannot own tasks: %s" owner)))
    (when (and (string-prefix-p "/" owner)
               (not (niwa-agent-path-p owner)))
      (error "Invalid canonical task owner: %s" owner))
    (when (and (niwa-agent-path-p owner)
               (not (assoc owner agent-registry)))
      (error "Unknown canonical task owner: %s" owner))
    owner)))

(defun niwa-task-prune-resolved-dependencies (tasks)
  "Remove resolved dependency edges from TASKS.
An edge is resolved when its target is absent from TASKS or already
completed; either way nothing will ever clear it later, so a task
carrying one would stay blocked for the rest of the session.  Return
TASKS after updating `blocked-by' in place."
  (let ((open (cl-loop for task in tasks
                       unless (eq (niwa-task-status task) 'completed)
                       collect (niwa-task-id task))))
    (dolist (task tasks)
      (setf (niwa-task-blocked-by task)
            (cl-loop for id in (niwa-task-blocked-by task)
                     when (memq id open)
                     collect id))))
  tasks)


;;
;;; Dual-buffer cross-references

(defvar-local niwa--data-buffer nil
  "The gptel data buffer for this view buffer.
Set on view buffers and derived buffers (diff preview etc.) to point
back to the data buffer where `niwa--session' and gptel state live.")

(put 'niwa--data-buffer 'permanent-local t)

(defvar-local niwa--view-buffer nil
  "The view buffer for this data buffer.
Set on data buffers to point to the user-facing view buffer.  Nil when
no view buffer exists yet.")

(put 'niwa--view-buffer 'permanent-local t)

(defvar-local niwa-view--agent-transcript-p nil
  "Non-nil when this view buffer renders an agent transcript for inspection.
Root view, interaction and persistence code read it whether or not
`niwa-view-agent' has loaded.")


;;
;;; Session buffer-local

(defvar-local niwa--workspace nil
  "Cached `niwa-workspace' struct for this buffer.
Chat setup uses it before session creation.  Session buffers subsequently
resolve workspace identity through their shared session; non-session buffers
such as patch previews retain the workspace directly.")

(defvar-local niwa--session nil
  "The `niwa-session' struct for this chat buffer.
Set when a session is created, never cleared during buffer lifetime.")

;; Survive major-mode changes (e.g., during buffer setup)
(put 'niwa--session 'permanent-local t)


;;
;;; Session helpers

(defun niwa-session-audit-target (session)
  "Return SESSION's durable parent target, defaulting to SESSION.

Transient conversations may set `niwa-session-audit-session' to a
durable parent.  Sanitized audit events and shared mutation authority use this
target.  Runtime state and unsanitized subsystem logs continue to belong to
SESSION."
  (or (and session (niwa-session-audit-session session)) session))

(defun niwa-session-tool-results-directory (session)
  "Return SESSION's persisted tool-results directory, or nil.
The directory is not created here; callers that write into it own that."
  (when-let* ((save-path (and session (niwa-session-save-path session))))
    (file-name-concat save-path "tool-results")))

(defun niwa-session-buffer-name (session-name workspace)
  "Return the buffer name for SESSION-NAME in WORKSPACE.
Format: *niwa:SESSION@WORKSPACE*"
  (format "*niwa:%s@%s*" session-name (niwa-workspace-name workspace)))

(defun niwa-session-generate-id ()
  "Return a name-independent timestamp and 12-hex-character session id."
  (format "%s-%s" (format-time-string "%FT%H-%M")
          (substring (secure-hash
                      'sha256
                      (format "%s-%s-%s" (random most-positive-fixnum)
                              (float-time) (emacs-pid)))
                     0 12)))

(defun niwa-session-create (name workspace &optional working-directory session-id)
  "Create a new session named NAME for WORKSPACE.

Returns the session struct.  Does not create the buffer -- the caller is
responsible for buffer setup.  WORKING-DIRECTORY defaults to the
workspace root and is kept stable for the lifetime of the session.
SESSION-ID supplies an already allocated identity; nil generates one."
  (let* ((id (or session-id (niwa-session-generate-id)))
         (target
          (niwa-execution-target-create
           (niwa-workspace-root workspace)))
         (directory
          (file-name-as-directory
           (niwa-execution-target-expand-path
            target
            (or working-directory
                (niwa-workspace-root workspace))))))
    (niwa-session--create
     :name (or name id)
     :naming-state (if name 'explicit 'pending)
     :session-id id
     :workspace workspace
     :execution-target target
     :authority-mode
     (niwa-session-authority-mode-for-workspace workspace)
     :working-directory directory
     :touched-files (make-hash-table :test #'equal)
     :mentions-shown (make-hash-table :test #'equal)
     :permission-mode
     (if (boundp 'niwa-permission-mode)
         (default-toplevel-value 'niwa-permission-mode)
       'ask)
     :sandbox-mode
     (if (boundp 'niwa-sandbox-mode)
         (default-toplevel-value 'niwa-sandbox-mode)
       'best-effort)
     :last-observed-date (format-time-string "%F")
     :agent-types-snapshot :uninitialized
     :turn-count 0)))

(defun niwa-session-pending-inputs (session category)
  "Return SESSION pending inputs in CATEGORY."
  (pcase category
    ('steering (niwa-session-pending-steering session))
    ('follow-up (niwa-session-pending-follow-ups session))
    (_ (error "Unknown pending input category: %S" category))))

(defun niwa-session-pending-input-p (session)
  "Return non-nil when SESSION has pending steering or follow-ups."
  (or (niwa-session-pending-steering session)
      (niwa-session-pending-follow-ups session)))

(defun niwa-session-set-pending-inputs (session category entries)
  "Replace SESSION pending input CATEGORY with ENTRIES."
  (pcase category
    ('steering (setf (niwa-session-pending-steering session) entries))
    ('follow-up (setf (niwa-session-pending-follow-ups session) entries))
    (_ (error "Unknown pending input category: %S" category))))

(defun niwa-session-enqueue-pending-input (session category entry)
  "Append ENTRY to SESSION pending input CATEGORY.
Return the copied entry with a stable session-local identity and category."
  (let* ((entries (niwa-session-pending-inputs session category))
         (next-id (1+ (or (niwa-session-pending-input-next-id session) 0)))
         (entry (copy-sequence entry)))
    (setf (niwa-session-pending-input-next-id session) next-id)
    (setq entry (plist-put entry :id next-id)
          entry (plist-put entry :category category))
    (niwa-session-set-pending-inputs
     session category
    (append entries (list entry)))
    entry))

(defun niwa-session-set-pending-input-paused (session paused)
  "Set whether SESSION automatic pending-input delivery is PAUSED."
  (setf (niwa-session-pending-input-paused session) (and paused t)))

(defun niwa-session-set-pending-input-failure-paused (session paused)
  "Set whether SESSION pending input awaits explicit failure recovery."
  (setf (niwa-session-pending-input-failure-paused session)
        (and paused t)))

(defun niwa-session-pending-input-delivery-paused-p (session)
  "Return non-nil when SESSION automatic pending-input delivery is paused."
  (or (niwa-session-pending-input-paused session)
      (niwa-session-pending-input-failure-paused session)))

(defun niwa-session-set-hook-context-pending (session entries)
  "Set SESSION's pending hook context ENTRIES."
  (setf (niwa-session-hook-context-pending session) entries))

(defun niwa-session-set-model-provider (session provider)
  "Set SESSION's serialized model PROVIDER."
  (setf (niwa-session-model-provider session) provider))

(defun niwa-session-set-reasoning-effort (session effort)
  "Set SESSION's reasoning EFFORT."
  (setf (niwa-session-reasoning-effort session) effort))

(defun niwa-session--set-agent-registry (session registry)
  "Store retained agent REGISTRY on SESSION."
  (setf (niwa-session-agent-registry session) registry))

(defun niwa-session--set-agent-reservations (session reservations)
  "Store transient agent RESERVATIONS on SESSION."
  (setf (niwa-session-agent-reservations session) reservations))

(defun niwa-session--set-agent-root-waiter (session waiter)
  "Store transient root WaitAgent WAITER on SESSION."
  (setf (niwa-session-agent-root-waiter session) waiter))

(defun niwa-session--set-messages (session messages)
  "Store unread root mailbox MESSAGES on SESSION."
  (setf (niwa-session-messages session) messages))

(defun niwa-session--set-execution-state (session state)
  "Store opaque execution-module STATE on SESSION."
  (setf (niwa-session-execution-state session) state))

(defun niwa-session--set-sandbox-mode (session mode)
  "Store child confinement MODE on SESSION."
  (setf (niwa-session-sandbox-mode session) mode))

(defun niwa-session--set-dropped-file-grants (session paths)
  "Replace SESSION's pending dropped-file grants with PATHS."
  (setf (niwa-session-dropped-file-grants session) paths))

(defun niwa-session--set-active-dropped-file-grants (session paths)
  "Replace SESSION's active dropped-file grants with PATHS."
  (setf (niwa-session-active-dropped-file-grants session) paths))

(defun niwa-session--normalize-dropped-file-path (path)
  "Return PATH as an expanded file name, or nil when invalid."
  (when (and (stringp path) (not (string-empty-p path)))
    (expand-file-name path)))

(defun niwa-session-add-dropped-file-grant (session path)
  "Add PATH as a pending exact-file drag/drop grant for SESSION.
Return the expanded path recorded, or nil when PATH is invalid."
  (when-let* ((expanded (niwa-session--normalize-dropped-file-path path)))
    (cl-pushnew expanded
                (niwa-session-dropped-file-grants session)
                :test #'equal)
    expanded))

(defun niwa-session-pop-dropped-file-grants (session paths)
  "Consume pending drag/drop grants in SESSION that exactly match PATHS.
Return the consumed expanded paths.  Non-matching pending grants remain
pending until the composer is cleared."
  (let ((wanted (delq nil
                      (mapcar #'niwa-session--normalize-dropped-file-path
                              paths)))
        consumed
        remaining)
    (dolist (path (niwa-session-dropped-file-grants session))
      (if (member path wanted)
          (push path consumed)
        (push path remaining)))
    (setf (niwa-session-dropped-file-grants session)
          (nreverse remaining))
    (nreverse consumed)))

(defun niwa-session-clear-dropped-file-grants (session)
  "Clear pending drag/drop grants for SESSION."
  (setf (niwa-session-dropped-file-grants session) nil))

(defun niwa-session-activate-dropped-file-grants (session paths)
  "Activate exact-file drag/drop grants PATHS for SESSION.

Each activated path is recorded as an in-memory session-scoped exact-file
`Read' grant.  The grant is not persisted and does not broaden to the
containing directory.

Return the expanded paths activated."
  (let (activated)
    (dolist (path paths)
      (when-let* ((expanded (niwa-session--normalize-dropped-file-path
                             path)))
        (cl-pushnew expanded
                    (niwa-session-active-dropped-file-grants session)
                    :test #'equal)
        (push expanded activated)))
    (nreverse activated)))

(defun niwa-session-enqueue-pending-reminder (session body)
  "Append reminder BODY to SESSION's pending reminder FIFO."
  (when (and session (stringp body) (not (equal body "")))
    (setf (niwa-session-pending-reminders session)
          (append (niwa-session-pending-reminders session)
                  (list body)))))


;;
;;; Request struct

(defvar-local niwa--current-request nil
  "The `niwa-request' struct for the active request.
Set at request start and cleared by the request lifecycle owner.")

(defvar-local niwa--agent-invocation nil
  "Invocation that owns this retained agent conversation buffer.")

(defvar-local niwa--conversation-working-directory nil
  "Durable agent directory projected into its conversation buffer.
Nil preserves the session's ordinary working-directory policy.")

(defvar-local niwa--conversation-instruction-root nil
  "Instruction boundary for an agent in an external linked checkout.
This affects instruction discovery only, never permission roots.")

(defun niwa-conversation-working-directory (&optional session buffer)
  "Return the execution directory for SESSION in BUFFER.
An agent's projected directory overrides the shared session directory, not
its workspace identity or resource authority.  BUFFER defaults to current."
  (with-current-buffer (or buffer (current-buffer))
    (let* ((override (and (or (null session) (eq session niwa--session))
                          niwa--conversation-working-directory))
           (session (or session niwa--session))
           (workspace (or (and session (niwa-session-workspace session))
                          niwa--workspace)))
      (file-name-as-directory
       (expand-file-name
        (or override
            (and session (niwa-session-working-directory session))
            (and workspace (niwa-workspace-root workspace))
            default-directory))))))

(defvar-local niwa--pending-implementation-policy nil
  "Accepted implementation policy waiting for request admission.")

(defvar-local niwa--pending-permission-mode nil
  "Mode-only permission override waiting for request admission.")

(put 'niwa--pending-implementation-policy 'permanent-local t)
(put 'niwa--pending-permission-mode 'permanent-local t)

(cl-defstruct (niwa-request (:constructor niwa-request--create))
  "Per-request state, scoped to a single LLM request/response cycle.
Created at request start, cleared in the termination handler."
  id                ; process-unique request correlation id
  session           ; back-reference to niwa-session
  turn              ; reserved session turn committed at settlement
  fsm               ; owning root gptel FSM, or nil for non-provider requests
  context           ; request-owned workflow context for external engines
  model-policy      ; frozen effective backend/model/effort, independent of selection
  implementation-policy ; accepted full policy captured at admission, or nil
  permission-mode-override ; scoped canonical authority, or nil for live baseline
  workspace-instruction-hashes ; isolated request's acknowledged path instructions
  file-snapshots    ; hash-table: filepath -> original content at request start
  untracked-effects ; alist: source -> reason capture cannot be complete
  plan-read-only    ; immutable Plan capability boundary for this request
  one-shot-mutations-p ; non-read-only tools require one-time approval
  ephemeral-p       ; suppress request-owned durable conversation artifacts
  cancellers        ; list of zero-arg thunks; each drains a primitive's pending overlays with 'aborted
  cancelled-p       ; terminal teardown has begun; later registrations cancel immediately
  started-at        ; wall-clock time when the request began
  active-work-pause-started-at ; wall-clock time when the current pause began
  (active-work-pause-duration 0) ; accumulated completed pause seconds
  origin            ; canonical requesting agent path
  ;; Exact read-only accepted-plan authority derived for an active Goal turn.
  goal-plan-read-path
  ;; Rules accumulated by an owning skill die with the request struct.
  skill-permission-rules
  ;; Closed nested-tool restriction accumulated from owning command skills.
  (ptc-primitives :unrestricted)
  ;; User-attached `niwa-skill-invocation-record' structs.
  attached-skill-records
  hook-rules
  ;; (WORKSPACE . ID) of the store artifacts this request edited; each gets
  ;; one version when the request settles.
  edited-artifacts)

(defun niwa-request-set-active-work-paused (request paused &optional now)
  "Set whether REQUEST active-work timing is PAUSED at NOW."
  (let ((now (or now (current-time))))
    (if paused
        (unless (niwa-request-active-work-pause-started-at request)
          (setf (niwa-request-active-work-pause-started-at request) now))
      (when-let* ((started
                   (niwa-request-active-work-pause-started-at request)))
        (cl-incf (niwa-request-active-work-pause-duration request)
                 (float-time (time-subtract now started)))
        (setf (niwa-request-active-work-pause-started-at request) nil))))
  request)

(defun niwa-request-active-elapsed-seconds (request &optional now)
  "Return REQUEST active-work seconds at NOW, excluding pauses."
  (when-let* ((started-at (niwa-request-started-at request)))
    (let ((now (or now (current-time))))
      (max
       0
       (- (float-time (time-subtract now started-at))
          (or (niwa-request-active-work-pause-duration request) 0)
          (if-let* ((pause-started
                     (niwa-request-active-work-pause-started-at request)))
              (float-time (time-subtract now pause-started))
            0))))))


;;
;;; Skill invocation record

(cl-defstruct (niwa-skill-invocation-record
               (:constructor niwa-skill-invocation-record--create))
  "Record of a single skill invocation, kept on the session for
compaction/replay.

NAME is the skill identifier.  ARGS is the raw argument string
passed to the skill.  ROLE is `command' or `instruction'.  ORIGIN is
`user', `model', or `internal'.  AGENT-PATH is the canonical conversation
that invoked it.  TURN is the session turn-count when the skill was invoked.
SOURCE-PATH is the absolute path of the SKILL.md that was loaded.
REQUIRED-BY-SOURCE-PATH is the requiring parent's source path for an attached
dependency, or nil for an invoked root.  DEPENDENCY-DEPTH is zero for a root
and increases for each required attachment edge.
PREPARED-BODY is the post-substitution, post-shell-expansion body string --
stored verbatim so compaction can summarize the skill's actual contribution
to the conversation without re-reading SKILL.md (which may have changed)."
  name
  args
  role
  origin
  agent-path
  turn
  source-path
  required-by-source-path
  dependency-depth
  prepared-body)


(provide 'niwa-structs)
;;; niwa-structs.el ends here
