;;; mevedel-structs.el -- Core data structures -*- lexical-binding: t -*-

;;; Commentary:

;; Workspace, session, request, and task structs that form the foundation for
;; mevedel's state management, plus canonical task data invariants.  All other
;; modules reference these definitions.

;;; Code:

(eval-when-compile (require 'cl-lib))

;; `mevedel-agents'
(declare-function mevedel-agent-invocation-p "mevedel-agents" (cl-x))
(declare-function mevedel-agent-invocation-require-path
                  "mevedel-agents" (invocation))
(defvar mevedel--agent-invocation)

;; `mevedel-permission-queue'
(declare-function mevedel-permission-queue-sweep-origin
                  "mevedel-permission-queue"
                  (origin &optional session no-render))

;; `mevedel-plan-mode'
(declare-function mevedel-plan-approval-abort
                  "mevedel-plan-mode" (&optional session outcome))

;; `mevedel-telemetry'
(declare-function mevedel-telemetry-record
                  "mevedel-telemetry" (session event &rest props))

(defun mevedel-agent-path-p (path)
  "Return non-nil when PATH is a canonical address in an agent tree."
  (and (stringp path)
       (let ((case-fold-search nil))
         (string-match-p
          "\\`/root\\(?:/[a-z0-9_]+\\)*\\'" path))))


;;
;;; Customization

(defcustom mevedel-user-dir (expand-file-name "~/.mevedel/")
  "Global user state directory.

Stores user-wide skills, global config, and global permission rules.
Project-level state goes in PROJECT/.mevedel/ instead.  Lookup functions
check project dir first, then global."
  :type 'directory
  :group 'mevedel)


;;
;;; Workspace struct

(cl-defstruct (mevedel-workspace (:constructor mevedel-workspace--create))
  "Project-level shared state.
One workspace per project, shared by all sessions for that project."
  type              ; symbol: project, file, etc.
  id                ; opaque identifier
  root              ; cached absolute path
  name              ; display name
  file-cache)       ; mevedel-file-cache struct: LRU workspace file cache


;;
;;; File state structs

(cl-defstruct (mevedel-file-state (:constructor mevedel-file-state--create))
  "Cached state of a tracked file within a workspace.

Stored inside a `mevedel-file-cache' on the workspace; updated by
Read/Edit/Write tool handlers and polled by external-change detection.

PATH is the absolute filesystem path.  CONTENT is the full file
content as captured at the last read or write.  MTIME is the file's
modification time at that same moment (as returned by
`file-attribute-modification-time').  SIZE is the byte length of
CONTENT, cached to avoid repeated `length' calls during LRU
accounting."
  path
  content
  mtime
  size)

(cl-defstruct (mevedel-file-interaction
               (:constructor mevedel-file-interaction--create))
  "Per-session record of interaction with a file.

Stored in the session `touched-files' hash-table, keyed by absolute
path.  READ-TURN is the session turn count when the file was last
read by a tool, or nil if never.  MODIFIED-TURN is the same for tool
modifications.  READ-OFFSET and READ-LIMIT capture the arguments of
the last read call so that repeated Read calls over the same range
can be deduplicated when the on-disk mtime is unchanged."
  path
  read-turn
  modified-turn
  read-offset
  read-limit)

(cl-defstruct (mevedel-file-cache (:constructor mevedel-file-cache--create))
  "LRU-bounded workspace file cache.

TABLE is a hash-table mapping absolute path to `mevedel-file-state'.
ORDER is a list of paths in most-recently-used order (head is MRU).
TOTAL-BYTES is the current sum of cached CONTENT sizes, maintained
incrementally on every put/remove so eviction does not need to walk
the table."
  table
  order
  total-bytes)


;;
;;; Workspace registry

(defvar mevedel-workspace--registry (make-hash-table :test #'equal)
  "Global registry of workspace structs.

Keyed by (TYPE . ID) cons cells.  Workspaces are created lazily on first
chat buffer creation and cached here.")

(defun mevedel-workspace--normalize-root (root)
  "Return ROOT expanded for workspace filesystem paths."
  (if (stringp root)
      (expand-file-name root)
    root))

(defun mevedel-workspace-get-or-create (type id root name)
  "Return the workspace for TYPE and ID, creating it if needed.

ROOT is the absolute project root path.  NAME is the display name.  If a
workspace already exists for this TYPE and ID, return it (ignoring ROOT
and NAME arguments)."
  (let* ((id (if (and (eq type 'project)
                      (stringp id)
                      (file-name-absolute-p id))
                 (mevedel-workspace--normalize-root id)
               id))
         (root (mevedel-workspace--normalize-root root))
         (key (cons type id)))
    (or (gethash key mevedel-workspace--registry)
        (puthash key
                 (mevedel-workspace--create
                  :type type
                  :id id
                  :root root
                  :name name
                  :file-cache (mevedel-file-cache--create
                               :table (make-hash-table :test #'equal)
                               :order nil
                               :total-bytes 0))
                 mevedel-workspace--registry))))

(defun mevedel-workspace-clear-registry ()
  "Remove all workspaces from the global registry.

Intended for testing and cleanup."
  (clrhash mevedel-workspace--registry))


;;
;;; Workspace helpers

(defun mevedel-workspace-state-dir (workspace)
  "Return the .mevedel/ directory for WORKSPACE."
  (file-name-concat
   (mevedel-workspace--normalize-root (mevedel-workspace-root workspace))
   ".mevedel/"))

(defun mevedel-workspace-find-state-file (workspace filename)
  "Find FILENAME in WORKSPACE's state dir, falling back to global.

Returns the first existing path, or the project path if neither exists."
  (let ((project-path (file-name-concat
                       (mevedel-workspace-state-dir workspace) filename))
        (global-path (file-name-concat mevedel-user-dir filename)))
    (cond
     ((file-exists-p project-path) project-path)
     ((file-exists-p global-path) global-path)
     (t project-path))))


;;
;;; Session struct

(cl-defstruct (mevedel-session (:constructor mevedel-session--create))
  "Per-chat-buffer session state.

Each chat buffer has exactly one session. Multiple sessions can share a
workspace."
  name              ; string: "main", "refactor", "tutor", etc.
  workspace         ; mevedel-workspace struct (shared by reference)
  working-directory ; absolute directory used for relative tools/prompts
  tasks             ; list of mevedel-task structs
  task-status-notes ; alist: owner -> plist with :note/:updated-turn/:updated-at
  last-task-write-turn ; integer or nil: turn of last task tool write
  touched-files     ; hash-table: filepath -> mevedel-file-interaction
  permission-rules  ; session-scoped permission rules
  resource-grants   ; session-scoped exact path/access permission plists
  permission-mode   ; current permission mode
  plan-mode         ; non-nil during a sticky Plan conversation
  preset-name       ; selected mevedel preset symbol
  preset-settings   ; alist of resolved buffer-local mevedel variables
  turn-count        ; integer: for reminder throttling
  reminders         ; list of active mevedel-reminder structs
  last-observed-date ; YYYY-MM-DD string last advertised to the model
  agent-types-snapshot ; alist or :uninitialized: last advertised agents
  skills-snapshot   ; alist or :uninitialized: last advertised skill roster
  pending-reminders ; transient FIFO of model-visible reminder bodies
  specialist-nudge-state ; transient plist: nudge family -> (:count N :turn T)
  deferred-set      ; alist: (CATEGORY NAME) -> SHORT-DESCRIPTION
  deferred-pending  ; list of gptel-tool structs queued for injection
  deferred-injected ; alist: tool-name -> TTL counter
  deferred-used     ; list of tool-name strings used during current turn
  deferred-expired  ; list of tool-name strings expired on last turn
  messages          ; list of inbound-message plists queued for next turn
  agent-registry    ; alist: canonical path -> retained `mevedel-agent-record'
  agent-reservations ; transient alist of unpublished agent records
  (agent-root-activity 'idle) ; root roster activity: running or idle
  agent-root-waiter ; transient async WaitAgent callback and timer
  (agent-turn-capacity 3) ; maximum active non-root turns in this session tree
  queued-user-messages ; transient FIFO of bound or prepared prompts awaiting dispatch
  dropped-file-grants ; pending exact-file read grants from drag/drop
  active-dropped-file-grants ; session-scoped exact-file read grants
  mentions-shown    ; hash-table: (KIND . KEY) -> (turn . content-hash) for mention dedup
  skills            ; list of mevedel-skill structs available to this session
  hook-rules         ; transient session-scoped declarative hook rules
  hook-log           ; transient per-session hook execution log
  repair-log         ; transient bounded tool-input repair telemetry
  permission-log-pending ; transient diagnostics awaiting materialization
  telemetry-pending  ; transient lifecycle telemetry awaiting materialization
  hook-context-pending ; transient hook context injected into the next prompt
  execution-state   ; transient opaque state owned by `mevedel-execution'
  ;; Persistence -- nil until lazy materialization.
  save-path         ; absolute path to the session directory under .mevedel/sessions/
  session-id        ; string: stable session identifier (matches save-path basename)
  created-at        ; ISO timestamp string of session creation
  updated-at        ; ISO timestamp string of last save
  current-segment   ; integer: highest-numbered segment file (1, 2, ...)
  forked-from-session-id ; string or nil: parent session's id if this is a fork
  forked-from-turn  ; integer or nil: parent's turn number at fork point
  prompt-index      ; alist: (segment-number . list of prompt plists) for picker
  file-snapshots    ; alist: (turn-number . file-map alist) for file-history restore
  ;; Sub-agent transcript index.  Alist of
  ;; (AGENT-ID . PLIST) where PLIST has :agent-type, :description,
  ;; :path (relative to save-path), :status (running | completed |
  ;; error | aborted | incomplete), :created-at, :updated-at,
  ;; :parent-turn.  Authoritative in-memory copy; the sidecar
  ;; `:agent-transcripts' value mirrors this slot.
  agent-transcripts
  ;; List of `mevedel-skill-invocation-record' structs recording every skill
  ;; invoked during this session.  Used by compaction/replay so the
  ;; expanded body survives even when SKILL.md is rewritten on disk.
  ;; Session-lifetime; see also dynamic let-bound depth counter
  ;; `mevedel-skills--invoke-depth' for recursion bookkeeping.
  invoked-skills
  ;; heterogeneous FIFO permission queue.  Entries are
  ;; plists with :kind (`generic' / `bash' / `eval'), :origin (the
  ;; canonical requesting agent path), :callback (continuation
  ;; receiving the queue's outcome vocabulary), and kind-specific
  ;; fields.  Transient runtime state -- never persisted to the
  ;; sidecar; empty at every completed-turn boundary because
  ;; pending tool calls are not recoverable.
  permission-queue
  ;; The single transient Plan approval interaction descriptor, or nil.
  pending-plan-approval
  ;; Plan artifact metadata.  Goal plan paths are recorded here.
  plan-metadata
  ;; The session-owned current `mevedel-goal', or nil.
  goal)


;;
;;; Goal struct

(cl-defstruct (mevedel-goal (:constructor mevedel-goal--create)
                            (:copier nil))
  "Session-owned durable objective driving automatic continuation."
  id                 ; versioned string identity
  objective          ; non-empty free-form string
  status             ; active, paused, blocked, budget-limited, or complete
  reason             ; non-empty string for paused/blocked/budget-limited
  token-budget       ; positive integer or nil
  tokens-used        ; non-negative integer
  time-used-seconds  ; non-negative integer
  turns-run          ; non-negative integer
  plan-reference     ; normalized relative accepted-plan path or nil
  created-at         ; ISO timestamp
  updated-at)        ; ISO timestamp


;;
;;; Task struct

(cl-defstruct (mevedel-task (:constructor mevedel-task--create))
  "A single task in a session's task list."
  id                ; integer: unique per session
  subject           ; string: short one-line summary
  description       ; string or nil: detailed notes
  status            ; symbol: pending, in-progress, completed
  owner             ; string or nil: canonical agent path or user bucket
  blocks            ; list of task IDs this task blocks
  blocked-by        ; list of task IDs blocking this task
  completed-turn    ; integer or nil: turn when status changed to completed
  metadata)         ; plist or nil: free-form extra data

(defun mevedel-task-normalize-owner (owner agent-registry)
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
               (not (mevedel-agent-path-p owner)))
      (error "Invalid canonical task owner: %s" owner))
    (when (and (mevedel-agent-path-p owner)
               (not (assoc owner agent-registry)))
      (error "Unknown canonical task owner: %s" owner))
    owner)))

(defun mevedel-task-prune-dangling-dependencies (tasks)
  "Remove dependency edges from TASKS to task IDs absent from TASKS.
Return TASKS after updating both `blocks' and `blocked-by' in place."
  (let ((ids (mapcar #'mevedel-task-id tasks)))
    (dolist (task tasks)
      (setf (mevedel-task-blocks task)
            (cl-loop for id in (mevedel-task-blocks task)
                     when (memq id ids)
                     collect id)
            (mevedel-task-blocked-by task)
            (cl-loop for id in (mevedel-task-blocked-by task)
                     when (memq id ids)
                     collect id))))
  tasks)


;;
;;; Dual-buffer cross-references

(defvar-local mevedel--data-buffer nil
  "The gptel data buffer for this view buffer.
Set on view buffers and derived buffers (diff preview etc.) to point
back to the data buffer where `mevedel--session' and gptel state live.")

(put 'mevedel--data-buffer 'permanent-local t)

(defvar-local mevedel--view-buffer nil
  "The view buffer for this data buffer.
Set on data buffers to point to the user-facing view buffer.  Nil when
no view buffer exists yet.")

(put 'mevedel--view-buffer 'permanent-local t)


;;
;;; Session buffer-local

(defvar-local mevedel--session nil
  "The `mevedel-session' struct for this chat buffer.
Set when a session is created, never cleared during buffer lifetime.")

;; Survive major-mode changes (e.g., during buffer setup)
(put 'mevedel--session 'permanent-local t)


;;
;;; Session helpers

(defun mevedel-session-buffer-name (session-name workspace)
  "Return the buffer name for SESSION-NAME in WORKSPACE.
Format: *mevedel:SESSION@WORKSPACE*"
  (format "*mevedel:%s@%s*" session-name (mevedel-workspace-name workspace)))

(defun mevedel-session-create (name workspace &optional working-directory)
  "Create a new session named NAME for WORKSPACE.

Returns the session struct.  Does not create the buffer -- the caller is
responsible for buffer setup.  WORKING-DIRECTORY defaults to the
workspace root and is kept stable for the lifetime of the session."
  (mevedel-session--create
   :name name
   :workspace workspace
   :working-directory (file-name-as-directory
                       (expand-file-name
                        (or working-directory
                            (mevedel-workspace-root workspace))))
   :touched-files (make-hash-table :test #'equal)
   :mentions-shown (make-hash-table :test #'equal)
   :last-observed-date (format-time-string "%F")
   :agent-types-snapshot :uninitialized
   :skills-snapshot :uninitialized
   :turn-count 0))

(defun mevedel-session-set-queued-user-messages (session queue)
  "Set SESSION's transient queued user message QUEUE."
  (setf (mevedel-session-queued-user-messages session) queue))

(defun mevedel-session-set-hook-context-pending (session entries)
  "Set SESSION's pending hook context ENTRIES."
  (setf (mevedel-session-hook-context-pending session) entries))

(defun mevedel-session--set-agent-registry (session registry)
  "Store retained agent REGISTRY on SESSION."
  (setf (mevedel-session-agent-registry session) registry))

(defun mevedel-session--set-agent-reservations (session reservations)
  "Store transient agent RESERVATIONS on SESSION."
  (setf (mevedel-session-agent-reservations session) reservations))

(defun mevedel-session--set-agent-root-waiter (session waiter)
  "Store transient root WaitAgent WAITER on SESSION."
  (setf (mevedel-session-agent-root-waiter session) waiter))

(defun mevedel-session--set-messages (session messages)
  "Store unread root mailbox MESSAGES on SESSION."
  (setf (mevedel-session-messages session) messages))

(defun mevedel-session--set-execution-state (session state)
  "Store opaque execution-module STATE on SESSION."
  (setf (mevedel-session-execution-state session) state))

(defun mevedel-session--normalize-dropped-file-path (path)
  "Return PATH as an expanded file name, or nil when invalid."
  (when (and (stringp path) (not (string-empty-p path)))
    (expand-file-name path)))

(defun mevedel-session-add-dropped-file-grant (session path)
  "Add PATH as a pending exact-file drag/drop grant for SESSION.
Return the expanded path recorded, or nil when PATH is invalid."
  (when-let* ((expanded (mevedel-session--normalize-dropped-file-path path)))
    (cl-pushnew expanded
                (mevedel-session-dropped-file-grants session)
                :test #'equal)
    expanded))

(defun mevedel-session-pop-dropped-file-grants (session paths)
  "Consume pending drag/drop grants in SESSION that exactly match PATHS.
Return the consumed expanded paths.  Non-matching pending grants remain
pending until the composer is cleared."
  (let ((wanted (delq nil
                      (mapcar #'mevedel-session--normalize-dropped-file-path
                              paths)))
        consumed
        remaining)
    (dolist (path (mevedel-session-dropped-file-grants session))
      (if (member path wanted)
          (push path consumed)
        (push path remaining)))
    (setf (mevedel-session-dropped-file-grants session)
          (nreverse remaining))
    (nreverse consumed)))

(defun mevedel-session-clear-dropped-file-grants (session)
  "Clear pending drag/drop grants for SESSION."
  (setf (mevedel-session-dropped-file-grants session) nil))

(defun mevedel-session-activate-dropped-file-grants (session paths)
  "Activate exact-file drag/drop grants PATHS for SESSION.

Each activated path is recorded as an in-memory session-scoped exact-file
`Read' grant.  The grant is not persisted and does not broaden to the
containing directory.

Return the expanded paths activated."
  (let (activated)
    (dolist (path paths)
      (when-let* ((expanded (mevedel-session--normalize-dropped-file-path
                             path)))
        (cl-pushnew expanded
                    (mevedel-session-active-dropped-file-grants session)
                    :test #'equal)
        (push expanded activated)))
    (nreverse activated)))

(defun mevedel-session-enqueue-pending-reminder (session body)
  "Append reminder BODY to SESSION's pending reminder FIFO."
  (when (and session (stringp body) (not (equal body "")))
    (setf (mevedel-session-pending-reminders session)
          (append (mevedel-session-pending-reminders session)
                  (list body)))))


;;
;;; Request struct

(cl-defstruct (mevedel-request (:constructor mevedel-request--create))
  "Per-request state, scoped to a single LLM request/response cycle.
Created at request start, cleared in the termination handler."
  id                ; process-unique request correlation id
  session           ; back-reference to mevedel-session
  file-snapshots    ; hash-table: filepath -> original content at request start
  directive-uuid    ; UUID of directive being processed, if any
  pending-plan      ; pending plan action plist
  cancellers        ; list of zero-arg thunks; each drains a primitive's pending overlays with 'aborted
  started-at        ; wall-clock time when the request began
  origin            ; canonical requesting agent path
  ;; Exact read-only accepted-plan authority derived for an active Goal turn.
  goal-plan-read-path
  ;; Rules accumulated by an owning skill die with the request struct.
  skill-permission-rules
  ;; User-attached `mevedel-skill-invocation-record' structs.
  attached-skill-records
  hook-rules)


;;
;;; Skill invocation record

(cl-defstruct (mevedel-skill-invocation-record
               (:constructor mevedel-skill-invocation-record--create))
  "Record of a single skill invocation, kept on the session for
compaction/replay.

NAME is the skill identifier.  ARGS is the raw argument string
passed to the skill.  ROLE is `command' or `instruction'.  ORIGIN is
`user', `model', or `internal'.  TURN is the session turn-count when
the skill was invoked.  SOURCE-PATH is the absolute path of the SKILL.md that was
loaded.  PREPARED-BODY is the post-substitution, post-shell-expansion
body string -- stored verbatim so compaction can summarize the
skill's actual contribution to the conversation without re-reading
SKILL.md (which may have changed)."
  name
  args
  role
  origin
  turn
  source-path
  prepared-body)


;;
;;; Request buffer-local

(defvar-local mevedel--current-request nil
  "The `mevedel-request' struct for the active request.
Set at request start, cleared in the termination handler.  Tool functions
access this through the buffer-local.")

(defun mevedel-request-active-p (&optional buffer)
  "Return non-nil when BUFFER has an active request."
  (let ((buffer (or buffer (current-buffer))))
    (and (buffer-live-p buffer)
         (with-current-buffer buffer
           (bound-and-true-p mevedel--current-request)))))

(defun mevedel-request-state-label (&optional buffer)
  "Return BUFFER's compact request state label."
  (if (mevedel-request-active-p buffer) "running" "idle"))


;;
;;; Request cancellers

(defun mevedel-request-push-canceller (request canceller)
  "Append CANCELLER (a zero-arg thunk) onto REQUEST's `cancellers' list.

Each canceller is invoked exactly once during teardown via the
drain-then-invoke helper.  Primitives that own pending overlays
register a thunk that drains their own overlays with the
`aborted' sentinel."
  (when request
    (setf (mevedel-request-cancellers request)
          (append (mevedel-request-cancellers request)
                  (list canceller)))))

(defun mevedel-request-drain-cancellers (request)
  "Atomically clear and invoke every canceller on REQUEST.

Drains the list before invoking, so a canceller that registers a new
canceller during its run does not re-enter the current drain.  Each
canceller runs inside `ignore-errors' so a misbehaving thunk cannot
strand the others.

Used by `mevedel-abort', `mevedel-request-end', and the stale-request
replacement path in `mevedel-request-begin'.  Together these are the
only call sites that may invoke cancellers."
  (when request
    (let ((cancellers (mevedel-request-cancellers request)))
      (setf (mevedel-request-cancellers request) nil)
      (dolist (canceller cancellers)
        (ignore-errors (funcall canceller))))))


;;
;;; Request lifecycle

(defun mevedel-current-origin ()
  "Return the canonical owner for the current execution context."
  (or (and (bound-and-true-p mevedel--current-request)
           (mevedel-request-p mevedel--current-request)
           (mevedel-request-origin mevedel--current-request))
      (and-let* ((inv (bound-and-true-p mevedel--agent-invocation))
                 ((fboundp 'mevedel-agent-invocation-p))
                 ((mevedel-agent-invocation-p inv)))
        (mevedel-agent-invocation-require-path inv))
      "/root"))

(defun mevedel-request-begin (session &optional directive-uuid)
  "Create a new request for SESSION, guarding against stale requests.

If `mevedel--current-request' is already set, log a warning and replace
it.  Optional DIRECTIVE-UUID sets the directive being processed.  Returns
the new request struct."
  (when mevedel--current-request
    (message "mevedel: stale request found, replacing")
    (mevedel-request-end t))
  (let* ((origin (mevedel-current-origin))
         (id (format "request-%s-%s"
                     (format-time-string "%Y%m%dT%H%M%S")
                     (substring
                      (secure-hash
                       'sha1
                       (format "%s:%s:%s" (emacs-pid) (float-time) origin))
                      0 12)))
         (request (mevedel-request--create
                   :id id
                   :session session
                   :file-snapshots (make-hash-table :test #'equal)
                   :directive-uuid directive-uuid
                   :started-at (current-time)
                   :origin origin)))
    (setq mevedel--current-request request)
    (when (equal origin "/root")
      (setf (mevedel-session-agent-root-activity session) 'running))
    (when (fboundp 'mevedel-telemetry-record)
      (mevedel-telemetry-record
       session 'request-queued :request-id id :origin origin
       :permission-mode (mevedel-session-permission-mode session))
      (mevedel-telemetry-record
       session 'request-start :request-id id :origin origin
       :permission-mode (mevedel-session-permission-mode session)))
    request))

(defun mevedel-request-cancel (request &optional abort-plan-approval)
  "Cancel REQUEST and its owned pending interactions.
Queued permission prompts are swept only for REQUEST's owner.  Plan
approvals normally outlive the request that presented them; when
ABORT-PLAN-APPROVAL is non-nil, abort it too."
  (when request
    (let ((session (mevedel-request-session request))
          (origin (or (mevedel-request-origin request) "/root")))
      (mevedel-request-drain-cancellers request)
      (when (fboundp 'mevedel-permission-queue-sweep-origin)
        (mevedel-permission-queue-sweep-origin origin session))
      (when (and abort-plan-approval
                 (fboundp 'mevedel-plan-approval-abort))
        (mevedel-plan-approval-abort session)))))

(defun mevedel-request-end (&optional abort-plan-approval)
  "Cancel the current request, then clear `mevedel--current-request'."
  (when mevedel--current-request
    (let ((request mevedel--current-request))
      (when (fboundp 'mevedel-telemetry-record)
        (mevedel-telemetry-record
         (mevedel-request-session request) 'request-teardown
         :request-id (mevedel-request-id request)
         :origin (mevedel-request-origin request)
         :abort-plan-approval (and abort-plan-approval t)))
      (mevedel-request-cancel request abort-plan-approval)
      (when (equal (mevedel-request-origin request) "/root")
        (setf (mevedel-session-agent-root-activity
               (mevedel-request-session request))
              'idle)))
    (setq mevedel--current-request nil)))

(provide 'mevedel-structs)
;;; mevedel-structs.el ends here
