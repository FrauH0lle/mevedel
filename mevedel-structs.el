;;; mevedel-structs.el -- Core data structures -*- lexical-binding: t -*-

;;; Commentary:

;; Workspace, session, and request structs that form the foundation for
;; mevedel's state management. All other modules reference these structs.

;;; Code:

(eval-when-compile (require 'cl-lib))

(declare-function mevedel-permission-queue-sweep-origin
                  "mevedel-permission-queue"
                  (origin &optional session no-render))
(declare-function mevedel-plan-queue-abort-all
                  "mevedel-tool-plan" (&optional session))
(declare-function mevedel-agent-invocation-p "mevedel-agents" (cl-x))
(declare-function mevedel-agent-invocation-agent-id
                  "mevedel-agents" (cl-x) t)

(defvar mevedel--agent-invocation)


;;
;;; Customization

(defcustom mevedel-user-dir (expand-file-name "~/.mevedel/")
  "Global user state directory.

Stores user-wide skills, global config, and global permission rules.
Project-level state goes in PROJECT/.mevedel/ instead. Lookup functions
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
  additional-roots  ; list of extra allowed dirs (shared across sessions)
  file-cache        ; mevedel-file-cache struct: LRU workspace file cache
  hints)            ; list of hint plists (shared across all sessions)


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

Keyed by (TYPE . ID) cons cells. Workspaces are created lazily on first
chat buffer creation and cached here.")

(defun mevedel-workspace-get (type id)
  "Look up a workspace by TYPE and ID in the global registry.

Returns the workspace struct or nil if not found."
  (gethash (cons type id) mevedel-workspace--registry))

(defun mevedel-workspace-register (workspace)
  "Register WORKSPACE in the global registry.

Keyed by (TYPE . ID). Overwrites any existing entry for the same key."
  (puthash (cons (mevedel-workspace-type workspace)
                 (mevedel-workspace-id workspace))
           workspace
           mevedel-workspace--registry)
  workspace)

(defun mevedel-workspace-get-or-create (type id root name)
  "Return the workspace for TYPE and ID, creating it if needed.

ROOT is the absolute project root path. NAME is the display name. If a
workspace already exists for this TYPE and ID, return it (ignoring ROOT
and NAME arguments)."
  (or (mevedel-workspace-get type id)
      (mevedel-workspace-register
       (mevedel-workspace--create
        :type type
        :id id
        :root root
        :name name
        :file-cache (mevedel-file-cache--create
                     :table (make-hash-table :test #'equal)
                     :order nil
                     :total-bytes 0)))))

(defun mevedel-workspace-all ()
  "Return a list of all registered workspace structs."
  (let (workspaces)
    (maphash (lambda (_key ws) (push ws workspaces))
             mevedel-workspace--registry)
    workspaces))

(defun mevedel-workspace-clear-registry ()
  "Remove all workspaces from the global registry.

Intended for testing and cleanup."
  (clrhash mevedel-workspace--registry))


;;
;;; Workspace helpers

(defun mevedel-workspace-state-dir (workspace)
  "Return the .mevedel/ directory for WORKSPACE."
  (file-name-concat (mevedel-workspace-root workspace) ".mevedel/"))

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
  agents            ; alist: agent-id -> FSM
  tasks             ; list of mevedel-task structs
  task-status-notes ; alist: owner -> plist with :note/:updated-turn/:updated-at
  task-overlay      ; previous task overlay reference
  last-task-write-turn ; integer or nil: turn of last task tool write
  touched-files     ; hash-table: filepath -> mevedel-file-interaction
  permission-rules  ; session-scoped permission rules
  permission-mode   ; current permission mode
  turn-count        ; integer: for reminder throttling
  reminders         ; list of active mevedel-reminder structs
  last-observed-date ; YYYY-MM-DD string last advertised to the model
  agent-types-snapshot ; alist or :uninitialized: last advertised agents
  pending-reminders ; transient FIFO of model-visible reminder bodies
  specialist-nudge-state ; transient plist: nudge family -> (:count N :turn T)
  deferred-set      ; alist: (CATEGORY NAME) -> SHORT-DESCRIPTION
  deferred-pending  ; list of gptel-tool structs queued for injection
  deferred-injected ; alist: tool-name -> TTL counter
  deferred-used     ; list of tool-name strings used during current turn
  deferred-expired  ; list of tool-name strings expired on last turn
  messages          ; list of inbound-message plists queued for next turn
  queued-user-messages ; transient FIFO of prepared prompts queued during an active request
  dropped-file-grants ; pending exact-file read grants from drag/drop
  active-dropped-file-grants ; session-scoped exact-file read grants
  background-agents ; list of agent-id strings for running background children
  mentions-shown    ; hash-table: (KIND . KEY) -> (turn . content-hash) for mention dedup
  skills            ; list of mevedel-skill structs available to this session
  hook-rules         ; transient session-scoped declarative hook rules
  hook-log           ; transient per-session hook execution log
  hook-context-pending ; transient hook context injected into the next prompt
  ;; Persistence (spec 19) -- nil until lazy materialization
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
  ;; spec Request-Scoped Skill Context: list of
  ;; `mevedel-skill-invocation-record' structs recording every skill
  ;; invoked during this session.  Used by compaction/replay so the
  ;; expanded body survives even when SKILL.md is rewritten on disk.
  ;; Session-lifetime; see also dynamic let-bound depth counter
  ;; `mevedel-skills--invoke-depth' for recursion bookkeeping.
  invoked-skills
  ;; heterogeneous FIFO permission queue.  Entries are
  ;; plists with :kind (`generic' / `bash' / `eval'), :origin (the
  ;; canonical agent-id or "main"), :callback (continuation
  ;; receiving the queue's outcome vocabulary), and kind-specific
  ;; fields.  Transient runtime state -- never persisted to the
  ;; sidecar; empty at every completed-turn boundary because
  ;; pending tool calls are not recoverable.
  permission-queue
  ;; Plan approval FIFO queue.  Same FIFO machinery as
  ;; permission-queue but a separate slot since approval outcomes
  ;; (`implement' / `implement-clear' / `feedback' / `aborted')
  ;; differ from permission outcomes and never coalesce.
  ;; Transient.
  plan-queue
  ;; Plan-mode lifecycle metadata.  Plist persisted in the session
  ;; sidecar; plan text itself lives in the session-local plans/current.md.
  plan-metadata)


;;
;;; Task struct

(cl-defstruct (mevedel-task (:constructor mevedel-task--create))
  "A single task in a session's task list."
  id                ; integer: unique per session
  subject           ; string: short one-line summary
  description       ; string or nil: detailed notes
  status            ; symbol: pending, in-progress, completed
  owner             ; string or nil: agent name that owns this task
  blocks            ; list of task IDs this task blocks
  blocked-by        ; list of task IDs blocking this task
  completed-turn    ; integer or nil: turn when status changed to completed
  metadata)         ; plist or nil: free-form extra data


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

Returns the session struct. Does not create the buffer -- the caller is
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
   :turn-count 0))

(defun mevedel-session-set-queued-user-messages (session queue)
  "Set SESSION's transient queued user message QUEUE."
  (setf (mevedel-session-queued-user-messages session) queue))

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

(defun mevedel-session-active-dropped-file-grant-p (session path)
  "Return non-nil when SESSION has an active exact-file grant for PATH."
  (when-let* ((expanded (mevedel-session--normalize-dropped-file-path path)))
    (member expanded (mevedel-session-active-dropped-file-grants session))))

(defun mevedel-session-clear-active-dropped-file-grants (session)
  "Clear active drag/drop grants for SESSION."
  (setf (mevedel-session-active-dropped-file-grants session) nil))

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
  session           ; back-reference to mevedel-session
  file-snapshots    ; hash-table: filepath -> original content at request start
  directive-uuid    ; UUID of directive being processed, if any
  pending-plan      ; pending plan action plist
  cancellers        ; list of zero-arg thunks; each drains a primitive's pending overlays with 'aborted
  started-at        ; wall-clock time when the request began
  origin            ; "main" or canonical agent-id that owns this request
  ;; spec Request-Scoped Skill Context: rules accumulate across
  ;; nested skills (additive); model selector/effort are
  ;; last-writer-wins.  All three die with the request struct.
  skill-permission-rules
  skill-model-override
  skill-effort-override
  hook-rules)


;;
;;; Skill invocation record

(cl-defstruct (mevedel-skill-invocation-record
               (:constructor mevedel-skill-invocation-record--create))
  "Record of a single skill invocation, kept on the session for
compaction/replay.

NAME is the skill identifier.  ARGS is the raw argument string
passed to the skill.  TRIGGER is `user-slash', `model-skill', or
`internal'.  TURN is the session turn-count when the skill was
invoked.  SOURCE-PATH is the absolute path of the SKILL.md that was
loaded.  PREPARED-BODY is the post-substitution, post-shell-expansion
body string -- stored verbatim so compaction can summarize the
skill's actual contribution to the conversation without re-reading
SKILL.md (which may have changed)."
  name
  args
  trigger
  turn
  source-path
  prepared-body)


;;
;;; Request buffer-local

(defvar-local mevedel--current-request nil
  "The `mevedel-request' struct for the active request.
Set at request start, cleared in the termination handler. Tool functions
access this through the buffer-local.")


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

(defun mevedel-request--current-origin ()
  "Return the permission-queue owner for the current request."
  (or (and-let* ((inv (and (boundp 'mevedel--agent-invocation)
                           mevedel--agent-invocation))
                 ((fboundp 'mevedel-agent-invocation-p))
                 ((mevedel-agent-invocation-p inv)))
        (mevedel-agent-invocation-agent-id inv))
      "main"))

(defun mevedel-request-begin (session &optional directive-uuid)
  "Create a new request for SESSION, guarding against stale requests.

If `mevedel--current-request' is already set, log a warning and replace
it. Optional DIRECTIVE-UUID sets the directive being processed. Returns
the new request struct."
  (when mevedel--current-request
    (message "mevedel: stale request found, replacing")
    (mevedel-request-end t))
  (let ((request (mevedel-request--create
                  :session session
                  :file-snapshots (make-hash-table :test #'equal)
                  :directive-uuid directive-uuid
                  :started-at (current-time)
                  :origin (mevedel-request--current-origin))))
    (setq mevedel--current-request request)
    request))

(defun mevedel-request-end (&optional abort-plan-queue)
  "Clean up the current request.

Drains all registered cancellers, then clears
`mevedel--current-request'.  Queued permission prompts are swept only
for this request's owner, so a parent turn ending does not abort
background sub-agent prompts.  Plan approvals normally outlive the
request that presented them; when ABORT-PLAN-QUEUE is non-nil, abort
them too for stale-request replacement."
  (when mevedel--current-request
    (let ((session (mevedel-request-session mevedel--current-request))
          (origin (or (mevedel-request-origin mevedel--current-request)
                      "main")))
      (mevedel-request-drain-cancellers mevedel--current-request)
      (when (fboundp 'mevedel-permission-queue-sweep-origin)
        (mevedel-permission-queue-sweep-origin origin session))
      (when (and abort-plan-queue
                 (fboundp 'mevedel-plan-queue-abort-all))
        (mevedel-plan-queue-abort-all session)))
    (setq mevedel--current-request nil)))

(provide 'mevedel-structs)
;;; mevedel-structs.el ends here
