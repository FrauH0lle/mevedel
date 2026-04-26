;;; mevedel-structs.el -- Core data structures -*- lexical-binding: t -*-

;;; Commentary:

;; Workspace, session, and request structs that form the foundation for
;; mevedel's state management. All other modules reference these structs.

;;; Code:

(eval-when-compile (require 'cl-lib))


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
  agents            ; alist: agent-id -> FSM
  tasks             ; list of mevedel-task structs
  task-overlay      ; previous task overlay reference
  touched-files     ; hash-table: filepath -> mevedel-file-interaction
  permission-rules  ; session-scoped permission rules
  permission-mode   ; current permission mode
  turn-count        ; integer: for reminder throttling
  reminders         ; list of active mevedel-reminder structs
  deferred-set      ; alist: (CATEGORY NAME) -> SHORT-DESCRIPTION
  deferred-pending  ; list of gptel-tool structs queued for injection
  deferred-injected ; alist: tool-name -> TTL counter
  deferred-used     ; list of tool-name strings used during current turn
  deferred-expired  ; list of tool-name strings expired on last turn
  messages          ; list of inbound-message plists queued for next turn
  background-agents ; list of agent-id strings for running background children
  mentions-shown    ; hash-table: (KIND . KEY) -> (turn . content-hash) for mention dedup
  skills            ; list of mevedel-skill structs available to this session
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
  agent-transcripts)


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

(defun mevedel-session-create (name workspace)
  "Create a new session named NAME for WORKSPACE.

Returns the session struct. Does not create the buffer -- the caller is
responsible for buffer setup."
  (mevedel-session--create
   :name name
   :workspace workspace
   :touched-files (make-hash-table :test #'equal)
   :mentions-shown (make-hash-table :test #'equal)
   :turn-count 0))


;;
;;; Request struct

(cl-defstruct (mevedel-request (:constructor mevedel-request--create))
  "Per-request state, scoped to a single LLM request/response cycle.
Created at request start, cleared in the termination handler."
  session           ; back-reference to mevedel-session
  file-snapshots    ; hash-table: filepath -> original content at request start
  directive-uuid    ; UUID of directive being processed, if any
  pending-plan      ; pending plan action plist
  cancellers)       ; list of zero-arg thunks; each drains a primitive's pending overlays with 'aborted


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

(defun mevedel-request-begin (session &optional directive-uuid)
  "Create a new request for SESSION, guarding against stale requests.

If `mevedel--current-request' is already set, log a warning and replace
it. Optional DIRECTIVE-UUID sets the directive being processed. Returns
the new request struct."
  (when mevedel--current-request
    (message "mevedel: stale request found, replacing")
    (mevedel-request-end))
  (let ((request (mevedel-request--create
                  :session session
                  :file-snapshots (make-hash-table :test #'equal)
                  :directive-uuid directive-uuid)))
    (setq mevedel--current-request request)
    request))

(defun mevedel-request-end ()
  "Clean up the current request.

Drains all registered cancellers, then clears
`mevedel--current-request'."
  (when mevedel--current-request
    (mevedel-request-drain-cancellers mevedel--current-request)
    (setq mevedel--current-request nil)))

(provide 'mevedel-structs)
;;; mevedel-structs.el ends here
