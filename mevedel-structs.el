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
  file-cache        ; hash-table: filepath -> mevedel-file-state
  hints)            ; list of hint plists (shared across all sessions)


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
        :file-cache (make-hash-table :test #'equal)))))

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
  turn-count        ; integer: for cue throttling
  cues              ; list of active mevedel-cue structs
  deferred-pending  ; list of gptel-tool structs queued for injection
  deferred-injected); alist: tool-name -> TTL counter


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

Returns the session struct. Does not create the buffer — the caller is
responsible for buffer setup."
  (mevedel-session--create
   :name name
   :workspace workspace
   :touched-files (make-hash-table :test #'equal)
   :turn-count 0))


;;
;;; Request struct

(cl-defstruct (mevedel-request (:constructor mevedel-request--create))
  "Per-request state, scoped to a single LLM request/response cycle.
Created at request start, cleared in the termination handler."
  session           ; back-reference to mevedel-session
  file-snapshots    ; hash-table: filepath -> original content at request start
  access-grants     ; list of directories granted during this request
  access-lock       ; non-nil when access request in progress
  directive-uuid    ; UUID of directive being processed, if any
  pending-plan      ; pending plan action plist
  prompt-overlay    ; overlay for user prompt request UI
  prompt-result     ; result of user prompt request
  cancel-fn)        ; function or nil: called by mevedel-abort for pipeline teardown


;;
;;; Request buffer-local

(defvar-local mevedel--current-request nil
  "The `mevedel-request' struct for the active request.
Set at request start, cleared in the termination handler. Tool functions
access this through the buffer-local.")


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

Calls `cancel-fn' if set, then clears `mevedel--current-request'."
  (when mevedel--current-request
    (when-let* ((cancel-fn (mevedel-request-cancel-fn mevedel--current-request)))
      (ignore-errors (funcall cancel-fn)))
    (setq mevedel--current-request nil)))

(provide 'mevedel-structs)
;;; mevedel-structs.el ends here
