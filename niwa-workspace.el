;;; niwa-workspace.el -- Project workspaces -*- lexical-binding: t -*-

;;; Commentary:

;; Workspace detection and registry.  A workspace is a
;; `niwa-workspace' struct carrying a root directory, a state
;; directory (`.niwa/' under root), and optional additional roots
;; for cross-project access.  The main entry point
;; `niwa-workspace' resolves the active workspace by checking
;; session > cached buffer-local > project.el detection, then a durable
;; workspace root or the current directory outside any project.
;;
;; Keeps a workspace registry so that distinct buffers under the
;; same project share a single workspace struct (and therefore a
;; single state directory and additional-roots list).
;;
;; Registering a workspace (`niwa-workspace-get-or-create' creating a
;; struct) observes its instruction store: a state-directory check and at
;; most one store read, or one target-side hash, skipped for a remote root
;; without a live connection.  It is bounded I/O, but I/O: avoid creating
;; workspaces on hot paths such as redisplay, timers or process filters.

;;; Code:

(require 'niwa-structs)

;; `cl-extra'
(declare-function cl-some "cl-extra" (cl-pred cl-seq &rest cl-rest))

;; `niwa-execution-target'
(declare-function niwa-execution-target-create
                  "niwa-execution-target" (workspace-root))
(declare-function niwa-execution-target-environment
                  "niwa-execution-target" (cl-x) t)
(declare-function niwa-execution-target-expand-path
                  "niwa-execution-target" (target path &optional directory))
(autoload 'niwa-execution-target-create "niwa-execution-target")
(autoload 'niwa-execution-target-environment "niwa-execution-target")
(autoload 'niwa-execution-target-expand-path "niwa-execution-target")

;; `niwa-persistence'
(declare-function niwa-instruction-store-activate
                  "niwa-persistence" (workspace))
(autoload 'niwa-instruction-store-activate "niwa-persistence")

;; `niwa-structs'
(declare-function niwa-file-cache--create "niwa-structs" (&rest slots))
(declare-function niwa-session-execution-target "niwa-structs" (cl-x) t)
(declare-function niwa-session-workspace "niwa-structs" (cl-x) t)
(declare-function niwa-workspace--create "niwa-structs" (&rest slots))
(declare-function niwa-workspace-root "niwa-structs" (cl-x) t)
(defvar niwa--data-buffer)
(defvar niwa--session)

;; `niwa-utilities'
(declare-function niwa-diagnostic-scope "niwa-utilities" (owner))
(declare-function niwa-diagnostic-user-error "niwa-utilities" (scope format-string &rest args))
(autoload 'niwa-diagnostic-scope "niwa-utilities")
(autoload 'niwa-diagnostic-user-error "niwa-utilities")

;; `niwa-system'
(defvar niwa-memory-dirs)

;; `project'
(declare-function project-current "project" (&optional maybe-prompt dir))
(declare-function project-name "project" (project))
(declare-function project-root "project" (project))

;; `tramp'
(defvar tramp-verbose)

(defcustom niwa-workspace-functions
  '(niwa-workspace--project-workspace
    niwa-workspace--directory-workspace)
  "Functions to determine the workspace for the current buffer.

Each function in this list is called with no arguments in the current
buffer until one returns a non-nil workspace cons cell of the form
\(TYPE . ID).

Functions should return nil if they cannot determine a workspace for the
current buffer, allowing other functions in the list to try.

Built-in workspace functions:
- `niwa-workspace--project-workspace' - Detects project.el workspaces
- `niwa-workspace--directory-workspace' - Uses an existing workspace root
  or the current directory
- `niwa-workspace--file-workspace' - Opt-in file-based workspace detector"
  :type 'hook
  :group 'niwa)

(defcustom niwa-workspace-types-alist
  '((project . (:get-root niwa-workspace--project-root
                :get-name niwa-workspace--project-name))
    (file . (:get-root file-name-directory
             :get-name file-name-nondirectory)))
  "Alist mapping workspace types to their defining functions.

Each entry is of the form (TYPE . PLIST) where TYPE is a symbol
representing the workspace type and PLIST contains the following keys:

- :get-root - Function to get the workspace root, i.e. the base
  directory for unified diffs and the \"root\" for relative filenames
  provided to tools.

- :get-name - Function to get a descriptive name for the workspace.

All functions receive the workspace ID as their only argument.

To add a new workspace type, add an entry to this alist and update
`niwa-workspace-functions' to detect it."
  :type '(alist :key-type symbol :value-type (plist :key-type keyword :value-type function))
  :group 'niwa)

(defcustom niwa-workspace-additional-roots nil
  "Alist mapping workspace roots to lists of additional allowed directories.

Format: ((WORKSPACE-ROOT . (DIR1 DIR2 ...)) ...)

For each workspace, the listed directories will be added to the allowed
roots for LLM access.  This allows granting access to directories outside
the primary workspace root on a per-workspace basis.

When set buffer-locally in a chat buffer, grants persist only for that
session.  When set globally, grants persist across all sessions."
  :type '(alist :key-type directory :value-type (repeat directory))
  :group 'niwa)


;;
;;; Workspace system variables

;; Ensure `niwa--workspace' is always buffer-local
(put 'niwa--workspace 'permanent-local t)


;;
;;; Workspace detection functions

(defvar niwa-workspace--remote-project-cache
  (make-hash-table :test #'equal)
  "Remote directory to its detected project, or nil when it has none.
Cleared with the workspace registry by `niwa-workspace-clear-registry'.")

(defun niwa-workspace--project-current (directory)
  "Return DIRECTORY's project, probing a remote directory once and quietly.

Project detection walks to the top of the tree, so on a remote target every
hop is a round trip and the last hop is the bare method prefix, which is not
a directory: TRAMP reports that miss to the user on every probe.  The probe
is best effort, so its own diagnostics are not the user's business, and its
answer is remembered per directory rather than re-walked.  A directory that
becomes a project later is picked up after
`niwa-workspace-clear-registry'."
  (if (not (file-remote-p directory))
      (project-current nil directory)
    ;; The key is the spelling as given: expanding a remote name asks the
    ;; target for its home directory, which opens a connection, and a cache
    ;; key must not perform I/O.  Distinct spellings only cost an entry.
    (let* ((key (file-name-as-directory directory))
           (cached (gethash key niwa-workspace--remote-project-cache
                            'niwa-workspace--absent)))
      (if (not (eq cached 'niwa-workspace--absent))
          cached
        (puthash key
                 (let ((tramp-verbose 0))
                   (project-current nil directory))
                 niwa-workspace--remote-project-cache)))))

(defun niwa-workspace--project-workspace ()
  "Detect project workspace for the current buffer.
Returns (project . ROOT) if the buffer is in a project, nil otherwise."
  (when-let* ((project (niwa-workspace--project-current default-directory)))
    (cons 'project (project-root project))))

(defun niwa-workspace--file-workspace ()
  "Detect file workspace for the current buffer.
Returns (file . FILENAME) if the buffer is visiting a file, nil otherwise."
  (when-let* ((filename (buffer-file-name)))
    (cons 'file filename)))

(defun niwa-workspace--directory-workspace ()
  "Detect a directory workspace independently of the current buffer's file.
Use the nearest durable workspace root, or `default-directory' if none exists."
  (cons 'project
        (file-name-as-directory
         (expand-file-name
          (or (locate-dominating-file default-directory ".niwa/workspace-id")
              default-directory)))))


;;
;;; Workspace type functions

(defun niwa-workspace--project-root (project-id)
  "Get the workspace root for PROJECT-ID, validating its directory."
  (unless (and (stringp project-id)
               (file-name-absolute-p project-id)
               (file-directory-p project-id))
    (error "Project ID '%s' is not a valid project root directory" project-id))
  project-id)

(defun niwa-workspace--project-name (project-id)
  "Get the project name for PROJECT-ID using project.el."
  (if-let* ((project (niwa-workspace--project-current project-id)))
      (project-name project)
    ;; Get the last directory name from the root (trailing slash removed).
    (file-name-nondirectory (directory-file-name project-id))))


;;
;;; Workspace registry

(defvar niwa-workspace--registry (make-hash-table :test #'equal)
  "Global registry of workspace structs.

Keyed by (TYPE . ID) cons cells.  Workspaces are created lazily on first
chat buffer creation and cached here.")

(defun niwa-workspace--normalize-root (root)
  "Return ROOT expanded for workspace filesystem paths."
  (if (stringp root)
      (expand-file-name root)
    root))

(defun niwa-workspace-get-or-create (type id root name)
  "Return the workspace for TYPE and ID, creating it if needed.

ROOT is the absolute project root path.  NAME is the display name.  If a
workspace already exists for this TYPE and ID, return it (ignoring ROOT
and NAME arguments)."
  (let* ((id (if (and (eq type 'project)
                      (stringp id)
                      (file-name-absolute-p id))
                 (niwa-workspace--normalize-root id)
               id))
         (root (niwa-workspace--normalize-root root))
         (key (cons type id)))
    (or (gethash key niwa-workspace--registry)
        (let ((workspace
               (puthash key
                        (niwa-workspace--create
                         :type type
                         :id id
                         :root root
                         :name name
                         :file-cache (niwa-file-cache--create
                                      :table (make-hash-table :test #'equal)
                                      :order nil
                                      :total-bytes 0)
                         )
                        niwa-workspace--registry)))
          ;; The workspace owns its instructions: make them available as
          ;; soon as it is activated, not only when a session resumes.
          (niwa-instruction-store-activate workspace)
          workspace))))

(defun niwa-workspace-clear-registry ()
  "Remove all workspaces from the global registry.

Also drops remembered remote project detections, which are the same decision
cached one level lower.  Intended for testing and cleanup."
  (clrhash niwa-workspace--registry)
  (when (boundp 'niwa-workspace--remote-project-cache)
    (clrhash niwa-workspace--remote-project-cache)))


;;
;;; Workspace helpers

(defun niwa-workspace-state-dir (workspace)
  "Return the .niwa/ directory for WORKSPACE."
  (file-name-concat
   (niwa-workspace--normalize-root (niwa-workspace-root workspace))
   ".niwa/"))


;;
;;; Workspace management


(defun niwa-workspace (&optional buffer)
  "Get the workspace for BUFFER as a `niwa-workspace' struct.

In chat buffers with an active session, returns the session's workspace.
When `niwa--workspace' is set (e.g., during buffer setup before the
session exists), returns that cached value.  Otherwise, auto-detects via
`niwa-workspace-functions' and returns a struct from the global
registry, creating one lazily if needed."
  (with-current-buffer (or buffer (current-buffer))
    (cond
     ;; Chat buffer with session: canonical path
     ((and (boundp 'niwa--session) niwa--session)
      (niwa-session-workspace niwa--session))
     ;; Cached workspace (during buffer setup, before session exists)
     ((and (boundp 'niwa--workspace) niwa--workspace)
      niwa--workspace)
     ;; Auto-detect from buffer context
     (t
      (when-let* ((detected (cl-some #'funcall niwa-workspace-functions)))
        (let* ((type (car detected))
               (id (cdr detected))
               (type-config (alist-get type niwa-workspace-types-alist))
               (root-fn (plist-get type-config :get-root))
               (name-fn (plist-get type-config :get-name))
               (root (when root-fn (funcall root-fn id)))
               (name (when name-fn (funcall name-fn id))))
          (niwa-workspace-get-or-create type id root name)))))))

(defun niwa-workspace-current ()
  "Return the current buffer's workspace for a command, or signal `user-error'."
  (or (niwa-workspace) (niwa-diagnostic-user-error (niwa-diagnostic-scope (current-buffer)) "No niwa workspace here")))


;;
;;; Generated state ignore

(defconst niwa-workspace--generated-state-excludes
  '("/.niwa/sessions/"
    "/.niwa/tool-results/"
    "/.niwa/input-history.el"
    "/.niwa/leases/"
    "/.niwa/state/")
  "Root-anchored generated state entries for `.git/info/exclude'.")

(defun niwa-workspace--git-exclude-file (root)
  "Return ROOT's `.git/info/exclude' path, or nil outside Git."
  (when-let* (((stringp root))
              ((file-directory-p root))
              (git-root (locate-dominating-file root ".git"))
              (dot-git (file-name-concat git-root ".git"))
              ((not (file-symlink-p dot-git))))
    (cond
     ((and (file-directory-p dot-git)
           (file-in-directory-p (file-truename dot-git)
                                (file-truename git-root)))
      (file-name-concat dot-git "info" "exclude"))
     ((file-regular-p dot-git)
      (with-temp-buffer
        (let ((default-directory root)
              (process-environment
               (unless (file-remote-p root) process-environment)))
          (unless (zerop (process-file "git" nil (list t t) nil
                                       "rev-parse" "--git-path"
                                       "info/exclude"))
            (error "Could not resolve Git exclude file"))
          (let ((path (string-trim (buffer-string))))
            (if (file-remote-p root)
                (progn
                  (niwa-execution-target-expand-path
                   (niwa-execution-target-create root) path root))
              (expand-file-name path root)))))))))

(defvar niwa-workspace--generated-state-ignored
  (make-hash-table :test #'equal)
  "Workspace roots whose generated-state exclusion has been settled.

Holds roots outside Git as well, because deciding that costs the same walk as
acting on it.")

(defun niwa-workspace-ensure-generated-state-ignored (workspace)
  "Add niwa generated-state paths to WORKSPACE's Git exclude file.

Only generated runtime artifacts are ignored.  The top-level
`.niwa/' directory is deliberately not ignored so durable project
state can still be tracked.

The answer is remembered per root for this Emacs process.  The exclude file
only ever gains entries, and it gains them once, but re-deriving that runs a
`locate-dominating-file' walk plus two `file-truename' resolutions -- on a
remote workspace that is a target round trip per path component, on every
save, to discover there is nothing to do.  A root whose exclude file is edited
by hand afterwards keeps the remembered answer until Emacs restarts, which is
the same bargain the session's `durable-tree-ensured' already makes for the
directory tree beside it."
  (let ((root (and workspace (niwa-workspace-root workspace))))
    (when (and root
               (not (gethash root niwa-workspace--generated-state-ignored)))
      (condition-case nil
          (progn
            (when-let* ((exclude-file
                         (niwa-workspace--git-exclude-file root)))
              (make-directory (file-name-directory exclude-file) t)
              (let ((changed nil))
                (with-temp-buffer
                  (when (file-exists-p exclude-file)
                    (insert-file-contents exclude-file))
                  (dolist (entry niwa-workspace--generated-state-excludes)
                    (goto-char (point-min))
                    (unless (re-search-forward
                             (concat "^" (regexp-quote entry) "$") nil t)
                      (goto-char (point-max))
                      (unless (or (bobp) (bolp))
                        (insert "\n"))
                      (insert entry "\n")
                      (setq changed t)))
                  (when changed
                    (write-region nil nil exclude-file nil 'silent)))))
            ;; Only a completed pass is remembered; a failure retries next save.
            (puthash root t niwa-workspace--generated-state-ignored))
        (error nil)))))


;;
;;; Project root management

(defun niwa--all-allowed-roots (&optional buffer)
  "Get all allowed roots for BUFFER's workspace.

Returns a list containing the workspace root, configured memory roots,
the execution target's temporary directory, and any additional roots via
`niwa-workspace-additional-roots'."
  (let* ((buffer (or buffer (current-buffer)))
         (workspace-root (niwa-workspace-root (niwa-workspace buffer)))
         (session
          (with-current-buffer buffer
            (or (and (boundp 'niwa--session) niwa--session)
                (and (boundp 'niwa--data-buffer)
                     (buffer-live-p niwa--data-buffer)
                     (buffer-local-value
                      'niwa--session niwa--data-buffer)))))
         (target
          (when (file-remote-p workspace-root)
            (or (and session (niwa-session-execution-target session))
                (niwa-execution-target-create workspace-root))))
         (temporary-root
          (if target
              (let ((target-tmpdir
                     (cdr (assoc "TMPDIR"
                                 (niwa-execution-target-environment
                                  target)))))
                (niwa-execution-target-expand-path
                 target
                 (if (and target-tmpdir (not (equal target-tmpdir "")))
                     target-tmpdir
                   "/tmp")
                 workspace-root))
            (with-current-buffer buffer temporary-file-directory)))
         (memory-dirs (if (boundp 'niwa-memory-dirs)
                          niwa-memory-dirs
                        '(".niwa/memory/" ".agents/memory/")))
         (roots (append
                 (list workspace-root temporary-root)
                 (mapcar
                  (lambda (dir)
                    (expand-file-name
                     dir
                     (unless (file-name-absolute-p dir)
                       workspace-root)))
                  memory-dirs)
                 ;; This alist is buffer-local to the session data buffer,
                 ;; so it has to be read in BUFFER for the same reason the
                 ;; temporary root above is.
                 (alist-get workspace-root
                            (buffer-local-value
                             'niwa-workspace-additional-roots buffer)
                            nil nil #'equal))))
    (delete-dups
     (delq nil
           (mapcar (lambda (root)
                     (file-name-as-directory (expand-file-name root)))
                   roots)))))

(defun niwa-workspace-file-in-allowed-roots-p (file &optional buffer)
  "Return the allowed root containing absolute FILE, or nil.
BUFFER specifies which workspace to check, defaulting to the current one.
The additional-roots alist is buffer-local to the session data buffer, so a
caller in a view buffer must name that buffer or it sees only the global
default."
  (let ((file (expand-file-name file)))
    (if (file-name-absolute-p file)
        (let ((roots (niwa--all-allowed-roots buffer)))
          (catch 'found
            (dolist (root roots)
              (when (file-in-directory-p file root)
                (throw 'found root))))))))

(defun niwa-workspace--session-data-buffer ()
  "Return the session data buffer reachable from `current-buffer'.
The data buffer carries the buffer-local `niwa-workspace-additional-roots'
copy installed at chat-buffer setup; commands that mutate that alist must
run there to avoid silently hitting the global default when invoked from
a view buffer.  Falls back to `current-buffer' when no session is in
scope so out-of-session calls still mutate the global default."
  (let ((cur (current-buffer)))
    (cond
     ((buffer-local-value 'niwa--session cur) cur)
     ((let ((db (buffer-local-value 'niwa--data-buffer cur)))
        (and db (buffer-live-p db) db)))
     (t cur))))

;;;###autoload
(defun niwa-add-project-root (directory)
  "Add DIRECTORY to the list of allowed roots for the current workspace.

This grants the LLM permission to read and write files in this
directory and its subdirectories for the current workspace only."
  (interactive "DAdd project root to current workspace: ")
  (unless (file-directory-p directory)
    (niwa-diagnostic-user-error (niwa-diagnostic-scope (current-buffer)) "%s is not a directory" directory))
  (with-current-buffer (niwa-workspace--session-data-buffer)
    (let* ((expanded (file-name-as-directory (expand-file-name directory)))
           ;; Try to get project root, otherwise default to directory
           (p-root (condition-case _
                       (project-root
                        (niwa-workspace--project-current expanded))
                     (error expanded)))
           (workspace-root (niwa-workspace-root (niwa-workspace)))
           (current-roots (alist-get workspace-root niwa-workspace-additional-roots nil nil #'equal)))
      (unless (member p-root current-roots)
        (setf (alist-get workspace-root niwa-workspace-additional-roots nil nil #'equal)
              (cons p-root current-roots))
        (message "Added project root to workspace %s: %s" workspace-root p-root)))))

;;;###autoload
(defun niwa-remove-project-root (directory)
  "Remove DIRECTORY from the list of allowed roots for the current workspace."
  (interactive
   (with-current-buffer (niwa-workspace--session-data-buffer)
     (let* ((workspace-root (niwa-workspace-root (niwa-workspace)))
            (current-roots (alist-get workspace-root niwa-workspace-additional-roots nil nil #'equal)))
       (list (if current-roots
                 (completing-read "Remove project root: " current-roots nil t)
               (niwa-diagnostic-user-error (niwa-diagnostic-scope (current-buffer)) "No additional project roots configured for this workspace"))))))
  (with-current-buffer (niwa-workspace--session-data-buffer)
    (let* ((workspace-root (niwa-workspace-root (niwa-workspace)))
           (current-roots (alist-get workspace-root niwa-workspace-additional-roots nil nil #'equal)))
      (setf (alist-get workspace-root niwa-workspace-additional-roots nil nil #'equal)
            (delete directory current-roots))
      (message "Removed project root from workspace: %s" directory))))

;;;###autoload
(defun niwa-list-project-roots ()
  "Display the list of allowed project roots for the current workspace."
  (interactive)
  (with-current-buffer (niwa-workspace--session-data-buffer)
    (let* ((workspace-root (niwa-workspace-root (niwa-workspace)))
           (additional-roots (alist-get workspace-root niwa-workspace-additional-roots nil nil #'equal)))
      (message "Workspace root: %s%s"
               workspace-root
               (if additional-roots
                   (format "\nAdditional roots: %s"
                           (mapconcat #'identity additional-roots ", "))
                 "")))))

(provide 'niwa-workspace)
;;; niwa-workspace.el ends here
