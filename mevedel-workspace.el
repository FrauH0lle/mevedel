;;; mevedel-workspace.el -- Project workspaces -*- lexical-binding: t -*-

;;; Commentary:

;; Workspace detection and registry.  A workspace is a
;; `mevedel-workspace' struct carrying a root directory, a state
;; directory (`.mevedel/' under root), and optional additional roots
;; for cross-project access.  The main entry point
;; `mevedel-workspace' resolves the active workspace by checking
;; session > cached buffer-local > project.el detection, with a
;; file-based fallback for buffers outside any project.
;;
;; Keeps a workspace registry so that distinct buffers under the
;; same project share a single workspace struct (and therefore a
;; single state directory and additional-roots list).

;;; Code:

(require 'mevedel-structs)

;; `cl-extra'
(declare-function cl-some "cl-extra" (cl-pred cl-seq &rest cl-rest))

;; `mevedel-execution-target'
(declare-function mevedel-execution-target-create
                  "mevedel-execution-target" (workspace-root))
(declare-function mevedel-execution-target-environment
                  "mevedel-execution-target" (cl-x) t)
(declare-function mevedel-execution-target-expand-path
                  "mevedel-execution-target" (target path &optional directory))
(autoload 'mevedel-execution-target-create "mevedel-execution-target")
(autoload 'mevedel-execution-target-environment "mevedel-execution-target")
(autoload 'mevedel-execution-target-expand-path "mevedel-execution-target")

;; `mevedel-structs'
(declare-function mevedel-file-cache--create "mevedel-structs" (&rest slots))
(declare-function mevedel-session-execution-target "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-workspace "mevedel-structs" (cl-x) t)
(declare-function mevedel-workspace--create "mevedel-structs" (&rest slots))
(declare-function mevedel-workspace-root "mevedel-structs" (cl-x) t)
(defvar mevedel--data-buffer)
(defvar mevedel--session)
(defvar mevedel-user-dir)

;; `mevedel-system'
(defvar mevedel-memory-dirs)

;; `project'
(declare-function project-current "project" (&optional maybe-prompt dir))
(declare-function project-name "project" (project))
(declare-function project-root "project" (project))

;; `tramp'
(defvar tramp-verbose)

(defcustom mevedel-workspace-functions '(mevedel-workspace--project-workspace mevedel-workspace--file-workspace)
  "Functions to determine the workspace for the current buffer.

Each function in this list is called with no arguments in the current
buffer until one returns a non-nil workspace cons cell of the form
\(TYPE . ID).

Functions should return nil if they cannot determine a workspace for the
current buffer, allowing other functions in the list to try.

Built-in workspace functions:
- `mevedel-workspace--project-workspace' - Detects project.el workspaces
- `mevedel-workspace--file-workspace' - Falls back to file-based workspace"
  :type 'hook
  :group 'mevedel)

(defcustom mevedel-workspace-types-alist
  '((project . (:get-root mevedel-workspace--project-root
                :get-name mevedel-workspace--project-name))
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
`mevedel-workspace-functions' to detect it."
  :type '(alist :key-type symbol :value-type (plist :key-type keyword :value-type function))
  :group 'mevedel)

(defcustom mevedel-workspace-additional-roots nil
  "Alist mapping workspace roots to lists of additional allowed directories.

Format: ((WORKSPACE-ROOT . (DIR1 DIR2 ...)) ...)

For each workspace, the listed directories will be added to the allowed
roots for LLM access.  This allows granting access to directories outside
the primary workspace root on a per-workspace basis.

When set buffer-locally in a chat buffer, grants persist only for that
session.  When set globally, grants persist across all sessions."
  :type '(alist :key-type directory :value-type (repeat directory))
  :group 'mevedel)


;;
;;; Workspace system variables

(defvar-local mevedel--workspace nil
  "Cached `mevedel-workspace' struct for this buffer.

In chat buffers, this is set during buffer creation (before the session
is created) and serves as a temporary cache.  Once `mevedel--session' is
set, workspace access goes through the session instead.

In non-session buffers (patch, diff-preview), this holds the workspace
struct directly.")

;; Ensure `mevedel--workspace' is always buffer-local
(put 'mevedel--workspace 'permanent-local t)


;;
;;; Workspace detection functions

(defvar mevedel-workspace--remote-project-cache
  (make-hash-table :test #'equal)
  "Remote directory to its detected project, or nil when it has none.
Cleared with the workspace registry by `mevedel-workspace-clear-registry'.")

(defun mevedel-workspace--project-current (directory)
  "Return DIRECTORY's project, probing a remote directory once and quietly.

Project detection walks to the top of the tree, so on a remote target every
hop is a round trip and the last hop is the bare method prefix, which is not
a directory: TRAMP reports that miss to the user on every probe.  The probe
is best effort, so its own diagnostics are not the user's business, and its
answer is remembered per directory rather than re-walked.  A directory that
becomes a project later is picked up after
`mevedel-workspace-clear-registry'."
  (if (not (file-remote-p directory))
      (project-current nil directory)
    ;; The key is the spelling as given: expanding a remote name asks the
    ;; target for its home directory, which opens a connection, and a cache
    ;; key must not perform I/O.  Distinct spellings only cost an entry.
    (let* ((key (file-name-as-directory directory))
           (cached (gethash key mevedel-workspace--remote-project-cache
                            'mevedel-workspace--absent)))
      (if (not (eq cached 'mevedel-workspace--absent))
          cached
        (puthash key
                 (let ((tramp-verbose 0))
                   (project-current nil directory))
                 mevedel-workspace--remote-project-cache)))))

(defun mevedel-workspace--project-workspace ()
  "Detect project workspace for the current buffer.
Returns (project . ROOT) if the buffer is in a project, nil otherwise."
  (when-let* ((project (mevedel-workspace--project-current default-directory)))
    (cons 'project (project-root project))))

(defun mevedel-workspace--file-workspace ()
  "Detect file workspace for the current buffer.
Returns (file . FILENAME) if the buffer is visiting a file, nil otherwise."
  (when-let* ((filename (buffer-file-name)))
    (cons 'file filename)))


(defun mevedel-workspace-file-buffers (workspace)
  "Return live buffers visiting files under WORKSPACE's root.

Membership is decided on the buffer's file name, never on the target.
`file-in-directory-p' resolves both arguments with `file-truename',
which on a remote root is a round trip per buffer -- and this runs on
every tool call, through the specialist-nudge step and again through
each capability reminder trigger.  Enough of them, issued on the same
connection the tool call is already using, stall the turn outright and
give a nested command somebody else's reply to parse.

Symlinks are still resolved when the root is local, where the check
costs no I/O.  A remote root compares by name only: a client that
reaches the same target through a different symlinked spelling is not
worth a per-buffer round trip on the hot path."
  (when-let* ((workspace workspace)
              (root (file-name-as-directory
                     (expand-file-name (mevedel-workspace-root workspace)))))
    ;; Hoisted: the truename of the root does not vary per buffer.
    (let ((true-root (unless (file-remote-p root)
                       (ignore-errors
                         (file-name-as-directory (file-truename root))))))
      (cl-remove-if-not
       (lambda (buffer)
         (when-let* ((file (buffer-file-name buffer)))
           (or (string-prefix-p root file)
               ;; A local root cannot contain a remote file, so the
               ;; fallback never reaches for a remote truename either.
               (and true-root
                    (not (file-remote-p file))
                    (when-let* ((true-file
                                 (ignore-errors (file-truename file))))
                      (string-prefix-p true-root true-file))))))
       (buffer-list)))))


;;
;;; Workspace type functions

(defun mevedel-workspace--project-root (project-id)
  "Get the project root for PROJECT-ID, validating it's a real project root."
  ;; Verify that project-id is actually a valid project root directory.
  (unless (and (stringp project-id)
               (file-name-absolute-p project-id)
               (file-directory-p project-id)
               (mevedel-workspace--project-current project-id))
    (error "Project ID '%s' is not a valid project root directory" project-id))
  project-id)

(defun mevedel-workspace--project-name (project-id)
  "Get the project name for PROJECT-ID using project.el."
  (if-let* ((project (mevedel-workspace--project-current project-id)))
      (project-name project)
    ;; Get the last directory name from the root (trailing slash removed).
    (file-name-nondirectory (directory-file-name project-id))))


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
                               :total-bytes 0)
                  :directives nil)
                 mevedel-workspace--registry))))

(defun mevedel-workspace-clear-registry ()
  "Remove all workspaces from the global registry.

Also drops remembered remote project detections, which are the same decision
cached one level lower.  Intended for testing and cleanup."
  (clrhash mevedel-workspace--registry)
  (when (boundp 'mevedel-workspace--remote-project-cache)
    (clrhash mevedel-workspace--remote-project-cache)))


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
;;; Workspace management


(defun mevedel-workspace (&optional buffer)
  "Get the workspace for BUFFER as a `mevedel-workspace' struct.

In chat buffers with an active session, returns the session's workspace.
When `mevedel--workspace' is set (e.g., during buffer setup before the
session exists), returns that cached value.  Otherwise, auto-detects via
`mevedel-workspace-functions' and returns a struct from the global
registry, creating one lazily if needed."
  (with-current-buffer (or buffer (current-buffer))
    (cond
     ;; Chat buffer with session: canonical path
     ((and (boundp 'mevedel--session) mevedel--session)
      (mevedel-session-workspace mevedel--session))
     ;; Cached workspace (during buffer setup, before session exists)
     ((and (boundp 'mevedel--workspace) mevedel--workspace)
      mevedel--workspace)
     ;; Auto-detect from buffer context
     (t
      (when-let* ((detected (cl-some #'funcall mevedel-workspace-functions)))
        (let* ((type (car detected))
               (id (cdr detected))
               (type-config (alist-get type mevedel-workspace-types-alist))
               (root-fn (plist-get type-config :get-root))
               (name-fn (plist-get type-config :get-name))
               (root (when root-fn (funcall root-fn id)))
               (name (when name-fn (funcall name-fn id))))
          (mevedel-workspace-get-or-create type id root name)))))))


;;
;;; Generated state ignore

(defconst mevedel-workspace--generated-state-excludes
  '("/.mevedel/sessions/"
    "/.mevedel/tool-results/"
    "/.mevedel/input-history.el"
    "/.mevedel/media/"
    "/.mevedel/plugin-data/")
  "Root-anchored generated state entries for `.git/info/exclude'.")

(defun mevedel-workspace--git-exclude-file (root)
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
                  (mevedel-execution-target-expand-path
                   (mevedel-execution-target-create root) path root))
              (expand-file-name path root)))))))))

(defvar mevedel-workspace--generated-state-ignored
  (make-hash-table :test #'equal)
  "Workspace roots whose generated-state exclusion has been settled.

Holds roots outside Git as well, because deciding that costs the same walk as
acting on it.")

(defun mevedel-workspace-ensure-generated-state-ignored (workspace)
  "Add mevedel generated-state paths to WORKSPACE's Git exclude file.

Only generated runtime artifacts are ignored.  The top-level
`.mevedel/' directory is deliberately not ignored so durable project
state can still be tracked.

The answer is remembered per root for this Emacs process.  The exclude file
only ever gains entries, and it gains them once, but re-deriving that runs a
`locate-dominating-file' walk plus two `file-truename' resolutions -- on a
remote workspace that is a target round trip per path component, on every
save, to discover there is nothing to do.  A root whose exclude file is edited
by hand afterwards keeps the remembered answer until Emacs restarts, which is
the same bargain the session's `durable-tree-ensured' already makes for the
directory tree beside it."
  (let ((root (and workspace (mevedel-workspace-root workspace))))
    (when (and root
               (not (gethash root mevedel-workspace--generated-state-ignored)))
      (condition-case nil
          (progn
            (when-let* ((exclude-file
                         (mevedel-workspace--git-exclude-file root)))
              (make-directory (file-name-directory exclude-file) t)
              (let ((changed nil))
                (with-temp-buffer
                  (when (file-exists-p exclude-file)
                    (insert-file-contents exclude-file))
                  (dolist (entry mevedel-workspace--generated-state-excludes)
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
            (puthash root t mevedel-workspace--generated-state-ignored))
        (error nil)))))


;;
;;; Project root management

(defun mevedel--all-allowed-roots (&optional buffer)
  "Get all allowed roots for BUFFER's workspace.

Returns a list containing the workspace root, configured memory roots,
the execution target's temporary directory, and any additional roots via
`mevedel-workspace-additional-roots'."
  (let* ((buffer (or buffer (current-buffer)))
         (workspace-root (mevedel-workspace-root (mevedel-workspace buffer)))
         (session
          (with-current-buffer buffer
            (or (and (boundp 'mevedel--session) mevedel--session)
                (and (boundp 'mevedel--data-buffer)
                     (buffer-live-p mevedel--data-buffer)
                     (buffer-local-value
                      'mevedel--session mevedel--data-buffer)))))
         (target
          (when (file-remote-p workspace-root)
            (or (and session (mevedel-session-execution-target session))
                (mevedel-execution-target-create workspace-root))))
         (temporary-root
          (if target
              (let ((target-tmpdir
                     (cdr (assoc "TMPDIR"
                                 (mevedel-execution-target-environment
                                  target)))))
                (mevedel-execution-target-expand-path
                 target
                 (if (and target-tmpdir (not (equal target-tmpdir "")))
                     target-tmpdir
                   "/tmp")
                 workspace-root))
            (with-current-buffer buffer temporary-file-directory)))
         (memory-dirs (if (boundp 'mevedel-memory-dirs)
                          mevedel-memory-dirs
                        '(".mevedel/memory/" ".agents/memory/")))
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
                             'mevedel-workspace-additional-roots buffer)
                            nil nil #'equal))))
    (delete-dups
     (delq nil
           (mapcar (lambda (root)
                     (file-name-as-directory (expand-file-name root)))
                   roots)))))

(defun mevedel-workspace-file-in-allowed-roots-p (file &optional buffer)
  "Return the allowed root containing absolute FILE, or nil.
BUFFER specifies which workspace to check, defaulting to the current one.
The additional-roots alist is buffer-local to the session data buffer, so a
caller in a view buffer must name that buffer or it sees only the global
default."
  (let ((file (expand-file-name file)))
    (if (file-name-absolute-p file)
        (let ((roots (mevedel--all-allowed-roots buffer)))
          (catch 'found
            (dolist (root roots)
              (when (file-in-directory-p file root)
                (throw 'found root))))))))

(defun mevedel-workspace--session-data-buffer ()
  "Return the session data buffer reachable from `current-buffer'.
The data buffer carries the buffer-local `mevedel-workspace-additional-roots'
copy installed at chat-buffer setup; commands that mutate that alist must
run there to avoid silently hitting the global default when invoked from
a view buffer.  Falls back to `current-buffer' when no session is in
scope so out-of-session calls still mutate the global default."
  (let ((cur (current-buffer)))
    (cond
     ((buffer-local-value 'mevedel--session cur) cur)
     ((let ((db (buffer-local-value 'mevedel--data-buffer cur)))
        (and db (buffer-live-p db) db)))
     (t cur))))

;;;###autoload
(defun mevedel-add-project-root (directory)
  "Add DIRECTORY to the list of allowed roots for the current workspace.

This grants the LLM permission to read and write files in this
directory and its subdirectories for the current workspace only."
  (interactive "DAdd project root to current workspace: ")
  (unless (file-directory-p directory)
    (user-error "%s is not a directory" directory))
  (with-current-buffer (mevedel-workspace--session-data-buffer)
    (let* ((expanded (file-name-as-directory (expand-file-name directory)))
           ;; Try to get project root, otherwise default to directory
           (p-root (condition-case _
                       (project-root
                        (mevedel-workspace--project-current expanded))
                     (error expanded)))
           (workspace-root (mevedel-workspace-root (mevedel-workspace)))
           (current-roots (alist-get workspace-root mevedel-workspace-additional-roots nil nil #'equal)))
      (unless (member p-root current-roots)
        (setf (alist-get workspace-root mevedel-workspace-additional-roots nil nil #'equal)
              (cons p-root current-roots))
        (message "Added project root to workspace %s: %s" workspace-root p-root)))))

;;;###autoload
(defun mevedel-remove-project-root (directory)
  "Remove DIRECTORY from the list of allowed roots for the current workspace."
  (interactive
   (with-current-buffer (mevedel-workspace--session-data-buffer)
     (let* ((workspace-root (mevedel-workspace-root (mevedel-workspace)))
            (current-roots (alist-get workspace-root mevedel-workspace-additional-roots nil nil #'equal)))
       (list (if current-roots
                 (completing-read "Remove project root: " current-roots nil t)
               (user-error "No additional project roots configured for this workspace"))))))
  (with-current-buffer (mevedel-workspace--session-data-buffer)
    (let* ((workspace-root (mevedel-workspace-root (mevedel-workspace)))
           (current-roots (alist-get workspace-root mevedel-workspace-additional-roots nil nil #'equal)))
      (setf (alist-get workspace-root mevedel-workspace-additional-roots nil nil #'equal)
            (delete directory current-roots))
      (message "Removed project root from workspace: %s" directory))))

;;;###autoload
(defun mevedel-list-project-roots ()
  "Display the list of allowed project roots for the current workspace."
  (interactive)
  (with-current-buffer (mevedel-workspace--session-data-buffer)
    (let* ((workspace-root (mevedel-workspace-root (mevedel-workspace)))
           (additional-roots (alist-get workspace-root mevedel-workspace-additional-roots nil nil #'equal)))
      (message "Workspace root: %s%s"
               workspace-root
               (if additional-roots
                   (format "\nAdditional roots: %s"
                           (mapconcat #'identity additional-roots ", "))
                 "")))))

(provide 'mevedel-workspace)
;;; mevedel-workspace.el ends here
