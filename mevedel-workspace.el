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

;; `cl-extra'
(declare-function cl-some "cl-extra" (cl-pred cl-seq &rest cl-rest))

;; `mevedel-structs'
(declare-function mevedel-workspace-get-or-create "mevedel-structs"
                  (type id root name))
(declare-function mevedel-workspace-root "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-workspace "mevedel-structs" (cl-x) t)
(defvar mevedel--session)
(defvar mevedel--data-buffer)

;; `mevedel-system'
(defvar mevedel-memory-dirs)

;; `project'
(declare-function project-current "project" (&optional maybe-prompt dir))
(declare-function project-root "project" (project))
(declare-function project-name "project" (project))

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

(defun mevedel-workspace--project-workspace ()
  "Detect project workspace for the current buffer.
Returns (project . ROOT) if the buffer is in a project, nil otherwise."
  (when-let* ((project (project-current nil default-directory)))
    (cons 'project (project-root project))))

(defun mevedel-workspace--file-workspace ()
  "Detect file workspace for the current buffer.
Returns (file . FILENAME) if the buffer is visiting a file, nil otherwise."
  (when-let ((filename (buffer-file-name)))
    (cons 'file filename)))


;;
;;; Workspace type functions

(defun mevedel-workspace--project-root (project-id)
  "Get the project root for PROJECT-ID, validating it's a real project root."
  ;; Verify that project-id is actually a valid project root directory.
  (unless (and (stringp project-id)
               (file-name-absolute-p project-id)
               (file-directory-p project-id)
               (project-current nil project-id))
    (error "Project ID '%s' is not a valid project root directory" project-id))
  project-id)

(defun mevedel-workspace--project-name (project-id)
  "Get the project name for PROJECT-ID using project.el."
  (if-let* ((project (project-current nil project-id)))
      (project-name project)
    ;; Get the last directory name from the root (trailing slash removed).
    (file-name-nondirectory (directory-file-name project-id))))


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
  (when (and (stringp root)
             (file-directory-p root)
             (executable-find "git"))
    (let ((default-directory root))
      (when (zerop (process-file "git" nil nil nil
                                 "rev-parse" "--is-inside-work-tree"))
        (with-temp-buffer
          (when (zerop (process-file "git" nil t nil
                                     "rev-parse" "--git-dir"))
            (expand-file-name
             "info/exclude"
             (replace-regexp-in-string "[\r\n]+\\'" ""
                                       (buffer-string)))))))))

(defun mevedel-workspace-ensure-generated-state-ignored (workspace)
  "Add mevedel generated-state paths to WORKSPACE's Git exclude file.

Only generated runtime artifacts are ignored.  The top-level
`.mevedel/' directory is deliberately not ignored so durable project
state can still be tracked."
  (condition-case nil
      (when-let* ((root (and workspace (mevedel-workspace-root workspace)))
                  (exclude-file (mevedel-workspace--git-exclude-file root)))
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
    (error nil)))


;;
;;; Project root management

(defun mevedel--all-allowed-roots (&optional buffer)
  "Get all allowed roots for BUFFER's workspace.

Returns a list containing the workspace root, configured memory roots,
the system temporary directory, and any additional roots configured via
`mevedel-workspace-additional-roots'."
  (let* ((workspace-root (mevedel-workspace-root (mevedel-workspace buffer)))
         (memory-dirs (if (boundp 'mevedel-memory-dirs)
                          mevedel-memory-dirs
                        '(".mevedel/memory/" ".agents/memory/")))
         (roots (append
                 (list workspace-root
                       (with-current-buffer (or buffer (current-buffer))
                         temporary-file-directory))
                 (mapcar
                  (lambda (dir)
                    (expand-file-name
                     dir
                     (unless (file-name-absolute-p dir)
                       workspace-root)))
                  memory-dirs)
                 (alist-get workspace-root mevedel-workspace-additional-roots
                            nil nil #'equal))))
    (delete-dups
     (delq nil
           (mapcar (lambda (root)
                     (file-name-as-directory (expand-file-name root)))
                   roots)))))

(defun mevedel-workspace--file-in-allowed-roots-p (file &optional buffer)
  "FILE needs to be absolute.
BUFFER specifies which workspace to check (defaults to current buffer).
Returns root of file or nil."
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
                       (project-root (project-current nil expanded))
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
