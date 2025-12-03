;;; mevedel-workspace.el -- Project workspaces -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;; `cl-extra'
(declare-function cl-some "cl-extra" (cl-pred cl-seq &rest cl-rest))

;; `project'
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
roots for LLM access. This allows granting access to directories outside
the primary workspace root on a per-workspace basis.

When set buffer-locally in a chat buffer, grants persist only for that
session. When set globally, grants persist across all sessions."
  :type '(alist :key-type directory :value-type (repeat directory))
  :group 'mevedel)


;;
;;; Workspace system variables

(defvar-local mevedel--workspace nil
  "Workspace information for the current mevedel session.
When nil (the default), uses `mevedel-workspace-functions' to determine
the workspace scope.

When set to a cons cell of the form (TYPE . ID) where:
- TYPE is a symbol identifying the workspace type
- ID is the workspace identifier (typically a root directory or file
  path)

The workspace is the space where the LLM is allowed to interact with
files.")

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
  "Get the workspace information for BUFFER.
Returns a cons cell representing the workspace scope.

The return value is always a cons cell of the form (TYPE . ID) where:
- TYPE is a symbol identifying the workspace type
- ID is the workspace identifier (typically a root directory or file path)

If the buffer-local variable `mevedel--workspace' is nil, this function
runs the functions in `mevedel-workspace-functions' until one returns a
non-nil workspace cons cell. If no function returns a workspace, it
returns nil."
  (with-current-buffer (or buffer (current-buffer))
    (or mevedel--workspace (cl-some #'funcall mevedel-workspace-functions))))

(defun mevedel-workspace--root (workspace)
  "Get the workspace root for WORKSPACE.

WORKSPACE is a cons cell (TYPE . ID) where TYPE is a workspace type.
Returns the root directory path for the workspace.

Uses the appropriate root function as configured in the
`mevedel-workspace-types-alist', and validates that the result is an
absolute path to a real directory."
  (let* ((workspace-type (car workspace))
         (workspace-id (cdr workspace))
         (type-config (alist-get workspace-type mevedel-workspace-types-alist))
         (root-fn (plist-get type-config :get-root))
         (root
          (if root-fn
              (or (funcall root-fn workspace-id)
                  (error
                   "Root function for workspace type %s failed to return a root" workspace-type))
            (error "No root function configured for workspace type %s" workspace-type))))
    ;; Verify that the root is a real directory.
    (unless (and root (file-name-absolute-p root))
      (error "Workspace root '%s' is not an absolute path" root))
    (unless (and root (file-directory-p root))
      (error "Workspace root '%s' is not a valid directory" root))
    root))

(defun mevedel-workspace--name (workspace)
  "Get a descriptive name for WORKSPACE.
WORKSPACE is a cons cell (TYPE . ID) where TYPE is a workspace type.
Returns a string containing the workspace name."
  (let* ((workspace-type (car workspace))
         (workspace-id (cdr workspace))
         (type-config (alist-get workspace-type mevedel-workspace-types-alist))
         (name-fn (plist-get type-config :get-name)))
    (if name-fn
        (funcall name-fn workspace-id)
      (error "No name function configured for workspace type %s" workspace-type))))


;;
;;; Project root management

(defun mevedel--all-allowed-roots (&optional buffer)
  "Get all allowed roots for BUFFER's workspace.

Returns a list containing the workspace root plus any additional roots
configured via `mevedel-workspace-additional-roots'."
  (let ((workspace-root (mevedel-workspace--root (mevedel-workspace buffer))))
    (cons workspace-root
          (alist-get workspace-root mevedel-workspace-additional-roots nil nil #'equal))))

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

;;;###autoload
(defun mevedel-add-project-root (directory)
  "Add DIRECTORY to the list of allowed roots for the current workspace.

This grants the LLM permission to read and write files in this
directory and its subdirectories for the current workspace only."
  (interactive "DAdd project root to current workspace: ")
  (unless (file-directory-p directory)
    (user-error "%s is not a directory" directory))
  (let* ((expanded (file-name-as-directory (expand-file-name directory)))
         ;; Try to get project root, otherwise default to directory
         (p-root (condition-case _
                     (project-root (project-current nil expanded))
                   (error expanded)))
         (workspace-root (mevedel-workspace--root (mevedel-workspace)))
         (current-roots (alist-get workspace-root mevedel-workspace-additional-roots nil nil #'equal)))
    (unless (member p-root current-roots)
      (setf (alist-get workspace-root mevedel-workspace-additional-roots nil nil #'equal)
            (cons p-root current-roots))
      (message "Added project root to workspace %s: %s" workspace-root p-root))))

;;;###autoload
(defun mevedel-remove-project-root (directory)
  "Remove DIRECTORY from the list of allowed roots for the current workspace."
  (interactive
   (let* ((workspace-root (mevedel-workspace--root (mevedel-workspace)))
          (current-roots (alist-get workspace-root mevedel-workspace-additional-roots nil nil #'equal)))
     (list (if current-roots
               (completing-read "Remove project root: " current-roots nil t)
             (user-error "No additional project roots configured for this workspace")))))
  (let* ((workspace-root (mevedel-workspace--root (mevedel-workspace)))
         (current-roots (alist-get workspace-root mevedel-workspace-additional-roots nil nil #'equal)))
    (setf (alist-get workspace-root mevedel-workspace-additional-roots nil nil #'equal)
          (delete directory current-roots))
    (message "Removed project root from workspace: %s" directory)))

;;;###autoload
(defun mevedel-list-project-roots ()
  "Display the list of allowed project roots for the current workspace."
  (interactive)
  (let* ((workspace-root (mevedel-workspace--root (mevedel-workspace)))
         (additional-roots (alist-get workspace-root mevedel-workspace-additional-roots nil nil #'equal)))
    (message "Workspace root: %s%s"
             workspace-root
             (if additional-roots
                 (format "\nAdditional roots: %s"
                         (mapconcat #'identity additional-roots ", "))
               ""))))

(provide 'mevedel-workspace)
;;; mevedel-workspace.el ends here
