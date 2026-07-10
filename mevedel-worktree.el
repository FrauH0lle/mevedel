;;; mevedel-worktree.el -- Git worktree sessions -*- lexical-binding: t -*-

;;; Commentary:

;; Provides the `/worktree' local slash command.  The command reports the
;; current Git worktree state and can create a fresh linked worktree under
;; `.worktrees/' with a new mevedel session rooted there.  Plan mode reuses
;; the same creation interface for worktree-backed implementation handoffs.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'tabulated-list)
  (require 'transient))

(require 'transient)

;; `mevedel-chat'
(declare-function mevedel--chat-buffer
                  "mevedel-chat"
                  (session-name &optional create workspace working-directory))
(declare-function mevedel--default-session-name-for-directory
                  "mevedel-chat" (workspace working-directory))
(declare-function mevedel--display-chat-buffer "mevedel-chat" (chat-buffer))
(declare-function mevedel--insert-local-user-turn
                  "mevedel-chat"
                  (prompt &optional display-text kind hook-context
                          no-spinner))
(declare-function mevedel--sessions-in-working-directory
                  "mevedel-chat" (sessions working-directory))
(declare-function mevedel--start-chat
                  "mevedel-chat"
                  (workspace working-directory prompt-session
                             &optional directory-scoped))
(declare-function mevedel--workspace-sessions "mevedel-chat" (workspace))

;; `mevedel-cockpit'
(declare-function mevedel-cockpit-context-data-buffer
                  "mevedel-cockpit" (&optional context))
(declare-function mevedel-cockpit-context-session
                  "mevedel-cockpit" (&optional context))
(declare-function mevedel-cockpit-context-workspace
                  "mevedel-cockpit" (&optional context))
(declare-function mevedel-cockpit-current-context
                  "mevedel-cockpit" ())
(declare-function mevedel-cockpit-open-surface
                  "mevedel-cockpit" (surface &optional context))
(declare-function mevedel-cockpit-quit "mevedel-cockpit" (&optional label))
(declare-function mevedel-cockpit-setup-tabulated-surface
                  "mevedel-cockpit" (surface))
(declare-function mevedel-cockpit-surface-context
                  "mevedel-cockpit" (&optional surface))
(declare-function mevedel-cockpit-surface-details
                  "mevedel-cockpit" ())
(declare-function mevedel-cockpit-surface-key-help-text
                  "mevedel-cockpit" (&optional surface))
(declare-function mevedel-cockpit-surface-quit
                  "mevedel-cockpit" ())
(declare-function mevedel-cockpit-surface-refresh
                  "mevedel-cockpit" (&optional selected-id))
(declare-function mevedel-cockpit-surface-selected
                  "mevedel-cockpit" (&optional no-error))

;; `mevedel-menu'
(declare-function mevedel-menu "mevedel-menu" ())

;; `mevedel-session-persistence'
(declare-function mevedel-session-persistence-save
                  "mevedel-session-persistence" (session buffer))
(defvar mevedel-session-persistence)

;; `mevedel-skills'
(defvar mevedel-slash-commands)

;; `mevedel-structs'
(declare-function mevedel-session-name "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-working-directory "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-workspace "mevedel-structs" (cl-x) t)
(declare-function mevedel-workspace-p "mevedel-structs" (cl-x))
(declare-function mevedel-workspace-root "mevedel-structs" (cl-x) t)
(defvar mevedel--data-buffer)
(defvar mevedel--current-request)
(defvar mevedel--session)
(defvar mevedel--view-buffer)

;; `mevedel-utilities'
(declare-function mevedel--file-in-directory-p
                  "mevedel-utilities" (file directory))
(declare-function mevedel--file-relative-name-or-absolute
                  "mevedel-utilities" (file directory))

;; `tabulated-list'
(declare-function tabulated-list-get-id "tabulated-list" ())
(declare-function tabulated-list-init-header "tabulated-list" ())
(declare-function tabulated-list-mode "tabulated-list" ())
(defvar tabulated-list-entries)
(defvar tabulated-list-format)
(defvar tabulated-list-padding)
(defvar tabulated-list-sort-key)


;;
;;; Git helpers

(defun mevedel-worktree--git-result (directory &rest args)
  "Run Git in DIRECTORY with ARGS and return (:exit N :output STRING)."
  (with-temp-buffer
    (let ((default-directory (file-name-as-directory
                              (expand-file-name directory))))
      (condition-case err
          (let ((exit (apply #'process-file
                             "git" nil (list t t) nil args)))
            (list :exit exit
                  :output (string-trim-right
                           (buffer-substring-no-properties
                            (point-min) (point-max)))))
        (file-missing
         (list :exit 127 :output (error-message-string err)))))))

(defun mevedel-worktree--git-success-output (directory &rest args)
  "Return trimmed Git output for ARGS in DIRECTORY, or nil on failure."
  (let ((result (apply #'mevedel-worktree--git-result directory args)))
    (when (eq 0 (plist-get result :exit))
      (string-trim (plist-get result :output)))))

(defun mevedel-worktree--git-exit (directory &rest args)
  "Return Git exit status for ARGS in DIRECTORY."
  (plist-get (apply #'mevedel-worktree--git-result directory args) :exit))

(defun mevedel-worktree--expand-git-path (path base-directory)
  "Expand Git PATH relative to BASE-DIRECTORY when needed."
  (when (and path (not (string-empty-p path)))
    (expand-file-name path base-directory)))

(defun mevedel-worktree--parse-worktree-list (output)
  "Parse `git worktree list --porcelain' OUTPUT into plist entries."
  (let (entries current)
    (dolist (line (split-string (or output "") "\n"))
      (cond
       ((string-empty-p line)
        (when current
          (push current entries)
          (setq current nil)))
       ((string-prefix-p "worktree " line)
        (when current
          (push current entries))
        (setq current (list :path (substring line 9))))
       ((string-prefix-p "HEAD " line)
        (setq current (plist-put current :head (substring line 5))))
       ((string-prefix-p "branch " line)
        (let ((branch (substring line 7)))
          (setq current
                (plist-put current :branch
                           (replace-regexp-in-string
                            "\\`refs/heads/" "" branch)))))
       ((string= line "detached")
        (setq current (plist-put current :detached t)))
       ((string= line "bare")
        (setq current (plist-put current :bare t)))
       ((string-prefix-p "locked" line)
        (setq current
              (plist-put current :locked
                         (string-trim (substring line 6)))))
       ((string-prefix-p "prunable" line)
        (setq current
              (plist-put current :prunable
                         (string-trim (substring line 8)))))))
    (when current
      (push current entries))
    (nreverse entries)))


;;
;;; Status

(defun mevedel-worktree--current-context ()
  "Return a plist describing the current session command context."
  (let* ((session (and (boundp 'mevedel--session) mevedel--session))
         (workspace (and session (mevedel-session-workspace session))))
    (list :data-buffer (and session (current-buffer))
          :session session
          :workspace workspace
          :directory (or (and session
                              (mevedel-session-working-directory session))
                         default-directory))))

(defun mevedel-worktree--ignore-state (directory)
  "Return `.worktrees/' ignore state for DIRECTORY."
  (pcase (mevedel-worktree--git-exit directory "check-ignore" "-q"
                                     ".worktrees/")
    (0 'ignored)
    (1 'not-ignored)
    (_ 'unknown)))

(defun mevedel-worktree--context-directory (context)
  "Return the normalized working directory for CONTEXT."
  (let* ((context (or context (mevedel-worktree--current-context)))
         (data-buffer (plist-get context :data-buffer))
         (session (plist-get context :session)))
    (file-name-as-directory
     (expand-file-name
      (or (plist-get context :directory)
          (and session (mevedel-session-working-directory session))
          (and (buffer-live-p data-buffer)
               (with-current-buffer data-buffer
                 default-directory))
          default-directory)))))

(defun mevedel-worktree--collect-status (&optional context)
  "Collect read-only worktree status for CONTEXT."
  (let* ((context (or context (mevedel-worktree--current-context)))
         (session (plist-get context :session))
         (workspace (or (plist-get context :workspace)
                        (and session (mevedel-session-workspace session))))
         (directory (mevedel-worktree--context-directory context))
         (workspace-root (and workspace
                              (file-name-as-directory
                               (expand-file-name
                                (mevedel-workspace-root workspace)))))
         (repo-root (mevedel-worktree--git-success-output
                     directory "rev-parse" "--show-toplevel")))
    (if (not repo-root)
        (list :session session
              :workspace workspace
              :directory directory
              :repo-root nil
              :isolation 'not-git
              :ignore-state 'unknown
              :worktrees nil)
      (let* ((repo-root (file-name-as-directory (expand-file-name repo-root)))
             (git-dir (mevedel-worktree--expand-git-path
                       (mevedel-worktree--git-success-output
                        directory "rev-parse" "--git-dir")
                       directory))
             (common-dir (mevedel-worktree--expand-git-path
                          (mevedel-worktree--git-success-output
                           directory "rev-parse" "--git-common-dir")
                          directory))
             (superproject (mevedel-worktree--git-success-output
                            directory
                            "rev-parse" "--show-superproject-working-tree"))
             (branch (mevedel-worktree--git-success-output
                      directory "branch" "--show-current"))
             (head (mevedel-worktree--git-success-output
                    directory "rev-parse" "--short" "HEAD"))
             (status-output (mevedel-worktree--git-success-output
                             directory "status" "--short"))
             (worktrees-output (mevedel-worktree--git-success-output
                                directory "worktree" "list" "--porcelain"))
             (ignore-directory (or workspace-root repo-root))
             (isolation (cond
                         ((and superproject
                               (not (string-empty-p superproject)))
                          'submodule)
                         ((not (file-equal-p git-dir common-dir))
                          'linked-worktree)
                         (t 'normal-checkout))))
        (list :session session
              :workspace workspace
              :directory directory
              :repo-root repo-root
              :git-common-dir common-dir
              :superproject superproject
              :isolation isolation
              :branch (and branch (not (string-empty-p branch)) branch)
              :head head
              :dirty-p (and status-output
                            (not (string-empty-p status-output)))
              :ignore-state (mevedel-worktree--ignore-state
                             ignore-directory)
              :worktrees (mevedel-worktree--parse-worktree-list
                          worktrees-output))))))

(defun mevedel-worktree--isolation-label (isolation)
  "Return a user-facing label for ISOLATION."
  (pcase isolation
    ('normal-checkout "normal checkout")
    ('linked-worktree "linked worktree")
    ('submodule "submodule")
    ('not-git "not a Git repository")
    (_ "unknown")))

(defun mevedel-worktree--ignore-label (state)
  "Return a user-facing label for `.worktrees/' ignore STATE."
  (pcase state
    ('ignored "ignored")
    ('not-ignored "not ignored")
    (_ "unknown")))

(defun mevedel-worktree--format-worktree-entry (entry)
  "Format one parsed Git worktree ENTRY."
  (format "- %s [%s]"
          (or (plist-get entry :path) "unknown")
          (or (plist-get entry :branch)
              (and (plist-get entry :detached)
                   (format "detached %s"
                           (or (plist-get entry :head) "HEAD")))
              (plist-get entry :head)
              "unknown")))

(defun mevedel-worktree--format-status (status)
  "Format collected worktree STATUS for display."
  (let* ((session (plist-get status :session))
         (branch (plist-get status :branch))
         (head (plist-get status :head))
         (worktrees (plist-get status :worktrees)))
    (string-join
     (append
      (list
       "Worktree status"
       (format "Repository: %s"
               (or (plist-get status :repo-root)
                   "not a Git repository"))
       (format "Current session: %s"
               (if session (mevedel-session-name session) "none"))
       (format "Current directory: %s"
               (plist-get status :directory))
       (format "Isolation: %s"
               (mevedel-worktree--isolation-label
                (plist-get status :isolation)))
       (format "Branch: %s"
               (cond
                (branch branch)
                (head (format "detached at %s" head))
                (t "unavailable")))
       (format ".worktrees/: %s"
               (mevedel-worktree--ignore-label
                (plist-get status :ignore-state)))
       (format "Dirty source checkout: %s"
               (if (plist-get status :dirty-p) "yes" "no"))
       "Existing worktrees:")
      (if worktrees
          (mapcar #'mevedel-worktree--format-worktree-entry worktrees)
        '("- none"))
      (list
       "Usage: /worktree status | /worktree create [NAME] [--for \"purpose\"] [--clean]"))
     "\n")))


;;
;;; Surface

(defconst mevedel-worktree-list-buffer-name "*mevedel worktree*"
  "Name of the tabulated worktree list buffer.")

(defconst mevedel-worktree-help-buffer-name "*mevedel worktree help*"
  "Name of the worktree cockpit help buffer.")

(defun mevedel-worktree-status--data-buffer (&optional context)
  "Return the data buffer that launched the worktree transient."
  (require 'mevedel-cockpit)
  (or (mevedel-cockpit-context-data-buffer
       (or context (mevedel-cockpit-current-context)))
      (user-error "No live mevedel session for this worktree status")))

(defun mevedel-worktree--branch-head-label (branch head)
  "Return compact branch or HEAD label from BRANCH and HEAD."
  (cond
   ((and branch (not (string-empty-p branch))) branch)
   ((and head (not (string-empty-p head))) (format "detached at %s" head))
   (t "unavailable")))

(defun mevedel-worktree-status-summary (&optional context)
  "Return a compact worktree status summary for CONTEXT."
  (let* ((context (or context (mevedel-worktree--current-context)))
         (directory (mevedel-worktree--context-directory context))
         (inside (mevedel-worktree--git-success-output
                  directory "rev-parse" "--is-inside-work-tree")))
    (if (not (string= inside "true"))
        '(:state not-git :label "not-git")
      (let ((branch (mevedel-worktree--git-success-output
                     directory "branch" "--show-current"))
            (head (mevedel-worktree--git-success-output
                   directory "rev-parse" "--short" "HEAD")))
        (list :state 'git
              :label (cond
                      ((and branch (not (string-empty-p branch))) branch)
                      ((and head (not (string-empty-p head)))
                       (format "detached %s" head))
                      (t "not-git")))))))

(defun mevedel-worktree-status--description ()
  "Return dynamic description for the worktree status transient."
  (require 'mevedel-cockpit)
  (let* ((context (mevedel-cockpit-current-context))
         (data-buffer (mevedel-worktree-status--data-buffer context))
         (status (with-current-buffer data-buffer
                   (mevedel-worktree--collect-status context)))
         (session (plist-get status :session))
         (worktrees (plist-get status :worktrees)))
    (string-join
     (list
      (propertize "mevedel worktree" 'face 'transient-heading)
      (format "Repo:       %s"
              (or (plist-get status :repo-root)
                  "not a Git repository"))
      (format "Session:    %s"
              (if session (mevedel-session-name session) "none"))
      (format "Directory:  %s" (plist-get status :directory))
      (format "Isolation:  %s"
              (mevedel-worktree--isolation-label
               (plist-get status :isolation)))
      (format "Branch:     %s"
              (mevedel-worktree--branch-head-label
               (plist-get status :branch)
               (plist-get status :head)))
      (format ".worktrees: %s"
              (mevedel-worktree--ignore-label
               (plist-get status :ignore-state)))
      (format "Dirty:      %s"
              (if (plist-get status :dirty-p) "yes" "no"))
      (format "Worktrees:  %d" (length worktrees)))
     "\n")))

(defun mevedel-worktree-status-create ()
  "Create a worktree from the status transient."
  (interactive)
  (let (result)
    (with-current-buffer (mevedel-worktree-status--data-buffer)
      (setq result (mevedel-cmd--worktree "create")))
    (when (stringp result)
      (message "%s" result))
    (with-current-buffer (mevedel-worktree-status--data-buffer)
      (mevedel-worktree-status-open))
    result))

(defun mevedel-worktree-status-list ()
  "Open the tabulated worktree list from the status transient."
  (interactive)
  (require 'mevedel-cockpit)
  (let* ((context (mevedel-cockpit-current-context))
         (data-buffer (mevedel-worktree-status--data-buffer context)))
    (with-current-buffer data-buffer
      (mevedel-worktree-list-open context))))

(defun mevedel-worktree-status-refresh ()
  "Refresh the worktree status transient."
  (interactive)
  (with-current-buffer (mevedel-worktree-status--data-buffer)
    (mevedel-worktree-status-open)))

(defun mevedel-worktree--help-text (&optional _context)
  "Return worktree cockpit help text."
  (require 'mevedel-cockpit)
  (concat
   "mevedel worktree cockpit\n\n"
   "Status keys\n"
   "c  Create a linked worktree session\n"
   "l  List Git worktrees\n"
   "g  Refresh status\n"
   "?  Show this help\n"
   "b  Back to the main session cockpit\n\n"
   "List keys\n"
   (mevedel-cockpit-surface-key-help-text mevedel-worktree-list--surface)
   "\n"))

(defun mevedel-worktree-status-help ()
  "Open worktree status help."
  (interactive)
  (let ((help-window-select t))
    (with-help-window mevedel-worktree-help-buffer-name
      (princ (mevedel-worktree--help-text)))))

(defun mevedel-worktree-status-back ()
  "Return from worktree status to the main session cockpit."
  (interactive)
  (with-current-buffer (mevedel-worktree-status--data-buffer)
    (require 'mevedel-menu)
    (mevedel-menu)))

(transient-define-prefix mevedel-worktree-status ()
  "Transient worktree status cockpit."
  [:description mevedel-worktree-status--description
   ["Worktree"
    :pad-keys t
    ("c" "Create" mevedel-worktree-status-create)
    ("l" "List" mevedel-worktree-status-list)
    ("g" "Refresh" mevedel-worktree-status-refresh)
    ("?" "Help" mevedel-worktree-status-help)
    ("b" "Back" mevedel-worktree-status-back)]])

(defun mevedel-worktree-status-open ()
  "Open the transient worktree status cockpit."
  (interactive)
  (transient-setup 'mevedel-worktree-status))

(defun mevedel-worktree-list--context ()
  "Return the current worktree list context."
  (require 'mevedel-cockpit)
  (mevedel-cockpit-surface-context))

(defun mevedel-worktree-list--context-data-buffer (&optional context)
  "Return the current worktree list data buffer."
  (let ((context (or context (mevedel-worktree-list--context))))
    (mevedel-cockpit-context-data-buffer context)))

(defun mevedel-worktree-list--status (&optional context)
  "Return current worktree status from CONTEXT's data buffer."
  (let ((context (or context (mevedel-worktree-list--context))))
    (with-current-buffer (mevedel-worktree-list--context-data-buffer context)
      (mevedel-worktree--collect-status context))))

(defun mevedel-worktree-list--normalize-path (path)
  "Return PATH as an absolute directory name."
  (file-name-as-directory (expand-file-name path)))

(defun mevedel-worktree-list--workspace-path (path workspace)
  "Return PATH using WORKSPACE's root spelling when possible."
  (let ((path (mevedel-worktree-list--normalize-path path)))
    (if (not (mevedel-workspace-p workspace))
        path
      (require 'mevedel-utilities)
      (let* ((root (mevedel-worktree-list--normalize-path
                    (mevedel-workspace-root workspace)))
             (relative (mevedel--file-relative-name-or-absolute
                        path root)))
        (if (file-name-absolute-p relative)
            path
          (mevedel-worktree-list--normalize-path
           (expand-file-name relative root)))))))

(defun mevedel-worktree-list--item-id (item)
  "Return stable row id for worktree ITEM."
  (plist-get item :path))

(defun mevedel-worktree-list--branch-label (entry)
  "Return the list branch label for worktree ENTRY."
  (cond
   ((plist-get entry :branch))
   ((plist-get entry :detached)
    (format "detached %s" (or (plist-get entry :head) "HEAD")))
   ((plist-get entry :bare) "bare")
   ((plist-get entry :head))
   (t "unknown")))

(defun mevedel-worktree-list--state-label (entry)
  "Return the normalized state label for worktree ENTRY."
  (cond
   ((plist-get entry :locked) "locked")
   ((plist-get entry :prunable) "prunable")
   ((plist-get entry :bare) "bare")
   ((plist-get entry :detached) "detached")
   ((plist-get entry :branch) "branch")
   (t "unknown")))

(defun mevedel-worktree-list--current-label (current-p)
  "Return the Current column label for CURRENT-P."
  (if current-p "yes" ""))

(defun mevedel-worktree-list--sessions (workspace path)
  "Return live session names in WORKSPACE whose cwd is PATH."
  (when workspace
    (require 'mevedel-chat)
    (mapcar
     #'car
     (mevedel--sessions-in-working-directory
      (mevedel--workspace-sessions workspace)
      path))))

(defun mevedel-worktree-list--item (status entry)
  "Return a tabulated worktree item from STATUS and porcelain ENTRY."
  (let* ((workspace (plist-get status :workspace))
         (path (mevedel-worktree-list--workspace-path
                (plist-get entry :path) workspace))
         (directory (mevedel-worktree-list--workspace-path
                     (plist-get status :directory) workspace))
         (current (equal path directory))
         (sessions (mevedel-worktree-list--sessions workspace path)))
    (list :path path
          :branch (mevedel-worktree-list--branch-label entry)
          :head (or (plist-get entry :head) "")
          :current current
          :state (mevedel-worktree-list--state-label entry)
          :sessions sessions
          :entry entry)))

(defun mevedel-worktree-list--items (status)
  "Return tabulated worktree items for STATUS."
  (mapcar (lambda (entry)
            (mevedel-worktree-list--item status entry))
          (plist-get status :worktrees)))

(defun mevedel-worktree-list--entry (item &optional _context)
  "Return a `tabulated-list-mode' row for worktree ITEM."
  (list
   (mevedel-worktree-list--item-id item)
   (vector
    (plist-get item :path)
    (plist-get item :branch)
    (plist-get item :head)
    (mevedel-worktree-list--current-label (plist-get item :current))
    (plist-get item :state)
    (string-join (plist-get item :sessions) ", "))))

(defun mevedel-worktree-list--collect (context)
  "Return worktree list items for CONTEXT."
  (mevedel-worktree-list--items
   (mevedel-worktree-list--status context)))

(defun mevedel-worktree-list-refresh ()
  "Refresh the tabulated worktree list."
  (interactive)
  (require 'mevedel-cockpit)
  (mevedel-cockpit-surface-refresh))

(defun mevedel-worktree-list--selected-item ()
  "Return the selected worktree item, or nil."
  (require 'mevedel-cockpit)
  (mevedel-cockpit-surface-selected t))

(defun mevedel-worktree-list--details-text (item &optional _context)
  "Return normalized details text for worktree ITEM."
  (string-join
   (list
    (format "Worktree %s" (plist-get item :path))
    (format "Path: %s" (plist-get item :path))
    (format "Branch: %s" (plist-get item :branch))
    (format "Head: %s" (or (plist-get item :head) ""))
    (format "Current: %s"
            (if (plist-get item :current) "yes" "no"))
    (format "State: %s" (plist-get item :state))
    (format "Sessions: %s"
            (or (string-join (plist-get item :sessions) ", ") "")))
   "\n"))

(defun mevedel-worktree-list-details ()
  "Show details for the selected worktree row."
  (interactive)
  (require 'mevedel-cockpit)
  (mevedel-cockpit-surface-details))

(defun mevedel-worktree-list-open-selected ()
  "Open or switch to the selected worktree's mevedel session."
  (interactive)
  (let* ((item (or (mevedel-worktree-list--selected-item)
                   (user-error "No worktree on this line")))
         (context (mevedel-worktree-list--context))
         (path (plist-get item :path))
         (workspace (mevedel-cockpit-context-workspace context)))
    (unless workspace
      (user-error "No mevedel workspace for selected worktree"))
    (require 'mevedel-chat)
    (mevedel--start-chat workspace path nil t)))

(defun mevedel-worktree-list-create ()
  "Create a worktree by delegating to `/worktree create'."
  (interactive)
  (let (result)
    (with-current-buffer (mevedel-worktree-list--context-data-buffer)
      (setq result (mevedel-cmd--worktree "create")))
    (when (stringp result)
      (message "%s" result))
    (mevedel-worktree-list-refresh)
    result))

(defun mevedel-worktree-list--delete (force)
  "Remove the selected worktree.
When FORCE is non-nil, pass `--force' to `git worktree remove'."
  (let* ((item (or (mevedel-worktree-list--selected-item)
                   (user-error "No worktree on this line")))
         (context (mevedel-worktree-list--context))
         (workspace (or (mevedel-cockpit-context-workspace context)
                        (user-error "No mevedel workspace for selected worktree")))
         (workspace-root (file-name-as-directory
                          (expand-file-name
                           (mevedel-workspace-root workspace))))
         (worktrees-root (file-name-as-directory
                          (file-name-concat workspace-root ".worktrees")))
         (path (mevedel-worktree-list--normalize-path
                (plist-get item :path)))
         (entry (plist-get item :entry))
         (branch (plist-get entry :branch))
         (sessions (mevedel-worktree-list--sessions workspace path)))
    (when (plist-get item :current)
      (user-error "Cannot remove the current worktree"))
    (require 'mevedel-utilities)
    (unless (mevedel--file-in-directory-p path worktrees-root)
      (user-error "Can only remove worktrees under %s" worktrees-root))
    (when (plist-get entry :bare)
      (user-error "Cannot remove a bare worktree"))
    (when (plist-get entry :locked)
      (user-error "Cannot remove a locked worktree"))
    (when sessions
      (user-error "Worktree has live mevedel sessions: %s"
                  (string-join sessions ", ")))
    (unless force
      (let ((status (mevedel-worktree--git-result
                     path "status" "--porcelain")))
        (unless (eq 0 (plist-get status :exit))
          (user-error "Git status failed: %s"
                      (plist-get status :output)))
        (unless (string-empty-p (plist-get status :output))
          (user-error "Worktree has uncommitted changes; use D to force remove"))))
    (unless (yes-or-no-p
             (if force
                 (format "Force remove %s and discard uncommitted changes? "
                         path)
               (format "Remove worktree %s? " path)))
      (user-error "Cancelled"))
    (let ((result (apply #'mevedel-worktree--git-result
                         (mevedel-worktree--context-directory context)
                         "worktree" "remove"
                         (append (when force (list "--force"))
                                 (list path)))))
      (unless (eq 0 (plist-get result :exit))
        (user-error "Git worktree remove failed: %s"
                    (plist-get result :output))))
    (mevedel-worktree-list-refresh)
    (message "mevedel: removed worktree %s; %s"
             path
             (if branch
                 (format "branch %s was left intact" branch)
               "no branch was deleted"))))

(defun mevedel-worktree-list-delete ()
  "Remove the selected clean worktree."
  (interactive)
  (mevedel-worktree-list--delete nil))

(defun mevedel-worktree-list-force-delete ()
  "Force-remove the selected worktree."
  (interactive)
  (mevedel-worktree-list--delete t))

(defun mevedel-worktree-list-help ()
  "Open worktree list help."
  (interactive)
  (mevedel-worktree-status-help))

(defun mevedel-worktree-list-quit ()
  "Quit the worktree list and return to the main session cockpit."
  (interactive)
  (require 'mevedel-cockpit)
  (mevedel-cockpit-quit "worktree list"))

(defconst mevedel-worktree-list--surface
  `(:buffer-name ,mevedel-worktree-list-buffer-name
    :label "worktree list"
    :row-label "worktree"
    :mode mevedel-worktree-list-mode
    :format [("Path" 36 t)
             ("Branch" 20 t)
             ("Head" 10 t)
             ("Current" 8 t)
             ("State" 10 t)
             ("Sessions" 0 t)]
    :sort-key ("Path" . nil)
    :collect mevedel-worktree-list--collect
    :entry mevedel-worktree-list--entry
    :details mevedel-worktree-list--details-text
    :details-buffer "*mevedel worktree details*"
    :help-function mevedel-worktree--help-text
    :help-buffer ,mevedel-worktree-help-buffer-name
    :keys (("c" "Create a linked worktree session"
            mevedel-worktree-list-create)
           ("o" "Open selected worktree session"
            mevedel-worktree-list-open-selected)
           ("d" "Delete selected worktree"
            mevedel-worktree-list-delete)
           ("D" "Force-delete selected worktree"
            mevedel-worktree-list-force-delete)))
  "Cockpit surface spec for the worktree list.")

(define-derived-mode mevedel-worktree-list-mode tabulated-list-mode
  "mevedel-worktree"
  "Major mode for the tabulated worktree list."
  (require 'mevedel-cockpit)
  (mevedel-cockpit-setup-tabulated-surface
   mevedel-worktree-list--surface))

(defun mevedel-worktree-list-open (&optional context)
  "Open the tabulated worktree list for CONTEXT."
  (require 'mevedel-cockpit)
  (let ((context (or context (mevedel-cockpit-current-context))))
    (mevedel-cockpit-open-surface mevedel-worktree-list--surface context)))


;;
;;; Create

(defun mevedel-worktree--split-args (args)
  "Split ARGS as slash-command arguments."
  (condition-case err
      (split-string-and-unquote (or args ""))
    (error
     (user-error "Could not parse /worktree arguments: %s"
                 (error-message-string err)))))

(defun mevedel-worktree--slugify (text)
  "Return a short branch-name slug for TEXT."
  (let* ((downcased (downcase (or text "")))
         (slug (replace-regexp-in-string "[^a-z0-9._-]+" "-" downcased))
         (slug (string-trim slug "-+" "-+")))
    (if (string-empty-p slug)
        "session"
      (substring slug 0 (min (length slug) 48)))))

(defun mevedel-worktree--default-branch-name (session purpose)
  "Return default branch name for SESSION and optional PURPOSE."
  (format "worktree/%s"
          (mevedel-worktree--slugify
           (or purpose
               (and session (mevedel-session-name session))
               "session"))))

(defun mevedel-worktree--parse-create-options (tokens)
  "Parse create command TOKENS."
  (let (name purpose clean)
    (while tokens
      (let ((token (pop tokens)))
        (cond
         ((string= token "--clean")
          (setq clean t))
         ((string= token "--for")
          (unless tokens
            (user-error "'--for' requires a purpose"))
          (setq purpose (pop tokens)))
         ((string-prefix-p "--" token)
          (user-error "Unknown /worktree create option: %s" token))
         ((not name)
          (setq name token))
         (t
          (user-error "Unexpected /worktree create argument: %s" token)))))
    (list :name name :purpose purpose :clean clean)))

(defun mevedel-worktree--validate-branch-name (name &optional directory)
  "Validate Git branch NAME for worktree creation.

When DIRECTORY is non-nil, use Git's refname checker for the full
branch-name grammar before any mutating Git command runs."
  (when (or (not (stringp name)) (string-empty-p name))
    (user-error "Branch name is required"))
  (when directory
    (pcase (mevedel-worktree--git-exit
            directory "check-ref-format" "--branch" name)
      (0 nil)
      (_ (user-error "Invalid branch name: %s" name)))))

(defun mevedel-worktree--branch-leaf (branch)
  "Return the final path component for BRANCH."
  (file-name-nondirectory (directory-file-name branch)))

(defun mevedel-worktree--ensure-local-exclude (common-git-dir)
  "Ensure `/.worktrees/' is listed in COMMON-GIT-DIR/info/exclude."
  (unless (and common-git-dir (file-directory-p common-git-dir))
    (user-error "Could not find Git common directory"))
  (let* ((info-dir (file-name-concat common-git-dir "info"))
         (exclude-file (file-name-concat info-dir "exclude"))
         (entry "/.worktrees/")
         (contents (if (file-exists-p exclude-file)
                       (with-temp-buffer
                         (insert-file-contents exclude-file)
                         (buffer-string))
                     "")))
    (unless (member entry (split-string contents "\n" t))
      (make-directory info-dir t)
      (with-temp-buffer
        (insert contents)
        (unless (or (string-empty-p contents)
                    (string-suffix-p "\n" contents))
          (insert "\n"))
        (insert entry "\n")
        (write-region (point-min) (point-max) exclude-file nil 'silent)))))

(defun mevedel-worktree--open-session (workspace worktree-directory)
  "Open a mevedel session for WORKSPACE at WORKTREE-DIRECTORY."
  (require 'mevedel-chat)
  (let* ((session-name (mevedel--default-session-name-for-directory
                        workspace worktree-directory))
         (chat-buffer (mevedel--chat-buffer
                       session-name t workspace worktree-directory)))
    (mevedel--display-chat-buffer chat-buffer)
    chat-buffer))

(defun mevedel-worktree--format-stub
    (source-session source-dir worktree-directory branch purpose warnings)
  "Return setup-context stub for SOURCE-SESSION.
SOURCE-DIR, WORKTREE-DIRECTORY, BRANCH, PURPOSE and WARNINGS describe the
new session."
  (string-join
   (append
    (list
     "Setup context for this worktree session."
     ""
     (format "Created from session: %s"
             (or source-session "unknown"))
     (format "Source directory: %s" source-dir)
     (format "Worktree directory: %s" worktree-directory)
     (format "Branch: %s" branch))
    (when (and purpose (not (string-empty-p purpose)))
      (list (format "Purpose: %s" purpose)))
    (when warnings
      (append '("" "Warnings:")
              (mapcar (lambda (warning) (format "- %s" warning))
                      warnings)))
    '("" "This is setup context only. Wait for the user's next prompt before taking action."))
   "\n"))

(defun mevedel-worktree--save-stub (chat-buffer)
  "Persist CHAT-BUFFER after a setup stub was inserted."
  (with-current-buffer chat-buffer
    (when (bound-and-true-p mevedel-session-persistence)
      (require 'mevedel-session-persistence)
      (condition-case err
          (mevedel-session-persistence-save mevedel--session chat-buffer)
        (error
         (display-warning
          'mevedel
          (format "Could not save worktree setup context: %s"
                  (error-message-string err))
          :warning))))))

(defun mevedel-worktree--cleanup-message
    (worktree-directory workspace-root)
  "Return shell cleanup commands for WORKTREE-DIRECTORY under WORKSPACE-ROOT."
  (let* ((path (if (and workspace-root
                        (file-in-directory-p worktree-directory workspace-root))
                   (directory-file-name
                    (file-relative-name worktree-directory workspace-root))
                 (directory-file-name worktree-directory)))
         (quoted (shell-quote-argument path)))
    (format
     "Cleanup:\n  git worktree remove %s\n  rm -rf %s\n  git worktree prune"
     quoted quoted)))

(defun mevedel-worktree-create-session (&optional branch purpose clean)
  "Create and return a new worktree session from the current session.
Prompt for BRANCH when it is nil.  PURPOSE is included in the setup
context.  When CLEAN is non-nil, omit that context.  The return value is
a plist with `:buffer', `:branch', `:directory', and `:warnings'."
  (let* ((context (mevedel-worktree--current-context))
         (data-buffer (plist-get context :data-buffer))
         (session (plist-get context :session)))
    (unless (and data-buffer session)
      (user-error "Worktree creation must run from a mevedel session"))
    (with-current-buffer data-buffer
      (when (bound-and-true-p mevedel--current-request)
        (user-error "A request is already active -- wait or abort first"))
      (let* ((branch
              (or branch
                  (read-string
                   "Worktree branch name: " nil nil
                   (mevedel-worktree--default-branch-name session purpose))))
             (workspace (mevedel-session-workspace session))
             (workspace-root (file-name-as-directory
                              (expand-file-name
                               (mevedel-workspace-root workspace))))
             (source-directory (file-name-as-directory
                                (expand-file-name
                                 (mevedel-session-working-directory
                                  session))))
             (status (mevedel-worktree--collect-status context))
             (repo-root (plist-get status :repo-root))
             (leaf (progn
                     (mevedel-worktree--validate-branch-name
                      branch source-directory)
                     (mevedel-worktree--branch-leaf branch)))
             (worktrees-dir (file-name-concat workspace-root ".worktrees"))
             (worktree-directory (file-name-as-directory
                                  (file-name-concat worktrees-dir leaf)))
             (warnings (delq
                        nil
                        (list
                         (when (plist-get status :dirty-p)
                           "source checkout is dirty; uncommitted changes were not copied")
                         (unless (plist-get status :branch)
                           "source checkout is detached; the new branch starts from the current commit")))))
        (unless repo-root
          (user-error "Current directory is not a Git repository"))
        (when (eq 'submodule (plist-get status :isolation))
          (user-error "Cannot create a worktree from a submodule"))
        (when (file-exists-p worktree-directory)
          (user-error "Worktree destination already exists: %s"
                      worktree-directory))
        (make-directory worktrees-dir t)
        (mevedel-worktree--ensure-local-exclude
         (plist-get status :git-common-dir))
        (let ((result (mevedel-worktree--git-result
                       source-directory
                       "worktree" "add" "-b" branch
                       worktree-directory)))
          (unless (eq 0 (plist-get result :exit))
            (user-error "Git worktree add failed: %s"
                        (plist-get result :output))))
        (condition-case err
            (let ((chat-buffer (mevedel-worktree--open-session
                                workspace worktree-directory)))
              (unless clean
                (with-current-buffer chat-buffer
                  (mevedel--insert-local-user-turn
                   (mevedel-worktree--format-stub
                    (mevedel-session-name session)
                    source-directory
                    worktree-directory
                    branch
                    purpose
                    warnings)
                   nil 'worktree nil t))
                (mevedel-worktree--save-stub chat-buffer))
              (list :buffer chat-buffer
                    :branch branch
                    :directory worktree-directory
                    :warnings warnings))
          (error
           (user-error
            "Created worktree at %s, but session setup failed: %s\n%s"
            worktree-directory
            (error-message-string err)
            (mevedel-worktree--cleanup-message
             worktree-directory workspace-root))))))))

(defun mevedel-worktree--create (args)
  "Create a new worktree session from slash command ARGS."
  (let* ((options (mevedel-worktree--parse-create-options
                   (cdr (mevedel-worktree--split-args args))))
         (result (mevedel-worktree-create-session
                  (plist-get options :name)
                  (plist-get options :purpose)
                  (plist-get options :clean))))
    (string-join
     (append
      (list
       (format "Created worktree session `%s' at %s"
               (plist-get result :branch)
               (plist-get result :directory)))
      (when-let* ((warnings (plist-get result :warnings)))
        (list (format "Warnings: %s"
                      (string-join warnings "; ")))))
     "\n")))


;;
;;; Slash command

(defun mevedel-cmd--worktree (args)
  "Run `/worktree' with ARGS."
  (let* ((tokens (mevedel-worktree--split-args args))
         (command (car tokens)))
    (cond
     ((or (null command) (string= command "status"))
      (mevedel-worktree-status-open)
      nil)
     ((string= command "list")
      (mevedel-worktree-list-open)
      nil)
     ((string= command "create")
      (mevedel-worktree--create args))
     (t
      (user-error "Unknown /worktree command: %s" command)))))

(defun mevedel-worktree-install-slash-command ()
  "Install `/worktree' into `mevedel-slash-commands'."
  (when (boundp 'mevedel-slash-commands)
    (setf (alist-get "worktree" mevedel-slash-commands nil nil #'equal)
          #'mevedel-cmd--worktree)))

(defun mevedel-worktree-uninstall-slash-command ()
  "Remove `/worktree' from `mevedel-slash-commands'."
  (when (boundp 'mevedel-slash-commands)
    (setf (alist-get "worktree" mevedel-slash-commands nil 'remove #'equal)
          nil)))

(provide 'mevedel-worktree)

;;; mevedel-worktree.el ends here
