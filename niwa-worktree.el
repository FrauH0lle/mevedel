;;; niwa-worktree.el -- Git worktree sessions -*- lexical-binding: t -*-

;;; Commentary:

;; Provides the `/worktree' local slash command.  The command reports the
;; current Git worktree state and can create a fresh linked worktree under
;; `.worktrees/' with a new niwa session rooted there.  Goal execution reuses
;; the same creation interface for worktree-backed implementation handoffs.

;;; Code:

(require 'niwa-utilities)

(require 'niwa-report)

(eval-when-compile
  (require 'cl-lib)
  (require 'tabulated-list)
  (require 'transient))

(require 'niwa-cockpit)
(require 'transient)

(defcustom niwa-worktree-setup-enabled nil
  "Whether newly created worktrees run the configured setup command.
Directory-local values use the `niwa-worktree-setup-mode' key in the
source checkout's .dir-locals.el and Emacs's normal local-variable approval.
Enabling this executes project code."
  :type 'boolean
  :group 'niwa)

(defcustom niwa-worktree-setup-command nil
  "Program and arguments to run in a newly created worktree.
Only used when `niwa-worktree-setup-enabled' is non-nil.  This is an
argument list, not a shell command.  Setup must exit zero before a session
is opened; output is retained in `*niwa worktree setup*'."
  :type '(repeat string)
  :group 'niwa)

(defvar niwa-worktree-list--surface)

;; `niwa-chat'
(declare-function niwa--chat-buffer
                  "niwa-chat"
                  (session-name &optional create workspace working-directory))
(declare-function niwa--display-chat-buffer "niwa-chat" (chat-buffer))
(declare-function niwa--insert-local-user-turn
                  "niwa-chat"
                  (prompt &optional display-text kind hook-context
                          no-spinner))
(declare-function niwa--sessions-in-working-directory
                  "niwa-chat" (sessions working-directory))
(declare-function niwa--start-chat
                  "niwa-chat"
                  (workspace working-directory prompt-session
                             &optional directory-scoped))
(declare-function niwa--workspace-sessions "niwa-chat" (workspace))
(autoload 'niwa--chat-buffer "niwa-chat")
(autoload 'niwa--display-chat-buffer "niwa-chat")
(autoload 'niwa--insert-local-user-turn "niwa-chat")
(autoload 'niwa--sessions-in-working-directory "niwa-chat")
(autoload 'niwa--start-chat "niwa-chat")
(autoload 'niwa--workspace-sessions "niwa-chat")

;; `niwa-cockpit'
(declare-function niwa-cockpit-context-data-buffer
                  "niwa-cockpit" (&optional context))
(declare-function niwa-cockpit-context-session
                  "niwa-cockpit" (&optional context))
(declare-function niwa-cockpit-context-workspace
                  "niwa-cockpit" (&optional context))
(declare-function niwa-cockpit-current-context
                  "niwa-cockpit" ())
(declare-function niwa-cockpit-format-header
                  "niwa-cockpit" (name scope state))
(declare-function niwa-cockpit-open-surface
                  "niwa-cockpit" (surface &optional context))
(declare-function niwa-cockpit-quit "niwa-cockpit" (&optional label))
(declare-function niwa-cockpit-setup-tabulated-surface
                  "niwa-cockpit" (surface))
(declare-function niwa-cockpit-surface-context
                  "niwa-cockpit" (&optional surface))
(declare-function niwa-cockpit-surface-details
                  "niwa-cockpit" ())
(declare-function niwa-cockpit-surface-key-help-text
                  "niwa-cockpit" (&optional surface))
(declare-function niwa-cockpit-surface-quit
                  "niwa-cockpit" ())
(declare-function niwa-cockpit-surface-refresh
                  "niwa-cockpit" (&optional selected-id))
(declare-function niwa-cockpit-surface-selected
                  "niwa-cockpit" (&optional no-error))

;; `niwa-execution-target'
(declare-function niwa-execution-target-create
                  "niwa-execution-target" (workspace-root))
(declare-function niwa-execution-target-expand-path
                  "niwa-execution-target" (target path &optional directory))
(declare-function niwa-execution-target-native-path
                  "niwa-execution-target" (target path))
(declare-function niwa-execution-target-remote-p
                  "niwa-execution-target" (target))
(autoload 'niwa-execution-target-create "niwa-execution-target")
(autoload 'niwa-execution-target-expand-path "niwa-execution-target")
(autoload 'niwa-execution-target-native-path "niwa-execution-target")
(autoload 'niwa-execution-target-remote-p "niwa-execution-target")

;; `niwa-menu'
(declare-function niwa-menu--setup "niwa-menu" (prefix &optional scope))
(autoload 'niwa-menu--setup "niwa-menu")

;; `niwa-resource'
(declare-function niwa-resource-within-root-p
                  "niwa-resource" (path root))
(autoload 'niwa-resource-within-root-p "niwa-resource")

;; `niwa-session-artifacts'
(declare-function niwa-session-artifacts-save
                  "niwa-session-artifacts"
                  (session buffer &optional settled force))
(autoload 'niwa-session-artifacts-save "niwa-session-artifacts")

;; `niwa-session-persistence'
(declare-function niwa-session-persistence-list-sessions
                  "niwa-session-persistence" (workspace &optional cached))
(declare-function niwa-session-persistence-restore
                  "niwa-session-persistence"
                  (session-dir &optional lifecycle-source session-override
                               workspace))
(autoload 'niwa-session-persistence-list-sessions
  "niwa-session-persistence")
(autoload 'niwa-session-persistence-restore
  "niwa-session-persistence")

;; `niwa-skills'
(defvar niwa-slash-commands)

;; `niwa-structs'
(declare-function niwa-session-name "niwa-structs" (cl-x) t)
(declare-function niwa-session-session-id "niwa-structs" (cl-x) t)
(declare-function niwa-session-working-directory "niwa-structs" (cl-x) t)
(declare-function niwa-session-workspace "niwa-structs" (cl-x) t)
(declare-function niwa-workspace-p "niwa-structs" (cl-x))
(declare-function niwa-workspace-root "niwa-structs" (cl-x) t)
(defvar niwa--data-buffer)
(defvar niwa--current-request)
(defvar niwa--session)
(defvar niwa--view-buffer)

;; `niwa-utilities'
(declare-function niwa--file-in-directory-p
                  "niwa-utilities" (file directory))
(declare-function niwa--file-relative-name-or-absolute
                  "niwa-utilities" (file directory))
(autoload 'niwa--file-in-directory-p "niwa-utilities")
(autoload 'niwa--file-relative-name-or-absolute "niwa-utilities")

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

(defun niwa-worktree--execution-target (directory)
  "Return the execution target containing DIRECTORY."
  (niwa-execution-target-create
   (file-name-as-directory (expand-file-name directory))))

(defun niwa-worktree--native-path (path directory)
  "Return PATH in DIRECTORY's target-native path domain."
  (niwa-execution-target-native-path
   (niwa-worktree--execution-target directory) path))

(defun niwa-worktree--git-result (directory &rest args)
  "Run Git in DIRECTORY with ARGS and return (:exit N :output STRING)."
  (with-temp-buffer
    (let* ((default-directory (file-name-as-directory
                               (expand-file-name directory)))
           (target (niwa-worktree--execution-target default-directory))
           (process-environment
            (unless (niwa-execution-target-remote-p target)
              process-environment))
           (args (mapcar
                  (lambda (arg)
                    (if (stringp arg)
                        (niwa-execution-target-native-path target arg)
                      arg))
                  args)))
      (condition-case err
          (let ((exit (apply #'process-file
                             "git" nil (list t t) nil args)))
            (list :exit exit
                  :output (string-trim-right
                           (buffer-substring-no-properties
                            (point-min) (point-max)))))
        (file-missing
         (list :exit 127 :output (error-message-string err)))))))

(defun niwa-worktree--git-success-output (directory &rest args)
  "Return trimmed Git output for ARGS in DIRECTORY, or nil on failure."
  (let ((result (apply #'niwa-worktree--git-result directory args)))
    (when (eq 0 (plist-get result :exit))
      (string-trim (plist-get result :output)))))

(defun niwa-worktree-repository-root (directory)
  "Return DIRECTORY's Git top level, or nil when it is not in a repository.
The public probe for callers that need the repository root rather than the
directory they happen to be sitting in."
  (when-let* ((root (niwa-worktree--git-success-output
                     directory "rev-parse" "--show-toplevel")))
    ;; Git answers in the target's own path domain, so expand it back into
    ;; the caller's: plain `expand-file-name' leaves a remote answer looking
    ;; local.
    (file-name-as-directory
     (niwa-worktree--expand-git-path root directory))))

(defun niwa-worktree--git-exit (directory &rest args)
  "Return Git exit status for ARGS in DIRECTORY."
  (plist-get (apply #'niwa-worktree--git-result directory args) :exit))

(defun niwa-worktree-related-directory-p (directory workspace-root)
  "Return non-nil if DIRECTORY belongs to WORKSPACE-ROOT's Git worktrees.
Both paths must already name the same execution target.  Compare canonical
common Git directories and the registered top-level checkout, not branch names
or path prefixes.  Subdirectories are accepted, but unrelated nested
repositories and submodules have different common directories."
  (when-let* ((root (niwa-worktree-repository-root directory))
              (common (niwa-worktree--git-success-output
                       directory "rev-parse" "--path-format=absolute"
                       "--git-common-dir"))
              (workspace-common
               (niwa-worktree--git-success-output
                workspace-root "rev-parse" "--path-format=absolute"
                "--git-common-dir"))
              ((equal (file-truename
                       (niwa-worktree--expand-git-path common directory))
                      (file-truename
                       (niwa-worktree--expand-git-path
                        workspace-common workspace-root))))
              (listing (niwa-worktree--git-success-output
                        workspace-root "worktree" "list" "--porcelain" "-z")))
    (let ((root (file-name-as-directory (file-truename root))))
      (cl-some
       (lambda (field)
         (and (string-prefix-p "worktree " field)
              (equal root
                     (file-name-as-directory
                      (file-truename
                       (niwa-worktree--expand-git-path
                        (substring field 9) workspace-root))))))
       (split-string listing "\0" t)))))

(defun niwa-worktree--expand-git-path (path base-directory)
  "Expand Git PATH relative to BASE-DIRECTORY when needed."
  (when (and path (not (string-empty-p path)))
    (niwa-execution-target-expand-path
     (niwa-worktree--execution-target base-directory)
     path base-directory)))

(defun niwa-worktree--ensure-git (directory)
  "Require Git on DIRECTORY's execution target or signal a user error."
  (let ((result (niwa-worktree--git-result directory "--version")))
    (unless (eq 0 (plist-get result :exit))
      (user-error "Git is required on the session execution target for worktree workflows: %s"
                  (plist-get result :output)))))

(defun niwa-worktree--ensure-worktree (directory)
  "Require Git worktree support in target DIRECTORY or signal a user error."
  (let ((result (niwa-worktree--git-result
                 directory "worktree" "list" "--porcelain")))
    (unless (eq 0 (plist-get result :exit))
      (user-error "Git worktree support is required on the session execution target: %s"
                  (plist-get result :output)))))

(defun niwa-worktree--parse-worktree-list (output)
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

(defun niwa-worktree--current-context ()
  "Return a plist describing the current session command context."
  (let* ((session (and (boundp 'niwa--session) niwa--session))
         (workspace (and session (niwa-session-workspace session))))
    (list :data-buffer (and session (current-buffer))
          :session session
          :workspace workspace
          :directory (or (and session
                              (niwa-session-working-directory session))
                         default-directory))))

(defun niwa-worktree--ignore-state (directory)
  "Return `.worktrees/' ignore state for DIRECTORY."
  (pcase (niwa-worktree--git-exit directory "check-ignore" "-q"
                                     ".worktrees/")
    (0 'ignored)
    (1 'not-ignored)
    (_ 'unknown)))

(defun niwa-worktree--context-directory (context)
  "Return the normalized working directory for CONTEXT."
  (let* ((context (or context (niwa-worktree--current-context)))
         (data-buffer (plist-get context :data-buffer))
         (session (plist-get context :session)))
    (file-name-as-directory
     (expand-file-name
      (or (plist-get context :directory)
          (and session (niwa-session-working-directory session))
          (and (buffer-live-p data-buffer)
               (with-current-buffer data-buffer
                 default-directory))
          default-directory)))))

(defun niwa-worktree-collect-status (&optional context)
  "Collect read-only worktree status for CONTEXT."
  (let* ((context (or context (niwa-worktree--current-context)))
         (session (plist-get context :session))
         (workspace (or (plist-get context :workspace)
                        (and session (niwa-session-workspace session))))
         (directory (niwa-worktree--context-directory context))
         (workspace-root (and workspace
                              (file-name-as-directory
                               (expand-file-name
                                (niwa-workspace-root workspace)))))
         (repo-root (niwa-worktree-repository-root directory)))
    (if (not repo-root)
        (list :session session
              :workspace workspace
              :directory directory
              :repo-root nil
              :isolation 'not-git
              :ignore-state 'unknown
              :worktrees nil)
      (let* ((git-dir (niwa-worktree--expand-git-path
                       (niwa-worktree--git-success-output
                        directory "rev-parse" "--git-dir")
                       directory))
             (common-dir (niwa-worktree--expand-git-path
                          (niwa-worktree--git-success-output
                           directory "rev-parse" "--git-common-dir")
                          directory))
             (superproject (niwa-worktree--git-success-output
                            directory
                            "rev-parse" "--show-superproject-working-tree"))
             (branch (niwa-worktree--git-success-output
                      directory "branch" "--show-current"))
             (head (niwa-worktree--git-success-output
                    directory "rev-parse" "--short" "HEAD"))
             (status-output (niwa-worktree--git-success-output
                             directory "status" "--short"))
             (worktrees-output (niwa-worktree--git-success-output
                                directory "worktree" "list" "--porcelain"))
             (ignore-directory (or workspace-root repo-root))
             (isolation (cond
                         ((and superproject
                               (not (string-empty-p superproject)))
                          'submodule)
                         ;; By name.  Both come from one `rev-parse' on
                         ;; one repository and are expanded against the
                         ;; same directory, so git spells the same
                         ;; directory the same way in both; `file-equal-p'
                         ;; would resolve two truenames on the target to
                         ;; reach that answer, and this is reached from a
                         ;; renderer, where a nested remote call is
                         ;; refused outright.
                         ((not (equal (file-name-as-directory git-dir)
                                      (file-name-as-directory common-dir)))
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
              :ignore-state (niwa-worktree--ignore-state
                             ignore-directory)
              :worktrees
              (mapcar
               (lambda (entry)
                 (plist-put
                  entry :path
                  (niwa-worktree--expand-git-path
                   (plist-get entry :path) directory)))
               (niwa-worktree--parse-worktree-list
                worktrees-output)))))))

(defun niwa-worktree--isolation-label (isolation)
  "Return a user-facing label for ISOLATION."
  (pcase isolation
    ('normal-checkout "normal checkout")
    ('linked-worktree "linked worktree")
    ('submodule "submodule")
    ('not-git "not a Git repository")
    (_ "unknown")))

(defun niwa-worktree--ignore-label (state)
  "Return a user-facing label for `.worktrees/' ignore STATE."
  (pcase state
    ('ignored "ignored")
    ('not-ignored "not ignored")
    (_ "unknown")))

;;
;;; Surface

(defconst niwa-worktree-list-buffer-name "*niwa worktree*"
  "Name of the tabulated worktree list buffer.")

(defconst niwa-worktree-details-buffer-name "*niwa worktree details*"
  "Name of the worktree status info panel buffer.")

(defconst niwa-worktree-help-buffer-name "*niwa worktree help*"
  "Name of the worktree cockpit help buffer.")

(defun niwa-worktree-status--data-buffer (&optional context)
  "Return the data buffer that launched the worktree transient."
  (or (niwa-cockpit-context-data-buffer
       (or context (niwa-cockpit-current-context)))
      (niwa-diagnostic-user-error (niwa-cockpit-diagnostic-scope) "No live niwa session for this worktree status")))

(defun niwa-worktree--branch-head-label (branch head)
  "Return compact branch or HEAD label from BRANCH and HEAD."
  (cond
   ((and branch (not (string-empty-p branch))) branch)
   ((and head (not (string-empty-p head))) (format "detached at %s" head))
   (t "unavailable")))

(defun niwa-worktree-status-summary (&optional context)
  "Return a compact worktree status summary for CONTEXT."
  (let* ((context (or context (niwa-worktree--current-context)))
         (directory (niwa-worktree--context-directory context))
         (inside (niwa-worktree--git-success-output
                  directory "rev-parse" "--is-inside-work-tree")))
    (if (not (string= inside "true"))
        '(:state not-git :label "not-git")
      (let ((branch (niwa-worktree--git-success-output
                     directory "branch" "--show-current"))
            (head (niwa-worktree--git-success-output
                   directory "rev-parse" "--short" "HEAD")))
        (list :state 'git
              :label (cond
                      ((and branch (not (string-empty-p branch))) branch)
                      ((and head (not (string-empty-p head)))
                       (format "detached %s" head))
                      (t "not-git")))))))

(defun niwa-worktree-status--collect ()
  "Return the worktree status of the current cockpit context."
  (let* ((context (niwa-cockpit-current-context))
         (data-buffer (niwa-worktree-status--data-buffer context)))
    (with-current-buffer data-buffer
      (niwa-worktree-collect-status context))))

(defun niwa-worktree-status--description ()
  "Return the one-line worktree status, plus an alert when it is actionable.
The complete repository state lives in the worktree info panel."
  (let* ((status (niwa-worktree-status--collect))
         (worktrees (plist-get status :worktrees))
         (count (length worktrees))
         (ignore-state (plist-get status :ignore-state)))
    (concat
     (propertize "Worktree" 'face 'transient-heading)
     "  "
     (propertize (niwa-worktree--branch-head-label
                  (plist-get status :branch)
                  (plist-get status :head))
                 'face 'transient-value)
     " · "
     (niwa-worktree--isolation-label (plist-get status :isolation))
     (if (plist-get status :dirty-p)
         (concat " · " (propertize "dirty" 'face 'warning))
       "")
     (format " · %d worktree%s    " count (if (= count 1) "" "s"))
     (propertize (niwa-worktree--native-path
                  (plist-get status :directory)
                  (plist-get status :directory))
                 'face 'transient-inactive-value)
     (if (eq ignore-state 'ignored)
         ""
       (concat "\n"
               (propertize
                (format "! .worktrees %s"
                        (niwa-worktree--ignore-label ignore-state))
                'face 'warning))))))

(defun niwa-worktree-status--details-report ()
  "Return the complete worktree status as an information report."
  (let* ((status (niwa-worktree-status--collect))
         (session (plist-get status :session))
         (directory (plist-get status :directory)))
    (list :title "Worktree status"
          :subtitle (if session (niwa-session-name session) "none")
          :identity session :refresh #'niwa-worktree-status--details-report
          :sections
          (list
           (list :id 'context :title "Context"
                 :body (niwa-report-fields
                        (list "Repo" (if-let* ((root (plist-get status :repo-root)))
                                         (niwa-worktree--native-path root directory)
                                       "not a Git repository"))
                        (list "Session" (if session (niwa-session-name session) "none"))
                        (list "Directory" (niwa-worktree--native-path directory directory))))
           (list :id 'git :title "Git"
                 :body (niwa-report-fields
                        (list "Isolation" (niwa-worktree--isolation-label (plist-get status :isolation)))
                        (list "Branch" (niwa-worktree--branch-head-label (plist-get status :branch) (plist-get status :head)))
                        (list ".worktrees" (niwa-worktree--ignore-label (plist-get status :ignore-state)))
                        (list "Dirty" (if (plist-get status :dirty-p) "yes" "no"))
                        (list "Worktrees" (length (plist-get status :worktrees)))))))))

(defun niwa-worktree-status-details ()
  "Open the worktree status info panel."
  (interactive)
  (niwa-cockpit-show-help
   niwa-worktree-details-buffer-name
   (niwa-worktree-status--details-report)))

(defun niwa-worktree-status-create ()
  "Create a worktree from the status transient."
  (interactive)
  (let (result)
    (with-current-buffer (niwa-worktree-status--data-buffer)
      (setq result (niwa-cmd--worktree "create")))
    (when (stringp result)
      (message "%s" result))
    (with-current-buffer (niwa-worktree-status--data-buffer)
      (niwa-worktree-status-open))
    result))

(defun niwa-worktree-status-list ()
  "Open the tabulated worktree list from the status transient."
  (interactive)
  (let* ((context (niwa-cockpit-current-context))
         (data-buffer (niwa-worktree-status--data-buffer context)))
    (with-current-buffer data-buffer
      (niwa-worktree-list-open context))))

(defun niwa-worktree-status-refresh ()
  "Refresh the worktree status transient."
  (interactive)
  (with-current-buffer (niwa-worktree-status--data-buffer)
    (niwa-worktree-status-open)))

(defun niwa-worktree--help-report (&optional _context)
  "Return the complete worktree help report."
  (list :title "Worktree help"
        :sections
        (list
         (list :id 'status :title "Status keys"
               :body (concat "c  Create a linked worktree session\n"
                             "l  List Git worktrees\ng  Refresh status\n"
                             "i  Show full repository status\n?  Show this help\n"
                             "q  Close, or return to the invoking menu\n"))
         (list :id 'list :title "List keys"
               :body (niwa-cockpit-surface-key-help-text niwa-worktree-list--surface)))))

(defun niwa-worktree-status-help ()
  "Open worktree status help."
  (interactive)
  (niwa-cockpit-show-help niwa-worktree-help-buffer-name (niwa-worktree--help-report)))

(defun niwa-worktree-status-back ()
  "Close worktree status, returning to its invoking menu if any."
  (interactive)
  (when-let* ((return (plist-get (transient-scope) :parent-menu)))
    (funcall return)))

(transient-define-prefix niwa-worktree-status ()
  "Transient worktree status cockpit."
  [:description niwa-worktree-status--description
   ["Worktree"
    :pad-keys t
    ("c" "Create" niwa-worktree-status-create)
    ("l" "List" niwa-worktree-status-list)
    ("g" "Refresh" niwa-worktree-status-refresh)]
   ["Inspect"
    :pad-keys t
    ("i" "Worktree details" niwa-worktree-status-details)
    ("?" "Help" niwa-worktree-status-help)
    ("q" "Back" niwa-worktree-status-back)]]
  (interactive)
  (niwa-worktree-status-open))

(defun niwa-worktree-status-open ()
  "Open the transient worktree status cockpit."
  (interactive)
  (niwa-menu--setup
   'niwa-worktree-status
   (list :parent-menu
         (if (eq transient-current-command 'niwa-worktree-status)
             (plist-get (transient-scope) :parent-menu)
           (niwa-cockpit-menu-return-function)))))

(defun niwa-worktree-list--context ()
  "Return the current worktree list context."
  (niwa-cockpit-surface-context))

(defun niwa-worktree-list--context-data-buffer (&optional context)
  "Return the current worktree list data buffer."
  (let ((context (or context (niwa-worktree-list--context))))
    (niwa-cockpit-context-data-buffer context)))

(defun niwa-worktree-list--status (&optional context)
  "Return current worktree status from CONTEXT's data buffer."
  (let ((context (or context (niwa-worktree-list--context))))
    (with-current-buffer (niwa-worktree-list--context-data-buffer context)
      (niwa-worktree-collect-status context))))

(defun niwa-worktree-list--normalize-path (path)
  "Return PATH as an absolute directory name."
  (file-name-as-directory (expand-file-name path)))

(defun niwa-worktree-list--workspace-path (path workspace)
  "Return PATH using WORKSPACE's root spelling when possible."
  (let ((path (niwa-worktree-list--normalize-path path)))
    (if (not (niwa-workspace-p workspace))
        path
      (let* ((root (niwa-worktree-list--normalize-path
                    (niwa-workspace-root workspace)))
             (relative (niwa--file-relative-name-or-absolute
                        path root)))
        (if (file-name-absolute-p relative)
            path
          (niwa-worktree-list--normalize-path
           (expand-file-name relative root)))))))

(defun niwa-worktree-list--item-id (item)
  "Return stable row id for worktree ITEM."
  (plist-get item :path))

(defun niwa-worktree-list--branch-label (entry)
  "Return the list branch label for worktree ENTRY."
  (cond
   ((plist-get entry :branch))
   ((plist-get entry :detached)
    (format "detached %s" (or (plist-get entry :head) "HEAD")))
   ((plist-get entry :bare) "bare")
   ((plist-get entry :head))
   (t "unknown")))

(defun niwa-worktree-list--state-label (entry)
  "Return the normalized state label for worktree ENTRY."
  (cond
   ((plist-get entry :locked) "locked")
   ((plist-get entry :prunable) "prunable")
   ((plist-get entry :bare) "bare")
   ((plist-get entry :detached) "detached")
   ((plist-get entry :branch) "branch")
   (t "unknown")))

(defun niwa-worktree-list--current-label (current-p)
  "Return the Current column label for CURRENT-P."
  (if current-p "yes" ""))

(defun niwa-worktree-list--sessions (workspace path)
  "Return live session names in WORKSPACE whose cwd is PATH."
  (when workspace
    (mapcar
     #'car
     (niwa--sessions-in-working-directory
      (niwa--workspace-sessions workspace)
      path))))

(defun niwa-worktree-list--item (status entry)
  "Return a tabulated worktree item from STATUS and porcelain ENTRY."
  (let* ((workspace (plist-get status :workspace))
         (path (niwa-worktree-list--workspace-path
                (plist-get entry :path) workspace))
         (directory (niwa-worktree-list--workspace-path
                     (plist-get status :directory) workspace))
         (current (equal path directory))
         (sessions (niwa-worktree-list--sessions workspace path)))
    (list :path path
          :display-path
          (niwa-worktree--native-path
           path (plist-get status :directory))
          :branch (niwa-worktree-list--branch-label entry)
          :head (or (plist-get entry :head) "")
          :current current
          :state (niwa-worktree-list--state-label entry)
          :sessions sessions
          :entry entry)))

(defun niwa-worktree-list--items (status)
  "Return tabulated worktree items for STATUS."
  (mapcar (lambda (entry)
            (niwa-worktree-list--item status entry))
          (plist-get status :worktrees)))

(defun niwa-worktree-list--entry (item &optional _context)
  "Return a `tabulated-list-mode' row for worktree ITEM."
  (list
   (niwa-worktree-list--item-id item)
   (vector
    (or (plist-get item :display-path) (plist-get item :path))
    (plist-get item :branch)
    (plist-get item :head)
    (niwa-worktree-list--current-label (plist-get item :current))
    (plist-get item :state)
    (string-join (plist-get item :sessions) ", "))))

(defun niwa-worktree-list--collect (context)
  "Return worktree list items for CONTEXT."
  (niwa-worktree-list--items
   (niwa-worktree-list--status context)))

(defun niwa-worktree-list--header (items context)
  "Return the worktree list header line for ITEMS and CONTEXT."
  (let ((status (niwa-worktree-list--status context)))
    (niwa-cockpit-format-header
     "worktrees"
     (niwa-worktree--native-path
      (or (plist-get status :repo-root) (plist-get status :directory))
      (plist-get status :directory))
     (format "%d worktree%s%s"
             (length items)
             (if (= 1 (length items)) "" "s")
             (if (plist-get status :dirty-p)
                 (concat " · " (propertize "dirty" 'face 'warning))
               "")))))

(defun niwa-worktree-list-refresh ()
  "Refresh the tabulated worktree list."
  (interactive)
  (niwa-cockpit-surface-refresh))

(defun niwa-worktree-list--selected-item ()
  "Return the selected worktree item, or nil."
  (niwa-cockpit-surface-selected t))

(defun niwa-worktree-list--details-report (item &optional _context)
  "Return the information report for worktree ITEM."
  (let ((path (or (plist-get item :display-path) (plist-get item :path))))
    (list :title "Worktree" :subtitle path :identity path
          :sections
          (list
           (list :id 'worktree :title "Worktree"
                 :body (niwa-report-fields
                        (list "Path" path) (list "Branch" (plist-get item :branch))
                        (list "Head" (or (plist-get item :head) ""))))
           (list :id 'state :title "State"
                 :body (niwa-report-fields
                        (list "Current" (if (plist-get item :current) "yes" "no"))
                        (list "State" (plist-get item :state))
                        (list "Sessions" (or (string-join (plist-get item :sessions) ", ") ""))))))))

(defun niwa-worktree-list-details ()
  "Show details for the selected worktree row."
  (interactive)
  (niwa-cockpit-surface-details))

(defun niwa-worktree-list-open-selected ()
  "Open or switch to the selected worktree's niwa session."
  (interactive)
  (let* ((item (or (niwa-worktree-list--selected-item)
                   (niwa-diagnostic-user-error (niwa-cockpit-diagnostic-scope) "No worktree on this line")))
         (context (niwa-worktree-list--context))
         (path (plist-get item :path))
         (workspace (niwa-cockpit-context-workspace context)))
    (unless workspace
      (niwa-diagnostic-user-error (niwa-cockpit-diagnostic-scope) "No niwa workspace for selected worktree"))
    (niwa--start-chat workspace path nil t)))

(defun niwa-worktree-list-create ()
  "Create a worktree by delegating to `/worktree create'."
  (interactive)
  (let (result)
    (with-current-buffer (niwa-worktree-list--context-data-buffer)
      (setq result (niwa-cmd--worktree "create")))
    (when (stringp result)
      (message "%s" result))
    (niwa-worktree-list-refresh)
    result))

(defun niwa-worktree-list--delete (force)
  "Remove the selected worktree.
When FORCE is non-nil, pass `--force' to `git worktree remove'."
  (let* ((item (or (niwa-worktree-list--selected-item)
                   (niwa-diagnostic-user-error (niwa-cockpit-diagnostic-scope) "No worktree on this line")))
         (context (niwa-worktree-list--context))
         (workspace (or (niwa-cockpit-context-workspace context)
                        (niwa-diagnostic-user-error (niwa-cockpit-diagnostic-scope) "No niwa workspace for selected worktree")))
         (workspace-root (file-name-as-directory
                          (expand-file-name
                           (niwa-workspace-root workspace))))
         (worktrees-root (file-name-as-directory
                          (file-name-concat workspace-root ".worktrees")))
         (path (niwa-worktree-list--normalize-path
                (plist-get item :path)))
         (display-path
          (or (plist-get item :display-path)
              (niwa-worktree--native-path path workspace-root)))
         (entry (plist-get item :entry))
         (branch (plist-get entry :branch))
         (sessions (niwa-worktree-list--sessions workspace path)))
    (when (plist-get item :current)
      (niwa-diagnostic-user-error (niwa-cockpit-diagnostic-scope) "Cannot remove the current worktree"))
    (unless (niwa--file-in-directory-p path worktrees-root)
      (niwa-diagnostic-user-error (niwa-cockpit-diagnostic-scope) "Can only remove worktrees under %s"
                  (niwa-worktree--native-path
                   worktrees-root workspace-root)))
    (when (plist-get entry :bare)
      (niwa-diagnostic-user-error (niwa-cockpit-diagnostic-scope) "Cannot remove a bare worktree"))
    (when (plist-get entry :locked)
      (niwa-diagnostic-user-error (niwa-cockpit-diagnostic-scope) "Cannot remove a locked worktree"))
    (when sessions
      (niwa-diagnostic-user-error (niwa-cockpit-diagnostic-scope) "Worktree has live niwa sessions: %s"
                  (string-join sessions ", ")))
    (unless force
      (let ((status (niwa-worktree--git-result
                     path "status" "--porcelain")))
        (unless (eq 0 (plist-get status :exit))
          (niwa-diagnostic-user-error (niwa-cockpit-diagnostic-scope) "Git status failed: %s"
                      (plist-get status :output)))
        (unless (string-empty-p (plist-get status :output))
          (niwa-diagnostic-user-error (niwa-cockpit-diagnostic-scope) "Worktree has uncommitted changes; use D to force remove"))))
    (unless (yes-or-no-p
             (if force
                 (format "Force remove %s and discard uncommitted changes? "
                         display-path)
               (format "Remove worktree %s? " display-path)))
      (niwa-diagnostic-user-error (niwa-cockpit-diagnostic-scope) "Cancelled"))
    (let ((result (apply #'niwa-worktree--git-result
                         (niwa-worktree--context-directory context)
                         "worktree" "remove"
                         (append (when force (list "--force"))
                                 (list path)))))
      (unless (eq 0 (plist-get result :exit))
        (niwa-diagnostic-user-error (niwa-cockpit-diagnostic-scope) "Git worktree remove failed: %s"
                    (plist-get result :output))))
    (niwa-worktree-list-refresh)
    (message "niwa: removed worktree %s; %s"
             display-path
             (if branch
                 (format "branch %s was left intact" branch)
               "no branch was deleted"))))

(defun niwa-worktree-list-delete ()
  "Remove the selected clean worktree."
  (interactive)
  (niwa-worktree-list--delete nil))

(defun niwa-worktree-list-force-delete ()
  "Force-remove the selected worktree."
  (interactive)
  (niwa-worktree-list--delete t))

(defun niwa-worktree-list-help ()
  "Open worktree list help."
  (interactive)
  (niwa-worktree-status-help))

(defun niwa-worktree-list-quit ()
  "Close the worktree list and restore the invoking layout."
  (interactive)
  (niwa-cockpit-quit "worktree list"))

(defconst niwa-worktree-list--surface
  `(:buffer-name ,niwa-worktree-list-buffer-name
    :label "worktree list"
    :row-label "worktree"
    :mode niwa-worktree-list-mode
    :format [("Path" 36 t)
             ("Branch" 20 t)
             ("Head" 10 t)
             ("Current" 8 t)
             ("State" 10 t)
             ("Sessions" 0 t)]
    :sort-key ("Path" . nil)
    :collect niwa-worktree-list--collect
    :entry niwa-worktree-list--entry
    :header niwa-worktree-list--header
    :details niwa-worktree-list--details-report
    :details-buffer "*niwa worktree details*"
    :help-function niwa-worktree--help-report
    :help-buffer ,niwa-worktree-help-buffer-name
    :keys (("c" "Create a linked worktree session"
            niwa-worktree-list-create)
           ("o" "Open selected worktree session"
            niwa-worktree-list-open-selected)
           ("d" "Delete selected worktree"
            niwa-worktree-list-delete)
           ("D" "Force-delete selected worktree"
            niwa-worktree-list-force-delete)))
  "Cockpit surface spec for the worktree list.")

(define-derived-mode niwa-worktree-list-mode tabulated-list-mode
  "niwa-worktree"
  "Major mode for the tabulated worktree list."
  (niwa-cockpit-setup-tabulated-surface
   niwa-worktree-list--surface))

(defun niwa-worktree-list-open (&optional context)
  "Open the tabulated worktree list for CONTEXT."
  (let ((context (or context (niwa-cockpit-current-context))))
    (niwa-cockpit-open-surface niwa-worktree-list--surface context)))


;;
;;; Create

(defun niwa-worktree--split-args (args)
  "Split ARGS as slash-command arguments."
  (condition-case err
      (split-string-and-unquote (or args ""))
    (error
     (user-error "Could not parse /worktree arguments: %s"
                 (error-message-string err)))))

(defun niwa-worktree--slugify (text)
  "Return a short branch-name slug for TEXT."
  (let* ((downcased (downcase (or text "")))
         (slug (replace-regexp-in-string "[^a-z0-9._-]+" "-" downcased))
         (slug (string-trim slug "-+" "-+")))
    (if (string-empty-p slug)
        "session"
      (substring slug 0 (min (length slug) 48)))))

(defun niwa-worktree-default-branch-name (session purpose)
  "Return default branch name for SESSION and optional PURPOSE."
  (format "worktree/%s"
          (niwa-worktree--slugify
           (or purpose
               (and session (niwa-session-name session))
               "session"))))

(defun niwa-worktree--parse-create-options (tokens)
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

(defun niwa-worktree-validate-branch-name (name &optional directory)
  "Validate Git branch NAME for worktree creation.

When DIRECTORY is non-nil, use Git's refname checker for the full
branch-name grammar before any mutating Git command runs."
  (when (or (not (stringp name)) (string-empty-p name))
    (user-error "Branch name is required"))
  (when directory
    (niwa-worktree--ensure-git directory)
    (niwa-worktree--ensure-worktree directory)
    (pcase (niwa-worktree--git-exit
            directory "check-ref-format" "--branch" name)
      (0 nil)
      (_ (user-error "Invalid branch name: %s" name)))))

(defun niwa-worktree--branch-leaf (branch)
  "Return the final path component for BRANCH."
  (file-name-nondirectory (directory-file-name branch)))

(defun niwa-worktree--validate-destination (workspace-root directory)
  "Return DIRECTORY after proving it is safe under WORKSPACE-ROOT."
  (let* ((workspace-root
          (file-name-as-directory (expand-file-name workspace-root)))
         (worktrees-directory
          (file-name-as-directory
           (file-name-concat workspace-root ".worktrees")))
         (directory (file-name-as-directory (expand-file-name directory)))
         (parent (file-name-directory (directory-file-name directory))))
    (unless (equal parent worktrees-directory)
      (user-error "Worktree destination is outside the workspace .worktrees directory: %s"
                  directory))
    ;; Containment is an authorization answer; the shared owner scans
    ;; every component for symlinks, proves truename containment, and
    ;; refuses to answer from a stale TRAMP cache -- the hand-rolled
    ;; check here did neither of the last two reliably.
    (unless (niwa-resource-within-root-p
             (directory-file-name directory) workspace-root)
      (user-error "Worktree destination resolves outside the workspace: %s"
                  directory))
    directory))

(defun niwa-worktree-fork-preflight (session)
  "Return immutable Git context for a Worktree Fork of SESSION."
  (let* ((source-directory
          (file-name-as-directory
           (expand-file-name
            (niwa-session-working-directory session))))
         (inside
          (progn
            (niwa-worktree--ensure-git source-directory)
            (niwa-worktree--git-success-output
             source-directory "rev-parse" "--is-inside-work-tree")))
         (repo-root
          (niwa-worktree--git-success-output
           source-directory "rev-parse" "--show-toplevel"))
         (base-commit
          (niwa-worktree--git-success-output
           source-directory "rev-parse" "HEAD"))
         (common-dir
          (niwa-worktree--git-success-output
           source-directory "rev-parse" "--git-common-dir"))
         (superproject
          (niwa-worktree--git-success-output
           source-directory "rev-parse" "--show-superproject-working-tree")))
    (unless (and (string= inside "true") repo-root)
      (user-error
       "Worktree Fork requires a supported Git repository; use Conversation Fork to share the current files"))
    (niwa-worktree--ensure-worktree source-directory)
    (unless base-commit
      (user-error "Worktree Fork requires an existing HEAD commit"))
    (when (and superproject (not (string-empty-p superproject)))
      (user-error "Worktree Fork does not support submodules"))
    (list
     :source-directory source-directory
     :source-root
     (file-name-as-directory
      (niwa-worktree--expand-git-path repo-root source-directory))
     :workspace-root
     (file-name-as-directory
      (expand-file-name
       (niwa-workspace-root (niwa-session-workspace session))))
     :common-git-dir
     (niwa-worktree--expand-git-path common-dir source-directory)
     :base-commit base-commit)))

(defun niwa-worktree-fork-reservation (session &optional preflight branch)
  "Return the first free branch and directory reservation for SESSION.
Use PREFLIGHT when it already describes SESSION's immutable Git state.
BRANCH, when non-nil, is the exact branch to reserve, at the deterministic
directory `niwa-worktree-session-directory' would give it; a taken branch
or directory signals instead of choosing another."
  (let* ((preflight (or preflight
                        (niwa-worktree-fork-preflight session)))
         (source-directory (plist-get preflight :source-directory))
         (workspace-root (plist-get preflight :workspace-root))
         (slug (niwa-worktree--slugify
                (niwa-session-name session)))
         (number 1)
         directory)
    (cl-flet ((taken-p ()
                (or (file-exists-p directory)
                    (eq 0
                        (niwa-worktree--git-exit
                         source-directory "show-ref" "--verify" "--quiet"
                         (concat "refs/heads/" branch))))))
      (if branch
          (progn
            (niwa-worktree-validate-branch-name branch source-directory)
            (setq directory
                  (file-name-as-directory
                   (file-name-concat workspace-root ".worktrees"
                                     (niwa-worktree--branch-leaf branch))))
            (niwa-worktree--validate-destination workspace-root directory)
            (when (taken-p)
              (user-error "Worktree branch or directory already exists: %s, %s"
                          branch
                          (niwa-worktree--native-path
                           directory source-directory))))
        (while
            (progn
              (setq branch (format "worktree/%s-fork-%d" slug number)
                    directory
                    (file-name-as-directory
                     (file-name-concat
                      workspace-root ".worktrees"
                      (format "%s-fork-%d" slug number))))
              (niwa-worktree--validate-destination
               workspace-root directory)
              (when (taken-p)
                (cl-incf number)
                t)))))
    (append
     (copy-sequence preflight)
     (list
      :branch branch
      :directory directory
      :cleanup-command
      (let ((native-source
             (niwa-worktree--native-path
              source-directory source-directory))
            (native-directory
             (niwa-worktree--native-path directory source-directory)))
        (format
         "git -C %s worktree remove --force %s && git -C %s branch -D %s"
         (shell-quote-argument native-source)
         (shell-quote-argument native-directory)
         (shell-quote-argument native-source)
         (shell-quote-argument branch)))))))

(defun niwa-worktree-fork-create (reservation)
  "Create the linked worktree described by RESERVATION."
  (let* ((source-directory (plist-get reservation :source-directory))
         (workspace-root (plist-get reservation :workspace-root))
         (directory (plist-get reservation :directory))
         (branch (plist-get reservation :branch))
         (base-commit (plist-get reservation :base-commit))
         (native-directory
          (niwa-worktree--native-path directory source-directory)))
    (niwa-worktree--ensure-git source-directory)
    (niwa-worktree--ensure-worktree source-directory)
    (niwa-worktree--validate-destination workspace-root directory)
    (make-directory (file-name-directory
                     (directory-file-name directory))
                    t)
    (niwa-worktree--ensure-local-exclude
     (plist-get reservation :common-git-dir))
    (niwa-worktree--validate-destination workspace-root directory)
    (let ((result
           (niwa-worktree--git-result
            source-directory
            "worktree" "add" "-b" branch directory base-commit)))
      (unless (eq 0 (plist-get result :exit))
        (user-error "Git worktree add failed for %s at %s: %s"
                    branch
                    native-directory
                    (plist-get result :output))))
    (niwa-worktree--setup source-directory directory)
    reservation))

(defun niwa-worktree-fork-validate-reservation (session reservation)
  "Validate that SESSION can still create its exact RESERVATION."
  (let ((source-directory (plist-get reservation :source-directory))
        (workspace-root (plist-get reservation :workspace-root))
        (directory (plist-get reservation :directory))
        (branch (plist-get reservation :branch))
        (cleanup-command (plist-get reservation :cleanup-command)))
    (unless (and (stringp source-directory)
                 (stringp workspace-root)
                 (stringp directory)
                 (stringp branch)
                 (stringp cleanup-command))
      (user-error "Worktree Fork reservation is incomplete; cancel and arm it again"))
    (niwa-worktree--validate-destination workspace-root directory)
    (when
        (or (file-exists-p directory)
            (eq 0
                (niwa-worktree--git-exit
                 source-directory "show-ref" "--verify" "--quiet"
                 (concat "refs/heads/" branch))))
      (user-error
       "Reserved Worktree Fork artifacts already exist: branch %s, directory %s. Cleanup: %s"
       branch
       (niwa-worktree--native-path directory source-directory)
       cleanup-command))
    (let ((current (niwa-worktree-fork-preflight session)))
      (dolist (key '(:source-directory :source-root :workspace-root
                     :common-git-dir :base-commit))
        (unless (equal (plist-get reservation key)
                       (plist-get current key))
          (user-error
           "Worktree Fork source changed after arming; cancel and arm it again"))))
    reservation))

(defun niwa-worktree--ensure-local-exclude (common-git-dir)
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

(define-derived-mode niwa-worktree-setup-mode fundamental-mode "Niwa-Setup"
  "Major mode for reading project worktree setup configuration.
Use `niwa-worktree-setup-mode' as the .dir-locals.el key, not nil, to
keep command-executing setup locals out of ordinary file buffers.")

(defun niwa-worktree--setup (source-root directory)
  "Run explicitly authorized project setup in new worktree DIRECTORY.
Read directory locals from SOURCE-ROOT in `niwa-worktree-setup-mode'.
Use that mode as the .dir-locals.el key to scope configuration to this
lookup.  Neither setup option is a safe local variable: an untrusted
project must not authorize its own code.
Leave the worktree intact on failure, with diagnostics and a retry path."
  (with-temp-buffer
    (niwa-worktree-setup-mode)
    (setq default-directory (file-name-as-directory source-root))
    (hack-dir-local-variables-non-file-buffer)
    (when niwa-worktree-setup-enabled
      (unless (and (consp niwa-worktree-setup-command)
                   (cl-every #'stringp niwa-worktree-setup-command))
        (user-error "Worktree created at %s, but setup command is not configured"
                    directory))
      (let* ((command niwa-worktree-setup-command)
             (default-directory (file-name-as-directory directory))
             (target (niwa-worktree--execution-target directory))
             (process-environment
              (unless (niwa-execution-target-remote-p target)
                process-environment))
             (output (get-buffer-create "*niwa worktree setup*"))
             exit)
        (with-current-buffer output
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert (format "Worktree: %s\nCommand: %S\n\n"
                            directory command))
            (let ((default-directory (file-name-as-directory directory)))
              (condition-case err
                  (setq exit (apply #'process-file (car command) nil
                                    (list t t) nil (cdr command)))
                (file-error
                 (setq exit 127)
                 (insert (error-message-string err)))))
            (special-mode)))
        (unless (eq exit 0)
          (user-error
           "Worktree created at %s, but setup failed (%s); see %s. Fix the cause and rerun %S in that worktree"
           directory exit (buffer-name output) command))
        (message "Worktree setup ready; report in %s" (buffer-name output))))))

(defun niwa-worktree--open-session (workspace worktree-directory)
  "Open a new niwa session for WORKSPACE at WORKTREE-DIRECTORY."
  (let* ((live (niwa--sessions-in-working-directory
                (niwa--workspace-sessions workspace)
                worktree-directory))
         (chat-buffer
          (if live
              (if (cdr live)
                  (error "Multiple sessions use Worktree directory: %s"
                         worktree-directory)
                (cdar live))
            (niwa--chat-buffer nil t workspace worktree-directory))))
    (niwa--display-chat-buffer chat-buffer)
    chat-buffer))

(defun niwa-worktree-session-directory (branch)
  "Return the deterministic Worktree session directory for BRANCH."
  (let* ((context (niwa-worktree--current-context))
         (session (plist-get context :session))
         (source-directory
          (and session
               (file-name-as-directory
                (expand-file-name
                 (niwa-session-working-directory session)))))
         (workspace (and session (niwa-session-workspace session))))
    (unless (and session workspace source-directory)
      (user-error "Worktree creation must run from a niwa session"))
    (niwa-worktree-validate-branch-name branch source-directory)
    (let* ((workspace-root
            (file-name-as-directory
             (expand-file-name (niwa-workspace-root workspace))))
           (directory
            (file-name-as-directory
             (file-name-concat
              workspace-root ".worktrees"
              (niwa-worktree--branch-leaf branch)))))
      (niwa-worktree--validate-destination workspace-root directory))))

(defun niwa-worktree--format-stub
    (source-session source-dir worktree-directory branch purpose warnings)
  "Return setup-context stub for SOURCE-SESSION.
SOURCE-DIR, WORKTREE-DIRECTORY, BRANCH, PURPOSE and WARNINGS describe the
new session."
  (let ((native-source
         (niwa-worktree--native-path source-dir source-dir))
        (native-worktree
         (niwa-worktree--native-path worktree-directory source-dir)))
    (string-join
     (append
      (list
       "Setup context for this worktree session."
       ""
       (format "Created from session: %s"
               (or source-session "unknown"))
       (format "Source directory: %s" native-source)
       (format "Worktree directory: %s" native-worktree)
       (format "Branch: %s" branch))
      (when (and purpose (not (string-empty-p purpose)))
        (list (format "Purpose: %s" purpose)))
      (when warnings
        (append '("" "Warnings:")
                (mapcar (lambda (warning) (format "- %s" warning))
                        warnings)))
      '("" "This is setup context only. Wait for the user's next prompt before taking action."))
     "\n")))

(defun niwa-worktree--save-stub (chat-buffer)
  "Persist CHAT-BUFFER after a setup stub was inserted."
  (with-current-buffer chat-buffer
    (condition-case err
        (niwa-session-artifacts-save niwa--session chat-buffer)
      (error
       (niwa-diagnostic-warning (niwa-diagnostic-scope chat-buffer)
        'niwa
        (format "%s (could not save worktree setup context)"
                (error-message-string err))
        :warning)))))

(defun niwa-worktree--cleanup-message
    (worktree-directory workspace-root)
  "Return shell cleanup commands for WORKTREE-DIRECTORY under WORKSPACE-ROOT."
  (let* ((path (if (and workspace-root
                        (file-in-directory-p worktree-directory workspace-root))
                   (directory-file-name
                    (file-relative-name worktree-directory workspace-root))
                 (directory-file-name
                  (niwa-worktree--native-path
                   worktree-directory worktree-directory))))
         (quoted (shell-quote-argument path)))
    (format
     "Cleanup:\n  git worktree remove %s\n  rm -rf %s\n  git worktree prune"
     quoted quoted)))

(defun niwa-worktree-create-session (&optional branch purpose clean)
  "Create and return a new worktree session from the current session.
Prompt for BRANCH when it is nil.  PURPOSE is included in the setup
context.  When CLEAN is non-nil, omit that context.
The return value is a plist with `:buffer', `:branch', `:directory', and
`:warnings'."
  (let* ((context (niwa-worktree--current-context))
         (data-buffer (plist-get context :data-buffer))
         (session (plist-get context :session)))
    (unless (and data-buffer session)
      (user-error "Worktree creation must run from a niwa session"))
    (with-current-buffer data-buffer
      (when (bound-and-true-p niwa--current-request)
        (user-error "A request is already active -- wait or abort first"))
      (let* ((branch
              (or branch
                  (read-string
                   "Worktree branch name: " nil nil
                   (niwa-worktree-default-branch-name session purpose))))
             (workspace (niwa-session-workspace session))
             (workspace-root (file-name-as-directory
                              (expand-file-name
                               (niwa-workspace-root workspace))))
             (source-directory (file-name-as-directory
                                (expand-file-name
                                 (niwa-session-working-directory
                                  session))))
             (status
              (progn
                (niwa-worktree--ensure-git source-directory)
                (niwa-worktree-collect-status context)))
             (repo-root (plist-get status :repo-root))
             (worktree-directory
              (niwa-worktree-session-directory branch))
             (worktrees-dir
              (file-name-directory
               (directory-file-name worktree-directory)))
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
        (progn
          (when (file-exists-p worktree-directory)
            (user-error "Worktree destination already exists: %s"
                        (niwa-worktree--native-path
                         worktree-directory source-directory)))
          (niwa-worktree--validate-destination
           workspace-root worktree-directory)
          (make-directory worktrees-dir t)
          (niwa-worktree--ensure-local-exclude
           (plist-get status :git-common-dir))
          (niwa-worktree--validate-destination
           workspace-root worktree-directory)
          (let ((result (niwa-worktree--git-result
                         source-directory
                         "worktree" "add" "-b" branch
                         worktree-directory)))
            (unless (eq 0 (plist-get result :exit))
              (user-error "Git worktree add failed: %s"
                          (plist-get result :output))))
          (condition-case err
              (let* ((_setup (niwa-worktree--setup
                              source-directory worktree-directory))
                     (chat-buffer
                     (niwa-worktree--open-session
                      workspace worktree-directory)))
                (unless clean
                  (with-current-buffer chat-buffer
                    (niwa--insert-local-user-turn
                     (niwa-worktree--format-stub
                      (niwa-session-name session)
                      source-directory
                      worktree-directory
                      branch
                      purpose
                      warnings)
                     nil 'worktree nil t))
                  (niwa-worktree--save-stub chat-buffer))
                (list :buffer chat-buffer
                      :branch branch
                      :directory worktree-directory
                      :warnings warnings))
            (error
             (user-error
              "Created worktree at %s, but session setup failed: %s\n%s"
              (niwa-worktree--native-path
               worktree-directory source-directory)
              (error-message-string err)
              (niwa-worktree--cleanup-message
               worktree-directory workspace-root)))))))))

(defun niwa-worktree--create (tokens)
  "Create a new worktree session from parsed option TOKENS."
  (let* ((options (niwa-worktree--parse-create-options tokens))
         (result (niwa-worktree-create-session
                  (plist-get options :name)
                  (plist-get options :purpose)
                  (plist-get options :clean))))
    (string-join
     (append
      (list
       (format "Created worktree session `%s' at %s"
               (plist-get result :branch)
               (niwa-worktree--native-path
                (plist-get result :directory)
                (plist-get result :directory))))
      (when-let* ((warnings (plist-get result :warnings)))
        (list (format "Warnings: %s"
                      (string-join warnings "; ")))))
     "\n")))


;;
;;; Slash command

(defun niwa-cmd--worktree (args)
  "Run `/worktree' with ARGS."
  (let* ((tokens (niwa-worktree--split-args args))
         (command (car tokens)))
    (cond
     ((or (null command) (string= command "status"))
      (niwa-worktree-status-open)
      nil)
     ((string= command "list")
      (niwa-worktree-list-open)
      nil)
     ((string= command "create")
      (niwa-worktree--create (cdr tokens)))
     (t
      (user-error "Unknown /worktree command: %s" command)))))

(defun niwa-worktree-install-slash-command ()
  "Install `/worktree' into `niwa-slash-commands'."
  (when (boundp 'niwa-slash-commands)
    (setf (alist-get "worktree" niwa-slash-commands nil nil #'equal)
          #'niwa-cmd--worktree)))

(defun niwa-worktree-uninstall-slash-command ()
  "Remove `/worktree' from `niwa-slash-commands'."
  (when (boundp 'niwa-slash-commands)
    (setf (alist-get "worktree" niwa-slash-commands nil 'remove #'equal)
          nil)))

(provide 'niwa-worktree)

;;; niwa-worktree.el ends here
