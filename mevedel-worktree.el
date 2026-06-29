;;; mevedel-worktree.el -- Git worktree sessions -*- lexical-binding: t -*-

;;; Commentary:

;; Provides the `/worktree' local slash command.  The command reports the
;; current Git worktree state and can create a fresh linked worktree under
;; `.worktrees/' with a new mevedel session rooted there.

;;; Code:

(eval-when-compile
  (require 'cl-lib))

;; `mevedel'
(declare-function mevedel--default-session-name-for-directory
                  "mevedel" (workspace working-directory))
(declare-function mevedel--display-chat-buffer "mevedel" (chat-buffer))

;; `mevedel-chat'
(declare-function mevedel--chat-buffer
                  "mevedel-chat"
                  (session-name &optional create workspace working-directory))
(declare-function mevedel--insert-local-user-turn
                  "mevedel-chat"
                  (prompt &optional display-text kind hook-context
                          no-spinner))

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
(declare-function mevedel-workspace-root "mevedel-structs" (cl-x) t)
(defvar mevedel--current-request)
(defvar mevedel--session)


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
        (setq current (plist-put current :detached t)))))
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

(defun mevedel-worktree--collect-status (&optional context)
  "Collect read-only worktree status for CONTEXT."
  (let* ((context (or context (mevedel-worktree--current-context)))
         (session (plist-get context :session))
         (workspace (plist-get context :workspace))
         (directory (file-name-as-directory
                     (expand-file-name (plist-get context :directory))))
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

(defun mevedel-worktree--parse-create-options (tokens session)
  "Parse create command TOKENS for SESSION."
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
    (unless name
      (setq name
            (read-string
             "Worktree branch name: " nil nil
             (mevedel-worktree--default-branch-name session purpose))))
    (list :name name :purpose purpose :clean clean)))

(defun mevedel-worktree--validate-branch-name (name &optional directory)
  "Validate Git branch NAME for v1 worktree creation.

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
  (let* ((session-name (mevedel--default-session-name-for-directory
                        workspace worktree-directory))
         (chat-buffer (mevedel--chat-buffer
                       session-name t workspace worktree-directory)))
    (mevedel--display-chat-buffer chat-buffer)
    chat-buffer))

(defun mevedel-worktree--format-stub
    (source-session source-dir worktree-directory branch purpose warnings)
  "Return setup-context stub for a new worktree session."
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
  "Return shell cleanup commands for WORKTREE-DIRECTORY."
  (let* ((path (if (and workspace-root
                        (file-in-directory-p worktree-directory workspace-root))
                   (directory-file-name
                    (file-relative-name worktree-directory workspace-root))
                 (directory-file-name worktree-directory)))
         (quoted (shell-quote-argument path)))
    (format
     "Cleanup:\n  git worktree remove %s\n  rm -rf %s\n  git worktree prune"
     quoted quoted)))

(defun mevedel-worktree--create (args)
  "Create a new worktree session from slash command ARGS."
  (let* ((context (mevedel-worktree--current-context))
         (data-buffer (plist-get context :data-buffer))
         (session (plist-get context :session)))
    (unless (and data-buffer session)
      (user-error "'/worktree create' must run from a mevedel session"))
    (with-current-buffer data-buffer
      (when (bound-and-true-p mevedel--current-request)
        (user-error "A request is already active -- wait or abort first"))
      (let* ((options (mevedel-worktree--parse-create-options
                       (cdr (mevedel-worktree--split-args args))
                       session))
             (branch (plist-get options :name))
             (purpose (plist-get options :purpose))
             (clean (plist-get options :clean))
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
          (user-error "Cannot create a worktree from a submodule in v1"))
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
              (string-join
               (append
                (list
                 (format "Created worktree session `%s' at %s"
                         branch worktree-directory))
                (when warnings
                  (list (format "Warnings: %s"
                                (string-join warnings "; ")))))
               "\n"))
          (error
           (user-error
            "Created worktree at %s, but session setup failed: %s\n%s"
            worktree-directory
            (error-message-string err)
            (mevedel-worktree--cleanup-message
             worktree-directory workspace-root))))))))


;;
;;; Slash command

(defun mevedel-cmd--worktree (args)
  "Run `/worktree' with ARGS."
  (let* ((tokens (mevedel-worktree--split-args args))
         (command (car tokens)))
    (cond
     ((or (null command) (string= command "status"))
      (mevedel-worktree--format-status
       (mevedel-worktree--collect-status)))
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
