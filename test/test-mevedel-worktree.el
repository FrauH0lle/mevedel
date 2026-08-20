;;; test-mevedel-worktree.el --- Tests for worktree sessions -*- lexical-binding: t -*-

;;; Commentary:

;; Focused command-seam coverage for `/worktree'.

;;; Code:

(require 'mevedel-worktree)
(require 'mevedel)
(require 'mevedel-chat)
(require 'mevedel-cockpit)
(require 'mevedel-mentions)
(require 'mevedel-session-persistence)
(require 'mevedel-view)
(require 'tabulated-list)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

;; `mevedel-cockpit'
(defvar mevedel-cockpit--context)

;; `mevedel-skills'
(defvar mevedel-slash-commands)

(defun mevedel-worktree-test--workspace (root)
  "Return a test workspace rooted at ROOT."
  (mevedel-workspace--create
   :type 'project
   :id root
   :root root
   :name "worktree-test"
   :file-cache (mevedel-file-cache--create
                :table (make-hash-table :test #'equal)
                :order nil
                :total-bytes 0)))

(defun mevedel-worktree-test--git-result (output &optional exit)
  "Return a mocked Git result with OUTPUT and EXIT."
  (list :exit (or exit 0) :output output))

(defun mevedel-worktree-test--base-response (root args)
  "Return a base mocked Git response for ROOT and ARGS."
  (let ((git-dir (file-name-concat root ".git")))
    (cond
     ((equal args '("rev-parse" "--is-inside-work-tree"))
      (mevedel-worktree-test--git-result "true"))
     ((equal args '("rev-parse" "--show-toplevel"))
      (mevedel-worktree-test--git-result root))
     ((equal args '("rev-parse" "--git-dir"))
      (mevedel-worktree-test--git-result git-dir))
     ((equal args '("rev-parse" "--git-common-dir"))
      (mevedel-worktree-test--git-result git-dir))
     ((equal args '("rev-parse" "--show-superproject-working-tree"))
      (mevedel-worktree-test--git-result ""))
     ((equal args '("branch" "--show-current"))
      (mevedel-worktree-test--git-result "main"))
     ((equal args '("rev-parse" "--short" "HEAD"))
      (mevedel-worktree-test--git-result "abc123"))
     ((equal args '("status" "--short"))
      (mevedel-worktree-test--git-result ""))
     ((equal args '("worktree" "list" "--porcelain"))
      (mevedel-worktree-test--git-result
       (format "worktree %s\nHEAD abc123\nbranch refs/heads/main\n" root)))
     ((equal args '("check-ignore" "-q" ".worktrees/"))
      (mevedel-worktree-test--git-result "" 1))
     ((equal (list (nth 0 args) (nth 1 args))
             '("check-ref-format" "--branch"))
      (mevedel-worktree-test--git-result ""))
     ((equal (list (nth 0 args) (nth 1 args)) '("worktree" "add"))
      (mevedel-worktree-test--git-result "ok"))
     (t
      (mevedel-worktree-test--git-result "")))))

(defun mevedel-worktree-test--git-ok (directory &rest args)
  "Run Git ARGS in DIRECTORY and signal on failure."
  (let ((result (apply #'mevedel-worktree--git-result directory args)))
    (unless (eq 0 (plist-get result :exit))
      (error "Git failed: %s" (plist-get result :output)))
    result))

(defun mevedel-worktree-test--init-repo (root)
  "Initialize a real Git repository rooted at ROOT."
  (mevedel-worktree-test--git-ok root "init")
  (mevedel-worktree-test--git-ok root "config" "user.email"
                                  "mevedel@example.invalid")
  (mevedel-worktree-test--git-ok root "config" "user.name" "Mevedel Test")
  (with-temp-file (file-name-concat root "file.txt")
    (insert "base\n"))
  (mevedel-worktree-test--git-ok root "add" "file.txt")
  (mevedel-worktree-test--git-ok root "commit" "-m" "init"))

(defun mevedel-worktree-test--open-real-list
    (root workspace data-buffer)
  "Open a worktree list backed by real Git state under ROOT."
  (with-current-buffer data-buffer
    (setq-local default-directory root))
  (mevedel-worktree-list-open
   (list :view-buffer data-buffer
         :data-buffer data-buffer
         :origin-buffer data-buffer
         :session nil
         :workspace workspace)))

(defmacro mevedel-worktree-test--with-session (root &rest body)
  "Run BODY in a temp buffer with a mevedel session rooted at ROOT."
  (declare (indent 1))
  `(let* ((workspace (mevedel-worktree-test--workspace ,root))
          (session (mevedel-session-create "main" workspace ,root)))
     (make-directory (file-name-concat ,root ".git") t)
     (with-temp-buffer
       (setq-local mevedel--session session)
       (setq-local default-directory (file-name-as-directory ,root))
       ,@body)))

(defun mevedel-worktree-test--cleanup-surfaces (&rest buffers)
  "Kill worktree cockpit test buffers and BUFFERS."
  (dolist (name (list mevedel-worktree-list-buffer-name
                      "*mevedel worktree details*"
                      mevedel-worktree-help-buffer-name))
    (when (get-buffer name)
      (kill-buffer name)))
  (dolist (buffer buffers)
    (when (buffer-live-p buffer)
      (kill-buffer buffer))))

(defun mevedel-worktree-test--status (root &optional worktrees)
  "Return a worktree status plist rooted at ROOT with WORKTREES."
  (let ((root (file-name-as-directory (expand-file-name root))))
    (list :directory root
          :repo-root root
          :workspace nil
          :session nil
          :isolation 'normal-checkout
          :branch "main"
          :head "abc123"
          :ignore-state 'ignored
          :dirty-p nil
          :worktrees
          (or worktrees
              (list (list :path root
                          :head "abc123"
                          :branch "main"))))))

(defun mevedel-worktree-test--context
    (status data-buffer &optional view-buffer origin-buffer)
  "Return a cockpit context for STATUS owned by DATA-BUFFER."
  (list :view-buffer (or view-buffer data-buffer)
        :data-buffer data-buffer
        :origin-buffer (or origin-buffer data-buffer)
        :session (plist-get status :session)
        :workspace (plist-get status :workspace)))

(defun mevedel-worktree-test--install-context
    (status data-buffer &optional view-buffer origin-buffer)
  "Install a cockpit context for STATUS in DATA-BUFFER."
  (with-current-buffer data-buffer
    (setq-local mevedel-cockpit--context
                (mevedel-worktree-test--context
                 status data-buffer view-buffer origin-buffer))))

(defun mevedel-worktree-test--open-list
    (status data-buffer &optional view-buffer)
  "Open a worktree list for STATUS owned by DATA-BUFFER."
  (let ((view-buffer (or view-buffer data-buffer)))
    (with-current-buffer data-buffer
      (setq-local mevedel--view-buffer view-buffer)
      (cl-letf (((symbol-function 'mevedel-worktree--collect-status)
                 (lambda (&optional _context) status))
                ((symbol-function 'mevedel-worktree-list--sessions)
                 (lambda (_workspace path)
                   (when (plist-get status :workspace)
                     (if (equal path (plist-get status :directory))
                         '("main")
                       '("side"))))))
        (mevedel-worktree-list-open
         (mevedel-worktree-test--context
          status data-buffer view-buffer data-buffer))))))


;;
;;; Installation

(mevedel-deftest mevedel-worktree-install-slash-command
  (:doc "installs and uninstalls only the worktree slash command")
  (let ((old (and (boundp 'mevedel-slash-commands)
                  mevedel-slash-commands)))
    (unwind-protect
        (progn
          (setq mevedel-slash-commands '(("tokens" . ignore)))
          (mevedel-worktree-install-slash-command)
          (should (eq #'mevedel-cmd--worktree
                      (cdr (assoc "worktree" mevedel-slash-commands))))
          (mevedel-worktree-uninstall-slash-command)
          (should-not (assoc "worktree" mevedel-slash-commands))
          (should (assoc "tokens" mevedel-slash-commands)))
      (setq mevedel-slash-commands old))))


;;
;;; Git helpers

(mevedel-deftest mevedel-worktree--git-result ()
  ,test
  (test)

  :doc "rejects a Git path argument naming another execution target"
  (let* ((root (file-name-as-directory
                (make-temp-file "mevedel-worktree-target-" t)))
         (source (format "/mevedelmock:first:%s" root))
         (other (format "/mevedelmock:second:%sother" root)))
    (unwind-protect
        (mevedel-test--with-local-shell-tramp '("first" "second")
          (should-error
           (mevedel-worktree--git-result
            source "worktree" "add" other "HEAD")
           :type 'mevedel-execution-target-error))
      (delete-directory root t)))

  :doc "does not forward client-only environment variables to target Git"
  (let ((process-environment
         (cons "MEVEDEL_CLIENT_SECRET=do-not-forward" process-environment)))
    (cl-letf (((symbol-function 'process-file)
               (lambda (_program _in _destination _display &rest _args)
                 (should-not (getenv "MEVEDEL_CLIENT_SECRET"))
                 0)))
      (should
       (eq 0
           (plist-get
            (mevedel-worktree--git-result
             "/ssh:user@host:/srv/project/" "status" "--short")
            :exit))))))

(mevedel-deftest mevedel-worktree--ensure-worktree ()
  ,test
  (test)

  :doc "reports missing target Git worktree support directly"
  (let ((root (file-name-as-directory
               (make-temp-file "mevedel-worktree-unsupported-" t)))
        calls)
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel-worktree--git-result)
                   (lambda (_directory &rest args)
                     (push args calls)
                     (mevedel-worktree-test--git-result
                      "git: 'worktree' is not a git command" 129))))
          (let ((err (should-error
                      (mevedel-worktree--ensure-worktree root)
                      :type 'user-error)))
            (should (string-match-p "Git worktree support is required"
                                    (error-message-string err)))
            (should (equal '(("worktree" "list" "--porcelain")) calls))))
      (delete-directory root t))))

(mevedel-deftest mevedel-worktree--parse-worktree-list ()
  ,test
  (test)

  :doc "parses branch, detached, bare, locked, and prunable porcelain rows"
  (let* ((output
          (string-join
           '("worktree /repo"
             "HEAD abc123"
             "branch refs/heads/main"
             ""
             "worktree /repo/.worktrees/topic"
             "HEAD def456"
             "detached"
             "locked maint"
             ""
             "worktree /repo/.worktrees/old"
             "HEAD 999999"
             "bare"
             "prunable stale"
             "")
           "\n"))
         (entries (mevedel-worktree--parse-worktree-list output)))
    (should (= 3 (length entries)))
    (should (equal (plist-get (nth 0 entries) :branch) "main"))
    (should (plist-get (nth 1 entries) :detached))
    (should (equal (plist-get (nth 1 entries) :locked) "maint"))
    (should (plist-get (nth 2 entries) :bare))
    (should (equal (plist-get (nth 2 entries) :prunable) "stale"))))

(mevedel-deftest mevedel-worktree-fork-preflight ()
  ,test
  (test)
  :doc "captures the full current HEAD and rejects non-Git sessions"
  (let ((root (file-name-as-directory
               (make-temp-file "mevedel-worktree-fork-preflight-" t))))
    (unwind-protect
        (let* ((workspace (mevedel-worktree-test--workspace root))
               (session (mevedel-session-create "main" workspace root)))
          (should-error (mevedel-worktree-fork-preflight session)
                        :type 'user-error)
          (mevedel-worktree-test--init-repo root)
          (let ((context (mevedel-worktree-fork-preflight session)))
            (should (equal root (plist-get context :source-root)))
            (should
             (equal
              (plist-get
               (mevedel-worktree-test--git-ok root "rev-parse" "HEAD")
               :output)
              (plist-get context :base-commit)))))
      (delete-directory root t)
      (mevedel-workspace-clear-registry)))
  :doc "reports a missing target Git before repository inspection"
  (let* ((root (file-name-as-directory
                (make-temp-file "mevedel-worktree-no-git-" t)))
         (workspace (mevedel-worktree-test--workspace root))
         (session (mevedel-session-create "main" workspace root))
         calls)
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel-worktree--git-result)
                   (lambda (_directory &rest args)
                     (push args calls)
                     (mevedel-worktree-test--git-result
                      "git: command not found" 127))))
          (let ((err (should-error
                      (mevedel-worktree-fork-preflight session)
                      :type 'user-error)))
            (should (string-match-p "Git is required.*execution target"
                                    (error-message-string err)))
            (should (equal '(("--version")) calls))))
      (delete-directory root t)
      (mevedel-workspace-clear-registry))))

(mevedel-deftest mevedel-worktree-fork-reservation ()
  ,test
  (test)
  :doc "uses the first suffix free in both Git and the worktree directory"
  (let ((root (file-name-as-directory
               (make-temp-file "mevedel-worktree-fork-reserve-" t))))
    (unwind-protect
        (let* ((workspace (mevedel-worktree-test--workspace root))
               (session (mevedel-session-create "main" workspace root)))
          (mevedel-worktree-test--init-repo root)
          (mevedel-worktree-test--git-ok
           root "branch" "worktree/main-fork-1")
          (make-directory
           (file-name-concat root ".worktrees" "main-fork-2") t)
          (let ((reservation
                 (mevedel-worktree-fork-reservation session)))
            (should (equal "worktree/main-fork-3"
                           (plist-get reservation :branch)))
            (should
             (equal
              (file-name-as-directory
               (file-name-concat root ".worktrees" "main-fork-3"))
              (plist-get reservation :directory)))
            (should
             (string-match-p
              "worktree remove --force.*main-fork-3.*branch -D.*main-fork-3"
              (plist-get reservation :cleanup-command)))))
      (delete-directory root t)
      (mevedel-workspace-clear-registry))))

(mevedel-deftest mevedel-worktree-fork-validate-reservation ()
  ,test
  (test)
  :doc "accepts the unchanged target and reports an exact artifact conflict"
  (let ((root (file-name-as-directory
               (make-temp-file "mevedel-worktree-fork-validate-" t))))
    (unwind-protect
        (let* ((workspace (mevedel-worktree-test--workspace root))
               (session (mevedel-session-create "main" workspace root)))
          (mevedel-worktree-test--init-repo root)
          (let ((reservation
                 (mevedel-worktree-fork-reservation session)))
            (should
             (eq reservation
                 (mevedel-worktree-fork-validate-reservation
                  session reservation)))
            (make-directory (plist-get reservation :directory) t)
            (let ((err
                   (should-error
                    (mevedel-worktree-fork-validate-reservation
                     session reservation)
                    :type 'user-error)))
              (should
               (string-match-p
                (regexp-quote
                 (plist-get reservation :cleanup-command))
                (error-message-string err))))))
      (delete-directory root t)
      (mevedel-workspace-clear-registry))))

(mevedel-deftest mevedel-worktree-fork-create ()
  ,test
  (test)
  :doc "checks Git before creating the reserved directory"
  (let* ((root (file-name-as-directory
                (make-temp-file "mevedel-worktree-create-no-git-" t)))
         (directory (file-name-as-directory
                     (file-name-concat root ".worktrees" "fork")))
         (reservation (list :source-directory root
                            :directory directory
                            :branch "worktree/fork"
                            :base-commit "deadbeef"
                            :common-git-dir
                            (file-name-concat root ".git"))))
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel-worktree--git-result)
                   (lambda (_directory &rest _args)
                     (mevedel-worktree-test--git-result
                      "git: command not found" 127))))
          (should-error (mevedel-worktree-fork-create reservation)
                        :type 'user-error)
          (should-not (file-exists-p
                       (file-name-directory
                        (directory-file-name directory)))))
      (delete-directory root t)))
  :doc "checks Git worktree support before creating the reserved directory"
  (let* ((root (file-name-as-directory
                (make-temp-file "mevedel-worktree-create-unsupported-" t)))
         (directory (file-name-as-directory
                     (file-name-concat root ".worktrees" "fork")))
         (git-dir (file-name-concat root ".git"))
         (reservation (list :source-directory root
                            :directory directory
                            :branch "worktree/fork"
                            :base-commit "deadbeef"
                            :common-git-dir git-dir)))
    (unwind-protect
        (progn
          (make-directory git-dir t)
          (cl-letf (((symbol-function 'mevedel-worktree--git-result)
                     (lambda (_directory &rest args)
                       (if (equal args '("worktree" "list" "--porcelain"))
                           (mevedel-worktree-test--git-result
                            "git: 'worktree' is not a git command" 129)
                         (mevedel-worktree-test--git-result "")))))
            (should-error (mevedel-worktree-fork-create reservation)
                          :type 'user-error)
            (should-not (file-exists-p
                         (file-name-directory
                          (directory-file-name directory))))))
      (delete-directory root t)))
  :doc "rejects a foreign target before commands or directory creation"
  (let* ((root (file-name-as-directory
                (make-temp-file "mevedel-worktree-create-target-" t)))
         (source (format "/mevedelmock:source:%s" root))
         (directory
          (file-name-as-directory
           (format "/mevedelmock:foreign:%s"
                   (file-name-concat root "foreign" "fork"))))
         (reservation (list :source-directory source
                            :directory directory
                            :branch "worktree/fork"
                            :base-commit "deadbeef"
                            :common-git-dir
                            (file-name-concat source ".git")))
         calls)
    (unwind-protect
        (mevedel-test--with-local-shell-tramp '("source" "foreign")
          (cl-letf (((symbol-function 'mevedel-worktree--git-result)
                     (lambda (_directory &rest args)
                       (push args calls)
                       (mevedel-worktree-test--git-result ""))))
            (should-error (mevedel-worktree-fork-create reservation)
                          :type 'mevedel-execution-target-error)
            (should-not calls)
            (should-not (file-exists-p
                         (file-name-concat root "foreign")))))
      (delete-directory root t)))
  :doc "rejects a remote escape introduced after reservation"
  (let* ((base (file-name-as-directory
                (make-temp-file "mevedel-worktree-fork-symlink-" t)))
         (root (file-name-as-directory (file-name-concat base "repo")))
         (outside (file-name-as-directory
                   (file-name-concat base "outside")))
         (remote-root (format "/mevedelmock:escape:%s" root)))
    (unwind-protect
        (mevedel-test--with-local-shell-tramp '("escape")
          (make-directory root t)
          (make-directory outside t)
          (mevedel-worktree-test--init-repo remote-root)
          (let* ((workspace
                  (mevedel-worktree-test--workspace remote-root))
                 (session
                  (mevedel-session-create "remote" workspace remote-root))
                 (reservation
                  (mevedel-worktree-fork-reservation session)))
            (make-symbolic-link
             outside (file-name-concat root ".worktrees"))
            (should-error (mevedel-worktree-fork-create reservation)
                          :type 'user-error)
            (should-not
             (file-exists-p
              (file-name-concat outside "remote-fork-1")))
            (should-not
             (eq 0
                 (mevedel-worktree--git-exit
                  remote-root "show-ref" "--verify" "--quiet"
                  "refs/heads/worktree/remote-fork-1")))))
      (delete-directory base t)
      (mevedel-workspace-clear-registry)))
  :doc "creates a linked worktree and branch at the reserved base commit"
  (let ((root (file-name-as-directory
               (make-temp-file "mevedel-worktree-fork-create-" t))))
    (unwind-protect
        (let* ((workspace (mevedel-worktree-test--workspace root))
               (session (mevedel-session-create "main" workspace root)))
          (mevedel-worktree-test--init-repo root)
          (let* ((reservation
                  (mevedel-worktree-fork-reservation session))
                 (directory (plist-get reservation :directory)))
            (mevedel-worktree-fork-create reservation)
            (should (file-directory-p directory))
            (should
             (equal
              (plist-get reservation :base-commit)
              (plist-get
               (mevedel-worktree-test--git-ok
                directory "rev-parse" "HEAD")
               :output)))
            (should
             (equal
              (plist-get reservation :branch)
              (plist-get
               (mevedel-worktree-test--git-ok
                directory "branch" "--show-current")
               :output)))))
      (delete-directory root t)
      (mevedel-workspace-clear-registry)))
  :doc "forks an existing linked worktree into a sibling at its current HEAD"
  (let ((root (file-name-as-directory
               (make-temp-file "mevedel-worktree-fork-sibling-" t))))
    (unwind-protect
        (let* ((workspace (mevedel-worktree-test--workspace root))
               (source
                (file-name-as-directory
                 (file-name-concat root ".worktrees" "source"))))
          (mevedel-worktree-test--init-repo root)
          (make-directory (file-name-directory
                           (directory-file-name source))
                          t)
          (mevedel-worktree-test--git-ok
           root "worktree" "add" "-b" "topic" source "HEAD")
          (write-region "source head\n" nil
                        (file-name-concat source "file.txt")
                        nil 'silent)
          (mevedel-worktree-test--git-ok source "add" "file.txt")
          (mevedel-worktree-test--git-ok
           source "commit" "-m" "source head")
          (let* ((session
                  (mevedel-session-create "nested" workspace source))
                 (reservation
                  (mevedel-worktree-fork-reservation session))
                 (target (plist-get reservation :directory))
                 (source-head
                  (plist-get
                   (mevedel-worktree-test--git-ok
                    source "rev-parse" "HEAD")
                   :output)))
            (should
             (equal
              (file-name-directory (directory-file-name source))
              (file-name-directory (directory-file-name target))))
            (should-not (file-in-directory-p target source))
            (mevedel-worktree-fork-create reservation)
            (should
             (equal
              source-head
              (plist-get
               (mevedel-worktree-test--git-ok
                target "rev-parse" "HEAD")
               :output)))))
      (delete-directory root t)
      (mevedel-workspace-clear-registry)))
  :doc "creates the linked worktree with target Git and native path arguments"
  (let* ((root (file-name-as-directory
                (make-temp-file "mevedel-worktree-remote-" t)))
         (remote-root (format "/mevedelmock:worktree:%s" root)))
    (unwind-protect
        (mevedel-test--with-local-shell-tramp '("worktree")
          (let* ((workspace (mevedel-worktree-test--workspace remote-root))
                 (session (mevedel-session-create
                           "remote" workspace remote-root)))
            (mevedel-worktree-test--init-repo remote-root)
            (let* ((reservation
                    (mevedel-worktree-fork-reservation session))
                   (directory (plist-get reservation :directory)))
              (should (equal remote-root
                             (plist-get reservation :source-root)))
              (should (file-remote-p directory))
              (should-not
               (string-match-p "/mevedelmock:"
                               (plist-get reservation :cleanup-command)))
              (mevedel-worktree-fork-create reservation)
              (should (file-directory-p directory))
              (should
               (equal
                (plist-get reservation :branch)
                (plist-get
                 (mevedel-worktree-test--git-ok
                  directory "branch" "--show-current")
                 :output))))))
      (delete-directory root t)
      (mevedel-workspace-clear-registry))))

(mevedel-deftest mevedel-worktree--validate-branch-name ()
  ,test
  (test)

  :doc "checks target Git before validating a Plan Worktree branch"
  (let ((root (file-name-as-directory
               (make-temp-file "mevedel-plan-worktree-no-git-" t)))
        calls)
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel-worktree--git-result)
                   (lambda (_directory &rest args)
                     (push args calls)
                     (mevedel-worktree-test--git-result
                      "git: command not found" 127))))
          (let ((err
                 (should-error
                  (mevedel-worktree--validate-branch-name
                   "worktree/accepted-plan" root)
                  :type 'user-error)))
            (should (string-match-p "Git is required.*execution target"
                                    (error-message-string err)))
            (should (equal '(("--version")) calls))))
      (delete-directory root t)))

  :doc "checks worktree support before accepting a Plan Worktree branch"
  (let ((root (file-name-as-directory
               (make-temp-file "mevedel-plan-worktree-unsupported-" t)))
        calls)
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel-worktree--git-result)
                   (lambda (_directory &rest args)
                     (push args calls)
                     (if (equal args '("worktree" "list" "--porcelain"))
                         (mevedel-worktree-test--git-result
                          "git: 'worktree' is not a git command" 129)
                       (mevedel-worktree-test--git-result "")))))
          (should-error
           (mevedel-worktree--validate-branch-name
            "worktree/accepted-plan" root)
           :type 'user-error)
          (should-not (member '("check-ref-format" "--branch"
                                "worktree/accepted-plan")
                              calls)))
      (delete-directory root t))))

(mevedel-deftest mevedel-worktree--validate-destination ()
  ,test
  (test)
  :doc "accepts a direct non-symlinked workspace destination"
  (let* ((root (file-name-as-directory
                (make-temp-file "mevedel-worktree-destination-" t)))
         (directory
          (file-name-as-directory
           (file-name-concat root ".worktrees" "topic"))))
    (unwind-protect
        (should
         (equal directory
                (mevedel-worktree--validate-destination root directory)))
      (delete-directory root t)))
  :doc "rejects a worktrees symlink even when its target stays inside"
  (let* ((root (file-name-as-directory
                (make-temp-file "mevedel-worktree-inside-symlink-" t)))
         (target (file-name-as-directory
                  (file-name-concat root "target")))
         (directory
          (file-name-as-directory
           (file-name-concat root ".worktrees" "topic"))))
    (unwind-protect
        (progn
          (make-directory target t)
          (make-symbolic-link target (file-name-concat root ".worktrees"))
          (should-error
           (mevedel-worktree--validate-destination root directory)
           :type 'user-error))
      (delete-directory root t))))


;;
;;; Status collection

(mevedel-deftest mevedel-worktree--collect-status ()
  ,test
  (test)

  :doc "uses explicit cockpit context for session, workspace, and directory"
  (let* ((root (file-name-as-directory
                (make-temp-file "mevedel-worktree-context-status-" t)))
         (workspace (mevedel-worktree-test--workspace root))
         (session (mevedel-session-create "main" workspace root))
         (data-buffer (generate-new-buffer " *mwt-context-status*"))
         (context (list :data-buffer data-buffer
                        :session session
                        :workspace workspace)))
    (unwind-protect
        (progn
          (make-directory (file-name-concat root ".git") t)
          (with-current-buffer data-buffer
            (setq-local default-directory root))
          (cl-letf (((symbol-function 'mevedel-worktree--git-result)
                     (lambda (_dir &rest args)
                       (mevedel-worktree-test--base-response root args))))
            (let ((status (mevedel-worktree--collect-status context)))
              (should (eq (plist-get status :session) session))
              (should (eq (plist-get status :workspace) workspace))
              (should (equal (plist-get status :directory) root)))))
      (mevedel-worktree-test--cleanup-surfaces data-buffer)
      (delete-directory root t))))


;;
;;; Surface

(mevedel-deftest mevedel-worktree-status--data-buffer ()
  ,test
  (test)

  :doc "uses the cockpit context from the transient original buffer"
  (let* ((root (make-temp-file "mevedel-worktree-status-data-" t))
         (status (mevedel-worktree-test--status root))
         (data-buffer (generate-new-buffer " *mwt-status-data*")))
    (unwind-protect
        (progn
          (mevedel-worktree-test--install-context status data-buffer)
          (with-temp-buffer
            (let ((transient--original-buffer data-buffer))
              (should (eq (mevedel-worktree-status--data-buffer)
                          data-buffer)))))
      (mevedel-worktree-test--cleanup-surfaces data-buffer)
      (delete-directory root t)))

  :doc "uses the current buffer's cockpit context outside transient commands"
  (let* ((root (make-temp-file "mevedel-worktree-status-current-" t))
         (status (mevedel-worktree-test--status root))
         (data-buffer (generate-new-buffer " *mwt-status-current*")))
    (unwind-protect
        (with-current-buffer data-buffer
          (mevedel-worktree-test--install-context status data-buffer)
          (should (eq (mevedel-worktree-status--data-buffer)
                      data-buffer)))
      (mevedel-worktree-test--cleanup-surfaces data-buffer)
      (delete-directory root t))))

(mevedel-deftest mevedel-worktree--branch-head-label ()
  ,test
  (test)

  :doc "prefers branch, then detached head, then unavailable"
  (should (equal (mevedel-worktree--branch-head-label "main" "abc123")
                 "main"))
  (should (equal (mevedel-worktree--branch-head-label nil "abc123")
                 "detached at abc123"))
  (should (equal (mevedel-worktree--branch-head-label nil nil)
                 "unavailable")))

(mevedel-deftest mevedel-worktree-status-summary ()
  ,test
  (test)

  :doc "summarizes a branch checkout"
  (cl-letf (((symbol-function 'mevedel-worktree--collect-status)
             (lambda (&optional _context)
               (ert-fail "summary should not collect full status")))
            ((symbol-function 'mevedel-worktree--git-success-output)
             (lambda (_directory &rest args)
               (cond
                ((equal args '("rev-parse" "--is-inside-work-tree"))
                 "true")
                ((equal args '("branch" "--show-current")) "main")
                ((equal args '("rev-parse" "--short" "HEAD")) "abc123")))))
    (should (equal '(:state git :label "main")
                   (mevedel-worktree-status-summary '(:directory "/tmp")))))

  :doc "summarizes a detached checkout with the menu's compact label"
  (cl-letf (((symbol-function 'mevedel-worktree--git-success-output)
             (lambda (_directory &rest args)
               (cond
                ((equal args '("rev-parse" "--is-inside-work-tree"))
                 "true")
                ((equal args '("branch" "--show-current")) "")
                ((equal args '("rev-parse" "--short" "HEAD")) "abc123")))))
    (should (equal '(:state git :label "detached abc123")
                   (mevedel-worktree-status-summary '(:directory "/tmp")))))

  :doc "summarizes non-Git directories"
  (cl-letf (((symbol-function 'mevedel-worktree--git-success-output)
             (lambda (_directory &rest args)
               (when (equal args '("rev-parse" "--is-inside-work-tree"))
                 nil))))
    (should (equal '(:state not-git :label "not-git")
                   (mevedel-worktree-status-summary '(:directory "/tmp"))))))

(mevedel-deftest mevedel-worktree-status--description ()
  ,test
  (test)

  :doc "renders the compact one-line worktree status"
  (let* ((root (file-name-as-directory
                (make-temp-file "mevedel-worktree-desc-" t)))
         (workspace (mevedel-worktree-test--workspace root))
         (session (mevedel-session-create "main" workspace root))
         (status (plist-put
                  (plist-put (mevedel-worktree-test--status root)
                             :session session)
                  :workspace workspace))
         called-buffer
         called-context)
    (let ((data-buffer (generate-new-buffer " *mwt-desc-data*")))
      (unwind-protect
          (progn
            (mevedel-worktree-test--install-context status data-buffer)
            (with-current-buffer data-buffer
              (cl-letf (((symbol-function 'mevedel-worktree--collect-status)
                         (lambda (&optional context)
                           (setq called-buffer (current-buffer)
                                 called-context context)
                           status)))
                (let ((description (substring-no-properties
                                    (mevedel-worktree-status--description))))
                  (should (string-match-p "Worktree  main" description))
                  (should (string-match-p "normal checkout" description))
                  (should (string-match-p "1 worktree" description))
                  ;; An ignored `.worktrees' and a clean tree are nominal,
                  ;; so neither earns an alert line.
                  (should-not (string-match-p "dirty" description))
                  (should-not (string-match-p "\n" description)))))
            (should (eq called-buffer data-buffer))
            (should (eq (plist-get called-context :session) session))
            (should (eq (plist-get called-context :workspace) workspace)))
        (mevedel-worktree-test--cleanup-surfaces data-buffer)
        (delete-directory root t))))

  :doc "raises a dirty tree and an untracked .worktrees directory"
  (let* ((root (file-name-as-directory
                (make-temp-file "mevedel-worktree-desc-" t)))
         (workspace (mevedel-worktree-test--workspace root))
         (session (mevedel-session-create "main" workspace root))
         (status (thread-first
                   (mevedel-worktree-test--status root)
                   (plist-put :session session)
                   (plist-put :workspace workspace)
                   (plist-put :dirty-p t)
                   (plist-put :ignore-state 'not-ignored))))
    (let ((data-buffer (generate-new-buffer " *mwt-desc-data*")))
      (unwind-protect
          (progn
            (mevedel-worktree-test--install-context status data-buffer)
            (with-current-buffer data-buffer
              (cl-letf (((symbol-function 'mevedel-worktree--collect-status)
                         (lambda (&optional _context) status)))
                (let* ((description (substring-no-properties
                                     (mevedel-worktree-status--description)))
                       (alert (nth 1 (split-string description "\n"))))
                  (should (string-match-p "dirty" description))
                  (should (string-match-p "! .worktrees not ignored"
                                          alert))))))
        (mevedel-worktree-test--cleanup-surfaces data-buffer)
        (delete-directory root t)))))

(mevedel-deftest mevedel-worktree-status--details-text ()
  ,test
  (test)

  :doc "renders the complete repository status as aligned rows"
  (let* ((root (file-name-as-directory
                (make-temp-file "mevedel-worktree-details-" t)))
         (workspace (mevedel-worktree-test--workspace root))
         (session (mevedel-session-create "main" workspace root))
         (status (plist-put
                  (plist-put (mevedel-worktree-test--status root)
                             :session session)
                  :workspace workspace)))
    (let ((data-buffer (generate-new-buffer " *mwt-details-data*")))
      (unwind-protect
          (progn
            (mevedel-worktree-test--install-context status data-buffer)
            (with-current-buffer data-buffer
              (cl-letf (((symbol-function 'mevedel-worktree--collect-status)
                         (lambda (&optional _context) status)))
                (let ((text (mevedel-worktree-status--details-text)))
                  (should (string-match-p "mevedel worktree" text))
                  (should (string-match-p "Repo " text))
                  (should (string-match-p "Session       main" text))
                  (should (string-match-p "Directory " text))
                  (should (string-match-p "Isolation     normal checkout"
                                          text))
                  (should (string-match-p "Branch        main" text))
                  (should (string-match-p ".worktrees    ignored" text))
                  (should (string-match-p "Dirty         no" text))
                  (should (string-match-p "Worktrees     1" text))))))
        (mevedel-worktree-test--cleanup-surfaces data-buffer)
        (delete-directory root t)))))

(mevedel-deftest mevedel-worktree-status-create ()
  ,test
  (test)

  :doc "delegates create in the data buffer and reopens status"
  (let* ((root (make-temp-file "mevedel-worktree-status-create-" t))
         (status (mevedel-worktree-test--status root))
         (data-buffer (generate-new-buffer " *mwt-status-create*"))
         called-buffer
         called-args
         reopened
         message-text)
    (unwind-protect
        (progn
          (mevedel-worktree-test--install-context status data-buffer)
          (with-current-buffer data-buffer
            (let ((transient--original-buffer data-buffer))
              (cl-letf (((symbol-function 'mevedel-cmd--worktree)
                         (lambda (args)
                           (setq called-buffer (current-buffer)
                                 called-args args)
                           "created"))
                        ((symbol-function 'mevedel-worktree-status-open)
                         (lambda () (setq reopened (current-buffer))))
                        ((symbol-function 'message)
                         (lambda (fmt &rest args)
                           (setq message-text (apply #'format fmt args)))))
                (should (equal (mevedel-worktree-status-create)
                               "created"))))))
      (mevedel-worktree-test--cleanup-surfaces data-buffer)
      (delete-directory root t))
    (should (eq called-buffer data-buffer))
    (should (equal called-args "create"))
    (should (eq reopened data-buffer))
    (should (equal message-text "created"))))

(mevedel-deftest mevedel-worktree-status-list ()
  ,test
  (test)

  :doc "opens the list from the transient data buffer"
  (let* ((root (make-temp-file "mevedel-worktree-status-list-" t))
         (status (mevedel-worktree-test--status root))
         (data-buffer (generate-new-buffer " *mwt-status-list*"))
         called-buffer
         called-context)
    (unwind-protect
        (progn
          (mevedel-worktree-test--install-context status data-buffer)
          (with-current-buffer data-buffer
            (let ((transient--original-buffer data-buffer))
              (cl-letf (((symbol-function 'mevedel-worktree-list-open)
                         (lambda (&optional context)
                           (setq called-buffer (current-buffer)
                                 called-context context))))
                (mevedel-worktree-status-list)))))
      (mevedel-worktree-test--cleanup-surfaces data-buffer)
      (delete-directory root t))
    (should (eq called-buffer data-buffer))
    (should (equal called-context
                   (mevedel-worktree-test--context status data-buffer)))))

(mevedel-deftest mevedel-worktree-status-refresh ()
  ,test
  (test)

  :doc "reopens the status transient from the data buffer"
  (let* ((root (make-temp-file "mevedel-worktree-status-refresh-" t))
         (status (mevedel-worktree-test--status root))
         (data-buffer (generate-new-buffer " *mwt-status-refresh*"))
         called-buffer)
    (unwind-protect
        (progn
          (mevedel-worktree-test--install-context status data-buffer)
          (with-current-buffer data-buffer
            (let ((transient--original-buffer data-buffer))
              (cl-letf (((symbol-function 'mevedel-worktree-status-open)
                         (lambda () (setq called-buffer (current-buffer)))))
                (mevedel-worktree-status-refresh)))))
      (mevedel-worktree-test--cleanup-surfaces data-buffer)
      (delete-directory root t))
    (should (eq called-buffer data-buffer))))

(mevedel-deftest mevedel-worktree-status-details ()
  ,test
  (test)

  :doc "opens the worktree info panel"
  (let* ((root (file-name-as-directory
                (make-temp-file "mevedel-worktree-panel-" t)))
         (workspace (mevedel-worktree-test--workspace root))
         (session (mevedel-session-create "main" workspace root))
         (status (plist-put
                  (plist-put (mevedel-worktree-test--status root)
                             :session session)
                  :workspace workspace))
         (data-buffer (generate-new-buffer " *mwt-panel-data*")))
    (unwind-protect
        (progn
          (mevedel-worktree-test--install-context status data-buffer)
          (with-current-buffer data-buffer
            (cl-letf (((symbol-function 'mevedel-worktree--collect-status)
                       (lambda (&optional _context) status)))
              (mevedel-worktree-status-details)))
          (with-current-buffer mevedel-worktree-details-buffer-name
            (should (string-match-p "Session       main" (buffer-string)))))
      (when (get-buffer mevedel-worktree-details-buffer-name)
        (kill-buffer mevedel-worktree-details-buffer-name))
      (mevedel-worktree-test--cleanup-surfaces data-buffer)
      (delete-directory root t))))

(mevedel-deftest mevedel-worktree-status-help ()
  ,test
  (test)

  :doc "opens worktree cockpit help"
  (unwind-protect
      (progn
        (mevedel-worktree-status-help)
        (with-current-buffer mevedel-worktree-help-buffer-name
          (should (string-match-p "c  Create a linked worktree session"
                                  (buffer-string)))
          (should (string-match-p "i  Show full repository status"
                                  (buffer-string)))
          (should (string-match-p "q  Back to the main session cockpit"
                                  (buffer-string)))
          (should (string-match-p "RET  Show selected worktree details"
                                  (buffer-string)))
          (should (string-match-p "g    Refresh table"
                                  (buffer-string)))))
    (mevedel-worktree-test--cleanup-surfaces)))

(mevedel-deftest mevedel-worktree-status-back ()
  ,test
  (test)

  :doc "returns to the main menu from the data buffer"
  (let* ((root (make-temp-file "mevedel-worktree-status-back-" t))
         (status (mevedel-worktree-test--status root))
         (data-buffer (generate-new-buffer " *mwt-status-back*"))
         called-buffer)
    (unwind-protect
        (progn
          (mevedel-worktree-test--install-context status data-buffer)
          (with-current-buffer data-buffer
            (let ((transient--original-buffer data-buffer))
              (cl-letf (((symbol-function 'mevedel-menu)
                         (lambda () (setq called-buffer (current-buffer)))))
                (mevedel-worktree-status-back)))))
      (mevedel-worktree-test--cleanup-surfaces data-buffer)
      (delete-directory root t))
    (should (eq called-buffer data-buffer))))

(mevedel-deftest mevedel-worktree-status-open ()
  ,test
  (test)

  :doc "opens the worktree status transient"
  (let (called-prefix)
    (cl-letf (((symbol-function 'transient-setup)
               (lambda (prefix &rest _)
                 (setq called-prefix prefix))))
      (mevedel-worktree-status-open))
    (should (eq called-prefix 'mevedel-worktree-status))))

(mevedel-deftest mevedel-worktree-list--status ()
  ,test
  (test)

  :doc "collects status in the owning data buffer"
  (let* ((root (make-temp-file "mevedel-worktree-status-owner-" t))
         (status (mevedel-worktree-test--status root))
         (data-buffer (generate-new-buffer " *mwt-status-owner*"))
         (surface (get-buffer-create mevedel-worktree-list-buffer-name))
         called-buffer
         called-context)
    (unwind-protect
        (with-current-buffer surface
          (mevedel-worktree-list-mode)
          (setq-local mevedel-cockpit--context
                      (mevedel-worktree-test--context
                       status data-buffer data-buffer data-buffer))
          (cl-letf (((symbol-function 'mevedel-worktree--collect-status)
                     (lambda (&optional context)
                       (setq called-buffer (current-buffer))
                       (setq called-context context)
                       :status)))
            (should (eq (mevedel-worktree-list--status) :status))))
      (mevedel-worktree-test--cleanup-surfaces data-buffer)
      (when (buffer-live-p surface)
        (kill-buffer surface))
      (delete-directory root t))
    (should (eq called-buffer data-buffer))
    (should (equal called-context
                   (mevedel-worktree-test--context
                    status data-buffer data-buffer data-buffer)))))

(mevedel-deftest mevedel-worktree-list--normalize-path ()
  ,test
  (test)

  :doc "returns an absolute directory path"
  (let ((path (mevedel-worktree-list--normalize-path ".")))
    (should (file-name-absolute-p path))
    (should (string-suffix-p "/" path))))

(mevedel-deftest mevedel-worktree-list--item-id ()
  ,test
  (test)

  :doc "uses the normalized worktree path as the row id"
  (should (equal (mevedel-worktree-list--item-id
                  '(:path "/repo/.worktrees/topic/"))
                 "/repo/.worktrees/topic/")))

(mevedel-deftest mevedel-worktree-list--branch-label ()
  ,test
  (test)

  :doc "formats branch, detached, bare, head, and unknown branch cells"
  (should (equal (mevedel-worktree-list--branch-label
                  '(:branch "main" :head "abc"))
                 "main"))
  (should (equal (mevedel-worktree-list--branch-label
                  '(:detached t :head "abc"))
                 "detached abc"))
  (should (equal (mevedel-worktree-list--branch-label
                  '(:bare t :head "abc"))
                 "bare"))
  (should (equal (mevedel-worktree-list--branch-label
                  '(:head "abc"))
                 "abc"))
  (should (equal (mevedel-worktree-list--branch-label nil)
                 "unknown")))

(mevedel-deftest mevedel-worktree-list--state-label ()
  ,test
  (test)

  :doc "normalizes porcelain state flags"
  (should (equal (mevedel-worktree-list--state-label
                  '(:locked "reason" :branch "main"))
                 "locked"))
  (should (equal (mevedel-worktree-list--state-label
                  '(:prunable "stale" :branch "main"))
                 "prunable"))
  (should (equal (mevedel-worktree-list--state-label '(:bare t))
                 "bare"))
  (should (equal (mevedel-worktree-list--state-label '(:detached t))
                 "detached"))
  (should (equal (mevedel-worktree-list--state-label '(:branch "main"))
                 "branch"))
  (should (equal (mevedel-worktree-list--state-label nil)
                 "unknown")))

(mevedel-deftest mevedel-worktree-list--current-label ()
  ,test
  (test)

  :doc "uses a compact yes/blank current column"
  (should (equal (mevedel-worktree-list--current-label t) "yes"))
  (should (equal (mevedel-worktree-list--current-label nil) "")))

(mevedel-deftest mevedel-worktree-list--sessions ()
  ,test
  (test)

  :doc "shows live session names for the selected worktree only"
  (cl-letf (((symbol-function 'mevedel--workspace-sessions)
             (lambda (workspace)
               (should (eq workspace :workspace))
               :sessions))
            ((symbol-function 'mevedel--sessions-in-working-directory)
             (lambda (sessions path)
               (should (eq sessions :sessions))
               (should (equal path "/repo/"))
               '(("main" . :session)
                 ("alt" . :session)))))
    (should (equal (mevedel-worktree-list--sessions
                    :workspace "/repo/")
                   '("main" "alt"))))
  (should-not (mevedel-worktree-list--sessions nil "/repo/")))

(mevedel-deftest mevedel-worktree-list--item ()
  ,test
  (test)

  :doc "builds a row item from status and porcelain data"
  (let* ((root (file-name-as-directory
                (make-temp-file "mevedel-worktree-item-" t)))
         (entry (list :path root :branch "main" :head "abc123"))
         (status (plist-put (mevedel-worktree-test--status root
                                                           (list entry))
                            :workspace :workspace)))
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel-worktree-list--sessions)
                   (lambda (workspace path)
                     (should (eq workspace :workspace))
                     (should (equal path root))
                     '("main"))))
          (let ((item (mevedel-worktree-list--item status entry)))
            (should (equal (plist-get item :path) root))
            (should (equal (plist-get item :branch) "main"))
            (should (equal (plist-get item :head) "abc123"))
            (should (plist-get item :current))
            (should (equal (plist-get item :state) "branch"))
            (should (equal (plist-get item :sessions) '("main")))))
      (delete-directory root t)))

  :doc "uses workspace root spelling for aliased porcelain paths"
  (let* ((alias-root (file-name-as-directory
                      (expand-file-name "/alias/root")))
         (real-root (file-name-as-directory
                     (expand-file-name "/real/root")))
         (workspace (mevedel-worktree-test--workspace real-root))
         (expected-workspace workspace)
         (alias-path (file-name-as-directory
                      (file-name-concat alias-root ".worktrees" "foo")))
         (real-path (file-name-as-directory
                     (file-name-concat real-root ".worktrees" "foo")))
         (entry (list :path alias-path :branch "worktree/foo" :head "def"))
         (status (list :directory alias-root
                       :workspace workspace
                       :worktrees (list entry))))
    (cl-letf (((symbol-function 'file-equal-p)
               (lambda (a b)
                 (let ((a (directory-file-name a))
                       (b (directory-file-name b))
                       (alias (directory-file-name alias-root))
                       (real (directory-file-name real-root)))
                   (or (equal a b)
                       (and (equal a alias) (equal b real))
                       (and (equal a real) (equal b alias))))))
              ((symbol-function 'mevedel-worktree-list--sessions)
               (lambda (workspace path)
                 (should (eq workspace expected-workspace))
                 (should (equal path real-path))
                 nil)))
      (let ((item (mevedel-worktree-list--item status entry)))
        (should (equal (plist-get item :path) real-path))
        (should (equal (mevedel-worktree-list--item-id item) real-path))
        (should-not (plist-get item :current))))))

(mevedel-deftest mevedel-worktree-list--items ()
  ,test
  (test)

  :doc "maps all porcelain worktrees into row items"
  (let* ((root (file-name-as-directory
                (make-temp-file "mevedel-worktree-items-" t)))
         (other (file-name-as-directory
                 (file-name-concat root ".worktrees" "other")))
         (status (mevedel-worktree-test--status
                  root
                  (list (list :path root :branch "main" :head "abc")
                        (list :path other :detached t :head "def")))))
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel-worktree-list--sessions)
                   (lambda (&rest _) nil)))
          (let ((items (mevedel-worktree-list--items status)))
            (should (= 2 (length items)))
            (should (plist-get (car items) :current))
            (should-not (plist-get (cadr items) :current))))
      (delete-directory root t))))

(mevedel-deftest mevedel-worktree-list--entry ()
  ,test
  (test)

  :doc "builds table cells from worktree item fields"
  (let* ((item '(:path "/repo/"
                 :branch "main"
                 :head "abc123"
                 :current t
                 :state "branch"
                 :sessions ("main" "alt")))
         (entry (mevedel-worktree-list--entry item))
         (cells (mevedel-test-tabulated-row-cells entry)))
    (should (equal (car entry) "/repo/"))
    (should (equal cells '("/repo/" "main" "abc123" "yes" "branch"
                           "main, alt")))))

(mevedel-deftest mevedel-worktree-list-refresh ()
  ,test
  (test)

  :doc "refreshes visible worktree row content"
  (let* ((root (file-name-as-directory
                (make-temp-file "mevedel-worktree-refresh-" t)))
         (other (file-name-as-directory
                 (file-name-concat root ".worktrees" "other")))
         (status (mevedel-worktree-test--status
                  root
                  (list (list :path root :branch "main" :head "abc")
                        (list :path other :branch "topic" :head "def"))))
         (view-buffer (generate-new-buffer " *mwt-refresh-view*"))
         (data-buffer (generate-new-buffer " *mwt-refresh-data*")))
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel-worktree--collect-status)
                   (lambda (&optional _context) status))
                  ((symbol-function 'mevedel-worktree-list--sessions)
                   (lambda (&rest _) nil)))
          (let ((buffer (mevedel-worktree-list-open
                         (mevedel-worktree-test--context
                          status data-buffer view-buffer data-buffer))))
            (with-current-buffer buffer
              (mevedel-cockpit-goto-id other)
              (setq status
                    (mevedel-worktree-test--status
                     root
                     (list (list :path root :branch "main" :head "abc")
                           (list :path other :branch "updated"
                                 :head "fff"))))
              (mevedel-worktree-list-refresh)
              (let ((rows (mevedel-test-tabulated-entries-cells)))
                (should (equal (cdr (assoc other rows))
                               (list other "updated" "fff" "" "branch"
                                     "")))))))
      (mevedel-worktree-test--cleanup-surfaces view-buffer data-buffer)
      (delete-directory root t))))

(mevedel-deftest mevedel-worktree-list--details-text ()
  ,test
  (test)

  :doc "formats normalized selected worktree details"
  (let ((text (mevedel-worktree-list--details-text
               '(:path "/repo/"
                 :branch "main"
                 :head "abc123"
                 :current t
                 :state "branch"
                 :sessions ("main")))))
    (should (string-match-p "Worktree /repo/" text))
    (should (string-match-p "Path: /repo/" text))
    (should (string-match-p "Branch: main" text))
    (should (string-match-p "Head: abc123" text))
    (should (string-match-p "Current: yes" text))
    (should (string-match-p "State: branch" text))
    (should (string-match-p "Sessions: main" text))))

(mevedel-deftest mevedel-worktree-list-details ()
  ,test
  (test)

  :doc "opens normalized details for the selected worktree"
  (let* ((root (file-name-as-directory
                (make-temp-file "mevedel-worktree-details-" t)))
         (status (mevedel-worktree-test--status root))
         (data-buffer (generate-new-buffer " *mwt-details-data*")))
    (unwind-protect
        (let ((buffer (mevedel-worktree-test--open-list
                       status data-buffer)))
          (with-current-buffer buffer
            (mevedel-cockpit-goto-id root)
            (mevedel-worktree-list-details))
          (with-current-buffer "*mevedel worktree details*"
            (should (string-match-p (regexp-quote root)
                                    (buffer-string)))
            (should (string-match-p "Branch: main"
                                    (buffer-string)))))
      (mevedel-worktree-test--cleanup-surfaces data-buffer)
      (delete-directory root t))))

(mevedel-deftest mevedel-worktree-list-open-selected ()
  ,test
  (test)

  :doc "opens the selected worktree with directory-scoped session semantics"
  (let* ((root (file-name-as-directory
                (make-temp-file "mevedel-worktree-open-" t)))
         (workspace (mevedel-worktree-test--workspace root))
         (status (plist-put (mevedel-worktree-test--status root)
                            :workspace workspace))
         (data-buffer (generate-new-buffer " *mwt-open-data*"))
         called-workspace
         called-path
         called-prompt
         called-directory-scoped)
    (unwind-protect
        (let ((buffer (mevedel-worktree-test--open-list
                       status data-buffer)))
          (with-current-buffer buffer
            (mevedel-cockpit-goto-id root)
            (cl-letf (((symbol-function 'mevedel--start-chat)
                       (lambda (workspace path prompt directory-scoped)
                         (setq called-workspace workspace
                               called-path path
                               called-prompt prompt
                               called-directory-scoped directory-scoped)
                         :chat)))
              (should (eq (mevedel-worktree-list-open-selected)
                          :chat)))))
      (mevedel-worktree-test--cleanup-surfaces data-buffer)
      (delete-directory root t))
    (should (eq called-workspace workspace))
    (should (equal called-path root))
    (should-not called-prompt)
    (should called-directory-scoped))

  :doc "signals when no workspace can own the selected worktree"
  (let* ((root (file-name-as-directory
                (make-temp-file "mevedel-worktree-open-noworkspace-" t)))
         (status (mevedel-worktree-test--status root))
         (data-buffer (generate-new-buffer " *mwt-open-noworkspace-data*")))
    (unwind-protect
        (let ((buffer (mevedel-worktree-test--open-list
                       status data-buffer)))
          (with-current-buffer buffer
            (mevedel-cockpit-goto-id root)
            (should-error (mevedel-worktree-list-open-selected)
                          :type 'user-error)))
      (mevedel-worktree-test--cleanup-surfaces data-buffer)
      (delete-directory root t))))

(mevedel-deftest mevedel-worktree-list-create
  (:quiet t :after-each (mevedel-worktree-test--cleanup-surfaces))
  ,test
  (test)

  :doc "delegates create to the existing worktree command in the data buffer"
  (let* (called-buffer
         called-args)
    (with-temp-buffer
      (let ((data-buffer (current-buffer))
            (surface (get-buffer-create mevedel-worktree-list-buffer-name)))
        (with-current-buffer surface
          (mevedel-worktree-list-mode)
          (setq-local mevedel-cockpit--context
                      (mevedel-worktree-test--context
                       (mevedel-worktree-test--status default-directory)
                       data-buffer data-buffer data-buffer))
          (cl-letf (((symbol-function 'mevedel-cmd--worktree)
                     (lambda (args)
                       (setq called-buffer (current-buffer)
                             called-args args)
                       "created"))
                    ((symbol-function 'mevedel-worktree-list-refresh)
                     #'ignore))
            (should (string= "created"
                             (mevedel-worktree-list-create)))))
        (should (eq called-buffer data-buffer))
        (should (equal "create" called-args))))))

(mevedel-deftest mevedel-worktree-list--delete
  (:after-each (mevedel-worktree-test--cleanup-surfaces))
  ,test
  (test)

  :doc "removes a clean selected .worktrees checkout and leaves its branch"
  (let* ((root (file-name-as-directory
                (make-temp-file "mevedel-worktree-delete-" t)))
         (workspace (mevedel-worktree-test--workspace root))
         (path (file-name-as-directory
                (file-name-concat root ".worktrees" "foo")))
         (data-buffer (generate-new-buffer " *mwt-delete-data*"))
         prompt message-text)
    (unwind-protect
        (progn
          (mevedel-worktree-test--init-repo root)
          (make-directory (file-name-concat root ".worktrees") t)
          (mevedel-worktree-test--git-ok
           root "worktree" "add" "-b" "worktree/foo" path)
          (cl-letf (((symbol-function 'yes-or-no-p)
                     (lambda (question)
                       (setq prompt question)
                       t))
                    ((symbol-function 'message)
                     (lambda (format-string &rest args)
                       (setq message-text
                             (apply #'format format-string args)))))
            (let ((buffer (mevedel-worktree-test--open-real-list
                           root workspace data-buffer)))
              (with-current-buffer buffer
                (mevedel-cockpit-goto-id path)
                (mevedel-worktree-list--delete nil))))
          (should-not (file-exists-p path))
          (mevedel-worktree-test--git-ok
           root "show-ref" "--verify" "refs/heads/worktree/foo")
          (should (string-match-p (regexp-quote path) prompt))
          (should (string-match-p "branch worktree/foo was left intact"
                                  message-text)))
      (mevedel-worktree-test--cleanup-surfaces data-buffer)
      (delete-directory root t)))

  :doc "refuses dirty worktrees before removing"
  (let* ((root (file-name-as-directory
                (make-temp-file "mevedel-worktree-dirty-delete-" t)))
         (workspace (mevedel-worktree-test--workspace root))
         (path (file-name-as-directory
                (file-name-concat root ".worktrees" "dirty")))
         (data-buffer (generate-new-buffer " *mwt-dirty-delete-data*")))
    (unwind-protect
        (progn
          (mevedel-worktree-test--init-repo root)
          (make-directory (file-name-concat root ".worktrees") t)
          (mevedel-worktree-test--git-ok
           root "worktree" "add" "-b" "worktree/dirty" path)
          (with-temp-file (file-name-concat path "dirty.txt")
            (insert "dirty\n"))
          (cl-letf (((symbol-function 'yes-or-no-p)
                     (lambda (&rest _)
                       (error "Should not prompt"))))
            (let ((buffer (mevedel-worktree-test--open-real-list
                           root workspace data-buffer)))
              (with-current-buffer buffer
                (mevedel-cockpit-goto-id path)
                (should-error (mevedel-worktree-list--delete nil)
                              :type 'user-error)))
            (should (file-exists-p path))))
      (mevedel-worktree-test--cleanup-surfaces data-buffer)
      (delete-directory root t)))

  :doc "force-removes a selected dirty .worktrees checkout"
  (let* ((root (file-name-as-directory
                (make-temp-file "mevedel-worktree-force-delete-" t)))
         (workspace (mevedel-worktree-test--workspace root))
         (path (file-name-as-directory
                (file-name-concat root ".worktrees" "force")))
         (data-buffer (generate-new-buffer " *mwt-force-delete-data*"))
         prompt)
    (unwind-protect
        (progn
          (mevedel-worktree-test--init-repo root)
          (make-directory (file-name-concat root ".worktrees") t)
          (mevedel-worktree-test--git-ok
           root "worktree" "add" "-b" "worktree/force" path)
          (with-temp-file (file-name-concat path "dirty.txt")
            (insert "dirty\n"))
          (cl-letf (((symbol-function 'yes-or-no-p)
                     (lambda (question)
                       (setq prompt question)
                       t))
                    ((symbol-function 'message)
                     #'ignore))
            (let ((buffer (mevedel-worktree-test--open-real-list
                           root workspace data-buffer)))
              (with-current-buffer buffer
                (mevedel-cockpit-goto-id path)
                (mevedel-worktree-list--delete t))))
          (should-not (file-exists-p path))
          (should (string-match-p "discard uncommitted changes" prompt))
          (mevedel-worktree-test--git-ok
           root "show-ref" "--verify" "refs/heads/worktree/force"))
      (mevedel-worktree-test--cleanup-surfaces data-buffer)
      (delete-directory root t)))

  :doc "refuses unsafe real worktree rows before removing"
  (let* ((root (file-name-as-directory
                (make-temp-file "mevedel-worktree-refuse-delete-" t)))
         (workspace (mevedel-worktree-test--workspace root))
         (worktrees-root (file-name-concat root ".worktrees"))
         (locked (file-name-as-directory
                  (file-name-concat worktrees-root "locked")))
         (live (file-name-as-directory
                (file-name-concat worktrees-root "live")))
         (outside (file-name-as-directory
                   (make-temp-name
                    (file-name-concat
                     (file-name-directory (directory-file-name root))
                     "outside"))))
         (data-buffer (generate-new-buffer " *mwt-refuse-delete-data*"))
         (live-buffer (generate-new-buffer " *mwt-refuse-live*")))
    (unwind-protect
        (progn
          (mevedel-worktree-test--init-repo root)
          (make-directory worktrees-root t)
          (mevedel-worktree-test--git-ok
           root "worktree" "add" "-b" "worktree/locked" locked)
          (mevedel-worktree-test--git-ok
           root "worktree" "lock" locked)
          (mevedel-worktree-test--git-ok
           root "worktree" "add" "-b" "worktree/live" live)
          (mevedel-worktree-test--git-ok
           root "worktree" "add" "-b" "worktree/outside" outside)
          (with-current-buffer live-buffer
            (setq-local mevedel--session
                        (mevedel-session-create "live" workspace live)))
          (let ((buffer (mevedel-worktree-test--open-real-list
                         root workspace data-buffer)))
            (dolist (path (list root locked live outside))
              (with-current-buffer buffer
                (mevedel-cockpit-surface-refresh path)
                (should-error (mevedel-worktree-list--delete nil)
                              :type 'user-error)))))
      (mevedel-worktree-test--cleanup-surfaces data-buffer live-buffer)
      (when (file-directory-p outside)
        (delete-directory outside t))
      (delete-directory root t)))

  :doc "refuses bare selected rows before removing"
  (let* ((root (file-name-as-directory
                (make-temp-file "mevedel-worktree-bare-delete-" t)))
         (workspace (mevedel-worktree-test--workspace root))
         (path (file-name-as-directory
                (file-name-concat root ".worktrees" "bare")))
         (status (plist-put
                  (mevedel-worktree-test--status
                   root
                   (list (list :path path :bare t :head "def")))
                  :workspace workspace))
         (data-buffer (generate-new-buffer " *mwt-bare-delete-data*")))
    (unwind-protect
        (progn
          (make-directory path t)
          (with-current-buffer data-buffer
            (setq-local default-directory root))
          (cl-letf (((symbol-function 'mevedel-worktree--collect-status)
                     (lambda (&optional _context) status))
                    ((symbol-function 'mevedel-worktree--git-result)
                     (lambda (&rest _)
                       (error "Should not remove"))))
            (let ((buffer (mevedel-worktree-list-open
                           (mevedel-worktree-test--context
                            status data-buffer data-buffer data-buffer))))
              (with-current-buffer buffer
                (mevedel-cockpit-goto-id path)
                (should-error (mevedel-worktree-list--delete nil)
                              :type 'user-error)))))
      (mevedel-worktree-test--cleanup-surfaces data-buffer)
      (delete-directory root t))))

(mevedel-deftest mevedel-worktree-list-delete
  (:after-each (mevedel-worktree-test--cleanup-surfaces))
  ,test
  (test)

  :doc "delegates to normal deletion"
  (let (force)
    (cl-letf (((symbol-function 'mevedel-worktree-list--delete)
               (lambda (value)
                 (setq force value)
                 :deleted)))
      (should (eq (mevedel-worktree-list-delete) :deleted))
      (should-not force))))

(mevedel-deftest mevedel-worktree-list-force-delete
  (:after-each (mevedel-worktree-test--cleanup-surfaces))
  ,test
  (test)

  :doc "delegates to force deletion"
  (let (force)
    (cl-letf (((symbol-function 'mevedel-worktree-list--delete)
               (lambda (value)
                 (setq force value)
                 :deleted)))
      (should (eq (mevedel-worktree-list-force-delete) :deleted))
      (should force))))

(mevedel-deftest mevedel-worktree-list-help ()
  ,test
  (test)

  :doc "opens worktree list help"
  (unwind-protect
      (progn
        (mevedel-worktree-list-help)
        (with-current-buffer mevedel-worktree-help-buffer-name
          (should (string-match-p "o    Open selected worktree session"
                                  (buffer-string)))
          (should (string-match-p "c    Create a linked worktree session"
                                  (buffer-string)))
          (should (string-match-p "d    Delete selected worktree"
                                  (buffer-string)))
          (should (string-match-p "D    Force-delete selected worktree"
                                  (buffer-string)))))
    (mevedel-worktree-test--cleanup-surfaces)))

(mevedel-deftest mevedel-worktree-list-quit ()
  ,test
  (test)

  :doc "delegates quit to the shared cockpit shell"
  (let (label)
    (cl-letf (((symbol-function 'mevedel-cockpit-quit)
               (lambda (quit-label)
                 (setq label quit-label))))
      (mevedel-worktree-list-quit))
    (should (equal label "worktree list"))))

(mevedel-deftest mevedel-worktree-list-open ()
  ,test
  (test)

  :doc "renders visible worktree rows"
  (let* ((root (file-name-as-directory
                (make-temp-file "mevedel-worktree-list-" t)))
         (status (mevedel-worktree-test--status root))
         (view-buffer (generate-new-buffer " *mwt-list-view*"))
         (data-buffer (generate-new-buffer " *mwt-list-data*")))
    (unwind-protect
        (let ((buffer (mevedel-worktree-test--open-list
                       status data-buffer view-buffer)))
          (with-current-buffer buffer
            (let ((rows (mevedel-test-tabulated-entries-cells)))
              (should (= 1 (length rows)))
              (should (equal (cdr (assoc root rows))
                             (list root "main" "abc123" "yes" "branch"
                                   ""))))))
      (mevedel-worktree-test--cleanup-surfaces view-buffer data-buffer)
      (delete-directory root t))))

(mevedel-deftest mevedel-cmd--worktree/status-surface ()
  ,test
  (test)

  :doc "keeps the superseded string renderer deleted"
  (should-not (fboundp 'mevedel-worktree--format-worktree-entry))
  (should-not (fboundp 'mevedel-worktree--format-status))

  :doc "routes blank and status commands to the worktree surface"
  (with-temp-buffer
    (let (opened-buffer)
      (cl-letf (((symbol-function 'mevedel-worktree-status-open)
                 (lambda () (setq opened-buffer (current-buffer)))))
        (dolist (args '("" "status"))
          (setq opened-buffer nil)
          (should (null (mevedel-cmd--worktree args)))
          (should (eq opened-buffer (current-buffer)))))))

  :doc "routes list commands to the tabulated worktree list"
  (with-temp-buffer
    (let (opened-buffer)
      (cl-letf (((symbol-function 'mevedel-worktree-list-open)
                 (lambda () (setq opened-buffer (current-buffer)))))
        (should (null (mevedel-cmd--worktree "list")))
        (should (eq opened-buffer (current-buffer)))))))


;;
;;; Create

(mevedel-deftest mevedel-worktree--open-session
  (:quiet t :doc "prefers a recorded target over a competing live session")
  ,test
  (test)
  (let* ((root (file-name-as-directory
                (make-temp-file "mevedel-worktree-resume-" t)))
         (directory (file-name-as-directory
                     (file-name-concat root ".worktrees" "retry")))
         (save-path (file-name-as-directory
                     (file-name-concat root ".mevedel" "target")))
         (workspace (mevedel-worktree-test--workspace root))
         (buffer (generate-new-buffer " *mevedel-worktree-resumed*"))
         (competitor
          (generate-new-buffer " *mevedel-worktree-competitor*"))
         (target-session
          (mevedel-session--create
           :name "target" :session-id "target-id"
           :working-directory directory))
         (competitor-session
          (mevedel-session--create
           :name "competitor" :session-id "competitor-id"
           :working-directory directory))
         restored displayed)
    (unwind-protect
        (progn
          (make-directory directory t)
          (make-directory save-path t)
          (with-current-buffer buffer
            (setq-local mevedel--session target-session))
          (with-current-buffer competitor
            (setq-local mevedel--session competitor-session))
          (cl-letf (((symbol-function 'mevedel--workspace-sessions)
                     (lambda (_) (list (cons "competitor" competitor))))
                    ((symbol-function
                      'mevedel-session-persistence-list-sessions)
                     (lambda (_)
                       (ert-fail "Recorded identity fell back to discovery")))
                    ((symbol-function 'mevedel-session-persistence-restore)
                     (lambda (path &optional _source _session workspace-arg)
                       (setq restored (list path workspace-arg))
                       buffer))
                    ((symbol-function 'mevedel--chat-buffer)
                     (lambda (&rest _)
                       (ert-fail "Durable target was not resumed")))
                    ((symbol-function 'mevedel--display-chat-buffer)
                     (lambda (candidate) (setq displayed candidate))))
            (should (eq buffer
                        (mevedel-worktree--open-session
                         workspace directory
                         (list :target-directory directory
                               :target-save-path save-path
                               :target-session-id "target-id"))))
            (should (equal (list save-path workspace) restored))
            (should (eq buffer displayed))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (when (buffer-live-p competitor) (kill-buffer competitor))
      (delete-directory root t)
      (mevedel-workspace-clear-registry)))

  :doc "resumes one real persisted target and rejects ambiguous durable targets"
  (let* ((root (file-name-as-directory
                (make-temp-file "mevedel-worktree-durable-" t)))
         (directory (file-name-as-directory
                     (file-name-concat root ".worktrees" "retry")))
         (name (file-name-nondirectory (directory-file-name root)))
         (workspace (progn
                      (mevedel-workspace-clear-registry)
                      (mevedel-workspace-get-or-create
                       'file name root name)))
         buffers sessions restored)
    (unwind-protect
        (progn
          (make-directory directory t)
          (mevedel-workspace-identity-ensure root)
          (dolist (session-name '("target" "competitor"))
            (let ((session
                   (mevedel-session-create
                    session-name workspace directory))
                  (buffer
                   (generate-new-buffer
                    (format " *mevedel-worktree-%s*" session-name))))
              (with-current-buffer buffer
                (org-mode)
                (setq-local mevedel--session session)
                (insert session-name "\n")
                (mevedel-session-artifacts-save session buffer))
              (push session sessions)
              (push buffer buffers)))
          (let ((target-id (mevedel-session-session-id (cadr sessions))))
            (mevedel-session-persistence-lock-release
             (mevedel-session-save-path (cadr sessions)) (cadr sessions))
            (with-current-buffer (cadr buffers)
              (set-buffer-modified-p nil))
            (kill-buffer (cadr buffers))
            (setf (cadr buffers) nil)
            (cl-letf (((symbol-function 'mevedel--display-chat-buffer)
                       #'ignore))
              (setq restored
                    (mevedel-worktree--open-session
                     workspace directory
                     (list :target-directory directory
                           :target-save-path
                           (mevedel-session-save-path (cadr sessions))
                           :target-session-id target-id))))
            (should (equal target-id
                           (mevedel-session-session-id
                            (buffer-local-value
                             'mevedel--session restored)))))
          (when (buffer-live-p restored)
            (let ((session (buffer-local-value 'mevedel--session restored)))
              (mevedel-session-persistence-lock-release
               (mevedel-session-save-path session) session))
            (with-current-buffer restored (set-buffer-modified-p nil))
            (kill-buffer restored)
            (setq restored nil))
          (dolist (pair (cl-mapcar #'cons buffers sessions))
            (when (buffer-live-p (car pair))
              (mevedel-session-persistence-lock-release
               (mevedel-session-save-path (cdr pair)) (cdr pair))
              (with-current-buffer (car pair) (set-buffer-modified-p nil))
              (kill-buffer (car pair))))
          (setq buffers nil)
          (cl-letf (((symbol-function 'mevedel--display-chat-buffer)
                     #'ignore))
            (should-error
             (mevedel-worktree--open-session
              workspace directory (list :target-directory directory)))))
      (when (buffer-live-p restored)
        (with-current-buffer restored (set-buffer-modified-p nil))
        (kill-buffer restored))
      (dolist (pair (cl-mapcar #'cons buffers sessions))
        (when (buffer-live-p (car pair))
          (ignore-errors
            (mevedel-session-persistence-lock-release
             (mevedel-session-save-path (cdr pair)) (cdr pair)))
          (with-current-buffer (car pair) (set-buffer-modified-p nil))
          (kill-buffer (car pair))))
      (delete-directory root t)
      (mevedel-workspace-clear-registry))))

(mevedel-deftest mevedel-worktree-create-session ()
  ,test
  (test)

  :doc "fails Git preflight before creating Plan Worktree state"
  (let ((root (file-name-as-directory
               (make-temp-file "mevedel-plan-worktree-no-git-" t)))
        opened)
    (unwind-protect
        (mevedel-worktree-test--with-session root
          (cl-letf (((symbol-function 'mevedel-worktree--git-result)
                     (lambda (_directory &rest _args)
                       (mevedel-worktree-test--git-result
                        "git: command not found" 127)))
                    ((symbol-function 'mevedel-worktree--open-session)
                     (lambda (&rest _args) (setq opened t))))
            (should-error
             (mevedel-worktree-create-session
              "worktree/accepted-plan" "Accepted Plan implementation" t)
             :type 'user-error)
            (should-not opened)
            (should-not (file-exists-p
                         (file-name-concat root ".worktrees")))))
      (delete-directory root t)
      (mevedel-workspace-clear-registry)))
  :doc "fails worktree capability preflight before creating Plan state"
  (let ((root (file-name-as-directory
               (make-temp-file "mevedel-plan-worktree-unsupported-" t)))
        opened)
    (unwind-protect
        (mevedel-worktree-test--with-session root
          (cl-letf (((symbol-function 'mevedel-worktree--git-result)
                     (lambda (_directory &rest args)
                       (if (equal args '("worktree" "list" "--porcelain"))
                           (mevedel-worktree-test--git-result
                            "git: 'worktree' is not a git command" 129)
                         (mevedel-worktree-test--base-response root args))))
                    ((symbol-function 'mevedel-worktree--open-session)
                     (lambda (&rest _args) (setq opened t))))
            (let ((err
                   (should-error
                    (mevedel-worktree-create-session
                     "worktree/accepted-plan"
                     "Accepted Plan implementation" t)
                    :type 'user-error)))
              (should (string-match-p "Git worktree support is required"
                                      (error-message-string err))))
            (should-not opened)
            (should-not (file-exists-p
                         (file-name-concat root ".worktrees")))))
      (delete-directory root t)
      (mevedel-workspace-clear-registry)))
  :doc "rejects an escaping local worktrees symlink before Git mutation"
  (let* ((base (file-name-as-directory
                (make-temp-file "mevedel-plan-worktree-symlink-" t)))
         (root (file-name-as-directory (file-name-concat base "repo")))
         (outside (file-name-as-directory
                   (file-name-concat base "outside")))
         opened)
    (unwind-protect
        (progn
          (make-directory root t)
          (make-directory outside t)
          (mevedel-worktree-test--init-repo root)
          (make-symbolic-link
           outside (file-name-concat root ".worktrees"))
          (mevedel-worktree-test--with-session root
            (cl-letf (((symbol-function 'mevedel-worktree--open-session)
                       (lambda (&rest _args) (setq opened t))))
              (should-error
               (mevedel-worktree-create-session
                "worktree/escape" nil t)
               :type 'user-error)
              (should-not opened)
              (should-not
               (file-exists-p (file-name-concat outside "escape")))
              (should-not
               (eq 0
                   (mevedel-worktree--git-exit
                    root "show-ref" "--verify" "--quiet"
                    "refs/heads/worktree/escape"))))))
      (delete-directory base t)
      (mevedel-workspace-clear-registry)))
  :doc "creates Plan Worktree with target Git on the source target"
  (let* ((root (file-name-as-directory
                (make-temp-file "mevedel-plan-worktree-remote-" t)))
         (remote-root (format "/mevedelmock:plan:%s" root))
         (new-buffer (generate-new-buffer
                      " *mevedel-plan-worktree-remote*"))
         opened-directory)
    (unwind-protect
        (mevedel-test--with-local-shell-tramp '("plan")
          (mevedel-worktree-test--init-repo remote-root)
          (let* ((workspace (mevedel-worktree-test--workspace remote-root))
                 (session (mevedel-session-create
                           "remote" workspace remote-root)))
            (with-temp-buffer
              (setq-local mevedel--session session)
              (setq-local default-directory remote-root)
              (cl-letf (((symbol-function 'mevedel-worktree--open-session)
                         (lambda (_workspace directory)
                           (setq opened-directory directory)
                           new-buffer)))
                (let ((result
                       (mevedel-worktree-create-session
                        "worktree/accepted-plan"
                        "Accepted Plan implementation" t)))
                  (should (equal opened-directory
                                 (plist-get result :directory)))
                  (should (equal (file-remote-p remote-root)
                                 (file-remote-p opened-directory)))
                  (should
                   (equal
                    "worktree/accepted-plan"
                    (plist-get
                     (mevedel-worktree-test--git-ok
                      opened-directory "branch" "--show-current")
                     :output))))))))
      (when (buffer-live-p new-buffer)
        (kill-buffer new-buffer))
      (delete-directory root t)
      (mevedel-workspace-clear-registry)))
  :doc "returns a clean worktree session created with the source-session default"
  (let* ((root (file-name-as-directory
                (make-temp-file "mevedel-worktree-interface-" t)))
         (git-dir (file-name-concat root ".git"))
         (new-buffer (generate-new-buffer " *mevedel-worktree-interface*"))
         prompted-default)
    (unwind-protect
        (progn
          (make-directory (file-name-concat git-dir "info") t)
          (mevedel-worktree-test--with-session root
            (cl-letf (((symbol-function 'read-string)
                       (lambda (_prompt &optional _initial _history default
                                &rest _)
                         (setq prompted-default default)
                         default))
                      ((symbol-function 'mevedel-worktree--open-session)
                       (lambda (_workspace _dir) new-buffer))
                      ((symbol-function 'mevedel-worktree--git-result)
                       (lambda (_dir &rest args)
                         (mevedel-worktree-test--base-response root args))))
              (let* ((result (mevedel-worktree-create-session nil nil t))
                     (expected-dir
                      (file-name-as-directory
                       (file-name-concat root ".worktrees" "main"))))
                (should (equal "worktree/main" prompted-default))
                (should (eq new-buffer (plist-get result :buffer)))
                (should (equal "worktree/main" (plist-get result :branch)))
                (should (equal expected-dir (plist-get result :directory)))
                (should-not (plist-get result :warnings))))))
      (when (buffer-live-p new-buffer) (kill-buffer new-buffer))
      (delete-directory root t)))
  :doc "reopens the exact existing Plan worktree without a second Git creation"
  (let* ((root (file-name-as-directory
                (make-temp-file "mevedel-plan-worktree-reopen-" t)))
         (workspace (mevedel-worktree-test--workspace root))
         (session (mevedel-session-create "main" workspace root))
         (new-buffer (generate-new-buffer " *mevedel-worktree-reopen*"))
         (git-function (symbol-function 'mevedel-worktree--git-result))
         (creates 0)
         resume)
    (unwind-protect
        (progn
          (mevedel-worktree-test--init-repo root)
          (with-temp-buffer
            (setq-local mevedel--session session)
            (setq-local default-directory root)
            (cl-letf (((symbol-function 'mevedel-worktree--git-result)
                       (lambda (directory &rest args)
                         (when (equal (seq-take args 2)
                                      '("worktree" "add"))
                           (cl-incf creates))
                         (apply git-function directory args)))
                      ((symbol-function 'mevedel-worktree--open-session)
                       (lambda (_workspace _directory &optional resume-arg)
                         (setq resume resume-arg)
                         new-buffer)))
              (mevedel-worktree-create-session
               "plan/retry" "Accepted Plan implementation" t)
              (let ((result
                     (mevedel-worktree-create-session
                      "plan/retry" "Accepted Plan implementation" t
                      (list :target-directory
                            (file-name-as-directory
                             (file-name-concat
                              root ".worktrees" "retry"))))))
                (should (= 1 creates))
                (should resume)
                (should (plist-get result :reused))
                (should (eq new-buffer (plist-get result :buffer))))
              (should-error
               (mevedel-worktree-create-session
                "other/retry" "Accepted Plan implementation" t
                (list :target-directory
                      (file-name-as-directory
                       (file-name-concat root ".worktrees" "retry"))))
               :type 'user-error)
              (should (= 1 creates))))
      (when (buffer-live-p new-buffer) (kill-buffer new-buffer))
      (delete-directory root t)
      (mevedel-workspace-clear-registry)))))

(mevedel-deftest mevedel-worktree--session-directory
  (:doc "derives the accepted branch's deterministic workspace path")
  ,test
  (test)
  (let* ((root (file-name-as-directory
                (make-temp-file "mevedel-worktree-reservation-" t)))
         (workspace (mevedel-worktree-test--workspace root))
         (session (mevedel-session-create "main" workspace root)))
    (unwind-protect
        (progn
          (mevedel-worktree-test--init-repo root)
          (with-temp-buffer
            (setq-local mevedel--session session)
            (setq-local default-directory root)
            (should
             (equal
              (file-name-as-directory
               (file-name-concat root ".worktrees" "accepted"))
              (mevedel-worktree--session-directory
               "plan/accepted")))))
      (delete-directory root t)
      (mevedel-workspace-clear-registry))))

(mevedel-deftest mevedel-cmd--worktree/create ()
  ,test
  (test)

  :doc "creates a clean worktree session with prompted branch name"
  (let* ((root (file-name-as-directory
                (make-temp-file "mevedel-worktree-create-" t)))
         (git-dir (file-name-concat root ".git"))
         (opened nil)
         (calls nil))
    (unwind-protect
        (progn
          (make-directory (file-name-concat git-dir "info") t)
          (mevedel-worktree-test--with-session root
            (cl-letf (((symbol-function 'read-string)
                       (lambda (&rest _) "worktree/foo"))
                      ((symbol-function 'mevedel-worktree--open-session)
                       (lambda (_workspace dir)
                         (setq opened dir)
                         (current-buffer)))
                      ((symbol-function 'mevedel-worktree--git-result)
                       (lambda (_dir &rest args)
                         (push args calls)
                         (mevedel-worktree-test--base-response
                          root args))))
              (let ((out (mevedel-cmd--worktree "create --clean")))
                (should (string-match-p
                         "Created worktree session `worktree/foo'"
                         out))
                (should (equal (file-name-as-directory
                                (file-name-concat root ".worktrees" "foo"))
                               opened))
                (should (member
                         `("worktree" "add" "-b" "worktree/foo"
                           ,(file-name-as-directory
                             (file-name-concat root ".worktrees" "foo")))
                         calls))
                (should (file-directory-p
                         (file-name-concat root ".worktrees")))
                (should (with-temp-buffer
                          (insert-file-contents
                           (file-name-concat git-dir "info" "exclude"))
                          (search-forward "/.worktrees/" nil t)))))))
      (delete-directory root t)))

  :doc "blocks create during an active request"
  (let ((root (file-name-as-directory
               (make-temp-file "mevedel-worktree-active-" t))))
    (unwind-protect
        (mevedel-worktree-test--with-session root
          (setq-local mevedel--current-request t)
          (should-error (mevedel-cmd--worktree "create worktree/foo --clean")
                        :type 'user-error))
      (delete-directory root t)))

  :doc "rejects invalid names before Git add"
  (let ((root (file-name-as-directory
               (make-temp-file "mevedel-worktree-invalid-" t)))
        (calls nil))
    (unwind-protect
        (mevedel-worktree-test--with-session root
          (cl-letf (((symbol-function 'mevedel-worktree--git-result)
                     (lambda (_dir &rest args)
                       (push args calls)
                       (if (equal args '("check-ref-format" "--branch"
                                         "bad name"))
                           (mevedel-worktree-test--git-result "" 1)
                         (mevedel-worktree-test--base-response
                          root args)))))
            (should-error (mevedel-cmd--worktree
                           "create \"bad name\" --clean")
                          :type 'user-error)
            (should-not (member '("worktree" "add") calls))))
      (delete-directory root t)))

  :doc "rejects Git-invalid ref names before worktree add"
  (let ((root (file-name-as-directory
               (make-temp-file "mevedel-worktree-invalid-ref-" t)))
        (calls nil))
    (unwind-protect
        (mevedel-worktree-test--with-session root
          (cl-letf (((symbol-function 'mevedel-worktree--git-result)
                     (lambda (_dir &rest args)
                       (push args calls)
                       (if (equal args '("check-ref-format" "--branch"
                                         "worktree/foo.lock"))
                           (mevedel-worktree-test--git-result "" 1)
                         (mevedel-worktree-test--base-response
                          root args)))))
            (should-error (mevedel-cmd--worktree
                           "create worktree/foo.lock --clean")
                          :type 'user-error)
            (should-not
             (cl-find-if
              (lambda (args)
                (equal (list (nth 0 args) (nth 1 args))
                       '("worktree" "add")))
              calls))))
      (delete-directory root t))))


;;
;;; Setup stub

(mevedel-deftest mevedel-worktree--format-stub ()
  ,test
  (test)

  :doc "renders remote setup context with target-native paths"
  (let* ((root (file-name-as-directory
                (make-temp-file "mevedel-worktree-stub-remote-" t)))
         (source (format "/mevedelmock:stub:%s" root))
         (worktree (file-name-concat source ".worktrees" "topic")))
    (unwind-protect
        (mevedel-test--with-local-shell-tramp '("stub")
          (let ((out (mevedel-worktree--format-stub
                      "main" source worktree "worktree/topic" nil nil)))
            (should (string-match-p
                     (regexp-quote
                      (format "Source directory: %s" root))
                     out))
            (should (string-match-p
                     (regexp-quote
                      (format "Worktree directory: %s"
                              (file-name-concat
                               root ".worktrees" "topic")))
                     out))
            (should-not (string-match-p "/mevedelmock:" out))))
      (delete-directory root t))))

(mevedel-deftest mevedel-cmd--worktree/setup-stub ()
  ,test
  (test)

  :doc "inserts and saves visible setup context without sending"
  (let* ((root (file-name-as-directory
                (make-temp-file "mevedel-worktree-stub-" t)))
         (git-dir (file-name-concat root ".git"))
         (new-buffer (generate-new-buffer " *mevedel-worktree-stub*"))
         (view-buffer (generate-new-buffer " *mevedel-worktree-stub-view*"))
         (saved nil)
         (sent nil))
    (unwind-protect
        (progn
          (make-directory (file-name-concat git-dir "info") t)
          (mevedel-worktree-test--with-session root
            (let* ((workspace (mevedel-session-workspace session))
                   (worktree-dir (file-name-as-directory
                                  (file-name-concat root ".worktrees" "foo")))
                   (new-session (mevedel-session-create
                                 ".worktrees:foo" workspace worktree-dir)))
              (with-current-buffer new-buffer
                (org-mode)
                (setq-local gptel-response-separator "\n\n")
                (setq-local gptel-prompt-prefix-alist
                            '((org-mode . "* User\n")))
                (setq-local mevedel--session new-session)
                (setq-local mevedel--view-buffer view-buffer))
              (mevedel-view--setup view-buffer new-buffer)
              (cl-letf (((symbol-function 'gptel-send)
                         (lambda (&rest _) (setq sent t)))
                        ((symbol-function 'mevedel-worktree--open-session)
                         (lambda (_workspace _dir) new-buffer))
                        ((symbol-function 'mevedel-session-artifacts-save)
                         (lambda (_session _buffer) (setq saved t)))
                        ((symbol-function 'mevedel-worktree--git-result)
                         (lambda (_dir &rest args)
                           (if (equal args '("status" "--short"))
                               (mevedel-worktree-test--git-result " M foo.el")
                             (mevedel-worktree-test--base-response
                              root args)))))
                (mevedel-cmd--worktree
                 "create worktree/foo --for \"fix the parser\"")
                (with-current-buffer new-buffer
                  (let ((text (buffer-substring-no-properties
                               (point-min) (point-max))))
                    (should (string-match-p
                             "Purpose: fix the parser" text))
                    (should (string-match-p
                             "source checkout is dirty" text))
                    (should (string-match-p
                             "Wait for the user's next prompt" text)))))
              (with-current-buffer view-buffer
                (let ((text (buffer-substring-no-properties
                             (point-min) mevedel-view--input-marker)))
                  (should (string-match-p
                           "Purpose: fix the parser" text))
                  (should-not mevedel-view--in-flight-turn-start)
                  (should-not mevedel-view--spinner-status)))
              (should saved)
              (should-not sent))))
      (when (buffer-live-p new-buffer)
        (kill-buffer new-buffer))
      (when (buffer-live-p view-buffer)
        (kill-buffer view-buffer))
      (delete-directory root t)))

  :doc "--clean suppresses setup context and save"
  (let* ((root (file-name-as-directory
                (make-temp-file "mevedel-worktree-clean-" t)))
         (git-dir (file-name-concat root ".git"))
         (new-buffer (generate-new-buffer " *mevedel-worktree-clean*"))
         (saved nil))
    (unwind-protect
        (progn
          (make-directory (file-name-concat git-dir "info") t)
          (mevedel-worktree-test--with-session root
            (let* ((workspace (mevedel-session-workspace session))
                   (worktree-dir (file-name-as-directory
                                  (file-name-concat root ".worktrees" "foo")))
                   (new-session (mevedel-session-create
                                 ".worktrees:foo" workspace worktree-dir)))
              (with-current-buffer new-buffer
                (org-mode)
                (setq-local gptel-response-separator "\n\n")
                (setq-local gptel-prompt-prefix-alist
                            '((org-mode . "* User\n")))
                (setq-local mevedel--session new-session))
              (cl-letf (((symbol-function 'mevedel-worktree--open-session)
                         (lambda (_workspace _dir) new-buffer))
                        ((symbol-function 'mevedel-session-artifacts-save)
                         (lambda (_session _buffer) (setq saved t)))
                        ((symbol-function 'mevedel-worktree--git-result)
                         (lambda (_dir &rest args)
                           (mevedel-worktree-test--base-response root args))))
                (mevedel-cmd--worktree
                 "create worktree/foo --for \"ignored\" --clean"))
              (with-current-buffer new-buffer
                (should (string-empty-p
                         (buffer-substring-no-properties
                          (point-min) (point-max)))))
              (should-not saved))))
      (when (buffer-live-p new-buffer)
        (kill-buffer new-buffer))
      (delete-directory root t))))

;;; test-mevedel-worktree.el ends here
