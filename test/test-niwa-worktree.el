;;; test-niwa-worktree.el --- Tests for worktree sessions -*- lexical-binding: t -*-

;;; Commentary:

;; Focused command-seam coverage for `/worktree'.

;;; Code:

(require 'niwa-report-test-support
         (file-name-concat
          (file-name-directory (or buffer-file-name load-file-name))
          "niwa-report-test-support"))

(require 'niwa-worktree)
(require 'niwa-chat)
(require 'niwa-cockpit)
(require 'niwa-session-persistence)
(require 'niwa-structs)
(require 'niwa-view)
(require 'niwa-workspace)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

;; `eask'
(defvar eask-depends-on)
(defvar eask-depends-on-dev)

;; `niwa-cockpit'
(defvar niwa-cockpit--context)

;; `niwa-skills'
(defvar niwa-slash-commands)

(defun niwa-worktree-test--workspace (root)
  "Return a test workspace rooted at ROOT."
  (niwa-workspace--create
   :type 'project
   :id root
   :root root
   :name "worktree-test"
   :file-cache (niwa-file-cache--create
                :table (make-hash-table :test #'equal)
                :order nil
                :total-bytes 0)))

(defun niwa-worktree-test--git-result (output &optional exit)
  "Return a mocked Git result with OUTPUT and EXIT."
  (list :exit (or exit 0) :output output))

(defun niwa-worktree-test--base-response (root args)
  "Return a base mocked Git response for ROOT and ARGS."
  (let ((git-dir (file-name-concat root ".git")))
    (cond
     ((equal args '("rev-parse" "--is-inside-work-tree"))
      (niwa-worktree-test--git-result "true"))
     ((equal args '("rev-parse" "--show-toplevel"))
      (niwa-worktree-test--git-result root))
     ((equal args '("rev-parse" "--git-dir"))
      (niwa-worktree-test--git-result git-dir))
     ((equal args '("rev-parse" "--git-common-dir"))
      (niwa-worktree-test--git-result git-dir))
     ((equal args '("rev-parse" "--show-superproject-working-tree"))
      (niwa-worktree-test--git-result ""))
     ((equal args '("branch" "--show-current"))
      (niwa-worktree-test--git-result "main"))
     ((equal args '("rev-parse" "--short" "HEAD"))
      (niwa-worktree-test--git-result "abc123"))
     ((equal args '("status" "--short"))
      (niwa-worktree-test--git-result ""))
     ((equal args '("worktree" "list" "--porcelain"))
      (niwa-worktree-test--git-result
       (format "worktree %s\nHEAD abc123\nbranch refs/heads/main\n" root)))
     ((equal args '("check-ignore" "-q" ".worktrees/"))
      (niwa-worktree-test--git-result "" 1))
     ((equal (list (nth 0 args) (nth 1 args))
             '("check-ref-format" "--branch"))
      (niwa-worktree-test--git-result ""))
     ((equal (list (nth 0 args) (nth 1 args)) '("worktree" "add"))
      (niwa-worktree-test--git-result "ok"))
     (t
      (niwa-worktree-test--git-result "")))))

(defun niwa-worktree-test--git-ok (directory &rest args)
  "Run Git ARGS in DIRECTORY and signal on failure."
  (let ((result (apply #'niwa-worktree--git-result directory args)))
    (unless (eq 0 (plist-get result :exit))
      (error "Git failed: %s" (plist-get result :output)))
    result))

(defun niwa-worktree-test--init-repo (root)
  "Initialize a real Git repository rooted at ROOT."
  (niwa-worktree-test--git-ok root "init")
  (niwa-worktree-test--git-ok root "config" "user.email"
                                  "niwa@example.invalid")
  (niwa-worktree-test--git-ok root "config" "user.name" "Niwa Test")
  (with-temp-file (file-name-concat root "file.txt")
    (insert "base\n"))
  (niwa-worktree-test--git-ok root "add" "file.txt")
  (niwa-worktree-test--git-ok root "commit" "-m" "init"))

(niwa-deftest niwa-worktree-related-directory-p
  (:doc "accepts registered related checkouts and rejects foreign or nested repositories")
  (let* ((base (make-temp-file "niwa-related-worktree-" t))
         (root (file-name-concat base "root"))
         (linked (file-name-concat base "linked checkout"))
         (foreign (file-name-concat base "foreign"))
         (subdir (file-name-concat linked "subdir")))
    (unwind-protect
        (progn
          (make-directory root)
          (make-directory foreign)
          (niwa-worktree-test--init-repo root)
          (niwa-worktree-test--init-repo foreign)
          (niwa-worktree-test--git-ok root "worktree" "add" "-b" "child" linked)
          (make-directory subdir)
          (should (niwa-worktree-related-directory-p linked root))
          (should (niwa-worktree-related-directory-p subdir root))
          (should-not (niwa-worktree-related-directory-p foreign root))
          (niwa-worktree-test--init-repo subdir)
          (should-not (niwa-worktree-related-directory-p subdir root)))
      (delete-directory base t))))

(defun niwa-worktree-test--open-real-list
    (root workspace data-buffer)
  "Open a worktree list backed by real Git state under ROOT."
  (with-current-buffer data-buffer
    (setq-local default-directory root))
  (niwa-worktree-list-open
   (list :view-buffer data-buffer
         :data-buffer data-buffer
         :origin-buffer data-buffer
         :session nil
         :workspace workspace)))

(defmacro niwa-worktree-test--with-session (root &rest body)
  "Run BODY in a temp buffer with a niwa session rooted at ROOT."
  (declare (indent 1))
  `(let* ((workspace (niwa-worktree-test--workspace ,root))
          (session (niwa-session-create "main" workspace ,root)))
     (make-directory (file-name-concat ,root ".git") t)
     (with-temp-buffer
       (setq-local niwa--session session)
       (setq-local default-directory (file-name-as-directory ,root))
       ,@body)))

(defun niwa-worktree-test--cleanup-surfaces (&rest buffers)
  "Kill worktree cockpit test buffers and BUFFERS."
  (dolist (name (list niwa-worktree-list-buffer-name
                      "*niwa worktree details*"
                      niwa-worktree-help-buffer-name))
    (when (get-buffer name)
      (kill-buffer name)))
  (dolist (buffer buffers)
    (when (buffer-live-p buffer)
      (kill-buffer buffer))))

(defun niwa-worktree-test--status (root &optional worktrees)
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

(defun niwa-worktree-test--context
    (status data-buffer &optional view-buffer origin-buffer)
  "Return a cockpit context for STATUS owned by DATA-BUFFER."
  (list :view-buffer (or view-buffer data-buffer)
        :data-buffer data-buffer
        :origin-buffer (or origin-buffer data-buffer)
        :session (plist-get status :session)
        :workspace (plist-get status :workspace)))

(defun niwa-worktree-test--install-context
    (status data-buffer &optional view-buffer origin-buffer)
  "Install a cockpit context for STATUS in DATA-BUFFER."
  (with-current-buffer data-buffer
    (setq-local niwa-cockpit--context
                (niwa-worktree-test--context
                 status data-buffer view-buffer origin-buffer))))

(defun niwa-worktree-test--open-list
    (status data-buffer &optional view-buffer)
  "Open a worktree list for STATUS owned by DATA-BUFFER."
  (let ((view-buffer (or view-buffer data-buffer)))
    (with-current-buffer data-buffer
      (setq-local niwa--view-buffer view-buffer)
      (cl-letf (((symbol-function 'niwa-worktree-collect-status)
                 (lambda (&optional _context) status))
                ((symbol-function 'niwa-worktree-list--sessions)
                 (lambda (_workspace path)
                   (when (plist-get status :workspace)
                     (if (equal path (plist-get status :directory))
                         '("main")
                       '("side"))))))
        (niwa-worktree-list-open
         (niwa-worktree-test--context
          status data-buffer view-buffer data-buffer))))))


;;
;;; Installation

(niwa-deftest niwa-worktree-install-slash-command
  (:doc "installs and uninstalls only the worktree slash command")
  (let ((old (and (boundp 'niwa-slash-commands)
                  niwa-slash-commands)))
    (unwind-protect
        (progn
          (setq niwa-slash-commands '(("tokens" . ignore)))
          (niwa-worktree-install-slash-command)
          (should (eq #'niwa-cmd--worktree
                      (cdr (assoc "worktree" niwa-slash-commands))))
          (niwa-worktree-uninstall-slash-command)
          (should-not (assoc "worktree" niwa-slash-commands))
          (should (assoc "tokens" niwa-slash-commands)))
      (setq niwa-slash-commands old))))


;;
;;; Git helpers

(niwa-deftest niwa-worktree--git-result ()
  ,test
  (test)

  :doc "rejects a Git path argument naming another execution target"
  (let* ((root (file-name-as-directory
                (make-temp-file "niwa-worktree-target-" t)))
         (source (format "/niwamock:first:%s" root))
         (other (format "/niwamock:second:%sother" root)))
    (unwind-protect
        (niwa-test--with-local-shell-tramp '("first" "second")
          (should-error
           (niwa-worktree--git-result
            source "worktree" "add" other "HEAD")
           :type 'niwa-execution-target-error))
      (delete-directory root t)))

  :doc "does not forward client-only environment variables to target Git"
  (let ((process-environment
         (cons "NIWA_CLIENT_SECRET=do-not-forward" process-environment)))
    (cl-letf (((symbol-function 'process-file)
               (lambda (_program _in _destination _display &rest _args)
                 (should-not (getenv "NIWA_CLIENT_SECRET"))
                 0)))
      (should
       (eq 0
           (plist-get
            (niwa-worktree--git-result
             "/ssh:user@host:/srv/project/" "status" "--short")
            :exit))))))

(niwa-deftest niwa-worktree--ensure-worktree ()
  ,test
  (test)

  :doc "reports missing target Git worktree support directly"
  (let ((root (file-name-as-directory
               (make-temp-file "niwa-worktree-unsupported-" t)))
        calls)
    (unwind-protect
        (cl-letf (((symbol-function 'niwa-worktree--git-result)
                   (lambda (_directory &rest args)
                     (push args calls)
                     (niwa-worktree-test--git-result
                      "git: 'worktree' is not a git command" 129))))
          (let ((err (should-error
                      (niwa-worktree--ensure-worktree root)
                      :type 'user-error)))
            (should (string-match-p "Git worktree support is required"
                                    (error-message-string err)))
            (should (equal '(("worktree" "list" "--porcelain")) calls))))
      (delete-directory root t))))

(niwa-deftest niwa-worktree--parse-worktree-list ()
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
         (entries (niwa-worktree--parse-worktree-list output)))
    (should (= 3 (length entries)))
    (should (equal (plist-get (nth 0 entries) :branch) "main"))
    (should (plist-get (nth 1 entries) :detached))
    (should (equal (plist-get (nth 1 entries) :locked) "maint"))
    (should (plist-get (nth 2 entries) :bare))
    (should (equal (plist-get (nth 2 entries) :prunable) "stale"))))

(niwa-deftest niwa-worktree-fork-preflight ()
  ,test
  (test)
  :doc "captures the full current HEAD and rejects non-Git sessions"
  (let ((root (file-name-as-directory
               (make-temp-file "niwa-worktree-fork-preflight-" t))))
    (unwind-protect
        (let* ((workspace (niwa-worktree-test--workspace root))
               (session (niwa-session-create "main" workspace root)))
          (should-error (niwa-worktree-fork-preflight session)
                        :type 'user-error)
          (niwa-worktree-test--init-repo root)
          (let ((context (niwa-worktree-fork-preflight session)))
            (should (equal root (plist-get context :source-root)))
            (should
             (equal
              (plist-get
               (niwa-worktree-test--git-ok root "rev-parse" "HEAD")
               :output)
              (plist-get context :base-commit)))))
      (delete-directory root t)
      (niwa-workspace-clear-registry)))
  :doc "reports a missing target Git before repository inspection"
  (let* ((root (file-name-as-directory
                (make-temp-file "niwa-worktree-no-git-" t)))
         (workspace (niwa-worktree-test--workspace root))
         (session (niwa-session-create "main" workspace root))
         calls)
    (unwind-protect
        (cl-letf (((symbol-function 'niwa-worktree--git-result)
                   (lambda (_directory &rest args)
                     (push args calls)
                     (niwa-worktree-test--git-result
                      "git: command not found" 127))))
          (let ((err (should-error
                      (niwa-worktree-fork-preflight session)
                      :type 'user-error)))
            (should (string-match-p "Git is required.*execution target"
                                    (error-message-string err)))
            (should (equal '(("--version")) calls))))
      (delete-directory root t)
      (niwa-workspace-clear-registry))))

(niwa-deftest niwa-worktree-fork-reservation ()
  ,test
  (test)
  :doc "uses the first suffix free in both Git and the worktree directory"
  (let ((root (file-name-as-directory
               (make-temp-file "niwa-worktree-fork-reserve-" t))))
    (unwind-protect
        (let* ((workspace (niwa-worktree-test--workspace root))
               (session (niwa-session-create "main" workspace root)))
          (niwa-worktree-test--init-repo root)
          (niwa-worktree-test--git-ok
           root "branch" "worktree/main-fork-1")
          (make-directory
           (file-name-concat root ".worktrees" "main-fork-2") t)
          (let ((reservation
                 (niwa-worktree-fork-reservation session)))
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
      (niwa-workspace-clear-registry))))

(niwa-deftest niwa-worktree-fork-validate-reservation ()
  ,test
  (test)
  :doc "accepts the unchanged target and reports an exact artifact conflict"
  (let ((root (file-name-as-directory
               (make-temp-file "niwa-worktree-fork-validate-" t))))
    (unwind-protect
        (let* ((workspace (niwa-worktree-test--workspace root))
               (session (niwa-session-create "main" workspace root)))
          (niwa-worktree-test--init-repo root)
          (let ((reservation
                 (niwa-worktree-fork-reservation session)))
            (should
             (eq reservation
                 (niwa-worktree-fork-validate-reservation
                  session reservation)))
            (make-directory (plist-get reservation :directory) t)
            (let ((err
                   (should-error
                    (niwa-worktree-fork-validate-reservation
                     session reservation)
                    :type 'user-error)))
              (should
               (string-match-p
                (regexp-quote
                 (plist-get reservation :cleanup-command))
                (error-message-string err))))))
      (delete-directory root t)
      (niwa-workspace-clear-registry))))

(niwa-deftest niwa-worktree-fork-create ()
  ,test
  (test)
  :doc "checks Git before creating the reserved directory"
  (let* ((root (file-name-as-directory
                (make-temp-file "niwa-worktree-create-no-git-" t)))
         (directory (file-name-as-directory
                     (file-name-concat root ".worktrees" "fork")))
         (reservation (list :source-directory root
                            :directory directory
                            :branch "worktree/fork"
                            :base-commit "deadbeef"
                            :common-git-dir
                            (file-name-concat root ".git"))))
    (unwind-protect
        (cl-letf (((symbol-function 'niwa-worktree--git-result)
                   (lambda (_directory &rest _args)
                     (niwa-worktree-test--git-result
                      "git: command not found" 127))))
          (should-error (niwa-worktree-fork-create reservation)
                        :type 'user-error)
          (should-not (file-exists-p
                       (file-name-directory
                        (directory-file-name directory)))))
      (delete-directory root t)))
  :doc "checks Git worktree support before creating the reserved directory"
  (let* ((root (file-name-as-directory
                (make-temp-file "niwa-worktree-create-unsupported-" t)))
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
          (cl-letf (((symbol-function 'niwa-worktree--git-result)
                     (lambda (_directory &rest args)
                       (if (equal args '("worktree" "list" "--porcelain"))
                           (niwa-worktree-test--git-result
                            "git: 'worktree' is not a git command" 129)
                         (niwa-worktree-test--git-result "")))))
            (should-error (niwa-worktree-fork-create reservation)
                          :type 'user-error)
            (should-not (file-exists-p
                         (file-name-directory
                          (directory-file-name directory))))))
      (delete-directory root t)))
  :doc "rejects a foreign target before commands or directory creation"
  (let* ((root (file-name-as-directory
                (make-temp-file "niwa-worktree-create-target-" t)))
         (source (format "/niwamock:source:%s" root))
         (directory
          (file-name-as-directory
           (format "/niwamock:foreign:%s"
                   (file-name-concat root "foreign" "fork"))))
         (reservation (list :source-directory source
                            :directory directory
                            :branch "worktree/fork"
                            :base-commit "deadbeef"
                            :common-git-dir
                            (file-name-concat source ".git")))
         calls)
    (unwind-protect
        (niwa-test--with-local-shell-tramp '("source" "foreign")
          (cl-letf (((symbol-function 'niwa-worktree--git-result)
                     (lambda (_directory &rest args)
                       (push args calls)
                       (niwa-worktree-test--git-result ""))))
            (should-error (niwa-worktree-fork-create reservation)
                          :type 'niwa-execution-target-error)
            (should-not calls)
            (should-not (file-exists-p
                         (file-name-concat root "foreign")))))
      (delete-directory root t)))
  :doc "rejects a remote escape introduced after reservation"
  (let* ((base (file-name-as-directory
                (make-temp-file "niwa-worktree-fork-symlink-" t)))
         (root (file-name-as-directory (file-name-concat base "repo")))
         (outside (file-name-as-directory
                   (file-name-concat base "outside")))
         (remote-root (format "/niwamock:escape:%s" root)))
    (unwind-protect
        (niwa-test--with-local-shell-tramp '("escape")
          (make-directory root t)
          (make-directory outside t)
          (niwa-worktree-test--init-repo remote-root)
          (let* ((workspace
                  (niwa-worktree-test--workspace remote-root))
                 (session
                  (niwa-session-create "remote" workspace remote-root))
                 (reservation
                  (niwa-worktree-fork-reservation session)))
            (make-symbolic-link
             outside (file-name-concat root ".worktrees"))
            (should-error (niwa-worktree-fork-create reservation)
                          :type 'user-error)
            (should-not
             (file-exists-p
              (file-name-concat outside "remote-fork-1")))
            (should-not
             (eq 0
                 (niwa-worktree--git-exit
                  remote-root "show-ref" "--verify" "--quiet"
                  "refs/heads/worktree/remote-fork-1")))))
      (delete-directory base t)
      (niwa-workspace-clear-registry)))
  :doc "creates a linked worktree and branch at the reserved base commit"
  (let ((root (file-name-as-directory
               (make-temp-file "niwa-worktree-fork-create-" t)))
        (niwa-worktree-setup-enabled t)
        (niwa-worktree-setup-command '("sh" "-c" "touch setup-ready")))
    (unwind-protect
        (let* ((workspace (niwa-worktree-test--workspace root))
               (session (niwa-session-create "main" workspace root)))
          (niwa-worktree-test--init-repo root)
          (let* ((reservation
                  (niwa-worktree-fork-reservation session))
                 (directory (plist-get reservation :directory)))
            (niwa-test--with-captured-messages nil
              (niwa-worktree-fork-create reservation))
            (should (file-directory-p directory))
            (should (file-exists-p (file-name-concat directory "setup-ready")))
            (should
             (equal
              (plist-get reservation :base-commit)
              (plist-get
               (niwa-worktree-test--git-ok
                directory "rev-parse" "HEAD")
               :output)))
            (should
             (equal
              (plist-get reservation :branch)
              (plist-get
               (niwa-worktree-test--git-ok
                directory "branch" "--show-current")
               :output)))))
      (delete-directory root t)
      (niwa-workspace-clear-registry)
      (when (get-buffer "*niwa worktree setup*")
        (kill-buffer "*niwa worktree setup*"))))
  :doc "forks an existing linked worktree into a sibling at its current HEAD"
  (let ((root (file-name-as-directory
               (make-temp-file "niwa-worktree-fork-sibling-" t))))
    (unwind-protect
        (let* ((workspace (niwa-worktree-test--workspace root))
               (source
                (file-name-as-directory
                 (file-name-concat root ".worktrees" "source"))))
          (niwa-worktree-test--init-repo root)
          (make-directory (file-name-directory
                           (directory-file-name source))
                          t)
          (niwa-worktree-test--git-ok
           root "worktree" "add" "-b" "topic" source "HEAD")
          (write-region "source head\n" nil
                        (file-name-concat source "file.txt")
                        nil 'silent)
          (niwa-worktree-test--git-ok source "add" "file.txt")
          (niwa-worktree-test--git-ok
           source "commit" "-m" "source head")
          (let* ((session
                  (niwa-session-create "nested" workspace source))
                 (reservation
                  (niwa-worktree-fork-reservation session))
                 (target (plist-get reservation :directory))
                 (source-head
                  (plist-get
                   (niwa-worktree-test--git-ok
                    source "rev-parse" "HEAD")
                   :output)))
            (should
             (equal
              (file-name-directory (directory-file-name source))
              (file-name-directory (directory-file-name target))))
            (should-not (file-in-directory-p target source))
            (niwa-worktree-fork-create reservation)
            (should
             (equal
              source-head
              (plist-get
               (niwa-worktree-test--git-ok
                target "rev-parse" "HEAD")
               :output)))))
      (delete-directory root t)
      (niwa-workspace-clear-registry)))
  :doc "creates the linked worktree with target Git and native path arguments"
  (let* ((root (file-name-as-directory
                (make-temp-file "niwa-worktree-remote-" t)))
         (remote-root (format "/niwamock:worktree:%s" root)))
    (unwind-protect
        (niwa-test--with-local-shell-tramp '("worktree")
          (let* ((workspace (niwa-worktree-test--workspace remote-root))
                 (session (niwa-session-create
                           "remote" workspace remote-root)))
            (niwa-worktree-test--init-repo remote-root)
            (let* ((reservation
                    (niwa-worktree-fork-reservation session))
                   (directory (plist-get reservation :directory)))
              (should (equal remote-root
                             (plist-get reservation :source-root)))
              (should (file-remote-p directory))
              (should-not
               (string-match-p "/niwamock:"
                               (plist-get reservation :cleanup-command)))
              (niwa-worktree-fork-create reservation)
              (should (file-directory-p directory))
              (should
               (equal
                (plist-get reservation :branch)
                (plist-get
                 (niwa-worktree-test--git-ok
                  directory "branch" "--show-current")
                 :output))))))
      (delete-directory root t)
      (niwa-workspace-clear-registry))))

(niwa-deftest niwa-worktree-validate-branch-name ()
  ,test
  (test)

  :doc "checks target Git before validating a Plan Worktree branch"
  (let ((root (file-name-as-directory
               (make-temp-file "niwa-plan-worktree-no-git-" t)))
        calls)
    (unwind-protect
        (cl-letf (((symbol-function 'niwa-worktree--git-result)
                   (lambda (_directory &rest args)
                     (push args calls)
                     (niwa-worktree-test--git-result
                      "git: command not found" 127))))
          (let ((err
                 (should-error
                  (niwa-worktree-validate-branch-name
                   "worktree/accepted-plan" root)
                  :type 'user-error)))
            (should (string-match-p "Git is required.*execution target"
                                    (error-message-string err)))
            (should (equal '(("--version")) calls))))
      (delete-directory root t)))

  :doc "checks worktree support before accepting a Plan Worktree branch"
  (let ((root (file-name-as-directory
               (make-temp-file "niwa-plan-worktree-unsupported-" t)))
        calls)
    (unwind-protect
        (cl-letf (((symbol-function 'niwa-worktree--git-result)
                   (lambda (_directory &rest args)
                     (push args calls)
                     (if (equal args '("worktree" "list" "--porcelain"))
                         (niwa-worktree-test--git-result
                          "git: 'worktree' is not a git command" 129)
                       (niwa-worktree-test--git-result "")))))
          (should-error
           (niwa-worktree-validate-branch-name
            "worktree/accepted-plan" root)
           :type 'user-error)
          (should-not (member '("check-ref-format" "--branch"
                                "worktree/accepted-plan")
                              calls)))
      (delete-directory root t))))

(niwa-deftest niwa-worktree--validate-destination ()
  ,test
  (test)
  :doc "accepts a direct non-symlinked workspace destination"
  (let* ((root (file-name-as-directory
                (make-temp-file "niwa-worktree-destination-" t)))
         (directory
          (file-name-as-directory
           (file-name-concat root ".worktrees" "topic"))))
    (unwind-protect
        (should
         (equal directory
                (niwa-worktree--validate-destination root directory)))
      (delete-directory root t)))
  :doc "rejects a worktrees symlink even when its target stays inside"
  (let* ((root (file-name-as-directory
                (make-temp-file "niwa-worktree-inside-symlink-" t)))
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
           (niwa-worktree--validate-destination root directory)
           :type 'user-error))
      (delete-directory root t))))


;;
;;; Status collection

(niwa-deftest niwa-worktree-collect-status ()
  ,test
  (test)

  :doc "uses explicit cockpit context for session, workspace, and directory"
  (let* ((root (file-name-as-directory
                (make-temp-file "niwa-worktree-context-status-" t)))
         (workspace (niwa-worktree-test--workspace root))
         (session (niwa-session-create "main" workspace root))
         (data-buffer (generate-new-buffer " *mwt-context-status*"))
         (context (list :data-buffer data-buffer
                        :session session
                        :workspace workspace)))
    (unwind-protect
        (progn
          (make-directory (file-name-concat root ".git") t)
          (with-current-buffer data-buffer
            (setq-local default-directory root))
          (cl-letf (((symbol-function 'niwa-worktree--git-result)
                     (lambda (_dir &rest args)
                       (niwa-worktree-test--base-response root args))))
            (let ((status (niwa-worktree-collect-status context)))
              (should (eq (plist-get status :session) session))
              (should (eq (plist-get status :workspace) workspace))
              (should (equal (plist-get status :directory) root))
              (should (equal (plist-get status :repo-root) root)))))
      (niwa-worktree-test--cleanup-surfaces data-buffer)
      (delete-directory root t))))


;;
;;; Surface

(niwa-deftest niwa-worktree-status--data-buffer ()
  ,test
  (test)

  :doc "uses the cockpit context from the transient original buffer"
  (let* ((root (make-temp-file "niwa-worktree-status-data-" t))
         (status (niwa-worktree-test--status root))
         (data-buffer (generate-new-buffer " *mwt-status-data*")))
    (unwind-protect
        (progn
          (niwa-worktree-test--install-context status data-buffer)
          (with-temp-buffer
            (let ((transient--original-buffer data-buffer))
              (should (eq (niwa-worktree-status--data-buffer)
                          data-buffer)))))
      (niwa-worktree-test--cleanup-surfaces data-buffer)
      (delete-directory root t)))

  :doc "uses the current buffer's cockpit context outside transient commands"
  (let* ((root (make-temp-file "niwa-worktree-status-current-" t))
         (status (niwa-worktree-test--status root))
         (data-buffer (generate-new-buffer " *mwt-status-current*")))
    (unwind-protect
        (with-current-buffer data-buffer
          (niwa-worktree-test--install-context status data-buffer)
          (should (eq (niwa-worktree-status--data-buffer)
                      data-buffer)))
      (niwa-worktree-test--cleanup-surfaces data-buffer)
      (delete-directory root t))))

(niwa-deftest niwa-worktree--branch-head-label ()
  ,test
  (test)

  :doc "prefers branch, then detached head, then unavailable"
  (should (equal (niwa-worktree--branch-head-label "main" "abc123")
                 "main"))
  (should (equal (niwa-worktree--branch-head-label nil "abc123")
                 "detached at abc123"))
  (should (equal (niwa-worktree--branch-head-label nil nil)
                 "unavailable")))

(niwa-deftest niwa-worktree-status-summary ()
  ,test
  (test)

  :doc "summarizes a branch checkout"
  (cl-letf (((symbol-function 'niwa-worktree-collect-status)
             (lambda (&optional _context)
               (ert-fail "summary should not collect full status")))
            ((symbol-function 'niwa-worktree--git-success-output)
             (lambda (_directory &rest args)
               (cond
                ((equal args '("rev-parse" "--is-inside-work-tree"))
                 "true")
                ((equal args '("branch" "--show-current")) "main")
                ((equal args '("rev-parse" "--short" "HEAD")) "abc123")))))
    (should (equal '(:state git :label "main")
                   (niwa-worktree-status-summary '(:directory "/tmp")))))

  :doc "summarizes a detached checkout with the menu's compact label"
  (cl-letf (((symbol-function 'niwa-worktree--git-success-output)
             (lambda (_directory &rest args)
               (cond
                ((equal args '("rev-parse" "--is-inside-work-tree"))
                 "true")
                ((equal args '("branch" "--show-current")) "")
                ((equal args '("rev-parse" "--short" "HEAD")) "abc123")))))
    (should (equal '(:state git :label "detached abc123")
                   (niwa-worktree-status-summary '(:directory "/tmp")))))

  :doc "summarizes non-Git directories"
  (cl-letf (((symbol-function 'niwa-worktree--git-success-output)
             (lambda (_directory &rest args)
               (when (equal args '("rev-parse" "--is-inside-work-tree"))
                 nil))))
    (should (equal '(:state not-git :label "not-git")
                   (niwa-worktree-status-summary '(:directory "/tmp"))))))

(niwa-deftest niwa-worktree-status--description ()
  ,test
  (test)

  :doc "renders the compact one-line worktree status"
  (let* ((root (file-name-as-directory
                (make-temp-file "niwa-worktree-desc-" t)))
         (workspace (niwa-worktree-test--workspace root))
         (session (niwa-session-create "main" workspace root))
         (status (plist-put
                  (plist-put (niwa-worktree-test--status root)
                             :session session)
                  :workspace workspace))
         called-buffer
         called-context)
    (let ((data-buffer (generate-new-buffer " *mwt-desc-data*")))
      (unwind-protect
          (progn
            (niwa-worktree-test--install-context status data-buffer)
            (with-current-buffer data-buffer
              (cl-letf (((symbol-function 'niwa-worktree-collect-status)
                         (lambda (&optional context)
                           (setq called-buffer (current-buffer)
                                 called-context context)
                           status)))
                (let ((description (substring-no-properties
                                    (niwa-worktree-status--description))))
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
        (niwa-worktree-test--cleanup-surfaces data-buffer)
        (delete-directory root t))))

  :doc "raises a dirty tree and an untracked .worktrees directory"
  (let* ((root (file-name-as-directory
                (make-temp-file "niwa-worktree-desc-" t)))
         (workspace (niwa-worktree-test--workspace root))
         (session (niwa-session-create "main" workspace root))
         (status (thread-first
                   (niwa-worktree-test--status root)
                   (plist-put :session session)
                   (plist-put :workspace workspace)
                   (plist-put :dirty-p t)
                   (plist-put :ignore-state 'not-ignored))))
    (let ((data-buffer (generate-new-buffer " *mwt-desc-data*")))
      (unwind-protect
          (progn
            (niwa-worktree-test--install-context status data-buffer)
            (with-current-buffer data-buffer
              (cl-letf (((symbol-function 'niwa-worktree-collect-status)
                         (lambda (&optional _context) status)))
                (let* ((description (substring-no-properties
                                     (niwa-worktree-status--description)))
                       (alert (nth 1 (split-string description "\n"))))
                  (should (string-match-p "dirty" description))
                  (should (string-match-p "! .worktrees not ignored"
                                          alert))))))
        (niwa-worktree-test--cleanup-surfaces data-buffer)
        (delete-directory root t)))))

(niwa-deftest niwa-worktree-status--details-report ()
  ,test
  (test)

  :doc "renders the complete repository status as aligned rows"
  (let* ((root (file-name-as-directory
                (make-temp-file "niwa-worktree-details-" t)))
         (workspace (niwa-worktree-test--workspace root))
         (session (niwa-session-create "main" workspace root))
         (status (plist-put
                  (plist-put (niwa-worktree-test--status root)
                             :session session)
                  :workspace workspace)))
    (let ((data-buffer (generate-new-buffer " *mwt-details-data*")))
      (unwind-protect
          (progn
            (niwa-worktree-test--install-context status data-buffer)
            (with-current-buffer data-buffer
              (cl-letf (((symbol-function 'niwa-worktree-collect-status)
                         (lambda (&optional _context) status)))
			       (let ((text (niwa-report-test-text (niwa-worktree-status--details-report))))
				 (should (string-match-p "Worktree status" text))
                  (should (string-match-p "Repo " text))
				 (should (string-match-p "Session +main" text))
                  (should (string-match-p "Directory " text))
				 (should (string-match-p "Isolation +normal checkout"
                                          text))
				 (should (string-match-p "Branch +main" text))
				 (should (string-match-p ".worktrees +ignored" text))
				 (should (string-match-p "Dirty +no" text))
				 (should (string-match-p "Worktrees +1" text))))))
        (niwa-worktree-test--cleanup-surfaces data-buffer)
        (delete-directory root t)))))

(niwa-deftest niwa-worktree-status-create ()
  ,test
  (test)

  :doc "delegates create in the data buffer and reopens status"
  (let* ((root (make-temp-file "niwa-worktree-status-create-" t))
         (status (niwa-worktree-test--status root))
         (data-buffer (generate-new-buffer " *mwt-status-create*"))
         called-buffer
         called-args
         reopened
         message-text)
    (unwind-protect
        (progn
          (niwa-worktree-test--install-context status data-buffer)
          (with-current-buffer data-buffer
            (let ((transient--original-buffer data-buffer))
              (cl-letf (((symbol-function 'niwa-cmd--worktree)
                         (lambda (args)
                           (setq called-buffer (current-buffer)
                                 called-args args)
                           "created"))
                        ((symbol-function 'niwa-worktree-status-open)
                         (lambda () (setq reopened (current-buffer))))
                        ((symbol-function 'message)
                         (lambda (fmt &rest args)
                           (setq message-text (apply #'format fmt args)))))
                (should (equal (niwa-worktree-status-create)
                               "created"))))))
      (niwa-worktree-test--cleanup-surfaces data-buffer)
      (delete-directory root t))
    (should (eq called-buffer data-buffer))
    (should (equal called-args "create"))
    (should (eq reopened data-buffer))
    (should (equal message-text "created"))))

(niwa-deftest niwa-worktree-status-list ()
  ,test
  (test)

  :doc "opens the list from the transient data buffer"
  (let* ((root (make-temp-file "niwa-worktree-status-list-" t))
         (status (niwa-worktree-test--status root))
         (data-buffer (generate-new-buffer " *mwt-status-list*"))
         called-buffer
         called-context)
    (unwind-protect
        (progn
          (niwa-worktree-test--install-context status data-buffer)
          (with-current-buffer data-buffer
            (let ((transient--original-buffer data-buffer))
              (cl-letf (((symbol-function 'niwa-worktree-list-open)
                         (lambda (&optional context)
                           (setq called-buffer (current-buffer)
                                 called-context context))))
                (niwa-worktree-status-list)))))
      (niwa-worktree-test--cleanup-surfaces data-buffer)
      (delete-directory root t))
    (should (eq called-buffer data-buffer))
    (should (equal called-context
                   (niwa-worktree-test--context status data-buffer)))))

(niwa-deftest niwa-worktree-status-refresh ()
  ,test
  (test)

  :doc "reopens the status transient from the data buffer"
  (let* ((root (make-temp-file "niwa-worktree-status-refresh-" t))
         (status (niwa-worktree-test--status root))
         (data-buffer (generate-new-buffer " *mwt-status-refresh*"))
         called-buffer)
    (unwind-protect
        (progn
          (niwa-worktree-test--install-context status data-buffer)
          (with-current-buffer data-buffer
            (let ((transient--original-buffer data-buffer))
              (cl-letf (((symbol-function 'niwa-worktree-status-open)
                         (lambda () (setq called-buffer (current-buffer)))))
                (niwa-worktree-status-refresh)))))
      (niwa-worktree-test--cleanup-surfaces data-buffer)
      (delete-directory root t))
    (should (eq called-buffer data-buffer))))

(niwa-deftest niwa-worktree-status-details ()
  ,test
  (test)

  :doc "opens the worktree info panel"
  (let* ((root (file-name-as-directory
                (make-temp-file "niwa-worktree-panel-" t)))
         (workspace (niwa-worktree-test--workspace root))
         (session (niwa-session-create "main" workspace root))
         (status (plist-put
                  (plist-put (niwa-worktree-test--status root)
                             :session session)
                  :workspace workspace))
         (data-buffer (generate-new-buffer " *mwt-panel-data*")))
    (unwind-protect
        (progn
          (niwa-worktree-test--install-context status data-buffer)
          (with-current-buffer data-buffer
            (cl-letf (((symbol-function 'niwa-worktree-collect-status)
                       (lambda (&optional _context) status)))
              (niwa-worktree-status-details)))
          (with-current-buffer niwa-worktree-details-buffer-name
			   (should (string-match-p "Session +main" (buffer-string)))))
      (when (get-buffer niwa-worktree-details-buffer-name)
        (kill-buffer niwa-worktree-details-buffer-name))
      (niwa-worktree-test--cleanup-surfaces data-buffer)
      (delete-directory root t))))

(niwa-deftest niwa-worktree-status-help ()
  ,test
  (test)

  :doc "opens worktree cockpit help"
  (unwind-protect
      (progn
        (niwa-worktree-status-help)
        (with-current-buffer niwa-worktree-help-buffer-name
          (should (string-match-p "c  Create a linked worktree session"
                                  (buffer-string)))
          (should (string-match-p "i  Show full repository status"
                                  (buffer-string)))
          (should (string-match-p "q  Close, or return to the invoking menu"
                                  (buffer-string)))
          (should (string-match-p "RET  Show selected worktree details"
                                  (buffer-string)))
          (should (string-match-p "g    Refresh table"
                                  (buffer-string)))))
    (niwa-worktree-test--cleanup-surfaces)))

(niwa-deftest niwa-worktree-status-back ()
  ,test
  (test)

  :doc "returns only to an explicitly recorded parent menu"
  (let (called)
    (cl-letf (((symbol-function 'transient-scope)
               (lambda (&rest _) nil)))
      (niwa-worktree-status-back))
    (should-not called)
    (cl-letf (((symbol-function 'transient-scope)
               (lambda (&rest _)
                 (list :parent-menu (lambda () (setq called t))))))
      (niwa-worktree-status-back))
    (should called)))

(niwa-deftest niwa-worktree-status-open ()
  ,test
  (test)

  :doc "opens the worktree status transient"
  (let (called-prefix)
    (cl-letf (((symbol-function 'transient-setup)
               (lambda (prefix &rest _)
                 (setq called-prefix prefix))))
      (niwa-worktree-status-open))
    (should (eq called-prefix 'niwa-worktree-status))))

(niwa-deftest niwa-worktree-list--status ()
  ,test
  (test)

  :doc "collects status in the owning data buffer"
  (let* ((root (make-temp-file "niwa-worktree-status-owner-" t))
         (status (niwa-worktree-test--status root))
         (data-buffer (generate-new-buffer " *mwt-status-owner*"))
         (surface (get-buffer-create niwa-worktree-list-buffer-name))
         called-buffer
         called-context)
    (unwind-protect
        (with-current-buffer surface
          (niwa-worktree-list-mode)
          (setq-local niwa-cockpit--context
                      (niwa-worktree-test--context
                       status data-buffer data-buffer data-buffer))
          (cl-letf (((symbol-function 'niwa-worktree-collect-status)
                     (lambda (&optional context)
                       (setq called-buffer (current-buffer))
                       (setq called-context context)
                       :status)))
            (should (eq (niwa-worktree-list--status) :status))))
      (niwa-worktree-test--cleanup-surfaces data-buffer)
      (when (buffer-live-p surface)
        (kill-buffer surface))
      (delete-directory root t))
    (should (eq called-buffer data-buffer))
    (should (equal called-context
                   (niwa-worktree-test--context
                    status data-buffer data-buffer data-buffer)))))

(niwa-deftest niwa-worktree-list--normalize-path ()
  ,test
  (test)

  :doc "returns an absolute directory path"
  (let ((path (niwa-worktree-list--normalize-path ".")))
    (should (file-name-absolute-p path))
    (should (string-suffix-p "/" path))))

(niwa-deftest niwa-worktree-list--item-id ()
  ,test
  (test)

  :doc "uses the normalized worktree path as the row id"
  (should (equal (niwa-worktree-list--item-id
                  '(:path "/repo/.worktrees/topic/"))
                 "/repo/.worktrees/topic/")))

(niwa-deftest niwa-worktree-list--branch-label ()
  ,test
  (test)

  :doc "formats branch, detached, bare, head, and unknown branch cells"
  (should (equal (niwa-worktree-list--branch-label
                  '(:branch "main" :head "abc"))
                 "main"))
  (should (equal (niwa-worktree-list--branch-label
                  '(:detached t :head "abc"))
                 "detached abc"))
  (should (equal (niwa-worktree-list--branch-label
                  '(:bare t :head "abc"))
                 "bare"))
  (should (equal (niwa-worktree-list--branch-label
                  '(:head "abc"))
                 "abc"))
  (should (equal (niwa-worktree-list--branch-label nil)
                 "unknown")))

(niwa-deftest niwa-worktree-list--state-label ()
  ,test
  (test)

  :doc "normalizes porcelain state flags"
  (should (equal (niwa-worktree-list--state-label
                  '(:locked "reason" :branch "main"))
                 "locked"))
  (should (equal (niwa-worktree-list--state-label
                  '(:prunable "stale" :branch "main"))
                 "prunable"))
  (should (equal (niwa-worktree-list--state-label '(:bare t))
                 "bare"))
  (should (equal (niwa-worktree-list--state-label '(:detached t))
                 "detached"))
  (should (equal (niwa-worktree-list--state-label '(:branch "main"))
                 "branch"))
  (should (equal (niwa-worktree-list--state-label nil)
                 "unknown")))

(niwa-deftest niwa-worktree-list--current-label ()
  ,test
  (test)

  :doc "uses a compact yes/blank current column"
  (should (equal (niwa-worktree-list--current-label t) "yes"))
  (should (equal (niwa-worktree-list--current-label nil) "")))

(niwa-deftest niwa-worktree-list--sessions ()
  ,test
  (test)

  :doc "shows live session names for the selected worktree only"
  (cl-letf (((symbol-function 'niwa--workspace-sessions)
             (lambda (workspace)
               (should (eq workspace :workspace))
               :sessions))
            ((symbol-function 'niwa--sessions-in-working-directory)
             (lambda (sessions path)
               (should (eq sessions :sessions))
               (should (equal path "/repo/"))
               '(("main" . :session)
                 ("alt" . :session)))))
    (should (equal (niwa-worktree-list--sessions
                    :workspace "/repo/")
                   '("main" "alt"))))
  (should-not (niwa-worktree-list--sessions nil "/repo/")))

(niwa-deftest niwa-worktree-list--item ()
  ,test
  (test)

  :doc "builds a row item from status and porcelain data"
  (let* ((root (file-name-as-directory
                (make-temp-file "niwa-worktree-item-" t)))
         (entry (list :path root :branch "main" :head "abc123"))
         (status (plist-put (niwa-worktree-test--status root
                                                           (list entry))
                            :workspace :workspace)))
    (unwind-protect
        (cl-letf (((symbol-function 'niwa-worktree-list--sessions)
                   (lambda (workspace path)
                     (should (eq workspace :workspace))
                     (should (equal path root))
                     '("main"))))
          (let ((item (niwa-worktree-list--item status entry)))
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
         (workspace (niwa-worktree-test--workspace real-root))
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
              ((symbol-function 'niwa-worktree-list--sessions)
               (lambda (workspace path)
                 (should (eq workspace expected-workspace))
                 (should (equal path real-path))
                 nil)))
      (let ((item (niwa-worktree-list--item status entry)))
        (should (equal (plist-get item :path) real-path))
        (should (equal (niwa-worktree-list--item-id item) real-path))
        (should-not (plist-get item :current))))))

(niwa-deftest niwa-worktree-list--items ()
  ,test
  (test)

  :doc "maps all porcelain worktrees into row items"
  (let* ((root (file-name-as-directory
                (make-temp-file "niwa-worktree-items-" t)))
         (other (file-name-as-directory
                 (file-name-concat root ".worktrees" "other")))
         (status (niwa-worktree-test--status
                  root
                  (list (list :path root :branch "main" :head "abc")
                        (list :path other :detached t :head "def")))))
    (unwind-protect
        (cl-letf (((symbol-function 'niwa-worktree-list--sessions)
                   (lambda (&rest _) nil)))
          (let ((items (niwa-worktree-list--items status)))
            (should (= 2 (length items)))
            (should (plist-get (car items) :current))
            (should-not (plist-get (cadr items) :current))))
      (delete-directory root t))))

(niwa-deftest niwa-worktree-list--entry ()
  ,test
  (test)

  :doc "builds table cells from worktree item fields"
  (let* ((item '(:path "/repo/"
                 :branch "main"
                 :head "abc123"
                 :current t
                 :state "branch"
                 :sessions ("main" "alt")))
         (entry (niwa-worktree-list--entry item))
         (cells (niwa-test-tabulated-row-cells entry)))
    (should (equal (car entry) "/repo/"))
    (should (equal cells '("/repo/" "main" "abc123" "yes" "branch"
                           "main, alt")))))

(niwa-deftest niwa-worktree-list-refresh ()
  ,test
  (test)

  :doc "refreshes visible worktree row content"
  (let* ((root (file-name-as-directory
                (make-temp-file "niwa-worktree-refresh-" t)))
         (other (file-name-as-directory
                 (file-name-concat root ".worktrees" "other")))
         (status (niwa-worktree-test--status
                  root
                  (list (list :path root :branch "main" :head "abc")
                        (list :path other :branch "topic" :head "def"))))
         (view-buffer (generate-new-buffer " *mwt-refresh-view*"))
         (data-buffer (generate-new-buffer " *mwt-refresh-data*")))
    (unwind-protect
        (cl-letf (((symbol-function 'niwa-worktree-collect-status)
                   (lambda (&optional _context) status))
                  ((symbol-function 'niwa-worktree-list--sessions)
                   (lambda (&rest _) nil)))
          (let ((buffer (niwa-worktree-list-open
                         (niwa-worktree-test--context
                          status data-buffer view-buffer data-buffer))))
            (with-current-buffer buffer
              (niwa-cockpit-goto-id other)
              (setq status
                    (niwa-worktree-test--status
                     root
                     (list (list :path root :branch "main" :head "abc")
                           (list :path other :branch "updated"
                                 :head "fff"))))
              (niwa-worktree-list-refresh)
              (let ((rows (niwa-test-tabulated-entries-cells)))
                (should (equal (cdr (assoc other rows))
                               (list other "updated" "fff" "" "branch"
                                     "")))))))
      (niwa-worktree-test--cleanup-surfaces view-buffer data-buffer)
      (delete-directory root t))))

(niwa-deftest niwa-worktree-list--details-report ()
  ,test
  (test)

  :doc "formats normalized selected worktree details"
		 (let ((text (niwa-report-test-text (niwa-worktree-list--details-report
               '(:path "/repo/"
                 :branch "main"
                 :head "abc123"
                 :current t
                 :state "branch"
								:sessions ("main"))))))
		   (should (string-match-p "Worktree\n/repo/" text))
		   (should (string-match-p "Path +/repo/" text))
		   (should (string-match-p "Branch +main" text))
		   (should (string-match-p "Head +abc123" text))
		   (should (string-match-p "Current +yes" text))
		   (should (string-match-p "State +branch" text))
		   (should (string-match-p "Sessions +main" text))))

(niwa-deftest niwa-worktree-list-details ()
  ,test
  (test)

  :doc "opens normalized details for the selected worktree"
  (let* ((root (file-name-as-directory
                (make-temp-file "niwa-worktree-details-" t)))
         (status (niwa-worktree-test--status root))
         (data-buffer (generate-new-buffer " *mwt-details-data*")))
    (unwind-protect
        (let ((buffer (niwa-worktree-test--open-list
                       status data-buffer)))
          (with-current-buffer buffer
            (niwa-cockpit-goto-id root)
            (niwa-worktree-list-details))
          (with-current-buffer "*niwa worktree details*"
            (should (string-match-p (regexp-quote root)
                                    (buffer-string)))
			   (should (string-match-p "Branch +main"
                                    (buffer-string)))))
      (niwa-worktree-test--cleanup-surfaces data-buffer)
      (delete-directory root t))))

(niwa-deftest niwa-worktree-list-open-selected ()
  ,test
  (test)

  :doc "opens the selected worktree with directory-scoped session semantics"
  (let* ((root (file-name-as-directory
                (make-temp-file "niwa-worktree-open-" t)))
         (workspace (niwa-worktree-test--workspace root))
         (status (plist-put (niwa-worktree-test--status root)
                            :workspace workspace))
         (data-buffer (generate-new-buffer " *mwt-open-data*"))
         called-workspace
         called-path
         called-prompt
         called-directory-scoped)
    (unwind-protect
        (let ((buffer (niwa-worktree-test--open-list
                       status data-buffer)))
          (with-current-buffer buffer
            (niwa-cockpit-goto-id root)
            (cl-letf (((symbol-function 'niwa--start-chat)
                       (lambda (workspace path prompt directory-scoped)
                         (setq called-workspace workspace
                               called-path path
                               called-prompt prompt
                               called-directory-scoped directory-scoped)
                         :chat)))
              (should (eq (niwa-worktree-list-open-selected)
                          :chat)))))
      (niwa-worktree-test--cleanup-surfaces data-buffer)
      (delete-directory root t))
    (should (eq called-workspace workspace))
    (should (equal called-path root))
    (should-not called-prompt)
    (should called-directory-scoped))

  :doc "signals when no workspace can own the selected worktree"
  (let* ((root (file-name-as-directory
                (make-temp-file "niwa-worktree-open-noworkspace-" t)))
         (status (niwa-worktree-test--status root))
         (data-buffer (generate-new-buffer " *mwt-open-noworkspace-data*")))
    (unwind-protect
        (let ((buffer (niwa-worktree-test--open-list
                       status data-buffer)))
          (with-current-buffer buffer
            (niwa-cockpit-goto-id root)
            (should-error (niwa-worktree-list-open-selected)
                          :type 'user-error)))
      (niwa-worktree-test--cleanup-surfaces data-buffer)
      (delete-directory root t))))

(niwa-deftest niwa-worktree-list-create
  (:quiet t :after-each (niwa-worktree-test--cleanup-surfaces))
  ,test
  (test)

  :doc "delegates create to the existing worktree command in the data buffer"
  (let* (called-buffer
         called-args)
    (with-temp-buffer
      (let ((data-buffer (current-buffer))
            (surface (get-buffer-create niwa-worktree-list-buffer-name)))
        (with-current-buffer surface
          (niwa-worktree-list-mode)
          (setq-local niwa-cockpit--context
                      (niwa-worktree-test--context
                       (niwa-worktree-test--status default-directory)
                       data-buffer data-buffer data-buffer))
          (cl-letf (((symbol-function 'niwa-cmd--worktree)
                     (lambda (args)
                       (setq called-buffer (current-buffer)
                             called-args args)
                       "created"))
                    ((symbol-function 'niwa-worktree-list-refresh)
                     #'ignore))
            (should (string= "created"
                             (niwa-worktree-list-create)))))
        (should (eq called-buffer data-buffer))
        (should (equal "create" called-args))))))

(niwa-deftest niwa-worktree-list--delete
  (:after-each (niwa-worktree-test--cleanup-surfaces))
  ,test
  (test)

  :doc "removes a clean selected .worktrees checkout and leaves its branch"
  (let* ((root (file-name-as-directory
                (make-temp-file "niwa-worktree-delete-" t)))
         (workspace (niwa-worktree-test--workspace root))
         (path (file-name-as-directory
                (file-name-concat root ".worktrees" "foo")))
         (data-buffer (generate-new-buffer " *mwt-delete-data*"))
         prompt message-text)
    (unwind-protect
        (progn
          (niwa-worktree-test--init-repo root)
          (make-directory (file-name-concat root ".worktrees") t)
          (niwa-worktree-test--git-ok
           root "worktree" "add" "-b" "worktree/foo" path)
          (cl-letf (((symbol-function 'yes-or-no-p)
                     (lambda (question)
                       (setq prompt question)
                       t))
                    ((symbol-function 'message)
                     (lambda (format-string &rest args)
                       (setq message-text
                             (apply #'format format-string args)))))
            (let ((buffer (niwa-worktree-test--open-real-list
                           root workspace data-buffer)))
              (with-current-buffer buffer
                (niwa-cockpit-goto-id path)
                (niwa-worktree-list--delete nil))))
          (should-not (file-exists-p path))
          (niwa-worktree-test--git-ok
           root "show-ref" "--verify" "refs/heads/worktree/foo")
          (should (string-match-p (regexp-quote path) prompt))
          (should (string-match-p "branch worktree/foo was left intact"
                                  message-text)))
      (niwa-worktree-test--cleanup-surfaces data-buffer)
      (delete-directory root t)))

  :doc "refuses dirty worktrees before removing"
  (let* ((root (file-name-as-directory
                (make-temp-file "niwa-worktree-dirty-delete-" t)))
         (workspace (niwa-worktree-test--workspace root))
         (path (file-name-as-directory
                (file-name-concat root ".worktrees" "dirty")))
         (data-buffer (generate-new-buffer " *mwt-dirty-delete-data*")))
    (unwind-protect
        (progn
          (niwa-worktree-test--init-repo root)
          (make-directory (file-name-concat root ".worktrees") t)
          (niwa-worktree-test--git-ok
           root "worktree" "add" "-b" "worktree/dirty" path)
          (with-temp-file (file-name-concat path "dirty.txt")
            (insert "dirty\n"))
          (cl-letf (((symbol-function 'yes-or-no-p)
                     (lambda (&rest _)
                       (error "Should not prompt"))))
            (let ((buffer (niwa-worktree-test--open-real-list
                           root workspace data-buffer)))
              (with-current-buffer buffer
                (niwa-cockpit-goto-id path)
                (should-error (niwa-worktree-list--delete nil)
                              :type 'user-error)))
            (should (file-exists-p path))))
      (niwa-worktree-test--cleanup-surfaces data-buffer)
      (delete-directory root t)))

  :doc "force-removes a selected dirty .worktrees checkout"
  (let* ((root (file-name-as-directory
                (make-temp-file "niwa-worktree-force-delete-" t)))
         (workspace (niwa-worktree-test--workspace root))
         (path (file-name-as-directory
                (file-name-concat root ".worktrees" "force")))
         (data-buffer (generate-new-buffer " *mwt-force-delete-data*"))
         prompt)
    (unwind-protect
        (progn
          (niwa-worktree-test--init-repo root)
          (make-directory (file-name-concat root ".worktrees") t)
          (niwa-worktree-test--git-ok
           root "worktree" "add" "-b" "worktree/force" path)
          (with-temp-file (file-name-concat path "dirty.txt")
            (insert "dirty\n"))
          (cl-letf (((symbol-function 'yes-or-no-p)
                     (lambda (question)
                       (setq prompt question)
                       t))
                    ((symbol-function 'message)
                     #'ignore))
            (let ((buffer (niwa-worktree-test--open-real-list
                           root workspace data-buffer)))
              (with-current-buffer buffer
                (niwa-cockpit-goto-id path)
                (niwa-worktree-list--delete t))))
          (should-not (file-exists-p path))
          (should (string-match-p "discard uncommitted changes" prompt))
          (niwa-worktree-test--git-ok
           root "show-ref" "--verify" "refs/heads/worktree/force"))
      (niwa-worktree-test--cleanup-surfaces data-buffer)
      (delete-directory root t)))

  :doc "refuses unsafe real worktree rows before removing"
  (let* ((root (file-name-as-directory
                (make-temp-file "niwa-worktree-refuse-delete-" t)))
         (workspace (niwa-worktree-test--workspace root))
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
          (niwa-worktree-test--init-repo root)
          (make-directory worktrees-root t)
          (niwa-worktree-test--git-ok
           root "worktree" "add" "-b" "worktree/locked" locked)
          (niwa-worktree-test--git-ok
           root "worktree" "lock" locked)
          (niwa-worktree-test--git-ok
           root "worktree" "add" "-b" "worktree/live" live)
          (niwa-worktree-test--git-ok
           root "worktree" "add" "-b" "worktree/outside" outside)
          (with-current-buffer live-buffer
            (setq-local niwa--session
                        (niwa-session-create "live" workspace live)))
          (let ((buffer (niwa-worktree-test--open-real-list
                         root workspace data-buffer)))
            (dolist (path (list root locked live outside))
              (with-current-buffer buffer
                (niwa-cockpit-surface-refresh path)
                (should-error (niwa-worktree-list--delete nil)
                              :type 'user-error)))))
      (niwa-worktree-test--cleanup-surfaces data-buffer live-buffer)
      (when (file-directory-p outside)
        (delete-directory outside t))
      (delete-directory root t)))

  :doc "refuses bare selected rows before removing"
  (let* ((root (file-name-as-directory
                (make-temp-file "niwa-worktree-bare-delete-" t)))
         (workspace (niwa-worktree-test--workspace root))
         (path (file-name-as-directory
                (file-name-concat root ".worktrees" "bare")))
         (status (plist-put
                  (niwa-worktree-test--status
                   root
                   (list (list :path path :bare t :head "def")))
                  :workspace workspace))
         (data-buffer (generate-new-buffer " *mwt-bare-delete-data*")))
    (unwind-protect
        (progn
          (make-directory path t)
          (with-current-buffer data-buffer
            (setq-local default-directory root))
          (cl-letf (((symbol-function 'niwa-worktree-collect-status)
                     (lambda (&optional _context) status))
                    ((symbol-function 'niwa-worktree--git-result)
                     (lambda (&rest _)
                       (error "Should not remove"))))
            (let ((buffer (niwa-worktree-list-open
                           (niwa-worktree-test--context
                            status data-buffer data-buffer data-buffer))))
              (with-current-buffer buffer
                (niwa-cockpit-goto-id path)
                (should-error (niwa-worktree-list--delete nil)
                              :type 'user-error)))))
      (niwa-worktree-test--cleanup-surfaces data-buffer)
      (delete-directory root t))))

(niwa-deftest niwa-worktree-list-delete
  (:after-each (niwa-worktree-test--cleanup-surfaces))
  ,test
  (test)

  :doc "delegates to normal deletion"
  (let (force)
    (cl-letf (((symbol-function 'niwa-worktree-list--delete)
               (lambda (value)
                 (setq force value)
                 :deleted)))
      (should (eq (niwa-worktree-list-delete) :deleted))
      (should-not force))))

(niwa-deftest niwa-worktree-list-force-delete
  (:after-each (niwa-worktree-test--cleanup-surfaces))
  ,test
  (test)

  :doc "delegates to force deletion"
  (let (force)
    (cl-letf (((symbol-function 'niwa-worktree-list--delete)
               (lambda (value)
                 (setq force value)
                 :deleted)))
      (should (eq (niwa-worktree-list-force-delete) :deleted))
      (should force))))

(niwa-deftest niwa-worktree-list-help ()
  ,test
  (test)

  :doc "opens worktree list help"
  (unwind-protect
      (progn
        (niwa-worktree-list-help)
        (with-current-buffer niwa-worktree-help-buffer-name
          (should (string-match-p "o    Open selected worktree session"
                                  (buffer-string)))
          (should (string-match-p "c    Create a linked worktree session"
                                  (buffer-string)))
          (should (string-match-p "d    Delete selected worktree"
                                  (buffer-string)))
          (should (string-match-p "D    Force-delete selected worktree"
                                  (buffer-string)))))
    (niwa-worktree-test--cleanup-surfaces)))

(niwa-deftest niwa-worktree-list-quit ()
  ,test
  (test)

  :doc "delegates quit to the shared cockpit shell"
  (let (label)
    (cl-letf (((symbol-function 'niwa-cockpit-quit)
               (lambda (quit-label)
                 (setq label quit-label))))
      (niwa-worktree-list-quit))
    (should (equal label "worktree list"))))

(niwa-deftest niwa-worktree-list-open ()
  ,test
  (test)

  :doc "renders visible worktree rows"
  (let* ((root (file-name-as-directory
                (make-temp-file "niwa-worktree-list-" t)))
         (status (niwa-worktree-test--status root))
         (view-buffer (generate-new-buffer " *mwt-list-view*"))
         (data-buffer (generate-new-buffer " *mwt-list-data*")))
    (unwind-protect
        (let ((buffer (niwa-worktree-test--open-list
                       status data-buffer view-buffer)))
          (with-current-buffer buffer
            (let ((rows (niwa-test-tabulated-entries-cells)))
              (should (= 1 (length rows)))
              (should (equal (cdr (assoc root rows))
                             (list root "main" "abc123" "yes" "branch"
                                   ""))))))
      (niwa-worktree-test--cleanup-surfaces view-buffer data-buffer)
      (delete-directory root t))))

(niwa-deftest niwa-cmd--worktree/status-surface ()
  ,test
  (test)

  :doc "keeps the superseded string renderer deleted"
  (should-not (fboundp 'niwa-worktree--format-worktree-entry))
  (should-not (fboundp 'niwa-worktree--format-status))

  :doc "routes blank and status commands to the worktree surface"
  (with-temp-buffer
    (let (opened-buffer)
      (cl-letf (((symbol-function 'niwa-worktree-status-open)
                 (lambda () (setq opened-buffer (current-buffer)))))
        (dolist (args '("" "status"))
          (setq opened-buffer nil)
          (should (null (niwa-cmd--worktree args)))
          (should (eq opened-buffer (current-buffer)))))))

  :doc "routes list commands to the tabulated worktree list"
  (with-temp-buffer
    (let (opened-buffer)
      (cl-letf (((symbol-function 'niwa-worktree-list-open)
                 (lambda () (setq opened-buffer (current-buffer)))))
        (should (null (niwa-cmd--worktree "list")))
        (should (eq opened-buffer (current-buffer)))))))


;;
;;; Create

(niwa-deftest niwa-worktree-bootstrap-probe
  (:doc "checks manifest minimums before reporting dependency readiness")
  (with-temp-buffer
    (should
     (eq 0 (call-process
            "python3" nil t nil "-c"
            "import runpy,sys;sys.stdout.write(runpy.run_path(sys.argv[1])['PROBE'])"
            (file-name-concat niwa-test--worktree-root
                              "scripts/bootstrap-worktree.py"))))
    (let* ((probe (read (buffer-string)))
           (eask-depends-on '(("acp" "99999999")))
           (eask-depends-on-dev nil)
           (standard-output (current-buffer))
           (err (should-error (eval probe t))))
      (should (string-match-p "Missing or outdated test dependency: acp >=99999999"
                              (error-message-string err)))
      (erase-buffer)
      (setq eask-depends-on '(("acp" "0.15.2")))
      (eval probe t)
      (should (string-match-p "gptel-request: version=.*sha256=" (buffer-string)))
      (should (string-match-p "Worktree bootstrap: ready" (buffer-string))))))

(niwa-deftest niwa-worktree-bootstrap-branch-dependencies
  (:doc "rejects new runtime and development dependencies absent from shared Eask")
  (dolist (development '(nil t))
    (let* ((root (file-name-as-directory
                  (make-temp-file "niwa-branch-dependency-" t)))
           (destination (file-name-concat root "linked"))
           (script (file-name-concat niwa-test--worktree-root
                                     "scripts/bootstrap-worktree.py")))
      (unwind-protect
          (progn
            (niwa-worktree-test--init-repo root)
            (with-temp-file (file-name-concat root "Eask")
              (insert ";; -*- mode: eask; lexical-binding: t -*-\n"
                      "(package \"bootstrap-fixture\" \"1.0.0\" \"Fixture\")\n"
                      "(package-file \"fixture.el\")\n"
                      "(depends-on \"gptel\")\n"))
            (with-temp-file (file-name-concat root "fixture.el")
              (insert ";;; fixture.el --- Fixture -*- lexical-binding: t -*-\n"
                      "(provide 'fixture)\n"))
            (with-temp-file (file-name-concat root ".gitignore")
              (insert ".eask\nlinked/\n"))
            (niwa-worktree-test--git-ok root "add" "Eask" "fixture.el" ".gitignore")
            (niwa-worktree-test--git-ok root "commit" "-m" "Eask fixture")
            (make-symbolic-link
             (file-truename (file-name-concat niwa-test--worktree-root ".eask"))
             (file-name-concat root ".eask"))
            (niwa-worktree-test--git-ok root "worktree" "add" "--detach" destination)
            (with-temp-buffer
              (let ((default-directory (file-name-as-directory destination)))
                (let ((status (process-file "python3" nil (list t t) nil script)))
                  (ert-info ((buffer-string))
                    (should (eq 0 status))))
                (should (string-match-p "Worktree bootstrap: ready" (buffer-string)))
                (erase-buffer)
                (with-temp-buffer
                  (insert "(depends-on \"niwa-bootstrap-absent-dependency\" \"1.0\")")
                  (when development
                    (goto-char (point-min))
                    (insert "(development ")
                    (goto-char (point-max))
                    (insert ")"))
                  (write-region (concat "\n" (buffer-string) "\n") nil
                                (file-name-concat destination "Eask") t 'silent))
                (should-not (eq 0 (process-file "python3" nil (list t t) nil script)))
                (should (string-match-p
                         "Missing or outdated test dependency: niwa-bootstrap-absent-dependency >=1.0"
                         (buffer-string)))
                (should (string-match-p "install-deps --dev" (buffer-string)))
                (should (string-match-p "In the failing worktree, run" (buffer-string)))
                (should (string-match-p "get approval to change shared dependencies"
                                        (buffer-string)))
                (should-not (string-match-p "Worktree bootstrap: ready" (buffer-string))))))
        (delete-directory root t)))))

(niwa-deftest niwa-worktree-setup-mode
  (:doc "limits repository setup locals to dedicated setup lookup buffers")
  (let* ((root (make-temp-file "niwa-setup-locals-scope-" t))
         (file (file-name-concat root "ordinary.el"))
         (command '("python3" "scripts/bootstrap-worktree.py"))
         (enable-local-variables :safe)
         (safe-local-variable-values
          `((niwa-worktree-setup-enabled . t)
            (niwa-worktree-setup-command . ,command)))
         ordinary)
    (unwind-protect
        (progn
          (copy-file (file-name-concat niwa-test--worktree-root ".dir-locals.el")
                     (file-name-concat root ".dir-locals.el"))
          (with-temp-file file
            (insert ";;; ordinary.el --- Fixture -*- lexical-binding: t -*-\n"))
          (setq ordinary (find-file-noselect file))
          (with-current-buffer ordinary
            (should (eq major-mode 'emacs-lisp-mode))
            (dolist (variable '(niwa-worktree-setup-enabled
                                niwa-worktree-setup-command))
              (should-not (local-variable-p variable))
              (should-not (assq variable dir-local-variables-alist))))
          (with-temp-buffer
            (niwa-worktree-setup-mode)
            (should (eq major-mode 'niwa-worktree-setup-mode))
            (should-not (local-variable-p 'niwa-worktree-setup-enabled))
            (setq default-directory (file-name-as-directory root))
            (hack-dir-local-variables-non-file-buffer)
            (should (local-variable-p 'niwa-worktree-setup-enabled))
            (should niwa-worktree-setup-enabled)
            (should (equal command niwa-worktree-setup-command))))
      (when (buffer-live-p ordinary)
        (kill-buffer ordinary))
      (delete-directory root t))))

(niwa-deftest niwa-worktree--setup
  (:quiet t :after-each (when (get-buffer "*niwa worktree setup*")
                         (kill-buffer "*niwa worktree setup*")))
  ,test
  (test)

  :doc "does not trust a project's setup settings or script presence"
  (let* ((root (make-temp-file "niwa-untrusted-setup-" t))
         (enable-local-variables :safe)
         (safe-local-variable-values nil))
    (unwind-protect
        (progn
          (with-temp-file (file-name-concat root ".dir-locals.el")
            (prin1 '((niwa-worktree-setup-mode
                      . ((niwa-worktree-setup-enabled . t)
                         (niwa-worktree-setup-command
                          . ("sh" "-c" "touch unsafe")))))
                   (current-buffer)))
          (niwa-worktree--setup root root)
          (should-not (file-exists-p (file-name-concat root "unsafe")))
          (should-not (safe-local-variable-p
                       'niwa-worktree-setup-enabled t))
          (should-not (safe-local-variable-p
                       'niwa-worktree-setup-command '("sh" "-c" "touch unsafe"))))
      (delete-directory root t)))
  :doc "runs approved source directory locals in the destination and retains output"
  (let* ((root (make-temp-file "niwa-approved-setup-" t))
         (source (file-name-concat root "source"))
         (destination (file-name-concat root "destination"))
         (command '("sh" "-c" "printf provenance; touch ready"))
         (enable-local-variables :safe)
         (safe-local-variable-values
          `((niwa-worktree-setup-enabled . t)
            (niwa-worktree-setup-command . ,command))))
    (unwind-protect
        (progn
          (make-directory source)
          (make-directory destination)
          (with-temp-file (file-name-concat source ".dir-locals.el")
            (prin1 `((niwa-worktree-setup-mode
                      . ((niwa-worktree-setup-enabled . t)
                         (niwa-worktree-setup-command . ,command))))
                   (current-buffer)))
          (niwa-worktree--setup source destination)
          (should (file-exists-p (file-name-concat destination "ready")))
          (should-not (file-exists-p (file-name-concat source "ready")))
          (with-current-buffer "*niwa worktree setup*"
            (should buffer-read-only)
            (should (string-match-p "provenance" (buffer-string)))))
      (delete-directory root t)))
  :doc "keeps exact nonzero diagnostics and a retained worktree retry path"
  (let* ((root (make-temp-file "niwa-failed-setup-" t))
         (niwa-worktree-setup-enabled t)
         (niwa-worktree-setup-command '("sh" "-c" "printf missing-gptel; exit 17")))
    (unwind-protect
        (let ((err (should-error (niwa-worktree--setup root root)
                                 :type 'user-error)))
          (should (string-match-p "setup failed (17)" (error-message-string err)))
          (should (string-match-p "rerun" (error-message-string err)))
          (should (file-directory-p root))
          (with-current-buffer "*niwa worktree setup*"
            (should (string-match-p "missing-gptel" (buffer-string)))))
      (delete-directory root t)))
  :doc "runs enabled setup on the remote worktree target"
  (let* ((root (file-name-as-directory
                (make-temp-file "niwa-remote-setup-" t)))
         (remote-root (format "/niwamock:setup:%s" root))
         (niwa-worktree-setup-enabled t)
         (niwa-worktree-setup-command '("sh" "-c" "printf remote; touch ready")))
    (unwind-protect
        (niwa-test--with-local-shell-tramp '("setup")
          (niwa-worktree--setup remote-root remote-root)
          (should (file-exists-p (file-name-concat remote-root "ready")))
          (with-current-buffer "*niwa worktree setup*"
            (should (string-match-p "remote" (buffer-string)))))
      (delete-directory root t)))
  :doc "requires a nonempty argv when enabled and diagnoses a missing program"
  (let ((root (make-temp-file "niwa-invalid-setup-" t))
        (niwa-worktree-setup-enabled t)
        (niwa-worktree-setup-command nil))
    (unwind-protect
        (progn
          (should-error (niwa-worktree--setup root root) :type 'user-error)
          (setq niwa-worktree-setup-command '("niwa-no-such-setup-program"))
          (let ((err (should-error (niwa-worktree--setup root root)
                                   :type 'user-error)))
            (should (string-match-p "setup failed (127)" (error-message-string err)))))
      (delete-directory root t))))

(niwa-deftest niwa-worktree-create-session ()
  ,test
  (test)

  :doc "setup completes before opening a session and failure retains the worktree"
  (dolist (status '(0 19))
    (let* ((root (file-name-as-directory
                  (make-temp-file "niwa-session-setup-" t)))
           (niwa-worktree-setup-enabled t)
           (niwa-worktree-setup-command
            (list "sh" "-c" (format "touch setup-ready; exit %d" status)))
           (new-buffer (generate-new-buffer " *niwa-setup-session*"))
           opened)
      (unwind-protect
          (progn
            (niwa-worktree-test--init-repo root)
            (niwa-worktree-test--with-session root
              (cl-letf (((symbol-function 'niwa-worktree--open-session)
                         (lambda (_workspace directory)
                           (should (file-exists-p
                                    (file-name-concat directory "setup-ready")))
                           (setq opened t)
                           new-buffer)))
                (if (zerop status)
                    (niwa-test--with-captured-messages nil
                      (niwa-worktree-create-session "worktree/setup" nil t))
                  (let ((err (should-error
                              (niwa-worktree-create-session "worktree/setup" nil t)
                              :type 'user-error)))
                    (should (string-match-p "setup failed (19)"
                                            (error-message-string err)))))
                (should (eq (zerop status) opened))
                (should (file-directory-p (file-name-concat root ".worktrees/setup"))))))
        (kill-buffer new-buffer)
        (when (get-buffer "*niwa worktree setup*")
          (kill-buffer "*niwa worktree setup*"))
        (delete-directory root t)
        (niwa-workspace-clear-registry))))
  :doc "fails Git preflight before creating worktree state"
  (let ((root (file-name-as-directory
               (make-temp-file "niwa-plan-worktree-no-git-" t)))
        opened)
    (unwind-protect
        (niwa-worktree-test--with-session root
          (cl-letf (((symbol-function 'niwa-worktree--git-result)
                     (lambda (_directory &rest _args)
                       (niwa-worktree-test--git-result
                        "git: command not found" 127)))
                    ((symbol-function 'niwa-worktree--open-session)
                     (lambda (&rest _args) (setq opened t))))
            (should-error
             (niwa-worktree-create-session
              "worktree/accepted-plan" "Accepted Plan implementation" t)
             :type 'user-error)
            (should-not opened)
            (should-not (file-exists-p
                         (file-name-concat root ".worktrees")))))
      (delete-directory root t)
      (niwa-workspace-clear-registry)))
  :doc "fails worktree capability preflight before creating state"
  (let ((root (file-name-as-directory
               (make-temp-file "niwa-plan-worktree-unsupported-" t)))
        opened)
    (unwind-protect
        (niwa-worktree-test--with-session root
          (cl-letf (((symbol-function 'niwa-worktree--git-result)
                     (lambda (_directory &rest args)
                       (if (equal args '("worktree" "list" "--porcelain"))
                           (niwa-worktree-test--git-result
                            "git: 'worktree' is not a git command" 129)
                         (niwa-worktree-test--base-response root args))))
                    ((symbol-function 'niwa-worktree--open-session)
                     (lambda (&rest _args) (setq opened t))))
            (let ((err
                   (should-error
                    (niwa-worktree-create-session
                     "worktree/accepted-plan"
                     "Accepted Plan implementation" t)
                    :type 'user-error)))
              (should (string-match-p "Git worktree support is required"
                                      (error-message-string err))))
            (should-not opened)
            (should-not (file-exists-p
                         (file-name-concat root ".worktrees")))))
      (delete-directory root t)
      (niwa-workspace-clear-registry)))
  :doc "rejects an escaping local worktrees symlink before Git mutation"
  (let* ((base (file-name-as-directory
                (make-temp-file "niwa-plan-worktree-symlink-" t)))
         (root (file-name-as-directory (file-name-concat base "repo")))
         (outside (file-name-as-directory
                   (file-name-concat base "outside")))
         opened)
    (unwind-protect
        (progn
          (make-directory root t)
          (make-directory outside t)
          (niwa-worktree-test--init-repo root)
          (make-symbolic-link
           outside (file-name-concat root ".worktrees"))
          (niwa-worktree-test--with-session root
            (cl-letf (((symbol-function 'niwa-worktree--open-session)
                       (lambda (&rest _args) (setq opened t))))
              (should-error
               (niwa-worktree-create-session
                "worktree/escape" nil t)
               :type 'user-error)
              (should-not opened)
              (should-not
               (file-exists-p (file-name-concat outside "escape")))
              (should-not
               (eq 0
                   (niwa-worktree--git-exit
                    root "show-ref" "--verify" "--quiet"
                    "refs/heads/worktree/escape"))))))
      (delete-directory base t)
      (niwa-workspace-clear-registry)))
  :doc "creates the worktree with target Git on the source target"
  (let* ((root (file-name-as-directory
                (make-temp-file "niwa-plan-worktree-remote-" t)))
         (remote-root (format "/niwamock:plan:%s" root))
         (new-buffer (generate-new-buffer
                      " *niwa-plan-worktree-remote*"))
         opened-directory)
    (unwind-protect
        (niwa-test--with-local-shell-tramp '("plan")
          (niwa-worktree-test--init-repo remote-root)
          (let* ((workspace (niwa-worktree-test--workspace remote-root))
                 (session (niwa-session-create
                           "remote" workspace remote-root)))
            (with-temp-buffer
              (setq-local niwa--session session)
              (setq-local default-directory remote-root)
              (cl-letf (((symbol-function 'niwa-worktree--open-session)
                         (lambda (_workspace directory)
                           (setq opened-directory directory)
                           new-buffer)))
                (let ((result
                       (niwa-worktree-create-session
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
                     (niwa-worktree-test--git-ok
                      opened-directory "branch" "--show-current")
                     :output))))))))
      (when (buffer-live-p new-buffer)
        (kill-buffer new-buffer))
      (delete-directory root t)
      (niwa-workspace-clear-registry)))
  :doc "returns a clean worktree session created with the source-session default"
  (let* ((root (file-name-as-directory
                (make-temp-file "niwa-worktree-interface-" t)))
         (git-dir (file-name-concat root ".git"))
         (new-buffer (generate-new-buffer " *niwa-worktree-interface*"))
         prompted-default)
    (unwind-protect
        (progn
          (make-directory (file-name-concat git-dir "info") t)
          (niwa-worktree-test--with-session root
            (cl-letf (((symbol-function 'read-string)
                       (lambda (_prompt &optional _initial _history default
                                &rest _)
                         (setq prompted-default default)
                         default))
                      ((symbol-function 'niwa-worktree--open-session)
                       (lambda (_workspace _dir) new-buffer))
                      ((symbol-function 'niwa-worktree--git-result)
                       (lambda (_dir &rest args)
                         (niwa-worktree-test--base-response root args))))
              (let* ((result (niwa-worktree-create-session nil nil t))
                     (expected-dir
                      (file-name-as-directory
                       (file-name-concat root ".worktrees" "main"))))
                (should (equal "worktree/main" prompted-default))
                (should (eq new-buffer (plist-get result :buffer)))
                (should (equal "worktree/main" (plist-get result :branch)))
                (should (equal expected-dir (plist-get result :directory)))
                (should-not (plist-get result :warnings))))))
      (when (buffer-live-p new-buffer) (kill-buffer new-buffer))
      (delete-directory root t))))

(niwa-deftest niwa-worktree-session-directory
  (:doc "derives the accepted branch's deterministic workspace path")
  ,test
  (test)
  (let* ((root (file-name-as-directory
                (make-temp-file "niwa-worktree-reservation-" t)))
         (workspace (niwa-worktree-test--workspace root))
         (session (niwa-session-create "main" workspace root)))
    (unwind-protect
        (progn
          (niwa-worktree-test--init-repo root)
          (with-temp-buffer
            (setq-local niwa--session session)
            (setq-local default-directory root)
            (should
             (equal
              (file-name-as-directory
               (file-name-concat root ".worktrees" "accepted"))
              (niwa-worktree-session-directory
               "plan/accepted")))))
      (delete-directory root t)
      (niwa-workspace-clear-registry))))

(niwa-deftest niwa-cmd--worktree/create ()
  ,test
  (test)

  :doc "creates a clean worktree session with prompted branch name"
  (let* ((root (file-name-as-directory
                (make-temp-file "niwa-worktree-create-" t)))
         (git-dir (file-name-concat root ".git"))
         (opened nil)
         (calls nil))
    (unwind-protect
        (progn
          (make-directory (file-name-concat git-dir "info") t)
          (niwa-worktree-test--with-session root
            (cl-letf (((symbol-function 'read-string)
                       (lambda (&rest _) "worktree/foo"))
                      ((symbol-function 'niwa-worktree--open-session)
                       (lambda (_workspace dir)
                         (setq opened dir)
                         (current-buffer)))
                      ((symbol-function 'niwa-worktree--git-result)
                       (lambda (_dir &rest args)
                         (push args calls)
                         (niwa-worktree-test--base-response
                          root args))))
              (let ((out (niwa-cmd--worktree "create --clean")))
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
               (make-temp-file "niwa-worktree-active-" t))))
    (unwind-protect
        (niwa-worktree-test--with-session root
          (setq-local niwa--current-request t)
          (should-error (niwa-cmd--worktree "create worktree/foo --clean")
                        :type 'user-error))
      (delete-directory root t)))

  :doc "rejects invalid branch names before Git add"
  (dolist (name '("bad name" "worktree/foo.lock"))
    (let ((root (file-name-as-directory
                 (make-temp-file "niwa-worktree-invalid-" t)))
          calls)
      (unwind-protect
          (niwa-worktree-test--with-session root
            (cl-letf (((symbol-function 'niwa-worktree--git-result)
                       (lambda (_dir &rest args)
                         (push args calls)
                         (if (equal args (list "check-ref-format" "--branch" name))
                             (niwa-worktree-test--git-result "" 1)
                           (niwa-worktree-test--base-response root args)))))
              (should-error (niwa-cmd--worktree
                             (format "create %S --clean" name))
                            :type 'user-error)
              (should-not
               (cl-find-if
                (lambda (args) (equal (seq-take args 2) '("worktree" "add")))
                calls))))
        (delete-directory root t)))))


;;
;;; Setup stub

(niwa-deftest niwa-worktree--format-stub ()
  ,test
  (test)

  :doc "renders remote setup context with target-native paths"
  (let* ((root (file-name-as-directory
                (make-temp-file "niwa-worktree-stub-remote-" t)))
         (source (format "/niwamock:stub:%s" root))
         (worktree (file-name-concat source ".worktrees" "topic")))
    (unwind-protect
        (niwa-test--with-local-shell-tramp '("stub")
          (let ((out (niwa-worktree--format-stub
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
            (should-not (string-match-p "/niwamock:" out))))
      (delete-directory root t))))

(niwa-deftest niwa-cmd--worktree/setup-stub ()
  ,test
  (test)

  :doc "inserts and saves visible setup context without sending"
  (let* ((root (file-name-as-directory
                (make-temp-file "niwa-worktree-stub-" t)))
         (git-dir (file-name-concat root ".git"))
         (new-buffer (generate-new-buffer " *niwa-worktree-stub*"))
         (view-buffer (generate-new-buffer " *niwa-worktree-stub-view*"))
         (saved nil)
         (sent nil))
    (unwind-protect
        (progn
          (make-directory (file-name-concat git-dir "info") t)
          (niwa-worktree-test--with-session root
            (let* ((workspace (niwa-session-workspace session))
                   (worktree-dir (file-name-as-directory
                                  (file-name-concat root ".worktrees" "foo")))
                   (new-session (niwa-session-create
                                 ".worktrees:foo" workspace worktree-dir)))
              (with-current-buffer new-buffer
                (org-mode)
                (setq-local gptel-response-separator "\n\n")
                (setq-local niwa--session new-session)
                (setq-local niwa--view-buffer view-buffer))
              (cl-letf (((symbol-function 'gptel-send)
                         (lambda (&rest _) (setq sent t)))
                        ((symbol-function 'niwa-worktree--open-session)
                         (lambda (_workspace dir)
                           (make-directory dir t)
                           (niwa-view--setup view-buffer new-buffer)
                           new-buffer))
                        ((symbol-function 'niwa-session-artifacts-save)
                         (lambda (_session _buffer) (setq saved t)))
                        ((symbol-function 'niwa-worktree--git-result)
                         (lambda (_dir &rest args)
                           (if (equal args '("status" "--short"))
                               (niwa-worktree-test--git-result " M foo.el")
                             (niwa-worktree-test--base-response
                              root args)))))
                (niwa-cmd--worktree
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
                             (point-min) (niwa-view-transcript-tail))))
                  (should (string-match-p
                           "Purpose: fix the parser" text))
                  (should-not niwa-view--in-flight-turn-start)
                  (should-not niwa-view--spinner-status)))
              (should (equal worktree-dir
                             (niwa-session-working-directory new-session)))
              (should saved)
              (should-not sent))))
      (when (buffer-live-p new-buffer)
        (kill-buffer new-buffer))
      (when (buffer-live-p view-buffer)
        (kill-buffer view-buffer))
      (delete-directory root t)))

  :doc "--clean suppresses setup context and save"
  (let* ((root (file-name-as-directory
                (make-temp-file "niwa-worktree-clean-" t)))
         (git-dir (file-name-concat root ".git"))
         (new-buffer (generate-new-buffer " *niwa-worktree-clean*"))
         (saved nil))
    (unwind-protect
        (progn
          (make-directory (file-name-concat git-dir "info") t)
          (niwa-worktree-test--with-session root
            (let* ((workspace (niwa-session-workspace session))
                   (worktree-dir (file-name-as-directory
                                  (file-name-concat root ".worktrees" "foo")))
                   (new-session (niwa-session-create
                                 ".worktrees:foo" workspace worktree-dir)))
              (with-current-buffer new-buffer
                (org-mode)
                (setq-local gptel-response-separator "\n\n")
                (setq-local niwa--session new-session))
              (cl-letf (((symbol-function 'niwa-worktree--open-session)
                         (lambda (_workspace _dir) new-buffer))
                        ((symbol-function 'niwa-session-artifacts-save)
                         (lambda (_session _buffer) (setq saved t)))
                        ((symbol-function 'niwa-worktree--git-result)
                         (lambda (_dir &rest args)
                           (niwa-worktree-test--base-response root args))))
                (niwa-cmd--worktree
                 "create worktree/foo --for \"ignored\" --clean"))
              (with-current-buffer new-buffer
                (should (string-empty-p
                         (buffer-substring-no-properties
                          (point-min) (point-max)))))
              (should-not saved))))
      (when (buffer-live-p new-buffer)
        (kill-buffer new-buffer))
      (delete-directory root t))))

;;; test-niwa-worktree.el ends here
