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
        (mevedel-worktree-list-open view-buffer data-buffer data-buffer)))))


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


;;
;;; Status

(mevedel-deftest mevedel-worktree--format-status ()
  ,test
  (test)

  :doc "reports normal checkout status"
  (let* ((root (file-name-as-directory
                (make-temp-file "mevedel-worktree-status-" t)))
         (responses nil))
    (unwind-protect
        (mevedel-worktree-test--with-session root
          (cl-letf (((symbol-function 'mevedel-worktree--git-result)
                     (lambda (_dir &rest args)
                       (or (cdr (assoc args responses))
                           (mevedel-worktree-test--base-response
                            root args)))))
            (let ((out (mevedel-worktree--format-status
                        (mevedel-worktree--collect-status))))
              (should (string-match-p "Isolation: normal checkout" out))
              (should (string-match-p "Current session: main" out))
              (should (string-match-p
                       "Usage: /worktree status | /worktree create"
                       out)))))
      (delete-directory root t)))

  :doc "reports linked worktree status"
  (let* ((root (file-name-as-directory
                (make-temp-file "mevedel-worktree-linked-" t)))
         (responses
          `((("rev-parse" "--git-dir")
             . ,(mevedel-worktree-test--git-result
                 (file-name-concat root ".git" "worktrees" "foo"))))))
    (unwind-protect
        (progn
          (make-directory (file-name-concat root ".git" "worktrees" "foo") t)
          (mevedel-worktree-test--with-session root
            (cl-letf (((symbol-function 'mevedel-worktree--git-result)
                       (lambda (_dir &rest args)
                         (or (cdr (assoc args responses))
                             (mevedel-worktree-test--base-response
                              root args)))))
              (should (string-match-p
                       "Isolation: linked worktree"
                       (mevedel-worktree--format-status
                        (mevedel-worktree--collect-status)))))))
      (delete-directory root t)))

  :doc "reports submodule status"
  (let* ((root (file-name-as-directory
                (make-temp-file "mevedel-worktree-submodule-" t)))
         (responses
          `((("rev-parse" "--show-superproject-working-tree")
             . ,(mevedel-worktree-test--git-result "/super/")))))
    (unwind-protect
        (mevedel-worktree-test--with-session root
          (cl-letf (((symbol-function 'mevedel-worktree--git-result)
                     (lambda (_dir &rest args)
                       (or (cdr (assoc args responses))
                           (mevedel-worktree-test--base-response
                            root args)))))
            (should (string-match-p
                     "Isolation: submodule"
                     (mevedel-worktree--format-status
                      (mevedel-worktree--collect-status))))))
      (delete-directory root t)))

  :doc "reports non-Git status without blocking on active request"
  (let* ((root (file-name-as-directory
                (make-temp-file "mevedel-worktree-notgit-" t)))
         (responses
          `((("rev-parse" "--show-toplevel")
             . ,(mevedel-worktree-test--git-result "fatal" 128)))))
    (unwind-protect
        (mevedel-worktree-test--with-session root
          (setq-local mevedel--current-request t)
          (cl-letf (((symbol-function 'mevedel-worktree--git-result)
                     (lambda (_dir &rest args)
                       (or (cdr (assoc args responses))
                           (mevedel-worktree-test--base-response
                            root args)))))
            (should (string-match-p
                     "Isolation: not a Git repository"
                     (mevedel-worktree--format-status
                      (mevedel-worktree--collect-status))))))
      (delete-directory root t))))


;;
;;; Surface

(mevedel-deftest mevedel-worktree-status--data-buffer ()
  ,test
  (test)

  :doc "uses the transient original buffer when it is live"
  (let ((data-buffer (generate-new-buffer " *mwt-status-data*")))
    (unwind-protect
        (with-temp-buffer
          (let ((transient--original-buffer data-buffer))
            (should (eq (mevedel-worktree-status--data-buffer)
                        data-buffer))))
      (mevedel-worktree-test--cleanup-surfaces data-buffer)))

  :doc "falls back to the current buffer outside transient commands"
  (with-temp-buffer
    (should (eq (mevedel-worktree-status--data-buffer)
                (current-buffer)))))

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

(mevedel-deftest mevedel-worktree-status--description ()
  ,test
  (test)

  :doc "renders compact dynamic worktree status rows"
  (let* ((root (file-name-as-directory
                (make-temp-file "mevedel-worktree-desc-" t)))
         (workspace (mevedel-worktree-test--workspace root))
         (session (mevedel-session-create "main" workspace root))
         (status (plist-put (mevedel-worktree-test--status root)
                            :session session)))
    (unwind-protect
        (with-temp-buffer
          (cl-letf (((symbol-function 'mevedel-worktree--collect-status)
                     (lambda (&optional _context) status)))
            (let ((description (mevedel-worktree-status--description)))
              (should (string-match-p "mevedel worktree" description))
              (should (string-match-p "Repo:" description))
              (should (string-match-p "Session:    main" description))
              (should (string-match-p "Directory:" description))
              (should (string-match-p "Isolation:  normal checkout"
                                      description))
              (should (string-match-p "Branch:     main" description))
              (should (string-match-p ".worktrees: ignored" description))
              (should (string-match-p "Dirty:      no" description))
              (should (string-match-p "Worktrees:  1" description)))))
      (delete-directory root t))))

(mevedel-deftest mevedel-worktree-status-create ()
  ,test
  (test)

  :doc "delegates create in the data buffer and reopens status"
  (let ((data-buffer (generate-new-buffer " *mwt-status-create*"))
        called-buffer
        called-args
        reopened
        message-text)
    (unwind-protect
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
              (should (equal (mevedel-worktree-status-create) "created")))))
      (mevedel-worktree-test--cleanup-surfaces data-buffer))
    (should (eq called-buffer data-buffer))
    (should (equal called-args "create"))
    (should (eq reopened data-buffer))
    (should (equal message-text "created"))))

(mevedel-deftest mevedel-worktree-status-list ()
  ,test
  (test)

  :doc "opens the list from the transient data buffer"
  (let ((data-buffer (generate-new-buffer " *mwt-status-list*"))
        called-buffer)
    (unwind-protect
        (with-current-buffer data-buffer
          (let ((transient--original-buffer data-buffer))
            (cl-letf (((symbol-function 'mevedel-worktree-list-open)
                       (lambda () (setq called-buffer (current-buffer)))))
              (mevedel-worktree-status-list))))
      (mevedel-worktree-test--cleanup-surfaces data-buffer))
    (should (eq called-buffer data-buffer))))

(mevedel-deftest mevedel-worktree-status-refresh ()
  ,test
  (test)

  :doc "reopens the status transient from the data buffer"
  (let ((data-buffer (generate-new-buffer " *mwt-status-refresh*"))
        called-buffer)
    (unwind-protect
        (with-current-buffer data-buffer
          (let ((transient--original-buffer data-buffer))
            (cl-letf (((symbol-function 'mevedel-worktree-status-open)
                       (lambda () (setq called-buffer (current-buffer)))))
              (mevedel-worktree-status-refresh))))
      (mevedel-worktree-test--cleanup-surfaces data-buffer))
    (should (eq called-buffer data-buffer))))

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
          (should (string-match-p "RET  Show selected worktree details"
                                  (buffer-string)))))
    (mevedel-worktree-test--cleanup-surfaces)))

(mevedel-deftest mevedel-worktree-status-back ()
  ,test
  (test)

  :doc "returns to the main menu from the data buffer"
  (let ((data-buffer (generate-new-buffer " *mwt-status-back*"))
        called-buffer)
    (unwind-protect
        (with-current-buffer data-buffer
          (let ((transient--original-buffer data-buffer))
            (cl-letf (((symbol-function 'mevedel-menu)
                       (lambda () (setq called-buffer (current-buffer)))))
              (mevedel-worktree-status-back))))
      (mevedel-worktree-test--cleanup-surfaces data-buffer))
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

(mevedel-deftest mevedel-worktree-list--data-buffer ()
  ,test
  (test)

  :doc "returns the cockpit data buffer before the legacy fallback"
  (let ((view-buffer (generate-new-buffer " *mwt-owner-view*"))
        (data-buffer (generate-new-buffer " *mwt-owner-data*"))
        (legacy-buffer (generate-new-buffer " *mwt-owner-legacy*")))
    (unwind-protect
        (let ((buffer
               (cl-letf (((symbol-function 'mevedel-worktree-list-refresh)
                          #'ignore))
                 (mevedel-worktree-list-open
                  view-buffer data-buffer data-buffer))))
          (with-current-buffer buffer
            (setq mevedel-worktree-list--data-buffer legacy-buffer)
            (should (eq (mevedel-worktree-list--data-buffer)
                        data-buffer))))
      (mevedel-worktree-test--cleanup-surfaces
       view-buffer data-buffer legacy-buffer)))

  :doc "falls back to the legacy worktree data buffer"
  (let ((legacy-buffer (generate-new-buffer " *mwt-owner-fallback*")))
    (unwind-protect
        (with-temp-buffer
          (setq mevedel-worktree-list--data-buffer legacy-buffer)
          (should (eq (mevedel-worktree-list--data-buffer)
                      legacy-buffer)))
      (mevedel-worktree-test--cleanup-surfaces legacy-buffer)))

  :doc "signals when no data buffer is live"
  (with-temp-buffer
    (should-error (mevedel-worktree-list--data-buffer)
                  :type 'user-error)))

(mevedel-deftest mevedel-worktree-list--require-owner ()
  ,test
  (test)

  :doc "requires live cockpit owner buffers"
  (let* ((root (make-temp-file "mevedel-worktree-owner-" t))
         (status (mevedel-worktree-test--status root))
         (view-buffer (generate-new-buffer " *mwt-require-view*"))
         (data-buffer (generate-new-buffer " *mwt-require-data*")))
    (unwind-protect
        (let ((buffer (mevedel-worktree-test--open-list
                       status data-buffer view-buffer)))
          (with-current-buffer buffer
            (should (mevedel-worktree-list--require-owner))
            (kill-buffer data-buffer)
            (should-error (mevedel-worktree-list--require-owner)
                          :type 'user-error)))
      (mevedel-worktree-test--cleanup-surfaces view-buffer data-buffer)
      (delete-directory root t))))

(mevedel-deftest mevedel-worktree-list--status ()
  ,test
  (test)

  :doc "collects status in the owning data buffer"
  (let ((data-buffer (generate-new-buffer " *mwt-status-owner*"))
        called-buffer)
    (unwind-protect
        (with-temp-buffer
          (setq mevedel-worktree-list--data-buffer data-buffer)
          (cl-letf (((symbol-function 'mevedel-worktree--collect-status)
                     (lambda (&optional _context)
                       (setq called-buffer (current-buffer))
                       :status)))
            (should (eq (mevedel-worktree-list--status) :status))))
      (mevedel-worktree-test--cleanup-surfaces data-buffer))
    (should (eq called-buffer data-buffer))))

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
      (delete-directory root t))))

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
         (cells (cadr entry)))
    (should (equal (car entry) "/repo/"))
    (should (equal (aref cells 0) "/repo/"))
    (should (equal (aref cells 1) "main"))
    (should (equal (aref cells 2) "abc123"))
    (should (equal (aref cells 3) "yes"))
    (should (equal (aref cells 4) "branch"))
    (should (equal (aref cells 5) "main, alt"))))

(mevedel-deftest mevedel-worktree-list-refresh ()
  ,test
  (test)

  :doc "refreshes rows and preserves the selected worktree where possible"
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
                         view-buffer data-buffer data-buffer)))
            (with-current-buffer buffer
              (mevedel-cockpit-goto-id other)
              (setq status
                    (mevedel-worktree-test--status
                     root
                     (list (list :path root :branch "main" :head "abc")
                           (list :path other :branch "updated"
                                 :head "fff"))))
              (mevedel-worktree-list-refresh)
              (should (equal (tabulated-list-get-id) other))
              (should (equal (aref (cadr (assoc other
                                                tabulated-list-entries))
                                  1)
                             "updated")))))
      (mevedel-worktree-test--cleanup-surfaces view-buffer data-buffer)
      (delete-directory root t))))

(mevedel-deftest mevedel-worktree-list--selected-item ()
  ,test
  (test)

  :doc "returns the item represented by the current row"
  (let* ((root (file-name-as-directory
                (make-temp-file "mevedel-worktree-selected-" t)))
         (other (file-name-as-directory
                 (file-name-concat root ".worktrees" "other")))
         (status (mevedel-worktree-test--status
                  root
                  (list (list :path root :branch "main" :head "abc")
                        (list :path other :branch "topic" :head "def"))))
         (data-buffer (generate-new-buffer " *mwt-selected-data*")))
    (unwind-protect
        (let ((buffer (mevedel-worktree-test--open-list
                       status data-buffer)))
          (with-current-buffer buffer
            (mevedel-cockpit-goto-id other)
            (should (equal (plist-get
                            (mevedel-worktree-list--selected-item)
                            :path)
                           other))))
      (mevedel-worktree-test--cleanup-surfaces data-buffer)
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
  (:after-each (mevedel-worktree-test--cleanup-surfaces))
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
          (setq mevedel-worktree-list--data-buffer data-buffer)
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

(mevedel-deftest mevedel-worktree-list-help ()
  ,test
  (test)

  :doc "opens worktree list help"
  (unwind-protect
      (progn
        (mevedel-worktree-list-help)
        (with-current-buffer mevedel-worktree-help-buffer-name
          (should (string-match-p "o    Open selected worktree session"
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

  :doc "opens a tabulated worktree list for live owner buffers"
  (let* ((root (file-name-as-directory
                (make-temp-file "mevedel-worktree-list-" t)))
         (status (mevedel-worktree-test--status root))
         (view-buffer (generate-new-buffer " *mwt-list-view*"))
         (data-buffer (generate-new-buffer " *mwt-list-data*")))
    (unwind-protect
        (let ((buffer (mevedel-worktree-test--open-list
                       status data-buffer view-buffer)))
          (with-current-buffer buffer
            (should (derived-mode-p 'mevedel-worktree-list-mode))
            (should (eq mevedel-worktree-list--data-buffer data-buffer))
            (should (equal tabulated-list-sort-key '("Path" . nil)))
            (should (= 1 (length tabulated-list-entries)))
            (should (assoc root tabulated-list-entries))))
      (mevedel-worktree-test--cleanup-surfaces view-buffer data-buffer)
      (delete-directory root t))))

(mevedel-deftest mevedel-cmd--worktree/status-surface ()
  ,test
  (test)

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
                        ((symbol-function 'mevedel-session-persistence-save)
                         (lambda (_session _buffer) (setq saved t)))
                        ((symbol-function 'mevedel-worktree--git-result)
                         (lambda (_dir &rest args)
                           (if (equal args '("status" "--short"))
                               (mevedel-worktree-test--git-result " M foo.el")
                             (mevedel-worktree-test--base-response
                              root args)))))
                (let ((mevedel-session-persistence t))
                  (mevedel-cmd--worktree
                   "create worktree/foo --for \"fix the parser\""))
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
                        ((symbol-function 'mevedel-session-persistence-save)
                         (lambda (_session _buffer) (setq saved t)))
                        ((symbol-function 'mevedel-worktree--git-result)
                         (lambda (_dir &rest args)
                           (mevedel-worktree-test--base-response root args))))
                (let ((mevedel-session-persistence t))
                  (mevedel-cmd--worktree
                   "create worktree/foo --for \"ignored\" --clean")))
              (with-current-buffer new-buffer
                (should (string-empty-p
                         (buffer-substring-no-properties
                          (point-min) (point-max)))))
              (should-not saved))))
      (when (buffer-live-p new-buffer)
        (kill-buffer new-buffer))
      (delete-directory root t))))

;;; test-mevedel-worktree.el ends here
