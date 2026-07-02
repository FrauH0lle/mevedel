;;; test-mevedel-worktree.el --- Tests for worktree sessions -*- lexical-binding: t -*-

;;; Commentary:

;; Focused command-seam coverage for `/worktree'.

;;; Code:

(require 'mevedel-worktree)
(require 'mevedel-chat)
(require 'mevedel-mentions)
(require 'mevedel-session-persistence)
(require 'mevedel-view)
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

(mevedel-deftest mevedel-worktree-list-open
  (:after-each (when (get-buffer mevedel-worktree-list-buffer-name)
                 (kill-buffer mevedel-worktree-list-buffer-name)))
  ,test
  (test)

  :doc "opens a refreshable status surface for the current data buffer"
  (with-temp-buffer
    (let ((data-buffer (current-buffer)))
      (cl-letf (((symbol-function 'mevedel-worktree--collect-status)
                 (lambda (&optional _context) :status))
                ((symbol-function 'mevedel-worktree--format-status)
                 (lambda (status)
                   (should (eq status :status))
                   "Worktree status\nBranch: main")))
        (let ((buffer (mevedel-worktree-list-open)))
          (with-current-buffer buffer
            (should (derived-mode-p 'mevedel-worktree-list-mode))
            (should (eq mevedel-worktree-list--data-buffer data-buffer))
            (should (string-match-p
                     "Keys: g refresh/status, c create worktree"
                     (buffer-string)))
            (should (string-match-p "Branch: main"
                                    (buffer-string)))))))))

(mevedel-deftest mevedel-worktree-list-create
  (:after-each (when (get-buffer mevedel-worktree-list-buffer-name)
                 (kill-buffer mevedel-worktree-list-buffer-name)))
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

(mevedel-deftest mevedel-cmd--worktree/status-surface ()
  ,test
  (test)

  :doc "routes blank and status commands to the worktree surface"
  (with-temp-buffer
    (let (opened-buffer)
      (cl-letf (((symbol-function 'mevedel-worktree-list-open)
                 (lambda () (setq opened-buffer (current-buffer)))))
        (dolist (args '("" "status"))
          (setq opened-buffer nil)
          (should (null (mevedel-cmd--worktree args)))
          (should (eq opened-buffer (current-buffer))))))))


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
