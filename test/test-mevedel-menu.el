;;; test-mevedel-menu.el -- Tests for session cockpit -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name
               load-file-name
               byte-compile-current-file))
          "helpers"))
(require 'mevedel-menu)
(require 'mevedel-mentions)
(require 'mevedel-structs)
(require 'mevedel-view)
(require 'mevedel-workspace)

(defvar transient--original-buffer)

(defmacro mevedel-menu-test--with-buffers (&rest body)
  "Execute BODY with a paired data and view buffer."
  (declare (indent 0) (debug t))
  `(let* ((root (make-temp-file "mevedel-menu-root-" t))
          (mevedel-user-dir (file-name-as-directory
                             (make-temp-file "mevedel-menu-user-" t)))
          (workspace (mevedel-workspace-get-or-create
                      'project (format "menu-%s" root) root "mevedel"))
          (session (mevedel-session-create "main" workspace))
          (data-buf (generate-new-buffer " *menu-data*"))
          (view-buf (generate-new-buffer " *menu-view*")))
     (unwind-protect
         (progn
           (with-current-buffer data-buf
             (org-mode)
             (setq-local default-directory (file-name-as-directory root))
             (setq-local mevedel--session session)
             (setq-local mevedel-permission-mode 'default)
             (setq-local gptel-model 'gpt-5.5)
             (setq-local gptel-tools '(read edit)))
           (mevedel-view--setup view-buf data-buf)
           ,@body)
       (when (buffer-live-p view-buf) (kill-buffer view-buf))
       (when (buffer-live-p data-buf) (kill-buffer data-buf))
       (when (file-directory-p mevedel-user-dir)
         (delete-directory mevedel-user-dir t))
       (when (file-directory-p root)
         (delete-directory root t)))))

(mevedel-deftest mevedel-menu ()
  ,test
  (test)
  :doc "opens the top cockpit from the view buffer"
  (mevedel-menu-test--with-buffers
    (let (called-prefix called-buffer)
      (cl-letf (((symbol-function 'transient-setup)
                 (lambda (prefix &rest _)
                   (setq called-prefix prefix
                         called-buffer (current-buffer)))))
        (with-current-buffer view-buf
          (call-interactively #'mevedel-menu)))
      (should (eq called-prefix 'mevedel-menu--top))
      (should (eq called-buffer view-buf))))

  :doc "opens the top cockpit from the paired data buffer"
  (mevedel-menu-test--with-buffers
    (let (called-prefix called-buffer)
      (cl-letf (((symbol-function 'transient-setup)
                 (lambda (prefix &rest _)
                   (setq called-prefix prefix
                         called-buffer (current-buffer)))))
        (with-current-buffer data-buf
          (call-interactively #'mevedel-menu)))
      (should (eq called-prefix 'mevedel-menu--top))
      (should (eq called-buffer data-buf)))))

(mevedel-deftest mevedel-menu-open ()
  ,test
  (test)
  :doc "opens the requested top area"
  (mevedel-menu-test--with-buffers
    (let (called-prefix)
      (cl-letf (((symbol-function 'transient-setup)
                 (lambda (prefix &rest _)
                   (setq called-prefix prefix))))
        (with-current-buffer view-buf
          (mevedel-menu-open 'top)))
      (should (eq called-prefix 'mevedel-menu--top))))

  :doc "signals outside a live view/data pair"
  (with-temp-buffer
    (should-error (mevedel-menu-open 'top) :type 'user-error)))

(mevedel-deftest mevedel-menu--pair ()
  ,test
  (test)
  :doc "resolves from view buffer"
  (mevedel-menu-test--with-buffers
    (with-current-buffer view-buf
      (should (equal (mevedel-menu--pair)
                     (cons view-buf data-buf)))))

  :doc "resolves from data buffer"
  (mevedel-menu-test--with-buffers
    (with-current-buffer data-buf
      (should (equal (mevedel-menu--pair)
                     (cons view-buf data-buf)))))

  :doc "resolves from transient original buffer"
  (mevedel-menu-test--with-buffers
    (let ((transient--original-buffer view-buf))
      (with-temp-buffer
        (should (equal (mevedel-menu--pair)
                       (cons view-buf data-buf))))))

  :doc "rejects unrelated buffers"
  (with-temp-buffer
    (should-error (mevedel-menu--pair) :type 'user-error)))

(mevedel-deftest mevedel-menu--header ()
  ,test
  (test)
  :doc "shows session orientation and idle state"
  (mevedel-menu-test--with-buffers
    (with-current-buffer view-buf
      (let ((header (mevedel-menu--header)))
        (should (string-match-p "mevedel: main" header))
        (should (string-match-p "ask" header))
        (should (string-match-p "idle" header)))))

  :doc "shows running request state"
  (mevedel-menu-test--with-buffers
    (with-current-buffer data-buf
      (setq-local mevedel--current-request t))
    (with-current-buffer view-buf
      (should (string-match-p "running" (mevedel-menu--header))))))

(mevedel-deftest mevedel-menu--worktree-label ()
  ,test
  (test)
  :doc "shows the current branch"
  (mevedel-menu-test--with-buffers
    (cl-letf (((symbol-function 'mevedel-menu--git-line)
               (lambda (_directory &rest args)
                 (pcase args
                   ('("branch" "--show-current") "main")
                   ('("rev-parse" "--short" "HEAD") "abc123")))))
      (with-current-buffer view-buf
        (should (string= "main" (mevedel-menu--worktree-label))))))

  :doc "falls back to detached HEAD"
  (mevedel-menu-test--with-buffers
    (cl-letf (((symbol-function 'mevedel-menu--git-line)
               (lambda (_directory &rest args)
                 (pcase args
                   ('("branch" "--show-current") "")
                   ('("rev-parse" "--short" "HEAD") "abc123")))))
      (with-current-buffer view-buf
        (should (string= "detached abc123"
                         (mevedel-menu--worktree-label))))))

  :doc "reports non-Git directories"
  (mevedel-menu-test--with-buffers
    (cl-letf (((symbol-function 'mevedel-menu--git-line)
               (lambda (&rest _) nil)))
      (with-current-buffer view-buf
        (should (string= "not-git" (mevedel-menu--worktree-label)))))))

(mevedel-deftest mevedel-menu--worktree-description ()
  ,test
  (test)
  :doc "shows worktree description with branch label"
  (cl-letf (((symbol-function 'mevedel-menu--worktree-label)
             (lambda () "main")))
    (should (string= "Worktree main"
                     (mevedel-menu--worktree-description)))))

(mevedel-deftest mevedel-menu--send ()
  ,test
  (test)
  :doc "send runs in the paired view buffer"
  (mevedel-menu-test--with-buffers
    (let (called-buffer)
      (cl-letf (((symbol-function 'mevedel-view-send)
                 (lambda ()
                   (interactive)
                   (setq called-buffer (current-buffer)))))
        (with-current-buffer data-buf
          (mevedel-menu--send))
        (should (eq called-buffer view-buf))))))

(mevedel-deftest mevedel-menu--abort ()
  ,test
  (test)
  :doc "abort runs in the paired view buffer"
  (mevedel-menu-test--with-buffers
    (let (called-buffer)
      (cl-letf (((symbol-function 'mevedel-view-abort)
                 (lambda ()
                   (interactive)
                   (setq called-buffer (current-buffer)))))
        (with-current-buffer data-buf
          (mevedel-menu--abort))
        (should (eq called-buffer view-buf))))))

(mevedel-deftest mevedel-menu--compact ()
  ,test
  (test)
  :doc "compact runs in the paired data buffer"
  (mevedel-menu-test--with-buffers
    (let (called-buffer)
      (cl-letf (((symbol-function 'mevedel-compact)
                 (lambda (&rest _)
                   (interactive)
                   (setq called-buffer (current-buffer)))))
        (with-current-buffer view-buf
          (mevedel-menu--compact))
        (should (eq called-buffer data-buf))))))

(mevedel-deftest mevedel-menu--review ()
  ,test
  (test)
  :doc "review runs in the paired data buffer"
  (mevedel-menu-test--with-buffers
    (let (called-buffer)
      (cl-letf (((symbol-function 'mevedel-review)
                 (lambda (&rest _)
                   (interactive)
                   (setq called-buffer (current-buffer)))))
        (with-current-buffer view-buf
          (mevedel-menu--review))
        (should (eq called-buffer data-buf))))))

(mevedel-deftest mevedel-menu--verify ()
  ,test
  (test)
  :doc "verify runs in the paired data buffer"
  (mevedel-menu-test--with-buffers
    (let (called-buffer)
      (cl-letf (((symbol-function 'mevedel-verify)
                 (lambda (&rest _)
                   (interactive)
                   (setq called-buffer (current-buffer)))))
        (with-current-buffer view-buf
          (mevedel-menu--verify))
        (should (eq called-buffer data-buf))))))

(mevedel-deftest mevedel-menu--toggle-data-view ()
  ,test
  (test)
  :doc "toggle data/view switches both directions"
  (mevedel-menu-test--with-buffers
    (switch-to-buffer view-buf)
    (mevedel-menu--toggle-data-view)
    (should (eq (window-buffer (selected-window)) data-buf))
    (with-current-buffer data-buf
      (mevedel-menu--toggle-data-view))
    (should (eq (window-buffer (selected-window)) view-buf))))

(mevedel-deftest mevedel-menu--send-inapt-p ()
  ,test
  (test)
  :doc "idle session can send"
  (mevedel-menu-test--with-buffers
    (with-current-buffer view-buf
      (should-not (mevedel-menu--send-inapt-p))))

  :doc "running session cannot send"
  (mevedel-menu-test--with-buffers
    (with-current-buffer data-buf
      (setq-local mevedel--current-request t))
    (with-current-buffer view-buf
      (should (mevedel-menu--send-inapt-p)))))

(mevedel-deftest mevedel-menu--abort-inapt-p ()
  ,test
  (test)
  :doc "idle session cannot abort"
  (mevedel-menu-test--with-buffers
    (with-current-buffer view-buf
      (should (mevedel-menu--abort-inapt-p))))

  :doc "running session can abort"
  (mevedel-menu-test--with-buffers
    (with-current-buffer data-buf
      (setq-local mevedel--current-request t))
    (with-current-buffer view-buf
      (should-not (mevedel-menu--abort-inapt-p)))))

(mevedel-deftest mevedel-view-mode-map ()
  ,test
  (test)
  :doc "view mode binds the cockpit command"
  (should (eq (lookup-key mevedel-view-mode-map (kbd "C-c C-m"))
              #'mevedel-menu)))

(provide 'test-mevedel-menu)

;;; test-mevedel-menu.el ends here
