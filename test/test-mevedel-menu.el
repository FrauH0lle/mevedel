;;; test-mevedel-menu.el -- Tests for session cockpit -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'gptel)
(require 'gptel-openai)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name
               load-file-name
               byte-compile-current-file))
          "helpers"))
(require 'mevedel-cockpit)
(require 'mevedel-gptel-bridge)
(require 'mevedel-menu)
(require 'mevedel-mentions)
(require 'mevedel-plugins)
(require 'mevedel-skills)
(require 'mevedel-structs)
(require 'mevedel-tools)
(require 'mevedel-tools-list)
(require 'mevedel-view)
(require 'mevedel-workspace)
(require 'mevedel-worktree)

;; `gptel'
(defvar gptel--known-backends)
(defvar gptel-backend)
(defvar gptel-model)
(defvar gptel-tools)

(defmacro mevedel-menu-test--with-model-backends (&rest body)
  "Run BODY with isolated gptel model backends."
  (declare (indent 0) (debug t))
  `(let ((gptel--known-backends nil))
     (gptel-make-openai "Fast" :key "test" :models '(fast-model))
     (gptel-make-openai "Balanced" :key "test" :models '(balanced-model))
     ,@body))

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
           (mevedel-gptel-bridge--clear-return-state)
           (mevedel-gptel-bridge--cleanup-advice)
           (with-current-buffer data-buf
             (org-mode)
             (setq-local default-directory (file-name-as-directory root))
             (setq-local mevedel--session session)
             (setq-local mevedel-permission-mode 'default)
             (setq-local gptel-model 'gpt-5.5)
             (setq-local gptel-tools '(read edit)))
           (mevedel-view--setup view-buf data-buf)
           ,@body)
       (mevedel-gptel-bridge--clear-return-state)
       (mevedel-gptel-bridge--cleanup-advice)
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

  :doc "opens requested mode and model cockpit surfaces"
  (mevedel-menu-test--with-buffers
    (let (called-prefix)
      (cl-letf (((symbol-function 'transient-setup)
                 (lambda (prefix &rest _)
                   (setq called-prefix prefix))))
        (with-current-buffer view-buf
          (dolist (area '((mode . mevedel-menu--mode)
                          (model . mevedel-menu--model)))
            (setq called-prefix nil)
            (mevedel-menu-open (car area))
            (should (eq called-prefix (cdr area))))))))

  :doc "opens requested tools, skills, and plugins management surfaces"
  (mevedel-menu-test--with-buffers
    (let (tools-context tools-buffer
          skills-context skills-buffer
          plugins-context plugins-buffer)
      (cl-letf (((symbol-function 'mevedel-tools-list-open)
                 (lambda (context)
                   (setq tools-context context
                         tools-buffer (current-buffer))))
                ((symbol-function 'mevedel-skills-list-open)
                 (lambda (context)
                   (setq skills-context context
                         skills-buffer (current-buffer))))
                ((symbol-function 'mevedel-plugins-list-open)
                 (lambda (context)
                   (setq plugins-context context
                         plugins-buffer (current-buffer)))))
        (with-current-buffer view-buf
          (mevedel-menu-open 'tools)
          (mevedel-menu-open 'skills)
          (mevedel-menu-open 'plugins)))
      (should (eq (mevedel-cockpit-context-session tools-context) session))
      (should (eq (mevedel-cockpit-context-view-buffer tools-context)
                  view-buf))
      (should (eq (mevedel-cockpit-context-data-buffer tools-context)
                  data-buf))
      (should (eq (mevedel-cockpit-context-origin-buffer tools-context)
                  view-buf))
      (should (eq tools-buffer data-buf))
      (should (eq (mevedel-cockpit-context-session skills-context)
                  session))
      (should (eq (mevedel-cockpit-context-view-buffer skills-context)
                  view-buf))
      (should (eq (mevedel-cockpit-context-data-buffer skills-context)
                  data-buf))
      (should (eq (mevedel-cockpit-context-origin-buffer skills-context)
                  view-buf))
      (should (eq skills-buffer data-buf))
      (should (eq (mevedel-cockpit-context-workspace plugins-context)
                  (mevedel-session-workspace session)))
      (should (eq (mevedel-cockpit-context-view-buffer plugins-context)
                  view-buf))
      (should (eq (mevedel-cockpit-context-data-buffer plugins-context)
                  data-buf))
      (should (eq (mevedel-cockpit-context-origin-buffer plugins-context)
                  view-buf))
      (should (eq plugins-buffer data-buf))))

  :doc "opens plugins management surface from the paired data buffer"
  (mevedel-menu-test--with-buffers
    (let (plugins-context plugins-buffer)
      (cl-letf (((symbol-function 'mevedel-plugins-list-open)
                 (lambda (context)
                   (setq plugins-context context
                         plugins-buffer (current-buffer)))))
        (with-current-buffer data-buf
          (mevedel-menu-open 'plugins)))
      (should (eq (mevedel-cockpit-context-view-buffer plugins-context)
                  view-buf))
      (should (eq (mevedel-cockpit-context-data-buffer plugins-context)
                  data-buf))
      (should (eq (mevedel-cockpit-context-origin-buffer plugins-context)
                  data-buf))
      (should (eq plugins-buffer data-buf))))

  :doc "opens requested worktree and help cockpit surfaces"
  (mevedel-menu-test--with-buffers
    (let (worktree-buffer help-opened)
      (cl-letf (((symbol-function 'mevedel-worktree-status-open)
                 (lambda () (setq worktree-buffer (current-buffer))))
                ((symbol-function 'mevedel-menu-help-open)
                 (lambda () (setq help-opened t))))
        (with-current-buffer view-buf
          (mevedel-menu-open 'worktree)
          (mevedel-menu-open 'help)))
      (should (eq worktree-buffer data-buf))
      (should help-opened)))

  :doc "opens the requested gptel bridge area"
  (mevedel-menu-test--with-buffers
    (let (called-context)
      (cl-letf (((symbol-function 'mevedel-gptel-bridge-open)
                 (lambda (context)
                   (setq called-context context))))
        (with-current-buffer data-buf
          (mevedel-menu-open 'gptel)))
      (should (eq (mevedel-cockpit-context-data-buffer called-context)
                  data-buf))))

  :doc "signals outside a live view/data pair"
  (with-temp-buffer
    (should-error (mevedel-menu-open 'top) :type 'user-error)))

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
    (cl-letf (((symbol-function 'mevedel-worktree-status-summary)
               (lambda (&optional _context)
                 '(:state normal-checkout :label "main"))))
      (with-current-buffer view-buf
        (should (string= "main" (mevedel-menu--worktree-label))))))

  :doc "falls back to detached HEAD"
  (mevedel-menu-test--with-buffers
    (cl-letf (((symbol-function 'mevedel-worktree-status-summary)
               (lambda (&optional _context)
                 '(:state normal-checkout :label "detached abc123"))))
      (with-current-buffer view-buf
        (should (string= "detached abc123"
                         (mevedel-menu--worktree-label))))))

  :doc "reports non-Git directories"
  (mevedel-menu-test--with-buffers
    (cl-letf (((symbol-function 'mevedel-worktree-status-summary)
               (lambda (&optional _context)
                 '(:state not-git :label "not-git"))))
      (with-current-buffer view-buf
        (should (string= "not-git" (mevedel-menu--worktree-label)))))))

(mevedel-deftest mevedel-menu--worktree-description ()
  ,test
  (test)
  :doc "shows worktree description with branch label"
  (mevedel-menu-test--with-buffers
    (cl-letf (((symbol-function 'mevedel-worktree-status-summary)
               (lambda (&optional _context)
                 '(:state normal-checkout :label "main"))))
      (with-current-buffer view-buf
        (should (string= "Worktree  main"
                         (substring-no-properties
                          (mevedel-menu--worktree-description))))))))

(mevedel-deftest mevedel-menu--top-descriptions ()
  ,test
  (test)
  :doc "shows padded top-level state rows"
  (mevedel-menu-test--with-buffers
    (cl-letf (((symbol-function 'mevedel-worktree-status-summary)
               (lambda (&optional _context)
                 '(:state normal-checkout :label "main"))))
      (with-current-buffer view-buf
        (should (string= "Mode      ask"
                         (substring-no-properties
                          (mevedel-menu--mode-description))))
        (should (string= "Model     gpt-5.5"
                         (substring-no-properties
                          (mevedel-menu--model-description))))
        (should (string= "Tools     2 active"
                         (substring-no-properties
                          (mevedel-menu--tools-description))))
        (should (string-match-p
                 (rx string-start "Skills" (+ space) (+ digit) "/" (+ digit)
                     string-end)
                 (substring-no-properties
                  (mevedel-menu--skills-description))))
        (should (string-match-p
                 (rx string-start "Plugins" (+ space) (+ digit) "/" (+ digit)
                     string-end)
                 (substring-no-properties
                  (mevedel-menu--plugins-description))))
        (should (string= "Worktree  main"
                         (substring-no-properties
                          (mevedel-menu--worktree-description))))))))

(mevedel-deftest mevedel-menu--mode-choice-description ()
  ,test
  (test)
  :doc "marks the active mode without exposing internal mode names"
  (mevedel-menu-test--with-buffers
    (with-current-buffer view-buf
      (should (string= "ask     current ask before write tools"
                       (substring-no-properties
                        (mevedel-menu--mode-default-description))))
      (should (string= "edits           auto-apply edit previews"
                       (substring-no-properties
                        (mevedel-menu--mode-accept-edits-description))))))

  :doc "updates the current marker when the session mode changes"
  (mevedel-menu-test--with-buffers
    (with-current-buffer view-buf
      (mevedel-menu--set-mode 'trust-all)
      (should (string= "auto!   current auto-allow tools"
                       (substring-no-properties
                        (mevedel-menu--mode-trust-all-description))))
      (should (string= "ask             ask before write tools"
                       (substring-no-properties
                        (mevedel-menu--mode-default-description)))))))

(mevedel-deftest mevedel-menu-help--text ()
  ,test
  (test)
  :doc "covers cockpit keys, slash routing, modes, and buffer ownership"
  (let ((text (mevedel-menu-help--text)))
    (dolist (needle '("Session cockpit keys"
                      "Next display"
                      "Previous query"
                      "Slash commands that open UI"
                      "Direct slash commands"
                      "/mode MODE, /model MODEL"
                      "Modes"
                      "View and data buffers"))
      (should (string-match-p (regexp-quote needle) text)))))

(mevedel-deftest mevedel-menu--mode-symbol ()
  ,test
  (test)
  :doc "uses the cockpit view as the permission surface"
  (mevedel-menu-test--with-buffers
    (with-current-buffer view-buf
      (setq-local mevedel-permission-mode 'trust-all)
      (should (eq (mevedel-menu--mode-symbol
                   session data-buf view-buf)
                  'trust-all))
      (should (string= "Mode      auto!"
                       (substring-no-properties
                        (mevedel-menu--mode-description)))))))

(mevedel-deftest mevedel-menu--set-mode ()
  ,test
  (test)
  :doc "mode setter updates the paired data buffer session"
  (mevedel-menu-test--with-buffers
    (with-current-buffer view-buf
      (mevedel-menu--set-mode 'accept-edits))
    (with-current-buffer data-buf
      (should (eq 'accept-edits
                  (mevedel-session-permission-mode mevedel--session)))
      (should (eq 'accept-edits mevedel-permission-mode)))))

(mevedel-deftest mevedel-menu--model-surface-description ()
  ,test
  (test)
  :doc "model surface description shows current backend and model"
  (mevedel-menu-test--with-model-backends
    (mevedel-menu-test--with-buffers
      (with-current-buffer data-buf
        (setq-local gptel-backend (gptel-get-backend "Fast"))
        (setq-local gptel-model 'fast-model))
      (with-current-buffer view-buf
        (should (string= "Current model: Fast:fast-model"
                         (substring-no-properties
                          (mevedel-menu--model-surface-description))))))))

(mevedel-deftest mevedel-menu--select-model ()
  ,test
  (test)
  :doc "model selector applies the chosen registered provider"
  (mevedel-menu-test--with-model-backends
    (mevedel-menu-test--with-buffers
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest _) "Balanced:balanced-model")))
        (with-current-buffer view-buf
          (mevedel-menu--select-model)))
      (with-current-buffer data-buf
        (should (equal "Balanced" (gptel-backend-name gptel-backend)))
        (should (eq 'balanced-model gptel-model)))))

  :doc "model selector rejects an empty model registry"
  (let ((gptel--known-backends nil))
    (mevedel-menu-test--with-buffers
      (with-current-buffer view-buf
        (should-error (mevedel-menu--select-model) :type 'user-error)))))

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

(mevedel-deftest mevedel-menu-help-open
  (:after-each (when (get-buffer mevedel-menu-help-buffer-name)
                 (kill-buffer mevedel-menu-help-buffer-name)))
  ,test
  (test)
  :doc "opens the help surface buffer"
  (let ((buffer (mevedel-menu-help-open)))
    (with-current-buffer buffer
      (should (derived-mode-p 'special-mode)))))

(mevedel-deftest mevedel-menu--open-gptel ()
  ,test
  (test)
  :doc "delegates to the gptel bridge with the current cockpit context"
  (mevedel-menu-test--with-buffers
    (let (called-context)
      (cl-letf (((symbol-function 'mevedel-gptel-bridge-open)
                 (lambda (context)
                   (setq called-context context))))
        (with-current-buffer view-buf
          (mevedel-menu--open-gptel)))
      (should (eq (mevedel-cockpit-context-view-buffer called-context)
                  view-buf))
      (should (eq (mevedel-cockpit-context-data-buffer called-context)
                  data-buf)))))

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

(provide 'test-mevedel-menu)

;;; test-mevedel-menu.el ends here
