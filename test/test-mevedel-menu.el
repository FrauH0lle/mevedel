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
(require 'mevedel-menu)
(require 'mevedel-mentions)
(require 'mevedel-plugins)
(require 'mevedel-skills)
(require 'mevedel-structs)
(require 'mevedel-tools)
(require 'mevedel-view)
(require 'mevedel-workspace)
(require 'mevedel-worktree)

;; `gptel'
(defvar gptel--known-backends)
(defvar gptel-backend)
(defvar gptel-model)
(defvar gptel-system-prompt)
(defvar gptel-tools)

;; `gptel-transient'
(declare-function gptel--set-with-scope "ext:gptel-transient"
                  (sym value &optional scope))

;; `transient'
(defvar transient--original-buffer)
(defvar transient--prefix)
(defvar transient-post-exit-hook)

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
           (mevedel-view--gptel-clear-return-state)
           (mevedel-menu--cleanup-gptel-bridge-advice)
           (with-current-buffer data-buf
             (org-mode)
             (setq-local default-directory (file-name-as-directory root))
             (setq-local mevedel--session session)
             (setq-local mevedel-permission-mode 'default)
             (setq-local gptel-model 'gpt-5.5)
             (setq-local gptel-tools '(read edit)))
           (mevedel-view--setup view-buf data-buf)
           ,@body)
       (mevedel-view--gptel-clear-return-state)
       (mevedel-menu--cleanup-gptel-bridge-advice)
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
    (let (tools-session tools-view tools-data tools-origin tools-buffer
          skills-session skills-view skills-data skills-origin
          skills-buffer
          plugins-workspace plugins-view plugins-data plugins-origin
          plugins-buffer)
      (cl-letf (((symbol-function 'mevedel-tools-list-open)
                 (lambda (session view-buffer data-buffer origin-buffer)
                   (setq tools-session session
                         tools-view view-buffer
                         tools-data data-buffer
                         tools-origin origin-buffer
                         tools-buffer (current-buffer))))
                ((symbol-function 'mevedel-skills-list-open)
                 (lambda (session view-buffer data-buffer origin-buffer)
                   (setq skills-session session
                         skills-view view-buffer
                         skills-data data-buffer
                         skills-origin origin-buffer
                         skills-buffer (current-buffer))))
                ((symbol-function 'mevedel-plugins-list-open)
                 (lambda (workspace view-buffer data-buffer origin-buffer)
                   (setq plugins-workspace workspace
                         plugins-view view-buffer
                         plugins-data data-buffer
                         plugins-origin origin-buffer
                         plugins-buffer (current-buffer)))))
        (with-current-buffer view-buf
          (mevedel-menu-open 'tools)
          (mevedel-menu-open 'skills)
          (mevedel-menu-open 'plugins)))
      (should (eq tools-session session))
      (should (eq tools-view view-buf))
      (should (eq tools-data data-buf))
      (should (eq tools-origin view-buf))
      (should (eq tools-buffer data-buf))
      (should (eq skills-session session))
      (should (eq skills-view view-buf))
      (should (eq skills-data data-buf))
      (should (eq skills-origin view-buf))
      (should (eq skills-buffer data-buf))
      (should (eq plugins-workspace (mevedel-session-workspace session)))
      (should (eq plugins-view view-buf))
      (should (eq plugins-data data-buf))
      (should (eq plugins-origin view-buf))
      (should (eq plugins-buffer data-buf))))

  :doc "opens plugins management surface from the paired data buffer"
  (mevedel-menu-test--with-buffers
    (let (plugins-view plugins-data plugins-origin plugins-buffer)
      (cl-letf (((symbol-function 'mevedel-plugins-list-open)
                 (lambda (_workspace view-buffer data-buffer origin-buffer)
                   (setq plugins-view view-buffer
                         plugins-data data-buffer
                         plugins-origin origin-buffer
                         plugins-buffer (current-buffer)))))
        (with-current-buffer data-buf
          (mevedel-menu-open 'plugins)))
      (should (eq plugins-view view-buf))
      (should (eq plugins-data data-buf))
      (should (eq plugins-origin data-buf))
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
    (let (called-buffer)
      (cl-letf (((symbol-function 'gptel-menu)
                 (lambda ()
                   (interactive)
                   (setq called-buffer (current-buffer)))))
        (with-current-buffer data-buf
          (mevedel-menu-open 'gptel)))
      (should (eq called-buffer data-buf))))

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
    (should (string= "Worktree  main"
                     (substring-no-properties
                      (mevedel-menu--worktree-description))))))

(mevedel-deftest mevedel-menu--top-descriptions ()
  ,test
  (test)
  :doc "shows padded top-level state rows"
  (mevedel-menu-test--with-buffers
    (cl-letf (((symbol-function 'mevedel-menu--worktree-label)
               (lambda () "main")))
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
                      "Slash commands that open UI"
                      "Direct slash commands"
                      "/mode MODE, /model MODEL"
                      "Modes"
                      "View and data buffers"))
      (should (string-match-p (regexp-quote needle) text)))))

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
  :doc "opens gptel-menu with gptel scoped setters owned by the data buffer"
  (mevedel-menu-test--with-buffers
    (let ((window (selected-window))
          called-buffer
          called-prompt
          called-window-buffer)
      (with-current-buffer data-buf
        (setq-local gptel-system-prompt "data prompt")
        (setq-local gptel-model 'data-model)
        (setq-local gptel-tools '(data-tool)))
      (with-current-buffer view-buf
        (setq-local gptel-system-prompt "view prompt")
        (setq-local gptel-model 'view-model)
        (setq-local gptel-tools '(view-tool)))
      (unwind-protect
          (progn
            (set-window-buffer window view-buf)
            (cl-letf (((symbol-function 'gptel-menu)
                       (lambda ()
                         (interactive)
                         (setq called-buffer (current-buffer)
                               called-prompt gptel-system-prompt
                               called-window-buffer
                               (window-buffer (selected-window)))
                         (with-current-buffer called-window-buffer
                           (gptel--set-with-scope
                            'gptel-system-prompt "bridge prompt" t)
                           (gptel--set-with-scope
                            'gptel-model 'bridge-model t)
                           (gptel--set-with-scope
                            'gptel-tools '(bridge-tool) t)))))
              (with-current-buffer view-buf
                (mevedel-menu--open-gptel)))
            (mevedel-view--gptel-return-to-view))
        (when (window-live-p window)
          (set-window-buffer window view-buf)))
      (should (eq called-buffer data-buf))
      (should (eq called-window-buffer data-buf))
      (should (equal called-prompt "data prompt"))
      (with-current-buffer data-buf
        (should (equal gptel-system-prompt "bridge prompt"))
        (should (eq gptel-model 'bridge-model))
        (should (equal gptel-tools '(bridge-tool))))
      (with-current-buffer view-buf
        (should (equal gptel-system-prompt "view prompt"))
        (should (eq gptel-model 'view-model))
        (should (equal gptel-tools '(view-tool))))))

  :doc "restores the view window after gptel displays the data buffer"
  (mevedel-menu-test--with-buffers
    (let ((window (selected-window)))
      (unwind-protect
          (progn
            (set-window-buffer window view-buf)
            (cl-letf (((symbol-function 'gptel-menu)
                       (lambda ()
                         (interactive)
                         (set-window-buffer window data-buf))))
              (with-current-buffer view-buf
                (mevedel-menu--open-gptel)))
            (should (eq (window-buffer window) data-buf))
            (should (memq #'mevedel-view--gptel-return-to-view
                          transient-post-exit-hook))
            (mevedel-view--gptel-return-to-view)
            (should (eq (window-buffer window) view-buf)))
        (when (window-live-p window)
          (set-window-buffer window view-buf)))))

  :doc "raw data-buffer bridge does not schedule view restoration"
  (mevedel-menu-test--with-buffers
    (cl-letf (((symbol-function 'gptel-menu) #'ignore))
      (with-current-buffer data-buf
        (mevedel-menu--open-gptel)))
    (should-not mevedel-view--gptel-return-view-buffer)))

(mevedel-deftest mevedel-menu--gptel-edit-directive-advice ()
  ,test
  (test)
  :doc "wraps edit callbacks so nested gptel menus run in the data buffer"
  (mevedel-menu-test--with-buffers
    (let (callback callback-buffer callback-prompt)
      (with-current-buffer data-buf
        (setq-local gptel-system-prompt "data prompt"))
      (with-current-buffer view-buf
        (setq-local gptel-system-prompt "view prompt"))
      (mevedel-view--gptel-schedule-return-to-view view-buf data-buf)
      (mevedel-menu--gptel-edit-directive-advice
       (lambda (&rest args)
         (setq callback (plist-get (cdr args) :callback)))
       'gptel-system-prompt
       :callback
       (lambda (_message)
         (setq callback-buffer (current-buffer)
               callback-prompt gptel-system-prompt)))
      (should callback)
      (with-temp-buffer
        (setq-local gptel-system-prompt "prompt buffer")
        (funcall callback "new prompt"))
      (should (eq callback-buffer data-buf))
      (should (equal callback-prompt "data prompt"))
      (should-not (mevedel-menu--gptel-bridge-active-p))))

  :doc "keeps bridge state when the callback opens another transient"
  (mevedel-menu-test--with-buffers
    (let (callback)
      (mevedel-view--gptel-schedule-return-to-view view-buf data-buf)
      (mevedel-menu--gptel-edit-directive-advice
       (lambda (&rest args)
         (setq callback (plist-get (cdr args) :callback)))
       'gptel-system-prompt
       :callback
       (lambda (_message) nil))
      (should callback)
      (let ((transient--prefix t))
        (funcall callback "new prompt"))
      (should (mevedel-menu--gptel-bridge-active-p)))))

(mevedel-deftest mevedel-menu--gptel-bridge-active-p ()
  ,test
  (test)
  :doc "returns non-nil while view restoration is pending"
  (mevedel-menu-test--with-buffers
    (mevedel-view--gptel-schedule-return-to-view view-buf data-buf)
    (should (mevedel-menu--gptel-bridge-active-p)))

  :doc "returns nil after restoration state clears"
  (mevedel-menu-test--with-buffers
    (mevedel-view--gptel-clear-return-state)
    (should-not (mevedel-menu--gptel-bridge-active-p))))

(mevedel-deftest mevedel-menu--install-gptel-bridge-advice ()
  ,test
  (test)
  :doc "installs temporary edit advice and cleanup hook"
  (mevedel-menu-test--with-buffers
    (mevedel-menu--install-gptel-bridge-advice)
    (should (advice-member-p #'mevedel-menu--gptel-edit-directive-advice
                             'gptel--edit-directive))
    (should (memq #'mevedel-menu--cleanup-gptel-bridge-advice
                  transient-post-exit-hook))))

(mevedel-deftest mevedel-menu--cleanup-gptel-bridge-advice ()
  ,test
  (test)
  :doc "keeps temporary advice while view restoration is pending"
  (mevedel-menu-test--with-buffers
    (mevedel-view--gptel-schedule-return-to-view view-buf data-buf)
    (mevedel-menu--install-gptel-bridge-advice)
    (mevedel-menu--cleanup-gptel-bridge-advice)
    (should (advice-member-p #'mevedel-menu--gptel-edit-directive-advice
                             'gptel--edit-directive))
    (should (memq #'mevedel-menu--cleanup-gptel-bridge-advice
                  transient-post-exit-hook)))

  :doc "removes temporary advice after restoration clears"
  (mevedel-menu-test--with-buffers
    (mevedel-menu--install-gptel-bridge-advice)
    (mevedel-view--gptel-clear-return-state)
    (mevedel-menu--cleanup-gptel-bridge-advice)
    (should-not
     (advice-member-p #'mevedel-menu--gptel-edit-directive-advice
                      'gptel--edit-directive))
    (should-not (memq #'mevedel-menu--cleanup-gptel-bridge-advice
                      transient-post-exit-hook))))

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
