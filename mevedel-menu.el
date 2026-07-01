;;; mevedel-menu.el -- Session cockpit transient -*- lexical-binding: t -*-

;;; Commentary:

;; Transient-backed session cockpit for mevedel sessions.  The cockpit
;; resolves the live view/data buffer pair once, then routes commands to
;; the buffer that owns the relevant state.

;;; Code:

(require 'transient)

;; `gptel'
(declare-function gptel--model-name "ext:gptel" (model))
(defvar gptel-model)
(defvar gptel-tools)

;; `gptel-transient'
(declare-function gptel-menu "ext:gptel-transient" ())

;; `mevedel-compact'
(declare-function mevedel-compact "mevedel-compact"
                  (&optional aggressive instructions))

;; `mevedel-plugins'
(declare-function mevedel-plugins-enabled "mevedel-plugins"
                  (&optional workspace))
(declare-function mevedel-plugins-list "mevedel-plugins"
                  (&optional workspace))

;; `mevedel-review'
(declare-function mevedel-review "mevedel-review" (&optional instructions))
(declare-function mevedel-verify "mevedel-review" (&optional instructions))

;; `mevedel-skills'
(declare-function mevedel-skills--skill-enabled-p "mevedel-skills"
                  (skill))

;; `mevedel-structs'
(declare-function mevedel-session-name "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-permission-mode "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-skills "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-workspace "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-working-directory "mevedel-structs" (cl-x) t)
(declare-function mevedel-workspace-root "mevedel-structs" (cl-x) t)
(defvar mevedel--current-request)
(defvar mevedel--data-buffer)
(defvar mevedel--session)
(defvar mevedel--view-buffer)
(defvar mevedel-permission-mode)

;; `mevedel-view'
(declare-function mevedel-view--gptel-edit-directive-args
                  "mevedel-view" (args))
(declare-function mevedel-view--gptel-return-to-view "mevedel-view" ())
(declare-function mevedel-view--gptel-schedule-return-to-view
                  "mevedel-view" (view-buffer data-buffer))
(declare-function mevedel-view-abort "mevedel-view" ())
(declare-function mevedel-view-send "mevedel-view" ())
(defvar mevedel-view--gptel-return-view-buffer)

;; `transient'
(defvar transient--original-buffer)
(defvar transient--prefix)
(defvar transient-post-exit-hook)


;;
;;; Pair resolution

(defun mevedel-menu--pair-for-buffer (buffer)
  "Return (VIEW-BUFFER . DATA-BUFFER) for BUFFER, or nil."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (cond
       ((and (derived-mode-p 'mevedel-view-mode)
             (boundp 'mevedel--data-buffer)
             mevedel--data-buffer
             (buffer-live-p mevedel--data-buffer))
        (cons buffer mevedel--data-buffer))
       ((and (boundp 'mevedel--view-buffer)
             mevedel--view-buffer
             (buffer-live-p mevedel--view-buffer)
             (with-current-buffer mevedel--view-buffer
               (and (derived-mode-p 'mevedel-view-mode)
                    (eq mevedel--data-buffer buffer))))
        (cons mevedel--view-buffer buffer))))))

(defun mevedel-menu--origin-buffer ()
  "Return the buffer that launched the current cockpit command."
  (cond
   ((mevedel-menu--pair-for-buffer (current-buffer))
    (current-buffer))
   ((and (boundp 'transient--original-buffer)
         (buffer-live-p transient--original-buffer)
         (mevedel-menu--pair-for-buffer transient--original-buffer))
    transient--original-buffer)))

(defun mevedel-menu--pair ()
  "Return the current cockpit's (VIEW-BUFFER . DATA-BUFFER) pair."
  (if-let* ((origin (mevedel-menu--origin-buffer))
            (pair (mevedel-menu--pair-for-buffer origin)))
      pair
    (user-error "No mevedel session cockpit here")))

(defun mevedel-menu--view-buffer ()
  "Return the current cockpit's view buffer."
  (car (mevedel-menu--pair)))

(defun mevedel-menu--data-buffer ()
  "Return the current cockpit's data buffer."
  (cdr (mevedel-menu--pair)))

(defun mevedel-menu--session (&optional data-buffer)
  "Return DATA-BUFFER's session."
  (with-current-buffer (or data-buffer (mevedel-menu--data-buffer))
    (and (boundp 'mevedel--session) mevedel--session)))

(defun mevedel-menu--call-in-view (function &rest args)
  "Call FUNCTION with ARGS in the current cockpit's view buffer."
  (with-current-buffer (mevedel-menu--view-buffer)
    (apply function args)))

(defun mevedel-menu--call-in-data (function &rest args)
  "Call FUNCTION with ARGS in the current cockpit's data buffer."
  (with-current-buffer (mevedel-menu--data-buffer)
    (apply function args)))


;;
;;; Labels

(defun mevedel-menu--mode-symbol (&optional session data-buffer)
  "Return the effective permission mode for SESSION and DATA-BUFFER."
  (or (and session (mevedel-session-permission-mode session))
      (with-current-buffer (or data-buffer (mevedel-menu--data-buffer))
        (and (boundp 'mevedel-permission-mode) mevedel-permission-mode))
      'default))

(defun mevedel-menu--mode-label (&optional mode)
  "Return the cockpit label for permission MODE."
  (pcase (or mode 'default)
    ('accept-edits "edits")
    ('trust-all "auto!")
    ('plan "plan")
    (_ "ask")))

(defun mevedel-menu--model-label ()
  "Return the current model label."
  (with-current-buffer (mevedel-menu--data-buffer)
    (cond
     ((not (bound-and-true-p gptel-model)) "none")
     ((fboundp 'gptel--model-name) (gptel--model-name gptel-model))
     (t (format "%s" gptel-model)))))

(defun mevedel-menu--active-tool-count ()
  "Return the number of active gptel tools in the data buffer."
  (with-current-buffer (mevedel-menu--data-buffer)
    (length (and (boundp 'gptel-tools) gptel-tools))))

(defun mevedel-menu--skill-count-label (session)
  "Return enabled/total skill count label for SESSION."
  (let ((enabled 0)
        (total 0))
    (dolist (skill (and session (mevedel-session-skills session)))
      (setq total (1+ total))
      (when (or (not (fboundp 'mevedel-skills--skill-enabled-p))
                (mevedel-skills--skill-enabled-p skill))
        (setq enabled (1+ enabled))))
    (format "%d/%d" enabled total)))

(defun mevedel-menu--plugin-count-label (workspace)
  "Return enabled/total plugin count label for WORKSPACE."
  (if (and workspace
           (fboundp 'mevedel-plugins-list)
           (fboundp 'mevedel-plugins-enabled))
      (format "%d/%d"
              (length (mevedel-plugins-enabled workspace))
              (length (mevedel-plugins-list workspace)))
    "0/0"))

(defun mevedel-menu--root-label (workspace)
  "Return a compact root label for WORKSPACE."
  (if (and workspace (fboundp 'mevedel-workspace-root))
      (abbreviate-file-name (mevedel-workspace-root workspace))
    "unknown"))

(defun mevedel-menu--working-directory (session data-buffer)
  "Return the effective working directory for SESSION and DATA-BUFFER."
  (file-name-as-directory
   (or (and session (mevedel-session-working-directory session))
       (with-current-buffer data-buffer default-directory))))

(defun mevedel-menu--git-line (directory &rest args)
  "Return trimmed Git output for ARGS in DIRECTORY, or nil on failure."
  (with-temp-buffer
    (let ((default-directory (file-name-as-directory
                              (expand-file-name directory))))
      (condition-case nil
          (when (eq 0 (apply #'process-file
                             "git" nil (list t nil) nil args))
            (string-trim
             (buffer-substring-no-properties (point-min) (point-max))))
        (file-missing nil)))))

(defun mevedel-menu--worktree-label ()
  "Return the current branch or detached HEAD label."
  (let* ((data-buffer (mevedel-menu--data-buffer))
         (session (mevedel-menu--session data-buffer))
         (directory (mevedel-menu--working-directory session data-buffer))
         (branch (mevedel-menu--git-line directory
                                         "branch" "--show-current"))
         (head (mevedel-menu--git-line directory
                                       "rev-parse" "--short" "HEAD")))
    (cond
     ((and branch (not (string-empty-p branch))) branch)
     ((and head (not (string-empty-p head))) (format "detached %s" head))
     (t "not-git"))))

(defun mevedel-menu--request-state-label (data-buffer)
  "Return the request state label for DATA-BUFFER."
  (with-current-buffer data-buffer
    (if (bound-and-true-p mevedel--current-request) "running" "idle")))

(defun mevedel-menu--header ()
  "Return the cockpit header string."
  (let* ((data-buffer (mevedel-menu--data-buffer))
         (session (mevedel-menu--session data-buffer))
         (workspace (and session (mevedel-session-workspace session)))
         (mode (mevedel-menu--mode-symbol session data-buffer)))
    (format "mevedel: %s  %s   %s · %s"
            (or (and session (mevedel-session-name session)) "unknown")
            (mevedel-menu--root-label workspace)
            (mevedel-menu--mode-label mode)
            (mevedel-menu--request-state-label data-buffer))))

(defun mevedel-menu--mode-description ()
  "Return the top-level mode row description."
  (format "Mode    %s"
          (mevedel-menu--mode-label
           (mevedel-menu--mode-symbol
            (mevedel-menu--session)
            (mevedel-menu--data-buffer)))))

(defun mevedel-menu--model-description ()
  "Return the top-level model row description."
  (format "Model   %s" (mevedel-menu--model-label)))

(defun mevedel-menu--tools-description ()
  "Return the top-level tools row description."
  (format "Tools   %d active" (mevedel-menu--active-tool-count)))

(defun mevedel-menu--skills-description ()
  "Return the top-level skills row description."
  (format "Skills  %s"
          (mevedel-menu--skill-count-label
           (mevedel-menu--session))))

(defun mevedel-menu--plugins-description ()
  "Return the top-level plugins row description."
  (let* ((session (mevedel-menu--session))
         (workspace (and session (mevedel-session-workspace session))))
    (format "Plugins %s" (mevedel-menu--plugin-count-label workspace))))

(defun mevedel-menu--worktree-description ()
  "Return the top-level worktree row description."
  (format "Worktree %s" (mevedel-menu--worktree-label)))


;;
;;; Inapt predicates

(defun mevedel-menu--request-active-p ()
  "Return non-nil when the current session has an active request."
  (with-current-buffer (mevedel-menu--data-buffer)
    (bound-and-true-p mevedel--current-request)))

(defun mevedel-menu--send-inapt-p ()
  "Return non-nil when sending should be inapt."
  (mevedel-menu--request-active-p))

(defun mevedel-menu--abort-inapt-p ()
  "Return non-nil when aborting should be inapt."
  (not (mevedel-menu--request-active-p)))


;;
;;; Commands

;;;###autoload
(defun mevedel-menu ()
  "Open the mevedel session cockpit."
  (interactive)
  (mevedel-menu-open 'top))

;;;###autoload
(defun mevedel-menu-open (area)
  "Open session cockpit AREA.
AREA is `top' for the main cockpit.  Other areas are reserved for
dedicated cockpit surfaces."
  (interactive (list 'top))
  (mevedel-menu--pair)
  (pcase area
    ('top
     (transient-setup 'mevedel-menu--top))
    (_
     (message "mevedel: %s cockpit surface is not implemented yet" area))))

(defun mevedel-menu--send ()
  "Send the current composer from the view buffer."
  (interactive)
  (mevedel-menu--call-in-view #'mevedel-view-send))

(defun mevedel-menu--abort ()
  "Abort the active request from the view buffer."
  (interactive)
  (mevedel-menu--call-in-view #'mevedel-view-abort))

(defun mevedel-menu--compact ()
  "Compact the current data buffer."
  (interactive)
  (mevedel-menu--call-in-data #'mevedel-compact))

(defun mevedel-menu--review ()
  "Run the review picker from the data buffer."
  (interactive)
  (mevedel-menu--call-in-data #'mevedel-review))

(defun mevedel-menu--verify ()
  "Run the verify picker from the data buffer."
  (interactive)
  (mevedel-menu--call-in-data #'mevedel-verify))

(defun mevedel-menu--toggle-data-view ()
  "Toggle between the view buffer and raw data buffer."
  (interactive)
  (let* ((origin (or (mevedel-menu--origin-buffer)
                     (user-error "No mevedel session cockpit here")))
         (pair (mevedel-menu--pair-for-buffer origin))
         (target (if (eq origin (cdr pair)) (car pair) (cdr pair))))
    (switch-to-buffer target)))

(defun mevedel-menu--open-mode ()
  "Open the mode cockpit surface."
  (interactive)
  (mevedel-menu-open 'mode))

(defun mevedel-menu--open-model ()
  "Open the model cockpit surface."
  (interactive)
  (mevedel-menu-open 'model))

(defun mevedel-menu--open-tools ()
  "Open the tools cockpit surface."
  (interactive)
  (mevedel-menu-open 'tools))

(defun mevedel-menu--open-skills ()
  "Open the skills cockpit surface."
  (interactive)
  (mevedel-menu-open 'skills))

(defun mevedel-menu--open-plugins ()
  "Open the plugins cockpit surface."
  (interactive)
  (mevedel-menu-open 'plugins))

(defun mevedel-menu--open-worktree ()
  "Open the worktree cockpit surface."
  (interactive)
  (mevedel-menu-open 'worktree))

(defun mevedel-menu--open-help ()
  "Open the help cockpit surface."
  (interactive)
  (mevedel-menu-open 'help))

(defun mevedel-menu--gptel-bridge-active-p ()
  "Return non-nil while a view-launched gptel bridge is restoring."
  (and (boundp 'mevedel-view--gptel-return-view-buffer)
       mevedel-view--gptel-return-view-buffer
       (buffer-live-p mevedel-view--gptel-return-view-buffer)))

(defun mevedel-menu--gptel-edit-directive-advice (orig-fn &rest args)
  "Wrap gptel directive edit callback while the bridge is active."
  (let* ((args (mevedel-view--gptel-edit-directive-args args))
         (leading (and args (not (keywordp (car args)))))
         (sym (and leading (car args)))
         (plist (if leading (cdr args) args))
         (callback (plist-get plist :callback)))
    (when callback
      (setq plist
            (plist-put
             (copy-sequence plist)
             :callback
             (lambda (message)
               (unwind-protect
                   (funcall callback message)
                 (unless (bound-and-true-p transient--prefix)
                   (mevedel-view--gptel-return-to-view)
                   (mevedel-menu--cleanup-gptel-bridge-advice)))))))
    (apply orig-fn (if leading (cons sym plist) plist))))

(defun mevedel-menu--cleanup-gptel-bridge-advice ()
  "Remove temporary gptel bridge advice after final transient exit."
  (unless (mevedel-menu--gptel-bridge-active-p)
    (remove-hook 'transient-post-exit-hook
                 #'mevedel-menu--cleanup-gptel-bridge-advice)
    (when (fboundp 'gptel--edit-directive)
      (advice-remove 'gptel--edit-directive
                     #'mevedel-menu--gptel-edit-directive-advice))))

(defun mevedel-menu--install-gptel-bridge-advice ()
  "Install temporary advice needed by the explicit gptel bridge."
  (require 'gptel-transient)
  (unless (advice-member-p #'mevedel-menu--gptel-edit-directive-advice
                           'gptel--edit-directive)
    (advice-add 'gptel--edit-directive
                :around #'mevedel-menu--gptel-edit-directive-advice))
  (add-hook 'transient-post-exit-hook
            #'mevedel-menu--cleanup-gptel-bridge-advice 90))

(defun mevedel-menu--open-gptel ()
  "Open the gptel bridge surface."
  (interactive)
  (let* ((origin (or (mevedel-menu--origin-buffer)
                     (user-error "No mevedel session cockpit here")))
         (pair (mevedel-menu--pair-for-buffer origin))
         (view-buffer (car pair))
         (data-buffer (cdr pair))
         (view-origin-p (eq origin view-buffer))
         (window (and view-origin-p
                      (or (get-buffer-window view-buffer t)
                          (selected-window)))))
    (if view-origin-p
        (let ((setup-ok nil))
          (mevedel-view--gptel-schedule-return-to-view
           view-buffer data-buffer)
          (mevedel-menu--install-gptel-bridge-advice)
          (unwind-protect
              (progn
                (when (window-live-p window)
                  (set-window-buffer window data-buffer))
                (with-selected-window
                    (if (window-live-p window) window (selected-window))
                  (with-current-buffer data-buffer
                    (call-interactively #'gptel-menu)))
                (setq setup-ok t))
            (unless setup-ok
              (mevedel-view--gptel-return-to-view)
              (mevedel-menu--cleanup-gptel-bridge-advice))))
      (with-current-buffer data-buffer
        (call-interactively #'gptel-menu)))))


;;
;;; Transient

(transient-define-prefix mevedel-menu--top ()
  "Top-level mevedel session cockpit."
  [:description mevedel-menu--header
   ["Session"
    ("RET" "Send" mevedel-menu--send
     :inapt-if mevedel-menu--send-inapt-p)
    ("a" "Abort" mevedel-menu--abort
     :inapt-if mevedel-menu--abort-inapt-p)
    ("c" "Compact" mevedel-menu--compact)
    ("r" "Review" mevedel-menu--review)
    ("v" "Verify" mevedel-menu--verify)]
   ["Configure"
    ("m" mevedel-menu--mode-description mevedel-menu--open-mode)
    ("M" mevedel-menu--model-description mevedel-menu--open-model)
    ("t" mevedel-menu--tools-description mevedel-menu--open-tools)
    ("s" mevedel-menu--skills-description mevedel-menu--open-skills)
    ("p" mevedel-menu--plugins-description mevedel-menu--open-plugins)]
   ["Inspect"
    ("d" "Toggle data view" mevedel-menu--toggle-data-view)
    ("g" "gptel menu" mevedel-menu--open-gptel)
    ("w" mevedel-menu--worktree-description mevedel-menu--open-worktree)
    ("?" "Help" mevedel-menu--open-help)]]
  (interactive)
  (mevedel-menu--pair)
  (transient-setup 'mevedel-menu--top))

(provide 'mevedel-menu)

;;; mevedel-menu.el ends here
