;;; mevedel-menu.el -- Session cockpit transient -*- lexical-binding: t -*-

;;; Commentary:

;; Transient-backed session cockpit for mevedel sessions.  The cockpit
;; resolves the live view/data buffer pair once, then routes commands to
;; the buffer that owns the relevant state.

;;; Code:

(require 'transient)

;; `gptel'
(declare-function gptel--model-name "ext:gptel" (model))
(declare-function gptel-backend-name "ext:gptel" (cl-x) t)
(defvar gptel--known-backends)
(defvar gptel-backend)
(defvar gptel-model)
(defvar gptel-tools)

;; `gptel-request'
(declare-function gptel-backend-models "ext:gptel-request" (cl-x) t)

;; `gptel-transient'
(declare-function gptel-menu "ext:gptel-transient" ())

;; `mevedel-compact'
(declare-function mevedel-compact "mevedel-compact"
                  (&optional aggressive instructions))

;; `mevedel-permissions'
(declare-function mevedel-permission-mode-transition
                  "mevedel-permissions"
                  (mode &optional prompt display-text hook-context))
(defvar mevedel-permission-mode)

;; `mevedel-plugins'
(declare-function mevedel-plugins-enabled "mevedel-plugins"
                  (&optional workspace))
(declare-function mevedel-plugins-list "mevedel-plugins"
                  (&optional workspace))
(declare-function mevedel-plugins-list-open "mevedel-plugins"
                  (&optional workspace view-buffer data-buffer origin-buffer))

;; `mevedel-review'
(declare-function mevedel-review "mevedel-review" (&optional instructions))
(declare-function mevedel-verify "mevedel-review" (&optional instructions))

;; `mevedel-skills'
(declare-function mevedel-skills--skill-enabled-p "mevedel-skills"
                  (skill))
(declare-function mevedel-skills-list-open "mevedel-skills"
                  (&optional session view-buffer data-buffer origin-buffer))

;; `mevedel-structs'
(declare-function mevedel-session-name "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-permission-mode "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-skills "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-working-directory "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-workspace "mevedel-structs" (cl-x) t)
(declare-function mevedel-workspace-root "mevedel-structs" (cl-x) t)
(defvar mevedel--current-request)
(defvar mevedel--data-buffer)
(defvar mevedel--session)
(defvar mevedel--view-buffer)

;; `mevedel-tools'
(declare-function mevedel-tools-list-open "mevedel-tools"
                  (&optional session view-buffer data-buffer origin-buffer))

;; `mevedel-view'
(declare-function mevedel-view--gptel-edit-directive-args
                  "mevedel-view" (args))
(declare-function mevedel-view--gptel-return-to-view "mevedel-view" ())
(declare-function mevedel-view--gptel-schedule-return-to-view
                  "mevedel-view" (view-buffer data-buffer))
(declare-function mevedel-view-abort "mevedel-view" ())
(declare-function mevedel-view-next-display "mevedel-view" ())
(declare-function mevedel-view-next-user-query "mevedel-view" ())
(declare-function mevedel-view-previous-display "mevedel-view" ())
(declare-function mevedel-view-previous-user-query "mevedel-view" ())
(declare-function mevedel-view-send "mevedel-view" ())
(declare-function mevedel-view-toggle-section "mevedel-view" ())
(defvar mevedel-view--gptel-return-view-buffer)

;; `mevedel-worktree'
(declare-function mevedel-worktree-list-open "mevedel-worktree" ())
(declare-function mevedel-worktree-status-open "mevedel-worktree" ())

;; `transient'
(defvar transient--original-buffer)
(defvar transient--prefix)
(defvar transient-post-exit-hook)

(defconst mevedel-menu-help-buffer-name "*mevedel help*"
  "Name of the session cockpit help buffer.")


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

(defun mevedel-menu--face (text face)
  "Return TEXT propertized with FACE."
  (propertize (format "%s" text) 'face face))

(defun mevedel-menu--value (text &optional face)
  "Return TEXT as a cockpit state value using FACE."
  (mevedel-menu--face text (or face 'transient-value)))

(defun mevedel-menu--inactive-value (text)
  "Return TEXT as an inactive cockpit state value."
  (mevedel-menu--face text 'transient-inactive-value))

(defun mevedel-menu--state-description (label value &optional face)
  "Return a padded cockpit row for LABEL and state VALUE."
  (format "%-9s %s" label (mevedel-menu--value value face)))

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
         (mode (mevedel-menu--mode-symbol session data-buffer))
         (request-state (mevedel-menu--request-state-label data-buffer)))
    (concat
     (mevedel-menu--face "mevedel:" 'transient-heading)
     " "
     (mevedel-menu--value
      (or (and session (mevedel-session-name session)) "unknown"))
     "  "
     (mevedel-menu--value (mevedel-menu--root-label workspace))
     "        "
     (mevedel-menu--value (mevedel-menu--mode-label mode))
     " · "
     (mevedel-menu--value
      request-state
      (if (string= request-state "running") 'warning 'transient-value))
     "\n")))

(defun mevedel-menu--mode-description ()
  "Return the top-level mode row description."
  (mevedel-menu--state-description
   "Mode"
   (mevedel-menu--mode-label
    (mevedel-menu--mode-symbol
     (mevedel-menu--session)
     (mevedel-menu--data-buffer)))))

(defun mevedel-menu--model-description ()
  "Return the top-level model row description."
  (let ((model (mevedel-menu--model-label)))
    (format "%-9s %s"
            "Model"
            (if (string= model "none")
                (mevedel-menu--inactive-value model)
              (mevedel-menu--value model)))))

(defun mevedel-menu--model-surface-description ()
  "Return the model surface description."
  (with-current-buffer (mevedel-menu--data-buffer)
    (let ((model (cond
                  ((and (bound-and-true-p gptel-backend)
                        (bound-and-true-p gptel-model))
                   (format "%s:%s"
                           (gptel-backend-name gptel-backend)
                           (gptel--model-name gptel-model)))
                  ((bound-and-true-p gptel-model)
                   (gptel--model-name gptel-model))
                  (t "none"))))
      (concat (mevedel-menu--face "Current model: " 'transient-heading)
              (if (string= model "none")
                  (mevedel-menu--inactive-value model)
                (mevedel-menu--value model))))))

(defun mevedel-menu--tools-description ()
  "Return the top-level tools row description."
  (mevedel-menu--state-description
   "Tools"
   (format "%d active" (mevedel-menu--active-tool-count))
   'warning))

(defun mevedel-menu--skills-description ()
  "Return the top-level skills row description."
  (mevedel-menu--state-description
   "Skills"
   (mevedel-menu--skill-count-label
    (mevedel-menu--session))
   'warning))

(defun mevedel-menu--plugins-description ()
  "Return the top-level plugins row description."
  (let* ((session (mevedel-menu--session))
         (workspace (and session (mevedel-session-workspace session))))
    (mevedel-menu--state-description
     "Plugins" (mevedel-menu--plugin-count-label workspace) 'warning)))

(defun mevedel-menu--worktree-description ()
  "Return the top-level worktree row description."
  (let ((worktree (mevedel-menu--worktree-label)))
    (format "%-9s %s"
            "Worktree"
            (if (string= worktree "not-git")
                (mevedel-menu--inactive-value worktree)
              (mevedel-menu--value worktree)))))

(defun mevedel-menu--mode-choice-description (mode detail)
  "Return the MODE surface row with DETAIL and current-state marker."
  (let* ((current (eq mode
                      (mevedel-menu--mode-symbol
                       (mevedel-menu--session)
                       (mevedel-menu--data-buffer))))
         (label (mevedel-menu--mode-label mode)))
    (format "%-7s %-7s %s"
            (if current (mevedel-menu--value label) label)
            (if current (mevedel-menu--value "current" 'warning) "")
            detail)))

(defun mevedel-menu--mode-default-description ()
  "Return the default mode row description."
  (mevedel-menu--mode-choice-description 'default "ask before write tools"))

(defun mevedel-menu--mode-accept-edits-description ()
  "Return the accept-edits mode row description."
  (mevedel-menu--mode-choice-description
   'accept-edits "auto-apply edit previews"))

(defun mevedel-menu--mode-plan-description ()
  "Return the plan mode row description."
  (mevedel-menu--mode-choice-description 'plan "read-only planning mode"))

(defun mevedel-menu--mode-trust-all-description ()
  "Return the trust-all mode row description."
  (mevedel-menu--mode-choice-description 'trust-all "auto-allow tools"))


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
;;; Mode surface

(defun mevedel-menu--set-mode (mode)
  "Set the current session permission MODE."
  (require 'mevedel-permissions)
  (mevedel-menu--call-in-data #'mevedel-permission-mode-transition mode)
  (force-mode-line-update t))


;;
;;; Model surface

(defun mevedel-menu--model-candidates ()
  "Return registered model candidates as (LABEL . PROVIDER) pairs."
  (let (candidates)
    (dolist (entry (and (boundp 'gptel--known-backends)
                        gptel--known-backends))
      (let ((backend (cdr entry)))
        (dolist (model (and (fboundp 'gptel-backend-models)
                            (gptel-backend-models backend)))
          (push
           (cons (format "%s:%s"
                         (gptel-backend-name backend)
                         (gptel--model-name model))
                 (list :backend backend :model model))
           candidates))))
    (sort candidates (lambda (a b) (string< (car a) (car b))))))

(defun mevedel-menu--set-model (provider)
  "Set the current data buffer's gptel PROVIDER."
  (let ((backend (plist-get provider :backend))
        (model (plist-get provider :model)))
    (with-current-buffer (mevedel-menu--data-buffer)
      (setq-local gptel-backend backend)
      (setq-local gptel-model model))
    (force-mode-line-update t)
    (message "mevedel: model set to %s:%s"
             (gptel-backend-name backend)
             (gptel--model-name model))))

(defun mevedel-menu--select-model ()
  "Select a registered backend/model pair for the current data buffer."
  (interactive)
  (let ((candidates (mevedel-menu--model-candidates)))
    (unless candidates
      (user-error "No registered gptel models"))
    (let ((label (completing-read "Model: " candidates nil t)))
      (mevedel-menu--set-model (cdr (assoc label candidates))))))

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
AREA is `top' for the main cockpit, or a named cockpit surface."
  (interactive (list 'top))
  (mevedel-menu--pair)
  (pcase area
    ('top
     (transient-setup 'mevedel-menu--top))
    ('mode
     (transient-setup 'mevedel-menu--mode))
    ('model
     (transient-setup 'mevedel-menu--model))
    ('skills
     (require 'mevedel-skills)
     (let* ((origin (mevedel-menu--origin-buffer))
            (view-buffer (mevedel-menu--view-buffer))
            (data-buffer (mevedel-menu--data-buffer))
            (session (mevedel-menu--session data-buffer)))
       (mevedel-menu--call-in-data
        #'mevedel-skills-list-open
        session
        view-buffer
        data-buffer
        origin)))
    ('plugins
     (require 'mevedel-plugins)
     (let* ((origin (mevedel-menu--origin-buffer))
            (view-buffer (mevedel-menu--view-buffer))
            (data-buffer (mevedel-menu--data-buffer))
            (session (mevedel-menu--session data-buffer))
            (workspace (and session (mevedel-session-workspace session))))
       (mevedel-menu--call-in-data
        #'mevedel-plugins-list-open
        workspace
        view-buffer
        data-buffer
        origin)))
    ('tools
     (require 'mevedel-tools)
     (let* ((origin (mevedel-menu--origin-buffer))
            (view-buffer (mevedel-menu--view-buffer))
            (data-buffer (mevedel-menu--data-buffer))
            (session (mevedel-menu--session data-buffer)))
       (mevedel-menu--call-in-data
        #'mevedel-tools-list-open
        session
        view-buffer
        data-buffer
        origin)))
    ('worktree
     (require 'mevedel-worktree)
     (mevedel-menu--call-in-data #'mevedel-worktree-status-open))
    ('help
     (mevedel-menu-help-open))
    ('gptel
     (mevedel-menu--open-gptel))
    (_
     (user-error "Unknown cockpit area: %s" area))))

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

(defun mevedel-menu-help--text ()
  "Return command-discovery text for the session cockpit."
  (string-join
   '("mevedel help"
     ""
     "Session cockpit keys"
     "RET Send              a Abort"
     "c Compact             r Review              v Verify"
     "n Next display        b Previous display"
     "N Next query          B Previous query       e Toggle section"
     "m Mode                M Model"
     "t Tools               s Skills              p Plugins"
     "d Toggle data view    g gptel menu          w Worktree"
     "? Help"
     ""
     "Slash commands that open UI"
     "/plugin, /plugin list       Plugins"
     "/skills, /skills list       Skills"
     "/mode                       Mode"
     "/model                      Model"
     "/tools, /tools list         Tools"
     "/worktree, /worktree status Worktree"
     "/help                       Help"
     ""
     "Direct slash commands"
     "/plugin enable NAME, disable NAME, reload, update NAME"
     "/plugin install TARGET, remove NAME, uninstall NAME, hooks ..."
     "/skills enable NAME, disable NAME, help NAME"
     "/mode MODE, /model MODEL"
     "/worktree create [NAME] [--for \"purpose\"] [--clean]"
     "/compact, /review, /verify, /plan ..., /auto, /clear, /init ..., /tokens"
     ""
     "Modes"
     "default / ask       Ask before write tools."
     "accept-edits        Auto-apply edit previews."
     "plan                Read-only planning mode."
     "trust-all / auto    Auto-allow tools."
     ""
     "View and data buffers"
     "The view buffer owns the composer, compact transcript, and status strip."
     "The data buffer owns raw gptel state, tools, model, and transcript data."
     "The cockpit resolves the view/data pair once and routes actions to the owning buffer."
     "The raw data buffer keeps gptel header behavior; g opens gptel-menu there.")
   "\n"))

(defun mevedel-menu-help-open ()
  "Open the session cockpit help surface."
  (with-help-window mevedel-menu-help-buffer-name
    (princ (mevedel-menu-help--text))
    (princ "\n"))
  (get-buffer mevedel-menu-help-buffer-name))

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
    :pad-keys t
    ("RET" "Send" mevedel-menu--send
     :inapt-if mevedel-menu--send-inapt-p)
    ("a" "Abort" mevedel-menu--abort
     :inapt-if mevedel-menu--abort-inapt-p)
    ("c" "Compact" mevedel-menu--compact)
    ("r" "Review" mevedel-menu--review)
    ("v" "Verify" mevedel-menu--verify)]
   ["Navigate"
    :pad-keys t
    ("n" "Next display"
     (lambda () (interactive)
       (mevedel-menu--call-in-view #'mevedel-view-next-display))
     :transient t)
    ("N" "Previous display"
     (lambda () (interactive)
       (mevedel-menu--call-in-view #'mevedel-view-previous-display))
     :transient t)
    ("C-n" "Next query"
     (lambda () (interactive)
       (mevedel-menu--call-in-view #'mevedel-view-next-user-query))
     :transient t)
    ("C-p" "Previous query"
     (lambda () (interactive)
       (mevedel-menu--call-in-view #'mevedel-view-previous-user-query))
     :transient t)
    ("e" "Toggle section"
     (lambda () (interactive)
       (mevedel-menu--call-in-view #'mevedel-view-toggle-section))
     :transient t)]
   ["Configure"
    :pad-keys t
    ("m" mevedel-menu--open-mode
     :description mevedel-menu--mode-description)
    ("M" mevedel-menu--open-model
     :description mevedel-menu--model-description)
    ("t" mevedel-menu--open-tools
     :description mevedel-menu--tools-description)
    ("s" mevedel-menu--open-skills
     :description mevedel-menu--skills-description)
    ("p" mevedel-menu--open-plugins
     :description mevedel-menu--plugins-description)]
   ["Inspect"
    :pad-keys t
    ("d" "Toggle data view" mevedel-menu--toggle-data-view)
    ("g" "gptel menu" mevedel-menu--open-gptel)
    ("w" mevedel-menu--open-worktree
     :description mevedel-menu--worktree-description)
    ("?" "Help" mevedel-menu--open-help)]]
  (interactive)
  (mevedel-menu--pair)
  (transient-setup 'mevedel-menu--top))

(transient-define-prefix mevedel-menu--mode ()
  "Permission mode cockpit surface."
  [:description mevedel-menu--header
   ["Mode"
    :pad-keys t
    ("d" mevedel-menu--mode-default-description
     (lambda () (interactive) (mevedel-menu--set-mode 'default)))
    ("e" mevedel-menu--mode-accept-edits-description
     (lambda () (interactive) (mevedel-menu--set-mode 'accept-edits)))
    ("p" mevedel-menu--mode-plan-description
     (lambda () (interactive) (mevedel-menu--set-mode 'plan)))
    ("a" mevedel-menu--mode-trust-all-description
     (lambda () (interactive) (mevedel-menu--set-mode 'trust-all)))]
   ["Navigation"
    :pad-keys t
    ("b" "Back" mevedel-menu)]]
  (interactive)
  (mevedel-menu--pair)
  (transient-setup 'mevedel-menu--mode))

(transient-define-prefix mevedel-menu--model ()
  "Model cockpit surface."
  [:description mevedel-menu--model-surface-description
   ["Model"
    :pad-keys t
    ("RET" "Select model" mevedel-menu--select-model)
    ("g" "gptel menu" mevedel-menu--open-gptel)]
   ["Navigation"
    :pad-keys t
    ("b" "Back" mevedel-menu)]]
  (interactive)
  (mevedel-menu--pair)
  (transient-setup 'mevedel-menu--model))

(provide 'mevedel-menu)

;;; mevedel-menu.el ends here
