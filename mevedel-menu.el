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

;; `gptel-request'
(declare-function gptel-backend-models "ext:gptel-request" (cl-x) t)

;; `mevedel-cockpit'
(declare-function mevedel-cockpit-call-in-data
                  "mevedel-cockpit" (context function &rest args))
(declare-function mevedel-cockpit-call-in-view
                  "mevedel-cockpit" (context function &rest args))
(declare-function mevedel-cockpit-context-data-buffer
                  "mevedel-cockpit" (&optional context))
(declare-function mevedel-cockpit-context-origin-buffer
                  "mevedel-cockpit" (&optional context))
(declare-function mevedel-cockpit-context-session
                  "mevedel-cockpit" (&optional context))
(declare-function mevedel-cockpit-context-view-buffer
                  "mevedel-cockpit" (&optional context))
(declare-function mevedel-cockpit-context-workspace
                  "mevedel-cockpit" (&optional context))
(declare-function mevedel-cockpit-current-context
                  "mevedel-cockpit" ())

;; `mevedel-compact'
(declare-function mevedel-compact "mevedel-compact"
                  (&optional aggressive instructions))

;; `mevedel-gptel-bridge'
(declare-function mevedel-gptel-bridge-open
                  "mevedel-gptel-bridge" (&optional context))

;; `mevedel-models'
(declare-function mevedel-model-current-label "mevedel-models"
                  (&optional buffer))
(declare-function mevedel-model-current-provider-label "mevedel-models"
                  (&optional buffer))

;; `mevedel-permissions'
(declare-function mevedel-permission-mode-effective "mevedel-permissions"
                  (&optional session data-buffer surface-buffer))
(declare-function mevedel-permission-mode-label "mevedel-permissions"
                  (&optional mode))
(declare-function mevedel-permission-mode-transition
                  "mevedel-permissions"
                  (mode &optional prompt display-text hook-context))
(defvar mevedel-permission-mode)

;; `mevedel-plugins'
(declare-function mevedel-plugins-count-label "mevedel-plugins"
                  (&optional workspace))
(declare-function mevedel-plugins-list-open "mevedel-plugins"
                  (&optional context))

;; `mevedel-review'
(declare-function mevedel-review "mevedel-review" (&optional instructions))
(declare-function mevedel-verify "mevedel-review" (&optional instructions))

;; `mevedel-skills'
(declare-function mevedel-skills-count-label "mevedel-skills" (session))
(declare-function mevedel-skills-list-open "mevedel-skills"
                  (&optional context))

;; `mevedel-structs'
(declare-function mevedel-request-active-p "mevedel-structs"
                  (&optional buffer))
(declare-function mevedel-request-state-label "mevedel-structs"
                  (&optional buffer))
(declare-function mevedel-session-name "mevedel-structs" (cl-x) t)
(declare-function mevedel-workspace-root "mevedel-structs" (cl-x) t)

;; `mevedel-tools'
(declare-function mevedel-tools-active-count "mevedel-tools"
                  (&optional buffer))

;; `mevedel-tools-list'
(declare-function mevedel-tools-list-open "mevedel-tools-list"
                  (&optional context))

;; `mevedel-view'
(declare-function mevedel-view-abort "mevedel-view" ())
(declare-function mevedel-view-next-display "mevedel-view" ())
(declare-function mevedel-view-next-user-query "mevedel-view" ())
(declare-function mevedel-view-previous-display "mevedel-view" ())
(declare-function mevedel-view-previous-user-query "mevedel-view" ())
(declare-function mevedel-view-send "mevedel-view" ())
(declare-function mevedel-view-toggle-section "mevedel-view" ())

;; `mevedel-worktree'
(declare-function mevedel-worktree-status-summary "mevedel-worktree"
                  (&optional context))
(declare-function mevedel-worktree-status-open "mevedel-worktree" ())

(defconst mevedel-menu-help-buffer-name "*mevedel help*"
  "Name of the session cockpit help buffer.")


;;
;;; Context resolution

(defun mevedel-menu--context ()
  "Return the current live cockpit context."
  (require 'mevedel-cockpit)
  (let ((context (mevedel-cockpit-current-context)))
    (unless (and (mevedel-cockpit-context-view-buffer context)
                 (mevedel-cockpit-context-data-buffer context))
      (user-error "No mevedel session cockpit here"))
    context))


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

(defun mevedel-menu--mode-symbol (&optional session data-buffer surface-buffer)
  "Return the effective permission mode for the cockpit context."
  (require 'mevedel-permissions)
  (let ((context (unless (and session data-buffer surface-buffer)
                   (condition-case nil
                       (mevedel-menu--context)
                     (user-error nil)))))
    (setq session
          (or session
              (and context (mevedel-cockpit-context-session context))))
    (setq data-buffer
          (or data-buffer
              (and context (mevedel-cockpit-context-data-buffer context))))
    (setq surface-buffer
          (or surface-buffer
              (and context (mevedel-cockpit-context-view-buffer context))
              data-buffer))
    (mevedel-permission-mode-effective session data-buffer surface-buffer)))

(defun mevedel-menu--mode-label (&optional mode)
  "Return the cockpit label for permission MODE."
  (require 'mevedel-permissions)
  (mevedel-permission-mode-label mode))

(defun mevedel-menu--model-label ()
  "Return the current model label."
  (require 'mevedel-models)
  (mevedel-model-current-label
   (mevedel-cockpit-context-data-buffer (mevedel-menu--context))))

(defun mevedel-menu--active-tool-count ()
  "Return the number of active gptel tools in the data buffer."
  (require 'mevedel-tools)
  (mevedel-tools-active-count
   (mevedel-cockpit-context-data-buffer (mevedel-menu--context))))

(defun mevedel-menu--root-label (workspace)
  "Return a compact root label for WORKSPACE."
  (if (and workspace (fboundp 'mevedel-workspace-root))
      (abbreviate-file-name (mevedel-workspace-root workspace))
    "unknown"))

(defun mevedel-menu--worktree-label ()
  "Return the current branch or detached HEAD label."
  (require 'mevedel-worktree)
  (plist-get (mevedel-worktree-status-summary (mevedel-menu--context))
             :label))

(defun mevedel-menu--header ()
  "Return the cockpit header string."
  (let* ((context (mevedel-menu--context))
         (data-buffer (mevedel-cockpit-context-data-buffer context))
         (session (mevedel-cockpit-context-session context))
         (workspace (mevedel-cockpit-context-workspace context))
         (mode (mevedel-menu--mode-symbol
                session data-buffer
                (mevedel-cockpit-context-view-buffer context)))
         (request-state (mevedel-request-state-label data-buffer)))
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
  (let ((context (mevedel-menu--context)))
    (mevedel-menu--state-description
     "Mode"
     (mevedel-menu--mode-label
      (mevedel-menu--mode-symbol
       (mevedel-cockpit-context-session context)
       (mevedel-cockpit-context-data-buffer context)
       (mevedel-cockpit-context-view-buffer context))))))

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
  (require 'mevedel-models)
  (let ((model (mevedel-model-current-provider-label
                (mevedel-cockpit-context-data-buffer
                 (mevedel-menu--context)))))
    (concat (mevedel-menu--face "Current model: " 'transient-heading)
            (if (string= model "none")
                (mevedel-menu--inactive-value model)
              (mevedel-menu--value model)))))

(defun mevedel-menu--tools-description ()
  "Return the top-level tools row description."
  (mevedel-menu--state-description
   "Tools"
   (format "%d active" (mevedel-menu--active-tool-count))
   'warning))

(defun mevedel-menu--skills-description ()
  "Return the top-level skills row description."
  (require 'mevedel-skills)
  (let ((context (mevedel-menu--context)))
    (mevedel-menu--state-description
     "Skills"
     (mevedel-skills-count-label (mevedel-cockpit-context-session context))
     'warning)))

(defun mevedel-menu--plugins-description ()
  "Return the top-level plugins row description."
  (require 'mevedel-plugins)
  (let ((workspace (mevedel-cockpit-context-workspace
                    (mevedel-menu--context))))
    (mevedel-menu--state-description
     "Plugins" (mevedel-plugins-count-label workspace) 'warning)))

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
  (let* ((context (mevedel-menu--context))
         (current (eq mode
                      (mevedel-menu--mode-symbol
                       (mevedel-cockpit-context-session context)
                       (mevedel-cockpit-context-data-buffer context)
                       (mevedel-cockpit-context-view-buffer context))))
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
  (mevedel-request-active-p
   (mevedel-cockpit-context-data-buffer (mevedel-menu--context))))

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
  (mevedel-cockpit-call-in-data
   (mevedel-menu--context) #'mevedel-permission-mode-transition mode)
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
    (with-current-buffer
        (mevedel-cockpit-context-data-buffer (mevedel-menu--context))
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
  (let ((context (mevedel-menu--context)))
    (pcase area
      ('top
       (transient-setup 'mevedel-menu--top))
      ('mode
       (transient-setup 'mevedel-menu--mode))
      ('model
       (transient-setup 'mevedel-menu--model))
      ('skills
       (require 'mevedel-skills)
       (mevedel-cockpit-call-in-data
        context #'mevedel-skills-list-open context))
      ('plugins
       (require 'mevedel-plugins)
       (mevedel-cockpit-call-in-data
        context #'mevedel-plugins-list-open context))
      ('tools
       (require 'mevedel-tools-list)
       (mevedel-cockpit-call-in-data
        context #'mevedel-tools-list-open context))
      ('worktree
       (require 'mevedel-worktree)
       (mevedel-cockpit-call-in-data
        context #'mevedel-worktree-status-open))
      ('help
       (mevedel-menu-help-open))
      ('gptel
       (mevedel-menu--open-gptel))
      (_
       (user-error "Unknown cockpit area: %s" area)))))

(defun mevedel-menu--send ()
  "Send the current composer from the view buffer."
  (interactive)
  (mevedel-cockpit-call-in-view (mevedel-menu--context) #'mevedel-view-send))

(defun mevedel-menu--abort ()
  "Abort the active request from the view buffer."
  (interactive)
  (mevedel-cockpit-call-in-view (mevedel-menu--context) #'mevedel-view-abort))

(defun mevedel-menu--compact ()
  "Compact the current data buffer."
  (interactive)
  (mevedel-cockpit-call-in-data (mevedel-menu--context) #'mevedel-compact))

(defun mevedel-menu--review ()
  "Run the review picker from the data buffer."
  (interactive)
  (mevedel-cockpit-call-in-data (mevedel-menu--context) #'mevedel-review))

(defun mevedel-menu--verify ()
  "Run the verify picker from the data buffer."
  (interactive)
  (mevedel-cockpit-call-in-data (mevedel-menu--context) #'mevedel-verify))

(defun mevedel-menu--toggle-data-view ()
  "Toggle between the view buffer and raw data buffer."
  (interactive)
  (let* ((context (mevedel-menu--context))
         (origin (or (mevedel-cockpit-context-origin-buffer context)
                     (current-buffer)))
         (view-buffer (mevedel-cockpit-context-view-buffer context))
         (data-buffer (mevedel-cockpit-context-data-buffer context))
         (target (if (eq origin data-buffer) view-buffer data-buffer)))
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
     "Session cockpit"
     "The transient menu is the live key reference for session commands."
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
     "The raw data buffer keeps gptel header behavior for the gptel menu.")
   "\n"))

(defun mevedel-menu-help-open ()
  "Open the session cockpit help surface."
    (let ((help-window-select t))
      (with-help-window mevedel-menu-help-buffer-name
        (princ (mevedel-menu-help--text))
        (princ "\n"))
      (get-buffer mevedel-menu-help-buffer-name)))

(defun mevedel-menu--open-gptel ()
  "Open the gptel bridge surface."
  (interactive)
  (require 'mevedel-gptel-bridge)
  (mevedel-gptel-bridge-open (mevedel-menu--context)))


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
       (mevedel-cockpit-call-in-view
        (mevedel-menu--context) #'mevedel-view-next-display))
     :transient t)
    ("N" "Previous display"
     (lambda () (interactive)
       (mevedel-cockpit-call-in-view
        (mevedel-menu--context) #'mevedel-view-previous-display))
     :transient t)
    ("C-n" "Next query"
     (lambda () (interactive)
       (mevedel-cockpit-call-in-view
        (mevedel-menu--context) #'mevedel-view-next-user-query))
     :transient t)
    ("C-p" "Previous query"
     (lambda () (interactive)
       (mevedel-cockpit-call-in-view
        (mevedel-menu--context) #'mevedel-view-previous-user-query))
     :transient t)
    ("e" "Toggle section"
     (lambda () (interactive)
       (mevedel-cockpit-call-in-view
        (mevedel-menu--context) #'mevedel-view-toggle-section))
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
  (mevedel-menu--context)
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
  (mevedel-menu--context)
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
  (mevedel-menu--context)
  (transient-setup 'mevedel-menu--model))

(provide 'mevedel-menu)

;;; mevedel-menu.el ends here
