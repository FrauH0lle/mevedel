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

;; `mevedel-execution'
(declare-function mevedel-execution-count-user "mevedel-execution" (session))

;; `mevedel-executions-list'
(declare-function mevedel-executions-list-open
                  "mevedel-executions-list" (&optional context))

;; `mevedel-goal'
(declare-function mevedel-goal-clear "mevedel-goal" ())
(declare-function mevedel-goal-cycle-record "mevedel-goal" (goal))
(declare-function mevedel-goal-edit "mevedel-goal" (objective))
(declare-function mevedel-goal-latest-provider "mevedel-goal" (goal workload))
(declare-function mevedel-goal-owned-by-session-p "mevedel-goal" (goal session))
(declare-function mevedel-goal-pause "mevedel-goal" ())
(declare-function mevedel-goal-resume "mevedel-goal" (&optional input))
(declare-function mevedel-goal-set-token-budget "mevedel-goal" (budget))
(declare-function mevedel-goal-start "mevedel-goal"
                  (objective &optional display-text approval-policy submission))

;; `mevedel-gptel-bridge'
(declare-function mevedel-gptel-bridge-open
                  "mevedel-gptel-bridge" (&optional context))

;; `mevedel-models'
(declare-function mevedel-model-current-label "mevedel-models"
                  (&optional buffer))
(declare-function mevedel-model-current-provider-label "mevedel-models"
                  (&optional buffer))
(declare-function mevedel-model-resolve-workload "mevedel-models"
                  (workload &optional explicit-selector explicit-effort))
(defvar mevedel-model-tiers)
(defvar mevedel-model-workloads)

;; `mevedel-permissions'
(declare-function mevedel-permission-mode-effective "mevedel-permissions"
                  (&optional session data-buffer surface-buffer))
(declare-function mevedel-permission-mode-label "mevedel-permissions"
                  (&optional mode))
(declare-function mevedel-permission-mode-transition
                  "mevedel-permissions"
                  (mode &optional prompt display-text hook-context))
(defvar mevedel-permission-mode)

;; `mevedel-plan-mode'
(declare-function mevedel-plan-mode-enter
                  "mevedel-plan-mode" (&optional session))

;; `mevedel-plugins'
(declare-function mevedel-plugins-count-label "mevedel-plugins"
                  (&optional workspace))
(declare-function mevedel-plugins-list-open "mevedel-plugins"
                  (&optional context))

;; `mevedel-presets'
(declare-function mevedel-preset-apply "mevedel-presets" (name &optional buffer))
(defvar mevedel-preset--registry)

;; `mevedel-review'
(declare-function mevedel-review "mevedel-review" (&optional instructions))
(declare-function mevedel-verify "mevedel-review" (&optional instructions))

;; `mevedel-skills-ui'
(declare-function mevedel-skills-count-label "mevedel-skills-ui" (session))
(declare-function mevedel-skills-list-open "mevedel-skills-ui"
                  (&optional context))

;; `mevedel-structs'
(declare-function mevedel-goal-approval-policy "mevedel-structs" (cl-x) t)
(declare-function mevedel-goal-checkpoint "mevedel-structs" (cl-x) t)
(declare-function mevedel-goal-current-plan "mevedel-structs" (cl-x) t)
(declare-function mevedel-goal-cycle "mevedel-structs" (cl-x) t)
(declare-function mevedel-goal-execution-home "mevedel-structs" (cl-x) t)
(declare-function mevedel-goal-implementation-context "mevedel-structs" (cl-x) t)
(declare-function mevedel-goal-objective "mevedel-structs" (cl-x) t)
(declare-function mevedel-goal-phase "mevedel-structs" (cl-x) t)
(declare-function mevedel-goal-reason "mevedel-structs" (cl-x) t)
(declare-function mevedel-goal-review-findings "mevedel-structs" (cl-x) t)
(declare-function mevedel-goal-review-summary "mevedel-structs" (cl-x) t)
(declare-function mevedel-goal-status "mevedel-structs" (cl-x) t)
(declare-function mevedel-goal-token-budget "mevedel-structs" (cl-x) t)
(declare-function mevedel-goal-token-usage "mevedel-structs" (cl-x) t)
(declare-function mevedel-request-active-p "mevedel-structs"
                  (&optional buffer))
(declare-function mevedel-request-state-label "mevedel-structs"
                  (&optional buffer))
(declare-function mevedel-session-goal "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-name "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-plan-mode "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-preset-name "mevedel-structs" (cl-x) t)
(declare-function mevedel-workspace-root "mevedel-structs" (cl-x) t)

;; `mevedel-tools'
(declare-function mevedel-tools-active-count "mevedel-tools"
                  (&optional buffer))

;; `mevedel-tools-list'
(declare-function mevedel-tools-list-open "mevedel-tools-list"
                  (&optional context))

;; `mevedel-view-composer'
(declare-function mevedel-view-abort "mevedel-view-composer" ())
(declare-function mevedel-view-send "mevedel-view-composer" ())

;; `mevedel-view'
(declare-function mevedel-view-next-display "mevedel-view" ())
(declare-function mevedel-view-next-user-query "mevedel-view" ())
(declare-function mevedel-view-previous-display "mevedel-view" ())
(declare-function mevedel-view-previous-user-query "mevedel-view" ())
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
     (let* ((session (mevedel-cockpit-context-session context))
            (permission
             (mevedel-menu--mode-label
              (mevedel-menu--mode-symbol
               session
               (mevedel-cockpit-context-data-buffer context)
               (mevedel-cockpit-context-view-buffer context)))))
       (if (mevedel-session-plan-mode session)
           (format "Plan/%s" permission)
         permission)))))

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

(defun mevedel-menu--executions-description ()
  "Return the top-level executions row description."
  (require 'mevedel-execution)
  (let* ((session (mevedel-cockpit-context-session
                   (mevedel-menu--context)))
         (count (mevedel-execution-count-user session)))
    (mevedel-menu--state-description
     "Processes"
     (format "%d live" count)
     (if (> count 0) 'warning 'transient-inactive-value))))

(defun mevedel-menu--skills-description ()
  "Return the top-level skills row description."
  (require 'mevedel-skills-ui)
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

(defun mevedel-menu--current-goal ()
  "Return the current session Goal, or nil."
  (mevedel-session-goal
   (mevedel-cockpit-context-session (mevedel-menu--context))))

(defun mevedel-menu--owned-goal ()
  "Return the current Goal when this session owns it, or nil."
  (when-let* ((goal (mevedel-menu--current-goal))
              (session (mevedel-cockpit-context-session
                        (mevedel-menu--context)))
              ((mevedel-goal-owned-by-session-p goal session)))
    goal))

(defun mevedel-menu--goal-active-p ()
  "Return non-nil when the current Goal can be paused."
  (when-let* ((goal (mevedel-menu--owned-goal)))
    (eq (mevedel-goal-status goal) 'active)))

(defun mevedel-menu--goal-resumable-p ()
  "Return non-nil when the current Goal can be resumed."
  (when-let* ((goal (mevedel-menu--owned-goal)))
    (memq (mevedel-goal-status goal) '(paused blocked))))

(defun mevedel-menu--goal-start-inapt-p ()
  "Return non-nil when an unfinished Goal already exists."
  (when-let* ((goal (mevedel-menu--current-goal)))
    (not (eq (mevedel-goal-status goal) 'complete))))

(defun mevedel-menu--goal-editable-p ()
  "Return non-nil when Goal settings may be changed here."
  (when-let* ((goal (mevedel-menu--owned-goal)))
    (not (eq (mevedel-goal-status goal) 'complete))))

(defun mevedel-menu--goal-clearable-p ()
  "Return non-nil when Goal state may be cleared now."
  (and (mevedel-menu--current-goal)
       (not (mevedel-menu--request-active-p))))

(defun mevedel-menu--goal-description ()
  "Return the complete Goal cockpit description."
  (if-let* ((goal (mevedel-menu--current-goal)))
      (let* ((context (mevedel-menu--context))
             (home (mevedel-goal-execution-home goal))
             (plan (mevedel-goal-current-plan goal))
             (review (or (mevedel-goal-review-summary goal)
                         (mevedel-goal-review-findings goal)))
             (cycle-record (mevedel-goal-cycle-record goal))
             (guardian (car (last (plist-get cycle-record :guardian-audits))))
             (checkpoint (mevedel-goal-checkpoint goal))
             (budget (mevedel-goal-token-budget goal)))
        (string-join
         (list
          (format "Goal: %s" (mevedel-goal-objective goal))
          (format "Status / phase: %s / %s (cycle %d)"
                  (mevedel-goal-status goal) (mevedel-goal-phase goal)
                  (mevedel-goal-cycle goal))
          (format "Approval: %s · Tool permissions: %s"
                  (mevedel-goal-approval-policy goal)
                  (mevedel-menu--mode-symbol
                   (mevedel-cockpit-context-session context)
                   (mevedel-cockpit-context-data-buffer context)
                   (mevedel-cockpit-context-view-buffer context)))
          (format "Budget: %d%s" (or (mevedel-goal-token-usage goal) 0)
                  (if budget (format "/%d tokens" budget)
                    " tokens / unlimited"))
          (format "Execution: %s · %s · %s"
                  (or (plist-get home :kind) 'unknown)
                  (or (plist-get home :directory) "unavailable")
                  (or (mevedel-goal-implementation-context goal) 'full))
          (format "Plan: %s"
                  (or (plist-get plan :absolute-path)
                      (plist-get plan :path) "none"))
          (format "Latest review: %s"
                  (cond ((and review (listp review))
                         (format "%s — %s" (plist-get review :verdict)
                                 (plist-get review :summary)))
                        (review review) (t "none")))
          (format "Guardian: %s"
                  (if guardian
                      (format "%s — %s" (plist-get guardian :verdict)
                              (plist-get guardian :reason))
                    "none"))
          (format "Recovery: %s%s"
                  (or (plist-get checkpoint :dispatch-state) "none")
                  (if-let* ((reason (mevedel-goal-reason goal)))
                      (format " — %s" reason) "")))
         "\n"))
    "No active Goal. Start one here or with /goal OBJECTIVE."))

(defun mevedel-menu--preset-description ()
  "Return selected preset, tiers, workloads, and actual request policies."
  (let* ((context (mevedel-menu--context))
         (session (mevedel-cockpit-context-session context)))
    (with-current-buffer (mevedel-cockpit-context-data-buffer context)
      (let* ((goal (mevedel-session-goal session))
             (workloads (delete-dups
                         (append (mapcar #'car mevedel-model-workloads)
                                 '(planning goal-guardian implementation review)))))
        (string-join
         (append
          (list (format "Preset: %s"
                        (or (mevedel-session-preset-name session) "none"))
                (format "Tiers: %S" mevedel-model-tiers)
                "Workloads:")
          (mapcar
           (lambda (workload)
             (format "  %-18s %s%s"
                     workload
                     (condition-case err
                         (let ((policy (mevedel-model-resolve-workload workload)))
                           (format "%s:%s · effort %s"
                                   (gptel-backend-name
                                    (plist-get policy :backend))
                                   (gptel--model-name (plist-get policy :model))
                                   (or (plist-get policy :effort) "default")))
                       (error (format "ERROR: %s — fix this preset before dispatch"
                                      (error-message-string err))))
                     (if-let* ((used (and goal
                                          (mevedel-goal-latest-provider
                                           goal workload))))
                         (format " · actual %s / effort %s"
                                 (plist-get used :provider)
                                 (or (plist-get used :effort) "default"))
                       "")))
           workloads))
         "\n")))))

(defun mevedel-menu--mode-choice-description (mode detail)
  "Return the MODE surface row with DETAIL and current-state marker."
  (let* ((context (mevedel-menu--context))
         (session (mevedel-cockpit-context-session context))
         (current (and (not (mevedel-session-plan-mode session))
                       (eq mode
                           (mevedel-menu--mode-symbol
                            session
                            (mevedel-cockpit-context-data-buffer context)
                            (mevedel-cockpit-context-view-buffer context)))))
         (label (mevedel-menu--mode-label mode)))
    (format "%-7s %-7s %s"
            (if current (mevedel-menu--value label) label)
            (if current (mevedel-menu--value "current" 'warning) "")
            detail)))

(defun mevedel-menu--mode-ask-description ()
  "Return the ask mode row description."
  (mevedel-menu--mode-choice-description
   'ask "prompt for edits and uncertain execution"))

(defun mevedel-menu--mode-auto-description ()
  "Return the auto mode row description."
  (mevedel-menu--mode-choice-description
   'auto "auto-apply edit previews"))

(defun mevedel-menu--mode-full-auto-description ()
  "Return the full-auto mode row description."
  (mevedel-menu--mode-choice-description 'full-auto "auto-allow tools"))

(defun mevedel-menu--mode-plan-description ()
  "Return the Plan mode row description."
  (let* ((session (mevedel-cockpit-context-session
                   (mevedel-menu--context)))
         (current (mevedel-session-plan-mode session)))
    (format "%-7s %-7s %s"
            (if current (mevedel-menu--value "Plan") "Plan")
            (if current (mevedel-menu--value "current" 'warning) "")
            "inspect and discuss without direct edits")))


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

(defun mevedel-menu--enter-plan ()
  "Enter Plan mode for the current cockpit session."
  (interactive)
  (require 'mevedel-plan-mode)
  (mevedel-cockpit-call-in-data
   (mevedel-menu--context) #'mevedel-plan-mode-enter)
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
      ('goal
       (transient-setup 'mevedel-menu--goal))
      ('preset
       (transient-setup 'mevedel-menu--preset))
      ('skills
       (require 'mevedel-skills-ui)
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
      ('executions
       (require 'mevedel-executions-list)
       (mevedel-cockpit-call-in-data
        context #'mevedel-executions-list-open context))
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

(defun mevedel-menu--open-goal ()
  "Open the Goal cockpit surface."
  (interactive)
  (mevedel-menu-open 'goal))

(defun mevedel-menu--open-preset ()
  "Open the Preset cockpit surface."
  (interactive)
  (mevedel-menu-open 'preset))

(defun mevedel-menu--goal-call (function &rest args)
  "Call Goal FUNCTION with ARGS in the owning data buffer."
  (apply #'mevedel-cockpit-call-in-data
         (mevedel-menu--context) function args)
  (force-mode-line-update t))

(defun mevedel-menu--goal-start ()
  "Prompt for and start a supervised Goal."
  (interactive)
  (mevedel-menu--goal-call
   #'mevedel-goal-start (read-string "Goal objective: ")))

(defun mevedel-menu--goal-start-auto ()
  "Prompt for and start an automatic Goal."
  (interactive)
  (let ((objective (read-string "Automatic Goal objective: ")))
    (mevedel-menu--goal-call
     #'mevedel-goal-start objective objective 'automatic)))

(defun mevedel-menu--goal-edit ()
  "Edit the current Goal objective."
  (interactive)
  (mevedel-menu--goal-call
   #'mevedel-goal-edit
   (read-string "Goal objective: "
                (and (mevedel-menu--current-goal)
                     (mevedel-goal-objective
                      (mevedel-menu--current-goal))))))

(defun mevedel-menu--goal-set-budget ()
  "Set the current Goal token budget."
  (interactive)
  (let ((value (read-string "Goal token budget (blank = unlimited): ")))
    (mevedel-menu--goal-call
     #'mevedel-goal-set-token-budget
     (unless (string-blank-p value) (string-to-number value)))))

(defun mevedel-menu--goal-approval-toggle-description ()
  "Return the Goal approval toggle description."
  (if-let* ((goal (mevedel-menu--current-goal))
            (policy (mevedel-goal-approval-policy goal)))
      (format "Approval: %s → %s" policy
              (if (eq policy 'automatic) 'supervised 'automatic))
    "Approval policy"))

(defun mevedel-menu--goal-toggle-approval-policy ()
  "Toggle the current Goal's approval policy."
  (interactive)
  (let ((policy (mevedel-goal-approval-policy
                 (or (mevedel-menu--current-goal)
                     (user-error "No current Goal")))))
    (mevedel-menu--goal-call
     #'mevedel-goal-set-approval-policy
     (if (eq policy 'automatic) 'supervised 'automatic))))

(defun mevedel-menu--select-preset ()
  "Select and apply a preset to the current session only."
  (interactive)
  (let* ((names (mapcar (lambda (entry) (symbol-name (car entry)))
                        mevedel-preset--registry))
         (name (intern (completing-read "Preset: " names nil t))))
    (mevedel-cockpit-call-in-data
     (mevedel-menu--context) #'mevedel-preset-apply name)
    (force-mode-line-update t)
    (message "mevedel: preset set to %s for this session" name)))

(defun mevedel-menu--open-tools ()
  "Open the tools cockpit surface."
  (interactive)
  (mevedel-menu-open 'tools))

(defun mevedel-menu--open-executions ()
  "Open the live execution cockpit surface."
  (interactive)
  (mevedel-menu-open 'executions))

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
     "Cockpit G / P               Goal / Preset model team"
     "/tools, /tools list         Tools"
     "/ps                         Live executions"
     "/stop [EXECUTION_ID]        Stop one execution or open /ps"
     "/worktree, /worktree status Worktree"
     "/help                       Help"
     ""
     "Direct slash commands"
     "/plugin enable NAME, disable NAME, reload, update NAME"
     "/plugin install TARGET, remove NAME, uninstall NAME, hooks ..."
     "/skills enable NAME, disable NAME, help NAME"
     "/mode MODE, /model MODEL"
     "/worktree create [NAME] [--for \"purpose\"] [--clean]"
     "/goal OBJECTIVE, /goal approval [supervised|automatic]"
     "/compact, /review, /verify, /auto, /clear, /init ..., /tokens"
     ""
     "Modes"
     "ask       Prompt for edits and uncertain execution."
     "auto      Auto-apply native edits; check Bash and Eval."
     "full-auto Skip heuristic Bash and Eval prompts."
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
    ("P" mevedel-menu--open-preset
     :description (lambda () (format "%-9s %s" "Preset"
                                     (or (mevedel-session-preset-name
                                          (mevedel-cockpit-context-session
                                           (mevedel-menu--context)))
                                         "none"))))
    ("t" mevedel-menu--open-tools
     :description mevedel-menu--tools-description)
    ("x" mevedel-menu--open-executions
     :description mevedel-menu--executions-description)
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
    ("G" mevedel-menu--open-goal
     :description (lambda () (format "%-9s %s" "Goal"
                                     (if-let* ((goal (mevedel-menu--current-goal)))
                                         (format "%s/%s cycle %d"
                                                 (mevedel-goal-status goal)
                                                 (mevedel-goal-phase goal)
                                                 (mevedel-goal-cycle goal))
                                       "none"))))
    ("?" "Help" mevedel-menu--open-help)]]
  (interactive)
  (mevedel-menu--context)
  (transient-setup 'mevedel-menu--top))

(transient-define-prefix mevedel-menu--mode ()
  "Permission mode cockpit surface."
  [:description mevedel-menu--header
   ["Mode"
    :pad-keys t
    ("k" mevedel-menu--mode-ask-description
     (lambda () (interactive) (mevedel-menu--set-mode 'ask)))
    ("a" mevedel-menu--mode-auto-description
     (lambda () (interactive) (mevedel-menu--set-mode 'auto)))
    ("f" mevedel-menu--mode-full-auto-description
     (lambda () (interactive) (mevedel-menu--set-mode 'full-auto)))
    ("p" mevedel-menu--mode-plan-description mevedel-menu--enter-plan)]
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

(transient-define-prefix mevedel-menu--goal ()
  "Goal cockpit surface."
  [:description mevedel-menu--goal-description
   ["Lifecycle"
    :pad-keys t
    ("s" "Start supervised Goal" mevedel-menu--goal-start
     :inapt-if mevedel-menu--goal-start-inapt-p)
    ("a" "Start automatic Goal" mevedel-menu--goal-start-auto
     :inapt-if mevedel-menu--goal-start-inapt-p)
    ("p" "Pause" (lambda () (interactive)
                     (mevedel-menu--goal-call #'mevedel-goal-pause))
     :inapt-if-not mevedel-menu--goal-active-p)
    ("r" "Resume" (lambda () (interactive)
                      (mevedel-menu--goal-call #'mevedel-goal-resume))
     :inapt-if-not mevedel-menu--goal-resumable-p)
    ("e" "Edit objective" mevedel-menu--goal-edit
     :inapt-if-not mevedel-menu--goal-editable-p)
    ("b" "Set budget" mevedel-menu--goal-set-budget
     :inapt-if-not mevedel-menu--goal-editable-p)
    ("o" mevedel-menu--goal-toggle-approval-policy
     :description mevedel-menu--goal-approval-toggle-description
     :inapt-if-not mevedel-menu--goal-editable-p)
    ("c" "Clear" (lambda () (interactive)
                     (mevedel-menu--goal-call #'mevedel-goal-clear))
     :inapt-if-not mevedel-menu--goal-clearable-p)]
   ["Navigation" ("q" "Back" mevedel-menu)]]
  (interactive)
  (mevedel-menu--context)
  (transient-setup 'mevedel-menu--goal))

(transient-define-prefix mevedel-menu--preset ()
  "Preset and model-team cockpit surface."
  [:description mevedel-menu--preset-description
   ["Preset"
    :pad-keys t
    ("RET" "Select session preset" mevedel-menu--select-preset)
    ("g" "gptel menu" mevedel-menu--open-gptel)]
   ["Navigation" ("q" "Back" mevedel-menu)]]
  (interactive)
  (mevedel-menu--context)
  (transient-setup 'mevedel-menu--preset))

(provide 'mevedel-menu)

;;; mevedel-menu.el ends here
