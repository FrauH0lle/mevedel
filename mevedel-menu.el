;;; mevedel-menu.el -- Session cockpit transient -*- lexical-binding: t -*-

;;; Commentary:

;; Transient-backed session cockpit and shared model-selection surface.
;; The cockpit resolves the live view/data buffer pair once, then routes
;; commands to the buffer or caller that owns the relevant state.

;;; Code:

(require 'transient)
(require 'mevedel-cockpit)
(require 'mevedel-models)

;; `gptel'
(declare-function gptel--model-name "ext:gptel" (model))
(declare-function gptel-backend-name "ext:gptel" (cl-x) t)
(defvar gptel--known-backends)
(defvar gptel-backend)
(defvar gptel-model)
(defvar gptel-reasoning-effort)

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
(declare-function mevedel-cockpit-show-help
                  "mevedel-cockpit" (buffer text))

;; `mevedel-compact'
(declare-function mevedel-compact "mevedel-compact"
                  (&optional aggressive instructions))

;; `mevedel-execution'
(declare-function mevedel-execution-count-user "mevedel-execution" (session))
(autoload 'mevedel-execution-count-user "mevedel-execution")

;; `mevedel-execution-target'
(declare-function mevedel-execution-target-label
                  "mevedel-execution-target" (target))
(declare-function mevedel-execution-target-native-path
                  "mevedel-execution-target" (target path))
(declare-function mevedel-execution-target-native-root
                  "mevedel-execution-target" (cl-x) t)
(declare-function mevedel-execution-target-readiness
                  "mevedel-execution-target" (cl-x) t)
(declare-function mevedel-execution-target-remote-p
                  "mevedel-execution-target" (target))
(declare-function mevedel-execution-target-support-tier
                  "mevedel-execution-target" (cl-x) t)
(autoload 'mevedel-execution-target-label "mevedel-execution-target")
(autoload 'mevedel-execution-target-native-path "mevedel-execution-target")
(autoload 'mevedel-execution-target-native-root "mevedel-execution-target")
(autoload 'mevedel-execution-target-readiness "mevedel-execution-target")
(autoload 'mevedel-execution-target-remote-p "mevedel-execution-target")
(autoload 'mevedel-execution-target-support-tier "mevedel-execution-target")

;; `mevedel-executions-list'
(declare-function mevedel-executions-list-open
                  "mevedel-executions-list" (&optional context))
(autoload 'mevedel-executions-list-open "mevedel-executions-list")

;; `mevedel-goal'
(declare-function mevedel-goal-clear "mevedel-goal" ())
(declare-function mevedel-goal-edit "mevedel-goal" (objective))
(declare-function mevedel-goal-pause "mevedel-goal" ())
(declare-function mevedel-goal-resume "mevedel-goal" (&optional input))
(declare-function mevedel-goal-set-budget "mevedel-goal" (value))
(declare-function mevedel-goal-start "mevedel-goal"
                  (objective))

;; `mevedel-gptel-bridge'
(declare-function mevedel-gptel-bridge-open
                  "mevedel-gptel-bridge" (&optional context))
(autoload 'mevedel-gptel-bridge-open "mevedel-gptel-bridge")

;; `mevedel-models'
(declare-function mevedel-model-current-label "mevedel-models"
                  (&optional buffer))
(declare-function mevedel-model-current-provider-label "mevedel-models"
                  (&optional buffer))
(declare-function mevedel-model-resolve-provider "mevedel-models"
                  (spec &optional noerror))
(declare-function mevedel-model-resolve-tier "mevedel-models"
                  (tier &optional noerror))
(declare-function mevedel-model-resolve-workload "mevedel-models"
                  (workload &optional explicit-selector explicit-effort))
(declare-function mevedel-model-set-session-effort "mevedel-models"
                  (session effort &optional buffer))
(declare-function mevedel-model-set-session-provider "mevedel-models"
                  (session provider &optional buffer))
(declare-function mevedel-model-supported-efforts "mevedel-models" (model))
(defvar mevedel-model-tiers)
(defvar mevedel-model-workloads)

;; `mevedel-permission-mode'
(declare-function mevedel-permission-mode-effective "mevedel-permission-mode"
                  (&optional session data-buffer surface-buffer))
(declare-function mevedel-permission-mode-label "mevedel-permission-mode"
                  (&optional mode))
(declare-function mevedel-permission-mode-transition
                  "mevedel-permission-mode" (mode))
(autoload 'mevedel-permission-mode-effective "mevedel-permission-mode")
(autoload 'mevedel-permission-mode-label "mevedel-permission-mode")
(autoload 'mevedel-permission-mode-transition "mevedel-permission-mode")
(defvar mevedel-permission-mode)

;; `mevedel-permissions-list'
(declare-function mevedel-permissions-list-open "mevedel-permissions-list"
                  (&optional context))
(autoload 'mevedel-permissions-list-open "mevedel-permissions-list")

;; `mevedel-plan-mode'
(declare-function mevedel-plan-approval-render
                  "mevedel-plan-mode" (&optional session))
(declare-function mevedel-plan-mode-enter
                  "mevedel-plan-mode" (&optional session))
(autoload 'mevedel-plan-approval-render "mevedel-plan-mode")
(autoload 'mevedel-plan-mode-enter "mevedel-plan-mode")

;; `mevedel-plugin-registry'
(declare-function mevedel-plugins-count-label "mevedel-plugin-registry"
                  (&optional workspace))
(autoload 'mevedel-plugins-count-label "mevedel-plugin-registry")

;; `mevedel-plugin-ui'
(declare-function mevedel-plugins-list-open "mevedel-plugin-ui"
                  (&optional context))
(autoload 'mevedel-plugins-list-open "mevedel-plugin-ui")

;; `mevedel-presets'
(declare-function mevedel-preset-apply "mevedel-presets" (name &optional buffer))
(defvar mevedel-preset--registry)

;; `mevedel-review'
(declare-function mevedel-review "mevedel-review" (&optional instructions))
(declare-function mevedel-verify "mevedel-review" (&optional instructions))

;; `mevedel-session-control-transfer'
(declare-function mevedel-session-control-transfer-drain-blocker
                  "mevedel-session-control-transfer" (session))
(autoload 'mevedel-session-control-transfer-drain-blocker
  "mevedel-session-control-transfer")

;; `mevedel-session-publication'
(declare-function mevedel-session-publication-status
                  "mevedel-session-publication" (session))
(autoload 'mevedel-session-publication-status
  "mevedel-session-publication")

;; `mevedel-skills-ui'
(declare-function mevedel-skills-count-label "mevedel-skills-ui" (session))
(declare-function mevedel-skills-list-open "mevedel-skills-ui"
                  (&optional context))
(autoload 'mevedel-skills-count-label "mevedel-skills-ui")
(autoload 'mevedel-skills-list-open "mevedel-skills-ui")

;; `mevedel-structs'
(declare-function mevedel-goal-objective "mevedel-structs" (cl-x) t)
(declare-function mevedel-goal-plan-reference "mevedel-structs" (cl-x) t)
(declare-function mevedel-goal-reason "mevedel-structs" (cl-x) t)
(declare-function mevedel-goal-status "mevedel-structs" (cl-x) t)
(declare-function mevedel-goal-time-used-seconds "mevedel-structs" (cl-x) t)
(declare-function mevedel-goal-token-budget "mevedel-structs" (cl-x) t)
(declare-function mevedel-goal-tokens-used "mevedel-structs" (cl-x) t)
(declare-function mevedel-goal-turns-run "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-control-transfer
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-current-segment
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-execution-target
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-goal "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-name "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-pending-plan-approval
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-plan-mode "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-preset-name "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-workspace "mevedel-structs" (cl-x) t)
(declare-function mevedel-workspace-root "mevedel-structs" (cl-x) t)

;; `mevedel-tools'
(declare-function mevedel-tools-active-count "mevedel-tools"
                  (&optional buffer))
(autoload 'mevedel-tools-active-count "mevedel-tools")

;; `mevedel-tools-list'
(declare-function mevedel-tools-list-open "mevedel-tools-list"
                  (&optional context))
(autoload 'mevedel-tools-list-open "mevedel-tools-list")

;; `mevedel-turn'
(declare-function mevedel-request-active-p "mevedel-turn"
                  (&optional buffer))
(declare-function mevedel-request-state-label "mevedel-turn"
                  (&optional buffer))
(autoload 'mevedel-request-active-p "mevedel-turn")
(autoload 'mevedel-request-state-label "mevedel-turn")

;; `mevedel-view-composer'
(declare-function mevedel-view--assert-live-tip
                  "mevedel-view-composer" (&optional allow-armed-fork))
(declare-function mevedel-view-abort "mevedel-view-composer" ())
(declare-function mevedel-view-arm-conversation-fork
                  "mevedel-view-composer" ())
(declare-function mevedel-view-arm-worktree-fork
		  "mevedel-view-composer" ())
(declare-function mevedel-view-send "mevedel-view-composer" ())

;; `mevedel-view-disclosure'
(declare-function mevedel-view-toggle-section "mevedel-view-disclosure" ())

;; `mevedel-view-control-transfer'
(declare-function mevedel-refresh-session "mevedel-view-control-transfer" ())
(declare-function mevedel-release-control "mevedel-view-control-transfer" ())
(declare-function mevedel-take-control "mevedel-view-control-transfer" ())
(declare-function mevedel-toggle-follow "mevedel-view-control-transfer" ())
(declare-function mevedel-view-control-transfer-grant
                  "mevedel-view-control-transfer" ())
(declare-function mevedel-view-control-transfer-keep
                  "mevedel-view-control-transfer" ())
(autoload 'mevedel-refresh-session "mevedel-view-control-transfer")
(autoload 'mevedel-release-control "mevedel-view-control-transfer")
(autoload 'mevedel-take-control "mevedel-view-control-transfer")
(autoload 'mevedel-toggle-follow "mevedel-view-control-transfer")
(autoload 'mevedel-view-control-transfer-grant
  "mevedel-view-control-transfer")
(autoload 'mevedel-view-control-transfer-keep
  "mevedel-view-control-transfer")
(defvar mevedel-session--read-only-mode)
(defvar mevedel-session-follow-published)

;; `mevedel-view-render'
(declare-function mevedel-view-next-display "mevedel-view-render" ())
(declare-function mevedel-view-next-user-query "mevedel-view-render" ())
(declare-function mevedel-view-previous-display "mevedel-view-render" ())
(declare-function mevedel-view-previous-user-query "mevedel-view-render" ())
(declare-function mevedel-view-rewind-at-point "mevedel-view-render" ())
(declare-function mevedel-view-switch-conversation-variant-at-point
                  "mevedel-view-render" ())

;; `mevedel-view-segments'
(declare-function mevedel-view-go-to-segment "mevedel-view-segments"
                  (&optional number))
(declare-function mevedel-view-next-segment "mevedel-view-segments" ())
(declare-function mevedel-view-previous-segment "mevedel-view-segments" ())
(declare-function mevedel-view-segments-current-number
                  "mevedel-view-segments" ())
(autoload 'mevedel-view-go-to-segment "mevedel-view-segments")
(autoload 'mevedel-view-next-segment "mevedel-view-segments")
(autoload 'mevedel-view-previous-segment "mevedel-view-segments")
(autoload 'mevedel-view-segments-current-number "mevedel-view-segments")

;; `mevedel-worktree'
(declare-function mevedel-worktree-status-summary "mevedel-worktree"
                  (&optional context))
(declare-function mevedel-worktree-status-open "mevedel-worktree" ())
(autoload 'mevedel-worktree-status-open "mevedel-worktree")
(autoload 'mevedel-worktree-status-summary "mevedel-worktree")

(defconst mevedel-menu-help-buffer-name "*mevedel help*"
  "Name of the session cockpit help buffer.")

(defconst mevedel-menu-session-info-buffer-name "*mevedel session info*"
  "Name of the session info panel buffer.")

(defconst mevedel-menu-preset-report-buffer-name "*mevedel preset*"
  "Name of the resolved preset policy info panel buffer.")

(defconst mevedel-menu-goal-record-buffer-name "*mevedel goal*"
  "Name of the Goal record info panel buffer.")

;;
;;; Context resolution

(defun mevedel-menu--context ()
  "Return the current live cockpit context."
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
  (format "%-10s %s" label (mevedel-menu--value value face)))

(defun mevedel-menu--mode-symbol (&optional session data-buffer surface-buffer)
  "Return the effective permission mode for the cockpit context."
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
  (mevedel-permission-mode-label mode))

(defun mevedel-menu--model-label ()
  "Return the current model label."
  (mevedel-model-current-label
   (mevedel-cockpit-context-data-buffer (mevedel-menu--context))))

(defun mevedel-menu--active-tool-count ()
  "Return the number of active gptel tools in the data buffer."
  (mevedel-tools-active-count
   (mevedel-cockpit-context-data-buffer (mevedel-menu--context))))

(defun mevedel-menu--root-label (workspace &optional target)
  "Return a compact root label for WORKSPACE and TARGET."
  (cond
   (target
    (abbreviate-file-name (mevedel-execution-target-native-root target)))
   ((and workspace (fboundp 'mevedel-workspace-root))
    (abbreviate-file-name (mevedel-workspace-root workspace)))
   (t "unknown")))

(defun mevedel-menu--worktree-label ()
  "Return the current branch or detached HEAD label."
  (plist-get (mevedel-worktree-status-summary (mevedel-menu--context))
             :label))

(defconst mevedel-menu--info-indent (make-string 14 ?\s)
  "Continuation indent for wrapped session info-panel rows.")

(defun mevedel-menu--info-row (label value)
  "Return an info-panel row for LABEL and VALUE."
  (format "%-13s %s" label value))

(defun mevedel-menu--target-description (session)
  "Return SESSION's execution-target info-panel rows."
  (when-let* ((target (and session
                           (mevedel-session-execution-target session))))
    (let* ((readiness (mevedel-execution-target-readiness target))
           (status (or (plist-get readiness :status) 'not-probed))
           (sandbox (plist-get readiness :sandbox-status)))
      (concat
       (mevedel-menu--info-row
        "Target" (mevedel-execution-target-label target))
       "\n"
       mevedel-menu--info-indent
       (format "tier %s · readiness %s%s"
               (mevedel-execution-target-support-tier target)
               status
               (if sandbox (format " · sandbox %s" sandbox) ""))))))

(defun mevedel-menu--durability-description (session)
  "Return SESSION's persistence, lease, and publication info-panel rows."
  (when (and session (mevedel-session-workspace session))
    (let* ((target (mevedel-session-execution-target session))
           (status (mevedel-session-publication-status session))
           (path (plist-get status :authoritative-state-path))
           (lease (or (plist-get status :lease-state)
                      (if (and target
                               (mevedel-execution-target-remote-p target))
                          'none
                        'local))))
      (concat
       (mevedel-menu--info-row
        "Persistence"
        (if target
            (mevedel-execution-target-native-path target path)
          path))
       "\n"
       mevedel-menu--info-indent
       (format "lease %s · publication %s"
               lease
               (if (plist-get status :pending-publication)
                   "pending"
                 "published"))))))

(defun mevedel-menu--alerts (session)
  "Return SESSION's off-nominal state tokens as a list of strings.
Nominal state belongs in the session info panel; only conditions the user
may want to act on earn a line in a cockpit header."
  (when session
    (let* ((target (mevedel-session-execution-target session))
           (readiness (and target
                           (mevedel-execution-target-readiness target)))
           (status (and readiness (plist-get readiness :status)))
           (sandbox (and readiness (plist-get readiness :sandbox-status)))
           (publication (and (mevedel-session-workspace session)
                             (mevedel-session-publication-status session)))
           (lease (and publication (plist-get publication :lease-state)))
           alerts)
      (when (and status (not (eq status 'ready)))
        (push (format "target %s" status) alerts))
      (when (memq sandbox '(unavailable unsupported))
        (push (format "sandbox %s" sandbox) alerts))
      (when (and lease (not (memq lease '(owned local))))
        (push (format "lease %s" lease) alerts))
      (when (and publication (plist-get publication :pending-publication))
        (push "publication pending" alerts))
      (nreverse alerts))))

(defun mevedel-menu--alert-line (session)
  "Return SESSION's warning-face alert line, or nil when all state is nominal."
  (when-let* ((alerts (mevedel-menu--alerts session)))
    (concat (mevedel-menu--value
             (concat "! " (string-join alerts " · "))
             'warning)
            "\n")))

(defun mevedel-menu--header ()
  "Return the cockpit header string.
One identity line, plus an alert line when session state is off-nominal.
The complete target and durability state lives in the session info panel."
  (let* ((context (mevedel-menu--context))
         (data-buffer (mevedel-cockpit-context-data-buffer context))
         (session (mevedel-cockpit-context-session context))
         (workspace (mevedel-cockpit-context-workspace context))
         (target (and session
                      (mevedel-session-execution-target session)))
         (mode (mevedel-menu--mode-symbol
                session data-buffer
                (mevedel-cockpit-context-view-buffer context)))
         (request-state (mevedel-request-state-label data-buffer)))
    (concat
     (mevedel-menu--face "mevedel" 'transient-heading)
     " "
     (mevedel-menu--value
      (or (and session (mevedel-session-name session)) "unknown"))
     " · "
     (mevedel-menu--value (mevedel-menu--mode-label mode))
     " · "
     (mevedel-menu--value
      request-state
      (if (string= request-state "running") 'warning 'transient-value))
     "    "
     (mevedel-menu--face (mevedel-menu--root-label workspace target)
                         'transient-inactive-value)
     (when target
       (mevedel-menu--face
        (format " · %s" (mevedel-execution-target-label target))
        'transient-inactive-value))
     "\n"
     (mevedel-menu--alert-line session))))

(defun mevedel-menu--read-only-p ()
  "Return non-nil when this cockpit's session is read-only here."
  (when-let ((data (mevedel-cockpit-context-data-buffer
                    (mevedel-menu--context))))
    (buffer-local-value 'mevedel-session--read-only-mode data)))

(defun mevedel-menu--transfer-pending-p ()
  "Return non-nil when another client is waiting for this session's lease."
  (when-let ((session (mevedel-cockpit-context-session
                       (mevedel-menu--context))))
    (and (not (mevedel-menu--read-only-p))
         (memq (plist-get (mevedel-session-control-transfer session) :state)
               '(requested quiescing)))))

(defun mevedel-menu--follow-description ()
  "Return the follow toggle description for the control surface."
  (let ((data (mevedel-cockpit-context-data-buffer (mevedel-menu--context))))
    (format "%-10s %s" "Follow"
            (cond
             ((not (mevedel-menu--read-only-p)) "owner")
             ((and data (buffer-local-value 'mevedel-session-follow-published
                                            data))
              "on")
             (t "off")))))

(defun mevedel-menu--control-summary ()
  "Return the top-level cockpit entry description for session control."
  (format "%-10s %s" "Control"
          (if (mevedel-menu--read-only-p) "read-only" "writable")))

(defun mevedel-menu--control-description ()
  "Return the control surface header.

Authority first, because every action on the surface either changes it or is
unavailable until it changes."
  (let* ((context (mevedel-menu--context))
         (session (mevedel-cockpit-context-session context))
         (read-only-p (mevedel-menu--read-only-p))
         (transfer (and session (mevedel-session-control-transfer session)))
         (label (plist-get (plist-get transfer :request) :requester-label)))
    (concat
     (mevedel-menu--face "control" 'transient-heading)
     " "
     (mevedel-menu--value
      (if read-only-p "read-only" "writable")
      (if read-only-p 'transient-inactive-value 'transient-value))
     " · "
     (mevedel-menu--face
      (pcase (plist-get transfer :state)
        ('requested
         (if read-only-p
             "control requested, waiting for the owner"
           (format "%s is asking for control" (or label "another client"))))
        ('quiescing
         (if read-only-p
             "granted, waiting for the owner to finish"
           (format "granting to %s, finishing %s"
                   (or label "another client")
                   (or (mevedel-session-control-transfer-drain-blocker
                        session)
                       "up"))))
        ('rejected "control request was declined")
        (_ (if read-only-p
               "another client is writing this session"
             "you hold this session's lease")))
      'transient-inactive-value))))

(defun mevedel-menu--session-info-text ()
  "Return the complete session state as info-panel text."
  (let* ((context (mevedel-menu--context))
         (data-buffer (mevedel-cockpit-context-data-buffer context))
         (session (mevedel-cockpit-context-session context))
         (workspace (mevedel-cockpit-context-workspace context))
         (target (and session
                      (mevedel-session-execution-target session)))
         (mode (mevedel-menu--mode-symbol
                session data-buffer
                (mevedel-cockpit-context-view-buffer context))))
    (string-join
     (delq
      nil
      (list
       (format "mevedel session — %s"
               (or (and session (mevedel-session-name session)) "unknown"))
       ""
       (mevedel-menu--info-row
        "Workspace" (mevedel-menu--root-label workspace target))
       (mevedel-menu--target-description session)
       (mevedel-menu--durability-description session)
       (mevedel-menu--info-row
        "Request"
        (format "%s · mode %s · preset %s"
                (mevedel-request-state-label data-buffer)
                (mevedel-menu--mode-label mode)
                (or (and session (mevedel-session-preset-name session))
                    "none")))
       (mevedel-menu--info-row
        "Model"
        (format "%s · effort %s"
                (mevedel-model-current-provider-label data-buffer)
                (or (with-current-buffer data-buffer
                      (and (boundp 'gptel-reasoning-effort)
                           gptel-reasoning-effort))
                    "default")))
       ""))
     "\n")))

(defun mevedel-menu--call-in-view (function)
  "Call view FUNCTION in the cockpit's paired view buffer."
  (mevedel-cockpit-call-in-view (mevedel-menu--context) function))

(defun mevedel-menu--navigate-description ()
  "Return the navigation surface header with the projected segment."
  (let* ((context (mevedel-menu--context))
         (session (mevedel-cockpit-context-session context))
         (view-buffer (mevedel-cockpit-context-view-buffer context))
         (total (or (and session (mevedel-session-current-segment session)) 1))
         (shown (and view-buffer
                     (with-current-buffer view-buffer
                       (mevedel-view-segments-current-number)))))
    (concat
     (mevedel-menu--face "Navigate" 'transient-heading)
     "  "
     (mevedel-menu--value
      (or (and session (mevedel-session-name session)) "unknown"))
     " · segment "
     (mevedel-menu--value
      (if shown
          (format "%d/%d" shown total)
        (format "live (%d)" total))))))

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
    (format "%-10s %s"
            "Model"
            (if (string= model "none")
                (mevedel-menu--inactive-value model)
              (mevedel-menu--value model)))))

(defun mevedel-menu--model-selection-description ()
  "Return the caller-owned model-selection description."
  (let ((scope (transient-scope)))
    (concat
     (mevedel-menu--face (plist-get scope :title) 'transient-heading)
     "  "
     (mevedel-menu--value (plist-get scope :model-provider))
     " · effort "
     (mevedel-menu--value
      (or (plist-get scope :reasoning-effort) "default"))
     (if (plist-get scope :inherited) " · session" ""))))

(defun mevedel-menu--model-selection-effort-description ()
  "Return the effort-cycling row description with its next value."
  (let* ((scope (transient-scope))
         (efforts (mevedel-menu--model-selection-efforts scope))
         (current (plist-get scope :reasoning-effort))
         (position (seq-position efforts current))
         (next (nth (mod (1+ (or position -1)) (length efforts)) efforts)))
    (format "%-13s %s → %s"
            "Cycle effort"
            (mevedel-menu--value (or current "default"))
            (mevedel-menu--inactive-value (or next "default")))))

(defun mevedel-menu--tools-description ()
  "Return the top-level tools row description."
  (mevedel-menu--state-description
   "Tools"
   (format "%d active" (mevedel-menu--active-tool-count))
   'warning))

(defun mevedel-menu--executions-description ()
  "Return the top-level executions row description."
  (let* ((session (mevedel-cockpit-context-session
                   (mevedel-menu--context)))
         (count (mevedel-execution-count-user session)))
    (mevedel-menu--state-description
     "Executions"
     (format "%d live" count)
     (if (> count 0) 'warning 'transient-inactive-value))))

(defun mevedel-menu--skills-description ()
  "Return the top-level skills row description."
  (let ((context (mevedel-menu--context)))
    (mevedel-menu--state-description
     "Skills"
     (mevedel-skills-count-label (mevedel-cockpit-context-session context))
     'warning)))

(defun mevedel-menu--plugins-description ()
  "Return the top-level plugins row description."
  (let ((workspace (mevedel-cockpit-context-workspace
                    (mevedel-menu--context))))
    (mevedel-menu--state-description
     "Plugins" (mevedel-plugins-count-label workspace) 'warning)))

(defun mevedel-menu--worktree-description ()
  "Return the top-level worktree row description."
  (let ((worktree (mevedel-menu--worktree-label)))
    (format "%-10s %s"
            "Worktree"
            (if (string= worktree "not-git")
                (mevedel-menu--inactive-value worktree)
              (mevedel-menu--value worktree)))))

(defun mevedel-menu--current-goal ()
  "Return the current session Goal, or nil."
  (mevedel-session-goal
   (mevedel-cockpit-context-session (mevedel-menu--context))))

(defun mevedel-menu--goal-active-p ()
  "Return non-nil when the current Goal can be paused."
  (when-let* ((goal (mevedel-menu--current-goal)))
    (eq (mevedel-goal-status goal) 'active)))

(defun mevedel-menu--goal-resumable-p ()
  "Return non-nil when the current Goal can be resumed."
  (when-let* ((goal (mevedel-menu--current-goal)))
    (memq (mevedel-goal-status goal) '(paused blocked))))

(defun mevedel-menu--goal-start-inapt-p ()
  "Return non-nil when an unfinished Goal already exists."
  (when-let* ((goal (mevedel-menu--current-goal)))
    (not (eq (mevedel-goal-status goal) 'complete))))

(defun mevedel-menu--goal-clearable-p ()
  "Return non-nil when Goal state may be cleared now."
  (and (mevedel-menu--current-goal)
       (not (mevedel-menu--request-active-p))))

(defun mevedel-menu--goal-budget-label (goal)
  "Return GOAL's token accounting as a compact label."
  (let ((budget (mevedel-goal-token-budget goal)))
    (if budget
        (format "%d/%d tokens" (mevedel-goal-tokens-used goal) budget)
      (format "%d tokens · unbounded" (mevedel-goal-tokens-used goal)))))

(defun mevedel-menu--goal-description ()
  "Return the one-line Goal cockpit status.
The full record lives in the Goal info panel."
  (if-let* ((goal (mevedel-menu--current-goal)))
      (concat
       (mevedel-menu--face "Goal" 'transient-heading)
       "  "
       (mevedel-menu--value (mevedel-goal-objective goal))
       " · "
       (mevedel-menu--value (format "%s" (mevedel-goal-status goal))
                            (if (eq (mevedel-goal-status goal) 'blocked)
                                'warning
                              'transient-value))
       (format " · %d turns · " (mevedel-goal-turns-run goal))
       (mevedel-menu--value (mevedel-menu--goal-budget-label goal)))
    (concat
     (mevedel-menu--face "Goal" 'transient-heading)
     "  "
     (mevedel-menu--inactive-value
      "none — s starts one, or /goal OBJECTIVE"))))

(defun mevedel-menu--goal-record-text ()
  "Return the complete Goal record as info-panel text."
  (if-let* ((goal (mevedel-menu--current-goal)))
      (string-join
       (list
        "mevedel Goal"
        ""
        (mevedel-menu--info-row "Objective" (mevedel-goal-objective goal))
        (mevedel-menu--info-row
         "Status"
         (format "%s%s"
                 (mevedel-goal-status goal)
                 (if-let* ((reason (mevedel-goal-reason goal)))
                     (format " — %s" reason) "")))
        (mevedel-menu--info-row
         "Budget" (mevedel-menu--goal-budget-label goal))
        (mevedel-menu--info-row
         "Turns"
         (format "%d · elapsed %ds"
                 (mevedel-goal-turns-run goal)
                 (mevedel-goal-time-used-seconds goal)))
        (mevedel-menu--info-row
         "Plan" (or (mevedel-goal-plan-reference goal) "none"))
        "")
       "\n")
    "mevedel Goal\n\nNo active Goal. Start one here or with /goal OBJECTIVE.\n"))

(defun mevedel-menu--preset-policies ()
  "Return the session's resolved preset policies.
Each element is (KIND NAME LABEL ERROR), where ERROR is the resolution
failure message or nil.  KIND is `tier' or `workload'."
  (let ((context (mevedel-menu--context)))
    (with-current-buffer (mevedel-cockpit-context-data-buffer context)
      (let ((resolve
             (lambda (kind name resolver)
               (condition-case err
                   (let ((policy (funcall resolver name)))
                     (list kind name
                           (format "%s:%s · effort %s"
                                   (gptel-backend-name
                                    (plist-get policy :backend))
                                   (gptel--model-name
                                    (plist-get policy :model))
                                   (or (plist-get policy :effort) "default"))
                           nil))
                 (error
                  (list kind name nil (error-message-string err)))))))
        (append
         (mapcar (lambda (tier)
                   (funcall resolve 'tier tier #'mevedel-model-resolve-tier))
                 (delete-dups (mapcar #'car mevedel-model-tiers)))
         (mapcar (lambda (workload)
                   (funcall resolve 'workload workload
                            #'mevedel-model-resolve-workload))
                 (delete-dups (mapcar #'car mevedel-model-workloads))))))))

(defun mevedel-menu--preset-description ()
  "Return the one-line preset summary, plus an alert for broken policies.
The resolved policy table lives in the preset info panel."
  (let* ((context (mevedel-menu--context))
         (session (mevedel-cockpit-context-session context))
         (policies (mevedel-menu--preset-policies))
         (broken (seq-filter (lambda (policy) (nth 3 policy)) policies))
         (tiers (seq-count (lambda (policy) (eq (car policy) 'tier)) policies)))
    (concat
     (mevedel-menu--face "Preset" 'transient-heading)
     "  "
     (mevedel-menu--value
      (or (and session (mevedel-session-preset-name session)) "none"))
     (format " · %d tiers · %d workloads · " tiers (- (length policies) tiers))
     (if broken
         (mevedel-menu--value (format "%d broken" (length broken)) 'error)
       (mevedel-menu--value "all resolved"))
     (when broken
       (concat
        "\n"
        (mevedel-menu--value
         (format "! %s %s does not resolve — fix before dispatch"
                 (nth 0 (car broken)) (nth 1 (car broken)))
         'error))))))

(defun mevedel-menu--preset-report-text ()
  "Return the resolved preset policy table as info-panel text."
  (let* ((context (mevedel-menu--context))
         (session (mevedel-cockpit-context-session context))
         (policies (mevedel-menu--preset-policies))
         (row (lambda (policy)
                (format "  %-18s %s"
                        (nth 1 policy)
                        (or (nth 2 policy)
                            (format "ERROR: %s" (nth 3 policy))))))
         (of-kind (lambda (kind)
                    (mapcar row
                            (seq-filter
                             (lambda (policy) (eq (car policy) kind))
                             policies)))))
    (string-join
     (append
      (list (format "mevedel preset — %s"
                    (or (and session (mevedel-session-preset-name session))
                        "none"))
            ""
            "Tiers")
      (funcall of-kind 'tier)
      (list "" "Workloads")
      (funcall of-kind 'workload)
      (list ""))
     "\n")))

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
    (mevedel-menu--mode-row label detail current)))

(defun mevedel-menu--mode-row (label detail current)
  "Return a mode-surface row for LABEL and DETAIL.
CURRENT marks the row as the active choice.  The marker trails the
descriptions so they stay aligned and read as a comparison."
  (string-trim-right
   (format "%-10s %-40s %s"
           (if current (mevedel-menu--value label) label)
           detail
           (if current (mevedel-menu--value "current" 'warning) ""))))

(defun mevedel-menu--mode-ask-description ()
  "Return the ask mode row description."
  (mevedel-menu--mode-choice-description
   'ask "prompt for edits and uncertain execution"))

(defun mevedel-menu--mode-edits-description ()
  "Return the edits mode row description."
  (mevedel-menu--mode-choice-description
   'edits "auto-apply edit previews"))

(defun mevedel-menu--mode-full-auto-description ()
  "Return the full-auto mode row description."
  (mevedel-menu--mode-choice-description 'full-auto "auto-allow tools"))

(defun mevedel-menu--mode-plan-description ()
  "Return the Plan mode row description.
Plan is orthogonal to the permission mode, so it reads as its own on/off
state rather than as a fourth permission choice."
  (let* ((session (mevedel-cockpit-context-session
                   (mevedel-menu--context)))
         (current (mevedel-session-plan-mode session)))
    (format "%-10s %s · %s"
            "Plan mode"
            (if current
                (mevedel-menu--value "on" 'warning)
              (mevedel-menu--inactive-value "off"))
            "inspect and discuss, no edits")))

(defun mevedel-menu--mode-surface-description ()
  "Return the one-line mode surface header."
  (let* ((context (mevedel-menu--context))
         (session (mevedel-cockpit-context-session context))
         (permission
          (mevedel-menu--mode-label
           (mevedel-menu--mode-symbol
            session
            (mevedel-cockpit-context-data-buffer context)
            (mevedel-cockpit-context-view-buffer context)))))
    (concat
     (mevedel-menu--face "Mode" 'transient-heading)
     "  permission "
     (mevedel-menu--value permission)
     " · Plan "
     (if (mevedel-session-plan-mode session)
         (mevedel-menu--value "on" 'warning)
       (mevedel-menu--inactive-value "off")))))


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
  (mevedel-cockpit-call-in-data
   (mevedel-menu--context) #'mevedel-permission-mode-transition mode)
  (force-mode-line-update t))

(defun mevedel-menu--enter-plan ()
  "Enter Plan mode for the current cockpit session."
  (interactive)
  (mevedel-cockpit-call-in-data
   (mevedel-menu--context) #'mevedel-plan-mode-enter)
  (force-mode-line-update t))


;;
;;; Model surface

(defun mevedel-menu--refresh-plan-approval (session)
  "Refresh SESSION's visible Plan approval when one is pending."
  (when (and (mevedel-session-pending-plan-approval session)
             (fboundp 'mevedel-plan-approval-render))
    (mevedel-plan-approval-render session)))

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

(defun mevedel-menu--model-selection-select-model ()
  "Select a model for the caller-owned model-selection scope."
  (interactive)
  (let ((candidates (mevedel-menu--model-candidates)))
    (unless candidates
      (user-error "No registered gptel models"))
    (let* ((scope (transient-scope))
           (label (completing-read "Model: " candidates nil t))
           (provider (cdr (assoc label candidates)))
           (effort (plist-get scope :reasoning-effort))
           (model (plist-get provider :model)))
      (unless (memq effort (mevedel-model-supported-efforts model))
        (setq effort nil))
      (plist-put scope :model-provider label)
      (plist-put scope :reasoning-effort effort)
      (plist-put scope :inherited nil)
      (funcall (plist-get scope :update) label effort))))

(defun mevedel-menu--model-selection-efforts (&optional scope)
  "Return the selectable efforts for SCOPE's model.
The list is the model's supported efforts followed by nil for the
provider default, which is the full cycle the surface offers."
  (let* ((scope (or scope (transient-scope)))
         (provider (mevedel-model-resolve-provider
                    (plist-get scope :model-provider) t)))
    (append (and provider
                 (mevedel-model-supported-efforts
                  (plist-get provider :model)))
            (list nil))))

(defun mevedel-menu--model-selection-cycle-effort ()
  "Advance the caller-owned model selection to the next reasoning effort.
Effort is a closed enum the chosen model determines, so it cycles in
place instead of prompting."
  (interactive)
  (let* ((scope (transient-scope))
         (efforts (mevedel-menu--model-selection-efforts scope))
         (current (plist-get scope :reasoning-effort))
         (position (seq-position efforts current))
         (effort (nth (mod (1+ (or position -1)) (length efforts)) efforts)))
    (plist-put scope :reasoning-effort effort)
    (plist-put scope :inherited nil)
    (funcall (plist-get scope :update)
             (plist-get scope :model-provider) effort)))

(defun mevedel-menu--model-selection-reset ()
  "Reset the caller-owned model selection to its inherited values."
  (interactive)
  (let* ((scope (transient-scope))
         (values (funcall (plist-get scope :reset))))
    (plist-put scope :model-provider (car values))
    (plist-put scope :reasoning-effort (cadr values))
    (plist-put scope :inherited t)))

(cl-defun mevedel-menu-open-model-selection
    (&key title provider effort update reset inherited)
  "Open shared model selection for caller-owned state."
  (transient-setup
   'mevedel-menu--model-selection nil nil
   :scope (list :title title
                :model-provider provider
                :reasoning-effort effort
                :update update
                :reset reset
                :inherited inherited)))

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
      ('navigate
       (transient-setup 'mevedel-menu--navigate))
      ('session-info
       (mevedel-menu--open-session-info))
      ('mode
       (transient-setup 'mevedel-menu--mode))
      ('permissions
       (mevedel-cockpit-call-in-data
        context #'mevedel-permissions-list-open context))
      ('model
       (mevedel-menu--open-model))
      ('goal
       (transient-setup 'mevedel-menu--goal))
      ('control
       (transient-setup 'mevedel-menu--control))
      ('preset
       (transient-setup 'mevedel-menu--preset))
      ('skills
       (mevedel-cockpit-call-in-data
        context #'mevedel-skills-list-open context))
      ('plugins
       (mevedel-cockpit-call-in-data
        context #'mevedel-plugins-list-open context))
      ('tools
       (mevedel-cockpit-call-in-data
        context #'mevedel-tools-list-open context))
      ('executions
       (mevedel-cockpit-call-in-data
        context #'mevedel-executions-list-open context))
      ('worktree
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

(defun mevedel-menu--take-control ()
  "Take control of this session from the view buffer."
  (interactive)
  (mevedel-cockpit-call-in-view (mevedel-menu--context) #'mevedel-take-control))

(defun mevedel-menu--release-control ()
  "Release this session's lease from the view buffer."
  (interactive)
  (mevedel-cockpit-call-in-view
   (mevedel-menu--context) #'mevedel-release-control))

(defun mevedel-menu--grant-control ()
  "Grant the pending control-transfer request from the view buffer."
  (interactive)
  (mevedel-cockpit-call-in-view
   (mevedel-menu--context) #'mevedel-view-control-transfer-grant))

(defun mevedel-menu--keep-control ()
  "Decline the pending control-transfer request from the view buffer."
  (interactive)
  (mevedel-cockpit-call-in-view
   (mevedel-menu--context) #'mevedel-view-control-transfer-keep))

(defun mevedel-menu--toggle-follow ()
  "Toggle published-state following from the view buffer."
  (interactive)
  (mevedel-cockpit-call-in-view (mevedel-menu--context) #'mevedel-toggle-follow))

(defun mevedel-menu--refresh-session ()
  "Re-read the owner's newest published state from the view buffer."
  (interactive)
  (mevedel-cockpit-call-in-view
   (mevedel-menu--context) #'mevedel-refresh-session))

(defun mevedel-menu--rewind-here ()
  "Rewind to the settled assistant turn at point in the view buffer."
  (interactive)
  (mevedel-cockpit-call-in-view
   (mevedel-menu--context) #'mevedel-view-rewind-at-point))

(defun mevedel-menu--switch-variant-here ()
  "Switch conversation variants at point in the paired view buffer."
  (interactive)
  (mevedel-cockpit-call-in-view
   (mevedel-menu--context)
   #'mevedel-view-switch-conversation-variant-at-point))

(defun mevedel-menu--fork-conversation-here ()
  "Arm a Conversation Fork at point in the paired view buffer."
  (interactive)
  (mevedel-cockpit-call-in-view
   (mevedel-menu--context) #'mevedel-view-arm-conversation-fork))

(defun mevedel-menu--fork-worktree-here ()
  "Arm a Worktree Fork at point in the paired view buffer."
  (interactive)
  (mevedel-cockpit-call-in-view
   (mevedel-menu--context) #'mevedel-view-arm-worktree-fork))

(defun mevedel-menu--call-live-tip-data (function)
  "Call data-buffer FUNCTION after checking the paired view is live."
  (let ((context (mevedel-menu--context)))
    (mevedel-cockpit-call-in-view
     context #'mevedel-view--assert-live-tip)
    (mevedel-cockpit-call-in-data context function)))

(defun mevedel-menu--compact ()
  "Compact the current data buffer."
  (interactive)
  (mevedel-menu--call-live-tip-data #'mevedel-compact))

(defun mevedel-menu--review ()
  "Run the review picker from the data buffer."
  (interactive)
  (mevedel-menu--call-live-tip-data #'mevedel-review))

(defun mevedel-menu--verify ()
  "Run the verify picker from the data buffer."
  (interactive)
  (mevedel-menu--call-live-tip-data #'mevedel-verify))

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

(defun mevedel-menu--open-permissions ()
  "Open the permissions cockpit surface."
  (interactive)
  (mevedel-menu-open 'permissions))

(defun mevedel-menu--open-model ()
  "Open the model cockpit surface."
  (interactive)
  (let* ((context (mevedel-menu--context))
         (session (mevedel-cockpit-context-session context))
         (buffer (mevedel-cockpit-context-data-buffer context)))
    (mevedel-menu-open-model-selection
     :title "Session model"
     :provider (mevedel-model-current-provider-label buffer)
     :effort (with-current-buffer buffer
               (and (boundp 'gptel-reasoning-effort)
                    gptel-reasoning-effort))
     :update
     (lambda (provider effort)
       (mevedel-model-set-session-provider
        session (mevedel-model-resolve-provider provider) buffer)
       (mevedel-model-set-session-effort session effort buffer)
       (mevedel-menu--refresh-plan-approval session)
       (force-mode-line-update t)))))

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
  "Prompt for and start a Goal."
  (interactive)
  (mevedel-menu--goal-call
   #'mevedel-goal-start (read-string "Goal objective: ")))

(defun mevedel-menu--goal-edit ()
  "Prompt for and replace the current Goal objective."
  (interactive)
  (mevedel-menu--goal-call
   #'mevedel-goal-edit (read-string "New Goal objective: ")))

(defun mevedel-menu--goal-budget ()
  "Prompt for and replace the current Goal token budget."
  (interactive)
  (mevedel-menu--goal-call
   #'mevedel-goal-set-budget
   (read-string "Goal token budget (positive integer or none): ")))

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

(defun mevedel-menu--open-navigate ()
  "Open the navigation cockpit surface."
  (interactive)
  (mevedel-menu-open 'navigate))

(defun mevedel-menu--open-control ()
  "Open the session control cockpit surface."
  (interactive)
  (mevedel-menu-open 'control))

(defun mevedel-menu--open-session-info ()
  "Open the session info panel."
  (interactive)
  (mevedel-cockpit-show-help
   mevedel-menu-session-info-buffer-name
   (mevedel-menu--session-info-text)))

(defun mevedel-menu--open-preset-report ()
  "Open the resolved preset policy info panel."
  (interactive)
  (mevedel-cockpit-show-help
   mevedel-menu-preset-report-buffer-name
   (mevedel-menu--preset-report-text)))

(defun mevedel-menu--open-goal-record ()
  "Open the Goal record info panel."
  (interactive)
  (mevedel-cockpit-show-help
   mevedel-menu-goal-record-buffer-name
   (mevedel-menu--goal-record-text)))

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
     "Cockpit u                   Remembered permission authority"
     "Cockpit i                   Session info panel"
     "/tools, /tools list         Tools"
     "/ps                         Live executions"
     "/stop [EXECUTION_ID]        Stop one execution, or all when omitted"
     "/worktree, /worktree status Worktree"
     "/help                       Help"
     ""
     "Direct slash commands"
     "/plugin enable NAME, disable NAME, reload, update NAME"
     "/plugin install TARGET, remove NAME, uninstall NAME, hooks ..."
     "/skills enable NAME, disable NAME, help NAME"
     "/mode MODE, /model MODEL"
     "/worktree create [NAME] [--for \"purpose\"] [--clean]"
     "/goal OBJECTIVE, /goal budget N|none, /goal edit|pause|resume|clear"
     "/compact, /review, /verify, /edits, /clear, /init ..., /tokens"
     ""
     "Modes"
     "ask       Prompt for edits and uncertain execution."
     "edits     Auto-apply native edits; check Bash and Eval."
     "full-auto Skip heuristic Bash and Eval prompts."
     ""
     "View and data buffers"
     "The view buffer owns the composer, compact transcript, and status strip."
     "The data buffer owns raw gptel state, tools, model, and transcript data."
     "The cockpit resolves the view/data pair once and routes actions to the owning buffer."
     "Cockpit N opens Navigate: [ / ] / g inspect session segments,"
     "n / p move through displays, C-n / C-p through queries, TAB folds a section."
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
  (mevedel-gptel-bridge-open (mevedel-menu--context)))


;;
;;; Transient

(transient-define-prefix mevedel-menu--top ()
  "Top-level mevedel session cockpit."
  [:description mevedel-menu--header
   ["Conversation"
    :pad-keys t
    ("RET" "Send" mevedel-menu--send
     :inapt-if mevedel-menu--send-inapt-p)
    ("a" "Abort" mevedel-menu--abort
     :inapt-if mevedel-menu--abort-inapt-p)
    ("c" "Compact" mevedel-menu--compact)
    ("r" "Review" mevedel-menu--review)
    ("v" "Verify" mevedel-menu--verify)]
   ["History"
    :pad-keys t
    ("N" "Navigate…" mevedel-menu--open-navigate)
    ("f" "Fork conversation" mevedel-menu--fork-conversation-here)
    ("F" "Fork worktree" mevedel-menu--fork-worktree-here)
    ("R" "Rewind here" mevedel-menu--rewind-here)
    ("B" "Switch variant" mevedel-menu--switch-variant-here)]
   ["Configure"
    :pad-keys t
    ("m" mevedel-menu--open-mode
     :description mevedel-menu--mode-description)
    ("M" mevedel-menu--open-model
     :description mevedel-menu--model-description)
    ("P" mevedel-menu--open-preset
     :description (lambda () (format "%-10s %s" "Preset"
                                     (or (mevedel-session-preset-name
                                          (mevedel-cockpit-context-session
                                           (mevedel-menu--context)))
                                         "none"))))
    ("u" "Permissions" mevedel-menu--open-permissions)
    ("G" mevedel-menu--open-goal
     :description (lambda () (format "%-10s %s" "Goal"
                                     (if-let* ((goal (mevedel-menu--current-goal)))
                                         (format "%s, %d turns"
                                                 (mevedel-goal-status goal)
                                                 (mevedel-goal-turns-run goal))
                                       "none"))))]
   ["Cockpits"
    :pad-keys t
    ("t" mevedel-menu--open-tools
     :description mevedel-menu--tools-description)
    ("x" mevedel-menu--open-executions
     :description mevedel-menu--executions-description)
    ("s" mevedel-menu--open-skills
     :description mevedel-menu--skills-description)
    ("p" mevedel-menu--open-plugins
     :description mevedel-menu--plugins-description)
    ("w" mevedel-menu--open-worktree
     :description mevedel-menu--worktree-description)
    ("C" mevedel-menu--open-control
     :description mevedel-menu--control-summary)
    ("d" "Data view" mevedel-menu--toggle-data-view)
    ("i" "Session info" mevedel-menu--open-session-info)
    ("g" "gptel menu" mevedel-menu--open-gptel)
    ("?" "Help" mevedel-menu--open-help)]]
  (interactive)
  (mevedel-menu--context)
  (transient-setup 'mevedel-menu--top))

(transient-define-prefix mevedel-menu--control ()
  "Session control cockpit surface.

Who may write this session, and what a non-owner sees of the owner's work.
The same surface serves both sides: a client that wants control and a host
that is being asked for it are the same protocol seen from two ends."
  [:description mevedel-menu--control-description
   ["Control"
    :pad-keys t
    ("t" "Take control" mevedel-menu--take-control
     :inapt-if-not mevedel-menu--read-only-p)
    ("q" "Release control" mevedel-menu--release-control
     :inapt-if mevedel-menu--read-only-p)
    ("g" "Grant" mevedel-menu--grant-control
     :inapt-if-not mevedel-menu--transfer-pending-p)
    ("k" "Keep" mevedel-menu--keep-control
     :inapt-if-not mevedel-menu--transfer-pending-p)]
   ["Follow"
    :pad-keys t
    ("f" mevedel-menu--toggle-follow
     :description mevedel-menu--follow-description
     :inapt-if-not mevedel-menu--read-only-p
     :transient t)
    ("r" "Refresh now" mevedel-menu--refresh-session
     :inapt-if-not mevedel-menu--read-only-p
     :transient t)]
   ["Inspect"
    :pad-keys t
    ("i" "Session info" mevedel-menu--open-session-info)]]
  (interactive)
  (mevedel-menu--context)
  (transient-setup 'mevedel-menu--control))

(transient-define-prefix mevedel-menu--navigate ()
  "Transcript navigation cockpit surface.
Every entry stays open so repeated motion needs one menu, and the keys
match the ones the view buffer itself binds."
  [:description mevedel-menu--navigate-description
   ["Segment"
    :pad-keys t
    ("[" "Previous"
     (lambda () (interactive)
       (mevedel-menu--call-in-view #'mevedel-view-previous-segment))
     :transient t)
    ("]" "Next"
     (lambda () (interactive)
       (mevedel-menu--call-in-view #'mevedel-view-next-segment))
     :transient t)
    ("g" "Go to…"
     (lambda () (interactive)
       (mevedel-menu--call-in-view #'mevedel-view-go-to-segment))
     :transient t)]
   ["Display"
    :pad-keys t
    ("n" "Next"
     (lambda () (interactive)
       (mevedel-menu--call-in-view #'mevedel-view-next-display))
     :transient t)
    ("p" "Previous"
     (lambda () (interactive)
       (mevedel-menu--call-in-view #'mevedel-view-previous-display))
     :transient t)]
   ["Query"
    :pad-keys t
    ("C-n" "Next"
     (lambda () (interactive)
       (mevedel-menu--call-in-view #'mevedel-view-next-user-query))
     :transient t)
    ("C-p" "Previous"
     (lambda () (interactive)
       (mevedel-menu--call-in-view #'mevedel-view-previous-user-query))
     :transient t)]
   ["Section"
    :pad-keys t
    ("TAB" "Toggle"
     (lambda () (interactive)
       (mevedel-menu--call-in-view #'mevedel-view-toggle-section))
     :transient t)
    ("q" "Back" mevedel-menu)]]
  (interactive)
  (mevedel-menu--context)
  (transient-setup 'mevedel-menu--navigate))

(transient-define-prefix mevedel-menu--mode ()
  "Permission mode cockpit surface."
  [:description mevedel-menu--mode-surface-description
   ["Permission mode"
    :pad-keys t
    ("k" (lambda () (interactive) (mevedel-menu--set-mode 'ask))
     :description mevedel-menu--mode-ask-description)
    ("e" (lambda () (interactive) (mevedel-menu--set-mode 'edits))
     :description mevedel-menu--mode-edits-description)
    ("f" (lambda () (interactive) (mevedel-menu--set-mode 'full-auto))
     :description mevedel-menu--mode-full-auto-description)]
   ["Conversation"
    :pad-keys t
    ("p" mevedel-menu--enter-plan
     :description mevedel-menu--mode-plan-description)
    ("q" "Back" mevedel-menu)]]
  (interactive)
  (mevedel-menu--context)
  (transient-setup 'mevedel-menu--mode))

(transient-define-prefix mevedel-menu--model-selection ()
  "Shared caller-owned model-selection surface."
  [:description mevedel-menu--model-selection-description
   ["Model"
    :pad-keys t
    ("RET" "Choose model…" mevedel-menu--model-selection-select-model
     :transient t)
    ;; Bind the GUI Return event too, without advertising it twice.
    ("<return>" "Choose model…" mevedel-menu--model-selection-select-model
     :transient t :if (lambda (&rest _) nil))
    ("s" "Use session model" mevedel-menu--model-selection-reset
     :if (lambda () (plist-get (transient-scope) :reset))
     :transient t)]
   ["Effort"
    :pad-keys t
    ("e" mevedel-menu--model-selection-cycle-effort
     :description mevedel-menu--model-selection-effort-description
     :transient t)
    ("q" "Back" transient-quit-one)]])

(transient-define-prefix mevedel-menu--goal ()
  "Goal cockpit surface."
  [:description mevedel-menu--goal-description
   ["Lifecycle"
    :pad-keys t
    ("s" "Start Goal" mevedel-menu--goal-start
     :inapt-if mevedel-menu--goal-start-inapt-p)
    ("p" "Pause" (lambda () (interactive)
                     (mevedel-menu--goal-call #'mevedel-goal-pause))
     :inapt-if-not mevedel-menu--goal-active-p)
    ("r" "Resume" (lambda () (interactive)
                      (mevedel-menu--goal-call #'mevedel-goal-resume))
     :inapt-if-not mevedel-menu--goal-resumable-p)
    ("c" "Clear" (lambda () (interactive)
                     (mevedel-menu--goal-call #'mevedel-goal-clear))
     :inapt-if-not mevedel-menu--goal-clearable-p)]
   ["Adjust"
    :pad-keys t
    ("e" "Edit objective" mevedel-menu--goal-edit
     :inapt-if-not mevedel-menu--current-goal)
    ("b" "Set budget" mevedel-menu--goal-budget
     :inapt-if-not mevedel-menu--current-goal)]
   ["Inspect"
    :pad-keys t
    ("i" "Goal record" mevedel-menu--open-goal-record)
    ("q" "Back" mevedel-menu)]]
  (interactive)
  (mevedel-menu--context)
  (transient-setup 'mevedel-menu--goal))

(transient-define-prefix mevedel-menu--preset ()
  "Preset and model-team cockpit surface."
  [:description mevedel-menu--preset-description
   ["Preset"
    :pad-keys t
    ("RET" "Choose preset…" mevedel-menu--select-preset)
    ("g" "gptel menu" mevedel-menu--open-gptel)]
   ["Inspect"
    :pad-keys t
    ("i" "Model policy report" mevedel-menu--open-preset-report)
    ("q" "Back" mevedel-menu)]]
  (interactive)
  (mevedel-menu--context)
  (transient-setup 'mevedel-menu--preset))

(provide 'mevedel-menu)

;;; mevedel-menu.el ends here
