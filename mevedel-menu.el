;;; mevedel-menu.el -- Session cockpit transient -*- lexical-binding: t -*-

;;; Commentary:

;; Transient-backed session cockpit and shared model-selection surface.
;; The cockpit resolves the live view/data buffer pair once, then routes
;; commands to the buffer or caller that owns the relevant state.

;;; Code:

(require 'mevedel-report)

(require 'transient)
(require 'mevedel-cockpit)
(require 'mevedel-models)

;; `gptel'
(declare-function gptel--model-name "ext:gptel" (model))
(declare-function gptel-backend-name "ext:gptel" (cl-x) t)
(defvar gptel-backend)
(defvar gptel-model)
(defvar gptel-reasoning-effort)

;; `mevedel-artifacts-list'
(declare-function mevedel-artifacts-list-count
                  "mevedel-artifacts-list" (session))
(declare-function mevedel-artifacts-list-open
                  "mevedel-artifacts-list" (&optional context))
(autoload 'mevedel-artifacts-list-count "mevedel-artifacts-list")
(autoload 'mevedel-artifacts-list-open "mevedel-artifacts-list")
(autoload 'mevedel-artifacts-new-shared-item "mevedel-artifacts-list" nil t)

;; `mevedel-chat'
(declare-function mevedel--display-chat-buffer "mevedel-chat" (chat-buffer))
(autoload 'mevedel--display-chat-buffer "mevedel-chat")

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

;; `mevedel-collaboration'
(declare-function mevedel-collaboration-room-summary
                  "mevedel-collaboration" (data-buffer))
(declare-function mevedel-collaboration-shares "mevedel-collaboration" ())
(declare-function mevedel-collaboration-show-share
                  "mevedel-collaboration" (entry))
(declare-function mevedel-collaboration-status "mevedel-collaboration" ())
(declare-function mevedel-collaboration-stop-all "mevedel-collaboration" ())
(declare-function mevedel-collaboration-stop-session
                  "mevedel-collaboration" (data-buffer))
(declare-function mevedel-collaboration-stop-share
                  "mevedel-collaboration" (entry))
(declare-function mevedel-collaboration-view "mevedel-collaboration" ())
(autoload 'mevedel-collaboration-status "mevedel-collaboration" nil t)
(autoload 'mevedel-collaboration-stop-session "mevedel-collaboration")
(autoload 'mevedel-collaboration-view "mevedel-collaboration" nil t)

;; `mevedel-collaboration-lobby'
(declare-function mevedel-collaboration-lobby "mevedel-collaboration-lobby" ())
(declare-function mevedel-collaboration-lobby-live-p
                  "mevedel-collaboration-lobby" (workspace))
(declare-function mevedel-collaboration-lobby-rotate
                  "mevedel-collaboration-lobby" ())
(declare-function mevedel-collaboration-lobby-stop "mevedel-collaboration-lobby" ())
(autoload 'mevedel-collaboration-lobby "mevedel-collaboration-lobby" nil t)
(autoload 'mevedel-collaboration-lobby-rotate "mevedel-collaboration-lobby" nil t)
(autoload 'mevedel-collaboration-lobby-stop "mevedel-collaboration-lobby" nil t)

;; `mevedel-compact'
(declare-function mevedel-compact "mevedel-compact"
                  (&optional aggressive instructions))

;; `mevedel-execution'
(declare-function mevedel-execution-count-user "mevedel-execution" (session))
(autoload 'mevedel-execution-count-user "mevedel-execution")

;; `mevedel-execution-target'
(declare-function mevedel-execution-target-label
                  "mevedel-execution-target" (target &optional directory))
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
(autoload 'mevedel-executions-list-open "mevedel-executions-list" nil t)

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
(autoload 'mevedel-gptel-bridge-open "mevedel-gptel-bridge" nil t)

;; `mevedel-memory-list'
(declare-function mevedel-memory-list-open "mevedel-memory-list" (&optional context))
(autoload 'mevedel-memory-list-open "mevedel-memory-list")

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
(declare-function mevedel-model-candidates "mevedel-models" ())
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
(autoload 'mevedel-plan-mode-enter "mevedel-plan-mode" nil t)

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

;; `mevedel-session-rewind'
(declare-function mevedel-redo "mevedel-session-rewind" ())
(autoload 'mevedel-redo "mevedel-session-rewind" nil t)

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
(declare-function mevedel-goal-tokens-incomplete-p "mevedel-structs" (cl-x) t)
(declare-function mevedel-goal-tokens-used "mevedel-structs" (cl-x) t)
(declare-function mevedel-goal-turns-run "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-authority-mode "mevedel-structs" (cl-x) t)
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
(declare-function mevedel-workspace-memory-observation "mevedel-structs" (cl-x) t)
(declare-function mevedel-workspace-root "mevedel-structs" (cl-x) t)

;; `mevedel-subscription-usage'
(autoload 'mevedel-subscription-usage-show "mevedel-subscription-usage" nil t)

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
(declare-function mevedel-turn-busy-p "mevedel-turn" (&optional buffer))
(autoload 'mevedel-request-active-p "mevedel-turn")
(autoload 'mevedel-request-state-label "mevedel-turn")
(autoload 'mevedel-turn-busy-p "mevedel-turn")

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
(autoload 'mevedel-refresh-session "mevedel-view-control-transfer" nil t)
(autoload 'mevedel-release-control "mevedel-view-control-transfer" nil t)
(autoload 'mevedel-take-control "mevedel-view-control-transfer" nil t)
(autoload 'mevedel-toggle-follow "mevedel-view-control-transfer" nil t)
(autoload 'mevedel-view-control-transfer-grant
  "mevedel-view-control-transfer" nil t)
(autoload 'mevedel-view-control-transfer-keep
  "mevedel-view-control-transfer" nil t)
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
(autoload 'mevedel-view-go-to-segment "mevedel-view-segments" nil t)
(autoload 'mevedel-view-next-segment "mevedel-view-segments" nil t)
(autoload 'mevedel-view-previous-segment "mevedel-view-segments" nil t)
(autoload 'mevedel-view-segments-current-number "mevedel-view-segments")

;; `mevedel-worktree'
(declare-function mevedel-worktree-status-summary "mevedel-worktree"
                  (&optional context))
(declare-function mevedel-worktree-status-open "mevedel-worktree" ())
(autoload 'mevedel-worktree-status-open "mevedel-worktree" nil t)
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

(defconst mevedel-menu--value-width 24
  "Most columns a top menu row's state value takes.
Longer values end in an ellipsis; their surface shows them whole.  The
layout widths in `mevedel-menu--layout-widths' assume this bound.")

(defun mevedel-menu--fit (text)
  "Return TEXT shortened to `mevedel-menu--value-width' columns."
  (truncate-string-to-width (format "%s" text) mevedel-menu--value-width
                            nil nil "…"))

(defun mevedel-menu--state-description (label value &optional face)
  "Return a padded cockpit row for LABEL and state VALUE."
  (format "%-10s %s" label (mevedel-menu--value (mevedel-menu--fit value) face)))

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

(defun mevedel-menu--target-description (session)
  "Return SESSION's execution-target info-panel rows."
  (when-let* ((target (and session (mevedel-session-execution-target session))))
    (let* ((readiness (mevedel-execution-target-readiness target))
           (status (or (plist-get readiness :status) 'not-probed))
           (sandbox (plist-get readiness :sandbox-status)))
      (mevedel-report-fields
       (list "Target" (mevedel-execution-target-label target))
       (list "Support tier" (mevedel-execution-target-support-tier target))
       (list "Readiness" status)
       (when sandbox (list "Sandbox" sandbox))))))

(defun mevedel-menu--durability-description (session)
  "Return SESSION's persistence, lease, and publication info-panel rows."
  (when (and session (mevedel-session-workspace session))
    (let* ((target (mevedel-session-execution-target session))
           (status (mevedel-session-publication-status session))
           (path (plist-get status :authoritative-state-path))
           (lease (or (plist-get status :lease-state)
                      (if (and target (mevedel-execution-target-remote-p target)) 'none 'local))))
      (mevedel-report-fields
       (list "State directory" (if target (mevedel-execution-target-native-path target path) path))
       (list "Lease" lease)
       (list "Publication" (if (plist-get status :pending-publication) "pending" "published"))))))

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
  (when-let* ((data (mevedel-cockpit-context-data-buffer
                    (mevedel-menu--context))))
    (buffer-local-value 'mevedel-session--read-only-mode data)))

(defun mevedel-menu--transfer-pending-p ()
  "Return non-nil when another client is waiting for this session's lease."
  (when-let* ((session (mevedel-cockpit-context-session
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
  (if (mevedel-menu--read-only-p)
      (mevedel-menu--state-description "Control" "read-only" 'warning)
    (mevedel-menu--state-description "Control" "writable" 'transient-inactive-value)))

(defun mevedel-menu--share-state ()
  "Return the current session's sharing summary, or nil when it is not shared.
Read from memory only: rendering the menu never loads collaboration,
connects, or touches a bearer link."
  (when (featurep 'mevedel-collaboration)
    (mevedel-collaboration-room-summary
     (mevedel-cockpit-context-data-buffer (mevedel-menu--context)))))

(defun mevedel-menu--lobby-live-p ()
  "Return non-nil while the current workspace's lobby is shared."
  (and (featurep 'mevedel-collaboration-lobby)
       (mevedel-collaboration-lobby-live-p
        (mevedel-cockpit-context-workspace (mevedel-menu--context)))))

(defun mevedel-menu--share-label (summary)
  "Return the sharing state label for room SUMMARY."
  (if summary
      (let ((guests (plist-get summary :guests)))
        (format "shared · %d guest%s" guests (if (= guests 1) "" "s")))
    "not shared"))

(defun mevedel-menu--share-summary ()
  "Return the top-level Share row description."
  (let ((summary (mevedel-menu--share-state))
        (elsewhere (length (mevedel-menu--shares-elsewhere))))
    (mevedel-menu--state-description
     "Share…"
     (concat (mevedel-menu--share-label summary)
             (if (> elsewhere 0) (format " · +%d more" elsewhere) ""))
     (unless (or summary (> elsewhere 0)) 'transient-inactive-value))))

(defun mevedel-menu--shares ()
  "Return every active share and lobby of this Emacs.
Read from memory only, like `mevedel-menu--share-state': before
collaboration loads there is nothing to list."
  (and (featurep 'mevedel-collaboration)
       (mevedel-collaboration-shares)))

(defun mevedel-menu--share-here-p (entry context)
  "Return non-nil when share ENTRY is CONTEXT's session or workspace lobby."
  (if (eq (plist-get entry :kind) 'room)
      (eq (plist-get entry :data-buffer)
          (mevedel-cockpit-context-data-buffer context))
    (when-let* ((workspace (mevedel-cockpit-context-workspace context)))
      (equal (plist-get entry :directory)
             (mevedel-workspace-root workspace)))))

(defun mevedel-menu--shares-elsewhere ()
  "Return the active shares other than this session's and workspace's."
  (let ((context (mevedel-menu--context)))
    (seq-remove (lambda (entry) (mevedel-menu--share-here-p entry context))
                (mevedel-menu--shares))))

(defun mevedel-menu--no-shares-p ()
  "Return non-nil when no share or lobby is active."
  (null (mevedel-menu--shares)))

(defconst mevedel-menu--share-row-limit 9
  "Most shares the Share surface lists, one per digit key.")

(defvar mevedel-menu--share-entries nil
  "Shares listed by the open Share surface, in digit-key order.")

(defun mevedel-menu--share-row-description (entry here)
  "Return the Share surface row for share ENTRY.
HERE says ENTRY belongs to the menu's own session or workspace."
  (let* ((kind (plist-get entry :kind))
         (guests (or (plist-get entry :guests) 0))
         (row
          (format "%s %-5s %-32s %s%s%s"
                  (if (eq kind 'recorded-lobby) "○" "●")
                  (if (eq kind 'room) "room" "lobby")
                  (truncate-string-to-width
                   (or (plist-get entry :label) "?") 32 nil nil "…")
                  (if (eq kind 'recorded-lobby)
                      "recorded, not running"
                    (format "%d %s%s" guests
                            (if (eq kind 'room) "guest" "tab")
                            (if (= guests 1) "" "s")))
                  (if (or (eq kind 'recorded-lobby)
                          (plist-get entry :connected))
                      ""
                    " · reconnecting")
                  (cond ((not here) "")
                        ((eq kind 'room) " · this session")
                        (t " · this workspace")))))
    (if (eq kind 'recorded-lobby)
        (mevedel-menu--face row 'transient-inactive-value)
      row)))

(defun mevedel-menu--share-active-description ()
  "Return the heading of the Share surface's active-share rows."
  (let ((count (length (mevedel-menu--shares))))
    (if (> count mevedel-menu--share-row-limit)
        (format "Active everywhere (first %d of %d; /collab status lists all)"
                mevedel-menu--share-row-limit count)
      "Active everywhere")))

(defun mevedel-menu--share-active-rows (_children)
  "Return one digit-keyed suffix per active share, this session's first."
  (let* ((context (mevedel-menu--context))
         (shares (mevedel-menu--shares))
         (entries
          (seq-take
           (append
            (seq-filter (lambda (entry) (mevedel-menu--share-here-p entry context))
                        shares)
            (seq-remove (lambda (entry) (mevedel-menu--share-here-p entry context))
                        shares))
           mevedel-menu--share-row-limit))
         (index 0))
    (setq mevedel-menu--share-entries entries)
    (transient-parse-suffixes
     'mevedel-menu--share
     (mapcar (lambda (entry)
               (setq index (1+ index))
               (list (number-to-string index)
                     (mevedel-menu--share-row-description
                      entry (mevedel-menu--share-here-p entry context))
                     'mevedel-menu--share-select))
             entries))))

(defun mevedel-menu--share-select ()
  "Open the surface of the share whose digit key was pressed."
  (interactive)
  (let ((entry (and (characterp last-command-event)
                    (<= ?1 last-command-event ?9)
                    (nth (- last-command-event ?1)
                         mevedel-menu--share-entries))))
    (unless entry
      (user-error "No share on that key"))
    (transient-setup 'mevedel-menu--share-item nil nil :scope entry)))

(defun mevedel-menu--share-item-room-p ()
  "Return non-nil when the share surface shows a session room."
  (eq 'room (plist-get (transient-scope) :kind)))

(defun mevedel-menu--share-item-recorded-p ()
  "Return non-nil when the share surface shows a lobby that is not running."
  (eq 'recorded-lobby (plist-get (transient-scope) :kind)))

(defun mevedel-menu--share-item-description ()
  "Return the header of one share's surface."
  (let* ((entry (transient-scope))
         (kind (plist-get entry :kind))
         (guests (or (plist-get entry :guests) 0)))
    (concat
     (mevedel-menu--face (if (eq kind 'room) "room" "lobby") 'transient-heading)
     " "
     (mevedel-menu--value (or (plist-get entry :label) "?"))
     (if (eq kind 'room)
         ""
       (concat " " (abbreviate-file-name (plist-get entry :directory))))
     "\n"
     (mevedel-menu--face
      (if (eq kind 'recorded-lobby)
          "Recorded to run, but not running: it failed to restart or was never started here."
        (format "%d %s%s connected · relay %s"
                guests (if (eq kind 'room) "guest" "tab")
                (if (= guests 1) "" "s")
                (if (plist-get entry :connected) "connected" "reconnecting")))
      'transient-inactive-value))))

(defun mevedel-menu--share-item-stop-description ()
  "Return the stop action's label for the shown share."
  (pcase (plist-get (transient-scope) :kind)
    ('room "Stop sharing")
    ('lobby "Stop the lobby")
    (_ "Forget it (no restart)")))

(defun mevedel-menu--share-item-open ()
  "Show the shared session."
  (interactive)
  (let ((buffer (plist-get (transient-scope) :data-buffer)))
    (unless (buffer-live-p buffer)
      (user-error "That session is no longer open"))
    (mevedel--display-chat-buffer buffer)))

(defun mevedel-menu--share-item-links ()
  "Show the shown share's links, restarting a recorded lobby first."
  (interactive)
  (mevedel-collaboration-show-share (transient-scope)))

(defun mevedel-menu--share-item-stop ()
  "Stop the shown share, or forget a recorded lobby."
  (interactive)
  (mevedel-collaboration-stop-share (transient-scope)))

(defun mevedel-menu--share-stop-all ()
  "Stop every share and lobby after confirmation."
  (interactive)
  (let* ((entries (mevedel-menu--shares))
         (rooms (seq-count (lambda (entry) (eq 'room (plist-get entry :kind)))
                           entries))
         (lobbies (- (length entries) rooms)))
    (if (yes-or-no-p
         (format (concat "Stop %d share%s and %d lobb%s? Their guests are "
                         "disconnected, and the session links stop working. ")
                 rooms (if (= rooms 1) "" "s")
                 lobbies (if (= lobbies 1) "y" "ies")))
        (mevedel-collaboration-stop-all)
      (message "mevedel: shares kept"))))

(defun mevedel-menu--share-description ()
  "Return the Share surface header."
  (let ((summary (mevedel-menu--share-state))
        (lobby (mevedel-menu--lobby-live-p)))
    (concat
     (mevedel-menu--face "share" 'transient-heading)
     " session "
     (mevedel-menu--value (mevedel-menu--share-label summary)
                          (unless summary 'transient-inactive-value))
     " · lobby "
     (mevedel-menu--value (if lobby "shared" "not shared")
                          (unless lobby 'transient-inactive-value))
     "\n"
     (mevedel-menu--face
      "Links are bearer credentials; their panel shows a QR code and copies them."
      'transient-inactive-value))))

(defun mevedel-menu--not-shared-p ()
  "Return non-nil when the current session is not shared."
  (not (mevedel-menu--share-state)))

(defun mevedel-menu--lobby-down-p ()
  "Return non-nil when the current workspace's lobby is not shared."
  (not (mevedel-menu--lobby-live-p)))

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

(defun mevedel-menu--session-info-report ()
  "Return the complete session state as a structured information report."
  (let* ((context (mevedel-menu--context))
         (data-buffer (mevedel-cockpit-context-data-buffer context))
         (session (mevedel-cockpit-context-session context))
         (workspace (mevedel-cockpit-context-workspace context))
         (target (and session (mevedel-session-execution-target session)))
         (mode (mevedel-menu--mode-symbol
                session data-buffer (mevedel-cockpit-context-view-buffer context))))
    (list
     :title "Session info"
     :subtitle (or (and session (mevedel-session-name session)) "unknown")
     :identity session :refresh #'mevedel-menu--session-info-report
     :sections
     (list
      (list :id 'request :title "Request"
            :body (mevedel-report-fields
                   (list "State" (mevedel-request-state-label data-buffer))
                   (list "Mode" (mevedel-menu--mode-label mode))
                   (list "Preset" (or (and session (mevedel-session-preset-name session)) "none"))))
      (list :id 'model :title "Model"
            :body (mevedel-report-fields
                   (list "Provider / model" (mevedel-model-current-provider-label data-buffer))
                   (list "Effort" (or (with-current-buffer data-buffer
                                        (and (boundp 'gptel-reasoning-effort) gptel-reasoning-effort))
                                      "default"))))
      (list :id 'target :title "Workspace & target"
            :body (concat (mevedel-report-fields
                           (list "Workspace" (mevedel-menu--root-label workspace target)))
                          (mevedel-menu--target-description session)))
      (list :id 'persistence :title "Persistence"
            :body (or (mevedel-menu--durability-description session) "No persistence state."))))))

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
  (let ((model (mevedel-menu--fit (mevedel-menu--model-label))))
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
         (next (car (or (cdr (member current efforts)) efforts))))
    (format "%-13s %s → %s"
            "Cycle effort"
            (mevedel-menu--value (or current "default"))
            (mevedel-menu--inactive-value (or next "default")))))

(defun mevedel-menu--memory-description ()
  "Return cached memory alerts without reading target files during rendering."
  (let* ((workspace (mevedel-cockpit-context-workspace (mevedel-menu--context)))
         (counts (mevedel-workspace-memory-observation workspace))
         (labels (cl-loop for (key label) in '((:pending "pending") (:recovery "recovery") (:unavailable "unavailable"))
                          for count = (or (plist-get counts key) 0)
                          when (> count 0) collect (format "%d %s" count label))))
    (mevedel-menu--state-description
     "Memory"
     (cond ((null counts) "not checked")
           ((>= (- (float-time) (plist-get counts :at)) 10)
            (concat (if labels (string-join labels ", ") "no alerts") " (cached)"))
           (labels (string-join labels ", "))
           (t "no alerts"))
     (cond ((or (> (or (plist-get counts :recovery) 0) 0)
                (> (or (plist-get counts :unavailable) 0) 0))
            'warning)
           ;; Nothing to act on is not worth the reader's attention.
           ((null labels) 'transient-inactive-value)))))

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

(defun mevedel-menu--artifacts-description ()
  "Return the top-level artifacts row description.
The value reads ATTACHED/IN-PROJECT: artifacts attached to this session,
out of the project's store."
  (let* ((session (mevedel-cockpit-context-session
                   (mevedel-menu--context)))
         (counts (mevedel-artifacts-list-count session)))
    (mevedel-menu--state-description
     "Artifacts"
     (format "%d/%d" (car counts) (cdr counts))
     (if (> (car counts) 0) 'warning 'transient-inactive-value))))

(defun mevedel-menu--skills-description ()
  "Return the top-level skills row description."
  (let ((context (mevedel-menu--context)))
    (mevedel-menu--state-description
     "Skills"
     (mevedel-skills-count-label (mevedel-cockpit-context-session context))
     'warning)))

(defun mevedel-menu--plugins-description ()
  "Return the top-level plugins row description."
  (let* ((workspace (mevedel-cockpit-context-workspace
                     (mevedel-menu--context)))
         (label (mevedel-plugins-count-label workspace)))
    (mevedel-menu--state-description
     "Plugins" label
     (if (string-suffix-p "/0" label) 'transient-inactive-value 'warning))))

(defun mevedel-menu--worktree-description ()
  "Return the top-level worktree row description."
  (let ((worktree (mevedel-menu--fit (mevedel-menu--worktree-label))))
    (format "%-10s %s"
            "Worktree"
            (if (string= worktree "not-git")
                (mevedel-menu--inactive-value worktree)
              (mevedel-menu--value worktree)))))

(defun mevedel-menu--preset-summary ()
  "Return the top-level preset row description."
  (let ((preset (mevedel-session-preset-name
                 (mevedel-cockpit-context-session (mevedel-menu--context)))))
    (if preset
        (mevedel-menu--state-description "Preset" preset)
      (mevedel-menu--state-description "Preset" "none" 'transient-inactive-value))))

(defun mevedel-menu--goal-summary ()
  "Return the top-level Goal row description."
  (if-let* ((goal (mevedel-menu--current-goal)))
      (mevedel-menu--state-description
       "Goal" (format "%s, %d turns" (mevedel-goal-status goal)
                      (mevedel-goal-turns-run goal)))
    (mevedel-menu--state-description "Goal" "none" 'transient-inactive-value)))

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
  (let ((budget (mevedel-goal-token-budget goal))
        (used (if (mevedel-goal-tokens-incomplete-p goal)
                  (format "at least %d (incomplete)" (mevedel-goal-tokens-used goal))
                (number-to-string (mevedel-goal-tokens-used goal)))))
    (if budget
        (format "%s/%d tokens" used budget)
      (format "%s tokens · unbounded" used))))

(defun mevedel-menu--goal-description ()
  "Return the one-line Goal cockpit status.
The full record lives in the Goal info panel."
  (if-let* ((goal (mevedel-menu--current-goal)))
      (let ((tail
             (concat
              " · "
              (mevedel-menu--value (format "%s" (mevedel-goal-status goal))
                                   (if (eq (mevedel-goal-status goal) 'blocked)
                                       'warning
                                     'transient-value))
              (format " · %d turns · " (mevedel-goal-turns-run goal))
              (mevedel-menu--value (mevedel-menu--goal-budget-label goal)))))
        (concat
         (mevedel-menu--face "Goal" 'transient-heading)
         "  "
         (mevedel-menu--value
          (truncate-string-to-width
           (string-clean-whitespace (mevedel-goal-objective goal))
           ;; Keep status and accounting visible; 6 = "Goal  ".
           (max 20 (- (frame-width) 6 (string-width tail) 1))
           nil nil "…"))
         tail))
    (concat
     (mevedel-menu--face "Goal" 'transient-heading)
     "  "
     (mevedel-menu--inactive-value
      "none — s starts one, or /goal OBJECTIVE"))))

(defun mevedel-menu--goal-record-report ()
  "Return the complete Goal as an information report."
  (let ((goal (mevedel-menu--current-goal)))
    (list :title "Goal" :identity goal :refresh #'mevedel-menu--goal-record-report
          :sections
          (if (not goal)
              (list (list :id 'goal :title "Goal"
                          :body "No active Goal. Start one here or with /goal OBJECTIVE."))
            (list
             (list :id 'objective :title "Objective" :body (mevedel-goal-objective goal))
             (list :id 'progress :title "Progress"
                   :body (mevedel-report-fields
                          (list "Status" (format "%s%s" (mevedel-goal-status goal)
                                                 (if-let* ((reason (mevedel-goal-reason goal))) (format " — %s" reason) "")))
                          (list "Budget" (mevedel-menu--goal-budget-label goal))
                          (list "Turns" (format "%d · elapsed %ds" (mevedel-goal-turns-run goal) (mevedel-goal-time-used-seconds goal)))))
             (list :id 'plan :title "Accepted plan"
                   :body (mevedel-report-fields (list "Plan" (or (mevedel-goal-plan-reference goal) "none")))))))))

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

(defun mevedel-menu--preset-report ()
  "Return resolved preset policy as an information report."
  (let* ((context (mevedel-menu--context))
         (session (mevedel-cockpit-context-session context))
         (policies (mevedel-menu--preset-policies)))
    (list :title "Preset"
          :subtitle (format "%s" (or (and session (mevedel-session-preset-name session)) "none"))
          :identity session :refresh #'mevedel-menu--preset-report
          :sections
          (mapcar
           (lambda (kind)
             (list :id kind :title (if (eq kind 'tier) "Tiers" "Workloads")
                   :body (mapconcat
                          (lambda (policy)
                            (mevedel-report-fields
                             (list (nth 1 policy)
                                   (or (nth 2 policy) (propertize (format "ERROR: %s" (nth 3 policy)) 'face 'error)))))
                          (seq-filter (lambda (policy) (eq (car policy) kind)) policies) "")))
           '(tier workload)))))

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
   'edits "automatic edits and confined execution"))

(defun mevedel-menu--mode-full-auto-description ()
  "Return the full-auto mode row description."
  (mevedel-menu--mode-choice-description 'full-auto "Full Access, no confinement or prompts"))

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
  (mevedel-turn-busy-p
   (mevedel-cockpit-context-data-buffer (mevedel-menu--context))))

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

(defun mevedel-menu--model-selection-select-model ()
  "Select a model for the caller-owned model-selection scope."
  (interactive)
  (let ((candidates (mevedel-model-candidates)))
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
         (effort (car (or (cdr (member current efforts)) efforts))))
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
AREA is `top' for the main cockpit, or a named cockpit surface.
A cockpit waits for keys in Emacs, so it refuses where nobody can be
asked, such as a collaboration guest's turn."
  (interactive (list 'top))
  (when inhibit-interaction
    (signal 'inhibited-interaction (list "The cockpit needs Emacs")))
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
      ('memory
       (mevedel-cockpit-call-in-data context #'mevedel-memory-list-open context))
      ('model
       (mevedel-menu--open-model))
      ('goal
       (transient-setup 'mevedel-menu--goal))
      ('control
       (transient-setup 'mevedel-menu--control))
      ('share
       (transient-setup 'mevedel-menu--share))
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
      ('artifacts
       (mevedel-cockpit-call-in-data
        context #'mevedel-artifacts-list-open context))
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
  (mevedel-menu--call-in-view #'mevedel-view-send))

(defun mevedel-menu--abort ()
  "Abort the active request from the view buffer."
  (interactive)
  (mevedel-menu--call-in-view #'mevedel-view-abort))

(defun mevedel-menu--take-control ()
  "Take control of this session from the view buffer."
  (interactive)
  (mevedel-menu--call-in-view #'mevedel-take-control))

(defun mevedel-menu--release-control ()
  "Release this session's lease from the view buffer."
  (interactive)
  (mevedel-menu--call-in-view #'mevedel-release-control))

(defun mevedel-menu--grant-control ()
  "Grant the pending control-transfer request from the view buffer."
  (interactive)
  (mevedel-menu--call-in-view #'mevedel-view-control-transfer-grant))

(defun mevedel-menu--keep-control ()
  "Decline the pending control-transfer request from the view buffer."
  (interactive)
  (mevedel-menu--call-in-view #'mevedel-view-control-transfer-keep))

(defun mevedel-menu--toggle-follow ()
  "Toggle published-state following from the view buffer."
  (interactive)
  (mevedel-menu--call-in-view #'mevedel-toggle-follow))

(defun mevedel-menu--refresh-session ()
  "Re-read the owner's newest published state from the view buffer."
  (interactive)
  (mevedel-menu--call-in-view #'mevedel-refresh-session))

(defun mevedel-menu--rewind-here ()
  "Rewind keeping the settled assistant turn at point in the view buffer."
  (interactive)
  (mevedel-menu--call-in-view #'mevedel-view-rewind-at-point))

(defun mevedel-menu--redo ()
  "Restore the session to one of its published heads."
  (interactive)
  (mevedel-menu--call-live-tip-data #'mevedel-redo))

(defun mevedel-menu--redo-inapt-p ()
  "Return non-nil when this session publishes no restorable head.

The recorded authority mode is read directly: offering the entry must
not be the step that rejects a session, and a file session simply has
nothing to restore."
  (let ((session (mevedel-cockpit-context-session (mevedel-menu--context))))
    (not (and session
              (eq (mevedel-session-authority-mode session) 'portable)))))

(defun mevedel-menu--switch-variant-here ()
  "Switch conversation variants at point in the paired view buffer."
  (interactive)
  (mevedel-menu--call-in-view #'mevedel-view-switch-conversation-variant-at-point))

(defun mevedel-menu--fork-conversation-here ()
  "Arm a Conversation Fork at point in the paired view buffer."
  (interactive)
  (mevedel-menu--call-in-view #'mevedel-view-arm-conversation-fork))

(defun mevedel-menu--fork-worktree-here ()
  "Arm a Worktree Fork at point in the paired view buffer."
  (interactive)
  (mevedel-menu--call-in-view #'mevedel-view-arm-worktree-fork))

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

(defun mevedel-menu--open-memory ()
  "Open the workspace memory proposals cockpit."
  (interactive)
  (mevedel-menu-open 'memory))

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

(defun mevedel-menu--open-artifacts ()
  "Open the artifacts cockpit surface."
  (interactive)
  (mevedel-menu-open 'artifacts))

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

(defun mevedel-menu--open-share ()
  "Open the Share cockpit surface."
  (interactive)
  (mevedel-menu-open 'share))

(defun mevedel-menu--share-session ()
  "Show this session's share links, first sharing it after confirmation."
  (interactive)
  (mevedel-menu--call-in-view #'mevedel-collaboration-view))

(defun mevedel-menu--share-stop ()
  "Stop sharing this session, leaving every other share running."
  (interactive)
  (mevedel-collaboration-stop-session
   (mevedel-cockpit-context-data-buffer (mevedel-menu--context))))

(defun mevedel-menu--share-status ()
  "Report every live share and lobby without exposing their links."
  (interactive)
  (mevedel-collaboration-status))

(defun mevedel-menu--share-lobby ()
  "Show this workspace's lobby links, first sharing it after confirmation."
  (interactive)
  (mevedel-cockpit-call-in-data (mevedel-menu--context) #'mevedel-collaboration-lobby))

(defun mevedel-menu--share-lobby-stop ()
  "Stop this workspace's lobby."
  (interactive)
  (mevedel-cockpit-call-in-data (mevedel-menu--context)
                                #'mevedel-collaboration-lobby-stop))

(defun mevedel-menu--share-lobby-rotate ()
  "Replace this workspace's lobby links after confirmation."
  (interactive)
  (if (yes-or-no-p (concat "Replace the lobby links? Every earlier lobby link "
                           "stops working, including ones already handed out. "))
      (mevedel-cockpit-call-in-data (mevedel-menu--context)
                                    #'mevedel-collaboration-lobby-rotate)
    (message "mevedel: lobby links kept")))

(defun mevedel-menu--open-session-info ()
  "Open the session info panel."
  (interactive)
  (mevedel-cockpit-show-help
   mevedel-menu-session-info-buffer-name
   (mevedel-menu--session-info-report)))

(defun mevedel-menu--open-preset-report ()
  "Open the resolved preset policy info panel."
  (interactive)
  (mevedel-cockpit-show-help
   mevedel-menu-preset-report-buffer-name
   (mevedel-menu--preset-report)))

(defun mevedel-menu--open-goal-record ()
  "Open the Goal record info panel."
  (interactive)
  (mevedel-cockpit-show-help
   mevedel-menu-goal-record-buffer-name
   (mevedel-menu--goal-record-report)))

(defun mevedel-menu-help--report ()
  "Return complete command discovery as an information report."
  (list :title "mevedel help"
        :sections (list (list :id 'help-0 :title "Session cockpit"
			      :body (string-join (list "The transient menu is the live key reference for session commands.") "\n"))
			(list :id 'help-1 :title "Slash commands that open UI"
			      :body (string-join (list "/plugin, /plugin list       Plugins" "/skills, /skills list       Skills" "/mode                       Mode" "/model                      Model" "Cockpit G / P               Goal / Preset model team" "Cockpit u                   Remembered permission authority" "Cockpit A                   Artifacts (attached/in project)" "Cockpit n                   New whiteboard or document" "/collab, Cockpit S          Share links, status, lobby" "Cockpit i                   Session info panel" "/usage, Cockpit U           Subscription usage" "/tools, /tools list         Tools" "/ps                         Live executions" "/stop [EXECUTION_ID]        Stop one execution, or all when omitted" "/worktree, /worktree status Worktree" "/help                       Help") "\n"))
			(list :id 'help-2 :title "Direct slash commands"
			      :body (string-join (list "/plugin enable NAME, disable NAME, reload, update NAME" "/plugin install TARGET, remove NAME, uninstall NAME, hooks ..." "/skills enable NAME, disable NAME, help NAME" "/mode MODE, /model MODEL" "/worktree create [NAME] [--for \"purpose\"] [--clean]" "/goal OBJECTIVE, /goal budget N|none, /goal edit|pause|resume|clear" "/compact, /remember [focus], /review, /verify, /edits, /clear, /init ..., /tokens") "\n"))
			(list :id 'help-3 :title "Modes"
			      :body (string-join (list "ask       Prompt for edits and uncertain execution." "edits     Automatic native edits and confined Bash/batch Eval." "full-auto Full Access: no confinement or permission prompts.") "\n"))
			(list :id 'help-4 :title "View and data buffers"
			      :body (string-join (list "The view buffer owns the composer, compact transcript, and status strip." "The data buffer owns raw gptel state, tools, model, and transcript data." "The cockpit resolves the view/data pair once and routes actions to the owning buffer." "Cockpit N opens Navigate: [ / ] / g inspect session segments," "n / p move through displays, C-n / C-p through queries, TAB folds a section." "The raw data buffer keeps gptel header behavior for the gptel menu.") "\n")))))

(defun mevedel-menu-help-open ()
  "Open the session cockpit help surface."
  (mevedel-cockpit-show-help mevedel-menu-help-buffer-name (mevedel-menu-help--report)))

(defun mevedel-menu--open-gptel ()
  "Open the gptel bridge surface."
  (interactive)
  (mevedel-gptel-bridge-open (mevedel-menu--context)))


;;
;;; Transient

;;;; Top menu layout

(defconst mevedel-menu--layout-widths '((wide . 170) (medium . 100))
  "Smallest frame widths, in columns, of the wider top menu layouts.
Wide shows the six groups side by side, medium in two rows of three,
and anything narrower in three rows of two.")

(defconst mevedel-menu--footer-width 72
  "Smallest frame width, in columns, that fits the utilities on one row.")

(defun mevedel-menu--layout ()
  "Return the top menu layout for the selected frame.
The value is `wide', `medium' or `narrow'; see
`mevedel-menu--layout-widths'."
  (let ((width (frame-width)))
    (cond ((>= width (alist-get 'wide mevedel-menu--layout-widths)) 'wide)
          ((>= width (alist-get 'medium mevedel-menu--layout-widths)) 'medium)
          (t 'narrow))))

(defun mevedel-menu--wide-p ()
  "Return non-nil when the top menu shows its groups side by side."
  (eq (mevedel-menu--layout) 'wide))

(defun mevedel-menu--medium-p ()
  "Return non-nil when the top menu shows two rows of three groups."
  (eq (mevedel-menu--layout) 'medium))

(defun mevedel-menu--narrow-p ()
  "Return non-nil when the top menu shows three rows of two groups."
  (eq (mevedel-menu--layout) 'narrow))

(defun mevedel-menu--split-footer-p ()
  "Return non-nil when the utilities need two rows."
  (< (frame-width) mevedel-menu--footer-width))

;; Each group is defined once and arranged by the width-dependent layouts
;; below; only one layout is active, so every key is bound exactly once.

(transient-define-group mevedel-menu--conversation-group
  ["Conversation"
   :pad-keys t
   ("RET" "Send" mevedel-menu--send
    :inapt-if mevedel-menu--send-inapt-p)
   ("a" "Abort" mevedel-menu--abort
    :inapt-if mevedel-menu--abort-inapt-p)
   ("c" "Compact" mevedel-menu--compact)
   ("r" "Review" mevedel-menu--review)
   ("v" "Verify" mevedel-menu--verify)])

(transient-define-group mevedel-menu--history-group
  ["History"
   :pad-keys t
   ("N" "Navigate…" mevedel-menu--open-navigate)
   ("f" "Fork conversation" mevedel-menu--fork-conversation-here)
   ("F" "Fork worktree" mevedel-menu--fork-worktree-here)
   ("R" "Rewind here" mevedel-menu--rewind-here)
   ("D" "Redo…" mevedel-menu--redo
    :inapt-if mevedel-menu--redo-inapt-p)
   ("B" "Switch variant" mevedel-menu--switch-variant-here)])

(transient-define-group mevedel-menu--configure-group
  ["Configure"
   :pad-keys t
   ("m" mevedel-menu--open-mode
    :description mevedel-menu--mode-description)
   ("M" mevedel-menu--open-model
    :description mevedel-menu--model-description)
   ("P" mevedel-menu--open-preset
    :description mevedel-menu--preset-summary)
   ("u" "Permissions" mevedel-menu--open-permissions)
   ("G" mevedel-menu--open-goal
    :description mevedel-menu--goal-summary)])

(transient-define-group mevedel-menu--workspace-group
  ["Workspace"
   :pad-keys t
   ("x" mevedel-menu--open-executions
    :description mevedel-menu--executions-description)
   ("w" mevedel-menu--open-worktree
    :description mevedel-menu--worktree-description)
   ("A" mevedel-menu--open-artifacts
    :description mevedel-menu--artifacts-description)
   ("l" mevedel-menu--open-memory
    :description mevedel-menu--memory-description)])

(transient-define-group mevedel-menu--collaborate-group
  ["Collaborate"
   :pad-keys t
   ("S" mevedel-menu--open-share
    :description mevedel-menu--share-summary)
   ("n" "New shared item" mevedel-artifacts-new-shared-item
    :inapt-if mevedel-menu--read-only-p)
   ("C" mevedel-menu--open-control
    :description mevedel-menu--control-summary)])

(transient-define-group mevedel-menu--extend-group
  ["Extend"
   :pad-keys t
   ("t" mevedel-menu--open-tools
    :description mevedel-menu--tools-description)
   ("s" mevedel-menu--open-skills
    :description mevedel-menu--skills-description)
   ("p" mevedel-menu--open-plugins
    :description mevedel-menu--plugins-description)])

(defconst mevedel-menu--utilities
  '(("d" "Data view" mevedel-menu--toggle-data-view)
    ("i" "Session info" mevedel-menu--open-session-info)
    ("U" "Usage" mevedel-subscription-usage-show)
    ("g" "gptel menu" mevedel-menu--open-gptel)
    ("?" "Help" mevedel-menu--open-help))
  "Utility suffixes of the top menu, shown in rows beneath its groups.")

(defun mevedel-menu--utility-suffixes (start &optional end)
  "Return `mevedel-menu--utilities' from START to END as parsed suffixes."
  (transient-parse-suffixes
   'mevedel-menu--top (seq-subseq mevedel-menu--utilities start end)))

(defun mevedel-menu--utilities-row (_children)
  "Return every utility suffix, for a single footer row."
  (mevedel-menu--utility-suffixes 0))

(defun mevedel-menu--utilities-first-row (_children)
  "Return the utilities of a split footer's first row."
  (mevedel-menu--utility-suffixes 0 3))

(defun mevedel-menu--utilities-second-row (_children)
  "Return the utilities of a split footer's second row."
  (mevedel-menu--utility-suffixes 3))

(transient-define-prefix mevedel-menu--top ()
  "Top-level mevedel session cockpit.
The groups are arranged by `mevedel-menu--layout'; utilities share a row."
  [:description mevedel-menu--header :if mevedel-menu--wide-p
   mevedel-menu--conversation-group mevedel-menu--history-group
   mevedel-menu--configure-group mevedel-menu--workspace-group
   mevedel-menu--collaborate-group mevedel-menu--extend-group]
  [:description mevedel-menu--header :if mevedel-menu--medium-p
   mevedel-menu--conversation-group mevedel-menu--history-group
   mevedel-menu--configure-group]
  [:if mevedel-menu--medium-p
   mevedel-menu--workspace-group mevedel-menu--collaborate-group
   mevedel-menu--extend-group]
  [:description mevedel-menu--header :if mevedel-menu--narrow-p
   mevedel-menu--conversation-group mevedel-menu--history-group]
  [:if mevedel-menu--narrow-p
   mevedel-menu--configure-group mevedel-menu--workspace-group]
  [:if mevedel-menu--narrow-p
   mevedel-menu--collaborate-group mevedel-menu--extend-group]
  [:class transient-row :if-not mevedel-menu--split-footer-p
   :setup-children mevedel-menu--utilities-row]
  [:class transient-row :if mevedel-menu--split-footer-p
   :setup-children mevedel-menu--utilities-first-row]
  [:class transient-row :if mevedel-menu--split-footer-p
   :setup-children mevedel-menu--utilities-second-row]
  (interactive)
  (mevedel-menu-open 'top))

(transient-define-prefix mevedel-menu--share ()
  "Share cockpit surface: this session's room and the workspace lobby.
Every action goes through the same commands as `/collab'."
  [:description mevedel-menu--share-description
   ["Session"
    :pad-keys t
    ("s" "Show links, or start sharing" mevedel-menu--share-session)
    ("x" "Stop sharing" mevedel-menu--share-stop
     :inapt-if mevedel-menu--not-shared-p)]
   ["Lobby"
    :pad-keys t
    ("l" "Show links, or start the lobby" mevedel-menu--share-lobby)
    ("L" "Stop the lobby" mevedel-menu--share-lobby-stop
     :inapt-if mevedel-menu--lobby-down-p)
    ("r" "Rotate lobby links…" mevedel-menu--share-lobby-rotate)]
   ["Inspect"
    :pad-keys t
    ("i" "Status" mevedel-menu--share-status)
    ("X" "Stop all…" mevedel-menu--share-stop-all
     :inapt-if mevedel-menu--no-shares-p)]]
  [:class transient-column
   :description mevedel-menu--share-active-description
   :if-not mevedel-menu--no-shares-p
   :setup-children mevedel-menu--share-active-rows]
  (interactive)
  (mevedel-menu--context)
  (transient-setup 'mevedel-menu--share))

(transient-define-prefix mevedel-menu--share-item ()
  "One share or lobby chosen on the Share surface.
Every action goes through the collaboration module, which refuses a share
that stopped meanwhile."
  [:description mevedel-menu--share-item-description
   ["Share"
    :pad-keys t
    ("o" "Open session" mevedel-menu--share-item-open
     :if mevedel-menu--share-item-room-p)
    ("s" "Show links" mevedel-menu--share-item-links
     :if-not mevedel-menu--share-item-recorded-p)
    ("R" "Restart and show links" mevedel-menu--share-item-links
     :if mevedel-menu--share-item-recorded-p)
    ("x" mevedel-menu--share-item-stop
     :description mevedel-menu--share-item-stop-description)]]
  (interactive)
  (user-error "Choose a share on the Share surface"))

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
