;;; niwa-menu.el -- Session cockpit transient -*- lexical-binding: t -*-

;;; Commentary:

;; Transient-backed session cockpit and shared model-selection surface.
;; The cockpit resolves the live view/data buffer pair once, then routes
;; commands to the buffer or caller that owns the relevant state.

;;; Code:

(require 'niwa-report)

(require 'transient)
(require 'niwa-cockpit)
(require 'niwa-models)

;; `gptel'
(declare-function gptel--model-name "ext:gptel" (model))
(declare-function gptel-backend-name "ext:gptel" (cl-x) t)
(defvar gptel-backend)
(defvar gptel-model)
(defvar gptel-reasoning-effort)

;; `niwa-artifacts-list'
(declare-function niwa-artifacts-list-count
                  "niwa-artifacts-list" (session))
(declare-function niwa-artifacts-list-open
                  "niwa-artifacts-list" (&optional context))
(autoload 'niwa-artifacts-list-count "niwa-artifacts-list")
(autoload 'niwa-artifacts-list-open "niwa-artifacts-list")
(autoload 'niwa-artifacts-new-shared-item "niwa-artifacts-list" nil t)

;; `niwa-background-list'
(declare-function niwa-background-list-open
                  "niwa-background-list" (&optional context))
(autoload 'niwa-background-list-open "niwa-background-list" nil t)

;; `niwa-schedules-list'
(autoload 'niwa-schedules-list-open "niwa-schedules-list" nil t)

;; `niwa-chat'
(declare-function niwa--display-chat-buffer "niwa-chat" (chat-buffer))
(autoload 'niwa--display-chat-buffer "niwa-chat")

;; `niwa-cockpit'
(declare-function niwa-cockpit-call-in-data
                  "niwa-cockpit" (context function &rest args))
(declare-function niwa-cockpit-call-in-view
                  "niwa-cockpit" (context function &rest args))
(declare-function niwa-cockpit-context-data-buffer
                  "niwa-cockpit" (&optional context))
(declare-function niwa-cockpit-context-origin-buffer
                  "niwa-cockpit" (&optional context))
(declare-function niwa-cockpit-context-session
                  "niwa-cockpit" (&optional context))
(declare-function niwa-cockpit-context-view-buffer
                  "niwa-cockpit" (&optional context))
(declare-function niwa-cockpit-context-workspace
                  "niwa-cockpit" (&optional context))
(declare-function niwa-cockpit-current-context
                  "niwa-cockpit" ())
(declare-function niwa-cockpit-show-help
                  "niwa-cockpit" (buffer text))

;; `niwa-collaboration'
(declare-function niwa-collaboration-room-summary
                  "niwa-collaboration" (data-buffer))
(declare-function niwa-collaboration-shares "niwa-collaboration" ())
(declare-function niwa-collaboration-show-share
                  "niwa-collaboration" (entry))
(declare-function niwa-collaboration-status "niwa-collaboration" ())
(declare-function niwa-collaboration-stop-all "niwa-collaboration" ())
(declare-function niwa-collaboration-stop-session
                  "niwa-collaboration" (data-buffer))
(declare-function niwa-collaboration-stop-share
                  "niwa-collaboration" (entry))
(declare-function niwa-collaboration-view "niwa-collaboration" ())
(autoload 'niwa-collaboration-status "niwa-collaboration" nil t)
(autoload 'niwa-collaboration-stop-session "niwa-collaboration")
(autoload 'niwa-collaboration-view "niwa-collaboration" nil t)

;; `niwa-collaboration-lobby'
(declare-function niwa-collaboration-lobby "niwa-collaboration-lobby" ())
(declare-function niwa-collaboration-lobby-live-p
                  "niwa-collaboration-lobby" (workspace))
(declare-function niwa-collaboration-lobby-rotate
                  "niwa-collaboration-lobby" ())
(declare-function niwa-collaboration-lobby-stop "niwa-collaboration-lobby" ())
(autoload 'niwa-collaboration-lobby "niwa-collaboration-lobby" nil t)
(autoload 'niwa-collaboration-lobby-rotate "niwa-collaboration-lobby" nil t)
(autoload 'niwa-collaboration-lobby-stop "niwa-collaboration-lobby" nil t)

;; `niwa-compact'
(declare-function niwa-compact "niwa-compact"
                  (&optional aggressive instructions))

;; `niwa-execution'
(declare-function niwa-execution-count-user "niwa-execution" (session))
(autoload 'niwa-execution-count-user "niwa-execution")

;; `niwa-execution-target'
(declare-function niwa-execution-target-label
                  "niwa-execution-target" (target &optional directory))
(declare-function niwa-execution-target-native-path
                  "niwa-execution-target" (target path))
(declare-function niwa-execution-target-native-root
                  "niwa-execution-target" (cl-x) t)
(declare-function niwa-execution-target-readiness
                  "niwa-execution-target" (cl-x) t)
(declare-function niwa-execution-target-remote-p
                  "niwa-execution-target" (target))
(declare-function niwa-execution-target-support-tier
                  "niwa-execution-target" (cl-x) t)
(autoload 'niwa-execution-target-label "niwa-execution-target")
(autoload 'niwa-execution-target-native-path "niwa-execution-target")
(autoload 'niwa-execution-target-native-root "niwa-execution-target")
(autoload 'niwa-execution-target-readiness "niwa-execution-target")
(autoload 'niwa-execution-target-remote-p "niwa-execution-target")
(autoload 'niwa-execution-target-support-tier "niwa-execution-target")

;; `niwa-goal'
(declare-function niwa-goal-clear "niwa-goal" ())
(declare-function niwa-goal-edit "niwa-goal" (objective))
(declare-function niwa-goal-pause "niwa-goal" ())
(declare-function niwa-goal-resume "niwa-goal" (&optional input))
(declare-function niwa-goal-set-budget "niwa-goal" (value))
(declare-function niwa-goal-start "niwa-goal"
                  (objective))

;; `niwa-gptel-bridge'
(declare-function niwa-gptel-bridge-open
                  "niwa-gptel-bridge" (&optional context))
(autoload 'niwa-gptel-bridge-open "niwa-gptel-bridge" nil t)

;; `niwa-memory-list'
(declare-function niwa-memory-list-open "niwa-memory-list" (&optional context))
(autoload 'niwa-memory-list-open "niwa-memory-list")

;; `niwa-models'
(declare-function niwa-model-current-label "niwa-models"
                  (&optional buffer))
(declare-function niwa-model-current-provider-label "niwa-models"
                  (&optional buffer))
(declare-function niwa-model-resolve-provider "niwa-models"
                  (spec &optional noerror))
(declare-function niwa-model-resolve-tier "niwa-models"
                  (tier &optional noerror))
(declare-function niwa-model-resolve-workload "niwa-models"
                  (workload &optional explicit-selector explicit-effort))
(declare-function niwa-model-set-session-effort "niwa-models"
                  (session effort &optional buffer))
(declare-function niwa-model-set-session-provider "niwa-models"
                  (session provider &optional buffer))
(declare-function niwa-model-candidates "niwa-models" ())
(declare-function niwa-model-supported-efforts "niwa-models" (model))
(defvar niwa-model-tiers)
(defvar niwa-model-workloads)

;; `niwa-permission-mode'
(declare-function niwa-permission-mode-effective "niwa-permission-mode"
                  (&optional session data-buffer surface-buffer))
(declare-function niwa-permission-mode-label "niwa-permission-mode"
                  (&optional mode))
(declare-function niwa-permission-mode-transition
                  "niwa-permission-mode" (mode))
(autoload 'niwa-permission-mode-effective "niwa-permission-mode")
(autoload 'niwa-permission-mode-label "niwa-permission-mode")
(autoload 'niwa-permission-mode-transition "niwa-permission-mode")
(defvar niwa-permission-mode)

;; `niwa-permissions-list'
(declare-function niwa-permissions-list-open "niwa-permissions-list"
                  (&optional context))
(autoload 'niwa-permissions-list-open "niwa-permissions-list")

;; `niwa-plan-mode'
(declare-function niwa-plan-approval-render
                  "niwa-plan-mode" (&optional session))
(declare-function niwa-plan-mode-enter
                  "niwa-plan-mode" (&optional session))
(autoload 'niwa-plan-approval-render "niwa-plan-mode")
(autoload 'niwa-plan-mode-enter "niwa-plan-mode" nil t)

;; `niwa-plugin-registry'
(declare-function niwa-plugins-count-label "niwa-plugin-registry"
                  (&optional workspace))
(autoload 'niwa-plugins-count-label "niwa-plugin-registry")

;; `niwa-plugin-ui'
(declare-function niwa-plugins-list-open "niwa-plugin-ui"
                  (&optional context))
(autoload 'niwa-plugins-list-open "niwa-plugin-ui")

;; `niwa-presets'
(declare-function niwa-preset-apply "niwa-presets" (name &optional buffer))
(defvar niwa-preset--registry)

;; `niwa-review'
(declare-function niwa-review "niwa-review" (&optional instructions))
(declare-function niwa-verify "niwa-review" (&optional instructions))

;; `niwa-session-control-transfer'
(declare-function niwa-session-control-transfer-drain-blocker
                  "niwa-session-control-transfer" (session))
(autoload 'niwa-session-control-transfer-drain-blocker
  "niwa-session-control-transfer")

;; `niwa-session-publication'
(declare-function niwa-session-publication-status
                  "niwa-session-publication" (session))
(autoload 'niwa-session-publication-status
  "niwa-session-publication")

;; `niwa-session-rewind'
(declare-function niwa-redo "niwa-session-rewind" ())
(autoload 'niwa-redo "niwa-session-rewind" nil t)

;; `niwa-skills-ui'
(declare-function niwa-skills-count-label "niwa-skills-ui" (session))
(declare-function niwa-skills-list-open "niwa-skills-ui"
                  (&optional context))
(autoload 'niwa-skills-count-label "niwa-skills-ui")
(autoload 'niwa-skills-list-open "niwa-skills-ui")

;; `niwa-structs'
(declare-function niwa-goal-objective "niwa-structs" (cl-x) t)
(declare-function niwa-goal-plan-reference "niwa-structs" (cl-x) t)
(declare-function niwa-goal-reason "niwa-structs" (cl-x) t)
(declare-function niwa-goal-status "niwa-structs" (cl-x) t)
(declare-function niwa-goal-time-used-seconds "niwa-structs" (cl-x) t)
(declare-function niwa-goal-token-budget "niwa-structs" (cl-x) t)
(declare-function niwa-goal-tokens-incomplete-p "niwa-structs" (cl-x) t)
(declare-function niwa-goal-tokens-used "niwa-structs" (cl-x) t)
(declare-function niwa-goal-turns-run "niwa-structs" (cl-x) t)
(declare-function niwa-session-authority-mode "niwa-structs" (cl-x) t)
(declare-function niwa-session-control-transfer
                  "niwa-structs" (cl-x) t)
(declare-function niwa-session-current-segment
                  "niwa-structs" (cl-x) t)
(declare-function niwa-session-execution-target
                  "niwa-structs" (cl-x) t)
(declare-function niwa-session-goal "niwa-structs" (cl-x) t)
(declare-function niwa-session-name "niwa-structs" (cl-x) t)
(declare-function niwa-session-pending-plan-approval
                  "niwa-structs" (cl-x) t)
(declare-function niwa-session-plan-mode "niwa-structs" (cl-x) t)
(declare-function niwa-session-preset-name "niwa-structs" (cl-x) t)
(declare-function niwa-session-workspace "niwa-structs" (cl-x) t)
(declare-function niwa-workspace-memory-observation "niwa-structs" (cl-x) t)
(declare-function niwa-workspace-root "niwa-structs" (cl-x) t)

;; `niwa-subscription-usage'
(autoload 'niwa-subscription-usage-show "niwa-subscription-usage" nil t)

;; `niwa-tools'
(declare-function niwa-tools-active-count "niwa-tools"
                  (&optional buffer))
(autoload 'niwa-tools-active-count "niwa-tools")

;; `niwa-tools-list'
(declare-function niwa-tools-list-open "niwa-tools-list"
                  (&optional context))
(autoload 'niwa-tools-list-open "niwa-tools-list")

;; `niwa-turn'
(declare-function niwa-request-active-p "niwa-turn"
                  (&optional buffer))
(declare-function niwa-request-state-label "niwa-turn"
                  (&optional buffer))
(declare-function niwa-turn-busy-p "niwa-turn" (&optional buffer))
(autoload 'niwa-request-active-p "niwa-turn")
(autoload 'niwa-request-state-label "niwa-turn")
(autoload 'niwa-turn-busy-p "niwa-turn")

;; `niwa-view-agent'
(declare-function niwa-view-agent-active-rows "niwa-view-agent" (session))
(autoload 'niwa-view-agent-active-rows "niwa-view-agent")

;; `niwa-view-composer'
(declare-function niwa-view--assert-live-tip
                  "niwa-view-composer" (&optional allow-armed-fork))
(declare-function niwa-view-abort "niwa-view-composer" ())
(declare-function niwa-view-arm-conversation-fork
                  "niwa-view-composer" ())
(declare-function niwa-view-arm-worktree-fork
		  "niwa-view-composer" ())
(declare-function niwa-view-send "niwa-view-composer" ())

;; `niwa-view-disclosure'
(declare-function niwa-view-toggle-section "niwa-view-disclosure" ())

;; `niwa-view-control-transfer'
(declare-function niwa-refresh-session "niwa-view-control-transfer" ())
(declare-function niwa-release-control "niwa-view-control-transfer" ())
(declare-function niwa-take-control "niwa-view-control-transfer" ())
(declare-function niwa-toggle-follow "niwa-view-control-transfer" ())
(declare-function niwa-view-control-transfer-grant
                  "niwa-view-control-transfer" ())
(declare-function niwa-view-control-transfer-keep
                  "niwa-view-control-transfer" ())
(autoload 'niwa-refresh-session "niwa-view-control-transfer" nil t)
(autoload 'niwa-release-control "niwa-view-control-transfer" nil t)
(autoload 'niwa-take-control "niwa-view-control-transfer" nil t)
(autoload 'niwa-toggle-follow "niwa-view-control-transfer" nil t)
(autoload 'niwa-view-control-transfer-grant
  "niwa-view-control-transfer" nil t)
(autoload 'niwa-view-control-transfer-keep
  "niwa-view-control-transfer" nil t)
(defvar niwa-session--read-only-mode)
(defvar niwa-session-follow-published)

;; `niwa-view-render'
(declare-function niwa-view-next-display "niwa-view-render" ())
(declare-function niwa-view-next-user-query "niwa-view-render" ())
(declare-function niwa-view-previous-display "niwa-view-render" ())
(declare-function niwa-view-previous-user-query "niwa-view-render" ())
(declare-function niwa-view-rewind-at-point "niwa-view-render" ())
(declare-function niwa-view-switch-conversation-variant-at-point
                  "niwa-view-render" ())

;; `niwa-view-segments'
(declare-function niwa-view-go-to-chat "niwa-view-segments"
                  (&optional number))
(declare-function niwa-view-go-to-segment "niwa-view-segments"
                  (&optional number))
(declare-function niwa-view-next-segment "niwa-view-segments" ())
(declare-function niwa-view-previous-segment "niwa-view-segments" ())
(declare-function niwa-view-segments-current-number
                  "niwa-view-segments" ())
(autoload 'niwa-view-go-to-chat "niwa-view-segments" nil t)
(autoload 'niwa-view-go-to-segment "niwa-view-segments" nil t)
(autoload 'niwa-view-next-segment "niwa-view-segments" nil t)
(autoload 'niwa-view-previous-segment "niwa-view-segments" nil t)
(autoload 'niwa-view-segments-current-number "niwa-view-segments")

;; `niwa-worktree'
(declare-function niwa-worktree-status-summary "niwa-worktree"
                  (&optional context))
(declare-function niwa-worktree-status-open "niwa-worktree" ())
(autoload 'niwa-worktree-status-open "niwa-worktree" nil t)
(autoload 'niwa-worktree-status-summary "niwa-worktree")

(defconst niwa-menu-help-buffer-name "*niwa help*"
  "Name of the session cockpit help buffer.")

(defconst niwa-menu-session-info-buffer-name "*niwa session info*"
  "Name of the session info panel buffer.")

(defconst niwa-menu-preset-report-buffer-name "*niwa preset*"
  "Name of the resolved preset policy info panel buffer.")

(defconst niwa-menu-goal-record-buffer-name "*niwa goal*"
  "Name of the Goal record info panel buffer.")

;;
;;; Context resolution

(defun niwa-menu--context ()
  "Return the current live cockpit context."
  (let ((context (niwa-cockpit-current-context)))
    (unless (and (niwa-cockpit-context-view-buffer context)
                 (niwa-cockpit-context-data-buffer context))
      (niwa-diagnostic-user-error (niwa-cockpit-diagnostic-scope context)
                                   "No niwa session cockpit here"))
    context))

(defun niwa-menu--display-buffer (buffer alist)
  "Display menu BUFFER using ALIST without reusing an editing side window."
  (let ((slot 0))
    (dolist (window (window-list nil 'no-minibuf))
      (when (eq (window-parameter window 'window-side) 'bottom)
        (setq slot (max slot (1+ (or (window-parameter window 'window-slot) 0))))))
    (display-buffer-in-side-window buffer (cons (cons 'slot slot) alist))))

(defun niwa-menu--setup (prefix &optional scope)
  "Set up PREFIX with SCOPE and explicit session-menu origin.
Use a disposable bottom side window, never an existing editing window."
  (let ((return-scope scope)
        (origin (selected-window))
        (configurations
         (mapcar (lambda (frame)
                   (cons frame (current-window-configuration frame)))
                 (frame-list)))
        (context (condition-case nil
                     (copy-sequence (niwa-menu--context))
                   (user-error nil))))
    (when context
      (setq context
            (plist-put context :return-menu
                       (lambda () (niwa-menu--setup prefix return-scope))))
      (setq scope (plist-put (copy-sequence scope) :niwa-context context)))
    (transient-setup
     prefix nil nil :scope scope
     :display-action `((niwa-menu--display-buffer)
                       (side . bottom)
                       (dedicated . t)
                       (inhibit-same-window . t)
                       (body-function
                        . ,(lambda (window)
                             (let ((configuration
                                    (alist-get (window-frame window) configurations)))
                               ;; Native Transient kills its popup before the
                               ;; suffix runs.  Restore even when a user display
                               ;; rule reused an editing window or side slot.
                               (with-current-buffer (window-buffer window)
                                 (add-hook
                                  'kill-buffer-hook
                                  (lambda ()
                                    (save-current-buffer
                                      (niwa-window-restore configuration origin t)))
                                  nil t)))))
                       (window-parameters . ((no-other-window . t)))))))


;;
;;; Labels

(defun niwa-menu--face (text face)
  "Return TEXT propertized with FACE."
  (propertize (format "%s" text) 'face face))

(defun niwa-menu--value (text &optional face)
  "Return TEXT as a cockpit state value using FACE."
  (niwa-menu--face text (or face 'transient-value)))

(defun niwa-menu--inactive-value (text)
  "Return TEXT as an inactive cockpit state value."
  (niwa-menu--face text 'transient-inactive-value))

(defconst niwa-menu--value-width 24
  "Most columns a top menu row's state value takes.
Longer values end in an ellipsis; their surface shows them whole.  The
layout widths in `niwa-menu--layout-widths' assume this bound.")

(defun niwa-menu--fit (text)
  "Return TEXT shortened to `niwa-menu--value-width' columns."
  (truncate-string-to-width (format "%s" text) niwa-menu--value-width
                            nil nil "…"))

(defun niwa-menu--state-description (label value &optional face)
  "Return a padded cockpit row for LABEL and state VALUE."
  (format "%-10s %s" label (niwa-menu--value (niwa-menu--fit value) face)))

(defun niwa-menu--mode-symbol (&optional session data-buffer surface-buffer)
  "Return the effective permission mode for the cockpit context."
  (let ((context (unless (and session data-buffer surface-buffer)
                   (condition-case nil
                       (niwa-menu--context)
                     (user-error nil)))))
    (setq session
          (or session
              (and context (niwa-cockpit-context-session context))))
    (setq data-buffer
          (or data-buffer
              (and context (niwa-cockpit-context-data-buffer context))))
    (setq surface-buffer
          (or surface-buffer
              (and context (niwa-cockpit-context-view-buffer context))
              data-buffer))
    (niwa-permission-mode-effective session data-buffer surface-buffer)))

(defun niwa-menu--mode-label (&optional mode)
  "Return the cockpit label for permission MODE."
  (niwa-permission-mode-label mode))

(defun niwa-menu--model-label ()
  "Return the current model label."
  (niwa-model-current-label
   (niwa-cockpit-context-data-buffer (niwa-menu--context))))

(defun niwa-menu--active-tool-count ()
  "Return the number of active gptel tools in the data buffer."
  (niwa-tools-active-count
   (niwa-cockpit-context-data-buffer (niwa-menu--context))))

(defun niwa-menu--root-label (workspace &optional target)
  "Return a compact root label for WORKSPACE and TARGET."
  (cond
   (target
    (abbreviate-file-name (niwa-execution-target-native-root target)))
   ((and workspace (fboundp 'niwa-workspace-root))
    (abbreviate-file-name (niwa-workspace-root workspace)))
   (t "unknown")))

(defun niwa-menu--worktree-label ()
  "Return the current branch or detached HEAD label."
  (plist-get (niwa-worktree-status-summary (niwa-menu--context))
             :label))

(defun niwa-menu--target-description (session)
  "Return SESSION's execution-target info-panel rows."
  (when-let* ((target (and session (niwa-session-execution-target session))))
    (let* ((readiness (niwa-execution-target-readiness target))
           (status (or (plist-get readiness :status) 'not-probed))
           (sandbox (plist-get readiness :sandbox-status)))
      (niwa-report-fields
       (list "Target" (niwa-execution-target-label target))
       (list "Support tier" (niwa-execution-target-support-tier target))
       (list "Readiness" status)
       (when sandbox (list "Sandbox" sandbox))))))

(defun niwa-menu--durability-description (session)
  "Return SESSION's persistence, lease, and publication info-panel rows."
  (when (and session (niwa-session-workspace session))
    (let* ((target (niwa-session-execution-target session))
           (status (niwa-session-publication-status session))
           (path (plist-get status :authoritative-state-path))
           (lease (or (plist-get status :lease-state)
                      (if (and target (niwa-execution-target-remote-p target)) 'none 'local))))
      (niwa-report-fields
       (list "State directory" (if target (niwa-execution-target-native-path target path) path))
       (list "Lease" lease)
       (list "Publication" (if (plist-get status :pending-publication) "pending" "published"))))))

(defun niwa-menu--alerts (session)
  "Return SESSION's off-nominal state tokens as a list of strings.
Nominal state belongs in the session info panel; only conditions the user
may want to act on earn a line in a cockpit header."
  (when session
    (let* ((target (niwa-session-execution-target session))
           (readiness (and target
                           (niwa-execution-target-readiness target)))
           (status (and readiness (plist-get readiness :status)))
           (sandbox (and readiness (plist-get readiness :sandbox-status)))
           (publication (and (niwa-session-workspace session)
                             (niwa-session-publication-status session)))
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

(defun niwa-menu--alert-line (session)
  "Return SESSION's warning-face alert line, or nil when all state is nominal."
  (when-let* ((alerts (niwa-menu--alerts session)))
    (concat (niwa-menu--value
             (concat "! " (string-join alerts " · "))
             'warning)
            "\n")))

(defun niwa-menu--header ()
  "Return the cockpit header string.
One identity line, plus an alert line when session state is off-nominal.
The complete target and durability state lives in the session info panel."
  (let* ((context (niwa-menu--context))
         (data-buffer (niwa-cockpit-context-data-buffer context))
         (session (niwa-cockpit-context-session context))
         (workspace (niwa-cockpit-context-workspace context))
         (target (and session
                      (niwa-session-execution-target session)))
         (mode (niwa-menu--mode-symbol
                session data-buffer
                (niwa-cockpit-context-view-buffer context)))
         (request-state (niwa-request-state-label data-buffer)))
    (concat
     (niwa-menu--face "niwa" 'transient-heading)
     " "
     (niwa-menu--value
      (or (and session (niwa-session-name session)) "unknown"))
     " · "
     (niwa-menu--value (niwa-menu--mode-label mode))
     " · "
     (niwa-menu--value
      request-state
      (if (string= request-state "running") 'warning 'transient-value))
     "    "
     (niwa-menu--face (niwa-menu--root-label workspace target)
                         'transient-inactive-value)
     (when target
       (niwa-menu--face
        (format " · %s" (niwa-execution-target-label target))
        'transient-inactive-value))
     "\n"
     (niwa-menu--alert-line session))))

(defun niwa-menu--read-only-p ()
  "Return non-nil when this cockpit's session is read-only here."
  (when-let* ((data (niwa-cockpit-context-data-buffer
                    (niwa-menu--context))))
    (buffer-local-value 'niwa-session--read-only-mode data)))

(defun niwa-menu--transfer-pending-p ()
  "Return non-nil when another client is waiting for this session's lease."
  (when-let* ((session (niwa-cockpit-context-session
                       (niwa-menu--context))))
    (and (not (niwa-menu--read-only-p))
         (memq (plist-get (niwa-session-control-transfer session) :state)
               '(requested quiescing)))))

(defun niwa-menu--follow-description ()
  "Return the follow toggle description for the control surface."
  (let ((data (niwa-cockpit-context-data-buffer (niwa-menu--context))))
    (format "%-10s %s" "Follow"
            (cond
             ((not (niwa-menu--read-only-p)) "owner")
             ((and data (buffer-local-value 'niwa-session-follow-published
                                            data))
              "on")
             (t "off")))))

(defun niwa-menu--control-summary ()
  "Return the top-level cockpit entry description for session control."
  (if (niwa-menu--read-only-p)
      (niwa-menu--state-description "Control" "read-only" 'warning)
    (niwa-menu--state-description "Control" "writable" 'transient-inactive-value)))

(defun niwa-menu--share-state ()
  "Return the current session's sharing summary, or nil when it is not shared.
Read from memory only: rendering the menu never loads collaboration,
connects, or touches a bearer link."
  (when (featurep 'niwa-collaboration)
    (niwa-collaboration-room-summary
     (niwa-cockpit-context-data-buffer (niwa-menu--context)))))

(defun niwa-menu--lobby-live-p ()
  "Return non-nil while the current workspace's lobby is shared."
  (and (featurep 'niwa-collaboration-lobby)
       (niwa-collaboration-lobby-live-p
        (niwa-cockpit-context-workspace (niwa-menu--context)))))

(defun niwa-menu--share-label (summary)
  "Return the sharing state label for room SUMMARY."
  (if summary
      (let ((guests (plist-get summary :guests)))
        (format "shared · %d guest%s" guests (if (= guests 1) "" "s")))
    "not shared"))

(defun niwa-menu--share-summary ()
  "Return the top-level Share row description."
  (let ((summary (niwa-menu--share-state))
        (elsewhere (length (niwa-menu--shares-elsewhere))))
    (niwa-menu--state-description
     "Share…"
     (concat (niwa-menu--share-label summary)
             (if (> elsewhere 0) (format " · +%d more" elsewhere) ""))
     (unless (or summary (> elsewhere 0)) 'transient-inactive-value))))

(defun niwa-menu--shares ()
  "Return every active share and lobby of this Emacs.
Read from memory only, like `niwa-menu--share-state': before
collaboration loads there is nothing to list."
  (and (featurep 'niwa-collaboration)
       (niwa-collaboration-shares)))

(defun niwa-menu--share-here-p (entry context)
  "Return non-nil when share ENTRY is CONTEXT's session or workspace lobby."
  (if (eq (plist-get entry :kind) 'room)
      (eq (plist-get entry :data-buffer)
          (niwa-cockpit-context-data-buffer context))
    (when-let* ((workspace (niwa-cockpit-context-workspace context)))
      (equal (plist-get entry :directory)
             (niwa-workspace-root workspace)))))

(defun niwa-menu--shares-elsewhere ()
  "Return the active shares other than this session's and workspace's."
  (let ((context (niwa-menu--context)))
    (seq-remove (lambda (entry) (niwa-menu--share-here-p entry context))
                (niwa-menu--shares))))

(defun niwa-menu--no-shares-p ()
  "Return non-nil when no share or lobby is active."
  (null (niwa-menu--shares)))

(defconst niwa-menu--share-row-limit 9
  "Most shares the Share surface lists, one per digit key.")

(defvar niwa-menu--share-entries nil
  "Shares listed by the open Share surface, in digit-key order.")

(defun niwa-menu--share-row-description (entry here &optional full-name)
  "Return the Share surface row for share ENTRY.
HERE says ENTRY belongs to the menu's own session or workspace.
FULL-NAME keeps the complete label for completion candidates."
  (let* ((kind (plist-get entry :kind))
         (guests (or (plist-get entry :guests) 0))
         (row
          (format "%s %-5s %-32s %s%s%s"
                  (if (eq kind 'recorded-lobby) "○" "●")
                  (if (eq kind 'room) "room" "lobby")
                  (if full-name
                      (or (plist-get entry :label) "?")
                    (truncate-string-to-width
                     (or (plist-get entry :label) "?") 32 nil nil "…"))
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
        (niwa-menu--face row 'transient-inactive-value)
      row)))

(defun niwa-menu--share-active-description ()
  "Return the heading of the Share surface's active-share rows."
  (let ((count (length (niwa-menu--shares))))
    (if (> count niwa-menu--share-row-limit)
        (format "Active everywhere (first %d of %d; a chooses any share)"
                niwa-menu--share-row-limit count)
      "Active everywhere")))

(defun niwa-menu--share-active-rows (_children)
  "Return one digit-keyed suffix per active share, this session's first."
  (let* ((context (niwa-menu--context))
         (shares (niwa-menu--shares))
         (entries
          (seq-take
           (append
            (seq-filter (lambda (entry) (niwa-menu--share-here-p entry context))
                        shares)
            (seq-remove (lambda (entry) (niwa-menu--share-here-p entry context))
                        shares))
           niwa-menu--share-row-limit))
         (index 0))
    (setq niwa-menu--share-entries entries)
    (transient-parse-suffixes
     'niwa-menu--share
     (mapcar (lambda (entry)
               (setq index (1+ index))
               (list (number-to-string index)
                     (niwa-menu--share-row-description
                      entry (niwa-menu--share-here-p entry context))
                     'niwa-menu--share-select))
             entries))))

(defun niwa-menu--share-select ()
  "Open the surface of the share whose digit key was pressed."
  (interactive)
  (let ((entry (and (characterp last-command-event)
                    (<= ?1 last-command-event ?9)
                    (nth (- last-command-event ?1)
                         niwa-menu--share-entries))))
    (unless entry
      (niwa-diagnostic-user-error (niwa-cockpit-diagnostic-scope)
                                   "No share on that key"))
    (niwa-menu--setup 'niwa-menu--share-item entry)))

(defun niwa-menu--share-choose ()
  "Choose any share, including those beyond the compact digit-key rows."
  (interactive)
  (let* ((context (niwa-menu--context))
         (index 0)
         (choices
          (mapcar
           (lambda (entry)
             ;; The ordinal disambiguates identically named sessions and
             ;; workspaces without exposing links or truncating their names.
             (cons (format "%d. %s%s"
                           (cl-incf index)
                           (niwa-menu--share-row-description
                            entry (niwa-menu--share-here-p entry context) t)
                           (if-let* ((root (plist-get entry :directory)))
                               (format " (%s)" (abbreviate-file-name root))
                             ""))
                   entry))
           (niwa-menu--shares))))
    (unless choices
      (niwa-diagnostic-user-error (niwa-cockpit-diagnostic-scope context)
                                   "No active shares or recorded lobbies"))
    (let ((entry (cdr (assoc (completing-read "Share: " choices nil t)
                             choices))))
      (when entry
        (niwa-menu--setup 'niwa-menu--share-item entry)))))

(defun niwa-menu--share-item-room-p ()
  "Return non-nil when the share surface shows a session room."
  (eq 'room (plist-get (transient-scope) :kind)))

(defun niwa-menu--share-item-recorded-p ()
  "Return non-nil when the share surface shows a lobby that is not running."
  (eq 'recorded-lobby (plist-get (transient-scope) :kind)))

(defun niwa-menu--share-item-description ()
  "Return the header of one share's surface."
  (let* ((entry (transient-scope))
         (kind (plist-get entry :kind))
         (guests (or (plist-get entry :guests) 0)))
    (concat
     (niwa-menu--face (if (eq kind 'room) "room" "lobby") 'transient-heading)
     " "
     (niwa-menu--value (or (plist-get entry :label) "?"))
     (if (eq kind 'room)
         ""
       (concat " " (abbreviate-file-name (plist-get entry :directory))))
     "\n"
     (niwa-menu--face
      (if (eq kind 'recorded-lobby)
          "Recorded to run, but not running: it failed to restart or was never started here."
        (format "%d %s%s connected · relay %s"
                guests (if (eq kind 'room) "guest" "tab")
                (if (= guests 1) "" "s")
                (if (plist-get entry :connected) "connected" "reconnecting")))
      'transient-inactive-value))))

(defun niwa-menu--share-item-stop-description ()
  "Return the stop action's label for the shown share."
  (pcase (plist-get (transient-scope) :kind)
    ('room "Stop sharing")
    ('lobby "Stop the lobby")
    (_ "Forget it (no restart)")))

(defun niwa-menu--share-item-open ()
  "Show the shared session."
  (interactive)
  (let ((buffer (plist-get (transient-scope) :data-buffer)))
    (unless (buffer-live-p buffer)
      (niwa-diagnostic-user-error
       (niwa-diagnostic-scope (or (plist-get (transient-scope) :directory)
                                    buffer 'unavailable))
       "That session is no longer open"))
    (niwa--display-chat-buffer buffer)))

(defun niwa-menu--share-item-links ()
  "Show the shown share's links, restarting a recorded lobby first."
  (interactive)
  (niwa-collaboration-show-share (transient-scope)))

(defun niwa-menu--share-item-stop ()
  "Stop the shown share, or forget a recorded lobby."
  (interactive)
  (niwa-collaboration-stop-share (transient-scope)))

(defun niwa-menu--share-stop-all ()
  "Stop every share and lobby after confirmation."
  (interactive)
  (let* ((entries (niwa-menu--shares))
         (rooms (seq-count (lambda (entry) (eq 'room (plist-get entry :kind)))
                           entries))
         (lobbies (- (length entries) rooms)))
    (if (yes-or-no-p
         (format (concat "Stop %d share%s and %d lobb%s? Their guests are "
                         "disconnected, and the session links stop working. ")
                 rooms (if (= rooms 1) "" "s")
                 lobbies (if (= lobbies 1) "y" "ies")))
        (niwa-collaboration-stop-all)
      (message "niwa: shares kept"))))

(defun niwa-menu--share-description ()
  "Return the Share surface header."
  (let ((summary (niwa-menu--share-state))
        (lobby (niwa-menu--lobby-live-p)))
    (concat
     (niwa-menu--face "share" 'transient-heading)
     " session "
     (niwa-menu--value (niwa-menu--share-label summary)
                          (unless summary 'transient-inactive-value))
     " · lobby "
     (niwa-menu--value (if lobby "shared" "not shared")
                          (unless lobby 'transient-inactive-value))
     "\n"
     (niwa-menu--face
      "Links are bearer credentials; their panel shows a QR code and copies them."
      'transient-inactive-value))))

(defun niwa-menu--not-shared-p ()
  "Return non-nil when the current session is not shared."
  (not (niwa-menu--share-state)))

(defun niwa-menu--lobby-down-p ()
  "Return non-nil when the current workspace's lobby is not shared."
  (not (niwa-menu--lobby-live-p)))

(defun niwa-menu--control-description ()
  "Return the control surface header.

Authority first, because every action on the surface either changes it or is
unavailable until it changes."
  (let* ((context (niwa-menu--context))
         (session (niwa-cockpit-context-session context))
         (read-only-p (niwa-menu--read-only-p))
         (transfer (and session (niwa-session-control-transfer session)))
         (label (plist-get (plist-get transfer :request) :requester-label)))
    (concat
     (niwa-menu--face "control" 'transient-heading)
     " "
     (niwa-menu--value
      (if read-only-p "read-only" "writable")
      (if read-only-p 'transient-inactive-value 'transient-value))
     " · "
     (niwa-menu--face
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
                   (or (niwa-session-control-transfer-drain-blocker
                        session)
                       "up"))))
        ('rejected "control request was declined")
        (_ (if read-only-p
               "another client is writing this session"
             "you hold this session's lease")))
      'transient-inactive-value))))

(defun niwa-menu--session-info-report ()
  "Return the complete session state as a structured information report."
  (let* ((context (niwa-menu--context))
         (data-buffer (niwa-cockpit-context-data-buffer context))
         (session (niwa-cockpit-context-session context))
         (workspace (niwa-cockpit-context-workspace context))
         (target (and session (niwa-session-execution-target session)))
         (mode (niwa-menu--mode-symbol
                session data-buffer (niwa-cockpit-context-view-buffer context))))
    (list
     :title "Session info"
     :subtitle (or (and session (niwa-session-name session)) "unknown")
     :identity session :refresh #'niwa-menu--session-info-report
     :sections
     (list
      (list :id 'request :title "Request"
            :body (niwa-report-fields
                   (list "State" (niwa-request-state-label data-buffer))
                   (list "Mode" (niwa-menu--mode-label mode))
                   (list "Preset" (or (and session (niwa-session-preset-name session)) "none"))))
      (list :id 'model :title "Model"
            :body (niwa-report-fields
                   (list "Provider / model" (niwa-model-current-provider-label data-buffer))
                   (list "Effort" (or (with-current-buffer data-buffer
                                        (and (boundp 'gptel-reasoning-effort) gptel-reasoning-effort))
                                      "default"))))
      (list :id 'target :title "Workspace & target"
            :body (concat (niwa-report-fields
                           (list "Workspace" (niwa-menu--root-label workspace target)))
                          (niwa-menu--target-description session)))
      (list :id 'persistence :title "Persistence"
            :body (or (niwa-menu--durability-description session) "No persistence state."))))))

(defun niwa-menu--call-in-view (function)
  "Call view FUNCTION in the cockpit's paired view buffer."
  (niwa-cockpit-call-in-view (niwa-menu--context) function))

(defun niwa-menu--navigate-description ()
  "Return the navigation surface header with the projected segment."
  (let* ((context (niwa-menu--context))
         (session (niwa-cockpit-context-session context))
         (view-buffer (niwa-cockpit-context-view-buffer context))
         (total (or (and session (niwa-session-current-segment session)) 1))
         (shown (and view-buffer
                     (with-current-buffer view-buffer
                       (niwa-view-segments-current-number)))))
    (concat
     (niwa-menu--face "Navigate" 'transient-heading)
     "  "
     (niwa-menu--value
      (or (and session (niwa-session-name session)) "unknown"))
     " · segment "
     (niwa-menu--value
      (if shown
          (format "%d/%d" shown total)
        (format "live (%d)" total))))))

(defun niwa-menu--mode-description ()
  "Return the top-level mode row description."
  (let ((context (niwa-menu--context)))
    (niwa-menu--state-description
     "Mode"
     (let* ((session (niwa-cockpit-context-session context))
            (permission
             (niwa-menu--mode-label
              (niwa-menu--mode-symbol
               session
               (niwa-cockpit-context-data-buffer context)
               (niwa-cockpit-context-view-buffer context)))))
       (if (niwa-session-plan-mode session)
           (format "Plan/%s" permission)
         permission)))))

(defun niwa-menu--model-description ()
  "Return the top-level model row description."
  (let ((model (niwa-menu--fit (niwa-menu--model-label))))
    (format "%-10s %s"
            "Model"
            (if (string= model "none")
                (niwa-menu--inactive-value model)
              (niwa-menu--value model)))))

(defun niwa-menu--model-selection-description ()
  "Return the caller-owned model-selection description."
  (let ((scope (transient-scope)))
    (concat
     (niwa-menu--face (plist-get scope :title) 'transient-heading)
     "  "
     (niwa-menu--value (plist-get scope :model-provider))
     " · effort "
     (niwa-menu--value
      (or (plist-get scope :reasoning-effort) "default"))
     (if (plist-get scope :inherited) " · session" ""))))

(defun niwa-menu--model-selection-effort-description ()
  "Return the effort-cycling row description with its next value."
  (let* ((scope (transient-scope))
         (efforts (niwa-menu--model-selection-efforts scope))
         (current (plist-get scope :reasoning-effort))
         (next (car (or (cdr (member current efforts)) efforts))))
    (format "%-13s %s → %s"
            "Cycle effort"
            (niwa-menu--value (or current "default"))
            (niwa-menu--inactive-value (or next "default")))))

(defun niwa-menu--memory-description ()
  "Return cached memory alerts without reading target files during rendering."
  (let* ((workspace (niwa-cockpit-context-workspace (niwa-menu--context)))
         (counts (niwa-workspace-memory-observation workspace))
         (labels (cl-loop for (key label) in '((:pending "pending") (:recovery "recovery") (:unavailable "unavailable"))
                          for count = (or (plist-get counts key) 0)
                          when (> count 0) collect (format "%d %s" count label))))
    (niwa-menu--state-description
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

(defun niwa-menu--tools-description ()
  "Return the top-level tools row description."
  (niwa-menu--state-description
   "Tools"
   (format "%d active" (niwa-menu--active-tool-count))
   'warning))

(defun niwa-menu--background-description ()
  "Return the top-level background-work row description."
  (let* ((session (niwa-cockpit-context-session
                   (niwa-menu--context)))
         (count (+ (niwa-execution-count-user session)
                   (length (niwa-view-agent-active-rows session)))))
    (niwa-menu--state-description
     "Background work"
     (format "%d live" count)
     (if (> count 0) 'warning 'transient-inactive-value))))

(defun niwa-menu--artifacts-description ()
  "Return the top-level artifacts row description.
The value reads ATTACHED/IN-PROJECT: artifacts attached to this session,
out of the project's store."
  (let* ((session (niwa-cockpit-context-session
                   (niwa-menu--context)))
         (counts (niwa-artifacts-list-count session)))
    (niwa-menu--state-description
     "Artifacts"
     (format "%d/%d" (car counts) (cdr counts))
     (if (> (car counts) 0) 'warning 'transient-inactive-value))))

(defun niwa-menu--skills-description ()
  "Return the top-level skills row description."
  (let ((context (niwa-menu--context)))
    (niwa-menu--state-description
     "Skills"
     (niwa-skills-count-label (niwa-cockpit-context-session context))
     'warning)))

(defun niwa-menu--plugins-description ()
  "Return the top-level plugins row description."
  (let* ((workspace (niwa-cockpit-context-workspace
                     (niwa-menu--context)))
         (label (niwa-plugins-count-label workspace)))
    (niwa-menu--state-description
     "Plugins" label
     (if (string-suffix-p "/0" label) 'transient-inactive-value 'warning))))

(defun niwa-menu--worktree-description ()
  "Return the top-level worktree row description."
  (let ((worktree (niwa-menu--fit (niwa-menu--worktree-label))))
    (format "%-10s %s"
            "Worktree"
            (if (string= worktree "not-git")
                (niwa-menu--inactive-value worktree)
              (niwa-menu--value worktree)))))

(defun niwa-menu--preset-summary ()
  "Return the top-level preset row description."
  (let ((preset (niwa-session-preset-name
                 (niwa-cockpit-context-session (niwa-menu--context)))))
    (if preset
        (niwa-menu--state-description "Preset" preset)
      (niwa-menu--state-description "Preset" "none" 'transient-inactive-value))))

(defun niwa-menu--goal-summary ()
  "Return the top-level Goal row description."
  (if-let* ((goal (niwa-menu--current-goal)))
      (niwa-menu--state-description
       "Goal" (format "%s, %d turns" (niwa-goal-status goal)
                      (niwa-goal-turns-run goal)))
    (niwa-menu--state-description "Goal" "none" 'transient-inactive-value)))

(defun niwa-menu--current-goal ()
  "Return the current session Goal, or nil."
  (niwa-session-goal
   (niwa-cockpit-context-session (niwa-menu--context))))

(defun niwa-menu--goal-active-p ()
  "Return non-nil when the current Goal can be paused."
  (when-let* ((goal (niwa-menu--current-goal)))
    (eq (niwa-goal-status goal) 'active)))

(defun niwa-menu--goal-resumable-p ()
  "Return non-nil when the current Goal can be resumed."
  (when-let* ((goal (niwa-menu--current-goal)))
    (memq (niwa-goal-status goal) '(paused blocked))))

(defun niwa-menu--goal-start-inapt-p ()
  "Return non-nil when an unfinished Goal already exists."
  (when-let* ((goal (niwa-menu--current-goal)))
    (not (eq (niwa-goal-status goal) 'complete))))

(defun niwa-menu--goal-clearable-p ()
  "Return non-nil when Goal state may be cleared now."
  (and (niwa-menu--current-goal)
       (not (niwa-menu--request-active-p))))

(defun niwa-menu--goal-budget-label (goal)
  "Return GOAL's token accounting as a compact label."
  (let ((budget (niwa-goal-token-budget goal))
        (used (if (niwa-goal-tokens-incomplete-p goal)
                  (format "at least %d (incomplete)" (niwa-goal-tokens-used goal))
                (number-to-string (niwa-goal-tokens-used goal)))))
    (if budget
        (format "%s/%d tokens" used budget)
      (format "%s tokens · unbounded" used))))

(defun niwa-menu--goal-description ()
  "Return the one-line Goal cockpit status.
The full record lives in the Goal info panel."
  (if-let* ((goal (niwa-menu--current-goal)))
      (let ((tail
             (concat
              " · "
              (niwa-menu--value (format "%s" (niwa-goal-status goal))
                                   (if (eq (niwa-goal-status goal) 'blocked)
                                       'warning
                                     'transient-value))
              (format " · %d turns · " (niwa-goal-turns-run goal))
              (niwa-menu--value (niwa-menu--goal-budget-label goal)))))
        (concat
         (niwa-menu--face "Goal" 'transient-heading)
         "  "
         (niwa-menu--value
          (truncate-string-to-width
           (string-clean-whitespace (niwa-goal-objective goal))
           ;; Keep status and accounting visible; 6 = "Goal  ".
           (max 20 (- (frame-width) 6 (string-width tail) 1))
           nil nil "…"))
         tail))
    (concat
     (niwa-menu--face "Goal" 'transient-heading)
     "  "
     (niwa-menu--inactive-value
      "none — s starts one, or /goal OBJECTIVE"))))

(defun niwa-menu--goal-record-report ()
  "Return the complete Goal as an information report."
  (let ((goal (niwa-menu--current-goal)))
    (list :title "Goal" :identity goal :refresh #'niwa-menu--goal-record-report
          :sections
          (if (not goal)
              (list (list :id 'goal :title "Goal"
                          :body "No active Goal. Start one here or with /goal OBJECTIVE."))
            (list
             (list :id 'objective :title "Objective" :body (niwa-goal-objective goal))
             (list :id 'progress :title "Progress"
                   :body (niwa-report-fields
                          (list "Status" (format "%s%s" (niwa-goal-status goal)
                                                 (if-let* ((reason (niwa-goal-reason goal))) (format " — %s" reason) "")))
                          (list "Budget" (niwa-menu--goal-budget-label goal))
                          (list "Turns" (format "%d · elapsed %ds" (niwa-goal-turns-run goal) (niwa-goal-time-used-seconds goal)))))
             (list :id 'plan :title "Accepted plan"
                   :body (niwa-report-fields (list "Plan" (or (niwa-goal-plan-reference goal) "none")))))))))

(defun niwa-menu--preset-policies ()
  "Return the session's resolved preset policies.
Each element is (KIND NAME LABEL ERROR), where ERROR is the resolution
failure message or nil.  KIND is `tier' or `workload'."
  (let ((context (niwa-menu--context)))
    (with-current-buffer (niwa-cockpit-context-data-buffer context)
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
                   (funcall resolve 'tier tier #'niwa-model-resolve-tier))
                 (delete-dups (mapcar #'car niwa-model-tiers)))
         (mapcar (lambda (workload)
                   (funcall resolve 'workload workload
                            #'niwa-model-resolve-workload))
                 (delete-dups (mapcar #'car niwa-model-workloads))))))))

(defun niwa-menu--preset-description ()
  "Return the one-line preset summary, plus an alert for broken policies.
The resolved policy table lives in the preset info panel."
  (let* ((context (niwa-menu--context))
         (session (niwa-cockpit-context-session context))
         (policies (niwa-menu--preset-policies))
         (broken (seq-filter (lambda (policy) (nth 3 policy)) policies))
         (tiers (seq-count (lambda (policy) (eq (car policy) 'tier)) policies)))
    (concat
     (niwa-menu--face "Preset" 'transient-heading)
     "  "
     (niwa-menu--value
      (or (and session (niwa-session-preset-name session)) "none"))
     (format " · %d tiers · %d workloads · " tiers (- (length policies) tiers))
     (if broken
         (niwa-menu--value (format "%d broken" (length broken)) 'error)
       (niwa-menu--value "all resolved"))
     (when broken
       (concat
        "\n"
        (niwa-menu--value
         (format "! %s %s does not resolve — fix before dispatch"
                 (nth 0 (car broken)) (nth 1 (car broken)))
         'error))))))

(defun niwa-menu--preset-report ()
  "Return resolved preset policy as an information report."
  (let* ((context (niwa-menu--context))
         (session (niwa-cockpit-context-session context))
         (policies (niwa-menu--preset-policies)))
    (list :title "Preset"
          :subtitle (format "%s" (or (and session (niwa-session-preset-name session)) "none"))
          :identity session :refresh #'niwa-menu--preset-report
          :sections
          (mapcar
           (lambda (kind)
             (list :id kind :title (if (eq kind 'tier) "Tiers" "Workloads")
                   :body (mapconcat
                          (lambda (policy)
                            (niwa-report-fields
                             (list (nth 1 policy)
                                   (or (nth 2 policy) (propertize (format "ERROR: %s" (nth 3 policy)) 'face 'error)))))
                          (seq-filter (lambda (policy) (eq (car policy) kind)) policies) "")))
           '(tier workload)))))

(defun niwa-menu--mode-choice-description (mode detail)
  "Return the MODE surface row with DETAIL and current-state marker."
  (let* ((context (niwa-menu--context))
         (session (niwa-cockpit-context-session context))
         (current (and (not (niwa-session-plan-mode session))
                       (eq mode
                           (niwa-menu--mode-symbol
                            session
                            (niwa-cockpit-context-data-buffer context)
                            (niwa-cockpit-context-view-buffer context)))))
         (label (niwa-menu--mode-label mode)))
    (niwa-menu--mode-row label detail current)))

(defun niwa-menu--mode-row (label detail current)
  "Return a mode-surface row for LABEL and DETAIL.
CURRENT marks the row as the active choice.  The marker trails the
descriptions so they stay aligned and read as a comparison."
  (string-trim-right
   (format "%-10s %-40s %s"
           (if current (niwa-menu--value label) label)
           detail
           (if current (niwa-menu--value "current" 'warning) ""))))

(defun niwa-menu--mode-ask-description ()
  "Return the ask mode row description."
  (niwa-menu--mode-choice-description
   'ask "prompt for edits and uncertain execution"))

(defun niwa-menu--mode-edits-description ()
  "Return the edits mode row description."
  (niwa-menu--mode-choice-description
   'edits "automatic edits and confined execution"))

(defun niwa-menu--mode-full-auto-description ()
  "Return the full-auto mode row description."
  (niwa-menu--mode-choice-description 'full-auto "Full Access, no confinement or prompts"))

(defun niwa-menu--mode-plan-description ()
  "Return the Plan mode row description.
Plan is orthogonal to the permission mode, so it reads as its own on/off
state rather than as a fourth permission choice."
  (let* ((session (niwa-cockpit-context-session
                   (niwa-menu--context)))
         (current (niwa-session-plan-mode session)))
    (format "%-10s %s · %s"
            "Plan mode"
            (if current
                (niwa-menu--value "on" 'warning)
              (niwa-menu--inactive-value "off"))
            "inspect and discuss, no edits")))

(defun niwa-menu--mode-surface-description ()
  "Return the one-line mode surface header."
  (let* ((context (niwa-menu--context))
         (session (niwa-cockpit-context-session context))
         (permission
          (niwa-menu--mode-label
           (niwa-menu--mode-symbol
            session
            (niwa-cockpit-context-data-buffer context)
            (niwa-cockpit-context-view-buffer context)))))
    (concat
     (niwa-menu--face "Mode" 'transient-heading)
     "  permission "
     (niwa-menu--value permission)
     " · Plan "
     (if (niwa-session-plan-mode session)
         (niwa-menu--value "on" 'warning)
       (niwa-menu--inactive-value "off")))))


;;
;;; Inapt predicates

(defun niwa-menu--request-active-p ()
  "Return non-nil when the current session has an active request."
  (niwa-request-active-p
   (niwa-cockpit-context-data-buffer (niwa-menu--context))))

(defun niwa-menu--send-inapt-p ()
  "Return non-nil when sending should be inapt."
  (niwa-turn-busy-p
   (niwa-cockpit-context-data-buffer (niwa-menu--context))))

(defun niwa-menu--abort-inapt-p ()
  "Return non-nil when aborting should be inapt."
  (not (niwa-menu--request-active-p)))


;;
;;; Mode surface

(defun niwa-menu--set-mode (mode)
  "Set the current session permission MODE."
  (niwa-cockpit-call-in-data
   (niwa-menu--context) #'niwa-permission-mode-transition mode)
  (force-mode-line-update t))

(defun niwa-menu--enter-plan ()
  "Enter Plan mode for the current cockpit session."
  (interactive)
  (niwa-cockpit-call-in-data
   (niwa-menu--context) #'niwa-plan-mode-enter)
  (force-mode-line-update t))


;;
;;; Model surface

(defun niwa-menu--refresh-plan-approval (session)
  "Refresh SESSION's visible Plan approval when one is pending."
  (when (and (niwa-session-pending-plan-approval session)
             (fboundp 'niwa-plan-approval-render))
    (niwa-plan-approval-render session)))

(defun niwa-menu--model-selection-select-model ()
  "Select a model for the caller-owned model-selection scope."
  (interactive)
  (let ((candidates (niwa-model-candidates)))
    (unless candidates
      (niwa-diagnostic-user-error 'global "No registered gptel models"))
    (let* ((scope (transient-scope))
           (label (completing-read "Model: " candidates nil t))
           (provider (cdr (assoc label candidates)))
           (effort (plist-get scope :reasoning-effort))
           (model (plist-get provider :model)))
      (unless (memq effort (niwa-model-supported-efforts model))
        (setq effort nil))
      (plist-put scope :model-provider label)
      (plist-put scope :reasoning-effort effort)
      (plist-put scope :inherited nil)
      (funcall (plist-get scope :update) label effort))))

(defun niwa-menu--model-selection-efforts (&optional scope)
  "Return the selectable efforts for SCOPE's model.
The list is the model's supported efforts followed by nil for the
provider default, which is the full cycle the surface offers."
  (let* ((scope (or scope (transient-scope)))
         (provider (niwa-model-resolve-provider
                    (plist-get scope :model-provider) t)))
    (append (and provider
                 (niwa-model-supported-efforts
                  (plist-get provider :model)))
            (list nil))))

(defun niwa-menu--model-selection-cycle-effort ()
  "Advance the caller-owned model selection to the next reasoning effort.
Effort is a closed enum the chosen model determines, so it cycles in
place instead of prompting."
  (interactive)
  (let* ((scope (transient-scope))
         (efforts (niwa-menu--model-selection-efforts scope))
         (current (plist-get scope :reasoning-effort))
         (effort (car (or (cdr (member current efforts)) efforts))))
    (plist-put scope :reasoning-effort effort)
    (plist-put scope :inherited nil)
    (funcall (plist-get scope :update)
             (plist-get scope :model-provider) effort)))

(defun niwa-menu--model-selection-reset ()
  "Reset the caller-owned model selection to its inherited values."
  (interactive)
  (let* ((scope (transient-scope))
         (values (funcall (plist-get scope :reset))))
    (plist-put scope :model-provider (car values))
    (plist-put scope :reasoning-effort (cadr values))
    (plist-put scope :inherited t)))

(cl-defun niwa-menu-open-model-selection
    (&key title provider effort update reset inherited)
  "Open shared model selection for caller-owned state."
  (niwa-menu--setup
   'niwa-menu--model-selection
   (list :title title
                :model-provider provider
                :reasoning-effort effort
                :update update
                :reset reset
                :inherited inherited)))

;;
;;; Commands

;;;###autoload
(defun niwa-menu ()
  "Open the niwa session cockpit."
  (interactive)
  (niwa-menu-open 'top))

;;;###autoload
(defun niwa-menu-open (area)
  "Open session cockpit AREA.
AREA is `top' for the main cockpit, or a named cockpit surface.
A cockpit waits for keys in Emacs, so it refuses where nobody can be
asked, such as a collaboration guest's turn."
  (interactive (list 'top))
  (when inhibit-interaction
    (signal 'inhibited-interaction (list "The cockpit needs Emacs")))
  (let ((context (niwa-menu--context)))
    (pcase area
      ('top
       (niwa-menu--setup 'niwa-menu--top))
      ('navigate
       (niwa-menu--setup 'niwa-menu--navigate))
      ('session-info
       (niwa-menu--open-session-info))
      ('mode
       (niwa-menu--setup 'niwa-menu--mode))
      ('permissions
       (niwa-cockpit-call-in-data
        context #'niwa-permissions-list-open context))
      ('memory
       (niwa-cockpit-call-in-data context #'niwa-memory-list-open context))
      ('model
       (niwa-menu--open-model))
      ('goal
       (niwa-menu--setup 'niwa-menu--goal))
      ('control
       (niwa-menu--setup 'niwa-menu--control))
      ('share
       (niwa-menu--setup 'niwa-menu--share))
      ('preset
       (niwa-menu--setup 'niwa-menu--preset))
      ('skills
       (niwa-cockpit-call-in-data
        context #'niwa-skills-list-open context))
      ('plugins
       (niwa-cockpit-call-in-data
        context #'niwa-plugins-list-open context))
      ('tools
       (niwa-cockpit-call-in-data
        context #'niwa-tools-list-open context))
      ('background
       (niwa-cockpit-call-in-data
        context #'niwa-background-list-open context))
      ('schedules
       (niwa-schedules-list-open context))
      ('artifacts
       (niwa-cockpit-call-in-data
        context #'niwa-artifacts-list-open context))
      ('worktree
       (niwa-cockpit-call-in-data
        context #'niwa-worktree-status-open))
      ('help
       (niwa-menu-help-open))
      ('gptel
       (niwa-menu--open-gptel))
      (_
       (niwa-diagnostic-user-error (niwa-cockpit-diagnostic-scope context)
                                     "Unknown cockpit area: %s" area)))))

(defun niwa-menu--send ()
  "Send the current composer from the view buffer."
  (interactive)
  (niwa-menu--call-in-view #'niwa-view-send))

(defun niwa-menu--abort ()
  "Abort the active request from the view buffer."
  (interactive)
  (niwa-menu--call-in-view #'niwa-view-abort))

(defun niwa-menu--take-control ()
  "Take control of this session from the view buffer."
  (interactive)
  (niwa-menu--call-in-view #'niwa-take-control))

(defun niwa-menu--release-control ()
  "Release this session's lease from the view buffer."
  (interactive)
  (niwa-menu--call-in-view #'niwa-release-control))

(defun niwa-menu--grant-control ()
  "Grant the pending control-transfer request from the view buffer."
  (interactive)
  (niwa-menu--call-in-view #'niwa-view-control-transfer-grant))

(defun niwa-menu--keep-control ()
  "Decline the pending control-transfer request from the view buffer."
  (interactive)
  (niwa-menu--call-in-view #'niwa-view-control-transfer-keep))

(defun niwa-menu--toggle-follow ()
  "Toggle published-state following from the view buffer."
  (interactive)
  (niwa-menu--call-in-view #'niwa-toggle-follow))

(defun niwa-menu--refresh-session ()
  "Re-read the owner's newest published state from the view buffer."
  (interactive)
  (niwa-menu--call-in-view #'niwa-refresh-session))

(defun niwa-menu--rewind-here ()
  "Rewind keeping the settled assistant turn at point in the view buffer."
  (interactive)
  (niwa-menu--call-in-view #'niwa-view-rewind-at-point))

(defun niwa-menu--redo ()
  "Restore the session to one of its published heads."
  (interactive)
  (niwa-menu--call-live-tip-data #'niwa-redo))

(defun niwa-menu--redo-inapt-p ()
  "Return non-nil when this session publishes no restorable head.

The recorded authority mode is read directly: offering the entry must
not be the step that rejects a session, and a file session simply has
nothing to restore."
  (let ((session (niwa-cockpit-context-session (niwa-menu--context))))
    (not (and session
              (eq (niwa-session-authority-mode session) 'portable)))))

(defun niwa-menu--switch-variant-here ()
  "Switch conversation variants at point in the paired view buffer."
  (interactive)
  (niwa-menu--call-in-view #'niwa-view-switch-conversation-variant-at-point))

(defun niwa-menu--fork-conversation-here ()
  "Arm a Conversation Fork at point in the paired view buffer."
  (interactive)
  (niwa-menu--call-in-view #'niwa-view-arm-conversation-fork))

(defun niwa-menu--fork-worktree-here ()
  "Arm a Worktree Fork at point in the paired view buffer."
  (interactive)
  (niwa-menu--call-in-view #'niwa-view-arm-worktree-fork))

(defun niwa-menu--call-live-tip-data (function)
  "Call data-buffer FUNCTION after checking the paired view is live."
  (let ((context (niwa-menu--context)))
    (niwa-cockpit-call-in-view
     context #'niwa-view--assert-live-tip)
    (niwa-cockpit-call-in-data context function)))

(defun niwa-menu--compact ()
  "Compact the current data buffer."
  (interactive)
  (niwa-menu--call-live-tip-data #'niwa-compact))

(defun niwa-menu--review ()
  "Run the review picker from the data buffer."
  (interactive)
  (niwa-menu--call-live-tip-data #'niwa-review))

(defun niwa-menu--verify ()
  "Run the verify picker from the data buffer."
  (interactive)
  (niwa-menu--call-live-tip-data #'niwa-verify))

(defun niwa-menu--toggle-data-view ()
  "Toggle between the view buffer and raw data buffer."
  (interactive)
  (let* ((context (niwa-menu--context))
         (origin (or (niwa-cockpit-context-origin-buffer context)
                     (current-buffer)))
         (view-buffer (niwa-cockpit-context-view-buffer context))
         (data-buffer (niwa-cockpit-context-data-buffer context))
         (target (if (eq origin data-buffer) view-buffer data-buffer)))
    (switch-to-buffer target)))

(defun niwa-menu--open-mode ()
  "Open the mode cockpit surface."
  (interactive)
  (niwa-menu-open 'mode))

(defun niwa-menu--open-permissions ()
  "Open the permissions cockpit surface."
  (interactive)
  (niwa-menu-open 'permissions))

(defun niwa-menu--open-memory ()
  "Open the workspace memory proposals cockpit."
  (interactive)
  (niwa-menu-open 'memory))

(defun niwa-menu--open-model ()
  "Open the model cockpit surface."
  (interactive)
  (let* ((context (niwa-menu--context))
         (session (niwa-cockpit-context-session context))
         (buffer (niwa-cockpit-context-data-buffer context)))
    (niwa-menu-open-model-selection
     :title "Session model"
     :provider (niwa-model-current-provider-label buffer)
     :effort (with-current-buffer buffer
               (and (boundp 'gptel-reasoning-effort)
                    gptel-reasoning-effort))
     :update
     (lambda (provider effort)
       (niwa-model-set-session-provider
        session (niwa-model-resolve-provider provider) buffer)
       (niwa-model-set-session-effort session effort buffer)
       (niwa-menu--refresh-plan-approval session)
       (force-mode-line-update t)))))

(defun niwa-menu--open-goal ()
  "Open the Goal cockpit surface."
  (interactive)
  (niwa-menu-open 'goal))

(defun niwa-menu--open-preset ()
  "Open the Preset cockpit surface."
  (interactive)
  (niwa-menu-open 'preset))

(defun niwa-menu--goal-call (function &rest args)
  "Call Goal FUNCTION with ARGS in the owning data buffer."
  (apply #'niwa-cockpit-call-in-data
         (niwa-menu--context) function args)
  (force-mode-line-update t))

(defun niwa-menu--goal-start ()
  "Prompt for and start a Goal."
  (interactive)
  (niwa-menu--goal-call
   #'niwa-goal-start (read-string "Goal objective: ")))

(defun niwa-menu--goal-edit ()
  "Prompt for and replace the current Goal objective."
  (interactive)
  (niwa-menu--goal-call
   #'niwa-goal-edit (read-string "New Goal objective: ")))

(defun niwa-menu--goal-budget ()
  "Prompt for and replace the current Goal token budget."
  (interactive)
  (niwa-menu--goal-call
   #'niwa-goal-set-budget
   (read-string "Goal token budget (positive integer or none): ")))

(defun niwa-menu--select-preset ()
  "Select and apply a preset to the current session only."
  (interactive)
  (let* ((names (mapcar (lambda (entry) (symbol-name (car entry)))
                        niwa-preset--registry))
         (name (intern (completing-read "Preset: " names nil t))))
    (niwa-cockpit-call-in-data
     (niwa-menu--context) #'niwa-preset-apply name)
    (force-mode-line-update t)
    (message "niwa: preset set to %s for this session" name)))

(defun niwa-menu--open-tools ()
  "Open the tools cockpit surface."
  (interactive)
  (niwa-menu-open 'tools))

(defun niwa-menu--open-background ()
  "Open the background-work cockpit surface."
  (interactive)
  (niwa-menu-open 'background))

(defun niwa-menu--open-schedules ()
  "Open the workspace's standing jobs."
  (interactive)
  (niwa-menu-open 'schedules))

(defun niwa-menu--open-artifacts ()
  "Open the artifacts cockpit surface."
  (interactive)
  (niwa-menu-open 'artifacts))

(defun niwa-menu--open-skills ()
  "Open the skills cockpit surface."
  (interactive)
  (niwa-menu-open 'skills))

(defun niwa-menu--open-plugins ()
  "Open the plugins cockpit surface."
  (interactive)
  (niwa-menu-open 'plugins))

(defun niwa-menu--open-worktree ()
  "Open the worktree cockpit surface."
  (interactive)
  (niwa-menu-open 'worktree))

(defun niwa-menu--open-help ()
  "Open the help cockpit surface."
  (interactive)
  (niwa-menu-open 'help))

(defun niwa-menu--open-navigate ()
  "Open the navigation cockpit surface."
  (interactive)
  (niwa-menu-open 'navigate))

(defun niwa-menu--open-control ()
  "Open the session control cockpit surface."
  (interactive)
  (niwa-menu-open 'control))

(defun niwa-menu--open-share ()
  "Open the Share cockpit surface."
  (interactive)
  (niwa-menu-open 'share))

(defun niwa-menu--share-session ()
  "Show this session's share links, first sharing it after confirmation."
  (interactive)
  (niwa-menu--call-in-view #'niwa-collaboration-view))

(defun niwa-menu--share-stop ()
  "Stop sharing this session, leaving every other share running."
  (interactive)
  (niwa-collaboration-stop-session
   (niwa-cockpit-context-data-buffer (niwa-menu--context))))

(defun niwa-menu--share-status ()
  "Report every live share and lobby without exposing their links."
  (interactive)
  (niwa-collaboration-status))

(defun niwa-menu--share-lobby ()
  "Show this workspace's lobby links, first sharing it after confirmation."
  (interactive)
  (niwa-cockpit-call-in-data (niwa-menu--context) #'niwa-collaboration-lobby))

(defun niwa-menu--share-lobby-stop ()
  "Stop this workspace's lobby."
  (interactive)
  (niwa-cockpit-call-in-data (niwa-menu--context)
                                #'niwa-collaboration-lobby-stop))

(defun niwa-menu--call-artifact-command (command)
  "Run artifact COMMAND interactively in this session's workspace."
  (niwa-cockpit-call-in-data (niwa-menu--context) #'call-interactively command))

(defun niwa-menu--share-artifact ()
  "Share one of this workspace's artifacts, or show its links."
  (interactive)
  (niwa-menu--call-artifact-command 'niwa-collaboration-share-artifact))

(defun niwa-menu--rotate-artifact ()
  "Replace the links of one of this workspace's shared artifacts."
  (interactive)
  (niwa-menu--call-artifact-command 'niwa-collaboration-rotate-artifact))

(defun niwa-menu--revoke-artifact ()
  "Stop sharing one of this workspace's artifacts."
  (interactive)
  (niwa-menu--call-artifact-command 'niwa-collaboration-revoke-artifact))

(defun niwa-menu--attach-remote-artifact ()
  "Attach a remote artifact to this workspace from its link."
  (interactive)
  (niwa-menu--call-artifact-command 'niwa-remote-artifact-attach))

(defun niwa-menu--detach-remote-artifact ()
  "Forget one of this workspace's remote artifacts."
  (interactive)
  (niwa-menu--call-artifact-command 'niwa-remote-artifact-detach))

(defun niwa-menu--share-lobby-rotate ()
  "Replace this workspace's lobby links after confirmation."
  (interactive)
  (if (yes-or-no-p (concat "Replace the lobby links? Every earlier lobby link "
                           "stops working, including ones already handed out. "))
      (niwa-cockpit-call-in-data (niwa-menu--context)
                                    #'niwa-collaboration-lobby-rotate)
    (message "niwa: lobby links kept")))

(defun niwa-menu--open-session-info ()
  "Open the session info panel."
  (interactive)
  (niwa-cockpit-show-help
   niwa-menu-session-info-buffer-name
   (niwa-menu--session-info-report)))

(defun niwa-menu--open-preset-report ()
  "Open the resolved preset policy info panel."
  (interactive)
  (niwa-cockpit-show-help
   niwa-menu-preset-report-buffer-name
   (niwa-menu--preset-report)))

(defun niwa-menu--open-goal-record ()
  "Open the Goal record info panel."
  (interactive)
  (niwa-cockpit-show-help
   niwa-menu-goal-record-buffer-name
   (niwa-menu--goal-record-report)))

(defun niwa-menu-help--report ()
  "Return complete command discovery as an information report."
  (list :title "niwa help"
        :sections (list (list :id 'help-0 :title "Session cockpit"
			      :body (string-join (list "The transient menu is the live key reference for session commands.") "\n"))
			(list :id 'help-1 :title "Slash commands that open UI"
			      :body (string-join (list "/plugin, /plugin list       Plugins" "/skills, /skills list       Skills" "/mode                       Mode" "/model                      Model" "Cockpit G / P               Goal / Preset model team" "Cockpit u                   Remembered permission authority" "Cockpit A                   Artifacts (attached/in project)" "Cockpit n                   New whiteboard or document" "/collab, Cockpit S          Share links, status, lobby" "Cockpit i                   Session info panel" "/usage, Cockpit U           Subscription usage" "/tools, /tools list         Tools" "/ps                         Background work" "/stop [EXECUTION_ID]        Stop one execution, or all when omitted" "/worktree, /worktree status Worktree" "/help                       Help") "\n"))
			(list :id 'help-2 :title "Direct slash commands"
			      :body (string-join (list "/plugin enable NAME, disable NAME, reload, update NAME" "/plugin install TARGET, remove NAME, uninstall NAME, hooks ..." "/skills enable NAME, disable NAME, help NAME" "/mode MODE, /model MODEL" "/worktree create [NAME] [--for \"purpose\"] [--clean]" "/goal OBJECTIVE, /goal budget N|none, /goal edit|pause|resume|clear" "/compact, /remember [focus], /review, /verify, /edits, /clear, /init ..., /tokens") "\n"))
			(list :id 'help-3 :title "Modes"
			      :body (string-join (list "ask       Prompt for edits and uncertain execution." "edits     Automatic native edits and confined Bash/fresh Eval." "full-auto Full Access: no confinement or permission prompts.") "\n"))
			(list :id 'help-4 :title "View and data buffers"
			      :body (string-join (list "The view buffer owns the composer, compact transcript, and status strip." "The data buffer owns raw gptel state, tools, model, and transcript data." "The cockpit resolves the view/data pair once and routes actions to the owning buffer." "Cockpit N opens Navigate: [ / ] / g inspect session segments," "n / p move through displays, C-n / C-p through queries, TAB folds a section." "The raw data buffer keeps gptel header behavior for the gptel menu.") "\n")))))

(defun niwa-menu-help-open ()
  "Open the session cockpit help surface."
  (niwa-cockpit-show-help niwa-menu-help-buffer-name (niwa-menu-help--report)))

(defun niwa-menu--open-gptel ()
  "Open the gptel bridge surface."
  (interactive)
  (niwa-gptel-bridge-open (niwa-menu--context)))


;;
;;; Transient

;;;; Top menu layout

(defconst niwa-menu--layout-widths '((wide . 170) (medium . 100))
  "Smallest frame widths, in columns, of the wider top menu layouts.
Wide shows the six groups side by side, medium in two rows of three,
and anything narrower in three rows of two.")

(defconst niwa-menu--footer-width 72
  "Smallest frame width, in columns, that fits the utilities on one row.")

(defun niwa-menu--layout ()
  "Return the top menu layout for the selected frame.
The value is `wide', `medium' or `narrow'; see
`niwa-menu--layout-widths'."
  (let ((width (frame-width)))
    (cond ((>= width (alist-get 'wide niwa-menu--layout-widths)) 'wide)
          ((>= width (alist-get 'medium niwa-menu--layout-widths)) 'medium)
          (t 'narrow))))

(defun niwa-menu--wide-p ()
  "Return non-nil when the top menu shows its groups side by side."
  (eq (niwa-menu--layout) 'wide))

(defun niwa-menu--medium-p ()
  "Return non-nil when the top menu shows two rows of three groups."
  (eq (niwa-menu--layout) 'medium))

(defun niwa-menu--narrow-p ()
  "Return non-nil when the top menu shows three rows of two groups."
  (eq (niwa-menu--layout) 'narrow))

(defun niwa-menu--split-footer-p ()
  "Return non-nil when the utilities need two rows."
  (< (frame-width) niwa-menu--footer-width))

;; Each group is defined once and arranged by the width-dependent layouts
;; below; only one layout is active, so every key is bound exactly once.

(transient-define-group niwa-menu--conversation-group
  ["Conversation"
   :pad-keys t
   ("RET" "Send" niwa-menu--send
    :inapt-if niwa-menu--send-inapt-p)
   ("a" "Abort" niwa-menu--abort
    :inapt-if niwa-menu--abort-inapt-p)
   ("c" "Compact" niwa-menu--compact)
   ("r" "Review" niwa-menu--review)
   ("v" "Verify" niwa-menu--verify)])

(transient-define-group niwa-menu--history-group
  ["History"
   :pad-keys t
   ("N" "Navigate…" niwa-menu--open-navigate)
   ("f" "Fork conversation" niwa-menu--fork-conversation-here)
   ("F" "Fork worktree" niwa-menu--fork-worktree-here)
   ("R" "Rewind here" niwa-menu--rewind-here)
   ("D" "Redo…" niwa-menu--redo
    :inapt-if niwa-menu--redo-inapt-p)
   ("B" "Switch variant" niwa-menu--switch-variant-here)])

(transient-define-group niwa-menu--configure-group
  ["Configure"
   :pad-keys t
   ("m" niwa-menu--open-mode
    :description niwa-menu--mode-description)
   ("M" niwa-menu--open-model
    :description niwa-menu--model-description)
   ("P" niwa-menu--open-preset
    :description niwa-menu--preset-summary)
   ("u" "Permissions" niwa-menu--open-permissions)
   ("G" niwa-menu--open-goal
    :description niwa-menu--goal-summary)])

(transient-define-group niwa-menu--workspace-group
  ["Workspace"
   :pad-keys t
   ("j" "Standing jobs" niwa-menu--open-schedules)
   ("x" niwa-menu--open-background
    :description niwa-menu--background-description)
   ("w" niwa-menu--open-worktree
    :description niwa-menu--worktree-description)
   ("A" niwa-menu--open-artifacts
    :description niwa-menu--artifacts-description)
   ("l" niwa-menu--open-memory
    :description niwa-menu--memory-description)])

(transient-define-group niwa-menu--collaborate-group
  ["Collaborate"
   :pad-keys t
   ("S" niwa-menu--open-share
    :description niwa-menu--share-summary)
   ("n" "New shared item" niwa-artifacts-new-shared-item
    :inapt-if niwa-menu--read-only-p)
   ("C" niwa-menu--open-control
    :description niwa-menu--control-summary)])

(transient-define-group niwa-menu--extend-group
  ["Extend"
   :pad-keys t
   ("t" niwa-menu--open-tools
    :description niwa-menu--tools-description)
   ("s" niwa-menu--open-skills
    :description niwa-menu--skills-description)
   ("p" niwa-menu--open-plugins
    :description niwa-menu--plugins-description)])

(defconst niwa-menu--utilities
  '(("d" "Data view" niwa-menu--toggle-data-view)
    ("i" "Session info" niwa-menu--open-session-info)
    ("U" "Usage" niwa-subscription-usage-show)
    ("g" "gptel menu" niwa-menu--open-gptel)
    ("?" "Help" niwa-menu--open-help))
  "Utility suffixes of the top menu, shown in rows beneath its groups.")

(defun niwa-menu--utility-suffixes (start &optional end)
  "Return `niwa-menu--utilities' from START to END as parsed suffixes."
  (transient-parse-suffixes
   'niwa-menu--top (seq-subseq niwa-menu--utilities start end)))

(defun niwa-menu--utilities-row (_children)
  "Return every utility suffix, for a single footer row."
  (niwa-menu--utility-suffixes 0))

(defun niwa-menu--utilities-first-row (_children)
  "Return the utilities of a split footer's first row."
  (niwa-menu--utility-suffixes 0 3))

(defun niwa-menu--utilities-second-row (_children)
  "Return the utilities of a split footer's second row."
  (niwa-menu--utility-suffixes 3))

(transient-define-prefix niwa-menu--top ()
  "Top-level niwa session cockpit.
The groups are arranged by `niwa-menu--layout'; utilities share a row."
  [:description niwa-menu--header :if niwa-menu--wide-p
   niwa-menu--conversation-group niwa-menu--history-group
   niwa-menu--configure-group niwa-menu--workspace-group
   niwa-menu--collaborate-group niwa-menu--extend-group]
  [:description niwa-menu--header :if niwa-menu--medium-p
   niwa-menu--conversation-group niwa-menu--history-group
   niwa-menu--configure-group]
  [:if niwa-menu--medium-p
   niwa-menu--workspace-group niwa-menu--collaborate-group
   niwa-menu--extend-group]
  [:description niwa-menu--header :if niwa-menu--narrow-p
   niwa-menu--conversation-group niwa-menu--history-group]
  [:if niwa-menu--narrow-p
   niwa-menu--configure-group niwa-menu--workspace-group]
  [:if niwa-menu--narrow-p
   niwa-menu--collaborate-group niwa-menu--extend-group]
  [:class transient-row :if-not niwa-menu--split-footer-p
   :setup-children niwa-menu--utilities-row]
  [:class transient-row :if niwa-menu--split-footer-p
   :setup-children niwa-menu--utilities-first-row]
  [:class transient-row :if niwa-menu--split-footer-p
   :setup-children niwa-menu--utilities-second-row]
  (interactive)
  (niwa-menu-open 'top))

(transient-define-prefix niwa-menu--share ()
  "Share cockpit surface: this session's room and the workspace lobby.
Every action goes through the same commands as `/collab'."
  [:description niwa-menu--share-description
   ["Session"
    :pad-keys t
    ("s" "Show links, or start sharing" niwa-menu--share-session)
    ("x" "Stop sharing" niwa-menu--share-stop
     :inapt-if niwa-menu--not-shared-p)]
   ["Lobby"
    :pad-keys t
    ("l" "Show links, or start the lobby" niwa-menu--share-lobby)
    ("L" "Stop the lobby" niwa-menu--share-lobby-stop
     :inapt-if niwa-menu--lobby-down-p)
    ("r" "Rotate lobby links…" niwa-menu--share-lobby-rotate)]
   ["Artifacts"
    :pad-keys t
    ("A" "Share one, or show links…" niwa-menu--share-artifact)
    ("Z" "Replace its links…" niwa-menu--rotate-artifact)
    ("U" "Stop sharing one…" niwa-menu--revoke-artifact)]
   ["Remote artifacts"
    :pad-keys t
    ("R" "Attach from a link…" niwa-menu--attach-remote-artifact)
    ("D" "Detach…" niwa-menu--detach-remote-artifact)]
   ["Inspect"
    :pad-keys t
    ("i" "Status" niwa-menu--share-status)
    ("a" "Choose any share…" niwa-menu--share-choose
     :inapt-if niwa-menu--no-shares-p)
    ("X" "Stop all…" niwa-menu--share-stop-all
     :inapt-if niwa-menu--no-shares-p)]]
  [:class transient-column
   :description niwa-menu--share-active-description
   :if-not niwa-menu--no-shares-p
   :setup-children niwa-menu--share-active-rows]
  (interactive)
  (niwa-menu--context)
  (niwa-menu--setup 'niwa-menu--share))

(transient-define-prefix niwa-menu--share-item ()
  "One share or lobby chosen on the Share surface.
Every action goes through the collaboration module, which refuses a share
that stopped meanwhile."
  [:description niwa-menu--share-item-description
   ["Share"
    :pad-keys t
    ("o" "Open session" niwa-menu--share-item-open
     :if niwa-menu--share-item-room-p)
    ("s" "Show links" niwa-menu--share-item-links
     :if-not niwa-menu--share-item-recorded-p)
    ("R" "Restart and show links" niwa-menu--share-item-links
     :if niwa-menu--share-item-recorded-p)
    ("x" niwa-menu--share-item-stop
     :description niwa-menu--share-item-stop-description)]]
  (interactive)
  (niwa-diagnostic-user-error (niwa-cockpit-diagnostic-scope)
                               "Choose a share on the Share surface"))

(transient-define-prefix niwa-menu--control ()
  "Session control cockpit surface.

Who may write this session, and what a non-owner sees of the owner's work.
The same surface serves both sides: a client that wants control and a host
that is being asked for it are the same protocol seen from two ends."
  [:description niwa-menu--control-description
   ["Control"
    :pad-keys t
    ("t" "Take control" niwa-menu--take-control
     :inapt-if-not niwa-menu--read-only-p)
    ("q" "Release control" niwa-menu--release-control
     :inapt-if niwa-menu--read-only-p)
    ("g" "Grant" niwa-menu--grant-control
     :inapt-if-not niwa-menu--transfer-pending-p)
    ("k" "Keep" niwa-menu--keep-control
     :inapt-if-not niwa-menu--transfer-pending-p)]
   ["Follow"
    :pad-keys t
    ("f" niwa-menu--toggle-follow
     :description niwa-menu--follow-description
     :inapt-if-not niwa-menu--read-only-p
     :transient t)
    ("r" "Refresh now" niwa-menu--refresh-session
     :inapt-if-not niwa-menu--read-only-p
     :transient t)]
   ["Inspect"
    :pad-keys t
    ("i" "Session info" niwa-menu--open-session-info)]]
  (interactive)
  (niwa-menu--context)
  (niwa-menu--setup 'niwa-menu--control))

(transient-define-prefix niwa-menu--navigate ()
  "Transcript navigation cockpit surface.
Every entry stays open so repeated motion needs one menu, and the keys
match the ones the view buffer itself binds."
  [:description niwa-menu--navigate-description
   ["Segment"
    :pad-keys t
    ("[" "Previous"
     (lambda () (interactive)
       (niwa-menu--call-in-view #'niwa-view-previous-segment))
     :transient t)
    ("]" "Next"
     (lambda () (interactive)
       (niwa-menu--call-in-view #'niwa-view-next-segment))
     :transient t)
    ("g" "Go to…"
     (lambda () (interactive)
       (niwa-menu--call-in-view #'niwa-view-go-to-segment))
     :transient t)
    ("c" "Chat…"
     (lambda () (interactive)
       (niwa-menu--call-in-view #'niwa-view-go-to-chat))
     :transient t)]
   ["Display"
    :pad-keys t
    ("n" "Next"
     (lambda () (interactive)
       (niwa-menu--call-in-view #'niwa-view-next-display))
     :transient t)
    ("p" "Previous"
     (lambda () (interactive)
       (niwa-menu--call-in-view #'niwa-view-previous-display))
     :transient t)]
   ["Query"
    :pad-keys t
    ("C-n" "Next"
     (lambda () (interactive)
       (niwa-menu--call-in-view #'niwa-view-next-user-query))
     :transient t)
    ("C-p" "Previous"
     (lambda () (interactive)
       (niwa-menu--call-in-view #'niwa-view-previous-user-query))
     :transient t)]
   ["Section"
    :pad-keys t
    ("TAB" "Toggle"
     (lambda () (interactive)
       (niwa-menu--call-in-view #'niwa-view-toggle-section))
     :transient t)
    ("q" "Back" niwa-menu)]]
  (interactive)
  (niwa-menu--context)
  (niwa-menu--setup 'niwa-menu--navigate))

(transient-define-prefix niwa-menu--mode ()
  "Permission mode cockpit surface."
  [:description niwa-menu--mode-surface-description
   ["Permission mode"
    :pad-keys t
    ("k" (lambda () (interactive) (niwa-menu--set-mode 'ask))
     :description niwa-menu--mode-ask-description)
    ("e" (lambda () (interactive) (niwa-menu--set-mode 'edits))
     :description niwa-menu--mode-edits-description)
    ("f" (lambda () (interactive) (niwa-menu--set-mode 'full-auto))
     :description niwa-menu--mode-full-auto-description)]
   ["Conversation"
    :pad-keys t
    ("p" niwa-menu--enter-plan
     :description niwa-menu--mode-plan-description)
    ("q" "Back" niwa-menu)]]
  (interactive)
  (niwa-menu--context)
  (niwa-menu--setup 'niwa-menu--mode))

(transient-define-prefix niwa-menu--model-selection ()
  "Shared caller-owned model-selection surface."
  [:description niwa-menu--model-selection-description
   ["Model"
    :pad-keys t
    ("RET" "Choose model…" niwa-menu--model-selection-select-model
     :transient t)
    ;; Bind the GUI Return event too, without advertising it twice.
    ("<return>" "Choose model…" niwa-menu--model-selection-select-model
     :transient t :if (lambda (&rest _) nil))
    ("s" "Use session model" niwa-menu--model-selection-reset
     :if (lambda () (plist-get (transient-scope) :reset))
     :transient t)]
   ["Effort"
    :pad-keys t
    ("e" niwa-menu--model-selection-cycle-effort
     :description niwa-menu--model-selection-effort-description
     :transient t)
    ("q" "Back" transient-quit-one)]])

(transient-define-prefix niwa-menu--goal ()
  "Goal cockpit surface."
  [:description niwa-menu--goal-description
   ["Lifecycle"
    :pad-keys t
    ("s" "Start Goal" niwa-menu--goal-start
     :inapt-if niwa-menu--goal-start-inapt-p)
    ("p" "Pause" (lambda () (interactive)
                     (niwa-menu--goal-call #'niwa-goal-pause))
     :inapt-if-not niwa-menu--goal-active-p)
    ("r" "Resume" (lambda () (interactive)
                      (niwa-menu--goal-call #'niwa-goal-resume))
     :inapt-if-not niwa-menu--goal-resumable-p)
    ("c" "Clear" (lambda () (interactive)
                     (niwa-menu--goal-call #'niwa-goal-clear))
     :inapt-if-not niwa-menu--goal-clearable-p)]
   ["Adjust"
    :pad-keys t
    ("e" "Edit objective" niwa-menu--goal-edit
     :inapt-if-not niwa-menu--current-goal)
    ("b" "Set budget" niwa-menu--goal-budget
     :inapt-if-not niwa-menu--current-goal)]
   ["Inspect"
    :pad-keys t
    ("i" "Goal record" niwa-menu--open-goal-record)
    ("q" "Back" niwa-menu)]]
  (interactive)
  (niwa-menu--context)
  (niwa-menu--setup 'niwa-menu--goal))

(transient-define-prefix niwa-menu--preset ()
  "Preset and model-team cockpit surface."
  [:description niwa-menu--preset-description
   ["Preset"
    :pad-keys t
    ("RET" "Choose preset…" niwa-menu--select-preset)
    ("g" "gptel menu" niwa-menu--open-gptel)]
   ["Inspect"
    :pad-keys t
    ("i" "Model policy report" niwa-menu--open-preset-report)
    ("q" "Back" niwa-menu)]]
  (interactive)
  (niwa-menu--context)
  (niwa-menu--setup 'niwa-menu--preset))

(provide 'niwa-menu)

;;; niwa-menu.el ends here
