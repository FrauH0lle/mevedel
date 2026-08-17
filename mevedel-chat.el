;;; mevedel-chat.el -- Chat buffer management and directive processing -*- lexical-binding: t -*-

;;; Commentary:

;; Chat session lifecycle: creates the gptel data buffer, wires up the
;; workspace, tool list, presets, and agents on it, and attaches the
;; corresponding `mevedel-view' buffer for user-facing display.
;; Supports multiple concurrent sessions per workspace (switch via
;; `mevedel-switch-session').
;;
;; Also submits accepted Plan prompt transactions through the ordinary
;; request path after `mevedel-plan-handoff' has prepared their context.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'gptel)
  (require 'gptel-request)
  (require 'mevedel-presets))

(require 'mevedel-hooks)

;; `cl-extra'
(declare-function cl-some "cl-extra" (cl-pred cl-seq &rest cl-rest))

;; `cl-lib'
(declare-function cl-oddp "cl-lib" (integer))

;; `cl-seq'
(declare-function cl-delete "cl-seq" (cl-item cl-seq &rest cl-keys))
(declare-function cl-find-if "cl-seq" (cl-pred cl-list &rest cl-keys))
(declare-function cl-position "cl-seq" (cl-item cl-seq &rest cl-keys))
(declare-function cl-remove-if-not "cl-seq" (cl-pred cl-list &rest cl-keys))
(declare-function cl-sort "cl-seq" (cl-seq cl-pred &rest cl-keys))

;; `gptel'
(declare-function gptel-markdown-cycle-block "ext:gptel" nil)
(declare-function gptel-mode "ext:gptel" (&optional arg))
(declare-function gptel-send "ext:gptel" nil)
(defvar gptel--markdown-block-map)
(defvar gptel-backend)
(defvar gptel-default-mode)
(defvar gptel-display-buffer-action)
(defvar gptel-mode)
(defvar gptel-model)
(defvar gptel-pre-tool-call-functions)
(defvar gptel-reasoning-effort)
(defvar gptel-send--handlers)
(defvar gptel-send--transitions)

;; `gptel-org'
(defvar gptel-org-branching-context)
(defvar gptel-org-ignore-elements)

;; `gptel-request'
(declare-function gptel-abort "ext:gptel-request" (buf))
(declare-function gptel-fsm-info "ext:gptel-request")
(declare-function gptel-fsm-state "ext:gptel-request")
(declare-function gptel-make-fsm "ext:gptel-request" (&rest args))
(declare-function gptel-request "ext:gptel-request")
(defvar gptel--request-alist)
(defvar gptel-org-convert-response)
(defvar gptel-prompt-prefix-alist)
(defvar gptel-prompt-transform-functions)
(defvar gptel-response-separator)
(defvar gptel-stream)

;; `mevedel-agents'
(declare-function mevedel-agent-invocation-parent-data-buffer
		  "mevedel-agents" (cl-x) t)

;; `mevedel-compact'
(declare-function mevedel--compact-transform-auto "mevedel-compact"
		  (continue fsm))
(defvar mevedel--compaction-cancel)

;; `mevedel-directive'
(declare-function mevedel-directive-actions "mevedel-directive" (directive))
(declare-function mevedel-directive-next-activity-sequence
                  "mevedel-directive" (directive))
(declare-function mevedel-directive-recompute-state
                  "mevedel-directive" (directive))
(declare-function mevedel-directive-remove-subdirective
                  "mevedel-directive" (directive subdirective))

;; `mevedel-directive-frame'
(declare-function mevedel-directive-frame-display
                  "mevedel-directive-frame"
                  (directive view-buffer &optional focus))

;; `mevedel-directive-plan'
(declare-function mevedel-directive-plan-start
                  "mevedel-directive-plan"
                  (directive action prompt-fn callback))

;; `mevedel-execution'
(declare-function mevedel-execution-acknowledge-unknown
                  "mevedel-execution" (session))
(declare-function mevedel-execution-mutation-blocked-p
                  "mevedel-execution" (session))

;; `mevedel-execution-target'
(declare-function mevedel-execution-target-probe
                  "mevedel-execution-target"
                  (target &optional refresh sandbox-mode))
(declare-function mevedel-execution-target-readiness-message
                  "mevedel-execution-target" (target))
(declare-function mevedel-execution-target-remote-p
                  "mevedel-execution-target" (target))

;; `mevedel-hooks'
(declare-function mevedel-hooks-event-plist "mevedel-hooks"
		  (event &optional session workspace &rest extra))
(declare-function mevedel-hooks-record-session-context "mevedel-hooks"
		  (session decision &optional event))
(declare-function mevedel-hooks-run-event "mevedel-hooks"
		  (event event-plist callback &optional session
			 workspace request invocation))

;; `mevedel-models'
(declare-function mevedel-model-apply-session-policy
                  "mevedel-models" (session &optional buffer))
(declare-function mevedel-model-resolve-provider
                  "mevedel-models" (spec &optional noerror))
(declare-function mevedel-model-validate-effort
                  "mevedel-models" (model effort))

;; `mevedel-overlays'
(declare-function mevedel--delete-instruction "mevedel-overlays"
		  (instruction))
(declare-function mevedel--detached-directive-p "mevedel-overlays"
		  (directive))
(declare-function mevedel--directive-llm-prompt "mevedel-overlays"
		  (directive))
(declare-function mevedel--directive-record "mevedel-overlays" (directive))
(declare-function mevedel--directive-text "mevedel-overlays"
		  (directive))
(declare-function mevedel--find-directive-by-uuid "mevedel-overlays"
		  (uuid))
(declare-function mevedel--highest-priority-instruction
		  "mevedel-overlays"
		  (instructions &optional non-processing))
(declare-function mevedel--instruction-with-uuid "mevedel-overlays"
                  (uuid &optional workspace))
(declare-function mevedel--instructions-at "mevedel-overlays"
		  (position &optional type))
(declare-function mevedel--reconcile-directive-sources "mevedel-overlays"
			  (workspace))
(declare-function mevedel--remove-directive-presentation
                  "mevedel-overlays" (directive))
(declare-function mevedel--set-directive-status "mevedel-overlays"
		  (directive status))
(declare-function mevedel--submitted-subdirectives "mevedel-overlays"
                  (directive))
(declare-function mevedel--topmost-instruction "mevedel-overlays"
		  (instruction type))
(declare-function mevedel--update-instruction-overlay
		  "mevedel-overlays" (instruction &optional force))
(declare-function mevedel-get-directive-patch "mevedel-overlays" (directive))

;; `mevedel-permissions'
(declare-function mevedel-permission-mode-set-raw
		  "mevedel-permissions" (mode))
(declare-function mevedel-permission-validate-persistent-stores
                  "mevedel-permissions" (workspace))

;; `mevedel-pipeline'
(declare-function mevedel-pipeline--format-render-data-block
		  "mevedel-pipeline" (render-data))
(declare-function mevedel-pipeline--strip-render-data-blocks
		  "mevedel-pipeline" (string))

;; `mevedel-plan-handoff'
(declare-function mevedel-plan-handoff--append-implementation-input
                  "mevedel-plan-handoff" (prompt selection))
(declare-function mevedel-plan-handoff--validate-skill-bindings
                  "mevedel-plan-handoff" (prompt session))

;; `mevedel-plan-mode'
(declare-function mevedel-plan-approval-abort "mevedel-plan-mode"
		  (&optional session outcome))
(declare-function mevedel-plan-mode--post-response "mevedel-plan-mode"
		  (start end))
(declare-function mevedel-plan-mode-restore-pending-approval
		  "mevedel-plan-mode" (&optional session chat-buffer))

;; `mevedel-plugins'
(declare-function mevedel-plugins-notify-pending-consent
		  "mevedel-plugins" (&optional workspace))

;; `mevedel-presets'
(declare-function mevedel-preset--build-handlers "mevedel-presets"
                  (handlers))
(declare-function mevedel-preset--build-transitions "mevedel-presets"
                  (transitions))
(declare-function mevedel-preset-apply "mevedel-presets"
		  (name &optional buffer))
(declare-function mevedel-preset-restore-session "mevedel-presets"
		  (session &optional buffer))
(defvar mevedel--directive-read-only-request-p)
(defvar mevedel-action-preset-alist)
(defvar mevedel-default-chat-preset)

;; `mevedel-prompt-submission'
(declare-function mevedel-prompt-submission-commit
		  "mevedel-prompt-submission" (submission))
(declare-function mevedel-prompt-submission-context
		  "mevedel-prompt-submission" (cl-x) t)
(declare-function mevedel-prompt-submission-input
		  "mevedel-prompt-submission" (cl-x) t)

;; `mevedel-reminders'
(declare-function mevedel-reminders-install-defaults
		  "mevedel-reminders" (session))
(defvar mevedel--session)
(defvar mevedel-permission-mode)

;; `mevedel-session-persistence'
(declare-function
 mevedel-session-persistence--install-gptel-save-state-advice
 "mevedel-session-persistence" nil)
(declare-function mevedel-session-persistence--release-on-kill
		  "mevedel-session-persistence" nil)
(declare-function mevedel-session-persistence-ensure-files
                  "mevedel-session-persistence" (session buffer))
(declare-function mevedel-session-persistence-header-segment
		  "mevedel-session-persistence" nil)
(declare-function mevedel-session-persistence-resume-id
                  "mevedel-session-persistence" (workspace session-id))
(declare-function mevedel-session-persistence-save
		  "mevedel-session-persistence" (session buffer))
(declare-function mevedel-session-persistence-assert-new-mutation-authority
                  "mevedel-session-persistence" (session))

;; `mevedel-skills-core'
(declare-function mevedel-skills--release-on-kill
		  "mevedel-skills-core" nil)
(declare-function mevedel-skills-install "mevedel-skills-core"
		  (session &optional buffer))
(defvar mevedel-skills--pending-request-context)

;; `mevedel-skills-invoke'
(declare-function mevedel-skills-prepare-user-input
                  "mevedel-skills-invoke" (text session))

;; `mevedel-skills-prompt'
(declare-function mevedel-skills-install-activation-hook
		  "mevedel-skills-prompt" nil)
(declare-function mevedel-skills-install-reminder
		  "mevedel-skills-prompt" (session))

;; `mevedel-skills-ui'
(declare-function mevedel-skills--refresh-view-input-prompt
		  "mevedel-skills-ui" nil)
(declare-function mevedel-slash-capf "mevedel-skills-ui" nil)

;; `mevedel-structs'
(declare-function mevedel-directive-attempt--create
                  "mevedel-structs" (&rest slots))
(declare-function mevedel-directive-attempt-capture
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-attempt-captured-at
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-attempt-checkpoint
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-attempt-consumed-subdirectives
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-attempt-directive-request
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-attempt-outcome
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-attempt-patch
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-attempt-plan
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-attempt-request
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-attempt-result
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-attempts "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-discussion "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-discussion-turn--create
                  "mevedel-structs" (&rest slots))
(declare-function mevedel-directive-discussion-turn-directive-request
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-discussion-turn-message
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-discussion-turn-outcome
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-discussion-turn-result
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-request "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-session-id "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-skills "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-state "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-subdirectives
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-goal-reason "mevedel-structs" (cl-x) t)
(declare-function mevedel-goal-status "mevedel-structs" (cl-x) t)
(declare-function mevedel-request-drain-cancellers "mevedel-structs"
		  (request))
(declare-function mevedel-request-end "mevedel-structs" nil)
(declare-function mevedel-request-file-snapshots "mevedel-structs"
		  (cl-x) t)
(declare-function mevedel-request-untracked-effects "mevedel-structs"
                  (cl-x) t)
(declare-function mevedel-session-audit-session "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-create "mevedel-structs"
		  (name workspace &optional working-directory))
(declare-function mevedel-session-enqueue-pending-reminder "mevedel-structs"
                  (session body))
(declare-function mevedel-session-execution-target
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-name "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-pending-reminders "mevedel-structs"
                  (cl-x) t)
(declare-function mevedel-session-permission-mode "mevedel-structs"
		  (cl-x) t)
(declare-function mevedel-session-session-id "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-turn-count "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-working-directory "mevedel-structs"
		  (cl-x) t)
(declare-function mevedel-session-workspace "mevedel-structs" (cl-x) t)
(declare-function mevedel-subdirective-id "mevedel-structs" (cl-x) t)
(declare-function mevedel-subdirective-request "mevedel-structs" (cl-x) t)
(declare-function mevedel-workspace-id "mevedel-structs" (cl-x) t)
(declare-function mevedel-workspace-name "mevedel-structs" (cl-x) t)
(declare-function mevedel-workspace-root "mevedel-structs" (cl-x) t)
(declare-function mevedel-workspace-type "mevedel-structs" (cl-x) t)
(defvar mevedel--current-request)

;; `mevedel-tool-fs'
(declare-function mevedel-tools--generate-diff "mevedel-tool-fs"
		  (original modified filepath))

;; `mevedel-transcript-audit'
(declare-function mevedel--format-hook-audit-record
                  "mevedel-transcript-audit" (record))

;; `mevedel-utilities'
(declare-function mevedel--clear-user-turn-gptel-properties
		  "mevedel-utilities" (start end))
(declare-function mevedel--optimize-transcript-buffer
		  "mevedel-utilities" nil)
(declare-function mevedel--transcript-org-mode "mevedel-utilities" nil)

;; `mevedel-view'
(declare-function mevedel-view--ensure "mevedel-view" (data-buf))
(defvar mevedel--agent-invocation)
(defvar mevedel--data-buffer)
(defvar mevedel--view-buffer)

;; `mevedel-view-composer'
(declare-function mevedel-view--begin-external-turn
		  "mevedel-view-composer"
		  (display-text data-turn-start &optional kind
				hook-context no-spinner))

;; `mevedel-view-render'
(declare-function mevedel-view--full-rerender "mevedel-view-render" ())

;; `mevedel-collaboration'
(declare-function mevedel-collaboration--safe-accepted-prompt
                  "mevedel-collaboration" (data-buffer))
(declare-function mevedel-collaboration--safe-post-response
                  "mevedel-collaboration" (start end))
(declare-function mevedel-collaboration--safe-post-stream
                  "mevedel-collaboration" nil)

;; `mevedel-view-stream'
(declare-function mevedel-view-stream-post-tool "mevedel-view-stream"
		  (args))
(declare-function mevedel-view-stream-pre-tool "mevedel-view-stream"
		  (args))
(declare-function mevedel-view-stream-render-response
		  "mevedel-view-stream" (start end))
(declare-function mevedel-view-stream-schedule "mevedel-view-stream"
		  nil)
(declare-function mevedel-view-stream-spinner-hook
		  "mevedel-view-stream" (info))
(declare-function mevedel-view-stream-stop "mevedel-view-stream" nil)

;; `mevedel-workspace'
(declare-function mevedel-workspace "mevedel-workspace"
		  (&optional buffer))
(defvar mevedel--workspace)
(defvar mevedel-workspace-additional-roots)

;; `org'
(defvar org-agenda-file-menu-enabled)

;; `org-element'
(declare-function org-element-cache-reset "ext:org-element"
		  (&optional all no-persistence))
(defvar org-element-cache-persistent)
(defvar org-element-use-cache)

;; `org-src'
(declare-function org-escape-code-in-string "ext:org-src" (s))

;;
;;; Customization

(defcustom mevedel-show-patch-buffer nil
  "Control if the mevedel patch buffer should be shown automatically.

If non-nil, the patch buffer will automatically be displayed after a
query completes."
  :type 'boolean
  :group 'mevedel)

(defvar mevedel--diff-preview-buffer-name "*mevedel-diff-preview*"
  "Name of the `diff' preview buffer.")

(defcustom mevedel-show-chat-buffer 'frame
  "How a directive request displays its execution-session view.

`frame' opens a directive frame anchored at the directive, without
taking focus.  `window' displays the view in an ordinary window using
`gptel-display-buffer-action'.  nil displays nothing.

A directive frame falls back to a window wherever child frames are
unavailable, so `frame' is safe on a terminal."
  :type '(choice (const :tag "Directive frame" frame)
                 (const :tag "Ordinary window" window)
                 (const :tag "Do not display" nil))
  :group 'mevedel)

;;
;;; Buffer management

(defun mevedel--chat-buffer-disable-org-element-cache ()
  "Disable Org's element cache in the current mevedel transcript buffer.

Mevedel keeps chat data buffers in `org-mode' so gptel can persist state
in org properties, but the buffer is not a normal hand-edited Org
document: gptel and mevedel insert hidden regions, property runs, and
generated Markdown-shaped list text throughout the file.  Org's
incremental element cache can become stale under those edits, which then
makes ordinary commands such as `org-cycle' fail while trying to resync
the cache.  Keeping the cache disabled locally preserves `org-mode'
editing and folding while forcing Org to parse freshly when it needs
structural information.

Also keeps gptel's Org prompt preparation on the fast path by stripping
only property drawers.  Other `gptel-org-ignore-elements' values require
a full Org element parse of every request transcript.

Finally disables expensive UI/checking minor modes in the hidden
transcript buffer; the user-facing mevedel view remains responsible for
interactive display."
  (when (fboundp 'org-element-cache-reset)
    (let ((org-element-use-cache t))
      (ignore-errors
        (org-element-cache-reset nil 'no-persistence))))
  (setq-local org-element-use-cache nil)
  (setq-local org-element-cache-persistent nil)
  (setq-local gptel-org-ignore-elements '(property-drawer))
  (require 'mevedel-utilities)
  (mevedel--optimize-transcript-buffer))

(defun mevedel-chat-prepare-transcript-buffer ()
  "Prepare the current buffer as a gptel org transcript data buffer.

Sets the transcript major mode, disables Org's element cache, keeps
model responses as raw Markdown (`gptel-org-convert-response' nil), and
enables `gptel-mode'.  The major-mode change calls
`kill-all-local-variables', so buffer-locals set before this call are
wiped unless permanent-local."
  (let ((org-agenda-file-menu-enabled nil)
        (org-element-use-cache nil)
        (org-element-cache-persistent nil))
    (require 'mevedel-utilities)
    (mevedel--transcript-org-mode))
  (mevedel--chat-buffer-disable-org-element-cache)
  (setq-local gptel-org-convert-response nil)
  (setq-local gptel-org-branching-context nil)
  (require 'gptel)
  (gptel-mode +1))

(defun mevedel-chat-install-request-hooks ()
  "Install buffer-local tool-repair and view-stream request hooks.

Repairs raw model input before view hooks observe the call and before
gptel maps the arguments into the pipeline wrapper.  Renders incremental
view updates on tool boundaries so the user sees progress per tool call,
and debounces mid-turn text updates a few times per second while the LLM
is producing text; tool-boundary hooks cancel the pending timer and
render immediately, so this never delays tool-call feedback."
  (require 'mevedel-view-stream)
  (require 'mevedel-tool-repair)
  (add-hook 'gptel-post-response-functions
            #'mevedel-view-stream-render-response nil t)
  (add-hook 'gptel-pre-tool-call-functions
            #'mevedel-tool-repair-pre-tool-call -100 t)
  (add-hook 'gptel-post-tool-call-functions
            #'mevedel-tool-repair-post-tool-call -100 t)
  (add-hook 'gptel-post-response-functions
            #'mevedel-tool-repair-clear-ledger nil t)
  (add-hook 'kill-buffer-hook #'mevedel-tool-repair-clear-ledger nil t)
  (add-hook 'gptel-pre-tool-call-functions
            #'mevedel-view-stream-spinner-hook nil t)
  (add-hook 'gptel-pre-tool-call-functions
            #'mevedel-view-stream-pre-tool nil t)
  (add-hook 'gptel-post-tool-call-functions
            #'mevedel-view-stream-post-tool nil t)
  (add-hook 'gptel-post-stream-hook #'mevedel-view-stream-schedule nil t)
  (add-hook 'gptel-post-stream-hook
            #'mevedel-collaboration--safe-post-stream nil t)
  (add-hook 'gptel-post-response-functions
            #'mevedel-collaboration--safe-post-response nil t))

(defun mevedel--chat-buffer (session-name &optional create workspace working-directory)
  "Get or create the mevedel chat buffer SESSION-NAME for WORKSPACE.

This buffer is where LLM interactions occur.  If CREATE is non-nil,
create the buffer if it doesn't exist.  WORKSPACE should be a
`mevedel-workspace' struct, or nil to use the current buffer's
workspace.

WORKING-DIRECTORY is used only when creating a fresh session.  If an
existing live session with SESSION-NAME has a different working
directory, signal `user-error' instead of silently switching context."
  (let* ((workspace (or workspace (mevedel-workspace)))
         (buf (mevedel--get-buffer session-name workspace create))
         (created-p (cdr buf))
         (buf (car buf))
         (working-directory (and working-directory
                                 (file-name-as-directory
                                  (expand-file-name working-directory)))))
    (when created-p
      (mevedel--chat-buffer-setup
       buf workspace session-name working-directory))
    (when (and buf working-directory (not created-p))
      (with-current-buffer buf
        (when (and (bound-and-true-p mevedel--session)
                   (not (equal working-directory
                               (mevedel-session-working-directory
                                mevedel--session))))
          (user-error "Session %s already uses working directory %s"
                      session-name
                      (mevedel-session-working-directory
                       mevedel--session)))))
    buf))

(defun mevedel--tutor-buffer (&optional create workspace)
  "Get or create the mevedel tutor buffer for WORKSPACE.

This buffer is where LLM interactions occur.  If CREATE is non-nil,
create the buffer if it doesn't exist.  WORKSPACE should be a
`mevedel-workspace' struct, or nil to use the current buffer's
workspace."
  (let* ((workspace (or workspace (mevedel-workspace)))
         (buf (mevedel--get-buffer "tutor" workspace create))
         (created-p (cdr buf))
         (buf (car buf)))
    (when created-p
      (mevedel--chat-buffer-setup buf workspace "tutor"))
    buf))

(defvar-local mevedel--session-start-hooks-pending nil
  "Non-nil while asynchronous SessionStart hooks are still running.")

(defun mevedel--probe-session-target (session &optional refresh)
  "Probe SESSION's remote execution target.

When REFRESH is non-nil, discard the live readiness cache first.  Local
sessions keep their existing startup behavior and return nil."
  (let ((target (mevedel-session-execution-target session)))
    (when (and target
               (progn
                 (require 'mevedel-execution-target)
                 (mevedel-execution-target-remote-p target)))
      (mevedel-execution-target-probe
       target refresh (mevedel-session-sandbox-mode session)))))

;;;###autoload
(defun mevedel-retry-target-readiness ()
  "Force a fresh readiness probe for the current remote session."
  (interactive)
  (let* ((data-buffer
          (cond
           ((bound-and-true-p mevedel--session) (current-buffer))
           ((and (boundp 'mevedel--data-buffer)
                 (buffer-live-p mevedel--data-buffer))
            mevedel--data-buffer)
           (t (mevedel--active-chat-buffer))))
         (session (and (buffer-live-p data-buffer)
                       (buffer-local-value 'mevedel--session data-buffer)))
         (target (and session (mevedel-session-execution-target session))))
    (unless session
      (user-error "No mevedel session here"))
    (unless (and target
                 (progn
                   (require 'mevedel-execution-target)
                   (mevedel-execution-target-remote-p target)))
      (user-error "Current mevedel session is local"))
    (let ((readiness (mevedel--probe-session-target session t)))
      (when (eq 'ready (plist-get readiness :status))
        (require 'mevedel-execution)
        (when (mevedel-execution-mutation-blocked-p session)
          (unless
              (yes-or-no-p
               (concat
                "The target is reachable, but a previous remote process "
                "may still be running. Acknowledge that outcome and allow "
                "new mutating execution? "))
            (user-error "Unknown remote execution remains unacknowledged"))
          (mevedel-execution-acknowledge-unknown session)))
      (when-let* ((view-buffer
                   (buffer-local-value 'mevedel--view-buffer data-buffer))
                  ((buffer-live-p view-buffer)))
        (with-current-buffer view-buffer
          (force-mode-line-update t)))
      (message "mevedel: execution target %s"
               (mevedel-execution-target-readiness-message target))
      readiness)))

(defun mevedel--run-session-start-hooks (source)
  "Run session-start hooks for the current buffer with SOURCE."
  (when (bound-and-true-p mevedel--session)
    (setf (mevedel-session-workspace-instruction-hashes mevedel--session)
          (unless (equal source "resume")
            (cl-delete
             "/root"
             (mevedel-session-workspace-instruction-hashes mevedel--session)
             :key #'caar :test #'equal))))
  (run-hooks 'mevedel-session-start-hook)
  (when (bound-and-true-p mevedel--session)
    (let ((buffer (current-buffer))
          (workspace (or (and (boundp 'mevedel--workspace)
                              mevedel--workspace)
                         (mevedel-session-workspace mevedel--session)))
          done)
      (setq-local mevedel--session-start-hooks-pending t)
      (mevedel-hooks-run-event
       'SessionStart
       (mevedel-hooks-event-plist
        'SessionStart mevedel--session workspace
        :source source)
       (lambda (decision)
         (when (buffer-live-p buffer)
           (with-current-buffer buffer
             (mevedel-hooks-record-session-context
              mevedel--session decision 'SessionStart)
             (setq-local mevedel--session-start-hooks-pending nil)))
         (setq done t))
       mevedel--session workspace nil nil)
      (while (not done)
        (accept-process-output nil 0.05)))))

(defun mevedel--run-session-end-hooks ()
  "Run native and declarative session-end hooks for the current buffer."
  (run-hooks 'mevedel-session-end-hook)
  (when (bound-and-true-p mevedel--session)
    (let ((workspace (or (and (boundp 'mevedel--workspace)
                              mevedel--workspace)
                         (mevedel-session-workspace mevedel--session))))
      (mevedel-hooks-run-event
       'SessionEnd
       (mevedel-hooks-event-plist
        'SessionEnd mevedel--session workspace
        :reason "kill-buffer")
       #'ignore mevedel--session workspace nil nil))))

(defun mevedel--queue-reconciliation-reminder (session)
  "Queue one recovery-state reminder for SESSION."
  (let ((body
         (concat
          "A previous session or request ended without proving that all "
          "effects settled. Processes may still be running, and aborted "
          "tools or commands may have partially changed files, tasks, or "
          "external state. Reconcile current state before continuing, "
          "prioritize the newest user request over any older ghost request, "
          "and verify effects before making final success claims.")))
    (unless (member body (mevedel-session-pending-reminders session))
      (mevedel-session-enqueue-pending-reminder session body))))

(defun mevedel--chat-buffer-init-common (buf workspace source
                                             &optional inspection-p)
  "Set up BUF for WORKSPACE and start its lifecycle with SOURCE.

Caller must already have set BUF's buffer-local `mevedel--session'.
Wires the FSM handler chain, header-line, visual settings, all
per-buffer hooks, the skill set, default reminders, and the companion
view buffer.

Both `mevedel--chat-buffer-setup' (fresh path) and restore paths
\(`mevedel-session-persistence-restore') call this after planting the
session struct. SOURCE is \"startup\", \"resume\", or \"fork\".  When
INSPECTION-P is non-nil, skip lifecycle hooks because the client has no
mutation lease."
  (with-current-buffer buf
    (when (derived-mode-p 'org-mode)
      (mevedel--chat-buffer-disable-org-element-cache))
    (setq-local gptel-org-convert-response nil)
    (setq-local gptel-org-branching-context nil)
    (mevedel-preset-restore-session mevedel--session buf)
    (require 'mevedel-models)
    (mevedel-model-apply-session-policy mevedel--session buf)
    (mevedel-reminders-install-defaults mevedel--session)
    (when (equal source "resume")
      (mevedel--queue-reconciliation-reminder mevedel--session))
    (require 'mevedel-goal)
    ;; Install the mevedel-augmented FSM handler chain as the buffer-local
    ;; `gptel-send--handlers' so every request from this buffer -- whether
    ;; driven by `gptel-send', `mevedel--process-directive', or
    ;; `mevedel--implement-plan' -- picks up the deferred-tool WAIT handler
    ;; and the terminal-state handlers (patch generation, callbacks,
    ;; cleanup, turn-count increment).  Building once at setup time keeps
    ;; the handlers stateless and idempotent across requests.
    (setq-local gptel-send--handlers
                (mevedel-preset--build-handlers
                 (copy-tree (default-value 'gptel-send--handlers))))
    (setq-local gptel-send--transitions
                (mevedel-preset--build-transitions
                 (copy-tree (default-value 'gptel-send--transitions))))
    ;; Wrap lines
    (visual-line-mode +1)
    ;; Auto-scroll when at end of buffer
    (setq-local window-point-insertion-type t)
    ;; Install the session working directory before cwd-dependent setup
    ;; such as skill discovery and dynamic prompt construction.
    (setq-local default-directory
                (or (mevedel-session-working-directory mevedel--session)
                    (mevedel-workspace-root workspace)))
    (mevedel--probe-session-target mevedel--session)
    (require 'mevedel-permissions)
    (mevedel-permission-validate-persistent-stores workspace)
    ;; Make workspace-additional-roots buffer-local for session-specific
    ;; access grants.  Restore path may have already set this from the
    ;; sidecar's `:additional-roots'; don't clobber.
    (unless (local-variable-p 'mevedel-workspace-additional-roots)
      (setq-local mevedel-workspace-additional-roots
                  (copy-alist mevedel-workspace-additional-roots)))
    ;; Per-completed-turn auto-save is installed as part of the DONE-state
    ;; transaction by `mevedel-preset--build-handlers'.  Loading the module
    ;; here pulls in `kill-buffer-hook' and
    ;; ensures handlers can reach the save function.
    (require 'mevedel-session-persistence)
    (require 'mevedel-view-stream)
    ;; gptel owns its `before-save-hook'; mevedel advises the save
    ;; function so dynamic preset system prompts are not serialized as
    ;; frozen `GPTEL_SYSTEM' strings.
    (mevedel-session-persistence--install-gptel-save-state-advice)
    ;; Release the session lock when the chat buffer is killed.
    (add-hook 'kill-buffer-hook
              #'mevedel-session-persistence--release-on-kill nil t)
    (unless inspection-p
      (add-hook 'kill-buffer-hook
                #'mevedel--run-session-end-hooks nil t))
    (mevedel-chat-install-request-hooks)
    (add-hook 'gptel-post-response-functions
              #'mevedel-plan-mode--post-response t t)
    ;; Install slash-command and $skill completion-at-point.
    (add-hook 'completion-at-point-functions #'mevedel-slash-capf nil t)
    ;; Populate session skills from workspace skill dirs
    (mevedel-skills-install mevedel--session (current-buffer))
    (require 'mevedel-plugins)
    (mevedel-plugins-notify-pending-consent workspace)
    ;; Drop this buffer from the skill watcher registry on kill so any
    ;; orphaned `file-notify' watchers are torn down.
    (add-hook 'kill-buffer-hook
              #'mevedel-skills--release-on-kill nil t)
    ;; Register skill event reminders on the session.
    (mevedel-skills-install-reminder mevedel--session)
    ;; Activate conditional skills when a tool touches a matching file
    (mevedel-skills-install-activation-hook)
    ;; Create the companion view buffer
    (require 'mevedel-view)
    (mevedel-view--ensure buf)
    (when (fboundp 'mevedel-plan-mode-restore-pending-approval)
      (mevedel-plan-mode-restore-pending-approval mevedel--session buf))
    (when (fboundp 'mevedel-directive-plan-restore-pending)
      (mevedel-directive-plan-restore-pending mevedel--session buf))
    (unless inspection-p
      (mevedel--run-session-start-hooks source))))

(defun mevedel--chat-buffer-setup (buf workspace session-name &optional working-directory)
  "Set up chat buffer BUF in WORKSPACE with SESSION-NAME and WORKING-DIRECTORY."
  (with-current-buffer buf
    ;; The data buffer is locked to `org-mode' so the persistence layer
    ;; has a single format to round-trip via `gptel-org--save-state'.
    (mevedel-chat-prepare-transcript-buffer)
    ;; Create session after mode setup so it isn't wiped
    (setq-local mevedel--session
                (mevedel-session-create
                 session-name workspace working-directory))
    (mevedel--chat-buffer-init-common buf workspace "startup")))

(defun mevedel--patch-buffer (&optional create workspace)
  "Get or create the mevedel patch staging buffer for WORKSPACE.

This buffer shows diffs generated by the LLM that are awaiting review
and application.  If CREATE is non-nil, create the buffer if it doesn't
exist.  WORKSPACE should be a `mevedel-workspace' struct, or nil to use
the current buffer's workspace."
  (let* ((buf (mevedel--get-buffer "patch" workspace create))
         (created-p (cdr buf))
         (buf (car buf)))
    (when created-p
      (with-current-buffer buf
        (diff-mode)
        (setq buffer-read-only t)))
    buf))

(defun mevedel--get-buffer (name &optional workspace create-p)
  "Get or create a mevedel buffer named NAME in WORKSPACE.

NAME is a string used in the buffer name.  For session buffers, use the
session name (e.g., \"main\", \"tutor\").  For auxiliary buffers, use a
descriptive name (e.g., \"patch\").

Buffer name format: *mevedel:NAME@WORKSPACE*.

Returns (BUFFER . CREATED-P) where CREATED-P indicates if buffer was
created. When CREATE-P is non-nil and buffer doesn't exist, create it
with workspace."
  (let* ((workspace (or workspace (mevedel-workspace)))
         (workspace-name (mevedel-workspace-name workspace))
         (buf-name (format "*mevedel:%s@%s*" name workspace-name))
         (target-buf (get-buffer buf-name))
         created-p)
    (when (and (not target-buf) create-p)
      (setq target-buf (get-buffer-create buf-name)
            created-p t)
      (with-current-buffer target-buf
        ;; Cache workspace struct for pre-session access
        (setq-local mevedel--workspace workspace)))
    (when target-buf
      (cons target-buf created-p))))

(defun mevedel--workspace-sessions (workspace)
  "Return alist of (SESSION-NAME . BUFFER) for WORKSPACE.

Scans live buffers for those with a `mevedel--session' whose workspace
matches WORKSPACE by type and id.

Skips view, retained-agent, and side-conversation buffers.  Those buffers
carry a session for local context but are not themselves root chat data
buffers."
  (let ((ws-type (mevedel-workspace-type workspace))
        (ws-id (mevedel-workspace-id workspace))
        sessions)
    (dolist (buf (buffer-list))
      (when (and (buffer-live-p buf)
                 (not (buffer-local-value 'mevedel--data-buffer buf))
                 (not (buffer-local-value 'mevedel--agent-invocation buf)))
        (when-let* ((session (buffer-local-value 'mevedel--session buf))
                    ;; Transient conversations audit into a durable
                    ;; parent session and are not root chat buffers.
                    ((not (mevedel-session-audit-session session)))
                    (sw (mevedel-session-workspace session))
                    ((eq (mevedel-workspace-type sw) ws-type))
                    ((equal (mevedel-workspace-id sw) ws-id)))
          (push (cons (mevedel-session-name session) buf) sessions))))
    (nreverse sessions)))

(defun mevedel--pick-session (sessions default)
  "Prompt for a session name via `completing-read'.

SESSIONS is an alist of (NAME . BUFFER) for the current workspace.
DEFAULT is the initial input; nil means no default.  Typing a name not
in SESSIONS creates a new session with that name."
  (let ((names (mapcar #'car sessions)))
    (completing-read "Session: " names nil nil nil nil default)))

(defun mevedel--display-chat-buffer (chat-buffer)
  "Ensure CHAT-BUFFER has a preset and display its view."
  (with-current-buffer chat-buffer
    (unless (mevedel-session-preset-name mevedel--session)
      (mevedel-preset-apply
       (alist-get mevedel-default-chat-preset mevedel-action-preset-alist))))
  (display-buffer (or (buffer-local-value 'mevedel--view-buffer chat-buffer)
                      chat-buffer)
                  gptel-display-buffer-action))

(defun mevedel--normalize-session-directory (directory workspace)
  "Return DIRECTORY as an absolute directory inside WORKSPACE."
  (let* ((dir (file-name-as-directory (expand-file-name directory)))
         (root (file-name-as-directory
                (expand-file-name (mevedel-workspace-root workspace)))))
    (unless (file-directory-p dir)
      (user-error "%s is not a directory" dir))
    (unless (file-in-directory-p dir root)
      (user-error "Working directory must be inside workspace root %s"
                  root))
    dir))

(defun mevedel--read-session-directory (workspace)
  "Read a session working directory under WORKSPACE."
  (mevedel--normalize-session-directory
   (read-directory-name "Start mevedel in directory: "
                        (mevedel-workspace-root workspace)
                        (mevedel-workspace-root workspace)
                        t)
   workspace))

(defun mevedel--default-session-name-for-directory (workspace working-directory)
  "Return a default session name for WORKING-DIRECTORY in WORKSPACE."
  (let* ((root (file-name-as-directory
                (expand-file-name (mevedel-workspace-root workspace))))
         (dir (file-name-as-directory (expand-file-name working-directory)))
         (relative (directory-file-name (file-relative-name dir root))))
    (if (or (equal relative "") (equal relative "."))
        "main"
      (replace-regexp-in-string "/" ":" relative t t))))

(defun mevedel--sessions-in-working-directory (sessions working-directory)
  "Filter SESSIONS to those whose session cwd is WORKING-DIRECTORY."
  (let ((dir (file-name-as-directory (expand-file-name working-directory))))
    (delq nil
          (mapcar
           (lambda (entry)
             (let ((buf (cdr entry)))
               (when (and (buffer-live-p buf)
                          (with-current-buffer buf
                            (and (bound-and-true-p mevedel--session)
                                 (equal dir
                                        (mevedel-session-working-directory
                                         mevedel--session)))))
                 entry)))
           sessions))))

(defun mevedel--start-chat (workspace working-directory prompt-session
                                      &optional directory-scoped)
  "Start or switch to a chat in WORKSPACE with WORKING-DIRECTORY.

When PROMPT-SESSION is non-nil, prompt for the target session.  When
DIRECTORY-SCOPED is non-nil, only sessions whose working directory matches
WORKING-DIRECTORY are considered."
  (let* ((all-sessions (mevedel--workspace-sessions workspace))
         (sessions (if directory-scoped
                       (mevedel--sessions-in-working-directory
                        all-sessions working-directory)
                     all-sessions))
         (default-name
          (if directory-scoped
              (mevedel--default-session-name-for-directory
               workspace working-directory)
            "main"))
         (session-name
          (cond
           (prompt-session (mevedel--pick-session sessions default-name))
           ((null sessions) default-name)
           ((= (length sessions) 1) (caar sessions))
           (t (mevedel--pick-session sessions default-name))))
         (existing (assoc session-name sessions))
         (target-directory
          (if existing
              (with-current-buffer (cdr existing)
                (mevedel-session-working-directory mevedel--session))
            working-directory))
         (chat-buffer (mevedel--chat-buffer
                       session-name t workspace target-directory)))
    (mevedel--display-chat-buffer chat-buffer)))

(defun mevedel--active-chat-buffer (&optional workspace)
  "Find the active chat (data) buffer for WORKSPACE.

Returns the gptel data buffer, never the view buffer.

If already in a mevedel chat buffer, return it.  If in a view
buffer, return the associated data buffer.  If in a sub-agent
buffer, return the invocation's `parent-data-buffer'
when live, otherwise fall through to the scan branch.  Otherwise
scan for session buffers matching WORKSPACE: if one exists return
it, if multiple return the most recently used one.  Returns nil
if none found."
  (cond
   ;; in an agent buffer, return the parent chat buffer
   ;; (not the agent buffer itself, which would falsely look like
   ;; a chat buffer because it carries the parent's session).
   ((and (boundp 'mevedel--agent-invocation) mevedel--agent-invocation)
    (let ((parent (mevedel-agent-invocation-parent-data-buffer
                   mevedel--agent-invocation)))
      (if (and parent (buffer-live-p parent))
          parent
        ;; Parent is dead: fall through to the scan branch.
        (when-let* ((workspace (or workspace (mevedel-workspace)))
                    (sessions (mevedel--workspace-sessions workspace)))
          (if (= (length sessions) 1)
              (cdar sessions)
            (let ((buf-list (buffer-list)))
              (cdr (car (cl-sort (copy-sequence sessions) #'<
                                 :key (lambda (s)
                                        (or (cl-position (cdr s) buf-list)
                                            most-positive-fixnum)))))))))))
   ;; In a view buffer -- return the associated data buffer
   ((and (boundp 'mevedel--data-buffer) mevedel--data-buffer
         (buffer-live-p mevedel--data-buffer))
    mevedel--data-buffer)
   ;; Already in a chat buffer with a session.  Check this after the
   ;; view-buffer case because rendered views also mirror the session.
   ((and (boundp 'mevedel--session) mevedel--session)
    (current-buffer))
   ;; Search for session buffers
   (t
    (when-let* ((workspace (or workspace (mevedel-workspace)))
                (sessions (mevedel--workspace-sessions workspace)))
      (if (= (length sessions) 1)
          (cdar sessions)
        ;; Multiple sessions: return most recently used (earliest in buffer-list)
        (let ((buf-list (buffer-list)))
          (cdr (car (cl-sort (copy-sequence sessions) #'<
                             :key (lambda (s)
                                    (or (cl-position (cdr s) buf-list)
                                        most-positive-fixnum)))))))))))

(defun mevedel--generate-final-patch (&optional workspace)
  "Generate final diffs for all tracked files in current request.

Return a unified diff string showing original -> final state for each
file.  Uses the active request's snapshots to compare original states
with current file contents in WORKSPACE."
  (let ((diffs "")
        (workspace-root (mevedel-workspace-root
                         (or workspace (mevedel-workspace))))
        paths)
    (when mevedel--current-request
      (maphash (lambda (filepath _original) (push filepath paths))
               (mevedel-request-file-snapshots mevedel--current-request)))
    (dolist (filepath (sort paths #'string<))
      (let* ((original (gethash filepath
                                (mevedel-request-file-snapshots
                                 mevedel--current-request)))
             (current (when (file-regular-p filepath)
                        (with-temp-buffer
                          (insert-file-contents filepath)
                          (buffer-string))))
             (relpath (file-relative-name filepath workspace-root)))

        ;; Generate diff if file changed, was deleted, or was created
        (when (and (not (and (listp original)
                             (plist-get original :gap)))
                   (or
               ;; Modified
               (and original current (not (string= original current)))
               ;; Deleted
               (and original (not current))
               ;; Created
               (and (not original) current)))
          (setq diffs (concat diffs
                              (format "diff --git a/%s b/%s\n" relpath relpath)
                              (cond
                               ((and (or (not original) (string-empty-p original))
                                     (and current (not (string-empty-p current))))
                                "new file mode 100644\n")
                               ((and (and original (not (string-empty-p original)))
                                     (or (not current) (string-empty-p current)))
                                "deleted file mode 100644\n"))
                              (mevedel-tools--generate-diff
                               (or original "")
                               (or current "")
                               relpath)
                              "\n")))))
    diffs))

(defun mevedel--directive-capture (request)
  "Return file coverage metadata captured by REQUEST."
  (let (covered gaps untracked-effects)
    (when request
      (setq untracked-effects
            (copy-tree (mevedel-request-untracked-effects request)))
      (maphash
       (lambda (path original)
         (if (and (listp original) (plist-get original :gap))
             (push (cons path (plist-get original :gap)) gaps)
           (push path covered)))
       (mevedel-request-file-snapshots request)))
    (list :capture (if (or gaps untracked-effects) 'incomplete 'complete)
          :covered-files (sort covered #'string<)
          :gaps (sort gaps (lambda (left right)
                             (string< (car left) (car right))))
          :untracked-effects
          (sort untracked-effects
                (lambda (left right) (string< (car left) (car right)))))))

(defun mevedel--replace-patch-buffer (patch-content)
  "Replace patch buffer contents with PATCH-CONTENT.
If PATCH-CONTENT is empty, does nothing."
  (when (and patch-content (> (length patch-content) 0))
    (with-current-buffer (mevedel--patch-buffer t)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert patch-content)
        (diff-mode)
        (goto-char (point-min))))
    (mevedel--indicate-patch-ready)))

(defun mevedel--indicate-patch-ready ()
  "Provide visual feedback that a patch is ready for review."
  (message "Patch ready in *mevedel-patch* buffer")
  (when mevedel-show-patch-buffer
    (display-buffer (mevedel--patch-buffer))))

;;;###autoload
(defun mevedel-clear-patch-buffer ()
  "Clear the patch buffer."
  (interactive)
  (when-let ((buf (mevedel--patch-buffer)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)))
    (message "Patch buffer cleared")))


;;
;;; Prompt generation

(defun mevedel--implement-directive-prompt (content)
  "Generate an implementation prompt for CONTENT in the current buffer."
  (format
   "## TASK: Implement the following request.

### INSTRUCTIONS:

1. Read and understand the implementation request below
2. Read and understand all provided references
3. Use the references to complete the request
4. Use your tools as needed
5. Create working, complete code that fulfills the request

### IMPLEMENTATION REQUEST:

%s"
   content))

(defun mevedel--request-changes-prompt
    (content directive feedback &optional new-context-p)
  "Build focused Request changes context for DIRECTIVE.
CONTENT contains the current request and freshly resolved references.  FEEDBACK
may be empty only when NEW-CONTEXT-P says new subdirectives supply the changes."
  (let* ((attempt (car (last (mevedel-directive-attempts directive))))
         (feedback (string-trim (or feedback ""))))
    (unless (memq 'request-changes (mevedel-directive-actions directive))
      (user-error "Request changes requires a successful current attempt"))
    (when (and (string-empty-p feedback) (not new-context-p))
      (user-error "Request changes requires feedback or new subdirectives"))
    (format
     "## TASK: Implement requested changes to the directive.

Current repository state is authoritative. Inspect it before editing; the
preceding patch below is historical evidence, not a patch to reapply.

### CURRENT REQUEST AND FRESH REFERENCES:

%s

### REQUESTED CHANGES:

%s

### IMMEDIATELY PRECEDING ATTEMPT (historical)

Captured at: %s
Capture completeness: %s

Answer:
%s

Historical observed patch:
%s%s"
     content
     (if (string-empty-p feedback)
         "Use the newly supplied directive context as the requested changes."
       feedback)
     (mevedel-directive-attempt-captured-at attempt)
     (upcase (symbol-name (mevedel-directive-attempt-capture attempt)))
     (mevedel-directive-attempt-result attempt)
     (mevedel-directive-attempt-patch attempt)
     (if-let* ((plan (mevedel-directive-attempt-plan attempt)))
         (format "\n\nAccepted plan from that attempt:\n%s" plan)
       ""))))

(defun mevedel--retry-directive-prompt (content directive guidance)
  "Build focused Retry context for DIRECTIVE from CONTENT and GUIDANCE."
  (let* ((attempt (car (last (mevedel-directive-attempts directive))))
         (guidance (string-trim (or guidance ""))))
    (unless (memq 'retry (mevedel-directive-actions directive))
      (user-error "Retry requires a failed or aborted current attempt"))
    (format
     "## TASK: Retry the directive implementation.

Current repository state is authoritative. Inspect it before editing; the
preceding partial patch is diagnostic evidence, not a patch to reapply.

### CURRENT REQUEST AND FRESH REFERENCES:

%s%s

### IMMEDIATELY PRECEDING FAILURE:

Captured at: %s
Capture completeness: %s

%s

Observed partial changes:
%s%s"
     content
     (if (string-empty-p guidance)
         ""
       (format "\n\n### OPTIONAL GUIDANCE:\n\n%s" guidance))
     (mevedel-directive-attempt-captured-at attempt)
     (upcase (symbol-name (mevedel-directive-attempt-capture attempt)))
     (mevedel-directive-attempt-result attempt)
     (mevedel-directive-attempt-patch attempt)
     (if-let* ((plan (mevedel-directive-attempt-plan attempt)))
         (format "\n\nAccepted plan from that attempt:\n%s" plan)
       ""))))

(defun mevedel--directive-bound-session-buffer (record workspace)
  "Return RECORD's live bound execution session buffer in WORKSPACE, or nil."
  (when-let* ((bound-id (mevedel-directive-session-id record)))
    (cl-loop
     for (_ . candidate) in (mevedel--workspace-sessions workspace)
     when (and (buffer-live-p candidate)
               (equal bound-id
                      (with-current-buffer candidate
                        (and (bound-and-true-p mevedel--session)
                             (mevedel-session-session-id mevedel--session)))))
     return candidate)))

(defun mevedel--attach-directive-skills (prompt record chat-buffer)
  "Append RECORD's selected skills to PROMPT and validate in CHAT-BUFFER.
Each skill's current source is reloaded at dispatch; a missing,
disabled, or malformed selection signals before any request starts."
  (if-let* ((skills (mevedel-directive-skills record)))
      (with-current-buffer chat-buffer
        (require 'mevedel-plan-handoff)
        (require 'mevedel-skills-invoke)
        (let ((result (mevedel-plan-handoff--append-implementation-input
                       prompt (list :skills skills))))
          (setq result (mevedel-skills-prepare-user-input
                        result mevedel--session))
          (mevedel-plan-handoff--validate-skill-bindings
           result mevedel--session)
          result))
    prompt))

(defun mevedel--directive-discussion-transcript (directive)
  "Return DIRECTIVE's current-request local discussion as plain text."
  (mapconcat
   (lambda (turn)
     (format "User: %s\nAssistant%s: %s"
             (mevedel-directive-discussion-turn-message turn)
             (if (eq (mevedel-directive-discussion-turn-outcome turn)
                     'success)
                 ""
               (format " (%s)"
                       (mevedel-directive-discussion-turn-outcome turn)))
             (string-trim-right
              (mevedel-pipeline--strip-render-data-blocks
               (mevedel-directive-discussion-turn-result turn)))))
   (cl-remove-if-not
    (lambda (turn)
      (equal (mevedel-directive-request directive)
             (mevedel-directive-discussion-turn-directive-request turn)))
    (mevedel-directive-discussion directive))
   "\n\n"))

(defun mevedel--discuss-directive-prompt
    (content &optional directive message attempt-index)
  "Generate a read-only discussion prompt from CONTENT.
When DIRECTIVE and MESSAGE are non-nil, include the complete directive-local
discussion.  ATTEMPT-INDEX attaches that implementation result."
  (let* ((discussion
          (and directive
               (mevedel--directive-discussion-transcript directive)))
         (attempt
          (and attempt-index
               (nth (1- attempt-index)
                    (mevedel-directive-attempts directive)))))
    (when (and attempt-index (not attempt))
      (user-error "Directive has no implementation attempt %d"
                  attempt-index))
    (format
     "## TASK: Answer the following request.

### INSTRUCTIONS:

1. Read and understand the request below
2. Read and understand all provided references
3. Use the references to complete the request
4. Use your tools to access files as needed

### REQUEST:

%s%s%s"
     content
     (if (and discussion (not (string-empty-p discussion)))
         (format "\n\n### LOCAL DISCUSSION:\n\n%s" discussion)
       "")
     (concat
      (if attempt
          (format
           "\n\n### SELECTED IMPLEMENTATION ATTEMPT %d:\n\nRequest:\n%s\n\nResult:\n%s\n\nObserved patch:\n%s"
           attempt-index
           (mevedel-directive-attempt-request attempt)
           (mevedel-directive-attempt-result attempt)
           (mevedel-directive-attempt-patch attempt))
        "")
      (if message
          (format "\n\n### QUESTION:\n\n%s" message)
        "")))))

(defun mevedel--implement-discussion-prompt (content directive)
  "Generate an implementation prompt from CONTENT and DIRECTIVE discussion."
  (let ((discussion (mevedel--directive-discussion-transcript directive)))
    (unless (memq 'implement-this (mevedel-directive-actions directive))
      (user-error "Implement this requires a current discussion"))
    (when (string-empty-p discussion)
      (user-error "Directive has no discussion to implement"))
    (concat
     (mevedel--implement-directive-prompt content)
     "\n\n### DISCUSSION FEEDBACK:\n\n"
     discussion)))

(defun mevedel--directive-implementation-prompt
    (content directive &optional feedback)
  "Build DIRECTIVE's complete next implementation prompt from CONTENT.
FEEDBACK supplies requested changes or optional retry guidance."
  (let ((actions (mevedel-directive-actions directive)))
    (cond
     ((memq 'implement-this actions)
     (mevedel--implement-discussion-prompt content directive))
     ((memq 'request-changes actions)
      (mevedel--request-changes-prompt
       content directive feedback (mevedel-directive-subdirectives directive)))
     ((memq 'retry actions)
      (mevedel--retry-directive-prompt content directive feedback))
     ((memq 'implement actions)
      (mevedel--implement-directive-prompt content))
     (t (user-error "Directive action is already in progress")))))


;;
;;; Directive processing

(defvar-local mevedel--current-directive-uuid nil
  "UUID of the directive currently being processed.")

(defconst mevedel--directive-action-labels
  '((implement . "Implement")
    (request-changes . "Request changes")
    (retry . "Retry")
    (plan . "Plan")
    (discuss . "Discuss")
    (tutor . "Tutor"))
  "Plain display labels for directive actions.")

(defun mevedel--directive-action-label (action)
  "Return the display label for directive ACTION."
  (or (alist-get (if (symbolp action) action (intern-soft action))
                 mevedel--directive-action-labels)
      (capitalize (replace-regexp-in-string
                   "[-_]+" " " (format "%s" action)))))

(defun mevedel--directive-display-text (action directive-text)
  "Return the human-facing transcript text for ACTION and DIRECTIVE-TEXT."
  (let ((label (mevedel--directive-action-label action)))
    (if (string-empty-p (string-trim directive-text))
        label
      (format "%s: %s" label directive-text))))

(defun mevedel--insert-directive-turn
    (directive-id turn directive-text prompt action)
  "Insert a directive turn into the current chat data buffer.

DIRECTIVE-ID and TURN identify the durable directive boundary.
DIRECTIVE-TEXT is the short overlay text shown in the transcript.
PROMPT is the full LLM-facing prompt, inserted in a `:PROMPT:' drawer
for inspection.  Request-time projection excludes the complete turn
from ordinary conversation context.  ACTION is the directive action
symbol.  Return a marker positioned where the assistant response should
be inserted."
  (require 'mevedel-utilities)
  (require 'mevedel-transcript-audit)
  (let* ((summary directive-text)
         (action-str (symbol-name action))
         (is-org-mode (derived-mode-p 'org-mode))
         (header-prefix (if is-org-mode "" (format "`%s` " action-str)))
         (header-postfix (if is-org-mode (format " :%s:" action-str) ""))
         (truncated-summary
          (let* ((lines (split-string summary "\n" t "[[:space:]]*"))
                 (first-line (or (car lines) ""))
                 (prefix (or (alist-get major-mode gptel-prompt-prefix-alist) ""))
                 (used-length (+ (length prefix)
                                 (length header-prefix)
                                 (length header-postfix)))
                 (available-length (max 10 (- (or fill-column 70)
                                              used-length))))
            (truncate-string-to-width first-line available-length nil nil "...")))
         (full-prompt-str
          (if is-org-mode
              (progn
                (require 'org-src)
                (concat ":PROMPT:\n"
                        (org-escape-code-in-string prompt)
                        "\n:END:\n"))
            (concat "``` prompt\n" prompt "\n```\n"))))
    (goto-char (point-max))
    (insert
     (mevedel--format-hook-audit-record
      (list :type 'directive-turn-boundary
            :edge 'start
            :directive-id directive-id
            :action action
            :turn turn)))
    (let ((user-turn-start (point)))
      (unless (bobp)
        (insert gptel-response-separator))
      (when-let* ((prefix (alist-get major-mode gptel-prompt-prefix-alist)))
        (let ((prefix-length (length prefix)))
          (unless (and (>= (point) (+ (point-min) prefix-length))
                       (string=
                        (buffer-substring-no-properties
                         (- (point) prefix-length) (point))
                        prefix))
            (unless (bolp)
              (insert "\n"))
            (insert prefix))))
      (insert (format "%s%s%s\n"
                      header-prefix truncated-summary header-postfix))
      (mevedel--clear-user-turn-gptel-properties user-turn-start (point)))
    (let ((cur-pt (point)))
      (insert (if (derived-mode-p 'markdown-mode)
                  (propertize full-prompt-str
                              'keymap gptel--markdown-block-map)
                full-prompt-str))
      (ignore-errors
        (if (derived-mode-p 'org-mode)
            (save-excursion
              (search-backward ":PROMPT:" cur-pt t)
              (when (looking-at "^:PROMPT:")
                (org-cycle)))
          (save-excursion
            (when (re-search-backward "^```" cur-pt t)
              (gptel-markdown-cycle-block))))))
    (copy-marker (point) nil)))

(defun mevedel--insert-directive-turn-end
    (directive-id turn action outcome activity-kind sequence)
  "Close a directive transcript turn at point.
DIRECTIVE-ID, TURN, ACTION, OUTCOME, ACTIVITY-KIND, and SEQUENCE link the
canonical transcript to its immutable workspace activity record."
  (require 'mevedel-transcript-audit)
  (insert
   (mevedel--format-hook-audit-record
    (list :type 'directive-turn-boundary
          :edge 'end
          :directive-id directive-id
          :action action
          :turn turn
          :outcome outcome
          :activity-kind activity-kind
          :sequence sequence))))

(defun mevedel--directive-save-buffer-p ()
  "Return non-nil when the current buffer should be saved before a directive.

Directive processing should offer to save normal modified file buffers so
subsequent file tools see the current source text on disk.  It should not
prompt for mevedel data or agent transcript buffers; those are persisted by
session/transcript autosave and may be modified while a request is still
settling."
  (and (buffer-file-name)
       (buffer-modified-p)
       (not (bound-and-true-p mevedel--session))
       (not (bound-and-true-p mevedel--agent-invocation))))

(defun mevedel--directive-model-policy (directive)
  "Return DIRECTIVE's resolved request-local model policy, or nil."
  (when-let* ((provider
               (overlay-get directive 'mevedel-directive-model-provider)))
    (require 'mevedel-models)
    (let* ((policy (mevedel-model-resolve-provider provider))
           (effort
            (overlay-get directive 'mevedel-directive-reasoning-effort)))
      (mevedel-model-validate-effort (plist-get policy :model) effort)
      (plist-put policy :effort effort))))

(defun mevedel--directive-session-buffer (directive workspace)
  "Return `(BUFFER . REBIND-P)' for DIRECTIVE in WORKSPACE."
  (let ((session-id (mevedel-directive-session-id directive)))
    (if (not session-id)
        (cons (mevedel--chat-buffer "main" t workspace) nil)
      (or
       (when-let* ((buffer (mevedel--directive-bound-session-buffer
                            directive workspace)))
         (cons buffer nil))
       (progn
         (require 'mevedel-session-persistence)
         (when-let* ((buffer
                      (mevedel-session-persistence-resume-id
                       workspace session-id)))
           (cons buffer nil)))
       (if (yes-or-no-p
            (format "Directive session %s is unavailable; rebind future activity to the current workspace session? "
                    session-id))
           (cons (mevedel--chat-buffer "main" t workspace) t)
         (user-error "Directive remains bound to unavailable session: %s"
                     session-id))))))

(defun mevedel--record-directive-terminal-activity
    (record action directive-text prompt result outcome checkpoint info
            options submitted-subdirectives)
  "Record one terminal directive ACTION and return its activity kind/sequence."
  (let ((sequence
         (and (memq action '(discuss plan implement request-changes retry))
              (mevedel-directive-next-activity-sequence record))))
    (pcase action
      ('plan
       (setf (mevedel-directive-planning record)
             (append
              (mevedel-directive-planning record)
               (list
                (list :sequence sequence
                      :action (plist-get options :planned-action)
                      :directive-request directive-text
                      :message (plist-get options :message)
                      :implementation-prompt
                      (plist-get (mevedel-directive-plan record)
                                 :implementation-prompt)
                      :proposal nil
                      :request prompt :result result :outcome outcome
                      :checkpoint checkpoint))))
       (mevedel-directive-recompute-state record)
       (cons 'planning sequence))
      ('discuss
       (setf (mevedel-directive-discussion record)
             (append
              (mevedel-directive-discussion record)
              (list
               (mevedel-directive-discussion-turn--create
                :sequence sequence :directive-request directive-text
                :message (plist-get options :message) :request prompt
                :result result :outcome outcome
                :attempt-index (plist-get options :attempt-index)
                :checkpoint checkpoint))))
       (mevedel-directive-recompute-state record)
       (cons 'discussion sequence))
      ((or 'implement 'request-changes 'retry)
       (setf (mevedel-directive-attempts record)
             (append
              (mevedel-directive-attempts record)
              (list
               (mevedel-directive-attempt--create
                :sequence sequence :action action
                :directive-request directive-text
                :request prompt :result result :outcome outcome
                :patch (or (plist-get info :mevedel-directive-patch) "")
                :capture (or (plist-get info :mevedel-directive-capture)
                             'incomplete)
                :covered-files
                (plist-get info :mevedel-directive-covered-files)
                :gaps (plist-get info :mevedel-directive-gaps)
                :untracked-effects
                (plist-get info :mevedel-directive-untracked-effects)
                :captured-at (format-time-string "%FT%T%z")
                :checkpoint checkpoint
                :plan (copy-tree (plist-get options :plan))
                :plan-context
                (and (plist-get options :plan)
                     (list
                      :request directive-text
                      :subdirectives
                      (mapcar
                       (lambda (subdirective)
                         (cons (mevedel-subdirective-id subdirective)
                               (mevedel-subdirective-request subdirective)))
                       submitted-subdirectives)))
                :plan-selection
                (copy-tree (plist-get options :plan-selection))
                :consumed-subdirectives
                (and (eq outcome 'success) submitted-subdirectives)))))
       (mevedel-directive-recompute-state record)
       (cons 'attempt sequence))
      ('tutor (cons 'tutor nil))
      (_ (error "Unknown directive action: %S" action)))))

(defun mevedel--consume-directive-subdirectives
    (record submitted-subdirectives workspace live-directive)
  "Consume SUBMITTED-SUBDIRECTIVES after LIVE-DIRECTIVE succeeds."
  (with-current-buffer (overlay-buffer live-directive)
    (dolist (submitted submitted-subdirectives)
      (let ((id (mevedel-subdirective-id submitted)))
        (if-let* ((child-directive
                   (mevedel--instruction-with-uuid id workspace)))
            (mevedel--delete-instruction child-directive)
          (when-let* ((current
                       (cl-find id (mevedel-directive-subdirectives record)
                                :key #'mevedel-subdirective-id :test #'equal)))
            (mevedel-directive-remove-subdirective record current)))))
    (save-excursion
      (goto-char (overlay-start live-directive))
      (unless (mevedel--detached-directive-p live-directive)
        (overlay-put live-directive 'evaporate t)))))

(defun mevedel--settle-directive-presentation
    (live-directive record workspace implementation-p submitted-subdirectives
                    err)
  "Settle LIVE-DIRECTIVE presentation after its terminal request."
  (when-let* ((live-directive live-directive)
              (directive-buffer (overlay-buffer live-directive)))
    (mevedel--set-directive-status
     live-directive (mevedel-directive-state record))
    (when (and err implementation-p)
      (overlay-put live-directive 'mevedel-directive-fail-reason
                   (if (eq err 'abort) "aborted" (format "%s" err))))
    (when (and implementation-p (not err))
      (mevedel--consume-directive-subdirectives
       record submitted-subdirectives workspace live-directive))
    (mevedel--update-instruction-overlay live-directive t)
    (with-current-buffer directive-buffer
      (pulse-momentary-highlight-region
       (overlay-start live-directive) (overlay-end live-directive)))))

(defun mevedel--directive-request-error (exit-code fsm)
  "Return the terminal error for EXIT-CODE and FSM, or nil on success."
  (cond
   (exit-code)
   ((eq (gptel-fsm-state fsm) 'ERRS)
    (let* ((info (gptel-fsm-info fsm))
           (error (plist-get info :error))
           (message (plist-get error :message)))
      (or message
          (format "%s: %s"
                  (plist-get error :type)
                  (plist-get info :status)))))))

(defun mevedel--send-directive-request
    (prompt chat-buffer response-start preset model-policy callback)
  "Send a directive PROMPT and invoke CALLBACK with its terminal error and FSM."
  (mevedel-with-preset preset
		       (let* ((request-callback
			       (lambda (exit-code fsm)
				 (funcall callback
					  (mevedel--directive-request-error exit-code fsm)
					  fsm)))
			      (fsm
			       (gptel-request
				prompt
				:buffer chat-buffer
				:position response-start
				:stream gptel-stream
				:transforms
				(append
				 gptel-prompt-transform-functions
				 (and model-policy
				      (list
				       (lambda (_fsm)
					 (setq-local
					  gptel-backend (plist-get model-policy :backend)
					  gptel-model (plist-get model-policy :model)
					  gptel-reasoning-effort
					  (plist-get model-policy :effort))))))
				:fsm (gptel-make-fsm :handlers gptel-send--handlers)))
			      (info (gptel-fsm-info fsm))
			      (fsm-callback (plist-get info :callback))
			      (wrapped-callback
			       (lambda (response &rest rest)
				 "Settle an abort, then pass RESPONSE to the gptel callback."
				 (when (eq response 'abort)
				   (funcall request-callback 'abort fsm))
				 (apply fsm-callback response rest))))
			 (setf (gptel-fsm-info fsm)
			       (plist-put info :callback wrapped-callback))
			 (setf (gptel-fsm-info fsm)
			       (plist-put (gptel-fsm-info fsm)
					  :mevedel-request-callback request-callback))
			 fsm)))

(defun mevedel--process-directive
    (directive preset prompt-fn callback &optional options)
  "Process DIRECTIVE using PRESET and PROMPT-FN, calling CALLBACK when complete.

DIRECTIVE is the instruction overlay to process.
PRESET is the gptel preset to use (mevedel-implement, mevedel-discuss, or
mevedel-tutor).
PROMPT-FN is a function that generates the prompt from the directive
content.
CALLBACK is called with (err fsm) when processing completes.

Updates directive status and overlay, handles success/failure states.
OPTIONS carries local discussion metadata for read-only discussion turns."
  (setq directive
        (or (mevedel--topmost-instruction directive 'directive)
            directive))
  (let ((transient-buffer
         (and (overlay-get directive 'mevedel-transient-source-missing)
              (overlay-buffer directive)))
        cleanup-chat-buffer cleanup-record cleanup-prior-state
        cleanup-request-context-set-p cleanup-request-reserved-p
        cleanup-turn-start cleanup-mode-applied-p cleanup-planning-session)
    (condition-case err
        (let* ((model-policy (or (plist-get options :model-policy)
                                 (mevedel--directive-model-policy directive)))
               ;; Get chat buffer for the directive's buffer workspace
               (workspace (with-current-buffer (overlay-buffer directive)
			    (mevedel-workspace)))
               (record (setq cleanup-record
			     (mevedel--directive-record directive)))
               (_prior-state
		(setq cleanup-prior-state (mevedel-directive-state record)))
               (bound-session-id (mevedel-directive-session-id record))
               (session-choice
		(mevedel--directive-session-buffer record workspace))
               (chat-buffer (setq cleanup-chat-buffer (car session-choice)))
               (rebind-p (cdr session-choice))
               (directive-uuid (overlay-get directive 'mevedel-uuid))
               (directive-text (mevedel--directive-text directive))
               (content (mevedel--directive-llm-prompt directive))
               (action (overlay-get directive 'mevedel-directive-action))
               (planning-p (eq action 'plan))
               (discussion-p (eq action 'discuss))
               (implementation-p
		(memq action '(implement request-changes retry)))
               ;; Directive-selected skills attach to direct
               ;; implementation prompts; an accepted plan's card
               ;; selection (:plan-selection) already carries its own.
               (prompt
                (let ((built (funcall prompt-fn content)))
                  (if (and implementation-p
                           (not (plist-get options :plan-selection)))
                      (mevedel--attach-directive-skills
                       built record chat-buffer)
                    built)))
               (submitted-subdirectives
		(and implementation-p
		     (mevedel--submitted-subdirectives directive)))
               execution-session-id
               reserved-turn
               response-start
               settled-p
               (callback-fn
		(lambda (err fsm)
		  (unless settled-p
		    (setq settled-p t)
		    (let* ((info (gptel-fsm-info fsm))
			   (outcome (cond ((eq err 'abort) 'aborted)
					  (err 'error)
					  (t 'success)))
			   (result
			    (if err
				(if (eq err 'abort)
				    "Request aborted"
				  (format "%s" err))
                              (with-current-buffer chat-buffer
				(buffer-substring-no-properties
				 response-start (point-max)))))
			   (turn reserved-turn)
			   (checkpoint
			    (list :session-id execution-session-id :turn turn))
			   (live-directive
			    (or (and (overlay-buffer directive) directive)
				(mevedel--find-directive-by-uuid directive-uuid)))
			   (activity
			    (mevedel--record-directive-terminal-activity
			     record action directive-text prompt result outcome
			     checkpoint info options submitted-subdirectives)))
                      (with-current-buffer chat-buffer
			(let ((inhibit-read-only t))
			  (goto-char (point-max))
			  (mevedel--insert-directive-turn-end
			   directive-uuid turn action outcome
			   (car activity) (cdr activity)))
			(setq mevedel--current-directive-uuid nil
                              mevedel--directive-read-only-request-p nil))
                      (mevedel--settle-directive-presentation
                       live-directive record workspace implementation-p
                       submitted-subdirectives err)
                      (mevedel--reconcile-directive-sources workspace)
                      (unwind-protect
			  (when callback
			    (funcall callback err fsm))
			(when (buffer-live-p transient-buffer)
			  (when (overlay-buffer directive)
			    (mevedel--remove-directive-presentation directive))
			  (kill-buffer transient-buffer))))))))

          (with-current-buffer chat-buffer
            (when mevedel--current-request
              (user-error "A request is already active -- wait or abort first"))
            (when planning-p
              (when (mevedel-session-plan-mode mevedel--session)
                (user-error
                 "Leave ordinary Plan mode before planning a directive"))
              (when-let* ((active
                           (mevedel-session-directive-planning
                            mevedel--session)))
                (unless (and (plist-get options :plan-continuation)
                             (equal (plist-get active :directive-id)
                                    directive-uuid))
                  (user-error
                   "A directive workflow already occupies this session")))
              (setf (mevedel-session-directive-planning mevedel--session)
                    (list :directive-id directive-uuid
                          :action (plist-get options :planned-action)
                          :phase 'planning))
              (setq cleanup-planning-session mevedel--session))
            (when-let* ((mode (and implementation-p
                                    (plist-get options :permission-mode))))
              (mevedel--implementation-permission-mode-apply mode)
              (setq cleanup-mode-applied-p t))
            (setq cleanup-request-context-set-p t)
	    (setq mevedel--current-directive-uuid
		  (overlay-get directive 'mevedel-uuid)
                  mevedel--directive-read-only-request-p
                  (or discussion-p planning-p))
	    (require 'mevedel-session-persistence)
	    (mevedel-session-persistence-ensure-files mevedel--session chat-buffer)
	    (setq execution-session-id
		  (mevedel-session-session-id mevedel--session)))

	  (save-some-buffers nil #'mevedel--directive-save-buffer-p)

          (when (or discussion-p planning-p implementation-p)
            (mevedel--set-directive-status
             directive (cond (discussion-p 'discussing)
                             (planning-p 'planning)
                             (t 'implementing)))
	    (mevedel--update-instruction-overlay directive t)
	    (pulse-momentary-highlight-region
	     (overlay-start directive) (overlay-end directive)))

	  ;; Display view buffer if configured (fall back to data buffer)
	  (let ((view (or (buffer-local-value 'mevedel--view-buffer chat-buffer)
			  chat-buffer)))
	    (pcase mevedel-show-chat-buffer
	      ;; No focus argument: a request the user just started must not
	      ;; move point into the frame.
	      ('frame
	       (require 'mevedel-directive-frame)
	       (mevedel-directive-frame-display directive view))
	      ('window
	       (display-buffer view gptel-display-buffer-action))))

	(with-current-buffer chat-buffer
	  (require 'mevedel-session-persistence)
	  (mevedel-session-persistence-assert-new-mutation-authority
	   mevedel--session)
	    (mevedel-preset-apply
	     (alist-get mevedel-default-chat-preset mevedel-action-preset-alist))
	    (mevedel-request-begin mevedel--session directive-uuid)
	    (setq cleanup-request-reserved-p t)
	    (setq reserved-turn (mevedel-request-turn mevedel--current-request))
	    (setq cleanup-turn-start (copy-marker (point-max) nil))
	    (setq response-start
		  (mevedel--insert-directive-turn
		   directive-uuid reserved-turn
		   directive-text prompt
		   (overlay-get directive 'mevedel-directive-action)))
	    (when-let* ((view-buf mevedel--view-buffer)
			(_ (buffer-live-p view-buf)))
              (with-current-buffer view-buf
		(mevedel-view--begin-external-turn
		 (mevedel--directive-display-text
		  (overlay-get directive 'mevedel-directive-action)
		  directive-text)
		 response-start
		 'directive)))

	    (let ((fsm
		   (mevedel--send-directive-request
		    prompt chat-buffer response-start preset model-policy callback-fn)))
              (when (or (not bound-session-id) rebind-p)
		(setf (mevedel-directive-session-id record)
                      execution-session-id))
              fsm)))
      (t
       ;; Restore authoritative directive state before a view redraw can
       ;; replace its source presentation.
       (when cleanup-record
         (setf (mevedel-directive-state cleanup-record)
               cleanup-prior-state))
       (when (overlay-buffer directive)
         (mevedel--set-directive-status directive cleanup-prior-state)
         (mevedel--update-instruction-overlay directive t))
       (when (buffer-live-p cleanup-chat-buffer)
         (with-current-buffer cleanup-chat-buffer
           (when (and (markerp cleanup-turn-start)
                      (marker-position cleanup-turn-start))
             (let ((inhibit-read-only t))
               (delete-region cleanup-turn-start (point-max))))
           (when cleanup-request-reserved-p
             (condition-case cleanup-error
                 (mevedel-request-end)
               (error
                (setq mevedel--current-request nil)
                (display-warning
                 'mevedel
                 (format "Directive request cleanup failed: %s"
                         (error-message-string cleanup-error))
                 :warning))))
           (when cleanup-request-context-set-p
             (setq mevedel--current-directive-uuid nil
                   mevedel--directive-read-only-request-p nil)
             (when-let* ((view-buffer mevedel--view-buffer)
                         ((buffer-live-p view-buffer)))
               (with-current-buffer view-buffer
                 (mevedel-view--full-rerender))))))
       (when (and cleanup-mode-applied-p
                  (buffer-live-p cleanup-chat-buffer))
         (with-current-buffer cleanup-chat-buffer
           (mevedel--implementation-permission-mode-restore)))
       (when cleanup-planning-session
         (setf (mevedel-session-directive-planning cleanup-planning-session)
               nil))
       (when (markerp cleanup-turn-start)
         (set-marker cleanup-turn-start nil))
       (when (buffer-live-p transient-buffer)
         (when (overlay-buffer directive)
           (mevedel--remove-directive-presentation directive))
         (kill-buffer transient-buffer))
       (signal (car err) (cdr err))))))

(defun mevedel--start-directive-discussion (directive &optional callback)
  "Submit DIRECTIVE itself as its initial read-only discussion turn.
CALLBACK receives the ordinary directive terminal arguments."
  (setq directive
        (or (mevedel--topmost-instruction directive 'directive)
            directive))
  (let ((record (mevedel--directive-record directive)))
    (unless (memq 'discuss (mevedel-directive-actions record))
      (user-error "Initial discussion requires a Ready directive"))
    (overlay-put directive 'mevedel-directive-action 'discuss)
    (mevedel--process-directive
     directive (alist-get 'discuss mevedel-action-preset-alist)
     #'mevedel--discuss-directive-prompt
     callback
     (list :message (mevedel-directive-request record)))))

(defun mevedel--discuss-directive-turn
    (directive message &optional attempt-index callback)
  "Submit MESSAGE as DIRECTIVE's next read-only discussion turn.
ATTEMPT-INDEX attaches one implementation result.  CALLBACK receives the
ordinary directive terminal arguments."
  (setq directive
        (or (mevedel--topmost-instruction directive 'directive)
            directive))
  (unless (and (stringp message) (not (string-empty-p (string-trim message))))
    (user-error "Discussion message must not be empty"))
  (let ((record (mevedel--directive-record directive)))
    (overlay-put directive 'mevedel-directive-action 'discuss)
    (mevedel--process-directive
     directive (alist-get 'discuss mevedel-action-preset-alist)
     (lambda (content)
       (mevedel--discuss-directive-prompt
        content record message attempt-index))
     callback
     (list :message message :attempt-index attempt-index))))

(defun mevedel--dispatch-directive-implementation
    (directive record action prompt-fn callback)
  "Run RECORD's ACTION implementation for DIRECTIVE, planning first when enabled.
PROMPT-FN builds the implementation prompt from resolved content and
CALLBACK receives the ordinary terminal (err fsm) arguments."
  (if (mevedel-directive-planning-enabled record)
      (progn
        (require 'mevedel-directive-plan)
        (mevedel-directive-plan-start directive action prompt-fn callback))
    (overlay-put directive 'mevedel-directive-action
                 (if (eq action 'implement-this) 'implement action))
    (mevedel--process-directive
     directive (alist-get 'implement mevedel-action-preset-alist)
     prompt-fn callback)))

(defun mevedel--implement-discussion (directive &optional callback)
  "Implement DIRECTIVE using its complete local discussion as feedback."
  (setq directive
        (or (mevedel--topmost-instruction directive 'directive)
            directive))
  (let ((record (mevedel--directive-record directive)))
    (mevedel--dispatch-directive-implementation
     directive record 'implement-this
     (lambda (content)
       (mevedel--implement-discussion-prompt content record))
     callback)))

(defun mevedel--request-directive-changes
    (directive feedback &optional callback)
  "Implement DIRECTIVE again using focused FEEDBACK and latest activity."
  (setq directive
        (or (mevedel--topmost-instruction directive 'directive)
            directive))
  (let* ((record (mevedel--directive-record directive))
         (new-context-p (mevedel-directive-subdirectives record)))
    ;; Validate before changing presentation or starting request setup.
    (mevedel--request-changes-prompt "" record feedback new-context-p)
    (mevedel--dispatch-directive-implementation
     directive record 'request-changes
     (lambda (content)
       (mevedel--request-changes-prompt
        content record feedback new-context-p))
     callback)))

(defun mevedel--retry-directive (directive guidance &optional callback)
  "Retry DIRECTIVE using its latest failure and optional GUIDANCE."
  (setq directive
        (or (mevedel--topmost-instruction directive 'directive)
            directive))
  (let ((record (mevedel--directive-record directive)))
    ;; Validate before changing presentation or starting request setup.
    (mevedel--retry-directive-prompt "" record guidance)
    (mevedel--dispatch-directive-implementation
     directive record 'retry
     (lambda (content)
       (mevedel--retry-directive-prompt content record guidance))
     callback)))

(defun mevedel-abort (&optional buf)
  "Abort any active request associated with buffer BUF.

Thus, abort `gptel' requests running in the mevedel chat buffer
associated with the `mevedel-workspace' for BUF.

If a callback was provided to the original request, it will be called
with the \\='abort symbol as the error parameter.

BUF defaults to the current buffer if not specified."
  (interactive)
  (with-current-buffer (or buf (current-buffer))
    (when-let* ((chat-buffer (mevedel--active-chat-buffer))
                (_ (buffer-live-p chat-buffer)))
      ;; Stop the spinner in the view buffer.  Unrelated to the
      ;; canceller drain but still worth doing up front so the UI
      ;; reflects the teardown while the rest of the sequence runs.
      (when-let* ((view-buf (buffer-local-value 'mevedel--view-buffer
                                                chat-buffer))
                  (_ (buffer-live-p view-buf)))
        (with-current-buffer view-buf
          (mevedel-view-stream-stop)))
      ;; Phase 1: drain the request's cancellers.  Each canceller
      ;; settles its owned overlays with `aborted' so FSMs parked in
      ;; TOOL can advance out. Draining before the
      ;; `gptel-abort' loop is load-bearing -- follow-up HTTP
      ;; requests launched by `aborted' callbacks land in
      ;; `gptel--request-alist' and get torn down in phase 2.
      (with-current-buffer chat-buffer
        (when (bound-and-true-p mevedel--current-request)
          (mevedel--queue-reconciliation-reminder mevedel--session)
          (mevedel-request-drain-cancellers mevedel--current-request))
        ;; flush any queued permission entries with 'aborted
        ;; so callbacks fire and the FSMs they belong to can unwind.
        ;; Run after the canceller drain so canceller-driven entries
        ;; have a chance to settle first.
        (when (fboundp 'mevedel-permission-queue-abort-all)
          (mevedel-permission-queue-abort-all))
        (when (fboundp 'mevedel-plan-approval-abort)
          (mevedel-plan-approval-abort))
        (when (functionp mevedel--compaction-cancel)
          (funcall mevedel--compaction-cancel)))
      ;; Phase 2: loop `gptel-abort'.  It only cancels one request per
      ;; call, so continue until no request owned by this root buffer remains.
      ;; Retained agent turns own separate buffers and continue independently.
      (let* ((inhibit-message t)
             (request-matches-p
              (lambda (entry)
                (let ((buf (plist-get (gptel-fsm-info (cadr entry))
                                      :buffer)))
                  (eq buf chat-buffer)))))
        (while (and (boundp 'gptel--request-alist)
                    gptel--request-alist
                    (cl-some request-matches-p gptel--request-alist))
          ;; Determine which buffer hosts the request we're about to
          ;; cancel; gptel-abort only cancels in-buffer.
          (let* ((entry (cl-find-if request-matches-p
                                    gptel--request-alist))
                 (target (plist-get (gptel-fsm-info (cadr entry))
                                    :buffer)))
            (gptel-abort (or target chat-buffer)))))
      (with-current-buffer chat-buffer
        (when-let* ((goal (and (bound-and-true-p mevedel--session)
                               (mevedel-session-goal mevedel--session)))
                    ((eq (mevedel-goal-status goal) 'active)))
          (setf (mevedel-goal-status goal) 'paused
                (mevedel-goal-reason goal) "interrupted by user"
                (mevedel-goal-updated-at goal)
                (format-time-string "%FT%T%z")))
        (when (bound-and-true-p mevedel--current-request)
          (mevedel-request-end))
        (when (and (bound-and-true-p mevedel--session)
                   (mevedel-session-workspace mevedel--session)
                   (not (bound-and-true-p
                         mevedel-session--read-only-mode)))
          (require 'mevedel-session-persistence)
          (condition-case err
              (mevedel-session-persistence-save mevedel--session chat-buffer)
            (error
             (display-warning
              'mevedel
              (format "Could not save session after abort: %S" err)
              :warning))))))))

;;
;;; Goal implementation

(defvar-local mevedel--implementation-permission-mode-restore nil
  "Wrapped permission mode to restore after Goal implementation.")

(defun mevedel--implementation-permission-mode-apply (mode)
  "Temporarily apply implementation permission MODE for this request."
  (when (and (memq mode '(ask edits full-auto))
             (bound-and-true-p mevedel--session))
    (setq mevedel--implementation-permission-mode-restore
          (list (mevedel-session-permission-mode mevedel--session)))
    (require 'mevedel-permissions)
    (mevedel-permission-mode-set-raw mode)
    (when (fboundp 'mevedel-skills--refresh-view-input-prompt)
      (mevedel-skills--refresh-view-input-prompt))))

(defun mevedel--implementation-permission-mode-restore ()
  "Restore permission mode after a temporary Goal implementation override."
  (when (and mevedel--implementation-permission-mode-restore
             (bound-and-true-p mevedel--session))
    (let ((restore (car mevedel--implementation-permission-mode-restore)))
      (setq mevedel--implementation-permission-mode-restore nil)
      (setf (mevedel-session-permission-mode mevedel--session) restore)
      (if restore
          (setq-local mevedel-permission-mode restore)
        (kill-local-variable 'mevedel-permission-mode))
      (when (and (boundp 'mevedel--view-buffer)
                 (buffer-live-p mevedel--view-buffer))
        (with-current-buffer mevedel--view-buffer
          (if restore
              (setq-local mevedel-permission-mode restore)
            (kill-local-variable 'mevedel-permission-mode))))
      (when (fboundp 'mevedel-skills--refresh-view-input-prompt)
        (mevedel-skills--refresh-view-input-prompt)))))

(defun mevedel--close-unclosed-blocks ()
  "Close any unclosed blocks at the end of the buffer.

When the main FSM is stopped mid-response (e.g., after plan acceptance),
the LLM may have left an open block.  This handles:
- Markdown fenced code blocks (``` reasoning, etc.)
- Org-mode blocks (#+begin_reasoning, etc.)"
  (let ((inhibit-read-only t))
    (save-excursion
      (cond
       ;; Markdown: count ``` fences; odd count means unclosed block
       ((derived-mode-p 'markdown-mode)
        (let ((fence-count 0))
          (goto-char (point-min))
          (while (re-search-forward "^```" nil t)
            (cl-incf fence-count))
          (when (cl-oddp fence-count)
            (goto-char (point-max))
            (unless (bolp) (insert "\n"))
            (insert "```\n")
            (gptel-markdown-cycle-block))))
       ;; Org-mode: find last unclosed #+begin_ block
       ((derived-mode-p 'org-mode)
        (let ((last-open nil))
          (goto-char (point-min))
          (while (re-search-forward
                  "^#\\+\\(begin\\|end\\)_\\([[:alpha:]_]+\\)" nil t)
            (if (string-equal-ignore-case (match-string 1) "begin")
                (setq last-open (match-string 2))
              (setq last-open nil)))
          (when last-open
            (goto-char (point-max))
            (unless (bolp) (insert "\n"))
            (insert (format "#+end_%s\n" last-open))
            (org-cycle))))))))

(defvar-local mevedel--pending-model-input nil
  "One-shot model input replacing the latest stored prompt at request time.")

(defun mevedel--insert-local-user-turn
    (prompt &optional display-text kind hook-context no-spinner)
  "Insert PROMPT as a user turn without sending a request.

DISPLAY-TEXT is mirrored to the view, defaulting to PROMPT.  KIND and
HOOK-CONTEXT are forwarded to `mevedel-view--begin-external-turn',
with NO-SPINNER forwarded when non-nil."
  (require 'mevedel-utilities)
  (goto-char (point-max))
  (let ((user-turn-start (point)))
    (insert gptel-response-separator)
    (when-let* ((prefix (alist-get major-mode gptel-prompt-prefix-alist)))
      (let ((prefix-length (length prefix)))
        (unless (and (>= (point) (+ (point-min) prefix-length))
                     (string= (buffer-substring-no-properties
                               (- (point) prefix-length) (point))
                              prefix))
          (unless (bolp) (insert "\n"))
          (insert prefix))))
    (insert prompt "\n")
    (mevedel--clear-user-turn-gptel-properties user-turn-start (point))
    (when (and display-text (not (equal display-text prompt)))
      (require 'mevedel-pipeline)
      (let ((start (point)))
        (insert (mevedel-pipeline--format-render-data-block
                 (list :kind 'user-display :text display-text)))
        (add-text-properties start (point) '(gptel ignore)))))
  (mevedel-collaboration--safe-accepted-prompt (current-buffer))
  (let ((data-turn-start (copy-marker (point) nil)))
    (when-let* ((view (and (boundp 'mevedel--view-buffer)
                           mevedel--view-buffer))
                ((buffer-live-p view))
                ((fboundp 'mevedel-view--begin-external-turn)))
      (with-current-buffer view
        (mevedel-view--begin-external-turn
         (or display-text prompt) data-turn-start kind hook-context
         no-spinner)))
    data-turn-start))

(defun mevedel--submit-generated-turn
    (prompt &optional display-text prompt-submission prepared-outcome)
  "Insert and send generated PROMPT through the canonical request path.
DISPLAY-TEXT is shown in the view instead of PROMPT.  PROMPT-SUBMISSION owns
accepted hook context until the turn is inserted.  PREPARED-OUTCOME carries
skill-expanded model input and transcript render data."
  (when prompt-submission
    (require 'mevedel-prompt-submission))
  (let* ((hook-context
          (and prompt-submission
               (mevedel-prompt-submission-context prompt-submission)))
         (stored-prompt
          (if prepared-outcome
              (concat (plist-get prepared-outcome :transcript-input)
                      (or (plist-get prepared-outcome :render-data) ""))
            (if hook-context
                (concat prompt "\n\n" hook-context)
              prompt)))
         (model-input
          (and prepared-outcome
               (concat (plist-get prepared-outcome :model-input)
                       (or (plist-get prepared-outcome :render-data) "")))))
    (mevedel--insert-local-user-turn
     stored-prompt display-text nil hook-context)
    (when prompt-submission
      (mevedel-prompt-submission-commit prompt-submission))
    (mevedel--gptel-send-request
     (or model-input (and hook-context stored-prompt)))))

(defun mevedel--gptel-send-request (&optional model-input)
  "Send the current gptel prompt and return its standard send FSM.
MODEL-INPUT replaces the stored prompt for this request only."
  (setq-local mevedel--pending-model-input model-input)
  (unwind-protect
      (gptel-request nil
        :stream gptel-stream
        :transforms gptel-prompt-transform-functions
        :fsm (gptel-make-fsm
              :table gptel-send--transitions
              :handlers gptel-send--handlers))
    (setq-local mevedel--pending-model-input nil)))

(defun mevedel--implement-plan (action-plist)
  "Implement the plan described by ACTION-PLIST.

ACTION-PLIST is a plist with keys:
  :permission-mode - Permission mode for implementation
  :display-text   - Optional compact transcript display text
  :prompt-submission - Accepted prompt transaction
  :prepared-outcome - Prepared skill and transcript components."
  (require 'mevedel-utilities)
  (let* ((permission-mode (plist-get action-plist :permission-mode))
         (display-text (or (plist-get action-plist :display-text)
                           "Implement accepted plan"))
         (prompt-submission (plist-get action-plist :prompt-submission))
         (prepared-outcome (plist-get action-plist :prepared-outcome))
         (prompt (and prompt-submission
                      (mevedel-prompt-submission-input prompt-submission))))
    (unless prompt
      (error "Implementation requires an accepted prompt submission"))
    (condition-case err
        (progn
          (mevedel--implementation-permission-mode-apply permission-mode)
          ;; Close any unclosed fenced code blocks (e.g., ``` reasoning)
          (mevedel--close-unclosed-blocks)
          (when prepared-outcome
            (setq-local mevedel-skills--pending-request-context
                        (plist-get prepared-outcome :request-context)))
          (mevedel--submit-generated-turn
           prompt display-text prompt-submission prepared-outcome))
      (error
       (setq-local mevedel-skills--pending-request-context nil)
       (mevedel--implementation-permission-mode-restore)
       (signal (car err) (cdr err))))))

(provide 'mevedel-chat)

;;; mevedel-chat.el ends here
