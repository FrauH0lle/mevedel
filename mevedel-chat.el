;;; mevedel-chat.el -- Chat buffer management -*- lexical-binding: t -*-

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
(declare-function cl-sort "cl-seq" (cl-seq cl-pred &rest cl-keys))

;; `gptel'
(declare-function gptel-markdown-cycle-block "ext:gptel" nil)
(declare-function gptel-mode "ext:gptel" (&optional arg))
(declare-function gptel-send "ext:gptel" nil)
(defvar gptel-display-buffer-action)
(defvar gptel-mode)
(defvar gptel-pre-tool-call-functions)
(defvar gptel-send--handlers)
(defvar gptel-send--transitions)

;; `gptel-org'
(defvar gptel-org-branching-context)
(defvar gptel-org-ignore-elements)

;; `gptel-request'
(declare-function gptel-abort "ext:gptel-request" (buf))
(declare-function gptel-fsm-info "ext:gptel-request")
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

;; `mevedel-collaboration'
(declare-function mevedel-collaboration--safe-accepted-prompt
                  "mevedel-collaboration" (data-buffer))
(declare-function mevedel-collaboration--safe-post-response
                  "mevedel-collaboration" (start end))
(declare-function mevedel-collaboration--safe-post-stream
                  "mevedel-collaboration" nil)

;; `mevedel-compact-run'
(defvar mevedel-compact-run-cancel)

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
(autoload 'mevedel-execution-target-probe "mevedel-execution-target")
(autoload 'mevedel-execution-target-readiness-message
  "mevedel-execution-target")
(autoload 'mevedel-execution-target-remote-p "mevedel-execution-target")

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

;; `mevedel-permission-mode'
(declare-function mevedel--implementation-permission-mode-apply
                  "mevedel-permission-mode" (mode))
(declare-function mevedel--implementation-permission-mode-restore
                  "mevedel-permission-mode" ())

;; `mevedel-permission-persistence'
(declare-function mevedel-permission-validate-persistent-stores
                  "mevedel-permission-persistence" (workspace))

;; `mevedel-plan-mode'
(declare-function mevedel-plan-approval-abort "mevedel-plan-mode"
		  (&optional session outcome))
(declare-function mevedel-plan-mode--post-response "mevedel-plan-mode"
		  (start end))
(declare-function mevedel-plan-mode-restore-pending-approval
		  "mevedel-plan-mode" (&optional session chat-buffer))

;; `mevedel-plugin-ui'
(declare-function mevedel-plugins-notify-pending-consent
                  "mevedel-plugin-ui" (&optional workspace))

;; `mevedel-presets'
(declare-function mevedel-preset--build-handlers "mevedel-presets"
                  (handlers))
(declare-function mevedel-preset--build-transitions "mevedel-presets"
                  (transitions))
(declare-function mevedel-preset-apply "mevedel-presets"
		  (name &optional buffer))
(declare-function mevedel-preset-restore-session "mevedel-presets"
		  (session &optional buffer))
(defvar mevedel-action-preset-alist)
(defvar mevedel-default-chat-preset)

;; `mevedel-prompt-submission'
(declare-function mevedel-prompt-submission-commit
		  "mevedel-prompt-submission" (submission))
(declare-function mevedel-prompt-submission-context
		  "mevedel-prompt-submission" (cl-x) t)
(declare-function mevedel-prompt-submission-input
		  "mevedel-prompt-submission" (cl-x) t)
(autoload 'mevedel-prompt-submission-commit "mevedel-prompt-submission")
(autoload 'mevedel-prompt-submission-context "mevedel-prompt-submission")
(autoload 'mevedel-prompt-submission-input "mevedel-prompt-submission")

;; `mevedel-reminders'
(declare-function mevedel-reminders-install-defaults
		  "mevedel-reminders" (session))

;; `mevedel-session-artifacts'
(declare-function mevedel-session-artifacts-inhibit-so-long
                  "mevedel-session-artifacts" ())
(declare-function
 mevedel-session-artifacts-install-gptel-save-state-advice
 "mevedel-session-artifacts" nil)
(declare-function mevedel-session-artifacts-save
                  "mevedel-session-artifacts"
                  (session buffer &optional settled force))
(declare-function mevedel-session-artifacts-strip-gptel-config-properties
                  "mevedel-session-artifacts" nil)
(autoload 'mevedel-session-artifacts-inhibit-so-long
  "mevedel-session-artifacts")

;; `mevedel-session-persistence'
(declare-function mevedel-session-persistence-release-on-kill
                  "mevedel-session-persistence" nil)

;; `mevedel-skills-core'
(declare-function mevedel-skills--release-on-kill
		  "mevedel-skills-core" nil)
(declare-function mevedel-skills-install "mevedel-skills-core"
		  (session &optional buffer))
(defvar mevedel-skills--pending-request-context)

;; `mevedel-skills-prompt'
(declare-function mevedel-skills-install-activation-hook
		  "mevedel-skills-prompt" nil)
(declare-function mevedel-skills-install-reminder
		  "mevedel-skills-prompt" (session))
(autoload 'mevedel-skills-install-activation-hook "mevedel-skills-prompt")
(autoload 'mevedel-skills-install-reminder "mevedel-skills-prompt")

;; `mevedel-skills-ui'
(declare-function mevedel-slash-capf "mevedel-skills-ui" nil)

;; `mevedel-structs'
(declare-function mevedel-goal-reason "mevedel-structs" (cl-x) t)
(declare-function mevedel-goal-status "mevedel-structs" (cl-x) t)
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
(declare-function mevedel-session-plan-metadata "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-working-directory "mevedel-structs"
		  (cl-x) t)
(declare-function mevedel-session-workspace "mevedel-structs" (cl-x) t)
(declare-function mevedel-workspace-id "mevedel-structs" (cl-x) t)
(declare-function mevedel-workspace-name "mevedel-structs" (cl-x) t)
(declare-function mevedel-workspace-root "mevedel-structs" (cl-x) t)
(declare-function mevedel-workspace-type "mevedel-structs" (cl-x) t)
(defvar mevedel--current-request)
(defvar mevedel--session)

;; `mevedel-tool-render-data'
(declare-function mevedel-tool-render-data-format
                  "mevedel-tool-render-data" (render-data &optional tool-use-id))
(autoload 'mevedel-tool-render-data-format "mevedel-tool-render-data")

;; `mevedel-tool-repair'
(declare-function mevedel-tool-repair-clear-ledger
                  "mevedel-tool-repair" (&rest _))
(declare-function mevedel-tool-repair-post-tool-call
                  "mevedel-tool-repair" (info))
(declare-function mevedel-tool-repair-pre-tool-call
                  "mevedel-tool-repair" (info))

;; `mevedel-transcript-restore'
(declare-function mevedel-transcript-enable-gptel-mode
                  "mevedel-transcript-restore" ())

;; `mevedel-turn'
(declare-function mevedel-request-drain-cancellers "mevedel-turn"
                  (request))
(declare-function mevedel-request-end
                  "mevedel-turn" (&optional abort-plan-approval))
(autoload 'mevedel-request-drain-cancellers "mevedel-turn")
(autoload 'mevedel-request-end "mevedel-turn")

;; `mevedel-utilities'
(declare-function mevedel--clear-user-turn-gptel-properties
		  "mevedel-utilities" (start end))
(declare-function mevedel--optimize-transcript-buffer
		  "mevedel-utilities" nil)
(declare-function mevedel--transcript-org-mode "mevedel-utilities" nil)
(declare-function mevedel-generate-diff "mevedel-utilities"
                  (original modified filepath &optional labels-real))
(autoload 'mevedel--clear-user-turn-gptel-properties "mevedel-utilities")
(autoload 'mevedel--optimize-transcript-buffer "mevedel-utilities")
(autoload 'mevedel--transcript-org-mode "mevedel-utilities")
(autoload 'mevedel-generate-diff "mevedel-utilities")

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
    (mevedel--transcript-org-mode))
  (mevedel--chat-buffer-disable-org-element-cache)
  (setq-local gptel-org-convert-response nil)
  (setq-local gptel-org-branching-context nil)
  (require 'gptel)
  (require 'mevedel-transcript-restore)
  (mevedel-transcript-enable-gptel-mode))

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
      (let (setup-complete-p)
        (unwind-protect
            (progn
              (mevedel--chat-buffer-setup
               buf workspace session-name working-directory)
              (setq setup-complete-p t))
          (unless setup-complete-p
            (when (buffer-live-p buf)
              (let ((view-buffer
                     (buffer-local-value 'mevedel--view-buffer buf)))
                (dolist (buffer (list buf view-buffer))
                  (when (buffer-live-p buffer)
                    (let* ((hooks (buffer-local-value
                                   'kill-buffer-hook buffer))
                           (safe-hooks
                            (lambda ()
                              (let ((kill-buffer-hook hooks))
                                (run-hook-wrapped
                                 'kill-buffer-hook
                                 (lambda (hook)
                                   (let ((kill-buffer-hook nil))
                                     (ignore-errors (funcall hook)))
                                   nil))))))
                      (with-current-buffer buffer
                        (let ((kill-buffer-query-functions nil)
                              (kill-buffer-hook (list safe-hooks)))
                          (ignore-errors (kill-buffer buffer))))))
                  (when (buffer-live-p buffer)
                    (with-current-buffer buffer
                      (let ((kill-buffer-hook nil)
                            (kill-buffer-query-functions nil))
                        (ignore-errors (kill-buffer buffer))))))))))))
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

(defvar-local mevedel--session-start-hooks-pending nil
  "Non-nil while asynchronous SessionStart hooks are still running.")

(defun mevedel--probe-session-target (session &optional refresh)
  "Probe SESSION's remote execution target.

When REFRESH is non-nil, discard the live readiness cache first.  Local
sessions keep their existing startup behavior and return nil."
  (let ((target (mevedel-session-execution-target session)))
    (when (and target
               (mevedel-execution-target-remote-p target))
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
                 (mevedel-execution-target-remote-p target))
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
  (require 'mevedel-session-persistence)
  (require 'mevedel-session-codec)
  (require 'mevedel-session-artifacts)
  (with-current-buffer buf
    (when (derived-mode-p 'org-mode)
      (mevedel--chat-buffer-disable-org-element-cache))
    (setq-local gptel-org-convert-response nil)
    (setq-local gptel-org-branching-context nil)
    ;; A restored segment may still carry gptel's request-config Org
    ;; properties; gptel's send advice would prefer them over the live
    ;; buffer-locals set below.  The sidecar is the config source, so
    ;; delete them on sight.  Fresh buffers have no drawer: no-op.
    (mevedel-session-artifacts-strip-gptel-config-properties)
    (mevedel-preset-restore-session mevedel--session buf)
    (require 'mevedel-models)
    (mevedel-model-apply-session-policy mevedel--session buf)
    (mevedel-reminders-install-defaults mevedel--session)
    (when (equal source "resume")
      (mevedel--queue-reconciliation-reminder mevedel--session)
      (when (and (not inspection-p)
                 (plist-get (mevedel-session-plan-metadata mevedel--session)
                            :implementation-retry))
        (message "mevedel: accepted plan implementation is pending; \
M-x mevedel-retry-plan-implementation resumes it")))
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
    (require 'mevedel-permission-persistence)
    (mevedel-permission-validate-persistent-stores workspace)
    ;; Make workspace-additional-roots buffer-local for session-specific
    ;; access grants.  Restore path may have already set this from the
    ;; sidecar's `:additional-roots'; don't clobber.
    (unless (local-variable-p 'mevedel-workspace-additional-roots)
      (setq-local mevedel-workspace-additional-roots
                  (copy-alist mevedel-workspace-additional-roots)))
    ;; gptel owns its `before-save-hook'; mevedel advises the save
    ;; function so request configuration is kept out of the transcript.
    (mevedel-session-artifacts-install-gptel-save-state-advice)
    ;; Release the session lock when the chat buffer is killed.
    (add-hook 'kill-buffer-hook
              #'mevedel-session-persistence-release-on-kill nil t)
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
    (require 'mevedel-plugin-ui)
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
session name (e.g., \"main\", \"refactor\").  For auxiliary buffers, use a
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
        (setq-local mevedel--workspace workspace)
        (mevedel-session-artifacts-inhibit-so-long)))
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

(defun mevedel--generate-final-patch (&optional workspace request)
  "Generate final diffs for all tracked files in REQUEST.

Return a unified diff string showing original -> final state for each
file.  Uses REQUEST's snapshots -- defaulting to `mevedel--current-request\'
-- to compare original states with current file contents in WORKSPACE.

REQUEST is resolved once, at entry.  Reading the buffer-local on each
iteration instead was a use-after-settle: the loop reads every touched
file, and on a remote workspace `insert-file-contents\' hands control to
TRAMP\'s wait loop, which runs timers and process sentinels.  One of those
settles the turn and clears `mevedel--current-request\', so an iteration
that began with a live request could reach the next one holding nil and
signal `wrong-type-argument\'.  A caller that already knows the request --
because it captured it while the turn was live -- should pass it."
  (let* ((diffs "")
         (workspace-root (mevedel-workspace-root
                          (or workspace (mevedel-workspace))))
         (request (or request mevedel--current-request))
         (snapshots (and request (mevedel-request-file-snapshots request)))
         paths)
    (when snapshots
      (maphash (lambda (filepath _original) (push filepath paths)) snapshots))
    (dolist (filepath (sort paths #'string<))
      (let* ((original (gethash filepath snapshots))
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
                              (mevedel-generate-diff
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
;;; Request cancellation

(defun mevedel-abort (&optional buf)
  "Abort any active request associated with buffer BUF.

Thus, abort `gptel' requests running in the mevedel chat buffer
associated with the `mevedel-workspace' for BUF.

If a callback was provided to the original request, it will be called
with the \\='abort symbol as the error parameter.

BUF defaults to the current buffer if not specified."
  (interactive)
  (require 'mevedel-session-artifacts)
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
        (when (and (boundp 'mevedel-compact-run-cancel)
                   (functionp mevedel-compact-run-cancel))
          (funcall mevedel-compact-run-cancel)))
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
        (if (bound-and-true-p mevedel--current-request)
            (mevedel-request-end)
          ;; A request can disappear without its own teardown -- a terminal
          ;; transition lost with the process that would have driven it.
          ;; `mevedel-request-end' is the only place that idles the root
          ;; roster, and it needs a request, so the roster stays marked
          ;; running with nothing running it and the view spins forever.
          ;; An abort is the user asserting the opposite.
          (when (bound-and-true-p mevedel--session)
            (setf (mevedel-session-agent-root-activity mevedel--session)
                  'idle)))
        (when (and (bound-and-true-p mevedel--session)
                   (mevedel-session-workspace mevedel--session)
                   (not (bound-and-true-p
                         mevedel-session--read-only-mode)))
          (condition-case err
              (mevedel-session-artifacts-save mevedel--session chat-buffer)
            (error
             (display-warning
              'mevedel
              (format "Could not save session after abort: %S" err)
              :warning))))))))

;;
;;; Goal implementation

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
      (insert (mevedel-tool-render-data-format
               (list :kind 'user-display :text display-text)))))
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
