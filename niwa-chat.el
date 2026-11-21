;;; niwa-chat.el -- Chat buffer management -*- lexical-binding: t -*-

;;; Commentary:

;; Chat session lifecycle: creates the gptel data buffer, wires up the
;; workspace, tool list, presets, and agents on it, and attaches the
;; corresponding `niwa-view' buffer for user-facing display.
;; Supports multiple concurrent sessions per workspace (switch via
;; `niwa-switch-session').  Each live session epoch ends once with
;; `SessionEnd': when its data buffer is killed or when Emacs exits.
;;
;; Also submits accepted Plan prompt transactions through the ordinary
;; request path after `niwa-plan-handoff' has prepared their context.

;;; Code:

(require 'niwa-utilities)

;; `setf' on a slot of a struct defined elsewhere needs that
;; `cl-defstruct' at compile time: `declare-function' supplies the
;; accessor but not the setter, and without the expander the form
;; compiles to a call to a function that does not exist.
(eval-when-compile (require 'niwa-structs))

(eval-when-compile
  (require 'cl-lib)
  (require 'gptel)
  (require 'gptel-request))

(require 'niwa-hooks)
(require 'niwa-implementation-policy)
(require 'niwa-journal-idle)
(require 'niwa-reminders)
(require 'niwa-claude-code-backend)
(require 'niwa-engine)
(require 'niwa-presets)
(require 'niwa-prompt-submission)
(require 'niwa-session-artifacts)
(require 'niwa-session-persistence)

;; `cl-extra'
(declare-function cl-some "cl-extra" (cl-pred cl-seq &rest cl-rest))

;; `cl-lib'
(declare-function cl-oddp "cl-lib" (integer))

;; `cl-seq'
(declare-function cl-delete "cl-seq" (cl-item cl-seq &rest cl-keys))
(declare-function cl-find-if "cl-seq" (cl-pred cl-list &rest cl-keys))

;; `gptel'
(declare-function gptel-markdown-cycle-block "ext:gptel" nil)
(declare-function gptel-mode "ext:gptel" (&optional arg))
(declare-function gptel-send "ext:gptel" nil)
(defvar gptel-display-buffer-action)
(defvar gptel-mode)
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
(declare-function gptel-make-fsm "ext:gptel-request" (&rest args))
(declare-function gptel-request "ext:gptel-request")
(defvar gptel--request-alist)
(defvar gptel-confirm-tool-calls)
(defvar gptel-org-convert-response)
(defvar gptel-prompt-transform-functions)
(defvar gptel-stream)

;; `niwa'
(defvar niwa--load-provenance)
(defvar niwa-default-chat-preset)

;; `niwa-agents'
(declare-function niwa-agent-invocation-parent-data-buffer
		  "niwa-agents" (cl-x) t)

;; `niwa-auth'
(declare-function niwa-auth-assert-ready "niwa-auth" (backend))
(autoload 'niwa-auth-assert-ready "niwa-auth")

;; `niwa-claude-code-btw'
(declare-function niwa-claude-code-btw-send "niwa-claude-code-btw" (input policy))
(autoload 'niwa-claude-code-btw-send "niwa-claude-code-btw")

;; `niwa-collaboration'
(declare-function niwa-collaboration--safe-accepted-prompt
                  "niwa-collaboration" (data-buffer))
(declare-function niwa-collaboration--safe-post-response
                  "niwa-collaboration" (&rest positions))

(autoload 'niwa-collaboration--safe-accepted-prompt "niwa-collaboration")
(autoload 'niwa-collaboration--safe-post-response "niwa-collaboration")

;; `niwa-compact-run'
(defvar niwa-compact-run-cancel)



;; `niwa-execution'
(declare-function niwa-execution-acknowledge-unknown
                  "niwa-execution" (session))
(declare-function niwa-execution-mutation-blocked-p
                  "niwa-execution" (session))

;; `niwa-execution-target'
(declare-function niwa-execution-target-probe
                  "niwa-execution-target"
                  (target &optional refresh sandbox-mode))
(declare-function niwa-execution-target-readiness-message
                  "niwa-execution-target" (target))
(declare-function niwa-execution-target-remote-p
                  "niwa-execution-target" (target))
(autoload 'niwa-execution-target-probe "niwa-execution-target")
(autoload 'niwa-execution-target-readiness-message
  "niwa-execution-target")
(autoload 'niwa-execution-target-remote-p "niwa-execution-target")

;; `niwa-hooks'
(declare-function niwa-hooks-event-plist "niwa-hooks"
		  (event &optional session workspace &rest extra))
(declare-function niwa-hooks-record-session-context "niwa-hooks"
		  (session decision &optional event))
(declare-function niwa-hooks-run-event "niwa-hooks"
		  (event event-plist callback &optional session
			 workspace request invocation))

;; `niwa-models'
(declare-function niwa-model-apply-session-policy
                  "niwa-models" (session &optional buffer))

;; `niwa-pending-inputs'
(defvar niwa-view--pending-guest-attribution)

;; `niwa-permission-persistence'
(declare-function niwa-permission-validate-persistent-stores
                  "niwa-permission-persistence" (workspace))

;; `niwa-plan-mode'
(declare-function niwa-plan-approval-abort "niwa-plan-mode"
		  (&optional session outcome))
(declare-function niwa-plan-mode--post-response "niwa-plan-mode"
		  (start end))
(autoload 'niwa-plan-approval-abort "niwa-plan-mode")
(declare-function niwa-plan-mode-restore-pending-approval
		  "niwa-plan-mode" (&optional session chat-buffer))

(autoload 'niwa-plan-mode--post-response "niwa-plan-mode")
(autoload 'niwa-plan-mode-restore-pending-approval "niwa-plan-mode")

;; `niwa-plugin-ui'
(declare-function niwa-plugins-notify-pending-consent
                  "niwa-plugin-ui" (&optional workspace))

;; `niwa-reminders'
(defvar niwa-reminders-reconciliation-body)
(declare-function niwa-reminders-install-defaults
		  "niwa-reminders" (session))


;; `niwa-sandbox'
(declare-function niwa-sandbox-mode-effective
                  "niwa-sandbox" (&optional session permission-mode))
(autoload 'niwa-sandbox-mode-effective "niwa-sandbox")

;; `niwa-session-naming'
(declare-function niwa-session-naming-consider "niwa-session-naming" (session prompt))
(autoload 'niwa-session-naming-consider "niwa-session-naming")

;; `niwa-skills-core'
(declare-function niwa-skills--release-on-kill
		  "niwa-skills-core" nil)
(declare-function niwa-skills-install "niwa-skills-core"
		  (session &optional buffer))
(autoload 'niwa-skills--release-on-kill "niwa-skills-core")
(autoload 'niwa-skills-install "niwa-skills-core")
(defvar niwa-skills--pending-request-context)

;; `niwa-skills-invoke'
(declare-function niwa-skills-request-model-policy "niwa-skills-invoke" ())
(autoload 'niwa-skills-request-model-policy "niwa-skills-invoke")

;; `niwa-skills-prompt'
(declare-function niwa-skills-install-activation-hook
		  "niwa-skills-prompt" nil)
(autoload 'niwa-skills-install-activation-hook "niwa-skills-prompt")

;; `niwa-skills-ui'
(declare-function niwa-slash-capf "niwa-skills-ui" nil)

;; `niwa-structs'
(declare-function niwa-goal-reason "niwa-structs" (cl-x) t)
(declare-function niwa-goal-reason-diagnostic "niwa-structs" (cl-x) t)
(declare-function niwa-goal-status "niwa-structs" (cl-x) t)
(declare-function niwa-request-file-snapshots "niwa-structs"
		  (cl-x) t)
(declare-function niwa-request-untracked-effects "niwa-structs"
                  (cl-x) t)
(declare-function niwa-session-audit-session "niwa-structs" (cl-x) t)
(declare-function niwa-session-buffer-name "niwa-structs"
                  (session-name workspace))
(declare-function niwa-session-dedicated-artifact "niwa-structs" (cl-x) t)
(declare-function niwa-session-create "niwa-structs"
		  (name workspace &optional working-directory session-id))
(declare-function niwa-session-enqueue-pending-reminder "niwa-structs"
                  (session body))
(declare-function niwa-session-execution-target
                  "niwa-structs" (cl-x) t)
(declare-function niwa-session-name "niwa-structs" (cl-x) t)
(declare-function niwa-session-pending-reminders "niwa-structs"
                  (cl-x) t)
(declare-function niwa-session-plan-metadata "niwa-structs" (cl-x) t)
(declare-function niwa-session-working-directory "niwa-structs"
		  (cl-x) t)
(declare-function niwa-session-workspace "niwa-structs" (cl-x) t)
(declare-function niwa-workspace-id "niwa-structs" (cl-x) t)
(declare-function niwa-workspace-name "niwa-structs" (cl-x) t)
(declare-function niwa-workspace-root "niwa-structs" (cl-x) t)
(declare-function niwa-workspace-type "niwa-structs" (cl-x) t)
(defvar niwa--current-request)
(defvar niwa--session)

;; `niwa-telemetry'
(declare-function niwa-telemetry-record "niwa-telemetry"
                  (session event &rest props))
(autoload 'niwa-telemetry-record "niwa-telemetry")

;; `niwa-tool-render-data'
(declare-function niwa-tool-render-data-format
                  "niwa-tool-render-data" (render-data &optional tool-use-id))
(autoload 'niwa-tool-render-data-format "niwa-tool-render-data")

;; `niwa-tool-repair'
(declare-function niwa-tool-repair-clear-ledger
                  "niwa-tool-repair" (&rest _))
(declare-function niwa-tool-repair-post-tool-call
                  "niwa-tool-repair" (info))
(declare-function niwa-tool-repair-pre-tool-call
                  "niwa-tool-repair" (info))

;; `niwa-transcript-restore'
(declare-function niwa-transcript-enable-gptel-mode
                  "niwa-transcript-restore" ())

;; `niwa-transport'
(declare-function niwa-transport-run-when-idle "niwa-transport"
                  (key path thunk &optional on-cancel delay))
(autoload 'niwa-transport-run-when-idle "niwa-transport")

;; `niwa-turn'
(declare-function niwa-request-cancel "niwa-turn"
                  (request &optional abort-plan-approval))
(declare-function niwa-request-end
                  "niwa-turn" (&optional abort-plan-approval))
(autoload 'niwa-request-cancel "niwa-turn")
(autoload 'niwa-request-end "niwa-turn")
(defvar niwa--turn-settlements-pending)

;; `niwa-utilities'
(declare-function niwa--insert-user-turn
                  "niwa-utilities" (input))
(declare-function niwa--optimize-transcript-buffer
		  "niwa-utilities" nil)
(declare-function niwa--transcript-org-mode "niwa-utilities" nil)
(declare-function niwa-generate-diff "niwa-utilities"
                  (original modified filepath callback &optional labels-real settlement))
(autoload 'niwa--insert-user-turn "niwa-utilities")
(autoload 'niwa--optimize-transcript-buffer "niwa-utilities")
(autoload 'niwa--transcript-org-mode "niwa-utilities")
(autoload 'niwa-generate-diff "niwa-utilities")

;; `niwa-view'
(declare-function niwa-view--ensure "niwa-view" (data-buf))
(declare-function niwa-view-composer-display "niwa-view-pair" (view-window))
(defvar niwa--agent-invocation)
(defvar niwa--data-buffer)
(defvar niwa--view-buffer)

;; `niwa-view-composer'
(declare-function niwa-view--begin-external-turn
		  "niwa-view-composer"
		  (display-text data-turn-start &optional kind
				hook-context no-spinner user-source attribution))

;; `niwa-view-stream'
(declare-function niwa-view-stream-post-tool "niwa-view-stream"
		  (args))
(declare-function niwa-view-stream-pre-tool "niwa-view-stream"
		  (args))
(declare-function niwa-view-stream-render-response
		  "niwa-view-stream" (start end))
(declare-function niwa-view-stream-schedule "niwa-view-stream"
		  nil)
(declare-function niwa-view-stream-spinner-hook
		  "niwa-view-stream" (info))
(declare-function niwa-view-stream-stop "niwa-view-stream" nil)

;; `niwa-workspace'
(declare-function niwa-workspace "niwa-workspace"
		  (&optional buffer))
(defvar niwa--workspace)
(defvar niwa-workspace-additional-roots)

;; `org'
(defvar org-agenda-file-menu-enabled)

;; `org-element'
(declare-function org-element-cache-reset "ext:org-element"
		  (&optional all no-persistence))
(defvar org-element-cache-persistent)
(defvar org-element-use-cache)

;;
;;; Customization

(defcustom niwa-show-patch-buffer nil
  "Control if the niwa patch buffer should be shown automatically.

If non-nil, the patch buffer will automatically be displayed after a
query completes."
  :type 'boolean
  :group 'niwa)

(defvar niwa--diff-preview-buffer-name "*niwa-diff-preview*"
  "Name of the `diff' preview buffer.")

;;
;;; Buffer management

(defun niwa--chat-buffer-disable-org-element-cache ()
  "Disable Org's element cache in the current niwa transcript buffer.

Niwa keeps chat data buffers in `org-mode' so gptel can persist state
in org properties, but the buffer is not a normal hand-edited Org
document: gptel and niwa insert hidden regions, property runs, and
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
transcript buffer; the user-facing niwa view remains responsible for
interactive display."
  (when (fboundp 'org-element-cache-reset)
    (let ((org-element-use-cache t))
      (ignore-errors
        (org-element-cache-reset nil 'no-persistence))))
  (setq-local org-element-use-cache nil)
  (setq-local org-element-cache-persistent nil)
  (setq-local gptel-org-ignore-elements '(property-drawer))
  (niwa--optimize-transcript-buffer))

(defun niwa-chat-prepare-transcript-buffer ()
  "Prepare the current buffer as a gptel org transcript data buffer.

Sets the transcript major mode, disables Org's element cache, keeps
model responses as raw Markdown (`gptel-org-convert-response' nil), and
enables `gptel-mode'.  The major-mode change calls
`kill-all-local-variables', so buffer-locals set before this call are
wiped unless permanent-local."
  (let ((org-agenda-file-menu-enabled nil)
        (org-element-use-cache nil))
    (niwa--transcript-org-mode))
  (niwa--chat-buffer-disable-org-element-cache)
  (setq-local gptel-org-convert-response nil)
  (setq-local gptel-org-branching-context nil)
  (require 'gptel)
  (require 'niwa-transcript-restore)
  (niwa-transcript-enable-gptel-mode)
  (add-hook 'before-change-functions #'niwa-engine-record-history-edit nil t))

(defun niwa-chat-install-request-hooks ()
  "Install buffer-local tool-repair and view-stream request hooks.

Repairs raw model input before view hooks observe the call and before
gptel maps the arguments into the pipeline wrapper.  Renders incremental
view updates on tool boundaries so the user sees progress per tool call,
and debounces mid-turn text updates a few times per second while the LLM
is producing text; tool-boundary hooks cancel the pending timer and
render immediately, so this never delays tool-call feedback."
  (require 'niwa-view-stream)
  (require 'niwa-tool-repair)
  (add-hook 'before-change-functions #'niwa-engine-record-history-edit nil t)
  (add-hook 'gptel-post-response-functions
            #'niwa-view-stream-render-response nil t)
  (add-hook 'gptel-pre-tool-call-functions
            #'niwa-tool-repair-pre-tool-call -100 t)
  (add-hook 'gptel-post-tool-call-functions
            #'niwa-tool-repair-post-tool-call -100 t)
  (add-hook 'gptel-post-response-functions
            #'niwa-tool-repair-clear-ledger nil t)
  (add-hook 'kill-buffer-hook #'niwa-tool-repair-clear-ledger nil t)
  (add-hook 'gptel-pre-tool-call-functions
            #'niwa-view-stream-spinner-hook nil t)
  (add-hook 'gptel-pre-tool-call-functions
            #'niwa-view-stream-pre-tool nil t)
  (add-hook 'gptel-post-tool-call-functions
            #'niwa-view-stream-post-tool nil t)
  (add-hook 'gptel-post-stream-hook #'niwa-view-stream-schedule nil t)
  (add-hook 'gptel-post-stream-hook
            #'niwa-collaboration--safe-post-response nil t)
  (add-hook 'gptel-post-response-functions
            #'niwa-collaboration--safe-post-response nil t))

(defun niwa-chat-discard-buffers (data-buffer &optional discard-modifications)
  "Discard failed DATA-BUFFER and its companion view without querying.
Run all cleanup hooks despite individual errors, then force closure if needed.
With DISCARD-MODIFICATIONS, clear each buffer's modified flag before its hooks;
otherwise leave the flag available to lifecycle cleanup.

While a buffer's hooks run, a nested kill of that same buffer is refused.
The data buffer's view hook kills its companion, whose own hook kills the
data buffer again; that inner kill would see the hook list this function
suspended and finish the buffer early, so the remaining hooks, among
them the session lock release, would run without the buffer or its
session."
  (when (buffer-live-p data-buffer)
    (let ((view-buffer (buffer-local-value 'niwa--view-buffer data-buffer)))
      (dolist (buffer (list data-buffer view-buffer))
        (when (buffer-live-p buffer)
          (let* ((hooks (buffer-local-value 'kill-buffer-hook buffer))
                 (refuse-nested (lambda () (not (eq (current-buffer) buffer))))
                 (safe-hooks
                  (lambda ()
                    ;; The outer kill has passed its queries; only nested
                    ;; kills of this buffer consult this binding.
                    (let ((kill-buffer-hook hooks)
                          (kill-buffer-query-functions (list refuse-nested)))
                      (run-hook-wrapped
                       'kill-buffer-hook
                       (lambda (hook)
                         (when (buffer-live-p buffer)
                           (with-current-buffer buffer
                             (let ((kill-buffer-hook nil))
                               (ignore-errors (funcall hook)))))
                         nil))))))
            (with-current-buffer buffer
              (when discard-modifications (set-buffer-modified-p nil))
              (let ((kill-buffer-query-functions nil)
                    (kill-buffer-hook (list safe-hooks)))
                (ignore-errors (kill-buffer buffer)))))
          (when (buffer-live-p buffer)
            (with-current-buffer buffer
              (let ((kill-buffer-hook nil)
                    (kill-buffer-query-functions nil))
                (ignore-errors (kill-buffer buffer))))))))))

(defun niwa--chat-buffer (session-name &optional create workspace working-directory)
  "Get or create the niwa chat buffer SESSION-NAME for WORKSPACE.

This buffer is where LLM interactions occur.  If CREATE is non-nil,
create the buffer if it doesn't exist.  WORKSPACE should be a
`niwa-workspace' struct, or nil to use the current buffer's
workspace.

WORKING-DIRECTORY is used only when creating a fresh session.  If an
existing live session with SESSION-NAME has a different working
directory, signal `user-error' instead of silently switching context."
  (let* ((workspace (or workspace (niwa-workspace)))
         (matches (and session-name
                       (cl-remove-if-not
                        (lambda (entry) (equal (car entry) session-name))
                        (niwa--workspace-sessions workspace))))
         (_ (when (cdr matches)
              (user-error "Several sessions have this name; select one by ID")))
         (buf (if matches
                  (cons (cdar matches) nil)
                (when create
                  (cons (generate-new-buffer " *niwa-new-session*") t))))
         (created-p (cdr buf))
         (buf (car buf))
         (working-directory (and working-directory
                                 (file-name-as-directory
                                  (expand-file-name working-directory)))))
    (when created-p
      (let (setup-complete-p)
        (unwind-protect
            (progn
              (niwa--chat-buffer-setup
               buf workspace session-name working-directory)
              (setq setup-complete-p t))
          (unless setup-complete-p
            (niwa-chat-discard-buffers buf)))))
    (when (and buf working-directory (not created-p))
      (with-current-buffer buf
        (when (and (bound-and-true-p niwa--session)
                   (not (equal working-directory
                               (niwa-session-working-directory
                                niwa--session))))
          (user-error "Session %s already uses working directory %s"
                      session-name
                      (niwa-session-working-directory
                       niwa--session)))))
    buf))

(defvar-local niwa--session-start-hooks-pending nil
  "Non-nil while asynchronous SessionStart hooks are still running.")

(defun niwa--probe-session-target (session &optional refresh)
  "Probe SESSION's remote execution target.

When REFRESH is non-nil, discard the live readiness cache first.  Local
sessions keep their existing startup behavior and return nil."
  (let ((target (niwa-session-execution-target session)))
    (when (and target
               (niwa-execution-target-remote-p target))
      (niwa-execution-target-probe
       target refresh (niwa-sandbox-mode-effective session)))))

;;;###autoload
(defun niwa-retry-target-readiness ()
  "Force a fresh readiness probe for the current remote session."
  (interactive)
  (let* ((data-buffer
          (cond
           ((bound-and-true-p niwa--session) (current-buffer))
           ((and (boundp 'niwa--data-buffer)
                 (buffer-live-p niwa--data-buffer))
            niwa--data-buffer)
           (t (niwa--active-chat-buffer))))
         (session (and (buffer-live-p data-buffer)
                       (buffer-local-value 'niwa--session data-buffer)))
         (target (and session (niwa-session-execution-target session))))
    (unless session
      (niwa-diagnostic-user-error (niwa-diagnostic-scope (current-buffer)) "No niwa session here"))
    (unless (and target
                 (niwa-execution-target-remote-p target))
      (niwa-diagnostic-user-error (niwa-diagnostic-scope (current-buffer)) "Current niwa session is local"))
    (let ((readiness (niwa--probe-session-target session t)))
      (when (eq 'ready (plist-get readiness :status))
        (require 'niwa-execution)
        (when (niwa-execution-mutation-blocked-p session)
          (unless
              (yes-or-no-p
               (concat
                "The target is reachable, but a previous remote process "
                "may still be running. Acknowledge that outcome and allow "
                "new mutating execution? "))
            (niwa-diagnostic-user-error (niwa-diagnostic-scope (current-buffer)) "Unknown remote execution remains unacknowledged"))
          (niwa-execution-acknowledge-unknown session)))
      (when-let* ((view-buffer
                   (buffer-local-value 'niwa--view-buffer data-buffer))
                  ((buffer-live-p view-buffer)))
        (with-current-buffer view-buffer
          (force-mode-line-update t)))
      (if (eq 'ready (plist-get readiness :status))
          (message "niwa: execution target %s"
                   (niwa-execution-target-readiness-message target))
        (niwa-diagnostic-message (niwa-diagnostic-scope session)
         "Execution target %s" (niwa-execution-target-readiness-message target)))
      readiness)))

(defun niwa--run-session-start-hooks (source)
  "Run session-start hooks for the current buffer with SOURCE."
  (when (bound-and-true-p niwa--session)
    (setf (niwa-session-workspace-instruction-hashes niwa--session)
          (cl-delete-if
           (lambda (entry)
             (let ((owner (caar entry)))
               (and (or (equal source "resume") (equal owner "/root"))
                    ;; Native history survives this local context epoch.
                    ;; Keep its learned scopes for refresh before continuation.
                    (not (assoc (if (equal owner "/root") "root" owner)
                                (niwa-session-external-conversations niwa--session))))))
           (niwa-session-workspace-instruction-hashes niwa--session))))
  (run-hooks 'niwa-session-start-hook)
  (when (bound-and-true-p niwa--session)
    (apply #'niwa-telemetry-record
           niwa--session 'session-runtime
           :boundary source :emacs-version emacs-version
           (bound-and-true-p niwa--load-provenance))
    (let ((buffer (current-buffer))
          (workspace (or (and (boundp 'niwa--workspace)
                              niwa--workspace)
                         (niwa-session-workspace niwa--session)))
          done)
      (setq-local niwa--session-start-hooks-pending t)
      (niwa-hooks-run-event
       'SessionStart
       (niwa-hooks-event-plist
        'SessionStart niwa--session workspace
        :source source)
       (lambda (decision)
         (when (buffer-live-p buffer)
           (with-current-buffer buffer
             (niwa-hooks-record-session-context
              niwa--session decision 'SessionStart)
             (setq-local niwa--session-start-hooks-pending nil)))
         (setq done t))
       niwa--session workspace nil nil)
      (while (not done)
        (accept-process-output nil 0.05)))))

(defun niwa--run-session-end-hooks (&optional reason callback)
  "Run native and declarative session-end hooks for the current buffer.

REASON is the `SessionEnd' matcher value: \"kill-buffer\" by default,
which is how `kill-buffer-hook' calls this, or \"exit\" when Emacs
exits.  The live session epoch ends once: this function removes itself
from the buffer's `kill-buffer-hook' before running anything.  CALLBACK,
when non-nil, receives the merged decision once every handler has
settled."
  (remove-hook 'kill-buffer-hook #'niwa--run-session-end-hooks t)
  (let ((niwa-session-end-reason (or reason "kill-buffer")))
    (run-hooks 'niwa-session-end-hook)
    (if (bound-and-true-p niwa--session)
        (let ((workspace (or (and (boundp 'niwa--workspace)
                                  niwa--workspace)
                             (niwa-session-workspace niwa--session))))
          (niwa-hooks-run-event
           'SessionEnd
           (niwa-hooks-event-plist
            'SessionEnd niwa--session workspace
            :reason niwa-session-end-reason)
           (or callback #'ignore) niwa--session workspace nil nil))
      (when callback (funcall callback nil)))))

(defun niwa--end-sessions-on-exit ()
  "End every live root session epoch with reason \"exit\".

Emacs exit kills no buffers, so `kill-buffer-hook' never reports these
sessions.  Each buffer whose epoch is still open fires `SessionEnd'
here; all handlers start before any is awaited, so the wait is bounded
by the slowest handler's own timeout rather than their sum.  Quitting
stops waiting.  One failing buffer does not keep the others from
ending."
  (let (pending)
    (dolist (buffer (buffer-list))
      (when (and (buffer-live-p buffer)
                 (memq #'niwa--run-session-end-hooks
                       (buffer-local-value 'kill-buffer-hook buffer)))
        (let ((cell (list nil)))
          (push cell pending)
          (with-current-buffer buffer
            (condition-case err
                (niwa--run-session-end-hooks
                 "exit" (lambda (_decision) (setcar cell t)))
              (error
               (setcar cell t)
               (niwa-diagnostic-message (niwa-diagnostic-scope buffer)
                        "%s (SessionEnd failed in %s)"
                        (error-message-string err)
                        (buffer-name buffer))))))))
    (with-local-quit
      (while (cl-some (lambda (cell) (not (car cell))) pending)
        (accept-process-output nil 0.05)))))

(defun niwa--queue-reconciliation-reminder (session)
  "Queue one recovery-state reminder for SESSION."
  (let ((body niwa-reminders-reconciliation-body))
    (unless (member body (niwa-session-pending-reminders session))
      (niwa-session-enqueue-pending-reminder session body))))

(defun niwa--chat-buffer-init-common (buf workspace source
                                             &optional inspection-p)
  "Set up BUF for WORKSPACE and start its lifecycle with SOURCE.

Caller must already have set BUF's buffer-local `niwa--session'.
Wires the FSM handler chain, header-line, visual settings, all
per-buffer hooks, the skill set, default reminders, and the companion
view buffer.

Both `niwa--chat-buffer-setup' (fresh path) and restore paths
\(`niwa-session-persistence-restore') call this after planting the
session struct. SOURCE is \"startup\", \"resume\", or \"fork\".  When
INSPECTION-P is non-nil, skip lifecycle hooks because the client has no
mutation lease."
  (with-current-buffer buf
    (when (derived-mode-p 'org-mode)
      (niwa--chat-buffer-disable-org-element-cache))
    (setq-local gptel-org-convert-response nil)
    (setq-local gptel-org-branching-context nil)
    ;; niwa's permission pipeline decides tool calls, in a form room
    ;; guests can answer; gptel's own confirmation would wait in Emacs.
    (setq-local gptel-confirm-tool-calls nil)
    ;; A restored segment may still carry gptel's request-config Org
    ;; properties; gptel's send advice would prefer them over the live
    ;; buffer-locals set below.  The sidecar is the config source, so
    ;; delete them on sight.  This derived cleanup is not a local edit:
    ;; dirtying a joined buffer would prevent following and control transfer.
    ;; Preserve existing edits, and do not cover arbitrary setup hooks below.
    (let ((modified (buffer-modified-p)))
      (unwind-protect
          (niwa-session-artifacts-strip-gptel-config-properties)
        (set-buffer-modified-p modified)))
    (niwa-preset-restore-session niwa--session buf)
    (require 'niwa-models)
    (niwa-model-apply-session-policy niwa--session buf)
    (niwa-reminders-install-defaults niwa--session)
    (when (equal source "resume")
      (niwa--queue-reconciliation-reminder niwa--session)
      (when (and (not inspection-p)
                 (plist-get (niwa-session-plan-metadata niwa--session)
                            :implementation-retry))
        (message "niwa: accepted plan implementation is pending; \
M-x niwa-retry-plan-implementation resumes it")))
    (require 'niwa-goal)
    ;; Install the niwa-augmented FSM handler chain as the buffer-local
    ;; `gptel-send--handlers' so every request from this buffer -- whether
    ;; driven by `gptel-send' or
    ;; `niwa--implement-plan' -- picks up the deferred-tool WAIT handler
    ;; and the terminal-state handlers (patch generation, callbacks,
    ;; cleanup, turn-count increment).  Building once at setup time keeps
    ;; the handlers stateless and idempotent across requests.
    (setq-local gptel-send--handlers
                (niwa-preset--build-handlers
                 (copy-tree (default-value 'gptel-send--handlers))))
    (setq-local gptel-send--transitions
                (niwa-preset--build-transitions
                 (copy-tree (default-value 'gptel-send--transitions))))
    ;; Wrap lines
    (visual-line-mode +1)
    ;; Auto-scroll when at end of buffer
    (setq-local window-point-insertion-type t)
    ;; Install the session working directory before cwd-dependent setup
    ;; such as skill discovery and dynamic prompt construction.
    (setq-local default-directory
                (or (niwa-session-working-directory niwa--session)
                    (niwa-workspace-root workspace)))
    (niwa--probe-session-target niwa--session)
    (require 'niwa-permission-persistence)
    (niwa-permission-validate-persistent-stores workspace)
    ;; Make workspace-additional-roots buffer-local for session-specific
    ;; access grants.  Restore path may have already set this from the
    ;; sidecar's `:additional-roots'; don't clobber.
    (unless (local-variable-p 'niwa-workspace-additional-roots)
      (setq-local niwa-workspace-additional-roots
                  (copy-alist niwa-workspace-additional-roots)))
    ;; gptel owns its `before-save-hook'; niwa advises the save
    ;; function so request configuration is kept out of the transcript.
    (niwa-session-artifacts-install-gptel-save-state-advice)
    ;; Release the session lock when the chat buffer is killed.
    (add-hook 'kill-buffer-query-functions
              #'niwa-session-persistence-allow-buffer-close-p nil t)
    (add-hook 'kill-buffer-hook
              #'niwa-session-persistence-release-on-kill nil t)
    (unless inspection-p
      (add-hook 'kill-buffer-hook
                #'niwa--run-session-end-hooks nil t)
      (niwa-session-persistence-schedule-cleanup workspace))
    (niwa-chat-install-request-hooks)
    (add-hook 'gptel-post-response-functions
              #'niwa-plan-mode--post-response t t)
    ;; Install slash-command and $skill completion-at-point.
    (add-hook 'completion-at-point-functions #'niwa-slash-capf nil t)
    ;; Populate session skills from workspace skill dirs
    (niwa-skills-install niwa--session (current-buffer))
    (require 'niwa-plugin-ui)
    (niwa-plugins-notify-pending-consent workspace)
    ;; Drop this buffer from the skill watcher registry on kill so any
    ;; orphaned `file-notify' watchers are torn down.
    (add-hook 'kill-buffer-hook
              #'niwa-skills--release-on-kill nil t)
    ;; Register skill event reminders on the session.
    ;; Activate conditional skills when a tool touches a matching file
    (niwa-skills-install-activation-hook)
    ;; Create the companion view buffer
    (require 'niwa-view)
    (niwa-view--ensure buf)
    (when (niwa-session-plan-mode niwa--session)
      (niwa-plan-mode-restore-pending-approval niwa--session buf))
    (unless inspection-p
      (niwa--run-session-start-hooks source))))

(defun niwa--chat-buffer-setup (buf workspace session-name &optional working-directory)
  "Set up chat buffer BUF in WORKSPACE with SESSION-NAME and WORKING-DIRECTORY."
  (require 'niwa-session-persistence)
  (with-current-buffer buf
    ;; The data buffer is locked to `org-mode' so the persistence layer
    ;; has a single format to round-trip via `gptel-org--save-state'.
    (niwa-chat-prepare-transcript-buffer)
    ;; Create session after mode setup so it isn't wiped
    (setq-local niwa--session
                (niwa-session-create
                 session-name workspace working-directory
                 (niwa-session-persistence-allocate-session-id
                  (niwa-session-artifacts-sessions-dir workspace))))
    (rename-buffer (niwa-session-buffer-name
                    (niwa-session-name niwa--session) workspace) t)
    (niwa--chat-buffer-init-common buf workspace "startup")))

(defun niwa--patch-buffer (&optional create workspace)
  "Get or create the niwa patch staging buffer for WORKSPACE.

This buffer shows diffs generated by the LLM that are awaiting review
and application.  If CREATE is non-nil, create the buffer if it doesn't
exist.  WORKSPACE should be a `niwa-workspace' struct, or nil to use
the current buffer's workspace."
  (let* ((buf (niwa--get-buffer "patch" workspace create))
         (created-p (cdr buf))
         (buf (car buf)))
    (when created-p
      (with-current-buffer buf
        (diff-mode)
        (setq buffer-read-only t)))
    buf))

(defun niwa--get-buffer (name &optional workspace create-p)
  "Get or create a niwa buffer named NAME in WORKSPACE.

NAME is a string used in the buffer name.  For session buffers, use the
session name (e.g., \"main\", \"refactor\").  For auxiliary buffers, use a
descriptive name (e.g., \"patch\").

Buffer name format: *niwa:NAME@WORKSPACE*.

Returns (BUFFER . CREATED-P) where CREATED-P indicates if buffer was
created. When CREATE-P is non-nil and buffer doesn't exist, create it
with workspace."
  (let* ((workspace (or workspace (niwa-workspace)))
         (workspace-name (niwa-workspace-name workspace))
         (buf-name (format "*niwa:%s@%s*" name workspace-name))
         (target-buf (get-buffer buf-name))
         created-p)
    (when (and (not target-buf) create-p)
      (setq target-buf (get-buffer-create buf-name)
            created-p t)
      (with-current-buffer target-buf
        ;; Cache workspace struct for pre-session access
        (setq-local niwa--workspace workspace)
        (niwa-session-artifacts-inhibit-so-long)))
    (when target-buf
      (cons target-buf created-p))))

(defun niwa--workspace-sessions (workspace)
  "Return alist of (SESSION-NAME . BUFFER) for WORKSPACE in buffer-list order.

Scans live buffers for those with a `niwa--session' whose workspace
matches WORKSPACE by type and id.

Skips view, retained-agent, and side-conversation buffers.  Those buffers
carry a session for local context but are not themselves root chat data
buffers."
  (let ((ws-type (niwa-workspace-type workspace))
        (ws-id (niwa-workspace-id workspace))
        sessions)
    (dolist (buf (buffer-list))
      (when (and (buffer-live-p buf)
                 (not (buffer-local-value 'niwa--data-buffer buf))
                 (not (buffer-local-value 'niwa--agent-invocation buf)))
        (when-let* ((session (buffer-local-value 'niwa--session buf))
                    ;; Transient conversations audit into a durable
                    ;; parent session and are not root chat buffers.
                    ((not (niwa-session-audit-session session)))
                    (sw (niwa-session-workspace session))
                    ((eq (niwa-workspace-type sw) ws-type))
                    ((equal (niwa-workspace-id sw) ws-id)))
          (push (cons (niwa-session-name session) buf) sessions))))
    (nreverse sessions)))

(defun niwa--workspace-chat-sessions (workspace)
  "Return WORKSPACE's ordinary live chats in buffer-list order.
Dedicated artifact conversations remain reachable through their artifacts;
each says so itself, so no artifact needs reading."
  (cl-remove-if
   (lambda (entry)
     (let ((session (buffer-local-value 'niwa--session (cdr entry))))
       (or (niwa-session-dedicated-artifact session)
           (niwa-session-note-conversation session))))
   (niwa--workspace-sessions workspace)))

(defun niwa--pick-session (sessions default)
  "Select a buffer from SESSIONS, or return a new display name.
DEFAULT supplies the initial name.  Empty input creates an unnamed session.
Completion labels include identity, because display names need not be unique."
  (let* ((choices
          (mapcar
           (lambda (entry)
             (cons (format "%s [%s]" (car entry)
                           (niwa-session-session-id
                            (buffer-local-value 'niwa--session (cdr entry))))
                   (cdr entry)))
           sessions))
         (choice (completing-read "Session: " choices nil nil nil nil default)))
    (or (cdr (assoc choice choices))
        (unless (string-blank-p choice) choice))))

(defun niwa--ensure-chat-preset (chat-buffer)
  "Apply the default chat preset to CHAT-BUFFER unless it has one."
  (with-current-buffer chat-buffer
    (unless (niwa-session-preset-name niwa--session)
      (niwa-preset-apply
       (or (alist-get niwa-default-chat-preset niwa-action-preset-alist)
           'niwa-implement)))))

(defun niwa--display-chat-buffer (chat-buffer)
  "Ensure CHAT-BUFFER has a preset and display its view."
  (niwa--ensure-chat-preset chat-buffer)
  (when-let* ((window (display-buffer
                       (or (buffer-local-value 'niwa--view-buffer chat-buffer)
                           chat-buffer)
                       gptel-display-buffer-action)))
    (when-let* ((composer (niwa-view-composer-display window))
                ((eq window (selected-window))))
      (select-window composer))
    window))

(defun niwa--normalize-session-directory (directory workspace)
  "Return DIRECTORY as an absolute directory inside WORKSPACE."
  (let* ((dir (file-name-as-directory (expand-file-name directory)))
         (root (file-name-as-directory
                (expand-file-name (niwa-workspace-root workspace)))))
    (unless (file-directory-p dir)
      (user-error "%s is not a directory" dir))
    (unless (file-in-directory-p dir root)
      (user-error "Working directory must be inside workspace root %s"
                  root))
    dir))

(defun niwa--read-session-directory (workspace)
  "Read a session working directory under WORKSPACE."
  (niwa--normalize-session-directory
   (read-directory-name "Start niwa in directory: "
                        (niwa-workspace-root workspace)
                        (niwa-workspace-root workspace)
                        t)
   workspace))

(defun niwa--sessions-in-working-directory (sessions working-directory)
  "Filter SESSIONS to those whose session cwd is WORKING-DIRECTORY."
  (let ((dir (file-name-as-directory (expand-file-name working-directory))))
    (delq nil
          (mapcar
           (lambda (entry)
             (let ((buf (cdr entry)))
               (when (and (buffer-live-p buf)
                          (with-current-buffer buf
                            (and (bound-and-true-p niwa--session)
                                 (equal dir
                                        (niwa-session-working-directory
                                         niwa--session)))))
                 entry)))
           sessions))))

(defun niwa--start-chat (workspace working-directory prompt-session
                                      &optional directory-scoped)
  "Start or switch to a chat in WORKSPACE with WORKING-DIRECTORY.

When PROMPT-SESSION is non-nil, prompt for the target session.  When
DIRECTORY-SCOPED is non-nil, only sessions whose working directory matches
WORKING-DIRECTORY are considered."
  (let* ((all-sessions (niwa--workspace-chat-sessions workspace))
         (sessions (if directory-scoped
                       (niwa--sessions-in-working-directory
                        all-sessions working-directory)
                     all-sessions))
         (selection
          (cond
           (prompt-session (niwa--pick-session sessions nil))
           ((null sessions) nil)
           ((= (length sessions) 1) (cdar sessions))
           (t (niwa--pick-session sessions nil))))
         (chat-buffer
          (if (bufferp selection) selection
            (niwa--chat-buffer selection t workspace working-directory))))
    (niwa--display-chat-buffer chat-buffer)))

(defun niwa--active-chat-buffer (&optional workspace)
  "Find the active chat (data) buffer for WORKSPACE.

Returns the gptel data buffer, never the view buffer.

If already in a niwa chat buffer, return it.  If in a view
buffer, return the associated data buffer.  If in a sub-agent
buffer, return the invocation's `parent-data-buffer'
when live, otherwise fall through to the scan branch.  Otherwise
scan for session buffers matching WORKSPACE: if one exists return
it, if multiple return the most recently used one.  Returns nil
if none found."
  (or
   (cond
    ;; An orphaned agent must fall back to workspace roots, not mistake its
    ;; copied session for root ownership or follow a view-buffer branch.
    (niwa--agent-invocation
     (let ((parent (niwa-agent-invocation-parent-data-buffer
                    niwa--agent-invocation)))
       (and (buffer-live-p parent) parent)))
    ((and (bound-and-true-p niwa--data-buffer)
          (buffer-live-p niwa--data-buffer))
     niwa--data-buffer)
    ((bound-and-true-p niwa--session) (current-buffer)))
   (when-let* ((workspace (or workspace (niwa-workspace))))
     (cdar (niwa--workspace-chat-sessions workspace)))))

(defun niwa--generate-final-patch (workspace request callback)
  "Generate final diffs for all tracked files in REQUEST, then call CALLBACK.

CALLBACK receives PATCH and ERROR: PATCH is a unified diff string showing
original -> final state for each file, compared from REQUEST's snapshots
against current file contents in WORKSPACE, and nil exactly when ERROR
carries the first failed diff's condition data.  The files are read up
front and diffed one after another; CALLBACK may run before this returns.

REQUEST is resolved by the caller while the turn is still live.  Reading
the buffer-local instead was a use-after-settle: the loop reads every
touched file, and on a remote workspace `insert-file-contents\' hands
control to TRAMP\'s wait loop, which runs timers and process sentinels.
One of those settles the turn and clears `niwa--current-request\'."
  (let* ((workspace-root (niwa-workspace-root workspace))
         (snapshots (and request (niwa-request-file-snapshots request)))
         paths entries parts)
    (when snapshots
      (maphash (lambda (filepath _original) (push filepath paths)) snapshots))
    (dolist (filepath (sort paths #'string<))
      (let* ((original (gethash filepath snapshots))
             coding
             (current (when (file-regular-p filepath)
                        (with-temp-buffer
                          (insert-file-contents filepath)
                          (setq coding last-coding-system-used)
                          (buffer-string))))
             (relpath (file-relative-name filepath workspace-root)))
        ;; Captures are literal bytes for Rewind.  This diff is presentation,
        ;; like the decoded current file above, not a file-history snapshot.
        (when (stringp original)
          (setq original
                (decode-coding-string
                 original (or coding (detect-coding-string original t) 'undecided))))
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
          (push (list (concat (format "diff --git a/%s b/%s\n" relpath relpath)
                              (cond
                               ((and (or (not original) (string-empty-p original))
                                     (and current (not (string-empty-p current))))
                                "new file mode 100644\n")
                               ((and (and original (not (string-empty-p original)))
                                     (or (not current) (string-empty-p current)))
                                "deleted file mode 100644\n")))
                      (or original "") (or current "") relpath)
                entries))))
    (setq entries (nreverse entries))
    (cl-labels
        ((next ()
           (if (null entries)
               ;; Collect per-file parts and join once: appending to one
               ;; growing string reallocated the whole patch per file.
               (funcall callback (apply #'concat (nreverse parts)) nil)
             (pcase-let ((`(,header ,original ,current ,relpath) (pop entries)))
               (niwa-generate-diff
                original current relpath
                (lambda (diff error)
                  (if error
                      (funcall callback nil error)
                    (push (concat header diff "\n") parts)
                    (next)))
                ;; Abort stops producers, not the final review of their effects.
                nil t)))))
      (next))))

(defun niwa--replace-patch-buffer (patch-content)
  "Replace patch buffer contents with PATCH-CONTENT.
If PATCH-CONTENT is empty, does nothing."
  (when (and patch-content (> (length patch-content) 0))
    (with-current-buffer (niwa--patch-buffer t)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert patch-content)
        ;; The buffer persists across turns; re-entering the mode on
        ;; every stream end re-ran its whole setup for nothing.
        (unless (derived-mode-p 'diff-mode)
          (diff-mode))
        (goto-char (point-min))))
    (niwa--indicate-patch-ready)))

(defun niwa--indicate-patch-ready ()
  "Provide visual feedback that a patch is ready for review."
  (message "Patch ready in *niwa-patch* buffer")
  (when niwa-show-patch-buffer
    (display-buffer (niwa--patch-buffer))))

;;;###autoload
(defun niwa-clear-patch-buffer ()
  "Clear the patch buffer."
  (interactive)
  (when-let* ((buf (niwa--patch-buffer)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)))
    (message "Patch buffer cleared")))


;;
;;; Request cancellation

(defun niwa-abort (&optional buf)
  "Abort any active request associated with buffer BUF.

Thus, abort `gptel' requests running in the niwa chat buffer
associated with the `niwa-workspace' for BUF.

If a callback was provided to the original request, it will be called
with the \\='abort symbol as the error parameter.

BUF defaults to the current buffer if not specified."
  (interactive)
  (with-current-buffer (or buf (current-buffer))
    (when-let* ((chat-buffer (niwa--active-chat-buffer))
                (_ (buffer-live-p chat-buffer)))
      (unwind-protect
          (progn
	    ;; Stop the spinner in the view buffer.  Unrelated to the
	    ;; canceller drain but still worth doing up front so the UI
	    ;; reflects the teardown while the rest of the sequence runs.
	    (when-let* ((view-buf (buffer-local-value 'niwa--view-buffer
                                                      chat-buffer))
			(_ (buffer-live-p view-buf)))
              (with-current-buffer view-buf
		(niwa-view-stream-stop)))
	    ;; Phase 1: mark cancellation before draining.  Each canceller
	    ;; settles its owned overlays with `aborted' so FSMs parked in
	    ;; TOOL can advance out. Draining before the
	    ;; `gptel-abort' loop is load-bearing -- follow-up HTTP
	    ;; requests launched by `aborted' callbacks land in
	    ;; `gptel--request-alist' and get torn down in phase 2.
	    (with-current-buffer chat-buffer
              (when (bound-and-true-p niwa--current-request)
		(niwa--queue-reconciliation-reminder niwa--session)
		(niwa-request-cancel niwa--current-request))
              ;; flush any queued permission entries with 'aborted
              ;; so callbacks fire and the FSMs they belong to can unwind.
              ;; Run after the canceller drain so canceller-driven entries
              ;; have a chance to settle first.
              (when (fboundp 'niwa-permission-queue-abort-all)
		(niwa-permission-queue-abort-all))
              (when (and niwa--session
                         (niwa-session-pending-plan-approval niwa--session))
                (niwa-plan-approval-abort))
              (when (and (boundp 'niwa-compact-run-cancel)
			 (functionp niwa-compact-run-cancel))
		(funcall niwa-compact-run-cancel)))
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
              (when-let* ((goal (and (bound-and-true-p niwa--session)
				     (niwa-session-goal niwa--session)))
			  ((eq (niwa-goal-status goal) 'active)))
		(setf (niwa-goal-status goal) 'paused
                      (niwa-goal-reason goal) "interrupted by user"
	                      (niwa-goal-reason-diagnostic goal) nil
                      (niwa-goal-updated-at goal)
                      (format-time-string "%FT%T%z")))
              (if (bound-and-true-p niwa--current-request)
			  ;; Terminal work still needs this reservation and its file
			  ;; snapshots.  Abort stops producers, not durable settlement.
			  (unless niwa--turn-settlements-pending
			    (niwa-request-end))
		;; A request can disappear without its own teardown -- a terminal
		;; transition lost with the process that would have driven it.
		;; `niwa-request-end' is the only place that idles the root
		;; roster, and it needs a request, so the roster stays marked
		;; running with nothing running it and the view spins forever.
		;; An abort is the user asserting the opposite.
		(when (bound-and-true-p niwa--session)
		  (setf (niwa-session-agent-root-activity niwa--session)
			'idle)))))
        ;; An abort can arrive from a process filter inside a remote
        ;; operation, where a target write would nest; checkpoint once idle.
        ;; Without transport integration nothing can nest: save now.
        (unless (niwa-transport-run-when-idle
                 (list 'abort-checkpoint chat-buffer)
                 (buffer-local-value 'default-directory chat-buffer)
                 (lambda () (niwa-session-persistence-autosave-buffer chat-buffer)))
          (niwa-session-persistence-autosave-buffer chat-buffer))))))


;;
;;; Goal implementation

(defun niwa--close-unclosed-blocks ()
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

(defvar-local niwa--pending-model-input nil
  "One-shot model input replacing the latest stored prompt at request time.")

(defun niwa--insert-local-user-turn
    (prompt &optional display-text kind hook-context no-spinner)
  "Insert PROMPT as a user turn without sending a request.

DISPLAY-TEXT is mirrored to the view, defaulting to PROMPT.  KIND and
HOOK-CONTEXT are forwarded to `niwa-view--begin-external-turn',
with NO-SPINNER forwarded when non-nil.  A pending turn attribution, such
as a background event's, is recorded with the turn and consumed."
  (goto-char (point-max))
  (let* ((body-start (niwa--insert-user-turn prompt))
         (user-source (cons (copy-marker body-start nil)
                            (copy-marker (point) nil)))
         (attribution (and (boundp 'niwa-view--pending-guest-attribution)
                           niwa-view--pending-guest-attribution)))
    (when (and display-text (not (equal display-text prompt)))
      (insert (niwa-tool-render-data-format
               (list :kind 'user-display :text display-text))))
    ;; Like a guest's, the attribution belongs inside the user turn, before
    ;; the response marker below exists.
    (when attribution
      (insert (niwa--format-hook-audit-record attribution))
      (setq niwa-view--pending-guest-attribution nil))
    (niwa-collaboration--safe-accepted-prompt (current-buffer))
    (let ((data-turn-start (copy-marker (point) nil)))
      (when-let* ((view (and (boundp 'niwa--view-buffer)
                            niwa--view-buffer))
                  ((buffer-live-p view))
                  ((fboundp 'niwa-view--begin-external-turn)))
        (with-current-buffer view
          (niwa-view--begin-external-turn
           (or display-text prompt) data-turn-start kind hook-context
           no-spinner user-source attribution)))
      data-turn-start)))

(defun niwa--submit-generated-turn
    (prompt &optional display-text prompt-submission prepared-outcome trigger)
  "Insert and send generated PROMPT through the canonical request path.
DISPLAY-TEXT is shown in the view instead of PROMPT.  PROMPT-SUBMISSION owns
accepted hook context until the turn is inserted.  PREPARED-OUTCOME carries
skill-expanded model input and transcript render data.  TRIGGER names the
automatic initiator; omit it for user-started turns."
  (let* ((hook-context
          (and prompt-submission
               (niwa-prompt-submission-context prompt-submission)))
         (preparation (or niwa-implementation-policy--admission-preparation
                          (niwa-implementation-policy-capture-preparation)))
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
    (when preparation
      (niwa-implementation-policy-assert-preparation preparation))
    (niwa--insert-local-user-turn
     stored-prompt display-text nil hook-context)
    (when prompt-submission
      (niwa-prompt-submission-commit prompt-submission)
      (niwa-session-naming-consider
       niwa--session
       (or (niwa-prompt-submission-display-text prompt-submission) display-text prompt)))
    ;; Insertion/view hooks can yield or change a Goal before the first model
    ;; transform.  A staged kickoff policy must not become Direct authority.
    (when preparation
      (niwa-implementation-policy-assert-preparation preparation))
    (niwa--send-request
     (or model-input (and hook-context stored-prompt)) trigger)))

(defvar niwa--dispatching nil
  "Non-nil while `niwa--dispatch-request' runs its local gptel send.
The `gptel-send' advice then passes through instead of dispatching again.")

(defun niwa--dispatch-request (model-input local-send &optional native-input trigger)
  "Dispatch MODEL-INPUT using the resolved root request policy.
LOCAL-SEND starts the caller's native gptel send when that engine is selected.
NATIVE-INPUT replaces a missing MODEL-INPUT only for an external engine, which
reads no prompt transforms; gptel keeps its stored transcript prompt.
TRIGGER identifies an automatic root submission to the external engine.
The effective model and effort apply only to this request, not saved selection."
  (condition-case err
      (let* ((policy (niwa-skills-request-model-policy))
             (backend (plist-get policy :backend)))
        ;; Never bind the shared root's selection around startup: synchronous
        ;; hooks and yielding checks can change that baseline.  gptel's model
        ;; transform owns its private prompt locals; native takes explicit args.
        (when (memq niwa--session (bound-and-true-p niwa-claude-code-fork--sessions))
          (niwa-diagnostic-user-error
           (niwa-diagnostic-scope (current-buffer))
           "Wait for native history preparation before starting a turn"))
        (if (niwa-claude-code-backend-p backend)
            (if (and niwa--session (niwa-session-audit-session niwa--session))
                (niwa-claude-code-btw-send (or model-input native-input) policy)
              (niwa-claude-code-send (or model-input native-input) policy nil trigger))
          (when (and niwa--session
                     (assoc "root" (niwa-session-external-conversations niwa--session)))
            (niwa-claude-code-release-history niwa--session))
          (setq-local niwa--pending-model-input model-input)
          (unwind-protect (let ((niwa--dispatching t))
                            (niwa-auth-assert-ready backend)
                            (funcall local-send))
            (setq-local niwa--pending-model-input nil))))
    (error
     ;; A stale preparation does not own the current Goal (or its failure).
     (let* ((preparation niwa-implementation-policy--admission-preparation)
            (goal (plist-get preparation :goal)))
       (when (and (not (eq (car err) 'niwa-stale-implementation-preparation))
                  (or (not preparation)
                      (and goal niwa--session
                           (eq goal (niwa-session-goal niwa--session))
                           (equal (plist-get preparation :goal-id) (niwa-goal-id goal))
                           (equal (plist-get preparation :policy)
                                  (niwa-goal-implementation-policy goal)))))
         (niwa-implementation-policy-pause-failure (current-buffer) err)))
     (signal (car err) (cdr err)))))

(defun niwa--send-request (&optional model-input trigger)
  "Send the current prompt through the selected conversation engine.
MODEL-INPUT replaces the stored prompt for this request only.
TRIGGER identifies an automatic submission across asynchronous preparation."
  (niwa--dispatch-request
   model-input
   (lambda ()
     (gptel-request nil
		    :context (if (listp trigger) trigger (list :turn-trigger trigger))
		    :stream gptel-stream
		    :transforms gptel-prompt-transform-functions
		    :fsm (gptel-make-fsm
			  :table gptel-send--transitions
			  :handlers gptel-send--handlers)))
   nil trigger))

(defun niwa--implement-plan (action-plist)
  "Implement the plan described by ACTION-PLIST.

ACTION-PLIST is a plist with keys:
  :implementation-policy - Accepted request-local model, effort and mode
  :implementation-goal - Expected Goal owner, nil for Direct
  :implementation-goal-id - Reserved kickoff identity, nil for Direct
  :display-text   - Optional compact transcript display text
  :prompt-submission - Accepted prompt transaction
  :prepared-outcome - Prepared skill and transcript components."
  (let* ((policy (copy-tree
                 (niwa-implementation-policy-validate
                  (plist-get action-plist :implementation-policy))))
         (display-text (or (plist-get action-plist :display-text)
                           "Implement accepted plan"))
         (prompt-submission (plist-get action-plist :prompt-submission))
         (prepared-outcome (plist-get action-plist :prepared-outcome))
         (goal (plist-get action-plist :implementation-goal))
         (goal-id (plist-get action-plist :implementation-goal-id))
         (prompt (and prompt-submission
                      (niwa-prompt-submission-input prompt-submission))))
    (unless prompt
      (error "Implementation requires an accepted prompt submission"))
    (unless policy
      (error "Implementation requires an accepted policy"))
    ;; Preparation/publication can yield to another admission or settlement.
    ;; Neither Direct nor Goal owns that turn: reject before staging policy or
    ;; inserting input, rather than letting WAIT attach our FSM to its request.
    (when (or niwa--current-request niwa--turn-settlements-pending)
      (signal 'niwa-stale-implementation-preparation nil))
    ;; Do not reinterpret a stale Goal kickoff as a Direct policy override.
    (when (or goal goal-id)
      (unless (and goal niwa--session
                   (eq goal (niwa-session-goal niwa--session))
                   (equal goal-id (niwa-goal-id goal))
                   (eq 'active (niwa-goal-status goal))
                   (equal policy (niwa-goal-implementation-policy goal)))
        (signal 'niwa-stale-implementation-preparation nil)))
    (condition-case err
        (progn
          (setq-local niwa--pending-implementation-policy policy)
          (let ((niwa-implementation-policy--admission-preparation
                 (niwa-implementation-policy-capture-preparation)))
            ;; Closing blocks can run the same insertion hooks as the prompt.
            (niwa--close-unclosed-blocks)
            (when prepared-outcome
              (setq-local niwa-skills--pending-request-context
                          (plist-get prepared-outcome :request-context)))
            (niwa--submit-generated-turn
             prompt display-text prompt-submission prepared-outcome)))
      (error
       (setq-local niwa-skills--pending-request-context nil)
       (when (eq policy niwa--pending-implementation-policy)
         (setq-local niwa--pending-implementation-policy nil))
       (signal (car err) (cdr err))))))

;; Before session persistence's exit hook: handlers see their sessions
;; while executions, transports, and ownership are still intact.
(add-hook 'kill-emacs-hook #'niwa--end-sessions-on-exit -50)

(provide 'niwa-chat)

;;; niwa-chat.el ends here
