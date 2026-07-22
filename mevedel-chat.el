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
(declare-function cl-find-if "cl-seq" (cl-pred cl-list &rest cl-keys))
(declare-function cl-position "cl-seq" (cl-item cl-seq &rest cl-keys))
(declare-function cl-remove-if-not "cl-seq"
		  (cl-pred cl-list &rest cl-keys))
(declare-function cl-sort "cl-seq" (cl-seq cl-pred &rest cl-keys))

;; `gptel'
(declare-function gptel-markdown-cycle-block "ext:gptel" nil)
(declare-function gptel-mode "ext:gptel" (&optional arg))
(declare-function gptel-send "ext:gptel" nil)
(defvar gptel--markdown-block-map)
(defvar gptel-default-mode)
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

;; `mevedel-hooks'
(declare-function mevedel-hooks-event-plist "mevedel-hooks"
		  (event &optional session workspace &rest extra))
(declare-function mevedel-hooks-record-session-context "mevedel-hooks"
		  (session decision &optional event))
(declare-function mevedel-hooks-run-event "mevedel-hooks"
		  (event event-plist callback &optional session
			 workspace request invocation))

;; `mevedel-overlays'
(declare-function mevedel--child-instructions "mevedel-overlays"
		  (instruction))
(declare-function mevedel--delete-instruction "mevedel-overlays"
		  (instruction))
(declare-function mevedel--directive-llm-prompt "mevedel-overlays"
		  (directive))
(declare-function mevedel--directive-text "mevedel-overlays"
		  (directive))
(declare-function mevedel--directivep "mevedel-overlays" (instruction))
(declare-function mevedel--find-directive-by-uuid "mevedel-overlays"
		  (uuid))
(declare-function mevedel--highest-priority-instruction
		  "mevedel-overlays"
		  (instructions &optional non-processing))
(declare-function mevedel--instructions-at "mevedel-overlays"
		  (position &optional type))
(declare-function mevedel--topmost-instruction "mevedel-overlays"
		  (instruction type))
(declare-function mevedel--update-instruction-overlay
		  "mevedel-overlays" (instruction &optional force))

;; `mevedel-permissions'
(declare-function mevedel-permission-mode-set-raw
		  "mevedel-permissions" (mode))

;; `mevedel-pipeline'
(declare-function mevedel-pipeline--format-render-data-block
		  "mevedel-pipeline" (render-data))

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
(declare-function mevedel-session-persistence-fork-now
		  "mevedel-session-persistence" (buffer))
(declare-function mevedel-session-persistence-header-segment
		  "mevedel-session-persistence" nil)
(declare-function mevedel-session-persistence-save
		  "mevedel-session-persistence" (session buffer))

;; `mevedel-skills-core'
(declare-function mevedel-skills--release-on-kill
		  "mevedel-skills-core" nil)
(declare-function mevedel-skills-install "mevedel-skills-core"
		  (session &optional buffer))

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
(declare-function mevedel-goal-reason "mevedel-structs" (cl-x) t)
(declare-function mevedel-goal-status "mevedel-structs" (cl-x) t)
(declare-function mevedel-request-drain-cancellers "mevedel-structs"
		  (request))
(declare-function mevedel-request-end "mevedel-structs" nil)
(declare-function mevedel-request-file-snapshots "mevedel-structs"
		  (cl-x) t)
(declare-function mevedel-session-create "mevedel-structs"
		  (name workspace &optional working-directory))
(declare-function mevedel-session-name "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-permission-mode "mevedel-structs"
		  (cl-x) t)
(declare-function mevedel-session-working-directory "mevedel-structs"
		  (cl-x) t)
(declare-function mevedel-session-workspace "mevedel-structs" (cl-x) t)
(declare-function mevedel-workspace-id "mevedel-structs" (cl-x) t)
(declare-function mevedel-workspace-name "mevedel-structs" (cl-x) t)
(declare-function mevedel-workspace-root "mevedel-structs" (cl-x) t)
(declare-function mevedel-workspace-type "mevedel-structs" (cl-x) t)
(defvar mevedel--current-request)

;; `mevedel-tool-fs'
(declare-function mevedel-tools--generate-diff "mevedel-tool-fs"
		  (original modified filepath))

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

(defcustom mevedel-show-chat-buffer t
  "Control if the mevedel chat buffer should be shown automatically.

If non-nil, the chat buffer will automatically be displayed."
  :type 'boolean
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

(defun mevedel--run-session-start-hooks (source)
  "Run session-start hooks for the current buffer with SOURCE."
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

(defun mevedel--chat-buffer-init-common (buf workspace source)
  "Set up BUF for WORKSPACE and start its lifecycle with SOURCE.

Caller must already have set BUF's buffer-local `mevedel--session'.
Wires the FSM handler chain, header-line, visual settings, all
per-buffer hooks, the skill set, default reminders, and the companion
view buffer.

Both `mevedel--chat-buffer-setup' (fresh path) and the resume path
\(`mevedel-session-persistence-restore') call this after planting the
session struct. SOURCE is `startup' or `resume' as a string."
  (with-current-buffer buf
    (when (derived-mode-p 'org-mode)
      (mevedel--chat-buffer-disable-org-element-cache))
    (setq-local gptel-org-convert-response nil)
    (setq-local gptel-org-branching-context nil)
    (mevedel-preset-restore-session mevedel--session buf)
    (mevedel-reminders-install-defaults mevedel--session)
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
    ;; Wrap lines
    (visual-line-mode +1)
    ;; Auto-scroll when at end of buffer
    (setq-local window-point-insertion-type t)
    ;; Install the session working directory before cwd-dependent setup
    ;; such as skill discovery and dynamic prompt construction.
    (setq-local default-directory
                (or (mevedel-session-working-directory mevedel--session)
                    (mevedel-workspace-root workspace)))
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
    (add-hook 'kill-buffer-hook
              #'mevedel--run-session-end-hooks nil t)
    (add-hook 'gptel-post-response-functions
              #'mevedel-view-stream-render-response nil t)
    (add-hook 'gptel-post-response-functions
              #'mevedel-plan-mode--post-response t t)
    ;; Repair raw model input before view hooks observe the call and before
    ;; gptel maps the arguments into the pipeline wrapper.
    (require 'mevedel-tool-repair)
    (add-hook 'gptel-pre-tool-call-functions
              #'mevedel-tool-repair-pre-tool-call -100 t)
    (add-hook 'gptel-post-tool-call-functions
              #'mevedel-tool-repair-post-tool-call -100 t)
    (add-hook 'gptel-post-response-functions
              #'mevedel-tool-repair-clear-ledger nil t)
    (add-hook 'kill-buffer-hook #'mevedel-tool-repair-clear-ledger nil t)
    (add-hook 'gptel-pre-tool-call-functions
              #'mevedel-view-stream-spinner-hook nil t)
    ;; Incremental view updates on tool boundaries so the user sees
    ;; progress per tool call, not only at turn end.
    (add-hook 'gptel-pre-tool-call-functions
              #'mevedel-view-stream-pre-tool nil t)
    (add-hook 'gptel-post-tool-call-functions
              #'mevedel-view-stream-post-tool nil t)
    ;; Debounced mid-turn text update: streams text chunks into the
    ;; view a few times per second while the LLM is producing text.
    ;; Tool-boundary hooks cancel the pending timer and render
    ;; immediately, so this never delays tool-call feedback.
    (add-hook 'gptel-post-stream-hook #'mevedel-view-stream-schedule nil t)
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
    (mevedel--run-session-start-hooks source)))

(defun mevedel--chat-buffer-setup (buf workspace session-name &optional working-directory)
  "Set up chat buffer BUF in WORKSPACE with SESSION-NAME and WORKING-DIRECTORY."
  (with-current-buffer buf
    ;; Set major mode first -- this calls `kill-all-local-variables'.
    ;; Buffer-locals set before this point are wiped unless
    ;; permanent-local.  The data buffer is locked to `org-mode' so
    ;; the persistence layer has a single format to round-trip via
    ;; `gptel-org--save-state'.
    (let ((org-agenda-file-menu-enabled nil)
          (org-element-use-cache nil)
          (org-element-cache-persistent nil))
      (require 'mevedel-utilities)
      (mevedel--transcript-org-mode))
    (mevedel--chat-buffer-disable-org-element-cache)
    (setq-local gptel-org-convert-response nil)
    (setq-local gptel-org-branching-context nil)
    ;; Enable `gptel-mode'
    (gptel-mode +1)
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

Skips view buffers and buffers whose `mevedel--agent-invocation' is
bound.  Those buffers carry a session for local context but they are
not themselves chat data buffers."
  (let ((ws-type (mevedel-workspace-type workspace))
        (ws-id (mevedel-workspace-id workspace))
        sessions)
    (dolist (buf (buffer-list))
      (when (and (buffer-live-p buf)
                 (not (buffer-local-value 'mevedel--data-buffer buf))
                 (not (buffer-local-value 'mevedel--agent-invocation buf)))
        (when-let* ((session (buffer-local-value 'mevedel--session buf))
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
             (current (when (file-exists-p filepath)
                        (with-temp-buffer
                          (insert-file-contents filepath)
                          (buffer-string))))
             (relpath (file-relative-name filepath workspace-root)))

        ;; Generate diff if file changed, was deleted, or was created
        (when (or
               ;; Modified
               (and original current (not (string= original current)))
               ;; Deleted
               (and original (not current))
               ;; Created
               (and (not original) current))
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

(defun mevedel--revise-directive-prompt (content &optional patch-buffer directive)
  "Generate a prompt for revising based on CONTENT (revision instructions).

The patch comes from either:
1. DIRECTIVE's stored patch (if provided and has one)
2. PATCH-BUFFER contents (defaulting to the mevedel patch buffer)

DIRECTIVE is the instruction overlay being revised."
  (let* ((directive-patch (when directive
                            (overlay-get directive 'mevedel-directive-patch)))
         (patch-buffer (or patch-buffer (mevedel--patch-buffer)))
         (patch-content
          (cond
           (directive-patch directive-patch)
           (patch-buffer
            (with-current-buffer patch-buffer
              (buffer-substring-no-properties (point-min) (point-max))))
           (t (user-error "No patch found for revision")))))
    (format
     "## TASK: Revise your previous implementation based on new feedback.

### INSTRUCTIONS:

1. Read the revision instructions below (if any)
2. Review your previous patch
3. Read and understand all provided references
4. Use the references to complete the request
5. Understand what needs to be changed or improved
6. Create a NEW implementation that addresses the feedback
7. Use your tools to make the changes
%s
==================================
YOUR PREVIOUS WORK (for reference)
==================================

%s"
     (if (and content (not (string-empty-p content)))
         (format "\n### REVISION INSTRUCTIONS:\n\n%s\n\n" content)
       "")
     patch-content)))

(defun mevedel--discuss-directive-prompt (content)
  "Generate a discussion prompt for CONTENT in the current buffer."
  (format
   "## TASK: Answer the following request.

### INSTRUCTIONS:

1. Read and understand the request below
2. Read and understand all provided references
3. Use the references to complete the request
4. Use your tools to access files as needed

### REQUEST:

%s"
   content))


;;
;;; Directive processing

(defvar-local mevedel--current-directive-uuid nil
  "UUID of the directive currently being processed.")

(defconst mevedel--directive-action-labels
  '((implement . "Implement")
    (revise . "Revise")
    (discuss . "Discuss")
    (tutor . "Tutor"))
  "Plain display labels for directive actions.")

(defun mevedel--directive-action-label (action)
  "Return the display label for directive ACTION."
  (or (alist-get action mevedel--directive-action-labels)
      (capitalize (replace-regexp-in-string
                   "[-_]+" " " (symbol-name action)))))

(defun mevedel--directive-display-text (action directive-text)
  "Return the human-facing transcript text for ACTION and DIRECTIVE-TEXT."
  (let ((label (mevedel--directive-action-label action)))
    (if (string-empty-p (string-trim directive-text))
        label
      (format "%s: %s" label directive-text))))

(defun mevedel--insert-directive-turn (directive-text prompt action)
  "Insert a directive turn into the current chat data buffer.

DIRECTIVE-TEXT is the short overlay text shown in the transcript.
PROMPT is the full LLM-facing prompt, inserted in an ignored
`:PROMPT:' drawer for inspection.  ACTION is the directive action
symbol.  Return a marker positioned where the assistant response
should be inserted."
  (require 'mevedel-utilities)
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
                              'gptel 'ignore
                              'keymap gptel--markdown-block-map)
                (propertize full-prompt-str 'gptel 'ignore)))
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

(defun mevedel--process-directive (directive preset prompt-fn callback)
  "Process DIRECTIVE using PRESET and PROMPT-FN, calling CALLBACK when complete.

DIRECTIVE is the instruction overlay to process.
PRESET is the gptel preset to use (mevedel-implement, mevedel-revise,
mevedel-discuss).
PROMPT-FN is a function that generates the prompt from the directive
content.
CALLBACK is called with (err fsm) when processing completes.

Updates directive status and overlay, handles success/failure states."
  (let* (;; Get chat buffer for the directive's buffer workspace
         (workspace (with-current-buffer (overlay-buffer directive)
                      (mevedel-workspace)))
         (chat-buffer (mevedel--chat-buffer "main" t workspace))
         (directive-uuid (overlay-get directive 'mevedel-uuid))
         (directive-text (mevedel--directive-text directive))
         (content (mevedel--directive-llm-prompt directive))
         (prompt (funcall prompt-fn content))
         response-start
         (callback-fn (lambda (err fsm)
                        (let ((live-directive
                               (or (and (overlay-buffer directive)
                                        directive)
                                   (mevedel--find-directive-by-uuid
                                    directive-uuid))))
                          (if err
                              (let ((reason (if (eq err 'abort)
                                                "aborted"
                                              (format "%s" err))))
                                (when-let* ((live-directive live-directive)
                                            (directive-buffer
                                             (overlay-buffer live-directive)))
                                  (overlay-put live-directive
                                               'mevedel-directive-status
                                               'failed)
                                  (overlay-put live-directive
                                               'mevedel-directive-fail-reason
                                               reason)
                                  (mevedel--update-instruction-overlay
                                   live-directive t)
                                  (with-current-buffer directive-buffer
                                    (pulse-momentary-highlight-region
                                     (overlay-start live-directive)
                                     (overlay-end live-directive))))
                                (when (buffer-live-p chat-buffer)
                                  (with-current-buffer chat-buffer
                                    (setq mevedel--current-directive-uuid nil)))
                                (when callback
                                  (funcall callback err fsm)))

                            (when-let* ((live-directive live-directive)
                                        (directive-buffer
                                         (overlay-buffer live-directive)))
                              (overlay-put live-directive
                                           'mevedel-directive-status
                                           'succeeded)
                              (with-current-buffer directive-buffer
                                ;; Delete any child directives of the top-level
                                ;; directive.
                                (let ((child-directives
                                       (cl-remove-if-not
                                        #'mevedel--directivep
                                        (mevedel--child-instructions
                                         live-directive))))
                                  (dolist (child-directive child-directives)
                                    (mevedel--delete-instruction
                                     child-directive)))
                                (save-excursion
                                  (goto-char (overlay-start live-directive))
                                  (overlay-put live-directive 'evaporate t))
                                (mevedel--update-instruction-overlay
                                 live-directive t)
                                (pulse-momentary-highlight-region
                                 (overlay-start live-directive)
                                 (overlay-end live-directive))))
                            (when (buffer-live-p chat-buffer)
                              (with-current-buffer chat-buffer
                                (setq mevedel--current-directive-uuid nil)))
                            (when callback
                              (funcall callback err fsm)))))))

    (with-current-buffer chat-buffer
      (when mevedel--current-request
        (user-error "A request is already active -- wait or abort first"))
      (when (bound-and-true-p mevedel-session--fork-pending)
        (require 'mevedel-session-persistence)
        (mevedel-session-persistence-fork-now chat-buffer))
      (setq mevedel--current-directive-uuid (overlay-get directive 'mevedel-uuid)))

    (save-some-buffers nil #'mevedel--directive-save-buffer-p)

    (overlay-put directive 'mevedel-directive-status 'processing)
    (mevedel--update-instruction-overlay directive t)
    (pulse-momentary-highlight-region (overlay-start directive) (overlay-end directive))

    ;; Display view buffer if configured (fall back to data buffer)
    (when mevedel-show-chat-buffer
      (display-buffer (or (buffer-local-value 'mevedel--view-buffer chat-buffer)
                          chat-buffer)
                      gptel-display-buffer-action))

    (with-current-buffer chat-buffer
      (mevedel-preset-apply
       (alist-get mevedel-default-chat-preset mevedel-action-preset-alist))
      (setq response-start
            (mevedel--insert-directive-turn
             directive-text prompt
             (overlay-get directive 'mevedel-directive-action)))
      (overlay-put directive 'mevedel-directive-response-start response-start)
      (when-let* ((view-buf mevedel--view-buffer)
                  (_ (buffer-live-p view-buf)))
        (with-current-buffer view-buf
          (mevedel-view--begin-external-turn
           (mevedel--directive-display-text
            (overlay-get directive 'mevedel-directive-action)
            directive-text)
           response-start
           'directive)))

      (mevedel-with-preset preset
        ;; Agents and deferred tools are wired up by the preset's own
        ;; `:post' hook (see `mevedel-define-preset').  FSM handlers are
        ;; installed buffer-locally on `gptel-send--handlers' by
        ;; `mevedel--chat-buffer-setup'.
        (let* ((request-callback
                (lambda (exit-code fsm)
                  (let* ((state (gptel-fsm-state fsm))
                         (error
                          (cond
                           ;; If we have a non-nil exit code (i.e. 'abort), just
                           ;; use it as the error.
                           (exit-code)
                           ;; If the FSM is in an errored state, extract the
                           ;; error text.
                           ((eq state 'ERRS)
                            (let* ((info (gptel-fsm-info fsm))
                                   (error (plist-get info :error))
                                   (http-msg (plist-get info :status))
                                   (error-type (plist-get error :type))
                                   (error-msg (plist-get error :message)))
                              (or error-msg (format "%s: %s" error-type http-msg))))
                           ;; Otherwise, consider the request successful
                           (t
                            nil))))

                    ;; Call overlay callback, including the original callback if
                    ;; provided
                    (when (functionp callback-fn)
                      (funcall callback-fn error fsm)))))
               (fsm (gptel-request prompt
                      :buffer chat-buffer
                      :position response-start
                      :stream gptel-stream
                      :transforms gptel-prompt-transform-functions
                      :fsm (gptel-make-fsm :handlers gptel-send--handlers)))
               ;; Extract the actual gptel callback for handling responses. By
               ;; default this will generally be `gptel--insert-response' or
               ;; `gptel-curl--stream-insert-response'.
               (info (gptel-fsm-info fsm))
               (fsm-callback (plist-get info :callback))
               (wrapped-callback
                (lambda (response &rest rest)
                  "Invoke the user-provided callback after the request is aborted.
Intercept tool results for patch capture. Then pass arguments through to
the original callback."
                  (when (eq response 'abort)
                    (funcall request-callback 'abort fsm))
                  ;; Always pass through to original callback for normal display
                  (apply fsm-callback response rest))))
          (setf (gptel-fsm-info fsm) (plist-put info :callback wrapped-callback))
          (setf (gptel-fsm-info fsm) (plist-put info :mevedel-request-callback request-callback))
          fsm)))))

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
      ;; TOOL can advance out; preview-mode's canceller invokes
      ;; `mevedel-preview-mode-dismiss-all'.  Draining before the
      ;; `gptel-abort' loop is load-bearing -- follow-up HTTP
      ;; requests launched by `aborted' callbacks land in
      ;; `gptel--request-alist' and get torn down in phase 2.
      (with-current-buffer chat-buffer
        (when (bound-and-true-p mevedel--current-request)
          (mevedel-request-drain-cancellers mevedel--current-request))
        ;; flush any queued permission entries with 'aborted
        ;; so callbacks fire and the FSMs they belong to can unwind.
        ;; Run after the canceller drain so canceller-driven entries
        ;; have a chance to settle first.
        (when (fboundp 'mevedel-permission-queue-abort-all)
          (mevedel-permission-queue-abort-all))
        (when (fboundp 'mevedel-plan-approval-abort)
          (mevedel-plan-approval-abort)))
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
                   (mevedel-session-workspace mevedel--session))
          (require 'mevedel-session-persistence)
          (condition-case err
              (mevedel-session-persistence-save mevedel--session chat-buffer)
            (error
             (display-warning
              'mevedel
              (format "Could not save session after abort: %S" err)
              :warning))))))))

(defun mevedel--main-fsm-on-error (fsm)
  "Cleanup hook: when FSM is the main agent's and it errored, terminate.

Recoverable failures belong to sub-agents -- the main turn's history
is the load-bearing transcript.  When the main FSM enters ERRS, the
session can no longer continue from the same conversation state, so
abort all pending sub-agents, drain queued permission prompts, and
clear ephemeral UI state.

A no-op for sub-agent FSMs (their buffers carry
`mevedel--agent-invocation' and have their own ERRS handler in
`mevedel-agent-exec--handlers')."
  (when (and fsm (fboundp 'gptel-fsm-info))
    (let* ((info (gptel-fsm-info fsm))
           (buf (and (listp info) (plist-get info :buffer))))
      (when (and buf
                 (buffer-live-p buf)
                 (not (buffer-local-value 'mevedel--agent-invocation buf))
                 (buffer-local-value 'mevedel--session buf))
        (condition-case err
            (mevedel-abort buf)
          (error
           (display-warning
            'mevedel
            (format "main-fsm-on-error cleanup failed: %S" err)
            :warning)))))))

;;
;;; Goal implementation

(defvar-local mevedel--implementation-permission-mode-restore nil
  "Wrapped permission mode to restore after Goal implementation.")

(defun mevedel--implementation-permission-mode-apply (mode)
  "Temporarily apply implementation permission MODE for this request."
  (when (and (memq mode '(ask auto full-auto))
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
    (prompt &optional display-text prompt-submission)
  "Insert and send generated PROMPT through the canonical request path.
DISPLAY-TEXT is shown in the view instead of PROMPT.  PROMPT-SUBMISSION owns
accepted hook context until the turn is inserted."
  (when prompt-submission
    (require 'mevedel-prompt-submission))
  (let* ((hook-context
          (and prompt-submission
               (mevedel-prompt-submission-context prompt-submission)))
         (stored-prompt
          (if hook-context
              (concat prompt "\n\n" hook-context)
            prompt)))
    (mevedel--insert-local-user-turn
     stored-prompt display-text nil hook-context)
    (when prompt-submission
      (mevedel-prompt-submission-commit prompt-submission))
    (mevedel--gptel-send-request
     (and hook-context stored-prompt))))

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
  :prompt-submission - Accepted prompt transaction."
  (require 'mevedel-utilities)
  (let* ((permission-mode (plist-get action-plist :permission-mode))
         (display-text (or (plist-get action-plist :display-text)
                           "Implement accepted plan"))
         (prompt-submission (plist-get action-plist :prompt-submission))
         (prompt (and prompt-submission
                      (mevedel-prompt-submission-input prompt-submission))))
    (unless prompt
      (error "Implementation requires an accepted prompt submission"))
    (condition-case err
        (progn
          (mevedel--implementation-permission-mode-apply permission-mode)
          ;; Close any unclosed fenced code blocks (e.g., ``` reasoning)
          (mevedel--close-unclosed-blocks)
          (mevedel--submit-generated-turn
           prompt display-text prompt-submission))
      (error
       (mevedel--implementation-permission-mode-restore)
       (signal (car err) (cdr err))))))

(provide 'mevedel-chat)

;;; mevedel-chat.el ends here
