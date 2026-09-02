;;; mevedel-view-composer.el -- View composer and send orchestration -*- lexical-binding: t -*-

;;; Commentary:

;; Owns editable composer geometry, prompt submission, root dispatch, and
;; fork/send coordination with the authoritative gptel data buffer.

;;; Code:

(eval-when-compile
  (require 'cl-lib))

;; The durable-transaction macro must expand for interpreted loads too,
;; so this is a load-time dependency rather than a compile-time one.
(require 'mevedel-session-durability)
(require 'mevedel-mention-bindings)
(require 'mevedel-overlay-ui)
(require 'mevedel-pending-inputs)
(require 'mevedel-permission-mode)
(require 'mevedel-skills-input)
(require 'mevedel-skills-ui)

;; `browse-url'
(declare-function browse-url "browse-url" (url &optional new-window))

;; `cl-seq'
(declare-function cl-find-if "cl-seq" (cl-pred cl-list &rest cl-keys))

;; `gptel'
(declare-function gptel--update-status "ext:gptel"
		  (msg &optional face))
(declare-function gptel-backend-name "ext:gptel-request" (cl-x) t)
(declare-function gptel-fsm-info "ext:gptel-request" (cl-x) t)
(declare-function gptel-fsm-state "ext:gptel-request" (cl-x) t)
(declare-function gptel-send "ext:gptel" (&optional arg))
(defvar gptel-backend)
(defvar gptel-prompt-prefix-alist)
(defvar gptel-response-separator)

;; `mevedel-agents'
(declare-function mevedel-agent-invocation-p "mevedel-agents" (cl-x))

;; `mevedel-chat'
(declare-function mevedel-abort "mevedel-chat" (&optional buf))
(defvar mevedel--pending-model-input)

;; `mevedel-collaboration'
(declare-function mevedel-collaboration--safe-accepted-prompt
                  "mevedel-collaboration" (data-buffer))
(autoload 'mevedel-collaboration--safe-accepted-prompt
  "mevedel-collaboration")

;; `mevedel-compact-run'
(defvar mevedel-compact-run-in-flight nil)

;; `mevedel-directive'
(declare-function mevedel-directive-actions "mevedel-directive" (directive))

;; `mevedel-directive-frame'
(declare-function mevedel-directive-frame-display
                  "mevedel-directive-frame"
                  (directive view-buffer &optional focus))
(autoload 'mevedel-directive-frame-display "mevedel-directive-frame")

;; `mevedel-directive-plan'
(declare-function mevedel-directive-plan-continue
                  "mevedel-directive-plan" (directive input))
(autoload 'mevedel-directive-plan-continue "mevedel-directive-plan")

;; `mevedel-directive-request'
(declare-function mevedel--directive-session-buffer
                  "mevedel-directive-request" (directive workspace))
(declare-function mevedel--discuss-directive-turn
                  "mevedel-directive-request"
                  (directive message &optional attempt-index callback))
(declare-function mevedel--request-directive-changes
                  "mevedel-directive-request" (directive feedback &optional callback))
(declare-function mevedel--retry-directive
                  "mevedel-directive-request" (directive guidance &optional callback))

;; `mevedel-directive-source'
(declare-function mevedel--directive-record
                  "mevedel-directive-source" (directive))

;; `mevedel-goal'
(declare-function mevedel-goal-start "mevedel-goal"
		  (objective &optional prompt-submission))
(autoload 'mevedel-goal-start "mevedel-goal")

;; `mevedel-hooks'
(declare-function mevedel-hooks-additional-context-string
		  "mevedel-hooks" (decision &optional event))
(declare-function mevedel-hooks-event-plist "mevedel-hooks"
		  (event &optional session workspace &rest extra))
(declare-function mevedel-hooks-format-context "mevedel-hooks"
		  (entries))
(declare-function mevedel-hooks-record-session-context "mevedel-hooks"
		  (session decision &optional event))
(declare-function mevedel-hooks-run-event "mevedel-hooks"
		  (event event-plist callback &optional session
			 workspace request invocation))
(declare-function mevedel-hooks-sanitize-final-decision
                  "mevedel-hooks" (event decision))
(autoload 'mevedel-hooks-event-plist "mevedel-hooks")

;; `mevedel-instruction-registry'
(declare-function mevedel--instruction-buffer-workspace
                  "mevedel-instruction-registry" (buffer))

;; `mevedel-mention-bindings'
(declare-function mevedel-mention-bindings-copy-text
		  "mevedel-mention-bindings" (text))
(declare-function mevedel-mention-bindings-invalidate-edit
		  "mevedel-mention-bindings"
		  (start end minimum maximum))
(declare-function mevedel-mention-bindings-ranges
		  "mevedel-mention-bindings" (text))
(declare-function mevedel-mention-bindings-set
		  "mevedel-mention-bindings"
		  (start end binding &optional object))

;; `mevedel-mentions'
(declare-function mevedel-mentions-expand-user-input
		  "mevedel-mentions" (text session))
(declare-function mevedel-mentions-file-token "mevedel-mentions"
		  (path))
(declare-function mevedel-mentions-install "mevedel-mentions" nil)
(declare-function mevedel-mentions-prepare-user-input
		  "mevedel-mentions" (text &optional session))
(autoload 'mevedel-mentions-install "mevedel-mentions")
(defvar mevedel-mentions-agent-enabled-p)

;; `mevedel-menu'
(declare-function mevedel-menu "mevedel-menu" nil)

;; `mevedel-overlay-ui'
(declare-function mevedel-overlay-ui-directive-action-label
                  "mevedel-overlay-ui" (action))

;; `mevedel-overlays'
(declare-function mevedel--directive-action-context
                  "mevedel-overlays" (record workspace))
(declare-function mevedel--topmost-instruction
                  "mevedel-overlays" (instruction &optional of-type pred))
(autoload 'mevedel--topmost-instruction "mevedel-overlays")

;; `mevedel-pending-inputs'
(declare-function mevedel-view--queue-prepared-steering
                  "mevedel-pending-inputs" (submission request))
(defvar mevedel-view--pending-guest-attribution)
(defvar mevedel-view--pending-input-edit)

;; `mevedel-permission-mode'
(declare-function mevedel-permission-mode-effective
                  "mevedel-permission-mode"
                  (&optional session data-buffer surface-buffer))
(declare-function mevedel-permission-mode-label "mevedel-permission-mode"
                  (&optional mode))
(declare-function mevedel-permission-mode-transition
                  "mevedel-permission-mode" (mode))
(defvar mevedel-permission-mode)

;; `mevedel-plan-handoff'
(declare-function mevedel-plan-handoff-reserved-goal-id
		  "mevedel-plan-handoff" (&optional session))
(autoload 'mevedel-plan-handoff-reserved-goal-id "mevedel-plan-handoff")

;; `mevedel-plan-mode'
(declare-function mevedel-plan-mode--invalidate-proposal
		  "mevedel-plan-mode" (&optional session))
(declare-function mevedel-plan-mode-enter "mevedel-plan-mode"
                  (&optional session))
(declare-function mevedel-plan-mode-exit "mevedel-plan-mode"
                  (&optional session))
(autoload 'mevedel-plan-mode-enter "mevedel-plan-mode")
(autoload 'mevedel-plan-mode-exit "mevedel-plan-mode")

;; `mevedel-prompt-submission'
(declare-function mevedel-prompt-submission-accept
		  "mevedel-prompt-submission"
		  (submission input context audits context-entries))
(declare-function mevedel-prompt-submission-audits
		  "mevedel-prompt-submission" (cl-x) t)
(declare-function mevedel-prompt-submission-cancel
		  "mevedel-prompt-submission" (submission))
(declare-function mevedel-prompt-submission-commit
		  "mevedel-prompt-submission" (submission))
(declare-function mevedel-prompt-submission-context
		  "mevedel-prompt-submission" (cl-x) t)
(declare-function mevedel-prompt-submission-create
		  "mevedel-prompt-submission" (&rest args))
(declare-function mevedel-prompt-submission-display-text
		  "mevedel-prompt-submission" (cl-x) t)
(declare-function mevedel-prompt-submission-input
		  "mevedel-prompt-submission" (cl-x) t)
(declare-function mevedel-prompt-submission-outcome
		  "mevedel-prompt-submission" (cl-x) t)
(declare-function mevedel-prompt-submission-reserve
		  "mevedel-prompt-submission" (submission))
(declare-function mevedel-prompt-submission-restore
		  "mevedel-prompt-submission" (submission))
(declare-function mevedel-prompt-submission-set-outcome
		  "mevedel-prompt-submission" (submission outcome))
(autoload 'mevedel-prompt-submission-cancel "mevedel-prompt-submission")
(autoload 'mevedel-prompt-submission-create "mevedel-prompt-submission")

;; `mevedel-resource-capf'
(declare-function mevedel-resource-capf "mevedel-resource-capf" ())
(autoload 'mevedel-resource-capf "mevedel-resource-capf")

;; `mevedel-review'
(declare-function mevedel-review-command-skill-p "mevedel-review"
		  (skill))
(declare-function mevedel-review-mark-command-outcome
		  "mevedel-review" (outcome))
(declare-function mevedel-review-transform-outcome "mevedel-review"
		  (skill-name outcome))

;; `mevedel-session-artifacts'
(declare-function mevedel-session-artifacts-assert-new-mutation-authority
                  "mevedel-session-artifacts" (session))
(autoload 'mevedel-session-artifacts-assert-new-mutation-authority
  "mevedel-session-artifacts")

;; `mevedel-session-fork'
(declare-function mevedel-session-fork-conversation-fork
                  "mevedel-session-fork" (buffer target))
(declare-function mevedel-session-fork-retarget-worktree-path
                  "mevedel-session-fork" (session path))
(declare-function mevedel-session-fork-worktree-fork
                  "mevedel-session-fork" (buffer target))
(autoload 'mevedel-session-fork-conversation-fork "mevedel-session-fork")
(autoload 'mevedel-session-fork-retarget-worktree-path
  "mevedel-session-fork")
(autoload 'mevedel-session-fork-worktree-fork "mevedel-session-fork")

;; `mevedel-session-persistence'
(defvar mevedel-session--read-only-mode)

;; `mevedel-session-rewind'
(declare-function mevedel-session-rewind-assert-stable-source
                  "mevedel-session-rewind"
                  (session buffer operation))
(autoload 'mevedel-session-rewind-assert-stable-source
  "mevedel-session-rewind")

;; `mevedel-side-conversation'
(declare-function mevedel-side-conversation-send
                  "mevedel-side-conversation" ())
(autoload 'mevedel-side-conversation-send "mevedel-side-conversation")

;; `mevedel-skills-core'
(declare-function mevedel-session-get-skill "mevedel-skills-core"
		  (session name))
(declare-function mevedel-skill-name "mevedel-skills-core" (cl-x) t)
(declare-function mevedel-skill-user-invocable-p "mevedel-skills-core"
		  (cl-x) t)

;; `mevedel-skills-input'
(declare-function mevedel-skills-input-insert-fork-result
                  "mevedel-skills-input" (outcome))
(declare-function mevedel-skills-input-parse-skill-line
                  "mevedel-skills-input" (text))
(declare-function mevedel-skills-input-prepare-user-input
                  "mevedel-skills-input" (text session))
(declare-function mevedel-skills-input-refresh-bound-input
                  "mevedel-skills-input" (text session))

;; `mevedel-skills-invoke'
(declare-function mevedel-skills-commit-invoked-records
		  "mevedel-skills-invoke" (session records))
(declare-function mevedel-skills-dispatch-prepared-fork
		  "mevedel-skills-invoke" t t)

;; `mevedel-skills-plan'
(declare-function mevedel-skill-invocation-plan-fork-p
		  "mevedel-skills-plan" (cl-x) t)
(declare-function mevedel-skill-invocation-plan-occurrences
		  "mevedel-skills-plan" (cl-x) t)
(declare-function mevedel-skills-plan-prepare "mevedel-skills-plan"
		  (plan callback &optional cancelled-p))
(declare-function mevedel-skills-plan-render-data
		  "mevedel-skills-plan" (plan prepared))
(declare-function mevedel-skills-plan-user-input "mevedel-skills-plan"
		  (text session))
(autoload 'mevedel-skills-plan-user-input "mevedel-skills-plan")

;; `mevedel-skills-ui'
(declare-function mevedel-skills-install-font-lock "mevedel-skills-ui"
                  (&optional origin-function))
(declare-function mevedel-skills-local-command-active-request-p
		  "mevedel-skills-ui" (name args))
(declare-function mevedel-skills-parse-slash-line "mevedel-skills-ui"
		  (text))
(declare-function mevedel-skills-remaining-argument-hint
		  "mevedel-skills-ui" (skill arguments))
(declare-function mevedel-skills-slash-capf "mevedel-skills-ui"
		  (buffer session local-commands &optional input-start))
(declare-function mevedel-skills-user-visible-skills
		  "mevedel-skills-ui" (session &optional inline-only))
(autoload 'mevedel-skills-install-font-lock "mevedel-skills-ui")
(autoload 'mevedel-skills-user-visible-skills "mevedel-skills-ui")
(defvar mevedel-slash-commands)

;; `mevedel-structs'
(declare-function mevedel-directive-attempts "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-id "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-p "mevedel-structs" (cl-x))
(declare-function mevedel-directive-plan "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-request "mevedel-structs" (cl-x) t)
(declare-function mevedel-goal-id "mevedel-structs" (cl-x) t)
(declare-function mevedel-goal-status "mevedel-structs" (cl-x) t)
(declare-function mevedel-request-fsm "mevedel-structs" (cl-x) t)
(declare-function mevedel-request-id "mevedel-structs" (cl-x) t)
(declare-function mevedel-session--set-dropped-file-grants
                  "mevedel-structs" (session paths))
(declare-function mevedel-session-clear-dropped-file-grants
		  "mevedel-structs" (session))
(declare-function mevedel-session-directive-planning
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-dropped-file-grants
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-forked-from-fork-point-id
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-goal "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-hook-context-pending
		  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-pending-follow-ups
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-pending-plan-approval
		  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-permission-mode "mevedel-structs"
		  (cl-x) t)
(declare-function mevedel-session-plan-metadata "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-plan-mode "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-turn-count "mevedel-structs" (cl-x)
		  t)
(declare-function mevedel-session-workspace "mevedel-structs" (cl-x) t)
(declare-function mevedel-workspace-directives "mevedel-structs" (cl-x) t)
(defvar mevedel--agent-invocation nil)
(defvar mevedel--current-directive-uuid)
(defvar mevedel--current-request)
(defvar mevedel--data-buffer)
(defvar mevedel--session)
(defvar mevedel--view-buffer)
(defvar mevedel--workspace)

;; `mevedel-tool-render-data'
(declare-function mevedel-tool-render-data-blocks
                  "mevedel-tool-render-data" (string))
(autoload 'mevedel-tool-render-data-blocks "mevedel-tool-render-data")

;; `mevedel-transcript'
(declare-function mevedel-transcript-prompt-transform-start
		  "mevedel-transcript" nil)
(autoload 'mevedel-transcript-prompt-transform-start "mevedel-transcript")

;; `mevedel-transcript-audit'
(declare-function mevedel--format-hook-audit-record
		  "mevedel-transcript-audit" (record))
(autoload 'mevedel--format-hook-audit-record "mevedel-transcript-audit")

;; `mevedel-turn'
(declare-function mevedel-request-assert-target-ready
                  "mevedel-turn" (session))
(declare-function mevedel-request-begin "mevedel-turn"
                  (session &optional directive-uuid))
(declare-function mevedel-request-end
                  "mevedel-turn" (&optional abort-plan-approval))
(declare-function mevedel-turn-busy-p "mevedel-turn" (&optional buffer))
(autoload 'mevedel-request-assert-target-ready "mevedel-turn")
(autoload 'mevedel-request-begin "mevedel-turn")
(autoload 'mevedel-request-end "mevedel-turn")
(autoload 'mevedel-turn-busy-p "mevedel-turn")

;; `mevedel-utilities'
(declare-function mevedel--clear-user-turn-gptel-properties
		  "mevedel-utilities" (start end))
(declare-function mevedel--normalize-message-text "mevedel-utilities"
		  (text))
(autoload 'mevedel--clear-user-turn-gptel-properties "mevedel-utilities")
(autoload 'mevedel--normalize-message-text "mevedel-utilities")

;; `mevedel-view'
(declare-function mevedel-view--abort-data-buffer
                  "mevedel-view" (data-buffer))
(autoload 'mevedel-view--abort-data-buffer "mevedel-view")
(defvar mevedel-view--interaction-marker)
(defvar mevedel-view--side-conversation-p)
(defvar mevedel-view--status-marker)

;; `mevedel-view-agent'
(defvar mevedel-view--agent-transcript-p)

;; `mevedel-view-audit'
(declare-function mevedel-view--prompt-rewrite-audit-record
		  "mevedel-view-audit"
		  (event original submitted decision))

;; `mevedel-view-disclosure'
(declare-function mevedel-view-disclosure-source-range
                  "mevedel-view-disclosure" (data-buffer start end))
(declare-function mevedel-view-disclosure-source-start
                  "mevedel-view-disclosure" (source))

;; `mevedel-view-history'
(declare-function mevedel-view-history-add "mevedel-view-history"
		  (input))
(declare-function mevedel-view-history-beginning-of-line
		  "mevedel-view-history" (&optional arg))
(declare-function mevedel-view-history-browse "mevedel-view-history" ())
(declare-function mevedel-view-history-clear-input
		  "mevedel-view-history" ())
(declare-function mevedel-view-history-load "mevedel-view-history"
		  (&optional session))
(declare-function mevedel-view-history-next "mevedel-view-history" ())
(declare-function mevedel-view-history-previous "mevedel-view-history" ())
(declare-function mevedel-view-history-search "mevedel-view-history" ())
(autoload 'mevedel-view-history-load "mevedel-view-history")

;; `mevedel-view-input-files'
(declare-function mevedel-view--activate-dropped-file-grants
                  "mevedel-view-input-files" (paths session))
(declare-function mevedel-view--install-dnd
                  "mevedel-view-input-files" ())
(declare-function mevedel-view--mentioned-file-paths
                  "mevedel-view-input-files" (input))
(declare-function mevedel-view--pop-dropped-file-grants-for-input
                  "mevedel-view-input-files" (input session))
(autoload 'mevedel-view--install-dnd "mevedel-view-input-files")

;; `mevedel-view-interaction'
(declare-function mevedel-view--interaction-rebuild
		  "mevedel-view-interaction" nil)
(declare-function mevedel-view--interaction-register
		  "mevedel-view-interaction" (descriptor))
(declare-function mevedel-view--interaction-unregister
		  "mevedel-view-interaction" (id))

;; `mevedel-view-render'
(declare-function mevedel-view--append-request-summary
                  "mevedel-view-render" (data-buf search-start &optional extra))
(declare-function mevedel-view--full-rerender "mevedel-view-render" ())
(declare-function mevedel-view--history-insertion-marker
                  "mevedel-view-render" ())
(declare-function mevedel-view--hook-context-events-from-text
                  "mevedel-view-render" (text))
(declare-function mevedel-view--inline-skill-prompt-summary-body
                  "mevedel-view-render" (render-data))
(declare-function mevedel-view--insert-rendered-tool
                  "mevedel-view-render" (rendering source))
(declare-function mevedel-view--insert-user-message
                  "mevedel-view-render"
                  (text &optional kind hook-context prompt-summary-body
                        prompt-summary-source hook-audits guest-name))
(declare-function mevedel-view-fork-point-at-point
                  "mevedel-view-render" ())
(declare-function mevedel-view-reset-agent-ephemeral-state
                  "mevedel-view-render" (&optional data-buf))
(autoload 'mevedel-view--append-request-summary "mevedel-view-render")
(defvar mevedel-view--display-map)

;; `mevedel-view-segments'
(declare-function mevedel-view-historical-segment-p
                  "mevedel-view-segments" ())
(autoload 'mevedel-view-historical-segment-p "mevedel-view-segments")

;; `mevedel-view-stream'
(declare-function mevedel-view--stop-request-progress
                  "mevedel-view-stream" ())
(declare-function mevedel-view-stream-begin-turn
                  "mevedel-view-stream"
                  (view-turn-start data-turn-start &optional no-spinner))
(declare-function mevedel-view-stream-stop "mevedel-view-stream" ())
(defvar mevedel-view--data-turn-start)
(defvar mevedel-view--in-flight-turn-start)

;; `mevedel-view-zone'
(declare-function mevedel-view-zone-bounds-at "mevedel-view-zone"
                  (&optional position))
(declare-function mevedel-view-zone-fragment-bounds "mevedel-view-zone"
                  (zone id))

;; `mevedel-worktree'
(declare-function mevedel-worktree-fork-preflight
		  "mevedel-worktree" (session))
(declare-function mevedel-worktree-fork-reservation
		  "mevedel-worktree" (session &optional preflight))
(autoload 'mevedel-worktree-fork-preflight "mevedel-worktree")
(autoload 'mevedel-worktree-fork-reservation "mevedel-worktree")

;;
;;; Input prompt

(defconst mevedel-view--input-prompt "> "
  "Read-only prefix rendered at the start of the input zone.")

(defface mevedel-view-directive-scope
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face for directive-scoped composer chrome."
  :group 'mevedel)

(defface mevedel-view-directive-scope-banner
  '((((background light))
     :inherit mevedel-view-directive-scope :background "#eef3fe" :extend t)
    (((background dark))
     :inherit mevedel-view-directive-scope :background "#0d1b3d" :extend t)
    (t :inherit mevedel-view-directive-scope :extend t))
  "Face for the full-width directive-scope composer banner line."
  :group 'mevedel)

(defvar-local mevedel-view--composer-scope nil
  "Current directive composer scope, or nil for ordinary chat.")

(defvar-local mevedel-view--composer-drafts nil
  "Draft snapshots keyed by chat or directive composer scope.")

(defun mevedel-view-composer-scope-label (&optional scope)
  "Return a concise label for SCOPE or the current composer scope."
  (when-let* ((scope (or scope mevedel-view--composer-scope)))
    (format "directive %s/%s"
            (substring (plist-get scope :directive-id)
                       0 (min 8 (length (plist-get scope :directive-id))))
            (symbol-name (plist-get scope :action)))))

(defun mevedel-view--effective-permission-mode ()
  "Return the permission mode to apply to the current view buffer."
  (mevedel-permission-mode-effective
   (and (boundp 'mevedel--session) mevedel--session)
   (and (boundp 'mevedel--data-buffer)
        (buffer-live-p mevedel--data-buffer)
        mevedel--data-buffer)
   (current-buffer)))

(defun mevedel-view--permission-mode-display (mode)
  "Return (LABEL FACE) for permission MODE."
  (list
   (mevedel-permission-mode-label mode)
   (pcase mode
     ('edits 'mevedel-view-permission-mode-edits)
     ('full-auto 'mevedel-view-permission-mode-full-auto)
     (_ 'mevedel-view-permission-mode-ask))))

(defun mevedel-view--plan-mode-p ()
  "Return non-nil when the current view's session is in Plan mode."
  (and (boundp 'mevedel--session)
       mevedel--session
       (mevedel-session-plan-mode mevedel--session)))

(defconst mevedel-view--permission-mode-cycle
  '(ask edits full-auto)
  "Permission states cycled by `mevedel-view-cycle-permission-mode'.")

(defun mevedel-view--next-permission-mode (&optional mode)
  "Return the permission mode after MODE in the view cycle.
Nil and unknown modes are treated as `ask'."
  (let* ((current (if (memq mode mevedel-view--permission-mode-cycle)
                      mode
                    'ask))
         (tail (cdr (memq current mevedel-view--permission-mode-cycle))))
    (or (car tail)
        (car mevedel-view--permission-mode-cycle))))

(defun mevedel-view--input-prompt-string (&optional mode)
  "Return the read-only input prompt string for permission MODE.
The prompt starts with a blank separator line so status and interaction
rows remain visually distinct from the editable composer."
  (let ((mode (or mode (mevedel-view--effective-permission-mode))))
    (if mevedel-view--composer-scope
        (let* ((record (plist-get mevedel-view--composer-scope :record))
               (action (plist-get mevedel-view--composer-scope :action))
               (action-label
                (mevedel-overlay-ui-directive-action-label
                 (if (eq action 'discuss)
                     (cond
                      ((plist-get mevedel-view--composer-scope :attempt-index)
                       'discuss-result)
                      ((and record
                            (memq 'continue-discussion
                                  (mevedel-directive-actions record)))
                       'continue-discussion)
                      (t 'discuss))
                   action)))
               (request
                (and record
                     (mevedel--truncate-display
                      (replace-regexp-in-string
                       "[ \t\n\r]+" " "
                      (mevedel-directive-request record))
                      80 "…")))
               (request-label
                (or request
                    (plist-get mevedel-view--composer-scope :directive-id)))
               (permission-display
                (mevedel-view--permission-mode-display mode))
               (permission-label (car permission-display))
               (permission-face (cadr permission-display)))
          (concat
           "\n"
           (propertize (format "◆ %s · %s\n" action-label request-label)
                       'font-lock-face 'mevedel-view-directive-scope-banner)
           (propertize "  isolated from chat · " 'font-lock-face 'shadow)
           (propertize permission-label 'font-lock-face permission-face)
           (when (mevedel-view--plan-mode-p)
             (concat
              (propertize " · " 'font-lock-face 'shadow)
              (propertize "Plan paused"
                          'font-lock-face 'mevedel-view-plan-mode)))
           (propertize " · C-c C-k Back" 'font-lock-face 'shadow)
           ;; The frame's own keys are only reachable while it is showing.
           (when (bound-and-true-p mevedel-directive-frame-mode)
             (propertize " · C-c C-f Filter · C-c C-z Close"
                         'font-lock-face 'shadow))
           (propertize "\n" 'font-lock-face 'shadow)
           (propertize "◆ > "
                       'font-lock-face 'mevedel-view-directive-scope)))
      (if mevedel-view--pending-input-edit
          (propertize
           (format "\n[Editing %s] %s"
                   (plist-get mevedel-view--pending-input-edit :label)
                   mevedel-view--input-prompt)
           'font-lock-face 'mevedel-view-input-prompt)
	(if (mevedel-view--plan-mode-p)
            (pcase-let* ((`(,label ,_) (mevedel-view--permission-mode-display mode))
			 (text (format "\n[Plan · %s] %s"
                                       label mevedel-view--input-prompt)))
              (add-text-properties
               0 (length text)
               '(font-lock-face mevedel-view-input-prompt)
               text)
              (add-text-properties
               2 (+ 9 (length label))
               '(font-lock-face mevedel-view-plan-mode)
               text)
              text)
	  (if (eq mode 'ask)
              (propertize (concat "\n" mevedel-view--input-prompt)
			  'font-lock-face 'mevedel-view-input-prompt)
            (pcase-let* ((`(,label ,face)
			  (mevedel-view--permission-mode-display mode))
			 (text (format "\n[%s] %s"
                                       label mevedel-view--input-prompt))
			 (label-start 2)
			 (label-end (+ label-start (length label))))
              (add-text-properties
               0 (length text)
               '(font-lock-face mevedel-view-input-prompt)
               text)
              (add-text-properties
               label-start label-end
               `(font-lock-face ,face)
               text)
              text)))))))


;;
;;; Composer state and redraw preservation

(defvar-local mevedel-view--input-marker nil
  "Marker separating request progress from the input zone.
Everything above this marker is read-only history/status/interaction
chrome; everything at or below it belongs to the input zone.  The input
zone starts with the read-only prompt prefix, followed by the editable
composer body.")

(defvar-local mevedel-view--armed-session-fork nil
  "Stable fork-point target armed for the next model-bound submission.")

(defvar-local mevedel-view--armed-session-fork-return-point nil
  "View position to restore when cancelling a historical session fork.")

(defvar-local mevedel-view--historical-composer-overlay nil
  "Overlay hiding the live composer during archived segment inspection.")

(defun mevedel-view-composer-session-fork-armed-p ()
  "Return non-nil when this view has an armed historical session fork."
  (and mevedel-view--armed-session-fork t))

(defun mevedel-view-composer-set-historical-visible (visible)
  "Show the live composer when VISIBLE, otherwise hide and lock it."
  (when (overlayp mevedel-view--historical-composer-overlay)
    (delete-overlay mevedel-view--historical-composer-overlay)
    (setq mevedel-view--historical-composer-overlay nil))
  (if visible
      (setq buffer-read-only nil)
    (setq mevedel-view--historical-composer-overlay
          (make-overlay
           (mevedel-view--input-marker-position) (point-max)
           (current-buffer) t t))
    (overlay-put mevedel-view--historical-composer-overlay
                 'invisible 'mevedel-view-historical-composer)
    (setq buffer-read-only t)))

(defvar-keymap mevedel-view--armed-session-fork-map
  :doc "Keymap for the armed session-fork interaction row."
  "RET" #'mevedel-view-cancel-session-fork
  "<mouse-1>" #'mevedel-view-cancel-session-fork)

(defun mevedel-view-cancel-session-fork (&optional _event)
  "Disarm the pending session fork without changing the composer draft."
  (interactive)
  (when mevedel-view--armed-session-fork
    (setq mevedel-view--armed-session-fork nil)
    (mevedel-view--interaction-unregister 'armed-session-fork)
    (when (mevedel-view-historical-segment-p)
      (mevedel-view-composer-set-historical-visible nil)
      (when mevedel-view--armed-session-fork-return-point
        (goto-char
         (min mevedel-view--armed-session-fork-return-point
              (mevedel-view--input-marker-position)))))
    (setq mevedel-view--armed-session-fork-return-point nil)
    t))

(defun mevedel-view-cancel-composer-state ()
  "Cancel the active composer mode, including directive scope."
  (interactive)
  (cond
   (mevedel-view--armed-session-fork
    (mevedel-view-cancel-session-fork))
   (mevedel-view--pending-input-edit
    (mevedel-pending-inputs-cancel-edit))
   (mevedel-view--composer-scope
    (mevedel-view-back-to-chat))))

(defun mevedel-view--arm-session-fork (fork-type)
  "Arm FORK-TYPE from the settled assistant response at point."
  (mevedel-view--ensure-interactive-chat-view)
  (let* ((target (mevedel-view-fork-point-at-point))
         (session (mevedel-view--session))
         (label (if (eq fork-type 'worktree) "worktree" "conversation"))
         reservation)
    (when (equal
           (plist-get target :fork-point-id)
           (mevedel-session-forked-from-fork-point-id session))
      (user-error
       "Fork the inherited response from Source; switch variants first"))
    (mevedel-session-rewind-assert-stable-source
     session mevedel--data-buffer "forking")
    (when (eq fork-type 'worktree)
      (let ((preflight (mevedel-worktree-fork-preflight session)))
        (setq reservation
              (mevedel-worktree-fork-reservation session preflight))))
    (setq target (copy-sequence target))
    (plist-put target :fork-type fork-type)
    (when reservation
      (plist-put target :worktree-reservation reservation))
    (when (mevedel-view-historical-segment-p)
      (setq mevedel-view--armed-session-fork-return-point (point))
      (mevedel-view-composer-set-historical-visible t))
    (mevedel-view--interaction-register
     (list :kind 'preview
           :id 'armed-session-fork
           :active-work-paused nil
           :body
           (format "Fork %s from Assistant turn %d  [Cancel]"
                   label (plist-get target :cum-turn))
           :keymap mevedel-view--armed-session-fork-map
           :help-echo (format "Cancel %s Fork"
                              (capitalize label))))
    (setq mevedel-view--armed-session-fork target)
    (goto-char (point-max))))

(defun mevedel-view--assert-live-tip (&optional allow-armed-fork)
  "Refuse live-tip actions during historical inspection.
ALLOW-ARMED-FORK permits submission of an already armed session fork."
  (when (and (mevedel-view-historical-segment-p)
             (not (and allow-armed-fork
                       mevedel-view--armed-session-fork)))
    (user-error
     "Viewing historical segment; return to latest or fork from an assistant response")))

(defun mevedel-view-arm-conversation-fork ()
  "Arm a Conversation Fork from the settled assistant response at point."
  (interactive)
  (mevedel-view--arm-session-fork 'conversation))

(defun mevedel-view-arm-worktree-fork ()
  "Arm a Worktree Fork from the settled assistant response at point."
  (interactive)
  (mevedel-view--arm-session-fork 'worktree))

(defvar-keymap mevedel-view--composer-keymap
  :doc "Keymap active over the editable composer body."
  "C-<tab>" #'mevedel-view-toggle-plan-mode
  "C-c RET" #'mevedel-view-send
  "C-c TAB" #'mevedel-view-send-follow-up
  "C-c C-c" #'mevedel-pending-inputs-save-edit
  "C-c C-e" #'mevedel-pending-inputs-open
  "C-c C-k" #'mevedel-view-cancel-composer-state
  "C-c C-l" #'mevedel-view-history-browse
  "C-c C-u" #'mevedel-view-history-clear-input
  "C-y" #'mevedel-view-yank-dwim
  "M-n" #'mevedel-view-history-next
  "M-p" #'mevedel-view-history-previous
  "M-r" #'mevedel-view-history-search
  "<backtab>" #'mevedel-view-cycle-permission-mode
  "S-TAB" #'mevedel-view-cycle-permission-mode)

(defvar-keymap mevedel-view--side-conversation-keymap
  :doc "Keymap active over an ephemeral side-conversation composer."
  "C-c RET" #'mevedel-view-send
  "C-c TAB" #'mevedel-view-send-follow-up
  "C-c C-k" #'mevedel-view-abort
  "C-c C-z" #'mevedel-side-conversation-close
  "C-y" #'mevedel-view-yank-dwim)

(define-key mevedel-view--composer-keymap
            [remap move-beginning-of-line]
            #'mevedel-view-history-beginning-of-line)

(defvar-local mevedel-view--composer-keymap-overlay nil
  "Overlay that gives the editable composer its local keymap.")


(defvar-local mevedel-view--skill-argument-hint-overlay nil
  "Zero-width overlay that displays skill argument guidance in the composer.")

(defvar-local mevedel-view--prompt-hook-pending nil
  "Prompt submission awaiting a `UserPromptSubmit' hook for this view.
This covers the interval before the prompt has been accepted and before
`mevedel--current-request' exists in the data buffer.")

(defvar-local mevedel-view--pending-skill-submission nil
  "Cancellation token for skill-plan preparation before request dispatch.")

(defun mevedel-view--cancel-pending-submission ()
  "Cancel this view's pending hook or skill preparation, if any."
  (when mevedel-view--prompt-hook-pending
    (mevedel-prompt-submission-cancel mevedel-view--prompt-hook-pending)
    (setq mevedel-view--prompt-hook-pending nil))
  (when mevedel-view--pending-skill-submission
    (setf (plist-get mevedel-view--pending-skill-submission :cancelled) t)
    (setq mevedel-view--pending-skill-submission nil)))


(defun mevedel-view--position-in-input-region-p (position)
  "Return non-nil when POSITION is in the editable composer."
  (and (boundp 'mevedel-view--input-marker)
       (markerp mevedel-view--input-marker)
       (marker-buffer mevedel-view--input-marker)
       (not (bound-and-true-p mevedel-view--agent-transcript-p))
       (ignore-errors
         (>= position (mevedel-view--input-start)))))

(defun mevedel-view--point-in-input-region-p ()
  "Return non-nil when point is in the editable composer."
  (mevedel-view--position-in-input-region-p (point)))

(defun mevedel-view--call-preserving-input-point (thunk)
  "Call THUNK, preserving point's offset inside the composer.
Redraws for spinners, agent status, and interaction prompts insert
or delete text above the editable input.  When the user is typing in
the composer, restore point by its input-relative offset after THUNK
finishes."
  (let* ((buffer (current-buffer))
         (preserve-p (mevedel-view--point-in-input-region-p))
         (offset (and preserve-p
                      (- (point) (mevedel-view--input-start))))
         result)
    (unwind-protect
        (setq result (funcall thunk))
      (when (and preserve-p
                 (buffer-live-p buffer))
        (with-current-buffer buffer
          (when (and (markerp mevedel-view--input-marker)
                     (marker-buffer mevedel-view--input-marker))
            (goto-char
             (min (point-max)
                  (+ (mevedel-view--input-start)
                     (max 0 offset))))))))
    result))

(defun mevedel-view--call-preserving-input-text (thunk)
  "Call THUNK without allowing it to mutate editable composer text.
History/status rendering should only change text above
`mevedel-view--input-marker'.  This guard restores the input body if a
late callback accidentally inserts transcript content below the prompt."
  (let* ((preserve-p
          (and (not (bound-and-true-p mevedel-view--agent-transcript-p))
               (markerp mevedel-view--input-marker)
               (marker-buffer mevedel-view--input-marker)))
         (text (and preserve-p
                    (progn
                      (mevedel-mention-bindings-copy-text
                       (buffer-substring
                        (mevedel-view--input-start) (point-max))))))
         result)
    (let ((inhibit-modification-hooks t))
      (setq result (funcall thunk)))
    (when (and preserve-p
               (markerp mevedel-view--input-marker)
               (marker-buffer mevedel-view--input-marker))
      (let* ((start (mevedel-view--input-start))
             (current (mevedel-mention-bindings-copy-text
                       (buffer-substring start (point-max)))))
        (unless (equal-including-properties current text)
          (let ((inhibit-read-only t)
                (inhibit-modification-hooks t))
            (delete-region start (point-max))
            (goto-char start)
            (insert text)))))
    result))

(defun mevedel-view--position-render-anchor (pos)
  "Return a semantic anchor for POS that survives a re-render, or nil.

Redraws delete and re-insert view text, so a raw buffer position saved
across one lands in different content whenever lengths shift above or
around it.  A managed fragment (zone namespace, fragment id, offset
into the fragment) or the transcript source map (data-buffer position
plus offset into its rendered run) identifies the same content after
the redraw.  Composer positions are handled by input offsets and
return nil here."
  (when (and (integer-or-marker-p pos)
             (>= pos (point-min))
             (< pos (point-max))
             (not (mevedel-view--position-in-input-region-p pos)))
    (or
     (when-let* ((bounds (mevedel-view-zone-bounds-at pos)))
       (list 'fragment
             (plist-get bounds :namespace)
             (plist-get bounds :id)
             (- pos (plist-get bounds :start))))
     (when-let* ((source (get-text-property pos 'mevedel-view-source))
                 ((consp source))
                 (data-start (mevedel-view-disclosure-source-start source)))
       (let ((run-start (previous-single-property-change
                         (min (1+ pos) (point-max))
                         'mevedel-view-source nil (point-min)))
             (nth 0)
             (scan (point-min)))
         ;; Several runs can share one source start -- a fold header and
         ;; its body, or a turn whose first segment starts the turn.  The
         ;; ordinal picks the same run back out after the redraw instead
         ;; of the first one that happens to match.
         (while (< scan run-start)
           (let ((next (or (next-single-property-change
                            scan 'mevedel-view-source nil run-start)
                           run-start))
                 (other (get-text-property scan 'mevedel-view-source)))
             (when (and (consp other)
                        (eql (mevedel-view-disclosure-source-start other)
                             data-start))
               (setq nth (1+ nth)))
             (setq scan next)))
         (list 'source data-start nth (- pos run-start)))))))

(defun mevedel-view--render-anchor-position (anchor)
  "Return the buffer position ANCHOR identifies after a redraw, or nil.
ANCHOR comes from `mevedel-view--position-render-anchor'.  The result
is clamped into the re-rendered fragment or source run, so a position
whose content shrank stays inside the same content instead of drifting
into a neighbor."
  (pcase anchor
    (`(fragment ,namespace ,id ,offset)
     (when-let* ((bounds (mevedel-view-zone-fragment-bounds namespace id)))
       (min (max (plist-get bounds :start)
                 (1- (plist-get bounds :end)))
            (+ (plist-get bounds :start) offset))))
    (`(source ,data-start ,nth ,offset)
     (let ((limit (if (and (markerp mevedel-view--input-marker)
                           (marker-buffer mevedel-view--input-marker))
                     (mevedel-view--input-start)
                    (point-max)))
           (pos (point-min))
           (count 0)
           found)
       (while (and (not found) (< pos limit))
         (let ((next (or (next-single-property-change
                          pos 'mevedel-view-source nil limit)
                         limit))
               (source (get-text-property pos 'mevedel-view-source)))
           (when (and (consp source)
                      (eql (mevedel-view-disclosure-source-start source)
                           data-start))
             (if (= count nth)
                 (setq found (min (max pos (1- next)) (+ pos offset)))
               (setq count (1+ count))))
           (setq pos next)))
       found))))

(defun mevedel-view--call-preserving-window-state (thunk)
  "Call THUNK while preserving each displayed window's browsing state.
Preserves those values for every window displaying the current buffer.
Windows already following the bottom continue following new output;
windows browsing older content retain their point and start.

Used to wrap delete-and-re-render operations so the user's scroll
position and caret do not jump back to the edit site on every
progress tick.  Point, window points, and window starts are restored
through semantic render anchors (`mevedel-view--position-render-anchor')
so a position inside re-rendered text returns to the same content
rather than to the same numeric offset; a position whose anchor cannot
be resolved after BODY is quietly clamped to the buffer.  The buffer
mark, active-region state, and selection direction are preserved with
point.  When either endpoint is in the editable composer, preserve it
by offset from `mevedel-view--input-start' so streaming text inserted
above the composer does not strand it in rendered transcript text."
  (let* ((mevedel-view--pww-selected-window (selected-window))
          (mevedel-view--pww-current-buffer (current-buffer))
          (mevedel-view--pww-current-point (point))
          (mevedel-view--pww-current-anchor
           (mevedel-view--position-render-anchor (point)))
          (mevedel-view--pww-current-mark (mark t))
          (mevedel-view--pww-mark-active mark-active)
          (mevedel-view--pww-deactivate-mark deactivate-mark)
          (mevedel-view--pww-current-input-offset
           (and (mevedel-view--point-in-input-region-p)
                (- (point) (mevedel-view--input-start))))
          (mevedel-view--pww-mark-input-offset
           (and mevedel-view--pww-current-mark
                (mevedel-view--position-in-input-region-p
                 mevedel-view--pww-current-mark)
                (- mevedel-view--pww-current-mark
                   (mevedel-view--input-start))))
          (mevedel-view--pww-saved
           (mapcar (lambda (w)
                     (with-current-buffer mevedel-view--pww-current-buffer
                       (let ((wp (window-point w))
                             (ws (window-start w)))
                         (list w
                               wp
                               ws
                               (and (mevedel-view--position-in-input-region-p wp)
                                    (- wp (mevedel-view--input-start)))
                               (= wp (point-max))
                               (mevedel-view--position-render-anchor wp)
                               (mevedel-view--position-render-anchor ws)))))
                   (get-buffer-window-list (current-buffer) nil t))))
     (prog1 (funcall thunk)
       (let ((restored-current-point
              (cond
               ((and mevedel-view--pww-current-input-offset
                     (markerp mevedel-view--input-marker)
                     (marker-buffer mevedel-view--input-marker))
                (+ (mevedel-view--input-start)
                   (max 0 mevedel-view--pww-current-input-offset)))
               ((mevedel-view--render-anchor-position
                 mevedel-view--pww-current-anchor))
               (t mevedel-view--pww-current-point))))
         (goto-char (min (point-max) restored-current-point)))
       (dolist (entry mevedel-view--pww-saved)
         (pcase-let ((`(,w ,wp ,ws ,input-offset ,at-bottom
                        ,wp-anchor ,ws-anchor)
                      entry))
           (when (window-live-p w)
             (let ((restored-point
                    (cond
                     ((and input-offset
                           (markerp mevedel-view--input-marker)
                           (marker-buffer mevedel-view--input-marker))
                      (+ (mevedel-view--input-start)
                         (max 0 input-offset)))
                     ((mevedel-view--render-anchor-position wp-anchor))
                     (t wp))))
               (when restored-point
                 (set-window-point w (min (point-max) restored-point)))
               (when (eq w mevedel-view--pww-selected-window)
                 (goto-char (window-point w))))
             (let ((restored-start
                    (or (mevedel-view--render-anchor-position ws-anchor)
                        ws)))
               (when (and restored-start (<= restored-start (point-max)))
                 (set-window-start w restored-start t)))
             (when (and at-bottom (not input-offset))
               (save-selected-window
                 (select-window w)
                 (goto-char (point-max))
                 (recenter -1))))))
       (when mevedel-view--pww-current-mark
         (set-mark
          (min
           (point-max)
           (if (and mevedel-view--pww-mark-input-offset
                    (markerp mevedel-view--input-marker)
                    (marker-buffer mevedel-view--input-marker))
               (+ (mevedel-view--input-start)
                  (max 0 mevedel-view--pww-mark-input-offset))
             mevedel-view--pww-current-mark)))
         (setq mark-active mevedel-view--pww-mark-active
               deactivate-mark mevedel-view--pww-deactivate-mark)))))

(defmacro mevedel-view--preserving-window-state (&rest body)
  "Execute BODY while preserving point and window positions."
  (declare (indent 0) (debug t))
  `(mevedel-view--call-preserving-window-state
    (lambda () ,@body)))

(defun mevedel-view--call-with-render-boundaries-advancing (thunk)
  "Call THUNK while zone boundary markers advance across insertions."
  (let ((status-type (and (markerp mevedel-view--status-marker)
                          (marker-insertion-type mevedel-view--status-marker)))
        (interaction-type
         (and (markerp mevedel-view--interaction-marker)
              (marker-insertion-type mevedel-view--interaction-marker)))
        (input-type (and (markerp mevedel-view--input-marker)
                         (marker-insertion-type mevedel-view--input-marker))))
    (unwind-protect
        (progn
          (when (markerp mevedel-view--status-marker)
            (set-marker-insertion-type mevedel-view--status-marker t))
          (when (markerp mevedel-view--interaction-marker)
            (set-marker-insertion-type mevedel-view--interaction-marker t))
          (when (markerp mevedel-view--input-marker)
            (set-marker-insertion-type mevedel-view--input-marker t))
          (funcall thunk))
      (when (markerp mevedel-view--status-marker)
        (set-marker-insertion-type mevedel-view--status-marker status-type))
      (when (markerp mevedel-view--interaction-marker)
        (set-marker-insertion-type mevedel-view--interaction-marker
                                   interaction-type))
      (when (markerp mevedel-view--input-marker)
        (set-marker-insertion-type mevedel-view--input-marker input-type)))))

(defmacro mevedel-view--with-render-boundaries-advancing (&rest body)
  "Execute BODY while zone boundary markers advance across insertions."
  (declare (indent 0) (debug t))
  `(mevedel-view--call-with-render-boundaries-advancing
    (lambda () ,@body)))

(defun mevedel-view--call-preserving-user-view-state (thunk)
  "Call THUNK without moving the user's live view cursor.
Async redraws may insert, delete, or reconcile view-owned text while the
user is typing in the composer or browsing transcript history.  Preserve
all displayed windows plus the editable composer text around THUNK."
  (mevedel-view--preserving-window-state
    (mevedel-view--call-preserving-input-text
     (lambda ()
       (mevedel-view--call-preserving-input-point thunk)))))


;;
;;; Initialization

(defun mevedel-view-composer-initialize ()
  "Initialize composer editing support in the current chat view."
  (unless mevedel-view--agent-transcript-p
    (setq mevedel-view--composer-scope nil
          mevedel-view--composer-drafts (make-hash-table :test #'equal))
    (setq-local mevedel-mentions-agent-enabled-p
                (not mevedel-view--side-conversation-p))
    (mevedel-mentions-install)
    (mevedel-view--install-dnd)
    (add-hook 'completion-at-point-functions
              #'mevedel-resource-capf nil t)
    (add-hook 'after-change-functions
              #'mevedel-view--refresh-skill-argument-hint-after-change
              nil t)
    (unless mevedel-view--side-conversation-p
      (mevedel-view-history-load mevedel--session)
      (add-hook 'completion-at-point-functions
                #'mevedel-view-slash-capf nil t)
      ;; Bound the $skill matcher to the draft: it runs per
      ;; refontification, and scanning the transcript prefix cost
      ;; hundreds of milliseconds per keystroke on a long session.
      (mevedel-skills-install-font-lock #'mevedel-view--input-start)
      (add-hook 'post-command-hook
                #'mevedel-view--refresh-skill-argument-hint nil t))
    (add-hook 'kill-buffer-hook
              #'mevedel-view--cancel-pending-submission nil t)
    (setq mevedel-view--composer-keymap-overlay
          (make-overlay
           (mevedel-view--input-start) (point-max)
           (current-buffer) nil t))
    (overlay-put mevedel-view--composer-keymap-overlay
                 'keymap
                 (if mevedel-view--side-conversation-p
                     mevedel-view--side-conversation-keymap
                   mevedel-view--composer-keymap))))

;;
;;; Input forwarding

(defun mevedel-view--ensure-interactive-chat-view ()
  "Signal when the current view buffer is not an editable chat view."
  (when mevedel-view--agent-transcript-p
    (user-error "Agent transcript views are read-only")))

(defun mevedel-view--transcript-gptel-send-blocked (&optional _arg)
  "Block `gptel-send' from transcript inspection views."
  (interactive "P")
  (user-error "Agent transcript views are read-only"))


(defun mevedel-view--begin-external-turn
    (display-text data-turn-start &optional kind hook-context no-spinner)
  "Begin a view turn initiated outside the editable input.

DISPLAY-TEXT is shown as the user-side turn in the view.
DATA-TURN-START is the data-buffer marker where the assistant
response for this turn begins.  KIND may be `directive'.  HOOK-CONTEXT
is model-visible hook context to summarize in the view.  When
NO-SPINNER is non-nil, render only the local user turn."
  (mevedel-view--ensure-interactive-chat-view)
  (let ((turn-start (mevedel-view--insert-user-message
                     display-text kind hook-context)))
    (when (eq kind 'directive)
      (when-let* ((drawer (mevedel-view--external-prompt-drawer
                           data-turn-start)))
        (save-excursion
          (goto-char (mevedel-view--history-insertion-marker))
          (mevedel-view--with-render-boundaries-advancing
            (let ((inhibit-read-only t)
                  (start (point)))
              (mevedel-view--insert-rendered-tool
               (list :header "Prompt"
                     :body (plist-get drawer :body)
                     :body-mode 'markdown-mode
                     :vtype 'prompt-summary
                     :initially-collapsed-p t)
               (cons (plist-get drawer :start)
                     (plist-get drawer :end)))
              (add-text-properties
               start (point)
               `(read-only t
                 keymap ,mevedel-view--display-map
                 front-sticky (read-only keymap)
                 rear-nonsticky (read-only keymap)))
              (setq turn-start (copy-marker (point) nil)))))))
    (mevedel-view-stream-begin-turn
     turn-start data-turn-start no-spinner)))

(defun mevedel-view--external-prompt-drawer (data-turn-start)
  "Return the prompt drawer ending before DATA-TURN-START, if any."
  (when-let* (((markerp data-turn-start))
              (data-buf (marker-buffer data-turn-start))
              ((buffer-live-p data-buf)))
    (with-current-buffer data-buf
      (save-excursion
        (goto-char data-turn-start)
        (when (re-search-backward "^:PROMPT:\n" nil t)
          (let ((drawer-start (match-beginning 0))
                (body-start (match-end 0)))
            (when (re-search-forward "^:END:[ \t]*\n?"
                                     data-turn-start t)
              (list :start drawer-start
                    :end (match-end 0)
                    :body (buffer-substring-no-properties
                           body-start (match-beginning 0))))))))))

(defun mevedel-view--prompt-start-position ()
  "Return the start of the read-only input prompt, or nil."
  (let* ((marker-pos
          (and (markerp mevedel-view--input-marker)
               (eq (marker-buffer mevedel-view--input-marker)
                   (current-buffer))
               (marker-position mevedel-view--input-marker)))
         (pos
          (if (and marker-pos
                   (< marker-pos (point-max))
                   (get-text-property marker-pos 'mevedel-view-prompt))
              marker-pos
            (text-property-any
             (point-min) (point-max) 'mevedel-view-prompt t))))
    (when pos
      (while (and (> pos (point-min))
                  (get-text-property (1- pos) 'mevedel-view-prompt))
        (setq pos (1- pos)))
      pos)))

(defun mevedel-view--input-marker-position ()
  "Return the recovered start position of the input prompt.
When prompt text properties survive but zone markers have drifted past
that prompt into the editable composer, repair the marker ordering so
later prompt refreshes do not operate on the draft body."
  (if-let* ((prompt-start (mevedel-view--prompt-start-position)))
      (progn
        (when (and (markerp mevedel-view--input-marker)
                   (marker-buffer mevedel-view--input-marker)
                   (not (= (marker-position mevedel-view--input-marker)
                           prompt-start)))
          (set-marker mevedel-view--input-marker prompt-start))
        (dolist (marker (list mevedel-view--status-marker
                              mevedel-view--interaction-marker))
          (when (and (markerp marker)
                     (marker-buffer marker)
                     (let ((pos (marker-position marker)))
                       (and pos (> pos prompt-start))))
            (set-marker marker prompt-start)))
        prompt-start)
    (and (markerp mevedel-view--input-marker)
         (marker-position mevedel-view--input-marker))))

(defun mevedel-view-refresh-associated-input-prompt ()
  "Refresh the view prompt associated with the current buffer, if any.
Callable from either half of a view/data pair: prompt-affecting state
changes land in the data buffer, and the prompt lives in the view."
  (let ((view-buf (cond
                   ((and (boundp 'mevedel--view-buffer)
                         (buffer-live-p mevedel--view-buffer))
                    mevedel--view-buffer)
                   ((and (boundp 'mevedel--data-buffer)
                         (buffer-live-p mevedel--data-buffer))
                    (buffer-local-value 'mevedel--view-buffer
                                        mevedel--data-buffer)))))
    (when (buffer-live-p view-buf)
      (with-current-buffer view-buf
        (mevedel-view-refresh-input-prompt)))))

(defun mevedel-view--input-start ()
  "Return the buffer position where the user's editable input begins.
This is the position immediately after the read-only `> ' prompt that
follows `mevedel-view--input-marker'."
  (save-excursion
    (goto-char (or (mevedel-view--input-marker-position)
                   mevedel-view--input-marker))
    (while (get-text-property (point) 'mevedel-view-prompt)
      (forward-char 1))
    (point)))

(defun mevedel-view-refresh-input-prompt ()
  "Refresh the input prompt to reflect the current permission mode."
  (interactive)
  (unless mevedel-view--agent-transcript-p
    (when (and (markerp mevedel-view--input-marker)
               (marker-buffer mevedel-view--input-marker))
      (mevedel-view--call-preserving-input-point
       (lambda ()
         (let* ((start (mevedel-view--input-marker-position))
                (end (mevedel-view--input-start))
                (status-type
                 (and (markerp mevedel-view--status-marker)
                      (marker-insertion-type mevedel-view--status-marker)))
                (interaction-type
                 (and (markerp mevedel-view--interaction-marker)
                      (marker-insertion-type mevedel-view--interaction-marker)))
                (input-type (marker-insertion-type mevedel-view--input-marker)))
           (save-excursion
             (goto-char start)
             (unwind-protect
                 (let ((inhibit-read-only t))
                   (when (markerp mevedel-view--status-marker)
                     (set-marker-insertion-type
                      mevedel-view--status-marker nil))
                   (when (markerp mevedel-view--interaction-marker)
                     (set-marker-insertion-type
                      mevedel-view--interaction-marker nil))
                   (set-marker-insertion-type mevedel-view--input-marker nil)
                   (delete-region start end)
                   (insert (mevedel-view--input-prompt-string))
                   (add-text-properties
                    start (point)
                    `(read-only t
                      mevedel-view-prompt t
                      front-sticky (read-only mevedel-view-prompt)
                      rear-nonsticky
                      (read-only mevedel-view-prompt font-lock-face))))
               (when (markerp mevedel-view--status-marker)
                 (set-marker-insertion-type
                  mevedel-view--status-marker status-type))
               (when (markerp mevedel-view--interaction-marker)
                 (set-marker-insertion-type
                  mevedel-view--interaction-marker interaction-type))
               (set-marker-insertion-type
                mevedel-view--input-marker input-type)))
           (when (overlayp mevedel-view--composer-keymap-overlay)
             (move-overlay
              mevedel-view--composer-keymap-overlay
              (mevedel-view--input-start) (point-max)))))))))

(defun mevedel-view-cycle-permission-mode ()
  "Cycle the current session's permission mode from the view buffer."
  (interactive)
  (let* ((data-buf (and (boundp 'mevedel--data-buffer)
                        mevedel--data-buffer
                        (buffer-live-p mevedel--data-buffer)
                        mevedel--data-buffer))
         (session (or (and (boundp 'mevedel--session) mevedel--session)
                      (and data-buf
                           (buffer-local-value 'mevedel--session data-buf)))))
    (unless (and data-buf session)
      (user-error "No mevedel session for permission mode cycling"))
    (let ((next
           (mevedel-view--next-permission-mode
            (or (mevedel-session-permission-mode session) 'ask))))
      (with-current-buffer data-buf
        (mevedel-permission-mode-transition next))
      (message "mevedel: permission mode %s"
               (car (mevedel-view--permission-mode-display next)))
      next)))

(defun mevedel-view-toggle-plan-mode ()
  "Toggle Plan for the current session without changing permissions."
  (interactive)
  (let* ((data-buf (and (boundp 'mevedel--data-buffer)
                        mevedel--data-buffer
                        (buffer-live-p mevedel--data-buffer)
                        mevedel--data-buffer))
         (session (or (and (boundp 'mevedel--session) mevedel--session)
                      (and data-buf
                           (buffer-local-value 'mevedel--session data-buf)))))
    (unless (and data-buf session)
      (user-error "No mevedel session for Plan mode"))
    (if (mevedel-session-plan-mode session)
        (mevedel-plan-mode-exit session)
      (mevedel-plan-mode-enter session))
    (mevedel-view-refresh-input-prompt)
    (message "mevedel: Plan mode %s"
             (if (mevedel-session-plan-mode session) "on" "off"))
    (mevedel-session-plan-mode session)))

(defun mevedel-view--clear-submitted-input (submitted-draft)
  "Clear the composer while it still holds SUBMITTED-DRAFT.
Preparation and prompt hooks can run asynchronously, so anything the user
typed after the submission captured SUBMITTED-DRAFT is a new draft: it
stays, with the mention bindings and dropped-file grants it carries.  A
caller that captured no draft clears unconditionally: a drained pending
input already required an empty composer, and a buffer with no composer
has no draft to protect."
  (when (or (null submitted-draft)
            (equal submitted-draft (mevedel-view--visible-draft)))
    (mevedel-view--clear-input)))

(defun mevedel-view--visible-draft ()
  "Return the composer's visible text, exactly as it stands.
This is the text a submission captures and the text a later clear
decision compares against, so it must not be trimmed or normalized.
Returns nil in a buffer with no editable composer, because a submission
raised from one has no draft to protect."
  (when (and (markerp mevedel-view--input-marker)
             (marker-buffer mevedel-view--input-marker))
    (buffer-substring-no-properties (mevedel-view--input-start) (point-max))))

(defun mevedel-view--input-text ()
  "Return the user's composer text, trimmed."
  (let ((text (mevedel-mention-bindings-copy-text
               (buffer-substring
                (mevedel-view--input-start) (point-max)))))
    (string-trim text)))

(defun mevedel-view--bind-input-mentions (session)
  "Bind known mentions in the live composer for SESSION and return input.
The visible text is unchanged.  Binding before asynchronous preparation
means a failed attempt leaves the exact source attached for a retry."
  (let* ((input-start (mevedel-view--input-start))
         (raw-input
          (mevedel-mention-bindings-copy-text
           (buffer-substring input-start (point-max))))
         (bound-input
          (with-current-buffer mevedel--data-buffer
            (mevedel-mentions-prepare-user-input
             (mevedel-skills-input-prepare-user-input raw-input session)
             session))))
    (with-silent-modifications
      (remove-text-properties
       input-start (point-max)
       '(mevedel-mention-binding nil rear-nonsticky nil))
      (dolist (range (mevedel-mention-bindings-ranges bound-input))
        (mevedel-mention-bindings-set
         (+ input-start (plist-get range :start))
         (+ input-start (plist-get range :end))
         (plist-get range :binding))))
    (string-trim bound-input)))

(defun mevedel-view--composer-snapshot (session)
  "Capture the editable composer and pending grants for SESSION."
  (let ((start (mevedel-view--input-start)))
    (list :text (buffer-substring start (point-max))
          :point-offset (- (point) start)
          :dropped-file-grants
          (copy-sequence (mevedel-session-dropped-file-grants session)))))

(defun mevedel-view--restore-composer-snapshot (snapshot session &optional force)
  "Restore SNAPSHOT for SESSION when its visible draft is still current.
When FORCE is non-nil, replace the current draft unconditionally."
  (let* ((start (mevedel-view--input-start))
         (text (plist-get snapshot :text)))
    (when (or force
              (equal (buffer-substring-no-properties start (point-max))
                     (substring-no-properties text)))
      (let ((inhibit-read-only t))
        (with-silent-modifications
          (delete-region start (point-max))
          (goto-char start)
          (insert text)))
      (mevedel-session--set-dropped-file-grants
       session
       (copy-sequence
        (plist-get snapshot :dropped-file-grants)))
      (goto-char
       (min (point-max)
            (+ start (plist-get snapshot :point-offset))))
      t)))

(defun mevedel-view--composer-scope-key (&optional scope)
  "Return the draft key for SCOPE or ordinary chat."
  (if-let* ((scope (or scope mevedel-view--composer-scope)))
      (list (plist-get scope :directive-id)
            (plist-get scope :action)
            (plist-get scope :attempt-index))
    'chat))

(defun mevedel-view--switch-composer-scope (scope)
  "Switch to SCOPE, preserving separate composer drafts."
  (let ((session (mevedel-view--session)))
    (unless session
      (user-error "No active session for composer scope"))
    (puthash (mevedel-view--composer-scope-key)
             (mevedel-view--composer-snapshot session)
             mevedel-view--composer-drafts)
    (setq mevedel-view--composer-scope scope)
    (mevedel-view-refresh-input-prompt)
    (mevedel-view--restore-composer-snapshot
     (or (gethash (mevedel-view--composer-scope-key scope)
                  mevedel-view--composer-drafts)
         '(:text "" :point-offset 0 :dropped-file-grants nil))
     session t)
    (force-mode-line-update t)
    scope))

(defun mevedel-view-back-to-chat ()
  "Leave directive scope and restore the ordinary chat draft."
  (interactive)
  (when mevedel-view--composer-scope
    (mevedel-view--switch-composer-scope nil)
    (message "mevedel: composer returned to chat scope")))

(defun mevedel-view--directive-record (directive workspace)
  "Resolve DIRECTIVE to a workspace record and WORKSPACE."
  (cond
   ((overlayp directive)
    (let* ((owner (mevedel--topmost-instruction directive 'directive))
           (workspace
            (mevedel--instruction-buffer-workspace (overlay-buffer owner))))
      (list (mevedel--directive-record owner) workspace)))
   ((and workspace (mevedel-directive-p directive)
         (memq directive (mevedel-workspace-directives workspace)))
    (list directive workspace))
   (t (user-error "No directive selected"))))

(defun mevedel-view-enter-directive-scope
    (directive action &optional attempt-index workspace)
  "Open DIRECTIVE's shared session view in sticky ACTION scope."
  (unless (memq action '(discuss plan request-changes retry))
    (error "Unknown directive composer action: %S" action))
  (pcase-let* ((`(,record ,workspace)
                (mevedel-view--directive-record directive workspace))
               (actions (mevedel-directive-actions record)))
    (unless (pcase action
              ('discuss
               (or (memq 'discuss actions)
                   (memq 'continue-discussion actions)
                   (memq 'discuss-result actions)))
              ('plan
               (memq (plist-get (mevedel-directive-plan record) :status)
                     '(draft proposed)))
              (_ (memq action actions)))
      (user-error "Directive action is not available: %s"
                  (mevedel-overlay-ui-directive-action-label action)))
    (let* ((data-buffer
            (car (mevedel--directive-session-buffer record workspace)))
           (view-buffer
            (buffer-local-value 'mevedel--view-buffer data-buffer))
           (scope
            (list :directive-id (mevedel-directive-id record)
                  :action action
                  :attempt-index
                  (or attempt-index
                      (and (eq action 'discuss)
                           (memq 'discuss-result actions)
                           (length (mevedel-directive-attempts record))))
                  :record record
                  :workspace workspace)))
      (unless (buffer-live-p view-buffer)
        (error "Directive session has no live view"))
      (with-current-buffer view-buffer
        (mevedel-view--switch-composer-scope scope)
        (goto-char (point-max)))
      (mevedel-directive-frame-display directive view-buffer t)
      view-buffer)))

(defun mevedel-view--clear-input ()
  "Clear the user's composer text, leaving the prompt in place."
  (mevedel-view--ensure-interactive-chat-view)
  (when-let* ((session (mevedel-view--session)))
    (mevedel-session-clear-dropped-file-grants session))
  (delete-region (mevedel-view--input-start) (point-max))
  (mevedel-view--delete-skill-argument-hint))

(defun mevedel-view--session ()
  "Return the session associated with the current view buffer."
  (or (and (boundp 'mevedel--session) mevedel--session)
      (and (boundp 'mevedel--data-buffer)
           (buffer-live-p mevedel--data-buffer)
           (buffer-local-value 'mevedel--session mevedel--data-buffer))))

(defun mevedel-view--reserved-goal-handoff-id (&optional session)
  "Return SESSION's Goal handoff reservation, or nil."
  (mevedel-plan-handoff-reserved-goal-id
   (or session (mevedel-view--session))))

(defun mevedel-view--occupied-root-workflow (session)
  "Return a symbol naming SESSION's occupying workflow, or nil.
Non-nil means later input is a follow-up.  The symbol is one of
`plan-approval', `directive-planning', `implementation-retry',
`goal-handoff', `goal-budget', or `goal'."
  (cond
   ((mevedel-session-pending-plan-approval session) 'plan-approval)
   ((mevedel-session-directive-planning session) 'directive-planning)
   ((plist-get (mevedel-session-plan-metadata session)
               :implementation-retry)
    'implementation-retry)
   ((mevedel-view--reserved-goal-handoff-id session) 'goal-handoff)
   ((when-let* ((goal (mevedel-session-goal session)))
      (pcase (mevedel-goal-status goal)
        ('complete nil)
        ('budget-limited 'goal-budget)
        (_ 'goal))))))

(defun mevedel-view--occupied-root-workflow-error (occupied)
  "Signal the user error for the OCCUPIED root workflow symbol."
  (user-error
   "%s"
   (pcase occupied
     ('implementation-retry
      "An accepted plan implementation is pending -- M-x mevedel-retry-plan-implementation resumes it")
     ('plan-approval
      "A plan proposal is awaiting approval -- respond to it first")
     ('goal-handoff
      "An accepted-plan Goal handoff is being prepared -- wait for it to finish")
     ('goal-budget
      "The unfinished Goal has exhausted its token budget -- /goal budget N or /goal budget none continues it")
     ('goal
      "An unfinished Goal owns this session -- /goal resume continues it")
     (_ "The workflow is occupied -- use C-c TAB for a follow-up"))))

(defun mevedel-view--steerable-root-request-p (request)
  "Return non-nil when REQUEST is an ordinary root provider turn."
  (and request (mevedel-request-fsm request)))

(defun mevedel-view--delete-skill-argument-hint ()
  "Remove the composer skill argument hint overlay."
  (when (overlayp mevedel-view--skill-argument-hint-overlay)
    (delete-overlay mevedel-view--skill-argument-hint-overlay))
  (setq mevedel-view--skill-argument-hint-overlay nil))

(defun mevedel-view--skill-argument-hint ()
  "Return display-only skill argument hint for the current composer."
  (when-let* ((session (mevedel-view--session))
              (input-start (and (markerp mevedel-view--input-marker)
                                (marker-buffer mevedel-view--input-marker)
                                (mevedel-view--input-start)))
              ((>= (point) input-start))
              (text (buffer-substring-no-properties input-start (point-max)))
              (parsed (mevedel-skills-input-parse-skill-line text))
              (name (nth 0 parsed))
              (skill (mevedel-session-get-skill session name))
              ((mevedel-skill-user-invocable-p skill)))
    (mevedel-skills-remaining-argument-hint skill (nth 1 parsed))))

(defun mevedel-view--refresh-skill-argument-hint ()
  "Refresh the display-only skill argument hint in the composer."
  (let* ((input-marker-pos (and (markerp mevedel-view--input-marker)
                                (marker-buffer mevedel-view--input-marker)
                                (marker-position mevedel-view--input-marker)))
         (marker-at-prompt-p
          (and input-marker-pos
               (< input-marker-pos (point-max))
               (get-text-property input-marker-pos 'mevedel-view-prompt))))
    (cond
     ((or mevedel-view--agent-transcript-p
          (not input-marker-pos))
      (mevedel-view--delete-skill-argument-hint))
     ((and marker-at-prompt-p (< (point) input-marker-pos))
      (mevedel-view--delete-skill-argument-hint))
     ((< (point) (mevedel-view--input-start))
      (mevedel-view--delete-skill-argument-hint))
     (t
      (let ((hint (mevedel-view--skill-argument-hint)))
        (if (and hint (not (string-empty-p hint)))
            (progn
              (unless (overlayp mevedel-view--skill-argument-hint-overlay)
                (setq mevedel-view--skill-argument-hint-overlay
                      (make-overlay (point) (point) (current-buffer) nil t))
                (overlay-put mevedel-view--skill-argument-hint-overlay
                             'priority 10))
              (move-overlay mevedel-view--skill-argument-hint-overlay
                            (point) (point) (current-buffer))
              (overlay-put
               mevedel-view--skill-argument-hint-overlay
               'after-string
               (propertize (concat " " hint) 'font-lock-face 'shadow)))
          (mevedel-view--delete-skill-argument-hint)))))))

(defun mevedel-view--refresh-skill-argument-hint-after-change
    (start end _old-length)
  "Invalidate edited bindings and refresh the composer skill hint."
  (when (and (markerp mevedel-view--input-marker)
             (marker-buffer mevedel-view--input-marker)
             (>= start (mevedel-view--input-start)))
    (mevedel-mention-bindings-invalidate-edit
     start end (mevedel-view--input-start) (point-max)))
  (mevedel-view--refresh-skill-argument-hint))

(defun mevedel-view-slash-capf ()
  "Completion-at-point for slash commands and `$' skills in the composer.
Offers local slash commands at `/name', session skills at `$name',
and command argument completion for commands with finite choices."
  (when (and mevedel--data-buffer
             (buffer-live-p mevedel--data-buffer)
             (>= (point) (mevedel-view--input-start)))
    (let ((session (buffer-local-value 'mevedel--session
                                       mevedel--data-buffer)))
      (mevedel-skills-slash-capf
       mevedel--data-buffer session mevedel-slash-commands
       (mevedel-view--input-start)))))

(defun mevedel-view--start-fork-skill-turn
    (input display-text &optional hook-context submitted-draft)
  "Render and record a fork skill INPUT without calling `gptel-send'.

DISPLAY-TEXT is shown in the view for the user turn.  INPUT is written
to the data buffer as the authoritative user prompt.  The data-turn
  marker is anchored after that prompt so the eventual fork result can be
rendered by the normal post-response hook.  HOOK-CONTEXT is summarized
in the view when present.  SUBMITTED-DRAFT is the composer text captured
when the submission started."
  (when (mevedel-turn-busy-p mevedel--data-buffer)
    (user-error "Turn settlement is still pending"))
  (let ((view-turn-start
         (mevedel-view--insert-user-message display-text nil hook-context)))
    (mevedel-view--clear-submitted-input submitted-draft)
    (with-current-buffer mevedel--data-buffer
      (when mevedel--session
        (mevedel-request-begin
         mevedel--session
         (and (boundp 'mevedel--current-directive-uuid)
              mevedel--current-directive-uuid)))
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
        (insert input "\n")
        (mevedel--clear-user-turn-gptel-properties user-turn-start (point)))
      (let ((data-turn-start (copy-marker (point) nil)))
        (with-current-buffer mevedel--view-buffer
          (mevedel-view-stream-begin-turn
           view-turn-start data-turn-start))))))

(defun mevedel-view--finish-fork-skill-outcome
    (name outcome view-buffer data-buffer &optional skill)
  "Handle fork skill OUTCOME for NAME."
  (when (and (buffer-live-p view-buffer)
             (buffer-live-p data-buffer))
    (when (and (fboundp 'mevedel-review-command-skill-p)
               (mevedel-review-command-skill-p skill)
               (fboundp 'mevedel-review-mark-command-outcome))
      (setq outcome (mevedel-review-mark-command-outcome outcome)))
    (when (fboundp 'mevedel-review-transform-outcome)
      (setq outcome (mevedel-review-transform-outcome name outcome)))
    (pcase (plist-get outcome :status)
      ('ok
       (pcase (plist-get outcome :kind)
         ('fork
          (with-current-buffer data-buffer
            (mevedel-skills-input-insert-fork-result outcome)))
         (_
          (message "Skill '%s' returned unsupported outcome: %S"
                   name outcome))))
      (_
       (with-current-buffer view-buffer
         (mevedel-view--stop-request-progress)
         (message "Skill '%s' failed: %s"
                  name
                  (or (plist-get outcome :message)
                      "unknown error")))
       (with-current-buffer data-buffer
         (when (bound-and-true-p mevedel--current-request)
           (mevedel-request-end))
         (gptel--update-status " Ready" 'success))))))

(defun mevedel-view--skill-submission-active-p
    (token view-buffer data-buffer)
  "Return non-nil when TOKEN still owns a live skill submission."
  (and (buffer-live-p view-buffer)
       (buffer-live-p data-buffer)
       (not (plist-get token :cancelled))
       (with-current-buffer view-buffer
         (eq mevedel-view--pending-skill-submission token))))

(defun mevedel-view--finish-skill-submission (token)
  "Clear TOKEN when it still owns the current skill submission."
  (when (eq mevedel-view--pending-skill-submission token)
    (setq mevedel-view--pending-skill-submission nil)))

(defun mevedel-view--prepared-fork-outcome (prepared)
  "Return PREPARED's fork command outcome, or nil."
  (when-let* ((pair
               (cl-find-if
                (lambda (item)
                  (eq (plist-get (plist-get item :outcome) :kind) 'fork))
                (plist-get prepared :prepared-entries))))
    (plist-get pair :outcome)))

(defun mevedel-view--block-planned-submission (submission &optional prepared)
  "Finish SUBMISSION as blocked, optionally reporting failed PREPARED work."
  (let ((token (plist-get submission :token))
        (on-block (plist-get submission :on-block)))
    (mevedel-view--finish-skill-submission token)
    (when (and prepared
               (not (eq (plist-get prepared :reason) 'cancelled)))
      (message "mevedel: skill $%s failed: %s"
               (or (plist-get prepared :name) "unknown")
               (or (plist-get prepared :message) "unknown error")))
    (when on-block
      (funcall on-block))))

(defun mevedel-view--prepared-plan-outcome
    (submission prepared hook-input hook-context hook-audits)
  "Return the structured prepared outcome for SUBMISSION.
PREPARED is the skill planner result.  HOOK-INPUT, HOOK-CONTEXT, and
HOOK-AUDITS are the accepted `UserPromptSubmit' result."
  (let* ((plan (plist-get submission :plan))
         (input (plist-get submission :input))
         (prepared-input (plist-get prepared :model-input))
         (rewrite-preserves-plan-p
          (string-search prepared-input hook-input))
         (hook-input (if rewrite-preserves-plan-p
                         hook-input
                       prepared-input))
         (hook-audits (and rewrite-preserves-plan-p hook-audits))
         (model-input
          (if hook-context
              (concat hook-input "\n\n" hook-context)
            hook-input)))
    (list :model-input
          model-input
          :transcript-input
          (if hook-context
              (concat input "\n\n" hook-context)
            input)
          :hook-input hook-input
          :hook-context hook-context
          :hook-audits
          (append (plist-get prepared :hook-audits) hook-audits)
          :request-context (plist-get prepared :request-context)
          :render-data (mevedel-skills-plan-render-data plan prepared)
          :fork-outcome
          (and (mevedel-skill-invocation-plan-fork-p plan)
               (mevedel-view--prepared-fork-outcome prepared)))))

(cl-defun mevedel-view--dispatch-prepared-outcome
    (submission data-buffer &key before-send after-insert on-block dispatch
                submitted-draft)
  "Dispatch accepted prompt SUBMISSION through DATA-BUFFER.
BEFORE-SEND runs at the dispatch boundary.  AFTER-INSERT runs once the prompt
is durably recorded.  ON-BLOCK runs after a dispatch error.  DISPATCH, when
non-nil, receives SUBMISSION instead of starting a request.  SUBMITTED-DRAFT is
the composer text captured before preparation began, so a draft typed while it
ran survives the send."
  (let* ((view-buffer (current-buffer))
         (outcome (mevedel-prompt-submission-outcome submission))
         (input (mevedel-prompt-submission-display-text submission))
         (model-input (plist-get outcome :model-input))
         (transcript-input (plist-get outcome :transcript-input))
         (hook-input (plist-get outcome :hook-input))
         (hook-context (plist-get outcome :hook-context))
         (all-audits (plist-get outcome :hook-audits))
         (request-context (plist-get outcome :request-context))
         (render-data (or (plist-get outcome :render-data) ""))
         (fork-outcome (plist-get outcome :fork-outcome))
         (view-context
          (and (not dispatch)
               (mevedel-view--join-hook-contexts
                (mevedel-hooks-format-context
                 (mevedel-view--hook-context-events-from-text hook-input))
                hook-context))))
    (condition-case err
        (progn
          (when before-send
            (funcall before-send))
          (cond
           (dispatch
            (funcall dispatch submission))
           (fork-outcome
            (let* ((skill (plist-get fork-outcome :skill))
                   (name (mevedel-skill-name skill)))
              (mevedel-view--start-fork-skill-turn
               (concat transcript-input render-data) input view-context
               submitted-draft)
              (mevedel-prompt-submission-commit submission)
              (when after-insert
                (funcall after-insert))
              (with-current-buffer data-buffer
                (mevedel-skills-dispatch-prepared-fork
                 fork-outcome
                 (lambda (result)
                   (mevedel-view--finish-fork-skill-outcome
                    name result view-buffer data-buffer skill))
                 :prompt model-input
                 :request-context request-context
                 :hook-audits all-audits))))
           (t
            (with-current-buffer data-buffer
              (setq-local mevedel-skills--pending-request-context
                          request-context))
            (mevedel-view--forward-input
             (concat transcript-input render-data)
             :display-text input
             :prompt-checked t
             :submission submission
             :after-insert after-insert
             :model-input (concat model-input render-data)
             :submitted-draft submitted-draft))))
      (error
       (when (buffer-live-p data-buffer)
         (with-current-buffer data-buffer
           (setq-local mevedel-skills--pending-request-context nil)))
       (message "mevedel: skill dispatch failed: %s"
                (error-message-string err))
       (when on-block
         (funcall on-block))
       nil))))

(defun mevedel-view--dispatch-prepared-plan
    (plan-submission prepared prompt-submission)
  "Dispatch PREPARED plan for PLAN-SUBMISSION and PROMPT-SUBMISSION."
  (let* ((token (plist-get plan-submission :token))
         (view-buffer (plist-get plan-submission :view-buffer))
         (data-buffer (plist-get plan-submission :data-buffer)))
    (when (mevedel-view--skill-submission-active-p
           token view-buffer data-buffer)
      (with-current-buffer view-buffer
        (mevedel-view--finish-skill-submission token)
        (let* ((hook-input
                (mevedel-prompt-submission-input prompt-submission))
               (hook-context
                (mevedel-prompt-submission-context prompt-submission))
               (hook-audits
                (mevedel-prompt-submission-audits prompt-submission))
               (outcome
                (mevedel-view--prepared-plan-outcome
                 plan-submission prepared hook-input hook-context hook-audits)))
          (mevedel-prompt-submission-set-outcome
           prompt-submission outcome)
          (when-let* ((warnings (plist-get prepared :warnings)))
            (message "mevedel: %s" (string-join warnings "; ")))
          (mevedel-view--dispatch-prepared-outcome
           prompt-submission data-buffer
           :before-send (plist-get plan-submission :before-send)
           :after-insert (plist-get plan-submission :after-insert)
           :on-block (plist-get plan-submission :on-block)
           :dispatch (plist-get plan-submission :dispatch)
           :submitted-draft (plist-get plan-submission :submitted-draft)))))))

(defun mevedel-view--handle-prepared-plan (submission prepared)
  "Continue SUBMISSION after PREPARED skill work settles."
  (let ((token (plist-get submission :token))
        (view-buffer (plist-get submission :view-buffer))
        (data-buffer (plist-get submission :data-buffer)))
    (when (mevedel-view--skill-submission-active-p
           token view-buffer data-buffer)
      (with-current-buffer view-buffer
        (if (not (eq (plist-get prepared :status) 'ok))
            (mevedel-view--block-planned-submission submission prepared)
          (mevedel-view--run-prompt-submit-hook
           (plist-get prepared :model-input)
           (plist-get submission :input)
           (lambda (prompt-submission)
             (mevedel-view--dispatch-prepared-plan
              submission prepared prompt-submission))
           (lambda ()
             (when (mevedel-view--skill-submission-active-p
                    token view-buffer data-buffer)
               (with-current-buffer view-buffer
                 (mevedel-view--block-planned-submission submission))))
           (plist-get prepared :hook-context)))))))

(defun mevedel-view--submit-planned-input
    (input &optional before-send on-block dispatch after-insert inert-skills)
  "Plan, prepare, and submit atomically bound user INPUT.

BEFORE-SEND runs exactly once at the dispatch boundary.  ON-BLOCK runs when
planning, preparation, or `UserPromptSubmit' rejects the submission.  Derived
skill bodies and hook output are never scanned for additional invocations.
When DISPATCH is non-nil, call it with an accepted prompt submission instead of
starting a new request.  AFTER-INSERT runs once the prompt is durably recorded.
When INERT-SKILLS is non-nil, skip skill planning entirely: any skill token in
INPUT stays literal text.  External input -- a collaboration guest's prompt --
carries prompting authority only, never skill invocation."
  (let ((view-buffer (current-buffer))
        (data-buffer mevedel--data-buffer)
        (session (mevedel-view--session))
        ;; Captured here because both skill preparation and the prompt hook
        ;; below run asynchronously, and only this entry is still holding
        ;; the draft the user actually submitted.
        (submitted-draft (mevedel-view--visible-draft)))
    (let* ((plan
            (unless inert-skills
              (with-current-buffer data-buffer
                (mevedel-skills-input-refresh-bound-input input session)
                (mevedel-skills-plan-user-input input session)))))
      (if (or inert-skills
              (null (mevedel-skill-invocation-plan-occurrences plan)))
          (if dispatch
              (mevedel-view--run-prompt-submit-hook
               input input
               (lambda (prompt-submission)
                 (let* ((hook-input
                         (mevedel-prompt-submission-input
                          prompt-submission))
                        (hook-context
                         (mevedel-prompt-submission-context
                          prompt-submission))
                        (prepared-input
                         (if hook-context
                             (concat hook-input "\n\n" hook-context)
                           hook-input)))
                   (mevedel-prompt-submission-set-outcome
                    prompt-submission
                    (list :model-input prepared-input
                          :transcript-input prepared-input
                          :hook-input hook-input
                          :hook-context hook-context
                          :hook-audits
                          (mevedel-prompt-submission-audits
                           prompt-submission)))
                   (mevedel-view--dispatch-prepared-outcome
                    prompt-submission data-buffer
                    :before-send before-send
                    :after-insert after-insert
                    :on-block on-block
                    :dispatch dispatch
                    :submitted-draft submitted-draft)))
               on-block)
            (mevedel-view--forward-input
             input :before-send before-send :after-insert after-insert
             :on-block on-block :submitted-draft submitted-draft))
        (let* ((token (list :cancelled nil))
               (submission
                (list :token token
                      :input input
                      :plan plan
                      :view-buffer view-buffer
                      :data-buffer data-buffer
                      :before-send before-send
                      :after-insert after-insert
                      :dispatch dispatch
                      :on-block on-block
                      :submitted-draft submitted-draft)))
          (setq mevedel-view--pending-skill-submission token)
          (with-current-buffer data-buffer
            (mevedel-skills-plan-prepare
             plan
             (lambda (prepared)
               (mevedel-view--handle-prepared-plan submission prepared))
             (lambda ()
               (not (mevedel-view--skill-submission-active-p
                     token view-buffer data-buffer))))))))))

(defun mevedel-view--submit-armed-session-fork
    (source-view input target snapshot)
  "Publish TARGET from SOURCE-VIEW, then submit INPUT in its Child.
SNAPSHOT is the exact Source composer state transferred on publication."
  (let* ((fork-type (plist-get target :fork-type))
         (source-data
          (buffer-local-value 'mevedel--data-buffer source-view))
         (referenced-grants
          (cl-intersection
           (mevedel-view--mentioned-file-paths input)
           (plist-get snapshot :dropped-file-grants)
           :test #'equal))
         (child-data
          (progn
            (pcase fork-type
              ('conversation
               (mevedel-session-fork-conversation-fork
                source-data target))
              ('worktree
               (mevedel-session-fork-worktree-fork
                source-data target))
              (_
               (error "Unknown session fork type: %S" fork-type)))))
         (child-view
          (buffer-local-value 'mevedel--view-buffer child-data))
         (child-session
          (buffer-local-value 'mevedel--session child-data)))
    (unless (buffer-live-p child-view)
      (error "Session Fork has no live view"))
    (with-current-buffer source-view
      (mevedel-view--full-rerender))
    (setq snapshot (copy-tree snapshot t))
    (when (eq fork-type 'worktree)
      (setq input
            (mevedel-view--retarget-worktree-mention-bindings
             input child-session)
            referenced-grants
            (mapcar
             (lambda (path)
               (mevedel-session-fork-retarget-worktree-path
                child-session path))
             referenced-grants))
      (plist-put
       snapshot :text
       (mevedel-view--retarget-worktree-mention-bindings
        (plist-get snapshot :text) child-session)))
    (plist-put snapshot :dropped-file-grants referenced-grants)
    (with-current-buffer child-view
      (mevedel-view--restore-composer-snapshot
       snapshot child-session t))
    (with-current-buffer source-view
      (mevedel-view--clear-input)
      (mevedel-view-cancel-session-fork))
    (display-buffer child-view)
    (with-current-buffer child-view
      (mevedel-view--submit-planned-input
       input nil nil nil
       (lambda () (mevedel-view-history-add input))))
    child-data))

(defun mevedel-view--retarget-worktree-mention-bindings (text session)
  "Retarget repository-local mention bindings in TEXT for SESSION."
  (let ((copy (mevedel-mention-bindings-copy-text text)))
    (dolist (range (mevedel-mention-bindings-ranges copy))
      (let* ((binding (copy-tree (plist-get range :binding) t))
             (key
              (pcase (plist-get binding :kind)
                ('file :path)
                ('skill :source-file))))
        (when key
          (plist-put
           binding key
           (mevedel-session-fork-retarget-worktree-path
            session (plist-get binding key)))
          (mevedel-mention-bindings-set
           (plist-get range :start)
           (plist-get range :end)
           binding copy))))
    copy))

(defun mevedel-view--queued-scope (&optional scope)
  "Return the serializable queue identity for directive SCOPE."
  (when-let* ((scope (or scope mevedel-view--composer-scope)))
    (list :directive-id (plist-get scope :directive-id)
          :action (plist-get scope :action)
          :attempt-index (plist-get scope :attempt-index))))

(defun mevedel-view--dispatch-directive-input (scope input)
  "Dispatch directive-scoped INPUT according to SCOPE."
  (let* ((session (mevedel-view--session))
         (workspace (mevedel-session-workspace session))
         (id (plist-get scope :directive-id))
         (record
          (or (and (mevedel-directive-p (plist-get scope :record))
                   (plist-get scope :record))
              (cl-find id (mevedel-workspace-directives workspace)
                       :key #'mevedel-directive-id :test #'equal))))
    (unless record
      (user-error "Directive no longer exists: %s" id))
    (let ((directive
           (plist-get
            (mevedel--directive-action-context record workspace)
            :directive)))
      (pcase (plist-get scope :action)
        ('discuss
         (mevedel--discuss-directive-turn
          directive input (plist-get scope :attempt-index)))
        ('plan
         (mevedel-directive-plan-continue directive input))
        ('request-changes
         (mevedel--request-directive-changes directive input))
        ('retry
         (mevedel--retry-directive directive input))
        (_ (error "Unknown directive composer action: %S"
                  (plist-get scope :action)))))))

(defun mevedel-view--send-directive-input (input)
  "Send INPUT in the current sticky directive scope."
  (let ((session (mevedel-view--session)))
    (when (or (buffer-local-value 'mevedel--current-request
                                  mevedel--data-buffer)
              (and (mevedel-view--occupied-root-workflow session)
                   (not (eq (plist-get mevedel-view--composer-scope :action)
                            'plan))))
      (user-error "The workflow is occupied -- use C-c TAB to queue this directive follow-up"))
    (when (mevedel-skills-parse-slash-line input)
      (user-error "Slash commands are unavailable in directive scope"))
    (mevedel-view--dispatch-directive-input mevedel-view--composer-scope input)
    (mevedel-view-history-add input)
    (mevedel-view--clear-input)))

(defun mevedel-view-send ()
  "Send the current root or ephemeral side-conversation composer text."
  (interactive)
  (if mevedel-view--side-conversation-p
      (mevedel-side-conversation-send)
    (mevedel-view--send-root)))

(defun mevedel-view--send-root ()
  "Send the current composer text to the LLM via the data buffer.
Extracts text from the input zone, plans all bound `$skill' mentions,
renders the original text in the history region, and dispatches either
one coherent request or one leading fork command.  Slash commands retain
their local dispatch path."
  (mevedel-view--ensure-interactive-chat-view)
  (mevedel-view--assert-live-tip t)
  (when mevedel-view--pending-input-edit
    (user-error "Save or cancel the pending-input edit first"))
  (unless mevedel--data-buffer
    (user-error "No data buffer associated with this view"))
  (unless (buffer-live-p mevedel--data-buffer)
    (user-error "Data buffer has been killed"))
  (when (and (mevedel-turn-busy-p mevedel--data-buffer)
             (not (buffer-local-value 'mevedel--current-request
                                      mevedel--data-buffer)))
    (user-error "Turn settlement is still pending"))
  (when mevedel-view--prompt-hook-pending
    (user-error "A prompt hook is still running -- wait or abort first"))
  (when mevedel-view--pending-skill-submission
    (user-error "Skill preparation is still running -- wait or abort first"))
  (when (buffer-local-value 'mevedel-compact-run-in-flight mevedel--data-buffer)
    (message "mevedel: compacting, please wait...")
    (user-error "Compaction in progress"))
  (when (buffer-local-value 'mevedel-session--read-only-mode
                            mevedel--data-buffer)
    (user-error "Session is open read-only (another host holds the lock)"))
  (let* ((session (buffer-local-value 'mevedel--session
                                      mevedel--data-buffer))
         (snapshot (and session
                        (mevedel-view--composer-snapshot session)))
         (input (if (and session (not mevedel-view--composer-scope))
                    (mevedel-view--bind-input-mentions session)
                  (mevedel-view--input-text))))
    (when session
      (mevedel-session-artifacts-assert-new-mutation-authority session))
    (when (string-empty-p input)
      (user-error "Nothing to send"))
    (if mevedel-view--composer-scope
        (mevedel-view--send-directive-input input)
      (let* ((slash-parsed (mevedel-skills-parse-slash-line input))
             (fork-target mevedel-view--armed-session-fork)
             (active-request
              (buffer-local-value 'mevedel--current-request
                                  mevedel--data-buffer))
             (occupied
              (and (not slash-parsed)
                   (mevedel-view--occupied-root-workflow session))))
	(when (and slash-parsed (mevedel-view-historical-segment-p))
          (user-error
           "Slash commands are unavailable while viewing a historical segment"))
	(when (and fork-target (not slash-parsed))
          (mevedel-session-rewind-assert-stable-source
           session mevedel--data-buffer "forking"))
	(if (or active-request occupied)
            (let ((restore
                   (lambda ()
                     (when snapshot
                       (mevedel-view--restore-composer-snapshot
			snapshot session)))))
              (cond
               ((and slash-parsed
                     (mevedel-skills-local-command-active-request-p
                      (nth 0 slash-parsed) (nth 1 slash-parsed)))
		(let ((result (with-current-buffer mevedel--data-buffer
				(funcall (cdr (assoc (nth 0 slash-parsed)
                                                     mevedel-slash-commands))
					 (nth 1 slash-parsed)))))
                  (unless (eq result 'mevedel-view-sent)
                    (when (stringp result) (message "%s" result))
                    (mevedel-view-history-add input)
                    (mevedel-view--clear-input))))
               (slash-parsed
		(funcall restore)
		(user-error
		 "A request is already active -- wait or abort first"))
               ((not active-request)
		(funcall restore)
		(mevedel-view--occupied-root-workflow-error
		 occupied))
               ((not (mevedel-view--steerable-root-request-p active-request))
		(funcall restore)
		(user-error
		 "This workflow cannot be steered -- use C-c TAB"))
               (t
		(condition-case err
                    (mevedel-view--submit-planned-input
                     input nil
                     (lambda ()
                       (funcall restore)
                       (message
			"mevedel: steering preparation failed; use C-c TAB"))
                     (lambda (submission)
                       (unless
                           (mevedel-view--queue-prepared-steering
                            submission active-request)
			 (funcall restore))))
                  (error
                   (funcall restore)
                   (user-error "%s; use C-c TAB for a follow-up"
                               (error-message-string err)))))))
          (cond
           (slash-parsed
            (mevedel-view-run-invocation
             (nth 0 slash-parsed) (nth 1 slash-parsed)
             :display input
             :on-quiet (lambda ()
                         (mevedel-view-history-add input)
                         (mevedel-view--clear-input))
             :on-sent (lambda () (mevedel-view-history-add input))))
           (t
            (let ((source-view (current-buffer)))
              (if fork-target
                  (progn
                    ;; Parsing stays in Source so malformed skill syntax cannot
                    ;; publish a child.  Expansion and hooks belong to Child.
                    (with-current-buffer mevedel--data-buffer
                      (mevedel-skills-plan-user-input input session))
                    (mevedel-view--submit-armed-session-fork
                     source-view input fork-target snapshot))
		(mevedel-view--submit-planned-input
		 input
		 (lambda ()
                   (mevedel-view-history-add input)))))))))))
  ;; Accepted sends clear the submitted draft and land at the composer end.
  ;; Rejected sends preserve the exact input-relative point.
  (unless (mevedel-view--point-in-input-region-p)
    (goto-char (point-max))))

(defun mevedel-view-invocation-kind (name &optional session)
  "Return how NAME is invoked: `command\=', `skill\=', or nil.

Local slash commands and skills are separate namespaces reached with
different sigils -- `/name\=' runs a command, `$name\=' runs a skill -- so
anything dispatching a bare name has to ask which one it is."
  (cond
   ((assoc name mevedel-slash-commands) 'command)
   ((let ((session (or session (mevedel-view--session))))
      (cl-find name (mevedel-skills-user-visible-skills session)
               :key #'mevedel-skill-name :test #'equal))
    'skill)))

(cl-defun mevedel-view-run-invocation (name args &key display on-quiet on-sent)
  "Dispatch invocation NAME with ARGS from the current view buffer.

NAME is a local slash command or a user-invocable skill; ARGS is the
argument string after it.  DISPLAY is the original input line, used
where a command needs the text the user actually wrote.  ON-QUIET runs
when a local command completed without starting a turn, ON-SENT when
one was started.

This is the one place that decides between the two invocation
namespaces.  Composer input and queued external invocations both route
through it, so a collaboration guest\='s button runs exactly what typing
the same line in the composer would."
  (let* ((local (assoc name mevedel-slash-commands))
         (kind (if local
                   'command
                 (mevedel-view-invocation-kind name)))
        (display (or display
                     (concat (if (eq kind 'skill)
                                 "$" "/")
                             name
                             (if (and args (not (string-blank-p args)))
                                 (concat " " args)
                               "")))))
    (cond
     ((and local
           (string= name "goal")
           args
           (not (string-blank-p args))
           (not (member (car (split-string args "[ \t\n]+" t))
                        '("edit" "pause" "resume" "clear"))))
      (mevedel-view--send-local-goal display args)
      (when on-sent (funcall on-sent)))
     ;; `/plan ARGS\=' is Plan mode plus a turn: the arguments are the
     ;; prompt, and the command itself takes none.
     ((and local
           (string= name "plan")
           args
           (not (string-blank-p args)))
      (let ((data-buffer mevedel--data-buffer))
        (mevedel-view--submit-planned-input
         args
         (lambda ()
           (with-current-buffer data-buffer
             (mevedel-plan-mode-enter))
           (when on-sent (funcall on-sent))))))
     (local
      (let ((result (with-current-buffer mevedel--data-buffer
                      (funcall (cdr local) args))))
        ;; Most local slash commands don't send a turn.  A command may
        ;; return this sentinel when it took ownership of the input.
        (if (eq result 'mevedel-view-sent)
            (when on-sent (funcall on-sent))
          (when (stringp result) (message "%s" result))
          (when on-quiet (funcall on-quiet)))))
     ((eq kind 'skill)
      ;; A skill is invoked with its own sigil and planned like any
      ;; other authored `$skill\=' line.
      (mevedel-view--submit-planned-input
       (concat "$" name
               (if (and args (not (string-blank-p args)))
                   (concat " " args)
                 ""))
       (lambda () (when on-sent (funcall on-sent)))))
     (t
      (message "Unknown slash command: /%s" name)
      (when on-quiet (funcall on-quiet))))))

(defun mevedel-view--send-local-goal (input args)
  "Run pre-send check and start local `/goal' with ARGS.
INPUT is the original composer text, including the slash command."
  (let* ((view-buffer (current-buffer))
         (data-buffer mevedel--data-buffer)
         (objective args)
         (submitted-draft (mevedel-view--visible-draft)))
    (when (string-blank-p objective)
      (user-error "Goal objective must not be blank"))
    (mevedel-view--run-prompt-submit-hook
     objective input
     (lambda (submission)
       (when (and (buffer-live-p view-buffer)
                  (buffer-live-p data-buffer))
         (with-current-buffer view-buffer
           (mevedel-view-history-add input)
           (mevedel-view--clear-submitted-input submitted-draft))
         (with-current-buffer data-buffer
           (mevedel-goal-start
            (mevedel-prompt-submission-input submission) submission)))))))

(defun mevedel-view--join-hook-contexts (&rest contexts)
  "Return CONTEXTS joined as separate hook context blocks."
  (let ((contexts (delq nil contexts)))
    (when contexts
      (mapconcat #'identity contexts "\n\n"))))

(defun mevedel-view--run-prompt-submit-hook
    (input display-text callback &optional blocked-callback prior-context)
  "Run `UserPromptSubmit' for INPUT, then call CALLBACK if accepted.
DISPLAY-TEXT is the user-facing prompt text.  CALLBACK receives
one `mevedel-prompt-submission'.  PRIOR-CONTEXT, when non-nil, is placed
after pending session context and before submit-hook context.  The consumer
commits the submission immediately after it durably records the accepted
input."
  (mevedel-view--ensure-interactive-chat-view)
  (when mevedel-view--prompt-hook-pending
    (user-error "A prompt hook is still running -- wait or abort first"))
  (let ((view-buffer (current-buffer))
        (data-buffer mevedel--data-buffer)
        session workspace submission)
    (unless (and data-buffer (buffer-live-p data-buffer))
      (user-error "Data buffer has been killed"))
    (with-current-buffer data-buffer
      (setq session mevedel--session
            workspace mevedel--workspace))
    (setq submission
          (mevedel-prompt-submission-create
           :input input :display-text display-text :session session)
          mevedel-view--prompt-hook-pending submission)
    (condition-case err
        (with-current-buffer data-buffer
          (mevedel-hooks-run-event
           'UserPromptSubmit
           (mevedel-hooks-event-plist
            'UserPromptSubmit session workspace
            :prompt input
            :display-text display-text)
           (lambda (decision)
             (when (buffer-live-p view-buffer)
               (with-current-buffer view-buffer
                 (when (eq mevedel-view--prompt-hook-pending submission)
                   (setq mevedel-view--prompt-hook-pending nil)
                   (when (buffer-live-p data-buffer)
                     (setq decision
                           (mevedel-hooks-sanitize-final-decision
                            'UserPromptSubmit decision))
                     (cond
                      ((and (plist-member decision :continue)
                            (not (plist-get decision :continue)))
                       (mevedel-prompt-submission-cancel submission)
                       (mevedel-hooks-record-session-context
                        session decision 'UserPromptSubmit)
                       (when blocked-callback
                         (funcall blocked-callback))
                       (message "mevedel: prompt blocked by hook: %s"
                                (or (plist-get decision :stop-reason)
                                    "no reason provided")))
                      (t
                       (when-let* ((msg (plist-get decision :system-message)))
                         (message "mevedel: %s" msg))
                       (let* ((submitted
                               (if (stringp (plist-get decision :updated-input))
                                   (plist-get decision :updated-input)
                                 input))
                              (pending-entries
                               (mevedel-session-hook-context-pending session))
                              (pending-context
                               (mevedel-hooks-format-context pending-entries))
                              (submit-context
                               (mevedel-hooks-additional-context-string
                                decision 'UserPromptSubmit))
                              (context
                               (mevedel-view--join-hook-contexts
                                pending-context prior-context submit-context))
                              (audit
                               (mevedel-view--prompt-rewrite-audit-record
                                'UserPromptSubmit input submitted decision)))
                         (when (fboundp
                                'mevedel-plan-mode--invalidate-proposal)
                           (mevedel-plan-mode--invalidate-proposal session))
                         (mevedel-prompt-submission-accept
                          submission submitted context
                          (and audit (list audit)) pending-entries)
                         (funcall callback submission)))))))))
           session workspace nil nil))
      (error
       (when (eq mevedel-view--prompt-hook-pending submission)
         (setq mevedel-view--prompt-hook-pending nil))
       (mevedel-prompt-submission-cancel submission)
       (signal (car err) (cdr err))))))

(cl-defun mevedel-view--forward-input
    (input &key display-text before-send after-insert prompt-checked on-block
           submission model-input
           (submitted-draft (mevedel-view--visible-draft)))
  "Render INPUT in the history region, forward to the data buffer, and send.
Helper for `mevedel-view-send'.  When DISPLAY-TEXT is non-nil, show
that in the view instead of INPUT (e.g., compact skill invocation).
Optional BEFORE-SEND is called after prompt hooks allow the send but
before any user-visible prompt or data-buffer prompt is inserted.  When
PROMPT-CHECKED is non-nil, skip `UserPromptSubmit' because the caller
already ran it.  ON-BLOCK is called when a prompt hook blocks.
SUBMISSION carries hook context, audits, and commit ownership when
PROMPT-CHECKED is non-nil.  MODEL-INPUT, when non-nil, replaces INPUT only in
the temporary request prompt.  SUBMITTED-DRAFT defaults to the composer text
captured when this submission starts.

Anchors the incremental-render markers so progress hooks can redraw
the in-flight assistant turn as tool calls complete:
`mevedel-view--in-flight-turn-start' points into the view just above
the input zone (where the assistant turn will be rendered);
`mevedel-view--data-turn-start' points into the data buffer just
after the forwarded prompt, where the LLM's response will begin."
  (cl-labels
      ((send-now (stored-input view-text accepted request-input)
         (when (mevedel-turn-busy-p mevedel--data-buffer)
           (user-error "The session became busy before dispatch"))
         (when before-send
           (funcall before-send))
         (mevedel-view--forward-input-now
          stored-input
          :display-text view-text
          :submission accepted
          :after-insert after-insert
          :model-input request-input
          :submitted-draft submitted-draft)))
    (if prompt-checked
        (send-now input (or display-text input) submission model-input)
      (mevedel-view--run-prompt-submit-hook
       input display-text
       (lambda (accepted)
         (let ((hook-input
                (mevedel-prompt-submission-input accepted))
               (context
                (mevedel-prompt-submission-context accepted)))
           (send-now
            (if context
                (concat hook-input "\n\n" context)
              hook-input)
            (or display-text hook-input)
            accepted
            nil)))
       on-block))))

(cl-defun mevedel-view--forward-input-now
    (input &key display-text submission after-insert model-input
           submitted-draft)
  "Forward INPUT to gptel immediately, after prompt hooks have run.
DISPLAY-TEXT is shown in the view instead of INPUT when non-nil.  SUBMISSION
supplies hook context, audits, and commit ownership.  MODEL-INPUT, when non-nil,
replaces INPUT only in the temporary request prompt.  SUBMITTED-DRAFT is the
composer text this submission captured; a draft the user changed while
asynchronous preparation ran is left alone instead of cleared."
  (mevedel-view--ensure-interactive-chat-view)
  (when (buffer-local-value 'mevedel-compact-run-in-flight mevedel--data-buffer)
    (message "mevedel: compacting, please wait...")
    (user-error "Compaction in progress"))
  (mevedel-session-durability-with-transaction
   (let* ((input (mevedel--normalize-message-text input))
          (display-text (and display-text
                             (mevedel--normalize-message-text display-text)))
          (hook-context
           (and submission
		(mevedel-prompt-submission-context submission)))
          (hook-audits
           (and submission
		(mevedel-prompt-submission-audits submission)))
          (prompt-summary-body
           (mevedel-view--inline-skill-prompt-summary-body
            (or model-input input)))
          (data-buffer mevedel--data-buffer)
          (session (mevedel-view--session))
          (_admission
           (progn
             (mevedel-request-assert-target-ready session)
             (when (mevedel-turn-busy-p mevedel--data-buffer)
               (user-error "The session became busy before dispatch"))))
          (dropped-file-grants
           (mevedel-view--pop-dropped-file-grants-for-input input session)))
     (let (data-turn-start
           hook-audits-with-source
           prompt-summary-source
           guest-name)
       ;; Forward to the data buffer first so immediate inline-skill
       ;; Prompt handles can expand through the same source-backed fold
       ;; path as a full rerender.
       (with-current-buffer mevedel--data-buffer
         (goto-char (point-max))
         (let ((user-turn-start (point))
               body-start)
           ;; Insert response separator
           (insert gptel-response-separator)
           ;; Insert prompt prefix if needed (e.g., org heading marker)
           (when-let* ((prefix (alist-get major-mode gptel-prompt-prefix-alist)))
             (let ((prefix-length (length prefix)))
               (unless (and (>= (point) (+ (point-min) prefix-length))
                            (string= (buffer-substring-no-properties
                                      (- (point) prefix-length) (point))
                                     prefix))
                 (unless (bolp) (insert "\n"))
                 (insert prefix))))
           (setq body-start (point))
           (insert input "\n")
           (when-let* ((prompt-summary-body)
                       (block
			(car (last
                              (mevedel-tool-render-data-blocks input)))))
             (setq prompt-summary-source
                   (mevedel-view-disclosure-source-range
                    data-buffer
                    (+ body-start (car block))
                    (+ body-start (cadr block)))))
           (mevedel--clear-user-turn-gptel-properties
            user-turn-start (point)))
         (dolist (audit hook-audits)
           (let ((audit-start (point)))
             (insert (mevedel--format-hook-audit-record audit))
             (push (append audit
                           (list :source
                                 (mevedel-view-disclosure-source-range
                                  data-buffer audit-start (point))))
                   hook-audits-with-source)))
         (setq hook-audits-with-source (nreverse hook-audits-with-source))
         ;; A collaboration guest's attribution must land here, with the
         ;; hook audits, before the response marker below exists: any
         ;; later insertion at the turn boundary is claimed by the
         ;; response span and would reach model context.
         (when mevedel-view--pending-guest-attribution
           (setq guest-name mevedel-view--pending-guest-attribution
                 mevedel-view--pending-guest-attribution nil)
           (insert (mevedel--format-hook-audit-record
                    (list :type 'guest-prompt :name guest-name))))
         ;; Anchor the data-side marker after the forwarded prompt so
         ;; incremental renders extract only the in-flight assistant
         ;; segments from here forward.  Pushed onto the view buffer's
         ;; buffer-local so live rendering can read it
         ;; without switching buffers.
         (setq data-turn-start (copy-marker (point) nil)))
       (mevedel-collaboration--safe-accepted-prompt data-buffer)
       (when submission
         (mevedel-prompt-submission-commit submission))
       (when after-insert
         (funcall after-insert))
       ;; Render the user's message in the view after the data source is
       ;; known, but before the model request starts.
       (let ((turn-start
              (mevedel-view--insert-user-message
               (or display-text input) nil hook-context
               prompt-summary-body prompt-summary-source
               hook-audits-with-source guest-name)))
         (mevedel-view-stream-begin-turn turn-start data-turn-start)
         (mevedel-view--clear-submitted-input submitted-draft))
       (with-current-buffer mevedel--data-buffer
         (mevedel-view--activate-dropped-file-grants
          dropped-file-grants session)
         (setq-local mevedel--pending-model-input model-input)
         (condition-case err
             (unwind-protect
                 (gptel-send)
               (setq-local mevedel--pending-model-input nil))
           ((error quit)
            ;; The user's turn is committed, but a start that failed or was
            ;; interrupted gets no gptel terminal callback, so this is the
            ;; only place that can settle it.  The summary is recorded first
            ;; because it reads the request's elapsed time, which ending the
            ;; request clears; the UI stops before that teardown so it never
            ;; outlives the request it describes.  C-g is the user's own
            ;; cancellation, not a provider failure, so it settles as
           ;; aborted like every other cancellation; and the interrupted
           ;; start left gptel's mode-line at " Waiting" for a request
           ;; that no longer exists.
            (let ((quit-p (eq (car err) 'quit)))
              (mevedel-view--append-request-summary
               (current-buffer) data-turn-start
               (list :outcome (if quit-p 'aborted 'error)
                     :backend (or (ignore-errors
                                    (gptel-backend-name gptel-backend))
                                  "Provider")
                     :message (if quit-p
                                  "Interrupted before the provider replied"
                                (error-message-string err))
                     :retry 'manual)))
            (gptel--update-status " Ready" 'success)
            (when (buffer-live-p mevedel--view-buffer)
              (with-current-buffer mevedel--view-buffer
                (mevedel-view-stream-stop)))
            (mevedel-request-end)
            (signal (car err) (cdr err)))))))))

(defun mevedel-view--transform-model-input (fsm)
  "Replace the latest stored prompt with its one-shot model input for FSM."
  (when-let* ((chat-buffer (plist-get (gptel-fsm-info fsm) :buffer))
              ((buffer-live-p chat-buffer))
              (model-input
               (buffer-local-value 'mevedel--pending-model-input
                                   chat-buffer)))
    (with-current-buffer chat-buffer
      (setq-local mevedel--pending-model-input nil))
    (goto-char (mevedel-transcript-prompt-transform-start))
    (delete-region (point) (point-max))
    (insert model-input)))

(defun mevedel-view--agent-fsm-p (info data-buffer)
  "Return non-nil when INFO or DATA-BUFFER belongs to an agent request."
  (or (and (fboundp 'mevedel-agent-invocation-p)
           (mevedel-agent-invocation-p
            (plist-get info :mevedel-agent-invocation)))
      (and (buffer-live-p data-buffer)
           (with-current-buffer data-buffer
             (bound-and-true-p mevedel--agent-invocation)))))

(defun mevedel-view-abort ()
  "Abort the active request from the view buffer."
  (interactive)
  (mevedel-view--ensure-interactive-chat-view)
  (mevedel-view--cancel-pending-submission)
  (mevedel-view--stop-request-progress)
  (when-let* ((data-buf mevedel--data-buffer)
              (_ (buffer-live-p data-buf)))
    (mevedel-view--abort-data-buffer data-buf)))


(provide 'mevedel-view-composer)
;;; mevedel-view-composer.el ends here
