;;; niwa-view-composer.el -- View composer and send orchestration -*- lexical-binding: t -*-

;;; Commentary:

;; Owns editable composer geometry, prompt submission, root dispatch, and
;; fork/send coordination with the authoritative gptel data buffer.
;;
;; The only supported access to the draft is the `niwa-view-composer-'
;; interface: `niwa-view-composer-buffer' and `niwa-view-composer-window'
;; find the editing surface; `niwa-view-composer-draft-start',
;; `niwa-view-composer-draft-end', and `niwa-view-composer-in-draft-p'
;; locate the editable body; `niwa-view-composer-draft-text',
;; `niwa-view-composer-insert', and `niwa-view-composer-replace' read
;; and edit it.  Draft positions belong to the composer buffer, not
;; necessarily to the view.  The inline prompt and its marker are not
;; part of this interface.

;;; Code:

;; Composer dispatch uses runtime sequence functions as well as CL macros.
(require 'cl-lib)
(require 'face-remap)

;; The durable-transaction macro must expand for interpreted loads too,
;; so this is a load-time dependency rather than a compile-time one.
(require 'niwa-session-durability)
(require 'niwa-mention-bindings)
(require 'niwa-overlay-ui)
(require 'niwa-pending-inputs)
(require 'niwa-permission-mode)
(require 'niwa-prompt-submission)
(require 'niwa-skills-input)
(require 'niwa-skills-ui)
(require 'niwa-utilities)
(require 'niwa-view-pair)
(require 'niwa-view-viewport)

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
(defvar gptel-send--handlers)

;; `niwa-agents'
(declare-function niwa-agent-invocation-p "niwa-agents" (cl-x))


;; `niwa-chat'
(declare-function niwa-abort "niwa-chat" (&optional buf))
(declare-function niwa--dispatch-request "niwa-chat" (model-input local-send &optional native-input))
(autoload 'niwa--dispatch-request "niwa-chat")
(defvar niwa--pending-model-input)

;; `niwa-collaboration'
(declare-function niwa-collaboration--safe-accepted-prompt
                  "niwa-collaboration" (data-buffer))
(autoload 'niwa-collaboration--safe-accepted-prompt
  "niwa-collaboration")

;; `niwa-compact-run'
(defvar niwa-compact-run-in-flight nil)

;; `niwa-goal'
(declare-function niwa-goal-start "niwa-goal"
		  (objective &optional prompt-submission))
(autoload 'niwa-goal-start "niwa-goal")

;; `niwa-hooks'
(declare-function niwa-hooks-additional-context-string
		  "niwa-hooks" (decision &optional event))
(declare-function niwa-hooks-event-plist "niwa-hooks"
		  (event &optional session workspace &rest extra))
(declare-function niwa-hooks-format-context "niwa-hooks"
		  (entries))
(declare-function niwa-hooks-record-session-context "niwa-hooks"
		  (session decision &optional event))
(declare-function niwa-hooks-run-event "niwa-hooks"
		  (event event-plist callback &optional session
			 workspace request invocation))
(declare-function niwa-hooks-sanitize-final-decision
                  "niwa-hooks" (event decision &optional scope))
(autoload 'niwa-hooks-event-plist "niwa-hooks")

;; `niwa-implementation-policy'
(declare-function niwa-implementation-policy-assert-preparation
                  "niwa-implementation-policy" (preparation))
(declare-function niwa-implementation-policy-capture-preparation
                  "niwa-implementation-policy" ())
(declare-function niwa-implementation-policy-pause-failure
                  "niwa-implementation-policy" (buffer problem &optional preparation))
(autoload 'niwa-implementation-policy-assert-preparation
  "niwa-implementation-policy")
(autoload 'niwa-implementation-policy-capture-preparation
  "niwa-implementation-policy")
(autoload 'niwa-implementation-policy-pause-failure
  "niwa-implementation-policy")

;; `niwa-instruction-registry'
(declare-function niwa--instruction-buffer-workspace
                  "niwa-instruction-registry" (buffer))

;; `niwa-mention-bindings'
(declare-function niwa-mention-bindings-copy-text
		  "niwa-mention-bindings" (text))
(declare-function niwa-mention-bindings-invalidate-edit
		  "niwa-mention-bindings"
		  (start end minimum maximum))
(declare-function niwa-mention-bindings-ranges
		  "niwa-mention-bindings" (text))
(declare-function niwa-mention-bindings-set
		  "niwa-mention-bindings"
		  (start end binding &optional object))

;; `niwa-mentions'
(declare-function niwa-mentions-expand-user-input
			  "niwa-mentions" (text session &optional fresh-p model))
(declare-function niwa-mentions-file-paths-in-text
                  "niwa-mentions" (text))
(declare-function niwa-mentions-file-token "niwa-mentions"
		  (path))
(declare-function niwa-mentions-install "niwa-mentions" nil)
(declare-function niwa-mentions-prepare-user-input
		  "niwa-mentions" (text &optional session))
(autoload 'niwa-mentions-file-paths-in-text "niwa-mentions")
(autoload 'niwa-mentions-install "niwa-mentions")
(defvar niwa-mentions-agent-enabled-p)

;; `niwa-menu'
(declare-function niwa-menu "niwa-menu" nil)

;; `niwa-overlays'
(declare-function niwa--topmost-instruction
                  "niwa-overlays" (instruction &optional of-type pred))
(autoload 'niwa--topmost-instruction "niwa-overlays")

;; `niwa-pending-inputs'
(declare-function niwa-pending-inputs-cancel-resume
                  "niwa-pending-inputs" ())
(declare-function niwa-view--queue-prepared-steering
                  "niwa-pending-inputs"
                  (submission request &optional submitted-draft))
(defvar niwa-view--pending-guest-attribution)
(defvar niwa-view--pending-input-edit)
(defvar niwa-view--turn-guest-attribution)

;; `niwa-permission-mode'
(declare-function niwa-permission-mode-effective
                  "niwa-permission-mode"
                  (&optional session data-buffer surface-buffer))
(declare-function niwa-permission-mode-label "niwa-permission-mode"
                  (&optional mode))
(declare-function niwa-permission-mode-transition
                  "niwa-permission-mode" (mode))
(defvar niwa-permission-mode)

;; `niwa-permission-queue'
(declare-function niwa-permission-absence-clear
                  "niwa-permission-queue" (session))
(autoload 'niwa-permission-absence-clear "niwa-permission-queue")

;; `niwa-plan-handoff'
(declare-function niwa-plan-handoff-reserved-goal-id
		  "niwa-plan-handoff" (&optional session))
(autoload 'niwa-plan-handoff-reserved-goal-id "niwa-plan-handoff")

;; `niwa-plan-mode'
(declare-function niwa-plan-mode--invalidate-proposal
		  "niwa-plan-mode" (&optional session))
(declare-function niwa-plan-mode-enter "niwa-plan-mode"
                  (&optional session))
(declare-function niwa-plan-mode-exit "niwa-plan-mode"
                  (&optional session))
(autoload 'niwa-plan-mode-enter "niwa-plan-mode" nil t)
(autoload 'niwa-plan-mode-exit "niwa-plan-mode" nil t)

;; `niwa-prompt-submission'
(declare-function niwa-prompt-submission-accept
		  "niwa-prompt-submission"
		  (submission input context audits context-entries))
(declare-function niwa-prompt-submission-audits
		  "niwa-prompt-submission" (cl-x) t)
(declare-function niwa-prompt-submission-cancel
		  "niwa-prompt-submission" (submission))
(declare-function niwa-prompt-submission-commit
		  "niwa-prompt-submission" (submission))
(declare-function niwa-prompt-submission-context
		  "niwa-prompt-submission" (cl-x) t)
(declare-function niwa-prompt-submission-create
		  "niwa-prompt-submission" (&rest args))
(declare-function niwa-prompt-submission-display-text
		  "niwa-prompt-submission" (cl-x) t)
(declare-function niwa-prompt-submission-input
		  "niwa-prompt-submission" (cl-x) t)
(declare-function niwa-prompt-submission-outcome
		  "niwa-prompt-submission" (cl-x) t)
(declare-function niwa-prompt-submission-reserve
		  "niwa-prompt-submission" (submission))
(declare-function niwa-prompt-submission-restore
		  "niwa-prompt-submission" (submission))
(declare-function niwa-prompt-submission-set-outcome
		  "niwa-prompt-submission" (submission outcome))

;; `niwa-readiness'
(declare-function niwa-readiness-assert "niwa-readiness" (session &optional backend))
(autoload 'niwa-readiness-assert "niwa-readiness")

;; `niwa-resource-capf'
(declare-function niwa-resource-capf "niwa-resource-capf" ())
(autoload 'niwa-resource-capf "niwa-resource-capf")

;; `niwa-review'
(declare-function niwa-review-command-skill-p "niwa-review"
		  (skill))
(declare-function niwa-review-mark-command-outcome
		  "niwa-review" (outcome))
(declare-function niwa-review-transform-outcome "niwa-review"
		  (skill-name outcome))

;; `niwa-session-artifacts'
(declare-function niwa-session-artifacts-assert-new-mutation-authority
                  "niwa-session-artifacts" (session))
(autoload 'niwa-session-artifacts-assert-new-mutation-authority
  "niwa-session-artifacts")

;; `niwa-session-fork'
(declare-function niwa-session-fork-create
                  "niwa-session-fork" (buffer target type))
(declare-function niwa-session-fork-retarget-worktree-path
                  "niwa-session-fork" (session path))
(autoload 'niwa-session-fork-create "niwa-session-fork")
(autoload 'niwa-session-fork-retarget-worktree-path
  "niwa-session-fork")

;; `niwa-session-naming'
(declare-function niwa-session-naming-consider "niwa-session-naming" (session prompt))
(autoload 'niwa-session-naming-consider "niwa-session-naming")

;; `niwa-session-persistence'
(defvar niwa-session--read-only-mode)

;; `niwa-session-rewind'
(declare-function niwa-session-rewind-assert-stable-source
                  "niwa-session-rewind"
                  (session buffer operation))
(autoload 'niwa-session-rewind-assert-stable-source
  "niwa-session-rewind")

;; `niwa-side-conversation'
(declare-function niwa-side-conversation-close "niwa-side-conversation" ())
(autoload 'niwa-side-conversation-close "niwa-side-conversation" nil t)
(declare-function niwa-side-conversation-send
                  "niwa-side-conversation" ())
(autoload 'niwa-side-conversation-send "niwa-side-conversation" nil t)

;; `niwa-skills-core'
(declare-function niwa-session-get-skill "niwa-skills-core"
		  (session name))
(declare-function niwa-skill-name "niwa-skills-core" (cl-x) t)
(declare-function niwa-skill-user-invocable-p "niwa-skills-core"
		  (cl-x) t)

;; `niwa-skills-input'
(declare-function niwa-skills-input-insert-fork-result
                  "niwa-skills-input" (outcome))
(declare-function niwa-skills-input-parse-skill-line
                  "niwa-skills-input" (text))
(declare-function niwa-skills-input-prepare-user-input
                  "niwa-skills-input" (text session))
(declare-function niwa-skills-input-refresh-bound-input
                  "niwa-skills-input" (text session))

;; `niwa-skills-invoke'
(declare-function niwa-skills-commit-invoked-records
		  "niwa-skills-invoke" (session records))
(declare-function niwa-skills-dispatch-prepared-fork
		  "niwa-skills-invoke" t t)
(declare-function niwa-skills-request-model-policy "niwa-skills-invoke" ())
(defvar niwa-skills--pending-request-context)
(autoload 'niwa-skills-request-model-policy "niwa-skills-invoke")

;; `niwa-skills-plan'
(declare-function niwa-skill-invocation-plan-fork-p
		  "niwa-skills-plan" (cl-x) t)
(declare-function niwa-skill-invocation-plan-occurrences
		  "niwa-skills-plan" (cl-x) t)
(declare-function niwa-skill-invocation-plan-text
                  "niwa-skills-plan" (cl-x) t)
(declare-function niwa-skills-plan-prepare "niwa-skills-plan"
		  (plan callback &optional cancelled-p))
(declare-function niwa-skills-plan-render-data
		  "niwa-skills-plan" (plan prepared))
(declare-function niwa-skills-plan-user-input "niwa-skills-plan"
		  (text session &optional selected-skills))
(autoload 'niwa-skills-plan-user-input "niwa-skills-plan")
(autoload 'niwa-skills-plan-prepare "niwa-skills-plan")
(autoload 'niwa-skills-plan-render-data "niwa-skills-plan")

;; `niwa-skills-ui'
(declare-function niwa-skills-install-font-lock "niwa-skills-ui"
                  (&optional origin-function))
(declare-function niwa-skills-local-command-active-request-p
		  "niwa-skills-ui" (name args))
(declare-function niwa-skills-parse-slash-line "niwa-skills-ui"
		  (text))
(declare-function niwa-skills-remaining-argument-hint
		  "niwa-skills-ui" (skill arguments))
(declare-function niwa-skills-slash-capf "niwa-skills-ui"
		  (buffer session local-commands &optional input-start))
(declare-function niwa-skills-user-visible-skills
		  "niwa-skills-ui" (session &optional inline-only))
(autoload 'niwa-skills-install-font-lock "niwa-skills-ui")
(autoload 'niwa-skills-user-visible-skills "niwa-skills-ui")
(defvar niwa-slash-commands)

;; `niwa-structs'
(declare-function niwa-goal-id "niwa-structs" (cl-x) t)
(declare-function niwa-goal-status "niwa-structs" (cl-x) t)
(declare-function niwa-request-fsm "niwa-structs" (cl-x) t)
(declare-function niwa-request-id "niwa-structs" (cl-x) t)
(declare-function niwa-session--set-dropped-file-grants
                  "niwa-structs" (session paths))
(declare-function niwa-session-clear-dropped-file-grants
		  "niwa-structs" (session))
(declare-function niwa-session-dropped-file-grants
                  "niwa-structs" (cl-x) t)
(declare-function niwa-session-forked-from-fork-point-id
                  "niwa-structs" (cl-x) t)
(declare-function niwa-session-goal "niwa-structs" (cl-x) t)
(declare-function niwa-session-hook-context-pending
		  "niwa-structs" (cl-x) t)
(declare-function niwa-session-pending-follow-ups
                  "niwa-structs" (cl-x) t)
(declare-function niwa-session-pending-plan-approval
		  "niwa-structs" (cl-x) t)
(declare-function niwa-session-permission-mode "niwa-structs"
		  (cl-x) t)
(declare-function niwa-session-plan-metadata "niwa-structs" (cl-x) t)
(declare-function niwa-session-plan-mode "niwa-structs" (cl-x) t)
(declare-function niwa-session-turn-count "niwa-structs" (cl-x)
		  t)
(declare-function niwa-session-workspace "niwa-structs" (cl-x) t)
(defvar niwa--agent-invocation)
(defvar niwa--current-request)
(defvar niwa--data-buffer)
(defvar niwa--session)
(defvar niwa-view-mode-map)
(defvar niwa-view-zone-owned-properties)
(defvar niwa--view-buffer)
(defvar niwa--workspace)

;; `niwa-tool-render-data'
(declare-function niwa-tool-render-data-blocks
                  "niwa-tool-render-data" (string))
(autoload 'niwa-tool-render-data-blocks "niwa-tool-render-data")

;; `niwa-transcript'
(declare-function niwa-transcript-prompt-transform-start
		  "niwa-transcript" nil)
(autoload 'niwa-transcript-prompt-transform-start "niwa-transcript")

;; `niwa-transcript-audit'
(declare-function niwa--format-hook-audit-record
		  "niwa-transcript-audit" (record))
(autoload 'niwa--format-hook-audit-record "niwa-transcript-audit")

;; `niwa-turn'
(declare-function niwa-request-assert-target-ready
                  "niwa-turn" (session))
(declare-function niwa-request-begin "niwa-turn"
                  (session &optional options))
(declare-function niwa-request-call-owned
                  "niwa-turn" (request buffer function &rest args))
(declare-function niwa-request-end
                  "niwa-turn" (&optional abort-plan-approval))
(declare-function niwa-turn-busy-p "niwa-turn" (&optional buffer))
(autoload 'niwa-request-assert-target-ready "niwa-turn")
(autoload 'niwa-request-begin "niwa-turn")
(autoload 'niwa-request-call-owned "niwa-turn")
(autoload 'niwa-request-end "niwa-turn")
(autoload 'niwa-turn-busy-p "niwa-turn")

;; `niwa-utilities'
(declare-function niwa--insert-user-turn
                  "niwa-utilities" (input))
(declare-function niwa--normalize-message-text "niwa-utilities"
		  (text))
(declare-function niwa-diagnostic-message "niwa-utilities" (scope format-string &rest args))
(declare-function niwa-diagnostic-scope "niwa-utilities" (owner))
(declare-function niwa-diagnostic-user-error "niwa-utilities" (scope format-string &rest args))
(autoload 'niwa--insert-user-turn "niwa-utilities")
(autoload 'niwa--normalize-message-text "niwa-utilities")
(autoload 'niwa-diagnostic-message "niwa-utilities")
(autoload 'niwa-diagnostic-scope "niwa-utilities")
(autoload 'niwa-diagnostic-user-error "niwa-utilities")

;; `niwa-view'
(declare-function niwa-view--abort-data-buffer
                  "niwa-view" (data-buffer))
(autoload 'niwa-view--abort-data-buffer "niwa-view")
(defvar niwa-view--side-conversation-p)

;; `niwa-view-audit'
(declare-function niwa-view--prompt-rewrite-audit-record
		  "niwa-view-audit"
		  (event original submitted decision))

;; `niwa-view-disclosure'
(declare-function niwa-view-disclosure-source-range
                  "niwa-view-disclosure" (data-buffer start end))
(declare-function niwa-view-disclosure-source-start
                  "niwa-view-disclosure" (source))

;; `niwa-view-fontify'
(declare-function niwa-view--fontify-markdown-blocks "niwa-view-fontify"
                  (text from to))

;; `niwa-view-history'
(declare-function niwa-view-history-add "niwa-view-history"
		  (input))
(declare-function niwa-view-history-beginning-of-line
		  "niwa-view-history" (&optional arg))
(declare-function niwa-view-history-browse "niwa-view-history" ())
(declare-function niwa-view-history-clear-input
		  "niwa-view-history" ())
(declare-function niwa-view-history-load "niwa-view-history"
		  (&optional session))
(declare-function niwa-view-history-next "niwa-view-history" ())
(declare-function niwa-view-history-previous "niwa-view-history" ())
(declare-function niwa-view-history-search "niwa-view-history" ())
(autoload 'niwa-view-history-load "niwa-view-history")

;; `niwa-view-input-files'
(declare-function niwa-view-yank-dwim "niwa-view-input-files" (&optional arg))
(declare-function niwa-view--activate-dropped-file-grants
                  "niwa-view-input-files" (paths session))
(declare-function niwa-view--install-dnd
                  "niwa-view-input-files" ())
(declare-function niwa-view--pop-dropped-file-grants-for-input
                  "niwa-view-input-files" (input session))
(autoload 'niwa-view--install-dnd "niwa-view-input-files")

;; `niwa-view-interaction'
(declare-function niwa-view--interaction-rebuild
		  "niwa-view-interaction" nil)
(declare-function niwa-view--interaction-register
		  "niwa-view-interaction" (descriptor))
(declare-function niwa-view--interaction-unregister
		  "niwa-view-interaction" (id))

;; `niwa-view-render'
(declare-function niwa-view--append-request-summary
                  "niwa-view-render" (data-buf search-start &optional extra))
(declare-function niwa-view-rerender "niwa-view" (&optional buffer))
(declare-function niwa-view--history-insertion-marker
                  "niwa-view-render" ())
(declare-function niwa-view--hook-context-events-from-text
                  "niwa-view-render" (text))
(declare-function niwa-view--inline-skill-prompt-summary-body
                  "niwa-view-render" (render-data))
(declare-function niwa-view--insert-rendered-tool
                  "niwa-view-render" (rendering source))
(declare-function niwa-view--insert-user-message
                  "niwa-view-render"
                  (text &optional kind hook-context prompt-summary-body
                        prompt-summary-source hook-audits attribution user-source))
(declare-function niwa-view-fork-point-at-point
                  "niwa-view-render" ())
(autoload 'niwa-view--append-request-summary "niwa-view-render")
(defvar niwa-view--display-map)

;; `niwa-view-segments'
(declare-function niwa-view-historical-segment-p
                  "niwa-view-segments" ())
(autoload 'niwa-view-historical-segment-p "niwa-view-segments")

;; `niwa-view-stream'
(declare-function niwa-view--render-request-progress
                  "niwa-view-stream" ())
(declare-function niwa-view-stream-composer-window-change
                  "niwa-view-stream" (window &optional start))
(autoload 'niwa-view-stream-composer-window-change "niwa-view-stream")
(declare-function niwa-view--stop-request-progress
                  "niwa-view-stream" ())
(declare-function niwa-view-stream-begin-turn
                  "niwa-view-stream"
                  (view-turn-start data-turn-start &optional no-spinner))
(declare-function niwa-view-stream-stop "niwa-view-stream" ())
(defvar niwa-view--data-turn-start)
(defvar niwa-view--in-flight-turn-start)
(defvar niwa-view--rail-attention-map)

;; `niwa-worktree'
(declare-function niwa-worktree-fork-preflight
		  "niwa-worktree" (session))
(declare-function niwa-worktree-fork-reservation
		  "niwa-worktree" (session &optional preflight))
(autoload 'niwa-worktree-fork-preflight "niwa-worktree")
(autoload 'niwa-worktree-fork-reservation "niwa-worktree")

;;
;;; Input prompt

(defconst niwa-view--input-prompt "> "
  "Read-only prefix rendered at the start of the input zone.")

(defun niwa-view--propertize-input-prompt (start end)
  "Mark START..END as the read-only input prompt.
The prompt is its own field, so line and word motion stop at the input
start instead of entering the prompt.  The caret cannot rest in it: the
draft starts where the prompt ends."
  (add-text-properties
   start end
   '(read-only t
     niwa-view-prompt t
     field niwa-view-prompt
     cursor-intangible t
     front-sticky (read-only niwa-view-prompt cursor-intangible)
     rear-nonsticky (read-only niwa-view-prompt field font-lock-face cursor-intangible))))

(defface niwa-view-note-scope
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face for note frames."
  :group 'niwa)


(defun niwa-view--effective-permission-mode ()
  "Return the permission mode to apply to the current view buffer."
  (niwa-permission-mode-effective
   (and (boundp 'niwa--session) niwa--session)
   (and (boundp 'niwa--data-buffer)
        (buffer-live-p niwa--data-buffer)
        niwa--data-buffer)
   (current-buffer)))

(defun niwa-view--permission-mode-display (mode)
  "Return (LABEL FACE) for permission MODE."
  (list
   (niwa-permission-mode-label mode)
   (pcase mode
     ('edits 'niwa-view-permission-mode-edits)
     ('full-auto 'niwa-view-permission-mode-full-auto)
     (_ 'niwa-view-permission-mode-ask))))

(defun niwa-view--plan-mode-p ()
  "Return non-nil when the current view's session is in Plan mode."
  (and (boundp 'niwa--session)
       niwa--session
       (niwa-session-plan-mode niwa--session)))

(defconst niwa-view--permission-mode-cycle
  '(ask edits full-auto)
  "Permission states cycled by `niwa-view-cycle-permission-mode'.")

(defun niwa-view--next-permission-mode (&optional mode)
  "Return the permission mode after MODE in the view cycle.
Nil and unknown modes are treated as `ask'."
  (let* ((current (if (memq mode niwa-view--permission-mode-cycle)
                      mode
                    'ask))
         (tail (cdr (memq current niwa-view--permission-mode-cycle))))
    (or (car tail)
        (car niwa-view--permission-mode-cycle))))

(defun niwa-view--composer-mode-badge (&optional mode)
  "Return the composer's mode badge for the permanent rail.
MODE defaults to the effective permission mode.  Editing and Plan retain
their distinct labels."
  (pcase-let* ((`(,label ,face)
                (niwa-view--permission-mode-display
                 (or mode (niwa-view--effective-permission-mode)))))
    (cond
     (niwa-view--pending-input-edit
      (propertize
       (format "[Editing %s]"
               (plist-get niwa-view--pending-input-edit :label))
       'font-lock-face 'niwa-view-input-prompt))
     ((niwa-view--plan-mode-p)
      (propertize (format "[Plan · %s]" label)
                  'font-lock-face 'niwa-view-plan-mode))
     (t (propertize (format "[%s]" label) 'font-lock-face face)))))

(defvar-local niwa-view--input-hint nil
  "Cached hint line above the composer prompt, or nil.")

(defun niwa-view--input-hint-display ()
  "Return the hint and first prompt glyph for the fixed prompt cell."
  (when niwa-view--input-hint
    (concat niwa-view--input-hint "\n"
            (propertize (substring niwa-view--input-prompt 0 1)
                        'face 'niwa-view-input-prompt))))

(defun niwa-view--input-prompt-string ()
  "Return the read-only input prompt below the permanent composer rail."
  (let ((prompt (propertize niwa-view--input-prompt
                            'font-lock-face 'niwa-view-input-prompt)))
    (when-let* ((hint (niwa-view--input-hint-display)))
      (put-text-property 0 1 'display hint prompt))
    prompt))


;;
;;; Composer state and redraw preservation

(defvar-local niwa-view--composer-buffer nil
  "The buffer editing this view's draft.")

(defvar-local niwa-composer--view nil
  "The transcript that owns this composer.")

(defvar-local niwa-composer--draft-start nil
  "Marker after the composer's prompt prefix.")

(defvar-local niwa-composer--emulation-maps nil
  "Draft bindings that outrank ordinary minor modes in the composer.")

(defvar-local niwa-composer--fit-timer nil
  "Deferred fit for this draft's displayed docks.")

(defface niwa-composer-rule
  '((t :inherit (shadow default) :box nil :underline nil :overline t))
  "Rule across the top of the docked composer."
  :group 'niwa)

(defface niwa-composer-back-to-bottom
  '((t :inherit (shadow mode-line-inactive) :box nil))
  "Button for returning to the end of the paired transcript."
  :group 'niwa)

(defun niwa-composer-back-to-bottom (event)
  "Show the end of the transcript paired with EVENT's composer window."
  (interactive "e")
  (when-let* ((dock (posn-window (event-start event)))
              ((window-live-p dock))
              ((eq (niwa-view-composer-window dock) dock))
              (transcript (niwa-view-transcript-window dock)))
    (niwa-view-transcript-show-end transcript)
    (with-current-buffer (window-buffer dock)
      (force-mode-line-update))))

(defconst niwa-composer--bottom-button
  (propertize " ↓ Back to bottom "
              'face 'niwa-composer-back-to-bottom
              'mouse-face 'highlight
              'help-echo "mouse-1: show the transcript end"
              'local-map (define-keymap
                           "<mouse-1>" #'niwa-composer-back-to-bottom
                           "<header-line> <mouse-1>" #'niwa-composer-back-to-bottom))
  "Right-aligned button used when the paired transcript is scrolled up.")

(defvar-local niwa-composer--attention nil
  "Actionable count label last supplied by this composer's rail.")

(defvar-local niwa-composer--attention-button nil
  "Cached header button for the pending count when the rail is hidden.")

(defun niwa-composer-set-attention (attention)
  "Cache ATTENTION and its header button during a semantic rail update."
  (unless (equal attention niwa-composer--attention)
    (setq niwa-composer--attention attention
          niwa-composer--attention-button
          (when attention
            (propertize (concat " ⚠ " attention " ")
                        'face 'warning
                        'mouse-face 'highlight
                        'help-echo "mouse-1 or C-c C-j: visit the next pending card"
                        'local-map niwa-view--rail-attention-map)))))

(defvar niwa-view-table--font-generation)
(defvar niwa-view-table--fontset-generation)

(defun niwa-composer--hidden-label (window)
  "Return a cached label for draft screen lines hidden above WINDOW.
`niwa-composer--forget-hidden' drops the count when an edit lands above
the window start; edits below it cannot change the count."
  (let ((start (window-start window))
        (draft (and niwa-composer--draft-start
                    (marker-position niwa-composer--draft-start))))
    (if (and start draft (> start draft))
        (let* ((cache (window-parameter window 'niwa-composer-hidden))
               (key (list (current-buffer) start
                          (window-body-width window t) (window-hscroll window)
                          face-remapping-alist
                          (bound-and-true-p niwa-view-table--font-generation)
                          (bound-and-true-p niwa-view-table--fontset-generation))))
          (if (equal key (car cache))
              (cdr cache)
            (let* ((prompt (save-excursion
                             (goto-char draft)
                             (line-beginning-position)))
                   (count (count-screen-lines prompt start nil window))
                   (label (if (zerop count) ""
                            (propertize (format " ↑ %d more " count) 'face 'shadow))))
              (set-window-parameter window 'niwa-composer-hidden
                                    (cons (copy-tree key) label))
              label)))
      "")))

(defun niwa-composer--forget-hidden (beg _end _length)
  "Drop hidden-line counts of windows whose start follows an edit at BEG."
  (dolist (window (get-buffer-window-list nil nil t))
    (when (> (window-start window) beg)
      (set-window-parameter window 'niwa-composer-hidden nil))))

(defun niwa-composer-invalidate-transcript-end (window)
  "Discard the header's view of WINDOW before native transcript redisplay."
  (set-window-parameter window 'niwa-composer-transcript-end nil))

(defun niwa-composer--transcript-end-visible-p (window)
  "Reuse WINDOW's exact end visibility across header formats until it changes."
  (with-current-buffer (window-buffer window)
    (let* ((key (list (current-buffer) (buffer-modified-tick) (point-min) (point-max)
                      (window-start window) (window-vscroll window t) (window-hscroll window)
                      (window-body-width window t) (window-pixel-height window)))
           (cached (window-parameter window 'niwa-composer-transcript-end)))
      (if (equal key (car cached))
          (cdr cached)
        (let ((visible (niwa-view-transcript-end-visible-p window)))
          (set-window-parameter window 'niwa-composer-transcript-end (cons key visible))
          visible)))))

(defun niwa-composer--top-line ()
  "Draw the separator and the buttons and counts for this window."
  (let* ((window (selected-window))
         (pending (when-let* ((button niwa-composer--attention-button)
                              (start (window-start window))
                              ((> start (point-min))))
                    button))
         (more (niwa-composer--hidden-label window))
         (transcript (niwa-view-transcript-window window))
         (button (if (and transcript (not (niwa-composer--transcript-end-visible-p transcript)))
                     niwa-composer--bottom-button ""))
         (button-width (string-width button))
         (graphic (display-graphic-p))
         (line (concat pending more
                       (if graphic
                           (propertize " " 'display `(space :align-to (- right ,button-width)))
                         (make-string (max 0 (- (window-total-width window) (string-width more)
                                                button-width (if pending (string-width pending) 0))) ?─))
                       button)))
    (if (not graphic) line
      (let* ((frame (window-frame window))
             (overline (face-attribute 'niwa-composer-rule :overline frame t)))
        ;; An overline of t uses each label's text color, not the rule's.
        (when (eq overline t)
          (add-face-text-property
           0 (length line) (list :overline (face-foreground 'niwa-composer-rule frame t))
           nil line))
        ;; Emacs paints header lines across the body fringe.
        (concat line (propertize " " 'display '(space :align-to right-fringe))
                (propertize " " 'face 'fringe 'display '(space :width right-fringe)))))))

(defun niwa-composer-refresh-transcript-position (transcript _start)
  "Refresh the dock's jump button after TRANSCRIPT scrolls."
  (when-let* ((dock (niwa-view-composer-window transcript)))
    (with-current-buffer (window-buffer dock)
      (force-mode-line-update))))

(defun niwa-view-composer-height (draft pair chrome line)
  "Fit DRAFT inside a third of PAIR, keeping CHROME and at least one LINE.
All heights use the same units; CHROME includes the rail.  A tiny pair
can exceed the cap to keep the dock usable."
  (max (+ chrome line) (min (+ chrome draft) (/ pair 3))))

(defvar-local niwa-composer--fit-edits 0
  "Count edits observed by the fit hook, excluding silent rail updates.")

(defun niwa-composer--fit-key (dock transcript)
  "Identify the current draft and layout of DOCK and TRANSCRIPT."
  (list (current-buffer) (buffer-chars-modified-tick) niwa-composer--fit-edits
        (copy-tree face-remapping-alist)
        (frame-char-height (window-frame dock)) (window-body-width dock t)
        (window-pixel-height dock) (window-pixel-height transcript)
        (window-scroll-bar-height dock) (window-bottom-divider-width dock)))

(defun niwa-composer--rail-clipped-p (dock start)
  "Return non-nil if DOCK can show the whole draft but START clips its rail."
  (and (window-parameter dock 'niwa-composer--rail-fits)
       (or (> start (point-min)) (> (window-vscroll dock t) 0))))

(defun niwa-composer--restore-rail (dock)
  "Show DOCK's rail again if the whole draft fits."
  (when (niwa-composer--rail-clipped-p dock (window-start dock))
    (set-window-vscroll dock 0 t)
    (set-window-start dock (point-min))))

(defun niwa-composer--fit (dock transcript &optional fresh)
  "Fit DOCK to its draft at its current width, taking space from TRANSCRIPT.
Use FRESH chrome geometry when fitting before redisplay."
  (let* ((height (window-pixel-height dock))
         (line (frame-char-height (window-frame dock)))
         (pair (+ height (window-pixel-height transcript)))
         (frame-chrome (- height (if fresh
                                    (cdr (niwa-view--window-body-geometry dock))
                                  (window-body-height dock t))))
         (chrome (+ frame-chrome line))
         ;; Measure one extra row to distinguish a full body from a capped one.
         (limit (+ line (max (* 2 line) (- (/ pair 3) frame-chrome))))
         (text (cdr (window-text-pixel-size dock (point-min) (point-max) nil limit)))
         (target (niwa-view-composer-height (- text line) pair chrome line))
         (delta (- target height)))
    (unless (zerop delta)
      (let ((follow (niwa-view-transcript-end-visible-p transcript))
            (window-min-height 1)
            (window-resize-pixelwise t)
            (window-combination-resize nil))
        (window-preserve-size dock nil nil)
        (unwind-protect
            (let ((step (window-resizable dock delta nil nil t)))
              (unless (zerop step)
                (window-resize dock step nil nil t)))
          (window-preserve-size dock nil t))
        (when follow (niwa-view-transcript-show-end transcript))))
    (set-window-parameter dock 'niwa-composer--rail-fits
                          (<= text (- (window-pixel-height dock) frame-chrome)))
    (niwa-composer--restore-rail dock)
    (set-window-parameter dock 'niwa-composer--fit-key
                          (niwa-composer--fit-key dock transcript))))

(defun niwa-composer--fit-displayed (buffer)
  "Fit BUFFER's surviving docks after edit and window hooks finish."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq niwa-composer--fit-timer nil)
      (dolist (dock (get-buffer-window-list buffer nil t))
        (when-let* ((transcript (niwa-view-transcript-window dock)))
          ;; Our own layout notifies redisplay too, sometimes with stale chrome.
          (if (equal (window-parameter dock 'niwa-composer--fit-key)
                     (niwa-composer--fit-key dock transcript))
              (niwa-composer--restore-rail dock)
            (niwa-composer--fit dock transcript)))))))

(defun niwa-composer--queue-fit (&optional _beg end _length)
  "Coalesce edits and size changes into one fit outside redisplay.
END distinguishes a draft edit from a one-argument window notification."
  (when end (cl-incf niwa-composer--fit-edits))
  (unless (or noninteractive niwa-composer--fit-timer)
    (setq niwa-composer--fit-timer
          (run-at-time 0 nil #'niwa-composer--fit-displayed (current-buffer)))))

(defun niwa-composer--queue-rail-restore (dock start)
  "Defer restoring DOCK's rail after a scroll to START."
  (when (niwa-composer--rail-clipped-p dock start)
    (niwa-composer--queue-fit)))

(defun niwa-composer--reveal-rail ()
  "Show the rail after a command moved the caret above the draft.
`cursor-intangible-mode' then returns the caret to the draft start."
  (when (and niwa-composer--draft-start
             (< (point) niwa-composer--draft-start)
             (eq (window-buffer) (current-buffer)))
    (set-window-start nil (point-min) t)))

(declare-function adaptive-wrap-prefix-mode "ext:adaptive-wrap" (&optional arg))

(defun niwa-composer--refuse-adaptive-wrap ()
  "Keep `adaptive-wrap-prefix-mode' off; the composer owns its wrap prefixes.
A user's `text-mode-hook' may enable it, and its fontifier signals on the
rail's displayed first line besides overwriting the prompt's prefix."
  (when (bound-and-true-p adaptive-wrap-prefix-mode)
    (adaptive-wrap-prefix-mode -1)))

(define-derived-mode niwa-composer-mode text-mode "Composer"
  "Edit the draft of a niwa conversation."
  (visual-line-mode 1)
  (add-hook 'kill-buffer-query-functions #'niwa-composer--request-close nil t)
  ;; Parent mode hooks run after this body, so refuse from the mode's hook.
  (add-hook 'adaptive-wrap-prefix-mode-hook
            #'niwa-composer--refuse-adaptive-wrap nil t)
  ;; Geometry queries can evaluate the header without selecting its window.
  (setq-local header-line-format
              '(:eval (when (eq (window-buffer) (current-buffer))
                        (niwa-composer--top-line))))
  (dolist (face '(header-line header-line-inactive))
    (face-remap-add-relative face 'niwa-composer-rule))
  (add-hook 'window-buffer-change-functions #'niwa-view-stream-composer-window-change nil t)
  (add-hook 'window-size-change-functions #'niwa-view-stream-composer-window-change nil t)
  (add-hook 'window-scroll-functions #'niwa-view-stream-composer-window-change nil t)
  (add-hook 'window-scroll-functions #'niwa-composer--queue-rail-restore nil t)
  (add-hook 'window-buffer-change-functions #'niwa-composer--queue-fit nil t)
  (add-hook 'window-size-change-functions #'niwa-composer--queue-fit nil t)
  (add-hook 'after-change-functions #'niwa-composer--queue-fit nil t)
  (add-hook 'after-change-functions #'niwa-composer--forget-hidden nil t)
  ;; Motion above the draft, such as C-p or M-<, stops at the draft start.
  (cursor-intangible-mode 1)
  (add-hook 'post-command-hook #'niwa-composer--reveal-rail nil t)
  (unless (eq yank-excluded-properties t)
    (setq-local yank-excluded-properties
                (append niwa-view-zone-owned-properties
                        ;; Copied prompt text must not become unreachable draft.
                        '(niwa-view-interaction-id cursor-intangible)
                        yank-excluded-properties)))
  (setq-local scroll-conservatively 101
              scroll-margin 0
              other-window-scroll-default #'niwa-view-transcript-window))

(defvar niwa-view-composer--releasing nil
  "The draft buffer a view is releasing right now, or nil.")

(defun niwa-view-composer-kill ()
  "Release the current view's dock, then kill its composer.
View kill and major-mode change both come here, so pair teardown does not
depend on the composer's own kill hook running first."
  (when (buffer-live-p niwa-view--composer-buffer)
    (niwa-view-pair-hide (current-buffer))
    (let ((niwa-view-composer--releasing niwa-view--composer-buffer))
      (kill-buffer niwa-view--composer-buffer))))

(defun niwa-composer--request-close ()
  "Turn a user's kill of this draft into a close request for its view.
The view applies its normal close policy once and, if it accepts, kills the
draft itself.  Return non-nil only when the view is already gone or is
releasing this draft."
  (or (eq niwa-view-composer--releasing (current-buffer))
      (not (buffer-live-p niwa-composer--view))
      (progn (kill-buffer niwa-composer--view) nil)))

(defun niwa-view-composer-owner ()
  "Return the current composer's transcript, or the current buffer."
  (or niwa-composer--view (current-buffer)))

(defun niwa-view-composer-create ()
  "Create the current editable view's composer without displaying it."
  (unless niwa-view--agent-transcript-p
    (let ((view (current-buffer))
          (data niwa--data-buffer)
          (side niwa-view--side-conversation-p)
          (directory default-directory))
      (setq niwa-view--composer-buffer
            (generate-new-buffer (format "%s composer" (buffer-name))))
      (unless side (niwa-view-history-load niwa--session))
      (add-hook 'kill-buffer-hook #'niwa-view--cancel-pending-submission nil t)
      (with-current-buffer niwa-view--composer-buffer
        (niwa-composer-mode)
        ;; Text-mode and global hooks may have enabled numbers during setup.
        (when (bound-and-true-p display-line-numbers-mode)
          (display-line-numbers-mode -1))
        (setq-local display-line-numbers nil)
        (setq-local niwa-composer--view view
                    niwa--data-buffer data
                    niwa-view--side-conversation-p side
                    default-directory directory)
        ;; Display cells keep draft undo positions stable when the rail changes.
        (insert (propertize "   " 'display "") "\n")
        (add-text-properties (point-min) (point)
                             '(read-only t cursor-intangible t
                               front-sticky (read-only cursor-intangible)
                               rear-nonsticky t))
        (let ((start (point)))
          (insert (niwa-view--input-prompt-string))
          (niwa-view--propertize-input-prompt start (point)))
        (setq niwa-composer--draft-start (point-marker))
        (setq buffer-undo-list nil)
        (niwa-view-composer-initialize))
      (niwa-view-pair-watch niwa-view--composer-buffer))))

(defun niwa-view-composer-buffer (&optional view)
  "Return the buffer holding VIEW's draft, or nil if it has no composer.
VIEW defaults to the current buffer.  This does not display the composer."
  (let ((view (or view (current-buffer))))
    (when (buffer-live-p view)
      (with-current-buffer view
        (cond
         ((buffer-live-p niwa-view--composer-buffer)
          niwa-view--composer-buffer)
         ((buffer-live-p niwa-composer--view) view))))))

(defvar-local niwa-view--armed-session-fork nil
  "Stable fork-point target armed for the next model-bound submission.")

(defvar-local niwa-view--armed-session-fork-return-point nil
  "View position to restore when cancelling a historical session fork.")

(defvar-local niwa-view--armed-session-fork-overlay nil
  "Overlay dimming the turns an armed session fork leaves out.")

(defun niwa-view-composer-session-fork-armed-p ()
  "Return non-nil when this view has an armed historical session fork."
  (and niwa-view--armed-session-fork t))

(defun niwa-view-composer-set-historical-visible (visible)
  "Show the live composer when VISIBLE, otherwise hide and lock it."
  (unless niwa-view--agent-transcript-p
    (setq buffer-read-only (not visible)))
  (when-let* ((composer (niwa-view-composer-buffer)))
    (let ((view (current-buffer)))
      (with-current-buffer composer
        (setq buffer-read-only (not visible)))
      (if visible
          (dolist (window (get-buffer-window-list view nil t))
            (niwa-view-composer-display window))
        (niwa-view-pair-hide view)))))

(defvar-keymap niwa-view--armed-session-fork-map
  :doc "Keymap for the armed session-fork interaction row."
  "RET" #'niwa-view-cancel-session-fork
  "<mouse-1>" #'niwa-view-cancel-session-fork)

(defun niwa-view-cancel-session-fork (&optional _event)
  "Disarm the pending session fork without changing the composer draft."
  (interactive)
  (when niwa-view--armed-session-fork
    (setq niwa-view--armed-session-fork nil)
    (niwa-view--interaction-unregister 'armed-session-fork)
    (niwa-view--dim-unforked-turns)
    (when (niwa-view-historical-segment-p)
      (niwa-view-composer-set-historical-visible nil)
      (when niwa-view--armed-session-fork-return-point
        (goto-char
         (min niwa-view--armed-session-fork-return-point
              (niwa-view-transcript-tail)))))
    (setq niwa-view--armed-session-fork-return-point nil)
    t))

(defun niwa-view--dim-unforked-turns ()
  "Dim the rendered turns after the armed session fork's response.
Redraws replace the transcript text, so the armed row's interaction render
calls this again to place the overlay on the new text."
  (when (overlayp niwa-view--armed-session-fork-overlay)
    (delete-overlay niwa-view--armed-session-fork-overlay)
    (setq niwa-view--armed-session-fork-overlay nil))
  (when-let* ((cutoff (plist-get niwa-view--armed-session-fork
                                 :transcript-cutoff)))
    (let ((pos (point-min))
          (limit (niwa-view-transcript-tail))
          start end)
      (while (< pos limit)
        (let ((next (or (next-single-property-change
                         pos 'niwa-view-turn-id nil limit)
                        limit)))
          (when (get-text-property pos 'niwa-view-turn-id)
            (when-let* (((not start))
                        (source-start
                         (niwa-view-disclosure-source-start
                          (get-text-property pos 'niwa-view-source)))
                        ((>= source-start cutoff)))
              (setq start pos))
            (setq end next))
          (setq pos next)))
      (when start
        (setq niwa-view--armed-session-fork-overlay
              (make-overlay start end))
        ;; Header rules are `:extend' overlines; the dim must extend too,
        ;; or the rule past the header text keeps its color.
        (overlay-put niwa-view--armed-session-fork-overlay
                     'face '(:inherit shadow :extend t))))))

(defun niwa-view-cancel-composer-state ()
  "Cancel the active composer edit or fork."
  (interactive)
  (with-current-buffer (niwa-view-composer-owner)
    (cond
     (niwa-view--armed-session-fork
      (niwa-view-cancel-session-fork))
     (niwa-view--pending-input-edit
      (niwa-pending-inputs-cancel-edit)))))

(defun niwa-view--arm-session-fork (fork-type)
  "Arm FORK-TYPE from the settled assistant response at point."
  (niwa-view--ensure-interactive-chat-view)
  (let* ((target (niwa-view-fork-point-at-point))
         (session (niwa-view--session))
         (label (if (eq fork-type 'worktree) "worktree" "conversation"))
         reservation)
    (when (equal
           (plist-get target :fork-point-id)
           (niwa-session-forked-from-fork-point-id session))
      (niwa-diagnostic-user-error (niwa-diagnostic-scope (current-buffer))
       "Fork the inherited response from Source; switch variants first"))
    (niwa-session-rewind-assert-stable-source
     session niwa--data-buffer "forking")
    (when (eq fork-type 'worktree)
      (let ((preflight (niwa-worktree-fork-preflight session)))
        (setq reservation
              (niwa-worktree-fork-reservation session preflight))))
    (setq target (copy-sequence target))
    (plist-put target :fork-type fork-type)
    (when reservation
      (plist-put target :worktree-reservation reservation))
    (when (niwa-view-historical-segment-p)
      (setq niwa-view--armed-session-fork-return-point (point))
      (niwa-view-composer-set-historical-visible t))
    (setq niwa-view--armed-session-fork target)
    (niwa-view--interaction-register
     (list :kind 'preview
           :id 'armed-session-fork
           :active-work-paused nil
           :body
           (format "Fork %s from Assistant turn %d  [Cancel]"
                   label (plist-get target :cum-turn))
           :keymap niwa-view--armed-session-fork-map
           :help-echo (format "Cancel %s Fork"
                              (capitalize label))
           :after-render #'niwa-view--dim-unforked-turns))
    (goto-char (point-max))))

(defun niwa-view--assert-live-tip (&optional allow-armed-fork)
  "Refuse live-tip actions during historical inspection.
ALLOW-ARMED-FORK permits submission of an already armed session fork."
  (when (and (niwa-view-historical-segment-p)
             (not (and allow-armed-fork
                       niwa-view--armed-session-fork)))
    (niwa-diagnostic-user-error (niwa-diagnostic-scope (current-buffer))
     "Viewing historical segment; return to latest or fork from an assistant response")))

(defun niwa-view-arm-conversation-fork ()
  "Arm a Conversation Fork from the settled assistant response at point."
  (interactive)
  (niwa-view--arm-session-fork 'conversation))

(defun niwa-view-arm-worktree-fork ()
  "Arm a Worktree Fork from the settled assistant response at point."
  (interactive)
  (niwa-view--arm-session-fork 'worktree))

(declare-function niwa-view-jump-to-interaction "niwa-view-interaction" ())

(defvar-keymap niwa-view--composer-keymap
  :doc "Keymap active over the editable composer body."
  "C-<tab>" #'niwa-view-toggle-plan-mode
  "C-c RET" #'niwa-view-send
  "C-c TAB" #'niwa-view-send-follow-up
  "C-c C-c" #'niwa-pending-inputs-save-edit
  "C-c C-e" #'niwa-pending-inputs-open
  "C-c C-k" #'niwa-view-cancel-composer-state
  "C-c C-j" #'niwa-view-jump-to-interaction
  "C-c C-SPC" #'niwa-view-pair-toggle-focus
  "C-M-v" #'scroll-other-window
  "C-M-S-v" #'scroll-other-window-down
  "C-c C-l" #'niwa-view-history-browse
  "C-c C-r" #'niwa-view-resume-pending
  "C-c C-u" #'niwa-view-history-clear-input
  "C-c C-w" #'niwa-view-retract-pending
  "C-y" #'niwa-view-yank-dwim
  "M-n" #'niwa-view-history-next
  "M-p" #'niwa-view-history-previous
  "M-r" #'niwa-view-history-search
  "<backtab>" #'niwa-view-cycle-permission-mode
  "S-TAB" #'niwa-view-cycle-permission-mode
  "<remap> <switch-to-buffer>" #'niwa-view-pair-switch-buffer)

(defvar-keymap niwa-view--side-conversation-keymap
  :doc "Keymap active over an ephemeral side-conversation composer."
  "C-c RET" #'niwa-view-send
  "C-c TAB" #'niwa-view-send-follow-up
  "C-c C-k" #'niwa-view-abort
  "C-c C-z" #'niwa-side-conversation-close
  "C-y" #'niwa-view-yank-dwim
  "<remap> <switch-to-buffer>" #'niwa-view-pair-switch-buffer)

(define-key niwa-view--composer-keymap
            [remap move-beginning-of-line]
            #'niwa-view-history-beginning-of-line)

(defvar-local niwa-view--composer-display-overlay nil
  "Presentation overlay over the editable composer.
It supplies the continuation indent that aligns later draft lines under
the first character after the prompt, without participating in key lookup.")

(defun niwa-view--update-composer-indent ()
  "Align composer continuation lines with the end of the prompt.
The prefix is display-only: the draft text, submission, and kills never
contain it.  The first draft line starts inside the read-only prompt, so
only explicit and soft-wrapped continuation lines receive it."
  (when (overlayp niwa-view--composer-display-overlay)
    (let* ((start (niwa-view-composer-draft-start))
           (indent (make-string
                    (string-width
                     (buffer-substring-no-properties
                      ;; `pos-bol' ignores the prompt field: measure from the true line start.
                      (save-excursion (goto-char start) (pos-bol))
                      start))
                    ?\s)))
      (overlay-put niwa-view--composer-display-overlay 'line-prefix indent)
      (overlay-put niwa-view--composer-display-overlay 'wrap-prefix indent)
      (niwa-view--update-composer-end))))

(defun niwa-view--update-composer-end (&rest _)
  "Align the cursor on an empty final composer line, without changing text.
Emacs does not apply an overlay's line prefix at end of buffer.  Supply
that prefix as display text only after a trailing newline; an ordinary
nonempty final line needs no extra display characters that could wrap."
  (when (overlayp niwa-view--composer-display-overlay)
    (overlay-put niwa-view--composer-display-overlay 'after-string
                 (when (eq (char-before (niwa-view-composer-draft-end)) ?\n)
                   (concat
                    (overlay-get niwa-view--composer-display-overlay 'line-prefix)
                    (propertize " " 'cursor t))))))


(defvar-local niwa-view--skill-argument-hint-overlay nil
  "Zero-width overlay that displays skill argument guidance in the composer.")

(defvar-local niwa-view--skill-argument-hint-tick nil
  "`buffer-chars-modified-tick' of the last skill argument hint refresh.")

(defvar-local niwa-view--prompt-hook-pending nil
  "Prompt submission awaiting a `UserPromptSubmit' hook for this view.
This covers the interval before the prompt has been accepted and before
`niwa--current-request' exists in the data buffer.")

(defvar-local niwa-view--pending-skill-submission nil
  "Cancellation token for skill-plan preparation before request dispatch.")

(defun niwa-view--cancel-pending-submission ()
  "Cancel this view's pending hook or skill preparation, if any."
  (niwa-pending-inputs-cancel-resume)
  (when niwa-view--prompt-hook-pending
    (niwa-prompt-submission-cancel niwa-view--prompt-hook-pending)
    (setq niwa-view--prompt-hook-pending nil))
  (when niwa-view--pending-skill-submission
    (setf (plist-get niwa-view--pending-skill-submission :cancelled) t)
    (setq niwa-view--pending-skill-submission nil)))


(defun niwa-view--position-in-input-region-p (position)
  "Return non-nil when POSITION is in the editable composer."
  (and (eq (current-buffer) (niwa-view-composer-buffer))
       (niwa-view-composer-in-draft-p position)))

(defun niwa-view--point-in-input-region-p ()
  "Return non-nil when point is in the editable composer."
  (niwa-view--position-in-input-region-p (point)))


;;
;;; Initialization

(defun niwa-view--composer-faces-p (from to face)
  "Return non-nil when the draft from FROM to TO already has FACE."
  (while (and (< from to) (equal face (get-text-property from 'font-lock-face)))
    (setq from (next-single-property-change from 'font-lock-face nil to)))
  (>= from to))

(defun niwa-view--fontify-composer (beg end)
  "Highlight Markdown in the draft blocks from BEG to END.
Parse the whole draft but fontify only the top-level blocks covering the
region and the rest of the displayed draft, so a keystroke does not
refontify a long draft.  An edit cannot change earlier blocks; later text
is refontified when it is next displayed.  Copy only faces: markup stays
visible and editable, and mention bindings and other input properties
remain untouched."
  (when-let* (((eq (current-buffer) (niwa-view-composer-buffer)))
              (start (niwa-view-composer-draft-start))
              ((> end start)))
    (save-restriction
      (widen)
      (let* ((draft-end (niwa-view-composer-draft-end))
             ;; Only windows showing the region: another may be far away.
             (shown (apply #'max end
                           (mapcar #'window-end
                                   (seq-filter (lambda (window)
                                                 (<= (window-start window) beg
                                                     (window-end window)))
                                               (get-buffer-window-list nil nil t)))))
             (blocks (niwa-view--fontify-markdown-blocks
                      (buffer-substring-no-properties start draft-end)
                      (- (max beg start) start) (- (min shown draft-end) start)))
             (base (+ start (car blocks)))
             (text (cdr blocks))
             (pos 0))
        (with-silent-modifications
          (while (< pos (length text))
            (let ((next (next-single-property-change pos 'font-lock-face text (length text)))
                  (face (get-text-property pos 'font-lock-face text)))
              (unless (niwa-view--composer-faces-p (+ base pos) (+ base next) face)
                (if face
                    (put-text-property (+ base pos) (+ base next) 'font-lock-face face)
                  (remove-text-properties (+ base pos) (+ base next) '(font-lock-face nil))))
              (setq pos next)))
          ;; This edit may restyle later blocks, as an opened or closed fence does.
          (when (< (+ base pos) draft-end)
            (put-text-property (+ base pos) draft-end 'fontified nil)))))))

(defun niwa-view-composer-initialize ()
  "Install editing support in the new composer."
  (use-local-map niwa-view-mode-map)
  (setq niwa-composer--emulation-maps
        `((niwa-composer--view . ,(if niwa-view--side-conversation-p
                                       niwa-view--side-conversation-keymap
                                     niwa-view--composer-keymap))))
  (setq-local emulation-mode-map-alists
              (cons 'niwa-composer--emulation-maps emulation-mode-map-alists))
  (setq-local niwa-mentions-agent-enabled-p
              (not niwa-view--side-conversation-p))
  (niwa-mentions-install)
  (jit-lock-register #'niwa-view--fontify-composer)
  (niwa-view--install-dnd)
  (add-hook 'completion-at-point-functions #'niwa-resource-capf nil t)
  (add-hook 'after-change-functions
            #'niwa-view--refresh-skill-argument-hint-after-change nil t)
  (add-hook 'before-change-functions #'niwa-view--note-draft-deletion nil t)
  (add-hook 'after-change-functions
            #'niwa-view--offer-follow-up-after-draft-change nil t)
  (unless niwa-view--side-conversation-p
    (add-hook 'completion-at-point-functions #'niwa-view-slash-capf nil t)
    (niwa-skills-install-font-lock #'niwa-view-composer-draft-start)
    (add-hook 'post-command-hook #'niwa-view--refresh-skill-argument-hint nil t))
  (setq niwa-view--composer-display-overlay
        (make-overlay (point) (point-max) (current-buffer) nil t))
  (add-hook 'after-change-functions #'niwa-view--update-composer-end nil t)
  (niwa-view--update-composer-indent))

;;
;;; Input forwarding

(defun niwa-view--ensure-interactive-chat-view ()
  "Signal when the current view buffer is not an editable chat view."
  (when niwa-view--agent-transcript-p
    (niwa-diagnostic-user-error (niwa-diagnostic-scope (current-buffer)) "Agent transcript views are read-only")))

(defun niwa-view--transcript-gptel-send-blocked (&optional _arg)
  "Block `gptel-send' from transcript inspection views."
  (interactive "P")
  (niwa-diagnostic-user-error (niwa-diagnostic-scope (current-buffer)) "Agent transcript views are read-only"))


(defun niwa-view--begin-external-turn
    (display-text data-turn-start &optional kind hook-context no-spinner
                  user-source attribution)
  "Begin a view turn initiated outside the editable input.

DISPLAY-TEXT is shown as the user-side turn in the view.
DATA-TURN-START is the data-buffer marker where the assistant
response for this turn begins.  KIND identifies the user turn.  HOOK-CONTEXT
is model-visible hook context to summarize in the view.  When
NO-SPINNER is non-nil, render only the local user turn.  USER-SOURCE bounds
the stored prompt, including its trusted skill presentation metadata.
ATTRIBUTION names who the turn speaks for when that is not the user."
  (niwa-view--ensure-interactive-chat-view)
  (let ((turn-start (niwa-view--insert-user-message
                     display-text kind hook-context nil nil nil attribution
                     user-source)))
    (niwa-view-stream-begin-turn
     turn-start data-turn-start no-spinner)))

(defun niwa-view-refresh-associated-input-prompt ()
  "Refresh the view prompt associated with the current buffer, if any.
Callable from either half of a view/data pair: prompt-affecting state
changes land in the data buffer, and the prompt lives in the view."
  (let ((view-buf (cond
                   ((and (boundp 'niwa--view-buffer)
                         (buffer-live-p niwa--view-buffer))
                    niwa--view-buffer)
                   ((and (boundp 'niwa--data-buffer)
                         (buffer-live-p niwa--data-buffer))
                    (buffer-local-value 'niwa--view-buffer
                                        niwa--data-buffer)))))
    (when (buffer-live-p view-buf)
      (with-current-buffer view-buf
        (niwa-view-refresh-input-prompt)))))

(defun niwa-view-composer-draft-start (&optional view)
  "Return the first editable position of VIEW's draft, or nil if absent.
VIEW defaults to the current buffer.  Ignore narrowing."
  (when-let* ((buffer (niwa-view-composer-buffer view)))
    (marker-position
     (buffer-local-value 'niwa-composer--draft-start buffer))))

(defun niwa-view-composer-draft-end (&optional view)
  "Return the position just after VIEW's draft, or nil if absent.
VIEW defaults to the current buffer.  Ignore narrowing."
  (when-let* ((buffer (niwa-view-composer-buffer view)))
    (1+ (buffer-size buffer))))

(defun niwa-view-composer-draft-text (&optional view without-properties)
  "Return VIEW's whole draft as a string, or nil if there is no composer.
Keep whitespace and text properties unless WITHOUT-PROPERTIES is non-nil,
in which case omit properties only.  VIEW defaults to the current buffer."
  (when-let* ((buffer (niwa-view-composer-buffer view)))
    (with-current-buffer buffer
      (let ((substring (if without-properties
                           #'buffer-substring-no-properties
                         #'buffer-substring)))
        (if (buffer-narrowed-p)
            (save-restriction
              (widen)
              (funcall substring niwa-composer--draft-start (point-max)))
          (funcall substring niwa-composer--draft-start (point-max)))))))

(defun niwa-view-composer-in-draft-p (position &optional view)
  "Return non-nil if POSITION is a valid caret position in VIEW's draft.
Include both boundaries.  A marker must belong to the composer buffer;
an integer is interpreted in that buffer.  VIEW defaults to the current
buffer."
  (when-let* ((buffer (niwa-view-composer-buffer view))
              (position (if (markerp position)
                            (and (eq (marker-buffer position) buffer)
                                 (marker-position position))
                          position)))
    (and (integerp position)
         (<= (niwa-view-composer-draft-start view) position
             (niwa-view-composer-draft-end view)))))

(defun niwa-view-composer-insert (text &optional position view)
  "Insert TEXT at POSITION in VIEW's draft and return point after the edit.
POSITION defaults to draft end and must be a draft caret position.
VIEW defaults to the current buffer.  This moves point in the composer
buffer without selecting a window or overriding read-only protections."
  (let* ((buffer (or (niwa-view-composer-buffer view)
                     (error "No composer for view")))
         (position (or position (niwa-view-composer-draft-end view))))
    (unless (niwa-view-composer-in-draft-p position view)
      (error "Insertion position is outside the composer draft"))
    (with-current-buffer buffer
      (save-restriction
        (widen)
        (goto-char position)
        (insert text)
        (point)))))

(defun niwa-view-composer-replace (text &optional view)
  "Replace VIEW's whole draft with TEXT and return point after the edit.
Use an empty string to clear the draft.  VIEW defaults to the current buffer.
This moves point in the composer buffer without selecting a window or
overriding read-only protections."
  (let ((buffer (or (niwa-view-composer-buffer view)
                    (error "No composer for view"))))
    (with-current-buffer buffer
      (save-restriction
        (widen)
        (barf-if-buffer-read-only)
        (atomic-change-group
          (delete-region niwa-composer--draft-start (point-max))
          (goto-char niwa-composer--draft-start)
          (insert text)
          (point))))))

(defun niwa-view-refresh-input-prompt ()
  "Refresh the composer prefix and its request rail."
  (interactive)
  (with-current-buffer (niwa-view-composer-owner)
    (when-let* ((composer (niwa-view-composer-buffer)))
      (let ((hint (niwa-view--input-hint-display)))
        (with-current-buffer composer
          (save-restriction
            (widen)
            (let ((start (save-excursion (goto-char (point-min))
                                         (line-beginning-position 2))))
              (unless (equal hint (get-text-property start 'display))
                (let ((inhibit-read-only t))
                  (with-silent-modifications
                    (put-text-property start (1+ start) 'display hint)))))))))
    (unless niwa-view--agent-transcript-p
      (niwa-view--render-request-progress))))

(defun niwa-view-cycle-permission-mode ()
  "Cycle the current session's permission mode from the view buffer."
  (interactive)
  (let* ((data-buf (and (boundp 'niwa--data-buffer)
                        niwa--data-buffer
                        (buffer-live-p niwa--data-buffer)
                        niwa--data-buffer))
         (session (or (and (boundp 'niwa--session) niwa--session)
                      (and data-buf
                           (buffer-local-value 'niwa--session data-buf)))))
    (unless (and data-buf session)
      (niwa-diagnostic-user-error (niwa-diagnostic-scope (current-buffer)) "No niwa session for permission mode cycling"))
    (let ((next
           (niwa-view--next-permission-mode
            (or (niwa-session-permission-mode session) 'ask))))
      (with-current-buffer data-buf
        (niwa-permission-mode-transition next))
      (message "niwa: permission mode %s"
               (car (niwa-view--permission-mode-display next)))
      next)))

(defun niwa-view-toggle-plan-mode ()
  "Toggle Plan for the current session without changing permissions."
  (interactive)
  (let* ((data-buf (and (boundp 'niwa--data-buffer)
                        niwa--data-buffer
                        (buffer-live-p niwa--data-buffer)
                        niwa--data-buffer))
         (session (or (and (boundp 'niwa--session) niwa--session)
                      (and data-buf
                           (buffer-local-value 'niwa--session data-buf)))))
    (unless (and data-buf session)
      (niwa-diagnostic-user-error (niwa-diagnostic-scope (current-buffer)) "No niwa session for Plan mode"))
    (if (niwa-session-plan-mode session)
        (niwa-plan-mode-exit session)
      (niwa-plan-mode-enter session))
    (niwa-view-refresh-input-prompt)
    (message "niwa: Plan mode %s"
             (if (niwa-session-plan-mode session) "on" "off"))
    (niwa-session-plan-mode session)))

(defun niwa-view--clear-submitted-input (submitted-draft)
  "Clear the composer while it still holds SUBMITTED-DRAFT.
Preparation and prompt hooks can run asynchronously, so anything the user
typed after the submission captured SUBMITTED-DRAFT is a new draft: it
stays, with the mention bindings and dropped-file grants it carries.  A
caller that captured no draft clears unconditionally: a drained pending
input already required an empty composer, and a buffer with no composer
has no draft to protect."
  (when (or (null submitted-draft)
            (equal submitted-draft (niwa-view--visible-draft)))
    (niwa-view--clear-input)))

(defun niwa-view--visible-draft ()
  "Return the composer's visible text, exactly as it stands.
This is the text a submission captures and the text a later clear
decision compares against, so it must not be trimmed or normalized.
Returns nil in a buffer with no editable composer, because a submission
raised from one has no draft to protect."
  (niwa-view-composer-draft-text nil t))

(defun niwa-view--input-text ()
  "Return the user's composer text, trimmed."
  (string-trim (niwa-mention-bindings-copy-text
                (or (niwa-view-composer-draft-text) ""))))

(defun niwa-view--bind-input-mentions (session)
  "Bind known mentions in the live composer for SESSION and return input.
The visible text is unchanged.  Binding before asynchronous preparation
means a failed attempt leaves the exact source attached for a retry."
  (let* ((view (current-buffer))
         (raw-input
          (niwa-mention-bindings-copy-text
           (niwa-view-composer-draft-text view)))
         (bound-input
          (with-current-buffer niwa--data-buffer
            (niwa-mentions-prepare-user-input
             (niwa-skills-input-prepare-user-input raw-input session)
             session))))
    (with-current-buffer (niwa-view-composer-buffer view)
      (save-restriction
        (widen)
        (let ((start (niwa-view-composer-draft-start view))
              (end (niwa-view-composer-draft-end view)))
          (with-silent-modifications
            (remove-text-properties
             start end '(niwa-mention-binding nil rear-nonsticky nil))
            (dolist (range (niwa-mention-bindings-ranges bound-input))
              (niwa-mention-bindings-set
               (+ start (plist-get range :start))
               (+ start (plist-get range :end))
               (plist-get range :binding)))))))
    (string-trim bound-input)))

(defun niwa-view--composer-snapshot (session)
  "Capture the editable composer and pending grants for SESSION."
  (let ((start (niwa-view-composer-draft-start)))
    (list :text (niwa-view-composer-draft-text)
          :point-offset (- (with-current-buffer (niwa-view-composer-buffer)
                             (point))
                           start)
          :dropped-file-grants
          (copy-sequence (niwa-session-dropped-file-grants session)))))

(defun niwa-view--restore-composer-snapshot (snapshot session &optional force)
  "Restore SNAPSHOT for SESSION when its visible draft is still current.
When FORCE is non-nil, replace the current draft unconditionally."
  (let ((view (current-buffer))
        (text (plist-get snapshot :text)))
    (when (or force
              (equal (niwa-view-composer-draft-text view t)
                     (substring-no-properties text)))
      (with-current-buffer (niwa-view-composer-buffer view)
        (let ((inhibit-read-only t))
          (with-silent-modifications
            (niwa-view-composer-replace text view)))
        (niwa-view--update-composer-end)
        (goto-char
         (min (niwa-view-composer-draft-end view)
              (+ (niwa-view-composer-draft-start view)
                 (plist-get snapshot :point-offset)))))
      (niwa-session--set-dropped-file-grants
       session
       (copy-sequence
        (plist-get snapshot :dropped-file-grants)))
      t)))


(defun niwa-view--clear-input ()
  "Clear the user's composer text, leaving the prompt in place."
  (niwa-view--ensure-interactive-chat-view)
  (when-let* ((view (current-buffer))
              (composer (niwa-view-composer-buffer view)))
    (with-current-buffer composer
      (save-excursion (niwa-view-composer-replace "" view))
      (niwa-view--delete-skill-argument-hint))
    (when-let* ((session (niwa-view--session)))
      (niwa-session-clear-dropped-file-grants session))))

(defun niwa-view--session ()
  "Return the session associated with the current view buffer."
  (or (and (boundp 'niwa--session) niwa--session)
      (and (boundp 'niwa--data-buffer)
           (buffer-live-p niwa--data-buffer)
           (buffer-local-value 'niwa--session niwa--data-buffer))))

(defun niwa-view--reserved-goal-handoff-id (&optional session)
  "Return SESSION's Goal handoff reservation, or nil."
  (niwa-plan-handoff-reserved-goal-id
   (or session (niwa-view--session))))

(defun niwa-view--occupied-root-workflow (session)
  "Return a symbol naming SESSION's occupying workflow, or nil.
Non-nil means later input is a follow-up.  The symbol is one of
`plan-approval', `implementation-retry',
`goal-budget', or `goal'."
  (cond
   ((niwa-session-pending-plan-approval session) 'plan-approval)
   ((plist-get (niwa-session-plan-metadata session)
               :implementation-retry)
    'implementation-retry)
   ((when-let* ((goal (niwa-session-goal session)))
      (pcase (niwa-goal-status goal)
        ('complete nil)
        ('budget-limited 'goal-budget)
        (_ 'goal))))))

(defun niwa-view--occupied-root-workflow-error (occupied)
  "Signal the user error for the OCCUPIED root workflow symbol."
  (niwa-diagnostic-user-error (niwa-diagnostic-scope (current-buffer))
   "%s"
   (pcase occupied
     ('implementation-retry
      "An accepted plan implementation is pending -- M-x niwa-retry-plan-implementation resumes it")
     ('plan-approval
      "A plan proposal is awaiting approval -- respond to it first")
     ('goal-budget
      "The unfinished Goal has exhausted its token budget -- /goal budget N or /goal budget none continues it")
     ('goal
      "An unfinished Goal owns this session -- /goal resume continues it")
     (_ "The workflow is occupied -- use C-c TAB for a follow-up"))))

(defun niwa-view--delete-skill-argument-hint ()
  "Remove the composer skill argument hint overlay."
  (when (overlayp niwa-view--skill-argument-hint-overlay)
    (delete-overlay niwa-view--skill-argument-hint-overlay))
  (setq niwa-view--skill-argument-hint-overlay nil))

(defun niwa-view--skill-argument-hint ()
  "Return display-only skill argument hint for the current composer."
  (when-let* ((session (niwa-view--session))
              (text (niwa-view-composer-draft-text nil t))
              (parsed (niwa-skills-input-parse-skill-line text))
              (name (nth 0 parsed))
              (skill (niwa-session-get-skill session name))
              ((niwa-skill-user-invocable-p skill)))
    (niwa-skills-remaining-argument-hint skill (nth 1 parsed))))

(defun niwa-view--refresh-skill-argument-hint ()
  "Show the display-only skill argument hint after the draft.
The hint names arguments still to be typed at the end of the draft.  Its
place and visibility depend only on the buffer text, never on point, so
moving the cursor never reflows the composer.  Unchanged text, as after
any motion command, returns without work."
  (let ((tick (buffer-chars-modified-tick)))
    (unless (eql tick niwa-view--skill-argument-hint-tick)
      (setq niwa-view--skill-argument-hint-tick tick)
      (let ((hint (and (eq (current-buffer) (niwa-view-composer-buffer))
                       (niwa-view--skill-argument-hint))))
        (if (and hint (not (string-empty-p hint)))
            (let ((end (niwa-view-composer-draft-end))
                  (string (propertize (concat " " hint)
                                      'font-lock-face 'shadow)))
              (unless (overlayp niwa-view--skill-argument-hint-overlay)
                (setq niwa-view--skill-argument-hint-overlay
                      (make-overlay end end (current-buffer) nil t))
                (overlay-put niwa-view--skill-argument-hint-overlay
                             'priority 10))
              (move-overlay niwa-view--skill-argument-hint-overlay
                            end end (current-buffer))
              (unless (equal-including-properties
                       string
                       (overlay-get niwa-view--skill-argument-hint-overlay
                                    'after-string))
                (overlay-put niwa-view--skill-argument-hint-overlay
                             'after-string string)))
          (niwa-view--delete-skill-argument-hint))))))

(defun niwa-view--refresh-skill-argument-hint-after-change
    (start end _old-length)
  "Invalidate edited bindings and refresh the composer skill hint."
  (when-let* (((eq (current-buffer) (niwa-view-composer-buffer)))
              (draft-start (niwa-view-composer-draft-start))
              ((>= start draft-start))
              (draft-end (niwa-view-composer-draft-end))
              ((<= end draft-end)))
    (niwa-mention-bindings-invalidate-edit
     start end draft-start draft-end))
  (niwa-view--refresh-skill-argument-hint))

(defun niwa-view-slash-capf ()
  "Completion-at-point for slash commands and `$' skills in the composer.
Offers local slash commands at `/name', session skills at `$name',
and command argument completion for commands with finite choices."
  (when (and (buffer-live-p niwa--data-buffer)
             (niwa-view--point-in-input-region-p))
    (let ((session (buffer-local-value 'niwa--session
                                       niwa--data-buffer)))
      (niwa-skills-slash-capf
       niwa--data-buffer session niwa-slash-commands
       (niwa-view-composer-draft-start)))))

(defun niwa-view--start-fork-skill-turn
    (input display-text &optional hook-context submitted-draft dispatch)
  "Render and record a fork skill INPUT without calling `gptel-send'.

DISPLAY-TEXT is shown in the view for the user turn.  INPUT is written
to the data buffer as the authoritative user prompt.  The data-turn
marker is anchored after that prompt so the eventual fork result can be
rendered by the normal post-response hook.  HOOK-CONTEXT is summarized
in the view when present.  SUBMITTED-DRAFT is the composer text captured
when the submission started.  DISPATCH receives the admitted request and
starts the fork after insertion."
  (when (niwa-turn-busy-p niwa--data-buffer)
    (niwa-diagnostic-user-error (niwa-diagnostic-scope niwa--data-buffer) "Turn settlement is still pending"))
  (let ((view-turn-start
         (niwa-view--insert-user-message display-text nil hook-context))
        request)
    (niwa-view--clear-submitted-input submitted-draft)
    (with-current-buffer niwa--data-buffer
      (when niwa--session
        (setq request
              (niwa-request-begin
               niwa--session)))
      (goto-char (point-max))
      (niwa--insert-user-turn input)
      (let ((data-turn-start (copy-marker (point) nil)))
        (with-current-buffer niwa--view-buffer
          (niwa-view-stream-begin-turn
           view-turn-start data-turn-start))))
    (when dispatch (funcall dispatch request))))

(defun niwa-view--finish-fork-skill-outcome
    (name outcome view-buffer data-buffer &optional skill)
  "Handle fork skill OUTCOME for NAME."
  (when (and (buffer-live-p view-buffer)
             (buffer-live-p data-buffer))
    (when (and (fboundp 'niwa-review-command-skill-p)
               (niwa-review-command-skill-p skill)
               (fboundp 'niwa-review-mark-command-outcome))
      (setq outcome (niwa-review-mark-command-outcome outcome)))
    (when (fboundp 'niwa-review-transform-outcome)
      (setq outcome (niwa-review-transform-outcome name outcome)))
    (pcase (plist-get outcome :status)
      ('ok
       (pcase (plist-get outcome :kind)
         ('fork
          (with-current-buffer data-buffer
            (niwa-skills-input-insert-fork-result outcome)))
         (_
          (niwa-diagnostic-message (niwa-diagnostic-scope data-buffer) "Skill '%s' returned unsupported outcome: %S"
                   name outcome))))
      (_
       (with-current-buffer view-buffer
         (niwa-view--stop-request-progress)
         (niwa-diagnostic-message (niwa-diagnostic-scope data-buffer) "%s (Skill '%s' failed)"
                  (or (plist-get outcome :message) "unknown error") name))
       (with-current-buffer data-buffer
         (when (bound-and-true-p niwa--current-request)
           (niwa-request-end))
         (gptel--update-status " Ready" 'success))))))

(defun niwa-view--skill-submission-active-p
    (token view-buffer data-buffer)
  "Return non-nil when TOKEN still owns a live skill submission."
  (and (buffer-live-p view-buffer)
       (buffer-live-p data-buffer)
       (not (plist-get token :cancelled))
       (with-current-buffer view-buffer
         (eq niwa-view--pending-skill-submission token))))

(defun niwa-view--finish-skill-submission (token)
  "Clear TOKEN when it still owns the current skill submission."
  (when (eq niwa-view--pending-skill-submission token)
    (setq niwa-view--pending-skill-submission nil)))

(defun niwa-view--prepared-fork-outcome (prepared)
  "Return PREPARED's fork command outcome, or nil."
  (when-let* ((pair
               (cl-find-if
                (lambda (item)
                  (eq (plist-get (plist-get item :outcome) :kind) 'fork))
                (plist-get prepared :prepared-entries))))
    (plist-get pair :outcome)))

(defun niwa-view--block-planned-submission (submission &optional prepared)
  "Finish SUBMISSION as blocked, optionally reporting failed PREPARED work."
  (let ((token (plist-get submission :token))
        (on-block (plist-get submission :on-block)))
    (niwa-view--finish-skill-submission token)
    (when (and prepared
               (not (eq (plist-get prepared :reason) 'cancelled)))
      (niwa-diagnostic-message (niwa-diagnostic-scope (plist-get submission :data-buffer)) "%s (skill $%s failed)"
               (or (plist-get prepared :message) "unknown error")
               (or (plist-get prepared :name) "unknown")))
    (when on-block
      (funcall on-block))))

(defun niwa-view--prepared-plan-outcome
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
          :render-data (niwa-skills-plan-render-data plan prepared)
          :fork-outcome
          (and (niwa-skill-invocation-plan-fork-p plan)
               (niwa-view--prepared-fork-outcome prepared)))))

(cl-defun niwa-view--dispatch-prepared-outcome
    (submission data-buffer &key before-send after-insert on-block dispatch
                submitted-draft)
  "Dispatch accepted prompt SUBMISSION through DATA-BUFFER.
BEFORE-SEND runs at the dispatch boundary.  AFTER-INSERT runs once the prompt
is durably recorded.  ON-BLOCK runs after a dispatch error.  DISPATCH, when
non-nil, receives SUBMISSION instead of starting a request.  SUBMITTED-DRAFT is
the composer text captured before preparation began, so a draft typed while it
ran survives the send."
  (let* ((view-buffer (current-buffer))
         (diagnostic-scope (niwa-diagnostic-scope data-buffer))
         (outcome (niwa-prompt-submission-outcome submission))
         (input (niwa-prompt-submission-display-text submission))
         (model-input (plist-get outcome :model-input))
         (transcript-input (plist-get outcome :transcript-input))
         (hook-input (plist-get outcome :hook-input))
         (hook-context (plist-get outcome :hook-context))
         (all-audits (plist-get outcome :hook-audits))
         (request-context (plist-get outcome :request-context))
         (failure-preparation
          (when (buffer-live-p data-buffer)
            (with-current-buffer data-buffer
              ;; A staged kickoff owns this unadmitted policy, not an ordinary
              ;; submission finishing preparation in the same root.  Refuse
              ;; before replacing its skill context or interpreting its Goal.
              (when (and (not dispatch) niwa--pending-implementation-policy)
                (signal 'niwa-stale-implementation-preparation nil))
              (niwa-implementation-policy-capture-preparation))))
         context-installed-p
         (render-data (or (plist-get outcome :render-data) ""))
         (fork-outcome (plist-get outcome :fork-outcome))
         (view-context
          (and (not dispatch)
               (niwa-view--join-hook-contexts
                (niwa-hooks-format-context
                 (niwa-view--hook-context-events-from-text hook-input))
                hook-context))))
    (condition-case err
        (progn
          (when before-send
            (funcall before-send))
          (when failure-preparation
            (with-current-buffer data-buffer
              (niwa-implementation-policy-assert-preparation failure-preparation)))
          (cond
           (dispatch
            (funcall dispatch submission))
           (fork-outcome
            (let* ((skill (plist-get fork-outcome :skill))
                   (name (niwa-skill-name skill)))
              (niwa-view--start-fork-skill-turn
               (concat transcript-input render-data) input view-context
               submitted-draft
               (lambda (request)
                 (niwa-prompt-submission-commit submission)
                 (niwa-session-naming-consider
                  (buffer-local-value 'niwa--session data-buffer) input)
                 (when after-insert
                   (funcall after-insert))
                 (with-current-buffer data-buffer
                   (niwa-request-call-owned
                    request data-buffer #'niwa-skills-dispatch-prepared-fork
                    fork-outcome
                    (lambda (result)
                      (niwa-request-call-owned
                       request data-buffer #'niwa-view--finish-fork-skill-outcome
                       name result view-buffer data-buffer skill))
                    :prompt model-input
                    :request-context request-context
                    :hook-audits all-audits))))))
           (t
            (with-current-buffer data-buffer
              (setq-local niwa-skills--pending-request-context
                          request-context)
              (setq context-installed-p t))
            (niwa-view--forward-input
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
           (when (and context-installed-p
                      (not niwa--current-request)
                      (eq request-context niwa-skills--pending-request-context))
             (setq-local niwa-skills--pending-request-context nil))))
       (niwa-implementation-policy-pause-failure
        data-buffer err failure-preparation)
       (niwa-diagnostic-message diagnostic-scope "%s"
                (error-message-string err))
       (when on-block
         (funcall on-block))
       nil))))

(defun niwa-view--dispatch-prepared-plan
    (plan-submission prepared prompt-submission)
  "Dispatch PREPARED plan for PLAN-SUBMISSION and PROMPT-SUBMISSION."
  (let* ((token (plist-get plan-submission :token))
         (view-buffer (plist-get plan-submission :view-buffer))
         (data-buffer (plist-get plan-submission :data-buffer)))
    (when (niwa-view--skill-submission-active-p
           token view-buffer data-buffer)
      (with-current-buffer view-buffer
        (niwa-view--finish-skill-submission token)
        (let* ((hook-input
                (niwa-prompt-submission-input prompt-submission))
               (hook-context
                (niwa-prompt-submission-context prompt-submission))
               (hook-audits
                (niwa-prompt-submission-audits prompt-submission))
               (outcome
                (niwa-view--prepared-plan-outcome
                 plan-submission prepared hook-input hook-context hook-audits)))
          (niwa-prompt-submission-set-outcome
           prompt-submission outcome)
          (when-let* ((warnings (plist-get prepared :warnings)))
            (niwa-diagnostic-message (niwa-diagnostic-scope data-buffer) "%s" (string-join warnings "; ")))
          (niwa-view--dispatch-prepared-outcome
           prompt-submission data-buffer
           :before-send (plist-get plan-submission :before-send)
           :after-insert (plist-get plan-submission :after-insert)
           :on-block (plist-get plan-submission :on-block)
           :dispatch (plist-get plan-submission :dispatch)
           :submitted-draft (plist-get plan-submission :submitted-draft)))))))

(defun niwa-view--handle-prepared-plan (submission prepared)
  "Continue SUBMISSION after PREPARED skill work settles."
  (let ((token (plist-get submission :token))
        (view-buffer (plist-get submission :view-buffer))
        (data-buffer (plist-get submission :data-buffer)))
    (when (niwa-view--skill-submission-active-p
           token view-buffer data-buffer)
      (with-current-buffer view-buffer
        (if (not (eq (plist-get prepared :status) 'ok))
            (niwa-view--block-planned-submission submission prepared)
          (niwa-view--run-prompt-submit-hook
           (plist-get prepared :model-input)
           (plist-get submission :input)
           (lambda (prompt-submission)
             (niwa-view--dispatch-prepared-plan
              submission prepared prompt-submission))
           (lambda ()
             (when (niwa-view--skill-submission-active-p
                    token view-buffer data-buffer)
               (with-current-buffer view-buffer
                 (niwa-view--block-planned-submission submission))))
           (plist-get prepared :hook-context)))))))

(defun niwa-view--submit-planned-input
    (input &optional before-send on-block dispatch after-insert inert-skills
           selected-skills)
  "Plan, prepare, and submit atomically bound user INPUT.

BEFORE-SEND runs exactly once at the dispatch boundary.  ON-BLOCK runs when
planning, preparation, or `UserPromptSubmit' rejects the submission.  Derived
skill bodies and hook output are never scanned for additional invocations.
When DISPATCH is non-nil, call it with an accepted prompt submission instead of
starting a new request.  AFTER-INSERT runs once the prompt is durably recorded.
When INERT-SKILLS is non-nil, skip skill planning entirely: any skill token in
INPUT stays literal text.  External input -- a collaboration guest's prompt --
carries prompting authority only, never skill invocation.
SELECTED-SKILLS explicitly names the skills to apply to literal INPUT."
  (let ((view-buffer (current-buffer))
        (data-buffer niwa--data-buffer)
        (session (niwa-view--session))
        ;; Captured here because both skill preparation and the prompt hook
        ;; below run asynchronously, and only this entry is still holding
        ;; the draft the user actually submitted.
        (submitted-draft (niwa-view--visible-draft)))
    ;; A recorded message ends a wait for the user after a permission queue
    ;; expired.  `after-insert' runs only once the prompt is durably recorded,
    ;; before its request starts; a rejected submission keeps the wait.  A
    ;; DISPATCH callback owns its own acceptance, as steering does.  Queued
    ;; follow-ups cannot reach here while it holds.
    (let* ((caller-after-insert after-insert)
           (after-insert
            (if dispatch
                after-insert
              (lambda ()
                (niwa-permission-absence-clear session)
                (when caller-after-insert
                  (funcall caller-after-insert)))))
           (plan
            (unless inert-skills
              (with-current-buffer data-buffer
                (niwa-skills-input-refresh-bound-input input session)
                (niwa-skills-plan-user-input input session selected-skills)))))
      (when selected-skills
        (setq input (niwa-skill-invocation-plan-text plan)))
      (if (or inert-skills
              (null (niwa-skill-invocation-plan-occurrences plan)))
          (if dispatch
              (niwa-view--run-prompt-submit-hook
               input input
               (lambda (prompt-submission)
                 (let* ((hook-input
                         (niwa-prompt-submission-input
                          prompt-submission))
                        (hook-context
                         (niwa-prompt-submission-context
                          prompt-submission))
                        (prepared-input
                         (if hook-context
                             (concat hook-input "\n\n" hook-context)
                           hook-input)))
                   (niwa-prompt-submission-set-outcome
                    prompt-submission
                    (list :model-input prepared-input
                          :transcript-input prepared-input
                          :hook-input hook-input
                          :hook-context hook-context
                          :hook-audits
                          (niwa-prompt-submission-audits
                           prompt-submission)))
                   (niwa-view--dispatch-prepared-outcome
                    prompt-submission data-buffer
                    :before-send before-send
                    :after-insert after-insert
                    :on-block on-block
                    :dispatch dispatch
                    :submitted-draft submitted-draft)))
               on-block)
            (niwa-view--forward-input
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
          (setq niwa-view--pending-skill-submission token)
          (with-current-buffer data-buffer
            (niwa-skills-plan-prepare
             plan
             (lambda (prepared)
               (niwa-view--handle-prepared-plan submission prepared))
             (lambda ()
               (not (niwa-view--skill-submission-active-p
                     token view-buffer data-buffer))))))))))

(defun niwa-view--submit-armed-session-fork
    (source-view input target snapshot)
  "Publish TARGET from SOURCE-VIEW, then submit INPUT in its Child.
SNAPSHOT is the exact Source composer state transferred on publication."
  (let* ((fork-type (plist-get target :fork-type))
         (source-data
          (buffer-local-value 'niwa--data-buffer source-view))
         (referenced-grants
          (cl-intersection
           (niwa-mentions-file-paths-in-text input)
           (plist-get snapshot :dropped-file-grants)
           :test #'equal))
         (child-data
          (niwa-session-fork-create source-data target fork-type))
         (child-view
          (buffer-local-value 'niwa--view-buffer child-data))
         (child-session
          (buffer-local-value 'niwa--session child-data)))
    (unless (buffer-live-p child-view)
      (error "Session Fork has no live view"))
    (niwa-view-rerender source-view)
    (setq snapshot (copy-tree snapshot t))
    (when (eq fork-type 'worktree)
      (setq input
            (niwa-view--retarget-worktree-mention-bindings
             input child-session)
            referenced-grants
            (mapcar
             (lambda (path)
               (niwa-session-fork-retarget-worktree-path
                child-session path))
             referenced-grants))
      (plist-put
       snapshot :text
       (niwa-view--retarget-worktree-mention-bindings
        (plist-get snapshot :text) child-session)))
    (plist-put snapshot :dropped-file-grants referenced-grants)
    (with-current-buffer child-view
      (niwa-view--restore-composer-snapshot
       snapshot child-session t))
    (with-current-buffer source-view
      (niwa-view--clear-input)
      (niwa-view-cancel-session-fork))
    (display-buffer child-view)
    (with-current-buffer child-view
      (niwa-view--submit-planned-input
       input nil nil nil
       (lambda () (niwa-view-history-add input))))
    child-data))

(defun niwa-view--retarget-worktree-mention-bindings (text session)
  "Retarget repository-local mention bindings in TEXT for SESSION."
  (let ((copy (niwa-mention-bindings-copy-text text)))
    (dolist (range (niwa-mention-bindings-ranges copy))
      (let* ((binding (copy-tree (plist-get range :binding) t))
             (key
              (pcase (plist-get binding :kind)
                ('file :path)
                ('skill :source-file))))
        (when key
          (plist-put
           binding key
           (niwa-session-fork-retarget-worktree-path
            session (plist-get binding key)))
          (niwa-mention-bindings-set
           (plist-get range :start)
           (plist-get range :end)
           binding copy))))
    copy))


(defun niwa-view-send ()
  "Send the current root or ephemeral side-conversation composer text."
  (interactive)
  (with-current-buffer (niwa-view-composer-owner)
    (niwa-with-diagnostic-errors (niwa-diagnostic-scope (current-buffer))
      (if niwa-view--side-conversation-p
          (niwa-side-conversation-send)
        (niwa-view--send-root)))))

(defun niwa-view--send-root (&optional steering-input)
  "Send the current composer text to the LLM via the data buffer.
Extracts text from the input zone, plans all bound `$skill' mentions,
renders the original text in the history region, and dispatches either
one coherent request or one leading fork command.  Slash commands retain
their local dispatch path.
When STEERING-INPUT is non-nil, submit that text to the active root turn
instead of the composer, preserving its draft."
  (niwa-view--ensure-interactive-chat-view)
  (niwa-view--assert-live-tip t)
  (when niwa-view--pending-input-edit
    (niwa-diagnostic-user-error (niwa-diagnostic-scope (current-buffer)) "Save or cancel the pending-input edit first"))
  (unless niwa--data-buffer
    (niwa-diagnostic-user-error (niwa-diagnostic-scope (current-buffer)) "No data buffer associated with this view"))
  (unless (buffer-live-p niwa--data-buffer)
    (niwa-diagnostic-user-error (niwa-diagnostic-scope (current-buffer)) "Data buffer has been killed"))
  (when niwa-view--prompt-hook-pending
    (niwa-diagnostic-user-error (niwa-diagnostic-scope (current-buffer)) "A prompt hook is still running -- wait or abort first"))
  (when niwa-view--pending-skill-submission
    (niwa-diagnostic-user-error (niwa-diagnostic-scope (current-buffer)) "Skill preparation is still running -- wait or abort first"))
  (when (buffer-local-value 'niwa-compact-run-in-flight niwa--data-buffer)
    (message "niwa: compacting, please wait...")
    (niwa-diagnostic-user-error (niwa-diagnostic-scope (current-buffer)) "Compaction in progress"))
  (when (buffer-local-value 'niwa-session--read-only-mode
                            niwa--data-buffer)
    (niwa-diagnostic-user-error (niwa-diagnostic-scope (current-buffer)) "Session is open read-only (another host holds the lock)"))
  (when steering-input
    (unless (buffer-local-value 'niwa--current-request niwa--data-buffer)
      (niwa-diagnostic-user-error (niwa-diagnostic-scope (current-buffer)) "No active root turn to steer")))
  (let* ((session (buffer-local-value 'niwa--session
                                      niwa--data-buffer))
         (snapshot (and session (not steering-input)
                        (niwa-view--composer-snapshot session)))
         (input
          (if steering-input
              (with-current-buffer niwa--data-buffer
                (string-trim
                 (niwa-mentions-prepare-user-input
                  (niwa-skills-input-prepare-user-input steering-input session)
                  session)))
            (if session
                (niwa-view--bind-input-mentions session)
              (niwa-view--input-text)))))
    (when session
      (niwa-session-artifacts-assert-new-mutation-authority session))
    (when (string-empty-p input)
      (niwa-diagnostic-user-error (niwa-diagnostic-scope (current-buffer)) "Nothing to send"))
    (when (and steering-input (niwa-skills-parse-slash-line input))
      (niwa-diagnostic-user-error (niwa-diagnostic-scope (current-buffer)) "Slash commands cannot be queued as steering"))
    (let* ((slash-parsed (niwa-skills-parse-slash-line input))
             (fork-target niwa-view--armed-session-fork)
             (active-request
              (buffer-local-value 'niwa--current-request
                                  niwa--data-buffer))
             (submitted-draft
              (when (and active-request (not steering-input))
                (niwa-view--visible-draft)))
             (settling
              (buffer-local-value 'niwa--turn-settlements-pending
                                  niwa--data-buffer))
             (occupied
              (and (not slash-parsed)
                   (niwa-view--occupied-root-workflow session))))
	(when (and slash-parsed (niwa-view-historical-segment-p))
          (niwa-diagnostic-user-error (niwa-diagnostic-scope (current-buffer))
           "Slash commands are unavailable while viewing a historical segment"))
	(when (and fork-target (not slash-parsed))
          (niwa-session-rewind-assert-stable-source
           session niwa--data-buffer "forking"))
        (if (or active-request occupied settling)
            (let ((restore
                   (lambda ()
                     (when snapshot
                       (niwa-view--restore-composer-snapshot
			snapshot session)))))
              (cond
               ((and slash-parsed
                     (niwa-skills-local-command-active-request-p
                      (nth 0 slash-parsed) (nth 1 slash-parsed)))
		(let ((scope (niwa-diagnostic-scope session))
                      (result (with-current-buffer niwa--data-buffer
				(funcall (cdr (assoc (nth 0 slash-parsed)
                                                     niwa-slash-commands))
					 (nth 1 slash-parsed)))))
                  (unless (eq result 'niwa-view-sent)
                    (niwa-skills-message-command-result
                     scope (nth 0 slash-parsed) result)
                    (niwa-view-history-add input)
                    (niwa-view--clear-input))))
               (slash-parsed
		(funcall restore)
		(niwa-diagnostic-user-error (niwa-diagnostic-scope (current-buffer))
		 "A request is already active -- wait or abort first"))
               ((and settling (not steering-input))
                ;; Publication still owns the old turn; the reply belongs next.
                (niwa-view--queue-follow-up input))
               ((not active-request)
		(funcall restore)
		(niwa-view--occupied-root-workflow-error
		 occupied))
               ((not (niwa-view--steerable-root-request-p active-request))
		(funcall restore)
		(niwa-diagnostic-user-error (niwa-diagnostic-scope (current-buffer))
		 "This workflow cannot be steered -- use C-c TAB"))
               (t
		(condition-case err
                    (niwa-view--submit-planned-input
                     input nil
                     (lambda ()
                       (funcall restore)
                       (niwa-diagnostic-message (niwa-diagnostic-scope session)
			"steering preparation failed; use C-c TAB"))
                     (lambda (submission)
                       (unless
                           (niwa-view--queue-prepared-steering
                            submission active-request submitted-draft)
			 (funcall restore))))
                  (error
                   (funcall restore)
                   (niwa-diagnostic-user-error (niwa-diagnostic-scope session) "%s; use C-c TAB for a follow-up"
                               (error-message-string err)))))))
          (cond
           (slash-parsed
            (niwa-view-run-invocation
             (nth 0 slash-parsed) (nth 1 slash-parsed)
             :display input
             :on-quiet (lambda ()
                         (niwa-view-history-add input)
                         (niwa-view--clear-input))
             :on-sent (lambda () (niwa-view-history-add input))))
           (t
            (let ((source-view (current-buffer)))
              (if fork-target
                  (progn
                    ;; Parsing stays in Source so malformed skill syntax cannot
                    ;; publish a child.  Expansion and hooks belong to Child.
                    (with-current-buffer niwa--data-buffer
                      (niwa-skills-plan-user-input input session))
                    (niwa-view--submit-armed-session-fork
                     source-view input fork-target snapshot))
		(niwa-view--submit-planned-input
		 input
		 (lambda ()
	                   (niwa-view-history-add input))))))))))
  ;; Accepted sends clear the submitted draft and land at the composer end.
  ;; Rejected sends preserve the exact input-relative point.
  (let ((view (current-buffer)))
    (with-current-buffer (niwa-view-composer-buffer view)
      (unless (niwa-view-composer-in-draft-p (point) view)
        (goto-char (niwa-view-composer-draft-end view))))))

(defun niwa-view-invocation-kind (name &optional session)
  "Return how NAME is invoked: `command\=', `skill\=', or nil.

Local slash commands and skills are separate namespaces reached with
different sigils -- `/name\=' runs a command, `$name\=' runs a skill -- so
anything dispatching a bare name has to ask which one it is."
  (cond
   ((assoc name niwa-slash-commands) 'command)
   ((let ((session (or session (niwa-view--session))))
      (cl-find name (niwa-skills-user-visible-skills session)
               :key #'niwa-skill-name :test #'equal))
    'skill)))

(cl-defun niwa-view-run-invocation (name args &key display on-quiet on-sent)
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
  (let* ((scope (niwa-diagnostic-scope niwa--data-buffer))
         (local (assoc name niwa-slash-commands))
         (kind (if local
                   'command
                 (niwa-view-invocation-kind name)))
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
      (niwa-view--send-local-goal display args)
      (when on-sent (funcall on-sent)))
     ;; `/plan ARGS\=' is Plan mode plus a turn: the arguments are the
     ;; prompt, and the command itself takes none.
     ((and local
           (string= name "plan")
           args
           (not (string-blank-p args)))
      (let ((data-buffer niwa--data-buffer))
        (niwa-view--submit-planned-input
         args
         (lambda ()
           (with-current-buffer data-buffer
             (niwa-plan-mode-enter))
           (when on-sent (funcall on-sent))))))
     (local
      (let ((result (with-current-buffer niwa--data-buffer
                      (funcall (cdr local) args))))
        ;; Most local slash commands don't send a turn.  A command may
        ;; return this sentinel when it took ownership of the input.
        (if (eq result 'niwa-view-sent)
            (when on-sent (funcall on-sent))
          (niwa-skills-message-command-result scope name result)
          (when on-quiet (funcall on-quiet)))))
     ((eq kind 'skill)
      ;; A skill is invoked with its own sigil and planned like any
      ;; other authored `$skill\=' line.
      (niwa-view--submit-planned-input
       (concat "$" name
               (if (and args (not (string-blank-p args)))
                   (concat " " args)
                 ""))
       (lambda () (when on-sent (funcall on-sent)))))
     (t
      (niwa-diagnostic-message (niwa-diagnostic-scope (current-buffer)) "Unknown slash command: /%s" name)
      (when on-quiet (funcall on-quiet))))))

(defun niwa-view--send-local-goal (input args)
  "Run pre-send check and start local `/goal' with ARGS.
INPUT is the original composer text, including the slash command."
  (let* ((view-buffer (current-buffer))
         (data-buffer niwa--data-buffer)
         (objective args)
         (submitted-draft (niwa-view--visible-draft)))
    (when (string-blank-p objective)
      (niwa-diagnostic-user-error (niwa-diagnostic-scope (current-buffer)) "Goal objective must not be blank"))
    (niwa-view--run-prompt-submit-hook
     objective input
     (lambda (submission)
       (when (and (buffer-live-p view-buffer)
                  (buffer-live-p data-buffer))
         (with-current-buffer view-buffer
           (niwa-view-history-add input)
           (niwa-view--clear-submitted-input submitted-draft))
         (with-current-buffer data-buffer
           (niwa-goal-start
           (niwa-prompt-submission-input submission) submission)
          ;; The Goal's first turn is scheduled, not started, so the wait
          ;; ends before it runs; a refused start keeps the wait.
          (niwa-permission-absence-clear niwa--session)))))))

(defun niwa-view--join-hook-contexts (&rest contexts)
  "Return CONTEXTS joined as separate hook context blocks."
  (let ((contexts (delq nil contexts)))
    (when contexts
      (mapconcat #'identity contexts "\n\n"))))

(defun niwa-view--run-prompt-submit-hook
    (input display-text callback &optional blocked-callback prior-context)
  "Run `UserPromptSubmit' for INPUT, then call CALLBACK if accepted.
DISPLAY-TEXT is the user-facing prompt text.  CALLBACK receives
one `niwa-prompt-submission'.  PRIOR-CONTEXT, when non-nil, is placed
after pending session context and before submit-hook context.  The consumer
commits the submission immediately after it durably records the accepted
input."
  (niwa-view--ensure-interactive-chat-view)
  (when niwa-view--prompt-hook-pending
    (niwa-diagnostic-user-error (niwa-diagnostic-scope (current-buffer)) "A prompt hook is still running -- wait or abort first"))
  (let ((view-buffer (current-buffer))
        (data-buffer niwa--data-buffer)
        session workspace submission scope)
    (unless (and data-buffer (buffer-live-p data-buffer))
      (niwa-diagnostic-user-error (niwa-diagnostic-scope (current-buffer)) "Data buffer has been killed"))
    (with-current-buffer data-buffer
      (setq session niwa--session
            workspace niwa--workspace))
    (setq scope (niwa-diagnostic-scope session))
    (setq submission
          (niwa-prompt-submission-create
           :input input :display-text display-text :session session)
          niwa-view--prompt-hook-pending submission)
    (condition-case err
        (with-current-buffer data-buffer
          (niwa-hooks-run-event
           'UserPromptSubmit
           (niwa-hooks-event-plist
            'UserPromptSubmit session workspace
            :prompt input
            :display-text display-text)
           (lambda (decision)
             (when (buffer-live-p view-buffer)
               (with-current-buffer view-buffer
                 (when (eq niwa-view--prompt-hook-pending submission)
                   (setq niwa-view--prompt-hook-pending nil)
                   (when (buffer-live-p data-buffer)
                     (setq decision
                           (niwa-hooks-sanitize-final-decision
                            'UserPromptSubmit decision scope))
                     (cond
                      ((and (plist-member decision :continue)
                            (not (plist-get decision :continue)))
                       (niwa-prompt-submission-cancel submission)
                       (niwa-hooks-record-session-context
                        session decision 'UserPromptSubmit)
                       (when blocked-callback
                         (funcall blocked-callback))
                       (niwa-diagnostic-message (niwa-diagnostic-scope session) "prompt blocked by hook: %s"
                                (or (plist-get decision :stop-reason)
                                    "no reason provided")))
                      (t
                       (when-let* ((msg (plist-get decision :system-message)))
                         (message "niwa: %s" msg))
                       (let* ((submitted
                               (if (stringp (plist-get decision :updated-input))
                                   (plist-get decision :updated-input)
                                 input))
                              (pending-entries
                               (niwa-session-hook-context-pending session))
                              (pending-context
                               (niwa-hooks-format-context pending-entries))
                              (submit-context
                               (niwa-hooks-additional-context-string
                                decision 'UserPromptSubmit))
                              (context
                               (niwa-view--join-hook-contexts
                                pending-context prior-context submit-context))
                              (audit
                               (niwa-view--prompt-rewrite-audit-record
                                'UserPromptSubmit input submitted decision)))
                         (when (fboundp
                                'niwa-plan-mode--invalidate-proposal)
                           (niwa-plan-mode--invalidate-proposal session))
                         (niwa-prompt-submission-accept
                          submission submitted context
                          (and audit (list audit)) pending-entries)
                         (funcall callback submission)))))))))
           session workspace nil nil))
      (error
       (when (eq niwa-view--prompt-hook-pending submission)
         (setq niwa-view--prompt-hook-pending nil))
       (niwa-prompt-submission-cancel submission)
       (signal (car err) (cdr err))))))

(cl-defun niwa-view--forward-input
    (input &key display-text before-send after-insert prompt-checked on-block
           submission model-input
           (submitted-draft (niwa-view--visible-draft)))
  "Render INPUT in the history region, forward to the data buffer, and send.
Helper for `niwa-view-send'.  When DISPLAY-TEXT is non-nil, show
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
`niwa-view--in-flight-turn-start' points into the view just above
the input zone (where the assistant turn will be rendered);
`niwa-view--data-turn-start' points into the data buffer just
after the forwarded prompt, where the LLM's response will begin."
  (cl-labels
      ((send-now (stored-input view-text accepted request-input)
         ;; Direct Worktree policy is staged in Target while its durable retry
         ;; belongs to Source.  It is not ambient authority for another prompt,
         ;; including a public send reentering from insertion/readiness hooks.
         (when (buffer-local-value 'niwa--pending-implementation-policy
                                   niwa--data-buffer)
           (signal 'niwa-stale-implementation-preparation nil))
         (when (niwa-turn-busy-p niwa--data-buffer)
           (niwa-diagnostic-user-error (niwa-diagnostic-scope niwa--data-buffer) "The session became busy before dispatch"))
         (when before-send
           (funcall before-send))
         (niwa-view--forward-input-now
          stored-input
          :display-text view-text
          :submission accepted
          :after-insert after-insert
          :model-input request-input
          :submitted-draft submitted-draft)))
    (if prompt-checked
        (send-now input (or display-text input) submission model-input)
      (niwa-view--run-prompt-submit-hook
       input display-text
       (lambda (accepted)
         (let ((hook-input
                (niwa-prompt-submission-input accepted))
               (context
                (niwa-prompt-submission-context accepted)))
           (send-now
            (if context
                (concat hook-input "\n\n" context)
              hook-input)
            (or display-text hook-input)
            accepted
            nil)))
       on-block))))

(cl-defun niwa-view--forward-input-now
    (input &key display-text submission after-insert model-input
           submitted-draft)
  "Forward INPUT to gptel immediately, after prompt hooks have run.
DISPLAY-TEXT is shown in the view instead of INPUT when non-nil.  SUBMISSION
supplies hook context, audits, and commit ownership.  MODEL-INPUT, when non-nil,
replaces INPUT only in the temporary request prompt.  SUBMITTED-DRAFT is the
composer text this submission captured; a draft the user changed while
asynchronous preparation ran is left alone instead of cleared."
  (niwa-view--ensure-interactive-chat-view)
  (when (buffer-local-value 'niwa-compact-run-in-flight niwa--data-buffer)
    (message "niwa: compacting, please wait...")
    (niwa-diagnostic-user-error (niwa-diagnostic-scope niwa--data-buffer) "Compaction in progress"))
  (niwa-session-durability-with-transaction
   (let* ((input (niwa--normalize-message-text input))
          (display-text (and display-text
                             (niwa--normalize-message-text display-text)))
          (hook-context
           (and submission
		(niwa-prompt-submission-context submission)))
          (hook-audits
           (and submission
		(niwa-prompt-submission-audits submission)))
          (prompt-summary-body
           (niwa-view--inline-skill-prompt-summary-body
            (or model-input input)))
          (data-buffer niwa--data-buffer)
          (session (niwa-view--session))
          (preparation
           (with-current-buffer data-buffer
             (niwa-implementation-policy-capture-preparation)))
          (_admission
           (progn
             (niwa-request-assert-target-ready session)
             (when session
               (with-current-buffer niwa--data-buffer
                 (niwa-readiness-assert
                  session (plist-get (niwa-skills-request-model-policy) :backend))))
             (when preparation
               (with-current-buffer data-buffer
                 (niwa-implementation-policy-assert-preparation preparation)))
             (when (memq session (bound-and-true-p niwa-claude-code-fork--sessions))
               (niwa-diagnostic-user-error
                (niwa-diagnostic-scope data-buffer)
                "Wait for native history preparation before starting a turn"))
             (when (niwa-turn-busy-p niwa--data-buffer)
               (niwa-diagnostic-user-error (niwa-diagnostic-scope data-buffer) "The session became busy before dispatch"))))
          (dropped-file-grants
           (niwa-view--pop-dropped-file-grants-for-input input session)))
     (let (data-turn-start
           hook-audits-with-source
           prompt-summary-source
           guest-attribution user-source)
       ;; Forward to the data buffer first so immediate inline-skill
       ;; Prompt handles can expand through the same source-backed fold
       ;; path as a full rerender.
       (with-current-buffer niwa--data-buffer
         (goto-char (point-max))
         (let ((body-start (niwa--insert-user-turn input)))
           (setq user-source
                 (niwa-view-disclosure-source-range
                  data-buffer body-start (point)))
           (when-let* ((prompt-summary-body)
                       (block
			(car (last
                              (niwa-tool-render-data-blocks input)))))
             (setq prompt-summary-source
                   (niwa-view-disclosure-source-range
                    data-buffer
                    (+ body-start (car block))
                    (+ body-start (cadr block))))))
         (dolist (audit hook-audits)
           (let ((audit-start (point)))
             (insert (niwa--format-hook-audit-record audit))
             (push (append audit
                           (list :source
                                 (niwa-view-disclosure-source-range
                                  data-buffer audit-start (point))))
                   hook-audits-with-source)))
         (setq hook-audits-with-source (nreverse hook-audits-with-source))
         ;; A collaboration guest's attribution must land here, with the
         ;; hook audits, before the response marker below exists: any
         ;; later insertion at the turn boundary is claimed by the
         ;; response span and would reach model context.
         (when niwa-view--pending-guest-attribution
           (setq guest-attribution niwa-view--pending-guest-attribution)
           (insert (niwa--format-hook-audit-record
                    niwa-view--pending-guest-attribution))
           (setq niwa-view--pending-guest-attribution nil))
         (setq niwa-view--turn-guest-attribution guest-attribution)
         ;; Anchor the data-side marker after the forwarded prompt so
         ;; incremental renders extract only the in-flight assistant
         ;; segments from here forward.  Pushed onto the view buffer's
         ;; buffer-local so live rendering can read it
         ;; without switching buffers.
         (setq data-turn-start (copy-marker (point) nil)))
       (niwa-collaboration--safe-accepted-prompt data-buffer)
       (when submission
         (niwa-prompt-submission-commit submission))
       (niwa-session-naming-consider session (or display-text input))
       (when after-insert
         (funcall after-insert))
       ;; Render the user's message in the view after the data source is
       ;; known, but before the model request starts.
       (let ((turn-start
              (niwa-view--insert-user-message
               (or display-text input) nil hook-context
               prompt-summary-body prompt-summary-source
               hook-audits-with-source guest-attribution user-source)))
         (niwa-view-stream-begin-turn turn-start data-turn-start)
         (niwa-view--clear-submitted-input submitted-draft))
       (with-current-buffer niwa--data-buffer
         (niwa-view--activate-dropped-file-grants
          dropped-file-grants session)
         (let ((gptel-send--handlers (copy-tree gptel-send--handlers))
               startup-fsm)
           ;; Capture the send's FSM before admission: startup can reenter and
           ;; install a different request before signaling an error.
           (when-let* ((wait (assq 'WAIT gptel-send--handlers)))
             (push (lambda (fsm) (unless startup-fsm (setq startup-fsm fsm)))
                   (cdr wait)))
           (condition-case err
               (niwa--dispatch-request model-input #'gptel-send input)
             ((error quit)
              (let ((request (and startup-fsm
                                  (plist-get (gptel-fsm-info startup-fsm)
                                             :niwa-request))))
                (when (or (null niwa--current-request)
                          (and request (eq request niwa--current-request)))
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
                    (niwa-view--append-request-summary
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
                  (when (buffer-live-p niwa--view-buffer)
                    (with-current-buffer niwa--view-buffer
                      (niwa-view-stream-stop)))
                  (when (eq request niwa--current-request)
                    (niwa-request-end))))
              (signal (car err) (cdr err))))))))))

(defun niwa-view--transform-model-input (fsm)
  "Replace the latest stored prompt with its one-shot model input for FSM."
  (when-let* ((chat-buffer (plist-get (gptel-fsm-info fsm) :buffer))
              ((buffer-live-p chat-buffer))
              (model-input
               (buffer-local-value 'niwa--pending-model-input
                                   chat-buffer)))
    (with-current-buffer chat-buffer
      (setq-local niwa--pending-model-input nil))
    (goto-char (niwa-transcript-prompt-transform-start))
    (delete-region (point) (point-max))
    (insert model-input)))

(defun niwa-view--agent-fsm-p (info data-buffer)
  "Return non-nil when INFO or DATA-BUFFER belongs to an agent request."
  ;; An autoload makes `fboundp' true without loading the struct metadata
  ;; that an inlined predicate needs.  Root requests must not load agents.
  (or (and (featurep 'niwa-agents)
           (niwa-agent-invocation-p
            (plist-get info :niwa-agent-invocation)))
      (and (buffer-live-p data-buffer)
           (with-current-buffer data-buffer
             niwa--agent-invocation))))

(defun niwa-view-abort ()
  "Abort the active request from the view buffer."
  (interactive)
  (with-current-buffer (niwa-view-composer-owner)
    (niwa-view--ensure-interactive-chat-view)
    (niwa-view--cancel-pending-submission)
    (niwa-view--stop-request-progress)
    (when (buffer-live-p niwa--data-buffer)
      (niwa-view--abort-data-buffer niwa--data-buffer))))


(provide 'niwa-view-composer)
;;; niwa-view-composer.el ends here
