;;; mevedel-view.el -- Compact view buffer for chat sessions -*- lexical-binding: t -*-

;;; Commentary:

;; Coordinates the user-facing view mode, session lifecycle, and managed
;; zones.  `mevedel-view-composer' owns editable input and submission;
;; `mevedel-view-render' owns the transcript projection.  The gptel data
;; buffer remains the authoritative conversation.
;;
;; Architecture:
;;   data buffer (org-mode, gptel) <--- authoritative
;;     |
;;     +---> view buffer (mevedel-view-mode) <--- user-facing
;;
;; The view buffer is ephemeral and always reconstructable from the
;; data buffer.

;;; Code:

(require 'cl-lib)
(require 'mevedel-utilities)
(require 'mevedel-transcript)

;; `cl-extra'
(declare-function cl-subseq "cl-extra" (seq start &optional end))

;; `cl-macs'
(declare-function cl-gensym "cl-macs" (&optional prefix))

;; `cl-seq'
(declare-function cl-position "cl-seq" (cl-item cl-seq &rest cl-keys))
(declare-function cl-find-if "cl-seq" (cl-pred cl-list &rest cl-keys))
(declare-function cl-find "cl-seq" (cl-item cl-seq &rest cl-keys))

;; `gptel'
(declare-function gptel--inject-prompt "ext:gptel-request"
                  (backend data new-prompt &optional position))
(declare-function gptel-fsm-info "ext:gptel-request" (cl-x) t)
(declare-function gptel-send "ext:gptel" (&optional arg))
(defvar gptel-prompt-prefix-alist)
(defvar gptel-response-separator)

;; `mevedel-menu'
(declare-function mevedel-menu "mevedel-menu" ())
(declare-function mevedel-menu-open "mevedel-menu" (area))

;; `mevedel-models'
(declare-function mevedel-model-current-label "mevedel-models"
                  (&optional buffer))

;; `mevedel-permissions'
(declare-function mevedel-permission-mode-effective "mevedel-permissions"
                  (&optional session data-buffer surface-buffer))
(declare-function mevedel-permission-mode-label "mevedel-permissions"
                  (&optional mode))
(declare-function mevedel-permission-mode-transition
                  "mevedel-permissions"
                  (mode &optional prompt display-text hook-context))
(defvar mevedel-permission-mode)

;; `transient'
(defvar transient-post-exit-hook)

;; `mevedel-chat'
(declare-function mevedel-abort "mevedel-chat" (&optional buf))

;; `mevedel-hooks'
(declare-function mevedel-hooks-additional-context-string "mevedel-hooks"
                  (decision &optional event))
(declare-function mevedel-hooks-decision-reason
                  "mevedel-hooks" (decision))
(declare-function mevedel-hooks-event-plist "mevedel-hooks"
                  (event &optional session workspace &rest extra))
(declare-function mevedel-hooks-format-context "mevedel-hooks"
                  (entries))
(declare-function mevedel-hooks-run-event "mevedel-hooks"
                  (event event-plist callback
                         &optional session workspace request invocation))
(declare-function mevedel-hooks-take-session-context "mevedel-hooks"
                  (session))

;; `mevedel-interaction-prompt'
(defvar mevedel--prompt-overlays)

;; `mevedel-permission-queue'
(declare-function mevedel-permission-queue--render-head
                  "mevedel-permission-queue" (&optional session))
(declare-function mevedel-permission-queue-abort-all
                  "mevedel-permission-queue" (&optional session))

;; `mevedel-structs'
(declare-function mevedel-request-begin "mevedel-structs"
                  (session &optional directive-uuid))
(declare-function mevedel-request-end "mevedel-structs" ())
(declare-function mevedel-request-started-at "mevedel-structs" (cl-x) t)
(declare-function mevedel-request-state-label "mevedel-structs"
                  (&optional buffer))
(declare-function mevedel-session-activate-dropped-file-grants
                  "mevedel-structs" (session paths))
(declare-function mevedel-session-add-dropped-file-grant
                  "mevedel-structs" (session path))
(declare-function mevedel-session-agent-transcripts
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-clear-dropped-file-grants
                  "mevedel-structs" (session))
(declare-function mevedel-session-hook-context-pending
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-name "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-permission-mode
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-permission-queue
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-plan-queue "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-pop-dropped-file-grants
                  "mevedel-structs" (session paths))
(declare-function mevedel-session-queued-user-messages
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-save-path "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-session-id "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-set-queued-user-messages
                  "mevedel-structs" (session queue))
(declare-function mevedel-session-skills "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-turn-count "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-workspace "mevedel-structs" (cl-x) t)
(declare-function mevedel-workspace-name "mevedel-structs" (cl-x) t)
(declare-function mevedel-workspace-root "mevedel-structs" (cl-x) t)
(declare-function mevedel-workspace-state-dir "mevedel-structs" (workspace))
(defvar mevedel--agent-invocation nil)
(defvar mevedel--compaction-in-flight nil)
(defvar mevedel--current-directive-uuid)
(defvar mevedel--current-request)
(defvar mevedel--data-buffer)
(defvar mevedel--session)
(defvar mevedel--view-buffer)
(defvar mevedel--workspace)

;; `mevedel-tool-plan'
(declare-function mevedel-plan-mode-extract-proposed-plan
                  "mevedel-tool-plan" (text))
(declare-function mevedel-plan-mode-known-proposed-plan-p
                  "mevedel-tool-plan" (plan-markdown &optional session))
(declare-function mevedel-plan-mode-strip-proposed-plans
                  "mevedel-tool-plan" (text))
(declare-function mevedel-plan-queue--render-head
                  "mevedel-tool-plan" (&optional session))
(declare-function mevedel-plan-queue-abort-all
                  "mevedel-tool-plan" (&optional session))

;; `mevedel-tool-task'
(declare-function mevedel-tool-task--display-string
                  "mevedel-tool-task" (session show-completed))
(declare-function mevedel-tool-task--session-has-active-p
                  "mevedel-tool-task" (session))
(declare-function mevedel-toggle-tasks "mevedel-tool-task" ())
(defvar mevedel-tool-task--status-keymap)

;; `mevedel-workspace'
(declare-function mevedel-workspace-ensure-generated-state-ignored
                  "mevedel-workspace" (workspace))

;; `mevedel-tools'
(declare-function mevedel-tools-active-count "mevedel-tools"
                  (&optional buffer))

;; `mevedel-review'
(declare-function mevedel-review--mark-command-outcome
                  "mevedel-review" (outcome))
(declare-function mevedel-review-command-skill-p
                  "mevedel-review" (skill))

;; `mevedel-view-audit'
(declare-function mevedel-view--insert-hook-audit-block
                  "mevedel-view-audit" (record &optional source expanded))
(declare-function mevedel-view--prompt-rewrite-audit-record
                  "mevedel-view-audit" (event original submitted decision))

;; `mevedel-view-composer'
(declare-function mevedel-view--call-preserving-input-point
                  "mevedel-view-composer" (thunk))
(declare-function mevedel-view--call-preserving-input-text
                  "mevedel-view-composer" (thunk))
(declare-function mevedel-view--call-preserving-window-state
                  "mevedel-view-composer" (thunk))
(declare-function mevedel-view--effective-permission-mode
                  "mevedel-view-composer" ())
(declare-function mevedel-view--input-marker-position
                  "mevedel-view-composer" ())
(declare-function mevedel-view--input-prompt-string
                  "mevedel-view-composer" (&optional mode))
(declare-function mevedel-view--permission-mode-display
                  "mevedel-view-composer" (mode))
(declare-function mevedel-view--position-in-input-region-p
                  "mevedel-view-composer" (position))
(declare-function mevedel-view--queued-user-messages-render
                  "mevedel-view-composer" (&optional session))
(declare-function mevedel-view--transcript-gptel-send-blocked
                  "mevedel-view-composer" (&optional arg))
(declare-function mevedel-view-abort "mevedel-view-composer" ())
(declare-function mevedel-view-clear-queued-messages
                  "mevedel-view-composer" ())
(declare-function mevedel-view-composer-initialize
                  "mevedel-view-composer" ())
(declare-function mevedel-view-cycle-permission-mode
                  "mevedel-view-composer" ())
(declare-function mevedel-view-edit-last-queued-message
                  "mevedel-view-composer" ())
(declare-function mevedel-view-send "mevedel-view-composer" ())
(declare-function mevedel-view-yank-dwim
                  "mevedel-view-composer" (&optional arg))
(defvar mevedel-view--input-marker)

;; `mevedel-view-markdown'
(autoload 'mevedel-view--normalize-local-file-uri-path
  "mevedel-view-markdown")

;; `mevedel-preview-mode'
(defvar mevedel-preview-mode--pending)

;; `mevedel-agents'
(declare-function mevedel-agent-name "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-agent "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-p "mevedel-agents" (cl-x))
(declare-function mevedel-agent-invocation-agent-id "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-description "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-call-count "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-background-agents "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-started-at "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-transcript-status "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-terminal-reason "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-verdict "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-parent-context "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-parent-data-buffer "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-buffer "mevedel-agents" (cl-x) t)

;; `org'
(declare-function org-entry-get "ext:org" (pom property &optional inherit literal-nil))
(declare-function org-mode "ext:org" ())
(declare-function org-unescape-code-in-string "ext:org-src" (s))
(defvar org-mode-hook)

;; `mevedel-tool-registry'
(declare-function mevedel-tool-display-string "mevedel-tool-registry" (tool-name args))
(declare-function mevedel-tool-get "mevedel-tool-registry" (name &optional category))
(declare-function mevedel-tool-name "mevedel-tool-registry" (cl-x) t)
(declare-function mevedel-tool-renderer "mevedel-tool-registry" (cl-x) t)

;; `mevedel-tool-repair'
(declare-function mevedel-tool-repair-format-path
                  "mevedel-tool-repair" (path))
(declare-function mevedel-tool-repair-normalize-audit-record
                  "mevedel-tool-repair-diagnostics" (record))

;; `mevedel-transcript-audit'
(declare-function mevedel-transcript-audit-only-p
                  "mevedel-transcript-audit" (text))

;; `mevedel-transcript-restore'
(declare-function mevedel-transcript-restore-properties
                  "mevedel-transcript-restore" (&optional only-if-missing))

;; `mevedel-tool-ui'
(declare-function mevedel-tool-ui--handle-badge "mevedel-tool-ui" (render-data))
(declare-function mevedel-tool-ui--render-agent
                  "mevedel-tool-ui" (name args result render-data))

;; `mevedel-agent-runtime'
(declare-function mevedel-agent-runtime--agent-invocation-at
                  "mevedel-agent-runtime" (fsm))
(declare-function mevedel-agent-runtime--prune-stale-agents-fsm
                  "mevedel-agent-runtime" ())
(declare-function mevedel-agent-runtime-display-label
                  "mevedel-agent-runtime" (agent-id))
(defvar mevedel-agent-runtime--fsms nil)

;; `mevedel-pipeline'
(declare-function mevedel-pipeline-extract-render-data
                  "mevedel-pipeline"
                  (result-string &optional session expected-tool-use-id
                                 allow-payload-tool-use-id))
(declare-function mevedel-pipeline--format-render-data-block
                  "mevedel-pipeline" (render-data))
(declare-function mevedel-pipeline--strip-render-data-blocks
                  "mevedel-pipeline" (string))

;; `mevedel-skills'
(declare-function mevedel-session-get-skill "mevedel-skills" (session name))
(declare-function mevedel-skill-context "mevedel-skills" (cl-x) t)
(declare-function mevedel-skill-name "mevedel-skills" (cl-x) t)
(declare-function mevedel-skill-user-invocable-p "mevedel-skills" (cl-x) t)
(declare-function mevedel-skills--clear-pending-inline-attachments
                  "mevedel-skills" ())
(declare-function mevedel-skills--inline-skill-mentions
                  "mevedel-skills" (text session))
(declare-function mevedel-skills--insert-fork-result "mevedel-skills" (outcome))
(declare-function mevedel-skills--parse-skill-line "mevedel-skills" (text))
(declare-function mevedel-skills--parse-slash-line "mevedel-skills" (text))
(declare-function mevedel-skills--prepare-inline-attachments-for-text
                  "mevedel-skills" (text session callback))
(declare-function mevedel-skills--remaining-argument-hint
                  "mevedel-skills" (skill arguments))
(declare-function mevedel-skills--slash-annotation
                  "mevedel-skills" (name buffer session local-commands))
(declare-function mevedel-skills--slash-capf
                  "mevedel-skills" (buffer session local-commands
                                            &optional input-start))
(declare-function mevedel-skills--slash-completion-table
                  "mevedel-skills" (buffer session local-commands))
(declare-function mevedel-skills--stage-inline-attachments
                  "mevedel-skills" (attachments))
(declare-function mevedel-skills-format-inline-render-data
                  "mevedel-skills" (skill arguments))
(declare-function mevedel-skills-inline-display-text
                  "mevedel-skills" (name arguments))
(declare-function mevedel-skills-install-font-lock "mevedel-skills" ())
(declare-function mevedel-skills-invoke "mevedel-skills" t t)
(defvar mevedel-slash-commands)

;; `mevedel-review'
(declare-function mevedel-review-strip-user-action-blocks
                  "mevedel-review" (text))
(declare-function mevedel-review-transform-outcome
                  "mevedel-review" (skill-name outcome))

;; `mevedel-mentions'
(declare-function mevedel-mentions-install "mevedel-mentions" ())
(declare-function mevedel-mentions-file-paths-in-text
                  "mevedel-mentions" (text))

;; `dnd'
(declare-function dnd-get-local-file-name "dnd" (uri &optional must-exist))
(defvar dnd-protocol-alist)

;; `browse-url'
(declare-function browse-url "browse-url" (url &optional new-window))

;; `select'
(declare-function gui-get-selection "select" (selection-symbol target-type))

;; `mevedel-session-persistence'
(declare-function mevedel-session-persistence-fork-now
                  "mevedel-session-persistence" (buffer))
(declare-function mevedel-session-persistence--find-file-noselect
                  "mevedel-session-persistence" (file))

;; `mevedel-view-history'
(declare-function mevedel-view-history-add "mevedel-view-history" (input))
(declare-function mevedel-view-history-beginning-of-line
                  "mevedel-view-history" (&optional arg))
(declare-function mevedel-view-history-browse "mevedel-view-history" ())
(declare-function mevedel-view-history-clear-input
                  "mevedel-view-history" ())
(declare-function mevedel-view-history-load "mevedel-view-history"
                  (&optional session))
(declare-function mevedel-view-history-next "mevedel-view-history" ())
(declare-function mevedel-view-history-previous "mevedel-view-history" ())
(declare-function mevedel-view-history-save "mevedel-view-history"
                  (&optional view-buffer))
(declare-function mevedel-view-history-search "mevedel-view-history" ())
(declare-function mevedel-session-persistence--validate-transcript-path
                  "mevedel-session-persistence" (path save-path))
(defvar mevedel-session--fork-pending)
(defvar mevedel-session--read-only-mode)

;; `gptel'
(declare-function gptel--update-status "ext:gptel" (msg &optional face))

;; `subr'
(defvar read-eval)

;; `mevedel-view-render'
(declare-function mevedel-view--add-display-region-properties
                  "mevedel-view-render" (start end &optional type))
(declare-function mevedel-view--after-header-position
                  "mevedel-view-render" ())
(declare-function mevedel-view--agent-invocation
                  "mevedel-view-render" (agent-id))
(declare-function mevedel-view--agent-status-blocked-p
                  "mevedel-view-render" (entry))
(declare-function mevedel-view--debug-log
                  "mevedel-view-render" (event &rest fields))
(declare-function mevedel-view--fontify-directive-display-text
                  "mevedel-view-render" (text))
(declare-function mevedel-view--full-rerender
                  "mevedel-view-render" ())
(declare-function mevedel-view--history-insertion-marker
                  "mevedel-view-render" ())
(declare-function mevedel-view--history-tail-position
                  "mevedel-view-render" ())
(declare-function mevedel-view--hook-context-events-from-text
                  "mevedel-view-render" (text))
(declare-function mevedel-view--inline-skill-prompt-summary-body
                  "mevedel-view-render" (render-data))
(declare-function mevedel-view--insert-hook-context-block
                  "mevedel-view-render" (events &optional source expanded))
(declare-function mevedel-view--insert-rendered-tool
                  "mevedel-view-render" (rendering source))
(declare-function mevedel-view--mailbox-collapse-hint
                  "mevedel-view-render" (line-count))
(declare-function mevedel-view--non-history-view-position-p
                  "mevedel-view-render" (pos))
(declare-function mevedel-view--queue-has-origin-p
                  "mevedel-view-render" (queue origin))
(declare-function mevedel-view--record-source-collapse-state
                  "mevedel-view-render" (source vtype collapsed))
(declare-function mevedel-view--rendering-with-collapse-state
                  "mevedel-view-render" (rendering source))
(declare-function mevedel-view--section-bounds
                  "mevedel-view-render" ())
(declare-function mevedel-view--segment-rendering
                  "mevedel-view-render"
                  (data-buf seg-start seg-end &optional collapsed-only))
(declare-function mevedel-view--source-collapse-state-entry
                  "mevedel-view-render" (source vtype))
(declare-function mevedel-view--tool-call-parse
                  "mevedel-view-render"
                  (data-buf seg-start seg-end &optional raw))
(declare-function mevedel-view-next-display "mevedel-view-render" ())
(declare-function mevedel-view-previous-display "mevedel-view-render" ())
(declare-function mevedel-view-render-initialize
                  "mevedel-view-render" ())
(declare-function mevedel-view-reset-agent-ephemeral-state
                  "mevedel-view-render" (&optional data-buf))
(declare-function mevedel-view-toggle-section "mevedel-view-render" ())
(declare-function mevedel-view-toggle-transcript "mevedel-view-render" ())
(defvar mevedel-view--user-pre-rendered)

;; `mevedel-view-stream'
(declare-function mevedel-view--delete-pending-tool-live-lines
                  "mevedel-view-stream" ())
(declare-function mevedel-view--ensure-request-progress
                  "mevedel-view-stream" (&optional data-buf status))
(declare-function mevedel-view--in-flight-turn-start-position
                  "mevedel-view-stream" ())
(declare-function mevedel-view--insert-pending-tool-lines
                  "mevedel-view-stream" (entries))
(declare-function mevedel-view--request-progress-region-start
                  "mevedel-view-stream" ())
(declare-function mevedel-view--set-in-flight-turn-start
                  "mevedel-view-stream" (position))
(declare-function mevedel-view--spinner-frame "mevedel-view-stream" ())
(declare-function mevedel-view--spinner-region-p
                  "mevedel-view-stream" (start end))
(declare-function mevedel-view--start-spinner
                  "mevedel-view-stream" (&optional status))
(declare-function mevedel-view--stop-request-progress
                  "mevedel-view-stream" ())
(declare-function mevedel-view-stream-begin-turn
                  "mevedel-view-stream"
                  (view-turn-start data-turn-start &optional no-spinner))
(declare-function mevedel-view-stream-active-response-marker
                  "mevedel-view-stream" (info data-buffer))
(defvar mevedel-view--data-turn-start)
(defvar mevedel-view--in-flight-turn-start)
(defvar mevedel-view--pending-tool-calls)

;; `mevedel-view-zone'
(declare-function mevedel-view-zone-collapse-state
                  "mevedel-view-zone" (key &optional default))
(declare-function mevedel-view-zone-collapse-state-set-p
                  "mevedel-view-zone" (key))
(declare-function mevedel-view-zone-clear
                  "mevedel-view-zone" (zone))
(declare-function mevedel-view-zone-forget
                  "mevedel-view-zone" (&optional zone))
(declare-function mevedel-view-zone-fragment-bounds
                  "mevedel-view-zone" (zone id))
(declare-function mevedel-view-zone-next
                  "mevedel-view-zone" (&optional limit))
(declare-function mevedel-view-zone-previous
                  "mevedel-view-zone" (&optional limit))
(declare-function mevedel-view-zone-reconcile
                  "mevedel-view-zone" (zone start end fragments))
(declare-function mevedel-view-zone-region
                  "mevedel-view-zone" (zone))
(declare-function mevedel-view-zone-set-collapse-state
                  "mevedel-view-zone" (key collapsed))
(declare-function mevedel-view-zone-start
                  "mevedel-view-zone" (zone))
(declare-function mevedel-view-zone-toggle-collapsed
                  "mevedel-view-zone" (&optional position))


;;
;;; Customization

(defcustom mevedel-view-inline-image-max-width 600
  "Maximum pixel width for inline images rendered in the view."
  :type 'integer
  :group 'mevedel)


(defface mevedel-view-separator
  '((t :inherit shadow :extend t))
  "Face for separator lines in the view buffer."
  :group 'mevedel)

(defface mevedel-view-header
  '((t :inherit (bold shadow) :overline t :extend t))
  "Face for the session header at the top of the view buffer."
  :group 'mevedel)

(defface mevedel-view-user-header
  '((t :inherit bold :overline t :extend t))
  "Face for user message headers in the view buffer."
  :group 'mevedel)

(defface mevedel-view-directive-action
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face for directive action labels in the view buffer."
  :group 'mevedel)

(defface mevedel-view-assistant-header
  '((t :inherit (bold font-lock-function-name-face) :overline t :extend t))
  "Face for assistant message headers in the view buffer."
  :group 'mevedel)

(defface mevedel-view-tool-summary
  '((t :inherit default))
  "Face for collapsed tool call summaries."
  :group 'mevedel)

(defface mevedel-view-tool-marker
  '((t :inherit success :weight bold))
  "Face for successful tool summary markers."
  :group 'mevedel)

(defface mevedel-view-tool-name
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face for tool names in collapsed summaries."
  :group 'mevedel)

(defface mevedel-view-tool-argument
  '((t :inherit font-lock-string-face))
  "Face for primary tool arguments in collapsed summaries."
  :group 'mevedel)

(defface mevedel-view-tool-metadata
  '((t :inherit shadow))
  "Face for line counts and secondary metadata in summaries."
  :group 'mevedel)

(defface mevedel-view-tool-diff-added
  '((t :inherit success :weight bold))
  "Face for added-line counts in Edit and Write summaries."
  :group 'mevedel)

(defface mevedel-view-tool-diff-removed
  '((t :inherit error :weight bold))
  "Face for removed-line counts in Edit and Write summaries."
  :group 'mevedel)

(defface mevedel-view-tool-warning
  '((t :inherit warning :weight bold))
  "Face for blocked or warning tool summary markers."
  :group 'mevedel)

(defface mevedel-view-hook-context
  '((t :inherit (shadow italic)))
  "Face for hook context indicators in user turns."
  :group 'mevedel)

(defface mevedel-view-hook-audit
  '((t :inherit (shadow italic)))
  "Face for hook audit indicators in transcript turns."
  :group 'mevedel)

(defface mevedel-view-thinking-summary
  '((t :inherit (shadow italic)))
  "Face for collapsed thinking/reasoning summaries."
  :group 'mevedel)

(defface mevedel-view-system-reminder
  '((t :inherit shadow))
  "Face for collapsed system reminder summaries."
  :group 'mevedel)

(defface mevedel-view-thinking-marker
  '((t :inherit (shadow italic)))
  "Face for thinking/reasoning summary markers."
  :group 'mevedel)

(defface mevedel-view-response-summary
  '((t :inherit shadow))
  "Face for collapsed response summaries."
  :group 'mevedel)

(defface mevedel-view-response-marker
  '((t :inherit font-lock-function-name-face :weight bold))
  "Face for collapsed response summary markers."
  :group 'mevedel)

(defface mevedel-view-source-block
  '((t :inherit org-block :foreground unspecified :extend t))
  "Face for rendered Markdown source block panels."
  :group 'mevedel)

(defface mevedel-view-source-block-language
  '((t :inherit (italic font-lock-type-face mevedel-view-source-block)))
  "Face for rendered Markdown source block language labels."
  :group 'mevedel)

(defface mevedel-view-spinner
  '((t :inherit (bold font-lock-comment-face)))
  "Face for the spinner status line."
  :group 'mevedel)

(defface mevedel-view-turn-rule
  '((t :inherit shadow :overline t :extend t))
  "Face for the horizontal rule that closes an assistant turn."
  :group 'mevedel)

(defface mevedel-view-activity-rule
  '((t :inherit shadow :overline t :extend t))
  "Face for the separator before assistant activity rows."
  :group 'mevedel)

(defface mevedel-view-input-prompt
  '((t :inherit shadow :weight bold))
  "Face for the read-only `> ' prompt in the input zone."
  :group 'mevedel)

(defface mevedel-view-permission-mode-default
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face for the default permission mode prompt label."
  :group 'mevedel)

(defface mevedel-view-permission-mode-plan
  '((t :inherit font-lock-warning-face :weight bold))
  "Face for the plan permission mode prompt label."
  :group 'mevedel)

(defface mevedel-view-permission-mode-accept-edits
  '((t :inherit success :weight bold))
  "Face for the accept-edits permission mode prompt label."
  :group 'mevedel)

(defface mevedel-view-permission-mode-trust-all
  '((t :inherit error :weight bold))
  "Face for the trust-all permission mode prompt label."
  :group 'mevedel)

(defface mevedel-view-zone-separator
  '((t :inherit shadow))
  "Face for status / interaction zone separator lines."
  :group 'mevedel)

(defface mevedel-view-ephemeral
  '((t :inherit shadow))
  "Face for ephemeral live-tail lines (spinner, \"Calling X…\")."
  :group 'mevedel)

(defface mevedel-view-attribution
  '((t :inherit (link shadow)))
  "Face for the `from <type>--<idshort>' fragment.
Click target on handles, mailbox blocks, plan summaries, and
permission prompts."
  :group 'mevedel)

(defface mevedel-view-mailbox-gutter
  '((t :inherit mevedel-view-tool-metadata))
  "Face for the gutter prefix on expanded mailbox deliveries."
  :group 'mevedel)

(defface mevedel-view-mailbox-body
  '((t :inherit default))
  "Face for expanded mailbox delivery body text."
  :group 'mevedel)

(defface mevedel-view-handle-running
  '((t :inherit bold))
  "Face for the `[running · N calls]' handle badge."
  :group 'mevedel)

(defface mevedel-view-agent-running
  '((t :inherit (font-lock-escape-face bold)))
  "Face for active running agent handle rows.
This follows gptel-agent's own active-agent status styling."
  :group 'mevedel)

(defface mevedel-view-handle-blocked
  '((t :inherit warning))
  "Face for the `[blocked · awaiting …]' handle badge."
  :group 'mevedel)

(defface mevedel-view-handle-done
  '((t :inherit success))
  "Face for the `✓ done · …' handle badge."
  :group 'mevedel)

(defface mevedel-view-handle-error
  '((t :inherit error))
  "Face for the `✗ error · …' / `✗ aborted' handle badges."
  :group 'mevedel)

(defcustom mevedel-view-pending-tools-visible-max 5
  "Maximum number of `Calling X…' lines shown in the live tail.
When more tools are in flight than this cap, the visible lines are
the most recent and the rest are summarised in a single tail line."
  :type 'integer
  :group 'mevedel)

(defcustom mevedel-agent-view-display-action
  '(display-buffer-in-side-window
    (side . right) (slot . 0) (window-width . 0.4))
  "Action passed to `pop-to-buffer' when opening an agent transcript.
Consulted by `mevedel-view-open-agent-transcript' so callers do not
need to set `display-buffer-overriding-action'.  Power users can
override globally or via `display-buffer-alist'."
  :type 'sexp
  :group 'mevedel)

;;
;;; Buffer-locals

(defvar-local mevedel-view--status-marker nil
  "Marker separating the history region from the status zone.
Insertion-type t so history-content insertion advances it; status-zone
content renders here as read-only text.")

(defvar-local mevedel-view--interaction-marker nil
  "Marker separating the status zone from the interaction zone.
Insertion-type t so status content above advances it; interaction-zone
overlays anchor here.  Permission queue head, plan confirmation, and
preview overlays render against this marker.")

(defvar-local mevedel-view--interaction-descriptors nil
  "Hash table of live interaction-zone descriptors keyed by descriptor id.")

(defvar-local mevedel-view--interaction-overlays nil
  "Hash table of live interaction-zone overlays keyed by descriptor id.")

(defvar-local mevedel-view--agent-transcript-p nil
  "Non-nil when this view buffer renders an agent transcript for inspection.")

(defvar-local mevedel-view--agent-id nil
  "Canonical agent id rendered by an agent transcript inspection view.")

(defvar-local mevedel-view--agent-transcript-info nil
  "Resolved transcript metadata plist for an agent transcript inspection view.")

(defvar-local mevedel-view--agent-transcript-parent-view nil
  "Parent session view buffer that opened this transcript inspection view.")

(defvar-local mevedel-view--agent-transcript-window nil
  "Side window currently displaying an agent transcript for this view buffer.")

(defvar mevedel-view--agent-transcript-data-kill-in-progress nil
  "Non-nil while transcript view/data kill hooks are already paired.")

(defconst mevedel-view--status-task-collapse-key '(status tasks)
  "Stable fragment collapse key for the task status block.")

(defconst mevedel-view--status-agent-collapse-key '(status agents)
  "Stable fragment collapse key for the aggregate agent status block.")

(defvar-local mevedel-view--agent-refresh-timers nil
  "Hash table of pending coalesced agent refresh timers by agent id.")


(defcustom mevedel-view-spinner-animate t
  "Non-nil means animate view buffer spinner glyphs."
  :type 'boolean
  :group 'mevedel)

(defcustom mevedel-view-spinner-interval 0.12
  "Seconds between view buffer spinner frame updates."
  :type 'number
  :group 'mevedel)

(defconst mevedel-view-spinner-braille-frames
  '("⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏")
  "Braille Pattern frames for animated view buffer spinners.")

(defconst mevedel-view-spinner-ascii-frames
  '("-" "\\" "|" "/")
  "ASCII fallback frames for animated view buffer spinners.")

(defcustom mevedel-view-spinner-frames
  mevedel-view-spinner-braille-frames
  "Frames used for animated view buffer spinners.
The default frames are Braille Pattern Unicode code points U+280B,
U+2819, U+2839, U+2838, U+283C, U+2834, U+2826, U+2827, U+2807,
and U+280F.  If your font does not render these glyphs, set this to
`mevedel-view-spinner-ascii-frames'."
  :type '(repeat string)
  :group 'mevedel)


;;
;;; Major mode

(defvar-keymap mevedel-view--display-map
  :doc "Keymap active in the read-only history/status/interaction area.
Applied via the `keymap' text property so these bindings only fire
above `mevedel-view--input-marker'."
  "TAB" #'mevedel-view-toggle-section
  "RET" #'mevedel-view-activate-at-point
  "<mouse-1>" #'mevedel-view-activate-at-point
  "<mouse-2>" #'mevedel-view-activate-at-point
  "n" #'mevedel-view-next-display
  "p" #'mevedel-view-previous-display
  "t" #'mevedel-view-toggle-transcript
  "q" #'mevedel-view-close-agent-transcript)

(defvar-keymap mevedel-view--agent-handle-map
  :doc "Keymap active on non-label text in Agent handles.
The visible agent type label carries its own transcript-opening
keymap; the rest of the handle remains navigable without opening the
transcript on click."
  "TAB" #'mevedel-view-toggle-section
  "n" #'mevedel-view-next-display
  "p" #'mevedel-view-previous-display
  "t" #'mevedel-view-toggle-transcript
  "q" #'mevedel-view-close-agent-transcript)

(defvar-keymap mevedel-view--agent-label-map
  :doc "Keymap active on the visible agent type label in Agent handles."
  "RET" #'mevedel-view-activate-at-point
  "<mouse-1>" #'mevedel-view-activate-at-point
  "<mouse-2>" #'mevedel-view-activate-at-point)

(defvar-keymap mevedel-view-mode-map
  :doc "Keymap for `mevedel-view-mode'."
  "C-c RET" #'mevedel-view-send
  "C-c C-k" #'mevedel-view-abort
  "C-c C-o" #'mevedel-menu
  "C-c C-l" #'mevedel-view-history-browse
  "C-c C-u" #'mevedel-view-history-clear-input
  "C-c C-e" #'mevedel-view-edit-last-queued-message
  "C-c C-q" #'mevedel-view-clear-queued-messages
  "C-y" #'mevedel-view-yank-dwim
  "M-p" #'mevedel-view-history-previous
  "M-n" #'mevedel-view-history-next
  "M-r" #'mevedel-view-history-search)

(defun mevedel-view--display-fragment-keymap (&rest maps)
  "Return a composed display-fragment keymap from MAPS.
MAPS take precedence, with `mevedel-view--display-map' providing shared
navigation and activation fallbacks."
  (make-composed-keymap
   (delq nil (append maps (list mevedel-view--display-map)))))

(defun mevedel-view--status-task-keymap ()
  "Return the `view-buffer' keymap for the task status fragment."
  (mevedel-view--display-fragment-keymap
   (define-keymap
     "<tab>" #'mevedel-view-toggle-section
     "TAB" #'mevedel-view-toggle-section
     "<return>" #'mevedel-view-activate-at-point
     "RET" #'mevedel-view-activate-at-point)
   mevedel-tool-task--status-keymap))

(define-key mevedel-view-mode-map
            [remap move-beginning-of-line]
            #'mevedel-view-history-beginning-of-line)
(define-key mevedel-view-mode-map (kbd "<backtab>")
            #'mevedel-view-cycle-permission-mode)
(define-key mevedel-view-mode-map (kbd "S-TAB")
            #'mevedel-view-cycle-permission-mode)

(defun mevedel-view--enforce-ephemeral (&rest _)
  "Keep the current view buffer out of Emacs save machinery."
  (setq buffer-file-name nil
        buffer-file-truename nil
        buffer-file-number nil)
  (setq-local buffer-offer-save nil)
  (setq-local buffer-auto-save-file-name nil)
  (setq-local buffer-save-without-query nil)
  (setq-local auto-save-default nil)
  (setq-local make-backup-files nil)
  (setq-local create-lockfiles nil)
  (set-buffer-modified-p nil))

(define-derived-mode mevedel-view-mode text-mode "MevView"
  "Major mode for the mevedel chat view buffer.

Displays a compact rendering of the gptel data buffer.  Interactive view
buffers are ordered as history region, status zone, interaction zone,
request progress row, and input zone.  The input zone starts at
`mevedel-view--input-marker' with a read-only prompt prefix followed by
the editable composer body.

\\{mevedel-view-mode-map}"
  (visual-line-mode +1)
  (setq-local window-point-insertion-type t)
  (auto-save-mode -1)
  (mevedel-view--enforce-ephemeral)
  (add-hook 'after-change-functions
            #'mevedel-view--enforce-ephemeral nil t)
  (add-hook 'post-command-hook
            #'mevedel-view--enforce-ephemeral nil t))


;;
;;; Header helper

(defun mevedel-view--header-string (data-buf)
  "Return the read-only session-header string for DATA-BUF.
The line shows \"SESSION @ WORKSPACE\" using the `mevedel-view-header'
face and carries the display-region text properties so it participates
in read-only enforcement and section navigation."
  (let* ((session (buffer-local-value 'mevedel--session data-buf))
         (ws (and session (mevedel-session-workspace session)))
         (label (if session
                    (format "%s @ %s"
                            (mevedel-session-name session)
                            (or (and ws (mevedel-workspace-name ws))
                                "mevedel"))
                  "mevedel")))
    (propertize (concat label "\n")
                'read-only t
                'keymap mevedel-view--display-map
                'front-sticky '(read-only keymap)
                'rear-nonsticky '(read-only keymap)
                'font-lock-face 'mevedel-view-header)))

(defun mevedel-view--setup (view-buf data-buf &optional options)
  "Initialize VIEW-BUF as the view buffer for DATA-BUF.
Activates `mevedel-view-mode', wires the cross-references, and
inserts the initial separator with input marker.

OPTIONS is a plist.  When `:agent-transcript-p' is non-nil, create
a read-only transcript inspection view instead of an interactive chat
view.  When `:preserve-data-view-buffer' is non-nil, leave DATA-BUF's
  existing `mevedel--view-buffer' binding untouched."
  (require 'mevedel-view-composer)
  (require 'mevedel-view-history)
  (require 'mevedel-view-render)
  (require 'mevedel-view-stream)
  (with-current-buffer view-buf
    (mevedel-view-mode)
    (mevedel-view--enforce-ephemeral)
    (setq-local mevedel--data-buffer data-buf)
    (setq-local mevedel--session
                (and (buffer-live-p data-buf)
                     (buffer-local-value 'mevedel--session data-buf)))
    (setq-local mevedel-view--agent-transcript-p
                (and (plist-get options :agent-transcript-p) t))
    (setq-local mevedel-view--agent-id (plist-get options :agent-id))
    (setq-local mevedel-view--agent-transcript-info
                (plist-get options :transcript-info))
    (setq-local mevedel-view--agent-transcript-parent-view
                (plist-get options :parent-view))
    (setq-local mevedel-view--agent-refresh-timers
                (make-hash-table :test #'equal))
    (mevedel-view-render-initialize)
    (setq-local mevedel-view--interaction-descriptors
                (make-hash-table :test #'equal))
    (setq-local mevedel-view--interaction-overlays
                (make-hash-table :test #'equal))
    ;; Copy workspace directory so relative paths resolve correctly
    (setq-local default-directory
                (buffer-local-value 'default-directory data-buf))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (if mevedel-view--agent-transcript-p
          (let ((start (point)))
            (setq mevedel-view--status-marker (copy-marker start t))
            (setq mevedel-view--interaction-marker (copy-marker start t))
            (setq mevedel-view--input-marker (copy-marker start nil)))
        ;; Insert session header and set up zone markers.
        ;;
        ;; Three markers carve the buffer above the input prompt into
        ;; four zones (history / status / interaction / input).
        (insert (mevedel-view--header-string data-buf))
        ;; Insert the prompt first, then place all three zone markers
        ;; at start-of-prompt.  Order matters because the status and
        ;; interaction markers have insertion-type t.
        (let ((start (point)))
          (insert (mevedel-view--input-prompt-string))
          (add-text-properties
           start (point)
           `(read-only t
             mevedel-view-prompt t
             front-sticky (read-only mevedel-view-prompt)
             rear-nonsticky (read-only mevedel-view-prompt font-lock-face)))
          (setq mevedel-view--status-marker (copy-marker start t))
          (setq mevedel-view--interaction-marker (copy-marker start t))
          (setq mevedel-view--input-marker (copy-marker start nil)))))
    (mevedel-view-composer-initialize)
    ;; Kill-buffer lifecycle: view killed -> clear ref on data buffer
    (add-hook 'kill-buffer-hook #'mevedel-view--on-view-killed nil t)
    (add-hook 'kill-buffer-hook #'mevedel-view-history-save nil t)
    (when mevedel-view--agent-transcript-p
      ;; Transcript inspection binds a few keys differently from the
      ;; interactive parent view.  Copy the major-mode map before
      ;; installing those bindings so they do not leak back to normal
      ;; chat buffers.
      (use-local-map (copy-keymap mevedel-view-mode-map))
      (add-hook 'kill-buffer-hook
                #'mevedel-view--on-agent-transcript-view-killed nil t)
      (local-set-key (kbd "q") #'mevedel-view-close-agent-transcript)
      (local-set-key [remap gptel-send]
                     #'mevedel-view--transcript-gptel-send-blocked))
    (setq header-line-format
          (if mevedel-view--agent-transcript-p
              '(:eval (mevedel-view--agent-transcript-header-line))
            '(:eval (mevedel-view--status-strip)))))
  ;; Wire the reverse reference on the data buffer.  Live agent buffers
  ;; keep this pointing at the interactive parent view so their queued
  ;; permission/Ask/plan overlays remain visible while a read-only
  ;; transcript inspection view is open.
  (when (and (plist-get options :agent-transcript-p)
             (plist-get options :preserve-data-view-buffer))
    (with-current-buffer data-buf
      (add-hook 'kill-buffer-hook
                #'mevedel-view--on-agent-transcript-data-killed nil t)))
  (unless (plist-get options :preserve-data-view-buffer)
    (with-current-buffer data-buf
      (setq-local mevedel--view-buffer view-buf)
      (use-local-map
       (copy-keymap (or (current-local-map) (make-sparse-keymap))))
      (local-set-key (kbd "C-c C-o") #'mevedel-menu)
      ;; Kill-buffer lifecycle: data killed -> kill view buffer
      (add-hook 'kill-buffer-hook #'mevedel-view--on-data-killed nil t))))

(defun mevedel-view--ensure (data-buf &optional view-name options)
  "Return the view buffer for DATA-BUF, creating it if needed.
VIEW-NAME and OPTIONS are forwarded to `mevedel-view--setup' when a
new view buffer is created."
  (or (let ((vb (buffer-local-value 'mevedel--view-buffer data-buf)))
        (and vb
             (buffer-live-p vb)
             (with-current-buffer vb
               (eq (and mevedel-view--agent-transcript-p t)
                   (and (plist-get options :agent-transcript-p) t)))
             vb))
      (let* ((data-name (buffer-name data-buf))
             ;; Derive view buffer name from data buffer name:
             ;; *mevedel:main@proj* -> *mevedel:main@proj:view*
             (derived-name (if (string-match "\\*$" data-name)
                               (replace-match ":view*" t t data-name)
                             (concat data-name ":view")))
             (view-buf (get-buffer-create (or view-name derived-name))))
        (mevedel-view--setup view-buf data-buf options)
        view-buf)))


;;
;;; Lifecycle

(defun mevedel-view--abort-data-buffer (data-buffer)
  "Abort active work owned by DATA-BUFFER."
  (when (and (buffer-live-p data-buffer)
             (buffer-local-value 'mevedel--session data-buffer)
             (fboundp 'mevedel-abort))
    (condition-case err
        (mevedel-abort data-buffer)
      (error
       (display-warning
        'mevedel
        (format "Could not abort session during buffer cleanup: %S" err)
        :warning)))))

(defun mevedel-view--on-view-killed ()
  "Hook run when the view buffer is killed.
Clears `mevedel--view-buffer' on the associated data buffer and kills
it.  The reference is cleared before killing so the data buffer's own
kill hook sees nil and exits without re-entering this function."
  (if (bound-and-true-p mevedel-view--agent-transcript-p)
      (let ((db mevedel--data-buffer)
            (view-buf (current-buffer))
            (live-p (plist-get mevedel-view--agent-transcript-info
                               :live-buffer)))
        (mevedel-view--clear-parent-transcript-window)
        (when (and db (buffer-live-p db))
          (with-current-buffer db
            (when (eq mevedel--view-buffer view-buf)
              (setq mevedel--view-buffer nil)))
          (unless (or live-p
                      mevedel-view--agent-transcript-data-kill-in-progress)
            (let ((mevedel-view--agent-transcript-data-kill-in-progress t))
              (kill-buffer db)))))
    (let ((view-buffer (current-buffer)))
      (mevedel-view--interaction-clear)
      (when-let* ((db mevedel--data-buffer)
                  (_ (buffer-live-p db)))
        (mevedel-view--abort-data-buffer db)
        (mevedel-view--kill-agent-transcript-views-for-parent view-buffer)
        (with-current-buffer db
          (when (fboundp 'mevedel-permission-queue-abort-all)
            (mevedel-permission-queue-abort-all mevedel--session))
          (when (fboundp 'mevedel-plan-queue-abort-all)
            (mevedel-plan-queue-abort-all mevedel--session))
          (setq mevedel--view-buffer nil))
        (kill-buffer db)))))

(defun mevedel-view--on-data-killed ()
  "Hook run when the data buffer is killed.
Kills the associated view buffer."
  (mevedel-view--abort-data-buffer (current-buffer))
  (when (fboundp 'mevedel-permission-queue-abort-all)
    (mevedel-permission-queue-abort-all mevedel--session))
  (when (fboundp 'mevedel-plan-queue-abort-all)
    (mevedel-plan-queue-abort-all mevedel--session))
  (when-let* ((vb mevedel--view-buffer)
              (_ (buffer-live-p vb)))
    (with-current-buffer vb
      (mevedel-view--kill-agent-transcript-views-for-parent vb)
      (mevedel-view--interaction-clear))
    (kill-buffer vb)))

(defun mevedel-view--status-strip-button (label area help)
  "Return clickable status strip LABEL for cockpit AREA with HELP."
  (let* ((map (make-sparse-keymap))
         (command (lambda (&optional _event)
                    (interactive "e")
                    (mevedel-menu-open area))))
    (define-key map [header-line mouse-1] command)
    (propertize label
                'face 'link
                'mouse-face 'highlight
                'help-echo help
                'local-map map
                'mevedel-view-cockpit-area area)))

(defun mevedel-view--status-strip-width ()
  "Return display columns available for the status strip."
  (let* ((buffer (current-buffer))
         (selected (selected-window))
         (windows (get-buffer-window-list buffer nil t))
         (width (cond
                 ((eq (window-buffer selected) buffer)
                  (window-body-width selected))
                 (windows
                  (apply #'min (mapcar #'window-body-width windows)))
                 (t
                  (window-body-width)))))
    (max 20 (1- width))))

(defun mevedel-view--status-strip-root-label (root max-width)
  "Return ROOT shortened to fit MAX-WIDTH display columns."
  (cond
   ((<= max-width 0) "")
   ((<= (string-width root) max-width) root)
   (t
    (let* ((base (file-name-nondirectory (directory-file-name root)))
           (tail (concat "…/" base "/")))
      (if (<= (string-width tail) max-width) tail "")))))

(defun mevedel-view--status-strip-spacer (rhs)
  "Return a spacer that right-aligns RHS in the header line."
  (propertize
   " " 'display
   (if (and (fboundp 'string-pixel-width)
            (display-graphic-p))
       `(space :align-to (- right (,(string-pixel-width rhs))))
     `(space :align-to (- right ,(string-width rhs))))))

(defun mevedel-view--status-strip ()
  "Return a mevedel-owned clickable status strip for the view buffer."
  (when (and (boundp 'mevedel--data-buffer)
             (buffer-live-p mevedel--data-buffer))
    (require 'mevedel-models)
    (require 'mevedel-tools)
    (let* ((data-buffer mevedel--data-buffer)
           (session (with-current-buffer data-buffer
                      (and (boundp 'mevedel--session) mevedel--session)))
           (workspace (and session (mevedel-session-workspace session)))
           (session-name (or (and session (mevedel-session-name session))
                             "unknown"))
           (root (abbreviate-file-name
                  (file-name-as-directory
                   (or (and workspace (mevedel-workspace-root workspace))
                       (with-current-buffer data-buffer default-directory)))))
           (mode (car (mevedel-view--permission-mode-display
                       (mevedel-view--effective-permission-mode))))
           (state (mevedel-request-state-label data-buffer))
           (model-label (mevedel-model-current-label data-buffer))
           (model (if (string= model-label "none")
                      "model none"
                    model-label))
           (tool-count (mevedel-tools-active-count data-buffer))
           (tools (format "%d tool%s"
                          tool-count
                          (if (= tool-count 1) "" "s")))
           (rhs
            (mapconcat
             #'identity
             (list
              (mevedel-view--status-strip-button
               mode 'mode "Open mode cockpit")
              (propertize state 'face (cond ((string= state "running") 'success)
                                            (t 'shadow)))
              (mevedel-view--status-strip-button
               model 'model "Open model cockpit")
              (mevedel-view--status-strip-button
               tools 'tools "Open tools cockpit"))
             " · "))
           (root-max
            (- (mevedel-view--status-strip-width)
               (string-width session-name)
               (string-width rhs)
               3))
           (root-label
            (mevedel-view--status-strip-root-label root root-max))
           (lhs
            (if (string-empty-p root-label)
                session-name
              (format "%s  %s" session-name root-label))))
      (concat
       (mevedel-view--status-strip-button
        lhs
        'top "Open session cockpit")
       (mevedel-view--status-strip-spacer rhs)
       rhs))))

(defun mevedel-view--agent-terminal-status-p (status)
  "Return non-nil when STATUS names a terminal agent transcript state."
  (memq status '(completed error aborted incomplete)))

(defun mevedel-view--agent-effective-status (inv entry)
  "Return the effective visible status from live INV and sidecar ENTRY.
Terminal live invocation state wins over stale sidecar metadata.  If
there is no terminal live state, terminal sidecar metadata wins over a
non-terminal live state so a finalized transcript does not regress to
running in the UI."
  (let ((inv-status (and inv
                         (mevedel-agent-invocation-transcript-status inv)))
        (entry-status (and entry (plist-get entry :status))))
    (cond
     ((mevedel-view--agent-terminal-status-p inv-status) inv-status)
     ((mevedel-view--agent-terminal-status-p entry-status) entry-status)
     (inv-status inv-status)
     (t entry-status))))

(defun mevedel-view--agent-transcript-header-line ()
  "Return the header-line string for a transcript inspection view."
  (let* ((info mevedel-view--agent-transcript-info)
         (agent-id (or mevedel-view--agent-id
                       (plist-get info :agent-id)))
         (display-label (mevedel-view--display-label-for-agent agent-id))
         (badge (mevedel-tool-ui--handle-badge
                 (list :status (plist-get info :status)
                       :calls (plist-get info :calls)
                       :elapsed (plist-get info :elapsed)
                       :reason (plist-get info :reason))))
         (calls (plist-get info :calls))
         (elapsed (plist-get info :elapsed))
         (session-label
          (or (plist-get info :session-label)
              (when-let* ((session (plist-get info :session)))
                (or (mevedel-session-session-id session)
                    (mevedel-session-name session)))
              (when-let* ((parent mevedel-view--agent-transcript-parent-view)
                          ((buffer-live-p parent))
                          (parent-data
                           (buffer-local-value 'mevedel--data-buffer parent))
                          ((buffer-live-p parent-data))
                          (session (buffer-local-value 'mevedel--session
                                                       parent-data)))
                (or (mevedel-session-session-id session)
                    (mevedel-session-name session)))
              "unknown")))
    (string-join
     (delq nil
           (list (format "Agent %s" display-label)
                 (unless (string-empty-p badge) badge)
                 (when (integerp calls) (format "%d calls" calls))
                 (when (numberp elapsed) (format "%.1fs" elapsed))
                 (format "session %s" session-label)))
     "  ")))

(defun mevedel-view--clear-parent-transcript-window ()
  "Clear this transcript view from its parent view's singleton slot."
  (when-let* ((parent mevedel-view--agent-transcript-parent-view)
              ((buffer-live-p parent)))
    (with-current-buffer parent
      (setq mevedel-view--agent-transcript-window nil))))

(defun mevedel-view--on-agent-transcript-view-killed ()
  "Hook run when an agent transcript inspection view is killed."
  (mevedel-view--clear-parent-transcript-window))

(defun mevedel-view--agent-transcript-view-p (buffer)
  "Return non-nil when BUFFER is an agent transcript inspection view."
  (and (buffer-live-p buffer)
       (with-current-buffer buffer
         (bound-and-true-p mevedel-view--agent-transcript-p))))

(defun mevedel-view--agent-transcript-views-for-parent (parent-view)
  "Return transcript inspection views opened from PARENT-VIEW."
  (let (views)
    (dolist (buf (buffer-list) (nreverse views))
      (when (and (mevedel-view--agent-transcript-view-p buf)
                 (with-current-buffer buf
                   (eq mevedel-view--agent-transcript-parent-view
                       parent-view)))
        (push buf views)))))

(defun mevedel-view--agent-transcript-views-for-data (data-buffer)
  "Return transcript inspection views rendering DATA-BUFFER."
  (let (views)
    (dolist (buf (buffer-list) (nreverse views))
      (when (and (mevedel-view--agent-transcript-view-p buf)
                 (with-current-buffer buf
                   (eq mevedel--data-buffer data-buffer)))
        (push buf views)))))

(defun mevedel-view--kill-agent-transcript-view (view-buffer)
  "Kill transcript inspection VIEW-BUFFER and its non-live data buffer."
  (when (mevedel-view--agent-transcript-view-p view-buffer)
    (let (data-buffer live-p)
      (with-current-buffer view-buffer
        (setq data-buffer mevedel--data-buffer
              live-p (plist-get mevedel-view--agent-transcript-info
                                :live-buffer)))
      (dolist (win (get-buffer-window-list view-buffer nil t))
        (ignore-errors
          (quit-window nil win)))
      (when (buffer-live-p view-buffer)
        (kill-buffer view-buffer))
      (when (and (not live-p)
                 data-buffer
                 (buffer-live-p data-buffer))
        (let ((mevedel-view--agent-transcript-data-kill-in-progress t))
          (kill-buffer data-buffer))))))

(defun mevedel-view--kill-agent-transcript-views-for-parent (parent-view)
  "Kill every transcript inspection view opened from PARENT-VIEW."
  (dolist (view (mevedel-view--agent-transcript-views-for-parent parent-view))
    (mevedel-view--kill-agent-transcript-view view)))

(defun mevedel-view--on-agent-transcript-data-killed ()
  "Kill live transcript inspection views when their data buffer dies."
  (unless mevedel-view--agent-transcript-data-kill-in-progress
    (let ((data-buffer (current-buffer))
          (mevedel-view--agent-transcript-data-kill-in-progress t))
      (dolist (view (mevedel-view--agent-transcript-views-for-data data-buffer))
        (when (buffer-live-p view)
          (kill-buffer view))))))

(defun mevedel-view--agent-live-transcript-views (agent-buf &optional agent-id)
  "Return live transcript views displaying AGENT-BUF.
When AGENT-ID is non-nil, only return views for that agent id."
  (let (views)
    (dolist (buf (buffer-list) (nreverse views))
      (when (and (buffer-live-p buf)
                 (with-current-buffer buf
                   (and (bound-and-true-p mevedel-view--agent-transcript-p)
                        (eq mevedel--data-buffer agent-buf)
                        (or (not agent-id)
                            (equal mevedel-view--agent-id agent-id)
                            (equal (mevedel-view--display-label-for-agent
                                    mevedel-view--agent-id)
                                   agent-id)))))
        (push buf views)))))

(defun mevedel-view-agent-live-transcript-finalize (invocation)
  "Refresh live transcript views for finalized INVOCATION.
Returns non-nil when a live transcript view still references the
agent buffer and should keep that buffer alive.  The view is marked
non-live so closing it after finalization kills the retained data
buffer."
  (when-let* (((mevedel-agent-invocation-p invocation))
              (agent-buf (mevedel-agent-invocation-buffer invocation))
              ((buffer-live-p agent-buf)))
    (let* ((agent-id (mevedel-agent-invocation-agent-id invocation))
           (views (mevedel-view--agent-live-transcript-views
                   agent-buf agent-id))
           (status (mevedel-agent-invocation-transcript-status invocation))
           (started (mevedel-agent-invocation-started-at invocation))
           (elapsed (and started
                         (float-time
                          (time-subtract (current-time) started))))
           (reason (mevedel-agent-invocation-terminal-reason invocation))
           (calls (mevedel-agent-invocation-call-count invocation)))
      (dolist (view views)
        (when (buffer-live-p view)
          (with-current-buffer view
            (setq mevedel-view--agent-transcript-info
                  (plist-put (copy-sequence
                              mevedel-view--agent-transcript-info)
                             :live-buffer nil))
            (setq mevedel-view--agent-transcript-info
                  (plist-put mevedel-view--agent-transcript-info
                             :status status))
            (setq mevedel-view--agent-transcript-info
                  (plist-put mevedel-view--agent-transcript-info
                             :calls calls))
            (when elapsed
              (setq mevedel-view--agent-transcript-info
                    (plist-put mevedel-view--agent-transcript-info
                               :elapsed elapsed)))
            (when reason
              (setq mevedel-view--agent-transcript-info
                    (plist-put mevedel-view--agent-transcript-info
                               :reason reason)))
            (when (buffer-live-p mevedel--data-buffer)
              (mevedel-view--full-rerender)))))
      views)))

(defun mevedel-view-close-agent-transcript ()
  "Close the selected transcript side window.
Saved transcript file buffers are killed with the view.  Live running
agent buffers are left alone."
  (interactive)
  (unless mevedel-view--agent-transcript-p
    (user-error "Not an agent transcript view"))
  (mevedel-view--kill-agent-transcript-view (current-buffer)))


;;
;;; Tool status

(defun mevedel-view--tool-status-string (tool-name args)
  "Build a short status string for TOOL-NAME with ARGS."
  (let ((primary-arg (mevedel-tool-display-string tool-name args)))
    (if primary-arg
        (format "Calling %s: %s..." tool-name primary-arg)
      (format "Calling %s..." tool-name))))


;;
;;; Rerender coordination


(defvar-local mevedel-view--rerender-timer nil
  "Buffer-local debounce timer for `mevedel-view-rerender'.
A pending timer collapses bursts of rerender requests (e.g. from
a sub-agent making 30 tool calls in quick succession) into a
single full-rerender after a short quiescence window.")

(defcustom mevedel-view-rerender-debounce 0.15
  "Seconds to wait after the last `mevedel-view-rerender' call before re-rendering.
Bursts of requests inside this window collapse into one
re-render; useful for smoothing out background-handle patch
storms during multi-tool sub-agent dispatches."
  :type 'number
  :group 'mevedel)

(defcustom mevedel-view-agent-refresh-delay 0.05
  "Seconds to coalesce live agent handle refreshes.
Agent tool start/finish hooks can arrive in bursts.  This delay keeps those
bursts from repainting the same handle repeatedly while still updating live
badges promptly."
  :type 'number
  :group 'mevedel)

(defun mevedel-view-rerender (&optional buffer)
  "Schedule a debounced re-render of BUFFER.
Default to the current buffer.  Public re-render entry point used by
the background handle patch path, plan-summary disk-write reconstruction,
and any caller that
mutates render-data and wants the visible card refreshed without
waiting for the next stream tick.

Bursts collapse into one rerender via the option
`mevedel-view-rerender-debounce'.  When the view is mid-stream (a parent
FSM is streaming), the debounce window also lets the incremental render
path pick up the latest render-data on its own tick before the
full-rerender fires.

Currently the actual re-render delegates to
`mevedel-view--full-rerender' as the guaranteed-correct path; a
follow-up may swap to the cheaper incremental path when an
in-flight turn boundary is established."
  (let ((view-buf (or buffer (current-buffer))))
    (when (buffer-live-p view-buf)
      (with-current-buffer view-buf
        (when (and (boundp 'mevedel--data-buffer) mevedel--data-buffer)
          (when (timerp mevedel-view--rerender-timer)
            (cancel-timer mevedel-view--rerender-timer))
          (setq mevedel-view--rerender-timer
                (run-with-idle-timer
                 mevedel-view-rerender-debounce nil
                 (lambda ()
                   (when (buffer-live-p view-buf)
                     (with-current-buffer view-buf
                       (setq mevedel-view--rerender-timer nil)
                       (condition-case _
                           (mevedel-view--full-rerender)
                         (error nil))))))))))))


;;
;;; Sub-agent transcript open command

(defun mevedel-view--event-position (&optional event)
  "Return buffer position referenced by mouse EVENT, or nil."
  (and event
       (eventp event)
       (let ((pos (posn-point (event-end event))))
         (and (integer-or-marker-p pos) pos))))

(defun mevedel-view-activate-at-point (&optional event)
  "Activate actionable display or fragment text at point or EVENT.
This command is installed only on display text keymaps; direct calls from
the editable composer signal instead of settling queued interactions."
  (interactive (list last-nonmenu-event))
  (let* ((event-pos (mevedel-view--event-position event))
         (pos (or event-pos (point)))
         (activate (get-text-property pos 'mevedel-view-zone-activate)))
    (when event-pos
      (mouse-set-point event))
    (cond
     ((mevedel-view--position-in-input-region-p pos)
      (user-error "No actionable fragment at point"))
     ((get-text-property pos 'mevedel-view-agent-id)
      (mevedel-view-open-agent-transcript-at-point event))
     ((and activate
           (not (get-text-property pos 'mevedel-view-interaction-overlay)))
      (funcall activate))
     ((get-text-property pos 'mevedel-tool-task)
      (mevedel-toggle-tasks))
     ((and event (eventp event))
      nil)
     (t
      (user-error "No actionable fragment at point")))))

(defun mevedel-view-open-agent-transcript-at-point (&optional event)
  "Open the transcript referenced by the attribution at point or EVENT.

When EVENT is a mouse event outside an attribution, just move point as
usual.  Keyboard invocation outside an attribution signals a user error."
  (interactive (list last-nonmenu-event))
  (let* ((event-pos (and event
                         (eventp event)
                         (posn-point (event-end event))))
         (pos (if (integer-or-marker-p event-pos) event-pos (point)))
         (agent-id (get-text-property pos 'mevedel-view-agent-id)))
    (cond
     (agent-id
      (when (and event (eventp event))
        (mouse-set-point event))
      (if (get-text-property pos 'mevedel-view-agent-handle-p)
          (mevedel-view-agent-handle-activate agent-id)
        (mevedel-view--open-agent-transcript-or-message
         agent-id
         (get-text-property pos 'mevedel-view-agent-live-click)
         (get-text-property pos 'mevedel-view-agent-calls))))
     ((and event (eventp event))
      (mouse-set-point event))
     (t
      (user-error "No transcript at point")))))

(defun mevedel-view--lookup-transcript-pair (agent-id)
  "Return the parent session's transcript pair for AGENT-ID.

Resolve the parent chat (data) buffer from the current view
buffer, read its `mevedel--session', and look up AGENT-ID in the
session's `agent-transcripts' alist.

AGENT-ID may be the canonical id (for example, `type--32hex') or the
display label (`type--8hex') shown in rendered view text.  Returns nil
if any link is missing."
  (when-let* ((data-buf (and (boundp 'mevedel--data-buffer)
                             mevedel--data-buffer))
              ((buffer-live-p data-buf))
              (session (buffer-local-value 'mevedel--session data-buf))
              (entries (mevedel-session-agent-transcripts session)))
    (or (assoc agent-id entries)
        (cl-find-if
         (lambda (entry)
           (equal (mevedel-view--display-label-for-agent (car entry))
                  agent-id))
         entries))))

(defun mevedel-view--lookup-transcript-entry (agent-id)
  "Return the parent session's transcript entry plist for AGENT-ID.
See `mevedel-view--lookup-transcript-pair' for accepted id forms."
  (cdr (mevedel-view--lookup-transcript-pair agent-id)))

(defun mevedel-view--display-label-for-agent (agent-id)
  "Return the short display label for AGENT-ID."
  (or (and (fboundp 'mevedel-agent-runtime-display-label)
           (mevedel-agent-runtime-display-label agent-id))
      agent-id))

(defun mevedel-view--resolve-agent-transcript (agent-id)
  "Return transcript info for AGENT-ID.
Terminal agents resolve through their saved transcript file.  Running
agents resolve through their live invocation buffer when available.
Signals `user-error' when no transcript source can be opened."
  (let* ((data-buf (and (boundp 'mevedel--data-buffer)
                        mevedel--data-buffer))
         (session (and data-buf (buffer-live-p data-buf)
                       (buffer-local-value 'mevedel--session data-buf)))
         (pair (and session (mevedel-view--lookup-transcript-pair agent-id)))
         (canonical-id (or (car pair) agent-id))
         (entry (cdr pair))
         (inv (mevedel-view--agent-invocation agent-id))
         (status (mevedel-view--agent-effective-status inv entry))
         (live-buffer (and inv
                           (not (mevedel-view--agent-terminal-status-p status))
                           (mevedel-agent-invocation-buffer inv)))
         (save-path (and session (mevedel-session-save-path session)))
         (rel-path (and entry (plist-get entry :path))))
    (unless (or entry live-buffer)
      (user-error "No transcript entry for agent-id: %s" agent-id))
    (if (and live-buffer (buffer-live-p live-buffer))
        (append (list :agent-id canonical-id
                      :status status
                      :entry entry
                      :session session
                      :buffer live-buffer
                      :live-buffer t
                      :calls (or (and inv
                                      (mevedel-agent-invocation-call-count inv))
                                 (plist-get entry :calls))
                      :elapsed (mevedel-view--agent-row-elapsed inv entry)
                      :reason (or (and inv
                                       (mevedel-agent-invocation-terminal-reason
                                        inv))
                                  (plist-get entry :reason))
                      :session-label (or (and session
                                              (mevedel-session-session-id
                                               session))
                                         (and session
                                              (mevedel-session-name session))
                                         "unknown"))
                entry)
      (unless save-path
        (user-error "Parent session has no save-path"))
      (unless (mevedel-session-persistence--validate-transcript-path
               rel-path save-path)
        (user-error "Transcript path failed validation: %s" rel-path))
      (let ((abs (expand-file-name rel-path save-path)))
        (unless (file-exists-p abs)
          (user-error "Transcript file missing: %s" abs))
        (append (list :agent-id canonical-id
                      :status status
                      :entry entry
                      :session session
                      :save-path save-path
                      :relative-path rel-path
                      :absolute-path abs
                      :session-label (or (mevedel-session-session-id session)
                                         (mevedel-session-name session)
                                         "unknown"))
                entry)))))

(defun mevedel-view--display-agent-transcript-view (view-buf)
  "Display transcript inspection VIEW-BUF in this view's singleton slot."
  (let ((parent-view (or (and (boundp 'mevedel-view--agent-transcript-parent-view)
                              mevedel-view--agent-transcript-parent-view)
                         (current-buffer))))
    (when (and (buffer-live-p parent-view)
               (not (eq parent-view view-buf)))
      (with-current-buffer parent-view
        (let ((win mevedel-view--agent-transcript-window))
          (setq win
                (cond
                 ((and (window-live-p win)
                       (not (window-dedicated-p win))
                       (buffer-live-p (window-buffer win))
                       (with-current-buffer (window-buffer win)
                         mevedel-view--agent-transcript-p))
                  (condition-case nil
                      (let ((old-view (window-buffer win)))
                        (set-window-buffer win view-buf)
                        (unless (eq old-view view-buf)
                          (mevedel-view--kill-agent-transcript-view
                           old-view))
                        win)
                    (error nil)))
                 ((window-live-p win)
                  (ignore-errors (quit-window nil win))
                  nil)
                 (t nil)))
          (unless (window-live-p win)
            (setq win
                  (condition-case nil
                      (display-buffer view-buf mevedel-agent-view-display-action)
                    (error
                     (pop-to-buffer view-buf mevedel-agent-view-display-action)
                     (selected-window)))))
          (setq mevedel-view--agent-transcript-window
                (and (window-live-p win) win))
          (when (window-live-p win)
            (select-window win)
            (with-current-buffer view-buf
              (goto-char (point-max))
              (set-window-point win (point)))))))))

(defun mevedel-view--ensure-agent-transcript-view (agent-id info parent-view)
  "Return a rendered transcript inspection view for AGENT-ID and INFO.
PARENT-VIEW is the session view that opened the transcript."
  (let* ((live-p (plist-get info :live-buffer))
         (agent-data (or (plist-get info :buffer)
                         (mevedel-session-persistence--find-file-noselect
                          (plist-get info :absolute-path))))
         (display-label (mevedel-view--display-label-for-agent agent-id))
         (view-name (format "*mevedel-agent:%s*" display-label))
         (agent-view
          (mevedel-view--ensure
           agent-data view-name
           (list :agent-transcript-p t
                 :agent-id agent-id
                 :parent-view parent-view
                 :preserve-data-view-buffer live-p
                 :transcript-info info))))
    (with-current-buffer agent-data
      (when (eq major-mode 'so-long-mode)
        (org-mode))
      (unless (derived-mode-p 'org-mode)
        (org-mode))
      ;; Saved transcripts rely on gptel-org's GPTEL_BOUNDS property
      ;; for the `gptel' text properties that distinguish user text,
      ;; responses, reasoning, and tool calls.  Restore just those
      ;; bounds; full `gptel-org--restore-state' also restores backend
      ;; and tool objects, which is unnecessary and noisy for a
      ;; read-only transcript view.
      (unless live-p
        (require 'mevedel-transcript-restore)
        (mevedel-transcript-restore-properties))
      (unless (or live-p buffer-read-only)
        (read-only-mode +1)))
    (with-current-buffer agent-view
      (mevedel-view--full-rerender))
    agent-view))

(defun mevedel-view-agent-handle-activate (&optional agent-id)
  "Open the rendered agent handle at point or AGENT-ID."
  (interactive)
  (let ((id (or agent-id
                (get-text-property (point) 'mevedel-view-agent-id))))
    (unless id
      (user-error "No agent handle at point"))
    (condition-case err
        (mevedel-view-open-agent-transcript id)
      (user-error
       (message "%s" (error-message-string err))))))

(defun mevedel-view--open-agent-transcript-or-message
    (agent-id &optional _live-click-p calls)
  "Open AGENT-ID's transcript or explain why it is not openable.
CALLS is the optional number of tool calls to mention in fallback messages.

This is the click/RET path for attribution fragments."
  (let* ((entry (mevedel-view--lookup-transcript-entry agent-id))
         (inv (mevedel-view--agent-invocation agent-id))
         (status (mevedel-view--agent-effective-status inv entry))
         (session (and (boundp 'mevedel--data-buffer)
                       mevedel--data-buffer
                       (buffer-live-p mevedel--data-buffer)
                       (buffer-local-value 'mevedel--session
                                           mevedel--data-buffer)))
         (save-path (and session (mevedel-session-save-path session)))
         (rel-path (and entry (plist-get entry :path)))
         (path-ok (and entry save-path
                       (mevedel-session-persistence--validate-transcript-path
                        rel-path save-path)))
         (terminal-p (memq status '(completed error aborted incomplete)))
         (display-label (mevedel-view--display-label-for-agent agent-id))
         (count (or calls (and entry (plist-get entry :calls)))))
    (cond
     ((and path-ok terminal-p)
      (mevedel-view-open-agent-transcript agent-id))
     ((and (eq status 'running) inv)
      (mevedel-view-open-agent-transcript agent-id))
     ((eq status 'running)
      (message "Agent %s still running%s. Live buffer unavailable."
               display-label
               (if (integerp count)
                   (format " (%d tool calls)" count)
                 "")))
     ((not entry)
      (message "No transcript recorded for %s" display-label))
     ((not path-ok)
      (message "Transcript path is unavailable for %s" display-label))
     (t
      (message "Transcript unavailable for %s" display-label)))))

(defun mevedel-view-open-agent-transcript (agent-id)
  "Open AGENT-ID's transcript as a rendered read-only inspection view.

Looks up the entry in the parent session's `agent-transcripts'
slot and validates the path through
`mevedel-session-persistence--validate-transcript-path' before opening
completed transcripts.  Running agents open from their live invocation
buffer."
  (interactive
   (list (completing-read
          "Agent transcript: "
          (when-let* ((data-buf (and (boundp 'mevedel--data-buffer)
                                     mevedel--data-buffer))
                      ((buffer-live-p data-buf))
                      (session (buffer-local-value
                                'mevedel--session data-buf)))
            (mapcar #'car (mevedel-session-agent-transcripts session)))
          nil t)))
  (let* ((parent-view (current-buffer))
         (info (mevedel-view--resolve-agent-transcript agent-id))
         (canonical-id (plist-get info :agent-id))
         (agent-view (mevedel-view--ensure-agent-transcript-view
                      canonical-id info parent-view)))
    (mevedel-view--display-agent-transcript-view agent-view)))

(defun mevedel-view--agent-handle-ids-in-buffer ()
  "Return agent ids whose source-backed handles are present in the view."
  (let (ids)
    (save-excursion
      (goto-char (point-min))
      (while (< (point) (point-max))
        (let ((id (and (not (mevedel-view--agent-status-region-position-p
                             (point)))
                       (get-text-property (point) 'mevedel-view-agent-handle-p)
                       (get-text-property (point) 'mevedel-view-agent-id))))
          (when (and id (not (member id ids)))
            (push id ids)))
        (goto-char (or (next-single-property-change
                        (point) 'mevedel-view-agent-id nil (point-max))
                       (point-max)))))
    (nreverse ids)))

(defun mevedel-view--agent-row-description (_agent-id inv entry)
  "Return the agent row description from INV or transcript ENTRY."
  (or (and inv (mevedel-agent-invocation-description inv))
      (plist-get entry :description)
      ""))

(defun mevedel-view--agent-row-type (agent-id inv entry)
  "Return the agent type for AGENT-ID from INV, ENTRY, or the id prefix."
  (or (and inv
           (mevedel-agent-invocation-agent inv)
           (mevedel-agent-name (mevedel-agent-invocation-agent inv)))
      (plist-get entry :type)
      (plist-get entry :agent-type)
      (when (stringp agent-id)
        (if-let* ((sep (string-search "--" agent-id)))
            (substring agent-id 0 sep)
          agent-id))
      "?"))

(defun mevedel-view--agent-row-elapsed (inv entry)
  "Return elapsed seconds for INV or transcript ENTRY."
  (or (plist-get entry :elapsed)
      (and inv
           (mevedel-agent-invocation-started-at inv)
           (float-time
            (time-subtract (current-time)
                           (mevedel-agent-invocation-started-at inv))))))

(defun mevedel-view--agent-row-verdict (inv entry)
  "Return parsed verifier verdict for INV or transcript ENTRY."
  (or (and inv (mevedel-agent-invocation-verdict inv))
      (plist-get entry :verdict)))

(defun mevedel-view--agent-row-parent-id (inv entry)
  "Return parent agent id for INV or transcript ENTRY, when known."
  (or (plist-get entry :parent-agent-id)
      (when-let* (((mevedel-agent-invocation-p inv))
                  (parent (mevedel-agent-invocation-parent-context inv))
                  ((mevedel-agent-invocation-p parent)))
        (mevedel-agent-invocation-agent-id parent))))

(defun mevedel-view--agent-entry-has-visible-child-p (agent-id entries)
  "Return non-nil if ENTRIES include an active child of AGENT-ID."
  (catch 'found
    (dolist (pair entries nil)
      (let* ((entry (cdr pair))
             (parent-id (plist-get entry :parent-agent-id))
             (status (plist-get entry :status)))
        (when (and (equal parent-id agent-id)
                   (or (eq status 'running)
                       (mevedel-view--agent-terminal-status-p status)))
          (throw 'found t))))))

(defun mevedel-view--agent-status-counts ()
  "Return a plist of active agent counts for the current view."
  (let* ((data-buf (and (boundp 'mevedel--data-buffer) mevedel--data-buffer))
         (session (and data-buf (buffer-live-p data-buf)
                       (buffer-local-value 'mevedel--session data-buf)))
         (entries (and session (mevedel-session-agent-transcripts session)))
         (seen (make-hash-table :test #'equal))
         (blocked 0)
         (running 0))
    (cl-labels
        ((record (agent-id status)
           (when (and agent-id status)
             (puthash agent-id status seen))))
      (when entries
        (dolist (pair entries)
          (let* ((agent-id (car pair))
                 (entry (cdr pair))
                 (inv (mevedel-view--agent-invocation agent-id))
                 (status (mevedel-view--agent-effective-status inv entry)))
            (when (eq status 'running)
              (record agent-id
                      (if (mevedel-view--agent-status-blocked-p agent-id)
                          'blocked
                        'running))))))
      (when (and data-buf (buffer-live-p data-buf))
        (with-current-buffer data-buf
          (dolist (pair mevedel-agent-runtime--fsms)
            (let* ((agent-id (car pair))
                   (inv (ignore-errors
                         (mevedel-agent-runtime--agent-invocation-at (cdr pair))))
                   (entry (cdr (assoc agent-id entries)))
                   (status (mevedel-view--agent-effective-status inv entry)))
              (when (eq status 'running)
                (record agent-id
                        (if (mevedel-view--agent-status-blocked-p agent-id)
                            'blocked
                          'running)))))))
      (maphash (lambda (_agent-id status)
                 (pcase status
                   ('blocked (cl-incf blocked))
                   ('running (cl-incf running))))
               seen)
      (list :blocked blocked :running running))))

(defun mevedel-view--agent-status-collect ()
  "Collect aggregate agent status rows for agents without visible handles."
  (let* ((data-buf (and (boundp 'mevedel--data-buffer) mevedel--data-buffer))
         (session (and data-buf (buffer-live-p data-buf)
                       (buffer-local-value 'mevedel--session data-buf)))
         (entries (and session (mevedel-session-agent-transcripts session)))
         (handle-ids (mevedel-view--agent-handle-ids-in-buffer))
         ;; Inline handles are the primary UI for agent status and
         ;; transcript opening.  The aggregate footer only covers
         ;; agents that have no visible handle in the rendered turn.
         (live-ids (copy-sequence handle-ids))
         rows)
    (when (and data-buf (buffer-live-p data-buf))
      (with-current-buffer data-buf
        ;; Preserve the ids of terminal live invocations before pruning
        ;; removes their FSMs, so stale running sidecar metadata cannot
        ;; reappear as aggregate status rows.
        (dolist (pair mevedel-agent-runtime--fsms)
          (let* ((agent-id (car pair))
                 (inv (ignore-errors
                         (mevedel-agent-runtime--agent-invocation-at (cdr pair))))
                 (status (and inv
                              (mevedel-agent-invocation-transcript-status
                               inv))))
            (when (mevedel-view--agent-terminal-status-p status)
              (push agent-id live-ids))))
        (when (fboundp 'mevedel-agent-runtime--prune-stale-agents-fsm)
          (mevedel-agent-runtime--prune-stale-agents-fsm))
        (dolist (pair mevedel-agent-runtime--fsms)
          (let* ((agent-id (car pair))
                 (fsm (cdr pair))
                 (inv (ignore-errors
                        (mevedel-agent-runtime--agent-invocation-at fsm)))
                 (entry (cdr (assoc agent-id entries)))
                 (status (mevedel-view--agent-effective-status inv entry)))
            (when inv
              (push agent-id live-ids)
              (unless (member agent-id handle-ids)
                (when (eq status 'running)
                  (let ((blocked
                         (and session
                              (or (mevedel-view--queue-has-origin-p
                                   (mevedel-session-permission-queue session)
                                   agent-id)
                                  (mevedel-view--queue-has-origin-p
                                   (mevedel-session-plan-queue session)
                                   agent-id))))
                        (parent-id
                         (mevedel-view--agent-row-parent-id inv entry)))
                    (push (list :agent-id agent-id
                                :status (if blocked 'blocked 'running)
                                :agent-type
                                (mevedel-view--agent-row-type
                                 agent-id inv entry)
                                :description
                                (mevedel-view--agent-row-description agent-id inv entry)
                                :calls (mevedel-agent-invocation-call-count inv)
                                :elapsed (mevedel-view--agent-row-elapsed inv entry)
                                :verdict
                                (mevedel-view--agent-row-verdict inv entry)
                                :parent-agent-id parent-id
                                :depth (if parent-id 1 0)
                                :parent-turn (plist-get entry :parent-turn)
                                :reason
                                (or (mevedel-agent-invocation-terminal-reason inv)
                                    (plist-get entry :reason)))
                          rows)))))))))
    (dolist (pair entries)
      (let* ((agent-id (car pair))
             (entry (cdr pair))
             (inv (mevedel-view--agent-invocation agent-id))
             (status (mevedel-view--agent-effective-status inv entry))
             (blocked (and (eq status 'running)
                           (mevedel-view--agent-status-blocked-p agent-id)))
             (visible-status (if blocked 'blocked status))
             (parent-id (mevedel-view--agent-row-parent-id inv entry)))
        (when (and (or (not (member agent-id live-ids))
                       (mevedel-view--agent-entry-has-visible-child-p
                        agent-id entries))
                   (eq status 'running))
          (push agent-id live-ids)
          (push (list :agent-id agent-id
                      :status visible-status
                      :agent-type
                      (mevedel-view--agent-row-type agent-id inv entry)
                      :description
                      (mevedel-view--agent-row-description agent-id inv entry)
                      :calls (or (and inv
                                      (mevedel-agent-invocation-call-count inv))
                                 (plist-get entry :calls))
                      :elapsed (mevedel-view--agent-row-elapsed inv entry)
                      :verdict (mevedel-view--agent-row-verdict inv entry)
                      :parent-agent-id parent-id
                      :depth (if parent-id 1 0)
                      :parent-turn (plist-get entry :parent-turn)
                      :reason (plist-get entry :reason))
                rows))))
    (mevedel-view--agent-status-order-rows rows)))

(defun mevedel-view--agent-status-rank (row)
  "Return status sort rank for ROW."
  (or (cl-position (plist-get row :status)
                   '(blocked running error aborted incomplete completed))
      99))

(defun mevedel-view--agent-status-infer-parent (row rows)
  "Return inferred parent id for ROW from ROWS, or nil.

Parentage normally comes from recorded transcript metadata or the live
invocation parent context.  If that metadata is missing, live parent
invocations can still identify their running background children."
  (or (plist-get row :parent-agent-id)
      (let ((agent-id (plist-get row :agent-id)))
        (catch 'parent
          (dolist (candidate rows)
            (let ((candidate-id (plist-get candidate :agent-id)))
              (when (and candidate-id
                         (not (equal candidate-id agent-id)))
                (when-let* ((inv (mevedel-view--agent-invocation
                                  candidate-id))
                            ((mevedel-agent-invocation-p inv))
                            (children
                             (mevedel-agent-invocation-background-agents
                              inv))
                            ((member agent-id children)))
                  (throw 'parent candidate-id)))))
          nil))))

(defun mevedel-view--agent-status-order-rows (rows)
  "Return ROWS ordered as a parent-before-child hierarchy."
  (let ((index 0)
        (table (make-hash-table :test #'equal))
        (children (make-hash-table :test #'equal))
        ordered)
    (dolist (row rows)
      (unless (plist-member row :index)
        (setq row (plist-put row :index index))
        (cl-incf index))
      (puthash (plist-get row :agent-id) row table))
    (dolist (row rows)
      (when-let* ((parent-id (mevedel-view--agent-status-infer-parent
                              row rows))
                  ((gethash parent-id table)))
        (setq row (plist-put row :parent-agent-id parent-id))
        (puthash (plist-get row :agent-id) row table)))
    (maphash
     (lambda (_id row)
       (let ((parent-id (plist-get row :parent-agent-id)))
         (if (and parent-id (gethash parent-id table))
             (puthash parent-id
                      (cons row (gethash parent-id children))
                      children)
           (push row ordered))))
     table)
    (cl-labels
        ((sibling-less-p
          (a b)
          (let ((rank-a (mevedel-view--agent-status-rank a))
                (rank-b (mevedel-view--agent-status-rank b)))
            (if (= rank-a rank-b)
                (< (or (plist-get a :index) 0)
                   (or (plist-get b :index) 0))
              (< rank-a rank-b))))
         (depth-of
          (row)
          (if-let* ((parent-id (plist-get row :parent-agent-id))
                    (parent (gethash parent-id table)))
              (1+ (depth-of parent))
            0))
         (emit
          (row)
          (let ((row (plist-put (copy-sequence row) :depth (depth-of row))))
            (cons row
                  (mapcan #'emit
                          (sort (copy-sequence
                                 (gethash (plist-get row :agent-id)
                                          children))
                                #'sibling-less-p))))))
      (mapcan #'emit (sort ordered #'sibling-less-p)))))

(defun mevedel-view--agent-status-summary (rows)
  "Return a compact summary label for aggregate ROWS."
  (let ((blocked 0)
        (running 0)
        (terminal 0))
    (dolist (row rows)
      (pcase (plist-get row :status)
        ('blocked (cl-incf blocked))
        ('running (cl-incf running))
        (_ (cl-incf terminal))))
    (string-join
     (delq nil
           (list (when (> blocked 0) (format "%d blocked" blocked))
                 (when (> running 0) (format "%d running" running))
                 (when (> terminal 0) (format "%d finished" terminal))))
     " · ")))

(defun mevedel-view--agent-status-buttonize-toggle (header suffix)
  "Return HEADER with SUFFIX made into the aggregate-status toggle.
Only the visible `[+]' / `[-]' suffix is made clickable so the
status line behaves like other compact view buffer affordances."
  (let ((start (string-match (regexp-quote suffix) header))
        (map (make-sparse-keymap)))
    (when start
      (define-key map (kbd "RET") #'mevedel-view-activate-at-point)
      (define-key map [mouse-1] #'mevedel-view-activate-at-point)
      (define-key map [mouse-2] #'mevedel-view-activate-at-point)
      (add-text-properties
       start (+ start (length suffix))
       `(face link
         keymap ,map
         mouse-face highlight
         follow-link t
         help-echo "Expand or collapse agent status")
       header))
    header))

(defun mevedel-view--agent-status-string (rows expanded-p)
  "Return the collapsed aggregate status header for ROWS.
EXPANDED-P controls whether the toggle suffix displays an expanded or
collapsed marker; expanded row content is rendered from Agent handles."
  (let* ((summary (mevedel-view--agent-status-summary rows))
         (suffix (if expanded-p "[-]" "[+]"))
         (header (mevedel-view--zone-separator
                  (format "%d %s: %s%s"
                          (length rows)
                          (if (= 1 (length rows)) "agent" "agents")
                          summary
                          (concat " " suffix)))))
    (setq header (mevedel-view--agent-status-buttonize-toggle
                  header suffix))
    (concat header "\n")))

(defun mevedel-view--agent-status-row-rendering (row &optional header-width)
  "Return an Agent-handle rendering plist for aggregate status ROW.
HEADER-WIDTH is the optional width used to align the row header."
  (let* ((agent-id (plist-get row :agent-id))
         (status (plist-get row :status))
         (render-status (if (eq status 'blocked) 'running status))
         (agent-type (or (plist-get row :agent-type)
                         (mevedel-view--agent-row-type agent-id nil nil)))
         (description (or (plist-get row :description) ""))
         (calls (plist-get row :calls))
         (elapsed (plist-get row :elapsed))
         (verdict (plist-get row :verdict))
         (reason (plist-get row :reason))
         (blocked-reason (and (eq status 'blocked) "interaction"))
         (render-data (append
                       (list :kind 'agent-transcript
                             :agent-id agent-id
                             :status render-status
                             :calls (or calls 0)
                             :background t
                             :omit-attribution t)
                       (when header-width
                         (list :header-width header-width))
                       (when elapsed (list :elapsed elapsed))
                       (when verdict (list :verdict verdict))
                       (when blocked-reason
                         (list :blocked-reason blocked-reason))
                       (when reason (list :reason reason))))
         (body (if (eq render-status 'running)
                   "Agent is running."
                 "Agent completed.")))
    (mevedel-tool-ui--render-agent
     "Agent"
     (list :subagent_type agent-type
           :description description)
     body
     render-data)))

(defun mevedel-view--agent-status-line-width ()
  "Return the maximum display width for one aggregate agent status row."
  (let* ((buffer (current-buffer))
         (selected (selected-window))
         (windows (get-buffer-window-list buffer nil t))
         (width (cond
                 ((eq (window-buffer selected) buffer)
                  (window-body-width selected))
                 (windows
                  (apply #'min (mapcar #'window-body-width windows)))
                 (t
                  (window-body-width)))))
    (max 20 (1- width))))

(defun mevedel-view--agent-status-handles-string (rows)
  "Return Agent-handle text for ROWS, preserving live expansion state."
  (let ((data-buffer
         (and (boundp 'mevedel--data-buffer) mevedel--data-buffer))
        (session
         (and (boundp 'mevedel--session) mevedel--session))
        (line-width (mevedel-view--agent-status-line-width)))
    (with-temp-buffer
      (let ((mevedel--data-buffer data-buffer)
            (mevedel--session session)
            (mevedel-view--input-marker (copy-marker (point-max) t))
            (mevedel-view--status-marker (copy-marker (point-max) t))
            (mevedel-view--interaction-marker (copy-marker (point-max) t)))
        (dolist (row rows)
          (let* ((depth (or (plist-get row :depth) 0))
                 (header-width (max 20 (- line-width
                                          4
                                          (* 2 depth)))))
            (when-let* ((rendering
                         (mevedel-view--agent-status-row-rendering
                          row header-width)))
              (let ((start (point)))
                (mevedel-view--insert-rendered-tool rendering nil)
                (when (> depth 0)
                  (mevedel-view--indent-region-lines
                   start (point)
                   (propertize
                    (make-string (* 2 depth) ?\s)
                    'font-lock-face 'mevedel-view-tool-metadata)))))))
        (let ((inhibit-read-only t))
          (remove-text-properties
           (point-min) (point-max)
           '(mevedel-view-source nil mevedel-view-source-key nil))
          (mevedel-view--add-display-region-properties
           (point-min) (point-max) 'agent-handle)
          (remove-text-properties
           (point-min) (point-max)
           '(read-only nil front-sticky nil rear-nonsticky nil)))
        (buffer-string)))))

(defun mevedel-view--indent-region-lines (start end prefix)
  "Insert PREFIX before every non-empty line between START and END."
  (save-excursion
    (goto-char start)
    (let ((end-marker (copy-marker end t)))
      (unwind-protect
          (while (< (point) end-marker)
            (unless (looking-at-p "[ \t]*$")
              (insert prefix))
            (forward-line 1))
        (set-marker end-marker nil)))))

(defun mevedel-view--current-buffer-marker-position (marker)
  "Return MARKER's position when it belongs to the current buffer."
  (and (markerp marker)
       (eq (marker-buffer marker) (current-buffer))
       (marker-position marker)))

(defun mevedel-view--status-anchor ()
  "Return the recovered start of fragment-managed status text."
  (let* ((input-pos (mevedel-view--input-marker-position))
         (status-pos (mevedel-view--current-buffer-marker-position
                      mevedel-view--status-marker))
         (history-tail (mevedel-view--history-tail-position))
         (status-valid-p
          (and status-pos
               (or (not input-pos) (<= status-pos input-pos))
               (>= status-pos history-tail)
               (not (mevedel-view--non-history-view-position-p
                     status-pos))
               (not (and (> status-pos (mevedel-view--after-header-position))
                         (mevedel-view--non-history-view-position-p
                          (1- status-pos)))))))
    (or (and status-valid-p status-pos)
        history-tail)))

(defun mevedel-view--status-trailing-newline-suffix (body)
  "Return the suffix needed to preserve BODY's trailing newlines."
  (let ((pos (length body))
        (count 0))
    (while (and (> pos 0) (eq (aref body (1- pos)) ?\n))
      (setq pos (1- pos)
            count (1+ count)))
    (when (> count 1)
      (make-string (1- count) ?\n))))

(defun mevedel-view--status-task-show-completed-p ()
  "Return non-nil when task status should show completed rows."
  (require 'mevedel-view-zone)
  (not (mevedel-view-zone-collapse-state
        mevedel-view--status-task-collapse-key t)))

(defun mevedel-view--status-agent-expanded-p ()
  "Return non-nil when aggregate agent status should show handle rows."
  (require 'mevedel-view-zone)
  (if (mevedel-view-zone-collapse-state-set-p
       mevedel-view--status-agent-collapse-key)
      (not (mevedel-view-zone-collapse-state
            mevedel-view--status-agent-collapse-key nil))
    t))

(defun mevedel-view--status-task-body (session show-completed)
  "Return propertized status-zone task text for SESSION and SHOW-COMPLETED."
  (let ((body (mevedel-tool-task--display-string session show-completed)))
    (add-text-properties 0 (length body) '(mevedel-tool-task t) body)
    body))

(defun mevedel-view--status-session (&optional data-buf)
  "Return DATA-BUF session used for status rendering."
  (or (and data-buf
           (buffer-live-p data-buf)
           (buffer-local-value 'mevedel--session data-buf))
      (and (boundp 'mevedel--session) mevedel--session)
      (and (boundp 'mevedel--data-buffer)
           (buffer-live-p mevedel--data-buffer)
           (buffer-local-value 'mevedel--session mevedel--data-buffer))))

(defun mevedel-view--status-model (&optional data-buf)
  "Return the authoritative status-zone model for DATA-BUF."
  (let* ((session (mevedel-view--status-session data-buf))
         (show-completed (mevedel-view--status-task-show-completed-p))
         (task-active-p (and session
                             (require 'mevedel-tool-task nil t)
                             (mevedel-tool-task--session-has-active-p
                              session)))
         (task-body (and task-active-p
                         (mevedel-view--status-task-body
                          session show-completed)))
         (agent-rows (mevedel-view--agent-status-collect))
         (agent-expanded-p (and agent-rows
                                (mevedel-view--status-agent-expanded-p)))
         (agent-body (and agent-rows
                          (if agent-expanded-p
                              (mevedel-view--agent-status-handles-string
                               agent-rows)
                            (mevedel-view--agent-status-string
                             agent-rows nil)))))
    (list :session session
          :show-completed show-completed
          :task-active-p task-active-p
          :task-body task-body
          :agent-rows agent-rows
          :agent-expanded-p agent-expanded-p
          :agent-body agent-body)))

(defun mevedel-view--status-fragments (model)
  "Return status fragments for MODEL."
  (let (fragments)
    (when-let* ((body (plist-get model :task-body)))
      (let ((fragment (list :namespace 'status
                            :id 'tasks
                            :priority 100
                            :body body
                            :keymap (mevedel-view--status-task-keymap)
                            :navigatable t
                            :activate #'mevedel-toggle-tasks
                            :entry 'tasks
                            :collapsible t
                            :collapse-key mevedel-view--status-task-collapse-key
                            :collapsed (not (plist-get model
                                                        :show-completed))))
            (suffix (mevedel-view--status-trailing-newline-suffix body)))
        (when suffix
          (setq fragment (plist-put fragment :body-suffix suffix)))
        (push fragment fragments)))
    (when-let* ((body (plist-get model :agent-body)))
      (let ((fragment (list :namespace 'status
                            :id 'agents
                            :priority 0
                            :body body
                            :keymap (mevedel-view--display-fragment-keymap)
                            :navigatable t
                            :activate #'mevedel-view-agent-status-toggle
                            :entry 'agents
                            :collapsible t
                            :collapse-key mevedel-view--status-agent-collapse-key
                            :collapsed (not (plist-get model
                                                        :agent-expanded-p))))
            (suffix (mevedel-view--status-trailing-newline-suffix body)))
        (when suffix
          (setq fragment (plist-put fragment :body-suffix suffix)))
        (push fragment fragments)))
    (nreverse fragments)))

(defun mevedel-view--render-status (&optional data-buf)
  "Render task and aggregate agent status fragments for DATA-BUF."
  (unless mevedel-view--agent-transcript-p
    (require 'mevedel-view-zone)
    (let* ((model (mevedel-view--status-model data-buf))
           (fragments (mevedel-view--status-fragments model))
           (start (mevedel-view--status-anchor))
           (input-pos (mevedel-view--input-marker-position))
           (interaction-pos (mevedel-view--current-buffer-marker-position
                             mevedel-view--interaction-marker))
           (end (if (and interaction-pos
                         (<= start interaction-pos)
                         (or (not input-pos) (<= interaction-pos input-pos)))
                    interaction-pos
                  start)))
      (mevedel-view-zone-reconcile 'status start end fragments))))

(defun mevedel-view--render-agent-status ()
  "Render or remove the aggregate live agent status text."
  (mevedel-view--render-status))

(defun mevedel-view--agent-status-region-position-p (pos)
  "Return non-nil when POS is inside the aggregate status fragment."
  (and (integer-or-marker-p pos)
       (eq (get-text-property pos 'mevedel-view-zone-namespace) 'status)
       (eq (get-text-property pos 'mevedel-view-zone-id) 'agents)))

(defun mevedel-view--agent-source-present-p (agent-id)
  "Return non-nil if the data buffer has an Agent source for AGENT-ID."
  (when (and (boundp 'mevedel--data-buffer)
             (buffer-live-p mevedel--data-buffer))
    (let ((data-buf mevedel--data-buffer))
      (with-current-buffer data-buf
        (save-restriction
          (widen)
          (catch 'found
            (dolist (seg (mevedel-transcript-segments (point-min) (point-max)))
              (when (eq (car seg) 'tool)
                (when-let* ((call (mevedel-view--tool-call-parse
                                   data-buf (cadr seg) (caddr seg))))
                  (when (and (equal (plist-get call :name) "Agent")
                             (equal (plist-get (plist-get call :render-data)
                                               :agent-id)
                                    agent-id))
                    (throw 'found t)))))
            nil))))))

(defun mevedel-view--agent-handle-refresh-points (agent-id)
  "Return source-backed visible handle positions for AGENT-ID.
The return value is (POINTS . STALE-P), where STALE-P means a visible
non-status handle existed but lacked usable source metadata."
  (let ((pos (point-min))
        points
        stale-p)
    (while (< pos (point-max))
      (let* ((id (get-text-property pos 'mevedel-view-agent-id))
             (handle-p (get-text-property pos 'mevedel-view-agent-handle-p))
             (source (get-text-property pos 'mevedel-view-source)))
        (when (and handle-p
                   (equal id agent-id)
                   (not (mevedel-view--agent-status-region-position-p pos)))
          (if (and (consp source)
                   (integer-or-marker-p (car source))
                   (integer-or-marker-p (cdr source)))
              (unless (cl-find-if
                       (lambda (point)
                         (eq (get-text-property point 'mevedel-view-source)
                             source))
                       points)
                (push pos points))
            (setq stale-p t))))
      (setq pos (or (next-single-property-change
                     pos 'mevedel-view-agent-id nil (point-max))
                    (point-max))))
    (cons (sort points #'>) stale-p)))

(defun mevedel-view--refresh-agent-handle-at (pos _agent-id)
  "Refresh the rendered source-backed agent handle at POS.
Return non-nil on success."
  (save-excursion
    (goto-char pos)
    (let* ((source (get-text-property pos 'mevedel-view-source))
           (bounds (mevedel-view--section-bounds))
           (data-buf mevedel--data-buffer)
           (current-collapsed (and (get-text-property
                                    pos 'mevedel-view-collapsed)
                                   t))
           (state (mevedel-view--source-collapse-state-entry
                   source 'agent-handle))
           (collapsed (if state (cdr state) current-collapsed))
           (turn-id (get-text-property pos 'mevedel-view-turn-id))
           (in-flight-after-section-p
            (and bounds
                 (when-let* ((start (mevedel-view--in-flight-turn-start-position)))
                   (<= (car bounds) start (cdr bounds)))))
           (rendering
            (and bounds
                 data-buf
                 (buffer-live-p data-buf)
                 (mevedel-view--segment-rendering
                  data-buf (car source) (cdr source) collapsed))))
      (when (and bounds rendering)
        (unless state
          (mevedel-view--record-source-collapse-state
           source 'agent-handle collapsed))
        (let* ((view-start (car bounds))
               (view-end (cdr bounds))
               (rendering (mevedel-view--rendering-with-collapse-state
                           (plist-put (copy-sequence rendering)
                                      :initially-collapsed-p collapsed)
                           source)))
          (goto-char view-start)
          (set-marker-insertion-type mevedel-view--input-marker t)
          (unwind-protect
              (let ((ins-start (point)))
                (delete-region view-start view-end)
                (mevedel-view--insert-rendered-tool rendering source)
                (when turn-id
                  (put-text-property ins-start (point)
                                     'mevedel-view-turn-id turn-id))
                (when in-flight-after-section-p
                  (set-marker mevedel-view--in-flight-turn-start ins-start)))
            (set-marker-insertion-type mevedel-view--input-marker nil))
          t)))))

(defun mevedel-view--refresh-agent-rendering-now (agent-id)
  "Refresh visible rendering for AGENT-ID in the current view buffer."
  (let ((start-time (float-time))
        stale-p)
    (mevedel-view--call-preserving-window-state
     (lambda ()
       (mevedel-view--call-preserving-input-point
       (lambda ()
         (mevedel-view--call-preserving-input-text
          (lambda ()
            (let ((inhibit-read-only t)
                  (inhibit-modification-hooks t))
              (pcase-let ((`(,points . ,stale)
                           (mevedel-view--agent-handle-refresh-points agent-id)))
                (setq stale-p (or stale
                                  (and (null points)
                                       (mevedel-view--agent-source-present-p
                                        agent-id))))
                (dolist (point points)
                  (unless (mevedel-view--refresh-agent-handle-at point agent-id)
                    (setq stale-p t))))
              (mevedel-view--render-agent-status))))))))
    (mevedel-view--debug-log
     'agent-refresh
     :agent-id agent-id
     :elapsed (- (float-time) start-time)
     :fallback stale-p)
    (when stale-p
      (mevedel-view-rerender (current-buffer)))
    (not stale-p)))

(defun mevedel-view-refresh-agent-rendering (view-buffer agent-id)
  "Refresh VIEW-BUFFER's visible rendering for AGENT-ID.
Rapid calls for the same agent are coalesced so tool start/finish bursts update
one handle/status row without scheduling repeated full rerenders."
  (when (and agent-id (buffer-live-p view-buffer))
    (with-current-buffer view-buffer
      (unless (hash-table-p mevedel-view--agent-refresh-timers)
        (setq mevedel-view--agent-refresh-timers
              (make-hash-table :test #'equal)))
      (when-let* ((timer (gethash agent-id mevedel-view--agent-refresh-timers)))
        (when (timerp timer)
          (cancel-timer timer)))
      (if (or (not (numberp mevedel-view-agent-refresh-delay))
              (<= mevedel-view-agent-refresh-delay 0))
          (mevedel-view--refresh-agent-rendering-now agent-id)
        (puthash
         agent-id
         (run-at-time
          mevedel-view-agent-refresh-delay nil
          (lambda (buffer id)
            (when (buffer-live-p buffer)
              (with-current-buffer buffer
                (when (hash-table-p mevedel-view--agent-refresh-timers)
                  (remhash id mevedel-view--agent-refresh-timers))
                (mevedel-view--refresh-agent-rendering-now id))))
          view-buffer agent-id)
         mevedel-view--agent-refresh-timers)))))

(defun mevedel-view-agent-status-toggle ()
  "Toggle the aggregate live agent status rows."
  (interactive)
  (require 'mevedel-view-zone)
  (let ((collapsed (mevedel-view--status-agent-expanded-p)))
    (mevedel-view-zone-set-collapse-state
     mevedel-view--status-agent-collapse-key collapsed))
  (mevedel-view--render-agent-status))

(defun mevedel-view--interaction-target-buffer (&optional data-buffer)
  "Return the live view buffer that should host queued interactions.
DATA-BUFFER, when non-nil, is the chat/data buffer whose
`mevedel--view-buffer' binding should be consulted.  Signals when
there is no live non-transcript view.  Queue renderers catch this
as a render failure and abort the visible head rather than
silently placing controls in a data buffer."
  (cl-labels
      ((live-interaction-view-p (view)
         (and view
              (buffer-live-p view)
              (with-current-buffer view
                (and (not (bound-and-true-p
                           mevedel-view--agent-transcript-p))
                     (boundp 'mevedel-view--interaction-marker)
                     (markerp mevedel-view--interaction-marker)
                     (eq (marker-buffer mevedel-view--interaction-marker)
                         view)))))
       (view-for-data-buffer (buf &optional seen)
         (when (and buf
                    (buffer-live-p buf)
                    (not (memq buf seen)))
           (or (let ((view (buffer-local-value 'mevedel--view-buffer
                                               buf)))
                 (and (live-interaction-view-p view) view))
               (when-let* ((inv (buffer-local-value
                                 'mevedel--agent-invocation buf))
                           ((mevedel-agent-invocation-p inv))
                           (parent (mevedel-agent-invocation-parent-data-buffer
                                    inv)))
                 (view-for-data-buffer parent (cons buf seen)))))))
    (or (and (live-interaction-view-p (current-buffer))
             (current-buffer))
        (view-for-data-buffer data-buffer)
        (view-for-data-buffer (current-buffer))
        (and (boundp 'mevedel--view-buffer)
             (live-interaction-view-p mevedel--view-buffer)
             mevedel--view-buffer)
        (error "No live view for queued prompt"))))

(defun mevedel-view--zone-separator (label)
  "Return a propertized zone separator line for LABEL.
Format: ` ─── LABEL ─── ' followed by enough box-drawing dashes
to reach `(max 4 (min 60 (- (window-width) 4)))' total length,
then a trailing newline.  Width is clamped so very narrow windows
don't produce zero-length rules.

LABEL is a single string like \"tasks\", \"1 permission pending\",
\"2 previews · 1 plan pending\".  Caller is responsible for
constructing composite count labels via concatenation.

The returned string carries `mevedel-view-zone-separator' face on
the dash runs and inherits the same face on the label text so
tweaks via `customize-face' apply uniformly."
  (let* ((win-widths
          ;; Use the widest window currently displaying this buffer
          ;; if any; fall back to the selected window.  Bare
          ;; (window-width) returned the selected window's width
          ;; even when the call originated from an unrelated
          ;; context (e.g. a sub-agent FSM hook), producing a
          ;; mis-sized rule for split-window setups.
          (or (mapcar #'window-width
                      (get-buffer-window-list (current-buffer) nil t))
              (list (window-width))))
         (target-width (max 4 (min 60 (- (apply #'max win-widths) 4))))
         (pre " ─── ")
         (post " ")
         (decorated-label (or label ""))
         (used (+ (length pre) (length decorated-label) (length post)))
         (tail-len (max 3 (- target-width used)))
         (tail (concat (make-string tail-len ?─))))
    (concat
     (propertize (concat pre decorated-label post tail "\n")
                 'face 'mevedel-view-zone-separator
                 'font-lock-face 'mevedel-view-zone-separator))))

(defun mevedel-view--interaction-plural (n singular plural)
  "Return N followed by SINGULAR or PLURAL."
  (format "%d %s" n (if (= n 1) singular plural)))

(defun mevedel-view--interaction-count-label ()
  "Return the composite interaction-zone counter label, or nil."
  (let ((previews 0)
        (plans 0)
        (requests 0)
        (asks 0)
        (permissions 0)
        (queued-user-messages 0)
        parts)
    (when (hash-table-p mevedel-view--interaction-descriptors)
      (maphash
       (lambda (_id descriptor)
         (let ((count (or (plist-get descriptor :count) 0)))
           (pcase (plist-get descriptor :kind)
             ('preview (cl-incf previews count))
             ('plan (cl-incf plans count))
             ('request (cl-incf requests (max 1 count)))
             ('ask (cl-incf asks (max 1 count)))
             ('permission (cl-incf permissions count))
             ('queued-user-message
              (cl-incf queued-user-messages count)))))
       mevedel-view--interaction-descriptors))
    (let ((session (or (and (boundp 'mevedel--session)
                            mevedel--session)
                       (and (boundp 'mevedel--data-buffer)
                            (buffer-live-p mevedel--data-buffer)
                            (buffer-local-value 'mevedel--session
                                                mevedel--data-buffer)))))
      (when session
        (setq plans (max plans (length (mevedel-session-plan-queue session))))
        (setq permissions
              (max permissions
                   (length (mevedel-session-permission-queue session))))
        (setq queued-user-messages
              (max queued-user-messages
                   (length (mevedel-session-queued-user-messages session))))))
    (setq parts
          (delq nil
                (list
                 (when (> previews 0)
                   (mevedel-view--interaction-plural
                    previews "preview" "previews"))
                 (when (> plans 0)
                   (mevedel-view--interaction-plural plans "plan" "plans"))
                 (when (> requests 0)
                   (mevedel-view--interaction-plural
                    requests "request" "requests"))
                 (when (> asks 0)
                   (mevedel-view--interaction-plural asks "question" "questions"))
                 (when (> permissions 0)
                   (mevedel-view--interaction-plural
                    permissions "permission" "permissions"))
                 (when (> queued-user-messages 0)
                   (mevedel-view--interaction-plural
                    queued-user-messages "queued message"
                    "queued messages")))))
    (when parts
      (concat (string-join parts " · ") " pending"))))

(defun mevedel-view--interaction-kind-priority (kind)
  "Return the stable interaction overlay priority for KIND."
  (pcase kind
    ('preview 300)
    ('plan 200)
    ((or 'request 'ask) 150)
    ('permission 100)
    ('queued-user-message 80)
    (_ 50)))

(defun mevedel-view--interaction-preserve-on-rebuild-p (descriptor)
  "Return non-nil when DESCRIPTOR owns direct prompt state.
Direct request and preview prompts carry callbacks that are not represented
by a session queue.  Normal view rebuilds must keep them alive; explicit
clear/teardown paths still remove them."
  (memq (plist-get descriptor :kind) '(preview request ask)))

(defun mevedel-view--interaction-body (descriptor overlay)
  "Return DESCRIPTOR's body with standard interaction text properties.
OVERLAY is stored on the text as the descriptor's callback handle."
  (let* ((body (copy-sequence
                (mevedel--normalize-message-text
                 (or (plist-get descriptor :body) ""))))
         (map (mevedel-view--display-fragment-keymap
               (plist-get descriptor :keymap)))
         (help (plist-get descriptor :help-echo))
         (kind (plist-get descriptor :kind))
         (id (plist-get descriptor :id))
         (read-only (if (plist-member descriptor :read-only)
                        (plist-get descriptor :read-only)
                      t)))
    (add-text-properties
     0 (length body)
     `(mevedel-view-interaction-kind ,kind
       mevedel-view-interaction-id ,id
       mevedel-view-interaction-overlay ,overlay
       read-only ,read-only
       front-sticky nil
       rear-nonsticky t)
     body)
    (when map
      (add-text-properties 0 (length body) `(keymap ,map) body))
    (when help
      (add-text-properties 0 (length body) `(help-echo ,help) body))
    body))

(defun mevedel-view--interaction-region-end ()
  "Return the end boundary for fragment-managed interaction text."
  (let ((progress-start (mevedel-view--request-progress-region-start))
        (input-pos (mevedel-view--input-marker-position)))
    (or (and progress-start
             (or (not input-pos) (<= progress-start input-pos))
             progress-start)
        input-pos
        (point-max))))

(defun mevedel-view--interaction-descriptor-pairs ()
  "Return live interaction descriptor pairs sorted by display priority."
  (let (pairs)
    (when (hash-table-p mevedel-view--interaction-descriptors)
      (maphash
       (lambda (id descriptor)
         (push (cons id descriptor) pairs))
       mevedel-view--interaction-descriptors))
    (sort pairs
          (lambda (a b)
            (> (or (plist-get (cdr a) :priority)
                   (mevedel-view--interaction-kind-priority
                    (plist-get (cdr a) :kind)))
               (or (plist-get (cdr b) :priority)
                   (mevedel-view--interaction-kind-priority
                    (plist-get (cdr b) :kind))))))))

(defun mevedel-view--interaction-apply-overlay-properties
    (overlay descriptor)
  "Apply DESCRIPTOR metadata to interaction OVERLAY."
  (let ((kind (plist-get descriptor :kind))
        (id (plist-get descriptor :id)))
    (overlay-put overlay 'evaporate nil)
    (overlay-put overlay 'mevedel-view-interaction-kind kind)
    (overlay-put overlay 'mevedel-view-interaction-id id)
    (overlay-put overlay 'priority
                 (or (plist-get descriptor :priority)
                     (mevedel-view--interaction-kind-priority kind)))
    (overlay-put overlay 'read-only
                 (if (plist-member descriptor :read-only)
                     (plist-get descriptor :read-only)
                   t))
    (if-let* ((map (plist-get descriptor :keymap)))
        (overlay-put overlay 'keymap map)
      (overlay-put overlay 'keymap nil))
    (if-let* ((help (plist-get descriptor :help-echo)))
        (overlay-put overlay 'help-echo help)
      (overlay-put overlay 'help-echo nil))
    (if (plist-member descriptor :entry)
        (overlay-put overlay 'mevedel-view-interaction-entry
                     (plist-get descriptor :entry))
      (overlay-put overlay 'mevedel-view-interaction-entry nil))
    (if-let* ((activate (plist-get descriptor :activate)))
        (overlay-put overlay 'mevedel-view-interaction-activate activate)
      (overlay-put overlay 'mevedel-view-interaction-activate nil))
    overlay))

(defun mevedel-view--interaction-overlay-for (id descriptor)
  "Return live callback overlay for ID and DESCRIPTOR."
  (let ((overlay (and (hash-table-p mevedel-view--interaction-overlays)
                      (gethash id mevedel-view--interaction-overlays))))
    (unless (and (overlayp overlay) (overlay-buffer overlay))
      (let ((anchor (mevedel-view--interaction-anchor)))
        (setq overlay (make-overlay anchor anchor (current-buffer) nil t))))
    (when (hash-table-p mevedel-view--interaction-overlays)
      (puthash id overlay mevedel-view--interaction-overlays))
    (mevedel-view--interaction-apply-overlay-properties overlay descriptor)
    overlay))

(defun mevedel-view--interaction-separator-fragment (label)
  "Return the non-navigatable interaction separator fragment for LABEL."
  (list :namespace 'interaction
        :id :separator
        :priority 1000
        :body (mevedel-view--zone-separator label)
        :navigatable nil))

(defun mevedel-view--interaction-fragment (id descriptor)
  "Return a fragment plist for interaction DESCRIPTOR ID."
  (let* ((overlay (mevedel-view--interaction-overlay-for id descriptor))
         (body (mevedel-view--interaction-body descriptor overlay))
         (fragment (list :namespace 'interaction
                         :id id
                         :priority (or (plist-get descriptor :priority)
                                       (mevedel-view--interaction-kind-priority
                                        (plist-get descriptor :kind)))
                         :body body
                         :keymap (mevedel-view--display-fragment-keymap
                                  (plist-get descriptor :keymap))
                         :help-echo (plist-get descriptor :help-echo)
                         :entry (plist-get descriptor :entry)
                         :activate (plist-get descriptor :activate)
                         :navigatable (and (or (plist-get descriptor :activate)
                                               (plist-get descriptor :keymap))
                                           t))))
    (when (plist-member descriptor :read-only)
      (setq fragment (plist-put fragment :read-only
                                (plist-get descriptor :read-only))))
    fragment))

(defun mevedel-view--interaction-delete-stale-overlays ()
  "Delete descriptor overlays whose descriptors are no longer live."
  (when (hash-table-p mevedel-view--interaction-overlays)
    (maphash
     (lambda (id overlay)
       (unless (and (hash-table-p mevedel-view--interaction-descriptors)
                    (gethash id mevedel-view--interaction-descriptors))
         (delete-overlay overlay)
         (remhash id mevedel-view--interaction-overlays)))
     mevedel-view--interaction-overlays)))

(defun mevedel-view--interaction-sync-overlays (pairs)
  "Move descriptor callback overlays for PAIRS to fragment bounds."
  (dolist (pair pairs)
    (pcase-let* ((`(,id . ,descriptor) pair)
                 (overlay (and (hash-table-p mevedel-view--interaction-overlays)
                               (gethash id mevedel-view--interaction-overlays)))
                 (bounds (mevedel-view-zone-fragment-bounds
                          'interaction id)))
      (when (and (overlayp overlay) bounds)
        (move-overlay overlay
                      (plist-get bounds :start)
                      (plist-get bounds :end)
                      (current-buffer))
        (mevedel-view--interaction-apply-overlay-properties
         overlay descriptor)))))

(defun mevedel-view--interaction-render ()
  "Render interaction-zone fragments and descriptor callback overlays."
  (require 'mevedel-view-zone)
  (let* ((label (mevedel-view--interaction-count-label))
         (pairs (mevedel-view--interaction-descriptor-pairs))
         (render-p (or label pairs
                       (mevedel-view-zone-region 'interaction))))
    (mevedel-view--interaction-delete-stale-overlays)
    (when render-p
      (let* ((start (mevedel-view--interaction-anchor))
             (end (max start (mevedel-view--interaction-region-end)))
             (fragments
              (append
               (when label
                 (list (mevedel-view--interaction-separator-fragment label)))
               (mapcar
                (lambda (pair)
                  (pcase-let ((`(,id . ,descriptor) pair))
                    (mevedel-view--interaction-fragment id descriptor)))
                pairs))))
        (mevedel-view-zone-reconcile 'interaction start end fragments)
        (mevedel-view--interaction-sync-overlays pairs)))))

(defun mevedel-view--interaction-rebuild ()
  "Rebuild interaction-zone descriptors from live preview and queue state.
This deletes only interaction UI overlays and never settles callbacks."
  (unless mevedel-view--agent-transcript-p
    (mevedel-view--interaction-clear-for-rebuild)
    (when-let* ((session (or (and (boundp 'mevedel--session)
                                  mevedel--session)
                             (and (boundp 'mevedel--data-buffer)
                                  (buffer-live-p mevedel--data-buffer)
                                  (buffer-local-value
                                   'mevedel--session
                                   mevedel--data-buffer)))))
      (when (mevedel-session-plan-queue session)
        (require 'mevedel-tool-plan)
        (mevedel-plan-queue--render-head session))
      (when (mevedel-session-permission-queue session)
        (require 'mevedel-permission-queue)
        (mevedel-permission-queue--render-head session))
      (when (mevedel-session-queued-user-messages session)
        (mevedel-view--queued-user-messages-render session)))
    (mevedel-view--interaction-render)))

(defun mevedel-view--interaction-register (descriptor)
  "Register DESCRIPTOR in the interaction zone and return its overlay."
  (unless (hash-table-p mevedel-view--interaction-descriptors)
    (setq mevedel-view--interaction-descriptors
          (make-hash-table :test #'equal)))
  (unless (hash-table-p mevedel-view--interaction-overlays)
    (setq mevedel-view--interaction-overlays
          (make-hash-table :test #'equal)))
  (let* ((id (plist-get descriptor :id))
         (anchor (mevedel-view--interaction-anchor))
         (existing-overlay
          (and (hash-table-p mevedel-view--interaction-overlays)
               (gethash id mevedel-view--interaction-overlays)))
         (overlay (or existing-overlay
                      (make-overlay anchor anchor (current-buffer) nil t))))
    (puthash id descriptor mevedel-view--interaction-descriptors)
    (puthash id overlay mevedel-view--interaction-overlays)
    (mevedel-view--interaction-apply-overlay-properties overlay descriptor)
    (mevedel-view--interaction-render)
    overlay))

(defun mevedel-view--interaction-unregister (id)
  "Remove interaction-zone descriptor ID and its overlay."
  (when (hash-table-p mevedel-view--interaction-descriptors)
    (remhash id mevedel-view--interaction-descriptors))
  (when (hash-table-p mevedel-view--interaction-overlays)
    (when-let* ((overlay (gethash id mevedel-view--interaction-overlays)))
          (when (and (boundp 'mevedel--prompt-overlays)
                     (listp mevedel--prompt-overlays))
            (setq mevedel--prompt-overlays
                  (delq overlay mevedel--prompt-overlays)))
              (delete-overlay overlay))
            (remhash id mevedel-view--interaction-overlays))
          (mevedel-view--interaction-render))

(defun mevedel-view--interaction-clear-for-rebuild ()
  "Delete rebuild-owned interaction UI while preserving direct prompt UI."
  (let (remove-ids)
    (when (hash-table-p mevedel-view--interaction-descriptors)
      (maphash
       (lambda (id descriptor)
         (unless (mevedel-view--interaction-preserve-on-rebuild-p descriptor)
           (push id remove-ids)))
       mevedel-view--interaction-descriptors))
    (dolist (id remove-ids)
      (when (hash-table-p mevedel-view--interaction-overlays)
        (when-let* ((overlay (gethash id
                                      mevedel-view--interaction-overlays)))
          (delete-overlay overlay))
        (remhash id mevedel-view--interaction-overlays))
      (when (hash-table-p mevedel-view--interaction-descriptors)
        (remhash id mevedel-view--interaction-descriptors))))
  (when (and (boundp 'mevedel--prompt-overlays)
             (listp mevedel--prompt-overlays))
    (let (live)
      (dolist (ov mevedel--prompt-overlays)
        (let* ((id (and (overlayp ov)
                        (overlay-get ov 'mevedel-view-interaction-id)))
               (descriptor
                (and id
                     (hash-table-p mevedel-view--interaction-descriptors)
                     (gethash id mevedel-view--interaction-descriptors))))
          (cond
           ((not (and (overlayp ov) (overlay-buffer ov))))
           ((and (eq (overlay-buffer ov) (current-buffer))
                 id
                 (not (mevedel-view--interaction-preserve-on-rebuild-p
                       descriptor)))
            (delete-overlay ov))
           (t
            (push ov live)))))
      (setq mevedel--prompt-overlays (nreverse live))))
  (mevedel-view--interaction-render))

(defun mevedel-view--interaction-clear ()
  "Delete all interaction-zone overlays without firing callbacks."
  (when (hash-table-p mevedel-view--interaction-descriptors)
    (clrhash mevedel-view--interaction-descriptors))
  (mevedel-view--interaction-render)
  (when (hash-table-p mevedel-view--interaction-overlays)
    (maphash (lambda (_id overlay) (delete-overlay overlay))
             mevedel-view--interaction-overlays)
    (clrhash mevedel-view--interaction-overlays))
  (when (and (boundp 'mevedel--prompt-overlays)
             (listp mevedel--prompt-overlays))
    (let (live)
      (dolist (ov mevedel--prompt-overlays)
        (cond
         ((not (and (overlayp ov) (overlay-buffer ov))))
         ((and (eq (overlay-buffer ov) (current-buffer))
               (overlay-get ov 'mevedel-view-interaction-id))
          (delete-overlay ov))
         (t
          (push ov live))))
      (setq mevedel--prompt-overlays (nreverse live)))))

(defun mevedel-view--header-end-position ()
  "Return the position after the current view header, when recognized."
  (or
   (when (eq (get-text-property (point-min) 'font-lock-face)
             'mevedel-view-header)
     (save-excursion
       (goto-char (point-min))
       (let ((end (line-end-position)))
         (if (and (< end (point-max))
                  (eq (char-after end) ?\n))
             (1+ end)
           end))))
   (when-let* ((data-buf (and (boundp 'mevedel--data-buffer)
                              mevedel--data-buffer))
               ((buffer-live-p data-buf))
               (header (substring-no-properties
                        (mevedel-view--header-string data-buf)))
               (end (+ (point-min) (length header)))
               ((<= end (point-max)))
               ((equal header
                       (buffer-substring-no-properties (point-min) end))))
     end)))

(defun mevedel-view--interaction-anchor ()
  "Return the buffer position to anchor an interaction-zone overlay.
View buffers require a live `mevedel-view--interaction-marker'.  If its
position has drifted outside the status/input boundaries, repair it to the
current status anchor.  Non-view buffers use `(point-max)' so tool rendering
can still build isolated fragments."
  (if (not (derived-mode-p 'mevedel-view-mode))
      (point-max)
    (unless (and (markerp mevedel-view--interaction-marker)
                 (eq (marker-buffer mevedel-view--interaction-marker)
                     (current-buffer))
                 (marker-position mevedel-view--interaction-marker))
      (error "View interaction marker is not live"))
    (let* ((input-pos (mevedel-view--input-marker-position))
           (status-pos (mevedel-view--status-anchor))
           (interaction-pos (marker-position
                             mevedel-view--interaction-marker)))
      (if (and (>= interaction-pos status-pos)
               (or (not input-pos) (<= interaction-pos input-pos)))
          interaction-pos
        (let ((anchor (if input-pos
                          (min status-pos input-pos)
                        status-pos)))
          (set-marker mevedel-view--interaction-marker anchor)
          anchor)))))

(defun mevedel-view--insert-attribution
    (agent-id &optional _live-click-p calls)
  "Insert the `from <type>--<idshort>' attribution fragment for AGENT-ID.
Returns the propertized string (does not modify the buffer).
The agent-id portion is always propertized as a click target.
Click dispatches through
`mevedel-view--open-agent-transcript-or-message', which either
opens via `mevedel-view-open-agent-transcript' or reports why the
transcript is not openable yet.

Running transcripts open from the live invocation buffer when present.
CALLS, when non-nil, is used in the running-state echo-area message."
  (let* ((display-label (mevedel-view--display-label-for-agent agent-id))
         (entry (mevedel-view--lookup-transcript-entry agent-id))
         (inv (mevedel-view--agent-invocation agent-id))
         (status (mevedel-view--agent-effective-status inv entry))
         (session (and (boundp 'mevedel--data-buffer)
                       mevedel--data-buffer
                       (buffer-live-p mevedel--data-buffer)
                       (buffer-local-value 'mevedel--session
                                           mevedel--data-buffer)))
         (save-path (and session (mevedel-session-save-path session)))
         (rel-path (and entry (plist-get entry :path)))
         (path-ok (and entry save-path
                       (mevedel-session-persistence--validate-transcript-path
                        rel-path save-path)))
         (terminal-p (memq status
                           '(completed error aborted incomplete)))
         (live-openable (and (eq status 'running) inv))
         (openable (or (and path-ok terminal-p) live-openable))
         (echo (cond
                (openable (format "Open transcript for %s" agent-id))
                ((eq status 'running)
                 (format
                  "Agent %s still running%s. Live buffer unavailable."
                  display-label
                  (if (integerp calls)
                      (format " (%d tool calls)" calls)
                    "")))
                ((not entry)
                 (format "No transcript recorded for %s" agent-id))
                ((not path-ok)
                 (format "Transcript path is unavailable for %s" agent-id))
                (t (format "Transcript unavailable for %s" agent-id))))
         (header (concat "from " display-label))
         (s (copy-sequence header)))
    (add-text-properties 0 (length s)
                         (list 'font-lock-face 'mevedel-view-attribution)
                         s)
    (let* ((from-prefix-len (length "from "))
           (id-end (length s))
           (open-fn
            (lambda ()
              (interactive)
              (mevedel-view--open-agent-transcript-or-message
               agent-id nil calls)))
           (map (make-sparse-keymap)))
      (define-key map [mouse-1] open-fn)
      (define-key map [mouse-2] open-fn)
      (define-key map (kbd "RET") open-fn)
      ;; Apply button-style properties directly to the agent-id
      ;; substring of s so the returned string carries them.  An
      ;; earlier draft passed a substring copy to make-text-button
      ;; which produced a properly-buttoned new string but threw
      ;; it away -- the returned s only had mouse-face.  Now uses
      ;; add-text-properties on the original string so the
      ;; keymap, click action, and link face all stick.
      (add-text-properties
       from-prefix-len id-end
       `(face link
         follow-link t
         mouse-face highlight
         keymap ,map
         mevedel-view-agent-id ,agent-id
         mevedel-view-agent-live-click nil
         mevedel-view-agent-calls ,calls
         help-echo ,echo)
       s))
    s))

(provide 'mevedel-view)

;;; mevedel-view.el ends here
