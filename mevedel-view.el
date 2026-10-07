;;; mevedel-view.el -- Compact view buffer for chat sessions -*- lexical-binding: t -*-

;;; Commentary:

;; Defines the shared ephemeral surface mode and coordinates the user-facing
;; chat view, session lifecycle, and managed zones.  `mevedel-view-composer'
;; owns editable input and submission;
;; `mevedel-view-prepare'
(declare-function mevedel-view-prepare-resume "mevedel-view-prepare" ())
(defvar mevedel-view-prepare-enabled)

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
(require 'mevedel-execution)
(require 'mevedel-theme-faces)
(require 'mevedel-tool-task)
(require 'mevedel-transcript)
(require 'mevedel-transport)
(require 'mevedel-utilities)
(require 'mevedel-view-zone)

;; `browse-url'
(declare-function browse-url "browse-url" (url &optional new-window))

;; `mevedel-agent-control'
(declare-function mevedel-agent-control-teardown-session
                  "mevedel-agent-control" (session))
(autoload 'mevedel-agent-control-teardown-session "mevedel-agent-control")

;; `mevedel-agents'
(declare-function mevedel-agent-invocation-parent-data-buffer
                  "mevedel-agents" (cl-x) t)
(defvar mevedel--agent-invocation)

;; `mevedel-chat'
(declare-function mevedel-abort "mevedel-chat" (&optional buf))

;; `mevedel-execution'
(declare-function mevedel-execution-count-user "mevedel-execution" (session))
(declare-function mevedel-execution-list-user "mevedel-execution" (session))

;; `mevedel-view-audit'
(declare-function mevedel-view-audit-show-control-result
                  "mevedel-view-audit" (execution-id))

;; `mevedel-view-agent'
(declare-function mevedel-view-open-agent-transcript
                  "mevedel-view-agent" (agent-path))
(declare-function mevedel-execution-teardown-session
                  "mevedel-execution" (session))
(declare-function mevedel-execution-unsettled-mutation-p
                  "mevedel-execution" (session))
(defvar mevedel-execution-state-change-hook)

;; `mevedel-execution-target'
(declare-function mevedel-execution-target-label
                  "mevedel-execution-target" (target &optional directory))
(declare-function mevedel-execution-target-native-root
                  "mevedel-execution-target" (cl-x) t)

;; `mevedel-executions-list'
(declare-function mevedel-executions-list-open
                  "mevedel-executions-list" (&optional context))
(autoload 'mevedel-executions-list-open "mevedel-executions-list")

;; `mevedel-journal-capture'
(declare-function mevedel-journal-capture-seal-and-schedule "mevedel-journal-capture"
                  (session buffer trigger &optional captures))
(autoload 'mevedel-journal-capture-seal-and-schedule "mevedel-journal-capture")

;; `mevedel-menu'
(declare-function mevedel-menu "mevedel-menu" ())
(declare-function mevedel-menu-open "mevedel-menu" (area))

;; `mevedel-models'
(declare-function mevedel-model-current-label "mevedel-models"
                  (&optional buffer))

;; `mevedel-pending-inputs'
(declare-function mevedel-pending-inputs-clear
                  "mevedel-pending-inputs" ())

;; `mevedel-permission-queue'
(declare-function mevedel-permission-queue-abort-all
                  "mevedel-permission-queue" (&optional session))

;; `mevedel-plan-mode'
(declare-function mevedel-plan-approval-abort
                  "mevedel-plan-mode" (&optional session outcome))

;; `mevedel-session-artifacts'
(declare-function mevedel-session-artifacts--segment-tail-prompt-count
                  "mevedel-session-artifacts" ())
(declare-function mevedel-session-artifacts-collect-prompts
                  "mevedel-session-artifacts" (buffer))
(declare-function mevedel-session-artifacts-read-segment
                  "mevedel-session-artifacts" (session number))
(declare-function mevedel-session-artifacts-segment-summary-bounds
                  "mevedel-session-artifacts" ())
(autoload 'mevedel-session-artifacts--segment-tail-prompt-count
  "mevedel-session-artifacts")
(autoload 'mevedel-session-artifacts-collect-prompts "mevedel-session-artifacts")
(autoload 'mevedel-session-artifacts-read-segment "mevedel-session-artifacts")
(autoload 'mevedel-session-artifacts-segment-summary-bounds
  "mevedel-session-artifacts")

;; `mevedel-session-publication'
(declare-function mevedel-session-publication-status
                  "mevedel-session-publication" (session))

;; `mevedel-structs'
(declare-function mevedel-session-current-segment "mevedel-structs" (cl-x) t)
(declare-function mevedel-goal-status "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-execution-target
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-goal "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-lease "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-name "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-pending-publication
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-plan-mode "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-preset-name "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-prompt-index "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-workspace "mevedel-structs" (cl-x) t)
(declare-function mevedel-workspace-name "mevedel-structs" (cl-x) t)
(declare-function mevedel-workspace-root "mevedel-structs" (cl-x) t)
(defvar mevedel--data-buffer)
(defvar mevedel--session)
(defvar mevedel--view-buffer)

;; `mevedel-tool-registry'
(declare-function mevedel-tool-display-string "mevedel-tool-registry" (tool-name args))

;; `mevedel-tool-task'
(declare-function mevedel-tool-task-display-string
                  "mevedel-tool-task" (session show-completed))
(declare-function mevedel-tool-task-session-has-active-p
                  "mevedel-tool-task" (session))
(declare-function mevedel-toggle-tasks "mevedel-tool-task" ())
(defvar mevedel-tool-task-status-keymap)

;; `mevedel-tools'
(declare-function mevedel-tools-active-count "mevedel-tools"
                  (&optional buffer))

;; `mevedel-transport'
(declare-function mevedel-transport-busy-p
                  "mevedel-transport" (&optional path))

;; `mevedel-turn'
(declare-function mevedel-request-state-label "mevedel-turn"
                  (&optional buffer))

;; `mevedel-view-agent'
(declare-function mevedel-view--on-agent-transcript-data-killed
                  "mevedel-view-agent" ())
(declare-function mevedel-view-agent-cleanup-parent
                  "mevedel-view-agent" (parent-view))
(declare-function mevedel-view-agent-handle-view-kill
                  "mevedel-view-agent" ())
(declare-function mevedel-view-agent-initialize
                  "mevedel-view-agent" (options data-buffer))
(declare-function mevedel-view-agent-status-fragment
                  "mevedel-view-agent" ())
(declare-function mevedel-view-close-agent-transcript
                  "mevedel-view-agent" ())
(declare-function mevedel-view-open-agent-transcript-at-point
                  "mevedel-view-agent" (&optional event))
(defvar mevedel-view--agent-transcript-p)

;; `mevedel-view-composer'
(declare-function mevedel-view--effective-permission-mode
                  "mevedel-view-composer" ())
(declare-function mevedel-view--input-marker-position
                  "mevedel-view-composer" ())
(declare-function mevedel-view--input-prompt-string
                  "mevedel-view-composer" (&optional mode))
(declare-function mevedel-view-composer-scope-label
                  "mevedel-view-composer" (&optional scope))
(declare-function mevedel-view--permission-mode-display
                  "mevedel-view-composer" (mode))
(declare-function mevedel-view--plan-mode-p "mevedel-view-composer" ())
(declare-function mevedel-view--position-in-input-region-p
                  "mevedel-view-composer" (position))
(declare-function mevedel-view--sanitize-undo "mevedel-view-composer" ())
(declare-function mevedel-view-abort "mevedel-view-composer" ())
(declare-function mevedel-view-composer-initialize
                  "mevedel-view-composer" ())
(defvar mevedel-view--composer-keymap-overlay)
(defvar mevedel-view--composer-scope)
(defvar mevedel-view--input-marker)

;; `mevedel-view-control-transfer'
(declare-function mevedel-view-control-transfer-stop-polling
                  "mevedel-view-control-transfer" ())
(declare-function mevedel-view-control-transfer-teardown
                  "mevedel-view-control-transfer" ())
(autoload 'mevedel-view-control-transfer-stop-polling
  "mevedel-view-control-transfer")
(autoload 'mevedel-view-control-transfer-teardown
  "mevedel-view-control-transfer")

;; `mevedel-view-disclosure'
(declare-function mevedel-view-toggle-section "mevedel-view-disclosure" ())

;; `mevedel-view-history'
(declare-function mevedel-view-history-save
                  "mevedel-view-history" (&optional view-buffer))

;; `mevedel-view-interaction'
(declare-function mevedel-view--interaction-clear
                  "mevedel-view-interaction" ())
(declare-function mevedel-view--interaction-rebuild
                  "mevedel-view-interaction" ())
(declare-function mevedel-view-interaction-initialize
                  "mevedel-view-interaction" ())

;; `mevedel-view-markdown'
(declare-function mevedel-view--buffer-substring-filter
                  "mevedel-view-markdown" (beg end &optional delete))
(declare-function mevedel-view--enable-markdown-realign
                  "mevedel-view-markdown" ())
(autoload 'mevedel-view--buffer-substring-filter "mevedel-view-markdown")
(autoload 'mevedel-view--enable-markdown-realign "mevedel-view-markdown")
(autoload 'mevedel-view--normalize-local-file-uri-path
  "mevedel-view-markdown")

;; `mevedel-view-path'
(declare-function mevedel-view-path-teardown "mevedel-view-path" ())

;; `mevedel-view-render'
(declare-function mevedel-view--after-header-position
                  "mevedel-view-render" ())
(declare-function mevedel-view--expand-turn "mevedel-view-render" ())
(declare-function mevedel-view--full-rerender
                  "mevedel-view-render"
                  (&optional transcript-buffer source-changed-p))
(declare-function mevedel-view--history-tail-position
                  "mevedel-view-render" ())
(declare-function mevedel-view--non-history-view-position-p
                  "mevedel-view-render" (pos))
(declare-function mevedel-view--prompt-preview
                  "mevedel-view-render" (text shared-display))
(declare-function mevedel-view--refresh-tool-row
                  "mevedel-view-render" (data-buffer tool-use-id))
(declare-function mevedel-view-next-display "mevedel-view-render" ())
(declare-function mevedel-view-previous-display "mevedel-view-render" ())
(declare-function mevedel-view-render-batched-full "mevedel-view-render" ())
(declare-function mevedel-view-render-initialize
                  "mevedel-view-render" ())
(declare-function mevedel-view-render-invalidate-live-tail
                  "mevedel-view-render" ())
(declare-function mevedel-view-render-resume-batch "mevedel-view-render" ())
(declare-function mevedel-view-render-toggle-user-input
                  "mevedel-view-render" ())
(declare-function mevedel-view-toggle-transcript "mevedel-view-render" ())
(declare-function mevedel-view--user-turn-text
                  "mevedel-view-render" (segments data-buf))

;; `mevedel-view-segments'
(declare-function mevedel-view-historical-segment-p
                  "mevedel-view-segments" ())
(declare-function mevedel-view-segments-jump-to-prompt
                  "mevedel-view-segments" (segment source-pos &optional window))

;; `mevedel-view-stream'
(declare-function mevedel-view--refresh-animation-options
                  "mevedel-view-stream" ())
(declare-function mevedel-view--ensure-request-progress
                  "mevedel-view-stream" (&optional data-buf status))
(declare-function mevedel-view--stop-spinner-timer
                  "mevedel-view-stream" ())
(declare-function mevedel-view--render-stream-update
                  "mevedel-view-stream" (data-buf))
(declare-function mevedel-view--start-spinner-timer
                  "mevedel-view-stream" (&optional resumed))
(declare-function mevedel-view-stream--schedule-execution-row-recovery
                  "mevedel-view-stream" (data-buffer))

;; `mevedel-view-zone'
(declare-function mevedel-view-zone-collapse-state
                  "mevedel-view-zone" (key &optional default))
(declare-function mevedel-view-zone-reconcile
                  "mevedel-view-zone" (zone start end fragments))

;; `org'
(declare-function org-mode "ext:org" ())


;;
;;; Customization

(defcustom mevedel-view-inline-image-max-width 600
  "Maximum width for inline images rendered in the view.
A positive integer is a fixed pixel width.  A float in (0, 1] sizes
each image to that fraction of the displaying window's pixel width
and re-scales it when the window changes."
  :type '(restricted-sexp
          :tag "Positive pixels or window-width fraction"
          :match-alternatives
          ((lambda (value)
             (or (and (integerp value) (> value 0))
                 (and (floatp value)
                      (< 0.0 value)
                      (<= value 1.0))))))
  :group 'mevedel)

(defvar-local mevedel-view--side-conversation-p nil
  "Non-nil when this view displays an ephemeral `/btw' conversation.")

(defvar-local mevedel-view--transcript-start nil
  "Data-buffer marker before which transcript projection is hidden.")

(defvar-local mevedel-view--abort-function nil
  "Optional data-buffer function used to abort work without root lifecycle.")

(defvar-local mevedel-view--aborted-p nil
  "Non-nil once this data buffer's active work has been aborted.

Killing either half of a view pair aborts the data buffer, and the first
kill then kills its partner, whose own hook aborts the same data buffer
again.  Aborting settles the session, so a second pass repeats a whole
durable publication for state that cannot have changed in between.")


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

(defface mevedel-view-guest-header
  '((t :inherit (bold font-lock-constant-face) :overline t :extend t))
  "Face for collaboration guest message headers in the view buffer.
A guest turn is someone else\='s input arriving in your session, so it
reads as neither your own prompt nor the assistant."
  :group 'mevedel)

(defface mevedel-view-assistant-header
  '((t :inherit (bold font-lock-function-name-face) :overline t :extend t))
  "Face for assistant message headers in the view buffer."
  :group 'mevedel)

(defface mevedel-view-muted
  '((t :inherit shadow))
  "Quiet but readable: folded content the user may still want to skim.
The foreground is blended from the active theme by
`mevedel--derive-theme-faces'; `shadow' is only the fallback for a
display whose colors cannot be measured.  A separate tier is needed
because `shadow' is the theme's chrome colour and lands around 2.4:1 on
a dark theme, under the 3:1 floor, while every other option is a full
accent."
  :group 'mevedel)

(mevedel--derive-theme-face
 'mevedel-view-muted
 (lambda (foreground background)
   (list :foreground (mevedel--muted-color foreground background))))

(defface mevedel-view-tool-summary
  '((t :inherit default))
  "Face for collapsed tool call summaries."
  :group 'mevedel)

(defface mevedel-view-tool-marker
  '((t :inherit success))
  "Face for successful tool summary markers."
  :group 'mevedel)

(defface mevedel-view-tool-name
  '((t :inherit font-lock-keyword-face))
  "Face for tool names in collapsed summaries."
  :group 'mevedel)

(defface mevedel-view-tool-argument
  '((t :inherit default))
  "Face for primary tool arguments in collapsed summaries.
Plain rather than string-coloured: an argument is usually a path, and
string green already carries the success marker and the added-line count
on the same row."
  :group 'mevedel)

(defface mevedel-view-tool-metadata
  '((t :inherit shadow))
  "Face for line counts and secondary metadata in summaries."
  :group 'mevedel)

(defface mevedel-view-tool-diff-added
  '((t :inherit success :weight bold))
  "Face for added-line counts in patch summaries."
  :group 'mevedel)

(defface mevedel-view-tool-diff-removed
  '((t :inherit error :weight bold))
  "Face for removed-line counts in patch summaries."
  :group 'mevedel)

(defface mevedel-view-tool-warning
  '((t :inherit warning :weight bold))
  "Face for blocked or warning tool summary markers."
  :group 'mevedel)

(defface mevedel-view-hook-context
  '((t :inherit (mevedel-view-muted italic)))
  "Face for hook context indicators in user turns."
  :group 'mevedel)

(defface mevedel-view-hook-audit
  '((t :inherit (mevedel-view-muted italic)))
  "Face for hook audit indicators in transcript turns."
  :group 'mevedel)

(defface mevedel-view-thinking-summary
  '((t :inherit (mevedel-view-muted italic)))
  "Face for collapsed thinking/reasoning summaries."
  :group 'mevedel)

(defface mevedel-view-system-reminder
  '((t :inherit mevedel-view-muted))
  "Face for collapsed system reminder summaries."
  :group 'mevedel)

(defface mevedel-view-thinking-marker
  '((t :inherit (mevedel-view-muted italic)))
  "Face for thinking/reasoning summary markers."
  :group 'mevedel)

(defface mevedel-view-response-summary
  '((t :inherit mevedel-view-muted))
  "Face for collapsed response summaries."
  :group 'mevedel)

(defface mevedel-view-response-marker
  '((t :inherit font-lock-function-name-face))
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

(defface mevedel-view-permission-mode-ask
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face for the ask permission mode prompt label."
  :group 'mevedel)

(defface mevedel-view-permission-mode-edits
  '((t :inherit success :weight bold))
  "Face for the edits permission mode prompt label."
  :group 'mevedel)

(defface mevedel-view-permission-mode-full-auto
  '((t :inherit error :weight bold))
  "Face for the full-auto permission mode prompt label."
  :group 'mevedel)

(defface mevedel-view-plan-mode
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face for the active Plan workflow label."
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
  '((t :inherit mevedel-view-muted :underline t))
  "Face for the `from <type>--<idshort>' fragment.
Click target on handles, mailbox blocks, plan summaries, and
permission prompts.  A muted underline rather than a full link: in
`:inherit (link shadow)' the earlier entry won every attribute, so the
`shadow' never rendered and the fragment read as loud as a real link."
  :group 'mevedel)

(defface mevedel-view-mailbox-header
  '((t :inherit font-lock-keyword-face))
  "Face for delivered message and completion headers."
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
  "Face for active running agent handle rows."
  :group 'mevedel)

(defface mevedel-view-handle-blocked
  '((t :inherit warning :weight bold))
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

(defvar-local mevedel-view--status-marker nil
  "Marker separating the history region from the status zone.
Insertion-type t so history-content insertion advances it; status-zone
content renders here as read-only text.")

(defvar-local mevedel-view--interaction-marker nil
  "Marker separating the status zone from the interaction zone.
Insertion-type t so status content above advances it; interaction-zone
overlays anchor here.  Permission queue head, plan confirmation, and
preview overlays render against this marker.")

(defvar-local mevedel-view--status-strip-cache-key nil
  "Semantic state used to build the cached header-line status strip.")

(defvar-local mevedel-view--status-strip-cache-value nil
  "Cached header-line status strip for the current view buffer.")

(defvar-local mevedel-view--continuation-prompt-cache nil
  "Cached (SESSION SEGMENT SUMMARY-P PROMPT) for the live segment.")

(defconst mevedel-view--status-task-collapse-key '(status tasks)
  "Stable fragment collapse key for the task status block.")


(defun mevedel-view--set-spinner-option (symbol value)
  "Set spinner option SYMBOL to VALUE and refresh active views."
  (pcase symbol
    ('mevedel-view-spinner-framerate
     (unless (and (integerp value) (<= 1 value 60))
       (user-error "Spinner frame rate must be an integer from 1 to 60")))
    ('mevedel-view-spinner-battery-framerate
     (unless (and (integerp value) (<= 0 value 60))
       (user-error "Battery frame rate must be an integer from 0 to 60"))))
  (set-default symbol value)
  (when (fboundp 'mevedel-view--refresh-animation-options)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (derived-mode-p 'mevedel-view-mode)
          (mevedel-view--refresh-animation-options))))))

(defcustom mevedel-view-spinner-animate t
  "Non-nil means animate view buffer progress and pending-tool indicators."
  :type 'boolean
  :set #'mevedel-view--set-spinner-option
  :group 'mevedel)

(defcustom mevedel-view-spinner-style 'shimmer
  "Animation style for the foreground request-progress label."
  :type '(choice (const shimmer) (const breathe) (const bounce)
                 (const dots) (const ellipsis) (const braille)
                 (const ascii) (const static))
  :set #'mevedel-view--set-spinner-option
  :group 'mevedel)

(defcustom mevedel-view-tool-spinner-style 'braille
  "Compact animation style for pending-tool rows."
  :type '(choice (const braille) (const ascii) (const dots) (const static))
  :set #'mevedel-view--set-spinner-option
  :group 'mevedel)

(defcustom mevedel-view-spinner-framerate 60
  "Maximum graphical progress frames per second on external power."
  :type '(integer 1 60)
  :set #'mevedel-view--set-spinner-option
  :group 'mevedel)

(defcustom mevedel-view-spinner-battery-framerate 30
  "Maximum animation frames per second when saving power.
Zero freezes decorative animation but leaves progress metadata current."
  :type '(integer 0 60)
  :set #'mevedel-view--set-spinner-option
  :group 'mevedel)

(defcustom mevedel-view-spinner-power-policy 'auto
  "How animation responds to the Emacs host's power source.
`auto' uses battery status, treating unknown as battery; `full' uses
the normal frame rate; `save' always applies the battery ceiling."
  :type '(choice (const auto) (const full) (const save))
  :set #'mevedel-view--set-spinner-option
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

(defvar-keymap mevedel-surface-mode-map
  :doc "Keymap inherited by editable mevedel surfaces."
  :parent text-mode-map)

(defvar-keymap mevedel-view-mode-map
  :doc "Keymap for `mevedel-view-mode'."
  :parent mevedel-surface-mode-map
  "C-g" #'mevedel-view-abort
  "C-c C-k" #'mevedel-view-abort
  "C-c C-o" #'mevedel-menu
  "C-c C-q" #'mevedel-pending-inputs-clear)

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
   mevedel-tool-task-status-keymap))

(defun mevedel-surface--enforce-ephemeral (&rest _)
  "Keep the current mevedel surface out of Emacs save machinery."
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

(define-derived-mode mevedel-surface-mode text-mode "MevSurface"
  "Base mode for ephemeral mevedel surfaces with editable regions."
  (visual-line-mode +1)
  (setq-local window-point-insertion-type t)
  (auto-save-mode -1)
  (mevedel-surface--enforce-ephemeral)
  (add-hook 'after-change-functions
            #'mevedel-surface--enforce-ephemeral nil t)
  (add-hook 'post-command-hook
            #'mevedel-surface--enforce-ephemeral nil t))

(define-derived-mode mevedel-view-mode mevedel-surface-mode "MevView"
  "Major mode for the mevedel chat view buffer.

Displays a compact rendering of the gptel data buffer.  Interactive view
buffers are ordered as history region, status zone, interaction zone,
request progress row, and input zone.  The input zone starts at
`mevedel-view--input-marker' with a read-only prompt prefix followed by
the editable composer body.

\\{mevedel-view-mode-map}"
  ;; View projection uses several independent symbolic invisibility categories.
  ;; Keep the wildcard form so adding or removing one category cannot expose
  ;; unrelated collapsed content.
  (setq-local buffer-invisibility-spec t)
  ;; Copying a rendered table yields its canonical pipe Markdown.
  (setq-local filter-buffer-substring-function
              #'mevedel-view--buffer-substring-filter)
  ;; Reflow rendered tables and ratio-sized images when a window
  ;; showing this view changes size or first displays it.
  (mevedel-view--enable-markdown-realign)
  ;; A render deferred while nobody watched runs once a window shows
  ;; the view again.  Also release animation observation when its last
  ;; window stops showing it.  Buffer-local, so the hook dies with the buffer.
  (add-hook 'window-buffer-change-functions
            #'mevedel-view--resume-on-window-change nil t)
  (add-hook 'window-scroll-functions
            #'mevedel-view--resume-on-window-scroll nil t))


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
existing `mevedel--view-buffer' binding untouched.  A
`:side-conversation-p' view omits durable input history, and
`:transcript-start' hides inherited model context from projection."
  (require 'mevedel-models)
  (require 'mevedel-tools)
  (require 'mevedel-view-composer)
  (require 'mevedel-view-agent)
  (require 'mevedel-view-history)
  (require 'mevedel-view-interaction)
  (require 'mevedel-view-render)
  (with-current-buffer view-buf
    (when (overlayp mevedel-view--composer-keymap-overlay)
      (delete-overlay mevedel-view--composer-keymap-overlay))
    (mevedel-view-mode)
    (mevedel-surface--enforce-ephemeral)
    (setq-local mevedel-view--side-conversation-p
                (plist-get options :side-conversation-p)
                mevedel-view--transcript-start
                (plist-get options :transcript-start))
    (setq-local mevedel--data-buffer data-buf)
    (setq-local mevedel--session
                (and (buffer-live-p data-buf)
                     (buffer-local-value 'mevedel--session data-buf)))
    (mevedel-view-agent-initialize options data-buf)
    (mevedel-view-render-initialize)
    (mevedel-view-interaction-initialize)
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
    ;; A buffer repurposed in another mode must stop decorative wakeups too.
    (add-hook 'change-major-mode-hook #'mevedel-view--stop-spinner-timer nil t)
    (unless mevedel-view--agent-transcript-p
      (add-hook 'kill-buffer-query-functions
                #'mevedel-view--allow-session-close-p nil t))
    (unless mevedel-view--side-conversation-p
      (add-hook 'kill-buffer-hook #'mevedel-view-history-save nil t))
    ;; A transcript has no line numbers worth counting, and they cost four
    ;; columns of a directive frame that is already narrow.
    (setq-local display-line-numbers nil)
    (unless mevedel-view--agent-transcript-p
      (setq header-line-format '(:eval (mevedel-view--sticky-prompt-line)))
      (setq-local tab-line-format
                  '(:eval (mevedel-view--status-strip)))))
  (unless (plist-get options :preserve-data-view-buffer)
    (with-current-buffer data-buf
      (setq-local mevedel--view-buffer view-buf)
      (use-local-map
       (copy-keymap (or (current-local-map) (make-sparse-keymap))))
      (local-set-key (kbd "C-c C-o") #'mevedel-menu)
      ;; Kill-buffer lifecycle: data killed -> kill view buffer
      (unless (plist-get options :agent-transcript-p)
        (add-hook 'kill-buffer-query-functions
                  #'mevedel-view--allow-session-close-p nil t))
      (add-hook 'kill-buffer-hook
                (if (plist-get options :agent-transcript-p)
                    #'mevedel-view--on-agent-transcript-data-killed
                  #'mevedel-view--on-data-killed)
                nil t))))

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
             (derived-name
              (string-trim-left
               (if (string-match "\\*$" data-name)
                   (replace-match ":view*" t t data-name)
                 (concat data-name ":view"))))
             (view-name (or view-name derived-name))
             (existing (get-buffer view-name))
             (view-buf (or existing (get-buffer-create view-name)))
             setup-complete-p)
        (unwind-protect
            (progn
              (mevedel-view--setup view-buf data-buf options)
              (setq setup-complete-p t)
              view-buf)
          (when (and (not setup-complete-p)
                     (not existing)
                     (buffer-live-p view-buf))
            (with-current-buffer view-buf
              (setq mevedel--data-buffer nil)
              (let ((kill-buffer-query-functions nil))
                (ignore-errors (kill-buffer view-buf))))
            (when (buffer-live-p view-buf)
              (with-current-buffer view-buf
                (let ((kill-buffer-hook nil)
                      (kill-buffer-query-functions nil))
                  (ignore-errors (kill-buffer view-buf))))))))))


;;
;;; Lifecycle

(defun mevedel-view--allow-session-close-p ()
  "Return non-nil when the current session may be closed safely."
  (cond
   ((and mevedel--session
         (mevedel-session-pending-publication mevedel--session))
    (message
     (concat
      "mevedel: session publication is pending; run "
      "mevedel-session-publication-retry or "
      "mevedel-session-publication-abandon first"))
    nil)
   ((and mevedel--session
         (not
          (eq 'foreign
              (plist-get (mevedel-session-lease mevedel--session)
                         :state)))
         (mevedel-execution-unsettled-mutation-p mevedel--session))
    (message
     (concat
      "mevedel: remote mutation is unsettled; stop live executions or run "
      "mevedel-retry-target-readiness to acknowledge it first"))
    nil)
   (t t)))

(defun mevedel-view--abort-data-buffer (data-buffer)
  "Abort active work owned by DATA-BUFFER."
  (when (buffer-live-p data-buffer)
    (condition-case err
        (with-current-buffer data-buffer
          (if mevedel-view--abort-function
              (funcall mevedel-view--abort-function data-buffer)
            (when (and mevedel--session (fboundp 'mevedel-abort))
              (mevedel-abort data-buffer))))
      (error
       (display-warning
        'mevedel
        (format "Could not abort session during buffer cleanup: %S" err)
        :warning)))))

(defun mevedel-view--abort-data-buffer-for-kill (data-buffer)
  "Abort DATA-BUFFER's active work once for the whole teardown of its pair.

Killing either half of a view pair kills the other, and both kill hooks
reach this data buffer.  Aborting settles the session, so a second pass
would repeat a full durable publication for state that cannot have changed
in between.  The latch is set before the abort runs, so a partner hook
performs no second abort even when the first one signals."
  (when (and (buffer-live-p data-buffer)
             (not (buffer-local-value 'mevedel-view--aborted-p data-buffer)))
    (with-current-buffer data-buffer
      (setq mevedel-view--aborted-p t))
    (mevedel-view--abort-data-buffer data-buffer)))

(defun mevedel-view--on-view-killed ()
  "Hook run when the view buffer is killed.
Clears `mevedel--view-buffer' on the associated data buffer and kills
it.  The reference is cleared before killing so the data buffer's own
kill hook sees nil and exits without re-entering this function."
  ;; Per-view timers must die with the view on every kill path, the
  ;; agent-transcript branch and the hookless fallback's survivor
  ;; included: a live timer holding a killed buffer is exactly the
  ;; leaked state the test isolation rule forbids, and outside tests it
  ;; is a needless wakeup.
  (when (fboundp 'mevedel-view-path-teardown)
    (mevedel-view-path-teardown))
  (mevedel-view--stop-spinner-timer)
  (mevedel-view--cancel-scheduled-render)
  (mevedel-view-render-invalidate-live-tail)
  (mevedel-view-control-transfer-stop-polling)
  (unless (mevedel-view-agent-handle-view-kill)
    (let ((view-buffer (current-buffer)))
      ;; Both pair-kill orders pass here before root registration is cleared.
      ;; The later data-buffer release hook can no longer identify that root.
      (when (buffer-live-p mevedel--data-buffer)
        (with-current-buffer mevedel--data-buffer
          (when mevedel--session
            (mevedel-journal-capture-seal-and-schedule
             mevedel--session (current-buffer) 'session-end))))
      (mevedel-view-control-transfer-teardown)
      (mevedel-view--interaction-clear)
      (when-let* ((db mevedel--data-buffer)
                  (_ (buffer-live-p db)))
        (mevedel-view--abort-data-buffer-for-kill db)
        (mevedel-view-agent-cleanup-parent view-buffer)
        (with-current-buffer db
          (when (fboundp 'mevedel-permission-queue-abort-all)
            (mevedel-permission-queue-abort-all mevedel--session))
          (when (fboundp 'mevedel-plan-approval-abort)
            (mevedel-plan-approval-abort mevedel--session))
          (setq mevedel--view-buffer nil))
        (kill-buffer db)))))

(defun mevedel-view--on-data-killed ()
  "Hook run when the data buffer is killed.
Kills the associated view buffer."
  (mevedel-view--abort-data-buffer-for-kill (current-buffer))
  (when (and mevedel--session
             (fboundp 'mevedel-execution-teardown-session))
    (mevedel-execution-teardown-session mevedel--session))
  (when mevedel--session
    (mevedel-agent-control-teardown-session mevedel--session))
  (when (fboundp 'mevedel-permission-queue-abort-all)
    (mevedel-permission-queue-abort-all mevedel--session))
  (when (fboundp 'mevedel-plan-approval-abort)
    (mevedel-plan-approval-abort mevedel--session))
  (when-let* ((vb mevedel--view-buffer)
              (_ (buffer-live-p vb)))
    (kill-buffer vb)))

(defun mevedel-view--status-strip-button (label area help)
  "Return clickable status strip LABEL for cockpit AREA with HELP."
  (let* ((map (make-sparse-keymap))
         (command (lambda (&optional _event)
                    (interactive "e")
                    (mevedel-menu-open area))))
    (define-key map [tab-line mouse-1] command)
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
  "Return a spacer that right-aligns RHS in the tab line."
  (propertize
   " " 'display
   (if (and (fboundp 'string-pixel-width)
            (display-graphic-p))
       `(space :align-to (- right (,(string-pixel-width rhs))))
     `(space :align-to (- right ,(string-width rhs))))))

(defun mevedel-view--archived-prompt (session segment entry)
  "Return the source-backed ENTRY in SESSION's archived SEGMENT.
An unreadable or stale segment cannot supply a clickable prompt."
  (let ((archive (condition-case nil
                     (mevedel-session-artifacts-read-segment session segment)
                   (error nil)))
        result)
    (when archive
      (unwind-protect
          (with-current-buffer archive
            (when-let* ((position (plist-get entry :pos))
                        ((integerp position))
                        (source-prompt
                         (cl-find position
                                  (mevedel-session-artifacts-collect-prompts
                                   archive)
                                  :key (lambda (prompt)
                                         (plist-get prompt :pos))))
                        ((equal (plist-get entry :preview)
                                (plist-get source-prompt :preview))))
              (let* ((source (cl-find-if
                              (lambda (span)
                                (and (eq (car span) 'user)
                                     (<= position (caddr span))))
                              (mevedel-transcript-segments
                               (point-min) (point-max))))
                     (text (and source
                                (mevedel-view--user-turn-text
                                 (list source) archive)))
                     (preview (and text
                                   (mevedel-view--prompt-preview text nil))))
                (when preview
                  (setq result (list :segment segment :pos position
                                     :preview preview))))))
        (kill-buffer archive)))
    result))

(defun mevedel-view--refresh-continuation-prompt (data-buffer historical-p)
  "Cache the prompt governing DATA-BUFFER's compacted live segment.
Resolve archives only on source projection, never during header redisplay."
  (unless historical-p
    (let* ((session (and (buffer-live-p data-buffer)
                         (buffer-local-value 'mevedel--session data-buffer)))
           (segment (and session (mevedel-session-current-segment session)))
           (summary-p (and session (with-current-buffer data-buffer
                                     (mevedel-session-artifacts-segment-summary-bounds))))
           (tail-count (and session (with-current-buffer data-buffer
                                      (mevedel-session-artifacts--segment-tail-prompt-count))))
           (cached mevedel-view--continuation-prompt-cache))
      (unless (and (eq session (nth 0 cached))
                   (eql segment (nth 1 cached))
                   (eq (and summary-p t) (nth 2 cached))
                   (eql tail-count (nth 3 cached))
                   cached)
        (let ((prompt
               (when (and summary-p (integerp segment) (> segment 1))
                 ;; Copied tail prompts are indexed in earlier segments.  The
                 ;; summary precedes them, so its governing prompt is the
                 ;; indexed entry immediately before that copied suffix.
                 (let ((skip tail-count))
                   (catch 'found
                     (cl-loop for previous downfrom (1- segment) to 1 do
                            (let ((indexed
                                   (cdr (assoc previous
                                               (mevedel-session-prompt-index
                                                session)))))
                              (when indexed
                                (if (>= skip (length indexed))
                                    (setq skip (- skip (length indexed)))
                                  ;; An indexed but invalid origin is not a
                                  ;; reason to jump past it to an older prompt.
                                  (throw 'found
                                         (mevedel-view--archived-prompt
                                          session previous
                                          (nth (- (length indexed) skip 1)
                                               indexed))))))
                            ;; A fresh segment has no summary.  Do not cross
                            ;; /clear even after later compactions.
                            (let ((archive
                                   (condition-case nil
                                       (mevedel-session-artifacts-read-segment
                                        session previous)
                                     (error nil))))
                              (unless archive (throw 'found nil))
                              (unwind-protect
                                  (unless (with-current-buffer archive
                                            (mevedel-session-artifacts-segment-summary-bounds))
                                    (throw 'found nil))
                                (kill-buffer archive)))))))))
          (setq mevedel-view--continuation-prompt-cache
                (list session segment (and summary-p t) tail-count prompt)))))))

(defun mevedel-view--continuation-prompt ()
  "Return this live view's resolved archived prompt, if any."
  (unless (mevedel-view-historical-segment-p)
    (nth 4 mevedel-view--continuation-prompt-cache)))

(defun mevedel-view--pinned-prompt (window)
  "Return (POSITION . PREVIEW) for the prompt above WINDOW's top edge.
Metadata lives on rendered prompt headers, not in the model transcript."
  (when (and (window-live-p window)
             (eq (window-buffer window) (current-buffer))
             (not (get-text-property
                   (window-start window) 'mevedel-view-prompt-preview)))
    (let ((top (min (window-start window)
                    (or (and (markerp mevedel-view--input-marker)
                             (marker-position mevedel-view--input-marker))
                        (point-max))))
          (pos (min (1+ (window-start window)) (point-max)))
          found)
      (while (and (not found) (> pos (point-min)))
        (let ((edge (previous-single-property-change
                     pos 'mevedel-view-prompt-preview nil (point-min))))
          (if (not edge)
              (setq pos (point-min))
            (let ((preview (get-text-property
                            (max (point-min) (1- edge))
                            'mevedel-view-prompt-preview)))
              (if (and preview (<= edge top))
                  (setq found
                        (cons (or (previous-single-property-change
                                   edge 'mevedel-view-prompt-preview nil
                                   (point-min))
                                  (point-min))
                              preview))
                (setq pos edge))))))
      found)))

(defun mevedel-view--jump-to-pinned-prompt (position &optional event)
  "Reveal the pinned prompt at POSITION in the clicked window from EVENT."
  (interactive)
  (let ((window (if event (posn-window (event-start event))
                  (selected-window))))
    (when (window-live-p window)
      (with-selected-window window
        (when (derived-mode-p 'mevedel-view-mode)
          (goto-char position)
          (when (get-text-property (point) 'mevedel-view-stash)
            (mevedel-view--expand-turn)
            (goto-char position))
          (forward-line 1)
          (when (eq (get-text-property (point) 'mevedel-view-type)
                    'user-input-summary)
            (when (get-text-property (point) 'mevedel-view-collapsed)
              (mevedel-view-render-toggle-user-input)))
          (goto-char position)
          (recenter 0))))))

(defun mevedel-view--pinned-prompt-button (preview position width &optional archive)
  "Return a clickable PREVIEW at POSITION fitted to WIDTH columns.
When ARCHIVE is non-nil, POSITION is a source position in that segment."
  (if (<= width 0)
      ""
    (let ((map (make-sparse-keymap)))
      (define-key map [header-line mouse-1]
                  (lambda (event)
                    (interactive "e")
                    (if archive
                        (mevedel-view-segments-jump-to-prompt
                         archive position (posn-window (event-start event)))
                      (mevedel-view--jump-to-pinned-prompt position event))))
      (propertize
       (replace-regexp-in-string
        "%" "%%"
        (truncate-string-to-width preview width 0 nil "…") t t)
       'face 'link 'mouse-face 'highlight
       'help-echo "Jump to this prompt"
       'local-map map))))

(defun mevedel-view--sticky-prompt-line ()
  "Return the current window's pinned prompt on its own header line."
  (let* ((window (selected-window))
         (local (mevedel-view--pinned-prompt window))
         (archived (and (not local)
                        (not (get-text-property
                              (window-start window)
                              'mevedel-view-prompt-preview))
                        (let ((next (next-single-property-change
                                     (window-start window)
                                     'mevedel-view-prompt-preview nil
                                     (mevedel-view--input-marker-position))))
                          (not (and next
                                    (get-text-property
                                     next 'mevedel-view-prompt-preview)
                                    (< next
                                       (save-excursion
                                         (goto-char (window-start window))
                                         (vertical-motion
                                          (window-body-height window) window)
                                         (point)))
                                    (not (invisible-p next)))))
                        (mevedel-view--continuation-prompt)))
         (pinned (or local archived)))
    (when pinned
      (let* ((width (max 0 (1- (window-body-width window))))
             (label (if (> width 8) "Prompt  " "")))
        (concat (propertize label 'face 'shadow)
                (mevedel-view--pinned-prompt-button
                 (if local (cdr pinned) (plist-get pinned :preview))
                 (if local (car pinned) (plist-get pinned :pos))
                 (- width (string-width label))
                 (and archived (plist-get pinned :segment))))))))

(defun mevedel-view--status-strip ()
  "Return a mevedel-owned clickable status strip for the view buffer."
  (when (and (boundp 'mevedel--data-buffer)
             (buffer-live-p mevedel--data-buffer))
    (let* ((data-buffer mevedel--data-buffer)
           (session (with-current-buffer data-buffer
                      (and (boundp 'mevedel--session) mevedel--session)))
           (workspace (and session (mevedel-session-workspace session)))
           (target (and session
                        (mevedel-session-execution-target session)))
           (target-label
            (and target (mevedel-execution-target-label target)))
           (durability
            (and session workspace
                 (mevedel-session-publication-status session)))
           (pending-publication
            (plist-get durability :pending-publication))
           (lease-state (plist-get durability :lease-state))
           (session-name (or (and session (mevedel-session-name session))
                             "unknown"))
           (root (abbreviate-file-name
                  (file-name-as-directory
                   (or (and target
                            (mevedel-execution-target-native-root target))
                       (and workspace (mevedel-workspace-root workspace))
                       (with-current-buffer data-buffer default-directory)))))
           (permission-label
            (car (mevedel-view--permission-mode-display
                  (mevedel-view--effective-permission-mode))))
           (mode (if (and session (mevedel-session-plan-mode session))
                     (format "Plan/%s" permission-label)
                   permission-label))
           (scope (mevedel-view-composer-scope-label))
           (state (mevedel-request-state-label data-buffer))
           (model-label (mevedel-model-current-label data-buffer))
           (model (if (string= model-label "none")
                      "model none"
                    model-label))
           (goal (and session (mevedel-session-goal session)))
           (phase-model
            (if goal
                (format "%s · %d turns · %s"
                        (mevedel-goal-status goal)
                        (mevedel-goal-turns-run goal)
                        model)
              model))
           (preset-name (and session (mevedel-session-preset-name session)))
           (tool-count (mevedel-tools-active-count data-buffer))
           (tools (format "%d tool%s"
                          tool-count
                          (if (= tool-count 1) "" "s")))
           (width (mevedel-view--status-strip-width))
           (cache-key
            (list data-buffer session-name root target-label
                  pending-publication lease-state mode scope state phase-model
                  (and goal t) preset-name tools width (display-graphic-p)
                  (selected-window))))
      (if (equal cache-key mevedel-view--status-strip-cache-key)
          mevedel-view--status-strip-cache-value
        (let* ((rhs
                (mapconcat
                 #'identity
                 (delq nil
                       (list
                        (and target-label
                             (mevedel-view--status-strip-button
                              target-label 'top "Open session cockpit"))
                        (and pending-publication
                             (propertize "publication pending" 'face 'error))
                        (and (memq lease-state
                                   '(foreign expired lost contested))
                             (propertize (format "lease %s" lease-state)
                                         'face 'warning))
                        (mevedel-view--status-strip-button
                         mode 'mode "Open mode cockpit")
                        (and scope
                             (propertize scope
                                         'face 'mevedel-view-directive-scope))
                        (propertize
                         state 'face (if (string= state "running")
                                         'success
                                       'shadow))
                        (mevedel-view--status-strip-button
                         phase-model (if goal 'goal 'model)
                         (if goal "Open Goal cockpit" "Open model cockpit"))
                        (and preset-name
                             (mevedel-view--status-strip-button
                              (format "preset %s" preset-name)
                              'preset "Open Preset cockpit"))
                        (mevedel-view--status-strip-button
                         tools 'tools "Open tools cockpit")))
                 " · "))
               (session-max
                (max 0 (min (string-width session-name)
                            (- width (string-width rhs) 3))))
               (session-label
                (if (zerop session-max) ""
                  (truncate-string-to-width session-name session-max
                                            0 nil "…")))
               (root-max
                (- width
                   (string-width session-label)
                   (string-width rhs)
                   3))
               (root-label
                (mevedel-view--status-strip-root-label root root-max))
               (lhs
                (if (string-empty-p root-label)
                    session-label
                  (format "%s  %s" session-label root-label)))
               (value
                (concat
                 (mevedel-view--status-strip-button
                  lhs 'top "Open session cockpit")
                 (mevedel-view--status-strip-spacer rhs)
                 rhs)))
          (setq mevedel-view--status-strip-cache-key cache-key
                mevedel-view--status-strip-cache-value value)
          value)))))


(defun mevedel-view--tool-status-string (tool-name args)
  "Build a short status string for TOOL-NAME with ARGS."
  (if (equal tool-name "WaitAgent")
      "Waiting for agents"
    (let ((primary-arg (mevedel-tool-display-string tool-name args)))
      (if primary-arg
          (format "Calling %s: %s..." tool-name primary-arg)
        (format "Calling %s..." tool-name)))))


;;
;;; Rerender coordination


(defvar-local mevedel-view--render-timer nil
  "Timer for the next coalesced transcript render.")

(defvar-local mevedel-view--pending-render-kind nil
  "Pending render kind: `tools', `incremental', or `full'.")

(defvar-local mevedel-view--pending-tool-rows nil
  "Tool-use IDs whose unattended progress has not reached the view.")

(defvar-local mevedel-view--pending-render-data-buffer nil
  "Authoritative data buffer for the pending transcript render.")

(defcustom mevedel-view-rerender-debounce 0.15
  "Seconds to wait before a queued full transcript refresh.
Requests arriving while any transcript refresh is pending join that
refresh; a full request upgrades a pending incremental refresh."
  :type 'number
  :group 'mevedel)

(defun mevedel-view--cancel-scheduled-render ()
  "Cancel the current view buffer's pending transcript render."
  (when (timerp mevedel-view--render-timer)
    (cancel-timer mevedel-view--render-timer))
  (setq mevedel-view--render-timer nil
        mevedel-view--pending-render-kind nil
        mevedel-view--pending-tool-rows nil
        mevedel-view--pending-render-data-buffer nil))

(defun mevedel-view--unattended-p (&optional buffer)
  "Return non-nil when nobody can be watching BUFFER's view.

BUFFER defaults to the current buffer.  A view is unattended when every
window showing it sits on a frame that is invisible or iconified, or on a
graphical frame without input focus; a child frame reports the focus of
its top-level ancestor.  A view with no window, or one on a terminal
frame, counts as attended: focus is unknowable there, and batch tests
run their views without windows.  An unattended session otherwise paid a
quarter of its CPU redisplaying spinner frames and live rows nobody saw."
  (let ((windows (get-buffer-window-list (or buffer (current-buffer)) nil t)))
    (and windows
         (cl-every
          (lambda (window)
            (let* ((frame (window-frame window))
                   (top frame))
              (while (frame-parent top)
                (setq top (frame-parent top)))
              (or (not (eq (frame-visible-p frame) t))
                  (and (display-graphic-p top)
                       (null (frame-focus-state top))))))
          windows))))

(defun mevedel-view--flush-scheduled-render (view-buffer &optional synchronous)
  "Run VIEW-BUFFER's pending transcript render once.
SYNCHRONOUS finishes a full projection before returning; ordinary timer work
batches settled history around its readers.
An unattended view keeps its pending kind instead: the focus and
redisplay hooks reschedule it once someone can see the result."
  (when (buffer-live-p view-buffer)
    (with-current-buffer view-buffer
      (let ((mevedel-view-prepare-enabled (not synchronous))
            (kind mevedel-view--pending-render-kind)
            (tool-rows mevedel-view--pending-tool-rows)
            (data-buffer mevedel-view--pending-render-data-buffer))
        (cond
         ((mevedel-view--unattended-p)
          (setq mevedel-view--render-timer nil))
         ;; Rendering reads target files, so it waits for an idle transport.
         ;; It must not test `tramp-current-connection': that is the last
         ;; connection timestamp, which stays set for the life of the process
         ;; once any remote file has been touched, so testing it postponed
         ;; every render on a remote workspace forever.
         ((mevedel-transport-busy-p
           (and (buffer-live-p data-buffer)
                (buffer-local-value 'default-directory data-buffer)))
          (setq mevedel-view--render-timer
                (run-at-time
                 (max 0.1 mevedel-view-rerender-debounce) nil
                 #'mevedel-view--flush-scheduled-render view-buffer)))
         (t
          (setq mevedel-view--render-timer nil
                mevedel-view--pending-render-kind nil
                mevedel-view--pending-tool-rows nil
                mevedel-view--pending-render-data-buffer nil)
          (condition-case err
              (mevedel--with-gc-batched
                (if (mevedel-view-historical-segment-p)
                    (progn
                      (mevedel-view--render-status data-buffer)
                      (mevedel-view--interaction-rebuild)
                      (mevedel-view--ensure-request-progress data-buffer))
                  (pcase kind
                    ('full (if synchronous
                               (mevedel-view--full-rerender)
                             (mevedel-view-render-batched-full)))
                    ('incremental
                     (when (buffer-live-p data-buffer)
                       (mevedel-view--render-stream-update data-buffer))))
                  ;; A full projection already consumes every progress
                  ;; entry.  Otherwise refresh only the rows that changed,
                  ;; including background executions before the live tail.
                  (when (and (not (eq kind 'full))
                             (buffer-live-p data-buffer))
                    (dolist (id tool-rows)
                      (unless (mevedel-view--refresh-tool-row data-buffer id)
                        (mevedel-view-stream--schedule-execution-row-recovery
                         data-buffer))))))
            (error
             (message "mevedel: view refresh failed: %s"
                      (error-message-string err))))
          (mevedel-view--sanitize-undo)))))))

(defun mevedel-view--resume-render-if-attended (view-buffer)
  "Reschedule VIEW-BUFFER's pending render once someone can see it again."
  (when (buffer-live-p view-buffer)
    (with-current-buffer view-buffer
      (when (and (derived-mode-p 'mevedel-view-mode)
                 (fboundp 'mevedel-view--start-spinner-timer))
        (mevedel-view--start-spinner-timer t))
      (when (and (derived-mode-p 'mevedel-view-mode)
                 (not (mevedel-view--unattended-p)))
        (if mevedel-view--pending-render-kind
            (when (and (buffer-live-p mevedel-view--pending-render-data-buffer)
                       (not (mevedel--timer-pending-p mevedel-view--render-timer)))
              (mevedel-view--schedule-render
               mevedel-view--pending-render-kind
               mevedel-view--pending-render-data-buffer
               mevedel-view-rerender-debounce))
          (mevedel-view-render-resume-batch))
        (mevedel-view-prepare-resume)))))

(defun mevedel-view--resume-on-window-scroll (window _start)
  "Update animation scheduling after scrolling WINDOW."
  (when (and (eq (window-buffer window) (current-buffer))
             (fboundp 'mevedel-view--start-spinner-timer))
    (mevedel-view--start-spinner-timer t)))

(defun mevedel-view--resume-attended-views (&rest _)
  "Resume the pending render of every view that became attended.
Runs after every frame focus change; the predicate filters focus-out."
  ;; ponytail: focus and redisplay hooks only; add
  ;; `window-state-change-functions' if a deiconified but unfocused frame
  ;; is observed to keep a stale view.
  (dolist (buffer (buffer-list))
    (mevedel-view--resume-render-if-attended buffer)))

(defun mevedel-view--resume-on-window-change (window)
  "Update the current view when WINDOW starts or stops showing it.
The buffer-local hook also runs with a departing view current.  Only a
newly displayed view resumes pending rendering; the departing view must
still release animation and power observers when it becomes invisible."
  (if (eq (window-buffer window) (current-buffer))
      (mevedel-view--resume-render-if-attended (current-buffer))
    (when (fboundp 'mevedel-view--start-spinner-timer)
      (mevedel-view--start-spinner-timer t))))

(defun mevedel-view--schedule-render (kind data-buffer delay)
  "Coalesce a KIND render of DATA-BUFFER after DELAY seconds.
`full' supersedes `incremental', which supersedes `tools' row updates.
Once scheduled, later requests join
the same refresh instead of creating independent stream, tool, and full
render timers.  A non-positive DELAY flushes at once, which still defers
while the view is unattended."
  (unless (memq kind '(tools incremental full))
    (error "Unknown render kind: %S" kind))
  (when (buffer-live-p data-buffer)
    (setq mevedel-view--pending-render-data-buffer data-buffer)
    (when (or (eq kind 'full)
              (and (eq kind 'incremental)
                   (eq mevedel-view--pending-render-kind 'tools))
              (null mevedel-view--pending-render-kind))
      (setq mevedel-view--pending-render-kind kind))
    (if (and (numberp delay) (> delay 0))
        ;; Test the timer's presence on `timer-list', not the variable: a
        ;; timer armed from a stream or tool hook while TRAMP had timers
        ;; suspended is silently discarded, and trusting the stale object
        ;; would wedge every future render of this view.
        (unless (mevedel--timer-pending-p mevedel-view--render-timer)
          (let ((view-buffer (current-buffer)))
            (setq mevedel-view--render-timer
                  (run-at-time
                   delay nil #'mevedel-view--flush-scheduled-render
                   view-buffer))))
      (when (timerp mevedel-view--render-timer)
        (cancel-timer mevedel-view--render-timer))
      (mevedel-view--flush-scheduled-render (current-buffer) t))))

(defun mevedel-view-rerender (&optional buffer)
  "Schedule a coalesced full re-render of BUFFER.
Default to the current buffer.  Full, stream, and tool-boundary requests
share one timer, and a full request upgrades an already pending
incremental refresh."
  (let ((view-buffer (or buffer (current-buffer))))
    (when (buffer-live-p view-buffer)
      (with-current-buffer view-buffer
        (when (and (boundp 'mevedel--data-buffer)
                   (buffer-live-p mevedel--data-buffer))
          (mevedel-view--schedule-render
           'full mevedel--data-buffer mevedel-view-rerender-debounce))))))


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
  (when (mevedel-view--event-position event)
    (mouse-set-point event))
  (let* ((pos (point))
         (activate (get-text-property pos 'mevedel-view-zone-activate)))
    (cond
     ((mevedel-view--position-in-input-region-p pos)
      (user-error "No actionable fragment at point"))
     ((get-text-property pos 'mevedel-view-agent-path)
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
  (not (mevedel-view-zone-collapse-state
        mevedel-view--status-task-collapse-key t)))

(defun mevedel-view--status-task-body (session show-completed)
  "Return propertized status-zone task text for SESSION and SHOW-COMPLETED."
  (let ((body (mevedel-tool-task-display-string session show-completed)))
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
                             (mevedel-tool-task-session-has-active-p
                              session)))
         (task-body (and task-active-p
                         (mevedel-view--status-task-body
                          session show-completed))))
    (list :session session
          :show-completed show-completed
          :task-active-p task-active-p
          :task-body task-body)))

(defun mevedel-view--status-fragments (model)
  "Return status fragments for MODEL."
  (let (fragments)
    (when-let* ((session (plist-get model :session))
                (issues (mevedel-session-recovery-issues session)))
      (push (list :namespace 'status :id 'recovery :priority 120
                  :body (mapconcat (lambda (issue) (plist-get issue :message)) issues "\n"))
            fragments))
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
    (when-let* ((session (plist-get model :session))
                ;; The count is O(1); this zone renders on every live update.
                ((> (mevedel-execution-count-user session) 0))
                (body (mevedel-view--status-executions-body session)))
      (progn
        (push (list :namespace 'status
                    :id 'executions
                    :priority 50
                    :body body
                    ;; Without this the row sits flush against the
                    ;; agents separator below it and reads as that
                    ;; rule's caption.  A blank line cannot go in
                    ;; `:body' -- the zone trims it to one newline.
                    :body-suffix "\n"
                    :keymap (mevedel-view--display-fragment-keymap)
                    :navigatable t
                    :activate #'mevedel-view-open-executions
                    :entry 'executions)
              fragments)))
    (when-let* ((fragment (mevedel-view-agent-status-fragment)))
      (push fragment fragments))
    (nreverse fragments)))

(defvar mevedel-view--status-executions-visible-max 5
  "Most background executions listed by name in the status zone.")

(defun mevedel-view--status-execution-line (snapshot)
  "Return the status line for live execution SNAPSHOT.
It names the command and owner without output: the original Bash row owns
progress, and RET jumps there."
  (let* ((id (plist-get snapshot :execution-id))
         (owner (plist-get snapshot :owner))
         (command (replace-regexp-in-string
                   "[\n\r\t]+" " " (or (plist-get snapshot :command) "Bash")))
         (line (concat
                "  "
                (propertize "●" 'font-lock-face 'mevedel-view-agent-running)
                " "
                (truncate-string-to-width command 72 nil nil "…")
                (if (equal owner "/root") "" (format " · %s" owner))
                "\n")))
    (add-text-properties
     2 (1- (length line))
     (list 'mouse-face 'highlight
           'help-echo "RET: show the running command's Bash row"
           'mevedel-view-zone-activate
           (lambda () (mevedel-view-show-live-execution id owner)))
     line)
    line))

(defun mevedel-view--status-executions-body (session)
  "Return status lines for SESSION's background executions, or nil.
Foreground commands already show as the pending call they belong to."
  (when-let* ((live (cl-remove-if-not
                     (lambda (snapshot) (plist-get snapshot :yielded))
                     (mevedel-execution-list-user session))))
    (let ((hidden (- (length live) mevedel-view--status-executions-visible-max)))
      (concat
       (mapconcat #'mevedel-view--status-execution-line
                  (seq-take live mevedel-view--status-executions-visible-max)
                  "")
       (when (> hidden 0)
         (propertize (format "  +%d more\n" hidden)
                     'font-lock-face 'shadow))))))

(defun mevedel-view-show-live-execution (execution-id owner)
  "Show the Bash row of live EXECUTION-ID owned by OWNER."
  (if (equal owner "/root")
      (mevedel-view-audit-show-control-result execution-id)
    (mevedel-view-open-agent-transcript owner)
    (with-current-buffer (window-buffer (selected-window))
      (mevedel-view-audit-show-control-result execution-id))))

(defun mevedel-view-open-executions ()
  "Open the current session's live execution cockpit."
  (interactive)
  (mevedel-executions-list-open))

(defun mevedel-view--execution-state-changed (session data-buffer)
  "Refresh main views owned by SESSION after live executions change."
  (when (buffer-live-p data-buffer)
    (let* ((invocation
            (buffer-local-value 'mevedel--agent-invocation data-buffer))
           (main-data-buffer
            (if invocation
                (mevedel-agent-invocation-parent-data-buffer invocation)
              data-buffer))
           (view-buffer
            (and (buffer-live-p main-data-buffer)
                 (buffer-local-value 'mevedel--view-buffer
                                     main-data-buffer))))
      (when (buffer-live-p view-buffer)
        (with-current-buffer view-buffer
          (when (and (derived-mode-p 'mevedel-view-mode)
                     (not mevedel-view--agent-transcript-p)
                     (eq session (mevedel-view--status-session)))
            (mevedel-view--render-status)))))))

(add-hook 'mevedel-execution-state-change-hook
          #'mevedel-view--execution-state-changed)

(defun mevedel-view--render-status (&optional data-buf)
  "Render task, execution, and aggregate agent status for DATA-BUF."
  (unless mevedel-view--agent-transcript-p
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

(provide 'mevedel-view)

;;; mevedel-view.el ends here
