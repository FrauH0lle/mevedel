;;; niwa-view.el -- Compact view buffer for chat sessions -*- lexical-binding: t -*-

;;; Commentary:

;; Defines the shared ephemeral surface mode and coordinates the user-facing
;; chat view, session lifecycle, and managed zones.  `niwa-view-composer'
;; owns editable input and submission;
;; `niwa-view-prepare'
(declare-function niwa-view-prepare-resume "niwa-view-prepare" ())
(defvar niwa-view-prepare-enabled)

;; `niwa-view-render' owns the transcript projection.  The gptel data
;; buffer remains the authoritative conversation.
;;
;; Architecture:
;;   data buffer (org-mode, gptel) <--- authoritative
;;     |
;;     +---> view buffer (niwa-view-mode) <--- user-facing
;;
;; The view buffer is ephemeral and always reconstructable from the
;; data buffer.

;;; Code:

(require 'button)
(require 'cl-lib)
(require 'niwa-execution)
(require 'niwa-present)
(require 'niwa-theme-faces)
(require 'niwa-tool-task)
(require 'niwa-transcript)
(require 'niwa-transport)
(require 'niwa-utilities)
(require 'niwa-view-animation)
(require 'niwa-view-execution)
(require 'niwa-view-zone)

;; `adaptive-wrap'
(declare-function adaptive-wrap-prefix-mode "ext:adaptive-wrap" (&optional arg))
(defvar adaptive-wrap-prefix-mode)

;; `browse-url'
(declare-function browse-url "browse-url" (url &optional new-window))

;; `gptel-request'
(declare-function gptel--model-name "ext:gptel-request" (model))

;; `niwa-agent-control'
(declare-function niwa-agent-control-teardown-session
                  "niwa-agent-control" (session))
(autoload 'niwa-agent-control-teardown-session "niwa-agent-control")

;; `niwa-chat'
(declare-function niwa-abort "niwa-chat" (&optional buf))

;; `niwa-compact'
(defvar niwa-compact-auto)
(defvar niwa--compact-auto-disabled)

;; `niwa-compact-run'
(defvar niwa-compact-run-failure-count)

;; `niwa-engine'
(declare-function niwa-engine-external-p "niwa-engine" (backend))

;; `niwa-execution'
(declare-function niwa-execution-teardown-session
                  "niwa-execution" (session))
(declare-function niwa-execution-unsettled-mutation-p
                  "niwa-execution" (session))

;; `niwa-execution-target'
(declare-function niwa-execution-target-label
                  "niwa-execution-target" (target &optional directory))
(declare-function niwa-execution-target-native-root
                  "niwa-execution-target" (cl-x) t)

;; `niwa-journal-capture'
(declare-function niwa-journal-capture-seal-and-schedule "niwa-journal-capture"
                  (session buffer trigger &optional captures))
(autoload 'niwa-journal-capture-seal-and-schedule "niwa-journal-capture")

;; `niwa-menu'
(declare-function niwa-menu "niwa-menu" ())
(declare-function niwa-menu-open "niwa-menu" (area))

;; `niwa-implementation-policy'
(declare-function niwa-implementation-policy-current
                  "niwa-implementation-policy" (&optional session buffer))
(autoload 'niwa-implementation-policy-current "niwa-implementation-policy")

;; `niwa-pending-inputs'
(declare-function niwa-pending-inputs-clear
                  "niwa-pending-inputs" ())

;; `niwa-permission-queue'
(declare-function niwa-permission-queue-abort-all
                  "niwa-permission-queue" (&optional session))

;; `niwa-plan-mode'
(declare-function niwa-plan-approval-abort
                  "niwa-plan-mode" (&optional session outcome))

;; `niwa-recovery'
(declare-function niwa-recovery-dismiss "niwa-recovery" (session issue))
(autoload 'niwa-recovery-dismiss "niwa-recovery")

;; `niwa-session-artifacts'
(declare-function niwa-session-artifacts--segment-tail-prompt-count
                  "niwa-session-artifacts" ())
(declare-function niwa-session-artifacts-collect-prompts
                  "niwa-session-artifacts" (buffer &optional preview-length))
(declare-function niwa-session-artifacts-read-segment
                  "niwa-session-artifacts" (session number))
(declare-function niwa-session-artifacts-segment-summary-bounds
                  "niwa-session-artifacts" ())
(autoload 'niwa-session-artifacts--segment-tail-prompt-count
  "niwa-session-artifacts")
(autoload 'niwa-session-artifacts-collect-prompts "niwa-session-artifacts")
(autoload 'niwa-session-artifacts-read-segment "niwa-session-artifacts")
(autoload 'niwa-session-artifacts-segment-summary-bounds
  "niwa-session-artifacts")

;; `niwa-structs'
(declare-function niwa-session-current-segment "niwa-structs" (cl-x) t)
(declare-function niwa-session-collection-cancel "niwa-session-collection" (session))
(declare-function niwa-goal-status "niwa-structs" (cl-x) t)
(declare-function niwa-session-execution-target
                  "niwa-structs" (cl-x) t)
(declare-function niwa-session-goal "niwa-structs" (cl-x) t)
(declare-function niwa-session-lease "niwa-structs" (cl-x) t)
(declare-function niwa-session-name "niwa-structs" (cl-x) t)
(declare-function niwa-session-pending-publication
                  "niwa-structs" (cl-x) t)
(declare-function niwa-session-plan-mode "niwa-structs" (cl-x) t)
(declare-function niwa-session-preset-name "niwa-structs" (cl-x) t)
(declare-function niwa-session-prompt-index "niwa-structs" (cl-x) t)
(declare-function niwa-session-root-buffer "niwa-structs" (cl-x) t)
(declare-function niwa-session-workspace "niwa-structs" (cl-x) t)
(declare-function niwa-workspace-name "niwa-structs" (cl-x) t)
(declare-function niwa-workspace-root "niwa-structs" (cl-x) t)
(defvar niwa--data-buffer)
(defvar niwa--session)
(defvar niwa--view-buffer)

;; `niwa-tool-registry'
(declare-function niwa-tool-display-string "niwa-tool-registry" (tool-name args &optional scope))

;; `niwa-tool-task'
(declare-function niwa-tool-task-display-string
                  "niwa-tool-task" (session show-completed))
(declare-function niwa-tool-task-session-has-active-p
                  "niwa-tool-task" (session))
(declare-function niwa-toggle-tasks "niwa-tool-task" ())
(defvar niwa-tool-task-status-keymap)

;; `niwa-tools'
(declare-function niwa-tools-active-count "niwa-tools"
                  (&optional buffer))

;; `niwa-transport'
(declare-function niwa-transport-busy-p
                  "niwa-transport" (&optional path))

;; `niwa-turn'
(declare-function niwa-request-state-label "niwa-turn"
                  (&optional buffer))

;; `niwa-view-agent'
(declare-function niwa-view--on-agent-transcript-data-killed
                  "niwa-view-agent" ())
(declare-function niwa-view-agent-cleanup-parent
                  "niwa-view-agent" (parent-view))
(declare-function niwa-view-agent-handle-view-kill
                  "niwa-view-agent" ())
(declare-function niwa-view-agent-initialize
                  "niwa-view-agent" (options data-buffer))
(declare-function niwa-view-quit "niwa-view-agent" ())
(declare-function niwa-view-close-agent-transcript
                  "niwa-view-agent" ())
(declare-function niwa-view-open-agent-transcript-at-point
                  "niwa-view-agent" (&optional event))

;; `niwa-view-composer'
(declare-function niwa-view--assert-live-tip
                  "niwa-view-composer" (&optional allow-armed-fork))
(declare-function niwa-view--effective-permission-mode
                  "niwa-view-composer" ())
(declare-function niwa-view--permission-mode-display
                  "niwa-view-composer" (mode))
(declare-function niwa-view--plan-mode-p "niwa-view-composer" ())

(declare-function niwa-view-abort "niwa-view-composer" ())
(declare-function niwa-view-composer-buffer "niwa-view-composer" (&optional view))
(declare-function niwa-view-composer-in-draft-p
                  "niwa-view-composer" (position &optional view))
(declare-function niwa-view-composer-create "niwa-view-composer" ())
(declare-function niwa-view-composer-kill "niwa-view-composer" ())
(declare-function niwa-composer-refresh-transcript-position
                  "niwa-view-composer" (transcript start))
(declare-function niwa-composer-invalidate-transcript-end
                  "niwa-view-composer" (window))
(declare-function niwa-view-pair-toggle-focus "niwa-view-pair" ())
(declare-function niwa-view-transcript-window "niwa-view-pair" (&optional window))
(declare-function niwa-view--install-dnd "niwa-view-input-files" ())
(declare-function niwa-view-transcript-tail-initialize
                  "niwa-view-composer" ())
(declare-function niwa-view-transcript-tail "niwa-view-composer" ())
(declare-function niwa-view-transcript-tail-marker "niwa-view-composer" ())

;; `niwa-view-control-transfer'
(declare-function niwa-view-control-transfer-stop-polling
                  "niwa-view-control-transfer" ())
(declare-function niwa-view-control-transfer-teardown
                  "niwa-view-control-transfer" ())
(autoload 'niwa-view-control-transfer-stop-polling
  "niwa-view-control-transfer")
(autoload 'niwa-view-control-transfer-teardown
  "niwa-view-control-transfer")

;; `niwa-view-disclosure'
(declare-function niwa-view-toggle-section "niwa-view-disclosure" ())

;; `niwa-view-history'
(declare-function niwa-view-history-save
                  "niwa-view-history" (&optional view-buffer))

;; `niwa-view-interaction'
(declare-function niwa-view--interaction-clear
                  "niwa-view-interaction" ())
(declare-function niwa-view--interaction-rebuild
                  "niwa-view-interaction" ())
(declare-function niwa-view-interaction-initialize
                  "niwa-view-interaction" ())
(declare-function niwa-view-jump-to-interaction "niwa-view-interaction" ())

;; `niwa-view-markdown'
(declare-function niwa-view--buffer-substring-filter
                  "niwa-view-markdown" (beg end &optional delete))
(declare-function niwa-view--enable-markdown-realign
                  "niwa-view-markdown" ())
(autoload 'niwa-view--buffer-substring-filter "niwa-view-markdown")
(autoload 'niwa-view--enable-markdown-realign "niwa-view-markdown")
(autoload 'niwa-view--normalize-local-file-uri-path
  "niwa-view-markdown")

;; `niwa-view-path'
(declare-function niwa-view-path-teardown "niwa-view-path" ())

;; `niwa-view-render'
(declare-function niwa-view--after-header-position
                  "niwa-view-render" ())
(declare-function niwa-view--expand-turn "niwa-view-render" ())
(declare-function niwa-view--full-rerender
                  "niwa-view-render"
                  (&optional transcript-buffer source-changed-p))
(declare-function niwa-view--history-tail-position
                  "niwa-view-render" ())
(declare-function niwa-view--non-history-view-position-p
                  "niwa-view-render" (pos))
(declare-function niwa-view--prompt-preview
                  "niwa-view-render" (text shared-display))
(declare-function niwa-view--refresh-tool-row
                  "niwa-view-render" (data-buffer tool-use-id))
(declare-function niwa-view-next-display "niwa-view-render" ())
(declare-function niwa-view-previous-display "niwa-view-render" ())
(declare-function niwa-view-render-batched-full "niwa-view-render" ())
(declare-function niwa-view-render-initialize
                  "niwa-view-render" ())
(declare-function niwa-view-render-invalidate-live-tail
                  "niwa-view-render" ())
(declare-function niwa-view-render-mutate
                  "niwa-view-render" (key function &optional replacement cleanup))
(declare-function niwa-view-render-resume-batch "niwa-view-render" ())
(declare-function niwa-view-render-toggle-user-input
                  "niwa-view-render" ())
(declare-function niwa-view-toggle-transcript "niwa-view-render" ())
(declare-function niwa-view--user-turn-text
                  "niwa-view-render" (segments data-buf))
(defvar niwa-view-render--owner)

;; `niwa-view-segments'
(declare-function niwa-view-historical-segment-p
                  "niwa-view-segments" ())
(declare-function niwa-view-segments-jump-to-prompt
                  "niwa-view-segments" (segment source-pos &optional window))

;; `niwa-view-stream'
(declare-function niwa-view--refresh-animation-options
                  "niwa-view-stream" ())
(declare-function niwa-view--ensure-request-progress
                  "niwa-view-stream" (&optional data-buf status))
(declare-function niwa-view--stop-spinner-timer
                  "niwa-view-stream" ())
(declare-function niwa-view--render-stream-update
                  "niwa-view-stream" (data-buf))
(declare-function niwa-view--start-spinner-timer
                  "niwa-view-stream" (&optional resumed))
(declare-function niwa-view--recheck-spinner-after-redisplay "niwa-view-stream" ())
(declare-function niwa-view--render-request-progress "niwa-view-stream" ())
(declare-function niwa-view--spinner-active-p "niwa-view-stream" ())
(declare-function niwa-view-stream--schedule-execution-row-recovery
                  "niwa-view-stream" (data-buffer tool-use-id))

;; `niwa-view-zone'
(declare-function niwa-view-zone-collapse-state
                  "niwa-view-zone" (key &optional default))
(declare-function niwa-view-zone-reconcile
                  "niwa-view-zone" (zone start end fragments))

;; `org'
(declare-function org-mode "ext:org" ())

;; `visual-wrap'
(declare-function visual-wrap-prefix-mode "visual-wrap" (&optional arg))
(defvar visual-wrap-prefix-mode)


;;
;;; Customization

(defcustom niwa-view-inline-image-max-width 300
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
  :group 'niwa)

(defcustom niwa-view-sticky-prompt-max-width 80
  "Maximum width in columns of the sticky prompt preview.
The preview also never exceeds its window.  nil uses the full window
width."
  :type '(choice (const :tag "Window width" nil)
                 (natnum :tag "Columns"))
  :group 'niwa)

(defvar-local niwa-view--side-conversation-p nil
  "Non-nil when this view displays an ephemeral `/btw' conversation.")

(defvar-local niwa-view--transcript-start nil
  "Data-buffer marker before which transcript projection is hidden.")

(defvar-local niwa-view--abort-function nil
  "Optional data-buffer function used to abort work without root lifecycle.")

(defvar-local niwa-view--aborted-p nil
  "Non-nil once this data buffer's active work has been aborted.

Killing either half of a view pair aborts the data buffer, and the first
kill then kills its partner, whose own hook aborts the same data buffer
again.  Aborting settles the session, so a second pass repeats a whole
durable publication for state that cannot have changed in between.")


(defface niwa-view-separator
  '((t :inherit shadow :extend t))
  "Face for separator lines in the view buffer."
  :group 'niwa)

(defface niwa-view-header
  '((t :inherit (bold shadow) :overline t :extend t))
  "Face for the session header at the top of the view buffer."
  :group 'niwa)

(defface niwa-view-user-header
  '((t :inherit bold :overline t :extend t))
  "Face for user message headers in the view buffer."
  :group 'niwa)

(defface niwa-view-guest-header
  '((t :inherit (bold font-lock-constant-face) :overline t :extend t))
  "Face for collaboration guest message headers in the view buffer.
A guest turn is someone else\='s input arriving in your session, so it
reads as neither your own prompt nor the assistant."
  :group 'niwa)

(defface niwa-view-assistant-header
  '((t :inherit (bold font-lock-function-name-face) :overline t :extend t))
  "Face for assistant message headers in the view buffer."
  :group 'niwa)

(defface niwa-view-muted
  '((t :inherit shadow))
  "Quiet but readable: folded content the user may still want to skim.
The foreground is blended from the active theme by
`niwa--derive-theme-faces'; `shadow' is only the fallback for a
display whose colors cannot be measured.  A separate tier is needed
because `shadow' is the theme's chrome colour and lands around 2.4:1 on
a dark theme, under the 3:1 floor, while every other option is a full
accent."
  :group 'niwa)

(niwa--derive-theme-face
 'niwa-view-muted
 (lambda (foreground background)
   (list :foreground (niwa--muted-color foreground background))))

(defface niwa-view-tool-summary
  '((t :inherit default))
  "Face for collapsed tool call summaries."
  :group 'niwa)

(defface niwa-view-tool-marker
  '((t :inherit success))
  "Face for successful tool summary markers."
  :group 'niwa)

(defface niwa-view-tool-name
  '((t :inherit font-lock-keyword-face))
  "Face for tool names in collapsed summaries."
  :group 'niwa)

(defface niwa-view-tool-argument
  '((t :inherit default))
  "Face for primary tool arguments in collapsed summaries.
Plain rather than string-coloured: an argument is usually a path, and
string green already carries the success marker and the added-line count
on the same row."
  :group 'niwa)

(defface niwa-view-tool-metadata
  '((t :inherit shadow))
  "Face for line counts and secondary metadata in summaries."
  :group 'niwa)

(defface niwa-view-tool-diff-added
  '((t :inherit success :weight bold))
  "Face for added-line counts in patch summaries."
  :group 'niwa)

(defface niwa-view-tool-diff-removed
  '((t :inherit error :weight bold))
  "Face for removed-line counts in patch summaries."
  :group 'niwa)

(defface niwa-view-tool-warning
  '((t :inherit warning :weight bold))
  "Face for blocked or warning tool summary markers."
  :group 'niwa)

(defface niwa-view-hook-context
  '((t :inherit (niwa-view-muted italic)))
  "Face for hook context indicators in user turns."
  :group 'niwa)

(defface niwa-view-hook-audit
  '((t :inherit (niwa-view-muted italic)))
  "Face for hook audit indicators in transcript turns."
  :group 'niwa)

(defface niwa-view-thinking-summary
  '((t :inherit (niwa-view-muted italic)))
  "Face for collapsed thinking/reasoning summaries."
  :group 'niwa)

(defface niwa-view-system-reminder
  '((t :inherit niwa-view-muted))
  "Face for collapsed system reminder summaries."
  :group 'niwa)

(defface niwa-view-thinking-marker
  '((t :inherit (niwa-view-muted italic)))
  "Face for thinking/reasoning summary markers."
  :group 'niwa)

(defface niwa-view-response-summary
  '((t :inherit niwa-view-muted))
  "Face for collapsed response summaries."
  :group 'niwa)

(defface niwa-view-response-marker
  '((t :inherit font-lock-function-name-face))
  "Face for collapsed response summary markers."
  :group 'niwa)

(defface niwa-view-source-block
  '((t :inherit org-block :foreground unspecified :extend t))
  "Face for rendered Markdown source block panels."
  :group 'niwa)

(defface niwa-view-source-block-language
  '((t :inherit (italic font-lock-type-face niwa-view-source-block)))
  "Face for rendered Markdown source block language labels."
  :group 'niwa)

(defface niwa-view-spinner
  '((t :inherit (bold font-lock-comment-face)))
  "Face for the spinner status line."
  :group 'niwa)

(defface niwa-view-turn-rule
  '((t :inherit shadow :overline t :extend t))
  "Face for the horizontal rule that closes an assistant turn."
  :group 'niwa)

(defface niwa-view-request-summary-rule
  '((t :inherit shadow :strike-through t :extend t))
  "Face for the rule that continues a closing request summary.
It is applied to the summary line's newline only: `:extend' carries the
strike-through from the end of the label to the window edge, so the
label reads as part of the turn's closing rule."
  :group 'niwa)

(defface niwa-view-activity-rule
  '((t :inherit shadow :overline t :extend t))
  "Face for the quiet rule between assistant activity and the next prose.
The overline is blended from the active theme by
`niwa--derive-theme-faces', about half as strong as `shadow', so the
rule inside a turn stays lighter than the rule that closes it."
  :group 'niwa)

(niwa--derive-theme-face
 'niwa-view-activity-rule
 (lambda (foreground background)
   (list :overline (niwa--tint background foreground 0.15))))

(defface niwa-view-input-prompt
  '((t :inherit shadow :weight bold))
  "Face for the read-only `> ' prompt in the input zone."
  :group 'niwa)

(defface niwa-view-permission-mode-ask
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face for the ask permission mode prompt label."
  :group 'niwa)

(defface niwa-view-permission-mode-edits
  '((t :inherit success :weight bold))
  "Face for the edits permission mode prompt label."
  :group 'niwa)

(defface niwa-view-permission-mode-full-auto
  '((t :inherit error :weight bold))
  "Face for the full-auto permission mode prompt label."
  :group 'niwa)

(defface niwa-view-plan-mode
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face for the active Plan workflow label."
  :group 'niwa)

(defface niwa-view-zone-separator
  '((t :inherit shadow))
  "Face for status / interaction zone separator lines."
  :group 'niwa)

(defface niwa-view-ephemeral
  '((t :inherit shadow))
  "Face for ephemeral progress, such as spinners and running tool markers."
  :group 'niwa)

(defface niwa-view-attribution
  '((t :inherit niwa-view-muted :underline t))
  "Face for the `from <type>--<idshort>' fragment.
Click target on handles, mailbox blocks, plan summaries, and
permission prompts.  A muted underline rather than a full link: in
`:inherit (link shadow)' the earlier entry won every attribute, so the
`shadow' never rendered and the fragment read as loud as a real link."
  :group 'niwa)

(defface niwa-view-mailbox-header
  '((t :inherit font-lock-keyword-face))
  "Face for delivered message and completion headers."
  :group 'niwa)

(defface niwa-view-mailbox-gutter
  '((t :inherit niwa-view-tool-metadata))
  "Face for the gutter prefix on expanded mailbox deliveries."
  :group 'niwa)

(defface niwa-view-mailbox-body
  '((t :inherit default))
  "Face for expanded mailbox delivery body text."
  :group 'niwa)

(defface niwa-view-handle-running
  '((t :inherit bold))
  "Face for the `[running · N calls]' handle badge."
  :group 'niwa)

(defface niwa-view-agent-running
  '((t :inherit (font-lock-escape-face bold)))
  "Face for active running agent handle rows."
  :group 'niwa)

(defface niwa-view-handle-blocked
  '((t :inherit warning :weight bold))
  "Face for the `[blocked · awaiting …]' handle badge."
  :group 'niwa)

(defface niwa-view-handle-done
  '((t :inherit success))
  "Face for the `✓ done · …' handle badge."
  :group 'niwa)

(defface niwa-view-handle-error
  '((t :inherit error))
  "Face for the `✗ error · …' / `✗ aborted' handle badges."
  :group 'niwa)

(defvar-local niwa-view--status-marker nil
  "Marker separating the history region from the status zone.
Insertion-type t so history-content insertion advances it; status-zone
content renders here as read-only text.")

(defvar-local niwa-view--interaction-marker nil
  "Marker separating the status zone from the interaction zone.
Insertion-type t so status content above advances it; interaction-zone
overlays anchor here.  Permission queue head, plan confirmation, and
preview overlays render against this marker.")

(defvar-local niwa-view--status-strip-cache-key nil
  "Semantic state used to build the cached header-line status strip.")

(defvar-local niwa-view--status-strip-cache-value nil
  "Cached header-line status strip for the current view buffer.")

(defvar-local niwa-view--continuation-prompt-cache nil
  "Cached (SESSION SEGMENT SUMMARY-P PROMPT) for the live segment.")

(defconst niwa-view--status-task-collapse-key '(status tasks)
  "Stable fragment collapse key for the task status block.")


(defun niwa-view--set-spinner-option (symbol value)
  "Set spinner option SYMBOL to VALUE and refresh active views."
  (pcase symbol
    ('niwa-view-spinner-framerate
     (unless (and (integerp value) (<= 1 value 60))
       (niwa-diagnostic-user-error 'global "Spinner frame rate must be an integer from 1 to 60")))
    ('niwa-view-spinner-battery-framerate
     (unless (and (integerp value) (<= 0 value 60))
       (niwa-diagnostic-user-error 'global "Battery frame rate must be an integer from 0 to 60"))))
  (set-default symbol value)
  (when (fboundp 'niwa-view--refresh-animation-options)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (derived-mode-p 'niwa-view-mode)
          (niwa-view--refresh-animation-options))))))

(defcustom niwa-view-spinner-animate t
  "Non-nil means animate view buffer progress and pending-tool indicators."
  :type 'boolean
  :set #'niwa-view--set-spinner-option
  :group 'niwa)

(defcustom niwa-view-spinner-style 'shimmer
  "Animation style for the foreground request-progress label."
  :type '(choice (const shimmer) (const breathe) (const bounce)
                 (const dots) (const ellipsis) (const braille)
                 (const ascii) (const static))
  :set #'niwa-view--set-spinner-option
  :group 'niwa)

(defcustom niwa-view-tool-spinner-style 'shimmer
  "Animation style for running tool rows.
Shimmer sweeps a running call's tool name, or a running group's
`N running' count, in step with the request label's shimmer, so both
move in the same frames; the arguments stay still.  The glyph styles
animate the row's marker instead."
  :type '(choice (const shimmer) (const braille) (const ascii) (const dots)
                 (const static))
  :set #'niwa-view--set-spinner-option
  :group 'niwa)

(defcustom niwa-view-spinner-framerate 30
  "Maximum graphical progress frames per second on external power.
Ordinary text frames require editor redisplay; supported native surfaces
present independently.  Shimmer draws during its one-second sweep every
four seconds.  Text breathe/bounce keep 8 fps unless PGTK presents only
what changed (`niwa-present-enabled')."
  :type '(integer 1 60)
  :set #'niwa-view--set-spinner-option
  :group 'niwa)

(defcustom niwa-view-spinner-battery-framerate 15
  "Maximum animation frames per second when saving power.
Zero freezes decorative animation but leaves progress metadata current."
  :type '(integer 0 60)
  :set #'niwa-view--set-spinner-option
  :group 'niwa)

(defcustom niwa-view-spinner-power-policy 'auto
  "How animation responds to the Emacs host's power source.
`auto' uses battery status, treating unknown as battery; `full' uses
the normal frame rate; `save' always applies the battery ceiling."
  :type '(choice (const auto) (const full) (const save))
  :set #'niwa-view--set-spinner-option
  :group 'niwa)


;;
;;; Major mode

(defvar-keymap niwa-view--display-map
  :doc "Keymap active in the read-only history/status/interaction area.
Applied via the `keymap' text property so these bindings only fire
above the prompt boundary."
  "TAB" #'niwa-view-toggle-section
  "RET" #'niwa-view-activate-at-point
  "<mouse-1>" #'niwa-view-activate-at-point
  "<mouse-2>" #'niwa-view-activate-at-point
  "n" #'niwa-view-next-display
  "p" #'niwa-view-previous-display
  "t" #'niwa-view-toggle-transcript
  "q" #'niwa-view-quit)

(defvar-keymap niwa-surface-mode-map
  :doc "Keymap inherited by editable niwa surfaces."
  :parent text-mode-map)

(defvar-keymap niwa-view-mode-map
  :doc "Keymap for `niwa-view-mode'."
  :parent niwa-surface-mode-map
  "C-g" #'niwa-view-abort
  "C-c C-k" #'niwa-view-abort
  "C-c C-j" #'niwa-view-jump-to-interaction
  "C-c C-o" #'niwa-menu
  "C-c C-SPC" #'niwa-view-pair-toggle-focus
  "C-c C-q" #'niwa-pending-inputs-clear)

(defun niwa-view--display-fragment-keymap (&rest maps)
  "Return a composed display-fragment keymap from MAPS.
MAPS take precedence, with `niwa-view--display-map' providing shared
navigation and activation fallbacks."
  (make-composed-keymap
   (delq nil (append maps (list niwa-view--display-map)))))

(defun niwa-view--status-task-keymap ()
  "Return the `view-buffer' keymap for the task status fragment."
  (niwa-view--display-fragment-keymap
   (define-keymap
     "<tab>" #'niwa-view-toggle-section
     "TAB" #'niwa-view-toggle-section
     "<return>" #'niwa-view-activate-at-point
     "RET" #'niwa-view-activate-at-point)
   niwa-tool-task-status-keymap))

(defun niwa-surface--enforce-ephemeral (&rest _)
  "Keep the current niwa surface out of Emacs save machinery."
  (setq buffer-file-name nil
        buffer-file-truename nil
        buffer-file-number nil)
  (setq-local buffer-offer-save nil)
  (setq-local buffer-auto-save-file-name nil)
  (setq-local buffer-save-without-query nil)
  (setq-local auto-save-default nil)
  (setq-local make-backup-files nil)
  (setq-local create-lockfiles nil)
  ;; A no-op reset forces redisplay and misplaces point on wrapped hidden markup.
  (when (buffer-modified-p)
    (set-buffer-modified-p nil)))

(define-derived-mode niwa-surface-mode text-mode "MevSurface"
  "Base mode for ephemeral niwa surfaces with editable regions."
  (visual-line-mode +1)
  (setq-local window-point-insertion-type t)
  (auto-save-mode -1)
  (niwa-surface--enforce-ephemeral)
  (add-hook 'after-change-functions
            #'niwa-surface--enforce-ephemeral nil t)
  (add-hook 'post-command-hook
            #'niwa-surface--enforce-ephemeral nil t))

(define-derived-mode niwa-view-mode niwa-surface-mode "MevView"
  "Major mode for the niwa chat view buffer.

Displays a compact rendering of the gptel data buffer.  Interactive view
buffers are ordered as history region, status zone, interaction zone,
and request progress row.  An editable view owns a separate composer buffer.

\\{niwa-view-mode-map}"
  :after-hook
  (progn
    ;; Text-mode hooks can enable these after the mode body.  The renderer
    ;; owns continuation prefixes; automatic filling would overwrite them.
    (when (bound-and-true-p adaptive-wrap-prefix-mode)
      (adaptive-wrap-prefix-mode -1))
    (when (bound-and-true-p visual-wrap-prefix-mode)
      (visual-wrap-prefix-mode -1)))
  ;; Stock PGTK presents the whole frame for each redisplay.
  (niwa-present-ensure (selected-frame))
  ;; View projection uses several independent symbolic invisibility categories.
  ;; Keep the wildcard form so adding or removing one category cannot expose
  ;; unrelated collapsed content.
  (setq-local buffer-invisibility-spec t)
  ;; Copying a rendered table yields its canonical pipe Markdown.
  (setq-local filter-buffer-substring-function
              #'niwa-view--buffer-substring-filter)
  ;; Keep the composer where it is: when it reaches the window bottom,
  ;; scroll one line instead of recentering (which jumped about half a
  ;; window when a status row appeared or the draft grew by a line).
  (setq-local scroll-conservatively 101
              scroll-margin 0)
  ;; Reflow rendered tables and ratio-sized images when a window
  ;; showing this view changes size or first displays it.
  (niwa-view--enable-markdown-realign)
  ;; A render deferred while nobody watched runs once a window shows
  ;; the view again.  Also release animation observation when its last
  ;; window stops showing it.  Buffer-local, so the hook dies with the buffer.
  (add-hook 'window-buffer-change-functions
            #'niwa-view--resume-on-window-change nil t)
  (add-hook 'window-scroll-functions
            #'niwa-view--resume-on-window-scroll nil t)
  (add-hook 'window-scroll-functions
            #'niwa-composer-refresh-transcript-position nil t)
  ;; Overlay and font changes need not change the transcript's text tick.
  (add-hook 'pre-redisplay-functions #'niwa-composer-invalidate-transcript-end nil t)
  ;; Keep pixel scrolling a forced redisplay start would drop under the caret.
  (add-hook 'pre-redisplay-functions #'niwa-view--note-redisplay-vscroll nil t)
  (add-hook 'window-scroll-functions #'niwa-view--restore-redisplay-vscroll nil t)
  ;; Resizing (`delete-other-windows', a taller frame) can bring a span
  ;; into view without scrolling, and changes the rail's width.
  (add-hook 'window-size-change-functions
            #'niwa-view--resume-on-window-resize nil t))


;;
;;; Header helper

(defun niwa-view--header-string (data-buf)
  "Return the read-only session-header string for DATA-BUF.
The line shows \"SESSION @ WORKSPACE\" using the `niwa-view-header'
face and carries the display-region text properties so it participates
in read-only enforcement and section navigation."
  (let* ((session (buffer-local-value 'niwa--session data-buf))
         (ws (and session (niwa-session-workspace session)))
         (label (if session
                    (format "%s @ %s"
                            (niwa-session-name session)
                            (or (and ws (niwa-workspace-name ws))
                                "niwa"))
                  "niwa")))
    (propertize (concat label "\n")
                'read-only t
                'keymap niwa-view--display-map
                'front-sticky '(read-only keymap)
                'rear-nonsticky '(read-only keymap)
                'font-lock-face 'niwa-view-header)))

(defun niwa-view--setup (view-buf data-buf &optional options)
  "Initialize VIEW-BUF as the view buffer for DATA-BUF.
Activates `niwa-view-mode', wires the cross-references, and creates
the composer for editable views.

OPTIONS is a plist.  When `:agent-transcript-p' is non-nil, create
a read-only transcript inspection view instead of an interactive chat
view.  When `:preserve-data-view-buffer' is non-nil, leave DATA-BUF's
existing `niwa--view-buffer' binding untouched.  A
`:side-conversation-p' view omits durable input history, and
`:transcript-start' hides inherited model context from projection."
  (require 'niwa-compact)
  (require 'niwa-models)
  (require 'niwa-tools)
  (require 'niwa-view-composer)
  (require 'niwa-view-agent)
  (require 'niwa-view-history)
  (require 'niwa-view-interaction)
  (require 'niwa-view-render)
  (with-current-buffer view-buf
    (niwa-view-composer-kill)
    (niwa-view-mode)
    (buffer-disable-undo)
    (niwa-surface--enforce-ephemeral)
    (setq-local niwa-view--side-conversation-p
                (plist-get options :side-conversation-p)
                niwa-view--transcript-start
                (plist-get options :transcript-start))
    (setq-local niwa--data-buffer data-buf)
    (setq-local niwa--session
                (and (buffer-live-p data-buf)
                     (buffer-local-value 'niwa--session data-buf)))
    (niwa-view-agent-initialize options data-buf)
    (niwa-view-render-initialize)
    (niwa-view-interaction-initialize)
    ;; Copy workspace directory so relative paths resolve correctly
    (setq-local default-directory
                (buffer-local-value 'default-directory data-buf))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (unless niwa-view--agent-transcript-p
        (insert (niwa-view--header-string data-buf)))
      (let ((boundary (niwa-view-transcript-tail-initialize)))
        (setq niwa-view--status-marker (copy-marker boundary t)
              niwa-view--interaction-marker (copy-marker boundary t))))
    (niwa-view-composer-create)
    (unless niwa-view--agent-transcript-p
      (niwa-view--install-dnd))
    (add-hook 'kill-buffer-hook #'niwa-view-composer-kill 90 t)
    (add-hook 'change-major-mode-hook #'niwa-view-composer-kill nil t)
    (niwa-view--ensure-request-progress data-buf)
    ;; Kill-buffer lifecycle: view killed -> clear ref on data buffer
    (add-hook 'kill-buffer-hook #'niwa-view--on-view-killed nil t)
    ;; A buffer repurposed in another mode must stop decorative wakeups too.
    (add-hook 'change-major-mode-hook #'niwa-view--stop-spinner-timer nil t)
    (unless niwa-view--agent-transcript-p
      (add-hook 'kill-buffer-query-functions
                #'niwa-view--allow-session-close-p nil t))
    (unless niwa-view--side-conversation-p
      (add-hook 'kill-buffer-hook #'niwa-view-history-save nil t))
    ;; A transcript has no line numbers worth counting, and they cost four
    ;; columns of a note frame that is already narrow.
    (setq-local display-line-numbers nil)
    ;; Agent transcript views supply their own status strip but share the
    ;; pinned prompt row.
    (setq header-line-format '(:eval (niwa-view--sticky-prompt-line)))
    (unless niwa-view--agent-transcript-p
      (setq-local tab-line-format
                  '(:eval (niwa-view--status-strip)))))
  (unless (plist-get options :preserve-data-view-buffer)
    (with-current-buffer data-buf
      (setq-local niwa--view-buffer view-buf)
      (use-local-map
       (copy-keymap (or (current-local-map) (make-sparse-keymap))))
      (local-set-key (kbd "C-c C-o") #'niwa-menu)
      ;; Kill-buffer lifecycle: data killed -> kill view buffer
      (unless (plist-get options :agent-transcript-p)
        (add-hook 'kill-buffer-query-functions
                  #'niwa-view--allow-session-close-p nil t))
      (add-hook 'kill-buffer-hook
                (if (plist-get options :agent-transcript-p)
                    #'niwa-view--on-agent-transcript-data-killed
                  #'niwa-view--on-data-killed)
                nil t))))

(defun niwa-view--ensure (data-buf &optional view-name options)
  "Return the view buffer for DATA-BUF, creating it if needed.
VIEW-NAME and OPTIONS are forwarded to `niwa-view--setup' when a
new view buffer is created."
  (or (let ((vb (buffer-local-value 'niwa--view-buffer data-buf)))
        (and vb
             (buffer-live-p vb)
             (with-current-buffer vb
               (eq (and niwa-view--agent-transcript-p t)
                   (and (plist-get options :agent-transcript-p) t)))
             vb))
      (let* ((data-name (buffer-name data-buf))
             ;; Derive view buffer name from data buffer name:
             ;; *niwa:main@proj* -> *niwa:main@proj:view*
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
              (niwa-view--setup view-buf data-buf options)
              (setq setup-complete-p t)
              view-buf)
          (when (and (not setup-complete-p)
                     (not existing)
                     (buffer-live-p view-buf))
            (with-current-buffer view-buf
              (setq niwa--data-buffer nil)
              (let ((kill-buffer-query-functions nil))
                (ignore-errors (kill-buffer view-buf))))
            (when (buffer-live-p view-buf)
              (with-current-buffer view-buf
                (let ((kill-buffer-hook nil)
                      (kill-buffer-query-functions nil))
                  (ignore-errors (kill-buffer view-buf))))))))))


;;
;;; Lifecycle

(defvar niwa-view--closing-data nil
  "The data buffer a view's teardown is killing right now, or nil.")

(defun niwa-view--allow-session-close-p ()
  "Return non-nil when the current session may be closed safely.
A data buffer killed by its view's teardown was already approved with the view."
  (cond
   ((eq niwa-view--closing-data (current-buffer)) t)
   ((and niwa--session
         (niwa-session-pending-publication niwa--session))
    (niwa-diagnostic-message
     (niwa-diagnostic-scope niwa--session)
     (concat
      "Session publication is pending; run "
      "niwa-session-publication-retry or "
      "niwa-session-publication-abandon first"))
    nil)
   ((and niwa--session
         (not
          (eq 'foreign
              (plist-get (niwa-session-lease niwa--session)
                         :state)))
         (niwa-execution-unsettled-mutation-p niwa--session))
    (niwa-diagnostic-message
     (niwa-diagnostic-scope niwa--session)
     (concat
      "Remote mutation is unsettled; stop live executions or run "
      "niwa-retry-target-readiness to acknowledge it first"))
    nil)
   (t t)))

(defun niwa-view--abort-data-buffer (data-buffer)
  "Abort active work owned by DATA-BUFFER."
  (when (buffer-live-p data-buffer)
    (let ((scope (niwa-diagnostic-scope data-buffer)))
      (condition-case err
          (with-current-buffer data-buffer
            (if niwa-view--abort-function
                (funcall niwa-view--abort-function data-buffer)
              (when (and niwa--session (fboundp 'niwa-abort))
                (niwa-abort data-buffer))))
        (error
         (niwa-diagnostic-warning
          scope 'niwa
          (format "%s (Could not abort session during buffer cleanup)"
                  (error-message-string err))
          :warning))))))

(defun niwa-view--abort-data-buffer-for-kill (data-buffer)
  "Abort DATA-BUFFER's active work once for the whole teardown of its pair.

Killing either half of a view pair kills the other, and both kill hooks
reach this data buffer.  Aborting settles the session, so a second pass
would repeat a full durable publication for state that cannot have changed
in between.  The latch is set before the abort runs, so a partner hook
performs no second abort even when the first one signals."
  (when (and (buffer-live-p data-buffer)
             (not (buffer-local-value 'niwa-view--aborted-p data-buffer)))
    (with-current-buffer data-buffer
      (setq niwa-view--aborted-p t))
    (niwa-view--abort-data-buffer data-buffer)))

(defun niwa-view--on-view-killed ()
  "Hook run when the view buffer is killed.
Unlink and kill the associated data buffer if it still owns this view."
  ;; Per-view timers must die with the view on every kill path, the
  ;; agent-transcript branch and the hookless fallback's survivor
  ;; included: a live timer holding a killed buffer is exactly the
  ;; leaked state the test isolation rule forbids, and outside tests it
  ;; is a needless wakeup.
  (when (fboundp 'niwa-view-path-teardown)
    (niwa-view-path-teardown))
  (niwa-view--stop-spinner-timer)
  (niwa-view--cancel-scheduled-render)
  (niwa-view-render-invalidate-live-tail)
  (niwa-view-control-transfer-stop-polling)
  (unless (niwa-view-agent-handle-view-kill)
    (let ((view-buffer (current-buffer)))
      ;; Both pair-kill orders pass here before root registration is cleared.
      ;; The later data-buffer release hook can no longer identify that root.
      (when (buffer-live-p niwa--data-buffer)
        (with-current-buffer niwa--data-buffer
          (when niwa--session
            (niwa-journal-capture-seal-and-schedule
             niwa--session (current-buffer) 'session-end))))
      (niwa-view-control-transfer-teardown)
      (niwa-view--interaction-clear)
      (when-let* ((db niwa--data-buffer)
                  (_ (buffer-live-p db)))
        (niwa-view--abort-data-buffer-for-kill db)
        (niwa-view-agent-cleanup-parent view-buffer)
        (when (with-current-buffer db
                (when (fboundp 'niwa-permission-queue-abort-all)
                  (niwa-permission-queue-abort-all niwa--session))
                (when (fboundp 'niwa-plan-approval-abort)
                  (niwa-plan-approval-abort niwa--session))
                (when (eq niwa--view-buffer view-buffer)
                  (setq niwa--view-buffer nil)
                  t))
          (let ((niwa-view--closing-data db))
            (kill-buffer db)))))))

(defun niwa-view--on-data-killed ()
  "Hook run when the data buffer is killed.
Kills the associated view buffer."
  (niwa-view--abort-data-buffer-for-kill (current-buffer))
  (when (and niwa--session
             (fboundp 'niwa-execution-teardown-session))
    (niwa-execution-teardown-session niwa--session))
  (when niwa--session
    (niwa-agent-control-teardown-session niwa--session))
  (when (fboundp 'niwa-permission-queue-abort-all)
    (niwa-permission-queue-abort-all niwa--session))
  (when (fboundp 'niwa-plan-approval-abort)
    (niwa-plan-approval-abort niwa--session))
  ;; Settlement during this hook can schedule collection after its kill hook ran.
  (when (and niwa--session
             (eq (niwa-session-root-buffer niwa--session) (current-buffer))
             (fboundp 'niwa-session-collection-cancel))
    (niwa-session-collection-cancel niwa--session))
  (when-let* ((vb niwa--view-buffer)
              (_ (buffer-live-p vb)))
    ;; Aborting can dirty the transcript; a second kill could prompt to save.
    (setq niwa--view-buffer nil)
    (kill-buffer vb)))

(defun niwa-view-toggle-auto-compaction (&optional event)
  "Toggle automatic compaction for this chat, or the chat clicked in EVENT.
Only future local compaction admission changes; an ongoing compaction is
not cancelled.  Native providers manage their own automatic compaction."
  (interactive (list (when (mouse-event-p last-input-event) last-input-event)))
  (with-current-buffer (if event (window-buffer (posn-window (event-start event)))
                         (current-buffer))
    (niwa-view--assert-live-tip)
    (when niwa-view--side-conversation-p
      (niwa-diagnostic-user-error (niwa-diagnostic-scope (current-buffer)) "Side conversations do not compact"))
    (unless (buffer-live-p niwa--data-buffer)
      (niwa-diagnostic-user-error (niwa-diagnostic-scope (current-buffer)) "No live chat buffer"))
    (with-current-buffer niwa--data-buffer
      (when (niwa-engine-external-p gptel-backend)
        (niwa-diagnostic-user-error (niwa-diagnostic-scope (current-buffer)) "Automatic compaction is managed by the provider"))
      (setq-local niwa-compact-auto
                  (not (and niwa-compact-auto
                            (not niwa--compact-auto-disabled))))
      (when niwa-compact-auto
        (setq niwa--compact-auto-disabled nil
              niwa-compact-run-failure-count 0)))
    (force-mode-line-update)))

(defun niwa-view--status-strip-button (label area help)
  "Return clickable status strip LABEL for cockpit AREA with HELP."
  (let* ((map (make-sparse-keymap))
         (command (lambda (&optional _event)
                    (interactive "e")
                    (niwa-menu-open area))))
    (define-key map [tab-line mouse-1] command)
    (propertize label
                'face 'link
                'mouse-face 'highlight
                'help-echo help
                'local-map map
                'niwa-view-cockpit-area area)))

(defvar-local niwa-view--status-strip-root nil
  "(DIRECTORY . ABBREVIATED) for the status strip's last root.")

(defun niwa-view--status-strip-abbreviated (directory)
  "Return DIRECTORY abbreviated for display, remembering the last answer.
The strip is evaluated on every redisplay, and `abbreviate-file-name'
consults the file name handlers each time."
  (if (equal directory (car niwa-view--status-strip-root))
      (cdr niwa-view--status-strip-root)
    (cdr (setq niwa-view--status-strip-root
               (cons directory
                     (abbreviate-file-name
                      (file-name-as-directory directory)))))))

(defun niwa-view--status-strip-width ()
  "Return display columns available for the status strip."
  (let* ((buffer (current-buffer))
         (selected (selected-window))
         (windows (niwa-view--windows 'geometry buffer))
         (width (cond
                 ((eq (window-buffer selected) buffer)
                  (window-body-width selected))
                 (windows
                  (apply #'min (mapcar #'window-body-width windows)))
                 (t
                  (window-body-width)))))
    (max 20 (1- width))))

(defun niwa-view--status-strip-root-label (root max-width)
  "Return ROOT shortened to fit MAX-WIDTH display columns."
  (cond
   ((<= max-width 0) "")
   ((<= (string-width root) max-width) root)
   (t
    (let* ((base (file-name-nondirectory (directory-file-name root)))
           (tail (concat "…/" base "/")))
      (if (<= (string-width tail) max-width) tail "")))))

(defun niwa-view--status-strip-spacer (rhs)
  "Return a spacer that right-aligns RHS in the tab line.
Fall back to columns when the display refuses pixel measurement during
redisplay, as Neomacs's layout engine does."
  (propertize
   " " 'display
   (if-let* (((fboundp 'string-pixel-width))
             ((display-graphic-p))
             (pixels (ignore-errors (string-pixel-width rhs))))
       `(space :align-to (- right (,pixels)))
     `(space :align-to (- right ,(string-width rhs))))))

(defun niwa-view--archived-prompt (session segment entry)
  "Return the source-backed ENTRY in SESSION's archived SEGMENT.
An unreadable or stale segment cannot supply a clickable prompt."
  (let ((archive (condition-case nil
                     (niwa-session-artifacts-read-segment session segment)
                   (error nil)))
        result)
    (when archive
      (unwind-protect
          (with-current-buffer archive
            (when-let* ((position (plist-get entry :pos))
                        ((integerp position))
                        (source-prompt
                         (cl-find position
                                  (niwa-session-artifacts-collect-prompts
                                   archive)
                                  :key (lambda (prompt)
                                         (plist-get prompt :pos))))
                        ((equal (plist-get entry :preview)
                                (plist-get source-prompt :preview))))
              (let* ((source (cl-find-if
                              (lambda (span)
                                (and (eq (car span) 'user)
                                     (<= position (caddr span))))
                              (niwa-transcript-segments
                               (point-min) (point-max))))
                     (text (and source
                                (niwa-view--user-turn-text
                                 (list source) archive)))
                     (preview (and text
                                   (niwa-view--prompt-preview text nil))))
                (when preview
                  (setq result (list :segment segment :pos position
                                     :preview preview))))))
        (kill-buffer archive)))
    result))

(defun niwa-view--refresh-continuation-prompt (data-buffer historical-p)
  "Cache the prompt governing DATA-BUFFER's compacted live segment.
Resolve archives only on source projection, never during header redisplay."
  (unless historical-p
    (let* ((session (and (buffer-live-p data-buffer)
                         (buffer-local-value 'niwa--session data-buffer)))
           (segment (and session (niwa-session-current-segment session)))
           (summary-p (and session (with-current-buffer data-buffer
                                     (niwa-session-artifacts-segment-summary-bounds))))
           (tail-count (and session (with-current-buffer data-buffer
                                      (niwa-session-artifacts--segment-tail-prompt-count))))
           (cached niwa-view--continuation-prompt-cache))
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
                                               (niwa-session-prompt-index
                                                session)))))
                              (when indexed
                                (if (>= skip (length indexed))
                                    (setq skip (- skip (length indexed)))
                                  ;; An indexed but invalid origin is not a
                                  ;; reason to jump past it to an older prompt.
                                  (throw 'found
                                         (niwa-view--archived-prompt
                                          session previous
                                          (nth (- (length indexed) skip 1)
                                               indexed))))))
                            ;; A fresh segment has no summary.  Do not cross
                            ;; /clear even after later compactions.
                            (let ((archive
                                   (condition-case nil
                                       (niwa-session-artifacts-read-segment
                                        session previous)
                                     (error nil))))
                              (unless archive (throw 'found nil))
                              (unwind-protect
                                  (unless (with-current-buffer archive
                                            (niwa-session-artifacts-segment-summary-bounds))
                                    (throw 'found nil))
                                (kill-buffer archive)))))))))
          (setq niwa-view--continuation-prompt-cache
                (list session segment (and summary-p t) tail-count prompt)))))))

(defun niwa-view--continuation-prompt ()
  "Return this live view's resolved archived prompt, if any."
  (unless (niwa-view-historical-segment-p)
    (nth 4 niwa-view--continuation-prompt-cache)))

(defvar-local niwa-view--prompt-headers nil
  "(CHARS-TICK . RUNS) of validated prompt header marker pairs.")

(defun niwa-view--prompt-header-runs ()
  "Return a vector of (START END PREVIEW) prompt runs with marker bounds.
The renderer clears the cache before inserting a new prompt header.
Markers follow text edits; when text changes, check just the known
headers for deletion or replacement.  Scroll-time fontification only
changes properties and does not invalidate the cache."
  (let ((tick (buffer-chars-modified-tick)))
    (if (and niwa-view--prompt-headers
             (or (eql tick (car niwa-view--prompt-headers))
                 (cl-every (lambda (run) (< (car run) (cadr run)))
                           (cdr niwa-view--prompt-headers))))
        (progn
          (setcar niwa-view--prompt-headers tick)
          (cdr niwa-view--prompt-headers))
      (let ((pos (point-min)) runs)
        (while (< pos (point-max))
          (let ((next (next-single-property-change
                       pos 'niwa-view-prompt-preview nil (point-max)))
                (preview (get-text-property pos 'niwa-view-prompt-preview)))
            (when preview
              (push (list (copy-marker pos t) (copy-marker next) preview) runs))
            (setq pos next)))
        (setq runs (vconcat (nreverse runs))
              niwa-view--prompt-headers (cons tick runs))
        runs))))

(defun niwa-view--window-top (window)
  "Return the first visible position at or after WINDOW's start.
A start inside hidden text displays the next visible text at the top."
  (let ((top (window-start window))
        (limit (or (niwa-view-transcript-tail-marker)
                   (point-max))))
    (while (and (< top limit) (invisible-p top))
      (setq top (next-single-char-property-change top 'invisible nil limit)))
    (min top limit)))

(defun niwa-view--pinned-prompt (window)
  "Return (POSITION . PREVIEW) for the prompt above WINDOW's top edge.
Metadata lives on rendered prompt headers, not in the model transcript.
Hidden headers are never selected."
  (when-let* (((window-live-p window))
              ((eq (window-buffer window) (current-buffer)))
              (top (niwa-view--window-top window))
              ((not (get-text-property top 'niwa-view-prompt-preview))))
    (let* ((runs (niwa-view--prompt-header-runs))
           (lo 0) (hi (length runs)))
      ;; The last header run ending at or before TOP.
      (while (< lo hi)
        (let ((mid (/ (+ lo hi) 2)))
          (if (<= (nth 1 (aref runs mid)) top)
              (setq lo (1+ mid))
            (setq hi mid))))
      ;; Visibility is not part of the per-text-change runs: the filter
      ;; changes it without changing the text.  Usually the nearest header
      ;; is visible, so this checks one position.
      (while (and (> lo 0) (invisible-p (nth 0 (aref runs (1- lo)))))
        (setq lo (1- lo)))
      (when (> lo 0)
        (let ((run (aref runs (1- lo))))
          (cons (marker-position (nth 0 run)) (nth 2 run)))))))

(defun niwa-view--jump-to-pinned-prompt (position &optional event)
  "Reveal the pinned prompt at POSITION in the clicked window from EVENT."
  (interactive)
  (let ((window (if event (posn-window (event-start event))
                  (selected-window))))
    (when (window-live-p window)
      (with-selected-window window
        (when (derived-mode-p 'niwa-view-mode)
          (goto-char position)
          (when (get-text-property (point) 'niwa-view-stash)
            (niwa-view--expand-turn)
            (goto-char position))
          (forward-line 1)
          (when (eq (get-text-property (point) 'niwa-view-type)
                    'user-input-summary)
            (when (get-text-property (point) 'niwa-view-collapsed)
              (niwa-view-render-toggle-user-input)))
          (goto-char position)
          (recenter 0))))))

(defun niwa-view--click-pinned-prompt (event)
  "Reveal the pinned prompt clicked in EVENT.
The clicked header-line text names its target, so one map serves every
preview and the header line allocates no keymap per redisplay."
  (interactive "e")
  (let* ((posn (event-start event))
         (object (posn-string posn))
         (target (and object
                      (get-text-property (cdr object) 'niwa-view-pinned-target
                                         (car object)))))
    (cond
     ((null target))
     ((car target)
      (niwa-view-segments-jump-to-prompt
       (car target) (cdr target) (posn-window posn)))
     (t (niwa-view--jump-to-pinned-prompt (cdr target) event)))))

(defvar niwa-view--pinned-prompt-map
  (let ((map (make-sparse-keymap)))
    (define-key map [header-line mouse-1] #'niwa-view--click-pinned-prompt)
    map)
  "Header-line map shared by every pinned prompt preview.")

(defun niwa-view--pinned-prompt-button (preview position width &optional archive)
  "Return a clickable PREVIEW at POSITION fitted to WIDTH columns.
When ARCHIVE is non-nil, POSITION is a source position in that segment."
  (if (<= width 0)
      ""
    (let ((text (truncate-string-to-width preview width 0 nil "…")))
      (propertize
       (if (string-search "%" text)
           (replace-regexp-in-string "%" "%%" text t t)
         text)
       'face 'link 'mouse-face 'highlight
       'help-echo "Jump to this prompt"
       'niwa-view-pinned-target (cons archive position)
       'local-map niwa-view--pinned-prompt-map))))

(defun niwa-view--prompt-on-screen-p (window)
  "Return non-nil when a prompt header below WINDOW's start is on screen.
Header lines are evaluated on every redisplay, so the answer is kept per
window until its start or scrolling, the buffer, its invisibility or its
text layout changes: size, wrapping, text scale, line spacing or font."
  (let ((key (list (current-buffer)
                   ;; Animation frames rewrite display properties but move
                   ;; nothing a header depends on.
                   (niwa-view--content-tick)
                   buffer-invisibility-spec (window-start window)
                   (window-hscroll window) (window-vscroll window t)
                   (niwa--window-text-layout window)
                   (niwa-view-transcript-tail)))
        (cached (window-parameter window 'niwa-view--prompt-on-screen)))
    (if (equal key (car cached))
        (cdr cached)
      (let* ((next (next-single-property-change
                    (window-start window)
                    'niwa-view-prompt-preview nil
                    (niwa-view-transcript-tail)))
             (visible (and next
                           (get-text-property next 'niwa-view-prompt-preview)
                           (if (display-graphic-p (window-frame window))
                               ;; Pixel-height rows need not match the frame's
                               ;; canonical character height after text scaling.
                               ;; The pixel probe evaluates header-line-format.
                               ;; Keep its reserved row without re-entering us.
                               (let ((header-line-format ""))
                                 (pos-visible-in-window-p next window t))
                             (< next
                                (save-excursion
                                  (goto-char (window-start window))
                                  (vertical-motion
                                   (window-body-height window) window)
                                  (point))))
                           (not (invisible-p next))
                           t)))
        (set-window-parameter window 'niwa-view--prompt-on-screen
                              (cons key visible))
        visible))))

(defun niwa-view--sticky-prompt-line ()
  "Return the current window's pinned prompt on its own header line."
  (let* ((window (selected-window))
         ;; A pixel-position query can format this header while the dock is
         ;; selected, even though the transcript is the current buffer.
         (window (if (eq (window-buffer window) (current-buffer)) window
                   (or (niwa-view-transcript-window window) window)))
         (local (niwa-view--pinned-prompt window))
         (continuation (and (eq (window-buffer window) (current-buffer))
                            (not local) (niwa-view--continuation-prompt)))
         (archived (and continuation
                        (not (get-text-property
                              (window-start window)
                              'niwa-view-prompt-preview))
                        (not (niwa-view--prompt-on-screen-p window))
                        continuation))
         (pinned (or local archived)))
    (when pinned
      (let* ((window-width (max 0 (1- (window-body-width window))))
             (label (if (> window-width 8) "Prompt  " ""))
             (width (if niwa-view-sticky-prompt-max-width
                        (min window-width
                             (+ (string-width label)
                                niwa-view-sticky-prompt-max-width))
                      window-width)))
        (concat (propertize label 'face 'shadow)
                (niwa-view--pinned-prompt-button
                 (if local (cdr pinned) (plist-get pinned :preview))
                 (if local (car pinned) (plist-get pinned :pos))
                 (- width (string-width label))
                 (and archived (plist-get pinned :segment))))))))

(defun niwa-view--status-strip ()
  "Return a niwa-owned clickable status strip for the view buffer.
This runs on every redisplay of the view, so the cache key holds the raw
inputs, read without switching buffers, and labels are formatted only
when one of them changes."
  (when-let* ((data-buffer (bound-and-true-p niwa--data-buffer))
              ((buffer-live-p data-buffer)))
    (let* ((session (buffer-local-value 'niwa--session data-buffer))
           (workspace (and session (niwa-session-workspace session)))
           (target (and session
                        (niwa-session-execution-target session)))
           ;; Read the two facts directly: the full publication status also
           ;; resolves a state directory, through TRAMP on a remote target,
           ;; and this runs on every redisplay of the view.
           (pending-publication
            (and session workspace
                 (niwa-session-pending-publication session)
                 t))
           (lease-state (and session workspace
                             (plist-get (niwa-session-lease session) :state)))
           (session-name (or (and session (niwa-session-name session))
                             "unknown"))
           (directory (or (and target
                               (niwa-execution-target-native-root target))
                          (and workspace (niwa-workspace-root workspace))
                          (buffer-local-value 'default-directory data-buffer)))
           (permission-mode (niwa-view--effective-permission-mode))
           (plan-mode (and session (niwa-session-plan-mode session)))
           (state (niwa-request-state-label data-buffer))
           (request (buffer-local-value 'niwa--current-request data-buffer))
           (model-policy (and request (niwa-request-model-policy request)))
           (implementation (unless model-policy
                             (niwa-implementation-policy-current session data-buffer)))
           (provider (plist-get implementation :model-provider))
           (model (or (plist-get model-policy :model)
                      (and provider (substring provider (1+ (string-search ":" provider))))
                      (buffer-local-value 'gptel-model data-buffer)))
           (goal (and session (niwa-session-goal session)))
           (goal-status (and goal (niwa-goal-status goal)))
           (goal-turns (and goal (niwa-goal-turns-run goal)))
           (preset-name (and session (niwa-session-preset-name session)))
           (compaction
            (and session (not niwa-view--side-conversation-p)
                 (cond
                  ((niwa-engine-external-p
                    (buffer-local-value 'gptel-backend data-buffer)) 'provider)
                  ((and (buffer-local-value 'niwa-compact-auto data-buffer)
                        (not (buffer-local-value
                              'niwa--compact-auto-disabled data-buffer))) 'on)
                  (t 'off))))
           (tool-count (niwa-tools-active-count data-buffer))
           (width (niwa-view--status-strip-width))
           (cache-key
            (vector data-buffer session-name directory
                    (and target (niwa-execution-target-identity target))
                    pending-publication lease-state permission-mode plan-mode
                    state model (and goal t) goal-status goal-turns
                    preset-name compaction tool-count width
                    (display-graphic-p) (selected-window))))
      (if (equal cache-key niwa-view--status-strip-cache-key)
          niwa-view--status-strip-cache-value
        (let* ((target-label
                (and target (niwa-execution-target-label target)))
               (root (niwa-view--status-strip-abbreviated directory))
               (permission-label
                (car (niwa-view--permission-mode-display permission-mode)))
               (mode (if plan-mode
                         (format "Plan/%s" permission-label)
                       permission-label))
               (model (if model (gptel--model-name model) "model none"))
               (tools (format "%d tool%s"
                              tool-count
                              (if (= tool-count 1) "" "s")))
               (rhs
                (mapconcat
                 #'identity
                 (delq nil
                       (list
                        (and target-label
                             (niwa-view--status-strip-button
                              target-label 'top "Open session cockpit"))
                        (and pending-publication
                             (propertize "publication pending" 'face 'error))
                        (and (memq lease-state
                                   '(foreign expired lost contested))
                             (propertize (format "lease %s" lease-state)
                                         'face 'warning))
                        (when compaction
                          (if (eq compaction 'provider)
                              (propertize "compact provider" 'face 'shadow
                                          'help-echo "Automatic compaction is managed by the provider")
                            (let ((map (make-sparse-keymap)))
                              (define-key map [tab-line mouse-1]
                                          #'niwa-view-toggle-auto-compaction)
                              (propertize
                               (format "compact %s" compaction)
                               'face 'link 'mouse-face 'highlight 'local-map map
                               'help-echo "Toggle automatic compaction for this open session"))))
                        (niwa-view--status-strip-button
                         mode 'mode "Open mode cockpit")
                        (propertize
                         state 'face (if (string= state "running")
                                         'success
                                       'shadow))
                        (and goal
                             (niwa-view--status-strip-button
                              (format "Goal %s · %d turns" goal-status goal-turns)
                              'goal "Open Goal cockpit"))
                        (niwa-view--status-strip-button
                         model 'model "Open model cockpit")
                        (and preset-name
                             (niwa-view--status-strip-button
                              (format "preset %s" preset-name)
                              'preset "Open Preset cockpit"))
                        (niwa-view--status-strip-button
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
                (niwa-view--status-strip-root-label root root-max))
               (lhs
                (if (string-empty-p root-label)
                    session-label
                  (format "%s  %s" session-label root-label)))
               (value
                (concat
                 (niwa-view--status-strip-button
                  lhs 'top "Open session cockpit")
                 (niwa-view--status-strip-spacer rhs)
                 rhs)))
          (setq niwa-view--status-strip-cache-key cache-key
                niwa-view--status-strip-cache-value value)
          value)))))


(defun niwa-view--tool-status-string (tool-name args)
  "Build a short status string for TOOL-NAME with ARGS."
  (if (equal tool-name "WaitAgent")
      "Waiting for agents"
    (let ((primary-arg (niwa-tool-display-string
                        tool-name args (niwa-diagnostic-scope niwa--data-buffer))))
      (if primary-arg
          (format "Calling %s: %s..." tool-name primary-arg)
        (format "Calling %s..." tool-name)))))


;;
;;; Rerender coordination


(defvar-local niwa-view--render-timer nil
  "Timer for the next coalesced transcript render.")

(defvar-local niwa-view--pending-render-kind nil
  "Pending render kind: `tools', `incremental', or `full'.")

(defvar-local niwa-view--pending-tool-rows nil
  "Tool-use IDs whose unattended progress has not reached the view.")

(defvar-local niwa-view--pending-render-data-buffer nil
  "Authoritative data buffer for the pending transcript render.")

(defvar-local niwa-view--pending-render-scope 'unavailable
  "Captured workspace scope for the pending transcript render.")

(defcustom niwa-view-rerender-debounce 0.15
  "Seconds to wait before a queued full transcript refresh.
Requests arriving while any transcript refresh is pending join that
refresh; a full request upgrades a pending incremental refresh."
  :type 'number
  :group 'niwa)

(defun niwa-view--cancel-scheduled-render (&optional keep)
  "Cancel the current view buffer's pending transcript render.
KEEP retires only streamed work: a pending full render, such as the
recovery a failed terminal render requests or an agent's refresh, stays
pending, and the tool rows an unattended view retained remain as a
`tools' render of their own.  A settling or stopped turn projects only
its own range, so an older row, such as a background execution's, would
otherwise keep stale progress until something else redrew it."
  (unless (and keep (eq niwa-view--pending-render-kind 'full))
    (let ((rows niwa-view--pending-tool-rows)
          (data-buffer niwa-view--pending-render-data-buffer))
      (when (timerp niwa-view--render-timer)
        (cancel-timer niwa-view--render-timer))
      (setq niwa-view--render-timer nil
            niwa-view--pending-render-kind nil
            niwa-view--pending-tool-rows nil
            niwa-view--pending-render-data-buffer nil)
      (when (and keep rows (buffer-live-p data-buffer))
        (setq niwa-view--pending-tool-rows rows)
        (niwa-view--schedule-render
         'tools data-buffer niwa-view-rerender-debounce)))))

(defun niwa-view--window-attended-p (window)
  "Return non-nil when WINDOW is visible on a focused frame family."
  (let* ((frame (window-frame window))
         (top frame))
    (while (frame-parent top)
      (setq top (frame-parent top)))
    (and (eq (frame-visible-p frame) t)
         (or (not (display-graphic-p top))
             ;; PGTK can focus the child without focusing its parent.
             (frame-focus-state frame)
             (and (not (eq frame top)) (frame-focus-state top))))))

(defun niwa-view--unattended-p (&optional buffer)
  "Return non-nil when nobody can be watching BUFFER's view.

BUFFER defaults to the current buffer.  A view is unattended when no
window shows it, or when every window showing it sits on a frame that is
invisible or iconified, or on a graphical frame without input focus; a
child frame accepts its own focus or its top-level ancestor's.  A view on a
terminal frame counts as attended, since focus is unknowable there, and
so does a view without windows in a batch Emacs, where tests run views
undisplayed.  The window-change hook resumes a view when it reappears.
An unattended session otherwise paid a quarter of its CPU redisplaying
spinner frames and live rows nobody saw."
  (let ((windows (niwa-view--windows 'visibility buffer)))
    (if (null windows)
        (not noninteractive)
      (not (cl-some #'niwa-view--window-attended-p windows)))))

(defun niwa-view--flush-scheduled-render (view-buffer &optional synchronous)
  "Run VIEW-BUFFER's pending transcript render once.
SYNCHRONOUS finishes a full projection before returning; ordinary timer work
batches settled history around its readers.
An unattended view keeps its pending kind instead: the focus and
redisplay hooks reschedule it once someone can see the result."
  (when (buffer-live-p view-buffer)
    (with-current-buffer view-buffer
      (let ((niwa-view-prepare-enabled (not synchronous))
            (kind niwa-view--pending-render-kind)
            (tool-rows niwa-view--pending-tool-rows)
            (data-buffer niwa-view--pending-render-data-buffer)
            (scope niwa-view--pending-render-scope))
        (cond
         ((niwa-view--unattended-p)
          (setq niwa-view--render-timer nil))
         ;; Rendering reads target files, so it waits for an idle transport.
         ;; It must not test `tramp-current-connection': that is the last
         ;; connection timestamp, which stays set for the life of the process
         ;; once any remote file has been touched, so testing it postponed
         ;; every render on a remote workspace forever.
         ((niwa-transport-busy-p
           (and (buffer-live-p data-buffer)
                (buffer-local-value 'default-directory data-buffer)))
          (setq niwa-view--render-timer
                (run-at-time
                 (max 0.1 niwa-view-rerender-debounce) nil
                 #'niwa-view--flush-scheduled-render view-buffer)))
         (t
          (setq niwa-view--render-timer nil
                niwa-view--pending-render-kind nil
                niwa-view--pending-tool-rows nil
                niwa-view--pending-render-data-buffer nil)
          (condition-case err
              (niwa--with-gc-batched
                (if (niwa-view-historical-segment-p)
                    (progn
                      (niwa-view--render-status data-buffer)
                      (niwa-view--interaction-rebuild)
                      (niwa-view--ensure-request-progress data-buffer))
                  (pcase kind
                    ('full (if synchronous
                               (niwa-view--full-rerender)
                             (niwa-view-render-batched-full)))
                    ('incremental
                     (when (buffer-live-p data-buffer)
                       (niwa-view--render-stream-update data-buffer))))
                  ;; A full projection already consumes every progress
                  ;; entry.  Otherwise refresh only the rows that changed,
                  ;; including background executions before the live tail.
                  (when (and (not (eq kind 'full))
                             (buffer-live-p data-buffer))
                    (dolist (id tool-rows)
                      (unless (niwa-view--refresh-tool-row data-buffer id)
                        (niwa-view-stream--schedule-execution-row-recovery
                         data-buffer id))))))
            (error
             (niwa-diagnostic-message
              scope "%s (View refresh failed)" (error-message-string err))))))))))

(defun niwa-view--resume-render-if-attended (view-buffer)
  "Reschedule VIEW-BUFFER's pending render once someone can see it again."
  (when (buffer-live-p view-buffer)
    (with-current-buffer view-buffer
      (when (and (derived-mode-p 'niwa-view-mode)
                 (fboundp 'niwa-view--start-spinner-timer))
        (niwa-view--start-spinner-timer t))
      (when (and (derived-mode-p 'niwa-view-mode)
                 (not (niwa-view--unattended-p)))
        (if niwa-view--pending-render-kind
            (when (and (buffer-live-p niwa-view--pending-render-data-buffer)
                       (not (niwa--timer-pending-p niwa-view--render-timer)))
              ;; Already coalesced while hidden; leave redisplay before writing.
              (setq niwa-view--render-timer
                    (run-at-time 0 nil #'niwa-view--flush-scheduled-render
                                 view-buffer)))
          (niwa-view-render-resume-batch))
        (niwa-view-prepare-resume)))))

(defun niwa-view--resume-on-window-scroll (window &optional _start)
  "Update animation scheduling after WINDOW scrolled.
The hook runs before redisplay, while `window-end' still describes the
old range, so the decision waits for the completed redisplay.

Never change buffer text here: `window-scroll-functions' run inside the
window's redisplay, which afterwards restores point to its earlier
numeric position.  Text inserted or deleted above the composer would
then move the cursor within the draft, or out of it."
  (when (and (eq (window-buffer window) (current-buffer))
             (fboundp 'niwa-view--recheck-spinner-after-redisplay)
             (niwa-view--spinner-active-p))
    (niwa-view--recheck-spinner-after-redisplay)))

(defun niwa-view--note-redisplay-vscroll (window)
  "Remember the start and pixel vscroll WINDOW is about to display.
Writers and commands have finished here, so this is the intended viewport
for `niwa-view--restore-redisplay-vscroll'."
  (set-window-parameter window 'niwa-view--redisplay-viewport
                        (cons (window-start window) (window-vscroll window t))))

(defun niwa-view--restore-redisplay-vscroll (window start)
  "Restore pixel vscroll native redisplay dropped at WINDOW's unchanged START.
Redisplay recomputes and forces a start inside a wrapped line when text
around it changed; unless preservation was requested for that very pass,
it clears vscroll and then moves point off the clipped bottom row.  It runs
these hooks between the reset and that cursor check, so restoring the
viewport noted before redisplay keeps both point and the composer."
  (when-let* ((noted (window-parameter window 'niwa-view--redisplay-viewport))
              ((eql start (car noted)))
              ((> (cdr noted) 0))
              ((zerop (window-vscroll window t))))
    (set-window-vscroll window (cdr noted) t t)))

(defun niwa-view--resume-on-window-resize (window)
  "Refit the rail and prompt, and update animation, after WINDOW resized.
Size changes run before windows are redisplayed, so their text can
change here with the usual composer point preservation."
  (when (eq (window-buffer window) (current-buffer))
    (niwa-view--render-request-progress))
  (niwa-view--resume-on-window-scroll window))

(defun niwa-view--resume-attended-views (&rest _)
  "Resume the pending render of every view that became attended.
Runs after every frame focus change; the predicate filters focus-out."
  ;; ponytail: focus and redisplay hooks only; add
  ;; `window-state-change-functions' if a deiconified but unfocused frame
  ;; is observed to keep a stale view.
  (dolist (buffer (buffer-list))
    (niwa-view--resume-render-if-attended buffer)))

(defun niwa-view--resume-on-window-change (window)
  "Update the current view when WINDOW starts or stops showing it.
The buffer-local hook also runs with a departing view current.  Only a
newly displayed view resumes pending rendering; the departing view must
still release animation and power observers when it becomes invisible."
  (if (eq (window-buffer window) (current-buffer))
      (niwa-view--resume-render-if-attended (current-buffer))
    (when (fboundp 'niwa-view--start-spinner-timer)
      (niwa-view--start-spinner-timer t)))
  (niwa-view--render-request-progress))

(defun niwa-view--schedule-render (kind data-buffer delay)
  "Coalesce a KIND render of DATA-BUFFER after DELAY seconds.
`full' supersedes `incremental', which supersedes `tools' row updates.
Once scheduled, later requests join
the same refresh instead of creating independent stream, tool, and full
render timers.  A non-positive DELAY flushes at once, which still defers
while the view is unattended.  An unattended view only records the kind:
a timer would wake the editor just to defer again, once per streamed
chunk or progress event.  The resume hooks schedule it when it is seen."
  (unless (memq kind '(tools incremental full))
    (error "Unknown render kind: %S" kind))
  (when (buffer-live-p data-buffer)
    (setq niwa-view--pending-render-data-buffer data-buffer
          niwa-view--pending-render-scope (niwa-diagnostic-scope data-buffer))
    (when (or (eq kind 'full)
              (and (eq kind 'incremental)
                   (eq niwa-view--pending-render-kind 'tools))
              (null niwa-view--pending-render-kind))
      (setq niwa-view--pending-render-kind kind))
    (cond
     ((niwa-view--unattended-p) nil)
     ((and (numberp delay) (> delay 0))
      ;; Test the timer's presence on `timer-list', not the variable: a
      ;; timer armed from a stream or tool hook while TRAMP had timers
      ;; suspended is silently discarded, and trusting the stale object
      ;; would wedge every future render of this view.
      (unless (niwa--timer-pending-p niwa-view--render-timer)
        (let ((view-buffer (current-buffer)))
          (setq niwa-view--render-timer
                (run-at-time
                 delay nil #'niwa-view--flush-scheduled-render
                 view-buffer)))))
     (t
      (when (timerp niwa-view--render-timer)
        (cancel-timer niwa-view--render-timer))
      (niwa-view--flush-scheduled-render (current-buffer) t)))))

(defun niwa-view-rerender (&optional buffer)
  "Schedule a coalesced full re-render of BUFFER.
Default to the current buffer.  Full, stream, and tool-boundary requests
share one timer, and a full request upgrades an already pending
incremental refresh."
  (let ((view-buffer (or buffer (current-buffer))))
    (when (buffer-live-p view-buffer)
      (with-current-buffer view-buffer
        (when (and (boundp 'niwa--data-buffer)
                   (buffer-live-p niwa--data-buffer))
          (niwa-view--schedule-render
           'full niwa--data-buffer niwa-view-rerender-debounce))))))


;;
;;; Sub-agent transcript open command

(defun niwa-view--event-position (&optional event)
  "Return buffer position referenced by mouse EVENT, or nil."
  (and event
       (mouse-event-p event)
       (let ((pos (posn-point (event-end event))))
         (and (integer-or-marker-p pos) pos))))

(defun niwa-view-activate-at-point (&optional event)
  "Activate actionable display or fragment text at point or EVENT.
This command is installed only on display text keymaps; direct calls from
the editable composer signal instead of settling queued interactions."
  (interactive (list last-nonmenu-event))
  (when (niwa-view--event-position event)
    (mouse-set-point event))
  (let* ((pos (point))
         (activate (get-text-property pos 'niwa-view-zone-activate)))
    (cond
     ((and (eq (niwa-view-composer-buffer) (current-buffer))
           (niwa-view-composer-in-draft-p pos))
      (niwa-diagnostic-user-error (niwa-diagnostic-scope (current-buffer)) "No actionable fragment at point"))
     ((get-text-property pos 'niwa-view-agent-path)
      (niwa-view-open-agent-transcript-at-point event))
     ((and activate
           (not (get-text-property pos 'niwa-view-interaction-overlay)))
      (funcall activate))
     ((get-text-property pos 'niwa-tool-task)
      (niwa-toggle-tasks))
     ((and event (mouse-event-p event))
      nil)
     (t
      (niwa-diagnostic-user-error (niwa-diagnostic-scope (current-buffer)) "No actionable fragment at point")))))

(defun niwa-view--indent-region-lines (start end prefix)
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

(defun niwa-view--current-buffer-marker-position (marker)
  "Return MARKER's position when it belongs to the current buffer."
  (and (markerp marker)
       (eq (marker-buffer marker) (current-buffer))
       (marker-position marker)))

(defun niwa-view--status-anchor ()
  "Return the recovered start of fragment-managed status text."
  (let* ((input-pos (niwa-view-transcript-tail))
         (status-pos (niwa-view--current-buffer-marker-position
                      niwa-view--status-marker))
         (history-tail (niwa-view--history-tail-position))
         (status-valid-p
          (and status-pos
               (or (not input-pos) (<= status-pos input-pos))
               (>= status-pos history-tail)
               (not (niwa-view--non-history-view-position-p
                     status-pos))
               (not (and (> status-pos (niwa-view--after-header-position))
                         (niwa-view--non-history-view-position-p
                          (1- status-pos)))))))
    (or (and status-valid-p status-pos)
        history-tail)))

(defun niwa-view--status-trailing-newline-suffix (body)
  "Return the suffix needed to preserve BODY's trailing newlines."
  (let ((pos (length body))
        (count 0))
    (while (and (> pos 0) (eq (aref body (1- pos)) ?\n))
      (setq pos (1- pos)
            count (1+ count)))
    (when (> count 1)
      (make-string (1- count) ?\n))))

(defun niwa-view--status-task-show-completed-p ()
  "Return non-nil when task status should show completed rows."
  (not (niwa-view-zone-collapse-state
        niwa-view--status-task-collapse-key t)))

(defun niwa-view--status-task-body (session show-completed)
  "Return propertized status-zone task text for SESSION and SHOW-COMPLETED."
  (let ((body (niwa-tool-task-display-string session show-completed)))
    (add-text-properties 0 (length body) '(niwa-tool-task t) body)
    (add-text-properties
     0 (or (string-search "\n" body) (length body))
     (list 'keymap (niwa-view--display-fragment-keymap)
           'follow-link t 'mouse-face 'highlight
           'help-echo "Toggle task panel (mouse-1, TAB or RET)")
     body)
    body))

(defun niwa-view--status-session (&optional data-buf)
  "Return DATA-BUF session used for status rendering."
  (or (and data-buf
           (buffer-live-p data-buf)
           (buffer-local-value 'niwa--session data-buf))
      (and (boundp 'niwa--session) niwa--session)
      (and (boundp 'niwa--data-buffer)
           (buffer-live-p niwa--data-buffer)
           (buffer-local-value 'niwa--session niwa--data-buffer))))

(defun niwa-view--status-model (&optional data-buf)
  "Return the authoritative status-zone model for DATA-BUF."
  (let* ((session (niwa-view--status-session data-buf))
         (show-completed (niwa-view--status-task-show-completed-p))
         (task-active-p (and session
                             (niwa-tool-task-session-has-active-p
                              session)))
         (task-body (and task-active-p
                         (niwa-view--status-task-body
                          session show-completed))))
    (list :session session
          :show-completed show-completed
          :task-active-p task-active-p
          :task-body task-body)))

(defun niwa-view--status-recovery-body (session issue)
  "Return ISSUE's notice text and a dismissal control for SESSION.
Blocking issues have no dismissal control.  The action captures this exact
issue, so a stale control cannot dismiss a later failure with the same id."
  (concat
   (niwa-diagnostic-format
    (niwa-diagnostic-scope session) "%s" (plist-get issue :message))
   (unless (plist-get issue :blocking)
     (concat
      "  "
      (make-text-button
       "[Dismiss]" nil
       'follow-link t
       'help-echo "Dismiss this notice; the failure remains in the transcript"
       'action (lambda (_button) (niwa-recovery-dismiss session issue)))))))

(defun niwa-view--status-fragments (model)
  "Return status fragments for MODEL."
  (let (fragments)
    (when-let* ((session (plist-get model :session))
                (issues (niwa-session-recovery-issues session)))
      (push (list :namespace 'status :id 'recovery :priority 120
                  :body (mapconcat
                         (lambda (issue)
                           (niwa-view--status-recovery-body session issue))
                         issues "\n")
                  :keymap (niwa-view--display-fragment-keymap))
            fragments))
    (when-let* ((body (plist-get model :task-body)))
      (let ((fragment (list :namespace 'status
                            :id 'tasks
                            :priority 100
                            :body body
                            :keymap (niwa-view--status-task-keymap)
                            :navigatable t
                            :activate #'niwa-toggle-tasks
                            :entry 'tasks
                            :collapsible t
                            :collapse-key niwa-view--status-task-collapse-key
                            :collapsed (not (plist-get model
                                                        :show-completed))))
            (suffix (niwa-view--status-trailing-newline-suffix body)))
        (when suffix
          (setq fragment (plist-put fragment :body-suffix suffix)))
        (push fragment fragments)))
    (when-let* ((fragment (niwa-view-execution-fragment
                          (plist-get model :session))))
      (push fragment fragments))
    (nreverse fragments)))

(defun niwa-view--render-status (&optional data-buf)
  "Render task, execution, and aggregate agent status for DATA-BUF.
Coalesce materialization until an active projection writer finishes;
idle status redraws remain immediate."
  (if (and (derived-mode-p 'niwa-view-mode)
           (bound-and-true-p niwa-view-render--owner))
      (niwa-view-render-mutate
       'status (lambda () (niwa-view--render-status-now data-buf)))
    (niwa-view--render-status-now data-buf)))

(defun niwa-view--render-status-now (&optional data-buf)
  "Materialize DATA-BUF's status without reentering a projection writer."
  (unless niwa-view--agent-transcript-p
    (let* ((model (niwa-view--status-model data-buf))
           (fragments (niwa-view--status-fragments model))
           (start (niwa-view--status-anchor))
           (input-pos (niwa-view-transcript-tail))
           (interaction-pos (niwa-view--current-buffer-marker-position
                             niwa-view--interaction-marker))
           (end (if (and interaction-pos
                         (<= start interaction-pos)
                         (or (not input-pos) (<= interaction-pos input-pos)))
                    interaction-pos
                  start)))
      (niwa-view-zone-reconcile 'status start end fragments)
      (let ((had-elapsed niwa-view-execution--elapsed-targets))
        (niwa-view-execution-capture-elapsed)
        (when (or had-elapsed niwa-view-execution--elapsed-targets)
          (niwa-view--start-spinner-timer))))))

(defun niwa-view--zone-separator (label)
  "Return a propertized zone separator line for LABEL.
Format: ` ─── LABEL ─── ' followed by enough box-drawing dashes
to reach four columns less than `niwa-view-zone-width', the narrowest
window displaying this buffer, then a trailing newline.  A rule sized
for a wider window wrapped onto two or three rows in a narrower one.
The minimum width is ten columns; labels are truncated when necessary
so all rules use the same width.

LABEL is a single string like \"tasks\", \"1 permission pending\",
\"2 previews · 1 plan pending\".  Caller is responsible for
constructing composite count labels via concatenation.

The returned string carries `niwa-view-zone-separator' face on
the dash runs and inherits the same face on the label text so
tweaks via `customize-face' apply uniformly."
  (let* ((target-width (max 10 (- (niwa-view-zone-width) 4)))
         (pre " ─── ")
         (post " ")
         (label-budget (- target-width (string-width pre) (string-width post) 3))
         (decorated-label
          (if (> (string-width (or label "")) label-budget)
              (truncate-string-to-width label label-budget nil nil "…")
            (or label "")))
         (used (+ (string-width pre) (string-width decorated-label)
                  (string-width post)))
         (tail-len (- target-width used))
         (tail (concat (make-string tail-len ?─))))
    (concat
     (propertize (concat pre decorated-label post tail "\n")
                 'face 'niwa-view-zone-separator
                 'font-lock-face 'niwa-view-zone-separator))))

;; Status rules and rows are laid out for the narrowest window showing the
;; view; redraw them when that width changes.
(add-hook 'niwa-view-zone-width-change-functions #'niwa-view--render-status)

(defun niwa-view--header-end-position ()
  "Return the position after the current view header, when recognized."
  (or
   (when (eq (get-text-property (point-min) 'font-lock-face)
             'niwa-view-header)
     (save-excursion
       (goto-char (point-min))
       (let ((end (line-end-position)))
         (if (and (< end (point-max))
                  (eq (char-after end) ?\n))
             (1+ end)
           end))))
   (when-let* ((data-buf (and (boundp 'niwa--data-buffer)
                              niwa--data-buffer))
               ((buffer-live-p data-buf))
               (header (substring-no-properties
                        (niwa-view--header-string data-buf)))
               (end (+ (point-min) (length header)))
               ((<= end (point-max)))
               ((equal header
                       (buffer-substring-no-properties (point-min) end))))
     end)))

(provide 'niwa-view)

;;; niwa-view.el ends here
