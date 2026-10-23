;;; mevedel-view-render.el --- Transcript rendering and navigation -*- lexical-binding: t -*-

;;; Commentary:

;; Owns transcript turn grouping, source-backed rendering, renderer
;; interpretation, folding, expansion, full and incremental redraws, and
;; transcript navigation for mevedel view buffers.

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'mevedel-execution-transcript)
(require 'mevedel-plan)
(require 'mevedel-view-fontify)
(require 'mevedel-view-prepare)

;; `cl-extra'
(declare-function cl-some "cl-extra"
		  (predicate sequence &rest more-sequences))
(declare-function cl-subseq "cl-extra" (seq start &optional end))

;; `cl-macs'
(declare-function cl-gensym "cl-macs" (&optional prefix))

;; `cl-seq'
(declare-function cl-find "cl-seq" (cl-item cl-seq &rest cl-keys))
(declare-function cl-find-if "cl-seq" (cl-pred cl-list &rest cl-keys))
(declare-function cl-remove "cl-seq" (cl-item cl-seq &rest cl-keys))

;; `gptel'
(defvar gptel-display-buffer-action)

;; `mevedel-agents'
(declare-function mevedel-agent-invocation-p "mevedel-agents" (cl-x))
(declare-function mevedel-agent-invocation-transcript-status
		  "mevedel-agents" (cl-x) t)

;; `mevedel-chat'
(declare-function mevedel--replace-patch-buffer "mevedel-chat" (patch))

;; `mevedel-directive'
(declare-function mevedel-directive-actions "mevedel-directive" (directive))

;; `mevedel-directive-activity'
(declare-function mevedel-open-directive-activity
                  "mevedel-directive-activity"
                  (&optional directive workspace))
(autoload 'mevedel-open-directive-activity "mevedel-directive-activity" nil t)

;; `mevedel-directive-frame'
(declare-function mevedel-directive-frame-refresh-filter
                  "mevedel-directive-frame" ())

;; `mevedel-directive-request'
(declare-function mevedel--implement-discussion
                  "mevedel-directive-request" (directive &optional callback))

;; `mevedel-execution-telemetry'
(declare-function mevedel-execution-telemetry-sandbox-summary-class
                  "mevedel-execution-telemetry" (summary))

;; `mevedel-execution-transcript'
(declare-function mevedel-execution-transcript-pending-render-data
                  "mevedel-execution-transcript"
                  (data-buffer tool-use-id))

;; `mevedel-overlay-ui'
(declare-function mevedel-overlay-ui-directive-action-label
                  "mevedel-overlay-ui" (action))

;; `mevedel-overlays'
(declare-function mevedel--directive-action-context
                  "mevedel-overlays" (record workspace))
(autoload 'mevedel--directive-action-context "mevedel-overlays")

;; `mevedel-plan'
(declare-function mevedel-plan-extract-proposed "mevedel-plan" (text))
(declare-function mevedel-plan-strip-proposed "mevedel-plan" (text))

;; `mevedel-review'
(declare-function mevedel-review-strip-user-action-blocks
		  "mevedel-review" (text))
(autoload 'mevedel-review-strip-user-action-blocks "mevedel-review")

;; `mevedel-session-artifacts'
(declare-function mevedel-session-artifacts-fork-point-at-source
                  "mevedel-session-artifacts"
                  (buffer source-start source-end))
(declare-function mevedel-session-artifacts-fork-point-spans
                  "mevedel-session-artifacts" (buffer))

;; `mevedel-session-persistence'
(declare-function mevedel-session-persistence-choose-conversation-variant
                  "mevedel-session-persistence"
                  (variants current-session-id))
(declare-function mevedel-session-persistence-conversation-variants
                  "mevedel-session-persistence"
                  (session fork-point-id &optional sessions))
(declare-function mevedel-session-persistence-list-sessions
                  "mevedel-session-persistence" (workspace &optional cached))
(declare-function mevedel-session-persistence-restore
                  "mevedel-session-persistence"
                  (session-dir &optional lifecycle-source
                               session-override workspace))

;; `mevedel-session-rewind'
(declare-function mevedel-session-rewind-rewind
                  "mevedel-session-rewind" (buffer target &optional boundary))
(declare-function mevedel-session-rewind-rewind-checkpoint
                  "mevedel-session-rewind"
                  (workspace checkpoint &optional buffer))
(autoload 'mevedel-session-rewind-rewind "mevedel-session-rewind")
(autoload 'mevedel-session-rewind-rewind-checkpoint "mevedel-session-rewind")

;; `mevedel-structs'
(declare-function mevedel-directive-attempt-checkpoint
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-attempt-patch
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-attempt-sequence
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-attempts "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-discussion "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-discussion-turn-attempt-index
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-discussion-turn-sequence
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-id "mevedel-structs" (cl-x) t)
(declare-function mevedel-request-active-elapsed-seconds
                  "mevedel-structs" (request &optional now))
(declare-function mevedel-request-directive-uuid "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-current-segment "mevedel-structs"
                  (cl-x) t)
(declare-function mevedel-session-goal "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-pending-plan-approval
		  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-permission-queue "mevedel-structs"
		  (cl-x) t)
(declare-function mevedel-session-plan-mode "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-prompt-index "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-save-path "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-session-id "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-workspace "mevedel-structs" (cl-x) t)
(declare-function mevedel-workspace-directives "mevedel-structs" (cl-x) t)
(defvar mevedel--agent-invocation)
(defvar mevedel--current-request)
(defvar mevedel--data-buffer)
(defvar mevedel--session)
(defvar mevedel--view-buffer)

;; `mevedel-tool-registry'
(declare-function mevedel-tool-display-string "mevedel-tool-registry"
		  (tool-name args))
(declare-function mevedel-tool-for-call "mevedel-tool-registry" (name))
(declare-function mevedel-tool-get "mevedel-tool-registry"
		  (name &optional category))
(declare-function mevedel-tool-name "mevedel-tool-registry" (cl-x) t)
(declare-function mevedel-tool-renderer "mevedel-tool-registry" (cl-x)
                  t)
(defvar mevedel-tool-render-summary-only)

;; `mevedel-tool-render-data'
(declare-function mevedel-tool-render-data-block-at
                  "mevedel-tool-render-data" (start &optional limit))
(declare-function mevedel-tool-render-data-blocks
                  "mevedel-tool-render-data" (string))
(declare-function mevedel-tool-render-data-direct-call
                  "mevedel-tool-render-data" (name data))
(declare-function mevedel-tool-render-data-extract
                  "mevedel-tool-render-data"
                  (result-string &optional session expected-tool-use-id
                                 allow-payload-tool-use-id))
(declare-function mevedel-tool-render-data-format
                  "mevedel-tool-render-data" (render-data &optional tool-use-id))
(declare-function mevedel-tool-render-data-segment-bounds
                  "mevedel-tool-render-data" (tool-use-id))
(declare-function mevedel-tool-render-data-strip
                  "mevedel-tool-render-data" (string &optional expected-tool-use-id))
(declare-function mevedel-tool-render-data-trusted-range-p
                  "mevedel-tool-render-data" (start end &optional object))

;; `mevedel-tool-ui'
(declare-function mevedel-tool-ui--render-agent "mevedel-tool-ui"
		  (name args result render-data))

;; `mevedel-transcript'
(declare-function mevedel-transcript--mailbox-any-block-at-point
		  "mevedel-transcript" (limit))
(declare-function mevedel-transcript--mailbox-find-close
		  "mevedel-transcript" (open-regexp close-tag limit))
(declare-function mevedel-transcript--mailbox-open-at-point
                  "mevedel-transcript" (kind limit))
(declare-function mevedel-transcript--org-tool-block-parts
		  "mevedel-transcript" (start end))
(declare-function mevedel-transcript--skip-leading-properties-drawer
		  "mevedel-transcript" (pos))
(declare-function mevedel-transcript--skip-leading-summary-block
		  "mevedel-transcript" (pos))
(declare-function mevedel-transcript--system-reminder-range-at-point
		  "mevedel-transcript" (limit))
(declare-function mevedel-transcript--tool-block-bounds-for-run
		  "mevedel-transcript"
		  (seg-start seg-end &optional limit))
(declare-function mevedel-transcript--tool-id-in-range
		  "mevedel-transcript" (start end))
(declare-function mevedel-transcript-scan-cancel "mevedel-transcript" (job))
(declare-function mevedel-transcript-scan-result "mevedel-transcript" (job))
(declare-function mevedel-transcript-scan-start "mevedel-transcript" (start end))
(declare-function mevedel-transcript-scan-step "mevedel-transcript" (job))
(declare-function mevedel-transcript-segments "mevedel-transcript"
		  (start end))
(defvar mevedel-transcript--tool-block-index)
(defvar mevedel-view--path-link-memo)

;; `mevedel-transcript-audit'
(declare-function mevedel-transcript-audit-buffer-only-p
                  "mevedel-transcript-audit" (start end))
(declare-function mevedel-transcript-audit-only-p
                  "mevedel-transcript-audit" (text))
(declare-function mevedel-transcript-audit-shared-context
                  "mevedel-transcript-audit" (text shared))
(declare-function mevedel-transcript-buffer-directive-ranges
                  "mevedel-transcript-audit" (&optional allow-open))
(autoload 'mevedel--strip-hook-audit-blocks "mevedel-transcript-audit")

;; `mevedel-transcript-restore'
(declare-function mevedel-transcript-restore-properties
                  "mevedel-transcript-restore" (&optional only-if-missing))

;; `mevedel-transport'
(declare-function mevedel-transport-busy-p "mevedel-transport" (target))

;; `mevedel-utilities'
(declare-function mevedel--duration-label "mevedel-utilities" (seconds))
(declare-function mevedel--gc-hold "mevedel-utilities" (key live-p))
(declare-function mevedel--gc-release "mevedel-utilities" (key))
(declare-function mevedel--timer-pending-p "mevedel-utilities" (timer))
(declare-function mevedel--trim-tool-result "mevedel-utilities" (text))
(declare-function mevedel--warn-once
                  "mevedel-utilities" (key format &rest args))
(defvar mevedel--hook-audit-close)
(defvar mevedel--hook-audit-open)
(defvar mevedel-transcript-audit--decode-cache)

;; `mevedel-view'
(declare-function mevedel-view--header-string
                  "mevedel-view" (data-buf))
(declare-function mevedel-view--pinned-prompt "mevedel-view" (window))
(declare-function mevedel-view--refresh-continuation-prompt
                  "mevedel-view" (data-buffer historical-p))
(declare-function mevedel-view--render-status-now
                  "mevedel-view" (&optional data-buf))
(declare-function mevedel-view--unattended-p "mevedel-view" (&optional buffer))
(declare-function mevedel-view-rerender "mevedel-view" (&optional buffer))
(defvar mevedel-view--display-map)
(defvar mevedel-view--interaction-marker)
(defvar mevedel-view--status-marker)

;; `mevedel-view-agent'
(declare-function mevedel-view--insert-attribution
                  "mevedel-view-agent"
                  (agent-path))
(declare-function mevedel-view--render-agent-status
                  "mevedel-view-agent" ())
(defvar mevedel-view--agent-handle-map)
(defvar mevedel-view--agent-label-map)
(defvar mevedel-view--agent-path)

;; `mevedel-view-composer'
(declare-function mevedel-view--call-preserving-input-text
                  "mevedel-view-composer" (thunk))
(declare-function mevedel-view--call-preserving-user-view-state
                  "mevedel-view-composer" (thunk &optional region))
(declare-function mevedel-view--call-preserving-window-state
                  "mevedel-view-composer"
                  (thunk &optional positions-preserved region))
(declare-function mevedel-view--call-with-render-boundaries-advancing
                  "mevedel-view-composer" (thunk))
(declare-function mevedel-view--ensure-interactive-chat-view
                  "mevedel-view-composer" ())
(declare-function mevedel-view--input-marker-position
                  "mevedel-view-composer" ())
(declare-function mevedel-view--input-start
                  "mevedel-view-composer" ())
(declare-function mevedel-view--position-render-anchor
                  "mevedel-view-composer" (pos &optional window region))
(declare-function mevedel-view--prompt-start-position
                  "mevedel-view-composer" ())
(declare-function mevedel-view--render-anchor-position
                  "mevedel-view-composer" (anchor &optional strict))
(declare-function mevedel-view-enter-directive-scope
                  "mevedel-view-composer"
                  (directive action &optional attempt-index workspace))
(declare-function mevedel-view-refresh-input-prompt
                  "mevedel-view-composer" ())
(defvar mevedel-view--input-marker)

;; `mevedel-view-disclosure'
(declare-function mevedel-view-disclosure--toggle-mailbox
                  "mevedel-view-disclosure" ())
(declare-function mevedel-view-disclosure-apply-rendering-state
                  "mevedel-view-disclosure" (rendering source))
(declare-function mevedel-view-disclosure-capture-state
                  "mevedel-view-disclosure" (from to))
(declare-function mevedel-view-disclosure-collapse-mailbox-body
                  "mevedel-view-disclosure" (start end))
(declare-function mevedel-view-disclosure-data-substring
                  "mevedel-view-disclosure"
                  (data-buf start end &optional properties))
(declare-function mevedel-view-disclosure-indent-text
                  "mevedel-view-disclosure" (text width))
(autoload 'mevedel-view-disclosure-indent-text "mevedel-view-disclosure")
(declare-function mevedel-view-disclosure-initialize
                  "mevedel-view-disclosure" ())
(declare-function mevedel-view-disclosure-mailbox-hint
                  "mevedel-view-disclosure" (line-count))
(declare-function mevedel-view-disclosure-mailbox-line-count
                  "mevedel-view-disclosure" (start end))
(declare-function mevedel-view-disclosure-rebase-state
                  "mevedel-view-disclosure" (delta))
(declare-function mevedel-view-disclosure-record-state
                  "mevedel-view-disclosure" (source vtype collapsed))
(declare-function mevedel-view-disclosure-record-state-for-key
                  "mevedel-view-disclosure" (key collapsed))
(declare-function mevedel-view-disclosure-reset-state
                  "mevedel-view-disclosure" ())
(declare-function mevedel-view-disclosure-restore-state
                  "mevedel-view-disclosure" (from to states))
(declare-function mevedel-view-disclosure-section-bounds
                  "mevedel-view-disclosure" (&optional property))
(declare-function mevedel-view-disclosure-source-range
                  "mevedel-view-disclosure" (data-buffer start end))
(declare-function mevedel-view-disclosure-source-start
                  "mevedel-view-disclosure" (source))
(declare-function mevedel-view-disclosure-state-entry
                  "mevedel-view-disclosure" (source vtype))
(declare-function mevedel-view-disclosure-state-for-key
                  "mevedel-view-disclosure" (key))
(declare-function mevedel-view-disclosure-state-key
                  "mevedel-view-disclosure"
                  (source vtype &optional previous-key))
(declare-function mevedel-view-disclosure-truncate-line
                  "mevedel-view-disclosure" (text limit))
(defvar mevedel-view-disclosure--settling-p)

;; `mevedel-view-interaction'
(declare-function mevedel-view--interaction-focused-visits
                  "mevedel-view-interaction" ())
(declare-function mevedel-view--interaction-rebuild
                  "mevedel-view-interaction" (&optional owned visits))

;; `mevedel-view-markdown'
(declare-function mevedel-view--last-live-response-boundary
                  "mevedel-view-markdown" (data-buf start end))

;; `mevedel-view-prepare'
(declare-function mevedel-view-prepare-cancel "mevedel-view-prepare" ())
(declare-function mevedel-view-prepare-get "mevedel-view-prepare" (source start end key))
(declare-function mevedel-view-prepare-initialize "mevedel-view-prepare" ())
(defvar mevedel-view-prepare-enabled)

;; `mevedel-session-artifacts'
(declare-function mevedel-session-artifacts-read-transcript-segment
                  "mevedel-session-artifacts" (session descriptor))
(declare-function mevedel-session-artifacts-transcript-segments
                  "mevedel-session-artifacts"
                  (session live-buffer &optional agent-transcript-p))
(autoload 'mevedel-session-artifacts-read-transcript-segment "mevedel-session-artifacts")
(autoload 'mevedel-session-artifacts-transcript-segments "mevedel-session-artifacts")

;; `mevedel-view-segments'
(declare-function mevedel-view-go-to-segment
                  "mevedel-view-segments" (&optional number))
(declare-function mevedel-view-historical-segment-p
                  "mevedel-view-segments" ())
(declare-function mevedel-view-return-to-latest-segment
                  "mevedel-view-segments" (&optional _event))
(declare-function mevedel-view-segments-banner
                  "mevedel-view-segments" ())
(declare-function mevedel-view-segments-chats-line
                  "mevedel-view-segments" ())
(declare-function mevedel-view-segments-display-buffer
                  "mevedel-view-segments" ())
(declare-function mevedel-view-segments-initialize
                  "mevedel-view-segments" ())
(autoload 'mevedel-view-segments-display-buffer "mevedel-view-segments")
(autoload 'mevedel-view-historical-segment-p "mevedel-view-segments")

;; `mevedel-view-stream'
(declare-function mevedel-view--adopt-tool-animation-targets
                  "mevedel-view-stream" (start end previous))
(declare-function mevedel-view--ensure-request-progress
                  "mevedel-view-stream" (&optional data-buf status))
(declare-function mevedel-view--forget-request-progress-region
                  "mevedel-view-stream" ())
(declare-function mevedel-view--repair-request-progress-separator
                  "mevedel-view-stream" ())
(declare-function mevedel-view--retire-pending-tool-rows
                  "mevedel-view-stream" ())
(declare-function mevedel-view--schedule-tool-boundary-render
                  "mevedel-view-stream" (data-buf))
(declare-function mevedel-view--snapshot-tool-animation-targets
                  "mevedel-view-stream" ())
(declare-function mevedel-view-animation-frame
                  "mevedel-view-animation" (style label seconds face &optional frame))
(declare-function mevedel-view--spinner-region-p
                  "mevedel-view-stream" (start end))
(declare-function mevedel-view-stream-in-flight-turn-start-position
                  "mevedel-view-stream" ())
(declare-function mevedel-view-stream-set-in-flight-turn-start
                  "mevedel-view-stream" (position))
(defvar mevedel-view--data-turn-start)
(defvar mevedel-view--carried-tool-samples)
(defvar mevedel-view--execution-events)
(defvar mevedel-view--in-flight-turn-start)
(defvar mevedel-view--pending-tool-calls)
(defvar mevedel-view--spinner-tool-targets)
(defvar mevedel-view-tool-spinner-style)

;; `mevedel-view-table'
(defvar mevedel-view-table--streaming-p)

;; `mevedel-view-zone'
(declare-function mevedel-view-zone-bounds-at
                  "mevedel-view-zone" (&optional position))
(declare-function mevedel-view-zone-next
                  "mevedel-view-zone" (&optional limit))
(declare-function mevedel-view-zone-previous
                  "mevedel-view-zone" (&optional limit))
(declare-function mevedel-view-zone-region "mevedel-view-zone" (zone))

;; `org'
(declare-function org-mode "ext:org" ())
(declare-function org-unescape-code-in-string "ext:org-src" (s))
(defvar org-mode-hook)


;;
;;; Customization

(defcustom mevedel-view-fontify-responses t
  "Non-nil means fontify response bodies using Markdown syntax.
Each assistant response stays as model-written Markdown in the view and
is fontified through `markdown-ts-mode' when its tree-sitter grammars are
installed; otherwise the raw Markdown text stays visible."
  :type 'boolean
  :group 'mevedel)


(defcustom mevedel-view-hide-markdown-markup t
  "Non-nil means hide Markdown markup delimiters in the view.
Heading hashes, emphasis asterisks, and code-span backticks stay in the
text but are made invisible, so `**bold**' reads as bold alone: only
display changes, never the text the view holds or the positions it maps
back to the data buffer.  Has no effect without the `markdown-ts-mode'
grammars."
  :type 'boolean
  ;; The default needs no re-render, and the setter is defined further down.
  :initialize #'custom-initialize-default
  :set #'mevedel-view--set-hide-markdown-markup
  :group 'mevedel)


(defcustom mevedel-view-render-debug nil
  "Non-nil means trace view buffer render decisions.
The trace is written to `mevedel-view-render-debug-buffer-name'.
It includes marker positions, replacement decisions, and short text
previews around the live in-flight region.  Enable only while
reproducing a view-rendering bug."
  :type 'boolean
  :group 'mevedel)

(defcustom mevedel-view-render-debug-buffer-name "*mevedel-view-render-trace*"
  "Name of the buffer that receives view-render debug traces."
  :type 'string
  :group 'mevedel)

(defcustom mevedel-view-mailbox-collapse-line-threshold 0
  "Mailbox delivery bodies longer than this many lines start collapsed.
Shorter bodies render fully expanded."
  :type 'integer
  :group 'mevedel)

(defcustom mevedel-view-user-input-collapse-line-threshold 15
  "User prompts longer than this many lines fold to a one-line summary.
The folded line shows the truncated first line and the hidden line
count, and expands in place.  Zero disables folding."
  :type 'integer
  :group 'mevedel)

(defcustom mevedel-view-tool-group-collapse-threshold 3
  "Runs of more than this many consecutive tool rows fold into one group.
The group renders as a one-line activity summary that expands into the
individual tool rows.  Zero disables grouping."
  :type 'integer
  :group 'mevedel)

(defcustom mevedel-view-render-cache-max-entries 256
  "Maximum number of response-fontification and source-bounds cache entries.
Tool renderings instead retain the working set used by the last full
projection; a fixed entry limit made long activity runs reparse every tool."
  :type 'integer
  :group 'mevedel)


;;
;;; Summary primitives

(defconst mevedel-view--thinking-glyph "… "
  "Prefix shown in front of thinking/reasoning summary lines.")

(defconst mevedel-view--response-glyph "▸ "
  "Prefix shown in front of collapsed response summary lines.")


(defun mevedel-view--operation-line
    (marker marker-face label &optional detail metadata label-face)
  "Return a compact propertized operation summary line.
MARKER is the leading status glyph.  MARKER-FACE styles that glyph.
LABEL is the primary label and DETAIL/METADATA are optional secondary
fragments.  LABEL-FACE defaults to `mevedel-view-tool-name'."
  (concat
   (propertize (concat "  " marker " ")
               'font-lock-face marker-face)
   (propertize label
               'font-lock-face (or label-face 'mevedel-view-tool-name))
   (when (and detail (not (string-empty-p detail)))
     (concat "  "
             (propertize detail
                         'font-lock-face 'mevedel-view-tool-argument)))
   (when (and metadata (not (string-empty-p metadata)))
     (concat " "
             (mevedel-view--tool-metadata-text metadata)))))

(defun mevedel-view--tool-metadata-text (metadata)
  "Return propertized summary METADATA text.

Diff metadata of the form `(+N -M)' keeps the parenthesized wrapper in
the normal metadata face while styling the added and removed counts
separately."
  (when (and metadata (not (string-empty-p metadata)))
    (if (string-match "\\`(\\(\\+[0-9]+\\) \\(-[0-9]+\\))\\'" metadata)
        (concat
         (propertize "(" 'font-lock-face 'mevedel-view-tool-metadata)
         (propertize (match-string 1 metadata)
                     'font-lock-face 'mevedel-view-tool-diff-added)
         (propertize " " 'font-lock-face 'mevedel-view-tool-metadata)
         (propertize (match-string 2 metadata)
                     'font-lock-face 'mevedel-view-tool-diff-removed)
         (propertize ")" 'font-lock-face 'mevedel-view-tool-metadata))
      (propertize metadata
                  'font-lock-face 'mevedel-view-tool-metadata))))

(defun mevedel-view--tool-call-line
    (marker marker-face name &optional primary-arg metadata name-face)
  "Return a propertized compact tool call line.
MARKER and MARKER-FACE describe the leading status glyph.  NAME is the
tool label.  PRIMARY-ARG, when non-empty, is rendered after a literal
colon so every tool row keeps the same `Tool: argument' shape.
METADATA is optional secondary summary text.  NAME-FACE overrides the
face used for NAME."
  (concat
   (propertize (concat "  " marker " ")
               'font-lock-face marker-face)
   (propertize (or name "Tool")
               'font-lock-face (or name-face 'mevedel-view-tool-name))
   (when (and primary-arg (not (string-empty-p primary-arg)))
     (concat ": "
             (propertize primary-arg
                         'font-lock-face 'mevedel-view-tool-argument)))
   (when (and metadata (not (string-empty-p metadata)))
     (concat " "
             (mevedel-view--tool-metadata-text metadata)))))

(defun mevedel-view--tool-result-error-p (result-text)
  "Return non-nil when RESULT-TEXT resembles a tool-level failure."
  (and (stringp result-text)
       (string-match-p
        "\\`[ \t\n]*\\(?:Error\\b\\|FAILED\\b\\|Tool failed\\b\\)"
        result-text)))

(defun mevedel-view--tool-summary-line
    (name primary-arg result-lines &optional blocked error-p)
  "Return a propertized collapsed tool summary line.
NAME is the tool name, PRIMARY-ARG is the renderer-provided compact
argument, and RESULT-LINES is the number of output lines.  BLOCKED is
the hook-block plist returned by `mevedel-view--tool-hook-blocked-info'.
ERROR-P means the result itself looks like a tool-level failure."
  (let* ((blocked-p (and blocked t))
         (failed-p (or blocked-p error-p))
         (summary
          (mevedel-view--tool-call-line
           (if failed-p "×" "✓")
           (if failed-p
               'error
             'mevedel-view-tool-marker)
           name
           primary-arg
           (format "(%d lines)" result-lines))))
    (if blocked-p
        (concat
         summary
         "\n"
         (propertize "    blocked by "
                     'font-lock-face 'mevedel-view-tool-metadata)
         (propertize
          (format "%s: %s"
                  (plist-get blocked :event)
                  (plist-get blocked :reason))
          'font-lock-face 'mevedel-view-tool-metadata))
      summary)))

(defun mevedel-view--tool-header-fallback-info (raw)
  "Return fallback tool display info parsed from RAW's org tool header."
  (when (and (stringp raw)
             (string-match "^#\\+begin_tool[[:space:]]+(\\([^[:space:])]+\\)" raw))
    (let ((name (match-string 1 raw))
          primary)
      (dolist (key '("file_path" "path" "command" "pattern" "url" "id"))
        (when (and (not primary)
                   (string-match
                    (format ":%s[[:space:]]+\\\"\\([^\\\"]+\\)\\\"" key)
                    raw))
          (setq primary (match-string 1 raw))))
      (list :name name :primary-arg primary))))

(defun mevedel-view--tool-fallback-line (raw)
  "Return a compact propertized fallback summary for unparseable RAW."
  (let* ((header-info (mevedel-view--tool-header-fallback-info raw))
         (material (string-trim
                    (replace-regexp-in-string
                     "#\\+\\(?:begin\\|end\\)_\\(?:tool\\|reasoning\\)[^\n]*\n?"
                     "" (or raw "")))))
    (unless (string-empty-p material)
      (if header-info
          (mevedel-view--tool-call-line
           "?"
           'mevedel-view-tool-warning
           (plist-get header-info :name)
           (plist-get header-info :primary-arg)
           nil)
        (mevedel-view--operation-line
         "?"
         'mevedel-view-tool-warning
         (mevedel--truncate-display (replace-regexp-in-string "[\n\r]+" " " raw)
          60 "...")
         nil nil
         'mevedel-view-tool-summary)))))

(defun mevedel-view--text-has-font-lock-face-p (text)
  "Return non-nil when TEXT already carries any `font-lock-face'."
  (and (stringp text)
       (> (length text) 0)
       (text-property-not-all 0 (length text) 'font-lock-face nil text)))

(defun mevedel-view--summary-with-face (summary face)
  "Return SUMMARY with FACE when it has no existing font-lock styling."
  (if (or (null face)
          (mevedel-view--text-has-font-lock-face-p summary))
      summary
    (propertize summary 'font-lock-face face)))

(defun mevedel-view--insert-summary-region (summary props)
  "Insert SUMMARY followed by a newline and add non-face PROPS.
Text-local fontification in SUMMARY is preserved."
  (let ((start (point)))
    (insert summary)
    (unless (and (> (point) start)
                 (eq (char-before) ?\n))
      (insert "\n"))
    (add-text-properties start (point) props)
    start))


;;
;;; Buffer-locals


(defvar-local mevedel-view--render-insertion-marker nil
  "Temporary marker used by render helpers as their insertion point.
Nil means render at `mevedel-view--input-marker'.  Incremental history
rebuilds bind this to `mevedel-view--status-marker' so the in-flight
assistant turn is inserted into the history region above status and
interaction zones instead of inside them.")


(defvar-local mevedel-view--tool-rendering-cache nil
  "Hash table caching parsed/rendered tool metadata for this view.")

(defvar-local mevedel-view--directive-collapse-states nil
  "Hash table of directive turn fold states keyed by directive id and turn.")

(defconst mevedel-view--missing-directive-collapse-state
  (make-symbol "mevedel-view-missing-directive-collapse-state")
  "Sentinel for absent directive turn fold-state entries.")

(defvar-local mevedel-view--response-fontify-cache nil
  "Hash table caching response fontification for this view.")

(defvar mevedel-view-render--prepared-responses nil
  "Dynamically bound response cache owned by a scheduled full projection.")

(defvar-local mevedel-view--response-cache-entries 0
  "Approximate number of entries in `mevedel-view--response-fontify-cache'.")

(defvar mevedel-view--conversation-variant-sessions nil
  "Persisted session summaries bound once around a full render.")

(defvar-local mevedel-view--user-pre-rendered nil
  "Non-nil when the most recent user turn was pre-rendered by the view.

Set by `mevedel-view--insert-user-message' when the view's send path
echoes the user's input immediately, and consumed (cleared) by
`mevedel-view-stream-render-response' to skip the user turn that
`mevedel-transcript-segments' may pick up for the same exchange,
which would otherwise produce a duplicate \"You\" block above the
assistant reply.  Tests that drive function
`mevedel-view-stream-render-response' directly (without going through the
send path) leave the flag nil and see user
turns rendered as usual.")

(defvar-local mevedel-view--live-data-tail-start nil
  "Data-buffer marker where the mutable live render tail starts.")

(defvar-local mevedel-view--live-view-tail-start nil
  "View-buffer marker where the mutable live render tail starts.")

(defvar-local mevedel-view--live-source-change-hook nil
  "Change hook that invalidates this view's retained live prefix.")

(defvar mevedel-view-render--reader-members nil
  "Tool rows under a reader when the current projection writer started.
Each element is a row's data source start, or an in-flight call's key.")

(defvar-local mevedel-view-render--pending-reader-group nil
  "Running-only group opened beneath the reader, as (TURN-MARKER START).
START is the data end where its first result will arrive; the canonical
tool segment includes gptel's separator and Org wrapper.  Once a result
lands, transfer expansion to the source-backed group's disclosure state.
TURN-MARKER is compared by identity so an aborted turn cannot lend its
expansion to a later turn starting at the same data position.")

(defvar-local mevedel-view-render--batch nil
  "Pending full-history projection, owned by this view.")

(defun mevedel-view-render-initialize ()
  "Initialize transcript-rendering state in the current view buffer."
  (mevedel-view-render-invalidate-live-tail)
  (mevedel-view-render-cancel-batch)
  (add-hook 'kill-buffer-hook #'mevedel-view-render-cancel-batch nil t)
  ;; Loading the mode also registers its grammars in
  ;; `treesit-language-source-alist', so the install hint has a source.
  (require 'markdown-ts-mode nil t)
  (mevedel-view--markdown-grammar-hint)
  (require 'org-src nil t)
  (require 'xml)
  (require 'mevedel-execution-telemetry)
  (require 'mevedel-overlay-ui)
  (require 'mevedel-review)
  (require 'mevedel-session-artifacts)
  (require 'mevedel-session-persistence)
  (require 'mevedel-tool-render-data)
  (require 'mevedel-transcript)
  (require 'mevedel-transcript-audit)
  (require 'mevedel-transcript-restore)
  (require 'mevedel-view-disclosure)
  (mevedel-view-prepare-initialize)
  (require 'mevedel-view-segments)
  (require 'mevedel-view-stream)
  (require 'mevedel-view-zone)
  (setq-local mevedel-view--tool-rendering-cache
              (make-hash-table :test #'equal))
  (mevedel-view-disclosure-initialize)
  (setq-local mevedel-view--directive-collapse-states
              (make-hash-table :test #'equal))
  (setq-local mevedel-view--response-fontify-cache
              (make-hash-table :test #'equal))
  (setq-local mevedel-view--response-cache-entries 0)
  (setq-local mevedel-view--user-pre-rendered nil)
  (setq-local mevedel-view--live-data-tail-start nil)
  (setq-local mevedel-view--live-view-tail-start nil)
  (setq-local mevedel-view--live-source-change-hook nil)
  (mevedel-view-segments-initialize))

(defun mevedel-view-render-invalidate-live-tail ()
  "Forget the current view buffer's retained live-render tail."
  (let ((data-marker mevedel-view--live-data-tail-start)
        (view-marker mevedel-view--live-view-tail-start)
        (change-hook mevedel-view--live-source-change-hook))
    (when-let* (((markerp data-marker))
                (data-buf (marker-buffer data-marker))
                ((buffer-live-p data-buf))
                ((functionp change-hook)))
      (with-current-buffer data-buf
        (remove-hook 'before-change-functions change-hook t)))
    (setq mevedel-view--live-data-tail-start nil
          mevedel-view--live-view-tail-start nil)
    (when (markerp data-marker)
      (set-marker data-marker nil))
    (when (markerp view-marker)
      (set-marker view-marker nil))))

(defun mevedel-view--live-tail-valid-p (data-buf)
  "Return non-nil when this view's retained tail belongs to DATA-BUF."
  (and (markerp mevedel-view--live-data-tail-start)
       (marker-position mevedel-view--live-data-tail-start)
       (eq (marker-buffer mevedel-view--live-data-tail-start) data-buf)
       (markerp mevedel-view--live-view-tail-start)
       (marker-position mevedel-view--live-view-tail-start)
       (eq (marker-buffer mevedel-view--live-view-tail-start)
           (current-buffer))))

(defun mevedel-view--live-tail-intact-p (data-buf first-end end)
  "Return non-nil if the retained DATA-BUF unit remains whole through END.
FIRST-END tracks the end of its original first character, with insertion
type nil.  That character must survive before one uninterrupted source run;
otherwise an advancing start marker could accept a replaced unit's suffix."
  (when (mevedel-view--live-tail-valid-p data-buf)
    (let ((start (marker-position mevedel-view--live-view-tail-start))
          (source (marker-position mevedel-view--live-data-tail-start)))
      (and (< start end)
           (= (1+ start) first-end)
           (or (= start (point-min))
               (not (equal source
                           (get-text-property
                            (1- start) 'mevedel-view-live-unit-source))))
           (not (text-property-not-all
                 start end 'mevedel-view-live-unit-source source))))))

(defun mevedel-view--mark-live-render-unit (start source-start)
  "Mark START through point as one live unit backed by SOURCE-START."
  (when (< start (point))
    (put-text-property start (point)
                       'mevedel-view-live-unit-source source-start)))

(defun mevedel-view--note-source-change (view-buf data-buf beg _end)
  "Invalidate VIEW-BUF when DATA-BUF changes before retained position BEG."
  (when (buffer-live-p view-buf)
    (with-current-buffer view-buf
      (when (and (markerp mevedel-view--live-data-tail-start)
                 (mevedel-view--live-tail-valid-p data-buf)
                 (< beg (marker-position mevedel-view--live-data-tail-start)))
        (mevedel-view-render-invalidate-live-tail)))))

(defun mevedel-view--retain-last-live-render-unit (data-buf start end)
  "Retain the last live render unit between START and END for DATA-BUF."
  (let ((pos (1- end)) source)
    (while (and (>= pos start) (null source))
      (setq source (get-text-property pos 'mevedel-view-live-unit-source))
      (unless source
        (setq pos (1- (or (previous-single-property-change
                           (1+ pos) 'mevedel-view-live-unit-source nil start)
                          start)))))
    (if (not source)
        (mevedel-view-render-invalidate-live-tail)
      (let ((view-start pos))
        (while (and (> view-start start)
                    (equal source
                           (get-text-property
                            (1- view-start)
                            'mevedel-view-live-unit-source)))
          (setq view-start (previous-single-property-change
                            view-start 'mevedel-view-live-unit-source
                            nil start)))
        (mevedel-view-render-invalidate-live-tail)
        (unless mevedel-view--live-source-change-hook
          (let ((view-buf (current-buffer)))
            (setq mevedel-view--live-source-change-hook
                  (lambda (beg end)
                    (mevedel-view--note-source-change
                     view-buf (current-buffer) beg end)))))
        (let ((change-hook mevedel-view--live-source-change-hook))
          (setq mevedel-view--live-data-tail-start
                (with-current-buffer data-buf
                  (add-hook 'before-change-functions change-hook nil t)
                  (copy-marker source nil))))
        (setq mevedel-view--live-view-tail-start
              (copy-marker view-start nil))))))

(defun mevedel-view--split-live-response-tail (segments data-buf)
  "Split SEGMENTS at the last stable response block in DATA-BUF."
  (let ((reversed (reverse segments)) done out)
    (dolist (seg reversed out)
      (if (or done (not (eq (car seg) 'response)))
          (push seg out)
        (setq done t)
        (let ((boundary (mevedel-view--last-live-response-boundary
                         data-buf (cadr seg) (caddr seg))))
          (if boundary
              (progn
                (push (list 'response boundary (caddr seg)) out)
                (push (list 'response (cadr seg) boundary) out))
            (push seg out)))))))

(defun mevedel-view--rebase-data-sources (delta)
  "Shift rendered data-buffer source coordinates by DELTA."
  (unless (zerop delta)
    (mevedel-view-disclosure-rebase-state delta)
    ;; Property values are ordinary Lisp objects.  Mutating each shared
    ;; value once updates its coordinates without modifying the visible
    ;; buffer and forcing a frame-wide redisplay.
    (let ((seen-sources (make-hash-table :test #'eq))
          (seen-keys (make-hash-table :test #'eq))
          (limit (point-max))
          (pos (point-min)))
      (while (< pos limit)
        (let* ((source (get-text-property pos 'mevedel-view-source))
               (next (or (next-single-property-change
                          pos 'mevedel-view-source nil limit)
                         limit)))
          (when (and (consp source)
                     (not (gethash source seen-sources)))
            (puthash source t seen-sources)
            (when (integerp (car source))
              (setcar source (+ (car source) delta)))
            (when (integerp (cdr source))
              (setcdr source (+ (cdr source) delta))))
          (setq pos next)))
      (setq pos (point-min))
      (while (< pos limit)
        (let* ((key (get-text-property pos 'mevedel-view-source-key))
               (next (or (next-single-property-change
                          pos 'mevedel-view-source-key nil limit)
                         limit)))
          (when (and (consp key)
                     (not (gethash key seen-keys)))
            (puthash key t seen-keys)
            (when (and (eq (car key) 'source)
                       (integerp (nth 2 key)))
              (setcar (nthcdr 2 key) (+ (nth 2 key) delta))))
          (setq pos next))))
    (when (hash-table-p mevedel-view--tool-rendering-cache)
      (clrhash mevedel-view--tool-rendering-cache))))


(defun mevedel-view--debug-buffer ()
  "Return the view-render debug buffer, creating it when needed."
  (get-buffer-create mevedel-view-render-debug-buffer-name))

(defun mevedel-view-render-debug-enable (&optional clear)
  "Enable view-render debug tracing.
With prefix argument CLEAR, erase the trace buffer first."
  (interactive "P")
  (setq mevedel-view-render-debug t)
  (when clear
    (mevedel-view-render-debug-clear))
  (message "mevedel view render trace enabled (buffer: %s)"
           mevedel-view-render-debug-buffer-name))

(defun mevedel-view-render-debug-disable ()
  "Disable view-render debug tracing."
  (interactive)
  (setq mevedel-view-render-debug nil)
  (message "mevedel view render trace disabled"))

(defun mevedel-view-render-debug-clear ()
  "Erase the view-render debug trace buffer."
  (interactive)
  (with-current-buffer (mevedel-view--debug-buffer)
    (let ((inhibit-read-only t))
      (erase-buffer))))

(defun mevedel-view-render-debug-open ()
  "Open the view-render debug trace buffer."
  (interactive)
  (pop-to-buffer (mevedel-view--debug-buffer)))

(defun mevedel-view--debug-marker-position (marker)
  "Return MARKER's position, or nil when MARKER is not live."
  (and (markerp marker)
       (marker-buffer marker)
       (marker-position marker)))

(defun mevedel-view--debug-region (start end)
  "Return a compact plist describing START..END in the current buffer."
  (when (and start end (<= start end))
    (let* ((len (- end start))
           (limit 180)
           (preview-end (min end (+ start limit)))
           (preview
            (when (<= start preview-end)
              (buffer-substring-no-properties start preview-end))))
      (list :start start
            :end end
            :len len
            :preview
            (when preview
              (replace-regexp-in-string
               "\n" "\\\\n"
               (if (> len limit)
                   (concat preview "...")
                 preview)
               t t))))))

(defun mevedel-view--debug-spinner-state ()
  "Return a plist describing the current request-progress region."
  (let ((ov (mevedel-view-zone-region 'progress)))
    (cond
     ((not (overlayp ov)) nil)
     ((not (overlay-buffer ov)) '(:detached t))
     (t
      (let ((start (overlay-start ov))
            (end (overlay-end ov))
            (buf (overlay-buffer ov)))
        (append
         (list :buffer (buffer-name buf)
               :start start
               :end end
               :spinner-text-p
               (and (eq buf (current-buffer))
                    (mevedel-view--spinner-region-p start end)))
         (when (eq buf (current-buffer))
           (list :region (mevedel-view--debug-region start end)))))))))

(defun mevedel-view--debug-fragment-position (position)
  "Return managed-fragment coordinates for POSITION, or nil."
  (when (and (integer-or-marker-p position)
             (< position (point-max)))
    (when-let* ((bounds (mevedel-view-zone-bounds-at position)))
      (list :namespace (plist-get bounds :namespace)
            :id (plist-get bounds :id)
            :section (get-text-property
                      position 'mevedel-view-zone-section)
            :offset (- position (plist-get bounds :start))))))

(defun mevedel-view--debug-state (&optional data-buf start end)
  "Return a plist describing the current view-render state.
DATA-BUF, START, and END describe the data-buffer range being rendered."
  (when mevedel-view-render-debug
    (let* ((input (mevedel-view--debug-marker-position
                   mevedel-view--input-marker))
           (prompt-start (mevedel-view--prompt-start-position))
           (composer-start
            (and prompt-start
		 (next-single-property-change
                  prompt-start 'mevedel-view-prompt nil (point-max))))
           (window (and (eq (window-buffer (selected-window))
                            (current-buffer))
			(selected-window)))
           (window-point (and window (window-point window)))
           (window-start (and window (window-start window)))
           (status (mevedel-view--debug-marker-position
                    mevedel-view--status-marker))
           (interaction (mevedel-view--debug-marker-position
			 mevedel-view--interaction-marker))
           (in-flight (mevedel-view--debug-marker-position
                       mevedel-view--in-flight-turn-start))
           (data-start (mevedel-view--debug-marker-position
			mevedel-view--data-turn-start))
           (tail-end (or status input))
           (live-tail (and in-flight tail-end
                           (<= in-flight tail-end)
                           (mevedel-view--debug-region in-flight tail-end))))
      (list :view (buffer-name)
            :point (point)
            :point-input-offset (and input (>= (point) input)
                                     (- (point) input))
            :point-in-composer (and composer-start
                                    (>= (point) composer-start))
            :point-composer-offset (and composer-start
					(>= (point) composer-start)
					(- (point) composer-start))
            :point-fragment (mevedel-view--debug-fragment-position (point))
            :window-point window-point
            :window-input-offset (and input window-point
                                      (>= window-point input)
                                      (- window-point input))
            :window-point-in-composer (and composer-start window-point
                                           (>= window-point composer-start))
            :window-composer-offset (and composer-start window-point
					 (>= window-point composer-start)
					 (- window-point composer-start))
            :window-point-fragment
            (mevedel-view--debug-fragment-position window-point)
            :window-start window-start
            :window-start-fragment
            (mevedel-view--debug-fragment-position window-start)
            :point-max (point-max)
            :input input
            :status status
            :interaction interaction
            :in-flight in-flight
            :data-turn-start data-start
            :pending mevedel-view--pending-tool-calls
            :spinner (mevedel-view--debug-spinner-state)
            :live-tail live-tail
            :data-buffer (and (buffer-live-p data-buf)
                              (buffer-name data-buf))
            :data-start start
            :data-end end
            :data-point-max (and (buffer-live-p data-buf)
				 (with-current-buffer data-buf (point-max)))))))

(defun mevedel-view--debug-log (event &rest data)
  "Log EVENT and DATA when `mevedel-view-render-debug' is enabled."
  (when mevedel-view-render-debug
    (condition-case err
        (let ((print-length 80)
              (print-level 8))
          (with-current-buffer (mevedel-view--debug-buffer)
            (goto-char (point-max))
            (insert
             (format "[%s] %-24s %S\n"
                     (format-time-string "%H:%M:%S.%3N")
                     event
                     data))))
      (error
       (message "mevedel view render trace failed: %s"
                (error-message-string err))))))

(defun mevedel-view--debug-turn-summary (turns data-buf)
  "Return compact debug metadata for DATA-BUF.
TURNS is the list of rendered turn plists."
  (when mevedel-view-render-debug
    (mapcar
     (lambda (turn)
       (let* ((start (plist-get turn :start))
              (end (plist-get turn :end))
              (text
               (and (buffer-live-p data-buf)
                    start end
                    (with-current-buffer data-buf
                      (buffer-substring-no-properties
                       start (min end (+ start 120)))))))
         (list :role (plist-get turn :role)
               :start start
               :end end
               :preview (and text
                             (replace-regexp-in-string "\n" "\\\\n"
                                                       text t t)))))
     turns)))


;;
;;; Display properties and fontification


(defun mevedel-view--default-display-keymap (vtype)
  "Return the default display-region keymap for VTYPE."
  (if (eq vtype 'agent-handle)
      mevedel-view--agent-handle-map
    mevedel-view--display-map))

(defun mevedel-view-render-add-display-properties
    (start end &optional default-vtype)
  "Mark START..END read-only and attach default display keymaps.
Existing local `keymap' properties, such as transcript attribution
buttons, are preserved.  DEFAULT-VTYPE is used when a character has
no `mevedel-view-type' property yet.  Invisibility is rear-nonsticky so
decorating a collapsed card cannot hide its visible terminator from point."
  (add-text-properties start end
                       '(read-only t
                         front-sticky (read-only keymap)
                         rear-nonsticky
                         (read-only keymap line-prefix wrap-prefix invisible)))
  (let ((pos start))
    (while (< pos end)
      (let* ((keymap-next
              (or (next-single-property-change pos 'keymap nil end) end))
             (type-next
              (or (next-single-property-change
                   pos 'mevedel-view-type nil end)
                  end))
             (next (min keymap-next type-next)))
        (unless (get-text-property pos 'keymap)
          (put-text-property
           pos next 'keymap
           (mevedel-view--default-display-keymap
            (or (get-text-property pos 'mevedel-view-type)
                default-vtype))))
        (setq pos next)))))


(defun mevedel-view--set-hide-markdown-markup (symbol value)
  "Set SYMBOL to VALUE and re-render every view that already drew markup.
The `invisible\=' property is applied while fontifying rather than at
display time, so the reusable Markdown buffer and every cached rendering
predate the new setting and would otherwise keep the old look until the
session restarted."
  (let ((changed (not (eq (and (boundp symbol) (symbol-value symbol))
                          value))))
    (set-default symbol value)
    (when (and changed (fboundp 'mevedel-view--release-markdown-fontify-buffer))
      (mevedel-view--release-markdown-fontify-buffer)
      (dolist (buffer (buffer-list))
        (with-current-buffer buffer
          (when (derived-mode-p 'mevedel-view-mode)
            (when (hash-table-p mevedel-view--response-fontify-cache)
              (clrhash mevedel-view--response-fontify-cache)
              (setq-local mevedel-view--response-cache-entries 0))
            (when (hash-table-p mevedel-view--tool-rendering-cache)
              (clrhash mevedel-view--tool-rendering-cache))
            (when (buffer-live-p mevedel--data-buffer)
              (mevedel-view--full-rerender))))))))

(defun mevedel-view--markdown-grammar-hint ()
  "Warn once that Markdown view text stays unfontified, and how to fix it.
Without this the failure is silent: responses render as raw Markdown with
no indication that a missing grammar is the reason.  The hint is a user
affordance, so batch Emacs -- where nobody can act on it -- stays quiet."
  (when (and (not noninteractive)
             mevedel-view-fontify-responses
             (not (mevedel-view--markdown-grammars-ready-p)))
    (mevedel--warn-once
     'view-render-markdown-grammar
     "Markdown responses render unfontified: the `markdown' and \
`markdown-inline' tree-sitter grammars are not installed. \
Run `M-x markdown-ts-mode-install-parsers' to build them")))

(defun mevedel-view--visible-response-text (text)
  "Return response TEXT with model protocol hidden when appropriate."
  (let ((text (mevedel-view--strip-render-data-display-text text)))
    (when (mevedel-view--strip-proposed-plans-p text)
      (setq text (mevedel-plan-strip-proposed text)))
    (replace-regexp-in-string "^</?proposed_plan>[ \t]*\n?" "" text)))

(defun mevedel-view--render-cache-key (text)
  "Return a compact cache key for TEXT content."
  ;; `sxhash-equal' samples long strings: equal-length metadata edits can
  ;; collide even with matching length, prefix, and suffix checks.
  (secure-hash 'sha1 text))

(defun mevedel-view--source-position (pos)
  "Return POS as an integer so markers and integers key identically.
The targeted agent refresh passes markers where the full render passes
integers; a marker never `equal's its position, so the same segment
would otherwise occupy two cache entries."
  (if (markerp pos) (marker-position pos) pos))

(defun mevedel-view--cache-put (table key value counter-symbol)
  "Put VALUE in TABLE under KEY and bump COUNTER-SYMBOL.
Clear TABLE before adding beyond `mevedel-view-render-cache-max-entries'."
  (unless (gethash key table)
    (when (>= (symbol-value counter-symbol)
              mevedel-view-render-cache-max-entries)
      (clrhash table)
      (set counter-symbol 0))
    (set counter-symbol (1+ (symbol-value counter-symbol))))
  (puthash key value table)
  value)

(defun mevedel-view--fontify-response (text)
  "Return TEXT with view-safe Markdown face properties.
Returns normalized TEXT without faces when
`mevedel-view-fontify-responses' is nil or no Markdown mode is available.
MODE is resolved only to key the cache; the fontification itself goes
through the tag so it reuses `mevedel-view--markdown-fontify-target'.
Faces are stored as `font-lock-face' so they survive the view
buffer's font-lock refontification cycles."
  (let* ((start-time (float-time))
         (text (mevedel-view--visible-response-text text))
         (mode (mevedel-view--markdown-fontify-mode))
         (cache (and (hash-table-p mevedel-view--response-fontify-cache)
                     mevedel-view--response-fontify-cache))
         (key (and cache
                   (list :response
                         mevedel-view-fontify-responses
                         mode
                         (mevedel-view--render-cache-key text))))
         (cached (and key (or (and mevedel-view-render--prepared-responses
                                   (gethash key mevedel-view-render--prepared-responses))
                              (gethash key cache)))))
    (prog1
        (or cached
            (let ((rendered
                   (if (and mevedel-view-fontify-responses mode)
                       (condition-case err
                           ;; The tag, not MODE: only the tag routes to the
                           ;; reusable buffer, and responses are the hot path
                           ;; the reuse exists for.
                           (mevedel-view--fontify-as text 'markdown-mode)
                         (error
                          (mevedel--warn-once
                           'view-render-fontify
                           "Could not fontify response as Markdown: %s"
                           (error-message-string err))
                          text))
                     text)))
              (if key
                  (mevedel-view--cache-put cache key rendered
                                           'mevedel-view--response-cache-entries)
                rendered)))
      (mevedel-view--debug-log
       'fontify-response
       :chars (length text)
       :cached (and cached t)
       :elapsed (- (float-time) start-time)))))


;;
;;; Rendering state and agent helpers

(defmacro mevedel-view-render--preserving-window-state (&rest body)
  "Execute BODY while preserving point and window positions."
  (declare (indent 0) (debug t))
  `(mevedel-view--call-preserving-window-state
    (lambda () ,@body)))

(defun mevedel-view--running-agent-transcript-buffer-p ()
  "Return non-nil when the current buffer is a live agent transcript."
  (let ((inv mevedel--agent-invocation))
    (and (mevedel-agent-invocation-p inv)
         (eq (mevedel-agent-invocation-transcript-status inv)
             'running))))

(defun mevedel-view--group-into-turns (segments data-buf &optional state next)
  "Group SEGMENTS by conversation role.
A turn is a list of consecutive segments belonging to one role.
A new user segment starts a new turn.  Returns a list of turns,
where each turn is a plist (:role ROLE :segments SEGS :start S :end E).
ROLE is `user' or `assistant'.

DATA-BUF is the authoritative transcript buffer.

A segment classified as `user' (gptel property nil) starts a new turn
when it follows another `user' or `response' segment, when non-scaffolding
text follows a closed `reasoning' or `tool' segment, or when it contains a
retained `* Agent Task:' heading.  When it follows an `ignore' segment it is
reasoning text embedded in the assistant turn and is absorbed as such.

Additionally, a nil segment immediately after a `response' is
absorbed into the assistant turn when the next segment is `ignore'
or `tool' (mid-turn reasoning gap between response chunks), but only
when DATA-BUF shows that the segment is org scaffolding rather than a
real user message.

With NEXT, continue grouping later: NEXT is the following segment used for
lookahead at the end of SEGMENTS.  Return the resumable STATE rather than
the completed turns.  Pass that state into the next call.  A final call
without NEXT returns the complete turns."
  (let ((turns (plist-get state :turns))
        (current-segs (plist-get state :current-segs))
        (current-role (plist-get state :current-role))
        (turn-start (plist-get state :turn-start))
        (prev-type (plist-get state :prev-type))
        (rest segments))
    (while rest
      (let* ((seg (car rest))
             (type (car seg))
             (seg-start (cadr seg))
             (seg-end (caddr seg))
             (next-type (car-safe (or (cadr rest) next)))
             ;; One substring and one render-data extraction per segment
             ;; feed every classification below.  The predicate wrappers
             ;; used to extract the same span three to five times per
             ;; segment on every streaming frame, which dominated live
             ;; render allocation.  Properties are kept: the side-channel
             ;; extractors authorize blocks through text properties.
             (seg-text
              (and data-buf
                   (memq type '(user render-data ignored))
                   (with-current-buffer data-buf
                     (save-restriction
                       (widen)
                       (let* ((pmin (point-min))
                              (pmax (point-max))
                              (s (max pmin (min seg-start pmax)))
                              (e (max pmin (min seg-end pmax))))
                         (if (< s e) (buffer-substring s e) ""))))))
             (seg-render-data
              (and seg-text
                   (cdr (mevedel-tool-render-data-extract seg-text))))
             (seg-render-kind
              (and (consp seg-render-data)
                   (plist-get seg-render-data :kind)))
             (hook-audit-only-p
              (and (eq type 'ignored)
                   seg-text
                   (mevedel-transcript-audit-only-p seg-text)))
             (seg-scaffolding-only-p
              (and (memq type '(user ignored))
                   seg-text
                   ;; Audit-only segments already preserve the preceding
                   ;; type below.  Their scaffolding verdict is never used;
                   ;; do not clean the encoded payload to compute it.
                   (not hook-audit-only-p)
                   (mevedel-view--scaffolding-only-text-p seg-text)))
             (scaffolding-before-hook-audit-p
              (and (eq type 'user)
                   seg-scaffolding-only-p
                     (let ((following (or (cadr rest) next)))
                       (and (eq (car-safe following) 'ignored)
                          (mevedel-view--hook-audit-only-segment-p
                           data-buf (cadr following) (caddr following))))))
             (review-action-p
              (and (eq type 'user)
                   data-buf
                   (mevedel-view--review-action-segment-p
                    data-buf seg-start seg-end)))
             (agent-task-p
              (and (eq type 'user)
                   data-buf
                   (with-current-buffer data-buf
                     (save-excursion
                       (goto-char seg-start)
                       (let ((case-fold-search nil))
                         (re-search-forward
                          "^\\* Agent Task:" seg-end t))))))
             (system-reminder-p (eq type 'reminder))
             (inline-skill-render-p
              (eq seg-render-kind 'inline-skill))
             (request-summary-p
              (eq seg-render-kind 'request-summary))
             (render-data-only-p
              (and seg-text
                   (not (and (memq type '(render-data ignored))
                             (eq seg-render-kind 'collaboration-event)
                             (eq (plist-get seg-render-data :event)
                                 'started)))
                   (mevedel-view--render-data-only-text-p seg-text)))
             ;; Assistant-side glue with no turn before it has nothing to
             ;; join and nothing to draw.  Rewinding to before the first
             ;; turn leaves exactly this: leading whitespace still
             ;; carrying tool or audit properties.  An in-flight turn
             ;; always follows a user turn, so it is never dropped here.
             (leading-assistant-residue-p
              (and (null current-role)
                   (null turns)
                   (memq type '(ignored tool reasoning))
                   data-buf
                   (mevedel-view--blank-segment-p
                    data-buf seg-start seg-end))))
        (cond
         (review-action-p
          nil)
         (system-reminder-p
          (unless current-role
            (setq current-role 'assistant
                  turn-start seg-start))
          (push (list 'system-reminder seg-start (caddr seg)) current-segs))
         (request-summary-p
          (unless current-role
            (setq current-role 'assistant
                  turn-start seg-start))
          (push (list 'request-summary seg-start (caddr seg)) current-segs))
         (scaffolding-before-hook-audit-p
          nil)
         ((and (null current-role)
               (eq (plist-get (car turns) :role) 'user)
               (or (memq type '(prompt hook-context))
                   hook-audit-only-p
                   (and (eq type 'render-data)
                        (eq seg-render-kind 'user-display)
                        (stringp (plist-get seg-render-data :text)))
                   inline-skill-render-p))
          (let ((turn (car turns)))
            (setq turn
                  (plist-put turn :segments
                             (append (plist-get turn :segments)
                                     (list seg))))
            (setq turn (plist-put turn :end (caddr seg)))
            (setcar turns turn)))
         (render-data-only-p
          nil)
         (leading-assistant-residue-p
          nil)
         ((eq type 'task-background)
          (when current-segs
            (push (list :role current-role
                        :segments (nreverse current-segs)
                        :start turn-start
                        :end (caddr (car current-segs)))
                  turns))
          (push (list :role 'task-background
                      :segments (list seg)
                      :start seg-start
                      :end (caddr seg))
                turns)
          (setq current-segs nil current-role nil turn-start nil))
         ((and (eq type 'user)
               (or agent-task-p
                   (memq prev-type '(nil user response))
                   (and (memq prev-type '(reasoning tool))
                        data-buf
                        (not seg-scaffolding-only-p)))
               ;; Look-ahead: a scaffolding-only nil gap right after a
               ;; response is assistant-side glue.  Require DATA-BUF proof
               ;; so a real user prompt remains a user turn.
               (not (and (eq prev-type 'response)
                         (or (and (memq next-type
                                       '(reasoning ignored tool mailbox
                                         reminder render-data))
                                  seg-scaffolding-only-p)
                             (and (eq next-type 'response)
                                  data-buf
                                  seg-scaffolding-only-p)))))
          ;; Genuine user turn: either the first segment, follows a
          ;; user/response segment, or follows reasoning or tool activity.
          (progn
            ;; Flush any accumulated assistant turn
            (when current-segs
              (push (list :role current-role
                          :segments (nreverse current-segs)
                          :start turn-start
                          :end (caddr (car current-segs)))
                    turns))
            ;; Start a new user turn (single segment)
            (push (list :role 'user
                        :segments (list seg)
                        :start seg-start
                        :end (caddr seg))
                  turns)
            (setq current-segs nil current-role nil turn-start nil)))
         (t
          ;; Assistant-side segment (response, tool, ignore, pure
          ;; mailbox delivery, or reasoning text misclassified as user).
          (unless current-role
            (setq current-role 'assistant
                  turn-start seg-start))
          (push seg current-segs)))
        (setq prev-type
              (cond
               (system-reminder-p prev-type)
               (request-summary-p 'response)
               (hook-audit-only-p prev-type)
               (render-data-only-p prev-type)
               (leading-assistant-residue-p prev-type)
               ((and (eq type 'ignored) seg-scaffolding-only-p)
                prev-type)
               (scaffolding-before-hook-audit-p prev-type)
               (t type)))
        (setq rest (cdr rest))))
    (if next
        (list :turns turns :current-segs current-segs
              :current-role current-role :turn-start turn-start
              :prev-type prev-type)
      ;; Only the final slice closes an assistant turn.  Earlier slices keep
      ;; the segments reversed so adding to the current turn remains cheap.
      (when current-segs
        (push (list :role current-role
                    :segments (nreverse current-segs)
                    :start turn-start
                    :end (caddr (car current-segs)))
              turns))
      (nreverse turns))))

(defun mevedel-view--directive-boundary-segment-p (segment data-buf)
  "Return non-nil when SEGMENT is directive boundary audit data."
  (and (eq (car segment) 'ignored)
       (with-current-buffer data-buf
         (mevedel-transcript-audit-records
          (buffer-substring
           (cadr segment) (caddr segment))
          'directive-turn-boundary))))

(defvar-local mevedel-view--directive-ranges-cache nil
  "Memo for `mevedel-view--directive-ranges' in a data buffer.
A list (TICK ALLOW-OPEN RANGES): TICK is the buffer's modification tick
when RANGES were scanned and ALLOW-OPEN the flag the scan used.  The
scan walks the whole transcript, so an unmodified redraw must not
repeat it.")

(defun mevedel-view--directive-ranges (data-buf)
  "Return absolute directive transcript ranges in DATA-BUF.
Memoized on the buffer's modification tick.  `:render-id's stay stable
across unmodified redraws; consumers only need them unique per range
within one buffer state.  Each call returns fresh shallow copies so a
caller's `plist-put' cannot corrupt the memo."
  (with-current-buffer data-buf
    (let ((tick (buffer-modified-tick))
          (allow-open
           (and mevedel--current-request
                (mevedel-request-directive-uuid mevedel--current-request)))
          (cache mevedel-view--directive-ranges-cache))
      (unless (and cache
                   (equal (nth 0 cache) tick)
                   (equal (nth 1 cache) allow-open))
        (setq cache
              (list tick allow-open
                    (mapcar
                     (lambda (range)
                       (plist-put range :render-id
                                  (gensym "mevedel-view-directive-turn-")))
                     (mevedel-transcript-buffer-directive-ranges allow-open)))
              mevedel-view--directive-ranges-cache cache))
      (mapcar #'copy-sequence (nth 2 cache)))))

(defun mevedel-view--group-transcript-turns (segments data-buf)
  "Group SEGMENTS and annotate first-class directive turns in DATA-BUF."
  (let* ((ranges (mevedel-view--directive-ranges data-buf))
         (turns
          (mevedel-view--group-into-turns
           (cl-remove-if
            (lambda (segment)
              (mevedel-view--directive-boundary-segment-p segment data-buf))
            segments)
           data-buf)))
    (dolist (turn turns)
      (mevedel-view-render--annotate-directive-turn turn ranges))
    turns))

(defun mevedel-view-render--annotate-directive-turn (turn ranges)
  "Annotate TURN with its first overlapping directive in RANGES."
  (when-let* ((range
               (cl-find-if
                (lambda (candidate)
                  (and (< (plist-get turn :start)
                          (plist-get candidate :body-end))
                       (> (plist-get turn :end)
                          (plist-get candidate :body-start))))
                ranges)))
    (plist-put turn :directive range)
    (plist-put turn :render-id (plist-get range :render-id))))


;;
;;; Tool one-liner generation

(defun mevedel-view--tool-one-liner (data-buf seg-start seg-end)
  "Generate a one-line summary for a tool segment.
Reads the tool content from DATA-BUF between SEG-START and SEG-END,
parses the S-expression to extract tool name, and builds a summary.

Skips a leading `#+begin_tool …' / `#+end_reasoning' / blank-line
preamble before parsing so a segment whose start drifted into the
org-block scaffolding (incremental render boundary expansion or a
patched render-data block can shift the gptel-property run) still
produces a `Bash: …' / `Read: …' header instead of bare `Tool'."
  (with-current-buffer data-buf
    (let* ((raw (mevedel-view--tool-segment-text seg-start seg-end))
           (wrapped-p (mevedel-view--tool-wrapped-text-p raw))
           (text (substring raw (mevedel-view--tool-readable-start raw))))
      (condition-case nil
          (let* ((sexp (read text))
                 (name (plist-get sexp :name))
                 (args (plist-get sexp :args))
                 ;; Count result lines (text after the sexp)
                 (sexp-end (with-temp-buffer
                             (insert text)
                             (goto-char (point-min))
                             (forward-sexp 1)
                             (point)))
                 (result-text (string-trim (substring text sexp-end)))
                 (result-text
                  (if wrapped-p
                      (mevedel-view--strip-trailing-tool-marker
                       result-text)
                    result-text))
                 (result-text
                  (mevedel--strip-hook-audit-blocks result-text))
                 (result-lines (length (split-string result-text "\n" t)))
                 (primary-arg (mevedel-tool-display-string name args))
                 (blocked (mevedel-view--tool-hook-blocked-info
                           result-text))
                 (error-p (mevedel-view--tool-result-error-p result-text))
                 (summary
                  (mevedel-view--tool-summary-line
                   name primary-arg result-lines blocked error-p)))
            summary)
        (error
         ;; Fallback: show truncated raw text
         (mevedel-view--tool-fallback-line raw))))))

(defun mevedel-view--tool-hook-blocked-info (result-text)
  "Return hook blocking info parsed from RESULT-TEXT, or nil."
  (when (and (stringp result-text)
             (string-match
              "\\(?:Error:[ \t]*\\)?\\(?:Permission denied:[ \t]*\\)?blocked by \\(PreToolUse\\|PermissionRequest\\):[ \t]*\\(.+\\)"
              result-text))
    (list :event (match-string 1 result-text)
          :reason (string-trim (match-string 2 result-text)))))

(defun mevedel-view--read-args-media-p (args)
  "Return non-nil when Read ARGS identify a media-capable file."
  (when-let* ((path (plist-get args :file_path))
              ((stringp path))
              (ext (downcase (or (file-name-extension path) ""))))
    (member ext '("pdf" "png" "jpg" "jpeg" "gif" "webp"))))


;;
;;; Renderer plist interpreter

;; Tools can register a pure `renderer' function that consumes the `render-data'
;; side-channel attached to their result and returns a rendering plist of the
;; form:
;;
;;   (:header STRING            ; one-line collapsed summary
;;    :body STRING              ; full expanded body text
;;    :preview-body STRING      ; body shown when the row renders expanded
;;                              ; by default; explicit expansion shows :body
;;    :body-mode SYMBOL         ; major-mode symbol for fontification (or nil)
;;    :status SYMBOL            ; optional visual status, e.g. success/error
;;    :expandable-p BOOL        ; nil means render as a compact event line
;;    :hidden-p BOOL            ; non-nil means insert nothing
;;    :coalesce-key STRING      ; adjacent equal keys keep only the last row
;;    :child-calls LIST         ; nested calls rendered as their own rows
;;    :initially-collapsed-p BOOL)
;;
;; `:child-calls' belongs to compound tools that run other tools
;; (ToolCall).  Each entry is a plist (:id ID :tool NAME :args PLIST :result STRING :status SYM
;; :render-data DATA) and is rendered by that tool's own registered renderer,
;; so a nested Grep row gets Grep's header and `grep-mode' body.  Rows are
;; inserted only while the owning block is expanded, and each row is its own
;; collapsible section.
;;
;; The interpreter below parses the tool segment in the data buffer, invokes the
;; renderer (with a condition-case fallback to the generic renderer on error),
;; and inserts the rendered output. Expand and collapse re-invoke the renderer
;; on every transition so no state is cached in text properties.

(defvar mevedel-view-render--parse-checkpoint nil
  "Optional callback between source-preparation stages.
Preparation owners may validate their source and yield here.  Ordinary callers
leave this nil.  Registered renderers are never invoked at these checkpoints.")

(defmacro mevedel-view-render--parse-step (&rest body)
  "Evaluate BODY, then offer a source-preparation checkpoint."
  (declare (indent 0) (debug t))
  `(prog1 (progn ,@body)
     (when mevedel-view-render--parse-checkpoint
       (funcall mevedel-view-render--parse-checkpoint))))

(defun mevedel-view--tool-call-parse (data-buf seg-start seg-end &optional raw)
  "Parse the tool segment in DATA-BUF between SEG-START and SEG-END.
Return a plist (:name NAME :args ARGS :result STRING :render-data DATA)
or nil when the segment is not a well-formed tool block.

When RAW is non-nil, use it as the already-expanded segment text.

Skips any leading `#+begin_tool …' / `#+end_reasoning' / blank-line
scaffolding before reading the call sexp -- gptel writes the open
tool marker on its own line with no `gptel' property, so a segment
whose start drifted onto the marker (boundary expansion, patched
render-data block) would otherwise fail to parse and force the
renderer to fall back to the bare `Tool' one-liner."
  (with-current-buffer data-buf
    (let* ((raw (or raw
                    (mevedel-view--tool-segment-text seg-start seg-end)))
           (wrapped-p (mevedel-view--tool-wrapped-text-p raw))
           (read-start (mevedel-view--tool-readable-start raw))
           (tool-id
            (let ((pos seg-start)
                  found prop)
              (while (and (< pos seg-end) (not found))
                (setq prop (get-text-property pos 'gptel))
                (when (and (consp prop) (eq (car prop) 'tool))
                  (setq found (cdr prop)))
                (setq pos (or (next-single-property-change
                               pos 'gptel nil seg-end)
                              seg-end)))
              found)))
      (condition-case nil
          (let* ((parsed (read-from-string raw read-start))
                 (sexp (car parsed))
                 (name (plist-get sexp :name))
                 (args (plist-get sexp :args)))
            (when (stringp name)
              (let* ((full-result (mevedel-view-render--parse-step
                                   (mevedel--trim-tool-result (substring raw (cdr parsed)))))
                     (full-result
                      (if (and (derived-mode-p 'org-mode)
                               (fboundp 'org-unescape-code-in-string))
                          (mevedel-view-render--parse-step
                            (org-unescape-code-in-string full-result))
                        full-result))
                     (hook-audits
                      (pcase-let* ((bounds
                                    (mevedel-view--tool-block-bounds
                                     seg-start seg-end))
                                   (audit-start
                                    (min seg-start
                                         (or (car-safe bounds) seg-start)))
                                   (audit-end
                                    (max seg-end
                                         (or (cdr-safe bounds) seg-end))))
                        (save-excursion
                          (goto-char audit-start)
                          (when (search-forward mevedel--hook-audit-open audit-end t)
                            (mevedel-view--hook-audit-records-from-text
                             (buffer-substring audit-start audit-end)
                             nil data-buf audit-start)))))
                     (full-result
                      (mevedel--strip-hook-audit-blocks full-result))
                     (extract (mevedel-view-render--parse-step
                               (mevedel-tool-render-data-extract
                               full-result
                               (and (boundp 'mevedel--session)
                                    mevedel--session)
                               tool-id
                               (and (stringp tool-id)
                                    (not (string-empty-p tool-id))
                                    (equal name "Read")
                                    (mevedel-view--read-args-media-p args)))))
                     (visible-result (car extract)))
                (list :name name
                      :tool-use-id tool-id
                      :args args
                      :result (if wrapped-p
                                  (mevedel-view--strip-trailing-tool-marker
                                   visible-result)
                                visible-result)
                      :render-data (cdr extract)
                      :hook-audits hook-audits))))
        (error nil)))))

(defun mevedel-view--rendering-plist-p (p)
  "Return non-nil when P is a structurally valid rendering plist.
Requires:
  `:header'               -- a string (required).
  `:body' (if present)    -- must be a string.
  `:preview-body' (if present) -- must be a string.
  `:body-mode' (if present) -- must be a symbol.
  `:status' (if present) -- must be a symbol.
  `:expandable-p' (if present) -- must be a boolean.
  `:hidden-p' (if present) -- must be a boolean.
  `:coalesce-key' (if present) -- must be a string.
  `:child-calls' (if present) -- must be a list of plists.
Malformed plists are rejected here so the interpreter never tries to
insert a non-string or `funcall' a non-symbol."
  (and (listp p)
       (stringp (plist-get p :header))
       (let ((body (plist-get p :body))
             (preview (plist-get p :preview-body))
             (mode (plist-get p :body-mode))
             (status (plist-get p :status))
             (expandable (plist-get p :expandable-p))
             (hidden (plist-get p :hidden-p))
             (children (plist-get p :child-calls))
             (coalesce-key (plist-get p :coalesce-key)))
         (and (or (null body) (stringp body))
              (or (null preview) (stringp preview))
              (or (null children)
                  (and (listp children)
                       (seq-every-p (lambda (child)
                                      (and (listp child)
                                           (keywordp (car-safe child))))
                                    children)))
              (or (null mode) (symbolp mode))
              (or (not (plist-member p :status)) (symbolp status))
              (or (not (plist-member p :expandable-p))
                  (memq expandable '(nil t)))
              (or (not (plist-member p :hidden-p))
                  (memq hidden '(nil t)))
              (or (not (plist-member p :coalesce-key))
                  (stringp coalesce-key))))))

(defun mevedel-view--presentation-render-data (render-data)
  "Return RENDER-DATA with a successful observation's visual status.
The handler's error status is still sent to the model, including nested
ToolCall error values.  A collected process failure belongs to Bash, not
to the input or poll that successfully observed it."
  (if (and (memq (plist-get render-data :execution-control) '(input poll))
           (eq (plist-get render-data :control-succeeded-p) t))
      (plist-put (copy-sequence render-data) :status 'success)
    render-data))

(defun mevedel-view--permission-approval-reason (render-data)
  "Return the bounded reviewer explanation in canonical RENDER-DATA.
This is display-only provenance for this invocation, not permission authority.
Child approvals belong to child render data, never the enclosing call."
  (let ((records (plist-get render-data :permission-approvals)) reasons)
    (when (proper-list-p records)
      (dolist (record records)
        (when (and (proper-list-p record)
                   (eq (plist-get record :source) 'reviewer)
                   (eq (plist-get record :outcome) 'allow-once)
                   (stringp (plist-get record :reason)))
          (push (substring-no-properties (plist-get record :reason)) reasons))))
    (when reasons
      (truncate-string-to-width
       (string-join (delete-dups (nreverse reasons)) "\n") 1024 nil nil "…"))))

(defun mevedel-view--permission-approval-hint (reason)
  "Return a subdued approval hint with REASON as its tooltip, or nil."
  (when reason
    (propertize "Auto-approved · once"
                'font-lock-face 'mevedel-view-tool-metadata
                'help-echo reason)))

(defun mevedel-view--tool-render-status (result &optional render-data)
  "Return the renderer dispatch status for RESULT and RENDER-DATA."
  (setq render-data (mevedel-view--presentation-render-data render-data))
  (or (pcase (plist-get render-data :status)
        ('success 'success)
        ((or 'error 'failed 'denied 'blocked) 'error))
      (and (mevedel-view--tool-result-error-p result) 'error)
      'success))

(defun mevedel-view--renderer-for-status (renderer status)
  "Return renderer function from RENDERER for STATUS, or nil.

RENDERER may be a function or an alist mapping status symbols to
functions.  Alist lookup tries STATUS first, then `default'."
  (cond
   ((functionp renderer) renderer)
   ((listp renderer)
    (let ((fn (or (alist-get status renderer nil nil #'eq)
                  (alist-get 'default renderer nil nil #'eq))))
      (and (functionp fn) fn)))))

(defun mevedel-view--renderer-malformed-p (renderer status)
  "Return non-nil if RENDERER is present but unusable for STATUS."
  (cond
   ((null renderer) nil)
   ((functionp renderer) nil)
   ((listp renderer)
    (let ((entry (or (assoc status renderer)
                     (assoc 'default renderer))))
      (and entry (not (functionp (cdr entry))))))
   (t t)))

(defun mevedel-view--invoke-renderer (tool render-data args result)
  "Invoke TOOL's renderer with NAME, ARGS, RESULT, and RENDER-DATA.
Return the rendering plist, or nil when no renderer is registered, the
renderer returns nil (opt-out), the renderer signals an error, or the
returned plist fails `mevedel-view--rendering-plist-p'.  Errors and
malformed returns are surfaced once per tool via `mevedel--warn-once';
callers treat a nil return as \"use the generic tool renderer\".

The renderer receives RENDER-DATA as-is (possibly nil): data-driven
renderers like the ApplyPatch summary can check for their kind
and opt out; output-driven renderers (Grep, Bash, Read, ...) work
straight off ARGS and RESULT without needing render-data."
  (if-let* ((child (mevedel-tool-render-data-direct-call
                    (and tool (mevedel-tool-name tool)) render-data)))
      (let* ((name (plist-get child :tool))
             (underlying (mevedel-tool-for-call name))
             (data (plist-get child :render-data)))
        (or (mevedel-view--invoke-renderer underlying data
                                           (plist-get child :args) result)
            (mevedel-view--generic-tool-rendering
             name (plist-get child :args) result nil data)))
    (let* ((render-data (mevedel-view--presentation-render-data render-data))
           (explicit-status
            (and (memq (plist-get render-data :status) '(success error))
		 (plist-get render-data :status)))
           (renderer (and tool (mevedel-tool-renderer tool)))
           (status (mevedel-view--tool-render-status result render-data))
           (fn (and renderer
                    (mevedel-view--renderer-for-status renderer status))))
      (when renderer
	(let ((tool-label (or (and tool (mevedel-tool-name tool)) "tool")))
          (cond
           ((not fn)
            (when (mevedel-view--renderer-malformed-p renderer status)
              (mevedel--warn-once
               (list 'view-render-renderer-uncallable tool-label)
               "Renderer for %s is not callable for status %s"
               tool-label status))
            nil)
           (t
            (condition-case err
		(let ((plist (funcall fn tool-label args result render-data)))
                  (cond
                   ((null plist) nil)
                   ((mevedel-view--rendering-plist-p plist)
                    (when-let* ((reason (mevedel-view--permission-approval-reason
                                        render-data)))
                      (setq plist (plist-put (copy-sequence plist)
                                             :permission-approval-reason reason)))
                    (if (or (eq status 'error)
                            (and explicit-status
                                 (not (memq (plist-get plist :status)
                                            '(warning running cancelled)))))
			(plist-put (copy-sequence plist)
                                   :status status)
                      plist))
                   (t
                    (mevedel--warn-once
                     (list 'view-render-renderer-malformed tool-label)
                     "Renderer for %s returned malformed plist: %S"
                     tool-label plist)
                    nil)))
              (error
               (mevedel--warn-once
		(list 'view-render-renderer-failed tool-label)
		"Renderer for %s failed: %s"
		tool-label (error-message-string err))
               nil)))))))))

(defun mevedel-view--tool-result-line-count (result)
  "Return the number of non-empty lines in RESULT."
  (if (not (stringp result))
      0
    (let ((pos 0)
          (lines 0)
          (len (length result)))
      (while (< pos len)
        (let ((next (or (string-search "\n" result pos) len)))
          (unless (= pos next)
            (cl-incf lines))
          (setq pos (1+ next))))
      lines)))

(defun mevedel-view--generic-tool-rendering
    (name args result &optional collapsed-only render-data)
  "Return a generic rendering plist for parsed tool NAME, ARGS, and RESULT.
This is used for tools without a custom renderer, including third-party
and MCP-style tools that are not registered in mevedel's tool registry.
When COLLAPSED-ONLY is non-nil, omit the body from the returned plist.
RENDER-DATA may carry the pipeline's structured `:status'."
  (let* ((tool-name (or name "Tool"))
         (primary (and (listp args)
                       (condition-case nil
                           (mevedel-tool-display-string tool-name args)
                         (error nil))))
         (lines (mevedel-view--tool-result-line-count result))
         (status (mevedel-view--tool-render-status result render-data))
         (metadata (if (eq status 'error)
                       "error"
                     (format "%d %s" lines
                             (if (= lines 1) "line" "lines"))))
         (header (concat tool-name
                         (when (and primary
                                    (not (string-empty-p primary)))
                           (concat ": " primary))
                         (format " (%s)" metadata))))
    (list :header header
          :body (and (not collapsed-only) (stringp result) result)
          :body-mode nil
          :status status
          :permission-approval-reason
          (mevedel-view--permission-approval-reason render-data)
          :initially-collapsed-p t)))

(defun mevedel-view-data-buffer-major-mode ()
  "Return the major mode of the data buffer the view is attached to.

Use this from a tool renderer that wants to fontify its body in the
same flavor as the chat transcript.  Mevedel data buffers are
`org-mode' for gptel state and tool-result storage, while assistant
responses are stored as raw Markdown and converted only by the view's
response renderer.

Returns nil (verbatim) when no data buffer is attached, so
`mevedel-view--fontify-as' inserts the text without activating a mode."
  (when-let* ((data-buffer (mevedel-view-segments-display-buffer))
              ((buffer-live-p data-buffer)))
    (buffer-local-value 'major-mode data-buffer)))

(defun mevedel-view-collapse-by-height-p (body)
  "Return non-nil when BODY should render collapsed by default.

Compares BODY's line count against the current window's height scaled
by `mevedel-inline-preview-threshold'.  When the threshold is <= 0
always collapse; when no window is attached (batch callers, no view
displayed) never collapse so output remains inspectable.

Intended for tool renderers to compute their `:initially-collapsed-p'
flag without duplicating the heuristic."
  (let* ((lines (if (stringp body)
                    (length (split-string body "\n"))
                  0))
         (window (get-buffer-window (current-buffer)))
         (height (and window (window-height window)))
         (threshold (if (boundp 'mevedel-inline-preview-threshold)
                        mevedel-inline-preview-threshold
                      0.8)))
    (cond
     ((<= threshold 0) t)
     ((null height) nil)
     (t (> lines (* height threshold))))))

(defun mevedel-view--queue-origin-fingerprint (queue)
  "Return the ORIGIN values in QUEUE for render cache invalidation."
  (mapcar (lambda (entry)
            (plist-get entry :origin))
          queue))

(defun mevedel-view--session-render-state-fingerprint (session)
  "Return state from SESSION that can affect cached tool renderings.

Agent registry state is deliberately absent: agent handle status
reaches a rendering only through render-data blocks patched into the
transcript text, which the content term of the cache key already
invalidates, while registry activity changes on every agent tick and
would defeat the cache exactly when it matters.  New live-state
dependencies must either ride a text patch or clear
`mevedel-view--tool-rendering-cache' at the mutation point."
  (when session
    (list :permission-origins
          (mevedel-view--queue-origin-fingerprint
           (mevedel-session-permission-queue session))
          :plan-pending
          (and (mevedel-session-pending-plan-approval session) t))))

(defun mevedel-view--stamp-agent-handle (start end rendering)
  "Stamp START..END with handle properties from RENDERING."
  (when-let* ((agent-path (plist-get rendering :agent-path)))
    (add-text-properties
     start end
     `(mevedel-view-agent-path ,agent-path
       mevedel-view-agent-handle-p t
       mevedel-view-agent-status ,(plist-get rendering :agent-status)))))

(defun mevedel-view--rendering-header-face (rendering)
  "Return the face for RENDERING's visible header line."
  (cond
   ((eq (plist-get rendering :vtype) 'request-failure)
    'mevedel-view-handle-error)
   ((and (eq (or (plist-get rendering :vtype) 'tool-summary)
             'agent-handle)
         (eq (plist-get rendering :agent-status) 'running))
    'mevedel-view-agent-running)
   (t 'mevedel-view-tool-summary)))

(defun mevedel-view--buttonize-agent-header-label (line agent-path)
  "Return LINE with its visible AGENT-PATH made clickable.
AGENT-PATH is stored on the label so it opens the retained transcript."
  (when (and agent-path
             (string-match (regexp-quote agent-path) line))
    (add-text-properties
     (match-beginning 0) (match-end 0)
     `(font-lock-face link
       keymap ,mevedel-view--agent-label-map
       mouse-face highlight
       follow-link t
       help-echo "Open agent transcript"
       mevedel-view-agent-path ,agent-path)
     line))
  line)

(defun mevedel-view--split-rendering-tool-header (header)
  "Split renderer HEADER into (NAME ARG METADATA).
Return nil when HEADER is not a `Tool: argument' style line."
  (when (string-match "\\`\\([^:\n]+\\):[ \t]*\\(.+\\)\\'" header)
    (let ((name (match-string 1 header))
          (arg (match-string 2 header))
          metadata)
      (when (string-match
             "\\`\\(.*\\)[ \t]+\\(([^()\n]+)\\)\\'"
             arg)
        (setq metadata (match-string 2 arg))
        (setq arg (match-string 1 arg)))
      (list name arg metadata))))

(defun mevedel-view--rendering-status (rendering &optional render-data)
  "Return RENDERING's visual status, with errors preceding warnings.
RENDER-DATA supplies child status and sandbox facts before attachment.
Execution outcome remains independent of lifecycle and disclosure state."
  (let ((status (plist-get rendering :status))
        (render-data (mevedel-view--presentation-render-data render-data)))
    (cond
     ((or (memq status '(error failed denied blocked))
          (memq (plist-get render-data :status) '(error failed denied blocked)))
      'error)
     ((memq status '(running cancelled)) status)
     ((memq (plist-get render-data :status) '(warning running cancelled))
      (plist-get render-data :status))
     ((or (eq status 'warning)
          (eq (mevedel-execution-telemetry-sandbox-summary-class
               (or (plist-get rendering :sandbox-summary)
                   (plist-get render-data :sandbox-summary)))
              'warning))
      'warning)
     (t status))))

(defun mevedel-view--rendering-header-line (rendering)
  "Return the propertized collapsed header line for RENDERING."
  (let* ((vtype (or (plist-get rendering :vtype) 'tool-summary))
         (header (if (eq vtype 'tool-group)
                     (mevedel-view--fit-group-header
                      (or (plist-get rendering :header) "Tool") rendering)
                   (or (plist-get rendering :header) "Tool")))
         (status (plist-get rendering :agent-status))
         (tool-status (mevedel-view--rendering-status rendering))
         (agent-p (eq vtype 'agent-handle))
         (prompt-p (eq vtype 'prompt-summary))
         (marker (cond
                  ((eq vtype 'request-failure) "✗")
                  ((eq vtype 'tool-preparing) "…")
                  (prompt-p "◆")
                  ((eq tool-status 'error) "×")
                  ((eq tool-status 'cancelled) "●")
                  ((and (not agent-p) (eq tool-status 'running)) "●")
                  ((and agent-p (eq status 'running)) "●")
                  ((and agent-p (memq status '(blocked waiting))) "!")
                  ((and agent-p (memq status '(error failed))) "×")
                  ((and agent-p (eq status 'aborted)) "✗")
                  ((and agent-p (memq status '(incomplete nil))) "…")
                  ((and agent-p (eq status 'completed)) "✓")
                  (agent-p "›")
                  ((eq tool-status 'warning) "!")
                  (t "✓")))
         (marker-face (cond
                       ((eq tool-status 'cancelled)
                        'mevedel-view-tool-metadata)
                       ((equal marker "×") 'error)
                       ((member marker '("!" "✗"))
                        'mevedel-view-tool-warning)
                       ((string= marker "●")
                        'mevedel-view-agent-running)
                       ((member marker '("…" "›"))
                        'mevedel-view-tool-metadata)
                       (prompt-p
                        'mevedel-view-response-marker)
                       (t 'mevedel-view-tool-marker)))
         (tool-split
          (and (not agent-p)
               (mevedel-view--split-rendering-tool-header header))))
    (if (or (eq vtype 'system-reminder-summary)
            (plist-get rendering :reasoning-p))
        header
      (let ((line
             (if tool-split
                 (mevedel-view--tool-call-line
                  marker marker-face
                  (nth 0 tool-split)
                  (nth 1 tool-split)
                  (nth 2 tool-split))
               (mevedel-view--operation-line
                marker marker-face header nil nil
                (mevedel-view--rendering-header-face rendering)))))
        (when-let* ((agent-path (plist-get rendering :agent-path)))
          (setq line (mevedel-view--buttonize-agent-header-label
                      line agent-path)))
        (when-let* ((id (plist-get rendering :execution-control-id)))
          (setq line
                (concat line "  "
                        (propertize
                         "[Show result]"
                         'font-lock-face 'link
                         'mouse-face 'highlight
                         'help-echo "RET: open the controlled execution"
                         'mevedel-view-zone-activate
                         (lambda ()
                           (mevedel-view-audit-show-control-result id))))))
        (when-let* ((hint (mevedel-view--permission-approval-hint
                          (plist-get rendering :permission-approval-reason))))
          (setq line (concat line "  " hint)))
        (let* ((count (plist-get rendering :pending-count))
               (pending-failed (plist-get rendering :pending-failed))
               (settled-failed (plist-get rendering :failed-count))
               (failed (and (or pending-failed settled-failed)
                            (+ (or pending-failed 0) (or settled-failed 0)))))
          (cond
           ((or count pending-failed)
            (mevedel-view--running-header
             line count (plist-get rendering :pending-virtual) failed
             (plist-get rendering :failed-names)))
           ;; A settled group keeps the count its running form showed, so
           ;; settling neither unfolds nor reshapes it.
           (settled-failed
            (concat line
                    (propertize " · " 'font-lock-face 'mevedel-view-tool-metadata)
                    (propertize (mevedel-view--failed-label
                                 settled-failed
                                 (plist-get rendering :failed-names))
                                'font-lock-face 'error)))
           (t line)))))))

(defconst mevedel-view--failed-names-width 30
  "Most columns a folded group spends naming its failed tools.")

(defun mevedel-view--failed-label (count names)
  "Return `N failed' for COUNT failures, naming the failed tools in NAMES.
NAMES are shown once each, in order, and end in `…' past
`mevedel-view--failed-names-width' columns, so the folded row can still
say which call failed without growing past one line."
  (let ((label (format "%d failed" count))
        (names (delete-dups (copy-sequence (delq nil names)))))
    (if (not names)
        label
      (concat label ": "
              (truncate-string-to-width
               (string-join names ", ") mevedel-view--failed-names-width
               nil nil "…")))))

(defun mevedel-view--sandbox-summary-line (summary)
  "Return the durable disclosure line for material sandbox SUMMARY.
A `warning' class states that something the model asked for did not run.
A `note' class records the boundary the call actually ran with, which is
the boundary the session was configured or granted -- nothing went wrong,
so it takes the quiet marker and metadata face instead of shouting on
every row."
  (when-let* ((class
               (and summary
                    (mevedel-execution-telemetry-sandbox-summary-class
                     summary)))
              (face (if (eq class 'note)
                        'mevedel-view-tool-metadata
                      'mevedel-view-tool-warning)))
    (let* ((notep (eq class 'note))
           (attempts (or (plist-get summary :attempt-count) 0))
           (started (or (plist-get summary :started-count) 0))
           (refused (or (plist-get summary :refused-count) 0))
           (writes (or (plist-get summary :additional-write-count) 0))
           (sandbox (plist-get summary :sandbox))
           (filesystem (plist-get summary :filesystem))
           (network (plist-get summary :network))
           (proc (plist-get summary :proc))
           (all-refused (and (> attempts 0)
                             (= refused attempts)
                             (zerop started)))
           details)
      (if all-refused
          (setq details '("execution refused" "no child started"))
        (pcase sandbox
          ('escalated (push "full execution access" details))
          ('off (push "sandbox disabled · ran without confinement" details))
          ('unavailable
           (push "sandbox unavailable · ran without confinement" details))
          ('refused (push "execution refused" details))
          ((pred (lambda (value)
                   (and value (not (eq value 'bubblewrap)))))
           (push (format "sandbox %s" sandbox) details)))
        (unless (memq sandbox '(escalated off unavailable))
          (when (and filesystem (not (eq filesystem 'workspace-write)))
            (push (pcase filesystem
                    ('unrestricted "unrestricted filesystem access")
                    ('unavailable "filesystem confinement unavailable")
                    (_ (format "filesystem %s" filesystem)))
                  details))
          (when (and network (not (eq network 'isolated)))
            (push (pcase network
                    ('unrestricted "network access allowed")
                    ('unavailable "network confinement unavailable")
                    (_ (format "network %s" network)))
                  details))
          (when (eq proc 'host)
            (push "host /proc access" details))
          (when (> writes 0)
            (push "additional filesystem write access" details)))
        (when (< started attempts)
          (push (format "%d %s did not start"
                        (- attempts started)
                        (if (= (- attempts started) 1)
                            "child"
                          "children"))
                details))
        (setq details (nreverse details)))
      (mevedel-view--operation-line
       (if notep "\u25c7" "!")
       face
       "Sandbox:"
       (string-join details " · ")
       nil
       'mevedel-view-tool-metadata))))

(defun mevedel-view--rendering-header-block (rendering)
  "Return RENDERING's header plus any durable sandbox disclosure."
  (let ((header (mevedel-view--rendering-header-line rendering))
        (sandbox
         (mevedel-view--sandbox-summary-line
          (plist-get rendering :sandbox-summary))))
    (let ((block (if sandbox (concat header "\n" sandbox) header)))
      ;; A collapsed group still projects the completions folded into it.
      (if-let* ((records (plist-get rendering :execution-completions)))
          (propertize block 'mevedel-view-execution-completions records)
        block))))

(defvar mevedel-view--rendering-indent ""
  "Line prefix applied to the rendering currently being inserted.
Bound to a non-empty string while a nested call row is inserted so its
header and body sit one level in from the block that ran it.")

(defun mevedel-view--rendering-indent-blank ()
  "Return the current rendering indent as plain spaces.
A batch glyph belongs on the first line of a row only, so wrapped text
and body lines are inset by the same width without repeating it."
  (make-string (string-width mevedel-view--rendering-indent) ?\s))

(defun mevedel-view--apply-rendering-indent (start end)
  "Indent START..END by `mevedel-view--rendering-indent' when it is set."
  (unless (string-empty-p mevedel-view--rendering-indent)
    (add-text-properties
     start end
     `(line-prefix ,mevedel-view--rendering-indent
       wrap-prefix ,(mevedel-view--rendering-indent-blank)
       rear-nonsticky (line-prefix wrap-prefix)))))

(defun mevedel-view--render-collapsed-header (rendering source)
  "Insert the collapsed header for RENDERING with SOURCE coordinates.
RENDERING is a rendering plist.  SOURCE is (DATA-START . DATA-END)."
  (let* ((vtype (or (plist-get rendering :vtype) 'tool-summary))
         (ins-start (point)))
    (mevedel-view--insert-summary-region
     (mevedel-view--rendering-header-block rendering)
     `(mevedel-view-type ,vtype
       mevedel-view-collapsed t
       mevedel-view-source ,source
       mevedel-view-source-key ,(mevedel-view-disclosure-state-key
                                 source vtype)
       mevedel-view-tool-use-id ,(plist-get rendering :tool-use-id)
       mevedel-view-rendered t))
    (when (eq vtype 'agent-handle)
      (mevedel-view--stamp-agent-handle ins-start (point) rendering))
    (mevedel-view--decorate-markdown-in-range ins-start (point))
    (mevedel-view--apply-rendering-indent ins-start (point))))

(defun mevedel-view--render-expanded-body (rendering source)
  "Insert the expanded body for RENDERING with SOURCE coordinates."
  (let* ((body (or (plist-get rendering :body) ""))
         (body-mode (plist-get rendering :body-mode))
         (vtype (or (plist-get rendering :vtype) 'tool-summary))
         (fontified (mevedel-view--fontify-as body body-mode))
         (header-line (mevedel-view--rendering-header-block rendering))
         (ins-start (point))
         body-start)
    (insert header-line "\n")
    (when (eq vtype 'agent-handle)
      (mevedel-view--stamp-agent-handle ins-start (point) rendering))
    (setq body-start (copy-marker (point)))
    (insert fontified)
    (when (memq vtype '(tool-summary tool-child))
      (mevedel-view-audit-insert-history-link rendering))
    (unless (eq (char-before) ?\n)
      (insert "\n"))
    (add-text-properties ins-start (point)
                         `(mevedel-view-type ,vtype
                           mevedel-view-collapsed nil
                           mevedel-view-source ,source
                           mevedel-view-source-key ,(mevedel-view-disclosure-state-key
                                                     source vtype)
                           mevedel-view-tool-use-id
                           ,(plist-get rendering :tool-use-id)
                           mevedel-view-force-expanded
                           ,(and (plist-get rendering :force-expanded-p) t)
                           mevedel-view-rendered t))
    ;; Stamp the body inset before decoration so a rendered table
    ;; subtracts it from its usable width, then again afterwards so
    ;; decoration-inserted text (panel labels, padding) is indented too.
    (let ((inset (concat (mevedel-view--rendering-indent-blank) "    ")))
      (add-text-properties body-start (point)
                           `(line-prefix ,inset
                             wrap-prefix ,inset
                             rear-nonsticky (line-prefix wrap-prefix)))
      (mevedel-view--decorate-markdown-in-range ins-start (point))
      (add-text-properties body-start (point)
                           `(line-prefix ,inset
                             wrap-prefix ,inset
                             rear-nonsticky (line-prefix wrap-prefix))))
    (mevedel-view--apply-rendering-indent ins-start body-start)
    (set-marker body-start nil)
    ;; Nested call rows follow the body so a compound tool's children are
    ;; visible exactly while the block that ran them is expanded.
    (mevedel-view--insert-child-calls rendering source)))

(defun mevedel-view--insert-rendered-tool (rendering source)
  "Insert a rendered tool block honouring RENDERING's initial state.
SOURCE is (DATA-START . DATA-END) identifying the data-buffer segment.
When `:initially-collapsed-p' is nil the body is inserted expanded;
otherwise only the header is shown.

When RENDERING carries `:expandable-p' nil, insert a compact event line
with no source coordinates so expand/collapse commands cannot reveal
the raw tool segment.  When `:hidden-p' is non-nil, insert nothing."
  (unless (plist-get rendering :hidden-p)
    (let ((hook-audits (plist-get rendering :hook-audits)))
      (setq rendering
            (mevedel-view-disclosure-apply-rendering-state rendering source))
      (if (and (plist-member rendering :expandable-p)
               (not (plist-get rendering :expandable-p)))
          (let ((ins-start (point)))
            (mevedel-view--insert-summary-region
             (mevedel-view--rendering-header-block
              (plist-put (copy-sequence rendering) :vtype 'tool-event))
             '(mevedel-view-type tool-event
               mevedel-view-rendered t))
            (mevedel-view-render-add-display-properties
             ins-start (point) 'tool-event)
            (mevedel-view--decorate-markdown-in-range ins-start (point)))
        (if (plist-member rendering :initially-collapsed-p)
            (if (plist-get rendering :initially-collapsed-p)
                (mevedel-view--render-collapsed-header rendering source)
              ;; `:preview-body' replaces `:body' for this initial
              ;; expansion only.  A later explicit expansion goes
              ;; through the disclosure path, which re-invokes the
              ;; renderer and inserts the complete `:body'.
              (mevedel-view--render-expanded-body
               (if-let* ((preview (plist-get rendering :preview-body)))
                   (plist-put (copy-sequence rendering) :body preview)
                 rendering)
               source))
          ;; Default: collapsed.
          (mevedel-view--render-collapsed-header rendering source)))
      (when hook-audits
        (let ((audit-start (point)))
          (dolist (audit hook-audits)
            (mevedel-view--insert-hook-audit-block
             audit (or (plist-get audit :source) source)))
          (mevedel-view-render-add-display-properties
           audit-start (point) 'hook-audit))))))

(defconst mevedel-view--child-call-indent "  "
  "Extra line prefix that sets a nested call row in from its own block.")

(defvar mevedel-view--child-call-depth 0
  "Nesting depth of the child call currently being rendered.")

(defun mevedel-view--child-call-rendering (child)
  "Return the rendering plist for nested call CHILD, or nil.

CHILD is one `:child-calls' entry.  The nested tool's own registered
renderer produces the row, which is why a nested Grep gets Grep's header
and `grep-mode' body without the compound tool formatting anything
itself.  Reasoning and mailbox children carry their prepared rendering.
Tool rows start collapsed, including failures.

A nested compound call keeps its own `:child-calls', so expanding it
inside a block or an activity group shows the calls it ran rather than
the text it returned.  A row's sandbox disclosure travels with it, so
folding a run into a group does not lose the boundary it ran with."
  (let* ((name (plist-get child :tool))
         (args (plist-get child :args))
         (data (mevedel-view-segments-display-buffer))
         (direct (mevedel-tool-render-data-direct-call
                  name (plist-get child :render-data)))
         (child-id (or (and (member name '("Bash" "Monitor"))
                            (stringp (plist-get child :id))
                            (plist-get child :id))
                       (and (member (plist-get direct :tool) '("Bash" "Monitor"))
                            (stringp (plist-get direct :id))
                            (plist-get direct :id))))
         (terminal (and child-id
                        (mevedel-view--execution-terminal-render-data
                         data child-id
                         (plist-get (or (plist-get direct :render-data)
                                        (plist-get child :render-data))
                                    :execution-id))))
         (event (and (not terminal) child-id
                     (bound-and-true-p mevedel-view--execution-events)
                     (hash-table-p mevedel-view--execution-events)
                     (gethash child-id mevedel-view--execution-events)))
         (progress (and (eq (plist-get event :type) 'progress)
                        (append (copy-sequence (plist-get event :facts))
                                (list :permission-approvals
                                      (plist-get event :permission-approvals)
                                      :status 'success :live-execution-p t
                                      :output-preview-truncated-p
                                      (plist-get event :output-preview-truncated-p)))))
         (latest (or terminal progress))
         (result (cond
                  ((plist-member latest :execution-output)
                   (or (plist-get latest :execution-output) ""))
                  (progress (or (plist-get event :output-tail) ""))
                  ((plist-member (plist-get direct :render-data)
                                 :execution-output)
                   (or (plist-get (plist-get direct :render-data)
                                  :execution-output) ""))
                  ((and (member name '("Bash" "Monitor"))
                        (plist-member (plist-get child :render-data)
                                      :execution-output))
                   (or (plist-get (plist-get child :render-data)
                                  :execution-output) ""))
                  (t (plist-get child :result))))
         (child-status (plist-get child :status))
         (render-data
          (cond ((and latest direct)
                 (plist-put
                  (copy-sequence (plist-get child :render-data)) :calls
                  (list (plist-put
                         (plist-put (copy-sequence direct)
                                    :render-data latest)
                         :status (plist-get latest :status)))))
                (latest latest)
                ((and (not (memq (plist-get (plist-get child :render-data)
                                            :state)
                                 '(completed interrupted lost archived)))
                      (memq child-status '(success error failed denied blocked)))
                 (plist-put (copy-sequence (plist-get child :render-data)) :status
                            (if (eq child-status 'success) 'success 'error)))
                (t (plist-get child :render-data))))
         (tool (and (stringp name) (mevedel-tool-get name)))
         (rendering
          (if (memq (plist-get child :kind) '(reasoning mailbox completion))
              (plist-get child :rendering)
            (or (and tool
                     (mevedel-view--invoke-renderer tool render-data args result))
                (mevedel-view--generic-tool-rendering
                 name args result nil render-data)))))
    (when rendering
      (setq rendering (copy-sequence rendering))
      (when (eq (plist-get child :kind) 'reasoning)
        (setq rendering (plist-put rendering :reasoning-p t)))
      (dolist (cell (list (cons :vtype 'tool-child)
                          (cons :expandable-p t)
                          (cons :coalesce-key nil)
                          (cons :hook-audits nil)
                          (cons :force-expanded-p nil)
                          (cons :initially-collapsed-p t)))
        (setq rendering (plist-put rendering (car cell) (cdr cell))))
      (when child-id
        (setq rendering (plist-put rendering :tool-use-id child-id))
        (setq rendering (plist-put rendering :execution-id
                                  (or (plist-get latest :execution-id)
                                      (plist-get render-data :execution-id)
                                      (plist-get (plist-get direct :render-data)
                                                 :execution-id)))))
      (when (and (not latest) (memq child-status '(warning running cancelled))
                 (not (eq (mevedel-view--rendering-status rendering) 'error)))
        (setq rendering (plist-put rendering :status child-status)))
      (when-let* ((summary
                   (or (plist-get render-data :sandbox-summary)
                       (plist-get (plist-get direct :render-data)
                                  :sandbox-summary)
                       (plist-get (plist-get child :render-data)
                                  :sandbox-summary))))
        (setq rendering
              (plist-put rendering :sandbox-summary (copy-tree summary))))
      (when-let* ((media (plist-get child :media)))
        (setq rendering
              (plist-put rendering :body
                         (concat (or (plist-get rendering :body) "")
                                 (format "\nMedia: %S" media)))))
      rendering)))

(defun mevedel-view--child-call-state-key (child source)
  "Return the disclosure key for CHILD's row under SOURCE, or nil.
Grouped transcript rows keep their standalone identity.  Compound-tool
children use their id to discriminate one block's shared coordinates."
  (if-let* ((own-source (plist-get child :source)))
      (mevedel-view-disclosure-state-key
       own-source (pcase (plist-get child :kind)
                    ('reasoning 'thinking-summary)
                    ('mailbox 'tool-child)
                    (_ 'tool-summary)))
    (when-let* ((base (mevedel-view-disclosure-state-key source 'tool-child))
                (id (plist-get child :id)))
      (append base (list id)))))

(defun mevedel-view--child-call-prefixes (children)
  "Return one line prefix per entry in CHILDREN, or nil.

A `:batch' value marks the calls of one concurrent join.  When some call
ran in a batch every row gets a two-column glyph gutter so the join is
visible as a group and every marker still lines up; a block that ran
everything in sequence pays no gutter at all."
  (let ((batches (mapcar (lambda (child) (plist-get child :batch)) children))
        previous)
    (when (cl-some #'identity batches)
      (cl-loop
       for (batch next) on batches
       collect
       (concat
        mevedel-view--child-call-indent
        (if (null batch)
            "  "
          (let ((first (not (equal batch previous)))
                (last (not (equal batch next))))
            (concat (cond ((and first last) "\u2500")
                          (first "\u250c")
                          (last "\u2514")
                          (t "\u251c"))
                    " "))))
       do (setq previous batch)))))

(defun mevedel-view--insert-child-call-block
    (child source &optional collapsed indent)
  "Insert the nested call row for CHILD under SOURCE.

COLLAPSED is `derive' to use the remembered state, or a boolean to force
one.  INDENT overrides the row's line prefix, which is how a concurrent
batch keeps its glyph gutter across a toggle.  The row gets its own copy
of SOURCE's coordinates because section bounds compare source identity
with `eq', which is what separates one row from the next and from the
body of the block that ran them."
  (if (eq (plist-get child :kind) 'pending)
      ;; An in-flight call has no source or result: it is a running row
      ;; with no disclosure of its own.
      (let ((mevedel-view--rendering-indent
             (concat mevedel-view--rendering-indent
                     (or indent mevedel-view--child-call-indent))))
        (mevedel-view--insert-pending-tool-row (plist-get child :entry)))
    (mevedel-view--insert-settled-child-call-block
     child source collapsed indent)))

(defun mevedel-view--insert-settled-child-call-block
    (child source collapsed indent)
  "Insert CHILD's nested row under SOURCE; see the caller for COLLAPSED, INDENT."
  (when-let* ((rendering (mevedel-view--child-call-rendering child))
              ((not (plist-get rendering :hidden-p)))
              (source (or (plist-get child :source) source))
              (source (and (consp source) (cons (car source) (cdr source)))))
    (let* ((key (mevedel-view--child-call-state-key child source))
           (remembered (and (eq collapsed 'derive)
                            (mevedel-view-disclosure-state-for-key key)))
           (mailbox-default-p
            (and (plist-get rendering :mailbox-text)
                 (eq collapsed 'derive) (not remembered)))
           (collapsed (cond
                       (remembered (cdr remembered))
                       ((eq collapsed 'derive)
                        (plist-get rendering :initially-collapsed-p))
                       (t collapsed)))
           (indent (or indent mevedel-view--child-call-indent))
           (mevedel-view--rendering-indent
            (concat mevedel-view--rendering-indent indent))
           (mevedel-view--child-call-depth (1+ mevedel-view--child-call-depth))
           (start (point)))
      (cond
       ((plist-get rendering :completion-record)
        (let ((record (plist-get rendering :completion-record)))
          (mevedel-view--insert-hook-audit-block record source)
          (add-text-properties
           start (point)
           `(mevedel-view-type tool-child
                               mevedel-view-source ,source
                               mevedel-view-collapsed ,collapsed))
          (mevedel-view--apply-rendering-indent start (point))))
       ((plist-get rendering :mailbox-text)
          (let ((text (plist-get rendering :mailbox-text))
                (mevedel-view-mailbox-collapse-line-threshold
                 (cond (mailbox-default-p
                        mevedel-view-mailbox-collapse-line-threshold)
                       (collapsed 0)
                       (t most-positive-fixnum))))
            (insert text "\n")
            (mevedel-view--decorate-agent-result-blocks start (point))
            (mevedel-view--decorate-agent-message-blocks start (point))
            ;; Notices default to collapsed independently of the line threshold.
            ;; An explicit or remembered expansion must override that default.
            (when (and (not mailbox-default-p) (not collapsed)
                       (get-text-property start 'mevedel-view-collapsed))
              (save-excursion
                (goto-char start)
                (mevedel-view-disclosure--toggle-mailbox)))
            (when mailbox-default-p
              (setq collapsed
                    (get-text-property
                     (or (text-property-any start (point)
                                            'mevedel-view-type 'mailbox-delivery)
                         start)
                     'mevedel-view-collapsed)))
            (add-text-properties
             start (point)
             `(mevedel-view-type tool-child
                                 mevedel-view-source ,source
                                 mevedel-view-collapsed ,collapsed))))
       (collapsed
        (mevedel-view--render-collapsed-header rendering source))
       (t
        (mevedel-view--render-expanded-body rendering source)))
      ;; Descendants have their own source identity and disclosure state.
      ;; Stamp only this row's body, even when it contains a compound call.
      ;; Retain the full prefix so a row-local toggle keeps its parent inset.
      (add-text-properties
       start (next-single-property-change start 'mevedel-view-source nil (point))
       `(mevedel-view-tool-child ,child
         mevedel-view-child-indent ,mevedel-view--rendering-indent
         mevedel-view-child-depth ,mevedel-view--child-call-depth
         mevedel-view-source-key ,key))
      (mevedel-view-render-add-display-properties start (point) 'tool-child)
      (mevedel-view-disclosure-record-state-for-key key collapsed))))

(defun mevedel-view--insert-child-calls (rendering source)
  "Insert one row per nested call in RENDERING under SOURCE."
  (let* ((children (plist-get rendering :child-calls))
         (prefixes (mevedel-view--child-call-prefixes children)))
    (dolist (child children)
      (mevedel-view--insert-child-call-block
       child source 'derive (pop prefixes)))))

(defun mevedel-view-render-child-calls-end (start limit source &optional parent-depth)
  "Return the end of the nested call rows that begin at START, before LIMIT.
Return START when no owned row begins there.  Only rows contained in SOURCE
belong to the section: an independent mailbox delivery can also have the
`tool-child' type.  With PARENT-DEPTH, stop before any row at that depth or
shallower, preserving siblings of a nested compound call."
  (let ((pos start))
    (while (and (< pos limit)
                (eq (get-text-property pos 'mevedel-view-type) 'tool-child)
                (let ((child-source (get-text-property pos 'mevedel-view-source)))
                  (and (consp child-source)
                       (<= (car source) (car child-source))
                       (<= (cdr child-source) (cdr source))))
                (or (null parent-depth)
                    (> (get-text-property pos 'mevedel-view-child-depth)
                       parent-depth)))
      (setq pos (or (next-single-property-change
                     pos 'mevedel-view-source nil limit)
                    limit)))
    pos))

(defun mevedel-view-render-section-body-end (start limit)
  "Return the end of the section body in START..LIMIT.
Stops before any nested call rows so re-stamping a re-rendered section
does not overwrite the identity of the rows it owns."
  (let ((pos start))
    (while (and (< pos limit)
                (not (eq (get-text-property pos 'mevedel-view-type)
                         'tool-child)))
      (setq pos (or (next-single-property-change
                     pos 'mevedel-view-type nil limit)
                    limit)))
    pos))

(defun mevedel-view-render-toggle-child-call ()
  "Toggle the nested call row at point."
  (let* ((bounds (mevedel-view-disclosure-section-bounds))
         (start (car-safe bounds))
         (child (and start (get-text-property start 'mevedel-view-tool-child)))
         (source (and start (get-text-property start 'mevedel-view-source)))
         (collapsed (and start (get-text-property start 'mevedel-view-collapsed)))
         (indent (and start (get-text-property start 'mevedel-view-child-indent)))
         (depth (and start (get-text-property start 'mevedel-view-child-depth)))
         (turn-id (and start (get-text-property start 'mevedel-view-turn-id))))
    (unless (and bounds child)
      (user-error "No collapsible section at point"))
    (let ((inhibit-read-only t))
      (save-excursion
        (goto-char start)
        (delete-region start (mevedel-view-render-child-calls-end
                              (cdr bounds) (point-max) source depth))
        (let ((mevedel-view--child-call-depth (1- depth))
              (mevedel-view--rendering-indent ""))
          (mevedel-view--insert-child-call-block
           child source (not collapsed) indent))
        (when turn-id
          (put-text-property start (point) 'mevedel-view-turn-id turn-id))))))


(defvar-local mevedel-view--source-revisions nil
  "Bounded map of source ranges to disposable rendering identities.")

(defvar-local mevedel-view--source-revision-tick nil
  "Character modification tick accounted for by source revision tracking.")

(defvar-local mevedel-view--source-change-start nil
  "Beginning of an observed source edit awaiting its after-change hook.")

(defun mevedel-view--source-before-change (beg _end)
  "Record an upcoming source modification at BEG.
Discard identities if an earlier character modification escaped the hooks."
  (when mevedel-view--source-revisions
    (unless (eql mevedel-view--source-revision-tick (buffer-chars-modified-tick))
      (clrhash mevedel-view--source-revisions))
    (setq mevedel-view--source-revision-tick (buffer-chars-modified-tick)
          mevedel-view--source-change-start beg)))

(defun mevedel-view--source-after-change (beg _end _old-length)
  "Account for a source modification beginning at BEG.
Character changes invalidate ranges at or after BEG.  Property-only changes
leave character identities intact; rendering keys inspect provenance directly."
  (when mevedel-view--source-revisions
    (unless (eql mevedel-view--source-revision-tick (buffer-chars-modified-tick))
      (if (null mevedel-view--source-change-start)
          (clrhash mevedel-view--source-revisions)
        (let ((start (min beg mevedel-view--source-change-start)))
          (maphash (lambda (range _identity)
                     (when (> (cdr range) start)
                       (remhash range mevedel-view--source-revisions)))
                   mevedel-view--source-revisions))))
    (setq mevedel-view--source-revision-tick (buffer-chars-modified-tick)
          mevedel-view--source-change-start nil)))

(defun mevedel-view--source-revision (start end)
  "Return a character identity for the current buffer's range START..END.
Any unobserved character modification, including one made with hooks inhibited,
discards identities before reuse.  No source text is copied or retained.
Property provenance is checked separately by the rendering key."
  (unless (and mevedel-view--source-revisions
               (eql mevedel-view--source-revision-tick (buffer-chars-modified-tick))
               (memq #'mevedel-view--source-before-change before-change-functions)
               (memq #'mevedel-view--source-after-change after-change-functions))
    (setq mevedel-view--source-revisions (make-hash-table :test #'equal)
          mevedel-view--source-revision-tick (buffer-chars-modified-tick)
          mevedel-view--source-change-start nil)
    (add-hook 'before-change-functions #'mevedel-view--source-before-change nil t)
    (add-hook 'after-change-functions #'mevedel-view--source-after-change nil t))
  (let ((range (cons start end)))
    (or (gethash range mevedel-view--source-revisions)
        (puthash range (make-symbol "source-revision")
                 mevedel-view--source-revisions))))

(defun mevedel-view--tool-source-properties (start end)
  "Return relevant provenance intervals in the source range START..END.
Inspect buffer properties directly without copying the payload."
  (let ((pos start) intervals)
    (while (< pos end)
      (let ((next (min (next-single-property-change pos 'gptel nil end)
                       (next-single-property-change pos 'mevedel-render-data nil end)
                       (next-single-property-change pos 'mevedel-hook-audit nil end))))
        (push (list (- pos start) (- next start)
                    (copy-tree (get-text-property pos 'gptel))
                    (copy-tree (get-text-property pos 'mevedel-render-data))
                    (copy-tree (get-text-property pos 'mevedel-hook-audit)))
              intervals)
        (setq pos next)))
    (nreverse intervals)))

(defvar mevedel-view--tool-cache-used nil
  "Tool keys visited by the current complete projection, or nil.
Staged projection jobs retain this table across their callbacks.")

(defun mevedel-view--sweep-tool-rendering-cache (used)
  "Retain only the current projection's USED tool keys and source identities.
Call after a successful complete projection, never for an incremental tail."
  (when (hash-table-p mevedel-view--tool-rendering-cache)
    (maphash (lambda (key _rendering)
               (unless (gethash key used)
                 (remhash key mevedel-view--tool-rendering-cache)))
             mevedel-view--tool-rendering-cache))
  (let ((sources (make-hash-table :test #'eq)))
    (maphash (lambda (key _)
               (let* ((data (car key))
                      (ranges (or (gethash data sources)
                                  (puthash data (make-hash-table :test #'equal) sources))))
                 (puthash (cons (nth 1 key) (nth 2 key)) t ranges))) used)
    (maphash (lambda (data ranges)
               (when (buffer-live-p data)
                 (with-current-buffer data
                   (when (hash-table-p mevedel-view--source-revisions)
                     (maphash (lambda (range _identity)
                                (unless (gethash range ranges)
                                  (remhash range mevedel-view--source-revisions)))
                              mevedel-view--source-revisions))))) sources)))

(defun mevedel-view--tool-cache-key
    (data-buf seg-start seg-end collapsed-only)
  "Return a source-tracked key for DATA-BUF SEG-START..SEG-END rendering.
COLLAPSED-ONLY distinguishes header and body renderings.  Unchanged source
characters and provenance reuse the key without copying or hashing payloads."
  (with-current-buffer data-buf
    (let ((start (mevedel-view--source-position seg-start))
          (end (mevedel-view--source-position seg-end)))
      (let ((key (list data-buf start end
                       (mevedel-view--source-revision start end)
                       (and (boundp 'mevedel--session)
                            (mevedel-view--session-render-state-fingerprint mevedel--session))
                       (and collapsed-only t)
                       (mevedel-view--tool-source-properties start end))))
        (when mevedel-view--tool-cache-used
          (puthash key t mevedel-view--tool-cache-used))
        key))))

(defun mevedel-view--collapsed-rendering-p (rendering)
  "Return non-nil when RENDERING initially renders as a collapsed header."
  (and rendering
       (not (and (plist-member rendering :expandable-p)
                 (not (plist-get rendering :expandable-p))))
       (or (not (plist-member rendering :initially-collapsed-p))
           (plist-get rendering :initially-collapsed-p))))

(defun mevedel-view--omit-rendering-body-for-cache (rendering)
  "Return RENDERING with its body omitted for collapsed-header caching.
Nested call rows go with the body: a collapsed block shows neither, and
retaining them would keep every child result in the header cache."
  (if (mevedel-view--collapsed-rendering-p rendering)
      (plist-put (plist-put (copy-sequence rendering) :body nil)
                 :child-calls nil)
    rendering))

(defun mevedel-view--execution-terminal-render-data
    (data-buf tool-use-id &optional yielded-id)
  "Return TOOL-USE-ID's latest completion, including after segment rotation.
An archived row still belongs to the live transcript: completion may have
arrived in an intermediate segment after the source row was archived.
YIELDED-ID limits older-segment inspection to yielded executions."
  (or (and (buffer-live-p mevedel--data-buffer)
           (not (eq mevedel--data-buffer data-buf))
           (mevedel-execution-transcript-pending-render-data
            mevedel--data-buffer tool-use-id))
      (and (buffer-live-p data-buf)
           (mevedel-execution-transcript-pending-render-data
            data-buf tool-use-id))
      (when (and yielded-id (buffer-live-p mevedel--data-buffer)
                 (not (eq mevedel--data-buffer data-buf)))
        (when-let* ((session (buffer-local-value
                              'mevedel--session mevedel--data-buffer)))
          (catch 'found
            (dolist (descriptor (reverse (mevedel-session-artifacts-transcript-segments
                                          session mevedel--data-buffer
                                          mevedel-view--agent-transcript-p)))
              (when (and (eq (plist-get descriptor :status) 'readable)
                         (not (plist-get descriptor :current-p)))
                (when-let* ((older (condition-case nil
                                      (mevedel-session-artifacts-read-transcript-segment
                                       session descriptor)
                                    (error nil))))
                  (unwind-protect
                      (when-let* ((terminal
                                   (mevedel-execution-transcript-pending-render-data
                                    older tool-use-id)))
                        (throw 'found terminal))
                    (kill-buffer older))))))))))

(defun mevedel-view--render-tool-call (call data-buf &optional collapsed-only)
  "Render parsed CALL from DATA-BUF using the current view and tool registry.
COLLAPSED-ONLY requests summary-only renderer work.  Source preparation must
finish before this function invokes registered renderers or reads live events."
  (when call
    (let* ((mevedel-tool-render-summary-only collapsed-only)
           (name (plist-get call :name))
           (args (plist-get call :args))
           (tool-use-id (plist-get call :tool-use-id))
           (event
            (and (bound-and-true-p mevedel-view--execution-events)
                 (hash-table-p mevedel-view--execution-events)
                 (gethash tool-use-id mevedel-view--execution-events)))
           (call-render-data (plist-get call :render-data))
           (terminal-render-data
            (and (member name '("Bash" "Monitor"))
                 (mevedel-view--execution-terminal-render-data
                  data-buf tool-use-id
                  (plist-get call-render-data :execution-id))))
           (event-type (plist-get event :type))
           (direct-child (mevedel-tool-render-data-direct-call
                          name call-render-data))
           (direct-terminal
            (and (member (plist-get direct-child :tool) '("Bash" "Monitor"))
                 (stringp (plist-get direct-child :id))
                 (mevedel-view--execution-terminal-render-data
                  data-buf (plist-get direct-child :id)
                  (plist-get (plist-get direct-child :render-data)
                             :execution-id))))
           (direct-event
            (and (not direct-terminal)
                 (stringp (plist-get direct-child :id))
                 (bound-and-true-p mevedel-view--execution-events)
                 (hash-table-p mevedel-view--execution-events)
                 (gethash (plist-get direct-child :id)
                          mevedel-view--execution-events)))
           (direct-progress
            (and (eq (plist-get direct-event :type) 'progress)
                 (append (copy-sequence (plist-get direct-event :facts))
                         (list :permission-approvals
                               (plist-get direct-event :permission-approvals)
                               :status 'success :live-execution-p t
                               :output-preview-truncated-p
                               (plist-get direct-event :output-preview-truncated-p)))))
           (direct-latest (or direct-terminal direct-progress))
           (result
            (cond
             ((eq event-type 'progress)
              (or (plist-get event :output-tail) ""))
             ((plist-member terminal-render-data :execution-output)
              (or (plist-get terminal-render-data :execution-output) ""))
             ((plist-member direct-latest :execution-output)
              (or (plist-get direct-latest :execution-output) ""))
             (direct-progress
              (or (plist-get direct-event :output-tail) ""))
             ((plist-member call-render-data :execution-output)
              (or (plist-get call-render-data :execution-output) ""))
             ((plist-member (plist-get direct-child :render-data)
                            :execution-output)
              (or (plist-get (plist-get direct-child :render-data)
                             :execution-output) ""))
             (t (plist-get call :result))))
           (render-data
            (if (eq event-type 'progress)
                (let ((facts (copy-sequence (plist-get event :facts))))
                  (append
                   facts
                   (list :permission-approvals
                         (plist-get event :permission-approvals)
                         :status 'success
                         :live-execution-p t
                         :output-preview-truncated-p
                         (plist-get event :output-preview-truncated-p))))
              (or terminal-render-data call-render-data)))
           (tool (mevedel-tool-get name))
           custom rendering)
      ;; Direct ToolCall rows unwrap the child without creating a separate view
      ;; row.  Reconcile its settled facts before that shortcut invokes Bash's
      ;; renderer, just as a separately rendered compound child would do.
      (when direct-latest
        (setq render-data
              (plist-put (copy-sequence render-data) :calls
                         (list (plist-put
                                (plist-put (copy-sequence direct-child)
                                           :render-data direct-latest)
                                :status (plist-get direct-latest :status))))))
      (setq custom (and tool (mevedel-view--invoke-renderer
                              tool render-data args result)))
      (setq rendering (or custom
                          (mevedel-view--generic-tool-rendering
                           name args result collapsed-only render-data)))
      (when-let* (((eq (plist-get rendering :vtype) 'agent-handle))
                  (path (plist-get render-data :path)))
        (setq rendering (plist-put rendering :agent-path path)))
      ;; A direct ToolCall renders its child as the outer row.  Its terminal
      ;; facts can arrive after the envelope, so retain the child's disclosure
      ;; and execution identity on that same row without changing the source ID.
      (when-let* ((summary (or (plist-get direct-latest :sandbox-summary)
                               (plist-get (plist-get direct-child :render-data)
                                          :sandbox-summary)
                               (plist-get call-render-data :sandbox-summary)
                               (plist-get render-data :sandbox-summary))))
        (setq rendering
              (plist-put rendering :sandbox-summary (copy-tree summary))))
      (when (member (plist-get direct-child :tool) '("Bash" "Monitor"))
        (when-let* ((id (or (plist-get direct-latest :execution-id)
                           (plist-get (plist-get direct-child :render-data)
                                      :execution-id))))
          (setq rendering (plist-put rendering :execution-id id))))
      (when tool-use-id
        (setq rendering
              (plist-put rendering :tool-use-id tool-use-id)))
      (when-let* ((audits
                   (mevedel-view--merge-hook-audits
                    (plist-get call :hook-audits)
                    (plist-get rendering :hook-audits))))
        (setq rendering (plist-put rendering :hook-audits audits)))
      ;; Grouping needs the same normalized result and render data.  Carry
      ;; them with this one computation; the cache drops the payload below
      ;; so a collapsed header does not retain a large result indefinitely.
      (setq rendering
            (plist-put rendering :group-tool
                       (or (plist-get direct-child :tool) name)))
      (setq rendering
            (plist-put
             rendering :group-child
             (list :tool name :args args :result result :render-data render-data
                   :status (if (and direct-child (plist-get rendering :status))
                               (mevedel-view--rendering-status rendering)
                             (mevedel-view--tool-render-status
                              result render-data)))))
      (if collapsed-only
          (mevedel-view--omit-rendering-body-for-cache rendering)
        rendering))))

(defun mevedel-view--prepare-tool-segment (data-buf seg-start seg-end)
  "Prepare DATA-BUF's SEG-START..SEG-END without invoking a renderer.
Return a parsed call or request failure and whether the complete span can be
cached under its own source identity.  Partial spans keep their recovery path.
The returned data is temporary; it can contain large hidden result payloads."
  (let* ((text (mevedel-view-render--parse-step
                (with-current-buffer data-buf
                  (buffer-substring seg-start seg-end))))
         (request-data (mevedel-view-render--parse-step
                         (mevedel-view--request-summary-render-data-from-text text))))
    (if (eq (plist-get request-data :outcome) 'error)
        (list :request-failure request-data :cacheable t)
      (let* ((raw (mevedel-view-render--parse-step
                   (with-current-buffer data-buf
                     (mevedel-view--tool-segment-text seg-start seg-end text))))
             ;; The key's revision covers the whole span, so a complete block
             ;; inside it -- followed by its merged hook audits -- is as
             ;; stable as the span.  Requiring equality left every audited
             ;; tool reparsed on each streamed update.
             (cacheable
              (mevedel-view-render--parse-step
                (and (string-search raw text)
                     (or (mevedel-view--complete-wrapped-tool-text-p raw)
                         (and (not (mevedel-view--tool-wrapped-text-p raw))
                              (mevedel-view--direct-tool-readable-text-p raw)))))))
        (list :call (mevedel-view--tool-call-parse data-buf seg-start seg-end raw)
              :cacheable cacheable)))))

(defun mevedel-view--render-prepared-tool (prepared data-buf &optional collapsed-only)
  "Render PREPARED source data using the current view's presentation state.
DATA-BUF supplies live execution metadata; COLLAPSED-ONLY requests summaries."
  (if-let* ((request-data (plist-get prepared :request-failure)))
      (let ((backend (or (plist-get request-data :backend) "Provider"))
            (status (plist-get request-data :status))
            (type (plist-get request-data :error-type))
            (code (plist-get request-data :error-code))
            (error-data (plist-get request-data :error-data))
            (retry-errors (plist-get request-data :retry-errors))
            (retry-exhausted (plist-get request-data :retry-exhausted))
            (message-text (plist-get request-data :message)))
        (list
         :header (concat backend " request failed"
                         (if type (format " · %s" type) "")
                         (if retry-exhausted " · retries exhausted" ""))
         :body
         (string-join
          (delq nil
                (list (and status (format "Status: %s" status))
                      (and type (format "Type: %s" type))
                      (and code (format "Code: %s" code))
                      (and (listp error-data)
                           (format "Provider data: %S" error-data))
                      ""
                      (and message-text (format "%s" message-text))
                      (and retry-exhausted
                           (format "Automatic retries exhausted%s."
                                   (if retry-errors
                                       (format " after %d attempts"
                                               (length retry-errors))
                                     "")))
                      (and retry-errors
                           (concat
                            "Attempt history (oldest first):\n"
                            (mapconcat
                             (lambda (attempt)
                               (format
                                "Attempt %s%s: %s"
                                (plist-get attempt :attempt)
                                (let ((details
                                       (delq nil
                                             (list (plist-get attempt :http-status)
                                                   (plist-get attempt :type)
                                                   (plist-get attempt :code)))))
                                  (if details
                                      (concat " · " (string-join details " · "))
                                    ""))
                                (plist-get attempt :message)))
                             retry-errors "\n")))
                      ""
                      "Retry the request manually."))
          "\n")
         :body-mode 'text-mode
         :vtype 'request-failure
         :status 'error
         :initially-collapsed-p nil))
    (mevedel-view--render-tool-call (plist-get prepared :call) data-buf collapsed-only)))

(defun mevedel-view--retire-tool-renderings (data-buf start end)
  "Discard renderings overlapping DATA-BUF START..END before a local refresh.
Local refreshes need this too: a full projection may not run again for a long
conversation, and patched agent handles otherwise retain every old body."
  (when (hash-table-p mevedel-view--tool-rendering-cache)
    (maphash
     (lambda (key _rendering)
       (when (and (eq (car-safe key) data-buf)
                  (< (nth 1 key) end) (> (nth 2 key) start))
         (remhash key mevedel-view--tool-rendering-cache)))
     mevedel-view--tool-rendering-cache)))

(defun mevedel-view--segment-rendering (data-buf seg-start seg-end
                                                 &optional collapsed-only)
  "Return rendering for DATA-BUF's SEG-START..SEG-END.
Provider-failure request summaries and tool segments are renderable.
Return nil only when the segment is malformed or unparseable.
COLLAPSED-ONLY requests a cached summary without retaining large bodies."
  (let* ((cache (and (hash-table-p mevedel-view--tool-rendering-cache)
                     mevedel-view--tool-rendering-cache))
         (key (and cache (mevedel-view--tool-cache-key
                          data-buf seg-start seg-end collapsed-only))))
    (or (and key (gethash key cache))
        (let* ((prepared (or (and collapsed-only (bound-and-true-p mevedel-view-prepare-enabled)
                                  (mevedel-view-prepare-get data-buf seg-start seg-end key))
                             (mevedel-view--prepare-tool-segment data-buf seg-start seg-end)))
               (rendering (or (plist-get prepared :pending)
                              (mevedel-view--render-prepared-tool prepared data-buf collapsed-only))))
          (when (and key rendering (plist-get prepared :cacheable))
            (puthash key (plist-put (copy-sequence rendering) :group-child nil) cache))
          rendering))))

(defvar-local mevedel-view--tool-row-starts nil
  "Hash table from tool-use ID to a marker at its last found view row.
A hint only: `mevedel-view--tool-row-region' revalidates it before use.")

(defconst mevedel-view--tool-row-starts-limit 32
  "Maximum number of remembered tool row markers per view.")

(defun mevedel-view--forget-tool-row-starts ()
  "Drop every remembered tool row marker in the current view."
  (when (hash-table-p mevedel-view--tool-row-starts)
    (maphash (lambda (_id marker) (set-marker marker nil))
             mevedel-view--tool-row-starts)
    (clrhash mevedel-view--tool-row-starts)))

(defun mevedel-view--tool-row-start-p (pos limit tool-use-id)
  "Return non-nil when TOOL-USE-ID's refreshable row starts at POS before LIMIT."
  (and (< pos limit)
       (consp (get-text-property pos 'mevedel-view-source))
       (eq (get-text-property pos 'mevedel-view-type) 'tool-summary)
       (equal (get-text-property pos 'mevedel-view-tool-use-id) tool-use-id)))

(defun mevedel-view--tool-row-start (tool-use-id limit)
  "Return the start of TOOL-USE-ID's refreshable row before LIMIT.
A projection shows each tool-use ID in at most one summary row.  Progress
refreshes the same row repeatedly, so a remembered start that still begins
that row spares scanning the transcript."
  (let* ((marker (and (hash-table-p mevedel-view--tool-row-starts)
                      (gethash tool-use-id mevedel-view--tool-row-starts)))
         (hint (and marker (marker-position marker))))
    (if (and hint
             (mevedel-view--tool-row-start-p hint limit tool-use-id)
             (or (= hint (point-min))
                 (not (equal (get-text-property (1- hint) 'mevedel-view-tool-use-id)
                             tool-use-id))))
        hint
      (when marker
        (set-marker marker nil)
        (remhash tool-use-id mevedel-view--tool-row-starts))
      (let ((pos (point-min)) found)
        (while (and (< pos limit) (not found))
          (if (mevedel-view--tool-row-start-p pos limit tool-use-id)
              (setq found pos)
            (setq pos (or (next-single-property-change
                           pos 'mevedel-view-tool-use-id nil limit)
                          limit))))
        (when found
          (unless (hash-table-p mevedel-view--tool-row-starts)
            (setq mevedel-view--tool-row-starts (make-hash-table :test #'equal)))
          (when (>= (hash-table-count mevedel-view--tool-row-starts)
                    mevedel-view--tool-row-starts-limit)
            (mevedel-view--forget-tool-row-starts))
          (puthash tool-use-id (copy-marker found) mevedel-view--tool-row-starts))
        found))))

(defun mevedel-view--tool-row-region (data-buffer tool-use-id)
  "Return the visible source-backed row for TOOL-USE-ID in DATA-BUFFER.
The result is `(VIEW-START VIEW-END SOURCE-BOUNDS)' or nil."
  (when-let* ((bounds
               (with-current-buffer data-buffer
                 (mevedel-tool-render-data-segment-bounds tool-use-id)))
              ;; Yanking a tool row keeps its identity properties.  A draft is
              ;; never a refresh target, even if grouping hides the original row.
              (limit (if mevedel-view--agent-transcript-p
                         (point-max)
                       (or (mevedel-view--input-marker-position) (point-max))))
              (pos (mevedel-view--tool-row-start tool-use-id limit)))
    (let ((source (get-text-property pos 'mevedel-view-source))
          (next (or (next-single-property-change
                     pos 'mevedel-view-tool-use-id nil limit)
                    limit)))
      ;; Expanded compound tools append child rows with their own IDs.
      ;; Replace the whole parent section, not just its text up to the
      ;; first child, or every progress refresh duplicates the children.
      (list pos (mevedel-view-render-child-calls-end next limit source)
            ;; Restoring transcript properties can split the metadata into an
            ;; ignored run.  The projected source still owns the complete
            ;; canonical span.
            (if (and (markerp (car source))
                     (markerp (cdr source))
                     (eq (marker-buffer (car source)) data-buffer)
                     (eq (marker-buffer (cdr source)) data-buffer)
                     (<= (car source) (car bounds))
                     (<= (cdr bounds) (cdr source)))
                (cons (marker-position (car source))
                      (marker-position (cdr source)))
              bounds)))))

(defun mevedel-view--refresh-tool-row (data-buffer tool-use-id)
  "Refresh only TOOL-USE-ID's visible row from DATA-BUFFER."
  (mevedel-view-render-mutate
   (list 'tool-row tool-use-id)
   (lambda () (mevedel-view-render--refresh-tool-row-now data-buffer tool-use-id))))

(defun mevedel-view-render--refresh-tool-row-now (data-buffer tool-use-id)
  "Rediscover TOOL-USE-ID in DATA-BUFFER with projection ownership held."
  (let ((owner tool-use-id) region)
    ;; Compound children share the outer gptel segment.  Refresh that row for
    ;; both attended events and deferred updates when focus returns.
    (while (and (not region) owner)
      (setq region (mevedel-view--tool-row-region data-buffer owner)
            owner (and (string-match "/[0-9]+\\'" owner)
                       (substring owner 0 (match-beginning 0)))))
    (when region
    (let* ((start (nth 0 region))
           (end (nth 1 region))
           (bounds (nth 2 region))
           (source (mevedel-view-disclosure-source-range
                    data-buffer (car bounds) (cdr bounds)))
           (collapsed (get-text-property start 'mevedel-view-collapsed))
           (previously-forced-p
            (get-text-property start 'mevedel-view-force-expanded))
           (turn-id (get-text-property start 'mevedel-view-turn-id)))
      ;; Rebase visible nested choices before replacing their source-backed row.
      (mevedel-view-disclosure-capture-state start end)
      ;; Progress changes rendering even when its source text stays unchanged.
      (mevedel-view--retire-tool-renderings data-buffer (car source) (cdr source))
      (when-let* ((rendering
                  (mevedel-view--segment-rendering
                   data-buffer (car source) (cdr source))))
        ;; The source span can include following audits, but this replacement
        ;; owns only the tool and its nested calls.  Their separate audit and
        ;; completion rows remain outside START..END; reinserting them here
        ;; would add another copy on every progress event.
        (setq rendering (plist-put (copy-sequence rendering) :hook-audits nil))
        (unless (or (plist-get rendering :force-expanded-p)
                    previously-forced-p)
          (setq rendering
                (plist-put (copy-sequence rendering)
                           :initially-collapsed-p collapsed)))
        (mevedel-view--call-preserving-user-view-state
         (lambda ()
           (mevedel-view--call-with-render-boundaries-advancing
            (lambda ()
              (let ((inhibit-read-only t)
                    (inhibit-modification-hooks t))
                (save-excursion
                  (goto-char start)
                  (delete-region start end)
                  (let ((insert-start (point)))
                    (mevedel-view--insert-rendered-tool rendering source)
                    (mevedel-view-render-add-display-properties
                     insert-start (point))
                    (when turn-id
                      (put-text-property
                       insert-start (point)
                       'mevedel-view-turn-id turn-id))))))))
         ;; Only the row changes; the reader's position elsewhere is exact.
         (cons start end))
        t)))))


;;
;;; Thinking block summary

(defun mevedel-view--reasoning-source-bounds (data-buf start end)
  "Return reasoning block bounds inside DATA-BUF START..END.
When a restored transcript leaves assistant-side text unpropertized,
the thinking group can start before the structural reasoning block.
Prefer the explicit `#+begin_reasoning' marker so expanding a thinking
summary does not include preceding agent-result or tool output text."
  (with-current-buffer data-buf
    (save-excursion
      (save-restriction
        (widen)
        (let* ((pmin (point-min))
               (pmax (point-max))
               (s (max pmin (min start pmax)))
               (e (max pmin (min end pmax)))
               block-start block-end)
          (when (< s e)
            (goto-char s)
            (when (re-search-forward "^#\\+begin_reasoning\\b[^\n]*\n?"
                                     e t)
              (setq block-start (match-beginning 0))
              (setq block-end
                    (if (re-search-forward "^#\\+end_reasoning[^\n]*\n?"
                                           e t)
                        (match-end 0)
                      e))
              (cons block-start block-end))))))))

(defun mevedel-view--strip-tool-blocks-from-reasoning (text)
  "Return reasoning TEXT with nested org tool blocks removed."
  (let ((text (or text ""))
        (case-fold-search (default-value 'case-fold-search)))
    (if (not (string-match-p "^#\\+begin_tool\\b" text))
        text
      (with-temp-buffer
        (insert text)
        (goto-char (point-min))
        (while (re-search-forward "^#\\+begin_tool\\b" nil t)
          (let ((start (match-beginning 0)))
            (if (re-search-forward "^#\\+end_tool[^\n]*\n?" nil t)
                (delete-region start (point))
              (delete-region start (point-max)))))
        (buffer-string)))))

(defconst mevedel-view--clean-reasoning-cache-size 24
  "Reasoning-text cleanings retained by `mevedel-view--clean-reasoning-cache'.

Sized for the completed reasoning blocks a transcript re-renders, not for
history: each entry holds a whole cleaned block, and a miss costs only the
work this cache exists to skip.")

(defvar mevedel-view--clean-reasoning-cache nil
  "Cleaned reasoning text, keyed by text after trusted audit removal.

Global for the same reason as `mevedel-view--scaffolding-only-cache\': the
cleaning depends on nothing but its argument.

Audit removal runs before lookup because trust depends on text properties,
which string equality ignores.

It earns its place on redraw.  A render walks every turn in the transcript,
so each tick re-cleans every completed reasoning block as well as the live
one -- four passes over each, allocating a fresh copy per pass.  Completed
blocks do not change, so they hit.  A streaming block has no end marker and
is not inserted: each growing version would miss and retain an obsolete
raw/cleaned pair.")

(defun mevedel-view--clean-reasoning-text (text)
  "Strip org scaffolding markers from reasoning TEXT.
Removes trusted audit records, reasoning block markers, nested tool blocks,
and generated system reminder wrappers.  TEXT must retain audit properties.

Completed blocks are cached globally; see
`mevedel-view--clean-reasoning-cache'."
  (setq text (mevedel--strip-hook-audit-blocks text))
  (or (cdr (assoc text mevedel-view--clean-reasoning-cache))
      (let ((cleaned (mevedel-view--clean-reasoning-text-1 text)))
        (when (string-match-p "^#\\+end_reasoning\\b" text)
          (setq mevedel-view--clean-reasoning-cache
                (cons (cons text cleaned)
                      (let ((kept
                             (seq-take
                              mevedel-view--clean-reasoning-cache
                              (1- mevedel-view--clean-reasoning-cache-size))))
                        (assoc-delete-all text kept)))))
        cleaned)))

(defun mevedel-view--clean-reasoning-text-1 (text)
  "Return TEXT with reasoning scaffolding removed, without consulting a cache."
  (let ((cleaned (mevedel-view--strip-render-data-display-text text)))
    (setq cleaned (mevedel-view--strip-system-reminder-blocks cleaned))
    (setq cleaned (mevedel-view--strip-tool-blocks-from-reasoning cleaned))
    ;; One pass over the marker lines rather than three.  Each
    ;; `replace-regexp-in-string' copies the whole string, and reasoning
    ;; text runs to thousands of lines while every redraw re-cleans it.
    (let ((markers "#\\+\\(?:begin\\|end\\)_\\(?:reasoning\\|tool\\)[^\n]*\n?"))
      (if (string-match-p markers cleaned)
          (replace-regexp-in-string markers "" cleaned)
        cleaned))))

(defun mevedel-view--strip-render-data-display-text (text)
  "Return TEXT without hidden render-data side-channel scaffolding."
  (mevedel-tool-render-data-strip (or text "")))

(defun mevedel-view--render-data-only-text-p (text)
  "Return non-nil if TEXT is only render-data scaffolding."
  (and (stringp text)
       ;; Without a block the text itself is what remains; skip the copies.
       (string-search mevedel--render-data-open text)
       (string-match-p "[^ \t\n\r]" text)
       (not (string-match-p
             "[^ \t\n\r]"
             (mevedel-view--strip-render-data-display-text text)))))

(defun mevedel-view--hook-audit-only-segment-p (data-buf seg-start seg-end)
  "Return non-nil when DATA-BUF's SEG-START..SEG-END is only hook audit data."
  (with-current-buffer data-buf
    (mevedel-transcript-audit-buffer-only-p seg-start seg-end)))

(defun mevedel-view--system-reminder-body-from-text (text)
  "Return generated system reminder body from TEXT, or nil.
TEXT must contain only one complete `<system-reminder>' block plus
surrounding whitespace.  Embedded literal examples are not treated
as generated control markup."
  (when (stringp text)
    (with-temp-buffer
      (insert (string-trim text))
      (goto-char (point-min))
      (when-let* ((range
                   (mevedel-transcript--system-reminder-range-at-point
                    (point-max)))
                  ((= (nth 3 range) (point-max))))
        (string-trim
         (buffer-substring-no-properties (nth 1 range) (nth 2 range)))))))

(defun mevedel-view--strip-system-reminder-blocks (text)
  "Return TEXT without generated `<system-reminder>' blocks."
  (let ((text (or text ""))
        (case-fold-search (default-value 'case-fold-search)))
    (if (not (string-match-p "<system-reminder>" text))
        text
      (with-temp-buffer
        (insert text)
        (goto-char (point-min))
        (while (search-forward "<system-reminder>" nil t)
          (let ((start (match-beginning 0)))
            (if (search-forward "</system-reminder>" nil t)
                (progn
                  (delete-region start (point))
                  (goto-char start))
              (goto-char (point-max)))))
        (buffer-string)))))

(defun mevedel-view--system-reminder-line-count (body)
  "Return the non-empty line count for system reminder BODY."
  (length (split-string (or body "") "\n" t "[ \t]+")))

(defun mevedel-view--inline-skill-attachments-summary (text)
  "Return the attached-skill row header for inline-skill TEXT, or nil."
  (when-let* ((data (mevedel-view--inline-skill-render-data-from-text text))
              (attachments (plist-get data :attachments)))
    (mevedel-view--attached-skills-summary
     (mapcar (lambda (attachment) (plist-get attachment :name)) attachments)
     (length attachments))))

(defun mevedel-view--system-reminder-summary (data-buf seg-start seg-end)
  "Return collapsed summary for DATA-BUF's SEG-START..SEG-END system reminder."
  (with-current-buffer data-buf
    ;; The inline-skill probe needs the render-data text properties, so
    ;; the properties are stripped only for the generic reminder path.
    (let ((text (buffer-substring seg-start seg-end)))
      (or (mevedel-view--inline-skill-attachments-summary text)
          (let* ((body (mevedel-view--system-reminder-body-from-text
                        (substring-no-properties text)))
                 (lines (max 1 (mevedel-view--system-reminder-line-count
                                body))))
            (propertize
             (format "  \u25c7 System reminder (%d %s)"
                     lines
                     (if (= lines 1) "line" "lines"))
             'font-lock-face 'mevedel-view-system-reminder))))))

(defun mevedel-view--attached-skills-summary (names count)
  "Return the collapsed header for COUNT attached-skill reminders.
NAMES labels the attached skills when the record carries them."
  (propertize
   (format "  \u25c7 %d attached skill%s%s"
           count
           (if (= 1 count) "" "s")
           (if names
               (format " (%s)" (mapconcat #'identity names ", "))
             ""))
   'font-lock-face 'mevedel-view-system-reminder))

(defun mevedel-view--scaffolding-only-p (data-buf seg-start seg-end)
  "Return non-nil if DATA-BUF region [SEG-START, SEG-END] is org-only glue.
A segment is org-only when it contains nothing but `#+begin_…' /
`#+end_…' marker lines, blank lines, and whitespace.  Used by the
assistant-turn renderer to drop glue segments between adjacent
`ignore'/`tool' segments so they don't surface as fake `Thinking…
\(1 lines)' entries.

Source-of-truth: the gptel text-property scheme alone leaves
markers and blank lines unpropertised, so the segment extractor
classifies them as `user'.  Without this filter, the assistant
turn shows one bogus thinking summary per tool boundary."
  (with-current-buffer data-buf
    (save-restriction
      (widen)
      (let* ((pmin (point-min))
             (pmax (point-max))
             (s (max pmin (min seg-start pmax)))
             (e (max pmin (min seg-end pmax))))
        (unless (mevedel-view--leading-content-p s e)
          (mevedel-view--scaffolding-only-text-p
           (and (< s e) (buffer-substring-no-properties s e))))))))

(defconst mevedel-view--leading-content-regexp
  "\\`\\(?:[ \t\r\n]*#\\+\\(?:begin\\|end\\)_reasoning\\b[^\n]*\\(?:\n\\|\\'\\)\\)*[ \t\r\n]*[^ \t\r\n#<]"
  "Match text that visibly begins with content.
Whitespace and reasoning marker lines may precede it; any other character
there is outside every block that reasoning cleaning removes, since each
removed construct begins with `#' or `<'.  Most segments are content, and
proving that by cleaning a copy of each on every redraw dominated its
allocation.")

(defun mevedel-view--leading-content-p (start end)
  "Return non-nil when the current buffer's START..END begins with content.
See `mevedel-view--leading-content-regexp'."
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (looking-at-p (substring mevedel-view--leading-content-regexp 2)))))

(defun mevedel-view--blank-segment-p (data-buf seg-start seg-end)
  "Return non-nil when DATA-BUF's SEG-START..SEG-END holds only whitespace.

Unlike `mevedel-view--scaffolding-only-p', this judges the text as it
stands rather than as glue: a tool or reasoning block with a body is
never blank, however much of it is marker lines."
  (with-current-buffer data-buf
    (save-restriction
      (widen)
      (let* ((pmin (point-min))
             (pmax (point-max))
             (s (max pmin (min seg-start pmax)))
             (e (max pmin (min seg-end pmax))))
        (or (>= s e)
            (string-blank-p (buffer-substring-no-properties s e)))))))

(defconst mevedel-view--scaffolding-only-cache-size 512
  "Scaffolding verdicts retained by `mevedel-view--scaffolding-only-cache'.

Only true verdicts are retained.  Growing live reasoning and ordinary user
text would otherwise keep whole false-result strings globally reachable.")

(defvar mevedel-view--scaffolding-only-cache (make-hash-table :test #'equal)
  "Scaffolding verdicts, keyed by the segment text they were derived from.

Global, not per buffer.  The verdict is a pure function of the text, so a
segment answers the same in whichever buffer asks -- and the render walks
more than one, so a buffer-local cache was refilled from empty rather than
reused.  That is why an earlier per-buffer version did not reduce this at
all.

A render walks every segment and asks each one whether it is glue, and the
answer came from cleaning the whole segment and testing the result for
emptiness -- four passes allocating a fresh copy each.  That was 81% of all
reasoning-cleaning allocation in a profiled remote turn, and it recurs on
every redraw because the segments do not change.

Only glue is retained.  Real content includes the growing live reasoning
segment, so caching false verdicts would preserve up to 512 progressively
larger copies merely to avoid recomputing nil.")

(defun mevedel-view--scaffolding-only-text-p (text)
  "Return non-nil if TEXT is org-only glue, or nil.
Nil TEXT counts as glue.  See `mevedel-view--scaffolding-only-p'.

Cached globally; see `mevedel-view--scaffolding-only-cache'."
  (or (null text)
      (gethash text mevedel-view--scaffolding-only-cache)
      (when (and (not (string-match-p
                       mevedel-view--leading-content-regexp text))
                 (string-empty-p
             (string-trim (mevedel-view--clean-reasoning-text-1 text))))
        ;; Cleared rather than evicted one by one: recomputing a verdict is
        ;; the work this cache already exists to skip, and a transcript long
        ;; enough to overflow re-examines its oldest segments least.
        (when (>= (hash-table-count mevedel-view--scaffolding-only-cache)
                  mevedel-view--scaffolding-only-cache-size)
          (clrhash mevedel-view--scaffolding-only-cache))
        (puthash text t mevedel-view--scaffolding-only-cache))))

(defun mevedel-view--inline-skill-render-data-from-text (text)
  "Return inline-skill render-data from TEXT, or nil."
  (let ((data (cdr (mevedel-tool-render-data-extract text))))
    (and (consp data)
         (eq (plist-get data :kind) 'inline-skill)
         data)))

(defun mevedel-view--collaboration-event-from-text (text)
  "Return a canonical started collaboration event from TEXT, or nil."
  (let ((data (cdr (mevedel-tool-render-data-extract text))))
    (and (consp data)
         (eq (plist-get data :kind) 'collaboration-event)
         (eq (plist-get data :event) 'started)
         data)))

(defun mevedel-view--request-summary-render-data-from-text (text)
  "Return request-summary render-data from TEXT, or nil."
  (let ((data (cdr (mevedel-tool-render-data-extract text))))
    (and (consp data)
         (eq (plist-get data :kind) 'request-summary)
         data)))

(defun mevedel-view--collaboration-event-segment-p
    (data-buf seg-start seg-end)
  "Return non-nil when DATA-BUF's span carries a started collaboration event."
  (with-current-buffer data-buf
    (mevedel-view--collaboration-event-from-text
     (buffer-substring seg-start seg-end))))

(defun mevedel-view--delete-request-summaries (data-buf start end)
  "Delete request-summary render-data blocks in DATA-BUF START..END."
  (with-current-buffer data-buf
    (save-excursion
      (let ((case-fold-search nil)
            (limit (copy-marker (or end (point-max)) t)))
        (goto-char (or start (point-min)))
        (unwind-protect
            (while (search-forward "<!-- mevedel-render-data -->" limit t)
              (when-let* ((open (match-beginning 0))
                          (block (mevedel-tool-render-data-block-at
                                  open (marker-position limit))))
                (goto-char (cadr block))
                (when (and (eq (plist-get (caddr block) :kind) 'request-summary)
                           (not (plist-member (caddr block) :mevedel-tool-use-id))
                           (mevedel-tool-render-data-trusted-range-p
                            open (cadr block)))
                  (delete-region (car block) (cadr block))
                  (goto-char (car block)))))
          (set-marker limit nil))))))

(defun mevedel-view--request-summary-elapsed-seconds (data-buf)
  "Return elapsed seconds for DATA-BUF's current request, or nil."
  (when-let* (((buffer-live-p data-buf))
              (request
               (buffer-local-value 'mevedel--current-request data-buf)))
    (mevedel-request-active-elapsed-seconds request)))

(defun mevedel-view--append-request-summary
    (data-buf search-start &optional extra)
  "Append hidden request-summary render-data to DATA-BUF if needed.
SEARCH-START bounds duplicate detection to the current response tail.
EXTRA is additional request metadata to persist.
Return the new data-buffer end position."
  (when-let* ((elapsed (mevedel-view--request-summary-elapsed-seconds
                        data-buf)))
    (with-current-buffer data-buf
      (let ((tail-start (or search-start (point-min))))
        (mevedel-view--delete-request-summaries
         data-buf tail-start (point-max))
        (save-excursion
          (goto-char (point-max))
          (unless (bolp) (insert "\n"))
          (insert
           (mevedel-tool-render-data-format
            (append
             (list :kind 'request-summary
                   :elapsed-seconds elapsed)
             extra))))))
    (with-current-buffer data-buf
      (point-max))))

(defun mevedel-view--review-action-segment-p (data-buf seg-start seg-end)
  "Return non-nil when DATA-BUF's SEG-START..SEG-END is only review action."
  (with-current-buffer data-buf
    (let ((text (buffer-substring-no-properties seg-start seg-end)))
      (and (string-search "<user_action>" text)
           (string-search "<action>review</action>" text)
           (string-empty-p
            (string-trim
             (mevedel-review-strip-user-action-blocks text)))))))

(defun mevedel-view--thinking-summary (data-buf seg-start seg-end)
  "Generate a summary for a thinking/reasoning block.
Reads content from DATA-BUF between SEG-START and SEG-END.
Returns empty string when the block is trivial (only whitespace
or org scaffolding markers)."
  (let* ((bounds (mevedel-view--reasoning-source-bounds
                  data-buf seg-start seg-end))
         (seg-start (or (car-safe bounds) seg-start))
         (seg-end (or (cdr-safe bounds) seg-end)))
    (with-current-buffer data-buf
      (let* ((text (buffer-substring seg-start seg-end))
             (cleaned (mevedel-view--clean-reasoning-text text))
             (count (mevedel-view--nonblank-line-count cleaned)))
        (if (> count 0)
            (concat
             "  "
             (propertize mevedel-view--thinking-glyph
                         'font-lock-face 'mevedel-view-thinking-marker)
             (propertize (format "Thinking... (%d lines)" count)
                         'font-lock-face 'mevedel-view-thinking-summary))
          "")))))

(defun mevedel-view--nonblank-line-count (text)
  "Return the number of non-blank lines in TEXT.

Counts in place.  The summary needs only the number, and building the
list of lines to take its length allocated one string per line of a
reasoning block that reaches thousands of lines -- on every redraw."
  (let ((count 0) (start 0) (length (length text)))
    (while (< start length)
      (let* ((newline (string-search "\n" text start))
             (end (or newline length))
             (blank t)
             (index start))
        (while (and blank (< index end))
          (unless (memq (aref text index) '(?\s ?\t))
            (setq blank nil))
          (setq index (1+ index)))
        (unless blank (setq count (1+ count)))
        (setq start (if newline (1+ newline) length))))
    count))


;;
;;; Rendering

(defun mevedel-view--strip-proposed-plans-p (text)
  "Return non-nil when TEXT's proposed-plan protocol blocks should be hidden.
Active planning hides incomplete streamed blocks.  Complete protocol blocks
stay hidden on later full rerenders without session-global hash history."
  (or (and (boundp 'mevedel--session)
           mevedel--session
           (mevedel-session-plan-mode mevedel--session))
      (mevedel-plan-extract-proposed text)))

(defun mevedel-view--current-render-insertion-marker ()
  "Return the marker render helpers should insert at."
  (or (and (markerp mevedel-view--render-insertion-marker)
           (marker-position mevedel-view--render-insertion-marker)
           mevedel-view--render-insertion-marker)
      mevedel-view--input-marker))

(defun mevedel-view--history-insertion-marker ()
  "Return the boundary where transcript/live-tail text should be inserted.

The history region ends at `mevedel-view--status-marker'.  Status and
interaction UI live below that boundary and above the input prompt."
  (unless (and (markerp mevedel-view--status-marker)
               (eq (marker-buffer mevedel-view--status-marker)
                   (current-buffer))
               (marker-position mevedel-view--status-marker))
    (error "View status marker is not live"))
  mevedel-view--status-marker)

(defun mevedel-view--after-header-position ()
  "Return the first history position after the session header.
The earlier-chats line belongs to the header chrome."
  (save-excursion
    (goto-char (point-min))
    ;; Agent transcript chrome lives in the header line, not in the
    ;; transcript.  Its first text line is already history.
    (unless mevedel-view--agent-transcript-p
      (forward-line 1)
      (when (get-text-property (point) 'mevedel-view-chats-line)
        (goto-char (or (next-single-property-change
                        (point) 'mevedel-view-chats-line)
                       (point-max)))))
    (point)))

(defun mevedel-view--transcript-history-position-p (pos)
  "Return non-nil when POS belongs to rendered transcript history."
  (or (get-text-property pos 'mevedel-view-source)
      (get-text-property pos 'mevedel-view-pending-tool-live)
      (memq (get-text-property pos 'mevedel-view-type)
            '(activity-separator
              hook-context
              mailbox-delivery
              prompt-summary
              request-summary
              response
              system-reminder-summary
              task-background
              thinking-summary
              tool-event
              tool-summary
              turn-header
              turn-summary
              user))))

(defun mevedel-view--non-history-view-position-p (pos)
  "Return non-nil when POS belongs to a non-history view row."
  (and (get-text-property pos 'mevedel-view-type)
       (not (mevedel-view--transcript-history-position-p pos))))

(defun mevedel-view--history-tail-position ()
  "Return the best-effort end of rendered transcript history.

When boundary markers are detached, recover the history/status boundary
from the last rendered transcript character before the composer.  If no
transcript has been rendered yet, return the position after the header."
  (let* ((after-header (mevedel-view--after-header-position))
         ;; A yanked response retains its source/type properties.  They do
         ;; not turn user-owned draft text into rendered history.
         (limit (if mevedel-view--agent-transcript-p
                    (point-max)
                  (or (mevedel-view--input-marker-position) (point-max))))
         (pos limit))
    (while (and (> pos after-header)
                (not
                 (mevedel-view--transcript-history-position-p (1- pos))))
      (setq pos (or (previous-property-change pos nil after-header)
                    after-header)))
    (if (and (> pos after-header)
             (mevedel-view--transcript-history-position-p (1- pos)))
        pos
      after-header)))

(defun mevedel-view--history-boundary-target ()
  "Return the live end of rendered history.

The normal target is the live history/status boundary, or a dynamic
render insertion marker that still points above the composer.  If marker
teardown detached the status marker, recover to the current rendered
history tail so text placed there stays above status/interaction content
without moving ahead of prior transcript turns."
  (let* ((input-pos (and (markerp mevedel-view--input-marker)
                         (marker-position mevedel-view--input-marker)))
         (render-pos (and (markerp mevedel-view--render-insertion-marker)
                          (marker-position
                           mevedel-view--render-insertion-marker)))
         (status-pos (and (markerp mevedel-view--status-marker)
                          (marker-position mevedel-view--status-marker)))
         (history-tail (mevedel-view--history-tail-position)))
    (or (and status-pos
             (= status-pos history-tail)
             mevedel-view--status-marker)
        (and render-pos
             (= render-pos history-tail)
             (or (not input-pos) (< render-pos input-pos))
             mevedel-view--render-insertion-marker)
        history-tail)))

;;;; In-flight tool calls
;;
;; A tool call the model has started but whose result has not reached the
;; data buffer has no source to project.  The live render still shows it,
;; in the grammar of the row it will become: it joins the turn's trailing
;; activity run, counts toward that run's grouping, and marks the row as
;; running.  When its result lands, the next render draws the same row
;; settled, so finishing changes the marker, not the layout.  Pending
;; decorations carry `mevedel-view-pending-tool-live'; nothing else depends
;; on them, and no disclosure state is keyed by them.

(defvar mevedel-view--render-pending nil
  "Cell (ENTRIES) of in-flight tool calls the live render still has to draw.
Bound only while the last in-flight turn renders.  The turn's trailing
activity run takes ENTRIES with `mevedel-view--take-render-pending';
otherwise the live render appends them after the turn.")

(defun mevedel-view--take-render-pending ()
  "Return and claim the in-flight tool entries of the current live render."
  (when mevedel-view--render-pending
    (prog1 (car mevedel-view--render-pending)
      (setcar mevedel-view--render-pending nil))))

(defun mevedel-view--pending-marker (id)
  "Return the leading marker area of a running row animated under ID.
Glyph spinner styles animate the marker itself.  Shimmer animates the
row's label instead, and static draws nothing moving, so both keep the
running dot.  Before insertion the display frame is unknown; glyphs use a
portable first frame."
  (if (memq mevedel-view-tool-spinner-style '(shimmer static))
      (propertize "  ● " 'font-lock-face 'mevedel-view-agent-running)
    (let ((frame (mevedel-view-animation-frame
                  mevedel-view-tool-spinner-style "" 0 'mevedel-view-ephemeral
                  :multiple)))
      (concat (propertize "  " 'font-lock-face 'mevedel-view-agent-running)
              (propertize frame
                          'font-lock-face 'mevedel-view-ephemeral
                          'mevedel-view-inline-spinner-frame t
                          'mevedel-view-tool-animation-id id
                          'display frame)))))

(defun mevedel-view--pending-label (text id)
  "Return TEXT, shimmering under animation ID when shimmer is the style.
The sweep uses TEXT's own face, so the label keeps the color it has
once its call settles."
  (if (eq mevedel-view-tool-spinner-style 'shimmer)
      (let ((face (or (get-text-property 0 'font-lock-face text)
                      'mevedel-view-ephemeral)))
        (propertize text
                    'mevedel-view-inline-spinner-frame t
                    'mevedel-view-tool-animation-id id
                    'mevedel-view-tool-animation-face face
                    'display (mevedel-view-animation-frame
                              'shimmer (substring-no-properties text) 0 face
                              (selected-frame))))
    text))

(defun mevedel-view--running-header (line count &optional virtual failed names)
  "Return group header LINE marked running with COUNT calls in flight.
The settled marker gives way to the running one, and `· N running'
follows the summary; FAILED failed calls add `· N failed' naming the tools
in NAMES, since a folded group cannot show their rows.  With no call left
running, a failure takes the `×' marker.  Unless VIRTUAL, the replaced
marker is kept on the new one so a stopped turn can restore it in place."
  (let ((marker (if count
                    (mevedel-view--pending-marker 'pending-run)
                  (propertize "  × " 'font-lock-face 'error)))
        (sep (propertize " · " 'font-lock-face 'mevedel-view-tool-metadata)))
    (concat
     (propertize marker
                 'mevedel-view-pending-tool-live t
                 'mevedel-view-settled-marker
                 (and (not virtual) (substring line 0 4)))
     (substring line 4)
     (propertize
      (concat (when count
                (concat sep
                        (mevedel-view--pending-label
                         (propertize (format "%d running" count)
                                     'font-lock-face 'mevedel-view-tool-metadata)
                         'pending-run)))
              (when failed
                (concat sep (propertize (mevedel-view--failed-label failed names)
                                        'font-lock-face 'error))))
      'mevedel-view-pending-tool-live t))))

(defun mevedel-view--pending-children (entries)
  "Return activity group children for in-flight tool ENTRIES."
  (mapcar (lambda (entry)
            (list :kind 'pending :tool (nth 1 entry) :entry entry))
          entries))

(defun mevedel-view--pending-entry-settled-p (entry)
  "Return non-nil when in-flight ENTRY finished before its result landed.
gptel writes a parallel batch's results only after the whole batch, so a
call the pipeline reports as finished keeps its place in the run until
then, shown finished instead of running."
  (plist-get (nth 3 entry) :settled))

(defun mevedel-view--pending-running-count (entries)
  "Return how many in-flight ENTRIES are still running, or nil for none."
  (let ((count (cl-count-if-not #'mevedel-view--pending-entry-settled-p
                                entries)))
    (and (> count 0) count)))

(defun mevedel-view--pending-failed-count (entries)
  "Return how many in-flight ENTRIES finished failed, or nil for none."
  (let ((count (cl-count-if
                (lambda (entry)
                  (memq (mevedel-view--pending-entry-settled-p entry)
                        '(error failed denied blocked)))
                entries)))
    (and (> count 0) count)))

(defun mevedel-view--pending-failed-names (entries)
  "Return the tool names of in-flight ENTRIES that finished failed."
  (delq nil (mapcar (lambda (entry)
                      (and (memq (mevedel-view--pending-entry-settled-p entry)
                                 '(error failed denied blocked))
                           (or (nth 1 entry) "Tool")))
                    entries)))

(defun mevedel-view--insert-pending-tool-row (entry)
  "Insert the row for in-flight tool ENTRY.
ENTRY is (KEY NAME DETAIL METADATA).  The row uses the settled tool row's
`Tool: argument' grammar; shimmer animates the tool name.  A call that
finished before its result landed takes the settled marker and is still."
  (let ((key (car entry))
        (detail (nth 2 entry))
        (settled (mevedel-view--pending-entry-settled-p entry))
        (start (point)))
    (insert (cond
             ((memq settled '(error failed denied blocked))
              (propertize "  × " 'font-lock-face 'error))
             (settled
              (propertize "  ✓ " 'font-lock-face 'mevedel-view-tool-marker))
             (t (mevedel-view--pending-marker key)))
            (let ((name (propertize (or (nth 1 entry) "Tool")
                                    'font-lock-face 'mevedel-view-tool-name)))
              (if settled name (mevedel-view--pending-label name key)))
            (if (and detail (not (string-empty-p detail)))
                (concat ": " (propertize detail 'font-lock-face
                                         'mevedel-view-tool-argument))
              ""))
    (when-let* ((hint (mevedel-view--permission-approval-hint
                      (mevedel-view--permission-approval-reason (nth 3 entry)))))
      (insert "  " hint))
    (insert "\n")
    (add-text-properties start (point)
                         `(mevedel-view-type tool-pending
                           mevedel-view-pending-key ,key
                           mevedel-view-pending-tool-use-id
                           ,(plist-get (nth 3 entry) :tool-use-id)
                           mevedel-view-pending-tool-live t))
    (mevedel-view--apply-rendering-indent start (point))))

(defun mevedel-view--insert-pending-run (entries)
  "Insert in-flight tool ENTRIES that no settled activity run absorbed.
Above the grouping threshold they form one running group row, as they
will once settled; otherwise each call gets its own running row.  When a
reader is on one of their rows, the group is drawn open beneath its
header, so that row stays on screen."
  (mevedel-view--ensure-blank-line-after-response)
  (if (and (> mevedel-view-tool-group-collapse-threshold 0)
           (> (length entries) mevedel-view-tool-group-collapse-threshold))
      (let* ((start (point))
             (data-buf (mevedel-view-segments-display-buffer))
             (data-end (and (buffer-live-p data-buf)
                            (with-current-buffer data-buf (point-max))))
             (open (or (mevedel-view-render--reader-on-member-p nil entries)
                       (and data-end
                            (eq (car mevedel-view-render--pending-reader-group)
                                mevedel-view--data-turn-start)
                            (eql (cadr mevedel-view-render--pending-reader-group)
                                 data-end)))))
        (when (and open data-end)
          (setq mevedel-view-render--pending-reader-group
                (list mevedel-view--data-turn-start data-end)))
        (mevedel-view--insert-summary-region
         (mevedel-view--rendering-header-line
          (list :header (mevedel-view--tool-group-header
                         (mevedel-view--pending-children entries))
                :vtype 'tool-group
                :pending-count (mevedel-view--pending-running-count entries)
                :pending-failed (mevedel-view--pending-failed-count entries)
                :failed-names (mevedel-view--pending-failed-names entries)
                :pending-virtual t))
         '(mevedel-view-type tool-pending
           mevedel-view-pending-tool-live t))
        (mevedel-view--apply-rendering-indent start (point))
        (when open
          (mapc #'mevedel-view--insert-pending-tool-row entries)))
    (mapc #'mevedel-view--insert-pending-tool-row entries)))

(defun mevedel-view--retire-pending-decorations (start end)
  "Remove in-flight tool decorations between START and END.
Virtual rows disappear; a running group regains its settled marker and
loses its running count.  Return non-nil when anything changed."
  (let ((inhibit-read-only t)
        (end (copy-marker end))
        changed)
    (save-excursion
      (let ((pos start))
        (while (and (< pos end)
                    (setq pos (text-property-any
                               pos end 'mevedel-view-pending-tool-live t)))
          (let* ((settled (get-text-property pos 'mevedel-view-settled-marker))
                 (stop (min (or (next-single-property-change
                                 pos 'mevedel-view-pending-tool-live nil end)
                                end)
                            (or (next-single-property-change
                                 pos 'mevedel-view-settled-marker nil end)
                                end))))
            (setq changed t)
            (if (not settled)
                (delete-region pos stop)
              ;; Keep the row's section identity, not the running look.
              (let ((section
                     (cl-loop for (key value) on (text-properties-at pos)
                              by #'cddr
                              unless (memq key '(font-lock-face
                                                 face display
                                                 mevedel-view-pending-tool-live
                                                 mevedel-view-settled-marker
                                                 mevedel-view-inline-spinner-frame
                                                 mevedel-view-tool-animation-id))
                              append (list key value))))
                (delete-region pos stop)
                (goto-char pos)
                (insert settled)
                (add-text-properties pos (point) section)
                (setq pos (point))))))))
    (set-marker end nil)
    changed))

(defun mevedel-view--strip-pending-decorations-from-string (text)
  "Return TEXT without in-flight tool decorations, or nil when empty."
  (when text
    (with-temp-buffer
      (insert text)
      (let ((inhibit-modification-hooks t))
        (mevedel-view--retire-pending-decorations (point-min) (point-max)))
      (let ((str (buffer-string)))
        (unless (string-empty-p str)
          str)))))

(defun mevedel-view--request-progress-anchor ()
  "Return where the foreground request progress row should be inserted.
Request progress lives after status and interaction rows, directly
before the input zone.  Fall back to the history boundary only when the
input boundary cannot be recovered."
  (or (let ((input-pos (and (markerp mevedel-view--input-marker)
                            (marker-buffer mevedel-view--input-marker)
                            (eq (marker-buffer mevedel-view--input-marker)
                                (current-buffer))
                            (mevedel-view--input-marker-position))))
        (and input-pos
             (<= (point-min) input-pos)
             (<= input-pos (point-max))
             input-pos))
      (mevedel-view--history-boundary-target)))

(defmacro mevedel-view-render--with-boundaries-advancing (&rest body)
  "Execute BODY while view zone boundary markers advance."
  (declare (indent 0) (debug t))
  `(mevedel-view--call-with-render-boundaries-advancing
    (lambda () ,@body)))

(defun mevedel-view--recover-in-flight-turn-start
    (data-from history-start history-end)
  "Recover an in-flight turn start between HISTORY-START and HISTORY-END.
DATA-FROM is the first data-buffer position for the in-flight turn."
  (when (and data-from history-start history-end (< history-start history-end))
    ;; Step by property runs: the view holds the whole transcript, and a
    ;; per-character walk cost a third of every streamed update.
    (let ((pos history-start)
          first-source)
      (while (and (< pos history-end) (not first-source))
        ;; A source whose marker no longer points anywhere, such as one into
        ;; a killed archive, cannot start the in-flight turn.
        (let ((start (mevedel-view-disclosure-source-start
                      (get-text-property pos 'mevedel-view-source))))
          (if (and start (>= start data-from))
              (setq first-source pos)
            (setq pos (next-single-property-change
                       pos 'mevedel-view-source nil history-end)))))
      (when first-source
        (let ((scan first-source)
              header)
          (while (and (> scan history-start) (not header))
            (setq scan (previous-single-property-change
                        scan 'mevedel-view-type nil history-start))
            (when (eq (get-text-property scan 'mevedel-view-type)
                      'turn-header)
              (setq header scan)))
          (or header first-source))))))

(defun mevedel-view--pre-rendered-user-visible-p ()
  "Return non-nil when the current in-flight marker follows a user block.
This detects whether the send-path echo inserted by
`mevedel-view--insert-user-message' is still present.  A full rerender
can wipe that ephemeral block while an in-flight marker remains live, so
the marker alone is not enough to decide whether a leading user turn
from the data buffer should be filtered."
  (when-let* ((pos (mevedel-view-stream-in-flight-turn-start-position))
              ((> pos (point-min))))
    (save-excursion
      (goto-char pos)
      (skip-chars-backward " \t\n")
      (and (> (point) (point-min))
           (memq (get-text-property (1- (point)) 'mevedel-view-type)
                 '(user user-input-summary))))))

(defun mevedel-view--render-live-region (data-buf settle-p &optional start end)
  "Render the in-flight turn from DATA-BUF, settling it when SETTLE-P.

Call from the view buffer.  Deletes the region between variable
`mevedel-view--in-flight-turn-start' and the history boundary (the
current rendering of the in-flight assistant turn) and re-renders from
the data buffer range \[`mevedel-view--data-turn-start',
end-of-data-buffer], grouping segments into turns and rendering them at
the history boundary.

Unless SETTLE-P, calls in `mevedel-view--pending-tool-calls' join the
last turn's trailing activity run as running calls, or follow the turn
when it ends in something they cannot join, so the user sees what is
running before results land in the data buffer.  They are drawn by this
render only, and change the marker, not the layout, once settled.

Assistant streaming remains data-buffer-derived full/incremental turn
rerendering here: transcript parsing, collapse recovery, and final
response reconciliation still need the authoritative data buffer.

Optional START / END are used by the post-response path to decide
whether the caller already has explicit segment coordinates.  When
supplied, they are preferred over the marker-based range so the
final `--render-response' invocation still gets gptel's authoritative
response bounds.

User turns inside the extracted range are filtered only when the
current user input is still visible immediately before the in-flight
assistant marker.  The user's input is echoed by
`mevedel-view--insert-user-message' at send time, but a full rerender
can remove that ephemeral echo before the final response render runs.
In that case the user turn must be rendered from the data buffer.

Section-level collapse state (expanded thinking block, collapsed
tool summary, …) is captured before the delete and re-applied after
the render so user toggles survive streaming ticks."
  (let* ((mevedel-transcript--tool-block-index (make-hash-table :test #'eq))
         (mevedel-view--path-link-memo (make-hash-table :test #'equal))
         (mevedel-transcript-audit--decode-cache (make-hash-table :test #'equal))
         (retained-p (and (not settle-p)
                          (mevedel-view--live-tail-valid-p data-buf)))
         (turn-from (and (markerp mevedel-view--data-turn-start)
                         (marker-position mevedel-view--data-turn-start)))
         (data-from
          (if retained-p
              (marker-position mevedel-view--live-data-tail-start)
            (cond
             ((and start turn-from) (min start turn-from))
             (start)
             (turn-from))))
         (data-to
          (or end
              (with-current-buffer data-buf (point-max))))
         (segments (when (and data-from data-to)
                     (with-current-buffer data-buf
                       (if retained-p
                           (save-restriction
                             (narrow-to-region data-from data-to)
                             (mevedel-transcript-segments
                              (point-min) (point-max)))
                         (mevedel-transcript-segments data-from data-to)))))
         (segments (if settle-p
                       segments
                     (mevedel-view--split-live-response-tail
                      segments data-buf)))
         (turns (mevedel-view--group-transcript-turns segments data-buf))
         (in-flight-p
          (if retained-p
              (marker-position mevedel-view--live-view-tail-start)
            (mevedel-view-stream-in-flight-turn-start-position)))
         (pre-rendered-user-visible-p
          (mevedel-view--pre-rendered-user-visible-p))
         (pending mevedel-view--pending-tool-calls))
    (mevedel-view--debug-log
     'incremental-extract
     :state (mevedel-view--debug-state data-buf data-from data-to)
     :start start
     :end end
     :data-from data-from
     :data-to data-to
     :segments (length segments)
     :turns (mapcar (lambda (turn) (plist-get turn :role)) turns)
     :turn-detail (mevedel-view--debug-turn-summary turns data-buf)
     :pre-rendered-user mevedel-view--user-pre-rendered
     :pre-rendered-user-visible pre-rendered-user-visible-p)
    ;; Filter the send-path user turn.  `mevedel-transcript-segments' expands a
    ;; start position back to the containing `gptel' property run, so a
    ;; data-turn marker sitting at the end of the prompt can still yield
    ;; a leading user turn whose source starts before DATA-FROM.  That is
    ;; the prompt already echoed by the send path, not new mailbox/user
    ;; content that arrived later in the turn.
    (while (and turns
                (eq (plist-get (car turns) :role) 'user)
                (or mevedel-view--user-pre-rendered
                    pre-rendered-user-visible-p
                    (and data-from
                         (< (or (plist-get (car turns) :start)
                                data-from)
                            data-from))))
      (setq turns (cdr turns)))
    (setq mevedel-view--user-pre-rendered nil)
    (mevedel-view--debug-log
     'incremental-filtered
     :turns (mapcar (lambda (turn) (plist-get turn :role)) turns)
     :turn-detail (mevedel-view--debug-turn-summary turns data-buf)
     :pending pending
     :state (mevedel-view--debug-state data-buf data-from data-to))
    (mevedel-view-render--preserving-window-state
     (mevedel-view--call-preserving-input-text
      (lambda ()
        ;; rebuild region stops at status-marker (top of zone
        ;; 2) rather than input-marker, so any future status- or
        ;; interaction-zone overlay anchors survive the re-render.
        ;; status-marker == input-marker today (zones empty), so this
        ;; is a no-op for current behavior; setting it correctly now
        ;; prevents a phase-8 regression when zone overlays land.
        (let ((inhibit-read-only t)
              (inhibit-modification-hooks t)
              ;; Running rows are redrawn below; keep their animation phase.
              ;; A full projection may already have removed them.
              (preserved-tools
               (when pending
                 (append (mevedel-view--snapshot-tool-animation-targets)
                         (prog1 mevedel-view--carried-tool-samples
                           (setq mevedel-view--carried-tool-samples nil)))))
              (animated (or pending mevedel-view--spinner-tool-targets)))
          ;; With nothing to redraw, calls that finished leave no row that a
          ;; replacement would remove.
          (unless (or turns pending)
            (mevedel-view--retire-pending-tool-rows))
          (let* (;; Permission prompts and tool callbacks can trigger a view
                 ;; refresh before gptel has written any result segments.  In
                 ;; that case keep the previous in-flight rendering instead of
                 ;; replacing it with a blank region.
                 (replace-p (or turns pending))
                 ;; Reject markers that pass `markerp' but are detached
                 ;; (`marker-position' returns nil): they would crash
                 ;; `<=' / `delete-region' / `apply-collapse-states' below.
                 (history-start (mevedel-view--after-header-position))
                 (history-tail (mevedel-view--history-tail-position))
                 (status-pos (and (markerp mevedel-view--status-marker)
                                  (marker-position mevedel-view--status-marker)))
                 (status-valid-p
                  (and status-pos
                       (>= status-pos history-tail)
                       (not (mevedel-view--non-history-view-position-p
                             status-pos))
                       (not (and (> status-pos history-start)
                                 (mevedel-view--non-history-view-position-p
                                  (1- status-pos))))))
                 (rebuild-end
                  (or (and status-valid-p mevedel-view--status-marker)
                      (copy-marker history-tail t)
                      (and (markerp mevedel-view--input-marker)
                           (marker-position mevedel-view--input-marker)
                           mevedel-view--input-marker)))
                 (rebuild-end-pos (marker-position rebuild-end))
                 ;; A retained tail already knows where to delete from.
                 (recovered-start
                  (unless (and retained-p in-flight-p)
                    (mevedel-view--recover-in-flight-turn-start
                     data-from history-start rebuild-end-pos)))
                 (delete-start
                  (or (and retained-p in-flight-p)
                      (and in-flight-p
                           (>= in-flight-p history-start)
                           (<= in-flight-p rebuild-end-pos)
                           (not (and recovered-start
                                     (< in-flight-p recovered-start)
                                     (mevedel-view--transcript-history-position-p
                                      in-flight-p)))
                           in-flight-p)
                      recovered-start))
                 (capture-p
                  (and delete-start
                       rebuild-end
                       (<= delete-start rebuild-end-pos)))
                 (saved-states
                  (when (and replace-p capture-p)
                    (mevedel-view-disclosure-capture-state
                     delete-start
                     rebuild-end-pos)))
                 (render-start nil))
            (mevedel-view--debug-log
             'incremental-decision
           :replace-p replace-p
           :capture-p capture-p
           :rebuild-end (mevedel-view--debug-marker-position rebuild-end)
           :saved-states (length saved-states)
           :state (mevedel-view--debug-state data-buf data-from data-to))
          ;; Wipe the current in-flight assistant turn render (if any)
          ;; so we can re-render it from scratch from the updated data.
          (when (and replace-p capture-p)
            (mevedel-view--debug-log
             'incremental-delete
             :region (mevedel-view--debug-region
                      delete-start
                      rebuild-end-pos)
             :state (mevedel-view--debug-state data-buf data-from data-to))
            (delete-region delete-start rebuild-end)
            (mevedel-view--debug-log
             'incremental-after-delete
             :state (mevedel-view--debug-state data-buf data-from data-to)))
          (when replace-p
            ;; The in-flight turn belongs to the history region.  Insert it
            ;; at the status boundary so any real-text status/interaction
            ;; UI below that boundary remains below the transcript.
            (let ((mevedel-view--render-insertion-marker rebuild-end)
                  (continuation-p retained-p)
                  (in-flight (and pending (not settle-p) (list pending))))
              (setq render-start (marker-position rebuild-end))
              (let ((rest turns))
                (while rest
                  ;; In-flight calls belong to the last turn's trailing run.
                  (let ((mevedel-view--render-pending
                         (and (null (cdr rest)) in-flight)))
                    (mevedel-view--render-turn
                     (car rest) data-buf nil nil
                     (prog1 continuation-p
                       (setq continuation-p nil))))
                  (setq rest (cdr rest))))
              ;; Calls that no trailing run absorbed follow the turn.
              (when-let* ((left (car in-flight)))
                (save-excursion
                  (goto-char rebuild-end)
                  (mevedel-view-render--with-boundaries-advancing
                    ;; A response that opens with tool calls has no turn in
                    ;; the transcript yet.  Draw its heading now, so the row
                    ;; does not move when the first result brings it.  It is
                    ;; part of the running decoration: a turn stopped before
                    ;; any result removes it with the rows.
                    (let ((interim (mevedel-view--live-turn-needs-header-p
                                    (min (or (mevedel-view-stream-in-flight-turn-start-position)
                                             render-start)
                                         render-start)
                                    (point) data-buf)))
                      (when interim
                        (mevedel-view--insert-assistant-header
                         nil '(mevedel-view-pending-tool-live t)))
                      (mevedel-view--insert-pending-run left)
                      ;; The interim turn ends like the turn it stands for,
                      ;; so the rows below stay put when results land.
                      (when interim
                        (insert (propertize "\n" 'mevedel-view-pending-tool-live t)
                                (propertize "\n"
                                            'font-lock-face 'mevedel-view-turn-rule
                                            'mevedel-view-pending-tool-live t)))))))
              (mevedel-view--ensure-request-progress data-buf))
            (mevedel-view--debug-log
             'incremental-after-insert
             :state (mevedel-view--debug-state data-buf data-from data-to)))
          ;; Restore user-toggled collapse/expand state that the delete
          ;; above just wiped.  Walk the freshly rendered span and toggle
          ;; only sections whose saved state differs from the default.
          ;; Record the complete final unit before a toggle can split its
          ;; source property.  Earlier disclosures may change length without
          ;; changing this unit; rescanning after restore can instead pick
          ;; only the last fragment of a rewritten unit.
          (when replace-p
            (if settle-p
                (mevedel-view-render-invalidate-live-tail)
              (mevedel-view--retain-last-live-render-unit
               data-buf render-start (marker-position rebuild-end))))
          (let ((tail-first-end
                 (when mevedel-view--live-view-tail-start
                   (copy-marker (1+ mevedel-view--live-view-tail-start) nil)))
                (tail-end
                 (when mevedel-view--live-view-tail-start
                   (copy-marker
                    (next-single-property-change
                     mevedel-view--live-view-tail-start
                     'mevedel-view-live-unit-source nil
                     (marker-position rebuild-end))
                    t)))
                restored completed)
            (when mevedel-view--live-view-tail-start
              ;; Follow insertions before the unit.  FIRST-END distinguishes
              ;; those from a replacement that consumes its first character.
              (set-marker-insertion-type mevedel-view--live-view-tail-start t))
            (unwind-protect
                (progn
                  (setq restored
                        (when (and saved-states delete-start
                                   (marker-position rebuild-end))
                          (mevedel-view-disclosure-restore-state
                           delete-start (marker-position rebuild-end)
                           saved-states)))
                  (setq completed t))
              (when (or (not completed)
                        (and restored
                             (not (and tail-end
                                       (mevedel-view--live-tail-intact-p
                                        data-buf
                                        (marker-position tail-first-end)
                                        (marker-position tail-end))))))
                (mevedel-view-render-invalidate-live-tail))
              (when mevedel-view--live-view-tail-start
                (set-marker-insertion-type
                 mevedel-view--live-view-tail-start nil))
              (when tail-first-end (set-marker tail-first-end nil))
              (when tail-end (set-marker tail-end nil))))
          (when (and animated replace-p (marker-position rebuild-end))
            (mevedel-view--adopt-tool-animation-targets
             (or render-start (point-min)) (marker-position rebuild-end)
             preserved-tools))
          (mevedel-view--ensure-request-progress data-buf)
          (unless mevedel-view--agent-transcript-p
            (mevedel-view--render-agent-status)
            (mevedel-view--interaction-rebuild)
            (when (fboundp 'mevedel-directive-frame-refresh-filter)
              (mevedel-directive-frame-refresh-filter))))))))))

(defvar-local mevedel-view-render--owner nil
  "Non-nil while this view has an active projection writer.")

(defvar-local mevedel-view-render--pending nil
  "Coalesced (KEY GENERATION SOURCE FUNCTION CLEANUP) projection intentions.")

(defvar-local mevedel-view-render--generation 0
  "Identity of the current source/turn mutation generation.")

(defvar-local mevedel-view-render--terminal-p nil
  "Non-nil once terminal presentation has been requested for this turn.")

(defvar-local mevedel-view-render--terminal-turn nil
  "Data-turn marker belonging to the latest terminal presentation intent.")

(defun mevedel-view-render-terminal ()
  "Invalidate incremental work for the current turn, without changing text."
  (setq mevedel-view-render--terminal-p t
        mevedel-view-render--terminal-turn mevedel-view--data-turn-start))

(defun mevedel-view-render-mutate (key function &optional replacement cleanup)
  "Run FUNCTION as this view's sole projection writer.
Nested requests coalesce by KEY and run after the active writer unwinds.
They carry the displayed source identity, never saved view coordinates.
REPLACEMENT invalidates queued work from the previous source or turn.
Optional CLEANUP must run even if replacement invalidates FUNCTION.
Return FUNCTION's value when immediate, or t when queued.  Composed internal
operations call their owned implementations rather than entering here again."
  (when replacement
    (mevedel-view-prepare-cancel)
    (cl-incf mevedel-view-render--generation)
    ;; Discard obsolete projection, not its mandatory terminal release.
    ;; Releases stay ahead of the replacement so they cannot clear its state.
    (setq mevedel-view-render--pending
          (cl-remove-if-not (lambda (entry) (nth 4 entry))
                            mevedel-view-render--pending)))
  (let ((entry (list key mevedel-view-render--generation
                     (mevedel-view-segments-display-buffer) function cleanup)))
    (if mevedel-view-render--owner
        (progn
          (if-let* ((previous (assoc key mevedel-view-render--pending)))
              (setcdr previous (cdr entry))
            (setq mevedel-view-render--pending
                  (nconc mevedel-view-render--pending (list entry))))
          t)
      (setq mevedel-view-render--owner t)
      ;; Fontification can yield to redisplay after a writer deletes its old
      ;; projection.  Keep intermediate text hidden through the queue drain.
      (let ((inhibit-redisplay t)
            failure result)
        (cl-labels
            ((run (work)
               (pcase-let ((`(,work-key ,generation ,source ,thunk ,release) work))
                 (unwind-protect
                     (condition-case err
                         (when (and (= generation mevedel-view-render--generation)
                                    (eq source (mevedel-view-segments-display-buffer)))
                           ;; Transcript writers need a complete projection.
                           ;; Chrome uses live zone markers, so a nested status
                           ;; update must not synchronously finish the batch.
                           (when (and mevedel-view-render--batch
                                      (not (memq work-key
                                                 '(full-batch status interaction))))
                             (mevedel-view-render-cancel-batch)
                             (unless (eq work-key 'full)
                               (mevedel-view-render--full-now)))
                           ;; Rows a reader is on when the writer deletes
                           ;; them must not fold out from under them.
                           (let ((mevedel-view-render--reader-members
                                  (mevedel-view-render--reader-members)))
                             (funcall thunk)))
                       ((error quit) (unless failure (setq failure err)) nil))
                   (when release
                     (condition-case err
                         (funcall release)
                       ((error quit) (unless failure (setq failure err)))))))))
          (unwind-protect
              (progn
                (unwind-protect
                    (setq result (run entry))
                  ;; A queued writer's failure must not starve the terminal
                  ;; or replacement behind it.  Report the first error only
                  ;; after every still-current intention has been drained.
                  (while mevedel-view-render--pending
                    (run (pop mevedel-view-render--pending)))
                  ;; Writers change how the transcript ends without
                  ;; redrawing the rail below it.
                  (unless failure
                    (condition-case err
                        (mevedel-view--repair-request-progress-separator)
                      (error (setq failure err)))))
                (if failure (signal (car failure) (cdr failure)) result))
            (setq mevedel-view-render--owner nil
                  mevedel-view-render--pending nil)))))))

(defun mevedel-view-render-live-update (data-buf)
  "Update the current in-flight turn from DATA-BUF's mutable tail.
Nothing happens while the view projects an archived segment: the live
turn is not on display, and drawing it would replace that segment's text."
  (unless (eq mevedel-view-render--terminal-turn mevedel-view--data-turn-start)
    (setq mevedel-view-render--terminal-p nil))
  (unless (or mevedel-view-render--terminal-p
              (mevedel-view-historical-segment-p))
    (mevedel-view-render-mutate
     'live
     (lambda ()
       (unless (or mevedel-view-render--terminal-p
                   (mevedel-view-historical-segment-p))
         (mevedel-view--render-live-region data-buf nil))))))

(defun mevedel-view-render-settle (data-buf start end)
  "Exactly reconcile DATA-BUF's completed response from START through END.
Disclosure state keys are computed with their durable post-settle
anchors: this render runs before the stream clears the in-flight turn
markers, and keys captured or stamped under the temporary `(in-flight)'
anchor here would be orphaned the moment those markers clear, collapsing
sections the user expanded during the turn on the next render."
  (mevedel-view-render-terminal)
  (let ((start (with-current-buffer data-buf (copy-marker start)))
        (end (with-current-buffer data-buf (copy-marker end t))))
    (mevedel-view-render-mutate
     'settle
     (lambda () (mevedel-view-render--settle-now data-buf start end)))))

(defun mevedel-view-render--settle-now (data-buf start end)
  "Reconcile DATA-BUF's START..END with projection ownership already held."
  (mevedel-view-render-invalidate-live-tail)
  (let ((mevedel-view-disclosure--settling-p t))
    (mevedel-view--render-live-region data-buf t start end)))

(defun mevedel-view--conversation-variant-button
    (data-buf source-start source-end &optional session)
  "Return DATA-BUF's variant switch for SOURCE-START..SOURCE-END, or nil.
SESSION supplies live session context when DATA-BUF is archived."
  (when-let* ((session
               (or session
                   (buffer-local-value 'mevedel--session data-buf)))
              ((mevedel-session-save-path session))
              (fork-point
               (mevedel-session-artifacts-fork-point-at-source
                data-buf source-start source-end))
              (fork-point-id (plist-get fork-point :fork-point-id))
              (variants
               (progn
                 (mevedel-session-persistence-conversation-variants
                  session fork-point-id
                  mevedel-view--conversation-variant-sessions)))
              ((> (length variants) 1))
              (current
               (cl-find
                (mevedel-session-session-id session)
                variants
                :test #'equal
                :key (lambda (entry)
                       (plist-get (plist-get entry :summary)
                                  :session-id)))))
    (let* ((origin (plist-get current :variant-origin))
           (label
            (pcase origin
              ('source "Source")
              ('conversation "Conversation")
              ('worktree "Worktree")))
           (text
            (format "[⇆ %s · %d variants]" label (length variants))))
      (propertize
       text
       'face 'link
       'mouse-face 'highlight
       'help-echo "Switch conversation variant"
       'mevedel-view-variant-fork-point-id fork-point-id
       'mevedel-view-zone-activate
       (lambda ()
         (mevedel-view-switch-conversation-variant fork-point-id))))))

(defun mevedel-view--render-turn
    (turn data-buf &optional decorate-variants variant-session continuation-p)
  "Render a single TURN into the view buffer at the input marker.
DATA-BUF is the gptel data buffer for reading source content.
TURN is a plist with :role, :segments, :start, :end.
DECORATE-VARIANTS adds conversation variant switches for settled history.
VARIANT-SESSION supplies their live session context when DATA-BUF is archived.
CONTINUATION-P appends an already-started assistant turn without a new header."
  (let ((role (plist-get turn :role))
        (segments (plist-get turn :segments))
        (turn-start (plist-get turn :start))
        (turn-end (plist-get turn :end))
        (directive (plist-get turn :directive))
        (context (unless continuation-p
                   (with-current-buffer data-buf
                     (list :data data-buf :tick (buffer-modified-tick)
                           :turn turn :start (point-min) :end (point-max)
                           :session variant-session
                           :variants mevedel-view--conversation-variant-sessions))))
        (turn-source nil))
    (setq turn-source
          (mevedel-view-disclosure-source-range data-buf turn-start turn-end))
    ;; Skip user turns that are empty after cleaning (e.g., turns
    ;; containing only org reasoning markers or response separators).
    (unless (and (eq role 'user)
                 (string-empty-p
                  (mevedel-view--user-turn-text segments data-buf))
                 (null (mevedel-view--user-turn-prompt-drawers
                        segments data-buf)))
    (save-excursion
      (let ((target (mevedel-view--current-render-insertion-marker)))
        (goto-char target)
        ;; Temporarily let the marker advance past our insertions so
        ;; successive turns are appended in order.  Incremental renders
        ;; bind TARGET to the status boundary; full renders use the
        ;; input marker.
        (mevedel-view-render--with-boundaries-advancing
          (let ((inhibit-read-only t)
                (insert-start (point)))
            (pcase role
              ('task-background
               (mevedel-view--render-task-background
                segments data-buf turn-source))
              ('user
               (mevedel-view--render-user-turn
                segments data-buf directive))
              ('assistant
               (mevedel-view--render-assistant-turn
                segments data-buf
                (and decorate-variants
                     (mevedel-view--conversation-variant-button
                      data-buf turn-start turn-end variant-session))
                directive continuation-p)))
            ;; Trailing separator: a rule closes an assistant turn, a plain
            ;; spacer follows any other turn that rendered something.
            (cond
             ((not (eq role 'assistant))
              (when (> (point) insert-start)
                (insert (propertize "\n" 'font-lock-face
                                    'mevedel-view-separator))))
             ;; The request summary already drew the closing rule.  Leave
             ;; one row under it before the next heading.
             ((eq (mevedel-view--previous-rendered-type) 'request-summary)
              (insert (propertize "\n" 'font-lock-face
                                  'mevedel-view-separator)))
             (t
              ;; Blank line above the rule so it doesn't butt up against
              ;; the last response line.
              (mevedel-view--ensure-blank-line-before-response)
              (insert (propertize "\n" 'font-lock-face
                                  'mevedel-view-turn-rule))))
            ;; Apply read-only to the entire block.  Per-segment source
            ;; coordinates are set by the individual render functions;
            ;; tag text that has no segment-level source with the turn
            ;; bounds (headers, separators).
            (mevedel-view-render-add-display-properties
             insert-start (point))
            ;; Fill in source on regions that have none yet (headers,
            ;; separators) so the entire block is navigable.  Mailbox
            ;; deliveries are locally toggled cards; do not stamp them
            ;; with the enclosing assistant turn source, otherwise TAB
            ;; can reinterpret them as the previous source-backed tool.
            (let ((pos insert-start))
              (while (< pos (point))
                (let* ((source-next
                        (or (next-single-property-change
                             pos 'mevedel-view-source nil (point))
                            (point)))
                       (type-next
                        (or (next-single-property-change
                             pos 'mevedel-view-type nil (point))
                            (point)))
                       (next (min source-next type-next)))
                  (cond
                   ((eq (get-text-property pos 'mevedel-view-type)
                        'mailbox-delivery)
                    (remove-text-properties
                     pos next
                     '(mevedel-view-source nil
                       mevedel-view-source-key nil
                       mevedel-view-agent-handle-p nil
                       mevedel-view-agent-status nil))
                    (setq pos next))
                   ((get-text-property pos 'mevedel-view-source)
                    (setq pos next))
                   (t
                    (put-text-property pos next 'mevedel-view-source
                                       turn-source)
                    (setq pos next))))))
            ;; Tag every character in this turn with a unique id so
            ;; turn-level fold/unfold can find the whole span even after
            ;; inner sections have been expanded or collapsed.
            (add-text-properties
             insert-start (point)
             `(mevedel-view-turn-context ,context
               mevedel-view-turn-id
               ,(or (and continuation-p
                         (> insert-start (point-min))
                         (get-text-property
                          (1- insert-start) 'mevedel-view-turn-id))
                    (plist-get turn :render-id)
                    (gensym "mevedel-view-turn-"))
               ,@(when directive
                   `(mevedel-view-directive ,directive
                     mevedel-view-turn-role directive)))))))))))

(defun mevedel-view--user-turn-display-text (segments data-buf)
  "Return persisted view text from user SEGMENTS in DATA-BUF, or nil."
  (with-current-buffer data-buf
    (cl-loop for seg in segments
             when (eq (car seg) 'render-data)
             for data = (cdr (mevedel-tool-render-data-extract
                              (buffer-substring
                               (cadr seg) (caddr seg))))
             when (and (eq (plist-get data :kind) 'user-display)
                       (stringp (plist-get data :text)))
             return (plist-get data :text))))

(defun mevedel-view--user-turn-text (segments data-buf)
  "Extract cleaned user text from SEGMENTS in DATA-BUF.
Returns the concatenated, trimmed text with org scaffolding removed.
Empty string when the turn contains only whitespace or markers."
  (or (mevedel-view--user-turn-display-text segments data-buf)
      (with-current-buffer data-buf
        (let (parts)
          (dolist (seg segments)
            (let* ((seg-start (cadr seg))
                   (seg-end (caddr seg))
                   (text (buffer-substring seg-start seg-end)))
              ;; An agent task heading shows as its title; its drawer (the
              ;; agent task path) is metadata, not prompt text.
              (when (let ((case-fold-search nil))
                      (string-match "\\`[ \t\n]*\\* \\(Agent Task:\\)" text))
                (setq text (substring text (match-beginning 1)))
                (when (string-match
                       "\\`\\([^\n]*\n\\):PROPERTIES:\n\\(?:.*\n\\)*?:END:\n?"
                       text)
                  (setq text (replace-match "\\1" t nil text))))
              ;; Strip hidden view render-data side channels.
              (setq text (mevedel-view--strip-render-data-display-text text))
              ;; Strip hidden hook audit side channels; they render as separate
              ;; disclosures below the affected transcript artifact.
              (setq text (mevedel--strip-hook-audit-blocks text))
              ;; Strip synthetic review action blocks.  They stay in the data
              ;; buffer so the model can resolve follow-ups like "fix finding 2",
              ;; but the normal view should show only the user's visible prompt.
              (setq text (mevedel-review-strip-user-action-blocks text))
              ;; Strip model-only hook and Goal lifecycle context.
              (setq text (mevedel-view--strip-model-context-blocks text))
              ;; Strip prompt drawer content
              (when (string-match "\\`:PROMPT:\n\\(?:.*\n\\)*?:END:\n?" text)
                (setq text (replace-match "" t t text)))
              ;; Strip leading gptel-org `:PROPERTIES: ... :END:' drawer.
              ;; gptel-org stores per-buffer state (preset, model, system
              ;; prompt, GPTEL_BOUNDS) here; without this strip, the entire
              ;; system prompt leaks into the visible "You" turn on a full
              ;; rerender that didn't pre-narrow past the drawer.
              (when (string-match "\\`[ \t\n]*:PROPERTIES:\n\\(?:.*\n\\)*?:END:\n?" text)
                (setq text (replace-match "" t t text)))
              ;; Strip reasoning and tool markers only when present:
              ;; replacement otherwise copies the whole delivery.
              ;; gptel emits `#+begin_tool ...'
              ;; and `#+end_tool' without the `gptel' text property, so the
              ;; separator text around a tool block appears here as a user
              ;; segment -- skip it, otherwise the raw header would render
              ;; as a spurious "You" turn.
              (dolist (markers '("#\\+\\(?:begin\\|end\\)_reasoning[^\n]*\n?"
                                 "#\\+begin_tool[^\n]*\n?"
                                 "#\\+end_tool[^\n]*\n?"))
                (when (string-match-p markers text)
                  (setq text (replace-regexp-in-string markers "" text))))
              (let ((trimmed (string-trim text)))
                (unless (string-empty-p trimmed)
                  (push trimmed parts)))))
          (if (cdr parts)
              (string-join (nreverse parts) "\n")
            (or (car parts) ""))))))

(defun mevedel-view--strip-model-context-blocks (text)
  "Return TEXT without generated hook or Goal context blocks."
  (let ((case-fold-search (default-value 'case-fold-search)))
    (if (not (or (string-match-p "<\\(hook\\|goal\\)-context\\(?:[ \\t][^>]*\\)?>" text)
                 (string-search "\n\n\n" text)))
        text
      (with-temp-buffer
        (insert text)
        (goto-char (point-min))
        (while (re-search-forward
                "<\\(hook\\|goal\\)-context\\(?:[ \\t][^>]*\\)?>" nil t)
          (let ((start (match-beginning 0))
                (close (format "</%s-context>" (match-string 1))))
            (if (search-forward close nil t)
                (progn
                  (delete-region start (point))
                  (goto-char start))
              (goto-char (point-max)))))
        (goto-char (point-min))
        (while (re-search-forward "\n\\{3,\\}" nil t)
          (replace-match "\n\n" t t))
        (buffer-string)))))

(autoload 'mevedel-view--hook-audit-records-from-text "mevedel-view-audit")
(autoload 'mevedel-view--hook-audit-key "mevedel-view-audit")
(autoload 'mevedel-view--merge-hook-audits "mevedel-view-audit")
(autoload 'mevedel-view--indent-hook-audit-text "mevedel-view-audit")
(autoload 'mevedel-view--hook-audit-value-text "mevedel-view-audit")
(autoload 'mevedel-view--prompt-rewrite-audit-record "mevedel-view-audit")
(autoload 'mevedel-view--user-turn-hook-audits "mevedel-view-audit")
(autoload 'mevedel-view--format-hook-audit-block "mevedel-view-audit")
(autoload 'mevedel-view--insert-hook-audit-block "mevedel-view-audit")
(autoload 'mevedel-view-audit-insert-history-link "mevedel-view-audit")
(autoload 'mevedel-view-audit-show-control-result "mevedel-view-audit")
(autoload 'mevedel-view-audit-breadcrumb-present-p "mevedel-view-audit")
(autoload 'mevedel-view-audit-breadcrumb-status "mevedel-view-audit")
(autoload 'mevedel-view-audit-toggle-hook-audit "mevedel-view-audit")
(autoload 'mevedel-view--decorate-code-blocks-in-range
  "mevedel-view-markdown")
(autoload 'mevedel-view--decorate-local-images-in-range
  "mevedel-view-markdown")
(autoload 'mevedel-view--decorate-markdown-in-range
  "mevedel-view-markdown")
(autoload 'mevedel-view--last-live-response-boundary
  "mevedel-view-markdown")
(autoload 'mevedel-view--linkify-paths-in-range "mevedel-view-markdown")

(defun mevedel-view--inline-skill-prompt-summary-body (text)
  "Return collapsed prompt body for inline-skill TEXT, or nil."
  (when-let* ((data (mevedel-view--inline-skill-render-data-from-text text))
              (body (plist-get data :prompt))
              (body (string-trim body))
              ((not (string-empty-p body))))
    body))

(defun mevedel-view--inline-skill-reminder-body (text)
  "Return the attached-skill reminder body of inline-skill TEXT, or nil."
  (when-let* ((data (mevedel-view--inline-skill-render-data-from-text text))
              (attachments (plist-get data :attachments)))
    (mapconcat (lambda (attachment) (plist-get attachment :body))
               attachments "\n\n")))

(defun mevedel-view--hook-context-unescape (text)
  "Unescape XML entities in hook context TEXT."
  (replace-regexp-in-string
   "&amp;" "&"
   (replace-regexp-in-string
    "&lt;" "<"
    (replace-regexp-in-string
     "&gt;" ">"
     (replace-regexp-in-string
      "&quot;" "\"" (or text "") t t)
     t t)
    t t)
   t t))

(defun mevedel-view--hook-context-event-body (text)
  "Return TEXT without the wrapper newlines added around event bodies."
  (let ((text (or text "")))
    (when (string-prefix-p "\n" text)
      (setq text (substring text 1)))
    (when (string-suffix-p "\n" text)
      (setq text (substring text 0 -1)))
    (mevedel-view--hook-context-unescape text)))

(defun mevedel-view--hook-context-events-from-body (body)
  "Return event-tagged hook context entries parsed from BODY."
  (when (stringp body)
    (let (events)
      (with-temp-buffer
        (insert body)
        (goto-char (point-min))
        (while (re-search-forward
                (concat "<hook-event[ \t\n]+name=\"\\([^\"]+\\)\""
                        "\\(?: source=\"\\([^\"]*\\)\"\\)?"
                        "\\(?: file=\"\\([^\"]*\\)\"\\)?"
                        "\\(?: plugin=\"\\([^\"]*\\)\"\\)?>") nil t)
          (let ((event (mevedel-view--hook-context-unescape
                        (match-string 1)))
                (source (match-string 2))
                (source-file (match-string 3))
                (plugin-name (match-string 4))
                (body-start (point)))
            (when (search-forward "</hook-event>" nil t)
              (let ((event-body
                     (mevedel-view--hook-context-event-body
                      (buffer-substring-no-properties
                       body-start (match-beginning 0)))))
                (unless (string-empty-p event-body)
                  (let ((entry (list :event event :body event-body)))
                    (dolist (field (list (cons :source source)
                                         (cons :source-file source-file)
                                         (cons :plugin-name plugin-name)))
                      (when (cdr field)
                        (setq entry
                              (plist-put
                               entry (car field)
                               (mevedel-view--hook-context-unescape
                                (cdr field))))))
                    (push entry events))))))))
      (nreverse events))))

(defun mevedel-view--hook-context-events-from-text (text)
  "Return generated hook context entries parsed from TEXT."
  (when (stringp text)
    (let (events)
      (with-temp-buffer
        (insert text)
        (goto-char (point-min))
        (while (search-forward "<hook-context>" nil t)
          (let ((body-start (point)))
            (when (search-forward "</hook-context>" nil t)
              (setq events
                    (append
                     events
                     (mevedel-view--hook-context-events-from-body
                      (buffer-substring-no-properties
                       body-start (match-beginning 0)))))))))
      events)))

(defun mevedel-view--segments-raw-text (segments data-buf)
  "Return raw DATA-BUF text covered by SEGMENTS."
  (with-current-buffer data-buf
    (mapconcat
     (lambda (seg)
       (buffer-substring-no-properties (cadr seg) (caddr seg)))
     segments
     "")))

(defun mevedel-view--user-turn-hook-contexts (segments data-buf)
  "Return hook context blocks found in user SEGMENTS from DATA-BUF."
  (with-current-buffer data-buf
    (let (events first-start last-end)
      (dolist (seg segments)
        (when (memq (car seg) '(user hook-context))
          (let ((seg-end (caddr seg)))
            (save-excursion
              (goto-char (cadr seg))
              (while (search-forward "<hook-context>" seg-end t)
                (let ((block-start (match-beginning 0))
                      (body-start (point)))
                  (when (search-forward "</hook-context>" seg-end t)
                    (unless first-start
                      (setq first-start block-start))
                    (setq last-end (point))
                    (setq events
                          (append
                           events
                           (mevedel-view--hook-context-events-from-body
                            (buffer-substring-no-properties
                             body-start (match-beginning 0))))))))))))
      (when events
        (list (list :start first-start
                    :end last-end
                    :events events))))))

(defun mevedel-view--normalize-hook-context-events (value)
  "Return normalized hook context event entries from VALUE."
  (cond
   ((null value) nil)
   ((stringp value)
    (mevedel-view--hook-context-events-from-body value))
   ((and (listp value)
         (keywordp (car-safe value))
         (plist-member value :body))
    (list value))
   ((listp value)
    (delq nil
          (mapcar (lambda (entry)
                    (when (and (listp entry)
                               (keywordp (car-safe entry))
                               (plist-member entry :body))
                      (let ((body (string-trim
                                   (format "%s" (plist-get entry :body)))))
                        (unless (string-empty-p body)
                          (let ((copy (copy-sequence entry)))
                            (setq copy
                                  (plist-put
                                   copy :event
                                   (format "%s" (or (plist-get entry :event)
                                                    "UserPromptSubmit"))))
                            (plist-put copy :body body))))))
                  value)))))

(defun mevedel-view--format-hook-context-block (events expanded)
  "Return display text for hook context EVENTS.
When EXPANDED is non-nil, include each event name and body."
  (let ((events (mevedel-view--normalize-hook-context-events events)))
    (concat
     "  \u25c7 hook context added\n"
     (when expanded
       (mapconcat
        (lambda (entry)
          (let ((body (plist-get entry :body)))
            (mevedel-view-disclosure-indent-text
             (concat
              (plist-get entry :event) "\n"
              (mapconcat
               (lambda (field)
                 (when-let* ((value (plist-get entry (car field))))
                   (format "%s: %s\n" (cdr field) value)))
               '((:source . "source") (:source-file . "file")
                 (:plugin-name . "plugin"))
               "")
              body "\n")
             4)))
        events
        "")))))

(defun mevedel-view--insert-hook-context-block
    (events &optional source expanded)
  "Insert a hook context disclosure for EVENTS.
SOURCE, when non-nil, is the source range in the data buffer.
EXPANDED means insert the disclosure body expanded."
  (when-let* ((events (mevedel-view--normalize-hook-context-events events)))
    (let ((start (point))
          (id (gensym "mevedel-hook-context-")))
      (insert (mevedel-view--format-hook-context-block events expanded))
      (add-text-properties
       start (point)
       `(font-lock-face mevedel-view-hook-context
         mevedel-view-type hook-context
         mevedel-view-collapsed ,(not expanded)
         mevedel-view-hook-context-id ,id
         mevedel-view-hook-context-events ,events
         mevedel-view-source ,source
         mevedel-view-source-key ,(mevedel-view-disclosure-state-key
                                   source 'hook-context))))))

(defun mevedel-view-render-toggle-hook-context ()
  "Toggle a hook context disclosure."
  (let* ((bounds (or (mevedel-view-disclosure-section-bounds
                      'mevedel-view-hook-context-id)
                     (mevedel-view-disclosure-section-bounds)))
         (source (and bounds
                      (get-text-property
                       (car bounds) 'mevedel-view-source)))
         (events (or (and bounds
                          (get-text-property
                           (car bounds) 'mevedel-view-hook-context-events))
                     (and source
                          (buffer-live-p
                           (mevedel-view-segments-display-buffer))
                          (mevedel-view--hook-context-events-from-text
                           (mevedel-view-disclosure-data-substring
                            (mevedel-view-segments-display-buffer)
                            (car source)
                            (cdr source))))))
         (collapsed (and bounds
                         (get-text-property
                          (car bounds) 'mevedel-view-collapsed)))
         (turn-id (and bounds
                       (get-text-property
                        (car bounds) 'mevedel-view-turn-id))))
    (unless bounds
      (user-error "No collapsible section at point"))
    (let ((inhibit-read-only t)
          (start (car bounds))
          (end (cdr bounds)))
      (save-excursion
        (goto-char start)
        (delete-region start end)
        (mevedel-view--insert-hook-context-block events source collapsed)
        (mevedel-view-disclosure-record-state source 'hook-context
                                                     (not collapsed))
        (when turn-id
          (put-text-property start (point)
                             'mevedel-view-turn-id turn-id))
        (mevedel-view-render-add-display-properties
         start (point) 'hook-context)))))

(defun mevedel-view--inline-skill-info (segments data-buf)
  "Return inline-skill render info from SEGMENTS in DATA-BUF, or nil."
  (with-current-buffer data-buf
    (let (info hook-audits)
      (dolist (seg segments)
        (when (memq (car seg)
                    '(user hook-context prompt render-data ignored))
          (let ((text (buffer-substring (cadr seg) (caddr seg))))
            (unless info
              (dolist (block (mevedel-tool-render-data-blocks text))
                (when-let* (((not info))
                            (data (caddr block))
                            ((eq (plist-get data :kind) 'inline-skill))
                            ((not (plist-member data :mevedel-tool-use-id)))
                            ((mevedel-tool-render-data-trusted-range-p
                              (car block) (cadr block) text)))
                  ;; Anchor the record, not its surrounding segment: gptel
                  ;; may append separators to that segment during dispatch.
                  ;; The codec already decoded the payload and its bounds.
                  (setq info
                        (plist-put
                         data :source
                         (mevedel-view-disclosure-source-range
                          data-buf (+ (cadr seg) (car block))
                          (+ (cadr seg) (cadr block))))))))
            (setq hook-audits
                  (append hook-audits
                          (mevedel-view--hook-audit-records-from-text text))))))
      (when info
        (plist-put info :hook-audits hook-audits)))))

(defun mevedel-view--style-mailbox-body (start end)
  "Style mailbox body text between START and END.
Adds a display-only gutter to payload lines and returns END.
Wrapped text aligns after the gutter.  Leading/trailing blank structural lines
are left bare, while blank lines between payload lines keep the gutter."
  (let ((end-marker (copy-marker end t))
        lines
        first
        last
        index)
    (save-excursion
      (goto-char start)
      (setq index 0)
      (while (< (point) (marker-position end-marker))
        (let* ((line-start (point))
               (line-end (min (line-end-position)
                              (marker-position end-marker)))
               (nonempty
                (string-match-p
                 "\\S-"
                 (buffer-substring-no-properties line-start line-end))))
          (push (cons (copy-marker line-start) nonempty) lines)
          (when nonempty
            (unless first
              (setq first index))
            (setq last index))
          (setq index (1+ index))
          (forward-line 1)))
      (when first
        (setq lines (nreverse lines))
        (cl-loop for line in lines
                 for n from 0
                 when (and (>= n first) (<= n last))
                 do
                 (goto-char (car line))
                 (add-text-properties
                  (point) (min (1+ (line-end-position)) end)
                  `(line-prefix
                    ,(concat (mevedel-view--rendering-indent-blank)
                             (propertize "    │ " 'font-lock-face
                                         'mevedel-view-mailbox-gutter))
                    wrap-prefix
                    ,(concat (mevedel-view--rendering-indent-blank) "      ")
                    rear-nonsticky (line-prefix wrap-prefix)))
                 (when (cdr line)
                   (put-text-property
                    (point) (line-end-position)
                    'font-lock-face 'mevedel-view-mailbox-body)))))
    (dolist (line lines)
      (set-marker (car line) nil))
    (prog1 (marker-position end-marker)
      (set-marker end-marker nil))))

(defun mevedel-view--mail-attribute (open-tag attribute)
  "Return unescaped ATTRIBUTE from mailbox OPEN-TAG, or nil."
  (save-match-data
    (when (string-match (concat "\\s-" (regexp-quote attribute)
                                "=\"\\([^\"]*\\)\"") open-tag)
      (let ((text (match-string 1 open-tag)))
        (dolist (entity '(("&quot;" . "\"") ("&apos;" . "'") ("&lt;" . "<")
                          ("&gt;" . ">") ("&amp;" . "&")))
          (setq text (string-replace (car entity) (cdr entity) text)))
        text))))

(defun mevedel-view--mail-notice (open-tag kind sender body)
  "Return (LABEL . FACE) for a background mail notice, or nil.
OPEN-TAG carries trusted metadata; BODY supplies only the event preview.
KIND and SENDER identify the mailbox record.  Payload text never determines
whether a record is a terminal event."
  (let* ((windows (get-buffer-window-list (current-buffer) nil t))
         (width (max 20 (- (if windows
                              (apply #'min (mapcar #'window-body-width windows))
                            (window-body-width))
                          4)))
         (type (mevedel-view--mail-attribute open-tag "type"))
         (description (mevedel-view--mail-attribute open-tag "description"))
         (termination (mevedel-view--mail-attribute open-tag "termination"))
         (outcome (mevedel-view--mail-attribute open-tag "outcome"))
         (exit (mevedel-view--mail-attribute open-tag "exit_code"))
         (stopped (member termination '("stopped" "interrupted" "cancelled"
                                       "owner-stopped")))
         (timed-out (member termination '("timeout" "timed-out" "expired")))
         (failed (and (not stopped) (not timed-out)
                      (or (member outcome '("failure" "errored" "lost"))
                          (equal termination "lost"))))
         (verb (cond (stopped "stopped") (timed-out "timed out")
                     (failed "failed") (t "completed")))
         (label
          (cond
           ((eq kind 'agent-result)
            (format "Agent %s %s" sender
                    (cond ((equal outcome "completed") "finished")
                          ((equal outcome "errored") "failed")
                          ((equal outcome "interrupted") "interrupted")
                          (t "incomplete"))))
           ((equal type "MONITOR")
            (format "Monitor event: %s · %s"
                    (car (split-string (string-trim body) "\n"))
                    (or description "monitor")))
           ((equal type "MONITOR_TERMINAL")
            (let ((count (mevedel-view--mail-attribute open-tag "event_count")))
              (format "Monitor \"%s\" %s%s" (or description "monitor") verb
                      (if count (format " · %s event%s" count
                                        (if (equal count "1") "" "s")) ""))))
           ((equal type "EXECUTION")
            (let* ((prefix "Background command \"")
                   (suffix (format "\" %s%s" verb
                                   (if exit (format " (exit code %s)" exit) "")))
                   (subject
                    (or (and description (not (string-empty-p description)) description)
                        (mevedel-view--mail-attribute open-tag "command") "command")))
              (concat prefix
                      (mevedel--truncate-display
                       (replace-regexp-in-string "[\n\r\t]+" " " subject)
                       (max 1 (- width (string-width prefix)
                                 (string-width suffix))) "…")
                      suffix))))))
    (when label
      (cons (mevedel--truncate-display
             (replace-regexp-in-string "[\n\r\t]+" " " label)
             width "…")
            (if failed 'error 'mevedel-view-tool-metadata)))))

(defun mevedel-view--own-completion-mail-p (open-tag sender)
  "Return non-nil when OPEN-TAG from SENDER completes this transcript's own work.
Its completion line is the breadcrumb this transcript already recorded when
the work ended, so the delivery adds no second line.  A child agent's
forwarded completion has no local breadcrumb and keeps its notice."
  (and (member (mevedel-view--mail-attribute open-tag "type")
               '("EXECUTION" "MONITOR_TERMINAL"))
       (equal sender (or mevedel-view--agent-path "/root"))))

(defun mevedel-view--decorate-mailbox-block
    (open-regex close-tag start end &optional kind)
  "Replace OPEN-REGEX/CLOSE-TAG regions from START to END with mailbox cards.
KIND identifies the mailbox block flavor.  Shared engine for
`<agent-message>' and `<agent-result>'
rendering.  OPEN-REGEX must capture the canonical sender path in match group
1.  Bodies between the matched open and close tags are preserved verbatim;
background notices summarize structured opener attributes.
Notice bodies start collapsed without a hint.  Ordinary message bodies
exceeding `mevedel-view-mailbox-collapse-line-threshold' start collapsed
with a `[N lines collapsed]' hint.  The `mailbox-delivery' vtype wires
both kinds to the same disclosure controls."
  (save-excursion
    (let ((end-marker (copy-marker end t)))
      (unwind-protect
          (progn
            (goto-char start)
            (while
                (let (close)
                  (while
                      (and
                       (re-search-forward
                        open-regex (marker-position end-marker) t)
                       (not
                        (setq close
                              (save-match-data
                                (save-excursion
                                  (mevedel-transcript--mailbox-find-close
                                   open-regex close-tag
                                   (marker-position end-marker))))))))
                  close)
              (let* ((open-start (match-beginning 0))
                     (open-end (match-end 0))
                     (sender (match-string-no-properties 1))
                     (own (and (not (eq kind 'agent-result))
                               (mevedel-view--own-completion-mail-p
                                (match-string-no-properties 0) sender)))
                     (notice
                      (unless own
                        (mevedel-view--mail-notice
                         (match-string-no-properties 0) kind sender
                         (buffer-substring-no-properties
                          open-end
                          (car (save-excursion
                                 (goto-char open-end)
                                 (mevedel-transcript--mailbox-find-close
                                  open-regex close-tag end-marker)))))))
                     (attribution (mevedel-view--insert-attribution sender))
                     (inhibit-read-only t))
                (if own
                    (let ((close (save-excursion
                                   (goto-char open-end)
                                   (mevedel-transcript--mailbox-find-close
                                    open-regex close-tag
                                    (marker-position end-marker)))))
                      (goto-char (if close (cdr close) open-end))
                      (when (eq (char-after) ?\n) (forward-char 1))
                      (delete-region open-start (point)))
                (delete-region open-start open-end)
                (goto-char open-start)
                (let ((card-start (point))
                      (card-id (gensym "mevedel-view-mailbox-")))
                  (insert "  ")
                  (insert (propertize
                           (if notice "● " "✉ ")
                           'font-lock-face
                           (if notice (cdr notice) 'mevedel-view-mailbox-header)
                           'mevedel-view-mailbox t))
                  (let ((label (propertize
                                (if notice (car notice) "message ")
                                'font-lock-face 'mevedel-view-mailbox-header
                                'mevedel-view-mailbox t)))
                    (insert (if (and notice (eq kind 'agent-result))
                                (mevedel-view--buttonize-agent-header-label label sender)
                              label)))
                  (unless notice (insert attribution))
                  (insert "\n")
                  (mevedel-view--apply-rendering-indent card-start (point))
                  (let ((body-start (point)))
                    (when-let* ((close
                                 (mevedel-transcript--mailbox-find-close
                                  open-regex close-tag
                                  (marker-position end-marker))))
                      (let* ((body-end (car close))
                             (close-end (cdr close))
                             (body-line-count
                              (mevedel-view-disclosure-mailbox-line-count
                               body-start body-end))
                             (long-body
                              (or (and notice t)
                                  (> body-line-count
                                     mevedel-view-mailbox-collapse-line-threshold))))
                        (mevedel-view--debug-log
                         'mailbox-decorate
                         :kind kind
                         :sender sender
                         :open-start open-start
                         :body-start body-start
                         :body-end body-end
                         :body-lines body-line-count
                         :long-body long-body
                         :preview
                         (replace-regexp-in-string
                          "\n" "\\\\n"
                          (buffer-substring-no-properties
                           body-start
                           (min body-end (+ body-start 120)))
                          t t))
                        (let ((styled-end
                               (mevedel-view--style-mailbox-body
                                body-start body-end)))
                          (setq close-end (+ close-end
                                             (- styled-end body-end))
                                body-end styled-end))
                        (when long-body
                          (let* ((hint
                                  (propertize
                                   (if notice ""
                                     (mevedel-view-disclosure-mailbox-hint
                                      body-line-count))
                                   'font-lock-face
                                   'mevedel-view-attribution
                                   'mevedel-view-mailbox-hint t))
                                 (hint-len (length hint)))
                            (goto-char body-start)
                            (when (eq (char-before) ?\n)
                              (backward-char))
                            (insert hint)
                            ;; The header inset predates the hint; a wrapped
                            ;; hint in a nested row must keep it too.
                            (mevedel-view--apply-rendering-indent
                             (- (point) hint-len) (point))
                            (setq body-start (+ body-start hint-len)
                                  body-end (+ body-end hint-len)
                                  close-end (+ close-end hint-len)))
                          (add-text-properties
                           body-start body-end
                           (list 'mevedel-view-mailbox-body t
                                 'mevedel-view-type 'mailbox-delivery
                                 'mevedel-view-collapsed t))
                          (mevedel-view-disclosure-collapse-mailbox-body
                           body-start body-end))
                        (unless long-body
                          (add-text-properties
                           body-start body-end
                           '(mevedel-view-mailbox-body t)))
                        (delete-region body-end close-end)
                        (goto-char body-end)
                        (remove-text-properties
                         card-start (point)
                         '(mevedel-view-source nil
                           mevedel-view-source-key nil
                           mevedel-view-agent-handle-p nil
                           mevedel-view-agent-status nil))
                        (remove-text-properties
                         body-start (point)
                         '(mevedel-view-agent-path nil))
                        (add-text-properties
                         card-start (point)
                         (list 'mevedel-view-type 'mailbox-delivery
                               'mevedel-view-mailbox-card card-id
                               'mevedel-view-mailbox-kind
                               (or kind 'agent-message)
                               'mevedel-view-mailbox-agent-path sender
                               'mevedel-view-mailbox-notice (and notice t)
                               'mevedel-view-collapsed long-body))))))))))
        (set-marker end-marker nil)))))

(defun mevedel-view--decorate-agent-result-blocks (start end)
  "Render agent result blocks from START to END as mailbox cards.
Delegates to `mevedel-view--decorate-mailbox-block' so
`<agent-message>' and `<agent-result>' render uniformly: same
header, same collapse threshold, and the same vtype tag for downstream
TAB toggling."
  (mevedel-view--decorate-mailbox-block
   "<agent-result\\s-+[^>]*sender=\"\\([^\"]+\\)\"[^>]*>"
   "</agent-result>"
   start end
   'agent-result))

(defun mevedel-view--decorate-agent-message-blocks (start end)
  "Decorate canonical `<agent-message sender=PATH>' blocks from START to END.
Delegates to `mevedel-view--decorate-mailbox-block' so the body
collapse threshold, click gating, and vtype tag are uniform with
`<agent-result>' rendering.

Multiple `<agent-message>' blocks in one user turn produce one mailbox
card each, in source order.  Non-matching prose in the same turn
remains as ordinary user text.

Point is restored with an advancing marker: callers sit at the end of the
block and must stay after its rendered card to stamp and append past it."
  (let ((return (copy-marker (point) t)))
    (unwind-protect
        (mevedel-view--decorate-mailbox-block
         "<agent-message\\s-+[^>]*sender=\"\\([^\"]+\\)\"[^>]*>"
         "</agent-message>" start end 'agent-message)
      (goto-char return)
      (set-marker return nil))))

(defun mevedel-view--mailbox-only-text-p (text)
  "Return non-nil if TEXT is only mailbox delivery blocks.

Pure mailbox turns are injected as user-role messages in the data
buffer for gptel, but the view must not render them as `You' turns."
  (and (stringp text)
       (with-temp-buffer
         (insert text)
         (goto-char (point-min))
         (let ((found nil)
               (ok t))
           (while (and ok (not (eobp)))
             (skip-chars-forward " \t\r\n")
             (if (eobp)
                 nil
               (if-let* ((block (mevedel-transcript--mailbox-any-block-at-point
                                  (point-max))))
                   (progn
                     (setq found t)
                     (goto-char (plist-get block :close-end)))
                 (setq ok nil))))
           (and found ok)))))

(defun mevedel-view--render-task-background (segments data-buf turn-source)
  "Render the advisory task-background SEGMENTS of DATA-BUF at TURN-SOURCE.
The block is folded like a tool result rather than shown as parent
dialogue, because the following Agent Task is the authoritative one."
  (let ((text
         (with-current-buffer data-buf
           (buffer-substring-no-properties
            (cadar segments) (caddar segments)))))
    (mevedel-view--insert-rendered-tool
     (list :header "Task background"
           :body (replace-regexp-in-string
                  "\\`<task-background>[ \t]*\n\\|\n?</task-background>[ \t]*\n?\\'"
                  "" text)
           :body-mode 'markdown-mode
           :vtype 'task-background
           :initially-collapsed-p t)
     turn-source)))

(defun mevedel-view--user-turn-attribution (segments data-buf)
  "Return the guest or background-event attribution owning this turn, or nil.
SEGMENTS are the turn's data-buffer spans.  The attribution block sits
in the turn's trailing audit strip, or inside the turn's own span when
segment repair absorbed it, so the lookup covers the turn extent plus
the contiguous run of audit blocks that follows it."
  (when (and segments (buffer-live-p data-buf))
    (with-current-buffer data-buf
      (let* ((start (cadr (car segments)))
             (end (apply #'max (mapcar #'caddr segments)))
             (strip-end
              (save-excursion
                (goto-char end)
                (catch 'done
                  (while t
                    (skip-chars-forward " \t\r\n")
                    (if (looking-at-p (regexp-quote mevedel--hook-audit-open))
                        (unless (search-forward mevedel--hook-audit-close
                                                nil t)
                          (throw 'done nil))
                      (throw 'done nil))))
                (point))))
        (cl-loop for (position . record)
                 in (mevedel-transcript-audit-turn-attributions)
                 when (and (>= position start) (< position strip-end))
                 return record)))))

(defun mevedel-view--user-input-line-count (text)
  "Return the number of lines in user input TEXT."
  (length (split-string (string-trim-right text "\n+") "\n")))

(defun mevedel-view--user-input-fold-p (text)
  "Return non-nil when user input TEXT should render folded.
Org block markers keep the text unfolded: their spans get their own
decorations, which a plain-text fold would hide undecorated."
  (and (> mevedel-view-user-input-collapse-line-threshold 0)
       (not (string-match-p "^#\\+begin_" text))
       (> (mevedel-view--user-input-line-count text)
          mevedel-view-user-input-collapse-line-threshold)))

(defun mevedel-view--user-input-fold-summary (text)
  "Return the one-line folded summary for user input TEXT."
  (let* ((hidden (1- (mevedel-view--user-input-line-count text)))
         (first-line (car (split-string text "\n"))))
    (concat
     (mevedel-view-disclosure-truncate-line first-line 80)
     " "
     (propertize (format "(+%d line%s)" hidden (if (= hidden 1) "" "s"))
                 'font-lock-face 'mevedel-view-tool-metadata))))

(defun mevedel-view--insert-user-input-fold (text &optional source)
  "Insert user input TEXT folded to an expandable one-line summary.
SOURCE, when available, is the data-buffer range backing the turn so the
fold state survives re-renders; the send-path echo has none and loses
its fold state to the next full rerender, which re-folds from source."
  (let ((start (point)))
    (insert (mevedel-view--user-input-fold-summary text))
    (unless (eq (char-before) ?\n)
      (insert "\n"))
    (add-text-properties
     start (point)
     `(mevedel-view-type user-input-summary
       mevedel-view-collapsed t
       mevedel-view-user-input-text ,text))
    (when source
      (add-text-properties
       start (point)
       `(mevedel-view-source ,source
         mevedel-view-source-key
         ,(mevedel-view-disclosure-state-key source 'user-input-summary))))
    (mevedel-view-render-add-display-properties
     start (point) 'user-input-summary)))

(defun mevedel-view-render-toggle-user-input ()
  "Toggle the folded user input section at point.
The full input travels in a text property rather than being re-read
from the data buffer, so the send-path echo -- which has no source
coordinates yet -- folds and expands the same way as a rendered turn."
  (let* ((bounds (and (eq (get-text-property (point) 'mevedel-view-type)
                        'user-input-summary)
                       (mevedel-view-disclosure-section-bounds
                        'mevedel-view-type)))
         (start (car-safe bounds)))
    (unless bounds
      (user-error "No collapsible section at point"))
    (let* ((inhibit-read-only t)
           (end (cdr bounds))
           (collapsed (get-text-property start 'mevedel-view-collapsed))
           (source (get-text-property start 'mevedel-view-source))
           (source-key (get-text-property start 'mevedel-view-source-key))
           (turn-id (get-text-property start 'mevedel-view-turn-id))
           (text (or (get-text-property start 'mevedel-view-user-input-text)
                     (buffer-substring-no-properties start end))))
      (save-excursion
        (goto-char start)
        (delete-region start end)
        (let ((ins-start (point)))
          (if collapsed
              (progn
                (insert (mevedel-view--fontify-as text 'markdown-mode))
                (unless (eq (char-before) ?\n)
                  (insert "\n")))
            (insert (mevedel-view--user-input-fold-summary text))
            (insert "\n"))
          (add-text-properties
           ins-start (point)
           `(mevedel-view-type user-input-summary
             mevedel-view-collapsed ,(not collapsed)
             mevedel-view-user-input-text ,text))
          (when source
            (let ((key (mevedel-view-disclosure-state-key
                        source 'user-input-summary source-key)))
              (add-text-properties
               ins-start (point)
               `(mevedel-view-source ,source
                 mevedel-view-source-key ,key))
              (mevedel-view-disclosure-record-state-for-key
               key (not collapsed))))
          (when turn-id
            (put-text-property
             ins-start (point) 'mevedel-view-turn-id turn-id))
          (mevedel-view-render-add-display-properties
           ins-start (point) 'user-input-summary))))))

(defun mevedel-view--insert-shared-context (display &optional source)
  "Insert the attributed question and folded context from DISPLAY.
SOURCE bounds the original user input.  Anchor the disclosure to the exact
context suffix so live echoes and later full renders share its fold state.
Return the start of the inserted disclosure."
  (insert (mevedel-view--fontify-as (plist-get display :text) 'markdown-mode)
          "\n")
  (let ((body (plist-get display :context))
        (start (point)))
    (when (and source (markerp (car source)))
      (setq source
            (with-current-buffer (marker-buffer (car source))
              (save-excursion
                (goto-char (car source))
                (when (search-forward body (cdr source) t)
                  (mevedel-view-disclosure-source-range
                   (current-buffer) (- (point) (length body)) (point)))))))
    (mevedel-view--insert-user-input-fold
     (concat (plist-get display :label) "\n" body) source)
    start))

(defun mevedel-view--prompt-preview (text shared-display)
  "Return a one-line preview without mailbox blocks from visible TEXT.
Use SHARED-DISPLAY text when it replaces the original prompt."
  (let ((visible (or (plist-get shared-display :text) text)))
    (when (string-match-p "<agent-\\(?:result\\|message\\)\\s-+" visible)
      (setq visible
            (with-temp-buffer
              (insert visible)
              (goto-char (point-min))
              (while (re-search-forward
                      "<agent-\\(?:result\\|message\\)\\s-+" nil t)
                (goto-char (match-beginning 0))
                (if-let* ((block (mevedel-transcript--mailbox-any-block-at-point
                                  (point-max))))
                    (delete-region (point) (plist-get block :close-end))
                  (forward-char 1)))
              (buffer-substring-no-properties (point-min) (point-max)))))
    (unless (string-blank-p visible)
      (let ((preview (replace-regexp-in-string
                      "[[:space:]\n]+" " " (string-trim visible))))
        ;; Header-line evaluation measures this string on every redisplay.
        ;; Retain the full prompt only in its source-backed transcript row.
        (if (> (length preview) 512)
            (concat (substring preview 0 511) "…")
          preview)))))

(defun mevedel-view--insert-skill-disclosures (info)
  "Insert source-backed attachment and Prompt disclosures from skill INFO."
  (let ((attachments (plist-get info :attachments))
        (body (string-trim (or (plist-get info :prompt) "")))
        (source (plist-get info :source)))
    (when attachments
      ;; Section bounds use source identity, not equality.  Keep the two
      ;; rows separate even though they originate in the same source record.
      (mevedel-view--insert-rendered-tool
       (list :header (mevedel-view--attached-skills-summary
                      (mapcar (lambda (attachment) (plist-get attachment :name))
                              attachments)
                      (length attachments))
             :body (mapconcat (lambda (attachment) (plist-get attachment :body))
                              attachments "\n\n")
             :body-mode 'markdown-mode
             :vtype 'system-reminder-summary
             :initially-collapsed-p t)
       (cons (car source) (cdr source))))
    (unless (string-empty-p body)
      (mevedel-view--insert-rendered-tool
       (list :header "Prompt" :body body :body-mode 'markdown-mode
             :vtype 'prompt-summary :initially-collapsed-p t)
       source))))

(defun mevedel-view--render-user-turn (segments data-buf &optional directive)
  "Render user SEGMENTS from DATA-BUF, with optional DIRECTIVE metadata."
  (let* ((insert-start (point))
         (raw-text (mevedel-view--user-turn-text segments data-buf))
         (prompt-drawers (mevedel-view--user-turn-prompt-drawers
                          segments data-buf))
         (hook-contexts (mevedel-view--user-turn-hook-contexts
                         segments data-buf))
         (inline-skill (mevedel-view--inline-skill-info segments data-buf))
         (hook-audits (mevedel-view--merge-hook-audits
                       (mevedel-view--user-turn-hook-audits
                        segments data-buf)
                       (plist-get inline-skill :hook-audits)))
         (inline-source-seg (cl-find 'user segments :key #'car))
         (text (if prompt-drawers
                   (mevedel-view--fontify-directive-display-text
                    (mevedel-view--directive-turn-display-text raw-text))
                 (or (mevedel-view--user-turn-display-text segments data-buf)
                     (plist-get inline-skill :display-text)
                     raw-text)))
         (attribution (unless directive
                        (mevedel-view--user-turn-attribution segments data-buf)))
         (shared-display (mevedel-transcript-audit-shared-context
                          text (plist-get attribution :shared)))
         (text-start nil))
    (when (eq (plist-get attribution :type) 'background-event)
      (setq text ""))
    (cond
     ((and (string-empty-p text)
           (null prompt-drawers)
           (null hook-contexts)
           (null hook-audits))
      nil)
     ((mevedel-view--mailbox-only-text-p text)
      (setq text-start (point))
      (insert text)
      (mevedel-view--decorate-agent-result-blocks text-start (point))
      (mevedel-view--decorate-agent-message-blocks text-start (point)))
     (t
      (let* ((header-start (point))
             (guest (plist-get attribution :name))
             (background
              (eq (plist-get attribution :type) 'background-event)))
        (insert (propertize
                 (cond
                  (directive
                   (format "◆ %s · %s · T%s · excluded from model context\n"
                           (mevedel--truncate-display
                            (or (plist-get directive :directive-id) "?")
                            8 "…")
                           (mevedel-overlay-ui-directive-action-label
                            (plist-get directive :action))
                           (or (plist-get directive :turn) "?")))
                  (background "")
                  (guest (format "%s (guest)\n" guest))
                  (t "You\n"))
                 'font-lock-face
                 (if (or guest background)
                     'mevedel-view-guest-header
                   'mevedel-view-user-header)
                 'mevedel-view-type 'turn-header
                 'mevedel-view-prompt-preview
                 (mevedel-view--prompt-preview text shared-display)
                 'mevedel-view-turn-role
                 (if directive 'directive 'user)
                 'mevedel-view-collapsed nil))
        (when directive
          (add-text-properties
           header-start (1- (point))
           (list 'mevedel-view-zone-activate
                 (lambda () (mevedel-view-directive-actions directive))
                 'mouse-face 'highlight
                 'help-echo "RET: directive actions"))))
      (setq text-start (point))
      (unless (string-empty-p text)
        (cond
         (shared-display
          (mevedel-view--insert-shared-context
           shared-display
           (mevedel-view-disclosure-source-range
            data-buf (cadr (car segments)) (apply #'max (mapcar #'caddr segments)))))
         ((and (null prompt-drawers)
               (not directive)
               (mevedel-view--user-input-fold-p text))
          (mevedel-view--insert-user-input-fold
           text
           (when-let* ((user-segs
                       (cl-remove-if-not
                        (lambda (seg) (eq (car seg) 'user))
                        segments)))
             (mevedel-view-disclosure-source-range
              data-buf
              (cadr (car user-segs))
              (caddr (car (last user-segs)))))))
         (t
          (insert (if (or prompt-drawers directive)
                      text
                    (mevedel-view--fontify-as text 'markdown-mode)))
          (unless (eq (char-before) ?\n)
            (insert "\n")))))
      ;; Decorate mailbox blocks that appear inside mixed user text.
      (mevedel-view--decorate-agent-result-blocks text-start (point))
      (mevedel-view--decorate-agent-message-blocks text-start (point))
      (dolist (ctx hook-contexts)
        (mevedel-view--insert-hook-context-block
         (plist-get ctx :events)
         (mevedel-view-disclosure-source-range
          data-buf (plist-get ctx :start) (plist-get ctx :end))))
      (dolist (audit hook-audits)
        (mevedel-view--insert-hook-audit-block
         audit
         (plist-get audit :source)))
      (dolist (drawer prompt-drawers)
        (mevedel-view--insert-rendered-tool
         (list :header "Prompt"
               :body (plist-get drawer :body)
               :body-mode 'markdown-mode
               :vtype 'prompt-summary
               :initially-collapsed-p t)
         (mevedel-view-disclosure-source-range
          data-buf (plist-get drawer :start) (plist-get drawer :end))))
      (when (and inline-skill inline-source-seg)
        (mevedel-view--insert-skill-disclosures inline-skill))))
    ;; A background wake and its attribution can both render nothing.
    ;; Keep visible reminders, but do not give a hidden turn spacer rows.
    (when (> (point) insert-start)
      (insert "\n"))))

(defun mevedel-view--directive-turn-display-text (text)
  "Return the compact display text for a directive turn TEXT.

Directive turns are stored in the data buffer as regular gptel user
turns plus an ignored `:PROMPT:' drawer.  In org buffers the action is
stored as a trailing tag (\"Text :implement:\"); in markdown buffers it
is stored as a leading code-formatted action (\"`implement` Text\")."
  (let ((trimmed (string-trim text)))
    (cond
     ((string-match "\\`\\(.*?\\)[ \t]+:\\([[:alnum:]_-]+\\):\\'" trimmed)
      (let ((body (string-trim (match-string 1 trimmed)))
            (action (match-string 2 trimmed)))
        (if (string-empty-p body)
            (mevedel-overlay-ui-directive-action-label action)
          (format "%s: %s"
                  (mevedel-overlay-ui-directive-action-label action)
                  body))))
     ((string-match "\\``\\([^`]+\\)`[ \t\n]+\\(.+\\)\\'" trimmed)
      (format "%s: %s"
              (mevedel-overlay-ui-directive-action-label
               (match-string 1 trimmed))
              (match-string 2 trimmed)))
     (t trimmed))))

(defun mevedel-view--directive-metadata-context (directive)
  "Return `(RECORD WORKSPACE ATTEMPT ATTEMPT-INDEX)' for DIRECTIVE metadata."
  (when-let* ((data-buffer (mevedel-view-segments-display-buffer))
              ((buffer-live-p data-buffer))
              (session
               (or (buffer-local-value 'mevedel--session data-buffer)
                   (and (buffer-live-p mevedel--data-buffer)
                        (buffer-local-value
                         'mevedel--session mevedel--data-buffer))))
              (workspace (mevedel-session-workspace session))
              (record
               (cl-find (plist-get directive :directive-id)
                        (mevedel-workspace-directives workspace)
                        :key #'mevedel-directive-id :test #'equal)))
    (let* ((attempt
            (and (eq (plist-get directive :activity-kind) 'attempt)
                 (cl-find (plist-get directive :sequence)
                          (mevedel-directive-attempts record)
                          :key #'mevedel-directive-attempt-sequence)))
           (discussion
            (and (eq (plist-get directive :activity-kind) 'discussion)
                 (cl-find (plist-get directive :sequence)
                          (mevedel-directive-discussion record)
                          :key #'mevedel-directive-discussion-turn-sequence)))
           (attempt-index
            (if attempt
                (1+ (cl-position
                     attempt (mevedel-directive-attempts record)
                     :test #'eq))
              (and discussion
                   (mevedel-directive-discussion-turn-attempt-index
                    discussion)))))
      (list record workspace attempt attempt-index))))

(defun mevedel-view--directive-checkpoint-buffer (checkpoint)
  "Return the live execution buffer matching CHECKPOINT, or nil."
  (when (buffer-live-p mevedel--data-buffer)
    (when-let* ((session
                 (buffer-local-value 'mevedel--session mevedel--data-buffer))
                ((equal (plist-get checkpoint :session-id)
                        (mevedel-session-session-id session))))
      mevedel--data-buffer)))

(defun mevedel-view-directive-actions (directive)
  "Choose a state-correct action for the rendered DIRECTIVE turn."
  (interactive (list (get-text-property (point) 'mevedel-view-directive)))
  (pcase-let* ((`(,record ,workspace ,attempt ,attempt-index)
                (or (mevedel-view--directive-metadata-context directive)
                    (user-error "Directive record is unavailable")))
               (actions (mevedel-directive-actions record))
               (discussion-p
                (eq (plist-get directive :activity-kind) 'discussion))
               (choices
                (append
                 (when attempt '((?d "discuss result")))
                 (when discussion-p '((?d "continue discussion")))
                 (when (memq 'implement-this actions)
                   '((?i "implement this")))
                 (when (memq 'request-changes actions)
                   '((?c "request changes")))
                 (when (memq 'retry actions)
                   '((?r "retry")))
                 (when (and attempt
                            (not (string-empty-p
                                  (or (mevedel-directive-attempt-patch attempt)
                                      ""))))
                   '((?p "view patch")))
                 (when (and attempt
                            (mevedel-directive-attempt-checkpoint attempt))
                   '((?w "rewind before this implementation")))
                 '((?o "inspect"))))
               (choice (car (read-multiple-choice "Directive action: " choices))))
    (pcase choice
      (?d (mevedel-view-enter-directive-scope
           record 'discuss attempt-index workspace))
      (?i (mevedel--implement-discussion
           (plist-get (mevedel--directive-action-context record workspace)
                      :directive)))
      (?c (mevedel-view-enter-directive-scope
           record 'request-changes nil workspace))
      (?r (mevedel-view-enter-directive-scope record 'retry nil workspace))
      (?p (mevedel--replace-patch-buffer
           (mevedel-directive-attempt-patch attempt)))
      (?w (let ((checkpoint (mevedel-directive-attempt-checkpoint attempt)))
            (mevedel-session-rewind-rewind-checkpoint
             workspace checkpoint
             (mevedel-view--directive-checkpoint-buffer checkpoint))))
      (?o (mevedel-open-directive-activity record workspace)))))

(defun mevedel-view--fontify-directive-display-text (text)
  "Return TEXT with the directive action label fontified."
  (let ((text (copy-sequence text)))
    (if (string-match "\\`\\([^:\n]+:\\|[^:\n]+\\)\\(?:[ \t\n]\\|\\'\\)" text)
        (progn
          (put-text-property (match-beginning 1) (match-end 1)
                             'font-lock-face
                             'mevedel-view-directive-action
                             text)
          text)
      text)))

(defun mevedel-view--user-turn-prompt-drawers (segments data-buf)
  "Return prompt drawer plists from user SEGMENTS in DATA-BUF.
Each plist contains :start, :end, and :body for a `:PROMPT:' drawer."
  (with-current-buffer data-buf
    (let (drawers)
      (dolist (seg segments)
        (when (memq (car seg) '(user prompt ignored))
          (let ((seg-end (caddr seg)))
            (save-excursion
              (goto-char (cadr seg))
              (while (re-search-forward "^:PROMPT:\n" seg-end t)
                (let ((drawer-start (match-beginning 0))
                      (body-start (match-end 0)))
                  (when (re-search-forward "^:END:[ \t]*\n?" seg-end t)
                    (let ((body-end (match-beginning 0))
                          (drawer-end (match-end 0)))
                      (push (list :start drawer-start
                                  :end drawer-end
                                  :body (buffer-substring-no-properties
                                         body-start body-end))
                            drawers)))))))))
      (nreverse drawers))))

(defun mevedel-view--flush-thinking-group (thinking-group data-buf)
  "Render accumulated THINKING-GROUP segments from DATA-BUF.
Merges adjacent thinking/reasoning segments into a single summary."
  (when thinking-group
    (let* ((segs (nreverse thinking-group))
           (first-start (cadr (car segs)))
           (last-end (caddr (car (last segs))))
           (bounds (mevedel-view--reasoning-source-bounds
                    data-buf first-start last-end))
           (first-start (or (car-safe bounds) first-start))
           (last-end (or (cdr-safe bounds) last-end))
           (source (mevedel-view-disclosure-source-range
                    data-buf first-start last-end))
           (summary (mevedel-view--thinking-summary
                     data-buf first-start last-end)))
      (unless (string-empty-p summary)
        (let ((unit-start (point)))
          (mevedel-view--ensure-blank-line-after-response)
          (mevedel-view--insert-summary-region
           (mevedel-view--summary-with-face
            summary 'mevedel-view-thinking-summary)
           `(mevedel-view-type thinking-summary
             mevedel-view-collapsed t
             mevedel-view-source ,source
             mevedel-view-source-key ,(mevedel-view-disclosure-state-key
                                       source
                                       'thinking-summary)))
          (mevedel-view--mark-live-render-unit unit-start first-start))))))

(defun mevedel-view--render-system-reminder-segment (seg data-buf)
  "Render system-reminder SEG from DATA-BUF as a control row."
  (let* ((seg-start (cadr seg))
         (seg-end (caddr seg))
         (source (mevedel-view-disclosure-source-range data-buf seg-start seg-end))
         (body
          (with-current-buffer data-buf
            (mevedel-view--system-reminder-body-from-text
             (buffer-substring-no-properties seg-start seg-end))))
         (summary (mevedel-view--system-reminder-summary
                   data-buf seg-start seg-end)))
    (mevedel-view--ensure-blank-line-after-response)
    (mevedel-view--insert-rendered-tool
     (list :header summary
           :body body
           :body-mode 'markdown-mode
           :vtype 'system-reminder-summary
           :initially-collapsed-p t)
     source)))

(defun mevedel-view--request-summary-line (render-data)
  "Return the visible request summary line for RENDER-DATA."
  (let ((elapsed (plist-get render-data :elapsed-seconds)))
    (when (numberp elapsed)
      (format "── Worked for %s " (mevedel--duration-label elapsed)))))

(defun mevedel-view--render-request-summary-segment (seg data-buf)
  "Render request-summary SEG from DATA-BUF as the assistant turn's closing rule.
The label sits on the rule: a strike-through on the extended newline
continues it to the window edge, so no width has to be computed for
narrow or split windows.  `mevedel-view--render-turn' then omits its own
rule."
  (let* ((seg-start (cadr seg))
         (seg-end (caddr seg))
         (render-data
          (with-current-buffer data-buf
            (mevedel-view--request-summary-render-data-from-text
             (buffer-substring seg-start seg-end))))
         (line (and render-data
                    (mevedel-view--request-summary-line render-data)))
         (failure-rendering
          (mevedel-view--segment-rendering data-buf seg-start seg-end))
         (source (mevedel-view-disclosure-source-range data-buf seg-start seg-end)))
    (when failure-rendering
      (mevedel-view--ensure-blank-line-after-response)
      (mevedel-view--insert-rendered-tool failure-rendering source))
    (when line
      (mevedel-view--ensure-blank-line-before-response)
      (let ((start (point)))
        (insert line "\n")
        (put-text-property start (1- (point))
                           'font-lock-face 'mevedel-view-separator)
        (put-text-property (1- (point)) (point)
                           'font-lock-face 'mevedel-view-request-summary-rule)
        (add-text-properties
         start (point)
         `(mevedel-view-type request-summary
           mevedel-view-source ,source
           mevedel-view-collapsed nil))))))

(defun mevedel-view--ensure-blank-line-before-response ()
  "Insert a blank line before a response segment when missing.
Visually separates the response text from preceding thinking summaries,
tool summaries, or the \"Assistant\" turn header.  A blank line is only
added when the text before point does not already end with a blank line
-- so consecutive response segments don't accumulate extra spacing."
  (unless (or (bobp)
              (and (eq (char-before) ?\n)
                   (> (1- (point)) (point-min))
                   (eq (char-before (1- (point))) ?\n)))
    (insert "\n")))

(defun mevedel-view--previous-rendered-type ()
  "Return the `mevedel-view-type' of the preceding rendered character."
  (let ((pos (point)))
    (while (and (> pos (point-min))
                (memq (char-before pos) '(?\n ?\s ?\t)))
      (setq pos (1- pos)))
    (when (> pos (point-min))
      (get-text-property (1- pos) 'mevedel-view-type))))

(defun mevedel-view--ensure-blank-line-after-response ()
  "Insert a blank line before activity following response prose.
Activity hangs below the prose that announced it; the rule that sets it
apart comes before the next prose instead, see
`mevedel-view--insert-activity-rule-before-response'."
  (when (eq (mevedel-view--previous-rendered-type) 'response)
    (mevedel-view--ensure-blank-line-before-response)))

(defun mevedel-view--activity-before-point-p ()
  "Return non-nil when this turn's preceding rendered row is activity.
Activity is anything other than prose or a heading: tool rows, thinking,
reminders, mailbox cards.  Turn-closing chrome (the user spacer, the turn
rule) ends the search, so a turn never takes its rule from the turn
before it, as a directive response without its own heading otherwise
would."
  (let ((pos (point)))
    (while (and (> pos (point-min))
                (memq (char-before pos) '(?\n ?\s ?\t))
                (not (memq (get-text-property (1- pos) 'font-lock-face)
                           '(mevedel-view-separator
                             mevedel-view-request-summary-rule
                             mevedel-view-turn-rule))))
      (setq pos (1- pos)))
    (and (> pos (point-min))
         (not (memq (char-before pos) '(?\n ?\s ?\t)))
         (when-let* ((type (get-text-property (1- pos) 'mevedel-view-type)))
           (not (memq type '(response turn-header activity-separator)))))))

(defconst mevedel-view--activity-rule
  (concat (propertize "  " 'mevedel-view-type 'activity-separator
                      'mevedel-view-collapsed nil)
          (propertize "\n"
                      'font-lock-face 'mevedel-view-activity-rule
                      'mevedel-view-type 'activity-separator
                      'mevedel-view-collapsed nil))
  "Text of the quiet rule before prose that follows activity.
The two leading columns stay undecorated, which insets the rule.")

(defun mevedel-view--insert-activity-rule-before-response ()
  "Insert a quiet inset rule before prose that follows activity.
The rule closes the activity the previous prose announced, so each
section reads as intent followed by its actions.  It is decided when
the prose arrives: the activity before it is already rendered, so no
lookahead or later repair is needed."
  (when (mevedel-view--activity-before-point-p)
    (mevedel-view--ensure-blank-line-before-response)
    (insert mevedel-view--activity-rule)))

(defun mevedel-view--tool-activity-tool-segment-p (seg data-buf)
  "Return non-nil when SEG belongs to a tool run in DATA-BUF."
  (or (eq (car seg) 'tool)
      (and (eq (car seg) 'ignored)
           (mevedel-view--hook-audit-only-segment-p
            data-buf (cadr seg) (caddr seg)))))

(defvar mevedel-view--activity-entry-cache nil
  "Entries shared by classification and insertion of one activity run.")

(defun mevedel-view--tool-segment-entry (seg data-buf)
  "Return SEG's normalized tool or standalone audit entry in DATA-BUF.
Reuse classification within one activity run while source and session state
stay unchanged.  Each caller owns the returned entry's coalescing count."
  (let* ((state (and mevedel-view--activity-entry-cache
                     (boundp 'mevedel--session)
                     (mevedel-view--session-render-state-fingerprint
                      mevedel--session)))
         (key (and mevedel-view--activity-entry-cache
                   (with-current-buffer data-buf
                     (list data-buf seg (buffer-modified-tick)
                           (point-min) (point-max) state))))
         (cached (and key (gethash key mevedel-view--activity-entry-cache))))
    (if cached
        (copy-sequence (cdr cached))
      (let ((entry
	     (let* ((seg-start (cadr seg))
		    (seg-end (caddr seg))
		    (source (mevedel-view-disclosure-source-range
			     data-buf seg-start seg-end))
		    (audit-p (eq (car seg) 'ignored))
		    (audits (when audit-p
			      (with-current-buffer data-buf
				(mevedel-view--hook-audit-records-from-text
				 (buffer-substring seg-start seg-end)
				 nil data-buf seg-start))))
		    (rendering (unless audit-p
				 (mevedel-view--segment-rendering
				  data-buf seg-start seg-end t)))
		    (vtype (or (plist-get rendering :vtype) 'tool-summary))
		    (state (and rendering
				(mevedel-view-disclosure-state-entry source vtype))))
	       (when (and state (not (cdr state)))
		 (setq rendering
		       (or (mevedel-view--segment-rendering
			    data-buf seg-start seg-end)
			   rendering)))
	       (let ((group-child (plist-get rendering :group-child)))
		 (when group-child
		   (setq rendering
			 (plist-put (copy-sequence rendering) :group-child nil)))
		 (unless (or (plist-get rendering :hidden-p)
			     (and audit-p (null audits)))
		   (list :kind (if audit-p 'hook-audit 'tool)
			 :start seg-start
			 :end seg-end
			 :source source
			 :rendering rendering
			 :hook-audits audits
			 :group-child group-child
			 :count 1))))))
        (when key
          (puthash key (cons t (copy-sequence entry))
                   mevedel-view--activity-entry-cache))
        entry))))


(defun mevedel-view--thinking-group-entry (segments data-buf)
  "Return one grouped reasoning entry for SEGMENTS in DATA-BUF, or nil."
  (when segments
    (let* ((first-start (cadr (car segments)))
           (last-end (caddr (car (last segments))))
           (bounds (mevedel-view--reasoning-source-bounds
                    data-buf first-start last-end))
           (start (or (car-safe bounds) first-start))
           (end (or (cdr-safe bounds) last-end))
           (summary (mevedel-view--thinking-summary data-buf start end))
           (body
            (with-current-buffer data-buf
              (mevedel-view--clean-reasoning-text
               (buffer-substring start end)))))
      (unless (or (string-empty-p summary)
                  (string-empty-p (string-trim body)))
        (list :kind 'reasoning
              :start start
              :end end
              :group-child
              (list :kind 'reasoning
                    :status 'success
                    :rendering
                    (list :header summary
                          :body body
                          :body-mode 'markdown-mode)))))))

(defun mevedel-view--mailbox-activity-entry (seg data-buf)
  "Return the activity entry for mailbox SEG in DATA-BUF.
Keep the canonical card text so grouped and standalone deliveries use
one renderer, including execution summaries and sender links.  Return nil
for this transcript's own completion delivery, which renders nothing.
Classification and insertion reuse this entry within one activity run."
  (let* ((key (and mevedel-view--activity-entry-cache
                   (let ((agent mevedel-view--agent-path))
                     (with-current-buffer data-buf
                       (list 'mailbox data-buf seg (buffer-modified-tick)
                             (point-min) (point-max) agent)))))
         (cached (and key (gethash key mevedel-view--activity-entry-cache))))
    (if cached
        (copy-sequence (cdr cached))
      (let* ((text (mevedel-view--user-turn-text (list seg) data-buf))
             (opener (and (string-match
                           "\\`[ \t\n]*<\\(agent-result\\|agent-message\\)\\s-[^>]*>" text)
                          (match-string 0 text)))
             (result-p (and opener (equal (match-string 1 text) "agent-result")))
             (notice
              (and opener
                   (or result-p
                       (member (mevedel-view--mail-attribute opener "type")
                               '("EXECUTION" "MONITOR" "MONITOR_TERMINAL")))))
             (entry
              (unless (and notice (not result-p)
                           (mevedel-view--own-completion-mail-p
                            opener (mevedel-view--mail-attribute opener "sender")))
                (list :kind 'mailbox :start (cadr seg) :end (caddr seg)
                      :notice-p notice
                      :group-child
                      (list :kind 'mailbox :status 'success
                            :rendering (list :mailbox-text text))))))
        (when key
          (puthash key (cons t (copy-sequence entry)) mevedel-view--activity-entry-cache))
        entry))))

(defun mevedel-view--completion-record-p (record)
  "Return non-nil when hook audit RECORD is an execution breadcrumb."
  (eq (plist-get record :type) 'execution-breadcrumb))

(defun mevedel-view--split-completion-entries (entries)
  "Return activity ENTRIES with execution breadcrumbs as entries of their own.
A breadcrumb lands in the hook audits of whichever tool or audit segment
precedes it, which would make that row demand individual presentation.  As a
`completion' entry it instead folds into the activity group and is counted
there, and still renders as its own line when no group absorbs it."
  (mapcan
   (lambda (entry)
     (let* ((audit-p (eq (plist-get entry :kind) 'hook-audit))
            (rendering (plist-get entry :rendering))
            (audits (if audit-p
                        (plist-get entry :hook-audits)
                      (plist-get rendering :hook-audits)))
            (crumbs (cl-remove-if-not #'mevedel-view--completion-record-p
                                      audits)))
       (if (null crumbs)
           (list entry)
         (let ((others (cl-remove-if #'mevedel-view--completion-record-p
                                     audits)))
           (append
            (cond
             ((not audit-p)
              (list (plist-put (copy-sequence entry) :rendering
                               (plist-put (copy-sequence rendering)
                                          :hook-audits others))))
             (others
              (list (plist-put (copy-sequence entry) :hook-audits others))))
            (mapcar
             (lambda (record)
               (let ((source (or (plist-get record :source)
                                 (plist-get entry :source))))
                 (list :kind 'completion
                       :start (marker-position (car source))
                       :end (marker-position (cdr source))
                       :source source
                       :count 1
                       :group-child
                       (list :kind 'completion :status 'success
                             :rendering (list :completion-record record)))))
             crumbs))))))
   entries))

(defun mevedel-view--tool-activity-entries (segments data-buf)
  "Return ordered tool, reasoning, and mailbox entries for SEGMENTS in DATA-BUF."
  (let (out thinking-group)
    (cl-labels
        ((flush-thinking
          ()
          (when thinking-group
            (when-let* ((entry (mevedel-view--thinking-group-entry
                                (nreverse thinking-group) data-buf)))
              (push entry out))
            (setq thinking-group nil))))
      (dolist (seg (mevedel-view--merge-tool-hook-audit-segments
                    segments data-buf))
        (pcase (car seg)
          ('tool
           (flush-thinking)
           (when-let* ((entry (mevedel-view--tool-segment-entry seg data-buf)))
             (push entry out)))
          ('mailbox
           (when-let* ((entry (mevedel-view--mailbox-activity-entry seg data-buf)))
             (flush-thinking)
             (push entry out)))
          ('ignored
           (if (mevedel-view--hook-audit-only-segment-p
                data-buf (cadr seg) (caddr seg))
               (progn
                 (flush-thinking)
                 (when-let* ((entry (mevedel-view--tool-segment-entry seg data-buf)))
                   (push entry out)))
             (push seg thinking-group)))
          (_ (push seg thinking-group))))
      (flush-thinking))
    (mevedel-view--split-completion-entries (nreverse out))))

(defun mevedel-view--tool-activity-groupable-p (entries &optional pending)
  "Return non-nil when activity ENTRIES may fold across thinking.
PENDING in-flight calls count toward the threshold, as they will once
settled."
  (let ((tool-entries
         (cl-remove-if-not
          (lambda (entry) (eq (plist-get entry :kind) 'tool))
          entries)))
    (and (not (seq-some (lambda (entry)
                         (or (eq (plist-get entry :kind) 'hook-audit)
                             (plist-get entry :notice-p)))
                       entries))
         (> mevedel-view-tool-group-collapse-threshold 0)
         (> (+ (length tool-entries) (length pending))
            mevedel-view-tool-group-collapse-threshold)
         (cl-every
          (lambda (entry)
            (and (null (plist-get (plist-get entry :rendering)
                                  :coalesce-key))
                 (mevedel-view--tool-group-entry-p entry)))
          tool-entries))))

(defun mevedel-view--render-tool-activity (segments data-buf &optional pending)
  "Render activity SEGMENTS from DATA-BUF in source order.
Special tool rows split otherwise groupable tool/reasoning runs.  Keep
this activity mutable as one live unit until a response or other turn
boundary arrives, so later tool calls can join the same group.
PENDING in-flight tool entries join the trailing tool run, or follow the
activity when it ends in something they cannot join."
  (let ((unit-start (point))
        (mevedel-view--activity-entry-cache
         (or mevedel-view--activity-entry-cache (make-hash-table :test #'equal))))
    (cl-labels
        ((render-run (segments &optional pending)
           (if (cl-every
                (lambda (seg)
                  (mevedel-view--tool-activity-tool-segment-p seg data-buf))
                segments)
               (mevedel-view--render-tool-group segments data-buf pending)
             (let ((entries (mevedel-view--tool-activity-entries segments data-buf)))
               (if (mevedel-view--tool-activity-groupable-p entries pending)
                   (progn
                     (mevedel-view--ensure-blank-line-after-response)
                     (mevedel-view--insert-tool-group entries data-buf pending))
                 (let (tool-group thinking-group)
                   (dolist (seg segments)
                     (if (mevedel-view--tool-activity-tool-segment-p seg data-buf)
                         (progn
                           (when thinking-group
                             (mevedel-view--flush-thinking-group
                              thinking-group data-buf)
                             (setq thinking-group nil))
                           (push seg tool-group))
                       (when tool-group
                         (mevedel-view--render-tool-group
                          (nreverse tool-group) data-buf)
                         (setq tool-group nil))
                       (if (eq (car seg) 'mailbox)
                           (progn
                             (mevedel-view--flush-thinking-group
                              thinking-group data-buf)
                             (setq thinking-group nil)
                             (when-let* ((entry (mevedel-view--mailbox-activity-entry
                                                 seg data-buf))
                                         (child (mevedel-view--tool-group-child
                                                 entry data-buf 0)))
                               (mevedel-view--insert-child-call-block
                                child (plist-get child :source) 'derive "")))
                         (push seg thinking-group))))
                   (when tool-group
                     (mevedel-view--render-tool-group
                      (nreverse tool-group) data-buf pending)
                     (setq pending nil))
                   (mevedel-view--flush-thinking-group
                    thinking-group data-buf)
                   (when pending
                     (mevedel-view--insert-pending-run pending))))))))
      (if (cl-every
           (lambda (seg)
             (mevedel-view--tool-activity-tool-segment-p seg data-buf))
           segments)
          (render-run segments pending)
        (let (run special-run)
          (dolist (seg (mevedel-view--merge-tool-hook-audit-segments
                        segments data-buf))
            (let* ((entry (pcase (car seg)
                            ('tool (mevedel-view--tool-segment-entry seg data-buf))
                            ('mailbox (mevedel-view--mailbox-activity-entry seg data-buf))))
                   (special (and entry
                                 (or (plist-get entry :notice-p)
                                     (and (eq (car seg) 'tool)
                                 (or (plist-get (plist-get entry :rendering)
                                                :coalesce-key)
                                     (not (mevedel-view--tool-group-entry-p
                                           entry))))))))
              ;; This transcript's own completion delivery renders nothing,
              ;; so it must not split the activity run around it either.
              (unless (and (eq (car seg) 'mailbox) (null entry))
                (when (and run (not (eq (and special t) special-run)))
                  (render-run (nreverse run))
                  (setq run nil))
                (setq special-run (and special t))
                (push seg run))))
          (when run (render-run (nreverse run) pending)))))
    (mevedel-view--mark-live-render-unit
     unit-start (cadr (car segments)))))

(defun mevedel-view--insert-assistant-header (&optional variant-button extra)
  "Insert the `Assistant' turn heading at point.
VARIANT-BUTTON, when non-nil, follows the label.  EXTRA is a property
list added to the heading's text."
  (let ((header-start (point)))
    (insert "Assistant")
    (when variant-button
      (insert "  " variant-button))
    (insert "\n")
    (add-text-properties
     header-start (point)
     (append '(font-lock-face mevedel-view-assistant-header
               mevedel-view-type turn-header
               mevedel-view-turn-role assistant
               mevedel-view-collapsed nil)
             extra))))

(defun mevedel-view--live-turn-needs-header-p (start end data-buf)
  "Return non-nil when running calls at END need an `Assistant' heading.
START..END is the in-flight turn's rendering.  The heading is missing
when no turn heading precedes END there, or the last one belongs to
another role, such as a mailbox delivery.  A directive request in
DATA-BUF has its own heading, so it never needs one."
  (unless (with-current-buffer data-buf
            (and mevedel--current-request
                 (mevedel-request-directive-uuid mevedel--current-request)))
    (let ((pos start) last)
      (while (and (< pos end)
                  (setq pos (text-property-any pos end 'mevedel-view-type
                                               'turn-header)))
        (setq last pos
              pos (or (next-single-property-change
                       pos 'mevedel-view-type nil end)
                      end)))
      (not (and last (eq (get-text-property last 'mevedel-view-turn-role)
                         'assistant))))))

(defun mevedel-view--render-assistant-turn
    (segments data-buf &optional variant-button directive continuation-p)
  "Render assistant SEGMENTS from DATA-BUF.
Response text is shown inline, tool calls as collapsed one-liners,
reasoning blocks as collapsed summaries.  Reasoning inside a plain tool
activity run does not prevent that run from folding.  Adjacent thinking
segments are merged into a single summary.  VARIANT-BUTTON, when non-nil,
is inserted beside the header.  CONTINUATION-P suppresses that header."
  (unless (or directive continuation-p)
    (mevedel-view--insert-assistant-header variant-button))
  (let ((view-buf (current-buffer))
        tool-group thinking-group request-summary-group)
    (cl-labels
        ((flush-thinking ()
           (mevedel-view--flush-thinking-group thinking-group data-buf)
           (setq thinking-group nil))
         (flush-tools (&optional trailing)
           (when tool-group
             (mevedel-view--render-tool-activity
              (nreverse tool-group) data-buf
              ;; Only the turn's final activity can take in-flight calls.
              (and trailing (mevedel-view--take-render-pending)))
             (setq tool-group nil))))
      (dolist (seg segments)
        (let ((type (car seg)))
          (pcase type
            ('response
             ;; Flush accumulated groups
             (flush-thinking)
             (flush-tools)
             ;; Insert response text with source tracking
             (let ((seg-start (cadr seg))
                   (seg-end (caddr seg))
                   (source nil))
               (setq source
                     (mevedel-view-disclosure-source-range data-buf seg-start seg-end))
               (with-current-buffer data-buf
                 (let ((text (string-trim
                               (buffer-substring-no-properties seg-start seg-end))))
                   (setq text (mevedel-view--visible-response-text text))
                   (with-current-buffer view-buf
                     (unless (string-empty-p text)
                       (let ((unit-start (point)))
                         (mevedel-view--ensure-blank-line-before-response)
                         ;; The rule belongs to this prose's live unit, so
                         ;; retained activity above it never changes.
                         (mevedel-view--insert-activity-rule-before-response)
                         (let ((start (point)))
                         (insert (mevedel-view--fontify-response text) "\n")
                         (let ((response-end (copy-marker (point) t)))
                           (add-text-properties
                            start response-end
                            `(mevedel-view-source ,source
                              mevedel-view-source-key ,(mevedel-view-disclosure-state-key
                                                        source
                                                        'response)
                              mevedel-view-type response
                              mevedel-view-collapsed nil))
                           (mevedel-view--decorate-agent-result-blocks
                            start response-end)
                           (mevedel-view--decorate-agent-message-blocks
                            start response-end)
                           ;; Idle table layout must not prettify an unfinished
                           ;; streamed response: the next chunk would replace it
                           ;; with raw source again.  Terminal reconciliation
                           ;; redecorates without this flag.
                           (let ((mevedel-view-table--streaming-p
                                  (and (not mevedel-view-disclosure--settling-p)
                                       (markerp mevedel-view--data-turn-start)
                                       (marker-position mevedel-view--data-turn-start)
                                       (mevedel-view-stream-in-flight-turn-start-position)
                                       (>= seg-start
                                           (marker-position
                                            mevedel-view--data-turn-start)))))
                             (mevedel-view--decorate-markdown-in-range
                              start response-end))
                           (goto-char response-end)
                           (set-marker response-end nil)))
                         (mevedel-view--mark-live-render-unit
                          unit-start seg-start))))))))
            ('tool
             ;; Flush thinking group before tools
             (flush-thinking)
             ;; Accumulate consecutive tool segments
             (push seg tool-group))
            ('system-reminder
             (flush-thinking)
             (flush-tools)
             (mevedel-view--render-system-reminder-segment seg data-buf))
            ('request-summary
             (push seg request-summary-group))
            ('mailbox
             (flush-thinking)
             (if tool-group
                 (push seg tool-group)
               (let ((text (mevedel-view--user-turn-text (list seg) data-buf))
                     (text-start nil))
                 (mevedel-view--ensure-blank-line-before-response)
                 (setq text-start (point))
                 (insert text "\n")
                 (mevedel-view--decorate-agent-result-blocks text-start (point))
                 (mevedel-view--decorate-agent-message-blocks text-start (point)))))
            ('user
             (let ((seg-start (cadr seg))
                   (seg-end (caddr seg)))
               ;; Drop org-only glue (`#+end_tool', `#+begin_tool …',
               ;; blank lines) so it doesn't surface as a one-line
               ;; `Thinking…' between adjacent tool blocks.  Skip without
               ;; flushing the tool-group so consecutive tool segments
               ;; separated only by glue still group / render together.
               (unless (mevedel-view--scaffolding-only-p
                        data-buf seg-start seg-end)
                 (flush-tools)
                 (push seg thinking-group))))
            ((or 'reasoning 'render-data 'ignored)
             (cond
              ((and (eq type 'ignored)
                    (mevedel-view--hook-audit-only-segment-p
                     data-buf (cadr seg) (caddr seg))
                    (not tool-group))
               (flush-thinking)
               (let* ((source (mevedel-view-disclosure-source-range
                               data-buf (cadr seg) (caddr seg)))
                      (text (with-current-buffer data-buf
                              (buffer-substring
                               (cadr seg) (caddr seg)))))
                 (dolist (record
                          (mevedel-view--hook-audit-records-from-text text))
                   (mevedel-view--insert-hook-audit-block record source))))
              ((mevedel-view--collaboration-event-segment-p
                data-buf (cadr seg) (caddr seg))
               (flush-thinking)
               (flush-tools)
               (mevedel-view--render-collaboration-event-segment data-buf seg))
              ((and tool-group
                    (mevedel-view--hook-audit-only-segment-p
                     data-buf (cadr seg) (caddr seg)))
               (push seg tool-group))
              (t
                 ;; Drop org-only glue (`#+end_tool', `#+begin_tool …', blank
                 ;; lines) so it doesn't surface as a one-line `Thinking…'
                 ;; between adjacent tool blocks.  Skip without flushing the
                 ;; tool-group so consecutive tool segments separated only
                 ;; by glue still group / render together.
                 ;; Thinking after a tool stays pending so a long plain tool run
                 ;; can fold across it.  Leading thinking keeps its old path.
                 (if tool-group
                     (push seg tool-group)
                   (push seg thinking-group))))))))
      ;; Flush remaining groups
      (flush-thinking)
      (flush-tools t)
      ;; Calls after the turn's last prose start their run here, where
      ;; they will settle, rather than after the turn's closing spacing.
      (when-let* ((left (mevedel-view--take-render-pending)))
        (mevedel-view--insert-pending-run left))
      (dolist (seg (nreverse request-summary-group))
        (mevedel-view--render-request-summary-segment seg data-buf)))))

(defun mevedel-view--render-collaboration-event-segment (data-buf seg)
  "Render a canonical started collaboration event SEG from DATA-BUF."
  (let* ((seg-start (cadr seg))
         (seg-end (caddr seg))
         (source (mevedel-view-disclosure-source-range data-buf seg-start seg-end))
         (render-data
          (with-current-buffer data-buf
            (mevedel-view--collaboration-event-from-text
             (buffer-substring seg-start seg-end)))))
    (when-let* ((rendering
                 (and render-data
                      (mevedel-tool-ui--render-agent
                       "Agent"
                       (list :task_name
                             (file-name-nondirectory
                              (or (plist-get render-data :path) "agent"))
                             :message
                             (or (plist-get render-data :description) ""))
                       (or (plist-get render-data :body) "")
                       render-data))))
      (mevedel-view--insert-rendered-tool rendering source))))

(defun mevedel-view--same-tool-call-segment-p (left right data-buf)
  "Return non-nil when LEFT and RIGHT belong to the same tool call."
  (and (eq (car left) 'tool)
       (eq (car right) 'tool)
       (with-current-buffer data-buf
         (and (not (mevedel-transcript--org-tool-block-parts
                    (cadr right) (caddr right)))
              (let ((left-id (mevedel-transcript--tool-id-in-range
                              (cadr left) (caddr left)))
                    (right-id (mevedel-transcript--tool-id-in-range
                               (cadr right) (caddr right))))
                (and left-id right-id (equal left-id right-id)))))))

(defun mevedel-view--merge-tool-hook-audit-segments (segments data-buf)
  "Merge hook audit side-channel SEGMENTS into adjacent tool segments."
  (let (out)
    (dolist (seg segments (nreverse out))
      (cond
       ((and out
             (eq (caar out) 'tool)
             (eq (car seg) 'ignored)
             (mevedel-view--hook-audit-only-segment-p
              data-buf (cadr seg) (caddr seg)))
        (setcar out (list 'tool (cadar out) (caddr seg))))
       ((and out
             (mevedel-view--same-tool-call-segment-p (car out) seg data-buf))
        (setcar out (list 'tool (cadar out) (caddr seg))))
       (t
        (push seg out))))))

(defconst mevedel-view--tool-group-verbs
  '(("Grep" "searched %d pattern" "searched %d patterns")
    ("Glob" "matched %d glob" "matched %d globs")
    ("Read" "read %d file" "read %d files")
    ("Bash" "ran %d command" "ran %d commands")
    ("Eval" "evaluated %d form" "evaluated %d forms")
    ("WebFetch" "fetched %d page" "fetched %d pages")
    ("WebSearch" "ran %d web search" "ran %d web searches"))
  "Verb phrases for grouped tool run summaries, per tool name.
Each entry is (NAME SINGULAR PLURAL) with a `%d' count slot.  Tools
without an entry -- MCP tools included -- fall back to \"NAME xN\".")

(defun mevedel-view--group-child-completion (child)
  "Return the execution completion grouped CHILD presents, or nil."
  (when (eq (plist-get child :kind) 'completion)
    (plist-get (plist-get child :rendering) :completion-record)))

(defun mevedel-view--group-completions (children)
  "Return the distinct origin execution completions among grouped CHILDREN."
  (let (records)
    (dolist (child children (nreverse records))
      (when-let* ((record (mevedel-view--group-child-completion child))
                  ((not (cl-find-if
                         (lambda (prior)
                           (and (equal (plist-get prior :execution-id)
                                       (plist-get record :execution-id))
                                (equal (plist-get prior :owner)
                                       (plist-get record :owner))))
                         records))))
        (push record records)))))

(defun mevedel-view--completion-summary-parts (records)
  "Return group summary phrases counting completion RECORDS by outcome."
  (let (counts parts)
    (dolist (record records)
      (cl-incf (alist-get (downcase (mevedel-view-audit-breadcrumb-status record))
                          counts 0 nil #'equal)))
    (dolist (status '("completed" "failed" "stopped" "timed out") (nreverse parts))
      (when-let* ((count (alist-get status counts nil nil #'equal)))
        (push (if parts
                  (format "%d %s" count status)
                (format "%d command%s %s" count (if (= count 1) "" "s") status))
              parts)))))

(defun mevedel-view--tool-group-header (children &optional completions)
  "Return the one-line activity summary for grouped CHILDREN.
COMPLETIONS are the distinct execution completions among them."
  (let (names counts (thought-count 0) (message-count 0))
    (dolist (child children)
      (cond
       ((mevedel-view--group-child-completion child))
       ((eq (plist-get child :kind) 'mailbox) (cl-incf message-count))
       ((eq (plist-get child :kind) 'reasoning) (cl-incf thought-count))
       (t
        (let ((name (or (plist-get child :tool) "Tool")))
          (unless (member name names)
            (push name names))
          (cl-incf (alist-get name counts 0 nil #'equal))))))
    (setq names (nreverse names))
    (let* ((parts
            (append
             (mapcar
              (lambda (name)
                (let ((count (alist-get name counts 0 nil #'equal))
                      (verb (assoc name mevedel-view--tool-group-verbs)))
                  (cond
                   (verb
                    (format (if (= count 1) (nth 1 verb) (nth 2 verb))
                            count))
                   ((= count 1) name)
                   (t (format "%s ×%d" name count)))))
              names)
             (mevedel-view--completion-summary-parts completions)
             (when (> message-count 0)
               (list (format (if (= message-count 1)
                                 "received %d message"
                               "received %d messages")
                             message-count)))
             (when (> thought-count 0)
               (list (format (if (= thought-count 1)
                                 "thought %d time"
                               "thought %d times")
                             thought-count)))))
           (summary (string-join parts ", ")))
      ;; Capitalize only a verb phrase; a leading tool name keeps its
      ;; own casing.
      (if (and (not (string-empty-p summary))
               (assoc (car names) mevedel-view--tool-group-verbs))
          (concat (upcase (substring summary 0 1))
                  (substring summary 1))
        summary))))

(defun mevedel-view--tool-group-child (entry data-buf index &optional summary-only)
  "Return the nested-call plist for activity ENTRY in DATA-BUF, or nil.
INDEX discriminates the row's disclosure key from its siblings.  A
segment whose own rendering is hidden yields nil so grouping does not
resurrect rows the renderer suppressed.  SUMMARY-ONLY permits a cached
tool name instead of reparsing arguments and results for a collapsed group.
A large result still being prepared off the main thread is never parsed
here; its row shows the call as running until the result is published."
  (when-let* ((child
               (or (and (eq (plist-get (plist-get entry :rendering) :vtype)
                            'tool-preparing)
                        (list :tool (plist-get (plist-get entry :rendering)
                                               :group-tool)
                              :status 'running))
                   (and summary-only
                        (when-let* ((name (plist-get (plist-get entry :rendering)
                                                    :group-tool)))
                          (list :tool name)))
                   (plist-get entry :group-child)
                   (when-let* ((parsed
                                (mevedel-view--tool-call-parse
                                 data-buf
                                 (plist-get entry :start)
                                 (plist-get entry :end))))
                     (list :tool (plist-get parsed :name)
                           :args (plist-get parsed :args)
                           :status
                           (mevedel-view--tool-render-status
                            (plist-get parsed :result)
                            (plist-get parsed :render-data))
                           :result (plist-get parsed :result)
                           :render-data (plist-get parsed :render-data))))))
    (append (list :id (format "%d" index) :order index
                  :source (mevedel-view-disclosure-source-range
                           data-buf (plist-get entry :start)
                           (plist-get entry :end)))
            child)))

(defun mevedel-view--tool-group-rendering
    (entries data-buf &optional summary-only pending)
  "Return the grouped rendering for activity ENTRIES in DATA-BUF, or nil.
The result reuses the compound-tool row machinery: each tool, reasoning
occurrence, or delivery is a `:child-calls' entry with its own disclosure
state.  A newly formed group keeps an already open child visible.
SUMMARY-ONLY omits cached tool bodies; recompute before expanding children.
PENDING in-flight tool entries count into the summary as running calls
and follow the settled children."
  (let* ((settled
          (let ((index 0)
                out)
            (dolist (entry entries (nreverse out))
              (when-let* ((child (mevedel-view--tool-group-child
                                  entry data-buf index summary-only)))
                (push child out))
              (cl-incf index))))
         (children (append settled (mevedel-view--pending-children pending))))
    (when children
      (let ((completions (mevedel-view--group-completions children))
            (reader-p (mevedel-view-render--reader-on-member-p settled pending)))
      (list :header (mevedel-view--tool-group-header children completions)
            :execution-completions completions
            :vtype 'tool-group
            :expandable-p t
            :child-calls children
            :pending-count (mevedel-view--pending-running-count pending)
            :pending-failed (mevedel-view--pending-failed-count pending)
            :failed-count (mevedel-view--group-failed-count entries)
            :failed-names (append (mevedel-view--group-failed-names entries)
                                  (mevedel-view--pending-failed-names pending))
            :status (let ((statuses
                           (mapcar (lambda (entry)
                                     (mevedel-view--rendering-status
                                      (plist-get entry :rendering)))
                                   entries)))
                      (cond ((memq 'error statuses) 'error)
                            ((memq 'warning statuses) 'warning)))
            :reader-open reader-p
            ;; A new group must not hide a row the reader already opened or
            ;; is on.  An explicit fold of the group itself still takes
            ;; precedence.
            :initially-collapsed-p
            (not (or reader-p
                     (cl-some
                      (lambda (child)
                        (when-let* ((state
                                     (mevedel-view-disclosure-state-for-key
                                      (mevedel-view--child-call-state-key
                                       child (plist-get child :source)))))
                          (not (cdr state))))
                      settled))))))))

(defun mevedel-view--fit-group-header (header rendering)
  "Return HEADER shortened so RENDERING's folded group row is one screen line.
The summary grows with every kind of tool a run uses, and its running and
failed counts come and go; a summary that wrapped would add or drop a row
as they did.  Room is kept for those counts, measured against the
narrowest window showing the view."
  (let* ((windows (get-buffer-window-list (current-buffer) nil t))
         (width (if windows
                    (apply #'min (mapcar #'window-body-width windows))
                  (window-body-width)))
         (count (plist-get rendering :pending-count))
         (failed (+ (or (plist-get rendering :pending-failed) 0)
                    (or (plist-get rendering :failed-count) 0)))
         (suffix (concat (and count (format " · %d running" count))
                         (and (> failed 0)
                              (concat " · " (mevedel-view--failed-label
                                            failed
                                            (plist-get rendering :failed-names))))))
         ;; The marker column, a line prefix for nested rows, and the
         ;; last column, which would wrap the line in a terminal.
         (available (max 10 (- width 4 2 1 (string-width suffix)))))
    (if (> (string-width header) available)
        (truncate-string-to-width header available nil nil "…")
      header)))

(defun mevedel-view--group-failed-count (entries)
  "Return how many settled activity ENTRIES failed, or nil for none."
  (let ((count (cl-count-if
                (lambda (entry)
                  (and (eq (plist-get entry :kind) 'tool)
                       (eq (mevedel-view--rendering-status
                            (plist-get entry :rendering))
                           'error)))
                entries)))
    (and (> count 0) count)))

(defun mevedel-view--group-failed-names (entries)
  "Return the tool names of settled activity ENTRIES that failed.
Use the name each rendering carries for grouping, never reparsing a call."
  (delq nil (mapcar (lambda (entry)
                      (let ((rendering (plist-get entry :rendering)))
                        (and (eq (plist-get entry :kind) 'tool)
                             (eq (mevedel-view--rendering-status rendering) 'error)
                             (or (plist-get rendering :group-tool) "Tool"))))
                    entries)))

(defun mevedel-view-render--reader-members ()
  "Return the tool rows under this view's point and its window points."
  (let (members)
    (dolist (pos (cons (point)
                       (mapcar #'window-point
                               (get-buffer-window-list nil nil t))))
      (when (and (< pos (point-max))
                 (memq (get-text-property pos 'mevedel-view-type)
                       '(tool-summary tool-pending)))
        (if-let* ((key (get-text-property pos 'mevedel-view-pending-key)))
            (progn
              (push key members)
              ;; Its settled row is found by the call's transcript id.
              (when-let* ((id (get-text-property
                               pos 'mevedel-view-pending-tool-use-id)))
                (push (list :tool-use-id id) members)))
          (when-let* ((start (mevedel-view-disclosure-source-start
                              (get-text-property pos 'mevedel-view-source))))
            (push start members)))))
    members))

(defun mevedel-view-render--reader-on-member-p (children pending)
  "Return non-nil when a reader is on one of CHILDREN or PENDING calls.
CHILDREN are settled group children; PENDING are in-flight entries.  A
reader on a running row still holds that call once it settles: the row's
bound tool-use id is the id its result carries in the transcript."
  (when-let* ((members mevedel-view-render--reader-members))
    (let ((ids (delq nil (mapcar (lambda (member)
                                   (plist-get (and (consp member)
                                                   (keywordp (car member))
                                                   member)
                                              :tool-use-id))
                                 members))))
      (or (cl-some (lambda (child)
                     (let ((source (plist-get child :source)))
                       (or (member (mevedel-view-disclosure-source-start source)
                                   members)
                           (and ids (consp source) (markerp (car source))
                                (member (with-current-buffer
                                            (marker-buffer (car source))
                                          (mevedel-transcript--tool-id-in-range
                                           (car source) (cdr source)))
                                        ids)))))
                   children)
          (cl-some (lambda (entry)
                     (or (member (car entry) members)
                         (and ids (member (plist-get (nth 3 entry) :tool-use-id)
                                          ids))))
                   pending)))))

(defun mevedel-view--tool-group-rendering-from-source
    (data-buf start end)
  "Rebuild a grouped activity rendering for DATA-BUF START..END.
`mevedel-transcript-segments' expands its END to the containing property
run, so re-segmenting a folded run also returns the segment that begins
where the run ended.  That row was never part of the group: keeping it
summarizes the following response, mailbox, or prompt as one more
reasoning occurrence."
  (let ((entries
         (with-current-buffer data-buf
           (save-restriction
             (widen)
             (mevedel-view--tool-activity-entries
              (cl-remove-if (lambda (seg) (>= (cadr seg) end))
                            (mevedel-transcript-segments start end))
              data-buf)))))
    (mevedel-view--tool-group-rendering entries data-buf)))

(defun mevedel-view--tool-group-entry-p (entry)
  "Return non-nil when ENTRY may fold into a grouped activity row.
Rows that demand individual presentation stay out: agent handles and other
non-tool vtypes, compound tools with their own nested
rows, rows carrying hook audits, rows their renderer wants expanded or compact,
coalesced rows, and renderer fallbacks.  Warning and note disclosures
stay with their nested row inside the group.  Failed calls and results still
being prepared stay too: a run's grouping must not change when a call fails
or its result is published."
  (let ((rendering (plist-get entry :rendering)))
    (or (eq (plist-get entry :kind) 'completion)
    (and rendering
         (= (plist-get entry :count) 1)
         (memq (or (plist-get rendering :vtype) 'tool-summary)
               '(tool-summary tool-preparing))
         (null (plist-get rendering :child-calls))
         (null (plist-get rendering :hook-audits))
         (not (plist-get rendering :force-expanded-p))
         (not (and (plist-member rendering :expandable-p)
                   (not (plist-get rendering :expandable-p))))
         (not (and (plist-member rendering :initially-collapsed-p)
                   (not (plist-get rendering :initially-collapsed-p))))))))

(defun mevedel-view--insert-tool-group (entries data-buf &optional pending)
  "Insert grouped activity ENTRIES as one expandable row for DATA-BUF.
PENDING in-flight tool entries join the group as running calls.
Return non-nil when the group row was inserted."
  ;; A split breadcrumb entry follows its call but ends inside the call's
  ;; segment, so the last entry need not reach the group's end.  Collapse
  ;; removes only rows contained in this range.
  (let* ((group-start (apply #'min (mapcar (lambda (entry)
                                             (plist-get entry :start))
                                           entries)))
         (group-end (apply #'max (mapcar (lambda (entry)
                                           (plist-get entry :end))
                                         entries)))
         (rendering (mevedel-view--tool-group-rendering
                     entries data-buf t pending))
         (source (and rendering
                      (mevedel-view-disclosure-source-range
                       data-buf group-start group-end))))
    (when rendering
      (when (and (eq (car mevedel-view-render--pending-reader-group)
                     mevedel-view--data-turn-start)
                 (eql (cadr mevedel-view-render--pending-reader-group) group-start))
        (unless (mevedel-view-disclosure-state-entry source 'tool-group)
          (mevedel-view-disclosure-record-state source 'tool-group nil))
        (setq mevedel-view-render--pending-reader-group nil))
      (unless (plist-get
               (mevedel-view-disclosure-apply-rendering-state rendering source)
               :initially-collapsed-p)
        ;; A group that formed open under the reader stays open after the
        ;; reader moves on: folding later would move what they just read.
        (when (and (plist-get rendering :reader-open)
                   (not (mevedel-view-disclosure-state-entry source 'tool-group)))
          (mevedel-view-disclosure-record-state source 'tool-group nil))
        (setq rendering (mevedel-view--tool-group-rendering
                         entries data-buf nil pending)))
      (mevedel-view--insert-rendered-tool rendering source)
      t)))

(defun mevedel-view--render-tool-group (tool-segments data-buf &optional pending)
  "Render consecutive TOOL-SEGMENTS from DATA-BUF.
Each tool call gets its own collapsible entry.  A registered
`:renderer' is invoked when the segment carries a render-data
side-channel, falling back to the default one-liner otherwise.
Adjacent visible renderings with equal `:coalesce-key' values keep
only the final row and show the number of combined calls.  Hook audits
from every combined call remain in source order.  Runs of more than
`mevedel-view-tool-group-collapse-threshold' plain rows fold into one
grouped activity row that expands into compound-tool nested rows.
PENDING in-flight tool entries extend the trailing run, counting toward
its threshold, or start a run of their own after a row that cannot fold."
  (let* ((unit-start (point))
         (unit-source (cadr (car tool-segments)))
         (tool-segments
          (mevedel-view--merge-tool-hook-audit-segments
           tool-segments data-buf))
         (entries
          (mevedel-view--split-completion-entries
          (let (out)
            (dolist (seg tool-segments (nreverse out))
              (when-let* ((entry (mevedel-view--tool-segment-entry
                                  seg data-buf)))
                (let* ((rendering (plist-get entry :rendering))
                       (key (plist-get rendering :coalesce-key))
                       (previous (car out)))
                    (if (and key
                             previous
                             (equal
                              key
                              (plist-get
                               (plist-get previous :rendering)
                               :coalesce-key)))
                        (progn
                          (setq rendering
                                (plist-put
                                 (copy-sequence rendering)
                                 :hook-audits
                                 (append
                                  (plist-get
                                   (plist-get previous :rendering)
                                   :hook-audits)
                                  (plist-get rendering :hook-audits))))
                          (setq entry
                                (plist-put
                                 (plist-put entry :rendering rendering)
                                 :count
                                 (1+ (plist-get previous :count))))
                          (setcar out entry))
                      (push entry out))))))))
        (start-time (float-time))
        ;; Spacing after prose is decided once per run, not per row.
        (spaced nil)
        (rendered 0)
        (fallbacks 0))
    (unwind-protect
        (cl-flet*
            ((insert-entry
               (entry)
               (let* ((seg-start (plist-get entry :start))
                      (seg-end (plist-get entry :end))
                      (source (plist-get entry :source))
                      (count (plist-get entry :count))
                      (rendering (plist-get entry :rendering)))
                 (when (and rendering (> count 1))
                   (setq rendering
                         (plist-put
                          (copy-sequence rendering)
                          :header
                          (format "%s ×%d"
                                  (plist-get rendering :header)
                                  count))))
                 (cond
                  ((eq (plist-get entry :kind) 'completion)
                   (let ((record (plist-get
                                  (plist-get (plist-get entry :group-child)
                                             :rendering)
                                  :completion-record)))
                     (mevedel-view--insert-hook-audit-block
                      record (plist-get record :source))))
                  ((eq (plist-get entry :kind) 'hook-audit)
                   (dolist (record (plist-get entry :hook-audits))
                     (mevedel-view--insert-hook-audit-block
                      record (plist-get record :source))))
                  (rendering
                   (unless spaced
                     (mevedel-view--ensure-blank-line-after-response)
                     (setq spaced t))
                   (cl-incf rendered)
                   (mevedel-view--insert-rendered-tool rendering source))
                  (t
                   (when-let* ((summary (mevedel-view--tool-one-liner
                                         data-buf seg-start seg-end)))
                     (unless spaced
                       (mevedel-view--ensure-blank-line-after-response)
                       (setq spaced t))
                     (cl-incf fallbacks)
                     (let ((ins-start (point)))
                       (mevedel-view--insert-summary-region
                        (mevedel-view--summary-with-face
                         summary 'mevedel-view-tool-summary)
                        `(mevedel-view-type tool-summary
                          mevedel-view-collapsed t
                          mevedel-view-source ,source
                          mevedel-view-source-key
                          ,(mevedel-view-disclosure-state-key
                            source 'tool-summary)))
                       (mevedel-view--decorate-markdown-in-range
                        ins-start (point))))))))
             (flush-run
               (run &optional pending)
               (let* ((run-entries (nreverse run))
                      (total (+ (length run-entries) (length pending))))
                 (if (and (> mevedel-view-tool-group-collapse-threshold 0)
                          run-entries
                          (> total mevedel-view-tool-group-collapse-threshold)
                          (progn
                            (unless spaced
                              (mevedel-view--ensure-blank-line-after-response)
                              (setq spaced t))
                            (mevedel-view--insert-tool-group
                             run-entries data-buf pending)))
                     (cl-incf rendered (length run-entries))
                   (mapc #'insert-entry run-entries)
                   (when pending
                     (if run-entries
                         (mapc #'mevedel-view--insert-pending-tool-row pending)
                       (mevedel-view--insert-pending-run pending)))))))
          (let (run)
            (dolist (entry entries)
              (if (mevedel-view--tool-group-entry-p entry)
                  (push entry run)
                (when run
                  (flush-run run)
                  (setq run nil))
                (insert-entry entry)))
            (when (or run pending)
              (flush-run run pending))))
      (mevedel-view--debug-log
       'render-tool-group
       :segments (length tool-segments)
       :rendered rendered
       :fallbacks fallbacks
       :elapsed (- (float-time) start-time)))
    (mevedel-view--mark-live-render-unit unit-start unit-source)))

(defun mevedel-view--tool-readable-start (raw)
  "Return the reader offset of the tool call in RAW.

Text-property boundaries can include org drawers, `#+begin_tool'
markers, or other unpropertized scaffolding.  Prefer the structural
tool form itself when it is present inside RAW.  Return an offset so
reading the call does not copy its potentially large result body."
  (let ((start 0))
    (when (string-match
           "\\`[ \t\n]*:PROPERTIES:\n\\(?:.*\n\\)*?:END:\n?" raw)
      (setq start (match-end 0)))
    (when (and (string-match
                "\\(?:[ \t]*\\(?:#\\+\\(?:begin\\|end\\)_\\(?:tool\\|reasoning\\)[^\n]*\\)?\n\\)+"
                raw start)
               (= (match-beginning 0) start))
      (setq start (match-end 0)))
    (if (string-match "(\\s-*:name\\_>" raw start)
        (match-beginning 0)
      start)))

(defun mevedel-view--tool-wrapped-text-p (raw)
  "Return non-nil when RAW includes persisted org tool block scaffolding."
  (and (stringp raw)
       (string-match-p "\\`\\(?:[ \t\n]*\\|:PROPERTIES:\n\\(?:.*\n\\)*?:END:\n?\\)*#\\+begin_tool\\b"
                       raw)))

(defun mevedel-view--direct-tool-readable-text-p (raw)
  "Return non-nil when RAW itself begins with a readable tool call."
  (when (stringp raw)
    (let ((text (string-trim-left raw)))
      (and (string-match-p "\\`(\\s-*:name\\_>" text)
           (condition-case nil
               (let ((sexp (read text)))
                 (and (listp sexp)
                      (stringp (plist-get sexp :name))))
             (error nil))))))

(defun mevedel-view--complete-wrapped-tool-text-p (raw)
  "Return non-nil when RAW is already one complete wrapped tool block."
  (and (mevedel-view--tool-wrapped-text-p raw)
       (string-match-p "\n#\\+end_tool[^\n]*\n?\\'" raw)
       (condition-case nil
           (let ((sexp (car (read-from-string
                            raw (mevedel-view--tool-readable-start raw)))))
             (and (listp sexp)
                  (stringp (plist-get sexp :name))))
         (error nil))))

(defun mevedel-view--strip-trailing-tool-marker (text)
  "Return TEXT without a trailing org `#+end_tool' marker."
  (if (stringp text)
      (string-trim-right
       (replace-regexp-in-string "\n*#\\+end_tool[^\n]*\\'" "" text t t))
    text))

(defvar-local mevedel-view--tool-block-bounds-memo nil
  "Data-buffer memo for `mevedel-view--tool-block-bounds'.
Keys are (SEG-START SEG-END TICK) integers with TICK from
`buffer-modified-tick': bounds depend on buffer text and gptel text
properties, and transcript restoration stamps properties without
character changes, so the chars tick would serve stale bounds.
Bounded through `mevedel-view--cache-put'.")

(defvar-local mevedel-view--tool-block-bounds-memo-entries 0
  "Entry count backing `mevedel-view--tool-block-bounds-memo'.")

(defun mevedel-view--tool-block-bounds (seg-start seg-end)
  "Return org tool-block bounds overlapping SEG-START..SEG-END, or nil.

Restored `GPTEL_BOUNDS' can drift into the `#+begin_tool' line or
past `#+end_tool' when older transcripts are opened.  The org block
markers remain structural anchors, so use them to recover the whole
tool block before parsing the tool plist and render-data side channel.

The underlying search is unbounded in both directions, so the result
is memoized per segment and buffer tick; segment text parsing asks for
the same bounds several times per redraw."
  (let* ((key (list (mevedel-view--source-position seg-start)
                    (mevedel-view--source-position seg-end)
                    (buffer-modified-tick)))
         (memo (or mevedel-view--tool-block-bounds-memo
                   (setq mevedel-view--tool-block-bounds-memo
                         (make-hash-table :test #'equal))))
         (cached (gethash key memo 'mevedel--miss)))
    (if (not (eq cached 'mevedel--miss))
        cached
      (mevedel-view--cache-put
       memo key
       (mevedel-transcript--tool-block-bounds-for-run seg-start seg-end)
       'mevedel-view--tool-block-bounds-memo-entries))))

(defun mevedel-view--tool-segment-text (seg-start seg-end &optional raw)
  "Return raw tool text for SEG-START..SEG-END.
If the segment overlaps an org tool block, expand to the block bounds
first so stale restored text properties do not hide the `(:name ...)'
form or the render-data block from the parser.
RAW, when supplied, is the already copied SEG-START..SEG-END text."
  (let ((raw (or raw (buffer-substring seg-start seg-end))))
    (if (or (mevedel-view--complete-wrapped-tool-text-p raw)
            (and (not (mevedel-view--tool-wrapped-text-p raw))
                 (mevedel-view--direct-tool-readable-text-p raw)))
        raw
      (pcase-let ((`(,start . ,end)
                   (or (mevedel-view--tool-block-bounds seg-start seg-end)
                       (cons seg-start seg-end))))
        (buffer-substring start end)))))

;;
;;; Disclosure content

(defun mevedel-view--response-summary (data-buf data-start data-end)
  "Build a one-line response summary for DATA-START..DATA-END in DATA-BUF."
  (let* ((text
          (mevedel-view--visible-response-text
           (mevedel-view-disclosure-data-substring
            data-buf data-start data-end)))
         (trimmed (string-trim text))
         (lines (split-string trimmed "\n"))
         (non-empty (seq-drop-while #'string-empty-p lines))
         (first-line (or (car non-empty) ""))
         (line-count (length lines)))
    (mevedel-view--operation-line
     (string-trim mevedel-view--response-glyph)
     'mevedel-view-response-marker
     (concat (mevedel-view-disclosure-truncate-line first-line 80)
             (if (> line-count 1) "..." ""))
     nil (format "(%d lines)" line-count)
     'mevedel-view-response-summary)))

(defun mevedel-view--prompt-drawer-body (data-buf data-start data-end)
  "Return prompt disclosure text for DATA-START..DATA-END in DATA-BUF."
  (with-current-buffer data-buf
    (save-excursion
      (goto-char data-start)
      (if (re-search-forward "^:PROMPT:\n" data-end t)
          (let ((body-start (point)))
            (if (re-search-forward "^:END:[ \t]*\n?" data-end t)
                (buffer-substring-no-properties
                 body-start (match-beginning 0))
              (buffer-substring-no-properties body-start data-end)))
        (mevedel-view--user-turn-text
         (list (list 'user data-start data-end)) data-buf)))))

(defun mevedel-view-render-insert-expanded-disclosure
    (data-buf source vtype header)
  "Insert expanded VTYPE content from SOURCE in DATA-BUF.
HEADER is retained for disclosure kinds whose collapsed label remains visible.
Return the normalized source coordinates used for the insertion."
  (let* ((trimmed
          (and (eq vtype 'thinking-summary)
               (mevedel-view--reasoning-source-bounds
                data-buf (car source) (cdr source))))
         (source (if trimmed
                     (mevedel-view-disclosure-source-range
                      data-buf (car trimmed) (cdr trimmed))
                   source))
         (data-start (car source))
         (data-end (cdr source))
         (rendering
          (if (eq vtype 'tool-group)
              (mevedel-view--tool-group-rendering-from-source
               data-buf data-start data-end)
            (let ((candidate
                   (mevedel-view--segment-rendering
                    data-buf data-start data-end)))
              (and (eq vtype
                       (or (plist-get candidate :vtype) 'tool-summary))
                   candidate))))
         (start (point)))
    (if rendering
        (progn
          (mevedel-view--render-expanded-body rendering source)
          (mevedel-view-render-add-display-properties
           start (point) (plist-get rendering :vtype)))
      (let ((text (mevedel-view-disclosure-data-substring
                   data-buf data-start data-end
                   (memq vtype '(hook-audit thinking-summary))))
            body-start)
        (when (eq vtype 'thinking-summary)
          (setq text
                (mevedel-view--fontify-as
                 (string-trim (mevedel-view--clean-reasoning-text text))
                 'markdown-mode)))
        (when (eq vtype 'response)
          (setq text (mevedel-view--fontify-response (string-trim text))))
        (when (eq vtype 'prompt-summary)
          (let* ((source-text
                  (mevedel-view-disclosure-data-substring
                   data-buf data-start data-end t))
                 (inline-body
                  (mevedel-view--inline-skill-prompt-summary-body source-text))
                 (drawer-body
                  (string-trim
                   (mevedel-view--prompt-drawer-body
                    data-buf data-start data-end))))
            (setq text
                  (mevedel-view--fontify-as
                   (or inline-body
                       (unless (string-empty-p drawer-body) drawer-body)
                       (string-trim
                        (mevedel-view--user-turn-text
                         (list (list 'user data-start data-end)) data-buf)))
                   'markdown-mode))))
        (when (eq vtype 'hook-context)
          (setq text
                (mevedel-view--format-hook-context-block
                 (mevedel-view--hook-context-events-from-text text) t)))
        (when (eq vtype 'hook-audit)
          (setq text
                (if-let* ((record
                           (and header
                                (get-text-property
                                 0 'mevedel-view-hook-audit-record header))))
                    (mevedel-view--format-hook-audit-block record t)
                  (mapconcat
                   (lambda (record)
                     (mevedel-view--format-hook-audit-block record t))
                   (mevedel-view--hook-audit-records-from-text text) ""))))
        (when (eq vtype 'system-reminder-summary)
          (let ((source-text
                 (mevedel-view-disclosure-data-substring
                  data-buf data-start data-end t)))
            (setq text
                  (mevedel-view--fontify-as
                   (or (mevedel-view--inline-skill-reminder-body source-text)
                       (mevedel-view--system-reminder-body-from-text text)
                       text)
                   'markdown-mode))))
        (when (string-empty-p text)
          (setq text "[section no longer available]"))
        (when header
          (insert header))
        (setq body-start (point))
        (insert text)
        (unless (eq (char-before) ?\n)
          (insert "\n"))
        (when (eq vtype 'response)
          (mevedel-view--decorate-agent-result-blocks start (point))
          (mevedel-view--decorate-agent-message-blocks start (point))
          (mevedel-view--decorate-markdown-in-range start (point)))
        (when (memq vtype
                    '(thinking-summary tool-summary prompt-summary
                      system-reminder-summary agent-handle))
          (add-text-properties
           body-start (point) '(line-prefix "    " wrap-prefix "    ")))
        (mevedel-view-render-add-display-properties start (point) vtype)))
    source))

(defun mevedel-view-render-collapsed-disclosure (data-buf source vtype)
  "Return collapsed disclosure data for SOURCE and VTYPE in DATA-BUF.
The result contains normalized `:source', `:summary', and `:face' values."
  (let* ((trimmed
          (and (eq vtype 'thinking-summary)
               (mevedel-view--reasoning-source-bounds
                data-buf (car source) (cdr source))))
         (source (if trimmed
                     (mevedel-view-disclosure-source-range
                      data-buf (car trimmed) (cdr trimmed))
                   source))
         (data-start (car source))
         (data-end (cdr source))
         (rendering
          (if (eq vtype 'tool-group)
              (mevedel-view--tool-group-rendering-from-source
               data-buf data-start data-end)
            (let ((candidate
                   (mevedel-view--segment-rendering
                    data-buf data-start data-end t)))
              (and (eq vtype
                       (or (plist-get candidate :vtype) 'tool-summary))
                   candidate))))
         (summary
          (if rendering
              (mevedel-view--rendering-header-block rendering)
            (pcase vtype
              ('tool-summary
               (mevedel-view--tool-one-liner data-buf data-start data-end))
              ('thinking-summary
               (mevedel-view--thinking-summary data-buf data-start data-end))
              ('response
               (mevedel-view--response-summary
                data-buf data-start data-end))
              ('prompt-summary
               (mevedel-view--operation-line
                "◆" 'mevedel-view-response-marker "Prompt" nil nil
                'mevedel-view-tool-summary))
              ('hook-context
               (propertize "  \u25c7 hook context added"
                           'font-lock-face 'mevedel-view-hook-context))
              ('hook-audit
               (propertize "  \u25c7 hook audit"
                           'font-lock-face 'mevedel-view-hook-audit))
              ('system-reminder-summary
               (mevedel-view--system-reminder-summary
                data-buf data-start data-end)))))
         (face
          (pcase vtype
            ((or 'tool-summary 'tool-group 'agent-handle 'prompt-summary)
             'mevedel-view-tool-summary)
            ('thinking-summary 'mevedel-view-thinking-summary)
            ('response 'mevedel-view-response-summary)
            ('hook-context 'mevedel-view-hook-context)
            ('hook-audit 'mevedel-view-hook-audit)
            ('system-reminder-summary 'mevedel-view-system-reminder))))
    (and summary
         (list :source source
               :summary (mevedel-view--summary-with-face summary face)))))


;;
;;; Turn-level expand/collapse

(defun mevedel-view--user-turn-summary (start end)
  "Build a one-line summary for a user turn between START and END.
Return nil when the body is a single line -- short turns are already
compact enough that folding adds no value."
  (save-excursion
    (goto-char start)
    ;; Skip the "You\n" header line.
    (forward-line 1)
    (let ((body-start (point)))
      (when (< body-start end)
        (let* ((body-end (save-excursion
                           (goto-char end)
                           (skip-chars-backward "\n")
                           (point)))
               (body-lines (max 0 (count-lines body-start body-end))))
          (when (> body-lines 1)
            (let ((first-line
                   (buffer-substring-no-properties
                    body-start
                    (min (save-excursion
                           (goto-char body-start)
                           (line-end-position))
                         body-end))))
              (format "%s... (%d lines)"
                      (mevedel-view-disclosure-truncate-line first-line 80)
                      body-lines))))))))

(defun mevedel-view--assistant-turn-summary (start end)
  "Build a one-line summary for an assistant turn between START and END.
Scans the rendered view for response/tool/thinking sections and
synthesizes a preview with tool counters."
  (let ((tool-count 0)
        (has-thinking nil)
        (reminder-count 0)
        (response-preview nil))
    (save-excursion
      (let ((pos start))
        (while (< pos end)
          (let ((vtype (get-text-property pos 'mevedel-view-type))
                (next (or (next-single-property-change
                           pos 'mevedel-view-type nil end)
                          end)))
            (pcase vtype
              ('thinking-summary (setq has-thinking t))
              ('system-reminder-summary (cl-incf reminder-count))
              ('tool-summary (cl-incf tool-count))
              ('response
               (unless response-preview
                 (let* ((line-end (save-excursion
                                    (goto-char pos)
                                    (line-end-position)))
                        (raw (buffer-substring-no-properties
                              pos (min line-end next end))))
                   (setq response-preview (string-trim raw))))))
            (setq pos next)))))
    (let ((body-lines (max 0 (1- (count-lines start end)))))
      (cond
       ((and response-preview (not (string-empty-p response-preview)))
        (format "Assistant — %s (%d lines%s%s%s)"
                (mevedel-view-disclosure-truncate-line response-preview 80)
                body-lines
                (if has-thinking ", thinking" "")
                (cond ((= tool-count 0) "")
                      ((= tool-count 1) ", 1 tool")
                      (t (format ", %d tools" tool-count)))
                (cond ((= reminder-count 0) "")
                      ((= reminder-count 1) ", 1 reminder")
                      (t (format ", %d reminders" reminder-count)))))
       ((or has-thinking (> tool-count 0) (> reminder-count 0))
        (format "Assistant — [%s%s%s%s%s]"
                (if has-thinking "thinking" "")
                (if (and has-thinking
                         (or (> tool-count 0) (> reminder-count 0)))
                    ", " "")
                (cond ((= tool-count 0) "")
                      ((= tool-count 1) "1 tool")
                      (t (format "%d tools" tool-count)))
                (if (and (> tool-count 0) (> reminder-count 0)) ", " "")
                (cond ((= reminder-count 0) "")
                      ((= reminder-count 1) "1 reminder")
                      (t (format "%d reminders" reminder-count)))))
       (t "Assistant")))))

(defun mevedel-view--directive-turn-summary (start end directive)
  "Build a one-line summary for DIRECTIVE rendered between START and END."
  (let ((tool-count 0)
        (attempt (nth 2 (mevedel-view--directive-metadata-context directive))))
    (let ((pos start))
      (while (< pos end)
        (when (eq (get-text-property pos 'mevedel-view-type) 'tool-summary)
          (cl-incf tool-count))
        (setq pos (or (next-single-property-change
                       pos 'mevedel-view-type nil end)
                      end))))
    (concat
     (format "◆ %s · %s · T%s · %s%s%s"
             (mevedel--truncate-display (or (plist-get directive :directive-id) "?") 8 "…")
             (mevedel-overlay-ui-directive-action-label
              (plist-get directive :action))
             (or (plist-get directive :turn) "?")
             (pcase (plist-get directive :outcome)
               ('success
                (if (eq (plist-get directive :activity-kind) 'attempt)
                    "Implemented"
                  "Discussed"))
               ('error "Failed")
               ('aborted "Aborted")
               (_ "Running"))
             (pcase tool-count
               (0 "")
               (1 " · 1 tool call")
               (_ (format " · %d tool calls" tool-count)))
             (if-let* ((patch (and attempt
                                   (mevedel-directive-attempt-patch attempt)))
                       ((not (string-empty-p patch))))
                 (let ((additions 0)
                       (deletions 0))
                   (dolist (line (split-string patch "\n"))
                     (cond
                      ((and (string-prefix-p "+" line)
                            (not (string-prefix-p "+++" line)))
                       (cl-incf additions))
                      ((and (string-prefix-p "-" line)
                            (not (string-prefix-p "---" line)))
                       (cl-incf deletions))))
                   (format " · +%d −%d" additions deletions))
               ""))
     (propertize " · RET: actions"
                 'face `(:inherit shadow
                         :overline
                         ,(face-attribute 'mevedel-view-user-header
                                          :foreground nil 'default))))))

(defun mevedel-view--directive-collapse-state-key (directive)
  "Return the stable fold-state key for DIRECTIVE metadata."
  (list (plist-get directive :directive-id)
        (plist-get directive :turn)))

(defun mevedel-view--record-directive-collapse-state (directive collapsed)
  "Remember whether DIRECTIVE is COLLAPSED in the current view."
  (unless (hash-table-p mevedel-view--directive-collapse-states)
    (setq mevedel-view--directive-collapse-states
          (make-hash-table :test #'equal)))
  (puthash (mevedel-view--directive-collapse-state-key directive)
           (and collapsed t)
           mevedel-view--directive-collapse-states))

(defun mevedel-view--collapse-turn ()
  "Collapse the turn at point into a one-line summary.
Stashes the original propertized text on the summary so expand can
restore the turn with all inner section state intact.  Signals a
`user-error' when the turn is too short to benefit from folding."
  (let* ((bounds (mevedel-view-disclosure-section-bounds
                  'mevedel-view-turn-id))
         (role (get-text-property (point) 'mevedel-view-turn-role))
         (id (get-text-property (point) 'mevedel-view-turn-id))
         (directive (get-text-property (point) 'mevedel-view-directive)))
    (unless (and bounds role id)
      (user-error "No turn at point"))
    (mevedel-view-stream-in-flight-turn-start-position)
    (let* ((turn-start (car bounds))
           (turn-end (cdr bounds))
           (stash (buffer-substring turn-start turn-end))
           (context (get-text-property turn-start 'mevedel-view-turn-context))
           (source (get-text-property turn-start 'mevedel-view-source))
           (prompt-preview
            (get-text-property turn-start 'mevedel-view-prompt-preview))
           (variant-start
            (text-property-not-all
             turn-start turn-end
             'mevedel-view-variant-fork-point-id nil))
           (variant-button
            (when variant-start
              (buffer-substring
               variant-start
               (or (next-single-property-change
                    variant-start
                    'mevedel-view-variant-fork-point-id nil turn-end)
                   turn-end))))
           (summary (pcase role
                      ('user (mevedel-view--user-turn-summary
                              turn-start turn-end))
                      ('assistant (mevedel-view--assistant-turn-summary
                                   turn-start turn-end))
                      ('directive
                       (mevedel-view--directive-turn-summary
                        turn-start turn-end directive))))
           (face (pcase role
                   ('user 'mevedel-view-user-header)
                   ('assistant 'mevedel-view-assistant-header)
                   ('directive 'mevedel-view-user-header))))
      (unless summary
        (user-error "Turn is already compact"))
      (when directive
        (mevedel-view--record-directive-collapse-state directive t))
      (let ((inhibit-read-only t))
        (save-excursion
          (goto-char turn-start)
          ;; Let the input marker ride forward across our delete+insert
          ;; so it keeps spanning the rendered content afterwards.
          (set-marker-insertion-type mevedel-view--input-marker t)
          (unwind-protect
              (progn
                (delete-region turn-start turn-end)
                (let ((summary-start (point)))
                  (insert (propertize (concat summary
                                              (when variant-button
                                                (concat "  " variant-button))
                                              "\n\n")
                                      'font-lock-face face
                                      'mevedel-view-type 'turn-summary
                                      'mevedel-view-source source
                                      'mevedel-view-turn-role role
                                      'mevedel-view-prompt-preview prompt-preview
                                      'mevedel-view-turn-id id
                                      'mevedel-view-turn-context context
                                      'mevedel-view-directive directive
                                      'mevedel-view-collapsed t
                                      'mevedel-view-stash stash
                                      'read-only t
                                      'keymap mevedel-view--display-map
                                      'front-sticky '(read-only keymap)
                                      'rear-nonsticky '(read-only keymap)))
                  ;; The trailing blank line is not a visible prompt header.
                  (when prompt-preview
                    (remove-text-properties
                     (- (point) 2) (point)
                     '(mevedel-view-prompt-preview nil)))
                  (when directive
                    (add-text-properties
                     summary-start
                     (save-excursion
                       (goto-char summary-start)
                       (1+ (line-end-position)))
                     (list 'mevedel-view-zone-activate
                           (lambda ()
                             (mevedel-view-directive-actions directive))
                           'mouse-face 'highlight
                           'help-echo "RET: directive actions")))))
            (set-marker-insertion-type mevedel-view--input-marker nil)))))))

(defun mevedel-view--expand-turn ()
  "Restore a collapsed turn at point from its stashed content."
  (let* ((bounds (mevedel-view-disclosure-section-bounds
                  'mevedel-view-turn-id))
         (stash (get-text-property (point) 'mevedel-view-stash))
         (directive (get-text-property (point) 'mevedel-view-directive)))
    (unless (and bounds stash)
      (user-error "No collapsed turn at point"))
    (mevedel-view-stream-in-flight-turn-start-position)
    (when directive
      (mevedel-view--record-directive-collapse-state directive nil))
    (let ((inhibit-read-only t))
      (save-excursion
        (goto-char (car bounds))
        (set-marker-insertion-type mevedel-view--input-marker t)
        (unwind-protect
            (progn
              (delete-region (car bounds) (cdr bounds))
              (insert stash))
          (set-marker-insertion-type mevedel-view--input-marker nil))))))

(defun mevedel-view-render-toggle-turn (collapsed)
  "Expand the current turn when COLLAPSED, otherwise collapse it."
  (mevedel-view-stream-in-flight-turn-start-position)
  (if collapsed
      (mevedel-view--expand-turn)
    (mevedel-view--collapse-turn)))

(defun mevedel-view--collapse-settled-directive-turns (&optional collapse-newest)
  "Collapse settled directive turns except the newest turn by default.
When COLLAPSE-NEWEST is non-nil, collapse that turn too."
  (unless (mevedel-view-stream-in-flight-turn-start-position)
    (let ((pos (point-min))
          (limit (mevedel-view--input-marker-position)))
      (while (< pos limit)
        (let* ((directive
                (get-text-property pos 'mevedel-view-directive))
               (next (or (next-single-property-change
                          pos 'mevedel-view-turn-id nil limit)
                         limit))
               (collapse-state
                (and directive
                     (gethash
                      (mevedel-view--directive-collapse-state-key directive)
                      mevedel-view--directive-collapse-states
                      mevedel-view--missing-directive-collapse-state)))
               (later-turn-p
                (and (< next limit)
                     (text-property-not-all
                      next limit 'mevedel-view-turn-id nil))))
          (when (and directive
                     (plist-get directive :outcome)
                     (not (eq collapse-state nil))
                     (or collapse-newest later-turn-p
                         (eq collapse-state t))
                     (not (get-text-property pos 'mevedel-view-collapsed)))
            (save-excursion
              (goto-char pos)
              (mevedel-view--collapse-turn))
            (setq limit (mevedel-view--input-marker-position)
                  next (or (next-single-property-change
                            pos 'mevedel-view-turn-id nil limit)
                           limit)))
          (setq pos next))))))

(defun mevedel-view--settled-response-at-point ()
  "Return the stable settled response target at point."
  (let* ((bounds (mevedel-view-disclosure-section-bounds
                  'mevedel-view-turn-id))
         (role (and bounds
                    (get-text-property
                     (car bounds) 'mevedel-view-turn-role)))
         (source-pos
          (and bounds
               (if (eq role 'directive)
                   (text-property-any
                    (car bounds) (cdr bounds) 'mevedel-view-type 'response)
                 (car bounds))))
         (source (and source-pos
                      (get-text-property source-pos 'mevedel-view-source)))
         (data-buffer (mevedel-view-segments-display-buffer)))
    (unless (and (memq role '(assistant directive))
                 (consp source)
                 (buffer-live-p data-buffer))
      (user-error "Point is not on an assistant response"))
    (or (mevedel-session-artifacts-fork-point-at-source
         data-buffer (car source) (cdr source))
        (user-error "Assistant response is not a settled fork point"))))

(defun mevedel-view-fork-point-at-point ()
  "Return the stable assistant fork point at point.

Signal a user error for directive turns because they have no conversational
continuation context."
  (when (get-text-property (point) 'mevedel-view-directive)
    (user-error "Directive turns cannot be forked"))
  (mevedel-view--settled-response-at-point))

(defun mevedel-view-goto-conversation-variant (fork-point-id)
  "Move point to the assistant header for FORK-POINT-ID."
  (let ((limit (or (mevedel-view--input-marker-position) (point-max)))
        (pos (point-min))
        found)
    (while (and (< pos limit) (not found))
      (if (equal fork-point-id
                 (get-text-property
                  pos 'mevedel-view-variant-fork-point-id))
          (setq found pos)
        (setq pos
              (or (next-single-property-change
                   pos 'mevedel-view-variant-fork-point-id nil limit)
                  limit))))
    (unless found
      (user-error "Conversation fork point is no longer available"))
    (goto-char found)
    (when-let* ((bounds (mevedel-view-disclosure-section-bounds
                        'mevedel-view-turn-id)))
      (goto-char (car bounds)))))

(defun mevedel-view-switch-conversation-variant (fork-point-id)
  "Open the sole related session at FORK-POINT-ID."
  (let* ((session
          (and (buffer-live-p mevedel--data-buffer)
               (buffer-local-value 'mevedel--session
                                   mevedel--data-buffer)))
         (_ (unless session
              (user-error "Active view has no mevedel session")))
         (session-id (mevedel-session-session-id session))
         (variants
          (mevedel-session-persistence-conversation-variants
           session fork-point-id))
         (alternatives
          (cl-remove
           session-id variants
           :test #'equal
           :key (lambda (entry)
                  (plist-get (plist-get entry :summary) :session-id))))
         (target
          (pcase (length alternatives)
            (0 (user-error "No related conversation variant survives"))
            (1 (car alternatives))
            (_
             (mevedel-session-persistence-choose-conversation-variant
              variants session-id))))
         (target-data
          (mevedel-session-persistence-restore
           (plist-get target :save-path)))
         (target-view
          (buffer-local-value 'mevedel--view-buffer target-data)))
    (unless (buffer-live-p target-view)
      (error "Conversation variant has no live view"))
    (with-current-buffer target-view
      (let ((target-session
             (and (buffer-live-p mevedel--data-buffer)
                  (buffer-local-value 'mevedel--session
                                      mevedel--data-buffer))))
        (when-let* ((segment
                     (cl-loop
                      for (number . prompts)
                      in (mevedel-session-prompt-index target-session)
                      when (cl-find
                            fork-point-id prompts
                            :test #'equal
                            :key (lambda (prompt)
                                   (plist-get prompt :fork-point-id)))
                      return number))
                    ((/= segment
                         (or (mevedel-session-current-segment target-session)
                             1))))
          (mevedel-view-go-to-segment segment)))
      (mevedel-view-goto-conversation-variant fork-point-id))
    (display-buffer target-view gptel-display-buffer-action)
    target-data))

(defun mevedel-view-switch-conversation-variant-at-point ()
  "Switch variants at the settled assistant turn at point."
  (interactive)
  (mevedel-view-switch-conversation-variant
   (plist-get (mevedel-view-fork-point-at-point) :fork-point-id)))

(defun mevedel-view-rewind-at-point ()
  "Rewind the session keeping the settled assistant turn at point.

The turn under point is the last one kept: everything after it is
discarded.  Point names a response you are looking at, so the boundary
falls after it; `mevedel-rewind' instead picks one of your prompts and
returns to just before it."
  (interactive)
  (when (mevedel-session-rewind-rewind
         mevedel--data-buffer (mevedel-view--settled-response-at-point)
         'after)
    (mevedel-view-return-to-latest-segment)
    t))

(defun mevedel-view--rendered-turn-starts ()
  "Return rendered turn starts before the input zone."
  (let ((pos (point-min))
        (limit (mevedel-view--input-marker-position))
        starts)
    (while (< pos limit)
      (let ((id (get-text-property pos 'mevedel-view-turn-id)))
        (when id
          (push pos starts))
        (setq pos
              (or (next-single-property-change
                   pos 'mevedel-view-turn-id nil limit)
                  limit))))
    (nreverse starts)))

(defun mevedel-view-render-capture-segment-state ()
  "Capture point, window, and disclosure state for the displayed segment.
Point and the window start are kept as semantic render anchors as well:
the segment is projected again on return, and deferred formatting such as
table layout makes that projection differ in length from this one."
  (let* ((input-start (mevedel-view--input-marker-position))
         (turn-starts (mevedel-view--rendered-turn-starts))
         (window (get-buffer-window (current-buffer) t)))
    (list
     :point (point)
     :point-anchor (mevedel-view--position-render-anchor (point))
     :input-offset
     (and input-start
          (>= (point) input-start)
          (- (point) (mevedel-view--input-start)))
     :window-start (and window (window-start window))
     :window-start-anchor
     (and window (mevedel-view--position-render-anchor (window-start window)))
     :collapsed-turns
     (cl-loop
      for pos in turn-starts
      for index from 1
      when (get-text-property pos 'mevedel-view-collapsed)
      collect index)
     :collapse-states
     (and input-start
          (mevedel-view-disclosure-capture-state
           (point-min) input-start)))))

(defun mevedel-view-render--restore-segment-state (state direction)
  "Restore segment view STATE, using DIRECTION for a first visit."
  (when-let* ((collapse-states (plist-get state :collapse-states)))
    (mevedel-view-disclosure-restore-state
     (point-min) (mevedel-view--input-marker-position) collapse-states))
  (let ((turn-starts (mevedel-view--rendered-turn-starts)))
    (dolist (pos
             (sort
              (delq
               nil
               (mapcar
                (lambda (index)
                  (nth (1- index) turn-starts))
                (plist-get state :collapsed-turns)))
              #'>))
      (goto-char pos)
      (mevedel-view--collapse-turn)))
  (cond
   ((plist-get state :input-offset)
    (goto-char
     (min (point-max)
          (+ (mevedel-view--input-start)
             (plist-get state :input-offset)))))
   ((plist-get state :point)
    (goto-char
     (max (point-min)
          (min (or (mevedel-view--render-anchor-position
                    (plist-get state :point-anchor))
                   (plist-get state :point))
               (mevedel-view--input-marker-position)))))
   ((eq direction 'backward)
    (let ((pos (point-min))
          (limit (mevedel-view--input-marker-position))
          target)
      (while (< pos limit)
        (when (get-text-property pos 'mevedel-view-turn-role)
          (setq target pos))
        (setq pos
              (or (next-single-property-change
                   pos 'mevedel-view-turn-role nil limit)
                  limit)))
      (when target
        (goto-char target))))
   (t
    (goto-char (point-min))
    (mevedel-view-next-turn)))
  (when-let* ((window (get-buffer-window (current-buffer) t))
              (start (or (mevedel-view--render-anchor-position
                          (plist-get state :window-start-anchor))
                         (plist-get state :window-start))))
    (set-window-start window (min start (point-max)) t)))

(defun mevedel-view-render-project-segment (data-buffer state direction)
  "Render DATA-BUFFER and restore projection STATE for DIRECTION.
The segment owner holds projection ownership across source replacement."
  (mevedel-view-render--full-now data-buffer t)
  (mevedel-view-render--restore-segment-state state direction))

;;
;;; Navigation

(defun mevedel-view--display-navigation-limit ()
  "Return the upper bound for display/chrome navigation."
  (or (mevedel-view--input-marker-position) (point-max)))

(defun mevedel-view--next-fragment-position (limit)
  "Return the next navigatable fragment position before LIMIT."
  (save-excursion
    (and (< (point) limit)
         (mevedel-view-zone-next limit))))

(defun mevedel-view--next-turn-position (limit)
  "Return the next rendered turn position before LIMIT, or nil."
  (let ((origin (point)))
    (save-excursion
      (mevedel-view-next-turn)
      (let ((pos (point)))
        (and (> pos origin)
             (< pos limit)
             (get-text-property pos 'mevedel-view-source)
             pos)))))

(defun mevedel-view--previous-fragment-position ()
  "Return the previous navigatable fragment position."
  (save-excursion
    (mevedel-view-zone-previous (point-min))))

(defun mevedel-view--previous-turn-position ()
  "Return the previous rendered turn position, or nil."
  (let ((origin (point)))
    (save-excursion
      (mevedel-view-prev-turn)
      (let ((pos (point)))
        (and (< pos origin)
             (get-text-property pos 'mevedel-view-source)
             pos)))))

(defun mevedel-view-next-display ()
  "Move point to the next navigatable fragment or rendered turn."
  (interactive)
  (let* ((limit (mevedel-view--display-navigation-limit)))
    (when (> (point) limit)
      (goto-char limit))
    (let* ((fragment-pos (mevedel-view--next-fragment-position limit))
           (turn-pos (mevedel-view--next-turn-position limit))
           (target (car (sort (delq nil (list fragment-pos turn-pos)) #'<))))
      (when target
        (goto-char target)))))

(defun mevedel-view-previous-display ()
  "Move point to the previous navigatable fragment or rendered turn."
  (interactive)
  (let ((limit (mevedel-view--display-navigation-limit)))
    (when (> (point) limit)
      (goto-char limit))
    (let* ((fragment-pos (mevedel-view--previous-fragment-position))
           (turn-pos (mevedel-view--previous-turn-position))
           (target (car (sort (delq nil (list fragment-pos turn-pos)) #'>))))
      (when target
        (goto-char target)))))

(defun mevedel-view--user-query-positions (limit)
  "Return visible user query header positions before LIMIT."
  (let ((pos (point-min))
        positions)
    (while (< pos limit)
      (when (eq (get-text-property pos 'mevedel-view-turn-role) 'user)
        (push pos positions))
      (setq pos
            (or (next-single-property-change
                 pos 'mevedel-view-turn-role nil limit)
                limit)))
    (nreverse positions)))

(defun mevedel-view-next-user-query ()
  "Move point to the next user query header."
  (interactive)
  (let* ((limit (mevedel-view--display-navigation-limit))
         (origin (min (point) limit))
         (target (cl-find-if
                  (lambda (pos) (> pos origin))
                  (mevedel-view--user-query-positions limit))))
    (when target
      (goto-char target))))

(defun mevedel-view-previous-user-query ()
  "Move point to the previous user query header."
  (interactive)
  (let* ((limit (mevedel-view--display-navigation-limit))
         (origin (min (point) limit))
         target)
    (dolist (pos (mevedel-view--user-query-positions limit))
      (when (< pos origin)
        (setq target pos)))
    (when target
      (goto-char target))))

(defun mevedel-view-next-turn ()
  "Move point to the next turn header."
  (interactive)
  (let ((pos (point))
        (cur-source (get-text-property (point) 'mevedel-view-source)))
    ;; Move past the current turn (skip all positions with the same source)
    (when cur-source
      (while (and (< pos mevedel-view--input-marker)
                  (equal (get-text-property pos 'mevedel-view-source)
                         cur-source))
        (setq pos (or (next-single-property-change pos 'mevedel-view-source)
                       mevedel-view--input-marker))))
    ;; Find next position that has a source (skip separators)
    (while (and (< pos mevedel-view--input-marker)
                (not (get-text-property pos 'mevedel-view-source)))
      (setq pos (or (next-single-property-change pos 'mevedel-view-source)
                     mevedel-view--input-marker)))
    (if (< pos mevedel-view--input-marker)
        (goto-char pos)
      ;; No more turns; go to the input zone.
      (goto-char mevedel-view--input-marker))))

(defun mevedel-view-prev-turn ()
  "Move point to the previous turn header."
  (interactive)
  (let ((pos (point))
        (cur-source (get-text-property (point) 'mevedel-view-source)))
    ;; Move before the current turn (skip all positions with the same source)
    (when cur-source
      (while (and (> pos (point-min))
                  (equal (get-text-property pos 'mevedel-view-source)
                         cur-source))
        (setq pos (or (previous-single-property-change pos 'mevedel-view-source)
                       (point-min)))))
    ;; Skip separator regions (no source)
    (while (and (> pos (point-min))
                (not (get-text-property pos 'mevedel-view-source)))
      (setq pos (or (previous-single-property-change pos 'mevedel-view-source)
                     (point-min))))
    ;; Now pos is inside the previous turn -- find its start
    (let ((target-source (get-text-property pos 'mevedel-view-source)))
      (when target-source
        (while (and (> pos (point-min))
                    (equal (get-text-property (1- pos) 'mevedel-view-source)
                           target-source))
          (setq pos (or (previous-single-property-change pos 'mevedel-view-source)
                         (point-min))))))
    (goto-char pos)))

(defun mevedel-view-toggle-transcript ()
  "Toggle between the view buffer and the raw data buffer."
  (interactive)
  (if-let* ((data-buffer (mevedel-view-segments-display-buffer)))
      (switch-to-buffer data-buffer)
    (user-error "No data buffer associated with this view")))


;;
;;; Full re-render
(defun mevedel-view--live-tail-rendered-position
    (live-tail limit &optional prefix-start prefix-end)
  "Return the position where LIVE-TAIL already appears before LIMIT.

Full rerenders may preserve an in-flight view tail when the data buffer
has not yet received a replacement assistant turn.  Some refresh paths
can lose the data-turn anchor while the same assistant text is already
renderable from the data buffer; in that case appending the preserved
tail would duplicate the visible transcript.  PREFIX-START restricts
stable-prefix matching to the latest rendered assistant turn, which
ends at PREFIX-END."
  (let* ((tail (string-trim
                (substring-no-properties (or live-tail ""))))
         (lines (and (not (string-empty-p tail))
                     (split-string tail "\n[ \t\n]*" t "[ \t]+")))
         (stable-lines
          (cl-loop for line in lines
                   unless (mevedel-view--volatile-live-tail-line-p line)
                   collect line))
         (prefix-lines (cl-subseq stable-lines
                                  0 (min 2 (length stable-lines)))))
    (when lines
      (or (mevedel-view--live-tail-lines-rendered-position lines limit)
          (when (and prefix-start prefix-end (cdr prefix-lines))
            (mevedel-view--live-tail-lines-rendered-position
             prefix-lines prefix-end prefix-start))))))

(defun mevedel-view--volatile-live-tail-line-p (line)
  "Return non-nil when LINE is too volatile for live-tail matching.
In-flight tool decorations never reach this check: they are stripped
before the tail is matched."
  (let ((trimmed (string-trim (or line ""))))
    (or (string-empty-p trimmed)
        (string-match-p "\\`[[:space:]]*… Thinking\\.\\.\\." trimmed)
        (string-match-p "\\`[[:space:]]*[✓✗●!›…]?[[:space:]]*Agent:"
                        trimmed))))

(defun mevedel-view--live-tail-lines-rendered-position
    (lines limit &optional start)
  "Return position where literal LINES appear from START to LIMIT.
Blank gaps and indentation between lines are allowed."
  (when lines
    (cl-labels
        ((skip-gap ()
           (let (saw-newline)
             (while (and (< (point) limit)
                         (memq (char-after) '(?\s ?\t ?\n)))
               (when (eq (char-after) ?\n)
                 (setq saw-newline t))
               (forward-char 1))
             saw-newline))
         (looking-at-line-p (line)
           (let ((end (+ (point) (length line))))
             (and (<= end limit)
                  (equal line (buffer-substring-no-properties
                               (point) end))
                  (or (= end limit)
                      (eq (char-after end) ?\n))))))
      (let ((first (car lines))
            (rest (cdr lines)))
        (save-excursion
          (goto-char (or start (point-min)))
          (unless (bolp)
            (forward-line 1))
          (catch 'found
            (while (< (point) limit)
              (skip-chars-forward " \t" limit)
              (let ((candidate (point)))
                (save-excursion
                  (when (looking-at-line-p first)
                    (goto-char (+ (point) (length first)))
                    (catch 'mismatch
                      (dolist (line rest)
                        (unless (and (skip-gap)
                                     (looking-at-line-p line))
                          (throw 'mismatch nil))
                        (goto-char (+ (point) (length line))))
                      (throw 'found candidate)))))
              (forward-line 1))))))))

(defun mevedel-view--insert-compaction-indicator
    (view-buf &optional hook-audits source)
  "Insert a compacted-conversation indicator into VIEW-BUF.
HOOK-AUDITS are optional audit records attached to the skipped summary.
SOURCE is the source range of the skipped summary in the data buffer."
  (when (buffer-live-p view-buf)
    (with-current-buffer view-buf
      (save-excursion
        (goto-char mevedel-view--input-marker)
        (set-marker-insertion-type mevedel-view--input-marker t)
        (unwind-protect
            (progn
              (insert
               (propertize "--- conversation compacted ---\n"
                           'read-only t
                           'keymap mevedel-view--display-map
                           'front-sticky '(read-only keymap)
                           'rear-nonsticky '(read-only keymap)
                           'font-lock-face
                           'mevedel-view-separator))
              (when hook-audits
                (let ((audit-start (point)))
                  (dolist (audit hook-audits)
                    (mevedel-view--insert-hook-audit-block audit source))
                  (mevedel-view-render-add-display-properties
                   audit-start (point) 'hook-audit))))
          (set-marker-insertion-type mevedel-view--input-marker nil))))))

(defun mevedel-view--summary-hook-audits (data-buf start end)
  "Return hook audit records stored in DATA-BUF summary START..END."
  (when (and data-buf (buffer-live-p data-buf) (< start end))
    (with-current-buffer data-buf
      (mevedel-view--hook-audit-records-from-text
       (buffer-substring start end)))))

(defun mevedel-view--full-rerender-reset
    (data-buf session-data-buf historical-p agent-transcript-p)
  "Reset the current view before projecting DATA-BUF.
SESSION-DATA-BUF supplies the live header.  HISTORICAL-P inserts the
historical banner.  AGENT-TRANSCRIPT-P selects the headerless layout."
  (unless agent-transcript-p
    (mevedel-view--refresh-continuation-prompt data-buf historical-p))
  (mevedel-view-render-invalidate-live-tail)
  (mevedel-view--forget-tool-row-starts)
  (if agent-transcript-p
      (progn
        (mevedel-view--debug-log
         'full-rerender-delete-transcript
         :region (mevedel-view--debug-region (point-min) (point-max))
         :state (mevedel-view--debug-state data-buf))
        (delete-region (point-min) (point-max))
        (goto-char (point-min)))
    (mevedel-view--debug-log
     'full-rerender-delete-display
     :region (mevedel-view--debug-region
              (point-min)
              (marker-position mevedel-view--input-marker))
     :state (mevedel-view--debug-state data-buf))
    (delete-region (point-min) mevedel-view--input-marker)
    (mevedel-view--forget-request-progress-region)
    (goto-char (point-min))
    (insert (mevedel-view--header-string session-data-buf))
    (if historical-p
        (insert (mevedel-view-segments-banner))
      (when-let* ((line (mevedel-view-segments-chats-line)))
        (insert line))))
  (set-marker mevedel-view--input-marker (point))
  (when (markerp mevedel-view--status-marker)
    (set-marker mevedel-view--status-marker (point)))
  (when (markerp mevedel-view--interaction-marker)
    (set-marker mevedel-view--interaction-marker (point)))
  (mevedel-view--debug-log
   'full-rerender-after-header
   :state (mevedel-view--debug-state data-buf)))

(defun mevedel-view-render--plan-bounds (data-buf view-buf)
  "Return DATA-BUF's source bounds and summary for VIEW-BUF.
Call after restoring properties.  This does not mutate the view."
  (with-current-buffer data-buf
    (let ((scan-start
           (mevedel-transcript--skip-leading-properties-drawer (point-min)))
          summary-source)
      (when-let* ((transcript-start
                   (buffer-local-value 'mevedel-view--transcript-start view-buf))
                  ((markerp transcript-start))
                  ((eq (marker-buffer transcript-start) data-buf)))
        (setq scan-start (max scan-start (marker-position transcript-start))))
      (when (eq (get-text-property scan-start 'face) 'shadow)
        (setq scan-start
              (or (next-single-property-change scan-start 'face nil (point-max))
                  (point-max)))
        (let ((summary-start scan-start))
          (save-excursion
            (goto-char scan-start)
            (when (re-search-forward "^#\\+end_summary\n\\|^```\n" nil t)
              (setq scan-start (point))))
          (setq summary-source (cons summary-start scan-start))))
      (let ((after-summary
             (mevedel-transcript--skip-leading-summary-block scan-start)))
        (when (> after-summary scan-start)
          (unless summary-source
            (setq summary-source (cons scan-start after-summary)))
          (setq scan-start after-summary)))
      (let ((summary-audits
             (and summary-source
                  (mevedel-view--summary-hook-audits
                   data-buf (car summary-source) (cdr summary-source)))))
        (list :start scan-start :end (point-max)
              :summary-source summary-source :summary-audits summary-audits)))))

(defun mevedel-view-render--plan-finish
    (plan turns data-buf session-data-buf agent-transcript-p)
  "Add TURNS and variant metadata to PLAN without changing the view."
  (let* ((session (and (not agent-transcript-p)
                       (buffer-local-value 'mevedel--session session-data-buf)))
         (variants
          (when (and session
                     (mevedel-session-save-path session)
                     (mevedel-session-workspace session)
                     (with-current-buffer data-buf
                       (mevedel-session-artifacts-fork-point-spans data-buf)))
            ;; Settled buttons use the last live listing; activation enumerates
            ;; live instead of requiring a target round trip per rendered turn.
            (mevedel-session-persistence-list-sessions
             (mevedel-session-workspace session) 'cached))))
    (append plan (list :turns turns :session session :variants variants))))

(defun mevedel-view--full-rerender-plan
    (data-buf session-data-buf view-buf agent-transcript-p)
  "Prepare DATA-BUF's canonical turns without changing VIEW-BUF.
SESSION-DATA-BUF supplies live session metadata.  AGENT-TRANSCRIPT-P selects
headerless agent history.  Return source limits, turns, summary and variants."
  (with-current-buffer data-buf
    (unless (mevedel-view--running-agent-transcript-buffer-p)
      (mevedel-transcript-restore-properties t))
    (let ((plan (mevedel-view-render--plan-bounds data-buf view-buf)))
      (save-restriction
        (narrow-to-region (plist-get plan :start) (plist-get plan :end))
        (mevedel-view-render--plan-finish
         plan (mevedel-view--group-transcript-turns
               (mevedel-transcript-segments (point-min) (point-max)) data-buf)
         data-buf session-data-buf agent-transcript-p)))))

(defun mevedel-view-render--plan-fontify-step (job)
  "Prepare one response's Markdown in JOB without projecting a partial turn.
Non-response segments are skipped in groups.  The result remains job-local
until every response has finished and the source is revalidated."
  (if-let* ((fontify (plist-get job :fontify)))
      (when (mevedel-view--markdown-fontify-job-step fontify 2048)
        (puthash (plist-get job :fontify-key)
                 (mevedel-view--markdown-fontify-job-result fontify)
                 (plist-get job :fontified))
        (mevedel-view--markdown-fontify-job-cancel fontify)
        (setf (plist-get job :fontify) nil
              (plist-get job :fontify-key) nil))
    (let ((turns (plist-get job :fontify-turns))
          (segments (plist-get job :fontify-segments))
          (count 0))
      (while (and (< count 24) (not (plist-get job :fontify))
                  (or segments turns))
        (unless segments
          (setq segments (plist-get (car turns) :segments)
                turns (cdr turns)))
        (when-let* ((segment (pop segments))
                    ((eq (car segment) 'response))
                    ((and mevedel-view-fontify-responses
                          (mevedel-view--markdown-fontify-mode)))
                    (text (with-current-buffer (plist-get job :data)
                            (mevedel-view--visible-response-text
                             (string-trim (buffer-substring-no-properties
                                           (cadr segment) (caddr segment))))))
                    ((not (string-empty-p text))))
          (let ((key (list :response mevedel-view-fontify-responses
                           (mevedel-view--markdown-fontify-mode)
                           (mevedel-view--render-cache-key text))))
            (unless (or (gethash key (plist-get job :fontified))
                        (and (hash-table-p mevedel-view--response-fontify-cache)
                             (gethash key mevedel-view--response-fontify-cache)))
              (setf (plist-get job :fontify-key) key
                    (plist-get job :fontify)
                    (mevedel-view--markdown-fontify-job-start text)))))
        (setq count (1+ count)))
      (setf (plist-get job :fontify-turns) turns
            (plist-get job :fontify-segments) segments)
      (unless (or turns segments (plist-get job :fontify))
        (setf (plist-get job :tools-turns) (plist-get job :turns)
              ;; Preparation reads remembered fold choices, not reader/composer
              ;; anchors.  Any intervening view mutation invalidates its entries;
              ;; publication still captures the reader's current state below.
              (plist-get job :tool-state-tick) (buffer-modified-tick)
              (plist-get job :prepare-stage) 'tools)))))

(defun mevedel-view-render--plan-tool-step (job)
  "Prepare a few settled tool entries in JOB without mutating the view.
The same cache is used when projecting the canonical run.  The source and
the reader's disclosure state are checked again before publication."
  (let ((turns (plist-get job :tools-turns))
        (segments (plist-get job :tools-segments))
        (data (plist-get job :data))
        (plan (plist-get job :plan))
        (count 0)
        (mevedel-view--activity-entry-cache (plist-get job :tool-cache)))
    (while (and (< count 4) (or segments turns))
      (unless segments
        (setq segments (plist-get (car turns) :segments)
              turns (cdr turns)))
      (when-let* ((segment (pop segments))
                  ((eq (car segment) 'tool)))
        (with-current-buffer data
          (save-restriction
            (narrow-to-region (plist-get plan :start) (plist-get plan :end))
            (with-current-buffer (plist-get job :view)
              (mevedel-view--tool-segment-entry segment data)))))
      (setq count (1+ count)))
    (setf (plist-get job :tools-turns) turns
          (plist-get job :tools-segments) segments)
    (unless (or turns segments)
      (setf (plist-get job :prepare-stage) 'finish))))

(defun mevedel-view-render--plan-step (job)
  "Advance JOB's canonical source preparation by one bounded stage.
Return non-nil when the complete plan is ready.  Until then the old view
remains intact; no unclassified placeholder can acquire a wrong prompt."
  (let ((data (plist-get job :data))
        (view (current-buffer))
        (stage (plist-get job :prepare-stage)))
    (pcase stage
      ('restore
       (with-current-buffer data
         (unless (mevedel-view--running-agent-transcript-buffer-p)
           (mevedel-transcript-restore-properties t))
         (setf (plist-get job :tick) (buffer-modified-tick)))
       (setf (plist-get job :prepare-stage) 'bounds))
      ('bounds
       (let ((plan (mevedel-view-render--plan-bounds data view)))
         (setf (plist-get job :plan) plan
               (plist-get job :prepare-stage) 'scan
               (plist-get job :scan)
               (with-current-buffer data
                 (save-restriction
                   (narrow-to-region (plist-get plan :start) (plist-get plan :end))
                   (mevedel-transcript-scan-start (point-min) (point-max)))))))
      ('scan
       (when (with-current-buffer data
               (save-restriction
                 (narrow-to-region (plist-get (plist-get job :plan) :start)
                                   (plist-get (plist-get job :plan) :end))
                 (mevedel-transcript-scan-step (plist-get job :scan))))
         (setf (plist-get job :remaining)
               (mevedel-transcript-scan-result (plist-get job :scan))
               (plist-get job :scan) nil
               (plist-get job :prepare-stage) 'directive)))
      ('directive
       (setf (plist-get job :ranges) (mevedel-view--directive-ranges data)
             (plist-get job :prepare-stage) 'filter))
      ('filter
       (let ((rest (plist-get job :remaining))
             (filtered (plist-get job :filtered-rev))
             (count 0))
         (while (and rest (< count 24))
           (unless (mevedel-view--directive-boundary-segment-p (car rest) data)
             (push (car rest) filtered))
           (setq rest (cdr rest) count (1+ count)))
         (setf (plist-get job :remaining) rest
               (plist-get job :filtered-rev) filtered)
         (unless rest
           (setf (plist-get job :remaining) (nreverse filtered)
                 (plist-get job :filtered-rev) nil
                 (plist-get job :prepare-stage) 'group))))
      ('group
       (let* ((rest (plist-get job :remaining))
              (chunk (cl-loop for seg in rest repeat 24 while seg collect seg))
              (remaining (nthcdr (length chunk) rest))
              (result (mevedel-view--group-into-turns
                       chunk data (plist-get job :group-state) (car remaining))))
         (setf (plist-get job :remaining) remaining)
         (if remaining
             (setf (plist-get job :group-state) result)
           (setf (plist-get job :turns) result
                 (plist-get job :annotate-rest) result
                 (plist-get job :group-state) nil
                 (plist-get job :prepare-stage) 'annotate))))
      ('annotate
       (let ((rest (plist-get job :annotate-rest))
             (count 0))
         (while (and rest (< count 16))
           (mevedel-view-render--annotate-directive-turn
            (car rest) (plist-get job :ranges))
           (setq rest (cdr rest) count (1+ count)))
         (setf (plist-get job :annotate-rest) rest)
         (unless rest
           (setf (plist-get job :fontify-turns) (plist-get job :turns)
                 (plist-get job :prepare-stage) 'fontify))))
      ('fontify
       (mevedel-view-render--plan-fontify-step job))
      ('tools
       (mevedel-view-render--plan-tool-step job))
      ('finish
       (setf (plist-get job :plan)
             (mevedel-view-render--plan-finish
              (plist-get job :plan) (plist-get job :turns) data
              (plist-get job :live-data) (plist-get job :agent))
             (plist-get job :turns) nil
             (plist-get job :fontify-turns) nil
             (plist-get job :fontify-segments) nil
             (plist-get job :tools-turns) nil
             (plist-get job :tools-segments) nil
             (plist-get job :ranges) nil
             (plist-get job :remaining) nil
             (plist-get job :prepare-stage) 'done)))
    (eq (plist-get job :prepare-stage) 'done)))

(defun mevedel-view--full-rerender-project
    (data-buf session-data-buf render-view-buf
              agent-transcript-p data-turn-start-pos saved-states)
  "Project DATA-BUF into its view and return the last-turn rendering state.
SESSION-DATA-BUF supplies live session metadata.  RENDER-VIEW-BUF is
used for agent transcripts.  DATA-TURN-START-POS identifies the live
turn.  SAVED-STATES restores matching disclosure state."
  (let* ((view-buf (if agent-transcript-p render-view-buf
                     (buffer-local-value 'mevedel--view-buffer data-buf)))
         (plan (mevedel-view--full-rerender-plan
                data-buf session-data-buf view-buf agent-transcript-p))
         (mevedel-view--conversation-variant-sessions (plist-get plan :variants))
         last-assistant-turn-start last-assistant-turn-end
         last-assistant-turn-data-start last-current-assistant-turn-start
         last-current-assistant-turn-data-start last-turn-role)
    (when-let* ((summary-source (plist-get plan :summary-source)))
      (mevedel-view--insert-compaction-indicator
       view-buf (plist-get plan :summary-audits) summary-source))
    (with-current-buffer data-buf
      (save-restriction
        (narrow-to-region (plist-get plan :start) (plist-get plan :end))
        (with-current-buffer view-buf
          (dolist (turn (plist-get plan :turns))
            (setq last-turn-role (plist-get turn :role))
            (when (eq last-turn-role 'assistant)
              (let ((view-turn-start (copy-marker mevedel-view--input-marker nil)))
                (setq last-assistant-turn-start view-turn-start
                      last-assistant-turn-data-start (plist-get turn :start))
                (when (and data-turn-start-pos (plist-get turn :end)
                           (> (plist-get turn :end) data-turn-start-pos))
                  (setq last-current-assistant-turn-start view-turn-start
                        last-current-assistant-turn-data-start (plist-get turn :start)))))
            (mevedel-view--render-turn turn data-buf t (plist-get plan :session))
            (when (eq last-turn-role 'assistant)
              (setq last-assistant-turn-end (copy-marker mevedel-view--input-marker nil))))
          (mevedel-view--collapse-settled-directive-turns)
          (when saved-states
            (mevedel-view-disclosure-restore-state
             (point-min) (marker-position mevedel-view--input-marker) saved-states)))))
    (list :view-buffer view-buf
          :last-assistant-turn-start last-assistant-turn-start
          :last-assistant-turn-end last-assistant-turn-end
          :last-assistant-turn-data-start last-assistant-turn-data-start
          :last-current-assistant-turn-start last-current-assistant-turn-start
          :last-current-assistant-turn-data-start last-current-assistant-turn-data-start
          :last-turn-role last-turn-role)))

(defconst mevedel-view-render--gc-grace 5
  "Seconds a finished history job keeps deferring collection during typing.")

(defun mevedel-view-render-cancel-batch ()
  "Cancel this view's pending full-history callbacks and release their markers."
  (when mevedel-view-render--batch
    (when-let* ((timer (plist-get mevedel-view-render--batch :timer)))
      (cancel-timer timer))
    (when-let* ((scan (plist-get mevedel-view-render--batch :scan)))
      (mevedel-transcript-scan-cancel scan))
    (when-let* ((fontify (plist-get mevedel-view-render--batch :fontify)))
      (mevedel-view--markdown-fontify-job-cancel fontify))
    (dolist (entry (plist-get mevedel-view-render--batch :pending))
      (set-marker (nth 1 entry) nil)
      (set-marker (nth 2 entry) nil))
    (setf (plist-get mevedel-view-render--batch :settled-until)
          (+ (float-time) mevedel-view-render--gc-grace))
    (setq mevedel-view-render--batch nil)))

(defun mevedel-view-render--hold-gc (job)
  "Hold the busy collection threshold while this view runs batch JOB.
Collection waits for a pause in typing while the job runs and briefly
after it: a refresh allocates tens of megabytes, and one collection
landing in a callback was its slowest step.  A job parked on an
unattended view releases the hold, which would otherwise keep the
threshold raised and the maintenance timer waking each second until the
view is shown again; resuming takes it again."
  (let ((view (current-buffer)))
    (mevedel--gc-hold
     job (lambda ()
           (or (and (buffer-live-p view)
                    (eq job (buffer-local-value
                             'mevedel-view-render--batch view)))
               (< (float-time)
                  (or (plist-get job :settled-until) 0)))))))

(defun mevedel-view-render-resume-batch ()
  "Resume this view's paused history work without scheduling duplicate callbacks."
  (when (and mevedel-view-render--batch
             (or (eq (plist-get mevedel-view-render--batch :phase) 'prepare)
                 (plist-get mevedel-view-render--batch :pending))
             (not (mevedel-view--unattended-p))
             (not (mevedel--timer-pending-p
                   (plist-get mevedel-view-render--batch :timer))))
    (mevedel-view-render--hold-gc mevedel-view-render--batch)
    (setf (plist-get mevedel-view-render--batch :timer)
          (run-at-time 0.001 nil #'mevedel-view-render--batch-step
                       (current-buffer) mevedel-view-render--batch))))

(defun mevedel-view-render--priority-turns (turns &optional anchors-only)
  "Return TURNS visible to the current view's readers.
With ANCHORS-ONLY, return only turns needed immediately to restore point,
selection and window starts, plus their governing prompts; other visible
turns may follow in callbacks."
  (let ((ranges (list (cons (point) (1+ (point))))) sources selected prompt)
    (when (and mark-active (mark))
      (push (cons (region-beginning) (1+ (region-end))) ranges))
    (dolist (window (get-buffer-window-list (current-buffer) nil t))
      ;; The response at the top is anchored, but its governing user turn
      ;; can be farther up.  Keep that header in the immediate projection.
      (when anchors-only
        (when-let* ((pinned (mevedel-view--pinned-prompt window)))
          (push (cons (car pinned) (1+ (car pinned))) ranges)))
      (push (cons (window-start window)
                  (if anchors-only
                      (1+ (window-start window))
                    (or (window-end window) (window-start window)))) ranges)
      (push (cons (window-point window) (1+ (window-point window))) ranges))
    (dolist (range ranges)
      (let ((pos (max (point-min) (car range)))
            (end (min (point-max) (cdr range))))
        (while (< pos end)
          (when-let* ((source (get-text-property pos 'mevedel-view-source))
                      ((consp source))
                      (start (mevedel-view-disclosure-source-start source)))
            (push start sources))
          (setq pos (next-single-property-change pos 'mevedel-view-source nil end)))))
    (dolist (turn turns)
      (when (and anchors-only (eq (plist-get turn :role) 'user))
        (setq prompt turn))
      (when (seq-some (lambda (source)
                       (and (<= (plist-get turn :start) source)
                            (< source (plist-get turn :end)))) sources)
        (when (and anchors-only prompt (not (eq prompt turn))
                   (not (memq prompt selected)))
          (push prompt selected))
        (push turn selected)))
    ;; A new view has no source anchors.  Its current conversation comes first.
    (or (nreverse selected)
        (if (and anchors-only prompt (not (eq prompt (car (last turns)))))
            (list prompt (car (last turns)))
          (last turns)))))

(defun mevedel-view-render--batch-turn (job entry)
  "Replace JOB's placeholder ENTRY with its canonical rendered turn."
  (let* ((data (plist-get job :data))
         (plan (plist-get job :plan))
         (start (nth 1 entry))
         (end (nth 2 entry))
         (insertion (copy-marker start t))
         (mevedel-view--render-insertion-marker insertion)
         (mevedel-view--conversation-variant-sessions (plist-get plan :variants))
         (mevedel-transcript--tool-block-index (plist-get job :index))
         (mevedel-transcript-audit--decode-cache (plist-get job :audits))
         (mevedel-view--activity-entry-cache (plist-get job :tool-cache))
         (view (current-buffer))
         (inhibit-read-only t))
    (unwind-protect
        (progn
          (set-marker-insertion-type start nil)
          (set-marker-insertion-type end t)
          (delete-region start end)
          (with-current-buffer data
            (save-restriction
              (narrow-to-region (plist-get plan :start) (plist-get plan :end))
              (with-current-buffer view
                (mevedel-view--render-turn (car entry) data t (plist-get plan :session)))))
          (mevedel-view--collapse-settled-directive-turns)
          (mevedel-view-disclosure-restore-state
           start end (plist-get job :states)))
      (set-marker insertion nil)
      (set-marker-insertion-type start t)
      (set-marker-insertion-type end nil))))

(defun mevedel-view-render--replace-turn (data plan turn pos)
  "Re-render TURN from DATA under PLAN in place of the rendered turn at POS.
PLAN supplies the source restriction, session and variants, as a turn
context does.  Preserve reader, composer and disclosure state.  Return
non-nil on success; nil leaves the old rendering in place."
  (let* ((bounds (save-excursion
                   (goto-char pos)
                   (mevedel-view-disclosure-section-bounds
                    'mevedel-view-turn-id)))
         (from (copy-marker (car bounds)))
         (to (copy-marker (cdr bounds)))
         (copy (copy-sequence turn))
         (job (list :data data :plan plan
                    :index (make-hash-table :test #'eq)
                    :audits (make-hash-table :test #'equal)
                    :states (mevedel-view-disclosure-capture-state from to))))
    (setf (plist-get copy :render-id)
          (get-text-property pos 'mevedel-view-turn-id))
    (unwind-protect
        (condition-case err
            (progn
              (mevedel-view--call-preserving-user-view-state
               (lambda ()
                 (atomic-change-group
                   (mevedel-view-render--batch-turn
                    job (list copy from to)))))
              t)
          (error
           (mevedel-view--debug-log 'local-render-recovery :error err)
           nil))
      (set-marker from nil)
      (set-marker to nil))))

(defun mevedel-view-render--refresh-source (data start end)
  "Refresh the rendered turn containing DATA's START..END, if still current.
Return non-nil on success.  The caller already owns view mutation.  A source
change or missing context leaves the existing full-history fallback in charge."
  (when (and (buffer-live-p data)
             (eq data (mevedel-view-segments-display-buffer)))
    (let ((pos (point-min))
          (limit (mevedel-view--input-marker-position))
          (tick (with-current-buffer data (buffer-modified-tick))))
      (catch 'refreshed
        (while (< pos limit)
          (let* ((context (get-text-property pos 'mevedel-view-turn-context))
                 (turn (plist-get context :turn)))
            (when (and (eq data (plist-get context :data))
                       (eql tick (plist-get context :tick))
                       (<= (plist-get turn :start) start end
                           (plist-get turn :end)))
              (throw 'refreshed
                     (mevedel-view-render--replace-turn data context turn pos)))
            (setq pos (next-single-property-change
                       pos 'mevedel-view-turn-context nil limit))))))))

(defun mevedel-view-render--last-turn-position ()
  "Return the start of the last rendered turn before the composer, or nil."
  (let ((pos (mevedel-view--input-marker-position))
        found)
    (while (and (not found) pos (> pos (point-min)))
      (setq pos (previous-single-property-change
                 pos 'mevedel-view-turn-context nil (point-min)))
      (when (get-text-property pos 'mevedel-view-turn-context)
        (setq found pos)))
    found))

(defun mevedel-view-render--refresh-latest-turn-now (data)
  "Re-render DATA's newest assistant turn through DATA's end.
Return non-nil when the view is current afterwards; nil asks for a full
projection.  The caller owns view mutation."
  (cond
   ((not (buffer-live-p data)) t)
   ;; An archived segment is on display: the live turn is not rendered.
   ((not (eq data (mevedel-view-segments-display-buffer))) t)
   ((mevedel-view-stream-in-flight-turn-start-position) nil)
   (t
    (when-let* ((pos (mevedel-view-render--last-turn-position))
                (context (get-text-property pos 'mevedel-view-turn-context))
                ((eq data (plist-get context :data)))
                (turn (plist-get context :turn))
                ((eq (plist-get turn :role) 'assistant))
                (end (with-current-buffer data (point-max)))
                ((<= (plist-get turn :end) end))
                (turns
                 (with-current-buffer data
                   (save-restriction
                     (narrow-to-region (plist-get context :start) end)
                     (mevedel-view--group-transcript-turns
                      (mevedel-transcript-segments (plist-get turn :start) end)
                      data))))
                (fresh (car turns))
                ((null (cdr turns)))
                ((eq (plist-get fresh :role) 'assistant))
                ((eql (plist-get fresh :start) (plist-get turn :start)))
                ((equal (plist-get fresh :directive) (plist-get turn :directive))))
      (let ((plan (copy-sequence context)))
        (setf (plist-get plan :end) end
              (plist-get plan :variants)
              (and (not mevedel-view--agent-transcript-p)
                   (plist-get (mevedel-view-render--plan-finish
                               nil nil data mevedel--data-buffer nil)
                              :variants)))
        (mevedel-view-render--replace-turn data plan fresh pos))))))

(defun mevedel-view-render-refresh-latest-turn (data)
  "Re-render the newest assistant turn after settlement appended to DATA.
Saving a settled turn appends its durable fork-point record after the
response, outside the coordinates that turn was rendered with.  Only that
turn needs new source coordinates, so it is rendered again alone instead
of rebuilding history.  When the newest rendered turn cannot be matched
to DATA's tail, schedule a full projection instead."
  ;; A pending history job revalidates the source tick and restarts by
  ;; itself.  Entering the writer queue would force it to finish now.
  (unless mevedel-view-render--batch
    (mevedel-view-render-mutate
     'latest-turn
     (lambda ()
       (unless (mevedel-view-render--refresh-latest-turn-now data)
         (mevedel-view-rerender (current-buffer)))))))

(defun mevedel-view-render--batch-step (view job)
  "Advance one preparation stage or project one turn of JOB in VIEW."
  (when (buffer-live-p view)
    (with-current-buffer view
      (when (eq job mevedel-view-render--batch)
        (when-let* ((timer (plist-get job :timer))) (cancel-timer timer))
        (setf (plist-get job :timer) nil)
        (mevedel-view-render-mutate
         'full-batch
         (lambda ()
           (let ((mevedel-view-prepare-enabled t)
                 (mevedel-view--tool-cache-used (plist-get job :cache-used)))
             ;; Timers can fire inside a yielding writer.  Validate and consume
             ;; the job only after acquiring ownership, never when merely queued.
             (when (eq job mevedel-view-render--batch)
               (let ((data (plist-get job :data)))
		 (cond
                  ((not (buffer-live-p data)) (mevedel-view-render-cancel-batch))
                  ((mevedel-view--unattended-p) (mevedel--gc-release job))
                  ((mevedel-transport-busy-p (buffer-local-value 'default-directory data))
                   (setf (plist-get job :timer)
			 (run-at-time 0.1 nil #'mevedel-view-render--batch-step view job)))
                  ((or (not (eq data (mevedel-view-segments-display-buffer)))
                       (not (= (plist-get job :tick)
                               (with-current-buffer data (buffer-modified-tick)))))
                   (mevedel-view-render--start-batch))
                  ((eq (plist-get job :phase) 'prepare)
                   (condition-case err
                       (let ((mevedel-transcript--tool-block-index
                              (plist-get job :index))
                             (mevedel-transcript-audit--decode-cache
                              (plist-get job :audits)))
                         (if (mevedel-view-render--plan-step job)
                             (mevedel-view-render--install-batch job)
                           (mevedel-view-render-resume-batch)))
                     (error
                      (mevedel-view-render-cancel-batch)
                      (mevedel-view--debug-log 'batch-prepare-recovery :error err)
                      (mevedel-view-render--full-now))))
                  (t
                   (condition-case err
                       (let ((mevedel-view-render--prepared-responses
                              (plist-get job :fontified)))
                         (mevedel-view--call-preserving-user-view-state
                          (lambda ()
                            (atomic-change-group
                              (mevedel-view-render--batch-turn
                               job (car (plist-get job :pending)))))))
                     (error
                      (mevedel-view-render-cancel-batch)
                      (mevedel-view--debug-log 'batch-render-recovery :error err)
                      (mevedel-view-render--full-now)))
                   (when (eq job mevedel-view-render--batch)
                     (let ((entry (pop (plist-get job :pending))))
                       (set-marker (nth 1 entry) nil)
                       (set-marker (nth 2 entry) nil))
                     (if (plist-get job :pending)
			 (mevedel-view-render-resume-batch)
                       (mevedel-view--sweep-tool-rendering-cache
                        (plist-get job :cache-used))
                       (mevedel-view-render-cancel-batch))))))))))))))

(defun mevedel-view-render-batched-full ()
  "Rebuild history around its readers, then fill remaining turns in callbacks."
  (mevedel-view-render-mutate
   'full (lambda ()
           (let ((mevedel-view-prepare-enabled t))
             (mevedel-view-render--start-batch)))))

(defun mevedel-view-render--start-batch ()
  "Start a resumable settled-history projection with mutation ownership held.
In-flight streaming retains synchronous reconciliation of view-only live text."
  (mevedel-view-render-cancel-batch)
  (if (mevedel-view-stream-in-flight-turn-start-position)
      (mevedel-view-render--full-now)
    (let* ((data (mevedel-view-segments-display-buffer))
           (job (list :data data :view (current-buffer)
                      :live-data mevedel--data-buffer
                      :agent mevedel-view--agent-transcript-p
                      :started (float-time) :phase 'prepare
                      :prepare-stage 'restore :scan nil :remaining nil
                      :filtered-rev nil :group-state nil :turns nil
                      :annotate-rest nil :ranges nil
                      :fontify-turns nil :fontify-segments nil
                      :fontify nil :fontify-key nil
                      :fontified (make-hash-table :test #'equal)
                      :tools-turns nil :tools-segments nil :tool-state-tick nil
                      :tool-cache (make-hash-table :test #'equal)
                      :cache-used (make-hash-table :test #'equal)
                      :plan nil :states nil :pending nil :timer nil
                      :settled-until nil
                      :tick (with-current-buffer data (buffer-modified-tick))
                      :index (make-hash-table :test #'eq)
                      :audits (make-hash-table :test #'equal))))
      (setq mevedel-view-render--batch job)
      (mevedel-view-render-resume-batch))))

(defun mevedel-view-render--install-batch (job)
  "Publish JOB's prepared history with reader state captured now.
Until the complete plan is ready, keep the previous view in place."
  (let* ((view (current-buffer))
         (data (plist-get job :data))
         (live-data (plist-get job :live-data))
         (historical (not (eq data live-data)))
         (agent (plist-get job :agent))
         (started (plist-get job :started))
         (plan (plist-get job :plan))
         (interaction-visits (mevedel-view--interaction-focused-visits))
         (mevedel-view-render--prepared-responses (plist-get job :fontified))
         (mevedel-transcript--tool-block-index (plist-get job :index))
         (mevedel-transcript-audit--decode-cache (plist-get job :audits))
         ;; Everything a reader can see renders now.  A visible placeholder
         ;; would be drawn and then replaced, moving text the reader is on.
         (immediate (append (mevedel-view-render--priority-turns
                             (plist-get plan :turns))
                            (mevedel-view-render--priority-turns
                             (plist-get plan :turns) t)))
           (states (mevedel-view-disclosure-capture-state
                    (point-min) (mevedel-view--input-marker-position)))
           installed)
      ;; The projection removes running rows; their next live render
      ;; restores the phase they displayed.
      (when (and (not historical) mevedel-view--pending-tool-calls)
        (setq mevedel-view--carried-tool-samples
              (mevedel-view--snapshot-tool-animation-targets)))
      ;; A reader can move onto any pending response before its callback.
      ;; Project chat prompts now so each such response has a real header
      ;; above it, with both a preview and a click target.  Keep the costly
      ;; assistant turns deferred.
      (unless agent
        (dolist (turn (plist-get plan :turns))
          (when (eq (plist-get turn :role) 'user)
            (push turn immediate))))
      (setf (plist-get job :states) states)
      (unless (eql (buffer-modified-tick) (plist-get job :tool-state-tick))
        (clrhash (plist-get job :tool-cache)))
      (unwind-protect
          (progn
            (mevedel-view--call-preserving-user-view-state
             (lambda ()
               (atomic-change-group
                 (let ((inhibit-read-only t))
                   (mevedel-view--full-rerender-reset data live-data historical agent)
                   (when-let* ((summary (plist-get plan :summary-source)))
                     (mevedel-view--insert-compaction-indicator
                      view (plist-get plan :summary-audits) summary))
                   (dolist (turn (plist-get plan :turns))
                     (goto-char mevedel-view--input-marker)
                     (let ((start (copy-marker (point) nil)))
                       (mevedel-view-render--with-boundaries-advancing
                         (insert (propertize
                                  "Loading conversation turn...\n"
                                  'read-only t
                                  'mevedel-view-type 'history-pending
                                  'mevedel-view-turn-id (make-symbol "pending-turn")
                                  'mevedel-view-source
                                  (mevedel-view-disclosure-source-range
                                   data (plist-get turn :start) (plist-get turn :end)))))
                       (set-marker-insertion-type start t)
                       (push (list turn start (copy-marker (point) nil))
                             (plist-get job :pending))))
                   (setf (plist-get job :pending) (nreverse (plist-get job :pending)))
                   (dolist (entry (plist-get job :pending))
                     (when (memq (car entry) immediate)
                       (mevedel-view-render--batch-turn job entry)
                       (set-marker (nth 1 entry) nil)
                       (set-marker (nth 2 entry) nil)))
                   (setf (plist-get job :pending)
                         (cl-remove-if (lambda (entry) (memq (car entry) immediate))
                                       (plist-get job :pending)))
                   (mevedel-view--full-rerender-finish
                    data live-data
                    (list :view-buffer view
                          :interaction-visits interaction-visits)
                    historical
                    started)))))
            (setf (plist-get job :phase) 'project)
            (if (plist-get job :pending)
                (mevedel-view-render-resume-batch)
              (mevedel-view--sweep-tool-rendering-cache (plist-get job :cache-used))
              (mevedel-view-render-cancel-batch))
            (setq installed t))
        (unless installed (mevedel-view-render-cancel-batch)))))

(defun mevedel-view--reanchor-data-turn-start (data-buf position)
  "Point `mevedel-view--data-turn-start' at POSITION in DATA-BUF.
Call from the view buffer.  POSITION nil leaves the marker untouched."
  (when position
    (when (markerp mevedel-view--data-turn-start)
      (set-marker mevedel-view--data-turn-start nil))
    (setq mevedel-view--data-turn-start
          (with-current-buffer data-buf
            (copy-marker position nil)))))

(defun mevedel-view--full-rerender-reanchor
    (data-buf rendering in-flight-was data-turn-start-pos preserved-live-tail)
  "Recover the live-turn anchor after projecting DATA-BUF.
RENDERING is the projection state.  IN-FLIGHT-WAS enables recovery.
DATA-TURN-START-POS identifies the active data turn, and
PRESERVED-LIVE-TAIL contains any view-only streamed text."
  (when in-flight-was
    (let ((view-buf (plist-get rendering :view-buffer))
          (last-assistant
           (plist-get rendering :last-assistant-turn-start))
          (last-assistant-end
           (plist-get rendering :last-assistant-turn-end))
          (last-current-assistant
           (plist-get rendering :last-current-assistant-turn-start))
          (last-role (plist-get rendering :last-turn-role)))
      (with-current-buffer view-buf
        (let ((tail-start
               (and preserved-live-tail
                    (mevedel-view--live-tail-rendered-position
                     preserved-live-tail mevedel-view--input-marker
                     last-assistant last-assistant-end))))
          (cond
           (last-current-assistant
            (mevedel-view--debug-log
             'full-rerender-reanchor
             :decision 'current-assistant
             :last-turn-role last-role
             :last-assistant-turn-start last-assistant
             :last-current-assistant-turn-start last-current-assistant
             :data-turn-start data-turn-start-pos
             :state (mevedel-view--debug-state data-buf))
            (mevedel-view-stream-set-in-flight-turn-start
             last-current-assistant)
            (mevedel-view--reanchor-data-turn-start
             data-buf
             (plist-get rendering :last-current-assistant-turn-data-start)))
           ((and (not data-turn-start-pos)
                 (eq last-role 'assistant)
                 last-assistant)
            (mevedel-view--debug-log
             'full-rerender-reanchor
             :decision 'last-assistant
             :last-turn-role last-role
             :last-assistant-turn-start last-assistant
             :state (mevedel-view--debug-state data-buf))
            (mevedel-view-stream-set-in-flight-turn-start last-assistant)
            (mevedel-view--reanchor-data-turn-start
             data-buf
             (plist-get rendering :last-assistant-turn-data-start)))
           (tail-start
            (mevedel-view--debug-log
             'full-rerender-reanchor
             :decision 'existing-live-tail
             :last-turn-role last-role
             :tail-start tail-start
             :state (mevedel-view--debug-state data-buf))
            (mevedel-view-stream-set-in-flight-turn-start tail-start))
           (preserved-live-tail
            (goto-char mevedel-view--input-marker)
            (mevedel-view-render--with-boundaries-advancing
              (let ((tail-start (point)))
                (insert preserved-live-tail)
                (mevedel-view--debug-log
                 'full-rerender-reanchor
                 :decision 'preserved-live-tail
                 :last-turn-role last-role
                 :tail-start tail-start
                 :state (mevedel-view--debug-state data-buf))
                (mevedel-view-stream-set-in-flight-turn-start tail-start))))
           (t
            (mevedel-view--debug-log
             'full-rerender-reanchor
             :decision 'input-marker
             :last-turn-role last-role
             :state (mevedel-view--debug-state data-buf))
            (mevedel-view-stream-set-in-flight-turn-start
             mevedel-view--input-marker)))
          ;; The projection just rendered everything up to the data
          ;; buffer's end as settled history.  When no assistant turn
          ;; anchored the data marker above, park it at that end so the
          ;; next incremental render extracts only content that arrives
          ;; later, instead of re-rendering from a stale position -- a
          ;; whole-buffer rewrite (compaction, segment rotation)
          ;; collapses the old marker to `point-min', which made every
          ;; incremental render re-render the entire transcript.
          (unless (or last-current-assistant
                      (and (not data-turn-start-pos)
                           (eq last-role 'assistant)
                           last-assistant))
            (mevedel-view--reanchor-data-turn-start
             data-buf
             (with-current-buffer data-buf (point-max)))))))))

(defun mevedel-view--full-rerender-finish
    (data-buf live-data-buf rendering historical-p start-time)
  "Rebuild live chrome after projecting DATA-BUF.
LIVE-DATA-BUF owns live status.  RENDERING identifies the view.
HISTORICAL-P suppresses live-only rows.  START-TIME is for diagnostics.
The projection drew settled history only; in-flight tool calls return
with the next boundary render of the live turn."
  (with-current-buffer (plist-get rendering :view-buffer)
    (when (and (not historical-p) mevedel-view--pending-tool-calls)
      (mevedel-view--schedule-tool-boundary-render live-data-buf))
    (unless mevedel-view--agent-transcript-p
      (mevedel-view-refresh-input-prompt)
      ;; Reset removed these zones too.  Materialize them under the current
      ;; writer so its preservation wrapper can resolve their reader anchors;
      ;; queued redraws would run only after those anchors were restored.
      (mevedel-view--render-status-now live-data-buf)
      (mevedel-view--interaction-rebuild
       t (plist-get rendering :interaction-visits))
      (mevedel-view--ensure-request-progress live-data-buf)
      (when (fboundp 'mevedel-directive-frame-refresh-filter)
        (mevedel-directive-frame-refresh-filter)))
    (mevedel-view--debug-log
     'full-rerender-after-render
     :last-assistant-turn-start
     (plist-get rendering :last-assistant-turn-start)
     :last-current-assistant-turn-start
     (plist-get rendering :last-current-assistant-turn-start)
     :elapsed (- (float-time) start-time)
     :state (mevedel-view--debug-state data-buf))))

(defun mevedel-view--full-rerender (&optional transcript-buffer source-changed-p)
  "Rebuild the projection, coalescing nested requests for its current source.
TRANSCRIPT-BUFFER selects the transcript; SOURCE-CHANGED-P drops old fold state."
  (mevedel-view-render-mutate
   'full
   (lambda ()
     (mevedel-view-render--full-now transcript-buffer source-changed-p))
   source-changed-p))

(defun mevedel-view-render--full-now (&optional transcript-buffer source-changed-p)
  "Re-render the view from TRANSCRIPT-BUFFER or its displayed transcript.
Wipe all rendered content and re-render from scratch.  Used after
compaction, session resume, or manual refresh.

When SOURCE-CHANGED-P is non-nil, do not carry disclosure state from the
previously projected transcript into this render.

Preserves the active composer, live window state, and an in-flight
assistant anchor while rebuilding the transcript projection and live
view chrome."
  (unless mevedel--data-buffer
    (error "No data buffer"))
  (unless (bound-and-true-p mevedel-view-prepare-enabled)
    (mevedel-view-prepare-cancel))
  (mevedel-view-render-cancel-batch)
  (atomic-change-group
    (mevedel-view-render--preserving-window-state
     (mevedel-view--call-preserving-input-text
      (lambda ()
      (let* ((mevedel-view--tool-cache-used (make-hash-table :test #'equal))
             (mevedel-transcript--tool-block-index (make-hash-table :test #'eq))
             (mevedel-view--path-link-memo (make-hash-table :test #'equal))
             (mevedel-transcript-audit--decode-cache (make-hash-table :test #'equal))
             (start-time (float-time))
             (data-buf
              (or transcript-buffer
                  (mevedel-view-segments-display-buffer)))
             (live-data-buf mevedel--data-buffer)
             (historical-p (not (eq data-buf live-data-buf)))
             (session-data-buf
              (if historical-p live-data-buf data-buf))
             (render-view-buf (current-buffer))
             (agent-transcript-p mevedel-view--agent-transcript-p)
             (inhibit-read-only t)
             (inhibit-modification-hooks t)
             (inhibit-redisplay t)
             (saved-states
              (and (not source-changed-p)
                   (markerp mevedel-view--input-marker)
                   (marker-position mevedel-view--input-marker)
                   (mevedel-view-disclosure-capture-state
                    (point-min)
                    (marker-position mevedel-view--input-marker))))
             (data-turn-start-pos
              (and (not historical-p)
                   (markerp mevedel-view--data-turn-start)
                   (marker-position mevedel-view--data-turn-start)))
             (in-flight-was
              (and (not historical-p)
                   (mevedel-view-stream-in-flight-turn-start-position)))
             (interaction-visits (mevedel-view--interaction-focused-visits))
             (preserved-live-tail
              (when-let* (((not historical-p))
                          ((not agent-transcript-p))
                          (tail-start
                           (mevedel-view-stream-in-flight-turn-start-position))
                          ((markerp mevedel-view--status-marker))
                          (tail-end
                           (marker-position mevedel-view--status-marker))
                          ((< tail-start tail-end)))
                (mevedel-view--strip-pending-decorations-from-string
                 (buffer-substring tail-start tail-end)))))
        (when (and (not historical-p) mevedel-view--pending-tool-calls)
          (setq mevedel-view--carried-tool-samples
                (mevedel-view--snapshot-tool-animation-targets)))
        (mevedel-view--debug-log
         'full-rerender-begin
         :in-flight-was in-flight-was
         :preserved-live-tail-len
         (and preserved-live-tail (length preserved-live-tail))
         :state (mevedel-view--debug-state data-buf))
        ;; Visible-state capture cannot see children of a folded group.
        ;; Retain their identities while rebuilding the same transcript.
        (when source-changed-p
          (mevedel-view-disclosure-reset-state))
        (mevedel-view--full-rerender-reset
         data-buf session-data-buf historical-p agent-transcript-p)
        (let ((rendering
               (mevedel-view--full-rerender-project
                data-buf session-data-buf render-view-buf
                agent-transcript-p data-turn-start-pos saved-states)))
          (setq rendering (plist-put rendering :interaction-visits
                                     interaction-visits))
          (mevedel-view--full-rerender-reanchor
           data-buf rendering in-flight-was
           data-turn-start-pos preserved-live-tail)
          (prog1
              (mevedel-view--full-rerender-finish
               data-buf live-data-buf rendering historical-p start-time)
            (mevedel-view--sweep-tool-rendering-cache
             mevedel-view--tool-cache-used)))))))))

;;
;;; Optimistic user turn rendering

(defun mevedel-view--insert-user-message
    (text &optional kind hook-context prompt-summary-body
          prompt-summary-source hook-audits attribution user-source)
  "Render TEXT as a user message in the history region.
Inserts at the history boundary with read-only protection.
KIND may be `directive' to fontify directive-specific display text.
HOOK-CONTEXT is model-visible hook context to summarize in the view.
PROMPT-SUMMARY-BODY, when non-nil, is shown as a collapsed Prompt
section backed by PROMPT-SUMMARY-SOURCE when available.  HOOK-AUDITS
is a list of hook audit records to render under the user turn.
ATTRIBUTION identifies a background event or a collaboration guest and any
attached shared context.
USER-SOURCE bounds the submitted prompt in the data buffer, including
trusted skill metadata used for immediate attachment disclosures.

Sets `mevedel-view--user-pre-rendered' so the post-response render
path knows to skip the user turn it would otherwise extract for this
same exchange -- see `mevedel-view-stream-render-response'.  Returns a
marker at the end of the inserted block."
  (mevedel-view--ensure-interactive-chat-view)
  (save-excursion
    (goto-char (mevedel-view--history-insertion-marker))
    (mevedel-view-render--with-boundaries-advancing
      (let ((inhibit-read-only t)
            (start (point))
            (fold-start nil)
            (guest-name (plist-get attribution :name))
            (background
             (eq (plist-get attribution :type) 'background-event))
            (shared-display (mevedel-transcript-audit-shared-context
                             text (plist-get attribution :shared)))
            user-end)
        (insert (propertize (cond
                             (background "")
                             (guest-name (format "%s (guest)\n" guest-name))
                             (t "You\n"))
                            'font-lock-face
                            (if (or guest-name background)
                                'mevedel-view-guest-header
                              'mevedel-view-user-header)
                            'mevedel-view-prompt-preview
                            (mevedel-view--prompt-preview text shared-display)))
        (cond
         (background)
         (shared-display
          (setq fold-start
                (mevedel-view--insert-shared-context shared-display user-source)))
         ((and (not (eq kind 'directive))
               (mevedel-view--user-input-fold-p text))
          (setq fold-start (point))
          (mevedel-view--insert-user-input-fold text))
         (t
          (insert (if (eq kind 'directive)
                      (mevedel-view--fontify-directive-display-text text)
                    (mevedel-view--fontify-as text 'markdown-mode)))
          (unless (eq (char-before) ?\n)
            (insert "\n"))))
        (setq user-end (point))
        (when-let* ((events (mevedel-view--hook-context-events-from-text
                             hook-context)))
          (mevedel-view--insert-hook-context-block events))
        (dolist (audit hook-audits)
          (mevedel-view--insert-hook-audit-block
           audit
           (plist-get audit :source)))
        (let ((inline-skill
               (when (and user-source (buffer-live-p mevedel--data-buffer))
                 (with-current-buffer mevedel--data-buffer
                   (mevedel-view--inline-skill-info
                    (mevedel-transcript-segments (car user-source) (cdr user-source))
                    (current-buffer))))))
          (if inline-skill
              (mevedel-view--insert-skill-disclosures inline-skill)
            (let ((prompt-body (and (stringp prompt-summary-body)
                                    (string-trim prompt-summary-body))))
              (when (and prompt-body (not (string-empty-p prompt-body)))
                (mevedel-view--insert-rendered-tool
                 (list :header "Prompt" :body prompt-body
                       :body-mode 'markdown-mode :vtype 'prompt-summary
                       :initially-collapsed-p t)
                 prompt-summary-source)))))
        (insert (propertize "\n" 'font-lock-face 'mevedel-view-separator))
        (add-text-properties start (point)
                             `(read-only t
                               keymap ,mevedel-view--display-map
                               front-sticky (read-only keymap)
                               rear-nonsticky (read-only keymap)))
        (put-text-property start user-end 'mevedel-view-type 'user)
        ;; The blanket `user' stamp above overwrites the fold's own
        ;; vtype, which is what its toggle dispatches on.
        (when fold-start
          (put-text-property fold-start user-end
                             'mevedel-view-type 'user-input-summary))
        (setq mevedel-view--user-pre-rendered t)
        (copy-marker (point) nil)))))


(provide 'mevedel-view-render)
;;; mevedel-view-render.el ends here
