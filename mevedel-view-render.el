;;; mevedel-view-render.el --- Transcript rendering and navigation -*- lexical-binding: t -*-

;;; Commentary:

;; Owns transcript turn grouping, source-backed rendering, renderer
;; interpretation, folding, expansion, full and incremental redraws, and
;; transcript navigation for mevedel view buffers.

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'mevedel-execution-transcript)
(require 'mevedel-plan)

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
(autoload 'mevedel-open-directive-activity "mevedel-directive-activity")

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
(declare-function mevedel-tool-get "mevedel-tool-registry"
		  (name &optional category))
(declare-function mevedel-tool-name "mevedel-tool-registry" (cl-x) t)
(declare-function mevedel-tool-renderer "mevedel-tool-registry" (cl-x)
                  t)

;; `mevedel-tool-render-data'
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

;; `mevedel-tool-ui'
(declare-function mevedel-tool-ui--render-agent "mevedel-tool-ui"
		  (name args result render-data))

;; `mevedel-transcript'
(declare-function mevedel-transcript--mailbox-any-block-at-point
		  "mevedel-transcript" (limit))
(declare-function mevedel-transcript--mailbox-find-close
		  "mevedel-transcript" (open-regexp close-tag limit))
(declare-function mevedel-transcript--org-tool-block-parts
		  "mevedel-transcript" (start end))
(declare-function mevedel-transcript--skip-leading-properties-drawer
		  "mevedel-transcript" (pos))
(declare-function mevedel-transcript--skip-leading-summary-block
		  "mevedel-transcript" (pos))
(declare-function mevedel-transcript--tool-block-bounds-for-run
		  "mevedel-transcript"
		  (seg-start seg-end &optional limit))
(declare-function mevedel-transcript--tool-id-in-range
		  "mevedel-transcript" (start end))
(declare-function mevedel-transcript-segments "mevedel-transcript"
		  (start end))

;; `mevedel-transcript-audit'
(declare-function mevedel-transcript-audit-guest-prompts
                  "mevedel-transcript-audit" ())
(declare-function mevedel-transcript-audit-only-p
                  "mevedel-transcript-audit" (text))
(declare-function mevedel-transcript-buffer-directive-ranges
                  "mevedel-transcript-audit" (&optional allow-open))
(autoload 'mevedel--strip-hook-audit-blocks "mevedel-transcript-audit")

;; `mevedel-transcript-restore'
(declare-function mevedel-transcript-restore-properties
                  "mevedel-transcript-restore" (&optional only-if-missing))

;; `mevedel-utilities'
(declare-function mevedel--trim-tool-result "mevedel-utilities" (text))
(declare-function mevedel--warn-once
                  "mevedel-utilities" (key format &rest args))
(defvar mevedel--hook-audit-close)
(defvar mevedel--hook-audit-open)

;; `mevedel-view'
(declare-function mevedel-view--header-string
                  "mevedel-view" (data-buf))
(declare-function mevedel-view--render-status
                  "mevedel-view" (&optional data-buf))
(defvar mevedel-view--display-map)
(defvar mevedel-view--interaction-marker)
(defvar mevedel-view--status-marker)
(defvar mevedel-view-pending-tools-visible-max)

;; `mevedel-view-agent'
(declare-function mevedel-view--insert-attribution
                  "mevedel-view-agent"
                  (agent-path))
(declare-function mevedel-view--render-agent-status
                  "mevedel-view-agent" ())
(defvar mevedel-view--agent-handle-map)
(defvar mevedel-view--agent-label-map)
(defvar mevedel-view--agent-transcript-p)

;; `mevedel-view-composer'
(declare-function mevedel-view--call-preserving-input-text
                  "mevedel-view-composer" (thunk))
(declare-function mevedel-view--call-preserving-user-view-state
                  "mevedel-view-composer" (thunk))
(declare-function mevedel-view--call-preserving-window-state
                  "mevedel-view-composer" (thunk))
(declare-function mevedel-view--call-with-render-boundaries-advancing
                  "mevedel-view-composer" (thunk))
(declare-function mevedel-view--ensure-interactive-chat-view
                  "mevedel-view-composer" ())
(declare-function mevedel-view--input-marker-position
                  "mevedel-view-composer" ())
(declare-function mevedel-view--input-start
                  "mevedel-view-composer" ())
(declare-function mevedel-view--prompt-start-position
                  "mevedel-view-composer" ())
(declare-function mevedel-view-enter-directive-scope
                  "mevedel-view-composer"
                  (directive action &optional attempt-index workspace))
(declare-function mevedel-view-refresh-input-prompt
                  "mevedel-view-composer" ())
(defvar mevedel-view--input-marker)

;; `mevedel-view-disclosure'
(declare-function mevedel-view-disclosure-apply-rendering-state
                  "mevedel-view-disclosure" (rendering source))
(declare-function mevedel-view-disclosure-capture-state
                  "mevedel-view-disclosure" (from to))
(declare-function mevedel-view-disclosure-data-substring
                  "mevedel-view-disclosure"
                  (data-buf start end &optional properties))
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
                  "mevedel-view-disclosure" ())
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
(declare-function mevedel-view--interaction-rebuild
                  "mevedel-view-interaction" ())

;; `mevedel-view-markdown'
(declare-function mevedel-view--last-live-response-boundary
                  "mevedel-view-markdown" (data-buf start end))

;; `mevedel-view-segments'
(declare-function mevedel-view-go-to-segment
                  "mevedel-view-segments" (&optional number))
(declare-function mevedel-view-return-to-latest-segment
                  "mevedel-view-segments" (&optional _event))
(declare-function mevedel-view-segments-banner
                  "mevedel-view-segments" ())
(declare-function mevedel-view-segments-display-buffer
                  "mevedel-view-segments" ())
(declare-function mevedel-view-segments-initialize
                  "mevedel-view-segments" ())

;; `mevedel-view-stream'
(declare-function mevedel-view--delete-pending-tool-live-lines
                  "mevedel-view-stream" ())
(declare-function mevedel-view--duration-label
                  "mevedel-view-stream" (seconds))
(declare-function mevedel-view--ensure-request-progress
                  "mevedel-view-stream" (&optional data-buf status))
(declare-function mevedel-view--forget-request-progress-region
                  "mevedel-view-stream" ())
(declare-function mevedel-view--insert-pending-tool-lines
                  "mevedel-view-stream" (entries))
(declare-function mevedel-view--refresh-pending-tool-lines
                  "mevedel-view-stream" ())
(declare-function mevedel-view--spinner-frame "mevedel-view-stream" ())
(declare-function mevedel-view--spinner-region-p
                  "mevedel-view-stream" (start end))
(declare-function mevedel-view-stream-in-flight-turn-start-position
                  "mevedel-view-stream" ())
(declare-function mevedel-view-stream-set-in-flight-turn-start
                  "mevedel-view-stream" (position))
(defvar mevedel-view--data-turn-start)
(defvar mevedel-view--execution-events)
(defvar mevedel-view--in-flight-turn-start)
(defvar mevedel-view--pending-tool-calls)
(defvar mevedel-view-spinner-frames)

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
is fontified in a temporary Markdown buffer when `markdown-ts-mode' or
`markdown-mode' is available."
  :type 'boolean
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
  "Maximum number of view-local cached render entries before clearing.
The cache is disposable and keyed by data-buffer positions plus modification
tick, so clearing it only affects rendering speed."
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
         (warning-p (or blocked-p error-p))
         (summary
          (mevedel-view--tool-call-line
           (if warning-p "!" "✓")
           (if warning-p
               'mevedel-view-tool-warning
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
          'font-lock-face 'mevedel-view-tool-warning))
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
         (truncate-string-to-width
          (replace-regexp-in-string "[\n\r]+" " " raw)
          60 nil nil "...")
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

(defvar-local mevedel-view--render-cache-entries 0
  "Approximate number of entries in view-local render caches.")

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

(defun mevedel-view-render-initialize ()
  "Initialize transcript-rendering state in the current view buffer."
  (mevedel-view-render-invalidate-live-tail)
  (require 'markdown-mode nil t)
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
  (setq-local mevedel-view--render-cache-entries 0)
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
        (setq pos (1- pos))))
    (if (not source)
        (mevedel-view-render-invalidate-live-tail)
      (let ((view-start pos))
        (while (and (> view-start start)
                    (equal source
                           (get-text-property
                            (1- view-start)
                            'mevedel-view-live-unit-source)))
          (setq view-start (1- view-start)))
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
    (cl-labels
        ((shift-key
          (key)
          (if (and (consp key)
                   (eq (car key) 'source)
                   (integerp (nth 2 key)))
              (let ((shifted (copy-tree key)))
                (setcar (nthcdr 2 shifted) (+ (nth 2 key) delta))
                shifted)
            key)))
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
        (clrhash mevedel-view--tool-rendering-cache)
        (setq mevedel-view--render-cache-entries 0)))))


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
no `mevedel-view-type' property yet."
  (add-text-properties start end
                       '(read-only t
                         front-sticky (read-only keymap)
                         rear-nonsticky
                         (read-only keymap line-prefix wrap-prefix)))
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


(defun mevedel-view--promote-face-to-font-lock-face (s)
  "Rename `face' text properties on S to `font-lock-face' in place.
`text-mode' (and most other major modes) enable `font-lock-mode'
through `global-font-lock-mode'.  Font-lock's unfontify pass strips
the `face' property from any region it touches, which would wipe
out the org faces we pre-apply to response text.  `font-lock-face'
survives unfontify and is rendered identically in font-lock-enabled
buffers, so promoting the property keeps our pre-applied highlighting
through font-lock refontification cycles.  Returns S."
  (let ((pos 0)
        (end (length s)))
    (while (< pos end)
      (let* ((next (or (next-single-property-change pos 'face s) end))
             (face (get-text-property pos 'face s)))
        (when face
          (remove-text-properties pos next '(face nil) s)
          (put-text-property pos next 'font-lock-face face s))
        (setq pos next)))
    s))

(defun mevedel-view--markdown-fontify-mode ()
  "Return the best available Markdown major mode for temp fontification."
  (cond
   ((fboundp 'markdown-ts-mode) 'markdown-ts-mode)
   ((fboundp 'markdown-mode)
    'markdown-mode)))

(defun mevedel-view--visible-response-text (text)
  "Return response TEXT with model protocol hidden when appropriate."
  (let ((text (mevedel-view--strip-render-data-display-text text)))
    (when (mevedel-view--strip-proposed-plans-p text)
      (setq text (mevedel-plan-strip-proposed text)))
    (replace-regexp-in-string "^</?proposed_plan>[ \t]*\n?" "" text)))

(defmacro mevedel-view--with-render-temp-buffer (&rest body)
  "Run BODY in a temporary buffer with user mode hooks suppressed."
  (declare (indent 0) (debug t))
  ;; Modes chatter while they set themselves up (`sh-mode' announces its
  ;; indentation setup, `python-mode' guesses its offset).  Rendering must
  ;; not push that into the echo area.
  `(let ((change-major-mode-after-body-hook nil)
         (after-change-major-mode-hook nil)
         (hack-local-variables-hook nil)
         (enable-local-variables nil)
         (font-lock-mode-hook nil)
         (inhibit-message t)
         (org-mode-hook nil))
     (with-temp-buffer
       (delay-mode-hooks
         ,@body))))

(defun mevedel-view--render-cache-key (text)
  "Return a compact cache key for TEXT content."
  (list (length text)
        (sxhash-equal text)
        (and (> (length text) 32) (substring text 0 16))
        (and (> (length text) 32)
             (substring text (- (length text) 16)))))

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
Suppresses major-mode hooks so temp-buffer fontification does not run
user UI setup.
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
         (cached (and key (gethash key cache))))
    (prog1
        (or cached
            (let ((rendered
                   (if (and mevedel-view-fontify-responses mode)
                       (condition-case err
                           (mevedel-view--fontify-as text mode)
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
  (let ((inv (and (boundp 'mevedel--agent-invocation)
                  mevedel--agent-invocation)))
    (and (mevedel-agent-invocation-p inv)
         (eq (mevedel-agent-invocation-transcript-status inv)
             'running))))

(defun mevedel-view--group-into-turns (segments data-buf)
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
real user message."
  (let (turns current-segs current-role turn-start prev-type
        (rest segments))
    (while rest
      (let* ((seg (car rest))
             (type (car seg))
             (seg-start (cadr seg))
             (seg-end (caddr seg))
             (next-type (car-safe (cadr rest)))
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
             (seg-scaffolding-only-p
              (and (memq type '(user ignored))
                   seg-text
                   (mevedel-view--scaffolding-only-text-p seg-text)))
             (prompt-drawer-after-user-p
              (and (eq type 'prompt)
                   (null current-role)
                   turns
                   (eq (plist-get (car turns) :role) 'user)))
             (hook-audit-only-p
              (and (eq type 'ignored)
                   seg-text
                   (mevedel-view--hook-audit-only-text-p seg-text)))
             (hook-audit-after-user-p
              (and hook-audit-only-p
                   (null current-role)
                   turns
                   (eq (plist-get (car turns) :role) 'user)))
             (hook-context-after-user-p
              (and (eq type 'hook-context)
                   (null current-role)
                   turns
                   (eq (plist-get (car turns) :role) 'user)))
             (user-display-after-user-p
              (and (eq type 'render-data)
                   (null current-role)
                   turns
                   (eq (plist-get (car turns) :role) 'user)
                   (eq seg-render-kind 'user-display)
                   (stringp (plist-get seg-render-data :text))))
             (scaffolding-before-hook-audit-p
              (and (eq type 'user)
                   seg-scaffolding-only-p
                   (let ((next (cadr rest)))
                     (and (eq (car-safe next) 'ignored)
                          (mevedel-view--hook-audit-only-segment-p
                           data-buf (cadr next) (caddr next))))))
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
         ((or prompt-drawer-after-user-p
              hook-audit-after-user-p
              hook-context-after-user-p
              user-display-after-user-p
              (and inline-skill-render-p
                   (null current-role)
                   turns
                   (eq (plist-get (car turns) :role) 'user)))
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
               (or review-action-p
                   agent-task-p
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
    ;; Flush final turn
    (when current-segs
      (push (list :role current-role
                  :segments (nreverse current-segs)
                  :start turn-start
                  :end (caddr (car current-segs)))
            turns))
    (nreverse turns)))

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
                                  (cl-gensym "mevedel-view-directive-turn-")))
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
    turns))


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
           (text (mevedel-view--tool-readable-text raw)))
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
;; (ToolScript).  Each entry is a plist (:id ID :tool NAME :args PLIST :result STRING :status SYM
;; :render-data DATA) and is rendered by that tool's own registered renderer,
;; so a nested Grep row gets Grep's header and `grep-mode' body.  Rows are
;; inserted only while the owning block is expanded, and each row is its own
;; collapsible section.
;;
;; The interpreter below parses the tool segment in the data buffer, invokes the
;; renderer (with a condition-case fallback to the generic renderer on error),
;; and inserts the rendered output. Expand and collapse re-invoke the renderer
;; on every transition so no state is cached in text properties.

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
           (text (mevedel-view--tool-readable-text raw))
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
          (let* ((sexp (read text))
                 (name (plist-get sexp :name))
                 (args (plist-get sexp :args)))
            (when (stringp name)
              (let* ((sexp-end (with-temp-buffer
                                 (insert text)
                                 (goto-char (point-min))
                                 (forward-sexp 1)
                                 (point)))
                     (full-result (mevedel--trim-tool-result
                                   (substring text sexp-end)))
                     (full-result
                      (if (and (derived-mode-p 'org-mode)
                               (fboundp 'org-unescape-code-in-string))
                          (org-unescape-code-in-string full-result)
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
                        (mevedel-view--hook-audit-records-from-text
                         (buffer-substring audit-start audit-end)
                         nil data-buf audit-start)))
                     (full-result
                      (mevedel--strip-hook-audit-blocks full-result))
                     (extract (mevedel-tool-render-data-extract
                               full-result
                               (and (boundp 'mevedel--session)
                                    mevedel--session)
                               tool-id
                               (and (stringp tool-id)
                                    (not (string-empty-p tool-id))
                                    (equal name "Read")
                                    (mevedel-view--read-args-media-p args))))
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

(defun mevedel-view--tool-render-status (result &optional render-data)
  "Return the renderer dispatch status for RESULT and RENDER-DATA."
  (or (and (memq (plist-get render-data :status) '(success error))
           (plist-get render-data :status))
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
  (let* ((explicit-status
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
                  (if (or explicit-status (eq status 'error))
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
             nil))))))))

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

(defun mevedel-view--fontify-as (text mode)
  "Return TEXT fontified as if displayed in MODE.
MODE is a major-mode symbol.  Unknown or nil MODE returns TEXT verbatim.
Uses a throwaway temp buffer with mode hooks and local variables disabled,
and `font-lock-ensure' to force a full fontification pass.
Faces are promoted to `font-lock-face' so they survive the view
buffer's font-lock refontification cycles."
  (let ((mode (if (eq mode 'markdown-mode)
                  (or (mevedel-view--markdown-fontify-mode) mode)
                mode)))
    (if (or (null mode)
            (eq mode 'text-mode)
            (eq mode 'fundamental-mode)
            (not (fboundp mode)))
        text
      (condition-case _
          (mevedel-view--promote-face-to-font-lock-face
           (mevedel-view--with-render-temp-buffer
             (insert text)
             (funcall mode)
             (font-lock-ensure)
             (buffer-string)))
        (error text)))))

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
   ((memq (plist-get rendering :status)
          '(error failed blocked warning))
    'mevedel-view-tool-warning)
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
     `(keymap ,mevedel-view--agent-label-map
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

(defun mevedel-view--rendering-header-line (rendering)
  "Return the propertized collapsed header line for RENDERING."
  (let* ((header (or (plist-get rendering :header) "Tool"))
         (vtype (or (plist-get rendering :vtype) 'tool-summary))
         (status (plist-get rendering :agent-status))
         (tool-status (plist-get rendering :status))
         (agent-p (eq vtype 'agent-handle))
         (prompt-p (eq vtype 'prompt-summary))
         (marker (cond
                  ((eq vtype 'request-failure) "✗")
                  (prompt-p "◆")
                  ((and agent-p (eq status 'running)) "●")
                  ((and agent-p (memq status '(blocked waiting))) "!")
                  ((and agent-p (memq status '(aborted error failed))) "✗")
                  ((and agent-p (memq status '(incomplete nil))) "…")
                  ((and agent-p (eq status 'completed)) "✓")
                  (agent-p "›")
                  ((memq tool-status '(error failed blocked warning)) "!")
                  (t "✓")))
         (marker-face (cond
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
    (if (eq vtype 'system-reminder-summary)
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
        (if-let* ((agent-path (plist-get rendering :agent-path)))
            (mevedel-view--buttonize-agent-header-label line agent-path)
          line)))))

(defun mevedel-view--sandbox-summary-line (summary)
  "Return the durable disclosure line for material sandbox SUMMARY.
A `warning' class states something went wrong on this call.  A `note'
class restates the session's standing confinement configuration, which
already warned once when it was first hit, so it renders in the quiet
metadata face instead of shouting on every row."
  (when-let* ((class
               (and summary
                    (mevedel-execution-telemetry-sandbox-summary-class
                     summary)))
              (face (if (eq class 'note)
                        'mevedel-view-tool-metadata
                      'mevedel-view-tool-warning)))
    (let* ((attempts (or (plist-get summary :attempt-count) 0))
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
       "!"
       face
       "Sandbox:"
       (string-join details " · ")
       nil
       face))))

(defun mevedel-view--rendering-header-block (rendering)
  "Return RENDERING's header plus any durable sandbox disclosure."
  (let ((header (mevedel-view--rendering-header-line rendering))
        (sandbox
         (mevedel-view--sandbox-summary-line
          (plist-get rendering :sandbox-summary))))
    (if sandbox
        (concat header "\n" sandbox)
      header)))

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

(defun mevedel-view--child-call-rendering (child)
  "Return the rendering plist for nested call CHILD, or nil.

CHILD is one `:child-calls' entry.  The nested tool's own registered
renderer produces the row, which is why a nested Grep gets Grep's header
and `grep-mode' body without the compound tool formatting anything
itself.  A failed row renders expanded: its output is the reason the
reader opened the block.

A nested compound call keeps its own `:child-calls', so expanding it
inside a block or an activity group shows the calls it ran rather than
the text it returned."
  (let* ((name (plist-get child :tool))
         (args (plist-get child :args))
         (result (plist-get child :result))
         (render-data (plist-get child :render-data))
         (failed (not (eq (plist-get child :status) 'success)))
         (tool (and (stringp name) (mevedel-tool-get name)))
         (rendering
          (or (and tool
                   (mevedel-view--invoke-renderer tool render-data args result))
              (mevedel-view--generic-tool-rendering
               name args result nil render-data))))
    (when rendering
      (setq rendering (copy-sequence rendering))
      (dolist (cell (list (cons :vtype 'tool-child)
                          (cons :hidden-p nil)
                          (cons :expandable-p t)
                          (cons :coalesce-key nil)
                          (cons :hook-audits nil)
                          (cons :force-expanded-p nil)
                          (cons :initially-collapsed-p (not failed))))
        (setq rendering (plist-put rendering (car cell) (cdr cell))))
      (when failed
        (setq rendering (plist-put rendering :status 'error)))
      (when-let* ((media (plist-get child :media)))
        (setq rendering
              (plist-put rendering :body
                         (concat (or (plist-get rendering :body) "")
                                 (format "\nMedia: %S" media)))))
      rendering)))

(defun mevedel-view--child-call-state-key (child source)
  "Return the disclosure key for CHILD's row under SOURCE, or nil.
The child id discriminates rows that share one block's coordinates."
  (when-let* ((base (mevedel-view-disclosure-state-key source 'tool-child))
              (id (plist-get child :id)))
    (append base (list id))))

(defun mevedel-view--child-call-prefixes (children)
  "Return one line prefix per entry in CHILDREN, or nil.

A `:batch' value marks the calls of one concurrent join.  When some call
ran in a batch every row gets a two-column glyph gutter so the join is
visible as a group and every marker still lines up; a block that ran
everything in sequence pays no gutter at all."
  (let ((batches (mapcar (lambda (child) (plist-get child :batch)) children)))
    (when (delq nil (copy-sequence batches))
      (cl-loop
       for batch in batches
       for index from 0
       collect
       (concat
        mevedel-view--child-call-indent
        (if (null batch)
            "  "
          (let ((first (or (zerop index)
                           (not (equal batch (nth (1- index) batches)))))
                (last (not (equal batch (nth (1+ index) batches)))))
            (concat (cond ((and first last) "\u2500")
                          (first "\u250c")
                          (last "\u2514")
                          (t "\u251c"))
                    " "))))))))

(defun mevedel-view--insert-child-call-block
    (child source &optional collapsed indent)
  "Insert the nested call row for CHILD under SOURCE.

COLLAPSED is `derive' to use the remembered state, or a boolean to force
one.  INDENT overrides the row's line prefix, which is how a concurrent
batch keeps its glyph gutter across a toggle.  The row gets its own copy
of SOURCE's coordinates because section bounds compare source identity
with `eq', which is what separates one row from the next and from the
body of the block that ran them."
  (when-let* ((rendering (mevedel-view--child-call-rendering child))
              (source (and (consp source) (cons (car source) (cdr source)))))
    (let* ((key (mevedel-view--child-call-state-key child source))
           (remembered (and (eq collapsed 'derive)
                            (mevedel-view-disclosure-state-for-key key)))
           (collapsed (cond
                       (remembered (cdr remembered))
                       ((eq collapsed 'derive)
                        (plist-get rendering :initially-collapsed-p))
                       (t collapsed)))
           (indent (or indent mevedel-view--child-call-indent))
           (mevedel-view--rendering-indent
            (concat mevedel-view--rendering-indent indent))
           (start (point)))
      (if collapsed
          (mevedel-view--render-collapsed-header rendering source)
        (mevedel-view--render-expanded-body rendering source))
      (add-text-properties start (point)
                           `(mevedel-view-tool-child ,child
                             mevedel-view-child-indent ,indent
                             mevedel-view-source-key ,key))
      (mevedel-view-render-add-display-properties start (point) 'tool-child)
      (mevedel-view-disclosure-record-state-for-key key collapsed))))

(defun mevedel-view--insert-child-calls (rendering source)
  "Insert one row per nested call in RENDERING under SOURCE."
  (let* ((children (plist-get rendering :child-calls))
         (prefixes (mevedel-view--child-call-prefixes children)))
    (cl-loop for child in children
             for index from 0
             do (mevedel-view--insert-child-call-block
                 child source 'derive (nth index prefixes)))))

(defun mevedel-view-render-child-calls-end (start limit)
  "Return the end of the nested call rows that begin at START, before LIMIT.
Return START when no row begins there.  Nested rows always follow the
body of the block that ran them, so a run starting at START belongs to
the section that ends there."
  (let ((pos start))
    (while (and (< pos limit)
                (eq (get-text-property pos 'mevedel-view-type) 'tool-child))
      (setq pos (or (next-single-property-change
                     pos 'mevedel-view-type nil limit)
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
         (turn-id (and start (get-text-property start 'mevedel-view-turn-id))))
    (unless (and bounds child)
      (user-error "No collapsible section at point"))
    (let ((inhibit-read-only t))
      (save-excursion
        (goto-char start)
        (delete-region start (cdr bounds))
        (mevedel-view--insert-child-call-block
         child source (not collapsed) indent)
        (when turn-id
          (put-text-property start (point) 'mevedel-view-turn-id turn-id))))))


(defun mevedel-view--tool-cache-key
    (data-buf seg-start seg-end collapsed-only raw)
  "Return a cache key for DATA-BUF SEG-START..SEG-END rendering.
RAW is the expanded tool segment text used for content-based invalidation.
COLLAPSED-ONLY records whether only collapsed rendering is needed.
Unrelated appends to DATA-BUF should not invalidate completed tool segment
renderings, but changes to the segment text itself should."
  (with-current-buffer data-buf
    (list data-buf
          (mevedel-view--source-position seg-start)
          (mevedel-view--source-position seg-end)
          (mevedel-view--render-cache-key raw)
          (and (boundp 'mevedel--session)
               (mevedel-view--session-render-state-fingerprint mevedel--session))
          (and collapsed-only t))))

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

(defun mevedel-view--compute-segment-rendering
    (data-buf seg-start seg-end &optional collapsed-only raw)
  "Compute rendering for DATA-BUF SEG-START..SEG-END.
When COLLAPSED-ONLY is non-nil and the result initially renders collapsed,
omit its body so large tool outputs are not retained in the collapsed cache.
RAW is an optional precomputed expanded tool segment text."
  (when-let* ((call (mevedel-view--tool-call-parse
                     data-buf seg-start seg-end raw)))
    (let* ((name (plist-get call :name))
           (args (plist-get call :args))
           (tool-use-id (plist-get call :tool-use-id))
           (event
            (and (bound-and-true-p mevedel-view--execution-events)
                 (hash-table-p mevedel-view--execution-events)
                 (gethash tool-use-id mevedel-view--execution-events)))
           (terminal-render-data
            (and (equal name "Bash")
                 (buffer-live-p data-buf)
                 (mevedel-execution-transcript-pending-render-data
                  data-buf tool-use-id)))
           (event-type (plist-get event :type))
           (call-render-data (plist-get call :render-data))
           (result
            (cond
             ((eq event-type 'progress)
              (or (plist-get event :output-tail) ""))
             ((plist-member call-render-data :execution-output)
              (or (plist-get call-render-data :execution-output) ""))
             ((plist-member terminal-render-data :execution-output)
              (or (plist-get terminal-render-data :execution-output) ""))
             (t (plist-get call :result))))
           (render-data
            (if (eq event-type 'progress)
                (let ((facts (copy-sequence (plist-get event :facts))))
                  (append
                   facts
                   (list :status 'success
                         :live-execution-p t)))
              (or terminal-render-data call-render-data)))
           (tool (mevedel-tool-get name))
           (custom (and tool
                        (mevedel-view--invoke-renderer
                         tool
                         render-data
                         args
                         result)))
           (rendering (or custom
                          (mevedel-view--generic-tool-rendering
                           name args result collapsed-only render-data))))
      (when-let* (((eq (plist-get rendering :vtype) 'agent-handle))
                  (path (plist-get render-data :path)))
        (setq rendering (plist-put rendering :agent-path path)))
      (when-let* ((summary (plist-get render-data :sandbox-summary)))
        (setq rendering
              (plist-put rendering :sandbox-summary (copy-tree summary))))
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
            (plist-put
             rendering :group-child
             (list :tool name :args args :result result :render-data render-data
                   :status (mevedel-view--tool-render-status
                            result render-data))))
      (if collapsed-only
          (mevedel-view--omit-rendering-body-for-cache rendering)
        rendering))))

(defun mevedel-view--segment-rendering (data-buf seg-start seg-end
                                                 &optional collapsed-only)
  "Return rendering for DATA-BUF's SEG-START..SEG-END.
Provider-failure request summaries and tool segments are renderable.
Return nil only when the segment is malformed or unparseable.
Registered renderers get first chance; otherwise a generic rendering
keeps parseable tool calls from expanding into raw org scaffolding.
When COLLAPSED-ONLY is non-nil, cache a header rendering that omits large
bodies for initially collapsed tools."
  (let* ((segment-text
          (with-current-buffer data-buf
            (buffer-substring seg-start seg-end)))
         (request-data
          (mevedel-view--request-summary-render-data-from-text segment-text))
         (failure-p (eq (plist-get request-data :outcome) 'error))
         (raw (and (not failure-p)
                   (with-current-buffer data-buf
                     (mevedel-view--tool-segment-text seg-start seg-end))))
         (cache (and (hash-table-p mevedel-view--tool-rendering-cache)
                     mevedel-view--tool-rendering-cache))
         (key (and raw cache
                   (mevedel-view--tool-cache-key
                    data-buf seg-start seg-end collapsed-only raw))))
    (if failure-p
        (let ((backend (or (plist-get request-data :backend) "Provider"))
              (status (plist-get request-data :status))
              (type (plist-get request-data :error-type))
              (code (plist-get request-data :error-code))
              (error-data (plist-get request-data :error-data))
              (message-text (plist-get request-data :message)))
          (list
           :header (concat backend " request failed"
                           (if type (format " · %s" type) ""))
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
                        ""
                        "Retry the request manually."))
            "\n")
           :body-mode 'text-mode
           :vtype 'request-failure
           :status 'error
           :initially-collapsed-p nil))
      (or (and key (gethash key cache))
          (let ((rendering (mevedel-view--compute-segment-rendering
                            data-buf seg-start seg-end collapsed-only raw)))
            (when (and key rendering)
              (mevedel-view--cache-put
               cache key
               (plist-put (copy-sequence rendering) :group-child nil)
               'mevedel-view--render-cache-entries))
            rendering)))))

(defun mevedel-view--tool-row-region (data-buffer tool-use-id)
  "Return the visible source-backed row for TOOL-USE-ID in DATA-BUFFER.
The result is `(VIEW-START VIEW-END SOURCE-BOUNDS)' or nil."
  (when-let* ((bounds
               (with-current-buffer data-buffer
                 (mevedel-tool-render-data-segment-bounds tool-use-id))))
    (let ((pos (point-min))
          (limit (point-max))
          found)
      (while (and (< pos limit) (not found))
        (let* ((source (get-text-property pos 'mevedel-view-source))
               (next (or (next-single-property-change
                          pos 'mevedel-view-tool-use-id nil limit)
                         limit)))
          (when (and (consp source)
                     (eq (get-text-property pos 'mevedel-view-type)
                         'tool-summary)
                     (equal
                      (get-text-property pos 'mevedel-view-tool-use-id)
                      tool-use-id))
            (setq found (list pos next bounds)))
          (setq pos next)))
      found)))

(defun mevedel-view--refresh-tool-row (data-buffer tool-use-id)
  "Refresh only TOOL-USE-ID's visible row from DATA-BUFFER."
  (when-let ((region
              (mevedel-view--tool-row-region data-buffer tool-use-id)))
    (let* ((start (nth 0 region))
           (end (nth 1 region))
           (source (nth 2 region))
           (collapsed (get-text-property start 'mevedel-view-collapsed))
           (previously-forced-p
            (get-text-property start 'mevedel-view-force-expanded))
           (turn-id (get-text-property start 'mevedel-view-turn-id)))
      (when (hash-table-p mevedel-view--tool-rendering-cache)
        (clrhash mevedel-view--tool-rendering-cache))
      (when-let ((rendering
                  (mevedel-view--segment-rendering
                   data-buffer (car source) (cdr source))))
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
                       'mevedel-view-turn-id turn-id)))))))))
        t))))


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
  (with-temp-buffer
    (insert (or text ""))
    (goto-char (point-min))
    (while (re-search-forward "^#\\+begin_tool\\b" nil t)
      (let ((start (match-beginning 0)))
        (if (re-search-forward "^#\\+end_tool[^\n]*\n?" nil t)
            (delete-region start (point))
          (delete-region start (point-max)))))
    (buffer-string)))

(defconst mevedel-view--clean-reasoning-cache-size 24
  "Reasoning-text cleanings retained by `mevedel-view--clean-reasoning-cache'.

Sized for the completed reasoning blocks a transcript re-renders, not for
history: each entry holds a whole cleaned block, and a miss costs only the
work this cache exists to skip.")

(defvar-local mevedel-view--clean-reasoning-cache nil
  "Cleaned reasoning text, keyed by the raw text it was produced from.

`mevedel-view--clean-reasoning-text' is a pure function of its argument, so
equal input gives equal output and the raw text is a sound key.

It earns its place on redraw.  A render walks every turn in the transcript,
so each tick re-cleans every completed reasoning block as well as the live
one -- four passes over each, allocating a fresh copy per pass.  Completed
blocks do not change, so they hit; the streaming block grows on every tick
and always misses, which is the one case the cache cannot help and does not
try to.")

(defun mevedel-view--clean-reasoning-cached (text)
  "Return TEXT's cleaning from the buffer cache, or nil when absent."
  (when mevedel-view--clean-reasoning-cache
    (cdr (assoc text mevedel-view--clean-reasoning-cache))))

(defun mevedel-view--clean-reasoning-remember (text cleaned)
  "Record CLEANED as TEXT's cleaning, evicting the oldest entry."
  (setq mevedel-view--clean-reasoning-cache
        (cons (cons text cleaned)
              (let ((kept (seq-take mevedel-view--clean-reasoning-cache
                                    (1- mevedel-view--clean-reasoning-cache-size))))
                (assoc-delete-all text kept))))
  cleaned)

(defun mevedel-view--clean-reasoning-text (text)
  "Strip org scaffolding markers from reasoning TEXT.
Removes reasoning block markers, nested tool blocks, and generated
system reminder wrappers.

Cached per buffer; see `mevedel-view--clean-reasoning-cache'."
  (or (mevedel-view--clean-reasoning-cached text)
      (mevedel-view--clean-reasoning-remember
       text (mevedel-view--clean-reasoning-text-1 text))))

(defun mevedel-view--clean-reasoning-text-1 (text)
  "Return TEXT with reasoning scaffolding removed, without consulting a cache."
  (let ((cleaned (mevedel-view--strip-render-data-display-text text)))
    (setq cleaned (mevedel-view--strip-system-reminder-blocks cleaned))
    (setq cleaned (mevedel-view--strip-tool-blocks-from-reasoning cleaned))
    ;; One pass over the marker lines rather than three.  Each
    ;; `replace-regexp-in-string' copies the whole string, and reasoning
    ;; text runs to thousands of lines while every redraw re-cleans it.
    (replace-regexp-in-string
     "#\\+\\(?:begin\\|end\\)_\\(?:reasoning\\|tool\\)[^\n]*\n?" "" cleaned)))

(defun mevedel-view--strip-render-data-display-text (text)
  "Return TEXT without hidden render-data side-channel scaffolding."
  (mevedel-tool-render-data-strip (or text "")))

(defun mevedel-view--render-data-only-text-p (text)
  "Return non-nil if TEXT is only render-data scaffolding."
  (and (stringp text)
       (not (string-empty-p (string-trim text)))
       (string-empty-p
        (string-trim
         (mevedel-view--strip-render-data-display-text text)))))

(defun mevedel-view--hook-audit-only-text-p (text)
  "Return non-nil if TEXT is only hook audit scaffolding."
  (mevedel-transcript-audit-only-p text))

(defun mevedel-view--hook-audit-only-segment-p (data-buf seg-start seg-end)
  "Return non-nil when DATA-BUF's SEG-START..SEG-END is only hook audit data."
  (with-current-buffer data-buf
    (mevedel-view--hook-audit-only-text-p
     (buffer-substring seg-start seg-end))))

(defun mevedel-view--system-reminder-body-from-text (text)
  "Return generated system reminder body from TEXT, or nil.
TEXT must contain only one complete `<system-reminder>' block plus
surrounding whitespace.  Embedded literal examples are not treated
as generated control markup."
  (when (stringp text)
    (with-temp-buffer
      (insert (string-trim text))
      (goto-char (point-min))
      (when (looking-at "<system-reminder>")
        (let ((body-start (match-end 0)))
          (goto-char body-start)
          (when (search-forward "</system-reminder>" nil t)
            (let ((body-end (match-beginning 0)))
              (skip-chars-forward " \t\r\n")
              (when (= (point) (point-max))
                (string-trim
                 (buffer-substring-no-properties body-start body-end))))))))))

(defun mevedel-view--strip-system-reminder-blocks (text)
  "Return TEXT without generated `<system-reminder>' blocks."
  (with-temp-buffer
    (insert (or text ""))
    (goto-char (point-min))
    (while (search-forward "<system-reminder>" nil t)
      (let ((start (match-beginning 0)))
        (if (search-forward "</system-reminder>" nil t)
            (progn
              (delete-region start (point))
              (goto-char start))
          (goto-char (point-max)))))
    (buffer-string)))

(defun mevedel-view--system-reminder-line-count (body)
  "Return the non-empty line count for system reminder BODY."
  (length (split-string (or body "") "\n" t "[ \t]+")))

(defun mevedel-view--partial-worktree-fork-reminder-p (body)
  "Return non-nil when BODY discloses a partial Worktree Fork restore."
  (and (stringp body)
       (string-prefix-p "Worktree Fork (partial restoration)" body)))

(defun mevedel-view--system-reminder-summary (data-buf seg-start seg-end)
  "Return collapsed summary for DATA-BUF's SEG-START..SEG-END system reminder."
  (with-current-buffer data-buf
    (let* ((text (buffer-substring-no-properties seg-start seg-end))
           (body (mevedel-view--system-reminder-body-from-text text))
           (lines (max 1 (mevedel-view--system-reminder-line-count body)))
           (partial
            (mevedel-view--partial-worktree-fork-reminder-p body)))
      (propertize
       (format (if partial
                   "  ! Partial Worktree Fork (%d %s)"
                 "  \u25c7 System reminder (%d %s)")
               lines
               (if (= lines 1) "line" "lines"))
       'font-lock-face
       (if partial
           'mevedel-view-tool-warning
         'mevedel-view-system-reminder)))))

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
        (mevedel-view--scaffolding-only-text-p
         (and (< s e) (buffer-substring-no-properties s e)))))))

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

Generous because a verdict is one boolean.  The cleaned text it is derived
from is not retained here, so this bound costs almost nothing while
covering every segment a long transcript re-examines.")

(defvar-local mevedel-view--scaffolding-only-cache nil
  "Scaffolding verdicts, keyed by the segment text they were derived from.

A render walks every segment and asks each one whether it is glue, and the
answer came from cleaning the whole segment and testing the result for
emptiness -- four passes allocating a fresh copy each.  That was 81% of all
reasoning-cleaning allocation in a profiled remote turn, and it recurs on
every redraw because the segments do not change.

Kept apart from `mevedel-view--clean-reasoning-cache' precisely because the
values are cheap: a shared bound sized for whole cleaned blocks was small
enough that a long transcript evicted its own entries before reusing them.")

(defun mevedel-view--scaffolding-only-text-p (text)
  "Return non-nil if TEXT is org-only glue, or nil.
Nil TEXT counts as glue.  See `mevedel-view--scaffolding-only-p'.

Cached per buffer; see `mevedel-view--scaffolding-only-cache'."
  (if (null text)
      t
    (unless mevedel-view--scaffolding-only-cache
      (setq mevedel-view--scaffolding-only-cache
            (make-hash-table :test #'equal)))
    (let ((cached (gethash text mevedel-view--scaffolding-only-cache 'miss)))
      (if (eq cached 'miss)
          (progn
            ;; Cleared rather than evicted one by one: recomputing a
            ;; verdict is the work this cache already exists to skip, and
            ;; a transcript long enough to overflow re-examines its oldest
            ;; segments least.
            (when (>= (hash-table-count mevedel-view--scaffolding-only-cache)
                      mevedel-view--scaffolding-only-cache-size)
              (clrhash mevedel-view--scaffolding-only-cache))
            (puthash text
                     (string-empty-p
                      (string-trim (mevedel-view--clean-reasoning-text text)))
                     mevedel-view--scaffolding-only-cache))
        cached))))

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
              (let ((block-start (match-beginning 0)))
                (when-let* ((close (search-forward
                                    "<!-- /mevedel-render-data -->"
                                    limit t)))
                  (when (mevedel-view--request-summary-render-data-from-text
                         (buffer-substring block-start close))
                    (let ((delete-start
                           (if (and (> block-start (point-min))
                                    (eq (char-before block-start) ?\n))
                               (1- block-start)
                             block-start))
                          (delete-end
                           (if (and (< close (point-max))
                                    (eq (char-after close) ?\n))
                               (1+ close)
                             close)))
                      (delete-region delete-start delete-end)
                      (goto-char delete-start))))))
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
             (mevedel-view--strip-review-action-blocks text)))))))

(defun mevedel-view--strip-review-action-blocks (text)
  "Return TEXT without synthetic review `<user_action>' blocks.
The review module is always loaded with mevedel, so its stripper is the
one implementation; a second copy here had already started to drift."
  (mevedel-review-strip-user-action-blocks text))

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
      (let* ((text (buffer-substring-no-properties seg-start seg-end))
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
  "Return the first history position after the session header."
  (save-excursion
    (goto-char (point-min))
    (forward-line 1)
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
         (limit (point-max))
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

(defun mevedel-view--pending-tool-insertion-target ()
  "Return where pending tool live-tail lines should be inserted.

The normal target is the live history/status boundary, or a dynamic
render insertion marker that still points above the composer.  If marker
teardown detached the status marker, recover to the current rendered
history tail so pending live-tail text stays above status/interaction
content without moving ahead of prior transcript turns."
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

(defun mevedel-view--pending-tool-line-body (label)
  "Return the propertized fragment body for pending tool LABEL."
  (let ((frame (mevedel-view--spinner-frame)))
    (concat
     (propertize frame
                 'font-lock-face 'mevedel-view-ephemeral
                 'mevedel-view-inline-spinner-frame t
                 'mevedel-view-pending-tool-live t
                 'display frame)
     (propertize (format " %s\n" label)
                 'font-lock-face 'mevedel-view-ephemeral
                 'mevedel-view-pending-tool-live t))))

(defun mevedel-view--pending-tool-fragments (entries)
  "Return live-tail fragments for pending tool ENTRIES."
  (let ((cap mevedel-view-pending-tools-visible-max)
        (total (length mevedel-view--pending-tool-calls))
        fragments)
    (dolist (entry entries)
      (push (list :namespace 'history-live
                  :id (car entry)
                  :body (mevedel-view--pending-tool-line-body (cdr entry)))
            fragments))
    (when (> total cap)
      (push (list :namespace 'history-live
                  :id :pending-tool-overflow
                  :body (mevedel-view--pending-tool-line-body
                         (format "%d more tools running…" (- total cap))))
            fragments))
    (nreverse fragments)))

(defun mevedel-view--strip-history-live-fragments-from-string (text)
  "Return TEXT without fragment-backed live history rows."
  (when text
    (with-temp-buffer
      (insert text)
      (let ((inhibit-read-only t)
            (inhibit-modification-hooks t)
            (pos (point-min)))
        (while (setq pos (text-property-any
                          pos (point-max)
                          'mevedel-view-zone-namespace 'history-live))
          (let ((end (or (next-single-property-change
                          pos 'mevedel-view-zone-namespace nil (point-max))
                         (point-max))))
            (delete-region pos end))))
      (let ((str (buffer-string)))
        (unless (string-empty-p str)
          str)))))

(defun mevedel-view--request-progress-anchor ()
  "Return where the foreground request progress row should be inserted.
Request progress lives after status and interaction rows, directly
before the input zone.  Fall back to the pending-tool target only when
the input boundary cannot be recovered."
  (or (let ((input-pos (and (markerp mevedel-view--input-marker)
                            (marker-buffer mevedel-view--input-marker)
                            (eq (marker-buffer mevedel-view--input-marker)
                                (current-buffer))
                            (mevedel-view--input-marker-position))))
        (and input-pos
             (<= (point-min) input-pos)
             (<= input-pos (point-max))
             input-pos))
      (mevedel-view--pending-tool-insertion-target)))

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
    (let ((pos history-start)
          first-source)
      (while (and (< pos history-end) (not first-source))
        (let ((source (get-text-property pos 'mevedel-view-source)))
          (when (and (consp source)
                     (integer-or-marker-p (car source))
                     (>= (mevedel-view-disclosure-source-start source)
                         data-from))
            (setq first-source pos)))
        (setq pos (1+ pos)))
      (when first-source
        (let ((scan (1- first-source))
              header)
          (while (and (>= scan history-start) (not header))
            (when (eq (get-text-property scan 'mevedel-view-type)
                      'turn-header)
              (setq header scan))
            (setq scan (1- scan)))
          (or (and header
                   (progn
                     (while (and (> header history-start)
                                 (eq (get-text-property (1- header)
                                                        'mevedel-view-type)
                                     'turn-header))
                       (setq header (1- header)))
                     header))
              first-source))))))

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

When `mevedel-view--pending-tool-calls' is non-empty, reconciles one
fragment-backed \"Calling TOOLNAME…\" history live-tail row per in-flight
tool (capped by `mevedel-view-pending-tools-visible-max') so the user
sees what's running even before results land in the data buffer.

Assistant streaming remains data-buffer-derived full/incremental turn
rerendering here: transcript parsing, collapse recovery, and final
response reconciliation still need the authoritative data buffer.  The
fragment migration is limited to ephemeral pending-tool live-tail rows.

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
  (let* ((retained-p (and (not settle-p)
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
              (inhibit-modification-hooks t))
          (when pending
            (mevedel-view--delete-pending-tool-live-lines))
          (let* (;; Permission prompts and tool callbacks can trigger a view
                 ;; refresh in the small window after pending tool lines have
                 ;; been removed but before gptel has written the corresponding
                 ;; result segments.  In that case, keep the previous in-flight
                 ;; rendering instead of replacing it with a blank region.
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
                 (recovered-start
                  (mevedel-view--recover-in-flight-turn-start
                   data-from history-start rebuild-end-pos))
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
                  (continuation-p retained-p))
              (setq render-start (marker-position rebuild-end))
              (dolist (turn turns)
                (mevedel-view--render-turn
                 turn data-buf nil nil
                 (prog1 continuation-p
                   (setq continuation-p nil))))
              (mevedel-view--ensure-request-progress data-buf)
              (when pending
                (let* ((cap mevedel-view-pending-tools-visible-max)
                       (visible (cl-subseq pending 0 (min cap (length pending)))))
                  (mevedel-view--insert-pending-tool-lines visible))))
            (mevedel-view--debug-log
             'incremental-after-insert
             :state (mevedel-view--debug-state data-buf data-from data-to)))
          ;; Restore user-toggled collapse/expand state that the delete
          ;; above just wiped.  Walk the freshly rendered span and toggle
          ;; only sections whose saved state differs from the default.
          (let ((restore-toggled
                 (when (and saved-states
                            delete-start
                            rebuild-end
                            (marker-position rebuild-end))
                   (mevedel-view-disclosure-restore-state
                    delete-start
                    (marker-position rebuild-end)
                    saved-states))))
            ;; Retain the live tail only after the restore above, and
            ;; only when the restore rewrote nothing: a restored toggle
            ;; splits the freshly marked render unit, leaving the
            ;; retained view marker mid-unit relative to its data-buffer
            ;; twin, and the next retained render then deletes or
            ;; duplicates content its narrowed reparse cannot
            ;; regenerate.  Skipping retention costs one full-turn
            ;; reparse per tick exactly while a live-tail section holds
            ;; non-default state.
            (when replace-p
              (if (or settle-p restore-toggled)
                  (mevedel-view-render-invalidate-live-tail)
                (mevedel-view--retain-last-live-render-unit
                 data-buf render-start (marker-position rebuild-end)))))
          (mevedel-view--ensure-request-progress data-buf)
          (unless mevedel-view--agent-transcript-p
            (mevedel-view--render-agent-status)
            (mevedel-view--interaction-rebuild)
            (when (fboundp 'mevedel-directive-frame-refresh-filter)
              (mevedel-directive-frame-refresh-filter))))))))))

(defun mevedel-view-render-live-update (data-buf)
  "Update the current in-flight turn from DATA-BUF's mutable tail."
  (mevedel-view--render-live-region data-buf nil))

(defun mevedel-view-render-settle (data-buf start end)
  "Exactly reconcile DATA-BUF's completed response from START through END.
Disclosure state keys are computed with their durable post-settle
anchors: this render runs before the stream clears the in-flight turn
markers, and keys captured or stamped under the temporary `(in-flight)'
anchor here would be orphaned the moment those markers clear, collapsing
sections the user expanded during the turn on the next render."
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
            ;; Blank line above the trailing separator so the rule doesn't
            ;; butt up against the last response line.
            (when (eq role 'assistant)
              (mevedel-view--ensure-blank-line-before-response))
            ;; Trailing separator -- horizontal rule after assistant turns,
            ;; plain spacer after user turns.
            (insert (propertize "\n"
                                'font-lock-face
                                (if (eq role 'assistant)
                                    'mevedel-view-turn-rule
                                  'mevedel-view-separator)))
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
             `(mevedel-view-turn-id
               ,(or (and continuation-p
                         (> insert-start (point-min))
                         (get-text-property
                          (1- insert-start) 'mevedel-view-turn-id))
                    (plist-get turn :render-id)
                    (cl-gensym "mevedel-view-turn-"))
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
              ;; Strip org heading prefix (e.g., "*** ")
              (when (string-match "\\`\\*+ " text)
                (setq text (substring text (match-end 0))))
              ;; Strip hidden view render-data side channels.
              (setq text (mevedel-view--strip-render-data-display-text text))
              ;; Strip hidden hook audit side channels; they render as separate
              ;; disclosures below the affected transcript artifact.
              (setq text (mevedel--strip-hook-audit-blocks text))
              ;; Strip synthetic review action blocks.  They stay in the data
              ;; buffer so the model can resolve follow-ups like "fix finding 2",
              ;; but the normal view should show only the user's visible prompt.
              (setq text (mevedel-view--strip-review-action-blocks text))
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
              ;; Strip reasoning block markers
              (setq text (replace-regexp-in-string
                          "#\\+\\(?:begin\\|end\\)_reasoning[^\n]*\n?" "" text))
              ;; Strip tool block markers.  gptel emits `#+begin_tool ...'
              ;; and `#+end_tool' without the `gptel' text property, so the
              ;; separator text around a tool block appears here as a user
              ;; segment -- skip it, otherwise the raw header would render
              ;; as a spurious "You" turn.
              (setq text (replace-regexp-in-string
                          "#\\+begin_tool[^\n]*\n?" "" text))
              (setq text (replace-regexp-in-string
                          "#\\+end_tool[^\n]*\n?" "" text))
              (let ((trimmed (string-trim text)))
                (unless (string-empty-p trimmed)
                  (push trimmed parts)))))
          (string-join (nreverse parts) "\n")))))

(defun mevedel-view--strip-model-context-blocks (text)
  "Return TEXT without generated hook or Goal context blocks."
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
    (buffer-string)))

(autoload 'mevedel-view--hook-audit-records-from-text "mevedel-view-audit")
(autoload 'mevedel-view--hook-audit-key "mevedel-view-audit")
(autoload 'mevedel-view--merge-hook-audits "mevedel-view-audit")
(autoload 'mevedel-view--indent-hook-audit-text "mevedel-view-audit")
(autoload 'mevedel-view--hook-audit-value-text "mevedel-view-audit")
(autoload 'mevedel-view--prompt-rewrite-audit-record "mevedel-view-audit")
(autoload 'mevedel-view--user-turn-hook-audits "mevedel-view-audit")
(autoload 'mevedel-view--format-hook-audit-block "mevedel-view-audit")
(autoload 'mevedel-view--insert-hook-audit-block "mevedel-view-audit")
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
              (body (plist-get data :expanded-prompt))
              ((not (string-empty-p body))))
    body))

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
                "<hook-event[ \t\n]+name=\"\\([^\"]+\\)\">" nil t)
          (let ((event (mevedel-view--hook-context-unescape
                        (match-string 1)))
                (body-start (point)))
            (when (search-forward "</hook-event>" nil t)
              (let ((event-body
                     (mevedel-view--hook-context-event-body
                      (buffer-substring-no-properties
                       body-start (match-beginning 0)))))
                (unless (string-empty-p event-body)
                  (push (list :event event :body event-body) events)))))))
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
                          (list :event (format "%s"
                                               (or (plist-get entry :event)
                                                   "UserPromptSubmit"))
                                :body body)))))
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
            (concat
             "    " (plist-get entry :event) "\n"
             (mapconcat (lambda (line) (concat "    " line))
                        (split-string body "\n")
                        "\n")
             "\n")))
        events
        "")))))

(defun mevedel-view--insert-hook-context-block
    (events &optional source expanded)
  "Insert a hook context disclosure for EVENTS.
SOURCE, when non-nil, is the source range in the data buffer.
EXPANDED means insert the disclosure body expanded."
  (when-let* ((events (mevedel-view--normalize-hook-context-events events)))
    (let ((start (point))
          (id (cl-gensym "mevedel-hook-context-")))
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

(defun mevedel-view--hook-context-section-bounds ()
  "Return bounds of the hook context disclosure at point, or nil."
  (let ((id (get-text-property (point) 'mevedel-view-hook-context-id)))
    (when id
      (let ((start (or (previous-single-property-change
                        (point) 'mevedel-view-hook-context-id)
                       (point-min)))
            (end (or (next-single-property-change
                      (point) 'mevedel-view-hook-context-id)
                     (point-max))))
        (when (and (< start (point))
                   (not (eq (get-text-property
                             start 'mevedel-view-hook-context-id)
                            id)))
          (setq start (or (next-single-property-change
                           start 'mevedel-view-hook-context-id)
                          (point))))
        (cons start end)))))

(defun mevedel-view-render-toggle-hook-context ()
  "Toggle a hook context disclosure."
  (let* ((bounds (or (mevedel-view--hook-context-section-bounds)
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
              (when-let* ((data
                           (mevedel-view--inline-skill-render-data-from-text
                            text)))
                (setq info
                      (plist-put
                       data :source
                       (mevedel-view-disclosure-source-range
                        data-buf (cadr seg) (caddr seg))))))
            (setq hook-audits
                  (append hook-audits
                          (mevedel-view--hook-audit-records-from-text text))))))
      (when info
        (plist-put info :hook-audits hook-audits)))))

(defun mevedel-view--style-mailbox-body (start end)
  "Style mailbox body text between START and END.
Adds a small gutter to payload lines and returns the adjusted end
position after insertions.  Leading/trailing blank structural lines
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
                 (insert (propertize "    │ "
                                     'font-lock-face
                                     'mevedel-view-mailbox-gutter))
                 (when (cdr line)
                   (put-text-property
                    (point) (line-end-position)
                    'font-lock-face 'mevedel-view-mailbox-body)))))
    (dolist (line lines)
      (set-marker (car line) nil))
    (prog1 (marker-position end-marker)
      (set-marker end-marker nil))))

(defun mevedel-view--bash-completion-summary (text)
  "Return compact visible execution facts from Bash completion TEXT."
  (when (string-match "<bash-execution[^<>]*?/>[[:space:]]*\\'" text)
    (condition-case nil
        (with-temp-buffer
          (insert (match-string 0 text))
          (let* ((attributes (cadr (car (xml-parse-region
                                         (point-min) (point-max)))))
                 (id (alist-get 'execution_id attributes))
                 (exit-code (alist-get 'exit_code attributes))
                 (outcome (alist-get 'outcome attributes))
                 (termination (alist-get 'termination attributes))
                 (wall-time (alist-get 'wall_time_seconds attributes))
                 (lines (alist-get 'output_lines attributes))
                 (bytes (alist-get 'output_bytes attributes)))
            (and id
                 (string-join
                  (delq nil
                        (list id
                              outcome
                              termination
                              (and exit-code (format "exit %s" exit-code))
                              (and wall-time
                                   (format "%.1fs"
                                           (string-to-number wall-time)))
                              (and lines (format "%s lines" lines))
                              (and bytes (format "%s bytes" bytes))))
                  " · "))))
      (error nil))))

(defun mevedel-view--decorate-mailbox-block
    (open-regex close-tag start end &optional kind)
  "Replace OPEN-REGEX/CLOSE-TAG regions from START to END with mailbox cards.
KIND identifies the mailbox block flavor.  Shared engine for
`<agent-message>' and `<agent-result>'
rendering.  OPEN-REGEX must capture the canonical sender path in match group
1.  Ordinary bodies between the matched open and close tags are preserved
verbatim; root EXECUTION bodies are summarized from their trailing facts.
If a body's line count exceeds CLOSE-TAG's threshold,
`mevedel-view-mailbox-collapse-line-threshold', the body is marked
invisible (with the `mailbox-delivery' vtype tag for downstream
TAB-toggle wiring) and the header gets a `[N lines collapsed]'
hint.  Searches that region."
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
                     (execution-p
                      (and (eq kind 'agent-message)
                           (string-match-p
                            "\\_<type=\"EXECUTION\""
                            (match-string-no-properties 0))))
                     (sender (match-string-no-properties 1))
                     (bash-summary
                      (and execution-p
                           (equal sender "/root")
                           (save-excursion
                             (goto-char open-end)
                             (when-let* ((close
                                          (mevedel-transcript--mailbox-find-close
                                           open-regex close-tag
                                           (marker-position end-marker))))
                               (mevedel-view--bash-completion-summary
                                (buffer-substring-no-properties
                                 open-end (car close)))))))
                     (attribution (mevedel-view--insert-attribution sender))
                     (inhibit-read-only t))
                (delete-region open-start open-end)
                (goto-char open-start)
                (let ((card-start (point))
                      (card-id (cl-gensym "mevedel-view-mailbox-")))
                  (insert "  ")
                  (insert (propertize
                           (cond
                            (bash-summary "✓ Bash completed · ")
                            ((eq kind 'agent-result) "✓ Finished ")
                            (t "✉ message "))
                           'font-lock-face 'mevedel-view-attribution
                           'mevedel-view-mailbox t))
                  ;; The bash and result cards name their sender
                  ;; directly; only a plain message reads as "from
                  ;; PATH".  All three take the attribution string, so
                  ;; the sender keeps its face and click target -- the
                  ;; bash card used to insert it bare.
                  (if (or bash-summary (eq kind 'agent-result))
                      (let ((label-start (point)))
                        (insert attribution)
                        (when (string-prefix-p "from " attribution)
                          (delete-region label-start
                                         (+ label-start (length "from ")))))
                    (insert attribution))
                  (insert "\n")
                  (let ((body-start (point)))
                    (when-let* ((close
                                 (mevedel-transcript--mailbox-find-close
                                  open-regex close-tag
                                  (marker-position end-marker))))
                      (when bash-summary
                        (let ((old-end (car close)))
                          (delete-region body-start old-end)
                          (goto-char body-start)
                          (insert bash-summary "\n")
                          (setcdr close (+ (cdr close) (- (point) old-end)))
                          (setcar close (point))))
                      (let* ((body-end (car close))
                             (close-end (cdr close))
                             (body-line-count
                              (mevedel-view-disclosure-mailbox-line-count
                               body-start body-end))
                             (long-body
                              (> body-line-count
                                 mevedel-view-mailbox-collapse-line-threshold)))
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
                                   (mevedel-view-disclosure-mailbox-hint
                                    body-line-count)
                                   'font-lock-face
                                   'mevedel-view-attribution
                                   'mevedel-view-mailbox-hint t))
                                 (hint-len (length hint)))
                            (goto-char body-start)
                            (when (eq (char-before) ?\n)
                              (backward-char))
                            (insert hint)
                            (setq body-start (+ body-start hint-len)
                                  body-end (+ body-end hint-len)
                                  close-end (+ close-end hint-len)))
                          (add-text-properties
                           body-start body-end
                           (list 'invisible 'mevedel-view-mailbox-collapsed
                                 'mevedel-view-mailbox-body t
                                 'mevedel-view-type 'mailbox-delivery
                                 'mevedel-view-collapsed t)))
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
                               'mevedel-view-collapsed long-body)))))))))
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
remains as ordinary user text."
  (mevedel-view--decorate-mailbox-block
   "<agent-message\\s-+[^>]*sender=\"\\([^\"]+\\)\"[^>]*>"
   "</agent-message>"
   start end
   'agent-message))

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

(defun mevedel-view--user-turn-guest-name (segments data-buf)
  "Return the collaboration guest name owning this user turn, or nil.
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
        (cl-loop for (position . name)
                 in (mevedel-transcript-audit-guest-prompts)
                 when (and (>= position start) (< position strip-end))
                 return name)))))

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

(defun mevedel-view--user-input-fold-bounds ()
  "Return bounds of the contiguous user-input fold section at point."
  (when (eq (get-text-property (point) 'mevedel-view-type)
            'user-input-summary)
    (let ((start (or (previous-single-property-change
                      (point) 'mevedel-view-type)
                     (point-min)))
          (end (or (next-single-property-change
                    (point) 'mevedel-view-type)
                   (point-max))))
      ;; `previous-single-property-change' lands in the previous run
      ;; when point sits at the start of this one.
      (when (and (< start (point))
                 (not (eq (get-text-property start 'mevedel-view-type)
                          'user-input-summary)))
        (setq start (or (next-single-property-change
                         start 'mevedel-view-type)
                        (point))))
      (cons start end))))

(defun mevedel-view-render-toggle-user-input ()
  "Toggle the folded user input section at point.
The full input travels in a text property rather than being re-read
from the data buffer, so the send-path echo -- which has no source
coordinates yet -- folds and expands the same way as a rendered turn."
  (let* ((bounds (mevedel-view--user-input-fold-bounds))
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
                (insert text)
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

(defun mevedel-view--render-user-turn (segments data-buf &optional directive)
  "Render user SEGMENTS from DATA-BUF, with optional DIRECTIVE metadata."
  (let* ((raw-text (mevedel-view--user-turn-text segments data-buf))
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
                 (or (plist-get inline-skill :display-text)
                     raw-text)))
         (text-start nil))
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
      (let ((header-start (point)))
        (insert (propertize
                 (if directive
                   (format "◆ %s · %s · T%s · excluded from model context\n"
                           (truncate-string-to-width
                            (or (plist-get directive :directive-id) "?")
                            8 nil nil "…")
                           (mevedel-overlay-ui-directive-action-label
                            (plist-get directive :action))
                           (or (plist-get directive :turn) "?"))
                   (if-let* ((guest (mevedel-view--user-turn-guest-name
                                     segments data-buf)))
                       (format "%s (guest)\n" guest)
                     "You\n"))
                 'font-lock-face 'mevedel-view-user-header
                 'mevedel-view-type 'turn-header
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
        (if (and (null prompt-drawers)
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
                (caddr (car (last user-segs))))))
          (insert text)
          (unless (eq (char-before) ?\n)
            (insert "\n"))))
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
        (mevedel-view--insert-rendered-tool
         (list :header "Prompt"
               :body (plist-get inline-skill :expanded-prompt)
               :body-mode 'markdown-mode
               :vtype 'prompt-summary
               :initially-collapsed-p t)
         (plist-get inline-skill :source))))))
  (insert "\n"))

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
          (mevedel-view--insert-activity-rule-after-response)
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
         (partial
          (mevedel-view--partial-worktree-fork-reminder-p body))
         (summary (mevedel-view--system-reminder-summary
                   data-buf seg-start seg-end)))
    (mevedel-view--insert-activity-rule-after-response)
    (mevedel-view--insert-rendered-tool
     (list :header summary
           :body body
           :body-mode 'markdown-mode
           :vtype 'system-reminder-summary
           :status (and partial 'warning)
           :initially-collapsed-p (not partial))
     source)))

(defun mevedel-view--request-summary-line (render-data)
  "Return the visible request summary line for RENDER-DATA."
  (let ((elapsed (plist-get render-data :elapsed-seconds)))
    (when (numberp elapsed)
      (format "─ Worked for %s" (mevedel-view--duration-label elapsed)))))

(defun mevedel-view--render-request-summary-segment (seg data-buf)
  "Render request-summary SEG from DATA-BUF as an assistant footer."
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
      (mevedel-view--insert-rendered-tool failure-rendering source))
    (when line
      (let ((start (point)))
        (insert (propertize (concat line "\n")
                            'font-lock-face 'mevedel-view-separator))
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

(defun mevedel-view--insert-activity-rule-after-response ()
  "Insert a quiet separator before activity following response prose."
  (when (eq (mevedel-view--previous-rendered-type) 'response)
    (mevedel-view--ensure-blank-line-before-response)
    (insert (propertize "\n"
                        'font-lock-face 'mevedel-view-activity-rule
                        'mevedel-view-type 'activity-separator
                        'mevedel-view-collapsed nil))))

(defun mevedel-view--render-assistant-turn
    (segments data-buf &optional variant-button directive continuation-p)
  "Render assistant SEGMENTS from DATA-BUF.
Response text is shown inline, tool calls as collapsed one-liners,
reasoning blocks as collapsed summaries.  Adjacent thinking segments
are merged into a single summary.  VARIANT-BUTTON, when non-nil, is
inserted beside the header.  CONTINUATION-P suppresses that header."
  (unless (or directive continuation-p)
    (let ((header-start (point)))
      (insert "Assistant")
      (when variant-button
        (insert "  " variant-button))
      (insert "\n")
      (add-text-properties
       header-start (point)
       '(font-lock-face mevedel-view-assistant-header
         mevedel-view-type turn-header
         mevedel-view-turn-role assistant
         mevedel-view-collapsed nil))))
  (let ((view-buf (current-buffer))
        tool-group thinking-group request-summary-group)
    (dolist (seg segments)
      (let ((type (car seg)))
        (pcase type
          ('response
           ;; Flush accumulated groups
           (mevedel-view--flush-thinking-group thinking-group data-buf)
           (setq thinking-group nil)
           (when tool-group
             (mevedel-view--render-tool-group (nreverse tool-group) data-buf)
             (setq tool-group nil))
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
                         (mevedel-view--decorate-markdown-in-range
                          start response-end)
                         (goto-char response-end)
                         (set-marker response-end nil)))
                       (mevedel-view--mark-live-render-unit
                        unit-start seg-start))))))))
          ('tool
           ;; Flush thinking group before tools
           (mevedel-view--flush-thinking-group thinking-group data-buf)
           (setq thinking-group nil)
           ;; Accumulate consecutive tool segments
           (push seg tool-group))
          ('system-reminder
           (mevedel-view--flush-thinking-group thinking-group data-buf)
           (setq thinking-group nil)
           (when tool-group
             (mevedel-view--render-tool-group (nreverse tool-group) data-buf)
             (setq tool-group nil))
           (mevedel-view--render-system-reminder-segment seg data-buf))
          ('request-summary
           (push seg request-summary-group))
          ('mailbox
           (mevedel-view--flush-thinking-group thinking-group data-buf)
           (setq thinking-group nil)
           (when tool-group
             (mevedel-view--render-tool-group
              (nreverse tool-group) data-buf)
             (setq tool-group nil))
           (let ((text (mevedel-view--user-turn-text (list seg) data-buf))
                 (text-start nil))
             (mevedel-view--ensure-blank-line-before-response)
             (setq text-start (point))
             (insert text "\n")
             (mevedel-view--decorate-agent-result-blocks text-start (point))
             (mevedel-view--decorate-agent-message-blocks text-start (point))))
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
               (when tool-group
                 (mevedel-view--render-tool-group
                  (nreverse tool-group) data-buf)
                 (setq tool-group nil))
               (push seg thinking-group))))
          ((or 'reasoning 'render-data 'ignored)
           (cond
            ((and (eq type 'ignored)
                  (mevedel-view--hook-audit-only-segment-p
                   data-buf (cadr seg) (caddr seg))
                  (not tool-group))
             (mevedel-view--flush-thinking-group thinking-group data-buf)
             (setq thinking-group nil)
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
             (mevedel-view--flush-thinking-group thinking-group data-buf)
             (setq thinking-group nil)
             (when tool-group
               (mevedel-view--render-tool-group
                (nreverse tool-group) data-buf)
             (setq tool-group nil))
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
               ;; Flush tool group before thinking
               (when tool-group
                 (mevedel-view--render-tool-group
                  (nreverse tool-group) data-buf)
                 (setq tool-group nil))
               ;; Accumulate consecutive thinking segments
               (push seg thinking-group)))))))
    ;; Flush remaining groups
    (mevedel-view--flush-thinking-group thinking-group data-buf)
    (when tool-group
      (mevedel-view--render-tool-group (nreverse tool-group) data-buf))
    (dolist (seg (nreverse request-summary-group))
      (mevedel-view--render-request-summary-segment seg data-buf))))

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

(defun mevedel-view--tool-group-header (children)
  "Return the one-line activity summary for grouped CHILDREN."
  (let (names counts)
    (dolist (child children)
      (let ((name (or (plist-get child :tool) "Tool")))
        (unless (member name names)
          (push name names))
        (cl-incf (alist-get name counts 0 nil #'equal))))
    (setq names (nreverse names))
    (let* ((parts
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
             names))
           (summary (string-join parts ", ")))
      ;; Capitalize only a verb phrase; a leading tool name keeps its
      ;; own casing.
      (if (and (not (string-empty-p summary))
               (assoc (car names) mevedel-view--tool-group-verbs))
          (concat (upcase (substring summary 0 1))
                  (substring summary 1))
        summary))))

(defun mevedel-view--tool-group-child (entry data-buf index)
  "Return the nested-call plist for tool ENTRY in DATA-BUF, or nil.
INDEX discriminates the row's disclosure key from its siblings.  A
segment whose own rendering is hidden yields nil so grouping does not
resurrect rows the renderer suppressed."
  (when-let* ((child
               (or (plist-get entry :group-child)
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
    (append (list :id (format "%d" index) :order index) child)))

(defun mevedel-view--tool-group-rendering (entries data-buf)
  "Return the grouped rendering for tool ENTRIES in DATA-BUF, or nil.
The result reuses the compound-tool row machinery: each grouped call is
a `:child-calls' entry rendered by its own tool's renderer, so the
expanded group has the same layout, per-row disclosure, and toggles as a
ToolScript block.  A run with any failed call carries a warning marker but
still renders collapsed: the marker says something went wrong, and the
reader opens the group when they want to know what."
  (let* ((children
          (let ((index 0)
                out)
            (dolist (entry entries (nreverse out))
              (when-let* ((child (mevedel-view--tool-group-child
                                  entry data-buf index)))
                (push child out))
              (cl-incf index))))
         (failed-p
          (cl-some (lambda (child)
                     (not (eq (plist-get child :status) 'success)))
                   children)))
    (when children
      (list :header (mevedel-view--tool-group-header children)
            :vtype 'tool-group
            :expandable-p t
            :child-calls children
            :status (and failed-p 'warning)
            :initially-collapsed-p t))))

(defun mevedel-view--tool-group-rendering-from-source
    (data-buf start end)
  "Rebuild a grouped tool rendering for DATA-BUF's START..END disclosure."
  (let ((entries
         (with-current-buffer data-buf
           (save-restriction
             (widen)
             (mapcar
              (lambda (segment)
                (list :start (cadr segment) :end (caddr segment)))
              (mevedel-view--merge-tool-hook-audit-segments
               (cl-remove-if-not
                (lambda (segment) (eq (car segment) 'tool))
                (mevedel-transcript-segments start end))
               data-buf))))))
    (mevedel-view--tool-group-rendering entries data-buf)))

(defun mevedel-view--tool-group-entry-p (entry)
  "Return non-nil when ENTRY may fold into a grouped activity row.
Rows that demand individual presentation stay out: agent handles and
other non-tool vtypes, compound tools with their own nested rows, rows
carrying hook audits or a sandbox warning, rows their renderer wants
expanded or compact, coalesced rows, and renderer fallbacks.

A `note'-class sandbox summary folds like any other row.  It restates
the session's standing confinement configuration, which already warned
once on its own, and a nested row does not carry the summary -- so the
note is dropped rather than repeated one fold deeper."
  (let ((rendering (plist-get entry :rendering)))
    (and rendering
         (= (plist-get entry :count) 1)
         (eq (or (plist-get rendering :vtype) 'tool-summary) 'tool-summary)
         (null (plist-get rendering :child-calls))
         (null (plist-get rendering :hook-audits))
         (memq (mevedel-execution-telemetry-sandbox-summary-class
                (plist-get rendering :sandbox-summary))
               '(nil note))
         (not (plist-get rendering :force-expanded-p))
         (not (and (plist-member rendering :expandable-p)
                   (not (plist-get rendering :expandable-p))))
         (not (and (plist-member rendering :initially-collapsed-p)
                   (not (plist-get rendering :initially-collapsed-p)))))))

(defun mevedel-view--insert-tool-group (entries data-buf)
  "Insert grouped ENTRIES from DATA-BUF as one expandable activity row.
Return non-nil when the group row was inserted."
  (let* ((group-start (plist-get (car entries) :start))
         (group-end (plist-get (car (last entries)) :end))
         (rendering (mevedel-view--tool-group-rendering
                     entries data-buf))
         (source (and rendering
                      (mevedel-view-disclosure-source-range
                       data-buf group-start group-end))))
    (when rendering
      (mevedel-view--insert-rendered-tool rendering source)
      t)))

(defun mevedel-view--render-tool-group (tool-segments data-buf)
  "Render consecutive TOOL-SEGMENTS from DATA-BUF.
Each tool call gets its own collapsible entry.  A registered
`:renderer' is invoked when the segment carries a render-data
side-channel, falling back to the default one-liner otherwise.
Adjacent visible renderings with equal `:coalesce-key' values keep
only the final row and show the number of combined calls.  Hook audits
from every combined call remain in source order.  Runs of more than
`mevedel-view-tool-group-collapse-threshold' plain rows fold into one
grouped activity row that expands into compound-tool nested rows."
  (let* ((unit-start (point))
         (unit-source (cadr (car tool-segments)))
         (tool-segments
          (mevedel-view--merge-tool-hook-audit-segments
           tool-segments data-buf))
         (entries
          (let (out)
            (dolist (seg tool-segments (nreverse out))
              (let* ((seg-start (cadr seg))
                     (seg-end (caddr seg))
                     (source (mevedel-view-disclosure-source-range
                              data-buf seg-start seg-end))
                     (rendering (mevedel-view--segment-rendering
                                 data-buf seg-start seg-end t))
                     (vtype (or (plist-get rendering :vtype)
                                'tool-summary))
                     (state
                      (and rendering
                           (mevedel-view-disclosure-state-entry
                            source vtype))))
                (when (and state (not (cdr state)))
                  (setq rendering
                        (or (mevedel-view--segment-rendering
                             data-buf seg-start seg-end)
                            rendering)))
                (let ((group-child (plist-get rendering :group-child)))
                  (when group-child
                    (setq rendering
                          (plist-put (copy-sequence rendering)
                                     :group-child nil)))
                (unless (plist-get rendering :hidden-p)
                  (let* ((entry
                          (list :start seg-start :end seg-end
                                :source source :rendering rendering
                                :group-child group-child
                                :count 1))
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
                      (push entry out)))))))))
        (start-time (float-time))
        (inserted-rule nil)
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
                 (if rendering
                     (progn
                       (unless inserted-rule
                         (mevedel-view--insert-activity-rule-after-response)
                         (setq inserted-rule t))
                       (cl-incf rendered)
                       (mevedel-view--insert-rendered-tool rendering source))
                   (when-let* ((summary (mevedel-view--tool-one-liner
                                         data-buf seg-start seg-end)))
                     (unless inserted-rule
                       (mevedel-view--insert-activity-rule-after-response)
                       (setq inserted-rule t))
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
                        ins-start (point)))))))
             (flush-run
               (run)
               (let ((run-entries (nreverse run)))
                 (if (and (> mevedel-view-tool-group-collapse-threshold 0)
                          (> (length run-entries)
                             mevedel-view-tool-group-collapse-threshold)
                          (progn
                            (unless inserted-rule
                              (mevedel-view--insert-activity-rule-after-response)
                              (setq inserted-rule t))
                            (mevedel-view--insert-tool-group
                             run-entries data-buf)))
                     (cl-incf rendered (length run-entries))
                   (mapc #'insert-entry run-entries)))))
          (let (run)
            (dolist (entry entries)
              (if (mevedel-view--tool-group-entry-p entry)
                  (push entry run)
                (when run
                  (flush-run run)
                  (setq run nil))
                (insert-entry entry)))
            (when run
              (flush-run run))))
      (mevedel-view--debug-log
       'render-tool-group
       :segments (length tool-segments)
       :rendered rendered
       :fallbacks fallbacks
       :elapsed (- (float-time) start-time)))
    (mevedel-view--mark-live-render-unit unit-start unit-source)))

(defun mevedel-view--tool-readable-text (raw)
  "Return RAW advanced to the readable tool call when possible.

Text-property boundaries can include org drawers, `#+begin_tool'
markers, or other unpropertized scaffolding.  Prefer the structural
tool form itself when it is present inside RAW."
  (let ((text raw))
    (setq text
          (replace-regexp-in-string
           "\\`[ \t\n]*:PROPERTIES:\n\\(?:.*\n\\)*?:END:\n?"
           "" text))
    (setq text
          (replace-regexp-in-string
           "\\`\\(?:[ \t]*\\(?:#\\+\\(?:begin\\|end\\)_\\(?:tool\\|reasoning\\)[^\n]*\\)?\n\\)+"
           "" text))
    (if (string-match "(\\s-*:name\\_>" text)
        (substring text (match-beginning 0))
      text)))

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
           (let ((sexp (read (mevedel-view--tool-readable-text raw))))
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

(defun mevedel-view--tool-segment-text (seg-start seg-end)
  "Return raw tool text for SEG-START..SEG-END.
If the segment overlaps an org tool block, expand to the block bounds
first so stale restored text properties do not hide the `(:name ...)'
form or the render-data block from the parser."
  (let ((raw (buffer-substring seg-start seg-end)))
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
         (source (or trimmed source))
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
                   data-buf data-start data-end (eq vtype 'hook-audit)))
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
          (setq text
                (mevedel-view--fontify-as
                 (or (mevedel-view--system-reminder-body-from-text text) text)
                 'markdown-mode)))
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
         (source (or trimmed source))
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

(defun mevedel-view--turn-bounds ()
  "Return (START . END) bounds of the turn at point.
A turn is the contiguous run of text sharing the same
`mevedel-view-turn-id'.  Returns nil when point has no turn id."
  (let ((id (get-text-property (point) 'mevedel-view-turn-id)))
    (when id
      (let ((start (or (previous-single-property-change
                        (point) 'mevedel-view-turn-id)
                       (point-min)))
            (end (or (next-single-property-change
                      (point) 'mevedel-view-turn-id)
                     (point-max))))
        ;; `previous-single-property-change' lands in the PREVIOUS run
        ;; when point is at the start of the current run.  Advance past
        ;; any leading region whose id is not `eq' to ours.
        (when (and (< start (point))
                   (not (eq (get-text-property start 'mevedel-view-turn-id)
                            id)))
          (setq start (or (next-single-property-change
                           start 'mevedel-view-turn-id)
                          (point))))
        (cons start end)))))

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
             (truncate-string-to-width
              (or (plist-get directive :directive-id) "?") 8 nil nil "…")
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
  (let* ((bounds (mevedel-view--turn-bounds))
         (role (get-text-property (point) 'mevedel-view-turn-role))
         (id (get-text-property (point) 'mevedel-view-turn-id))
         (directive (get-text-property (point) 'mevedel-view-directive)))
    (unless (and bounds role id)
      (user-error "No turn at point"))
    (mevedel-view-stream-in-flight-turn-start-position)
    (let* ((turn-start (car bounds))
           (turn-end (cdr bounds))
           (stash (buffer-substring turn-start turn-end))
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
                                      'mevedel-view-turn-role role
                                      'mevedel-view-turn-id id
                                      'mevedel-view-directive directive
                                      'mevedel-view-collapsed t
                                      'mevedel-view-stash stash
                                      'read-only t
                                      'keymap mevedel-view--display-map
                                      'front-sticky '(read-only keymap)
                                      'rear-nonsticky '(read-only keymap)))
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
  (let* ((bounds (mevedel-view--turn-bounds))
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
  (let* ((bounds (mevedel-view--turn-bounds))
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
    (when-let* ((bounds (mevedel-view--turn-bounds)))
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
  "Capture point, window, and disclosure state for the displayed segment."
  (let* ((input-start (mevedel-view--input-marker-position))
         (turn-starts (mevedel-view--rendered-turn-starts))
         (window (get-buffer-window (current-buffer) t)))
    (list
     :point (point)
     :input-offset
     (and input-start
          (>= (point) input-start)
          (- (point) (mevedel-view--input-start)))
     :window-start (and window (window-start window))
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
          (min (plist-get state :point)
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
              (start (plist-get state :window-start)))
    (set-window-start window (min start (point-max)) t)))

(defun mevedel-view-render-project-segment (data-buffer state direction)
  "Render DATA-BUFFER and restore projection STATE for DIRECTION."
  (mevedel-view--full-rerender data-buffer t)
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
  "Return non-nil when LINE is too volatile for live-tail matching."
  (let* ((trimmed (string-trim (or line "")))
         (pending-labels
          (delq nil
                (mapcar (lambda (entry)
                          (let ((label (and (consp entry) (cdr entry))))
                            (and (stringp label) (string-trim label))))
                        mevedel-view--pending-tool-calls))))
    (when (> (length mevedel-view--pending-tool-calls)
             mevedel-view-pending-tools-visible-max)
      (push (format "%d more tools running…"
                    (- (length mevedel-view--pending-tool-calls)
                       mevedel-view-pending-tools-visible-max))
            pending-labels))
    (or (string-empty-p trimmed)
        (string-match-p "\\`[[:space:]]*… Thinking\\.\\.\\." trimmed)
        (cl-some (lambda (label)
                   (or (string= trimmed label)
                       (cl-some (lambda (frame)
                                  (and (not (string-empty-p frame))
                                       (string-prefix-p frame trimmed)
                                       (string= (string-trim-left
                                                 (substring trimmed
                                                            (length frame)))
                                                label)))
                                mevedel-view-spinner-frames)))
                 pending-labels)
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
  (mevedel-view-render-invalidate-live-tail)
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
    (when historical-p
      (insert (mevedel-view-segments-banner))))
  (set-marker mevedel-view--input-marker (point))
  (when (markerp mevedel-view--status-marker)
    (set-marker mevedel-view--status-marker (point)))
  (when (markerp mevedel-view--interaction-marker)
    (set-marker mevedel-view--interaction-marker (point)))
  (mevedel-view--debug-log
   'full-rerender-after-header
   :state (mevedel-view--debug-state data-buf)))

(defun mevedel-view--full-rerender-project
    (data-buf session-data-buf render-view-buf
              agent-transcript-p data-turn-start-pos saved-states)
  "Project DATA-BUF into its view and return the last-turn rendering state.
SESSION-DATA-BUF supplies live session metadata.  RENDER-VIEW-BUF is
used for agent transcripts.  DATA-TURN-START-POS identifies the live
turn.  SAVED-STATES restores matching disclosure state."
  (with-current-buffer data-buf
    (unless (mevedel-view--running-agent-transcript-buffer-p)
      (mevedel-transcript-restore-properties t))
    (let ((scan-start
           (mevedel-transcript--skip-leading-properties-drawer
            (point-min)))
          (view-buf
           (if agent-transcript-p
               render-view-buf
             (buffer-local-value 'mevedel--view-buffer data-buf)))
          (compaction-indicator-inserted nil))
      (when-let* ((transcript-start
                   (buffer-local-value
                    'mevedel-view--transcript-start view-buf))
                  ((markerp transcript-start))
                  ((eq (marker-buffer transcript-start) data-buf)))
        (setq scan-start (max scan-start (marker-position transcript-start))))
      (when (eq (get-text-property scan-start 'face) 'shadow)
        (setq scan-start
              (or (next-single-property-change
                   scan-start 'face nil (point-max))
                  (point-max)))
        (let ((summary-start scan-start))
          (save-excursion
            (goto-char scan-start)
            (when (re-search-forward "^#\\+end_summary\n\\|^```\n" nil t)
              (setq scan-start (point))))
          (mevedel-view--insert-compaction-indicator
           view-buf
           (mevedel-view--summary-hook-audits
            data-buf summary-start scan-start)
           (cons summary-start scan-start)))
        (setq compaction-indicator-inserted t))
      (let ((after-summary
             (mevedel-transcript--skip-leading-summary-block scan-start)))
        (when (> after-summary scan-start)
          (unless compaction-indicator-inserted
            (mevedel-view--insert-compaction-indicator
             view-buf
             (mevedel-view--summary-hook-audits
              data-buf scan-start after-summary)
             (cons scan-start after-summary)))
          (setq scan-start after-summary))
      (save-restriction
        (narrow-to-region scan-start (point-max))
        (let* ((segments
                (mevedel-transcript-segments (point-min) (point-max)))
               (turns (mevedel-view--group-transcript-turns
                       segments data-buf))
               (session
                (and (not agent-transcript-p)
                     (buffer-local-value
                      'mevedel--session session-data-buf)))
               (mevedel-view--conversation-variant-sessions
                (when (and session
                           (mevedel-session-save-path session)
                           (mevedel-session-workspace session))
                  ;; Every settled turn carries a fork point, so this runs
                  ;; on every full re-render.  Enumerating the workspace
                  ;; live would cost several target round trips per
                  ;; persisted session each time; the buttons only decorate
                  ;; settled history, so they tolerate the last live
                  ;; listing (picker, resume, fork), and activating one
                  ;; enumerates live anyway.
                  (when (mevedel-session-artifacts-fork-point-spans
                         data-buf)
                    (mevedel-session-persistence-list-sessions
                     (mevedel-session-workspace session) 'cached))))
               last-assistant-turn-start
               last-assistant-turn-end
               last-assistant-turn-data-start
               last-current-assistant-turn-start
               last-current-assistant-turn-data-start
               last-turn-role)
          (with-current-buffer view-buf
            (dolist (turn turns)
              (setq last-turn-role (plist-get turn :role))
              (when (eq last-turn-role 'assistant)
                (let ((view-turn-start
                       (copy-marker mevedel-view--input-marker nil)))
                  (setq last-assistant-turn-start view-turn-start
                        last-assistant-turn-data-start
                        (plist-get turn :start))
                  (when (and data-turn-start-pos
                             (plist-get turn :end)
                             (> (plist-get turn :end)
                                data-turn-start-pos))
                    (setq last-current-assistant-turn-start
                          view-turn-start
                          last-current-assistant-turn-data-start
                          (plist-get turn :start)))))
              (mevedel-view--render-turn turn data-buf t session)
              (when (eq last-turn-role 'assistant)
                (setq last-assistant-turn-end
                      (copy-marker mevedel-view--input-marker nil))))
            (mevedel-view--collapse-settled-directive-turns)
            (when saved-states
              (mevedel-view-disclosure-restore-state
               (point-min)
               (marker-position mevedel-view--input-marker)
               saved-states)))
          (list
           :view-buffer view-buf
           :last-assistant-turn-start last-assistant-turn-start
           :last-assistant-turn-end last-assistant-turn-end
           :last-assistant-turn-data-start last-assistant-turn-data-start
           :last-current-assistant-turn-start
           last-current-assistant-turn-start
           :last-current-assistant-turn-data-start
           last-current-assistant-turn-data-start
           :last-turn-role last-turn-role)))))))

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
HISTORICAL-P suppresses live-only rows.  START-TIME is for diagnostics."
  (with-current-buffer (plist-get rendering :view-buffer)
    (unless mevedel-view--agent-transcript-p
      (mevedel-view-refresh-input-prompt)
      (when (and (not historical-p)
                 mevedel-view--pending-tool-calls)
        (mevedel-view--refresh-pending-tool-lines))
      (mevedel-view--render-status live-data-buf)
      (mevedel-view--interaction-rebuild)
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
  (atomic-change-group
    (mevedel-view-render--preserving-window-state
     (mevedel-view--call-preserving-input-text
      (lambda ()
      (let* ((start-time (float-time))
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
             (preserved-live-tail
              (when-let* (((not historical-p))
                          ((not agent-transcript-p))
                          (tail-start
                           (mevedel-view-stream-in-flight-turn-start-position))
                          ((markerp mevedel-view--status-marker))
                          (tail-end
                           (marker-position mevedel-view--status-marker))
                          ((< tail-start tail-end)))
                (mevedel-view--strip-history-live-fragments-from-string
                 (buffer-substring tail-start tail-end)))))
        (unless mevedel-view--pending-tool-calls
          (mevedel-view--delete-pending-tool-live-lines))
        (mevedel-view--debug-log
         'full-rerender-begin
         :in-flight-was in-flight-was
         :preserved-live-tail-len
         (and preserved-live-tail (length preserved-live-tail))
         :state (mevedel-view--debug-state data-buf))
        (mevedel-view-disclosure-reset-state)
        (mevedel-view--full-rerender-reset
         data-buf session-data-buf historical-p agent-transcript-p)
        (let ((rendering
               (mevedel-view--full-rerender-project
                data-buf session-data-buf render-view-buf
                agent-transcript-p data-turn-start-pos saved-states)))
          (mevedel-view--full-rerender-reanchor
           data-buf rendering in-flight-was
           data-turn-start-pos preserved-live-tail)
            (mevedel-view--full-rerender-finish
             data-buf live-data-buf rendering historical-p start-time))))))))

;;
;;; Optimistic user turn rendering

(defun mevedel-view--insert-user-message
    (text &optional kind hook-context prompt-summary-body
          prompt-summary-source hook-audits guest-name)
  "Render TEXT as a user message in the history region.
Inserts at the history boundary with read-only protection.
KIND may be `directive' to fontify directive-specific display text.
HOOK-CONTEXT is model-visible hook context to summarize in the view.
PROMPT-SUMMARY-BODY, when non-nil, is shown as a collapsed Prompt
section backed by PROMPT-SUMMARY-SOURCE when available.  HOOK-AUDITS
is a list of hook audit records to render under the user turn.
GUEST-NAME, when non-nil, names the collaboration guest whose queued
prompt this is; the turn heading carries it instead of \"You\".

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
            user-end)
        (insert (propertize (if guest-name
                                (format "%s (guest)\n" guest-name)
                              "You\n")
                            'font-lock-face 'mevedel-view-user-header))
        (if (and (not (eq kind 'directive))
                 (mevedel-view--user-input-fold-p text))
            (progn
              (setq fold-start (point))
              (mevedel-view--insert-user-input-fold text))
          (insert (if (eq kind 'directive)
                      (mevedel-view--fontify-directive-display-text text)
                    text))
          (unless (eq (char-before) ?\n)
            (insert "\n")))
        (setq user-end (point))
        (when-let* ((events (mevedel-view--hook-context-events-from-text
                             hook-context)))
          (mevedel-view--insert-hook-context-block events))
        (dolist (audit hook-audits)
          (mevedel-view--insert-hook-audit-block
           audit
           (plist-get audit :source)))
        (let ((prompt-body (and (stringp prompt-summary-body)
                                (string-trim prompt-summary-body))))
          (when (and prompt-body
                     (not (string-empty-p prompt-body)))
            (mevedel-view--insert-rendered-tool
             (list :header "Prompt"
                   :body prompt-body
                   :body-mode 'markdown-mode
                   :vtype 'prompt-summary
                   :initially-collapsed-p t)
             prompt-summary-source)))
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
