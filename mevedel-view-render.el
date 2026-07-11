;;; mevedel-view-render.el --- Transcript rendering and navigation -*- lexical-binding: t -*-

;;; Commentary:

;; Owns transcript turn grouping, source-backed rendering, renderer
;; interpretation, folding, expansion, full and incremental redraws, and
;; transcript navigation for mevedel view buffers.

;;; Code:

(eval-when-compile (require 'cl-lib))

;; `cl-extra'
(declare-function cl-some "cl-extra" (predicate sequence &rest more-sequences))
(declare-function cl-subseq "cl-extra" (seq start &optional end))

;; `cl-macs'
(declare-function cl-gensym "cl-macs" (&optional prefix))

;; `cl-seq'
(declare-function cl-find "cl-seq" (cl-item cl-seq &rest cl-keys))
(declare-function cl-find-if "cl-seq" (cl-pred cl-list &rest cl-keys))

;; `mevedel-agent-runtime'
(declare-function mevedel-agent-runtime--agent-invocation-at
                  "mevedel-agent-runtime" (fsm))
(defvar mevedel-agent-runtime--fsms)

;; `mevedel-agents'
(declare-function mevedel-agent-invocation-p "mevedel-agents" (cl-x))
(declare-function mevedel-agent-invocation-transcript-status
                  "mevedel-agents" (cl-x) t)

;; `mevedel-pipeline'
(declare-function mevedel-pipeline--format-render-data-block
                  "mevedel-pipeline" (render-data))
(declare-function mevedel-pipeline--strip-render-data-blocks
                  "mevedel-pipeline" (string))
(declare-function mevedel-pipeline-extract-render-data
                  "mevedel-pipeline"
                  (result-string &optional session expected-tool-use-id
                                 allow-payload-tool-use-id))

;; `mevedel-review'
(declare-function mevedel-review-strip-user-action-blocks
                  "mevedel-review" (text))

;; `mevedel-structs'
(declare-function mevedel-request-started-at "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-agent-transcripts
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-permission-mode
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-permission-queue
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-plan-queue "mevedel-structs" (cl-x) t)
(defvar mevedel--agent-invocation)
(defvar mevedel--current-request)
(defvar mevedel--data-buffer)
(defvar mevedel--session)
(defvar mevedel--view-buffer)

;; `mevedel-tool-plan'
(declare-function mevedel-plan-mode-extract-proposed-plan
                  "mevedel-tool-plan" (text))
(declare-function mevedel-plan-mode-known-proposed-plan-p
                  "mevedel-tool-plan" (plan-markdown &optional session))
(declare-function mevedel-plan-mode-strip-proposed-plans
                  "mevedel-tool-plan" (text))

;; `mevedel-tool-registry'
(declare-function mevedel-tool-display-string
                  "mevedel-tool-registry" (tool-name args))
(declare-function mevedel-tool-get
                  "mevedel-tool-registry" (name &optional category))
(declare-function mevedel-tool-name "mevedel-tool-registry" (cl-x) t)
(declare-function mevedel-tool-renderer "mevedel-tool-registry" (cl-x) t)

;; `mevedel-tool-task'
(declare-function mevedel-toggle-tasks "mevedel-tool-task" ())

;; `mevedel-tool-ui'
(declare-function mevedel-tool-ui--render-agent
                  "mevedel-tool-ui" (name args result render-data))

;; `mevedel-transcript'
(declare-function mevedel-transcript--mailbox-any-block-at-point
                  "mevedel-transcript" (limit))
(declare-function mevedel-transcript--mailbox-find-close
                  "mevedel-transcript" (open-regexp close-tag limit))
(declare-function mevedel-transcript--queued-user-message-batch-items-from-text
                  "mevedel-transcript" (text))
(declare-function mevedel-transcript--skip-leading-properties-drawer
                  "mevedel-transcript" (pos))
(declare-function mevedel-transcript--skip-leading-summary-block
                  "mevedel-transcript" (pos))
(declare-function mevedel-transcript--tool-block-bounds-for-run
                  "mevedel-transcript" (seg-start seg-end &optional limit))
(declare-function mevedel-transcript-segments
                  "mevedel-transcript" (start end))

;; `mevedel-transcript-audit'
(autoload 'mevedel--strip-hook-audit-blocks "mevedel-transcript-audit")
(declare-function mevedel-transcript-audit-only-p
                  "mevedel-transcript-audit" (text))

;; `mevedel-transcript-restore'
(declare-function mevedel-transcript-restore-properties
                  "mevedel-transcript-restore" (&optional only-if-missing))

;; `mevedel-view'
(declare-function mevedel-view--display-label-for-agent
                  "mevedel-view" (agent-id))
(declare-function mevedel-view--header-string
                  "mevedel-view" (data-buf))
(declare-function mevedel-view--insert-attribution
                  "mevedel-view" (agent-id &optional live-click-p calls))
(declare-function mevedel-view--interaction-rebuild
                  "mevedel-view" ())
(declare-function mevedel-view--render-agent-status
                  "mevedel-view" ())
(declare-function mevedel-view--render-status
                  "mevedel-view" (&optional data-buf))
(declare-function mevedel-view-agent-handle-activate
                  "mevedel-view" (&optional agent-id))
(declare-function mevedel-view-agent-status-toggle
                  "mevedel-view" ())
(defvar mevedel-view--agent-handle-map)
(defvar mevedel-view--agent-label-map)
(defvar mevedel-view--agent-transcript-p)
(defvar mevedel-view--display-map)
(defvar mevedel-view--interaction-marker)
(defvar mevedel-view--status-agent-collapse-key)
(defvar mevedel-view--status-marker)
(defvar mevedel-view-pending-tools-visible-max)

;; `mevedel-view-composer'
(declare-function mevedel-view--call-preserving-input-text
                  "mevedel-view-composer" (thunk))
(declare-function mevedel-view--call-preserving-window-state
                  "mevedel-view-composer" (thunk))
(declare-function mevedel-view--call-with-render-boundaries-advancing
                  "mevedel-view-composer" (thunk))
(declare-function mevedel-view--ensure-interactive-chat-view
                  "mevedel-view-composer" ())
(declare-function mevedel-view--input-marker-position
                  "mevedel-view-composer" ())
(declare-function mevedel-view-refresh-input-prompt
                  "mevedel-view-composer" ())
(defvar mevedel-view--input-marker)

;; `mevedel-view-stream'
(declare-function mevedel-view--delete-pending-tool-live-lines
                  "mevedel-view-stream" ())
(declare-function mevedel-view--duration-label
                  "mevedel-view-stream" (seconds))
(declare-function mevedel-view--ensure-request-progress
                  "mevedel-view-stream" (&optional data-buf status))
(declare-function mevedel-view--forget-request-progress-region
                  "mevedel-view-stream" ())
(declare-function mevedel-view--in-flight-turn-start-position
                  "mevedel-view-stream" ())
(declare-function mevedel-view--insert-pending-tool-lines
                  "mevedel-view-stream" (entries))
(declare-function mevedel-view--refresh-pending-tool-lines
                  "mevedel-view-stream" ())
(declare-function mevedel-view--set-in-flight-turn-start
                  "mevedel-view-stream" (position))
(declare-function mevedel-view--spinner-frame "mevedel-view-stream" ())
(declare-function mevedel-view--spinner-region-p
                  "mevedel-view-stream" (start end))
(defvar mevedel-view--data-turn-start)
(defvar mevedel-view--in-flight-turn-start)
(defvar mevedel-view--pending-tool-calls)
(defvar mevedel-view-spinner-frames)

;; `mevedel-view-zone'
(declare-function mevedel-view-zone-next
                  "mevedel-view-zone" (&optional limit))
(declare-function mevedel-view-zone-previous
                  "mevedel-view-zone" (&optional limit))
(declare-function mevedel-view-zone-region "mevedel-view-zone" (zone))
(declare-function mevedel-view-zone-set-collapse-state
                  "mevedel-view-zone" (key collapsed))

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

(defcustom mevedel-view-mailbox-collapse-line-threshold 5
  "Mailbox delivery bodies longer than this many lines start collapsed.
Shorter bodies render fully expanded."
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
        "\\`[ \t\n]*\\(?:Error:\\|FAILED\\b\\|Tool failed\\b\\)"
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

(defvar-local mevedel-view--source-collapse-states nil
  "Hash table of source-backed disclosure states for this view.
Keys are source-collapse keys from `mevedel-view--source-collapse-state-key'.
Values are t when collapsed and nil when expanded.")

(defvar-local mevedel-view--response-fontify-cache nil
  "Hash table caching response fontification for this view.")

(defvar-local mevedel-view--render-cache-entries 0
  "Approximate number of entries in view-local render caches.")

(defvar-local mevedel-view--response-cache-entries 0
  "Approximate number of entries in `mevedel-view--response-fontify-cache'.")

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

(defun mevedel-view-render-initialize ()
  "Initialize transcript-rendering state in the current view buffer."
  (setq-local mevedel-view--tool-rendering-cache
              (make-hash-table :test #'equal))
  (setq-local mevedel-view--source-collapse-states
              (make-hash-table :test #'equal))
  (setq-local mevedel-view--response-fontify-cache
              (make-hash-table :test #'equal))
  (setq-local mevedel-view--render-cache-entries 0)
  (setq-local mevedel-view--response-cache-entries 0)
  (setq-local mevedel-view--user-pre-rendered nil))


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
  (require 'mevedel-view-zone)
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

(defun mevedel-view--debug-state (&optional data-buf start end)
  "Return a plist describing the current view-render state.
DATA-BUF, START, and END describe the data-buffer range being rendered."
  (let* ((input (mevedel-view--debug-marker-position
                 mevedel-view--input-marker))
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
                               (with-current-buffer data-buf (point-max))))))

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

(defun mevedel-view--add-display-region-properties
    (start end &optional default-vtype)
  "Mark START..END read-only and attach default display keymaps.
Existing local `keymap' properties, such as transcript attribution
buttons, are preserved.  DEFAULT-VTYPE is used when a character has
no `mevedel-view-type' property yet."
  (add-text-properties start end
                       '(read-only t
                         front-sticky (read-only keymap)
                         rear-nonsticky (read-only keymap)))
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
   ((and (require 'markdown-mode nil t)
         (fboundp 'markdown-mode))
    'markdown-mode)))

(defun mevedel-view--visible-response-text (text)
  "Return response TEXT with model protocol hidden when appropriate."
  (let ((text (mevedel-view--strip-render-data-display-text text)))
    (if (and (fboundp 'mevedel-plan-mode-strip-proposed-plans)
             (mevedel-view--strip-proposed-plans-p text))
        (mevedel-plan-mode-strip-proposed-plans text)
      text)))

(defmacro mevedel-view--with-render-temp-buffer (&rest body)
  "Run BODY in a temporary buffer with user mode hooks suppressed."
  (declare (indent 0) (debug t))
  `(let ((change-major-mode-after-body-hook nil)
         (after-change-major-mode-hook nil)
         (hack-local-variables-hook nil)
         (enable-local-variables nil)
         (font-lock-mode-hook nil)
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

(defun mevedel-view--tool-content-fingerprint (text)
  "Return a compact full-content fingerprint for tool segment TEXT."
  (list (length text) (secure-hash 'sha1 text)))

(defun mevedel-view--cache-put (table key value counter-symbol)
  "Put VALUE in TABLE under KEY and bump COUNTER-SYMBOL.
Clear TABLE when `mevedel-view-render-cache-max-entries' is exceeded."
  (unless (gethash key table)
    (set counter-symbol (1+ (symbol-value counter-symbol))))
  (puthash key value table)
  (when (> (symbol-value counter-symbol)
           mevedel-view-render-cache-max-entries)
    (clrhash table)
    (set counter-symbol 0))
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
                          (display-warning
                           'mevedel
                           (format "Could not fontify response as Markdown: %s"
                                   (error-message-string err))
                           :warning)
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

A segment classified as `user' (gptel property nil) only starts a
new turn when it follows another `user' or `response' segment.
When it follows `ignore' or `tool' segments it is reasoning text
embedded in the assistant turn and is absorbed as such.

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
             (next-type (car-safe (cadr rest)))
             (prompt-drawer-after-user-p
              (and (eq type 'prompt)
                   (null current-role)
                   turns
                   (eq (plist-get (car turns) :role) 'user)))
             (hook-audit-after-user-p
              (and (eq type 'ignored)
                   data-buf
                   (null current-role)
                   turns
                   (eq (plist-get (car turns) :role) 'user)
                   (mevedel-view--hook-audit-only-segment-p
                    data-buf seg-start (caddr seg))))
             (hook-context-after-user-p
              (and (eq type 'hook-context)
                   (null current-role)
                   turns
                   (eq (plist-get (car turns) :role) 'user)))
             (scaffolding-before-hook-audit-p
              (and (eq type 'user)
                   data-buf
                   (mevedel-view--scaffolding-only-p
                    data-buf seg-start (caddr seg))
                   (let ((next (cadr rest)))
                     (and (eq (car-safe next) 'ignored)
                          (mevedel-view--hook-audit-only-segment-p
                           data-buf (cadr next) (caddr next))))))
             (review-action-p
              (and (eq type 'user)
                   data-buf
                   (mevedel-view--review-action-segment-p
                    data-buf seg-start (caddr seg))))
             (queued-batch-p (eq type 'queued-message))
             (queued-batch-continuation-p
              (and (eq type 'user)
                   data-buf
                   (null current-role)
                   turns
                   (eq (plist-get (car turns) :role) 'user)
                   (mevedel-view--queued-user-message-batch-items-from-text
                    (mevedel-view--queued-user-message-batch-normalized-text
                     (mevedel-view--segments-raw-text
                      (append (plist-get (car turns) :segments)
                              (list seg))
                      data-buf)))))
             (system-reminder-p (eq type 'reminder))
             (inline-skill-render-p
              (and data-buf
                   (memq type '(user render-data ignored))
                   (mevedel-view--inline-skill-render-segment-p
                    data-buf seg-start (caddr seg))))
             (request-summary-p
              (and data-buf
                   (memq type '(user render-data ignored))
                   (mevedel-view--request-summary-render-segment-p
                    data-buf seg-start (caddr seg))))
             (render-data-only-p
              (and data-buf
                   (memq type '(user render-data ignored))
                   (not (and (memq type '(render-data ignored))
                             (mevedel-view--agent-transcript-render-segment-p
                              data-buf seg-start (caddr seg))))
                   (mevedel-view--render-data-only-segment-p
                    data-buf seg-start (caddr seg)))))
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
         (queued-batch-p
          (when current-segs
            (push (list :role current-role
                        :segments (nreverse current-segs)
                        :start turn-start
                        :end (caddr (car current-segs)))
                  turns))
          (push (list :role 'user
                      :segments (list seg)
                      :start seg-start
                      :end (caddr seg))
                turns)
          (setq current-segs nil current-role nil turn-start nil))
         (scaffolding-before-hook-audit-p
          nil)
         ((or prompt-drawer-after-user-p
              hook-audit-after-user-p
              hook-context-after-user-p
              queued-batch-continuation-p
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
         ((and (eq type 'user)
               (or review-action-p
                   (memq prev-type '(nil user response)))
               ;; Look-ahead: a scaffolding-only nil gap right after a
               ;; response is assistant-side glue.  Require DATA-BUF proof
               ;; so a real user prompt remains a user turn.
               (not (and (eq prev-type 'response)
                         (or (and (memq next-type
                                       '(reasoning ignored tool mailbox
                                         reminder render-data))
                                  (mevedel-view--scaffolding-only-p
                                   data-buf seg-start (caddr seg)))
                             (and (eq next-type 'response)
                                  data-buf
                                  (mevedel-view--scaffolding-only-p
                                   data-buf seg-start (caddr seg)))))))
          ;; Genuine user turn: either the first segment, or follows
          ;; a user/response segment.
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
               (system-reminder-p 'system-reminder)
               (request-summary-p 'response)
               (render-data-only-p prev-type)
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
;;    :body-mode SYMBOL         ; major-mode symbol for fontification (or nil)
;;    :status SYMBOL            ; optional visual status, e.g. success/error
;;    :expandable-p BOOL        ; nil means render as a compact event line
;;    :initially-collapsed-p BOOL)
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
                     (full-result (string-trim (substring text sexp-end)))
                     (full-result
                      (if (and (derived-mode-p 'org-mode)
                               (require 'org-src nil t)
                               (fboundp 'org-unescape-code-in-string))
                          (org-unescape-code-in-string full-result)
                        full-result))
                     (hook-audits
                      (mevedel-view--hook-audit-records-from-text
                       full-result))
                     (full-result
                      (mevedel--strip-hook-audit-blocks full-result))
                     (extract (mevedel-pipeline-extract-render-data
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
  `:body-mode' (if present) -- must be a symbol.
  `:status' (if present) -- must be a symbol.
  `:expandable-p' (if present) -- must be a boolean.
Malformed plists are rejected here so the interpreter never tries to
insert a non-string or `funcall' a non-symbol."
  (and (listp p)
       (stringp (plist-get p :header))
       (let ((body (plist-get p :body))
             (mode (plist-get p :body-mode))
             (status (plist-get p :status))
             (expandable (plist-get p :expandable-p)))
         (and (or (null body) (stringp body))
              (or (null mode) (symbolp mode))
              (or (not (plist-member p :status)) (symbolp status))
              (or (not (plist-member p :expandable-p))
                  (memq expandable '(nil t)))))))

(defun mevedel-view--tool-render-status (result)
  "Return the renderer dispatch status for RESULT."
  (if (mevedel-view--tool-result-error-p result) 'error 'success))

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
malformed returns are surfaced once via `display-warning' under
category `mevedel'; callers treat a nil return as \"use the generic
tool renderer\".

The renderer receives RENDER-DATA as-is (possibly nil): data-driven
renderers like the Edit/Write diff summary can check for their kind
and opt out; output-driven renderers (Grep, Bash, Read, ...) work
straight off ARGS and RESULT without needing render-data."
  (let* ((renderer (and tool (mevedel-tool-renderer tool)))
         (status (mevedel-view--tool-render-status result))
         (fn (and renderer
                  (mevedel-view--renderer-for-status renderer status))))
    (when renderer
      (let ((tool-label (or (and tool (mevedel-tool-name tool)) "tool")))
        (cond
         ((not fn)
          (when (mevedel-view--renderer-malformed-p renderer status)
            (display-warning
             'mevedel
             (format "Renderer for %s is not callable for status %s"
                     tool-label status)
             :warning))
          nil)
         (t
          (condition-case err
              (let ((plist (funcall fn tool-label args result render-data)))
                (cond
                 ((null plist) nil)
                 ((mevedel-view--rendering-plist-p plist) plist)
                 (t
                  (display-warning
                   'mevedel
                   (format "Renderer for %s returned malformed plist: %S"
                           tool-label plist)
                   :warning)
                  nil)))
            (error
             (display-warning
              'mevedel
              (format "Renderer for %s failed: %s"
                      tool-label (error-message-string err))
              :warning)
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

(defun mevedel-view--generic-tool-rendering (name args result &optional collapsed-only)
  "Return a generic rendering plist for parsed tool NAME, ARGS, and RESULT.
This is used for tools without a custom renderer, including third-party
and MCP-style tools that are not registered in mevedel's tool registry.
When COLLAPSED-ONLY is non-nil, omit the body from the returned plist."
  (let* ((tool-name (or name "Tool"))
         (primary (and (listp args)
                       (condition-case nil
                           (mevedel-tool-display-string tool-name args)
                         (error nil))))
         (lines (mevedel-view--tool-result-line-count result))
         (status (mevedel-view--tool-render-status result))
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
  (when (and (boundp 'mevedel--data-buffer)
             mevedel--data-buffer
             (buffer-live-p mevedel--data-buffer))
    (buffer-local-value 'major-mode mevedel--data-buffer)))

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

(defun mevedel-view--agent-invocation (agent-id)
  "Return the live invocation for AGENT-ID visible to this view."
  (cl-labels
      ((id-match-p
        (candidate)
        (or (equal candidate agent-id)
            (equal (mevedel-view--display-label-for-agent candidate)
                   agent-id)))
       (lookup-in-buffer
        (buf)
        (when (and buf (buffer-live-p buf))
          (with-current-buffer buf
            (catch 'found
              (dolist (pair mevedel-agent-runtime--fsms)
                (when (id-match-p (car pair))
                  (when-let* ((inv (ignore-errors
                                      (mevedel-agent-runtime--agent-invocation-at
                                       (cdr pair)))))
                    (throw 'found inv))))
              nil)))))
    (when-let* ((data-buf (and (boundp 'mevedel--data-buffer)
                               mevedel--data-buffer))
                ((buffer-live-p data-buf)))
      (or (lookup-in-buffer data-buf)
          (let ((session (buffer-local-value 'mevedel--session data-buf)))
            (catch 'found
              (dolist (buf (buffer-list))
                (when (and (buffer-live-p buf)
                           (eq (buffer-local-value 'mevedel--session buf)
                               session))
                  (when-let* ((inv (lookup-in-buffer buf)))
                    (throw 'found inv))))
              nil))))))

(defun mevedel-view-reset-agent-ephemeral-state (&optional view-buffer)
  "Reset view-local ephemeral agent UI state in VIEW-BUFFER.
Defaults to the current buffer."
  (with-current-buffer (or view-buffer (current-buffer))
    (require 'mevedel-view-zone)
    (mevedel-view-zone-set-collapse-state
     mevedel-view--status-agent-collapse-key nil)
    (mevedel-view--render-status)))

(defun mevedel-view--queue-has-origin-p (queue origin)
  "Return non-nil if QUEUE has an entry with ORIGIN."
  (let (found)
    (while (and queue (not found))
      (setq found (equal (plist-get (car queue) :origin) origin))
      (setq queue (cdr queue)))
    found))

(defun mevedel-view--queue-origin-fingerprint (queue)
  "Return the ORIGIN values in QUEUE for render cache invalidation."
  (mapcar (lambda (entry)
            (plist-get entry :origin))
          queue))

(defun mevedel-view--session-render-state-fingerprint (session)
  "Return state from SESSION that can affect cached tool renderings."
  (when session
    (list :permission-origins
          (mevedel-view--queue-origin-fingerprint
           (mevedel-session-permission-queue session))
          :plan-origins
          (mevedel-view--queue-origin-fingerprint
           (mevedel-session-plan-queue session))
          :agent-transcripts
          (mapcar (lambda (entry)
                    (let ((data (cdr entry)))
                      (list (car entry)
                            (plist-get data :status)
                            (plist-get data :path))))
                  (mevedel-session-agent-transcripts session)))))

(defun mevedel-view--agent-status-blocked-p (agent-id)
  "Return non-nil when AGENT-ID is waiting on a parent interaction queue."
  (when-let* ((data-buf (and (boundp 'mevedel--data-buffer)
                             mevedel--data-buffer))
              ((buffer-live-p data-buf))
              (session (buffer-local-value 'mevedel--session data-buf)))
    (or (mevedel-view--queue-has-origin-p
         (mevedel-session-permission-queue session) agent-id)
        (mevedel-view--queue-has-origin-p
         (mevedel-session-plan-queue session) agent-id))))

(defun mevedel-view--stamp-agent-handle (start end rendering)
  "Stamp START..END with handle properties from RENDERING."
  (when-let* ((agent-id (plist-get rendering :agent-id)))
    (add-text-properties
     start end
     `(mevedel-view-agent-id ,agent-id
       mevedel-view-agent-handle-p t
       mevedel-view-agent-status ,(plist-get rendering :agent-status)))))

(defun mevedel-view--rendering-header-face (rendering)
  "Return the face for RENDERING's visible header line."
  (if (and (eq (or (plist-get rendering :vtype) 'tool-summary)
               'agent-handle)
           (eq (plist-get rendering :agent-status) 'running))
      'mevedel-view-agent-running
    'mevedel-view-tool-summary))

(defun mevedel-view--buttonize-agent-header-label (line agent-id)
  "Return LINE with its visible agent type label made clickable.
AGENT-ID is stored on the label so it can open the same transcript the
old attribution suffix opened."
  (when (and agent-id
             (string-match "Agent:[ \t]+\\([^ \t\n]+\\)" line))
    (add-text-properties
     (match-beginning 1) (match-end 1)
     `(keymap ,mevedel-view--agent-label-map
       mouse-face highlight
       follow-link t
       help-echo "Open agent transcript"
       mevedel-view-agent-id ,agent-id)
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
    (if tool-split
        (mevedel-view--tool-call-line
         marker marker-face
         (nth 0 tool-split)
         (nth 1 tool-split)
         (nth 2 tool-split))
      (let ((line
             (mevedel-view--operation-line
              marker marker-face header nil nil
              (mevedel-view--rendering-header-face rendering))))
        (if agent-p
            (mevedel-view--buttonize-agent-header-label
             line (plist-get rendering :agent-id))
          line)))))

(defun mevedel-view--render-collapsed-header (rendering source)
  "Insert the collapsed header for RENDERING with SOURCE coordinates.
RENDERING is a rendering plist.  SOURCE is (DATA-START . DATA-END)."
  (let* ((vtype (or (plist-get rendering :vtype) 'tool-summary))
         (ins-start (point)))
    (mevedel-view--insert-summary-region
     (mevedel-view--rendering-header-line rendering)
     `(mevedel-view-type ,vtype
       mevedel-view-collapsed t
       mevedel-view-source ,source
       mevedel-view-source-key ,(mevedel-view--source-collapse-state-key
                                 source vtype)
       mevedel-view-rendered t))
    (when (eq vtype 'agent-handle)
      (mevedel-view--stamp-agent-handle ins-start (point) rendering))
    (mevedel-view--decorate-markdown-in-range ins-start (point))))

(defun mevedel-view--render-expanded-body (rendering source)
  "Insert the expanded body for RENDERING with SOURCE coordinates."
  (let* ((body (or (plist-get rendering :body) ""))
         (body-mode (plist-get rendering :body-mode))
         (vtype (or (plist-get rendering :vtype) 'tool-summary))
         (fontified (mevedel-view--fontify-as body body-mode))
         (header-line (mevedel-view--rendering-header-line rendering))
         (ins-start (point)))
    (insert header-line "\n")
    (when (eq vtype 'agent-handle)
      (mevedel-view--stamp-agent-handle ins-start (point) rendering))
    (insert fontified)
    (unless (eq (char-before) ?\n)
      (insert "\n"))
    (add-text-properties ins-start (point)
                         `(mevedel-view-type ,vtype
                           mevedel-view-collapsed nil
                           mevedel-view-source ,source
                           mevedel-view-source-key ,(mevedel-view--source-collapse-state-key
                                                     source vtype)
                           mevedel-view-rendered t))
    (mevedel-view--decorate-markdown-in-range ins-start (point))))

(defun mevedel-view--insert-rendered-tool (rendering source)
  "Insert a rendered tool block honouring RENDERING's initial state.
SOURCE is (DATA-START . DATA-END) identifying the data-buffer segment.
When `:initially-collapsed-p' is nil the body is inserted expanded;
otherwise only the header is shown.

When RENDERING carries `:expandable-p' nil, insert a compact event line
with no source coordinates so expand/collapse commands cannot reveal
the raw tool segment."
  (let ((hook-audits (plist-get rendering :hook-audits)))
    (setq rendering (mevedel-view--rendering-with-collapse-state rendering source))
    (if (and (plist-member rendering :expandable-p)
             (not (plist-get rendering :expandable-p)))
        (let ((ins-start (point)))
          (mevedel-view--insert-summary-region
           (mevedel-view--rendering-header-line
            (plist-put (copy-sequence rendering) :vtype 'tool-event))
           '(mevedel-view-type tool-event
             mevedel-view-rendered t))
          (mevedel-view--add-display-region-properties
           ins-start (point) 'tool-event)
          (mevedel-view--decorate-markdown-in-range ins-start (point)))
      (if (plist-member rendering :initially-collapsed-p)
          (if (plist-get rendering :initially-collapsed-p)
              (mevedel-view--render-collapsed-header rendering source)
            (mevedel-view--render-expanded-body rendering source))
        ;; Default: collapsed.
        (mevedel-view--render-collapsed-header rendering source)))
    (when hook-audits
      (let ((audit-start (point)))
        (dolist (audit hook-audits)
          (mevedel-view--insert-hook-audit-block audit source))
        (mevedel-view--add-display-region-properties
         audit-start (point) 'hook-audit)))))

(defun mevedel-view--tool-cache-key
    (data-buf seg-start seg-end collapsed-only raw)
  "Return a cache key for DATA-BUF SEG-START..SEG-END rendering.
RAW is the expanded tool segment text used for content-based invalidation.
COLLAPSED-ONLY records whether only collapsed rendering is needed.
Unrelated appends to DATA-BUF should not invalidate completed tool segment
renderings, but changes to the segment text itself should."
  (with-current-buffer data-buf
    (list data-buf seg-start seg-end (mevedel-view--tool-content-fingerprint raw)
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
  "Return RENDERING with its body omitted for collapsed-header caching."
  (if (mevedel-view--collapsed-rendering-p rendering)
      (plist-put (copy-sequence rendering) :body nil)
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
           (result (plist-get call :result))
           (tool (mevedel-tool-get name))
           (custom (and tool
                        (mevedel-view--invoke-renderer
                         tool
                         (plist-get call :render-data)
                         args
                         result)))
           (rendering (or custom
                          (mevedel-view--generic-tool-rendering
                           name args result collapsed-only))))
      (when-let* ((audits (append (plist-get rendering :hook-audits)
                                  (plist-get call :hook-audits))))
        (setq rendering (plist-put rendering :hook-audits audits)))
      (if collapsed-only
          (mevedel-view--omit-rendering-body-for-cache rendering)
        rendering))))

(defun mevedel-view--segment-rendering (data-buf seg-start seg-end
                                                 &optional collapsed-only)
  "Return the rendering plist for DATA-BUF's SEG-START..SEG-END tool segment.
Return nil only when the segment is malformed or unparseable.
Registered renderers get first chance; otherwise a generic rendering
keeps parseable tool calls from expanding into raw org scaffolding.
When COLLAPSED-ONLY is non-nil, cache a header rendering that omits large
bodies for initially collapsed tools."
  (let* ((raw (with-current-buffer data-buf
                (mevedel-view--tool-segment-text seg-start seg-end)))
         (cache (and (hash-table-p mevedel-view--tool-rendering-cache)
                     mevedel-view--tool-rendering-cache))
         (key (and cache
                   (mevedel-view--tool-cache-key
                    data-buf seg-start seg-end collapsed-only raw))))
    (or (and key (gethash key cache))
        (let ((rendering (mevedel-view--compute-segment-rendering
                          data-buf seg-start seg-end collapsed-only raw)))
          (if (and key rendering)
              (mevedel-view--cache-put cache key rendering
                                       'mevedel-view--render-cache-entries)
            rendering)))))


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

(defun mevedel-view--clean-reasoning-text (text)
  "Strip org scaffolding markers from reasoning TEXT.
Removes reasoning block markers, nested tool blocks, and generated
system reminder wrappers."
  (let ((cleaned (mevedel-view--strip-render-data-display-text text)))
    (setq cleaned (mevedel-view--strip-system-reminder-blocks cleaned))
    (setq cleaned (mevedel-view--strip-tool-blocks-from-reasoning cleaned))
    (setq cleaned (replace-regexp-in-string
                   "#\\+\\(?:begin\\|end\\)_reasoning[^\n]*\n?" "" cleaned))
    (setq cleaned (replace-regexp-in-string
                   "#\\+begin_tool[^\n]*\n?" "" cleaned))
    (setq cleaned (replace-regexp-in-string
                   "#\\+end_tool[^\n]*\n?" "" cleaned))
    cleaned))

(defun mevedel-view--strip-render-data-display-text (text)
  "Return TEXT without hidden render-data side-channel scaffolding."
  (require 'mevedel-pipeline)
  (let ((cleaned (mevedel-pipeline--strip-render-data-blocks (or text ""))))
    (setq cleaned
          (replace-regexp-in-string
           "^<!--[ \t]*/?mevedel-render-data[ \t]*-->[ \t]*\n?"
           "" cleaned))
    cleaned))

(defun mevedel-view--render-data-only-text-p (text)
  "Return non-nil if TEXT is only render-data scaffolding."
  (and (stringp text)
       (not (string-empty-p (string-trim text)))
       (string-empty-p
        (string-trim
         (mevedel-view--strip-render-data-display-text text)))))

(defun mevedel-view--render-data-only-segment-p (data-buf seg-start seg-end)
  "Return non-nil when DATA-BUF's SEG-START..SEG-END is only hidden render-data."
  (with-current-buffer data-buf
    (mevedel-view--render-data-only-text-p
     (buffer-substring-no-properties seg-start seg-end))))

(defun mevedel-view--hook-audit-only-text-p (text)
  "Return non-nil if TEXT is only hook audit scaffolding."
  (require 'mevedel-transcript-audit)
  (mevedel-transcript-audit-only-p text))

(defun mevedel-view--hook-audit-only-segment-p (data-buf seg-start seg-end)
  "Return non-nil when DATA-BUF's SEG-START..SEG-END is only hook audit data."
  (with-current-buffer data-buf
    (mevedel-view--hook-audit-only-text-p
     (buffer-substring-no-properties seg-start seg-end))))

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

(defun mevedel-view--system-reminder-summary (data-buf seg-start seg-end)
  "Return collapsed summary for DATA-BUF's SEG-START..SEG-END system reminder."
  (with-current-buffer data-buf
    (let* ((text (buffer-substring-no-properties seg-start seg-end))
           (body (mevedel-view--system-reminder-body-from-text text))
           (lines (max 1 (mevedel-view--system-reminder-line-count body))))
      (propertize
       (format "  \u25c7 System reminder (%d %s)"
               lines
               (if (= lines 1) "line" "lines"))
       'font-lock-face 'mevedel-view-system-reminder))))

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
             (e (max pmin (min seg-end pmax)))
             (text (and (< s e)
                        (buffer-substring-no-properties s e)))
             (cleaned (and text (mevedel-view--clean-reasoning-text text))))
        (or (null text)
            (string-empty-p (string-trim cleaned)))))))

(defun mevedel-view--inline-skill-render-data-from-text (text)
  "Return inline-skill render-data from TEXT, or nil."
  (let ((data (cdr (mevedel-pipeline-extract-render-data text))))
    (and (consp data)
         (eq (plist-get data :kind) 'inline-skill)
         data)))

(defun mevedel-view--agent-transcript-render-data-from-text (text)
  "Return agent-transcript render-data from TEXT, or nil."
  (let ((data (cdr (mevedel-pipeline-extract-render-data text))))
    (and (consp data)
         (eq (plist-get data :kind) 'agent-transcript)
         data)))

(defun mevedel-view--request-summary-render-data-from-text (text)
  "Return request-summary render-data from TEXT, or nil."
  (let ((data (cdr (mevedel-pipeline-extract-render-data text))))
    (and (consp data)
         (eq (plist-get data :kind) 'request-summary)
         data)))

(defun mevedel-view--inline-skill-render-segment-p
    (data-buf seg-start seg-end)
  "Return non-nil when DATA-BUF's SEG-START..SEG-END carries inline skill data."
  (with-current-buffer data-buf
    (mevedel-view--inline-skill-render-data-from-text
     (buffer-substring-no-properties seg-start seg-end))))

(defun mevedel-view--agent-transcript-render-segment-p
    (data-buf seg-start seg-end)
  "Return non-nil when DATA-BUF's SEG-START..SEG-END carries transcript data."
  (with-current-buffer data-buf
    (mevedel-view--agent-transcript-render-data-from-text
     (buffer-substring-no-properties seg-start seg-end))))

(defun mevedel-view--request-summary-render-segment-p
    (data-buf seg-start seg-end)
  "Return non-nil when DATA-BUF's SEG-START..SEG-END carries a request summary."
  (with-current-buffer data-buf
    (mevedel-view--request-summary-render-data-from-text
     (buffer-substring-no-properties seg-start seg-end))))

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
                         (buffer-substring-no-properties block-start close))
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
              (request (buffer-local-value 'mevedel--current-request data-buf))
              (started-at (mevedel-request-started-at request)))
    (float-time (time-subtract (current-time) started-at))))

(defun mevedel-view--append-request-summary (data-buf search-start)
  "Append hidden request-summary render-data to DATA-BUF if needed.
SEARCH-START bounds duplicate detection to the current response tail.
Return the new data-buffer end position."
  (when-let* ((elapsed (mevedel-view--request-summary-elapsed-seconds
                        data-buf)))
    (require 'mevedel-pipeline)
    (with-current-buffer data-buf
      (let ((tail-start (or search-start (point-min))))
        (mevedel-view--delete-request-summaries
         data-buf tail-start (point-max))
        (save-excursion
          (goto-char (point-max))
          (unless (bolp) (insert "\n"))
          (let ((start (point)))
            (insert (mevedel-pipeline--format-render-data-block
                     (list :kind 'request-summary
                           :elapsed-seconds elapsed)))
            (add-text-properties start (point) '(gptel ignore))))))
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
  "Return TEXT without synthetic review `<user_action>' blocks."
  (if (fboundp 'mevedel-review-strip-user-action-blocks)
      (mevedel-review-strip-user-action-blocks text)
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (while (search-forward "<user_action>" nil t)
        (let ((start (match-beginning 0)))
          (if (search-forward "</user_action>" nil t)
              (let ((end (point))
                    (block (buffer-substring-no-properties start (point))))
                (when (string-match-p "<action>review</action>" block)
                  (when (and (< end (point-max))
                             (eq (char-after end) ?\n))
                    (cl-incf end))
                  (delete-region start end)
                  (goto-char start)))
            (goto-char (point-max)))))
      (string-trim (buffer-string)))))

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
             (lines (split-string cleaned "\n" t "[ \t]+")))
        (if lines
            (concat
             "  "
             (propertize mevedel-view--thinking-glyph
                         'font-lock-face 'mevedel-view-thinking-marker)
             (propertize (format "Thinking... (%d lines)" (length lines))
                         'font-lock-face 'mevedel-view-thinking-summary))
          "")))))


;;
;;; Rendering

(defun mevedel-view--strip-proposed-plans-p (text)
  "Return non-nil when TEXT's proposed-plan protocol blocks should be hidden.
Plan-mode responses are hidden live while Plan mode is active.  After
mode exit, full rerenders still hide previously presented plan bodies so
historical Plan-mode protocol does not leak back into the view."
  (and (boundp 'mevedel--session)
       mevedel--session
       (or (eq (mevedel-session-permission-mode mevedel--session) 'plan)
           (and (fboundp 'mevedel-plan-mode-extract-proposed-plan)
                (fboundp 'mevedel-plan-mode-known-proposed-plan-p)
                (let ((proposed
                       (mevedel-plan-mode-extract-proposed-plan text)))
                  (and proposed
                       (mevedel-plan-mode-known-proposed-plan-p
                        proposed mevedel--session)))))))

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
         (pos after-header)
         last-history)
    (while (< pos limit)
      (when (mevedel-view--transcript-history-position-p pos)
        (setq last-history (1+ pos)))
      (setq pos (1+ pos)))
    (or last-history after-header)))

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

(defvar mevedel-view--collapsible-vtypes)

(defun mevedel-view--source-collapse-vtype-p (vtype)
  "Return non-nil when VTYPE can be restored from source coordinates."
  (or (memq vtype mevedel-view--collapsible-vtypes)
      (eq vtype 'agent-handle)))

(defconst mevedel-view--missing-collapse-state
  (make-symbol "mevedel-view-missing-collapse-state")
  "Sentinel for absent source-backed collapse state entries.")

(defun mevedel-view--source-start-position (source)
  "Return SOURCE's numeric start position, or nil."
  (when (and source (consp source) (integer-or-marker-p (car source)))
    (if (markerp (car source))
        (marker-position (car source))
      (car source))))

(defun mevedel-view--source-in-flight-turn-p (source)
  "Return non-nil when SOURCE belongs to the active in-flight turn."
  (when-let* ((start (mevedel-view--source-start-position source))
              (turn-start (cond
                           ((markerp mevedel-view--data-turn-start)
                            (marker-position mevedel-view--data-turn-start))
                           ((integerp mevedel-view--data-turn-start)
                            mevedel-view--data-turn-start))))
    (>= start turn-start)))

(defun mevedel-view--source-collapse-anchor (source)
  "Return a render-time identity anchor for SOURCE in the data buffer.
The anchor should remain stable when a source-backed segment extends in
place, but change when a later rewrite reuses the same numeric start."
  (when (and source
             (consp source)
             (integer-or-marker-p (car source))
             (integer-or-marker-p (cdr source))
             (boundp 'mevedel--data-buffer)
             (buffer-live-p mevedel--data-buffer))
    (let ((data-buf mevedel--data-buffer)
          (in-flight-p (mevedel-view--source-in-flight-turn-p source)))
      (with-current-buffer data-buf
        (let* ((pmin (point-min))
               (pmax (point-max))
               (start (if (markerp (car source))
                          (marker-position (car source))
                        (car source)))
               (end (if (markerp (cdr source))
                        (marker-position (cdr source))
                      (cdr source)))
               (start (and start (max pmin (min start pmax))))
               (end (and end (max pmin (min end pmax)))))
          (when (and start end (< start end))
            (or
             (let ((pos start)
                   tool-id)
               (while (and (< pos end) (not tool-id))
                 (let ((prop (get-text-property pos 'gptel)))
                   (when (and (consp prop) (eq (car prop) 'tool))
                     (setq tool-id (cdr prop))))
                 (setq pos (or (next-single-property-change
                                pos 'gptel nil end)
                               end)))
               (and tool-id (list 'tool tool-id)))
             (and in-flight-p '(in-flight))
             (md5 (buffer-substring-no-properties start end)))))))))

(defun mevedel-view--source-collapse-state-key (source vtype)
  "Return the source-backed collapse-state key for SOURCE and VTYPE."
  (when (and source
             (consp source)
             (mevedel-view--source-start-position source)
             (mevedel-view--source-collapse-vtype-p vtype))
    (list 'source vtype
          (mevedel-view--source-start-position source)
          (mevedel-view--source-collapse-anchor source))))

(defun mevedel-view--source-collapse-in-flight-key-p (key)
  "Return non-nil when KEY has the temporary in-flight anchor."
  (and (consp key)
       (eq (car key) 'source)
       (equal (nth 3 key) '(in-flight))))

(defun mevedel-view--ensure-source-collapse-states ()
  "Return the view-local source-backed collapse-state table."
  (unless (hash-table-p mevedel-view--source-collapse-states)
    (setq mevedel-view--source-collapse-states
          (make-hash-table :test #'equal)))
  mevedel-view--source-collapse-states)

(defun mevedel-view--record-source-collapse-state (source vtype collapsed)
  "Record source-backed collapse state for SOURCE and VTYPE.
COLLAPSED is stored as t for collapsed and nil for expanded."
  (when-let* ((key (mevedel-view--source-collapse-state-key source vtype)))
    (puthash key (and collapsed t)
             (mevedel-view--ensure-source-collapse-states))))

(defun mevedel-view--source-collapse-state-entry (source vtype)
  "Return saved collapse state entry for SOURCE and VTYPE, or nil.
The returned cons is (KEY . COLLAPSED), where COLLAPSED may be nil for
an explicitly expanded section."
  (when-let* ((key (mevedel-view--source-collapse-state-key source vtype))
              ((hash-table-p mevedel-view--source-collapse-states)))
    (let ((value (gethash key mevedel-view--source-collapse-states
                          mevedel-view--missing-collapse-state)))
      (unless (eq value mevedel-view--missing-collapse-state)
        (cons key value)))))

(defun mevedel-view--rendering-with-collapse-state (rendering source)
  "Return RENDERING with saved collapse state from SOURCE applied.
When no saved state exists, return RENDERING unchanged."
  (if-let* ((rendering rendering)
            (vtype (or (plist-get rendering :vtype) 'tool-summary))
            (entry (mevedel-view--source-collapse-state-entry source vtype)))
      (plist-put (copy-sequence rendering)
                 :initially-collapsed-p (cdr entry))
    rendering))

(defun mevedel-view--collapse-state-next-change (pos limit)
  "Return the next fold-relevant property change after POS before LIMIT."
  (let ((next limit))
    (dolist (prop '(mevedel-view-source
                    mevedel-view-type
                    mevedel-view-mailbox-card))
      (when-let* ((change (next-single-property-change pos prop nil limit)))
        (setq next (min next change))))
    next))

(defun mevedel-view--mailbox-section-bounds-at (position)
  "Return bounds of the mailbox card at POSITION, or nil."
  (let ((card (get-text-property position 'mevedel-view-mailbox-card)))
    (when card
      (let ((start (or (previous-single-property-change
                        position 'mevedel-view-mailbox-card)
                       (point-min)))
            (end (or (next-single-property-change
                      position 'mevedel-view-mailbox-card)
                     (point-max))))
        (when (and (< start position)
                   (not (eq (get-text-property
                             start 'mevedel-view-mailbox-card)
                            card)))
          (setq start (or (next-single-property-change
                           start 'mevedel-view-mailbox-card)
                          position)))
        (cons start end)))))

(defun mevedel-view--mailbox-body-text (start end)
  "Return the visible payload text for a mailbox card between START and END."
  (string-join
   (mapcar (lambda (range)
             (buffer-substring-no-properties (car range) (cdr range)))
           (mevedel-view--mailbox-body-ranges start end))
   "\n"))

(defun mevedel-view--mailbox-collapse-state-key (position counts)
  "Return a stable collapse-state key for the mailbox card at POSITION.
COUNTS is a hash table tracking repeated equivalent cards while the
caller scans the render span in display order."
  (when-let* ((bounds (mevedel-view--mailbox-section-bounds-at position)))
    (let* ((kind (or (get-text-property
                      (car bounds) 'mevedel-view-mailbox-kind)
                     'agent-message))
           (agent-id (get-text-property
                      (car bounds) 'mevedel-view-mailbox-agent-id))
           (body-hash (md5 (mevedel-view--mailbox-body-text
                            (car bounds) (cdr bounds))))
           (base (list 'mailbox-delivery kind agent-id body-hash))
           (index (1+ (or (gethash base counts) 0))))
      (puthash base index counts)
      (append base (list index)))))

(defun mevedel-view--capture-collapse-states (from to)
  "Return an alist of collapse states for sections in FROM..TO.

Source-backed keys use the segment vtype, the car of
`mevedel-view-source', and the render-time source anchor.  Values are t
when collapsed, nil when expanded.  Identity is keyed on the data-start
only (not the full source cons) plus that anchor so thinking-summary and
tool-summary segments keep their saved state when streaming extends the
segment's end position, but rewritten data at the same numeric start does
not inherit stale state.  Locally decorated mailbox cards use their
rendered kind, agent id, body hash, and ordinal."
  (let ((mailbox-counts (make-hash-table :test 'equal))
        (states nil)
        (pos from))
    (while (< pos to)
      (let* ((vtype (get-text-property pos 'mevedel-view-type))
             (source (get-text-property pos 'mevedel-view-source))
             (collapsed (get-text-property pos 'mevedel-view-collapsed))
             (source-key (get-text-property pos 'mevedel-view-source-key))
             (mailbox-bounds
              (and (eq vtype 'mailbox-delivery)
                   (mevedel-view--mailbox-section-bounds-at pos)))
             (next (if mailbox-bounds
                       (min to (cdr mailbox-bounds))
                     (mevedel-view--collapse-state-next-change pos to)))
             (key
              (cond
               ((mevedel-view--source-in-flight-turn-p source)
                (mevedel-view--source-collapse-state-key source vtype))
               ((mevedel-view--source-collapse-in-flight-key-p source-key)
                (mevedel-view--source-collapse-state-key source vtype))
               (source-key)
               ((mevedel-view--source-collapse-state-key source vtype))
               (mailbox-bounds
                (mevedel-view--mailbox-collapse-state-key
                 pos mailbox-counts)))))
        (when (and key (not (assoc key states)))
          (let ((state (and collapsed t)))
            (push (cons key state) states)
            (when (eq (car key) 'source)
              (puthash key state
                       (mevedel-view--ensure-source-collapse-states)))))
        (setq pos next)))
    states))

(defun mevedel-view--apply-collapse-states (from to states)
  "Toggle sections in FROM..TO so collapse state matches STATES.
STATES is an alist from `mevedel-view--capture-collapse-states'.
Sections whose current state already matches are left alone; only
mismatches are toggled, via `mevedel-view--expand-section' /
`--collapse-section' or the mailbox card toggle.  Upper bound is held as
a marker so toggles that change buffer length do not invalidate the walk."
  (when states
    (save-excursion
      (let ((mailbox-counts (make-hash-table :test 'equal))
            (to-marker (copy-marker to t)))
        (unwind-protect
            (progn
              (let ((pos from))
                (while (< pos (marker-position to-marker))
                  (let* ((vtype (get-text-property pos 'mevedel-view-type))
                         (source (get-text-property pos 'mevedel-view-source))
                         (collapsed (and (get-text-property
                                          pos 'mevedel-view-collapsed)
                                         t))
                         (source-key (get-text-property
                                      pos 'mevedel-view-source-key))
                         (mailbox-bounds
                          (and (eq vtype 'mailbox-delivery)
                               (mevedel-view--mailbox-section-bounds-at pos)))
                         (key
                          (cond
                           ((mevedel-view--source-in-flight-turn-p source)
                            (mevedel-view--source-collapse-state-key source vtype))
                           ((mevedel-view--source-collapse-in-flight-key-p source-key)
                            (mevedel-view--source-collapse-state-key source vtype))
                           (source-key)
                           ((mevedel-view--source-collapse-state-key source vtype))
                           (mailbox-bounds
                            (mevedel-view--mailbox-collapse-state-key
                             pos mailbox-counts)))))
                    (when-let* ((entry (and key (assoc key states)))
                                ((not (eq collapsed (cdr entry)))))
                      (goto-char pos)
                      (cond
                       ((eq vtype 'mailbox-delivery)
                        (mevedel-view--toggle-mailbox-delivery))
                       ((cdr entry)
                        (mevedel-view--collapse-section source vtype))
                       (t
                        (mevedel-view--expand-section source vtype))))
                    (setq pos
                          (if mailbox-bounds
                              (min (marker-position to-marker)
                                   (or (cdr (mevedel-view--mailbox-section-bounds-at
                                             pos))
                                       (cdr mailbox-bounds)))
                            (mevedel-view--collapse-state-next-change
                             pos (marker-position to-marker))))))))
          (set-marker to-marker nil))))))

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
                     (integerp (car source))
                     (>= (car source) data-from))
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
  (when-let* ((pos (mevedel-view--in-flight-turn-start-position))
              ((> pos (point-min))))
    (save-excursion
      (goto-char pos)
      (skip-chars-backward " \t\n")
      (and (> (point) (point-min))
           (eq (get-text-property (1- (point)) 'mevedel-view-type)
               'user)))))

(defun mevedel-view--render-incremental (data-buf &optional start end)
  "Rebuild the in-flight assistant turn in the view from DATA-BUF.

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
  (require 'mevedel-transcript)
  (let* ((turn-from (and (markerp mevedel-view--data-turn-start)
                         (marker-position mevedel-view--data-turn-start)))
         (data-from (cond
                     ((and start turn-from) (min start turn-from))
                     (start)
                     (turn-from)))
         (data-to
          (or end
              (with-current-buffer data-buf (point-max))))
         (segments (when (and data-from data-to)
                     (with-current-buffer data-buf
                       (mevedel-transcript-segments data-from data-to))))
         (turns (mevedel-view--group-into-turns segments data-buf))
         (in-flight-p (mevedel-view--in-flight-turn-start-position))
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
                (not (mevedel-view--queued-user-message-batch-turn-p
                      (car turns) data-buf))
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
                  (or (and in-flight-p
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
                    (mevedel-view--capture-collapse-states
                     delete-start
                     rebuild-end-pos))))
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
            (let ((mevedel-view--render-insertion-marker rebuild-end))
              (dolist (turn turns)
                (mevedel-view--render-turn turn data-buf))
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
          (when (and saved-states
                     delete-start
                     rebuild-end
                     (marker-position rebuild-end))
            (mevedel-view--apply-collapse-states
             delete-start
             (marker-position rebuild-end)
             saved-states))
          (mevedel-view--ensure-request-progress data-buf)
          (unless mevedel-view--agent-transcript-p
            (mevedel-view--render-agent-status)
            (mevedel-view--interaction-rebuild)))))))))

(defun mevedel-view--render-turn (turn data-buf)
  "Render a single TURN into the view buffer at the input marker.
DATA-BUF is the gptel data buffer for reading source content.
TURN is a plist with :role, :segments, :start, :end."
  (let ((role (plist-get turn :role))
        (segments (plist-get turn :segments))
        (turn-start (plist-get turn :start))
        (turn-end (plist-get turn :end)))
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
              ('user
               (mevedel-view--render-user-turn segments data-buf))
              ('assistant
               (mevedel-view--render-assistant-turn segments data-buf)))
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
            (mevedel-view--add-display-region-properties
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
                                       (cons turn-start turn-end))
                    (setq pos next))))))
            ;; Tag every character in this turn with a unique id so
            ;; turn-level fold/unfold can find the whole span even after
            ;; inner sections have been expanded or collapsed.
            (put-text-property insert-start (point)
                               'mevedel-view-turn-id
                               (cl-gensym "mevedel-view-turn-")))))))))

(defun mevedel-view--user-turn-text (segments data-buf)
  "Extract cleaned user text from SEGMENTS in DATA-BUF.
Returns the concatenated, trimmed text with org scaffolding removed.
Empty string when the turn contains only whitespace or markers."
  (with-current-buffer data-buf
    (let* ((batch-text (mevedel-view--queued-user-message-batch-normalized-text
                        (mevedel-view--segments-raw-text segments data-buf)))
           (batch-display
            (and (mevedel-view--queued-user-message-batch-items-from-text
                  batch-text)
                 (mevedel-view--queued-user-message-batch-display-text
                  batch-text))))
      (if batch-display
          (string-trim (mevedel-view--strip-hook-context-blocks batch-display))
        (let ((parts nil))
          (dolist (seg segments)
            (let* ((seg-start (cadr seg))
                   (seg-end (caddr seg))
                   (text (buffer-substring-no-properties seg-start seg-end)))
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
              ;; Queued batches are model-visible control wrappers around real
              ;; follow-up messages.  Render only the follow-up bodies.
              (setq text (mevedel-view--queued-user-message-batch-display-text text))
              ;; Strip model-only prompt context added by UserPromptSubmit hooks.
              (setq text (mevedel-view--strip-hook-context-blocks text))
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
          (string-join (nreverse parts) "\n"))))))

(defun mevedel-view--strip-hook-context-blocks (text)
  "Return TEXT without generated `<hook-context>' blocks."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (while (search-forward "<hook-context>" nil t)
      (let ((start (match-beginning 0)))
        (if (search-forward "</hook-context>" nil t)
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
(autoload 'mevedel-view--toggle-hook-audit "mevedel-view-audit")
(autoload 'mevedel-view--decorate-code-blocks-in-range
  "mevedel-view-markdown")
(autoload 'mevedel-view--decorate-local-images-in-range
  "mevedel-view-markdown")
(autoload 'mevedel-view--decorate-markdown-in-range
  "mevedel-view-markdown")
(autoload 'mevedel-view--linkify-paths-in-range "mevedel-view-markdown")
(autoload 'mevedel-view--prettify-markdown-tables-in-range
  "mevedel-view-markdown")

(defun mevedel-view--inline-skill-prompt-summary-body (text)
  "Return collapsed prompt body for inline-skill TEXT, or nil."
  (when (mevedel-view--inline-skill-render-data-from-text text)
    (let ((body (mevedel-view--strip-render-data-display-text text)))
      (setq body (mevedel--strip-hook-audit-blocks body))
      (setq body (mevedel-view--strip-review-action-blocks body))
      (setq body (mevedel-view--strip-hook-context-blocks body))
      (when (string-match
             "\\`[ \t\n]*:PROPERTIES:\n\\(?:.*\n\\)*?:END:\n?"
             body)
        (setq body (replace-match "" t t body)))
      (setq body (string-trim body))
      (unless (string-empty-p body)
        body))))

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

(defun mevedel-view--queued-user-message-batch-items-from-text (text)
  "Return generated queued user-message items parsed from TEXT, or nil."
  (require 'mevedel-transcript)
  (mevedel-transcript--queued-user-message-batch-items-from-text text))

(defun mevedel-view--queued-user-message-batch-normalized-text (text)
  "Return TEXT normalized for generated queued-message batch parsing."
  (let ((text (string-trim-left
               (mevedel--strip-hook-audit-blocks text))))
    (when (string-match "\\`\\*+ " text)
      (setq text (substring text (match-end 0))))
    text))

(defun mevedel-view--segments-raw-text (segments data-buf)
  "Return raw DATA-BUF text covered by SEGMENTS."
  (with-current-buffer data-buf
    (mapconcat
     (lambda (seg)
       (buffer-substring-no-properties (cadr seg) (caddr seg)))
     segments
     "")))

(defun mevedel-view--queued-user-message-batch-turn-p (turn data-buf)
  "Return non-nil when TURN in DATA-BUF has a generated message batch."
  (and (eq (plist-get turn :role) 'user)
       (mevedel-view--queued-user-message-batch-items-from-text
        (mevedel-view--queued-user-message-batch-normalized-text
         (mevedel-view--segments-raw-text
          (plist-get turn :segments)
          data-buf)))))

(defun mevedel-view--queued-user-message-batch-display-text (text)
  "Return view display text for queued-message batch TEXT."
  (if-let* ((items (mevedel-view--queued-user-message-batch-items-from-text
                    text)))
      (let* ((items (mapcar #'mevedel--strip-hook-audit-blocks items))
             (label (if (= (length items) 1)
                       "Queued message"
                     (format "Queued messages (%d)" (length items)))))
        (string-join (cons label items) "\n\n"))
    text))

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
         mevedel-view-source-key ,(mevedel-view--source-collapse-state-key
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

(defun mevedel-view--toggle-hook-context ()
  "Toggle a hook context disclosure."
  (let* ((bounds (or (mevedel-view--hook-context-section-bounds)
                     (mevedel-view--section-bounds)))
         (source (and bounds
                      (get-text-property
                       (car bounds) 'mevedel-view-source)))
         (events (or (and bounds
                          (get-text-property
                           (car bounds) 'mevedel-view-hook-context-events))
                     (and source
                          (buffer-live-p mevedel--data-buffer)
                          (mevedel-view--hook-context-events-from-text
                           (mevedel-view--data-substring
                            mevedel--data-buffer
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
        (mevedel-view--record-source-collapse-state source 'hook-context
                                                     (not collapsed))
        (when turn-id
          (put-text-property start (point)
                             'mevedel-view-turn-id turn-id))
        (mevedel-view--add-display-region-properties
         start (point) 'hook-context)))))

(defun mevedel-view--inline-skill-info (segments data-buf)
  "Return inline-skill render info from SEGMENTS in DATA-BUF, or nil."
  (with-current-buffer data-buf
    (let (info hook-audits)
      (dolist (seg segments)
        (when (memq (car seg)
                    '(user queued-message hook-context prompt
                      render-data ignored))
          (let ((text (buffer-substring-no-properties
                       (cadr seg) (caddr seg))))
            (unless info
              (setq info
                    (mevedel-view--inline-skill-render-data-from-text text)))
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

(defun mevedel-view--decorate-mailbox-block
    (open-regex close-tag start end &optional kind)
  "Replace OPEN-REGEX/CLOSE-TAG regions from START to END with mailbox cards.
KIND identifies the mailbox block flavor.  Shared engine for
`<agent-message>' and `<agent-result>'
rendering.  OPEN-REGEX must capture the agent-id in match group
1.  Body between the matched open and close tags is preserved
verbatim; if its line count exceeds CLOSE-TAG's threshold,
`mevedel-view-mailbox-collapse-line-threshold', the body is marked
invisible (with the `mailbox-delivery' vtype tag for downstream
TAB-toggle wiring) and the header gets a `[N lines collapsed]'
hint.  Searches that region."
  (save-excursion
    (let ((end-marker (copy-marker end t)))
      (unwind-protect
          (progn
            (goto-char start)
            (while (re-search-forward open-regex (marker-position end-marker) t)
              (let* ((open-start (match-beginning 0))
                     (open-end (match-end 0))
                     (id (match-string-no-properties 1))
                     (attribution (mevedel-view--insert-attribution id))
                     (inhibit-read-only t))
                (delete-region open-start open-end)
                (goto-char open-start)
                (let ((card-start (point))
                      (card-id (cl-gensym "mevedel-view-mailbox-")))
                  (insert (propertize
                           (pcase kind
                             ('agent-result "✓ finished ")
                             (_ "✉ message "))
                           'font-lock-face 'mevedel-view-attribution
                           'mevedel-view-mailbox t))
                  (if (eq kind 'agent-result)
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
                      (let* ((body-end (car close))
                             (close-end (cdr close))
                             (body-line-count
                              (mevedel-view--mailbox-body-line-count
                               body-start body-end))
                             (long-body
                              (> body-line-count
                                 mevedel-view-mailbox-collapse-line-threshold)))
                        (mevedel-view--debug-log
                         'mailbox-decorate
                         :kind kind
                         :id id
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
                                   (mevedel-view--mailbox-collapse-hint
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
                         '(mevedel-view-agent-id nil))
                        (add-text-properties
                         card-start (point)
                         (list 'mevedel-view-type 'mailbox-delivery
                               'mevedel-view-mailbox-card card-id
                               'mevedel-view-mailbox-kind
                               (or kind 'agent-message)
                               'mevedel-view-mailbox-agent-id id
                               'mevedel-view-collapsed long-body)))))))))
        (set-marker end-marker nil)))))

(defun mevedel-view--decorate-agent-result-blocks (start end)
  "Render agent result blocks from START to END as mailbox cards.
Delegates to `mevedel-view--decorate-mailbox-block' so
`<agent-message>' and `<agent-result>' render uniformly: same
header, same collapse threshold, and the same vtype tag for downstream
TAB toggling."
  (mevedel-view--decorate-mailbox-block
   "<agent-result\\s-+[^>]*agent-id=\"\\([^\"]+\\)\"[^>]*>"
   "</agent-result>"
   start end
   'agent-result))

(defun mevedel-view--decorate-agent-message-blocks (start end)
  "Decorate `<agent-message from=ID>...</agent-message>' from START to END.
Delegates to `mevedel-view--decorate-mailbox-block' so the body
collapse threshold, click gating, and vtype tag are uniform with
`<agent-result>' rendering.

Multiple `<agent-message>' blocks in one user turn produce one mailbox
card each, in source order.  Non-matching prose in the same turn
remains as ordinary user text."
  (mevedel-view--decorate-mailbox-block
   "<agent-message\\s-+[^>]*from=\"\\([^\"]+\\)\"[^>]*>"
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

(defun mevedel-view--render-user-turn (segments data-buf)
  "Render user SEGMENTS from DATA-BUF."
  (require 'mevedel-transcript)
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
      (insert (propertize "You\n"
                          'font-lock-face 'mevedel-view-user-header
                          'mevedel-view-type 'turn-header
                          'mevedel-view-turn-role 'user
                          'mevedel-view-collapsed nil))
      (setq text-start (point))
      (unless (string-empty-p text)
        (insert text)
        (unless (eq (char-before) ?\n)
          (insert "\n")))
      ;; Decorate mailbox blocks that appear inside mixed user text.
      (mevedel-view--decorate-agent-result-blocks text-start (point))
      (mevedel-view--decorate-agent-message-blocks text-start (point))
      (dolist (ctx hook-contexts)
        (mevedel-view--insert-hook-context-block
         (plist-get ctx :events)
         (cons (plist-get ctx :start)
               (plist-get ctx :end))))
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
         (cons (plist-get drawer :start)
               (plist-get drawer :end))))
      (when (and inline-skill inline-source-seg)
        (mevedel-view--insert-rendered-tool
         (list :header "Prompt"
               :body raw-text
               :body-mode 'markdown-mode
               :vtype 'prompt-summary
               :initially-collapsed-p t)
         (cons (cadr inline-source-seg)
               (caddr inline-source-seg)))))))
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
            (mevedel-view--directive-action-label action)
          (format "%s: %s"
                  (mevedel-view--directive-action-label action)
                  body))))
     ((string-match "\\``\\([^`]+\\)`[ \t\n]+\\(.+\\)\\'" trimmed)
      (format "%s: %s"
              (mevedel-view--directive-action-label (match-string 1 trimmed))
              (match-string 2 trimmed)))
     (t trimmed))))

(defconst mevedel-view--directive-action-labels
  '(("implement" . "Implement")
    ("revise" . "Revise")
    ("discuss" . "Discuss")
    ("tutor" . "Tutor"))
  "Plain display labels for directive actions.")

(defun mevedel-view--directive-action-label (action)
  "Return the display label for directive ACTION."
  (or (cdr (assoc (format "%s" action) mevedel-view--directive-action-labels))
      (capitalize (replace-regexp-in-string
                   "[-_]+" " " (format "%s" action)))))

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
           (summary (mevedel-view--thinking-summary
                     data-buf first-start last-end)))
      (unless (string-empty-p summary)
        (mevedel-view--insert-activity-rule-after-response)
        (mevedel-view--insert-summary-region
         (mevedel-view--summary-with-face
          summary 'mevedel-view-thinking-summary)
         `(mevedel-view-type thinking-summary
           mevedel-view-collapsed t
           mevedel-view-source ,(cons first-start last-end)
           mevedel-view-source-key ,(mevedel-view--source-collapse-state-key
                                     (cons first-start last-end)
                                     'thinking-summary)))))))

(defun mevedel-view--render-system-reminder-segment (seg data-buf)
  "Render system-reminder SEG from DATA-BUF as a collapsed control row."
  (let* ((seg-start (cadr seg))
         (seg-end (caddr seg))
         (summary (mevedel-view--system-reminder-summary
                   data-buf seg-start seg-end)))
    (mevedel-view--insert-activity-rule-after-response)
    (mevedel-view--insert-summary-region
     summary
     `(mevedel-view-type system-reminder-summary
       mevedel-view-collapsed t
       mevedel-view-source ,(cons seg-start seg-end)
       mevedel-view-source-key ,(mevedel-view--source-collapse-state-key
                                 (cons seg-start seg-end)
                                 'system-reminder-summary)))))

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
             (buffer-substring-no-properties seg-start seg-end))))
         (line (and render-data
                    (mevedel-view--request-summary-line render-data))))
    (when line
      (let ((start (point)))
        (insert (propertize (concat line "\n")
                            'font-lock-face 'mevedel-view-separator))
        (add-text-properties
         start (point)
         `(mevedel-view-type request-summary
           mevedel-view-source ,(cons seg-start seg-end)
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

(defun mevedel-view--render-assistant-turn (segments data-buf)
  "Render assistant SEGMENTS from DATA-BUF.
Response text is shown inline, tool calls as collapsed one-liners,
reasoning blocks as collapsed summaries.  Adjacent thinking segments
are merged into a single summary."
  (insert (propertize "Assistant\n"
                      'font-lock-face 'mevedel-view-assistant-header
                      'mevedel-view-type 'turn-header
                      'mevedel-view-turn-role 'assistant
                      'mevedel-view-collapsed nil))
  (let (tool-group thinking-group request-summary-group)
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
                 (seg-end (caddr seg)))
             (with-current-buffer data-buf
               (let ((text (string-trim
                             (buffer-substring-no-properties seg-start seg-end))))
                 (setq text (mevedel-view--visible-response-text text))
                 (with-current-buffer (buffer-local-value
                                       'mevedel--view-buffer data-buf)
                   (unless (string-empty-p text)
                     (mevedel-view--ensure-blank-line-before-response)
                     (let ((start (point)))
                       (insert (mevedel-view--fontify-response text) "\n")
                       (let ((response-end (copy-marker (point) t)))
                         (add-text-properties
                          start response-end
                          `(mevedel-view-source ,(cons seg-start seg-end)
                            mevedel-view-source-key ,(mevedel-view--source-collapse-state-key
                                                      (cons seg-start seg-end)
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
                         (set-marker response-end nil)))))))))
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
           (if (mevedel-view--agent-transcript-render-segment-p
                data-buf (cadr seg) (caddr seg))
               (progn
                 (mevedel-view--flush-thinking-group thinking-group data-buf)
                 (setq thinking-group nil)
                 (when tool-group
                   (mevedel-view--render-tool-group
                    (nreverse tool-group) data-buf)
                   (setq tool-group nil))
                 (mevedel-view--render-agent-transcript-segment
                  data-buf seg))
             (if (and tool-group
                      (mevedel-view--hook-audit-only-segment-p
                       data-buf (cadr seg) (caddr seg)))
                 (push seg tool-group)
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

(defun mevedel-view--render-agent-transcript-segment (data-buf seg)
  "Render an agent-transcript render-data SEG from DATA-BUF."
  (let* ((seg-start (cadr seg))
         (seg-end (caddr seg))
         (source (cons seg-start seg-end))
         (render-data
          (with-current-buffer data-buf
            (mevedel-view--agent-transcript-render-data-from-text
             (buffer-substring-no-properties seg-start seg-end)))))
    (when-let* ((rendering
                 (and render-data
                      (mevedel-tool-ui--render-agent
                       (or (plist-get render-data :name) "Agent")
                       (list :subagent_type
                             (or (plist-get render-data :agent-type) "agent")
                             :description
                             (or (plist-get render-data :description) ""))
                       (or (plist-get render-data :body) "")
                       render-data))))
      (mevedel-view--insert-rendered-tool rendering source))))

(defun mevedel-view--same-tool-call-segment-p (left right data-buf)
  "Return non-nil when LEFT and RIGHT belong to the same tool call."
  (and (eq (car left) 'tool)
       (eq (car right) 'tool)
       (with-current-buffer data-buf
         (equal (get-text-property (cadr left) 'gptel)
                (get-text-property (cadr right) 'gptel)))))

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

(defun mevedel-view--render-tool-group (tool-segments data-buf)
  "Render consecutive TOOL-SEGMENTS from DATA-BUF.
Each tool call gets its own collapsible entry.  A registered
`:renderer' is invoked when the segment carries a render-data
side-channel, falling back to the default one-liner otherwise."
  (let ((tool-segments
         (mevedel-view--merge-tool-hook-audit-segments
          tool-segments data-buf))
        (start-time (float-time))
        (inserted-rule nil)
        (rendered 0)
        (fallbacks 0))
    (unwind-protect
        (dolist (seg tool-segments)
          (let* ((seg-start (cadr seg))
                 (seg-end (caddr seg))
                 (source (cons seg-start seg-end))
                 (rendering (mevedel-view--segment-rendering
                             data-buf seg-start seg-end t))
                 (vtype (or (plist-get rendering :vtype) 'tool-summary))
                 (state (and rendering
                             (mevedel-view--source-collapse-state-entry
                              source vtype))))
            (when (and state (not (cdr state)))
              (setq rendering (or (mevedel-view--segment-rendering
                                   data-buf seg-start seg-end)
                                  rendering)))
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
                     mevedel-view-source-key ,(mevedel-view--source-collapse-state-key
                                               source 'tool-summary)))
                  (mevedel-view--decorate-markdown-in-range ins-start (point)))))))
      (mevedel-view--debug-log
       'render-tool-group
       :segments (length tool-segments)
       :rendered rendered
       :fallbacks fallbacks
       :elapsed (- (float-time) start-time)))))

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

(defun mevedel-view--tool-block-bounds (seg-start seg-end)
  "Return org tool-block bounds overlapping SEG-START..SEG-END, or nil.

Restored `GPTEL_BOUNDS' can drift into the `#+begin_tool' line or
past `#+end_tool' when older transcripts are opened.  The org block
markers remain structural anchors, so use them to recover the whole
tool block before parsing the tool plist and render-data side channel."
  (require 'mevedel-transcript)
  (mevedel-transcript--tool-block-bounds-for-run seg-start seg-end))

(defun mevedel-view--tool-segment-text (seg-start seg-end)
  "Return raw tool text for SEG-START..SEG-END.
If the segment overlaps an org tool block, expand to the block bounds
first so stale restored text properties do not hide the `(:name ...)'
form or the render-data block from the parser."
  (let ((raw (buffer-substring-no-properties seg-start seg-end)))
    (if (or (mevedel-view--complete-wrapped-tool-text-p raw)
            (and (not (mevedel-view--tool-wrapped-text-p raw))
                 (mevedel-view--direct-tool-readable-text-p raw)))
        raw
      (pcase-let ((`(,start . ,end)
                   (or (mevedel-view--tool-block-bounds seg-start seg-end)
                       (cons seg-start seg-end))))
        (buffer-substring-no-properties start end)))))


;;
;;; Expand/collapse

(defvar mevedel-view--collapsible-vtypes
  '(thinking-summary tool-summary response
    plan-summary prompt-summary hook-context hook-audit
    system-reminder-summary)
  "View types that `mevedel-view-toggle-section' treats as section folds.
Turn-level folds (`turn-header', `turn-summary') are handled
separately.  Regions with other vtypes are navigable but not
toggleable.")

(defun mevedel-view--truncate-line (text limit)
  "Return TEXT truncated to LIMIT characters with a trailing `...'."
  (if (> (length text) limit)
      (concat (substring text 0 (max 0 (- limit 3))) "...")
    text))

(defun mevedel-view--toggle-fragment-section ()
  "Toggle the migrated fragment-backed section at point.
Return non-nil when point was on a migrated fragment surface handled by
this helper.  Source-backed transcript/tool disclosure remains owned by
`mevedel-view-toggle-section'."
  (let ((namespace (get-text-property (point)
                                      'mevedel-view-zone-namespace))
        (id (get-text-property (point) 'mevedel-view-zone-id)))
    (cond
     ((and (eq namespace 'status) (eq id 'tasks)
           (get-text-property (point) 'mevedel-view-zone-collapsible))
      (mevedel-toggle-tasks)
      t)
     ((and (eq namespace 'status) (eq id 'agents)
           (get-text-property (point) 'mevedel-view-zone-collapsible))
      (mevedel-view-agent-status-toggle)
      t))))

(defun mevedel-view-toggle-section ()
  "Toggle expand/collapse of the section or turn at point.
On a turn header or collapsed-turn summary, toggles the whole turn.
On an inner section summary (thinking, tool, response), toggles that
section only."
  (interactive)
  (let ((collapsed (get-text-property (point) 'mevedel-view-collapsed))
        (source (get-text-property (point) 'mevedel-view-source))
        (vtype (get-text-property (point) 'mevedel-view-type)))
    (cond
     ((mevedel-view--toggle-fragment-section)
      t)
     ((memq vtype '(turn-header turn-summary))
      (mevedel-view--in-flight-turn-start-position)
      (if collapsed
          (mevedel-view--expand-turn)
        (mevedel-view--collapse-turn)))
     ((eq vtype 'mailbox-delivery)
      (mevedel-view--toggle-mailbox-delivery))
     ((and (eq vtype 'agent-handle)
           (get-text-property (point) 'mevedel-view-agent-id)
           (eq (get-text-property (point) 'mevedel-view-agent-status)
               'running))
      (let ((agent-id (get-text-property (point) 'mevedel-view-agent-id)))
        (mevedel-view-agent-handle-activate agent-id)))
     ((eq vtype 'hook-context)
      (mevedel-view--toggle-hook-context))
     ((eq vtype 'hook-audit)
      (mevedel-view--toggle-hook-audit))
     ((and source (memq vtype mevedel-view--collapsible-vtypes))
      (if collapsed
          (mevedel-view--expand-section source vtype)
        (mevedel-view--collapse-section source vtype)))
     ((and (eq vtype 'agent-handle)
           (get-text-property (point) 'mevedel-view-agent-id))
      (let ((agent-id (get-text-property (point) 'mevedel-view-agent-id)))
        (mevedel-view-agent-handle-activate agent-id)))
     ((and source (eq vtype 'agent-handle))
      (if collapsed
          (mevedel-view--expand-section source vtype)
        (mevedel-view--collapse-section source vtype)))
     (t
      (user-error "No collapsible section at point")))))

(defun mevedel-view--mailbox-section-bounds ()
  "Return bounds of the mailbox card at point, or nil."
  (let ((card (get-text-property (point) 'mevedel-view-mailbox-card)))
    (when card
      (let ((start (or (previous-single-property-change
                        (point) 'mevedel-view-mailbox-card)
                       (point-min)))
            (end (or (next-single-property-change
                      (point) 'mevedel-view-mailbox-card)
                     (point-max))))
        (when (and (< start (point))
                   (not (eq (get-text-property
                             start 'mevedel-view-mailbox-card)
                            card)))
          (setq start (or (next-single-property-change
                           start 'mevedel-view-mailbox-card)
                          (point))))
        (cons start end)))))

(defun mevedel-view--mailbox-body-ranges (start end)
  "Return mailbox body ranges between START and END."
  (let ((pos start)
        ranges)
    (while (< pos end)
      (let ((next (or (next-single-property-change
                       pos 'mevedel-view-mailbox-body nil end)
                      end)))
        (if (get-text-property pos 'mevedel-view-mailbox-body)
            (progn
              (push (cons pos next) ranges)
              (setq pos next))
          (setq pos next))))
    (nreverse ranges)))

(defun mevedel-view--mailbox-body-line-count (start end)
  "Return the number of non-empty mailbox body lines from START to END."
  (let ((count 0))
    (save-excursion
      (goto-char start)
      (while (< (point) end)
        (let ((line (buffer-substring-no-properties
                     (point) (min (line-end-position) end))))
          (when (string-match-p "\\S-" line)
            (setq count (1+ count))))
        (forward-line 1)))
    count))

(defun mevedel-view--mailbox-collapse-hint (line-count)
  "Return a mailbox collapse hint for LINE-COUNT body lines."
  (format " [%d %s collapsed]"
          line-count
          (if (= line-count 1) "line" "lines")))

(defun mevedel-view--mailbox-delete-hints (start end)
  "Delete mailbox collapse hints between START and END."
  (let ((end-marker (copy-marker end t)))
    (unwind-protect
        (save-excursion
          (goto-char start)
          (while (< (point) (marker-position end-marker))
            (let ((next (or (next-single-property-change
                             (point) 'mevedel-view-mailbox-hint nil
                             (marker-position end-marker))
                            (marker-position end-marker))))
              (if (get-text-property (point) 'mevedel-view-mailbox-hint)
                  (delete-region (point) next)
                (goto-char next)))))
      (set-marker end-marker nil))))

(defun mevedel-view--toggle-mailbox-delivery ()
  "Toggle a mailbox delivery card without consulting source text."
  (let* ((bounds (mevedel-view--mailbox-section-bounds))
         (collapsed (and bounds
                         (get-text-property
                          (car bounds) 'mevedel-view-collapsed))))
    (unless bounds
      (user-error "No collapsible section at point"))
    (let ((inhibit-read-only t)
          (start (car bounds))
          (end-marker (copy-marker (cdr bounds) t)))
      (unwind-protect
          (save-excursion
            (if collapsed
                (progn
                  (mevedel-view--mailbox-delete-hints
                   start (marker-position end-marker))
                  (remove-text-properties
                   start (marker-position end-marker)
                   '(invisible nil))
                  (put-text-property
                   start (marker-position end-marker)
                   'mevedel-view-collapsed nil))
              (let* ((ranges (mevedel-view--mailbox-body-ranges
                              start (marker-position end-marker)))
                     (line-count
                      (apply #'+
                             (mapcar (lambda (range)
                                       (mevedel-view--mailbox-body-line-count
                                        (car range) (cdr range)))
                                     ranges))))
                (unless ranges
                  (user-error "No collapsible section at point"))
                (mevedel-view--mailbox-delete-hints
                 start (marker-position end-marker))
                (dolist (range (mevedel-view--mailbox-body-ranges
                                start (marker-position end-marker)))
                  (add-text-properties
                   (car range) (cdr range)
                   '(invisible mevedel-view-mailbox-collapsed)))
                (goto-char (caar ranges))
                (when (eq (char-before) ?\n)
                  (backward-char))
                (insert
                 (propertize
                  (mevedel-view--mailbox-collapse-hint line-count)
                  'font-lock-face 'mevedel-view-attribution
                  'mevedel-view-mailbox-hint t
                  'mevedel-view-mailbox-card
                  (get-text-property start 'mevedel-view-mailbox-card)
                  'mevedel-view-type 'mailbox-delivery
                  'mevedel-view-collapsed t
                  'read-only t
                  'keymap mevedel-view--display-map
                  'front-sticky '(read-only keymap)
                  'rear-nonsticky '(read-only keymap)))
                (put-text-property
                 start (marker-position end-marker)
                 'mevedel-view-collapsed t))))
        (set-marker end-marker nil)))))

(defun mevedel-view--section-bounds ()
  "Return (START . END) of the current section at point.
A section is a contiguous region with the same `mevedel-view-source'.
Compared with `eq' to match property-change scanning semantics -- two
conses with equal values but distinct identity are treated as a
boundary, which matters because the turn-level fallback source can
share a value with a nested section without being the same object."
  (let ((source (get-text-property (point) 'mevedel-view-source)))
    (when source
      (let ((start (or (previous-single-property-change
                        (point) 'mevedel-view-source)
                       (point-min)))
            (end (or (next-single-property-change
                      (point) 'mevedel-view-source)
                     (point-max))))
        ;; `previous-single-property-change' returns the latest change
        ;; position before point -- which lands in the PREVIOUS run when
        ;; point is at the start of the current run.  Advance past any
        ;; such leading region whose source is not `eq' to point's.
        (when (and (< start (point))
                   (not (eq (get-text-property start 'mevedel-view-source)
                            source)))
          (setq start (or (next-single-property-change
                           start 'mevedel-view-source)
                          (point))))
        (cons start end)))))

(defun mevedel-view--data-substring (data-buf start end)
  "Return text in DATA-BUF between START and END.
Widens DATA-BUF so narrowing does not hide valid coordinates, then
clamps START and END to the accessible range.  Returns the empty
string when the clamped range is empty, which keeps expand/collapse
from signalling `args-out-of-range' on stale source coordinates."
  (with-current-buffer data-buf
    (save-restriction
      (widen)
      (let* ((pmin (point-min))
             (pmax (point-max))
             (s (max pmin (min start pmax)))
             (e (max pmin (min end pmax))))
        (if (>= s e)
            ""
          (buffer-substring-no-properties s e))))))

(defun mevedel-view--expand-section (source vtype)
  "Expand a collapsed section with SOURCE coordinates and VTYPE."
  (let* ((bounds (mevedel-view--section-bounds))
         (data-buf mevedel--data-buffer)
         (trimmed (and data-buf
                       (buffer-live-p data-buf)
                       (eq vtype 'thinking-summary)
                       (mevedel-view--reasoning-source-bounds
                        data-buf (car source) (cdr source))))
         (source (or trimmed source))
         (data-start (car source))
         (data-end (cdr source))
         (rendering (and data-buf (buffer-live-p data-buf)
                         (mevedel-view--segment-rendering
                          data-buf data-start data-end))))
    (when (and bounds data-buf (buffer-live-p data-buf))
      (let* ((inhibit-read-only t)
             (view-start (car bounds))
             (view-end (cdr bounds))
             ;; Preserve the enclosing turn-id across delete+insert so
             ;; turn-level fold still recognises this section as part of
             ;; the turn.
             (turn-id (get-text-property (car bounds) 'mevedel-view-turn-id))
             (in-flight-after-section-p
              (when-let* ((pos (mevedel-view--in-flight-turn-start-position)))
                (<= view-start pos view-end))))
        (save-excursion
          (goto-char view-start)
          (set-marker-insertion-type mevedel-view--input-marker t)
          (unwind-protect
              (progn
                (delete-region view-start view-end)
                (if rendering
                    ;; Renderer-driven body -- produce expanded form and
                    ;; stamp the same read-only/keymap properties the
                    ;; default path adds so navigation still works.
                    (let ((ins-start (point)))
                      (mevedel-view--render-expanded-body rendering source)
                      (mevedel-view--add-display-region-properties
                       ins-start (point) (plist-get rendering :vtype)))
                  (let ((text (mevedel-view--data-substring
                               data-buf data-start data-end)))
                    ;; Clean org scaffolding from reasoning blocks
                    (when (eq vtype 'thinking-summary)
                      (setq text
                            (mevedel-view--fontify-as
                             (string-trim
                              (mevedel-view--clean-reasoning-text text))
                             'markdown-mode)))
                    ;; Trim response text to match the initial render,
                    ;; then apply org fontification so an expanded response
                    ;; matches the look of the freshly-rendered inline one.
                    (when (eq vtype 'response)
                      (setq text (mevedel-view--fontify-response
                                  (string-trim text))))
                    (when (eq vtype 'prompt-summary)
                      (let ((drawer-body
                             (string-trim
                              (mevedel-view--prompt-drawer-body
                               data-buf data-start data-end))))
                        (setq text
                              (mevedel-view--fontify-as
                               (if (string-empty-p drawer-body)
                                   (string-trim
                                    (mevedel-view--user-turn-text
                                     (list (list 'user data-start data-end))
                                     data-buf))
                                 drawer-body)
                               'markdown-mode))))
                    (when (eq vtype 'hook-context)
                      (setq text
                            (mevedel-view--format-hook-context-block
                             (mevedel-view--hook-context-events-from-text
                              text)
                             t)))
                    (when (eq vtype 'hook-audit)
                      (setq text
                            (mapconcat
                             (lambda (record)
                               (mevedel-view--format-hook-audit-block
                                record t))
                             (mevedel-view--hook-audit-records-from-text
                              text)
                             "")))
                    (when (eq vtype 'system-reminder-summary)
                      (setq text
                            (mevedel-view--fontify-as
                             (or (mevedel-view--system-reminder-body-from-text
                                  text)
                                 text)
                             'markdown-mode)))
                    (when (string-empty-p text)
                      (setq text "[section no longer available]"))
                    (insert text)
                    (unless (eq (char-before) ?\n)
                      (insert "\n"))
                    (add-text-properties view-start (point)
                                         `(mevedel-view-source ,source
                                                               mevedel-view-source-key ,(mevedel-view--source-collapse-state-key
                                                                                         source vtype)
                                                               mevedel-view-type ,vtype
                                                               mevedel-view-collapsed nil))
                    (when (eq vtype 'response)
                      (mevedel-view--decorate-agent-result-blocks
                       view-start (point))
                      (mevedel-view--decorate-agent-message-blocks
                       view-start (point))
                      (mevedel-view--decorate-markdown-in-range
                       view-start (point)))
                    (mevedel-view--add-display-region-properties
                     view-start (point) vtype)))
                (mevedel-view--record-source-collapse-state source vtype nil)
                (when turn-id
                  (put-text-property view-start (point)
                                     'mevedel-view-turn-id turn-id))
                (when in-flight-after-section-p
                  (set-marker mevedel-view--in-flight-turn-start (point))))
            (set-marker-insertion-type mevedel-view--input-marker nil)))))))

(defun mevedel-view--collapse-section (source vtype)
  "Collapse an expanded section back to a one-liner.
SOURCE is the data buffer coordinates, VTYPE the section type.
Only handles the known collapsible vtypes in
`mevedel-view--collapsible-vtypes' -- unknown vtypes are ignored so
that a stray TAB on a non-collapsible region never rewrites a large
span of the buffer with a best-guess preview.

Tool segments with a registered renderer produce the renderer's
`:header' string; everything else falls back to the default summary."
  (let* ((bounds (mevedel-view--section-bounds))
         (data-buf mevedel--data-buffer)
         (trimmed (and data-buf
                       (buffer-live-p data-buf)
                       (eq vtype 'thinking-summary)
                       (mevedel-view--reasoning-source-bounds
                        data-buf (car source) (cdr source))))
         (source (or trimmed source))
         (data-start (car source))
         (data-end (cdr source))
         (rendering (and data-buf (buffer-live-p data-buf)
                         (mevedel-view--segment-rendering
                          data-buf data-start data-end t)))
         (summary
          (cond
           (rendering
            (mevedel-view--rendering-header-line rendering))
           (t
            (pcase vtype
              ('tool-summary
               (mevedel-view--tool-one-liner data-buf data-start data-end))
              ('thinking-summary
               (mevedel-view--thinking-summary data-buf data-start data-end))
              ('response
               (mevedel-view--response-summary data-buf data-start data-end))
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
                data-buf data-start data-end)))))))
    (when (and bounds data-buf (buffer-live-p data-buf) summary)
      (let* ((inhibit-read-only t)
             (view-start (car bounds))
             (view-end (cdr bounds))
             (face (pcase vtype
                     ((or 'tool-summary 'agent-handle 'prompt-summary)
                     'mevedel-view-tool-summary)
                     ('thinking-summary 'mevedel-view-thinking-summary)
                     ('response 'mevedel-view-response-summary)
                     ('hook-context 'mevedel-view-hook-context)
                     ('hook-audit 'mevedel-view-hook-audit)
                     ('system-reminder-summary
                      'mevedel-view-system-reminder)))
             (turn-id (get-text-property (car bounds) 'mevedel-view-turn-id))
             (in-flight-after-section-p
              (when-let* ((pos (mevedel-view--in-flight-turn-start-position)))
                (<= view-start pos view-end))))
        (save-excursion
          (goto-char view-start)
          (set-marker-insertion-type mevedel-view--input-marker t)
          (unwind-protect
              (progn
                (delete-region view-start view-end)
                (let ((ins-start (point)))
                  (mevedel-view--insert-summary-region
                   (mevedel-view--summary-with-face summary face)
                   `(mevedel-view-type ,vtype
                     mevedel-view-collapsed t
                     mevedel-view-source ,source
                     mevedel-view-source-key ,(mevedel-view--source-collapse-state-key
                                               source vtype)))
                  (mevedel-view--add-display-region-properties
                   ins-start (point) vtype)
                  (mevedel-view--record-source-collapse-state source vtype t)
                  (when turn-id
                    (put-text-property ins-start (point)
                                       'mevedel-view-turn-id turn-id))
                  (when in-flight-after-section-p
                    (set-marker mevedel-view--in-flight-turn-start (point)))))
            (set-marker-insertion-type mevedel-view--input-marker nil)))))))

(defun mevedel-view--response-summary (data-buf data-start data-end)
  "Build a one-line summary of a response block in DATA-BUF.
Reads the text between DATA-START and DATA-END, extracts the first
non-empty line, and annotates the line count."
  (let* ((text (mevedel-view--visible-response-text
                (mevedel-view--data-substring data-buf data-start data-end)))
         (trimmed (string-trim text))
         (lines (split-string trimmed "\n"))
         (non-empty (seq-drop-while #'string-empty-p lines))
         (first-line (or (car non-empty) ""))
         (line-count (length lines)))
    (mevedel-view--operation-line
     (string-trim mevedel-view--response-glyph)
     'mevedel-view-response-marker
     (concat (mevedel-view--truncate-line first-line 80)
             (if (> line-count 1) "..." ""))
     nil
     (format "(%d lines)" line-count)
     'mevedel-view-response-summary)))

(defun mevedel-view--prompt-drawer-body (data-buf data-start data-end)
  "Return the prompt-summary body for DATA-START..DATA-END in DATA-BUF.
Prefer the body of a `:PROMPT:' drawer.  When the source is an inline
skill prompt without a drawer, return cleaned user text so saved-session
org metadata such as `GPTEL_BOUNDS' does not leak into the expanded view."
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
         (list (list 'user data-start data-end))
         data-buf)))))


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
                      (mevedel-view--truncate-line first-line 80)
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
                (mevedel-view--truncate-line response-preview 80)
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

(defun mevedel-view--collapse-turn ()
  "Collapse the turn at point into a one-line summary.
Stashes the original propertized text on the summary so expand can
restore the turn with all inner section state intact.  Signals a
`user-error' when the turn is too short to benefit from folding."
  (let* ((bounds (mevedel-view--turn-bounds))
         (role (get-text-property (point) 'mevedel-view-turn-role))
         (id (get-text-property (point) 'mevedel-view-turn-id)))
    (unless (and bounds role id)
      (user-error "No turn at point"))
    (mevedel-view--in-flight-turn-start-position)
    (let* ((turn-start (car bounds))
           (turn-end (cdr bounds))
           (stash (buffer-substring turn-start turn-end))
           (summary (pcase role
                      ('user (mevedel-view--user-turn-summary
                              turn-start turn-end))
                      ('assistant (mevedel-view--assistant-turn-summary
                                   turn-start turn-end))))
           (face (pcase role
                   ('user 'mevedel-view-user-header)
                   ('assistant 'mevedel-view-assistant-header))))
      (unless summary
        (user-error "Turn is already compact"))
      (let ((inhibit-read-only t))
        (save-excursion
          (goto-char turn-start)
          ;; Let the input marker ride forward across our delete+insert
          ;; so it keeps spanning the rendered content afterwards.
          (set-marker-insertion-type mevedel-view--input-marker t)
          (unwind-protect
              (progn
                (delete-region turn-start turn-end)
                (insert (propertize (concat summary "\n\n")
                                    'font-lock-face face
                                    'mevedel-view-type 'turn-summary
                                    'mevedel-view-turn-role role
                                    'mevedel-view-turn-id id
                                    'mevedel-view-collapsed t
                                    'mevedel-view-stash stash
                                    'read-only t
                                    'keymap mevedel-view--display-map
                                    'front-sticky '(read-only keymap)
                                    'rear-nonsticky '(read-only keymap))))
            (set-marker-insertion-type mevedel-view--input-marker nil)))))))

(defun mevedel-view--expand-turn ()
  "Restore a collapsed turn at point from its stashed content."
  (let* ((bounds (mevedel-view--turn-bounds))
         (stash (get-text-property (point) 'mevedel-view-stash)))
    (unless (and bounds stash)
      (user-error "No collapsed turn at point"))
    (mevedel-view--in-flight-turn-start-position)
    (let ((inhibit-read-only t))
      (save-excursion
        (goto-char (car bounds))
        (set-marker-insertion-type mevedel-view--input-marker t)
        (unwind-protect
            (progn
              (delete-region (car bounds) (cdr bounds))
              (insert stash))
          (set-marker-insertion-type mevedel-view--input-marker nil))))))


;;
;;; Navigation

(defun mevedel-view--display-navigation-limit ()
  "Return the upper bound for display/chrome navigation."
  (or (mevedel-view--input-marker-position) (point-max)))

(defun mevedel-view--next-fragment-position (limit)
  "Return the next navigatable fragment position before LIMIT."
  (require 'mevedel-view-zone)
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
  (require 'mevedel-view-zone)
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
  (if mevedel--data-buffer
      (switch-to-buffer mevedel--data-buffer)
    (user-error "No data buffer associated with this view")))


;;
;;; Full re-render
(defun mevedel-view--live-tail-rendered-position (live-tail limit)
  "Return the position where LIVE-TAIL already appears before LIMIT.

Full rerenders may preserve an in-flight view tail when the data buffer
has not yet received a replacement assistant turn.  Some refresh paths
can lose the data-turn anchor while the same assistant text is already
renderable from the data buffer; in that case appending the preserved
tail would duplicate the visible transcript."
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
          (when (cdr prefix-lines)
            (mevedel-view--live-tail-lines-rendered-position
             prefix-lines limit))))))

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

(defun mevedel-view--live-tail-lines-rendered-position (lines limit)
  "Return position where LINES appear before LIMIT, allowing blank gaps."
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
                               (point) end))))))
      (let ((first (car lines))
            (rest (cdr lines)))
        (save-excursion
          (goto-char (point-min))
          (catch 'found
            (while (search-forward first limit t)
              (let ((start (match-beginning 0)))
                (save-excursion
                  (catch 'mismatch
                    (dolist (line rest)
                      (unless (and (skip-gap)
                                   (looking-at-line-p line))
                        (throw 'mismatch nil))
                      (goto-char (+ (point) (length line))))
                    (throw 'found start)))))))))))

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
                  (mevedel-view--add-display-region-properties
                   audit-start (point) 'hook-audit))))
          (set-marker-insertion-type mevedel-view--input-marker nil))))))

(defun mevedel-view--summary-hook-audits (data-buf start end)
  "Return hook audit records stored in DATA-BUF summary START..END."
  (when (and data-buf (buffer-live-p data-buf) (< start end))
    (with-current-buffer data-buf
      (mevedel-view--hook-audit-records-from-text
       (buffer-substring-no-properties start end)))))

(defun mevedel-view--full-rerender ()
  "Re-render the entire view buffer from the data buffer.
Wipe all rendered content and re-render from scratch.  Used after
compaction, session resume, or manual refresh.

Re-anchors `mevedel-view--in-flight-turn-start' to the rerendered
position of the last (in-flight) turn when a turn was in flight at
the time of the rerender; otherwise the wipe collapses the marker to
`point-min' and the next incremental render erases the freshly
rerendered history (and its `You' header along with it).

Preserves live window state so
the caret + scroll position survive a rerender triggered
mid-stream (e.g. by the post-permission accept callback's view
rerender)."
  (require 'mevedel-transcript)
  (unless mevedel--data-buffer
    (error "No data buffer"))
  (mevedel-view-render--preserving-window-state
   (mevedel-view--call-preserving-input-text
    (lambda ()
      (let ((start-time (float-time))
            (data-buf mevedel--data-buffer)
            (render-view-buf (current-buffer))
            (render-agent-transcript-p mevedel-view--agent-transcript-p)
            (inhibit-read-only t)
            (inhibit-modification-hooks t)
            (saved-states
             (and (markerp mevedel-view--input-marker)
                  (marker-position mevedel-view--input-marker)
                  (mevedel-view--capture-collapse-states
                   (point-min)
                   (marker-position mevedel-view--input-marker))))
            (data-turn-start-pos
             (and (markerp mevedel-view--data-turn-start)
                  (marker-position mevedel-view--data-turn-start)))
            (in-flight-was (mevedel-view--in-flight-turn-start-position))
            (preserved-live-tail
             (when-let* (((not mevedel-view--agent-transcript-p))
                         (tail-start
                          (mevedel-view--in-flight-turn-start-position))
                         ((markerp mevedel-view--status-marker))
                         (tail-end (marker-position mevedel-view--status-marker))
                         ((< tail-start tail-end)))
               (mevedel-view--strip-history-live-fragments-from-string
                (buffer-substring tail-start tail-end)))))
        (unless mevedel-view--pending-tool-calls
          (mevedel-view--delete-pending-tool-live-lines))
        (mevedel-view--debug-log
         'full-rerender-begin
         :in-flight-was in-flight-was
         :preserved-live-tail-len (and preserved-live-tail
                                       (length preserved-live-tail))
         :state (mevedel-view--debug-state data-buf))
        ;; Full rerender is also the recovery path for compaction, resume,
        ;; and explicit refresh.  Capture/apply below preserves matching
        ;; live sections; the table itself must not keep stale source keys
        ;; from a data-buffer rewrite or clear.
        (setq mevedel-view--source-collapse-states
              (make-hash-table :test #'equal))
        (if mevedel-view--agent-transcript-p
            (progn
              (mevedel-view--debug-log
               'full-rerender-delete-transcript
               :region (mevedel-view--debug-region (point-min) (point-max))
               :state (mevedel-view--debug-state data-buf))
              (delete-region (point-min) (point-max))
              (goto-char (point-min))
              (set-marker mevedel-view--input-marker (point))
              (when (markerp mevedel-view--status-marker)
                (set-marker mevedel-view--status-marker (point)))
              (when (markerp mevedel-view--interaction-marker)
                (set-marker mevedel-view--interaction-marker (point))))
          ;; Wipe history/status/interaction/request-progress rows above the input marker.
          (mevedel-view--debug-log
           'full-rerender-delete-display
           :region (mevedel-view--debug-region
                    (point-min)
                    (marker-position mevedel-view--input-marker))
           :state (mevedel-view--debug-state data-buf))
          (delete-region (point-min) mevedel-view--input-marker)
          (mevedel-view--forget-request-progress-region)
          ;; Re-insert header and reset all three zone markers to the
          ;; end of the header so the zone ordering invariant holds after
          ;; full rerender (compaction, resume, manual refresh).
          (goto-char (point-min))
          (insert (mevedel-view--header-string data-buf))
          (set-marker mevedel-view--input-marker (point))
          (when (markerp mevedel-view--status-marker)
            (set-marker mevedel-view--status-marker (point)))
          (when (markerp mevedel-view--interaction-marker)
            (set-marker mevedel-view--interaction-marker (point))))
        (mevedel-view--debug-log
         'full-rerender-after-header
         :state (mevedel-view--debug-state data-buf))
    ;; Render all content from data buffer
    (with-current-buffer data-buf
      (unless (mevedel-view--running-agent-transcript-buffer-p)
        (require 'mevedel-transcript-restore)
        (mevedel-transcript-restore-properties t))
      ;; Skip compacted region at the start.  In-place compaction leaves
      ;; ignored/shadowed old content followed by a
      ;; summary block; segment rotation starts directly with a summary
      ;; block followed by live tail content.
      (let ((scan-start (mevedel-transcript--skip-leading-properties-drawer
                         (point-min)))
            (view-buf (if render-agent-transcript-p
                          render-view-buf
                        (buffer-local-value 'mevedel--view-buffer
                                            data-buf)))
            (compaction-indicator-inserted nil))
        (when (eq (get-text-property scan-start 'face) 'shadow)
          ;; Skip past shadow region (old conversation)
          (setq scan-start (or (next-single-property-change
                                scan-start 'face nil (point-max))
                               (point-max)))
          ;; Skip past the compaction separator + summary block by
          ;; searching for the end marker.
          (let ((summary-start scan-start))
            (save-excursion
              (goto-char scan-start)
              (when (re-search-forward
                     "^#\\+end_summary\n\\|^```\n" nil t)
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
               (cons scan-start after-summary))
              (setq compaction-indicator-inserted t)))
          (setq scan-start after-summary))
        ;; Narrow so that transcript segment boundary expansion
        ;; (`previous-single-property-change' bounded by `point-min')
        ;; can't walk back into the leading drawer / compacted region.
        (save-restriction
          (narrow-to-region scan-start (point-max))
          (let* ((segments (mevedel-transcript-segments
                            (point-min) (point-max)))
                 (turns (mevedel-view--group-into-turns segments data-buf))
                 (view-buf (if render-agent-transcript-p
                               render-view-buf
                             (buffer-local-value 'mevedel--view-buffer
                                                 data-buf)))
                 (last-assistant-turn-start nil)
                 (last-current-assistant-turn-start nil)
                 (last-turn-role nil))
            (with-current-buffer view-buf
              (dolist (turn turns)
                (setq last-turn-role (plist-get turn :role))
                (when (eq (plist-get turn :role) 'assistant)
                  (let ((view-turn-start
                         (copy-marker mevedel-view--input-marker nil)))
                    (setq last-assistant-turn-start view-turn-start)
                    (when (and data-turn-start-pos
                               (plist-get turn :end)
                               (> (plist-get turn :end)
                                  data-turn-start-pos))
                      (setq last-current-assistant-turn-start
                            view-turn-start))))
                (mevedel-view--render-turn turn data-buf))
              (when saved-states
                (mevedel-view--apply-collapse-states
                 (point-min)
                 (marker-position mevedel-view--input-marker)
                 saved-states)))
            ;; Re-anchor `in-flight-turn-start' for the in-flight assistant
            ;; turn.  Prefer an assistant turn overlapping the current
            ;; data-turn range; mailbox/user turns can arrive after that
            ;; assistant while the request is still in flight, and the
            ;; next incremental render still needs to replace from the
            ;; assistant start.  If a new request has rendered its user
            ;; turn but no assistant replacement yet, the last assistant
            ;; belongs to the previous exchange and must not become the
            ;; new wipe start.  Without a correct in-flight anchor, the
            ;; wipe above collapsed the marker to point-min and the next
            ;; incremental render would erase the rerendered history.
            (when in-flight-was
              (with-current-buffer view-buf
                (cond
                 (last-current-assistant-turn-start
                  (mevedel-view--debug-log
                   'full-rerender-reanchor
                   :decision 'current-assistant
                   :last-turn-role last-turn-role
                   :last-assistant-turn-start last-assistant-turn-start
                   :last-current-assistant-turn-start
                   last-current-assistant-turn-start
                   :data-turn-start data-turn-start-pos
                   :state (mevedel-view--debug-state data-buf))
                  (mevedel-view--set-in-flight-turn-start
                   last-current-assistant-turn-start))
                 ((and (not data-turn-start-pos)
                       (eq last-turn-role 'assistant)
                       last-assistant-turn-start)
                  (mevedel-view--debug-log
                   'full-rerender-reanchor
                   :decision 'last-assistant
                   :last-turn-role last-turn-role
                   :last-assistant-turn-start last-assistant-turn-start
                   :state (mevedel-view--debug-state data-buf))
                  (mevedel-view--set-in-flight-turn-start
                   last-assistant-turn-start))
                 ((and preserved-live-tail
                       (mevedel-view--live-tail-rendered-position
                        preserved-live-tail mevedel-view--input-marker))
                  (let ((tail-start
                         (mevedel-view--live-tail-rendered-position
                          preserved-live-tail mevedel-view--input-marker)))
                    (mevedel-view--debug-log
                     'full-rerender-reanchor
                     :decision 'existing-live-tail
                     :last-turn-role last-turn-role
                     :tail-start tail-start
                     :state (mevedel-view--debug-state data-buf))
                    (mevedel-view--set-in-flight-turn-start tail-start)))
                 (preserved-live-tail
                  (goto-char mevedel-view--input-marker)
                  (mevedel-view-render--with-boundaries-advancing
                    (let ((tail-start (point)))
                      (insert preserved-live-tail)
                      (mevedel-view--debug-log
                       'full-rerender-reanchor
                       :decision 'preserved-live-tail
                       :last-turn-role last-turn-role
                       :tail-start tail-start
                       :state (mevedel-view--debug-state data-buf))
                      (mevedel-view--set-in-flight-turn-start tail-start))))
                 (t
                  (mevedel-view--debug-log
                   'full-rerender-reanchor
                   :decision 'input-marker
                   :last-turn-role last-turn-role
                   :state (mevedel-view--debug-state data-buf))
                  (mevedel-view--set-in-flight-turn-start
                   mevedel-view--input-marker)))))
            (with-current-buffer view-buf
              (unless mevedel-view--agent-transcript-p
                (mevedel-view-refresh-input-prompt)
                (when mevedel-view--pending-tool-calls
                  (mevedel-view--refresh-pending-tool-lines))
                (mevedel-view--render-status data-buf)
                (mevedel-view--interaction-rebuild)
                (mevedel-view--ensure-request-progress data-buf))
              (mevedel-view--debug-log
               'full-rerender-after-render
               :last-assistant-turn-start last-assistant-turn-start
               :last-current-assistant-turn-start
               last-current-assistant-turn-start
               :elapsed (- (float-time) start-time)
               :state (mevedel-view--debug-state data-buf))))))))))))


;;
;;; Optimistic user turn rendering

(defun mevedel-view--insert-user-message
    (text &optional kind hook-context prompt-summary-body
          prompt-summary-source hook-audits)
  "Render TEXT as a user message in the history region.
Inserts at the history boundary with read-only protection.
KIND may be `directive' to fontify directive-specific display text.
HOOK-CONTEXT is model-visible hook context to summarize in the view.
PROMPT-SUMMARY-BODY, when non-nil, is shown as a collapsed Prompt
section backed by PROMPT-SUMMARY-SOURCE when available.  HOOK-AUDITS
is a list of hook audit records to render under the user turn.

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
            user-end)
        (insert (propertize "You\n" 'font-lock-face 'mevedel-view-user-header))
        (insert (if (eq kind 'directive)
                    (mevedel-view--fontify-directive-display-text text)
                  text))
        (unless (eq (char-before) ?\n)
          (insert "\n"))
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
        (setq mevedel-view--user-pre-rendered t)
        (copy-marker (point) nil)))))


(provide 'mevedel-view-render)
;;; mevedel-view-render.el ends here
