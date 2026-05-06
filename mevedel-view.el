;;; mevedel-view.el -- Compact view buffer for chat sessions -*- lexical-binding: t -*-

;;; Commentary:

;; Provides a user-facing view buffer that renders a compact display of the
;; gptel data buffer. The data buffer (org-mode) is the authoritative
;; conversation where gptel operates. The view buffer (`mevedel-view-mode')
;; shows collapsed tool results and an editable input region at the bottom.
;;
;; Architecture:
;;   data buffer (org-mode, gptel) <--- authoritative
;;     |
;;     +---> view buffer (mevedel-view-mode) <--- user-facing
;;
;; The view buffer is ephemeral and always reconstructable from the
;; data buffer.

;;; Code:

(eval-when-compile (require 'cl-lib))

;; `gptel'
(declare-function gptel-send "ext:gptel" (&optional arg))
(declare-function gptel--restore-props "ext:gptel" (bounds-alist))
(defvar gptel-prompt-prefix-alist)
(defvar gptel-response-separator)

;; `mevedel-structs'
(defvar mevedel--data-buffer)
(defvar mevedel--view-buffer)
(defvar mevedel--session)
(defvar mevedel--current-request)
(defvar mevedel--current-directive-uuid)
(defvar mevedel--compaction-in-flight nil)
(declare-function mevedel-request-begin "mevedel-structs"
                  (session &optional directive-uuid))
(declare-function mevedel-request-end "mevedel-structs" ())
(declare-function mevedel-session-skills "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-name "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-session-id "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-workspace "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-agent-transcripts "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-save-path "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-permission-queue "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-plan-queue "mevedel-structs" (cl-x) t)
(declare-function mevedel-workspace-name "mevedel-structs" (cl-x) t)
(declare-function mevedel-permission-queue-abort-all
                  "mevedel-permission-queue" (&optional session))
(declare-function mevedel-permission-queue--render-head
                  "mevedel-permission-queue" (&optional session))
(declare-function mevedel-plan-queue-abort-all
                  "mevedel-tool-plan" (&optional session))
(declare-function mevedel-plan-queue--render-head
                  "mevedel-tool-plan" (&optional session))
(declare-function mevedel-workspace-root "mevedel-structs" (cl-x) t)

;; `mevedel-preview-mode'
(defvar mevedel-preview-mode--pending)

;; `mevedel-agents'
(declare-function mevedel-agent-name "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-agent "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-agent-id "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-description "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-call-count "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-started-at "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-transcript-status "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-terminal-reason "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-activity "mevedel-agents" (cl-x) t)

;; `org'
(declare-function org-entry-get "ext:org" (pom property &optional inherit literal-nil))
(declare-function org-fontify-like-in-org-mode "ext:org" (s &optional odd-levels))
(declare-function org-mode "ext:org" ())
(declare-function org-unescape-code-in-string "ext:org-src" (s))
(defvar org-inhibit-startup)
(defvar org-mode-hook)

;; `mevedel-tool-registry'
(declare-function mevedel-tool-display-string "mevedel-tool-registry" (tool-name args))
(declare-function mevedel-tool-get "mevedel-tool-registry" (name &optional category))
(declare-function mevedel-tool-name "mevedel-tool-registry" (cl-x) t)
(declare-function mevedel-tool-renderer "mevedel-tool-registry" (cl-x) t)

;; `mevedel-tool-ui'
(declare-function mevedel-permission--prompt-approve-always
                  "mevedel-tool-ui" ())
(declare-function mevedel-permission--prompt-approve-once
                  "mevedel-tool-ui" ())
(declare-function mevedel-permission--prompt-approve-session
                  "mevedel-tool-ui" ())
(declare-function mevedel-permission--prompt-deny-once
                  "mevedel-tool-ui" ())
(declare-function mevedel-permission--prompt-deny-session
                  "mevedel-tool-ui" ())
(declare-function mevedel-permission--prompt-feedback
                  "mevedel-tool-ui" ())
(declare-function mevedel-tool-ui--display-label-from-canonical
                  "mevedel-tool-ui" (agent-id))
(declare-function mevedel-tool-ui--handle-badge "mevedel-tool-ui" (render-data))
(declare-function mevedel-tool-ui--render-agent
                  "mevedel-tool-ui" (name args result render-data))

;; `mevedel-tools'
(declare-function mevedel-tools--agent-invocation-at "mevedel-tools" (fsm))
(defvar mevedel-tools--agents-fsm)

;; `mevedel-pipeline'
(declare-function mevedel-pipeline-extract-render-data "mevedel-pipeline" (result-string))

;; `mevedel-skills'
(declare-function mevedel-skills--parse-slash-line "mevedel-skills" (text))
(declare-function mevedel-skills--slash-annotation
                  "mevedel-skills" (name buffer session local-commands))
(declare-function mevedel-skills--slash-completion-table
                  "mevedel-skills" (buffer session local-commands))
(declare-function mevedel-skills--insert-fork-result "mevedel-skills" (outcome))
(declare-function mevedel-skills-invoke "mevedel-skills" t t)
(declare-function mevedel-session-get-skill "mevedel-skills" (session name))
(declare-function mevedel-skill-name "mevedel-skills" (cl-x) t)
(declare-function mevedel-skill-context "mevedel-skills" (cl-x) t)
(defvar mevedel-slash-commands)

;; `mevedel-mentions'
(declare-function mevedel-mentions-install "mevedel-mentions" ())

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


;;
;;; Customization

(defcustom mevedel-view-fontify-responses t
  "Non-nil means fontify response bodies using `org-mode' syntax.
Each assistant response is run through `org-fontify-like-in-org-mode'
so org markers (headings, bold, verbatim, code blocks, links) render
with faces.  The view buffer itself stays in `mevedel-view-mode' -- no
org commands or keymaps are installed."
  :type 'boolean
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
  '((t :inherit shadow))
  "Face for collapsed tool call summaries."
  :group 'mevedel)

(defface mevedel-view-thinking-summary
  '((t :inherit (shadow italic)))
  "Face for collapsed thinking/reasoning summaries."
  :group 'mevedel)

(defface mevedel-view-response-summary
  '((t :inherit shadow))
  "Face for collapsed response summaries."
  :group 'mevedel)

(defface mevedel-view-spinner
  '((t :inherit (bold font-lock-comment-face)))
  "Face for the spinner status line."
  :group 'mevedel)

(defface mevedel-view-turn-rule
  '((t :inherit shadow :overline t :extend t))
  "Face for the horizontal rule that closes an assistant turn."
  :group 'mevedel)

(defface mevedel-view-input-prompt
  '((t :inherit shadow :weight bold))
  "Face for the read-only `> ' prompt in the input region."
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

(defcustom mevedel-view-render-debug nil
  "Non-nil means trace view-buffer render decisions.
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

(defcustom mevedel-view-agent-activity-max 5
  "Maximum number of live activity events shown for a running agent.
Activity is ephemeral and newest-last.  Values below zero behave like
zero; when zero, running agent handles do not expand into an activity
body."
  :type 'integer
  :group 'mevedel)

(defcustom mevedel-view-mailbox-collapse-line-threshold 5
  "Mailbox delivery bodies longer than this many lines start collapsed.
Shorter bodies render fully expanded."
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
;;; Glyphs and input prompt

(defconst mevedel-view--tool-glyph "› "
  "Prefix shown in front of tool-call summary lines.")

(defconst mevedel-view--thinking-glyph "… "
  "Prefix shown in front of thinking/reasoning summary lines.")

(defconst mevedel-view--response-glyph "▸ "
  "Prefix shown in front of collapsed response summary lines.")

(defconst mevedel-view--input-prompt "> "
  "Read-only prefix rendered at the start of the input region.")


;;
;;; Buffer-locals

(defvar-local mevedel-view--input-marker nil
  "Marker separating the display region (above) from the input region (below).
Everything above this marker is read-only rendered content; everything
at or below is the user's editable input area.")

(defvar-local mevedel-view--status-marker nil
  "Marker delimiting the bottom of zone 1 (history) and top of zone 2 (status).
Insertion-type `t' so history-content insertion advances it; status-zone
content renders here as read-only text.")

(defvar-local mevedel-view--interaction-marker nil
  "Marker delimiting the bottom of zone 2 (status) and top of zone 3 (interaction).
Insertion-type `t' so status content above advances it; interaction-zone
overlays anchor here.  Permission queue head, plan confirmation, and
preview overlays render against this marker.")

(defvar-local mevedel-view--render-insertion-marker nil
  "Temporary marker used by render helpers as their insertion point.
Nil means render at `mevedel-view--input-marker'.  Incremental
history rebuilds bind this to `mevedel-view--status-marker' so
the in-flight assistant turn is inserted above status and
interaction zones instead of inside them.")

(defvar-local mevedel-view--interaction-descriptors nil
  "Hash table of live interaction-zone descriptors keyed by descriptor id.")

(defvar-local mevedel-view--interaction-overlays nil
  "Hash table of live interaction-zone overlays keyed by descriptor id.")

(defvar-local mevedel-view--interaction-separator-overlay nil
  "Zero-width overlay that renders the composite interaction-zone separator.")

(defvar-local mevedel-view--interaction-materialized-overlay nil
  "Overlay covering descriptor-rendered real text in the interaction zone.")

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

(defvar-local mevedel-view--agent-activity-expanded nil
  "Hash table keyed by agent id storing running-handle expansion state.")

(defvar-local mevedel-view--agent-status-overlay nil
  "Overlay covering materialized aggregate live agent status text.")

(defvar-local mevedel-view--agent-status-expanded-p nil
  "Non-nil means the aggregate live agent status line shows rows.")

(defvar-local mevedel-view--user-pre-rendered nil
  "Non-nil when the most recent user turn was pre-rendered by the view.

Set by `mevedel-view--insert-user-message' when the view's send path
echoes the user's input immediately, and consumed (cleared) by
`mevedel-view--render-response' to skip the user turn that
`mevedel-view--extract-segments' may pick up for the same exchange,
which would otherwise produce a duplicate \"You\" block above the
assistant reply.  Tests that drive `--render-response' directly
(without going through the send path) leave the flag nil and see user
turns rendered as usual.")

(defvar-local mevedel-view--spinner-overlay nil
  "Overlay showing status during an active request, or nil when idle.")

(defvar-local mevedel-view--spinner-status nil
  "Current status text shown by `mevedel-view--spinner-overlay'.")

(defvar-local mevedel-view--spinner-timer nil
  "Buffer-local timer animating visible spinner frames.")

(defvar-local mevedel-view--spinner-frame-index 0
  "Current frame index for animated view-buffer spinners.")

(defvar-local mevedel-view--in-flight-turn-start nil
  "View-buffer marker at which the current assistant turn's render begins.

Set by the send path right after the user turn is echoed, consumed by
`mevedel-view--render-incremental' to bound the delete-and-re-render
region for each progress update, and cleared when the final
`gptel-post-response-functions' render completes.  Nil outside an
active exchange.")

(defvar-local mevedel-view--data-turn-start nil
  "Data-buffer marker at which the current assistant turn starts.

Anchored just after the user prompt was forwarded to the data
buffer, so `mevedel-view--render-incremental' can extract only the
in-flight assistant portion (not the whole conversation) when
rebuilding the view.  Nil outside an active exchange.")

(defvar-local mevedel-view--pending-tool-calls nil
  "Alist of in-flight tool calls.
Each entry is `(KEY . TOOL-NAME)' where KEY identifies the dispatch
and TOOL-NAME is the displayed tool name.

KEY is the backend call id when gptel exposes one, falling back to a
fingerprint built from `(NAME . ARGS-PRINT)' on older gptel builds.

Pre-tool hook adds an entry; post-tool hook removes by KEY.  The
render path walks this alist and emits one `Calling X…' line per
entry in arrival order, respecting
`mevedel-view-pending-tools-visible-max' for truncation when many
tools are in flight in parallel.")

(defvar-local mevedel-view--stream-render-timer nil
  "Idle timer scheduling a `gptel-post-stream-hook'-driven render.

`mevedel-view--schedule-stream-render' sets this on each stream chunk
to batch the burst of per-chunk hook fires into one incremental render
after a short quiescence window.  Tool-boundary hooks cancel the
pending timer and render immediately, so tool events are never
delayed by a queued stream tick.")

(defcustom mevedel-view-stream-render-delay 0.4
  "Seconds to wait after the last stream chunk before re-rendering.

The `gptel-post-stream-hook' path fires once per streamed chunk (up to
dozens per second).  `mevedel-view--schedule-stream-render' debounces
those fires by waiting this long for no new chunks before calling
`mevedel-view--render-incremental'.  Tune higher if the render cost
is visible in your environment; lower for snappier updates.

Tool-boundary hooks bypass the debounce entirely."
  :type 'number
  :group 'mevedel)

(defcustom mevedel-view-spinner-animate t
  "Non-nil means animate view-buffer spinner glyphs."
  :type 'boolean
  :group 'mevedel)

(defcustom mevedel-view-spinner-interval 0.12
  "Seconds between view-buffer spinner frame updates."
  :type 'number
  :group 'mevedel)

(defconst mevedel-view-spinner-braille-frames
  '("⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏")
  "Braille Pattern frames for animated view-buffer spinners.")

(defconst mevedel-view-spinner-ascii-frames
  '("-" "\\" "|" "/")
  "ASCII fallback frames for animated view-buffer spinners.")

(defcustom mevedel-view-spinner-frames
  mevedel-view-spinner-braille-frames
  "Frames used for animated view-buffer spinners.
The default frames are Braille Pattern Unicode code points U+280B,
U+2819, U+2839, U+2838, U+283C, U+2834, U+2826, U+2827, U+2807,
and U+280F.  If your font does not render these glyphs, set this to
`mevedel-view-spinner-ascii-frames'."
  :type '(repeat string)
  :group 'mevedel)


;;
;;; Render trace instrumentation

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
  "Return a plist describing the current spinner overlay."
  (let ((ov mevedel-view--spinner-overlay))
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
  "Return compact debug metadata for TURNS from DATA-BUF."
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
;;; Major mode

(defvar-keymap mevedel-view--display-map
  :doc "Keymap active in the read-only display region of the view buffer.
Applied via the `keymap' text property so these bindings only fire
above `mevedel-view--input-marker'."
  "TAB" #'mevedel-view-toggle-section
  "RET" #'mevedel-view-open-agent-transcript-at-point
  "<mouse-1>" #'mevedel-view-open-agent-transcript-at-point
  "<mouse-2>" #'mevedel-view-open-agent-transcript-at-point
  "n" #'mevedel-view-next-turn
  "p" #'mevedel-view-prev-turn
  "t" #'mevedel-view-toggle-transcript
  "q" #'quit-window)

(defvar-keymap mevedel-view--agent-handle-map
  :doc "Keymap active on non-attribution text in Agent handles.
The attribution id carries its own transcript-opening keymap; the
rest of the handle should remain navigable and foldable without
opening the transcript on click."
  "TAB" #'mevedel-view-toggle-section
  "n" #'mevedel-view-next-turn
  "p" #'mevedel-view-prev-turn
  "t" #'mevedel-view-toggle-transcript
  "q" #'quit-window)

(defvar-keymap mevedel-view-mode-map
  :doc "Keymap for `mevedel-view-mode'."
  "C-c RET" #'mevedel-view-send
  "C-c C-k" #'mevedel-view-abort
  "C-c C-l" #'mevedel-view-history-browse
  "C-c C-u" #'mevedel-view-history-clear-input
  "M-p" #'mevedel-view-history-previous
  "M-n" #'mevedel-view-history-next
  "M-r" #'mevedel-view-history-search
  "a" #'mevedel-permission--prompt-approve-once
  "s" #'mevedel-permission--prompt-approve-session
  "A" #'mevedel-permission--prompt-approve-always
  "d" #'mevedel-permission--prompt-deny-once
  "D" #'mevedel-permission--prompt-deny-session
  "f" #'mevedel-permission--prompt-feedback)

(define-key mevedel-view-mode-map
            [remap move-beginning-of-line]
            #'mevedel-view-history-beginning-of-line)

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

Displays a compact rendering of the gptel data buffer. The buffer is
divided into two regions by `mevedel-view--input-marker':

  - Above the marker: read-only rendered conversation (protected by
    the `read-only' text property and the `mevedel-view--display-map'
    keymap property).
  - Below the marker: editable input area where the user types
    messages.

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
;;; Header and fontification helpers

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

(defun mevedel-view--response-display-text (text)
  "Return response TEXT normalized for the rendered view.

The data buffer is org-mode, and gptel converts markdown fences in
assistant responses into org source blocks.  Keep that representation
in the authoritative transcript, but show markdown-style fences in the
view so conversion scaffolding does not leak into the chat display."
  (let ((case-fold-search t))
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (while (re-search-forward
              "^#\\+begin_src\\(?:[ \t]+\\([^ \t\n]+\\).*\\)?[ \t]*$"
              nil t)
        (replace-match (concat "```" (or (match-string 1) "")) t t))
      (goto-char (point-min))
      (while (re-search-forward "^#\\+end_src[ \t]*$" nil t)
        (replace-match "```" t t))
      (buffer-string))))

(defun mevedel-view--fontify-response (text)
  "Return TEXT with view-safe response markup and face properties.
Returns normalized TEXT without faces when
`mevedel-view-fontify-responses' is nil or `org' cannot be loaded.
Binds `org-inhibit-startup' and clears `org-mode-hook' so the temp
buffer used by `org-fontify-like-in-org-mode' is as lightweight as possible.
Faces are stored as `font-lock-face' so they survive the view
buffer's font-lock refontification cycles."
  (let ((text (mevedel-view--response-display-text text)))
    (if (and mevedel-view-fontify-responses
             (require 'org nil t))
        (let ((org-inhibit-startup t)
              (org-mode-hook nil))
          (mevedel-view--promote-face-to-font-lock-face
           (org-fontify-like-in-org-mode text)))
      text)))


;;
;;; Setup

(defun mevedel-view--setup (view-buf data-buf &optional options)
  "Initialize VIEW-BUF as the view buffer for DATA-BUF.
Activates `mevedel-view-mode', wires the cross-references, and
inserts the initial separator with input marker.

OPTIONS is a plist.  When `:agent-transcript-p' is non-nil, create
a read-only transcript inspection view instead of an interactive chat
view."
  (require 'mevedel-view-history)
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
    (setq-local mevedel-view--agent-activity-expanded (make-hash-table :test #'equal))
    (setq-local mevedel-view--agent-status-expanded-p nil)
    (setq-local mevedel-view--interaction-descriptors
                (make-hash-table :test #'equal))
    (setq-local mevedel-view--interaction-overlays
                (make-hash-table :test #'equal))
    (setq-local mevedel-view--interaction-separator-overlay nil)
    (setq-local mevedel-view--interaction-materialized-overlay nil)
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
          (insert mevedel-view--input-prompt)
          (add-text-properties
           start (point)
           `(read-only t
             font-lock-face mevedel-view-input-prompt
             mevedel-view-prompt t
             front-sticky (read-only mevedel-view-prompt)
             rear-nonsticky (read-only mevedel-view-prompt font-lock-face)))
          (setq mevedel-view--status-marker (copy-marker start t))
          (setq mevedel-view--interaction-marker (copy-marker start t))
          (setq mevedel-view--input-marker (copy-marker start nil)))))
    (unless mevedel-view--agent-transcript-p
      (mevedel-view-history-load mevedel--session)
      ;; Install slash-command completion
      (add-hook 'completion-at-point-functions
                #'mevedel-view-slash-capf nil t)
      ;; Install @ref/@file font-lock and completion
      (mevedel-mentions-install))
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
            '(:eval (mevedel-view--proxy-header-line)))))
  ;; Wire the reverse reference on the data buffer
  (with-current-buffer data-buf
    (setq-local mevedel--view-buffer view-buf)
    ;; Kill-buffer lifecycle: data killed -> kill view buffer
    (add-hook 'kill-buffer-hook #'mevedel-view--on-data-killed nil t)))

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

(defun mevedel-view--on-view-killed ()
  "Hook run when the view buffer is killed.
Clears `mevedel--view-buffer' on the associated data buffer and kills
it.  The reference is cleared before killing so the data buffer's own
kill hook sees nil and exits without re-entering this function."
  (when (bound-and-true-p mevedel-view--agent-transcript-p)
    (mevedel-view--clear-parent-transcript-window))
  (mevedel-view--interaction-clear)
  (when-let* ((db mevedel--data-buffer)
              (_ (buffer-live-p db)))
    (with-current-buffer db
      (when (fboundp 'mevedel-permission-queue-abort-all)
        (mevedel-permission-queue-abort-all mevedel--session))
      (when (fboundp 'mevedel-plan-queue-abort-all)
        (mevedel-plan-queue-abort-all mevedel--session))
      (setq mevedel--view-buffer nil))
    (kill-buffer db)))

(defun mevedel-view--on-data-killed ()
  "Hook run when the data buffer is killed.
Kills the associated view buffer."
  (when (fboundp 'mevedel-permission-queue-abort-all)
    (mevedel-permission-queue-abort-all mevedel--session))
  (when (fboundp 'mevedel-plan-queue-abort-all)
    (mevedel-plan-queue-abort-all mevedel--session))
  (when-let* ((vb mevedel--view-buffer)
              (_ (buffer-live-p vb)))
    (with-current-buffer vb
      (mevedel-view--interaction-clear))
    (kill-buffer vb)))

(defun mevedel-view--proxy-header-line ()
  "Return the header-line string from the data buffer.
Used as `:eval' form in the view buffer's `header-line-format'."
  (when-let* ((db mevedel--data-buffer)
              (_ (buffer-live-p db))
              (fmt (buffer-local-value 'header-line-format db)))
    (format-mode-line fmt nil nil db)))

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

(defun mevedel-view-close-agent-transcript ()
  "Close the selected transcript side window and kill both transcript buffers."
  (interactive)
  (unless mevedel-view--agent-transcript-p
    (user-error "Not an agent transcript view"))
  (let ((data-buf mevedel--data-buffer)
        (view-buf (current-buffer)))
    (when-let* ((win (get-buffer-window view-buf t)))
      (quit-window nil win))
    (when (buffer-live-p view-buf)
      (kill-buffer view-buf))
    (when (and data-buf (buffer-live-p data-buf))
      (kill-buffer data-buf))))


;;
;;; gptel-menu proxy

(defun mevedel-view--gptel-menu-advice (orig-fn &rest args)
  "Run ORIG-FN in the associated data buffer when called from a view buffer.
`gptel-menu' reads buffer-local gptel state (backend, model, preset,
system message, context) which the view buffer does not own.  Switch
into the paired data buffer so the menu and its suffixes see the same
state as if it had been invoked there directly."
  (if-let* (((derived-mode-p 'mevedel-view-mode))
            (db mevedel--data-buffer)
            ((buffer-live-p db)))
      (with-current-buffer db
        (apply orig-fn args))
    (apply orig-fn args)))

(defun mevedel-view-install-gptel-menu-advice ()
  "Install `gptel-menu' proxy so it targets the data buffer from views."
  (advice-add 'gptel-menu :around #'mevedel-view--gptel-menu-advice))

(defun mevedel-view-uninstall-gptel-menu-advice ()
  "Remove the `gptel-menu' proxy advice."
  (advice-remove 'gptel-menu #'mevedel-view--gptel-menu-advice))


;;
;;; Spinner

(defun mevedel-view--spinner-frame ()
  "Return the current spinner frame string."
  (or (nth (mod mevedel-view--spinner-frame-index
                (max 1 (length mevedel-view-spinner-frames)))
           mevedel-view-spinner-frames)
      ""))

(defun mevedel-view--format-spinner-line (status &optional face)
  "Return propertized spinner line for STATUS.
FACE defaults to `mevedel-view-spinner'."
  (let* ((frame (mevedel-view--spinner-frame))
         (face (or face 'mevedel-view-spinner)))
    (concat
     (unless (string-empty-p frame)
       (concat
        (propertize frame
                    'font-lock-face face
                    'mevedel-view-spinner-frame t
                    'display frame
                    'read-only t
                    'keymap mevedel-view--display-map
                    'front-sticky '(read-only keymap)
                    'rear-nonsticky '(read-only keymap))
        (propertize " "
                    'font-lock-face face
                    'read-only t
                    'keymap mevedel-view--display-map
                    'front-sticky '(read-only keymap)
                    'rear-nonsticky '(read-only keymap))))
     (propertize (concat status "\n")
                 'font-lock-face face
                 'read-only t
                 'keymap mevedel-view--display-map
                 'front-sticky '(read-only keymap)
                 'rear-nonsticky '(read-only keymap)))))

(defun mevedel-view--spinner-active-p ()
  "Return non-nil when this view buffer has visible spinner work."
  (or (and (overlayp mevedel-view--spinner-overlay)
           (overlay-buffer mevedel-view--spinner-overlay)
           (overlay-start mevedel-view--spinner-overlay)
           (overlay-end mevedel-view--spinner-overlay))
      mevedel-view--pending-tool-calls))

(defun mevedel-view--stop-spinner-timer ()
  "Stop the buffer-local spinner animation timer."
  (when (timerp mevedel-view--spinner-timer)
    (cancel-timer mevedel-view--spinner-timer))
  (setq mevedel-view--spinner-timer nil))

(defun mevedel-view--start-spinner-timer ()
  "Start the buffer-local spinner animation timer when needed."
  (when (and mevedel-view-spinner-animate
             (cdr mevedel-view-spinner-frames)
             (not (timerp mevedel-view--spinner-timer)))
    (let ((buffer (current-buffer))
          timer)
      (setq timer
            (run-at-time
             mevedel-view-spinner-interval
             mevedel-view-spinner-interval
             (lambda ()
               (if (not (buffer-live-p buffer))
                   (cancel-timer timer)
                 (with-current-buffer buffer
                   (if (mevedel-view--spinner-active-p)
                       (mevedel-view--spinner-tick)
                     (mevedel-view--stop-spinner-timer)))))))
      (setq mevedel-view--spinner-timer timer))))

(defun mevedel-view--refresh-spinner-frame-spans (property start end face)
  "Refresh spinner frame spans with PROPERTY between START and END.
FACE is kept on the span while its `display' property changes to
the current frame.  This avoids rewriting buffer text during
animation ticks, so point does not jump when it sits on a spinner
line."
  (let ((frame (mevedel-view--spinner-frame))
        (pos start))
    (while (and pos (< pos end))
      (setq pos (text-property-any pos end property t))
      (when pos
        (let ((span-end (or (next-single-property-change pos property nil end)
                            end)))
          (put-text-property pos span-end 'display frame)
          (put-text-property pos span-end 'font-lock-face face)
          (setq pos span-end))))))

(defun mevedel-view--refresh-spinner-overlay ()
  "Refresh the active spinner overlay with the current frame."
  (let ((ov mevedel-view--spinner-overlay))
    (when (and mevedel-view--spinner-status
               (overlayp ov)
               (overlay-buffer ov)
               (overlay-start ov)
               (overlay-end ov)
               (mevedel-view--spinner-region-p (overlay-start ov)
                                               (overlay-end ov)))
      (let ((inhibit-read-only t))
        (mevedel-view--refresh-spinner-frame-spans
         'mevedel-view-spinner-frame
         (overlay-start ov)
         (overlay-end ov)
         'mevedel-view-spinner)))))

(defun mevedel-view--refresh-inline-spinner-frames ()
  "Refresh all inline pending-tool spinner frame spans."
  (let ((inhibit-read-only t))
    (mevedel-view--refresh-spinner-frame-spans
     'mevedel-view-inline-spinner-frame
     (point-min)
     (point-max)
     'mevedel-view-ephemeral)))

(defun mevedel-view--spinner-tick ()
  "Advance visible spinner frames in the current view buffer."
  (setq mevedel-view--spinner-frame-index
        (mod (1+ mevedel-view--spinner-frame-index)
             (max 1 (length mevedel-view-spinner-frames))))
  (mevedel-view--refresh-spinner-overlay)
  (mevedel-view--refresh-inline-spinner-frames))

(defun mevedel-view--start-spinner (&optional status)
  "Show a spinner overlay with STATUS text in the view buffer.
STATUS defaults to \"Thinking...\"."
  (mevedel-view--debug-log
   'spinner-start
   :status status
   :state (mevedel-view--debug-state mevedel--data-buffer))
  (mevedel-view--stop-spinner)
  (setq mevedel-view--spinner-status (or status "Thinking..."))
  (save-excursion
    (goto-char mevedel-view--input-marker)
    (let* ((inhibit-read-only t)
           (text (mevedel-view--format-spinner-line
                  mevedel-view--spinner-status))
           (start (point)))
      (insert text)
      (let ((ov (make-overlay start (point) nil t)))
        (overlay-put ov 'mevedel-view-spinner t)
        (overlay-put ov 'evaporate t)
        (setq mevedel-view--spinner-overlay ov))))
  (mevedel-view--start-spinner-timer))

(defun mevedel-view--spinner-region-p (start end)
  "Return non-nil when START..END still contains spinner text."
  (and start
       end
       (< start end)
       (text-property-any start end
                          'font-lock-face
                          'mevedel-view-spinner)))

(defun mevedel-view--delete-stray-spinner-lines ()
  "Delete spinner text whose overlay was lost.
Full rerenders and async callbacks can temporarily move spinner text
without preserving the overlay object.  The spinner frame text property
is specific to the bottom live spinner, not to folded reasoning
summaries, so deleting those lines is safe during request cleanup."
  (let ((pos (point-min))
        line-start line-end)
    (while (setq pos (text-property-any pos (point-max)
                                        'mevedel-view-spinner-frame t))
      (save-excursion
        (goto-char pos)
        (setq line-start (line-beginning-position)
              line-end (min (point-max) (1+ (line-end-position)))))
      (let ((inhibit-read-only t))
        (delete-region line-start line-end))
      (setq pos line-start))))

(defun mevedel-view--update-spinner (status)
  "Update the spinner overlay to show STATUS text."
  (mevedel-view--debug-log
   'spinner-update
   :status status
   :state (mevedel-view--debug-state mevedel--data-buffer))
  (setq mevedel-view--spinner-status status)
  (let* ((ov mevedel-view--spinner-overlay)
         (live-p (and (overlayp ov)
                      (overlay-buffer ov)
                      (buffer-live-p (overlay-buffer ov))
                      (overlay-start ov)
                      (overlay-end ov)
                      (with-current-buffer (overlay-buffer ov)
                        (mevedel-view--spinner-region-p
                         (overlay-start ov)
                         (overlay-end ov))))))
    (cond
     (live-p
      (let ((inhibit-read-only t)
            (start (overlay-start ov))
            (end (overlay-end ov)))
        (save-excursion
          (goto-char start)
          (delete-region start end)
          (insert (mevedel-view--format-spinner-line status))
          (move-overlay ov start (point)))))
     (t
      ;; The variable might still point at a detached overlay
      ;; (overlay-start = nil) after a rerender wiped its anchor
      ;; region.  Drop the stale reference and start fresh.
      (when ov (delete-overlay ov))
      (setq mevedel-view--spinner-overlay nil)
      (mevedel-view--start-spinner status))))
  (mevedel-view--start-spinner-timer))

(defun mevedel-view--stop-spinner ()
  "Remove the spinner overlay if present.
Tolerates a detached overlay (`overlay-start' / `overlay-end' return
nil when the overlay's anchor region was wiped by an unrelated
rerender).  Only deletes the anchor region when it still has live
bounds and still contains spinner text; either way drops the variable
so the next spinner starts fresh."
  (let ((ov mevedel-view--spinner-overlay))
    (when ov
      (let ((start (overlay-start ov))
            (end (overlay-end ov))
            (buf (overlay-buffer ov)))
        (if (and start end buf (buffer-live-p buf))
            (with-current-buffer buf
              (let ((spinner-p (mevedel-view--spinner-region-p start end)))
                (mevedel-view--debug-log
                 (if spinner-p
                     'spinner-stop-delete
                   'spinner-stop-skip-stale)
                 :region (mevedel-view--debug-region start end)
                 :state (mevedel-view--debug-state mevedel--data-buffer))
                (when spinner-p
                  (let ((inhibit-read-only t))
                    (delete-region start end)))))
          (mevedel-view--debug-log
           'spinner-stop-detached
           :start start
           :end end
           :buffer-live-p (and buf (buffer-live-p buf))
           :state (mevedel-view--debug-state mevedel--data-buffer))))
      (delete-overlay ov)
      (setq mevedel-view--spinner-overlay nil
            mevedel-view--spinner-status nil))
    (mevedel-view--delete-stray-spinner-lines)
    (unless mevedel-view--pending-tool-calls
      (mevedel-view--stop-spinner-timer))))

(defun mevedel-view--discard-spinner-overlay ()
  "Forget the spinner overlay without deleting its covered text.
Incremental and full rerenders delete the spinner text as part of a
larger replacement region.  If the overlay object survives that delete
and then tracks newly inserted assistant text, a later
`mevedel-view--stop-spinner' would delete the assistant block.  Drop
the overlay before such rerenders so the region replacement owns the
text deletion exactly once."
  (mevedel-view--debug-log
   'spinner-discard
   :spinner (mevedel-view--debug-spinner-state)
   :state (mevedel-view--debug-state mevedel--data-buffer))
  (when (overlayp mevedel-view--spinner-overlay)
    (delete-overlay mevedel-view--spinner-overlay))
  (setq mevedel-view--spinner-overlay nil
        mevedel-view--spinner-status nil)
  (unless mevedel-view--pending-tool-calls
    (mevedel-view--stop-spinner-timer)))

(defun mevedel-view--spinner-status-from-region (start end)
  "Return spinner status text from the visible spinner region START..END."
  (let ((text (string-trim
               (buffer-substring-no-properties start end))))
    (dolist (frame mevedel-view-spinner-frames)
      (when (string-prefix-p frame text)
        (setq text (string-trim-left
                    (substring text (length frame))))))
    (if (string-empty-p text) "Thinking..." text)))

(defun mevedel-view--restore-spinner-overlay-in-region (start end)
  "Recreate `mevedel-view--spinner-overlay' over spinner text in START..END.
Full rerenders temporarily discard overlays before deleting and
reinserting display text.  When a live in-flight tail is preserved, its
spinner text is copied as ordinary propertized text; restore the overlay
so the final response path can remove it with `mevedel-view--stop-spinner'."
  (when-let* ((spinner-start
               (text-property-any start end
                                  'font-lock-face
                                  'mevedel-view-spinner))
              (spinner-end
               (or (next-single-property-change
                    spinner-start 'font-lock-face nil end)
                   end)))
    (when (overlayp mevedel-view--spinner-overlay)
      (delete-overlay mevedel-view--spinner-overlay))
    (setq mevedel-view--spinner-status
          (mevedel-view--spinner-status-from-region spinner-start
                                                    spinner-end))
    (let ((ov (make-overlay spinner-start spinner-end nil t)))
      (overlay-put ov 'mevedel-view-spinner t)
      (overlay-put ov 'evaporate t)
      (setq mevedel-view--spinner-overlay ov))
    (mevedel-view--start-spinner-timer)))

(defun mevedel-view--spinner-hook (info)
  "Update spinner from `gptel-pre-tool-call-functions'.
INFO is a plist with at least :name and :args."
  (when-let* ((view-buf (buffer-local-value 'mevedel--view-buffer
                                            (current-buffer)))
              (_ (buffer-live-p view-buf))
              (tool-name (plist-get info :name))
              (args (plist-get info :args)))
    (with-current-buffer view-buf
      ;; `mevedel-view--pre-tool-hook' owns in-flight tool status lines.
      ;; Avoid creating a second overlay-backed "Calling ..." line before
      ;; that hook renders the animated pending-tool live tail.
      (unless (and (markerp mevedel-view--in-flight-turn-start)
                   (marker-position mevedel-view--in-flight-turn-start)
                   (markerp mevedel-view--data-turn-start)
                   (marker-position mevedel-view--data-turn-start))
        (let ((summary (mevedel-view--tool-status-string tool-name args)))
          (mevedel-view--update-spinner summary)))))
  ;; Return nil so the hook does not interfere with tool execution
  nil)

(defun mevedel-view--tool-status-string (tool-name args)
  "Build a short status string for TOOL-NAME with ARGS."
  (let ((primary-arg (mevedel-tool-display-string tool-name args)))
    (if primary-arg
        (format "Calling %s: %s..." tool-name primary-arg)
      (format "Calling %s..." tool-name))))


;;
;;; Data buffer segment extraction

(defun mevedel-view--skip-leading-properties-drawer (pos)
  "Return POS advanced past a leading `:PROPERTIES:' drawer, if any.

gptel-org stores per-buffer state (preset, model, backend, system
prompt, bounds) in an org `:PROPERTIES:' drawer at the top of the
data buffer.  The drawer has no `gptel' text property, so the segment
extractor would classify it as a user turn and render its raw text in
the view on a full rerender (session resume, compaction, manual
refresh).  Skip past it so the rendered view starts at real content."
  (save-excursion
    (goto-char pos)
    (if (and (looking-at-p ":PROPERTIES:$")
             (re-search-forward "^:END:[ \t]*\n" nil t))
        (point)
      pos)))

(defun mevedel-view--skip-leading-summary-block (pos)
  "Return POS advanced past a leading compaction summary block, if any."
  (save-excursion
    (goto-char pos)
    (skip-chars-forward " \t\n")
    (if (and (looking-at-p "#\\+begin_summary\\b")
             (re-search-forward "^#\\+end_summary[^\n]*\n?" nil t))
        (progn
          (skip-chars-forward " \t\n")
          (point))
      pos)))

(defun mevedel-view--extract-segments (start end)
  "Extract segments from the data buffer between START and END.
Returns a list of (TYPE DATA-START DATA-END) where TYPE is one of
`user', `response', `tool', or `ignore'.  Walks forward through
text property changes on the `gptel' property.

START and END are first expanded to whole `gptel' property runs.  This
matters for incremental re-rendering via `gptel-post-response-functions':
those hooks may report a changed region that begins in the middle of an
existing tool or response segment.  Without boundary expansion, the
first extracted tool segment can start after the leading newline and
opening paren of the tool plist, so reparsing sees `:name ...' instead
of `(:name ...)'."
  (let (segments seg-start seg-type)
    (save-excursion
      (setq start (or (previous-single-property-change (min (1+ start) (point-max))
                                                       'gptel nil (point-min))
                      (point-min))
            end (or (next-single-property-change end 'gptel nil (point-max))
                    (point-max)))
      (setq start (mevedel-view--skip-leading-properties-drawer start))
      (setq start (mevedel-view--skip-leading-summary-block start))
      (goto-char start)
      (setq seg-start start
            seg-type (mevedel-view--classify-gptel-prop
                      (get-text-property start 'gptel)))
      (while (< (point) end)
        (let ((next (next-single-property-change (point) 'gptel nil end)))
          (goto-char next)
          (when (< next end)
            ;; Property changed before end -- push the completed segment
            ;; and start a new one.
            (push (list seg-type seg-start next) segments)
            (setq seg-start next
                  seg-type (mevedel-view--classify-gptel-prop
                            (get-text-property next 'gptel))))))
      ;; Push the final (or only) segment.
      (when (< seg-start end)
        (push (list seg-type seg-start end) segments)))
    (nreverse segments)))

(defun mevedel-view--classify-gptel-prop (prop)
  "Classify a `gptel' text property value PROP into a segment type symbol."
  (pcase prop
    ('nil 'user)
    ('response 'response)
    ('ignore 'ignore)
    (`(tool . ,_id) 'tool)
    (_ 'response)))


;;
;;; Turn grouping

(defun mevedel-view--group-into-turns (segments &optional data-buf)
  "Group SEGMENTS into turns.
A turn is a list of consecutive segments belonging to one role.
A new user segment starts a new turn.  Returns a list of turns,
where each turn is a plist (:role ROLE :segments SEGS :start S :end E).
ROLE is `user' or `assistant'.

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
             (mailbox-user-p
              (and (eq type 'user)
                   data-buf
                   (mevedel-view--mailbox-only-text-p
                    (mevedel-view--user-turn-text (list seg) data-buf))))
             (prompt-drawer-after-user-p
              (and (eq type 'ignore)
                   data-buf
                   (null current-role)
                   turns
                   (eq (plist-get (car turns) :role) 'user)
                   (mevedel-view--prompt-drawer-segment-p
                    data-buf seg-start (caddr seg)))))
        (cond
         (prompt-drawer-after-user-p
          (let ((turn (car turns)))
            (setq turn
                  (plist-put turn :segments
                             (append (plist-get turn :segments)
                                     (list seg))))
            (setq turn (plist-put turn :end (caddr seg)))
            (setcar turns turn)))
         ((and (eq type 'user)
               (not mailbox-user-p)
               (memq prev-type '(nil user response))
               ;; Look-ahead: a scaffolding-only nil gap right after
               ;; response with ignore/tool coming next is mid-turn
               ;; reasoning.  A real user prompt can also be followed
               ;; by `#+begin_reasoning' and must still start a user
               ;; turn.
               (not (and (eq prev-type 'response)
                         (memq next-type '(ignore tool))
                         (or (null data-buf)
                             (mevedel-view--scaffolding-only-p
                              data-buf seg-start (caddr seg))))))
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
        (setq prev-type type)
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
    (let* ((raw (buffer-substring-no-properties seg-start seg-end))
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
                 (result-lines (length (split-string result-text "\n" t)))
                 (primary-arg (mevedel-tool-display-string name args)))
            (if primary-arg
                (format "%s%s: %s (%d lines)"
                        mevedel-view--tool-glyph
                        (or name "Tool") primary-arg result-lines)
              (format "%s%s (%d lines)"
                      mevedel-view--tool-glyph
                      (or name "Tool") result-lines)))
        (error
         ;; Fallback: show truncated raw text
         (concat mevedel-view--tool-glyph
                 (truncate-string-to-width
                  (replace-regexp-in-string "[\n\r]+" " " raw)
                  60 nil nil "...")))))))


;;
;;; Renderer plist interpreter
;;
;; Tools can register a pure `renderer' function that consumes the
;; `render-data' side-channel attached to their result and returns a
;; rendering plist of the form:
;;
;;   (:header STRING            ; one-line collapsed summary
;;    :body STRING              ; full expanded body text
;;    :body-mode SYMBOL         ; major-mode symbol for fontification (or nil)
;;    :initially-collapsed-p BOOL)
;;
;; The interpreter below parses the tool segment in the data buffer,
;; invokes the renderer (with a condition-case fallback to the default
;; one-liner on error), and inserts the rendered output.  Expand and
;; collapse re-invoke the renderer on every transition so no state is
;; cached in text properties.

(defun mevedel-view--tool-call-parse (data-buf seg-start seg-end)
  "Parse the tool segment in DATA-BUF between SEG-START and SEG-END.
Return a plist (:name NAME :args ARGS :result STRING :render-data DATA)
or nil when the segment is not a well-formed tool block.

Skips any leading `#+begin_tool …' / `#+end_reasoning' / blank-line
scaffolding before reading the call sexp -- gptel writes the open
tool marker on its own line with no `gptel' property, so a segment
whose start drifted onto the marker (boundary expansion, patched
render-data block) would otherwise fail to parse and force the
renderer to fall back to the bare `Tool' one-liner."
  (with-current-buffer data-buf
    (let* ((raw (buffer-substring-no-properties seg-start seg-end))
           (text (mevedel-view--tool-readable-text raw)))
      (condition-case nil
          (let* ((sexp (read text))
                 (name (plist-get sexp :name))
                 (args (plist-get sexp :args))
                 (sexp-end (with-temp-buffer
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
                 (extract (mevedel-pipeline-extract-render-data full-result)))
            (list :name name
                  :args args
                  :result (car extract)
                  :render-data (cdr extract)))
        (error nil)))))

(defun mevedel-view--rendering-plist-p (p)
  "Return non-nil when P is a structurally valid rendering plist.
Requires:
  `:header'               -- a string (required).
  `:body' (if present)    -- must be a string.
  `:body-mode' (if present) -- must be a symbol.
Malformed plists are rejected here so the interpreter never tries to
insert a non-string or `funcall' a non-symbol."
  (and (listp p)
       (stringp (plist-get p :header))
       (let ((body (plist-get p :body))
             (mode (plist-get p :body-mode)))
         (and (or (null body) (stringp body))
              (or (null mode) (symbolp mode))))))

(defun mevedel-view--invoke-renderer (tool render-data args result)
  "Invoke TOOL's renderer with NAME, ARGS, RESULT, and RENDER-DATA.
Return the rendering plist, or nil when no renderer is registered, the
renderer returns nil (opt-out), the renderer signals an error, or the
returned plist fails `mevedel-view--rendering-plist-p'.  Errors and
malformed returns are surfaced once via `display-warning' under
category `mevedel'; callers treat a nil return as \"fall back to the
one-liner\".

The renderer receives RENDER-DATA as-is (possibly nil): data-driven
renderers like the Edit/Write diff summary can check for their kind
and opt out; output-driven renderers (Grep, Bash, Read, ...) work
straight off ARGS and RESULT without needing render-data."
  (let ((renderer (and tool (mevedel-tool-renderer tool))))
    (when renderer
      (let ((tool-label (or (and tool (mevedel-tool-name tool)) "tool")))
        (condition-case err
            (let ((plist (funcall renderer tool-label args result render-data)))
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
           nil))))))

(defvar mevedel-view--linkify-path-regexp
  ;; Match either an absolute /foo/bar/... path, or a relative segment that
  ;; contains at least one slash (e.g. foo/bar.el, src/mod/file).  The
  ;; trailing `-' inside each character class stays last to avoid being
  ;; parsed as a range delimiter.
  "\\(?:/[[:alnum:]_./+@-]+\\|[[:alnum:]_.+@-]+\\(?:/[[:alnum:]_./+@-]+\\)+\\)"
  "Regular expression matching candidate file paths in rendered bodies.")

(defun mevedel-view--path-candidate-p (text)
  "Return non-nil when TEXT looks like a real path worth linkifying.
Filters trivial cases (no `/') and guards against matching URLs."
  (and (stringp text)
       (string-search "/" text)
       (not (string-prefix-p "//" text))
       (not (string-match-p "\\`https?:" text))))

(defun mevedel-view--resolve-path (raw)
  "Return an absolute path for RAW, or nil when no sensible anchor exists.
Absolute RAW is returned untouched. Relative RAW is resolved against the
workspace root of the session tied to the current data buffer."
  (cond
   ((not (stringp raw)) nil)
   ((file-name-absolute-p raw) raw)
   (t (when-let* ((session (and (boundp 'mevedel--session) mevedel--session))
                  (workspace (ignore-errors
                               (mevedel-session-workspace session)))
                  (root (ignore-errors (mevedel-workspace-root workspace))))
        (expand-file-name raw root)))))

(defun mevedel-view--linkify-path-action (button)
  "Open the file referenced by BUTTON via `find-file'."
  (let ((path (button-get button 'mevedel-view-path)))
    (when (and path (file-exists-p path))
      (find-file-other-window path))))

(defun mevedel-view--linkify-paths-in-range (start end)
  "Scan the buffer between START and END and turn paths into text buttons.
Clickable targets are resolved to absolute paths via
`mevedel-view--resolve-path' and gated on `file-exists-p' -- paths that
don't resolve to an existing file stay as plain text."
  (save-excursion
    (goto-char start)
    (while (re-search-forward mevedel-view--linkify-path-regexp end t)
      (let* ((mb (match-beginning 0))
             (me (match-end 0))
             (raw (buffer-substring-no-properties mb me))
             (resolved (and (mevedel-view--path-candidate-p raw)
                            (mevedel-view--resolve-path raw))))
        (when (and resolved (file-exists-p resolved))
          (make-text-button
           mb me
           'action #'mevedel-view--linkify-path-action
           'mevedel-view-path resolved
           'follow-link t
           'help-echo (format "Visit %s" resolved)))))))

(defun mevedel-view-data-buffer-major-mode ()
  "Return the major mode of the data buffer the view is attached to.

Use this from a tool renderer that wants to fontify its body in the
same flavor as the chat transcript: gptel converts markdown to org
when the chat buffer is in org-mode (see `gptel-org-convert-response'
and the `:transformer' it installs in `gptel-request.el:2565'), so
sub-agent output arrives org-shaped in org-mode chats and
markdown-shaped elsewhere.  Rendering with the data buffer's mode
matches.

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
MODE is a major-mode symbol. Unknown or nil MODE returns TEXT verbatim.
Uses a throwaway temp buffer with `delay-mode-hooks' to avoid side
effects, and `font-lock-ensure' to force a full fontification pass.
Faces are promoted to `font-lock-face' so they survive the view
buffer's font-lock refontification cycles."
  (if (or (null mode)
          (eq mode 'text-mode)
          (eq mode 'fundamental-mode)
          (not (fboundp mode)))
      text
    (condition-case _
        (mevedel-view--promote-face-to-font-lock-face
         (with-temp-buffer
           (insert text)
           (delay-mode-hooks (funcall mode))
           (font-lock-ensure)
           (buffer-string)))
      (error text))))

(defun mevedel-view--agent-invocation (agent-id)
  "Return the live invocation for AGENT-ID from this view's data buffer."
  (when-let* ((data-buf (and (boundp 'mevedel--data-buffer)
                             mevedel--data-buffer))
              ((buffer-live-p data-buf)))
    (with-current-buffer data-buf
      (when-let* ((fsm (cdr (assoc agent-id mevedel-tools--agents-fsm))))
        (mevedel-tools--agent-invocation-at fsm)))))

(defun mevedel-view--agent-state (agent-id)
  "Return the expansion-state plist for AGENT-ID, creating it if needed."
  (unless (hash-table-p mevedel-view--agent-activity-expanded)
    (setq mevedel-view--agent-activity-expanded (make-hash-table :test #'equal)))
  (or (gethash agent-id mevedel-view--agent-activity-expanded)
      (puthash agent-id
               (list :expanded nil
                     :blocked nil
                     :pre-block-expanded nil
                     :changed-during-block nil)
               mevedel-view--agent-activity-expanded)))

(defun mevedel-view--agent-activity-state (agent-id)
  "Return the expansion-state plist for AGENT-ID, creating it if needed."
  (mevedel-view--agent-state agent-id))

(defun mevedel-view-reset-agent-ephemeral-state (&optional view-buffer)
  "Reset view-local ephemeral agent UI state in VIEW-BUFFER.
Defaults to the current buffer."
  (with-current-buffer (or view-buffer (current-buffer))
    (setq mevedel-view--agent-activity-expanded (make-hash-table :test #'equal))
    (setq mevedel-view--agent-status-expanded-p nil)
    (mevedel-view--delete-agent-status-region)))

(defun mevedel-view--agent-set-state (agent-id state)
  "Store STATE as the expansion plist for AGENT-ID."
  (unless (hash-table-p mevedel-view--agent-activity-expanded)
    (setq mevedel-view--agent-activity-expanded (make-hash-table :test #'equal)))
  (puthash agent-id state mevedel-view--agent-activity-expanded))

(defun mevedel-view--set-agent-expanded (agent-id expanded)
  "Set running activity expansion for AGENT-ID to EXPANDED."
  (when agent-id
    (let ((state (mevedel-view--agent-state agent-id)))
      (setq state (plist-put state :expanded expanded))
      (when (plist-get state :blocked)
        (setq state (plist-put state :changed-during-block t)))
      (mevedel-view--agent-set-state agent-id state))))

(defun mevedel-view--agent-status-blocked-p (agent-id)
  "Return non-nil when AGENT-ID is waiting on a parent interaction queue."
  (when-let* ((data-buf (and (boundp 'mevedel--data-buffer)
                             mevedel--data-buffer))
              ((buffer-live-p data-buf))
              (session (buffer-local-value 'mevedel--session data-buf)))
    (or (cl-some (lambda (entry)
                   (equal (plist-get entry :origin) agent-id))
                 (mevedel-session-permission-queue session))
        (cl-some (lambda (entry)
                   (equal (plist-get entry :origin) agent-id))
                 (mevedel-session-plan-queue session)))))

(defun mevedel-view--agent-normalize-expansion-state (agent-id status)
  "Apply blocked/unblocked/terminal transitions for AGENT-ID and STATUS."
  (when agent-id
    (cond
     ((mevedel-view--agent-terminal-status-p status)
      (when (hash-table-p mevedel-view--agent-activity-expanded)
        (remhash agent-id mevedel-view--agent-activity-expanded)))
     ((eq status 'running)
      (let* ((blocked (mevedel-view--agent-status-blocked-p agent-id))
             (state (mevedel-view--agent-state agent-id))
             (was-blocked (plist-get state :blocked)))
        (cond
         ((and blocked (not was-blocked))
          (setq state (plist-put state :pre-block-expanded
                                 (plist-get state :expanded)))
          (setq state (plist-put state :changed-during-block nil))
          (setq state (plist-put state :blocked t))
          (setq state (plist-put state :expanded t)))
         ((and (not blocked) was-blocked)
          (unless (plist-get state :changed-during-block)
            (setq state (plist-put state :expanded
                                   (plist-get state :pre-block-expanded))))
          (setq state (plist-put state :blocked nil))
          (setq state (plist-put state :pre-block-expanded nil))
          (setq state (plist-put state :changed-during-block nil))))
        (mevedel-view--agent-set-state agent-id state))))))

(defun mevedel-view--agent-activity-prefix (item)
  "Return the visible prefix for activity ITEM."
  (pcase (plist-get item :type)
    ('tool-start "->")
    ('tool-finish "✓")
    ('tool-error "✗")
    ('waiting "…")
    ('message "✉")
    ('status
     (pcase (plist-get item :status)
       ('completed "✓")
       ((or 'error 'aborted) "✗")
       ('incomplete "○")
       (_ "…")))
    (_ nil)))

(defun mevedel-view--agent-activity-line (item)
  "Return a one-line display string for activity ITEM, or nil."
  (when-let* ((prefix (mevedel-view--agent-activity-prefix item)))
    (let ((summary
           (or (plist-get item :summary)
               (pcase (plist-get item :type)
                 ('tool-start (format "%s(...)"
                                      (or (plist-get item :tool-name) "Tool")))
                 ('tool-finish (format "%s done"
                                       (or (plist-get item :tool-name) "Tool")))
                 ('tool-error (or (plist-get item :error) "tool error"))
                 ('waiting "waiting")
                 ('message (format "message from %s"
                                   (or (plist-get item :from) "unknown")))
                 ('status (format "%s" (or (plist-get item :status) "")))
                 (_ nil)))))
      (when summary
        (format "%s %s"
                prefix
                (truncate-string-to-width
                 (string-trim
                  (replace-regexp-in-string
                   "[\n\r\t ]+" " " (format "%s" summary)))
                 100 nil nil "..."))))))

(defun mevedel-view--agent-activity-body-from-items (items &optional cap)
  "Return a display body from activity ITEMS.
CAP limits the number of items shown.  Nil means show all items."
  (let* ((cap (max 0 (or cap (length items))))
         (trimmed (and items
                       (last items (min cap (length items)))))
         (warned nil)
         (lines nil))
    (dolist (item trimmed)
      (if-let* ((line (mevedel-view--agent-activity-line item)))
          (push line lines)
        (unless warned
          (setq warned t)
          (display-warning
           'mevedel
           "Malformed agent activity item skipped"
           :warning))))
    (setq lines (nreverse lines))
    (cond
     ((<= cap 0) "")
     ((null lines) "… waiting\n")
     (t (concat (string-join lines "\n") "\n")))))

(defun mevedel-view--agent-activity-body (agent-id)
  "Return the ephemeral activity body for running AGENT-ID."
  (let* ((cap (max 0 mevedel-view-agent-activity-max))
         (inv (and (> cap 0) (mevedel-view--agent-invocation agent-id)))
         (items (and inv (mevedel-agent-invocation-activity inv))))
    (mevedel-view--agent-activity-body-from-items items cap)))

(defun mevedel-view--agent-handle-expanded-p (rendering)
  "Return non-nil when RENDERING should show an expanded activity body."
  (let ((agent-id (plist-get rendering :agent-id))
        (status (plist-get rendering :agent-status)))
    (when (and agent-id (eq status 'running))
      (mevedel-view--agent-normalize-expansion-state agent-id status)
      (plist-get (mevedel-view--agent-state agent-id) :expanded))))

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

(defun mevedel-view--render-collapsed-header (rendering source)
  "Insert the collapsed header for RENDERING with SOURCE coordinates.
RENDERING is a rendering plist. SOURCE is (DATA-START . DATA-END)."
  (let* ((header (plist-get rendering :header))
         (vtype (or (plist-get rendering :vtype) 'tool-summary))
         (line (concat mevedel-view--tool-glyph header))
         (face (mevedel-view--rendering-header-face rendering))
         (ins-start (point)))
    (mevedel-view--agent-normalize-expansion-state
     (plist-get rendering :agent-id)
     (plist-get rendering :agent-status))
    (insert (propertize (concat line "\n")
                        'font-lock-face face
                        'mevedel-view-type vtype
                        'mevedel-view-collapsed t
                        'mevedel-view-source source
                        'mevedel-view-rendered t))
    (when (eq vtype 'agent-handle)
      (mevedel-view--stamp-agent-handle ins-start (point) rendering))
    (mevedel-view--linkify-paths-in-range ins-start (point))))

(defun mevedel-view--render-expanded-body (rendering source)
  "Insert the expanded body for RENDERING with SOURCE coordinates."
  (let* ((header (plist-get rendering :header))
         (agent-id (plist-get rendering :agent-id))
         (agent-status (plist-get rendering :agent-status))
         (agent-activity-p (and agent-id (eq agent-status 'running)))
         (saved-activity
          (and (plist-get rendering :agent-background)
               (not agent-activity-p)
               (plist-get rendering :agent-activity)))
         (body (cond
                (agent-activity-p
                 (mevedel-view--agent-activity-body agent-id))
                (saved-activity
                 (mevedel-view--agent-activity-body-from-items
                  saved-activity))
                (t (or (plist-get rendering :body) ""))))
         (body-mode (plist-get rendering :body-mode))
         (vtype (or (plist-get rendering :vtype) 'tool-summary))
         (fontified (if (or agent-activity-p saved-activity)
                        (propertize body 'font-lock-face 'mevedel-view-ephemeral)
                      (mevedel-view--fontify-as body body-mode)))
         (header-line (concat mevedel-view--tool-glyph header))
         (face (mevedel-view--rendering-header-face rendering))
         (ins-start (point)))
    (mevedel-view--agent-normalize-expansion-state agent-id agent-status)
    (insert (propertize (concat header-line "\n")
                        'font-lock-face face))
    (when (eq vtype 'agent-handle)
      (mevedel-view--stamp-agent-handle ins-start (point) rendering))
    (insert fontified)
    (unless (eq (char-before) ?\n)
      (insert "\n"))
    (add-text-properties ins-start (point)
                         `(mevedel-view-type ,vtype
                           mevedel-view-collapsed nil
                           mevedel-view-source ,source
                           mevedel-view-rendered t))
    (mevedel-view--linkify-paths-in-range ins-start (point))))

(defun mevedel-view--insert-rendered-tool (rendering source)
  "Insert a rendered tool block honouring RENDERING's initial state.
SOURCE is (DATA-START . DATA-END) identifying the data-buffer segment.
When `:initially-collapsed-p' is nil the body is inserted expanded;
otherwise only the header is shown."
  (if (plist-member rendering :initially-collapsed-p)
      (if (and (plist-get rendering :initially-collapsed-p)
               (not (mevedel-view--agent-handle-expanded-p rendering)))
          (mevedel-view--render-collapsed-header rendering source)
        (mevedel-view--render-expanded-body rendering source))
    ;; Default: collapsed.
    (mevedel-view--render-collapsed-header rendering source)))

(defun mevedel-view--segment-rendering (data-buf seg-start seg-end)
  "Return the rendering plist for the tool segment in DATA-BUF.
Returns nil when the segment has no tool, the tool has no renderer,
the renderer declines to render, or the renderer raises."
  (when-let* ((call (mevedel-view--tool-call-parse
                     data-buf seg-start seg-end))
              (tool (mevedel-tool-get (plist-get call :name))))
    (mevedel-view--invoke-renderer
     tool
     (plist-get call :render-data)
     (plist-get call :args)
     (plist-get call :result))))


;;
;;; Thinking block summary

(defun mevedel-view--clean-reasoning-text (text)
  "Strip org scaffolding markers from reasoning TEXT.
Removes reasoning block markers and tool block markers."
  (let ((cleaned text))
    (setq cleaned (replace-regexp-in-string
                   "#\\+\\(?:begin\\|end\\)_reasoning[^\n]*\n?" "" cleaned))
    (setq cleaned (replace-regexp-in-string
                   "#\\+begin_tool[^\n]*\n?" "" cleaned))
    (setq cleaned (replace-regexp-in-string
                   "#\\+end_tool[^\n]*\n?" "" cleaned))
    cleaned))

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

(defun mevedel-view--prompt-drawer-segment-p (data-buf seg-start seg-end)
  "Return non-nil when DATA-BUF segment contains a directive prompt drawer."
  (with-current-buffer data-buf
    (save-excursion
      (goto-char seg-start)
      (re-search-forward "^:PROMPT:\n" seg-end t))))

(defun mevedel-view--thinking-summary (data-buf seg-start seg-end)
  "Generate a summary for a thinking/reasoning block.
Reads content from DATA-BUF between SEG-START and SEG-END.
Returns empty string when the block is trivial (only whitespace
or org scaffolding markers)."
  (with-current-buffer data-buf
    (let* ((text (buffer-substring-no-properties seg-start seg-end))
           (cleaned (mevedel-view--clean-reasoning-text text))
           (lines (split-string cleaned "\n" t "[ \t]+")))
      (if lines
          (format "%sThinking... (%d lines)"
                  mevedel-view--thinking-glyph (length lines))
        ""))))


;;
;;; Rendering

(defun mevedel-view--current-render-insertion-marker ()
  "Return the marker render helpers should insert at."
  (or (and (markerp mevedel-view--render-insertion-marker)
           (marker-position mevedel-view--render-insertion-marker)
           mevedel-view--render-insertion-marker)
      mevedel-view--input-marker))

(defmacro mevedel-view--with-render-boundaries-advancing (&rest body)
  "Execute BODY while zone boundary markers advance across insertions.
History rendering may insert at the status marker while the input
marker is at the same position.  Temporarily making all zone
boundaries advancing keeps the history/status/interaction/input
ordering intact, then restores their normal insertion behavior."
  (declare (indent 0) (debug t))
  `(let ((status-type (and (markerp mevedel-view--status-marker)
                           (marker-insertion-type
                            mevedel-view--status-marker)))
         (interaction-type (and (markerp mevedel-view--interaction-marker)
                                (marker-insertion-type
                                 mevedel-view--interaction-marker)))
         (input-type (and (markerp mevedel-view--input-marker)
                          (marker-insertion-type
                           mevedel-view--input-marker))))
     (unwind-protect
         (progn
           (when (markerp mevedel-view--status-marker)
             (set-marker-insertion-type mevedel-view--status-marker t))
           (when (markerp mevedel-view--interaction-marker)
             (set-marker-insertion-type mevedel-view--interaction-marker t))
           (when (markerp mevedel-view--input-marker)
             (set-marker-insertion-type mevedel-view--input-marker t))
           ,@body)
       (when (markerp mevedel-view--status-marker)
         (set-marker-insertion-type mevedel-view--status-marker status-type))
       (when (markerp mevedel-view--interaction-marker)
         (set-marker-insertion-type mevedel-view--interaction-marker
                                    interaction-type))
       (when (markerp mevedel-view--input-marker)
         (set-marker-insertion-type mevedel-view--input-marker input-type)))))

(defun mevedel-view--render-response (start end)
  "Render the data buffer region [START, END] into the view buffer.
Intended for use as a `gptel-post-response-functions' hook.
Operates on the data buffer (current buffer when the hook fires),
renders into the associated view buffer.

When `mevedel-view--user-pre-rendered' is set (the view's send path
already echoed the user's input), the first user turn in TURNS is
filtered out so it does not appear twice above the assistant reply.
The flag is cleared once consumed so subsequent full re-renders
behave normally."
  (when-let* ((view-buf (buffer-local-value 'mevedel--view-buffer
                                            (current-buffer)))
              (_ (buffer-live-p view-buf)))
    (let ((data-buf (current-buffer)))
      (with-current-buffer view-buf
        (mevedel-view--debug-log
         'render-response-begin
         :start start
         :end end
         :state (mevedel-view--debug-state data-buf start end))
        ;; A final response means any pre-send auto-compaction has
        ;; settled.  Clear the lock defensively in case an async
        ;; callback ran with a different current buffer.
        (with-current-buffer data-buf
          (setq-local mevedel--compaction-in-flight nil))
        ;; Stop the spinner
        (mevedel-view--stop-spinner)
        (mevedel-view--debug-log
         'render-response-after-spinner
         :state (mevedel-view--debug-state data-buf start end))
        ;; Cancel any pending debounced stream render -- the final
        ;; render below subsumes whatever it would have drawn.
        (mevedel-view--cancel-stream-render)
        ;; A final response means gptel has left the live tool-call
        ;; phase.  Clear pending status before the final incremental
        ;; render so stale "Calling ..." lines are not preserved or
        ;; reinserted beside completed tool output.
        (setq mevedel-view--pending-tool-calls nil)
        (mevedel-view--delete-pending-tool-live-lines)
        ;; Delegate to the shared incremental path, which deletes the
        ;; in-flight assistant turn (if any) and re-renders it from the
        ;; data buffer.  After the final render completes, clear the
        ;; in-flight markers so the next exchange starts clean.
        (mevedel-view--render-incremental data-buf start end)
        (mevedel-view--debug-log
         'render-response-after-incremental
         :state (mevedel-view--debug-state data-buf start end))
        (mevedel-view--stop-spinner-timer)
        (when (markerp mevedel-view--in-flight-turn-start)
          (set-marker mevedel-view--in-flight-turn-start nil)
          (setq mevedel-view--in-flight-turn-start nil))
        (when (markerp mevedel-view--data-turn-start)
          (set-marker mevedel-view--data-turn-start nil)
          (setq mevedel-view--data-turn-start nil)))))
  ;; gptel inspects hook return values for control plists.  Rendering
  ;; is side-effect-only; never leak propertized UI strings upward.
  nil)

(defmacro mevedel-view--preserving-window-state (&rest body)
  "Execute BODY preserving window-point and window-start of every
window displaying the current buffer.

Used to wrap delete-and-re-render operations so the user's scroll
position and caret do not jump back to the edit site on every
progress tick.  Positions that are no longer valid after BODY (e.g.
point was inside the deleted region) are quietly clamped to the
buffer."
  (declare (indent 0) (debug t))
  `(let ((mevedel-view--pww-saved
          (mapcar (lambda (w)
                    (list w (window-point w) (window-start w)))
                  (get-buffer-window-list (current-buffer) nil t))))
     (prog1 (progn ,@body)
       (dolist (entry mevedel-view--pww-saved)
         (pcase-let ((`(,w ,wp ,ws) entry))
           (when (window-live-p w)
             (when (and wp (<= wp (point-max)))
               (set-window-point w wp))
             (when (and ws (<= ws (point-max)))
               (set-window-start w ws t))))))))

(defvar mevedel-view--collapsible-vtypes)

(defun mevedel-view--capture-collapse-states (from to)
  "Return an alist of collapse states for sections in FROM..TO.

Keys are (VTYPE . DATA-START) -- the segment vtype plus the car of
its `mevedel-view-source' cons.  Values are t when collapsed, nil
when expanded.  Identity is keyed on the data-start only (not the
full source cons) so thinking-summary and tool-summary segments keep
their saved state even when streaming extends the segment's end
position."
  (let ((states nil)
        (pos from))
    (while (< pos to)
      (let ((vtype (get-text-property pos 'mevedel-view-type))
            (source (get-text-property pos 'mevedel-view-source))
            (collapsed (get-text-property pos 'mevedel-view-collapsed))
            (next (or (next-single-property-change
                       pos 'mevedel-view-source nil to)
                      to)))
        (when (and source
                   (consp source)
                   (memq vtype mevedel-view--collapsible-vtypes))
          (let ((key (cons vtype (car source))))
            (unless (assoc key states)
              (push (cons key (and collapsed t)) states))))
        (setq pos next)))
    states))

(defun mevedel-view--apply-collapse-states (from to states)
  "Toggle sections in FROM..TO so collapse state matches STATES.
STATES is an alist from `mevedel-view--capture-collapse-states'.
Sections whose current state already matches are left alone; only
mismatches are toggled, via `mevedel-view--expand-section' /
`--collapse-section'.  Upper bound is held as a marker so toggles
that change buffer length do not invalidate the walk."
  (when states
    (save-excursion
      (let ((to-marker (copy-marker to t)))
        (unwind-protect
            (let ((pos from))
              (while (< pos (marker-position to-marker))
                (let ((vtype (get-text-property pos 'mevedel-view-type))
                      (source (get-text-property pos 'mevedel-view-source))
                      (collapsed (and (get-text-property
                                       pos 'mevedel-view-collapsed)
                                      t)))
                  (when (and source
                             (consp source)
                             (memq vtype mevedel-view--collapsible-vtypes))
                    (let* ((entry (assoc (cons vtype (car source)) states))
                           (saved (cdr entry)))
                      (when (and entry (not (eq collapsed saved)))
                        (goto-char pos)
                        (if saved
                            (mevedel-view--collapse-section source vtype)
                          (mevedel-view--expand-section source vtype)))))
                  (setq pos (or (next-single-property-change
                                 pos 'mevedel-view-source nil
                                 (marker-position to-marker))
                                (marker-position to-marker))))))
          (set-marker to-marker nil))))))

(defun mevedel-view--pre-rendered-user-visible-p ()
  "Return non-nil when the current in-flight marker follows a user block.
This detects whether the send-path echo inserted by
`mevedel-view--insert-user-message' is still present.  A full rerender
can wipe that ephemeral block while an in-flight marker remains live, so
the marker alone is not enough to decide whether a leading user turn
from the data buffer should be filtered."
  (when-let* (((markerp mevedel-view--in-flight-turn-start))
              (pos (marker-position mevedel-view--in-flight-turn-start))
              ((> pos (point-min))))
    (save-excursion
      (goto-char pos)
      (skip-chars-backward " \t\n")
      (and (> (point) (point-min))
           (eq (get-text-property (1- (point)) 'mevedel-view-type)
               'user)))))

(defun mevedel-view--render-incremental (data-buf &optional start end)
  "Rebuild the in-flight assistant turn in the view from DATA-BUF.

Call from the view buffer.  Deletes the region between
`mevedel-view--in-flight-turn-start' and `mevedel-view--input-marker'
(the current rendering of the in-flight assistant turn) and
re-renders from the data buffer range
\[`mevedel-view--data-turn-start', end-of-data-buffer], grouping
segments into turns and rendering them at the input marker.

When `mevedel-view--pending-tool-calls' is non-empty, appends one
\"Calling TOOLNAME…\" line per in-flight tool (capped by
`mevedel-view-pending-tools-visible-max') so the user sees what's
running even before results land in the data buffer.

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
  (let* ((turn-from (and (markerp mevedel-view--data-turn-start)
                         (marker-position mevedel-view--data-turn-start)))
         (data-from (or start turn-from))
         (data-to
          (or end
              (with-current-buffer data-buf (point-max))))
         (segments (when (and data-from data-to)
                     (with-current-buffer data-buf
                       (mevedel-view--extract-segments data-from data-to))))
         (turns (mevedel-view--group-into-turns segments data-buf))
         (in-flight-p (and (markerp mevedel-view--in-flight-turn-start)
                           (marker-position mevedel-view--in-flight-turn-start)))
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
    ;; Filter the send-path user turn.  `--extract-segments' expands a
    ;; start position back to the containing `gptel' property run, so a
    ;; data-turn marker sitting at the end of the prompt can still yield
    ;; a leading user turn whose source starts before DATA-FROM.  That is
    ;; the prompt already echoed by the send path, not new mailbox/user
    ;; content that arrived later in the turn.
    (while (and turns
                (or mevedel-view--user-pre-rendered
                    pre-rendered-user-visible-p
                    (and data-from
                         (< (or (plist-get (car turns) :start)
                                data-from)
                            data-from)))
                (eq (plist-get (car turns) :role) 'user))
      (setq turns (cdr turns)))
    (setq mevedel-view--user-pre-rendered nil)
    (mevedel-view--debug-log
     'incremental-filtered
     :turns (mapcar (lambda (turn) (plist-get turn :role)) turns)
     :turn-detail (mevedel-view--debug-turn-summary turns data-buf)
     :pending pending
     :state (mevedel-view--debug-state data-buf data-from data-to))
    (mevedel-view--preserving-window-state
      ;; rebuild region stops at status-marker (top of zone
      ;; 2) rather than input-marker, so any future status- or
      ;; interaction-zone overlay anchors survive the re-render.
      ;; status-marker == input-marker today (zones empty), so this
      ;; is a no-op for current behavior; setting it correctly now
      ;; prevents a phase-8 regression when zone overlays land.
      (let* ((inhibit-read-only t)
             ;; Permission prompts and tool callbacks can trigger a view
             ;; refresh in the small window after pending tool lines have
             ;; been removed but before gptel has written the corresponding
             ;; result segments.  In that case, keep the previous in-flight
             ;; rendering instead of replacing it with a blank region.
             (replace-p (or turns pending))
             ;; Reject markers that pass `markerp' but are detached
             ;; (`marker-position' returns nil): they would crash
             ;; `<=' / `delete-region' / `apply-collapse-states' below.
             (rebuild-end
              (or (and (markerp mevedel-view--status-marker)
                       (marker-position mevedel-view--status-marker)
                       mevedel-view--status-marker)
                  (and (markerp mevedel-view--input-marker)
                       (marker-position mevedel-view--input-marker)
                       mevedel-view--input-marker)))
             (capture-p
              (and in-flight-p
                   rebuild-end
                   (<= (marker-position mevedel-view--in-flight-turn-start)
                       (marker-position rebuild-end))))
             (saved-states
              (when (and replace-p capture-p)
                (mevedel-view--capture-collapse-states
                 (marker-position mevedel-view--in-flight-turn-start)
                 (marker-position rebuild-end)))))
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
                    (marker-position mevedel-view--in-flight-turn-start)
                    (marker-position rebuild-end))
           :state (mevedel-view--debug-state data-buf data-from data-to))
          (mevedel-view--discard-spinner-overlay)
          (delete-region mevedel-view--in-flight-turn-start
                         rebuild-end)
          (mevedel-view--debug-log
           'incremental-after-delete
           :state (mevedel-view--debug-state data-buf data-from data-to)))
        (when replace-p
          ;; The in-flight turn belongs to zone 1 (history).  Insert it
          ;; at the status boundary so any real-text status/interaction
          ;; UI below that boundary remains below the transcript.
          (let ((mevedel-view--render-insertion-marker rebuild-end))
            (dolist (turn turns)
              (mevedel-view--render-turn turn data-buf))
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
                   in-flight-p
                   rebuild-end
                   (marker-position mevedel-view--in-flight-turn-start)
                   (marker-position rebuild-end))
          (mevedel-view--apply-collapse-states
           (marker-position mevedel-view--in-flight-turn-start)
           (marker-position rebuild-end)
           saved-states))
        (unless mevedel-view--agent-transcript-p
          (mevedel-view--render-agent-status)
          (mevedel-view--interaction-rebuild))))))

(defun mevedel-view--cancel-stream-render ()
  "Cancel any pending debounced stream render on the view buffer."
  (when (and (boundp 'mevedel-view--stream-render-timer)
             mevedel-view--stream-render-timer)
    (cancel-timer mevedel-view--stream-render-timer)
    (setq mevedel-view--stream-render-timer nil)))

(defun mevedel-view--schedule-stream-render ()
  "Schedule a debounced incremental render driven by the stream hook.

Intended for `gptel-post-stream-hook', which fires once per streamed
chunk in the data buffer.  Defers the incremental render by
`mevedel-view-stream-render-delay' seconds of quiescence so the view
rebuilds at most a few times per second rather than per token."
  (when-let* ((view-buf (and (boundp 'mevedel--view-buffer)
                             mevedel--view-buffer))
              ((buffer-live-p view-buf))
              (data-buf (current-buffer)))
    (with-current-buffer view-buf
      (mevedel-view--debug-log
       'stream-render-schedule
       :state (mevedel-view--debug-state data-buf))
      ;; Only schedule when a turn is in-flight.  Before the first
      ;; user send -- or after the final post-response cleanup -- the
      ;; incremental markers are nil and rendering would no-op.
      (when (and (markerp mevedel-view--in-flight-turn-start)
                 (markerp mevedel-view--data-turn-start))
        (unless mevedel-view--stream-render-timer
          (setq mevedel-view--stream-render-timer
                (run-at-time
                 mevedel-view-stream-render-delay nil
                 (lambda ()
                   (when (buffer-live-p view-buf)
                     (with-current-buffer view-buf
                       (setq mevedel-view--stream-render-timer nil)
                       (when (buffer-live-p data-buf)
                         (mevedel-view--debug-log
                          'stream-render-fire
                          :state (mevedel-view--debug-state data-buf))
                         (mevedel-view--render-incremental data-buf)))))))))))
  nil)

(defun mevedel-view--pending-tool-key (info)
  "Return the call-id key for pending tool INFO.

Prefer gptel's per-call id when present so identical parallel calls
remain distinct in the live tail.  Fall back to the old name/args
fingerprint only for older gptel builds that do not expose an id."
  (or (plist-get info :id)
      (plist-get info :call-id)
      (plist-get info :tool-call-id)
      (plist-get info :tool_call_id)
      (cons (plist-get info :name)
            (let ((print-level 4)
                  (print-length 32)
                  (print-circle t))
              (prin1-to-string (plist-get info :args))))))

(defun mevedel-view--pre-tool-hook (args)
  "Mark an in-flight tool call and re-render the view.

Runs as a `gptel-pre-tool-call-functions' hook in the data buffer.
Adds an entry to `mevedel-view--pending-tool-calls' on the
associated view buffer so `mevedel-view--render-incremental' appends
a \"Calling TOOLNAME…\" status line, then triggers an incremental
render immediately so any assistant text or reasoning that arrived
before this tool call is reflected in the view before the tool
runs."
  (when-let* ((view-buf (and (boundp 'mevedel--view-buffer)
                             mevedel--view-buffer))
              ((buffer-live-p view-buf))
              (name (plist-get args :name))
              (data-buf (current-buffer)))
    (with-current-buffer view-buf
      (mevedel-view--debug-log
       'pre-tool-hook
       :args (list :id (plist-get args :id)
                   :call-id (plist-get args :call-id)
                   :name name)
       :state (mevedel-view--debug-state data-buf))
      (mevedel-view--cancel-stream-render)
      (unless (equal name "Agent")
        (let ((key (mevedel-view--pending-tool-key args))
              (label (mevedel-view--tool-status-string
                      name (plist-get args :args))))
          (unless (assoc key mevedel-view--pending-tool-calls)
            (setq mevedel-view--pending-tool-calls
                  (append mevedel-view--pending-tool-calls
                          (list (cons key label)))))))
      ;; `mevedel-view--spinner-hook' runs earlier in the same hook
      ;; list.  Replace its overlay-backed status with the pending-tool
      ;; live line below, whose frame span is refreshed by the shared
      ;; spinner timer.
      (mevedel-view--stop-spinner)
      (mevedel-view--start-spinner-timer)
      (when (and (markerp mevedel-view--in-flight-turn-start)
                 (markerp mevedel-view--data-turn-start))
        (mevedel-view--render-incremental data-buf)
        (mevedel-view--debug-log
         'pre-tool-hook-after-render
         :state (mevedel-view--debug-state data-buf)))))
  ;; gptel pre-tool hooks must return nil unless they intentionally
  ;; provide a control plist.
  nil)

(defun mevedel-view--post-tool-hook (args)
  "Clear the in-flight tool marker and re-render the view.

Runs as a `gptel-post-tool-call-functions' hook in the data buffer.
ARGS is the tool-call plist.  The re-render picks up the just-
completed tool call and its result from the data buffer, replacing
the ephemeral \"Calling TOOLNAME…\" line inserted by the pre-tool
hook."
  (when-let* ((view-buf (and (boundp 'mevedel--view-buffer)
                             mevedel--view-buffer))
              ((buffer-live-p view-buf))
              (name (plist-get args :name))
              (data-buf (current-buffer)))
    (with-current-buffer view-buf
      (mevedel-view--debug-log
       'post-tool-hook
       :args (list :id (plist-get args :id)
                   :call-id (plist-get args :call-id)
                   :name name)
       :state (mevedel-view--debug-state data-buf))
      (mevedel-view--cancel-stream-render)
      (let ((key (mevedel-view--pending-tool-key args)))
        (setq mevedel-view--pending-tool-calls
              (assoc-delete-all key mevedel-view--pending-tool-calls)))
      (unless mevedel-view--pending-tool-calls
        (mevedel-view--delete-pending-tool-live-lines))
      (unless (or mevedel-view--pending-tool-calls
                  mevedel-view--spinner-overlay)
        (mevedel-view--stop-spinner-timer))
      (when (and (markerp mevedel-view--in-flight-turn-start)
                 (markerp mevedel-view--data-turn-start))
        (mevedel-view--render-incremental data-buf)
        (mevedel-view--debug-log
         'post-tool-hook-after-render
         :state (mevedel-view--debug-state data-buf)))))
  ;; gptel post-tool hooks must return nil unless they intentionally
  ;; provide a control plist.
  nil)

(defun mevedel-view--delete-pending-tool-live-lines ()
  "Delete rendered pending-tool live-tail lines from the view buffer."
  (let ((inhibit-read-only t))
    (cl-labels
        ((delete-ranges-for-property
          (property)
          (let ((pos (point-min)))
            (while (setq pos (text-property-any pos (point-max) property t))
              (let* ((line-start (save-excursion
                                   (goto-char pos)
                                   (line-beginning-position)))
                     (line-end (save-excursion
                                 (goto-char pos)
                                 (min (point-max)
                                      (1+ (line-end-position))))))
                (delete-region line-start line-end)
                (setq pos line-start))))))
      ;; Newer pending lines carry the whole-line property.  Older
      ;; live tails only carried the frame property on the spinner
      ;; glyph; remove those too so upgraded sessions do not retain a
      ;; stale "Calling ..." line.
      (delete-ranges-for-property 'mevedel-view-pending-tool-live)
      (delete-ranges-for-property 'mevedel-view-inline-spinner-frame))))

(defun mevedel-view--insert-pending-tool-lines (entries)
  "Insert ephemeral `Calling X…' lines for ENTRIES.
ENTRIES is a subset of `mevedel-view--pending-tool-calls' (head N).
When the full list exceeds `mevedel-view-pending-tools-visible-max',
the caller passes only the visible head and a tail-summary line is
  appended."
  (save-excursion
    (let ((target (mevedel-view--current-render-insertion-marker)))
      (goto-char target)
      (mevedel-view--with-render-boundaries-advancing
        (let ((inhibit-read-only t)
              (cap mevedel-view-pending-tools-visible-max)
              (total (length mevedel-view--pending-tool-calls))
              (frame (mevedel-view--spinner-frame)))
          (dolist (entry entries)
            (let ((label (cdr entry)))
              (insert
               (propertize frame
                           'font-lock-face 'mevedel-view-ephemeral
                           'mevedel-view-inline-spinner-frame t
                           'mevedel-view-pending-tool-live t
                           'display frame
                           'read-only t
                           'front-sticky '(read-only)
                           'rear-nonsticky '(read-only))
               (propertize (format " %s\n" label)
                           'font-lock-face 'mevedel-view-ephemeral
                           'mevedel-view-pending-tool-live t
                           'read-only t
                           'front-sticky '(read-only)
                           'rear-nonsticky '(read-only)))))
          (when (> total cap)
            (insert (propertize
                     frame
                     'font-lock-face 'mevedel-view-ephemeral
                     'mevedel-view-inline-spinner-frame t
                     'mevedel-view-pending-tool-live t
                     'display frame
                     'read-only t
                     'front-sticky '(read-only)
                     'rear-nonsticky '(read-only))
                    (propertize
                     (format " %d more tools running…\n" (- total cap))
                     'font-lock-face 'mevedel-view-ephemeral
                     'mevedel-view-pending-tool-live t
                     'read-only t
                     'front-sticky '(read-only)
                     'rear-nonsticky '(read-only)))))))))


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
        (mevedel-view--with-render-boundaries-advancing
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
    (let ((parts nil))
      (dolist (seg segments)
        (let* ((seg-start (cadr seg))
               (seg-end (caddr seg))
               (text (buffer-substring-no-properties seg-start seg-end)))
          ;; Strip org heading prefix (e.g., "*** ")
          (when (string-match "\\`\\*+ " text)
            (setq text (substring text (match-end 0))))
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
      (string-join (nreverse parts) "\n"))))

(defun mevedel-view--mailbox-only-text-p (text)
  "Return non-nil when TEXT contains only mailbox delivery blocks.

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
             (cond
              ((eobp))
              ((looking-at "<agent-message\\s-+[^>]*from=\"[^\"]+\"\\s-*>")
               (setq found t)
               (goto-char (match-end 0))
               (if (search-forward "</agent-message>" nil t)
                   nil
                 (setq ok nil)))
              ((looking-at "<agent-result\\s-+[^>]*agent-id=\"[^\"]+\"[^>]*>")
               (setq found t)
               (goto-char (match-end 0))
               (if (search-forward "</agent-result>" nil t)
                   nil
                 (setq ok nil)))
              (t
               (setq ok nil))))
           (and found ok)))))

(defun mevedel-view--render-user-turn (segments data-buf)
  "Render user SEGMENTS from DATA-BUF."
  (let* ((raw-text (mevedel-view--user-turn-text segments data-buf))
         (prompt-drawers (mevedel-view--user-turn-prompt-drawers
                          segments data-buf))
         (text (if prompt-drawers
                   (mevedel-view--fontify-directive-display-text
                    (mevedel-view--directive-turn-display-text raw-text))
                 raw-text))
        (text-start nil))
    (cond
     ((and (string-empty-p text) (null prompt-drawers))
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
      (dolist (drawer prompt-drawers)
        (mevedel-view--insert-rendered-tool
         (list :header "Prompt"
               :body (plist-get drawer :body)
               :body-mode 'markdown-mode
               :vtype 'prompt-summary
               :initially-collapsed-p t)
         (cons (plist-get drawer :start)
               (plist-get drawer :end)))))))
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
        (when (memq (car seg) '(user ignore))
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
           (summary (mevedel-view--thinking-summary
                     data-buf first-start last-end)))
      (unless (string-empty-p summary)
        (insert (propertize (concat summary "\n")
                            'font-lock-face 'mevedel-view-thinking-summary
                            'mevedel-view-type 'thinking-summary
                            'mevedel-view-collapsed t
                            'mevedel-view-source (cons first-start last-end)))))))

(defun mevedel-view--ensure-blank-line-before-response ()
  "Insert a blank line before a response segment when missing.
Visually separates the response text from preceding thinking summaries,
tool summaries, or the \"Assistant\" turn header.  A blank line is only
added when point is not already at the start of a blank line -- so
consecutive response segments don't accumulate extra spacing."
  (unless (or (bobp)
              (save-excursion
                (forward-line 0)
                (looking-at-p "^$")))
    (insert "\n")))

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
  (let (tool-group thinking-group)
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
                 (with-current-buffer (buffer-local-value
                                       'mevedel--view-buffer data-buf)
                   (unless (string-empty-p text)
                     (mevedel-view--ensure-blank-line-before-response)
                     (let ((start (point)))
                       (insert (mevedel-view--fontify-response text) "\n")
                       (add-text-properties
                        start (point)
                        `(mevedel-view-source ,(cons seg-start seg-end)
                          mevedel-view-type response
                          mevedel-view-collapsed nil)))))))))
          ('tool
           ;; Flush thinking group before tools
           (mevedel-view--flush-thinking-group thinking-group data-buf)
           (setq thinking-group nil)
           ;; Accumulate consecutive tool segments
           (push seg tool-group))
          ('user
           (let ((seg-start (cadr seg))
                 (seg-end (caddr seg)))
             (if (mevedel-view--mailbox-only-text-p
                  (mevedel-view--user-turn-text (list seg) data-buf))
                 (progn
                   (mevedel-view--flush-thinking-group thinking-group data-buf)
                   (setq thinking-group nil)
                   (when tool-group
                     (mevedel-view--render-tool-group
                      (nreverse tool-group) data-buf)
                     (setq tool-group nil))
                   (let ((text (mevedel-view--user-turn-text
                                (list seg) data-buf))
                         (text-start nil))
                     (mevedel-view--ensure-blank-line-before-response)
                     (setq text-start (point))
                     (insert text "\n")
                     (mevedel-view--decorate-agent-result-blocks
                      text-start (point))
                     (mevedel-view--decorate-agent-message-blocks
                      text-start (point))))
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
                 (push seg thinking-group)))))
          ('ignore
           ;; Drop org-only glue (`#+end_tool', `#+begin_tool …', blank
           ;; lines) so it doesn't surface as a one-line `Thinking…'
           ;; between adjacent tool blocks.  Skip without flushing the
           ;; tool-group so consecutive tool segments separated only
           ;; by glue still group / render together.
           ;; Flush tool group before thinking
           (when tool-group
             (mevedel-view--render-tool-group (nreverse tool-group) data-buf)
             (setq tool-group nil))
           ;; Accumulate consecutive thinking segments
           (push seg thinking-group)))))
    ;; Flush remaining groups
    (mevedel-view--flush-thinking-group thinking-group data-buf)
    (when tool-group
      (mevedel-view--render-tool-group (nreverse tool-group) data-buf))))

(defun mevedel-view--render-tool-group (tool-segments data-buf)
  "Render consecutive TOOL-SEGMENTS from DATA-BUF.
Each tool call gets its own collapsible entry.  A registered
`:renderer' is invoked when the segment carries a render-data
side-channel, falling back to the default one-liner otherwise."
  (dolist (seg tool-segments)
    (let* ((seg-start (cadr seg))
           (seg-end (caddr seg))
           (source (cons seg-start seg-end))
           (rendering (mevedel-view--segment-rendering
                       data-buf seg-start seg-end)))
      (if rendering
          (mevedel-view--insert-rendered-tool rendering source)
        (let ((summary (mevedel-view--tool-one-liner
                        data-buf seg-start seg-end)))
          (insert (propertize (concat summary "\n")
                              'font-lock-face 'mevedel-view-tool-summary
                              'mevedel-view-type 'tool-summary
                              'mevedel-view-collapsed t
                              'mevedel-view-source source)))))))

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


;;
;;; Expand/collapse

(defvar mevedel-view--collapsible-vtypes
  '(thinking-summary tool-summary response
    plan-summary agent-handle prompt-summary)
  "Vtypes that `mevedel-view-toggle-section' treats as section-level
folds.  Turn-level folds (`turn-header', `turn-summary') are handled
separately.  Regions with other vtypes are navigable but not
toggleable.")

(defun mevedel-view--truncate-line (text limit)
  "Return TEXT truncated to LIMIT characters with a trailing `...'."
  (if (> (length text) limit)
      (concat (substring text 0 (max 0 (- limit 3))) "...")
    text))

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
     ((memq vtype '(turn-header turn-summary))
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
        (mevedel-view--set-agent-expanded
         agent-id
         (not (plist-get (mevedel-view--agent-state agent-id)
                         :expanded)))
        (mevedel-view--full-rerender)))
     ((and source (memq vtype mevedel-view--collapsible-vtypes))
      (if collapsed
          (mevedel-view--expand-section source vtype)
        (mevedel-view--collapse-section source vtype)))
     ((and (eq vtype 'agent-handle)
           (get-text-property (point) 'mevedel-view-agent-id))
      (let ((agent-id (get-text-property (point) 'mevedel-view-agent-id)))
        (mevedel-view-agent-handle-activate agent-id)))
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
                                       (count-lines (car range) (cdr range)))
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
                  (format " [%d lines collapsed]" line-count)
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
              (and (markerp mevedel-view--in-flight-turn-start)
                   (marker-position mevedel-view--in-flight-turn-start)
                   (<= view-start
                       (marker-position mevedel-view--in-flight-turn-start)
                       view-end))))
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
                      (when (and (eq (plist-get rendering :vtype) 'agent-handle)
                                 (eq (plist-get rendering :agent-status) 'running))
                        (mevedel-view--set-agent-expanded
                         (plist-get rendering :agent-id) t))
                      (mevedel-view--render-expanded-body rendering source)
                      (mevedel-view--add-display-region-properties
                       ins-start (point) (plist-get rendering :vtype)))
                  (let ((text (mevedel-view--data-substring
                               data-buf data-start data-end)))
                    ;; Clean org scaffolding from reasoning blocks
                    (when (eq vtype 'thinking-summary)
                      (setq text (string-trim
                                  (mevedel-view--clean-reasoning-text text))))
                    ;; Trim response text to match the initial render,
                    ;; then apply org fontification so an expanded response
                    ;; matches the look of the freshly-rendered inline one.
                    (when (eq vtype 'response)
                      (setq text (mevedel-view--fontify-response
                                  (string-trim text))))
                    (when (eq vtype 'prompt-summary)
                      (setq text (mevedel-view--fontify-as
                                  (string-trim
                                   (mevedel-view--prompt-drawer-body
                                    data-buf data-start data-end))
                                  'markdown-mode)))
                    (when (string-empty-p text)
                      (setq text "[section no longer available]"))
                    (insert text)
                    (unless (eq (char-before) ?\n)
                      (insert "\n"))
                    (add-text-properties view-start (point)
                                         `(mevedel-view-source ,source
                                           mevedel-view-type ,vtype
                                           mevedel-view-collapsed nil))
                    (mevedel-view--add-display-region-properties
                     view-start (point) vtype)))
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
         (data-start (car source))
         (data-end (cdr source))
         (rendering (and data-buf (buffer-live-p data-buf)
                         (mevedel-view--segment-rendering
                          data-buf data-start data-end)))
         (summary
          (cond
           (rendering
            (concat mevedel-view--tool-glyph (plist-get rendering :header)))
           (t
            (pcase vtype
              ('tool-summary
               (mevedel-view--tool-one-liner data-buf data-start data-end))
              ('thinking-summary
               (mevedel-view--thinking-summary data-buf data-start data-end))
              ('response
               (mevedel-view--response-summary data-buf data-start data-end))
              ('prompt-summary
               (concat mevedel-view--tool-glyph "Prompt")))))))
    (when (and bounds data-buf (buffer-live-p data-buf) summary)
      (let* ((inhibit-read-only t)
             (view-start (car bounds))
             (view-end (cdr bounds))
             (face (pcase vtype
                     ((or 'tool-summary 'agent-handle 'prompt-summary)
                      'mevedel-view-tool-summary)
                     ('thinking-summary 'mevedel-view-thinking-summary)
                     ('response 'mevedel-view-response-summary)))
             (turn-id (get-text-property (car bounds) 'mevedel-view-turn-id))
             (in-flight-after-section-p
              (and (markerp mevedel-view--in-flight-turn-start)
                   (marker-position mevedel-view--in-flight-turn-start)
                   (<= view-start
                       (marker-position mevedel-view--in-flight-turn-start)
                       view-end))))
        (save-excursion
          (goto-char view-start)
          (set-marker-insertion-type mevedel-view--input-marker t)
          (unwind-protect
              (progn
                (delete-region view-start view-end)
                (when (and (eq (plist-get rendering :vtype) 'agent-handle)
                           (eq (plist-get rendering :agent-status) 'running))
                  (mevedel-view--set-agent-expanded
                   (plist-get rendering :agent-id) nil))
                (let ((ins-start (point)))
                  (insert (propertize (concat summary "\n")
                                      'font-lock-face face
                                      'mevedel-view-type vtype
                                      'mevedel-view-collapsed t
                                      'mevedel-view-source source))
                  (mevedel-view--add-display-region-properties
                   ins-start (point) vtype)
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
  (let* ((text (mevedel-view--response-display-text
                (mevedel-view--data-substring data-buf data-start data-end)))
         (trimmed (string-trim text))
         (lines (split-string trimmed "\n"))
         (non-empty (seq-drop-while #'string-empty-p lines))
         (first-line (or (car non-empty) ""))
         (line-count (length lines)))
    (format "%s%s%s (%d lines)"
            mevedel-view--response-glyph
            (mevedel-view--truncate-line first-line 80)
            (if (> line-count 1) "..." "")
            line-count)))

(defun mevedel-view--prompt-drawer-body (data-buf data-start data-end)
  "Return the body of a `:PROMPT:' drawer in DATA-BUF."
  (with-current-buffer data-buf
    (save-excursion
      (goto-char data-start)
      (if (re-search-forward "^:PROMPT:\n" data-end t)
          (let ((body-start (point)))
            (if (re-search-forward "^:END:[ \t]*\n?" data-end t)
                (buffer-substring-no-properties
                 body-start (match-beginning 0))
              (buffer-substring-no-properties body-start data-end)))
        (mevedel-view--data-substring data-buf data-start data-end)))))


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
synthesises a preview with activity counters."
  (let ((tool-count 0)
        (has-thinking nil)
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
        (format "Assistant — %s (%d lines%s%s)"
                (mevedel-view--truncate-line response-preview 80)
                body-lines
                (if has-thinking ", thinking" "")
                (cond ((= tool-count 0) "")
                      ((= tool-count 1) ", 1 tool")
                      (t (format ", %d tools" tool-count)))))
       ((or has-thinking (> tool-count 0))
        (format "Assistant — [%s%s%s]"
                (if has-thinking "thinking" "")
                (if (and has-thinking (> tool-count 0)) ", " "")
                (cond ((= tool-count 0) "")
                      ((= tool-count 1) "1 tool")
                      (t (format "%d tools" tool-count)))))
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
      ;; No more turns; go to input area
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

(defun mevedel-view-rerender (&optional buffer)
  "Schedule a debounced re-render of BUFFER (default: current buffer).
Public re-render entry point used by the background handle patch
path, plan-summary disk-write reconstruction, and any caller that
mutates render-data and wants the visible card refreshed without
waiting for the next stream tick.

Bursts collapse into one rerender via
`mevedel-view-rerender-debounce'.  When the view is mid-stream
(a parent FSM is streaming), the debounce window also lets the
incremental render path pick up the latest render-data on its
own tick before the full-rerender fires.

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

(defun mevedel-view--full-rerender ()
  "Re-render the entire view buffer from the data buffer.
Wipes all rendered content and re-renders from scratch.  Used after
compaction, session resume, or manual refresh.

Re-anchors `mevedel-view--in-flight-turn-start' to the rerendered
position of the last (in-flight) turn when a turn was in flight at
the time of the rerender; otherwise the wipe collapses the marker to
`point-min' and the next incremental render erases the freshly
rerendered history (and its `You' header along with it).

Wraps the re-render in `mevedel-view--preserving-window-state' so the
caret + scroll position survive a rerender triggered mid-stream
(e.g. by the post-permission accept callback's view rerender)."
  (unless mevedel--data-buffer
    (error "No data buffer"))
  (mevedel-view--preserving-window-state
   (let ((data-buf mevedel--data-buffer)
         (inhibit-read-only t)
         (data-turn-start-pos
          (and (markerp mevedel-view--data-turn-start)
               (marker-position mevedel-view--data-turn-start)))
         (in-flight-was (and (markerp mevedel-view--in-flight-turn-start)
                             (marker-position mevedel-view--in-flight-turn-start)))
         (_cleanup-stale-pending
          (unless mevedel-view--pending-tool-calls
            (mevedel-view--delete-pending-tool-live-lines)))
         (preserved-live-tail
          (when-let* (((not mevedel-view--agent-transcript-p))
                      ((markerp mevedel-view--in-flight-turn-start))
                      (tail-start
                       (marker-position mevedel-view--in-flight-turn-start))
                      ((markerp mevedel-view--status-marker))
                      (tail-end (marker-position mevedel-view--status-marker))
                      ((< tail-start tail-end)))
            (buffer-substring tail-start tail-end))))
    (mevedel-view--debug-log
     'full-rerender-begin
     :in-flight-was in-flight-was
     :preserved-live-tail-len (and preserved-live-tail
                                   (length preserved-live-tail))
     :state (mevedel-view--debug-state data-buf))
    (mevedel-view--discard-spinner-overlay)
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
      ;; Wipe display area (everything above input marker)
      (mevedel-view--debug-log
       'full-rerender-delete-display
       :region (mevedel-view--debug-region
                (point-min)
                (marker-position mevedel-view--input-marker))
       :state (mevedel-view--debug-state data-buf))
      (delete-region (point-min) mevedel-view--input-marker)
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
      ;; Skip compacted region at the start.  Legacy in-buffer
      ;; compaction leaves ignored/shadowed old content followed by a
      ;; summary block; segment rotation starts directly with a summary
      ;; block followed by live tail content.
      (let ((scan-start (mevedel-view--skip-leading-properties-drawer
                         (point-min))))
        (when (eq (get-text-property scan-start 'face) 'shadow)
          ;; Skip past shadow region (old conversation)
          (setq scan-start (or (next-single-property-change
                                scan-start 'face nil (point-max))
                               (point-max)))
          ;; Skip past the compaction separator + summary block by
          ;; searching for the end marker.
          (save-excursion
            (goto-char scan-start)
            (when (re-search-forward
                   "^#\\+end_summary\n\\|^```\n" nil t)
              (setq scan-start (point))))
          ;; Insert a compaction indicator in the view buffer
          (with-current-buffer (buffer-local-value 'mevedel--view-buffer data-buf)
            (save-excursion
              (goto-char mevedel-view--input-marker)
              (set-marker-insertion-type mevedel-view--input-marker t)
              (unwind-protect
                  (insert (propertize "--- conversation compacted ---\n"
                                      'read-only t
                                      'keymap mevedel-view--display-map
                                      'front-sticky '(read-only keymap)
                                      'rear-nonsticky '(read-only keymap)
                                      'font-lock-face 'mevedel-view-separator))
                (set-marker-insertion-type mevedel-view--input-marker nil)))))
        (setq scan-start (mevedel-view--skip-leading-summary-block scan-start))
        ;; Narrow so that `extract-segments' boundary expansion
        ;; (`previous-single-property-change' bounded by `point-min')
        ;; can't walk back into the leading drawer / compacted region.
        (save-restriction
          (narrow-to-region scan-start (point-max))
          (let* ((segments (mevedel-view--extract-segments
                            (point-min) (point-max)))
                 (turns (mevedel-view--group-into-turns segments data-buf))
                 (view-buf (buffer-local-value 'mevedel--view-buffer data-buf))
                 (last-assistant-turn-start nil)
                 (last-current-assistant-turn-start nil)
                 (last-turn-role nil))
            (with-current-buffer view-buf
              (dolist (turn turns)
                (setq last-turn-role (plist-get turn :role))
                (when (eq (plist-get turn :role) 'assistant)
                  (let ((view-turn-start
                         (marker-position mevedel-view--input-marker)))
                    (setq last-assistant-turn-start view-turn-start)
                    (when (and data-turn-start-pos
                               (plist-get turn :end)
                               (> (plist-get turn :end)
                                  data-turn-start-pos))
                      (setq last-current-assistant-turn-start
                            view-turn-start))))
                (mevedel-view--render-turn turn data-buf)))
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
                  (set-marker mevedel-view--in-flight-turn-start
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
                  (set-marker mevedel-view--in-flight-turn-start
                              last-assistant-turn-start))
                 (preserved-live-tail
                  (goto-char mevedel-view--input-marker)
                  (mevedel-view--with-render-boundaries-advancing
                    (let ((tail-start (point)))
                      (insert preserved-live-tail)
                      (mevedel-view--restore-spinner-overlay-in-region
                       tail-start (point))
                      (mevedel-view--debug-log
                       'full-rerender-reanchor
                       :decision 'preserved-live-tail
                       :last-turn-role last-turn-role
                       :tail-start tail-start
                       :state (mevedel-view--debug-state data-buf))
                      (set-marker mevedel-view--in-flight-turn-start
                                  tail-start))))
                 (t
                  (mevedel-view--debug-log
                   'full-rerender-reanchor
                   :decision 'input-marker
                   :last-turn-role last-turn-role
                   :state (mevedel-view--debug-state data-buf))
                  (set-marker mevedel-view--in-flight-turn-start
                              mevedel-view--input-marker)))))
            (with-current-buffer view-buf
              (unless mevedel-view--agent-transcript-p
                (mevedel-view--render-agent-status)
                (mevedel-view--interaction-rebuild))
              (mevedel-view--debug-log
               'full-rerender-after-render
               :last-assistant-turn-start last-assistant-turn-start
               :last-current-assistant-turn-start
               last-current-assistant-turn-start
               :state (mevedel-view--debug-state data-buf))))))))))


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

(defun mevedel-view--insert-user-message (text &optional kind)
  "Render TEXT as a user message in the display region.
Inserts above `mevedel-view--input-marker' with read-only protection.
KIND may be `directive' to fontify directive-specific display text.

Sets `mevedel-view--user-pre-rendered' so the post-response render
path knows to skip the user turn it would otherwise extract for this
same exchange -- see `mevedel-view--render-response'."
  (mevedel-view--ensure-interactive-chat-view)
  (save-excursion
    (goto-char mevedel-view--input-marker)
    (set-marker-insertion-type mevedel-view--input-marker t)
    (unwind-protect
	        (let ((inhibit-read-only t)
	              (start (point)))
	          (insert (propertize "You\n" 'font-lock-face 'mevedel-view-user-header))
	          (insert (if (eq kind 'directive)
                              (mevedel-view--fontify-directive-display-text text)
                            text))
          (unless (eq (char-before) ?\n)
            (insert "\n"))
          (insert (propertize "\n" 'font-lock-face 'mevedel-view-separator))
          (add-text-properties start (point)
                               `(read-only t
                                 keymap ,mevedel-view--display-map
                                 front-sticky (read-only keymap)
                                 rear-nonsticky (read-only keymap)
                                 mevedel-view-type user))
          (setq mevedel-view--user-pre-rendered t))
      (set-marker-insertion-type mevedel-view--input-marker nil))))

(defun mevedel-view--begin-external-turn (display-text data-turn-start
                                                       &optional kind)
  "Begin a view turn initiated outside the editable input.

DISPLAY-TEXT is shown as the user-side turn in the view.
DATA-TURN-START is the data-buffer marker where the assistant
response for this turn begins."
  (mevedel-view--ensure-interactive-chat-view)
  (mevedel-view--insert-user-message display-text kind)
  (when (eq kind 'directive)
    (when-let* ((drawer (mevedel-view--external-prompt-drawer
                         data-turn-start)))
      (save-excursion
        (goto-char mevedel-view--input-marker)
        (set-marker-insertion-type mevedel-view--input-marker t)
        (unwind-protect
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
                 rear-nonsticky (read-only keymap))))
          (set-marker-insertion-type mevedel-view--input-marker nil)))))
  (setq mevedel-view--in-flight-turn-start
        (copy-marker mevedel-view--input-marker nil))
  (setq mevedel-view--data-turn-start data-turn-start)
  (mevedel-view--start-spinner))

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

(defun mevedel-view--input-start ()
  "Return the buffer position where the user's editable input begins.
This is the position immediately after the read-only `> ' prompt that
follows `mevedel-view--input-marker'.  Degrades to the marker position
when the prompt has not (yet) been installed, so a buffer created
before this feature still works."
  (save-excursion
    (goto-char mevedel-view--input-marker)
    (while (get-text-property (point) 'mevedel-view-prompt)
      (forward-char 1))
    (point)))

(defun mevedel-view--input-text ()
  "Return the user's input text from the input region, trimmed."
  (let ((text (buffer-substring-no-properties
               (mevedel-view--input-start) (point-max))))
    (string-trim text)))

(defun mevedel-view--clear-input ()
  "Clear the user's input region, leaving the prompt in place."
  (mevedel-view--ensure-interactive-chat-view)
  (delete-region (mevedel-view--input-start) (point-max)))

(defun mevedel-view-slash-capf ()
  "Completion-at-point for `/command' prefixes in the view input area.
Offers local slash commands and session skills when point follows
a `/' at the very start of the user's input (immediately after the
read-only `> ' prompt)."
  (when (and mevedel--data-buffer
             (buffer-live-p mevedel--data-buffer)
             (>= (point) (mevedel-view--input-start))
             (save-excursion
               (skip-chars-backward "A-Za-z0-9_-")
               (and (eq (char-before) ?/)
                    (save-excursion
                      (backward-char)
                      (= (point) (mevedel-view--input-start))))))
    (let* ((end (point))
           (start (save-excursion
                    (skip-chars-backward "A-Za-z0-9_-")
                    (point)))
           (session (buffer-local-value 'mevedel--session
                                        mevedel--data-buffer))
           (local-commands mevedel-slash-commands))
      (list start end
            (mevedel-skills--slash-completion-table
             mevedel--data-buffer session local-commands)
            :exclusive 'no
            :annotation-function
            (lambda (name)
              (mevedel-skills--slash-annotation
               name mevedel--data-buffer session local-commands))))))

(defun mevedel-view--start-fork-skill-turn (input display-text)
  "Render and record a slash fork INPUT without calling `gptel-send'.

DISPLAY-TEXT is shown in the view for the user turn.  INPUT is written
to the data buffer as the authoritative user prompt.  The data-turn
marker is anchored after that prompt so the eventual fork result can be
rendered by the normal post-response hook."
  (mevedel-view--insert-user-message display-text)
  (setq mevedel-view--in-flight-turn-start
        (copy-marker mevedel-view--input-marker nil))
  (mevedel-view--clear-input)
  (mevedel-view--start-spinner)
  (with-current-buffer mevedel--data-buffer
    (when mevedel--session
      (mevedel-request-begin mevedel--session
                             (and (boundp 'mevedel--current-directive-uuid)
                                  mevedel--current-directive-uuid)))
    (goto-char (point-max))
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
    (let ((data-turn-start (copy-marker (point) nil)))
      (with-current-buffer mevedel--view-buffer
        (setq mevedel-view--data-turn-start data-turn-start)))))

(defun mevedel-view-send ()
  "Send the current input to the LLM via the data buffer.
Extracts text from the input region, renders it in the display area,
forwards it to the data buffer, and calls `gptel-send'.
When the input starts with a `/command', dispatches it as a slash
command or skill instead of forwarding to the LLM.

If the data buffer is in rewind preview state
(`mevedel-session--fork-pending' is set), materialize the fork just
before the send actually reaches the LLM so empty input, unknown
slash commands, and local-only slash commands do not spuriously
create a fork."
  (interactive)
  (mevedel-view--ensure-interactive-chat-view)
  (unless mevedel--data-buffer
    (user-error "No data buffer associated with this view"))
  (unless (buffer-live-p mevedel--data-buffer)
    (user-error "Data buffer has been killed"))
  (when (buffer-local-value 'mevedel--current-request mevedel--data-buffer)
    (user-error "A request is already active -- wait or abort first"))
  (when (buffer-local-value 'mevedel--compaction-in-flight mevedel--data-buffer)
    (message "mevedel: compacting, please wait...")
    (user-error "Compaction in progress"))
  (when (buffer-local-value 'mevedel-session--read-only-mode
                            mevedel--data-buffer)
    (user-error "Session is open read-only (another host holds the lock)"))
  (let ((input (mevedel-view--input-text)))
    (when (string-empty-p input)
      (user-error "Nothing to send"))
    ;; Check for slash commands before forwarding.
    (let ((parsed (mevedel-skills--parse-slash-line input)))
      (if (not parsed)
          ;; Normal message -- fork if pending, then forward.
          (progn
            (mevedel-view-history-add input)
            (mevedel-view--fork-if-pending)
            (mevedel-view--forward-input input))
        ;; Slash command detected.
        (let* ((name (nth 0 parsed))
               (args (nth 1 parsed))
               (local (assoc name mevedel-slash-commands))
               (skill (with-current-buffer mevedel--data-buffer
                        (and (bound-and-true-p mevedel--session)
                             (mevedel-session-get-skill
                              mevedel--session name)))))
          (cond
           (local
            ;; Local slash commands don't send a turn -- no fork.
            (mevedel-view-history-add input)
            (mevedel-view--clear-input)
            (with-current-buffer mevedel--data-buffer
              (funcall (cdr local) args)))
           (skill
            (mevedel-view-history-add input)
            (mevedel-view--fork-if-pending)
            (let ((fork-p (eq (mevedel-skill-context skill) 'fork))
                  (view-buffer (current-buffer))
                  (data-buffer mevedel--data-buffer)
                  (display-text (concat "/" name
                                        (when args (concat " " args)))))
              (when fork-p
                (mevedel-view--start-fork-skill-turn input display-text))
              (with-current-buffer mevedel--data-buffer
                (mevedel-skills-invoke
                 skill args
                 (lambda (outcome)
                   (when (and (buffer-live-p view-buffer)
                              (buffer-live-p data-buffer))
                     (pcase (plist-get outcome :status)
                       ('ok
                        (pcase (plist-get outcome :kind)
                          ('inline
                           (with-current-buffer view-buffer
                             (mevedel-view--forward-input
                              (or (plist-get outcome :body)
                                  (format "Skill '%s' produced no body."
                                          name))
                              display-text)))
                          ('fork
                           (with-current-buffer data-buffer
                             (mevedel-skills--insert-fork-result outcome)))
                          (_
                           (message "Skill '%s' returned unsupported outcome: %S"
                                    name outcome))))
	                       (_
	                        (with-current-buffer view-buffer
	                          (when fork-p
	                            (mevedel-view--stop-spinner))
	                          (message "Skill '%s' failed: %s"
	                                   name
	                                   (or (plist-get outcome :message)
	                                       "unknown error")))
	                        (when fork-p
	                          (with-current-buffer data-buffer
	                            (when (bound-and-true-p
	                                   mevedel--current-request)
	                              (mevedel-request-end))
	                            (gptel--update-status
	                             " Ready" 'success)))))))
	                 :trigger 'user-slash))))
           (t
            (message "Unknown slash command: /%s" name)))))))

  ;; Ensure point ends up in the input area.
  (goto-char (point-max)))

(defun mevedel-view--fork-if-pending ()
  "Materialize the fork if the data buffer is in rewind preview state.
No-op otherwise.  Shared safety net for any path that actually sends
a turn to the LLM."
  (when (buffer-local-value 'mevedel-session--fork-pending mevedel--data-buffer)
    (require 'mevedel-session-persistence)
    (mevedel-view-reset-agent-ephemeral-state)
    (mevedel-session-persistence-fork-now mevedel--data-buffer)))

(defun mevedel-view--forward-input (input &optional display-text)
  "Render INPUT in the display area, forward to the data buffer, and send.
Helper for `mevedel-view-send'.  When DISPLAY-TEXT is non-nil, show
that in the view instead of INPUT (e.g., compact skill invocation).

Anchors the incremental-render markers so progress hooks can redraw
the in-flight assistant turn as tool calls complete:
`mevedel-view--in-flight-turn-start' points into the view just above
the input area (where the assistant turn will be rendered);
`mevedel-view--data-turn-start' points into the data buffer just
after the forwarded prompt, where the LLM's response will begin."
  (mevedel-view--ensure-interactive-chat-view)
  (when (buffer-local-value 'mevedel--compaction-in-flight mevedel--data-buffer)
    (message "mevedel: compacting, please wait...")
    (user-error "Compaction in progress"))
  ;; Render the user's message in the view
  (mevedel-view--insert-user-message (or display-text input))
  ;; Anchor the view-side marker for incremental re-render.
  (setq mevedel-view--in-flight-turn-start
        (copy-marker mevedel-view--input-marker nil))
  ;; Clear input area
  (mevedel-view--clear-input)
  ;; Start spinner
  (mevedel-view--start-spinner)
  ;; Forward to data buffer and send
  (with-current-buffer mevedel--data-buffer
    (goto-char (point-max))
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
    (insert input "\n")
    ;; Anchor the data-side marker after the forwarded prompt so
    ;; incremental renders extract only the in-flight assistant
    ;; segments from here forward.  Pushed onto the view buffer's
    ;; buffer-local so it is readable from `--render-incremental'
    ;; without switching buffers.
    (let ((data-turn-start (copy-marker (point) nil)))
      (with-current-buffer mevedel--view-buffer
        (setq mevedel-view--data-turn-start data-turn-start)))
    (gptel-send)))

(defun mevedel-view-abort ()
  "Abort the active request from the view buffer."
  (interactive)
  (mevedel-view--ensure-interactive-chat-view)
  (mevedel-view--stop-spinner)
  (when-let* ((data-buf mevedel--data-buffer)
              (_ (buffer-live-p data-buf)))
    ;; Delegate to mevedel-abort which handles the full teardown
    (with-current-buffer data-buf
      (when (fboundp 'mevedel-abort)
        (funcall #'mevedel-abort)))))


;;
;;; Sub-agent transcript open command

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

Resolves the parent chat (data) buffer from the current view
buffer, reads its `mevedel--session', and looks up AGENT-ID in the
session's `agent-transcripts' alist.

AGENT-ID may be the canonical id (`type--32hex') or the display label
(`type--8hex') shown in rendered view text.  Returns nil if any link is
missing."
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
  (or (and (fboundp 'mevedel-tool-ui--display-label-from-canonical)
           (mevedel-tool-ui--display-label-from-canonical agent-id))
      agent-id))

(defun mevedel-view--resolve-agent-transcript (agent-id)
  "Return validated transcript info for terminal AGENT-ID.
Signals `user-error' when the transcript is missing, non-terminal, or
fails path validation."
  (let* ((data-buf (and (boundp 'mevedel--data-buffer)
                        mevedel--data-buffer))
         (session (and data-buf (buffer-live-p data-buf)
                       (buffer-local-value 'mevedel--session data-buf)))
         (pair (and session (mevedel-view--lookup-transcript-pair agent-id)))
         (canonical-id (or (car pair) agent-id))
         (entry (cdr pair))
         (inv (mevedel-view--agent-invocation agent-id))
         (status (mevedel-view--agent-effective-status inv entry))
         (save-path (and session (mevedel-session-save-path session)))
         (rel-path (and entry (plist-get entry :path))))
    (unless entry
      (user-error "No transcript entry for agent-id: %s" agent-id))
    (unless (mevedel-view--agent-terminal-status-p status)
      (user-error "Agent %s is still running; transcript available when complete"
                  (mevedel-view--display-label-for-agent agent-id)))
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
              entry))))

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
                      (progn
                        (set-window-buffer win view-buf)
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
            (select-window win)))))))

(defun mevedel-view--ensure-agent-transcript-view (agent-id info parent-view)
  "Return a rendered transcript inspection view for AGENT-ID and INFO."
  (let* ((agent-data (mevedel-session-persistence--find-file-noselect
                      (plist-get info :absolute-path)))
         (display-label (mevedel-view--display-label-for-agent agent-id))
         (view-name (format "*mevedel-agent:%s*" display-label))
         (agent-view
          (mevedel-view--ensure
           agent-data view-name
           (list :agent-transcript-p t
                 :agent-id agent-id
                 :parent-view parent-view
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
      (mevedel-view--restore-gptel-bounds)
      (unless buffer-read-only
        (read-only-mode +1)))
    (with-current-buffer agent-view
      (mevedel-view--full-rerender))
    agent-view))

(defun mevedel-view--restore-gptel-bounds ()
  "Restore saved `gptel' text properties from the current org buffer."
  (when (require 'gptel nil t)
    (when-let* ((bounds (org-entry-get (point-min) "GPTEL_BOUNDS")))
      (condition-case err
          (gptel--restore-props (read bounds))
        (error
         (display-warning
          'mevedel
          (format "Could not restore transcript GPTEL_BOUNDS: %s"
                  (error-message-string err))))))))

(defun mevedel-view-agent-handle-activate (&optional agent-id)
  "Activate the rendered agent handle at point or AGENT-ID.
Running handles toggle their ephemeral activity body.  Terminal
handles open the rendered transcript inspection view."
  (interactive)
  (let* ((id (or agent-id
                 (get-text-property (point) 'mevedel-view-agent-id)))
         (entry (and id (mevedel-view--lookup-transcript-entry id)))
         (inv (and id (mevedel-view--agent-invocation id)))
         (status (mevedel-view--agent-effective-status inv entry)))
    (unless id
      (user-error "No agent handle at point"))
    (cond
     ((mevedel-view--agent-terminal-status-p status)
      (mevedel-view-open-agent-transcript id))
     ((or (eq status 'running) inv)
      (if (<= (max 0 mevedel-view-agent-activity-max) 0)
          (message "Agent activity display is disabled")
        (let* ((state (mevedel-view--agent-state id))
               (expanded (not (plist-get state :expanded))))
          (mevedel-view--set-agent-expanded id expanded)
          (mevedel-view--full-rerender))))
     (t
      (message "Transcript unavailable for %s"
               (mevedel-view--display-label-for-agent id))))))

(defun mevedel-view--open-agent-transcript-or-message
    (agent-id &optional _live-click-p calls)
  "Open AGENT-ID's transcript or explain why it is not openable.

This is the click/RET path for attribution fragments.  Terminal
transcripts open normally.  Running transcript files are not opened
through the normal UI; the click target reports that the transcript
will be available after completion."
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
     ((eq status 'running)
      (message
       "Agent %s still running%s. Transcript available when complete."
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
`mevedel-session-persistence--validate-transcript-path' before
opening.  Surfaces a `user-error' when the entry is missing, the
path fails validation, or the file is absent on disk."
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
  (let* ((entry (mevedel-view--lookup-transcript-entry agent-id))
         (inv (mevedel-view--agent-invocation agent-id))
         (status (mevedel-view--agent-effective-status inv entry)))
    (if (and status
             (not (mevedel-view--agent-terminal-status-p status)))
        (if (and inv (> (max 0 mevedel-view-agent-activity-max) 0))
            (mevedel-view-agent-handle-activate agent-id)
          (message "Agent transcript is available after completion"))
      (let* ((parent-view (current-buffer))
             (info (mevedel-view--resolve-agent-transcript agent-id))
             (canonical-id (plist-get info :agent-id))
             (agent-view (mevedel-view--ensure-agent-transcript-view
                          canonical-id info parent-view)))
        (mevedel-view--display-agent-transcript-view agent-view)))))

(defun mevedel-view--decorate-mailbox-block
    (open-regex close-tag start end &optional kind)
  "Replace OPEN-REGEX/CLOSE-TAG-bracketed regions with mailbox cards.
Shared engine for `<agent-message>' and `<agent-result>'
rendering.  OPEN-REGEX must capture the agent-id in match group
1.  Body between the matched open and close tags is preserved
verbatim; if its line count exceeds
`mevedel-view-mailbox-collapse-line-threshold' the body is marked
invisible (with the `mailbox-delivery' vtype tag for downstream
  TAB-toggle wiring) and the header gets a `[N lines collapsed]'
  hint.  Searches the region START..END."
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
                    (when (re-search-forward
                           (regexp-quote close-tag)
                           (marker-position end-marker) t)
                      (let* ((body-end (match-beginning 0))
                             (close-end (match-end 0))
                             (body-line-count
                              (count-lines body-start body-end))
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
                        (when long-body
                          (let* ((hint
                                  (propertize
                                   (format " [%d lines collapsed]"
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
                           mevedel-view-agent-handle-p nil
                           mevedel-view-agent-status nil))
                        (remove-text-properties
                         body-start (point)
                         '(mevedel-view-agent-id nil))
                        (add-text-properties
                         card-start (point)
                         (list 'mevedel-view-type 'mailbox-delivery
                               'mevedel-view-mailbox-card card-id
                               'mevedel-view-collapsed long-body)))))))))
        (set-marker end-marker nil)))))

(defun mevedel-view--decorate-agent-result-blocks (start end)
  "Render `<agent-result agent-id=...>...</agent-result>' as mailbox cards.
Delegates to `mevedel-view--decorate-mailbox-block' so
`<agent-message>' and `<agent-result>' render uniformly: same
header, same collapse threshold, same vtype tag for downstream
TAB toggling."
  (mevedel-view--decorate-mailbox-block
   "<agent-result\\s-+[^>]*agent-id=\"\\([^\"]+\\)\"[^>]*>"
   "</agent-result>"
   start end
   'agent-result))

(defun mevedel-view--agent-handle-ids-in-buffer ()
  "Return agent ids whose handles are present in the current view buffer."
  (let* ((status-start
          (and (overlayp mevedel-view--agent-status-overlay)
               (eq (overlay-buffer mevedel-view--agent-status-overlay)
                   (current-buffer))
               (overlay-start mevedel-view--agent-status-overlay)))
         (status-end
          (and status-start
               (overlay-end mevedel-view--agent-status-overlay)))
         ids)
    (save-excursion
      (goto-char (point-min))
      (while (< (point) (point-max))
        (let ((id (and (not (and status-start
                                  status-end
                                  (>= (point) status-start)
                                  (< (point) status-end)))
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
        (dolist (pair mevedel-tools--agents-fsm)
          (let* ((agent-id (car pair))
                 (fsm (cdr pair))
                 (inv (mevedel-tools--agent-invocation-at fsm))
                 (entry (cdr (assoc agent-id entries)))
                 (status (mevedel-view--agent-effective-status inv entry)))
            (when inv
              (push agent-id live-ids)
              (unless (member agent-id handle-ids)
                (cond
                 ((eq status 'running)
                  (let ((blocked
                         (and session
                              (or (cl-some
                                   (lambda (entry)
                                     (equal (plist-get entry :origin) agent-id))
                                   (mevedel-session-permission-queue session))
                                  (cl-some
                                   (lambda (entry)
                                     (equal (plist-get entry :origin) agent-id))
                                   (mevedel-session-plan-queue session))))))
                    (push (list :agent-id agent-id
                                :status (if blocked 'blocked 'running)
                                :agent-type
                                (mevedel-view--agent-row-type
                                 agent-id inv entry)
                                :description
                                (mevedel-view--agent-row-description agent-id inv entry)
                                :calls (mevedel-agent-invocation-call-count inv)
                                :elapsed (mevedel-view--agent-row-elapsed inv entry)
                                :reason
                                (or (mevedel-agent-invocation-terminal-reason inv)
                                    (plist-get entry :reason)))
                          rows)))
                 ((mevedel-view--agent-terminal-status-p status)
                  (push (list :agent-id agent-id
                              :status status
                              :agent-type
                              (mevedel-view--agent-row-type
                               agent-id inv entry)
                              :description
                              (mevedel-view--agent-row-description agent-id inv entry)
                              :calls (or (and inv
                                              (mevedel-agent-invocation-call-count inv))
                                         (plist-get entry :calls))
                              :elapsed (mevedel-view--agent-row-elapsed inv entry)
                              :reason
                              (or (and inv
                                       (mevedel-agent-invocation-terminal-reason inv))
                                  (plist-get entry :reason)))
                        rows)))))))))
    (dolist (pair entries)
      (let* ((agent-id (car pair))
             (entry (cdr pair))
             (inv (mevedel-view--agent-invocation agent-id))
             (status (mevedel-view--agent-effective-status inv entry)))
        (when (and (eq status 'running)
                   (not (member agent-id live-ids))
                   (mevedel-view--agent-status-blocked-p agent-id))
          (push agent-id live-ids)
          (push (list :agent-id agent-id
                      :status 'blocked
                      :agent-type
                      (mevedel-view--agent-row-type agent-id inv entry)
                      :description
                      (mevedel-view--agent-row-description agent-id inv entry)
                      :calls (plist-get entry :calls)
                      :elapsed (plist-get entry :elapsed)
                      :reason (plist-get entry :reason))
                rows))))
    (sort rows
          (lambda (a b)
            (< (or (cl-position (plist-get a :status)
                                '(blocked running error aborted incomplete completed))
                   99)
               (or (cl-position (plist-get b :status)
                                '(blocked running error aborted incomplete completed))
                   99))))))

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

(defun mevedel-view--agent-status-row-string (row)
  "Return a propertized aggregate status line for ROW."
  (let* ((agent-id (plist-get row :agent-id))
         (display (mevedel-view--display-label-for-agent agent-id))
         (description (plist-get row :description))
         (status (plist-get row :status))
         (badge (mevedel-tool-ui--handle-badge
                 (list :status (if (eq status 'blocked) 'running status)
                       :blocked-reason (and (eq status 'blocked) "interaction")
                       :calls (plist-get row :calls)
                       :elapsed (plist-get row :elapsed)
                       :reason (plist-get row :reason))))
         (text (format "  %s%s%s\n"
                       display
                       (if (and description (not (string-empty-p description)))
                           (format " -- %s" description)
                         "")
                       (if (string-empty-p badge) "" (concat "  " badge))))
         (map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'mevedel-view-agent-status-activate-row)
    (define-key map [mouse-1] #'mevedel-view-agent-status-activate-row)
    (add-text-properties
     0 (length text)
     `(keymap ,map
       mouse-face highlight
       follow-link t
       mevedel-view-agent-id ,agent-id
       help-echo "Reveal agent handle")
     text)
    text))

(defun mevedel-view--agent-status-buttonize-toggle (header suffix)
  "Return HEADER with SUFFIX made into the aggregate-status toggle.
Only the visible `[+]' / `[-]' suffix is made clickable so the
status line behaves like other compact view-buffer affordances."
  (let ((start (string-match (regexp-quote suffix) header))
        (map (make-sparse-keymap)))
    (when start
      (define-key map (kbd "RET") #'mevedel-view-agent-status-toggle)
      (define-key map [mouse-1] #'mevedel-view-agent-status-toggle)
      (define-key map [mouse-2] #'mevedel-view-agent-status-toggle)
      (add-text-properties
       start (+ start (length suffix))
       `(face link
         keymap ,map
         mouse-face highlight
         follow-link t
         help-echo "Expand or collapse agent status")
       header))
    header))

(defun mevedel-view--agent-status-string (rows)
  "Return the aggregate status overlay string for ROWS."
  (let* ((summary (mevedel-view--agent-status-summary rows))
         (suffix (if mevedel-view--agent-status-expanded-p "[-]" "[+]"))
         (header (mevedel-view--zone-separator
                  (format "%d %s: %s%s"
                          (length rows)
                          (if (= 1 (length rows)) "agent" "agents")
                          summary
                          (concat " " suffix)))))
    (setq header (mevedel-view--agent-status-buttonize-toggle
                  header suffix))
    (concat
     (if mevedel-view--agent-status-expanded-p
         (concat header
                 (mapconcat #'mevedel-view--agent-status-row-string rows ""))
       header)
     "\n")))

(defun mevedel-view--agent-status-row-rendering (row)
  "Return an Agent-handle rendering plist for aggregate status ROW."
  (let* ((agent-id (plist-get row :agent-id))
         (status (plist-get row :status))
         (render-status (if (eq status 'blocked) 'running status))
         (agent-type (or (plist-get row :agent-type)
                         (mevedel-view--agent-row-type agent-id nil nil)))
         (description (or (plist-get row :description) ""))
         (calls (plist-get row :calls))
         (elapsed (plist-get row :elapsed))
         (reason (plist-get row :reason))
         (blocked-reason (and (eq status 'blocked) "interaction"))
         (render-data (append
                       (list :kind 'agent-transcript
                             :agent-id agent-id
                             :status render-status
                             :calls (or calls 0))
                       (when elapsed (list :elapsed elapsed))
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

(defun mevedel-view--agent-status-handles-string (rows)
  "Return Agent-handle text for ROWS, preserving live expansion state."
  (let ((data-buffer
         (and (boundp 'mevedel--data-buffer) mevedel--data-buffer))
        (session
         (and (boundp 'mevedel--session) mevedel--session))
        (expanded-state mevedel-view--agent-activity-expanded))
    (with-temp-buffer
      (let ((mevedel--data-buffer data-buffer)
            (mevedel--session session)
            (mevedel-view--agent-activity-expanded expanded-state)
            (mevedel-view--input-marker (copy-marker (point-max) t))
            (mevedel-view--status-marker (copy-marker (point-max) t))
            (mevedel-view--interaction-marker (copy-marker (point-max) t)))
        (dolist (row rows)
          (when-let* ((rendering
                       (mevedel-view--agent-status-row-rendering row)))
            (mevedel-view--insert-rendered-tool rendering nil)))
        (buffer-string)))))

(defun mevedel-view--delete-agent-status-region ()
  "Delete materialized aggregate agent-status text, if present."
  (when (overlayp mevedel-view--agent-status-overlay)
    (let ((start (overlay-start mevedel-view--agent-status-overlay))
          (end (overlay-end mevedel-view--agent-status-overlay))
          (buf (overlay-buffer mevedel-view--agent-status-overlay)))
      (delete-overlay mevedel-view--agent-status-overlay)
      (setq mevedel-view--agent-status-overlay nil)
      (when (and start end buf (buffer-live-p buf) (< start end))
        (with-current-buffer buf
          (let ((inhibit-read-only t))
            (delete-region start end)))))))

(defun mevedel-view--render-agent-status ()
  "Render or remove the aggregate live agent status text."
  (let ((rows (mevedel-view--agent-status-collect)))
    (mevedel-view--delete-agent-status-region)
    (when rows
      (let ((anchor (and (markerp mevedel-view--status-marker)
                         (marker-position mevedel-view--status-marker)))
            (status-type (and (markerp mevedel-view--status-marker)
                              (marker-insertion-type
                               mevedel-view--status-marker)))
            (interaction-type (and (markerp mevedel-view--interaction-marker)
                                   (marker-insertion-type
                                    mevedel-view--interaction-marker)))
            (input-type (and (markerp mevedel-view--input-marker)
                             (marker-insertion-type
                              mevedel-view--input-marker))))
        (when anchor
          (save-excursion
            (let ((inhibit-read-only t)
                  (text (mevedel-view--agent-status-handles-string rows)))
              (goto-char anchor)
              (when (markerp mevedel-view--status-marker)
                (set-marker-insertion-type mevedel-view--status-marker nil))
              (when (markerp mevedel-view--interaction-marker)
                (set-marker-insertion-type mevedel-view--interaction-marker t))
              (when (markerp mevedel-view--input-marker)
                (set-marker-insertion-type mevedel-view--input-marker t))
              (unwind-protect
                  (let ((start (point)))
                    (insert text)
                    (remove-text-properties
                     start (point)
                     '(mevedel-view-source nil))
                    (mevedel-view--add-display-region-properties
                     start (point) 'agent-handle)
                    (setq mevedel-view--agent-status-overlay
                          (make-overlay start (point) (current-buffer)
                                        nil t))
                    (overlay-put mevedel-view--agent-status-overlay
                                 'mevedel-view-agent-status t)
                    (overlay-put mevedel-view--agent-status-overlay
                                 'evaporate t))
                (when (markerp mevedel-view--status-marker)
                  (set-marker-insertion-type mevedel-view--status-marker
                                             status-type))
                (when (markerp mevedel-view--interaction-marker)
                  (set-marker-insertion-type mevedel-view--interaction-marker
                                             interaction-type))
                (when (markerp mevedel-view--input-marker)
                  (set-marker-insertion-type mevedel-view--input-marker
                                             input-type))))))))))

(defun mevedel-view-agent-status-toggle ()
  "Toggle the aggregate live agent status rows."
  (interactive)
  (setq mevedel-view--agent-status-expanded-p
        (not mevedel-view--agent-status-expanded-p))
  (mevedel-view--render-agent-status))

(defun mevedel-view--agent-locate-handle (agent-id)
  "Move point to AGENT-ID's rendered handle and return non-nil on success."
  (cl-labels
      ((find-visible ()
         (let ((found nil))
           (goto-char (point-min))
           (while (and (not found) (< (point) (point-max)))
             (when (and (get-text-property (point) 'mevedel-view-agent-handle-p)
                        (equal (get-text-property
                                (point) 'mevedel-view-agent-id)
                               agent-id))
               (setq found t))
             (unless found
               (goto-char (or (next-single-property-change
                               (point) 'mevedel-view-agent-handle-p
                               nil (point-max))
                              (point-max)))))
           found))
       (stash-has-agent-p (stash)
         (and (stringp stash)
              (let ((pos 0)
                    (found nil)
                    next)
                (while (and (not found)
                            (< pos (length stash)))
                  (when (equal (get-text-property
                                pos 'mevedel-view-agent-id stash)
                               agent-id)
                    (setq found t))
                  (setq next (next-single-property-change
                              pos 'mevedel-view-agent-id stash
                              (length stash)))
                  (setq pos (or next (length stash))))
                found)))
       (expand-collapsed-turn ()
         (let ((pos (point-min))
               (expanded nil))
           (while (and (not expanded) (< pos (point-max)))
             (when (and (eq (get-text-property pos 'mevedel-view-type)
                            'turn-summary)
                        (get-text-property pos 'mevedel-view-collapsed)
                        (stash-has-agent-p
                         (get-text-property pos 'mevedel-view-stash)))
               (goto-char pos)
               (condition-case err
                   (progn
                     (mevedel-view--expand-turn)
                     (setq expanded t))
                 (error
                  (message "Containing turn could not be opened: %s"
                           (error-message-string err)))))
             (unless expanded
               (setq pos (or (next-single-property-change
                              pos 'mevedel-view-type nil (point-max))
                             (point-max)))))
           expanded)))
    (or (find-visible)
        (and (expand-collapsed-turn)
             (find-visible)))))

(defun mevedel-view-agent-status-activate-row (&optional event)
  "Reveal the agent handle referenced by the aggregate status row at point."
  (interactive (list last-nonmenu-event))
  (let* ((event-pos (and event (eventp event) (posn-point (event-end event))))
         (pos (if (integer-or-marker-p event-pos) event-pos (point)))
         (agent-id (get-text-property pos 'mevedel-view-agent-id)))
    (when (and event (eventp event))
      (mouse-set-point event))
    (unless agent-id
      (user-error "No agent status row at point"))
    (if (mevedel-view--agent-locate-handle agent-id)
        (progn
          (when (or (mevedel-view--agent-invocation agent-id)
                    (eq (plist-get (mevedel-view--lookup-transcript-entry agent-id)
                                   :status)
                        'running))
            (mevedel-view--set-agent-expanded agent-id t)
            (mevedel-view--full-rerender)
            (mevedel-view--agent-locate-handle agent-id)))
      (message "Agent handle is not in the current view"))))

(defun mevedel-view--interaction-target-buffer (&optional data-buffer)
  "Return the live view buffer that should host queued interactions.
DATA-BUFFER, when non-nil, is the chat/data buffer whose
`mevedel--view-buffer' binding should be consulted.  Signals when
there is no live non-transcript view.  Queue renderers catch this
as a render failure and abort the visible head rather than
silently placing controls in a data buffer."
  (or (and (not (bound-and-true-p mevedel-view--agent-transcript-p))
           (boundp 'mevedel-view--interaction-marker)
           (markerp mevedel-view--interaction-marker)
           (eq (marker-buffer mevedel-view--interaction-marker)
               (current-buffer))
           (current-buffer))
      (and data-buffer
           (buffer-live-p data-buffer)
           (let ((view (buffer-local-value 'mevedel--view-buffer
                                           data-buffer)))
             (and view
                  (buffer-live-p view)
                  (with-current-buffer view
                    (and (not (bound-and-true-p
                               mevedel-view--agent-transcript-p))
                         (boundp 'mevedel-view--interaction-marker)
                         (markerp mevedel-view--interaction-marker)
                         (eq (marker-buffer mevedel-view--interaction-marker)
                             view)
                         view)))))
      (and (boundp 'mevedel--view-buffer)
           mevedel--view-buffer
           (buffer-live-p mevedel--view-buffer)
           (with-current-buffer mevedel--view-buffer
             (and (not (bound-and-true-p mevedel-view--agent-transcript-p))
                  (boundp 'mevedel-view--interaction-marker)
                  (markerp mevedel-view--interaction-marker)
                  (eq (marker-buffer mevedel-view--interaction-marker)
                      mevedel--view-buffer)
                  mevedel--view-buffer)))
      (error "No live view for queued prompt")))

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
        (permissions 0))
    (when (hash-table-p mevedel-view--interaction-descriptors)
      (maphash
       (lambda (_id descriptor)
         (let ((count (or (plist-get descriptor :count) 0)))
           (pcase (plist-get descriptor :kind)
             ('preview (cl-incf previews count))
             ('plan (cl-incf plans count))
             ('permission (cl-incf permissions count)))))
       mevedel-view--interaction-descriptors))
    (when-let* ((session (or (and (boundp 'mevedel--session)
                                  mevedel--session)
                             (and (boundp 'mevedel--data-buffer)
                                  (buffer-live-p mevedel--data-buffer)
                                  (buffer-local-value 'mevedel--session
                                                      mevedel--data-buffer)))))
      (setq plans (max plans (length (mevedel-session-plan-queue session))))
      (setq permissions
            (max permissions
                 (length (mevedel-session-permission-queue session)))))
    (let ((parts
           (delq nil
                 (list
                  (when (> previews 0)
                    (mevedel-view--interaction-plural
                     previews "preview" "previews"))
                  (when (> plans 0)
                    (mevedel-view--interaction-plural plans "plan" "plans"))
                  (when (> permissions 0)
                    (mevedel-view--interaction-plural
                     permissions "permission" "permissions"))))))
      (when parts
        (concat (string-join parts " · ") " pending")))))

(defun mevedel-view--interaction-kind-priority (kind)
  "Return the stable interaction overlay priority for KIND."
  (pcase kind
    ('preview 300)
    ('plan 200)
    ((or 'request 'ask) 150)
    ('permission 100)
    (_ 50)))

(defun mevedel-view--interaction-body (descriptor overlay)
  "Return DESCRIPTOR's body with standard interaction text properties.
OVERLAY is stored on the text so keymap commands can find the
owning interaction overlay from the materialized text span."
  (let* ((body (copy-sequence (or (plist-get descriptor :body) "")))
         (map (plist-get descriptor :keymap))
         (help (plist-get descriptor :help-echo))
         (kind (plist-get descriptor :kind))
         (id (plist-get descriptor :id)))
    (add-text-properties
     0 (length body)
     `(mevedel-view-interaction-kind ,kind
       mevedel-view-interaction-id ,id
       mevedel-view-interaction-overlay ,overlay
       mouse-face highlight
       front-sticky nil
       rear-nonsticky t)
     body)
    (when map
      (add-text-properties 0 (length body) `(keymap ,map) body))
    (when help
      (add-text-properties 0 (length body) `(help-echo ,help) body))
    body))

(defun mevedel-view--interaction-delete-materialized-region ()
  "Delete descriptor-rendered real text from the interaction zone."
  (when (and (overlayp mevedel-view--interaction-materialized-overlay)
             (overlay-buffer mevedel-view--interaction-materialized-overlay))
    (let ((start (overlay-start mevedel-view--interaction-materialized-overlay))
          (end (overlay-end mevedel-view--interaction-materialized-overlay)))
      (delete-overlay mevedel-view--interaction-materialized-overlay)
      (setq mevedel-view--interaction-materialized-overlay nil)
      (when (and start end (< start end))
        (let ((inhibit-read-only t))
          (delete-region start end))))))

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

(defun mevedel-view--interaction-render ()
  "Render the composite interaction-zone separator and descriptors."
  (let ((anchor (mevedel-view--interaction-anchor))
        (label (mevedel-view--interaction-count-label))
        (pairs (mevedel-view--interaction-descriptor-pairs)))
    (mevedel-view--interaction-delete-materialized-region)
    (if label
        (progn
          (unless (overlayp mevedel-view--interaction-separator-overlay)
            (setq mevedel-view--interaction-separator-overlay
                  (make-overlay anchor anchor (current-buffer) nil t))
            (overlay-put mevedel-view--interaction-separator-overlay
                         'mevedel-view-interaction-separator t)
            (overlay-put mevedel-view--interaction-separator-overlay
                         'priority 400))
          (move-overlay mevedel-view--interaction-separator-overlay
                        anchor anchor)
          (overlay-put mevedel-view--interaction-separator-overlay
                       'before-string
                       (concat (mevedel-view--zone-separator label) "\n")))
      (when (overlayp mevedel-view--interaction-separator-overlay)
        (delete-overlay mevedel-view--interaction-separator-overlay)
        (setq mevedel-view--interaction-separator-overlay nil)))
    (when (hash-table-p mevedel-view--interaction-overlays)
      (maphash
       (lambda (id overlay)
         (unless (and (hash-table-p mevedel-view--interaction-descriptors)
                      (gethash id mevedel-view--interaction-descriptors))
           (delete-overlay overlay)
           (remhash id mevedel-view--interaction-overlays)))
       mevedel-view--interaction-overlays))
    (when pairs
      (let ((start anchor)
            (status-type (and (markerp mevedel-view--status-marker)
                              (marker-insertion-type
                               mevedel-view--status-marker)))
            (interaction-type (and (markerp mevedel-view--interaction-marker)
                                   (marker-insertion-type
                                    mevedel-view--interaction-marker)))
            (input-type (and (markerp mevedel-view--input-marker)
                             (marker-insertion-type
                              mevedel-view--input-marker))))
        (save-excursion
          (goto-char anchor)
          (when (markerp mevedel-view--status-marker)
            (set-marker-insertion-type mevedel-view--status-marker nil))
          (when (markerp mevedel-view--interaction-marker)
            (set-marker-insertion-type mevedel-view--interaction-marker nil))
          (when (markerp mevedel-view--input-marker)
            (set-marker-insertion-type mevedel-view--input-marker t))
          (unwind-protect
              (let ((inhibit-read-only t))
                (dolist (pair pairs)
                  (pcase-let* ((`(,id . ,descriptor) pair)
                               (overlay
                                (or (and (hash-table-p
                                          mevedel-view--interaction-overlays)
                                         (gethash id
                                                  mevedel-view--interaction-overlays))
                                    (make-overlay (point) (point)
                                                  (current-buffer) nil t)))
                               (body (concat
                                      (mevedel-view--interaction-body
                                       descriptor overlay)
                                      "\n"))
                               (from (point)))
                    (when (hash-table-p mevedel-view--interaction-overlays)
                      (puthash id overlay
                               mevedel-view--interaction-overlays))
                    (insert body)
                    (move-overlay overlay from (point) (current-buffer))
                    (mevedel-view--interaction-apply-overlay-properties
                     overlay descriptor)))
                (when (< start (point))
                  (setq mevedel-view--interaction-materialized-overlay
                        (make-overlay start (point) (current-buffer) t nil))
                  (overlay-put mevedel-view--interaction-materialized-overlay
                               'mevedel-view-interaction-materialized t)
                  (overlay-put mevedel-view--interaction-materialized-overlay
                               'read-only t)
                  (overlay-put mevedel-view--interaction-materialized-overlay
                               'evaporate nil)))
            (when (markerp mevedel-view--status-marker)
              (set-marker-insertion-type mevedel-view--status-marker
                                         status-type))
            (when (markerp mevedel-view--interaction-marker)
              (set-marker-insertion-type mevedel-view--interaction-marker
                                         interaction-type))
            (when (markerp mevedel-view--input-marker)
              (set-marker-insertion-type mevedel-view--input-marker
                                         input-type))))))))

(defun mevedel-view--interaction-rebuild ()
  "Rebuild interaction-zone descriptors from live preview and queue state.
This deletes only interaction UI overlays and never settles callbacks."
  (unless mevedel-view--agent-transcript-p
    (mevedel-view--interaction-clear)
    (when-let* ((session (and (boundp 'mevedel--session)
                              mevedel--session)))
      (when (mevedel-session-plan-queue session)
        (when (fboundp 'mevedel-plan-queue--render-head)
          (mevedel-plan-queue--render-head session)))
      (when (mevedel-session-permission-queue session)
        (when (fboundp 'mevedel-permission-queue--render-head)
          (mevedel-permission-queue--render-head session))))))

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
         (overlay (or (gethash id mevedel-view--interaction-overlays)
                      (make-overlay anchor anchor (current-buffer) nil t))))
    (puthash id descriptor mevedel-view--interaction-descriptors)
    (puthash id overlay mevedel-view--interaction-overlays)
    (mevedel-view--interaction-apply-overlay-properties overlay descriptor)
    (mevedel-view--interaction-render)
    (when (and (overlay-buffer overlay)
               (not (= (overlay-start overlay) (overlay-end overlay))))
      (goto-char (overlay-start overlay)))
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

(defun mevedel-view--interaction-clear ()
  "Delete all interaction-zone overlays without firing callbacks."
  (mevedel-view--interaction-delete-materialized-region)
  (when (overlayp mevedel-view--interaction-separator-overlay)
    (delete-overlay mevedel-view--interaction-separator-overlay)
    (setq mevedel-view--interaction-separator-overlay nil))
  (when (hash-table-p mevedel-view--interaction-overlays)
    (maphash (lambda (_id overlay) (delete-overlay overlay))
             mevedel-view--interaction-overlays)
    (clrhash mevedel-view--interaction-overlays))
  (when (and (boundp 'mevedel--prompt-overlays)
             (listp mevedel--prompt-overlays))
    (setq mevedel--prompt-overlays
          (cl-remove-if-not
           (lambda (ov)
             (and (overlayp ov) (overlay-buffer ov)))
           mevedel--prompt-overlays)))
  (when (hash-table-p mevedel-view--interaction-descriptors)
    (clrhash mevedel-view--interaction-descriptors)))

(defun mevedel-view--interaction-anchor ()
  "Return the buffer position to anchor an interaction-zone overlay.
Prefers `mevedel-view--interaction-marker' (zone 3 boundary) when
populated, falls back to `mevedel-view--input-marker' for legacy
view buffers without zone markers, and to `(point-max)' for
non-view buffers (e.g. dispatch from a chat buffer that lacks a
view).  Used by permission, preview, and access-request overlays
so they all anchor at the interaction-zone boundary
rather than just above the input prompt."
  (or (and (boundp 'mevedel-view--interaction-marker)
           mevedel-view--interaction-marker
           (marker-position mevedel-view--interaction-marker))
      (and (boundp 'mevedel-view--input-marker)
           mevedel-view--input-marker
           (marker-position mevedel-view--input-marker))
      (point-max)))

(defun mevedel-view--insert-attribution
    (agent-id &optional _live-click-p calls)
  "Insert the `from <type>--<idshort>' attribution fragment for AGENT-ID.
Returns the propertized string (does not modify the buffer).
The agent-id portion is propertized as a click target when the
source agent has a transcript entry.  Click dispatches through
`mevedel-view--open-agent-transcript-or-message', which either
opens via `mevedel-view-open-agent-transcript' or reports why the
transcript is not openable yet.

Running transcripts are not opened through normal attribution UI,
including read-only attach.  CALLS, when non-nil, is used in the
running-state echo-area message."
  (let* ((display-label (mevedel-view--display-label-for-agent agent-id))
         (entry (mevedel-view--lookup-transcript-entry agent-id))
         (status (and entry (plist-get entry :status)))
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
         (openable (and path-ok terminal-p))
         (targetable entry)
         (echo (cond
                (openable (format "Open transcript for %s" agent-id))
                ((eq status 'running)
                 (format
                  "Agent %s still running%s. Transcript available when complete."
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
    (when targetable
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
         s)))
    (unless targetable
      (add-text-properties (length "from ") (length s)
                           `(help-echo ,echo)
                           s))
    s))

(defun mevedel-view--decorate-agent-message-blocks (start end)
  "Decorate `<agent-message from=ID>...</agent-message>' as mailbox cards.
Delegates to `mevedel-view--decorate-mailbox-block' so the body
collapse threshold, click gating, and vtype tag are uniform with
`<agent-result>' rendering.

Multiple `<agent-message>' blocks in one user turn produce one mailbox
card each, in source order.  Non-matching prose in the same turn
remains as ordinary user text."
  (mevedel-view--decorate-mailbox-block
   "<agent-message\\s-+from=\"\\([^\"]+\\)\"\\s-*>"
   "</agent-message>"
   start end
   'agent-message))

(provide 'mevedel-view)

;;; mevedel-view.el ends here
