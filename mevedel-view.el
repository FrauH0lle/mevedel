;;; mevedel-view.el -- Compact view buffer for chat sessions -*- lexical-binding: t -*-

;;; Commentary:

;; Provides a user-facing view buffer that renders a compact display of the
;; gptel data buffer. The data buffer (org-mode) is the authoritative
;; conversation where gptel operates. The view buffer (`mevedel-view-mode')
;; shows collapsed tool results, grouped reads, and an editable input region at
;; the bottom.
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
(defvar gptel-prompt-prefix-alist)
(defvar gptel-response-separator)

;; `mevedel-structs'
(defvar mevedel--data-buffer)
(defvar mevedel--view-buffer)
(defvar mevedel--session)
(defvar mevedel--current-request)
(defvar mevedel--current-directive-uuid)
(declare-function mevedel-request-begin "mevedel-structs"
                  (session &optional directive-uuid))
(declare-function mevedel-request-end "mevedel-structs" ())
(declare-function mevedel-session-skills "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-name "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-workspace "mevedel-structs" (cl-x) t)
(declare-function mevedel-workspace-name "mevedel-structs" (cl-x) t)
(declare-function mevedel-workspace-root "mevedel-structs" (cl-x) t)

;; `org'
(declare-function org-fontify-like-in-org-mode "ext:org" (s &optional odd-levels))
(defvar org-inhibit-startup)
(defvar org-mode-hook)

;; `mevedel-tool-registry'
(declare-function mevedel-tool-display-string "mevedel-tool-registry" (tool-name args))
(declare-function mevedel-tool-get "mevedel-tool-registry" (name &optional category))
(declare-function mevedel-tool-name "mevedel-tool-registry" (cl-x) t)
(declare-function mevedel-tool-renderer "mevedel-tool-registry" (cl-x) t)

;; `mevedel-pipeline'
(declare-function mevedel-pipeline-extract-render-data "mevedel-pipeline" (result-string))

;; `mevedel-skills'
(declare-function mevedel-skills--parse-slash-line "mevedel-skills" (text))
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
with faces.  The view buffer itself stays in `mevedel-view-mode' — no
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

(defcustom mevedel-view-mailbox-collapse-line-threshold 5
  "Mailbox ✉ block bodies longer than this many lines start collapsed.
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
overlays anchor here without moving it.  Overlay-based status content
displays via `before-string'.  See spec 23 \"Zone model\".")

(defvar-local mevedel-view--interaction-marker nil
  "Marker delimiting the bottom of zone 2 (status) and top of zone 3 (interaction).
Insertion-type `t' so status content above advances it; interaction-zone
overlays anchor here.  Permission queue head, plan confirmation, and
preview overlays render against this marker.")

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

KEY is a fingerprint built from `(NAME . ARGS-PRINT)' since gptel's
pre/post-tool-call hooks do not expose the backend tool-call id.
Parallel dispatches with identical name + args are indistinguishable
(they share one entry); this is acceptable in practice — users rarely
issue two simultaneous identical calls, and the visible \"Calling
X…\" line is informational.

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


;;
;;; Major mode

(defvar-keymap mevedel-view--display-map
  :doc "Keymap active in the read-only display region of the view buffer.
Applied via the `keymap' text property so these bindings only fire
above `mevedel-view--input-marker'."
  "TAB" #'mevedel-view-toggle-section
  "n" #'mevedel-view-next-turn
  "p" #'mevedel-view-prev-turn
  "t" #'mevedel-view-toggle-transcript
  "q" #'quit-window)

(defvar-keymap mevedel-view-mode-map
  :doc "Keymap for `mevedel-view-mode'."
  "C-c RET" #'mevedel-view-send
  "C-c C-k" #'mevedel-view-abort)

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
  (setq-local window-point-insertion-type t))


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

(defun mevedel-view--fontify-response (text)
  "Return TEXT with `org-mode' face properties applied.
Returns TEXT unchanged when `mevedel-view-fontify-responses' is nil or
`org' cannot be loaded.  Binds `org-inhibit-startup' and clears
`org-mode-hook' so the temp buffer used by
`org-fontify-like-in-org-mode' is as lightweight as possible.
Faces are stored as `font-lock-face' so they survive the view
buffer's font-lock refontification cycles."
  (if (and mevedel-view-fontify-responses
           (require 'org nil t))
      (let ((org-inhibit-startup t)
            (org-mode-hook nil))
        (mevedel-view--promote-face-to-font-lock-face
         (org-fontify-like-in-org-mode text)))
    text))


;;
;;; Setup

(defun mevedel-view--setup (view-buf data-buf)
  "Initialize VIEW-BUF as the view buffer for DATA-BUF.
Activates `mevedel-view-mode', wires the cross-references, and
inserts the initial separator with input marker."
  (with-current-buffer view-buf
    (mevedel-view-mode)
    (setq-local mevedel--data-buffer data-buf)
    ;; Copy workspace directory so relative paths resolve correctly
    (setq-local default-directory
                (buffer-local-value 'default-directory data-buf))
    ;; Insert session header and set up zone markers (spec 23).
    ;;
    ;; Three markers carve the buffer above the input prompt into
    ;; four zones (history / status / interaction / input).  At
    ;; setup all three coincide at end-of-header; the first piece
    ;; of history content pushes them all forward together.  Status
    ;; and interaction zones populate via overlays anchored to
    ;; their markers, not via inserted text, so the markers stay
    ;; put when status / interaction content appears.
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (mevedel-view--header-string data-buf))
      ;; status-marker first — history pushes it forward (insertion-type t).
      (setq mevedel-view--status-marker (point-marker))
      (set-marker-insertion-type mevedel-view--status-marker t)
      ;; interaction-marker next — likewise advances under status content.
      (setq mevedel-view--interaction-marker (point-marker))
      (set-marker-insertion-type mevedel-view--interaction-marker t)
      ;; input-marker last — stays before insertions (insertion-type nil)
      ;; so the prompt is always the last thing.
      (setq mevedel-view--input-marker (point-marker))
      (set-marker-insertion-type mevedel-view--input-marker nil)
      ;; Insert the read-only prompt after the input marker.  The
      ;; marker keeps its `nil' insertion type, so it stays BEFORE the
      ;; prompt; later renders insert new content at the marker
      ;; position which pushes the prompt further down, preserving
      ;; the invariant that the prompt is always the last thing
      ;; before the user's input.
      (let ((start (point)))
        (insert mevedel-view--input-prompt)
        (add-text-properties
         start (point)
         `(read-only t
           font-lock-face mevedel-view-input-prompt
           mevedel-view-prompt t
           front-sticky (read-only mevedel-view-prompt)
           rear-nonsticky (read-only mevedel-view-prompt font-lock-face)))))
    ;; Install slash-command completion
    (add-hook 'completion-at-point-functions
              #'mevedel-view-slash-capf nil t)
    ;; Install @ref/@file font-lock and completion
    (mevedel-mentions-install)
    ;; Kill-buffer lifecycle: view killed -> clear ref on data buffer
    (add-hook 'kill-buffer-hook #'mevedel-view--on-view-killed nil t)
    ;; Proxy header-line from data buffer
    (setq header-line-format
          '(:eval (mevedel-view--proxy-header-line))))
  ;; Wire the reverse reference on the data buffer
  (with-current-buffer data-buf
    (setq-local mevedel--view-buffer view-buf)
    ;; Kill-buffer lifecycle: data killed -> kill view buffer
    (add-hook 'kill-buffer-hook #'mevedel-view--on-data-killed nil t)))

(defun mevedel-view--ensure (data-buf)
  "Return the view buffer for DATA-BUF, creating it if needed."
  (or (let ((vb (buffer-local-value 'mevedel--view-buffer data-buf)))
        (and vb (buffer-live-p vb) vb))
      (let* ((data-name (buffer-name data-buf))
             ;; Derive view buffer name from data buffer name:
             ;; *mevedel:main@proj* -> *mevedel:main@proj:view*
             (view-name (if (string-match "\\*$" data-name)
                            (replace-match ":view*" t t data-name)
                          (concat data-name ":view")))
             (view-buf (get-buffer-create view-name)))
        (mevedel-view--setup view-buf data-buf)
        view-buf)))


;;
;;; Lifecycle

(defun mevedel-view--on-view-killed ()
  "Hook run when the view buffer is killed.
Clears `mevedel--view-buffer' on the associated data buffer and kills
it.  The reference is cleared before killing so the data buffer's own
kill hook sees nil and exits without re-entering this function."
  (when-let* ((db mevedel--data-buffer)
              (_ (buffer-live-p db)))
    (with-current-buffer db
      (setq mevedel--view-buffer nil))
    (kill-buffer db)))

(defun mevedel-view--on-data-killed ()
  "Hook run when the data buffer is killed.
Kills the associated view buffer."
  (when-let* ((vb mevedel--view-buffer)
              (_ (buffer-live-p vb)))
    (kill-buffer vb)))

(defun mevedel-view--proxy-header-line ()
  "Return the header-line string from the data buffer.
Used as `:eval' form in the view buffer's `header-line-format'."
  (when-let* ((db mevedel--data-buffer)
              (_ (buffer-live-p db))
              (fmt (buffer-local-value 'header-line-format db)))
    (format-mode-line fmt nil nil db)))


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

(defun mevedel-view--start-spinner (&optional status)
  "Show a spinner overlay with STATUS text in the view buffer.
STATUS defaults to \"Thinking...\"."
  (mevedel-view--stop-spinner)
  (save-excursion
    (goto-char mevedel-view--input-marker)
    (let* ((inhibit-read-only t)
           (text (propertize (concat (or status "Thinking...") "\n")
                             'font-lock-face 'mevedel-view-spinner
                             'read-only t
                             'keymap mevedel-view--display-map
                             'front-sticky '(read-only keymap)
                             'rear-nonsticky '(read-only keymap)))
           (start (point)))
      (insert text)
      (let ((ov (make-overlay start (point) nil t)))
        (overlay-put ov 'mevedel-view-spinner t)
        (overlay-put ov 'evaporate t)
        (setq mevedel-view--spinner-overlay ov)))))

(defun mevedel-view--update-spinner (status)
  "Update the spinner overlay to show STATUS text."
  (if mevedel-view--spinner-overlay
      (let ((inhibit-read-only t)
            (start (overlay-start mevedel-view--spinner-overlay))
            (end (overlay-end mevedel-view--spinner-overlay)))
        (save-excursion
          (goto-char start)
          (delete-region start end)
          (insert (propertize (concat status "\n")
                              'font-lock-face 'mevedel-view-spinner
                              'read-only t
                              'keymap mevedel-view--display-map
                              'front-sticky '(read-only keymap)
                              'rear-nonsticky '(read-only keymap)))
          (move-overlay mevedel-view--spinner-overlay start (point))))
    (mevedel-view--start-spinner status)))

(defun mevedel-view--stop-spinner ()
  "Remove the spinner overlay if present."
  (when mevedel-view--spinner-overlay
    (let ((inhibit-read-only t))
      (delete-region (overlay-start mevedel-view--spinner-overlay)
                     (overlay-end mevedel-view--spinner-overlay)))
    (delete-overlay mevedel-view--spinner-overlay)
    (setq mevedel-view--spinner-overlay nil)))

(defun mevedel-view--spinner-hook (info)
  "Update spinner from `gptel-pre-tool-call-functions'.
INFO is a plist with at least :name and :args."
  (when-let* ((view-buf (buffer-local-value 'mevedel--view-buffer
                                            (current-buffer)))
              (_ (buffer-live-p view-buf))
              (tool-name (plist-get info :name))
              (args (plist-get info :args)))
    (let ((summary (mevedel-view--tool-status-string tool-name args)))
      (with-current-buffer view-buf
        (mevedel-view--update-spinner summary))))
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

(defun mevedel-view--group-into-turns (segments)
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
or `tool' (mid-turn reasoning gap between response chunks)."
  (let (turns current-segs current-role turn-start prev-type
        (rest segments))
    (while rest
      (let* ((seg (car rest))
             (type (car seg))
             (seg-start (cadr seg))
             (next-type (car-safe (cadr rest))))
        (if (and (eq type 'user)
                 (memq prev-type '(nil user response))
                 ;; Look-ahead: a nil gap right after response with
                 ;; ignore/tool coming next is mid-turn reasoning.
                 (not (and (eq prev-type 'response)
                           (memq next-type '(ignore tool)))))
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
              (setq current-segs nil current-role nil turn-start nil))
          ;; Assistant-side segment (response, tool, ignore, or
          ;; reasoning text misclassified as user).
          (unless current-role
            (setq current-role 'assistant
                  turn-start seg-start))
          (push seg current-segs))
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
parses the S-expression to extract tool name, and builds a summary."
  (with-current-buffer data-buf
    (let ((text (buffer-substring-no-properties seg-start seg-end)))
      (condition-case nil
          (let* ((sexp (read (substring text 0)))
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
                  (replace-regexp-in-string "[\n\r]+" " " text)
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
or nil when the segment is not a well-formed tool block."
  (with-current-buffer data-buf
    (let ((text (buffer-substring-no-properties seg-start seg-end)))
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
`mevedel-view--resolve-path' and gated on `file-exists-p' — paths that
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

(defun mevedel-view--render-collapsed-header (rendering source)
  "Insert the collapsed header for RENDERING with SOURCE coordinates.
RENDERING is a rendering plist. SOURCE is (DATA-START . DATA-END)."
  (let* ((header (plist-get rendering :header))
         (line (concat mevedel-view--tool-glyph header))
         (ins-start (point)))
    (insert (propertize (concat line "\n")
                        'font-lock-face 'mevedel-view-tool-summary
                        'mevedel-view-type 'tool-summary
                        'mevedel-view-collapsed t
                        'mevedel-view-source source
                        'mevedel-view-rendered t))
    (mevedel-view--linkify-paths-in-range ins-start (point))))

(defun mevedel-view--render-expanded-body (rendering source)
  "Insert the expanded body for RENDERING with SOURCE coordinates."
  (let* ((header (plist-get rendering :header))
         (body (or (plist-get rendering :body) ""))
         (body-mode (plist-get rendering :body-mode))
         (fontified (mevedel-view--fontify-as body body-mode))
         (header-line (concat mevedel-view--tool-glyph header))
         (ins-start (point)))
    (insert (propertize (concat header-line "\n")
                        'font-lock-face 'mevedel-view-tool-summary))
    (insert fontified)
    (unless (eq (char-before) ?\n)
      (insert "\n"))
    (add-text-properties ins-start (point)
                         `(mevedel-view-type tool-summary
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
      (if (plist-get rendering :initially-collapsed-p)
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
                   "#\\+\\(?:begin\\|end\\)_reasoning" "" cleaned))
    (setq cleaned (replace-regexp-in-string
                   "#\\+begin_tool[^\n]*\n?" "" cleaned))
    (setq cleaned (replace-regexp-in-string
                   "#\\+end_tool" "" cleaned))
    cleaned))

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
        ;; Stop the spinner
        (mevedel-view--stop-spinner)
        ;; Cancel any pending debounced stream render -- the final
        ;; render below subsumes whatever it would have drawn.
        (mevedel-view--cancel-stream-render)
        ;; Delegate to the shared incremental path, which deletes the
        ;; in-flight assistant turn (if any) and re-renders it from the
        ;; data buffer.  After the final render completes, clear the
        ;; in-flight markers so the next exchange starts clean.
        (mevedel-view--render-incremental data-buf start end)
        (setq mevedel-view--pending-tool-calls nil)
        (when (markerp mevedel-view--in-flight-turn-start)
          (set-marker mevedel-view--in-flight-turn-start nil)
          (setq mevedel-view--in-flight-turn-start nil))
        (when (markerp mevedel-view--data-turn-start)
          (set-marker mevedel-view--data-turn-start nil)
          (setq mevedel-view--data-turn-start nil))))))

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

Keys are (VTYPE . DATA-START) — the segment vtype plus the car of
its `mevedel-view-source' cons.  Values are t when collapsed, nil
when expanded.  Identity is keyed on the data-start only (not the
full source cons) so thinking-summary and tool-group segments keep
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

User turns inside the extracted range are filtered whenever the
exchange is in-flight (`mevedel-view--in-flight-turn-start' is a live
marker).  The user's input was echoed once by
`mevedel-view--insert-user-message' at send time, so re-rendering it
here would produce a duplicate \"You\" block above the assistant
reply.

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
         (turns (mevedel-view--group-into-turns segments))
         (in-flight-p (and (markerp mevedel-view--in-flight-turn-start)
                           (marker-position mevedel-view--in-flight-turn-start)))
         (pending mevedel-view--pending-tool-calls))
    ;; Filter pre-rendered user turn(s) for the duration of the
    ;; exchange.  Either of two signals indicate the user turn is
    ;; already echoed: the classic flag (from `--insert-user-message')
    ;; or the presence of the in-flight-turn marker (which persists
    ;; across multiple incremental renders within one exchange).
    (while (and turns
                (or mevedel-view--user-pre-rendered in-flight-p)
                (eq (plist-get (car turns) :role) 'user))
      (setq turns (cdr turns)))
    (setq mevedel-view--user-pre-rendered nil)
    (mevedel-view--preserving-window-state
      (let* ((inhibit-read-only t)
             (capture-p
              (and in-flight-p
                   (<= (marker-position mevedel-view--in-flight-turn-start)
                       (marker-position mevedel-view--input-marker))))
             (saved-states
              (when capture-p
                (mevedel-view--capture-collapse-states
                 (marker-position mevedel-view--in-flight-turn-start)
                 (marker-position mevedel-view--input-marker)))))
        ;; Wipe the current in-flight assistant turn render (if any)
        ;; so we can re-render it from scratch from the updated data.
        (when capture-p
          (delete-region mevedel-view--in-flight-turn-start
                         mevedel-view--input-marker))
        (dolist (turn turns)
          (mevedel-view--render-turn turn data-buf))
        (when pending
          (let* ((cap mevedel-view-pending-tools-visible-max)
                 (visible (cl-subseq pending 0 (min cap (length pending)))))
            (mevedel-view--insert-pending-tool-lines visible)))
        ;; Restore user-toggled collapse/expand state that the delete
        ;; above just wiped.  Walk the freshly rendered span and toggle
        ;; only sections whose saved state differs from the default.
        (when (and saved-states in-flight-p)
          (mevedel-view--apply-collapse-states
           (marker-position mevedel-view--in-flight-turn-start)
           (marker-position mevedel-view--input-marker)
           saved-states))))))

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
                         (mevedel-view--render-incremental data-buf))))))))))))

(defun mevedel-view--pending-tool-key (name args)
  "Build a fingerprint key for the pending-tool-calls alist.
NAME is the tool name string; ARGS is the args plist.  Used by the
pre/post-tool hooks to correlate calls in the absence of a gptel-
exposed call id."
  (cons name
        (let ((print-level 4)
              (print-length 32)
              (print-circle t))
          (prin1-to-string args))))

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
              (tool-args (plist-get args :args))
              (data-buf (current-buffer)))
    (with-current-buffer view-buf
      (mevedel-view--cancel-stream-render)
      (let ((key (mevedel-view--pending-tool-key name tool-args)))
        (unless (assoc key mevedel-view--pending-tool-calls)
          (setq mevedel-view--pending-tool-calls
                (append mevedel-view--pending-tool-calls
                        (list (cons key name))))))
      (when (and (markerp mevedel-view--in-flight-turn-start)
                 (markerp mevedel-view--data-turn-start))
        (mevedel-view--render-incremental data-buf)))))

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
              (tool-args (plist-get args :args))
              (data-buf (current-buffer)))
    (with-current-buffer view-buf
      (mevedel-view--cancel-stream-render)
      (let ((key (mevedel-view--pending-tool-key name tool-args)))
        (setq mevedel-view--pending-tool-calls
              (assoc-delete-all key mevedel-view--pending-tool-calls)))
      (when (and (markerp mevedel-view--in-flight-turn-start)
                 (markerp mevedel-view--data-turn-start))
        (mevedel-view--render-incremental data-buf)))))

(defun mevedel-view--insert-pending-tool-lines (entries)
  "Insert ephemeral `Calling X…' lines for ENTRIES.
ENTRIES is a subset of `mevedel-view--pending-tool-calls' (head N).
When the full list exceeds `mevedel-view-pending-tools-visible-max',
the caller passes only the visible head and a tail-summary line is
appended."
  (save-excursion
    (goto-char mevedel-view--input-marker)
    (set-marker-insertion-type mevedel-view--input-marker t)
    (unwind-protect
        (let ((inhibit-read-only t)
              (cap mevedel-view-pending-tools-visible-max)
              (total (length mevedel-view--pending-tool-calls)))
          (dolist (entry entries)
            (let ((name (cdr entry)))
              (insert (propertize (format "⠋ Calling %s…\n" name)
                                  'font-lock-face 'mevedel-view-ephemeral
                                  'read-only t
                                  'front-sticky '(read-only)
                                  'rear-nonsticky '(read-only)))))
          (when (> total cap)
            (insert (propertize
                     (format "⠋ %d more tools running…\n" (- total cap))
                     'font-lock-face 'mevedel-view-ephemeral
                     'read-only t
                     'front-sticky '(read-only)
                     'rear-nonsticky '(read-only)))))
      (set-marker-insertion-type mevedel-view--input-marker nil))))

(defun mevedel-view--insert-pending-tool-line (tool-name)
  "Insert an ephemeral `Calling TOOLNAME…' status line above the input.

Backwards-compatible single-tool helper.  New parallel-tool path uses
`mevedel-view--insert-pending-tool-lines'."
  (mevedel-view--insert-pending-tool-lines
   (list (cons (mevedel-view--pending-tool-key tool-name nil) tool-name))))

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
                  (mevedel-view--user-turn-text segments data-buf)))
    (save-excursion
      (goto-char mevedel-view--input-marker)
      ;; Temporarily let the marker advance past our insertions so
      ;; successive turns are appended in order.
      (set-marker-insertion-type mevedel-view--input-marker t)
      (unwind-protect
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
            ;; Trailing separator — horizontal rule after assistant turns,
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
            (add-text-properties insert-start (point)
                                 `(read-only t
                                   keymap ,mevedel-view--display-map
                                   front-sticky (read-only keymap)
                                   rear-nonsticky (read-only keymap)))
            ;; Fill in source on regions that have none yet (headers,
            ;; separators) so the entire block is navigable.
            (let ((pos insert-start))
              (while (< pos (point))
                (if (get-text-property pos 'mevedel-view-source)
                    (setq pos (or (next-single-property-change
                                   pos 'mevedel-view-source nil (point))
                                  (point)))
                  (let ((next (or (next-single-property-change
                                   pos 'mevedel-view-source nil (point))
                                  (point))))
                    (put-text-property pos next 'mevedel-view-source
                                       (cons turn-start turn-end))
                    (setq pos next)))))
            ;; Tag every character in this turn with a unique id so
            ;; turn-level fold/unfold can find the whole span even after
            ;; inner sections have been expanded or collapsed.
            (put-text-property insert-start (point)
                               'mevedel-view-turn-id
                               (cl-gensym "mevedel-view-turn-")))
        (set-marker-insertion-type mevedel-view--input-marker nil))))))

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
          ;; Strip reasoning block markers
          (setq text (replace-regexp-in-string
                      "#\\+\\(?:begin\\|end\\)_reasoning" "" text))
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

(defun mevedel-view--render-user-turn (segments data-buf)
  "Render user SEGMENTS from DATA-BUF."
  (insert (propertize "You\n"
                      'font-lock-face 'mevedel-view-user-header
                      'mevedel-view-type 'turn-header
                      'mevedel-view-turn-role 'user
                      'mevedel-view-collapsed nil))
  (let ((text (mevedel-view--user-turn-text segments data-buf))
        (text-start nil))
    (unless (string-empty-p text)
      (setq text-start (point))
      (insert text)
      ;; decorate any `<agent-result agent-id=...>' blocks
      ;; (background mailbox deliveries) with open-transcript buttons.
      (mevedel-view--decorate-agent-result-blocks text-start (point))))
  (insert "\n"))

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
          ((or 'ignore 'user)
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
  "Render a group of consecutive TOOL-SEGMENTS from DATA-BUF.
If the group has 3+ read-like tools, collapse into a single summary.
Otherwise render each individually — a registered `:renderer' is
invoked when the segment carries a render-data side-channel, falling
back to the default one-liner otherwise."
  (let ((count (length tool-segments)))
    (if (and (>= count 3)
             (cl-every (lambda (seg)
                         (mevedel-view--read-like-tool-p data-buf
                                                         (cadr seg)
                                                         (caddr seg)))
                       tool-segments))
        ;; Grouped summary for consecutive reads/searches
        (let* ((first-start (cadr (car tool-segments)))
               (last-end (caddr (car (last tool-segments))))
               (summary (format "%sReading %d files..."
                                mevedel-view--tool-glyph count)))
          (insert (propertize (concat summary "\n")
                              'font-lock-face 'mevedel-view-tool-summary
                              'mevedel-view-type 'tool-group
                              'mevedel-view-collapsed t
                              'mevedel-view-source (cons first-start last-end))))
      ;; Individual one-liners
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
                                  'mevedel-view-source source)))))))))

(defun mevedel-view--read-like-tool-p (data-buf seg-start _seg-end)
  "Return non-nil if the tool segment at SEG-START in DATA-BUF is read-like."
  (with-current-buffer data-buf
    (condition-case nil
        (let* ((text (buffer-substring-no-properties
                      seg-start (min (+ seg-start 200) (point-max))))
               (sexp (read text))
               (name (plist-get sexp :name)))
          (member name '("Read" "Glob" "Grep")))
      (error nil))))


;;
;;; Expand/collapse

(defvar mevedel-view--collapsible-vtypes
  '(thinking-summary tool-summary tool-group response)
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
     ((and source (memq vtype mevedel-view--collapsible-vtypes))
      (if collapsed
          (mevedel-view--expand-section source vtype)
        (mevedel-view--collapse-section source vtype)))
     (t
      (user-error "No collapsible section at point")))))

(defun mevedel-view--section-bounds ()
  "Return (START . END) of the current section at point.
A section is a contiguous region with the same `mevedel-view-source'.
Compared with `eq' to match property-change scanning semantics — two
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
        ;; position before point — which lands in the PREVIOUS run when
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
         (rendering (and (eq vtype 'tool-summary)
                         data-buf (buffer-live-p data-buf)
                         (mevedel-view--segment-rendering
                          data-buf data-start data-end))))
    (when (and bounds data-buf (buffer-live-p data-buf))
      (let ((inhibit-read-only t)
            (view-start (car bounds))
            (view-end (cdr bounds))
            ;; Preserve the enclosing turn-id across delete+insert so
            ;; turn-level fold still recognises this section as part of
            ;; the turn.
            (turn-id (get-text-property (car bounds) 'mevedel-view-turn-id)))
        (save-excursion
          (goto-char view-start)
          (set-marker-insertion-type mevedel-view--input-marker t)
          (unwind-protect
              (progn
                (delete-region view-start view-end)
                (if rendering
                    ;; Renderer-driven body — produce expanded form and
                    ;; stamp the same read-only/keymap properties the
                    ;; default path adds so navigation still works.
                    (let ((ins-start (point)))
                      (mevedel-view--render-expanded-body rendering source)
                      (add-text-properties
                       ins-start (point)
                       `(read-only t
                         keymap ,mevedel-view--display-map
                         front-sticky (read-only keymap)
                         rear-nonsticky (read-only keymap))))
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
                    (when (string-empty-p text)
                      (setq text "[section no longer available]"))
                    (insert text)
                    (unless (eq (char-before) ?\n)
                      (insert "\n"))
                    (add-text-properties view-start (point)
                                         `(read-only t
                                           keymap ,mevedel-view--display-map
                                           front-sticky (read-only keymap)
                                           rear-nonsticky (read-only keymap)
                                           mevedel-view-source ,source
                                           mevedel-view-type ,vtype
                                           mevedel-view-collapsed nil))))
                (when turn-id
                  (put-text-property view-start (point)
                                     'mevedel-view-turn-id turn-id)))
            (set-marker-insertion-type mevedel-view--input-marker nil)))))))

(defun mevedel-view--collapse-section (source vtype)
  "Collapse an expanded section back to a one-liner.
SOURCE is the data buffer coordinates, VTYPE the section type.
Only handles the known collapsible vtypes in
`mevedel-view--collapsible-vtypes' — unknown vtypes are ignored so
that a stray TAB on a non-collapsible region never rewrites a large
span of the buffer with a best-guess preview.

Tool segments with a registered renderer produce the renderer's
`:header' string; everything else falls back to the default summary."
  (let* ((bounds (mevedel-view--section-bounds))
         (data-buf mevedel--data-buffer)
         (data-start (car source))
         (data-end (cdr source))
         (rendering (and (eq vtype 'tool-summary)
                         data-buf (buffer-live-p data-buf)
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
              ('tool-group (concat mevedel-view--tool-glyph "Reading files..."))
              ('thinking-summary
               (mevedel-view--thinking-summary data-buf data-start data-end))
              ('response
               (mevedel-view--response-summary data-buf data-start data-end)))))))
    (when (and bounds data-buf (buffer-live-p data-buf) summary)
      (let ((inhibit-read-only t)
            (view-start (car bounds))
            (view-end (cdr bounds))
            (face (pcase vtype
                    ((or 'tool-summary 'tool-group) 'mevedel-view-tool-summary)
                    ('thinking-summary 'mevedel-view-thinking-summary)
                    ('response 'mevedel-view-response-summary)))
            (turn-id (get-text-property (car bounds) 'mevedel-view-turn-id)))
        (save-excursion
          (goto-char view-start)
          (set-marker-insertion-type mevedel-view--input-marker t)
          (unwind-protect
              (progn
                (delete-region view-start view-end)
                (let ((ins-start (point)))
                  (insert (propertize (concat summary "\n")
                                      'font-lock-face face
                                      'mevedel-view-type vtype
                                      'mevedel-view-collapsed t
                                      'mevedel-view-source source
                                      'read-only t
                                      'keymap mevedel-view--display-map
                                      'front-sticky '(read-only keymap)
                                      'rear-nonsticky '(read-only keymap)))
                  (when turn-id
                    (put-text-property ins-start (point)
                                       'mevedel-view-turn-id turn-id))))
            (set-marker-insertion-type mevedel-view--input-marker nil)))))))

(defun mevedel-view--response-summary (data-buf data-start data-end)
  "Build a one-line summary of a response block in DATA-BUF.
Reads the text between DATA-START and DATA-END, extracts the first
non-empty line, and annotates the line count."
  (let* ((text (mevedel-view--data-substring data-buf data-start data-end))
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
Return nil when the body is a single line — short turns are already
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
              ((or 'tool-summary 'tool-group) (cl-incf tool-count))
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

(defun mevedel-view--full-rerender ()
  "Re-render the entire view buffer from the data buffer.
Wipes all rendered content and re-renders from scratch.  Used after
compaction, session resume, or manual refresh."
  (unless mevedel--data-buffer
    (error "No data buffer"))
  (let ((data-buf mevedel--data-buffer)
        (inhibit-read-only t))
    ;; Wipe display area (everything above input marker)
    (delete-region (point-min) mevedel-view--input-marker)
    ;; Re-insert header and advance the input marker past it.  The
    ;; marker's default insertion-type is nil, so inserting at its
    ;; position leaves it BEFORE the inserted text; without the
    ;; explicit `set-marker' below, the first turn render would be
    ;; inserted before the header and push it to the bottom.
    (goto-char (point-min))
    (insert (mevedel-view--header-string data-buf))
    (set-marker mevedel-view--input-marker (point))
    ;; Render all content from data buffer
    (with-current-buffer data-buf
      ;; Skip compacted region at the start.  After compaction the data
      ;; buffer has: [ignore+shadow old content] [ignore separator]
      ;; [ignore #+begin_summary] [nil summary text] [ignore
      ;; #+end_summary] [live content].  Skip past all of it.
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
        ;; Narrow so that `extract-segments' boundary expansion
        ;; (`previous-single-property-change' bounded by `point-min')
        ;; can't walk back into the leading drawer / compacted region.
        (save-restriction
          (narrow-to-region scan-start (point-max))
          (let* ((segments (mevedel-view--extract-segments
                            (point-min) (point-max)))
                 (turns (mevedel-view--group-into-turns segments)))
            (with-current-buffer (buffer-local-value 'mevedel--view-buffer data-buf)
              (dolist (turn turns)
                (mevedel-view--render-turn turn data-buf)))))))))


;;
;;; Input forwarding

(defun mevedel-view--insert-user-message (text)
  "Render TEXT as a user message in the display region.
Inserts above `mevedel-view--input-marker' with read-only protection.

Sets `mevedel-view--user-pre-rendered' so the post-response render
path knows to skip the user turn it would otherwise extract for this
same exchange -- see `mevedel-view--render-response'."
  (save-excursion
    (goto-char mevedel-view--input-marker)
    (set-marker-insertion-type mevedel-view--input-marker t)
    (unwind-protect
        (let ((inhibit-read-only t)
              (start (point)))
          (insert (propertize "You\n" 'font-lock-face 'mevedel-view-user-header))
          (insert text)
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
           (skill-names
            (when session
              (mapcar #'mevedel-skill-name
                      (mevedel-session-skills session))))
           (local-names (mapcar #'car mevedel-slash-commands))
           (candidates (append local-names skill-names)))
      (list start end candidates
            :exclusive 'no
            :annotation-function
            (lambda (name)
              (if (assoc name mevedel-slash-commands)
                  " [command]"
                " [skill]"))))))

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
  (unless mevedel--data-buffer
    (user-error "No data buffer associated with this view"))
  (unless (buffer-live-p mevedel--data-buffer)
    (user-error "Data buffer has been killed"))
  (when (buffer-local-value 'mevedel--current-request mevedel--data-buffer)
    (user-error "A request is already active -- wait or abort first"))
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
            (mevedel-view--clear-input)
            (with-current-buffer mevedel--data-buffer
              (funcall (cdr local) args)))
           (skill
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
  (mevedel-view--stop-spinner)
  (when-let* ((data-buf mevedel--data-buffer)
              (_ (buffer-live-p data-buf)))
    ;; Delegate to mevedel-abort which handles the full teardown
    (with-current-buffer data-buf
      (when (fboundp 'mevedel-abort)
        (funcall #'mevedel-abort)))))


;;
;;; Sub-agent transcript open command

(declare-function mevedel-session-agent-transcripts
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-save-path "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-persistence--validate-transcript-path
                  "mevedel-session-persistence" (path save-path))
(defvar mevedel-session--read-only-mode)

(defun mevedel-view--lookup-transcript-entry (agent-id)
  "Return the parent session's transcript entry plist for AGENT-ID.

Resolves the parent chat (data) buffer from the current view
buffer, reads its `mevedel--session', and looks up AGENT-ID in the
session's `agent-transcripts' alist.  Returns nil if any link is
missing."
  (when-let* ((data-buf (and (boundp 'mevedel--data-buffer)
                             mevedel--data-buffer))
              ((buffer-live-p data-buf))
              (session (buffer-local-value 'mevedel--session data-buf))
              (entries (mevedel-session-agent-transcripts session)))
    (cdr (assoc agent-id entries))))

(defun mevedel-view-open-agent-transcript (agent-id)
  "Open the on-disk transcript file for AGENT-ID in read-only mode.

Looks up the entry in the parent session's `agent-transcripts'
slot and validates the path through
`mevedel-session-persistence--validate-transcript-path' before
opening.  Surfaces a `user-error' when the entry is missing, the
path fails validation, or the file is absent on disk.

When the parent session is in read-only attach mode and the entry
status is `running', emits a one-line warning that another Emacs
is the live writer."
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
  (let* ((data-buf (and (boundp 'mevedel--data-buffer)
                        mevedel--data-buffer))
         (session (and data-buf (buffer-live-p data-buf)
                       (buffer-local-value 'mevedel--session data-buf)))
         (entry (and session (cdr (assoc agent-id
                                         (mevedel-session-agent-transcripts
                                          session)))))
         (save-path (and session (mevedel-session-save-path session)))
         (rel-path (and entry (plist-get entry :path)))
         (status (and entry (plist-get entry :status)))
         (readonly-attach
          (and data-buf (buffer-live-p data-buf)
               (buffer-local-value 'mevedel-session--read-only-mode
                                   data-buf))))
    (unless entry
      (user-error "No transcript entry for agent-id: %s" agent-id))
    (unless save-path
      (user-error "Parent session has no save-path"))
    (unless (mevedel-session-persistence--validate-transcript-path
             rel-path save-path)
      (user-error "Transcript path failed validation: %s" rel-path))
    (let ((abs (expand-file-name rel-path save-path)))
      (unless (file-exists-p abs)
        (user-error "Transcript file missing: %s" abs))
      (let ((buf (find-file-noselect abs)))
        (with-current-buffer buf
          (unless buffer-read-only (read-only-mode +1)))
        (pop-to-buffer buf)
        (when (and readonly-attach (eq status 'running))
          (message "Note: transcript is being written by another \
Emacs; contents may be incomplete"))))))

(defun mevedel-view--decorate-agent-result-blocks (start end)
  "Add open-transcript buttons over `<agent-result agent-id=...>' blocks.

Scans the region START..END in the current view buffer for
`<agent-result>' tags.  For each match, joins the agent-id back to
the parent session's transcript slot, validates the recorded path
through `mevedel-session-persistence--validate-transcript-path',
and decorates the opening tag with a clickable button only when
both the entry exists and the path is safe.  Activating the button
calls `mevedel-view-open-agent-transcript' on the parsed agent-id."
  (save-excursion
    (goto-char start)
    (while (re-search-forward
            "<agent-result\\s-+[^>]*agent-id=\"\\([^\"]+\\)\"" end t)
      (let ((id-start (match-beginning 0))
            (id-end (match-end 0))
            (id (match-string-no-properties 1)))
        (let* ((entry (mevedel-view--lookup-transcript-entry id))
               (session (and (boundp 'mevedel--data-buffer)
                             mevedel--data-buffer
                             (buffer-live-p mevedel--data-buffer)
                             (buffer-local-value 'mevedel--session
                                                 mevedel--data-buffer)))
               (save-path (and session (mevedel-session-save-path session)))
               (rel-path (and entry (plist-get entry :path))))
          (when (and entry save-path
                     (mevedel-session-persistence--validate-transcript-path
                      rel-path save-path))
            (make-text-button
             id-start id-end
             'face 'link
             'follow-link t
             'help-echo
             (format "Open transcript for %s [%s]" id
                     (or (plist-get entry :status) "?"))
             'action
             (lambda (_btn)
               (mevedel-view-open-agent-transcript id)))))))))

(provide 'mevedel-view)

;;; mevedel-view.el ends here
