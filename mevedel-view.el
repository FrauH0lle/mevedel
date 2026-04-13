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
(defvar gptel-display-buffer-action)

;; `mevedel-structs'
(defvar mevedel--data-buffer)
(defvar mevedel--view-buffer)
(defvar mevedel--session)
(defvar mevedel--current-request)
(declare-function mevedel-session-skills "mevedel-structs" (cl-x) t)

;; `mevedel-tool-registry'
(declare-function mevedel-tool-display-string "mevedel-tool-registry" (tool-name args))

;; `mevedel-skills'
(declare-function mevedel-skills--parse-slash-line "mevedel-skills" (text))
(declare-function mevedel-skills--prepare-body "mevedel-skills" (skill arguments session))
(declare-function mevedel-session-get-skill "mevedel-skills" (session name))
(declare-function mevedel-skill-name "mevedel-skills" (cl-x) t)
(defvar mevedel-slash-commands)


;;
;;; Customization

(defface mevedel-view-separator
  '((t :inherit shadow :extend t))
  "Face for separator lines in the view buffer."
  :group 'mevedel)

(defface mevedel-view-user-header
  '((t :inherit bold))
  "Face for user message headers in the view buffer."
  :group 'mevedel)

(defface mevedel-view-assistant-header
  '((t :inherit (bold font-lock-function-name-face)))
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

(defface mevedel-view-spinner
  '((t :inherit (bold font-lock-comment-face)))
  "Face for the spinner status line."
  :group 'mevedel)


;;
;;; Buffer-locals

(defvar-local mevedel-view--input-marker nil
  "Marker separating the display region (above) from the input region (below).
Everything above this marker is read-only rendered content; everything
at or below is the user's editable input area.")

(defvar-local mevedel-view--spinner-overlay nil
  "Overlay showing status during an active request, or nil when idle.")


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

(define-derived-mode mevedel-view-mode fundamental-mode "MevView"
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
    ;; Insert initial separator and set up input marker
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (propertize "--- mevedel ---\n"
                          'read-only t
                          'keymap mevedel-view--display-map
                          'front-sticky '(read-only keymap)
                          'rear-nonsticky '(read-only keymap)
                          'face 'mevedel-view-separator))
      (setq mevedel-view--input-marker (point-marker))
      (set-marker-insertion-type mevedel-view--input-marker nil))
    ;; Install slash-command completion
    (add-hook 'completion-at-point-functions
              #'mevedel-view-slash-capf nil t)
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
Clears `mevedel--view-buffer' on the associated data buffer."
  (when-let* ((db mevedel--data-buffer)
              (_ (buffer-live-p db)))
    (with-current-buffer db
      (setq mevedel--view-buffer nil))))

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
;;; Spinner

(defun mevedel-view--start-spinner (&optional status)
  "Show a spinner overlay with STATUS text in the view buffer.
STATUS defaults to \"Thinking...\"."
  (mevedel-view--stop-spinner)
  (save-excursion
    (goto-char mevedel-view--input-marker)
    (let* ((inhibit-read-only t)
           (text (propertize (concat (or status "Thinking...") "\n")
                             'face 'mevedel-view-spinner
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
                              'face 'mevedel-view-spinner
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

(defun mevedel-view--extract-segments (start end)
  "Extract segments from the data buffer between START and END.
Returns a list of (TYPE DATA-START DATA-END) where TYPE is one of
`user', `response', `tool', or `ignore'.  Walks forward through
text property changes on the `gptel' property."
  (let (segments seg-start seg-type)
    (save-excursion
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
                (format "%s: %s (%d lines)"
                        (or name "Tool") primary-arg result-lines)
              (format "%s (%d lines)" (or name "Tool") result-lines)))
        (error
         ;; Fallback: show truncated raw text
         (truncate-string-to-width
          (replace-regexp-in-string "[\n\r]+" " " text)
          60 nil nil "..."))))))


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
          (format "Thinking... (%d lines)" (length lines))
        ""))))


;;
;;; Rendering

(defun mevedel-view--render-response (start end)
  "Render the data buffer region [START, END] into the view buffer.
Intended for use as a `gptel-post-response-functions' hook.
Operates on the data buffer (current buffer when the hook fires),
renders into the associated view buffer."
  (when-let* ((view-buf (buffer-local-value 'mevedel--view-buffer
                                            (current-buffer)))
              (_ (buffer-live-p view-buf)))
    (let* ((data-buf (current-buffer))
           (segments (mevedel-view--extract-segments start end))
           (turns (mevedel-view--group-into-turns segments)))
      (with-current-buffer view-buf
        ;; Stop the spinner
        (mevedel-view--stop-spinner)
        ;; Render each turn
        (dolist (turn turns)
          (mevedel-view--render-turn turn data-buf))))))

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
            ;; Trailing separator
            (insert (propertize "\n" 'face 'mevedel-view-separator))
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
                    (setq pos next))))))
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
          (let ((trimmed (string-trim text)))
            (unless (string-empty-p trimmed)
              (push trimmed parts)))))
      (string-join (nreverse parts) "\n"))))

(defun mevedel-view--render-user-turn (segments data-buf)
  "Render user SEGMENTS from DATA-BUF."
  (insert (propertize "You\n" 'face 'mevedel-view-user-header))
  (let ((text (mevedel-view--user-turn-text segments data-buf)))
    (unless (string-empty-p text)
      (insert text)))
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
                            'face 'mevedel-view-thinking-summary
                            'mevedel-view-type 'thinking-summary
                            'mevedel-view-collapsed t
                            'mevedel-view-source (cons first-start last-end)))))))

(defun mevedel-view--render-assistant-turn (segments data-buf)
  "Render assistant SEGMENTS from DATA-BUF.
Response text is shown inline, tool calls as collapsed one-liners,
reasoning blocks as collapsed summaries.  Adjacent thinking segments
are merged into a single summary."
  (insert (propertize "Assistant\n" 'face 'mevedel-view-assistant-header))
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
                     (let ((start (point)))
                       (insert text "\n")
                       (put-text-property start (point) 'mevedel-view-source
                                          (cons seg-start seg-end)))))))))
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
Otherwise render each as an individual one-liner."
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
               (summary (format "Reading %d files..." count)))
          (insert (propertize (concat summary "\n")
                              'face 'mevedel-view-tool-summary
                              'mevedel-view-type 'tool-group
                              'mevedel-view-collapsed t
                              'mevedel-view-source (cons first-start last-end))))
      ;; Individual one-liners
      (dolist (seg tool-segments)
        (let* ((seg-start (cadr seg))
               (seg-end (caddr seg))
               (summary (mevedel-view--tool-one-liner data-buf seg-start seg-end)))
          (insert (propertize (concat summary "\n")
                              'face 'mevedel-view-tool-summary
                              'mevedel-view-type 'tool-summary
                              'mevedel-view-collapsed t
                              'mevedel-view-source (cons seg-start seg-end))))))))

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

(defun mevedel-view-toggle-section ()
  "Toggle expand/collapse of the section at point."
  (interactive)
  (let ((collapsed (get-text-property (point) 'mevedel-view-collapsed))
        (source (get-text-property (point) 'mevedel-view-source))
        (vtype (get-text-property (point) 'mevedel-view-type)))
    (unless source
      (user-error "No collapsible section at point"))
    (if collapsed
        (mevedel-view--expand-section source vtype)
      (mevedel-view--collapse-section source vtype))))

(defun mevedel-view--section-bounds ()
  "Return (START . END) of the current section at point.
A section is a contiguous region with the same `mevedel-view-source'."
  (let ((source (get-text-property (point) 'mevedel-view-source)))
    (when source
      (let ((start (or (previous-single-property-change
                        (point) 'mevedel-view-source)
                       (point-min)))
            (end (or (next-single-property-change
                      (point) 'mevedel-view-source)
                     (point-max))))
        ;; Adjust start: previous-single-property-change returns the
        ;; position where the property changes, which is one past the
        ;; end of the preceding section.
        (when (and (> start (point-min))
                   (not (equal (get-text-property start 'mevedel-view-source)
                               source)))
          (setq start (next-single-property-change start 'mevedel-view-source)))
        (cons start end)))))

(defun mevedel-view--expand-section (source vtype)
  "Expand a collapsed section with SOURCE coordinates and VTYPE."
  (let* ((bounds (mevedel-view--section-bounds))
         (data-buf mevedel--data-buffer)
         (data-start (car source))
         (data-end (cdr source)))
    (when (and bounds data-buf (buffer-live-p data-buf))
      (let ((inhibit-read-only t)
            (view-start (car bounds))
            (view-end (cdr bounds)))
        (save-excursion
          (goto-char view-start)
          (delete-region view-start view-end)
          (let ((text (with-current-buffer data-buf
                        (buffer-substring-no-properties data-start data-end))))
            ;; Clean org scaffolding from reasoning blocks
            (when (eq vtype 'thinking-summary)
              (setq text (string-trim (mevedel-view--clean-reasoning-text text))))
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
                                   mevedel-view-collapsed nil))))))))

(defun mevedel-view--collapse-section (source vtype)
  "Collapse an expanded section back to a one-liner.
SOURCE is the data buffer coordinates, VTYPE the section type."
  (let* ((bounds (mevedel-view--section-bounds))
         (data-buf mevedel--data-buffer)
         (data-start (car source))
         (data-end (cdr source)))
    (when (and bounds data-buf (buffer-live-p data-buf))
      (let ((inhibit-read-only t)
            (view-start (car bounds))
            (view-end (cdr bounds))
            (summary
             (pcase vtype
               ('tool-summary
                (mevedel-view--tool-one-liner data-buf data-start data-end))
               ('tool-group
                (format "Reading files..."))
               ('thinking-summary
                (mevedel-view--thinking-summary data-buf data-start data-end))
               (_
                ;; Response text or unknown: first-line preview
                (with-current-buffer data-buf
                  (let ((text (string-trim
                               (buffer-substring-no-properties
                                data-start (min data-end (+ data-start 200))))))
                    (truncate-string-to-width
                     (car (split-string text "\n")) 80 nil nil "...")))))))
        (save-excursion
          (goto-char view-start)
          (delete-region view-start view-end)
          (let ((face (pcase vtype
                        ((or 'tool-summary 'tool-group) 'mevedel-view-tool-summary)
                        ('thinking-summary 'mevedel-view-thinking-summary)
                        (_ 'mevedel-view-separator))))
            (insert (propertize (concat summary "\n")
                                'face face
                                'mevedel-view-type vtype
                                'mevedel-view-collapsed t
                                'mevedel-view-source source
                                'read-only t
                                'keymap mevedel-view--display-map
                                'front-sticky '(read-only keymap)
                                'rear-nonsticky '(read-only keymap)))))))))


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
    ;; Re-insert header
    (save-excursion
      (goto-char (point-min))
      (insert (propertize "--- mevedel ---\n"
                          'read-only t
                          'keymap mevedel-view--display-map
                          'front-sticky '(read-only keymap)
                          'rear-nonsticky '(read-only keymap)
                          'face 'mevedel-view-separator)))
    ;; Render all content from data buffer
    (with-current-buffer data-buf
      ;; Skip compacted region at the start.  After compaction the data
      ;; buffer has: [ignore+shadow old content] [ignore separator]
      ;; [ignore #+begin_summary] [nil summary text] [ignore
      ;; #+end_summary] [live content].  Skip past all of it.
      (let ((scan-start (point-min)))
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
                                      'face 'mevedel-view-separator))
                (set-marker-insertion-type mevedel-view--input-marker nil)))))
        (let* ((segments (mevedel-view--extract-segments
                          scan-start (point-max)))
               (turns (mevedel-view--group-into-turns segments)))
          (with-current-buffer (buffer-local-value 'mevedel--view-buffer data-buf)
            (dolist (turn turns)
              (mevedel-view--render-turn turn data-buf))))))))


;;
;;; Input forwarding

(defun mevedel-view--insert-user-message (text)
  "Render TEXT as a user message in the display region.
Inserts above `mevedel-view--input-marker' with read-only protection."
  (save-excursion
    (goto-char mevedel-view--input-marker)
    (set-marker-insertion-type mevedel-view--input-marker t)
    (unwind-protect
        (let ((inhibit-read-only t)
              (start (point)))
          (insert (propertize "You\n" 'face 'mevedel-view-user-header))
          (insert text)
          (unless (eq (char-before) ?\n)
            (insert "\n"))
          (insert (propertize "\n" 'face 'mevedel-view-separator))
          (add-text-properties start (point)
                               `(read-only t
                                 keymap ,mevedel-view--display-map
                                 front-sticky (read-only keymap)
                                 rear-nonsticky (read-only keymap)
                                 mevedel-view-type user)))
      (set-marker-insertion-type mevedel-view--input-marker nil))))

(defun mevedel-view--input-text ()
  "Return the user's input text from the input region.
Returns the string below `mevedel-view--input-marker', trimmed."
  (let ((text (buffer-substring-no-properties
               mevedel-view--input-marker (point-max))))
    (string-trim text)))

(defun mevedel-view--clear-input ()
  "Clear the input region below `mevedel-view--input-marker'."
  (delete-region mevedel-view--input-marker (point-max)))

(defun mevedel-view-slash-capf ()
  "Completion-at-point for `/command' prefixes in the view input area.
Offers local slash commands and session skills when point follows
a `/' at the beginning of a line in the input region."
  (when (and mevedel--data-buffer
             (buffer-live-p mevedel--data-buffer)
             (>= (point) mevedel-view--input-marker)
             (save-excursion
               (skip-chars-backward "A-Za-z0-9_-")
               (and (eq (char-before) ?/)
                    (save-excursion
                      (backward-char)
                      (bolp)))))
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

(defun mevedel-view-send ()
  "Send the current input to the LLM via the data buffer.
Extracts text from the input region, renders it in the display area,
forwards it to the data buffer, and calls `gptel-send'.
When the input starts with a `/command', dispatches it as a slash
command or skill instead of forwarding to the LLM."
  (interactive)
  (unless mevedel--data-buffer
    (user-error "No data buffer associated with this view"))
  (unless (buffer-live-p mevedel--data-buffer)
    (user-error "Data buffer has been killed"))
  (when (buffer-local-value 'mevedel--current-request mevedel--data-buffer)
    (user-error "A request is already active -- wait or abort first"))
  (let ((input (mevedel-view--input-text)))
    (when (string-empty-p input)
      (user-error "Nothing to send"))
    ;; Check for slash commands before forwarding.
    (let ((parsed (mevedel-skills--parse-slash-line input)))
      (if (not parsed)
          ;; Normal message -- forward to data buffer.
          (mevedel-view--forward-input input)
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
            (mevedel-view--clear-input)
            (with-current-buffer mevedel--data-buffer
              (funcall (cdr local) args)))
           (skill
            (mevedel-view--clear-input)
            (let ((body (with-current-buffer mevedel--data-buffer
                          (mevedel-skills--prepare-body
                           skill args mevedel--session))))
              ;; Show compact /skill-name in view, send expanded body
              (mevedel-view--forward-input
               (or body (format "Skill '%s' has no body." name))
               (concat "/" name (when args (concat " " args))))))
           (t
            (message "Unknown slash command: /%s" name)))))))

  ;; Ensure point ends up in the input area.
  (goto-char (point-max)))

(defun mevedel-view--forward-input (input &optional display-text)
  "Render INPUT in the display area, forward to the data buffer, and send.
Helper for `mevedel-view-send'.  When DISPLAY-TEXT is non-nil, show
that in the view instead of INPUT (e.g., compact skill invocation)."
  ;; Render the user's message in the view
  (mevedel-view--insert-user-message (or display-text input))
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

(provide 'mevedel-view)

;;; mevedel-view.el ends here
