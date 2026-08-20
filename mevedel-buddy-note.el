;;; mevedel-buddy-note.el -- Ephemeral model annotations on source lines -*- lexical-binding: t -*-

;;; Commentary:

;; Buddy notes are short, model-authored remarks attached to a source
;; line.  They exist only in memory: nothing here writes a file, touches
;; persistent memory, or enters a session transcript.  Closing Emacs
;; discards every note, and the next review re-derives whatever is still
;; true from the buffers themselves.
;;
;; A note is NOT an instruction.  Instruction enumeration selects on the
;; `mevedel-instruction' overlay property, so a note that never sets it
;; is invisible to instruction navigation, tinting, persistence, and
;; deletion.  Nothing in this file may set that property.
;;
;; The model addresses notes by id, so it revises and retracts its own
;; earlier remarks instead of restating them.  Dismissed notes stay in
;; the serialized set, labelled, so the same rejected advice is not
;; raised twice.  That is the whole mute list, and it needs no file.

;;; Code:

(eval-when-compile
  (require 'cl-lib))
(require 'seq)

;; `gptel'
(declare-function gptel-make-tool "ext:gptel-request" (&rest slots))


;;
;;; Customization

(defcustom mevedel-buddy-note-width 72
  "Column budget for laying out a note.

Deliberately a fixed budget rather than the window width.  An overlay's
`after-string' is shared by every window showing its buffer, so a layout
fitted to one window would be wrong in another and wrong again after a
split."
  :type 'natnum
  :group 'mevedel-buddy)

(defcustom mevedel-buddy-note-current-line-style 'below
  "How to show a note on the line point is on.

`below' lays the whole note out on its own lines under the code, `eol'
appends it after the code and shortens it to fit, and nil hides it."
  :type '(choice (const :tag "Full text below the code" below)
                 (const :tag "Shortened after the code" eol)
                 (const :tag "Hidden" nil))
  :group 'mevedel-buddy)

(defcustom mevedel-buddy-note-other-lines-style 'eol
  "How to show a note on any line point is not on.

Defaults to `eol' so that at most one note is ever laid out in full: the
one being read.  Set it to nil to annotate only the line at point."
  :type '(choice (const :tag "Shortened after the code" eol)
                 (const :tag "Full text below the code" below)
                 (const :tag "Hidden" nil))
  :group 'mevedel-buddy)

(defcustom mevedel-buddy-note-serialize-limit 40
  "How many notes at most are described to the model in one request.

Every request carries the note set and dismissed notes are never
purged, so an unbounded set would make a background feature's
per-request cost grow all session.  The newest notes are kept."
  :type 'natnum
  :group 'mevedel-buddy)

(defface mevedel-buddy-note-face
  '((t :slant italic))
  "Base face for Buddy note overlays."
  :group 'mevedel-buddy)

(defface mevedel-buddy-note-trivial-face
  '((t :inherit (mevedel-buddy-note-face shadow)))
  "Face for trivial Buddy notes."
  :group 'mevedel-buddy)

(defface mevedel-buddy-note-significant-face
  '((t :inherit (mevedel-buddy-note-face warning)))
  "Face for significant Buddy notes."
  :group 'mevedel-buddy)

(defface mevedel-buddy-note-critical-face
  '((t :inherit (mevedel-buddy-note-face error) :weight bold))
  "Face for critical Buddy notes."
  :group 'mevedel-buddy)


;;
;;; State

(defvar mevedel-buddy-note--notes nil
  "Every Buddy note this Emacs session, most recent first.

Each note is a plist with `:id', `:buffer', `:line', `:note',
`:severity', `:status', `:review-needed', and `:overlay'.")

(defvar mevedel-buddy-note--next-id 1
  "Id to assign to the next Buddy note.")

(defvar-local mevedel-buddy-note--laid-out-line nil
  "Start position of the line this buffer's notes were last laid out for.")

(defvar mevedel-buddy-note--scope-buffers nil
  "Buffer names the running review may touch.

Nil means no review is running, and denies everything.  A tool call can
still arrive after a review is abandoned or times out, and no scope
must not then mean every buffer in Emacs.")

(defvar mevedel-buddy-note--markers nil
  "Alist of captured line boundaries for the running review.

Each buffer entry maps a shown line number to advancing start and
non-advancing end markers.  The boundaries distinguish insertion and
replacement from deletion.")

(defun mevedel-buddy-note--severities ()
  "Return the accepted severity names, least to most severe."
  '("trivial" "significant" "critical"))

(defun mevedel-buddy-note--face (severity)
  "Return the overlay face for SEVERITY."
  (pcase severity
    ("trivial" 'mevedel-buddy-note-trivial-face)
    ("critical" 'mevedel-buddy-note-critical-face)
    (_ 'mevedel-buddy-note-significant-face)))

(defun mevedel-buddy-note--find (id)
  "Return the note with ID, or nil."
  (let ((id (if (stringp id) (string-to-number id) id)))
    (seq-find (lambda (note) (equal (plist-get note :id) id))
              mevedel-buddy-note--notes)))

(defun mevedel-buddy-note--line (note)
  "Return NOTE's current line number, following its overlay when live."
  (let ((overlay (plist-get note :overlay)))
    (if (and (overlayp overlay) (overlay-buffer overlay))
        (with-current-buffer (overlay-buffer overlay)
          (line-number-at-pos (overlay-start overlay)))
      (plist-get note :line))))


;;
;;; Marker capture

(defun mevedel-buddy-note-capture-markers (shown-lines)
  "Capture markers for SHOWN-LINES, an alist of (BUFFER-NAME . LINES).

A model answers with the line numbers it was shown, but the user keeps
typing while the request is in flight.  Resolving those numbers through
markers taken now means a note lands on the text it describes rather
than wherever that number points by the time the answer arrives.

Only lines the model was shown are marked.  Emacs walks a buffer marker
list on every insertion, so marking every line of a large file would
make typing lag for as long as the request runs."
  (mevedel-buddy-note-release-markers)
  (setq mevedel-buddy-note--markers
        (delq nil
              (mapcar
               (lambda (entry)
                 (when-let* ((buffer (get-buffer (car entry)))
                             ((buffer-live-p buffer)))
                   (cons (car entry)
                         (with-current-buffer buffer
                           (save-excursion
                             (save-restriction
                               (widen)
                               (mapcar
                                (lambda (line)
                                  (goto-char (point-min))
                                  (forward-line (1- (max 1 line)))
                                  (let ((start (point))
                                        (end (line-beginning-position 2)))
                                    (list line
                                          (copy-marker start t)
                                          (copy-marker end))))
                                (cdr entry))))))))
               shown-lines))))

(defun mevedel-buddy-note-release-markers ()
  "Drop the captured line markers so they stop tracking edits."
  (dolist (entry mevedel-buddy-note--markers)
    (dolist (pair (cdr entry))
      (set-marker (nth 1 pair) nil)
      (set-marker (nth 2 pair) nil)))
  (setq mevedel-buddy-note--markers nil))

(defun mevedel-buddy-note--position (buffer line)
  "Return the position in BUFFER for LINE as the model saw it.

Return nil unless a live marker captured for the running review covers
LINE.  Raw line counting is not annotation authority."
  (when-let* ((markers (cdr (assoc (buffer-name buffer)
                                   mevedel-buddy-note--markers)))
              (entry (and (integerp line) (assq line markers)))
              (start (nth 1 entry))
              (end (nth 2 entry))
              ((eq (marker-buffer start) buffer))
              ((eq (marker-buffer end) buffer))
              (start-pos (marker-position start))
              (end-pos (marker-position end))
              ((/= start-pos end-pos)))
    (min start-pos end-pos)))


;;
;;; Overlays

(defun mevedel-buddy-note--modification-hook (overlay after &rest _args)
  "Mark OVERLAY's note for review once its line has changed.

AFTER distinguishes the notification made after the change from the one
made before it."
  (when after
    (when-let* ((note (seq-find (lambda (candidate)
                                  (eq (plist-get candidate :overlay) overlay))
                                mevedel-buddy-note--notes)))
      (plist-put note :review-needed t))))

;; Notes are laid out to a fixed column budget, never to the window width.
;; Truncating a note is only acceptable while its full text is one cursor
;; move away, which is why the line at point and every other line get
;; different styles: exactly one note is ever laid out in full.

(defun mevedel-buddy-note--wrap (text width)
  "Return TEXT filled to WIDTH columns as a list of lines."
  (with-temp-buffer
    (let ((fill-column (max 20 width))
          (adaptive-fill-mode nil)
          (fill-prefix nil))
      (insert text)
      (fill-region (point-min) (point-max)))
    (split-string (buffer-string) "\n")))

(defun mevedel-buddy-note--eol-string (note severity column)
  "Return NOTE for display after code ending at COLUMN, styled by SEVERITY.

Shortened to what is left of the budget, because the full text is
available by moving point onto the line."
  (let* ((room (max 20 (- mevedel-buddy-note-width column)))
         (text (truncate-string-to-width
                (replace-regexp-in-string "[ \t\n]+" " " note)
                room nil nil t)))
    (propertize (concat "  " text)
                'face (mevedel-buddy-note--face severity))))

(defun mevedel-buddy-note--below-string (note severity indent)
  "Return NOTE laid out under the code, indented to INDENT and faced by SEVERITY."
  (let* ((prefix (make-string indent ?\s))
         (lines (mevedel-buddy-note--wrap
                 note (- mevedel-buddy-note-width indent))))
    (propertize
     (concat "\n"
             (mapconcat (lambda (line) (concat prefix line)) lines "\n"))
     'face (mevedel-buddy-note--face severity))))

(defun mevedel-buddy-note--style-for (overlay)
  "Return the display style to use for OVERLAY right now.

Whether point shares the note's line is answered by comparing positions
rather than line numbers.  `line-number-at-pos' counts newlines from
`point-min' on every call, and this runs once per note on every command
that moves point to another line."
  (if (and (overlay-buffer overlay)
           (eq (overlay-buffer overlay) (current-buffer))
           (<= (line-beginning-position)
               (overlay-start overlay)
               (line-end-position)))
      mevedel-buddy-note-current-line-style
    mevedel-buddy-note-other-lines-style))

(defun mevedel-buddy-note--render (overlay note severity)
  "Show NOTE on OVERLAY, styled for SEVERITY in the style its line calls for.

Measuring happens in the overlay's own buffer.  `update_note' runs as a
tool call, so the buffer current at the time is the one the review was
started from, which under a workspace-wide scope is often not the buffer
holding the note."
  (when-let* ((buffer (overlay-buffer overlay)))
    (with-current-buffer buffer
      ;; The style depends on where the user's point is, so it must be
      ;; decided before `save-excursion' moves point to the note.
      (let ((style (mevedel-buddy-note--style-for overlay)))
        (overlay-put
         overlay 'after-string
         (save-excursion
           (goto-char (overlay-start overlay))
           (pcase style
             ('eol
              (mevedel-buddy-note--eol-string
               note severity
               ;; The display column, not the character count: a tab is
               ;; one character and eight columns, and counting it as one
               ;; over-estimates the room left and overflows the budget
               ;; the fixed width exists to hold.
               (progn (goto-char (line-end-position)) (current-column))))
             ('below
              (mevedel-buddy-note--below-string
               note severity
               (progn (back-to-indentation) (current-column))))
             (_ nil))))))))

(defun mevedel-buddy-note--relayout ()
  "Lay out this buffer's notes again after point changed line.

Runs from `post-command-hook', so it does nothing at all unless the line
number actually changed."
  (let ((line (line-beginning-position)))
    (unless (eql line mevedel-buddy-note--laid-out-line)
      (setq mevedel-buddy-note--laid-out-line line)
      (dolist (record mevedel-buddy-note--notes)
        (let ((overlay (plist-get record :overlay)))
          (when (and (overlayp overlay)
                     (eq (overlay-buffer overlay) (current-buffer)))
            (mevedel-buddy-note--render overlay
                                        (plist-get record :note)
                                        (plist-get record :severity))))))))

(defun mevedel-buddy-note--make-overlay (buffer line note severity)
  "Return a note overlay on LINE of BUFFER showing NOTE at SEVERITY."
  (with-current-buffer buffer
    (let* ((position (mevedel-buddy-note--position buffer line))
           (overlay (save-excursion
                      (goto-char position)
                      (make-overlay (line-beginning-position)
                                    (line-end-position)
                                    buffer nil nil))))
      ;; Deliberately no `mevedel-instruction' property: a note must stay
      ;; invisible to instruction enumeration.
      (overlay-put overlay 'mevedel-buddy-note t)
      (overlay-put overlay 'evaporate nil)
      (overlay-put overlay 'modification-hooks
                   (list #'mevedel-buddy-note--modification-hook))
      (overlay-put overlay 'insert-behind-hooks
                   (list #'mevedel-buddy-note--modification-hook))
      (mevedel-buddy-note--render overlay note severity)
      (add-hook 'kill-buffer-hook #'mevedel-buddy-note--on-kill-buffer nil t)
      (add-hook 'post-command-hook #'mevedel-buddy-note--relayout nil t)
      overlay)))

(defun mevedel-buddy-note--on-kill-buffer ()
  "Drop the current buffer's notes as it is killed.

Notes are ephemeral and anchored to live text, so a dead buffer must
not leave records behind that name it."
  (mevedel-buddy-note-forget-buffer (buffer-name)))


;;
;;; Tool operations

(defun mevedel-buddy-note--in-scope-p (buffer-name)
  "Return non-nil when BUFFER-NAME may be touched by the running review."
  (and mevedel-buddy-note--scope-buffers
       (member buffer-name mevedel-buddy-note--scope-buffers)
       t))

(defun mevedel-buddy-note-add (buffer-name line note severity)
  "Attach NOTE to LINE of BUFFER-NAME at SEVERITY.

Return the new note id, or an explanatory string the model can act on."
  (let ((buffer (get-buffer buffer-name)))
    (cond
     ((not (mevedel-buddy-note--in-scope-p buffer-name))
      (format "Buffer %s is not in the review scope" buffer-name))
     ((not (buffer-live-p buffer))
      (format "Unknown buffer: %s" buffer-name))
     ((not (mevedel-buddy-note--position buffer line))
      (format "Line %s was not shown in this review" line))
     ((not (member severity (mevedel-buddy-note--severities)))
      (format "Unknown severity: %s" severity))
     (t
      (let* ((id mevedel-buddy-note--next-id)
             (overlay (mevedel-buddy-note--make-overlay
                       buffer line note severity)))
        (setq mevedel-buddy-note--next-id (1+ id))
        (push (list :id id
                    :buffer buffer-name
                    :line line
                    :note note
                    :severity severity
                    :status 'active
                    :review-needed nil
                    :overlay overlay)
              mevedel-buddy-note--notes)
        id)))))

(defun mevedel-buddy-note-update (id note)
  "Replace the text of note ID with NOTE, leaving it where it is."
  (if-let* ((record (mevedel-buddy-note--find id)))
      (if (not (mevedel-buddy-note--in-scope-p (plist-get record :buffer)))
          (format "Note %s is not in the review scope" (plist-get record :id))
        (let ((overlay (plist-get record :overlay)))
          (plist-put record :note note)
          (plist-put record :review-needed nil)
          (when (and (overlayp overlay) (overlay-buffer overlay))
            (mevedel-buddy-note--render
             overlay note (plist-get record :severity)))
          (format "Updated note %s" (plist-get record :id))))
    (format "Unknown note: %s" id)))

(defun mevedel-buddy-note-remove (id)
  "Retract note ID."
  (if-let* ((record (mevedel-buddy-note--find id)))
      (if (not (mevedel-buddy-note--in-scope-p (plist-get record :buffer)))
          (format "Note %s is not in the review scope" (plist-get record :id))
        (mevedel-buddy-note--delete record)
        (format "Removed note %s" (plist-get record :id)))
    (format "Unknown note: %s" id)))

(defun mevedel-buddy-note--delete (record)
  "Delete RECORD's overlay and drop it from the note set."
  (let* ((overlay (plist-get record :overlay))
         (buffer (and (overlayp overlay) (overlay-buffer overlay))))
    (when (overlayp overlay) (delete-overlay overlay))
    (setq mevedel-buddy-note--notes (delq record mevedel-buddy-note--notes))
    (mevedel-buddy-note--sync-relayout-hook buffer)))

(defun mevedel-buddy-note--sync-relayout-hook (buffer)
  "Remove BUFFER's relayout hook once it holds no note overlays."
  (when (buffer-live-p buffer)
    (unless (seq-some
             (lambda (record)
               (let ((overlay (plist-get record :overlay)))
                 (and (overlayp overlay)
                      (eq (overlay-buffer overlay) buffer))))
             mevedel-buddy-note--notes)
      (with-current-buffer buffer
        (remove-hook 'post-command-hook #'mevedel-buddy-note--relayout t)))))

(defun mevedel-buddy-note-dismiss (id &optional reason)
  "Dismiss note ID for REASON, keeping the record so it is not repeated."
  (when-let* ((record (mevedel-buddy-note--find id)))
    (let* ((overlay (plist-get record :overlay))
           ;; Capture the buffer first: `overlay-buffer' is nil once the
           ;; overlay is deleted.
           (buffer (and (overlayp overlay) (overlay-buffer overlay))))
      (plist-put record :line (mevedel-buddy-note--line record))
      (when (overlayp overlay) (delete-overlay overlay))
      (plist-put record :overlay nil)
      (plist-put record :status 'dismissed)
      (plist-put record :dismissed-reason (or reason "removed"))
      (mevedel-buddy-note--sync-relayout-hook buffer))
    record))

(defun mevedel-buddy-note-clear-all ()
  "Discard every note and its overlay."
  (let (buffers)
    (dolist (record (copy-sequence mevedel-buddy-note--notes))
      (let ((overlay (plist-get record :overlay)))
        (when (overlayp overlay)
          (when-let* ((buffer (overlay-buffer overlay)))
            (cl-pushnew buffer buffers))
          (delete-overlay overlay))))
    (setq mevedel-buddy-note--notes nil
          mevedel-buddy-note--next-id 1)
    ;; Every buffer that held a note now holds none, so none of them still
    ;; needs the hook that lays notes out.
    (dolist (buffer buffers)
      (mevedel-buddy-note--sync-relayout-hook buffer)))
  (mevedel-buddy-note-release-markers))

(defun mevedel-buddy-note-forget-buffer (buffer-name)
  "Drop every note belonging to BUFFER-NAME."
  (dolist (record (copy-sequence mevedel-buddy-note--notes))
    (when (equal (plist-get record :buffer) buffer-name)
      (mevedel-buddy-note--delete record))))


;;
;;; Serialization for the prompt

(defun mevedel-buddy-note-serialize ()
  "Return the note set described for the model, or an empty string.

Only notes belonging to buffers in the running review scope are
described: a review of one project has no business seeing another
project buffer names, line numbers, and note text, and could otherwise
be told to maintain notes it cannot see.

Active and dismissed notes are both described.  Dismissed ones are
labelled so the model can see what the user already rejected instead of
raising it again."
  (let ((notes (seq-take (seq-filter
                          (lambda (record)
                            (mevedel-buddy-note--in-scope-p
                             (plist-get record :buffer)))
                          mevedel-buddy-note--notes)
                         mevedel-buddy-note-serialize-limit)))
    (if (null notes)
        ""
      (concat
       "\n\nNotes you have already left, newest first.  Active notes are "
       "visible to the user; dismissed notes are not, but you should still "
       "avoid repeating them.\n"
       (mapconcat
        (lambda (record)
          (format "- note_id %s [%s] %s:%s: %s%s"
                  (plist-get record :id)
                  (plist-get record :status)
                  (plist-get record :buffer)
                  (mevedel-buddy-note--line record)
                  (plist-get record :note)
                  (cond
                   ((eq (plist-get record :status) 'dismissed)
                    (format " (dismissed: %s)"
                            (or (plist-get record :dismissed-reason)
                                "unknown")))
                   ((plist-get record :review-needed)
                    (concat " (the annotated line changed since this note was"
                            " written; decide whether to update or remove it)"))
                   (t ""))))
        notes
        "\n")))))

;;
;;; Model-facing tools

(defun mevedel-buddy-note--live-tool (currentp function)
  "Wrap FUNCTION so it refuses once CURRENTP reports its review is over.

A request outlives the review that started it, and the buffer allowlist
is repopulated by whichever review runs next, so a straggler's calls
would otherwise pass the scope check and land in that review -- with
`remove_note' deleting its notes, since ids come from one counter."
  (lambda (&rest args)
    (if (funcall currentp)
        (apply function args)
      "This review has ended; its notes are no longer yours to change")))

(defun mevedel-buddy-note-tools (currentp)
  "Return the gptel tools a Buddy review is given.

CURRENTP is called with no arguments and reports whether the review that
owns these tools is still the one in flight.

These are plain gptel tools rather than registry tools: they need
argument validation but no permission check, no snapshot, and no
persistence, and nothing outside a Buddy review may call them."
  (require 'gptel)
  (list
   (gptel-make-tool
    :name "add_note"
    :function (mevedel-buddy-note--live-tool currentp #'mevedel-buddy-note-add)
    :description
    (concat "Attach one short remark to one line of a buffer, shown to the "
            "user as an overlay. Returns the note_id you need to update or "
            "remove it later.")
    :args '((:name "buffer" :type string
             :description "Name of the buffer to annotate.")
            (:name "line_number" :type integer
             :description "Line to annotate, as numbered in the diff.")
            (:name "note" :type string
             :description "The remark.  One sentence.")
            (:name "severity" :type string
             :enum ["trivial" "significant" "critical"]
             :description "How much this matters."))
    :category "buddy")
   (gptel-make-tool
    :name "update_note"
    :function (mevedel-buddy-note--live-tool currentp #'mevedel-buddy-note-update)
    :description
    (concat "Replace the text of a note you left earlier, when it is still "
            "worth making but no longer worded correctly.")
    :args '((:name "note_id" :type integer
             :description "The note_id from the note list.")
            (:name "note" :type string
             :description "Replacement text.  One sentence."))
    :category "buddy")
   (gptel-make-tool
    :name "remove_note"
    :function (mevedel-buddy-note--live-tool currentp #'mevedel-buddy-note-remove)
    :description
    (concat "Retract a note you left earlier, because the user addressed it "
            "or it no longer applies.")
    :args '((:name "note_id" :type integer
             :description "The note_id from the note list."))
    :category "buddy")))


;;
;;; Commands

(defun mevedel-buddy-note--at-point ()
  "Return the note whose overlay covers point, or nil."
  (seq-find
   (lambda (record)
     (let ((overlay (plist-get record :overlay)))
       (and (overlayp overlay)
            (eq (overlay-buffer overlay) (current-buffer))
            (<= (overlay-start overlay) (point) (overlay-end overlay)))))
   mevedel-buddy-note--notes))

;;;###autoload
(defun mevedel-buddy-dismiss-note ()
  "Dismiss the Buddy note at point.

The note stops being shown, but the model is still told it was
dismissed so it does not raise the same point again."
  (interactive)
  (if-let* ((record (mevedel-buddy-note--at-point)))
      (progn
        (mevedel-buddy-note-dismiss (plist-get record :id) "user")
        (message "mevedel: note dismissed"))
    (user-error "No buddy note at point")))

;;;###autoload
(defun mevedel-buddy-dismiss-notes ()
  "Dismiss every Buddy note visible in the current buffer."
  (interactive)
  (let ((dismissed 0)
        (buffer-name (buffer-name)))
    (dolist (record (copy-sequence mevedel-buddy-note--notes))
      (when (and (eq (plist-get record :status) 'active)
                 (equal (plist-get record :buffer) buffer-name))
        (mevedel-buddy-note-dismiss (plist-get record :id) "user")
        (setq dismissed (1+ dismissed))))
    (message "mevedel: %d note%s dismissed"
             dismissed (if (= dismissed 1) "" "s"))))


(provide 'mevedel-buddy-note)
;;; mevedel-buddy-note.el ends here
