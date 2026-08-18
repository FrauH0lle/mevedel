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


;;
;;; Customization

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

(defvar mevedel-buddy-note--scope-buffers nil
  "Buffer names the running review may annotate, or nil for no limit.")

(defvar mevedel-buddy-note--markers nil
  "Alist of (BUFFER-NAME . MARKER-ALIST) captured for the running review.

Each MARKER-ALIST maps a line number, as it stood when the request was
sent, to a marker at that line.  Markers move with the buffer, so a
note still lands on the text it was written about when the user has
typed while the request was in flight.")

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

(defun mevedel-buddy-note-capture-markers (buffer-names)
  "Capture line markers in BUFFER-NAMES for the review about to run.

A model answers with the line numbers it was shown, but the user keeps
typing while the request is in flight.  Resolving those numbers through
markers taken now means a note lands on the text it describes rather
than wherever that number points by the time the answer arrives."
  (setq mevedel-buddy-note--markers
        (delq nil
              (mapcar
               (lambda (buffer-name)
                 (when-let* ((buffer (get-buffer buffer-name))
                             ((buffer-live-p buffer)))
                   (cons buffer-name
                         (with-current-buffer buffer
                           (save-excursion
                             (save-restriction
                               (widen)
                               (goto-char (point-min))
                               (let ((line 1) markers)
                                 (while (not (eobp))
                                   (push (cons line (copy-marker (point)))
                                         markers)
                                   (setq line (1+ line))
                                   (forward-line 1))
                                 (nreverse markers))))))))
               buffer-names))))

(defun mevedel-buddy-note-release-markers ()
  "Drop the captured line markers so they stop tracking edits."
  (dolist (entry mevedel-buddy-note--markers)
    (dolist (pair (cdr entry))
      (set-marker (cdr pair) nil)))
  (setq mevedel-buddy-note--markers nil))

(defun mevedel-buddy-note--position (buffer line)
  "Return the position in BUFFER for LINE as the model saw it.

Resolves through the markers captured for the running review when one
covers LINE, and falls back to counting lines when none does."
  (or (when-let* ((markers (cdr (assoc (buffer-name buffer)
                                       mevedel-buddy-note--markers)))
                  (marker (cdr (assq line markers)))
                  ((marker-buffer marker)))
        (marker-position marker))
      (with-current-buffer buffer
        (save-excursion
          (save-restriction
            (widen)
            (goto-char (point-min))
            (forward-line (1- (max 1 line)))
            (line-beginning-position))))))


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

(defun mevedel-buddy-note--render (overlay note severity)
  "Show NOTE on OVERLAY, styled for SEVERITY."
  (overlay-put overlay 'after-string
               (propertize (concat "  " note)
                           'face (mevedel-buddy-note--face severity))))

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
      overlay)))

(defun mevedel-buddy-note--on-kill-buffer ()
  "Drop the current buffer's notes as it is killed.

Notes are ephemeral and anchored to live text, so a dead buffer must
not leave records behind that name it."
  (mevedel-buddy-note-forget-buffer (buffer-name)))


;;
;;; Tool operations

(defun mevedel-buddy-note--in-scope-p (buffer-name)
  "Return non-nil when BUFFER-NAME may be annotated by the running review."
  (or (null mevedel-buddy-note--scope-buffers)
      (member buffer-name mevedel-buddy-note--scope-buffers)))

(defun mevedel-buddy-note-add (buffer-name line note severity)
  "Attach NOTE to LINE of BUFFER-NAME at SEVERITY.

Return the new note id, or an explanatory string the model can act on."
  (cond
   ((not (mevedel-buddy-note--in-scope-p buffer-name))
    (format "Buffer %s is not in the review scope" buffer-name))
   ((not (buffer-live-p (get-buffer buffer-name)))
    (format "Unknown buffer: %s" buffer-name))
   ((not (member severity (mevedel-buddy-note--severities)))
    (format "Unknown severity: %s" severity))
   (t
    (let* ((buffer (get-buffer buffer-name))
           (id mevedel-buddy-note--next-id)
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
      id))))

(defun mevedel-buddy-note-update (id note)
  "Replace the text of note ID with NOTE, leaving it where it is."
  (if-let* ((record (mevedel-buddy-note--find id)))
      (let ((overlay (plist-get record :overlay)))
        (plist-put record :note note)
        (plist-put record :review-needed nil)
        (when (and (overlayp overlay) (overlay-buffer overlay))
          (mevedel-buddy-note--render
           overlay note (plist-get record :severity)))
        (format "Updated note %s" (plist-get record :id)))
    (format "Unknown note: %s" id)))

(defun mevedel-buddy-note-remove (id)
  "Retract note ID."
  (if-let* ((record (mevedel-buddy-note--find id)))
      (progn
        (mevedel-buddy-note--delete record)
        (format "Removed note %s" (plist-get record :id)))
    (format "Unknown note: %s" id)))

(defun mevedel-buddy-note--delete (record)
  "Delete RECORD's overlay and drop it from the note set."
  (let ((overlay (plist-get record :overlay)))
    (when (overlayp overlay) (delete-overlay overlay)))
  (setq mevedel-buddy-note--notes (delq record mevedel-buddy-note--notes)))

(defun mevedel-buddy-note-dismiss (id &optional reason)
  "Dismiss note ID for REASON, keeping the record so it is not repeated."
  (when-let* ((record (mevedel-buddy-note--find id)))
    (let ((overlay (plist-get record :overlay)))
      (plist-put record :line (mevedel-buddy-note--line record))
      (when (overlayp overlay) (delete-overlay overlay))
      (plist-put record :overlay nil)
      (plist-put record :status 'dismissed)
      (plist-put record :dismissed-reason (or reason "removed")))
    record))

(defun mevedel-buddy-note-clear-all ()
  "Discard every note and its overlay."
  (dolist (record (copy-sequence mevedel-buddy-note--notes))
    (let ((overlay (plist-get record :overlay)))
      (when (overlayp overlay) (delete-overlay overlay))))
  (setq mevedel-buddy-note--notes nil
        mevedel-buddy-note--next-id 1)
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

Active and dismissed notes are both described.  Dismissed ones are
labelled so the model can see what the user already rejected instead of
raising it again."
  (let ((notes (seq-take mevedel-buddy-note--notes
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

(provide 'mevedel-buddy-note)
;;; mevedel-buddy-note.el ends here
