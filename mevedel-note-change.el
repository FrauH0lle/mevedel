;;; mevedel-note-change.el -- Prepared note changes -*- lexical-binding: t -*-

;;; Commentary:

;; Note-session source patches settle immediately as durable proposals.  User
;; commands later apply them through the normal patch transaction and review.

;;; Code:

(require 'cl-lib)
(require 'mevedel-change)
(require 'mevedel-notes)
(require 'mevedel-persistence)
(require 'mevedel-tool-patch)

(defun mevedel-note-prepare-change (proposal changes data-buffer)
  "Hold PROPOSAL and CHANGES for DATA-BUFFER's notes without writing source.
Return an ordinary tool result immediately; no tool callback is retained."
  (let* ((session (buffer-local-value 'mevedel--session data-buffer))
         (workspace (mevedel-session-workspace session))
         (id (mevedel-session-session-id session))
         (notes (cl-remove-if-not
                 (lambda (entry)
                   (equal id (plist-get (plist-get entry :properties)
                                        'mevedel-note-session-id)))
                 (mevedel-notes--entries workspace)))
         (record (list :id (mevedel--create-uuid)
                       :proposal (copy-tree proposal)
                       :changes (copy-tree changes)
                       :before (mapcar (lambda (change)
                                         (mevedel-tool-patch--snapshot
                                          (plist-get change :path)))
                                       changes))))
    (unless notes (error "No open note owns this conversation"))
    (dolist (entry notes)
      (let ((note (mevedel-notes--resolve (plist-get entry :uuid) workspace)))
        (overlay-put note 'mevedel-note-prepared-change record)
        (overlay-put note 'mevedel-note-attention-state 'ready)
        (mevedel-notes--render note)))
    (mevedel-instruction-store-touch workspace)
    (mevedel-instruction-store-flush t)
    (let* ((result (mevedel-tool-patch-result proposal changes))
           (text (plist-get result :result)))
      (plist-put result :result
                 (concat "Prepared change ready; source files are unchanged. "
                         "The user can apply, review, or re-ask later.\n\n" text))
      result)))

(defun mevedel-note-change--note (&optional note)
  "Return live NOTE, the source note at point, or this conversation's note."
  (let* ((data (if (buffer-live-p mevedel--data-buffer) mevedel--data-buffer (current-buffer)))
         (session (buffer-local-value 'mevedel--session data))
         (workspace (and session (mevedel-session-workspace session))))
    (setq note
          (or note
              (mevedel--highest-priority-instruction (mevedel--instructions-at (point) 'note))
              (when session
                (when-let* ((entry (cl-find-if
                                   (lambda (entry)
                                     (equal (mevedel-session-session-id session)
                                            (plist-get (plist-get entry :properties)
                                                       'mevedel-note-session-id)))
                                   (mevedel-notes--entries workspace))))
                  (mevedel-notes--resolve (plist-get entry :uuid) workspace)))))
    (unless (and (overlayp note) (overlay-buffer note)
                 (eq (overlay-get note 'mevedel-instruction-type) 'note))
      (user-error "No live note here"))
    note))

(defun mevedel-note-change--record (note)
  "Return NOTE's waiting change, or refuse."
  (or (overlay-get note 'mevedel-note-prepared-change)
      (user-error "No prepared change for this note")))

(defun mevedel-note-change--plan (note)
  "Prepare NOTE's waiting changes against current source, without writes."
  (let ((record (mevedel-note-change--record note))
        changes conflicts)
    (dolist (change (plist-get record :changes))
      (let* ((path (plist-get change :path))
             (before (cl-find path (plist-get record :before)
                              :key (lambda (snapshot) (plist-get snapshot :path)) :test #'equal))
             (after (list :path path :exists (eq (plist-get change :action) 'write)
                          :bytes (when (eq (plist-get change :action) 'write)
                                   (or (plist-get change :bytes)
                                       (encode-coding-string (plist-get change :content) 'utf-8-unix)))
                          :mode (when (eq (plist-get change :action) 'write)
                                  (or (plist-get change :mode) (plist-get before :mode)
                                      (default-file-modes)))))
             (current (mevedel-tool-patch--snapshot path))
             (prepared (mevedel-change-prepare before after current)))
        (setq conflicts (append conflicts (plist-get prepared :conflicts)))
        (unless (plist-get prepared :conflicts)
          (let* ((result (plist-get prepared :after))
                 (buffer (find-buffer-visiting path))
                 (coding (if buffer (buffer-local-value 'buffer-file-coding-system buffer)
                           (if (plist-get current :exists)
                               (cdr (mevedel-tool-patch--read-file path t)) 'utf-8-unix))))
            (when (and buffer (buffer-modified-p buffer))
              (push (list :path path :start-line 1 :end-line 1) conflicts))
            (push (list :path path :before current
                        :action (if (plist-get result :exists) 'write 'delete)
                        :bytes (plist-get result :bytes)
                        :content (and (plist-get result :exists)
                                      (decode-coding-string (plist-get result :bytes) coding))
                        :mode (plist-get result :mode)) changes)))))
    (when conflicts
      (overlay-put note 'mevedel-note-attention-state 'stale)
      (mevedel-notes--render note)
      (user-error "Change stale at %s; re-ask on current code or merge"
                  (mapconcat (lambda (conflict)
                               (format "%s:%d-%d" (plist-get conflict :path)
                                       (plist-get conflict :start-line)
                                       (plist-get conflict :end-line))) conflicts ", ")))
    (nreverse changes)))

(defun mevedel-note-change--finish (note record)
  "Clear waiting RECORD from NOTE and its batch siblings after application."
  (let ((workspace (mevedel--instruction-buffer-workspace (overlay-buffer note))))
    (dolist (entry (mevedel-notes--entries workspace))
      (when (equal (plist-get record :id)
                   (plist-get (plist-get (plist-get entry :properties)
                                        'mevedel-note-prepared-change) :id))
        (let ((item (mevedel-notes--resolve (plist-get entry :uuid) workspace)))
          (overlay-put item 'mevedel-note-prepared-change nil)
          (overlay-put item 'mevedel-note-attention-state nil)
          (mevedel-notes--render item))))
    (mevedel-instruction-store-touch workspace)))

;;;###autoload
(defun mevedel-note-apply-change (&optional note)
  "Apply NOTE's prepared change as one undo step, refusing newer source edits."
  (interactive)
  (setq note (mevedel-note-change--note note))
  (let ((workspace (mevedel--instruction-buffer-workspace (overlay-buffer note))))
    (mevedel-with-diagnostic-errors (mevedel-diagnostic-scope workspace)
      (let ((record (mevedel-note-change--record note))
            (changes (mevedel-note-change--plan note)))
        (dolist (change changes)
          (when-let* ((buffer (find-buffer-visiting (plist-get change :path))))
            (with-current-buffer buffer (undo-boundary))))
        (mevedel-tool-patch-commit changes)
        (dolist (change changes)
          (when-let* ((buffer (find-buffer-visiting (plist-get change :path))))
            (with-current-buffer buffer (undo-boundary))))
        (mevedel-note-change--finish note record)
        (message "mevedel: change applied; mark the note done when ready")))))

(provide 'mevedel-note-change)
;;; mevedel-note-change.el ends here
