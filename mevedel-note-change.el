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

(provide 'mevedel-note-change)
;;; mevedel-note-change.el ends here
