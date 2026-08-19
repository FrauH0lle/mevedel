;;; mevedel-queue.el -- Shared interaction entry helpers -*- lexical-binding: t -*-

;;; Commentary:

;; Private session resolution and interaction metadata shared by permission
;; prompts and Plan approval.  Each feature owns its own lifecycle.

;;; Code:

(defvar mevedel--session)
(defvar mevedel--data-buffer)
(defvar mevedel--view-buffer)

;; `mevedel-view-interaction'
(declare-function mevedel-view--interaction-unregister
                  "mevedel-view-interaction" (id))

(defvar mevedel-queue--entry-ui-metadata
  (make-hash-table :test #'eq :weakness 'key)
  "Interaction-zone metadata keyed by queue entry identity.")

(defun mevedel-queue--entry-metadata-get (entry key)
  "Return ENTRY's interaction metadata value for KEY."
  (plist-get (gethash entry mevedel-queue--entry-ui-metadata) key))

(defun mevedel-queue--entry-metadata-put (entry key value)
  "Store VALUE under KEY for ENTRY's interaction metadata."
  (let ((metadata (copy-sequence
                   (gethash entry mevedel-queue--entry-ui-metadata))))
    (setq metadata (plist-put metadata key value))
    (puthash entry metadata mevedel-queue--entry-ui-metadata)
    value))

(defun mevedel-queue--entry-metadata-remhash (entry)
  "Remove ENTRY's interaction metadata."
  (remhash entry mevedel-queue--entry-ui-metadata))

(defun mevedel-queue--current-session ()
  "Resolve the session struct for a queue operation."
  (or (and (boundp 'mevedel--session) mevedel--session)
      (and (boundp 'mevedel--data-buffer) mevedel--data-buffer
           (buffer-live-p mevedel--data-buffer)
           (buffer-local-value 'mevedel--session mevedel--data-buffer))))

(defun mevedel-queue--entry-interaction-buffer (entry)
  "Return the live interaction-zone view buffer recorded for ENTRY."
  (or (and (mevedel-queue--entry-metadata-get entry :view-buffer)
           (buffer-live-p (mevedel-queue--entry-metadata-get entry :view-buffer))
           (mevedel-queue--entry-metadata-get entry :view-buffer))
      (and (boundp 'mevedel--view-buffer)
           mevedel--view-buffer
           (buffer-live-p mevedel--view-buffer)
           mevedel--view-buffer)
      (and (boundp 'mevedel--data-buffer)
           mevedel--data-buffer
           (buffer-live-p mevedel--data-buffer)
           (let ((view (buffer-local-value 'mevedel--view-buffer
                                           mevedel--data-buffer)))
             (and view (buffer-live-p view) view)))))

(defun mevedel-queue--unregister-entry-interaction (entry)
  "Remove ENTRY's interaction-zone overlay, if it has one."
  (unwind-protect
      (when-let* ((id (mevedel-queue--entry-metadata-get entry :interaction-id))
                  (view (mevedel-queue--entry-interaction-buffer entry)))
        (with-current-buffer view
          (when (fboundp 'mevedel-view--interaction-unregister)
            (ignore-errors
              (mevedel-view--interaction-unregister id)))))
    (mevedel-queue--entry-metadata-remhash entry)))

(provide 'mevedel-queue)

;;; mevedel-queue.el ends here
