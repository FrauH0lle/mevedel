;;; niwa-queue.el -- Shared interaction entry helpers -*- lexical-binding: t -*-

;;; Commentary:

;; Private session resolution shared by execution, permission prompts and
;; Plan approval, plus interaction metadata for queued prompts.  Each feature
;; owns its own lifecycle.

;;; Code:

(defvar niwa--session)
(defvar niwa--data-buffer)
(defvar niwa--view-buffer)

;; `niwa-view-interaction'
(declare-function niwa-view--interaction-unregister
                  "niwa-view-interaction" (id))

(defvar niwa-queue--entry-ui-metadata
  (make-hash-table :test #'eq :weakness 'key)
  "Interaction-zone metadata keyed by queue entry identity.")

(defun niwa-queue--entry-metadata-get (entry key)
  "Return ENTRY's interaction metadata value for KEY."
  (plist-get (gethash entry niwa-queue--entry-ui-metadata) key))

(defun niwa-queue--entry-metadata-put (entry key value)
  "Store VALUE under KEY for ENTRY's interaction metadata."
  (let ((metadata (copy-sequence
                   (gethash entry niwa-queue--entry-ui-metadata))))
    (setq metadata (plist-put metadata key value))
    (puthash entry metadata niwa-queue--entry-ui-metadata)
    value))

(defun niwa-queue--entry-metadata-remhash (entry)
  "Remove ENTRY's interaction metadata."
  (remhash entry niwa-queue--entry-ui-metadata))

(defun niwa-queue--current-session ()
  "Resolve the session struct for a queue operation."
  (or (and (boundp 'niwa--session) niwa--session)
      (and (boundp 'niwa--data-buffer) niwa--data-buffer
           (buffer-live-p niwa--data-buffer)
           (buffer-local-value 'niwa--session niwa--data-buffer))))

(defun niwa-queue--entry-interaction-buffer (entry)
  "Return the live interaction-zone view buffer recorded for ENTRY."
  (or (and (niwa-queue--entry-metadata-get entry :view-buffer)
           (buffer-live-p (niwa-queue--entry-metadata-get entry :view-buffer))
           (niwa-queue--entry-metadata-get entry :view-buffer))
      (and (boundp 'niwa--view-buffer)
           niwa--view-buffer
           (buffer-live-p niwa--view-buffer)
           niwa--view-buffer)
      (and (boundp 'niwa--data-buffer)
           niwa--data-buffer
           (buffer-live-p niwa--data-buffer)
           (let ((view (buffer-local-value 'niwa--view-buffer
                                           niwa--data-buffer)))
             (and view (buffer-live-p view) view)))))

(defun niwa-queue--unregister-entry-interaction (entry)
  "Remove ENTRY's interaction-zone overlay, if it has one."
  (unwind-protect
      (when-let* ((id (niwa-queue--entry-metadata-get entry :interaction-id))
                  (view (niwa-queue--entry-interaction-buffer entry)))
        (with-current-buffer view
          (when (fboundp 'niwa-view--interaction-unregister)
            (ignore-errors
              (niwa-view--interaction-unregister id)))))
    (niwa-queue--entry-metadata-remhash entry)))

(provide 'niwa-queue)

;;; niwa-queue.el ends here
