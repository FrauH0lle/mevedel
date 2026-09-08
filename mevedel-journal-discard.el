;;; mevedel-journal-discard.el -- Durable journal omissions -*- lexical-binding: t -*-

;;; Commentary:

;; Prepares explicit omissions and completes accepted discard outcomes.  The
;; processing coordinator owns claim election; this module owns the closed
;; omission record and source-pin release.  An unreadable capture requires an
;; explicitly supplied original source directory and its matching valid pin.

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'mevedel-journal-capture)

(define-error 'mevedel-journal-discard-source-required
              "Capture descriptor is unreadable; provide its original source directory" 'user-error)

(defun mevedel-journal-discard-prepare (workspace id &optional original-source)
  "Freeze an explicit discard payload for WORKSPACE's capture ID.
ORIGINAL-SOURCE is required only when the source descriptor is unreadable.
A matching source pin must be present and valid before accepting the omission."
  (let* ((capture (ignore-errors (mevedel-journal-capture--read workspace id)))
         (source (if capture (mevedel-journal-capture--source-directory workspace capture)
                   (or original-source (signal 'mevedel-journal-discard-source-required nil))))
         (remote (file-remote-p source)))
    (when (and capture original-source
               (not (equal (mevedel-session-control-fs-physical-path source)
                           (mevedel-session-control-fs-physical-path original-source))))
      (user-error "Source directory does not match the capture's original source"))
    (when (and remote (not (equal remote (file-remote-p (mevedel-workspace-root workspace)))))
      (user-error "Source belongs to a different execution target"))
    (mevedel-journal-pins--read (mevedel-journal-pins--path source id))
    (json-serialize
     (list :class "discard" :capture-id id
           :created (mevedel-session-control-fs-target-time (mevedel-workspace-root workspace))
           :source-directory (file-local-name (mevedel-session-control-fs-physical-path source))
           :source-kind (if capture (plist-get capture :source-kind) (if remote "target" "client"))
           :source-client (if capture (plist-get capture :source-client) (mevedel-workspace-identity-client))
           :reason "Discarded explicitly by user"))))

(defun mevedel-journal-discard-accepted (outcome id)
  "Decode OUTCOME's accepted omission for ID, or nil for another outcome.
Malformed cancelled payloads fail closed so corruption cannot reopen a discard."
  (when (eq 'cancelled (plist-get outcome :status))
    (let ((record (json-parse-string (plist-get outcome :payload) :object-type 'plist)))
      (when (equal "discard" (plist-get record :class))
        (unless (and (proper-list-p record) (= 14 (length record))
                     (cl-every (lambda (key) (plist-member record key))
                               '(:class :capture-id :created :source-directory :source-kind :source-client :reason))
                     (equal id (plist-get record :capture-id))
                     (mevedel-journal-store-id-p id)
                     (integerp (plist-get record :created)) (> (plist-get record :created) 0)
                     (stringp (plist-get record :source-directory))
                     (file-name-absolute-p (plist-get record :source-directory))
                     (not (file-remote-p (plist-get record :source-directory)))
                     (member (plist-get record :source-kind) '("target" "client"))
                     (stringp (plist-get record :source-client))
                     (mevedel-journal-store-id-p (plist-get record :source-client))
                     (equal "Discarded explicitly by user" (plist-get record :reason)))
          (error "Invalid journal discard outcome"))
        record))))

(defun mevedel-journal-discard-complete (workspace record)
  "Complete accepted omission RECORD in WORKSPACE under journal ownership.
Persist the omission before releasing its pin.  Repetition is harmless, and
an interrupted release retains enough origin information for later recovery."
  (unless (mevedel-session-control-fs-create-or-verify
           (mevedel-journal-capture--file workspace (plist-get record :capture-id) "discard.json")
           (json-serialize record))
    (error "Conflicting journal discard record"))
  (mevedel-journal-capture--retire workspace (append (list :id (plist-get record :capture-id)) record)
                                   "discarded\n")
  record)

(provide 'mevedel-journal-discard)
;;; mevedel-journal-discard.el ends here
