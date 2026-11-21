;;; niwa-journal-discard.el -- Durable journal omissions -*- lexical-binding: t -*-

;;; Commentary:

;; Prepares explicit omissions and completes accepted discard outcomes.  The
;; processing coordinator owns claim election; this module owns the closed
;; omission record and source-pin release.  An unreadable capture requires an
;; explicitly supplied original source directory and its matching valid pin.

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'niwa-journal-capture)

(define-error 'niwa-journal-discard-source-required
              "Capture descriptor is unreadable; provide its original source directory" 'user-error)

;; `niwa-utilities'
(declare-function niwa-diagnostic-scope "niwa-utilities" (owner))
(declare-function niwa-diagnostic-user-error "niwa-utilities" (scope format &rest args))
(autoload 'niwa-diagnostic-scope "niwa-utilities")
(autoload 'niwa-diagnostic-user-error "niwa-utilities")

(defun niwa-journal-discard-prepare (workspace id &optional original-source)
  "Freeze an explicit discard payload for WORKSPACE's capture ID.
ORIGINAL-SOURCE is required only when the source descriptor is unreadable.
A matching source pin must be present and valid before accepting the omission."
  (let* ((capture (ignore-errors (niwa-journal-capture--read workspace id)))
         (source (if capture (niwa-journal-capture--source-directory workspace capture)
                   (or original-source (signal 'niwa-journal-discard-source-required nil))))
         (remote (file-remote-p source)))
    (when (and capture original-source
               (not (equal (niwa-session-control-fs-physical-path source)
                           (niwa-session-control-fs-physical-path original-source))))
      (niwa-diagnostic-user-error (niwa-diagnostic-scope workspace) "Source directory does not match the capture's original source"))
    (when (and remote (not (equal remote (file-remote-p (niwa-workspace-root workspace)))))
      (niwa-diagnostic-user-error (niwa-diagnostic-scope workspace) "Source belongs to a different execution target"))
    (niwa-journal-pins--read (niwa-journal-pins--path source id))
    (json-serialize
     (list :class "discard" :capture-id id
           :created (niwa-session-control-fs-target-time (niwa-workspace-root workspace))
           :source-directory (file-local-name (niwa-session-control-fs-physical-path source))
           :source-kind (if capture (plist-get capture :source-kind) (if remote "target" "client"))
           :source-client (if capture (plist-get capture :source-client) (niwa-workspace-identity-client))
           :reason "Discarded explicitly by user"))))

(defun niwa-journal-discard-accepted (outcome id)
  "Decode OUTCOME's accepted omission for ID, or nil for another outcome.
Malformed cancelled payloads fail closed so corruption cannot reopen a discard."
  (when (eq 'cancelled (plist-get outcome :status))
    (let ((record (json-parse-string (plist-get outcome :payload) :object-type 'plist)))
      (when (equal "discard" (plist-get record :class))
        (unless (and (proper-list-p record) (= 14 (length record))
                     (cl-every (lambda (key) (plist-member record key))
                               '(:class :capture-id :created :source-directory :source-kind :source-client :reason))
                     (equal id (plist-get record :capture-id))
                     (niwa-journal-store-id-p id)
                     (integerp (plist-get record :created)) (> (plist-get record :created) 0)
                     (stringp (plist-get record :source-directory))
                     (file-name-absolute-p (plist-get record :source-directory))
                     (not (file-remote-p (plist-get record :source-directory)))
                     (member (plist-get record :source-kind) '("target" "client"))
                     (stringp (plist-get record :source-client))
                     (niwa-journal-store-id-p (plist-get record :source-client))
                     (equal "Discarded explicitly by user" (plist-get record :reason)))
          (error "Invalid journal discard outcome"))
        record))))

(defun niwa-journal-discard-complete (workspace record)
  "Complete accepted omission RECORD in WORKSPACE under journal ownership.
Persist the omission before releasing its pin.  Repetition is harmless, and
an interrupted release retains enough origin information for later recovery."
  (unless (niwa-session-control-fs-create-or-verify
           (niwa-journal-capture--file workspace (plist-get record :capture-id) "discard.json")
           (json-serialize record))
    (error "Conflicting journal discard record"))
  (niwa-journal-capture--retire workspace (append (list :id (plist-get record :capture-id)) record)
                                   "discarded\n")
  record)

(provide 'niwa-journal-discard)
;;; niwa-journal-discard.el ends here
