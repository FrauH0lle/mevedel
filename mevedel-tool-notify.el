;;; mevedel-tool-notify.el --- Deliberate root notices -*- lexical-binding: t -*-

;;; Commentary:
;; The root uses its ordinary tool result as the transcript notice line.

;;; Code:

(require 'mevedel-notify)
(require 'mevedel-schedule)
(require 'mevedel-tool-registry)
(require 'mevedel-structs)
(require 'mevedel-engine)
(require 'mevedel-utilities)

;; `mevedel-session-artifacts'
(autoload 'mevedel-session-artifacts-assert-mutation-authority "mevedel-session-artifacts")

;; `mevedel-session-persistence'
(autoload 'mevedel-session-persistence-shallow-ensure-files "mevedel-session-persistence")

;; `mevedel-tools'
(declare-function mevedel-tools-visibility "mevedel-tools" (&optional owner))
(defvar mevedel-tools--current-engine)

;; `mevedel-workspace-schedule'
(declare-function mevedel-workspace-schedule-notify-receipt-file
                  "mevedel-workspace-schedule" (session))
(declare-function mevedel-workspace-schedule-record-notice "mevedel-workspace-schedule" ())
(defvar mevedel-workspace-schedule-run)

(defun mevedel-tool-notify--receipt-file (session)
  "Return the notice receipt file owned by SESSION.
An active workspace schedule run shares its schedule's file instead."
  (or (and (featurep 'mevedel-workspace-schedule)
           (mevedel-workspace-schedule-notify-receipt-file session))
      (file-name-concat
       (or (mevedel-session-save-path session)
           (mevedel-session-persistence-shallow-ensure-files session (current-buffer))
           (error "Cannot record a notice for an unsaved session"))
       "notify-receipts.json")))

(defun mevedel-tool-notify--claim (session text)
  "Record a receipt and return non-nil if SESSION may deliver TEXT now."
  (mevedel-session-artifacts-assert-mutation-authority session (current-buffer))
  (let* ((file (mevedel-tool-notify--receipt-file session))
         (receipts (condition-case nil
                       (with-temp-buffer (insert-file-contents file) (json-parse-buffer))
                     (file-missing (make-hash-table :test #'equal))))
         (key (secure-hash 'sha256 (encode-coding-string text 'utf-8)))
         (now (mevedel-schedule-now)))
    (maphash (lambda (digest sent)
               (when (>= (- now sent) 86400) (remhash digest receipts))) receipts)
    (unless (gethash key receipts)
      (puthash key now receipts)
      (mevedel--write-file-atomically file (json-serialize receipts) nil #o600)
      t)))

(defun mevedel-tool-notify--handle (args)
  "Send the deliberate notice in ARGS."
  (let ((owner mevedel-tools--current-engine))
    (unless (and owner mevedel--current-request
                 (eq owner (mevedel-engine-owner mevedel--current-request))
                 (eq (current-buffer) (plist-get (mevedel-engine-info owner) :buffer))
                 (eq mevedel--session (mevedel-request-session mevedel--current-request))
                 (funcall (mevedel-tools-visibility owner) "Notify"))
      (error "Notify requires its owning root request")))
  (let ((text (plist-get args :text)))
    (unless (and (stringp text) (<= (length text) 240)
                 (not (string-blank-p text))
                 (not (string-match-p "[[:cntrl:]\u2028\u2029]" text)))
      (error "Notify needs one nonblank line of at most 240 characters"))
    (if (not (mevedel-tool-notify--claim mevedel--session text))
        (list :result "Duplicate notice suppressed.")
      (mevedel-notify mevedel--session 'notify text)
      (when (featurep 'mevedel-workspace-schedule)
        (mevedel-workspace-schedule-record-notice))
      (list :result (concat "Notified: " text)))))

(defun mevedel-tool-notify-register ()
  "Register the root notification tool."
  (mevedel-define-tool
   :name "Notify"
   :description "Send the user one short deliberate notice. Identical text is suppressed for 24 hours."
   :args ((text string :required "A single nonblank line, at most 240 characters."))
   :handler #'mevedel-tool-notify--handle
   :read-only-p t))

(provide 'mevedel-tool-notify)
;;; mevedel-tool-notify.el ends here
