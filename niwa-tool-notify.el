;;; niwa-tool-notify.el --- Deliberate root notices -*- lexical-binding: t -*-

;;; Commentary:
;; The root uses its ordinary tool result as the transcript notice line.

;;; Code:

(require 'niwa-notify)
(require 'niwa-schedule)
(require 'niwa-tool-registry)
(require 'niwa-structs)
(require 'niwa-engine)
(require 'niwa-utilities)

;; `niwa-session-artifacts'
(autoload 'niwa-session-artifacts-assert-mutation-authority "niwa-session-artifacts")

;; `niwa-session-persistence'
(autoload 'niwa-session-persistence-shallow-ensure-files "niwa-session-persistence")

;; `niwa-tools'
(declare-function niwa-tools-visibility "niwa-tools" (&optional owner))
(defvar niwa-tools--current-engine)

;; `niwa-workspace-schedule'
(declare-function niwa-workspace-schedule-notify-receipt-file
                  "niwa-workspace-schedule" (session))
(declare-function niwa-workspace-schedule-record-notice "niwa-workspace-schedule" ())
(defvar niwa-workspace-schedule-run)

(defun niwa-tool-notify--receipt-file (session)
  "Return the notice receipt file owned by SESSION.
An active workspace schedule run shares its schedule's file instead."
  (or (and (featurep 'niwa-workspace-schedule)
           (niwa-workspace-schedule-notify-receipt-file session))
      (file-name-concat
       (or (niwa-session-save-path session)
           (niwa-session-persistence-shallow-ensure-files session (current-buffer))
           (error "Cannot record a notice for an unsaved session"))
       "notify-receipts.json")))

(defun niwa-tool-notify--claim (session text)
  "Record a receipt and return non-nil if SESSION may deliver TEXT now."
  (niwa-session-artifacts-assert-mutation-authority session (current-buffer))
  (let* ((file (niwa-tool-notify--receipt-file session))
         (receipts (condition-case nil
                       (with-temp-buffer (insert-file-contents file) (json-parse-buffer))
                     (file-missing (make-hash-table :test #'equal))))
         (key (secure-hash 'sha256 (encode-coding-string text 'utf-8)))
         (now (niwa-schedule-now)))
    (maphash (lambda (digest sent)
               (when (>= (- now sent) 86400) (remhash digest receipts))) receipts)
    (unless (gethash key receipts)
      (puthash key now receipts)
      (niwa--write-file-atomically file (json-serialize receipts) nil #o600)
      t)))

(defun niwa-tool-notify--handle (args)
  "Send the deliberate notice in ARGS."
  (let ((owner niwa-tools--current-engine))
    (unless (and owner niwa--current-request
                 (eq owner (niwa-engine-owner niwa--current-request))
                 (eq (current-buffer) (plist-get (niwa-engine-info owner) :buffer))
                 (eq niwa--session (niwa-request-session niwa--current-request))
                 (funcall (niwa-tools-visibility owner) "Notify"))
      (error "Notify requires its owning root request")))
  (let ((text (plist-get args :text)))
    (unless (and (stringp text) (<= (length text) 240)
                 (not (string-blank-p text))
                 (not (string-match-p "[[:cntrl:]\u2028\u2029]" text)))
      (error "Notify needs one nonblank line of at most 240 characters"))
    (if (not (niwa-tool-notify--claim niwa--session text))
        (list :result "Duplicate notice suppressed.")
      (niwa-notify niwa--session 'notify text)
      (when (featurep 'niwa-workspace-schedule)
        (niwa-workspace-schedule-record-notice))
      (list :result (concat "Notified: " text)))))

(defun niwa-tool-notify-register ()
  "Register the root notification tool."
  (niwa-define-tool
   :name "Notify"
   :description "Send the user one short deliberate notice. Identical text is suppressed for 24 hours."
   :args ((text string :required "A single nonblank line, at most 240 characters."))
   :handler #'niwa-tool-notify--handle
   :read-only-p t))

(provide 'niwa-tool-notify)
;;; niwa-tool-notify.el ends here
