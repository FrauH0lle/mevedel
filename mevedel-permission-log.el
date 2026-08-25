;;; mevedel-permission-log.el -- Permission diagnostics -*- lexical-binding: t -*-

;;; Commentary:

;; Append-only, per-session diagnostics for permission prompts and
;; permission-prompt overlays.  This log is intentionally separate from
;; session state: it exists for postmortems when a transient prompt or
;; queue entry disappears before a model-visible tool result is saved.

;;; Code:

(eval-when-compile (require 'cl-lib))

;; `mevedel-execution-target'
(declare-function mevedel-execution-target-remote-p
                  "mevedel-execution-target" (target))
(autoload 'mevedel-execution-target-remote-p "mevedel-execution-target")

;; `mevedel-session-publication'
(declare-function mevedel-session-publication-append-diagnostic
                  "mevedel-session-publication" (session path content))
(autoload 'mevedel-session-publication-append-diagnostic
  "mevedel-session-publication")

;; `mevedel-structs'
(declare-function mevedel-session-execution-target
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-permission-log-pending
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-permission-mode "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-save-path "mevedel-structs" (cl-x) t)

;; `mevedel-telemetry'
(declare-function mevedel-telemetry-record
                  "mevedel-telemetry" (session event &rest props))


;;
;;; Customization

(defcustom mevedel-permission-log-enabled t
  "When non-nil, persist permission diagnostics in each session directory."
  :type 'boolean
  :group 'mevedel)

(defcustom mevedel-permission-log-file-name "permission-log.el"
  "File name for per-session permission diagnostics."
  :type 'string
  :group 'mevedel)


;;
;;; Session and origin helpers

(defun mevedel-permission-log-path (session)
  "Return the persistent permission log path for SESSION, or nil."
  (when-let* ((save-path (and session
                              (ignore-errors
                                (mevedel-session-save-path session)))))
    (file-name-concat save-path mevedel-permission-log-file-name)))


;;
;;; Persistence

(defun mevedel-permission-log--remote-p (session)
  "Return non-nil when SESSION's execution target is remote."
  (when-let* ((target (mevedel-session-execution-target session)))
    (mevedel-execution-target-remote-p target)))

(defun mevedel-permission-log--printable-value (value)
  "Return a disk-log-safe representation of VALUE."
  (cond
   ((or (null value)
        (keywordp value)
        (symbolp value)
        (stringp value)
        (numberp value))
    value)
   ((and (listp value) (keywordp (car-safe value)))
    (let (out)
      (while value
        (let ((key (pop value))
              (val (pop value)))
          (setq out
                (plist-put out key
                           (mevedel-permission-log--printable-value val)))))
      out))
   ((consp value)
    (cons (mevedel-permission-log--printable-value (car value))
          (mevedel-permission-log--printable-value (cdr value))))
   ((vectorp value)
    (vconcat (mapcar #'mevedel-permission-log--printable-value
                     (append value nil))))
   (t
   (format "%S" value))))

(defun mevedel-permission-log--entry-text (entry)
  "Return sanitized permission log ENTRY in its durable line format."
  (let ((print-length nil)
        (print-level nil)
        (print-quoted t))
    (concat
     (prin1-to-string (mevedel-permission-log--printable-value entry))
     "\n")))

(defun mevedel-permission-log--persist-content (session content)
  "Append serialized permission log CONTENT for SESSION."
  (when-let* ((file (and mevedel-permission-log-enabled
                         (mevedel-permission-log-path session))))
    (condition-case err
        (if (mevedel-permission-log--remote-p session)
            (mevedel-session-publication-append-diagnostic
             session file content)
          (make-directory (file-name-directory file) t)
          (write-region content nil file t 'silent)
          t)
      (error
       (message "mevedel: permission log persistence failed: %s"
                (error-message-string err))
       nil))))

(defun mevedel-permission-log--persist (session entry)
  "Append sanitized permission log ENTRY to SESSION's persistent log."
  (mevedel-permission-log--persist-content
   session (mevedel-permission-log--entry-text entry)))

(defun mevedel-permission-log-flush (session)
  "Persist SESSION's queued permission diagnostics, retaining failures."
  (when session
    (let ((pending (mevedel-session-permission-log-pending session)))
      (if (and pending (mevedel-permission-log--remote-p session))
          (when (mevedel-permission-log--persist-content
                 session (mapconcat #'mevedel-permission-log--entry-text
                                    pending ""))
            (setf (mevedel-session-permission-log-pending session) nil))
        (let (remaining)
          (dolist (entry pending)
            (unless (mevedel-permission-log--persist session entry)
              (push entry remaining)))
          (setf (mevedel-session-permission-log-pending session)
                (nreverse remaining)))))))

(defun mevedel-permission-log (session event &rest props)
  "Append EVENT and PROPS to SESSION's permission diagnostic log."
  (when (and mevedel-permission-log-enabled session)
    (let ((entry (append (list :event event
                               :time (format-time-string "%FT%T%z"))
                         props))
          (pending (mevedel-session-permission-log-pending session)))
      (if (and (null pending)
               (mevedel-session-save-path session)
               (not (mevedel-permission-log--remote-p session))
               (mevedel-permission-log--persist session entry))
          nil
        (setf (mevedel-session-permission-log-pending session)
              (append pending (list entry))))
      (when (fboundp 'mevedel-telemetry-record)
        (apply #'mevedel-telemetry-record session event
               :permission-mode-base
               (mevedel-session-permission-mode session)
               :permission-mode-effective (plist-get props :mode)
               props)))))

(provide 'mevedel-permission-log)

;;; mevedel-permission-log.el ends here
