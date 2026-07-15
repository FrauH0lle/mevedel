;;; mevedel-permission-log.el -- Permission diagnostics -*- lexical-binding: t -*-

;;; Commentary:

;; Append-only, per-session diagnostics for permission prompts and
;; permission-prompt overlays.  This log is intentionally separate from
;; session state: it exists for postmortems when a transient prompt or
;; queue entry disappears before a model-visible tool result is saved.

;;; Code:

(eval-when-compile (require 'cl-lib))

;; `mevedel-structs'
(declare-function mevedel-session-permission-log-pending
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-save-path "mevedel-structs" (cl-x) t)

;; `mevedel-agents'
(declare-function mevedel-agent-invocation-agent-id
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-p "mevedel-agents" (cl-x))
(declare-function mevedel-agent-invocation-parent-session
                  "mevedel-agents" (cl-x) t)

(defvar mevedel--agent-invocation)
(defvar mevedel--data-buffer)
(defvar mevedel--session)


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

(defun mevedel-permission-log-current-session (&optional buffer)
  "Return the session visible from BUFFER, or nil."
  (let ((buf (or buffer (current-buffer))))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (or (and (boundp 'mevedel--session) mevedel--session)
            (and (boundp 'mevedel--data-buffer)
                 (buffer-live-p mevedel--data-buffer)
                 (buffer-local-value 'mevedel--session
                                     mevedel--data-buffer))
            (and (boundp 'mevedel--agent-invocation)
                 (fboundp 'mevedel-agent-invocation-p)
                 (mevedel-agent-invocation-p mevedel--agent-invocation)
                 (fboundp 'mevedel-agent-invocation-parent-session)
                 (mevedel-agent-invocation-parent-session
                  mevedel--agent-invocation)))))))

(defun mevedel-permission-log-origin (&optional buffer)
  "Return the prompt origin visible from BUFFER."
  (let ((buf (or buffer (current-buffer))))
    (or
     (and (buffer-live-p buf)
          (with-current-buffer buf
            (and (boundp 'mevedel--agent-invocation)
                 (fboundp 'mevedel-agent-invocation-p)
                 (mevedel-agent-invocation-p mevedel--agent-invocation)
                 (fboundp 'mevedel-agent-invocation-agent-id)
                 (mevedel-agent-invocation-agent-id
                  mevedel--agent-invocation))))
     "main")))

(defun mevedel-permission-log-path (session)
  "Return the persistent permission log path for SESSION, or nil."
  (when-let* ((save-path (and session
                              (ignore-errors
                                (mevedel-session-save-path session)))))
    (file-name-concat save-path mevedel-permission-log-file-name)))


;;
;;; Persistence

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

(defun mevedel-permission-log--persist (session entry)
  "Append sanitized permission log ENTRY to SESSION's persistent log."
  (when-let* ((file (and mevedel-permission-log-enabled
                         (mevedel-permission-log-path session))))
    (condition-case err
        (let ((print-length nil)
              (print-level nil)
              (print-quoted t))
          (make-directory (file-name-directory file) t)
          (with-temp-buffer
            (prin1 (mevedel-permission-log--printable-value entry)
                   (current-buffer))
            (insert "\n")
            (append-to-file (point-min) (point-max) file)))
      (error
       (message "mevedel: permission log persistence failed: %s"
                (error-message-string err))))))

(defun mevedel-permission-log (session event &rest props)
  "Append EVENT and PROPS to SESSION's permission diagnostic log."
  (when (and mevedel-permission-log-enabled session)
    (let ((entry (append (list :event event
                               :time (format-time-string "%FT%T%z"))
                         props)))
      (if (mevedel-session-save-path session)
          (mevedel-permission-log--persist session entry)
        (setf (mevedel-session-permission-log-pending session)
              (append (mevedel-session-permission-log-pending session)
                      (list entry)))))))

(provide 'mevedel-permission-log)

;;; mevedel-permission-log.el ends here
