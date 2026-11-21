;;; niwa-permission-log.el -- Permission diagnostics -*- lexical-binding: t -*-

;;; Commentary:

;; Append-only, per-session diagnostics for permission prompts and
;; permission-prompt overlays.  This log is intentionally separate from
;; session state: it exists for postmortems when a transient prompt or
;; queue entry disappears before a model-visible tool result is saved.

;;; Code:

;; `setf' on a slot of a struct defined elsewhere needs that
;; `cl-defstruct' at compile time: `declare-function' supplies the
;; accessor but not the setter, and without the expander the form
;; compiles to a call to a function that does not exist.
(eval-when-compile (require 'niwa-structs))

(eval-when-compile (require 'cl-lib))

(require 'niwa-telemetry)
(require 'niwa-utilities)

;; `niwa-execution-target'
(declare-function niwa-execution-target-remote-p
                  "niwa-execution-target" (target))
(autoload 'niwa-execution-target-remote-p "niwa-execution-target")

;; `niwa-permission-mode'
(declare-function niwa-permission-mode-effective
                  "niwa-permission-mode"
                  (&optional session data-buffer surface-buffer request invocation))
(autoload 'niwa-permission-mode-effective "niwa-permission-mode")

;; `niwa-session-publication'
(declare-function niwa-session-publication-append-diagnostic
                  "niwa-session-publication" (session path content))
(autoload 'niwa-session-publication-append-diagnostic
  "niwa-session-publication")

;; `niwa-structs'
(declare-function niwa-session-execution-target
                  "niwa-structs" (cl-x) t)
(declare-function niwa-session-permission-log-pending
                  "niwa-structs" (cl-x) t)
(declare-function niwa-session-permission-mode "niwa-structs" (cl-x) t)
(declare-function niwa-session-save-path "niwa-structs" (cl-x) t)

;; `niwa-telemetry'
(declare-function niwa-telemetry-record
                  "niwa-telemetry" (session event &rest props))

;; `niwa-utilities'
(declare-function niwa--diagnostic-entry-text "niwa-utilities" (entry))
(autoload 'niwa--diagnostic-entry-text "niwa-utilities")


;;
;;; Customization

(defcustom niwa-permission-log-enabled t
  "When non-nil, persist permission diagnostics in each session directory."
  :type 'boolean
  :group 'niwa)

(defcustom niwa-permission-log-file-name "permission-log.el"
  "File name for per-session permission diagnostics."
  :type 'string
  :group 'niwa)


;;
;;; Session and origin helpers

(defun niwa-permission-log-path (session)
  "Return the persistent permission log path for SESSION, or nil."
  (when-let* ((save-path (and session
                              (ignore-errors
                                (niwa-session-save-path session)))))
    (file-name-concat save-path niwa-permission-log-file-name)))


;;
;;; Persistence

(defun niwa-permission-log--remote-p (session)
  "Return non-nil when SESSION's execution target is remote."
  (when-let* ((target (niwa-session-execution-target session)))
    (niwa-execution-target-remote-p target)))

(defun niwa-permission-log--persist-content (session content)
  "Append serialized permission log CONTENT for SESSION."
  (when-let* ((file (and niwa-permission-log-enabled
                         (niwa-permission-log-path session))))
    (condition-case err
        (if (niwa-permission-log--remote-p session)
            (niwa-session-publication-append-diagnostic
             session file content)
          (let ((coding-system-for-write 'utf-8-unix))
            (condition-case nil
                (write-region content nil file t 'silent)
              (file-missing
               (make-directory (file-name-directory file) t)
               (write-region content nil file t 'silent))))
          t)
      (error
       (niwa-diagnostic-message
        (niwa-diagnostic-scope session)
        "Permission log persistence failed: %s" (error-message-string err))
       nil))))

(defun niwa-permission-log--persist (session entry)
  "Append sanitized permission log ENTRY to SESSION's persistent log."
  (niwa-permission-log--persist-content
   session (niwa--diagnostic-entry-text entry)))

(defun niwa-permission-log-flush (session)
  "Persist SESSION's queued permission diagnostics, retaining failures."
  (when session
    (let ((pending (niwa-session-permission-log-pending session)))
      (if (and pending (niwa-permission-log--remote-p session))
          (when (niwa-permission-log--persist-content
                 session (mapconcat #'niwa--diagnostic-entry-text
                                    pending ""))
            (setf (niwa-session-permission-log-pending session) nil))
        (let (remaining)
          (dolist (entry pending)
            (unless (niwa-permission-log--persist session entry)
              (push entry remaining)))
          (setf (niwa-session-permission-log-pending session)
                (nreverse remaining)))))))

(defun niwa-permission-log (session event &rest props)
  "Append EVENT and PROPS to SESSION's permission diagnostic log."
  (when (and niwa-permission-log-enabled session)
    (let ((entry (append (list :event event
                               :time (format-time-string "%FT%T%z"))
                         props))
          (pending (niwa-session-permission-log-pending session)))
      (if (and (null pending)
               (niwa-session-save-path session)
               (not (niwa-permission-log--remote-p session))
               (niwa-permission-log--persist session entry))
          nil
        (setf (niwa-session-permission-log-pending session)
              (append pending (list entry))))
      (apply #'niwa-telemetry-record session event
             :permission-mode-base
             (or (plist-get props :permission-mode-base)
                 (niwa-session-permission-mode session))
             :permission-mode-effective
             (or (plist-get props :permission-mode-effective)
                 (plist-get props :mode)
                 (niwa-permission-mode-effective session))
             props))))

(provide 'niwa-permission-log)

;;; niwa-permission-log.el ends here
