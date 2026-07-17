;;; mevedel-executions-list.el --- Live execution cockpit -*- lexical-binding: t -*-

;;; Commentary:

;; Tabulated user controls for every live Bash execution in one session.  The
;; execution module owns process state and authority; this module owns only the
;; cockpit projection and interactive actions.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'tabulated-list))

;; `mevedel-cockpit'
(declare-function mevedel-cockpit-context-session
                  "mevedel-cockpit" (&optional context))
(declare-function mevedel-cockpit-current-context "mevedel-cockpit" ())
(declare-function mevedel-cockpit-open-surface
                  "mevedel-cockpit" (surface &optional context))
(declare-function mevedel-cockpit-quit "mevedel-cockpit" (&optional label))
(declare-function mevedel-cockpit-setup-tabulated-surface
                  "mevedel-cockpit" (surface))
(declare-function mevedel-cockpit-surface-context
                  "mevedel-cockpit" (&optional surface))
(declare-function mevedel-cockpit-surface-refresh
                  "mevedel-cockpit" (&optional selected-id))
(declare-function mevedel-cockpit-surface-selected
                  "mevedel-cockpit" (&optional no-error))

;; `mevedel-execution'
(declare-function mevedel-execution-interrupt-user
                  "mevedel-execution" (session execution-id))
(declare-function mevedel-execution-list-user
                  "mevedel-execution" (session))
(declare-function mevedel-execution-stop-user
                  "mevedel-execution" (session execution-id))
(declare-function mevedel-execution-write-user
                  "mevedel-execution" (session execution-id chars))
(defvar mevedel-execution-event-functions)
(defvar mevedel-execution-state-change-hook)

;; `mevedel-sandbox'
(declare-function mevedel-sandbox-status-text "mevedel-sandbox" (facts))

;; `subr-x'
(declare-function string-empty-p "subr-x" (string))

;; `tabulated-list'
(declare-function tabulated-list-mode "tabulated-list" ())

(defconst mevedel-executions-list-buffer-name "*mevedel executions*"
  "Name of the live execution cockpit buffer.")

(defconst mevedel-executions-list-help-buffer-name
  "*mevedel executions help*"
  "Name of the live execution cockpit help buffer.")

(defun mevedel-executions-list--session (&optional context)
  "Return the execution cockpit session for CONTEXT."
  (or (mevedel-cockpit-context-session
       (or context (mevedel-cockpit-surface-context)))
      (user-error "No mevedel session in this buffer")))

(defun mevedel-executions-list--collect (context)
  "Collect live execution snapshots for CONTEXT."
  (require 'mevedel-execution)
  (mevedel-execution-list-user
   (mevedel-executions-list--session context)))

(defun mevedel-executions-list--elapsed (seconds)
  "Return compact elapsed time for SECONDS."
  (cond
   ((< seconds 60) (format "%.1fs" seconds))
   ((< seconds 3600) (format "%dm%02ds" (/ (floor seconds) 60)
                             (% (floor seconds) 60)))
   (t (format "%dh%02dm" (/ (floor seconds) 3600)
              (% (/ (floor seconds) 60) 60)))))

(defun mevedel-executions-list--entry (item _context)
  "Return tabulated row for execution ITEM."
  (let ((id (plist-get item :execution-id)))
    (list
     id
     (vector
      id
      (or (plist-get item :owner) "")
      (if (plist-get item :tty) "PTY" "pipe")
      (mevedel-executions-list--elapsed
       (or (plist-get item :wall-time-seconds) 0))
      (number-to-string (or (plist-get item :output-bytes) 0))
      (format "%s" (or (plist-get item :sandbox-state) 'pending))
      (truncate-string-to-width
       (or (plist-get item :command) "") 60 nil nil "…")))))

(defun mevedel-executions-list--header (items _context)
  "Return cockpit header for execution ITEMS."
  (format "%d live execution%s"
          (length items) (if (= 1 (length items)) "" "s")))

(defun mevedel-executions-list--details (item _context)
  "Return detail text for execution ITEM."
  (require 'mevedel-sandbox)
  (require 'subr-x)
  (let ((tail (or (plist-get item :output-tail) ""))
        (sandbox (plist-get item :sandbox-facts)))
    (format
     (concat "Execution: %s\nOwner: %s\nCommand: %s\nMode: %s\n"
             "Elapsed: %s\nOutput: %d bytes, %d lines\nTimeout: %s\n"
             "Artifact: %s\n%s\n\nLive output tail:\n%s")
     (plist-get item :execution-id)
     (plist-get item :owner)
     (plist-get item :command)
     (if (plist-get item :tty) "PTY" "pipe")
     (mevedel-executions-list--elapsed
      (or (plist-get item :wall-time-seconds) 0))
     (or (plist-get item :output-bytes) 0)
     (or (plist-get item :output-lines) 0)
     (or (plist-get item :timeout-seconds) "none")
     (or (plist-get item :artifact-path) "none")
     (if sandbox
         (mevedel-sandbox-status-text sandbox)
       "sandbox: pending")
     (if (string-empty-p tail) "(no output yet)" tail))))

(defun mevedel-executions-list--selected ()
  "Return the selected live execution snapshot."
  (mevedel-cockpit-surface-selected))

(defun mevedel-executions-list--call-control (function &rest args)
  "Call user control FUNCTION with selected execution and ARGS."
  (let* ((context (mevedel-cockpit-surface-context))
         (session (mevedel-executions-list--session context))
         (item (mevedel-executions-list--selected))
         (id (plist-get item :execution-id)))
    (condition-case nil
        (apply function session id args)
      (mevedel-execution-not-found
       (mevedel-cockpit-surface-refresh)
       (user-error "Execution %s is no longer live" id)))))

(defun mevedel-executions-list-send-input ()
  "Send one newline-terminated input line to the selected PTY execution."
  (interactive)
  (let ((item (mevedel-executions-list--selected)))
    (unless (plist-get item :tty)
      (user-error "Selected execution uses closed pipe input"))
    (mevedel-executions-list--call-control
     #'mevedel-execution-write-user
     (concat (read-string "PTY input: ") "\n"))))

(defun mevedel-executions-list-interrupt ()
  "Interrupt the selected execution process group."
  (interactive)
  (mevedel-executions-list--call-control
   #'mevedel-execution-interrupt-user)
  (message "mevedel: execution interrupted"))

(defun mevedel-executions-list-stop ()
  "Stop the selected execution process group."
  (interactive)
  (mevedel-executions-list--call-control #'mevedel-execution-stop-user)
  (message "mevedel: execution stopping"))

(defun mevedel-executions-list-open-artifact ()
  "Open the selected execution's live retained artifact."
  (interactive)
  (let ((path (plist-get (mevedel-executions-list--selected)
                         :artifact-path)))
    (unless (and path (file-readable-p path))
      (user-error "Execution artifact is no longer available"))
    (find-file path)))

(defun mevedel-executions-list-quit ()
  "Quit the execution cockpit and return to the session cockpit."
  (interactive)
  (require 'mevedel-cockpit)
  (mevedel-cockpit-quit "execution cockpit"))

(defconst mevedel-executions-list--surface
  `(:buffer-name ,mevedel-executions-list-buffer-name
    :label "execution cockpit"
    :row-label "execution"
    :mode mevedel-executions-list-mode
    :format [("Execution" 15 t)
             ("Owner" 20 t)
             ("Mode" 6 t)
             ("Elapsed" 9 t)
             ("Bytes" 10 t)
             ("Sandbox" 12 t)
             ("Command" 0 t)]
    :sort-key ("Execution" . nil)
    :require-session t
    :collect mevedel-executions-list--collect
    :entry mevedel-executions-list--entry
    :header mevedel-executions-list--header
    :details mevedel-executions-list--details
    :details-buffer "*mevedel execution details*"
    :help-buffer ,mevedel-executions-list-help-buffer-name
    :keys (("i" "Send a line to the selected PTY" mevedel-executions-list-send-input)
           ("C-c C-c" "Interrupt the selected execution" mevedel-executions-list-interrupt)
           ("k" "Stop the selected execution" mevedel-executions-list-stop)
           ("o" "Open the selected live artifact" mevedel-executions-list-open-artifact)))
  "Cockpit surface spec for live executions.")

(define-derived-mode mevedel-executions-list-mode tabulated-list-mode
  "mevedel-executions"
  "Major mode for inspecting and controlling live executions."
  (require 'mevedel-cockpit)
  (mevedel-cockpit-setup-tabulated-surface
   mevedel-executions-list--surface))

(defun mevedel-executions-list-open (&optional context)
  "Open the live execution cockpit for CONTEXT."
  (interactive)
  (require 'mevedel-cockpit)
  (let ((context (or context (mevedel-cockpit-current-context))))
    (mevedel-executions-list--session context)
    (mevedel-cockpit-open-surface mevedel-executions-list--surface context)))

(defun mevedel-executions-list--refresh-session (session &optional _data-buffer)
  "Refresh an open execution cockpit owned by SESSION."
  (when-let* ((buffer (get-buffer mevedel-executions-list-buffer-name)))
    (with-current-buffer buffer
      (when (and (derived-mode-p 'mevedel-executions-list-mode)
                 (eq session
                     (condition-case nil
                         (mevedel-executions-list--session)
                       (user-error nil))))
        (mevedel-cockpit-surface-refresh)))))

(defun mevedel-executions-list-handle-event (event)
  "Refresh the execution cockpit for immutable execution EVENT."
  (when (memq (plist-get event :type) '(progress yield))
    (mevedel-executions-list--refresh-session
     (plist-get event :session))))

(add-hook 'mevedel-execution-event-functions
          #'mevedel-executions-list-handle-event)
(add-hook 'mevedel-execution-state-change-hook
          #'mevedel-executions-list--refresh-session)

(provide 'mevedel-executions-list)
;;; mevedel-executions-list.el ends here
