;;; mevedel-background-list.el --- Background-work cockpit -*- lexical-binding: t -*-

;;; Commentary:

;; Tabulated user controls for one session's background work: every live
;; managed execution (Bash, including foreground work, and Monitor) and every
;; active agent.  The execution and agent-control modules own process, turn
;; and authority state; this module owns only the cockpit projection and its
;; interactive actions.

;;; Code:

(require 'mevedel-report)

(eval-when-compile
  (require 'cl-lib)
  (require 'tabulated-list))

;; `mevedel-agent-control'
(declare-function mevedel-agent-control-interrupt-user
                  "mevedel-agent-control" (session path))
(autoload 'mevedel-agent-control-interrupt-user "mevedel-agent-control")

;; `mevedel-cockpit'
(declare-function mevedel-cockpit-context-session
                  "mevedel-cockpit" (&optional context))
(declare-function mevedel-cockpit-current-context "mevedel-cockpit" ())
(declare-function mevedel-cockpit-format-header
                  "mevedel-cockpit" (name scope state))
(declare-function mevedel-cockpit-number-cell "mevedel-cockpit" (text number))
(declare-function mevedel-cockpit-open-surface
                  "mevedel-cockpit" (surface &optional context))
(declare-function mevedel-cockpit-quit "mevedel-cockpit" (&optional label))
(declare-function mevedel-cockpit-setup-tabulated-surface
                  "mevedel-cockpit" (surface))
(declare-function mevedel-cockpit-sort-by-number "mevedel-cockpit" (column))
(declare-function mevedel-cockpit-surface-context
                  "mevedel-cockpit" (&optional surface))
(declare-function mevedel-cockpit-surface-refresh
                  "mevedel-cockpit" (&optional selected-id))
(declare-function mevedel-cockpit-surface-selected
                  "mevedel-cockpit" (&optional no-error))
(autoload 'mevedel-cockpit-context-session "mevedel-cockpit")
(autoload 'mevedel-cockpit-current-context "mevedel-cockpit")
(autoload 'mevedel-cockpit-format-header "mevedel-cockpit")
(autoload 'mevedel-cockpit-number-cell "mevedel-cockpit")
(autoload 'mevedel-cockpit-open-surface "mevedel-cockpit")
(autoload 'mevedel-cockpit-quit "mevedel-cockpit" nil t)
(autoload 'mevedel-cockpit-setup-tabulated-surface "mevedel-cockpit")
(autoload 'mevedel-cockpit-sort-by-number "mevedel-cockpit")
(autoload 'mevedel-cockpit-surface-context "mevedel-cockpit")
(autoload 'mevedel-cockpit-surface-refresh "mevedel-cockpit" nil t)
(autoload 'mevedel-cockpit-surface-selected "mevedel-cockpit")

;; `mevedel-execution'
(declare-function mevedel-execution-interrupt-user
                  "mevedel-execution" (session execution-id))
(declare-function mevedel-execution-list-user
                  "mevedel-execution" (session))
(declare-function mevedel-execution-stop-user
                  "mevedel-execution" (session execution-id))
(declare-function mevedel-execution-write-user
                  "mevedel-execution" (session execution-id chars))
(autoload 'mevedel-execution-interrupt-user "mevedel-execution")
(autoload 'mevedel-execution-list-user "mevedel-execution")
(autoload 'mevedel-execution-stop-user "mevedel-execution")
(autoload 'mevedel-execution-write-user "mevedel-execution")
(defvar mevedel-execution-event-functions)
(defvar mevedel-execution-state-change-hook)

;; `mevedel-sandbox'
(declare-function mevedel-sandbox-status-text "mevedel-sandbox" (facts))
(autoload 'mevedel-sandbox-status-text "mevedel-sandbox")

;; `mevedel-structs'
(declare-function mevedel-session-name "mevedel-structs" (cl-x) t)

;; `mevedel-utilities'
(declare-function mevedel--duration-label "mevedel-utilities" (seconds))
(declare-function mevedel--truncate-display
                  "mevedel-utilities" (text width &optional ellipsis))
(autoload 'mevedel--duration-label "mevedel-utilities")
(autoload 'mevedel--truncate-display "mevedel-utilities")

;; `mevedel-view-agent'
(declare-function mevedel-view-agent-active-rows "mevedel-view-agent" (session))
(declare-function mevedel-view-agent-model-details "mevedel-view-agent" (metadata))
(declare-function mevedel-view-agent-model-label
                  "mevedel-view-agent" (metadata &optional full))
(declare-function mevedel-view-open-agent-transcript
                  "mevedel-view-agent" (agent-path))
(autoload 'mevedel-view-agent-active-rows "mevedel-view-agent")
(autoload 'mevedel-view-agent-model-details "mevedel-view-agent")
(autoload 'mevedel-view-agent-model-label "mevedel-view-agent")
(autoload 'mevedel-view-open-agent-transcript "mevedel-view-agent" nil t)

;; `subr-x'
(declare-function string-empty-p "subr-x" (string))
(autoload 'string-empty-p "subr-x")

;; `tabulated-list'
(declare-function tabulated-list-mode "tabulated-list" ())

(defconst mevedel-background-list-buffer-name "*mevedel background work*"
  "Name of the background-work cockpit buffer.")

(defconst mevedel-background-list-help-buffer-name
  "*mevedel background work help*"
  "Name of the background-work cockpit help buffer.")

(defun mevedel-background-list--session (&optional context)
  "Return the background-work cockpit session for CONTEXT."
  (or (mevedel-cockpit-context-session
       (or context (mevedel-cockpit-surface-context)))
      (user-error "No mevedel session in this buffer")))

(defun mevedel-background-list--agent-p (item)
  "Return non-nil when cockpit ITEM is an agent rather than an execution."
  (eq (plist-get item :kind) 'agent))

(defun mevedel-background-list--collect (context)
  "Collect active agents, then live executions, for CONTEXT."
  (let ((session (mevedel-background-list--session context)))
    (append
     (mapcar (lambda (row) (append (list :kind 'agent :id (plist-get row :path)) row))
             (mevedel-view-agent-active-rows session))
     (mapcar (lambda (item) (append (list :id (plist-get item :execution-id)) item))
             (mevedel-execution-list-user session)))))

(defun mevedel-background-list--agent-state (item)
  "Return the state label of agent ITEM."
  (pcase (plist-get item :status)
    ('blocked "Blocked")
    ('waiting "Waiting")
    (_ "Running")))

(defun mevedel-background-list--agent-seconds (item)
  "Return seconds since agent ITEM's current turn started, or nil."
  (when-let* ((started (plist-get item :started-at)))
    (float-time (time-subtract (current-time) started))))

(defun mevedel-background-list--execution-state (item)
  "Return the state label of execution ITEM."
  (pcase (plist-get item :state)
    ('queued "Queued")
    ('stopping "Stopping")
    (_ (if (eq (plist-get item :kind) 'monitor) "Watching" "Running"))))

(defun mevedel-background-list--entry (item _context)
  "Return the tabulated row for background ITEM."
  (if (mevedel-background-list--agent-p item)
      (let ((seconds (mevedel-background-list--agent-seconds item)))
        (list (plist-get item :id)
              (vector
               "agent"
               (mevedel-background-list--agent-state item)
               (plist-get item :path)
               (or (plist-get item :parent-path) "")
               (mevedel-cockpit-number-cell
                (if seconds (mevedel--duration-label seconds) "—") (or seconds 0))
               (pcase (plist-get item :activity)
                 ('permission-blocked "waiting for permission")
                 ('interaction-blocked "waiting for input")
                 (_ (or (mevedel-view-agent-model-label
                         (plist-get item :model-metadata))
                        ""))))))
    (let ((monitor-p (eq (plist-get item :kind) 'monitor))
          (seconds (or (plist-get item :wall-time-seconds) 0)))
      (list
       (plist-get item :id)
       (vector
        (if monitor-p "monitor" "bash")
        (mevedel-background-list--execution-state item)
        (mevedel--truncate-display
         (or (plist-get item :description) (plist-get item :command) "") 40 "…")
        (or (plist-get item :owner) "")
        (mevedel-cockpit-number-cell
         (if monitor-p "—" (mevedel--duration-label seconds))
         (if monitor-p 0 seconds))
        (string-join
         (delq nil
               (list (plist-get item :execution-id)
                     (when monitor-p
                       (format "%d events" (or (plist-get item :event-count) 0)))
                     (when-let* ((expires (and monitor-p (plist-get item :expires-at))))
                       (format "until %s" (format-time-string "%H:%M" expires)))
                     (if (plist-get item :tty) "PTY" "pipe")
                     (format "%d bytes" (or (plist-get item :output-bytes) 0))
                     (format "sandbox %s" (or (plist-get item :sandbox-state) 'pending))))
         " · "))))))

(defun mevedel-background-list--header (items context)
  "Return the cockpit header for background ITEMS and CONTEXT."
  (let* ((session (mevedel-cockpit-context-session context))
         (agents (cl-count-if #'mevedel-background-list--agent-p items)))
    (mevedel-cockpit-format-header
     "background work"
     (if session (mevedel-session-name session) "")
     (format "%d %s · %d %s"
             agents (if (= agents 1) "agent" "agents")
             (- (length items) agents)
             (if (= (- (length items) agents) 1) "execution" "executions")))))

(defun mevedel-background-list--agent-details (item)
  "Return the information report for agent ITEM."
  (let ((seconds (mevedel-background-list--agent-seconds item)))
    (list :title "Agent" :subtitle (plist-get item :path)
          :identity (plist-get item :id)
          :sections
          (list
           (list :id 'agent :title "Agent"
                 :body (concat
                        (mevedel-report-fields
                         (list "Agent" (plist-get item :path))
                         (when-let* ((parent (plist-get item :parent-path)))
                           (list "Parent" parent))
                         (list "State" (mevedel-background-list--agent-state item))
                         (list "Activity" (format "%s" (plist-get item :activity)))
                         (list "Elapsed" (if seconds (mevedel--duration-label seconds) "unknown")))
                        (when-let* ((model (mevedel-view-agent-model-details
                                            (plist-get item :model-metadata))))
                          (concat "\n" model))))))))

(defun mevedel-background-list--execution-details (item)
  "Return the information report for execution ITEM."
  (let ((tail (or (plist-get item :output-tail) ""))
        (monitor-p (eq (plist-get item :kind) 'monitor))
        (sandbox (plist-get item :sandbox-facts)))
    (list :title "Execution" :subtitle (plist-get item :execution-id)
          :identity (plist-get item :id)
          :sections
          (list
           (list :id 'execution :title "Execution"
                 :body (apply
                        #'mevedel-report-fields
                        (append
                         (list
                          (list "Execution" (plist-get item :execution-id))
                          (list "Owner" (plist-get item :owner))
                          (when-let* ((description (plist-get item :description)))
                            (list "Description" description))
                          (list "Command" (plist-get item :command))
                          (list "Mode" (if (plist-get item :tty) "PTY" "pipe")))
                         (if monitor-p
                             (list
                              (list "Status" (mevedel-background-list--execution-state item))
                              (when-let* ((expires (plist-get item :expires-at)))
                                (list "Expires" (format-time-string
                                                 "%Y-%m-%d %H:%M:%S %Z"
                                                 expires)))
                              (list "Events" (number-to-string (or (plist-get item :event-count) 0))))
                           (list
                            (list "Elapsed" (mevedel--duration-label
                                             (or (plist-get item :wall-time-seconds) 0))))))))
           (list :id 'output :title "Output & sandbox"
                 :body (concat
                        (mevedel-report-fields
                         (list "Output" (format "%d bytes, %d lines" (or (plist-get item :output-bytes) 0) (or (plist-get item :output-lines) 0)))
                         (list "Artifact" (or (plist-get item :artifact-path) "none")))
                        (if sandbox (mevedel-sandbox-status-text sandbox) "sandbox: pending")))
           (list :id 'tail :title "Live output tail"
                 :body (if (string-empty-p tail) "(no output yet)" tail))))))

(defun mevedel-background-list--details (item _context)
  "Return the information report for background ITEM."
  (if (mevedel-background-list--agent-p item)
      (mevedel-background-list--agent-details item)
    (mevedel-background-list--execution-details item)))

(defun mevedel-background-list--selected-execution ()
  "Return the selected item, signalling unless it is an execution."
  (let ((item (mevedel-cockpit-surface-selected)))
    (when (mevedel-background-list--agent-p item)
      (user-error "Selected item is an agent, not an execution"))
    item))

(defun mevedel-background-list--call-control (function &rest args)
  "Call execution user control FUNCTION with the selected execution and ARGS."
  (let* ((session (mevedel-background-list--session))
         (id (plist-get (mevedel-background-list--selected-execution) :execution-id)))
    (condition-case nil
        (apply function session id args)
      (mevedel-execution-not-found
       (mevedel-cockpit-surface-refresh)
       (user-error "Execution %s is no longer live" id)))))

(defun mevedel-background-list--interrupt-agent (item)
  "Interrupt agent ITEM's current turn, reporting the outcome."
  (if (mevedel-agent-control-interrupt-user
       (mevedel-background-list--session) (plist-get item :path))
      (message "mevedel: agent %s interrupted" (plist-get item :path))
    (message "mevedel: agent %s has no active turn" (plist-get item :path)))
  (mevedel-cockpit-surface-refresh))

(defun mevedel-background-list-send-input ()
  "Send one newline-terminated input line to the selected PTY execution."
  (interactive)
  (unless (plist-get (mevedel-background-list--selected-execution) :tty)
    (user-error "Selected execution uses closed pipe input"))
  (mevedel-background-list--call-control
   #'mevedel-execution-write-user
   (concat (read-string "PTY input: ") "\n")))

(defun mevedel-background-list-interrupt ()
  "Interrupt the selected execution's process group or agent's turn."
  (interactive)
  (let ((item (mevedel-cockpit-surface-selected)))
    (if (mevedel-background-list--agent-p item)
        (mevedel-background-list--interrupt-agent item)
      (mevedel-background-list--call-control #'mevedel-execution-interrupt-user)
      (message "mevedel: execution interrupted"))))

(defun mevedel-background-list-stop ()
  "Stop the selected execution, or interrupt the selected agent's turn.
An agent stays retained after its turn is interrupted."
  (interactive)
  (let ((item (mevedel-cockpit-surface-selected)))
    (if (mevedel-background-list--agent-p item)
        (mevedel-background-list--interrupt-agent item)
      (mevedel-background-list--call-control #'mevedel-execution-stop-user)
      (message "mevedel: execution stopping"))))

(defun mevedel-background-list-visit ()
  "Open the selected agent's transcript or execution's live artifact."
  (interactive)
  (let ((item (mevedel-cockpit-surface-selected)))
    (if (mevedel-background-list--agent-p item)
        (mevedel-view-open-agent-transcript (plist-get item :path))
      (let ((path (plist-get item :artifact-path)))
        (unless (and path (file-readable-p path))
          (user-error "Execution artifact is no longer available"))
        (find-file path)))))

(defun mevedel-background-list-quit ()
  "Quit the background-work cockpit and return to the session cockpit."
  (interactive)
  (mevedel-cockpit-quit "background-work cockpit"))

(defconst mevedel-background-list--surface
  `(:buffer-name ,mevedel-background-list-buffer-name
    :label "background-work cockpit"
    :row-label "background item"
    :mode mevedel-background-list-mode
    :format [("Kind" 8 t)
             ("State" 9 t)
             ("Name" 40 t)
             ("Owner" 18 t)
             ("Elapsed" 9 ,(mevedel-cockpit-sort-by-number 4))
             ("Details" 0 t)]
    :sort-key nil
    :require-session t
    :collect mevedel-background-list--collect
    :entry mevedel-background-list--entry
    :header mevedel-background-list--header
    :details mevedel-background-list--details
    :details-buffer "*mevedel background item*"
    :help-buffer ,mevedel-background-list-help-buffer-name
    :keys (("i" "Send a line to the selected PTY execution" mevedel-background-list-send-input)
           ("C-c C-c" "Interrupt the selected execution or agent turn" mevedel-background-list-interrupt)
           ("k" "Stop the selected execution or interrupt the agent's turn" mevedel-background-list-stop)
           ("o" "Open the agent transcript or execution artifact" mevedel-background-list-visit)))
  "Cockpit surface spec for background work.")

(define-derived-mode mevedel-background-list-mode tabulated-list-mode
  "mevedel-background"
  "Major mode for inspecting and controlling background work."
  (mevedel-cockpit-setup-tabulated-surface
   mevedel-background-list--surface))

(defun mevedel-background-list-open (&optional context)
  "Open the background-work cockpit for CONTEXT."
  (interactive)
  (let ((context (or context (mevedel-cockpit-current-context))))
    (mevedel-background-list--session context)
    (mevedel-cockpit-open-surface mevedel-background-list--surface context)))

(defun mevedel-background-list-refresh-session (session &optional _data-buffer)
  "Refresh an open background-work cockpit owned by SESSION."
  (when-let* ((buffer (get-buffer mevedel-background-list-buffer-name)))
    (with-current-buffer buffer
      (when (and (derived-mode-p 'mevedel-background-list-mode)
                 (eq session
                     (condition-case nil
                         (mevedel-background-list--session)
                       (user-error nil))))
        (mevedel-cockpit-surface-refresh)))))

(defun mevedel-background-list-handle-event (event)
  "Refresh the background-work cockpit for immutable execution EVENT."
  (when (memq (plist-get event :type) '(progress yield))
    (mevedel-background-list-refresh-session
     (plist-get event :session))))

(add-hook 'mevedel-execution-event-functions
          #'mevedel-background-list-handle-event)
(add-hook 'mevedel-execution-state-change-hook
          #'mevedel-background-list-refresh-session)

(provide 'mevedel-background-list)
;;; mevedel-background-list.el ends here
