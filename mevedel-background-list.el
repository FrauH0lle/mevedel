;;; mevedel-background-list.el --- Background-work cockpit -*- lexical-binding: t -*-

;;; Commentary:

;; Tabulated user controls for one session's background work: every live
;; managed execution (Bash, including foreground work, and Monitor) and every
;; active agent, then the session's pending and expired wake-ups.  The
;; execution, agent-control and schedule modules own process, turn, authority
;; and wake-up state; this module owns only the cockpit projection and its
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

;; `mevedel-eval-sandbox'
(declare-function mevedel-eval-sandbox-list-idle "mevedel-eval-sandbox" (session calls))
(declare-function mevedel-eval-sandbox-any-live-p "mevedel-eval-sandbox" (session))
(autoload 'mevedel-eval-sandbox-any-live-p "mevedel-eval-sandbox")
(declare-function mevedel-eval-sandbox-stop-user "mevedel-eval-sandbox" (session snapshot))
(autoload 'mevedel-eval-sandbox-list-idle "mevedel-eval-sandbox")
(autoload 'mevedel-eval-sandbox-stop-user "mevedel-eval-sandbox")

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

;; `mevedel-schedule'
(declare-function mevedel-schedule-cancel "mevedel-schedule" (session buffer id))
(declare-function mevedel-schedule-format-time "mevedel-schedule" (seconds))
(declare-function mevedel-schedule-now "mevedel-schedule" ())
(autoload 'mevedel-schedule-cancel "mevedel-schedule")
(autoload 'mevedel-schedule-format-time "mevedel-schedule")
(autoload 'mevedel-schedule-now "mevedel-schedule")
(autoload 'mevedel-schedule-resume "mevedel-schedule")

;; `mevedel-structs'
(declare-function mevedel-session-name "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-root-buffer "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-schedules "mevedel-structs" (cl-x) t)

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
      (mevedel-diagnostic-user-error (mevedel-cockpit-diagnostic-scope context)
                                   "No mevedel session in this buffer")))

(defun mevedel-background-list--agent-p (item)
  "Return non-nil when cockpit ITEM is an agent rather than an execution."
  (eq (plist-get item :kind) 'agent))

(defun mevedel-background-list--wake-up-p (item)
  "Return non-nil when cockpit ITEM is a wake-up."
  (eq (plist-get item :kind) 'wake-up))

(defun mevedel-background-list--collect (context)
  "Collect active agents, live executions, then wake-ups, for CONTEXT."
  (let* ((session (mevedel-background-list--session context))
         (calls (mevedel-execution-list-user session)))
    (append
     (mapcar (lambda (row) (append (list :kind 'agent :id (plist-get row :path)) row))
             (mevedel-view-agent-active-rows session))
     (mapcar (lambda (item) (append (list :id (plist-get item :execution-id)) item))
             calls)
     (when (mevedel-eval-sandbox-any-live-p session)
       (mevedel-eval-sandbox-list-idle session calls))
     (mapcar (lambda (entry) (cons :kind (cons 'wake-up entry)))
             (mevedel-session-schedules session)))))

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
  (cond
   ((mevedel-background-list--wake-up-p item)
    (let ((seconds (max 0 (- (plist-get item :due) (mevedel-schedule-now))))
          (pending (eq 'pending (plist-get item :status))))
      (list (plist-get item :id)
            (vector "wake-up" (capitalize (symbol-name (plist-get item :status)))
                    (mevedel--truncate-display
                     (replace-regexp-in-string "[\n\r\t]+" " " (plist-get item :note))
                     40 "…")
                    "/root"
                    (mevedel-cockpit-number-cell
                     (if pending (concat "in " (mevedel--duration-label seconds)) "—")
                     (if pending seconds 0))
                    (format "%s · at %s" (plist-get item :id)
                            (format-time-string "%F %H:%M" (plist-get item :due)))))))
   ((eq (plist-get item :kind) 'sandbox)
    (let ((age (- (float-time) (plist-get item :started-at))))
      (list (plist-get item :id)
            (vector "sandbox" "Idle" "Persistent Eval" (plist-get item :owner)
                    (mevedel-cockpit-number-cell (mevedel--duration-label age) age)
                    (format "%s features" (plist-get item :features))))))
   ((mevedel-background-list--agent-p item)
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
                        "")))))))
   (t
    (let ((monitor-p (eq (plist-get item :kind) 'monitor))
          (seconds (or (plist-get item :wall-time-seconds) 0)))
      (list
       (plist-get item :id)
       (vector
        (symbol-name (or (plist-get item :kind) 'bash))
        (mevedel-background-list--execution-state item)
        (mevedel--truncate-display
         (or (plist-get item :description) (plist-get item :command)
             (and (eq (plist-get item :kind) 'eval) "Eval") "") 40 "…")
        (or (plist-get item :owner) "")
        (mevedel-cockpit-number-cell
         (if monitor-p "—" (mevedel--duration-label seconds))
         (if monitor-p 0 seconds))
        (string-join
         (delq nil
               (list (plist-get item :execution-id)
                     (when (eq (plist-get item :eval-mode) 'sandbox) "persistent sandbox")
                     (when monitor-p
                       (format "%d events" (or (plist-get item :event-count) 0)))
                     (when-let* ((expires (and monitor-p (plist-get item :expires-at))))
                       (format "until %s" (format-time-string "%H:%M" expires)))
                     (if (plist-get item :tty) "PTY" "pipe")
                     (format "%d bytes" (or (plist-get item :output-bytes) 0))
                     (format "sandbox %s" (or (plist-get item :sandbox-state) 'pending))))
         " · ")))))))

(defun mevedel-background-list--header (items context)
  "Return the cockpit header for background ITEMS and CONTEXT."
  (let* ((session (mevedel-cockpit-context-session context))
         (agents (cl-count-if #'mevedel-background-list--agent-p items))
         (wake-ups (cl-count-if #'mevedel-background-list--wake-up-p items))
         (executions (- (length items) agents wake-ups)))
    (mevedel-cockpit-format-header
     "background work"
     (if session (mevedel-session-name session) "")
     (format "%d %s · %d %s%s"
             agents (if (= agents 1) "agent" "agents")
             executions (if (= executions 1) "execution" "executions")
             (if (> wake-ups 0)
                 (format " · %d %s" wake-ups (if (= wake-ups 1) "wake-up" "wake-ups"))
               "")))))

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
  (cond
   ((mevedel-background-list--wake-up-p item)
    (list :title "Wake-up" :subtitle (plist-get item :id)
          :identity (plist-get item :id)
          :sections
          (list (list :id 'wake-up :title "Wake-up"
                      :body (concat
                             (mevedel-report-fields
                              (list "Status" (symbol-name (plist-get item :status)))
                              (when-let* ((reason (plist-get item :pause-reason))) (list "Reason" reason))
                              (list "Time" (mevedel-schedule-format-time (plist-get item :due)))
                              (when-let* ((expires (plist-get item :expires-at)))
                                (list "Expires" (mevedel-schedule-format-time expires))))
                             "\n" (plist-get item :note) "\n"
                             (mapconcat
                              (lambda (run)
                                (format "%s  %s%s" (mevedel-schedule-format-time (plist-get run :time))
                                        (capitalize (symbol-name (plist-get run :outcome)))
                                        (if-let* ((reason (plist-get run :reason)))
                                            (concat ": " reason) "")))
                              (plist-get item :runs) "\n"))))))
   ((eq (plist-get item :kind) 'sandbox)
    (list :title "Idle sandbox" :subtitle (plist-get item :owner)
          :sections (list (list :id 'sandbox :title "Persistent Eval"
                                :body (format "%s features. Stop with k in /ps."
                                              (plist-get item :features))))))
   ((mevedel-background-list--agent-p item)
    (mevedel-background-list--agent-details item))
   (t (mevedel-background-list--execution-details item))))

(defun mevedel-background-list--selected-execution ()
  "Return the selected item, signalling unless it is an execution."
  (let ((item (mevedel-cockpit-surface-selected)))
    (when (memq (plist-get item :kind) '(agent sandbox wake-up))
      (mevedel-diagnostic-user-error (mevedel-cockpit-diagnostic-scope)
                                   "Selected item is not an execution"))
    item))

(defun mevedel-background-list--call-control (function &rest args)
  "Call execution user control FUNCTION with the selected execution and ARGS."
  (let* ((session (mevedel-background-list--session))
         (id (plist-get (mevedel-background-list--selected-execution) :execution-id)))
    (condition-case nil
        (apply function session id args)
      (mevedel-execution-not-found
       (mevedel-cockpit-surface-refresh)
       (mevedel-diagnostic-user-error (mevedel-diagnostic-scope session)
                                     "Execution %s is no longer live" id)))))

(defun mevedel-background-list--interrupt-agent (item)
  "Interrupt agent ITEM's current turn, reporting the outcome."
  (if (mevedel-agent-control-interrupt-user
       (mevedel-background-list--session) (plist-get item :path))
      (message "mevedel: agent %s interrupted" (plist-get item :path))
    (mevedel-diagnostic-message (mevedel-cockpit-diagnostic-scope)
                                "Agent %s has no active turn" (plist-get item :path)))
  (mevedel-cockpit-surface-refresh))

(defun mevedel-background-list-send-input ()
  "Send one newline-terminated input line to the selected PTY execution."
  (interactive)
  (unless (plist-get (mevedel-background-list--selected-execution) :tty)
    (mevedel-diagnostic-user-error (mevedel-cockpit-diagnostic-scope)
                                 "Selected execution uses closed pipe input"))
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
An agent stays retained after its turn is interrupted.  A selected wake-up
is cancelled."
  (interactive)
  (let ((item (mevedel-cockpit-surface-selected)))
    (cond
     ((mevedel-background-list--wake-up-p item)
      (let ((session (mevedel-background-list--session)))
        (mevedel-schedule-cancel
         session
         (or (mevedel-session-root-buffer session)
             (plist-get (mevedel-cockpit-surface-context) :data-buffer))
         (plist-get item :id))
        (mevedel-cockpit-surface-refresh)
        (message "mevedel: wake-up %s cancelled" (plist-get item :id))))
     ((eq (plist-get item :kind) 'sandbox)
      (mevedel-eval-sandbox-stop-user (mevedel-background-list--session) item))
     ((mevedel-background-list--agent-p item)
      (mevedel-background-list--interrupt-agent item))
     (t
      (mevedel-background-list--call-control #'mevedel-execution-stop-user)
      (message "mevedel: execution stopping")))))

(defun mevedel-background-list-resume ()
  "Resume the selected paused wake-up."
  (interactive)
  (let ((session (mevedel-background-list--session)) (item (mevedel-cockpit-surface-selected)))
    (unless (and (mevedel-background-list--wake-up-p item) (eq 'paused (plist-get item :status)))
      (mevedel-diagnostic-user-error (mevedel-diagnostic-scope session) "Select a paused wake-up"))
    (mevedel-schedule-resume session (mevedel-session-root-buffer session) (plist-get item :id))))

(defun mevedel-background-list-visit ()
  "Open the selected agent's transcript or execution's live artifact."
  (interactive)
  (let ((item (mevedel-cockpit-surface-selected)))
    (if (mevedel-background-list--agent-p item)
        (mevedel-view-open-agent-transcript (plist-get item :path))
      (let ((path (plist-get item :artifact-path)))
        (unless (and path (file-readable-p path))
          (mevedel-diagnostic-user-error (mevedel-cockpit-diagnostic-scope)
                                       "Execution artifact is no longer available"))
        (find-file path)))))

(defun mevedel-background-list-quit ()
  "Close the background-work cockpit and restore the invoking layout."
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
           ("k" "Stop the selected execution, interrupt the agent's turn, or cancel the wake-up" mevedel-background-list-stop)
           ("r" "Resume the selected paused wake-up" mevedel-background-list-resume)
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
