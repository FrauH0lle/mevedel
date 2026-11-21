;;; niwa-background-list.el --- Background-work cockpit -*- lexical-binding: t -*-

;;; Commentary:

;; Tabulated user controls for one session's background work: every live
;; managed execution (Bash, including foreground work, and Monitor) and every
;; active agent, then the session's pending and expired wake-ups.  The
;; execution, agent-control and schedule modules own process, turn, authority
;; and wake-up state; this module owns only the cockpit projection and its
;; interactive actions.

;;; Code:

(require 'niwa-report)

(eval-when-compile
  (require 'cl-lib)
  (require 'tabulated-list))

;; `niwa-agent-control'
(declare-function niwa-agent-control-interrupt-user
                  "niwa-agent-control" (session path))
(autoload 'niwa-agent-control-interrupt-user "niwa-agent-control")

;; `niwa-cockpit'
(declare-function niwa-cockpit-context-session
                  "niwa-cockpit" (&optional context))
(declare-function niwa-cockpit-current-context "niwa-cockpit" ())
(declare-function niwa-cockpit-format-header
                  "niwa-cockpit" (name scope state))
(declare-function niwa-cockpit-number-cell "niwa-cockpit" (text number))
(declare-function niwa-cockpit-open-surface
                  "niwa-cockpit" (surface &optional context))
(declare-function niwa-cockpit-quit "niwa-cockpit" (&optional label))
(declare-function niwa-cockpit-setup-tabulated-surface
                  "niwa-cockpit" (surface))
(declare-function niwa-cockpit-sort-by-number "niwa-cockpit" (column))
(declare-function niwa-cockpit-surface-context
                  "niwa-cockpit" (&optional surface))
(declare-function niwa-cockpit-surface-refresh
                  "niwa-cockpit" (&optional selected-id))
(declare-function niwa-cockpit-surface-selected
                  "niwa-cockpit" (&optional no-error))
(autoload 'niwa-cockpit-context-session "niwa-cockpit")
(autoload 'niwa-cockpit-current-context "niwa-cockpit")
(autoload 'niwa-cockpit-format-header "niwa-cockpit")
(autoload 'niwa-cockpit-number-cell "niwa-cockpit")
(autoload 'niwa-cockpit-open-surface "niwa-cockpit")
(autoload 'niwa-cockpit-quit "niwa-cockpit" nil t)
(autoload 'niwa-cockpit-setup-tabulated-surface "niwa-cockpit")
(autoload 'niwa-cockpit-sort-by-number "niwa-cockpit")
(autoload 'niwa-cockpit-surface-context "niwa-cockpit")
(autoload 'niwa-cockpit-surface-refresh "niwa-cockpit" nil t)
(autoload 'niwa-cockpit-surface-selected "niwa-cockpit")

;; `niwa-eval-sandbox'
(declare-function niwa-eval-sandbox-list-idle "niwa-eval-sandbox" (session calls))
(declare-function niwa-eval-sandbox-any-live-p "niwa-eval-sandbox" (session))
(autoload 'niwa-eval-sandbox-any-live-p "niwa-eval-sandbox")
(declare-function niwa-eval-sandbox-stop-user "niwa-eval-sandbox" (session snapshot))
(autoload 'niwa-eval-sandbox-list-idle "niwa-eval-sandbox")
(autoload 'niwa-eval-sandbox-stop-user "niwa-eval-sandbox")

;; `niwa-execution'
(declare-function niwa-execution-interrupt-user
                  "niwa-execution" (session execution-id))
(declare-function niwa-execution-list-user
                  "niwa-execution" (session))
(declare-function niwa-execution-stop-user
                  "niwa-execution" (session execution-id))
(declare-function niwa-execution-write-user
                  "niwa-execution" (session execution-id chars))
(autoload 'niwa-execution-interrupt-user "niwa-execution")
(autoload 'niwa-execution-list-user "niwa-execution")
(autoload 'niwa-execution-stop-user "niwa-execution")
(autoload 'niwa-execution-write-user "niwa-execution")
(defvar niwa-execution-event-functions)
(defvar niwa-execution-state-change-hook)

;; `niwa-sandbox'
(declare-function niwa-sandbox-status-text "niwa-sandbox" (facts))
(autoload 'niwa-sandbox-status-text "niwa-sandbox")

;; `niwa-schedule'
(declare-function niwa-schedule-cancel "niwa-schedule" (session buffer id))
(declare-function niwa-schedule-format-time "niwa-schedule" (seconds))
(declare-function niwa-schedule-now "niwa-schedule" ())
(autoload 'niwa-schedule-cancel "niwa-schedule")
(autoload 'niwa-schedule-format-time "niwa-schedule")
(autoload 'niwa-schedule-now "niwa-schedule")
(autoload 'niwa-schedule-resume "niwa-schedule")

;; `niwa-structs'
(declare-function niwa-session-name "niwa-structs" (cl-x) t)
(declare-function niwa-session-root-buffer "niwa-structs" (cl-x) t)
(declare-function niwa-session-schedules "niwa-structs" (cl-x) t)

;; `niwa-utilities'
(declare-function niwa--duration-label "niwa-utilities" (seconds))
(declare-function niwa--truncate-display
                  "niwa-utilities" (text width &optional ellipsis))
(autoload 'niwa--duration-label "niwa-utilities")
(autoload 'niwa--truncate-display "niwa-utilities")

;; `niwa-view-agent'
(declare-function niwa-view-agent-active-rows "niwa-view-agent" (session))
(declare-function niwa-view-agent-model-details "niwa-view-agent" (metadata))
(declare-function niwa-view-agent-model-label
                  "niwa-view-agent" (metadata &optional full))
(declare-function niwa-view-open-agent-transcript
                  "niwa-view-agent" (agent-path))
(autoload 'niwa-view-agent-active-rows "niwa-view-agent")
(autoload 'niwa-view-agent-model-details "niwa-view-agent")
(autoload 'niwa-view-agent-model-label "niwa-view-agent")
(autoload 'niwa-view-open-agent-transcript "niwa-view-agent" nil t)

;; `subr-x'
(declare-function string-empty-p "subr-x" (string))
(autoload 'string-empty-p "subr-x")

;; `tabulated-list'
(declare-function tabulated-list-mode "tabulated-list" ())

(defconst niwa-background-list-buffer-name "*niwa background work*"
  "Name of the background-work cockpit buffer.")

(defconst niwa-background-list-help-buffer-name
  "*niwa background work help*"
  "Name of the background-work cockpit help buffer.")

(defun niwa-background-list--session (&optional context)
  "Return the background-work cockpit session for CONTEXT."
  (or (niwa-cockpit-context-session
       (or context (niwa-cockpit-surface-context)))
      (niwa-diagnostic-user-error (niwa-cockpit-diagnostic-scope context)
                                   "No niwa session in this buffer")))

(defun niwa-background-list--agent-p (item)
  "Return non-nil when cockpit ITEM is an agent rather than an execution."
  (eq (plist-get item :kind) 'agent))

(defun niwa-background-list--wake-up-p (item)
  "Return non-nil when cockpit ITEM is a wake-up."
  (eq (plist-get item :kind) 'wake-up))

(defun niwa-background-list--collect (context)
  "Collect active agents, live executions, then wake-ups, for CONTEXT."
  (let* ((session (niwa-background-list--session context))
         (calls (niwa-execution-list-user session)))
    (append
     (mapcar (lambda (row) (append (list :kind 'agent :id (plist-get row :path)) row))
             (niwa-view-agent-active-rows session))
     (mapcar (lambda (item) (append (list :id (plist-get item :execution-id)) item))
             calls)
     (when (niwa-eval-sandbox-any-live-p session)
       (niwa-eval-sandbox-list-idle session calls))
     (mapcar (lambda (entry) (cons :kind (cons 'wake-up entry)))
             (niwa-session-schedules session)))))

(defun niwa-background-list--agent-state (item)
  "Return the state label of agent ITEM."
  (pcase (plist-get item :status)
    ('blocked "Blocked")
    ('waiting "Waiting")
    (_ "Running")))

(defun niwa-background-list--agent-seconds (item)
  "Return seconds since agent ITEM's current turn started, or nil."
  (when-let* ((started (plist-get item :started-at)))
    (float-time (time-subtract (current-time) started))))

(defun niwa-background-list--execution-state (item)
  "Return the state label of execution ITEM."
  (pcase (plist-get item :state)
    ('queued "Queued")
    ('stopping "Stopping")
    (_ (if (eq (plist-get item :kind) 'monitor) "Watching" "Running"))))

(defun niwa-background-list--entry (item _context)
  "Return the tabulated row for background ITEM."
  (cond
   ((niwa-background-list--wake-up-p item)
    (let ((seconds (max 0 (- (plist-get item :due) (niwa-schedule-now))))
          (pending (eq 'pending (plist-get item :status))))
      (list (plist-get item :id)
            (vector "wake-up" (capitalize (symbol-name (plist-get item :status)))
                    (niwa--truncate-display
                     (replace-regexp-in-string "[\n\r\t]+" " " (plist-get item :note))
                     40 "…")
                    "/root"
                    (niwa-cockpit-number-cell
                     (if pending (concat "in " (niwa--duration-label seconds)) "—")
                     (if pending seconds 0))
                    (format "%s · at %s" (plist-get item :id)
                            (format-time-string "%F %H:%M" (plist-get item :due)))))))
   ((eq (plist-get item :kind) 'sandbox)
    (let ((age (- (float-time) (plist-get item :started-at))))
      (list (plist-get item :id)
            (vector "sandbox" "Idle" "Persistent Eval" (plist-get item :owner)
                    (niwa-cockpit-number-cell (niwa--duration-label age) age)
                    (format "%s features" (plist-get item :features))))))
   ((niwa-background-list--agent-p item)
      (let ((seconds (niwa-background-list--agent-seconds item)))
        (list (plist-get item :id)
              (vector
               "agent"
               (niwa-background-list--agent-state item)
               (plist-get item :path)
               (or (plist-get item :parent-path) "")
               (niwa-cockpit-number-cell
                (if seconds (niwa--duration-label seconds) "—") (or seconds 0))
               (pcase (plist-get item :activity)
                 ('permission-blocked "waiting for permission")
                 ('interaction-blocked "waiting for input")
                 (_ (or (niwa-view-agent-model-label
                         (plist-get item :model-metadata))
                        "")))))))
   (t
    (let ((monitor-p (eq (plist-get item :kind) 'monitor))
          (seconds (or (plist-get item :wall-time-seconds) 0)))
      (list
       (plist-get item :id)
       (vector
        (symbol-name (or (plist-get item :kind) 'bash))
        (niwa-background-list--execution-state item)
        (niwa--truncate-display
         (or (plist-get item :description) (plist-get item :command)
             (and (eq (plist-get item :kind) 'eval) "Eval") "") 40 "…")
        (or (plist-get item :owner) "")
        (niwa-cockpit-number-cell
         (if monitor-p "—" (niwa--duration-label seconds))
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

(defun niwa-background-list--header (items context)
  "Return the cockpit header for background ITEMS and CONTEXT."
  (let* ((session (niwa-cockpit-context-session context))
         (agents (cl-count-if #'niwa-background-list--agent-p items))
         (wake-ups (cl-count-if #'niwa-background-list--wake-up-p items))
         (executions (- (length items) agents wake-ups)))
    (niwa-cockpit-format-header
     "background work"
     (if session (niwa-session-name session) "")
     (format "%d %s · %d %s%s"
             agents (if (= agents 1) "agent" "agents")
             executions (if (= executions 1) "execution" "executions")
             (if (> wake-ups 0)
                 (format " · %d %s" wake-ups (if (= wake-ups 1) "wake-up" "wake-ups"))
               "")))))

(defun niwa-background-list--agent-details (item)
  "Return the information report for agent ITEM."
  (let ((seconds (niwa-background-list--agent-seconds item)))
    (list :title "Agent" :subtitle (plist-get item :path)
          :identity (plist-get item :id)
          :sections
          (list
           (list :id 'agent :title "Agent"
                 :body (concat
                        (niwa-report-fields
                         (list "Agent" (plist-get item :path))
                         (when-let* ((parent (plist-get item :parent-path)))
                           (list "Parent" parent))
                         (list "State" (niwa-background-list--agent-state item))
                         (list "Activity" (format "%s" (plist-get item :activity)))
                         (list "Elapsed" (if seconds (niwa--duration-label seconds) "unknown")))
                        (when-let* ((model (niwa-view-agent-model-details
                                            (plist-get item :model-metadata))))
                          (concat "\n" model))))))))

(defun niwa-background-list--execution-details (item)
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
                        #'niwa-report-fields
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
                              (list "Status" (niwa-background-list--execution-state item))
                              (when-let* ((expires (plist-get item :expires-at)))
                                (list "Expires" (format-time-string
                                                 "%Y-%m-%d %H:%M:%S %Z"
                                                 expires)))
                              (list "Events" (number-to-string (or (plist-get item :event-count) 0))))
                           (list
                            (list "Elapsed" (niwa--duration-label
                                             (or (plist-get item :wall-time-seconds) 0))))))))
           (list :id 'output :title "Output & sandbox"
                 :body (concat
                        (niwa-report-fields
                         (list "Output" (format "%d bytes, %d lines" (or (plist-get item :output-bytes) 0) (or (plist-get item :output-lines) 0)))
                         (list "Artifact" (or (plist-get item :artifact-path) "none")))
                        (if sandbox (niwa-sandbox-status-text sandbox) "sandbox: pending")))
           (list :id 'tail :title "Live output tail"
                 :body (if (string-empty-p tail) "(no output yet)" tail))))))

(defun niwa-background-list--details (item _context)
  "Return the information report for background ITEM."
  (cond
   ((niwa-background-list--wake-up-p item)
    (list :title "Wake-up" :subtitle (plist-get item :id)
          :identity (plist-get item :id)
          :sections
          (list (list :id 'wake-up :title "Wake-up"
                      :body (concat
                             (niwa-report-fields
                              (list "Status" (symbol-name (plist-get item :status)))
                              (when-let* ((reason (plist-get item :pause-reason))) (list "Reason" reason))
                              (list "Time" (niwa-schedule-format-time (plist-get item :due)))
                              (when-let* ((expires (plist-get item :expires-at)))
                                (list "Expires" (niwa-schedule-format-time expires))))
                             "\n" (plist-get item :note) "\n"
                             (mapconcat
                              (lambda (run)
                                (format "%s  %s%s" (niwa-schedule-format-time (plist-get run :time))
                                        (capitalize (symbol-name (plist-get run :outcome)))
                                        (if-let* ((reason (plist-get run :reason)))
                                            (concat ": " reason) "")))
                              (plist-get item :runs) "\n"))))))
   ((eq (plist-get item :kind) 'sandbox)
    (list :title "Idle sandbox" :subtitle (plist-get item :owner)
          :sections (list (list :id 'sandbox :title "Persistent Eval"
                                :body (format "%s features. Stop with k in /ps."
                                              (plist-get item :features))))))
   ((niwa-background-list--agent-p item)
    (niwa-background-list--agent-details item))
   (t (niwa-background-list--execution-details item))))

(defun niwa-background-list--selected-execution ()
  "Return the selected item, signalling unless it is an execution."
  (let ((item (niwa-cockpit-surface-selected)))
    (when (memq (plist-get item :kind) '(agent sandbox wake-up))
      (niwa-diagnostic-user-error (niwa-cockpit-diagnostic-scope)
                                   "Selected item is not an execution"))
    item))

(defun niwa-background-list--call-control (function &rest args)
  "Call execution user control FUNCTION with the selected execution and ARGS."
  (let* ((session (niwa-background-list--session))
         (id (plist-get (niwa-background-list--selected-execution) :execution-id)))
    (condition-case nil
        (apply function session id args)
      (niwa-execution-not-found
       (niwa-cockpit-surface-refresh)
       (niwa-diagnostic-user-error (niwa-diagnostic-scope session)
                                     "Execution %s is no longer live" id)))))

(defun niwa-background-list--interrupt-agent (item)
  "Interrupt agent ITEM's current turn, reporting the outcome."
  (if (niwa-agent-control-interrupt-user
       (niwa-background-list--session) (plist-get item :path))
      (message "niwa: agent %s interrupted" (plist-get item :path))
    (niwa-diagnostic-message (niwa-cockpit-diagnostic-scope)
                                "Agent %s has no active turn" (plist-get item :path)))
  (niwa-cockpit-surface-refresh))

(defun niwa-background-list-send-input ()
  "Send one newline-terminated input line to the selected PTY execution."
  (interactive)
  (unless (plist-get (niwa-background-list--selected-execution) :tty)
    (niwa-diagnostic-user-error (niwa-cockpit-diagnostic-scope)
                                 "Selected execution uses closed pipe input"))
  (niwa-background-list--call-control
   #'niwa-execution-write-user
   (concat (read-string "PTY input: ") "\n")))

(defun niwa-background-list-interrupt ()
  "Interrupt the selected execution's process group or agent's turn."
  (interactive)
  (let ((item (niwa-cockpit-surface-selected)))
    (if (niwa-background-list--agent-p item)
        (niwa-background-list--interrupt-agent item)
      (niwa-background-list--call-control #'niwa-execution-interrupt-user)
      (message "niwa: execution interrupted"))))

(defun niwa-background-list-stop ()
  "Stop the selected execution, or interrupt the selected agent's turn.
An agent stays retained after its turn is interrupted.  A selected wake-up
is cancelled."
  (interactive)
  (let ((item (niwa-cockpit-surface-selected)))
    (cond
     ((niwa-background-list--wake-up-p item)
      (let ((session (niwa-background-list--session)))
        (niwa-schedule-cancel
         session
         (or (niwa-session-root-buffer session)
             (plist-get (niwa-cockpit-surface-context) :data-buffer))
         (plist-get item :id))
        (niwa-cockpit-surface-refresh)
        (message "niwa: wake-up %s cancelled" (plist-get item :id))))
     ((eq (plist-get item :kind) 'sandbox)
      (niwa-eval-sandbox-stop-user (niwa-background-list--session) item))
     ((niwa-background-list--agent-p item)
      (niwa-background-list--interrupt-agent item))
     (t
      (niwa-background-list--call-control #'niwa-execution-stop-user)
      (message "niwa: execution stopping")))))

(defun niwa-background-list-resume ()
  "Resume the selected paused wake-up."
  (interactive)
  (let ((session (niwa-background-list--session)) (item (niwa-cockpit-surface-selected)))
    (unless (and (niwa-background-list--wake-up-p item) (eq 'paused (plist-get item :status)))
      (niwa-diagnostic-user-error (niwa-diagnostic-scope session) "Select a paused wake-up"))
    (niwa-schedule-resume session (niwa-session-root-buffer session) (plist-get item :id))))

(defun niwa-background-list-visit ()
  "Open the selected agent's transcript or execution's live artifact."
  (interactive)
  (let ((item (niwa-cockpit-surface-selected)))
    (if (niwa-background-list--agent-p item)
        (niwa-view-open-agent-transcript (plist-get item :path))
      (let ((path (plist-get item :artifact-path)))
        (unless (and path (file-readable-p path))
          (niwa-diagnostic-user-error (niwa-cockpit-diagnostic-scope)
                                       "Execution artifact is no longer available"))
        (find-file path)))))

(defun niwa-background-list-quit ()
  "Close the background-work cockpit and restore the invoking layout."
  (interactive)
  (niwa-cockpit-quit "background-work cockpit"))

(defconst niwa-background-list--surface
  `(:buffer-name ,niwa-background-list-buffer-name
    :label "background-work cockpit"
    :row-label "background item"
    :mode niwa-background-list-mode
    :format [("Kind" 8 t)
             ("State" 9 t)
             ("Name" 40 t)
             ("Owner" 18 t)
             ("Elapsed" 9 ,(niwa-cockpit-sort-by-number 4))
             ("Details" 0 t)]
    :sort-key nil
    :require-session t
    :collect niwa-background-list--collect
    :entry niwa-background-list--entry
    :header niwa-background-list--header
    :details niwa-background-list--details
    :details-buffer "*niwa background item*"
    :help-buffer ,niwa-background-list-help-buffer-name
    :keys (("i" "Send a line to the selected PTY execution" niwa-background-list-send-input)
           ("C-c C-c" "Interrupt the selected execution or agent turn" niwa-background-list-interrupt)
           ("k" "Stop the selected execution, interrupt the agent's turn, or cancel the wake-up" niwa-background-list-stop)
           ("r" "Resume the selected paused wake-up" niwa-background-list-resume)
           ("o" "Open the agent transcript or execution artifact" niwa-background-list-visit)))
  "Cockpit surface spec for background work.")

(define-derived-mode niwa-background-list-mode tabulated-list-mode
  "niwa-background"
  "Major mode for inspecting and controlling background work."
  (niwa-cockpit-setup-tabulated-surface
   niwa-background-list--surface))

(defun niwa-background-list-open (&optional context)
  "Open the background-work cockpit for CONTEXT."
  (interactive)
  (let ((context (or context (niwa-cockpit-current-context))))
    (niwa-background-list--session context)
    (niwa-cockpit-open-surface niwa-background-list--surface context)))

(defun niwa-background-list-refresh-session (session &optional _data-buffer)
  "Refresh an open background-work cockpit owned by SESSION."
  (when-let* ((buffer (get-buffer niwa-background-list-buffer-name)))
    (with-current-buffer buffer
      (when (and (derived-mode-p 'niwa-background-list-mode)
                 (eq session
                     (condition-case nil
                         (niwa-background-list--session)
                       (user-error nil))))
        (niwa-cockpit-surface-refresh)))))

(defun niwa-background-list-handle-event (event)
  "Refresh the background-work cockpit for immutable execution EVENT."
  (when (memq (plist-get event :type) '(progress yield))
    (niwa-background-list-refresh-session
     (plist-get event :session))))

(add-hook 'niwa-execution-event-functions
          #'niwa-background-list-handle-event)
(add-hook 'niwa-execution-state-change-hook
          #'niwa-background-list-refresh-session)

(provide 'niwa-background-list)
;;; niwa-background-list.el ends here
