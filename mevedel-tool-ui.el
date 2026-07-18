;;; mevedel-tool-ui.el -- User interaction tool assembly -*- lexical-binding: t -*-

;;; Commentary:

;; Assembles the user-interaction tool surface and owns the small adapters for
;; Agent, FollowupAgent, InterruptAgent, ListAgents, ToolSearch, SendMessage, and
;; WaitAgent.
;; Ask lives in a focused tool module; generic, Bash, and Eval prompts live in
;; the permission-prompt module.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'mevedel-tool-registry)
  (require 'subr-x))

;; `mevedel-agent-control'
(declare-function mevedel-agent-control-cancel-wait
                  "mevedel-agent-control" (session path))
(declare-function mevedel-agent-control-followup
                  "mevedel-agent-control" t t)
(declare-function mevedel-agent-control-interrupt
                  "mevedel-agent-control" (session target))
(declare-function mevedel-agent-control-list-agents
                  "mevedel-agent-control" (session &optional path-prefix))
(declare-function mevedel-agent-control-send-message
                  "mevedel-agent-control" (session target message))
(declare-function mevedel-agent-control-spawn
                  "mevedel-agent-control" t t)
(declare-function mevedel-agent-control-wait
                  "mevedel-agent-control" (session callback &optional timeout-ms))
(declare-function mevedel-agent-record-conversation-location
                  "mevedel-agent-control" (cl-x) t)
(declare-function mevedel-agent-record-id
                  "mevedel-agent-control" (cl-x) t)
(declare-function mevedel-agent-record-path
                  "mevedel-agent-control" (cl-x) t)

;; `mevedel-structs'
(declare-function mevedel-request-push-canceller
                  "mevedel-structs" (request canceller))
(declare-function mevedel-session-agent-transcripts
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-permission-queue
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-plan-queue
                  "mevedel-structs" (cl-x) t)
(defvar mevedel--current-request)
(defvar mevedel--session)

;; `mevedel-tool-ask'
(declare-function mevedel-tool-ask-register "mevedel-tool-ask" ())

;; `mevedel-tool-registry'
(declare-function mevedel-tool-truthy-p
                  "mevedel-tool-registry" (value))

;; `mevedel-tools'
(declare-function mevedel-tools--handle-message-inject
                  "mevedel-tools" (fsm))
(declare-function mevedel-tools--handle-terminal-mailbox
                  "mevedel-tools" (fsm))
(declare-function mevedel-tools--tool-search
                  "mevedel-tools" (callback query &optional load))
(defvar mevedel-tools--current-fsm)

;; `mevedel-view'
(declare-function mevedel-view-data-buffer-major-mode "mevedel-view" ())


;;
;;; Agent tool

(defcustom mevedel-tool-ui-agent-description-width 96
  "Maximum display width for task text in Agent handle headers."
  :type 'integer
  :group 'mevedel)

(defun mevedel-tool-ui--deliver-result (callback value &rest args)
  "Deliver VALUE to CALLBACK as a canonical tool result."
  (apply callback
         (if (and (proper-list-p value) (plist-member value :result))
             value
           (list :result value))
         args))

(defun mevedel-tool-ui--compact-agent-description (description &optional width)
  "Return DESCRIPTION normalized to one truncated display line.
WIDTH defaults to `mevedel-tool-ui-agent-description-width'."
  (let ((text (string-trim
               (replace-regexp-in-string
                "[\n\r\t ]+" " " (or description ""))))
        (width (or width mevedel-tool-ui-agent-description-width)))
    (cond
     ((<= width 0) "")
     ((<= width (string-width "..."))
      (truncate-string-to-width "..." width))
     (t
      (truncate-string-to-width text width nil nil "...")))))

(defun mevedel-tool-ui--handle-badge (render-data)
  "Return a propertized state badge for RENDER-DATA, or an empty string."
  (let* ((status (plist-get render-data :status))
         (blocked-reason (plist-get render-data :blocked-reason))
         (calls (plist-get render-data :calls))
         (elapsed (plist-get render-data :elapsed))
         (reason (plist-get render-data :reason))
         (verdict (plist-get render-data :verdict))
         (calls-suffix (if (and calls (> calls 0))
                           (format " · %d calls" calls)
                         ""))
         (elapsed-suffix (if (and elapsed (> elapsed 0))
                             (format " · %.1fs" elapsed)
                           "")))
    (if blocked-reason
        (propertize (format "[blocked · awaiting %s]" blocked-reason)
                    'font-lock-face 'mevedel-view-handle-blocked)
      (pcase status
        ('running
         (propertize (format "[running%s]" calls-suffix)
                     'font-lock-face 'mevedel-view-handle-running))
        ('completed
         (pcase verdict
           ('fail
            (propertize (format "✗ verdict FAIL%s%s"
                                elapsed-suffix calls-suffix)
                        'font-lock-face 'mevedel-view-handle-error))
           ('partial
            (propertize (format "○ verdict PARTIAL%s%s"
                                elapsed-suffix calls-suffix)
                        'font-lock-face 'mevedel-view-handle-error))
           ('pass
            (propertize (format "✓ verdict PASS%s%s"
                                elapsed-suffix calls-suffix)
                        'font-lock-face 'mevedel-view-handle-done))
           (_
            (propertize (format "✓ done%s%s" elapsed-suffix calls-suffix)
                        'font-lock-face 'mevedel-view-handle-done))))
        ('error
         (propertize (format "✗ error%s"
                             (if reason (format " · %s" reason) ""))
                     'font-lock-face 'mevedel-view-handle-error))
        ('aborted
         (propertize "✗ aborted"
                     'font-lock-face 'mevedel-view-handle-error))
        ('incomplete
         (propertize "○ incomplete"
                     'font-lock-face 'mevedel-view-handle-error))
        (_ "")))))

(defun mevedel-tool-ui--agent-blocked-reason (agent-id session)
  "Return the visible blocked reason for AGENT-ID in SESSION, or nil."
  (when (and agent-id session)
    (cond
     ((cl-some (lambda (entry)
                 (equal (plist-get entry :origin) agent-id))
               (mevedel-session-permission-queue session))
      "permission")
     ((cl-some (lambda (entry)
                 (equal (plist-get entry :origin) agent-id))
               (mevedel-session-plan-queue session))
      "plan"))))

(defun mevedel-tool-ui--agent (callback args)
  "Launch the agent described by ARGS and report through CALLBACK."
  (dolist (obsolete '(:subagent_type :description :prompt :run_in_background))
    (when (plist-member args obsolete)
      (error "Superseded Agent parameter: %s" obsolete)))
  (let ((task-name (plist-get args :task_name))
        (message (plist-get args :message))
        (role (plist-get args :role)))
    (require 'json)
    (require 'mevedel-agent-control)
    (let* ((record
            (mevedel-agent-control-spawn
             mevedel--session task-name message
             :role role
             :parent-fsm (and (boundp 'mevedel-tools--current-fsm)
                              mevedel-tools--current-fsm)
             :message-handler #'mevedel-tools--handle-message-inject
             :terminal-handler #'mevedel-tools--handle-terminal-mailbox))
           (path (mevedel-agent-record-path record)))
      (mevedel-tool-ui--deliver-result
       callback
       (list :result (json-serialize (list :path path))
             :render-data
             (list :kind 'collaboration-event
                   :event 'started
                   :path path
                   :agent-id (mevedel-agent-record-id record)
                   :transcript-relative-path
                   (mevedel-agent-record-conversation-location record)
                   :status 'running))))))

(defun mevedel-tool-ui--followup-agent (args)
  "Continue or steer the retained agent described by ARGS."
  (require 'mevedel-agent-control)
  (let* ((record
          (mevedel-agent-control-followup
           mevedel--session
           (plist-get args :target)
           (plist-get args :message)
           :parent-fsm (and (boundp 'mevedel-tools--current-fsm)
                            mevedel-tools--current-fsm)
           :message-handler #'mevedel-tools--handle-message-inject
           :terminal-handler #'mevedel-tools--handle-terminal-mailbox))
         (path (mevedel-agent-record-path record)))
    (list :result ""
          :render-data
          (list :kind 'collaboration-event
                :event 'interacted
                :path path))))

(defun mevedel-tool-ui--list-agents (args)
  "Return the retained agent roster described by ARGS."
  (require 'json)
  (require 'mevedel-agent-control)
  (list :result
        (json-serialize
         (vconcat
          (mevedel-agent-control-list-agents
           mevedel--session (plist-get args :path_prefix))))))

(defun mevedel-tool-ui--interrupt-agent (args)
  "Interrupt the retained agent turn described by ARGS."
  (require 'json)
  (require 'mevedel-agent-control)
  (let* ((result
          (mevedel-agent-control-interrupt
           mevedel--session (plist-get args :target)))
         (path (plist-get result :path)))
    (list :result
          (json-serialize
           (list :previous_activity
                 (symbol-name (plist-get result :previous-activity))))
          :render-data
          (list :kind 'collaboration-event
                :event 'interrupted
                :path path))))

(defun mevedel-tool-ui--tool-search (callback args)
  "Search for deferred tools described by ARGS and call CALLBACK."
  (let ((query (plist-get args :query))
        (load (plist-get args :load)))
    (unless (stringp query)
      (error "Parameter query is required"))
    (mevedel-tools--tool-search
     (apply-partially #'mevedel-tool-ui--deliver-result callback)
     query load)))

(defun mevedel-tool-ui--send-message (args)
  "Queue the SendMessage described by ARGS."
  (require 'mevedel-agent-control)
  (let ((path
         (mevedel-agent-control-send-message
          mevedel--session
          (plist-get args :target)
          (plist-get args :message))))
    (list :result ""
          :render-data
          (list :kind 'collaboration-event
                :event 'interacted
                :path path))))

(defun mevedel-tool-ui--wait-summary (reason)
  "Return the model-visible WaitAgent summary for REASON."
  (pcase reason
    ('mailbox "Mailbox activity")
    ('steering "Follow-up steering received")
    ('user "User steering received")
    ('timeout "Timeout elapsed")
    (_ "Wait finished")))

(defun mevedel-tool-ui--wait-agent (callback args)
  "Suspend WaitAgent ARGS and report the wake reason through CALLBACK."
  (require 'mevedel-agent-control)
  (when-let* ((path
               (mevedel-agent-control-wait
                mevedel--session
                (lambda (reason)
                  (mevedel-tool-ui--deliver-result
                   callback
                   (list :result (mevedel-tool-ui--wait-summary reason)
                         :render-data
                         (list :kind 'collaboration-event
                               :event 'finished-waiting
                               :reason reason))))
                (plist-get args :timeout_ms))))
    (when (and (equal path "/root")
               (boundp 'mevedel--current-request)
               mevedel--current-request)
      (mevedel-request-push-canceller
       mevedel--current-request
       (apply-partially
        #'mevedel-agent-control-cancel-wait mevedel--session path)))))


;;
;;; Renderers

(defun mevedel-tool-ui--render-agent (name args result render-data)
  "Return rendering plist for Agent NAME, ARGS, RESULT, and RENDER-DATA."
  (when (stringp result)
    (if (plist-get args :task_name)
        (let ((path (plist-get render-data :path)))
          (list :header (format "Started %s" path)
                :body result
                :body-mode nil
                :vtype 'agent-handle
                :agent-id (plist-get render-data :agent-id)
                :agent-status (plist-get render-data :status)
                :initially-collapsed-p t))
      (let* ((agent-id (and (consp render-data)
                          (plist-get render-data :agent-id)))
           (session (and (boundp 'mevedel--session) mevedel--session))
           (sidecar-entry
            (and agent-id session
                 (cdr (assoc agent-id
                             (mevedel-session-agent-transcripts session)))))
           (effective-render-data
            (if sidecar-entry
                (append (list :status (plist-get sidecar-entry :status)
                              :transcript-relative-path
                              (plist-get sidecar-entry :path))
                        render-data)
              render-data))
           (blocked-reason
            (and (eq (plist-get effective-render-data :status) 'running)
                 (mevedel-tool-ui--agent-blocked-reason agent-id session)))
           (progress-p (plist-get effective-render-data :progress-handle))
           (agent-type (or (plist-get args :subagent_type) "?"))
           (badge (mevedel-tool-ui--handle-badge
                   (if blocked-reason
                       (plist-put (copy-sequence effective-render-data)
                                  :blocked-reason blocked-reason)
                     effective-render-data)))
           (badge-suffix (if (string-empty-p badge) "" (concat "  " badge)))
           (description (or (plist-get args :description) ""))
           (header-width (plist-get effective-render-data :header-width))
           (description-width
            (when header-width
              (let* ((base (if progress-p "" (format "%s -- " agent-type)))
                     (fixed (format "%s: %s%s"
                                    (or name "Agent") base badge-suffix)))
                (max 0 (- header-width (string-width fixed))))))
           (compact-description
            (mevedel-tool-ui--compact-agent-description
             description description-width))
           (shown (cond
                   ((and progress-p (not (string-empty-p compact-description)))
                    compact-description)
                   ((string-empty-p compact-description) agent-type)
                   (t (format "%s -- %s" agent-type compact-description)))))
        (list :header (format "%s: %s%s"
                              (or name "Agent") shown badge-suffix)
              :body result
              :body-mode (mevedel-view-data-buffer-major-mode)
              :vtype 'agent-handle
              :agent-id agent-id
              :agent-status (plist-get effective-render-data :status)
              :agent-description description
              :hook-audits (plist-get effective-render-data :hook-audits)
              :initially-collapsed-p t)))))

(defun mevedel-tool-ui--render-agent-interaction
    (_name _args result render-data)
  "Render an agent interaction RESULT from RENDER-DATA."
  (when (and (stringp result)
             (stringp (plist-get render-data :path)))
    (list :header (format "Interacted with %s"
                          (plist-get render-data :path))
          :expandable-p nil)))

(defun mevedel-tool-ui--result-status (result)
  "Return a renderer status for RESULT."
  (and (stringp result)
       (string-prefix-p "Error:" result)
       'error))

(defun mevedel-tool-ui--line-count (result)
  "Return non-empty line count for RESULT."
  (if (stringp result)
      (length (split-string result "\n" t))
    0))

(defun mevedel-tool-ui--render-interrupt-agent
    (_name _args result render-data)
  "Render an interrupted-agent RESULT from RENDER-DATA."
  (when (and (stringp result)
             (eq (plist-get render-data :event) 'interrupted)
             (stringp (plist-get render-data :path)))
    (list :header (format "Interrupted %s"
                          (plist-get render-data :path))
          :expandable-p nil)))

(defun mevedel-tool-ui--render-tool-search (name args result _render-data)
  "Return rendering plist for ToolSearch NAME, ARGS, and RESULT."
  (when (stringp result)
    (let* ((query (or (plist-get args :query) ""))
           (load (mevedel-tool-truthy-p (plist-get args :load)))
           (count (mevedel-tool-ui--line-count result)))
      (list :header (format "%s: %s (%s, %d %s)"
                            (or name "ToolSearch") query
                            (if load "load" "search")
                            count (if (= count 1) "line" "lines"))
            :body result
            :body-mode nil
            :status (mevedel-tool-ui--result-status result)
            :initially-collapsed-p t))))

(defun mevedel-tool-ui--render-wait-agent
    (_name _args result render-data)
  "Render completed WaitAgent RESULT from RENDER-DATA."
  (when (and (stringp result)
             (eq (plist-get render-data :event) 'finished-waiting))
    (list :header "Finished waiting"
          :expandable-p nil)))


;;
;;; Tool assembly

(defun mevedel-tool-ui--register ()
  "Register the user-interaction tool surface."
  (require 'mevedel-tool-ask)
  (mevedel-tool-ask-register)
  (mevedel-define-tool
    :name "Agent"
    :description "Start a retained asynchronous child agent."
    :prompt-file "tools/agent.md"
    :handler #'mevedel-tool-ui--agent
    :args ((task_name string :required
                      "Lowercase ASCII name for the new child path segment.")
           (message string :required
                    "Complete non-empty task for the child agent.")
           (role string :optional
                 "Named role overlay. Omit to inherit the delegator."
                 :enum []))
    :async-p t
    :max-result-size 50000
    :groups (util)
    :get-name (lambda (args) (plist-get args :task_name))
    :read-only-p t
    :renderer #'mevedel-tool-ui--render-agent)
  (mevedel-define-tool
    :name "FollowupAgent"
    :description "Continue or steer one retained non-root agent."
    :prompt-file "tools/followupagent.md"
    :handler #'mevedel-tool-ui--followup-agent
    :args ((target string :required
                   "Canonical path or relative descendant path.")
           (message string :required
                    "Complete non-empty follow-up task."))
    :groups (util)
    :get-name (lambda (args) (plist-get args :target))
    :read-only-p t
    :renderer #'mevedel-tool-ui--render-agent-interaction)
  (mevedel-define-tool
    :name "ListAgents"
    :description "List retained agent paths, roles, and activity."
    :prompt-file "tools/listagents.md"
    :handler #'mevedel-tool-ui--list-agents
    :args ((path_prefix string :optional
                        "Canonical subtree path prefix."))
    :groups (util)
    :read-only-p t)
  (mevedel-define-tool
    :name "InterruptAgent"
    :description "Interrupt one retained agent's current turn."
    :prompt-file "tools/interruptagent.md"
    :handler #'mevedel-tool-ui--interrupt-agent
    :args ((target string :required
                   "Canonical path or relative descendant path."))
    :groups (util)
    :read-only-p t
    :renderer #'mevedel-tool-ui--render-interrupt-agent)
  (mevedel-define-tool
    :name "ToolSearch"
    :description "Search for and load deferred tools before using them."
    :prompt-file "tools/toolsearch.md"
    :handler #'mevedel-tool-ui--tool-search
    :args ((query string :required
                  "Search query: tool name or capability description.")
           (load boolean :optional
                 "Set true when you intend to call the matched tool; it becomes available now for your next tool call."))
    :async-p t
    :read-only-p t
    :groups (util)
    :renderer #'mevedel-tool-ui--render-tool-search)
  (mevedel-define-tool
    :name "SendMessage"
    :description "Queue a message for any retained agent without starting a turn."
    :prompt-file "tools/sendmessage.md"
    :handler #'mevedel-tool-ui--send-message
    :args ((target string :required
                   "Canonical path or relative descendant path.")
           (message string :required
                    "Message body to deliver."))
    :read-only-p t
    :groups (util)
    :renderer #'mevedel-tool-ui--render-agent-interaction)
  (mevedel-define-tool
    :name "WaitAgent"
    :description "Wait for mailbox activity, user steering, or timeout."
    :prompt-file "tools/waitagent.md"
    :handler #'mevedel-tool-ui--wait-agent
    :args ((timeout_ms integer :optional
                       "Timeout in milliseconds; defaults to 30000."))
    :async-p t
    :read-only-p t
    :groups (util)
    :renderer #'mevedel-tool-ui--render-wait-agent))

(provide 'mevedel-tool-ui)

;;; mevedel-tool-ui.el ends here
