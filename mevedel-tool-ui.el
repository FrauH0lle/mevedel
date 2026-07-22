;;; mevedel-tool-ui.el -- User interaction tool assembly -*- lexical-binding: t -*-

;;; Commentary:

;; Assembles the user-interaction tool surface and owns the small adapters for
;; Agent, FollowupAgent, InterruptAgent, ListAgents, ToolSearch, SendMessage, and
;; WaitAgent.
;; Ask lives in a focused tool module; generic, Bash, and Eval prompts live in
;; the permission-prompt module.

;;; Code:

(eval-when-compile
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
(defvar mevedel--current-request)
(defvar mevedel--session)

;; `mevedel-tool-ask'
(declare-function mevedel-tool-ask-register "mevedel-tool-ask" ())

;; `mevedel-tool-registry'
(declare-function mevedel-tool-truthy-p
                  "mevedel-tool-registry" (value))

;; `mevedel-tools'
(declare-function mevedel-tools--tool-search
                  "mevedel-tools" (callback query &optional load))


;;
;;; Agent tool

(defun mevedel-tool-ui--deliver-result (callback value &rest args)
  "Deliver VALUE to CALLBACK as a canonical tool result."
  (apply callback
         (if (and (proper-list-p value) (plist-member value :result))
             value
           (list :result value))
         args))

(defun mevedel-tool-ui--agent (callback args)
  "Launch the agent described by ARGS and report through CALLBACK."
  (let ((task-name (plist-get args :task_name))
        (message (plist-get args :message))
        (role (plist-get args :role))
        (fork-turns (plist-get args :fork_turns))
        (model (plist-get args :model))
        (effort (plist-get args :effort)))
    (require 'json)
    (require 'mevedel-agent-control)
    (let* ((record
            (mevedel-agent-control-spawn
             mevedel--session task-name
             message
             :role role
             :fork-turns fork-turns
             :model model
             :effort effort))
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
           (plist-get args :message)))
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
  (let ((path-prefix (plist-get args :path_prefix)))
    (list :result
          (json-serialize
           (vconcat
            (mevedel-agent-control-list-agents
             mevedel--session
             (unless (equal path-prefix "") path-prefix)))))))

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

(defun mevedel-tool-ui--render-agent (_name _args result render-data)
  "Return Agent rendering for RESULT and RENDER-DATA."
  (when (and (stringp result)
             (eq 'collaboration-event (plist-get render-data :kind))
             (eq 'started (plist-get render-data :event))
             (stringp (plist-get render-data :path)))
    (let ((path (plist-get render-data :path)))
      (list :header (format "Started %s" path)
            :body result
            :body-mode nil
            :vtype 'agent-handle
            :agent-path path
            :agent-id (plist-get render-data :agent-id)
            :agent-status (plist-get render-data :status)
            :initially-collapsed-p t))))

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

(defun mevedel-tool-ui--render-list-agents
    (_name _args result _render-data)
  "Render the ListAgents RESULT as a compact table."
  (when (stringp result)
    (condition-case nil
        (let* ((agents (json-parse-string
                        result :array-type 'list :object-type 'plist))
               (rows
                (mapcar
                 (lambda (agent)
                   (let ((path (plist-get agent :path))
                         (role (plist-get agent :role))
                         (activity (plist-get agent :activity)))
                     (unless (and (stringp path)
                                  (stringp role)
                                  (stringp activity))
                       (error "Invalid agent roster"))
                     (list path role activity)))
                 agents))
               (path-width
                (apply #'max (length "Path")
                       (mapcar (lambda (row) (length (car row))) rows)))
               (role-width
                (apply #'max (length "Role")
                       (mapcar (lambda (row) (length (cadr row))) rows)))
               (row-format
                (format "%%-%ds  %%-%ds  %%s" path-width role-width)))
          (list :header (format "Session agents (%d)" (length rows))
                :body
                (mapconcat
                 (lambda (row) (apply #'format row-format row))
                 (cons '("Path" "Role" "Activity") rows)
                 "\n")
                :body-mode nil
                :initially-collapsed-p t))
      (error nil))))

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
                 :enum [])
           (fork_turns string :optional
                       "Parent context to copy: all, none, or positive last-N.")
           (model string :optional
                  "Configured tier or exact BACKEND:MODEL override.")
           (effort string :optional
                   "Reasoning-effort override validated for the model."))
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
                        "Canonical subtree path prefix. Omit to list all."))
    :groups (util)
    :read-only-p t
    :renderer #'mevedel-tool-ui--render-list-agents)
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
