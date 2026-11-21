;;; niwa-tool-ui.el -- User interaction tool assembly -*- lexical-binding: t -*-

;;; Commentary:

;; Assembles the user-interaction tool surface and owns the small adapters for
;; Agent, InterruptAgent, ListAgents, ToolSearch, SendMessage, and
;; WaitAgent.
;; Ask lives in a focused tool module; generic, Bash, and Eval prompts live in
;; the permission-prompt module.

;;; Code:

(eval-when-compile
  (require 'niwa-tool-registry)
  (require 'subr-x))

(require 'json)
(require 'niwa-agent-control)
(require 'niwa-turn)

;; `niwa-agents'
(declare-function niwa-agents-agent-contract
                  "niwa-agents" (args summary))
(autoload 'niwa-agents-agent-contract "niwa-agents")

;; `niwa-engine'
(declare-function niwa-engine-info "niwa-engine" (owner))

;; `niwa-mcp'
(defvar niwa-mcp-client-idle-timeout)

;; `niwa-pipeline'
(declare-function niwa-pipeline-active-tool-use-id
                  "niwa-pipeline" ())
(autoload 'niwa-pipeline-active-tool-use-id "niwa-pipeline")

;; `niwa-structs'
(defvar niwa--current-request)
(defvar niwa--session)

;; `niwa-tool-ask'
(declare-function niwa-tool-ask-register "niwa-tool-ask" ())

;; `niwa-tools'
(declare-function niwa-tools--tool-search
                  "niwa-tools" (callback query))
(defvar niwa-tools--current-engine)

;; `niwa-view-agent'
(declare-function niwa-view-agent-model-details
                  "niwa-view-agent" (metadata))
(declare-function niwa-view-agent-model-label
                  "niwa-view-agent" (metadata &optional full))
(autoload 'niwa-view-agent-model-details "niwa-view-agent")
(autoload 'niwa-view-agent-model-label "niwa-view-agent")


;;
;;; Agent tool

(defun niwa-tool-ui--deliver-result (callback value &rest args)
  "Deliver VALUE to CALLBACK as a canonical tool result."
  (apply callback
         (if (and (proper-list-p value) (plist-member value :result))
             value
           (list :result value))
         args))

(defun niwa-tool-ui--optional-string (args key)
  "Return optional argument KEY from ARGS, treating an empty string as omitted."
  (let ((value (plist-get args key)))
    (unless (equal value "") value)))

(defun niwa-tool-ui--agent (callback args)
  "Launch the agent described by ARGS and report through CALLBACK."
  (let ((task-name (plist-get args :task_name))
        (message (plist-get args :message))
        (role (niwa-tool-ui--optional-string args :role))
        (context (niwa-tool-ui--optional-string args :context))
        (model (niwa-tool-ui--optional-string args :model))
        (effort (niwa-tool-ui--optional-string args :effort)))
    (let ((cancel
           (niwa-agent-control-spawn
            niwa--session task-name message
            (lambda (outcome)
              (pcase (plist-get outcome :outcome)
                ('success
                 (let* ((record (plist-get outcome :record))
                        (path (niwa-agent-record-path record)))
                   (niwa-tool-ui--deliver-result
                    callback
                    (list
                     :result (json-serialize (list :path path))
                     :render-data
                     (list :kind 'collaboration-event
                           :event 'started
                           :path path
                           :agent-id (niwa-agent-record-id record)
                           :transcript-relative-path
                           (niwa-agent-record-conversation-location record)
                           :summary-metadata
                           (plist-get outcome :summary-metadata)
                           :model-metadata
                           (copy-tree (plist-get outcome :model-metadata))
                           :status 'running)))))
                ('error
                 (niwa-tool-ui--deliver-result
                  callback (format "Error: %s" (plist-get outcome :error))))
                ('aborted
                 (niwa-tool-ui--deliver-result
                  callback "Error: Agent preparation aborted"))))
            :role role
            :context context
            :model model
            :effort effort
            :working-directory (niwa-tool-ui--optional-string args :working_directory)
            :parent-tool-use-id
            (niwa-pipeline-active-tool-use-id))))
      (when (and (boundp 'niwa--current-request)
                 niwa--current-request)
        (niwa-request-push-canceller niwa--current-request cancel)))))

(defun niwa-tool-ui--list-agents (args)
  "Return the retained agent roster described by ARGS."
  (list :result
        (json-serialize
         (vconcat
          (niwa-agent-control-list-agents
           niwa--session (niwa-tool-ui--optional-string args :path_prefix))))))

(defun niwa-tool-ui--interrupt-agent (args)
  "Interrupt the retained agent turn described by ARGS."
  (let* ((result
          (niwa-agent-control-interrupt
           niwa--session (plist-get args :target)))
         (path (plist-get result :path)))
    (list :result
          (json-serialize
           (list :previous_activity
                 (symbol-name (plist-get result :previous-activity))))
          :render-data
          (list :kind 'collaboration-event
                :event 'interrupted
                :path path))))

(defun niwa-tool-ui--tool-search (callback args)
  "Retrieve tool contracts described by ARGS and call CALLBACK."
  (niwa-tools--tool-search
   (apply-partially #'niwa-tool-ui--deliver-result callback)
   (plist-get args :query)))

(defun niwa-tool-ui--send-message (args)
  "Deliver the SendMessage described by ARGS."
  (let* ((outcome
          (niwa-agent-control-send-message
           niwa--session
           (niwa-tool-ui--optional-string args :target)
           (plist-get args :message)
           :parent-tool-use-id
           (niwa-pipeline-active-tool-use-id)
           :working-directory (niwa-tool-ui--optional-string args :working_directory)))
         (path (plist-get outcome :path))
         (delivery (plist-get outcome :delivery)))
    (list :result (format (pcase-exhaustive delivery
                            ('started "Delivered to %s; its turn started.")
                            ('queued "Queued for %s.")
                            ('waiting "Queued for %s; it starts when a slot frees."))
                          path)
          :render-data
          (list :kind 'collaboration-event
                :event 'interacted
                :path path
                :delivery delivery))))

(defun niwa-tool-ui--wait-summary (reason)
  "Return the model-visible WaitAgent summary for REASON."
  (pcase reason
    ('mailbox "Mailbox activity")
    ('user "User steering received")
    ('timeout "Timeout elapsed")
    (_ "Wait finished")))

(defun niwa-tool-ui--wait-agent (callback args)
  "Suspend WaitAgent ARGS and report the wake reason through CALLBACK.
When the owning turn's MCP client accepts no progress notifications, the wait
ends before `niwa-mcp-client-idle-timeout' so the client does not abort it."
  (let* ((requested (or (plist-get args :timeout_ms)
                        niwa-agent-control--wait-default-ms))
         (limit (and niwa-tools--current-engine
                     (plist-get (niwa-engine-info niwa-tools--current-engine)
                                :niwa-mcp-idle-bounded)
                     (floor (* 900 niwa-mcp-client-idle-timeout))))
         (cut (and limit (< limit requested))))
    (when-let* ((path
                 (niwa-agent-control-wait
                  niwa--session
                  (lambda (reason)
                    (niwa-tool-ui--deliver-result
                     callback
                     (list :result
                           (if (and cut (eq reason 'timeout))
                               "Still waiting: the MCP client's idle limit ended this call. Call WaitAgent again to keep waiting."
                             (niwa-tool-ui--wait-summary reason))
                           :render-data
                           (list :kind 'collaboration-event
                                 :event 'finished-waiting
                                 :reason reason))))
                  (if cut limit requested))))
      (let* ((session niwa--session)
             (waiter (niwa-agent-control--waiter session path))
             (cancel (lambda ()
                       (when (eq waiter
                                 (niwa-agent-control--waiter session path))
                         (niwa-agent-control-cancel-wait session path)))))
        (when niwa--current-request
          (niwa-request-push-canceller niwa--current-request cancel))
        (list :cancel cancel)))))


;;
;;; Renderers

(defun niwa-tool-ui--render-agent (_name args result render-data)
  "Return Agent rendering for RESULT and RENDER-DATA."
  (cond
   ((and (equal (plist-get args :context) "summary")
         (or (null result) (equal result ""))
         (null render-data))
    (list :header
          (format "Preparing summary context... %s"
                  (or (plist-get args :task_name) "agent"))
          :expandable-p nil
          :vtype 'agent-preparation))
   ((and (stringp result)
         (eq 'collaboration-event (plist-get render-data :kind))
         (eq 'started (plist-get render-data :event))
         (stringp (plist-get render-data :path)))
    (let* ((path (plist-get render-data :path))
           (metadata (plist-get render-data :model-metadata))
           (model (niwa-view-agent-model-label metadata))
           (summary (plist-get render-data :summary-metadata))
           (header
            (concat
             (format "Started %s" path)
             (when model (concat " · " model))
             (when summary
               (format " · summary %s/%s%s"
                       (plist-get summary :backend)
                       (plist-get summary :model)
                       (if-let* ((effort (plist-get summary :effort)))
                           (format " (%s)" effort)
                         ""))))))
      (list :header header
            :body (string-join
                   (delq nil (list (when-let* ((details (niwa-view-agent-model-details metadata)))
                                     (concat "At launch\n" details))
                                   result))
                   "\n\n")
            :body-mode nil
            :vtype 'agent-handle
            :agent-path path
            :agent-id (plist-get render-data :agent-id)
            :agent-status (plist-get render-data :status)
            :initially-collapsed-p t)))))

(defun niwa-tool-ui--render-agent-interaction
    (name args result render-data)
  "Render an agent interaction RESULT from RENDER-DATA."
  (when (and (stringp result)
             (stringp (plist-get render-data :path)))
    (let ((path (plist-get render-data :path)))
      (list :header (format "%s: %s (%s)"
                            (or name "SendMessage") path
                            (pcase (plist-get render-data :delivery)
                              ('started "started")
                              ('waiting "waiting for a slot")
                              (_ "message queued")))
            :body (or (plist-get args :message) "")
            :body-mode nil
            :agent-path path
            :initially-collapsed-p t))))

(defun niwa-tool-ui--render-list-agents
    (name _args result _render-data)
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
          (list :header (format "%s: session (%d %s)"
                                (or name "ListAgents") (length rows)
                                (if (= (length rows) 1) "agent" "agents"))
                :body
                (mapconcat
                 (lambda (row) (apply #'format row-format row))
                 (cons '("Path" "Role" "Activity") rows)
                 "\n")
                :body-mode nil
                :initially-collapsed-p t))
      (error nil))))

(defun niwa-tool-ui--render-interrupt-agent
    (name _args result render-data)
  "Render an interrupted-agent RESULT from RENDER-DATA."
  (when (and (stringp result)
             (eq (plist-get render-data :event) 'interrupted)
             (stringp (plist-get render-data :path)))
    (list :header (format "%s: %s (interrupted)"
                          (or name "InterruptAgent")
                          (plist-get render-data :path))
          :expandable-p nil)))

(defun niwa-tool-ui--render-tool-search (name args result _render-data)
  "Return rendering plist for ToolSearch NAME, ARGS, and RESULT."
  (when (stringp result)
    (let* ((query (or (plist-get args :query) ""))
           (count (length (split-string result "\n" t))))
      (list :header (format "%s: %s (search, %d %s)"
                            (or name "ToolSearch") query
                            count (if (= count 1) "line" "lines"))
            :body result
            :body-mode nil
            :status (and (string-prefix-p "Error:" result) 'error)
            :initially-collapsed-p t))))

(defun niwa-tool-ui--render-wait-agent
    (name _args result render-data)
  "Render completed WaitAgent RESULT from RENDER-DATA."
  (when (and (stringp result)
             (eq (plist-get render-data :event) 'finished-waiting))
    (list :header (format "%s: agents (%s)" (or name "WaitAgent") result)
          :expandable-p nil
          :coalesce-key "WaitAgent")))


;;
;;; Tool assembly

(defun niwa-tool-ui--register ()
  "Register the user-interaction tool surface."
  (require 'niwa-tool-ask)
  (niwa-tool-ask-register)
  (niwa-define-tool
    :name "Agent"
    :description "Start a retained asynchronous child agent; omit role for general-purpose delegation with inherited capabilities."
    :prompt-file "prompts/tools/agent.md"
    :handler #'niwa-tool-ui--agent
    :contextualize #'niwa-agents-agent-contract
    :args ((task_name string :required
                      "Free-form task name. The harness derives a unique lowercase ASCII path segment and returns the canonical path; collisions get numeric suffixes.")
           (message string :required
                    "Complete non-empty task for the child agent.")
           (role string :optional
                 "Omit for general-purpose delegation with the delegator's effective instructions and capabilities. Select a named role for its specialization."
                 :enum [])
           (context string :optional
                       "Parent context. Defaults to none; use summary for task-focused generated background, positive last-N for recent dialogue, or all for the complete conversation. Copied turns retain model-visible roles and may contain actionable instructions.")
           (model string :optional
                  "Configured tier or exact BACKEND:MODEL override.")
           (effort string :optional
                   "Reasoning-effort override validated for the model.")
           (working_directory string :optional
                              "Existing directory, absolute or relative to the parent agent's directory. Must be in the session workspace or a registered Git worktree of its repository on the same target. Inherited by descendants; does not grant filesystem authority."))
    :async-p t
    :max-result-size 50000
    :groups (agents)
    :get-name (lambda (args) (plist-get args :task_name))
    :read-only-p t
    :renderer #'niwa-tool-ui--render-agent)
  (niwa-define-tool
    :name "ListAgents"
    :description "List retained agent paths, roles, and activity."
    :prompt-file "prompts/tools/listagents.md"
    :handler #'niwa-tool-ui--list-agents
    :args ((path_prefix string :optional
                        "Canonical subtree path prefix. Omit to list all."))
    :groups (agents)
    :read-only-p t
    :renderer '((success . niwa-tool-ui--render-list-agents)))
  (niwa-define-tool
    :name "InterruptAgent"
    :description "Interrupt one retained agent's current turn."
    :prompt-file "prompts/tools/interruptagent.md"
    :handler #'niwa-tool-ui--interrupt-agent
    :args ((target string :required
                   "Canonical path or relative descendant path."))
    :groups (agents)
    :read-only-p t
    :renderer '((success . niwa-tool-ui--render-interrupt-agent)))
  (niwa-define-tool
    :name "ToolSearch"
    :description "Find specialist tool contracts and calling expressions."
    :prompt-file "prompts/tools/toolsearch.md"
    :handler #'niwa-tool-ui--tool-search
    :args ((query string :required
                  "One or more tool names or capability terms; matches any term."))
    :async-p t
    :read-only-p t
    :groups (util)
    :renderer '((success . niwa-tool-ui--render-tool-search)))
  (niwa-define-tool
    :name "SendMessage"
    :description "Send a message to a retained agent, starting its turn if idle."
    :prompt-file "prompts/tools/sendmessage.md"
    :handler #'niwa-tool-ui--send-message
    :args ((target string :optional
                   "Omit to message your immediate parent. Root must supply a target. Explicit canonical or relative descendant paths can address any retained agent.")
           (message string :required
                    "Message body to deliver.")
           (working_directory string :optional
                              "Existing directory for the retained agent's later turns, validated like Agent's. Use it when the agent's directory was removed or its next task lives in another worktree."))
    :read-only-p t
    :groups (agents)
    :renderer '((success . niwa-tool-ui--render-agent-interaction)))
  (niwa-define-tool
    :name "WaitAgent"
    :description "Wait for mailbox activity, user steering, or timeout. Only RESULT delivery is terminal."
    :prompt-file "prompts/tools/waitagent.md"
    :handler #'niwa-tool-ui--wait-agent
    :args ((timeout_ms integer :optional
                       "Timeout in milliseconds; omitted values default to 30000, out-of-range numeric values clamp to 10000-3600000, and unrepairable values are rejected."
                       :minimum 10000 :maximum 3600000))
    :async-p t
    :read-only-p t
    :groups (agents)
    :renderer '((success . niwa-tool-ui--render-wait-agent))))

(provide 'niwa-tool-ui)

;;; niwa-tool-ui.el ends here
