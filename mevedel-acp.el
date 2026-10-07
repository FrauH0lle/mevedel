;;; mevedel-acp.el --- External agent conversation lifecycle -*- lexical-binding: t -*-

;;; Commentary:
;; One retained ACP conversation, using acp.el for protocol and process I/O.
;; Owns startup, conversation identity, one active prompt, cancellation and
;; terminal delivery.  Tool effects belong to mevedel's MCP pipeline.  Agent
;; filesystem/terminal requests are never an alternate execution path.

;;; Code:

(require 'acp)
(require 'cl-lib)
(require 'seq)
(require 'mevedel-transport)

(cl-defstruct (mevedel-acp (:constructor mevedel-acp--create))
  "Runtime state for one external model conversation."
  client launch session-id capabilities commands prepared
  (state 'starting) active timer prepare-cancel ready failure)

(defconst mevedel-acp--control-timeout 30
  "Seconds allowed for startup and cancellation acknowledgement.")

(defun mevedel-acp--cancel-timer (connection)
  "Cancel CONNECTION's pending control timeout."
  (when-let* ((timer (mevedel-acp-timer connection)))
    (cancel-timer timer)
    (setf (mevedel-acp-timer connection) nil)))

(defun mevedel-acp--finish (connection turn outcome)
  "Deliver OUTCOME once for CONNECTION's captured TURN."
  (when (eq turn (mevedel-acp-active connection))
    (mevedel-acp--cancel-timer connection)
    (setf (mevedel-acp-active connection) nil)
    (unless (eq (mevedel-acp-state connection) 'closed)
      (setf (mevedel-acp-state connection) 'idle))
    (when-let* ((normalize (plist-get (mevedel-acp-launch connection)
                                      :normalize-outcome)))
      (setq outcome
            (condition-case err
                (funcall normalize outcome)
              (error (list :status 'error
                           :message (format "Invalid agent outcome: %s"
                                            (error-message-string err)))))))
    (funcall (plist-get turn :complete) outcome)))

(defun mevedel-acp--shutdown (connection outcome)
  "Close CONNECTION and deliver terminal OUTCOME to its owner."
  (unless (eq (mevedel-acp-state connection) 'closed)
    (let ((starting (eq (mevedel-acp-state connection) 'starting)))
      (setf (mevedel-acp-state connection) 'closed)
      (mevedel-acp--cancel-timer connection)
      (when-let* ((cancel (mevedel-acp-prepare-cancel connection)))
        (setf (mevedel-acp-prepare-cancel connection) nil)
        (funcall cancel))
      (acp-shutdown :client (mevedel-acp-client connection))
      (cond
       (starting (funcall (mevedel-acp-failure connection)
                          (or (plist-get outcome :message) "ACP connection closed")))
       ((mevedel-acp-active connection)
        (mevedel-acp--finish connection (mevedel-acp-active connection) outcome))))))

(defun mevedel-acp--fail (connection error)
  "Close CONNECTION after protocol or process ERROR."
  (mevedel-acp--shutdown
   connection (list :status 'error :message
                    (if (stringp error) error
                      (or (alist-get 'message error) (format "%S" error))))))

(defun mevedel-acp--live-p (connection)
  "Return non-nil unless CONNECTION's started agent process died.
acp.el restarts a dead client on send, outside this connection's launch,
so a dead process fails the connection instead."
  (let ((process (alist-get :process (mevedel-acp-client connection))))
    (or (not process) (process-live-p process)
        (progn (mevedel-acp--fail connection "ACP agent process exited") nil))))

(defun mevedel-acp--send (connection request success)
  "Send REQUEST on CONNECTION and deliver SUCCESS, closing on failure."
  (condition-case err
      (when (mevedel-acp--live-p connection)
        (acp-send-request
         :client (mevedel-acp-client connection) :request request
         :on-success success
         :on-failure (lambda (failure) (mevedel-acp--fail connection failure))))
    (error (mevedel-acp--fail connection (error-message-string err)))))

(defun mevedel-acp--request-sender (&rest args)
  "Install timer ownership before sending ACP request ARGS."
  (let* ((client (plist-get args :client))
         (process (alist-get :process client)))
    (unless (process-get process 'mevedel-acp-filter)
      (let ((filter (process-filter process)))
        (set-process-filter
         process (lambda (process input)
                   (mevedel-transport-call-with-retained-timers
                    (lambda () (funcall filter process input))))))
      (process-put process 'mevedel-acp-filter t)))
  (apply #'acp--request-sender args))

(defun mevedel-acp--ready (connection)
  "Admit prepared CONNECTION once any required command is advertised."
  (when (and (eq (mevedel-acp-state connection) 'starting)
             (mevedel-acp-prepared connection))
    (let* ((required (plist-get (mevedel-acp-launch connection) :required-command))
           (advertisement (assoc (mevedel-acp-session-id connection)
                                 (mevedel-acp-commands connection))))
      (cond
       ((and required (not advertisement)))
       ((and required
             (not (seq-some (lambda (command)
                              (equal required (alist-get 'name command)))
                            (cdr advertisement))))
        (mevedel-acp--fail
         connection (format "Agent does not advertise /%s; update the Claude CLI and ACP adapter" required)))
       (t
        (mevedel-acp--cancel-timer connection)
        (setf (mevedel-acp-state connection) 'idle)
        (funcall (mevedel-acp-ready connection) connection))))))

(defun mevedel-acp--new-session (connection response)
  "Create or resume CONNECTION after initialization RESPONSE."
  (when (eq (mevedel-acp-state connection) 'starting)
    (if (not (equal 1 (alist-get 'protocolVersion response)))
        (mevedel-acp--fail connection "Unsupported ACP protocol version")
      (setf (mevedel-acp-capabilities connection)
            (alist-get 'agentCapabilities response))
      (let* ((launch (mevedel-acp-launch connection))
             (id (plist-get launch :session-id))
             (capabilities (mevedel-acp-capabilities connection))
             (resume (assq 'resume (alist-get 'sessionCapabilities capabilities)))
             (load-p (eq t (alist-get 'loadSession capabilities)))
             (method (cond ((not id) "session/new")
                           (resume "session/resume")
                           (load-p "session/load"))))
        (if (not method)
            (mevedel-acp--fail connection "Agent cannot resume this conversation")
          (mevedel-acp--send
           connection
           `((:method . ,method)
             (:params . ((cwd . ,(directory-file-name (plist-get launch :cwd)))
                         (mcpServers . ,(or (plist-get launch :mcp) []))
                         ,@(when id `((sessionId . ,id)))
                         ,@(when-let* ((meta (plist-get launch :meta)))
                             `((_meta . ,meta))))))
           (lambda (session)
             (when (eq (mevedel-acp-state connection) 'starting)
               (let ((session-id (or (alist-get 'sessionId session) id)))
                 (if (not (and (stringp session-id) (not (equal session-id ""))
                               (or (not id) (equal session-id id))))
                     (mevedel-acp--fail connection "Agent returned an invalid session identity")
                   (setf (mevedel-acp-session-id connection) session-id
                         (alist-get 'sessionId session) session-id)
                   (condition-case err
                       (let ((ready
                              (lambda (&optional _metadata)
                                (when (eq (mevedel-acp-state connection) 'starting)
                                  (setf (mevedel-acp-prepared connection) t)
                                  (mevedel-acp--ready connection)))))
                         (if-let* ((prepare (plist-get launch :prepare-session)))
                             (funcall prepare connection session ready)
                           (funcall ready session)))
                     (error (mevedel-acp--fail connection (error-message-string err))))))))))))))

(defun mevedel-acp-open (launch ready failure)
  "Open an external conversation described by LAUNCH.
LAUNCH has :command, :args, absolute local :cwd, optional full :environment,
:mcp server vector, agent-specific :meta, and optional :session-id to resume.
Optional :prepare-launch receives zero-argument READY and error-string FAILURE
callbacks and returns a canceller.  It must complete before the agent process
starts; cancellation and startup timeout retire it and any late callbacks.
Optional :prepare-session receives the connection, session metadata and a
continuation.  It must finish configuration before invoking the continuation;
startup errors or timeout fail before any prompt is submitted.
Optional :required-command waits for a session command advertisement before
READY; missing support fails startup without sending a prompt.
Optional :compaction advertises support for retained session summaries.
Optional :normalize-outcome translates adapter-specific terminal metadata and
usage into an outcome plist; generic ACP cannot infer its accounting scope.
READY receives this connection after admission; FAILURE receives an error
string if startup fails.  Return the runtime connection immediately."
  (let ((cwd (plist-get launch :cwd)))
    (unless (and (stringp cwd) (file-name-absolute-p cwd)
                 (not (file-remote-p cwd)) (file-directory-p cwd))
      (error "ACP requires an existing absolute local working directory"))
    (let* ((connection (mevedel-acp--create :launch launch :ready ready :failure failure))
           (client (acp-make-client :command (plist-get launch :command)
                                    :command-params (plist-get launch :args)
                                    :request-sender #'mevedel-acp--request-sender)))
      (setf (mevedel-acp-client connection) client)
      (acp-subscribe-to-notifications
       :client client
       :on-notification
       (lambda (notification)
         (when-let* ((params (alist-get 'params notification))
                     (id (alist-get 'sessionId params))
                     (update (alist-get 'update params))
                     ((not (eq (mevedel-acp-state connection) 'closed)))
                     ((equal "available_commands_update" (alist-get 'sessionUpdate update))))
           (setf (alist-get id (mevedel-acp-commands connection) nil nil #'equal)
                 (alist-get 'availableCommands update))
           (mevedel-acp--ready connection))
         (when-let* ((turn (mevedel-acp-active connection))
                     (params (alist-get 'params notification))
                     ((equal (alist-get 'sessionId params)
                             (mevedel-acp-session-id connection))))
           (funcall (plist-get turn :event) notification))))
      (acp-subscribe-to-requests
       :client client
       :on-request
       (lambda (request)
         (acp-send-response
          :client client
          :response
          (if (equal (alist-get 'method request) "session/request_permission")
              (acp-make-session-request-permission-response
               :request-id (alist-get 'id request) :cancelled t)
            `((:request-id . ,(alist-get 'id request))
              (:error . ((code . -32601)
                         (message . "Use the admitted MCP tools for execution"))))))))
      (let (timer)
        (setq timer
              (mevedel-transport-run-at-time
               mevedel-acp--control-timeout
               (lambda ()
                 (when (and (eq timer (mevedel-acp-timer connection))
                            (eq (mevedel-acp-state connection) 'starting))
                   (mevedel-acp--fail connection "ACP startup timed out")))))
        (setf (mevedel-acp-timer connection) timer))
      (cl-labels
          ((start ()
             (when (eq (mevedel-acp-state connection) 'starting)
               (let ((default-directory (file-name-as-directory cwd))
                     (process-environment (or (plist-get launch :environment) process-environment))
                     (initialize (acp-make-initialize-request
                                  :protocol-version 1
                                  :client-info '((name . "mevedel") (version . "0.5.0")))))
                 (when (plist-get launch :compaction)
                   (setf (alist-get 'session (alist-get 'clientCapabilities (alist-get :params initialize)))
                         `((compaction . ,(make-hash-table :test #'equal)))))
                 (mevedel-acp--send
                  connection initialize
                  (lambda (response) (mevedel-acp--new-session connection response)))
                 (when-let* ((process (alist-get :process client)))
                   (let ((sentinel (process-sentinel process)))
                     (set-process-sentinel
                      process (lambda (process event)
                                (funcall sentinel process event)
                                (unless (process-live-p process)
                                  (mevedel-acp--fail connection "ACP agent process exited"))))))))))
        (if-let* ((prepare (plist-get launch :prepare-launch)))
            (let (cancel pending-cancel done)
              (setf (mevedel-acp-prepare-cancel connection)
                    (lambda ()
                      (setq done t pending-cancel t)
                      (when cancel (funcall cancel))))
              (condition-case err
                  (progn
                    (setq cancel
                          (funcall
                           prepare
                           (lambda ()
                             (unless done
                               (setq done t)
                               (setf (mevedel-acp-prepare-cancel connection) nil)
                               (condition-case err
                                   (start)
                                 (error
                                  (mevedel-acp--fail connection (error-message-string err))))))
                           (lambda (message)
                             (unless done
                               (setq done t)
                               (setf (mevedel-acp-prepare-cancel connection) nil)
                               (mevedel-acp--fail connection message)))))
                    (when (and pending-cancel cancel) (funcall cancel)))
                (error (mevedel-acp--fail connection (error-message-string err)))))
          (start)))
      connection)))

(defun mevedel-acp-prompt (connection content event complete)
  "Send CONTENT on idle CONNECTION, streaming to EVENT and settling COMPLETE.
CONTENT is a vector of ACP content-block alists.  EVENT receives session-owned
notifications.  COMPLETE receives one plist with :status, :stop-reason and raw
:response, or :message on failure.  No failed prompt is retried automatically."
  (unless (and (eq (mevedel-acp-state connection) 'idle)
               (process-live-p (alist-get :process (mevedel-acp-client connection))))
    (error "ACP conversation is not ready for a prompt"))
  (when (and (cl-some (lambda (part) (equal "image" (alist-get 'type part))) content)
             (not (eq t (alist-get 'image (alist-get 'promptCapabilities
                                                     (mevedel-acp-capabilities connection))))))
    (user-error "ACP agent does not support image input"))
  (let ((turn (list :event event :complete complete)))
    (setf (mevedel-acp-active connection) turn
          (mevedel-acp-state connection) 'prompting)
    (mevedel-acp--send
     connection (acp-make-session-prompt-request
                 :session-id (mevedel-acp-session-id connection) :prompt content)
     (lambda (response)
       (let ((reason (alist-get 'stopReason response)))
         (mevedel-acp--finish
          connection turn
          (list :status (cond
                         ((or (plist-get turn :cancelled)
                              (equal reason "cancelled")) 'interrupted)
                         ((equal reason "end_turn") 'success)
                         (t 'error))
                :stop-reason reason :response response)))))))

(defun mevedel-acp-cancel (connection)
  "Request interruption of CONNECTION's active prompt.
Wait for the terminal acknowledgement before accepting more input."
  (when-let* ((turn (mevedel-acp-active connection))
              ((not (plist-get turn :cancelled))))
    (setf (mevedel-acp-active connection) (plist-put turn :cancelled t))
    (let (timer)
      (setq timer
            (mevedel-transport-run-at-time
             mevedel-acp--control-timeout
             (lambda ()
               (when (and (eq timer (mevedel-acp-timer connection))
                          (eq turn (mevedel-acp-active connection)))
                 (mevedel-acp--fail connection "ACP cancellation timed out")))))
      (setf (mevedel-acp-timer connection) timer))
    (condition-case err
        (when (mevedel-acp--live-p connection)
          (acp-send-notification
           :client (mevedel-acp-client connection)
           :notification `((:method . "session/cancel")
                           (:params . ((sessionId . ,(mevedel-acp-session-id connection)))))))
      (error (mevedel-acp--fail connection (error-message-string err))))))

(defun mevedel-acp-close (connection)
  "Release CONNECTION and settle pending work as interrupted, once."
  (mevedel-acp--shutdown connection '(:status interrupted :message "ACP connection closed")))

(provide 'mevedel-acp)
;;; mevedel-acp.el ends here
