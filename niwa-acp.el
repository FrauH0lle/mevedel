;;; niwa-acp.el --- External agent conversation lifecycle -*- lexical-binding: t -*-

;;; Commentary:
;; One retained ACP conversation, using acp.el for protocol and process I/O.
;; Owns startup, conversation identity, one active prompt, cancellation and
;; terminal delivery.  Tool effects belong to niwa's MCP pipeline.  Agent
;; filesystem/terminal requests are never an alternate execution path.

;;; Code:

(require 'acp)
(require 'cl-lib)
(require 'seq)
(require 'niwa-structs)
(require 'niwa-transport)
(require 'niwa-utilities)

;; `niwa-gptel-stream-bridge'
(defvar niwa-gptel-stream-bridge-insert-batch-delay)

(cl-defstruct (niwa-acp (:constructor niwa-acp--create))
  "Runtime state for one external model conversation."
  client launch session-id capabilities commands prepared checked diagnostic-scope
  agent-name auth-methods
  (state 'starting) active timer prepare-cancel ready failure stderr)

(defconst niwa-acp--auth-required -32000
  "ACP v1 JSON-RPC code for auth_required.")

(defvar niwa-acp--auth-expiry-warned (make-hash-table :test #'equal)
  "Pairs of expiry reader and timestamp already warned about this Emacs session.")

(defconst niwa-acp--control-timeout 30
  "Seconds allowed for startup and cancellation acknowledgement.")

(defconst niwa-acp--stderr-limit 2048
  "Characters of agent stderr retained to explain a process exit.")

(defconst niwa-acp--stderr-log-limit 65536
  "Characters of recent agent exits retained in the stderr log.")

(defun niwa-acp--trim-stderr (&rest _change)
  "Retain only the recent stderr buffer tail after ACP's error handlers run."
  (when (> (buffer-size) niwa-acp--stderr-log-limit)
    (with-silent-modifications
      (delete-region (point-min) (- (point-max) niwa-acp--stderr-log-limit)))))

(defun niwa-acp--cancel-timer (connection)
  "Cancel CONNECTION's pending control timeout."
  (when-let* ((timer (niwa-acp-timer connection)))
    (cancel-timer timer)
    (setf (niwa-acp-timer connection) nil)))

(defconst niwa-acp--pace-backlog-bytes 4096
  "Reads at least this large mean the agent has more output waiting.")

(defconst niwa-acp--pace-after-text-reads 3
  "Consecutive text-only reads that mark a steady stream worth pacing.
A tool phase alternates short status text with tool frames; pausing there
only delayed each step, by up to a pause per tool call.")

(defconst niwa-acp--pace-margin 0.05
  "Seconds a paced agent stays stopped beyond its text batch's delay.")

(defun niwa-acp--process (connection)
  "Return CONNECTION's agent process, or nil."
  (alist-get :process (niwa-acp-client connection)))

(defun niwa-acp--wake (connection)
  "Continue CONNECTION's agent if it is paused for output pacing.
Every message to the agent and the end of a turn continue it first: a
stopped agent would answer late, and a full pipe would block the write."
  (when-let* ((process (niwa-acp--process connection)))
    (niwa-transport-resume-process process)))

(defun niwa-acp--streaming-only-p (output)
  "Return non-nil when OUTPUT holds only complete streamed-text frames.
Claude's adapter sends each delta twice: as an ACP text or thought chunk
and as the SDK's raw `content_block_delta' stream event.  The SDK's
message receipts, which tool calls must not overtake, are other types."
  (and (string-suffix-p "\n" output)
       (seq-every-p
        (lambda (line)
          (or (string-empty-p line)
              (string-match-p
               "\"sessionUpdate\": *\"agent_\\(?:message\\|thought\\)_chunk\""
               line)
              (and (string-match-p "\"_claude/sdkMessage\"" line)
                   (string-match-p "\"type\": *\"stream_event\"" line)
                   (string-match-p
                    "\"event\": *{\"type\": *\"content_block_delta\"" line))))
        (split-string output "\n"))))

(defun niwa-acp--pace (connection process output)
  "Pause CONNECTION's agent PROCESS after a small text-only OUTPUT read.
Each streamed chunk arrives as its own read, and Emacs redisplays after
every read: a turn streaming 32 chunks a second cost 67% editor CPU.
Stopped until the text batch is due, the agent leaves the CLI's output in
its pipe and Emacs reads it in one burst.  Any other frame -- a tool call,
a message receipt, a response or a request -- leaves the agent running:
MCP tool calls reach Emacs on another channel and must not overtake them.
Only a steady text stream pauses, see `niwa-acp--pace-after-text-reads'.
acp.el's sentinel ignores stop and continue events."
  (process-put process 'niwa-acp--text-reads
               (if (niwa-acp--streaming-only-p output)
                   (1+ (or (process-get process 'niwa-acp--text-reads) 0))
                 0))
  (when (and (eq (niwa-acp-state connection) 'prompting)
             (< (string-bytes output) niwa-acp--pace-backlog-bytes)
             (>= (process-get process 'niwa-acp--text-reads)
                 niwa-acp--pace-after-text-reads)
             ;; Pacing follows text batches; without batching it only delays.
             (numberp (bound-and-true-p
                       niwa-gptel-stream-bridge-insert-batch-delay))
             (> niwa-gptel-stream-bridge-insert-batch-delay 0))
    ;; acp.el inserts this text from a drain one timer pass after the read,
    ;; so the batch it starts falls due just after a pause of the same
    ;; length, and the two would each cost a redisplay.  The batch's flush
    ;; continues the agent; this pause only bounds a read with no text.
    (niwa-transport-pause-process
     process (+ niwa-gptel-stream-bridge-insert-batch-delay
                niwa-acp--pace-margin))))

(defun niwa-acp--pace-output (connection)
  "Pace CONNECTION's agent output during turns, once per process."
  (when-let* ((process (niwa-acp--process connection))
              ((not (process-get process 'niwa-acp--paced))))
    (process-put process 'niwa-acp--paced t)
    (add-function :after (process-filter process)
                  (lambda (process output)
                    (niwa-acp--pace connection process output)))))

(defun niwa-acp--finish (connection turn outcome)
  "Deliver OUTCOME once for CONNECTION's captured TURN."
  (niwa-acp--wake connection)
  (when (eq turn (niwa-acp-active connection))
    (niwa-acp--cancel-timer connection)
    (when-let* ((retry (plist-get turn :retry-timer)))
      (cancel-timer retry))
    (setf (niwa-acp-active connection) nil)
    (unless (eq (niwa-acp-state connection) 'closed)
      (setf (niwa-acp-state connection) 'idle))
    (when-let* ((normalize (plist-get (niwa-acp-launch connection)
                                      :normalize-outcome)))
      (setq outcome
            (condition-case err
                (funcall normalize outcome)
              (error (list :status 'error
                           :message (format "Invalid agent outcome: %s"
                                            (error-message-string err)))))))
    (funcall (plist-get turn :complete) outcome)))

(defun niwa-acp--log-stderr (connection message stderr)
  "Append CONNECTION's exit MESSAGE and STDERR to the agent stderr log.
The log outlives the warning buffer, which popup managers kill when it is
dismissed.  It keeps the most recent `niwa-acp--stderr-log-limit'
characters and is private to the user: stderr holds host logs.  Return
the log file, or nil when it cannot be written."
  (let ((file (file-name-concat niwa-user-dir "agent-stderr.log")))
    (condition-case nil
        (progn
          (make-directory (file-name-directory file) t)
          (with-temp-buffer
            (when (file-readable-p file)
              (insert-file-contents file))
            (goto-char (point-max))
            (insert (format "%s %s\n%s\nAgent stderr:\n%s\n\n"
                            (format-time-string "%F %T")
                            (plist-get (niwa-acp-launch connection) :command)
                            message stderr))
            (when (> (buffer-size) niwa-acp--stderr-log-limit)
              (goto-char (- (point-max) niwa-acp--stderr-log-limit))
              (delete-region (point-min) (line-beginning-position 2)))
            (let ((coding-system-for-write 'utf-8-unix)
                  (create-lockfiles nil))
              (with-file-modes #o600
                (write-region nil nil file nil 'silent))))
          file)
      (file-error nil))))

(defun niwa-acp--shutdown (connection outcome)
  "Close CONNECTION and deliver terminal OUTCOME to its owner."
  (niwa-acp--wake connection)
  (unless (eq (niwa-acp-state connection) 'closed)
    (let ((starting (eq (niwa-acp-state connection) 'starting)))
      (setf (niwa-acp-state connection) 'closed)
      (niwa-acp--cancel-timer connection)
      (when-let* ((cancel (niwa-acp-prepare-cancel connection)))
        (setf (niwa-acp-prepare-cancel connection) nil)
        (funcall cancel))
      (acp-shutdown :client (niwa-acp-client connection))
      (cond
       (starting (funcall (niwa-acp-failure connection)
                          (or (plist-get outcome :message) "ACP connection closed")
                          (plist-get outcome :code)))
       ((niwa-acp-active connection)
        (niwa-acp--finish connection (niwa-acp-active connection) outcome))))))

(defun niwa-acp--agent-label (connection)
  "Return CONNECTION's display name or executable name."
  (let ((launch (niwa-acp-launch connection)))
    (or (plist-get launch :agent-name) (niwa-acp-agent-name connection)
        (when-let* ((command (plist-get launch :command)))
          (file-name-nondirectory command))
        "ACP agent")))

(defun niwa-acp--auth-message (connection expired)
  "Describe CONNECTION's authentication failure, noting EXPIRED when known."
  (let* ((launch (niwa-acp-launch connection))
         (methods (niwa-acp-auth-methods connection))
         (hint (plist-get launch :auth-hint)))
    (concat (format "Authentication required for %s%s."
                    (niwa-acp--agent-label connection) (if expired ": login expired" ""))
            (when methods (format " Advertised sign-in methods: %s." (string-join methods ", ")))
            (when hint (concat " " hint)))))

(defun niwa-acp--check-auth-expiry (connection)
  "Warn about CONNECTION's upcoming login expiry, or fail and return nil."
  (let* ((launch (niwa-acp-launch connection))
         (reader (plist-get launch :auth-expiry))
         (process-environment (or (plist-get launch :environment) process-environment))
         (expiry (and reader (funcall reader)))
         (remaining (and expiry (- expiry (float-time))))
         (days (plist-get launch :auth-warn-days)))
    (cond
     ((and remaining (<= remaining 0))
      (niwa-acp--fail connection `((code . ,niwa-acp--auth-required) (expired . t)))
      nil)
     (t
      (when (and remaining days (<= remaining (* days 86400)))
        (let ((key (cons reader expiry)))
          (unless (gethash key niwa-acp--auth-expiry-warned)
            (puthash key t niwa-acp--auth-expiry-warned)
            (niwa-diagnostic-warning
             'global 'niwa
             (format "%s login expires on %s. %s"
                     (niwa-acp--agent-label connection)
                     (format-time-string "%Y-%m-%d %H:%M %Z" expiry)
                     (or (plist-get launch :auth-hint) "Sign in again before then."))))))
      t))))

(defun niwa-acp--fail (connection error)
  "Close CONNECTION after protocol or process ERROR.
A process exit warns the host with the agent's last stderr output, its only
explanation, and appends it to the agent stderr log, which survives the
warning buffer.  The outcome message names the log but omits the text: that
message is classified, shown to collaboration guests and returned to models,
and stderr holds host logs.
A closed CONNECTION is ignored: `delete-process' runs the sentinel during an
orderly close, and that exit is no failure."
  (let* ((code (and (listp error) (alist-get 'code error)))
         (message (cond ((eql code niwa-acp--auth-required)
                         (niwa-acp--auth-message connection (alist-get 'expired error)))
                        ((stringp error) error)
                        (t (or (alist-get 'message error) (format "%S" error)))))
         (process (alist-get :process (niwa-acp-client connection)))
         (stderr (string-trim (or (niwa-acp-stderr connection) ""))))
    (when (and (not (eq (niwa-acp-state connection) 'closed))
               process (not (process-live-p process)) (not (string-empty-p stderr)))
      (let* ((file (niwa-acp--log-stderr connection message stderr))
             (where (if file (abbreviate-file-name file) "*Warnings*")))
        (niwa-diagnostic-warning
         (or (niwa-acp-diagnostic-scope connection) 'unavailable)
         'niwa (format "%s\nAgent stderr (also in %s):\n%s"
                                          message where stderr))
        (setq message (format "%s; agent stderr is in %s" message where))))
    (niwa-acp--shutdown
     connection (list :status 'error
                      :code code
                      :message message))))

(defun niwa-acp--live-p (connection)
  "Return non-nil unless CONNECTION's started agent process died.
acp.el restarts a dead client on send, outside this connection's launch,
so a dead process fails the connection instead."
  (let ((process (alist-get :process (niwa-acp-client connection))))
    (or (not process) (process-live-p process)
        (progn (niwa-acp--fail connection "ACP agent process exited") nil))))

(defun niwa-acp--send (connection request success &optional failure)
  "Send REQUEST on CONNECTION and deliver SUCCESS.
An error response goes to FAILURE, which closes CONNECTION by default."
  (niwa-acp--wake connection)
  (condition-case err
      (when (niwa-acp--live-p connection)
        (acp-send-request
         :client (niwa-acp-client connection) :request request
         :on-success success
         :on-failure (or failure (lambda (failure) (niwa-acp--fail connection failure)))))
    (error (niwa-acp--fail connection (error-message-string err)))))

(defun niwa-acp--admit (connection)
  "Admit CONNECTION and hand it to its READY callback.
A READY that signals still settles its owner once: through FAILURE before a
prompt starts, otherwise through the prompt's completion."
  (niwa-acp--cancel-timer connection)
  (setf (niwa-acp-state connection) 'idle)
  (condition-case err
      (funcall (niwa-acp-ready connection) connection)
    (error
     (when (eq (niwa-acp-state connection) 'idle)
       (setf (niwa-acp-state connection) 'starting))
     (niwa-acp--fail connection (error-message-string err)))))

(defun niwa-acp--ready (connection)
  "Admit prepared and checked CONNECTION once any required command is advertised."
  (when (and (eq (niwa-acp-state connection) 'starting)
             (niwa-acp-prepared connection)
             (niwa-acp-checked connection))
    (let* ((required (plist-get (niwa-acp-launch connection) :required-command))
           (advertisement (assoc (niwa-acp-session-id connection)
                                 (niwa-acp-commands connection))))
      (cond
       ((and required (not advertisement)))
       ((and required
             (not (seq-some (lambda (command)
                              (equal required (alist-get 'name command)))
                            (cdr advertisement))))
        (niwa-acp--fail
         connection (format "Agent does not advertise /%s; update the Claude CLI and ACP adapter" required)))
       (t (niwa-acp--admit connection))))))

(cl-defun niwa-acp--new-session (connection response)
  "Create or resume CONNECTION after initialization RESPONSE."
  (when (eq (niwa-acp-state connection) 'starting)
    (if (not (equal 1 (alist-get 'protocolVersion response)))
        (niwa-acp--fail connection "Unsupported ACP protocol version")
      (setf (niwa-acp-capabilities connection)
            (alist-get 'agentCapabilities response))
      (let ((info (alist-get 'agentInfo response))
            (methods (alist-get 'authMethods response)))
        (setf (niwa-acp-agent-name connection)
              (when (listp info)
                (cl-find-if #'stringp (list (alist-get 'title info) (alist-get 'name info))))
              (niwa-acp-auth-methods connection)
              (when (or (vectorp methods) (listp methods))
                (cl-loop for method across (vconcat methods)
                         for name = (when (listp method)
                                      (cl-find-if #'stringp (list (alist-get 'name method)
                                                                 (alist-get 'id method))))
                         when name collect name))))
      (when (plist-get (niwa-acp-launch connection) :initialize-only)
        (setf (niwa-acp-prepared connection) t)
        (niwa-acp--ready connection)
        (cl-return-from niwa-acp--new-session nil))
      (let* ((launch (niwa-acp-launch connection))
             (id (plist-get launch :session-id))
             (capabilities (niwa-acp-capabilities connection))
             (resume (assq 'resume (alist-get 'sessionCapabilities capabilities)))
             (load-p (eq t (alist-get 'loadSession capabilities)))
             (make (cond ((not id) #'acp-make-session-new-request)
                         (resume #'acp-make-session-resume-request)
                         (load-p #'acp-make-session-load-request))))
        (if (not make)
            (niwa-acp--fail connection "Agent cannot resume this conversation")
          (niwa-acp--send
           connection
           (apply make :cwd (plist-get launch :cwd) :mcp-servers (plist-get launch :mcp)
                  :meta (plist-get launch :meta) (and id (list :session-id id)))
           (lambda (session)
             (when (eq (niwa-acp-state connection) 'starting)
               (let ((session-id (or (alist-get 'sessionId session) id)))
                 (if (not (and (stringp session-id) (not (equal session-id ""))
                               (or (not id) (equal session-id id))))
                     (niwa-acp--fail connection "Agent returned an invalid session identity")
                   (setf (niwa-acp-session-id connection) session-id
                         (alist-get 'sessionId session) session-id)
                   (condition-case err
                       (let ((ready
                              (lambda (&optional _metadata)
                                (when (eq (niwa-acp-state connection) 'starting)
                                  (setf (niwa-acp-prepared connection) t)
                                  (niwa-acp--ready connection)))))
                         (if-let* ((prepare (plist-get launch :prepare-session)))
                             (funcall prepare connection session ready)
                           (funcall ready session)))
                     (error (niwa-acp--fail connection (error-message-string err))))))))))))))

(defun niwa-acp-open (launch ready failure)
  "Open an external conversation described by LAUNCH.
LAUNCH has :command, :args, absolute local :cwd, optional full :environment,
:mcp server vector, agent-specific :meta, and optional :session-id to resume.
Optional :prepare-launch receives zero-argument READY and FAILURE callbacks and
returns a canceller; FAILURE takes an error string and an optional CODE, as
below.  It runs while the agent process starts,
and READY waits for it, so no prompt is sent before it succeeds; its failure,
cancellation or the startup timeout closes the connection and retires late
callbacks.
Optional :prepare-session receives the connection, session metadata and a
continuation.  It must finish configuration before invoking the continuation;
startup errors or timeout fail before any prompt is submitted.
Optional :required-command waits for a session command advertisement before
READY; missing support fails startup without sending a prompt.
Optional :initialize-only checks the protocol without creating a conversation.
Optional :diagnostic-scope overrides the root captured from the invoking buffer
for local warnings; machine-wide validation supplies `global'.
Optional :compaction advertises support for retained session summaries.
Optional :agent-name and :auth-hint give the agent's display name and sign-in
instructions.  An optional :auth-expiry function returns the login's hard
expiry (Unix seconds) or nil.  :auth-warn-days sets its warning lead time.
Optional :normalize-outcome translates adapter-specific terminal metadata and
usage into an outcome plist; generic ACP cannot infer its accounting scope.
Optional :prompt-retry-delay receives a prompt's JSON-RPC error and returns
seconds to wait before re-sending that prompt once, or nil to fail.  It is
consulted only while the prompt has reported no tool call, so a replay
repeats no effect.  A policy error fails the turn instead of leaving it active.
READY receives this connection after admission; FAILURE receives an error
string and a CODE if startup fails.  CODE is the agent's JSON-RPC error code,
the symbol `unavailable' when a launch check found the provider unusable
\(missing sign-in or runtime), or nil.  Return the runtime connection
immediately."
  (let ((cwd (plist-get launch :cwd)))
    (unless (and (stringp cwd) (file-name-absolute-p cwd)
                 (not (file-remote-p cwd)) (file-directory-p cwd))
      (error "ACP requires an existing absolute local working directory"))
    (let* ((connection (niwa-acp--create
                        :launch launch :ready ready :failure failure
                        :diagnostic-scope
                        (or (plist-get launch :diagnostic-scope)
                            (niwa-diagnostic-scope (current-buffer)))))
           (client (acp-make-client :command (plist-get launch :command)
                                    :command-params (plist-get launch :args))))
      (setf (niwa-acp-client connection) client)
      (acp-subscribe-to-errors
       :client client
       :on-error
       (lambda (err)
         (when-let* ((text (alist-get 'message err))
                     ((stringp text)))
           (let ((tail (concat (niwa-acp-stderr connection) text)))
             (setf (niwa-acp-stderr connection)
                   (substring tail (max 0 (- (length tail) niwa-acp--stderr-limit))))))))
      (acp-subscribe-to-notifications
       :client client
       :on-notification
       (lambda (notification)
         (when-let* ((params (alist-get 'params notification))
                     (id (alist-get 'sessionId params))
                     (update (alist-get 'update params))
                     ((not (eq (niwa-acp-state connection) 'closed)))
                     ((equal "available_commands_update" (alist-get 'sessionUpdate update))))
           (setf (alist-get id (niwa-acp-commands connection) nil nil #'equal)
                 (alist-get 'availableCommands update))
           (niwa-acp--ready connection))
         (when-let* ((turn (niwa-acp-active connection))
                     (params (alist-get 'params notification))
                     ((equal (alist-get 'sessionId params)
                             (niwa-acp-session-id connection))))
           (funcall (plist-get turn :event) notification))))
      (acp-subscribe-to-requests
       :client client
       :on-request
       (lambda (request)
         (niwa-acp--wake connection)
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
              (niwa-transport-run-at-time
               niwa-acp--control-timeout
               (lambda ()
                 (when (and (eq timer (niwa-acp-timer connection))
                            (eq (niwa-acp-state connection) 'starting))
                   (niwa-acp--fail connection "ACP startup timed out")))))
        (setf (niwa-acp-timer connection) timer))
      (cl-labels
          ((start ()
             (when (eq (niwa-acp-state connection) 'starting)
               (let ((default-directory (file-name-as-directory cwd))
                     (process-environment (or (plist-get launch :environment) process-environment))
                     (initialize (acp-make-initialize-request
                                  :protocol-version 1
                                  :client-info '((name . "niwa") (version . "0.5.0")))))
                 (when (plist-get launch :compaction)
                   (setf (alist-get 'session (alist-get 'clientCapabilities (alist-get :params initialize)))
                         `((compaction . ,(make-hash-table :test #'equal)))))
                 (niwa-acp--send
                  connection initialize
                  (lambda (response) (niwa-acp--new-session connection response)))
                 ;; Nothing is read before the first request returns, and the
                 ;; process never respawns, so its handlers are wrapped once.
                 (when-let* ((process (alist-get :process client)))
                   (process-put process 'niwa-acp-launch launch)
                   (when-let* ((stderr (get-process (concat (process-name process) " stderr")))
                               (buffer (process-buffer stderr)))
                     (with-current-buffer buffer
                       ;; acp.el consumes the whole inserted chunk before trimming.
                       (add-hook 'after-change-functions #'niwa-acp--trim-stderr t t)))
                   (let ((filter (process-filter process))
                         (sentinel (process-sentinel process)))
                     (set-process-filter
                      process (lambda (process input)
                                (niwa-transport-call-with-retained-timers
                                 (lambda () (funcall filter process input)))))
                     (set-process-sentinel
                      process (lambda (process event)
                                (funcall sentinel process event)
                                (unless (process-live-p process)
                                  (when-let* ((observer (plist-get launch :exit-observer)))
                                    (ignore-errors (funcall observer process)))
                                  (niwa-acp--fail connection "ACP agent process exited"))))))))))
        ;; Start at once: preparation (version and login checks) overlaps the
        ;; agent's own startup, and admission waits for both.
        (condition-case err
            (when (niwa-acp--check-auth-expiry connection) (start))
          (error (niwa-acp--fail connection (error-message-string err))))
        (if-let* (((eq (niwa-acp-state connection) 'starting))
                  (prepare (plist-get launch :prepare-launch)))
            (let (cancel pending-cancel done)
              (setf (niwa-acp-prepare-cancel connection)
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
                               (setf (niwa-acp-prepare-cancel connection) nil
                                     (niwa-acp-checked connection) t)
                               (niwa-acp--ready connection)))
                           (lambda (message &optional code)
                             (unless done
                               (setq done t)
                               (setf (niwa-acp-prepare-cancel connection) nil)
                               (niwa-acp--fail
                                connection (if code `((message . ,message) (code . ,code))
                                             message))))))
                    (when (and pending-cancel cancel) (funcall cancel)))
                (error (niwa-acp--fail connection (error-message-string err)))))
          (setf (niwa-acp-checked connection) t)))
      connection)))

(defun niwa-acp-session-operation (launch operation session-id complete)
  "Run native session OPERATION from LAUNCH on SESSION-ID, calling COMPLETE.
OPERATION is `fork' or `delete'.  COMPLETE receives an outcome plist with
:status and either :session-id or :message.  Fork requires advertised fork,
resume and delete capabilities and rejects an empty or unchanged identity.
This short-lived connection never resumes or prompts either conversation."
  (unless (and (memq operation '(fork delete))
               (stringp session-id) (not (string-empty-p session-id)))
    (error "Invalid native session operation"))
  (let ((launch (copy-sequence launch)) done)
    (setq launch (plist-put launch :initialize-only t)
          launch (plist-put launch :normalize-outcome nil))
    (cl-labels
        ((deliver (outcome)
           (unless done
             (setq done t)
             (funcall complete outcome))))
      (niwa-acp-open
       launch
       (lambda (connection)
         (let ((capabilities (alist-get 'sessionCapabilities
                                       (niwa-acp-capabilities connection))))
           (dolist (capability (if (eq operation 'fork) '(fork resume delete) '(delete)))
             (unless (assq capability capabilities)
               (user-error "Agent lacks session %s capability; /btw needs fork, resume and delete" capability))))
         (let ((turn
                (list :event #'ignore
                      :complete
                      (lambda (outcome)
                        (niwa-acp-close connection)
                        (deliver outcome)))))
           (setf (niwa-acp-active connection) turn
                 (niwa-acp-state connection) 'controlling
                 (niwa-acp-timer connection)
                 (niwa-transport-run-at-time
                  niwa-acp--control-timeout
                  (lambda ()
                    (when (eq turn (niwa-acp-active connection))
                      (niwa-acp--fail connection "Native session operation timed out")))))
           (niwa-acp--send
            connection
            (if (eq operation 'fork)
                (acp-make-session-fork-request
                 :session-id session-id :cwd (plist-get launch :cwd)
                 :mcp-servers [] :meta (plist-get launch :meta))
              (acp-make-session-delete-request :session-id session-id))
            (lambda (response)
              (let ((id (alist-get 'sessionId response)))
                (niwa-acp--finish
                 connection turn
                 (if (and (eq operation 'fork)
                          (not (and (stringp id) (not (string-empty-p id))
                                    (not (equal id session-id)))))
                     '(:status error :message "Agent returned an invalid fork identity")
                   (list :status 'success :session-id (if (eq operation 'fork) id session-id)))))))))
       (lambda (message &optional _code)
         (deliver (list :status 'error :message message)))))))

(defun niwa-acp-prompt (connection content event complete)
  "Send CONTENT on idle CONNECTION, streaming to EVENT and settling COMPLETE.
CONTENT is a vector of ACP content-block alists.  EVENT receives session-owned
notifications.  COMPLETE receives one plist with :status, :stop-reason and raw
:response, or :message on failure.  A failed prompt is re-sent once only when
it reported no tool call and the launch's :prompt-retry-delay accepts the
failure (see `niwa-acp-open')."
  (unless (and (eq (niwa-acp-state connection) 'idle)
               (process-live-p (alist-get :process (niwa-acp-client connection))))
    (error "ACP conversation is not ready for a prompt"))
  (when (and (cl-some (lambda (part) (equal "image" (alist-get 'type part))) content)
             (not (eq t (alist-get 'image (alist-get 'promptCapabilities
                                                     (niwa-acp-capabilities connection))))))
    (user-error "ACP agent does not support image input"))
  (let ((request (acp-make-session-prompt-request
                  :session-id (niwa-acp-session-id connection) :prompt content))
        turn)
    (cl-labels
        ((observe (notification)
           (when (equal "tool_call" (alist-get 'sessionUpdate
                                               (alist-get 'update (alist-get 'params notification))))
             (plist-put turn :acted t))
           (funcall event notification))
         (send ()
           (when (niwa-acp--check-auth-expiry connection)
             (when-let* ((observer (plist-get (niwa-acp-launch connection) :send-observer)))
               (ignore-errors (funcall observer (plist-get turn :retried))))
             (niwa-acp--send connection request #'settle #'failed)))
         (settle (response)
           (let ((reason (alist-get 'stopReason response)))
             (niwa-acp--finish
              connection turn
              (list :status (cond
                             ((or (plist-get turn :cancelled)
                                  (equal reason "cancelled")) 'interrupted)
                             ((equal reason "end_turn") 'success)
                             (t 'error))
                    :stop-reason reason :response response))))
         (failed (failure)
           ;; acp.el contains callback errors, but cannot settle our turn.
           (condition-case err
               (if-let* (((not (or (plist-get turn :acted) (plist-get turn :retried)
                                   (plist-get turn :cancelled))))
                         (retry (plist-get (niwa-acp-launch connection) :prompt-retry-delay))
                         (delay (funcall retry failure)))
                   (plist-put turn :retry-timer
                              (niwa-transport-run-at-time
                               delay (lambda ()
                                       (when (eq turn (niwa-acp-active connection))
                                         (plist-put turn :retry-timer nil)
                                         (plist-put turn :retried t)
                                         (send)))))
                 (niwa-acp--fail connection failure))
             (error (niwa-acp--fail connection (error-message-string err))))))
      (setq turn (list :event #'observe :complete complete))
      (niwa-acp--pace-output connection)
      (setf (niwa-acp-active connection) turn
            (niwa-acp-state connection) 'prompting)
      (send))))

(defun niwa-acp-cancel (connection)
  "Request interruption of CONNECTION's active prompt.
Wait for the terminal acknowledgement before accepting more input."
  (when-let* ((turn (niwa-acp-active connection))
              ((not (plist-get turn :cancelled))))
    (if (plist-get turn :retry-timer)
        ;; No prompt is in flight to acknowledge the cancellation.  COMPLETE
        ;; may start another turn: this cancellation belongs only to TURN.
        (niwa-acp--finish connection turn '(:status interrupted))
      (setf (niwa-acp-active connection) (plist-put turn :cancelled t))
      (let (timer)
        (setq timer
              (niwa-transport-run-at-time
               niwa-acp--control-timeout
               (lambda ()
                 (when (and (eq timer (niwa-acp-timer connection))
                            (eq turn (niwa-acp-active connection)))
                   (niwa-acp--fail connection "ACP cancellation timed out")))))
        (setf (niwa-acp-timer connection) timer))
      (niwa-acp--wake connection)
      (condition-case err
          (when (niwa-acp--live-p connection)
            (acp-send-notification
             :client (niwa-acp-client connection)
             :notification (acp-make-session-cancel-notification
                            :session-id (niwa-acp-session-id connection))))
        (error (niwa-acp--fail connection (error-message-string err)))))))

(defun niwa-acp-close (connection)
  "Release CONNECTION and settle pending work as interrupted, once."
  (niwa-acp--shutdown connection '(:status interrupted :message "ACP connection closed")))

(provide 'niwa-acp)
;;; niwa-acp.el ends here
