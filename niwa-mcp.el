;;; niwa-mcp.el --- Asynchronous local MCP tool server -*- lexical-binding: t -*-

;;; Commentary:

;; Exposes one caller-owned tool scope to an external MCP client.  The caller
;; supplies discovery and dispatch; this module owns the private socket, JSON
;; protocol, pending calls, cancellation and transport cleanup.  Dispatch may
;; wait for an ordinary Emacs interaction without blocking another request.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)

;; `niwa-utilities'
(autoload 'niwa-library-source-directory "niwa-utilities")

;; `niwa-transport'
(autoload 'niwa-transport-run-at-time "niwa-transport")

(defconst niwa-mcp--source-directory
  (niwa-library-source-directory (or load-file-name buffer-file-name))
  "Directory containing the packaged stdio bridge.")

(defconst niwa-mcp--max-message-bytes (* 32 1024 1024)
  "Maximum size of one incoming MCP message.")

(defvar niwa-mcp--progress-interval 30
  "Seconds between progress notifications for a pending tool call.
Clients such as Claude Code abort a call that sends no response or progress
for their idle limit; this keeps well below the shortest practical one.")

(defcustom niwa-mcp-client-idle-timeout 1800
  "Seconds an MCP client waits for a silent tool call before aborting it.
A client that requests no progress notifications cannot be kept waiting, so a
long WaitAgent call through it returns before this limit and asks the agent
to wait again.  The default matches Claude Code's
CLAUDE_CODE_MCP_TOOL_IDLE_TIMEOUT default."
  :type 'natnum
  :group 'niwa)

(cl-defstruct (niwa-mcp (:constructor niwa-mcp--create))
  "Private local server for one caller-owned tool scope."
  directory listener clients tools dispatch control rejected lost closed)

(defun niwa-mcp--encode (message)
  "Return MESSAGE as one newline-terminated JSON line."
  (concat (json-serialize message :null-object :null :false-object :json-false)
          "\n"))

(defun niwa-mcp--write (client line &optional reply)
  "Write encoded LINE to live CLIENT; return non-nil when it was written.
A peer can close after CLIENT was last read, which leaves CLIENT live until
Emacs reads its end.  The write then fails and Emacs closes CLIENT; that
lost client is retired like a disconnect.  When the line answered REPLY, a
(KIND . ID) pair naming a tool call or hook request, the server's lost
callback learns which reply was lost.  A failure that leaves CLIENT live
is unexpected and still signals."
  (when (process-live-p client)
    (condition-case err
        (progn (process-send-string client line) t)
      (error
       (when (process-live-p client) (signal (car err) (cdr err)))
       (let ((server (process-get client 'niwa-mcp-server)))
         (niwa-mcp--disconnect client)
         (when-let* ((lost (and reply server (niwa-mcp-lost server))))
           (funcall lost (list :kind (car reply) :id (cdr reply)))))
       nil))))

(defun niwa-mcp--send (client message &optional reply)
  "Send JSON MESSAGE to live CLIENT, answering REPLY; return non-nil if written."
  (niwa-mcp--write client (niwa-mcp--encode message) reply))

(defun niwa-mcp--error (client id code message &optional reply)
  "Reply to CLIENT request ID with error CODE and MESSAGE, answering REPLY."
  (niwa-mcp--send
   client (list :jsonrpc "2.0" :id id
                :error (list :code code :message message))
   reply))

;; Calls and hook controls share one pending table, so cancellation and
;; disconnect retire whichever is still unanswered.
(defun niwa-mcp--cancel (client id)
  "Retire CLIENT request ID and cancel its pending work."
  (let* ((calls (process-get client 'niwa-mcp-calls))
         (cell (and calls (gethash id calls))))
    (when cell
      ;; Retire before cancellation: cancellers can synchronously call back.
      (remhash id calls)
      (setcdr cell 'cancelled)
      (when (functionp (car cell))
        (funcall (car cell))))))

(defun niwa-mcp--disconnect (client)
  "Cancel pending work and release CLIENT."
  (when-let* ((server (process-get client 'niwa-mcp-server)))
    (setf (niwa-mcp-clients server)
          (delq client (niwa-mcp-clients server))))
  (when-let* ((calls (process-get client 'niwa-mcp-calls)))
    (dolist (id (hash-table-keys calls))
      (condition-case nil
          (niwa-mcp--cancel client id)
        ;; A canceller must not strand this client, its siblings or the socket.
        ((error quit) nil))))
  (when (process-live-p client) (delete-process client)))

(defun niwa-mcp--pending (client id kind start)
  "Answer CLIENT request ID when asynchronous START completes.
KIND is `tool' or `hook', naming the reply should it be lost.
START receives a completion taking a result plist, or nil and an error
message; it returns non-nil when its answer reached the client.  START may
return a canceller for client cancellation or disconnect.  Only the first
completion of a still-pending request answers it."
  (let ((calls (process-get client 'niwa-mcp-calls)))
    (if (gethash id calls)
        ;; Duplicate live IDs cannot identify which invocation is being answered.
        (niwa-mcp--disconnect client)
      (let* ((cell (cons nil 'pending))
             (reply (cons kind id))
             (complete
              (lambda (result &optional failure)
                (when (eq (gethash id calls) cell)
                  (remhash id calls)
                  (setcdr cell 'completed)
                  (if failure (niwa-mcp--error client id -32603 failure reply)
                    ;; Raw-byte text cannot be serialized; the call must still end.
                    (if-let* ((line (ignore-errors
                                      (niwa-mcp--encode
                                       (list :jsonrpc "2.0" :id id :result result)))))
                        (niwa-mcp--write client line reply)
                      (niwa-mcp--error client id -32603
                                          "Result is not valid JSON" reply)))))))
        (puthash id cell calls)
        (condition-case err
            (progn
              (setcar cell (funcall start complete))
              (when (and (eq (cdr cell) 'cancelled) (functionp (car cell)))
                (funcall (car cell))))
          (error
           ;; A failure while START still owes its answer becomes that answer.
           ;; Once completion began the request is retired, so a failure in
           ;; it, such as a write to a client that stays live, keeps
           ;; signalling instead of vanishing in a retired completion.
           (if (eq (cdr cell) 'completed)
               (signal (car err) (cdr err))
             (funcall complete nil (error-message-string err)))))))))

(defun niwa-mcp--call (server client id params)
  "Dispatch SERVER tool PARAMS for CLIENT request ID."
  (let ((name (plist-get params :name))
        (args (plist-get params :arguments)))
    (if (not (and (stringp name) (listp args)))
        (let ((message "Invalid tool call parameters"))
          (niwa-mcp--error client id -32602 message)
          (when (niwa-mcp-rejected server)
            (funcall (niwa-mcp-rejected server) message)))
      ;; The dispatcher checks NAME against its admitted roster.
      (niwa-mcp--pending
       client id 'tool
       (lambda (complete)
         (funcall (niwa-mcp-dispatch server)
                  name args (plist-get params :_meta) complete)))
      (when-let* ((meta (plist-get params :_meta))
                  ((listp meta))
                  (token (plist-get meta :progressToken))
                  ((or (stringp token) (integerp token)))
                  (cell (gethash id (process-get client 'niwa-mcp-calls))))
        (niwa-mcp--progress client id cell token 1)))))

(defun niwa-mcp--progress (client id cell token count)
  "Report progress COUNT under TOKEN while CLIENT's call ID is still CELL.
Each report schedules the next, so the reports stop once the call is
answered, cancelled or disconnected."
  (niwa-transport-run-at-time
   niwa-mcp--progress-interval
   (lambda ()
     (when (eq cell (gethash id (process-get client 'niwa-mcp-calls)))
       (niwa-mcp--send client (list :jsonrpc "2.0" :method "notifications/progress"
                                       :params (list :progressToken token
                                                     :progress count)))
       (niwa-mcp--progress client id cell token (1+ count))))))

(defun niwa-mcp--receive (server client message)
  "Handle one decoded MESSAGE from SERVER's CLIENT."
  (let ((id (plist-get message :id))
        (method (plist-get message :method))
        (params (plist-get message :params)))
    (cond
     ;; JSON-RPC never answers a response.
     ((and (listp message) (not method)
           (or (plist-member message :result) (plist-member message :error))))
     ((not (and (equal (plist-get message :jsonrpc) "2.0")
                (stringp method)
                (or (not (plist-member message :id))
                    (stringp id) (integerp id))))
      (niwa-mcp--error client :null -32600 "Invalid request"))
     ((not (plist-member message :id))
      (pcase method
        ("notifications/initialized"
         (when (eq (process-get client 'niwa-mcp-state) 'initializing)
           (process-put client 'niwa-mcp-state 'ready)))
        ("notifications/cancelled"
         (niwa-mcp--cancel client (plist-get params :requestId)))))
     ((equal method "initialize")
      (if (process-get client 'niwa-mcp-state)
          (niwa-mcp--error client id -32600 "Already initialized")
        (process-put client 'niwa-mcp-state 'initializing)
        (niwa-mcp--send
         client (list :jsonrpc "2.0" :id id
                      :result '(:protocolVersion "2025-03-26"
                                :capabilities (:tools ())
                                :serverInfo (:name "niwa" :version "1"))))))
     ((equal method "ping")
      (niwa-mcp--send client (list :jsonrpc "2.0" :id id :result '())))
     ((and (equal method "niwa/control") (niwa-mcp-control server))
      (niwa-mcp--pending
       client id 'hook (lambda (complete)
                         (funcall (niwa-mcp-control server) params complete))))
     ((not (eq (process-get client 'niwa-mcp-state) 'ready))
      (niwa-mcp--error client id -32600 "Initialize the connection first"))
     ((equal method "tools/list")
      (niwa-mcp--send
       client (list :jsonrpc "2.0" :id id
                    :result (list :tools (vconcat
                                         (funcall (niwa-mcp-tools server)))))))
     ((equal method "tools/call")
      (niwa-mcp--call server client id params))
     (t (niwa-mcp--error client id -32601 "Unknown method")))))

(defun niwa-mcp--filter (client input)
  "Consume newline-delimited JSON INPUT from CLIENT.
Handling a line may wait and re-enter this filter, so unread input is stored
on CLIENT before each line is handled.  Each input chunk is scanned once."
  (let ((rest (process-get client 'niwa-mcp-rest)))
    (process-put client 'niwa-mcp-rest
                 (cons (if rest (concat (substring (car rest) (cdr rest)) input)
                         input)
                       0)))
  (let (rest)
    (while (and (process-live-p client)
                (setq rest (process-get client 'niwa-mcp-rest)))
      (let* ((text (car rest))
             (end (string-search "\n" text (cdr rest)))
             (chunk (substring text (cdr rest) end))
             (bytes (+ (process-get client 'niwa-mcp-bytes) (string-bytes chunk)))
             ;; Chunks of the unfinished line, newest first.
             (partial (cons chunk (process-get client 'niwa-mcp-partial))))
        (process-put client 'niwa-mcp-rest
                     (and end (< (1+ end) (length text)) (cons text (1+ end))))
        (cond
         ((> bytes niwa-mcp--max-message-bytes)
          (niwa-mcp--disconnect client))
         ((not end)
          (process-put client 'niwa-mcp-partial partial)
          (process-put client 'niwa-mcp-bytes bytes))
         (t
          (process-put client 'niwa-mcp-partial nil)
          (process-put client 'niwa-mcp-bytes 0)
          (let ((line (apply #'concat (nreverse partial))))
            (unless (string-blank-p line)
              (pcase (condition-case nil
                         ;; gptel parses tool arguments with nil for JSON null.
                         (list (json-parse-string line :object-type 'plist
                                                  :null-object nil
                                                  :false-object :json-false))
                       (json-parse-error nil))
                ('nil (niwa-mcp--error client :null -32700 "Invalid JSON"))
                (`(,message)
                 (condition-case err
                     (niwa-mcp--receive
                      (process-get client 'niwa-mcp-server) client message)
                   (error
                    (let ((id (plist-get message :id)))
                      ;; Notifications are never answered.
                      (when (or (stringp id) (integerp id))
                        (niwa-mcp--error client id -32603
                                            (error-message-string err))))))))))))))))

(defvar niwa-mcp--socket-root nil
  "Socket directory established for this Emacs, so every user agrees on it.")

(defun niwa-mcp-socket-root (&optional noerror)
  "Return the owner-only local directory holding every MCP server socket.
One stable parent lets confinement mask all live servers at once.  A success
fixes the directory for this Emacs while it exists; ownership is checked every
call.  A vanished directory (for example a removed runtime directory) holds no
live server, so it is resolved again.  With NOERROR, return the existing
directory last established, or nil, instead of signalling: no server can exist
in a directory that was never established."
  (when (and niwa-mcp--socket-root
             (not (file-directory-p niwa-mcp--socket-root)))
    (setq niwa-mcp--socket-root nil))
  (condition-case err
      (let* ((runtime (getenv "XDG_RUNTIME_DIR"))
             (root (or niwa-mcp--socket-root
                       (file-name-concat
                        (if (and runtime (file-directory-p runtime))
                            runtime temporary-file-directory)
                        (format "niwa-mcp-%d" (user-uid)))))
             attributes)
        (with-file-modes #o700 (make-directory root t))
        (setq attributes (file-attributes root 'integer))
        ;; A shared temporary directory may hold a planted directory or symlink.
        (unless (and (eq t (file-attribute-type attributes))
                     (eql (user-uid) (file-attribute-user-id attributes)))
          (error "MCP socket directory is not owned by this user: %s" root))
        (set-file-modes root #o700)
        (setq niwa-mcp--socket-root root))
    (error (if noerror niwa-mcp--socket-root
             (signal (car err) (cdr err))))))

(defun niwa-mcp-start (tools dispatch &optional control rejected lost)
  "Start a private local MCP server using TOOLS and DISPATCH.
TOOLS is a zero-argument function returning MCP tool-schema plists.
DISPATCH receives a tool name, argument plist, metadata plist and completion
callback.  Metadata preserves the client's optional protocol extensions;
it returns a cancellation function.  Completion takes an MCP result plist.
Completion may be asynchronous, including after ordinary user interactions.
It returns non-nil when the reply reached the client.  A client that closed
before its reply is retired, cancelling its other pending work once; nothing
is retried, so work already done stays done.
The caller owns authorization, must reject tools outside its roster and must
reject obsolete request ownership.
Optional CONTROL handles native hook events on the same private endpoint.
It receives the event plist and a completion callback accepting the hook result
or nil and an error string.  Completion may be asynchronous; only its first
call answers the hook.  CONTROL may return a canceller, called when the hook
client disconnects before completion.  CONTROL is not exposed in discovery
and cannot execute tools.
Optional REJECTED receives a diagnostic when argument-shape validation
rejects a call before dispatch.  It may retire a bounded workload.
Optional LOST receives (:kind KIND :id ID) when a reply could not be written
because its client had closed: KIND is `tool' or `hook' and ID the client's
request id.  It carries no payload and must not end the owner's work, whose
agent reports its own outcome."
  (let* ((default-directory temporary-file-directory)
         (directory (make-temp-file
                     (file-name-concat (niwa-mcp-socket-root) "server-") t))
         (server (niwa-mcp--create :directory directory
                                     :tools tools :dispatch dispatch :control control
                                     :rejected rejected :lost lost)))
    (condition-case err
        (progn
          (set-file-modes directory #o700)
          (setf (niwa-mcp-listener server)
                (make-network-process
                 :name "niwa-mcp" :family 'local :server t :noquery t
                 :service (file-name-concat directory "socket")
                 :coding 'utf-8-unix :filter #'niwa-mcp--filter
                 :sentinel (lambda (client _event)
                             (unless (process-live-p client)
                               (niwa-mcp--disconnect client)))
                 :log (lambda (_listener client _message)
                        (process-put client 'niwa-mcp-server server)
                        (process-put client 'niwa-mcp-bytes 0)
                        (process-put client 'niwa-mcp-calls
                                     (make-hash-table :test #'equal))
                        (push client (niwa-mcp-clients server)))))
          (set-file-modes (file-name-concat directory "socket") #o600)
          server)
      (error (niwa-mcp-stop server) (signal (car err) (cdr err))))))

(defun niwa-mcp-bridge-file ()
  "Return the packaged stdio bridge, rejecting incomplete installations."
  (let ((file (file-name-concat niwa-mcp--source-directory "scripts" "niwa-mcp-stdio.py")))
    (unless (file-readable-p file)
      (user-error "Reinstall niwa with scripts/niwa-mcp-stdio.py included in its package files"))
    file))

(defun niwa-mcp-configuration (server)
  "Return SERVER's ACP stdio MCP configuration as an alist."
  (when (niwa-mcp-closed server) (error "MCP server is closed"))
  (let ((python (or (executable-find "python3")
                    (error "'python3' is required for the MCP bridge"))))
    `((name . "niwa") (command . ,python)
      ;; Isolated mode ignores user site packages and PYTHON* variables.
      (args . ,(vector "-I" (niwa-mcp-bridge-file)
                       (file-name-concat (niwa-mcp-directory server) "socket")))
      (env . []))))

(defun niwa-mcp-hook-command (server)
  "Return the shell command for native hook events on SERVER's private scope."
  (let ((config (niwa-mcp-configuration server)))
    (mapconcat #'shell-quote-argument
               (append (list (alist-get 'command config))
                       (append (alist-get 'args config) nil)
                       '("--hook")) " ")))

(defun niwa-mcp-stop (server)
  "Close SERVER, cancelling pending calls and deleting its private socket."
  (unless (niwa-mcp-closed server)
    (setf (niwa-mcp-closed server) t)
    (when-let* ((listener (niwa-mcp-listener server)))
      (delete-process listener))
    (dolist (client (copy-sequence (niwa-mcp-clients server)))
      (niwa-mcp--disconnect client))
    (when (file-directory-p (niwa-mcp-directory server))
      (delete-directory (niwa-mcp-directory server) t))))

(provide 'niwa-mcp)
;;; niwa-mcp.el ends here
