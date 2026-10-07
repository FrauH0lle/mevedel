;;; mevedel-mcp.el --- Asynchronous local MCP tool server -*- lexical-binding: t -*-

;;; Commentary:

;; Exposes one caller-owned tool scope to an external MCP client.  The caller
;; supplies discovery and dispatch; this module owns the private socket, JSON
;; protocol, pending calls, cancellation and transport cleanup.  Dispatch may
;; wait for an ordinary Emacs interaction without blocking another request.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)

;; `mevedel-utilities'
(autoload 'mevedel-library-source-directory "mevedel-utilities")

(defconst mevedel-mcp--source-directory
  (mevedel-library-source-directory (or load-file-name buffer-file-name))
  "Directory containing the packaged stdio bridge.")

(defconst mevedel-mcp--max-message-bytes (* 32 1024 1024)
  "Maximum size of one incoming MCP message.")

(cl-defstruct (mevedel-mcp (:constructor mevedel-mcp--create))
  "Private local server for one caller-owned tool scope."
  directory listener clients tools dispatch control rejected closed)

(defun mevedel-mcp--encode (message)
  "Return MESSAGE as one newline-terminated JSON line."
  (concat (json-serialize message :null-object :null :false-object :json-false)
          "\n"))

(defun mevedel-mcp--write (client line)
  "Write encoded LINE to live CLIENT."
  (when (process-live-p client)
    (process-send-string client line)))

(defun mevedel-mcp--send (client message)
  "Send JSON MESSAGE to live CLIENT."
  (mevedel-mcp--write client (mevedel-mcp--encode message)))

(defun mevedel-mcp--error (client id code message)
  "Reply to CLIENT request ID with error CODE and MESSAGE."
  (mevedel-mcp--send
   client (list :jsonrpc "2.0" :id id
                :error (list :code code :message message))))

;; Calls and hook controls share one pending table, so cancellation and
;; disconnect retire whichever is still unanswered.
(defun mevedel-mcp--cancel (client id)
  "Retire CLIENT request ID and cancel its pending work."
  (let* ((calls (process-get client 'mevedel-mcp-calls))
         (cell (and calls (gethash id calls))))
    (when cell
      ;; Retire before cancellation: cancellers can synchronously call back.
      (remhash id calls)
      (setcdr cell 'cancelled)
      (when (functionp (car cell))
        (funcall (car cell))))))

(defun mevedel-mcp--disconnect (client)
  "Cancel pending work and release CLIENT."
  (when-let* ((server (process-get client 'mevedel-mcp-server)))
    (setf (mevedel-mcp-clients server)
          (delq client (mevedel-mcp-clients server))))
  (when-let* ((calls (process-get client 'mevedel-mcp-calls)))
    (dolist (id (hash-table-keys calls))
      (condition-case nil
          (mevedel-mcp--cancel client id)
        (error nil))))
  (when (process-live-p client) (delete-process client)))

(defun mevedel-mcp--pending (client id start)
  "Answer CLIENT request ID when asynchronous START completes.
START receives a completion taking a result plist, or nil and an error
message.  It may return a canceller for client cancellation or disconnect.
Only the first completion of a still-pending request answers it."
  (let ((calls (process-get client 'mevedel-mcp-calls)))
    (if (gethash id calls)
        ;; Duplicate live IDs cannot identify which invocation is being answered.
        (mevedel-mcp--disconnect client)
      (let* ((cell (cons nil 'pending))
             (complete
              (lambda (result &optional failure)
                (when (eq (gethash id calls) cell)
                  (remhash id calls)
                  (setcdr cell 'completed)
                  (if failure (mevedel-mcp--error client id -32603 failure)
                    ;; Raw-byte text cannot be serialized; the call must still end.
                    (if-let* ((line (ignore-errors
                                      (mevedel-mcp--encode
                                       (list :jsonrpc "2.0" :id id :result result)))))
                        (mevedel-mcp--write client line)
                      (mevedel-mcp--error client id -32603
                                          "Result is not valid JSON")))))))
        (puthash id cell calls)
        (condition-case err
            (progn
              (setcar cell (funcall start complete))
              (when (and (eq (cdr cell) 'cancelled) (functionp (car cell)))
                (funcall (car cell))))
          (error (funcall complete nil (error-message-string err))))))))

(defun mevedel-mcp--call (server client id params)
  "Dispatch SERVER tool PARAMS for CLIENT request ID."
  (let ((name (plist-get params :name))
        (args (plist-get params :arguments)))
    (if (not (and (stringp name) (listp args)))
        (let ((message "Invalid tool call parameters"))
          (mevedel-mcp--error client id -32602 message)
          (when (mevedel-mcp-rejected server)
            (funcall (mevedel-mcp-rejected server) message)))
      ;; The dispatcher checks NAME against its admitted roster.
      (mevedel-mcp--pending
       client id
       (lambda (complete)
         (funcall (mevedel-mcp-dispatch server)
                  name args (plist-get params :_meta) complete))))))

(defun mevedel-mcp--receive (server client message)
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
      (mevedel-mcp--error client :null -32600 "Invalid request"))
     ((not (plist-member message :id))
      (pcase method
        ("notifications/initialized"
         (when (eq (process-get client 'mevedel-mcp-state) 'initializing)
           (process-put client 'mevedel-mcp-state 'ready)))
        ("notifications/cancelled"
         (mevedel-mcp--cancel client (plist-get params :requestId)))))
     ((equal method "initialize")
      (if (process-get client 'mevedel-mcp-state)
          (mevedel-mcp--error client id -32600 "Already initialized")
        (process-put client 'mevedel-mcp-state 'initializing)
        (mevedel-mcp--send
         client (list :jsonrpc "2.0" :id id
                      :result '(:protocolVersion "2025-03-26"
                                :capabilities (:tools ())
                                :serverInfo (:name "mevedel" :version "1"))))))
     ((equal method "ping")
      (mevedel-mcp--send client (list :jsonrpc "2.0" :id id :result '())))
     ((and (equal method "mevedel/control") (mevedel-mcp-control server))
      (mevedel-mcp--pending
       client id (lambda (complete)
                   (funcall (mevedel-mcp-control server) params complete))))
     ((not (eq (process-get client 'mevedel-mcp-state) 'ready))
      (mevedel-mcp--error client id -32600 "Initialize the connection first"))
     ((equal method "tools/list")
      (mevedel-mcp--send
       client (list :jsonrpc "2.0" :id id
                    :result (list :tools (vconcat
                                         (funcall (mevedel-mcp-tools server)))))))
     ((equal method "tools/call")
      (mevedel-mcp--call server client id params))
     (t (mevedel-mcp--error client id -32601 "Unknown method")))))

(defun mevedel-mcp--filter (client input)
  "Consume newline-delimited JSON INPUT from CLIENT.
Handling a line may wait and re-enter this filter, so unread input is stored
on CLIENT before each line is handled.  Each input chunk is scanned once."
  (let ((rest (process-get client 'mevedel-mcp-rest)))
    (process-put client 'mevedel-mcp-rest
                 (cons (if rest (concat (substring (car rest) (cdr rest)) input)
                         input)
                       0)))
  (let (rest)
    (while (and (process-live-p client)
                (setq rest (process-get client 'mevedel-mcp-rest)))
      (let* ((text (car rest))
             (end (string-search "\n" text (cdr rest)))
             (chunk (substring text (cdr rest) end))
             (bytes (+ (process-get client 'mevedel-mcp-bytes) (string-bytes chunk)))
             ;; Chunks of the unfinished line, newest first.
             (partial (cons chunk (process-get client 'mevedel-mcp-partial))))
        (process-put client 'mevedel-mcp-rest
                     (and end (< (1+ end) (length text)) (cons text (1+ end))))
        (cond
         ((> bytes mevedel-mcp--max-message-bytes)
          (mevedel-mcp--disconnect client))
         ((not end)
          (process-put client 'mevedel-mcp-partial partial)
          (process-put client 'mevedel-mcp-bytes bytes))
         (t
          (process-put client 'mevedel-mcp-partial nil)
          (process-put client 'mevedel-mcp-bytes 0)
          (let ((line (apply #'concat (nreverse partial))))
            (unless (string-blank-p line)
              (pcase (condition-case nil
                         ;; gptel parses tool arguments with nil for JSON null.
                         (list (json-parse-string line :object-type 'plist
                                                  :null-object nil
                                                  :false-object :json-false))
                       (json-parse-error nil))
                ('nil (mevedel-mcp--error client :null -32700 "Invalid JSON"))
                (`(,message)
                 (condition-case err
                     (mevedel-mcp--receive
                      (process-get client 'mevedel-mcp-server) client message)
                   (error
                    (let ((id (plist-get message :id)))
                      ;; Notifications are never answered.
                      (when (or (stringp id) (integerp id))
                        (mevedel-mcp--error client id -32603
                                            (error-message-string err))))))))))))))))

(defvar mevedel-mcp--socket-root nil
  "Socket directory established for this Emacs, so every user agrees on it.")

(defun mevedel-mcp-socket-root (&optional noerror)
  "Return the owner-only local directory holding every MCP server socket.
One stable parent lets confinement mask all live servers at once.  A success
fixes the directory for this Emacs while it exists; ownership is checked every
call.  A vanished directory (for example a removed runtime directory) holds no
live server, so it is resolved again.  With NOERROR, return the existing
directory last established, or nil, instead of signalling: no server can exist
in a directory that was never established."
  (when (and mevedel-mcp--socket-root
             (not (file-directory-p mevedel-mcp--socket-root)))
    (setq mevedel-mcp--socket-root nil))
  (condition-case err
      (let* ((runtime (getenv "XDG_RUNTIME_DIR"))
             (root (or mevedel-mcp--socket-root
                       (file-name-concat
                        (if (and runtime (file-directory-p runtime))
                            runtime temporary-file-directory)
                        (format "mevedel-mcp-%d" (user-uid)))))
             attributes)
        (with-file-modes #o700 (make-directory root t))
        (setq attributes (file-attributes root 'integer))
        ;; A shared temporary directory may hold a planted directory or symlink.
        (unless (and (eq t (file-attribute-type attributes))
                     (eql (user-uid) (file-attribute-user-id attributes)))
          (error "MCP socket directory is not owned by this user: %s" root))
        (set-file-modes root #o700)
        (setq mevedel-mcp--socket-root root))
    (error (if noerror mevedel-mcp--socket-root
             (signal (car err) (cdr err))))))

(defun mevedel-mcp-start (tools dispatch &optional control rejected)
  "Start a private local MCP server using TOOLS and DISPATCH.
TOOLS is a zero-argument function returning MCP tool-schema plists.
DISPATCH receives a tool name, argument plist, metadata plist and completion
callback.  Metadata preserves the client's optional protocol extensions;
it returns a cancellation function.  Completion takes an MCP result plist.
Completion may be asynchronous, including after ordinary user interactions.
The caller owns authorization, must reject tools outside its roster and must
reject obsolete request ownership.
Optional CONTROL handles native hook events on the same private endpoint.
It receives the event plist and a completion callback accepting the hook result
or nil and an error string.  Completion may be asynchronous; only its first
call answers the hook.  CONTROL may return a canceller, called when the hook
client disconnects before completion.  CONTROL is not exposed in discovery
and cannot execute tools.
Optional REJECTED receives a diagnostic when argument-shape validation
rejects a call before dispatch.  It may retire a bounded workload."
  (let* ((default-directory temporary-file-directory)
         (directory (make-temp-file
                     (file-name-concat (mevedel-mcp-socket-root) "server-") t))
         (server (mevedel-mcp--create :directory directory
                                     :tools tools :dispatch dispatch :control control
                                     :rejected rejected)))
    (condition-case err
        (progn
          (set-file-modes directory #o700)
          (setf (mevedel-mcp-listener server)
                (make-network-process
                 :name "mevedel-mcp" :family 'local :server t :noquery t
                 :service (file-name-concat directory "socket")
                 :coding 'utf-8-unix :filter #'mevedel-mcp--filter
                 :sentinel (lambda (client _event)
                             (unless (process-live-p client)
                               (mevedel-mcp--disconnect client)))
                 :log (lambda (_listener client _message)
                        (process-put client 'mevedel-mcp-server server)
                        (process-put client 'mevedel-mcp-bytes 0)
                        (process-put client 'mevedel-mcp-calls
                                     (make-hash-table :test #'equal))
                        (push client (mevedel-mcp-clients server)))))
          (set-file-modes (file-name-concat directory "socket") #o600)
          server)
      (error (mevedel-mcp-stop server) (signal (car err) (cdr err))))))

(defun mevedel-mcp-bridge-file ()
  "Return the packaged stdio bridge, rejecting incomplete installations."
  (let ((file (file-name-concat mevedel-mcp--source-directory "scripts" "mevedel-mcp-stdio.py")))
    (unless (file-readable-p file)
      (user-error "Reinstall mevedel with scripts/mevedel-mcp-stdio.py included in its package files"))
    file))

(defun mevedel-mcp-configuration (server)
  "Return SERVER's ACP stdio MCP configuration as an alist."
  (when (mevedel-mcp-closed server) (error "MCP server is closed"))
  (let ((python (or (executable-find "python3")
                    (error "'python3' is required for the MCP bridge"))))
    `((name . "mevedel") (command . ,python)
      ;; Isolated mode ignores user site packages and PYTHON* variables.
      (args . ,(vector "-I" (mevedel-mcp-bridge-file)
                       (file-name-concat (mevedel-mcp-directory server) "socket")))
      (env . []))))

(defun mevedel-mcp-hook-command (server)
  "Return the shell command for native hook events on SERVER's private scope."
  (let ((config (mevedel-mcp-configuration server)))
    (mapconcat #'shell-quote-argument
               (append (list (alist-get 'command config))
                       (append (alist-get 'args config) nil)
                       '("--hook")) " ")))

(defun mevedel-mcp-stop (server)
  "Close SERVER, cancelling pending calls and deleting its private socket."
  (unless (mevedel-mcp-closed server)
    (setf (mevedel-mcp-closed server) t)
    (when-let* ((listener (mevedel-mcp-listener server)))
      (delete-process listener))
    (dolist (client (copy-sequence (mevedel-mcp-clients server)))
      (mevedel-mcp--disconnect client))
    (when (file-directory-p (mevedel-mcp-directory server))
      (delete-directory (mevedel-mcp-directory server) t))))

(provide 'mevedel-mcp)
;;; mevedel-mcp.el ends here
