;;; mevedel-mcp.el --- Asynchronous local MCP tool server -*- lexical-binding: t -*-

;;; Commentary:

;; Exposes one caller-owned tool scope to an external MCP client.  The caller
;; supplies discovery and dispatch; this module owns the private socket, JSON
;; protocol, pending calls, cancellation and transport cleanup.  Dispatch may
;; wait for an ordinary Emacs interaction without blocking another request.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'seq)
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

(defun mevedel-mcp--send (client message)
  "Send JSON MESSAGE to live CLIENT."
  (when (process-live-p client)
    (process-send-string
     client (concat (json-serialize message :null-object :null
                                    :false-object :json-false) "\n"))))

(defun mevedel-mcp--error (client id code message)
  "Reply to CLIENT request ID with error CODE and MESSAGE."
  (mevedel-mcp--send
   client (list :jsonrpc "2.0" :id id
                :error (list :code code :message message))))

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

(defun mevedel-mcp--call (server client id params)
  "Dispatch SERVER tool PARAMS for CLIENT request ID."
  (let ((name (plist-get params :name))
        (args (plist-get params :arguments))
        (calls (process-get client 'mevedel-mcp-calls)))
    (cond
     ((or (not (stringp name)) (not (listp args))
          (not (seq-find (lambda (tool) (equal name (plist-get tool :name)))
                         (funcall (mevedel-mcp-tools server)))))
      (let ((message "Unknown tool or invalid arguments"))
        (mevedel-mcp--error client id -32602 message)
        (when (mevedel-mcp-rejected server)
          (funcall (mevedel-mcp-rejected server) message))))
     ((gethash id calls)
      ;; Duplicate live IDs cannot identify which invocation is being answered.
      (mevedel-mcp--disconnect client))
     (t
      (let* ((cell (cons nil 'pending))
             (finish
              (lambda (result)
                (when (eq (gethash id calls) cell)
                  (remhash id calls)
                  (setcdr cell 'completed)
                  (mevedel-mcp--send
                   client (list :jsonrpc "2.0" :id id :result result))))))
        (puthash id cell calls)
        (condition-case err
            (progn
              (setcar cell (funcall (mevedel-mcp-dispatch server)
                                   name args (plist-get params :_meta) finish))
              (when (and (eq (cdr cell) 'cancelled) (functionp (car cell)))
                (funcall (car cell))))
          (error
           (funcall finish
                    (list :isError t :content
                          (vector (list :type "text" :text
                                        (error-message-string err))))))))))))

(defun mevedel-mcp--receive (server client message)
  "Handle one decoded MESSAGE from SERVER's CLIENT."
  (let ((id (plist-get message :id))
        (method (plist-get message :method))
        (params (plist-get message :params)))
    (cond
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
                                :capabilities (:tools (:listChanged t))
                                :serverInfo (:name "mevedel" :version "1"))))))
     ((equal method "ping")
      (mevedel-mcp--send client (list :jsonrpc "2.0" :id id :result '())))
     ((and (equal method "mevedel/control") (mevedel-mcp-control server))
      (let (done)
        (cl-labels ((complete (result &optional failure)
                     (unless done
                       (setq done t)
                       (if failure (mevedel-mcp--error client id -32603 failure)
                         (mevedel-mcp--send client (list :jsonrpc "2.0" :id id :result result))))))
          (condition-case err
              (funcall (mevedel-mcp-control server) params #'complete)
            (error (complete nil (error-message-string err)))))))
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
  "Consume newline-delimited JSON INPUT from CLIENT."
  (let ((text (concat (process-get client 'mevedel-mcp-input) input)) end)
    (while (and (process-live-p client) (setq end (string-search "\n" text)))
      (let ((line (substring text 0 end)))
        (setq text (substring text (1+ end)))
        (if (> (string-bytes line) mevedel-mcp--max-message-bytes)
            (mevedel-mcp--disconnect client)
          (condition-case err
              (let ((message (json-parse-string line :object-type 'plist
                                                :null-object :null
                                                :false-object :json-false)))
                (mevedel-mcp--receive
                 (process-get client 'mevedel-mcp-server) client message))
            (json-parse-error
             (mevedel-mcp--error client :null -32700 "Invalid JSON"))
            (error
             (mevedel-mcp--error client :null -32603
                                 (error-message-string err)))))))
    (if (> (string-bytes text) mevedel-mcp--max-message-bytes)
        (mevedel-mcp--disconnect client)
      (process-put client 'mevedel-mcp-input text))))

(defun mevedel-mcp-start (tools dispatch &optional control rejected)
  "Start a private local MCP server using TOOLS and DISPATCH.
TOOLS is a zero-argument function returning MCP tool-schema plists.
DISPATCH receives a tool name, argument plist, metadata plist and completion
callback.  Metadata preserves the client's optional protocol extensions;
it returns a cancellation function.  Completion takes an MCP result plist.
Completion may be asynchronous, including after ordinary user interactions.
The caller owns authorization and must reject obsolete request ownership.
Optional CONTROL handles native hook events on the same private endpoint.
It receives the event plist and a completion callback accepting the hook result
and optional error string. Completion may be asynchronous; only its first call
answers the hook. CONTROL is not exposed in discovery and cannot execute tools.
Optional REJECTED receives a diagnostic when tool discovery or argument-shape
validation rejects a call before dispatch.  It may retire a bounded workload."
  (let* ((default-directory temporary-file-directory)
         (directory (make-temp-file "mevedel-mcp-" t))
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
                        (process-put client 'mevedel-mcp-input "")
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
      (args . ,(vector (mevedel-mcp-bridge-file)
                       (file-name-concat (mevedel-mcp-directory server) "socket")))
      (env . []))))

(defun mevedel-mcp-tools-changed (server)
  "Notify initialized clients that SERVER's tool scope changed."
  (dolist (client (mevedel-mcp-clients server))
    (when (eq (process-get client 'mevedel-mcp-state) 'ready)
      (mevedel-mcp--send
       client '(:jsonrpc "2.0" :method "notifications/tools/list_changed")))))

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
