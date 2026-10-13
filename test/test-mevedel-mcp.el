;;; test-mevedel-mcp.el --- Live MCP transport contracts -*- lexical-binding: t -*-

;;; Commentary:
;; Exercise the same stdio bridge an ACP agent starts, against live Emacs.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'mevedel-mcp)
(require 'helpers
         (file-name-concat
          (file-name-directory (or load-file-name buffer-file-name)) "helpers"))
(require 'mevedel-sigpipe-test-support
         (file-name-concat
          (file-name-directory (or load-file-name buffer-file-name))
          "mevedel-sigpipe-test-support"))

(defmacro mevedel-mcp-test--with-runtime (&rest body)
  "Run BODY with a private XDG runtime directory bound to `runtime'."
  (declare (indent 0) (debug t))
  `(let* ((runtime (make-temp-file "mevedel-mcp-test-" t))
          (mevedel-mcp--socket-root nil)
          (process-environment (cons (concat "XDG_RUNTIME_DIR=" runtime)
                                     process-environment)))
     (unwind-protect (progn ,@body)
       (delete-directory runtime t))))

(defmacro mevedel-mcp-test--with-client (tools dispatch &rest body)
  "Run BODY with a live stdio client for TOOLS and DISPATCH."
  (declare (indent 2) (debug t))
  `(mevedel-mcp-test--with-runtime
    (let* ((server (mevedel-mcp-start ,tools ,dispatch))
          (config (mevedel-mcp-configuration server))
          (stderr (generate-new-buffer " *mcp-test-stderr*"))
          (pending "") replies
          (client
           (make-process
            :name "mcp-test-client" :connection-type 'pipe :noquery t
            :stderr stderr
            :coding 'utf-8-unix
            :command (cons (alist-get 'command config)
                           (append (alist-get 'args config) nil))
            :filter
            (lambda (_process text)
              (setq pending (concat pending text))
              (while (string-match "\n" pending)
                (let ((line (substring pending 0 (match-beginning 0))))
                  (setq pending (substring pending (match-end 0)))
                  (push (json-parse-string line :object-type 'plist
                                          :null-object :null
                                          :false-object :json-false)
                        replies)))))))
     (unwind-protect
         (cl-labels
             ((send (value)
                (process-send-string
                 client (concat (json-serialize value :null-object :null
                                                :false-object :json-false)
                                "\n")))
              (response (id)
                (let ((deadline (+ (float-time) 5)) value)
                  (while (and (not (setq value (seq-find
                                               (lambda (row)
                                                 (equal (plist-get row :id) id))
                                               replies)))
                              (< (float-time) deadline))
                    (accept-process-output nil 0.01))
                  (should value)
                  value))
              (initialize ()
                (send '(:jsonrpc "2.0" :id 1 :method "initialize"
                        :params (:protocolVersion "2025-03-26"
                                 :capabilities (:tools ())
                                 :clientInfo (:name "test" :version "1"))))
                (should (equal "2025-03-26"
                               (plist-get (plist-get (response 1) :result)
                                          :protocolVersion)))
                (send '(:jsonrpc "2.0" :method "notifications/initialized"))))
           ,@body)
       (when (process-live-p client) (delete-process client))
       (when (buffer-live-p stderr) (kill-buffer stderr))
       (mevedel-mcp-stop server)))))

(mevedel-deftest mevedel-mcp-start ()
  ,test
  (test)

  :doc "stdio initialization, discovery and asynchronous calls preserve JSON types"
  (let* ((tools [(:name "Inspect" :description "Inspect input"
                 :inputSchema (:type "object" :properties ()))])
         called finish)
    (mevedel-mcp-test--with-client
        (lambda () tools)
        (lambda (name args metadata callback)
          (setq called (list name args metadata) finish callback)
          #'ignore)
      (initialize)
      (send '(:jsonrpc "2.0" :id 2 :method "tools/list"))
      (should (equal tools (plist-get (plist-get (response 2) :result) :tools)))
      (send '(:jsonrpc "2.0" :id 3 :method "tools/call"
              :params (:name "Inspect" :arguments (:flag :json-false :empty :null)
                       :_meta (:claudecode/toolUseId "toolu_native"))))
      ;; A pending tool must not block another request or the command loop.
      (send '(:jsonrpc "2.0" :id 4 :method "ping"))
      (response 4)
      ;; JSON null reaches dispatch as nil, matching gptel's argument parsing.
      (should (equal called '("Inspect" (:flag :json-false :empty nil)
                             (:claudecode/toolUseId "toolu_native"))))
      (should-not (seq-find (lambda (row) (equal (plist-get row :id) 3)) replies))
      (funcall finish '(:content [(:type "text" :text "Done")]))
      (should (equal "Done" (plist-get (aref (plist-get
                                             (plist-get (response 3) :result)
                                             :content) 0) :text)))
      (funcall finish '(:content [(:type "text" :text "Duplicate")]))
      (send '(:jsonrpc "2.0" :id 5 :method "ping"))
      (response 5)
      (should (= 1 (length (seq-filter
                           (lambda (row) (equal (plist-get row :id) 3)) replies))))))

  :doc "cancellation retires callbacks before invoking cancellation work"
  (let (finish (cancelled 0))
    (mevedel-mcp-test--with-client
        (lambda () [(:name "Wait" :inputSchema (:type "object"))])
        (lambda (_name _args _metadata callback)
          (setq finish callback)
          (lambda ()
            (cl-incf cancelled)
            (funcall callback '(:content []))))
      (initialize)
      (send '(:jsonrpc "2.0" :id 2 :method "tools/call" :params (:name "Wait")))
      (send '(:jsonrpc "2.0" :method "notifications/cancelled"
              :params (:requestId 2)))
      (send '(:jsonrpc "2.0" :id 3 :method "ping"))
      (response 3)
      (should (= cancelled 1))
      (funcall finish '(:content []))
      (send '(:jsonrpc "2.0" :id 4 :method "ping"))
      (response 4)
      (should-not (seq-find (lambda (row) (equal (plist-get row :id) 2)) replies))))

  :doc "invalid envelopes fail without dispatch and parsing recovers"
  (let ((calls 0))
    (mevedel-mcp-test--with-client
        (lambda () [])
        (lambda (&rest _) (cl-incf calls))
      (process-send-string client "{broken}\n")
      (should (= -32700 (plist-get (plist-get (response :null) :error) :code)))
      (setq replies nil)
      (send [1 2])
      (should (= -32600 (plist-get (plist-get (response :null) :error) :code)))
      (setq replies nil)
      (process-send-string client "{\"jsonrpc\":\"2.0\",\"id\":null,\"method\":\"ping\"}\n")
      (should (= -32600 (plist-get (plist-get (response :null) :error) :code)))
      (initialize)
      (send '(:jsonrpc "2.0" :id 2 :method "tools/call"
              :params (:name 7)))
      (should (= -32602 (plist-get (plist-get (response 2) :error) :code)))
      (send '(:jsonrpc "2.0" :id 3 :method "tools/call"
              :params (:name "Inspect" :arguments [1])))
      (should (= -32602 (plist-get (plist-get (response 3) :error) :code)))
      (should (= calls 0))))

  :doc "blank lines and responses get no reply; internal errors answer their request"
  (mevedel-mcp-test--with-client
      (lambda () (error "Schema build failed"))
      #'ignore
    (initialize)
    (process-send-string client "\n  \n")
    (send '(:jsonrpc "2.0" :id 8 :result ()))
    (send '(:jsonrpc "2.0" :id 9 :error (:code 1 :message "Client failure")))
    (send '(:jsonrpc "2.0" :id 2 :method "tools/list"))
    (let ((error (plist-get (response 2) :error)))
      (should (= -32603 (plist-get error :code)))
      (should (equal "Schema build failed" (plist-get error :message))))
    (send '(:jsonrpc "2.0" :id 3 :method "ping"))
    (response 3)
    (should (equal '(3 2 1) (mapcar (lambda (row) (plist-get row :id)) replies))))

  :doc "a dispatch waiting in the filter still reads split and later lines"
  (let (seen trigger)
    (mevedel-mcp-test--with-client
        (lambda () [(:name "T" :inputSchema (:type "object"))])
        (lambda (_name args _metadata complete)
          (push (plist-get args :n) seen)
          (when (eql 1 (plist-get args :n))
            ;; A nested wait re-enters the filter with the next message.
            (funcall trigger)
            (mevedel-test--await 5 "Nested message was not read" (memql 2 seen)))
          (funcall complete '(:content []))
          #'ignore)
      (initialize)
      (let ((first (json-serialize '(:jsonrpc "2.0" :id 10 :method "tools/call"
                                     :params (:name "T" :arguments (:n 1))))))
        (setq trigger (lambda ()
                        (send '(:jsonrpc "2.0" :id 11 :method "tools/call"
                                :params (:name "T" :arguments (:n 2))))))
        (process-send-string client (substring first 0 20))
        (mevedel-test--await 5 "Partial line was not read"
          (process-get (car (mevedel-mcp-clients server))
                       'mevedel-mcp-partial))
        (process-send-string client (concat (substring first 20) "\n")))
      (should-not (plist-get (response 11) :error))
      (should-not (plist-get (response 10) :error))
      (should (equal '(1 2) (reverse seen)))))

  :doc "an unserializable result still answers the call with an error"
  (let (finish)
    (mevedel-mcp-test--with-client
        (lambda () [(:name "T" :inputSchema (:type "object"))])
        (lambda (_name _args _metadata complete) (setq finish complete) #'ignore)
      (initialize)
      (send '(:jsonrpc "2.0" :id 2 :method "tools/call" :params (:name "T")))
      (mevedel-test--await 5 "Call was not dispatched" finish)
      (funcall finish (list :content (vector (list :type "text" :text "a\377b"))))
      (should (= -32603 (plist-get (plist-get (response 2) :error) :code))))))

(mevedel-deftest mevedel-mcp-stop ()
  ,test
  (test)

  :doc "closing cancels pending work once and removes the private endpoint"
  (let ((cancelled 0))
    (mevedel-mcp-test--with-client
        (lambda () [(:name "Wait" :inputSchema (:type "object"))])
        (lambda (&rest _) (lambda () (cl-incf cancelled)))
      (initialize)
      (send '(:jsonrpc "2.0" :id 2 :method "tools/call" :params (:name "Wait")))
      (send '(:jsonrpc "2.0" :id 3 :method "ping"))
      (response 3)
      (let ((socket (aref (alist-get 'args config) 2)))
        (should (equal "-I" (aref (alist-get 'args config) 0)))
        (should (file-in-directory-p socket (mevedel-mcp-socket-root)))
        (should (= #o700 (file-modes (file-name-directory socket))))
        (should (= #o600 (file-modes socket)))
        (mevedel-mcp-stop server)
        (mevedel-mcp-stop server)
        (should-not (file-exists-p (file-name-directory socket))))
      (should (= 1 cancelled))
      (should-error (mevedel-mcp-configuration server))))

  :doc "disconnect cancels pending work without waiting for server shutdown"
  (let ((cancelled 0))
    (mevedel-mcp-test--with-client
        (lambda () [(:name "Wait" :inputSchema (:type "object"))])
        (lambda (&rest _) (lambda () (cl-incf cancelled)))
      (initialize)
      (send '(:jsonrpc "2.0" :id 2 :method "tools/call" :params (:name "Wait")))
      (send '(:jsonrpc "2.0" :id 3 :method "ping"))
      (response 3)
      (process-send-eof client)
      (let ((deadline (+ (float-time) 5)))
        (while (and (= cancelled 0) (< (float-time) deadline))
          (accept-process-output nil 0.01)))
      (should (= 1 cancelled))))

  :doc "reentrant close cancels work even before dispatch returns its canceller"
  (let (owner (cancelled 0))
    (mevedel-mcp-test--with-client
        (lambda () [(:name "Wait" :inputSchema (:type "object"))])
        (lambda (&rest _)
          (mevedel-mcp-stop owner)
          (lambda () (cl-incf cancelled)))
      (setq owner server)
      (initialize)
      (send '(:jsonrpc "2.0" :id 2 :method "tools/call" :params (:name "Wait")))
      (let ((deadline (+ (float-time) 5)))
        (while (and (process-live-p client) (< (float-time) deadline))
          (accept-process-output nil 0.01)))
      (should (= 1 cancelled)))))

(defmacro mevedel-mcp-test--with-peer (&rest body)
  "Run BODY with SERVER answered through a raw local PEER socket.
Dispatch and control store their completions in COMPLETIONS by request id
and count cancellations in CANCELLED; BODY may replace PEND, which receives
each request's N and completion.  LOST collects disconnect diagnostics and
RECEIVED the text PEER read.  CLIENT is the server's end of PEER's connection
once `connect' returns."
  (declare (indent 0) (debug t))
  `(mevedel-mcp-test--with-runtime
    (let* (completions lost client peer (cancelled 0) (received "")
           (pend (lambda (id complete)
                   (push (cons id complete) completions)
                   (lambda () (cl-incf cancelled))))
           (server (mevedel-mcp-start
                    (lambda () [(:name "Wait" :inputSchema (:type "object"))])
                    (lambda (_name args _meta complete)
                      (funcall pend (plist-get args :n) complete))
                    (lambda (event complete)
                      (funcall pend (plist-get event :n) complete))
                    nil
                    (lambda (diagnostic) (push diagnostic lost)))))
      (unwind-protect
          (cl-labels
              ((send (value)
                 (process-send-string
                  peer (concat (json-serialize value :null-object :null
                                               :false-object :json-false)
                               "\n")))
               (connect ()
                 (setq peer (make-network-process
                             :name "mcp-test-peer" :family 'local :noquery t
                             :filter (lambda (_process text)
                                       (setq received (concat received text)))
                             :sentinel #'ignore
                             :service (aref (alist-get 'args (mevedel-mcp-configuration server)) 2)))
                 (mevedel-test--await 5 "Server did not accept the peer"
                   (setq client (car (mevedel-mcp-clients server)))))
               (initialize ()
                 (send '(:jsonrpc "2.0" :id 1 :method "initialize"))
                 (send '(:jsonrpc "2.0" :method "notifications/initialized")))
               (call (id n)
                 (send `(:jsonrpc "2.0" :id ,id :method "tools/call"
                                  :params (:name "Wait" :arguments (:n ,n)))))
               (control (id n)
                 (send `(:jsonrpc "2.0" :id ,id :method "mevedel/control" :params (:n ,n))))
               (await (&rest ns)
                 (mevedel-test--await 5 "Requests were not dispatched"
                   (cl-every (lambda (n) (assq n completions)) ns)))
               (complete (n &rest result)
                 (apply (cdr (assq n completions)) (or result '((:content [])))))
               (response (id)
                 (let (found)
                   (mevedel-test--await 5 "The peer received no answer"
                     (setq found
                           (seq-find (lambda (message) (equal id (plist-get message :id)))
                                     (mapcar (lambda (line)
                                               (json-parse-string line :object-type 'plist))
                                             (split-string received "\n" t)))))
                   found))
               ;; Close the peer's end without letting Emacs read it: the
               ;; server's client stays live until its end is read.
               (close-unread ()
                 (delete-process peer)
                 (should (process-live-p client))))
            ,@body)
        (when (and peer (process-live-p peer)) (delete-process peer))
        (mevedel-mcp-stop server)))))

(defconst mevedel-mcp-test--file (or load-file-name buffer-file-name)
  "This file, which the SIGPIPE-ignoring child loads.")

;; Writing to a closed peer kills a batch Emacs that does not ignore SIGPIPE;
;; these cases run in the child `mevedel-mcp--write' starts.
(when (mevedel-test-sigpipe-child-p)
  (mevedel-deftest mevedel-mcp--write/closed-peer ()
    ,test
    (test)

    :doc "a tool reply to a peer closed before Emacs read its end retires the client once"
    (mevedel-mcp-test--with-peer
    (connect)
    (initialize)
    (call 2 1)
    (call 3 2)
    (await 1 2)
    (close-unread)
    ;; The local result stays the caller's; only the reply is lost.
    (should-not (complete 1))
    (should-not (process-live-p client))
    (should-not (memq client (mevedel-mcp-clients server)))
    (should (= 1 cancelled))
    (should (equal '((:kind tool :id 2)) lost))
    ;; A duplicate or late completion neither writes nor cancels again.
    (should-not (complete 1))
    (should-not (complete 2))
    (accept-process-output nil 0.05)
    (should (= 1 cancelled))
    (should (= 1 (length lost))))

    :doc "a hook reply to a peer closed before Emacs read its end is reported as a hook"
    (mevedel-mcp-test--with-peer
    (connect)
    (control 7 1)
    (await 1)
    (close-unread)
    (should-not (complete 1 '(:continue t)))
    (should-not (process-live-p client))
    (should (equal '((:kind hook :id 7)) lost)))))

(mevedel-deftest mevedel-mcp--write ()
  ,test
  (test)

  :doc "replies to peers closed before Emacs read their end, where SIGPIPE is ignored"
  (mevedel-test-run-sigpipe-tests mevedel-mcp-test--file "\\`mevedel-mcp--write/closed-peer")

  :doc "a reply after Emacs read the closed end finds its call already retired"
  (mevedel-mcp-test--with-peer
    (connect)
    (initialize)
    (call 2 1)
    (await 1)
    (delete-process peer)
    (mevedel-test--await 5 "Disconnect was not processed" (not (process-live-p client)))
    (should (= 1 cancelled))
    (should-not (complete 1))
    (should-not lost))

  :doc "a delivered reply reports delivery"
  (mevedel-mcp-test--with-peer
    (connect)
    (initialize)
    (call 2 1)
    (await 1)
    (should (complete 1))
    (should (process-live-p client))
    (should-not lost))

  :doc "a failed write to a client that stays live still signals"
  (mevedel-mcp-test--with-peer
    (connect)
    (initialize)
    (call 2 1)
    (await 1)
    (cl-letf (((symbol-function 'process-send-string)
               (lambda (&rest _) (error "Unexpected write failure"))))
      (should-error (complete 1)))
    (should (process-live-p client))
    (should-not lost))

  :doc "a synchronous reply whose live write fails reaches its owner and its peer"
  (mevedel-mcp-test--with-peer
    (let ((send (symbol-function 'process-send-string))
          (failed 0) owner-error)
      (setq pend (lambda (_n complete)
                   ;; The owner answers before dispatch returns.
                   (condition-case err
                       (funcall complete '(:content [(:type "text" :text "done")]))
                     (error (setq owner-error err)
                            (signal (car err) (cdr err))))))
      (connect)
      (initialize)
      (cl-letf (((symbol-function 'process-send-string)
                 (lambda (process string)
                   ;; The result line fails once while its client stays live.
                   (if (and (eq process client) (= failed 0)
                            (string-search "\"result\":{\"content\"" string))
                       (progn (cl-incf failed)
                              (error "Injected write failure"))
                     (funcall send process string)))))
        (call 2 1)
        (let ((answer (response 2)))
          (should (= 1 failed))
          (should (equal "Injected write failure" (cadr owner-error)))
          (should (equal "Injected write failure"
                         (plist-get (plist-get answer :error) :message)))))
      (should (process-live-p client))
      (should (= 0 (hash-table-count (process-get client 'mevedel-mcp-calls))))
      (should-not lost))))

(mevedel-deftest mevedel-mcp-socket-root ()
  ,test
  (test)

  :doc "every server socket shares one owner-only runtime directory"
  (mevedel-mcp-test--with-runtime
    (let ((root (mevedel-mcp-socket-root)))
      (should (file-in-directory-p root runtime))
      (should (= #o700 (file-modes root)))
      (should (equal root (mevedel-mcp-socket-root)))))

  :doc "the first established directory stays fixed for servers and confinement"
  (mevedel-mcp-test--with-runtime
    (let ((root (mevedel-mcp-socket-root))
          (process-environment (cons (concat "XDG_RUNTIME_DIR="
                                             temporary-file-directory)
                                     process-environment)))
      (should (equal root (mevedel-mcp-socket-root)))))

  :doc "a vanished directory is resolved again instead of reused"
  (mevedel-mcp-test--with-runtime
    (let* ((root (mevedel-mcp-socket-root))
           (other (make-temp-file "mevedel-mcp-runtime-" t))
           (process-environment (cons (concat "XDG_RUNTIME_DIR=" other)
                                      process-environment)))
      (unwind-protect
          (progn
            (delete-directory root t)
            (should (file-in-directory-p (mevedel-mcp-socket-root) other)))
        (delete-directory other t))))

  :doc "a planted symlink in place of the directory is refused"
  (mevedel-mcp-test--with-runtime
    (make-symbolic-link runtime (file-name-concat
                                 runtime (format "mevedel-mcp-%d" (user-uid))))
    (should-error (mevedel-mcp-socket-root))
    (should-not (mevedel-mcp-socket-root t))))

(mevedel-deftest mevedel-mcp-hook-command ()
  ,test
  (test)

  :doc "native hook bridge returns owner decisions and stops on a lost owner"
  (mevedel-mcp-test--with-runtime
   (let* (stop seen timer
         (server (mevedel-mcp-start
                  (lambda () []) #'ignore
                  (lambda (event complete)
                    (setq seen event)
                    (setq timer
                          (run-at-time
                           .01 nil
                           (lambda ()
                             (funcall complete
                                      (if stop '(:continue :json-false :stopReason "Goal paused")
                                        '(:continue t)))
                             (funcall complete '(:continue :json-false :stopReason "Duplicate reply"))))))))
         (command (mevedel-mcp-hook-command server)))
    (unwind-protect
        (cl-labels
            ((invoke ()
               (let ((output (generate-new-buffer " *mcp-hook-output*"))
                     (errors (generate-new-buffer " *mcp-hook-errors*")) process)
                 (unwind-protect
                     (progn
                       (setq process
                             (make-process :name "mcp-hook-test" :noquery t
                                           :connection-type 'pipe :coding 'utf-8-unix
                                           :buffer output :stderr errors :sentinel #'ignore
                                           :command (list "sh" "-c" command)))
                       (process-send-string process "{\"hook_event_name\":\"PostToolBatch\"}\n")
                       (process-send-eof process)
                       (mevedel-test--await 5 "Hook bridge did not return"
                         (not (process-live-p process)))
                       (should (= 0 (process-exit-status process)))
                       (with-current-buffer output
                         (json-parse-string (buffer-string) :object-type 'plist
                                            :false-object :json-false)))
                   (when (and process (process-live-p process)) (delete-process process))
                   (kill-buffer output) (kill-buffer errors)))))
          (should (eq t (plist-get (invoke) :continue)))
          (should (equal "PostToolBatch" (plist-get seen :hook_event_name)))
          (setq stop t)
          (should (equal '(:continue :json-false :stopReason "Goal paused") (invoke)))
          (mevedel-mcp-stop server)
          (should (eq :json-false (plist-get (invoke) :continue))))
      (when timer (cancel-timer timer))
      (mevedel-mcp-stop server))))

  :doc "a hook client that disconnects cancels its queued control"
  (mevedel-mcp-test--with-runtime
   (let* ((cancelled 0) answer connection
          (server (mevedel-mcp-start
                   (lambda () []) #'ignore
                   (lambda (_event complete)
                     (setq answer complete)
                     (lambda () (cl-incf cancelled))))))
     (unwind-protect
         (progn
           (setq connection
                 (make-network-process
                  :name "mcp-hook-abandon" :family 'local :noquery t
                  :service (aref (alist-get 'args (mevedel-mcp-configuration server)) 2)))
           (process-send-string
            connection "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"mevedel/control\",\"params\":{}}\n")
           (mevedel-test--await 5 "Control was not received" answer)
           (delete-process connection)
           (mevedel-test--await 5 "Control was not cancelled" (not (= 0 cancelled)))
           (funcall answer '(:continue t))
           (should (= 1 cancelled)))
       (when (process-live-p connection) (delete-process connection))
       (mevedel-mcp-stop server)))))

(provide 'test-mevedel-mcp)
;;; test-mevedel-mcp.el ends here
