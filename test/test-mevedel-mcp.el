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

(defmacro mevedel-mcp-test--with-runtime (&rest body)
  "Run BODY with a private XDG runtime directory bound to `runtime'."
  (declare (indent 0) (debug t))
  `(let* ((runtime (make-temp-file "mevedel-mcp-test-" t))
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

  :doc "a dispatch waiting in the filter still reads split and later lines"
  (let (seen trigger)
    (mevedel-mcp-test--with-client
        (lambda () [(:name "T" :inputSchema (:type "object"))])
        (lambda (_name args _metadata complete)
          (push (plist-get args :n) seen)
          (when (eql 1 (plist-get args :n))
            ;; A nested wait re-enters the filter with the next message.
            (funcall trigger)
            (with-timeout (5 (ert-fail "Nested message was not read"))
              (while (not (memql 2 seen)) (accept-process-output nil 0.01))))
          (funcall complete '(:content []))
          #'ignore)
      (initialize)
      (let ((first (json-serialize '(:jsonrpc "2.0" :id 10 :method "tools/call"
                                     :params (:name "T" :arguments (:n 1))))))
        (setq trigger (lambda ()
                        (send '(:jsonrpc "2.0" :id 11 :method "tools/call"
                                :params (:name "T" :arguments (:n 2))))))
        (process-send-string client (substring first 0 20))
        (with-timeout (5 (ert-fail "Partial line was not read"))
          (while (not (process-get (car (mevedel-mcp-clients server))
                                   'mevedel-mcp-partial))
            (accept-process-output nil 0.01)))
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
      (with-timeout (5 (ert-fail "Call was not dispatched"))
        (while (not finish) (accept-process-output nil 0.01)))
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

(mevedel-deftest mevedel-mcp-socket-root ()
  ,test
  (test)

  :doc "every server socket shares one owner-only runtime directory"
  (mevedel-mcp-test--with-runtime
    (let ((root (mevedel-mcp-socket-root)))
      (should (file-in-directory-p root runtime))
      (should (= #o700 (file-modes root)))
      (should (equal root (mevedel-mcp-socket-root)))))

  :doc "a planted symlink in place of the directory is refused"
  (mevedel-mcp-test--with-runtime
    (make-symbolic-link runtime (file-name-concat
                                 runtime (format "mevedel-mcp-%d" (user-uid))))
    (should-error (mevedel-mcp-socket-root))))

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
                       (with-timeout (5 (ert-fail "Hook bridge did not return"))
                         (while (process-live-p process) (accept-process-output nil 0.01)))
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
           (with-timeout (5 (ert-fail "Control was not received"))
             (while (not answer) (accept-process-output nil 0.01)))
           (delete-process connection)
           (with-timeout (5 (ert-fail "Control was not cancelled"))
             (while (= 0 cancelled) (accept-process-output nil 0.01)))
           (funcall answer '(:continue t))
           (should (= 1 cancelled)))
       (when (process-live-p connection) (delete-process connection))
       (mevedel-mcp-stop server)))))

(provide 'test-mevedel-mcp)
;;; test-mevedel-mcp.el ends here
