;;; test-mevedel-collaboration.el --- Collaboration transport and projection tests -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name
               load-file-name
               byte-compile-current-file))
          "helpers"))
(require 'cl-lib)
(require 'gptel)
(require 'mevedel-collaboration-projection)
(require 'mevedel-collaboration-transport)
(require 'mevedel-collaboration)
(require 'mevedel-transcript)
(require 'mevedel-chat)
(require 'mevedel-view-composer)
(require 'mevedel-skills-invoke)
(require 'mevedel-skills-ui)


;;
;;; Credentials, origins, and framing

(mevedel-deftest mevedel-collaboration--random-token
  (:doc "creates a URL-safe 256-bit token and fails without OS randomness")
  (let ((token (mevedel-collaboration--random-token)))
    (should (= 43 (length token)))
    (should (string-match-p "\\`[A-Za-z0-9_-]+\\'" token)))
  (cl-letf (((symbol-function 'mevedel-collaboration--random-bytes)
             (lambda (_) (error "Random source unavailable"))))
    (should-error (mevedel-collaboration--random-token))))

(mevedel-deftest mevedel-collaboration--normalize-public-origin
  (:doc "accepts only credential-free HTTPS origins")
  (dolist (case '(("https://collab.example" . t)
                  ("http://collab.example" . nil)
                  ("https://collab.example/path" . nil)
                  ("https://user:pass@collab.example" . nil)
                  ("https://collab.example/?token=secret" . nil)))
    (let ((mevedel-collaboration-public-base-url (car case)))
      (if (cdr case)
          (should (equal (car case)
                         (mevedel-collaboration--normalize-public-origin)))
        (should-error (mevedel-collaboration--normalize-public-origin)
                      :type 'user-error)))))

(mevedel-deftest mevedel-collaboration--auth-boundaries
  (:doc "rejects missing, malformed, stale, and unsupported authentication")
  (let ((guest (list :authenticated nil))
        (room (list :pending-auth nil :room-id "room" :token "token"))
        closed)
    (setf (plist-get room :pending-auth) (list guest))
    (cl-letf (((symbol-function 'process-get) (lambda (&rest _) guest))
              ((symbol-function 'mevedel-collaboration--guest-close)
               (lambda (_guest reason) (setq closed reason))))
      (let ((mevedel-collaboration--room room))
        (dolist (payload '("{}"
                           "not-json"
                           "{\"type\":\"auth\",\"version\":2,\"room\":\"room\",\"token\":\"token\"}"
                           "{\"type\":\"auth\",\"version\":1,\"room\":\"room\",\"token\":\"stale\"}"
                           "{\"type\":\"auth\",\"version\":1,\"room\":\"room\"}"))
          (setq closed nil)
          (mevedel-collaboration--auth-message 'process payload)
          (should (eq 'auth-rejected closed)))
        (setq closed nil)
        (mevedel-collaboration--auth-message
         'process (string-as-unibyte "\377"))
        (should (eq 'auth-rejected closed))))))

(mevedel-deftest mevedel-collaboration--auth-timeout
  (:doc "closes an unauthenticated guest after the authentication deadline")
  (let ((closed nil)
        (guest (list :authenticated nil)))
    (cl-letf (((symbol-function 'process-get) (lambda (&rest _) guest))
              ((symbol-function 'mevedel-collaboration--guest-close)
               (lambda (_guest reason) (setq closed reason))))
      (mevedel-collaboration--auth-timeout 'process)
      (should (eq 'auth-timeout closed)))))

(mevedel-deftest mevedel-collaboration--valid-websocket-key-p
  (:doc "requires a canonical base64 WebSocket key containing 16 bytes")
  (let ((key (base64-encode-string (make-string 16 7) t)))
    (should (mevedel-collaboration--valid-websocket-key-p key))
    (should-not (mevedel-collaboration--valid-websocket-key-p
                 (base64-encode-string (make-string 15 7) t)))
    (should-not (mevedel-collaboration--valid-websocket-key-p
                 (concat key "=")))
    (should-not (mevedel-collaboration--valid-websocket-key-p "not-base64"))))

(mevedel-deftest mevedel-collaboration--frame-info
  (:doc "buffers partial frames and rejects unsafe frame shapes and bounds")
  (progn
    (let ((partial (unibyte-string #x81 #x82 1 2)))
      (should (eq 'incomplete
                  (plist-get (mevedel-collaboration--frame-info partial)
                             :state))))
    (should (eq 'invalid
                (plist-get
                 (mevedel-collaboration--frame-info
                  (unibyte-string #x01 0))
                 :state)))
    (let ((header (unibyte-string #x81 #xff
                                  1 0 0 0 0 0 0 0 0 0 0 0))
          (info nil))
      (setq info (mevedel-collaboration--frame-info header))
      (should (eq 'too-large (plist-get info :state))))))

(mevedel-deftest mevedel-collaboration--close-code
  (:doc "keeps slow-reader disposal reconnectable while auth rejection is terminal")
  (progn
    (should (= 1013 (mevedel-collaboration--close-code 'guest-too-slow)))
    (should (= 1008 (mevedel-collaboration--close-code 'auth-rejected)))
    (should (= 1008 (mevedel-collaboration--close-code 'inbound-after-auth)))))

(mevedel-deftest mevedel-collaboration--guest-close
  (:doc "owns deferred close teardown and cancels a prior close on repetition")
  (let ((guest (list :process 'process :close-timer nil))
        (room nil) (timers nil) (cancelled nil) (deleted nil)
        (close-callback nil) (close-args nil) (send-count 0))
    (setq room (list :guest guest))
    (let ((mevedel-collaboration--room room))
      (cl-letf (((symbol-function 'process-live-p) (lambda (_) t))
                ((symbol-function 'process-send-string)
                 (lambda (&rest _) (cl-incf send-count)))
                ((symbol-function 'set-process-sentinel) (lambda (&rest _) nil))
                ((symbol-function 'cancel-timer)
                 (lambda (timer) (push timer cancelled)))
                ((symbol-function 'delete-process)
                 (lambda (process) (push process deleted)))
                ((symbol-function 'accept-process-output)
                 (lambda (&rest _) (error "Close teardown must not wait")))
                ((symbol-function 'run-at-time)
                 (lambda (_delay _repeat callback &rest args)
                   (setq close-callback callback
                         close-args args)
                   (let ((timer (intern (format "timer-%d" (1+ (length timers))))))
                     (push timer timers)
                     timer))))
        (mevedel-collaboration--guest-close guest 'guest-too-slow)
        (let ((first-timer (plist-get guest :close-timer)))
          (should (eq first-timer (car timers)))
          (mevedel-collaboration--guest-close guest 'guest-too-slow)
          (should (memq first-timer cancelled))
          (should (= 2 send-count))
          (should (equal (list 'process guest) close-args))
          (apply close-callback close-args)
          (should-not (plist-get guest :close-timer))
          (should (equal '(process) deleted)))))))

(mevedel-deftest mevedel-collaboration--bounded-http-errors
  (:doc "bounds HTTP error bodies and emits an exact content length")
  (let (headers body)
    (cl-letf (((symbol-function 'ws-response-header)
               (lambda (_process code &rest values)
                 (setq headers (cons code values))))
              ((symbol-function 'process-send-string)
               (lambda (_process value) (setq body value))))
      (mevedel-collaboration--send-http
       'process 400 "text/plain; charset=utf-8"
       (make-string (* 2 1024 1024) ?x)))
    (let ((length-header (cdr (assoc "Content-Length" (cdr headers)))))
      (should (= 400 (car headers)))
      (should (stringp body))
      (should (<= (string-bytes body) mevedel-collaboration--max-message-bytes))
      (should (= (string-bytes body) length-header)))))

(mevedel-deftest mevedel-collaboration--preupgrade-filter
  (:doc "counts bounded headers and releases them only on a real upgrade")
  (let ((process (start-process "mevedel-preupgrade-test" nil "cat"))
        (delegated nil)
        (removed nil))
    (unwind-protect
        (cl-letf (((symbol-function 'ws-filter)
                   (lambda (_process string) (push string delegated)))
                  ((symbol-function 'mevedel-collaboration--preupgrade-remove)
                   (lambda (value &optional _preserve)
                     (setq removed value))))
          (process-put process :mevedel-collaboration-preupgrade-bytes 0)
          (mevedel-collaboration--preupgrade-filter
           process "GET / HTTP/1.1\r\nHost: x\r\n\r")
          (should-not removed)
          ;; End of headers keeps the bounds: only web-server replacing the
          ;; process plist with its message object means an upgrade.
          (mevedel-collaboration--preupgrade-filter process "\n")
          (should-not removed)
          (set-process-plist process (list :message 'websocket-message))
          (mevedel-collaboration--preupgrade-filter process "frame")
          (should (eq process removed))
          (should (equal (list "frame" "\n" "GET / HTTP/1.1\r\nHost: x\r\n\r")
                         delegated)))
      (dolist (slot '(:mevedel-collaboration-preupgrade-idle
                      :mevedel-collaboration-preupgrade-total))
        (when-let ((timer (process-get process slot)))
          (cancel-timer timer)))
      (remhash process mevedel-collaboration--preupgrade-state)
      (delete-process process))))

(mevedel-deftest mevedel-collaboration--preupgrade-stop
  (:doc "keeps every unupgraded connection tracked, bounded, and closable")
  (progn
    (skip-unless (not (eq system-type 'windows-nt)))
    (require 'web-server)
    (let* ((server (mevedel-collaboration--ws-start
                  #'mevedel-collaboration--request-handler t))
         (listener (mevedel-collaboration--web-server-slot server 'process))
         (port (process-contact listener :service))
         (clients nil)
         (mevedel-collaboration--room (list :server server)))
    (cl-flet ((connect ()
                (let ((client
                       (make-network-process
                        :name "mevedel-preupgrade-client"
                        :host "127.0.0.1" :service port
                        :coding 'binary :noquery t)))
                  (push client clients)
                  client)))
      (unwind-protect
          (progn
            ;; End of headers is not an upgrade: a multipart request keeps
            ;; web-server parsing, so the connection must stay bounded.
            (let ((client (connect)))
              (process-send-string
               client
               (concat "POST / HTTP/1.1\r\nHost: x\r\n"
                       "Content-Type: multipart/form-data; boundary=zz\r\n"
                       "\r\n"))
              (accept-process-output client 0.2)
              (let ((children (process-get
                               listener
                               :mevedel-collaboration-preupgrade-children)))
                (should (= 1 (length children)))
                (should (gethash (car children)
                                 mevedel-collaboration--preupgrade-state))))
            ;; The bounds stay armed after a completed header block that is
            ;; not an upgrade.
            (let* ((children (process-get
                              listener
                              :mevedel-collaboration-preupgrade-children))
                   (state (gethash (car children)
                                   mevedel-collaboration--preupgrade-state)))
              (should (timerp (plist-get state :idle)))
              (should (timerp (plist-get state :total)))
              (should (> (plist-get state :bytes) 0)))
            ;; A connection past the byte bound is closed at once, and one
            ;; that stops mid-headers is closed when its idle deadline
            ;; passes.
            (let ((mevedel-collaboration--max-preupgrade-bytes 4096)
                  (mevedel-collaboration--preupgrade-idle-timeout 0.2)
                  (client (connect)))
              (process-send-string
               client (concat "GET / HTTP/1.1\r\nX-Big: "
                              (make-string 8192 ?x) "\r\n"))
              (accept-process-output client 0.3)
              (should-not (process-live-p client)))
            (let ((mevedel-collaboration--preupgrade-idle-timeout 0.2)
                  (client (connect)))
              (process-send-string client "GET / HTTP/1.1\r\n")
              (with-timeout (3 (ert-fail "Idle connection was not closed"))
                (while (process-live-p client)
                  (accept-process-output client 0.05)))
              (should-not (process-live-p client)))
            ;; A valid route still answers.
            (let ((client (connect)))
              (process-send-string
               client "GET /index.html HTTP/1.1\r\nHost: x\r\n\r\n")
              (with-timeout (3 (ert-fail "Viewer route did not answer"))
                (while (and (process-live-p client)
                            (with-current-buffer
                                (or (process-buffer client)
                                    (set-process-buffer
                                     client (generate-new-buffer " *route*")))
                              (= (point-max) (point-min))))
                  (accept-process-output client 0.05)))
              (when-let ((buffer (process-buffer client)))
                (with-current-buffer buffer
                  (should (string-match-p "200 OK" (buffer-string))))
                (kill-buffer buffer)))
            ;; Held connections are capped rather than accumulated.
            (dotimes (_ (+ mevedel-collaboration--max-preupgrade-children 4))
              (let ((client (connect)))
                (process-send-string client "GET / HTTP/1.1\r\n")
                (accept-process-output client 0.05)))
            (should (<= (length (process-get
                                 listener
                                 :mevedel-collaboration-preupgrade-children))
                        mevedel-collaboration--max-preupgrade-children))
            ;; Stopping the room leaves no tracked child, no timer, and no
            ;; surviving connection.
            (mevedel-collaboration--preupgrade-stop server)
            (ws-stop server)
            (dolist (client clients)
              (accept-process-output client 0.2))
            (should-not (process-get
                         listener
                         :mevedel-collaboration-preupgrade-children))
            (should (zerop (hash-table-count
                            mevedel-collaboration--preupgrade-state)))
            (should-not (cl-find-if #'process-live-p clients)))
        (dolist (client clients)
          (when (process-live-p client)
            (delete-process client)))
        (condition-case nil (ws-stop server) (error nil)))))))


;;
;;; Canonical projection and bounded output

(mevedel-deftest mevedel-collaboration--canonical-records
  (:doc "uses allowlisted canonical text, stable identities, and revision zero")
  (with-temp-buffer
    (insert "prompt\nanswer\n")
    (let ((data-buffer (current-buffer)))
      (cl-letf (((symbol-function 'mevedel-transcript-segments)
                 (lambda (_start _end)
                   (list (list 'user 1 4) (list 'response 4 11))))
                ((symbol-function 'mevedel-view--user-turn-text)
                 (lambda (_segments _buffer) "visible prompt"))
                ((symbol-function 'mevedel-view--visible-response-text)
                 (lambda (_text) "visible answer")))
        (let ((records (mevedel-collaboration--canonical-records data-buffer)))
          (should (= 2 (length records)))
          (should (equal '(0 0) (mapcar (lambda (r) (plist-get r :revision))
                                        records)))
          (should-not (string-match-p "\\`user-[0-9]+\\'"
                                      (plist-get (car records) :id))))))))

(mevedel-deftest mevedel-collaboration--allowlist
  (:doc "serializes only visible canonical fields from adversarial records")
  (let* ((records
          (list (list :id "user" :kind "user" :revision 0 :text "visible"
                      :hidden "secret" :raw-html "<script>secret</script>"
                      :prompt "pending" :media "/secret.png")
                (list :id "assistant" :kind "assistant" :revision 1
                      :text "answer" :render-data "hidden")
                (list :id "tool" :kind "tool" :revision 0 :name "Bash"
                      :status "completed" :summary "Bash" :result "done"
                      :permission "secret" :execution-target "/target")))
         (payload (mapcar #'mevedel-collaboration--json-record records)))
    (should (equal '(("id" . "user") ("kind" . "user")
                     ("revision" . 0) ("text" . "visible"))
                   (car payload)))
    (should (equal '("id" "kind" "revision" "text")
                   (mapcar #'car (car payload))))
    (should (equal '("id" "kind" "revision" "text")
                   (mapcar #'car (cadr payload))))
    (should (equal '("id" "kind" "revision" "name" "status" "summary"
                     "result")
                   (mapcar #'car (caddr payload))))
    (should-not (string-match-p "secret\\|script\\|target"
                                (mevedel-collaboration--json-string payload)))))

(mevedel-deftest mevedel-collaboration--canonical-excluded-spans
  (:doc "projects visible payload exactly while excluding control and unknown spans")
  (with-temp-buffer
    (let ((visible-user "Visible user [bad](javascript:alert(1)) <img src=secret.png>\n")
          (visible-answer "Visible answer with <b>literal</b> text\n"))
      (insert visible-user)
      (let ((response-start (point)))
        (insert visible-answer)
        (put-text-property response-start (point) 'gptel 'response))
      (dolist (block '("#+begin_reasoning\nreason secret\n#+end_reasoning\n"
                       "<system-reminder>\nreminder secret\n</system-reminder>\n"
                       "<hook-context>\nhook secret\n</hook-context>\n"
                       "<!-- mevedel-render-data -->\n(:kind media :path \"/secret.png\")\n<!-- /mevedel-render-data -->\n"
                       "<!-- mevedel-hook-audit -->\nunknown secret\n<!-- /mevedel-hook-audit -->\n"))
        (let ((start (point)))
          (insert block)
          (put-text-property start (point) 'gptel 'ignore)))
      (let ((unknown-start (point)))
        (insert "Unknown kind payload must stay hidden\n")
        (put-text-property unknown-start (point) 'gptel 'ignore))
      (let ((tool-start (point)))
        (insert "#+begin_tool (Read :file_path \"/tmp/visible\")\n"
                "(:name \"Read\" :args (:file_path \"/tmp/visible\"))\n\n"
                "Visible tool result\n"
                "#+end_tool\n")
        (put-text-property tool-start (point) 'gptel '(tool . "visible-tool")))
      (let* ((records (mevedel-collaboration--canonical-records (current-buffer)))
             (payload (mapcar #'mevedel-collaboration--json-record records)))
        (should (= 3 (length records)))
        (should (equal (string-trim visible-user)
                       (cdr (assoc "text" (car payload)))))
        (should (equal (string-trim visible-answer)
                       (cdr (assoc "text" (cadr payload)))))
        (should (equal "Visible tool result"
                       (cdr (assoc "result" (nth 2 payload)))))
        (let ((json (mevedel-collaboration--json-string payload)))
          (dolist (hidden '("reason secret" "reminder secret" "hook secret"
                            "/secret.png" "unknown secret"
                            "Unknown kind payload"))
            (should-not (string-match-p (regexp-quote hidden) json))))))))

(mevedel-deftest mevedel-collaboration--snapshot-refusal
  (:doc "refuses an initial snapshot larger than eight MiB")
  (with-temp-buffer
    (cl-letf (((symbol-function 'mevedel-collaboration--canonical-records)
               (lambda (_) (list (list :id "huge" :kind "assistant"
                                        :revision 0
                                        :text (make-string (* 8 1024 1024) ?x))))))
      (should-error (mevedel-collaboration--start nil (current-buffer))
                    :type 'user-error))))

(mevedel-deftest mevedel-collaboration--local-remote-parity
  (:doc "keeps local and remote-backed projections semantically identical")
  (with-temp-buffer
    (insert "Prompt from the local session\n")
    (let ((response-start (point)))
      (insert "Answer from the shared session\n")
      (put-text-property response-start (point) 'gptel 'response))
    (let ((tool-start (point)))
      (insert "#+begin_tool (Bash :command \"true\")\n"
              "(:name \"Bash\" :args (:command \"true\"))\n\n"
              "done\n#+end_tool\n")
      (put-text-property tool-start (point) 'gptel '(tool . "parity-tool")))
    ;; Both rooms project the same data buffer, so equality between them
    ;; would be vacuous.  What matters is that nothing target-specific
    ;; reaches the wire at all.
    (let* ((remote-room (list :data-buffer (current-buffer)
                              :session 'remote-session
                              :execution-target "/ssh:someone@example:/srv/"))
           (wire (mevedel-collaboration--json-string
                  (mapcar #'mevedel-collaboration--json-record
                          (mevedel-collaboration--project-records
                           remote-room)))))
      (should (string-match-p "Answer from the shared session" wire))
      (should-not (string-match-p "execution-target\\|/ssh:\\|example" wire)))))

(mevedel-deftest mevedel-collaboration--tool-record
  (:doc "marks empty settled tool output completed rather than running")
  (with-temp-buffer
    (insert "tool")
    (cl-letf (((symbol-function 'mevedel-view--tool-call-parse)
               (lambda (_buffer _start _end)
                 '(:name "Bash" :result ""))))
      (let ((record (mevedel-collaboration--tool-record
                     (current-buffer) '(tool 1 5))))
        (should (equal "completed" (plist-get record :status)))
        (should (equal "" (plist-get record :result)))))))

(mevedel-deftest mevedel-collaboration--pre-tool
  (:doc "publishes one stable running tool record before settled completion")
  (with-temp-buffer
    (let* ((room (list :data-buffer (current-buffer)
                       :records nil :pending-tools nil
                       :tool-call-occurrences (make-hash-table :test #'equal)
                       :guest nil))
           (info '(:name "Bash" :args (:command "true")))
           (mevedel-collaboration--room room)
           canonical)
      (cl-letf (((symbol-function 'mevedel-collaboration--canonical-records)
                 (lambda (_) canonical)))
        (should-not (mevedel-collaboration--pre-tool info))
        (let* ((pending (plist-get room :pending-tools))
               (entry (car pending))
               (id (plist-get entry :id)))
          (should (= 1 (length pending)))
          (should (equal "running" (plist-get entry :status)))
          (should (stringp id))
          (should-not (mevedel-collaboration--pre-tool info))
          (should (= 1 (length (plist-get room :pending-tools))))
          (setq canonical
                (list (list :id "canonical" :kind "tool" :revision 0
                            :name "Bash" :status "completed"
                            :summary "Bash" :result "" :truncated nil)))
          (should-not (mevedel-collaboration--post-tool
                       '(:name "Bash" :args (:command "true") :result "")))
          (setq entry (car (plist-get room :records)))
          (should (equal id (plist-get entry :id)))
          (should (equal "completed" (plist-get entry :status)))
          (should (equal "" (plist-get entry :result)))
          (should-not (plist-get room :pending-tools)))))))

(mevedel-deftest mevedel-collaboration--clean-response
  (:doc "fails closed when canonical response projection is unavailable")
  (cl-letf (((symbol-function 'mevedel-view--visible-response-text)
             (lambda (_) (error "Projection failure"))))
    (should-error (mevedel-collaboration--clean-response "hidden"))))

(mevedel-deftest mevedel-collaboration--clean-user
  (:doc "fails closed when canonical user projection is unavailable")
  (with-temp-buffer
    (cl-letf (((symbol-function 'mevedel-view--user-turn-text)
               (lambda (_segments _buffer) nil)))
      (should-error
       (mevedel-collaboration--clean-user '(user 1 2) (current-buffer))))))

(mevedel-deftest mevedel-collaboration--snapshot-chunks
  (:doc "keeps each UTF-8 snapshot message below the frame bound")
  (progn
    (let* ((record (list :id "assistant-x" :kind "assistant" :revision 0
                         :text (make-string 100000 937)))
           (chunks (cdr (mevedel-collaboration--snapshot-chunks (list record)))))
      (should (= 1 (length chunks)))
      (let ((guest (list :next-sequence 0)))
        (dolist (chunk chunks)
          (should
           (<= (string-bytes
                (plist-get
                 (mevedel-collaboration--guest-json-with-sequence guest chunk)
                 :json))
               mevedel-collaboration--max-message-bytes)))))
    (let ((record (list :id "assistant-x" :kind "assistant" :revision 0
                        :text (make-string mevedel-collaboration--max-message-bytes
                                           ?x))))
      (should-error (mevedel-collaboration--snapshot-chunks (list record))
                    :type 'user-error))))

(mevedel-deftest mevedel-collaboration--guest-enqueue
  (:doc "accounts framed UTF-8 bytes before admission and rejects a full queue")
  (progn
    (let ((guest (list :process 'process :pending-bytes 0 :queue nil
                       :in-flight nil :pump-timer nil)))
      (cl-letf (((symbol-function 'process-live-p) (lambda (_) t))
                ((symbol-function 'process-put) (lambda (&rest _) nil))
                ((symbol-function 'run-at-time) (lambda (&rest _) 'timer))
                ((symbol-function 'ws-web-socket-frame) (lambda (wire) wire)))
        (should (mevedel-collaboration--guest-enqueue guest "Grüße"))
        (should (= (string-bytes (encode-coding-string "Grüße" 'utf-8 t))
                   (plist-get (car (plist-get guest :queue)) :bytes)))))
    (let ((closed nil)
          (guest (list :process 'process
                       :pending-bytes mevedel-collaboration--max-pending-bytes
                       :queue nil)))
      (cl-letf (((symbol-function 'mevedel-collaboration--guest-close)
                 (lambda (_guest reason) (setq closed reason)))
                ((symbol-function 'process-live-p) (lambda (_) t))
                ((symbol-function 'ws-web-socket-frame) (lambda (wire) wire)))
        (should-not (mevedel-collaboration--guest-enqueue guest "x"))
        (should (eq 'guest-too-slow closed))))))

(mevedel-deftest mevedel-collaboration--guest-send
  (:doc "coalesces only unsent record revisions and preserves queue order")
  (let ((guest (list :process 'process :pending-bytes 0 :queue nil
                     :in-flight nil :pump-timer nil))
        (first `(("type" . "record")
                 ("version" . 1)
                 ("record" . (("id" . "assistant-1") ("revision" . 1)))))
        (replacement `(("type" . "record")
                       ("version" . 1)
                       ("record" . (("id" . "assistant-1") ("revision" . 2)))))
        (second `(("type" . "record")
                  ("version" . 1)
                  ("record" . (("id" . "assistant-2") ("revision" . 1))))))
    (cl-letf (((symbol-function 'process-live-p) (lambda (_) t))
              ((symbol-function 'process-put) (lambda (&rest _) nil))
              ((symbol-function 'run-at-time) (lambda (&rest _) 'timer))
              ((symbol-function 'ws-web-socket-frame) (lambda (wire) wire)))
      (mevedel-collaboration--guest-send guest first)
      (mevedel-collaboration--guest-send guest replacement)
      (mevedel-collaboration--guest-send guest second)
      (let ((queue (plist-get guest :queue)))
        (should (= 2 (length queue)))
        (should (string-match-p "assistant-1" (plist-get (car queue) :frame)))
        (should (string-match-p "\\\"revision\\\":2"
                                (plist-get (car queue) :frame)))
        (should (string-match-p "assistant-2"
                                (plist-get (cadr queue) :frame)))
        (should (string-match-p "\\\"ack-token\\\":\\\"[A-Za-z0-9_-]+"
                                (plist-get (car queue) :frame)))
        (should (stringp (plist-get (car queue) :ack-token)))
        (should (= (+ (plist-get (car queue) :bytes)
                      (plist-get (cadr queue) :bytes))
                   (plist-get guest :pending-bytes))))))
  (let ((guest (list :process 'process :pending-bytes 0 :queue nil
                     :in-flight nil :pump-timer nil
                     :snapshot-active t :after-snapshot nil
                     :after-snapshot-bytes 0))
        (first `(("type" . "record")
                 ("version" . 1)
                 ("record" . (("id" . "assistant-1") ("revision" . 1)))))
        (replacement `(("type" . "record")
                       ("version" . 1)
                       ("record" . (("id" . "assistant-1") ("revision" . 2))))))
    (cl-letf (((symbol-function 'process-live-p) (lambda (_) t))
              ((symbol-function 'process-put) (lambda (&rest _) nil))
              ((symbol-function 'run-at-time) (lambda (&rest _) 'timer))
              ((symbol-function 'ws-web-socket-frame) (lambda (wire) wire)))
      (mevedel-collaboration--guest-send guest first)
      (mevedel-collaboration--guest-send guest replacement)
      (should (= 1 (length (plist-get guest :after-snapshot))))
        (should (string-match-p "\\\"revision\\\":2"
                              (plist-get (car (plist-get guest :after-snapshot))
                                         :json))))))

(mevedel-deftest mevedel-collaboration--ack-message
  (:doc "requires the exact per-frame token and sequence before advancing output")
  (let ((guest (list :process 'process
                     :in-flight (list :sequence 7 :ack-token "expected")
                     :acknowledged-sequence nil
                     :acknowledged-token nil
                     :pump-timer 'timer))
        (scheduled nil)
        (closed nil))
    (cl-letf (((symbol-function 'process-get)
               (lambda (&rest _) guest))
              ((symbol-function 'process-put) (lambda (&rest _) nil))
              ((symbol-function 'cancel-timer) (lambda (_) nil))
              ((symbol-function 'run-at-time)
               (lambda (&rest _) (setq scheduled t)))
              ((symbol-function 'mevedel-collaboration--guest-close)
               (lambda (_guest reason) (setq closed reason))))
      (mevedel-collaboration--ack-message
       'process "{\"type\":\"ack\",\"seq\":7,\"ack-token\":\"expected\"}")
      (should (= 7 (plist-get guest :acknowledged-sequence)))
      (should (equal "expected" (plist-get guest :acknowledged-token)))
      (should scheduled)
      (should-not closed)
      (setq scheduled nil)
      (mevedel-collaboration--ack-message
       'process "{\"type\":\"ack\",\"seq\":7,\"ack-token\":\"forged\"}")
      (should (eq 'inbound-after-auth closed))
      (should-not scheduled)
      (setq closed nil)
      (mevedel-collaboration--ack-message
       'process "{\"type\":\"ack\",\"seq\":7}")
      (should (eq 'inbound-after-auth closed)))))

(mevedel-deftest mevedel-collaboration--after-snapshot-bound
  (:doc "counts deferred live frames against the shared pending output bound")
  (let ((closed nil)
        (guest (list :process 'process :pending-bytes 0
                     :after-snapshot-bytes 0 :after-snapshot nil
                     :snapshot-active t)))
    (cl-letf (((symbol-function 'ws-web-socket-frame)
               (lambda (wire) wire))
              ((symbol-function 'mevedel-collaboration--guest-close)
               (lambda (_guest reason) (setq closed reason))))
      (dotimes (_ 2)
        (should
         (mevedel-collaboration--guest-send
          guest `(("type" . "update")
                  ("payload" . ,(make-string 800000 ?x))))))
      (should (> (plist-get guest :after-snapshot-bytes) 0))
      (should-not
       (mevedel-collaboration--guest-send
        guest `(("type" . "update")
                ("payload" . ,(make-string 800000 ?x)))))
      (should (eq 'guest-too-slow closed)))))

(mevedel-deftest mevedel-collaboration--publish
  (:doc "publishes simultaneous changes in canonical order with incremented revisions")
  (let* ((old (list (list :id "a" :kind "assistant" :revision 0 :text "one")
                    (list :id "b" :kind "assistant" :revision 0 :text "two")))
         (new (list (list :id "a" :kind "assistant" :revision 0 :text "ONE")
                    (list :id "b" :kind "assistant" :revision 0 :text "TWO")))
         (sent nil)
         (guest (list :authenticated t))
         (room (list :records old :guest guest)))
    (let ((mevedel-collaboration--room room))
      (cl-letf (((symbol-function 'mevedel-collaboration--project-records)
                 (lambda (_) new))
                ((symbol-function 'mevedel-collaboration--guest-send)
                 (lambda (_guest object) (push object sent))))
        (mevedel-collaboration--publish room)))
    (setq sent (nreverse sent))
    (should (equal '("a" "b")
                   (mapcar (lambda (object)
                             (cdr (assoc "id" (cdr (assoc "record" object)))))
                           sent)))
    (should (equal '(1 1)
                   (mapcar (lambda (object)
                             (cdr (assoc "revision" (cdr (assoc "record" object)))))
                           sent)))))

(mevedel-deftest mevedel-collaboration--safe-accepted-prompt
  (:doc "publishes accepted prompt insertion and isolates observer failure")
  (with-temp-buffer
    (let ((room (list :data-buffer (current-buffer)))
          published stopped)
      (let ((mevedel-collaboration--room room))
        (cl-letf (((symbol-function 'mevedel-collaboration--publish)
                   (lambda (_room) (setq published t))))
          (should-not (mevedel-collaboration--safe-accepted-prompt
                       (current-buffer)))
          (should published)))
      (let ((mevedel-collaboration--room room))
        (cl-letf (((symbol-function 'mevedel-collaboration--publish)
                   (lambda (_) (error "viewer failed")))
                  ((symbol-function 'mevedel-collaboration--stop-internal)
                   (lambda (reason) (setq stopped reason)))
                  ((symbol-function 'display-warning) (lambda (&rest _) nil)))
          (should-not (mevedel-collaboration--safe-accepted-prompt
                       (current-buffer)))
          (should (eq 'observer-failure stopped)))))))

(mevedel-deftest mevedel-collaboration--accepted-prompt-insertion-seams
  (:doc "publishes ordinary composer and generated turns at insertion")
  (let ((data-buffer (generate-new-buffer " *collaboration-prompt-seams*"))
        ordinary generated)
    (unwind-protect
        (progn
          (with-current-buffer data-buffer
            (setq-local gptel-response-separator "\n\n")
            (setq-local gptel-prompt-prefix-alist nil)
            (setq-local mevedel--compaction-in-flight nil))
          (with-temp-buffer
            (let ((mevedel--data-buffer data-buffer))
              (cl-letf (((symbol-function 'mevedel-view--ensure-interactive-chat-view)
                         (lambda () nil))
                        ((symbol-function 'mevedel-view--session)
                         (lambda () 'session))
                        ((symbol-function 'mevedel-request-assert-target-ready)
                         (lambda (&rest _) nil))
                        ((symbol-function 'mevedel-view--pop-dropped-file-grants-for-input)
                         (lambda (&rest _) nil))
                        ((symbol-function 'mevedel--normalize-message-text)
                         (lambda (text) text))
                        ((symbol-function 'mevedel-view--inline-skill-prompt-summary-body)
                         (lambda (_text) nil))
                        ((symbol-function 'mevedel--clear-user-turn-gptel-properties)
                         (lambda (&rest _) nil))
                        ((symbol-function 'mevedel-collaboration--safe-accepted-prompt)
                         (lambda (buffer) (setq ordinary (eq buffer data-buffer))))
                        ((symbol-function 'mevedel-view--insert-user-message)
                         (lambda (&rest _) 1))
                        ((symbol-function 'mevedel-view-stream-begin-turn)
                         (lambda (&rest _) nil))
                        ((symbol-function 'mevedel-view--clear-input)
                         (lambda () nil))
                        ((symbol-function 'mevedel-view--activate-dropped-file-grants)
                         (lambda (&rest _) nil))
                        ((symbol-function 'gptel-send) (lambda () nil)))
                (mevedel-view--forward-input-now "ordinary prompt")))
          (with-current-buffer data-buffer
            (let ((mevedel-collaboration--room
                   (list :data-buffer data-buffer)))
              (cl-letf (((symbol-function 'mevedel-collaboration--safe-accepted-prompt)
                         (lambda (buffer) (setq generated (eq buffer data-buffer))))
                        ((symbol-function 'mevedel-view--begin-external-turn)
                         (lambda (&rest _) nil)))
                (mevedel--insert-local-user-turn "generated prompt"))))
          (should ordinary)
          (should generated)
          (with-current-buffer data-buffer
            (should (string-match-p "ordinary prompt" (buffer-string)))
            (should (string-match-p "generated prompt" (buffer-string)))))
      (when (buffer-live-p data-buffer)
        (kill-buffer data-buffer))))))


;;
;;; Observer and command boundaries

(mevedel-deftest mevedel-collaboration--safe-post-stream
  (:doc "contains observer failures without signaling into the request")
  (let ((stopped nil))
    (cl-letf (((symbol-function 'mevedel-collaboration--post-stream)
               (lambda () (error "Observer failure")))
              ((symbol-function 'mevedel-collaboration--stop-internal)
               (lambda (reason) (setq stopped reason)))
              ((symbol-function 'display-warning) (lambda (&rest _) nil)))
      (should-not (mevedel-collaboration--safe-post-stream))
      (should (eq 'observer-failure stopped)))))

(mevedel-deftest mevedel-collaboration-status
  (:doc "reports safe active and inactive status without exposing the token")
  (let ((messages nil)
        (mevedel-collaboration--room
         (list :session-label "share"
               :local-origin "http://127.0.0.1:1234"
               :public-origin "https://collab.example"
               :token "secret-token"
               :guest (list :authenticated t))))
    (cl-letf (((symbol-function 'message)
               (lambda (format-string &rest args)
                 (push (apply #'format format-string args) messages))))
      (mevedel-collaboration-status)
      (should (string-match-p "share" (car messages)))
      (should (string-match-p "viewer authenticated" (car messages)))
      (should-not (string-match-p "secret-token" (car messages)))
      (setq mevedel-collaboration--room nil)
      (mevedel-collaboration-status)
      (should (string-match-p "inactive" (car messages))))))

(mevedel-deftest mevedel-collaboration-status--preserves-composer
  (:doc "preserves a multiline composer draft beginning with >")
  (with-temp-buffer
    (insert "> first line\nsecond line\n> third line")
    (let ((before (buffer-string))
          (mevedel-collaboration--room
           (list :session-label "draft" :local-origin "http://127.0.0.1:1")))
      (cl-letf (((symbol-function 'message) (lambda (&rest _) nil)))
        (mevedel-collaboration-status))
      (should (equal before (buffer-string))))))

(mevedel-deftest mevedel-collaboration-stop
  (:doc "stops an active room through the public command")
  (let ((stopped nil)
        (messages nil)
        (mevedel-collaboration--room (list :server 'server)))
    (cl-letf (((symbol-function 'mevedel-collaboration--stop-internal)
               (lambda (reason) (setq stopped reason)))
              ((symbol-function 'message)
               (lambda (format-string &rest args)
                 (push (apply #'format format-string args) messages))))
      (mevedel-collaboration-stop)
      (should (eq 'user-stop stopped))
      (should (string-match-p "stopped" (car messages)))
      (setq mevedel-collaboration--room nil)
      (mevedel-collaboration-stop)
      (should (string-match-p "not active" (car messages))))))

(mevedel-deftest mevedel-collaboration--lifecycle-hooks
  (:doc "stops on data-buffer, session, and Emacs lifecycle teardown")
  (with-temp-buffer
    (let ((room (list :data-buffer (current-buffer)))
          reasons)
      (let ((mevedel-collaboration--room room))
        (cl-letf (((symbol-function 'mevedel-collaboration--stop-internal)
                   (lambda (reason) (push reason reasons))))
          (mevedel-collaboration--stop-for-buffer)
          (mevedel-collaboration--stop-for-session)
          (mevedel-collaboration--stop-for-emacs))
        (should (equal '(emacs-exit data-buffer-killed data-buffer-killed)
                       reasons))))))

(mevedel-deftest mevedel-collaboration--second-session
  (:doc "rejects a second session while one room is active")
  (let ((mevedel-collaboration--room
         (list :server 'server :session 'first :session-label "first")))
    (should-error (mevedel-collaboration--start 'second (current-buffer))
                  :type 'user-error)))

(mevedel-deftest mevedel-collaboration--ws-sentinel
  (:doc "cancels detached guest timers without attempting a close frame")
  (let* ((guest (list :auth-timer 'auth :pump-timer 'pump
                      :snapshot-timer 'snapshot :receive-timer 'receive
                      :close-timer 'close))
         (cancelled nil)
         (sent nil)
         (room (list :guest guest)))
    (let ((mevedel-collaboration--room room))
      (cl-letf (((symbol-function 'process-get)
                 (lambda (_process _property) guest))
                ((symbol-function 'cancel-timer)
                 (lambda (timer) (push timer cancelled)))
                ((symbol-function 'mevedel-collaboration--send-close-frame)
                 (lambda (&rest _) (setq sent t))))
        (mevedel-collaboration--ws-sentinel 'detached nil)))
    (should (equal '(auth close pump receive snapshot)
                   (sort cancelled (lambda (a b)
                                     (string< (symbol-name a)
                                              (symbol-name b))))))
    (should-not sent)
    (should-not (plist-get guest :pump-timer))
    (should-not (plist-get guest :snapshot-timer))
    (should-not (plist-get guest :receive-timer))
    (should-not (plist-get guest :close-timer))))

(mevedel-deftest mevedel-collaboration-view
  (:doc "discloses secrets, link scope, and tunnel plaintext before starting")
  (let ((session (mevedel-session--create :name "share"))
        (data-buffer (generate-new-buffer " *collaboration-disclosure*"))
        prompts)
    (unwind-protect
        (progn
          (with-current-buffer data-buffer
            (setq-local mevedel--session session))
          (cl-letf (((symbol-function
                      'mevedel-collaboration--current-data-buffer)
                     (lambda () data-buffer))
                    ((symbol-function 'yes-or-no-p)
                     (lambda (prompt)
                       (push prompt prompts)
                       nil)))
            (let ((mevedel-collaboration-public-base-url nil))
              (should-error (mevedel-collaboration-view) :type 'user-error))
            (let ((mevedel-collaboration-public-base-url
                   "https://collab.example"))
              (should-error (mevedel-collaboration-view) :type 'user-error)))
          (should (string-match-p "credentials or secrets" (cadr prompts)))
          (should (string-match-p "local-only" (cadr prompts)))
          (should (string-match-p "plaintext local hop" (car prompts))))
      (when (buffer-live-p data-buffer)
        (kill-buffer data-buffer)))))

(mevedel-deftest mevedel-collaboration--stop-for-session
  (:doc "stops the room when its owning data buffer ends the session")
  (with-temp-buffer
    (let ((stopped nil)
          (mevedel-collaboration--room
           (list :data-buffer (current-buffer))))
      (cl-letf (((symbol-function 'mevedel-collaboration--stop-internal)
                 (lambda (reason) (setq stopped reason))))
        (mevedel-collaboration--stop-for-session)
        (should (eq 'data-buffer-killed stopped))))))

(mevedel-deftest mevedel-cmd--collab
  (:doc "does not return a bearer URL to slash dispatch")
  (cl-letf (((symbol-function 'mevedel-collaboration-view)
             (lambda () "http://127.0.0.1:1/index.html#room.secret")))
    (should-not (mevedel-cmd--collab "view"))))

(mevedel-deftest mevedel-skills--dispatch-slash-command
  (:doc "dispatches /collab without copying its bearer URL into messages")
  (with-temp-buffer
    (let ((gptel-prompt-prefix-alist '((fundamental-mode . "### ")))
          (messages nil))
      (insert "### /collab view")
      (cl-letf (((symbol-function 'mevedel-collaboration-view)
                 (lambda () "http://127.0.0.1:1/index.html#room.secret"))
                ((symbol-function 'message)
                 (lambda (format-string &rest args)
                   (push (apply #'format format-string args) messages))))
        (should (eq 'local (mevedel-skills--dispatch-slash-command)))
        (should-not (seq-some (lambda (message)
                                (string-match-p "room\\.secret" message))
                              messages))))))

(mevedel-deftest mevedel-skills-local-command-active-request-p
  (:doc "allows collaboration safety commands while a request is active")
  (progn
    (should (mevedel-skills-local-command-active-request-p "collab" "status"))
    (should (mevedel-skills-local-command-active-request-p "collab" "stop"))))

(mevedel-deftest mevedel-collaboration--read-asset
  (:doc "serves packaged viewer assets and the security disclosure")
  (progn
    (dolist (name '("index.html" "viewer.css" "viewer.js"))
      (let ((asset (mevedel-collaboration--read-asset name)))
        (should (stringp asset))
        (should-not (multibyte-string-p asset))
        (should (= (string-bytes asset)
                   (file-attribute-size
                    (file-attributes
                     (mevedel-collaboration--asset-path name)))))
        (should (> (length asset) 0))))
    (should (string-match-p "bearer" (mevedel-collaboration--read-asset "index.html")))))

;;; test-mevedel-collaboration.el ends here
