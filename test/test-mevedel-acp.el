;;; test-mevedel-acp.el --- ACP conversation lifecycle -*- lexical-binding: t -*-

;;; Commentary:
;; Drive a deterministic subprocess through acp.el and the public lifecycle.

;;; Code:

(require 'mevedel-acp)
(require 'helpers
         (file-name-concat
          (file-name-directory (or load-file-name buffer-file-name)) "helpers"))

(defvar mevedel-acp-test--launch-options nil
  "Additional launch settings captured by the connection fixture.")

(defmacro mevedel-acp-test--with-connection (session-id &rest body)
  "Run BODY with an ACP peer, optionally resuming SESSION-ID."
  (declare (indent 1) (debug t))
  `(let* ((directory (make-temp-file "mevedel-acp-test-" t))
          ready failure
          (connection
           (mevedel-acp-open
            (append (list :command (executable-find "python3")
                          :args (list mevedel-test--acp-peer)
                          :cwd directory :session-id ,session-id)
                    mevedel-acp-test--launch-options)
            (lambda (_connection) (setq ready t))
            (lambda (error) (setq failure error))))
          (startup-timer (mevedel-acp-timer connection)))
     (unwind-protect
         (cl-labels ((await (predicate)
                       (mevedel-test--await 5 "ACP peer did not respond" (funcall predicate))))
           (await (lambda () (or ready failure)))
           ,@body)
       (mevedel-acp-close connection)
       (delete-directory directory t))))

(defvar mevedel-gptel-stream-bridge-insert-batch-delay)

(mevedel-deftest mevedel-acp--pace ()
  ,test
  (test)
  :doc "pauses a turn's agent after small reads and wakes it before messages"
  (skip-unless (and (executable-find "sleep")
                    (not (memq system-type '(windows-nt ms-dos)))))
  (let* ((mevedel-gptel-stream-bridge-insert-batch-delay 30)
         (mevedel-transport--paused nil)
         (kill-emacs-hook nil)
         (process (make-process :name "mevedel-test-acp-pace" :command '("sleep" "30")
                                :connection-type 'pipe :noquery t))
         (connection (mevedel-acp--create :client (list (cons :process process))
                                          :state 'prompting))
         ;; Claude's adapter sends each delta as an ACP chunk and an SDK event.
         (text (concat "{\"method\":\"session/update\",\"params\":{\"update\":"
                       "{\"sessionUpdate\":\"agent_message_chunk\"}}}\n"
                       "{\"method\":\"_claude/sdkMessage\",\"params\":{\"message\":"
                       "{\"type\":\"stream_event\",\"event\":{\"type\":"
                       "\"content_block_delta\"}}}}\n")))
    (unwind-protect
        (progn
          (mevedel-acp--pace connection process (make-string 4096 ?x))
          (should-not mevedel-transport--paused)
          ;; Receipts, tool calls and replies must not wait behind a pause.
          (mevedel-acp--pace connection process
                             "{\"method\":\"_claude/sdkMessage\",\"params\":{\"message\":{\"type\":\"user\"}}}\n")
          (should-not mevedel-transport--paused)
          ;; A steady stream: only the third text read in a row pauses.
          (mevedel-acp--pace connection process text)
          (mevedel-acp--pace connection process text)
          (should-not mevedel-transport--paused)
          (mevedel-acp--pace connection process text)
          (should (memq process mevedel-transport--paused))
          (mevedel-acp--wake connection)
          (should-not mevedel-transport--paused)
          (setf (mevedel-acp-state connection) 'idle)
          (mevedel-acp--pace connection process text)
          (should-not mevedel-transport--paused))
      (delete-process process))))

(mevedel-deftest mevedel-acp-prompt ()
  ,test
  (test)
  :doc "streams only the owned session and settles each prompt once"
  (mevedel-acp-test--with-connection nil
    (should ready)
    (should-not failure)
    (should (equal "fixture-session" (mevedel-acp-session-id connection)))
    (let ((text "") completions)
      (mevedel-acp-prompt
       connection [((type . "text") (text . "hello"))]
       (lambda (notification)
         (let ((update (alist-get 'update (alist-get 'params notification))))
           (setq text (concat text (alist-get 'text (alist-get 'content update))))))
       (lambda (outcome) (push outcome completions)))
      (await (lambda () completions))
      (should (equal "answer:hello" text))
      (should (eq 'success (plist-get (car completions) :status)))
      (should (equal "end_turn" (plist-get (car completions) :stop-reason)))
      (should (= 1 (length completions)))
      (setq completions nil)
      (mevedel-acp-prompt connection [((type . "text") (text . "again"))]
                          #'ignore (lambda (outcome) (push outcome completions)))
      (await (lambda () completions))
      (should (= 1 (length completions)))))

  :doc "real peer output during an ordinary TRAMP wait retains its ACP drain"
  (mevedel-acp-test--with-connection nil
    (let (outcome)
      (mevedel-transport-call-as-remote-operation
       (lambda ()
         (with-tramp-suspended-timers
           (mevedel-acp-prompt connection [((type . "text") (text . "hello"))]
                               #'ignore (lambda (value) (setq outcome value)))
           (should (accept-process-output
                    (alist-get :process (mevedel-acp-client connection)) 2 nil 0))
           (should-not outcome))))
      (await (lambda () outcome))
      (should (eq 'success (plist-get outcome :status)))))

  :doc "image input is rejected before dispatch when the peer lacks the capability"
  (let ((process-environment (copy-sequence process-environment)))
    (setenv "MEVEDEL_ACP_TEST_NO_IMAGES" "1")
    (mevedel-acp-test--with-connection nil
      (should ready)
      (should-error
       (mevedel-acp-prompt connection [((type . "image") (data . "AA==") (mimeType . "image/png"))]
                           #'ignore #'ignore)
       :type 'user-error)
      (should (eq 'idle (mevedel-acp-state connection)))
      (let (outcome)
        (mevedel-acp-prompt connection [((type . "text") (text . "still usable"))]
                            #'ignore (lambda (value) (setq outcome value)))
        (await (lambda () outcome))
        (should (eq 'success (plist-get outcome :status))))))

  :doc "process loss fails the active prompt, warns the host with stderr, without restarting"
  (mevedel-acp-test--with-connection nil
    (let (outcomes warnings)
      (cl-letf (((symbol-function 'display-warning)
                 (lambda (_type text &rest _) (push text warnings))))
        (mevedel-acp-prompt connection [((type . "text") (text . "crash"))]
                            #'ignore (lambda (outcome) (push outcome outcomes)))
        ;; Busy Emacs: the stderr text and the exit are both pending at once.
        (let ((end (+ (float-time) 0.2))) (while (< (float-time) end)))
        (await (lambda () outcomes)))
      (should (= 1 (length outcomes)))
      (should (eq 'error (plist-get (car outcomes) :status)))
      ;; Host logs stay out of the classified, shared outcome message.
      (should-not (string-search "fixture crash: native binary missing"
                                 (plist-get (car outcomes) :message)))
      (should (string-search "fixture crash: native binary missing" (car warnings)))
      (should-error
       (mevedel-acp-prompt connection [((type . "text") (text . "again"))]
                           #'ignore #'ignore))))

  :doc "agent permission and filesystem requests are refused, never executed"
  (mevedel-acp-test--with-connection nil
    (let ((text "") outcome)
      (mevedel-acp-prompt
       connection [((type . "text") (text . "ask-permission"))]
       (lambda (notification)
         (let ((update (alist-get 'update (alist-get 'params notification))))
           (setq text (concat text (alist-get 'text (alist-get 'content update))))))
       (lambda (value) (setq outcome value)))
      (await (lambda () outcome))
      (should (eq 'success (plist-get outcome :status)))
      (let ((answers (json-parse-string text :object-type 'plist)))
        (should (equal '(:outcome (:outcome "cancelled"))
                       (plist-get answers :permission)))
        (should (= -32601 (plist-get (plist-get answers :fs) :code)))))))

(mevedel-deftest mevedel-acp-cancel
  (:doc "interrupt waits for the cancelled prompt response and keeps history usable")
  (mevedel-acp-test--with-connection nil
    (let (streaming outcomes)
      (mevedel-acp-prompt connection [((type . "text") (text . "wait"))]
                          (lambda (_) (setq streaming t))
                          (lambda (outcome) (push outcome outcomes)))
      (await (lambda () streaming))
      (should-error
       (mevedel-acp-prompt connection [] #'ignore #'ignore))
      (mevedel-acp-cancel connection)
      (let ((watchdog (mevedel-acp-timer connection)))
        (await (lambda () outcomes))
        (should (= 1 (length outcomes)))
        (should (eq 'interrupted (plist-get (car outcomes) :status)))
        (setq outcomes nil)
        (mevedel-acp-prompt connection [((type . "text") (text . "after cancel"))]
                            #'ignore (lambda (outcome) (push outcome outcomes)))
        (await (lambda () outcomes))
        (should (eq 'success (plist-get (car outcomes) :status)))
        ;; A cancelled timer may still be delivered after TRAMP restores its list.
        (funcall (timer--function watchdog))
        (should (eq 'idle (mevedel-acp-state connection)))))))

(mevedel-deftest mevedel-acp--live-p
  (:doc "a dead but unreaped agent fails the connection instead of restarting")
  (mevedel-acp-test--with-connection nil
    (let (outcome)
      (mevedel-acp-prompt connection [((type . "text") (text . "wait-silent"))]
                          #'ignore (lambda (result) (setq outcome result)))
      (let ((process (alist-get :process (mevedel-acp-client connection))))
        (signal-process process 'KILL)
        ;; Busy Emacs: the exit is recorded before any sentinel runs.
        (let ((end (+ (float-time) 3)))
          (while (and (process-live-p process) (< (float-time) end))))
        (should-not (process-live-p process))
        (let ((before (process-list)) warnings)
          (cl-letf (((symbol-function 'display-warning)
                     (lambda (_type text &rest _) (push text warnings))))
            (mevedel-acp-cancel connection))
          (should-not (cl-set-difference (process-list) before))
          (should (string-search "phase=register" (car warnings))))
        (should (eq 'closed (mevedel-acp-state connection)))
        (should (eq 'error (plist-get outcome :status)))
        ;; Let the deferred sentinel release the stderr pipe inside this test.
        (mevedel-test--await 5 "Agent sentinel did not run"
          (not (get-process (concat (process-name process) " stderr"))))))))

(mevedel-deftest mevedel-acp-open ()
  ,test
  (test)
  :doc "resume retains the external session identity"
  (mevedel-acp-test--with-connection "fixture-session"
    (should ready)
    (should-not failure)
    (should (equal "fixture-session" (mevedel-acp-session-id connection))))

  :doc "a retired startup watchdog cannot close an admitted conversation"
  (mevedel-acp-test--with-connection nil
    (should ready)
    (funcall (timer--function startup-timer))
    (should (eq 'idle (mevedel-acp-state connection)))
    (should-not failure))

  :doc "asynchronous readiness admits one process despite duplicate and late callbacks"
  (let (admit reject prep-timer (cancelled 0))
    (unwind-protect
        (let ((mevedel-acp-test--launch-options
               (list :prepare-launch
                     (lambda (success fail)
                       (setq admit success reject fail)
                       (setq prep-timer (run-at-time .01 nil success))
                       (lambda () (cl-incf cancelled) (cancel-timer prep-timer))))))
          (mevedel-acp-test--with-connection nil
            (should ready)
            (funcall admit)
            (funcall reject "Late failure")
            (should-not failure)
            (should (eq 'idle (mevedel-acp-state connection)))
            (should (= 0 cancelled))))
      (when prep-timer (cancel-timer prep-timer))))

  :doc "an asynchronous readiness continuation reports startup exceptions immediately"
  (let (prep-timer)
    (unwind-protect
        (let ((mevedel-acp-test--launch-options
               (list :prepare-launch
                     (lambda (success _fail)
                       (setq prep-timer (run-at-time .01 nil success))
                       (lambda () (cancel-timer prep-timer))))))
          (cl-letf (((symbol-function 'acp-make-initialize-request)
                     (lambda (&rest _) (error "Ready failed"))))
            (mevedel-acp-test--with-connection nil
              (should (equal "Ready failed" failure))
              (should-not ready)
              (should-not (alist-get :process (mevedel-acp-client connection))))))
      (when prep-timer (cancel-timer prep-timer))))

  :doc "admission waits for preparation that outlasts the agent's own startup"
  (let* ((directory (make-temp-file "mevedel-acp-test-" t))
         admit ready
         (connection
          (mevedel-acp-open
           (list :command (executable-find "python3")
                 :args (list mevedel-test--acp-peer) :cwd directory
                 :prepare-launch (lambda (success _fail) (setq admit success) #'ignore))
           (lambda (_connection) (setq ready t))
           (lambda (message) (ert-fail message)))))
    (unwind-protect
        (progn
          ;; The agent starts at once and finishes its session first.
          (mevedel-test--await 5 "Agent session did not start"
            (mevedel-acp-prepared connection))
          (should-not ready)
          (should (eq 'starting (mevedel-acp-state connection)))
          (funcall admit)
          (should ready)
          (should (eq 'idle (mevedel-acp-state connection))))
      (mevedel-acp-close connection)
      (delete-directory directory t)))

  :doc "readiness timeout cancels preparation and closes the starting agent"
  (let (admit reject (cancelled 0)
        (mevedel-acp--control-timeout .05))
    (let ((mevedel-acp-test--launch-options
           (list :prepare-launch
                 (lambda (success fail)
                   (setq admit success reject fail)
                   (lambda () (cl-incf cancelled))))))
      (mevedel-acp-test--with-connection nil
        (should-not ready)
        (should (equal "ACP startup timed out" failure))
        (should (= 1 cancelled))
        (funcall admit)
        (funcall reject "Late failure")
        (should-not (alist-get :process (mevedel-acp-client connection)))
        (should (eq 'closed (mevedel-acp-state connection))))))

  :doc "a failing READY settles its owner through FAILURE"
  (let* ((directory (make-temp-file "mevedel-acp-test-" t))
         failure
         (connection
          (mevedel-acp-open
           (list :command (executable-find "python3")
                 :args (list mevedel-test--acp-peer) :cwd directory)
           (lambda (_connection) (error "Ready exploded"))
           (lambda (message) (setq failure message)))))
    (unwind-protect
        (progn
          (mevedel-test--await 5 "READY failure was not delivered" failure)
          (should (equal "Ready exploded" failure))
          (should (eq 'closed (mevedel-acp-state connection))))
      (mevedel-acp-close connection)
      (delete-directory directory t)))

  :doc "missing retained history fails startup without creating a fresh session"
  (mevedel-acp-test--with-connection "missing-session"
    (should-not ready)
    (should (equal "Missing history" failure))
    (should-error
     (mevedel-acp-prompt connection [] #'ignore #'ignore))))

(mevedel-deftest mevedel-acp-close ()
  ,test
  (test)
  :doc "close settles a waiting prompt once and is idempotent"
  (mevedel-acp-test--with-connection nil
    (let (streaming outcomes)
      (mevedel-acp-prompt connection [((type . "text") (text . "wait"))]
                          (lambda (_) (setq streaming t))
                          (lambda (outcome) (push outcome outcomes)))
      (await (lambda () streaming))
      (mevedel-acp-close connection)
      (mevedel-acp-close connection)
      (should (= 1 (length outcomes)))
      (should (eq 'interrupted (plist-get (car outcomes) :status)))))
  :doc "an orderly close is not reported as an agent crash, despite routine stderr"
  (let (warnings)
    (cl-letf (((symbol-function 'display-warning)
               (lambda (_type text &rest _) (push text warnings))))
      (mevedel-acp-test--with-connection nil
        (let (outcome)
          (mevedel-acp-prompt connection [((type . "text") (text . "hello"))]
                              #'ignore (lambda (value) (setq outcome value)))
          (await (lambda () (and outcome (mevedel-acp-stderr connection))))
          (should (eq 'success (plist-get outcome :status)))
          ;; `delete-process' runs the agent's sentinel synchronously.
          (mevedel-acp-close connection))))
    (should-not warnings)))

(provide 'test-mevedel-acp)
;;; test-mevedel-acp.el ends here
