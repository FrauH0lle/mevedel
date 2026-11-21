;;; test-niwa-acp.el --- ACP conversation lifecycle -*- lexical-binding: t -*-

;;; Commentary:
;; Drive a deterministic subprocess through acp.el and the public lifecycle.

;;; Code:

(require 'niwa-acp)
(require 'niwa-transport)
(require 'helpers
         (file-name-concat
          (file-name-directory (or load-file-name buffer-file-name)) "helpers"))

(defvar niwa-acp-test--launch-options nil
  "Additional launch settings captured by the connection fixture.")

(defmacro niwa-acp-test--with-connection (session-id &rest body)
  "Run BODY with an ACP peer, optionally resuming SESSION-ID."
  (declare (indent 1) (debug t))
  `(let* ((directory (make-temp-file "niwa-acp-test-" t))
          (niwa-user-dir (file-name-concat directory "user/"))
          ready failure
          (connection
           (niwa-acp-open
            (append (list :command (executable-find "python3")
                          :args (list niwa-test--acp-peer)
                          :cwd directory :session-id ,session-id)
                    niwa-acp-test--launch-options)
            (lambda (_connection) (setq ready t))
            (lambda (error _code) (setq failure error))))
          (startup-timer (niwa-acp-timer connection)))
     (unwind-protect
         (cl-labels ((await (predicate)
                       (niwa-test--await 5 "ACP peer did not respond" (funcall predicate))))
           (await (lambda () (or ready failure)))
           ,@body)
       (niwa-acp-close connection)
       (delete-directory directory t))))

(defvar niwa-gptel-stream-bridge-insert-batch-delay)

(niwa-deftest niwa-acp--trim-stderr ()
  (niwa-acp-test--with-connection nil
    (should ready)
    (let* ((process (niwa-acp--process connection))
           (stderr (get-process (concat (process-name process) " stderr")))
           (niwa-acp--stderr-log-limit 64)
           (chunk (concat (make-string 100 ?x) " recent error\n")))
      (should stderr)
      (setf (niwa-acp-stderr connection) nil)
      (with-current-buffer (process-buffer stderr)
        (dotimes (_ 3)
          (goto-char (point-max))
          (insert chunk)
          (should (= 64 (buffer-size)))
          (should (string-suffix-p " recent error\n" (buffer-string)))))
      ;; Trimming must not emit a synthetic error or hide the original chunk.
      (should (equal (niwa-acp-stderr connection) (concat chunk chunk chunk))))))

(niwa-deftest niwa-acp--pace ()
  ,test
  (test)
  :doc "pauses a turn's agent after small reads and wakes it before messages"
  (skip-unless (and (executable-find "sleep")
                    (not (memq system-type '(windows-nt ms-dos)))))
  (let* ((niwa-gptel-stream-bridge-insert-batch-delay 30)
         (niwa-transport--paused nil)
         (niwa-transport--pause-guard nil)
         (process (make-process :name "niwa-test-acp-pace" :command '("sleep" "30")
                                :connection-type 'pipe :noquery t))
         (connection (niwa-acp--create :client (list (cons :process process))
                                          :state 'prompting))
         ;; Claude's adapter sends each delta as an ACP chunk and an SDK event.
         (text (concat "{\"method\":\"session/update\",\"params\":{\"update\":"
                       "{\"sessionUpdate\":\"agent_message_chunk\"}}}\n"
                       "{\"method\":\"_claude/sdkMessage\",\"params\":{\"message\":"
                       "{\"type\":\"stream_event\",\"event\":{\"type\":"
                       "\"content_block_delta\"}}}}\n")))
    (unwind-protect
        (progn
          (niwa-acp--pace connection process (make-string 4096 ?x))
          (should-not niwa-transport--paused)
          ;; Receipts, tool calls and replies must not wait behind a pause.
          (niwa-acp--pace connection process
                             "{\"method\":\"_claude/sdkMessage\",\"params\":{\"message\":{\"type\":\"user\"}}}\n")
          (should-not niwa-transport--paused)
          ;; A steady stream: only the third text read in a row pauses.
          (niwa-acp--pace connection process text)
          (niwa-acp--pace connection process text)
          (should-not niwa-transport--paused)
          (niwa-acp--pace connection process text)
          (should (memq process niwa-transport--paused))
          (niwa-acp--wake connection)
          ;; SIGCONT's status notification can arrive after wake returns.
          (niwa-test--await 5 "Paced child did not resume"
            (eq (process-status process) 'run))
          ;; The text batch, armed a pass after the read, falls due first.
          (let ((niwa-gptel-stream-bridge-insert-batch-delay 0.2)
                (schedule (symbol-function 'niwa-transport-run-at-time))
                requested-delay)
            (cl-letf (((symbol-function 'niwa-transport-run-at-time)
                       (lambda (seconds function &rest args)
                         (when (eq function #'niwa-transport-resume-process)
                           (setq requested-delay seconds))
                         (apply schedule seconds function args))))
              (niwa-acp--pace connection process text)
              (should (equal requested-delay 0.25)))
            (niwa-acp--wake connection))
          (should-not niwa-transport--paused)
          ;; Disabling text batching disables pacing, including its margin.
          (dolist (delay '(nil 0 -0.01))
            (let ((niwa-gptel-stream-bridge-insert-batch-delay delay))
              (niwa-acp--pace connection process text)
              (should-not niwa-transport--paused)
              (should-not
               (process-get process 'niwa-transport--resume-timer))))
          (setf (niwa-acp-state connection) 'idle)
          (niwa-acp--pace connection process text)
          (should-not niwa-transport--paused))
      (delete-process process)
      (when niwa-transport--pause-guard
        (delete-process niwa-transport--pause-guard)))))

(niwa-deftest niwa-acp-prompt ()
  ,test
  (test)
  :doc "streams only the owned session and settles each prompt once"
  (niwa-acp-test--with-connection nil
    (should ready)
    (should-not failure)
    (should (equal "fixture-session" (niwa-acp-session-id connection)))
    (let ((text "") completions)
      (niwa-acp-prompt
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
      (niwa-acp-prompt connection [((type . "text") (text . "again"))]
                          #'ignore (lambda (outcome) (push outcome completions)))
      (await (lambda () completions))
      (should (= 1 (length completions)))))

  :doc "real peer output during an ordinary TRAMP wait retains its ACP drain"
  (niwa-acp-test--with-connection nil
    (let (outcome)
      (niwa-transport-call-as-remote-operation
       (lambda ()
         (with-tramp-suspended-timers
           (niwa-acp-prompt connection [((type . "text") (text . "hello"))]
                               #'ignore (lambda (value) (setq outcome value)))
           (should (accept-process-output
                    (alist-get :process (niwa-acp-client connection)) 2 nil 0))
           (should-not outcome))))
      (await (lambda () outcome))
      (should (eq 'success (plist-get outcome :status)))))

  :doc "image input is rejected before dispatch when the peer lacks the capability"
  (let ((process-environment (copy-sequence process-environment)))
    (setenv "NIWA_ACP_TEST_NO_IMAGES" "1")
    (niwa-acp-test--with-connection nil
      (should ready)
      (should-error
       (niwa-acp-prompt connection [((type . "image") (data . "AA==") (mimeType . "image/png"))]
                           #'ignore #'ignore)
       :type 'user-error)
      (should (eq 'idle (niwa-acp-state connection)))
      (let (outcome)
        (niwa-acp-prompt connection [((type . "text") (text . "still usable"))]
                            #'ignore (lambda (value) (setq outcome value)))
        (await (lambda () outcome))
        (should (eq 'success (plist-get outcome :status))))))

  :doc "process loss fails the active prompt, warns the host with stderr, without restarting"
  (let ((niwa-acp-test--launch-options '(:diagnostic-scope "/workspace/provider/")))
  (niwa-acp-test--with-connection nil
    (let (outcomes warnings)
      (cl-letf (((symbol-function 'display-warning)
                 (lambda (_type text &rest _) (push text warnings))))
        (niwa-acp-prompt connection [((type . "text") (text . "crash"))]
                            #'ignore (lambda (outcome) (push outcome outcomes)))
        ;; Leave both stderr and exit pending without assuming child scheduling
        ;; latency.  Yielding the Lisp lock does not dispatch main-thread I/O.
        (let ((process (alist-get :process (niwa-acp-client connection)))
              (deadline (+ (float-time) 5)))
          (while (and (process-live-p process) (< (float-time) deadline))
            (thread-yield))
          (should-not (process-live-p process)))
        (should-not outcomes)
        (await (lambda () outcomes)))
      (should (= 1 (length outcomes)))
      (should (eq 'error (plist-get (car outcomes) :status)))
      (should (string-prefix-p "niwa [/workspace/provider/]: " (car warnings)))
      (should-not (string-search "niwa [" (plist-get (car outcomes) :message)))
      (let ((log (file-name-concat niwa-user-dir "agent-stderr.log")))
        ;; Host logs stay out of the classified, shared outcome message,
        ;; which names the log that outlives the warning buffer instead.
        (should-not (string-search "fixture crash: native binary missing"
                                   (plist-get (car outcomes) :message)))
        (should (string-suffix-p (concat "; agent stderr is in " (abbreviate-file-name log))
                                 (plist-get (car outcomes) :message)))
        (should (string-search "fixture crash: native binary missing" (car warnings)))
        (should (string-search "fixture crash: native binary missing"
                               (with-temp-buffer (insert-file-contents log) (buffer-string))))
        (should (= #o600 (file-modes log))))
      (should-error
       (niwa-acp-prompt connection [((type . "text") (text . "again"))]
                           #'ignore #'ignore)))))

  :doc "a prompt refused before any tool call is re-sent once when the launch accepts the failure"
  (let ((niwa-acp-test--launch-options
         (list :prompt-retry-delay
               (lambda (failure)
                 (and (string-search "refreshing it" (alist-get 'message failure)) 0.05)))))
    (niwa-acp-test--with-connection nil
      (let ((text "") outcomes)
        (niwa-acp-prompt
         connection [((type . "text") (text . "refresh-race"))]
         (lambda (notification)
           (let ((update (alist-get 'update (alist-get 'params notification))))
             (setq text (concat text (alist-get 'text (alist-get 'content update))))))
         (lambda (outcome) (push outcome outcomes)))
        (await (lambda () outcomes))
        (should (= 1 (length outcomes)))
        (should (eq 'success (plist-get (car outcomes) :status)))
        ;; The streamed refusal stays visible ahead of the replayed answer.
        (should (equal "Failed to refresh OAuth tokenanswer:attempt-2" text))
        (should (eq 'idle (niwa-acp-state connection))))))

  :doc "a refused prompt that reported a tool call fails instead of replaying"
  (let ((niwa-acp-test--launch-options (list :prompt-retry-delay (lambda (_) 0.05))))
    (niwa-acp-test--with-connection nil
      (let (outcomes)
        (niwa-acp-prompt connection [((type . "text") (text . "refresh-race-after-tool"))]
                            #'ignore (lambda (outcome) (push outcome outcomes)))
        (await (lambda () outcomes))
        (should (= 1 (length outcomes)))
        (should (eq 'error (plist-get (car outcomes) :status)))
        (should (string-search "refreshing it" (plist-get (car outcomes) :message)))
        (should (eq 'closed (niwa-acp-state connection))))))

  :doc "a second refusal fails the prompt; cancellation during the wait settles at once"
  (let ((niwa-acp-test--launch-options (list :prompt-retry-delay (lambda (_) 60))))
    (niwa-acp-test--with-connection nil
      (let (outcomes timer)
        (niwa-acp-prompt connection [((type . "text") (text . "refresh-race"))]
                            #'ignore (lambda (outcome) (push outcome outcomes)))
        (await (lambda () (plist-get (niwa-acp-active connection) :retry-timer)))
        (setq timer (plist-get (niwa-acp-active connection) :retry-timer))
        (niwa-acp-cancel connection)
        (should (= 1 (length outcomes)))
        (should (eq 'interrupted (plist-get (car outcomes) :status)))
        (should-not (memq timer timer-list))
        (should-not (niwa-acp-timer connection))
        (should (eq 'idle (niwa-acp-state connection))))))
  (let ((niwa-acp-test--launch-options (list :prompt-retry-delay (lambda (_) 0.05))))
    (niwa-acp-test--with-connection nil
      (let (outcomes)
        (niwa-acp-prompt connection [((type . "text") (text . "refresh-race-always"))]
                            #'ignore (lambda (outcome) (push outcome outcomes)))
        (await (lambda () outcomes))
        (should (= 1 (length outcomes)))
        (should (eq 'error (plist-get (car outcomes) :status))))))

  :doc "an error in the retry policy settles instead of stranding a refused prompt"
  (let ((niwa-acp-test--launch-options
         (list :prompt-retry-delay (lambda (_) (error "Retry policy failed")))))
    (niwa-acp-test--with-connection nil
      (let (outcomes)
        (niwa-acp-prompt connection [((type . "text") (text . "refresh-race"))]
                            #'ignore (lambda (outcome) (push outcome outcomes)))
        (await (lambda () outcomes))
        (should (= 1 (length outcomes)))
        (should (eq 'error (plist-get (car outcomes) :status)))
        (should (equal "Retry policy failed" (plist-get (car outcomes) :message)))
        (should (eq 'closed (niwa-acp-state connection))))))

  :doc "closing during a retry wait retires its timer without dispatching a replay"
  (let ((niwa-acp-test--launch-options (list :prompt-retry-delay (lambda (_) 60))))
    (niwa-acp-test--with-connection nil
      (let (outcomes timer)
        (niwa-acp-prompt connection [((type . "text") (text . "refresh-race"))]
                            #'ignore (lambda (outcome) (push outcome outcomes)))
        (await (lambda () (plist-get (niwa-acp-active connection) :retry-timer)))
        (setq timer (plist-get (niwa-acp-active connection) :retry-timer))
        (should (eq 'prompting (niwa-acp-state connection)))
        (should-error (niwa-acp-prompt connection [] #'ignore #'ignore))
        (niwa-acp-close connection)
        (funcall (timer--function timer))
        (should (= 1 (length outcomes)))
        (should (eq 'interrupted (plist-get (car outcomes) :status)))
        (should-not (memq timer timer-list))
        (should-not (alist-get :process (niwa-acp-client connection)))
        (should (eq 'closed (niwa-acp-state connection))))))

  :doc "agent permission and filesystem requests are refused, never executed"
  (niwa-acp-test--with-connection nil
    (let ((text "") outcome)
      (niwa-acp-prompt
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

(niwa-deftest niwa-acp-cancel ()
  ,test
  (test)
  :doc "interrupt waits for the cancelled prompt response and keeps history usable"
  (niwa-acp-test--with-connection nil
    (let (streaming outcomes)
      (niwa-acp-prompt connection [((type . "text") (text . "wait"))]
                          (lambda (_) (setq streaming t))
                          (lambda (outcome) (push outcome outcomes)))
      (await (lambda () streaming))
      (should-error
       (niwa-acp-prompt connection [] #'ignore #'ignore))
      (niwa-acp-cancel connection)
      (let ((watchdog (niwa-acp-timer connection)))
        (await (lambda () outcomes))
        (should (= 1 (length outcomes)))
        (should (eq 'interrupted (plist-get (car outcomes) :status)))
        (setq outcomes nil)
        (niwa-acp-prompt connection [((type . "text") (text . "after cancel"))]
                            #'ignore (lambda (outcome) (push outcome outcomes)))
        (await (lambda () outcomes))
        (should (eq 'success (plist-get (car outcomes) :status)))
        ;; A cancelled timer may still be delivered after TRAMP restores its list.
        (funcall (timer--function watchdog))
        (should (eq 'idle (niwa-acp-state connection))))))

  :doc "cancelling a retry wait cannot cancel a turn started by its completion"
  (let ((niwa-acp-test--launch-options (list :prompt-retry-delay (lambda (_) 60))))
    (niwa-acp-test--with-connection nil
      (let (outcomes replacement timer)
        (niwa-acp-prompt
         connection [((type . "text") (text . "refresh-race"))] #'ignore
         (lambda (outcome)
           (push outcome outcomes)
           (niwa-acp-prompt connection [((type . "text") (text . "wait"))]
                               #'ignore (lambda (value) (push value replacement)))))
        (await (lambda () (plist-get (niwa-acp-active connection) :retry-timer)))
        (setq timer (plist-get (niwa-acp-active connection) :retry-timer))
        (niwa-acp-cancel connection)
        (should (= 1 (length outcomes)))
        (should (eq 'interrupted (plist-get (car outcomes) :status)))
        (should-not (plist-get (niwa-acp-active connection) :cancelled))
        (should-not (niwa-acp-timer connection))
        ;; TRAMP can restore a cancelled timer: it must not replay the old prompt.
        (funcall (timer--function timer))
        (should-not replacement)
        (niwa-acp-cancel connection)
        (await (lambda () replacement))
        (should (= 1 (length replacement)))
        (should (eq 'interrupted (plist-get (car replacement) :status))))))

  :doc "cancellation racing the refusal never schedules a replay"
  (let ((niwa-acp-test--launch-options
         (list :prompt-retry-delay (lambda (_) (ert-fail "Cancelled prompt retried")))))
    (niwa-acp-test--with-connection nil
      (let (outcomes)
        (niwa-acp-prompt
         connection [((type . "text") (text . "refresh-race"))]
         (lambda (notification)
           (when (alist-get 'update (alist-get 'params notification))
             (niwa-acp-cancel connection)))
         (lambda (outcome) (push outcome outcomes)))
        (await (lambda () outcomes))
        (should (= 1 (length outcomes)))
        (should (eq 'error (plist-get (car outcomes) :status)))
        (should (eq 'closed (niwa-acp-state connection)))
        (should-not (niwa-acp-timer connection)))))

  :doc "cancellation after replay waits for that prompt's acknowledgement"
  (let ((niwa-acp-test--launch-options (list :prompt-retry-delay (lambda (_) 0.01))))
    (niwa-acp-test--with-connection nil
      (let (outcomes streaming)
        (niwa-acp-prompt
         connection [((type . "text") (text . "refresh-race-wait"))]
         (lambda (notification)
           (when (equal "retry waiting"
                        (alist-get 'text (alist-get 'content
                          (alist-get 'update (alist-get 'params notification)))))
             (setq streaming t)))
         (lambda (outcome) (push outcome outcomes)))
        (await (lambda () streaming))
        (should (plist-get (niwa-acp-active connection) :retried))
        (should-not (plist-get (niwa-acp-active connection) :retry-timer))
        (niwa-acp-cancel connection)
        (should-not outcomes)
        (should (niwa-acp-timer connection))
        (await (lambda () outcomes))
        (should (= 1 (length outcomes)))
        (should (eq 'interrupted (plist-get (car outcomes) :status)))
        (should (eq 'idle (niwa-acp-state connection)))
        (should-not (niwa-acp-timer connection))))))

(niwa-deftest niwa-acp--live-p
  (:doc "a dead but unreaped agent fails the connection instead of restarting")
  (niwa-acp-test--with-connection nil
    (let (outcome)
      (niwa-acp-prompt connection [((type . "text") (text . "wait-silent"))]
                          #'ignore (lambda (result) (setq outcome result)))
      (let ((process (alist-get :process (niwa-acp-client connection))))
        (signal-process process 'KILL)
        ;; Busy Emacs: the exit is recorded before any sentinel runs.
        (let ((end (+ (float-time) 3)))
          (while (and (process-live-p process) (< (float-time) end))))
        (should-not (process-live-p process))
        (let ((before (process-list)) warnings)
          (cl-letf (((symbol-function 'display-warning)
                     (lambda (_type text &rest _) (push text warnings))))
            (niwa-acp-cancel connection))
          (should-not (cl-set-difference (process-list) before))
          (should (string-search "phase=register" (car warnings))))
        (should (eq 'closed (niwa-acp-state connection)))
        (should (eq 'error (plist-get outcome :status)))
        ;; Let the deferred sentinel release the stderr pipe inside this test.
        (niwa-test--await 5 "Agent sentinel did not run"
          (not (get-process (concat (process-name process) " stderr"))))))))

(niwa-deftest niwa-acp-open ()
  ,test
  (test)
  :doc "resume retains the external session identity"
  (niwa-acp-test--with-connection "fixture-session"
    (should ready)
    (should-not failure)
    (should (equal "fixture-session" (niwa-acp-session-id connection))))

  :doc "a retired startup watchdog cannot close an admitted conversation"
  (niwa-acp-test--with-connection nil
    (should ready)
    (funcall (timer--function startup-timer))
    (should (eq 'idle (niwa-acp-state connection)))
    (should-not failure))

  :doc "asynchronous readiness admits one process despite duplicate and late callbacks"
  (let (admit reject prep-timer (cancelled 0))
    (unwind-protect
        (let ((niwa-acp-test--launch-options
               (list :prepare-launch
                     (lambda (success fail)
                       (setq admit success reject fail)
                       (setq prep-timer (run-at-time .01 nil success))
                       (lambda () (cl-incf cancelled) (cancel-timer prep-timer))))))
          (niwa-acp-test--with-connection nil
            (should ready)
            (funcall admit)
            (funcall reject "Late failure")
            (should-not failure)
            (should (eq 'idle (niwa-acp-state connection)))
            (should (= 0 cancelled))))
      (when prep-timer (cancel-timer prep-timer))))

  :doc "an asynchronous readiness continuation reports startup exceptions immediately"
  (let (prep-timer)
    (unwind-protect
        (let ((niwa-acp-test--launch-options
               (list :prepare-launch
                     (lambda (success _fail)
                       (setq prep-timer (run-at-time .01 nil success))
                       (lambda () (cancel-timer prep-timer))))))
          (cl-letf (((symbol-function 'acp-make-initialize-request)
                     (lambda (&rest _) (error "Ready failed"))))
            (niwa-acp-test--with-connection nil
              (should (equal "Ready failed" failure))
              (should-not ready)
              (should-not (alist-get :process (niwa-acp-client connection))))))
      (when prep-timer (cancel-timer prep-timer))))

  :doc "admission waits for preparation that outlasts the agent's own startup"
  (let* ((directory (make-temp-file "niwa-acp-test-" t))
         admit ready
         (connection
          (niwa-acp-open
           (list :command (executable-find "python3")
                 :args (list niwa-test--acp-peer) :cwd directory
                 :prepare-launch (lambda (success _fail) (setq admit success) #'ignore))
           (lambda (_connection) (setq ready t))
           (lambda (message _code) (ert-fail message)))))
    (unwind-protect
        (progn
          ;; The agent starts at once and finishes its session first.
          (niwa-test--await 5 "Agent session did not start"
            (niwa-acp-prepared connection))
          (should-not ready)
          (should (eq 'starting (niwa-acp-state connection)))
          (funcall admit)
          (should ready)
          (should (eq 'idle (niwa-acp-state connection))))
      (niwa-acp-close connection)
      (delete-directory directory t)))

  :doc "readiness timeout cancels preparation and closes the starting agent"
  (let (admit reject (cancelled 0)
        (niwa-acp--control-timeout .05))
    (let ((niwa-acp-test--launch-options
           (list :prepare-launch
                 (lambda (success fail)
                   (setq admit success reject fail)
                   (lambda () (cl-incf cancelled))))))
      (niwa-acp-test--with-connection nil
        (should-not ready)
        (should (equal "ACP startup timed out" failure))
        (should (= 1 cancelled))
        (funcall admit)
        (funcall reject "Late failure")
        (should-not (alist-get :process (niwa-acp-client connection)))
        (should (eq 'closed (niwa-acp-state connection))))))

  :doc "a failing READY settles its owner through FAILURE"
  (let* ((directory (make-temp-file "niwa-acp-test-" t))
         failure
         (connection
          (niwa-acp-open
           (list :command (executable-find "python3")
                 :args (list niwa-test--acp-peer) :cwd directory)
           (lambda (_connection) (error "Ready exploded"))
           (lambda (message _code) (setq failure message)))))
    (unwind-protect
        (progn
          (niwa-test--await 5 "READY failure was not delivered" failure)
          (should (equal "Ready exploded" failure))
          (should (eq 'closed (niwa-acp-state connection))))
      (niwa-acp-close connection)
      (delete-directory directory t)))

  :doc "missing retained history fails startup without creating a fresh session"
  (niwa-acp-test--with-connection "missing-session"
    (should-not ready)
    (should (equal "Missing history" failure))
    (should-error
     (niwa-acp-prompt connection [] #'ignore #'ignore))))

(niwa-deftest niwa-acp-close ()
  ,test
  (test)
  :doc "close settles a waiting prompt once and is idempotent"
  (niwa-acp-test--with-connection nil
    (let (streaming outcomes)
      (niwa-acp-prompt connection [((type . "text") (text . "wait"))]
                          (lambda (_) (setq streaming t))
                          (lambda (outcome) (push outcome outcomes)))
      (await (lambda () streaming))
      (niwa-acp-close connection)
      (niwa-acp-close connection)
      (should (= 1 (length outcomes)))
      (should (eq 'interrupted (plist-get (car outcomes) :status)))))
  :doc "an orderly close is not reported as an agent crash, despite routine stderr"
  (let (warnings)
    (cl-letf (((symbol-function 'display-warning)
               (lambda (_type text &rest _) (push text warnings))))
      (niwa-acp-test--with-connection nil
        (let (outcome)
          (niwa-acp-prompt connection [((type . "text") (text . "hello"))]
                              #'ignore (lambda (value) (setq outcome value)))
          (await (lambda () (and outcome (niwa-acp-stderr connection))))
          (should (eq 'success (plist-get outcome :status)))
          ;; `delete-process' runs the agent's sentinel synchronously.
          (niwa-acp-close connection)
          (should-not (file-exists-p (file-name-concat niwa-user-dir "agent-stderr.log"))))))
    (should-not warnings)))

(niwa-deftest niwa-acp--log-stderr ()
  ,test
  (test)
  :doc "appends exits after earlier ones and keeps only the most recent text"
  (let* ((niwa-user-dir (file-name-as-directory (make-temp-file "niwa-acp-log-" t)))
         (connection (niwa-acp--create :launch '(:command "agent")))
         (niwa-acp--stderr-log-limit 200))
    (unwind-protect
        (let ((file (niwa-acp--log-stderr connection "first exit" "old line")))
          (should (equal (file-name-concat niwa-user-dir "agent-stderr.log") file))
          (niwa-acp--log-stderr connection "second exit" "new line")
          (let ((text (with-temp-buffer (insert-file-contents file) (buffer-string))))
            (should (string-search "first exit" text))
            (should (< (string-search "old line" text) (string-search "new line" text)))
            (should (string-search " agent\nsecond exit\nAgent stderr:\nnew line\n" text)))
          (niwa-acp--log-stderr connection "third exit" (make-string 150 ?x))
          (let ((text (with-temp-buffer (insert-file-contents file) (buffer-string))))
            (should (<= (length text) 200))
            (should-not (string-search "old line" text))
            (should (string-search (make-string 150 ?x) text))))
      (delete-directory niwa-user-dir t)))
  :doc "an unwritable log returns nil instead of signalling"
  (let* ((root (make-temp-file "niwa-acp-log-" t))
         (niwa-user-dir (file-name-concat root "blocked/"))
         (connection (niwa-acp--create :launch '(:command "agent"))))
    (unwind-protect
        (progn
          (with-temp-file (directory-file-name niwa-user-dir))
          (should-not (niwa-acp--log-stderr connection "exit" "text")))
      (delete-directory root t))))

(provide 'test-niwa-acp)
;;; test-niwa-acp.el ends here
