;;; test-mevedel-acp.el --- ACP conversation lifecycle -*- lexical-binding: t -*-

;;; Commentary:
;; Drive a deterministic subprocess through acp.el and the public lifecycle.

;;; Code:

(require 'mevedel-acp)
(require 'helpers
         (file-name-concat
          (file-name-directory (or load-file-name buffer-file-name)) "helpers"))

(defconst mevedel-acp-test--peer
  (file-name-concat (file-name-directory (or load-file-name buffer-file-name))
                    "fixtures" "acp-agent.py"))

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
                          :args (list mevedel-acp-test--peer)
                          :cwd directory :session-id ,session-id)
                    mevedel-acp-test--launch-options)
            (lambda (_connection) (setq ready t))
            (lambda (error) (setq failure error))))
          (startup-timer (mevedel-acp-timer connection)))
     (unwind-protect
         (cl-labels ((await (predicate)
                       (with-timeout (5 (ert-fail "ACP peer did not respond"))
                         (while (not (funcall predicate))
                           (accept-process-output nil 0.01)))))
           (await (lambda () (or ready failure)))
           ,@body)
       (mevedel-acp-close connection)
       (delete-directory directory t))))

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

  :doc "process loss fails the active prompt with the agent's stderr, without restarting"
  (mevedel-acp-test--with-connection nil
    (let (outcomes)
      (mevedel-acp-prompt connection [((type . "text") (text . "crash"))]
                          #'ignore (lambda (outcome) (push outcome outcomes)))
      ;; Busy Emacs: the stderr text and the exit are both pending at once.
      (let ((end (+ (float-time) 0.2))) (while (< (float-time) end)))
      (await (lambda () outcomes))
      (should (= 1 (length outcomes)))
      (should (eq 'error (plist-get (car outcomes) :status)))
      (should (string-search "fixture crash: native binary missing"
                             (plist-get (car outcomes) :message)))
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
        (let ((before (process-list)))
          (mevedel-acp-cancel connection)
          (should-not (cl-set-difference (process-list) before)))
        (should (eq 'closed (mevedel-acp-state connection)))
        (should (eq 'error (plist-get outcome :status)))
        ;; Let the deferred sentinel release the stderr pipe inside this test.
        (with-timeout (5 (ert-fail "Agent sentinel did not run"))
          (while (get-process (concat (process-name process) " stderr"))
            (accept-process-output nil 0.01)))))))

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

  :doc "readiness timeout cancels preparation before any process starts"
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
                 :args (list mevedel-acp-test--peer) :cwd directory)
           (lambda (_connection) (error "Ready exploded"))
           (lambda (message) (setq failure message)))))
    (unwind-protect
        (progn
          (with-timeout (5 (ert-fail "READY failure was not delivered"))
            (while (not failure) (accept-process-output nil 0.01)))
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

(mevedel-deftest mevedel-acp-close
  (:doc "close settles a waiting prompt once and is idempotent")
  (mevedel-acp-test--with-connection nil
    (let (streaming outcomes)
      (mevedel-acp-prompt connection [((type . "text") (text . "wait"))]
                          (lambda (_) (setq streaming t))
                          (lambda (outcome) (push outcome outcomes)))
      (await (lambda () streaming))
      (mevedel-acp-close connection)
      (mevedel-acp-close connection)
      (should (= 1 (length outcomes)))
      (should (eq 'interrupted (plist-get (car outcomes) :status))))))

(provide 'test-mevedel-acp)
;;; test-mevedel-acp.el ends here
