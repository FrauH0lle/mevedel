;;; test-mevedel-eval-sandbox.el --- Sandbox service lifecycle tests -*- lexical-binding: t -*-

;;; Commentary:

;; Real confined child Emacsen, through the module and execution facade APIs.

;;; Code:

(require 'mevedel-eval-sandbox)
(require 'mevedel-execution)
(require 'mevedel-execution-process)
(require 'mevedel-sandbox)
(require 'mevedel-transport)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file)) "helpers"))
(require 'mevedel-execution-test-helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "mevedel-execution-test-helpers"))

(defun test-mevedel-eval-sandbox--fixture (function)
  "Call FUNCTION with an isolated real session and writable root."
  (let* ((root (file-name-as-directory (make-temp-file "eval-sandbox-" t)))
         (session (test-mevedel-execution--session root))
         (mevedel-eval-sandbox--sessions (make-hash-table :test #'eq))
         (mevedel-eval-sandbox-idle-timeout nil))
    (unwind-protect
        (funcall function session root)
      (mevedel-execution-teardown-session session)
      (delete-directory root t))))

(defun test-mevedel-eval-sandbox--call (session root expression &rest keys)
  "Evaluate EXPRESSION in SESSION at ROOT with optional KEYS and await reply."
  (let (result)
    (apply #'mevedel-eval-sandbox-call session "/root" expression
           (lambda (reply) (setq result reply))
           :workdir root :writable-roots (list root) keys)
    (test-mevedel-execution--wait (lambda () result) 10)
    result))

(defun test-mevedel-eval-sandbox--require-backend ()
  "Skip a real-child case only if the confinement backend is unavailable."
  (let ((probe (mevedel-sandbox-probe)))
    (unless (plist-get probe :available) (ert-skip (plist-get probe :reason)))))

(mevedel-deftest mevedel-eval-sandbox-call ()
  (test-mevedel-eval-sandbox--fixture
   (lambda (session root) ,test))
  (test)
  :doc "persists definitions, resets, and isolates retained owner paths"
  (progn
    (test-mevedel-eval-sandbox--require-backend)
    (should (plist-get (test-mevedel-eval-sandbox--call
			session root "(defun sandbox-test-value () 42)") :started))
    (let ((reply (test-mevedel-eval-sandbox--call session root "(sandbox-test-value)")))
      (should (equal "42" (plist-get reply :value)))
      (should-not (plist-get reply :started))
      (should (> (plist-get reply :features) 0)))
    (let (reply)
      (mevedel-eval-sandbox-call
       session "/root/agent" "(progn (setq agent-state 9) (fboundp 'sandbox-test-value))"
       (lambda (value) (setq reply value)) :workdir root :writable-roots (list root))
      (test-mevedel-execution--wait (lambda () reply) 10)
      (should (equal "nil" (plist-get reply :value)))
      (should (= 0 (mevedel-execution-stop-owner session "/root/agent")))
      (setq reply nil)
      (mevedel-eval-sandbox-call
       session "/root/agent" "agent-state" (lambda (value) (setq reply value))
       :workdir root :writable-roots (list root))
      (test-mevedel-execution--wait (lambda () reply) 10)
      (should (equal "9" (plist-get reply :value))))
    (let ((reply (test-mevedel-eval-sandbox--call
		  session root "(fboundp 'sandbox-test-value)" :reset t)))
      (should (equal "nil" (plist-get reply :value)))
      (should (plist-get reply :started))
      (should (plist-get reply :state-lost)))
    (should-not (plist-get (test-mevedel-eval-sandbox--call session root "t") :state-lost))
    (let ((rows (mevedel-eval-sandbox-list-idle session nil)))
      (should (= 2 (length rows)))
      (mevedel-eval-sandbox-stop-user session (car rows))
      (should (mevedel-eval-sandbox-any-live-p session))
      (mevedel-eval-sandbox-stop-user session (cadr rows))
      (should-not (mevedel-eval-sandbox-any-live-p session))))
  :doc "idle is inert; owner finalization keeps idle services but stops active calls"
  (progn
    (test-mevedel-eval-sandbox--require-backend)
    (test-mevedel-eval-sandbox--call session root "(setq sandbox-state 7)")
    (should-not (mevedel-execution-working-p))
    (should-not (mevedel-execution-owner-live-p session "/root"))
    (should-not (mevedel-execution-session-live-p session))
    (should-not (mevedel-execution-list session "/root"))
    (should-not (mevedel-execution-list-user session))
    (should (= 0 (mevedel-execution-stop-owner session "/root")))
    (should (equal "7" (plist-get (test-mevedel-eval-sandbox--call
				   session root "sandbox-state") :value)))
    (let (reply)
      (mevedel-eval-sandbox-call session "/root" "(sleep-for 30)"
				 (lambda (value) (setq reply value))
				 :workdir root :writable-roots (list root))
      (should (mevedel-execution-working-p))
      (should (mevedel-execution-owner-live-p session "/root"))
      (should (mevedel-execution-session-live-p session))
      (should-error (test-mevedel-eval-sandbox--call session root "t" :reset t))
      (mevedel-execution-stop-owner session "/root")
      (should (plist-get reply :error))
      (should-not (mevedel-execution-session-live-p session))))
  :doc "arms expiry only after settlement and removes it on death"
  (progn
    (test-mevedel-eval-sandbox--require-backend)
    (should (= 0 (hash-table-count mevedel-eval-sandbox--sessions)))
    (let ((mevedel-eval-sandbox-idle-timeout 0.1))
      (test-mevedel-eval-sandbox--call session root "t")
      (let* ((entry (mevedel-eval-sandbox--entry session "/root"))
	     (service (mevedel-eval-sandbox--entry-service entry)))
	(should (timerp (mevedel-eval-sandbox--entry-timer entry)))
	(let (reply)
	  (mevedel-eval-sandbox-call
	   session "/root" "(sleep-for 0.2)" (lambda (value) (setq reply value))
	   :workdir root :writable-roots (list root))
	  (should-not (mevedel-eval-sandbox--entry-timer entry))
	  (test-mevedel-execution--wait (lambda () reply))
	  (should-not (plist-get reply :error))
	  (should-not (plist-get reply :started)))
	(test-mevedel-execution--wait
	 (lambda () (not (mevedel-execution-service-live-p service))))
	(should-not (mevedel-eval-sandbox-any-live-p session))
	(should-not (mevedel-eval-sandbox--entry-timer entry))
	(should (plist-get (test-mevedel-eval-sandbox--call session root "t") :state-lost)))))
  :doc "directory and writable-root changes restart; mode changes do not"
  (progn
    (test-mevedel-eval-sandbox--require-backend)
    (test-mevedel-eval-sandbox--call session root "(setq sandbox-state 7)")
    (setf (mevedel-session-permission-mode session) 'full-auto)
    (should-not (plist-get (test-mevedel-eval-sandbox--call session root "sandbox-state") :started))
    (let ((sub (file-name-concat root "sub")) reply)
      (make-directory sub)
      (mevedel-eval-sandbox-call session "/root" "(boundp 'sandbox-state)"
				 (lambda (value) (setq reply value))
				 :workdir sub :writable-roots (list root))
      (test-mevedel-execution--wait (lambda () reply) 10)
      (should (plist-get reply :state-lost))
      (should (equal "nil" (plist-get reply :value)))
      (setq reply nil)
      (mevedel-eval-sandbox-call session "/root" "t" (lambda (value) (setq reply value))
				 :workdir sub :writable-roots (list root sub))
      (test-mevedel-execution--wait (lambda () reply) 10)
      (should (plist-get reply :state-lost))))
  :doc "unexpected death settles the call and the next call starts cleanly"
  (progn
    (test-mevedel-eval-sandbox--require-backend)
    (test-mevedel-eval-sandbox--call session root "t")
    (should (plist-get (test-mevedel-eval-sandbox--call session root "(kill-emacs 9)") :error))
    (should (plist-get (test-mevedel-eval-sandbox--call session root "t") :state-lost)))
  :doc "session and global teardown remove idle services and deadlines"
  (progn
    (test-mevedel-eval-sandbox--require-backend)
    (let ((mevedel-eval-sandbox-idle-timeout 600))
      (dolist (teardown (list (lambda () (mevedel-execution-stop-services session))
                              (lambda () (mevedel-execution-stop-all-user session))
			      (lambda () (mevedel-execution-teardown-session session))
			      #'mevedel-execution-teardown-all))
	(test-mevedel-eval-sandbox--call session root "t")
	(let* ((entry (mevedel-eval-sandbox--entry session "/root"))
	       (service (mevedel-eval-sandbox--entry-service entry)))
	  (funcall teardown)
	  (should-not (mevedel-execution-service-live-p service))
	  (should-not (mevedel-eval-sandbox--entry-service entry))
	  (should-not (mevedel-eval-sandbox--entry-timer entry))))))
  :doc "cancellation and malformed protocol kill the service and settle once"
  (progn
    (test-mevedel-eval-sandbox--require-backend)
    (test-mevedel-eval-sandbox--call session root "t")
    (let ((calls 0) reply)
      (let ((cancel (mevedel-eval-sandbox-call
                     session "/root" "(sleep-for 30)"
                     (lambda (value) (cl-incf calls) (setq reply value))
                     :workdir root :writable-roots (list root))))
        (funcall cancel)
        (funcall cancel))
      (should (= calls 1))
      (should (plist-get reply :error)))
    (should (plist-get
             (test-mevedel-eval-sandbox--call
              session root "(progn (princ \"\\036(bad-id)\\n\" 'external-debugging-output) (sleep-for 30))")
             :error))
    (should (plist-get (test-mevedel-eval-sandbox--call session root "t") :state-lost)))
  :doc "unavailable backend refuses even with Full Access and confinement off"
  (progn
    (setf (mevedel-session-permission-mode session) 'full-auto)
    (cl-letf (((symbol-function 'mevedel-sandbox-probe)
	       (lambda (&optional _) '(:available nil :reason "test unavailable"))))
      (let ((reply (test-mevedel-eval-sandbox--call session root "t")))
        (should (string-match-p "Use fresh Eval" (plist-get reply :error)))
        (should-not (string-match-p "(error\\|definitions are gone" (plist-get reply :error)))
        (should-not (mevedel-execution-session-live-p session))
        (should-not (mevedel-eval-sandbox--entry-lost
                     (mevedel-eval-sandbox--entry session "/root")))))
    (test-mevedel-eval-sandbox--require-backend)
    (should-not (plist-get (test-mevedel-eval-sandbox--call session root "t") :state-lost)))
  :doc "a crash loses definitions even before the first reply arrives"
  (progn
    (test-mevedel-eval-sandbox--require-backend)
    (let ((reply (test-mevedel-eval-sandbox--call
                  session root "(progn (setq before-crash 42) (kill-emacs 9))")))
      (should (plist-get reply :error))
      (should (string-match-p "definitions are gone" (plist-get reply :error))))
    (let ((reply (test-mevedel-eval-sandbox--call session root "(boundp 'before-crash)")))
      (should (equal "nil" (plist-get reply :value)))
      (should (plist-get reply :state-lost))))
  :doc "request IDs are fresh and absent from the evaluation environment and reader advice"
  (progn
    (test-mevedel-eval-sandbox--require-backend)
    (should (equal "nil" (plist-get (test-mevedel-eval-sandbox--call
                                    session root "(boundp 'request)") :value)))
    (test-mevedel-eval-sandbox--call
     session root
     "(progn (defvar request nil) (setq captured-request nil)
       (advice-add 'read-from-minibuffer :filter-return
                   (lambda (s) (setq captured-request s) s)))")
    (let* ((entry (mevedel-eval-sandbox--entry session "/root"))
           (id (mevedel-eval-sandbox--entry-request-id entry)))
      (should (stringp id))
      (should (string-match-p "\\`2:.+" id))
      (should (equal "(nil nil)"
                     (plist-get (test-mevedel-eval-sandbox--call
                                 session root "(list request captured-request)")
                                :value)))
      (should-not (equal id (mevedel-eval-sandbox--entry-request-id entry))))))

(mevedel-deftest mevedel-eval-sandbox--receive ()
  (test-mevedel-eval-sandbox--fixture
   (lambda (session root)
     (test-mevedel-eval-sandbox--require-backend)
     (test-mevedel-eval-sandbox--call session root "t")
     (let* ((entry (mevedel-eval-sandbox--entry session "/root"))
            (service (mevedel-eval-sandbox--entry-service entry))
            (calls 0) reply)
       (mevedel-eval-sandbox-call
        session "/root" "(sleep-for 30)"
        (lambda (value) (cl-incf calls) (setq reply value))
        :workdir root :writable-roots (list root))
       ,test)))
  (test)
  :doc "rebuilds all string fields without display, help or modification properties"
  (let* ((text (propertize "x" 'display '(when (error "Host display executed") . "y")
                           'help-echo '(lambda (&rest _) (error "Host help executed"))
                           'modification-hooks '((lambda (&rest _) (error "Host hook executed")))))
         (frame (list (mevedel-eval-sandbox--entry-request-id entry)
                      :value text :output text :error text :messages text :backtrace text :features 0)))
    (mevedel-eval-sandbox--receive entry service (concat "\036" (prin1-to-string frame) "\n"))
    (should (= 1 calls))
    (dolist (key '(:value :output :error :messages :backtrace))
      (should (equal-including-properties "x" (plist-get reply key))))
    (should (= 0 (plist-get reply :features)))
    (mevedel-execution-service-stop service))
  :doc "guessed IDs do not settle a call or clear its active state"
  (progn
    (dolist (id '(2 "2:guessed"))
      (mevedel-eval-sandbox--receive
       entry service
       (concat "\036" (prin1-to-string (list id :value "forged" :output nil :error nil
                                           :messages nil :backtrace nil :features 1)) "\n"))
      (should (= 0 calls))
      (should-not reply)
      (should (mevedel-execution-owner-live-p session "/root")))
    (mevedel-execution-service-stop service)
    (should (= 1 calls))
    (should (plist-get reply :error)))
  :doc "invalid typed fields settle as an error and kill the service"
  (progn
    (mevedel-eval-sandbox--receive
     entry service
     (concat "\036" (prin1-to-string
                     (list (mevedel-eval-sandbox--entry-request-id entry)
                           :value [bad] :output nil :error nil
                           :messages nil :backtrace nil :features 1)) "\n"))
    (should (= 1 calls))
    (should (string-match-p "Invalid sandbox reply field" (plist-get reply :error)))
    (should-not (mevedel-execution-service-live-p service)))
  :doc "a reply over 1 MiB settles once as an error and kills the service"
  (progn
    (mevedel-eval-sandbox--receive entry service (make-string (* 1024 1024) ?x))
    (should (= 0 calls))
    (mevedel-eval-sandbox--receive entry service "x")
    (should (= 1 calls))
    (should (string-match-p "exceeds 1 MiB" (plist-get reply :error)))
    (should-not (mevedel-execution-service-live-p service))
    (should-not (mevedel-eval-sandbox--entry-timer entry))))

(mevedel-deftest mevedel-eval-sandbox--reply ()
  (let ((entry (mevedel-eval-sandbox--entry-create :request-id "nonce" :callback #'ignore)))
    ,test)
  (test)
  :doc "rejects malformed, unknown, duplicate and incorrectly typed fields"
  (dolist (fields '((:value 42 :output nil :error nil :features 1)
                    (:value nil :output (lambda () t) :error nil :features 1)
                    (:value nil :output nil :error [bad] :features 1)
                    (:value nil :output nil :error nil :features -1)
                    (:value nil :output nil :error nil :features "1")
                    (:value nil :output nil :error nil :features 1 :extra t)
                    (:value nil :output nil :error nil :extra 1)
                    (:value nil :value nil :error nil :features 1)))
    (should-error (mevedel-eval-sandbox--reply
                   entry (prin1-to-string (cons "nonce" (append fields '(:messages nil :backtrace nil))))))))

(mevedel-deftest mevedel-eval-sandbox--arm-timer ()
  (test-mevedel-eval-sandbox--fixture
   (lambda (session root) ,test))
  (test)
  :doc "a cancelled TRAMP-held deadline cannot kill an active or subsequently idle service"
  (progn
    (test-mevedel-eval-sandbox--require-backend)
    (test-mevedel-eval-sandbox--call session root "t")
    (let* ((entry (mevedel-eval-sandbox--entry session "/root"))
           (service (mevedel-eval-sandbox--entry-service entry))
           (mevedel-eval-sandbox-idle-timeout 600)
           (mevedel-transport--held-timers nil)
           reply)
      (cl-letf (((symbol-function 'mevedel-transport-nested-p) (lambda () t)))
        (mevedel-eval-sandbox--arm-timer entry service))
      (let ((old (mevedel-eval-sandbox--entry-timer entry)))
        (should (mevedel-transport-held-timer-p old))
        (mevedel-eval-sandbox-call
         session "/root" "t" (lambda (value) (setq reply value))
         :workdir root :writable-roots (list root))
        (should-not (mevedel-eval-sandbox--entry-timer entry))
        (apply (timer--function old) (timer--args old))
        (should-not reply)
        (should (mevedel-execution-owner-live-p session "/root"))
        (test-mevedel-execution--wait (lambda () reply))
        (should-not (plist-get reply :error))
        (apply (timer--function old) (timer--args old))
        (should (mevedel-execution-service-live-p service))))))

(mevedel-deftest mevedel-execution-start-service ()
  (test-mevedel-eval-sandbox--fixture
   (lambda (session root) ,test))
  (test)
  :doc "writable pipe has no spool, PTY or periodic watch and stays confined"
  (progn
    (test-mevedel-eval-sandbox--require-backend)
    (let (service output closed)
      (setq service
	    (mevedel-execution-start-service
	     (lambda (handle)
	       (mevedel-execution-service-write handle "hello\n")
	       (mevedel-execution-service-set-active handle nil))
	     (lambda (_handle chunk) (setq output (concat output chunk)))
	     (lambda (facts) (setq closed facts))
	     :session session :owner "/root" :command '("cat")
	     :workdir root :writable-roots (list root)))
      (test-mevedel-execution--wait (lambda () (and output (string-match-p "hello" output))))
      (let ((child (mevedel-execution--record-child service)))
	(should-not (mevedel-execution-process-spool-path child))
	(should-not (mevedel-execution-process--child-watch-timer child))
	(should (mevedel-execution-process--child-confined-p child))
	(should-not (plist-get (mevedel-execution-process-status child) :tty-p)))
      (mevedel-execution-service-stop service)
      (should closed)
      (should-not (mevedel-execution-service-live-p service))))
  :doc "cancels pending preparation once without launching a child"
  (dolist (reentrant '(nil t))
    (let ((cancelled 0) (closed 0) service ready)
      ;; Hold the discovery seam; real confined launches are covered above.
      (cl-letf (((symbol-function 'mevedel-sandbox-prepare-start)
		 (lambda (callback &rest _)
                   (when reentrant
                     (mevedel-execution-stop-services session)
                     (funcall callback '(:state confined :command ("cat"))))
                   (lambda () (cl-incf cancelled)))))
	(setq service
              (mevedel-execution-start-service
               (lambda (_) (setq ready t)) #'ignore
               (lambda (_) (cl-incf closed))
               :session session :owner "/root/agent" :command '("cat")
               :workdir root :writable-roots (list root)))
	(unless reentrant
          (should (mevedel-execution-working-p))
          (should (mevedel-execution-owner-live-p session "/root/agent"))
          (should (mevedel-execution-session-live-p session)))
	(mevedel-execution-stop-all-user session)
	(mevedel-execution-service-stop service)
	(should (= 1 cancelled closed))
        (should-not ready)
        (should-not (mevedel-execution-process-launch-attempted-p
                     (mevedel-execution--record-child service)))
	(should-not (mevedel-execution-service-live-p service))))))

(mevedel-deftest mevedel-execution-start-service-call ()
  (test-mevedel-eval-sandbox--fixture
   (lambda (session root) ,test))
  (test)
  :doc "preparation stays foreground; abort or user stop prevents launch without invented loss"
  (dolist (stop '(nil t))
   (let (prepared call service observation (cancelled 0) (delivered 0))
    (cl-letf (((symbol-function 'mevedel-sandbox-prepare-start)
               (lambda (callback &rest _)
                 (setq prepared callback)
                 (lambda () (cl-incf cancelled)))))
      (setq call
            (mevedel-execution-start-service-call
             (lambda (value) (setq observation value) (cl-incf delivered))
             (lambda (handle)
               (mevedel-eval-sandbox-call
                session "/root" "t"
                (lambda (reply) (mevedel-execution-service-call-settle
                                 handle (list :output (plist-get reply :error) :outcome 'failure)))
                :call handle :workdir root :writable-roots (list root)))
             :session session :owner "/root" :workdir root :yield-time-ms 1)
            service (mevedel-execution--record-service call))
      (should prepared)
      (should-not (mevedel-execution--record-yield-timer call))
      (sleep-for 0.02)
      (should-not (mevedel-execution--record-yielded-p call))
      (should (= 0 delivered))
      (if stop
          (mevedel-execution-stop-user session (mevedel-execution--record-execution-id call))
        (mevedel-execution--abort-request-record call))
      (funcall prepared '(:state confined :command ("cat")))
      (should (= 1 cancelled delivered))
      (should (eq (if stop 'stopped 'aborted) (mevedel-execution--record-termination call)))
      (should-not (string-search "definitions are gone" (plist-get observation :result)))
      (should-not (mevedel-execution-service-live-p service))
      (should-not (mevedel-execution-process-launch-attempted-p
                   (mevedel-execution--record-child service)))))))

(provide 'test-mevedel-eval-sandbox)
;;; test-mevedel-eval-sandbox.el ends here
