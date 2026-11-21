;;; test-niwa-eval-sandbox.el --- Sandbox service lifecycle tests -*- lexical-binding: t -*-

;;; Commentary:

;; Real confined child Emacsen, through the module and execution facade APIs.

;;; Code:

(require 'niwa-eval-sandbox)
(require 'niwa-execution)
(require 'niwa-execution-process)
(require 'niwa-sandbox)
(require 'niwa-transport)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file)) "helpers"))
(require 'niwa-execution-test-helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "niwa-execution-test-helpers"))

(defun test-niwa-eval-sandbox--fixture (function)
  "Call FUNCTION with an isolated real session and writable root."
  (let* ((root (file-name-as-directory (make-temp-file "eval-sandbox-" t)))
         (session (test-niwa-execution--session root))
         (niwa-eval-sandbox--sessions (make-hash-table :test #'eq))
         (niwa-eval-sandbox-idle-timeout nil))
    (unwind-protect
        (funcall function session root)
      (niwa-execution-teardown-session session)
      (delete-directory root t))))

(defun test-niwa-eval-sandbox--call (session root expression &rest keys)
  "Evaluate EXPRESSION in SESSION at ROOT with optional KEYS and await reply."
  (let (result)
    (apply #'niwa-eval-sandbox-call session "/root" expression
           (lambda (reply) (setq result reply))
           :workdir root :writable-roots (list root) keys)
    (test-niwa-execution--wait (lambda () result) 10)
    result))

(defun test-niwa-eval-sandbox--require-backend ()
  "Skip a real-child case only if the confinement backend is unavailable."
  (let ((probe (niwa-sandbox-probe)))
    (unless (plist-get probe :available) (ert-skip (plist-get probe :reason)))))

(niwa-deftest niwa-eval-sandbox-call ()
  (test-niwa-eval-sandbox--fixture
   (lambda (session root) ,test))
  (test)
  :doc "persists definitions, resets, and isolates retained owner paths"
  (progn
    (test-niwa-eval-sandbox--require-backend)
    (should (plist-get (test-niwa-eval-sandbox--call
			session root "(defun sandbox-test-value () 42)") :started))
    (let ((reply (test-niwa-eval-sandbox--call session root "(sandbox-test-value)")))
      (should (equal "42" (plist-get reply :value)))
      (should-not (plist-get reply :started))
      (should (> (plist-get reply :features) 0)))
    (let (reply)
      (niwa-eval-sandbox-call
       session "/root/agent" "(progn (setq agent-state 9) (fboundp 'sandbox-test-value))"
       (lambda (value) (setq reply value)) :workdir root :writable-roots (list root))
      (test-niwa-execution--wait (lambda () reply) 10)
      (should (equal "nil" (plist-get reply :value)))
      (should (= 0 (niwa-execution-stop-owner session "/root/agent")))
      (setq reply nil)
      (niwa-eval-sandbox-call
       session "/root/agent" "agent-state" (lambda (value) (setq reply value))
       :workdir root :writable-roots (list root))
      (test-niwa-execution--wait (lambda () reply) 10)
      (should (equal "9" (plist-get reply :value))))
    (let ((reply (test-niwa-eval-sandbox--call
		  session root "(fboundp 'sandbox-test-value)" :reset t)))
      (should (equal "nil" (plist-get reply :value)))
      (should (plist-get reply :started))
      (should (plist-get reply :state-lost)))
    (should-not (plist-get (test-niwa-eval-sandbox--call session root "t") :state-lost))
    (let ((rows (niwa-eval-sandbox-list-idle session nil)))
      (should (= 2 (length rows)))
      (niwa-eval-sandbox-stop-user session (car rows))
      (should (niwa-eval-sandbox-any-live-p session))
      (niwa-eval-sandbox-stop-user session (cadr rows))
      (should-not (niwa-eval-sandbox-any-live-p session))))
  :doc "idle is inert; owner finalization keeps idle services but stops active calls"
  (progn
    (test-niwa-eval-sandbox--require-backend)
    (test-niwa-eval-sandbox--call session root "(setq sandbox-state 7)")
    (should-not (niwa-execution-working-p))
    (should-not (niwa-execution-owner-live-p session "/root"))
    (should-not (niwa-execution-session-live-p session))
    (should-not (niwa-execution-list session "/root"))
    (should-not (niwa-execution-list-user session))
    (should (= 0 (niwa-execution-stop-owner session "/root")))
    (should (equal "7" (plist-get (test-niwa-eval-sandbox--call
				   session root "sandbox-state") :value)))
    (let (reply)
      (niwa-eval-sandbox-call session "/root" "(sleep-for 30)"
				 (lambda (value) (setq reply value))
				 :workdir root :writable-roots (list root))
      (should (niwa-execution-working-p))
      (should (niwa-execution-owner-live-p session "/root"))
      (should (niwa-execution-session-live-p session))
      (should-error (test-niwa-eval-sandbox--call session root "t" :reset t))
      (niwa-execution-stop-owner session "/root")
      (should (plist-get reply :error))
      (should-not (niwa-execution-session-live-p session))))
  :doc "arms expiry only after settlement and removes it on death"
  (progn
    (test-niwa-eval-sandbox--require-backend)
    (should (= 0 (hash-table-count niwa-eval-sandbox--sessions)))
    (let ((niwa-eval-sandbox-idle-timeout 0.1))
      (test-niwa-eval-sandbox--call session root "t")
      (let* ((entry (niwa-eval-sandbox--entry session "/root"))
	     (service (niwa-eval-sandbox--entry-service entry)))
	(should (timerp (niwa-eval-sandbox--entry-timer entry)))
	(let (reply)
	  (niwa-eval-sandbox-call
	   session "/root" "(sleep-for 0.2)" (lambda (value) (setq reply value))
	   :workdir root :writable-roots (list root))
	  (should-not (niwa-eval-sandbox--entry-timer entry))
	  (test-niwa-execution--wait (lambda () reply))
	  (should-not (plist-get reply :error))
	  (should-not (plist-get reply :started)))
	(test-niwa-execution--wait
	 (lambda () (not (niwa-execution-service-live-p service))))
	(should-not (niwa-eval-sandbox-any-live-p session))
	(should-not (niwa-eval-sandbox--entry-timer entry))
	(should (plist-get (test-niwa-eval-sandbox--call session root "t") :state-lost)))))
  :doc "directory and writable-root changes restart; mode changes do not"
  (progn
    (test-niwa-eval-sandbox--require-backend)
    (test-niwa-eval-sandbox--call session root "(setq sandbox-state 7)")
    (setf (niwa-session-permission-mode session) 'full-auto)
    (should-not (plist-get (test-niwa-eval-sandbox--call session root "sandbox-state") :started))
    (let ((sub (file-name-concat root "sub")) reply)
      (make-directory sub)
      (niwa-eval-sandbox-call session "/root" "(boundp 'sandbox-state)"
				 (lambda (value) (setq reply value))
				 :workdir sub :writable-roots (list root))
      (test-niwa-execution--wait (lambda () reply) 10)
      (should (plist-get reply :state-lost))
      (should (equal "nil" (plist-get reply :value)))
      (setq reply nil)
      (niwa-eval-sandbox-call session "/root" "t" (lambda (value) (setq reply value))
				 :workdir sub :writable-roots (list root sub))
      (test-niwa-execution--wait (lambda () reply) 10)
      (should (plist-get reply :state-lost))))
  :doc "unexpected death settles the call and the next call starts cleanly"
  (progn
    (test-niwa-eval-sandbox--require-backend)
    (test-niwa-eval-sandbox--call session root "t")
    (should (plist-get (test-niwa-eval-sandbox--call session root "(kill-emacs 9)") :error))
    (should (plist-get (test-niwa-eval-sandbox--call session root "t") :state-lost)))
  :doc "session and global teardown remove idle services and deadlines"
  (progn
    (test-niwa-eval-sandbox--require-backend)
    (let ((niwa-eval-sandbox-idle-timeout 600))
      (dolist (teardown (list (lambda () (niwa-execution-stop-services session))
                              (lambda () (niwa-execution-stop-all-user session))
			      (lambda () (niwa-execution-teardown-session session))
			      #'niwa-execution-teardown-all))
	(test-niwa-eval-sandbox--call session root "t")
	(let* ((entry (niwa-eval-sandbox--entry session "/root"))
	       (service (niwa-eval-sandbox--entry-service entry)))
	  (funcall teardown)
	  (should-not (niwa-execution-service-live-p service))
	  (should-not (niwa-eval-sandbox--entry-service entry))
	  (should-not (niwa-eval-sandbox--entry-timer entry))))))
  :doc "cancellation and malformed protocol kill the service and settle once"
  (progn
    (test-niwa-eval-sandbox--require-backend)
    (test-niwa-eval-sandbox--call session root "t")
    (let ((calls 0) reply)
      (let ((cancel (niwa-eval-sandbox-call
                     session "/root" "(sleep-for 30)"
                     (lambda (value) (cl-incf calls) (setq reply value))
                     :workdir root :writable-roots (list root))))
        (funcall cancel)
        (funcall cancel))
      (should (= calls 1))
      (should (plist-get reply :error)))
    (should (plist-get
             (test-niwa-eval-sandbox--call
              session root "(progn (princ \"\\036(bad-id)\\n\" 'external-debugging-output) (sleep-for 30))")
             :error))
    (should (plist-get (test-niwa-eval-sandbox--call session root "t") :state-lost)))
  :doc "unavailable backend refuses even with Full Access and confinement off"
  (progn
    (setf (niwa-session-permission-mode session) 'full-auto)
    (cl-letf (((symbol-function 'niwa-sandbox-probe)
	       (lambda (&optional _) '(:available nil :reason "test unavailable"))))
      (let ((reply (test-niwa-eval-sandbox--call session root "t")))
        (should (string-match-p "Use fresh Eval" (plist-get reply :error)))
        (should-not (string-match-p "(error\\|definitions are gone" (plist-get reply :error)))
        (should-not (niwa-execution-session-live-p session))
        (should-not (niwa-eval-sandbox--entry-lost
                     (niwa-eval-sandbox--entry session "/root")))))
    (test-niwa-eval-sandbox--require-backend)
    (should-not (plist-get (test-niwa-eval-sandbox--call session root "t") :state-lost)))
  :doc "a crash loses definitions even before the first reply arrives"
  (progn
    (test-niwa-eval-sandbox--require-backend)
    (let ((reply (test-niwa-eval-sandbox--call
                  session root "(progn (setq before-crash 42) (kill-emacs 9))")))
      (should (plist-get reply :error))
      (should (string-match-p "definitions are gone" (plist-get reply :error))))
    (let ((reply (test-niwa-eval-sandbox--call session root "(boundp 'before-crash)")))
      (should (equal "nil" (plist-get reply :value)))
      (should (plist-get reply :state-lost))))
  :doc "request IDs are fresh and absent from the evaluation environment and reader advice"
  (progn
    (test-niwa-eval-sandbox--require-backend)
    (should (equal "nil" (plist-get (test-niwa-eval-sandbox--call
                                    session root "(boundp 'request)") :value)))
    (test-niwa-eval-sandbox--call
     session root
     "(progn (defvar request nil) (setq captured-request nil)
       (advice-add 'read-from-minibuffer :filter-return
                   (lambda (s) (setq captured-request s) s)))")
    (let* ((entry (niwa-eval-sandbox--entry session "/root"))
           (id (niwa-eval-sandbox--entry-request-id entry)))
      (should (stringp id))
      (should (string-match-p "\\`2:.+" id))
      (should (equal "(nil nil)"
                     (plist-get (test-niwa-eval-sandbox--call
                                 session root "(list request captured-request)")
                                :value)))
      (should-not (equal id (niwa-eval-sandbox--entry-request-id entry))))))

(niwa-deftest niwa-eval-sandbox--receive ()
  (test-niwa-eval-sandbox--fixture
   (lambda (session root)
     (test-niwa-eval-sandbox--require-backend)
     (test-niwa-eval-sandbox--call session root "t")
     (let* ((entry (niwa-eval-sandbox--entry session "/root"))
            (service (niwa-eval-sandbox--entry-service entry))
            (calls 0) reply)
       (niwa-eval-sandbox-call
        session "/root" "(sleep-for 30)"
        (lambda (value) (cl-incf calls) (setq reply value))
        :workdir root :writable-roots (list root))
       ,test)))
  (test)
  :doc "rebuilds all string fields without display, help or modification properties"
  (let* ((text (propertize "x" 'display '(when (error "Host display executed") . "y")
                           'help-echo '(lambda (&rest _) (error "Host help executed"))
                           'modification-hooks '((lambda (&rest _) (error "Host hook executed")))))
         (frame (list (niwa-eval-sandbox--entry-request-id entry)
                      :value text :output text :error text :messages text :backtrace text :features 0)))
    (niwa-eval-sandbox--receive entry service (concat "\036" (prin1-to-string frame) "\n"))
    (should (= 1 calls))
    (dolist (key '(:value :output :error :messages :backtrace))
      (should (equal-including-properties "x" (plist-get reply key))))
    (should (= 0 (plist-get reply :features)))
    (niwa-execution-service-stop service))
  :doc "guessed IDs do not settle a call or clear its active state"
  (progn
    (dolist (id '(2 "2:guessed"))
      (niwa-eval-sandbox--receive
       entry service
       (concat "\036" (prin1-to-string (list id :value "forged" :output nil :error nil
                                           :messages nil :backtrace nil :features 1)) "\n"))
      (should (= 0 calls))
      (should-not reply)
      (should (niwa-execution-owner-live-p session "/root")))
    (niwa-execution-service-stop service)
    (should (= 1 calls))
    (should (plist-get reply :error)))
  :doc "invalid typed fields settle as an error and kill the service"
  (progn
    (niwa-eval-sandbox--receive
     entry service
     (concat "\036" (prin1-to-string
                     (list (niwa-eval-sandbox--entry-request-id entry)
                           :value [bad] :output nil :error nil
                           :messages nil :backtrace nil :features 1)) "\n"))
    (should (= 1 calls))
    (should (string-match-p "Invalid sandbox reply field" (plist-get reply :error)))
    (should-not (niwa-execution-service-live-p service)))
  :doc "a reply over 1 MiB settles once as an error and kills the service"
  (progn
    (niwa-eval-sandbox--receive entry service (make-string (* 1024 1024) ?x))
    (should (= 0 calls))
    (niwa-eval-sandbox--receive entry service "x")
    (should (= 1 calls))
    (should (string-match-p "exceeds 1 MiB" (plist-get reply :error)))
    (should-not (niwa-execution-service-live-p service))
    (should-not (niwa-eval-sandbox--entry-timer entry))))

(niwa-deftest niwa-eval-sandbox--reply ()
  (let ((entry (niwa-eval-sandbox--entry-create :request-id "nonce" :callback #'ignore)))
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
    (should-error (niwa-eval-sandbox--reply
                   entry (prin1-to-string (cons "nonce" (append fields '(:messages nil :backtrace nil))))))))

(niwa-deftest niwa-eval-sandbox--arm-timer ()
  (test-niwa-eval-sandbox--fixture
   (lambda (session root) ,test))
  (test)
  :doc "a cancelled TRAMP-held deadline cannot kill an active or subsequently idle service"
  (progn
    (test-niwa-eval-sandbox--require-backend)
    (test-niwa-eval-sandbox--call session root "t")
    (let* ((entry (niwa-eval-sandbox--entry session "/root"))
           (service (niwa-eval-sandbox--entry-service entry))
           (niwa-eval-sandbox-idle-timeout 600)
           (niwa-transport--held-timers nil)
           reply)
      (cl-letf (((symbol-function 'niwa-transport-nested-p) (lambda () t)))
        (niwa-eval-sandbox--arm-timer entry service))
      (let ((old (niwa-eval-sandbox--entry-timer entry)))
        (should (niwa-transport-held-timer-p old))
        (niwa-eval-sandbox-call
         session "/root" "t" (lambda (value) (setq reply value))
         :workdir root :writable-roots (list root))
        (should-not (niwa-eval-sandbox--entry-timer entry))
        (apply (timer--function old) (timer--args old))
        (should-not reply)
        (should (niwa-execution-owner-live-p session "/root"))
        (test-niwa-execution--wait (lambda () reply))
        (should-not (plist-get reply :error))
        (apply (timer--function old) (timer--args old))
        (should (niwa-execution-service-live-p service))))))

(niwa-deftest niwa-execution-start-service ()
  (test-niwa-eval-sandbox--fixture
   (lambda (session root) ,test))
  (test)
  :doc "writable pipe has no spool, PTY or periodic watch and stays confined"
  (progn
    (test-niwa-eval-sandbox--require-backend)
    (let (service output closed)
      (setq service
	    (niwa-execution-start-service
	     (lambda (handle)
	       (niwa-execution-service-write handle "hello\n")
	       (niwa-execution-service-set-active handle nil))
	     (lambda (_handle chunk) (setq output (concat output chunk)))
	     (lambda (facts) (setq closed facts))
	     :session session :owner "/root" :command '("cat")
	     :workdir root :writable-roots (list root)))
      (test-niwa-execution--wait (lambda () (and output (string-match-p "hello" output))))
      (let ((child (niwa-execution--record-child service)))
	(should-not (niwa-execution-process-spool-path child))
	(should-not (niwa-execution-process--child-watch-timer child))
	(should (niwa-execution-process--child-confined-p child))
	(should-not (plist-get (niwa-execution-process-status child) :tty-p)))
      (niwa-execution-service-stop service)
      (should closed)
      (should-not (niwa-execution-service-live-p service))))
  :doc "cancels pending preparation once without launching a child"
  (dolist (reentrant '(nil t))
    (let ((cancelled 0) (closed 0) service ready)
      ;; Hold the discovery seam; real confined launches are covered above.
      (cl-letf (((symbol-function 'niwa-sandbox-prepare-start)
		 (lambda (callback &rest _)
                   (when reentrant
                     (niwa-execution-stop-services session)
                     (funcall callback '(:state confined :command ("cat"))))
                   (lambda () (cl-incf cancelled)))))
	(setq service
              (niwa-execution-start-service
               (lambda (_) (setq ready t)) #'ignore
               (lambda (_) (cl-incf closed))
               :session session :owner "/root/agent" :command '("cat")
               :workdir root :writable-roots (list root)))
	(unless reentrant
          (should (niwa-execution-working-p))
          (should (niwa-execution-owner-live-p session "/root/agent"))
          (should (niwa-execution-session-live-p session)))
	(niwa-execution-stop-all-user session)
	(niwa-execution-service-stop service)
	(should (= 1 cancelled closed))
        (should-not ready)
        (should-not (niwa-execution-process-launch-attempted-p
                     (niwa-execution--record-child service)))
	(should-not (niwa-execution-service-live-p service))))))

(niwa-deftest niwa-execution-start-service-call ()
  (test-niwa-eval-sandbox--fixture
   (lambda (session root) ,test))
  (test)
  :doc "preparation stays foreground; abort or user stop prevents launch without invented loss"
  (dolist (stop '(nil t))
   (let (prepared call service observation (cancelled 0) (delivered 0))
    (cl-letf (((symbol-function 'niwa-sandbox-prepare-start)
               (lambda (callback &rest _)
                 (setq prepared callback)
                 (lambda () (cl-incf cancelled)))))
      (setq call
            (niwa-execution-start-service-call
             (lambda (value) (setq observation value) (cl-incf delivered))
             (lambda (handle)
               (niwa-eval-sandbox-call
                session "/root" "t"
                (lambda (reply) (niwa-execution-service-call-settle
                                 handle (list :output (plist-get reply :error) :outcome 'failure)))
                :call handle :workdir root :writable-roots (list root)))
             :session session :owner "/root" :workdir root :yield-time-ms 1)
            service (niwa-execution--record-service call))
      (should prepared)
      (should-not (niwa-execution--record-yield-timer call))
      (sleep-for 0.02)
      (should-not (niwa-execution--record-yielded-p call))
      (should (= 0 delivered))
      (if stop
          (niwa-execution-stop-user session (niwa-execution--record-execution-id call))
        (niwa-execution--abort-request-record call))
      (funcall prepared '(:state confined :command ("cat")))
      (should (= 1 cancelled delivered))
      (should (eq (if stop 'stopped 'aborted) (niwa-execution--record-termination call)))
      (should-not (string-search "definitions are gone" (plist-get observation :result)))
      (should-not (niwa-execution-service-live-p service))
      (should-not (niwa-execution-process-launch-attempted-p
                   (niwa-execution--record-child service)))))))

(provide 'test-niwa-eval-sandbox)
;;; test-niwa-eval-sandbox.el ends here
