;;; test-niwa-session-durability-heartbeat.el --- Nonblocking lease timer -*- lexical-binding: t -*-

;;; Commentary:

;; Exercise the installed timer callback against real temporary lease files
;; and real control children.  Explicit synchronous renewal is a separate API.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'niwa-session-persistence)
(require 'niwa-session-control-fs)
(require 'niwa-session-durability)
(require 'niwa-session-publication)
(require 'niwa-structs)
(require 'niwa-workspace)
(require 'helpers (file-name-concat (file-name-directory
                                     (or load-file-name buffer-file-name)) "helpers"))

(defmacro test-niwa-heartbeat--with-session (&rest body)
  "Run BODY with a temporary portable SESSION and its installed TICK."
  (declare (indent 0))
  `(let* ((root (file-name-as-directory (make-temp-file "niwa-heartbeat-" t)))
          (session (niwa-session-create
                    "heartbeat" (niwa-workspace--create
                                 :type 'project :id root :root root :name "local")))
          (niwa-session-durability--client-id (make-string 64 ?a))
          tick)
     (setf (niwa-session-save-path session) root
           (niwa-session-session-id session) "heartbeat")
     (unwind-protect
         (progn
           (should (niwa-session-durability-lease-acquire root "*heartbeat*" session))
           (setq tick (timer--function (niwa-session-lease-renewal-timer session)))
           (niwa-session-durability--cancel-renewal session)
           ,@body)
       (niwa-session-control-fs-wait-async 5)
       (niwa-session-durability--cancel-renewal session)
       (ignore-errors (niwa-session-durability-lease-release root session))
       (delete-directory root t))))

(defun test-niwa-heartbeat--await (session)
  "Pump events until SESSION's heartbeat retires, failing after five seconds."
  (let ((deadline (+ (float-time) 5)))
    (while (and (niwa-session-lease-renewal-job session) (< (float-time) deadline))
      (accept-process-output nil 0.01))
    (should-not (niwa-session-lease-renewal-job session))))

(defun test-niwa-heartbeat--expire (session)
  "Age SESSION's real record without changing its ownership or lease window."
  (let* ((lease (niwa-session-lease session))
         (record (car (read-from-string (plist-get lease :bytes))))
         (directory (niwa-session-durability--lease-path
                     (niwa-session-save-path session))))
    (dolist (key '(:renewed-at :expires-at))
      (plist-put record key (- (plist-get record key) 120)))
    (niwa-session-durability--write-generation directory record)
    (niwa-session-durability--bind-lease
     session record 'owned (niwa-session-durability--record-bytes record))
    (niwa-session-durability--cancel-renewal session)))

(defmacro test-niwa-heartbeat--held (&rest body)
  "Hold the first real program's reply, then run BODY with RESUME and PROGRAMS.
The callback is held after the real child completed, not replaced by a fake
filesystem.  Subsequent phases retain the ordinary carrier."
  (declare (indent 0))
  `(let ((run (symbol-function 'niwa-session-control-fs-run-program-async))
         (programs 0) captured resume)
     (cl-letf (((symbol-function 'niwa-session-control-fs-run-program-async)
                (lambda (operations callback &optional scope)
                  (cl-incf programs)
                  (funcall run operations
                           (lambda (results error)
                             (if captured (funcall callback results error)
                               (setq captured t
                                     resume (lambda ()
                                              (setq resume nil)
                                              (funcall callback results error)))))
                           scope))))
       (unwind-protect
	   (progn
	     (funcall tick session)
	     (let ((deadline (+ (float-time) 5)))
	       (while (and (not resume) (< (float-time) deadline))
		 (accept-process-output nil 0.01)))
	     (should resume)
	     ,@body)
	 (when resume (funcall resume))))))

(niwa-deftest niwa-session-durability-heartbeat ()
  ,test
  (test)
  :doc "returns before its real held child and adopts durable renewal exactly once"
  (test-niwa-heartbeat--with-session
    (let* ((release (file-name-concat root "release"))
	   (watchdog-file (file-name-concat root "watchdog"))
	   (script niwa-session-control-fs--program-script)
	   (niwa-session-control-fs--program-script
	    (concat "while [ ! -e " (shell-quote-argument release)
		    " ]; do sleep 0.01; done\n" script))
	   ;; The watchdog bounds the old synchronous callback too.  Only it
	   ;; can unblock that callback; the independent Lisp event cannot run.
	   (watchdog (make-process
		      :name "heartbeat-watchdog" :buffer nil :noquery t
		      :command (list "sh" "-c"
				     (format "sleep 1; touch %s %s"
					     (shell-quote-argument watchdog-file)
					     (shell-quote-argument release)))))
	   (bind (symbol-function 'niwa-session-durability--bind-lease))
	   (adoptions 0) event)
      (unwind-protect
	  (progn
	    (cl-letf (((symbol-function 'niwa-session-durability--bind-lease)
		       (lambda (owner &rest arguments)
			 (when (eq owner session) (cl-incf adoptions))
			 (apply bind owner arguments)))
		      ((symbol-function 'niwa-session-control-fs-run-program)
		       (lambda (&rest _) (ert-fail "Synchronous program in timer heartbeat"))))
	      (funcall tick session)
	      (should-not (file-exists-p release))
	      (setq event (run-at-time 0 nil (lambda () (write-region "" nil release nil 'silent))))
	      (niwa-session-control-fs-wait-async 5)
	      (test-niwa-heartbeat--await session))
	    (should (file-exists-p release))
	    (should-not (file-exists-p watchdog-file))
	    (should (= adoptions 1))
	    (should (niwa-session-durability-lease-owned-p session))
	    (should (equal (plist-get (niwa-session-lease session) :bytes)
			   (niwa-session-durability--record-bytes
			    (niwa-session-durability--lease-head
			     (niwa-session-durability--lease-path root))))))
	(when event (cancel-timer event))
	(delete-process watchdog)
	;; If an assertion failed before the independent event was armed,
	;; release the real child before the fixture waits for cleanup.
	(write-region "" nil release nil 'silent))))

  :doc "a second tick neither drains nor launches and the shadow owns no timer"
  (test-niwa-heartbeat--with-session
    (test-niwa-heartbeat--held
      (let* ((job (niwa-session-lease-renewal-job session))
	     (shadow (niwa-session-durability--heartbeat-shadow job)))
	(cl-letf (((symbol-function 'niwa-session-control-fs-run-program)
		   (lambda (&rest _) (ert-fail "Heartbeat drained synchronously"))))
	  (funcall tick session)
	  (should (= programs 1))
	  (should (eq job (niwa-session-lease-renewal-job session)))
	  (funcall resume)
	  (test-niwa-heartbeat--await session))
	(should-not (niwa-session-lease-renewal-timer shadow))
	(should (eq 'owned (plist-get (niwa-session-lease session) :state))))))

  :doc "a changed lease, path, buffer or cancelled timer fences every later phase"
  (dolist (change '(lease path buffer cancel))
    (test-niwa-heartbeat--with-session
      (test-niwa-heartbeat--held
	(let ((expected (copy-sequence (niwa-session-lease session))))
	  (pcase change
	    ('lease (setf (niwa-session-lease session) expected))
	    ('path (setf (niwa-session-save-path session) (concat root "moved/")))
	    ('buffer (setf (niwa-session-root-buffer session) (current-buffer)))
	    ('cancel (niwa-session-durability--cancel-renewal session)))
	  (funcall resume)
	  (test-niwa-heartbeat--await session)
	  (should (= programs 1))
	  (should (equal expected (niwa-session-lease session)))
	  (setf (niwa-session-save-path session) root
		(niwa-session-root-buffer session) nil)))))

  :doc "own expiry reclaims a successor asynchronously and preserves protocol fields"
  (test-niwa-heartbeat--with-session
    (niwa-session-durability-set-unsettled-mutation session t)
    (niwa-session-durability--cancel-renewal session)
    (test-niwa-heartbeat--expire session)
    (let ((before (niwa-session-lease session)))
      (cl-letf (((symbol-function 'niwa-session-control-fs-run-program)
		 (lambda (&rest _) (ert-fail "Expiry fell back to a synchronous acquirer"))))
	(funcall tick session)
	(test-niwa-heartbeat--await session))
      (let ((after (niwa-session-lease session)))
	(should (eq 'owned (plist-get after :state)))
	(should (= (1+ (plist-get before :generation)) (plist-get after :generation)))
	(should (plist-get after :unsettled-mutation))
	(should (equal (plist-get before :transfer-generation)
		       (plist-get after :transfer-generation)))
	(should (equal (plist-get before :publication-head)
		       (plist-get after :publication-head))))))

  :doc "a successor between observation and commit cannot be overwritten or reclaimed"
  (test-niwa-heartbeat--with-session
    (test-niwa-heartbeat--expire session)
    (test-niwa-heartbeat--held
      (let ((niwa-session-durability--client-id (make-string 64 ?b)))
	(cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
	  (should (niwa-session-durability-lease-acquire root "*successor*"))))
      (cl-letf (((symbol-function 'niwa-session-control-fs-run-program)
		 (lambda (&rest _) (ert-fail "Lost heartbeat used a synchronous program"))))
	(funcall resume)
	(test-niwa-heartbeat--await session))
      (should (eq 'lost (plist-get (niwa-session-lease session) :state)))
      (should (equal (make-string 64 ?b)
		     (plist-get (niwa-session-durability--lease-head
				 (niwa-session-durability--lease-path root)) :client-id)))))

  :doc "a named lease-update decline is a lost lease: it reclaims and is no failure"
  (test-niwa-heartbeat--with-session
    (let ((warnings nil)
          (lease (niwa-session-lease session)))
      (test-niwa-heartbeat--held
        ;; Another client takes the bound generation after the program read it.
        (let ((niwa-session-durability--client-id (make-string 64 ?b)))
          (niwa-session-durability--write-plist
           (niwa-session-durability--generation-path
            (niwa-session-durability--lease-path root)
            (plist-get lease :generation))
           (niwa-session-durability--lease-record
            "*thief*" (plist-get lease :generation) 'active nil nil
            (plist-get lease :renewed-at))))
        (cl-letf (((symbol-function 'display-warning)
                   (lambda (&rest arguments) (push arguments warnings))))
          (funcall resume)
          (test-niwa-heartbeat--await session))
        ;; The reclaim ran as its own program after the update's decline.
        (should (> programs 1)))
      (should-not warnings)
      (should (eq 'lost (plist-get (niwa-session-lease session) :state)))))

  :doc "mutation, explicit renewal and stopping drain the earlier heartbeat first"
  (dolist (operation '(latch renew release))
    (test-niwa-heartbeat--with-session
      (test-niwa-heartbeat--held
	(let ((event (run-at-time 0 nil resume)))
	  (unwind-protect
	      (pcase operation
		('latch (should (niwa-session-durability-set-unsettled-mutation session t)))
		('renew (should (niwa-session-durability-lease-renew session)))
		('release
		 (setf (niwa-session-publication-stopping-p session) t)
		 (funcall tick session)
		 (should (= programs 1))
		 (niwa-session-publication-stopping session)
		 (niwa-session-durability-lease-release root session)))
	    (cancel-timer event)))
	(should-not (niwa-session-lease-renewal-job session))
	(pcase operation
	  ('latch (should (niwa-session-durability-unsettled-mutation-p session)))
	  ('renew (should (niwa-session-durability-lease-owned-p session)))
	  ('release
	   (should-not (niwa-session-lease-renewal-timer session))
	   (should (eq 'released
		       (plist-get (niwa-session-durability--lease-head
				   (niwa-session-durability--lease-path root)) :status))))))))

  :doc "accepted publication preparation cannot enter beside a heartbeat"
  (test-niwa-heartbeat--with-session
    (test-niwa-heartbeat--held
      (let (prepared)
	(niwa-session-publication-enqueue
	 session (lambda (_token)
		   (should-not (niwa-session-lease-renewal-job session))
		   (setq prepared t)))
	(accept-process-output nil 0.02)
	(should-not prepared)
	(let ((event (run-at-time 0 nil resume)))
	  (unwind-protect (niwa-session-publication-barrier session)
	    (cancel-timer event)))
	(should prepared))))

  :doc "ticks cannot enter the control API's synchronous fallback"
  (test-niwa-heartbeat--with-session
    (cl-letf (((symbol-function 'niwa-session-control-fs-run-program-async)
	       (lambda (&rest _) (ert-fail "Unavailable carrier admitted a heartbeat")))
	      ((symbol-function 'niwa-session-control-fs--pipe-capable-p)
	       (lambda (&rest _) nil)))
      (let ((niwa-session-control-fs--async-wait t)) (funcall tick session))
      (let ((niwa-session-durability--heartbeat-inhibit t)) (funcall tick session))
      (setf (niwa-session-save-path session) "/ssh:unsupported:/tmp/session/")
      (unwind-protect (funcall tick session)
	(setf (niwa-session-save-path session) root))))

  :doc "an unexpired own claiming head is not rotated by expiry reclaim"
  (test-niwa-heartbeat--with-session
    (let* ((before (niwa-session-lease session))
	   (record (car (read-from-string (plist-get before :bytes))))
	   (directory (niwa-session-durability--lease-path root)))
      (plist-put record :status 'claiming)
      (niwa-session-durability--write-generation directory record)
      (niwa-session-durability--bind-lease
       session record 'owned (niwa-session-durability--record-bytes record))
      (niwa-session-durability--cancel-renewal session)
      (cl-letf (((symbol-function 'niwa-session-control-fs-run-program)
		 (lambda (&rest _) (ert-fail "Claiming head used synchronous reclaim"))))
	(funcall tick session)
	(test-niwa-heartbeat--await session))
      (let ((head (niwa-session-durability--lease-head directory)))
	(should (= (plist-get before :generation) (plist-get head :generation)))
	(should (eq 'claiming (plist-get head :status))))))

  :doc "fresh final verification declines authority that expired during the job"
  (test-niwa-heartbeat--with-session
    (let ((run (symbol-function 'niwa-session-control-fs-run-program-async))
	  (decode (symbol-function 'niwa-session-control-fs--program-value))
	  (programs 0) final-clock)
      (cl-letf (((symbol-function 'niwa-session-control-fs-run-program-async)
                 (lambda (operations callback &optional scope)
		   (cl-incf programs)
                   (funcall run operations callback scope)))
		((symbol-function 'niwa-session-control-fs--program-value)
		 (lambda (operation payload)
		   (let ((value (funcall decode operation payload)))
		     (if (and (>= programs 3) (eq 'target-time (plist-get operation :op)))
			 (progn (setq final-clock t) (+ value 1000))
		       value))))
		((symbol-function 'niwa-session-control-fs-run-program)
		 (lambda (&rest _) (ert-fail "Final verification blocked"))))
	(funcall tick session)
	(test-niwa-heartbeat--await session))
      (should final-clock)
      (should (eq 'lost (plist-get (niwa-session-lease session) :state)))
      (should-not (niwa-session-lease-renewal-timer session)))))

(niwa-deftest niwa-session-durability-capture-context/heartbeat ()
  ,test
  (test)
  :doc "captures the heartbeat guard and binds it only inside its continuation"
  (let* ((guard (lambda () t))
	 (niwa-session-durability--async-valid-p guard)
	 (context (niwa-session-durability-capture-context)))
    (should (= (length context) 6))
    (should (eq guard (nth 5 context)))
    (let ((niwa-session-durability--async-valid-p nil)
	  (niwa-session-control-fs--async-wait t))
      (niwa-session-durability-call-in-context
       context (lambda ()
		 (should (eq guard niwa-session-durability--async-valid-p))
		 (should-not niwa-session-control-fs--async-wait)))
      (should-not niwa-session-durability--async-valid-p)
      (should niwa-session-control-fs--async-wait)
      (niwa-session-durability-call-in-context
       (list nil nil nil nil nil nil)
       (lambda () (should niwa-session-control-fs--async-wait))))))

(niwa-deftest niwa-session-durability--heartbeat-finish ()
  ,test
  (test)
  :doc "a real child failure stays visible after remote carrier cache invalidation"
  (test-niwa-heartbeat--with-session
    (let ((niwa-session-control-fs--programs
           (copy-hash-table niwa-session-control-fs--programs))
          (niwa-session-control-fs--program-script
           (concat "exit 71\n" niwa-session-control-fs--program-script))
          (remote (symbol-function 'file-remote-p))
          (run (symbol-function 'niwa-session-control-fs-run-program-async))
          (alias "/ssh:heartbeat-failure-alias:")
          (calls 0) failed warnings)
      (cl-letf (((symbol-function 'file-remote-p)
                 (lambda (path &rest arguments)
                   ;; Only completion sees the cold remote classification.
                   ;; The child and all filesystem operations are real/local.
                   (if (and failed (equal root path)) alias
                     (apply remote path arguments))))
                ((symbol-function 'niwa-session-control-fs-run-program-async)
                 (lambda (operations callback &optional scope)
                   (cl-incf calls)
                   (funcall run operations
                            (lambda (results error)
                              (should error)
                              (should-not (gethash "" niwa-session-control-fs--programs))
                              (setq failed t)
                              (remhash alias niwa-session-control-fs--programs)
                              (funcall callback results error))
                            scope)))
                ((symbol-function 'display-warning)
                 (lambda (_type message &rest _) (push message warnings)))
                ((symbol-function 'niwa-session-control-fs-run-program)
                 (lambda (&rest _) (ert-fail "Failed heartbeat fell back synchronously"))))
        (funcall tick session)
        (test-niwa-heartbeat--await session))
      (should failed)
      (should (= calls 1))
      (should (= (length warnings) 1))
      (should (string-prefix-p
               (format "niwa [%s]: Portable session heartbeat failed:"
                       (niwa-workspace-root (niwa-session-workspace session)))
               (car warnings)))
      (should (string-match-p "exit status 71" (car warnings)))
      (should (eq 'lost (plist-get (niwa-session-lease session) :state)))
      (should-not (niwa-session-lease-renewal-timer session))))

  :doc "qualification loss after a held observation refuses the next target launch"
  (test-niwa-heartbeat--with-session
    (let ((qualify (symbol-function 'niwa-session-durability--heartbeat-carrier-p))
          (available t) warnings)
      (cl-letf (((symbol-function 'niwa-session-durability--heartbeat-carrier-p)
                 (lambda (path) (and available (funcall qualify path))))
                ((symbol-function 'display-warning)
                 (lambda (_type message &rest _) (push message warnings)))
                ((symbol-function 'niwa-session-control-fs-run-program)
                 (lambda (&rest _) (ert-fail "Unavailable carrier fell back synchronously"))))
        (test-niwa-heartbeat--held
          (setq available nil)
          (funcall resume)
          (test-niwa-heartbeat--await session)
          (should (= programs 1))))
      (should (= (length warnings) 1))
      (should (string-match-p "Lease heartbeat carrier unavailable" (car warnings)))
      (should (eq 'lost (plist-get (niwa-session-lease session) :state))))))

(niwa-deftest niwa-session-durability-heartbeat/context ()
  ,test
  (test)
  :doc "a delivered timer cannot inherit an unrelated publisher's riders or child owner"
  (test-niwa-heartbeat--with-session
    (let* ((marker (file-name-concat root "unrelated-publisher"))
           (clock (list nil)) (assertions (list nil)) (recovery (list nil))
           (owners 0) delivered
           (niwa-session-durability--transaction-clock clock)
           (niwa-session-durability--asserted-directories assertions)
           (niwa-session-recovery--mutation-cache recovery)
           (niwa-session-durability--observed-time (+ (floor (float-time)) 86400))
           (niwa-session-durability--commit-riders
            (list :after (list (list :op 'append :path marker :content "foreign"))))
           (niwa-session-durability--final-commit t)
           (niwa-session-durability-async-process-function
            (lambda (_process) (cl-incf owners)))
           (timer (run-at-time 0 nil (lambda () (setq delivered t) (funcall tick session)))))
      (unwind-protect
          (progn
            (let ((deadline (+ (float-time) 5)))
              (while (and (not delivered) (< (float-time) deadline))
                (accept-process-output nil 0.01)))
            (should delivered)
            (test-niwa-heartbeat--await session)
            (should (equal '(nil 0 (nil) (nil) (nil))
                           (list (file-exists-p marker) owners clock assertions recovery)))
            (should (< (plist-get (niwa-session-lease session) :renewed-at)
                       (+ (float-time) 5)))
            (should (eq 'owned (plist-get (niwa-session-lease session) :state))))
        (cancel-timer timer)))))

(provide 'test-niwa-session-durability-heartbeat)
;;; test-niwa-session-durability-heartbeat.el ends here
