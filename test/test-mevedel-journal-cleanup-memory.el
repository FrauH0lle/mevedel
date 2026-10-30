;;; test-mevedel-journal-cleanup-memory.el --- Memory admission and cleanup -*- lexical-binding: t -*-

;;; Commentary:
;; Real cleanup phases must not take mutation ownership ahead of local memory work.

;;; Code:

(require 'helpers
         (file-name-concat (file-name-directory (or buffer-file-name load-file-name))
                           "helpers"))
(require 'mevedel-journal-cleanup)
(require 'mevedel-journal-worker)
(require 'mevedel-memory-pass)
(require 'mevedel-memory-decision)
(require 'mevedel-memory-store)
(require 'mevedel-state-cleanup)
(require 'mevedel-transport)

(defun mevedel-test-cleanup-memory-entry (root)
  "Publish an old reviewed digest under ROOT, eligible for native cleanup."
  (let ((entry
         (mevedel-journal-store-publish-digest
          root (list :capture-id (secure-hash 'sha256 "old") :session "old" :session-name "old"
                     :workspace (make-string 64 ?b) :trigger 'session-end :segment 1
                     :source-revision (make-string 64 ?c) :turns '(1)
                     :turn-ids (list (secure-hash 'sha256 "old-turn"))
                     :created "2000-01-01T00:00:00Z" :model "test:model")
          "## Done\n- Observed: complete (turn 1).\n\n## Learned\n- none\n\n## Surprised\n- none\n\n## Unfinished\n- none")))
    (mevedel-journal-store-publish-review
     root (list :pass-id (secure-hash 'sha256 "review") :workspace (plist-get entry :workspace)
                :created (format-time-string "%Y-%m-%dT%H:%M:%SZ" nil t)
                :model "test:model" :focus "" :digests (list (plist-get entry :id)) :proposals nil :references nil))
    entry))

(defun mevedel-test-cleanup-memory-step (root)
  "Deliver ROOT's current cleanup continuation without relying on batch idle."
  (let ((timer (car (gethash root mevedel-journal-cleanup--pending))))
    (should (timerp timer))
    (cancel-timer timer)
    (apply (timer--function timer) (timer--args timer))))

(defmacro mevedel-test-cleanup-memory-fixture (&rest body)
  "Run BODY with a native cleanup WORKSPACE and isolated local activity tables."
  (declare (indent 0) (debug t))
  `(let* ((root (make-temp-file "mevedel-cleanup-admission-" t))
          (workspace (mevedel-workspace--create :root root))
          (key (mevedel-memory-store--claim-directory workspace))
          (mevedel-journal-worker--child-p t)
          (mevedel-journal-cleanup--inhibit-scheduling nil)
          (mevedel-journal-cleanup--pending (make-hash-table :test #'equal))
          (mevedel-journal-cleanup--active (make-hash-table :test #'equal))
          (mevedel-memory-pass--running (make-hash-table :test #'equal))
          (mevedel-memory-pass--drains (make-hash-table :test #'equal))
          (mevedel-memory-decision--operations (make-hash-table :test #'equal))
          (mevedel-memory-decision--recovery-workers (make-hash-table :test #'equal))
          (mevedel-transport--pending (make-hash-table :test #'equal))
          (mevedel-journal-max-age-days 365)
          (entry (mevedel-test-cleanup-memory-entry root)))
     (unwind-protect (progn ,@body)
       (remhash key mevedel-memory-pass--drains)
       (mevedel-transport-cancel-pending (list 'journal-cleanup root))
       (when (timerp (car (gethash root mevedel-journal-cleanup--pending)))
         (let ((mevedel-journal-cleanup--inhibit-scheduling t))
           (mevedel-test-cleanup-memory-step root)))
       (delete-directory root t))))

(mevedel-deftest mevedel-journal-cleanup-schedule/memory ()
  (let ((mevedel-journal-worker--child-p t)) ,test)
  (test)
  :doc "forced cleanup cannot acquire native mutation ownership during pass drain operation or recovery"
  (dolist (owner '(pass drain operation recovery))
    (let* ((root (make-temp-file "mevedel-cleanup-memory-" t))
	   (workspace (mevedel-workspace--create :root root))
	   (key (mevedel-memory-store--claim-directory workspace))
	   (mevedel-journal-cleanup--inhibit-scheduling nil)
	   (mevedel-journal-cleanup--pending (make-hash-table :test #'equal))
	   (mevedel-journal-cleanup--active (make-hash-table :test #'equal))
	   (mevedel-memory-pass--running (make-hash-table :test #'equal))
	   (mevedel-memory-pass--drains (make-hash-table :test #'equal))
	   (mevedel-memory-decision--operations (make-hash-table :test #'equal))
	   (mevedel-memory-decision--recovery-workers (make-hash-table :test #'equal))
	   (mevedel-journal-max-age-days 365)
	   (entry (mevedel-test-cleanup-memory-entry root))
	   (claim (unless (eq owner 'drain) (mevedel-journal-claim-acquire key 180)))
	   (table (pcase owner
		    ('pass mevedel-memory-pass--running)
		    ('drain mevedel-memory-pass--drains)
		    ('operation mevedel-memory-decision--operations)
		    ('recovery mevedel-memory-decision--recovery-workers)))
	   (state (list :claim claim :status 'waiting))
	   mutation-admitted)
      (unwind-protect
	  (progn
	    (puthash key state table)
	    ;; Force must survive every deferred delivery, even inside the
	    ;; ordinary hourly gate and a drain's claim-free between-batch gap.
	    (setf (mevedel-workspace-journal-cleanup-at workspace) (float-time))
	    (mevedel-journal-cleanup-schedule workspace t)
	    (mevedel-journal-cleanup-schedule workspace)
	    (dotimes (_ 3) (mevedel-test-cleanup-memory-step root))
	    (mevedel-memory-store--mutation workspace (lambda (_claim) (setq mutation-admitted t)))
	    (should mutation-admitted)
	    (should (cdr (gethash root mevedel-journal-cleanup--pending)))
	    (should-not (mevedel-journal-cleanup-active-p workspace))
	    (should (mevedel-journal-store-read root (plist-get entry :file)))
	    (remhash key table)
	    (when claim (mevedel-journal-claim-settle claim 'completed ""))
	    (let ((deadline (+ (float-time) 15)))
	      (while (and (gethash root mevedel-journal-cleanup--pending) (< (float-time) deadline))
		(mevedel-test-cleanup-memory-step root)))
	    (should-not (gethash root mevedel-journal-cleanup--pending))
	    (should-not (file-exists-p (file-name-concat (mevedel-journal-store-directory root) (plist-get entry :file))))
	    (dolist (scope '(mutation digest-run consolidation))
	      (let ((token (mevedel-journal-claim-current
			    (mevedel-journal-store-claim-directory (mevedel-journal-store-directory root) scope))))
		(should token)
		(should (mevedel-journal-claim-outcome token)))))
	(remhash key table)
	(when claim (mevedel-journal-claim-settle claim 'completed ""))
	(when (gethash root mevedel-journal-cleanup--pending)
	  (let ((mevedel-journal-cleanup--inhibit-scheduling t))
	    (mevedel-test-cleanup-memory-step root)))
	(delete-directory root t))))

  :doc "another workspace's queued drain does not block native cleanup here"
  (mevedel-test-cleanup-memory-fixture
    (let ((other (make-temp-file "mevedel-cleanup-other-" t)))
      (unwind-protect
	  (progn
	    (puthash (mevedel-memory-store--claim-directory (mevedel-workspace--create :root other))
		     '(:status waiting) mevedel-memory-pass--drains)
	    (mevedel-journal-cleanup-schedule workspace t)
	    (let ((deadline (+ (float-time) 15)))
	      (while (and (gethash root mevedel-journal-cleanup--pending) (< (float-time) deadline))
		(mevedel-test-cleanup-memory-step root)))
	    (should-not (gethash root mevedel-journal-cleanup--pending))
	    (should-not (mevedel-journal-cleanup-active-p workspace))
	    (should-not (file-exists-p (file-name-concat (mevedel-journal-store-directory root) (plist-get entry :file)))))
	(delete-directory other t)))))

(mevedel-deftest mevedel-journal-cleanup-active-p ()
  ,test
  (test)
  :doc "transport-deferred is queued; a new drain cannot starve already admitted native cleanup"
  (mevedel-test-cleanup-memory-fixture
    (let ((state-cleanup (symbol-function 'mevedel-state-cleanup)) seen-active)
      (mevedel-journal-cleanup-schedule workspace t)
      (should-not (mevedel-journal-cleanup-active-p workspace))
      (let ((mevedel-transport--depth 1)) (mevedel-test-cleanup-memory-step root))
      (should-not (car (gethash root mevedel-journal-cleanup--pending)))
      (should-not (mevedel-journal-cleanup-active-p workspace))
      (puthash key '(:status waiting) mevedel-memory-pass--drains)
      (let ((timer (car (gethash (list 'journal-cleanup root) mevedel-transport--pending))))
	(should (timerp timer))
	(cancel-timer timer)
	(apply (timer--function timer) (timer--args timer)))
      ;; Recheck memory admission after transport deferral, before any IO.
      (should (timerp (car (gethash root mevedel-journal-cleanup--pending))))
      (should-not (mevedel-journal-cleanup-active-p workspace))
      (should-not (mevedel-journal-claim-current
		   (mevedel-journal-store-claim-directory (mevedel-journal-store-directory root) 'mutation)))
      (remhash key mevedel-memory-pass--drains)
      (cl-letf (((symbol-function 'mevedel-state-cleanup)
		 (lambda (ws)
		   (setq seen-active (mevedel-journal-cleanup-active-p ws))
		   (funcall state-cleanup ws))))
	(mevedel-test-cleanup-memory-step root))
      (should seen-active)
      (should (mevedel-journal-cleanup-active-p workspace))
      (puthash key '(:status waiting) mevedel-memory-pass--drains)
      (let ((deadline (+ (float-time) 15)))
	(while (and (gethash root mevedel-journal-cleanup--pending) (< (float-time) deadline))
	  (mevedel-test-cleanup-memory-step root)))
      (should-not (gethash root mevedel-journal-cleanup--pending))
      (should-not (mevedel-journal-cleanup-active-p workspace))
      (should-not (file-exists-p (file-name-concat (mevedel-journal-store-directory root) (plist-get entry :file))))
      (dolist (scope '(mutation digest-run consolidation))
	(should (mevedel-journal-claim-outcome
		 (mevedel-journal-claim-current
		  (mevedel-journal-store-claim-directory (mevedel-journal-store-directory root) scope)))))))

  :doc "iterator close error or quit clears only the exact retired cleanup job"
  (dolist (condition '(error quit))
    (dolist (replace '(nil t))
      (mevedel-test-cleanup-memory-fixture
	(let ((close (symbol-function 'iter-close)) injected
	      (successor (cons nil nil)))
	  (mevedel-journal-cleanup-schedule workspace t)
	  (dotimes (_ 3) (mevedel-test-cleanup-memory-step root))
	  (should (mevedel-journal-cleanup-active-p workspace))
	  (let ((token (mevedel-journal-claim-current
			(mevedel-journal-store-claim-directory (mevedel-journal-store-directory root) 'mutation))))
	    (should (mevedel-journal-claim-owned-p token))
	    (cl-letf (((symbol-function 'iter-close)
		       (lambda (iterator)
			 (funcall close iterator)
			 (setq injected t)
			 (when replace
			   (puthash root successor mevedel-journal-cleanup--pending)
			   (puthash root successor mevedel-journal-cleanup--active))
			 (signal condition (when (eq condition 'error) '("Injected iterator close failure"))))))
	      (mevedel-test--with-captured-messages nil
		(let ((mevedel-journal-cleanup--inhibit-scheduling t))
		  (mevedel-test-cleanup-memory-step root))))
	    (should injected)
	    (should (eq 'completed (plist-get (mevedel-journal-claim-outcome token) :status)))
	    (if replace
		(progn
		  (should (eq successor (gethash root mevedel-journal-cleanup--pending)))
		  (should (eq successor (gethash root mevedel-journal-cleanup--active))))
	      (should-not (gethash root mevedel-journal-cleanup--pending))
	      (should-not (mevedel-journal-cleanup-active-p workspace)))
	    (remhash root mevedel-journal-cleanup--pending)
	    (remhash root mevedel-journal-cleanup--active))))))

  :doc "synchronous native cleanup is active before IO and cannot be overwritten by queued cleanup"
  (mevedel-test-cleanup-memory-fixture
    (let ((state-cleanup (symbol-function 'mevedel-state-cleanup))
	  seen-active after-queued reentrant injected)
      (cl-letf (((symbol-function 'mevedel-state-cleanup)
		 (lambda (ws)
		   (unless injected
		     (setq injected t seen-active (mevedel-journal-cleanup-active-p ws))
		     (setq reentrant (mevedel-journal-cleanup-expired ws t))
		     (mevedel-journal-cleanup-schedule ws t)
		     (mevedel-test-cleanup-memory-step root)
		     (setq after-queued (mevedel-journal-cleanup-active-p ws)))
		   (funcall state-cleanup ws))))
	(should (= 1 (mevedel-journal-cleanup-expired workspace t))))
      (should injected)
      (should seen-active)
      (should (eq seen-active after-queued))
      (should-not reentrant)
      (should-not (mevedel-journal-cleanup-active-p workspace))
      (should (cdr (gethash root mevedel-journal-cleanup--pending)))
      (should-not (file-exists-p (file-name-concat (mevedel-journal-store-directory root) (plist-get entry :file))))
      (let ((deadline (+ (float-time) 15)))
	(while (and (gethash root mevedel-journal-cleanup--pending) (< (float-time) deadline))
	  (mevedel-test-cleanup-memory-step root)))
      (should-not (gethash root mevedel-journal-cleanup--pending))
      (should-not (mevedel-journal-cleanup-active-p workspace))))

  :doc "synchronous setup error or quit clears its observation token"
  (dolist (condition '(error quit))
    (mevedel-test-cleanup-memory-fixture
      (let (seen-active caught)
	(cl-letf (((symbol-function 'mevedel-state-cleanup)
		   (lambda (ws)
		     (setq seen-active (mevedel-journal-cleanup-active-p ws))
		     (signal condition (when (eq condition 'error) '("Injected synchronous setup failure"))))))
	  (condition-case err
	      (mevedel-journal-cleanup-expired workspace t)
	    ((error quit) (setq caught err))))
	(should seen-active)
	(should (eq condition (car caught)))
	(should-not (mevedel-journal-cleanup-active-p workspace))
	(should (file-exists-p (file-name-concat (mevedel-journal-store-directory root) (plist-get entry :file))))))))

(mevedel-deftest mevedel-journal-cleanup-schedule/memory-worker ()
  ,test
  (test)
  :doc "native child starts only after memory admission; forced successor waits for a newly queued drain"
  (mevedel-test-cleanup-memory-fixture
    (let* ((compiled (make-temp-file "mevedel-cleanup-compiled-" t))
           (source (file-name-concat compiled "mevedel-journal-cleanup.el"))
           (mevedel-journal-worker--child-p nil)
           (mevedel-journal-worker--processes nil)
           (kill-emacs-hook kill-emacs-hook)
           (start (symbol-function 'mevedel-journal-worker-start))
           (calls 0) seen-active job)
      (unwind-protect
          (progn
            ;; Compile in a cold child, not the helper-preloaded ERT process.
            ;; Maintenance children below can resolve only this bytecode from
            ;; their first load-path entry; leave no source to mask that path.
            (copy-file (concat (file-name-sans-extension (locate-library "mevedel-journal-cleanup")) ".el") source)
            (with-temp-buffer
              (let ((status
                     (call-process
                      (expand-file-name invocation-name invocation-directory) nil t nil
                      "--batch" "-Q" "--eval"
                      (prin1-to-string
                       `(progn
                          (setq load-path ',load-path native-comp-jit-compilation nil)
                          (when (native-comp-available-p)
                            (startup-redirect-eln-cache (expand-file-name "eln" (getenv "HOME"))))
                          (require 'bytecomp)
                          (let ((byte-compile-verbose nil) (byte-compile-error-on-warn t))
                            (unless (byte-compile-file ,source)
                              (error "Cleanup compilation failed"))))))))
                (ert-info ((buffer-string))
			  (should (equal 0 status))
			  (should (equal "" (buffer-string))))))
            (delete-file source)
            (should (file-exists-p (concat source "c")))
            (cl-letf (((symbol-function 'mevedel-journal-worker-start)
                       (lambda (&rest args)
			 (cl-incf calls)
			 (setq seen-active (mevedel-journal-cleanup-active-p workspace))
			 (let ((load-path (cons compiled load-path)))
                           (apply start args)))))
	      (should (mevedel-journal-worker-supported-p workspace))
	      (setf (mevedel-workspace-journal-cleanup-at workspace) (float-time))
	      (puthash key '(:status waiting) mevedel-memory-pass--drains)
	      (mevedel-journal-cleanup-schedule workspace t)
	      (dotimes (_ 3) (mevedel-test-cleanup-memory-step root))
	      (should (= 0 calls))
	      (should-not mevedel-journal-worker--processes)
	      (should-not (mevedel-journal-cleanup-active-p workspace))
	      (remhash key mevedel-memory-pass--drains)
	      (setq job (gethash root mevedel-journal-cleanup--pending))
	      (mevedel-test-cleanup-memory-step root)
	      (should seen-active)
	      (should (eq job seen-active))
	      (should (= 1 calls))
	      (should mevedel-journal-worker--processes)
	      (puthash key '(:status waiting) mevedel-memory-pass--drains)
	      (mevedel-journal-cleanup-schedule workspace t)
	      (let ((deadline (+ (float-time) 15)))
		(while (and (mevedel-journal-cleanup-active-p workspace) (< (float-time) deadline))
                  (accept-process-output nil .01)))
	      (should-not (mevedel-journal-cleanup-active-p workspace))
	      (should-not mevedel-journal-worker--processes)
	      (should-not (file-exists-p (file-name-concat (mevedel-journal-store-directory root) (plist-get entry :file))))
	      (dolist (scope '(mutation digest-run consolidation))
		(should (mevedel-journal-claim-outcome
			 (mevedel-journal-claim-current
                          (mevedel-journal-store-claim-directory (mevedel-journal-store-directory root) scope)))))
	      (should (gethash root mevedel-journal-cleanup--pending))
	      (should-not (eq job (gethash root mevedel-journal-cleanup--pending)))
	      (should (cdr (gethash root mevedel-journal-cleanup--pending)))
	      (dotimes (_ 3) (mevedel-test-cleanup-memory-step root))
	      (should (= 1 calls))
	      (remhash key mevedel-memory-pass--drains)
	      (mevedel-test-cleanup-memory-step root)
	      (let ((deadline (+ (float-time) 15)))
		(while (and (gethash root mevedel-journal-cleanup--pending) (< (float-time) deadline))
                  (accept-process-output nil .01)))
	      (should-not (gethash root mevedel-journal-cleanup--pending))
	      (should-not (mevedel-journal-cleanup-active-p workspace))
	      (should-not mevedel-journal-worker--processes)
	      (should (= 2 calls))))
        (let ((mevedel-journal-cleanup--inhibit-scheduling t))
          (mevedel-journal-worker-stop-all))
        (delete-directory compiled t)))))

(provide 'test-mevedel-journal-cleanup-memory)
;;; test-mevedel-journal-cleanup-memory.el ends here
