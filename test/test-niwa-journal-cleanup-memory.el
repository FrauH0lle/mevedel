;;; test-niwa-journal-cleanup-memory.el --- Memory admission and cleanup -*- lexical-binding: t -*-

;;; Commentary:
;; Real cleanup phases must not take mutation ownership ahead of local memory work.

;;; Code:

(require 'helpers
         (file-name-concat (file-name-directory (or buffer-file-name load-file-name))
                           "helpers"))
(require 'niwa-journal-cleanup)
(require 'niwa-journal-worker)
(require 'niwa-memory-pass)
(require 'niwa-memory-decision)
(require 'niwa-memory-store)
(require 'niwa-state-cleanup)
(require 'niwa-transport)

(defun niwa-test-cleanup-memory-entry (root)
  "Publish an old reviewed digest under ROOT, eligible for native cleanup."
  (let ((entry
         (niwa-journal-store-publish-digest
          root (list :capture-id (secure-hash 'sha256 "old") :session "old" :session-name "old"
                     :workspace (make-string 64 ?b) :trigger 'session-end :segment 1
                     :source-revision (make-string 64 ?c) :turns '(1)
                     :turn-ids (list (secure-hash 'sha256 "old-turn"))
                     :created "2000-01-01T00:00:00Z" :model "test:model")
          "## Done\n- Observed: complete (turn 1).\n\n## Learned\n- none\n\n## Surprised\n- none\n\n## Unfinished\n- none")))
    (niwa-journal-store-publish-review
     root (list :pass-id (secure-hash 'sha256 "review") :workspace (plist-get entry :workspace)
                :created (format-time-string "%Y-%m-%dT%H:%M:%SZ" nil t)
                :model "test:model" :focus "" :digests (list (plist-get entry :id)) :proposals nil :references nil))
    entry))

(defun niwa-test-cleanup-memory-step (root)
  "Deliver ROOT's current cleanup continuation without relying on batch idle."
  (let ((timer (car (gethash root niwa-journal-cleanup--pending))))
    (should (timerp timer))
    (cancel-timer timer)
    (apply (timer--function timer) (timer--args timer))))

(defmacro niwa-test-cleanup-memory-fixture (&rest body)
  "Run BODY with a native cleanup WORKSPACE and isolated local activity tables."
  (declare (indent 0) (debug t))
  `(let* ((root (make-temp-file "niwa-cleanup-admission-" t))
          (workspace (niwa-workspace--create :root root))
          (key (niwa-memory-store--claim-directory workspace))
          (niwa-journal-worker--child-p t)
          (niwa-journal-cleanup--inhibit-scheduling nil)
          (niwa-journal-cleanup--pending (make-hash-table :test #'equal))
          (niwa-journal-cleanup--active (make-hash-table :test #'equal))
          (niwa-memory-pass--running (make-hash-table :test #'equal))
          (niwa-memory-pass--drains (make-hash-table :test #'equal))
          (niwa-memory-decision--operations (make-hash-table :test #'equal))
          (niwa-memory-decision--recovery-workers (make-hash-table :test #'equal))
          (niwa-transport--pending (make-hash-table :test #'equal))
          (niwa-journal-max-age-days 365)
          (entry (niwa-test-cleanup-memory-entry root)))
     (unwind-protect (progn ,@body)
       (remhash key niwa-memory-pass--drains)
       (niwa-transport-cancel-pending (list 'journal-cleanup root))
       (when (timerp (car (gethash root niwa-journal-cleanup--pending)))
         (let ((niwa-journal-cleanup--inhibit-scheduling t))
           (niwa-test-cleanup-memory-step root)))
       (delete-directory root t))))

(niwa-deftest niwa-journal-cleanup-schedule/memory ()
  (let ((niwa-journal-worker--child-p t)) ,test)
  (test)
  :doc "forced cleanup cannot acquire native mutation ownership during pass drain operation or recovery"
  (dolist (owner '(pass drain operation recovery))
    (let* ((root (make-temp-file "niwa-cleanup-memory-" t))
	   (workspace (niwa-workspace--create :root root))
	   (key (niwa-memory-store--claim-directory workspace))
	   (niwa-journal-cleanup--inhibit-scheduling nil)
	   (niwa-journal-cleanup--pending (make-hash-table :test #'equal))
	   (niwa-journal-cleanup--active (make-hash-table :test #'equal))
	   (niwa-memory-pass--running (make-hash-table :test #'equal))
	   (niwa-memory-pass--drains (make-hash-table :test #'equal))
	   (niwa-memory-decision--operations (make-hash-table :test #'equal))
	   (niwa-memory-decision--recovery-workers (make-hash-table :test #'equal))
	   (niwa-journal-max-age-days 365)
	   (entry (niwa-test-cleanup-memory-entry root))
	   (claim (unless (eq owner 'drain) (niwa-journal-claim-acquire key 180)))
	   (table (pcase owner
		    ('pass niwa-memory-pass--running)
		    ('drain niwa-memory-pass--drains)
		    ('operation niwa-memory-decision--operations)
		    ('recovery niwa-memory-decision--recovery-workers)))
	   (state (list :claim claim :status 'waiting))
	   mutation-admitted)
      (unwind-protect
	  (progn
	    (puthash key state table)
	    ;; Force must survive every deferred delivery, even inside the
	    ;; ordinary hourly gate and a drain's claim-free between-batch gap.
	    (setf (niwa-workspace-journal-cleanup-at workspace) (float-time))
	    (niwa-journal-cleanup-schedule workspace t)
	    (niwa-journal-cleanup-schedule workspace)
	    (dotimes (_ 3) (niwa-test-cleanup-memory-step root))
	    (niwa-memory-store--mutation workspace (lambda (_claim) (setq mutation-admitted t)))
	    (should mutation-admitted)
	    (should (cdr (gethash root niwa-journal-cleanup--pending)))
	    (should-not (niwa-journal-cleanup-active-p workspace))
	    (should (niwa-journal-store-read root (plist-get entry :file)))
	    (remhash key table)
	    (when claim (niwa-journal-claim-settle claim 'completed ""))
	    (let ((deadline (+ (float-time) 15)))
	      (while (and (gethash root niwa-journal-cleanup--pending) (< (float-time) deadline))
		(niwa-test-cleanup-memory-step root)))
	    (should-not (gethash root niwa-journal-cleanup--pending))
	    (should-not (file-exists-p (file-name-concat (niwa-journal-store-directory root) (plist-get entry :file))))
	    (dolist (scope '(mutation digest-run consolidation))
	      (let ((token (niwa-journal-claim-current
			    (niwa-journal-store-claim-directory (niwa-journal-store-directory root) scope))))
		(should token)
		(should (niwa-journal-claim-outcome token)))))
	(remhash key table)
	(when claim (niwa-journal-claim-settle claim 'completed ""))
	(when (gethash root niwa-journal-cleanup--pending)
	  (let ((niwa-journal-cleanup--inhibit-scheduling t))
	    (niwa-test-cleanup-memory-step root)))
	(delete-directory root t))))

  :doc "another workspace's queued drain does not block native cleanup here"
  (niwa-test-cleanup-memory-fixture
    (let ((other (make-temp-file "niwa-cleanup-other-" t)))
      (unwind-protect
	  (progn
	    (puthash (niwa-memory-store--claim-directory (niwa-workspace--create :root other))
		     '(:status waiting) niwa-memory-pass--drains)
	    (niwa-journal-cleanup-schedule workspace t)
	    (let ((deadline (+ (float-time) 15)))
	      (while (and (gethash root niwa-journal-cleanup--pending) (< (float-time) deadline))
		(niwa-test-cleanup-memory-step root)))
	    (should-not (gethash root niwa-journal-cleanup--pending))
	    (should-not (niwa-journal-cleanup-active-p workspace))
	    (should-not (file-exists-p (file-name-concat (niwa-journal-store-directory root) (plist-get entry :file)))))
	(delete-directory other t)))))

(niwa-deftest niwa-journal-cleanup-active-p ()
  ,test
  (test)
  :doc "transport-deferred is queued; a new drain cannot starve already admitted native cleanup"
  (niwa-test-cleanup-memory-fixture
    (let ((state-cleanup (symbol-function 'niwa-state-cleanup)) seen-active)
      (niwa-journal-cleanup-schedule workspace t)
      (should-not (niwa-journal-cleanup-active-p workspace))
      (let ((niwa-transport--depth 1)) (niwa-test-cleanup-memory-step root))
      (should-not (car (gethash root niwa-journal-cleanup--pending)))
      (should-not (niwa-journal-cleanup-active-p workspace))
      (puthash key '(:status waiting) niwa-memory-pass--drains)
      (let ((timer (car (gethash (list 'journal-cleanup root) niwa-transport--pending))))
	(should (timerp timer))
	(cancel-timer timer)
	(apply (timer--function timer) (timer--args timer)))
      ;; Recheck memory admission after transport deferral, before any IO.
      (should (timerp (car (gethash root niwa-journal-cleanup--pending))))
      (should-not (niwa-journal-cleanup-active-p workspace))
      (should-not (niwa-journal-claim-current
		   (niwa-journal-store-claim-directory (niwa-journal-store-directory root) 'mutation)))
      (remhash key niwa-memory-pass--drains)
      (cl-letf (((symbol-function 'niwa-state-cleanup)
		 (lambda (ws)
		   (setq seen-active (niwa-journal-cleanup-active-p ws))
		   (funcall state-cleanup ws))))
	(niwa-test-cleanup-memory-step root))
      (should seen-active)
      (should (niwa-journal-cleanup-active-p workspace))
      (puthash key '(:status waiting) niwa-memory-pass--drains)
      (let ((deadline (+ (float-time) 15)))
	(while (and (gethash root niwa-journal-cleanup--pending) (< (float-time) deadline))
	  (niwa-test-cleanup-memory-step root)))
      (should-not (gethash root niwa-journal-cleanup--pending))
      (should-not (niwa-journal-cleanup-active-p workspace))
      (should-not (file-exists-p (file-name-concat (niwa-journal-store-directory root) (plist-get entry :file))))
      (dolist (scope '(mutation digest-run consolidation))
	(should (niwa-journal-claim-outcome
		 (niwa-journal-claim-current
		  (niwa-journal-store-claim-directory (niwa-journal-store-directory root) scope)))))))

  :doc "iterator close error or quit clears only the exact retired cleanup job"
  (dolist (condition '(error quit))
    (dolist (replace '(nil t))
      (niwa-test-cleanup-memory-fixture
	(let ((close (symbol-function 'iter-close)) injected
	      (successor (cons nil nil)))
	  (niwa-journal-cleanup-schedule workspace t)
	  (dotimes (_ 3) (niwa-test-cleanup-memory-step root))
	  (should (niwa-journal-cleanup-active-p workspace))
	  (let ((token (niwa-journal-claim-current
			(niwa-journal-store-claim-directory (niwa-journal-store-directory root) 'mutation))))
	    (should (niwa-journal-claim-owned-p token))
	    (cl-letf (((symbol-function 'iter-close)
		       (lambda (iterator)
			 (funcall close iterator)
			 (setq injected t)
			 (when replace
			   (puthash root successor niwa-journal-cleanup--pending)
			   (puthash root successor niwa-journal-cleanup--active))
			 (signal condition (when (eq condition 'error) '("Injected iterator close failure"))))))
	      (niwa-test--with-captured-messages nil
		(let ((niwa-journal-cleanup--inhibit-scheduling t))
		  (niwa-test-cleanup-memory-step root))))
	    (should injected)
	    (should (eq 'completed (plist-get (niwa-journal-claim-outcome token) :status)))
	    (if replace
		(progn
		  (should (eq successor (gethash root niwa-journal-cleanup--pending)))
		  (should (eq successor (gethash root niwa-journal-cleanup--active))))
	      (should-not (gethash root niwa-journal-cleanup--pending))
	      (should-not (niwa-journal-cleanup-active-p workspace)))
	    (remhash root niwa-journal-cleanup--pending)
	    (remhash root niwa-journal-cleanup--active))))))

  :doc "synchronous native cleanup is active before IO and cannot be overwritten by queued cleanup"
  (niwa-test-cleanup-memory-fixture
    (let ((state-cleanup (symbol-function 'niwa-state-cleanup))
	  seen-active after-queued reentrant injected)
      (cl-letf (((symbol-function 'niwa-state-cleanup)
		 (lambda (ws)
		   (unless injected
		     (setq injected t seen-active (niwa-journal-cleanup-active-p ws))
		     (setq reentrant (niwa-journal-cleanup-expired ws t))
		     (niwa-journal-cleanup-schedule ws t)
		     (niwa-test-cleanup-memory-step root)
		     (setq after-queued (niwa-journal-cleanup-active-p ws)))
		   (funcall state-cleanup ws))))
	(should (= 1 (plist-get (niwa-journal-cleanup-expired workspace t) :count))))
      (should injected)
      (should seen-active)
      (should (eq seen-active after-queued))
      (should-not reentrant)
      (should-not (niwa-journal-cleanup-active-p workspace))
      (should (cdr (gethash root niwa-journal-cleanup--pending)))
      (should-not (file-exists-p (file-name-concat (niwa-journal-store-directory root) (plist-get entry :file))))
      (let ((deadline (+ (float-time) 15)))
	(while (and (gethash root niwa-journal-cleanup--pending) (< (float-time) deadline))
	  (niwa-test-cleanup-memory-step root)))
      (should-not (gethash root niwa-journal-cleanup--pending))
      (should-not (niwa-journal-cleanup-active-p workspace))))

  :doc "synchronous setup error or quit clears its observation token"
  (dolist (condition '(error quit))
    (niwa-test-cleanup-memory-fixture
      (let (seen-active caught)
	(cl-letf (((symbol-function 'niwa-state-cleanup)
		   (lambda (ws)
		     (setq seen-active (niwa-journal-cleanup-active-p ws))
		     (signal condition (when (eq condition 'error) '("Injected synchronous setup failure"))))))
	  (condition-case err
	      (niwa-journal-cleanup-expired workspace t)
	    ((error quit) (setq caught err))))
	(should seen-active)
	(should (eq condition (car caught)))
	(should-not (niwa-journal-cleanup-active-p workspace))
	(should (file-exists-p (file-name-concat (niwa-journal-store-directory root) (plist-get entry :file))))))))

(niwa-deftest niwa-journal-cleanup-schedule/memory-worker ()
  ,test
  (test)
  :doc "native child starts only after memory admission; forced successor waits for a newly queued drain"
  (niwa-test-cleanup-memory-fixture
    (let* ((compiled (make-temp-file "niwa-cleanup-compiled-" t))
           (source (file-name-concat compiled "niwa-journal-cleanup.el"))
           (niwa-journal-worker--child-p nil)
           (niwa-journal-worker--processes nil)
           (kill-emacs-hook kill-emacs-hook)
           (start (symbol-function 'niwa-journal-worker-start))
           (calls 0) seen-active job)
      (unwind-protect
          (progn
            ;; Compile in a cold child, not the helper-preloaded ERT process.
            ;; Maintenance children below can resolve only this bytecode from
            ;; their first load-path entry; leave no source to mask that path.
            (copy-file (concat (file-name-sans-extension (locate-library "niwa-journal-cleanup")) ".el") source)
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
            (cl-letf (((symbol-function 'niwa-journal-worker-start)
                       (lambda (&rest args)
			 (cl-incf calls)
			 (setq seen-active (niwa-journal-cleanup-active-p workspace))
			 (let ((load-path (cons compiled load-path)))
                           (apply start args)))))
	      (should (niwa-journal-worker-local-p workspace))
	      (setf (niwa-workspace-journal-cleanup-at workspace) (float-time))
	      (puthash key '(:status waiting) niwa-memory-pass--drains)
	      (niwa-journal-cleanup-schedule workspace t)
	      (dotimes (_ 3) (niwa-test-cleanup-memory-step root))
	      (should (= 0 calls))
	      (should-not niwa-journal-worker--processes)
	      (should-not (niwa-journal-cleanup-active-p workspace))
	      (remhash key niwa-memory-pass--drains)
	      (setq job (gethash root niwa-journal-cleanup--pending))
	      (niwa-test-cleanup-memory-step root)
	      (should seen-active)
	      (should (eq job seen-active))
	      (should (= 1 calls))
	      (should niwa-journal-worker--processes)
	      (puthash key '(:status waiting) niwa-memory-pass--drains)
	      (niwa-journal-cleanup-schedule workspace t)
	      (let ((deadline (+ (float-time) 15)))
		(while (and (niwa-journal-cleanup-active-p workspace) (< (float-time) deadline))
                  (accept-process-output nil .01)))
	      (should-not (niwa-journal-cleanup-active-p workspace))
	      (should-not niwa-journal-worker--processes)
	      (should-not (file-exists-p (file-name-concat (niwa-journal-store-directory root) (plist-get entry :file))))
	      (dolist (scope '(mutation digest-run consolidation))
		(should (niwa-journal-claim-outcome
			 (niwa-journal-claim-current
                          (niwa-journal-store-claim-directory (niwa-journal-store-directory root) scope)))))
	      (should (gethash root niwa-journal-cleanup--pending))
	      (should-not (eq job (gethash root niwa-journal-cleanup--pending)))
	      (should (cdr (gethash root niwa-journal-cleanup--pending)))
	      (dotimes (_ 3) (niwa-test-cleanup-memory-step root))
	      (should (= 1 calls))
	      (remhash key niwa-memory-pass--drains)
	      (niwa-test-cleanup-memory-step root)
	      (let ((deadline (+ (float-time) 15)))
		(while (and (gethash root niwa-journal-cleanup--pending) (< (float-time) deadline))
                  (accept-process-output nil .01)))
	      (should-not (gethash root niwa-journal-cleanup--pending))
	      (should-not (niwa-journal-cleanup-active-p workspace))
	      (should-not niwa-journal-worker--processes)
	      (should (= 2 calls))))
        (let ((niwa-journal-cleanup--inhibit-scheduling t))
          (niwa-journal-worker-stop-all))
        (delete-directory compiled t)))))

(provide 'test-niwa-journal-cleanup-memory)
;;; test-niwa-journal-cleanup-memory.el ends here
