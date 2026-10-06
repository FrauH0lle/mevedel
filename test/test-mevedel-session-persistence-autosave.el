;;; test-mevedel-session-persistence-autosave.el --- Auto-save tests -*- lexical-binding: t -*-

;;; Commentary:

;; Native auto-save checkpoints and failure isolation using real storage.

;;; Code:

(require 'mevedel-session-test-support
         (file-name-concat
          (file-name-directory (or load-file-name buffer-file-name))
          "mevedel-session-test-support"))

(require 'mevedel-journal-test-support
         (file-name-concat
          (file-name-directory (or load-file-name buffer-file-name))
          "mevedel-journal-test-support"))

(mevedel-deftest mevedel-session-persistence-autosave-buffer
		 (:quiet t)
		 ,test
		 (test)
  :doc "opening, auto-saving and closing an untouched session leaves no saved state"
  (dolist (type '(file project))
    (let* ((root (make-temp-file "mevedel-empty-session-" t))
           (workspace
            (if (eq type 'file)
                (test-mevedel-session-persistence--make-file-workspace root)
              (test-mevedel-session-persistence--make-workspace root)))
           buffer session)
      (unwind-protect
          (progn
            (cl-letf (((symbol-function 'mevedel-journal-cleanup-schedule)
                       (lambda (&rest _) (ert-fail "Startup scheduled journal cleanup")))
                      ((symbol-function 'mevedel-journal-process-schedule)
                       (lambda (&rest _) (ert-fail "Startup scheduled journal processing")))
                      ((symbol-function 'mevedel-memory-pass-schedule)
                       (lambda (&rest _) (ert-fail "Startup scheduled memory review")))
                      ((symbol-function 'mevedel-memory-decision-schedule-recovery)
                       (lambda (&rest _) (ert-fail "Startup scheduled memory recovery"))))
              (setq buffer (mevedel--chat-buffer "empty" t workspace root)
                    session (buffer-local-value 'mevedel--session buffer)))
            (should (zerop (buffer-size buffer)))
            (should-not (mevedel-session-save-path session))
            (mevedel-session-persistence-autosave-buffer buffer)
            (should-not (mevedel-session-save-path session))
            (kill-buffer buffer)
            (should-not (mevedel-session-save-path session))
            (should-not (file-directory-p
                         (mevedel-session-artifacts-sessions-dir workspace))))
        (when (buffer-live-p buffer) (kill-buffer buffer))
        (mevedel-workspace-clear-registry)
        (delete-directory root t))))
		 :doc "checkpoints partial responses through both persistence profiles"
		 (dolist (type '(file project))
		   (let* ((root (make-temp-file "mevedel-autosave-" t))
			  (workspace
			   (if (eq type 'file)
			       (test-mevedel-session-persistence--make-file-workspace root)
			     (test-mevedel-session-persistence--make-workspace root)))
			  (session (mevedel-session-create "main" workspace))
			  (buffer (generate-new-buffer " *autosave*")))
		     (unwind-protect
			 (with-current-buffer buffer
			   (mevedel-chat-prepare-transcript-buffer)
			   (setq-local mevedel--session session)
			   (setq-local mevedel--workspace workspace)
			   (mevedel-session-set-root-buffer session buffer)
			   (insert "Prompt\n")
			   (insert (propertize "Partial response\n" 'gptel 'response))
			   (should (mevedel-session-persistence-autosave-buffer buffer))
			   (let ((text (mevedel-session-artifacts-read-artifact
					session "segment-0001.chat.org" t)))
			     (should (string-search "Partial response" text))
			     (should (string-search "GPTEL_BOUNDS" text)))
                   (should-not (buffer-modified-p))
                   (when (eq type 'project)
                     (should-not (file-exists-p buffer-file-name))
                     (should (= 0 (visited-file-modtime)))
                     (let ((head (plist-get (mevedel-session-publication session) :head)))
                       (should (mevedel-session-persistence-autosave-buffer buffer))
                       (should (equal head (plist-get (mevedel-session-publication session) :head)))))
			   ;; A checkpoint must not turn an unfinished response into a
			   ;; completed turn or emit a completed-turn journal capture.
			   (should (zerop (mevedel-session-turn-count session)))
			   (goto-char (point-max))
			   (insert "Read-only edit\n")
			   (setq-local mevedel-session--read-only-mode t)
			   (should-not (mevedel-session-persistence-autosave-buffer buffer))
			   (should-not
			    (string-search "Read-only edit"
					   (mevedel-session-artifacts-read-artifact
					    session "segment-0001.chat.org" t))))
		       (test-mevedel-session-persistence--release-and-kill buffer session)
		       (delete-directory root t))))
		 :doc "does not persist view projections or ordinary buffers"
		 (with-temp-buffer
		   (should-not
		    (mevedel-session-persistence-autosave-buffer (current-buffer)))
		   (setq-local mevedel--session
			       (mevedel-session--create
				:workspace (mevedel-workspace--create :type 'file)))
		   (insert "View chrome")
		   (should-not
		    (mevedel-session-persistence-autosave-buffer (current-buffer)))))

(mevedel-deftest mevedel-session-persistence-autosave (:quiet t)
		 ,test
		 (test)
		 :doc "native auto-save contains a failed write and retries after repair"
		 (let* ((root (make-temp-file "mevedel-autosave-retry-" t))
			(workspace
			 (test-mevedel-session-persistence--make-file-workspace root))
			(bad (mevedel-session-create "bad" workspace))
			(good (mevedel-session-create "good" workspace))
			(bad-buffer (generate-new-buffer " *bad-save*"))
			(good-buffer (generate-new-buffer " *good-save*")))
		   (unwind-protect
		       (progn
			 (dolist (pair (list (cons bad bad-buffer) (cons good good-buffer)))
			   (with-current-buffer (cdr pair)
			     (mevedel-chat-prepare-transcript-buffer)
			     (setq-local mevedel--session (car pair))
			     (setq-local mevedel--workspace workspace)
			     (insert "Prompt\n")
			     (mevedel-session-artifacts-save (car pair) (cdr pair))
			     (goto-char (point-max))
			     (insert (propertize "Latest text\n" 'gptel 'response))))
			 (let ((path (mevedel-session-artifacts-sidecar-path
				      (mevedel-session-save-path bad))))
			   (delete-file path)
			   (make-directory path)
			   (cl-letf (((symbol-function 'buffer-list)
				      (lambda (&optional _frame)
					(list bad-buffer good-buffer))))
			     (run-hooks 'auto-save-hook)
                             (with-timeout (2 (ert-fail "Auto-save remained queued"))
                               (while (> (hash-table-count mevedel-session-persistence--autosaves) 0)
                                 (sleep-for .002)))
			     (should (buffer-local-value
				      'mevedel-session--save-failed bad-buffer))
			     (should (string-search
				      "Latest text"
				      (mevedel-session-artifacts-read-artifact
				       good "segment-0001.chat.org" t)))
			     ;; The transcript write may have succeeded before its
			     ;; sidecar failed; retry must not depend on modified-p.
			     (delete-directory path)
			     (run-hooks 'auto-save-hook)
                             (with-timeout (2 (ert-fail "Auto-save remained queued"))
                               (while (> (hash-table-count mevedel-session-persistence--autosaves) 0)
                                 (sleep-for .002)))
			     (should-not (buffer-local-value
					  'mevedel-session--save-failed bad-buffer))
			     (should (file-regular-p path)))))
		     (test-mevedel-session-persistence--release-and-kill bad-buffer bad)
		     (test-mevedel-session-persistence--release-and-kill good-buffer good)
		     (delete-directory root t))))

(mevedel-deftest mevedel-session-persistence-release-on-kill (:quiet t)
		 ,test
		 (test)
		 :doc "closing a data buffer saves its partial response before releasing ownership"
		 (let* ((root (make-temp-file "mevedel-kill-save-" t))
			(workspace
			 (test-mevedel-session-persistence--make-file-workspace root))
			(session (mevedel-session-create "main" workspace))
			(buffer (generate-new-buffer " *kill-save*")))
		   (unwind-protect
		       (progn
			 (with-current-buffer buffer
			   (mevedel-chat-prepare-transcript-buffer)
			   (setq-local mevedel--session session)
			   (setq-local mevedel--workspace workspace)
			   (insert "Prompt\n")
			   (mevedel-session-artifacts-save session buffer)
			   (goto-char (point-max))
			   (insert (propertize "Unfinished response\n" 'gptel 'response))
			   (add-hook 'kill-buffer-hook
				     #'mevedel-session-persistence-release-on-kill nil t))
			 (kill-buffer buffer)
			 (should (string-search
				  "Unfinished response"
				  (mevedel-session-artifacts-read-artifact
				   session "segment-0001.chat.org" t)))
			 (should-not
			  (file-exists-p
			   (mevedel-session-persistence--lock-path
			    (mevedel-session-save-path session)))))
		     (test-mevedel-session-persistence--release-and-kill buffer session)
		     (delete-directory root t))))

(mevedel-deftest mevedel-session-persistence-autosave/responsiveness ()
  (let ((first (generate-new-buffer " *autosave-first*"))
        (second (generate-new-buffer " *autosave-second*"))
        (mevedel-transport--background-resume-at 0)
        (saved nil) input-timer input saw-input)
    (unwind-protect
        (progn
          (dolist (buffer (list first second))
            (with-current-buffer buffer
              (setq-local mevedel--session (mevedel-session--create))
              (insert "Changed")))
          (cl-letf (((symbol-function 'buffer-list)
                     (lambda (&optional _) (list first second)))
                    ((symbol-function 'mevedel-session-persistence-autosave-buffer)
                     (lambda (buffer)
                       (if saved (setq saw-input input)
                         (setq input-timer
                               (run-at-time 0 nil (lambda () (setq input t)))))
                       (push buffer saved)
                       (with-current-buffer buffer (set-buffer-modified-p nil)))))
            (mevedel-session-persistence-autosave)
            (mevedel-session-persistence-autosave)
            (should-not saved)
            (with-timeout (2 (ert-fail "Auto-save did not finish"))
              (while (< (length saved) 2) (sleep-for .002)))
            (should (= 2 (length saved)))
            (should saw-input)))
      (mevedel-transport-cancel-idle
       mevedel-session-persistence--autosaves 'conversation-autosave)
      (when input-timer (cancel-timer input-timer))
      (dolist (buffer (list first second))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer (set-buffer-modified-p nil))
          (kill-buffer buffer))))))

(mevedel-deftest mevedel-session-persistence-autosave-buffer/agent-transaction (:quiet t)
  (mevedel-test-journal-capture--with-portable-session
   (lambda (session root)
     (with-current-buffer root
       (insert "Root prompt\n" (propertize "Response\n" 'gptel 'response))
       (mevedel-session-artifacts-save session root))
     (let* ((child (generate-new-buffer " *autosave-agent*"))
            (invocation (mevedel-agent-invocation--create
                         :agent (mevedel-agent--create :name "worker")
                         :agent-id "worker--test" :path "/root/worker"
                         :buffer child :parent-session session :parent-data-buffer root))
            (publish (symbol-function 'mevedel-session-publication-publish))
            (publications 0))
       (unwind-protect
           (progn
             (mevedel-agent-runtime--setup-transcript invocation child)
             (with-current-buffer child
               (setq-local mevedel--session session mevedel--agent-invocation invocation)
               (insert "Partial agent response"))
             ;; Ordinary transcript changes do not mark the registry dirty.
             (setf (mevedel-agent-invocation-sidecar-dirty invocation) nil)
             (cl-letf (((symbol-function 'mevedel-session-publication-publish)
                        (lambda (&rest args)
                          (cl-incf publications) (apply publish args))))
               (should (mevedel-session-persistence-autosave-buffer child)))
             (should (= publications 1))
             (should-not (mevedel-session-publication-uncommitted-batches session))
             (should (equal "Partial agent response"
                            (mevedel-session-artifacts-read-artifact
                             session (mevedel-agent-invocation-transcript-relative-path invocation) t))))
         (with-current-buffer child
           (set-buffer-modified-p nil) (setq-local kill-buffer-hook nil))
         (kill-buffer child))))))

;;
;;; In-flight checkpoints

(mevedel-deftest mevedel-session-persistence-start-checkpoints
  (:after-each (mevedel-session-persistence-stop-checkpoints))
  ,test
  (test)
  :doc "starts one repeating timer however often requests begin"
  (let ((mevedel-session-checkpoint-interval 30))
    (mevedel-session-persistence-start-checkpoints)
    (let ((timer mevedel-session-persistence--checkpoint-timer))
      (should (memq timer timer-list))
      (should (= 30 (timer--repeat-delay timer)))
      (mevedel-session-persistence-start-checkpoints)
      (should (eq timer mevedel-session-persistence--checkpoint-timer))))
  :doc "a nil interval leaves mid-turn checkpoints to Emacs auto-save"
  (let ((mevedel-session-checkpoint-interval nil))
    (mevedel-session-persistence-start-checkpoints)
    (should-not mevedel-session-persistence--checkpoint-timer)))

(mevedel-deftest mevedel-session-persistence-stop-checkpoints
  (:doc "cancels the in-flight checkpoint timer")
  (let ((mevedel-session-checkpoint-interval 30))
    (mevedel-session-persistence-start-checkpoints)
    (let ((timer mevedel-session-persistence--checkpoint-timer))
      (mevedel-session-persistence-stop-checkpoints)
      (should-not (memq timer timer-list))
      (should-not mevedel-session-persistence--checkpoint-timer))))

(mevedel-deftest mevedel-session-persistence--request-in-flight-p
  (:doc "reports a request owned by any buffer")
  (let ((buffer (generate-new-buffer " *in-flight*")))
    (unwind-protect
        (cl-letf (((symbol-function 'buffer-list)
                   (lambda (&optional _frame) (list buffer))))
          (should-not (mevedel-session-persistence--request-in-flight-p))
          (with-current-buffer buffer
            (setq-local mevedel--current-request (mevedel-request--create)))
          (should (mevedel-session-persistence--request-in-flight-p)))
      (kill-buffer buffer))))

(mevedel-deftest mevedel-session-persistence--checkpoint-tick (:quiet t)
  ,test
  (test)
  :doc "checkpoints an unattended turn's partial response without input"
  (let* ((root (make-temp-file "mevedel-checkpoint-tick-" t))
         (workspace
          (test-mevedel-session-persistence--make-file-workspace root))
         (session (mevedel-session-create "main" workspace))
         (buffer (generate-new-buffer " *checkpoint-tick*"))
         (mevedel-session-checkpoint-interval 30))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (mevedel-chat-prepare-transcript-buffer)
            (setq-local mevedel--session session)
            (setq-local mevedel--workspace workspace)
            (insert "Prompt\n")
            (mevedel-session-artifacts-save session buffer)
            (goto-char (point-max))
            (insert (propertize "Streaming text\n" 'gptel 'response))
            (setq-local mevedel--current-request
                        (mevedel-request--create :session session)))
          (mevedel-session-persistence-start-checkpoints)
          (cl-letf (((symbol-function 'buffer-list)
                     (lambda (&optional _frame) (list buffer))))
            (mevedel-session-persistence--checkpoint-tick)
            (with-timeout (2 (ert-fail "Checkpoint remained queued"))
              (while (> (hash-table-count
                         mevedel-session-persistence--autosaves)
                        0)
                (sleep-for .002))))
          (should (string-search
                   "Streaming text"
                   (mevedel-session-artifacts-read-artifact
                    session "segment-0001.chat.org" t)))
          ;; The request is still in flight, so checkpoints continue.
          (should (memq mevedel-session-persistence--checkpoint-timer
                        timer-list)))
      (mevedel-session-persistence-stop-checkpoints)
      (mevedel-transport-cancel-idle
       mevedel-session-persistence--autosaves 'conversation-autosave)
      (with-current-buffer buffer
        (setq-local mevedel--current-request nil))
      (test-mevedel-session-persistence--release-and-kill buffer session)
      (delete-directory root t)))
  :doc "stops once no request is in flight"
  (let ((mevedel-session-checkpoint-interval 30)
        (saved nil))
    (mevedel-session-persistence-start-checkpoints)
    (cl-letf (((symbol-function 'buffer-list)
               (lambda (&optional _frame) nil))
              ((symbol-function 'mevedel-session-persistence-autosave)
               (lambda () (setq saved t))))
      (mevedel-session-persistence--checkpoint-tick))
    (should-not saved)
    (should-not mevedel-session-persistence--checkpoint-timer)))

(provide 'test-mevedel-session-persistence-autosave)
;;; test-mevedel-session-persistence-autosave.el ends here
