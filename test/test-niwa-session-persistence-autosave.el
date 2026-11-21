;;; test-niwa-session-persistence-autosave.el --- Auto-save tests -*- lexical-binding: t -*-

;;; Commentary:

;; Native auto-save checkpoints and failure isolation using real storage.

;;; Code:

(require 'niwa-session-test-support
         (file-name-concat
          (file-name-directory (or load-file-name buffer-file-name))
          "niwa-session-test-support"))

(require 'niwa-journal-test-support
         (file-name-concat
          (file-name-directory (or load-file-name buffer-file-name))
          "niwa-journal-test-support"))
(require 'niwa-agent-runtime)

(niwa-deftest niwa-session-persistence-autosave-buffer
		 (:quiet t)
		 ,test
		 (test)
  :doc "opening, auto-saving and closing an untouched session leaves no saved state"
  (dolist (type '(file project))
    (let* ((root (make-temp-file "niwa-empty-session-" t))
           (workspace
            (if (eq type 'file)
                (test-niwa-session-persistence--make-file-workspace root)
              (test-niwa-session-persistence--make-workspace root)))
           buffer session)
      (unwind-protect
          (progn
            (cl-letf (((symbol-function 'niwa-journal-cleanup-schedule)
                       (lambda (&rest _) (ert-fail "Startup scheduled journal cleanup")))
                      ((symbol-function 'niwa-journal-process-schedule)
                       (lambda (&rest _) (ert-fail "Startup scheduled journal processing")))
                      ((symbol-function 'niwa-memory-pass-schedule)
                       (lambda (&rest _) (ert-fail "Startup scheduled memory review")))
                      ((symbol-function 'niwa-memory-decision-schedule-recovery)
                       (lambda (&rest _) (ert-fail "Startup scheduled memory recovery"))))
              (setq buffer (niwa--chat-buffer "empty" t workspace root)
                    session (buffer-local-value 'niwa--session buffer)))
            (should (zerop (buffer-size buffer)))
            (should-not (niwa-session-save-path session))
            (niwa-session-persistence-autosave-buffer buffer)
            (should-not (niwa-session-save-path session))
            (kill-buffer buffer)
            (should-not (niwa-session-save-path session))
            (should-not (file-directory-p
                         (niwa-session-artifacts-sessions-dir workspace))))
        (when (buffer-live-p buffer) (kill-buffer buffer))
        (niwa-workspace-clear-registry)
        (delete-directory root t))))
		 :doc "checkpoints partial responses through both persistence profiles"
		 (dolist (type '(file project))
		   (let* ((root (make-temp-file "niwa-autosave-" t))
			  (workspace
			   (if (eq type 'file)
			       (test-niwa-session-persistence--make-file-workspace root)
			     (test-niwa-session-persistence--make-workspace root)))
			  (session (niwa-session-create "main" workspace))
			  (buffer (generate-new-buffer " *autosave*")))
		     (unwind-protect
			 (with-current-buffer buffer
			   (niwa-chat-prepare-transcript-buffer)
			   (setq-local niwa--session session)
			   (setq-local niwa--workspace workspace)
			   (niwa-session-set-root-buffer session buffer)
			   (insert "Prompt\n")
			   (insert (propertize "Partial response\n" 'gptel 'response))
			   (should (niwa-session-persistence-autosave-buffer buffer))
			   (let ((text (niwa-session-artifacts-read-artifact
					session "segment-0001.chat.org" t)))
			     (should (string-search "Partial response" text))
			     (should (string-search "GPTEL_BOUNDS" text)))
                   (should-not (buffer-modified-p))
                   (when (eq type 'project)
                     (should-not (file-exists-p buffer-file-name))
                     (should (= 0 (visited-file-modtime)))
                     (let ((head (plist-get (niwa-session-publication session) :head)))
                       (should (niwa-session-persistence-autosave-buffer buffer))
                       (should (equal head (plist-get (niwa-session-publication session) :head)))))
			   ;; A checkpoint must not turn an unfinished response into a
			   ;; completed turn or emit a completed-turn journal capture.
			   (should (zerop (niwa-session-turn-count session)))
			   (goto-char (point-max))
			   (insert "Read-only edit\n")
			   (setq-local niwa-session--read-only-mode t)
			   (should-not (niwa-session-persistence-autosave-buffer buffer))
			   (should-not
			    (string-search "Read-only edit"
					   (niwa-session-artifacts-read-artifact
					    session "segment-0001.chat.org" t))))
		       (test-niwa-session-persistence--release-and-kill buffer session)
		       (delete-directory root t))))
		 :doc "does not persist view projections or ordinary buffers"
		 (with-temp-buffer
		   (should-not
		    (niwa-session-persistence-autosave-buffer (current-buffer)))
		   (setq-local niwa--session
			       (niwa-session--create
				:workspace (niwa-workspace--create :type 'file)))
		   (insert "View chrome")
		   (should-not
		    (niwa-session-persistence-autosave-buffer (current-buffer)))))

(niwa-deftest niwa-session-persistence-autosave (:quiet t)
  ,test
  (test)
  :doc "native auto-save contains a failed lease publication and retries after repair"
  (let* ((root (make-temp-file "niwa-autosave-retry-" t))
         (workspace (test-niwa-session-persistence--make-file-workspace root))
         (bad (niwa-session-create "bad" workspace))
         (good (niwa-session-create "good" workspace))
         (bad-buffer (generate-new-buffer " *bad-save*"))
         (good-buffer (generate-new-buffer " *good-save*"))
         publications)
    (unwind-protect
        (progn
          (dolist (pair (list (cons bad bad-buffer) (cons good good-buffer)))
            (with-current-buffer (cdr pair)
              (niwa-chat-prepare-transcript-buffer)
              (setq-local niwa--session (car pair) niwa--workspace workspace)
              (insert "Prompt\n")
              (niwa-session-artifacts-save (car pair) (cdr pair))
              (goto-char (point-max))
              (insert (propertize "Latest text\n" 'gptel 'response))))
          (setq publications (file-name-concat
                              (niwa-session-save-path bad) ".publications"))
          (let ((head (plist-get (niwa-session-publication bad) :head)))
            (set-file-modes publications #o500)
            (should-not (file-writable-p publications))
            (cl-letf (((symbol-function 'buffer-list)
                       (lambda (&optional _frame) (list bad-buffer good-buffer))))
              (run-hooks 'auto-save-hook)
              (with-timeout (5 (ert-fail "Auto-save remained queued"))
                (while (or (> (hash-table-count niwa-session-persistence--autosaves) 0)
                           (niwa-session-publication-busy-p bad)
                           (niwa-session-publication-busy-p good))
                  (accept-process-output nil .002)))
              (should (buffer-local-value 'niwa-session--save-failed bad-buffer))
              (should (equal head (plist-get
                                  (niwa-session-publication-read
                                   (niwa-session-save-path bad)) :head)))
              (should-not (string-search
                           "Latest text" (niwa-session-artifacts-read-artifact
                                          bad "segment-0001.chat.org" t)))
              (should (string-search
                       "Latest text" (niwa-session-artifacts-read-artifact
                                      good "segment-0001.chat.org" t)))
              ;; Frozen lease recovery requires explicit retry before new writes.
              (should (niwa-session-pending-publication bad))
              (with-current-buffer bad-buffer (set-buffer-modified-p nil))
              (set-file-modes publications #o700)
              (niwa-session-publication-retry bad)
              (run-hooks 'auto-save-hook)
              (with-timeout (5 (ert-fail "Auto-save remained queued"))
                (while (or (> (hash-table-count niwa-session-persistence--autosaves) 0)
                           (niwa-session-publication-busy-p bad))
                  (accept-process-output nil .002)))
              (should-not (buffer-local-value 'niwa-session--save-failed bad-buffer))
              (should (string-search
                       "Latest text" (niwa-session-artifacts-read-artifact
                                      bad "segment-0001.chat.org" t)))
              (should-not (niwa-session-pending-publication bad)))))
      (when publications (set-file-modes publications #o700))
      (test-niwa-session-persistence--release-and-kill bad-buffer bad)
      (test-niwa-session-persistence--release-and-kill good-buffer good)
      (delete-directory root t))))

(niwa-deftest niwa-session-persistence-release-on-kill (:quiet t)
		 ,test
		 (test)
		 :doc "closing a data buffer saves its partial response before releasing ownership"
		 (let* ((root (make-temp-file "niwa-kill-save-" t))
			(workspace
			 (test-niwa-session-persistence--make-file-workspace root))
			(session (niwa-session-create "main" workspace))
			(buffer (generate-new-buffer " *kill-save*")))
		   (unwind-protect
		       (progn
			 (with-current-buffer buffer
			   (niwa-chat-prepare-transcript-buffer)
			   (setq-local niwa--session session)
			   (setq-local niwa--workspace workspace)
			   (insert "Prompt\n")
			   (niwa-session-artifacts-save session buffer)
			   (goto-char (point-max))
			   (insert (propertize "Unfinished response\n" 'gptel 'response))
			   (add-hook 'kill-buffer-hook
				     #'niwa-session-persistence-release-on-kill nil t))
			 (kill-buffer buffer)
			 (should (string-search
				  "Unfinished response"
				  (niwa-session-artifacts-read-artifact
				   session "segment-0001.chat.org" t)))
			 (should-not
			  (file-exists-p
			   (file-name-concat
			    (niwa-session-save-path session) ".lock"))))
		     (test-niwa-session-persistence--release-and-kill buffer session)
		     (delete-directory root t))))

(niwa-deftest niwa-session-persistence-autosave/responsiveness ()
  (let ((first (generate-new-buffer " *autosave-first*"))
        (second (generate-new-buffer " *autosave-second*"))
        (niwa-transport--background-resume-at 0)
        (saved nil) input-timer input saw-input)
    (unwind-protect
        (progn
          (dolist (buffer (list first second))
            (with-current-buffer buffer
              (setq-local niwa--session (niwa-session--create))
              (insert "Changed")))
          (cl-letf (((symbol-function 'buffer-list)
                     (lambda (&optional _) (list first second)))
                    ((symbol-function 'niwa-session-persistence-checkpoint-buffer)
                     (lambda (buffer)
                       (if saved (setq saw-input input)
                         (setq input-timer
                               (run-at-time 0 nil (lambda () (setq input t)))))
                       (push buffer saved)
                       (with-current-buffer buffer (set-buffer-modified-p nil)))))
            (niwa-session-persistence-autosave)
            (niwa-session-persistence-autosave)
            (should-not saved)
            (with-timeout (2 (ert-fail "Auto-save did not finish"))
              (while (< (length saved) 2) (sleep-for .002)))
            (should (= 2 (length saved)))
            (should saw-input)))
      (niwa-transport-cancel-idle
       niwa-session-persistence--autosaves 'conversation-autosave)
      (when input-timer (cancel-timer input-timer))
      (dolist (buffer (list first second))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer (set-buffer-modified-p nil))
          (kill-buffer buffer))))))

(niwa-deftest niwa-session-persistence-autosave-buffer/agent-transaction (:quiet t)
  (niwa-test-journal-capture--with-portable-session
   (lambda (session root)
     (with-current-buffer root
       (insert "Root prompt\n" (propertize "Response\n" 'gptel 'response))
       (niwa-session-artifacts-save session root))
     (let* ((child (generate-new-buffer " *autosave-agent*"))
            (invocation (niwa-agent-invocation--create
                         :agent (niwa-agent--create :name "worker")
                         :agent-id "worker--test" :path "/root/worker"
                         :buffer child :parent-session session :parent-data-buffer root))
            (publish (symbol-function 'niwa-session-publication-publish))
            (publications 0))
       (unwind-protect
           (progn
             (niwa-agent-runtime--setup-transcript invocation child)
             (with-current-buffer child
               (setq-local niwa--session session niwa--agent-invocation invocation)
               (insert "Partial agent response"))
             ;; Ordinary transcript changes do not mark the registry dirty.
             (setf (niwa-agent-invocation-sidecar-dirty invocation) nil)
             (cl-letf (((symbol-function 'niwa-session-publication-publish)
                        (lambda (&rest args)
                          (cl-incf publications) (apply publish args))))
               (should (niwa-session-persistence-autosave-buffer child)))
             (should (= publications 1))
             (should-not (niwa-session-publication-uncommitted-batches session))
             (should (equal "Partial agent response"
                            (niwa-session-artifacts-read-artifact
                             session (niwa-agent-invocation-transcript-relative-path invocation) t))))
         (with-current-buffer child
           (set-buffer-modified-p nil) (setq-local kill-buffer-hook nil))
         (kill-buffer child))))))

;;
;;; In-flight checkpoints

(niwa-deftest niwa-session-persistence-start-checkpoints
  (:after-each (niwa-session-persistence-stop-checkpoints))
  ,test
  (test)
  :doc "starts one repeating timer however often requests begin"
  (let ((niwa-session-checkpoint-interval 30))
    (niwa-session-persistence-start-checkpoints)
    (let ((timer niwa-session-persistence--checkpoint-timer))
      (should (memq timer timer-list))
      (should (= 30 (timer--repeat-delay timer)))
      (niwa-session-persistence-start-checkpoints)
      (should (eq timer niwa-session-persistence--checkpoint-timer))))
  :doc "a nil interval leaves mid-turn checkpoints to Emacs auto-save"
  (let ((niwa-session-checkpoint-interval nil))
    (niwa-session-persistence-start-checkpoints)
    (should-not niwa-session-persistence--checkpoint-timer)))

(niwa-deftest niwa-session-persistence-stop-checkpoints
  (:doc "cancels the in-flight checkpoint timer")
  (let ((niwa-session-checkpoint-interval 30))
    (niwa-session-persistence-start-checkpoints)
    (let ((timer niwa-session-persistence--checkpoint-timer))
      (niwa-session-persistence-stop-checkpoints)
      (should-not (memq timer timer-list))
      (should-not niwa-session-persistence--checkpoint-timer))))

(niwa-deftest niwa-session-persistence--request-in-flight-p
  (:doc "reports a request owned by any buffer")
  (let ((buffer (generate-new-buffer " *in-flight*")))
    (unwind-protect
        (cl-letf (((symbol-function 'buffer-list)
                   (lambda (&optional _frame) (list buffer))))
          (should-not (niwa-session-persistence--request-in-flight-p))
          (with-current-buffer buffer
            (setq-local niwa--current-request (niwa-request--create)))
          (should (niwa-session-persistence--request-in-flight-p)))
      (kill-buffer buffer))))

(niwa-deftest niwa-session-persistence--checkpoint-tick (:quiet t)
  ,test
  (test)
  :doc "checkpoints an unattended turn's partial response without input"
  (let* ((root (make-temp-file "niwa-checkpoint-tick-" t))
         (workspace
          (test-niwa-session-persistence--make-file-workspace root))
         (session (niwa-session-create "main" workspace))
         (buffer (generate-new-buffer " *checkpoint-tick*"))
         (niwa-session-checkpoint-interval 30))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (niwa-chat-prepare-transcript-buffer)
            (setq-local niwa--session session)
            (setq-local niwa--workspace workspace)
            (insert "Prompt\n")
            (niwa-session-artifacts-save session buffer)
            (goto-char (point-max))
            (insert (propertize "Streaming text\n" 'gptel 'response))
            (setq-local niwa--current-request
                        (niwa-request--create :session session)))
          (niwa-session-persistence-start-checkpoints)
          (cl-letf (((symbol-function 'buffer-list)
                     (lambda (&optional _frame) (list buffer))))
            (niwa-session-persistence--checkpoint-tick)
            (with-timeout (2 (ert-fail "Checkpoint remained queued"))
              (while (or (> (hash-table-count
                             niwa-session-persistence--autosaves) 0)
                         (niwa-session-publication-busy-p session))
                (sleep-for .002))))
          (should (string-search
                   "Streaming text"
                   (niwa-session-artifacts-read-artifact
                    session "segment-0001.chat.org" t)))
          ;; The request is still in flight, so checkpoints continue.
          (should (memq niwa-session-persistence--checkpoint-timer
                        timer-list)))
      (niwa-session-persistence-stop-checkpoints)
      (niwa-transport-cancel-idle
       niwa-session-persistence--autosaves 'conversation-autosave)
      (with-current-buffer buffer
        (setq-local niwa--current-request nil))
      (test-niwa-session-persistence--release-and-kill buffer session)
      (delete-directory root t)))
  :doc "stops once no request is in flight, after a final checkpoint"
  (let ((niwa-session-checkpoint-interval 30)
        (saved nil))
    (niwa-session-persistence-start-checkpoints)
    (cl-letf (((symbol-function 'buffer-list)
               (lambda (&optional _frame) nil))
              ((symbol-function 'niwa-session-persistence-autosave)
               (lambda () (setq saved t))))
      (niwa-session-persistence--checkpoint-tick))
    ;; A deferred change made after the last request settled is saved.
    (should saved)
    (should-not niwa-session-persistence--checkpoint-timer)))

(provide 'test-niwa-session-persistence-autosave)
;;; test-niwa-session-persistence-autosave.el ends here
