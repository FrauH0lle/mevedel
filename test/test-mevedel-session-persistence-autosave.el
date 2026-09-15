;;; test-mevedel-session-persistence-autosave.el --- Auto-save tests -*- lexical-binding: t -*-

;;; Commentary:

;; Native auto-save checkpoints and failure isolation using real storage.

;;; Code:

(require 'mevedel-session-test-support
         (file-name-concat
          (file-name-directory (or load-file-name buffer-file-name))
          "mevedel-session-test-support"))

(mevedel-deftest mevedel-session-persistence-autosave-buffer
		 (:quiet t)
		 ,test
		 (test)
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

(provide 'test-mevedel-session-persistence-autosave)
;;; test-mevedel-session-persistence-autosave.el ends here
