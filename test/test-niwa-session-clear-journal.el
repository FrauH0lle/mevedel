;;; test-niwa-session-clear-journal.el -- Clear journal integration -*- lexical-binding: t -*-

;;; Commentary:

;; Exercises the clear command against real saved sessions and journal storage.

;;; Code:

(require 'niwa-journal-test-support
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "niwa-journal-test-support"))
(require 'niwa-skills-ui)

(niwa-deftest niwa-cmd--clear-journal ()
		 ,test
		 (test)
		 :doc "successful clear seals only completed evidence with its captured title"
		 (niwa-test-journal-capture--with-session
		  (lambda (session buffer)
		    (setf (niwa-session-name session) "Pre-clear title"
			  (niwa-session-naming-state session) 'attempted)
		    (niwa-test-journal-capture--turn session buffer "Check the runner" "Completed runner result")
		    (let* ((workspace (niwa-session-workspace session))
			   (capture (car (niwa-journal-capture-list workspace)))
			   (metadata (copy-tree (plist-get capture :metadata)))
			   (evidence (niwa-journal-capture-evidence workspace capture))
			   (session-id (niwa-session-session-id session))
			   (save-path (niwa-session-save-path session)))
		      (with-current-buffer buffer
			(goto-char (point-max))
			(insert "Pending prompt\n" (propertize "Unfinished response" 'gptel 'response))
			(niwa-test--with-captured-messages nil (niwa-cmd--clear nil))
			(should-not (string-match-p (regexp-opt '("Completed runner result" "Unfinished response"))
						    (buffer-string))))
		      (should (= 2 (niwa-session-current-segment session)))
		      (should (equal session-id (niwa-session-session-id session)))
		      (should (equal save-path (niwa-session-save-path session)))
		      (should (eq 'pending (niwa-session-naming-state session)))
		      (should (eq 'clear (niwa-journal-capture-trigger workspace capture)))
		      (setf (niwa-session-name session) "Later title")
		      (should (equal (list capture) (niwa-journal-capture-list workspace)))
		      (should (equal "Pre-clear title" (plist-get metadata :session-name)))
		      (should (equal '(1) (plist-get metadata :turns)))
		      (should (equal evidence (niwa-journal-capture-evidence workspace capture)))
		      (should (string-match-p "Completed runner result" evidence))
		      (should-not (string-match-p (regexp-opt '("Pending prompt" "Unfinished response")) evidence))
		      (should (niwa-journal-pins-present-p save-path)))))

		 :doc "repeated clear and root close preserve the first seal and published coverage"
		 (niwa-test-journal-capture--with-session
		  (lambda (session buffer)
		    (niwa-test-journal-capture--turn session buffer "Request" "Completed result")
		    (let* ((workspace (niwa-session-workspace session))
			   (root (niwa-workspace-root workspace))
			   (capture (car (niwa-journal-capture-list workspace))))
		      (with-current-buffer buffer
			(niwa-test--with-captured-messages nil
							      (niwa-cmd--clear nil)
							      (niwa-cmd--clear nil)))
		      (should (= 3 (niwa-session-current-segment session)))
		      (should (equal (list capture) (niwa-journal-capture-list workspace)))
		      (should (eq 'clear (niwa-journal-capture-trigger workspace capture)))
		      (let* ((metadata (plist-put (niwa-journal-capture--metadata capture) :trigger 'clear))
			     (entry (niwa-journal-store-publish-digest
				     root metadata
				     "## Done\n- Observed: completed result (turn 1).\n\n## Learned\n- none\n\n## Surprised\n- none\n\n## Unfinished\n- none")))
			(with-current-buffer buffer
			  (niwa-test--with-captured-messages nil (niwa-cmd--clear nil))
			  (add-hook 'kill-buffer-hook #'niwa-session-persistence-release-on-kill nil t)
			  (set-buffer-modified-p nil))
			(kill-buffer buffer)
			(should-not (buffer-live-p buffer))
			(should (equal (list capture) (niwa-journal-capture-list workspace)))
			(should (eq 'clear (niwa-journal-capture-trigger workspace capture)))
			(should (equal (list entry) (niwa-journal-store-entries root)))
			(should (equal (plist-get metadata :turn-ids)
				       (niwa-journal-store-covered-turns root)))))))

		 :doc "empty materialized clear creates neither a capture nor a digest"
		 (niwa-test-journal-capture--with-session
		  (lambda (session buffer)
		    (niwa-session-artifacts-save session buffer t)
		    (with-current-buffer buffer
		      (niwa-test--with-captured-messages nil (niwa-cmd--clear nil)))
		    (should (= 2 (niwa-session-current-segment session)))
		    (should-not (niwa-journal-capture-list (niwa-session-workspace session) t))
		    (should-not (niwa-journal-pins-present-p (niwa-session-save-path session)))
		    (should-not (niwa-journal-store-entries
				 (niwa-workspace-root (niwa-session-workspace session))))))

		 :doc "disabled journaling preserves an unsealed checkpoint across successful clear"
		 (niwa-test-journal-capture--with-session
		  (lambda (session buffer)
		    (niwa-test-journal-capture--turn session buffer "Request" "Completed result")
		    (let* ((workspace (niwa-session-workspace session))
			   (capture (car (niwa-journal-capture-list workspace)))
			   (niwa-journal-enabled nil))
		      (with-current-buffer buffer
			(niwa-test--with-captured-messages nil (niwa-cmd--clear nil)))
		      (should (= 2 (niwa-session-current-segment session)))
		      (should (equal (list capture) (niwa-journal-capture-list workspace)))
		      (should-not (niwa-journal-capture-trigger workspace capture))
		      (should (niwa-journal-pins-present-p (niwa-session-save-path session))))))

		 :doc "read-only and pending-input guards reject clear without sealing completed work"
		 (dolist (guard '(read-only steering follow-up))
		   (niwa-test-journal-capture--with-session
		    (lambda (session buffer)
		      (niwa-test-journal-capture--turn session buffer "Request" "Completed result")
		      (let* ((workspace (niwa-session-workspace session))
			     (capture (car (niwa-journal-capture-list workspace))))
			(with-current-buffer buffer
			  (let ((before (buffer-string)))
			    (if (eq guard 'read-only)
				(setq-local niwa-session--read-only-mode t)
			      (niwa-session-enqueue-pending-input session guard '(:input "Keep pending")))
			    (should-error (niwa-cmd--clear nil) :type 'user-error)
			    (should (equal before (buffer-string)))))
			(should (= 1 (niwa-session-current-segment session)))
			(should (equal (list capture) (niwa-journal-capture-list workspace)))
			(should-not (niwa-journal-capture-trigger workspace capture))))))

		 :doc "unmaterialized clear honors cancellation and never creates journal state"
		 (dolist (confirm '(nil t))
		   (niwa-test-journal-capture--with-session
		    (lambda (session buffer)
		      (with-current-buffer buffer
			(insert "Unsaved unfinished conversation")
			(cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) confirm)))
			  (niwa-test--with-captured-messages nil (niwa-cmd--clear nil)))
			(if confirm
			    (should-not (string-match-p "Unsaved unfinished conversation" (buffer-string)))
			  (should (equal "Unsaved unfinished conversation" (buffer-string)))))
		      (should-not (niwa-session-save-path session))
		      (should-not (niwa-journal-capture-list (niwa-session-workspace session) t)))))

		 :doc "checkpoint and seal diagnostics do not fail a successful clear"
		 (dolist (seam '(niwa-journal-capture-checkpoint niwa-journal-capture--write-seal))
		   (niwa-test-journal-capture--with-session
		    (lambda (session buffer)
		      (niwa-test-journal-capture--turn session buffer "Request" "Completed result")
		      (let* ((workspace (niwa-session-workspace session))
			     (capture (car (niwa-journal-capture-list workspace)))
			     diagnostics)
			(with-current-buffer buffer
			  (cl-letf (((symbol-function seam)
				     (lambda (&rest _) (error "Injected journal failure"))))
			    (niwa-test--with-captured-diagnostics diagnostics (niwa-cmd--clear nil))))
			(should (string-match-p "Journal capture .* failed: Injected journal failure" diagnostics))
			(should (string-match-p "started a fresh chat segment" diagnostics))
			(should (= 2 (niwa-session-current-segment session)))
			(should (equal (list capture) (niwa-journal-capture-list workspace)))
			(should-not (niwa-journal-capture-trigger workspace capture))
			(should (niwa-journal-pins-present-p (niwa-session-save-path session))))))))

(niwa-deftest niwa-cmd--clear-journal-portable (:quiet t)
		 ,test
		 (test)
		 :doc "portable project clear rolls back before CAS and preserves reset and seal after CAS"
		 (dolist (phase '(before-commit after-transition after-commit))
		   (niwa-test-journal-capture--with-session
		    (lambda (session buffer)
		      (setf (niwa-workspace-type (niwa-session-workspace session)) 'project
			    (niwa-session-authority-mode session) 'portable
			    (niwa-session-naming-state session) 'attempted)
		      (niwa-test-journal-capture--turn session buffer "Portable request" "Portable completed result")
		      (let* ((workspace (niwa-session-workspace session))
			     (capture (car (niwa-journal-capture-list workspace)))
			     (directory (niwa-session-save-path session))
			     (old-head (plist-get (niwa-session-publication session) :head))
			     (old-sidecar (niwa-session-artifacts-read-artifact session "session.meta.el"))
			     (seam (pcase phase
				     ('before-commit 'niwa-session-durability-commit-publication-head)
				     ('after-commit 'niwa-session-publication--commit-marker-publication)
				     ('after-transition 'niwa-session-artifacts--publish-remote-segment-transition)))
			     (original (symbol-function seam))
			     injected diagnostics)
			(should old-head)
			(should (equal old-head (plist-get capture :head)))
			(with-current-buffer buffer
			  (let ((old-text (buffer-string)) (old-file buffer-file-name))
			    (cl-letf (((symbol-function seam)
				       (lambda (&rest args)
					 (setq injected t)
					 (unless (eq phase 'before-commit) (apply original args))
					 (error "Injected portable clear failure"))))
			      (if (eq phase 'after-commit)
				  (progn
				    ;; Required publication reports post-CAS cleanup errors.
				    (niwa-test--with-captured-diagnostics diagnostics (niwa-cmd--clear nil))
				    (should (string-match-p "Injected portable clear failure" diagnostics)))
				(let ((err (should-error (niwa-cmd--clear nil))))
				  (should (string-match-p "Injected portable clear failure" (error-message-string err))))))
			    (should injected)
			    (if (eq phase 'before-commit)
				(progn
				  (should (equal old-text (buffer-string)))
				  (should (equal old-file buffer-file-name))
				  (should (= 1 (niwa-session-current-segment session)))
				  (should (eq 'attempted (niwa-session-naming-state session)))
				  (should (equal old-head (plist-get (niwa-session-publication session) :head)))
				  (should (equal old-sidecar (niwa-session-artifacts-read-artifact session "session.meta.el")))
				  (should-not (niwa-session-pending-publication session))
				  (should-not (niwa-session-publication-queue session))
				  (should-not (niwa-journal-capture-trigger workspace capture)))
			      (should-not (string-match-p "Portable completed result" (buffer-string)))
			      (should (= 2 (niwa-session-current-segment session)))
			      (should (eq 'pending (niwa-session-naming-state session)))
			      (should-not (equal old-head (plist-get (niwa-session-publication session) :head)))
			      (let ((sidecar (car (read-from-string
						   (niwa-session-artifacts-read-artifact
						    session "session.meta.el" t)))))
				(should (= 2 (plist-get sidecar :current-segment)))
				(should (eq 'pending (plist-get sidecar :naming-state))))
			      (ert-info ((format "Phase: %s; pending batch: %S; diagnostics: %s"
						 phase (not (null (niwa-session-pending-publication session))) diagnostics))
					(should (eq 'clear (niwa-journal-capture-trigger workspace capture)))))))
			(should (equal (plist-get (niwa-session-publication session) :head)
				       (plist-get (niwa-session-publication-read directory) :head)))
			(should (equal (list capture) (niwa-journal-capture-list workspace)))
			(should (member old-head (niwa-journal-pins-heads directory)))))))

		 :doc "portable project clear resumes the new head with old title, pending naming, and frozen evidence"
		 (niwa-test-journal-capture--with-session
		  (lambda (session buffer)
		    (setf (niwa-workspace-type (niwa-session-workspace session)) 'project
			  (niwa-session-authority-mode session) 'portable
			  (niwa-session-name session) "Portable old title"
			  (niwa-session-naming-state session) 'attempted)
		    (niwa-test-journal-capture--turn session buffer "Portable request" "Portable completed result")
		    (let* ((workspace (niwa-session-workspace session))
			   (capture (car (niwa-journal-capture-list workspace)))
			   (evidence (niwa-journal-capture-evidence workspace capture))
			   (directory (niwa-session-save-path session))
			   (id (niwa-session-session-id session))
			   (old-head (plist-get (niwa-session-publication session) :head))
			   resumed resumed-session)
		      (unwind-protect
			  (progn
			    (with-current-buffer buffer (niwa-cmd--clear nil))
			    (should-not (equal old-head (plist-get (niwa-session-publication session) :head)))
			    (should (eq 'clear (niwa-journal-capture-trigger workspace capture)))
			    (test-niwa-session-persistence--release-and-kill buffer session)
			    (setq resumed (niwa-session-persistence-restore directory nil nil workspace)
				  resumed-session (buffer-local-value 'niwa--session resumed))
			    (should (= 2 (niwa-session-current-segment resumed-session)))
			    (should (equal id (niwa-session-session-id resumed-session)))
			    (should (equal directory (niwa-session-save-path resumed-session)))
			    (should (equal "Portable old title" (niwa-session-name resumed-session)))
			    (should (eq 'pending (niwa-session-naming-state resumed-session)))
			    (should (equal (plist-get (niwa-session-publication session) :head)
					   (plist-get (niwa-session-publication resumed-session) :head)))
			    (with-current-buffer resumed
			      (should-not (string-match-p "Portable completed result" (buffer-string))))
			    (should (equal (list capture) (niwa-journal-capture-list workspace)))
			    (should (equal evidence (niwa-journal-capture-evidence workspace capture)))
			    (should (member old-head (niwa-journal-pins-heads directory))))
			(let ((niwa-journal-enabled nil))
			  (test-niwa-session-persistence--release-and-kill resumed resumed-session)))))))

(niwa-deftest niwa-cmd--clear-journal-portable-quit (:quiet t)
		 (let ((phase ,phase))
		   (niwa-test-journal-capture--with-session
		    (lambda (session buffer)
		      (setf (niwa-workspace-type (niwa-session-workspace session)) 'project
			    (niwa-session-authority-mode session) 'portable
			    (niwa-session-naming-state session) 'attempted)
		      (niwa-test-journal-capture--turn session buffer "Request" "Completed portable result")
		      (let* ((workspace (niwa-session-workspace session))
			     (capture (car (niwa-journal-capture-list workspace)))
			     (directory (niwa-session-save-path session))
			     (old-head (plist-get (niwa-session-publication session) :head))
			     (transition (symbol-function 'niwa-session-artifacts--publish-remote-segment-transition))
			     cancelled injected)
			(with-current-buffer buffer
			  (let ((old-text (buffer-string)) (old-file buffer-file-name))
			    (setq-local niwa-session-naming--cancel (lambda () (setq cancelled t)))
			    (cl-letf (((symbol-function 'niwa-session-artifacts--publish-remote-segment-transition)
				       (lambda (&rest args)
					 (setq injected t)
					 (when (eq phase 'after-transition) (apply transition args))
					 (signal 'quit nil))))
			      (should (eq 'quit (condition-case nil
						    (progn (niwa-cmd--clear nil) 'returned)
						  (quit 'quit)))))
			    (should injected)
			    (ert-info ((format "Portable quit phase: %s" phase))
				      (if (eq phase 'before-transition)
					  (progn
					    (should (= 1 (niwa-session-current-segment session)))
					    (should (eq 'attempted (niwa-session-naming-state session)))
					    (should (equal old-text (buffer-string)))
					    (should (equal old-file buffer-file-name))
					    (should (equal old-head (plist-get (niwa-session-publication session) :head)))
					    (should-not (niwa-session-pending-publication session))
					    (should-not (niwa-session-publication-queue session))
					    (should-not cancelled)
					    (should-not (niwa-journal-capture-trigger workspace capture)))
					(should-not (equal old-head (plist-get (niwa-session-publication session) :head)))
					(should (= 2 (niwa-session-current-segment session)))
					(should (eq 'pending (niwa-session-naming-state session)))
					(should (eq 'clear (niwa-journal-capture-trigger workspace capture)))
					(should cancelled)))))
			(should (equal (plist-get (niwa-session-publication session) :head)
				       (plist-get (niwa-session-publication-read directory) :head)))
			(should (equal (list capture) (niwa-journal-capture-list workspace)))
			(should (member old-head (niwa-journal-pins-heads directory)))))))
		 (phase)
		 :doc "portable quit before transition restores naming and leaves completed work unsealed"
		 'before-transition
		 :doc "portable quit after committed transition cancels naming and seals the old work"
		 'after-transition)

(niwa-deftest niwa-session-publication-clear-recovery (:quiet t)
		 (let ((phase ,phase))
		   (niwa-test-journal-capture--with-session
		    (lambda (session buffer)
		      (setf (niwa-workspace-type (niwa-session-workspace session)) 'project
			    (niwa-session-authority-mode session) 'portable)
		      (niwa-test-journal-capture--turn session buffer "Request" "Completed result")
		      (let* ((directory (niwa-session-save-path session))
			     (marker (list :path (niwa-session-artifacts-sidecar-path directory)
					   :content (niwa-session-artifacts-read-artifact session "session.meta.el" t)
					   :commit-marker t))
			     (original (symbol-function 'niwa-session-publication--commit-marker-publication))
			     (commits 0)
			     committed-batches queued)
			(niwa-session-publication-publish
			 session (list (list :path (file-name-concat directory "local" "retained.txt")
					     :content "Retained before marker")))
			(cl-letf (((symbol-function 'niwa-session-publication--commit-marker-publication)
				   (lambda (owner batches batch-marker)
				     (cl-incf commits)
				     (if (= commits 1)
					 (prog1 (funcall original owner batches batch-marker)
					   (setq committed-batches (copy-sequence batches))
					   (should (eq 'queued
						       (niwa-session-publication-publish
							owner (list (list :path (file-name-concat directory "local" "queued.txt")
									  :content "Queued after marker")
								    marker))))
					   (setq queued (car (last (niwa-session-publication-queue owner))))
					   (when (eq phase 'after-first-commit)
					     (error "Injected error after first marker")))
				       (error "Injected error before second marker")))))
			  (should-error (niwa-session-publication-publish session (list marker))))
			(should (= commits (if (eq phase 'after-first-commit) 1 2)))
			(should (= 2 (length committed-batches)))
			(dolist (batch committed-batches)
			  (should-not (file-exists-p (plist-get batch :directory))))
			(should (equal (list queued) (plist-get (niwa-session-pending-publication session) :batches)))
			(should (file-directory-p (plist-get queued :directory)))
			(should-not (niwa-session-publication-uncommitted-batches session))
			(should-not (niwa-session-publication-queue session))
			(should (equal "Retained before marker"
				       (niwa-session-artifacts-read-artifact session "local/retained.txt" t)))
			(should-error (niwa-session-artifacts-read-artifact session "local/queued.txt" t))
			(niwa-session-publication-retry session)
			(should-not (niwa-session-pending-publication session))
			(should-not (file-exists-p (plist-get queued :directory)))
			(should (equal "Queued after marker"
				       (niwa-session-artifacts-read-artifact session "local/queued.txt" t)))))))
		 (phase)
		 :doc "post-CAS cleanup drops committed sources but preserves a later queued batch for retry"
		 'after-first-commit
		 :doc "failure before a second marker preserves that batch despite an earlier commit in the same drain"
		 'before-second-commit)

(niwa-deftest niwa-cmd--clear-journal-keyboard-quit (:quiet t)
		 ,test
		 (test)
		 :doc "ordinary pending keyboard quit during portable CAS waits for commit bookkeeping and sealing"
		 (niwa-test-journal-capture--with-session
		  (lambda (session buffer)
		    (setf (niwa-workspace-type (niwa-session-workspace session)) 'project
			  (niwa-session-authority-mode session) 'portable
			  (niwa-session-naming-state session) 'attempted)
		    (niwa-test-journal-capture--turn session buffer "Request" "Completed result")
		    (let* ((workspace (niwa-session-workspace session))
			   (capture (car (niwa-journal-capture-list workspace)))
			   (old-head (plist-get (niwa-session-publication session) :head))
			   (original (symbol-function 'niwa-session-durability-commit-publication-head))
			   injected)
		      (with-current-buffer buffer
			(cl-letf (((symbol-function 'niwa-session-durability-commit-publication-head)
				   (lambda (&rest args)
				     (should inhibit-quit)
				     (setq injected t quit-flag t)
				     (apply original args))))
			  (unwind-protect
			      (should (eq 'quit (condition-case nil
						    (progn (niwa-cmd--clear nil) 'returned)
						  (quit 'quit))))
			    (setq quit-flag nil))))
		      (should injected)
		      (should (= 2 (niwa-session-current-segment session)))
		      (should (eq 'pending (niwa-session-naming-state session)))
		      (should-not (equal old-head (plist-get (niwa-session-publication session) :head)))
		      (should (equal (plist-get (niwa-session-publication session) :head)
				     (plist-get (niwa-session-publication-read (niwa-session-save-path session)) :head)))
		      (should-not (niwa-session-pending-publication session))
		      (should-not (niwa-session-publication-queue session))
		      (should-not (niwa-session-publication-uncommitted-batches session))
		      (should (eq 'clear (niwa-journal-capture-trigger workspace capture)))
		      (should (niwa-session-durability-lease-owned-p session))))))

(provide 'test-niwa-session-clear-journal)
;;; test-niwa-session-clear-journal.el ends here
