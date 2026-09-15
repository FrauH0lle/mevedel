;;; test-mevedel-session-clear-journal.el -- Clear journal integration -*- lexical-binding: t -*-

;;; Commentary:

;; Exercises the clear command against real saved sessions and journal storage.

;;; Code:

(require 'mevedel-journal-test-support
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "mevedel-journal-test-support"))
(require 'mevedel-skills-ui)

(mevedel-deftest mevedel-cmd--clear-journal ()
		 ,test
		 (test)
		 :doc "successful clear seals only completed evidence with its captured title"
		 (mevedel-test-journal-capture--with-session
		  (lambda (session buffer)
		    (setf (mevedel-session-name session) "Pre-clear title"
			  (mevedel-session-naming-state session) 'attempted)
		    (mevedel-test-journal-capture--turn session buffer "Check the runner" "Completed runner result")
		    (let* ((workspace (mevedel-session-workspace session))
			   (capture (car (mevedel-journal-capture-list workspace)))
			   (metadata (copy-tree (plist-get capture :metadata)))
			   (evidence (mevedel-journal-capture-evidence workspace capture))
			   (session-id (mevedel-session-session-id session))
			   (save-path (mevedel-session-save-path session)))
		      (with-current-buffer buffer
			(goto-char (point-max))
			(insert "Pending prompt\n" (propertize "Unfinished response" 'gptel 'response))
			(mevedel-test--with-captured-messages nil (mevedel-cmd--clear nil))
			(should-not (string-match-p (regexp-opt '("Completed runner result" "Unfinished response"))
						    (buffer-string))))
		      (should (= 2 (mevedel-session-current-segment session)))
		      (should (equal session-id (mevedel-session-session-id session)))
		      (should (equal save-path (mevedel-session-save-path session)))
		      (should (eq 'pending (mevedel-session-naming-state session)))
		      (should (eq 'clear (mevedel-journal-capture-trigger workspace capture)))
		      (setf (mevedel-session-name session) "Later title")
		      (should (equal (list capture) (mevedel-journal-capture-list workspace)))
		      (should (equal "Pre-clear title" (plist-get metadata :session-name)))
		      (should (equal '(1) (plist-get metadata :turns)))
		      (should (equal evidence (mevedel-journal-capture-evidence workspace capture)))
		      (should (string-match-p "Completed runner result" evidence))
		      (should-not (string-match-p (regexp-opt '("Pending prompt" "Unfinished response")) evidence))
		      (should (mevedel-journal-pins-present-p save-path)))))

		 :doc "repeated clear and root close preserve the first seal and published coverage"
		 (mevedel-test-journal-capture--with-session
		  (lambda (session buffer)
		    (mevedel-test-journal-capture--turn session buffer "Request" "Completed result")
		    (let* ((workspace (mevedel-session-workspace session))
			   (root (mevedel-workspace-root workspace))
			   (capture (car (mevedel-journal-capture-list workspace))))
		      (with-current-buffer buffer
			(mevedel-test--with-captured-messages nil
							      (mevedel-cmd--clear nil)
							      (mevedel-cmd--clear nil)))
		      (should (= 3 (mevedel-session-current-segment session)))
		      (should (equal (list capture) (mevedel-journal-capture-list workspace)))
		      (should (eq 'clear (mevedel-journal-capture-trigger workspace capture)))
		      (let* ((metadata (plist-put (mevedel-journal-capture--metadata capture) :trigger 'clear))
			     (entry (mevedel-journal-store-publish-digest
				     root metadata
				     "## Done\n- Observed: completed result (turn 1).\n\n## Learned\n- none\n\n## Surprised\n- none\n\n## Unfinished\n- none")))
			(with-current-buffer buffer
			  (mevedel-test--with-captured-messages nil (mevedel-cmd--clear nil))
			  (add-hook 'kill-buffer-hook #'mevedel-session-persistence-release-on-kill nil t)
			  (set-buffer-modified-p nil))
			(kill-buffer buffer)
			(should-not (buffer-live-p buffer))
			(should (equal (list capture) (mevedel-journal-capture-list workspace)))
			(should (eq 'clear (mevedel-journal-capture-trigger workspace capture)))
			(should (equal (list entry) (mevedel-journal-store-entries root)))
			(should (equal (plist-get metadata :turn-ids)
				       (mevedel-journal-store-covered-turns root)))))))

		 :doc "empty materialized clear creates neither a capture nor a digest"
		 (mevedel-test-journal-capture--with-session
		  (lambda (session buffer)
		    (mevedel-session-artifacts-save session buffer t)
		    (with-current-buffer buffer
		      (mevedel-test--with-captured-messages nil (mevedel-cmd--clear nil)))
		    (should (= 2 (mevedel-session-current-segment session)))
		    (should-not (mevedel-journal-capture-list (mevedel-session-workspace session) t))
		    (should-not (mevedel-journal-pins-present-p (mevedel-session-save-path session)))
		    (should-not (mevedel-journal-store-entries
				 (mevedel-workspace-root (mevedel-session-workspace session))))))

		 :doc "disabled journaling preserves an unsealed checkpoint across successful clear"
		 (mevedel-test-journal-capture--with-session
		  (lambda (session buffer)
		    (mevedel-test-journal-capture--turn session buffer "Request" "Completed result")
		    (let* ((workspace (mevedel-session-workspace session))
			   (capture (car (mevedel-journal-capture-list workspace)))
			   (mevedel-journal-enabled nil))
		      (with-current-buffer buffer
			(mevedel-test--with-captured-messages nil (mevedel-cmd--clear nil)))
		      (should (= 2 (mevedel-session-current-segment session)))
		      (should (equal (list capture) (mevedel-journal-capture-list workspace)))
		      (should-not (mevedel-journal-capture-trigger workspace capture))
		      (should (mevedel-journal-pins-present-p (mevedel-session-save-path session))))))

		 :doc "read-only and pending-input guards reject clear without sealing completed work"
		 (dolist (guard '(read-only steering follow-up))
		   (mevedel-test-journal-capture--with-session
		    (lambda (session buffer)
		      (mevedel-test-journal-capture--turn session buffer "Request" "Completed result")
		      (let* ((workspace (mevedel-session-workspace session))
			     (capture (car (mevedel-journal-capture-list workspace))))
			(with-current-buffer buffer
			  (let ((before (buffer-string)))
			    (if (eq guard 'read-only)
				(setq-local mevedel-session--read-only-mode t)
			      (mevedel-session-enqueue-pending-input session guard '(:input "Keep pending")))
			    (should-error (mevedel-cmd--clear nil) :type 'user-error)
			    (should (equal before (buffer-string)))))
			(should (= 1 (mevedel-session-current-segment session)))
			(should (equal (list capture) (mevedel-journal-capture-list workspace)))
			(should-not (mevedel-journal-capture-trigger workspace capture))))))

		 :doc "unmaterialized clear honors cancellation and never creates journal state"
		 (dolist (confirm '(nil t))
		   (mevedel-test-journal-capture--with-session
		    (lambda (session buffer)
		      (with-current-buffer buffer
			(insert "Unsaved unfinished conversation")
			(cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) confirm)))
			  (mevedel-test--with-captured-messages nil (mevedel-cmd--clear nil)))
			(if confirm
			    (should-not (string-match-p "Unsaved unfinished conversation" (buffer-string)))
			  (should (equal "Unsaved unfinished conversation" (buffer-string)))))
		      (should-not (mevedel-session-save-path session))
		      (should-not (mevedel-journal-capture-list (mevedel-session-workspace session) t)))))

		 :doc "precommit sidecar failure rolls back clear without sealing its checkpoint"
		 (mevedel-test-journal-capture--with-session
		  (lambda (session buffer)
		    (setf (mevedel-session-naming-state session) 'attempted)
		    (mevedel-test-journal-capture--turn session buffer "Request" "Completed result")
		    (let* ((workspace (mevedel-session-workspace session))
			   (capture (car (mevedel-journal-capture-list workspace)))
			   (save-path (mevedel-session-save-path session))
			   (sidecar (mevedel-session-artifacts-sidecar-path save-path))
			   (before-sidecar (mevedel-session-control-fs-read-file sidecar))
			   (write (symbol-function 'mevedel-session-codec-write))
			   failed)
		      (with-current-buffer buffer
			(let ((before (buffer-string)) (before-file buffer-file-name))
			  (cl-letf (((symbol-function 'mevedel-session-codec-write)
				     (lambda (&rest args)
				       (if failed (apply write args)
					 (setq failed t)
					 (error "Injected precommit sidecar failure")))))
			    (should-error (mevedel-cmd--clear nil)))
			  (should failed)
			  (should (equal before (buffer-string)))
			  (should (equal before-file buffer-file-name))))
		      (should (= 1 (mevedel-session-current-segment session)))
		      (should (eq 'attempted (mevedel-session-naming-state session)))
		      (should (equal before-sidecar (mevedel-session-control-fs-read-file sidecar)))
		      (should-not (file-exists-p (mevedel-session-artifacts-segment-path save-path 2)))
		      (should (equal (list capture) (mevedel-journal-capture-list workspace)))
		      (should-not (mevedel-journal-capture-trigger workspace capture))
		      (should (mevedel-journal-pins-present-p save-path)))))

		 :doc "postcommit failure still seals clear and retains the durable naming reset"
		 (mevedel-test-journal-capture--with-session
		  (lambda (session buffer)
		    (setf (mevedel-session-naming-state session) 'attempted)
		    (mevedel-test-journal-capture--turn session buffer "Request" "Completed result")
		    (let* ((workspace (mevedel-session-workspace session))
			   (capture (car (mevedel-journal-capture-list workspace))))
		      (with-current-buffer buffer
			(cl-letf (((symbol-function 'mevedel-session-artifacts-save-instructions)
				   (lambda (&rest _) (error "Injected postcommit failure"))))
			  (should-error (mevedel-cmd--clear nil))))
		      (should (= 2 (mevedel-session-current-segment session)))
		      (should (eq 'pending (mevedel-session-naming-state session)))
		      (let ((sidecar (mevedel-session-codec-read
				      (mevedel-session-artifacts-sidecar-path (mevedel-session-save-path session)))))
			(should (= 2 (plist-get sidecar :current-segment)))
			(should (eq 'pending (plist-get sidecar :naming-state))))
		      (should (eq 'clear (mevedel-journal-capture-trigger workspace capture)))
		      (should (equal (list capture) (mevedel-journal-capture-list workspace))))))

		 :doc "checkpoint and seal diagnostics do not fail a successful clear"
		 (dolist (seam '(mevedel-journal-capture-checkpoint mevedel-journal-capture--write-seal))
		   (mevedel-test-journal-capture--with-session
		    (lambda (session buffer)
		      (mevedel-test-journal-capture--turn session buffer "Request" "Completed result")
		      (let* ((workspace (mevedel-session-workspace session))
			     (capture (car (mevedel-journal-capture-list workspace)))
			     diagnostics)
			(with-current-buffer buffer
			  (cl-letf (((symbol-function seam)
				     (lambda (&rest _) (error "Injected journal failure"))))
			    (mevedel-test--with-captured-diagnostics diagnostics (mevedel-cmd--clear nil))))
			(should (string-match-p "Journal capture .* failed: Injected journal failure" diagnostics))
			(should (string-match-p "started a fresh chat segment" diagnostics))
			(should (= 2 (mevedel-session-current-segment session)))
			(should (equal (list capture) (mevedel-journal-capture-list workspace)))
			(should-not (mevedel-journal-capture-trigger workspace capture))
			(should (mevedel-journal-pins-present-p (mevedel-session-save-path session))))))))

(mevedel-deftest mevedel-cmd--clear-journal-portable (:quiet t)
		 ,test
		 (test)
		 :doc "portable project clear rolls back before CAS and preserves reset and seal after CAS"
		 (dolist (phase '(before-commit after-transition after-commit))
		   (mevedel-test-journal-capture--with-session
		    (lambda (session buffer)
		      (setf (mevedel-workspace-type (mevedel-session-workspace session)) 'project
			    (mevedel-session-authority-mode session) 'portable
			    (mevedel-session-naming-state session) 'attempted)
		      (mevedel-test-journal-capture--turn session buffer "Portable request" "Portable completed result")
		      (let* ((workspace (mevedel-session-workspace session))
			     (capture (car (mevedel-journal-capture-list workspace)))
			     (directory (mevedel-session-save-path session))
			     (old-head (plist-get (mevedel-session-publication session) :head))
			     (old-sidecar (mevedel-session-artifacts-read-artifact session "session.meta.el"))
			     (seam (pcase phase
				     ('before-commit 'mevedel-session-durability-commit-publication-head)
				     ('after-commit 'mevedel-session-publication--commit-marker-publication)
				     ('after-transition 'mevedel-session-artifacts--publish-remote-segment-transition)))
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
				    (mevedel-test--with-captured-diagnostics diagnostics (mevedel-cmd--clear nil))
				    (should (string-match-p "Injected portable clear failure" diagnostics)))
				(let ((err (should-error (mevedel-cmd--clear nil))))
				  (should (string-match-p "Injected portable clear failure" (error-message-string err))))))
			    (should injected)
			    (if (eq phase 'before-commit)
				(progn
				  (should (equal old-text (buffer-string)))
				  (should (equal old-file buffer-file-name))
				  (should (= 1 (mevedel-session-current-segment session)))
				  (should (eq 'attempted (mevedel-session-naming-state session)))
				  (should (equal old-head (plist-get (mevedel-session-publication session) :head)))
				  (should (equal old-sidecar (mevedel-session-artifacts-read-artifact session "session.meta.el")))
				  (should-not (mevedel-session-pending-publication session))
				  (should-not (mevedel-session-publication-queue session))
				  (should-not (mevedel-journal-capture-trigger workspace capture)))
			      (should-not (string-match-p "Portable completed result" (buffer-string)))
			      (should (= 2 (mevedel-session-current-segment session)))
			      (should (eq 'pending (mevedel-session-naming-state session)))
			      (should-not (equal old-head (plist-get (mevedel-session-publication session) :head)))
			      (let ((sidecar (car (read-from-string
						   (mevedel-session-artifacts-read-artifact
						    session "session.meta.el" t)))))
				(should (= 2 (plist-get sidecar :current-segment)))
				(should (eq 'pending (plist-get sidecar :naming-state))))
			      (ert-info ((format "Phase: %s; pending batch: %S; diagnostics: %s"
						 phase (not (null (mevedel-session-pending-publication session))) diagnostics))
					(should (eq 'clear (mevedel-journal-capture-trigger workspace capture)))))))
			(should (equal (plist-get (mevedel-session-publication session) :head)
				       (plist-get (mevedel-session-publication-read directory) :head)))
			(should (equal (list capture) (mevedel-journal-capture-list workspace)))
			(should (member old-head (mevedel-journal-pins-heads directory)))))))

		 :doc "portable project clear resumes the new head with old title, pending naming, and frozen evidence"
		 (mevedel-test-journal-capture--with-session
		  (lambda (session buffer)
		    (setf (mevedel-workspace-type (mevedel-session-workspace session)) 'project
			  (mevedel-session-authority-mode session) 'portable
			  (mevedel-session-name session) "Portable old title"
			  (mevedel-session-naming-state session) 'attempted)
		    (mevedel-test-journal-capture--turn session buffer "Portable request" "Portable completed result")
		    (let* ((workspace (mevedel-session-workspace session))
			   (capture (car (mevedel-journal-capture-list workspace)))
			   (evidence (mevedel-journal-capture-evidence workspace capture))
			   (directory (mevedel-session-save-path session))
			   (id (mevedel-session-session-id session))
			   (old-head (plist-get (mevedel-session-publication session) :head))
			   resumed resumed-session)
		      (unwind-protect
			  (progn
			    (with-current-buffer buffer (mevedel-cmd--clear nil))
			    (should-not (equal old-head (plist-get (mevedel-session-publication session) :head)))
			    (should (eq 'clear (mevedel-journal-capture-trigger workspace capture)))
			    (test-mevedel-session-persistence--release-and-kill buffer session)
			    (setq resumed (mevedel-session-persistence-restore directory nil nil workspace)
				  resumed-session (buffer-local-value 'mevedel--session resumed))
			    (should (= 2 (mevedel-session-current-segment resumed-session)))
			    (should (equal id (mevedel-session-session-id resumed-session)))
			    (should (equal directory (mevedel-session-save-path resumed-session)))
			    (should (equal "Portable old title" (mevedel-session-name resumed-session)))
			    (should (eq 'pending (mevedel-session-naming-state resumed-session)))
			    (should (equal (plist-get (mevedel-session-publication session) :head)
					   (plist-get (mevedel-session-publication resumed-session) :head)))
			    (with-current-buffer resumed
			      (should-not (string-match-p "Portable completed result" (buffer-string))))
			    (should (equal (list capture) (mevedel-journal-capture-list workspace)))
			    (should (equal evidence (mevedel-journal-capture-evidence workspace capture)))
			    (should (member old-head (mevedel-journal-pins-heads directory))))
			(let ((mevedel-journal-enabled nil))
			  (test-mevedel-session-persistence--release-and-kill resumed resumed-session)))))))

(mevedel-deftest mevedel-cmd--clear-journal-quit ()
		 (let ((phase ,phase))
		   (mevedel-test-journal-capture--with-session
		    (lambda (session buffer)
		      (setf (mevedel-session-naming-state session) 'attempted)
		      (mevedel-test-journal-capture--turn session buffer "Request" "Completed result")
		      (let* ((workspace (mevedel-session-workspace session))
			     (capture (car (mevedel-journal-capture-list workspace)))
			     (directory (mevedel-session-save-path session))
			     (seam (if (eq phase 'before-commit)
				       'mevedel-session-codec-write
				     'mevedel-session-artifacts-save-instructions))
			     (original (symbol-function seam))
			     injected)
			(with-current-buffer buffer
			  (let ((old-text (buffer-string)) (old-file buffer-file-name))
			    (cl-letf (((symbol-function seam)
				       (lambda (&rest args)
					 (if injected (apply original args)
					   (setq injected t)
					   (signal 'quit nil)))))
			      (should (eq 'quit (condition-case nil
						    (progn (mevedel-cmd--clear nil) 'returned)
						  (quit 'quit)))))
			    (should injected)
			    (ert-info ((format "Quit phase: %s" phase))
				      (if (eq phase 'before-commit)
					  (progn
					    (should (= 1 (mevedel-session-current-segment session)))
					    (should (eq 'attempted (mevedel-session-naming-state session)))
					    (should (equal old-text (buffer-string)))
					    (should (equal old-file buffer-file-name))
					    (should-not (file-exists-p (mevedel-session-artifacts-segment-path directory 2)))
					    (should-not (mevedel-journal-capture-trigger workspace capture)))
					(should (= 2 (mevedel-session-current-segment session)))
					(should (eq 'pending (mevedel-session-naming-state session)))
					(should (eq 'clear (mevedel-journal-capture-trigger workspace capture)))))))
			(should (equal (list capture) (mevedel-journal-capture-list workspace)))
			(should (mevedel-journal-pins-present-p directory))))))
		 (phase)
		 :doc "quit after PID-lock commit still seals clear"
		 'after-commit
		 :doc "quit before PID-lock commit restores naming and transcript without sealing"
		 'before-commit)

(mevedel-deftest mevedel-cmd--clear-journal-portable-quit (:quiet t)
		 (let ((phase ,phase))
		   (mevedel-test-journal-capture--with-session
		    (lambda (session buffer)
		      (setf (mevedel-workspace-type (mevedel-session-workspace session)) 'project
			    (mevedel-session-authority-mode session) 'portable
			    (mevedel-session-naming-state session) 'attempted)
		      (mevedel-test-journal-capture--turn session buffer "Request" "Completed portable result")
		      (let* ((workspace (mevedel-session-workspace session))
			     (capture (car (mevedel-journal-capture-list workspace)))
			     (directory (mevedel-session-save-path session))
			     (old-head (plist-get (mevedel-session-publication session) :head))
			     (transition (symbol-function 'mevedel-session-artifacts--publish-remote-segment-transition))
			     cancelled injected)
			(with-current-buffer buffer
			  (let ((old-text (buffer-string)) (old-file buffer-file-name))
			    (setq-local mevedel-session-naming--cancel (lambda () (setq cancelled t)))
			    (cl-letf (((symbol-function 'mevedel-session-artifacts--publish-remote-segment-transition)
				       (lambda (&rest args)
					 (setq injected t)
					 (when (eq phase 'after-transition) (apply transition args))
					 (signal 'quit nil))))
			      (should (eq 'quit (condition-case nil
						    (progn (mevedel-cmd--clear nil) 'returned)
						  (quit 'quit)))))
			    (should injected)
			    (ert-info ((format "Portable quit phase: %s" phase))
				      (if (eq phase 'before-transition)
					  (progn
					    (should (= 1 (mevedel-session-current-segment session)))
					    (should (eq 'attempted (mevedel-session-naming-state session)))
					    (should (equal old-text (buffer-string)))
					    (should (equal old-file buffer-file-name))
					    (should (equal old-head (plist-get (mevedel-session-publication session) :head)))
					    (should-not (mevedel-session-pending-publication session))
					    (should-not (mevedel-session-publication-queue session))
					    (should-not cancelled)
					    (should-not (mevedel-journal-capture-trigger workspace capture)))
					(should-not (equal old-head (plist-get (mevedel-session-publication session) :head)))
					(should (= 2 (mevedel-session-current-segment session)))
					(should (eq 'pending (mevedel-session-naming-state session)))
					(should (eq 'clear (mevedel-journal-capture-trigger workspace capture)))
					(should cancelled)))))
			(should (equal (plist-get (mevedel-session-publication session) :head)
				       (plist-get (mevedel-session-publication-read directory) :head)))
			(should (equal (list capture) (mevedel-journal-capture-list workspace)))
			(should (member old-head (mevedel-journal-pins-heads directory)))))))
		 (phase)
		 :doc "portable quit before transition restores naming and leaves completed work unsealed"
		 'before-transition
		 :doc "portable quit after committed transition cancels naming and seals the old work"
		 'after-transition)

(mevedel-deftest mevedel-session-publication-clear-recovery (:quiet t)
		 (let ((phase ,phase))
		   (mevedel-test-journal-capture--with-session
		    (lambda (session buffer)
		      (setf (mevedel-workspace-type (mevedel-session-workspace session)) 'project
			    (mevedel-session-authority-mode session) 'portable)
		      (mevedel-test-journal-capture--turn session buffer "Request" "Completed result")
		      (let* ((directory (mevedel-session-save-path session))
			     (marker (list :path (mevedel-session-artifacts-sidecar-path directory)
					   :content (mevedel-session-artifacts-read-artifact session "session.meta.el" t)
					   :commit-marker t))
			     (original (symbol-function 'mevedel-session-publication--commit-marker-publication))
			     (commits 0)
			     committed-batches queued)
			(mevedel-session-publication-publish
			 session (list (list :path (file-name-concat directory "local" "retained.txt")
					     :content "Retained before marker")))
			(cl-letf (((symbol-function 'mevedel-session-publication--commit-marker-publication)
				   (lambda (owner batches batch-marker)
				     (cl-incf commits)
				     (if (= commits 1)
					 (prog1 (funcall original owner batches batch-marker)
					   (setq committed-batches (copy-sequence batches))
					   (should (eq 'queued
						       (mevedel-session-publication-publish
							owner (list (list :path (file-name-concat directory "local" "queued.txt")
									  :content "Queued after marker")
								    marker))))
					   (setq queued (car (last (mevedel-session-publication-queue owner))))
					   (when (eq phase 'after-first-commit)
					     (error "Injected error after first marker")))
				       (error "Injected error before second marker")))))
			  (should-error (mevedel-session-publication-publish session (list marker))))
			(should (= commits (if (eq phase 'after-first-commit) 1 2)))
			(should (= 2 (length committed-batches)))
			(dolist (batch committed-batches)
			  (should-not (file-exists-p (plist-get batch :directory))))
			(should (equal (list queued) (plist-get (mevedel-session-pending-publication session) :batches)))
			(should (file-directory-p (plist-get queued :directory)))
			(should-not (mevedel-session-publication-uncommitted-batches session))
			(should-not (mevedel-session-publication-queue session))
			(should (equal "Retained before marker"
				       (mevedel-session-artifacts-read-artifact session "local/retained.txt" t)))
			(should-error (mevedel-session-artifacts-read-artifact session "local/queued.txt" t))
			(mevedel-session-publication-retry session)
			(should-not (mevedel-session-pending-publication session))
			(should-not (file-exists-p (plist-get queued :directory)))
			(should (equal "Queued after marker"
				       (mevedel-session-artifacts-read-artifact session "local/queued.txt" t)))))))
		 (phase)
		 :doc "post-CAS cleanup drops committed sources but preserves a later queued batch for retry"
		 'after-first-commit
		 :doc "failure before a second marker preserves that batch despite an earlier commit in the same drain"
		 'before-second-commit)

(mevedel-deftest mevedel-cmd--clear-journal-keyboard-quit (:quiet t)
		 ,test
		 (test)
		 :doc "ordinary pending keyboard quit during portable CAS waits for commit bookkeeping and sealing"
		 (mevedel-test-journal-capture--with-session
		  (lambda (session buffer)
		    (setf (mevedel-workspace-type (mevedel-session-workspace session)) 'project
			  (mevedel-session-authority-mode session) 'portable
			  (mevedel-session-naming-state session) 'attempted)
		    (mevedel-test-journal-capture--turn session buffer "Request" "Completed result")
		    (let* ((workspace (mevedel-session-workspace session))
			   (capture (car (mevedel-journal-capture-list workspace)))
			   (old-head (plist-get (mevedel-session-publication session) :head))
			   (original (symbol-function 'mevedel-session-durability-commit-publication-head))
			   injected)
		      (with-current-buffer buffer
			(cl-letf (((symbol-function 'mevedel-session-durability-commit-publication-head)
				   (lambda (&rest args)
				     (should inhibit-quit)
				     (setq injected t quit-flag t)
				     (apply original args))))
			  (unwind-protect
			      (should (eq 'quit (condition-case nil
						    (progn (mevedel-cmd--clear nil) 'returned)
						  (quit 'quit))))
			    (setq quit-flag nil))))
		      (should injected)
		      (should (= 2 (mevedel-session-current-segment session)))
		      (should (eq 'pending (mevedel-session-naming-state session)))
		      (should-not (equal old-head (plist-get (mevedel-session-publication session) :head)))
		      (should (equal (plist-get (mevedel-session-publication session) :head)
				     (plist-get (mevedel-session-publication-read (mevedel-session-save-path session)) :head)))
		      (should-not (mevedel-session-pending-publication session))
		      (should-not (mevedel-session-publication-queue session))
		      (should-not (mevedel-session-publication-uncommitted-batches session))
		      (should (eq 'clear (mevedel-journal-capture-trigger workspace capture)))
		      (should (mevedel-session-durability-lease-owned-p session))))))

(provide 'test-mevedel-session-clear-journal)
;;; test-mevedel-session-clear-journal.el ends here
