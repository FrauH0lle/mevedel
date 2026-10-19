;;; test-mevedel-session-persistence-ordering.el --- Publication ordering tests -*- lexical-binding: t -*-

;;; Commentary:
;; Exercise sidecar commits between root checkpoint phases using real storage.

;;; Code:

(require 'mevedel-session-test-support
         (file-name-concat
          (file-name-directory (or buffer-file-name load-file-name))
          "mevedel-session-test-support"))
(require 'mevedel-turn)

(mevedel-deftest mevedel-session-artifacts-call-with-checkpoint ()
		 ,test
		 (test)
		 :doc "isolates one session, permits other sessions, and unwinds on failure"
		 (let ((session (mevedel-session--create))
		       (other (mevedel-session--create)))
		   (should-error
		    (mevedel-session-artifacts-call-with-checkpoint
		     session
		     (lambda ()
		       (should-error (mevedel-session-artifacts-call-with-checkpoint session #'ignore)
				     :type 'user-error)
		       (should-error (mevedel-session-publication-publish session nil)
				     :type 'user-error)
		       (should (eq 'other (mevedel-session-artifacts-call-with-checkpoint
					   other (lambda () 'other))))
		       (error "Injected checkpoint failure"))))
		   (should-not (memq session mevedel-session-artifacts--preparing-sessions))))

(mevedel-deftest mevedel-session-persistence-save-agent-state/ordering ()
		 ,test
		 (test)
		 :doc "acknowledgement before deferred or failed root save preserves root checkpoint"
		 (dolist (failed-save '(nil t))
		   (let* ((tempdir (file-name-as-directory (make-temp-file "mevedel-root-order-" t)))
			  (workspace (test-mevedel-session-persistence--make-workspace tempdir))
			  (session (mevedel-session-create "main" workspace))
			  (root (generate-new-buffer " *root-order*"))
			  (fsm (gptel-make-fsm :info (list :buffer root :mevedel-request-id "root")))
			  restored directory old-index deferred entered)
		     (unwind-protect
			 (progn
			   (with-current-buffer root
			     (mevedel-chat-prepare-transcript-buffer)
			     (setq-local mevedel--session session mevedel--workspace workspace)
			     (setq-local mevedel--current-request (mevedel-request--create :id "root" :turn 1))
			     (insert "Question\n\n" (propertize "Partial answer\n" 'gptel 'response))
			     (mevedel-session-artifacts-save session root))
			   (setq directory (mevedel-session-save-path session)
				 old-index (copy-tree (mevedel-session-prompt-index session)))
			   (should (= 1 (plist-get (car (cdar old-index)) :cum-turn)))
			   (with-current-buffer root
			     (goto-char (point-max))
			     (insert (propertize "Final answer tail\n" 'gptel 'response)))
			   ;; Stop at the real scheduling seam, after the synchronous commit.
			   (cl-letf (((symbol-function 'mevedel--defer-turn-steps)
				      (lambda (_machine steps &rest _) (setq deferred steps))))
			     (mevedel--complete-turn fsm))
			   (should (memq #'mevedel--turn-save deferred))
			   (should (= 1 (mevedel-session-turn-count session)))
			   (when failed-save
			     (cl-letf (((symbol-function 'mevedel-session-artifacts--instruction-artifacts)
					(lambda (&rest _) (setq entered t) (error "Injected staging failure"))))
			       (mevedel-test--with-captured-diagnostics nil
									(mevedel--turn-save fsm)))
			     (should entered)
			     (should-not (plist-get (gptel-fsm-info fsm) :mevedel-turn-saved)))
			   (mevedel-agent-control-send-message session "/root" "Durable agent mail")
			   (test-mevedel-session-persistence--release-and-kill root session)
			   (setq root nil restored (mevedel-session-persistence-restore directory))
			   (with-current-buffer restored
			     (should (equal "Durable agent mail"
					    (plist-get (car (mevedel-session-messages mevedel--session)) :payload)))
			     (should (string-search "Partial answer" (buffer-string)))
			     (if (string-search "Final answer tail" (buffer-string))
				 (should (= 1 (mevedel-session-turn-count mevedel--session)))
			       (should (= 0 (mevedel-session-turn-count mevedel--session)))
			       (should (equal old-index (mevedel-session-prompt-index mevedel--session))))))
		       (test-mevedel-session-persistence--release-and-kill root session)
		       (test-mevedel-session-persistence--release-and-kill
			restored (and restored (buffer-local-value 'mevedel--session restored)))
		       (delete-directory tempdir t)
		       (mevedel-workspace-clear-registry))))

		 :doc "nested acknowledged hook mutation cannot commit tentative mailbox removal"
		 (dolist (route '(ack registry sidecar))
		   (let* ((tempdir (file-name-as-directory (make-temp-file "mevedel-mail-order-" t)))
			  (workspace (test-mevedel-session-persistence--make-workspace tempdir))
			  (session (mevedel-session-create "main" workspace))
			  (root (generate-new-buffer " *mail-order*"))
			  restored directory entered)
		     (unwind-protect
			 (progn
			   (with-current-buffer root
			     (org-mode)
			     (setq-local mevedel--session session)
			     (insert "Original prompt\n")
			     (mevedel-session-artifacts-save session root))
			   (setq directory (mevedel-session-save-path session))
			   (mevedel-agent-control-send-message session "/root" "Captured mail A")
			   (with-current-buffer root
			     (setq-local before-save-hook
					 (list (lambda ()
						 (setq entered t)
						 (pcase route
						   ('ack (mevedel-agent-control-send-message
							  session "/root" "Later mail B"))
						   ('registry (mevedel-session-artifacts-save-agent-registry session root))
						   ('sidecar (mevedel-session-persistence-write-sidecar-now session root)))
						 (error "Injected failure after nested sidecar commit")))))
			   (mevedel-test--with-captured-diagnostics nil
								    (should-error
								     (mevedel-tools--handle-message-inject
								      (gptel-make-fsm :info (list :buffer root :backend nil
												  :data (list :messages (vector)))))))
			   (should entered)
			   (with-current-buffer root (setq-local before-save-hook nil))
			   (test-mevedel-session-persistence--release-and-kill root session)
			   (setq root nil restored (mevedel-session-persistence-restore directory))
			   (with-current-buffer restored
			     (should (or (seq-some (lambda (mail)
						     (equal "Captured mail A" (plist-get mail :payload)))
						   (mevedel-session-messages mevedel--session))
					 (string-search "Captured mail A" (buffer-string))))))
		       (when (buffer-live-p root)
			 (with-current-buffer root (setq-local before-save-hook nil)))
		       (test-mevedel-session-persistence--release-and-kill root session)
		       (test-mevedel-session-persistence--release-and-kill
			restored (and restored (buffer-local-value 'mevedel--session restored)))
		       (delete-directory tempdir t)
		       (mevedel-workspace-clear-registry)))))

(provide 'test-mevedel-session-persistence-ordering)
;;; test-mevedel-session-persistence-ordering.el ends here
