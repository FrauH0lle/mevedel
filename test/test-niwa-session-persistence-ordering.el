;;; test-niwa-session-persistence-ordering.el --- Publication ordering tests -*- lexical-binding: t -*-

;;; Commentary:
;; Exercise sidecar commits between root checkpoint phases using real storage.

;;; Code:

(require 'niwa-session-test-support
         (file-name-concat
          (file-name-directory (or buffer-file-name load-file-name))
          "niwa-session-test-support"))
(require 'niwa-turn)
(require 'niwa-session-checkpoint)

(niwa-deftest niwa-session-artifacts-call-with-checkpoint ()
		 ,test
		 (test)
		 :doc "isolates one session, permits other sessions, and unwinds on failure"
		 (let ((session (niwa-session--create))
		       (other (niwa-session--create)))
		   (should-error
		    (niwa-session-artifacts-call-with-checkpoint
		     session
		     (lambda ()
		       (should-error (niwa-session-artifacts-call-with-checkpoint session #'ignore)
				     :type 'user-error)
                               ;; Ordinary sidecars are allowed while preparing
                               ;; a transcript, but cannot advance its fields.
                               (should (= 0 (plist-get
                                             (niwa-session-checkpoint-project
                                              session '(:total-turn-count 99))
                                             :total-turn-count)))
		       (should (eq 'other (niwa-session-artifacts-call-with-checkpoint
					   other (lambda () 'other))))
		       (error "Injected checkpoint failure"))))
		   (should-not (memq session niwa-session-artifacts--preparing-sessions))))

(niwa-deftest niwa-session-persistence-save-agent-state/ordering ()
		 ,test
		 (test)
		 :doc "acknowledgement before deferred or failed root save preserves root checkpoint"
		 (dolist (failed-save '(nil t))
		   (let* ((tempdir (file-name-as-directory (make-temp-file "niwa-root-order-" t)))
			  (workspace (test-niwa-session-persistence--make-workspace tempdir))
			  (session (niwa-session-create "main" workspace))
			  (root (generate-new-buffer " *root-order*"))
			  (fsm (gptel-make-fsm :info (list :buffer root :niwa-request-id "root")))
			  restored directory old-index deferred entered)
		     (unwind-protect
			 (progn
			   (with-current-buffer root
			     (niwa-chat-prepare-transcript-buffer)
			     (setq-local niwa--session session niwa--workspace workspace)
			     (setq-local niwa--current-request (niwa-request--create :id "root" :turn 1))
			     (insert "Question\n\n" (propertize "Partial answer\n" 'gptel 'response))
			     (niwa-session-artifacts-save session root))
			   (setq directory (niwa-session-save-path session)
				 old-index (copy-tree (niwa-session-prompt-index session)))
			   (should (= 1 (plist-get (car (cdar old-index)) :cum-turn)))
			   (with-current-buffer root
			     (goto-char (point-max))
			     (insert (propertize "Final answer tail\n" 'gptel 'response)))
			   ;; Stop at the real scheduling seam, after the synchronous commit.
			   (cl-letf (((symbol-function 'niwa--defer-turn-steps)
				      (lambda (_machine steps &rest _) (setq deferred steps))))
			     (niwa--complete-turn fsm))
			   (should (memq #'niwa--turn-save deferred))
			   (should (= 1 (niwa-session-turn-count session)))
			   (when failed-save
			     (cl-letf (((symbol-function 'niwa-session-checkpoint-capture)
					(lambda (&rest _) (setq entered t) (error "Injected staging failure"))))
			       (niwa-test--with-captured-diagnostics nil
									(niwa--turn-save fsm)))
			     (should entered)
			     (should-not (plist-get (gptel-fsm-info fsm) :niwa-turn-saved)))
			   (niwa-agent-control-send-message session "/root" "Durable agent mail")
			   (test-niwa-session-persistence--release-and-kill root session)
			   (setq root nil restored (niwa-session-persistence-restore directory))
			   (with-current-buffer restored
			     (should (equal "Durable agent mail"
					    (plist-get (car (niwa-session-messages niwa--session)) :payload)))
			     (should (string-search "Partial answer" (buffer-string)))
                                     (should-not (string-search "Final answer tail" (buffer-string)))
                                     (should (= 0 (niwa-session-turn-count niwa--session)))
                                     (should (equal old-index (niwa-session-prompt-index niwa--session)))))
		       (test-niwa-session-persistence--release-and-kill root session)
		       (test-niwa-session-persistence--release-and-kill
			restored (and restored (buffer-local-value 'niwa--session restored)))
		       (delete-directory tempdir t)
		       (niwa-workspace-clear-registry))))

		 :doc "nested acknowledged hook mutation cannot commit tentative mailbox removal"
		 (dolist (route '(ack registry sidecar))
		   (let* ((tempdir (file-name-as-directory (make-temp-file "niwa-mail-order-" t)))
			  (workspace (test-niwa-session-persistence--make-workspace tempdir))
			  (session (niwa-session-create "main" workspace))
			  (root (generate-new-buffer " *mail-order*"))
			  restored directory entered)
		     (unwind-protect
			 (progn
			   (with-current-buffer root
			     (org-mode)
			     (setq-local niwa--session session)
			     (insert "Original prompt\n")
			     (niwa-session-artifacts-save session root))
			   (setq directory (niwa-session-save-path session))
			   (niwa-agent-control-send-message session "/root" "Captured mail A")
			   (with-current-buffer root
			     (setq-local before-save-hook
					 (list (lambda ()
						 (setq entered t)
						 (pcase route
						   ('ack (niwa-agent-control-send-message
							  session "/root" "Later mail B"))
						   ('registry (niwa-session-artifacts-save-agent-registry session root))
						   ('sidecar (niwa-session-persistence-write-sidecar-now session root)))
						 (error "Injected failure after nested sidecar commit")))))
                                   (niwa-tools--handle-message-inject
                                    (gptel-make-fsm :info (list :buffer root :backend nil
                                                               :data (list :messages (vector)))))
                                   (should-not entered)
                                   (should-not (niwa-session-messages session))
                                   (niwa-test--with-captured-diagnostics nil
                                     (should-error (niwa-session-artifacts-save session root)
                                                   :type 'error))
			   (should entered)
			   (with-current-buffer root (setq-local before-save-hook nil))
			   (test-niwa-session-persistence--release-and-kill root session)
			   (setq root nil restored (niwa-session-persistence-restore directory))
			   (with-current-buffer restored
                                     (should (seq-some (lambda (mail)
                                                         (equal "Captured mail A" (plist-get mail :payload)))
                                                       (niwa-session-messages niwa--session)))
                                     (should-not (string-search "Captured mail A" (buffer-string)))
                                     (when (eq route 'ack)
                                       (should (seq-some (lambda (mail)
                                                           (equal "Later mail B" (plist-get mail :payload)))
                                                         (niwa-session-messages niwa--session))))))
		       (when (buffer-live-p root)
			 (with-current-buffer root (setq-local before-save-hook nil)))
		       (test-niwa-session-persistence--release-and-kill root session)
		       (test-niwa-session-persistence--release-and-kill
			restored (and restored (buffer-local-value 'niwa--session restored)))
		       (delete-directory tempdir t)
		       (niwa-workspace-clear-registry)))))

(provide 'test-niwa-session-persistence-ordering)
;;; test-niwa-session-persistence-ordering.el ends here
