;;; test-mevedel-chat.el --- Tests for chat buffer management -*- lexical-binding: t -*-

;;; Commentary:

;; Focused coverage for chat-buffer setup helpers.

;;; Code:

(require 'mevedel-chat)
(require 'mevedel)
(require 'mevedel-permission-queue)
(require 'mevedel-tool-plan)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name
               load-file-name
               byte-compile-current-file))
          "helpers"))

(defvar mevedel-chat-test--hook-events nil)

(defun mevedel-chat-test--record-hook (event)
  "Record lifecycle hook EVENT for tests."
  (push event mevedel-chat-test--hook-events)
  nil)

(defun mevedel-chat-test--session-context-hook (event)
  "Record lifecycle hook EVENT and return startup context."
  (push event mevedel-chat-test--hook-events)
  '(:additional-context ("startup context")))


;;
;;; Org element cache handling

(mevedel-deftest mevedel-load-order ()
		 ,test
		 (test)

		 :doc "`mevedel' loads preset definitions used by `mevedel-install'"
		 (should (fboundp 'mevedel--define-presets)))

(mevedel-deftest mevedel-uninstall ()
		 ,test
		 (test)

		 :doc "tears down skill hot-reload lifecycle state"
		 (let ((gptel--known-tools gptel--known-tools)
		       (gptel--known-presets gptel--known-presets)
		       (gptel-prompt-transform-functions gptel-prompt-transform-functions)
		       called)
		   (cl-letf (((symbol-function 'mevedel-skills-uninstall-hot-reload)
			      (lambda () (setq called t)))
			     ((symbol-function 'mevedel-skills-uninstall-slash-commands)
			      #'ignore)
			     ((symbol-function 'mevedel-pipeline-uninstall-tool-result-scrubber)
			      #'ignore)
			     ((symbol-function 'mevedel-view-uninstall-gptel-menu-advice)
			      #'ignore))
		     (mevedel-uninstall))
		   (should called)))


(mevedel-deftest mevedel--chat-buffer-disable-org-element-cache ()
		 ,test
		 (test)

		 :doc "disables Org's element cache buffer-locally"
		 (with-temp-buffer
		   (org-mode)
		   (setq-local org-element-use-cache t)
		   (setq-local org-element-cache-persistent t)
		   (mevedel--chat-buffer-disable-org-element-cache)
		   (should-not org-element-use-cache)
		   (should-not org-element-cache-persistent))
		 :doc "disables configured transcript minor modes"
		 (with-temp-buffer
		   (org-mode)
		   (let ((mevedel-transcript-disabled-minor-modes
			  '(org-indent-mode visual-line-mode)))
		     (org-indent-mode +1)
		     (visual-line-mode +1)
		     (mevedel--chat-buffer-disable-org-element-cache)
		     (should-not org-indent-mode)
		     (should-not visual-line-mode)))
		 :doc "keeps gptel Org prompt preparation on the fast path"
		 (with-temp-buffer
		   (org-mode)
		   (setq-local gptel-org-ignore-elements
			       '(property-drawer src-block))
		   (mevedel--chat-buffer-disable-org-element-cache)
		   (should (equal '(property-drawer)
				  gptel-org-ignore-elements))))

(mevedel-deftest mevedel--chat-buffer-setup ()
		 ,test
		 (test)

		 :doc "does not install Org agenda menus while entering org-mode"
		 (let* ((root (file-name-as-directory
			       (make-temp-file "mevedel-chat-menu-" t)))
		  (workspace (mevedel-workspace--create
			      :type 'project
			      :id root
			      :root root
			      :name "menu"))
		 menu-called)
		   (unwind-protect
		       (with-temp-buffer
			 (let ((org-agenda-file-menu-enabled t)
			       (org-mode-hook
				(cons (lambda () (org-indent-mode +1))
				      org-mode-hook)))
			   (cl-letf (((symbol-function 'org-install-agenda-files-menu)
				      (lambda ()
					(setq menu-called t)
					(error "menu setup should not run")))
				     ((symbol-function 'gptel-mode)
				      #'ignore)
				     ((symbol-function
				       'mevedel--chat-buffer-init-common)
				      #'ignore))
			     (let ((gptel-org-branching-context t))
			       (mevedel--chat-buffer-setup
				(current-buffer) workspace "main" root))))
				 (should (derived-mode-p 'org-mode))
				 (should-not gptel-org-convert-response)
				 (should-not gptel-org-branching-context)
				 (should (equal '(property-drawer)
						gptel-org-ignore-elements))
				 (should-not org-indent-mode)
				 (should-not menu-called))
		     (delete-directory root t))))

(mevedel-deftest mevedel-session-lifecycle-hooks
		 (:doc "runs normal and declarative session lifecycle hooks")
		 (let* ((root (file-name-as-directory
			       (make-temp-file "mevedel-chat-hooks-" t)))
			(workspace (mevedel-workspace--create
				    :type 'project
				    :id root
				    :root root
				    :name "hooks"))
			(session (mevedel-session-create "main" workspace root))
			(normal-events nil)
			(mevedel-chat-test--hook-events nil)
			(mevedel-session-start-hook
			 (list (lambda () (push 'start normal-events))))
			(mevedel-session-end-hook
			 (list (lambda () (push 'end normal-events))))
			(mevedel-hook-rules
			 '((SessionStart
			    ((:matcher "startup"
				       :hooks ((:type elisp
						      :function
						      mevedel-chat-test--session-context-hook)))))
			   (SessionEnd
			    ((:matcher "kill-buffer"
				       :hooks ((:type elisp
						      :function mevedel-chat-test--record-hook))))))))
		   (unwind-protect
		       (with-temp-buffer
			 (setq-local mevedel--session session)
			 (setq-local mevedel--workspace workspace)
			 (mevedel--run-session-start-hooks)
			 (mevedel--run-session-end-hooks)
			 (should (equal (nreverse normal-events) '(start end)))
			 (should
			  (equal
			   (mapcar (lambda (event)
				     (plist-get event :hook-event-name))
				   (nreverse mevedel-chat-test--hook-events))
			   '(SessionStart SessionEnd)))
			 (should (equal (mevedel-session-hook-context-pending session)
					'("startup context"))))
		     (delete-directory root t))))

(mevedel-deftest mevedel-session-start-hooks-wait
		 (:doc "waits for asynchronous SessionStart context before returning")
		 (let* ((root (file-name-as-directory
			       (make-temp-file "mevedel-chat-hooks-wait-" t)))
			(workspace (mevedel-workspace--create
				    :type 'project
				    :id root
				    :root root
				    :name "hooks"))
			(session (mevedel-session-create "main" workspace root))
			(called nil))
		   (unwind-protect
		       (with-temp-buffer
			 (setq-local mevedel--session session)
			 (setq-local mevedel--workspace workspace)
			 (cl-letf (((symbol-function 'mevedel-hooks-run-event)
				    (lambda (_event _payload callback &rest _)
				      (run-at-time
				       0.01 nil
				       (lambda ()
					 (setq called t)
					 (funcall callback
						  '(:additional-context
						    ("async startup"))))))))
			   (mevedel--run-session-start-hooks))
			 (should called)
			 (should-not mevedel--session-start-hooks-pending)
			 (should (equal (mevedel-session-hook-context-pending session)
					'("async startup"))))
		     (delete-directory root t))))


;;
;;; Working directory sessions

(mevedel-deftest mevedel-session-working-directory
		 (:before-each (mevedel-workspace-clear-registry)
			       :vars* ((root-dir (file-name-as-directory
						  (make-temp-file "mevedel-chat-cwd-" t)))
				       (module-dir (file-name-concat root-dir "packages" "api"))
				       chat-buffer)
			       :after-each
			       (progn
				 (mevedel-workspace-clear-registry)
				 (when (and chat-buffer (buffer-live-p chat-buffer))
				   (let ((view-buf (buffer-local-value 'mevedel--view-buffer
								       chat-buffer)))
				     (when (buffer-live-p view-buf)
				       (kill-buffer view-buf)))
				   (kill-buffer chat-buffer))
				 (delete-directory root-dir t)))
		 ,test
		 (test)

		 :doc "fresh chat sessions use the selected working directory"
		 (progn
		   (make-directory module-dir t)
		   (let* ((workspace (mevedel-workspace-get-or-create
				      'project root-dir root-dir "cwd-proj")))
		     (setq chat-buffer
			   (mevedel--chat-buffer "packages:api" t workspace module-dir))
		     (with-current-buffer chat-buffer
		       (should (equal (file-name-as-directory module-dir)
				      (mevedel-session-working-directory mevedel--session)))
		       (should (equal (file-name-as-directory module-dir)
				      default-directory)))))

		 :doc "session name defaults to the directory path below the workspace root"
		 (let ((workspace (mevedel-workspace-get-or-create
				   'project root-dir root-dir "cwd-proj")))
		   (should (equal "main"
				  (mevedel--default-session-name-for-directory
				   workspace root-dir)))
		   (should (equal "packages:api"
				  (mevedel--default-session-name-for-directory
				   workspace module-dir))))

		 :doc "no-prefix start switches to the only live session across directories"
		 (progn
		   (make-directory module-dir t)
		   (let* ((workspace (mevedel-workspace-get-or-create
				      'project root-dir root-dir "cwd-proj"))
			  displayed)
		     (setq chat-buffer
			   (mevedel--chat-buffer "packages:api" t workspace module-dir))
		     (cl-letf (((symbol-function 'mevedel--display-chat-buffer)
				(lambda (buf)
				  (setq displayed buf))))
		       (mevedel--start-chat workspace root-dir nil nil))
		     (should (eq displayed chat-buffer))
		     (with-current-buffer displayed
		       (should (equal (file-name-as-directory module-dir)
				      (mevedel-session-working-directory mevedel--session)))))))


;;
;;; Directive processing

(mevedel-deftest mevedel--directive-save-buffer-p ()
  ,test
  (test)

  :doc "accepts normal modified file buffers"
  (with-temp-buffer
    (setq buffer-file-name "/tmp/mevedel-source.el")
    (set-buffer-modified-p t)
    (should (mevedel--directive-save-buffer-p)))

  :doc "skips mevedel data and agent transcript buffers"
  (with-temp-buffer
    (setq buffer-file-name "/tmp/segment-0001.chat.org")
    (setq-local mevedel--session 'session)
    (set-buffer-modified-p t)
    (should-not (mevedel--directive-save-buffer-p)))
  (with-temp-buffer
    (setq buffer-file-name "/tmp/agents/explorer.chat.org")
    (setq-local mevedel--agent-invocation 'invocation)
    (set-buffer-modified-p t)
    (should-not (mevedel--directive-save-buffer-p))))

(mevedel-deftest mevedel--process-directives-sequentially ()
  ,test
  (test)

  :doc "defers the next directive until terminal request cleanup can finish"
  (let ((buf (generate-new-buffer " *mevedel-directives-sequential*"))
        ov1 ov2 calls scheduled-fn scheduled-args)
    (unwind-protect
        (progn
          (with-current-buffer buf
            (insert "one\ntwo\n")
            (setq ov1 (make-overlay (point-min) (line-end-position)))
            (overlay-put ov1 'mevedel-id 1)
            (overlay-put ov1 'mevedel-directive-text "first")
            (forward-line 1)
            (setq ov2 (make-overlay (point) (line-end-position)))
            (overlay-put ov2 'mevedel-id 2)
            (overlay-put ov2 'mevedel-directive-text "second"))
          (cl-letf (((symbol-function 'mevedel--directive-text)
                     (lambda (directive)
                       (overlay-get directive 'mevedel-directive-text)))
                    ((symbol-function 'mevedel--process-directive)
                     (lambda (directive _preset _prompt-fn callback)
                       (push (overlay-get directive 'mevedel-id) calls)
                       (funcall callback nil nil)))
                    ((symbol-function 'run-at-time)
                     (lambda (_secs _repeat function &rest args)
                       (setq scheduled-fn function
                             scheduled-args args)
                       'timer)))
            (mevedel--process-directives-sequentially (list ov1 ov2) 1 2)
            (should (equal '(1) calls))
            (should (eq scheduled-fn #'mevedel--process-directives-sequentially))
            (should (equal (list (list ov2) 2 2) scheduled-args))
            (apply scheduled-fn scheduled-args)
            (should (equal '(2 1) calls))))
      (when (buffer-live-p buf)
        (kill-buffer buf)))))

(mevedel-deftest mevedel--process-directive
		 (:before-each (mevedel-workspace-clear-registry)
			       :after-each (mevedel-workspace-clear-registry))
		 ,test
		 (test)

		 :doc "writes directive transcript, starts view turn, and sends full prompt directly"
		 (let* ((tmpdir (file-name-as-directory
				 (make-temp-file "mevedel-directive-" t)))
			(file (file-name-concat tmpdir "sample.txt"))
			(buf (find-file-noselect file))
			captured-prompt captured-args captured-fsm captured-chat
			callback-result)
		   (unwind-protect
		       (with-current-buffer buf
			 (erase-buffer)
			 (insert "alpha\nbeta\n")
			 (write-region (point-min) (point-max) file nil 'silent)
			 (set-buffer-modified-p nil)
			 (goto-char (point-min))
			 (let ((directive (mevedel--create-directive-in
					   buf (point-min) (line-end-position)
					   nil "Change alpha.")))
			   (overlay-put directive 'mevedel-directive-action 'implement)
			   (cl-letf (((symbol-function 'save-some-buffers)
				      (lambda (&rest _) nil))
				     ((symbol-function 'display-buffer)
				      (lambda (&rest _) nil))
				     ((symbol-function 'gptel--apply-preset)
				      (lambda (&rest _) nil))
				     ((symbol-function 'gptel-request)
				      (lambda (prompt &rest args)
					(setq captured-prompt prompt
					      captured-args args
					      captured-chat (plist-get args :buffer))
					(let ((fsm (plist-get args :fsm)))
					  (setf (gptel-fsm-info fsm)
						(list :buffer captured-chat
						      :position (plist-get args :position)
						      :callback (lambda (&rest _) nil)))
					  (setq captured-fsm fsm)
					  fsm))))
			     (mevedel--process-directive
			      directive '(:system "test")
			      #'mevedel--implement-directive-prompt
			      (lambda (err fsm)
				(setq callback-result
				      (list err (eq fsm captured-fsm)))))
			     (should (string-match-p "IMPLEMENTATION REQUEST"
						     captured-prompt))
			     (should (string-match-p "Change alpha" captured-prompt))
			     (should (eq captured-chat (plist-get captured-args :buffer)))
			     (should (markerp (plist-get captured-args :position)))
			     (with-current-buffer captured-chat
			       (should (eq 'processing
					   (overlay-get directive
							'mevedel-directive-status)))
			       (should (equal (overlay-get directive 'mevedel-uuid)
					      mevedel--current-directive-uuid))
			       (save-excursion
				 (goto-char (point-min))
				 (should (search-forward ":PROMPT:" nil t))
				 (should (eq 'ignore (get-text-property (point) 'gptel)))))
			     (let ((view-buf (buffer-local-value 'mevedel--view-buffer
								 captured-chat)))
			       (should (buffer-live-p view-buf))
			       (with-current-buffer view-buf
				 (let ((view-text (buffer-substring-no-properties
						   (point-min) mevedel-view--input-marker)))
				   (should (string-match-p "Implement: Change alpha"
							   view-text))
				   (should (string-match-p "Prompt" view-text)))
				 (goto-char (point-min))
				 (search-forward "Implement:")
				 (should (eq 'mevedel-view-directive-action
					     (get-text-property (match-beginning 0)
								'font-lock-face)))
				 (should (markerp mevedel-view--in-flight-turn-start))
				 (should (markerp mevedel-view--data-turn-start))
				 (should mevedel-view--spinner-overlay)))
			     (let ((response-start
				    (overlay-get directive
						 'mevedel-directive-response-start)))
			       (should (markerp response-start))
			       (with-current-buffer captured-chat
				 (goto-char response-start)
				 (insert (propertize "Answer text.\n" 'gptel 'response)))
			       (let ((view-buf (buffer-local-value 'mevedel--view-buffer
								   captured-chat)))
				 (with-current-buffer view-buf
				   (mevedel-view--render-incremental captured-chat)))
			       (overlay-put directive 'mevedel-directive-response-start nil)
			       (cl-letf (((symbol-function
					   'mevedel--ov-actions--find-directive-response-start)
					  (lambda (&rest _) nil)))
				 (let ((gptel--fsm-last nil))
				   (mevedel--ov-actions-show-answer directive)))
			       (let ((view-buf (buffer-local-value 'mevedel--view-buffer
								   captured-chat)))
				 (with-current-buffer view-buf
				   (should (looking-at-p "Answer text")))))
			     (funcall (plist-get (gptel-fsm-info captured-fsm)
						 :mevedel-request-callback)
				      nil captured-fsm)
			     (should (equal '(nil t) callback-result))
			     (should (eq 'succeeded
					 (overlay-get directive
						      'mevedel-directive-status)))
			     (with-current-buffer captured-chat
			       (should-not mevedel--current-directive-uuid)))))
		     (when (buffer-live-p buf)
		       (kill-buffer buf))
		     (when (and captured-chat (buffer-live-p captured-chat))
		       (let ((view-buf (buffer-local-value 'mevedel--view-buffer
							   captured-chat)))
			 (when (buffer-live-p view-buf)
			   (kill-buffer view-buf)))
		       (kill-buffer captured-chat))
		     (delete-directory tmpdir t))))


;;
;;; Abort

(mevedel-deftest mevedel-abort
		 (:doc "aborts active chat request state")
		 ,test
		 (test)

		 :doc "flushes permission and plan queues"
		 (with-temp-buffer
		   (let* ((workspace (mevedel-workspace--create
				      :type 'project
				      :id "/tmp/mevedel-chat-abort/"
				      :root "/tmp/mevedel-chat-abort/"
				      :name "abort"))
			  (session (mevedel-session-create "main" workspace))
			  (outcomes nil))
		     (setq-local mevedel--session session)
		     (mevedel-request-begin session)
		     (setf (mevedel-session-permission-queue session)
			   (list (list :kind 'generic
				       :tool-name "Read"
				       :session session
				       :callback
				       (lambda (outcome)
					 (push (cons 'permission outcome) outcomes)))))
		     (setf (mevedel-session-plan-queue session)
			   (list (list :body "# Plan"
				       :chat-buffer (current-buffer)
				       :session session
				       :callback
				       (lambda (outcome)
					 (push (cons 'plan outcome) outcomes)))))
		     (mevedel-abort (current-buffer))
		     (should (null (mevedel-session-permission-queue session)))
		     (should (null (mevedel-session-plan-queue session)))
		     (should (null mevedel--current-request))
		     (should (equal '((plan . aborted) (permission . aborted))
				    outcomes))))

			 :doc "stops registered agents with no live request process"
			 (with-temp-buffer
			   (let* ((workspace (mevedel-workspace--create
					      :type 'project
					      :id "/tmp/mevedel-chat-abort-agents/"
					      :root "/tmp/mevedel-chat-abort-agents/"
					      :name "abort-agents"))
				  (session (mevedel-session-create "main" workspace))
				  (invocation (mevedel-agent-invocation--create
					       :agent-id "explorer--parked"
					       :description "parked"
					       :transcript-status 'running))
				  (stopped nil)
				  (gptel--request-alist nil))
			     (setq-local mevedel--session session)
			     (setq-local mevedel-tools--agents-fsm
					 (list (cons "explorer--parked"
						     (gptel-make-fsm
						      :info
						      (list
						       :mevedel-agent-invocation
						       invocation)))))
			     (cl-letf (((symbol-function 'mevedel-tools-stop-agent)
					(lambda (agent-id reason parent-buffer)
					  (push (list agent-id reason parent-buffer)
						stopped)
					  (setq mevedel-tools--agents-fsm
						(assoc-delete-all
						 agent-id
						 mevedel-tools--agents-fsm)))))
			       (mevedel-abort (current-buffer)))
			     (should (equal "explorer--parked" (caar stopped)))
			     (should (equal "parent request aborted" (cadar stopped)))
			     (should (eq (caddar stopped) (current-buffer)))
			     (should (null mevedel-tools--agents-fsm)))))


;;
;;; Plan implementation permission mode

(mevedel-deftest mevedel--implementation-permission-mode-apply
             (:doc "temporarily applies and restores implementation permission mode")
             ,test
             (test)

             :doc "temporarily applies and restores implementation permission mode"
             (let* ((session (mevedel-session--create
                              :name "test"
                              :workspace nil
                              :permission-mode 'default
                              :permission-rules nil
                              :permission-queue nil
                              :plan-queue nil))
                    (buffer (generate-new-buffer " *mev-chat-mode*"))
                    (refreshed 0))
               (unwind-protect
                   (cl-letf (((symbol-function 'mevedel-skills--refresh-view-input-prompt)
                              (lambda () (cl-incf refreshed))))
                     (with-current-buffer buffer
                       (setq-local mevedel--session session)
                       (mevedel--implementation-permission-mode-apply 'accept-edits)
                       (should (eq 'accept-edits
                                   (mevedel-session-permission-mode session)))
                       (should (equal '(default)
                                      mevedel--implementation-permission-mode-restore))
                       (mevedel--implementation-permission-mode-restore)
                       (should (eq 'default
                                   (mevedel-session-permission-mode session)))
                       (should-not mevedel--implementation-permission-mode-restore)
                       (should (= 2 refreshed))))
                 (when (buffer-live-p buffer) (kill-buffer buffer))))

             :doc "restores inherited global permission mode as nil session override"
             (let* ((session (mevedel-session--create
                              :name "test"
                              :workspace nil
                              :permission-mode nil
                              :permission-rules nil
                              :permission-queue nil
                              :plan-queue nil))
                    (buffer (generate-new-buffer " *mev-chat-mode*"))
                    (mevedel-permission-mode 'default)
                    (refreshed 0))
               (unwind-protect
                   (cl-letf (((symbol-function 'mevedel-skills--refresh-view-input-prompt)
                              (lambda () (cl-incf refreshed))))
                     (with-current-buffer buffer
                       (setq-local mevedel--session session)
                       (setq-local mevedel-permission-mode nil)
                       (mevedel--implementation-permission-mode-apply 'trust-all)
                       (should (eq 'trust-all
                                   (mevedel-session-permission-mode session)))
                       (should (equal '(nil)
                                      mevedel--implementation-permission-mode-restore))
                       (mevedel--implementation-permission-mode-restore)
                       (should-not (mevedel-session-permission-mode session))
                       (should-not (local-variable-p 'mevedel-permission-mode
                                                     buffer))
                       (should (eq 'default mevedel-permission-mode))
                       (should-not mevedel--implementation-permission-mode-restore)
                       (should (= 2 refreshed))))
                 (when (buffer-live-p buffer) (kill-buffer buffer)))))


(provide 'test-mevedel-chat)
;;; test-mevedel-chat.el ends here
