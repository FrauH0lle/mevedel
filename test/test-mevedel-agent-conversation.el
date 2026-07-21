;;; test-mevedel-agent-conversation.el --- Agent conversation tests -*- lexical-binding: t -*-

;;; Commentary:

;; Focused tests for retained agent conversation state and request locals.

;;; Code:

(require 'gptel)
(require 'mevedel-agent-conversation)
(require 'mevedel-agents)
(require 'mevedel-permissions)
(require 'mevedel-pipeline)
(require 'mevedel-sandbox)
(require 'mevedel-session-persistence)
(require 'mevedel-skills-prompt)
(require 'mevedel-structs)
(require 'mevedel-tool-repair)
(require 'mevedel-view-agent)
(require 'mevedel-workspace)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))


(mevedel-deftest mevedel-agent-conversation-configure ()
		 ,test
		 (test)
		 :doc "restores frozen request locals in the retained buffer"
		 (let* ((buffer (generate-new-buffer " *agent-conversation-locals*"))
			(agent (mevedel-agent--create :name "default"))
			(invocation (mevedel-agent-invocation-create agent)))
		   (unwind-protect
		       (progn
			 (with-current-buffer buffer
			   (setq-local gptel-backend 'stale-backend)
			   (setq-local gptel-model 'stale-model)
			   (setq-local gptel-tools '(stale-tools)))
			 (setf (mevedel-agent-invocation-buffer invocation) buffer
			       (mevedel-agent-invocation-frozen-configuration invocation)
			       (mevedel-agent-configuration--create
				:agent agent
				:request-locals
				'((gptel-backend . fresh-backend)
				  (gptel-model . fresh-model)
				  (gptel-tools . (fresh-tools)))))
			 (mevedel-agent-conversation-configure invocation)
			 (should (eq 'fresh-backend
				     (buffer-local-value 'gptel-backend buffer)))
			 (should (eq 'fresh-model
				     (buffer-local-value 'gptel-model buffer)))
			 (should (equal '(fresh-tools)
					(buffer-local-value 'gptel-tools buffer))))
		     (kill-buffer buffer)))

		 :doc "accepts the provider's explicit buffer before it is retained"
		 (let* ((buffer (generate-new-buffer " *agent-conversation-dispatch*"))
			(agent (mevedel-agent--create :name "default"))
			(invocation (mevedel-agent-invocation-create agent)))
		   (unwind-protect
		       (progn
			 (setf (mevedel-agent-invocation-frozen-configuration invocation)
			       (mevedel-agent-configuration--create
				:agent agent
				:request-locals '((gptel-system-prompt . "agent system"))))
			 (mevedel-agent-conversation-configure invocation buffer)
			 (should (equal "agent system"
					(buffer-local-value 'gptel-system-prompt buffer))))
		     (kill-buffer buffer))))

(mevedel-deftest mevedel-agent-conversation--reject-terminal-tool-call ()
		 ,test
		 (test)
		 :doc "allows live turns and stops tool work after terminal settlement"
		 (let ((mevedel--agent-invocation
			(mevedel-agent-invocation--create)))
		   (should-not
		    (mevedel-agent-conversation--reject-terminal-tool-call))
		   (setf (mevedel-agent-invocation-runtime-settled-p
			  mevedel--agent-invocation)
			 t)
		   (should
		    (equal '(:stop t :stop-reason "Agent turn already settled")
			   (mevedel-agent-conversation--reject-terminal-tool-call)))))

(mevedel-deftest mevedel-agent-conversation-final-response ()
		 ,test
		 (test)
		 :doc "returns the latest non-empty assistant response"
		 (let* ((buffer (generate-new-buffer " *agent-conversation-response*"))
			(invocation (mevedel-agent-invocation--create :buffer buffer)))
		   (unwind-protect
		       (progn
			 (with-current-buffer buffer
			   (insert "User prompt\n")
			   (insert (propertize "First response\n" 'gptel 'response))
			   (insert "Follow-up\n")
			   (insert (propertize " Final response \n" 'gptel '(response))))
			 (should (equal "Final response"
					(mevedel-agent-conversation-final-response
					 invocation))))
		     (kill-buffer buffer))))

(mevedel-deftest mevedel-agent-conversation-final-activity ()
		 ,test
		 (test)
		 :doc "returns full non-status activity without sharing list structure"
		 (let ((invocation
			(mevedel-agent-invocation--create
			 :activity '((:type tool-start :summary "Read")
				     (:type status :status running)
				     (:type tool-finish :summary "Read done")))))
		   (let ((activity
			  (mevedel-agent-conversation-final-activity invocation)))
		     (should (equal '((:type tool-start :summary "Read")
				      (:type tool-finish :summary "Read done"))
				    activity))
		     (setf (plist-get (car activity) :summary) "changed")
		     (should (equal "Read"
				    (plist-get
				     (car (mevedel-agent-invocation-activity invocation))
				     :summary))))))


(mevedel-deftest mevedel-agent-conversation-insert-user-block ()
		 ,test
		 (test)

		 :doc "appended user-role injection clears accidental gptel properties"
		 (let* ((buf (generate-new-buffer " *mev-agent-inject-append*"))
			(agent (mevedel-agent--create :name "explorer"))
			(inv (mevedel-agent-invocation--create
			      :path "/root/test_agent"
			      :agent agent
			      :agent-id "explorer--inject"
			      :buffer buf))
			(block (propertize "<agent-message from=\"main\">\nhello\n</agent-message>"
					   'gptel 'response
					   'invisible t)))
		   (unwind-protect
		       (progn
			 (with-current-buffer buf
			   (insert (propertize "Assistant text\n" 'gptel 'response)))
			 (cl-letf (((symbol-function
				     'mevedel-agent-conversation-save)
				    (lambda (_invocation) t)))
			   (mevedel-agent-conversation-insert-user-block inv block))
			 (with-current-buffer buf
			   (goto-char (point-min))
			   (should (search-forward "<agent-message" nil t))
			   (should-not (get-text-property (match-beginning 0) 'gptel))
			   (should-not (get-text-property (match-beginning 0) 'invisible))))
		     (when (buffer-live-p buf) (kill-buffer buf))))

		 :doc "marker user-role injection preserves chronology and clears properties"
		 (let* ((root (file-name-as-directory
			       (make-temp-file "mevedel-agent-inject-marker-" t)))
			(relative "agents/marker.chat.org")
			(absolute (expand-file-name relative root))
			(workspace
			 (mevedel-workspace--create
			  :type 'project :id root :root root :name "conversation"))
			(session (mevedel-session-create "main" workspace))
			(buf (generate-new-buffer " *mev-agent-inject-marker*"))
			(agent (mevedel-agent--create :name "explorer"))
			(inv (mevedel-agent-invocation--create
			      :path "/root/test_agent"
			      :agent agent
			      :agent-id "explorer--inject"
			      :buffer buf
			      :parent-session session
			      :transcript-relative-path relative))
			(marker nil)
			(block (propertize "Reminder text" 'gptel '(tool . "call_1"))))
		   (unwind-protect
		       (progn
			 (make-directory (file-name-directory absolute) t)
			 (setf (mevedel-session-save-path session) root)
			 (with-current-buffer buf
			   (insert "* Agent Task: inspect\nbody\n")
			   (set-visited-file-name absolute t t)
			   (setq marker (copy-marker (point-max))))
			 (mevedel-agent-conversation-insert-user-block inv block marker)
			 (with-current-buffer buf
			   (goto-char (point-min))
			   (should (search-forward "* Agent Task:" nil t))
			   (should (search-forward "Reminder text" nil t))
			   (should-not (get-text-property (match-beginning 0) 'gptel))
			   (should (= (marker-position marker) (point-max))))
			 (with-temp-buffer
			   (insert-file-contents absolute)
			   (should (string-match-p "Reminder text" (buffer-string)))))
		     (when (buffer-live-p buf)
		       (with-current-buffer buf
			 (set-buffer-modified-p nil)
			 (setq-local kill-buffer-hook nil))
		       (kill-buffer buf))
		     (delete-directory root t))))


;;
;;; Callback contract


(mevedel-deftest mevedel-agent-conversation-open ()
		 ,test
		 (test)

		 :doc "forces agent data buffers to use linear gptel Org context"
		 (let* ((root (file-name-as-directory
			       (make-temp-file "mevedel-agent-parent-" t)))
			(workspace (mevedel-workspace--create
				    :type 'project
				    :id root
				    :root root
				    :name "agent"))
			(session (mevedel-session-create "main" workspace root))
			(parent-buf (generate-new-buffer " *mev-agent-parent*"))
			(agent (mevedel-agent--create :name "explorer"))
			(inv (mevedel-agent-invocation--create
			      :path "/root/test_agent"
			      :agent agent
			      :agent-id "explorer--linear"))
			agent-buf)
		   (unwind-protect
		       (progn
			 (with-current-buffer parent-buf
			   (setq-local mevedel--session session)
			   (setq-local mevedel--workspace workspace))
			 (cl-letf (((symbol-function 'gptel-mode) #'ignore))
			   (let ((gptel-org-branching-context t))
			     (setq agent-buf
				   (mevedel-agent-conversation-open
				    inv parent-buf))))
			 (with-current-buffer agent-buf
			   (should (derived-mode-p 'org-mode))
			   (should (eq mevedel--session session))
			   (should (eq mevedel--agent-invocation inv))
			   (should (equal "/root/test_agent"
				          (mevedel-agent-invocation-path inv)))
			   (should-not gptel-org-branching-context)))
		     (when (buffer-live-p agent-buf) (kill-buffer agent-buf))
		     (when (buffer-live-p parent-buf) (kill-buffer parent-buf))
		     (delete-directory root t)))

		 :doc "shares root policy by reference through nested agent buffers"
		 (let* ((root (file-name-as-directory
			       (make-temp-file "mevedel-agent-policy-" t)))
			(workspace (mevedel-workspace--create
			            :type 'project :id root :root root
			            :name "agent"))
			(session (mevedel-session-create "main" workspace root))
			(parent-buf
			 (generate-new-buffer " *mev-agent-policy-parent*"))
			(worker-inv
			 (mevedel-agent-invocation--create
			  :path "/root/worker"
			  :agent (mevedel-agent--create :name "worker")
			  :agent-id "worker--policy"))
			(nested-inv
			 (mevedel-agent-invocation--create
			  :path "/root/worker/verifier"
			  :agent (mevedel-agent--create :name "verifier")
			  :agent-id "verifier--policy"))
			worker-buf
			nested-buf)
		   (setf (mevedel-session-permission-mode session) 'full-auto)
		   (setf (mevedel-session-permission-rules session)
			 '(("Edit" :path "/tmp/protected/**" :action deny)))
		   (setf (mevedel-session-resource-grants session)
			 '((:path "/tmp/protected/read.txt" :access read)))
		   (unwind-protect
		       (let ((mevedel-protected-paths
			      '(("/tmp/protected/**" . inaccessible)))
			     (mevedel-sandbox-mode 'required))
			 (with-current-buffer parent-buf
			   (setq-local mevedel--session session)
			   (setq-local mevedel--workspace workspace))
			 (cl-letf (((symbol-function 'gptel-mode) #'ignore))
			   (setq worker-buf
			         (mevedel-agent-conversation-open
			          worker-inv parent-buf))
			   (setq nested-buf
			         (mevedel-agent-conversation-open
			          nested-inv worker-buf)))
			 (dolist (buffer (list worker-buf nested-buf))
			   (should
			    (eq session
			        (buffer-local-value 'mevedel--session buffer))))
			 (with-current-buffer nested-buf
			   (should (eq 'required mevedel-sandbox-mode))
			   (should
			    (eq 'deny
			        (mevedel-check-permission
			         "Edit"
			         :path "/tmp/protected/write.txt"
			         :session-rules
			         (mevedel-session-permission-rules mevedel--session)
			         :mode
			         (mevedel-session-permission-mode mevedel--session)
			         :resource-grants
			         (mevedel-session-resource-grants mevedel--session))))
			   (should
			    (eq 'allow
			        (mevedel-check-permission
			         "Read"
			         :path "/tmp/protected/read.txt"
			         :resource-access 'read
			         :session-rules
			         (mevedel-session-permission-rules mevedel--session)
			         :mode
			         (mevedel-session-permission-mode mevedel--session)
			         :resource-grants
			         (mevedel-session-resource-grants mevedel--session))))))
		     (when (buffer-live-p nested-buf) (kill-buffer nested-buf))
		     (when (buffer-live-p worker-buf) (kill-buffer worker-buf))
		     (when (buffer-live-p parent-buf) (kill-buffer parent-buf))
		     (delete-directory root t)))

                 :doc "installs path-scoped skill activation in agent buffers"
                 (let* ((root (file-name-as-directory
                               (make-temp-file "mevedel-agent-parent-" t)))
                        (workspace (mevedel-workspace--create
                                    :type 'project
                                    :id root
                                    :root root
                                    :name "agent"))
                        (session (mevedel-session-create
                                  "main" workspace root))
                        (parent-buf
                         (generate-new-buffer " *mev-agent-parent*"))
                        (agent (mevedel-agent--create
                                :name "explorer"))
                        (inv (mevedel-agent-invocation--create
                              :path "/root/test_agent"
                              :agent agent
                              :agent-id "explorer--skills"))
                        agent-buf)
                   (unwind-protect
                       (progn
                         (with-current-buffer parent-buf
                           (setq-local mevedel--session session)
                           (setq-local mevedel--workspace workspace))
                         (cl-letf (((symbol-function 'gptel-mode)
                                    #'ignore))
                           (setq agent-buf
                                 (mevedel-agent-conversation-open
                                  inv parent-buf)))
                         (with-current-buffer agent-buf
                           (should (memq
                                    #'mevedel-skills--post-tool-activate
                                    gptel-post-tool-call-functions))))
                     (when (buffer-live-p agent-buf)
                       (kill-buffer agent-buf))
                     (when (buffer-live-p parent-buf)
                       (kill-buffer parent-buf))
                     (delete-directory root t)))

                 :doc "installs independent repair hooks in retained agent buffers"
                 (let* ((root (file-name-as-directory
                               (make-temp-file "mevedel-agent-repair-" t)))
                        (workspace (mevedel-workspace--create
                                    :type 'project :id root :root root
                                    :name "agent"))
                        (session (mevedel-session-create
                                  "main" workspace root))
                        (parent-buf
                         (generate-new-buffer " *mev-agent-parent*"))
                        buffers)
                   (unwind-protect
                       (progn
                         (with-current-buffer parent-buf
                           (setq-local mevedel--session session)
                           (setq-local mevedel--workspace workspace))
                         (dotimes (index 2)
                           (let* ((agent
                                   (mevedel-agent--create
                                    :name "explorer"))
                                  (inv
                                   (mevedel-agent-invocation--create
                                    :path "/root/test_agent"
                                    :agent agent
                                    :agent-id
                                    (format "explorer--%d" index)))
                                  buffer)
                             (cl-letf (((symbol-function 'gptel-mode)
                                        #'ignore))
                               (setq buffer
                                     (mevedel-agent-conversation-open
                                      inv parent-buf)))
                             (push buffer buffers)
                             (with-current-buffer buffer
                               (should
                                (memq #'mevedel-tool-repair-pre-tool-call
                                      gptel-pre-tool-call-functions))
                               (should
                                (memq #'mevedel-tool-repair-post-tool-call
                                      gptel-post-tool-call-functions))
                               (should
                                (memq #'mevedel-tool-repair-clear-ledger
                                      gptel-post-response-functions))
                               (should
                                (memq #'mevedel-view-agent-live-transcript-stream
                                      gptel-post-stream-hook))
                               (should
                                (memq #'mevedel-view-agent-live-transcript-pre-tool
                                      gptel-pre-tool-call-functions))
                               (should
                                (memq #'mevedel-view-agent-live-transcript-post-tool
                                      gptel-post-tool-call-functions))
                               (should
                                (memq #'mevedel-tool-repair-clear-ledger
                                      kill-buffer-hook)))))
                         (with-current-buffer (car buffers)
                           (setq-local mevedel-tool-repair--ledger
                                       '((:tool "first"
                                                :telemetry-recorded t))))
                         (with-current-buffer (cadr buffers)
                           (setq-local mevedel-tool-repair--ledger
                                       '((:tool "second"
                                                :telemetry-recorded t))))
                         (kill-buffer (car buffers))
                         (should
                          (equal
                           '((:tool "second" :telemetry-recorded t))
                           (buffer-local-value
                            'mevedel-tool-repair--ledger
                            (cadr buffers)))))
                     (dolist (buffer buffers)
                       (when (buffer-live-p buffer)
                         (kill-buffer buffer)))
                     (when (buffer-live-p parent-buf)
                       (kill-buffer parent-buf))
                     (delete-directory root t))))


(mevedel-deftest mevedel-agent-conversation-hydrate ()
		 ,test
		 (test)
		 :doc "drops redundant saved system prompts and restores the frozen prompt"
		 (let* ((root (file-name-as-directory
			       (make-temp-file "mevedel-agent-hydrate-" t)))
			(workspace (mevedel-workspace--create
				    :type 'project :id root :root root :name "agent"))
			(session (mevedel-session-create "main" workspace root))
			(parent-buffer (generate-new-buffer " *mev-agent-parent*"))
			(transcript (expand-file-name "agent.chat.org" root))
			(agent (mevedel-agent--create :name "worker"))
			(invocation
			 (mevedel-agent-invocation--create
			  :path "/root/worker"
			  :agent agent
			  :agent-id "worker--hydrate"
			  :parent-session session
			  :frozen-configuration
			  (mevedel-agent-configuration--create
			   :agent agent
			   :request-locals
			   '((gptel-system-prompt . "Frozen worker prompt")))))
			buffer)
		   (unwind-protect
		       (progn
			 (write-region
			  ":PROPERTIES:\n:GPTEL_SYSTEM: Expanded stale prompt\n:END:\nConversation\n"
			  nil transcript nil 'silent)
			 (with-current-buffer parent-buffer
			   (setq-local mevedel--session session)
			   (setq-local mevedel--workspace workspace))
			 (mevedel-session-persistence--install-gptel-save-state-advice)
			 (setq buffer
			       (mevedel-agent-conversation-hydrate
				invocation parent-buffer transcript))
			 (with-current-buffer buffer
			   (should (equal "Frozen worker prompt" gptel-system-prompt))
			   (should-not (org-entry-get (point-min) "GPTEL_SYSTEM"))
			   (should-not (buffer-modified-p))))
		     (when (buffer-live-p buffer)
		       (with-current-buffer buffer
			 (set-buffer-modified-p nil)
			 (setq-local kill-buffer-hook nil))
		       (kill-buffer buffer))
		     (when (buffer-live-p parent-buffer)
		       (kill-buffer parent-buffer))
		     (delete-directory root t))))


(mevedel-deftest mevedel-agent-conversation--render-data-bounds ()
		 ,test
		 (test)

		 :doc "uses cached render-data markers without rescanning parent buffer"
		 (let ((buf (generate-new-buffer " *mev-agent-render-data-cache*")))
		   (unwind-protect
		       (with-current-buffer buf
			 (let* ((agent (mevedel-agent--create :name "explorer"))
				(inv (mevedel-agent-invocation--create :agent agent))
				(scans 0))
			   (insert (mevedel-pipeline--format-render-data-block
				    '(:agent-id "explorer--1" :status running)))
			   (mevedel-agent-conversation--cache-render-data-bounds
			    inv (point-min) (point-max))
			   (cl-letf (((symbol-function
				       'mevedel-pipeline--find-render-data-block-by-agent-id)
				      (lambda (_agent-id)
					(cl-incf scans)
					nil)))
			     (let ((bounds
				    (mevedel-agent-conversation--render-data-bounds
				     inv "explorer--1")))
			       (should (consp bounds))
			       (should (markerp (car bounds)))
			       (should (markerp (cdr bounds)))
			       (should (= 0 scans))))))
		     (when (buffer-live-p buf) (kill-buffer buf))))

		 :doc "rejects stale cached markers before falling back to scan"
		 (let ((buf (generate-new-buffer " *mev-agent-render-data-stale*")))
		   (unwind-protect
		       (with-current-buffer buf
			 (let* ((agent (mevedel-agent--create :name "explorer"))
				(inv (mevedel-agent-invocation--create :agent agent))
				(scans 0)
				(fallback-bounds nil)
				(block-end nil))
			   (insert (mevedel-pipeline--format-render-data-block
				    '(:agent-id "other--1" :status running)))
			   (setq block-end (point))
			   (insert "tail\n")
			   (mevedel-agent-conversation--cache-render-data-bounds
			    inv (point-min) block-end)
			   (goto-char (point-max))
			   (setq fallback-bounds (cons (point) nil))
			   (insert (mevedel-pipeline--format-render-data-block
				    '(:agent-id "explorer--1" :status running)))
			   (setcdr fallback-bounds (point))
			   (cl-letf (((symbol-function
				       'mevedel-pipeline--find-render-data-block-by-agent-id)
				      (lambda (agent-id)
					(cl-incf scans)
					(and (equal agent-id "explorer--1")
					     fallback-bounds))))
			     (let ((bounds
				    (mevedel-agent-conversation--render-data-bounds
				     inv "explorer--1")))
			       (should (markerp (car bounds)))
			       (should (markerp (cdr bounds)))
			       (should (= (car fallback-bounds)
					  (marker-position (car bounds))))
			       (should (= (cdr fallback-bounds)
					  (marker-position (cdr bounds))))
			       (should (= 1 scans))))))
		     (when (buffer-live-p buf) (kill-buffer buf)))))









(mevedel-deftest mevedel-agent-conversation-record-activity ()
		 ,test
		 (test)

		 :doc "keeps all items and calls targeted agent refresh"
		 (let* ((agent (mevedel-agent--create :name "explorer"
						      :description "Explore"))
			(inv (mevedel-agent-invocation--create
			      :path "/root/test_agent"
			      :agent agent
			      :agent-id "explorer--activity"))
			(parent-buf (generate-new-buffer " *mev-agent-activity-parent*"))
			(view-buf (generate-new-buffer " *mev-agent-activity-view*"))
			(renders 0))
		   (unwind-protect
		       (progn
			 (setf (mevedel-agent-invocation-parent-data-buffer inv) parent-buf)
			 (with-current-buffer parent-buf
			   (setq-local mevedel--view-buffer view-buf))
			 (cl-letf (((symbol-function 'mevedel-view-refresh-agent-rendering)
				    (lambda (_buffer _agent-id)
				      (cl-incf renders))))
			   (mevedel-agent-conversation-record-activity
			    inv '(:type tool-start :summary "one"))
			   (mevedel-agent-conversation-record-activity
			    inv '(:type tool-finish :summary "two"))
			   (mevedel-agent-conversation-record-activity
			    inv '(:type waiting :summary "three")))
			 (let ((items (mevedel-agent-invocation-activity inv)))
			   (should (= 3 (length items)))
			   (should (equal "one" (plist-get (car items) :summary)))
			   (should (equal "two" (plist-get (cadr items) :summary)))
			   (should (equal "three" (plist-get (caddr items) :summary)))
			   (should (numberp (plist-get (car items) :time))))
			 (should (= 3 renders)))
		     (when (buffer-live-p view-buf) (kill-buffer view-buf))
		     (when (buffer-live-p parent-buf) (kill-buffer parent-buf))))

		 :doc "records allowed activity item types without rewriting their type"
		 (let* ((agent (mevedel-agent--create :name "explorer"
						      :description "Explore"))
			(inv (mevedel-agent-invocation--create
			      :path "/root/test_agent"
			      :agent agent
			      :agent-id "explorer--activity-types"))
			(parent-buf (generate-new-buffer " *mev-agent-activity-parent*"))
			(view-buf (generate-new-buffer " *mev-agent-activity-view*"))
			(types '(tool-start tool-finish tool-error waiting message status)))
		   (unwind-protect
		       (progn
			 (setf (mevedel-agent-invocation-parent-data-buffer inv) parent-buf)
			 (with-current-buffer parent-buf
			   (setq-local mevedel--view-buffer view-buf))
			 (cl-letf (((symbol-function 'mevedel-view-refresh-agent-rendering)
				    (lambda (_buffer _agent-id) nil)))
			   (dolist (type types)
			     (mevedel-agent-conversation-record-activity
			      inv (list :type type :summary (symbol-name type)))))
			 (should (equal types
					(mapcar (lambda (item) (plist-get item :type))
						(mevedel-agent-invocation-activity inv))))
			 (should (cl-every
				  (lambda (item) (numberp (plist-get item :time)))
				  (mevedel-agent-invocation-activity inv))))
		     (when (buffer-live-p view-buf) (kill-buffer view-buf))
		     (when (buffer-live-p parent-buf) (kill-buffer parent-buf))))

		 :doc "syncs running activity into transcript metadata"
		 (let* ((workspace
			 (mevedel-workspace--create
			  :type 'project
			  :id "agent-conversation"
			  :root temporary-file-directory
			  :name "agent-conversation"))
			(session (mevedel-session-create "main" workspace))
			(parent (generate-new-buffer " *agent-conversation-activity*"))
			(agent-id "explorer--activity-sync")
			(invocation
			 (mevedel-agent-invocation--create
			  :agent-id agent-id
			  :parent-session session
			  :parent-data-buffer parent
			  :transcript-status 'running
			  :call-count 20
			  :activity '((:type waiting :summary "waiting")))))
		   (unwind-protect
		       (progn
			 (setf (mevedel-session-agent-transcripts session)
			       (list (cons agent-id
					   '(:status running
						     :agent-type "explorer"
						     :description "validate"))))
			 (mevedel-agent-conversation-record-activity
			  invocation
			  '(:type tool-finish
				  :tool-name "Read"
				  :summary "Read done"))
			 (let ((entry
				(cdr (assoc agent-id
					    (mevedel-session-agent-transcripts session)))))
			   (should (= 20 (plist-get entry :calls)))
			   (should (= 2 (length (plist-get entry :activity))))
			   (should (equal "Read"
					  (plist-get (cadr (plist-get entry :activity))
						     :tool-name)))))
		     (kill-buffer parent))))

(mevedel-deftest mevedel-agent-conversation-save ()
		 ,test
		 (test)

		 :doc "ignores conversations without a live visited file"
		 (let* ((buffer (generate-new-buffer " *agent-conversation-unsaved*"))
			(invocation
			 (mevedel-agent-invocation-create
			  (mevedel-agent--create :name "default"))))
		   (setf (mevedel-agent-invocation-buffer invocation) buffer)
		   (unwind-protect
		       (should-not (mevedel-agent-conversation-save invocation))
		     (kill-buffer buffer))
		   (should-not (mevedel-agent-conversation-save invocation)))

		 :doc "writes a modified retained conversation"
		 (let* ((root (file-name-as-directory
			       (make-temp-file "mevedel-agent-conversation-save-" t)))
			(workspace
			 (mevedel-workspace--create
			  :type 'project :id root :root root :name "conversation"))
			(session (mevedel-session-create "main" workspace))
			(relative "agents/test.chat.org")
			(absolute (expand-file-name relative root))
			(buffer (generate-new-buffer " *agent-conversation-save*"))
			(invocation
			 (mevedel-agent-invocation--create
			  :agent-id "default--save"
			  :buffer buffer
			  :parent-session session
			  :transcript-relative-path relative)))
		   (unwind-protect
		       (progn
			 (make-directory (file-name-directory absolute) t)
			 (setf (mevedel-session-save-path session) root)
			 (with-current-buffer buffer
			   (org-mode)
			   (gptel-mode +1)
			   (setq-local mevedel--session session)
			   (setq-local mevedel--agent-invocation invocation)
			   (setq-local gptel-system-prompt
				       "A deliberately expanded retained-agent prompt")
			   (setq-local gptel-backend
				       (let ((gptel--known-backends nil))
					 (gptel-make-openai
					  "Agent Save" :key "test"
					  :models '(agent-save-model))))
			   (setq-local gptel-model 'agent-save-model)
			   (setq-local gptel-temperature nil)
			   (insert "* Agent Task: save\n\n")
			   (let ((start (point)))
			     (insert "durable conversation\n")
			     (add-text-properties start (point) '(gptel response)))
			   (set-visited-file-name absolute t t)
			   (set-buffer-modified-p t))
			 (mevedel-session-persistence--install-gptel-save-state-advice)
			 (should (mevedel-agent-conversation-save invocation))
			 (with-temp-buffer
			   (insert-file-contents absolute)
			   (org-mode)
			   (should (org-entry-get (point-min) "GPTEL_BOUNDS"))
			   (should-not (org-entry-get (point-min) "GPTEL_SYSTEM"))
			   (should (string-match-p "durable conversation"
					   (buffer-string)))))
		     (when (buffer-live-p buffer)
		       (with-current-buffer buffer
			 (set-buffer-modified-p nil)
			 (setq-local kill-buffer-hook nil))
		       (kill-buffer buffer))
		     (delete-directory root t)))

		 :doc "rejects a write when gptel state serialization fails"
		 (let* ((root (file-name-as-directory
			       (make-temp-file "mevedel-agent-conversation-save-" t)))
			(workspace
			 (mevedel-workspace--create
			  :type 'project :id root :root root :name "conversation"))
			(session (mevedel-session-create "main" workspace))
			(relative "agents/test.chat.org")
			(absolute (expand-file-name relative root))
			(buffer (generate-new-buffer " *agent-conversation-save*"))
			(invocation
			 (mevedel-agent-invocation--create
			  :agent-id "default--save"
			  :buffer buffer
			  :parent-session session
			  :transcript-relative-path relative)))
		   (unwind-protect
		       (progn
			 (make-directory (file-name-directory absolute) t)
			 (write-region "previous transcript\n" nil absolute nil 'silent)
			 (setf (mevedel-session-save-path session) root)
			 (with-current-buffer buffer
			   (org-mode)
			   (gptel-mode +1)
			   (insert "* Agent Task: save\n\nnew transcript\n")
			   (set-visited-file-name absolute t t)
			   (set-buffer-modified-p t))
			 (cl-letf (((symbol-function 'gptel--save-state)
				    (lambda () (error "Injected serialization failure"))))
			   (should-not
			    (mevedel-agent-conversation-save invocation)))
			 (should
			  (equal "previous transcript\n"
				 (with-temp-buffer
				   (insert-file-contents absolute)
				   (buffer-string)))))
		     (when (buffer-live-p buffer)
		       (with-current-buffer buffer
			 (set-buffer-modified-p nil)
			 (setq-local kill-buffer-hook nil))
		       (kill-buffer buffer))
		     (delete-directory root t)))

		 :doc "defers saves through one coalescing idle timer"
		 (let* ((invocation
			 (mevedel-agent-invocation-create
			  (mevedel-agent--create :name "default")))
			(mevedel-agent-conversation-save-debounce 10)
			(writes 0))
		   (unwind-protect
		       (cl-letf (((symbol-function 'mevedel-agent-conversation--write)
				  (lambda (_invocation) (cl-incf writes))))
			 (mevedel-agent-conversation-save invocation t)
			 (should (= 0 writes))
			 (should
			  (timerp
			   (mevedel-agent-invocation-transcript-save-timer invocation))))
		     (mevedel-agent-conversation--cancel-save invocation)))

		 :doc "zero debounce saves immediately"
		 (let* ((invocation
			 (mevedel-agent-invocation-create
			  (mevedel-agent--create :name "default")))
			(mevedel-agent-conversation-save-debounce 0)
			saved)
		   (cl-letf (((symbol-function 'mevedel-agent-conversation--write)
			      (lambda (argument) (setq saved argument))))
		     (mevedel-agent-conversation-save invocation t))
		   (should (eq invocation saved))
		   (should-not
		    (mevedel-agent-invocation-transcript-save-timer invocation))))



(provide 'test-mevedel-agent-conversation)

;;; test-mevedel-agent-conversation.el ends here
