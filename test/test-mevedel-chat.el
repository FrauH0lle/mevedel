;;; test-mevedel-chat.el --- Tests for chat buffer management -*- lexical-binding: t -*-

;;; Commentary:

;; Focused coverage for chat-buffer lifecycle and directive request commands.

;;; Code:

(require 'mevedel-chat)
(require 'mevedel-agent-control)
(require 'mevedel)
(require 'mevedel-permission-queue)
(require 'mevedel-goal)
(require 'mevedel-prompt-submission)
(require 'mevedel-view-zone)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name
               load-file-name
               byte-compile-current-file))
          "helpers"))

;; `gptel'
(defvar gptel--known-presets)
(defvar gptel-backend)
(defvar gptel-model)
(defvar gptel-reasoning-effort)

;; `gptel-org'
(defvar gptel-org-branching-context)
(defvar gptel-org-ignore-elements)

;; `mevedel-view'
(declare-function mevedel-view--render-incremental "mevedel-view"
                  (data-buffer))
(defvar mevedel-view--data-turn-start)
(defvar mevedel-view--in-flight-turn-start)
(defvar mevedel-view--input-marker)

;; `org'
(defvar org-mode-hook)

;; `org-agenda'
(defvar org-agenda-file-menu-enabled)

;; `org-element'
(defvar org-element-cache-persistent)
(defvar org-element-use-cache)

;; `org-indent'
(declare-function org-indent-mode "org-indent" (&optional arg))
(defvar org-indent-mode)

(defvar mevedel-chat-test--hook-events nil)

(defun mevedel-chat-test--record-hook (event)
  "Record lifecycle hook EVENT for chat cases."
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
			     ((symbol-function 'mevedel-view-stream-uninstall)
			      #'ignore))
		     (mevedel-uninstall))
		   (should called))

		 :doc "force-tears down executions"
		 (let ((gptel--known-tools gptel--known-tools)
		       (gptel--known-presets gptel--known-presets)
		       (gptel-prompt-transform-functions gptel-prompt-transform-functions)
		       torn-down)
		   (cl-letf (((symbol-function 'mevedel-execution-teardown-all)
			      (lambda () (setq torn-down t)))
			     ((symbol-function 'mevedel-skills-uninstall-hot-reload) #'ignore)
			     ((symbol-function 'mevedel-skills-uninstall-slash-commands) #'ignore)
			     ((symbol-function 'mevedel-pipeline-uninstall-tool-result-scrubber) #'ignore)
			     ((symbol-function 'mevedel-view-stream-uninstall) #'ignore))
		     (mevedel-uninstall))
		   (should torn-down)))


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
						(error "Menu setup should not run")))
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

(mevedel-deftest mevedel--chat-buffer-init-common
		 (:doc "notifies about pending plugin hook consent during setup")
		 (let* ((root (file-name-as-directory
			       (make-temp-file "mevedel-chat-init-" t)))
			(workspace (mevedel-workspace--create
				    :type 'project
				    :id root
				    :root root
				    :name "init"))
			(session (mevedel-session-create "main" workspace root))
			notified-workspace
			validated-workspace)
		   (unwind-protect
		       (with-temp-buffer
			 (setq-local mevedel--session session)
			 (require 'mevedel-session-persistence)
			 (require 'mevedel-view)
			   (cl-letf (((symbol-function
				     'mevedel-reminders-install-defaults)
				    #'ignore)
				   ((symbol-function
				     'mevedel-preset--build-handlers)
				    #'identity)
				   ((symbol-function
				     'mevedel-session-persistence--install-gptel-save-state-advice)
				    #'ignore)
				   ((symbol-function 'mevedel-skills-install)
				    #'ignore)
				   ((symbol-function
				     'mevedel-skills-install-reminder)
				    #'ignore)
				   ((symbol-function
				     'mevedel-skills-install-activation-hook)
				    #'ignore)
				   ((symbol-function 'mevedel-view--ensure)
				    #'ignore)
				   ((symbol-function
				     'mevedel-permission-validate-persistent-stores)
				    (lambda (workspace)
				      (setq validated-workspace workspace)))
				   ((symbol-function 'mevedel--run-session-start-hooks)
				    #'ignore)
				   ((symbol-function
				     'mevedel-plugins-notify-pending-consent)
				    (lambda (workspace)
				      (setq notified-workspace workspace))))
			   (mevedel--chat-buffer-init-common
			    (current-buffer) workspace "resume"))
			 (should (= 1 (length
			               (mevedel-session-pending-reminders session))))
			 (should (string-match-p
			          "reconcile current state"
			          (car (mevedel-session-pending-reminders session))))
			 (should (eq notified-workspace workspace))
			 (should (eq validated-workspace workspace))
			 (should (memq #'mevedel-tool-repair-pre-tool-call
				       gptel-pre-tool-call-functions))
			 (should (memq #'mevedel-tool-repair-post-tool-call
				       gptel-post-tool-call-functions))
			 (should (memq #'mevedel-tool-repair-clear-ledger
				       gptel-post-response-functions))
			 (should (memq #'mevedel-view-stream-render-response
				       gptel-post-response-functions))
			 (should (memq #'mevedel-view-stream-spinner-hook
				       gptel-pre-tool-call-functions))
			 (should (memq #'mevedel-view-stream-pre-tool
				       gptel-pre-tool-call-functions))
			 (should (memq #'mevedel-view-stream-post-tool
				       gptel-post-tool-call-functions))
			 (should (memq #'mevedel-view-stream-schedule
				       gptel-post-stream-hook))
			 (should (memq #'mevedel-tool-repair-clear-ledger
				       kill-buffer-hook)))
		     (delete-directory root t))))

(mevedel-deftest mevedel-session-lifecycle-hooks
		 (:doc "runs normal and declarative session lifecycle hooks")
		 (let* ((root (file-name-as-directory
			       (make-temp-file "mevedel-chat-hooks-" t)))
			(user-dir (file-name-as-directory
				   (make-temp-file "mevedel-chat-hooks-user-" t)))
			(workspace (mevedel-workspace--create
				    :type 'project
				    :id root
				    :root root
				    :name "hooks"))
			(session (mevedel-session-create "main" workspace root))
			(mevedel-user-dir user-dir)
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
			 (mevedel--run-session-start-hooks "startup")
			 (mevedel--run-session-end-hooks)
			 (should (equal (nreverse normal-events) '(start end)))
			 (should
			  (equal
			   (mapcar (lambda (event)
				     (plist-get event :hook-event-name))
				   (nreverse mevedel-chat-test--hook-events))
			   '(SessionStart SessionEnd)))
			 (should
			  (equal (mevedel-session-hook-context-pending session)
				 '((:event "SessionStart"
				    :body "startup context")))))
		     (delete-directory root t)
		     (delete-directory user-dir t))))

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
			(called nil)
                        payload)
		   (unwind-protect
		       (with-temp-buffer
			 (setq-local mevedel--session session)
			 (setq-local mevedel--workspace workspace)
			 (cl-letf (((symbol-function 'mevedel-hooks-run-event)
				    (lambda (_event event-payload callback &rest _)
                                      (setq payload event-payload)
				      (run-at-time
				       0.01 nil
				       (lambda ()
					 (setq called t)
					 (funcall callback
						  '(:additional-context
						    ("async startup"))))))))
			   (mevedel--run-session-start-hooks "compact"))
			 (should called)
			 (should (equal "compact" (plist-get payload :source)))
			 (should-not mevedel--session-start-hooks-pending)
			 (should
			  (equal (mevedel-session-hook-context-pending session)
				 '((:event "SessionStart"
				    :body "async startup")))))
		     (delete-directory root t))))


;;
;;; Local user turns

(mevedel-deftest mevedel--insert-local-user-turn
		 (:doc "persists distinct view text without exposing it to the model")
		 (let ((data-buffer (generate-new-buffer " *mevedel-local-turn-data*"))
		       (view-buffer (generate-new-buffer " *mevedel-local-turn-view*"))
		       (displayed nil)
		       (kind nil)
		       (sent nil)
		       (original-view-fn (and (fboundp 'mevedel-view--begin-external-turn)
					      (symbol-function 'mevedel-view--begin-external-turn))))
		   (unwind-protect
		       (progn
			 (fset 'mevedel-view--begin-external-turn
			       (lambda (display-text _marker &optional turn-kind
                                                     _hook-context _no-spinner)
				 (setq displayed display-text
				       kind turn-kind)))
			 (cl-letf (((symbol-function 'gptel-send)
				    (lambda (&rest _) (setq sent t))))
			   (with-current-buffer data-buffer
			     (org-mode)
			     (setq-local gptel-response-separator "\n\n")
			     (setq-local gptel-prompt-prefix-alist
					 '((org-mode . "* User\n")))
			     (setq-local mevedel--view-buffer view-buffer)
			     (let ((marker (mevedel--insert-local-user-turn
					    "Setup context" "Show setup" 'worktree)))
			       (should (markerp marker))
			       (should (string-match-p
					"Setup context"
					(buffer-substring-no-properties
					 (point-min) (point-max))))
			       (goto-char (point-min))
			       (search-forward "<!-- mevedel-render-data -->")
			       (should (eq 'ignore
					   (get-text-property (match-beginning 0) 'gptel)))
			       (should
				(equal
				 '(:kind user-display :text "Show setup")
				 (cdr (mevedel-pipeline-extract-render-data
				       (buffer-substring-no-properties
					(point-min) (point-max)))))))))
			 (should (equal "Show setup" displayed))
			 (should (eq 'worktree kind))
			 (should-not sent))
		     (if original-view-fn
			 (fset 'mevedel-view--begin-external-turn original-view-fn)
		       (fmakunbound 'mevedel-view--begin-external-turn))
		     (when (buffer-live-p data-buffer)
		       (kill-buffer data-buffer))
		     (when (buffer-live-p view-buffer)
		       (kill-buffer view-buffer)))))

(mevedel-deftest mevedel--gptel-send-request ()
  ,test
  (test)
  :doc "returns the FSM for a standard transformed streaming request"
  (with-temp-buffer
    (setq-local gptel-prompt-transform-functions '(transform)
                gptel-stream t)
    (let (model-input request-args)
      (cl-letf (((symbol-function 'gptel-request)
                 (lambda (&optional _prompt &rest args)
                   (should (local-variable-p 'mevedel--pending-model-input))
                   (setq model-input mevedel--pending-model-input
                         request-args args)
                   (setq-local mevedel--pending-model-input nil)
                   (plist-get args :fsm))))
        (let ((fsm (mevedel--gptel-send-request "derived prompt")))
          (should fsm)
          (should (eq fsm (plist-get request-args :fsm)))
          (should (equal "derived prompt" model-input))
          (should (equal '(transform) (plist-get request-args :transforms)))
          (should (plist-get request-args :stream)))
        (should-not mevedel--pending-model-input)))))

(mevedel-deftest mevedel--submit-generated-turn ()
  ,test
  (test)
  :doc "returns its FSM while persisting hook context in the transcript"
  (with-temp-buffer
    (org-mode)
    (setq-local gptel-response-separator "\n\n"
                gptel-prompt-prefix-alist '((org-mode . "* User\n"))
                gptel-prompt-transform-functions '(transform)
                gptel-stream t)
    (let ((fsm (gptel-make-fsm))
          model-input
          request-args)
      (cl-letf (((symbol-function 'gptel-request)
                 (lambda (&optional _prompt &rest args)
                   (setq model-input mevedel--pending-model-input
                         request-args args)
                   fsm)))
        (should
         (eq fsm
             (mevedel--submit-generated-turn
              "Planning prompt" "Goal"
              (mevedel-prompt-submission-create
               :context "<hook-context>ctx</hook-context>"
               :state 'committed)))))
      (should (string-match-p "Planning prompt" (buffer-string)))
      (should (string-match-p "hook-context" (buffer-string)))
      (should (string-match-p "hook-context" model-input))
      (should (equal '(transform) (plist-get request-args :transforms)))))

  :doc "stores the generated prompt while sending prepared skill context"
  (with-temp-buffer
    (org-mode)
    (setq-local gptel-response-separator "\n\n"
                gptel-prompt-prefix-alist '((org-mode . "* User\n")))
    (let (model-input)
      (cl-letf (((symbol-function 'gptel-request)
                 (lambda (&rest _)
                   (setq model-input mevedel--pending-model-input))))
        (mevedel--submit-generated-turn
         "hook input" "Implement accepted plan"
         (mevedel-prompt-submission-create :input "hook input")
         '(:transcript-input "Use $alpha"
           :model-input "Use [skill:alpha -- attached]\n\nALPHA BODY"
           :render-data "<!-- mevedel-render-data -->")))
      (should (string-match-p "Use \\$alpha" (buffer-string)))
      (should (string-match-p "mevedel-render-data" (buffer-string)))
      (should (string-match-p "ALPHA BODY" model-input))
      (should (string-match-p "mevedel-render-data" model-input))))

  :doc "commits inserted context before a synchronous request failure"
  (with-temp-buffer
    (org-mode)
    (let* ((session (mevedel-session--create :name "goal-context"))
           (context-entries '((:event SessionStart :body "goal context")))
           (submission
            (mevedel-prompt-submission-create
             :context "<hook-context>goal context</hook-context>"
             :session session
             :context-entries context-entries)))
      (setq-local mevedel--session session
                  gptel-response-separator "\n\n"
                  gptel-prompt-prefix-alist '((org-mode . "* User\n")))
      (setf (mevedel-session-hook-context-pending session) context-entries)
      (cl-letf (((symbol-function 'gptel-request)
                 (lambda (&rest _) (error "Request startup failed"))))
        (should-error
         (mevedel--submit-generated-turn
          "Planning prompt" "Goal" submission)))
      (should-not (mevedel-session-hook-context-pending session))
      (should (string-match-p "goal context" (buffer-string))))))

(mevedel-deftest mevedel--implement-plan ()
  ,test
  (test)
  :doc "submits the accepted current-context prompt with compact display text"
  (with-temp-buffer
    (org-mode)
    (let ((submission
           (mevedel-prompt-submission-create
            :input "Accepted plan artifact: /accepted.md\n\n# Current plan"
            :state 'committed))
          sent-prompt sent-display sent-submission)
      (cl-letf (((symbol-function
                  'mevedel--implementation-permission-mode-apply)
                 #'ignore)
                ((symbol-function 'mevedel--submit-generated-turn)
                 (lambda (prompt display accepted &optional _prepared)
                   (setq sent-prompt prompt
                         sent-display display
                         sent-submission accepted))))
        (mevedel--implement-plan
         (list :permission-mode 'edits
               :display-text "Implement accepted plan as Goal"
               :prompt-submission submission)))
      (should (equal (mevedel-prompt-submission-input submission)
                     sent-prompt))
      (should (equal "Implement accepted plan as Goal" sent-display))
      (should (eq submission sent-submission))))

  :doc "stages prepared skill request context for the implementation turn"
  (with-temp-buffer
    (org-mode)
    (let* ((submission
            (mevedel-prompt-submission-create :input "hook input"))
           (prepared
            '(:model-input "expanded" :transcript-input "authored"
              :request-context (:invoked-skills (alpha))))
           seen)
      (cl-letf (((symbol-function
                  'mevedel--implementation-permission-mode-apply)
                 #'ignore)
                ((symbol-function 'mevedel--submit-generated-turn)
                 (lambda (&rest _)
                   (setq seen mevedel-skills--pending-request-context))))
        (mevedel--implement-plan
         (list :permission-mode 'edits
               :prompt-submission submission
               :prepared-outcome prepared)))
      (should (equal '(:invoked-skills (alpha)) seen)))))


;;
;;; Working directory sessions

(mevedel-deftest mevedel--display-chat-buffer
  ()
  ,test
  (test)
  :doc "keeps an existing session's selected preset"
  (let ((buf (generate-new-buffer " *mevedel-existing-preset*"))
        applied)
    (unwind-protect
        (progn
          (with-current-buffer buf
            (setq-local mevedel--session
                        (mevedel-session--create
                         :name "test" :preset-name 'selected)))
          (cl-letf (((symbol-function 'mevedel-preset-apply)
                     (lambda (&rest _) (setq applied t)))
                    ((symbol-function 'display-buffer) #'ignore))
            (mevedel--display-chat-buffer buf))
          (should-not applied))
      (kill-buffer buf)))
  :doc "applies the default to a fresh session"
  (let ((buf (generate-new-buffer " *mevedel-fresh-preset*"))
        applied)
    (unwind-protect
        (progn
          (with-current-buffer buf
            (setq-local mevedel--session
                        (mevedel-session--create :name "test")))
          (cl-letf (((symbol-function 'mevedel-preset-apply)
                     (lambda (name &optional _)
                       (setq applied name)))
                    ((symbol-function 'display-buffer) #'ignore))
            (mevedel--display-chat-buffer buf))
          (should (eq applied
                      (alist-get mevedel-default-chat-preset
                                 mevedel-action-preset-alist))))
      (kill-buffer buf))))

(mevedel-deftest mevedel-session-working-directory
		 (:before-each (mevedel-workspace-clear-registry)
			       :vars* ((root-dir (file-name-as-directory
						  (make-temp-file "mevedel-chat-cwd-" t)))
				       (module-dir (file-name-concat root-dir "packages" "api"))
				       (chat-buffer nil))
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

(mevedel-deftest mevedel--request-changes-prompt ()
  ,test
  (test)
  :doc "uses fresh context and only the immediately preceding attempt"
  (let* ((older
          (mevedel-directive-attempt--create
           :directive-request "Original" :request "Older exact request"
           :result "Older answer" :outcome 'success :patch "older patch"
           :capture 'complete :captured-at "2026-07-01T10:00:00+0200"))
         (latest
         (mevedel-directive-attempt--create
           :directive-request "Current request" :request "Latest exact request"
           :result "Latest answer" :outcome 'success :patch "latest patch"
           :plan "Latest accepted plan"
           :capture 'incomplete :captured-at "2026-07-02T11:30:00+0200"))
         (directive
          (mevedel-directive--create
           :id "directive" :request "Current request"
           :anchor '(:state attached) :state 'implemented
           :attempts (list older latest)))
         (prompt
          (mevedel--request-changes-prompt
           "Current request\n\nFresh reference" directive
           "> change one\nchange two")))
    (dolist (text '("Current repository state is authoritative"
                    "Fresh reference" "> change one\nchange two"
                    "Latest answer" "latest patch"
                    "2026-07-02T11:30:00+0200" "INCOMPLETE"
                    "historical" "Latest accepted plan"))
      (should (string-search text prompt)))
    (dolist (text '("Older exact request" "Older answer" "older patch"
                    "Latest exact request"))
      (should-not (string-search text prompt))))

  :doc "requires feedback unless unconsumed subdirectives provide changes"
  (let ((directive
         (mevedel-directive--create
          :id "directive" :request "Request" :anchor '(:state attached)
          :state 'implemented
          :attempts
          (list
           (mevedel-directive-attempt--create
            :directive-request "Request" :request "Exact"
            :result "Done" :outcome 'success :patch "" :capture 'complete
            :captured-at "2026-07-02T12:00:00+0200")))))
    (should-error
     (mevedel--request-changes-prompt "Request" directive "")
     :type 'user-error)
    (should
     (string-search
      "newly supplied directive context"
      (mevedel--request-changes-prompt "Request\nChild change" directive nil
                                       t)))
    (mevedel-directive-set-request directive "Edited request")
    (should-error
     (mevedel--request-changes-prompt "Request" directive "Feedback")
     :type 'user-error)))

(mevedel-deftest mevedel--retry-directive-prompt ()
  ,test
  (test)
  :doc "uses the latest error and observed partial changes with optional guidance"
  (let* ((directive
          (mevedel-directive--create
           :id "directive" :request "Request" :anchor '(:state attached)
           :state 'failed
           :attempts
           (list
            (mevedel-directive-attempt--create
             :directive-request "Request" :request "Older"
             :result "Old failure" :outcome 'error :patch "old patch"
             :capture 'complete :captured-at "2026-07-01T10:00:00+0200")
            (mevedel-directive-attempt--create
             :directive-request "Request" :request "SECRET LATEST REQUEST"
             :result "Latest failure" :outcome 'error :patch "partial patch"
             :plan "Failed accepted plan"
             :capture 'incomplete
             :captured-at "2026-07-02T11:30:00+0200"))))
         (guided
          (mevedel--retry-directive-prompt
           "Request\n\nFresh reference" directive "Try the parser first"))
         (unguided
          (mevedel--retry-directive-prompt
           "Request\n\nFresh reference" directive "")))
    (dolist (text '("Current repository state is authoritative"
                    "Fresh reference" "Latest failure" "partial patch"
                    "Try the parser first" "Failed accepted plan"))
      (should (string-search text guided)))
    (dolist (text '("Old failure" "old patch" "SECRET LATEST REQUEST"))
      (should-not (string-search text guided)))
    (should-not (string-search "OPTIONAL GUIDANCE" unguided))
    (mevedel-directive-set-request directive "Edited request")
    (should-error
     (mevedel--retry-directive-prompt "Request" directive "")
     :type 'user-error)))

(mevedel-deftest mevedel--attach-directive-skills ()
  ,test
  (test)
  :doc "appends selected skills and validates them against the session"
  (progn
    (require 'mevedel-plan-handoff)
    (require 'mevedel-skills-invoke)
    (require 'mevedel-skills-core)
    (let* ((source "/tmp/alpha/SKILL.md")
           (skill (mevedel-skill--create
                   :name "alpha" :source-file source
                   :user-invocable-p t :active-p t))
           (session (mevedel-session--create :name "main"
                                             :skills (list skill)))
           (record (mevedel-directive--create
                    :id "directive" :request "Request"
                    :skills (list (list :name "alpha"
                                        :source-file source))))
           (chat-buffer (generate-new-buffer " *directive-skills-chat*")))
      (unwind-protect
          (with-current-buffer chat-buffer
            (setq-local mevedel--session session)
            (cl-letf (((symbol-function 'mevedel-skills-prepare-user-input)
                       (lambda (input _) input)))
              (let ((result (mevedel--attach-directive-skills
                             "PROMPT" record chat-buffer)))
                (should (string-search "Implementation skills:" result))
                (should (string-search "Use $alpha" result)))
              (setf (mevedel-session-skills session) nil)
              (should-error
               (mevedel--attach-directive-skills
                "PROMPT" record chat-buffer))))
        (kill-buffer chat-buffer))))

  :doc "returns the prompt unchanged without a selection"
  (let ((record (mevedel-directive--create :id "d" :request "r"))
        (chat-buffer (generate-new-buffer " *directive-skills-none*")))
    (unwind-protect
        (should (equal "PROMPT"
                       (mevedel--attach-directive-skills
                        "PROMPT" record chat-buffer)))
      (kill-buffer chat-buffer))))

(mevedel-deftest mevedel-revision-api-removed
  (:doc "removes the superseded revision command and prompt path")
  (should-not (fboundp 'mevedel-revise-directive))
  (should-not (fboundp 'mevedel--revise-directive-prompt)))

(mevedel-deftest mevedel-discuss-directive
  (:doc "opens shared directive scope and submits the initial discussion")
  (let ((directive (make-overlay (point-min) (point-min)))
        scoped started)
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel--instructions-at)
                   (lambda (&rest _) (list directive)))
                  ((symbol-function 'mevedel--highest-priority-instruction)
                   (lambda (instructions &optional _) (car instructions)))
                  ((symbol-function 'mevedel--topmost-instruction)
                   (lambda (instruction _) instruction))
                  ((symbol-function 'mevedel--directive-record)
                   (lambda (_) (mevedel-directive--create)))
                  ((symbol-function 'mevedel-view-enter-directive-scope)
                   (lambda (selected action &rest _)
                     (setq scoped (list selected action))))
                  ((symbol-function 'mevedel--directive-status)
                   (lambda (_) nil))
                  ((symbol-function 'mevedel--start-directive-discussion)
                   (lambda (selected &rest _)
                     (setq started selected)
                     'accepted)))
          (should (eq 'accepted (mevedel-discuss-directive)))
          (should (equal (list directive 'discuss) scoped))
          (should (eq directive started)))
      (delete-overlay directive))))

(mevedel-deftest mevedel-implement-directive ()
  ,test
  (test)

  :doc "starts planning instead of implementation when the directive opts in"
  (let ((directive (make-overlay (point-min) (point-min)))
        (record
         (mevedel-directive--create
          :request "Request" :planning-enabled t))
        planned processed)
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel--instructions-at)
                   (lambda (&rest _) (list directive)))
                  ((symbol-function 'mevedel--highest-priority-instruction)
                   (lambda (instructions &optional _) (car instructions)))
                  ((symbol-function 'mevedel--topmost-instruction)
                   (lambda (instruction _) instruction))
                  ((symbol-function 'mevedel--directive-record)
                   (lambda (_) record))
                  ((symbol-function 'mevedel-directive-plan-start)
                   (lambda (selected action prompt-fn callback &optional _)
                     (setq planned
                           (list selected action prompt-fn callback))))
                  ((symbol-function 'mevedel--process-directive)
                   (lambda (&rest _) (setq processed t))))
          (mevedel-implement-directive #'ignore)
          (should (equal (list directive 'implement
                               #'mevedel--implement-directive-prompt #'ignore)
                         planned))
          (should-not processed))
      (delete-overlay directive)))

  :doc "rejects a fresh implementation after an attempt exists"
  (let ((directive (make-overlay (point-min) (point-min)))
        (record
         (mevedel-directive--create
          :request "Request"
          :attempts
          (list (mevedel-directive-attempt--create
                 :directive-request "Request" :outcome 'success))))
        processed)
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel--instructions-at)
                   (lambda (&rest _) (list directive)))
                  ((symbol-function 'mevedel--highest-priority-instruction)
                   (lambda (instructions &optional _) (car instructions)))
                  ((symbol-function 'mevedel--topmost-instruction)
                   (lambda (instruction _) instruction))
                  ((symbol-function 'mevedel--directive-record)
                   (lambda (_) record))
                  ((symbol-function 'mevedel--process-directive)
                   (lambda (&rest _) (setq processed t))))
          (should-error (mevedel-implement-directive) :type 'user-error)
          (should-not processed))
      (delete-overlay directive))))

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

(mevedel-deftest mevedel--directive-model-policy ()
  ,test
  (test)

  :doc "returns nil when the directive inherits its session model"
  (with-temp-buffer
    (insert "directive")
    (should-not
     (mevedel--directive-model-policy
      (make-overlay (point-min) (point-max)))))

  :doc "resolves and validates a pinned provider and effort"
  (with-temp-buffer
    (insert "directive")
    (let ((directive (make-overlay (point-min) (point-max)))
          (backend (list :backend 'directive))
          validated)
      (overlay-put directive
                   'mevedel-directive-model-provider "Directive:model")
      (overlay-put directive
                   'mevedel-directive-reasoning-effort 'high)
      (cl-letf (((symbol-function 'mevedel-model-resolve-provider)
                 (lambda (provider &optional _)
                   (should (equal provider "Directive:model"))
                   (list :backend backend :model 'model)))
                ((symbol-function 'mevedel-model-validate-effort)
                 (lambda (model effort)
                   (setq validated (list model effort))
                   effort)))
        (should
         (equal (list :backend backend :model 'model :effort 'high)
                (mevedel--directive-model-policy directive)))
        (should (equal '(model high) validated))))))

(mevedel-deftest mevedel-process-directives
  (:vars
   ((gptel-default-mode 'markdown-mode)
    (mevedel--instruction-states (make-hash-table :test #'equal))
    (mevedel--instruction-current-state-key :global)))
  ,test
  (test)

  :doc "queues only top-level directives in stable source order"
  (let* ((workspace (mevedel-workspace--create
                     :type 'test :id "batch" :root nil :name "batch"))
         captured-records captured-workspace captured-current captured-total)
    (with-temp-buffer
      (insert "root one detail\nroot two\n")
      (setq-local mevedel--workspace workspace)
      (let* ((second
              (mevedel--create-directive-in
               (current-buffer) 17 25 nil "Root two"))
             (first
              (mevedel--create-directive-in
               (current-buffer) 1 16 nil "Root one")))
        (mevedel--create-directive-in
         (current-buffer) 6 9 nil "Nested detail")
        (goto-char (point-max))
        (deactivate-mark)
        (cl-letf (((symbol-function 'completing-read-multiple)
                   (lambda (_prompt collection &rest _)
                     (reverse collection)))
                  ((symbol-function 'mevedel--process-directives-sequentially)
                   (lambda (records owner current total)
                     (setq captured-records records
                           captured-workspace owner
                           captured-current current
                           captured-total total))))
          (mevedel-process-directives)
          (should
           (equal '("Root one" "Root two")
                  (mapcar #'mevedel-directive-request captured-records)))
          (should (eq workspace captured-workspace))
          (should (= 1 captured-current))
          (should (= 2 captured-total))
          (should (memq first (mevedel--instructions-in 1 25 'directive)))
          (should (memq second (mevedel--instructions-in 1 25 'directive))))))))

(mevedel-deftest mevedel--process-directives-sequentially ()
  ,test
  (test)

  :doc "defers the next directive until terminal request cleanup can finish"
  (let* ((buf (generate-new-buffer " *mevedel-directives-sequential*"))
         (workspace (mevedel-workspace--create
                     :type 'test :id "batch" :root nil :name "batch"))
         (record1 (mevedel-directive--create
                   :id "one" :request "first" :anchor '(:state attached)))
         (record2 (mevedel-directive--create
                   :id "two" :request "second" :anchor '(:state attached)))
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
          (cl-letf (((symbol-function 'mevedel--directive-action-context)
                     (lambda (record owner)
                       (should (eq workspace owner))
                       (list :directive (if (eq record record1) ov1 ov2)
                             :prompt "prompt")))
                    ((symbol-function 'mevedel--process-directive)
                     (lambda (directive _preset _prompt-fn callback)
                       (push (overlay-get directive 'mevedel-id) calls)
                       (funcall callback nil nil)))
                    ((symbol-function 'run-at-time)
                     (lambda (_secs _repeat function &rest args)
                       (setq scheduled-fn function
                             scheduled-args args)
                       'timer)))
            (mevedel--process-directives-sequentially
             (list record1 record2) workspace 1 2)
            (should (equal '(1) calls))
            (should (eq scheduled-fn #'mevedel--process-directives-sequentially))
            (should (equal (list (list record2) workspace 2 2)
                           scheduled-args))
            (apply scheduled-fn scheduled-args)
            (should (equal '(2 1) calls))))
      (when (buffer-live-p buf)
        (kill-buffer buf))))

  :doc "implements only eligible initial work and reports every skipped item"
  (let* ((buf (generate-new-buffer " *mevedel-directive-batch-state*"))
         (workspace (mevedel-workspace--create
                     :type 'test :id "batch" :root nil :name "batch"))
         (attempted-success
          (mevedel-directive--create
           :id "success" :request "Success" :anchor '(:state attached)
           :state 'implemented
           :attempts (list (mevedel-directive-attempt--create
                            :outcome 'success))))
         (attempted-failure
          (mevedel-directive--create
           :id "failure" :request "Failure" :anchor '(:state attached)
           :state 'failed
           :attempts (list (mevedel-directive-attempt--create
                            :outcome 'error))))
         (attempted-abort
          (mevedel-directive--create
           :id "abort" :request "Abort" :anchor '(:state attached)
           :state 'aborted
           :attempts (list (mevedel-directive-attempt--create
                            :outcome 'aborted))))
         (discussed
          (mevedel-directive--create
           :id "discussed" :request "Discussed" :anchor '(:state attached)
           :state 'discussed
           :discussion
           (list (mevedel-directive-discussion-turn--create
                  :directive-request "Discussed"
                  :message "Question" :request "Question prompt"
                  :result "Answer" :outcome 'success))))
         (ready
          (mevedel-directive--create
           :id "ready" :request "Ready" :anchor '(:state attached)))
         (detached
          (mevedel-directive--create
           :id "detached" :request "Detached" :anchor '(:state detached)))
         (missing
          (mevedel-directive--create
           :id "missing" :request "Missing" :anchor '(:state source-missing)))
         (busy
          (mevedel-directive--create
           :id "busy" :request "Busy" :anchor '(:state attached)
           :state 'implementing))
         calls validated messages)
    (unwind-protect
        (with-current-buffer buf
          (insert "abcdef")
          (let ((discussed-overlay (make-overlay 1 2))
                (ready-overlay (make-overlay 2 3))
                (detached-overlay (make-overlay 3 3)))
            (overlay-put discussed-overlay 'mevedel-id 4)
            (overlay-put ready-overlay 'mevedel-id 5)
            (overlay-put detached-overlay 'mevedel-id 6)
            (cl-letf (((symbol-function 'mevedel--directive-action-context)
                       (lambda (record owner)
                         (should (eq workspace owner))
                         (push (mevedel-directive-id record) validated)
                         (when (eq record missing)
                           (user-error "Directive prompt context is unavailable; reattach its source first"))
                         (list :directive
                               (cond ((eq record discussed) discussed-overlay)
                                     ((eq record ready) ready-overlay)
                                     ((eq record detached) detached-overlay)
                                     (t (error "Unexpected record")))
                               :prompt "Base prompt")))
                      ((symbol-function 'mevedel--implement-discussion)
                       (lambda (directive callback)
                         (push (list 'discussion directive) calls)
                         (funcall callback nil nil)))
                      ((symbol-function 'mevedel--process-directive)
                       (lambda (directive _preset _prompt callback &optional _)
                         (push (list 'initial directive) calls)
                         (funcall callback nil nil)))
                      ((symbol-function 'run-at-time)
                       (lambda (_secs _repeat function &rest args)
                         (apply function args)))
                      ((symbol-function 'message)
                       (lambda (format-string &rest args)
                         (push (apply #'format format-string args) messages))))
              (mevedel--process-directives-sequentially
               (list attempted-success attempted-failure attempted-abort
                     discussed ready detached missing busy)
               workspace 1 8)
              (should
               (equal (list (list 'discussion discussed-overlay)
                            (list 'initial ready-overlay)
                            (list 'initial detached-overlay))
                      (nreverse calls))))))
      (should (equal '("discussed" "ready" "detached" "missing")
                     (nreverse validated)))
      (let ((output (string-join messages "\n")))
        (should (string-match-p "existing implementation activity" output))
        (should (string-match-p "reattach its source first" output))
        (should (string-match-p "lifecycle state Implementing" output)))
      (when (buffer-live-p buf)
        (kill-buffer buf))))

  :doc "stops after the first failed or aborted implementation"
  (dolist (terminal '("model failure" abort))
    (let* ((buf (generate-new-buffer " *mevedel-directive-batch-stop*"))
           (workspace (mevedel-workspace--create
                       :type 'test :id "batch" :root nil :name "batch"))
           (first (mevedel-directive--create
                   :id "one" :request "One" :anchor '(:state attached)))
           (second (mevedel-directive--create
                    :id "two" :request "Two" :anchor '(:state attached)))
           (third (mevedel-directive--create
                   :id "three" :request "Three" :anchor '(:state attached)))
           calls timers messages)
      (unwind-protect
          (with-current-buffer buf
            (insert "abc")
            (let ((overlays
                   (mapcar (lambda (position)
                             (let ((overlay (make-overlay position (1+ position))))
                               (overlay-put overlay 'mevedel-id position)
                               overlay))
                           '(1 2 3))))
              (cl-letf (((symbol-function 'mevedel--directive-action-context)
                         (lambda (record _workspace)
                           (list :directive
                                 (nth (cl-position record
                                                   (list first second third))
                                      overlays)
                                 :prompt "prompt")))
                        ((symbol-function 'mevedel--process-directive)
                         (lambda (directive _preset _prompt callback &optional _)
                           (push directive calls)
                           (funcall callback terminal nil)))
                        ((symbol-function 'run-at-time)
                         (lambda (&rest _)
                           (push t timers)))
                        ((symbol-function 'message)
                         (lambda (format-string &rest args)
                           (push (apply #'format format-string args) messages))))
                (mevedel--process-directives-sequentially
                 (list first second third) workspace 1 3)
                (should (= 1 (length calls)))
                (should-not timers)
                (should
                 (string-match-p
                  (if (eq terminal 'abort) "aborted" "failed")
                  (string-join messages "\n"))))))
        (when (buffer-live-p buf)
          (kill-buffer buf))))))

(mevedel-deftest mevedel--process-directive
		 (:before-each (mevedel-workspace-clear-registry)
			       :after-each (mevedel-workspace-clear-registry))
		 ,test
		 (test)

		 :doc "isolates the prompt, binds the session, and settles a durable attempt"
		 (let* ((tmpdir (file-name-as-directory
				 (make-temp-file "mevedel-directive-" t)))
			(file (file-name-concat tmpdir "sample.txt"))
			(buf (find-file-noselect file))
                        (override-backend (list :backend 'directive))
                        (override-model 'directive-model)
			captured-prompt captured-args captured-fsm captured-chat
			callback-result override-validated later-child)
		   (unwind-protect
		       (with-current-buffer buf
			 (erase-buffer)
			 (insert "alpha\nbeta\n")
			 (write-region (point-min) (point-max) file nil 'silent)
			 (set-buffer-modified-p nil)
			 (goto-char (point-min))
			 (let* ((directive (mevedel--create-directive-in
					    buf (point-min) (line-end-position)
					    nil "Change alpha."))
				(submitted-child
				 (mevedel--create-directive-in
				  buf (1+ (point-min)) (+ 3 (point-min))
				  nil "Use the first detail."))
				(submitted-id
				 (overlay-get submitted-child 'mevedel-uuid))
				(older-attempt
				 (mevedel-directive-attempt--create
				  :directive-request "Older request"
				  :request "Older exact request" :result "Older result"
				  :outcome 'error :patch "" :capture 'complete
				  :captured-at "2026-08-01T01:00:00+0200"
				  :checkpoint '(:session-id "older" :turn 1))))
			   (setf (mevedel-directive-attempts
				  (mevedel--directive-record directive))
				 (list older-attempt))
			   (overlay-put directive 'mevedel-directive-action 'implement)
                           (overlay-put directive
                                        'mevedel-directive-model-provider
                                        "Directive:directive-model")
                           (overlay-put directive
                                        'mevedel-directive-reasoning-effort
                                        'high)
			   (cl-letf (((symbol-function 'save-some-buffers)
				      (lambda (&rest _) nil))
				     ((symbol-function 'display-buffer)
				      (lambda (&rest _) nil))
				     ((symbol-function 'gptel--apply-preset)
				      (lambda (&rest _) nil))
                                     ((symbol-function
                                       'mevedel-model-resolve-provider)
                                      (lambda (provider &optional _)
                                        (should
                                         (equal provider
                                                "Directive:directive-model"))
                                        (list :backend override-backend
                                              :model override-model)))
                                     ((symbol-function
                                       'mevedel-model-validate-effort)
                                      (lambda (model effort)
                                        (when (eq model override-model)
                                          (should (eq effort 'high))
                                          (setq override-validated t))
                                        effort))
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
			      submitted-child '(:system "test")
			      #'mevedel--implement-directive-prompt
			      (lambda (err fsm)
				(setq callback-result
				      (list err (eq fsm captured-fsm)))))
			     (should (string-match-p "IMPLEMENTATION REQUEST"
						     captured-prompt))
			     (should (string-match-p "Change alpha" captured-prompt))
			     (should (string-match-p "Use the first detail" captured-prompt))
			     (should (eq captured-chat (plist-get captured-args :buffer)))
			     (should (markerp (plist-get captured-args :position)))
                             (should override-validated)
                             (let* ((transforms
                                     (plist-get captured-args :transforms))
                                    (directive-transform
                                     (car (last transforms)))
                                    session-backend session-model session-effort)
                               (with-current-buffer captured-chat
                                 (setq session-backend gptel-backend
                                       session-model gptel-model
                                       session-effort gptel-reasoning-effort))
                               (with-temp-buffer
                                 (setq-local gptel-backend 'prompt-backend
                                             gptel-model 'prompt-model
                                             gptel-reasoning-effort 'low)
                                 (funcall directive-transform nil)
                                 (should (eq gptel-backend override-backend))
                                 (should (eq gptel-model override-model))
                                 (should (eq gptel-reasoning-effort 'high)))
                               (with-current-buffer captured-chat
                                 (should (eq gptel-backend session-backend))
                                 (should (eq gptel-model session-model))
                                 (should
                                  (eq gptel-reasoning-effort
                                      session-effort))))
			     (with-current-buffer captured-chat
			       (should (eq 'implementing
					   (mevedel--directive-status directive)))
			       (should (equal (overlay-get directive 'mevedel-uuid)
					      mevedel--current-directive-uuid))
			       (save-excursion
				 (goto-char (point-min))
				 (should (search-forward ":PROMPT:" nil t))
				 (should-not (get-text-property (point) 'gptel))))
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
				 (should (overlayp
					  (mevedel-view-zone-region 'progress)))))
			     (let ((response-start
				    (plist-get captured-args :position)))
			       (should (markerp response-start))
			       (with-current-buffer captured-chat
				 (goto-char response-start)
				 (insert (propertize "Answer text.\n" 'gptel 'response)))
			       (let ((view-buf (buffer-local-value 'mevedel--view-buffer
								   captured-chat)))
				 (with-current-buffer view-buf
				   (mevedel-view--render-incremental captured-chat)))
			       (let ((view-buf (buffer-local-value 'mevedel--view-buffer
								   captured-chat)))
				 (with-current-buffer view-buf
				   (should (string-search "Answer text"
							  (buffer-string))))))
			     (setf (gptel-fsm-info captured-fsm)
				   (append
				    (gptel-fsm-info captured-fsm)
				    '(:mevedel-directive-patch "diff --git a/sample.txt b/sample.txt\n"
				      :mevedel-directive-capture complete
				      :mevedel-directive-covered-files ("/tmp/sample.txt")
				      :mevedel-directive-gaps nil)))
			     (setq later-child
				   (mevedel--create-directive-in
				    buf (+ 3 (point-min)) (+ 5 (point-min))
				    nil "Keep later detail."))
			     (mevedel--remove-directive-presentation submitted-child)
				     (funcall (plist-get (gptel-fsm-info captured-fsm)
							 :mevedel-request-callback)
					      nil captured-fsm)
				     (with-current-buffer captured-chat
				       (mevedel--turn-commit captured-fsm)
				       (mevedel-request-end))
				     (should (equal '(nil t) callback-result))
			     (should (eq 'implemented
					 (mevedel--directive-status directive)))
			     (let* ((record (mevedel--directive-record directive))
				    (attempt
				     (car (last (mevedel-directive-attempts record))))
				    (checkpoint
				     (mevedel-directive-attempt-checkpoint attempt)))
			       (should (= 2 (length
					     (mevedel-directive-attempts record))))
			       (should (eq older-attempt
					   (car (mevedel-directive-attempts record))))
			       (should (equal (mevedel-directive-session-id record)
					      (plist-get checkpoint :session-id)))
			       (should (= 1 (plist-get checkpoint :turn)))
			       (should (equal captured-prompt
					      (mevedel-directive-attempt-request attempt)))
			       (should (eq 'implement
					   (mevedel-directive-attempt-action attempt)))
			       (should (equal "Change alpha."
					      (mevedel-directive-attempt-directive-request
					       attempt)))
			       (should
				(stringp
				 (mevedel-directive-attempt-captured-at attempt)))
			       (should (equal "Answer text.\n"
					      (mevedel-directive-attempt-result attempt)))
			       (should (eq 'success
					   (mevedel-directive-attempt-outcome attempt)))
			       (should (eq 'complete
					   (mevedel-directive-attempt-capture attempt)))
			       (should
				(equal
				 (list submitted-id)
				 (mapcar
				  #'mevedel-subdirective-id
				  (mevedel-directive-attempt-consumed-subdirectives
				   attempt))))
			       (should-not (overlay-buffer submitted-child))
			       (should (overlay-buffer later-child))
			       (should
				(equal
				 (list (overlay-get later-child 'mevedel-uuid))
				 (mapcar #'mevedel-subdirective-id
					 (mevedel-directive-subdirectives record)))))
			     (with-current-buffer captured-chat
			       (should (string-search ":PROMPT:" (buffer-string)))
			       (should (string-search "Answer text" (buffer-string)))
			       (should (= 2 (length
					     (mevedel-transcript-audit-records
					      (buffer-string)
					      'directive-turn-boundary))))
			       (mevedel-session-persistence-save
				mevedel--session captured-chat)
			       (let ((segment buffer-file-name))
				 (with-temp-buffer
				   (insert-file-contents segment)
				   (should (= 2 (length
						 (mevedel-transcript-audit-records
						  (buffer-string)
						  'directive-turn-boundary))))
				   (should (string-search "Answer text"
						  (buffer-string)))))
			       (should-not mevedel--current-directive-uuid)))))
		     (when (buffer-live-p buf)
		       (kill-buffer buf))
		     (when (and captured-chat (buffer-live-p captured-chat))
		       (let ((view-buf (buffer-local-value 'mevedel--view-buffer
							   captured-chat)))
			 (when (buffer-live-p view-buf)
			   (kill-buffer view-buf)))
		       (kill-buffer captured-chat))
			     (delete-directory tmpdir t)))

		 :doc "records failure and abort outcomes while retaining full turns"
		 (dolist (case '((error failed error "transport failed")
				 (abort aborted aborted "Request aborted")))
		   (pcase-let* ((`(,kind ,state ,outcome ,result) case)
				(tmpdir (file-name-as-directory
					 (make-temp-file "mevedel-directive-terminal-" t)))
				(file (file-name-concat tmpdir "sample.txt"))
				(buf (find-file-noselect file))
				(captured-fsm nil)
				(captured-chat nil))
		     (unwind-protect
			 (with-current-buffer buf
			   (insert "source\n")
			   (write-region (point-min) (point-max) file nil 'silent)
			   (set-buffer-modified-p nil)
			   (let* ((directive
				   (mevedel--create-directive-in
				    buf (point-min) (1- (point-max)) nil "Change it"))
				  (child
				   (mevedel--create-directive-in
				    buf (1+ (point-min)) (- (point-max) 2)
				    nil "Keep this detail")))
			     (overlay-put directive 'mevedel-directive-action 'implement)
			     (cl-letf (((symbol-function 'save-some-buffers)
					(lambda (&rest _) nil))
				       ((symbol-function 'display-buffer)
					(lambda (&rest _) nil))
				       ((symbol-function 'gptel--apply-preset)
					(lambda (&rest _) nil))
				       ((symbol-function 'gptel-request)
					(lambda (_prompt &rest args)
					  (setq captured-chat (plist-get args :buffer))
					  (let ((fsm (plist-get args :fsm)))
					    (setf (gptel-fsm-info fsm)
						  (list :buffer captured-chat
							:position (plist-get args :position)
							:callback (lambda (&rest _) nil)))
					    (setq captured-fsm fsm)
					    fsm))))
			       (mevedel--process-directive
				directive '(:system "test")
				#'mevedel--implement-directive-prompt nil)
			       (if (eq kind 'abort)
				   (funcall
				    (plist-get (gptel-fsm-info captured-fsm)
					       :mevedel-request-callback)
				    'abort captured-fsm)
				 (setf (gptel-fsm-state captured-fsm) 'ERRS
				       (gptel-fsm-info captured-fsm)
				       (plist-put
					(gptel-fsm-info captured-fsm)
					:error '(:message "transport failed")))
					 (funcall
					  (plist-get (gptel-fsm-info captured-fsm)
						     :mevedel-request-callback)
					  nil captured-fsm))
				       (with-current-buffer captured-chat
					 (mevedel--turn-commit captured-fsm)
					 (mevedel-request-end))
				       (let* ((record (mevedel--directive-record directive))
				      (attempt
				       (car (mevedel-directive-attempts record))))
				 (should (eq state (mevedel-directive-state record)))
				 (should
				  (eq outcome
				      (mevedel-directive-attempt-outcome attempt)))
				 (should
				  (equal result
					 (mevedel-directive-attempt-result attempt)))
				 (should-not
				  (mevedel-directive-attempt-consumed-subdirectives
				   attempt))
				 (should (overlay-buffer child))
				 (should (= 1 (length
					       (mevedel-directive-subdirectives record)))))
			       (with-current-buffer captured-chat
				 (should (= 2 (length
					       (mevedel-transcript-audit-records
						(buffer-string)
						'directive-turn-boundary))))
				 (should (string-search ":PROMPT:"
							(buffer-string)))))))
		       (when (buffer-live-p buf)
			 (kill-buffer buf))
		       (when (buffer-live-p captured-chat)
			 (let ((view-buf
				(buffer-local-value 'mevedel--view-buffer
						    captured-chat)))
			   (when (buffer-live-p view-buf)
			     (kill-buffer view-buf)))
			 (kill-buffer captured-chat))
		       (delete-directory tmpdir t))))

                 :doc "starts discussion from the directive as a first-class turn"
                 (let* ((tmpdir (file-name-as-directory
                                 (make-temp-file "mevedel-discussion-turn-" t)))
                        (file (file-name-concat tmpdir "sample.txt"))
                        (buf (find-file-noselect file))
                        (mevedel-action-preset-alist
                         '((discuss . (:system "test"))))
                        captured-fsm captured-chat captured-prompt)
                   (unwind-protect
                       (with-current-buffer buf
                         (insert "source\n")
                         (write-region (point-min) (point-max) file nil 'silent)
                         (set-buffer-modified-p nil)
                         (let ((directive
                                (mevedel--create-directive-in
                                 buf (point-min) (1- (point-max)) nil
                                 "Explain it")))
                           (overlay-put directive 'mevedel-directive-action 'discuss)
                           (cl-letf (((symbol-function 'save-some-buffers)
                                      (lambda (&rest _) nil))
                                     ((symbol-function 'display-buffer)
                                      (lambda (&rest _) nil))
                                     ((symbol-function 'gptel--apply-preset)
                                      (lambda (&rest _) nil))
                                     ((symbol-function 'gptel-request)
                                      (lambda (prompt &rest args)
                                        (setq captured-prompt prompt
                                              captured-chat
                                              (plist-get args :buffer))
                                        (let ((fsm (plist-get args :fsm)))
                                          (setf (gptel-fsm-info fsm)
                                                (list
                                                 :buffer captured-chat
                                                 :position
                                                 (plist-get args :position)
                                                 :callback
                                                 (lambda (&rest _) nil)))
                                          (setq captured-fsm fsm)
                                          fsm))))
                             (mevedel--start-directive-discussion
                              directive nil)
                             (should (eq 'discussing
                                         (mevedel--directive-status directive)))
                             (let ((response-start
                                    (plist-get (gptel-fsm-info captured-fsm)
                                               :position)))
                               (with-current-buffer captured-chat
                                 (goto-char response-start)
                                 (insert "Because.\n")))
                             (funcall
                              (plist-get (gptel-fsm-info captured-fsm)
                                         :mevedel-request-callback)
                              nil captured-fsm)
                             (with-current-buffer captured-chat
                               (mevedel--turn-commit captured-fsm)
                               (mevedel-request-end))
                             (let* ((record
                                     (mevedel--directive-record directive))
                                    (turn
                                     (car
                                      (mevedel-directive-discussion record))))
                               (should (eq 'discussed
                                           (mevedel-directive-state record)))
                               (should-not
                                (mevedel-directive-attempts record))
                               (should (equal "Explain it"
                                              (mevedel-directive-discussion-turn-message
                                               turn)))
                               (should (equal "Explain it"
                                              (mevedel-directive-discussion-turn-directive-request
                                               turn)))
                               (should (equal captured-prompt
                                              (mevedel-directive-discussion-turn-request
                                               turn)))
                               (should-not
                                (string-match-p "### QUESTION:" captured-prompt))
                               (should (equal "Because.\n"
                                              (mevedel-directive-discussion-turn-result
                                               turn)))
                               (should (= 1
                                          (mevedel-directive-discussion-turn-sequence
                                           turn)))
                               (setf
                                (mevedel-directive-attempts record)
                                (list
                                 (mevedel-directive-attempt--create
                                  :sequence 2
                                  :directive-request "Explain it"
                                  :outcome 'success))
                                (mevedel-directive-state record) 'implemented)
                               (mevedel--discuss-directive-turn
                                directive "Anything else?" 1 nil)
                               (let ((response-start
                                      (plist-get
                                       (gptel-fsm-info captured-fsm)
                                       :position)))
                                 (with-current-buffer captured-chat
                                   (goto-char response-start)
                                   (insert "No.\n")))
                               (funcall
                                (plist-get (gptel-fsm-info captured-fsm)
                                           :mevedel-request-callback)
                                nil captured-fsm)
                               (should (eq 'implemented
                                           (mevedel-directive-state record)))
                               (should (= 3
                                          (mevedel-directive-discussion-turn-sequence
                                           (car
                                            (last
                                             (mevedel-directive-discussion
                                              record)))))))
                             (with-current-buffer captured-chat
                               (should (= 4 (length
                                             (mevedel-transcript-audit-records
                                              (buffer-string)
                                              'directive-turn-boundary))))
                               (should (string-search "Because."
                                                      (buffer-string)))
                               (should (string-search "No."
                                                      (buffer-string))))))
                     (when (buffer-live-p buf)
                       (kill-buffer buf))
                     (when (buffer-live-p captured-chat)
                       (let ((view-buf
                              (buffer-local-value 'mevedel--view-buffer
                                                  captured-chat)))
                         (when (buffer-live-p view-buf)
                           (kill-buffer view-buf)))
                       (kill-buffer captured-chat))
                     (delete-directory tmpdir t))))

                 :doc "keeps Tutor outside directive lifecycle activity"
                 (let* ((tmpdir (file-name-as-directory
                                 (make-temp-file "mevedel-directive-tutor-" t)))
                        (file (file-name-concat tmpdir "sample.txt"))
                        (buf (find-file-noselect file))
                        captured-fsm captured-chat)
                   (unwind-protect
                       (with-current-buffer buf
                         (insert "source\n")
                         (write-region (point-min) (point-max) file nil 'silent)
                         (set-buffer-modified-p nil)
                         (let* ((directive
                                 (mevedel--create-directive-in
                                  buf (point-min) (1- (point-max)) nil
                                  "Explain it"))
                                (record (mevedel--directive-record directive))
                                (attempt
                                 (mevedel-directive-attempt--create
                                  :sequence 1 :action 'implement
                                  :directive-request "Explain it"
                                  :request "Implement" :result "Done"
                                  :outcome 'success :patch ""
                                  :capture 'complete :covered-files nil :gaps nil
                                  :untracked-effects nil
                                  :captured-at "2026-08-07T00:00:00+0200"
                                  :checkpoint
                                  '(:session-id "older" :turn 1))))
                           (setf (mevedel-directive-attempts record)
                                 (list attempt)
                                 (mevedel-directive-state record) 'implemented)
                           (overlay-put directive 'mevedel-directive-action 'tutor)
                           (cl-letf (((symbol-function 'save-some-buffers)
                                      (lambda (&rest _) nil))
                                     ((symbol-function 'display-buffer)
                                      (lambda (&rest _) nil))
                                     ((symbol-function 'gptel--apply-preset)
                                      (lambda (&rest _) nil))
                                     ((symbol-function 'gptel-request)
                                      (lambda (_prompt &rest args)
                                        (setq captured-chat
                                              (plist-get args :buffer))
                                        (let ((fsm (plist-get args :fsm)))
                                          (setf (gptel-fsm-info fsm)
                                                (list
                                                 :buffer captured-chat
                                                 :position
                                                 (plist-get args :position)
                                                 :callback
                                                 (lambda (&rest _) nil)))
                                          (setq captured-fsm fsm)
                                          fsm))))
                             (mevedel--process-directive
                              directive '(:system "test") #'identity nil)
                             (with-current-buffer captured-chat
                               (goto-char
                                (plist-get (gptel-fsm-info captured-fsm)
                                           :position))
                               (insert "Hint.\n"))
                             (funcall
                              (plist-get (gptel-fsm-info captured-fsm)
                                         :mevedel-request-callback)
                              nil captured-fsm)
                             (with-current-buffer captured-chat
                               (mevedel--turn-commit captured-fsm)
                               (mevedel-request-end))
                             (should (equal (list attempt)
                                            (mevedel-directive-attempts record)))
                             (should-not (mevedel-directive-discussion record))
                             (should (eq 'implemented
                                         (mevedel-directive-state record)))
                             (should
                              (mevedel--serialize-directives
                               (mevedel-workspace) tmpdir)))))
                     (when (buffer-live-p buf)
                       (kill-buffer buf))
                     (when (buffer-live-p captured-chat)
                       (let ((view-buf
                              (buffer-local-value 'mevedel--view-buffer
                                                  captured-chat)))
                         (when (buffer-live-p view-buf)
                           (kill-buffer view-buf)))
                       (kill-buffer captured-chat))
                     (delete-directory tmpdir t)))

                 :doc "discards synthetic source context when startup is quit"
                 (let* ((workspace
                         (mevedel-workspace--create
                          :type 'test :id "source-missing" :root "/tmp"
                          :name "source-missing"))
                        (record
                         (mevedel-directive--create
                          :id "directive" :request "Request"
                          :anchor
                          '(:state source-missing :file "/tmp/missing.el"
                            :start 1 :end 1
                            :evidence (:schema 1 :bodyless t)
                            :properties
                            (mevedel-instruction t
                             mevedel-uuid "directive"
                             mevedel-instruction-type directive))))
                        (context
                         (progn
                           (mevedel-workspace-set-directives
                            workspace (list record))
                           (mevedel--directive-action-context
                            record workspace)))
                        (directive (plist-get context :directive))
                        (transient-buffer (overlay-buffer directive)))
                   (unwind-protect
                       (progn
                         (overlay-put directive
                                      'mevedel-directive-action 'implement)
                        (cl-letf
                            (((symbol-function
                               'mevedel--directive-session-buffer)
                               (lambda (&rest _)
                                 (signal 'quit nil))))
                           (let (quit-p)
                             (condition-case nil
                                 (mevedel--process-directive
                                  directive '(:system "test")
                                  #'mevedel--implement-directive-prompt nil)
                               (quit (setq quit-p t)))
                             (should quit-p)))
                         (should-not (buffer-live-p transient-buffer))
                         (should-not (overlay-buffer directive)))
                     (when (buffer-live-p transient-buffer)
                       (kill-buffer transient-buffer))))

                 :doc "rolls back reservation, status, and boundary on startup error"
                 (let* ((tmpdir (file-name-as-directory
                                 (make-temp-file "mevedel-directive-startup-" t)))
                        (file (file-name-concat tmpdir "sample.txt"))
                        (buf (find-file-noselect file))
                        captured-chat)
                   (unwind-protect
                       (with-current-buffer buf
                         (insert "source\n")
                         (write-region (point-min) (point-max) file nil 'silent)
                         (set-buffer-modified-p nil)
                         (let ((directive
                                (mevedel--create-directive-in
                                 buf (point-min) (1- (point-max)) nil
                                 "Change it")))
                           (overlay-put directive 'mevedel-directive-action
                                        'implement)
                           (cl-letf (((symbol-function 'save-some-buffers)
                                      (lambda (&rest _) nil))
                                     ((symbol-function 'display-buffer)
                                      (lambda (&rest _) nil))
                                     ((symbol-function 'gptel--apply-preset)
                                      (lambda (&rest _) nil))
                                     ((symbol-function 'gptel-request)
                                      (lambda (_prompt &rest args)
                                        (setq captured-chat
                                              (plist-get args :buffer))
                                        (error "Synchronous startup failure"))))
                             (should-error
                              (mevedel--process-directive
                               directive '(:system "test")
                               #'mevedel--implement-directive-prompt nil))
                             (should-not (mevedel--directive-status directive))
                             (with-current-buffer captured-chat
                               (should-not mevedel--current-request)
                               (should-not mevedel--current-directive-uuid)
                               (should-not mevedel--directive-read-only-request-p)
                               (should-not
                                (mevedel-transcript-audit-records
                                 (buffer-string)
                                 'directive-turn-boundary))))))
                     (when (buffer-live-p buf)
                       (kill-buffer buf))
                     (when (buffer-live-p captured-chat)
                       (let ((view-buf
                              (buffer-local-value 'mevedel--view-buffer
                                                  captured-chat)))
                         (when (buffer-live-p view-buf)
                           (kill-buffer view-buf)))
                       (kill-buffer captured-chat))
                     (delete-directory tmpdir t)))

                 :doc "preserves prior state and an unrelated active request on pre-reservation errors"
                 (let* ((workspace
                         (mevedel-workspace--create
                          :type 'test :id "pre-reservation" :root "/tmp"
                          :name "pre-reservation"))
                        (source (generate-new-buffer " *directive-source*"))
                        (chat (generate-new-buffer " *directive-chat*"))
                        (session (mevedel-session-create "main" workspace)))
                   (unwind-protect
                       (with-current-buffer source
                         (insert "source")
                         (setq-local mevedel--workspace workspace)
                         (let* ((directive
                                 (mevedel--create-directive-in
                                  source (point-min) (point-max) nil "Change it"))
                                (record (mevedel--directive-record directive)))
                           (overlay-put directive 'mevedel-directive-action
                                        'implement)
                           (setf (mevedel-directive-state record) 'implemented)
                           (with-current-buffer chat
                             (setq-local mevedel--session session))
                           (cl-letf (((symbol-function
                                      'mevedel--directive-session-buffer)
                                     (lambda (&rest _) (cons chat nil))))
                             (should-error
                              (mevedel--process-directive
                               directive '(:system "test")
                               (lambda (_) (error "Prompt construction failed"))
                               nil))
                             (should (eq 'implemented
                                         (mevedel-directive-state record)))
                             (let ((active
                                    (mevedel-request--create
                                     :session session :turn 9)))
                               (with-current-buffer chat
                                 (setq-local mevedel--current-request active
                                             mevedel--current-directive-uuid
                                             "other-directive"
                                             mevedel--directive-read-only-request-p
                                             t))
                               (should-error
                                (mevedel--process-directive
                                 directive '(:system "test") #'identity nil)
                                :type 'user-error)
                               (with-current-buffer chat
                                 (should (eq active mevedel--current-request))
                                 (should (equal "other-directive"
                                                mevedel--current-directive-uuid))
                                 (should mevedel--directive-read-only-request-p))))))
                     (kill-buffer source)
                     (kill-buffer chat)))

                 :doc "reuses the directive session even when another is current"
                 (let* ((workspace (mevedel-workspace--create
                                    :type 'test :id "bound" :root "/tmp"
                                    :name "bound"))
                        (bound-session (mevedel-session-create "bound" workspace))
                        (other-session (mevedel-session-create "other" workspace))
                        (bound-buffer (generate-new-buffer " *bound-session*"))
                        (other-buffer (generate-new-buffer " *other-session*"))
                        (record (mevedel-directive--create
                                 :id "directive" :request "Request"
                                 :anchor '(:state attached)
                                 :session-id "bound-id")))
                   (unwind-protect
                       (progn
                         (setf (mevedel-session-session-id bound-session)
                               "bound-id"
                               (mevedel-session-session-id other-session)
                               "other-id")
                         (with-current-buffer bound-buffer
                           (setq-local mevedel--session bound-session))
                         (with-current-buffer other-buffer
                           (setq-local mevedel--session other-session))
                         (cl-letf (((symbol-function 'mevedel--workspace-sessions)
                                    (lambda (_workspace)
                                      (list (cons "other" other-buffer)
                                            (cons "bound" bound-buffer)))))
                           (should
                            (equal (cons bound-buffer nil)
                                   (mevedel--directive-session-buffer
                                    record workspace)))))
                     (kill-buffer bound-buffer)
                     (kill-buffer other-buffer))))

(mevedel-deftest mevedel--process-directive-detached-callback
  (:before-each (mevedel-workspace-clear-registry)
                :after-each (mevedel-workspace-clear-registry))
  ,test
  (test)

  :doc "detached source still settles a successful inspectable attempt"
  (let* ((tmpdir (file-name-as-directory
                  (make-temp-file "mevedel-directive-detached-" t)))
         (file (file-name-concat tmpdir "sample.txt"))
         (buf (find-file-noselect file))
         captured-fsm captured-chat callback-result)
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
                       (lambda (_prompt &rest args)
                         (setq captured-chat (plist-get args :buffer))
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
                 (setq callback-result (list err (eq fsm captured-fsm)))))
              (delete-region (overlay-start directive)
                             (overlay-end directive))
              (let ((detached
                     (mevedel--find-directive-by-uuid
                      (mevedel-directive-id
                       (car (mevedel-workspace-directives
                             (with-current-buffer buf
                               (mevedel-workspace))))))))
                (should (overlay-buffer detached))
                (should (= (overlay-start detached) (overlay-end detached))))
              (funcall (plist-get (gptel-fsm-info captured-fsm)
                                  :mevedel-request-callback)
                       nil captured-fsm)
              (should (equal '(nil t) callback-result))
              (let* ((record (car (mevedel-workspace-directives
                                   (with-current-buffer buf
                                     (mevedel-workspace)))))
                     (attempt (car (mevedel-directive-attempts record))))
                (should (eq 'implemented (mevedel-directive-state record)))
                (should (eq 'success
                            (mevedel-directive-attempt-outcome attempt))))
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

		 :doc "flushes permission queues and the pending plan approval"
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
		     (setf (mevedel-session-pending-plan-approval session)
			   (list :body "# Plan"
				       :chat-buffer (current-buffer)
				       :session session
				       :callback
				       (lambda (outcome)
					 (push (cons 'plan outcome) outcomes))))
		     (mevedel-abort (current-buffer))
		     (should (null (mevedel-session-permission-queue session)))
		     (should (null (mevedel-session-pending-plan-approval session)))
		     (should (null mevedel--current-request))
		     (should (= 1 (length
				   (mevedel-session-pending-reminders session))))
		     (should (equal '((plan . aborted) (permission . aborted))
				    outcomes))))

			 :doc "saves data buffer after abort teardown"
                         (with-temp-buffer
                           (let* ((workspace (mevedel-workspace--create
                                              :type 'project
                                              :id "/tmp/mevedel-chat-abort-save/"
                                              :root "/tmp/mevedel-chat-abort-save/"
                                              :name "abort-save"))
                                  (session (mevedel-session-create "main" workspace))
                                  (goal (mevedel-goal--create
                                         :id "g1" :objective "Ship"
                                         :status 'active :tokens-used 0
                                         :time-used-seconds 0 :turns-run 0))
                                  saved)
                             (setf (mevedel-session-goal session) goal)
                             (setq-local mevedel--session session)
                             (mevedel-request-begin session)
                             (cl-letf (((symbol-function
                                         'mevedel-session-persistence-save)
                                        (lambda (s b)
                                          (setq saved
                                                (list s b mevedel--current-request))
                                          "saved")))
                               (mevedel-abort (current-buffer)))
                             (should (equal (list session (current-buffer) nil)
                                            saved))
                             (should (null mevedel--current-request))
                             (should (eq 'paused (mevedel-goal-status goal)))
                             (should (equal "interrupted by user"
                                            (mevedel-goal-reason goal)))))

			 :doc "does not rewrite a Goal already paused for another reason"
			 (with-temp-buffer
			   (let* ((workspace (mevedel-workspace--create
			                      :type 'project :id "/tmp/abort-paused/"
			                      :root "/tmp/abort-paused/" :name "paused"))
			          (goal (mevedel-goal--create
			                 :id "g1" :objective "Ship" :status 'paused
			                 :reason "Waiting for review"))
			          (session (mevedel-session-create "main" workspace)))
			     (setf (mevedel-session-goal session) goal)
			     (setq-local mevedel--session session)
			     (mevedel-request-begin session)
			     (cl-letf (((symbol-function 'mevedel-session-persistence-save)
			                #'ignore))
			       (mevedel-abort (current-buffer)))
			     (should (eq 'paused (mevedel-goal-status goal)))
			     (should (equal "Waiting for review"
			                    (mevedel-goal-reason goal)))))

				 :doc "root abort leaves retained agent turns independent"
			 (with-temp-buffer
			   (let* ((workspace (mevedel-workspace--create
					      :type 'project
					      :id "/tmp/mevedel-chat-abort-agents/"
					      :root "/tmp/mevedel-chat-abort-agents/"
					      :name "abort-agents"))
				  (session (mevedel-session-create "main" workspace))
				  (invocation (mevedel-agent-invocation--create
					       :path "/root/explorer"
					       :description "retained"
					       :transcript-status 'running))
				  (record (mevedel-agent-record--create
					   :path "/root/explorer"
					   :activity 'running
					   :invocation invocation))
				  interrupted
				  (gptel--request-alist nil))
			     (setq-local mevedel--session session)
			     (setf (mevedel-session-agent-registry session)
				   (list (cons "/root/explorer" record)))
			     (cl-letf (((symbol-function 'mevedel-agent-runtime-interrupt)
					(lambda (&rest _) (setq interrupted t))))
			       (mevedel-abort (current-buffer)))
			     (should-not interrupted)
			     (should-not
			      (mevedel-session-pending-reminders session))
			     (should (eq 'running
					 (mevedel-agent-record-activity record))))))


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
                              :permission-mode 'ask
                              :permission-rules nil
                              :permission-queue nil
                              :pending-plan-approval nil))
                    (buffer (generate-new-buffer " *mev-chat-mode*"))
                    (refreshed 0))
               (unwind-protect
                   (cl-letf (((symbol-function 'mevedel-skills--refresh-view-input-prompt)
                              (lambda () (cl-incf refreshed))))
                     (with-current-buffer buffer
                       (setq-local mevedel--session session)
                       (mevedel--implementation-permission-mode-apply 'edits)
                       (should (eq 'edits
                                   (mevedel-session-permission-mode session)))
                       (should (equal '(ask)
                                      mevedel--implementation-permission-mode-restore))
                       (mevedel--implementation-permission-mode-restore)
                       (should (eq 'ask
                                   (mevedel-session-permission-mode session)))
                       (should-not mevedel--implementation-permission-mode-restore)
                       (should (= 2 refreshed))))
                 (when (buffer-live-p buffer) (kill-buffer buffer))))

             :doc "temporarily applies explicit ask mode over restored full-auto mode"
             (let* ((session (mevedel-session--create
                              :name "test"
                              :workspace nil
                              :permission-mode 'full-auto
                              :permission-rules nil
                              :permission-queue nil
                              :pending-plan-approval nil))
                    (buffer (generate-new-buffer " *mev-chat-mode*"))
                    (refreshed 0))
               (unwind-protect
                   (cl-letf (((symbol-function 'mevedel-skills--refresh-view-input-prompt)
                              (lambda () (cl-incf refreshed))))
                     (with-current-buffer buffer
                       (setq-local mevedel--session session)
                       (mevedel--implementation-permission-mode-apply 'ask)
                       (should (eq 'ask
                                   (mevedel-session-permission-mode session)))
                       (should (equal '(full-auto)
                                      mevedel--implementation-permission-mode-restore))
                       (mevedel--implementation-permission-mode-restore)
                       (should (eq 'full-auto
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
                              :pending-plan-approval nil))
                    (buffer (generate-new-buffer " *mev-chat-mode*"))
                    (mevedel-permission-mode 'ask)
                    (refreshed 0))
               (unwind-protect
                   (cl-letf (((symbol-function 'mevedel-skills--refresh-view-input-prompt)
                              (lambda () (cl-incf refreshed))))
                     (with-current-buffer buffer
                       (setq-local mevedel--session session)
                       (setq-local mevedel-permission-mode nil)
                       (mevedel--implementation-permission-mode-apply 'full-auto)
                       (should (eq 'full-auto
                                   (mevedel-session-permission-mode session)))
                       (should (equal '(nil)
                                      mevedel--implementation-permission-mode-restore))
                       (mevedel--implementation-permission-mode-restore)
                       (should-not (mevedel-session-permission-mode session))
                       (should-not (local-variable-p 'mevedel-permission-mode
                                                     buffer))
                       (should (eq 'ask mevedel-permission-mode))
                       (should-not mevedel--implementation-permission-mode-restore)
                       (should (= 2 refreshed))))
                 (when (buffer-live-p buffer) (kill-buffer buffer)))))

(mevedel-deftest mevedel--directive-session-buffer ()
  ,test
  (test)
  :doc "resumes the bound persisted session on demand"
  (let* ((workspace (mevedel-workspace--create
                     :type 'test :id "resume" :root "/tmp" :name "resume"))
         (record (mevedel-directive--create
                  :id "directive" :request "Request"
                  :anchor '(:state attached) :session-id "saved-id"))
         (restored (generate-new-buffer " *restored-directive-session*")))
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel--workspace-sessions)
                   (lambda (_workspace) nil))
                  ((symbol-function 'mevedel-session-persistence-resume-id)
                   (lambda (seen-workspace seen-id)
                     (should (eq workspace seen-workspace))
                     (should (equal "saved-id" seen-id))
                     restored)))
          (should (equal (cons restored nil)
                         (mevedel--directive-session-buffer
                          record workspace))))
      (kill-buffer restored)))

  :doc "requires explicit rebind and leaves historical links unchanged"
  (let* ((workspace (mevedel-workspace--create
                     :type 'test :id "rebind" :root "/tmp" :name "rebind"))
         (attempt
          (mevedel-directive-attempt--create
           :checkpoint '(:session-id "lost-id" :turn 3)))
         (record (mevedel-directive--create
                  :id "directive" :request "Request"
                  :anchor '(:state attached) :session-id "lost-id"
                  :attempts (list attempt)))
         (replacement (generate-new-buffer " *replacement-session*")))
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel--workspace-sessions)
                   (lambda (_workspace) nil))
                  ((symbol-function 'mevedel-session-persistence-resume-id)
                   (lambda (&rest _) nil))
                  ((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                  ((symbol-function 'mevedel--chat-buffer)
                   (lambda (&rest _) replacement)))
          (should (equal (cons replacement t)
                         (mevedel--directive-session-buffer record workspace)))
          (should (equal "lost-id" (mevedel-directive-session-id record)))
          (should
           (equal '(:session-id "lost-id" :turn 3)
                  (mevedel-directive-attempt-checkpoint attempt))))
      (kill-buffer replacement)))

  :doc "keeps an unavailable binding when rebind is declined"
  (let* ((workspace (mevedel-workspace--create
                     :type 'test :id "decline" :root "/tmp" :name "decline"))
         (record (mevedel-directive--create
                  :id "directive" :request "Request"
                  :anchor '(:state attached) :session-id "lost-id")))
    (cl-letf (((symbol-function 'mevedel--workspace-sessions)
               (lambda (_workspace) nil))
              ((symbol-function 'mevedel-session-persistence-resume-id)
               (lambda (&rest _) nil))
              ((symbol-function 'yes-or-no-p) (lambda (&rest _) nil)))
      (should-error
       (mevedel--directive-session-buffer record workspace)
       :type 'user-error))))

(mevedel-deftest mevedel--directive-discussion-transcript
  (:doc "renders only current-request local messages and replies in order")
  (let ((record
         (mevedel-directive--create
          :id "directive" :request "Request" :anchor '(:state attached)
          :discussion
          (list
           (mevedel-directive-discussion-turn--create
            :directive-request "Older request"
            :message "Stale" :request "Hidden stale request"
            :result "Stale answer" :outcome 'success)
           (mevedel-directive-discussion-turn--create
            :directive-request "Request"
            :message "One" :request "Hidden request one"
            :result (concat
                     "Answer one\n\n"
                     "<!-- mevedel-render-data -->\n"
                     "(:kind request-summary :elapsed-seconds 1.0)\n"
                     "<!-- /mevedel-render-data -->\n")
            :outcome 'success)
           (mevedel-directive-discussion-turn--create
            :directive-request "Request"
            :message "Two" :request "Hidden request two"
            :result "Transport failed" :outcome 'error)))))
    (should
     (equal "User: One\nAssistant: Answer one\n\nUser: Two\nAssistant (error): Transport failed"
            (mevedel--directive-discussion-transcript record)))))

(mevedel-deftest mevedel--discuss-directive-prompt
  (:doc "includes fresh context, complete local discussion, and selected result")
  (let* ((attempt
          (mevedel-directive-attempt--create
           :request "Implement exact" :result "Implementation answer"
           :outcome 'success :patch "diff --git a/a b/a\n"
           :capture 'complete :covered-files '("/tmp/a") :gaps nil
           :checkpoint '(:session-id "session-1" :turn 1)))
         (record
          (mevedel-directive--create
           :id "directive" :request "Request" :anchor '(:state attached)
           :attempts (list attempt)
           :discussion
           (list
            (mevedel-directive-discussion-turn--create
             :directive-request "Request"
             :message "First question" :request "Old exact request"
             :result "First answer" :outcome 'success
             :checkpoint '(:session-id "session-1" :turn 2)))))
         (prompt
          (mevedel--discuss-directive-prompt
           "Fresh directive and references" record "Follow up" 1)))
    (should (string-match-p "Fresh directive and references" prompt))
    (should (string-match-p "First question" prompt))
    (should (string-match-p "First answer" prompt))
    (should (string-match-p "Follow up" prompt))
    (should (string-match-p "Implementation answer" prompt))
    (should (string-match-p "diff --git" prompt))
    (should-not (string-match-p "Old exact request" prompt))))

(mevedel-deftest mevedel--implement-discussion-prompt
  (:doc "turns the complete local discussion into implementation feedback")
  (let* ((record
          (mevedel-directive--create
           :id "directive" :request "Request" :anchor '(:state attached)
           :state 'discussed
           :discussion
           (list
            (mevedel-directive-discussion-turn--create
             :directive-request "Request"
             :message "Prefer a small API" :request "Exact"
             :result "Use one entry point" :outcome 'success
             :checkpoint '(:session-id "session-1" :turn 1)))))
         (prompt
          (mevedel--implement-discussion-prompt "Fresh references" record)))
    (should (string-match-p "IMPLEMENTATION REQUEST" prompt))
    (should (string-match-p "Fresh references" prompt))
    (should (string-match-p "Prefer a small API" prompt))
    (should (string-match-p "Use one entry point" prompt))
    (setf (mevedel-directive-attempts record)
          (list (mevedel-directive-attempt--create
                 :directive-request "Request" :outcome 'success)))
    (should-error
     (mevedel--implement-discussion-prompt "Fresh references" record)
     :type 'user-error)))

(mevedel-deftest mevedel--directive-implementation-prompt
  (:doc "previews the complete prompt for the directive's next implementation")
  (let* ((attempt
          (mevedel-directive-attempt--create
           :directive-request "Request" :request "Exact"
           :result "Previous answer" :outcome 'success
           :patch "historical patch" :capture 'complete
           :captured-at "2026-08-02T01:00:00+0200"))
         (directive
          (mevedel-directive--create
           :id "directive" :request "Request" :anchor '(:state attached)
           :state 'implemented :attempts (list attempt)))
         (prompt
          (mevedel--directive-implementation-prompt
           "Fresh references" directive "Change the parser")))
    (dolist (text '("Fresh references" "Change the parser"
                    "Previous answer" "historical patch"))
      (should (string-search text prompt)))
    (setf (mevedel-directive-state directive) 'failed
          (mevedel-directive-attempt-outcome attempt) 'error
          (mevedel-directive-attempt-result attempt) "Transport failed")
    (setq prompt
          (mevedel--directive-implementation-prompt
           "Fresh references" directive "Try locally"))
    (should (string-search "Transport failed" prompt))
    (should (string-search "Try locally" prompt))
    (setf (mevedel-directive-request directive) "Edited request"
          (mevedel-directive-discussion directive)
          (list
           (mevedel-directive-discussion-turn--create
            :directive-request "Edited request"
            :message "Prefer the smaller API" :request "Exact discussion"
            :result "Agreed" :outcome 'success))
          (mevedel-directive-state directive) 'discussed)
    (setq prompt
          (mevedel--directive-implementation-prompt
           "Fresh references" directive))
    (should (string-search "DISCUSSION FEEDBACK" prompt))
    (should (string-search "Prefer the smaller API" prompt))))

(mevedel-deftest mevedel--implement-discussion
  (:vars
   ((mevedel--instruction-states (make-hash-table :test #'equal))
    (mevedel--instruction-current-state-key :global))
   :doc "dispatches ordinary implementation with complete discussion feedback")
  (let ((workspace (mevedel-workspace--create
                    :type 'test :id "implement-discussion" :root "/tmp"
                    :name "implement-discussion"))
        (mevedel-action-preset-alist
         '((implement . (:system "test"))))
        captured)
    (with-temp-buffer
      (insert "source")
      (setq-local mevedel--workspace workspace)
      (let* ((directive
              (mevedel--create-directive-in
               (current-buffer) (point-min) (point-max) nil "Request"))
             (record (mevedel--directive-record directive)))
        (setf (mevedel-directive-discussion record)
              (list
               (mevedel-directive-discussion-turn--create
                :directive-request "Request"
                :message "Keep it small" :request "Exact"
                :result "Agreed" :outcome 'success))
              (mevedel-directive-state record) 'discussed)
        (cl-letf (((symbol-function 'mevedel--process-directive)
                   (lambda (seen preset prompt-fn callback &optional options)
                     (setq captured
                           (list seen preset (funcall prompt-fn "Fresh")
                                 callback options))
                     'accepted)))
          (should (eq 'accepted
                      (mevedel--implement-discussion directive #'ignore)))
          (should (eq directive (nth 0 captured)))
          (should (equal '(:system "test") (nth 1 captured)))
          (should (string-match-p "Fresh" (nth 2 captured)))
          (should (string-match-p "Keep it small" (nth 2 captured)))
          (should (eq #'ignore (nth 3 captured)))
          (should-not (nth 4 captured)))))))

(mevedel-deftest mevedel--request-directive-changes
  (:vars
   ((mevedel--instruction-states (make-hash-table :test #'equal))
    (mevedel--instruction-current-state-key :global)))
  ,test
  (test)
  :doc "uses ordinary implementation authority and accepts child-only feedback"
  (let ((workspace (mevedel-workspace--create
                    :type 'test :id "changes" :root "/tmp" :name "changes"))
        (mevedel-action-preset-alist '((implement . implement-preset)))
        captured)
    (with-temp-buffer
      (insert "source")
      (setq-local mevedel--workspace workspace)
      (let* ((directive
              (mevedel--create-directive-in
               (current-buffer) (point-min) (point-max) nil "Request"))
             (_child
              (mevedel--create-directive-in
               (current-buffer) (1+ (point-min)) (1- (point-max))
               nil "Child change"))
             (record (mevedel--directive-record directive)))
        (setf (mevedel-directive-state record) 'implemented
              (mevedel-directive-attempts record)
              (list
               (mevedel-directive-attempt--create
                :directive-request "Request" :request "Exact"
                :result "Done" :outcome 'success :patch "old patch"
                :capture 'complete
                :captured-at "2026-08-02T01:00:00+0200")))
        (cl-letf (((symbol-function 'mevedel--process-directive)
                   (lambda (seen preset prompt-fn callback &optional options)
                     (setq captured
                           (list seen preset
                                 (funcall prompt-fn "Fresh\nChild change")
                                 callback options
                                 (overlay-get seen 'mevedel-directive-action)))
                     'accepted)))
          (should
           (eq 'accepted
               (mevedel--request-directive-changes directive "" #'ignore)))
          (should (eq directive (nth 0 captured)))
          (should (eq 'implement-preset (nth 1 captured)))
          (should (string-search "Child change" (nth 2 captured)))
          (should (eq #'ignore (nth 3 captured)))
          (should-not (nth 4 captured))
          (should (eq 'request-changes (nth 5 captured)))))))

  :doc "rejects Request changes without feedback or new subdirectives"
  (let ((record
         (mevedel-directive--create
          :id "directive" :request "Request" :anchor '(:state attached)
          :state 'implemented
          :attempts
          (list
           (mevedel-directive-attempt--create
            :directive-request "Request" :outcome 'success)))))
    (with-temp-buffer
      (insert "source")
      (let ((directive (make-overlay (point-min) (point-max))))
        (cl-letf (((symbol-function 'mevedel--directive-record)
                   (lambda (_) record))
                  ((symbol-function 'mevedel--topmost-instruction)
                   (lambda (seen _) seen)))
          (should-error
           (mevedel--request-directive-changes directive "" nil)
           :type 'user-error))))))

(mevedel-deftest mevedel--retry-directive
  (:doc "accepts empty guidance and uses ordinary implementation authority")
  (let ((record
         (mevedel-directive--create
          :id "directive" :request "Request" :anchor '(:state attached)
          :state 'failed
          :attempts
          (list
           (mevedel-directive-attempt--create
            :directive-request "Request" :request "Exact"
            :result "Failure" :outcome 'error :patch "partial"
            :capture 'incomplete :captured-at "2026-08-02T01:00:00+0200"))))
        (mevedel-action-preset-alist '((implement . implement-preset)))
        captured)
    (with-temp-buffer
      (insert "source")
      (let ((directive (make-overlay (point-min) (point-max))))
        (cl-letf (((symbol-function 'mevedel--directive-record)
                   (lambda (_) record))
                  ((symbol-function 'mevedel--topmost-instruction)
                   (lambda (seen _) seen))
                  ((symbol-function 'mevedel--process-directive)
                   (lambda (seen preset prompt-fn callback &optional options)
                     (setq captured
                           (list seen preset (funcall prompt-fn "Fresh")
                                 callback options
                                 (overlay-get seen 'mevedel-directive-action)))
                     'accepted)))
          (should (eq 'accepted (mevedel--retry-directive directive "" nil)))
          (should (eq 'implement-preset (nth 1 captured)))
          (should (string-search "Failure" (nth 2 captured)))
          (should (eq 'retry (nth 5 captured))))))))

(mevedel-deftest mevedel--generate-final-patch ()
  ,test
  (test)
  :doc "uses the active request snapshots in deterministic path order"
  (let* ((root (make-temp-file "mevedel-final-patch-" t))
         (workspace (mevedel-workspace--create
                     :type 'project :id root :root root :name "patch"))
         (a-file (file-name-concat root "a.txt"))
         (directory (file-name-concat root "created-directory"))
         (z-file (file-name-concat root "z.txt"))
         (snapshots (make-hash-table :test #'equal))
         (mevedel--current-request
          (mevedel-request--create :file-snapshots snapshots)))
    (unwind-protect
        (progn
          (puthash z-file nil snapshots)
          (puthash a-file "old\n" snapshots)
          (puthash directory nil snapshots)
          (make-directory directory)
          (with-temp-file a-file (insert "new\n"))
          (with-temp-file z-file (insert "created\n"))
          (let* ((patch (mevedel--generate-final-patch workspace))
                 (a-pos (string-search "diff --git a/a.txt" patch))
                 (z-pos (string-search "diff --git a/z.txt" patch)))
            (should a-pos)
            (should z-pos)
            (should (< a-pos z-pos))
            (should (string-match-p "new file mode 100644" patch))))
      (delete-directory root t))))

(mevedel-deftest mevedel--directive-capture ()
  ,test
  (test)
  :doc "reports deterministic complete and incomplete request coverage"
  (let ((snapshots (make-hash-table :test #'equal)))
    (puthash "/tmp/z" "old" snapshots)
    (puthash "/tmp/a" nil snapshots)
    (should
     (equal '(:capture complete
              :covered-files ("/tmp/a" "/tmp/z")
              :gaps nil
              :untracked-effects nil)
            (mevedel--directive-capture
             (mevedel-request--create :file-snapshots snapshots))))
    (puthash "/tmp/missing" '(:gap not-observed) snapshots)
    (should
     (equal '(:capture incomplete
              :covered-files ("/tmp/a" "/tmp/z")
              :gaps (("/tmp/missing" . not-observed))
              :untracked-effects nil)
            (mevedel--directive-capture
             (mevedel-request--create :file-snapshots snapshots))))
    (should
     (equal '(:capture incomplete
              :covered-files ("/tmp/a" "/tmp/z")
              :gaps (("/tmp/missing" . not-observed))
              :untracked-effects (("Bash" . "untracked")))
            (mevedel--directive-capture
             (mevedel-request--create
              :file-snapshots snapshots
              :untracked-effects '(("Bash" . "untracked"))))))))


(provide 'test-mevedel-chat)
;;; test-mevedel-chat.el ends here
