;;; test-niwa-chat.el --- Tests for chat buffer management -*- lexical-binding: t -*-

;;; Commentary:

;; Focused coverage for chat-buffer lifecycle, sessions, and request settlement.

;;; Code:

(require 'niwa-chat)
(require 'niwa-collaboration-artifact-room)
(require 'niwa-collaboration-owner)
(require 'niwa-agent-control)
(require 'niwa)
(require 'niwa-permission-persistence)
(require 'niwa-plugin-ui)
(require 'niwa-permission-queue)
(require 'niwa-goal)
(require 'niwa-prompt-submission)
(require 'niwa-schedule)
(require 'niwa-skills-core)
(require 'niwa-tool-render-data)
(require 'niwa-view)
(require 'niwa-view-zone)
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
(defvar gptel-display-buffer-action)
(defvar gptel-model)
(defvar gptel-reasoning-effort)

;; `gptel-org'
(defvar gptel-confirm-tool-calls)
(defvar gptel-org-branching-context)
(defvar gptel-org-ignore-elements)

;; `niwa-view'
(declare-function niwa-view-render-live-update "niwa-view-render"
                  (data-buffer))
(defvar niwa-view--data-turn-start)
(defvar niwa-view--in-flight-turn-start)

;; `org'
(defvar org-mode-hook)

;; `org-agenda'
(defvar org-agenda-file-menu-enabled)

(niwa-deftest niwa ()
  ,test
  (test)
  :doc "starts from Dired in a directory outside any project"
  (let* ((root (file-name-as-directory
                (make-temp-file "niwa-entry-directory-" t)))
         (niwa-workspace--registry (make-hash-table :test #'equal))
         started)
    (unwind-protect
        (with-temp-buffer
          (setq default-directory root)
          (dired-mode root)
          (should-not (project-current nil))
          (cl-letf (((symbol-function 'niwa--start-chat)
                     (lambda (workspace directory &rest _)
                       (setq started (list workspace directory)))))
            (niwa))
          (should (equal root (cadr started)))
          (should (equal root (niwa-workspace-root (car started))))
          (should (eq 'portable
                      (niwa-session-authority-mode-for-workspace
                       (car started)))))
      (delete-directory root t)))

  :doc "uses persisted entry choices for restore and explicit new sessions"
  (let* ((root (make-temp-file "niwa-entry-command-" t))
         (workspace
          (niwa-workspace--create
           :type 'project :id root :root root :name "entry"))
         (restored (generate-new-buffer " *niwa-entry-command*"))
         (entry restored)
         displayed
         started)
    (unwind-protect
        (cl-letf
            (((symbol-function 'niwa-workspace)
              (lambda () workspace))
             ((symbol-function 'niwa-session-persistence-choose-entry)
              (lambda (&rest _) entry))
             ((symbol-function 'niwa--display-chat-buffer)
              (lambda (buffer) (setq displayed buffer)))
             ((symbol-function 'niwa--start-chat)
              (lambda (&rest arguments) (setq started arguments))))
          (niwa)
          (should (eq restored displayed))
          (should-not started)
          (setq entry 'new
                displayed nil)
          (niwa)
          (should (equal (list workspace root t nil) started))
          (should-not displayed))
      (when (buffer-live-p restored)
        (kill-buffer restored))
      (when (file-directory-p root)
        (delete-directory root t))))
  :doc "displays inspection buffers without live chat initialization"
  (let* ((root (make-temp-file "niwa-inspection-command-" t))
         (workspace
          (niwa-workspace--create
           :type 'project :id root :root root :name "inspection"))
         (inspection (generate-new-buffer " *niwa-inspection-command*"))
         displayed)
    (unwind-protect
        (cl-letf
            (((symbol-function 'niwa-workspace)
              (lambda () workspace))
             ((symbol-function 'niwa-session-persistence-choose-entry)
              (lambda (&rest _)
                (list :action 'inspect :buffer inspection)))
             ((symbol-function 'display-buffer)
              (lambda (buffer &optional action)
                (setq displayed (list buffer action))))
             ((symbol-function 'niwa--display-chat-buffer)
              (lambda (&rest _)
                (ert-fail "Inspection must not initialize a chat view")))
             ((symbol-function 'niwa--start-chat)
              (lambda (&rest _)
                (ert-fail "Inspection must not start a session"))))
          (niwa)
          (should
           (equal (list inspection gptel-display-buffer-action) displayed)))
      (when (buffer-live-p inspection)
        (kill-buffer inspection))
      (when (file-directory-p root)
        (delete-directory root t)))))

;; `org-element'
(defvar org-element-cache-persistent)
(defvar org-element-use-cache)

;; `org-indent'
(declare-function org-indent-mode "org-indent" (&optional arg))
(defvar org-indent-mode)

(defvar niwa-chat-test--hook-events nil)

(defun niwa-chat-test--record-hook (event)
  "Record lifecycle hook EVENT for chat cases."
  (push event niwa-chat-test--hook-events)
  nil)

(defun niwa-chat-test--session-context-hook (event)
  "Record lifecycle hook EVENT and return startup context."
  (push event niwa-chat-test--hook-events)
  '(:additional-context ("startup context")))


;;
;;; Org element cache handling

(niwa-deftest niwa-load-order ()
		 ,test
		 (test)

		 :doc "`niwa' loads preset definitions used by `niwa-install'"
		 (should (fboundp 'niwa--define-presets)))

(niwa-deftest niwa-install (:quiet t)
  ,test
  (test)
  :doc "makes the focus callback callable before the first view opens"
  (with-temp-buffer
    (let ((status
           (call-process
            (expand-file-name invocation-name invocation-directory)
            nil t nil "--batch" "-Q" "--eval"
            (prin1-to-string
             `(progn
                (setq load-path ',load-path)
                (require 'ert)
                (require 'niwa)
                (let ((after-focus-change-function #'ignore))
                  (cl-letf (((symbol-function 'message)
                             (lambda (format-string &rest args)
                               (should
                                (member
                                 (apply #'format format-string args)
                                 '("niwa installed successfully"
                                   "niwa uninstalled successfully"))))))
                    (dotimes (_ 2)
                      (unwind-protect
                          (progn
                            (niwa-install)
                            (should-not
                             (eq after-focus-change-function #'ignore))
                            (funcall after-focus-change-function))
                        (niwa-uninstall))
                      (should (eq after-focus-change-function #'ignore))
                      (funcall after-focus-change-function)))))))))
      (ert-info ((buffer-string))
        (should (equal 0 status))
        (should (equal "" (buffer-string))))))
  :doc "restores focus-driven view resumption after uninstall and reinstall"
  (let ((after-focus-change-function #'ignore)
        (gptel--known-tools (copy-tree gptel--known-tools))
        (gptel--known-presets (copy-tree gptel--known-presets))
        (gptel-prompt-transform-functions gptel-prompt-transform-functions)
        (niwa-execution-event-functions niwa-execution-event-functions)
        (niwa-execution-mailbox-delivery-function
         niwa-execution-mailbox-delivery-function)
        (resumed 0))
    (cl-letf (((symbol-function 'niwa-transport-install) #'ignore)
              ((symbol-function 'niwa-transport-uninstall) #'ignore)
              ((symbol-function 'niwa-execution-teardown-all) #'ignore)
              ((symbol-function 'niwa-tools-register) #'ignore)
              ((symbol-function 'niwa--define-presets) #'ignore)
              ((symbol-function 'niwa-tool-render-data-install-provider-adapter) #'ignore)
              ((symbol-function 'niwa-tool-render-data-uninstall-provider-adapter) #'ignore)
              ((symbol-function 'niwa-tool-repair-install-shape-adapter) #'ignore)
              ((symbol-function 'niwa-tool-repair-uninstall-shape-adapter) #'ignore)
              ((symbol-function 'niwa-worktree-install-slash-command) #'ignore)
              ((symbol-function 'niwa-worktree-uninstall-slash-command) #'ignore)
              ((symbol-function 'niwa-skills-install-slash-commands) #'ignore)
              ((symbol-function 'niwa-skills-uninstall-slash-commands) #'ignore)
              ((symbol-function 'niwa-skills-install-hot-reload) #'ignore)
              ((symbol-function 'niwa-skills-uninstall-hot-reload) #'ignore)
              ((symbol-function 'niwa-gptel-stream-bridge-install) #'ignore)
              ((symbol-function 'niwa-gptel-stream-bridge-uninstall) #'ignore)
              ((symbol-function 'niwa-view--resume-attended-views)
               (lambda (&rest _) (cl-incf resumed))))
      (niwa-install)
      (should (advice-member-p #'niwa--refresh-animation-on-face
                               'set-face-attribute))
      (funcall after-focus-change-function)
      (should (= 1 resumed))
      (niwa-uninstall)
      (should-not (advice-member-p #'niwa--refresh-animation-on-face
                                   'set-face-attribute))
      (funcall after-focus-change-function)
      (should (= 1 resumed))
      (niwa-install)
      (should (advice-member-p #'niwa--refresh-animation-on-face
                               'set-face-attribute))
      (funcall after-focus-change-function)
      (should (= 2 resumed))
      (niwa-uninstall)))
  :doc "keeps the machine awake for live commands until uninstall"
  (let ((after-focus-change-function #'ignore)
        (gptel--known-tools (copy-tree gptel--known-tools))
        (gptel--known-presets (copy-tree gptel--known-presets))
        (gptel-prompt-transform-functions gptel-prompt-transform-functions)
        (niwa-execution-event-functions niwa-execution-event-functions)
        (niwa-execution-mailbox-delivery-function
         niwa-execution-mailbox-delivery-function)
        (niwa-execution-state-change-hook nil)
        (stopped 0))
    (require 'niwa-sleep)
    (cl-letf (((symbol-function 'niwa-transport-install) #'ignore)
              ((symbol-function 'niwa-transport-uninstall) #'ignore)
              ((symbol-function 'niwa-execution-teardown-all) #'ignore)
              ((symbol-function 'niwa-tools-register) #'ignore)
              ((symbol-function 'niwa--define-presets) #'ignore)
              ((symbol-function 'niwa-tool-render-data-install-provider-adapter) #'ignore)
              ((symbol-function 'niwa-tool-render-data-uninstall-provider-adapter) #'ignore)
              ((symbol-function 'niwa-tool-repair-install-shape-adapter) #'ignore)
              ((symbol-function 'niwa-tool-repair-uninstall-shape-adapter) #'ignore)
              ((symbol-function 'niwa-worktree-install-slash-command) #'ignore)
              ((symbol-function 'niwa-worktree-uninstall-slash-command) #'ignore)
              ((symbol-function 'niwa-skills-install-slash-commands) #'ignore)
              ((symbol-function 'niwa-skills-uninstall-slash-commands) #'ignore)
              ((symbol-function 'niwa-skills-install-hot-reload) #'ignore)
              ((symbol-function 'niwa-skills-uninstall-hot-reload) #'ignore)
              ((symbol-function 'niwa-gptel-stream-bridge-install) #'ignore)
              ((symbol-function 'niwa-gptel-stream-bridge-uninstall) #'ignore)
              ((symbol-function 'niwa-collaboration-lobby-restore) #'ignore)
              ((symbol-function 'niwa-collaboration-artifact-room-restore) #'ignore)
              ((symbol-function 'niwa-schedule-restore) #'ignore)
              ((symbol-function 'niwa-sleep-stop)
               (lambda () (cl-incf stopped))))
      (niwa-install)
      (should (memq #'niwa-sleep-update
                    niwa-execution-state-change-hook))
      (niwa-uninstall)
      (should-not (memq #'niwa-sleep-update
                        niwa-execution-state-change-hook))
      (should (= 1 stopped))))
  :doc "restarts recorded lobbies and shared artifacts now, or once startup is done; wake-ups once idle"
  (let ((after-focus-change-function #'ignore)
        (gptel--known-tools (copy-tree gptel--known-tools))
        (gptel--known-presets (copy-tree gptel--known-presets))
        (gptel-prompt-transform-functions gptel-prompt-transform-functions)
        (niwa-execution-event-functions niwa-execution-event-functions)
        (niwa-execution-mailbox-delivery-function
         niwa-execution-mailbox-delivery-function)
        (emacs-startup-hook nil)
        (restored 0)
        (shared 0)
        (woken 0))
    (cl-letf (((symbol-function 'niwa-transport-install) #'ignore)
              ((symbol-function 'niwa-transport-uninstall) #'ignore)
              ((symbol-function 'niwa-execution-teardown-all) #'ignore)
              ((symbol-function 'niwa-tools-register) #'ignore)
              ((symbol-function 'niwa--define-presets) #'ignore)
              ((symbol-function 'niwa-tool-render-data-install-provider-adapter) #'ignore)
              ((symbol-function 'niwa-tool-render-data-uninstall-provider-adapter) #'ignore)
              ((symbol-function 'niwa-tool-repair-install-shape-adapter) #'ignore)
              ((symbol-function 'niwa-tool-repair-uninstall-shape-adapter) #'ignore)
              ((symbol-function 'niwa-worktree-install-slash-command) #'ignore)
              ((symbol-function 'niwa-worktree-uninstall-slash-command) #'ignore)
              ((symbol-function 'niwa-skills-install-slash-commands) #'ignore)
              ((symbol-function 'niwa-skills-uninstall-slash-commands) #'ignore)
              ((symbol-function 'niwa-skills-install-hot-reload) #'ignore)
              ((symbol-function 'niwa-skills-uninstall-hot-reload) #'ignore)
              ((symbol-function 'niwa-gptel-stream-bridge-install) #'ignore)
              ((symbol-function 'niwa-gptel-stream-bridge-uninstall) #'ignore)
              ((symbol-function 'niwa-collaboration-lobby-restore)
               (lambda () (cl-incf restored)))
              ((symbol-function 'niwa-collaboration-artifact-room-restore)
               (lambda () (cl-incf shared)))
              ((symbol-function 'niwa-schedule-restore)
               (lambda () (cl-incf woken))))
      (cl-flet ((restore-timer ()
                  (cl-find #'niwa-schedule-restore timer-idle-list :key #'timer--function)))
        ;; Installed after startup: the user's configuration is in place.
        ;; Wake-up restore reads every indexed session, so it waits for idle.
        (let ((after-init-time (current-time)))
          (niwa-install)
          (should (= 1 restored))
          (should (= 1 shared))
          (should (= 0 woken))
          (let ((timer (restore-timer)))
            (should timer)
            (cancel-timer timer)
            (funcall (timer--function timer)))
          (should (= 1 woken))
          (should-not emacs-startup-hook)
          (niwa-install)
          (niwa-uninstall)
          (should-not (restore-timer)))
        ;; Installed from the init file: the relay settings may follow it.
        (let ((after-init-time nil))
          (setq restored 0 shared 0)
          (niwa-install)
          (should (= 0 restored))
          (should (= 0 shared))
          (should (equal '(niwa-collaboration-lobby-restore
                           niwa-collaboration-artifact-room-restore
                           niwa--restore-schedules-when-idle)
                         emacs-startup-hook))
          (niwa-uninstall)
          (should-not emacs-startup-hook)
          (should-not (restore-timer)))))))

(niwa-deftest niwa-uninstall ()
  (let ((transport-enabled niwa-transport--enabled-p))
    (unwind-protect
        ,test
      (if transport-enabled
          (niwa-transport-install)
        (niwa-transport-uninstall))))
  (test)

  :doc "tears down skill hot-reload lifecycle state"
  (let ((after-focus-change-function #'ignore)
        (gptel--known-tools (copy-tree gptel--known-tools))
        (gptel--known-presets (copy-tree gptel--known-presets))
        (gptel-prompt-transform-functions gptel-prompt-transform-functions)
        called)
    (cl-letf (((symbol-function 'niwa-skills-uninstall-hot-reload)
               (lambda () (setq called t)))
              ((symbol-function 'niwa-skills-uninstall-slash-commands)
               #'ignore)
              ((symbol-function 'niwa-tool-render-data-uninstall-provider-adapter)
               #'ignore)
              ((symbol-function 'niwa-gptel-stream-bridge-uninstall)
               #'ignore))
      (niwa-test--with-captured-diagnostics nil
                                               (niwa-uninstall)))
    (should called))

  :doc "cancels shared periodic callbacks and their host timer"
  (let ((after-focus-change-function #'ignore)
        (gptel--known-tools (copy-tree gptel--known-tools))
        (gptel--known-presets (copy-tree gptel--known-presets))
        (gptel-prompt-transform-functions gptel-prompt-transform-functions)
        (niwa--coalesced-timers nil)
        (niwa--coalesced-timer nil)
        (niwa--gc-holds (make-hash-table :test #'eq))
        (niwa--gc-timer nil)
        (niwa--gc-restore nil)
        (niwa--gc-percentage-restore nil)
        (niwa--gc-floor-applied nil)
        (gc-cons-percentage 0.1)
        (gc-cons-threshold gc-cons-threshold)
        (noninteractive nil)
        owned host)
    (setq owned (niwa--run-periodic-timer 'global 1 #'ignore)
          host niwa--coalesced-timer)
    ;; GC maintenance remembers its timer, so it must stop as an owner.
    (niwa--gc-hold 'request #'always)
    (unwind-protect
        (cl-letf (((symbol-function 'niwa-skills-uninstall-hot-reload) #'ignore)
                  ((symbol-function 'niwa-skills-uninstall-slash-commands) #'ignore)
                  ((symbol-function 'niwa-tool-render-data-uninstall-provider-adapter)
                   #'ignore)
                  ((symbol-function 'niwa-gptel-stream-bridge-uninstall) #'ignore))
          (niwa-test--with-captured-diagnostics nil
                                                   (niwa-uninstall))
          (should-not (niwa--ui-timer-pending-p owned))
          (should-not (memq host timer-list))
          (should-not niwa--coalesced-timer)
          (should-not niwa--gc-timer)
          (should (= 0 (hash-table-count niwa--gc-holds))))
      (niwa--ui-timer-cancel owned)))

  :doc "force-tears down executions"
  (let ((after-focus-change-function #'ignore)
        (gptel--known-tools (copy-tree gptel--known-tools))
        (gptel--known-presets (copy-tree gptel--known-presets))
        (gptel-prompt-transform-functions gptel-prompt-transform-functions)
        torn-down)
    (cl-letf (((symbol-function 'niwa-execution-teardown-all)
               (lambda () (setq torn-down t)))
              ((symbol-function 'niwa-skills-uninstall-hot-reload) #'ignore)
              ((symbol-function 'niwa-skills-uninstall-slash-commands) #'ignore)
              ((symbol-function 'niwa-tool-render-data-uninstall-provider-adapter) #'ignore)
              ((symbol-function 'niwa-gptel-stream-bridge-uninstall) #'ignore))
      (niwa-test--with-captured-diagnostics nil
                                               (niwa-uninstall)))
    (should torn-down))

  :doc "cancels deferred transport work and removes its advice"
  (let ((after-focus-change-function #'ignore)
        (gptel--known-tools (copy-tree gptel--known-tools))
        (gptel--known-presets (copy-tree gptel--known-presets))
        (gptel-prompt-transform-functions
         gptel-prompt-transform-functions)
        (niwa-transport-retry-seconds 5)
        (runs 0)
        timer)
    (unwind-protect
        (progn
          (niwa-transport-install)
          (let ((niwa-transport--depth 1))
            (niwa-transport-run-when-idle
             'uninstall-test "/ssh:user@host:/srv/x" #'ignore))
          (setq timer
                (car (gethash 'uninstall-test
                              niwa-transport--pending)))
          (should (timerp timer))
          (cl-letf
              (((symbol-function
                 'niwa-skills-uninstall-hot-reload)
                #'ignore)
               ((symbol-function
                 'niwa-skills-uninstall-slash-commands)
                #'ignore)
               ((symbol-function
                 'niwa-tool-render-data-uninstall-provider-adapter)
                #'ignore)
               ((symbol-function
                 'niwa-gptel-stream-bridge-uninstall)
                #'ignore))
            (niwa-test--with-captured-diagnostics nil
                                                     (niwa-uninstall)))
          (should-not
           (gethash 'uninstall-test
                    niwa-transport--pending))
          (should-not (memq timer timer-list))
          (should-not
           (advice-member-p
            #'niwa-transport--handler-advice
            'tramp-file-name-handler))
          (should-not
           (memq #'niwa-transport--detach tramp-unload-hook)))
      (niwa-transport-run-when-idle
       'late-uninstall-test "/srv/x" (lambda () (cl-incf runs)))
      (should (= 0 runs))
      (should-not
       (gethash 'late-uninstall-test niwa-transport--pending))
      (niwa-transport-install)
      (niwa-transport-run-when-idle
       'late-uninstall-test "/srv/x" (lambda () (cl-incf runs)))
      (should (= 1 runs))
      (niwa-transport-cancel-pending)
      (niwa-transport-install))))


(niwa-deftest niwa--chat-buffer-disable-org-element-cache ()
		 ,test
		 (test)

		 :doc "disables Org's element cache buffer-locally"
		 (with-temp-buffer
		   (org-mode)
		   (setq-local org-element-use-cache t)
		   (setq-local org-element-cache-persistent t)
		   (niwa--chat-buffer-disable-org-element-cache)
		   (should-not org-element-use-cache)
		   (should-not org-element-cache-persistent))
		 :doc "disables configured transcript minor modes"
		 (with-temp-buffer
		   (org-mode)
		   (let ((niwa-transcript-disabled-minor-modes
			  '(org-indent-mode visual-line-mode)))
		     (org-indent-mode +1)
		     (visual-line-mode +1)
		     (niwa--chat-buffer-disable-org-element-cache)
		     (should-not org-indent-mode)
		     (should-not visual-line-mode)))
		 :doc "keeps gptel Org prompt preparation on the fast path"
		 (with-temp-buffer
		   (org-mode)
		   (setq-local gptel-org-ignore-elements
			       '(property-drawer src-block))
		   (niwa--chat-buffer-disable-org-element-cache)
		   (should (equal '(property-drawer)
				  gptel-org-ignore-elements))))

(niwa-deftest niwa--chat-buffer-setup
  (:before-each (require 'org-indent))
		 ,test
		 (test)

		 :doc "does not install Org agenda menus while entering org-mode"
		 (let* ((root (file-name-as-directory
			       (make-temp-file "niwa-chat-menu-" t)))
		  (workspace (niwa-workspace--create
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
				       'niwa--chat-buffer-init-common)
				      #'ignore))
			     (niwa--chat-buffer-setup
			      (current-buffer) workspace "main" root)))
				 (should (derived-mode-p 'org-mode))
				 (should-not gptel-org-convert-response)
				 (should (local-variable-p
					  'gptel-org-branching-context))
				 (should-not gptel-org-branching-context)
				 (should (equal '(property-drawer)
						gptel-org-ignore-elements))
				 (should-not org-indent-mode)
				 (should-not menu-called))
		     (delete-directory root t))))

(niwa-deftest niwa--chat-buffer
  (:before-each (niwa-workspace-clear-registry)
   :vars* ((root-dir (file-name-as-directory
                      (make-temp-file "niwa-chat-transaction-" t)))
           (workspace (niwa-workspace-get-or-create
                       'project root-dir root-dir "transaction"))
           (data-name "*niwa:main@transaction*")
           (view-name "*niwa:main@transaction:view*")
           (chat-buffer nil))
   :after-each
   (progn
     (dolist (name (list data-name view-name))
       (when-let* ((buffer (get-buffer name))
                   ((buffer-live-p buffer)))
         (with-current-buffer buffer
           (let ((kill-buffer-query-functions nil))
             (kill-buffer buffer)))))
     (niwa-workspace-clear-registry)
     (delete-directory root-dir t))
   :quiet t
   :doc "first-time setup failure leaves no published session and retry succeeds")
  (let ((session-create (symbol-function 'niwa-session-create))
        (init-common (symbol-function 'niwa--chat-buffer-init-common))
        (view-ensure (symbol-function 'niwa-view--ensure))
        (start-hooks (symbol-function 'niwa--run-session-start-hooks)))
    (dolist (scenario '(before-session after-session after-view session-start))
      (let* ((fail-once t)
             (session-end-calls 0)
             (cleanup-calls 0)
             (niwa-session-end-hook
              (and (eq scenario 'after-view)
                   (list (lambda ()
                           (cl-incf session-end-calls)
                           (error "Injected SessionEnd failure"))))))
        (cl-letf (((symbol-function 'niwa-session-create)
                   (lambda (&rest args)
                     (when (and fail-once (eq scenario 'before-session))
                       (setq fail-once nil)
                       (error "Injected pre-session failure"))
                     (apply session-create args)))
                  ((symbol-function 'niwa--chat-buffer-init-common)
                   (lambda (&rest args)
                     (when (and fail-once (eq scenario 'after-session))
                       (setq fail-once nil)
                       (should (buffer-local-value 'niwa--session
                                                   (car args)))
                       (error "Injected post-session failure"))
                     (apply init-common args)))
                  ((symbol-function 'niwa-view--ensure)
                   (lambda (&rest args)
                     (let ((view (apply view-ensure args)))
                       (when (and fail-once (eq scenario 'after-view))
                         (with-current-buffer (car args)
                           (add-hook 'kill-buffer-hook
                                     (lambda () (cl-incf cleanup-calls))
                                     nil t))
                         (setq fail-once nil)
                         (error "Injected post-view failure"))
                       view)))
                  ((symbol-function 'niwa--run-session-start-hooks)
                   (lambda (&rest args)
                     (when (and fail-once (eq scenario 'session-start))
                       (setq fail-once nil)
                       (error "Injected SessionStart failure"))
                     (apply start-hooks args))))
          (let ((setup-error
                 (should-error
                  (niwa--chat-buffer "main" t workspace root-dir))))
            (should (string-match-p
                     (pcase scenario
                       ('before-session "pre-session")
                       ('after-session "post-session")
                       ('after-view "post-view")
                       (_ "SessionStart"))
                     (error-message-string setup-error))))
          (when (eq scenario 'after-view)
            (should (= 1 session-end-calls))
            (should (= 1 cleanup-calls)))
          (should-not (get-buffer data-name))
          (should-not (get-buffer view-name))
          (should-not (niwa--workspace-sessions workspace))
          (setq niwa-session-end-hook nil)
          (setq chat-buffer
                (niwa--chat-buffer "main" t workspace root-dir))
          (should (buffer-live-p chat-buffer))
          (should (buffer-local-value 'niwa--session chat-buffer))
          (should (buffer-live-p
                   (buffer-local-value 'niwa--view-buffer chat-buffer)))
          (with-current-buffer chat-buffer
            (let ((kill-buffer-query-functions nil))
              (kill-buffer chat-buffer)))
          (setq chat-buffer nil))))))

(niwa-deftest niwa-chat-discard-buffers
  (:doc "both failure paths run remaining cleanup hooks with their modification policy")
  (dolist (guestp '(nil t))
    (let ((view (generate-new-buffer " *discard-view*"))
          data seen)
      (unwind-protect
          (cl-labels
              ((prepare (buffer)
                 (setq data buffer)
                 (with-current-buffer data
                   (setq-local niwa--view-buffer view))
                 (dolist (entry (list (cons 'data data) (cons 'view view)))
                   (let ((label (car entry)))
                     (with-current-buffer (cdr entry)
                       (insert "unfinished")
                       (setq-local
                        kill-buffer-query-functions
                        (list (lambda () (ert-fail "Cleanup queried")))
                        kill-buffer-hook
                        (list (lambda () (error "Broken cleanup hook"))
                              (lambda ()
                                (push (cons label (buffer-modified-p)) seen)))))))))
            (if guestp
                (progn
                  (prepare (generate-new-buffer " *discard-data*"))
                  (niwa-collaboration--discard-created-session nil data))
              (cl-letf (((symbol-function 'niwa--workspace-sessions) #'ignore)
                        ((symbol-function 'niwa--chat-buffer-setup)
                         (lambda (buffer &rest _)
                           (prepare buffer)
                           (error "Setup failed"))))
                (should-error (niwa--chat-buffer "failed" t 'workspace))))
            (should-not (buffer-live-p data))
            (should-not (buffer-live-p view))
            (should (equal (list (cons 'data (not guestp))
                                (cons 'view (not guestp)))
                           (nreverse seen))))
        (dolist (buffer (list data view))
          (when (buffer-live-p buffer)
            (with-current-buffer buffer
              (setq-local kill-buffer-hook nil kill-buffer-query-functions nil)
              (set-buffer-modified-p nil))
            (kill-buffer buffer)))))))

(niwa-deftest niwa-chat-discard-buffers/reentrant
  (:doc "finishes the data buffer's hooks when its view kills it again")
  ;; The view's kill hook kills the data buffer, as the real pair's does.
  ;; That nested kill must not end the data buffer before its later hooks,
  ;; such as the session lock release, run in it.
  (let ((data (generate-new-buffer " *discard-reentrant-data*"))
        (view (generate-new-buffer " *discard-reentrant-view*"))
        later)
    (unwind-protect
        (progn
          (with-current-buffer data
            (setq-local niwa--view-buffer view)
            (add-hook 'kill-buffer-hook
                      (lambda ()
                        (setq later (list (eq (current-buffer) data)
                                          (buffer-live-p data))))
                      nil t)
            ;; Added last, so it runs first: kill the companion view.
            (add-hook 'kill-buffer-hook
                      (lambda () (kill-buffer view))
                      nil t))
          (with-current-buffer view
            (add-hook 'kill-buffer-hook
                      (lambda () (when (buffer-live-p data) (kill-buffer data)))
                      nil t))
          (niwa-chat-discard-buffers data t)
          (should (equal '(t t) later))
          (should-not (buffer-live-p data))
          (should-not (buffer-live-p view)))
      (dolist (buffer (list data view))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (setq-local kill-buffer-hook nil))
          (kill-buffer buffer))))))

(niwa-deftest niwa--probe-session-target ()
                 ,test
                 (test)
                 :doc "probes remote sessions and forwards forced refresh"
                 (let* ((target (niwa-execution-target-create
                                 "/ssh:user@host:/srv/project/"))
                        (session (niwa-session--create
                                  :execution-target target
                                  :sandbox-mode 'required))
                        seen)
                   (cl-letf (((symbol-function
                               'niwa-execution-target-probe)
                              (lambda (actual refresh sandbox-mode)
                                (setq seen
                                      (list actual refresh sandbox-mode))
                                '(:status ready))))
                     (should (equal '(:status ready)
                                    (niwa--probe-session-target
                                     session t))))
                   (should (equal (list target t 'required) seen)))

                 :doc "remote readiness uses effective confinement without changing preference"
                 (dolist (pair '((edits off required) (full-auto required off)))
                   (let* ((target (niwa-execution-target-create
                                   "/ssh:user@host:/srv/project/"))
                          (session (niwa-session--create
                                    :execution-target target :permission-mode (car pair)
                                    :sandbox-mode (cadr pair)))
                          seen)
                     (cl-letf (((symbol-function 'niwa-execution-target-probe)
                                (lambda (_target _refresh sandbox)
                                  (setq seen sandbox))))
                       (niwa--probe-session-target session))
                     (should (eq (caddr pair) seen))
                     (should (eq (cadr pair) (niwa-session-sandbox-mode session)))))

                 :doc "does not probe local sessions"
                 (let* ((workspace (niwa-workspace--create
                                    :type 'project :id "/tmp/project/"
                                    :root "/tmp/project/" :name "local"))
                        (session (niwa-session-create "main" workspace)))
                   (cl-letf (((symbol-function
                               'niwa-execution-target-probe)
                              (lambda (&rest _)
                                (ert-fail "local target was probed"))))
                     (should-not (niwa--probe-session-target session)))))

(niwa-deftest niwa-retry-target-readiness ()
                 ,test
                 (test)
                 :doc "force-reprobes the current remote session"
                 (with-temp-buffer
                   (let* ((target (niwa-execution-target-create
                                   "/ssh:user@host:/srv/project/"))
                          (session (niwa-session--create
                                    :execution-target target))
                          seen diagnostics)
                     (setq-local niwa--session session)
                     (cl-letf (((symbol-function
                                 'niwa--probe-session-target)
                                (lambda (actual refresh)
                                  (setq seen (list actual refresh))
                                  '(:status ready))))
                       (niwa-test--with-captured-diagnostics diagnostics
                         (should (equal '(:status ready)
                                        (niwa-retry-target-readiness)))))
                     (should (string-match-p "niwa: execution target"
                                             diagnostics))
                     (should (equal (list session t) seen))))

                 :doc "requires explicit acknowledgement after unknown loss"
                 (with-temp-buffer
                   (let* ((target (niwa-execution-target-create
                                   "/ssh:user@host:/srv/project/"))
                          (session (niwa-session--create
                                    :execution-target target))
                          acknowledged)
                     (setq-local niwa--session session)
                     (setf (niwa-execution-target-readiness target)
                           '(:status ready))
                     (cl-letf (((symbol-function
                                 'niwa--probe-session-target)
                                (lambda (&rest _args) '(:status ready)))
                               ((symbol-function
                                 'niwa-execution-mutation-blocked-p)
                                (lambda (_session) '(:group-id 42)))
                               ((symbol-function 'yes-or-no-p)
                                (lambda (&rest _) t))
                               ((symbol-function
                                 'niwa-execution-acknowledge-unknown)
                                (lambda (actual)
                                  (setq acknowledged actual))))
                       (niwa-test--with-captured-diagnostics nil
                         (niwa-retry-target-readiness)))
                     (should (eq session acknowledged))))

                 :doc "acknowledges a restored durable latch without process state"
                 (with-temp-buffer
                   (let* ((target (niwa-execution-target-create
                                   "/ssh:user@host:/srv/project/"))
                          (session (niwa-session--create
                                    :execution-target target
                                    :lease '(:unsettled-mutation t)))
                          acknowledged)
                     (setq-local niwa--session session)
                     (setf (niwa-execution-target-readiness target)
                           '(:status ready))
                     (cl-letf (((symbol-function
                                 'niwa--probe-session-target)
                                (lambda (&rest _args) '(:status ready)))
                               ((symbol-function 'yes-or-no-p)
                                (lambda (&rest _) t))
                               ((symbol-function
                                 'niwa-execution-acknowledge-unknown)
                                (lambda (actual)
                                  (setq acknowledged actual))))
                       (niwa-test--with-captured-diagnostics nil
                         (niwa-retry-target-readiness)))
                     (should (eq session acknowledged))))

                 :doc "keeps unknown loss blocked when acknowledgement is declined"
                 (with-temp-buffer
                   (let* ((target (niwa-execution-target-create
                                   "/ssh:user@host:/srv/project/"))
                          (session (niwa-session--create
                                    :execution-target target)))
                     (setq-local niwa--session session)
                     (setf (niwa-execution-target-readiness target)
                           '(:status ready))
                     (cl-letf (((symbol-function
                                 'niwa--probe-session-target)
                                (lambda (&rest _args) '(:status ready)))
                               ((symbol-function
                                 'niwa-execution-mutation-blocked-p)
                                (lambda (_session) '(:group-id 42)))
                               ((symbol-function 'yes-or-no-p)
                                (lambda (&rest _) nil))
                               ((symbol-function
                                 'niwa-execution-acknowledge-unknown)
                                (lambda (_session)
                                  (ert-fail "unknown outcome acknowledged"))))
                       (should-error
                        (niwa-retry-target-readiness)
                        :type 'user-error))))

                 :doc "rejects a local session"
                 (with-temp-buffer
                   (let* ((workspace (niwa-workspace--create
                                      :type 'project :id "/tmp/project/"
                                      :root "/tmp/project/" :name "local"))
                          (session (niwa-session-create "main" workspace)))
                     (setq-local niwa--session session)
                     (should-error (niwa-retry-target-readiness)
                                   :type 'user-error))))

(niwa-deftest niwa--chat-buffer-init-common ()
		 ,test
		 (test)
		 :doc "notifies about pending plugin hook consent during setup"
		 (let* ((root (file-name-as-directory
			       (make-temp-file "niwa-chat-init-" t)))
			(workspace (niwa-workspace--create
				    :type 'project
				    :id root
				    :root root
				    :name "init"))
			(session (niwa-session-create "main" workspace root))
		 notified-workspace
		 probed-session
		 validated-workspace
		 mutable-messages
		 inspection-messages)
		   (setf (niwa-session-plan-metadata session)
		         '(:implementation-retry (:selection (:location here))))
		   (unwind-protect
		       (with-temp-buffer
			         (org-mode)
			         (insert ":PROPERTIES:\n:GPTEL_MODEL: stale-model\n:END:\nTranscript\n")
			         (set-buffer-modified-p nil)
			 (setq-local niwa--session session)
			   (cl-letf (((symbol-function
				     'niwa-reminders-install-defaults)
				    #'ignore)
				   ((symbol-function
				     'niwa-preset--build-handlers)
				    #'identity)
				   ((symbol-function
                                     'niwa-session-artifacts-install-gptel-save-state-advice)
				    #'ignore)
				   ((symbol-function 'niwa-skills-install)
				    #'ignore)
				   ((symbol-function
				     'niwa-skills-install-activation-hook)
				    #'ignore)
				   ((symbol-function
				     'niwa-skills-install-activation-hook)
				    #'ignore)
				   ((symbol-function 'niwa-view--ensure)
				    #'ignore)
			   ((symbol-function
			     'niwa-permission-validate-persistent-stores)
				    (lambda (workspace)
				      (setq validated-workspace workspace)))
			   ((symbol-function 'niwa--run-session-start-hooks)
			    #'ignore)
			   ((symbol-function 'niwa--probe-session-target)
			    (lambda (session &optional _refresh)
			      (setq probed-session session)))
				   ((symbol-function
				     'niwa-plugins-notify-pending-consent)
				    (lambda (workspace)
				      (setq notified-workspace workspace))))
			   (niwa-test--with-captured-messages mutable-messages
			     (niwa--chat-buffer-init-common
			      (current-buffer) workspace "resume")))
			 (should (string-match-p
			          "niwa-retry-plan-implementation"
			          mutable-messages))
			 (should (= 1 (length
			               (niwa-session-pending-reminders session))))
			 (should (string-match-p
			          "reconcile current state"
			          (car (niwa-session-pending-reminders session))))
			 (should (eq notified-workspace workspace))
			 (should (eq probed-session session))
			 (should (eq validated-workspace workspace))
				 (should-not (org-entry-get (point-min) "GPTEL_MODEL"))
				 (should-not (buffer-modified-p))
			 (should (memq #'niwa-tool-repair-pre-tool-call
				       gptel-pre-tool-call-functions))
			 ;; niwa's permission pipeline confirms tool calls.
			 (should (local-variable-p 'gptel-confirm-tool-calls))
			 (should-not gptel-confirm-tool-calls)
			 (should (memq #'niwa-tool-repair-post-tool-call
				       gptel-post-tool-call-functions))
			 (should (memq #'niwa-tool-repair-clear-ledger
				       gptel-post-response-functions))
			 (should (memq #'niwa-view-stream-render-response
				       gptel-post-response-functions))
			 (should (memq #'niwa-view-stream-spinner-hook
				       gptel-pre-tool-call-functions))
			 (should (memq #'niwa-view-stream-pre-tool
				       gptel-pre-tool-call-functions))
			 (should (memq #'niwa-view-stream-post-tool
				       gptel-post-tool-call-functions))
			 (should (memq #'niwa-view-stream-schedule
				       gptel-post-stream-hook))
			 (should (memq #'niwa-tool-repair-clear-ledger
				       kill-buffer-hook))
				 (dolist (modified '(nil t))
				   (with-temp-buffer
				     (org-mode)
				     (insert ":PROPERTIES:\n:GPTEL_MODEL: stale-model\n:END:\nTranscript\n")
				     (set-buffer-modified-p modified)
				     (setq-local niwa--session session)
				     (setq buffer-read-only t)
                             (cl-letf (((symbol-function 'niwa-view--ensure) #'ignore))
				       (niwa-test--with-captured-messages inspection-messages
				         (niwa--chat-buffer-init-common
				          (current-buffer) workspace "resume" t)))
				     (should-not (org-entry-get (point-min) "GPTEL_MODEL"))
				     (should (eq modified (buffer-modified-p)))))
			 (should-not (string-match-p
			              "niwa-retry-plan-implementation"
			              inspection-messages)))
		     (delete-directory root t)))

		 :doc "watcher warnings do not derail view creation"
		 (let* ((root (file-name-as-directory
			       (make-temp-file "niwa-chat-watch-warning-" t)))
			(workspace (niwa-workspace--create
				    :type 'project
				    :id root
				    :root root
				    :name "watch-warning"))
			(session (niwa-session-create "main" workspace root))
			(chat (generate-new-buffer " *niwa-chat-watch-warning*"))
			(other (generate-new-buffer " *niwa-chat-selected*"))
			reminder-session
			ensured-buffer)
		   (unwind-protect
		       (with-current-buffer chat
			 (setq-local niwa--session session)
			 (cl-letf (((symbol-function
				     'niwa-reminders-install-defaults)
				    #'ignore)
				   ((symbol-function
				     'niwa-preset--build-handlers)
				    #'identity)
				   ((symbol-function
                                     'niwa-session-artifacts-install-gptel-save-state-advice)
				    #'ignore)
				   ((symbol-function 'niwa-skills-install)
				    (lambda (_session _buffer)
				      (niwa-skills--ensure-watcher root)))
				   ((symbol-function 'file-notify-add-watch)
				    (lambda (&rest _args)
				      (error "Watcher failed")))
				   ((symbol-function 'display-warning)
				    (lambda (&rest _args) (set-buffer other)))
				   ((symbol-function
				     'niwa-skills-install-activation-hook)
				    (lambda ()
				      (setq reminder-session niwa--session)))
				   ((symbol-function 'niwa-view--ensure)
				    (lambda (actual)
				      (setq ensured-buffer actual)))
				   ((symbol-function
				     'niwa-permission-validate-persistent-stores)
				    #'ignore)
				   ((symbol-function
				     'niwa--run-session-start-hooks)
				    #'ignore)
				   ((symbol-function
				     'niwa--probe-session-target)
				    #'ignore)
				   ((symbol-function
				     'niwa-plugins-notify-pending-consent)
				    #'ignore))
			   (niwa--chat-buffer-init-common
			    chat workspace "startup"))
			 (should (eq session reminder-session))
			 (should (eq chat ensured-buffer)))
		     (when (buffer-live-p chat) (kill-buffer chat))
		     (when (buffer-live-p other) (kill-buffer other))
		     (delete-directory root t))))

(niwa-deftest niwa-session-lifecycle-hooks
		 ()
		 ,test
		 (test)

		 :doc "runs normal and declarative session lifecycle hooks"
		 (let* ((root (file-name-as-directory
			       (make-temp-file "niwa-chat-hooks-" t)))
			(user-dir (file-name-as-directory
				   (make-temp-file "niwa-chat-hooks-user-" t)))
			(workspace (niwa-workspace--create
				    :type 'project
				    :id root
				    :root root
				    :name "hooks"))
			(session (niwa-session-create "main" workspace root))
			(niwa-user-dir user-dir)
			(normal-events nil)
			(niwa-chat-test--hook-events nil)
			(niwa-session-start-hook
			 (list (lambda () (push 'start normal-events))))
			(niwa-session-end-hook
			 (list (lambda () (push 'end normal-events))))
			(niwa-hook-rules
			 '((SessionStart
			    ((:matcher "startup"
				       :hooks ((:type elisp
						      :function
						      niwa-chat-test--session-context-hook)))))
			   (SessionEnd
			    ((:matcher "kill-buffer"
				       :hooks ((:type elisp
						      :function niwa-chat-test--record-hook))))))))
		   (unwind-protect
		       (with-temp-buffer
			 (setq-local niwa--session session)
			 (setq-local niwa--workspace workspace)
			 (niwa--run-session-start-hooks "startup")
			 (niwa--run-session-end-hooks)
			 (should (equal (nreverse normal-events) '(start end)))
			 (should
			  (equal
			   (mapcar (lambda (event)
				     (plist-get event :hook-event-name))
				   (nreverse niwa-chat-test--hook-events))
			   '(SessionStart SessionEnd)))
			 (should
			  (equal (niwa-session-hook-context-pending session)
				 '((:event "SessionStart"
				    :body "startup context" :source user)))))
		     (delete-directory root t)
		     (delete-directory user-dir t)))

		 :doc "resets workspace-instruction delivery for the affected contexts"
		 (let* ((root (file-name-as-directory
			       (make-temp-file "niwa-chat-context-epoch-" t)))
			(workspace (niwa-workspace--create
				    :type 'project :id root :root root
				    :name "context-epoch"))
			(session (niwa-session-create "main" workspace root))
			(niwa-session-start-hook nil))
		   (unwind-protect
		       (with-temp-buffer
			 (setq-local niwa--session session)
			 (setq-local niwa--workspace workspace)
			 (setf (niwa-session-workspace-instruction-hashes session)
			       '((("/root" "/workspace/AGENTS.md") . "root")
			         (("/root/agent" "/workspace/AGENTS.md") . "agent")))
			 (cl-letf (((symbol-function 'niwa-hooks-run-event)
				    (lambda (_event _payload callback &rest _)
				      (funcall callback nil))))
			   (niwa--run-session-start-hooks "compact")
			   (should
			    (equal (niwa-session-workspace-instruction-hashes session)
				   '((("/root/agent" "/workspace/AGENTS.md") . "agent"))))
			   (niwa--run-session-start-hooks "resume"))
			 (should-not (niwa-session-workspace-instruction-hashes session)))
			     (delete-directory root t)))

		 :doc "records the loaded code's revision once per start"
		 ;; A running Emacs keeps its loaded code while the checkout moves on.
		 (let* ((root (file-name-as-directory
			       (make-temp-file "niwa-chat-runtime-" t)))
			(workspace (niwa-workspace--create
				    :type 'project :id root :root root :name "runtime"))
			(session (niwa-session-create "main" workspace root))
			(niwa-session-start-hook nil)
			(niwa-hook-rules nil)
			(niwa--load-provenance
			 '(:loaded-at "2026-01-01T00:00:00Z"
			   :niwa-commit "fixture-revision" :niwa-version "fixture"
			   :gptel-commit nil :gptel-version nil))
			events)
		   (require 'niwa-telemetry)
		   (unwind-protect
		       (cl-letf (((symbol-function 'niwa-telemetry-record)
				  (lambda (_session event &rest props)
				    (when (eq event 'session-runtime) (push props events)))))
			 (with-temp-buffer
			   (setq-local niwa--session session)
			   (setq-local niwa--workspace workspace)
			   (niwa--run-session-start-hooks "resume"))
			 (should (= 1 (length events)))
			 (let ((props (car events)))
			   (should (equal "resume" (plist-get props :boundary)))
			   ;; Forward the loaded snapshot, including absent metadata.
			   (should (equal props
					  (append (list :boundary "resume"
							:emacs-version emacs-version)
						  niwa--load-provenance)))))
		     (delete-directory root t))))

(niwa-deftest niwa-session-start-hooks-wait
		 (:doc "waits for asynchronous SessionStart context before returning")
		 (let* ((root (file-name-as-directory
			       (make-temp-file "niwa-chat-hooks-wait-" t)))
			(workspace (niwa-workspace--create
				    :type 'project
				    :id root
				    :root root
				    :name "hooks"))
			(session (niwa-session-create "main" workspace root))
			(called nil)
                        payload)
		   (unwind-protect
		       (with-temp-buffer
			 (setq-local niwa--session session)
			 (setq-local niwa--workspace workspace)
			 (cl-letf (((symbol-function 'niwa-hooks-run-event)
				    (lambda (_event event-payload callback &rest _)
                                      (setq payload event-payload)
				      (run-at-time
				       0.01 nil
				       (lambda ()
					 (setq called t)
					 (funcall callback
						  '(:additional-context
						    ("async startup"))))))))
			   (niwa--run-session-start-hooks "compact"))
			 (should called)
			 (should (equal "compact" (plist-get payload :source)))
			 (should-not niwa--session-start-hooks-pending)
			 (should
			  (equal (niwa-session-hook-context-pending session)
				 '((:event "SessionStart"
				    :body "async startup")))))
		     (delete-directory root t))))


(niwa-deftest niwa--run-session-end-hooks
  ()
  ,test
  (test)

  :doc "ends the epoch once: kill-buffer no longer reports it afterwards"
  (let* ((root (file-name-as-directory
                (make-temp-file "niwa-chat-end-once-" t)))
         (workspace (niwa-workspace--create
                     :type 'project :id root :root root :name "end"))
         (session (niwa-session-create "main" workspace root))
         (buffer (generate-new-buffer " *niwa-end-once*"))
         (niwa-session-end-hook nil)
         reasons)
    (unwind-protect
        (cl-letf (((symbol-function 'niwa-hooks-run-event)
                   (lambda (_event payload callback &rest _)
                     (push (plist-get payload :reason) reasons)
                     (funcall callback nil))))
          (with-current-buffer buffer
            (setq-local niwa--session session)
            (setq-local niwa--workspace workspace)
            (add-hook 'kill-buffer-hook #'niwa--run-session-end-hooks nil t)
            (niwa--run-session-end-hooks "exit")
            (should-not (memq #'niwa--run-session-end-hooks
                              kill-buffer-hook)))
          (kill-buffer buffer)
          (should (equal '("exit") reasons)))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (delete-directory root t)))

  :doc "reports kill-buffer by default and settles CALLBACK without a session"
  (let* ((root (file-name-as-directory
                (make-temp-file "niwa-chat-end-default-" t)))
         (workspace (niwa-workspace--create
                     :type 'project :id root :root root :name "end"))
         (session (niwa-session-create "main" workspace root))
         (niwa-session-end-hook nil)
         reason settled)
    (unwind-protect
        (cl-letf (((symbol-function 'niwa-hooks-run-event)
                   (lambda (_event payload callback &rest _)
                     (setq reason (plist-get payload :reason))
                     (funcall callback nil))))
          (with-temp-buffer
            (setq-local niwa--session session)
            (niwa--run-session-end-hooks))
          (should (equal "kill-buffer" reason))
          (with-temp-buffer
            (niwa--run-session-end-hooks
             "exit" (lambda (_decision) (setq settled t))))
          (should settled))
      (delete-directory root t))))

(niwa-deftest niwa--end-sessions-on-exit
  ()
  ,test
  (test)

  :doc "ends open epochs with reason exit and waits for asynchronous handlers"
  (let* ((root (file-name-as-directory
                (make-temp-file "niwa-chat-exit-" t)))
         (workspace (niwa-workspace--create
                     :type 'project :id root :root root :name "exit"))
         (live (generate-new-buffer " *niwa-exit-live*"))
         (inspection (generate-new-buffer " *niwa-exit-inspection*"))
         (niwa-session-end-hook nil)
         ended settled)
    (unwind-protect
        (progn
          (dolist (buffer (list live inspection))
            (with-current-buffer buffer
              (setq-local niwa--session
                          (niwa-session-create "main" workspace root))
              (setq-local niwa--workspace workspace)))
          ;; Only a buffer that owns its session epoch reports its end.
          (with-current-buffer live
            (add-hook 'kill-buffer-hook #'niwa--run-session-end-hooks
                      nil t))
          (cl-letf (((symbol-function 'buffer-list)
                     (lambda (&optional _frame) (list live inspection)))
                    ((symbol-function 'niwa-hooks-run-event)
                     (lambda (_event payload callback &rest _)
                       (push (cons (current-buffer)
                                   (plist-get payload :reason))
                             ended)
                       (run-at-time 0.02 nil
                                    (lambda ()
                                      (setq settled t)
                                      (funcall callback nil))))))
            (niwa--end-sessions-on-exit)
            (should settled)
            (should (equal (list (cons live "exit")) ended))
            (kill-buffer live)
            (should (= 1 (length ended)))))
      (dolist (buffer (list live inspection))
        (when (buffer-live-p buffer) (kill-buffer buffer)))
      (delete-directory root t)))

  :doc "a failing session does not keep the others from ending"
  (let* ((root (file-name-as-directory
                (make-temp-file "niwa-chat-exit-fail-" t)))
         (workspace (niwa-workspace--create
                     :type 'project :id root :root root :name "exit"))
         (bad (generate-new-buffer " *niwa-exit-bad*"))
         (good (generate-new-buffer " *niwa-exit-good*"))
         ended messages)
    (unwind-protect
        (progn
          (dolist (buffer (list bad good))
            (with-current-buffer buffer
              (setq-local niwa--session
                          (niwa-session-create "main" workspace root))
              (setq-local niwa--workspace workspace)
              (add-hook 'kill-buffer-hook #'niwa--run-session-end-hooks
                        nil t)))
          (with-current-buffer bad
            (setq-local niwa-session-end-hook
                        (list (lambda () (error "Injected SessionEnd failure")))))
          (niwa-test--with-captured-diagnostics messages
            (cl-letf (((symbol-function 'buffer-list)
                       (lambda (&optional _frame) (list bad good)))
                      ((symbol-function 'niwa-hooks-run-event)
                       (lambda (_event _payload callback &rest _)
                         (push (current-buffer) ended)
                         (funcall callback nil))))
              (niwa--end-sessions-on-exit)))
          (should (equal (list good) ended))
          (should (string-match-p "Injected SessionEnd failure" messages)))
      (dolist (buffer (list bad good))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (remove-hook 'kill-buffer-hook #'niwa--run-session-end-hooks t))
          (kill-buffer buffer)))
      (delete-directory root t))))

;;
;;; Local user turns

(niwa-deftest niwa--insert-local-user-turn
		 (:doc "persists distinct view text without exposing it to the model")
		 (let ((data-buffer (generate-new-buffer " *niwa-local-turn-data*"))
		       (view-buffer (generate-new-buffer " *niwa-local-turn-view*"))
		       (displayed nil)
		       (kind nil)
		       (source nil)
		       (sent nil)
		       (original-view-fn (and (fboundp 'niwa-view--begin-external-turn)
					      (symbol-function 'niwa-view--begin-external-turn))))
		   (unwind-protect
		       (progn
			 (fset 'niwa-view--begin-external-turn
			       (lambda (display-text _marker &optional turn-kind
                                                     _hook-context _no-spinner user-source
                                                     _attribution)
				 (setq displayed display-text
				       kind turn-kind
				       source user-source)))
			 (cl-letf (((symbol-function 'gptel-send)
				    (lambda (&rest _) (setq sent t))))
			   (with-current-buffer data-buffer
			     (org-mode)
			     (setq-local gptel-response-separator "\n\n")
			     (setq-local niwa--view-buffer view-buffer)
			     (let ((marker (niwa--insert-local-user-turn
					    "Setup context" "Show setup" 'worktree)))
			       (should (markerp marker))
			       (should (string-match-p
					"Setup context"
					(buffer-substring-no-properties
					 (point-min) (point-max))))
			       (goto-char (point-min))
			       (search-forward "<!-- niwa-render-data -->")
			       (should (eq 'niwa-render-data
					   (get-text-property (match-beginning 0) 'gptel)))
			       (should
				(equal
				 '(:kind user-display :text "Show setup")
                                 (cdr (niwa-tool-render-data-extract
				       (buffer-substring
					(point-min) (point-max)))))))))
			 (should (equal "Show setup" displayed))
			 (should (eq 'worktree kind))
			 (with-current-buffer data-buffer
			   (should (equal "Setup context\n"
			                  (buffer-substring-no-properties (car source) (cdr source)))))
			 (should-not sent))
		     (if original-view-fn
			 (fset 'niwa-view--begin-external-turn original-view-fn)
		       (fmakunbound 'niwa-view--begin-external-turn))
		     (when (buffer-live-p data-buffer)
		       (kill-buffer data-buffer))
		     (when (buffer-live-p view-buffer)
		       (kill-buffer view-buffer)))))

(niwa-deftest niwa--send-request ()
  ,test
  (test)
  :doc "sends the session id as the Codex prompt_cache_key in the realized payload"
  (let* ((gptel--known-backends nil)
         (backend (gptel-make-openai-oauth "Chat Codex"))
         (gptel--openai-oauth-token-file (make-temp-file "mev-token-"))
         (orig (symbol-function 'gptel-request))
         key)
    ;; A current token, so the real readiness guard passes.
    (with-temp-file gptel--openai-oauth-token-file
      (prin1 (list :access_token "current" :expires_at (+ (float-time) 3600))
             (current-buffer)))
    (unwind-protect
        (with-temp-buffer
          (setq-local gptel-backend backend gptel-model 'gpt-5.5
                      gptel-use-tools nil gptel-stream nil
                      niwa--session (niwa-session--create :session-id "root-id"))
          ;; Real send path; :dry-run stops gptel after it realizes the payload.
          (cl-letf (((symbol-function 'gptel-request)
                     (lambda (_prompt &rest args)
                       (let ((fsm (apply orig "hi" :dry-run t args)))
                         (setq key (plist-get (plist-get (gptel-fsm-info fsm) :data)
                                              :prompt_cache_key))
                         fsm))))
            (niwa--send-request "derived prompt"))
          (should (equal "root-id" key)))
      (delete-file gptel--openai-oauth-token-file)))

  :doc "returns the FSM for a standard transformed streaming request"
  (with-temp-buffer
    (setq-local gptel-prompt-transform-functions '(transform)
                gptel-stream t)
    (let (model-input request-args)
      (cl-letf (((symbol-function 'gptel-request)
                 (lambda (&optional _prompt &rest args)
                   (should (local-variable-p 'niwa--pending-model-input))
                   (setq model-input niwa--pending-model-input
                         request-args args)
                   (setq-local niwa--pending-model-input nil)
                   (plist-get args :fsm))))
        (let ((fsm (niwa--send-request "derived prompt")))
          (should fsm)
          (should (eq fsm (plist-get request-args :fsm)))
          (should (equal "derived prompt" model-input))
          (should (equal '(transform)
                         (plist-get request-args :transforms)))
          (should (plist-get request-args :stream)))
        (should-not niwa--pending-model-input)))))

(niwa-deftest niwa--submit-generated-turn ()
  ,test
  (test)
  :doc "returns its FSM while persisting hook context in the transcript"
  (with-temp-buffer
    (org-mode)
    (setq-local gptel-response-separator "\n\n"
                gptel-prompt-transform-functions '(transform)
                gptel-stream t)
    (let ((fsm (gptel-make-fsm))
          model-input
          request-args)
      (cl-letf (((symbol-function 'gptel-request)
                 (lambda (&optional _prompt &rest args)
                   (setq model-input niwa--pending-model-input
                         request-args args)
                   fsm)))
        (should
         (eq fsm
             (niwa--submit-generated-turn
              "Planning prompt" "Goal"
              (niwa-prompt-submission-create
               :context "<hook-context>ctx</hook-context>"
               :state 'committed)))))
      (should (string-match-p "Planning prompt" (buffer-string)))
      (should (string-match-p "hook-context" (buffer-string)))
      (should (string-match-p "hook-context" model-input))
      (should (equal '(transform)
                         (plist-get request-args :transforms)))))

  :doc "stores the generated prompt while sending prepared skill context"
  (with-temp-buffer
    (org-mode)
    (setq-local gptel-response-separator "\n\n")
    (let (model-input)
      (cl-letf (((symbol-function 'gptel-request)
                 (lambda (&rest _)
                   (setq model-input niwa--pending-model-input))))
        (niwa--submit-generated-turn
         "hook input" "Implement accepted plan"
         (niwa-prompt-submission-create :input "hook input")
         '(:transcript-input "Use $alpha"
           :model-input "Use [skill:alpha -- attached]\n\nALPHA BODY"
           :render-data "<!-- niwa-render-data -->")))
      (should (string-match-p "Use \\$alpha" (buffer-string)))
      (should (string-match-p "niwa-render-data" (buffer-string)))
      (should (string-match-p "ALPHA BODY" model-input))
      (should (string-match-p "niwa-render-data" model-input))))

  :doc "commits inserted context before a synchronous request failure"
  (with-temp-buffer
    (org-mode)
    (let* ((session (niwa-session--create :name "goal-context"))
           (context-entries '((:event SessionStart :body "goal context")))
           (submission
            (niwa-prompt-submission-create
             :context "<hook-context>goal context</hook-context>"
             :session session
             :context-entries context-entries)))
      (setq-local niwa--session session
                  gptel-response-separator "\n\n")
      (setf (niwa-session-hook-context-pending session) context-entries)
      (cl-letf (((symbol-function 'gptel-request)
                 (lambda (&rest _) (error "Request startup failed"))))
        (should-error
         (niwa--submit-generated-turn
          "Planning prompt" "Goal" submission)))
      (should-not (niwa-session-hook-context-pending session))
      (should (string-match-p "goal context" (buffer-string))))))

(niwa-deftest niwa--implement-plan ()
  ,test
  (test)
  :doc "submits the accepted current-context prompt with compact display text"
  (with-temp-buffer
    (org-mode)
    (let ((submission
           (niwa-prompt-submission-create
            :input "Accepted plan artifact: /accepted.md\n\n# Current plan"
            :state 'committed))
          sent-prompt sent-display sent-submission sent-policy)
      (cl-letf (((symbol-function 'niwa--submit-generated-turn)
                 (lambda (prompt display accepted &optional _prepared)
                   (setq sent-prompt prompt
                         sent-display display
                         sent-submission accepted
                         sent-policy niwa--pending-implementation-policy))))
        (niwa--implement-plan
         (list :implementation-policy
               '(:model-provider "Test:model" :reasoning-effort nil :permission-mode edits)
               :display-text "Implement accepted plan as Goal"
               :prompt-submission submission)))
      (should (equal (niwa-prompt-submission-input submission)
                     sent-prompt))
      (should (equal "Implement accepted plan as Goal" sent-display))
      (should (eq submission sent-submission))
      (should (equal '(:model-provider "Test:model" :reasoning-effort nil :permission-mode edits)
                     sent-policy))))

  :doc "stages prepared skill request context for the implementation turn"
  (with-temp-buffer
    (org-mode)
    (let* ((submission
            (niwa-prompt-submission-create :input "hook input"))
           (prepared
            '(:model-input "expanded" :transcript-input "authored"
              :request-context (:invoked-skills (alpha))))
           seen)
      (cl-letf (((symbol-function 'niwa--submit-generated-turn)
                 (lambda (&rest _)
                   (setq seen niwa-skills--pending-request-context))))
        (niwa--implement-plan
         (list :implementation-policy
               '(:model-provider "Test:model" :reasoning-effort nil :permission-mode edits)
               :prompt-submission submission
               :prepared-outcome prepared)))
      (should (equal '(:invoked-skills (alpha)) seen))))

  :doc "startup failure clears staged policy without changing the session baseline"
  (with-temp-buffer
    (org-mode)
    (let ((session (niwa-session--create :permission-mode 'ask
                                           :model-provider "Baseline:model"))
          (submission (niwa-prompt-submission-create :input "Implement")))
      (setq-local niwa--session session)
      (cl-letf (((symbol-function 'niwa--submit-generated-turn)
                 (lambda (&rest _) (error "Request startup failed"))))
        (should-error
         (niwa--implement-plan
          (list :implementation-policy
                '(:model-provider "Test:model" :reasoning-effort nil :permission-mode edits)
                :prompt-submission submission))))
      (should-not niwa--pending-implementation-policy)
      (should (eq 'ask (niwa-session-permission-mode session)))
      (should (equal "Baseline:model" (niwa-session-model-provider session))))))


;;
;;; Working directory sessions

(niwa-deftest niwa--active-chat-buffer
  (:doc "prefers direct context and falls back to the workspace's most recent root")
  (let* ((workspace (niwa-workspace--create
                     :type 'project :id "active" :root "/tmp/" :name "active"))
         (other-workspace (niwa-workspace--create
                           :type 'project :id "other" :root "/tmp/" :name "other"))
         (session (niwa-session--create :name "same" :workspace workspace))
         (first (generate-new-buffer " *active-first*"))
         (second (generate-new-buffer " *active-second*"))
         (view (generate-new-buffer " *active-view*"))
         (agent (generate-new-buffer " *active-agent*"))
         (other (generate-new-buffer " *active-other*"))
         (transient (generate-new-buffer " *active-transient*"))
         (invocation (niwa-agent-invocation--create :parent-data-buffer first)))
    (unwind-protect
        (progn
          (dolist (buffer (list first second view agent))
            (with-current-buffer buffer (setq-local niwa--session session)))
          (with-current-buffer view (setq-local niwa--data-buffer first))
          (with-current-buffer agent
            (setq-local niwa--agent-invocation invocation))
          (with-current-buffer other
            (setq-local niwa--session
                        (niwa-session--create :workspace other-workspace)))
          (with-current-buffer transient
            (setq-local niwa--session
                        (niwa-session--create :workspace workspace
                                                :audit-session session)))
          (dolist (roots (list (list second first) (list first second)))
            (cl-letf (((symbol-function 'buffer-list)
                       (lambda (&optional _frame)
                         (append (list other agent view transient) roots)))
                      ((symbol-function 'niwa-workspace)
                       (lambda (&optional _buffer) workspace)))
              (with-temp-buffer
                (should (eq (car roots) (niwa--active-chat-buffer workspace)))
                (should (eq (car roots) (niwa--active-chat-buffer))))
              (dolist (buffer (list first view agent))
                (with-current-buffer buffer
                  (should (eq first (niwa--active-chat-buffer workspace)))))))
          (kill-buffer first)
          (cl-letf (((symbol-function 'buffer-list)
                     (lambda (&optional _frame)
                       (list agent view other transient second))))
            (with-current-buffer agent
              (should (eq second (niwa--active-chat-buffer workspace)))))
          (kill-buffer second)
          (cl-letf (((symbol-function 'buffer-list)
                     (lambda (&optional _frame) (list agent view other transient))))
            (with-current-buffer agent
              (should-not (niwa--active-chat-buffer workspace)))
            (with-temp-buffer
              (should-not (niwa--active-chat-buffer workspace)))))
      (dolist (buffer (list first second view agent other transient))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(niwa-deftest niwa--workspace-chat-sessions
  (:doc "ordinary chat selection excludes artifact conversations but retains their direct context")
  (let* ((root (make-temp-file "niwa-chat-artifact-" t))
         (workspace (niwa-workspace--create :type 'project :id "chat-artifact" :root root))
         (dedicated (generate-new-buffer " *dedicated-chat*"))
         (ordinary (generate-new-buffer " *ordinary-chat*")))
    (unwind-protect
        (progn
          (require 'niwa-artifact-store)
          (make-directory (niwa-artifact-store-artifact-directory workspace "a") t)
          (niwa-artifact-store-create-meta workspace "a" "a.md")
          (niwa-artifact-store-update-meta workspace "a" :dedicated-session "dedicated")
          (with-current-buffer dedicated
            (setq-local niwa--session
                        (niwa-session--create :session-id "dedicated" :name "Artifact"
                                                 :dedicated-artifact "a"
                                                 :workspace workspace :working-directory root)))
          (with-temp-buffer
            (should (= 1 (length (niwa--workspace-sessions workspace))))
            (should-not (niwa--workspace-chat-sessions workspace))
            (should-not (niwa--active-chat-buffer workspace))
            ;; A lone dedicated conversation must not absorb M-x niwa.
            (let (created displayed)
              (cl-letf (((symbol-function 'niwa--chat-buffer)
                         (lambda (&rest _) (setq created t) ordinary))
                        ((symbol-function 'niwa--display-chat-buffer)
                         (lambda (buffer) (setq displayed buffer))))
                (niwa--start-chat workspace root nil))
              (should created)
              (should (eq ordinary displayed))))
          (with-current-buffer dedicated
            (should (eq dedicated (niwa--active-chat-buffer workspace))))
          (with-current-buffer ordinary
            (setq-local niwa--session
                        (niwa-session--create :session-id "ordinary" :name "Chat"
                                                 :workspace workspace :working-directory root)))
          (should (equal (list (cons "Chat" ordinary))
                         (niwa--workspace-chat-sessions workspace)))
          (let (choices)
            (cl-letf (((symbol-function 'niwa--pick-session)
                       (lambda (sessions _) (setq choices sessions) ordinary))
                      ((symbol-function 'niwa--display-chat-buffer) #'ignore))
              (niwa--start-chat workspace root t))
            (should (equal (list (cons "Chat" ordinary)) choices))))
      (kill-buffer dedicated)
      (kill-buffer ordinary)
      (delete-directory root t))))

(niwa-deftest niwa--ensure-chat-preset
  ()
  ,test
  (test)
  :doc "keeps an existing session's selected preset"
  (let ((buf (generate-new-buffer " *niwa-existing-preset*"))
        applied)
    (unwind-protect
        (progn
          (with-current-buffer buf
            (setq-local niwa--session
                        (niwa-session--create
                         :name "test" :preset-name 'selected)))
          (cl-letf (((symbol-function 'niwa-preset-apply)
                     (lambda (&rest _) (setq applied t))))
            (niwa--ensure-chat-preset buf))
          (should-not applied))
      (kill-buffer buf)))
  :doc "applies the default to a fresh session"
  (let ((buf (generate-new-buffer " *niwa-fresh-preset*"))
        applied)
    (unwind-protect
        (progn
          (with-current-buffer buf
            (setq-local niwa--session
                        (niwa-session--create :name "test")))
          (cl-letf (((symbol-function 'niwa-preset-apply)
                     (lambda (name &optional _)
                       (setq applied name))))
            (niwa--ensure-chat-preset buf))
          (should (eq applied
                      (alist-get niwa-default-chat-preset
                                 niwa-action-preset-alist))))
      (kill-buffer buf))))

(niwa-deftest niwa-session-working-directory
		 (:before-each (niwa-workspace-clear-registry)
			       :vars* ((root-dir (file-name-as-directory
						  (make-temp-file "niwa-chat-cwd-" t)))
				       (module-dir (file-name-concat root-dir "packages" "api"))
				       (chat-buffer nil))
			       :after-each
			       (progn
				 (niwa-workspace-clear-registry)
				 (when (and chat-buffer (buffer-live-p chat-buffer))
				   (let ((view-buf (buffer-local-value 'niwa--view-buffer
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
		   (let* ((workspace (niwa-workspace-get-or-create
				      'project root-dir root-dir "cwd-proj")))
		     (setq chat-buffer
			   (niwa--chat-buffer "packages:api" t workspace module-dir))
		     (with-current-buffer chat-buffer
		       (should (equal (file-name-as-directory module-dir)
				      (niwa-session-working-directory niwa--session)))
		       (should (equal (file-name-as-directory module-dir)
				      default-directory)))))

                 :doc "unnamed sessions start with their stable id"
                 (let ((workspace (niwa-workspace-get-or-create
                                   'project root-dir root-dir "cwd-proj")))
                   (setq chat-buffer (niwa--chat-buffer nil t workspace root-dir))
                   (with-current-buffer chat-buffer
                     (should (equal (niwa-session-name niwa--session)
                                    (niwa-session-session-id niwa--session)))
                     (should (eq 'pending (niwa-session-naming-state niwa--session)))))

		 :doc "no-prefix start switches to the only live session across directories"
		 (progn
		   (make-directory module-dir t)
		   (let* ((workspace (niwa-workspace-get-or-create
				      'project root-dir root-dir "cwd-proj"))
			  displayed)
		     (setq chat-buffer
			   (niwa--chat-buffer "packages:api" t workspace module-dir))
		     (cl-letf (((symbol-function 'niwa--display-chat-buffer)
				(lambda (buf)
				  (setq displayed buf))))
		       (niwa--start-chat workspace root-dir nil nil))
		     (should (eq displayed chat-buffer))
		     (with-current-buffer displayed
		       (should (equal (file-name-as-directory module-dir)
				      (niwa-session-working-directory niwa--session)))))))


(niwa-deftest niwa-abort
		 (:doc "aborts active chat request state")
		 ,test
		 (test)

  :doc "marks cancellation before cleanup while retaining pending settlement"
  (dolist (pending '(nil t))
    (with-temp-buffer
      (let* ((session (niwa-session--create :permission-mode 'full-auto))
             (request (niwa-request--create :id "abort-reentry" :session session))
             (fsm (gptel-make-fsm))
             (gptel--request-alist nil)
             observed outcomes)
        (setq-local niwa--session session
                    niwa--current-request request)
        (setf (gptel-fsm-info fsm)
              (list :buffer (current-buffer) :niwa-request-id "abort-reentry"))
        (when pending (niwa--turn-hold fsm))
        (niwa-request-push-canceller
         request
         (lambda ()
           (push (niwa-request-cancelled-p request) observed)
           (niwa-request-push-canceller
            request (lambda () (push 'nested observed)))
           (niwa-permission-request
            (list :kind 'generic :tool-name "Read" :request request
                  :callback (lambda (outcome) (push outcome outcomes))) session)))
        (unwind-protect
            (progn
              (niwa-abort (current-buffer))
              (should (equal '(nested t) observed))
              (should (equal '(aborted) outcomes))
              (should (niwa-request-cancelled-p request))
              (should-not (niwa-request-cancellers request))
              (should (eq (and pending request) niwa--current-request)))
          (when pending (niwa--turn-release fsm))
          (niwa-request-end)))))

  :doc "saves partial text even when compaction cancellation throws"
  (let* ((root (make-temp-file "niwa-abort-save-" t))
         (workspace
          (niwa-workspace--create :type 'file :id root :root root
                                     :name "abort-save"))
         (session (niwa-session-create "main" workspace))
         (buffer (generate-new-buffer " *abort-save*")))
    (unwind-protect
        (with-current-buffer buffer
          (niwa-chat-prepare-transcript-buffer)
          (setq-local niwa--session session)
          (setq-local niwa--workspace workspace)
          (insert "User prompt\n")
          (niwa-session-artifacts-save session buffer)
          (goto-char (point-max))
          (insert (propertize "Partial answer\n" 'gptel 'response))
          (setq-local niwa-compact-run-cancel
                      (lambda () (error "Injected cancellation failure")))
          (should-error (niwa-abort buffer))
          (should (string-search
                   "Partial answer"
                   (niwa-session-artifacts-read-artifact
                    session "segment-0001.chat.org" t)))
          ;; A failed publication must remain retryable even when the
          ;; transcript was already saved and is no longer modified.
          (setq niwa-compact-run-cancel nil)
          (setf (niwa-session-name session) "retry-name")
          (let* ((publications (file-name-concat
                                (niwa-session-save-path session) ".publications"))
                 (head (plist-get (niwa-session-publication session) :head)))
            (should-not (buffer-modified-p))
            (set-file-modes publications #o500)
            (unwind-protect
                (let (diagnostics)
                  (should-not (file-writable-p publications))
                  (niwa-test--with-captured-diagnostics diagnostics
                    (niwa-abort buffer))
                  (should (string-search "Could not auto-save" diagnostics)))
              (set-file-modes publications #o700))
            (should niwa-session--save-failed)
            (should (equal head (plist-get
                                (niwa-session-publication-read
                                 (niwa-session-save-path session)) :head)))
            (should (niwa-session-pending-publication session))
            (niwa-session-publication-retry session)
            (should (niwa-session-persistence-autosave-buffer buffer))
            (should-not niwa-session--save-failed)
            (should (string-search
                     "Partial answer"
                     (niwa-session-artifacts-read-artifact
                      session "segment-0001.chat.org" t)))
            (should (equal "retry-name"
                           (plist-get
                            (read (niwa-session-artifacts-read-artifact
                                   session "session.meta.el" t))
                            :session-name)))))
      (niwa-session-persistence-lock-release
       (niwa-session-save-path session) session)
      (with-current-buffer buffer (set-buffer-modified-p nil))
      (kill-buffer buffer)
      (delete-directory root t)))

  :doc "defers its checkpoint until no remote operation is in flight"
  (let* ((root (make-temp-file "niwa-abort-defer-" t))
         (workspace
          (niwa-workspace--create :type 'file :id root :root root
                                     :name "abort-defer"))
         (session (niwa-session-create "main" workspace))
         (buffer (generate-new-buffer " *abort-defer*"))
         (key (list 'abort-checkpoint buffer)))
    (unwind-protect
        (with-current-buffer buffer
          (niwa-chat-prepare-transcript-buffer)
          (setq-local niwa--session session)
          (setq-local niwa--workspace workspace)
          (insert "User prompt\n")
          (niwa-session-artifacts-save session buffer)
          (goto-char (point-max))
          (insert (propertize "Partial answer\n" 'gptel 'response))
          (niwa-transport-call-as-remote-operation
           (lambda () (niwa-abort buffer)))
          (should (buffer-modified-p))
          (should (gethash key niwa-transport--pending))
          (with-timeout (5 (ert-fail "Deferred abort checkpoint did not run"))
            (while (gethash key niwa-transport--pending)
              (accept-process-output nil 0.01)))
          (should-not (buffer-modified-p))
          (should-not niwa-session--save-failed))
      (niwa-transport-cancel-pending key)
      (niwa-session-persistence-lock-release
       (niwa-session-save-path session) session)
      (with-current-buffer buffer (set-buffer-modified-p nil))
      (kill-buffer buffer)
      (delete-directory root t)))


  :doc "flushes permission queues and the pending plan approval"
  (let ((root (file-name-as-directory
               (make-temp-file "niwa-chat-abort-" t))))
    (unwind-protect
        (with-temp-buffer
          (let* ((workspace (niwa-workspace--create
                             :type 'project :id root :root root :name "abort"))
                 (session (niwa-session-create "main" workspace))
                 (outcomes nil))
            (setq-local niwa--session session)
            (setq-local niwa-compact-run-cancel
                        (lambda () (push '(compaction . aborted) outcomes)))
            (niwa-request-begin session)
            (setf (niwa-session-permission-queue session)
                  (list (list :kind 'generic
                              :tool-name "Read"
                              :session session
                              :callback
                              (lambda (outcome)
                                (push (cons 'permission outcome) outcomes)))))
            (setf (niwa-session-pending-plan-approval session)
                  (list :body "# Plan"
                        :chat-buffer (current-buffer)
                        :session session
                        :callback
                        (lambda (outcome)
                          (push (cons 'plan outcome) outcomes))))
            (niwa-abort (current-buffer))
            (should (null (niwa-session-permission-queue session)))
            (should (null (niwa-session-pending-plan-approval session)))
            (should (null niwa--current-request))
            (should (= 1 (length
                          (niwa-session-pending-reminders session))))
            (should (equal '((compaction . aborted)
                             (plan . aborted) (permission . aborted))
                           outcomes))))
      (delete-directory root t)))

  :doc "saves data buffer after abort teardown"
  (let ((root (file-name-as-directory
               (make-temp-file "niwa-chat-abort-save-" t))))
    (unwind-protect
        (with-temp-buffer
          (let* ((workspace (niwa-workspace--create
                             :type 'project :id root :root root
                             :name "abort-save"))
                 (session (niwa-session-create "main" workspace))
                 (goal (niwa-goal--create
                        :id "g1" :objective "Ship"
                        :status 'active :tokens-used 0 :reason-diagnostic t
                        :time-used-seconds 0 :turns-run 0))
                 saved)
            (setf (niwa-session-goal session) goal)
            (setq-local niwa--session session)
            (insert "Ship the requested change\n")
            (niwa-request-begin session)
            (cl-letf (((symbol-function
                        'niwa-session-artifacts-save)
                       (lambda (s b &rest _)
                         (setq saved
                               (list s b niwa--current-request))
                         "saved")))
              (niwa-abort (current-buffer)))
            (should (equal (list session (current-buffer) nil)
                           saved))
            (should (null niwa--current-request))
            (should (eq 'paused (niwa-goal-status goal)))
            (should (equal "interrupted by user"
                           (niwa-goal-reason goal)))
            (should-not (niwa-goal-reason-diagnostic goal))))
      (delete-directory root t)))

		 :doc "idles a root roster left running by a lost request teardown"
                 (with-temp-buffer
                   (let* ((workspace (niwa-workspace--create
                                      :type 'project
                                      :id "/tmp/niwa-chat-abort-roster/"
                                      :root "/tmp/niwa-chat-abort-roster/"
                                      :name "abort-roster"))
                          (session (niwa-session-create "main" workspace)))
                     (setq-local niwa--session session)
                     ;; The request vanished without its own teardown, so
                     ;; nothing idled the roster and the view keeps spinning.
                     (setq-local niwa--current-request nil)
                     (setf (niwa-session-agent-root-activity session)
                           'running)
                     (cl-letf (((symbol-function
                                 'niwa-session-artifacts-save)
                                (lambda (&rest _) "saved")))
                       (niwa-abort (current-buffer)))
                     (should (eq 'idle
                                 (niwa-session-agent-root-activity
                                  session)))))

		 :doc "does not save or warn during read-only inspection cleanup"
		 (with-temp-buffer
		   (let* ((workspace
			   (niwa-workspace--create
			    :type 'project
			    :id "/tmp/niwa-chat-abort-inspection/"
			    :root "/tmp/niwa-chat-abort-inspection/"
			    :name "abort-inspection"))
			  (session (niwa-session-create "main" workspace))
			  (save-count 0))
		     (setq-local niwa--session session)
		     (setq-local niwa-session--read-only-mode t)
		     (cl-letf
                         (((symbol-function 'niwa-session-artifacts-save)
			   (lambda (&rest _)
			     (cl-incf save-count)))
			  ((symbol-function 'display-warning)
			   (lambda (&rest _)
			     (ert-fail "Read-only inspection cleanup warned"))))
		       (niwa-abort (current-buffer)))
		     (should (= 0 save-count))))

  :doc "does not rewrite a Goal already paused for another reason"
  (let ((root (file-name-as-directory
               (make-temp-file "niwa-chat-abort-paused-" t))))
    (unwind-protect
        (with-temp-buffer
          (let* ((workspace (niwa-workspace--create
                             :type 'project :id root :root root :name "paused"))
                 (goal (niwa-goal--create
                        :id "g1" :objective "Ship" :status 'paused
                        :reason "Waiting for review"))
                 (session (niwa-session-create "main" workspace)))
            (setf (niwa-session-goal session) goal)
            (setq-local niwa--session session)
            (niwa-request-begin session)
            (cl-letf (((symbol-function 'niwa-session-artifacts-save)
                       (lambda (&rest _) "saved")))
              (niwa-abort (current-buffer)))
            (should (eq 'paused (niwa-goal-status goal)))
            (should (equal "Waiting for review"
                           (niwa-goal-reason goal)))))
      (delete-directory root t)))

				 :doc "root abort leaves retained agent turns independent"
			 (with-temp-buffer
			   (let* ((workspace (niwa-workspace--create
					      :type 'project
					      :id "/tmp/niwa-chat-abort-agents/"
					      :root "/tmp/niwa-chat-abort-agents/"
					      :name "abort-agents"))
				  (session (niwa-session-create "main" workspace))
				  (invocation (niwa-agent-invocation--create
					       :path "/root/explorer"
					       :description "retained"
					       :transcript-status 'running))
				  (record (niwa-agent-record--create
					   :id "explorer-1"
					   :path "/root/explorer"
					   :parent-path "/root"
					   :role "explorer"
					   :conversation-location
					   "agents/explorer-1.chat.org"
					   :configuration
					   (niwa-agent-configuration--create
					    :agent (niwa-agent--create
						    :name "explorer"
						    :description "retained"
						    :tools nil))
					   :activity 'running
					   :invocation invocation))
				  interrupted
				  (gptel--request-alist nil))
			     (setq-local niwa--session session)
			     (setf (niwa-session-agent-registry session)
				   (list (cons "/root/explorer" record)))
			     (cl-letf (((symbol-function 'niwa-agent-runtime-interrupt)
					(lambda (&rest _) (setq interrupted t)))
				       ((symbol-function 'niwa-session-artifacts-save)
					#'ignore))
			       (niwa-abort (current-buffer)))
			     (should-not interrupted)
			     (should-not
			      (niwa-session-pending-reminders session))
			     (should (eq 'running
					 (niwa-agent-record-activity record))))))


;;
;;; Plan implementation permission mode

(niwa-deftest niwa--generate-final-patch ()
  ,test
  (test)
  :doc "uses the active request snapshots in deterministic path order"
  (let* ((root (make-temp-file "niwa-final-patch-" t))
         (workspace (niwa-workspace--create
                     :type 'project :id root :root root :name "patch"))
         (a-file (file-name-concat root "a.txt"))
         (directory (file-name-concat root "created-directory"))
         (z-file (file-name-concat root "z.txt"))
         (snapshots (make-hash-table :test #'equal))
         (niwa--current-request
          (niwa-request--create :file-snapshots snapshots)))
    (unwind-protect
        (progn
          (puthash z-file nil snapshots)
          (puthash a-file "old\n" snapshots)
          (puthash directory nil snapshots)
          (make-directory directory)
          (with-temp-file a-file (insert "new\n"))
          (with-temp-file z-file (insert "created\n"))
          (let* ((patch nil)
                 (_ (niwa--generate-final-patch
                     workspace niwa--current-request
                     (lambda (value error)
                       (should-not error)
                       (setq patch value))))
                 (_ (with-timeout (10 (ert-fail "Final patch never settled"))
                      (while (not patch) (accept-process-output nil 0.02))))
                 (a-pos (string-search "diff --git a/a.txt" patch))
                 (z-pos (string-search "diff --git a/z.txt" patch)))
            (should a-pos)
            (should z-pos)
            (should (< a-pos z-pos))
            (should (string-match-p "new file mode 100644" patch))))
      (delete-directory root t))))


(provide 'test-niwa-chat)
;;; test-niwa-chat.el ends here
