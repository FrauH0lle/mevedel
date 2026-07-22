;;; test-mevedel-plan-mode.el --- Tests for mevedel-plan-mode.el -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'mevedel-plan)
(require 'mevedel-plan-mode)
(require 'mevedel-interaction-prompt)
(require 'mevedel-prompt-submission)
(require 'mevedel-session-persistence)
(require 'mevedel-structs)
(require 'mevedel-view)
(require 'mevedel-view-composer)
(require 'mevedel-view-interaction)
(require 'mevedel-worktree)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-compact)

(mevedel-deftest mevedel-plan-mode-active-p
  (:doc "reads Plan state from the explicit or current session")
  ,test
  (test)
  (let ((session (mevedel-session--create :name "test" :plan-mode t)))
    (should (mevedel-plan-mode-active-p session))
    (let ((mevedel--session session))
      (should (mevedel-plan-mode-active-p)))))

(mevedel-deftest mevedel-plan-mode--context-description
  (:doc "discloses the extra request required by Summary context")
  ,test
  (test)
  (should (string-match-p
           "additional model request"
           (mevedel-plan-mode--context-description 'summary))))

(mevedel-deftest mevedel-plan-mode-enter
  (:doc "enters Plan without changing the underlying permission mode")
  ,test
  (test)
  (let ((session (mevedel-session--create
                  :name "test" :permission-mode 'full-auto)))
    (mevedel-plan-mode-enter session)
    (should (mevedel-session-plan-mode session))
    (should (eq 'full-auto (mevedel-session-permission-mode session))))

  :doc "rejects every unfinished Goal status without changing state"
  (dolist (status '(active paused blocked))
    (let ((session
           (mevedel-session--create
            :name "test" :permission-mode 'auto
            :goal (mevedel-goal--create :status status))))
      (should-error (mevedel-plan-mode-enter session) :type 'user-error)
      (should-not (mevedel-session-plan-mode session))
      (should (eq 'auto (mevedel-session-permission-mode session)))))

  :doc "allows a completed Goal to remain as history"
  (let ((session
         (mevedel-session--create
          :name "test" :goal (mevedel-goal--create :status 'complete))))
    (should (mevedel-plan-mode-enter session))))

(mevedel-deftest mevedel-plan-mode-exit
  (:doc "leaves Plan without changing the underlying permission mode")
  ,test
  (test)
  (let ((session (mevedel-session--create
                  :name "test" :permission-mode 'auto :plan-mode t)))
    (mevedel-plan-mode-exit session)
    (should-not (mevedel-session-plan-mode session))
    (should (eq 'auto (mevedel-session-permission-mode session))))

  :doc "cancels a proposal into a draft and discards its selection"
  (let* ((selection '(:location here :context current
                      :execution direct :mode auto))
         (session (mevedel-session--create
                   :name "test" :plan-mode t
                   :plan-metadata
                   (list :status 'proposed :proposal-id '(1 2 "h")
                         :selection selection)))
         outcome
         (entry (list :session session
                      :callback (lambda (value) (setq outcome value)))))
    (setf (mevedel-session-pending-plan-approval session) entry)
    (mevedel-plan-mode-exit session)
    (let ((metadata (mevedel-session-plan-metadata session)))
      (should-not (mevedel-session-plan-mode session))
      (should-not (mevedel-session-pending-plan-approval session))
      (should (eq 'draft (plist-get metadata :status)))
      (should-not (plist-member metadata :proposal-id))
      (should-not (plist-member metadata :selection))
      (should (eq 'plan-exit outcome)))))

(mevedel-deftest mevedel-plan-mode--default-selection
  (:doc "defaults to Here, Current, Direct, and the underlying mode")
  ,test
  (test)
  (let ((session (mevedel-session--create
                  :name "test" :permission-mode 'auto)))
    (should (equal '(:location here :context current
                     :execution direct :mode auto)
                   (mevedel-plan-mode--default-selection session)))
    (should (eq 'auto (mevedel-session-permission-mode session)))))

(mevedel-deftest mevedel-plan-mode--invalidate-proposal
  (:doc "demotes an actionable proposal while preserving its selection")
  ,test
  (test)
  (let* ((selection '(:location here :context current
                      :execution direct :mode auto))
         (session (mevedel-session--create
                   :name "test" :plan-mode t
                   :plan-metadata
                   (list :status 'proposed :proposal-id '(1 2 "h")
                         :selection selection)))
         outcome
         (entry (list :session session
                      :callback (lambda (value) (setq outcome value)))))
    (setf (mevedel-session-pending-plan-approval session) entry)
    (should (mevedel-plan-mode--invalidate-proposal session))
    (let ((metadata (mevedel-session-plan-metadata session)))
      (should (eq 'draft (plist-get metadata :status)))
      (should (equal selection (plist-get metadata :selection)))
      (should-not (plist-member metadata :proposal-id))
      (should-not (mevedel-session-pending-plan-approval session))
      (should (eq 'invalidated outcome)))))

(mevedel-deftest mevedel-plan-mode--render-approval
  (:doc "renders compact Direct axes without applying Mode before acceptance")
  ,test
  (test)
  (let* ((session (mevedel-session--create
                   :name "test" :permission-mode 'auto))
         (data-buffer (generate-new-buffer " *plan-approval-data*"))
         (view-buffer (generate-new-buffer " *plan-approval-view*"))
         (selection (mevedel-plan-mode--default-selection session))
         (entry (mevedel-plan-mode--approval-entry
                 "# Plan" data-buffer session selection))
         descriptor outcome rerendered)
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel-view--interaction-target-buffer)
                   (lambda (_buffer) view-buffer))
                  ((symbol-function 'mevedel-view--fontify-as)
                   (lambda (text _mode) text))
                  ((symbol-function 'mevedel-view--interaction-register)
                   (lambda (value)
                     (setq descriptor value)
                     (make-overlay (point-min) (point-min))))
                  ((symbol-function 'mevedel--prompt--settle)
                   (lambda (_overlay value) (setq outcome value)))
                  ((symbol-function 'mevedel-plan-approval-render)
                   (lambda (&rest _) (setq rerendered t)))
                  ((symbol-function 'mevedel-plan-mode--read-worktree-branch)
                   (lambda (_entry) "plan/topic")))
          (mevedel-plan-mode--render-approval entry)
          (let ((body (plist-get descriptor :body))
                (keymap (plist-get descriptor :keymap)))
            (dolist (text '("Location   Here" "Context    Current"
                            "Execution  Direct" "Mode       auto"))
              (should (string-match-p text body)))
            (call-interactively (lookup-key keymap (kbd "c")))
            (should (eq 'fresh (plist-get selection :context)))
            (call-interactively (lookup-key keymap (kbd "c")))
            (should (eq 'summary (plist-get selection :context)))
            (should (string-match-p
                     "accepted plan"
                     (mevedel-plan-mode--context-description 'summary)))
            (call-interactively (lookup-key keymap (kbd "l")))
            (should (eq 'worktree (plist-get selection :location)))
            (should (eq 'summary (plist-get selection :context)))
            (call-interactively (lookup-key keymap (kbd "c")))
            (should (eq 'fresh (plist-get selection :context)))
            (call-interactively (lookup-key keymap (kbd "c")))
            (should (eq 'summary (plist-get selection :context)))
            (call-interactively (lookup-key keymap (kbd "l")))
            (should (eq 'here (plist-get selection :location)))
            (should (eq 'summary (plist-get selection :context)))
            (call-interactively (lookup-key keymap (kbd "l")))
            (call-interactively (lookup-key keymap (kbd "c")))
            (call-interactively (lookup-key keymap (kbd "m")))
            (should rerendered)
            (should (eq 'full-auto (plist-get selection :mode)))
            (should (eq 'auto (mevedel-session-permission-mode session)))
            (call-interactively (lookup-key keymap (kbd "RET")))
            (should (equal '(:accept t
                             :selection (:location worktree :context fresh
                                         :execution direct :mode full-auto
                                         :branch "plan/topic"))
                           outcome))))
      (when (buffer-live-p view-buffer) (kill-buffer view-buffer))
      (when (buffer-live-p data-buffer) (kill-buffer data-buffer))))

  :doc "warns for dirty Worktree state and cancellation keeps approval pending"
  (let* ((session (mevedel-session--create
                   :name "test" :working-directory default-directory))
         (data-buffer (generate-new-buffer " *plan-dirty-data*"))
         (view-buffer (generate-new-buffer " *plan-dirty-view*"))
         (selection '(:location worktree :context fresh
                      :execution direct :mode ask))
         (entry (mevedel-plan-mode--approval-entry
                 "# Plan" data-buffer session selection))
         descriptor outcome)
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel-view--interaction-target-buffer)
                   (lambda (_buffer) view-buffer))
                  ((symbol-function 'mevedel-view--interaction-register)
                   (lambda (value)
                     (setq descriptor value)
                     (make-overlay (point-min) (point-min))))
                  ((symbol-function 'mevedel--prompt--settle)
                   (lambda (_overlay value) (setq outcome value)))
                  ((symbol-function 'mevedel-worktree--collect-status)
                   (lambda (&optional _) '(:dirty-p t)))
                  ((symbol-function 'read-string)
                   (lambda (&rest _) (signal 'quit nil))))
          (mevedel-plan-mode--render-approval entry)
          (should (string-match-p
                   "Worktree starts at HEAD; uncommitted changes are not included\\."
                   (plist-get descriptor :body)))
          (should
           (eq 'quit
               (condition-case nil
                   (call-interactively
                    (lookup-key (plist-get descriptor :keymap) (kbd "RET")))
                 (quit 'quit))))
          (should-not outcome))
      (when (buffer-live-p view-buffer) (kill-buffer view-buffer))
      (when (buffer-live-p data-buffer) (kill-buffer data-buffer)))))

(mevedel-deftest mevedel-plan-mode--feedback-draft
  (:doc "replaces the composer with an editable replacement-plan request")
  ,test
  (test)
  (mevedel-view-test--with-buffers
    (let ((session
           (mevedel-session--create
            :name "test" :plan-mode t
            :plan-metadata '(:path "plans/current.md" :status draft))))
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (with-current-buffer view-buf
        (setq-local mevedel--session session)
        (mevedel-view-test--insert-composer-draft "old draft"))
      (mevedel-plan-mode--feedback-draft data-buf session)
      (with-current-buffer view-buf
        (let ((draft (mevedel-view--input-text)))
          (should (string-match-p "Plan feedback:" draft))
          (should (string-match-p "complete replacement" draft))
          (should (string-match-p "plans/current.md" draft))
          (should-not (string-match-p "old draft" draft)))))))

(mevedel-deftest mevedel-plan-mode--read-worktree-branch
  (:doc "collects the generated default and validates before acceptance")
  ,test
  (test)
  (let* ((directory (file-name-as-directory default-directory))
         (session (mevedel-session--create
                   :name "source" :working-directory directory))
         (entry (list :session session))
         validated)
    (cl-letf (((symbol-function 'read-string)
               (lambda (_prompt _initial _history default &rest _)
                 (should (equal "worktree/accepted-plan" default))
                 "plan/topic"))
              ((symbol-function 'mevedel-worktree--validate-branch-name)
               (lambda (branch source-directory)
                 (setq validated (list branch source-directory)))))
      (should (equal "plan/topic"
                     (mevedel-plan-mode--read-worktree-branch entry)))
      (should (equal (list "plan/topic" directory) validated)))))

(mevedel-deftest mevedel-plan-mode--post-response
  (:doc "only root-assistant prose creates one proposal per rendered turn")
  ,test
  (test)
  (let* ((save-dir (make-temp-file "mevedel-plan-proposal-" t))
         (session (mevedel-session--create
                   :name "test" :save-path save-dir :plan-mode t
                   :plan-metadata
                   '(:selection (:location here :context current
                                 :execution direct :mode auto)))))
    (unwind-protect
        (with-temp-buffer
          (setq-local mevedel--session session)
          (let ((start (point)))
            (insert "<proposed_plan>\n# Root\n<detail>keep</detail>\n</proposed_plan>\n")
            (add-text-properties start (point) '(gptel response)))
          (let ((start (point)))
            (insert "<proposed_plan>\n# Tool\n</proposed_plan>\n")
            (add-text-properties start (point) '(gptel (tool . "call-1"))))
          (cl-letf (((symbol-function 'mevedel-plan-approval-render) #'ignore))
            (mevedel-plan-mode--post-response (point-min) (point-max))
            (let ((first
                   (mevedel-session-pending-plan-approval session)))
              (mevedel-plan-mode--post-response (point-min) (point-max))
              (should (eq first
                          (mevedel-session-pending-plan-approval session)))
              (let ((later-start (point)))
                (insert "<proposed_plan>\n# Root\n<detail>keep</detail>\n</proposed_plan>\n")
                (add-text-properties later-start (point) '(gptel response))
                (mevedel-plan-mode--post-response later-start (point)))
              (should-not
               (eq first (mevedel-session-pending-plan-approval session)))))
          (should (equal "# Root\n<detail>keep</detail>"
                         (plist-get
                          (mevedel-session-pending-plan-approval session)
                          :body)))
          (should (equal '(:location here :context current
                           :execution direct :mode auto)
                         (plist-get
                          (mevedel-session-pending-plan-approval session)
                          :selection)))
          (should (equal 'proposed
                         (plist-get (mevedel-session-plan-metadata session)
                                    :status))))
      (delete-directory save-dir t)))

  :doc "tool output alone cannot create a proposal"
  (let ((session (mevedel-session--create :name "test" :plan-mode t))
        presented)
    (with-temp-buffer
      (setq-local mevedel--session session)
      (insert "<proposed_plan>\n# Tool only\n</proposed_plan>")
      (add-text-properties (point-min) (point-max)
                           '(gptel (tool . "call-1")))
      (cl-letf (((symbol-function 'mevedel-plan-approval-present)
                 (lambda (&rest _) (setq presented t))))
        (mevedel-plan-mode--post-response (point-min) (point-max)))
      (should-not presented)))

  :doc "injected agent output cannot create a proposal"
  (let ((session (mevedel-session--create :name "test" :plan-mode t))
        presented)
    (with-temp-buffer
      (insert "<agent-result sender=\"/root/worker\" recipient=\"/root\">\n"
              "<proposed_plan>\n# Agent only\n</proposed_plan>\n"
              "</agent-result>\n")
      (add-text-properties (point-min) (point-max) '(gptel response))
      (setq-local mevedel--session session)
      (cl-letf (((symbol-function 'mevedel-plan-approval-present)
                 (lambda (&rest _) (setq presented t))))
        (mevedel-plan-mode--post-response (point-min) (point-max)))
      (should-not presented))))

(mevedel-deftest mevedel-plan-mode-restore-pending-approval
  (:doc "restores a matching durable proposal without changing the composer")
  ,test
  (test)
  (let* ((save-dir (make-temp-file "mevedel-plan-restore-" t))
         (path (file-name-concat save-dir "plans" "current.md"))
         (plan "# Restored plan")
         (hash (mevedel-plan-hash plan))
         (selection '(:location here :context current
                      :execution direct :mode auto))
         (session
          (mevedel-session--create
           :name "test" :save-path save-dir :plan-mode t
           :plan-metadata
           (list :path "plans/current.md" :hash hash :status 'proposed
                 :proposal-id (list 10 20 hash) :selection selection))))
    (unwind-protect
        (progn
          (make-directory (file-name-directory path) t)
          (write-region plan nil path nil 'silent)
          (mevedel-view-test--with-buffers
            (with-current-buffer data-buf
              (setq-local mevedel--session session))
            (with-current-buffer view-buf
              (setq-local mevedel--session session)
              (mevedel-view-test--insert-composer-draft
               "> first\nsecond"))
            (with-current-buffer data-buf
              (should (mevedel-plan-mode-restore-pending-approval
                       session data-buf)))
            (should (equal plan
                           (plist-get
                            (mevedel-session-pending-plan-approval session)
                            :body)))
            (should (equal selection
                           (plist-get
                            (mevedel-session-pending-plan-approval session)
                            :selection)))
            (with-current-buffer view-buf
              (should (equal "> first\nsecond"
                             (mevedel-view--input-text))))))
      (delete-directory save-dir t)))

  :doc "demotes a proposed artifact whose durable identity no longer agrees"
  (let* ((save-dir (make-temp-file "mevedel-plan-restore-bad-" t))
         (path (file-name-concat save-dir "plans" "current.md"))
         (hash (mevedel-plan-hash "# Original"))
         (session
          (mevedel-session--create
           :name "test" :save-path save-dir :plan-mode t
           :plan-metadata
           (list :path "plans/current.md" :hash hash :status 'proposed
                 :proposal-id (list 1 2 hash)
                 :selection '(:location here :context current
                              :execution direct :mode ask)))))
    (unwind-protect
        (progn
          (make-directory (file-name-directory path) t)
          (write-region "# Tampered" nil path nil 'silent)
          (should-not
           (mevedel-plan-mode-restore-pending-approval session))
          (should (eq 'draft
                      (plist-get (mevedel-session-plan-metadata session)
                                 :status)))
          (should-not
           (plist-member (mevedel-session-plan-metadata session)
                         :proposal-id)))
      (delete-directory save-dir t))))

(mevedel-deftest mevedel-plan-mode--approval-callback
  (:doc "accepts immutably and starts one canonical Direct turn")
  ,test
  (test)
  (let* ((save-dir (make-temp-file "mevedel-plan-direct-" t))
         (session (mevedel-session--create
                   :name "test" :save-path save-dir
                   :permission-mode 'auto :plan-mode t))
         (data-buffer (generate-new-buffer " *plan-direct-data*"))
         (view-buffer (generate-new-buffer " *plan-direct-view*"))
         hook-input implementation)
    (unwind-protect
        (progn
          (with-current-buffer data-buffer
            (setq-local mevedel--session session
                        mevedel-permission-mode 'auto))
          (cl-letf (((symbol-function 'mevedel-view--interaction-target-buffer)
                     (lambda (_buffer) view-buffer))
                    ((symbol-function 'mevedel-view--run-prompt-submit-hook)
                     (lambda (input _display callback &rest _)
                       (setq hook-input input)
                       (funcall callback
                                (mevedel-prompt-submission-create
                                 :input input :state 'committed))))
                    ((symbol-function 'mevedel-plan-mode--persist) #'ignore)
                    ((symbol-function 'mevedel--implement-plan)
                     (lambda (action) (setq implementation action))))
            (mevedel-plan-mode--approval-callback
             "# Accepted\n\nDo it." data-buffer session
             '(:accept t
               :selection (:location here :context current
                           :execution direct :mode full-auto))))
          (let* ((metadata (mevedel-session-plan-metadata session))
                 (accepted (plist-get metadata :accepted-absolute-path)))
            (should-not (mevedel-session-plan-mode session))
            (should (eq 'full-auto
                        (mevedel-session-permission-mode session)))
            (should (eq 'accepted (plist-get metadata :status)))
            (should-not (plist-member metadata :verification-pending))
            (should (file-exists-p accepted))
            (should (string-match-p (regexp-quote accepted) hook-input))
            (should (string-match-p "# Accepted" hook-input))
            (should (eq 'current (plist-get implementation :context)))
            (should (equal accepted
                           (plist-get implementation :plan-file)))
            (should-not (plist-member metadata :implementation-retry))
            (should-not (mevedel-session-goal session))))
      (when (buffer-live-p view-buffer) (kill-buffer view-buffer))
      (when (buffer-live-p data-buffer) (kill-buffer data-buffer))
      (delete-directory save-dir t)))

  :doc "Worktree acceptance preserves the source Mode and validated branch"
  (let* ((save-dir (make-temp-file "mevedel-plan-worktree-accept-" t))
         (session (mevedel-session--create
                   :name "source" :save-path save-dir
                   :permission-mode 'ask :plan-mode t))
         (data-buffer (generate-new-buffer " *plan-worktree-accept*"))
         dispatched)
    (unwind-protect
        (progn
          (with-current-buffer data-buffer
            (setq-local mevedel--session session))
          (cl-letf (((symbol-function 'mevedel-plan-mode--persist) #'ignore)
                    ((symbol-function 'mevedel-plan-mode--dispatch-accepted)
                     (lambda (_session _buffer) (setq dispatched t)))
                    ((symbol-function 'mevedel-permission-mode-transition)
                     (lambda (_mode)
                       (ert-fail "Worktree acceptance changed source Mode"))))
            (mevedel-plan-mode--approval-callback
             "# Accepted" data-buffer session
             '(:accept t
               :selection (:location worktree :context fresh
                           :execution direct :mode full-auto
                           :branch "plan/topic"))))
          (let ((retry
                 (plist-get (mevedel-session-plan-metadata session)
                            :implementation-retry)))
            (should dispatched)
            (should (eq 'ask (mevedel-session-permission-mode session)))
            (should (eq 'prepare-worktree (plist-get retry :step)))
            (should (equal "plan/topic"
                           (plist-get (plist-get retry :selection)
                                      :branch)))))
      (when (buffer-live-p data-buffer) (kill-buffer data-buffer))
      (delete-directory save-dir t)))

  :doc "feedback preserves Plan and selection while requiring a replacement"
  (let* ((selection '(:location here :context current
                      :execution direct :mode auto))
         (session (mevedel-session--create
                   :name "test" :plan-mode t
                   :plan-metadata
                   (list :status 'proposed :proposal-id '(1 2 "h")
                         :selection selection)))
         drafted)
    (cl-letf (((symbol-function 'mevedel-plan-mode--feedback-draft)
               (lambda (&rest _) (setq drafted t))))
      (mevedel-plan-mode--approval-callback
       "# Plan" (current-buffer) session 'feedback-draft))
    (let ((metadata (mevedel-session-plan-metadata session)))
      (should (mevedel-session-plan-mode session))
      (should drafted)
      (should (eq 'draft (plist-get metadata :status)))
      (should (equal selection (plist-get metadata :selection)))
      (should-not (plist-member metadata :proposal-id))))

  :doc "cancellation retains a draft but discards approval selection"
  (let* ((session (mevedel-session--create
                   :name "test" :plan-mode t
                   :plan-metadata
                   '(:status proposed :proposal-id (1 2 "h")
                     :selection (:location here :context current
                                 :execution direct :mode ask)))))
    (mevedel-plan-mode--approval-callback
     "# Plan" (current-buffer) session 'aborted)
    (let ((metadata (mevedel-session-plan-metadata session)))
      (should (mevedel-session-plan-mode session))
      (should (eq 'draft (plist-get metadata :status)))
      (should-not (plist-member metadata :selection)))))

(mevedel-deftest mevedel-plan-mode--dispatch-accepted
  (:doc "Here/Fresh rotates once, runs setup hooks, and retries only submission")
  ,test
  (test)
  (let* ((save-dir (make-temp-file "mevedel-plan-fresh-" t))
         (accepted-path (file-name-concat save-dir "plans" "accepted.md"))
         (current-path (file-name-concat save-dir "plans" "current.md"))
         (body "# Immutable accepted plan")
         (selection '(:location here :context fresh
                      :execution direct :mode full-auto))
         (record
          (list :step 'prepare-context :selection selection
                :accepted-path "plans/accepted.md"
                :accepted-absolute-path accepted-path
                :accepted-hash (mevedel-plan-hash body)))
         (session
          (mevedel-session--create
           :name "test" :save-path save-dir :permission-mode 'full-auto
           :plan-metadata
           (list :status 'accepted :implementation-retry record)))
         (data-buffer (generate-new-buffer " *plan-fresh-data*"))
         (view-buffer (generate-new-buffer " *plan-fresh-view*"))
         (rotations 0)
         (attempts 0)
         hook-source
         prompts)
    (unwind-protect
        (progn
          (make-directory (file-name-directory accepted-path) t)
          (write-region body nil accepted-path nil 'silent)
          (write-region "# Mutable replacement" nil current-path nil 'silent)
          (with-current-buffer data-buffer
            (setq-local mevedel--session session
                        gptel-prompt-prefix-alist '((fundamental-mode . "> "))))
          (cl-letf (((symbol-function 'mevedel-plan-mode--persist) #'ignore)
                    ((symbol-function
                      'mevedel-session-persistence-start-fresh-segment)
                     (lambda (&rest _)
                       (cl-incf rotations)
                       "segment-0002.chat.org"))
                    ((symbol-function 'mevedel--run-session-start-hooks)
                     (lambda (source) (setq hook-source source)))
                    ((symbol-function 'mevedel-view--interaction-target-buffer)
                     (lambda (_buffer) view-buffer))
                    ((symbol-function 'mevedel-view--run-prompt-submit-hook)
                     (lambda (input _display callback &rest _)
                       (push input prompts)
                       (funcall callback
                                (mevedel-prompt-submission-create
                                 :input input :state 'committed))))
                    ((symbol-function 'mevedel--implement-plan)
                     (lambda (action)
                       (cl-incf attempts)
                       (should (eq 'current (plist-get action :context)))
                       (should (eq 'full-auto
                                   (plist-get action :permission-mode)))
                       (should (equal accepted-path
                                      (plist-get action :plan-file)))
                       (when (= attempts 1)
                         (error "Transport refused"))
                       'started)))
            (mevedel-plan-mode--dispatch-accepted session data-buffer)
            (let ((retry
                   (plist-get (mevedel-session-plan-metadata session)
                              :implementation-retry)))
              (should (= 1 rotations))
              (should (equal "clear" hook-source))
              (should (eq 'submit (plist-get retry :step)))
              (should (equal "Transport refused" (plist-get retry :failure)))
              (should (eq 'accepted
                          (plist-get (mevedel-session-plan-metadata session)
                                     :status))))
            (mevedel-retry-plan-implementation session data-buffer)
            (should (= 1 rotations))
            (should (= 2 attempts))
            (should-not
             (plist-member (mevedel-session-plan-metadata session)
                           :implementation-retry))
            (dolist (prompt prompts)
              (should (string-match-p "# Immutable accepted plan" prompt))
              (should-not (string-match-p "Mutable replacement" prompt)))))
      (when (buffer-live-p view-buffer) (kill-buffer view-buffer))
      (when (buffer-live-p data-buffer) (kill-buffer data-buffer))
      (delete-directory save-dir t)))

  :doc "a failed Fresh rotation remains retryable at the preparation step"
  (let* ((record
          '(:step prepare-context
            :selection (:location here :context fresh
                        :execution direct :mode auto)
            :accepted-path "plans/accepted.md"
            :accepted-absolute-path "/tmp/accepted.md"
            :accepted-hash "hash"))
         (session
          (mevedel-session--create
           :name "test"
           :plan-metadata
           (list :status 'accepted :implementation-retry record)))
         (data-buffer (generate-new-buffer " *plan-fresh-failure*")))
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel-plan-mode--persist) #'ignore)
                  ((symbol-function
                    'mevedel-session-persistence-start-fresh-segment)
                   (lambda (&rest _) (error "Rotation failed"))))
          (mevedel-plan-mode--dispatch-accepted session data-buffer)
          (let ((retry
                 (plist-get (mevedel-session-plan-metadata session)
                            :implementation-retry)))
            (should (eq 'prepare-context (plist-get retry :step)))
            (should (equal "Rotation failed" (plist-get retry :failure)))))
      (when (buffer-live-p data-buffer) (kill-buffer data-buffer))))

  :doc "Here/Summary caches one zero-tail compaction across dispatch retry"
  (let* ((save-dir (make-temp-file "mevedel-plan-summary-" t))
         (accepted-path (file-name-concat save-dir "plans" "accepted.md"))
         (body "# Accepted plan\n\nImplement the endpoint.")
         (selection '(:location here :context summary
                      :execution direct :mode auto))
         (record
          (list :step 'prepare-summary :selection selection
                :accepted-path "plans/accepted.md"
                :accepted-absolute-path accepted-path
                :accepted-hash (mevedel-plan-hash body)))
         (session
          (mevedel-session--create
           :name "test" :save-path save-dir
           :plan-metadata
           (list :status 'accepted :implementation-retry record)))
         (data-buffer (generate-new-buffer " *plan-summary-data*"))
         (view-buffer (generate-new-buffer " *plan-summary-view*"))
         (summary-runs 0)
         (compact-runs 0)
         (apply-attempts 0)
         (attempts 0)
         anchored-summary
         persisted
         prompts)
    (unwind-protect
        (progn
          (make-directory (file-name-directory accepted-path) t)
          (write-region body nil accepted-path nil 'silent)
          (cl-letf (((symbol-function 'mevedel-plan-mode--persist)
                     (lambda (&rest _)
                       (push (copy-tree
                              (mevedel-session-plan-metadata session))
                             persisted)))
                    ((symbol-function 'mevedel--compact-main-target)
                     (lambda ()
                       (list :apply
                             (lambda (_target summary &rest _)
                               (cl-incf apply-attempts)
                               (when (= apply-attempts 1)
                                 (error "Rotation failed"))
                               (setq anchored-summary summary)
                               t))))
                    ((symbol-function 'mevedel--compact-previous-summary)
                     (lambda () anchored-summary))
                    ((symbol-function 'mevedel--compact-run)
                     (lambda (&rest args)
                       (cl-incf compact-runs)
                       (should (plist-get args :aggressive))
                       (should (string-match-p
                                "Do not reproduce the accepted plan"
                                (plist-get args :instructions)))
                       (let* ((prepared (plist-get args :prepared-summary))
                              (summary
                               (or prepared
                                   (progn
                                     (cl-incf summary-runs)
                                     (concat
                                      "# Handoff\n\nDiscovery.\n\n" body))))
                              (summary
                               (funcall (plist-get args :summary-ready)
                                        summary))
                              (target (plist-get args :target)))
                         (condition-case err
                             (progn
                               (funcall (plist-get target :apply)
                                        target summary nil nil nil nil 0)
                               (funcall (plist-get args :callback) nil))
                           (error
                            (funcall (plist-get args :callback)
                                     (error-message-string err)))))))
                    ((symbol-function 'mevedel-view--interaction-target-buffer)
                     (lambda (_buffer) view-buffer))
                    ((symbol-function 'mevedel-view--run-prompt-submit-hook)
                     (lambda (input _display callback &rest _)
                       (push input prompts)
                       (funcall callback
                                (mevedel-prompt-submission-create
                                 :input input :state 'committed))))
                    ((symbol-function 'mevedel--implement-plan)
                     (lambda (_action)
                       (cl-incf attempts)
                       (when (= attempts 1)
                         (error "Request startup failed"))
                       'started)))
            (mevedel-plan-mode--dispatch-accepted session data-buffer)
            (let ((retry
                   (plist-get (mevedel-session-plan-metadata session)
                              :implementation-retry)))
              (should (= 1 summary-runs))
              (should (eq 'prepare-summary (plist-get retry :step)))
              (should (plist-get retry :summary))
              (should-not (string-search body (plist-get retry :summary)))
              (should
               (seq-some
                (lambda (metadata)
                  (let ((saved (plist-get metadata :implementation-retry)))
                    (and (eq 'prepare-summary (plist-get saved :step))
                         (plist-get saved :summary))))
                persisted)))
            (mevedel-retry-plan-implementation session data-buffer)
            (should (= 2 compact-runs))
            (should (= 1 summary-runs))
            (should (= 1 attempts))
            (should (equal anchored-summary
                           (plist-get
                            (plist-get (mevedel-session-plan-metadata session)
                                       :implementation-retry)
                            :summary)))
            (mevedel-retry-plan-implementation session data-buffer)
            (should (= 2 attempts))
            (should-not
             (plist-member (mevedel-session-plan-metadata session)
                           :implementation-retry))
            (dolist (prompt prompts)
              (let ((path-position
                     (string-search accepted-path prompt))
                    (plan-position (string-search body prompt))
                    (instruction-position
                     (string-search "Implementation instructions:" prompt)))
                (should path-position)
                (should plan-position)
                (should instruction-position)
                (should (< path-position plan-position instruction-position))
                (should-not (string-search anchored-summary prompt)))))
      (when (buffer-live-p view-buffer) (kill-buffer view-buffer))
      (when (buffer-live-p data-buffer) (kill-buffer data-buffer))
      (delete-directory save-dir t))))

  :doc "Worktree/Fresh prepares one target and reuses it after dispatch failure"
  (let* ((source-save (make-temp-file "mevedel-plan-worktree-source-" t))
         (target-save (make-temp-file "mevedel-plan-worktree-target-" t))
         (target-directory
          (file-name-as-directory
           (make-temp-file "mevedel-plan-worktree-checkout-" t)))
         (source-path (file-name-concat source-save "plans" "accepted.md"))
         (body "# Worktree plan\n\nImplement it.")
         (selection '(:location worktree :context fresh
                      :execution direct :mode full-auto
                      :branch "plan/topic"))
         (record
          (list :step 'prepare-worktree :selection selection
                :accepted-path "plans/accepted.md"
                :accepted-absolute-path source-path
                :accepted-hash (mevedel-plan-hash body)))
         (source-session
          (mevedel-session--create
           :name "source" :save-path source-save :permission-mode 'ask
           :preset-name 'source-preset
           :preset-settings '((mevedel-model-tiers . ((fast . source))))
           :plan-metadata
           (list :status 'accepted :implementation-retry record)))
         (target-session
          (mevedel-session--create
           :name "target" :session-id "target-id" :save-path target-save
           :working-directory target-directory :permission-mode 'ask))
         (source-buffer (generate-new-buffer " *plan-worktree-source*"))
         (target-buffer (generate-new-buffer " *plan-worktree-target*"))
         (view-buffer (generate-new-buffer " *plan-worktree-view*"))
         (archive-function (symbol-function 'mevedel-plan-archive-accepted))
         (creates 0)
         (archives 0)
         (attempts 0)
         prompts)
    (unwind-protect
        (progn
          (make-directory (file-name-directory source-path) t)
          (write-region body nil source-path nil 'silent)
          (with-current-buffer source-buffer
            (setq-local mevedel--session source-session))
          (with-current-buffer target-buffer
            (setq-local mevedel--session target-session))
          (cl-letf (((symbol-function 'mevedel-plan-mode--persist) #'ignore)
                    ((symbol-function 'mevedel-worktree-create-session)
                     (lambda (branch _purpose _clean)
                       (cl-incf creates)
                       (list :buffer target-buffer :branch branch
                             :directory target-directory)))
                    ((symbol-function
                      'mevedel-session-persistence-ensure-files)
                     (lambda (&rest _) target-save))
                    ((symbol-function 'mevedel-session-persistence-restore)
                     (lambda (_path) target-buffer))
                    ((symbol-function 'mevedel-plan-archive-accepted)
                     (lambda (&rest args)
                       (cl-incf archives)
                       (apply archive-function args)))
                    ((symbol-function 'mevedel-preset-restore-session)
                     (lambda (&rest _)))
                    ((symbol-function 'mevedel-permission-mode-transition)
                     (lambda (mode)
                       (setf (mevedel-session-permission-mode
                              (buffer-local-value
                               'mevedel--session (current-buffer)))
                             mode)))
                    ((symbol-function 'mevedel-view--interaction-target-buffer)
                     (lambda (buffer)
                       (should (eq target-buffer buffer))
                       view-buffer))
                    ((symbol-function 'mevedel-view--run-prompt-submit-hook)
                     (lambda (input _display callback &rest _)
                       (push input prompts)
                       (funcall callback
                                (mevedel-prompt-submission-create
                                 :input input :state 'committed))))
                    ((symbol-function 'mevedel--implement-plan)
                     (lambda (action)
                       (should (eq target-buffer (current-buffer)))
                       (should (equal
                                (file-name-concat target-save
                                                  "plans" "accepted.md")
                                (plist-get action :plan-file)))
                       (cl-incf attempts)
                       (when (= attempts 1)
                         (error "Startup failed"))
                       'started)))
            (mevedel-plan-mode--dispatch-accepted
             source-session source-buffer)
            (let* ((retry
                    (plist-get
                     (mevedel-session-plan-metadata source-session)
                     :implementation-retry))
                   (target-path
                    (plist-get retry :target-accepted-absolute-path)))
              (should (= 1 creates))
              (should (= 1 archives))
              (should (eq 'submit (plist-get retry :step)))
              (should (file-exists-p source-path))
              (should (equal
                       (with-temp-buffer
                         (insert-file-contents-literally source-path)
                         (buffer-string))
                       (with-temp-buffer
                         (insert-file-contents-literally target-path)
                         (buffer-string))))
              (should (equal
                       (mevedel-session-preset-settings source-session)
                       (mevedel-session-preset-settings target-session)))
              (should-not
               (eq (mevedel-session-preset-settings source-session)
                   (mevedel-session-preset-settings target-session)))
              (should (eq 'ask
                          (mevedel-session-permission-mode source-session)))
              (should (eq 'full-auto
                          (mevedel-session-permission-mode target-session))))
            (mevedel-retry-plan-implementation
             source-session source-buffer)
            (should (= 1 creates))
            (should (= 1 archives))
            (should (= 2 attempts))
            (should-not
             (plist-member (mevedel-session-plan-metadata source-session)
                           :implementation-retry))
            (dolist (prompt prompts)
              (should (string-match-p
                       (regexp-quote
                        (file-name-concat target-save
                                          "plans" "accepted.md"))
                       prompt)))))
      (dolist (buffer (list view-buffer target-buffer source-buffer))
        (when (buffer-live-p buffer) (kill-buffer buffer)))
      (delete-directory target-directory t)
      (delete-directory target-save t)
      (delete-directory source-save t)))

  :doc "Worktree/Summary preserves source and reuses every prepared handoff input"
  (let* ((source-save (make-temp-file "mevedel-plan-worktree-summary-source-" t))
         (target-save (make-temp-file "mevedel-plan-worktree-summary-target-" t))
         (source-directory
          (file-name-as-directory
           (make-temp-file "mevedel-plan-worktree-summary-checkout-" t)))
         (target-directory
          (file-name-as-directory
           (make-temp-file "mevedel-plan-worktree-summary-target-checkout-" t)))
         (source-path (file-name-concat source-save "plans" "accepted.md"))
         (body "# Worktree summary plan\n\nImplement it.")
         (selection '(:location worktree :context summary
                      :execution direct :mode auto :branch "plan/summary"))
         (record
          (list :step 'prepare-summary :selection selection
                :accepted-path "plans/accepted.md"
                :accepted-absolute-path source-path
                :accepted-hash (mevedel-plan-hash body)))
         (source-session
          (mevedel-session--create
           :name "source" :save-path source-save
           :working-directory source-directory :permission-mode 'ask
           :preset-name 'source-preset
           :preset-settings '((mevedel-model-tiers . ((fast . source))))
           :plan-metadata
           (list :status 'accepted :implementation-retry record)))
         (target-session
          (mevedel-session--create
           :name "target" :session-id "summary-target-id"
           :save-path target-save :working-directory target-directory
           :permission-mode 'ask))
         (source-buffer (generate-new-buffer " *plan-worktree-summary-source*"))
         (target-buffer (generate-new-buffer " *plan-worktree-summary-target*"))
         (view-buffer (generate-new-buffer " *plan-worktree-summary-view*"))
         (archive-function (symbol-function 'mevedel-plan-archive-accepted))
         (source-text "Planning transcript stays byte-for-byte.\n")
         (summary-runs 0)
         (creates 0)
         (archives 0)
         (attempts 0)
         clean
         prompts)
    (unwind-protect
        (progn
          (make-directory (file-name-directory source-path) t)
          (write-region body nil source-path nil 'silent)
          (with-current-buffer source-buffer
            (insert source-text)
            (setq-local mevedel--session source-session))
          (with-current-buffer target-buffer
            (setq-local mevedel--session target-session))
          (cl-letf
              (((symbol-function 'mevedel-plan-mode--persist) #'ignore)
               ((symbol-function 'mevedel--compact-main-target)
                (lambda ()
                  (list :apply (lambda (&rest _) (ert-fail "Source mutated"))
                        :begin-context-epoch t :warn-on-completion t)))
               ((symbol-function 'mevedel--compact-run)
                (lambda (&rest args)
                  (should (plist-get args :aggressive))
                  (should-not
                   (plist-get (plist-get args :target) :begin-context-epoch))
                  (let* ((prepared (plist-get args :prepared-summary))
                         (summary
                          (or prepared
                              (progn
                                (cl-incf summary-runs)
                                (format "# Handoff\n\nInspect %ssrc/api.el."
                                        source-directory))))
                         (summary
                          (funcall (plist-get args :summary-ready) summary))
                         (target (plist-get args :target)))
                    (funcall (plist-get target :apply)
                             target summary nil nil nil nil 0)
                    (funcall (plist-get args :callback) nil))))
               ((symbol-function 'mevedel-worktree-create-session)
                (lambda (branch _purpose clean-arg)
                  (cl-incf creates)
                  (setq clean clean-arg)
                  (list :buffer target-buffer :branch branch
                        :directory target-directory)))
               ((symbol-function 'mevedel-session-persistence-ensure-files)
                (lambda (&rest _) target-save))
               ((symbol-function 'mevedel-session-persistence-restore)
                (lambda (_path) target-buffer))
               ((symbol-function 'mevedel-plan-archive-accepted)
                (lambda (&rest args)
                  (cl-incf archives)
                  (apply archive-function args)))
               ((symbol-function 'mevedel-preset-restore-session)
                (lambda (&rest _)))
               ((symbol-function 'mevedel-permission-mode-transition)
                (lambda (mode)
                  (setf (mevedel-session-permission-mode
                         (buffer-local-value
                          'mevedel--session (current-buffer)))
                        mode)))
               ((symbol-function 'mevedel-view--interaction-target-buffer)
                (lambda (buffer)
                  (should (eq target-buffer buffer))
                  view-buffer))
               ((symbol-function 'mevedel-view--run-prompt-submit-hook)
                (lambda (input _display callback &rest _)
                  (push input prompts)
                  (funcall callback
                           (mevedel-prompt-submission-create
                            :input input :state 'committed))))
               ((symbol-function 'mevedel--implement-plan)
                (lambda (_action)
                  (should (eq target-buffer (current-buffer)))
                  (cl-incf attempts)
                  (when (= attempts 1) (error "Startup failed"))
                  'started)))
            (mevedel-plan-mode--dispatch-accepted
             source-session source-buffer)
            (let* ((retry
                    (plist-get (mevedel-session-plan-metadata source-session)
                               :implementation-retry))
                   (summary (plist-get retry :summary))
                   (prompt (car prompts)))
              (should (= 1 summary-runs))
              (should (= 1 creates))
              (should (= 1 archives))
              (should clean)
              (should (eq 'submit (plist-get retry :step)))
              (should (string-search "src/api.el" summary))
              (should-not (string-search source-directory summary))
              (should (equal source-text
                             (with-current-buffer source-buffer
                               (buffer-string))))
              (with-current-buffer target-buffer
                (should (equal summary (mevedel--compact-previous-summary)))
                (should (= 1 (how-many "^#\\+begin_summary"
                                       (point-min) (point-max)))))
              (let ((path-position
                     (string-search
                      (plist-get retry :target-accepted-absolute-path) prompt))
                    (plan-position (string-search body prompt))
                    (instruction-position
                     (string-search "Implementation instructions:" prompt)))
                (should (< path-position plan-position instruction-position))))
            (mevedel-retry-plan-implementation
             source-session source-buffer)
            (should (= 1 summary-runs))
            (should (= 1 creates))
            (should (= 1 archives))
            (should (= 2 attempts))
            (with-current-buffer target-buffer
              (should (= 1 (how-many "^#\\+begin_summary"
                                     (point-min) (point-max)))))
            (should-not
             (plist-member (mevedel-session-plan-metadata source-session)
                           :implementation-retry))))
      (dolist (buffer (list view-buffer target-buffer source-buffer))
        (when (buffer-live-p buffer) (kill-buffer buffer)))
      (dolist (directory
               (list target-directory source-directory target-save source-save))
        (delete-directory directory t)))))


(mevedel-deftest mevedel-plan-mode--current-session
  (:doc "resolves explicit, local, and paired data-buffer sessions")
  ,test
  (test)
  (let ((explicit (mevedel-session--create :name "explicit"))
        (local (mevedel-session--create :name "local"))
        (paired (mevedel-session--create :name "paired"))
        (data-buffer (generate-new-buffer " *mevedel-plan-session*")))
    (unwind-protect
        (progn
          (should (eq explicit
                      (mevedel-plan-mode--current-session explicit)))
          (let ((mevedel--session local))
            (should (eq local (mevedel-plan-mode--current-session))))
          (with-current-buffer data-buffer
            (setq-local mevedel--session paired))
          (let ((mevedel--session nil)
                (mevedel--data-buffer data-buffer))
            (should (eq paired (mevedel-plan-mode--current-session)))))
      (kill-buffer data-buffer))))

(mevedel-deftest mevedel-plan-mode--deactivate
  (:doc "clears Plan state without changing proposal metadata")
  ,test
  (test)
  (let ((session
         (mevedel-session--create
          :name "main" :plan-mode t :plan-metadata '(:status proposed))))
    (mevedel-plan-mode--deactivate session)
    (should-not (mevedel-session-plan-mode session))
    (should (eq 'proposed
                (plist-get (mevedel-session-plan-metadata session) :status)))))

(mevedel-deftest mevedel-plan-mode--selection-valid-p
  (:doc "accepts exactly the supported location/context/mode matrix")
  ,test
  (test)
  (dolist (selection
           '((:location here :context current :execution direct :mode ask)
             (:location here :context fresh :execution direct :mode auto)
             (:location worktree :context summary :execution direct
                        :mode full-auto)))
    (should (mevedel-plan-mode--selection-valid-p selection)))
  (dolist (selection
           '((:location worktree :context current :execution direct :mode ask)
             (:location here :context current :execution goal :mode ask)
             (:location here :context current :execution direct :mode plan)))
    (should-not (mevedel-plan-mode--selection-valid-p selection))))

(mevedel-deftest mevedel-plan-mode--demote-proposal
  (:doc "demotes proposals and optionally discards their selection")
  ,test
  (test)
  (let ((session
         (mevedel-session--create
          :name "main"
          :plan-metadata '(:status proposed :proposal-id (1 2 "h")
                           :selection (:mode auto)))))
    (mevedel-plan-mode--demote-proposal session nil)
    (should (eq 'draft
                (plist-get (mevedel-session-plan-metadata session) :status)))
    (should (plist-member (mevedel-session-plan-metadata session) :selection))
    (mevedel-plan-mode--demote-proposal session t)
    (should-not
     (plist-member (mevedel-session-plan-metadata session) :selection))))

(mevedel-deftest mevedel-plan-mode--assistant-prose
  (:doc "joins response spans while excluding tool-result spans")
  ,test
  (test)
  (with-temp-buffer
    (insert "firstTOOLsecond")
    (cl-letf (((symbol-function 'mevedel-transcript-segments)
               (lambda (&rest _)
                 '((response 1 6) (tool-result 6 10) (response 10 16)))))
      (should (equal "first\n\nsecond"
                     (mevedel-plan-mode--assistant-prose 1 16))))))

(mevedel-deftest mevedel-plan-mode--next-mode
  (:doc "cycles implementation permission modes")
  ,test
  (test)
  (should (eq 'auto (mevedel-plan-mode--next-mode 'ask)))
  (should (eq 'ask (mevedel-plan-mode--next-mode 'full-auto))))

(mevedel-deftest mevedel-plan-mode--next-context
  (:doc "cycles only contexts valid for the selected location")
  ,test
  (test)
  (should (eq 'fresh (mevedel-plan-mode--next-context 'here 'current)))
  (should (eq 'current (mevedel-plan-mode--next-context 'here 'summary)))
  (should (eq 'summary (mevedel-plan-mode--next-context 'worktree 'fresh)))
  (should (eq 'fresh (mevedel-plan-mode--next-context 'worktree 'summary))))

(mevedel-deftest mevedel-plan-mode--next-location
  (:doc "moves Current to Worktree/Fresh and preserves Fresh on return")
  ,test
  (test)
  (let ((selection '(:location here :context current)))
    (mevedel-plan-mode--next-location selection)
    (should (equal '(:location worktree :context fresh) selection))
    (mevedel-plan-mode--next-location selection)
    (should (equal '(:location here :context fresh) selection))))

(mevedel-deftest mevedel-plan-mode--implementation-prompt
  (:doc "includes the immutable artifact, plan body, and implementation order")
  ,test
  (test)
  (let ((prompt
         (mevedel-plan-mode--implementation-prompt
          '(:absolute-path "/tmp/accepted.md") "# Accepted")))
    (should (string-match-p "/tmp/accepted.md" prompt))
    (should (string-match-p "# Accepted" prompt))
    (should (string-match-p "Implement the accepted plan" prompt))))

(mevedel-deftest mevedel-plan-mode--implementation-record
  (:doc "chooses the first unfinished preparation step")
  ,test
  (test)
  (let ((accepted '(:path "accepted.md" :absolute-path "/tmp/accepted.md"
                    :hash "h")))
    (should (eq 'submit
                (plist-get
                 (mevedel-plan-mode--implementation-record
                  '(:location here :context current) accepted)
                 :step)))
    (should (eq 'prepare-context
                (plist-get
                 (mevedel-plan-mode--implementation-record
                  '(:location here :context fresh) accepted)
                 :step)))
    (should (eq 'prepare-worktree
                (plist-get
                 (mevedel-plan-mode--implementation-record
                  '(:location worktree :context fresh) accepted)
                 :step)))
    (should (eq 'prepare-summary
                (plist-get
                 (mevedel-plan-mode--implementation-record
                  '(:location worktree :context summary) accepted)
                 :step)))))

(mevedel-deftest mevedel-plan-mode--accepted-body
  (:doc "reads a matching immutable artifact and rejects a bad hash")
  ,test
  (test)
  (let ((path (make-temp-file "mevedel-plan-accepted-")))
    (unwind-protect
        (progn
          (write-region "# Accepted" nil path nil 'silent)
          (should (equal "# Accepted"
                         (mevedel-plan-mode--accepted-body
                          (list :accepted-absolute-path path
                                :accepted-hash
                                (mevedel-plan-hash "# Accepted")))))
          (should-error
           (mevedel-plan-mode--accepted-body
            (list :accepted-absolute-path path :accepted-hash "wrong"))))
      (delete-file path))))

(mevedel-deftest mevedel-plan-mode--summary-instructions
  (:doc "adds portable-path guidance only for Worktree summaries")
  ,test
  (test)
  (should-not
   (string-match-p "relative to the repository root"
                   (mevedel-plan-mode--summary-instructions)))
  (should (string-match-p
           "relative to the repository root"
           (mevedel-plan-mode--summary-instructions t))))

(mevedel-deftest mevedel-plan-mode--summary-without-plan
  (:doc "replaces an exact duplicate plan with a compact placeholder")
  ,test
  (test)
  (should (equal "Before\n(Accepted plan omitted; supplied separately.)\nAfter"
                 (mevedel-plan-mode--summary-without-plan
                  "Before\n# Plan\nAfter" "# Plan"))))

(mevedel-deftest mevedel-plan-mode--implementation-failed
  (:doc "records a retryable failure and clears preparation progress")
  ,test
  (test)
  (let* ((session
          (mevedel-session--create
           :name "main" :plan-metadata
           '(:implementation-retry (:step submit))))
         (chat-buffer (generate-new-buffer " *mevedel-plan-failed-data*"))
         (view-buffer (generate-new-buffer " *mevedel-plan-failed-view*"))
         stopped)
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel-view--interaction-target-buffer)
                   (lambda (_) view-buffer))
                  ((symbol-function 'mevedel-view--stop-request-progress)
                   (lambda () (setq stopped t)))
                  ((symbol-function 'mevedel-plan-mode--persist) #'ignore))
          (mevedel-plan-mode--implementation-failed
           session chat-buffer "Transport failed")
          (should stopped)
          (should (equal "Transport failed"
                         (plist-get
                          (plist-get (mevedel-session-plan-metadata session)
                                     :implementation-retry)
                          :failure))))
      (kill-buffer view-buffer)
      (kill-buffer chat-buffer))))

(mevedel-deftest mevedel-plan-mode--implementation-started
  (:doc "clears retry state after successful request startup")
  ,test
  (test)
  (let ((session
         (mevedel-session--create
          :name "main" :plan-metadata
          '(:status accepted :implementation-retry (:step submit)))))
    (cl-letf (((symbol-function 'mevedel-plan-mode--persist) #'ignore))
      (mevedel-plan-mode--implementation-started session (current-buffer)))
    (should-not
     (plist-member (mevedel-session-plan-metadata session)
                   :implementation-retry))))

(mevedel-deftest mevedel-plan-mode--approval-entry
  (:doc "builds one root interaction with a callable outcome callback")
  ,test
  (test)
  (let* ((session (mevedel-session--create :name "main"))
         (selection '(:location here :context current
                      :execution direct :mode ask))
         (entry
          (mevedel-plan-mode--approval-entry
           "# Plan" (current-buffer) session selection)))
    (should (equal "/root" (plist-get entry :origin)))
    (should (eq selection (plist-get entry :selection)))
    (should (functionp (plist-get entry :callback)))))

(mevedel-deftest mevedel-plan-mode--persist
  (:doc "delegates session saving to the canonical persistence writer")
  ,test
  (test)
  (let ((session (mevedel-session--create :name "main")) seen)
    (cl-letf (((symbol-function 'mevedel-session-persistence-save)
               (lambda (saved-session buffer)
                 (setq seen (list saved-session buffer)))))
      (mevedel-plan-mode--persist session (current-buffer)))
    (should (equal (list session (current-buffer)) seen))))

(mevedel-deftest mevedel-plan-mode--worktree-target-buffer
  (:doc "restores only the recorded Worktree session and directory")
  ,test
  (test)
  (let* ((directory (make-temp-file "mevedel-plan-target-dir-" t))
         (save-path (make-temp-file "mevedel-plan-target-save-" t))
         (buffer (generate-new-buffer " *mevedel-plan-target*"))
         (session
          (mevedel-session--create
           :name "target" :session-id "target-id"
           :working-directory directory)))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq-local mevedel--session session))
          (cl-letf (((symbol-function 'mevedel-session-persistence-restore)
                     (lambda (_) buffer)))
            (should
             (eq buffer
                 (mevedel-plan-mode--worktree-target-buffer
                  (list :target-save-path save-path
                        :target-session-id "target-id"
                        :target-directory directory))))))
      (kill-buffer buffer)
      (delete-directory save-path t)
      (delete-directory directory t))))

(mevedel-deftest mevedel-plan-mode--worktree-warning
  (:doc "warns only for a dirty Worktree source")
  ,test
  (test)
  (let ((entry
         (list :selection '(:location worktree)
               :chat-buffer (current-buffer))))
    (cl-letf (((symbol-function 'mevedel-worktree--collect-status)
               (lambda (&rest _) '(:dirty-p t))))
      (should (string-match-p
               "uncommitted changes"
               (mevedel-plan-mode--worktree-warning entry))))))

(mevedel-deftest mevedel-plan-mode--prepare-worktree
  (:doc "creates one clean target and persists its retry identity")
  ,test
  (test)
  (let* ((source-session (mevedel-session--create :name "source"))
         (target-session
          (mevedel-session--create :name "target" :session-id "target-id"))
         (source-buffer (generate-new-buffer " *mevedel-plan-worktree-source*"))
         (target-buffer (generate-new-buffer " *mevedel-plan-worktree-target*"))
         (record
          '(:selection (:location worktree :context summary
                        :execution direct :mode auto :branch "plan/test")))
         clean)
    (unwind-protect
        (progn
          (with-current-buffer target-buffer
            (setq-local mevedel--session target-session))
          (cl-letf (((symbol-function 'mevedel-worktree-create-session)
                     (lambda (branch _purpose clean-arg)
                       (setq clean clean-arg)
                       (list :branch branch :directory "/tmp/target"
                             :buffer target-buffer)))
                    ((symbol-function
                      'mevedel-session-persistence-ensure-files)
                     (lambda (&rest _) "/tmp/target-save"))
                    ((symbol-function 'mevedel-plan-mode--persist) #'ignore))
            (let ((prepared
                   (mevedel-plan-mode--prepare-worktree
                    source-session source-buffer record)))
              (should clean)
              (should (eq 'prepare-target (plist-get prepared :step)))
              (should (equal "target-id"
                             (plist-get prepared :target-session-id)))
              (should (equal prepared
                             (plist-get
                              (mevedel-session-plan-metadata source-session)
                              :implementation-retry))))))
      (kill-buffer target-buffer)
      (kill-buffer source-buffer))))

(mevedel-deftest mevedel-plan-mode--prepare-worktree-target
  (:doc "copies the accepted artifact and selected settings into the target")
  ,test
  (test)
  (let* ((path (make-temp-file "mevedel-plan-worktree-plan-"))
         (source-session
          (mevedel-session--create
           :name "source" :preset-name 'source-preset
           :preset-settings '(:model test)))
         (target-session (mevedel-session--create :name "target"))
         (source-buffer (generate-new-buffer " *mevedel-plan-target-source*"))
         (target-buffer (generate-new-buffer " *mevedel-plan-target-data*"))
         mode)
    (unwind-protect
        (progn
          (write-region "# Accepted" nil path nil 'silent)
          (with-current-buffer target-buffer
            (setq-local mevedel--session target-session))
          (cl-letf (((symbol-function
                      'mevedel-plan-mode--worktree-target-buffer)
                     (lambda (_) target-buffer))
                    ((symbol-function 'mevedel-plan-archive-accepted)
                     (lambda (&rest _)
                       '(:path "plans/accepted.md"
                         :absolute-path "/tmp/target-accepted.md"
                         :hash "target-hash")))
                    ((symbol-function 'mevedel-preset-restore-session)
                     #'ignore)
                    ((symbol-function 'mevedel-permission-mode-transition)
                     (lambda (selected) (setq mode selected)))
                    ((symbol-function 'mevedel-plan-mode--persist) #'ignore))
            (let* ((record
                    (list :selection
                          '(:location worktree :context fresh
                            :execution direct :mode full-auto)
                          :accepted-path "plans/source.md"
                          :accepted-absolute-path path
                          :accepted-hash (mevedel-plan-hash "# Accepted")))
                   (prepared
                    (mevedel-plan-mode--prepare-worktree-target
                     source-session source-buffer record)))
              (should (eq 'submit (plist-get prepared :step)))
              (should (eq 'full-auto mode))
              (should (eq 'source-preset
                          (mevedel-session-preset-name target-session)))
              (should (equal "/tmp/target-accepted.md"
                             (plist-get prepared
                                        :target-accepted-absolute-path))))))
      (kill-buffer target-buffer)
      (kill-buffer source-buffer)
      (delete-file path))))

(mevedel-deftest mevedel-plan-mode--prepare-summary
  (:doc "caches a plan-free portable summary before redispatch")
  ,test
  (test)
  (let* ((root (make-temp-file "mevedel-plan-summary-root-" t))
         (path (make-temp-file "mevedel-plan-summary-plan-"))
         (session
          (mevedel-session--create
           :name "source" :working-directory root))
         (buffer (generate-new-buffer " *mevedel-plan-summary-data*"))
         dispatched)
    (unwind-protect
        (progn
          (write-region "# Accepted" nil path nil 'silent)
          (cl-letf (((symbol-function 'mevedel--compact-main-target)
                     (lambda () (list :apply (lambda (&rest _)))))
                    ((symbol-function 'mevedel--compact-run)
                     (lambda (&rest args)
                       (let* ((summary
                               (funcall (plist-get args :summary-ready)
                                        (concat root "/file.el\n# Accepted")))
                              (target (plist-get args :target)))
                         (funcall (plist-get target :apply) target summary)
                         (funcall (plist-get args :callback) nil))))
                    ((symbol-function 'mevedel-plan-mode--persist) #'ignore)
                    ((symbol-function 'mevedel-plan-mode--dispatch-accepted)
                     (lambda (&rest _) (setq dispatched t))))
            (mevedel-plan-mode--prepare-summary
             session buffer
             (list :selection
                   '(:location worktree :context summary
                     :execution direct :mode ask)
                   :accepted-absolute-path path
                   :accepted-hash (mevedel-plan-hash "# Accepted"))))
          (let* ((retry
                  (plist-get (mevedel-session-plan-metadata session)
                             :implementation-retry))
                 (summary (plist-get retry :summary)))
            (should dispatched)
            (should-not (string-match-p (regexp-quote root) summary))
            (should-not (string-match-p "# Accepted" summary))))
      (kill-buffer buffer)
      (delete-file path)
      (delete-directory root t))))

(mevedel-deftest mevedel-plan-mode--accept
  (:doc "archives, exits Plan, shows preparation, and dispatches once")
  ,test
  (test)
  (let* ((session
          (mevedel-session--create
           :name "main" :plan-mode t :permission-mode 'ask))
         (chat-buffer (generate-new-buffer " *mevedel-plan-accept-data*"))
         (view-buffer (generate-new-buffer " *mevedel-plan-accept-view*"))
         status dispatched selected-mode)
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel-plan-accept)
                   (lambda (&rest _)
                     '(:accepted (:path "accepted.md"
                                   :absolute-path "/tmp/accepted.md"
                                   :hash "h"))))
                  ((symbol-function 'mevedel-view--interaction-target-buffer)
                   (lambda (_) view-buffer))
                  ((symbol-function 'mevedel-view--update-spinner)
                   (lambda (text owner) (setq status (list text owner))))
                  ((symbol-function 'mevedel-permission-mode-transition)
                   (lambda (mode) (setq selected-mode mode)))
                  ((symbol-function 'mevedel-plan-mode--dispatch-accepted)
                   (lambda (&rest _) (setq dispatched t))))
          (mevedel-plan-mode--accept
           "# Plan" chat-buffer session
           '(:location here :context current :execution direct :mode auto))
          (should-not (mevedel-session-plan-mode session))
          (should dispatched)
          (should (eq 'auto selected-mode))
          (should (equal '("Preparing implementation..." plan-preparation)
                         status)))
      (kill-buffer view-buffer)
      (kill-buffer chat-buffer))))

(mevedel-deftest mevedel-retry-plan-implementation
  (:doc "redispatches only when an accepted retry record exists")
  ,test
  (test)
  (let ((session
         (mevedel-session--create
          :name "main" :plan-metadata
          '(:implementation-retry (:step submit))))
        dispatched)
    (cl-letf (((symbol-function 'mevedel-plan-mode--dispatch-accepted)
               (lambda (retry-session retry-buffer)
                 (setq dispatched (list retry-session retry-buffer)))))
      (mevedel-retry-plan-implementation session (current-buffer)))
    (should (equal (list session (current-buffer)) dispatched))
    (setf (mevedel-session-plan-metadata session) nil)
    (should-error
     (mevedel-retry-plan-implementation session (current-buffer))
     :type 'user-error)))

(provide 'test-mevedel-plan-mode)
;;; test-mevedel-plan-mode.el ends here
