;;; test-mevedel-plan-mode.el --- Tests for mevedel-plan-mode.el -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'gptel-request)
(require 'mevedel-plan)
(require 'mevedel-plan-mode)
(require 'mevedel-goal)
(require 'mevedel-interaction-prompt)
(require 'mevedel-permissions)
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
    (should (mevedel-plan-mode-enter session)))

  :doc "new Plan conversations discard an earlier execution selection"
  (let ((session
         (mevedel-session--create
          :name "test" :plan-metadata
          '(:status accepted :selection (:execution goal)))))
    (mevedel-plan-mode-enter session)
    (should-not
     (plist-member (mevedel-session-plan-metadata session) :selection)))

  :doc "re-entering an active Plan preserves its proposal selection"
  (let* ((selection '(:location here :context current
                      :execution goal :mode auto))
         (session
          (mevedel-session--create
           :name "test" :plan-mode t
           :plan-metadata (list :status 'proposed :selection selection))))
    (mevedel-plan-mode-enter session)
    (should (equal selection
                   (plist-get (mevedel-session-plan-metadata session)
                              :selection)))))

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
                      :execution goal :mode auto))
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
  (:doc "renders and toggles execution without applying Mode before acceptance")
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
                            "Execution  Direct"
                            "one ordinary implementation turn"
                            "Mode       auto"))
              (should (string-match-p text body)))
            (call-interactively (lookup-key keymap (kbd "e")))
            (should (eq 'goal (plist-get selection :execution)))
            (should rerendered)
            (setq rerendered nil)
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
                                         :execution goal :mode full-auto
                                         :branch "plan/topic"))
                           outcome))))
      (when (buffer-live-p view-buffer) (kill-buffer view-buffer))
      (when (buffer-live-p data-buffer) (kill-buffer data-buffer))))

  :doc "shows selected Goal execution and its read-only effective budget"
  (dolist (case '((nil "Unlimited") (200000 "200000 tokens")))
    (let* ((session (mevedel-session--create :name "test"))
           (data-buffer (generate-new-buffer " *plan-goal-budget-data*"))
           (view-buffer (generate-new-buffer " *plan-goal-budget-view*"))
           (selection '(:location here :context current
                        :execution goal :mode ask))
           (entry (mevedel-plan-mode--approval-entry
                   "# Plan" data-buffer session selection))
           descriptor)
      (unwind-protect
          (progn
            (with-current-buffer data-buffer
              (setq-local mevedel-goal-token-budget (car case)))
            (cl-letf (((symbol-function
                        'mevedel-view--interaction-target-buffer)
                       (lambda (_buffer) view-buffer))
                      ((symbol-function 'mevedel-view--fontify-as)
                       (lambda (text _mode) text))
                      ((symbol-function 'mevedel-view--interaction-register)
                       (lambda (value)
                         (setq descriptor value)
                         (make-overlay (point-min) (point-min)))))
              (mevedel-plan-mode--render-approval entry))
            (let ((body (plist-get descriptor :body)))
              (should (string-match-p "Execution  Goal" body))
              (should (string-match-p
                       "continue automatically until complete" body))
              (should (string-match-p
                       (format "Goal budget %s" (cadr case)) body))))
        (when (buffer-live-p view-buffer) (kill-buffer view-buffer))
        (when (buffer-live-p data-buffer) (kill-buffer data-buffer)))))

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
  (:doc "restores a selected Goal proposal without changing the composer")
  ,test
  (test)
  (let* ((save-dir (make-temp-file "mevedel-plan-restore-" t))
         (path (file-name-concat save-dir "plans" "current.md"))
         (plan "# Restored plan")
         (hash (mevedel-plan-hash plan))
         (selection '(:location here :context current
                      :execution goal :mode auto))
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
                    ((symbol-function 'mevedel-plan-handoff--persist) #'ignore)
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
            (should-not (plist-member metadata :implementation-retry))
            (should-not (mevedel-session-goal session))))
      (when (buffer-live-p view-buffer) (kill-buffer view-buffer))
      (when (buffer-live-p data-buffer) (kill-buffer data-buffer))
      (delete-directory save-dir t)))

  :doc "constructs Here Goal with immutable contract and canonical kickoff"
  (let* ((save-dir (make-temp-file "mevedel-plan-goal-" t))
         (session (mevedel-session--create
                   :name "test" :save-path save-dir
                   :permission-mode 'auto :plan-mode t))
         (data-buffer (generate-new-buffer " *plan-goal-data*"))
         (view-buffer (generate-new-buffer " *plan-goal-view*"))
         (mevedel-goal-token-budget 1234)
         hook-input hook-display implementation reserved-id)
    (unwind-protect
        (progn
          (with-current-buffer data-buffer
            (setq-local mevedel--session session
                        mevedel-permission-mode 'auto
                        mevedel-goal-token-budget 1234))
          (cl-letf (((symbol-function 'mevedel-view--interaction-target-buffer)
                     (lambda (_buffer) view-buffer))
                    ((symbol-function 'mevedel-view--run-prompt-submit-hook)
                     (lambda (input display callback &rest _)
                       (setq hook-input input hook-display display)
                       (funcall callback
                                (mevedel-prompt-submission-create
                                 :input input :state 'committed))))
                    ((symbol-function 'mevedel-plan-handoff--persist)
                     (lambda (saved-session _buffer)
                       (when (and (not (mevedel-session-goal saved-session))
                                  (plist-get
                                   (mevedel-session-plan-metadata saved-session)
                                   :implementation-retry))
                         (setq reserved-id
                               (plist-get
                                (plist-get
                                 (mevedel-session-plan-metadata saved-session)
                                 :implementation-retry)
                                :goal-id)))))
                    ((symbol-function 'mevedel-session-persistence-save)
                     #'ignore)
                    ((symbol-function 'mevedel--implement-plan)
                     (lambda (action) (setq implementation action))))
            (mevedel-plan-mode--approval-callback
             "Free-form accepted plan." data-buffer session
             '(:accept t
               :selection (:location here :context current
                           :execution goal :mode full-auto))))
          (let* ((metadata (mevedel-session-plan-metadata session))
                 (goal (mevedel-session-goal session))
                 (accepted (plist-get metadata :accepted-absolute-path)))
            (should (equal mevedel-plan-handoff--accepted-goal-objective
                           (mevedel-goal-objective goal)))
            (should (equal reserved-id (mevedel-goal-id goal)))
            (should (equal (plist-get metadata :accepted-path)
                           (mevedel-goal-plan-reference goal)))
            (should (= 1234 (mevedel-goal-token-budget goal)))
            (should (string-match-p (regexp-quote accepted) hook-input))
            (should (string-match-p "Free-form accepted plan" hook-input))
            (should (string-match-p "Begin the active Goal" hook-input))
            (should-not (string-match-p
                         (regexp-quote accepted)
                         (mevedel-goal-objective goal)))
            (should (equal "Implement accepted plan as Goal" hook-display))
            (should (equal hook-display
                           (plist-get implementation :display-text)))
            (should-not (plist-member metadata :implementation-retry))))
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
          (cl-letf (((symbol-function 'mevedel-plan-handoff--persist) #'ignore)
                    ((symbol-function 'mevedel-plan-handoff--dispatch-accepted)
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

(mevedel-deftest mevedel-plan-mode--next-execution
  (:doc "toggles only Direct and Goal")
  ,test
  (test)
  (should (eq 'goal (mevedel-plan-mode--next-execution 'direct)))
  (should (eq 'direct (mevedel-plan-mode--next-execution 'goal))))

(mevedel-deftest mevedel-plan-mode--execution-description
  (:doc "describes one-turn and durable execution compactly")
  ,test
  (test)
  (should (string-match-p
           "one ordinary implementation turn"
           (mevedel-plan-mode--execution-description 'direct)))
  (should (string-match-p
           "continue automatically until complete"
           (mevedel-plan-mode--execution-description 'goal))))

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
                  ((symbol-function 'mevedel-plan-handoff--dispatch-accepted)
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

(provide 'test-mevedel-plan-mode)
;;; test-mevedel-plan-mode.el ends here
