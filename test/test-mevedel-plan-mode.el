;;; test-mevedel-plan-mode.el --- Tests for mevedel-plan-mode.el -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'gptel-request)
(require 'gptel-agent-tools)
(require 'mevedel-chat)
(require 'mevedel-menu)
(require 'mevedel-plan)
(require 'mevedel-plan-handoff)
(require 'mevedel-plan-mode)
(require 'mevedel-goal)
(require 'mevedel-interaction-prompt)
(require 'mevedel-pending-inputs)
(require 'mevedel-view-agent)
(require 'mevedel-view-render)
(require 'mevedel-permissions)
(require 'mevedel-prompt-submission)
(require 'mevedel-session-persistence)
(require 'mevedel-skills-ui)
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
  (let ((session (mevedel-session--create :authority-mode 'pid-lock :name "test" :plan-mode t)))
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
                  :authority-mode 'pid-lock
                  :name "test" :permission-mode 'full-auto)))
    (mevedel-plan-mode-enter session)
    (should (mevedel-session-plan-mode session))
    (should (eq 'full-auto (mevedel-session-permission-mode session))))

  :doc "rejects every unfinished Goal status without changing state"
  (dolist (status '(active paused blocked))
    (let ((session
           (mevedel-session--create
            :authority-mode 'pid-lock
            :name "test" :permission-mode 'edits
            :goal (mevedel-goal--create :status status))))
      (should-error (mevedel-plan-mode-enter session) :type 'user-error)
      (should-not (mevedel-session-plan-mode session))
      (should (eq 'edits (mevedel-session-permission-mode session)))))

  :doc "allows a completed Goal to remain as history"
  (let ((session
         (mevedel-session--create
          :authority-mode 'pid-lock
          :name "test" :goal (mevedel-goal--create :status 'complete))))
    (should (mevedel-plan-mode-enter session)))

  :doc "new Plan conversations discard an earlier execution selection"
  (let ((session
         (mevedel-session--create
          :authority-mode 'pid-lock
          :name "test" :plan-metadata
          '(:status accepted :selection (:execution goal)))))
    (mevedel-plan-mode-enter session)
    (should-not
     (plist-member (mevedel-session-plan-metadata session) :selection)))

  :doc "re-entering an active Plan preserves its proposal selection"
  (let* ((selection '(:location here :context current
                      :execution goal :mode edits))
         (session
          (mevedel-session--create
           :authority-mode 'pid-lock
           :name "test" :plan-mode t
           :plan-metadata (list :status 'proposed :selection selection))))
    (mevedel-plan-mode-enter session)
    (should (equal selection
                   (plist-get (mevedel-session-plan-metadata session)
                              :selection))))

  :doc "rejects ordinary Plan while directive planning owns the session"
  (let ((session
         (mevedel-session--create
          :authority-mode 'pid-lock
          :name "test"
          :directive-planning '(:directive-id "d1" :phase approval))))
    (should-error (mevedel-plan-mode-enter session) :type 'user-error)
    (should-not (mevedel-session-plan-mode session))))

(mevedel-deftest mevedel-plan-mode-exit
  (:doc "leaves Plan without changing the underlying permission mode")
  ,test
  (test)
  (let ((session (mevedel-session--create
                  :authority-mode 'pid-lock
                  :name "test" :permission-mode 'edits :plan-mode t)))
    (mevedel-plan-mode-exit session)
    (should-not (mevedel-session-plan-mode session))
    (should (eq 'edits (mevedel-session-permission-mode session))))

  :doc "cancels a proposal into a draft and discards its selection"
  (let* ((selection '(:location here :context current
                      :execution goal :mode edits))
         (session (mevedel-session--create
                   :authority-mode 'pid-lock
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
  (:doc "defaults to implementation axes, model snapshot, and Goal budget")
  ,test
  (test)
  (mevedel-skills-test--with-model-backends
    (let ((session (mevedel-session--create
                    :authority-mode 'pid-lock
                    :name "test" :permission-mode 'edits)))
      (with-temp-buffer
        (setq-local mevedel-goal-token-budget 1234
                    gptel-backend (gptel-get-backend "Balanced")
                    gptel-model 'balanced-model
                    gptel-reasoning-effort 'high)
        (should (equal '(:location here :context current
                         :execution direct :mode edits
                         :model-provider "Balanced:balanced-model"
                         :reasoning-effort high
                         :goal-token-budget 1234
                         :skills nil :instructions nil)
                       (mevedel-plan-mode--default-selection session))))
      (should (eq 'edits (mevedel-session-permission-mode session))))))

(mevedel-deftest mevedel-plan-mode--invalidate-proposal
  (:doc "demotes an actionable proposal while preserving its selection")
  ,test
  (test)
  (let* ((selection '(:location here :context current
                      :execution direct :mode edits))
         (session (mevedel-session--create
                   :authority-mode 'pid-lock
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

(mevedel-deftest mevedel-plan-approval-settle
  (:quiet t :doc "clears pending state before delivering the outcome")
  ,test
  (test)
  (let* ((session (mevedel-session--create :authority-mode 'pid-lock :name "test"))
         pending-during-callback
         (entry
          (list :session session
                :callback
                (lambda (_outcome)
                  (setq pending-during-callback
                        (mevedel-session-pending-plan-approval session))))))
    (setf (mevedel-session-pending-plan-approval session) entry)
    (mevedel-plan-approval-settle entry 'aborted)
    (should-not pending-during-callback)
    (should-not (mevedel-session-pending-plan-approval session)))

  :doc "restores and rerenders after a callback error"
  (let* ((session (mevedel-session--create :authority-mode 'pid-lock :name "test"))
         rendered
         (entry
          (list :session session
                :renderer (lambda (_entry) (setq rendered t))
                :callback (lambda (_outcome) (error "Boom")))))
    (setf (mevedel-session-pending-plan-approval session) entry)
    (mevedel-plan-approval-settle entry 'aborted)
    (should (eq entry (mevedel-session-pending-plan-approval session)))
    (should rendered))

  :doc "keeps acceptance settled after Goal-save or Mode-transition failure"
  (dolist (failure '(goal-save mode-transition))
    (let* ((session
            (mevedel-session--create
             :authority-mode 'pid-lock :name "test" :plan-mode t))
           (chat-buffer (generate-new-buffer " *plan-committed-failure*"))
           (selection
            (list :location 'here :context 'current
                  :execution (if (eq failure 'goal-save) 'goal 'direct)
                  :mode 'edits :model-provider "Test:test-model"))
           (outcome (list :accept t :selection selection))
           (entry
            (mevedel-plan-mode--approval-entry
             "# Accepted" chat-buffer session selection))
           (archives 0)
           (saves 0)
           (transitions 0)
           (submissions 0)
           (mode-fails (eq failure 'mode-transition)))
      (unwind-protect
          (progn
            (with-current-buffer chat-buffer
              (setq-local mevedel--session session))
            (setf (mevedel-session-pending-plan-approval session) entry)
            (cl-letf
                (((symbol-function 'mevedel-plan-accept)
                  (lambda (&rest _)
                    (cl-incf archives)
                    (setf (mevedel-session-plan-metadata session)
                          '(:status accepted
                            :accepted-path "local/plans/accepted.md"
                            :accepted-hash "hash"))
                    '(:accepted
                      (:path "local/plans/accepted.md" :hash "hash"))))
                 ((symbol-function 'mevedel-plan-handoff--persist)
                  (lambda (&rest _)
                    (cl-incf saves)
                    (when (and (eq failure 'goal-save) (= saves 1))
                      (error "Reservation save failed"))))
                 ((symbol-function 'mevedel-permission-mode-transition)
                  (lambda (mode)
                    (cl-incf transitions)
                    (if mode-fails
                        (error "Mode transition failed")
                      (setf (mevedel-session-permission-mode session)
                            mode))))
                 ((symbol-function 'mevedel-plan-handoff--submit)
                  (lambda (&rest _)
                    (cl-incf submissions))))
              (mevedel-plan-approval-settle entry outcome)
              (mevedel-plan-approval-settle entry outcome)
            (let* ((metadata (mevedel-session-plan-metadata session))
                   (retry (plist-get metadata :implementation-retry)))
              (should (= 1 archives))
              (should-not (mevedel-session-plan-mode session))
              (should-not
               (mevedel-session-pending-plan-approval session))
              (should (eq 'accepted (plist-get metadata :status)))
              (should (string-match-p "failed"
                                      (plist-get retry :failure)))
              (should (= (if (eq failure 'goal-save) 0 1)
                         transitions))
              (should (= 2 saves))
              (should (= 0 submissions)))
            (setq mode-fails nil)
            (mevedel-retry-plan-implementation session chat-buffer)
            (should (eq 'edits
                        (mevedel-session-permission-mode session)))
            (should (= (if (eq failure 'goal-save) 1 2)
                       transitions))
            (should (= 3 saves))
            (should (= 1 submissions))
            (should (= 1 archives))))
        (when (buffer-live-p chat-buffer) (kill-buffer chat-buffer))))))

(mevedel-deftest mevedel-plan-mode--render-approval
  (:doc "renders and toggles execution without applying Mode before acceptance")
  ,test
  (test)
  (let* ((session (mevedel-session--create
                   :authority-mode 'pid-lock
                   :name "test" :permission-mode 'edits))
         (data-buffer (generate-new-buffer " *plan-approval-data*"))
         (view-buffer (generate-new-buffer " *plan-approval-view*"))
         (selection
          (let ((value (mevedel-plan-mode--default-selection session)))
            (plist-put value :model-provider "OpenAI:gpt-5")
            (plist-put value :reasoning-effort 'low)
            value))
         (entry (mevedel-plan-mode--approval-entry
                 "# Plan" data-buffer session selection))
         descriptor outcome rerendered model-opened model-update)
    (unwind-protect
        (progn
          (with-current-buffer data-buffer
            (setq-local gptel-reasoning-effort 'high))
          (cl-letf (((symbol-function 'mevedel-view--interaction-target-buffer)
                   (lambda (_buffer) view-buffer))
                  ((symbol-function 'mevedel-model-current-provider-label)
                   (lambda (&optional _buffer) "Other:ambient"))
                  ((symbol-function 'mevedel-model-resolve-provider)
                   (lambda (&rest _) t))
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
                  ((symbol-function 'mevedel-menu-open-model-selection)
                   (lambda (&rest options)
                     (setq model-opened
                           (list (plist-get options :title)
                                 (plist-get options :provider)
                                 (plist-get options :effort))
                           model-update (plist-get options :update))))
                  ((symbol-function 'mevedel-plan-mode--read-worktree-branch)
                   (lambda (_entry) "plan/topic"))
                  ((symbol-function 'gptel-agent--block-bg)
                   (lambda () 'mevedel-test-block-bg)))
          (mevedel-plan-mode--render-approval entry)
          (let ((body (plist-get descriptor :body))
                (keymap (plist-get descriptor :keymap)))
            (dolist (text '("Implementation"
                            "Location    Here"
                            "Context     Current — full planning transcript"
                            "Execution   Direct — one implementation turn"
                            "Mode        Edits"
                            "Model       OpenAI:gpt-5 · effort low"
                            "Skills      None"
                            "Instructions None"))
              (should (string-match-p text body)))
            (should
             (memq 'mevedel-view-plan-mode
                   (flatten-tree
                    (get-text-property 1 'font-lock-face body))))
            (should
             (memq 'mevedel-test-block-bg
                   (flatten-tree
                    (get-text-property 1 'font-lock-face body))))
            (dolist (key-row '(("l" . "  Location")
                               ("c" . "  Context")
                               ("e" . "  Execution")
                               ("m" . "  Mode")
                               ("M" . "  Model")
                               ("s" . "  Skills")
                               ("i" . "  Instructions")
                               ("RET" . " implement")
                               ("f" . " feedback")
                               ("q" . " hide")
                               ("C-g" . " cancel")))
              (let ((position
                     (string-match
                      (regexp-quote (concat (car key-row) (cdr key-row)))
                      body)))
                (should position)
                (should
                 (memq 'help-key-binding
                       (flatten-tree
                        (get-text-property
                         position 'font-lock-face body))))))
            (should (eq (lookup-key keymap (kbd "m"))
                        (lookup-key keymap (kbd "TAB"))))
            (should (eq (lookup-key keymap (kbd "m"))
                        (lookup-key keymap (kbd "<tab>"))))
            (should-not (lookup-key keymap (kbd "b")))
            (should-not (string-match-p "Budget" body))
            (call-interactively (lookup-key keymap (kbd "M")))
            (should (equal '("Implementation model" "OpenAI:gpt-5" low)
                           model-opened))
            (funcall model-update "Other:implementation" nil)
            (should (equal "Other:implementation"
                           (plist-get selection :model-provider)))
            (should (eq selection
                        (plist-get (mevedel-session-plan-metadata session)
                                   :selection)))
            (should-not (mevedel-session-model-provider session))
            (funcall model-update "OpenAI:gpt-5" 'low)
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
            (should (eq 'edits (mevedel-session-permission-mode session)))
            (call-interactively (lookup-key keymap (kbd "RET")))
            (should (plist-get outcome :accept))
            (let ((accepted (plist-get outcome :selection)))
              (should (eq 'worktree (plist-get accepted :location)))
              (should (eq 'fresh (plist-get accepted :context)))
              (should (eq 'goal (plist-get accepted :execution)))
              (should (eq 'full-auto (plist-get accepted :mode)))
              (should (equal "plan/topic" (plist-get accepted :branch)))
              (should (equal "OpenAI:gpt-5"
                             (plist-get accepted :model-provider)))
              (should (eq 'low
                          (plist-get accepted :reasoning-effort)))))))
      (when (buffer-live-p view-buffer) (kill-buffer view-buffer))
      (when (buffer-live-p data-buffer) (kill-buffer data-buffer))))

  :doc "keeps approval pending when the selected model is unregistered"
  (let* ((session (mevedel-session--create :authority-mode 'pid-lock :name "test"))
         (data-buffer (generate-new-buffer " *plan-model-data*"))
         (view-buffer (generate-new-buffer " *plan-model-view*"))
         (selection (mevedel-plan-mode--default-selection session))
         (entry (mevedel-plan-mode--approval-entry
                 "# Plan" data-buffer session selection))
         descriptor outcome)
    (unwind-protect
        (progn
          (plist-put selection :model-provider "Missing:model")
          (cl-letf (((symbol-function 'mevedel-view--interaction-target-buffer)
                   (lambda (_buffer) view-buffer))
                  ((symbol-function 'mevedel-model-resolve-provider)
                   (lambda (&rest _) nil))
                  ((symbol-function 'mevedel-view--fontify-as)
                   (lambda (text _mode) text))
                  ((symbol-function 'mevedel-view--interaction-register)
                   (lambda (value)
                     (setq descriptor value)
                     (make-overlay (point-min) (point-min))))
                  ((symbol-function 'mevedel--prompt--settle)
                   (lambda (_overlay value) (setq outcome value)))
                  ((symbol-function 'gptel-agent--block-bg)
                   (lambda () 'mevedel-test-block-bg)))
          (mevedel-plan-mode--render-approval entry)
          (should-error
           (call-interactively
            (lookup-key (plist-get descriptor :keymap) (kbd "RET")))
           :type 'user-error)
          (should-not outcome)))
      (when (buffer-live-p view-buffer) (kill-buffer view-buffer))
      (when (buffer-live-p data-buffer) (kill-buffer data-buffer))))

  :doc "toggles canonical skills and saves multiline implementation instructions"
  (let* ((session (mevedel-session--create :authority-mode 'pid-lock :name "test"))
         (data-buffer (generate-new-buffer " *plan-extras-data*"))
         (view-buffer (generate-new-buffer " *plan-extras-view*"))
         (selection (mevedel-plan-mode--default-selection session))
         (entry (mevedel-plan-mode--approval-entry
                 "# Plan" data-buffer session selection))
         (skill (mevedel-skill--create
                 :name "alpha" :display-name "Alpha"
                 :source-file "/tmp/alpha/SKILL.md"
                 :user-invocable-p t :active-p t))
         descriptor editor)
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel-view--interaction-target-buffer)
                   (lambda (_buffer) view-buffer))
                  ((symbol-function 'mevedel-view--interaction-register)
                   (lambda (value)
                     (setq descriptor value)
                     (make-overlay (point-min) (point-min))))
                  ((symbol-function 'mevedel-plan-approval-render)
                   #'ignore)
                  ((symbol-function 'mevedel-skills--user-visible-skills)
                   (lambda (&rest _) (list skill)))
                  ((symbol-function 'completing-read)
                   (lambda (&rest _) "alpha"))
                  ((symbol-function 'pop-to-buffer)
                   (lambda (buffer &rest _) (setq editor buffer))))
          (mevedel-plan-mode--render-approval entry)
          (let ((keymap (plist-get descriptor :keymap)))
            (call-interactively (lookup-key keymap (kbd "s")))
            (should (equal '((:name "alpha"
                              :source-file "/tmp/alpha/SKILL.md"))
                           (plist-get selection :skills)))
            (call-interactively (lookup-key keymap (kbd "s")))
            (should-not (plist-get selection :skills))
            (call-interactively (lookup-key keymap (kbd "i")))
            (should (buffer-live-p editor))
            (with-current-buffer editor
              (insert "Use $alpha.\nRun focused tests.")
              (call-interactively (key-binding (kbd "C-c C-c"))))
            (should (equal "Use $alpha.\nRun focused tests."
                           (plist-get selection :instructions)))
            (should-not (buffer-live-p editor))))
      (when (buffer-live-p editor) (kill-buffer editor))
      (when (buffer-live-p view-buffer) (kill-buffer view-buffer))
      (when (buffer-live-p data-buffer) (kill-buffer data-buffer))))

  :doc "shows selected Goal execution and its editable proposal budget"
  (dolist (case '((nil "Unlimited") (200000 "200000 tokens")))
    (let* ((session (mevedel-session--create :authority-mode 'pid-lock :name "test"))
           (data-buffer (generate-new-buffer " *plan-goal-budget-data*"))
           (view-buffer (generate-new-buffer " *plan-goal-budget-view*"))
           (selection (list :location 'here :context 'current
                            :execution 'goal :mode 'ask
                            :goal-token-budget (car case)))
           (entry (mevedel-plan-mode--approval-entry
                   "# Plan" data-buffer session selection))
           descriptor)
      (unwind-protect
          (progn
            (with-current-buffer data-buffer
              (setq-local mevedel-goal-token-budget 999))
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
              (should (string-match-p "Execution   Goal" body))
              (should (string-match-p
                       "continue until complete, blocked" body))
              (should (string-match-p
                       (format "^b  Budget      %s" (cadr case)) body))))
        (when (buffer-live-p view-buffer) (kill-buffer view-buffer))
        (when (buffer-live-p data-buffer) (kill-buffer data-buffer)))))

  :doc "edits Goal budget proposal-locally and preserves it across execution"
  (let* ((session (mevedel-session--create :authority-mode 'pid-lock :name "test"))
         (data-buffer (generate-new-buffer " *plan-edit-budget-data*"))
         (view-buffer (generate-new-buffer " *plan-edit-budget-view*"))
         (selection '(:location here :context current
                      :execution goal :mode ask :goal-token-budget 100))
         (entry (mevedel-plan-mode--approval-entry
                 "# Plan" data-buffer session selection))
         (input " 200000 ")
         descriptor rerendered)
    (unwind-protect
        (progn
          (with-current-buffer data-buffer
            (setq-local mevedel-goal-token-budget 999))
          (cl-letf (((symbol-function
                      'mevedel-view--interaction-target-buffer)
                     (lambda (_buffer) view-buffer))
                    ((symbol-function 'mevedel-view--interaction-register)
                     (lambda (value)
                       (setq descriptor value)
                       (make-overlay (point-min) (point-min))))
                    ((symbol-function 'mevedel-plan-approval-render)
                     (lambda (&rest _) (setq rerendered t)))
                    ((symbol-function 'read-string)
                     (lambda (&rest _) input)))
            (mevedel-plan-mode--render-approval entry)
            (let* ((body (plist-get descriptor :body))
                   (keymap (plist-get descriptor :keymap))
                   (budget-position (string-match "b  Budget" body)))
              (should budget-position)
              (should
               (memq 'help-key-binding
                     (flatten-tree
                      (get-text-property
                       budget-position 'font-lock-face body))))
              (call-interactively (lookup-key keymap (kbd "b")))
              (should (= 200000
                         (plist-get selection :goal-token-budget)))
              (should (= 999
                         (buffer-local-value
                          'mevedel-goal-token-budget data-buffer)))
              (should rerendered)
              (should (equal selection
                             (plist-get
                              (mevedel-session-plan-metadata session)
                              :selection)))
              (call-interactively (lookup-key keymap (kbd "e")))
              (call-interactively (lookup-key keymap (kbd "e")))
              (should (= 200000
                         (plist-get selection :goal-token-budget)))
              (setq input "")
              (call-interactively (lookup-key keymap (kbd "b")))
              (should-not (plist-get selection :goal-token-budget))
              (setq input "0")
              (should-error
               (call-interactively (lookup-key keymap (kbd "b")))
               :type 'user-error)
              (should-not (plist-get selection :goal-token-budget)))))
      (when (buffer-live-p view-buffer) (kill-buffer view-buffer))
      (when (buffer-live-p data-buffer) (kill-buffer data-buffer))))

  :doc "warns for dirty Worktree state and cancellation keeps approval pending"
  (let* ((session (mevedel-session--create
                   :authority-mode 'pid-lock
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
          (let* ((body (plist-get descriptor :body))
                 (warning-position
                  (string-match "Worktree starts at HEAD" body)))
            (should
             (memq 'warning
                   (flatten-tree
                    (get-text-property
                     warning-position 'font-lock-face body)))))
          (should
           (eq 'quit
               (condition-case nil
                   (call-interactively
                    (lookup-key (plist-get descriptor :keymap) (kbd "RET")))
                 (quit 'quit))))
          (should-not outcome))
      (when (buffer-live-p view-buffer) (kill-buffer view-buffer))
      (when (buffer-live-p data-buffer) (kill-buffer data-buffer))))

  :doc "cycling a setting preserves a multiline leading-> composer draft"
  (mevedel-view-test--with-buffers
    (let* ((session (mevedel-session--create
                     :authority-mode 'pid-lock
                     :name "test" :permission-mode 'edits))
           (selection (mevedel-plan-mode--default-selection session))
           (entry (mevedel-plan-mode--approval-entry
                   "# Plan" data-buf session selection))
           (draft "> first line\nsecond line")
           (point-offset (length "> first")))
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (with-current-buffer view-buf
        (setq-local mevedel--session session)
        (mevedel-view-test--insert-composer-draft draft point-offset))
      (setf (mevedel-session-pending-plan-approval session) entry)
      (with-current-buffer data-buf
        (mevedel-plan-approval-render session))
      (with-current-buffer view-buf
        (let* ((descriptor
                (gethash (plist-get entry :interaction-id)
                         mevedel-view--interaction-descriptors))
               (keymap (plist-get descriptor :keymap)))
          (call-interactively (lookup-key keymap (kbd "c")))
          (should (string= draft (mevedel-view--input-text)))
          (should (= (point)
                     (+ (mevedel-view--input-start) point-offset)))))))

  :doc "directive approval shows only request-local controls and preserves the draft"
  (mevedel-view-test--with-buffers
    (let* ((session (mevedel-session--create
                     :authority-mode 'pid-lock
                     :name "test" :permission-mode 'edits))
           (selection (mevedel-plan-mode--default-selection session))
           (entry (mevedel-plan-mode--approval-entry
                   "# Directive plan" data-buf session selection))
           (draft "> first line\nsecond line"))
      (plist-put entry :directive t)
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (with-current-buffer view-buf
        (setq-local mevedel--session session)
        (mevedel-view-test--insert-composer-draft draft 4))
      (setf (mevedel-session-pending-plan-approval session) entry)
      (with-current-buffer data-buf
        (mevedel-plan-approval-render session))
      (with-current-buffer view-buf
        (let* ((descriptor
                (gethash (plist-get entry :interaction-id)
                         mevedel-view--interaction-descriptors))
               (body (plist-get descriptor :body))
               (keymap (plist-get descriptor :keymap)))
          (dolist (text '("Directive implementation" "Mode" "Model"
                          "Skills" "Instructions" "implement" "feedback"
                          "cancel"))
            (should (string-search text body)))
          (dolist (text '("Location" "Context" "Execution" "Budget" "hide"))
            (should-not (string-search text body)))
          (should-not (lookup-key keymap (kbd "q")))
          (call-interactively (lookup-key keymap (kbd "m")))
          (should (string= draft (mevedel-view--input-text)))))))

  :doc "hides, rebuilds, reopens, and cancels a pending approval"
  (mevedel-view-test--with-buffers
    (let* ((selection '(:location here :context current
                        :execution direct :mode ask
                        :goal-token-budget nil))
           (session
            (mevedel-session--create
             :authority-mode 'pid-lock
             :name "test"
             :plan-mode t
             :plan-metadata
             (list :status 'proposed :proposal-id '(1 2 "hash")
                   :selection selection)))
           (entry
            (mevedel-plan-mode--approval-entry
             "# Plan" data-buf session selection))
           (draft "> first line\nsecond line")
           (draft-offset (length "> first")))
      (with-current-buffer data-buf
        (setq-local mevedel--session session)
        (mevedel-plan-approval-present entry session))
      (with-current-buffer view-buf
        (setq-local mevedel--session session)
        (let* ((id (plist-get entry :interaction-id))
               (descriptor
                (gethash id mevedel-view--interaction-descriptors))
               (keymap (plist-get descriptor :keymap)))
          (call-interactively (lookup-key keymap (kbd "q")))
          (should (mevedel-session-pending-plan-approval session))
          (should (plist-get
                   (mevedel-session-pending-plan-approval session)
                   :hidden))
          (should-not (gethash id mevedel-view--interaction-descriptors))
          (should (string-match-p "1 plan pending" (buffer-string)))
          (mevedel-view-test--insert-composer-draft draft draft-offset)
          (mevedel-view--interaction-rebuild)
          (should-not (gethash id mevedel-view--interaction-descriptors))
          (should (string= draft (mevedel-view--input-text)))
          (should (= (point)
                     (+ (mevedel-view--input-start) draft-offset)))
          (goto-char (point-min))
          (search-forward "1 plan")
          (let* ((position (match-beginning 0))
                 (counter-map (get-text-property position 'keymap))
                 (show (and counter-map
                            (lookup-key counter-map (kbd "RET")))))
            (should show)
            (should (lookup-key counter-map [mouse-1]))
            (goto-char position)
            (call-interactively show))
          (should-not (plist-get
                       (mevedel-session-pending-plan-approval session)
                       :hidden))
          (setq descriptor
                (gethash id mevedel-view--interaction-descriptors))
          (should descriptor)
          (should (= (point)
                     (plist-get
                      (mevedel-view-zone-fragment-bounds 'interaction id)
                      :start)))
          (should (string= draft (mevedel-view--input-text)))
          (goto-char (point-max))
          (condition-case nil
              (call-interactively (key-binding (kbd "C-g")))
            (quit nil))
          (should-not (mevedel-session-pending-plan-approval session))
          (should-not
           (string-match-p "plan pending" (buffer-string)))
          (let ((metadata (mevedel-session-plan-metadata session)))
            (should (eq 'draft (plist-get metadata :status)))
            (should-not (plist-member metadata :selection))))))))

(mevedel-deftest mevedel-plan-mode--feedback-draft
  (:doc "replaces the composer with an editable replacement-plan request")
  ,test
  (test)
  (mevedel-view-test--with-buffers
    (let ((session
           (mevedel-session--create
            :authority-mode 'pid-lock
            :name "test" :plan-mode t
            :plan-metadata '(:status draft :path "plans/current.md"))))
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (with-current-buffer view-buf
        (setq-local mevedel--session session)
        (mevedel-view-test--insert-composer-draft "old draft"))
      (mevedel-plan-mode--feedback-draft data-buf)
      (with-current-buffer view-buf
        (let ((draft (mevedel-view--input-text)))
          (should (string-match-p "Plan feedback:" draft))
          (should (string-match-p "complete replacement" draft))
          (should (string-match-p "local://plans/current.md" draft))
          (should-not (string-match-p "local/plans/current.md" draft))
          (should-not (string-match-p "old draft" draft)))))))

(mevedel-deftest mevedel-plan-mode--read-worktree-branch
  (:doc "collects the generated default and validates before acceptance")
  ,test
  (test)
  (let* ((directory (file-name-as-directory default-directory))
         (session (mevedel-session--create
                   :authority-mode 'pid-lock
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
                   :authority-mode 'pid-lock
                   :name "test" :save-path save-dir :plan-mode t
                   :plan-metadata
                   '(:selection (:location here :context current
                                 :execution goal :mode edits
                                 :goal-token-budget 2468)))))
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
                           :execution goal :mode edits
                           :goal-token-budget 2468)
                         (plist-get
                          (mevedel-session-pending-plan-approval session)
                          :selection)))
          (should (equal 'proposed
                         (plist-get (mevedel-session-plan-metadata session)
                                    :status))))
      (delete-directory save-dir t)))

  :doc "tool output alone cannot create a proposal"
  (let ((session (mevedel-session--create :authority-mode 'pid-lock :name "test" :plan-mode t))
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
  (let ((session (mevedel-session--create :authority-mode 'pid-lock :name "test" :plan-mode t))
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
         (path (file-name-concat save-dir "local" "plans" "current.md"))
         (plan "# Restored plan")
         (hash (mevedel-plan-hash plan))
         (selection '(:location here :context current
                      :execution goal :mode edits
                      :model-provider "OpenAI:gpt-5"
                      :reasoning-effort nil
                      :goal-token-budget 1357))
         (session
          (mevedel-session--create
           :authority-mode 'pid-lock
           :name "test" :save-path save-dir :plan-mode t
           :plan-metadata
           (list :path "local/plans/current.md" :hash hash :status 'proposed
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

  :doc "demotes persisted proposals without an implementation model snapshot"
  (let* ((save-dir (make-temp-file "mevedel-plan-restore-old-" t))
         (path (file-name-concat save-dir "local" "plans" "current.md"))
         (plan "# Old proposal")
         (hash (mevedel-plan-hash plan))
         (session
          (mevedel-session--create
           :authority-mode 'pid-lock
           :name "test" :save-path save-dir :plan-mode t
           :plan-metadata
           (list :path "local/plans/current.md" :hash hash :status 'proposed
                 :proposal-id (list 1 2 hash)
                 :selection '(:location here :context current
                              :execution direct :mode ask)))))
    (unwind-protect
        (progn
          (make-directory (file-name-directory path) t)
          (write-region plan nil path nil 'silent)
          (should-not
           (mevedel-plan-mode-restore-pending-approval session))
          (should (eq 'draft
                      (plist-get (mevedel-session-plan-metadata session)
                                 :status)))
          (should-not
           (plist-member (mevedel-session-plan-metadata session)
                         :selection))
          (mevedel-skills-test--with-model-backends
            (with-temp-buffer
              (setq-local mevedel--session session
                          gptel-backend (gptel-get-backend "Balanced")
                          gptel-model 'balanced-model
                          gptel-reasoning-effort 'high)
              (insert "<proposed_plan>\n# Replacement\n</proposed_plan>\n")
              (add-text-properties (point-min) (point-max) '(gptel response))
              (cl-letf (((symbol-function 'mevedel-plan-approval-render)
                         #'ignore))
                (mevedel-plan-mode--post-response (point-min) (point-max)))
              (let ((replacement
                     (plist-get (mevedel-session-plan-metadata session)
                                :selection)))
                (should (equal "Balanced:balanced-model"
                               (plist-get replacement :model-provider)))
                (should (eq 'high
                            (plist-get replacement
                                       :reasoning-effort)))))))
      (delete-directory save-dir t)))

  :doc "demotes a proposed artifact whose durable identity no longer agrees"
  (let* ((save-dir (make-temp-file "mevedel-plan-restore-bad-" t))
         (path (file-name-concat save-dir "local" "plans" "current.md"))
         (hash (mevedel-plan-hash "# Original"))
         (session
          (mevedel-session--create
           :authority-mode 'pid-lock
           :name "test" :save-path save-dir :plan-mode t
           :plan-metadata
           (list :path "local/plans/current.md" :hash hash :status 'proposed
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
  (:doc "accepts immutably and retains Direct recovery until success")
  ,test
  (test)
  (let* ((save-dir (make-temp-file "mevedel-plan-direct-" t))
         (session (mevedel-session--create
                   :authority-mode 'pid-lock
                   :name "test" :save-path save-dir
                   :permission-mode 'edits :plan-mode t))
         (data-buffer (generate-new-buffer " *plan-direct-data*"))
         (view-buffer (generate-new-buffer " *plan-direct-view*"))
         hook-input implementation request-fsm)
    (unwind-protect
        (progn
          (with-current-buffer data-buffer
            (setq-local mevedel--session session
                        mevedel-permission-mode 'edits))
          (cl-letf (((symbol-function 'mevedel-view--interaction-target-buffer)
                     (lambda (_buffer) view-buffer))
                    ((symbol-function 'mevedel-view--submit-planned-input)
                     (lambda (input _before _blocked callback &optional _after)
                       (setq hook-input input)
                       (funcall callback
                                (mevedel-prompt-submission-create
                                 :input input :state 'committed
                                 :outcome
                                 (list :model-input input
                                       :transcript-input input)))))
                    ((symbol-function
                      'mevedel-plan-handoff--apply-model-policy)
                     #'ignore)
                    ((symbol-function 'mevedel-plan-handoff--persist) #'ignore)
                    ((symbol-function 'mevedel--implement-plan)
                     (lambda (action)
                       (setq implementation action
                             request-fsm (gptel-make-fsm :info nil)))))
            (mevedel-plan-mode--approval-callback
             "# Accepted\n\nDo it." data-buffer session
             '(:accept t
               :selection (:location here :context current
                           :execution direct :mode full-auto
                           :model-provider "OpenAI:gpt-5"))))
          (let* ((metadata (mevedel-session-plan-metadata session))
                 (accepted
                  (mevedel-plan-artifact-path
                   session (list :path (plist-get metadata :accepted-path))))
                 (address (mevedel-plan-resource-address
                           (plist-get metadata :accepted-path))))
            (should-not (mevedel-session-plan-mode session))
            (should (eq 'full-auto
                        (mevedel-session-permission-mode session)))
            (should (eq 'accepted (plist-get metadata :status)))
            (should-not (plist-member metadata :verification-pending))
            (should-not (plist-member metadata :accepted-absolute-path))
            (should (file-exists-p accepted))
            (should (string-match-p (regexp-quote address) hook-input))
            (should-not (string-match-p (regexp-quote accepted) hook-input))
            (should (string-match-p "# Accepted" hook-input))
            (should (plist-member metadata :implementation-retry))
            (should-not (mevedel-session-goal session))
            (cl-letf (((symbol-function 'mevedel-plan-handoff--persist)
                       #'ignore))
              (mevedel-plan-handoff-settle-request request-fsm 'success))
            (should-not
             (plist-member (mevedel-session-plan-metadata session)
                           :implementation-retry))))
      (when (buffer-live-p view-buffer) (kill-buffer view-buffer))
      (when (buffer-live-p data-buffer) (kill-buffer data-buffer))
      (delete-directory save-dir t)))

  :doc "constructs Here Goal with immutable contract and canonical kickoff"
  (let* ((save-dir (make-temp-file "mevedel-plan-goal-" t))
         (session (mevedel-session--create
                   :authority-mode 'pid-lock
                   :name "test" :save-path save-dir
                   :permission-mode 'edits :plan-mode t))
         (data-buffer (generate-new-buffer " *plan-goal-data*"))
         (view-buffer (generate-new-buffer " *plan-goal-view*"))
         (mevedel-goal-token-budget 1234)
         hook-input hook-display implementation reserved-id)
    (unwind-protect
        (progn
          (with-current-buffer data-buffer
            (setq-local mevedel--session session
                        mevedel-permission-mode 'edits
                        mevedel-goal-token-budget 1234))
          (cl-letf (((symbol-function 'mevedel-view--interaction-target-buffer)
                     (lambda (_buffer) view-buffer))
                    ((symbol-function 'mevedel-view--submit-planned-input)
                     (lambda (input _before _blocked callback &optional _after)
                       (setq hook-input input
                             hook-display "Implement accepted plan as Goal")
                       (funcall callback
                                (mevedel-prompt-submission-create
                                 :input input :state 'committed
                                 :outcome
                                 (list :model-input input
                                       :transcript-input input)))))
                    ((symbol-function
                      'mevedel-plan-handoff--apply-model-policy)
                     #'ignore)
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
                    ((symbol-function 'mevedel-session-artifacts-save)
                     #'ignore)
                    ((symbol-function 'mevedel--implement-plan)
                     (lambda (action) (setq implementation action))))
            (mevedel-plan-mode--approval-callback
             "Free-form accepted plan." data-buffer session
             '(:accept t
               :selection (:location here :context current
                           :execution goal :mode full-auto
                           :goal-token-budget 4321
                           :model-provider "OpenAI:gpt-5"))))
          (let* ((metadata (mevedel-session-plan-metadata session))
                 (goal (mevedel-session-goal session))
                 (accepted
                  (mevedel-plan-artifact-path
                   session (list :path (plist-get metadata :accepted-path))))
                 (address (mevedel-plan-resource-address
                           (plist-get metadata :accepted-path))))
            (should (equal mevedel-plan-handoff--accepted-goal-objective
                           (mevedel-goal-objective goal)))
            (should (equal reserved-id (mevedel-goal-id goal)))
            (should (equal (plist-get metadata :accepted-path)
                           (mevedel-goal-plan-reference goal)))
            (should (= 4321 (mevedel-goal-token-budget goal)))
            (should-not (plist-member metadata :accepted-absolute-path))
            (should (string-match-p (regexp-quote address) hook-input))
            (should-not (string-match-p (regexp-quote accepted) hook-input))
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
                   :authority-mode 'pid-lock
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
                           :execution goal :mode full-auto
                           :goal-token-budget 7000
                           :branch "plan/topic"))))
          (let ((retry
                 (plist-get (mevedel-session-plan-metadata session)
                            :implementation-retry)))
            (should dispatched)
            (should (eq 'ask (mevedel-session-permission-mode session)))
            (should (eq 'prepare-worktree (plist-get retry :step)))
            (should-not (plist-member retry :goal-token-budget))
            (should (= 7000
                       (plist-get (plist-get retry :selection)
                                  :goal-token-budget)))
            (should (equal "plan/topic"
                           (plist-get (plist-get retry :selection)
                                      :branch)))))
      (when (buffer-live-p data-buffer) (kill-buffer data-buffer))
      (delete-directory save-dir t)))

  :doc "feedback preserves Plan and selection while requiring a replacement"
  (let* ((selection '(:location here :context current
                      :execution goal :mode edits
                      :goal-token-budget 4321))
         (session (mevedel-session--create
                   :authority-mode 'pid-lock
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
                   :authority-mode 'pid-lock
                   :name "test" :plan-mode t
                   :plan-metadata
                   '(:status proposed :proposal-id (1 2 "h")
                     :selection (:location here :context current
                                 :execution goal :mode ask
                                 :goal-token-budget 4321)))))
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
  (let ((explicit (mevedel-session--create :authority-mode 'pid-lock :name "explicit"))
        (local (mevedel-session--create :authority-mode 'pid-lock :name "local"))
        (paired (mevedel-session--create :authority-mode 'pid-lock :name "paired"))
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
          :authority-mode 'pid-lock
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
           "one implementation turn"
           (mevedel-plan-mode--execution-description 'direct)))
  (should (string-match-p
           "continue until complete"
           (mevedel-plan-mode--execution-description 'goal))))

(mevedel-deftest mevedel-plan-mode--demote-proposal
  (:doc "demotes proposals and optionally discards their selection")
  ,test
  (test)
  (let ((session
         (mevedel-session--create
          :authority-mode 'pid-lock
          :name "main"
          :plan-metadata '(:status proposed :proposal-id (1 2 "h")
                           :selection (:mode edits)))))
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
  (should (eq 'edits (mevedel-plan-mode--next-mode 'ask)))
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
  (let* ((session (mevedel-session--create :authority-mode 'pid-lock :name "main"))
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
           :authority-mode 'pid-lock
           :name "main" :plan-mode t :permission-mode 'ask))
         (chat-buffer (generate-new-buffer " *mevedel-plan-accept-data*"))
         (view-buffer (generate-new-buffer " *mevedel-plan-accept-view*"))
         status dispatched)
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel-plan-accept)
                   (lambda (&rest _)
                     '(:accepted (:path "accepted.md"
                                   :hash "h"))))
                  ((symbol-function 'mevedel-view--interaction-target-buffer)
                   (lambda (_) view-buffer))
                  ((symbol-function 'mevedel-view--update-spinner)
                   (lambda (text owner) (setq status (list text owner))))
                  ((symbol-function 'mevedel-plan-handoff--dispatch-accepted)
                   (lambda (&rest _) (setq dispatched t))))
          (mevedel-plan-mode--accept
           "# Plan" chat-buffer session
           '(:location here :context current :execution direct :mode edits))
          (should-not (mevedel-session-plan-mode session))
          (should dispatched)
          (should (equal '("Preparing implementation..." plan-preparation)
                         status)))
      (kill-buffer view-buffer)
      (kill-buffer chat-buffer))))

(mevedel-deftest mevedel-plan-approval--current-session
  (:doc "loads its owner instead of assuming a caller already did")
  ;; Buffer cleanup reaches this resolver, and a partial load order left it
  ;; calling a function whose module nothing had required: the abort then
  ;; failed with `void-function' and the session was never aborted.
  (let ((saved (symbol-function 'mevedel-queue--current-session)))
    (unwind-protect
        (progn
          (fmakunbound 'mevedel-queue--current-session)
          (setq features (delq 'mevedel-queue features))
          (should-not (mevedel-plan-approval--current-session)))
      (unless (fboundp 'mevedel-queue--current-session)
        (fset 'mevedel-queue--current-session saved))
      (require 'mevedel-queue))))

(provide 'test-mevedel-plan-mode)
;;; test-mevedel-plan-mode.el ends here
