;;; test-mevedel-plan.el --- Tests for mevedel-plan.el -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'mevedel-plan)
(require 'mevedel-interaction-prompt)
(require 'mevedel-prompt-submission)
(require 'mevedel-session-persistence)
(require 'mevedel-structs)
(require 'mevedel-view)
(require 'mevedel-view-composer)
(require 'mevedel-view-interaction)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(mevedel-deftest mevedel-plan-mode-active-p
  (:doc "reads Plan state from the explicit or current session")
  ,test
  (test)
  (let ((session (mevedel-session--create :name "test" :plan-mode t)))
    (should (mevedel-plan-mode-active-p session))
    (let ((mevedel--session session))
      (should (mevedel-plan-mode-active-p)))))

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
                   (lambda (&rest _) (setq rerendered t))))
          (mevedel-plan-mode--render-approval entry)
          (let ((body (plist-get descriptor :body))
                (keymap (plist-get descriptor :keymap)))
            (dolist (text '("Location   Here" "Context    Current"
                            "Execution  Direct" "Mode       auto"))
              (should (string-match-p text body)))
            (call-interactively (lookup-key keymap (kbd "c")))
            (should (eq 'fresh (plist-get selection :context)))
            (call-interactively (lookup-key keymap (kbd "m")))
            (should rerendered)
            (should (eq 'full-auto (plist-get selection :mode)))
            (should (eq 'auto (mevedel-session-permission-mode session)))
            (call-interactively (lookup-key keymap (kbd "RET")))
            (should (equal '(:accept t
                             :selection (:location here :context fresh
                                         :execution direct :mode full-auto))
                           outcome))))
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
      (when (buffer-live-p data-buffer) (kill-buffer data-buffer)))))

(mevedel-deftest mevedel-plan-validate
  (:doc "normalizes nonblank plans and rejects invalid input")
  ,test
  (test)
  (should (equal "# Plan" (mevedel-plan-validate "# Plan")))
  (should-error (mevedel-plan-validate "  \n"))
  (should-error (mevedel-plan-validate nil)))

(mevedel-deftest mevedel-plan-extract-proposed
  (:doc "extracts the last line-oriented proposed-plan block")
  ,test
  (test)
  (should (equal
           "second"
           (mevedel-plan-extract-proposed
            "<proposed_plan>\nfirst\n</proposed_plan>\n<proposed_plan>\nsecond\n</proposed_plan>")))
  (should-not
   (mevedel-plan-extract-proposed
    "text <proposed_plan>\nnot a plan\n</proposed_plan>"))
  :doc "accepts a concise authoritative reference without template headings"
  (should
   (equal
    "Implement ticket 2 in .scratch/feature/tickets.md."
    (mevedel-plan-extract-proposed
     "<proposed_plan>\nImplement ticket 2 in .scratch/feature/tickets.md.\n</proposed_plan>"))))

(mevedel-deftest mevedel-plan-strip-proposed
  (:doc "removes complete and streaming proposed-plan blocks")
  ,test
  (test)
  (should (equal
           "Intro\nTail"
           (mevedel-plan-strip-proposed
            "Intro\n<proposed_plan>\n# Plan\n</proposed_plan>\nTail")))
  (should (equal
           "Intro"
           (mevedel-plan-strip-proposed
            "Intro\n<proposed_plan>\n# Streaming plan\n"))))

(mevedel-deftest mevedel-plan--metadata-put
  (:doc "updates one plan metadata key without dropping others")
  ,test
  (test)
  (let ((session
         (mevedel-session--create :name "test" :plan-metadata '(:old t))))
    (mevedel-plan--metadata-put session :new 1)
    (should (equal '(:old t :new 1)
                   (mevedel-session-plan-metadata session)))))

(mevedel-deftest mevedel-plan-hash
  (:doc "ignores trailing whitespace in stable plan hashes")
  ,test
  (test)
  (should (equal (mevedel-plan-hash "# Plan")
                 (mevedel-plan-hash "# Plan\n"))))

(mevedel-deftest mevedel-plan-current-path
  (:doc "returns the current artifact path below the session directory")
  ,test
  (test)
  (let ((save-dir (make-temp-file "mevedel-plan-path-" t)))
    (unwind-protect
        (let ((session
               (mevedel-session--create :name "test" :save-path save-dir)))
          (should (equal (file-name-concat save-dir "plans" "current.md")
                         (mevedel-plan-current-path session)))
          (should (equal (file-name-concat save-dir "goals" "g1" "current.md")
                         (mevedel-plan-current-path
                          session nil "goals/g1/current.md"))))
      (delete-directory save-dir t))))

(mevedel-deftest mevedel-plan--metadata-path
  (:doc "resolves the current artifact from persisted metadata")
  ,test
  (test)
  (let ((save-dir (make-temp-file "mevedel-plan-metadata-path-" t)))
    (unwind-protect
        (let ((session
               (mevedel-session--create
                :name "test" :save-path save-dir
                :plan-metadata '(:path "plans/current.md"))))
          (should (equal (file-name-concat save-dir "plans" "current.md")
                         (mevedel-plan--metadata-path session))))
      (delete-directory save-dir t))))

(mevedel-deftest mevedel-plan-write-current
  (:doc "writes a validated current artifact and returns its descriptor")
  ,test
  (test)
  (let ((save-dir (make-temp-file "mevedel-plan-write-" t)))
    (unwind-protect
        (with-temp-buffer
          (let* ((session
                  (mevedel-session--create :name "test" :save-path save-dir))
                 (artifact
                  (mevedel-plan-write-current "# Plan" session
                                              (current-buffer)
                                              "goals/g1/current.md")))
            (should (file-exists-p (plist-get artifact :absolute-path)))
            (should (equal "goals/g1/current.md"
                           (plist-get artifact :path)))
            (should (equal 'presented
                           (plist-get (mevedel-session-plan-metadata session)
                                      :status)))
            (should
             (equal (mevedel-plan-hash "# Plan")
                    (plist-get (mevedel-session-plan-metadata session)
                               :hash)))
            (should-not
             (plist-member (mevedel-session-plan-metadata session)
                           :presented-plan-hashes))))
      (delete-directory save-dir t))))

(mevedel-deftest mevedel-plan-archive-accepted
  (:doc "reuses an identical deterministic accepted artifact")
  ,test
  (test)
  (let ((save-dir (make-temp-file "mevedel-plan-archive-" t)))
    (unwind-protect
        (with-temp-buffer
          (let* ((session
                  (mevedel-session--create :name "test" :save-path save-dir))
                 (artifact
                  (mevedel-plan-write-current
                   "# Plan" session (current-buffer) "current.md"))
                 (accepted
                  (mevedel-plan-archive-accepted
                   artifact session "goals/g1/cycle-001-plan.md")))
            (should (file-exists-p (plist-get accepted :absolute-path)))
            (should (equal (plist-get artifact :hash)
                           (plist-get accepted :hash)))
            (should (equal "goals/g1/cycle-001-plan.md"
                           (plist-get accepted :path)))
            (should
             (equal accepted
                    (mevedel-plan-archive-accepted
                     artifact session
                     "goals/g1/cycle-001-plan.md")))
            (let ((different
                   (mevedel-plan-write-current
                    "# Different" session (current-buffer) "current.md")))
              (should-error
               (mevedel-plan-archive-accepted
                different session "goals/g1/cycle-001-plan.md")
               :type 'error))))
      (delete-directory save-dir t))))

(mevedel-deftest mevedel-plan-current-body
  (:doc "reads the normalized current artifact body")
  ,test
  (test)
  (let* ((save-dir (make-temp-file "mevedel-plan-body-" t))
         (path (file-name-concat save-dir "current.md")))
    (unwind-protect
        (progn
          (write-region "# Plan" nil path nil 'silent)
          (let ((session
                 (mevedel-session--create
                  :name "test" :save-path save-dir
                  :plan-metadata '(:path "current.md"))))
            (should (equal "# Plan" (mevedel-plan-current-body session)))))
      (delete-directory save-dir t))))

(mevedel-deftest mevedel-plan-current-exists-p
  (:doc "reports whether the recorded current artifact exists")
  ,test
  (test)
  (let* ((save-dir (make-temp-file "mevedel-plan-exists-" t))
         (path (file-name-concat save-dir "current.md"))
         (session
          (mevedel-session--create
           :name "test" :save-path save-dir
           :plan-metadata '(:path "current.md"))))
    (unwind-protect
        (progn
          (should-not (mevedel-plan-current-exists-p session))
          (write-region "# Plan" nil path nil 'silent)
          (should (mevedel-plan-current-exists-p session)))
      (delete-directory save-dir t))))

(mevedel-deftest mevedel-plan-mark-accepted
  (:doc "records current and accepted artifact descriptors")
  ,test
  (test)
  (let ((session (mevedel-session--create :name "test" :turn-count 3)))
    (mevedel-plan-mark-accepted
     session '(:path "current.md" :absolute-path "/tmp/current.md")
     '(:path "accepted.md" :absolute-path "/tmp/accepted.md" :hash "h"))
    (let ((metadata (mevedel-session-plan-metadata session)))
      (should (eq 'accepted (plist-get metadata :status)))
      (should (= 3 (plist-get metadata :accepted-turn)))
      (should (equal "accepted.md" (plist-get metadata :accepted-path)))
      (should (equal "h" (plist-get metadata :accepted-hash))))))

(mevedel-deftest mevedel-plan-accept
  (:doc "accepts a plan without depending on Goal controller state")
  ,test
  (test)
  (let ((save-dir (make-temp-file "mevedel-plan-accept-" t)))
    (unwind-protect
        (with-temp-buffer
          (let ((session (mevedel-session--create
                          :name "test"
                          :save-path save-dir
                          :permission-mode 'full-auto
                          :turn-count 4)))
            (let* ((result (mevedel-plan-accept
                            "# Plan\n\nDo it." session (current-buffer)
                            nil
                            "goals/g1/current.md"
                            "goals/g1/cycle-001-plan.md"))
                   (current (plist-get result :current))
                   (accepted (plist-get result :accepted))
                   (metadata (mevedel-session-plan-metadata session)))
              (should (eq 'accepted (plist-get metadata :status)))
              (should (file-exists-p
                       (plist-get current :absolute-path)))
              (should (file-exists-p
                       (plist-get accepted :absolute-path)))
              (should (equal (mevedel-plan-hash "# Plan\n\nDo it.")
                             (plist-get current :hash)))
              (should (equal "goals/g1/cycle-001-plan.md"
                             (plist-get accepted :path))))))
      (delete-directory save-dir t))))

(mevedel-deftest mevedel-plan-implementation-input
  (:doc "builds and validates explicit implementation input")
  ,test
  (test)
  (let ((path (make-temp-file "mevedel-plan-input-")))
    (unwind-protect
        (let ((input
               (mevedel-plan-implementation-input
                'focused (list :absolute-path path)
                'auto "Goal ID: g1")))
          (should (equal 'focused (plist-get input :context)))
          (should (equal path (plist-get input :plan-file)))
          (should (equal 'auto
                         (plist-get input :permission-mode)))
          (should-error
           (mevedel-plan-implementation-input
            'unknown (list :absolute-path path) 'ask "Goal ID: g1"))
          (should-error
           (mevedel-plan-implementation-input
            'focused (list :absolute-path path) 'ask nil))
          (should-error
           (mevedel-plan-implementation-input
            'full (list :absolute-path path) 'ask nil)))
      (delete-file path))))

(provide 'test-mevedel-plan)
;;; test-mevedel-plan.el ends here
