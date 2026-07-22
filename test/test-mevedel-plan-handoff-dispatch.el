;;; test-mevedel-plan-handoff-dispatch.el -- Dispatch tests for mevedel-plan-handoff.el -*- lexical-binding: t -*-

;;; Commentary:

;; Durable accepted-Plan dispatch and recovery tests.

;;; Code:

(require 'gptel-request)
(require 'mevedel-plan)
(require 'mevedel-plan-handoff)
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

(mevedel-deftest mevedel-plan-handoff--dispatch-accepted
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
                :accepted
                (list :path "plans/accepted.md"
                      :absolute-path accepted-path
                      :hash (mevedel-plan-hash body))))
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
          (cl-letf (((symbol-function 'mevedel-plan-handoff--persist) #'ignore)
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
                       (should (eq 'full-auto
                                   (plist-get action :permission-mode)))
                       (when (= attempts 1)
                         (error "Transport refused"))
                       'started)))
            (mevedel-plan-handoff--dispatch-accepted session data-buffer)
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
            :accepted (:path "plans/accepted.md"
                       :absolute-path "/tmp/accepted.md"
                       :hash "hash")))
         (session
          (mevedel-session--create
           :name "test"
           :plan-metadata
           (list :status 'accepted :implementation-retry record)))
         (data-buffer (generate-new-buffer " *plan-fresh-failure*")))
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel-plan-handoff--persist) #'ignore)
                  ((symbol-function
                    'mevedel-session-persistence-start-fresh-segment)
                   (lambda (&rest _) (error "Rotation failed"))))
          (mevedel-plan-handoff--dispatch-accepted session data-buffer)
          (let ((retry
                 (plist-get (mevedel-session-plan-metadata session)
                            :implementation-retry)))
            (should (eq 'prepare-context (plist-get retry :step)))
            (should (equal "Rotation failed" (plist-get retry :failure)))))
      (when (buffer-live-p data-buffer) (kill-buffer data-buffer))))

  :doc "constructs a prepared Worktree Goal only in the target session"
  (let* ((source-save (make-temp-file "mevedel-plan-goal-source-" t))
         (target-save (make-temp-file "mevedel-plan-goal-target-" t))
         (target-path (file-name-concat target-save "plans" "accepted.md"))
         (body "Worktree Goal plan")
         (selection '(:location worktree :context fresh
                      :execution goal :mode full-auto :branch "plan/goal"))
         (record
          (list :step 'submit :selection selection :goal-id "reserved-goal"
                :goal-token-budget 7000
                :target-save-path target-save :target-session-id "target"
                :target-directory default-directory
                :target-accepted
                (list :path "plans/accepted.md" :absolute-path target-path
                      :hash (mevedel-plan-hash body))))
         (source-session
          (mevedel-session--create
           :name "source" :save-path source-save :permission-mode 'ask
           :plan-metadata
           (list :status 'accepted :implementation-retry record)))
         (target-session
          (mevedel-session--create
           :name "target" :session-id "target" :save-path target-save
           :working-directory default-directory :permission-mode 'full-auto))
         (source-buffer (generate-new-buffer " *plan-goal-source*"))
         (target-buffer (generate-new-buffer " *plan-goal-target*"))
         (view-buffer (generate-new-buffer " *plan-goal-target-view*")))
    (unwind-protect
        (progn
          (make-directory (file-name-directory target-path) t)
          (write-region body nil target-path nil 'silent)
          (with-current-buffer source-buffer
            (setq-local mevedel--session source-session))
          (with-current-buffer target-buffer
            (setq-local mevedel--session target-session
                        mevedel-goal-token-budget 7000))
          (cl-letf (((symbol-function
                      'mevedel-plan-handoff--worktree-target-buffer)
                     (lambda (_record) target-buffer))
                    ((symbol-function 'mevedel-view--interaction-target-buffer)
                     (lambda (buffer)
                       (should (eq target-buffer buffer))
                       view-buffer))
                    ((symbol-function 'mevedel-view--run-prompt-submit-hook)
                     (lambda (input _display callback &rest _)
                       (funcall callback
                                (mevedel-prompt-submission-create
                                 :input input :state 'committed))))
                    ((symbol-function 'mevedel-session-persistence-save)
                     #'ignore)
                    ((symbol-function 'mevedel-plan-handoff--persist) #'ignore)
                    ((symbol-function 'mevedel--implement-plan) #'ignore))
            (mevedel-plan-handoff--dispatch-accepted
             source-session source-buffer))
          (should-not (mevedel-session-goal source-session))
          (let ((goal (mevedel-session-goal target-session)))
            (should goal)
            (should (= 7000 (mevedel-goal-token-budget goal)))
            (should (equal "plans/accepted.md"
                           (mevedel-goal-plan-reference goal))))
          (should (eq 'ask
                      (mevedel-session-permission-mode source-session))))
      (dolist (buffer (list view-buffer target-buffer source-buffer))
        (when (buffer-live-p buffer) (kill-buffer buffer)))
      (delete-directory target-save t)
      (delete-directory source-save t)))

  :doc "reuses a matching Goal left durable before source retry cleanup"
  (let* ((save-dir (make-temp-file "mevedel-plan-goal-crash-" t))
         (accepted-path (file-name-concat save-dir "plans" "accepted.md"))
         (body "Crash-window plan")
         (goal (mevedel-goal--create
                :id "reserved" :status 'paused :reason "session resumed"
                :plan-reference "plans/accepted.md"))
         (record
          (list :step 'submit :goal-id "reserved"
                :selection '(:location here :context current
                              :execution goal :mode auto)
                :accepted
                (list :path "plans/accepted.md"
                      :absolute-path accepted-path
                      :hash (mevedel-plan-hash body))))
         (session
          (mevedel-session--create
           :name "main" :save-path save-dir :goal goal
           :plan-metadata
           (list :status 'accepted :implementation-retry record)))
         (data-buffer (generate-new-buffer " *plan-goal-crash-data*"))
         (view-buffer (generate-new-buffer " *plan-goal-crash-view*"))
         implemented)
    (unwind-protect
        (progn
          (make-directory (file-name-directory accepted-path) t)
          (write-region body nil accepted-path nil 'silent)
          (with-current-buffer data-buffer
            (setq-local mevedel--session session))
          (cl-letf (((symbol-function 'mevedel-plan-handoff--persist) #'ignore)
                    ((symbol-function 'mevedel-session-persistence-save) #'ignore)
                    ((symbol-function 'mevedel-view--interaction-target-buffer)
                     (lambda (_) view-buffer))
                    ((symbol-function 'mevedel-view--run-prompt-submit-hook)
                     (lambda (input _display callback &rest _)
                       (funcall callback
                                (mevedel-prompt-submission-create
                                 :input input :state 'committed))))
                    ((symbol-function 'mevedel-goal-create)
                     (lambda (&rest _) (ert-fail "Constructed duplicate Goal")))
                    ((symbol-function 'mevedel--implement-plan)
                     (lambda (_) (setq implemented t))))
            (mevedel-plan-handoff--dispatch-accepted session data-buffer))
          (should implemented)
          (should (eq goal (mevedel-session-goal session)))
          (should (eq 'active (mevedel-goal-status goal)))
          (should-not (mevedel-goal-reason goal))
          (should-not
           (plist-member (mevedel-session-plan-metadata session)
                         :implementation-retry)))
      (kill-buffer view-buffer)
      (kill-buffer data-buffer)
      (delete-directory save-dir t)))

  :doc "leaves a different unfinished target Goal untouched and retryable"
  (let* ((save-dir (make-temp-file "mevedel-plan-goal-conflict-" t))
         (accepted-path (file-name-concat save-dir "plans" "accepted.md"))
         (body "Conflicting Goal plan")
         (goal (mevedel-goal--create
                :id "other" :status 'paused :reason "owned"
                :plan-reference "plans/other.md"))
         (record
          (list :step 'submit :goal-id "reserved"
                :selection '(:location here :context current
                              :execution goal :mode auto)
                :accepted
                (list :path "plans/accepted.md"
                      :absolute-path accepted-path
                      :hash (mevedel-plan-hash body))))
         (session
          (mevedel-session--create
           :name "main" :save-path save-dir :goal goal
           :plan-metadata
           (list :status 'accepted :implementation-retry record)))
         (data-buffer (generate-new-buffer " *plan-goal-conflict-data*"))
         (view-buffer (generate-new-buffer " *plan-goal-conflict-view*")))
    (unwind-protect
        (progn
          (make-directory (file-name-directory accepted-path) t)
          (write-region body nil accepted-path nil 'silent)
          (with-current-buffer data-buffer
            (setq-local mevedel--session session))
          (cl-letf (((symbol-function 'mevedel-plan-handoff--persist) #'ignore)
                    ((symbol-function 'mevedel-view--interaction-target-buffer)
                     (lambda (_) view-buffer))
                    ((symbol-function 'mevedel-view--run-prompt-submit-hook)
                     (lambda (input _display callback &rest _)
                       (funcall callback
                                (mevedel-prompt-submission-create
                                 :input input :state 'committed))))
                    ((symbol-function 'mevedel--implement-plan)
                     (lambda (_) (ert-fail "Started conflicting Goal"))))
            (mevedel-plan-handoff--dispatch-accepted session data-buffer))
          (should (eq goal (mevedel-session-goal session)))
          (should (eq 'paused (mevedel-goal-status goal)))
          (should (equal "owned" (mevedel-goal-reason goal)))
          (should
           (string-match-p
            "unfinished Goal other; expected reserved Goal reserved"
            (plist-get
             (plist-get (mevedel-session-plan-metadata session)
                        :implementation-retry)
             :failure))))
      (kill-buffer view-buffer)
      (kill-buffer data-buffer)
      (delete-directory save-dir t)))

  :doc "pauses after kickoff startup failure and resumes held input first"
  (let* ((save-dir (make-temp-file "mevedel-plan-goal-kickoff-" t))
         (accepted-path (file-name-concat save-dir "plans" "accepted.md"))
         (body "Kickoff failure plan")
         (record
          (list :step 'submit :goal-id "reserved"
                :selection '(:location here :context current
                              :execution goal :mode auto)
                :accepted
                (list :path "plans/accepted.md"
                      :absolute-path accepted-path
                      :hash (mevedel-plan-hash body))))
         (session
          (mevedel-session--create
           :name "main" :save-path save-dir
           :queued-user-messages
           '((:input "steer first" :queued-at-goal-id "reserved"))
           :plan-metadata
           (list :status 'accepted :implementation-retry record)))
         (data-buffer (generate-new-buffer " *plan-goal-kickoff-data*"))
         (view-buffer (generate-new-buffer " *plan-goal-kickoff-view*"))
         scheduled)
    (unwind-protect
        (progn
          (make-directory (file-name-directory accepted-path) t)
          (write-region body nil accepted-path nil 'silent)
          (with-current-buffer data-buffer
            (setq-local mevedel--session session))
          (cl-letf (((symbol-function 'mevedel-plan-handoff--persist) #'ignore)
                    ((symbol-function 'mevedel-session-persistence-save)
                     #'ignore)
                    ((symbol-function 'mevedel-view--interaction-target-buffer)
                     (lambda (_) view-buffer))
                    ((symbol-function 'mevedel-view--run-prompt-submit-hook)
                     (lambda (input _display callback &rest _)
                       (funcall callback
                                (mevedel-prompt-submission-create
                                 :input input :state 'committed))))
                    ((symbol-function 'mevedel--implement-plan)
                     (lambda (_) (error "Transport offline"))))
            (mevedel-plan-handoff--dispatch-accepted session data-buffer))
          (let ((goal (mevedel-session-goal session)))
            (should (eq 'paused (mevedel-goal-status goal)))
            (should (equal "Transport offline" (mevedel-goal-reason goal)))
            (should-not
             (plist-member (mevedel-session-plan-metadata session)
                           :implementation-retry))
            (should
             (mevedel-view--queued-user-message-auto-drain-blocked-p session))
            (cl-letf (((symbol-function 'mevedel-session-persistence-save)
                       #'ignore)
                      ((symbol-function 'run-at-time)
                       (lambda (_delay _repeat function &rest args)
                         (push (cons function args) scheduled))))
              (with-current-buffer data-buffer
                (mevedel-goal-resume))
              (should (eq 'active (mevedel-goal-status goal)))
              (should (eq 'queued-user-message
                          (mevedel-goal-continue-if-idle
                           session data-buffer))))
            (should
             (seq-some
              (lambda (call)
                (eq #'mevedel-view--run-queued-user-message-drain
                    (car call)))
              scheduled))
            (should (equal "steer first"
                           (plist-get
                            (car (mevedel-session-queued-user-messages session))
                            :input)))))
      (kill-buffer view-buffer)
      (kill-buffer data-buffer)
      (delete-directory save-dir t)))

  :doc "Here/Summary caches one zero-tail compaction across dispatch retry"
  (let* ((save-dir (make-temp-file "mevedel-plan-summary-" t))
         (accepted-path (file-name-concat save-dir "plans" "accepted.md"))
         (body "# Accepted plan\n\nImplement the endpoint.")
         (selection '(:location here :context summary
                      :execution direct :mode auto))
         (record
          (list :step 'prepare-summary :selection selection
                :accepted
                (list :path "plans/accepted.md"
                      :absolute-path accepted-path
                      :hash (mevedel-plan-hash body))))
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
          (cl-letf (((symbol-function 'mevedel-plan-handoff--persist)
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
            (mevedel-plan-handoff--dispatch-accepted session data-buffer)
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
                :accepted
                (list :path "plans/accepted.md"
                      :absolute-path source-path
                      :hash (mevedel-plan-hash body))))
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
          (cl-letf (((symbol-function 'mevedel-plan-handoff--persist) #'ignore)
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
                       (cl-incf attempts)
                       (when (= attempts 1)
                         (error "Startup failed"))
                       'started)))
            (mevedel-plan-handoff--dispatch-accepted
             source-session source-buffer)
            (let* ((retry
                    (plist-get
                     (mevedel-session-plan-metadata source-session)
                     :implementation-retry))
                   (target-path
                    (plist-get (plist-get retry :target-accepted)
                               :absolute-path)))
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
                :accepted
                (list :path "plans/accepted.md"
                      :absolute-path source-path
                      :hash (mevedel-plan-hash body))))
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
              (((symbol-function 'mevedel-plan-handoff--persist) #'ignore)
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
            (mevedel-plan-handoff--dispatch-accepted
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
                      (plist-get (plist-get retry :target-accepted)
                                 :absolute-path)
                      prompt))
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

(provide 'test-mevedel-plan-handoff-dispatch)
;;; test-mevedel-plan-handoff-dispatch.el ends here
