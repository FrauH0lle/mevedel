;;; test-mevedel-plan-handoff-dispatch.el -- Dispatch tests for mevedel-plan-handoff.el -*- lexical-binding: t -*-

;;; Commentary:

;; Durable accepted-Plan dispatch and recovery tests.

;;; Code:

(require 'gptel-request)
(require 'mevedel-compact-evidence)
(require 'mevedel-compact-run)
(require 'mevedel-compact-target)
(require 'mevedel-context-summary)
(require 'mevedel-plan)
(require 'mevedel-plan-handoff)
(require 'mevedel-plan-mode)
(require 'mevedel-goal)
(require 'mevedel-interaction-prompt)
(require 'mevedel-pending-inputs)
(require 'mevedel-permissions)
(require 'mevedel-chat)
(require 'mevedel-prompt-submission)
(require 'mevedel-session-artifacts)
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
(require 'mevedel-session-test-support
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "mevedel-session-test-support"))
(require 'mevedel-compact)

(mevedel-deftest mevedel-plan-handoff--dispatch-accepted
  (:quiet t :doc "Here/Fresh rotates once, runs setup hooks, and retries only submission")
  ,test
  (test)
  (let* ((save-dir (make-temp-file "mevedel-plan-fresh-" t))
         (accepted-path (file-name-concat save-dir "local" "plans" "accepted-20260813-120000.md"))
         (current-path (file-name-concat save-dir "local" "plans" "current.md"))
         (body "# Immutable accepted plan")
         (selection '(:location here :context fresh
                      :execution direct :mode full-auto
                      :model-provider "Test:test-model"))
         (record
          (list :step 'prepare-context :selection selection
                :accepted
                (list :path "local/plans/accepted-20260813-120000.md"
                      :hash (mevedel-plan-hash body))))
         (session
          (mevedel-session--create
           :authority-mode 'pid-lock
           :name "test" :save-path save-dir :permission-mode 'ask
           :plan-metadata
           (list :status 'accepted :implementation-retry record)))
         (data-buffer (generate-new-buffer " *plan-fresh-data*"))
         (view-buffer (generate-new-buffer " *plan-fresh-view*"))
         (rotations 0)
         (attempts 0)
         hook-source
         request-fsm
         prompts)
    (unwind-protect
        (progn
          (make-directory (file-name-directory accepted-path) t)
          (write-region body nil accepted-path nil 'silent)
          (write-region "# Mutable replacement" nil current-path nil 'silent)
          (with-current-buffer data-buffer
            (setq-local mevedel--session session))
          (cl-letf (((symbol-function 'mevedel-session-artifacts-save) #'ignore)
                    ((symbol-function
                      'mevedel-session-artifacts-start-fresh-segment)
                     (lambda (&rest _)
                       (cl-incf rotations)
                       "segment-0002.chat.org"))
                    ((symbol-function 'mevedel--run-session-start-hooks)
                     (lambda (source) (setq hook-source source)))
                    ((symbol-function 'mevedel-view--interaction-target-buffer)
                     (lambda (_buffer) view-buffer))
                    ((symbol-function 'mevedel-view--submit-planned-input)
                     (lambda (input _before _blocked callback &optional _after)
                       (push input prompts)
                       (funcall callback
                                (mevedel-prompt-submission-create
                                 :input input :state 'committed
                                 :outcome
                                 (list :model-input input
                                       :transcript-input input)))))
                    ((symbol-function 'mevedel--implement-plan)
                     (lambda (action)
                       (cl-incf attempts)
                       (should (eq 'full-auto
                                   (plist-get (plist-get action :implementation-policy)
                                              :permission-mode)))
                       (should (eq 'ask (mevedel-session-permission-mode session)))
                       (when (= attempts 1)
                         (error "Transport refused"))
                       (setq request-fsm (gptel-make-fsm :info nil)))))
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
            (should
             (plist-member (mevedel-session-plan-metadata session)
                           :implementation-retry))
            (cl-letf (((symbol-function 'mevedel-session-artifacts-save)
                       #'ignore))
              (mevedel-plan-handoff-settle-request request-fsm 'success))
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
                        :execution direct :mode edits
                        :model-provider "Test:test-model")
            :accepted (:path "local/plans/accepted-20260813-120000.md"
                       :hash "hash")))
         (session
          (mevedel-session--create
           :authority-mode 'pid-lock
           :name "test"
           :plan-metadata
           (list :status 'accepted :implementation-retry record)))
         (data-buffer (generate-new-buffer " *plan-fresh-failure*")))
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel-session-artifacts-save) #'ignore)
                  ((symbol-function
                    'mevedel-session-artifacts-start-fresh-segment)
                   (lambda (&rest _) (error "Rotation failed"))))
          (with-current-buffer data-buffer
            (setq-local mevedel--session session))
          (mevedel-plan-handoff--dispatch-accepted session data-buffer)
          (let ((retry
                 (plist-get (mevedel-session-plan-metadata session)
                            :implementation-retry)))
            (should (eq 'prepare-context (plist-get retry :step)))
            (should (equal "Rotation failed" (plist-get retry :failure)))))
      (when (buffer-live-p data-buffer) (kill-buffer data-buffer))))

  :doc "reuses a matching Goal left durable before source retry cleanup"
  (let* ((save-dir (make-temp-file "mevedel-plan-goal-crash-" t))
         (accepted-path (file-name-concat save-dir "local" "plans" "accepted-20260813-120000.md"))
         (body "Crash-window plan")
         (goal (mevedel-goal--create
                :id "reserved" :status 'paused :reason "session resumed"
                :implementation-policy
                '(:model-provider "Test:test-model" :reasoning-effort nil :permission-mode edits)
                :plan-reference "local/plans/accepted-20260813-120000.md"))
         (record
          (list :step 'submit :goal-id "reserved"
                :selection '(:location here :context current
                              :execution goal :mode edits
                              :model-provider "Test:test-model")
                :accepted
                (list :path "local/plans/accepted-20260813-120000.md"
                      :hash (mevedel-plan-hash body))))
         (session
          (mevedel-session--create
           :authority-mode 'pid-lock
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
          (cl-letf (((symbol-function 'mevedel-session-artifacts-save) #'ignore)
                    ((symbol-function 'mevedel-view--interaction-target-buffer)
                     (lambda (_) view-buffer))
                    ((symbol-function 'mevedel-view--submit-planned-input)
                     (lambda (input _before _blocked callback &optional _after)
                       (funcall callback
                                (mevedel-prompt-submission-create
                                 :input input :state 'committed
                                 :outcome
                                 (list :model-input input
                                       :transcript-input input)))))
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

  :doc "leaves a different unfinished Goal untouched and retryable"
  (let* ((save-dir (make-temp-file "mevedel-plan-goal-conflict-" t))
         (accepted-path (file-name-concat save-dir "local" "plans" "accepted-20260813-120000.md"))
         (body "Conflicting Goal plan")
         (goal (mevedel-goal--create
                :id "other" :status 'paused :reason "owned"
                :plan-reference "local/plans/other.md"))
         (record
          (list :step 'submit :goal-id "reserved"
                :selection '(:location here :context current
                              :execution goal :mode edits
                              :model-provider "Test:test-model")
                :accepted
                (list :path "local/plans/accepted-20260813-120000.md"
                      :hash (mevedel-plan-hash body))))
         (session
          (mevedel-session--create
           :authority-mode 'pid-lock
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
          (cl-letf (((symbol-function 'mevedel-session-artifacts-save) #'ignore)
                    ((symbol-function 'mevedel-view--interaction-target-buffer)
                     (lambda (_) view-buffer))
                    ((symbol-function 'mevedel-view--submit-planned-input)
                     (lambda (input _before _blocked callback &optional _after)
                       (funcall callback
                                (mevedel-prompt-submission-create
                                 :input input :state 'committed
                                 :outcome
                                 (list :model-input input
                                       :transcript-input input)))))
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
         (accepted-path (file-name-concat save-dir "local" "plans" "accepted-20260813-120000.md"))
         (body "Kickoff failure plan")
         (record
          (list :step 'submit :goal-id "reserved"
                :selection '(:location here :context current
                              :execution goal :mode edits
                              :model-provider "Test:test-model")
                :accepted
                (list :path "local/plans/accepted-20260813-120000.md"
                      :hash (mevedel-plan-hash body))))
         (session
          (mevedel-session--create
           :authority-mode 'pid-lock
           :name "main" :save-path save-dir
           :pending-follow-ups
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
          (cl-letf (((symbol-function 'mevedel-session-artifacts-save) #'ignore)
                    ((symbol-function 'mevedel-view--interaction-target-buffer)
                     (lambda (_) view-buffer))
                    ((symbol-function 'mevedel-view--submit-planned-input)
                     (lambda (input _before _blocked callback &optional _after)
                       (funcall callback
                                (mevedel-prompt-submission-create
                                 :input input :state 'committed
                                 :outcome
                                 (list :model-input input
                                       :transcript-input input)))))
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
             (mevedel-view--follow-up-auto-drain-blocked-p session))
            (cl-letf (((symbol-function 'mevedel-session-artifacts-save)
                       #'ignore)
                      ((symbol-function 'run-at-time)
                       (lambda (_delay _repeat function &rest args)
                         (push (cons function args) scheduled))))
              (with-current-buffer data-buffer
                (mevedel-goal-resume))
              (should (eq 'active (mevedel-goal-status goal)))
              (should (eq 'follow-up
                          (mevedel-goal-continue-if-idle
                           session data-buffer))))
            (should
             (seq-some
              (lambda (call)
                (eq #'mevedel-view--run-follow-up-drain
                    (car call)))
              scheduled))
            (should (equal "steer first"
                           (plist-get
                            (car (mevedel-session-pending-follow-ups session))
                            :input)))))
      (kill-buffer view-buffer)
      (kill-buffer data-buffer)
      (delete-directory save-dir t)))

  :doc "Here/Summary caches one zero-tail compaction across dispatch retry"
  (let* ((save-dir (make-temp-file "mevedel-plan-summary-" t))
         (accepted-path (file-name-concat save-dir "local" "plans" "accepted-20260813-120000.md"))
         (body "# Accepted plan\n\nImplement the endpoint.")
         (selection '(:location here :context summary
                      :execution direct :mode edits
                      :model-provider "Test:test-model"))
         (record
          (list :step 'prepare-summary :selection selection
                :accepted
                (list :path "local/plans/accepted-20260813-120000.md"
                      :hash (mevedel-plan-hash body))))
         (session
          (mevedel-session--create
           :authority-mode 'pid-lock
           :name "test" :save-path save-dir
           :plan-metadata
           (list :status 'accepted :implementation-retry record)))
         (data-buffer (generate-new-buffer " *plan-summary-data*"))
         (view-buffer (generate-new-buffer " *plan-summary-view*"))
         (summary-runs 0)
         (compact-runs 0)
         (apply-attempts 0)
         (attempts 0)
         request-fsm
         anchored-summary
         persisted
         prompts)
    (unwind-protect
        (progn
          (make-directory (file-name-directory accepted-path) t)
          (write-region body nil accepted-path nil 'silent)
          (with-current-buffer data-buffer
            (setq-local mevedel--session session))
          (cl-letf (((symbol-function 'mevedel-session-artifacts-save)
                     (lambda (&rest _)
                       (push (copy-tree
                              (mevedel-session-plan-metadata session))
                             persisted)))
                    ((symbol-function 'mevedel-compact-target-main-target)
                     (lambda ()
                       (list :apply
                             (lambda (_target summary &rest _)
                               (cl-incf apply-attempts)
                               (when (= apply-attempts 1)
                                 (error "Rotation failed"))
                               (setq anchored-summary summary)
                               t))))
                    ((symbol-function 'mevedel-compact-evidence-previous-summary)
                     (lambda () anchored-summary))
                    ((symbol-function 'mevedel-compact-run-start)
                     (lambda (&rest args)
                       (cl-incf compact-runs)
                       (should (plist-get args :aggressive))
                       (should (string-match-p
                                (regexp-quote body)
                                (plist-get args :focus)))
                       (let* ((prepared (plist-get args :prepared-summary))
                              (summary
                               (or prepared
                                   (progn
                                     (cl-incf summary-runs)
                                     "# Handoff\n\nDiscovery.")))
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
                    ((symbol-function 'mevedel-view--submit-planned-input)
                     (lambda (input _before _blocked callback &optional _after)
                       (push input prompts)
                       (funcall callback
                                (mevedel-prompt-submission-create
                                 :input input :state 'committed
                                 :outcome
                                 (list :model-input input
                                       :transcript-input input)))))
                    ((symbol-function 'mevedel--implement-plan)
                     (lambda (_action)
                       (cl-incf attempts)
                       (when (= attempts 1)
                         (error "Request startup failed"))
                       (setq request-fsm (gptel-make-fsm :info nil)))))
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
            (should
             (plist-member (mevedel-session-plan-metadata session)
                           :implementation-retry))
            (mevedel-plan-handoff-settle-request request-fsm 'success)
            (should-not
             (plist-member (mevedel-session-plan-metadata session)
                           :implementation-retry))
            (dolist (prompt prompts)
              (let ((path-position
                     (string-search
                      "work://plans/accepted-20260813-120000.md"
                      prompt))
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
      (delete-directory save-dir t)))))

(defun test-mevedel-plan-handoff-dispatch--commit-source (session)
  "Make SESSION's working directory a Git repository with one commit."
  (let ((root (file-name-as-directory
               (mevedel-session-working-directory session))))
    (test-mevedel-session-persistence--git root "init")
    (test-mevedel-session-persistence--git
     root "config" "user.email" "mevedel@example.invalid")
    (test-mevedel-session-persistence--git root "config" "user.name" "Mevedel Test")
    (write-region "HEAD state\n" nil (file-name-concat root "current.txt") nil 'silent)
    (test-mevedel-session-persistence--git root "add" "current.txt")
    (test-mevedel-session-persistence--git root "commit" "-m" "base")
    root))

(mevedel-deftest mevedel-plan-handoff--prepare-worktree (:quiet t)
  ,test
  (test)
  :doc "forks at the plan reply, gives the fork the plan, and closes the planning session"
  (let ((fixture (test-mevedel-session-persistence--make-fork-ready))
        fork-buffer begun)
    (unwind-protect
        (let* ((session (plist-get fixture :session))
               (source (plist-get fixture :buffer))
               (source-view (buffer-local-value 'mevedel--view-buffer source))
               (root (test-mevedel-plan-handoff-dispatch--commit-source session))
               (body "# Parent plan\n")
               (record
                (list :step 'prepare-worktree :fork-point-id "fixture-fork"
                      :goal-id "reserved"
                      :selection
                      (list :location 'worktree :context 'current :execution 'goal
                            :mode 'edits :model-provider "test-backend:test-model"
                            :branch "plan/topic"
                            :skills
                            (list (list :name "local"
                                        :source-file
                                        (file-name-concat
                                         root ".mevedel/skills/local/SKILL.md"))))
                      :accepted (list :path "local/plans/current.md"
                                      :hash (mevedel-plan-hash body)))))
          (mevedel-plan--metadata-put session :implementation-retry record)
          (save-window-excursion
            (set-window-buffer (selected-window) source-view)
            (cl-letf (((symbol-function 'mevedel--run-session-start-hooks) #'ignore)
                      ((symbol-function 'mevedel-model-apply-session-policy) #'ignore)
                      ((symbol-function 'mevedel-plan-handoff--begin)
                       (lambda (fork buffer) (setq begun (list fork buffer)))))
              (should-not (mevedel-plan-handoff--prepare-worktree
                           session source record)))
            (setq fork-buffer (cadr begun))
            (let* ((fork (car begun))
                   (fork-view (buffer-local-value 'mevedel--view-buffer fork-buffer))
                   (metadata (mevedel-session-plan-metadata fork))
                   (retry (plist-get metadata :implementation-retry))
                   (selection (plist-get retry :selection))
                   (directory (file-name-as-directory
                               (file-name-concat root ".worktrees" "topic"))))
              ;; The fork is the worktree session, linked to its planning parent.
              (should (eq fork (buffer-local-value 'mevedel--session fork-buffer)))
              (should (equal (mevedel-session-session-id session)
                             (mevedel-session-forked-from-session-id fork)))
              (should (equal "fixture-fork"
                             (mevedel-session-forked-from-fork-point-id fork)))
              (should (equal "plan/topic" (mevedel-session-worktree-branch fork)))
              (should (file-equal-p directory (mevedel-session-working-directory fork)))
              ;; It owns the plan and runs it as an ordinary Here handoff.
              (should (eq 'accepted (plist-get metadata :status)))
              (should (equal body (mevedel-plan-read-artifact
                                   fork (plist-get retry :accepted))))
              (should (eq 'submit (plist-get retry :step)))
              (should (eq 'here (plist-get selection :location)))
              (should-not (plist-member selection :branch))
              (should (eq 'current (plist-get selection :context)))
              (should (equal "reserved" (plist-get retry :goal-id)))
              (should (equal (file-name-concat directory ".mevedel/skills/local/SKILL.md")
                             (plist-get (car (plist-get selection :skills)) :source-file)))
              ;; The source keeps no retry, on disk either.
              (should-not (plist-member (mevedel-session-plan-metadata session)
                                        :implementation-retry))
              (should-not (plist-member
                           (plist-get (mevedel-session-codec-read
                                       (mevedel-session-artifacts-sidecar-path
                                        (plist-get fixture :parent-path)))
                                      :plan-metadata)
                           :implementation-retry))
              ;; The implementation takes the planning window, which then closes.
              (should (eq fork-view (window-buffer (selected-window))))
              (mevedel-test--await 5 "The planning session was not closed"
                (not (or (buffer-live-p source) (buffer-live-p source-view))))
              ;; An interrupted handoff resumes without a second fork or plan.
              (setq begun nil)
              (let ((retried (plist-put (copy-tree record) :target-save-path
                                        (mevedel-session-save-path fork))))
                (plist-put retried :worktree-reservation
                           (list :directory directory))
                (cl-letf (((symbol-function 'mevedel-session-fork-create)
                           (lambda (&rest _) (ert-fail "Forked twice")))
                          ((symbol-function 'mevedel-plan-archive-accepted)
                           (lambda (&rest _) (ert-fail "Installed the plan twice")))
                          ((symbol-function 'mevedel-plan-handoff--hand-over) #'ignore)
                          ((symbol-function 'mevedel-session-artifacts-save) #'ignore)
                          ((symbol-function 'mevedel-plan-handoff--begin)
                           (lambda (fork buffer) (setq begun (list fork buffer)))))
                  (mevedel-plan-handoff--prepare-worktree session source retried)))
              (should (eq fork-buffer (cadr begun))))))
      (when (buffer-live-p fork-buffer)
        (let ((view (buffer-local-value 'mevedel--view-buffer fork-buffer)))
          (with-current-buffer fork-buffer (set-buffer-modified-p nil))
          (when (buffer-live-p view) (kill-buffer view)))
        (when (buffer-live-p fork-buffer) (kill-buffer fork-buffer)))
      (test-mevedel-session-persistence--cleanup-fork-fixture fixture))))

(mevedel-deftest mevedel-plan-handoff-assert-worktree-ready
  (:doc "a session that cannot fork refuses before acceptance")
  ,test
  (test)
  (let* ((session (mevedel-session--create :authority-mode 'pid-lock :name "busy"))
         (buffer (generate-new-buffer " *plan-worktree-busy*")))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq-local mevedel--session session
                        mevedel--current-request 'running))
          (should-error (mevedel-plan-handoff-assert-worktree-ready session buffer)
                        :type 'user-error))
      (kill-buffer buffer))))

(provide 'test-mevedel-plan-handoff-dispatch)
;;; test-mevedel-plan-handoff-dispatch.el ends here
