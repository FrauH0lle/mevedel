;;; test-niwa-plan-handoff-dispatch.el -- Dispatch tests for niwa-plan-handoff.el -*- lexical-binding: t -*-

;;; Commentary:

;; Durable accepted-Plan dispatch and recovery tests.

;;; Code:

(require 'gptel-request)
(require 'niwa-compact-evidence)
(require 'niwa-compact-run)
(require 'niwa-compact-target)
(require 'niwa-context-summary)
(require 'niwa-plan)
(require 'niwa-plan-handoff)
(require 'niwa-plan-mode)
(require 'niwa-goal)
(require 'niwa-interaction-prompt)
(require 'niwa-pending-inputs)
(require 'niwa-permissions)
(require 'niwa-chat)
(require 'niwa-prompt-submission)
(require 'niwa-session-artifacts)
(require 'niwa-session-persistence)
(require 'niwa-structs)
(require 'niwa-view)
(require 'niwa-view-composer)
(require 'niwa-view-interaction)
(require 'niwa-worktree)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'niwa-session-test-support
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "niwa-session-test-support"))
(require 'niwa-compact)

(defun niwa-plan-handoff-dispatch-test--publish (session path body)
  "Commit fixture BODY at PATH with SESSION's genuine sidecar."
  (niwa-session-publication-publish
   session
   (list (list :path path :content body)
         (list :path (file-name-concat (niwa-session-save-path session) "session.meta.el")
               :content (niwa-session-artifacts-printed-value
                         (niwa-session-codec-serialize session))
               :commit-marker t))))

(niwa-deftest niwa-plan-handoff--dispatch-accepted
  (:quiet t :doc "Here/Fresh rotates once, runs setup hooks, and retries only submission")
  ,test
  (test)
  (let* ((save-dir (make-temp-file "niwa-plan-fresh-" t))
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
                      :hash (niwa-plan-hash body))))
         (session
          (test-niwa-session-persistence--leased-session save-dir
           :name "test"  :permission-mode 'ask
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
          (niwa-plan-handoff-dispatch-test--publish session accepted-path body)
          (write-region "# Mutable replacement" nil current-path nil 'silent)
          (with-current-buffer data-buffer
            (setq-local niwa--session session))
          (cl-letf (((symbol-function 'niwa-session-artifacts-save) #'ignore)
                    ((symbol-function
                      'niwa-session-artifacts-start-fresh-segment)
                     (lambda (&rest _)
                       (cl-incf rotations)
                       "segment-0002.chat.org"))
                    ((symbol-function 'niwa--run-session-start-hooks)
                     (lambda (source) (setq hook-source source)))
                    ((symbol-function 'niwa-view--interaction-target-buffer)
                     (lambda (_buffer) view-buffer))
                    ((symbol-function 'niwa-view--submit-planned-input)
                     (lambda (input _before _blocked callback &optional _after)
                       (push input prompts)
                       (funcall callback
                                (niwa-prompt-submission-create
                                 :input input :state 'committed
                                 :outcome
                                 (list :model-input input
                                       :transcript-input input)))))
                    ((symbol-function 'niwa--implement-plan)
                     (lambda (action)
                       (cl-incf attempts)
                       (should (eq 'full-auto
                                   (plist-get (plist-get action :implementation-policy)
                                              :permission-mode)))
                       (should (eq 'ask (niwa-session-permission-mode session)))
                       (when (= attempts 1)
                         (error "Transport refused"))
                       (setq request-fsm (gptel-make-fsm :info nil)))))
            (niwa-plan-handoff--dispatch-accepted session data-buffer)
            (let ((retry
                   (plist-get (niwa-session-plan-metadata session)
                              :implementation-retry)))
              (should (= 1 rotations))
              (should (equal "clear" hook-source))
              (should (eq 'submit (plist-get retry :step)))
              (should (equal "Transport refused" (plist-get retry :failure)))
              (should (eq 'accepted
                          (plist-get (niwa-session-plan-metadata session)
                                     :status))))
            (niwa-retry-plan-implementation session data-buffer)
            (should (= 1 rotations))
            (should (= 2 attempts))
            (should
             (plist-member (niwa-session-plan-metadata session)
                           :implementation-retry))
            (cl-letf (((symbol-function 'niwa-session-artifacts-save)
                       #'ignore))
              (niwa-plan-handoff-settle-request request-fsm 'success))
            (should-not
             (plist-member (niwa-session-plan-metadata session)
                           :implementation-retry))
            (dolist (prompt prompts)
              (should (string-match-p "# Immutable accepted plan" prompt))
              (should-not (string-match-p "Mutable replacement" prompt)))))
      (when (buffer-live-p view-buffer) (kill-buffer view-buffer))
      (when (buffer-live-p data-buffer) (kill-buffer data-buffer))
      (niwa-session-persistence-lock-release save-dir session)
      (niwa-workspace-clear-registry)
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
          (niwa-session--create
           :name "test"
           :plan-metadata
           (list :status 'accepted :implementation-retry record)))
         (data-buffer (generate-new-buffer " *plan-fresh-failure*")))
    (unwind-protect
        (cl-letf (((symbol-function 'niwa-session-artifacts-save) #'ignore)
                  ((symbol-function
                    'niwa-session-artifacts-start-fresh-segment)
                   (lambda (&rest _) (error "Rotation failed"))))
          (with-current-buffer data-buffer
            (setq-local niwa--session session))
          (niwa-plan-handoff--dispatch-accepted session data-buffer)
          (let ((retry
                 (plist-get (niwa-session-plan-metadata session)
                            :implementation-retry)))
            (should (eq 'prepare-context (plist-get retry :step)))
            (should (equal "Rotation failed" (plist-get retry :failure)))))
      (when (buffer-live-p data-buffer) (kill-buffer data-buffer))))

  :doc "reuses a matching Goal left durable before source retry cleanup"
  (let* ((save-dir (make-temp-file "niwa-plan-goal-crash-" t))
         (accepted-path (file-name-concat save-dir "local" "plans" "accepted-20260813-120000.md"))
         (body "Crash-window plan")
         (goal (niwa-goal--create
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
                      :hash (niwa-plan-hash body))))
         (session
          (test-niwa-session-persistence--leased-session save-dir
           :name "main"  :goal goal
           :plan-metadata
           (list :status 'accepted :implementation-retry record)))
         (data-buffer (generate-new-buffer " *plan-goal-crash-data*"))
         (view-buffer (generate-new-buffer " *plan-goal-crash-view*"))
         implemented)
    (unwind-protect
        (progn
          (make-directory (file-name-directory accepted-path) t)
          (niwa-plan-handoff-dispatch-test--publish session accepted-path body)
          (with-current-buffer data-buffer
            (setq-local niwa--session session))
          (cl-letf (((symbol-function 'niwa-session-artifacts-save) #'ignore)
                    ((symbol-function 'niwa-view--interaction-target-buffer)
                     (lambda (_) view-buffer))
                    ((symbol-function 'niwa-view--submit-planned-input)
                     (lambda (input _before _blocked callback &optional _after)
                       (funcall callback
                                (niwa-prompt-submission-create
                                 :input input :state 'committed
                                 :outcome
                                 (list :model-input input
                                       :transcript-input input)))))
                    ((symbol-function 'niwa-goal-create)
                     (lambda (&rest _) (ert-fail "Constructed duplicate Goal")))
                    ((symbol-function 'niwa--implement-plan)
                     (lambda (_) (setq implemented t))))
            (niwa-plan-handoff--dispatch-accepted session data-buffer))
          (should implemented)
          (should (eq goal (niwa-session-goal session)))
          (should (eq 'active (niwa-goal-status goal)))
          (should-not (niwa-goal-reason goal))
          (should-not
           (plist-member (niwa-session-plan-metadata session)
                         :implementation-retry)))
      (kill-buffer view-buffer)
      (kill-buffer data-buffer)
      (niwa-session-persistence-lock-release save-dir session)
      (niwa-workspace-clear-registry)
      (delete-directory save-dir t)))

  :doc "leaves a different unfinished Goal untouched and retryable"
  (let* ((save-dir (make-temp-file "niwa-plan-goal-conflict-" t))
         (accepted-path (file-name-concat save-dir "local" "plans" "accepted-20260813-120000.md"))
         (body "Conflicting Goal plan")
         (goal (niwa-goal--create
                :id "other" :status 'paused :reason "owned"
                :plan-reference "local/plans/other.md"))
         (record
          (list :step 'submit :goal-id "reserved"
                :selection '(:location here :context current
                              :execution goal :mode edits
                              :model-provider "Test:test-model")
                :accepted
                (list :path "local/plans/accepted-20260813-120000.md"
                      :hash (niwa-plan-hash body))))
         (session
          (test-niwa-session-persistence--leased-session save-dir
           :name "main"  :goal goal
           :plan-metadata
           (list :status 'accepted :implementation-retry record)))
         (data-buffer (generate-new-buffer " *plan-goal-conflict-data*"))
         (view-buffer (generate-new-buffer " *plan-goal-conflict-view*")))
    (unwind-protect
        (progn
          (make-directory (file-name-directory accepted-path) t)
          (niwa-plan-handoff-dispatch-test--publish session accepted-path body)
          (with-current-buffer data-buffer
            (setq-local niwa--session session))
          (cl-letf (((symbol-function 'niwa-session-artifacts-save) #'ignore)
                    ((symbol-function 'niwa-view--interaction-target-buffer)
                     (lambda (_) view-buffer))
                    ((symbol-function 'niwa-view--submit-planned-input)
                     (lambda (input _before _blocked callback &optional _after)
                       (funcall callback
                                (niwa-prompt-submission-create
                                 :input input :state 'committed
                                 :outcome
                                 (list :model-input input
                                       :transcript-input input)))))
                    ((symbol-function 'niwa--implement-plan)
                     (lambda (_) (ert-fail "Started conflicting Goal"))))
            (niwa-plan-handoff--dispatch-accepted session data-buffer))
          (should (eq goal (niwa-session-goal session)))
          (should (eq 'paused (niwa-goal-status goal)))
          (should (equal "owned" (niwa-goal-reason goal)))
          (should
           (string-match-p
            "unfinished Goal other; expected reserved Goal reserved"
            (plist-get
             (plist-get (niwa-session-plan-metadata session)
                        :implementation-retry)
             :failure))))
      (kill-buffer view-buffer)
      (kill-buffer data-buffer)
      (niwa-session-persistence-lock-release save-dir session)
      (niwa-workspace-clear-registry)
      (delete-directory save-dir t)))

  :doc "pauses after kickoff startup failure and resumes held input first"
  (let* ((save-dir (make-temp-file "niwa-plan-goal-kickoff-" t))
         (accepted-path (file-name-concat save-dir "local" "plans" "accepted-20260813-120000.md"))
         (body "Kickoff failure plan")
         (record
          (list :step 'submit :goal-id "reserved"
                :selection '(:location here :context current
                              :execution goal :mode edits
                              :model-provider "Test:test-model")
                :accepted
                (list :path "local/plans/accepted-20260813-120000.md"
                      :hash (niwa-plan-hash body))))
         (session
          (test-niwa-session-persistence--leased-session save-dir
           :name "main"
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
          (niwa-plan-handoff-dispatch-test--publish session accepted-path body)
          (with-current-buffer data-buffer
            (setq-local niwa--session session))
          (cl-letf (((symbol-function 'niwa-session-artifacts-save) #'ignore)
                    ((symbol-function 'niwa-view--interaction-target-buffer)
                     (lambda (_) view-buffer))
                    ((symbol-function 'niwa-view--submit-planned-input)
                     (lambda (input _before _blocked callback &optional _after)
                       (funcall callback
                                (niwa-prompt-submission-create
                                 :input input :state 'committed
                                 :outcome
                                 (list :model-input input
                                       :transcript-input input)))))
                    ((symbol-function 'niwa--implement-plan)
                     (lambda (_) (error "Transport offline"))))
            (niwa-plan-handoff--dispatch-accepted session data-buffer))
          (let ((goal (niwa-session-goal session)))
            (should (eq 'paused (niwa-goal-status goal)))
            (should (equal "Transport offline" (niwa-goal-reason goal)))
            (should-not
             (plist-member (niwa-session-plan-metadata session)
                           :implementation-retry))
            (should
             (niwa-view--follow-up-auto-drain-blocked-p session))
            (cl-letf (((symbol-function 'niwa-session-artifacts-save)
                       #'ignore)
                      ((symbol-function 'run-at-time)
                       (lambda (_delay _repeat function &rest args)
                         (push (cons function args) scheduled))))
              (with-current-buffer data-buffer
                (niwa-goal-resume))
              (should (eq 'active (niwa-goal-status goal)))
              (should (eq 'follow-up
                          (niwa-goal-continue-if-idle
                           session data-buffer))))
            (should
             (seq-some
              (lambda (call)
                (eq #'niwa-view--run-follow-up-drain
                    (car call)))
              scheduled))
            (should (equal "steer first"
                           (plist-get
                            (car (niwa-session-pending-follow-ups session))
                            :input)))))
      (kill-buffer view-buffer)
      (kill-buffer data-buffer)
      (niwa-session-persistence-lock-release save-dir session)
      (niwa-workspace-clear-registry)
      (delete-directory save-dir t)))

  :doc "Here/Summary caches one zero-tail compaction across dispatch retry"
  (let* ((save-dir (make-temp-file "niwa-plan-summary-" t))
         (accepted-path (file-name-concat save-dir "local" "plans" "accepted-20260813-120000.md"))
         (body "# Accepted plan\n\nImplement the endpoint.")
         (selection '(:location here :context summary
                      :execution direct :mode edits
                      :model-provider "Test:test-model"))
         (record
          (list :step 'prepare-summary :selection selection
                :accepted
                (list :path "local/plans/accepted-20260813-120000.md"
                      :hash (niwa-plan-hash body))))
         (session
          (test-niwa-session-persistence--leased-session save-dir
           :name "test"
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
          (niwa-plan-handoff-dispatch-test--publish session accepted-path body)
          (with-current-buffer data-buffer
            (setq-local niwa--session session))
          (cl-letf (((symbol-function 'niwa-session-artifacts-save)
                     (lambda (&rest _)
                       (push (copy-tree
                              (niwa-session-plan-metadata session))
                             persisted)))
                    ((symbol-function 'niwa-compact-target-main-target)
                     (lambda ()
                       (list :apply
                             (lambda (_target summary &rest _)
                               (cl-incf apply-attempts)
                               (when (= apply-attempts 1)
                                 (error "Rotation failed"))
                               (setq anchored-summary summary)
                               t))))
                    ((symbol-function 'niwa-compact-evidence-previous-summary)
                     (lambda () anchored-summary))
                    ((symbol-function 'niwa-compact-run-start)
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
                    ((symbol-function 'niwa-view--interaction-target-buffer)
                     (lambda (_buffer) view-buffer))
                    ((symbol-function 'niwa-view--submit-planned-input)
                     (lambda (input _before _blocked callback &optional _after)
                       (push input prompts)
                       (funcall callback
                                (niwa-prompt-submission-create
                                 :input input :state 'committed
                                 :outcome
                                 (list :model-input input
                                       :transcript-input input)))))
                    ((symbol-function 'niwa--implement-plan)
                     (lambda (_action)
                       (cl-incf attempts)
                       (when (= attempts 1)
                         (error "Request startup failed"))
                       (setq request-fsm (gptel-make-fsm :info nil)))))
            (niwa-plan-handoff--dispatch-accepted session data-buffer)
            (let ((retry
                   (plist-get (niwa-session-plan-metadata session)
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
            (niwa-retry-plan-implementation session data-buffer)
            (should (= 2 compact-runs))
            (should (= 1 summary-runs))
            (should (= 1 attempts))
            (should (equal anchored-summary
                           (plist-get
                            (plist-get (niwa-session-plan-metadata session)
                                       :implementation-retry)
                            :summary)))
            (niwa-retry-plan-implementation session data-buffer)
            (should (= 2 attempts))
            (should
             (plist-member (niwa-session-plan-metadata session)
                           :implementation-retry))
            (niwa-plan-handoff-settle-request request-fsm 'success)
            (should-not
             (plist-member (niwa-session-plan-metadata session)
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
      (niwa-session-persistence-lock-release save-dir session)
      (niwa-workspace-clear-registry)
      (delete-directory save-dir t)))))

(defun test-niwa-plan-handoff-dispatch--commit-source (session)
  "Make SESSION's working directory a Git repository with one commit."
  (let ((root (file-name-as-directory
               (niwa-session-working-directory session))))
    (test-niwa-session-persistence--git root "init")
    (test-niwa-session-persistence--git
     root "config" "user.email" "niwa@example.invalid")
    (test-niwa-session-persistence--git root "config" "user.name" "Niwa Test")
    (write-region "HEAD state\n" nil (file-name-concat root "current.txt") nil 'silent)
    (test-niwa-session-persistence--git root "add" "current.txt")
    (test-niwa-session-persistence--git root "commit" "-m" "base")
    root))

(niwa-deftest niwa-plan-handoff--prepare-worktree (:quiet t)
  ,test
  (test)
  :doc "forks at the plan reply, gives the fork the plan, and closes the planning session"
  (let ((fixture (test-niwa-session-persistence--make-fork-ready))
        fork-buffer begun)
    (unwind-protect
        (let* ((session (plist-get fixture :session))
               (source (plist-get fixture :buffer))
               (source-view (buffer-local-value 'niwa--view-buffer source))
               (root (test-niwa-plan-handoff-dispatch--commit-source session))
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
                                         root ".niwa/skills/local/SKILL.md"))))
                      :accepted (list :path "local/plans/current.md"
                                      :hash (niwa-plan-hash body)))))
          (niwa-plan--metadata-put session :implementation-retry record)
          (save-window-excursion
            (set-window-buffer (selected-window) source-view)
            (cl-letf (((symbol-function 'niwa--run-session-start-hooks) #'ignore)
                      ((symbol-function 'niwa-model-apply-session-policy) #'ignore)
                      ((symbol-function 'niwa-plan-handoff--begin)
                       (lambda (fork buffer) (setq begun (list fork buffer)))))
              (should-not (niwa-plan-handoff--prepare-worktree
                           session source record)))
            (setq fork-buffer (cadr begun))
            (let* ((fork (car begun))
                   (fork-view (buffer-local-value 'niwa--view-buffer fork-buffer))
                   (metadata (niwa-session-plan-metadata fork))
                   (retry (plist-get metadata :implementation-retry))
                   (selection (plist-get retry :selection))
                   (directory (file-name-as-directory
                               (file-name-concat root ".worktrees" "topic"))))
              ;; The fork is the worktree session, linked to its planning parent.
              (should (eq fork (buffer-local-value 'niwa--session fork-buffer)))
              (should (equal (niwa-session-session-id session)
                             (niwa-session-forked-from-session-id fork)))
              (should (equal "fixture-fork"
                             (niwa-session-forked-from-fork-point-id fork)))
              (should (equal "plan/topic" (niwa-session-worktree-branch fork)))
              (should (file-equal-p directory (niwa-session-working-directory fork)))
              ;; It owns the plan and runs it as an ordinary Here handoff.
              (should (eq 'accepted (plist-get metadata :status)))
              (should (equal body (niwa-plan-read-artifact
                                   fork (plist-get retry :accepted))))
              (should (eq 'submit (plist-get retry :step)))
              (should (eq 'here (plist-get selection :location)))
              (should-not (plist-member selection :branch))
              (should (eq 'current (plist-get selection :context)))
              (should (equal "reserved" (plist-get retry :goal-id)))
              (should (equal (file-name-concat directory ".niwa/skills/local/SKILL.md")
                             (plist-get (car (plist-get selection :skills)) :source-file)))
              ;; The source keeps no retry, on disk either.
              (should-not (plist-member (niwa-session-plan-metadata session)
                                        :implementation-retry))
              (should-not (plist-member
                           (plist-get (niwa-session-codec-read
                                       (niwa-session-artifacts-sidecar-path
                                        (plist-get fixture :parent-path)))
                                      :plan-metadata)
                           :implementation-retry))
              ;; The implementation takes the planning window, which then closes.
              (should (eq fork-view (window-buffer (selected-window))))
              (niwa-test--await 5 "The planning session was not closed"
                (not (or (buffer-live-p source) (buffer-live-p source-view))))
              ;; An interrupted handoff resumes without a second fork or plan.
              (setq begun nil)
              (let ((retried (plist-put (copy-tree record) :target-save-path
                                        (niwa-session-save-path fork))))
                (plist-put retried :worktree-reservation
                           (list :directory directory))
                (cl-letf (((symbol-function 'niwa-session-fork-create)
                           (lambda (&rest _) (ert-fail "Forked twice")))
                          ((symbol-function 'niwa-plan-archive-accepted)
                           (lambda (&rest _) (ert-fail "Installed the plan twice")))
                          ((symbol-function 'niwa-plan-handoff--hand-over) #'ignore)
                          ((symbol-function 'niwa-session-artifacts-save) #'ignore)
                          ((symbol-function 'niwa-plan-handoff--begin)
                           (lambda (fork buffer) (setq begun (list fork buffer)))))
                  (niwa-plan-handoff--prepare-worktree session source retried)))
              (should (eq fork-buffer (cadr begun))))))
      (when (buffer-live-p fork-buffer)
        (let ((view (buffer-local-value 'niwa--view-buffer fork-buffer)))
          (with-current-buffer fork-buffer (set-buffer-modified-p nil))
          (when (buffer-live-p view) (kill-buffer view)))
        (when (buffer-live-p fork-buffer) (kill-buffer fork-buffer)))
      (test-niwa-session-persistence--cleanup-fork-fixture fixture))))

(niwa-deftest niwa-plan-handoff-assert-worktree-ready
  (:doc "a session that cannot fork refuses before acceptance")
  ,test
  (test)
  (let* ((session (niwa-session--create :name "busy"))
         (buffer (generate-new-buffer " *plan-worktree-busy*")))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq-local niwa--session session
                        niwa--current-request 'running))
          (should-error (niwa-plan-handoff-assert-worktree-ready session buffer)
                        :type 'user-error))
      (kill-buffer buffer))))

(provide 'test-niwa-plan-handoff-dispatch)
;;; test-niwa-plan-handoff-dispatch.el ends here
