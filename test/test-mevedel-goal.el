;;; test-mevedel-goal.el --- Tests for mevedel-goal.el -*- lexical-binding: t -*-

;;; Commentary:

;; Supervised one-cycle Goal lifecycle tests.

;;; Code:

(require 'gptel-request)
(require 'mevedel-models)
(require 'mevedel-goal)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(defun test-mevedel-goal--workspace (root)
  "Return a test workspace rooted at ROOT."
  (mevedel-workspace--create
   :type 'test :id "goal" :root root :name "goal"
   :file-cache (mevedel-file-cache--create
                :table (make-hash-table :test #'equal)
                :order nil :total-bytes 0)))

(defun test-mevedel-goal--fsm (buffer phase)
  "Return a minimal FSM for BUFFER and Goal PHASE."
  (gptel-make-fsm
   :info (list :buffer buffer :mevedel-goal-phase phase)))

(mevedel-deftest mevedel-goal-start ()
  ,test
  (test)
  :doc "creates a supervised session-owned Goal and dispatches planning"
  (let* ((root (make-temp-file "mevedel-goal-start-" t))
         (session (mevedel-session-create
                   "main" (test-mevedel-goal--workspace root)))
         dispatched)
    (unwind-protect
        (with-temp-buffer
          (setq-local mevedel--session session)
          (let ((mevedel-goal-dispatch-function
                 (lambda (phase prompt display)
                   (setq dispatched (list phase prompt display)))))
            (let ((goal (mevedel-goal-start "Fix the race")))
              (should (eq goal (mevedel-session-goal session)))
              (should (eq 'active (mevedel-goal-status goal)))
              (should (eq 'planning (mevedel-goal-phase goal)))
              (should (eq 'supervised (mevedel-goal-approval-policy goal)))
              (should (equal "Fix the race" (mevedel-goal-objective goal)))
              (should (eq 'planning (car dispatched)))
              (should (string-match-p "<proposed_plan>"
                                      (cadr dispatched))))))
      (delete-directory root t)))
  :doc "rejects a blank objective and a second non-complete Goal"
  (let* ((root (make-temp-file "mevedel-goal-replace-" t))
         (session (mevedel-session-create
                   "main" (test-mevedel-goal--workspace root))))
    (unwind-protect
        (with-temp-buffer
          (setq-local mevedel--session session)
          (let ((mevedel-goal-dispatch-function #'ignore))
            (should-error (mevedel-goal-start "  ") :type 'user-error)
            (mevedel-goal-start "First")
            (should-error (mevedel-goal-start "Second") :type 'user-error)))
      (delete-directory root t)))
  :doc "restores prior Goal and plan metadata when planning cannot start"
  (let* ((root (make-temp-file "mevedel-goal-rollback-" t))
         (session (mevedel-session-create
                   "main" (test-mevedel-goal--workspace root)))
         (previous (mevedel-goal--create
                    :status 'complete :phase 'reviewing :objective "Old"))
         (metadata '(:status approved)))
    (unwind-protect
        (with-temp-buffer
          (setq-local mevedel--session session)
          (setf (mevedel-session-goal session) previous
                (mevedel-session-plan-metadata session) metadata)
          (let ((mevedel-goal-dispatch-function
                 (lambda (&rest _)
                   (error "Request startup failed"))))
            (should-error (mevedel-goal-start "New")))
          (should (eq previous (mevedel-session-goal session)))
          (should (eq metadata (mevedel-session-plan-metadata session))))
      (delete-directory root t))))

(mevedel-deftest mevedel-goal--call-with-workload ()
  ,test
  (test)
  :doc "applies one workload policy for dispatch and restores session settings"
  (with-temp-buffer
    (setq-local gptel-backend 'session-backend
                gptel-model 'session-model
                gptel-reasoning-effort 'medium)
    (cl-letf (((symbol-function 'mevedel-model-resolve-workload)
               (lambda (workload &rest _)
                 (should (eq workload 'planning))
                 '(:backend planning-backend
                   :model planning-model
                   :effort high))))
      (mevedel-goal--call-with-workload
       'planning
       (lambda ()
         (should (eq gptel-backend 'planning-backend))
         (should (eq gptel-model 'planning-model))
         (should (eq gptel-reasoning-effort 'high)))))
    (should (eq gptel-backend 'session-backend))
    (should (eq gptel-model 'session-model))
    (should (eq gptel-reasoning-effort 'medium))))

(mevedel-deftest mevedel-goal--dispatch-gptel ()
  ,test
  (test)
  :doc "maps a phase to its workload and tags the resulting request"
  (with-temp-buffer
    (let ((fsm (test-mevedel-goal--fsm (current-buffer) nil))
          workload)
      (cl-letf (((symbol-function 'mevedel-goal--call-with-workload)
                 (lambda (selected fn)
                   (setq workload selected)
                   (funcall fn)))
                ((symbol-function 'mevedel-goal--insert-and-send)
                 (lambda (&rest _) fsm)))
        (should (eq fsm
                    (mevedel-goal--dispatch-gptel
                     'reviewing "Review" "Review Goal"))))
      (should (eq workload 'review))
      (should (eq (plist-get (gptel-fsm-info fsm) :mevedel-goal-phase)
                  'reviewing)))))

(mevedel-deftest mevedel-goal--dispatch-phase ()
  ,test
  (test)
  :doc "dispatches only request-bearing Goal phases"
  (let (received)
    (let ((mevedel-goal-dispatch-function
           (lambda (&rest args) (setq received args))))
      (mevedel-goal--dispatch-phase 'planning "Prompt" "Display")
      (should (equal received '(planning "Prompt" "Display")))
      (should-error
       (mevedel-goal--dispatch-phase 'awaiting-approval "x" "x")))))

(mevedel-deftest mevedel-goal-read-only-phase-p ()
  ,test
  (test)
  :doc "planning and reviewing are read-only independently of permission mode"
  (let* ((root (make-temp-file "mevedel-goal-policy-" t))
         (session (mevedel-session-create
                   "main" (test-mevedel-goal--workspace root)))
         (goal (mevedel-goal--create
                :status 'active :phase 'planning :objective "x")))
    (unwind-protect
        (progn
          (setf (mevedel-session-goal session) goal
                (mevedel-session-permission-mode session) 'trust-all)
          (should (mevedel-goal-read-only-phase-p session))
          (setf (mevedel-goal-phase goal) 'implementing)
          (should-not (mevedel-goal-read-only-phase-p session))
          (setf (mevedel-goal-phase goal) 'reviewing)
          (should (mevedel-goal-read-only-phase-p session)))
      (delete-directory root t))))

(mevedel-deftest mevedel-goal--post-response ()
  ,test
  (test)
  :doc "planning captures a proposed plan and waits for approval"
  (let* ((root (make-temp-file "mevedel-goal-plan-" t))
         (session (mevedel-session-create
                   "main" (test-mevedel-goal--workspace root)))
         (goal (mevedel-goal--create
                :status 'active :phase 'planning :objective "x"))
         presented)
    (unwind-protect
        (with-temp-buffer
          (setq-local mevedel--session session)
          (setf (mevedel-session-goal session) goal)
          (insert "<proposed_plan>\n# Fix\n\nDo it.\n</proposed_plan>\n")
          (cl-letf (((symbol-function 'mevedel-goal-present-plan)
                     (lambda (plan &rest _)
                       (setq presented plan))))
            (mevedel-goal--post-response (point-min) (point-max)))
          (should (eq 'awaiting-approval (mevedel-goal-phase goal)))
          (should (equal "# Fix\n\nDo it." presented)))
      (delete-directory root t)))
  :doc "reviewing captures evidence but cannot complete before settlement"
  (let* ((root (make-temp-file "mevedel-goal-review-" t))
         (session (mevedel-session-create
                   "main" (test-mevedel-goal--workspace root)))
         (goal (mevedel-goal--create
                :status 'active :phase 'reviewing :objective "x")))
    (unwind-protect
        (with-temp-buffer
          (setq-local mevedel--session session)
          (setf (mevedel-session-goal session) goal)
          (insert "Tests and diff satisfy the objective.")
          (mevedel-goal--post-response (point-min) (point-max))
          (should (eq 'active (mevedel-goal-status goal)))
          (should (equal "Tests and diff satisfy the objective."
                         (mevedel-goal-review-summary goal))))
      (delete-directory root t))))

(mevedel-deftest mevedel-goal--approval-callback ()
  ,test
  (test)
  :doc "approval archives the plan, enters implementation, and keeps session mode"
  (let* ((root (make-temp-file "mevedel-goal-approve-" t))
         (buffer (generate-new-buffer " *goal-approve*"))
         (session (mevedel-session-create
                   "main" (test-mevedel-goal--workspace root)))
         (goal (mevedel-goal--create
                :status 'active :phase 'awaiting-approval :objective "x"))
         implementation)
    (unwind-protect
        (with-current-buffer buffer
          (setq-local mevedel--session session)
          (setf (mevedel-session-goal session) goal
                (mevedel-session-permission-mode session) 'accept-edits)
          (cl-letf (((symbol-function 'mevedel-goal--save-session-state)
                     #'ignore)
                    ((symbol-function 'mevedel-goal--ensure-reference-reminder)
                     #'ignore)
                    ((symbol-function 'mevedel-goal--call-with-workload)
                     (lambda (workload fn)
                       (should (eq workload 'implementation))
                       (funcall fn)))
                    ((symbol-function 'mevedel--implement-plan)
                     (lambda (input)
                       (setq implementation input)
                       nil)))
            (mevedel-goal--approval-callback
             "# Plan\n\nImplement it." buffer 'implement))
          (should (eq 'implementing (mevedel-goal-phase goal)))
          (should (eq 'accept-edits
                      (plist-get implementation :permission-mode)))
          (should (file-exists-p (plist-get implementation :plan-file))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (delete-directory root t)))
  :doc "rejecting approval pauses the Goal without inventing a status"
  (let* ((root (make-temp-file "mevedel-goal-reject-" t))
         (buffer (generate-new-buffer " *goal-reject*"))
         (session (mevedel-session-create
                   "main" (test-mevedel-goal--workspace root)))
         (goal (mevedel-goal--create
                :status 'active :phase 'awaiting-approval :objective "x")))
    (unwind-protect
        (with-current-buffer buffer
          (setq-local mevedel--session session)
          (setf (mevedel-session-goal session) goal)
          (cl-letf (((symbol-function 'mevedel-goal--save-session-state)
                     #'ignore))
            (mevedel-goal--approval-callback "# Plan" buffer 'aborted))
          (should (eq 'paused (mevedel-goal-status goal))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (delete-directory root t)))
  :doc "invalid approval state cannot write an accepted plan"
  (let* ((root (make-temp-file "mevedel-goal-invalid-approval-" t))
         (buffer (generate-new-buffer " *goal-invalid-approval*"))
         (session (mevedel-session-create
                   "main" (test-mevedel-goal--workspace root)))
         (goal (mevedel-goal--create
                :status 'paused :phase 'awaiting-approval :objective "x"))
         wrote)
    (unwind-protect
        (with-current-buffer buffer
          (setq-local mevedel--session session)
          (setf (mevedel-session-goal session) goal)
          (cl-letf (((symbol-function 'mevedel-plan-accept)
                     (lambda (&rest _)
                       (setq wrote t))))
            (should-error
             (mevedel-goal--approval-callback
              "# Plan" buffer 'implement)
             :type 'user-error))
          (should-not wrote))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (delete-directory root t))))

(mevedel-deftest mevedel-goal-present-plan ()
  ,test
  (test)
  :doc "persists a presented plan before enqueueing its approval"
  (let* ((root (make-temp-file "mevedel-goal-present-" t))
         (session (mevedel-session-create
                   "main" (test-mevedel-goal--workspace root)))
         queued)
    (unwind-protect
        (with-temp-buffer
          (setq-local mevedel--session session)
          (cl-letf (((symbol-function 'mevedel-plan-queue--enqueue)
                     (lambda (entry) (setq queued entry))))
            (mevedel-goal-present-plan "# Plan\n\nDo it." (current-buffer)))
          (should (file-exists-p
                   (mevedel-plan-current-path session (current-buffer))))
          (should (equal "# Plan\n\nDo it." (plist-get queued :body))))
      (delete-directory root t))))

(mevedel-deftest mevedel-goal-restore-pending-approval ()
  ,test
  (test)
  :doc "requeues a persisted presented plan for an awaiting Goal"
  (let* ((session (mevedel-session--create
                   :name "main"
                   :goal (mevedel-goal--create
                          :status 'active :phase 'awaiting-approval)
                   :plan-metadata '(:status presented)))
         queued)
    (with-temp-buffer
      (setq-local mevedel--session session)
      (cl-letf (((symbol-function 'mevedel-plan-current-body)
                 (lambda (&optional _session) "# Pending"))
                ((symbol-function 'mevedel-plan-queue--enqueue)
                 (lambda (entry) (setq queued entry))))
        (mevedel-goal-restore-pending-approval session (current-buffer)))
      (should (equal "# Pending" (plist-get queued :body)))
      (should (eq session (plist-get queued :session))))))

(mevedel-deftest mevedel-goal-settle-turn ()
  ,test
  (test)
  :doc "implementation settlement advances to review but never completes"
  (let* ((root (make-temp-file "mevedel-goal-settle-" t))
         (buffer (generate-new-buffer " *goal-settle*"))
         (session (mevedel-session-create
                   "main" (test-mevedel-goal--workspace root)))
         (goal (mevedel-goal--create
                :status 'active :phase 'implementing :objective "x")))
    (unwind-protect
        (with-current-buffer buffer
          (setq-local mevedel--session session)
          (setf (mevedel-session-goal session) goal)
          (mevedel-goal-settle-turn
           (test-mevedel-goal--fsm buffer 'implementing))
          (should (eq 'active (mevedel-goal-status goal)))
          (should (eq 'reviewing (mevedel-goal-phase goal))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (delete-directory root t)))
  :doc "only a settled review with evidence completes the Goal"
  (let* ((root (make-temp-file "mevedel-goal-complete-" t))
         (buffer (generate-new-buffer " *goal-complete*"))
         (session (mevedel-session-create
                   "main" (test-mevedel-goal--workspace root)))
         (goal (mevedel-goal--create
                :status 'active :phase 'reviewing :objective "x"
                :review-summary "All acceptance checks pass.")))
    (unwind-protect
        (with-current-buffer buffer
          (setq-local mevedel--session session)
          (setf (mevedel-session-goal session) goal)
          (mevedel-goal-settle-turn
           (test-mevedel-goal--fsm buffer 'reviewing))
          (should (eq 'complete (mevedel-goal-status goal))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (delete-directory root t))))

(mevedel-deftest mevedel-goal-dispatch-after-turn ()
  ,test
  (test)
  :doc "dispatches visible review after implementation settlement"
  (let* ((buffer (generate-new-buffer " *goal-review-dispatch*"))
         (session (mevedel-session--create :name "main"))
         (goal (mevedel-goal--create
                :status 'active :phase 'reviewing :objective "Fix it"
                :current-plan '(:absolute-path "/tmp/plan.md")))
         dispatched)
    (unwind-protect
        (with-current-buffer buffer
          (setq-local mevedel--session session)
          (setf (mevedel-session-goal session) goal)
          (let ((mevedel-goal-dispatch-function
                 (lambda (&rest args) (setq dispatched args))))
            (mevedel-goal-dispatch-after-turn
             (test-mevedel-goal--fsm buffer 'implementing)))
          (should (eq 'reviewing (car dispatched)))
          (should (string-match-p "Fix it" (cadr dispatched))))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(mevedel-deftest mevedel-goal-supervised-cycle ()
  ,test
  (test)
  :doc "runs planning, approval, implementation, and review through a deterministic boundary"
  (let* ((root (make-temp-file "mevedel-goal-cycle-" t))
         (buffer (generate-new-buffer " *goal-cycle*"))
         (session (mevedel-session-create
                   "main" (test-mevedel-goal--workspace root)))
         dispatched
         implementation)
    (unwind-protect
        (with-current-buffer buffer
          (setq-local mevedel--session session)
          (let ((mevedel-goal-dispatch-function
                 (lambda (phase prompt display)
                   (push (list phase prompt display) dispatched))))
            (mevedel-goal-start "Fix the race")
            (insert "<proposed_plan>\n# Plan\n\nFix and test.\n</proposed_plan>")
            (cl-letf (((symbol-function 'mevedel-goal-present-plan)
                       #'ignore))
              (mevedel-goal--post-response (point-min) (point-max)))
            (cl-letf (((symbol-function 'mevedel-goal--save-session-state)
                       #'ignore)
                      ((symbol-function 'mevedel-goal--ensure-reference-reminder)
                       #'ignore)
                      ((symbol-function 'mevedel-goal--call-with-workload)
                       (lambda (_workload fn) (funcall fn)))
                      ((symbol-function 'mevedel--implement-plan)
                       (lambda (input)
                         (setq implementation input)
                         nil)))
              (mevedel-goal--approval-callback
               "# Plan\n\nFix and test." buffer 'implement))
            (should implementation)
            (mevedel-goal-settle-turn
             (test-mevedel-goal--fsm buffer 'implementing))
            (mevedel-goal-dispatch-after-turn
             (test-mevedel-goal--fsm buffer 'implementing))
            (erase-buffer)
            (insert "Review confirms the objective and tests pass.")
            (mevedel-goal--post-response (point-min) (point-max))
            (mevedel-goal-settle-turn
             (test-mevedel-goal--fsm buffer 'reviewing))
            (should (eq 'complete
                        (mevedel-goal-status
                         (mevedel-session-goal session))))
            (should (equal '(reviewing planning)
                           (mapcar #'car dispatched)))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (delete-directory root t))))

(provide 'test-mevedel-goal)
;;; test-mevedel-goal.el ends here
