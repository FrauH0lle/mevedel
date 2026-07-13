;;; test-mevedel-goal.el --- Tests for mevedel-goal.el -*- lexical-binding: t -*-

;;; Commentary:

;; Supervised one-cycle Goal lifecycle tests.

;;; Code:

(require 'gptel-request)
(require 'mevedel)
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

(defun test-mevedel-goal--own (goal session root)
  "Make GOAL a valid current-checkout Goal owned by SESSION at ROOT."
  (require 'mevedel-session-persistence)
  (mevedel-session-persistence-ensure-files session (current-buffer))
  (let ((id (mevedel-session-session-id session)))
    (setf (mevedel-session-session-id session) id
          (mevedel-session-working-directory session)
          (file-name-as-directory root)
          (mevedel-goal-owner-session goal) id
          (mevedel-goal-execution-home goal)
          (list :kind 'current :directory (file-name-as-directory root)
                :session-id id :locked nil)
          (mevedel-goal-implementation-context goal) 'full)
    goal))

(defun test-mevedel-goal--fsm (buffer phase)
  "Return a minimal FSM for BUFFER and Goal PHASE."
  (let ((attempt-id
         (when (buffer-live-p buffer)
           (with-current-buffer buffer
             (when-let* ((session (and (bound-and-true-p mevedel--session)
                                       mevedel--session))
                         (goal (mevedel-session-goal session))
                         (checkpoint (mevedel-goal-checkpoint goal))
                         ((eq phase (plist-get checkpoint :phase))))
               (plist-get checkpoint :attempt-id))))))
    (gptel-make-fsm
     :info (list :buffer buffer :mevedel-goal-phase phase
                 :mevedel-goal-attempt-id attempt-id))))

(defun test-mevedel-goal--record-guardian (goal decision plan-hash &rest _)
  "Record test guardian DECISION for GOAL and PLAN-HASH in its cycle."
  (mevedel-goal--cycle-put
   goal :guardian-audits
   (list (append decision (list :plan-hash plan-hash)))))

(defun test-mevedel-goal--git (root &rest args)
  "Run Git ARGS at ROOT or signal with its output."
  (with-temp-buffer
    (let* ((default-directory root)
           (exit (apply #'process-file "git" nil t nil args)))
      (unless (zerop exit)
        (error "Git failed: %s" (buffer-string)))
      (buffer-string))))

(defun test-mevedel-goal--init-git (root)
  "Create a real one-commit Git repository at ROOT."
  (test-mevedel-goal--git root "init")
  (test-mevedel-goal--git root "config" "user.email"
                          "mevedel@example.invalid")
  (test-mevedel-goal--git root "config" "user.name" "Mevedel Test")
  (with-temp-file (file-name-concat root "file.txt")
    (insert "base\n"))
  (test-mevedel-goal--git root "add" "file.txt")
  (test-mevedel-goal--git root "commit" "-m" "init"))

(mevedel-deftest mevedel-goal--current ()
  ,test
  (test)
  :doc "returns the session Goal and rejects missing session state"
  (with-temp-buffer
    (should-error (mevedel-goal--current) :type 'user-error)
    (let* ((goal (mevedel-goal--create :id "g1"))
           (mevedel--session (mevedel-session--create
                              :name "main" :goal goal)))
      (should (eq goal (mevedel-goal--current))))))

(mevedel-deftest mevedel-goal-owned-by-session-p ()
  ,test
  (test)
  :doc "requires both lifecycle and execution-home ownership"
  (let* ((session (mevedel-session--create :name "main" :session-id "s1"))
         (goal (mevedel-goal--create
                :owner-session "s1"
                :execution-home
                '(:kind current :directory "/tmp/" :session-id "s1"))))
    (should (mevedel-goal-owned-by-session-p goal session))
    (setf (plist-get (mevedel-goal-execution-home goal) :session-id) "s2")
    (should-not (mevedel-goal-owned-by-session-p goal session))))

(mevedel-deftest mevedel-goal--assert-execution-home ()
  ,test
  (test)
  :doc "accepts only the owning session at its recorded directory"
  (let* ((root (file-name-as-directory
                (make-temp-file "mevedel-goal-home-" t)))
         (session (mevedel-session--create
                   :name "main" :session-id "s1" :working-directory root))
         (goal (mevedel-goal--create
                :owner-session "s1"
                :execution-home
                (list :kind 'current :directory root :session-id "s1"))))
    (unwind-protect
        (progn
          (should (equal root
                         (mevedel-goal--assert-execution-home goal session)))
          (setf (mevedel-session-working-directory session) "/tmp/")
          (should-error (mevedel-goal--assert-execution-home goal session)))
      (delete-directory root t))))

(mevedel-deftest mevedel-goal-context-fragment ()
  ,test
  (test)
  :doc "renders stable Goal identity, progress, and execution-home context"
  (let* ((session (mevedel-session--create
                   :save-path "/tmp/session/"))
         (goal (mevedel-goal--create
                :id "g1" :objective "Ship safely" :status 'active
                :phase 'implementing :cycle 3 :approval-policy 'automatic
                :current-plan '(:path "goals/g1/cycle-003-plan.md")
                :review-findings "Fix the remaining race"
                :token-budget 1000 :token-usage 400
                :execution-home
                '(:kind worktree :directory "/tmp/goal/" :session-id "s1")))
         (context (mevedel-goal-context-fragment goal session)))
    (dolist (needle '("authority=\"session-sidecar\"" "Goal ID: g1"
                      "Objective: Ship safely" "Status: active"
                      "Phase: implementing" "Cycle: 3"
                      "Approval policy: automatic"
                      "Accepted plan: goals/g1/cycle-003-plan.md"
                      "Cycle index: /tmp/session/goals/g1/cycles.el"
                      "Latest review: continue - Fix the remaining race"
                      "Budget: 400/1000 tokens"
                      "Execution home: /tmp/goal/"))
      (should (string-match-p (regexp-quote needle) context))))
  :doc "is the identical prefix beneath every phase-specific request"
  (let* ((session (mevedel-session--create :save-path "/tmp/session/"))
         (mevedel--session session)
         (goal (mevedel-goal--create
                :id "g1" :objective "Ship" :status 'active
                :phase 'planning :cycle 1 :approval-policy 'automatic
                :current-plan '(:absolute-path "/tmp/plan.md")
                :execution-home '(:directory "/tmp/project/")))
         (context (mevedel-goal-context-fragment goal session))
         (prompts
          (list (cons "Planning instructions:"
                      (mevedel-goal--planning-prompt goal))
                (cons "Guardian instructions:"
                      (mevedel-goal--guardian-prompt goal "# Plan"))
                (cons "Review instructions:"
                      (mevedel-goal--review-prompt goal))
                (cons "Recovery instructions:"
                      (mevedel-goal--recovery-prompt
                       goal '(:attempt-id "a1" :dispatch-state unknown))))))
    (dolist (entry prompts)
      (should (string-prefix-p context (cdr entry)))
      (should (string-match-p (regexp-quote (car entry)) (cdr entry))))))

(mevedel-deftest mevedel-goal--refresh-request-context ()
  ,test
  (test)
  :doc "replaces stale checkpoint context while preserving phase input"
  (let* ((session (mevedel-session--create :save-path "/tmp/session/"))
         (mevedel--session session)
         (goal (mevedel-goal--create
                :id "g1" :objective "Ship" :status 'paused
                :phase 'planning :cycle 1 :token-budget 100
                :token-usage 10))
         (old (mevedel-goal-context-fragment goal session)))
    (setf (mevedel-goal-status goal) 'active
          (mevedel-goal-phase goal) 'reviewing
          (mevedel-goal-cycle goal) 2
          (mevedel-goal-token-usage goal) 40)
    (let ((refreshed
           (mevedel-goal--refresh-request-context
            goal (concat old "\n\nReview instructions:\nKeep this exact input"))))
      (dolist (needle '("Status: active" "Phase: reviewing" "Cycle: 2"
                        "Budget: 40/100 tokens" "Keep this exact input"))
        (should (string-match-p (regexp-quote needle) refreshed)))
      (should-not (string-match-p "Status: paused" refreshed))
      (should (= 1 (let ((start 0) (count 0))
                     (while (string-match "<goal-context" refreshed start)
                       (setq count (1+ count)
                             start (match-end 0)))
                     count))))))

(mevedel-deftest mevedel-goal--enqueue-event-reminder ()
  ,test
  (test)
  :doc "queues an event without mutating Goal lifecycle state"
  (let* ((goal (mevedel-goal--create
                :id "g1" :status 'active :phase 'planning :cycle 1))
         (session (mevedel-session--create :goal goal))
         (before (copy-mevedel-goal goal)))
    (mevedel-goal--enqueue-event-reminder session "planning started")
    (should (equal before goal))
    (should (equal '("Goal lifecycle event: planning started")
                   (mevedel-session-pending-reminders session)))))

(mevedel-deftest mevedel-goal--relative-dir ()
  ,test
  (test)
  :doc "uses the stable Goal ID as the artifact directory"
  (should (equal "goals/goal-42"
                 (mevedel-goal--relative-dir
                  (mevedel-goal--create :id "goal-42")))))

(mevedel-deftest mevedel-goal--current-plan-relative-path ()
  ,test
  (test)
  :doc "keeps one mutable current plan per Goal"
  (should (equal "goals/goal-42/current-plan.md"
                 (mevedel-goal--current-plan-relative-path
                  (mevedel-goal--create :id "goal-42")))))

(mevedel-deftest mevedel-goal--cycle-plan-relative-path ()
  ,test
  (test)
  :doc "numbers immutable accepted plans by cycle"
  (should (equal "goals/goal-42/cycle-007-plan.md"
                 (mevedel-goal--cycle-plan-relative-path
                  (mevedel-goal--create :id "goal-42" :cycle 7)))))

(mevedel-deftest mevedel-goal-cycle-record ()
  ,test
  (test)
  :doc "returns only the current cycle's lightweight record"
  (let ((goal (mevedel-goal--create
               :cycle 2 :cycles '((:cycle 1) (:cycle 2 :plan "p")))))
    (should (equal '(:cycle 2 :plan "p")
                   (mevedel-goal-cycle-record goal)))))

(mevedel-deftest mevedel-goal-latest-provider ()
  ,test
  (test)
  :doc "returns the newest actual policy for a workload across cycles"
  (let* ((goal (mevedel-goal--create
                :cycles '((:cycle 1 :providers
                           ((planning :provider "old" :effort low)))
                          (:cycle 2 :providers
                           ((review :provider "reviewer")
                            (planning :provider "new" :effort high))))))
         (before (copy-tree (mevedel-goal-cycles goal))))
    (should (equal '(:provider "new" :effort high)
                   (mevedel-goal-latest-provider goal 'planning)))
    (should (equal '(:provider "reviewer")
                   (mevedel-goal-latest-provider goal 'review)))
    (should-not (mevedel-goal-latest-provider goal 'implementation))
    (should (equal before (mevedel-goal-cycles goal)))))

(mevedel-deftest mevedel-goal--cycle-put ()
  ,test
  (test)
  :doc "updates one cycle record while preserving sorted history"
  (let ((goal (mevedel-goal--create
               :cycle 2 :cycles '((:cycle 1 :plan "one")))))
    (mevedel-goal--cycle-put goal :plan "two")
    (let ((cycles (mevedel-goal-cycles goal)))
      (should (= 2 (length cycles)))
      (should (equal "one" (plist-get (nth 0 cycles) :plan)))
      (should (equal "two" (plist-get (nth 1 cycles) :plan)))
      (should (plist-get (nth 1 cycles) :started-at)))))

(mevedel-deftest mevedel-goal--persist-cycle-index ()
  ,test
  (test)
  :doc "writes a readable lightweight index without artifact bodies"
  (let* ((root (make-temp-file "mevedel-cycle-index-" t))
         (session (mevedel-session-create
                   "main" (test-mevedel-goal--workspace root)))
         (goal (mevedel-goal--create
                :id "goal-index" :cycle 1
                :cycles '((:cycle 1 :plan "cycle-001-plan.md")))))
    (unwind-protect
        (with-temp-buffer
          (let ((path (mevedel-goal--persist-cycle-index
                       goal session (current-buffer))))
            (should (file-exists-p path))
            (with-temp-buffer
              (insert-file-contents path)
              (should (equal (mevedel-goal-cycles goal)
                             (read (current-buffer)))))))
      (delete-directory root t))))

(mevedel-deftest mevedel-goal--parse-review ()
  ,test
  (test)
  :doc "accepts exactly one complete, continue, or blocked review result"
  (should (equal '(:verdict continue :summary "Fix the remaining test.")
                 (mevedel-goal--parse-review
                  "<goal_review>\nverdict: continue\nsummary: Fix the remaining test.\n</goal_review>")))
  :doc "rejects malformed, duplicate, or unknown verdicts"
  (dolist (text '("complete"
                  "preface\n<goal_review>\nverdict: complete\nsummary: x\n</goal_review>"
                  "<goal_review>\nverdict: done\nsummary: x\n</goal_review>"
                  "<goal_review>\nverdict: complete\nverdict: continue\nsummary: x\n</goal_review>"))
    (should-not (mevedel-goal--parse-review text))))

(mevedel-deftest mevedel-goal--record-phase-policy ()
  ,test
  (test)
  :doc "records provider and effort for the current phase without prose"
  (let* ((session (mevedel-session--create :name "main"))
         (goal (mevedel-goal--create
                :id "goal-policy" :cycle 1 :status 'active
                :cycles '((:cycle 1)))))
    (setf (mevedel-session-goal session) goal)
    (let ((mevedel--session session))
      (cl-letf (((symbol-function 'mevedel-goal--persist-cycle-index)
                 #'ignore))
        (mevedel-goal--record-phase-policy
         'planning '(:backend nil :model model :effort high))))
    (let* ((providers (plist-get (car (mevedel-goal-cycles goal))
                                 :providers))
           (planning (alist-get 'planning providers)))
      (should (equal "model" (plist-get planning :provider)))
      (should (eq 'high (plist-get planning :effort))))))

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
              (should (eq 'current
                          (plist-get (mevedel-goal-execution-home goal)
                                     :kind)))
              (should (eq 'full
                          (mevedel-goal-implementation-context goal)))
              (should (equal (mevedel-session-session-id session)
                             (mevedel-goal-owner-session goal)))
              (should (eq 'planning (car dispatched)))
              (should (string-match-p "<proposed_plan>"
                                      (cadr dispatched))))))
      (delete-directory root t)))
  :doc "creates an automatic Goal only when explicitly requested"
  (let* ((root (make-temp-file "mevedel-goal-auto-start-" t))
         (session (mevedel-session-create
                   "main" (test-mevedel-goal--workspace root))))
    (unwind-protect
        (with-temp-buffer
          (setq-local mevedel--session session)
          (setf (mevedel-session-permission-mode session) 'accept-edits)
          (let* ((mevedel-goal-execution-home 'worktree)
                 (mevedel-goal-implementation-context 'full)
                 (mevedel-goal-dispatch-function #'ignore)
                 (goal (mevedel-goal-start "Ship" nil 'automatic)))
            (should (eq 'automatic (mevedel-goal-approval-policy goal)))
            (should (eq 'worktree
                        (plist-get (mevedel-goal-execution-home goal) :kind)))
            (should (eq 'focused
                        (mevedel-goal-implementation-context goal)))
            (should (eq 'accept-edits
                        (mevedel-session-permission-mode session)))))
      (delete-directory root t)))
  :doc "rejects a blank objective and declined unfinished replacement"
  (let* ((root (make-temp-file "mevedel-goal-replace-" t))
         (session (mevedel-session-create
                   "main" (test-mevedel-goal--workspace root))))
    (unwind-protect
        (with-temp-buffer
          (setq-local mevedel--session session)
          (let ((mevedel-goal-dispatch-function #'ignore))
            (should-error (mevedel-goal-start "  ") :type 'user-error)
            (mevedel-goal-start "First")
            (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) nil)))
              (should-error (mevedel-goal-start "Second")
                            :type 'user-error))))
      (delete-directory root t)))
  :doc "confirmed unfinished and unconfirmed complete Goals are replaceable"
  (let* ((root (make-temp-file "mevedel-goal-replace-" t))
         (session (mevedel-session-create
                   "main" (test-mevedel-goal--workspace root))))
    (unwind-protect
        (with-temp-buffer
          (setq-local mevedel--session session)
          (let ((mevedel-goal-dispatch-function #'ignore))
            (setf (mevedel-session-goal session)
                  (mevedel-goal--create :id "old" :objective "Old"
                                        :status 'paused :phase 'planning))
            (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
              (mevedel-goal-start "Replacement"))
            (should (equal "Replacement"
                           (mevedel-goal-objective
                            (mevedel-session-goal session))))
            (setf (mevedel-goal-status
                   (mevedel-session-goal session)) 'complete)
            (cl-letf (((symbol-function 'yes-or-no-p)
                       (lambda (&rest _)
                         (error "Unexpected confirmation"))))
              (mevedel-goal-start "Next"))
            (should (equal "Next"
                           (mevedel-goal-objective
                            (mevedel-session-goal session))))))
      (delete-directory root t)))
  :doc "retains a new paused Goal checkpoint when planning startup fails"
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
            (mevedel-goal-start "New"))
          (let ((goal (mevedel-session-goal session)))
            (should-not (eq previous goal))
            (should (eq 'paused (mevedel-goal-status goal)))
            (should (eq 'unknown
                        (plist-get (mevedel-goal-checkpoint goal)
                                   :dispatch-state)))
            (should (mevedel-goal-checkpoint goal))))
      (delete-directory root t))))
  :doc "failed replacement preserves an awaiting approval interaction"
  (with-temp-buffer
    (let* ((previous (mevedel-goal--create
                      :id "old" :objective "Old" :status 'active
                      :phase 'awaiting-approval :cycle 1
                      :cycles '((:cycle 1))))
           (entry (list :body "# Plan" :callback #'ignore))
           (metadata '(:status presented))
           (session (mevedel-session--create
                     :name "main" :goal previous :plan-queue (list entry)
                     :plan-metadata metadata))
           (mevedel--session session))
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                ((symbol-function 'mevedel-goal--dispatch-phase)
                 (lambda (&rest _) (error "Startup failed"))))
        (should-error (mevedel-goal-start "New")))
      (should (eq previous (mevedel-session-goal session)))
      (should (eq 'awaiting-approval (mevedel-goal-phase previous)))
      (should (equal (list entry) (mevedel-session-plan-queue session)))
      (should (eq metadata (mevedel-session-plan-metadata session)))))

(mevedel-deftest mevedel-goal-description ()
  ,test
  (test)
  :doc "shows stable identity, lifecycle position, objective, and reason"
  (should
   (equal "Goal g1 [paused/planning, cycle 2]: Ship (Waiting)"
          (mevedel-goal-description
           (mevedel-goal--create
            :id "g1" :objective "Ship" :status 'paused
            :phase 'planning :cycle 2 :reason "Waiting")))))

(mevedel-deftest mevedel-goal-pause ()
  ,test
  (test)
  :doc "pauses an idle Goal immediately"
  (with-temp-buffer
    (let* ((goal (mevedel-goal--create
                  :id "g1" :objective "Ship" :status 'active
                  :phase 'planning :cycle 1 :cycles '((:cycle 1))))
           (session (mevedel-session--create :name "main" :goal goal)))
      (setq-local mevedel--session session)
      (cl-letf (((symbol-function 'mevedel-goal--save-session-state) #'ignore))
        (mevedel-goal-pause))
      (should (eq 'paused (mevedel-goal-status goal)))))
  :doc "waits for an active request boundary before pausing continuation"
  (with-temp-buffer
    (let* ((goal (mevedel-goal--create
                  :id "g1" :objective "Ship" :status 'active
                  :phase 'awaiting-approval :cycle 1 :cycles '((:cycle 1))))
           (session (mevedel-session--create :name "main" :goal goal)))
      (setq-local mevedel--session session
                  mevedel--current-request t)
      (mevedel-goal-pause)
      (should (eq 'active (mevedel-goal-status goal)))
      (should (mevedel-goal-pause-requested goal))
      (mevedel-goal-settle-turn
       (test-mevedel-goal--fsm (current-buffer) 'planning))
      (should (eq 'paused (mevedel-goal-status goal)))
      (should-not (mevedel-goal-pause-requested goal)))))

(mevedel-deftest mevedel-goal-edit ()
  ,test
  (test)
  :doc "changes the contract without replacing identity or cycle history"
  (with-temp-buffer
    (let* ((cycles '((:cycle 1 :plan "p")))
           (goal (mevedel-goal--create
                  :id "stable" :objective "Old" :status 'active
                  :phase 'reviewing :cycle 1 :cycles cycles))
           (session (mevedel-session--create :name "main" :goal goal)))
      (setq-local mevedel--session session)
      (cl-letf (((symbol-function 'mevedel-goal--save-session-state) #'ignore))
        (mevedel-goal-edit "New contract"))
      (should (equal "stable" (mevedel-goal-id goal)))
      (should (eq cycles (mevedel-goal-cycles goal)))
      (should (equal "New contract" (mevedel-goal-objective goal)))
      (should (eq 'paused (mevedel-goal-status goal)))
      (should (eq 'planning (mevedel-goal-phase goal))))))

(mevedel-deftest mevedel-goal-set-token-budget ()
  ,test
  (test)
  :doc "sets, persists, and removes the session-local Goal budget"
  (let* ((root (make-temp-file "mevedel-goal-budget-" t))
         (goal (mevedel-goal--create
                :id "g1" :objective "Ship" :status 'paused :phase 'planning
                :approval-policy 'supervised :cycle 1 :cycles '((:cycle 1))))
         (session (mevedel-session-create
                   "main" (test-mevedel-goal--workspace root))))
    (unwind-protect
        (with-temp-buffer
          (setq-local mevedel--session session)
          (test-mevedel-goal--own goal session root)
          (setf (mevedel-session-goal session) goal)
          (mevedel-goal-set-token-budget 100)
          (should (= 100 (mevedel-goal-token-budget goal)))
          (should (= 100 mevedel-goal-token-budget))
          (let* ((sidecar
                  (mevedel-session-persistence-load-sidecar
                   (mevedel-session-persistence--sidecar-path
                    (mevedel-session-save-path session))))
                 (saved (mevedel-session-persistence--goal-from-plist
                         (plist-get sidecar :goal))))
            (should (= 100 (mevedel-goal-token-budget saved))))
          (mevedel-goal-set-token-budget nil)
          (should-not (mevedel-goal-token-budget goal))
          (should-error (mevedel-goal-set-token-budget 0)
                        :type 'user-error))
      (delete-directory root t))))

(mevedel-deftest mevedel-goal-clear ()
  ,test
  (test)
  :doc "removes control state without deleting transcript, artifacts, or work"
  (let* ((root (make-temp-file "mevedel-goal-clear-" t))
         (artifact (file-name-concat root "cycle-001-plan.md"))
         (work (file-name-concat root "work.el"))
         (goal (mevedel-goal--create
                :id "g1" :objective "Ship" :status 'paused
                :phase 'planning :cycle 1 :cycles '((:cycle 1))))
         (session (mevedel-session--create :name "main" :goal goal)))
    (unwind-protect
        (with-temp-buffer
          (write-region "plan" nil artifact nil 'silent)
          (write-region "work" nil work nil 'silent)
          (insert "transcript")
          (setq-local mevedel--session session)
          (cl-letf (((symbol-function 'mevedel-goal--save-session-state)
                     #'ignore))
            (mevedel-goal-clear))
          (should-not (mevedel-session-goal session))
          (should (member
                   "Goal lifecycle event: cleared; transcript and artifacts were preserved"
                   (mevedel-session-pending-reminders session)))
          (should (equal "transcript" (buffer-string)))
          (should (file-exists-p artifact))
          (should (file-exists-p work)))
      (delete-directory root t))))

(mevedel-deftest mevedel-goal-resume ()
  ,test
  (test)
  :doc "dispatches the saved safe phase and reviews interrupted implementation"
  (dolist (case '((planning . planning)
                  (reviewing . reviewing)
                  (implementing . reviewing)))
    (let* ((root (make-temp-file "mevedel-goal-resume-phase-" t))
           (goal (mevedel-goal--create
                  :id "g1" :objective "Ship" :status 'paused
                  :phase (car case) :cycle 1 :cycles '((:cycle 1))
                  :current-plan '(:absolute-path "/tmp/plan")))
           (session (mevedel-session-create
                     "main" (test-mevedel-goal--workspace root)))
           dispatched)
      (unwind-protect
          (with-temp-buffer
            (setq-local mevedel--session session)
            (test-mevedel-goal--own goal session root)
            (setf (mevedel-session-goal session) goal)
            (let ((mevedel-goal-dispatch-function
                   (lambda (phase &rest _) (setq dispatched phase))))
              (mevedel-goal-resume))
            (should (eq (cdr case) dispatched))
            (should (eq 'active (mevedel-goal-status goal))))
        (delete-directory root t))))
  :doc "reconstructs approval instead of dispatching a model request"
  (let ((root (make-temp-file "mevedel-goal-resume-approval-" t)))
    (unwind-protect
        (with-temp-buffer
          (let* ((goal (mevedel-goal--create
                        :id "g1" :objective "Ship" :status 'paused
                        :phase 'awaiting-approval :cycle 1
                        :cycles '((:cycle 1))))
                 (session (mevedel-session-create
                           "main" (test-mevedel-goal--workspace root) root)))
            (setq-local mevedel--session session)
            (test-mevedel-goal--own goal session root)
            (setf (mevedel-session-goal session) goal)
            (mevedel-plan-write-current "# Plan" session (current-buffer))
            (cl-letf (((symbol-function 'mevedel-queue--render-head)
                       #'ignore))
              (mevedel-goal-resume))
            (should (= 1 (length (mevedel-session-plan-queue session))))
            (should (eq 'active (mevedel-goal-status goal)))))
      (delete-directory root t)))
  :doc "blocked resume replans with the blocker and new input"
  (let* ((root (make-temp-file "mevedel-goal-resume-blocked-" t))
         (goal (mevedel-goal--create
                :id "g1" :objective "Ship" :status 'blocked
                :phase 'reviewing :cycle 1 :cycles '((:cycle 1))
                :reason "Need credentials"))
         (session (mevedel-session-create
                   "main" (test-mevedel-goal--workspace root))))
    (unwind-protect
        (with-temp-buffer
          (setq-local mevedel--session session)
          (test-mevedel-goal--own goal session root)
          (setf (mevedel-session-goal session) goal)
          (let ((mevedel-goal-dispatch-function #'ignore))
            (mevedel-goal-resume "Credentials installed"))
          (should (eq 'planning (mevedel-goal-phase goal)))
          (should (string-match-p "Need credentials"
                                  (mevedel-goal-review-findings goal)))
          (should (string-match-p "Credentials installed"
                                  (mevedel-goal-review-findings goal))))
      (delete-directory root t)))
  :doc "resolves a changed provider at resume while retaining exact input"
  (let* ((root (make-temp-file "mevedel-goal-resume-provider-" t))
         (goal (mevedel-goal--create
                :id "g1" :objective "Ship" :status 'paused
                :phase 'planning :cycle 1 :cycles '((:cycle 1))
                :checkpoint
                '(:phase planning :cycle 1 :input "Exact prior request"
                  :workload planning :provider "old:model" :effort low
                  :attempt 1 :attempt-id "g1/1/planning/1" :retry-count 0
                  :dispatch-state failed :request-started nil
                  :last-settled-boundary nil
                  :prepared-at "2026-01-01T00:00:00Z")))
         (session (mevedel-session-create
                   "main" (test-mevedel-goal--workspace root)))
         observed)
    (unwind-protect
        (with-temp-buffer
          (setq-local mevedel--session session)
          (test-mevedel-goal--own goal session root)
          (setf (mevedel-session-goal session) goal)
          (cl-letf (((symbol-function 'mevedel-model-resolve-workload)
                     (lambda (&rest _)
                       '(:backend nil :model new-model :effort high))))
            (let ((mevedel-goal-dispatch-function
                   (lambda (_phase prompt _display)
                     (setq observed
                           (list prompt gptel-model
                                 gptel-reasoning-effort)))))
              (mevedel-goal-resume)))
          (should (string-match-p "Exact prior request" (car observed)))
          (should (string-match-p "Status: active" (car observed)))
          (should (equal (cdr observed) '(new-model high)))
          (should (equal "new-model"
                         (plist-get (mevedel-goal-checkpoint goal)
                                    :provider))))
      (delete-directory root t)))
  :doc "directly retries implementation known not to have started"
  (let* ((root (make-temp-file "mevedel-goal-resume-not-started-" t))
         (input '(:plan-file "plan.md" :permission-mode default))
         (goal (mevedel-goal--create
                :id "g1" :objective "Ship" :status 'paused
                :phase 'implementing :cycle 1 :cycles '((:cycle 1))
                :checkpoint
                (list :phase 'implementing :cycle 1 :input input
                      :workload 'implementation :provider "old:model"
                      :effort nil :attempt 1
                      :attempt-id "g1/1/implementing/1" :retry-count 0
                      :dispatch-state 'failed :request-started nil
                      :last-settled-boundary nil
                      :prepared-at "2026-01-01T00:00:00Z")))
         (session (mevedel-session-create
                   "main" (test-mevedel-goal--workspace root)))
         received)
    (unwind-protect
        (with-temp-buffer
          (setq-local mevedel--session session)
          (test-mevedel-goal--own goal session root)
          (setf (mevedel-session-goal session) goal)
          (cl-letf (((symbol-function 'mevedel-model-resolve-workload)
                     (lambda (&rest _)
                       '(:backend nil :model implementer :effort nil)))
                    ((symbol-function 'mevedel--implement-plan)
                     (lambda (value) (setq received value) nil)))
            (mevedel-goal-resume))
          (should (equal "plan.md" (plist-get received :plan-file)))
          (should (string-match-p
                   "Status: active" (plist-get received :goal-context)))
          (should (eq 'implementing (mevedel-goal-phase goal))))
      (delete-directory root t)))
  :doc "continues the automatic cascade after the user raises its budget"
  (let* ((root (make-temp-file "mevedel-goal-resume-budget-" t))
         (goal (mevedel-goal--create
                :id "g1" :objective "Ship" :status 'paused
                :phase 'awaiting-approval :approval-policy 'automatic
                :cycle 1 :cycles '((:cycle 1))
                :token-budget 10 :token-usage 10
                :checkpoint '(:phase planning :attempt-id "planning-1"
                              :dispatch-state settled)))
         (session (mevedel-session-create
                   "main" (test-mevedel-goal--workspace root)))
         guarded)
    (unwind-protect
        (with-temp-buffer
          (setq-local mevedel--session session
                      mevedel-goal-token-budget 20)
          (test-mevedel-goal--own goal session root)
          (setf (mevedel-session-goal session) goal)
          (cl-letf (((symbol-function 'mevedel-goal--guard-current-plan)
                     (lambda (&rest _) (setq guarded t))))
            (mevedel-goal-resume))
          (should guarded)
          (should (= 20 (mevedel-goal-token-budget goal)))
          (should (eq 'active (mevedel-goal-status goal))))
      (delete-directory root t))))

(mevedel-deftest mevedel-goal-settle-failure ()
  ,test
  (test)
  :doc "consumes a deferred pause without persisting before request teardown"
  (with-temp-buffer
    (let* ((goal (mevedel-goal--create
                  :id "g1" :objective "Ship" :status 'active
                  :phase 'planning :cycle 1 :cycles '((:cycle 1))
                  :pause-requested t))
           (session (mevedel-session--create :name "main" :goal goal))
           saved)
      (setq-local mevedel--session session)
      (cl-letf (((symbol-function 'mevedel-goal--save-session-state)
                 (lambda (&rest _) (setq saved t))))
        (mevedel-goal-settle-failure
         (test-mevedel-goal--fsm (current-buffer) 'planning)))
      (should-not saved)
      (should (eq 'paused (mevedel-goal-status goal)))
      (should-not (mevedel-goal-pause-requested goal))
      (should (mevedel-session-pending-reminders session)))))

(mevedel-deftest mevedel-goal-persist-failure ()
  ,test
  (test)
  :doc "persists the failed checkpoint after request teardown"
  (let* ((root (make-temp-file "mevedel-goal-failure-save-" t))
         (goal (mevedel-goal--create
                :id "g1" :objective "Ship" :status 'paused
                :phase 'planning :cycle 1 :cycles '((:cycle 1))))
         (session (mevedel-session-create
                   "main" (test-mevedel-goal--workspace root))))
    (unwind-protect
        (with-temp-buffer
          (setq-local mevedel--session session)
          (setf (mevedel-session-goal session) goal
                (mevedel-goal-checkpoint goal)
                '(:phase planning :cycle 1 :input "Exact"
                  :workload planning :provider "p:m" :effort nil
                  :plan-reference nil :attempt 1 :attempt-id "g1/1/planning/1"
                  :retry-count 0 :dispatch-state failed :request-started t
                  :last-settled-boundary nil
                  :prepared-at "2026-01-01T00:00:00Z"))
          (mevedel-goal-persist-failure
           (test-mevedel-goal--fsm (current-buffer) 'planning))
          (should (file-exists-p
                   (mevedel-session-persistence--sidecar-path
                    (mevedel-session-save-path session)))))
      (delete-directory root t))))

(mevedel-deftest mevedel-goal-dispatch-after-failure ()
  ,test
  (test)
  :doc "retries the exact failed read-only input under newly resolved policy"
  (let* ((root (make-temp-file "mevedel-goal-failure-retry-" t))
         (goal (mevedel-goal--create
                :id "g1" :objective "Ship" :status 'active
                :phase 'planning :cycle 1 :cycles '((:cycle 1))
                :checkpoint
                '(:phase planning :cycle 1 :input "Exact retry"
                  :workload planning :provider "old:model" :effort nil
                  :attempt 1 :attempt-id "g1/1/planning/1" :retry-count 1
                  :dispatch-state failed :request-started t
                  :last-settled-boundary nil
                  :prepared-at "2026-01-01T00:00:00Z")))
         (session (mevedel-session-create
                   "main" (test-mevedel-goal--workspace root)))
         observed)
    (unwind-protect
        (with-temp-buffer
          (setq-local mevedel--session session)
          (test-mevedel-goal--own goal session root)
          (setf (mevedel-session-goal session) goal)
          (cl-letf (((symbol-function 'mevedel-model-resolve-workload)
                     (lambda (&rest _)
                       '(:backend nil :model retry-model :effort high))))
            (let ((mevedel-goal-dispatch-function
                   (lambda (_phase prompt _display)
                     (setq observed
                           (list prompt gptel-model
                                 gptel-reasoning-effort)))))
              (mevedel-goal-dispatch-after-failure
               (test-mevedel-goal--fsm (current-buffer) 'planning))))
          (should (string-match-p "Exact retry" (car observed)))
          (should (string-match-p "Status: active" (car observed)))
          (should (equal (cdr observed) '(retry-model high))))
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
    (should (eq gptel-reasoning-effort 'medium)))
  :doc "checkpoints and pauses policy failures before request startup"
  (let* ((root (make-temp-file "mevedel-goal-policy-failure-" t))
         (goal (mevedel-goal--create
                :id "g1" :objective "Ship" :status 'active
                :phase 'planning :cycle 1 :cycles '((:cycle 1))))
         (session (mevedel-session-create
                   "main" (test-mevedel-goal--workspace root))))
    (unwind-protect
        (with-temp-buffer
          (setq-local mevedel--session session
                      gptel-backend nil
                      gptel-model 'fallback-model
                      gptel-reasoning-effort 'medium)
          (setf (mevedel-session-goal session) goal)
          (cl-letf (((symbol-function 'mevedel-model-resolve-workload)
                     (lambda (&rest _) (error "Unsupported effort"))))
            (should-error
             (mevedel-goal--call-with-workload
              'planning #'ignore 'planning "Exact input")))
          (let ((checkpoint (mevedel-goal-checkpoint goal)))
            (should (eq 'paused (mevedel-goal-status goal)))
            (should (eq 'failed (plist-get checkpoint :dispatch-state)))
            (should-not (plist-get checkpoint :request-started))
            (should (equal "Exact input" (plist-get checkpoint :input)))))
      (delete-directory root t)))
  :doc "marks synchronous post-start errors unknown for safe recovery"
  (let* ((root (make-temp-file "mevedel-goal-startup-failure-" t))
         (goal (mevedel-goal--create
                :id "g1" :objective "Ship" :status 'active
                :phase 'implementing :cycle 1 :cycles '((:cycle 1))))
         (session (mevedel-session-create
                   "main" (test-mevedel-goal--workspace root))))
    (unwind-protect
        (with-temp-buffer
          (setq-local mevedel--session session)
          (test-mevedel-goal--own goal session root)
          (setf (mevedel-session-goal session) goal)
          (cl-letf (((symbol-function 'mevedel-model-resolve-workload)
                     (lambda (&rest _)
                       '(:backend nil :model implementer :effort high))))
            (should-error
             (mevedel-goal--call-with-workload
              'implementation (lambda () (error "Transport broke"))
              'implementing '(:plan-file "plan.md"))))
          (let ((checkpoint (mevedel-goal-checkpoint goal)))
            (should (eq 'paused (mevedel-goal-status goal)))
            (should (eq 'unknown (plist-get checkpoint :dispatch-state)))
            (should (plist-get checkpoint :request-started))))
      (delete-directory root t))))

(mevedel-deftest mevedel-goal--policy-label ()
  ,test
  (test)
  :doc "formats model-only and backend policies with a stable fallback"
  (should (equal "planner"
                 (mevedel-goal--policy-label
                  '(:backend nil :model planner))))
  (let ((gptel--known-backends nil))
    (let ((backend (gptel-make-openai
                    "Provider" :key "test" :models '(model))))
      (should (equal "Provider:model"
                     (mevedel-goal--policy-label
                      (list :backend backend :model 'model))))))
  (cl-letf (((symbol-function 'gptel-backend-name)
             (lambda (_backend) (error "Unknown backend"))))
    (should (equal "backend:model"
                   (mevedel-goal--policy-label
                    '(:backend backend :model model))))))

(mevedel-deftest mevedel-goal--checkpoint-prepare ()
  ,test
  (test)
  :doc "writes exact phase input and resolved policy before dispatch"
  (let* ((root (make-temp-file "mevedel-goal-checkpoint-" t))
         (goal (mevedel-goal--create
                :id "g1" :objective "Ship" :status 'active
                :phase 'planning :cycle 2 :cycles '((:cycle 2))))
         (session (mevedel-session-create
                   "main" (test-mevedel-goal--workspace root))))
    (unwind-protect
        (with-temp-buffer
          (setq-local mevedel--session session)
          (test-mevedel-goal--own goal session root)
          (setf (mevedel-session-goal session) goal)
          (let ((checkpoint
                 (mevedel-goal--checkpoint-prepare
                  'planning "Exact prompt" 'planning
                  '(:backend nil :model planner :effort high))))
            (should (eq 'prepared (plist-get checkpoint :dispatch-state)))
            (should-not (plist-get checkpoint :request-started))
            (should (equal "Exact prompt" (plist-get checkpoint :input)))
            (should (equal "planner" (plist-get checkpoint :provider)))
            (should (eq 'high (plist-get checkpoint :effort)))
            (should (= 2 (plist-get checkpoint :cycle)))
            (should (= 1 (plist-get checkpoint :attempt)))
            (let* ((sidecar (mevedel-session-persistence-load-sidecar
                             (mevedel-session-persistence--sidecar-path
                              (mevedel-session-save-path session))))
                   (saved (plist-get (plist-get sidecar :goal) :checkpoint)))
              (should (equal checkpoint saved)))))
      (delete-directory root t))))

(mevedel-deftest mevedel-goal--checkpoint-state ()
  ,test
  (test)
  :doc "durably changes dispatch state and records failure evidence"
  (let* ((root (make-temp-file "mevedel-goal-checkpoint-state-" t))
         (goal (mevedel-goal--create
                :id "g1" :objective "Ship" :status 'active
                :phase 'planning :cycle 1 :cycles '((:cycle 1))))
         (session (mevedel-session-create
                   "main" (test-mevedel-goal--workspace root))))
    (unwind-protect
        (with-temp-buffer
          (setq-local mevedel--session session)
          (test-mevedel-goal--own goal session root)
          (setf (mevedel-session-goal session) goal)
          (mevedel-goal--checkpoint-prepare
           'planning "Exact" 'planning
           '(:backend nil :model planner :effort nil))
          (mevedel-goal--checkpoint-state
           'unknown :request-started t :error "Transport broke")
          (let ((checkpoint (mevedel-goal-checkpoint goal)))
            (should (eq 'unknown (plist-get checkpoint :dispatch-state)))
            (should (plist-get checkpoint :request-started))
            (should (equal "Transport broke"
                           (plist-get checkpoint :error)))))
      (delete-directory root t))))

(mevedel-deftest mevedel-goal--persist-checkpoint ()
  ,test
  (test)
  :doc "writes Goal recovery state to the real session sidecar"
  (let* ((root (make-temp-file "mevedel-goal-persist-checkpoint-" t))
         (goal (mevedel-goal--create
                :id "g1" :objective "Ship" :status 'paused
                :phase 'planning :cycle 1 :cycles '((:cycle 1))))
         (session (mevedel-session-create
                   "main" (test-mevedel-goal--workspace root))))
    (unwind-protect
        (with-temp-buffer
          (setq-local mevedel--session session)
          (setf (mevedel-session-goal session) goal
                (mevedel-goal-checkpoint goal)
                '(:phase planning :cycle 1 :input "Exact"
                  :workload planning :provider "p:m" :effort nil
                  :plan-reference nil :attempt 1 :attempt-id "g1/1/planning/1"
                  :retry-count 0 :dispatch-state failed :request-started nil
                  :last-settled-boundary nil
                  :prepared-at "2026-01-01T00:00:00Z"))
          (mevedel-goal--persist-checkpoint session (current-buffer))
          (should (file-exists-p
                   (mevedel-session-persistence--sidecar-path
                    (mevedel-session-save-path session)))))
      (delete-directory root t))))

(mevedel-deftest mevedel-goal--checkpoint-settle ()
  ,test
  (test)
  :doc "settles only the matching request attempt"
  (with-temp-buffer
    (let* ((attempt-id "g1/1/planning/1")
           (goal (mevedel-goal--create
                  :id "g1" :checkpoint
                  (list :attempt-id attempt-id :dispatch-state 'started)))
           (session (mevedel-session--create :name "main" :goal goal))
           (fsm (test-mevedel-goal--fsm (current-buffer) 'planning)))
      (setq-local mevedel--session session)
      (setf (gptel-fsm-info fsm)
            (plist-put (gptel-fsm-info fsm)
                       :mevedel-goal-attempt-id attempt-id))
      (mevedel-goal--checkpoint-settle fsm 'settled)
      (should (eq 'settled
                  (plist-get (mevedel-goal-checkpoint goal)
                             :dispatch-state)))
      (should (plist-get (mevedel-goal-checkpoint goal) :settled-at)))))

(mevedel-deftest mevedel-goal--fsm-failure-reason ()
  ,test
  (test)
  :doc "prefers structured provider messages over terminal status"
  (let ((fsm (test-mevedel-goal--fsm (current-buffer) 'planning)))
    (setf (gptel-fsm-info fsm)
          (plist-put (gptel-fsm-info fsm) :status "429")
          (gptel-fsm-info fsm)
          (plist-put (gptel-fsm-info fsm) :error
                     '(:type quota :message "Credits exhausted")))
    (should (equal "Credits exhausted"
                   (mevedel-goal--fsm-failure-reason fsm 'error)))))

(mevedel-deftest mevedel-goal--transient-failure-p ()
  ,test
  (test)
  :doc "classifies temporary transport failures but not quota failures"
  (should (mevedel-goal--transient-failure-p "Network timeout"))
  (should-not (mevedel-goal--transient-failure-p "Credits exhausted")))

(mevedel-deftest mevedel-goal--terminal-provider-failure-p ()
  ,test
  (test)
  :doc "classifies provider intervention failures"
  (dolist (reason '("Quota exhausted" "Authentication failed"
                    "Persistent rate limit 429"))
    (should (mevedel-goal--terminal-provider-failure-p reason)))
  (should-not (mevedel-goal--terminal-provider-failure-p
               "Temporary network timeout")))

(mevedel-deftest mevedel-goal--estimate-input-tokens ()
  ,test
  (test)
  :doc "estimates strings and structured implementation inputs"
  (should (= 2 (mevedel-goal--estimate-input-tokens "12345")))
  (should (> (mevedel-goal--estimate-input-tokens '(:plan "p")) 0)))

(mevedel-deftest mevedel-goal--record-token-usage ()
  ,test
  (test)
  :doc "charges provider-reported usage once per checkpoint"
  (let ((goal (mevedel-goal--create
               :token-usage 3 :checkpoint '(:usage-recorded nil))))
    (should (= 7 (mevedel-goal--record-token-usage
                  goal '(:tokens (:input 2 :output 5)))))
    (should (= 10 (mevedel-goal-token-usage goal)))
    (should-not (mevedel-goal--record-token-usage
                 goal '(:tokens (:input 100 :output 100))))
    (should (= 10 (mevedel-goal-token-usage goal))))
  :doc "falls back to the existing estimator when usage is unavailable"
  (let ((goal (mevedel-goal--create
               :token-usage 0
               :checkpoint '(:usage-recorded nil :token-baseline 8
                             :estimated-input-tokens 2))))
    (cl-letf (((symbol-function 'mevedel--estimate-tokens)
               (lambda () 13)))
      (should (= 5 (mevedel-goal--record-token-usage goal nil)))
      (should (= 5 (mevedel-goal-token-usage goal))))))

(mevedel-deftest mevedel-goal--recovery-prompt ()
  ,test
  (test)
  :doc "anchors recovery in repository evidence and forbids mutation replay"
  (let ((prompt
         (mevedel-goal--recovery-prompt
          (mevedel-goal--create
           :objective "Ship" :cycle 1
           :current-plan '(:absolute-path "/tmp/plan.md"))
          '(:attempt-id "g1/1/implementing/1" :dispatch-state unknown))))
    (dolist (needle '("Ship" "/tmp/plan.md" "actual repository"
                      "Do not replay" "verdict: complete|continue|blocked"))
      (should (string-match-p (regexp-quote needle) prompt)))))

(mevedel-deftest mevedel-goal-recovery ()
  ,test
  (test)
  :doc "audits real partial work and chooses a new plan without replaying mutation"
  (let* ((root (make-temp-file "mevedel-goal-recovery-" t))
         (work (file-name-concat root "partial.el"))
         (checkpoint
          '(:phase implementing :cycle 1 :input (:plan-file "plan.md")
            :workload implementation :provider "p:m" :effort high
            :plan-reference "plan.md" :attempt 1
            :attempt-id "g1/1/implementing/1" :retry-count 0
            :dispatch-state unknown :request-started t
            :last-settled-boundary nil :prepared-at "2026-01-01T00:00:00Z"))
         (goal (mevedel-goal--create
                :id "g1" :objective "Ship" :status 'paused
                :phase 'implementing :cycle 1 :cycles '((:cycle 1))
                :current-plan '(:absolute-path "/tmp/plan.md")
                :checkpoint checkpoint))
         (session (mevedel-session-create
                   "main" (test-mevedel-goal--workspace root)))
         dispatched
         replayed)
    (unwind-protect
        (with-temp-buffer
          (write-region "partial mutation" nil work nil 'silent)
          (setq-local mevedel--session session
                      default-directory (file-name-as-directory root))
          (test-mevedel-goal--own goal session root)
          (setf (mevedel-session-goal session) goal)
          (cl-letf (((symbol-function 'mevedel--implement-plan)
                     (lambda (&rest _) (setq replayed t))))
            (let ((mevedel-goal-dispatch-function
                   (lambda (phase prompt &rest _)
                     (setq dispatched (list phase prompt)))))
              (mevedel-goal-resume)))
          (should-not replayed)
          (should (file-exists-p work))
          (should (eq 'reviewing (mevedel-goal-phase goal)))
          (should (eq 'reviewing (car dispatched)))
          (should (string-match-p "Do not replay" (cadr dispatched)))
          (erase-buffer)
          (insert "<goal_review>\nverdict: continue\nsummary: Partial file needs a corrected plan.\n</goal_review>")
          (mevedel-goal--post-response (point-min) (point-max))
          (let* ((fsm (test-mevedel-goal--fsm
                       (current-buffer) 'reviewing))
                 (attempt-id
                  (plist-get (mevedel-goal-checkpoint goal) :attempt-id)))
            (setf (gptel-fsm-info fsm)
                  (plist-put (gptel-fsm-info fsm)
                             :mevedel-goal-attempt-id attempt-id))
            (mevedel-goal-settle-turn fsm))
          (should (eq 'planning (mevedel-goal-phase goal)))
          (should (= 2 (mevedel-goal-cycle goal))))
      (delete-directory root t))))

(mevedel-deftest mevedel-goal-retry-policy ()
  ,test
  (test)
  :doc "retries transient read-only failures once and pauses at the bound"
  (with-temp-buffer
    (let* ((attempt-id "g1/1/planning/1")
           (checkpoint
            (list :phase 'planning :cycle 1 :input "Exact"
                  :workload 'planning :provider "p:m" :effort nil
                  :attempt 1 :attempt-id attempt-id :retry-count 0
                  :dispatch-state 'started :last-settled-boundary nil))
           (goal (mevedel-goal--create
                  :id "g1" :objective "Ship" :status 'active
                  :phase 'planning :cycle 1 :cycles '((:cycle 1))
                  :checkpoint checkpoint))
           (session (mevedel-session--create :name "main" :goal goal))
           (fsm (test-mevedel-goal--fsm (current-buffer) 'planning)))
      (setq-local mevedel--session session)
      (setf (gptel-fsm-info fsm)
            (plist-put (gptel-fsm-info fsm)
                       :mevedel-goal-attempt-id attempt-id)
            (gptel-fsm-info fsm)
            (plist-put (gptel-fsm-info fsm) :error "Network timeout"))
      (let ((mevedel-goal-max-transient-retries 1))
        (mevedel-goal-settle-failure fsm 'error)
        (should (eq 'active (mevedel-goal-status goal)))
        (should (= 1 (plist-get (mevedel-goal-checkpoint goal)
                                :retry-count)))
        (setf (mevedel-goal-status goal) 'active)
        (mevedel-goal-settle-failure fsm 'error)
        (should (eq 'paused (mevedel-goal-status goal)))))))

(mevedel-deftest mevedel-goal--dispatch-gptel ()
  ,test
  (test)
  :doc "sends a phase under the caller's policy and tags the request"
  (with-temp-buffer
    (let ((fsm (test-mevedel-goal--fsm (current-buffer) nil)))
      (cl-letf (((symbol-function 'mevedel-goal--insert-and-send)
                 (lambda (&rest _) fsm)))
        (should (eq fsm
                    (mevedel-goal--dispatch-gptel
                     'reviewing "Review" "Review Goal"))))
      (should (eq (plist-get (gptel-fsm-info fsm) :mevedel-goal-phase)
                  'reviewing)))))

(mevedel-deftest mevedel-goal--dispatch-phase ()
  ,test
  (test)
  :doc "dispatches only request-bearing Goal phases"
  (let* ((goal (mevedel-goal--create
                :id "g1" :objective "Ship" :status 'active
                :phase 'planning :cycle 1))
         (mevedel--session (mevedel-session--create :goal goal))
         received)
    (let ((mevedel-goal-dispatch-function
           (lambda (&rest args) (setq received args))))
      (cl-letf (((symbol-function 'mevedel-goal--call-with-workload)
                 (lambda (_workload function &rest _) (funcall function))))
        (mevedel-goal--dispatch-phase 'planning "Prompt" "Display"))
      (should (eq 'planning (car received)))
      (should (string-match-p "authority=\"session-sidecar\""
                              (cadr received)))
      (should (string-match-p "Prompt" (cadr received)))
      (should (equal "Display" (caddr received)))
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
  :doc "automatic planning persists the plan without opening user approval first"
  (let* ((root (make-temp-file "mevedel-goal-auto-plan-" t))
         (session (mevedel-session-create
                   "main" (test-mevedel-goal--workspace root)))
         (goal (mevedel-goal--create
                :id "g1" :status 'active :phase 'planning :objective "x"
                :approval-policy 'automatic :cycle 1 :cycles '((:cycle 1))))
         presented)
    (unwind-protect
        (with-temp-buffer
          (setq-local mevedel--session session)
          (setf (mevedel-session-goal session) goal)
          (insert "<proposed_plan>\n# Guard me\n</proposed_plan>")
          (cl-letf (((symbol-function 'mevedel-goal-present-plan)
                     (lambda (&rest _) (setq presented t)))
                    ((symbol-function 'mevedel-goal--save-session-state)
                     #'ignore))
            (mevedel-goal--post-response (point-min) (point-max)))
          (should (eq 'awaiting-approval (mevedel-goal-phase goal)))
          (should-not presented)
          (should (equal "# Guard me" (mevedel-plan-current-body session))))
      (delete-directory root t)))
  :doc "planning without a required plan artifact pauses durably"
  (let* ((root (make-temp-file "mevedel-goal-missing-plan-" t))
         (session (mevedel-session-create
                   "main" (test-mevedel-goal--workspace root)))
         (goal (mevedel-goal--create
                :id "g1" :objective "Ship" :status 'active :phase 'planning
                :approval-policy 'automatic :cycle 1 :cycles '((:cycle 1)))))
    (unwind-protect
        (with-temp-buffer
          (setq-local mevedel--session session)
          (setf (mevedel-session-goal session) goal)
          (insert "I investigated the problem but omitted the artifact.")
          (mevedel-goal--post-response (point-min) (point-max))
          (should (eq 'paused (mevedel-goal-status goal)))
          (should (string-match-p "no proposed plan"
                                  (mevedel-goal-reason goal)))
          (let* ((sidecar
                  (mevedel-session-persistence-load-sidecar
                   (mevedel-session-persistence--sidecar-path
                    (mevedel-session-save-path session))))
                 (saved (plist-get sidecar :goal)))
            (should (eq 'paused (plist-get saved :status)))
            (should (string-match-p "no proposed plan"
                                    (plist-get saved :reason)))))
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
          (insert "<goal_review>\nverdict: complete\nsummary: Tests and diff satisfy the objective.\n</goal_review>")
          (mevedel-goal--post-response (point-min) (point-max))
          (should (eq 'active (mevedel-goal-status goal)))
          (should (equal 'complete
                         (plist-get (mevedel-goal-review-summary goal)
                                    :verdict)))
          (should (equal "Tests and diff satisfy the objective."
                         (plist-get (mevedel-goal-review-summary goal)
                                    :summary))))
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
                :id "goal-1" :cycle 1 :cycles '((:cycle 1))
                :status 'active :phase 'awaiting-approval :objective "x"))
         implementation)
    (unwind-protect
        (with-current-buffer buffer
          (setq-local mevedel--session session)
          (test-mevedel-goal--own goal session root)
          (setf (mevedel-session-goal session) goal
                (mevedel-session-permission-mode session) 'accept-edits)
          (cl-letf (((symbol-function 'mevedel-goal--save-session-state)
                     #'ignore)
                    ((symbol-function 'mevedel-goal--ensure-reference-reminder)
                     #'ignore)
                    ((symbol-function 'mevedel-goal--call-with-workload)
                     (lambda (workload fn &rest _)
                       (should (eq workload 'implementation))
                       (funcall fn)))
                    ((symbol-function 'mevedel--implement-plan)
                     (lambda (input)
                       (setq implementation input)
                       nil)))
            (mevedel-goal--approval-callback
             "# Plan\n\nImplement it." buffer '(:context full)))
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
          (test-mevedel-goal--own goal session root)
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
          (test-mevedel-goal--own goal session root)
          (setf (mevedel-session-goal session) goal)
          (cl-letf (((symbol-function 'mevedel-plan-accept)
                     (lambda (&rest _)
                       (setq wrote t))))
            (should-error
             (mevedel-goal--approval-callback
              "# Plan" buffer '(:context full))
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
            (mevedel-goal-present-plan
             "# Plan\n\nDo it." (current-buffer) "Need scope"))
          (should (file-exists-p
                   (mevedel-plan-current-path session (current-buffer))))
          (should (equal "# Plan\n\nDo it." (plist-get queued :body)))
          (should (equal "Need scope" (plist-get queued :guardian-reason))))
      (delete-directory root t))))

(mevedel-deftest mevedel-goal-restore-pending-approval ()
  ,test
  (test)
  :doc "requeues a persisted presented plan for an awaiting Goal"
  (let* ((session (mevedel-session--create
                   :name "main"
                   :goal (mevedel-goal--create
                          :status 'active :phase 'awaiting-approval)
                   :plan-metadata '(:status presented :guardian-pending t)))
         queued)
    (with-temp-buffer
      (setq-local mevedel--session session)
      (cl-letf (((symbol-function 'mevedel-plan-current-body)
                 (lambda (&optional _session) "# Pending"))
                ((symbol-function 'mevedel-plan-queue--enqueue)
                 (lambda (entry) (setq queued entry))))
        (mevedel-goal-restore-pending-approval session (current-buffer)))
      (should (equal "# Pending" (plist-get queued :body)))
      (should (string-match-p "interrupted"
                              (plist-get queued :guardian-reason)))
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
                :id "goal-complete" :cycle 1 :cycles '((:cycle 1))
                :review-summary
                '(:verdict complete :summary "All acceptance checks pass."))))
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
  (let* ((root (make-temp-file "mevedel-goal-review-dispatch-" t))
         (buffer (generate-new-buffer " *goal-review-dispatch*"))
         (session (mevedel-session-create
                   "main" (test-mevedel-goal--workspace root)))
         (goal (mevedel-goal--create
                :status 'active :phase 'reviewing :objective "Fix it"
                :cycle 1
                :current-plan '(:absolute-path "/tmp/plan.md")
                :checkpoint '(:phase implementing :attempt-id "i1"
                              :dispatch-state settled)))
         dispatched)
    (unwind-protect
        (with-current-buffer buffer
          (setq-local mevedel--session session)
          (test-mevedel-goal--own goal session root)
          (setf (mevedel-session-goal session) goal)
          (let ((mevedel-goal-dispatch-function
                 (lambda (&rest args) (setq dispatched args))))
            (mevedel-goal-dispatch-after-turn
             (test-mevedel-goal--fsm buffer 'implementing)))
          (should (eq 'reviewing (car dispatched)))
          (should (string-match-p "Fix it" (cadr dispatched))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (delete-directory root t)))
  :doc "runs the mandatory guardian after automatic planning settles"
  (with-temp-buffer
    (let* ((goal (mevedel-goal--create
                  :id "g1" :status 'active :phase 'awaiting-approval
                  :approval-policy 'automatic :cycle 1
                  :checkpoint '(:phase planning :attempt-id "p1"
                                :dispatch-state settled)))
           (root (make-temp-file "mevedel-goal-guardian-dispatch-" t))
           (session (mevedel-session-create
                     "main" (test-mevedel-goal--workspace root)))
           guarded)
      (unwind-protect
          (progn
            (setq-local mevedel--session session)
            (test-mevedel-goal--own goal session root)
            (setf (mevedel-session-goal session) goal)
            (cl-letf (((symbol-function 'mevedel-goal--guard-current-plan)
                       (lambda (selected buffer)
                         (setq guarded (list selected buffer)))))
              (mevedel-goal-dispatch-after-turn
               (test-mevedel-goal--fsm (current-buffer) 'planning)))
            (should (equal (list goal (current-buffer)) guarded)))
        (delete-directory root t)))))

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
                      ((symbol-function 'mevedel-model-resolve-workload)
                       (lambda (&rest _)
                         '(:backend nil :model test-model :effort nil)))
                      ((symbol-function 'mevedel--implement-plan)
                       (lambda (input)
                         (setq implementation input)
                         nil)))
              (mevedel-goal--approval-callback
               "# Plan\n\nFix and test." buffer '(:context full)))
            (should implementation)
            (mevedel-goal-settle-turn
             (test-mevedel-goal--fsm buffer 'implementing))
            (mevedel-goal-dispatch-after-turn
             (test-mevedel-goal--fsm buffer 'implementing))
            (erase-buffer)
            (insert "<goal_review>\nverdict: complete\nsummary: Review confirms the objective and tests pass.\n</goal_review>")
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

(mevedel-deftest mevedel-goal--approval-execution-home ()
  ,test
  (test)
  :doc "uses an explicit supervised choice or the captured Goal home"
  (let ((goal (mevedel-goal--create
               :execution-home '(:kind current :directory "/tmp/"
                                 :session-id "s1"))))
    (should (eq 'current
                (mevedel-goal--approval-execution-home
                 goal '(:context full))))
    (should (eq 'worktree
                (mevedel-goal--approval-execution-home
                 goal '(:context full :execution-home worktree))))
    (setf (plist-get (mevedel-goal-execution-home goal) :locked) t)
    (should-error
     (mevedel-goal--approval-execution-home
      goal '(:context full :execution-home worktree))
     :type 'user-error)))

(mevedel-deftest mevedel-goal--approval-context ()
  ,test
  (test)
  :doc "uses the Goal default and honors explicit approval selection"
  (let ((goal (mevedel-goal--create :implementation-context 'focused)))
    (should (eq 'focused
                (mevedel-goal--approval-context goal nil)))
    (should (eq 'focused
                (mevedel-goal--approval-context
                 goal '(:context focused))))
    (should (eq 'full
                (mevedel-goal--approval-context
                 goal '(:context full))))))

(mevedel-deftest mevedel-goal--approval-outcome-p ()
  ,test
  (test)
  :doc "recognizes only explicit full or focused implementation outcomes"
  (should (mevedel-goal--approval-outcome-p '(:context full)))
  (should (mevedel-goal--approval-outcome-p '(:context focused)))
  (should-not (mevedel-goal--approval-outcome-p 'implement))
  (should-not (mevedel-goal--approval-outcome-p 'aborted)))

(mevedel-deftest mevedel-goal--accept-plan ()
  ,test
  (test)
  :doc "persists the accepted artifact and Goal cycle references together"
  (let* ((root (make-temp-file "mevedel-goal-accept-plan-" t))
         (session (mevedel-session-create
                   "main" (test-mevedel-goal--workspace root) root))
         (goal (mevedel-goal--create
                :id "g1" :status 'active :phase 'awaiting-approval
                :cycle 1 :cycles '((:cycle 1))))
         accepted)
    (unwind-protect
        (with-temp-buffer
          (setq-local mevedel--session session)
          (test-mevedel-goal--own goal session root)
          (setf (mevedel-session-goal session) goal)
          (setq accepted
                (mevedel-goal--accept-plan "# Plan\n\nDo it." session
                                           (current-buffer)))
          (should (eq 'implementing (mevedel-goal-phase goal)))
          (should (equal accepted (mevedel-goal-current-plan goal)))
          (should (file-exists-p (plist-get accepted :absolute-path)))
          (should (equal (plist-get accepted :path)
                         (plist-get (mevedel-goal-cycle-record goal) :plan))))
      (delete-directory root t))))

(mevedel-deftest mevedel-plan-queue--entry-execution-home ()
  ,test
  (test)
  :doc "reads the Goal default until the approval entry overrides it"
  (let* ((goal (mevedel-goal--create
                :execution-home '(:kind current :directory "/tmp/"
                                  :session-id "s1")))
         (session (mevedel-session--create :name "main" :goal goal))
         (entry (list :session session)))
    (should (eq 'current
                (mevedel-plan-queue--entry-execution-home entry)))
    (mevedel-queue--entry-metadata-put entry :execution-home 'worktree)
    (should (eq 'worktree
                (mevedel-plan-queue--entry-execution-home entry)))))

(mevedel-deftest mevedel-plan-queue--entry-execution-home-mutable-p ()
  ,test
  (test)
  :doc "allows home selection only before the first accepted plan"
  (let* ((goal (mevedel-goal--create))
         (session (mevedel-session--create :name "main" :goal goal))
         (entry (list :session session)))
    (should (mevedel-plan-queue--entry-execution-home-mutable-p entry))
    (setf (plist-get (mevedel-goal-execution-home goal) :locked) t)
    (should-not (mevedel-plan-queue--entry-execution-home-mutable-p entry))))

(mevedel-deftest mevedel-plan-queue--cycle-entry-execution-home ()
  ,test
  (test)
  :doc "toggles current and worktree while rerendering the same entry"
  (let* ((goal (mevedel-goal--create))
         (session (mevedel-session--create :name "main" :goal goal))
         (entry (list :session session))
         rendered)
    (cl-letf (((symbol-function 'mevedel-plan-queue--render-entry)
               (lambda (candidate) (setq rendered candidate))))
      (mevedel-plan-queue--cycle-entry-execution-home entry)
      (should (eq 'worktree
                  (mevedel-plan-queue--entry-execution-home entry)))
      (should (eq entry rendered))
      (mevedel-plan-queue--cycle-entry-execution-home entry)
      (should (eq 'current
                  (mevedel-plan-queue--entry-execution-home entry)))
      (setf (plist-get (mevedel-goal-execution-home goal) :locked) t)
      (should-error
       (mevedel-plan-queue--cycle-entry-execution-home entry)
       :type 'user-error))))

(mevedel-deftest mevedel-plan-queue--keys-line ()
  ,test
  (test)
  :doc "shows full/focused context, permission mode, and execution home"
  (let ((text (substring-no-properties
               (mevedel-plan-queue--keys-line
                'accept-edits 'worktree t))))
    (dolist (needle '("implement (full)" "implement (focused)"
                      "mode: edit" "home: worktree"))
      (should (string-match-p (regexp-quote needle) text)))
    (let ((locked (substring-no-properties
                   (mevedel-plan-queue--keys-line
                    'default 'worktree nil))))
      (should (string-match-p "home: worktree (locked)" locked))
      (should-not (string-match-p "w home" locked)))))

(mevedel-deftest mevedel-goal--transfer-to-worktree ()
  ,test
  (test)
  :doc "transfers one durable owner into a real worktree and clear keeps it"
  (let* ((root (file-name-as-directory
                (make-temp-file "mevedel-goal-worktree-" t)))
         (source-buffer (generate-new-buffer " *goal-worktree-source*"))
         (target-buffer (generate-new-buffer " *goal-worktree-target*"))
         (workspace (test-mevedel-goal--workspace root))
         (source-session (mevedel-session-create "main" workspace root))
         target-session target-goal target-directory dispatched)
    (unwind-protect
        (progn
          (test-mevedel-goal--init-git root)
          (with-current-buffer source-buffer
            (setq-local mevedel--session source-session
                        default-directory root)
            (let ((goal (mevedel-goal--create
                         :id "g1" :objective "Ship" :status 'active
                         :phase 'awaiting-approval
                         :approval-policy 'supervised
                         :cycle 2 :cycles '((:cycle 1) (:cycle 2))
                         :token-budget 1000 :token-usage 125
                         :checkpoint '(:phase planning
                                       :dispatch-state settled))))
              (test-mevedel-goal--own goal source-session root)
              (setf (plist-get (mevedel-goal-execution-home goal) :kind)
                    'worktree
                    (mevedel-goal-implementation-context goal) 'full
                    (mevedel-session-goal source-session) goal)
              (cl-letf (((symbol-function 'mevedel-worktree--open-session)
                         (lambda (target-workspace directory)
                           (setq target-directory directory
                                 target-session
                                 (mevedel-session-create
                                  "goal-g1" target-workspace directory))
                           (with-current-buffer target-buffer
                             (setq-local mevedel--session target-session
                                         default-directory directory))
                           target-buffer)))
                (should
                 (eq target-buffer
                     (plist-get
                      (mevedel-goal--transfer-to-worktree
                       goal source-buffer
                       "# Plan\n\nImplement both slices.")
                      :buffer))))))
          (setq target-goal (mevedel-session-goal target-session))
          (should (file-directory-p target-directory))
          (should (file-exists-p
                   (file-name-concat target-directory ".git")))
          (should-not (mevedel-session-goal source-session))
          (should (equal "g1" (plist-get
                                (mevedel-session-goal-handoff source-session)
                                :goal-id)))
          (should (equal (mevedel-session-session-id target-session)
                         (mevedel-goal-owner-session target-goal)))
          (should (eq 'full
                      (mevedel-goal-implementation-context target-goal)))
          (should (file-exists-p
                   (plist-get (mevedel-goal-current-plan target-goal)
                              :absolute-path)))
          (should (= 125 (mevedel-goal-token-usage target-goal)))
          (should (= 2 (length (mevedel-goal-cycles target-goal))))
          (with-current-buffer target-buffer
            (setf (mevedel-goal-status target-goal) 'paused
                  (mevedel-goal-phase target-goal) 'planning
                  (mevedel-goal-checkpoint target-goal) nil)
            (let ((mevedel-goal-dispatch-function
                   (lambda (phase &rest _) (setq dispatched phase))))
              (mevedel-goal-resume))
            (should (eq 'planning dispatched))
            (setf (mevedel-goal-status target-goal) 'paused)
            (mevedel-goal-clear))
          (should (file-directory-p target-directory))
          (should-not (mevedel-session-goal target-session))
          (let ((source-sidecar
                 (mevedel-session-persistence-load-sidecar
                  (mevedel-session-persistence--sidecar-path
                   (mevedel-session-save-path source-session))))
                (target-sidecar
                 (mevedel-session-persistence-load-sidecar
                  (mevedel-session-persistence--sidecar-path
                   (mevedel-session-save-path target-session)))))
            (should-not (plist-get source-sidecar :goal))
            (should (plist-get source-sidecar :goal-handoff))
            (should-not (plist-get target-sidecar :goal))))
      (when (buffer-live-p source-buffer) (kill-buffer source-buffer))
      (when (buffer-live-p target-buffer) (kill-buffer target-buffer))
      (delete-directory root t)))
  :doc "a forceful stop after target persistence leaves one resumable owner"
  (let* ((root (file-name-as-directory
                (make-temp-file "mevedel-goal-transfer-crash-" t)))
         (source-buffer (generate-new-buffer " *goal-transfer-crash-source*"))
         (target-buffer (generate-new-buffer " *goal-transfer-crash-target*"))
         (workspace (test-mevedel-goal--workspace root))
         (source-session (mevedel-session-create "main" workspace root))
         target-session target-directory)
    (unwind-protect
        (progn
          (test-mevedel-goal--init-git root)
          (with-current-buffer source-buffer
            (setq-local mevedel--session source-session
                        default-directory root)
            (let* ((goal (mevedel-goal--create
                          :id "g-crash" :objective "Ship" :status 'active
                          :phase 'awaiting-approval
                          :approval-policy 'supervised :cycle 1
                          :cycles '((:cycle 1))
                          :implementation-context 'full))
                   (original-persist
                    (symbol-function 'mevedel-goal--persist-checkpoint))
                   (source-writes 0))
              (test-mevedel-goal--own goal source-session root)
              (setf (plist-get (mevedel-goal-execution-home goal) :kind)
                    'worktree
                    (mevedel-goal-implementation-context goal) 'full
                    (mevedel-session-goal source-session) goal)
              (cl-letf (((symbol-function 'mevedel-worktree--open-session)
                         (lambda (target-workspace directory)
                           (setq target-directory directory
                                 target-session
                                 (mevedel-session-create
                                  "goal-g-crash" target-workspace directory))
                           (with-current-buffer target-buffer
                             (setq-local mevedel--session target-session
                                         default-directory directory))
                           target-buffer))
                        ((symbol-function 'mevedel-goal--persist-checkpoint)
                         (lambda (session buffer)
                           (when (eq session source-session)
                             (cl-incf source-writes)
                             (when (= source-writes 2)
                               (throw 'force-stop :stopped)))
                           (funcall original-persist session buffer))))
                (should
                 (eq :stopped
                     (catch 'force-stop
                       (mevedel-goal--transfer-to-worktree
                        goal source-buffer "# Plan\n\nShip safely.")
                       nil))))))
          (mevedel-workspace-clear-registry)
          (let* ((source-sidecar
                  (mevedel-session-persistence-load-sidecar
                   (mevedel-session-persistence--sidecar-path
                    (mevedel-session-save-path source-session))))
                 (target-sidecar
                  (mevedel-session-persistence-load-sidecar
                   (mevedel-session-persistence--sidecar-path
                    (mevedel-session-save-path target-session))))
                 (restored-target
                  (plist-get
                   (mevedel-session-persistence-deserialize target-sidecar)
                   :session))
                 (restored-source
                  (plist-get
                   (mevedel-session-persistence-deserialize source-sidecar)
                   :session))
                 (restored-goal (mevedel-session-goal restored-target))
                 dispatched)
            (should (eq 'prepared
                        (plist-get (plist-get source-sidecar :goal-handoff)
                                   :state)))
            (should-not
             (mevedel-goal-owned-by-session-p
              (mevedel-session-goal restored-source) restored-source))
            (setf (mevedel-goal-status (mevedel-session-goal restored-source))
                  'paused)
            (with-temp-buffer
              (setq-local mevedel--session restored-source
                          default-directory root)
              (let ((mevedel-goal-dispatch-function
                     (lambda (&rest _) (setq dispatched t))))
                (should-error (mevedel-goal-resume) :type 'user-error)))
            (should-not dispatched)
            (should (eq 'implementing (mevedel-goal-phase restored-goal)))
            (should (file-exists-p
                     (plist-get (mevedel-goal-current-plan restored-goal)
                                :absolute-path)))
            (should (mevedel-goal-owned-by-session-p
                     restored-goal restored-target))))
      (when (buffer-live-p source-buffer) (kill-buffer source-buffer))
      (when (buffer-live-p target-buffer) (kill-buffer target-buffer))
      (delete-directory root t)))
  :doc "a target persistence failure rolls back cleanly and permits retry"
  (let* ((root (file-name-as-directory
                (make-temp-file "mevedel-goal-transfer-retry-" t)))
         (source-buffer (generate-new-buffer " *goal-transfer-retry-source*"))
         (workspace (test-mevedel-goal--workspace root))
         (source-session (mevedel-session-create "main" workspace root))
         (original-persist
          (symbol-function 'mevedel-goal--persist-checkpoint))
         (fail-target-save t)
         target-buffers target-directory)
    (unwind-protect
        (progn
          (test-mevedel-goal--init-git root)
          (with-current-buffer source-buffer
            (setq-local mevedel--session source-session
                        default-directory root)
            (let ((goal (mevedel-goal--create
                         :id "g-retry" :objective "Ship" :status 'active
                         :phase 'awaiting-approval
                         :approval-policy 'supervised :cycle 1
                         :cycles '((:cycle 1))
                         :implementation-context 'full)))
              (test-mevedel-goal--own goal source-session root)
              (setf (plist-get (mevedel-goal-execution-home goal) :kind)
                    'worktree
                    (plist-get (mevedel-goal-execution-home goal) :locked) t
                    (mevedel-session-goal source-session) goal)
              (cl-letf (((symbol-function 'mevedel-worktree--open-session)
                         (lambda (target-workspace directory)
                           (let* ((buffer (generate-new-buffer
                                           " *goal-transfer-retry-target*"))
                                  (session (mevedel-session-create
                                            "goal-g-retry"
                                            target-workspace directory)))
                             (setq target-directory directory)
                             (push buffer target-buffers)
                             (with-current-buffer buffer
                               (setq-local mevedel--session session
                                           default-directory directory))
                             buffer)))
                        ((symbol-function 'mevedel-goal--persist-checkpoint)
                         (lambda (session buffer)
                           (if (and fail-target-save
                                    (not (eq session source-session)))
                               (progn
                                 (setq fail-target-save nil)
                                 (error "Injected target save failure"))
                             (funcall original-persist session buffer)))))
                (should-error
                 (mevedel-goal--transfer-to-worktree
                  goal source-buffer "# Plan\n\nShip safely."))
                (should-not (file-exists-p target-directory))
                (should-not (mevedel-session-goal-handoff source-session))
                (should (mevedel-goal-owned-by-session-p
                         goal source-session))
                (let* ((result
                        (mevedel-goal--transfer-to-worktree
                         goal source-buffer "# Plan\n\nShip safely."))
                       (target-session
                        (buffer-local-value
                         'mevedel--session (plist-get result :buffer))))
                  (should (file-directory-p target-directory))
                  (should (mevedel-goal-current-plan
                           (mevedel-session-goal target-session))))))))
      (when (buffer-live-p source-buffer) (kill-buffer source-buffer))
      (dolist (buffer target-buffers)
        (when (buffer-live-p buffer) (kill-buffer buffer)))
      (delete-directory root t)))
  :doc "rollback-save failure preserves the durable target and source gate"
  (let* ((root (file-name-as-directory
                (make-temp-file "mevedel-goal-transfer-dual-" t)))
         (source-buffer (generate-new-buffer " *goal-transfer-dual-source*"))
         (target-buffer (generate-new-buffer " *goal-transfer-dual-target*"))
         (workspace (test-mevedel-goal--workspace root))
         (source-session (mevedel-session-create "main" workspace root))
         (original-persist
          (symbol-function 'mevedel-goal--persist-checkpoint))
         (source-writes 0)
         target-session target-directory)
    (unwind-protect
        (progn
          (test-mevedel-goal--init-git root)
          (with-current-buffer source-buffer
            (setq-local mevedel--session source-session
                        default-directory root)
            (let ((goal (mevedel-goal--create
                         :id "g-dual" :objective "Ship" :status 'active
                         :phase 'awaiting-approval
                         :approval-policy 'supervised :cycle 1
                         :cycles '((:cycle 1))
                         :implementation-context 'full)))
              (test-mevedel-goal--own goal source-session root)
              (setf (plist-get (mevedel-goal-execution-home goal) :kind)
                    'worktree
                    (plist-get (mevedel-goal-execution-home goal) :locked) t
                    (mevedel-session-goal source-session) goal)
              (cl-letf (((symbol-function 'mevedel-worktree--open-session)
                         (lambda (target-workspace directory)
                           (setq target-directory directory
                                 target-session
                                 (mevedel-session-create
                                  "goal-g-dual" target-workspace directory))
                           (with-current-buffer target-buffer
                             (setq-local mevedel--session target-session
                                         default-directory directory))
                           target-buffer))
                        ((symbol-function 'mevedel-goal--persist-checkpoint)
                         (lambda (session buffer)
                           (when (eq session source-session)
                             (cl-incf source-writes)
                             (when (memq source-writes '(2 3))
                               (error "Injected source save failure")))
                           (funcall original-persist session buffer))))
                (should-error
                 (mevedel-goal--transfer-to-worktree
                  goal source-buffer "# Plan\n\nShip safely.")))))
          (let ((source-sidecar
                 (mevedel-session-persistence-load-sidecar
                  (mevedel-session-persistence--sidecar-path
                   (mevedel-session-save-path source-session))))
                (target-sidecar
                 (mevedel-session-persistence-load-sidecar
                  (mevedel-session-persistence--sidecar-path
                   (mevedel-session-save-path target-session)))))
            (should (file-directory-p target-directory))
            (should (eq 'prepared
                        (plist-get (plist-get source-sidecar :goal-handoff)
                                   :state)))
            (should (plist-get target-sidecar :goal))
            (should (mevedel-session-goal target-session))
            (should-not (mevedel-goal-owned-by-session-p
                         (mevedel-session-goal source-session)
                         source-session))))
      (when (buffer-live-p source-buffer) (kill-buffer source-buffer))
      (when (buffer-live-p target-buffer) (kill-buffer target-buffer))
      (delete-directory root t)))
  :doc "target-clear failure re-gates source before restoring target ownership"
  (let* ((root (file-name-as-directory
                (make-temp-file "mevedel-goal-transfer-regate-" t)))
         (source-buffer (generate-new-buffer " *goal-transfer-regate-source*"))
         (target-buffer (generate-new-buffer " *goal-transfer-regate-target*"))
         (workspace (test-mevedel-goal--workspace root))
         (source-session (mevedel-session-create "main" workspace root))
         (original-persist
          (symbol-function 'mevedel-goal--persist-checkpoint))
         (source-writes 0)
         (target-writes 0)
         target-session target-directory)
    (unwind-protect
        (progn
          (test-mevedel-goal--init-git root)
          (with-current-buffer source-buffer
            (setq-local mevedel--session source-session
                        default-directory root)
            (let ((goal (mevedel-goal--create
                         :id "g-regate" :objective "Ship" :status 'active
                         :phase 'awaiting-approval
                         :approval-policy 'supervised :cycle 1
                         :cycles '((:cycle 1))
                         :implementation-context 'full)))
              (test-mevedel-goal--own goal source-session root)
              (setf (plist-get (mevedel-goal-execution-home goal) :kind)
                    'worktree
                    (plist-get (mevedel-goal-execution-home goal) :locked) t
                    (mevedel-session-goal source-session) goal)
              (cl-letf (((symbol-function 'mevedel-worktree--open-session)
                         (lambda (target-workspace directory)
                           (setq target-directory directory
                                 target-session
                                 (mevedel-session-create
                                  "goal-g-regate" target-workspace directory))
                           (with-current-buffer target-buffer
                             (setq-local mevedel--session target-session
                                         default-directory directory))
                           target-buffer))
                        ((symbol-function 'mevedel-goal--persist-checkpoint)
                         (lambda (session buffer)
                           (if (eq session source-session)
                               (progn
                                 (cl-incf source-writes)
                                 (when (= source-writes 2)
                                   (error "Injected final source save failure")))
                             (cl-incf target-writes)
                             (when (= target-writes 2)
                               (error "Injected target clear failure")))
                           (funcall original-persist session buffer))))
                (should-error
                 (mevedel-goal--transfer-to-worktree
                  goal source-buffer "# Plan\n\nShip safely."))))))
          (mevedel-workspace-clear-registry)
          (let* ((source-sidecar
                  (mevedel-session-persistence-load-sidecar
                   (mevedel-session-persistence--sidecar-path
                    (mevedel-session-save-path source-session))))
                 (target-sidecar
                  (mevedel-session-persistence-load-sidecar
                   (mevedel-session-persistence--sidecar-path
                    (mevedel-session-save-path target-session))))
                 (restored-source
                  (plist-get
                   (mevedel-session-persistence-deserialize source-sidecar)
                   :session))
                 (restored-target
                  (plist-get
                   (mevedel-session-persistence-deserialize target-sidecar)
                   :session))
                 (source-goal (mevedel-session-goal restored-source))
                 (target-goal (mevedel-session-goal restored-target)))
            (should (eq 'prepared
                        (plist-get (mevedel-session-goal-handoff
                                    restored-source)
                                   :state)))
            (should-not (mevedel-goal-owned-by-session-p
                         source-goal restored-source))
            (should (mevedel-goal-owned-by-session-p
                     target-goal restored-target))
            (should (= 1 (length
                          (delq nil
                                (list
                                 (mevedel-goal-owned-by-session-p
                                  source-goal restored-source)
                                 (mevedel-goal-owned-by-session-p
                                  target-goal restored-target)))))))
      (when (buffer-live-p source-buffer) (kill-buffer source-buffer))
      (when (buffer-live-p target-buffer) (kill-buffer target-buffer))
      (delete-directory root t))))

(mevedel-deftest mevedel-goal-multi-cycle ()
  ,test
  (test)
  :doc "continues through a second immutable plan before review completes"
  (let* ((root (make-temp-file "mevedel-goal-multi-" t))
         (buffer (generate-new-buffer " *goal-multi*"))
         (session (mevedel-session-create
                   "main" (test-mevedel-goal--workspace root)))
         dispatched)
    (unwind-protect
        (with-current-buffer buffer
          (setq-local mevedel--session session)
          (let ((mevedel-goal-dispatch-function
                 (lambda (phase prompt display)
                   (push (list phase prompt display) dispatched))))
            (mevedel-goal-start "Finish both slices")
            (cl-letf (((symbol-function 'mevedel-goal--save-session-state)
                       #'ignore)
                      ((symbol-function 'mevedel-goal--ensure-reference-reminder)
                       #'ignore)
                      ((symbol-function 'mevedel-model-resolve-workload)
                       (lambda (&rest _)
                         '(:backend nil :model test-model :effort nil)))
                      ((symbol-function 'mevedel--implement-plan)
                       (lambda (_input) nil)))
              (let ((goal (mevedel-session-goal session)))
                (setf (mevedel-goal-phase goal) 'awaiting-approval)
                (mevedel-goal--approval-callback
                 "# Cycle one\n\nImplement slice one."
                 buffer '(:context full))
                (mevedel-goal-settle-turn
                 (test-mevedel-goal--fsm buffer 'implementing))
                (mevedel-goal-dispatch-after-turn
                 (test-mevedel-goal--fsm buffer 'implementing))
                (erase-buffer)
                (insert "<goal_review>\nverdict: continue\nsummary: Implement slice two.\n</goal_review>")
                (mevedel-goal--post-response (point-min) (point-max))
                (mevedel-goal-settle-turn
                 (test-mevedel-goal--fsm buffer 'reviewing))
                (mevedel-goal-dispatch-after-turn
                 (test-mevedel-goal--fsm buffer 'reviewing))
                (should (= 2 (mevedel-goal-cycle goal)))
                (should (string-match-p "Implement slice two"
                                        (cadr (car dispatched))))
                (should-error
                 (mevedel-goal--approval-execution-home
                  goal '(:context full :execution-home worktree))
                 :type 'user-error)
                (setf (mevedel-goal-phase goal) 'awaiting-approval)
                (mevedel-goal--approval-callback
                 "# Cycle two\n\nImplement slice two."
                 buffer '(:context full))
                (mevedel-goal-settle-turn
                 (test-mevedel-goal--fsm buffer 'implementing))
                (mevedel-goal-dispatch-after-turn
                 (test-mevedel-goal--fsm buffer 'implementing))
                (erase-buffer)
                (insert "<goal_review>\nverdict: complete\nsummary: Both slices and tests pass.\n</goal_review>")
                (mevedel-goal--post-response (point-min) (point-max))
                (mevedel-goal-settle-turn
                 (test-mevedel-goal--fsm buffer 'reviewing))
                (should (eq 'complete (mevedel-goal-status goal)))
                (let* ((save-path (mevedel-session-save-path session))
                       (goal-dir (file-name-concat save-path "goals"
                                                   (mevedel-goal-id goal))))
                  (should (file-exists-p
                           (file-name-concat goal-dir
                                             "cycle-001-plan.md")))
                  (should (file-exists-p
                           (file-name-concat goal-dir
                                             "cycle-002-plan.md")))
                  (with-temp-buffer
                    (insert-file-contents
                     (file-name-concat goal-dir "cycles.el"))
                    (let ((index (read (current-buffer))))
                      (should (= 2 (length index)))
                      (should-not (string-match-p
                                   "Implement slice"
                                   (prin1-to-string index))))))))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (delete-directory root t)))
  :doc "blocked review settles with its concrete reason and no continuation"
  (let* ((root (make-temp-file "mevedel-goal-blocked-" t))
         (buffer (generate-new-buffer " *goal-blocked*"))
         (session (mevedel-session-create
                   "main" (test-mevedel-goal--workspace root)))
         (goal (mevedel-goal--create
                :id "blocked" :objective "Ship" :status 'active
                :phase 'reviewing :cycle 1 :cycles '((:cycle 1))
                :review-summary
                '(:verdict blocked :summary "Need an API credential."))))
    (unwind-protect
        (with-current-buffer buffer
          (setq-local mevedel--session session)
          (setf (mevedel-session-goal session) goal)
          (mevedel-goal-settle-turn
           (test-mevedel-goal--fsm buffer 'reviewing))
          (should (eq 'blocked (mevedel-goal-status goal)))
          (should (equal "Need an API credential."
                         (mevedel-goal-reason goal))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (delete-directory root t))))

(mevedel-deftest mevedel-goal-automatic-multi-cycle ()
  ,test
  (test)
  :doc "runs two guardian-approved cycles without manual prompting"
  (let* ((root (make-temp-file "mevedel-goal-auto-multi-" t))
         (buffer (generate-new-buffer " *goal-auto-multi*"))
         (session (mevedel-session-create
                   "main" (test-mevedel-goal--workspace root)))
         (dispatches nil)
         (implementations 0))
    (unwind-protect
        (with-current-buffer buffer
          (setq-local mevedel--session session)
          (let ((mevedel-goal-dispatch-function
                 (lambda (phase _prompt _display)
                   (push phase dispatches)))
                (mevedel-goal-guardian-function
                 (lambda (goal _plan _buffer callback)
                   (setf (mevedel-goal-checkpoint goal)
                         (list :phase 'guardian
                               :attempt-id
                               (format "guardian-%d" (mevedel-goal-cycle goal))
                               :dispatch-state 'started))
                   (funcall callback
                            '(:verdict approve :reason "Safe"
                              :provider "P:M" :effort high)))))
            (cl-letf (((symbol-function 'mevedel-model-resolve-workload)
                       (lambda (&rest _)
                         '(:backend nil :model test-model :effort nil)))
                      ((symbol-function 'mevedel--implement-plan)
                       (lambda (_input) (cl-incf implementations) nil))
                      ((symbol-function 'run-at-time)
                       (lambda (_seconds _repeat function &rest args)
                         (apply function args)
                         nil)))
              (mevedel-goal-start "Finish both slices" nil 'automatic)
              (dolist (cycle '(("# Plan one" continue
                                "Slice one done; implement slice two.")
                               ("# Plan two" complete
                                "Both slices and tests pass.")))
                (erase-buffer)
                (insert "<proposed_plan>\n" (nth 0 cycle)
                        "\n</proposed_plan>")
                (mevedel-goal--post-response (point-min) (point-max))
                (let ((planning-fsm
                       (test-mevedel-goal--fsm buffer 'planning)))
                  (mevedel-goal-settle-turn planning-fsm)
                  (mevedel-goal-dispatch-after-turn planning-fsm))
                (let ((implementation-fsm
                       (test-mevedel-goal--fsm buffer 'implementing)))
                  (mevedel-goal-settle-turn implementation-fsm)
                  (mevedel-goal-dispatch-after-turn implementation-fsm))
                (erase-buffer)
                (insert (format
                         "<goal_review>\nverdict: %s\nsummary: %s\n</goal_review>"
                         (nth 1 cycle) (nth 2 cycle)))
                (mevedel-goal--post-response (point-min) (point-max))
                (let ((review-fsm
                       (test-mevedel-goal--fsm buffer 'reviewing)))
                  (mevedel-goal-settle-turn review-fsm)
                  (mevedel-goal-dispatch-after-turn review-fsm)))
              (let ((goal (mevedel-session-goal session)))
                (should (eq 'complete (mevedel-goal-status goal)))
                (should (= 2 (mevedel-goal-cycle goal)))
                (should (= 2 implementations))
                (should (= 2
                           (cl-count 'planning dispatches)))))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (delete-directory root t))))

(mevedel-deftest mevedel-goal-duplicate-plan ()
  ,test
  (test)
  :doc "pauses instead of presenting the previous accepted plan again"
  (let* ((plan "# Same\n\nRepeat.")
         (session (mevedel-session--create :name "main"))
         (goal (mevedel-goal--create
                :id "duplicate" :objective "Finish" :status 'active
                :phase 'planning :cycle 2
                :cycles
                (list (list :cycle 1 :plan-hash (mevedel-plan-hash plan))
                      '(:cycle 2))))
         presented)
    (with-temp-buffer
      (setq-local mevedel--session session)
      (setf (mevedel-session-goal session) goal)
      (insert "<proposed_plan>\n" plan "\n</proposed_plan>")
      (cl-letf (((symbol-function 'mevedel-goal-present-plan)
                 (lambda (&rest _) (setq presented t)))
                ((symbol-function 'mevedel-goal--save-session-state)
                 #'ignore))
        (mevedel-goal--post-response (point-min) (point-max)))
      (should (eq 'paused (mevedel-goal-status goal)))
      (should-not presented))))

(mevedel-deftest mevedel-goal--guardian-prompt ()
  ,test
  (test)
  :doc "supplies the objective, state, plan, safety criteria, and strict contract"
  (let ((text (mevedel-goal--guardian-prompt
               (mevedel-goal--create
                :objective "Ship safely" :status 'active
                :phase 'awaiting-approval :cycle 2
                :approval-policy 'automatic)
               "# Plan\nRun tests.")))
    (dolist (needle '("Ship safely" "Cycle: 2" "# Plan"
                      "dangerous or irreversible" "verdict: approve|ask"))
      (should (string-match-p (regexp-quote needle) text)))))

(mevedel-deftest mevedel-goal--parse-guardian ()
  ,test
  (test)
  :doc "accepts the exact approve-or-ask contract"
  (should (equal '(:verdict approve :reason "Scoped and verified.")
                 (mevedel-goal--parse-guardian
                  "<goal_guardian>\nverdict: approve\nreason: Scoped and verified.\n</goal_guardian>")))
  :doc "rejects malformed, empty-reason, and extra-prose output"
  (dolist (text '("approve"
                  "<goal_guardian>\nverdict: approve\nreason: \n</goal_guardian>"
                  "Maybe\n<goal_guardian>\nverdict: ask\nreason: unclear\n</goal_guardian>"))
    (should-not (mevedel-goal--parse-guardian text))))

(mevedel-deftest mevedel-goal--normalize-guardian-decision ()
  ,test
  (test)
  :doc "preserves valid decisions and converts every malformed value to ask"
  (let ((valid '(:verdict approve :reason "Safe" :provider "P:M")))
    (should (eq valid (mevedel-goal--normalize-guardian-decision valid))))
  (dolist (value '(nil (:verdict approve) (:verdict maybe :reason "x")
                       (:verdict ask :reason "")))
    (let ((decision (mevedel-goal--normalize-guardian-decision value)))
      (should (eq 'ask (plist-get decision :verdict)))
      (should (string-match-p "malformed" (plist-get decision :reason))))))

(mevedel-deftest mevedel-goal--guardian-provider-label ()
  ,test
  (test)
  :doc "formats model-only policies without inventing a backend"
  (should (equal "guardian-model"
                 (mevedel-goal--guardian-provider-label
                  '(:backend nil :model guardian-model)))))

(mevedel-deftest mevedel-goal--guardian-request ()
  ,test
  (test)
  :doc "uses the goal-guardian workload with no tools or conversational transforms"
  (with-temp-buffer
    (let ((goal (mevedel-goal--create
                 :objective "Ship" :status 'active :phase 'awaiting-approval
                 :approval-policy 'automatic :cycle 1))
          decision resolved recorded)
      (cl-letf (((symbol-function 'mevedel-model-resolve-workload)
                 (lambda (workload &rest _)
                   (setq resolved workload)
                   '(:backend nil :model guardian-model :effort high)))
                ((symbol-function 'mevedel-goal--record-phase-policy)
                 (lambda (workload _policy) (setq recorded workload)))
                ((symbol-function 'gptel-request)
                 (lambda (_prompt &rest args)
                   (should-not gptel-use-tools)
                   (should-not gptel-tools)
                   (should-not gptel-use-context)
                   (should-not (plist-get args :transforms))
                   (funcall (plist-get args :callback)
                            '(reasoning . "Safety analysis") nil)
                   (should-not decision)
                   (funcall
                    (plist-get args :callback)
                    "<goal_guardian>\nverdict: approve\nreason: Safe.\n</goal_guardian>"
                    nil))))
        (mevedel-goal--guardian-request
         goal "# Plan" (current-buffer)
         (lambda (value) (setq decision value))))
      (should (eq 'goal-guardian resolved))
      (should (eq 'goal-guardian recorded))
      (should (eq 'approve (plist-get decision :verdict)))
      (should (equal "guardian-model" (plist-get decision :provider)))
      (should (eq 'high (plist-get decision :effort)))))
  :doc "fails closed when request startup errors"
  (with-temp-buffer
    (let ((goal (mevedel-goal--create
                 :objective "Ship" :status 'active :phase 'awaiting-approval
                 :approval-policy 'automatic :cycle 1))
          decision)
      (cl-letf (((symbol-function 'mevedel-model-resolve-workload)
                 (lambda (&rest _) (error "No credits"))))
        (mevedel-goal--guardian-request
         goal "# Plan" (current-buffer)
         (lambda (value) (setq decision value))))
      (should (eq 'ask (plist-get decision :verdict)))
      (should (string-match-p "No credits" (plist-get decision :reason)))))
  :doc "times out to ask when the transport never settles"
  (with-temp-buffer
    (let ((goal (mevedel-goal--create
                 :objective "Ship" :status 'active :phase 'awaiting-approval
                 :approval-policy 'automatic :cycle 1))
          timer-callback decision)
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (_seconds _repeat callback)
                   (setq timer-callback callback)
                   'timer))
                ((symbol-function 'cancel-timer) #'ignore)
                ((symbol-function 'timerp) (lambda (_) t))
                ((symbol-function 'mevedel-model-resolve-workload)
                 (lambda (&rest _)
                   '(:backend nil :model guardian-model :effort nil)))
                ((symbol-function 'mevedel-goal--record-phase-policy) #'ignore)
                ((symbol-function 'gptel-request) #'ignore))
        (mevedel-goal--guardian-request
         goal "# Plan" (current-buffer)
         (lambda (value) (setq decision value)))
        (should-not decision)
        (funcall timer-callback))
      (should (eq 'ask (plist-get decision :verdict)))
      (should (string-match-p "timed out" (plist-get decision :reason))))))

(mevedel-deftest mevedel-goal--append-guardian-audit ()
  ,test
  (test)
  :doc "writes a hidden model-ignored audit and requests a visible rerender"
  (let ((chat (generate-new-buffer " *goal-guardian-audit*"))
        (view (generate-new-buffer " *goal-guardian-view*"))
        rerendered)
    (unwind-protect
        (with-current-buffer chat
          (setq-local mevedel--view-buffer view)
          (cl-letf (((symbol-function 'mevedel-view-rerender)
                     (lambda (buffer) (setq rerendered buffer))))
            (mevedel-goal--append-guardian-audit
             '(:type goal-guardian :verdict approve :reason "Safe") chat))
          (should (eq view rerendered))
          (should (equal 'approve
                         (plist-get
                          (car (mevedel-transcript-audit-records
                                (buffer-string) 'goal-guardian))
                          :verdict)))
          (should (eq 'ignore (get-text-property (1- (point-max)) 'gptel))))
      (kill-buffer chat)
      (kill-buffer view))))

(mevedel-deftest mevedel-goal--record-guardian-decision ()
  ,test
  (test)
  :doc "persists auditable verdict, reason, policy, timestamp, and plan hash"
  (with-temp-buffer
    (let* ((goal (mevedel-goal--create
                  :id "g1" :cycle 1 :cycles '((:cycle 1))))
           (session (mevedel-session--create :name "main" :goal goal))
           appended)
      (cl-letf (((symbol-function 'mevedel-goal--persist-cycle-index) #'ignore)
                ((symbol-function 'mevedel-goal--save-session-state) #'ignore)
                ((symbol-function 'mevedel-goal--append-guardian-audit)
                 (lambda (record _buffer) (setq appended record))))
        (mevedel-goal--record-guardian-decision
         goal '(:verdict ask :reason "Clarify scope"
                 :provider "P:M" :effort high)
         "hash" session (current-buffer)))
      (let ((record (car (plist-get (mevedel-goal-cycle-record goal)
                                    :guardian-audits))))
        (should (equal record appended))
        (should (eq 'goal-guardian (plist-get record :type)))
        (should (equal "hash" (plist-get record :plan-hash)))
        (should (equal "P:M" (plist-get record :provider)))
        (should (plist-get record :at)))))
  :doc "writes the hidden audit before the ask path session save"
  (let* ((root (make-temp-file "goal-guardian-persist-" t))
         (workspace (test-mevedel-goal--workspace root))
         (session (mevedel-session-create "main" workspace))
         (goal (mevedel-goal--create
                :id "g1" :objective "Ship" :status 'active
                :phase 'awaiting-approval :approval-policy 'automatic
                :cycle 1 :cycles '((:cycle 1)))))
    (unwind-protect
        (with-temp-buffer
          (require 'mevedel)
          (org-mode)
          (setq-local mevedel--session session
                      mevedel--workspace workspace)
          (setf (mevedel-session-goal session) goal)
          (insert "Planning complete.\n")
          (mevedel-goal--record-guardian-decision
           goal '(:verdict ask :reason "Need scope"
                   :provider "P:M" :effort high)
           "hash" session (current-buffer))
          (let ((segment
                 (mevedel-session-persistence--segment-path
                  (mevedel-session-save-path session) 1)))
            (should (file-exists-p segment))
            (with-temp-buffer
              (insert-file-contents segment)
              (let ((record
                     (car (mevedel-transcript-audit-records
                           (buffer-string) 'goal-guardian))))
                (should (eq 'ask (plist-get record :verdict)))
                (should (equal "Need scope" (plist-get record :reason)))))))
      (delete-directory root t))))

(mevedel-deftest mevedel-goal-guardian-pending-p ()
  ,test
  (test)
  :doc "reflects only the session's in-flight guardian metadata"
  (let ((session (mevedel-session--create
                  :name "main" :plan-metadata '(:guardian-pending t))))
    (should (mevedel-goal-guardian-pending-p session))
    (setf (mevedel-session-plan-metadata session) '(:guardian-pending nil))
    (should-not (mevedel-goal-guardian-pending-p session))))

(mevedel-deftest mevedel-goal--pending-interaction-p ()
  ,test
  (test)
  :doc "detects every queued and visible user interaction gate"
  (with-temp-buffer
    (let ((session (mevedel-session--create :name "main")))
      (should-not (mevedel-goal--pending-interaction-p session))
      (setf (mevedel-session-queued-user-messages session) '("wait"))
      (should (mevedel-goal--pending-interaction-p session))
      (setf (mevedel-session-queued-user-messages session) nil
            (mevedel-session-permission-queue session) '(permission))
      (should (mevedel-goal--pending-interaction-p session))
      (setf (mevedel-session-permission-queue session) nil
            (mevedel-session-plan-queue session) '(plan))
      (should (mevedel-goal--pending-interaction-p session))))
  :doc "delegates live view interaction state to its owner"
  (let ((view (generate-new-buffer " *guardian-prompt-hook*")))
    (unwind-protect
        (with-temp-buffer
          (let ((session (mevedel-session--create :name "main")))
            (setq-local mevedel--view-buffer view)
            (cl-letf (((symbol-function 'mevedel-view-interaction-pending-p)
                       (lambda (candidate)
                         (should (eq view candidate))
                         t)))
              (should (mevedel-goal--pending-interaction-p session)))))
      (kill-buffer view))))

(mevedel-deftest mevedel-goal--budget-available-p ()
  ,test
  (test)
  :doc "pauses durably with progress, evidence, and blocker context"
  (let* ((root (make-temp-file "mevedel-goal-budget-gate-" t))
         (goal (mevedel-goal--create
                :id "g1" :objective "Ship" :status 'active :phase 'planning
                :approval-policy 'automatic :cycle 2
                :cycles '((:cycle 1 :review (:verdict continue)) (:cycle 2))
                :review-findings "One failing verifier remains"
                :token-budget 50 :token-usage 50))
         (session (mevedel-session-create
                   "main" (test-mevedel-goal--workspace root))))
    (unwind-protect
        (with-temp-buffer
          (setq-local mevedel--session session)
          (setf (mevedel-session-goal session) goal)
          (should-not (mevedel-goal--budget-available-p
                       goal session 'reviewing))
          (should (eq 'paused (mevedel-goal-status goal)))
          (should (string-match-p "Progress: 1 completed review cycle"
                                  (mevedel-goal-reason goal)))
          (should (string-match-p "One failing verifier remains"
                                  (mevedel-goal-reason goal)))
          (should (string-match-p "Blocker: raise or remove"
                                  (mevedel-goal-reason goal)))
          (let* ((sidecar
                  (mevedel-session-persistence-load-sidecar
                   (mevedel-session-persistence--sidecar-path
                    (mevedel-session-save-path session))))
                 (saved (plist-get sidecar :goal)))
            (should (equal (mevedel-goal-reason goal)
                           (plist-get saved :reason)))))
      (delete-directory root t))))

(mevedel-deftest mevedel-goal--guardian-approved-p ()
  ,test
  (test)
  :doc "requires an approve audit in the current cycle"
  (let ((goal (mevedel-goal--create
               :cycle 1 :cycles '((:cycle 1 :guardian-audits
                                    ((:verdict approve)))))))
    (should (mevedel-goal--guardian-approved-p goal))
    (mevedel-goal--cycle-put
     goal :guardian-audits '((:verdict ask)))
    (should-not (mevedel-goal--guardian-approved-p goal))))

(mevedel-deftest mevedel-goal--continuation-key ()
  ,test
  (test)
  :doc "is stable for equivalent state and changes with the durable attempt"
  (let* ((goal (mevedel-goal--create
                :id "g1" :phase 'reviewing :cycle 1 :cycles '((:cycle 1))
                :checkpoint '(:attempt-id "a1")))
         (first (mevedel-goal--continuation-key
                 goal 'reviewing 'planning)))
    (should (equal first (mevedel-goal--continuation-key
                          goal 'reviewing 'planning)))
    (setf (mevedel-goal-checkpoint goal) '(:attempt-id "a2"))
    (should-not (equal first (mevedel-goal--continuation-key
                              goal 'reviewing 'planning)))))

(mevedel-deftest mevedel-goal-continuation-ready-p ()
  ,test
  (test)
  :doc "requires settled state, idleness, guardian approval, and budget"
  (let* ((root (make-temp-file "mevedel-goal-continuation-" t))
         (goal (mevedel-goal--create
                :id "g1" :objective "Ship" :status 'active
                :phase 'awaiting-approval :approval-policy 'automatic
                :cycle 1 :cycles '((:cycle 1)) :token-usage 5
                :checkpoint '(:phase guardian :attempt-id "guardian-1"
                              :dispatch-state settled)))
         (session (mevedel-session-create
                   "main" (test-mevedel-goal--workspace root))))
    (unwind-protect
        (with-temp-buffer
          (setq-local mevedel--session session)
          (test-mevedel-goal--own goal session root)
          (setf (mevedel-session-goal session) goal)
          (should-not (mevedel-goal-continuation-ready-p
                       session 'guardian 'implementing))
          (mevedel-goal--cycle-put
           goal :guardian-audits '((:verdict approve)))
          (setf (mevedel-goal-token-budget goal) 5)
          (should-not (mevedel-goal-continuation-ready-p
                       session 'guardian 'implementing))
          (should (eq 'paused (mevedel-goal-status goal)))
          (setf (mevedel-goal-status goal) 'active
                (mevedel-goal-token-budget goal) 6
                (mevedel-goal-reason goal) nil)
          (should (mevedel-goal-continuation-ready-p
                   session 'guardian 'implementing))
          (should-not (mevedel-goal-continuation-ready-p
                       session 'guardian 'implementing))
          (should (eq 'paused (mevedel-goal-status goal))))
      (delete-directory root t)))
  :doc "rejects an unsettled checkpoint and any pending interaction"
  (with-temp-buffer
    (let* ((goal (mevedel-goal--create
                  :id "g1" :status 'active :cycle 1 :cycles '((:cycle 1))
                  :checkpoint '(:phase planning :attempt-id "p1"
                                :dispatch-state started)))
           (session (mevedel-session--create :name "main" :goal goal)))
      (setq-local mevedel--session session)
      (should-not (mevedel-goal-continuation-ready-p
                   session 'planning 'guardian))
      (setf (plist-get (mevedel-goal-checkpoint goal) :dispatch-state)
            'settled
            (mevedel-session-queued-user-messages session) '("stop"))
      (should-not (mevedel-goal-continuation-ready-p
                   session 'planning 'guardian)))))

(mevedel-deftest mevedel-goal--guardian-finished ()
  ,test
  (test)
  :doc "approved plans implement only when every continuation gate is clear"
  (let* ((root (make-temp-file "goal-guardian-finish-" t))
         (session (mevedel-session-create
                   "main" (test-mevedel-goal--workspace root)))
         (goal (mevedel-goal--create
                :id "g1" :objective "Ship" :status 'active
                :phase 'awaiting-approval :approval-policy 'automatic
                :cycle 1 :cycles '((:cycle 1))
                :checkpoint '(:phase guardian :attempt-id "g1-guardian"
                              :dispatch-state started)))
         approved presented scheduled)
    (unwind-protect
        (with-temp-buffer
          (setq-local mevedel--session session)
          (test-mevedel-goal--own goal session root)
          (setf (mevedel-session-goal session) goal)
          (mevedel-plan-write-current
           "# Plan" session (current-buffer)
           (mevedel-goal--current-plan-relative-path goal))
          (cl-letf (((symbol-function 'mevedel-goal--record-guardian-decision)
                     #'test-mevedel-goal--record-guardian)
                    ((symbol-function 'run-at-time)
                     (lambda (_seconds _repeat function &rest args)
                       (setq scheduled (cons function args))))
                    ((symbol-function 'mevedel-goal--approval-callback)
                     (lambda (&rest _) (setq approved t)))
                    ((symbol-function 'mevedel-goal-present-plan)
                     (lambda (&rest _) (setq presented t))))
            (mevedel-goal--guardian-finished
             "g1" "# Plan" (mevedel-plan-hash "# Plan")
             (current-buffer) '(:verdict approve :reason "Safe"))
            (should-not presented)
            (apply (car scheduled) (cdr scheduled))
            (should approved)
            (should-not presented)))
      (delete-directory root t)))
  :doc "ask and pending user input escalate through ordinary plan approval"
  (let* ((root (make-temp-file "goal-guardian-ask-" t))
         (session (mevedel-session-create
                   "main" (test-mevedel-goal--workspace root)))
         (goal (mevedel-goal--create
                :id "g1" :objective "Ship" :status 'active
                :phase 'awaiting-approval :approval-policy 'automatic
                :cycle 1 :cycles '((:cycle 1))
                :checkpoint '(:phase guardian :attempt-id "g1-guardian"
                              :dispatch-state started)))
         reasons scheduled)
    (unwind-protect
        (with-temp-buffer
          (setq-local mevedel--session session)
          (setf (mevedel-session-goal session) goal)
          (mevedel-plan-write-current
           "# Plan" session (current-buffer)
           (mevedel-goal--current-plan-relative-path goal))
          (cl-letf (((symbol-function 'mevedel-goal--record-guardian-decision)
                     #'test-mevedel-goal--record-guardian)
                    ((symbol-function 'run-at-time)
                     (lambda (_seconds _repeat function &rest args)
                       (setq scheduled (cons function args))))
                    ((symbol-function 'mevedel-goal--approval-callback)
                     (lambda (&rest _) (error "Must not implement")))
                    ((symbol-function 'mevedel-goal-present-plan)
                     (lambda (_plan _buffer reason) (push reason reasons))))
            (mevedel-goal--guardian-finished
             "g1" "# Plan" (mevedel-plan-hash "# Plan")
             (current-buffer) '(:verdict ask :reason "Need scope"))
            (should (member
                     "Goal lifecycle event: guardian escalated plan approval to the user"
                     (mevedel-session-pending-reminders session)))
            (setf (mevedel-session-queued-user-messages session) '("wait"))
            (mevedel-goal--guardian-finished
             "g1" "# Plan" (mevedel-plan-hash "# Plan")
             (current-buffer) '(:verdict approve :reason "Safe"))
            (should (equal '("Need scope") reasons))
            (apply (car scheduled) (cdr scheduled))
            (should (equal '("Automatic approval deferred because user input or an interaction is pending."
                             "Need scope")
                           reasons))))
      (delete-directory root t)))
  :doc "approval cannot overtake an asynchronous prompt hook intervention"
  (let* ((root (make-temp-file "goal-guardian-hook-race-" t))
         (session (mevedel-session-create
                   "main" (test-mevedel-goal--workspace root)))
         (goal (mevedel-goal--create
                :id "g1" :objective "Ship" :status 'active
                :phase 'awaiting-approval :approval-policy 'automatic
                :cycle 1 :cycles '((:cycle 1))
                :checkpoint '(:phase guardian :attempt-id "g1-guardian"
                              :dispatch-state started)))
         (view (generate-new-buffer " *guardian-hook-race*"))
         scheduled reason)
    (unwind-protect
        (with-temp-buffer
          (setq-local mevedel--session session
                      mevedel--view-buffer view)
          (with-current-buffer view
            (setq-local mevedel-view--prompt-hook-pending t))
          (setf (mevedel-session-goal session) goal)
          (mevedel-plan-write-current
           "# Plan" session (current-buffer)
           (mevedel-goal--current-plan-relative-path goal))
          (cl-letf (((symbol-function 'mevedel-goal--record-guardian-decision)
                     #'test-mevedel-goal--record-guardian)
                    ((symbol-function 'run-at-time)
                     (lambda (_seconds _repeat function &rest args)
                       (setq scheduled (cons function args))))
                    ((symbol-function 'mevedel-goal--approval-callback)
                     (lambda (&rest _) (error "Must not implement")))
                    ((symbol-function 'mevedel-goal-present-plan)
                     (lambda (_plan _buffer why) (setq reason why))))
            (mevedel-goal--guardian-finished
             "g1" "# Plan" (mevedel-plan-hash "# Plan")
             (current-buffer) '(:verdict approve :reason "Safe"))
            (should-not reason)
            (apply (car scheduled) (cdr scheduled))
            (should (string-match-p "interaction is pending" reason))))
      (kill-buffer view)
      (delete-directory root t)))
  :doc "audit persistence failure prevents automatic implementation"
  (let* ((root (make-temp-file "goal-guardian-audit-fail-" t))
         (session (mevedel-session-create
                   "main" (test-mevedel-goal--workspace root)))
         (goal (mevedel-goal--create
                :id "g1" :objective "Ship" :status 'active
                :phase 'awaiting-approval :approval-policy 'automatic
                :cycle 1 :cycles '((:cycle 1))
                :checkpoint '(:phase guardian :attempt-id "g1-guardian"
                              :dispatch-state started)))
         reason)
    (unwind-protect
        (with-temp-buffer
          (setq-local mevedel--session session)
          (setf (mevedel-session-goal session) goal)
          (mevedel-plan-write-current
           "# Plan" session (current-buffer)
           (mevedel-goal--current-plan-relative-path goal))
          (cl-letf (((symbol-function 'mevedel-goal--record-guardian-decision)
                     (lambda (&rest _) (error "Disk full")))
                    ((symbol-function 'mevedel-goal--approval-callback)
                     (lambda (&rest _) (error "Must not implement")))
                    ((symbol-function 'mevedel-goal-present-plan)
                     (lambda (_plan _buffer why) (setq reason why))))
            (mevedel-goal--guardian-finished
             "g1" "# Plan" (mevedel-plan-hash "# Plan")
             (current-buffer) '(:verdict approve :reason "Safe")))
          (should (string-match-p "Disk full" reason)))
      (delete-directory root t))))

(mevedel-deftest mevedel-goal--guard-current-plan ()
  ,test
  (test)
  :doc "runs the configured guardian over the persisted current plan"
  (let* ((root (make-temp-file "goal-guard-current-" t))
         (session (mevedel-session-create
                   "main" (test-mevedel-goal--workspace root)))
         (goal (mevedel-goal--create
                :id "g1" :objective "Ship" :status 'active
                :phase 'awaiting-approval :approval-policy 'automatic
                :cycle 1 :cycles '((:cycle 1))))
         finished)
    (unwind-protect
        (with-temp-buffer
          (setq-local mevedel--session session)
          (setf (mevedel-session-goal session) goal)
          (mevedel-plan-write-current
           "# Plan" session (current-buffer)
           (mevedel-goal--current-plan-relative-path goal))
          (let ((mevedel-goal-guardian-function
                 (lambda (_goal plan _buffer callback)
                   (should (equal "# Plan" plan))
                   (funcall callback '(:verdict approve :reason "Safe")))))
            (cl-letf (((symbol-function 'mevedel-goal--guardian-finished)
                       (lambda (&rest args) (setq finished args))))
              (mevedel-goal--guard-current-plan goal (current-buffer))))
          (should (eq 'approve (plist-get (car (last finished)) :verdict)))
          (should (plist-get (mevedel-session-plan-metadata session)
                             :guardian-pending)))
      (delete-directory root t))))

(provide 'test-mevedel-goal)
;;; test-mevedel-goal.el ends here
