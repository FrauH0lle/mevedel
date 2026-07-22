;;; test-mevedel-goal.el --- Tests for mevedel-goal.el -*- lexical-binding: t -*-

;;; Commentary:

;; Goal record, continuation, settlement, and command tests.

;;; Code:

(require 'gptel-request)
(require 'mevedel)
(require 'mevedel-goal)
(require 'mevedel-session-persistence)
(require 'mevedel-tool-goal)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(mevedel-deftest mevedel-goal-create
  (:doc "creates and persists the phase-free durable Goal record")
  (let* ((saved nil)
         (session (mevedel-session--create :name "main"))
         (goal
          (cl-letf (((symbol-function 'mevedel-session-persistence-save)
                     (lambda (&rest _) (setq saved t))))
            (with-temp-buffer
              (setq-local mevedel--session session)
              (mevedel-goal-create "Ship it" session nil "goal-1")))))
    (should saved)
    (should (eq goal (mevedel-session-goal session)))
    (should (equal "goal-1" (mevedel-goal-id goal)))
    (should (equal "Ship it" (mevedel-goal-objective goal)))
    (should (eq 'active (mevedel-goal-status goal)))
    (should-not (mevedel-goal-reason goal))
    (should-not (mevedel-goal-token-budget goal))
    (should (= 0 (mevedel-goal-tokens-used goal)))
    (should (= 0 (mevedel-goal-time-used-seconds goal)))
    (should (= 0 (mevedel-goal-turns-run goal)))
    (should-not (mevedel-goal-plan-reference goal))
    (should (stringp (mevedel-goal-created-at goal)))
    (should (equal (mevedel-goal-created-at goal)
                   (mevedel-goal-updated-at goal)))))

(mevedel-deftest mevedel-goal-start
  (:doc "rejects source and prepared-target Plan handoff reservations")
  (dolist (metadata
           '((:implementation-retry (:goal-id "reserved"))
             (:implementation-goal-id "reserved")))
    (let ((session (mevedel-session--create
                    :name "main" :plan-metadata metadata)))
      (with-temp-buffer
        (setq-local mevedel--session session)
        (should-error (mevedel-goal-start "Competing Goal")
                      :type 'user-error))
      (should-not (mevedel-session-goal session)))))

(mevedel-deftest mevedel-goal-ensure
  (:doc "reactivates only the matching reserved Goal without scheduling")
  (let* ((goal (mevedel-goal--create
                :id "reserved" :status 'paused :reason "session resumed"
                :plan-reference "plans/accepted.md"))
         (session (mevedel-session--create :name "target" :goal goal))
         saved scheduled)
    (with-temp-buffer
      (setq-local mevedel--session session)
      (cl-letf (((symbol-function 'mevedel-session-persistence-save)
                 (lambda (&rest _) (setq saved t)))
                ((symbol-function 'run-at-time)
                 (lambda (&rest _) (setq scheduled t)))
                ((symbol-function 'mevedel-goal-create)
                 (lambda (&rest _) (ert-fail "Constructed duplicate Goal"))))
        (should
         (eq goal
             (mevedel-goal-ensure
              "Objective" session "plans/accepted.md" "reserved")))))
    (should saved)
    (should-not scheduled)
    (should (eq 'active (mevedel-goal-status goal)))
    (should-not (mevedel-goal-reason goal))
    (setf (mevedel-goal-plan-reference goal) "plans/other.md")
    (should-error
     (with-temp-buffer
       (mevedel-goal-ensure
        "Objective" session "plans/accepted.md" "reserved")))
    (should (eq goal (mevedel-session-goal session)))))

(mevedel-deftest mevedel-goal-active-context
  (:doc "pauses and persists before dispatch when an accepted plan mutates")
  (progn
   (let* ((root (make-temp-file "mevedel-goal-plan-" t))
         (plan-file (file-name-concat root "accepted-plan.md"))
         (session (mevedel-session--create :name "main" :save-path root))
         (goal (mevedel-goal--create
                :id "goal-1" :objective "Ship" :status 'active
                :tokens-used 0 :time-used-seconds 0 :turns-run 0
                :plan-reference "accepted-plan.md"
                :created-at "now" :updated-at "now"))
         saved)
    (unwind-protect
        (progn
          (write-region "accepted" nil plan-file nil 'silent)
          (setf (mevedel-session-goal session) goal
                (mevedel-session-plan-metadata session)
                (list :accepted-path "accepted-plan.md"
                      :accepted-hash (mevedel-plan-hash "different")))
          (with-temp-buffer
            (setq-local mevedel--session session)
            (cl-letf (((symbol-function 'mevedel-session-persistence-save)
                       (lambda (&rest _) (setq saved t))))
              (should-error (mevedel-goal-active-context session))))
          (should saved)
          (should (eq 'paused (mevedel-goal-status goal)))
          (should (string-match-p "hash" (mevedel-goal-reason goal))))
      (delete-directory root t)))
  (let* ((session (mevedel-session--create :name "main"))
         (goal (mevedel-goal--create
                :id "goal-1" :objective "Ship" :status 'active
                :tokens-used 7 :time-used-seconds 0 :turns-run 1
                :created-at "now" :updated-at "now"))
         replacements)
    (setf (mevedel-session-goal session) goal)
    (cl-letf (((symbol-function 'mevedel-system-render-prompt-file)
               (lambda (_path values) (setq replacements values) "context")))
      (should (equal "context" (mevedel-goal-active-context session))))
    (should (equal "unbounded" (cdr (assoc "token-budget" replacements))))
    (should (equal "unbounded"
                   (cdr (assoc "tokens-remaining" replacements)))))))

(mevedel-deftest mevedel-goal-capture-request
  (:doc "attributes root requests and excludes child and compaction requests")
  (let* ((session (mevedel-session--create :name "main"))
         (goal (mevedel-goal--create
                :id "goal-1" :objective "Ship" :status 'active
                :tokens-used 0 :time-used-seconds 0 :turns-run 0
                :created-at "now" :updated-at "now"))
         (buffer (generate-new-buffer " *mevedel-goal-capture*")))
    (unwind-protect
        (progn
          (setf (mevedel-session-goal session) goal)
          (with-current-buffer buffer
            (setq-local mevedel--session session
                        mevedel--current-request
                        (mevedel-request--create :session session)))
          (let ((root (gptel-make-fsm :info (list :buffer buffer :data "abc"))))
            (mevedel-goal-capture-request root)
            (should (equal "goal-1"
                           (plist-get (gptel-fsm-info root)
                                      :mevedel-goal-id))))
          (with-current-buffer buffer
            (setq-local mevedel--agent-invocation
                        (mevedel-agent-invocation--create)))
          (let ((child (gptel-make-fsm :info (list :buffer buffer))))
            (mevedel-goal-capture-request child)
            (should-not (plist-get (gptel-fsm-info child)
                                   :mevedel-goal-id)))
          (with-current-buffer buffer
            (setq-local mevedel--agent-invocation nil))
          (let ((compact
                 (gptel-make-fsm
                  :info (list :buffer buffer
                              :context '(:mevedel-compaction t)))))
            (mevedel-goal-capture-request compact)
            (should-not (plist-get (gptel-fsm-info compact)
                                   :mevedel-goal-id))))
      (kill-buffer buffer))))

(mevedel-deftest mevedel-goal-continue-if-idle
  (:doc "applies idle, queue, interaction, and budget gates before dispatch")
  (let* ((session (mevedel-session--create :name "main"))
         (goal (mevedel-goal--create
                :id "goal-1" :objective "Ship" :status 'active
                :token-budget 10 :tokens-used 0
                :time-used-seconds 0 :turns-run 0
                :created-at "now" :updated-at "now"))
         (buffer (generate-new-buffer " *mevedel-goal-gates*"))
         dispatched scheduled)
    (unwind-protect
        (progn
          (setf (mevedel-session-goal session) goal)
          (with-current-buffer buffer
            (setq-local mevedel--session session))
          (cl-letf (((symbol-function 'mevedel--submit-generated-turn)
                     (lambda (&rest _) (setq dispatched t)))
                    ((symbol-function 'run-at-time)
                     (lambda (&rest args) (setq scheduled args))))
            (should (eq 'dispatched
                        (mevedel-goal-continue-if-idle session buffer)))
            (should dispatched)
            (with-current-buffer buffer
              (setq-local mevedel--current-request
                          (mevedel-request--create :session session)))
            (should (eq 'request
                        (mevedel-goal-continue-if-idle session buffer)))
            (with-current-buffer buffer
              (setq-local mevedel--current-request nil))
            (setf (mevedel-session-queued-user-messages session)
                  '((:input "steer")))
            (should (eq 'queued-user-message
                        (mevedel-goal-continue-if-idle session buffer)))
            (should scheduled)
            (setf (mevedel-session-queued-user-messages session) nil
                  (mevedel-session-permission-queue session) '(pending))
            (should (eq 'interaction
                        (mevedel-goal-continue-if-idle session buffer)))
            (setf (mevedel-session-permission-queue session) nil
                  (mevedel-goal-tokens-used goal) 10)
            (should (eq 'budget
                        (mevedel-goal-continue-if-idle session buffer)))))
      (kill-buffer buffer))))

(mevedel-deftest mevedel-goal-settle-turn
  (:doc "charges provider tokens, elapsed time, and one turn to an attributed Goal")
  (progn
   (let* ((session (mevedel-session--create :name "main"))
         (goal (mevedel-goal--create
                :id "goal-1" :objective "Ship" :status 'active
                :tokens-used 2 :time-used-seconds 1 :turns-run 3
                :created-at "now" :updated-at "now"))
         (buffer (generate-new-buffer " *mevedel-goal-settle*"))
         (fsm (gptel-make-fsm
               :info (list :buffer buffer :mevedel-goal-id "goal-1"
                           :mevedel-goal-started-at (- (float-time) 2)
                           :tokens-full '(:input 10 :output 3)))))
    (unwind-protect
        (progn
          (setf (mevedel-session-goal session) goal)
          (with-current-buffer buffer
            (setq-local mevedel--session session))
          (mevedel-goal-settle-turn fsm)
          (should (= 15 (mevedel-goal-tokens-used goal)))
          (should (>= (mevedel-goal-time-used-seconds goal) 3))
          (should (= 4 (mevedel-goal-turns-run goal))))
      (kill-buffer buffer)))
  (let* ((session (mevedel-session--create :name "main"))
         (goal (mevedel-goal--create
                :id "goal-2" :objective "Ship" :status 'active
                :token-budget 100 :tokens-used 49
                :time-used-seconds 0 :turns-run 0
                :created-at "now" :updated-at "now"))
         (buffer (generate-new-buffer " *mevedel-goal-budget-crossing*"))
         (fsm (gptel-make-fsm
               :info (list :buffer buffer :mevedel-goal-id "goal-2"
                           :tokens-full '(:input 40 :output 11 :cached 500)))))
    (unwind-protect
        (progn
          (setf (mevedel-session-goal session) goal)
          (with-current-buffer buffer (setq-local mevedel--session session))
          (mevedel-goal-settle-turn fsm)
          (should (= 100 (mevedel-goal-tokens-used goal)))
          (should (eq 'budget-limited (mevedel-goal-status goal)))
          (should (string-match-p "100/100" (mevedel-goal-reason goal)))
          (should (equal '("50%" "80%" "100%")
                         (mapcar
                          (lambda (reminder)
                            (and (string-match "\\(50%\\|80%\\|100%\\)"
                                               reminder)
                                 (match-string 1 reminder)))
                          (mevedel-session-pending-reminders session))))
          (should (string-match-p
                   "Prioritize the remaining requirements"
                   (nth 0 (mevedel-session-pending-reminders session))))
          (should (string-match-p
                   "avoid low-value detours"
                   (nth 1 (mevedel-session-pending-reminders session))))
          (mevedel-goal-settle-turn
           (gptel-make-fsm
            :info (list :buffer buffer :mevedel-goal-id "goal-2"
                        :tokens-full '(:input 1 :output 0))))
          (should (= 3 (length (mevedel-session-pending-reminders session)))))
      (kill-buffer buffer)))
  (dolist (status '(complete blocked))
    (let* ((session (mevedel-session--create :name "main"))
           (goal (mevedel-goal--create
                  :id "goal-3" :objective "Ship" :status status
                  :reason (and (eq status 'blocked) "Need access")
                  :token-budget 100 :tokens-used 99
                  :time-used-seconds 0 :turns-run 0
                  :created-at "now" :updated-at "now"))
           (buffer (generate-new-buffer " *mevedel-goal-budget-terminal*")))
      (unwind-protect
          (progn
            (setf (mevedel-session-goal session) goal)
            (with-current-buffer buffer (setq-local mevedel--session session))
            (mevedel-goal-settle-turn
             (gptel-make-fsm
              :info (list :buffer buffer :mevedel-goal-id "goal-3"
                          :tokens-full '(:input 1 :output 0))))
            (should (eq status (mevedel-goal-status goal))))
        (kill-buffer buffer))))))

(mevedel-deftest mevedel-goal-settle-failure
  (:doc "retries one transient failure and pauses on the next or a terminal failure")
  (let* ((session (mevedel-session--create :name "main"))
         (goal (mevedel-goal--create
                :id "goal-1" :objective "Ship" :status 'active
                :tokens-used 0 :time-used-seconds 0 :turns-run 0
                :created-at "now" :updated-at "now"))
         (buffer (generate-new-buffer " *mevedel-goal-failure*")))
    (unwind-protect
        (progn
          (setf (mevedel-session-goal session) goal)
          (with-current-buffer buffer
            (setq-local mevedel--session session
                        mevedel-goal--transient-retries 0))
          (let ((fsm (gptel-make-fsm
                      :info (list :buffer buffer :mevedel-goal-id "goal-1"
                                  :error "temporary network timeout"))))
            (mevedel-goal-settle-failure fsm 'error)
            (should (eq 'active (mevedel-goal-status goal)))
            (mevedel-goal-settle-failure fsm 'error)
            (should (eq 'paused (mevedel-goal-status goal)))
            (should (string-match-p "timeout" (mevedel-goal-reason goal))))
          (setf (mevedel-goal-status goal) 'active
                (mevedel-goal-reason goal) nil)
          (with-current-buffer buffer
            (setq-local mevedel-goal--transient-retries 0))
          (mevedel-goal-settle-failure
           (gptel-make-fsm
            :info (list :buffer buffer :mevedel-goal-id "goal-1"
                        :error "authentication failed"))
           'error)
          (should (eq 'paused (mevedel-goal-status goal)))
          (setf (mevedel-goal-status goal) 'complete
                (mevedel-goal-reason goal) nil)
          (mevedel-goal-settle-failure
           (gptel-make-fsm
            :info (list :buffer buffer :mevedel-goal-id "goal-1"
                        :error "provider failed"))
           'error)
          (should (eq 'complete (mevedel-goal-status goal))))
      (kill-buffer buffer))))

(mevedel-deftest mevedel-goal-pause-runtime-failure
  (:doc "persists a concrete runtime failure only for an active Goal")
  (let* ((session (mevedel-session--create :name "main"))
         (goal (mevedel-goal--create
                :id "goal-1" :objective "Ship" :status 'active
                :tokens-used 0 :time-used-seconds 0 :turns-run 0
                :created-at "now" :updated-at "now"))
         (buffer (generate-new-buffer " *mevedel-goal-runtime-failure*"))
         saved)
    (unwind-protect
        (progn
          (setf (mevedel-session-goal session) goal)
          (with-current-buffer buffer
            (setq-local mevedel--session session))
          (cl-letf (((symbol-function 'mevedel-session-persistence-save)
                     (lambda (&rest _) (setq saved t))))
            (should (eq goal (mevedel-goal-pause-runtime-failure
                              buffer "Compaction failed: boom"))))
          (should saved)
          (should (eq 'paused (mevedel-goal-status goal)))
          (should (equal "Compaction failed: boom"
                         (mevedel-goal-reason goal))))
      (kill-buffer buffer))))

(mevedel-deftest mevedel-tool-goal-update
  (:doc "accepts only matching active terminal transitions and requires blocked detail")
  (let* ((session (mevedel-session--create :name "main"))
         (goal (mevedel-goal--create
                :id "goal-1" :objective "Ship" :status 'active
                :tokens-used 0 :time-used-seconds 0 :turns-run 0
                :created-at "now" :updated-at "now")))
    (setf (mevedel-session-goal session) goal)
    (should-error (mevedel-tool-goal-update 'complete nil session "old"))
    (should-error (mevedel-tool-goal-update 'blocked "  " session "goal-1"))
    (should (equal "Goal status changed to blocked"
                   (mevedel-tool-goal-update
                    "blocked" " Need a credential. " session "goal-1")))
    (should (eq 'blocked (mevedel-goal-status goal)))
    (should (equal "Need a credential." (mevedel-goal-reason goal)))
    (should-error (mevedel-tool-goal-update 'complete nil session "goal-1"))))

(mevedel-deftest mevedel-goal-resume
  (:doc "queues optional steering before rearming continuation")
  (let* ((session (mevedel-session--create :name "main"))
         (goal (mevedel-goal--create
                :id "goal-1" :objective "Ship" :status 'paused
                :reason "paused by user" :tokens-used 0
                :time-used-seconds 0 :turns-run 0
                :created-at "now" :updated-at "now"))
         scheduled)
    (setf (mevedel-session-goal session) goal
          (mevedel-session-plan-metadata session)
          '(:status accepted :implementation-goal-id "goal-1"))
    (with-temp-buffer
      (setq-local mevedel--session session)
      (cl-letf (((symbol-function 'mevedel-session-persistence-save) #'ignore)
                ((symbol-function 'run-at-time)
                 (lambda (&rest args) (setq scheduled args))))
        (mevedel-goal-resume "  steer first  ")))
    (should scheduled)
    (should (eq 'active (mevedel-goal-status goal)))
    (should-not (mevedel-goal-reason goal))
    (should-not
     (plist-member (mevedel-session-plan-metadata session)
                   :implementation-goal-id))
    (should (equal '((:input "steer first"))
                   (mevedel-session-queued-user-messages session)))
    (setf (mevedel-goal-status goal) 'budget-limited)
    (with-temp-buffer
      (setq-local mevedel--session session)
      (should-error (mevedel-goal-resume) :type 'user-error))))

(mevedel-deftest mevedel-goal-set-budget
  (:doc "adjusts durable limits, emits reminders, and reactivates limited Goals")
  (let* ((session (mevedel-session--create :name "main"))
         (goal (mevedel-goal--create
                :id "goal-1" :objective "Ship" :status 'active
                :token-budget 100 :tokens-used 60
                :time-used-seconds 1 :turns-run 2
                :created-at "now" :updated-at "now"))
         (buffer (generate-new-buffer " *mevedel-goal-budget*"))
         (saved 0)
         (scheduled 0))
    (unwind-protect
        (progn
          (setf (mevedel-session-goal session) goal)
          (with-current-buffer buffer
            (setq-local mevedel--session session
                        mevedel--current-request
                        (mevedel-request--create :session session)))
          (cl-letf (((symbol-function 'mevedel-session-persistence-save)
                     (lambda (&rest _) (cl-incf saved)))
                    ((symbol-function 'run-at-time)
                     (lambda (&rest _) (cl-incf scheduled))))
            (with-current-buffer buffer
              (dolist (invalid '("" "0" "-1" "many"))
                (should-error (mevedel-goal-set-budget invalid)
                              :type 'user-error))
              (mevedel-goal-set-budget "50")
              (should (eq 'budget-limited (mevedel-goal-status goal)))
              (should (string-match-p "100.*50.*60.*0.*budget-limited"
                                      (car (last
                                            (mevedel-session-pending-reminders
                                             session)))))
              (mevedel-goal-set-budget "80")
              (should (eq 'active (mevedel-goal-status goal)))
              (should-not (mevedel-goal-reason goal))
              (should (eq 'request
                          (mevedel-goal-continue-if-idle session buffer)))
              (mevedel-goal-set-budget "none")
              (should-not (mevedel-goal-token-budget goal))))
          (should (= 3 saved))
          (should (= 1 scheduled))
          (should (= 3 (length (mevedel-session-pending-reminders session))))
          (should (string-match-p
                   "80.*unbounded.*60.*unbounded.*active"
                   (car (last (mevedel-session-pending-reminders session))))))
      (kill-buffer buffer))))

(mevedel-deftest mevedel-goal-tool-result-budget-warning
  (:doc "returns the 100% tool-boundary warning once for known provider usage")
  (let* ((session (mevedel-session--create :name "main"))
         (goal (mevedel-goal--create
                :id "goal-1" :objective "Ship" :status 'active
                :token-budget 100 :tokens-used 90
                :time-used-seconds 0 :turns-run 0
                :created-at "now" :updated-at "now"))
         (fsm (gptel-make-fsm
               :info '(:mevedel-goal-id "goal-1"
                       :tokens-full (:input 7 :output 3 :cached 1000)))))
    (setf (mevedel-session-goal session) goal)
    (should (string-match-p
             "stop new substantive work"
             (mevedel-goal-tool-result-budget-warning session fsm)))
    (should-not (mevedel-goal-tool-result-budget-warning session fsm))
    (should (= 90 (mevedel-goal-tokens-used goal)))))

(mevedel-deftest mevedel-goal-edit
  (:doc "rotates identity, retains the run, and refreshes an in-flight Goal")
  (let* ((session (mevedel-session--create :name "main"))
         (goal (mevedel-goal--create
                :id "goal-1" :objective "Old" :status 'active
                :token-budget 100 :tokens-used 20
                :time-used-seconds 30 :turns-run 4
                :plan-reference "accepted-plan.md"
                :created-at "created" :updated-at "old-update"))
         (buffer (generate-new-buffer " *mevedel-goal-edit*"))
         saved scheduled refreshed)
    (unwind-protect
        (progn
          (setf (mevedel-session-goal session) goal)
          (with-current-buffer buffer
            (setq-local mevedel--session session
                        mevedel--current-request
                        (mevedel-request--create :session session)))
          (with-current-buffer buffer
            (should-error (mevedel-goal-edit "  ") :type 'user-error))
          (should (equal "goal-1" (mevedel-goal-id goal)))
          (cl-letf (((symbol-function 'mevedel-session-persistence-save)
                     (lambda (&rest _) (setq saved t)))
                    ((symbol-function 'mevedel-goal-new-id)
                     (lambda () "goal-2"))
                    ((symbol-function 'mevedel-goal-active-context)
                     (lambda (_session) "updated context"))
                    ((symbol-function 'mevedel-agent-control-steer-user)
                     (lambda (actual-session message)
                       (setq refreshed (list actual-session message))))
                    ((symbol-function 'run-at-time)
                     (lambda (&rest args) (setq scheduled args))))
            (with-current-buffer buffer
              (should (eq goal (mevedel-goal-edit "  New objective  ")))))
          (should saved)
          (should scheduled)
          (should (equal (list session "updated context") refreshed))
          (should (equal "goal-2" (mevedel-goal-id goal)))
          (should (equal "New objective" (mevedel-goal-objective goal)))
          (should (eq 'active (mevedel-goal-status goal)))
          (should (= 100 (mevedel-goal-token-budget goal)))
          (should (= 20 (mevedel-goal-tokens-used goal)))
          (should (= 30 (mevedel-goal-time-used-seconds goal)))
          (should (= 4 (mevedel-goal-turns-run goal)))
          (should (equal "accepted-plan.md"
                         (mevedel-goal-plan-reference goal)))
          (should (equal "created" (mevedel-goal-created-at goal)))
          (should-not (equal "old-update" (mevedel-goal-updated-at goal)))
          (should (= 1 (length (mevedel-session-pending-reminders session))))
          (should (string-match-p
                   "New objective"
                   (car (mevedel-session-pending-reminders session))))
          (with-current-buffer buffer
            (should (eq 'request
                        (mevedel-goal-continue-if-idle session buffer))))
          (should-error
           (mevedel-tool-goal-update 'complete nil session "goal-1"))
          (mevedel-goal-settle-turn
           (gptel-make-fsm
            :info (list :buffer buffer :mevedel-goal-id "goal-1"
                        :tokens-full '(:input 3 :output 2))))
          (should (= 25 (mevedel-goal-tokens-used goal)))
          (should (= 5 (mevedel-goal-turns-run goal)))
          (should (equal "Goal status changed to complete"
                         (mevedel-tool-goal-update
                          'complete nil session "goal-2"))))
      (kill-buffer buffer))))

(provide 'test-mevedel-goal)
;;; test-mevedel-goal.el ends here
