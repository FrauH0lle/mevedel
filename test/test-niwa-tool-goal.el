;;; test-niwa-tool-goal.el --- Goal tools through the pipeline -*- lexical-binding: t -*-

;;; Commentary:
;; Root ownership, creation, inspection, completion and ToolCall discovery.

;;; Code:

(require 'niwa)
(require 'niwa-tool-goal)
(require 'niwa-tools)
(require 'niwa-collaboration)
(require 'niwa-collaboration-guest)
(require 'niwa-collaboration-transport)
(require 'niwa-context-delivery)
(require 'niwa-tool-ptc)
(require 'niwa-pipeline)
(require 'niwa-session-codec)
(require 'niwa-workspace-identity)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(defmacro niwa-tool-goal-test--with-request (&rest body)
  "Run BODY with an isolated root session, FSM and deferred continuations.
Completion verification is captured: `verify-prompts' lists requested prompts
and `verify-callback' delivers the verifier outcome."
  (declare (indent 0))
  `(let* ((gptel--known-backends nil)
          (niwa-workspace-identity--cache (make-hash-table :test #'equal))
          (root (make-temp-file "niwa-goal-tools-" t))
          (session (niwa-skills-test--make-session "goals" root))
          (scheduled nil)
          (verify-prompts nil)
          (verify-callback nil))
     (unwind-protect
         (with-temp-buffer
           (org-mode)
           (setq-local default-directory (file-name-as-directory root)
                       niwa--session session)
           (niwa-workspace-identity-ensure root)
           (setf (niwa-session-save-path session) root)
           (let* ((fsm (gptel-make-fsm
                        :info (list :buffer (current-buffer)
                                    :backend (gptel-make-openai "goal-test" :key "test" :models '(test))
                                    :data (list :messages [])
                                    :tokens-full '(:input 20 :output 5))))
                  (niwa-tools--current-engine fsm))
             (setq-local niwa--current-request
                         (niwa-request--create :session session :fsm fsm))
             (niwa-tool-goal--register)
             (setf (niwa-session-tool-catalog session)
                   '((("niwa" "CreateGoal") . "Create goal")
                     (("niwa" "GetGoal") . "Inspect goal")
                     (("niwa" "UpdateGoal") . "Complete goal")))
             (cl-letf (((symbol-function 'niwa-goal-schedule-continuation)
                        (lambda (&rest args) (push args scheduled)))
                       ((symbol-function 'niwa-review-verify)
                        (lambda (prompt callback &optional workload)
                          (should (eq 'goal-review workload))
                          (push prompt verify-prompts)
                          (setq verify-callback callback))))
               ,@body)))
       (delete-directory root t))))

(niwa-deftest niwa-tool-goal--handle-create ()
  ,test
  (test)
  :doc "creates through ToolCall, persists, attributes, and continues after settlement"
  (niwa-tool-goal-test--with-request
    (let (outcome)
      (niwa-tool-ptc--handler
       (lambda (value) (setq outcome value))
       '(:expression "(CreateGoal :objective \"Fix integration tests\" :token_budget 100)"))
      ;; Nested calls run from timers after the dispatch binding unwinds.
      (let ((deadline (+ (float-time) 5))
            (niwa-tools--current-engine nil))
        (while (and (not outcome) (< (float-time) deadline))
          (accept-process-output nil 0.01)))
      (should (eq 'success (plist-get outcome :status)))
      (let* ((goal (niwa-session-goal session))
             (reported (plist-get (json-parse-string
                                   (plist-get outcome :result) :object-type 'plist)
                                  :goal)))
        (should (equal "Fix integration tests" (niwa-goal-objective goal)))
        (should (= 100 (niwa-goal-token-budget goal)))
        (should (= 25 (plist-get reported :tokens_used)))
        (should (equal (niwa-goal-id goal)
                       (plist-get (gptel-fsm-info fsm) :niwa-goal-id)))
        (niwa-context-delivery-stage fsm)
        (let ((entries (plist-get (gptel-fsm-info fsm) :niwa-reminder-entries)))
          (should (assq 'context-active-goal
                        (mapcar (lambda (entry) (cons (plist-get entry :type) entry)) entries)))
          (should (cl-find 'context-goal-policy entries
                           :key (lambda (entry) (plist-get entry :type)))))
        (should (file-exists-p (file-name-concat root "session.meta.el")))
        (let* ((saved (niwa-session-codec-read
                       (file-name-concat root "session.meta.el")))
               (restored (niwa-session-codec-deserialize
                          saved (niwa-session-workspace session)))
               (restored-goal (niwa-session-goal (plist-get restored :session))))
          (should (equal (niwa-goal-id goal) (niwa-goal-id restored-goal)))
          (should (eq 'paused (niwa-goal-status restored-goal))))
        (should (eq 'request (niwa-goal-continue-if-idle session (current-buffer))))
        (niwa-goal-settle-turn fsm)
        (niwa-goal-settle-turn fsm)
        (should (= 25 (niwa-goal-tokens-used goal)))
        (should (= 1 (niwa-goal-turns-run goal)))
        (niwa-goal-dispatch-after-turn fsm)
        (should (= 2 (length scheduled)))
        (setq-local niwa--current-request nil)
        (let (sent)
          (cl-letf (((symbol-function 'niwa--submit-generated-turn)
                     (lambda (prompt &rest _) (setq sent prompt))))
            (should (eq 'dispatched (niwa-goal-continue-if-idle
                                     session (current-buffer)))))
          (should (equal niwa-goal--continuation-trigger sent))))))

  :doc "uses the configured default and rejects invalid input without creating a Goal"
  (niwa-tool-goal-test--with-request
    (setq-local niwa-goal-token-budget 250)
    (dolist (args '((:objective "  ")
                    (:objective "Ship" :token_budget 0)
                    (:objective "Ship" :token_budget -1)
                    (:objective "Ship" :token_budget 1.5)))
      (should-error (niwa-tool-goal--handle-create args))
      (should-not (niwa-session-goal session)))
    (niwa-tool-goal--handle-create '(:objective "  Ship  "))
    (should (= 250 (niwa-goal-token-budget (niwa-session-goal session))))
    (should (equal "Ship" (niwa-goal-objective (niwa-session-goal session))))
    (should-error (niwa-tool-goal--handle-create '(:objective "Replace"))))

  :doc "replaces a completed same-turn Goal without charging either Goal twice"
  (niwa-tool-goal-test--with-request
    (niwa-tool-goal--handle-create '(:objective "First" :token_budget 20))
    (let ((first (niwa-session-goal session)))
      (niwa-tool-goal--handle-update #'ignore '(:status "complete"))
      (funcall verify-callback '(:status ok :kind fork :verdict pass :result "ok"))
      (niwa-tool-goal--handle-create '(:objective "Second"))
      (should (= 25 (niwa-goal-tokens-used first)))
      (should (= 1 (niwa-goal-turns-run first)))
      (should-not (niwa-session-pending-reminders session))
      (plist-put (gptel-fsm-info fsm) :tokens-full '(:input 40 :output 10))
      (niwa-goal-settle-turn fsm)
      (should (= 25 (niwa-goal-tokens-used (niwa-session-goal session))))
      (should (= 1 (niwa-goal-turns-run (niwa-session-goal session)))))))

(niwa-deftest niwa-tool-goal--handle/non-ascii ()
  ,test
  (test)
  :doc "a non-ASCII objective reaches the pipeline result as multibyte text"
  (niwa-tool-goal-test--with-request
    (let (outcome)
      (niwa-tool-ptc--handler
       (lambda (value) (setq outcome value))
       '(:expression "(CreateGoal :objective \"Fix café — tests\" :token_budget 100)"))
      (let ((niwa-tools--current-engine nil))
        (niwa-test--await 5 "CreateGoal did not answer" outcome))
      (let ((text (plist-get outcome :result)))
        (should (multibyte-string-p text))
        (should (equal "Fix café — tests"
                       (plist-get (plist-get (json-parse-string
                                              text :object-type 'plist)
                                             :goal)
                                  :objective)))))))

(niwa-deftest niwa-tool-goal--handle-get ()
  ,test
  (test)
  :doc "returns no Goal and inspects every status without mutation or dispatch"
  (niwa-tool-goal-test--with-request
    (should (equal "{\"goal\":null}"
                   (plist-get (niwa-tool-goal--handle-get nil) :result)))
    (dolist (status '(active paused blocked budget-limited complete))
      (let ((goal (niwa-goal--create
                   :id "g" :objective "Ship" :status status :reason "Reason"
                   :token-budget 100 :tokens-used 70 :time-used-seconds 12
                   :turns-run 2 :plan-reference "local/plans/accepted-1.md")))
        (setf (niwa-session-goal session) goal)
        (let* ((before (copy-sequence goal))
               (value (plist-get
                       (json-parse-string
                        (plist-get (niwa-tool-goal--handle-get nil) :result)
                        :object-type 'plist)
                       :goal)))
          (should (equal (symbol-name status) (plist-get value :status)))
          (should (= 70 (plist-get value :tokens_used)))
          (should (= 30 (plist-get value :remaining_tokens)))
          (should (equal "work://plans/accepted-1.md" (plist-get value :plan_reference)))
          (should (equal before goal)))))
    ;; Even a Goal stopped for an invalid plan reference remains inspectable.
    (let ((goal (niwa-session-goal session)))
      (setf (niwa-goal-status goal) 'paused
            (niwa-goal-plan-reference goal) "invalid-plan.md")
      (let ((value (plist-get (json-parse-string
                               (plist-get (niwa-tool-goal--handle-get nil) :result)
                               :object-type 'plist) :goal)))
        (should (equal "paused" (plist-get value :status)))
        (should (stringp (plist-get value :plan_reference_error)))
        (should (eq 'paused (niwa-goal-status goal)))))
    (should-not scheduled))

  :doc "reports incomplete durable native usage as a lower bound"
  (niwa-tool-goal-test--with-request
    (setf (niwa-session-goal session)
          (niwa-goal--create :id "g" :objective "Ship" :status 'budget-limited
                               :reason "Usage unavailable" :token-budget 100
                               :tokens-used 12 :tokens-incomplete-p t
                               :time-used-seconds 0 :turns-run 1))
    (let ((value (plist-get (json-parse-string
                             (plist-get (niwa-tool-goal--handle-get nil) :result)
                             :object-type 'plist :null-object nil) :goal)))
      (should-not (plist-get value :tokens_used))
      (should-not (plist-get value :remaining_tokens))
      (should (= 12 (plist-get value :known_tokens_used)))))

  :doc "includes known live usage once, and never charges a different Goal"
  (niwa-tool-goal-test--with-request
    (niwa-tool-goal--handle-create '(:objective "Ship" :token_budget 100))
    (dolist (settle '(nil t))
      (when settle (niwa-goal-settle-turn fsm))
      (let ((value (plist-get (json-parse-string
                               (plist-get (niwa-tool-goal--handle-get nil) :result)
                               :object-type 'plist) :goal)))
        (should (= 25 (plist-get value :tokens_used)))
        ;; Complete usage leaves no separate lower bound to report.
        (should-not (plist-get value :known_tokens_used))
        (should (= 75 (plist-get value :remaining_tokens)))))
    (setf (niwa-goal-id (niwa-session-goal session)) "different")
    (plist-put (gptel-fsm-info fsm) :tokens-full '(:input 999))
    (let ((value (plist-get (json-parse-string
                             (plist-get (niwa-tool-goal--handle-get nil) :result)
                             :object-type 'plist) :goal)))
      (should (= 25 (plist-get value :tokens_used))))))

(niwa-deftest niwa-tool-goal--handle-update ()
  ,test
  (test)
  :doc "terminal Goals notify once locally and wake only writable devices without text"
  (dolist (terminal '("blocked" "complete"))
    (niwa-tool-goal-test--with-request
      (let* ((niwa-collaboration--rooms (make-hash-table :test #'eq))
             (subscriptions (make-hash-table :test #'equal))
             (guests (make-hash-table :test #'eq))
             notices frames sealed
             (niwa-notify-function
              (lambda (kind details)
                (push (list kind (plist-get details :text)) notices))))
        (puthash "reader" '(:writable nil) subscriptions)
        (puthash "phone" '(:writable t) subscriptions)
        (puthash 1 '(:ready t :writable t) guests)
        (puthash 2 '(:ready t :writable nil) guests)
        (puthash (current-buffer)
                 (list :session session :guests guests :push-guests subscriptions :transport 'relay)
                 niwa-collaboration--rooms)
        (cl-letf (((symbol-function 'niwa-collaboration--transport-control)
                   (lambda (_transport frame) (push frame frames)))
                  ((symbol-function 'niwa-collaboration--transport-send)
                   (lambda (_transport peer frame) (push (cons peer frame) sealed))))
          (niwa-tool-goal--handle-create '(:objective "Ship"))
          (niwa-pipeline-run-tool-outcome
           (niwa-tool-get "UpdateGoal") #'ignore
           (list :status terminal :summary "Review requested"))
          (when verify-callback
            (should-not notices)
            (funcall verify-callback '(:verdict pass :result "OK")))
          (should (equal notices (list (list 'goal (concat "Goal " terminal ": Ship")))))
          (should (equal frames '((:t "push" :guestIds ["phone"]))))
          (should (equal sealed '((1 :t "notice" :kind "goal"))))))))

  :doc "completes only after the verifier passes, leaving its usage to agent accounting"
  (niwa-tool-goal-test--with-request
    (niwa-tool-goal--handle-create '(:objective "Ship the parser"))
    (let ((goal (niwa-session-goal session))
          outcome)
      (niwa-pipeline-run-tool-outcome
       (niwa-tool-get "UpdateGoal") (lambda (value) (setq outcome value))
       '(:status "complete"))
      (should-not outcome)
      (should (eq 'active (niwa-goal-status goal)))
      (should (string-search "Ship the parser" (car verify-prompts)))
      (funcall verify-callback
               '(:status ok :kind fork :verdict pass :result "VERDICT: PASS" :usage 7))
      (should (eq 'success (plist-get outcome :status)))
      (should (equal "Goal status changed to complete" (plist-get outcome :result)))
      (should (eq 'complete (niwa-goal-status goal)))
      (should-not (niwa-turn-end-requested-p (gptel-fsm-info fsm)))
      ;; The verifier agent charges itself; the RESULT usage is not re-charged.
      (should (= 0 (niwa-goal-tokens-used goal)))
      (niwa-goal-settle-turn fsm)
      (should (= 25 (niwa-goal-tokens-used goal)))
      (should (eq 'inactive (niwa-goal-continue-if-idle session (current-buffer))))))

  :doc "returns every rejection as findings, keeps the Goal active, and ends the turn"
  (niwa-tool-goal-test--with-request
    (niwa-tool-goal--handle-create '(:objective "Ship"))
    (let ((goal (niwa-session-goal session))
          (niwa-goal-max-rejected-completions 10))
      (dolist (case '(((:status ok :kind fork :verdict fail :result "Parser crashes" :usage 5)
                       "returned FAIL" "Parser crashes")
                      ((:status ok :kind fork :verification-rejected t :result "rambling" :usage 5)
                       "no valid verdict" "rambling")
                      ((:status error :reason agent-errored :message "Provider failed" :usage 5)
                       "did not finish" "Provider failed")))
        (let (outcome)
          (niwa-pipeline-run-tool-outcome
           (niwa-tool-get "UpdateGoal") (lambda (value) (setq outcome value))
           '(:status "complete"))
          (funcall verify-callback (car case))
          (should (eq 'error (plist-get outcome :status)))
          (should (string-search "Goal remains active" (plist-get outcome :result)))
          (should (string-search (nth 1 case) (plist-get outcome :result)))
          (should (string-search (nth 2 case) (plist-get outcome :result)))
          (should (eq 'active (niwa-goal-status goal)))
          (should (eq 'goal-verification-failed
                      (niwa-turn-end-requested-p (gptel-fsm-info fsm))))))
      (should (= 0 (niwa-goal-tokens-used goal)))))

  :doc "PARTIAL completes the Goal and records that some requirements went unverified"
  (niwa-tool-goal-test--with-request
    (niwa-tool-goal--handle-create '(:objective "Ship"))
    (let ((goal (niwa-session-goal session))
          outcome)
      (niwa-pipeline-run-tool-outcome
       (niwa-tool-get "UpdateGoal") (lambda (value) (setq outcome value))
       '(:status "complete"))
      (funcall verify-callback
               '(:status ok :kind fork :verdict partial :result "No network for the API check"))
      (should (eq 'success (plist-get outcome :status)))
      (should (string-search "No network for the API check" (plist-get outcome :result)))
      (should (eq 'complete (niwa-goal-status goal)))
      (should (string-search "PARTIAL" (niwa-goal-reason goal)))
      (should-not (niwa-turn-end-requested-p (gptel-fsm-info fsm)))))

  :doc "consecutive rejected claims up to the limit block the Goal; resume restarts the count"
  (niwa-tool-goal-test--with-request
    (niwa-tool-goal--handle-create '(:objective "Ship"))
    (let ((goal (niwa-session-goal session))
          (niwa-goal-max-rejected-completions 2)
          outcome)
      (cl-flet ((reject (report)
                  (setq outcome nil)
                  (niwa-pipeline-run-tool-outcome
                   (niwa-tool-get "UpdateGoal") (lambda (value) (setq outcome value))
                   '(:status "complete"))
                  (funcall verify-callback
                           (list :status 'ok :kind 'fork :verdict 'fail :result report))))
        (reject "First miss")
        (should (eq 'active (niwa-goal-status goal)))
        (reject "Second miss")
        (should (eq 'blocked (niwa-goal-status goal)))
        (should (string-search "2 consecutive" (niwa-goal-reason goal)))
        (should (eq 'error (plist-get outcome :status)))
        (should (string-search "Goal blocked" (plist-get outcome :result)))
        (should (string-search "Second miss" (plist-get outcome :result)))
        (should (eq 'goal-verification-failed
                    (niwa-turn-end-requested-p (gptel-fsm-info fsm))))
        (niwa-goal-resume)
        (should (eq 'active (niwa-goal-status goal)))
        (reject "Third miss")
        (should (eq 'active (niwa-goal-status goal))))))

  :doc "blocks immediately without verification"
  (niwa-tool-goal-test--with-request
    (niwa-tool-goal--handle-create '(:objective "Ship"))
    (let (outcome)
      (niwa-tool-goal--handle-update
       (lambda (value) (setq outcome value))
       '(:status "blocked" :summary "Needs the production API key"))
      (should (equal "Goal status changed to blocked" (plist-get outcome :result)))
      (should-not verify-prompts)
      (should (eq 'blocked (niwa-goal-status (niwa-session-goal session))))))

  :doc "refuses to verify against an unreadable accepted plan"
  (niwa-tool-goal-test--with-request
    (niwa-tool-goal--handle-create '(:objective "Ship"))
    (setf (niwa-goal-plan-reference (niwa-session-goal session))
          "local/plans/accepted-1.md")
    (let (outcome)
      (niwa-pipeline-run-tool-outcome
       (niwa-tool-get "UpdateGoal") (lambda (value) (setq outcome value))
       '(:status "complete"))
      (should (string-search "accepted plan is unreadable" (plist-get outcome :result)))
      (should-not verify-prompts)
      (should (eq 'active (niwa-goal-status (niwa-session-goal session))))))

  :doc "a pass cannot complete a Goal edited, paused, or cleared meanwhile"
  (dolist (change (list (lambda (goal _session) (setf (niwa-goal-id goal) "edited"))
                        (lambda (goal _session) (setf (niwa-goal-status goal) 'paused))
                        (lambda (_goal session) (setf (niwa-session-goal session) nil))))
    (niwa-tool-goal-test--with-request
      (niwa-tool-goal--handle-create '(:objective "Ship"))
      (let ((goal (niwa-session-goal session))
            outcome)
        (niwa-pipeline-run-tool-outcome
         (niwa-tool-get "UpdateGoal") (lambda (value) (setq outcome value))
         '(:status "complete"))
        (funcall change goal session)
        (funcall verify-callback '(:status ok :kind fork :verdict pass :result "ok"))
        (should (eq 'error (plist-get outcome :status)))
        (should-not (eq 'complete (niwa-goal-status goal))))))

  :doc "a cancelled call ignores a late verdict"
  (niwa-tool-goal-test--with-request
    (niwa-tool-goal--handle-create '(:objective "Ship"))
    (let ((goal (niwa-session-goal session))
          outcome)
      (niwa-pipeline-run-tool-outcome
       (niwa-tool-get "UpdateGoal") (lambda (value) (setq outcome value))
       '(:status "complete"))
      (mapc #'funcall (niwa-request-cancellers niwa--current-request))
      (let ((cancelled outcome))
        (funcall verify-callback
                 '(:status ok :kind fork :verdict pass :result "ok" :usage 4))
        (should (eq cancelled outcome)))
      (should (eq 'active (niwa-goal-status goal)))
      (should (= 0 (niwa-goal-tokens-used goal)))))

  :doc "completes through ToolCall after the dispatch binding unwinds"
  (niwa-tool-goal-test--with-request
    (niwa-tool-goal--handle-create '(:objective "Ship"))
    (let (outcome)
      (niwa-tool-ptc--handler
       (lambda (value) (setq outcome value))
       '(:expression "(UpdateGoal :status \"complete\")"))
      (let ((deadline (+ (float-time) 5))
            (niwa-tools--current-engine nil))
        (while (and (not verify-callback) (< (float-time) deadline))
          (accept-process-output nil 0.01))
        (funcall verify-callback '(:status ok :kind fork :verdict pass :result "ok"))
        (while (and (not outcome) (< (float-time) deadline))
          (accept-process-output nil 0.01)))
      (should (eq 'success (plist-get outcome :status)))
      (should (eq 'complete (niwa-goal-status (niwa-session-goal session)))))))

(niwa-deftest niwa-tool-goal--verification-prompt ()
  ,test
  (test)
  :doc "states the objective verbatim and omits the plan when there is none"
  (let* ((session (niwa-session--create :name "main"))
         (goal (niwa-goal--create :id "g" :objective "Ship {{x}} *exactly*")))
    (let ((prompt (niwa-tool-goal--verification-prompt session goal)))
      (should (string-search "<objective>\nShip {{x}} *exactly*\n</objective>" prompt))
      (should-not (string-search "accepted-plan" prompt))
      (should (string-search "git status" prompt))))

  :doc "includes the accepted plan verbatim and refuses an unreadable one"
  (let* ((session (niwa-session--create
                   :name "main"
                   :plan-metadata (list :accepted-path "local/plans/accepted-1.md"
                                        :accepted-hash "hash-1")))
         (goal (niwa-goal--create :id "g" :objective "Ship"
                                     :plan-reference "local/plans/accepted-1.md"))
         seen)
    (cl-letf (((symbol-function 'niwa-plan-read-artifact)
               (lambda (_session artifact) (setq seen artifact) "Step 1\nStep 2")))
      (should (string-search "<accepted-plan>\nStep 1\nStep 2\n</accepted-plan>"
                             (niwa-tool-goal--verification-prompt session goal)))
      (should (equal '(:path "local/plans/accepted-1.md" :hash "hash-1") seen)))
    (cl-letf (((symbol-function 'niwa-plan-read-artifact)
               (lambda (&rest _) (error "Accepted plan hash mismatch"))))
      (should (string-search
               "accepted plan is unreadable"
               (error-message-string
                (should-error (niwa-tool-goal--verification-prompt session goal))))))))

(niwa-deftest niwa-tool-goal--verification-failure
  (:doc "names each rejected outcome and carries its report")
  (dolist (case '(((:verdict fail :result "r") "returned FAIL")
                  ((:verification-rejected t :result "r") "no valid verdict")
                  ((:status error :message "r") "did not finish")))
    (let ((reply (niwa-tool-goal--verification-failure (car case))))
      (should (eq 'error (plist-get reply :status)))
      (should (string-search (cadr case) (plist-get reply :result)))
      (should (string-suffix-p "\n\nr" (plist-get reply :result))))))

(niwa-deftest niwa-tool-goal-available-p ()
  ,test
  (test)
  :doc "filters native and catalog Goal tools for root, Plan, and agent contexts"
  (niwa-tool-goal-test--with-request
    (should (equal '("CreateGoal" "GetGoal") (niwa-tool-ptc--roster)))
    (setf (niwa-session-plan-mode session) t)
    (should (equal '("GetGoal") (niwa-tool-ptc--roster)))
    (should-error (niwa-tool-goal--handle-create '(:objective "Forbidden")))
    (setf (niwa-session-plan-mode session) nil)
    (dolist (kind '(ephemeral cancelled))
      (setf (niwa-request-ephemeral-p niwa--current-request) (eq kind 'ephemeral)
            (niwa-request-cancelled-p niwa--current-request) (eq kind 'cancelled))
      (should-not (niwa-tool-goal-available-p "CreateGoal" fsm)))
    (setq-local niwa--agent-invocation (niwa-agent-invocation--create
                                           :parent-session session))
    (dolist (name '("CreateGoal" "GetGoal" "UpdateGoal"))
      (should-not (niwa-tool-goal-available-p name fsm)))
    (should-error (niwa-tool-goal--handle-get nil))
    (setq-local niwa--agent-invocation nil)
    (plist-put (gptel-fsm-info fsm) :context '(:niwa-context-summary t))
    (should-not (niwa-tool-goal-available-p "GetGoal" fsm))
    (should (niwa-tool-goal-available-p "Read" fsm)))

  :doc "refuses every unfinished Goal and either accepted Plan reservation"
  (niwa-tool-goal-test--with-request
    (dolist (status '(active paused blocked budget-limited))
      (setf (niwa-session-goal session) (niwa-goal--create :status status))
      (should-not (niwa-tool-goal-available-p "CreateGoal" fsm)))
    (setf (niwa-session-goal session) nil)
    (setf (niwa-session-plan-metadata session) '(:implementation-retry t))
    (should-not (niwa-tool-goal-available-p "CreateGoal" fsm)))

  :doc "restores native UpdateGoal after creation within the same request"
  (niwa-tool-goal-test--with-request
    (plist-put (gptel-fsm-info fsm) :tools
               (mapcar (lambda (name) (niwa-tool-gptel-tool (niwa-tool-get name)))
                       '("CreateGoal" "GetGoal" "UpdateGoal")))
    (niwa-tools--handle-plan-tool-filter fsm)
    (should (equal '("CreateGoal" "GetGoal")
                   (mapcar #'gptel-tool-name (plist-get (gptel-fsm-info fsm) :tools))))
    (niwa-tool-goal--handle-create '(:objective "Ship"))
    (niwa-tools--handle-plan-tool-filter fsm)
    (should (equal '("GetGoal" "UpdateGoal")
                   (mapcar #'gptel-tool-name (plist-get (gptel-fsm-info fsm) :tools))))
    ;; Re-registered schema objects must not duplicate a stashed name.
    (plist-put (gptel-fsm-info fsm) :tools
               (mapcar #'gptel--copy-tool (plist-get (gptel-fsm-info fsm) :tools)))
    (niwa-tools--handle-plan-tool-filter fsm)
    (should (equal '("GetGoal" "UpdateGoal")
                   (mapcar #'gptel-tool-name (plist-get (gptel-fsm-info fsm) :tools))))))

(niwa-deftest niwa-tool-goal--request ()
  ,test
  (test)
  :doc "rejects stale dispatch and mismatched ownership before mutation"
  (niwa-tool-goal-test--with-request
    (should (eq fsm (niwa-tool-goal--request "GetGoal")))
    (setf (niwa-request-fsm niwa--current-request) (gptel-make-fsm))
    (should-error (niwa-tool-goal--handle-create '(:objective "Stale")))
    (should-not (niwa-session-goal session)))

  :doc "retains the initiating FSM when a permission callback resumes later"
  (niwa-tool-goal-test--with-request
    (let (resume outcome)
      (cl-letf (((symbol-function 'niwa-tool-permission-step)
                 (lambda (context next _fail)
                   (setq resume (apply-partially next context)))))
        (niwa-pipeline-run-tool-outcome
         (niwa-tool-get "GetGoal") (lambda (value) (setq outcome value)) nil))
      (should-not outcome)
      (let ((niwa-tools--current-engine nil)) (funcall resume))
      (should (eq 'success (plist-get outcome :status)))
      (should (equal "{\"goal\":null}" (plist-get outcome :result))))))

(provide 'test-niwa-tool-goal)
;;; test-niwa-tool-goal.el ends here
