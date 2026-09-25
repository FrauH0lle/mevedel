;;; test-mevedel-tool-goal.el --- Goal tools through the pipeline -*- lexical-binding: t -*-

;;; Commentary:
;; Root ownership, creation, inspection, completion and ToolCall discovery.

;;; Code:

(require 'mevedel)
(require 'mevedel-tool-goal)
(require 'mevedel-tool-ptc)
(require 'mevedel-pipeline)
(require 'mevedel-session-codec)
(require 'mevedel-workspace-identity)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(defmacro mevedel-tool-goal-test--with-request (&rest body)
  "Run BODY with an isolated root session, FSM and deferred continuations.
Completion verification is captured: `verify-prompts' lists requested prompts
and `verify-callback' delivers the verifier outcome."
  (declare (indent 0))
  `(let* ((gptel--known-backends nil)
          (mevedel-workspace-identity--cache (make-hash-table :test #'equal))
          (root (make-temp-file "mevedel-goal-tools-" t))
          (session (mevedel-skills-test--make-session "goals" root))
          (scheduled nil)
          (verify-prompts nil)
          (verify-callback nil))
     (unwind-protect
         (with-temp-buffer
           (org-mode)
           (setq-local default-directory (file-name-as-directory root)
                       mevedel--session session)
           (mevedel-workspace-identity-ensure root)
           (setf (mevedel-session-save-path session) root)
           (let* ((fsm (gptel-make-fsm
                        :info (list :buffer (current-buffer)
                                    :backend (gptel-make-openai "goal-test" :key "test" :models '(test))
                                    :data (list :messages [])
                                    :tokens-full '(:input 20 :output 5))))
                  (mevedel-tools--current-fsm fsm))
             (setq-local mevedel--current-request
                         (mevedel-request--create :session session :fsm fsm))
             (mevedel-tool-goal--register)
             (setf (mevedel-session-tool-catalog session)
                   '((("mevedel" "CreateGoal") . "Create goal")
                     (("mevedel" "GetGoal") . "Inspect goal")
                     (("mevedel" "UpdateGoal") . "Complete goal")))
             (cl-letf (((symbol-function 'mevedel-goal--schedule-continuation)
                        (lambda (&rest args) (push args scheduled)))
                       ((symbol-function 'mevedel-review-verify)
                        (lambda (prompt callback)
                          (push prompt verify-prompts)
                          (setq verify-callback callback))))
               ,@body)))
       (delete-directory root t))))

(mevedel-deftest mevedel-tool-goal--handle-create ()
  ,test
  (test)
  :doc "creates through ToolCall, persists, attributes, and continues after settlement"
  (mevedel-tool-goal-test--with-request
    (let (outcome)
      (mevedel-tool-ptc--handler
       (lambda (value) (setq outcome value))
       '(:expression "(CreateGoal :objective \"Fix integration tests\" :token_budget 100)"))
      ;; Nested calls run from timers after the dispatch binding unwinds.
      (let ((deadline (+ (float-time) 5))
            (mevedel-tools--current-fsm nil))
        (while (and (not outcome) (< (float-time) deadline))
          (accept-process-output nil 0.01)))
      (should (eq 'success (plist-get outcome :status)))
      (let* ((goal (mevedel-session-goal session))
             (reported (plist-get (json-parse-string
                                   (plist-get outcome :result) :object-type 'plist)
                                  :goal)))
        (should (equal "Fix integration tests" (mevedel-goal-objective goal)))
        (should (= 100 (mevedel-goal-token-budget goal)))
        (should (= 25 (plist-get reported :tokens_used)))
        (should (equal (mevedel-goal-id goal)
                       (plist-get (gptel-fsm-info fsm) :mevedel-goal-id)))
        (mevedel-context-delivery-stage fsm)
        (let ((entries (plist-get (gptel-fsm-info fsm) :mevedel-reminder-entries)))
          (should (assq 'context-active-goal
                        (mapcar (lambda (entry) (cons (plist-get entry :type) entry)) entries)))
          (should (cl-find 'context-goal-policy entries
                           :key (lambda (entry) (plist-get entry :type)))))
        (should (file-exists-p (file-name-concat root "session.meta.el")))
        (let* ((saved (mevedel-session-codec-read
                       (file-name-concat root "session.meta.el")))
               (restored (mevedel-session-codec-deserialize
                          saved (mevedel-session-workspace session)))
               (restored-goal (mevedel-session-goal (plist-get restored :session))))
          (should (equal (mevedel-goal-id goal) (mevedel-goal-id restored-goal)))
          (should (eq 'paused (mevedel-goal-status restored-goal))))
        (should (eq 'request (mevedel-goal-continue-if-idle session (current-buffer))))
        (mevedel-goal-settle-turn fsm)
        (mevedel-goal-settle-turn fsm)
        (should (= 25 (mevedel-goal-tokens-used goal)))
        (should (= 1 (mevedel-goal-turns-run goal)))
        (mevedel-goal-dispatch-after-turn fsm)
        (should (= 2 (length scheduled)))
        (setq-local mevedel--current-request nil)
        (let (sent)
          (cl-letf (((symbol-function 'mevedel--submit-generated-turn)
                     (lambda (prompt &rest _) (setq sent prompt))))
            (should (eq 'dispatched (mevedel-goal-continue-if-idle
                                     session (current-buffer)))))
          (should (equal mevedel-goal--continuation-trigger sent))))))

  :doc "uses the configured default and rejects invalid input without creating a Goal"
  (mevedel-tool-goal-test--with-request
    (setq-local mevedel-goal-token-budget 250)
    (dolist (args '((:objective "  ")
                    (:objective "Ship" :token_budget 0)
                    (:objective "Ship" :token_budget -1)
                    (:objective "Ship" :token_budget 1.5)))
      (should-error (mevedel-tool-goal--handle-create args))
      (should-not (mevedel-session-goal session)))
    (mevedel-tool-goal--handle-create '(:objective "  Ship  "))
    (should (= 250 (mevedel-goal-token-budget (mevedel-session-goal session))))
    (should (equal "Ship" (mevedel-goal-objective (mevedel-session-goal session))))
    (should-error (mevedel-tool-goal--handle-create '(:objective "Replace"))))

  :doc "replaces a completed same-turn Goal without charging either Goal twice"
  (mevedel-tool-goal-test--with-request
    (mevedel-tool-goal--handle-create '(:objective "First" :token_budget 20))
    (let ((first (mevedel-session-goal session)))
      (mevedel-tool-goal--handle-update #'ignore '(:status "complete"))
      (funcall verify-callback '(:status ok :kind fork :verdict pass :result "ok"))
      (mevedel-tool-goal--handle-create '(:objective "Second"))
      (should (= 25 (mevedel-goal-tokens-used first)))
      (should (= 1 (mevedel-goal-turns-run first)))
      (should-not (mevedel-session-pending-reminders session))
      (plist-put (gptel-fsm-info fsm) :tokens-full '(:input 40 :output 10))
      (mevedel-goal-settle-turn fsm)
      (should (= 25 (mevedel-goal-tokens-used (mevedel-session-goal session))))
      (should (= 1 (mevedel-goal-turns-run (mevedel-session-goal session)))))))

(mevedel-deftest mevedel-tool-goal--handle-get ()
  ,test
  (test)
  :doc "returns no Goal and inspects every status without mutation or dispatch"
  (mevedel-tool-goal-test--with-request
    (should (equal "{\"goal\":null}"
                   (plist-get (mevedel-tool-goal--handle-get nil) :result)))
    (dolist (status '(active paused blocked budget-limited complete))
      (let ((goal (mevedel-goal--create
                   :id "g" :objective "Ship" :status status :reason "Reason"
                   :token-budget 100 :tokens-used 70 :time-used-seconds 12
                   :turns-run 2 :plan-reference "local/plans/accepted-1.md")))
        (setf (mevedel-session-goal session) goal)
        (let* ((before (copy-sequence goal))
               (value (plist-get
                       (json-parse-string
                        (plist-get (mevedel-tool-goal--handle-get nil) :result)
                        :object-type 'plist)
                       :goal)))
          (should (equal (symbol-name status) (plist-get value :status)))
          (should (= 70 (plist-get value :tokens_used)))
          (should (= 30 (plist-get value :remaining_tokens)))
          (should (equal "work://plans/accepted-1.md" (plist-get value :plan_reference)))
          (should (equal before goal)))))
    ;; Even a Goal stopped for an invalid plan reference remains inspectable.
    (let ((goal (mevedel-session-goal session)))
      (setf (mevedel-goal-status goal) 'paused
            (mevedel-goal-plan-reference goal) "invalid-plan.md")
      (let ((value (plist-get (json-parse-string
                               (plist-get (mevedel-tool-goal--handle-get nil) :result)
                               :object-type 'plist) :goal)))
        (should (equal "paused" (plist-get value :status)))
        (should (stringp (plist-get value :plan_reference_error)))
        (should (eq 'paused (mevedel-goal-status goal)))))
    (should-not scheduled))

  :doc "includes known live usage once, and never charges a different Goal"
  (mevedel-tool-goal-test--with-request
    (mevedel-tool-goal--handle-create '(:objective "Ship" :token_budget 100))
    (dolist (settle '(nil t))
      (when settle (mevedel-goal-settle-turn fsm))
      (let ((value (plist-get (json-parse-string
                               (plist-get (mevedel-tool-goal--handle-get nil) :result)
                               :object-type 'plist) :goal)))
        (should (= 25 (plist-get value :tokens_used)))
        (should (= 75 (plist-get value :remaining_tokens)))))
    (setf (mevedel-goal-id (mevedel-session-goal session)) "different")
    (plist-put (gptel-fsm-info fsm) :tokens-full '(:input 999))
    (let ((value (plist-get (json-parse-string
                             (plist-get (mevedel-tool-goal--handle-get nil) :result)
                             :object-type 'plist) :goal)))
      (should (= 25 (plist-get value :tokens_used))))))

(mevedel-deftest mevedel-tool-goal--handle-update ()
  ,test
  (test)
  :doc "completes only after the verifier passes and charges its usage"
  (mevedel-tool-goal-test--with-request
    (mevedel-tool-goal--handle-create '(:objective "Ship the parser"))
    (let ((goal (mevedel-session-goal session))
          outcome)
      (mevedel-pipeline-run-tool-outcome
       (mevedel-tool-get "UpdateGoal") (lambda (value) (setq outcome value))
       '(:status "complete"))
      (should-not outcome)
      (should (eq 'active (mevedel-goal-status goal)))
      (should (string-search "Ship the parser" (car verify-prompts)))
      (funcall verify-callback
               '(:status ok :kind fork :verdict pass :result "VERDICT: PASS" :usage 7))
      (should (eq 'success (plist-get outcome :status)))
      (should (equal "Goal status changed to complete" (plist-get outcome :result)))
      (should (eq 'complete (mevedel-goal-status goal)))
      (should (= 7 (mevedel-goal-tokens-used goal)))
      (mevedel-goal-settle-turn fsm)
      (should (= 32 (mevedel-goal-tokens-used goal)))
      (should (eq 'inactive (mevedel-goal-continue-if-idle session (current-buffer))))))

  :doc "returns every non-passing verdict as findings and keeps the Goal active"
  (mevedel-tool-goal-test--with-request
    (mevedel-tool-goal--handle-create '(:objective "Ship"))
    (let ((goal (mevedel-session-goal session)))
      (dolist (case '(((:status ok :kind fork :verdict fail :result "Parser crashes" :usage 5)
                       "returned FAIL" "Parser crashes")
                      ((:status ok :kind fork :verdict partial :result "No network" :usage 5)
                       "returned PARTIAL" "No network")
                      ((:status ok :kind fork :verification-rejected t :result "rambling" :usage 5)
                       "no valid verdict" "rambling")
                      ((:status error :reason agent-errored :message "Provider failed" :usage 5)
                       "did not finish" "Provider failed")))
        (let (outcome)
          (mevedel-pipeline-run-tool-outcome
           (mevedel-tool-get "UpdateGoal") (lambda (value) (setq outcome value))
           '(:status "complete"))
          (funcall verify-callback (car case))
          (should (eq 'error (plist-get outcome :status)))
          (should (string-search "Goal remains active" (plist-get outcome :result)))
          (should (string-search (nth 1 case) (plist-get outcome :result)))
          (should (string-search (nth 2 case) (plist-get outcome :result)))
          (should (eq 'active (mevedel-goal-status goal)))))
      (should (= 20 (mevedel-goal-tokens-used goal)))))

  :doc "blocks immediately without verification"
  (mevedel-tool-goal-test--with-request
    (mevedel-tool-goal--handle-create '(:objective "Ship"))
    (let (outcome)
      (mevedel-tool-goal--handle-update
       (lambda (value) (setq outcome value))
       '(:status "blocked" :summary "Needs the production API key"))
      (should (equal "Goal status changed to blocked" (plist-get outcome :result)))
      (should-not verify-prompts)
      (should (eq 'blocked (mevedel-goal-status (mevedel-session-goal session))))))

  :doc "refuses to verify against an unreadable accepted plan"
  (mevedel-tool-goal-test--with-request
    (mevedel-tool-goal--handle-create '(:objective "Ship"))
    (setf (mevedel-goal-plan-reference (mevedel-session-goal session))
          "local/plans/accepted-1.md")
    (let (outcome)
      (mevedel-pipeline-run-tool-outcome
       (mevedel-tool-get "UpdateGoal") (lambda (value) (setq outcome value))
       '(:status "complete"))
      (should (string-search "accepted plan is unreadable" (plist-get outcome :result)))
      (should-not verify-prompts)
      (should (eq 'active (mevedel-goal-status (mevedel-session-goal session))))))

  :doc "a pass cannot complete a Goal edited, paused, or cleared meanwhile"
  (dolist (change (list (lambda (goal _session) (setf (mevedel-goal-id goal) "edited"))
                        (lambda (goal _session) (setf (mevedel-goal-status goal) 'paused))
                        (lambda (_goal session) (setf (mevedel-session-goal session) nil))))
    (mevedel-tool-goal-test--with-request
      (mevedel-tool-goal--handle-create '(:objective "Ship"))
      (let ((goal (mevedel-session-goal session))
            outcome)
        (mevedel-pipeline-run-tool-outcome
         (mevedel-tool-get "UpdateGoal") (lambda (value) (setq outcome value))
         '(:status "complete"))
        (funcall change goal session)
        (funcall verify-callback '(:status ok :kind fork :verdict pass :result "ok"))
        (should (eq 'error (plist-get outcome :status)))
        (should-not (eq 'complete (mevedel-goal-status goal))))))

  :doc "a cancelled call only charges a late verdict"
  (mevedel-tool-goal-test--with-request
    (mevedel-tool-goal--handle-create '(:objective "Ship"))
    (let ((goal (mevedel-session-goal session))
          outcome)
      (mevedel-pipeline-run-tool-outcome
       (mevedel-tool-get "UpdateGoal") (lambda (value) (setq outcome value))
       '(:status "complete"))
      (mapc #'funcall (mevedel-request-cancellers mevedel--current-request))
      (let ((cancelled outcome))
        (funcall verify-callback
                 '(:status ok :kind fork :verdict pass :result "ok" :usage 4))
        (should (eq cancelled outcome)))
      (should (eq 'active (mevedel-goal-status goal)))
      (should (= 4 (mevedel-goal-tokens-used goal)))))

  :doc "completes through ToolCall after the dispatch binding unwinds"
  (mevedel-tool-goal-test--with-request
    (mevedel-tool-goal--handle-create '(:objective "Ship"))
    (let (outcome)
      (mevedel-tool-ptc--handler
       (lambda (value) (setq outcome value))
       '(:expression "(UpdateGoal :status \"complete\")"))
      (let ((deadline (+ (float-time) 5))
            (mevedel-tools--current-fsm nil))
        (while (and (not verify-callback) (< (float-time) deadline))
          (accept-process-output nil 0.01))
        (funcall verify-callback '(:status ok :kind fork :verdict pass :result "ok"))
        (while (and (not outcome) (< (float-time) deadline))
          (accept-process-output nil 0.01)))
      (should (eq 'success (plist-get outcome :status)))
      (should (eq 'complete (mevedel-goal-status (mevedel-session-goal session)))))))

(mevedel-deftest mevedel-tool-goal--verification-prompt ()
  ,test
  (test)
  :doc "states the objective verbatim and omits the plan when there is none"
  (let* ((session (mevedel-session--create :name "main"))
         (goal (mevedel-goal--create :id "g" :objective "Ship {{x}} *exactly*")))
    (let ((prompt (mevedel-tool-goal--verification-prompt session goal)))
      (should (string-search "<objective>\nShip {{x}} *exactly*\n</objective>" prompt))
      (should-not (string-search "accepted-plan" prompt))
      (should (string-search "git status" prompt))))

  :doc "includes the accepted plan verbatim and refuses an unreadable one"
  (let* ((session (mevedel-session--create
                   :name "main"
                   :plan-metadata (list :accepted-path "local/plans/accepted-1.md"
                                        :accepted-hash "hash-1")))
         (goal (mevedel-goal--create :id "g" :objective "Ship"
                                     :plan-reference "local/plans/accepted-1.md"))
         seen)
    (cl-letf (((symbol-function 'mevedel-plan-read-artifact)
               (lambda (_session artifact) (setq seen artifact) "Step 1\nStep 2")))
      (should (string-search "<accepted-plan>\nStep 1\nStep 2\n</accepted-plan>"
                             (mevedel-tool-goal--verification-prompt session goal)))
      (should (equal '(:path "local/plans/accepted-1.md" :hash "hash-1") seen)))
    (cl-letf (((symbol-function 'mevedel-plan-read-artifact)
               (lambda (&rest _) (error "Accepted plan hash mismatch"))))
      (should (string-search
               "accepted plan is unreadable"
               (error-message-string
                (should-error (mevedel-tool-goal--verification-prompt session goal))))))))

(mevedel-deftest mevedel-tool-goal--verification-failure
  (:doc "names each non-passing outcome and carries its report")
  (dolist (case '(((:verdict fail :result "r") "returned FAIL")
                  ((:verdict partial :result "r") "returned PARTIAL")
                  ((:verification-rejected t :result "r") "no valid verdict")
                  ((:status error :message "r") "did not finish")))
    (let ((reply (mevedel-tool-goal--verification-failure (car case))))
      (should (eq 'error (plist-get reply :status)))
      (should (string-search (cadr case) (plist-get reply :result)))
      (should (string-suffix-p "\n\nr" (plist-get reply :result))))))

(mevedel-deftest mevedel-tool-goal-available-p ()
  ,test
  (test)
  :doc "filters native and catalog Goal tools for root, Plan, and agent contexts"
  (mevedel-tool-goal-test--with-request
    (should (equal '("CreateGoal" "GetGoal") (mevedel-tool-ptc--roster)))
    (setf (mevedel-session-plan-mode session) t)
    (should (equal '("GetGoal") (mevedel-tool-ptc--roster)))
    (should-error (mevedel-tool-goal--handle-create '(:objective "Forbidden")))
    (setf (mevedel-session-plan-mode session) nil)
    (dolist (kind '(ephemeral cancelled directive))
      (setf (mevedel-request-ephemeral-p mevedel--current-request) (eq kind 'ephemeral)
            (mevedel-request-cancelled-p mevedel--current-request) (eq kind 'cancelled)
            (mevedel-request-directive-uuid mevedel--current-request)
            (and (eq kind 'directive) "directive"))
      (should-not (mevedel-tool-goal-available-p "CreateGoal" fsm)))
    (setf (mevedel-request-directive-uuid mevedel--current-request) nil)
    (setq-local mevedel--agent-invocation (mevedel-agent-invocation--create
                                           :parent-session session))
    (dolist (name '("CreateGoal" "GetGoal" "UpdateGoal"))
      (should-not (mevedel-tool-goal-available-p name fsm)))
    (should-error (mevedel-tool-goal--handle-get nil))
    (setq-local mevedel--agent-invocation nil)
    (plist-put (gptel-fsm-info fsm) :context '(:mevedel-context-summary t))
    (should-not (mevedel-tool-goal-available-p "GetGoal" fsm))
    (should (mevedel-tool-goal-available-p "Read" fsm)))

  :doc "refuses every unfinished Goal and either accepted Plan reservation"
  (mevedel-tool-goal-test--with-request
    (dolist (status '(active paused blocked budget-limited))
      (setf (mevedel-session-goal session) (mevedel-goal--create :status status))
      (should-not (mevedel-tool-goal-available-p "CreateGoal" fsm)))
    (setf (mevedel-session-goal session) nil)
    (dolist (metadata '((:implementation-retry t) (:implementation-goal-id "reserved")))
      (setf (mevedel-session-plan-metadata session) metadata)
      (should-not (mevedel-tool-goal-available-p "CreateGoal" fsm))))

  :doc "restores native UpdateGoal after creation within the same request"
  (mevedel-tool-goal-test--with-request
    (plist-put (gptel-fsm-info fsm) :tools
               (mapcar (lambda (name) (mevedel-tool-gptel-tool (mevedel-tool-get name)))
                       '("CreateGoal" "GetGoal" "UpdateGoal")))
    (mevedel-tools--handle-plan-tool-filter fsm)
    (should (equal '("CreateGoal" "GetGoal")
                   (mapcar #'gptel-tool-name (plist-get (gptel-fsm-info fsm) :tools))))
    (mevedel-tool-goal--handle-create '(:objective "Ship"))
    (mevedel-tools--handle-plan-tool-filter fsm)
    (should (equal '("GetGoal" "UpdateGoal")
                   (mapcar #'gptel-tool-name (plist-get (gptel-fsm-info fsm) :tools))))
    ;; Re-registered schema objects must not duplicate a stashed name.
    (plist-put (gptel-fsm-info fsm) :tools
               (mapcar #'gptel--copy-tool (plist-get (gptel-fsm-info fsm) :tools)))
    (mevedel-tools--handle-plan-tool-filter fsm)
    (should (equal '("GetGoal" "UpdateGoal")
                   (mapcar #'gptel-tool-name (plist-get (gptel-fsm-info fsm) :tools))))))

(mevedel-deftest mevedel-tool-goal--request ()
  ,test
  (test)
  :doc "rejects stale dispatch and mismatched ownership before mutation"
  (mevedel-tool-goal-test--with-request
    (should (eq fsm (mevedel-tool-goal--request "GetGoal")))
    (setf (mevedel-request-fsm mevedel--current-request) (gptel-make-fsm))
    (should-error (mevedel-tool-goal--handle-create '(:objective "Stale")))
    (should-not (mevedel-session-goal session)))

  :doc "retains the initiating FSM when a permission callback resumes later"
  (mevedel-tool-goal-test--with-request
    (let (resume outcome)
      (cl-letf (((symbol-function 'mevedel-tool-permission-step)
                 (lambda (context next _fail)
                   (setq resume (apply-partially next context)))))
        (mevedel-pipeline-run-tool-outcome
         (mevedel-tool-get "GetGoal") (lambda (value) (setq outcome value)) nil))
      (should-not outcome)
      (let ((mevedel-tools--current-fsm nil)) (funcall resume))
      (should (eq 'success (plist-get outcome :status)))
      (should (equal "{\"goal\":null}" (plist-get outcome :result))))))

(provide 'test-mevedel-tool-goal)
;;; test-mevedel-tool-goal.el ends here
