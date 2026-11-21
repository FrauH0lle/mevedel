;;; test-niwa-plan-handoff-owner.el -- Published kickoff ownership -*- lexical-binding: t -*-

;;; Commentary:

;; Real Goal publication, handoff completion saves
;; and generated request startup.  Timers run user Goal commands while the
;; publication save is still on the stack; only provider transport is faked.

;;; Code:

(require 'niwa-plan-handoff-test-support
         (file-name-concat
          (file-name-directory (or load-file-name buffer-file-name))
          "niwa-plan-handoff-test-support"))

(niwa-deftest niwa-plan-handoff--dispatch-submission/published-owner (:quiet t)
  ,test
  (test)
  :doc "publication callbacks cannot transfer or pause stale kickoff authority"
  (progn
    (let ((stage 'publication))
      (dolist (change '(pause clear replace edit policy request))
        (niwa-handoff-owner-test--with-target
          (let ((save (symbol-function 'niwa-session-artifacts-save))
                changed owner replacement admitted sent)
            (cl-letf (((symbol-function 'niwa-auth-assert-ready) #'ignore)
                      ((symbol-function 'gptel-curl-get-response)
                       (lambda (fsm) (push fsm sent)))
                      ((symbol-function 'niwa-session-artifacts-save)
                       (lambda (saving-session saving-buffer)
                         (prog1 (funcall save saving-session saving-buffer)
                           (when (and (not changed)
                                      (niwa-session-goal session)
                                      (and (eq saving-session session)
                                               (not (plist-member (niwa-session-plan-metadata session)
                                                                  :implementation-retry))))
                             (setq changed t owner (niwa-session-goal session))
                             (run-at-time
                              0 nil
                              (lambda ()
                                (with-current-buffer buffer
                                  (should-not niwa--pending-implementation-policy)
                                  (pcase change
                                    ('pause (niwa-goal-pause))
                                    ('clear (niwa-goal-clear))
                                    ('replace
                                     (niwa-goal-clear)
                                     (setq replacement
                                           (niwa-goal-create
                                            "Unrelated replacement" :session session
                                            :id "reserved-kickoff" :implementation-policy policy)))
                                    ('edit (niwa-goal-edit "Revised kickoff"))
                                    ('request (setq admitted (niwa-request-begin session)))
                                    ('policy
                                     (setf (niwa-goal-implementation-policy owner)
                                           '(:model-provider "Balanced:balanced-model"
                                             :reasoning-effort high :permission-mode edits))
                                     (funcall save session buffer))))))
                             (accept-process-output nil .02))))))
              (niwa-plan-handoff--dispatch-submission
               session buffer record selection accepted
               "Implement" submission))
            (should changed)
            (should-not sent)
            (should (eq admitted niwa--current-request))
            (when admitted
              (should-not (niwa-request-cancelled-p admitted)))
            (should-not niwa--pending-implementation-policy)
            (should (eq 'pending (niwa-prompt-submission-state submission)))
            (let* ((current (niwa-session-goal session))
                   (saved (niwa-session-codec-read
                           (niwa-session-artifacts-sidecar-path
                            (niwa-session-save-path session)))))
              (should (eq (pcase change ('clear nil) ('pause 'paused) (_ 'active))
                          (and current (niwa-goal-status current))))
              (when replacement
                (should (eq replacement current))
                (should-not (eq owner current)))
              (should (eq (and current (niwa-goal-status current))
                          (plist-get (plist-get saved :goal) :status)))
            (when admitted (niwa-request-end t))))))))

  :doc "a real provider startup failure pauses its owner but never replacement authority"
  (progn
   (dolist (change '(none replace))
    (niwa-handoff-owner-test--with-target
      (let (sent replacement owner expected-status)
        (cl-letf (((symbol-function 'niwa-auth-assert-ready) #'ignore)
                  ((symbol-function 'gptel-curl-get-response)
                   (lambda (fsm)
                     (push fsm sent)
                     (with-current-buffer buffer
                       (setq owner (niwa-session-goal session))
                       (when (eq change 'replace)
                         (niwa-goal-clear)
                         (setq replacement
                               (niwa-goal-create
                                "Unrelated startup replacement" :session session
                                :implementation-policy policy)))
                       (setq expected-status (if (eq change 'none) 'paused 'active)))
                     (error "Provider startup refused"))))
          (niwa-plan-handoff--dispatch-submission
           session buffer record selection accepted
           "Implement" submission))
        (should (= 1 (length sent)))
        (let ((current (niwa-session-goal session)))
          (should (eq expected-status (niwa-goal-status current)))
          (when replacement
            (should (eq replacement current))
            (should-not (eq owner current)))
          (unless (eq change 'none)
            (should-not (niwa-goal-reason current))))
        ;; The fake transport threw before installing a network producer.
        ;; Abort its retained FSM to finish fixture settlement, without starting
        ;; a separate automatic turn for the newly active replacement Goal.
        (cl-letf (((symbol-function 'niwa-goal-schedule-continuation) #'ignore))
          (gptel--fsm-transition (car sent) 'ABRT)
          (niwa-test--await 5 "Failed kickoff did not settle"
            (not (niwa-turn-busy-p buffer))))
        (let* ((goal (niwa-session-goal session))
               (saved (niwa-session-codec-read
                       (niwa-session-artifacts-sidecar-path
                        (niwa-session-save-path session)))))
          (should (eq expected-status (niwa-goal-status goal)))
          (when (eq change 'none)
            (should (equal "reserved-kickoff" (niwa-goal-id goal)))
            (should (string-search "Provider startup refused" (niwa-goal-reason goal))))
          (should (eq expected-status (plist-get (plist-get saved :goal) :status)))))))))

(niwa-deftest niwa-plan-handoff--dispatch-submission/owner-failures (:quiet t)
  ,test
  (test)
  :doc "owned publication failures pause durably; insertion staleness preserves replacement"
  (progn
    (dolist (stage '(publication insertion))
      (niwa-handoff-owner-test--with-target
        (let ((save (symbol-function 'niwa-session-artifacts-save))
              changed owner replacement)
          (when (eq stage 'insertion)
            (setq-local after-change-functions
                        (list (lambda (&rest _)
                                (when (and niwa--pending-implementation-policy (not changed))
                                  (setq changed t owner (niwa-session-goal session))
                                  (niwa-goal-clear)
                                  (setq replacement
                                        (niwa-goal-create "Replacement during insertion"
                                                             :session session
                                                             :implementation-policy policy)))))))
          (cl-letf (((symbol-function 'gptel-curl-get-response)
                     (lambda (&rest _) (ert-fail "Stale or failed publication started a provider")))
                    ((symbol-function 'niwa-session-artifacts-save)
                     (lambda (saving-session saving-buffer)
                       (prog1 (funcall save saving-session saving-buffer)
                         (when (and (not changed) (not (eq stage 'insertion))
                                    (niwa-session-goal session)
                                    (and (eq saving-session session)
                                             (not (plist-member (niwa-session-plan-metadata session)
                                                                :implementation-retry))))
                           (setq changed t owner (niwa-session-goal session))
                           (error "Kickoff publication refused"))))))
            (niwa-plan-handoff--dispatch-submission
             session buffer record selection accepted
             "Implement" submission))
          (should changed)
          (should-not niwa--current-request)
          (should-not niwa--pending-implementation-policy)
          (let* ((current (niwa-session-goal session))
                 (saved (niwa-session-codec-read
                         (niwa-session-artifacts-sidecar-path
                          (niwa-session-save-path session)))))
            (if replacement
                (progn
                  (should (eq replacement current))
                  (should-not (eq owner current))
                  (should (eq 'active (niwa-goal-status current)))
                  (should-not (niwa-goal-reason current)))
              (should (eq owner current))
              (should (eq 'paused (niwa-goal-status current)))
              (should (string-search "publication refused" (niwa-goal-reason current))))
            (should (eq (niwa-goal-status current)
                        (plist-get (plist-get saved :goal) :status)))))))))

(niwa-deftest niwa--implement-plan/direct-busy-owner (:quiet t)
  ,test
  (test)
  :doc "public Direct retry cannot seize a newer generated-send FSM admitted during publication"
  (niwa-handoff-owner-test--with-target
    (setf (plist-get selection :execution) 'direct)
    (let ((view (generate-new-buffer " *direct-owner-view*"))
          (save (symbol-function 'niwa-session-artifacts-save))
          (accept (symbol-function 'niwa-prompt-submission-accept))
          changed sent hook-owners newer newer-fsm newer-text accepted-submission
          observations)
      (unwind-protect
          (save-window-excursion
            (niwa-view--setup view buffer)
            (set-window-buffer (selected-window) view)
            (setq-local gptel-post-request-hook
                        (list (lambda () (push niwa--current-request hook-owners))))
            (cl-letf (((symbol-function 'niwa-auth-assert-ready) #'ignore)
                      ((symbol-function 'gptel-curl-get-response)
                       (lambda (fsm) (push fsm sent)))
                      ((symbol-function 'niwa-prompt-submission-accept)
                       (lambda (transaction &rest args)
                         (setq accepted-submission transaction)
                         (apply accept transaction args)))
                      ((symbol-function 'niwa-session-artifacts-save)
                       (lambda (saving-session saving-buffer)
                         (prog1 (funcall save saving-session saving-buffer)
                           (when (and (not changed) (eq saving-session session))
                             (setq changed t)
                             ;; Real publication can yield to user input before
                             ;; the accepted handoff enters --implement-plan.
                             (run-at-time
                              0 nil
                              (lambda ()
                                (with-current-buffer buffer
                                  (setq newer-fsm
                                        (niwa--submit-generated-turn "Newer ordinary request")
                                        newer niwa--current-request)
                                  (should (eq 'WAIT (gptel-fsm-state newer-fsm)))
                                  (should (eq newer-fsm (niwa-request-fsm newer)))
                                  (niwa-model-set-session-effort session 'low buffer)
                                  ;; Serialize gptel's bounds before comparing
                                  ;; bytes; retry failure also performs a save.
                                  (funcall save session buffer)
                                  (setq newer-text (buffer-string)))))
                             (niwa-test--await 5 "Competing Direct request did not start"
                               newer-text))))))
              (niwa-retry-plan-implementation session buffer)
              ;; Save evidence before settling transports, even on the red run,
              ;; so fixture cleanup does not hide the ownership or policy leak.
              (setq observations
                    (list :sent (length sent) :hooks (length hook-owners)
                          :request niwa--current-request :fsm (niwa-request-fsm newer)
                          :text (buffer-string)
                          :submission-state (niwa-prompt-submission-state accepted-submission)
                          :pending niwa--pending-implementation-policy
                          :pending-mode niwa--pending-permission-mode
                          :context niwa-skills--pending-request-context))
              (dolist (fsm sent) (gptel--fsm-transition fsm 'ABRT))
              (niwa-test--await 5 "Newer Direct owner fixture did not settle"
                (not (niwa-turn-busy-p buffer)))
              (let ((ordinary (niwa--submit-generated-turn "Ordinary follow-up")))
                (setq observations
                      (append observations
                              (list :ordinary-model (plist-get (gptel-fsm-info ordinary) :model)
                                    :ordinary-effort (plist-get (gptel-fsm-info ordinary) :reasoning-effort)
                                    :ordinary-policy (niwa-request-implementation-policy niwa--current-request)
                                    :ordinary-mode (niwa-request-permission-mode-override niwa--current-request))))
                (gptel--fsm-transition ordinary 'ABRT)
                (niwa-test--await 5 "Direct owner follow-up did not settle"
                  (not (niwa-turn-busy-p buffer))))))
        (with-current-buffer view (niwa-view-stream-stop))
        (kill-buffer view))
      (should changed)
      (ert-info ((format "Direct owner: sends=%S, FSM preserved=%S, pending=%S, ordinary=%S/%S, override=%S"
                         (plist-get observations :sent)
                         (eq newer-fsm (plist-get observations :fsm))
                         (plist-get observations :pending)
                         (plist-get observations :ordinary-model)
                         (plist-get observations :ordinary-effort)
                         (plist-get observations :ordinary-mode)))
        (should (= 1 (plist-get observations :sent))))
      (should (= 1 (plist-get observations :hooks)))
      (should (eq newer (plist-get observations :request)))
      (should (eq newer-fsm (plist-get observations :fsm)))
      (should (equal newer-text (plist-get observations :text)))
      (should (eq 'pending (plist-get observations :submission-state)))
      (should-not (niwa-request-implementation-policy newer))
      (should-not (niwa-request-permission-mode-override newer))
      (should-not (plist-get (gptel-fsm-info newer-fsm) :niwa-plan-handoff-source-buffer))
      (should-not (plist-get observations :pending))
      (should-not (plist-get observations :pending-mode))
      (should-not (plist-get observations :context))
      (should (eq 'fast-model (plist-get observations :ordinary-model)))
      (should (eq 'low (plist-get observations :ordinary-effort)))
      (should-not (plist-get observations :ordinary-policy))
      (should-not (plist-get observations :ordinary-mode))
      (should (equal "Fast:fast-model" (niwa-session-model-provider session)))
      (should (eq 'low (niwa-session-reasoning-effort session)))
      (should (eq 'ask (niwa-session-permission-mode session)))))

  :doc "Direct cannot seize a post-request or still-settling owner"
  (progn
    (dolist (stage '(post-request settlement))
      (niwa-handoff-owner-test--with-target
        (setf (plist-get selection :execution) 'direct)
        (let ((persist (symbol-function 'niwa-goal-persist-failure))
              attempted sent newer-fsm newer observations)
          (cl-labels
              ((attempt (fsm)
                 (setq attempted t)
                 ;; Prime legitimate save metadata so exact byte comparison
                 ;; detects insertion rather than first-time bounds encoding.
                 (niwa-session-artifacts-save session buffer)
                 (let ((text (buffer-string))
                       (pending niwa--turn-settlements-pending)
                       (cancelled (niwa-request-cancelled-p newer))
                       (info (copy-sequence (gptel-fsm-info fsm))))
                   (if (eq stage 'settlement)
                       (progn
                         (should-not niwa--current-request)
                         (should (memq fsm pending)))
                     (should (eq newer niwa--current-request))
                     (should (eq fsm (niwa-request-fsm newer))))
                   (niwa-plan-handoff--dispatch-submission
                    session buffer record selection accepted
                    "Implement" submission)
                   (setq observations
                         (list :text-unchanged (equal text (buffer-string))
                               :info-unchanged (equal info (gptel-fsm-info fsm))
                               :settlements-unchanged (eq pending niwa--turn-settlements-pending)
                               :request niwa--current-request
                               :cancelled-unchanged (eq cancelled (niwa-request-cancelled-p newer))
                               :sends (length sent)
                               :policy niwa--pending-implementation-policy)))))
            (setq-local gptel-post-request-hook
                        (list (lambda ()
                                (setq newer niwa--current-request)
                                (when (eq stage 'post-request)
                                  (attempt (niwa-request-fsm newer))))))
            (cl-letf (((symbol-function 'niwa-auth-assert-ready) #'ignore)
                      ((symbol-function 'gptel-curl-get-response)
                       (lambda (fsm) (push fsm sent)))
                      ((symbol-function 'niwa-goal-persist-failure)
                       (lambda (fsm)
                         (prog1 (funcall persist fsm)
                           (when (and (eq stage 'settlement) (eq fsm newer-fsm) (not attempted))
                             (with-current-buffer buffer (attempt fsm)))))))
              (setq newer-fsm (niwa--submit-generated-turn "Newer owner"))
              (setq-local gptel-post-request-hook nil)
              (gptel--fsm-transition newer-fsm 'ABRT)
              (niwa-test--await 5 "Direct busy-owner callback did not settle"
                (not (niwa-turn-busy-p buffer)))
              (should attempted)
              (should (plist-get observations :text-unchanged))
              (should (plist-get observations :info-unchanged))
              (should (plist-get observations :settlements-unchanged))
              (should (eq (and (eq stage 'post-request) newer)
                          (plist-get observations :request)))
              (should (plist-get observations :cancelled-unchanged))
              (should (= 1 (plist-get observations :sends)))
              (should-not (plist-get observations :policy))
              (should (eq 'pending (niwa-prompt-submission-state submission)))
              (should-not niwa--pending-implementation-policy)
              (should-not niwa--pending-permission-mode)
              (should-not niwa-skills--pending-request-context)
              (let ((ordinary (niwa--submit-generated-turn "Ordinary after rejected Direct")))
                (should (eq 'fast-model (plist-get (gptel-fsm-info ordinary) :model)))
                (should (eq 'high (plist-get (gptel-fsm-info ordinary) :reasoning-effort)))
                (should-not (niwa-request-implementation-policy niwa--current-request))
                (should-not (niwa-request-permission-mode-override niwa--current-request))
                (gptel--fsm-transition ordinary 'ABRT)
                (niwa-test--await 5 "Direct callback follow-up did not settle"
                  (not (niwa-turn-busy-p buffer)))))))))))

(niwa-deftest niwa-plan-handoff--dispatch-submission/joined-authority (:quiet t)
  ,test
  (test)
  :doc "Direct/Goal join accepted provider and actual authority on one admitted request"
  (progn
    (dolist (execution '(direct goal))
      (ert-info ((format "Accepted handoff: %s" execution))
        (niwa-handoff-owner-test--with-target
          (setf (plist-get selection :execution) execution)
          ;; Ordinary selections differ from every accepted implementation axis.
          (niwa-model-set-session-provider
           session (niwa-model-resolve-provider "Fast:fast-model") buffer)
          (niwa-model-set-session-effort session 'high buffer)
          (with-current-buffer buffer
            (niwa-permission-mode-set-raw 'ask)
            (setf (niwa-session-sandbox-mode session) 'best-effort)
            (setq-local niwa-goal-token-budget 4321 niwa-model-workloads nil))
          (let ((edit-tool (niwa-tool--create :name "AcceptedHandoffEdit" :groups '(edit)))
                (command "make accepted-handoff-check")
                (global-budget (default-value 'niwa-goal-token-budget))
                (niwa-permission-rules nil)
                sent)
            (cl-labels
                ((baseline (owner data)
                   (list (niwa-session-model-provider owner)
                         (niwa-session-reasoning-effort owner)
                         (niwa-session-permission-mode owner)
                         (niwa-session-sandbox-mode owner)
                         (buffer-local-value 'gptel-backend data)
                         (buffer-local-value 'gptel-model data)
                         (buffer-local-value 'gptel-reasoning-effort data)
                         (buffer-local-value 'niwa-permission-mode data)
                         (buffer-local-value 'niwa-goal-token-budget data)))
                 (authority (admitted mode outcome confinement)
                   ;; No synthetic :mode: resolve this production request owner.
                   (let ((context (list :session session :buffer buffer
                                        :request admitted :workspace workspace)))
                     (should-not (plist-member context :mode))
                     (should (eq mode (niwa-bash-policy-effective-permission-mode context)))
                     (should (eq confinement (niwa-bash-policy-effective-sandbox-mode context)))
                     (should (eq outcome (niwa-bash-policy-check-permission
                                          command :permission-context context)))
                     (should (eq outcome
                                 (niwa-check-permission
                                  "AcceptedHandoffEdit" :tool-struct edit-tool
                                  :session session :request admitted
                                  :session-rules (niwa-session-permission-rules session))))))
                 (durable-goal (owner status)
                   (should (eq owner (niwa-session-goal session)))
                   (should (eq status (niwa-goal-status owner)))
                   (should (equal policy (niwa-goal-implementation-policy owner)))
                   (should (= 200 (niwa-goal-token-budget owner)))
                   (let* ((saved (niwa-session-codec-read
                                  (niwa-session-artifacts-sidecar-path
                                   (niwa-session-save-path session))))
                          (saved-goal (plist-get saved :goal)))
                     (should (equal (niwa-goal-id owner) (plist-get saved-goal :id)))
                     (should (eq status (plist-get saved-goal :status)))
                     (should (equal policy (plist-get saved-goal :implementation-policy)))
                     (should (= 200 (plist-get saved-goal :token-budget))))))
              (let ((source-before (baseline session buffer))
                    (target-before (baseline session buffer)))
                (cl-letf (((symbol-function 'niwa-auth-assert-ready) #'ignore)
                          ((symbol-function 'gptel-curl-get-response)
                           (lambda (fsm) (push fsm sent))))
                  (niwa-plan-handoff--dispatch-submission
                   session buffer record selection accepted
                   "Implement" submission)
                  (should (= 1 (length sent)))
                  (let* ((fsm (car sent))
                         (info (gptel-fsm-info fsm))
                         (admitted niwa--current-request)
                         (goal (niwa-session-goal session)))
                    (should (eq 'WAIT (gptel-fsm-state fsm)))
                    (should (niwa-request-p admitted))
                    (should (eq session (niwa-request-session admitted)))
                    (should (eq buffer (plist-get info :buffer)))
                    (should (eq fsm (niwa-request-fsm admitted)))
                    (should (eq (gptel-get-backend "Balanced") (plist-get info :backend)))
                    (should (eq 'balanced-model (plist-get info :model)))
                    (should (plist-member info :reasoning-effort))
                    (should-not (plist-get info :reasoning-effort))
                    (should (equal policy (niwa-request-implementation-policy admitted)))
                    (should (eq 'full-auto (niwa-request-permission-mode-override admitted)))
                    (should-not niwa--pending-implementation-policy)
                    (should-not niwa--pending-permission-mode)
                    (authority admitted 'full-auto 'allow 'off)
                    ;; Rules change after admission; the same request must deny.
                    (let ((rules (niwa-session-permission-rules session)))
                      (unwind-protect
                          (progn
                            (setf (niwa-session-permission-rules session)
                                  `(("AcceptedHandoffEdit" :action deny)
                                    ("Bash" :pattern ,command :action deny)))
                            (authority admitted 'full-auto 'deny 'off))
                        (setf (niwa-session-permission-rules session) rules)))
                    (should (equal source-before (baseline session buffer)))
                    (should (equal target-before (baseline session buffer)))
                    (if (eq execution 'goal)
                        (progn
                          (durable-goal goal 'active)
                          ;; Pause before settlement to avoid an automatic next turn.
                          ;; The admitted request retains its accepted authority.
                          (should (eq goal (niwa-goal-pause)))
                          (authority admitted 'full-auto 'allow 'off))
                      (should-not goal))
                    ;; The fake provider completes the real admitted FSM normally.
                    (gptel--fsm-transition fsm 'DONE)
                    (niwa-test--await 5 "Accepted handoff did not settle"
                      (not (niwa-turn-busy-p buffer)))
                    (should-not niwa--current-request)
                    (should (eq 'success (plist-get (gptel-fsm-info fsm) :niwa-terminal-status)))
                    (when goal (durable-goal goal 'paused))
                    (let* ((ordinary (niwa--submit-generated-turn "Ordinary baseline follow-up"))
                           (ordinary-info (gptel-fsm-info ordinary))
                           (ordinary-request niwa--current-request))
                      (should (= 2 (length sent)))
                      (should (eq 'WAIT (gptel-fsm-state ordinary)))
                      (should (eq ordinary (niwa-request-fsm ordinary-request)))
                      (should (eq (gptel-get-backend "Fast") (plist-get ordinary-info :backend)))
                      (should (eq 'fast-model (plist-get ordinary-info :model)))
                      (should (eq 'high (plist-get ordinary-info :reasoning-effort)))
                      (should-not (niwa-request-implementation-policy ordinary-request))
                      (should-not (niwa-request-permission-mode-override ordinary-request))
                      (authority ordinary-request 'ask 'ask 'best-effort)
                      (should (equal source-before (baseline session buffer)))
                      (should (equal target-before (baseline session buffer)))
                      (should (equal global-budget (default-value 'niwa-goal-token-budget)))
                      (gptel--fsm-transition ordinary 'DONE)
                      (niwa-test--await 5 "Ordinary handoff follow-up did not settle"
                        (not (niwa-turn-busy-p buffer))))
                    (should (equal source-before (baseline session buffer)))
                    (should (equal target-before (baseline session buffer)))
                    (should (equal global-budget (default-value 'niwa-goal-token-budget)))
                    (when goal (durable-goal goal 'paused))))))))))))

(provide 'test-niwa-plan-handoff-owner)
;;; test-niwa-plan-handoff-owner.el ends here
