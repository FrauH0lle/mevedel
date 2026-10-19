;;; test-mevedel-plan-handoff-owner.el -- Published kickoff ownership -*- lexical-binding: t -*-

;;; Commentary:

;; Real Goal publication, handoff completion saves
;; and generated request startup.  Timers run user Goal commands while the
;; publication save is still on the stack; only provider transport is faked.

;;; Code:

(require 'mevedel-engine-test-support
         (file-name-concat
          (file-name-directory (or load-file-name buffer-file-name))
          "mevedel-engine-test-support"))
(require 'mevedel-plan-handoff)
(require 'mevedel-bash-policy)
(require 'mevedel-permissions)
(require 'mevedel-view-composer)
(require 'mevedel-journal-capture)
(require 'gptel-openai)

(defmacro mevedel-handoff-owner-test--with-target (&rest body)
  "Run BODY with a published session holding a Here Goal handoff.
A Worktree handoff runs the same Here handoff in its fork."
  (declare (indent 0) (debug (body)))
  `(mevedel-engine-test--with-session
     (mevedel-request-end)
     (with-current-buffer buffer
       (mevedel-skills-test--with-model-backends
         (mevedel-model-set-session-provider
          session (mevedel-model-resolve-provider "Fast:fast-model") buffer)
         (mevedel-model-set-session-effort session 'high buffer)
         (mevedel-permission-mode-set-raw 'ask)
         (let* ((selection (list :location 'here :context 'current :execution 'goal
                                 :mode 'full-auto :model-provider "Balanced:balanced-model"
                                 :reasoning-effort nil :goal-token-budget 200))
                (policy (mevedel-implementation-policy-from-selection selection))
                (draft (mevedel-plan-write-current "# Accepted outcome\nRead and verify."
                                                   session buffer))
                (accepted (mevedel-plan-archive-accepted draft session))
                (record (list :step 'submit :goal-id "reserved-kickoff"
                              :selection selection :accepted accepted))
                (submission (mevedel-prompt-submission-create :session session))
                (mevedel-journal-enabled nil)
                (gptel-use-curl t) (gptel-use-tools nil) (gptel-use-context nil)
                (gptel-send--handlers
                 (mevedel-preset--build-handlers (copy-tree gptel-send--handlers)))
                (gptel-send--transitions
                 (mevedel-preset--build-transitions (copy-tree gptel-send--transitions)))
                (gptel-prompt-transform-functions
                 '(mevedel-skills--transform-apply-request-model-policy)))
           (mevedel-plan--metadata-put session :accepted-path (plist-get accepted :path))
           (mevedel-plan--metadata-put session :accepted-hash (plist-get accepted :hash))
           (mevedel-plan--metadata-put session :implementation-retry record)
           (mevedel-prompt-submission-accept submission "Implement accepted outcome" nil nil nil)
           ,@body)))))

(mevedel-deftest mevedel-plan-handoff--dispatch-submission/published-owner (:quiet t)
  ,test
  (test)
  :doc "publication callbacks cannot transfer or pause stale kickoff authority"
  (progn
    (let ((stage 'publication))
      (dolist (change '(pause clear replace edit policy request))
        (mevedel-handoff-owner-test--with-target
          (let ((save (symbol-function 'mevedel-session-artifacts-save))
                changed owner replacement admitted sent)
            (cl-letf (((symbol-function 'mevedel-auth-assert-ready) #'ignore)
                      ((symbol-function 'gptel-curl-get-response)
                       (lambda (fsm) (push fsm sent)))
                      ((symbol-function 'mevedel-session-artifacts-save)
                       (lambda (saving-session saving-buffer)
                         (prog1 (funcall save saving-session saving-buffer)
                           (when (and (not changed)
                                      (mevedel-session-goal session)
                                      (and (eq saving-session session)
                                               (not (plist-member (mevedel-session-plan-metadata session)
                                                                  :implementation-retry))))
                             (setq changed t owner (mevedel-session-goal session))
                             (run-at-time
                              0 nil
                              (lambda ()
                                (with-current-buffer buffer
                                  (should-not mevedel--pending-implementation-policy)
                                  (pcase change
                                    ('pause (mevedel-goal-pause))
                                    ('clear (mevedel-goal-clear))
                                    ('replace
                                     (mevedel-goal-clear)
                                     (setq replacement
                                           (mevedel-goal-create
                                            "Unrelated replacement" :session session
                                            :id "reserved-kickoff" :implementation-policy policy)))
                                    ('edit (mevedel-goal-edit "Revised kickoff"))
                                    ('request (setq admitted (mevedel-request-begin session)))
                                    ('policy
                                     (setf (mevedel-goal-implementation-policy owner)
                                           '(:model-provider "Balanced:balanced-model"
                                             :reasoning-effort high :permission-mode edits))
                                     (funcall save session buffer))))))
                             (accept-process-output nil .02))))))
              (mevedel-plan-handoff--dispatch-submission
               session buffer record selection accepted
               "Implement" submission))
            (should changed)
            (should-not sent)
            (should (eq admitted mevedel--current-request))
            (when admitted
              (should-not (mevedel-request-cancelled-p admitted)))
            (should-not mevedel--pending-implementation-policy)
            (should (eq 'pending (mevedel-prompt-submission-state submission)))
            (let* ((current (mevedel-session-goal session))
                   (saved (mevedel-session-codec-read
                           (mevedel-session-artifacts-sidecar-path
                            (mevedel-session-save-path session)))))
              (should (eq (pcase change ('clear nil) ('pause 'paused) (_ 'active))
                          (and current (mevedel-goal-status current))))
              (when replacement
                (should (eq replacement current))
                (should-not (eq owner current)))
              (should (eq (and current (mevedel-goal-status current))
                          (plist-get (plist-get saved :goal) :status)))
            (when admitted (mevedel-request-end t))))))))

  :doc "a real provider startup failure pauses its owner but never replacement authority"
  (progn
   (dolist (change '(none replace))
    (mevedel-handoff-owner-test--with-target
      (let (sent replacement owner expected-status)
        (cl-letf (((symbol-function 'mevedel-auth-assert-ready) #'ignore)
                  ((symbol-function 'gptel-curl-get-response)
                   (lambda (fsm)
                     (push fsm sent)
                     (with-current-buffer buffer
                       (setq owner (mevedel-session-goal session))
                       (when (eq change 'replace)
                         (mevedel-goal-clear)
                         (setq replacement
                               (mevedel-goal-create
                                "Unrelated startup replacement" :session session
                                :implementation-policy policy)))
                       (setq expected-status (if (eq change 'none) 'paused 'active)))
                     (error "Provider startup refused"))))
          (mevedel-plan-handoff--dispatch-submission
           session buffer record selection accepted
           "Implement" submission))
        (should (= 1 (length sent)))
        (let ((current (mevedel-session-goal session)))
          (should (eq expected-status (mevedel-goal-status current)))
          (when replacement
            (should (eq replacement current))
            (should-not (eq owner current)))
          (unless (eq change 'none)
            (should-not (mevedel-goal-reason current))))
        ;; The fake transport threw before installing a network producer.
        ;; Abort its retained FSM to finish fixture settlement, without starting
        ;; a separate automatic turn for the newly active replacement Goal.
        (cl-letf (((symbol-function 'mevedel-goal-schedule-continuation) #'ignore))
          (gptel--fsm-transition (car sent) 'ABRT)
          (mevedel-test--await 5 "Failed kickoff did not settle"
            (not (mevedel-turn-busy-p buffer))))
        (let* ((goal (mevedel-session-goal session))
               (saved (mevedel-session-codec-read
                       (mevedel-session-artifacts-sidecar-path
                        (mevedel-session-save-path session)))))
          (should (eq expected-status (mevedel-goal-status goal)))
          (when (eq change 'none)
            (should (equal "reserved-kickoff" (mevedel-goal-id goal)))
            (should (string-search "Provider startup refused" (mevedel-goal-reason goal))))
          (should (eq expected-status (plist-get (plist-get saved :goal) :status)))))))))

(mevedel-deftest mevedel-plan-handoff--dispatch-submission/owner-failures (:quiet t)
  ,test
  (test)
  :doc "owned publication failures pause durably; insertion staleness preserves replacement"
  (progn
    (dolist (stage '(publication insertion))
      (mevedel-handoff-owner-test--with-target
        (let ((save (symbol-function 'mevedel-session-artifacts-save))
              changed owner replacement)
          (when (eq stage 'insertion)
            (setq-local after-change-functions
                        (list (lambda (&rest _)
                                (when (and mevedel--pending-implementation-policy (not changed))
                                  (setq changed t owner (mevedel-session-goal session))
                                  (mevedel-goal-clear)
                                  (setq replacement
                                        (mevedel-goal-create "Replacement during insertion"
                                                             :session session
                                                             :implementation-policy policy)))))))
          (cl-letf (((symbol-function 'gptel-curl-get-response)
                     (lambda (&rest _) (ert-fail "Stale or failed publication started a provider")))
                    ((symbol-function 'mevedel-session-artifacts-save)
                     (lambda (saving-session saving-buffer)
                       (prog1 (funcall save saving-session saving-buffer)
                         (when (and (not changed) (not (eq stage 'insertion))
                                    (mevedel-session-goal session)
                                    (and (eq saving-session session)
                                             (not (plist-member (mevedel-session-plan-metadata session)
                                                                :implementation-retry))))
                           (setq changed t owner (mevedel-session-goal session))
                           (error "Kickoff publication refused"))))))
            (mevedel-plan-handoff--dispatch-submission
             session buffer record selection accepted
             "Implement" submission))
          (should changed)
          (should-not mevedel--current-request)
          (should-not mevedel--pending-implementation-policy)
          (let* ((current (mevedel-session-goal session))
                 (saved (mevedel-session-codec-read
                         (mevedel-session-artifacts-sidecar-path
                          (mevedel-session-save-path session)))))
            (if replacement
                (progn
                  (should (eq replacement current))
                  (should-not (eq owner current))
                  (should (eq 'active (mevedel-goal-status current)))
                  (should-not (mevedel-goal-reason current)))
              (should (eq owner current))
              (should (eq 'paused (mevedel-goal-status current)))
              (should (string-search "publication refused" (mevedel-goal-reason current))))
            (should (eq (mevedel-goal-status current)
                        (plist-get (plist-get saved :goal) :status)))))))))

(mevedel-deftest mevedel--implement-plan/direct-busy-owner (:quiet t)
  ,test
  (test)
  :doc "public Direct retry cannot seize a newer generated-send FSM admitted during publication"
  (mevedel-handoff-owner-test--with-target
    (setf (plist-get selection :execution) 'direct)
    (let ((view (generate-new-buffer " *direct-owner-view*"))
          (save (symbol-function 'mevedel-session-artifacts-save))
          (accept (symbol-function 'mevedel-prompt-submission-accept))
          changed sent hook-owners newer newer-fsm newer-text accepted-submission
          observations)
      (unwind-protect
          (save-window-excursion
            (mevedel-view--setup view buffer)
            (set-window-buffer (selected-window) view)
            (setq-local gptel-post-request-hook
                        (list (lambda () (push mevedel--current-request hook-owners))))
            (cl-letf (((symbol-function 'mevedel-auth-assert-ready) #'ignore)
                      ((symbol-function 'gptel-curl-get-response)
                       (lambda (fsm) (push fsm sent)))
                      ((symbol-function 'mevedel-prompt-submission-accept)
                       (lambda (transaction &rest args)
                         (setq accepted-submission transaction)
                         (apply accept transaction args)))
                      ((symbol-function 'mevedel-session-artifacts-save)
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
                                        (mevedel--submit-generated-turn "Newer ordinary request")
                                        newer mevedel--current-request)
                                  (should (eq 'WAIT (gptel-fsm-state newer-fsm)))
                                  (should (eq newer-fsm (mevedel-request-fsm newer)))
                                  (mevedel-model-set-session-effort session 'low buffer)
                                  ;; Serialize gptel's bounds before comparing
                                  ;; bytes; retry failure also performs a save.
                                  (funcall save session buffer)
                                  (setq newer-text (buffer-string)))))
                             (mevedel-test--await 5 "Competing Direct request did not start"
                               newer-text))))))
              (mevedel-retry-plan-implementation session buffer)
              ;; Save evidence before settling transports, even on the red run,
              ;; so fixture cleanup does not hide the ownership or policy leak.
              (setq observations
                    (list :sent (length sent) :hooks (length hook-owners)
                          :request mevedel--current-request :fsm (mevedel-request-fsm newer)
                          :text (buffer-string)
                          :submission-state (mevedel-prompt-submission-state accepted-submission)
                          :pending mevedel--pending-implementation-policy
                          :pending-mode mevedel--pending-permission-mode
                          :context mevedel-skills--pending-request-context))
              (dolist (fsm sent) (gptel--fsm-transition fsm 'ABRT))
              (mevedel-test--await 5 "Newer Direct owner fixture did not settle"
                (not (mevedel-turn-busy-p buffer)))
              (let ((ordinary (mevedel--submit-generated-turn "Ordinary follow-up")))
                (setq observations
                      (append observations
                              (list :ordinary-model (plist-get (gptel-fsm-info ordinary) :model)
                                    :ordinary-effort (plist-get (gptel-fsm-info ordinary) :reasoning-effort)
                                    :ordinary-policy (mevedel-request-implementation-policy mevedel--current-request)
                                    :ordinary-mode (mevedel-request-permission-mode-override mevedel--current-request))))
                (gptel--fsm-transition ordinary 'ABRT)
                (mevedel-test--await 5 "Direct owner follow-up did not settle"
                  (not (mevedel-turn-busy-p buffer))))))
        (with-current-buffer view (mevedel-view-stream-stop))
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
      (should-not (mevedel-request-implementation-policy newer))
      (should-not (mevedel-request-permission-mode-override newer))
      (should-not (plist-get (gptel-fsm-info newer-fsm) :mevedel-plan-handoff-source-buffer))
      (should-not (plist-get observations :pending))
      (should-not (plist-get observations :pending-mode))
      (should-not (plist-get observations :context))
      (should (eq 'fast-model (plist-get observations :ordinary-model)))
      (should (eq 'low (plist-get observations :ordinary-effort)))
      (should-not (plist-get observations :ordinary-policy))
      (should-not (plist-get observations :ordinary-mode))
      (should (equal "Fast:fast-model" (mevedel-session-model-provider session)))
      (should (eq 'low (mevedel-session-reasoning-effort session)))
      (should (eq 'ask (mevedel-session-permission-mode session)))))

  :doc "Direct cannot seize a post-request or still-settling owner"
  (progn
    (dolist (stage '(post-request settlement))
      (mevedel-handoff-owner-test--with-target
        (setf (plist-get selection :execution) 'direct)
        (let ((persist (symbol-function 'mevedel-goal-persist-failure))
              attempted sent newer-fsm newer observations)
          (cl-labels
              ((attempt (fsm)
                 (setq attempted t)
                 ;; Prime legitimate save metadata so exact byte comparison
                 ;; detects insertion rather than first-time bounds encoding.
                 (mevedel-session-artifacts-save session buffer)
                 (let ((text (buffer-string))
                       (pending mevedel--turn-settlements-pending)
                       (cancelled (mevedel-request-cancelled-p newer))
                       (info (copy-sequence (gptel-fsm-info fsm))))
                   (if (eq stage 'settlement)
                       (progn
                         (should-not mevedel--current-request)
                         (should (memq fsm pending)))
                     (should (eq newer mevedel--current-request))
                     (should (eq fsm (mevedel-request-fsm newer))))
                   (mevedel-plan-handoff--dispatch-submission
                    session buffer record selection accepted
                    "Implement" submission)
                   (setq observations
                         (list :text-unchanged (equal text (buffer-string))
                               :info-unchanged (equal info (gptel-fsm-info fsm))
                               :settlements-unchanged (eq pending mevedel--turn-settlements-pending)
                               :request mevedel--current-request
                               :cancelled-unchanged (eq cancelled (mevedel-request-cancelled-p newer))
                               :sends (length sent)
                               :policy mevedel--pending-implementation-policy)))))
            (setq-local gptel-post-request-hook
                        (list (lambda ()
                                (setq newer mevedel--current-request)
                                (when (eq stage 'post-request)
                                  (attempt (mevedel-request-fsm newer))))))
            (cl-letf (((symbol-function 'mevedel-auth-assert-ready) #'ignore)
                      ((symbol-function 'gptel-curl-get-response)
                       (lambda (fsm) (push fsm sent)))
                      ((symbol-function 'mevedel-goal-persist-failure)
                       (lambda (fsm)
                         (prog1 (funcall persist fsm)
                           (when (and (eq stage 'settlement) (eq fsm newer-fsm) (not attempted))
                             (with-current-buffer buffer (attempt fsm)))))))
              (setq newer-fsm (mevedel--submit-generated-turn "Newer owner"))
              (setq-local gptel-post-request-hook nil)
              (gptel--fsm-transition newer-fsm 'ABRT)
              (mevedel-test--await 5 "Direct busy-owner callback did not settle"
                (not (mevedel-turn-busy-p buffer)))
              (should attempted)
              (should (plist-get observations :text-unchanged))
              (should (plist-get observations :info-unchanged))
              (should (plist-get observations :settlements-unchanged))
              (should (eq (and (eq stage 'post-request) newer)
                          (plist-get observations :request)))
              (should (plist-get observations :cancelled-unchanged))
              (should (= 1 (plist-get observations :sends)))
              (should-not (plist-get observations :policy))
              (should (eq 'pending (mevedel-prompt-submission-state submission)))
              (should-not mevedel--pending-implementation-policy)
              (should-not mevedel--pending-permission-mode)
              (should-not mevedel-skills--pending-request-context)
              (let ((ordinary (mevedel--submit-generated-turn "Ordinary after rejected Direct")))
                (should (eq 'fast-model (plist-get (gptel-fsm-info ordinary) :model)))
                (should (eq 'high (plist-get (gptel-fsm-info ordinary) :reasoning-effort)))
                (should-not (mevedel-request-implementation-policy mevedel--current-request))
                (should-not (mevedel-request-permission-mode-override mevedel--current-request))
                (gptel--fsm-transition ordinary 'ABRT)
                (mevedel-test--await 5 "Direct callback follow-up did not settle"
                  (not (mevedel-turn-busy-p buffer)))))))))))

(mevedel-deftest mevedel-plan-handoff--dispatch-submission/joined-authority (:quiet t)
  ,test
  (test)
  :doc "Direct/Goal join accepted provider and actual authority on one admitted request"
  (progn
    (dolist (execution '(direct goal))
      (ert-info ((format "Accepted handoff: %s" execution))
        (mevedel-handoff-owner-test--with-target
          (setf (plist-get selection :execution) execution)
          ;; Ordinary selections differ from every accepted implementation axis.
          (mevedel-model-set-session-provider
           session (mevedel-model-resolve-provider "Fast:fast-model") buffer)
          (mevedel-model-set-session-effort session 'high buffer)
          (with-current-buffer buffer
            (mevedel-permission-mode-set-raw 'ask)
            (setf (mevedel-session-sandbox-mode session) 'best-effort)
            (setq-local mevedel-goal-token-budget 4321 mevedel-model-workloads nil))
          (let ((edit-tool (mevedel-tool--create :name "AcceptedHandoffEdit" :groups '(edit)))
                (command "make accepted-handoff-check")
                (global-budget (default-value 'mevedel-goal-token-budget))
                (mevedel-permission-rules nil)
                sent)
            (cl-labels
                ((baseline (owner data)
                   (list (mevedel-session-model-provider owner)
                         (mevedel-session-reasoning-effort owner)
                         (mevedel-session-permission-mode owner)
                         (mevedel-session-sandbox-mode owner)
                         (buffer-local-value 'gptel-backend data)
                         (buffer-local-value 'gptel-model data)
                         (buffer-local-value 'gptel-reasoning-effort data)
                         (buffer-local-value 'mevedel-permission-mode data)
                         (buffer-local-value 'mevedel-goal-token-budget data)))
                 (authority (admitted mode outcome confinement)
                   ;; No synthetic :mode: resolve this production request owner.
                   (let ((context (list :session session :buffer buffer
                                        :request admitted :workspace workspace)))
                     (should-not (plist-member context :mode))
                     (should (eq mode (mevedel-bash-policy-effective-permission-mode context)))
                     (should (eq confinement (mevedel-bash-policy-effective-sandbox-mode context)))
                     (should (eq outcome (mevedel-bash-policy-check-permission
                                          command :permission-context context)))
                     (should (eq outcome
                                 (mevedel-check-permission
                                  "AcceptedHandoffEdit" :tool-struct edit-tool
                                  :session session :request admitted
                                  :session-rules (mevedel-session-permission-rules session))))))
                 (durable-goal (owner status)
                   (should (eq owner (mevedel-session-goal session)))
                   (should (eq status (mevedel-goal-status owner)))
                   (should (equal policy (mevedel-goal-implementation-policy owner)))
                   (should (= 200 (mevedel-goal-token-budget owner)))
                   (let* ((saved (mevedel-session-codec-read
                                  (mevedel-session-artifacts-sidecar-path
                                   (mevedel-session-save-path session))))
                          (saved-goal (plist-get saved :goal)))
                     (should (equal (mevedel-goal-id owner) (plist-get saved-goal :id)))
                     (should (eq status (plist-get saved-goal :status)))
                     (should (equal policy (plist-get saved-goal :implementation-policy)))
                     (should (= 200 (plist-get saved-goal :token-budget))))))
              (let ((source-before (baseline session buffer))
                    (target-before (baseline session buffer)))
                (cl-letf (((symbol-function 'mevedel-auth-assert-ready) #'ignore)
                          ((symbol-function 'gptel-curl-get-response)
                           (lambda (fsm) (push fsm sent))))
                  (mevedel-plan-handoff--dispatch-submission
                   session buffer record selection accepted
                   "Implement" submission)
                  (should (= 1 (length sent)))
                  (let* ((fsm (car sent))
                         (info (gptel-fsm-info fsm))
                         (admitted mevedel--current-request)
                         (goal (mevedel-session-goal session)))
                    (should (eq 'WAIT (gptel-fsm-state fsm)))
                    (should (mevedel-request-p admitted))
                    (should (eq session (mevedel-request-session admitted)))
                    (should (eq buffer (plist-get info :buffer)))
                    (should (eq fsm (mevedel-request-fsm admitted)))
                    (should (eq (gptel-get-backend "Balanced") (plist-get info :backend)))
                    (should (eq 'balanced-model (plist-get info :model)))
                    (should (plist-member info :reasoning-effort))
                    (should-not (plist-get info :reasoning-effort))
                    (should (equal policy (mevedel-request-implementation-policy admitted)))
                    (should (eq 'full-auto (mevedel-request-permission-mode-override admitted)))
                    (should-not mevedel--pending-implementation-policy)
                    (should-not mevedel--pending-permission-mode)
                    (authority admitted 'full-auto 'allow 'off)
                    ;; Rules change after admission; the same request must deny.
                    (let ((rules (mevedel-session-permission-rules session)))
                      (unwind-protect
                          (progn
                            (setf (mevedel-session-permission-rules session)
                                  `(("AcceptedHandoffEdit" :action deny)
                                    ("Bash" :pattern ,command :action deny)))
                            (authority admitted 'full-auto 'deny 'off))
                        (setf (mevedel-session-permission-rules session) rules)))
                    (should (equal source-before (baseline session buffer)))
                    (should (equal target-before (baseline session buffer)))
                    (if (eq execution 'goal)
                        (progn
                          (durable-goal goal 'active)
                          ;; Pause before settlement to avoid an automatic next turn.
                          ;; The admitted request retains its accepted authority.
                          (should (eq goal (mevedel-goal-pause)))
                          (authority admitted 'full-auto 'allow 'off))
                      (should-not goal))
                    ;; The fake provider completes the real admitted FSM normally.
                    (gptel--fsm-transition fsm 'DONE)
                    (mevedel-test--await 5 "Accepted handoff did not settle"
                      (not (mevedel-turn-busy-p buffer)))
                    (should-not mevedel--current-request)
                    (should (eq 'success (plist-get (gptel-fsm-info fsm) :mevedel-terminal-status)))
                    (when goal (durable-goal goal 'paused))
                    (let* ((ordinary (mevedel--submit-generated-turn "Ordinary baseline follow-up"))
                           (ordinary-info (gptel-fsm-info ordinary))
                           (ordinary-request mevedel--current-request))
                      (should (= 2 (length sent)))
                      (should (eq 'WAIT (gptel-fsm-state ordinary)))
                      (should (eq ordinary (mevedel-request-fsm ordinary-request)))
                      (should (eq (gptel-get-backend "Fast") (plist-get ordinary-info :backend)))
                      (should (eq 'fast-model (plist-get ordinary-info :model)))
                      (should (eq 'high (plist-get ordinary-info :reasoning-effort)))
                      (should-not (mevedel-request-implementation-policy ordinary-request))
                      (should-not (mevedel-request-permission-mode-override ordinary-request))
                      (authority ordinary-request 'ask 'ask 'best-effort)
                      (should (equal source-before (baseline session buffer)))
                      (should (equal target-before (baseline session buffer)))
                      (should (equal global-budget (default-value 'mevedel-goal-token-budget)))
                      (gptel--fsm-transition ordinary 'DONE)
                      (mevedel-test--await 5 "Ordinary handoff follow-up did not settle"
                        (not (mevedel-turn-busy-p buffer))))
                    (should (equal source-before (baseline session buffer)))
                    (should (equal target-before (baseline session buffer)))
                    (should (equal global-budget (default-value 'mevedel-goal-token-budget)))
                    (when goal (durable-goal goal 'paused))))))))))))

(provide 'test-mevedel-plan-handoff-owner)
;;; test-mevedel-plan-handoff-owner.el ends here
