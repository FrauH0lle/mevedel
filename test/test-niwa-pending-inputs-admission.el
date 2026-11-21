;;; test-niwa-pending-inputs-admission.el --- Wake authority acceptance -*- lexical-binding: t -*-

;;; Commentary:
;; Exercise generated background turns through native and ACP admission.
;; Native requests stop at the network boundary; ACP uses the local fixture peer.

;;; Code:

(require 'niwa-engine-test-support
         (file-name-concat
          (file-name-directory (or load-file-name buffer-file-name))
          "niwa-engine-test-support"))
(require 'gptel-openai)
(require 'niwa-agent-control)
(require 'niwa-auth)
(require 'niwa-chat)
(require 'niwa-claude-code)
(require 'niwa-collaboration)
(require 'niwa-collaboration-transport)
(require 'niwa-pending-inputs)
(require 'niwa-permissions)
(require 'niwa-presets)
(require 'niwa-schedule)
(require 'niwa-transcript-audit)
(require 'niwa-view-composer)

(niwa-deftest niwa-pending-inputs--wake-for-event/admission (:quiet t)
  (dolist (implementation '(nil t))
    (dolist (sticky-plan '(nil t))
      ;; Each engine retains a project control.  Wake permission selection is
      ;; request-local; both storage modes still persist real transcripts.
      (niwa-engine-test--with-session :file-workspace (or implementation sticky-plan)
        (niwa-request-end)
        (let ((gptel--known-backends nil)
              (niwa-claude-code-directory (file-name-concat root "claude"))
              (view (generate-new-buffer " *wake-admission-view*"))
              (niwa-collaboration--rooms (make-hash-table :test #'eq))
              admitted controls turns-before)
          (unwind-protect
              (cl-letf (((symbol-function 'niwa-claude-code-unavailable-reason)
                         #'ignore))
                (let* ((external (eq ,engine 'acp))
                       (provider
                        (if external
                            (progn
                              (niwa-claude-code-register)
                              "Claude Code:sonnet")
                          (gptel-make-openai "Wake" :key "test" :models '(wake))
                          "Wake:wake"))
                       (capture
                        (lambda ()
                          (with-current-buffer buffer
                            (push (list niwa--current-request
                                        (niwa-permission-mode-effective session)
                                        (niwa-plan-read-only-p session))
                                  admitted)))))
                  (niwa-model-set-session-provider
                   session (niwa-model-resolve-provider provider) buffer)
                  (setq-local gptel-system-prompt "Wake admission fixture"
                              gptel-tools nil
                              gptel-use-curl nil
                              gptel-stream nil
                              gptel-prompt-transform-functions
                              '(niwa-skills--transform-apply-request-model-policy)
                              gptel-send--handlers
                              (niwa-preset--build-handlers
                               (copy-tree (default-value 'gptel-send--handlers))))
                  (niwa-chat-install-request-hooks)
                  (niwa-view--setup view buffer)
                  (setf (niwa-session-permission-mode session) 'ask)
                  (if implementation
                      (setq-local niwa--pending-implementation-policy
                                  (list :model-provider provider :reasoning-effort nil
                                        :permission-mode 'full-auto))
                    (setq-local niwa--pending-permission-mode 'full-auto))
                  (puthash buffer (list :data-buffer buffer :session session :status nil
                                        :guests (make-hash-table) :transport 'phone)
                           niwa-collaboration--rooms)
                  (cl-letf (((symbol-function 'niwa-collaboration--transport-control)
                             (lambda (_transport frame) (push frame controls)))
                            ((symbol-function 'gptel-curl-get-response)
                             (lambda (_fsm) (funcall capture)))
                            ((symbol-function 'gptel--url-get-response)
                             (lambda (_fsm) (funcall capture)))
                            ((symbol-function 'niwa-claude-code-launch)
                             (niwa-engine-test--claude-launch
                              (lambda (&rest _)
                                (funcall capture)
                                nil))))
                    ;; Real submission consumes the one-turn authority.
                    (niwa--submit-generated-turn "First request")
                    (niwa-test--await 5 "First request was not admitted"
                      (= 1 (length admitted)))
                    (let ((previous (caar admitted)))
                      (should (eq 'full-auto (cadar admitted)))
                      (should (eq 'full-auto
                                  (niwa-request-permission-mode-override previous)))
                      (should-not niwa--pending-permission-mode)
                      (should-not niwa--pending-implementation-policy)
                      ;; Complete the native fixture without contacting a provider.
                      (unless external
                        (gptel--fsm-transition (niwa-request-fsm previous) 'DONE))
                      (niwa-test--await 5 "First request did not settle"
                        (not (niwa-turn-busy-p buffer)))
                      (should (equal '((:t "push")) controls))
                      (setq controls nil)
                      ;; The live baseline can change after the originating turn.
                      (setf (niwa-session-permission-mode session) 'edits
                            (niwa-session-plan-mode session) sticky-plan)
                      (niwa-agent-control-enqueue-execution-result
                       session "/root" "Background command finished" "wake-exec" t)
                      (niwa-pending-inputs--wake-for-event buffer)
                      (niwa-test--await 5 "Background request was not admitted"
                        (= 2 (length admitted)))
                      (let ((wake-request (caar admitted)))
                        (should-not (eq previous wake-request))
                        (should-not (niwa-request-permission-mode-override wake-request))
                        (should-not (niwa-request-implementation-policy wake-request))
                        (should (eq 'edits (cadar admitted)))
                        (should (eq sticky-plan (nth 2 (car admitted))))
                        (when sticky-plan
                          (should
                           (eq 'deny
                               (niwa-check-permission
                                "Eval" :session session :request wake-request)))))
                      (unless external
                        (gptel--fsm-transition
                         (niwa-request-fsm (caar admitted)) 'DONE))
                      (niwa-test--await 5 "Background request did not settle"
                        (not (niwa-turn-busy-p buffer)))
                      (should-not controls)
                      ;; Exercise the other initiators once per engine, not once
                      ;; per permission configuration.  End with a fresh user turn.
                      (unless (or implementation sticky-plan)
                        (dolist (source '(workspace-schedule goal monitor bash wake mixed user))
                          (ert-info ((format "Completion after %S" source))
                            (setq admitted nil controls nil)
                            (niwa-session--set-messages session nil)
                            (setq turns-before 0)
                            (when-let* ((goal (niwa-session-goal session)))
                              (setq turns-before (niwa-goal-turns-run goal))
                              (unless (eq source 'wake)
                                (setf (niwa-goal-status goal) 'active)))
                            (pcase source
                              ('goal
                               (niwa-goal-create "Check the build" :session session :token-budget nil)
                               (should (eq 'dispatched (niwa-goal-continue-if-idle session buffer))))
                              ('workspace-schedule
                               (niwa--submit-generated-turn "Check CI" nil nil nil 'workspace-schedule))
                              ('user (niwa--submit-generated-turn "Check CI"))
                              ('wake
                               (let* ((now (float-time)) (niwa-schedule-clock (lambda () now)))
                                 ;; Hold the previous turn's idle offer until the wake commits.
                                 (with-current-buffer view (niwa-view-test--insert-composer-draft "draft"))
                                 (niwa-schedule-create session buffer "20m" "Check CI")
                                 (cl-incf now 1200)
                                 (niwa-schedule-run-due)
                                 (should-not (niwa-session-schedules session))
                                 (niwa-test--await 5 "Wake did not commit"
                                   (niwa-agent-control-wake-pending-p session))
                                 (should-not admitted)
                                 (setf (niwa-goal-status (niwa-session-goal session)) 'active)
                                 (niwa-view-composer-replace "" view)))
                              (_
                               (when (eq source 'mixed)
                                 (niwa-agent-control-enqueue-wake-up session "Check CI" nil nil))
                               (when (memq source '(monitor mixed))
                                 (niwa-agent-control-enqueue-monitor-events
                                  session "monitor-ci" "CI" '(("CI finished")) t))
                               (when (eq source 'bash)
                                 (niwa-agent-control-enqueue-execution-result
                                  session "/root" "CI finished" "wake-exec" t))
                               (niwa-pending-inputs--wake-for-event buffer)))
                            (niwa-test--await 5 "Request was not admitted" admitted)
                            (when-let* ((goal (niwa-session-goal session)))
                              (let* ((info (niwa-engine-info (niwa-engine-owner (caar admitted))))
                                     (goal-id (unless (memq source '(wake mixed)) (niwa-goal-id goal))))
                                (when (memq source '(wake mixed))
                                  (should (eq (plist-get (plist-get info :context) :turn-trigger) 'scheduled-wake)))
                                (should (equal goal-id (plist-get info :niwa-goal-id)))
                                (should (equal goal-id (plist-get info :niwa-goal-accounting-id))))
                              (setf (niwa-goal-status goal) 'paused))
                            (unless external
                              (gptel--fsm-transition (niwa-request-fsm (caar admitted)) 'DONE))
                            (niwa-test--await 5 "Request did not settle" (not (niwa-turn-busy-p buffer)))
                            (when-let* ((goal (niwa-session-goal session)))
                              (should (= (niwa-goal-turns-run goal)
                                         (+ turns-before (if (memq source '(wake mixed)) 0 1)))))
                            (should (equal controls (and (eq source 'user) '((:t "push"))))))))))))
            (when (buffer-live-p view) (kill-buffer view)))))))
  (engine)
  :doc "native wake admission drops one-turn overrides and honors live sticky Plan"
  'native
  :doc "ACP wake admission drops one-turn overrides and honors live sticky Plan"
  'acp)

(niwa-deftest niwa-pending-inputs--wake-for-event/run-accounting (:quiet t)
  (niwa-engine-test--with-session :file-workspace t
    (niwa-request-end)
    (let* ((gptel--known-backends nil)
           (now (floor (float-time))) (niwa-schedule-clock (lambda () now))
           (view (generate-new-buffer " *wake-run-view*"))
           (niwa-claude-code-directory (file-name-concat root "claude"))
           (curl (symbol-function 'gptel-curl-get-response))
           (missing-curl (file-name-concat root "missing-curl"))
           notices machine settled release
           (refuse-start (eq ,engine 'start-error))
           (receive (lambda (fsm)
                      (if refuse-start
                          (let ((gptel-use-curl missing-curl)
                                ;; A nonexistent executable cannot contact the provider.
                                (niwa-test-allow-provider-requests t))
                            (funcall curl fsm))
                        (setq machine fsm))))
           (niwa-notify-function (lambda (kind details)
                                      (push (list kind (plist-get details :text)) notices))))
      (unwind-protect
          (cl-letf (((symbol-function 'niwa-claude-code-unavailable-reason) #'ignore)
                    ((symbol-function 'gptel--url-get-response) receive)
                    ((symbol-function 'gptel-curl-get-response) receive)
                    ((symbol-function 'niwa-claude-code-launch)
                     (niwa-engine-test--claude-launch
                      (lambda (&rest _) '(:meta ((promptResponse . ((stopReason . "refusal")))))))))
            (niwa-model-set-session-provider
             session (niwa-model-resolve-provider
                      (if (eq ,engine 'acp)
                          (progn (niwa-claude-code-register) "Claude Code:sonnet")
                        (gptel-make-openai "Wake" :key "k" :models '(wake)) "Wake:wake")) buffer)
            (setq-local gptel-use-curl nil gptel-stream nil gptel-tools nil
                        gptel-system-prompt "Wake fixture"
                        gptel-prompt-transform-functions '(niwa-skills--transform-apply-request-model-policy)
                        gptel-send--handlers (niwa-preset--build-handlers
                                              (copy-tree (default-value 'gptel-send--handlers))))
            (niwa-chat-install-request-hooks)
            (when (eq ,engine 'async-preflight)
              (setq-local gptel-prompt-transform-functions
                          (append gptel-prompt-transform-functions
                                  (list (lambda (continue _fsm) (setq release continue))))
                          niwa-turn-settled-functions (list (lambda (_) (setq settled t)))))
            (niwa-view--setup view buffer)
            (with-current-buffer view (niwa-view-test--insert-composer-draft "draft"))
            (let ((wake (niwa-schedule-create
                         session buffer (if (memq ,engine '(preflight async-preflight)) "1m" "every 1m") "Check CI")))
              (cl-incf now 60)
              (niwa-schedule-run-due)
              (niwa-test--await 5 "Wake not delivered" (niwa-agent-control-wake-pending-p session))
              (niwa-agent-control-enqueue-monitor-events
               session "mon" "CI" '(("Monitor check finished")) t)
              (niwa-view-composer-replace "" view)
              (pcase ,engine
                ('async-preflight
                 (niwa-pending-inputs--wake-for-event buffer)
                 (should release)
                 (should-not niwa--current-request)
                 (setf (niwa-session-plan-mode session) t)
                 (funcall release)
                 (should-not machine)
                 (should-not settled))
                ('preflight
                 (cl-letf (((symbol-function 'niwa-auth-assert-ready)
                            (lambda (_) (error "Token expired"))))
                   (should-error (niwa-pending-inputs--wake-for-event buffer))))
                ('start-error
                 (let ((unread (niwa-agent-control-context-mailbox session)))
                   (should-error (niwa-pending-inputs--wake-for-event buffer)
                                 :type 'file-missing)
                   (setq refuse-start nil)
                   (should-not machine)
                   (should niwa--current-request)
                   (should-not (niwa-agent-control-context-mailbox session))
                   (niwa-request-end)
                   (niwa-test--await 5 "Failed request teardown did not settle"
                     (not (niwa-turn-busy-p buffer)))
                   (let ((receipts (niwa-transcript-audit-buffer-spans 'mailbox-receipt)))
                     (should (equal (mapcar (lambda (record) (plist-get record :id)) unread)
                                    (mapcar (lambda (span) (plist-get (plist-get span :record) :id))
                                            receipts))))))
                (_ (niwa-pending-inputs--wake-for-event buffer)))
              (when (memq ,engine '(native write-error))
                (niwa-test--await 5 "Provider not started" machine)
                (setf (gptel-fsm-info machine) (plist-put (gptel-fsm-info machine) :error '(:message "Quota exhausted")))
                (setq-local niwa-turn-settled-functions (cons (lambda (outcome) (setq settled outcome))
                                                                  niwa-turn-settled-functions))
                (if (eq ,engine 'write-error)
                    (cl-letf (((symbol-function 'niwa-agent-control-commit-session)
                               (lambda (&rest _) (error "Sidecar write refused"))))
                      (gptel--fsm-transition machine 'ERRS))
                  (gptel--fsm-transition machine 'ERRS))
                (should (eq 'error settled)))
              (unless (memq ,engine '(start-error preflight async-preflight))
                (niwa-test--await 5 "Wake did not settle" (not (niwa-turn-busy-p buffer)))
                (should (eq 'error (niwa-session-last-outcome session))))
              (if (memq ,engine '(preflight async-preflight))
                  (progn
                    (should (equal '(MONITOR) (mapcar (lambda (r) (plist-get r :type))
                                                            (niwa-agent-control-context-mailbox session))))
                    (let ((entry (car (niwa-session-schedules session))))
                      (should (eq 'failed (plist-get entry :status)))
                      (should (niwa-schedule-entry-p entry))
                      (should (equal (if (eq ,engine 'preflight) "Token expired"
                                       "Implementation context changed during preparation; submit again")
                                     (plist-get (car (plist-get entry :runs)) :reason)))))
                (should-not (niwa-agent-control-context-mailbox session)))
              (if (eq ,engine 'write-error)
                  (progn (should-not notices)
                         (should-not (plist-get (car (niwa-schedule-pending session)) :runs)))
                (should (equal '(failure) (mapcar #'car notices)))
                (should (string-search (plist-get wake :id) (cadar notices))))
              (when (eq ,engine 'start-error)
                (let ((runs (plist-get (car (niwa-session-schedules session)) :runs)))
                  (should (equal '(failed) (mapcar (lambda (run) (plist-get run :outcome)) runs))))
                (niwa--submit-generated-turn "Continue after the failed start")
                (niwa-test--await 5 "Next request did not start" machine)
                (let ((payload (prin1-to-string (plist-get (gptel-fsm-info machine) :data))))
                  (dolist (text '("Check CI" "Monitor check finished"))
                    (let ((start (string-search text payload)))
                      (should start)
                      (should-not (string-search text payload (+ start (length text)))))))
                (should (= 2 (length (niwa-transcript-audit-buffer-spans 'mailbox-receipt))))
                (gptel--fsm-transition machine 'DONE)
                (niwa-test--await 5 "Next request did not settle" (not (niwa-turn-busy-p buffer)))
                (should (= 1 (length notices))))
              (when (eq ,engine 'native)
                ;; A user reading wake mail owns their failure.  A successful wake
                ;; resets the streak; an interrupted wake and a skipped cadence do not.
                (dolist (step '((user 1) (user-lost 1) (wake-lost 2) (DONE 2) (ERRS 3) (ABRT 3) (ERRS 4) (ERRS 5)))
                  (setq machine nil)
                  (with-current-buffer view (niwa-view-test--insert-composer-draft "hold"))
                  (cl-incf now (if (= 3 (cadr step)) 180 60))
                  (niwa-schedule-run-due)
                  (niwa-test--await 5 "Missing recurring wake" (niwa-agent-control-wake-pending-p session))
                  (niwa-view-composer-replace "" view)
                  (if (memq (car step) '(user user-lost)) (niwa--submit-generated-turn "My request")
                    (niwa-pending-inputs--wake-for-event buffer))
                  (niwa-test--await 5 "Request not admitted" machine)
                  (if (memq (car step) '(user-lost wake-lost))
                      (progn
                        ;; lose the request slot before the provider's terminal event
                        (setq niwa--current-request nil)
                        (gptel--fsm-transition machine 'ERRS)
                        (niwa-test--await 5 "Loss not recorded" (eq 'lost (niwa-session-last-outcome session)))
                        (gptel--fsm-transition machine 'DONE)) ; late duplicate
                    (gptel--fsm-transition machine (if (eq 'user (car step)) 'ERRS (car step))))
                  (niwa-test--await 5 "Request did not settle" (not (niwa-turn-busy-p buffer)))
                  (should (= (cadr step) (length notices))))
                (should-not (niwa-schedule-pending session))
                (should (string-search "paused" (cadar notices)))
                (let ((entry (car (niwa-session-schedules session))))
                  (should (eq 'paused (plist-get entry :status)))
                  (should (niwa-schedule-entry-p entry))
                  (should (stringp (plist-get entry :pause-reason))))
                (let* ((niwa-schedule-max-pending 1)
                       (other (niwa-schedule-create session buffer "1h" "Another wake-up")))
                  (should-error (niwa-schedule-resume session buffer (plist-get wake :id))
                                :type 'user-error)
                  (niwa-schedule-cancel session buffer (plist-get other :id)))
                (niwa-schedule-resume session buffer (plist-get wake :id))
                (let ((entry (car (niwa-schedule-pending session))))
                  (should (= (+ now 60) (plist-get entry :due)))
                  (should (zerop (plist-get entry :failures)))))
              (niwa-schedule-cancel session buffer (plist-get wake :id))))
        (when (buffer-live-p view) (kill-buffer view)))))
  (engine)
  :doc "native error for a delivered wake in mixed mail produces one failure notice"
  'native
  :doc "ACP error for a delivered wake in mixed mail produces one failure notice"
  'acp
  :doc "a failed Curl launch accounts the wake once and replays its mail only as history"
  'start-error
  :doc "a preflight failure records the one-shot and retires no unrelated mail"
  'preflight
  :doc "refused sidecar write leaves the run unchanged and ordinary terminal hooks settle"
  'write-error
  :doc "the preset rejects a scheduled turn after asynchronous preparation without running user hooks"
  'async-preflight)

(provide 'test-niwa-pending-inputs-admission)
;;; test-niwa-pending-inputs-admission.el ends here
