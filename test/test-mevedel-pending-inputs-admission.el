;;; test-mevedel-pending-inputs-admission.el --- Wake authority acceptance -*- lexical-binding: t -*-

;;; Commentary:
;; Exercise generated background turns through native and ACP admission.
;; Native requests stop at the network boundary; ACP uses the local fixture peer.

;;; Code:

(require 'mevedel-engine-test-support
         (file-name-concat
          (file-name-directory (or load-file-name buffer-file-name))
          "mevedel-engine-test-support"))
(require 'gptel-openai)
(require 'mevedel-agent-control)
(require 'mevedel-auth)
(require 'mevedel-chat)
(require 'mevedel-claude-code)
(require 'mevedel-collaboration)
(require 'mevedel-collaboration-transport)
(require 'mevedel-pending-inputs)
(require 'mevedel-permissions)
(require 'mevedel-presets)
(require 'mevedel-schedule)
(require 'mevedel-transcript-audit)
(require 'mevedel-view-composer)

(mevedel-deftest mevedel-pending-inputs--wake-for-event/admission (:quiet t)
  (dolist (implementation '(nil t))
    (dolist (sticky-plan '(nil t))
      ;; Each engine retains a project control.  Wake permission selection is
      ;; request-local; both storage modes still persist real transcripts.
      (mevedel-engine-test--with-session :file-workspace (or implementation sticky-plan)
        (mevedel-request-end)
        (let ((gptel--known-backends nil)
              (mevedel-claude-code-directory (file-name-concat root "claude"))
              (view (generate-new-buffer " *wake-admission-view*"))
              (mevedel-collaboration--rooms (make-hash-table :test #'eq))
              admitted controls turns-before)
          (unwind-protect
              (cl-letf (((symbol-function 'mevedel-claude-code-unavailable-reason)
                         #'ignore))
                (let* ((external (eq ,engine 'acp))
                       (provider
                        (if external
                            (progn
                              (mevedel-claude-code-register)
                              "Claude Code:sonnet")
                          (gptel-make-openai "Wake" :key "test" :models '(wake))
                          "Wake:wake"))
                       (capture
                        (lambda ()
                          (with-current-buffer buffer
                            (push (list mevedel--current-request
                                        (mevedel-permission-mode-effective session)
                                        (mevedel-plan-read-only-p session))
                                  admitted)))))
                  (mevedel-model-set-session-provider
                   session (mevedel-model-resolve-provider provider) buffer)
                  (setq-local gptel-system-prompt "Wake admission fixture"
                              gptel-tools nil
                              gptel-use-curl nil
                              gptel-stream nil
                              gptel-prompt-transform-functions
                              '(mevedel-skills--transform-apply-request-model-policy)
                              gptel-send--handlers
                              (mevedel-preset--build-handlers
                               (copy-tree (default-value 'gptel-send--handlers))))
                  (mevedel-chat-install-request-hooks)
                  (mevedel-view--setup view buffer)
                  (setf (mevedel-session-permission-mode session) 'ask)
                  (if implementation
                      (setq-local mevedel--pending-implementation-policy
                                  (list :model-provider provider :reasoning-effort nil
                                        :permission-mode 'full-auto))
                    (setq-local mevedel--pending-permission-mode 'full-auto))
                  (puthash buffer (list :data-buffer buffer :session session :status nil
                                        :guests (make-hash-table) :transport 'phone)
                           mevedel-collaboration--rooms)
                  (cl-letf (((symbol-function 'mevedel-collaboration--transport-control)
                             (lambda (_transport frame) (push frame controls)))
                            ((symbol-function 'gptel-curl-get-response)
                             (lambda (_fsm) (funcall capture)))
                            ((symbol-function 'gptel--url-get-response)
                             (lambda (_fsm) (funcall capture)))
                            ((symbol-function 'mevedel-claude-code-launch)
                             (mevedel-engine-test--claude-launch
                              (lambda (&rest _)
                                (funcall capture)
                                nil))))
                    ;; Real submission consumes the one-turn authority.
                    (mevedel--submit-generated-turn "First request")
                    (mevedel-test--await 5 "First request was not admitted"
                      (= 1 (length admitted)))
                    (let ((previous (caar admitted)))
                      (should (eq 'full-auto (cadar admitted)))
                      (should (eq 'full-auto
                                  (mevedel-request-permission-mode-override previous)))
                      (should-not mevedel--pending-permission-mode)
                      (should-not mevedel--pending-implementation-policy)
                      ;; Complete the native fixture without contacting a provider.
                      (unless external
                        (gptel--fsm-transition (mevedel-request-fsm previous) 'DONE))
                      (mevedel-test--await 5 "First request did not settle"
                        (not (mevedel-turn-busy-p buffer)))
                      (should (equal '((:t "push")) controls))
                      (setq controls nil)
                      ;; The live baseline can change after the originating turn.
                      (setf (mevedel-session-permission-mode session) 'edits
                            (mevedel-session-plan-mode session) sticky-plan)
                      (mevedel-agent-control-enqueue-execution-result
                       session "/root" "Background command finished" "wake-exec" t)
                      (mevedel-pending-inputs--wake-for-event buffer)
                      (mevedel-test--await 5 "Background request was not admitted"
                        (= 2 (length admitted)))
                      (let ((wake-request (caar admitted)))
                        (should-not (eq previous wake-request))
                        (should-not (mevedel-request-permission-mode-override wake-request))
                        (should-not (mevedel-request-implementation-policy wake-request))
                        (should (eq 'edits (cadar admitted)))
                        (should (eq sticky-plan (nth 2 (car admitted))))
                        (when sticky-plan
                          (should
                           (eq 'deny
                               (mevedel-check-permission
                                "Eval" :session session :request wake-request)))))
                      (unless external
                        (gptel--fsm-transition
                         (mevedel-request-fsm (caar admitted)) 'DONE))
                      (mevedel-test--await 5 "Background request did not settle"
                        (not (mevedel-turn-busy-p buffer)))
                      (should-not controls)
                      ;; Exercise the other initiators once per engine, not once
                      ;; per permission configuration.  End with a fresh user turn.
                      (unless (or implementation sticky-plan)
                        (dolist (source '(workspace-schedule goal monitor bash wake mixed user))
                          (ert-info ((format "Completion after %S" source))
                            (setq admitted nil controls nil)
                            (mevedel-session--set-messages session nil)
                            (setq turns-before 0)
                            (when-let* ((goal (mevedel-session-goal session)))
                              (setq turns-before (mevedel-goal-turns-run goal))
                              (unless (eq source 'wake)
                                (setf (mevedel-goal-status goal) 'active)))
                            (pcase source
                              ('goal
                               (mevedel-goal-create "Check the build" :session session :token-budget nil)
                               (should (eq 'dispatched (mevedel-goal-continue-if-idle session buffer))))
                              ('workspace-schedule
                               (mevedel--submit-generated-turn "Check CI" nil nil nil 'workspace-schedule))
                              ('user (mevedel--submit-generated-turn "Check CI"))
                              ('wake
                               (let* ((now (float-time)) (mevedel-schedule-clock (lambda () now)))
                                 ;; Hold the previous turn's idle offer until the wake commits.
                                 (with-current-buffer view (mevedel-view-test--insert-composer-draft "draft"))
                                 (mevedel-schedule-create session buffer "20m" "Check CI")
                                 (cl-incf now 1200)
                                 (mevedel-schedule-run-due)
                                 (should-not (mevedel-session-schedules session))
                                 (mevedel-test--await 5 "Wake did not commit"
                                   (mevedel-agent-control-wake-pending-p session))
                                 (should-not admitted)
                                 (setf (mevedel-goal-status (mevedel-session-goal session)) 'active)
                                 (mevedel-view-composer-replace "" view)))
                              (_
                               (when (eq source 'mixed)
                                 (mevedel-agent-control-enqueue-wake-up session "Check CI" nil nil))
                               (when (memq source '(monitor mixed))
                                 (mevedel-agent-control-enqueue-monitor-events
                                  session "monitor-ci" "CI" '(("CI finished")) t))
                               (when (eq source 'bash)
                                 (mevedel-agent-control-enqueue-execution-result
                                  session "/root" "CI finished" "wake-exec" t))
                               (mevedel-pending-inputs--wake-for-event buffer)))
                            (mevedel-test--await 5 "Request was not admitted" admitted)
                            (when-let* ((goal (mevedel-session-goal session)))
                              (let* ((info (mevedel-engine-info (mevedel-engine-owner (caar admitted))))
                                     (goal-id (unless (memq source '(wake mixed)) (mevedel-goal-id goal))))
                                (when (memq source '(wake mixed))
                                  (should (eq (plist-get (plist-get info :context) :turn-trigger) 'scheduled-wake)))
                                (should (equal goal-id (plist-get info :mevedel-goal-id)))
                                (should (equal goal-id (plist-get info :mevedel-goal-accounting-id))))
                              (setf (mevedel-goal-status goal) 'paused))
                            (unless external
                              (gptel--fsm-transition (mevedel-request-fsm (caar admitted)) 'DONE))
                            (mevedel-test--await 5 "Request did not settle" (not (mevedel-turn-busy-p buffer)))
                            (when-let* ((goal (mevedel-session-goal session)))
                              (should (= (mevedel-goal-turns-run goal)
                                         (+ turns-before (if (memq source '(wake mixed)) 0 1)))))
                            (should (equal controls (and (eq source 'user) '((:t "push"))))))))))))
            (when (buffer-live-p view) (kill-buffer view)))))))
  (engine)
  :doc "native wake admission drops one-turn overrides and honors live sticky Plan"
  'native
  :doc "ACP wake admission drops one-turn overrides and honors live sticky Plan"
  'acp)

(mevedel-deftest mevedel-pending-inputs--wake-for-event/run-accounting (:quiet t)
  (mevedel-engine-test--with-session :file-workspace t
    (mevedel-request-end)
    (let* ((gptel--known-backends nil)
           (now (floor (float-time))) (mevedel-schedule-clock (lambda () now))
           (view (generate-new-buffer " *wake-run-view*"))
           (mevedel-claude-code-directory (file-name-concat root "claude"))
           (curl (symbol-function 'gptel-curl-get-response))
           (missing-curl (file-name-concat root "missing-curl"))
           notices machine settled release
           (refuse-start (eq ,engine 'start-error))
           (receive (lambda (fsm)
                      (if refuse-start
                          (let ((gptel-use-curl missing-curl)
                                ;; A nonexistent executable cannot contact the provider.
                                (mevedel-test-allow-provider-requests t))
                            (funcall curl fsm))
                        (setq machine fsm))))
           (mevedel-notify-function (lambda (kind details)
                                      (push (list kind (plist-get details :text)) notices))))
      (unwind-protect
          (cl-letf (((symbol-function 'mevedel-claude-code-unavailable-reason) #'ignore)
                    ((symbol-function 'gptel--url-get-response) receive)
                    ((symbol-function 'gptel-curl-get-response) receive)
                    ((symbol-function 'mevedel-claude-code-launch)
                     (mevedel-engine-test--claude-launch
                      (lambda (&rest _) '(:meta ((promptResponse . ((stopReason . "refusal")))))))))
            (mevedel-model-set-session-provider
             session (mevedel-model-resolve-provider
                      (if (eq ,engine 'acp)
                          (progn (mevedel-claude-code-register) "Claude Code:sonnet")
                        (gptel-make-openai "Wake" :key "k" :models '(wake)) "Wake:wake")) buffer)
            (setq-local gptel-use-curl nil gptel-stream nil gptel-tools nil
                        gptel-system-prompt "Wake fixture"
                        gptel-prompt-transform-functions '(mevedel-skills--transform-apply-request-model-policy)
                        gptel-send--handlers (mevedel-preset--build-handlers
                                              (copy-tree (default-value 'gptel-send--handlers))))
            (mevedel-chat-install-request-hooks)
            (when (eq ,engine 'async-preflight)
              (setq-local gptel-prompt-transform-functions
                          (append gptel-prompt-transform-functions
                                  (list (lambda (continue _fsm) (setq release continue))))
                          mevedel-turn-settled-functions (list (lambda (_) (setq settled t)))))
            (mevedel-view--setup view buffer)
            (with-current-buffer view (mevedel-view-test--insert-composer-draft "draft"))
            (let ((wake (mevedel-schedule-create
                         session buffer (if (memq ,engine '(preflight async-preflight)) "1m" "every 1m") "Check CI")))
              (cl-incf now 60)
              (mevedel-schedule-run-due)
              (mevedel-test--await 5 "Wake not delivered" (mevedel-agent-control-wake-pending-p session))
              (mevedel-agent-control-enqueue-monitor-events
               session "mon" "CI" '(("Monitor check finished")) t)
              (mevedel-view-composer-replace "" view)
              (pcase ,engine
                ('async-preflight
                 (mevedel-pending-inputs--wake-for-event buffer)
                 (should release)
                 (should-not mevedel--current-request)
                 (setf (mevedel-session-plan-mode session) t)
                 (funcall release)
                 (should-not machine)
                 (should-not settled))
                ('preflight
                 (cl-letf (((symbol-function 'mevedel-auth-assert-ready)
                            (lambda (_) (error "Token expired"))))
                   (should-error (mevedel-pending-inputs--wake-for-event buffer))))
                ('start-error
                 (let ((unread (mevedel-agent-control-context-mailbox session)))
                   (should-error (mevedel-pending-inputs--wake-for-event buffer)
                                 :type 'file-missing)
                   (setq refuse-start nil)
                   (should-not machine)
                   (should mevedel--current-request)
                   (should-not (mevedel-agent-control-context-mailbox session))
                   (mevedel-request-end)
                   (mevedel-test--await 5 "Failed request teardown did not settle"
                     (not (mevedel-turn-busy-p buffer)))
                   (let ((receipts (mevedel-transcript-audit-buffer-spans 'mailbox-receipt)))
                     (should (equal (mapcar (lambda (record) (plist-get record :id)) unread)
                                    (mapcar (lambda (span) (plist-get (plist-get span :record) :id))
                                            receipts))))))
                (_ (mevedel-pending-inputs--wake-for-event buffer)))
              (when (memq ,engine '(native write-error))
                (mevedel-test--await 5 "Provider not started" machine)
                (setf (gptel-fsm-info machine) (plist-put (gptel-fsm-info machine) :error '(:message "Quota exhausted")))
                (setq-local mevedel-turn-settled-functions (cons (lambda (outcome) (setq settled outcome))
                                                                  mevedel-turn-settled-functions))
                (if (eq ,engine 'write-error)
                    (cl-letf (((symbol-function 'mevedel-agent-control-commit-session)
                               (lambda (&rest _) (error "Sidecar write refused"))))
                      (gptel--fsm-transition machine 'ERRS))
                  (gptel--fsm-transition machine 'ERRS))
                (should (eq 'error settled)))
              (unless (memq ,engine '(start-error preflight async-preflight))
                (mevedel-test--await 5 "Wake did not settle" (not (mevedel-turn-busy-p buffer)))
                (should (eq 'error (mevedel-session-last-outcome session))))
              (if (memq ,engine '(preflight async-preflight))
                  (progn
                    (should (equal '(MONITOR) (mapcar (lambda (r) (plist-get r :type))
                                                            (mevedel-agent-control-context-mailbox session))))
                    (let ((entry (car (mevedel-session-schedules session))))
                      (should (eq 'failed (plist-get entry :status)))
                      (should (mevedel-schedule-entry-p entry))
                      (should (equal (if (eq ,engine 'preflight) "Token expired"
                                       "Implementation context changed during preparation; submit again")
                                     (plist-get (car (plist-get entry :runs)) :reason)))))
                (should-not (mevedel-agent-control-context-mailbox session)))
              (if (eq ,engine 'write-error)
                  (progn (should-not notices)
                         (should-not (plist-get (car (mevedel-schedule-pending session)) :runs)))
                (should (equal '(failure) (mapcar #'car notices)))
                (should (string-search (plist-get wake :id) (cadar notices))))
              (when (eq ,engine 'start-error)
                (let ((runs (plist-get (car (mevedel-session-schedules session)) :runs)))
                  (should (equal '(failed) (mapcar (lambda (run) (plist-get run :outcome)) runs))))
                (mevedel--submit-generated-turn "Continue after the failed start")
                (mevedel-test--await 5 "Next request did not start" machine)
                (let ((payload (prin1-to-string (plist-get (gptel-fsm-info machine) :data))))
                  (dolist (text '("Check CI" "Monitor check finished"))
                    (let ((start (string-search text payload)))
                      (should start)
                      (should-not (string-search text payload (+ start (length text)))))))
                (should (= 2 (length (mevedel-transcript-audit-buffer-spans 'mailbox-receipt))))
                (gptel--fsm-transition machine 'DONE)
                (mevedel-test--await 5 "Next request did not settle" (not (mevedel-turn-busy-p buffer)))
                (should (= 1 (length notices))))
              (when (eq ,engine 'native)
                ;; A user reading wake mail owns their failure.  A successful wake
                ;; resets the streak; an interrupted wake and a skipped cadence do not.
                (dolist (step '((user 1) (user-lost 1) (wake-lost 2) (DONE 2) (ERRS 3) (ABRT 3) (ERRS 4) (ERRS 5)))
                  (setq machine nil)
                  (with-current-buffer view (mevedel-view-test--insert-composer-draft "hold"))
                  (cl-incf now (if (= 3 (cadr step)) 180 60))
                  (mevedel-schedule-run-due)
                  (mevedel-test--await 5 "Missing recurring wake" (mevedel-agent-control-wake-pending-p session))
                  (mevedel-view-composer-replace "" view)
                  (if (memq (car step) '(user user-lost)) (mevedel--submit-generated-turn "My request")
                    (mevedel-pending-inputs--wake-for-event buffer))
                  (mevedel-test--await 5 "Request not admitted" machine)
                  (if (memq (car step) '(user-lost wake-lost))
                      (progn
                        ;; lose the request slot before the provider's terminal event
                        (setq mevedel--current-request nil)
                        (gptel--fsm-transition machine 'ERRS)
                        (mevedel-test--await 5 "Loss not recorded" (eq 'lost (mevedel-session-last-outcome session)))
                        (gptel--fsm-transition machine 'DONE)) ; late duplicate
                    (gptel--fsm-transition machine (if (eq 'user (car step)) 'ERRS (car step))))
                  (mevedel-test--await 5 "Request did not settle" (not (mevedel-turn-busy-p buffer)))
                  (should (= (cadr step) (length notices))))
                (should-not (mevedel-schedule-pending session))
                (should (string-search "paused" (cadar notices)))
                (let ((entry (car (mevedel-session-schedules session))))
                  (should (eq 'paused (plist-get entry :status)))
                  (should (mevedel-schedule-entry-p entry))
                  (should (stringp (plist-get entry :pause-reason))))
                (let* ((mevedel-schedule-max-pending 1)
                       (other (mevedel-schedule-create session buffer "1h" "Another wake-up")))
                  (should-error (mevedel-schedule-resume session buffer (plist-get wake :id))
                                :type 'user-error)
                  (mevedel-schedule-cancel session buffer (plist-get other :id)))
                (mevedel-schedule-resume session buffer (plist-get wake :id))
                (let ((entry (car (mevedel-schedule-pending session))))
                  (should (= (+ now 60) (plist-get entry :due)))
                  (should (zerop (plist-get entry :failures)))))
              (mevedel-schedule-cancel session buffer (plist-get wake :id))))
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

(provide 'test-mevedel-pending-inputs-admission)
;;; test-mevedel-pending-inputs-admission.el ends here
