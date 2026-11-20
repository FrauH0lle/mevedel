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
(require 'mevedel-collaboration)
(require 'mevedel-collaboration-transport)
(require 'mevedel-pending-inputs)
(require 'mevedel-permissions)
(require 'mevedel-presets)
(require 'mevedel-schedule)
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
                                 (with-current-buffer view
                                   (delete-region (mevedel-view--input-start) (point-max)))))
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

(provide 'test-mevedel-pending-inputs-admission)
;;; test-mevedel-pending-inputs-admission.el ends here
