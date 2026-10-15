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
(require 'mevedel-pending-inputs)
(require 'mevedel-permissions)
(require 'mevedel-presets)
(require 'mevedel-view-composer)

(mevedel-deftest mevedel-pending-inputs--wake-for-event/admission (:quiet t)
  (dolist (implementation '(nil t))
    (dolist (sticky-plan '(nil t))
      (mevedel-engine-test--with-session
        (mevedel-request-end)
        (let ((gptel--known-backends nil)
              (mevedel-claude-code-directory (file-name-concat root "claude"))
              (view (generate-new-buffer " *wake-admission-view*"))
              admitted)
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
                  (cl-letf (((symbol-function 'gptel-curl-get-response)
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
                        (not (mevedel-turn-busy-p buffer)))))))
            (when (buffer-live-p view) (kill-buffer view)))))))
  (engine)
  :doc "native wake admission drops one-turn overrides and honors live sticky Plan"
  'native
  :doc "ACP wake admission drops one-turn overrides and honors live sticky Plan"
  'acp)

(provide 'test-mevedel-pending-inputs-admission)
;;; test-mevedel-pending-inputs-admission.el ends here
