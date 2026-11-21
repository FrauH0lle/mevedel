;;; test-niwa-implementation-failure.el -- Scoped provider failures -*- lexical-binding: t -*-

;;; Commentary:

;; Exercise structured provider rejection through the real request admission
;; and terminal handlers, including concurrent edits to the session baseline.

;;; Code:

(require 'niwa-engine-test-support
         (file-name-concat
          (file-name-directory (or buffer-file-name load-file-name))
          "niwa-engine-test-support"))
(require 'gptel-openai)
(require 'niwa-implementation-policy)
(require 'niwa-goal)

(niwa-deftest niwa--turn-record-request-failure/implementation (:quiet t)
  ,test
  (test)
  :doc "a structured accepted-model rejection preserves the live and durable baseline"
  (dolist (execution '(direct goal))
    (niwa-engine-test--with-session
      (niwa-request-end)
      (niwa-skills-test--with-model-backends
        (gptel-make-openai "Fallback" :key "test" :models '(fallback-model))
        (niwa-model-set-session-provider
         session (niwa-model-resolve-provider "Fast:fast-model") buffer)
        (niwa-model-set-session-effort session 'high buffer)
        (niwa-permission-mode-set-raw 'ask)
        (let* ((policy '(:model-provider "Balanced:balanced-model"
                        :reasoning-effort nil :permission-mode full-auto))
               (niwa-model-fallback-provider "Fallback:fallback-model")
               (gptel-use-curl t) (gptel-use-tools nil) (gptel-use-context nil)
               (niwa-journal-enabled nil)
               (gptel-send--handlers
                (niwa-preset--build-handlers (copy-tree gptel-send--handlers)))
               (gptel-send--transitions
                (niwa-preset--build-transitions (copy-tree gptel-send--transitions)))
               (gptel-prompt-transform-functions
                '(niwa-skills--transform-apply-request-model-policy))
               goal fsm owner sent)
          (if (eq execution 'goal)
              (setq goal (niwa-goal-create
                          "Implement accepted outcome" :session session
                          :implementation-policy policy :token-budget 200))
            (setq-local niwa--pending-implementation-policy (copy-tree policy)))
          (cl-letf (((symbol-function 'gptel-curl-get-response)
                     (lambda (machine) (push machine sent))))
            (setq fsm (niwa--submit-generated-turn "Implement accepted outcome")
                  owner niwa--current-request)
            (should (eq 'WAIT (gptel-fsm-state fsm)))
            (should (eq owner (plist-get (gptel-fsm-info fsm) :niwa-request)))
            (should (equal policy (niwa-request-implementation-policy owner)))
            (should (eq 'balanced-model (plist-get (gptel-fsm-info fsm) :model)))
            ;; A baseline edit during implementation must survive failure too.
            (niwa-model-set-session-effort session 'low buffer)
            (let ((info (gptel-fsm-info fsm)))
              (setq info (plist-put info :http-status "404")
                    info (plist-put info :status "404 Not Found")
                    info (plist-put info :error
                                    '(:code "model_not_found" :type "invalid_request_error"
                                      :message "Accepted model disappeared")))
              (setf (gptel-fsm-info fsm) info))
            (gptel--fsm-transition fsm 'ERRS)
            (niwa-test--await 5 "Implementation model rejection did not settle"
              (not (niwa-turn-busy-p buffer)))
            (should (= 1 (length sent)))
            (should (equal "Fast:fast-model" (niwa-session-model-provider session)))
            (should (eq 'low (niwa-session-reasoning-effort session)))
            (should (eq (gptel-get-backend "Fast") gptel-backend))
            (should (eq 'fast-model gptel-model))
            (should (eq 'low gptel-reasoning-effort))
            (should (eq 'ask (niwa-session-permission-mode session)))
            (should-not niwa--pending-implementation-policy)
            (should (equal policy (niwa-request-implementation-policy owner)))
            (when goal
              (should (eq goal (niwa-session-goal session)))
              (should (eq 'paused (niwa-goal-status goal)))
              (should (equal policy (niwa-goal-implementation-policy goal))))
            (let* ((saved (niwa-session-codec-read
                           (niwa-session-artifacts-sidecar-path
                            (niwa-session-save-path session)))))
              (should (equal "Fast:fast-model" (plist-get saved :model-provider)))
              (should (eq 'low (plist-get saved :reasoning-effort))))
            (let ((ordinary (niwa--submit-generated-turn "Ordinary follow-up")))
              (should (eq 'fast-model (plist-get (gptel-fsm-info ordinary) :model)))
              (should (eq 'low (plist-get (gptel-fsm-info ordinary) :reasoning-effort)))
              (should-not (niwa-request-implementation-policy niwa--current-request))
              (should-not (niwa-request-permission-mode-override niwa--current-request))
              (gptel--fsm-transition ordinary 'ABRT)
              (niwa-test--await 5 "Ordinary follow-up did not settle"
                (not (niwa-turn-busy-p buffer))))))))))

(provide 'test-niwa-implementation-failure)
;;; test-niwa-implementation-failure.el ends here
