;;; test-mevedel-implementation-failure.el -- Scoped provider failures -*- lexical-binding: t -*-

;;; Commentary:

;; Exercise structured provider rejection through the real request admission
;; and terminal handlers, including concurrent edits to the session baseline.

;;; Code:

(require 'mevedel-engine-test-support
         (file-name-concat
          (file-name-directory (or buffer-file-name load-file-name))
          "mevedel-engine-test-support"))
(require 'gptel-openai)
(require 'mevedel-implementation-policy)
(require 'mevedel-goal)

(mevedel-deftest mevedel--turn-record-request-failure/implementation (:quiet t)
  ,test
  (test)
  :doc "a structured accepted-model rejection preserves the live and durable baseline"
  (dolist (execution '(direct goal))
    (mevedel-engine-test--with-session
      (mevedel-request-end)
      (mevedel-skills-test--with-model-backends
        (gptel-make-openai "Fallback" :key "test" :models '(fallback-model))
        (mevedel-model-set-session-provider
         session (mevedel-model-resolve-provider "Fast:fast-model") buffer)
        (mevedel-model-set-session-effort session 'high buffer)
        (mevedel-permission-mode-set-raw 'ask)
        (let* ((policy '(:model-provider "Balanced:balanced-model"
                        :reasoning-effort nil :permission-mode full-auto))
               (mevedel-model-fallback-provider "Fallback:fallback-model")
               (gptel-use-curl t) (gptel-use-tools nil) (gptel-use-context nil)
               (mevedel-journal-enabled nil)
               (gptel-send--handlers
                (mevedel-preset--build-handlers (copy-tree gptel-send--handlers)))
               (gptel-send--transitions
                (mevedel-preset--build-transitions (copy-tree gptel-send--transitions)))
               (gptel-prompt-transform-functions
                '(mevedel-skills--transform-apply-request-model-policy))
               goal fsm owner sent)
          (if (eq execution 'goal)
              (setq goal (mevedel-goal-create
                          "Implement accepted outcome" :session session
                          :implementation-policy policy :token-budget 200))
            (setq-local mevedel--pending-implementation-policy (copy-tree policy)))
          (cl-letf (((symbol-function 'gptel-curl-get-response)
                     (lambda (machine) (push machine sent))))
            (setq fsm (mevedel--submit-generated-turn "Implement accepted outcome")
                  owner mevedel--current-request)
            (should (eq 'WAIT (gptel-fsm-state fsm)))
            (should (eq owner (plist-get (gptel-fsm-info fsm) :mevedel-request)))
            (should (equal policy (mevedel-request-implementation-policy owner)))
            (should (eq 'balanced-model (plist-get (gptel-fsm-info fsm) :model)))
            ;; A baseline edit during implementation must survive failure too.
            (mevedel-model-set-session-effort session 'low buffer)
            (let ((info (gptel-fsm-info fsm)))
              (setq info (plist-put info :http-status "404")
                    info (plist-put info :status "404 Not Found")
                    info (plist-put info :error
                                    '(:code "model_not_found" :type "invalid_request_error"
                                      :message "Accepted model disappeared")))
              (setf (gptel-fsm-info fsm) info))
            (gptel--fsm-transition fsm 'ERRS)
            (mevedel-test--await 5 "Implementation model rejection did not settle"
              (not (mevedel-turn-busy-p buffer)))
            (should (= 1 (length sent)))
            (should (equal "Fast:fast-model" (mevedel-session-model-provider session)))
            (should (eq 'low (mevedel-session-reasoning-effort session)))
            (should (eq (gptel-get-backend "Fast") gptel-backend))
            (should (eq 'fast-model gptel-model))
            (should (eq 'low gptel-reasoning-effort))
            (should (eq 'ask (mevedel-session-permission-mode session)))
            (should-not mevedel--pending-implementation-policy)
            (should (equal policy (mevedel-request-implementation-policy owner)))
            (when goal
              (should (eq goal (mevedel-session-goal session)))
              (should (eq 'paused (mevedel-goal-status goal)))
              (should (equal policy (mevedel-goal-implementation-policy goal))))
            (let* ((saved (mevedel-session-codec-read
                           (mevedel-session-artifacts-sidecar-path
                            (mevedel-session-save-path session)))))
              (should (equal "Fast:fast-model" (plist-get saved :model-provider)))
              (should (eq 'low (plist-get saved :reasoning-effort))))
            (let ((ordinary (mevedel--submit-generated-turn "Ordinary follow-up")))
              (should (eq 'fast-model (plist-get (gptel-fsm-info ordinary) :model)))
              (should (eq 'low (plist-get (gptel-fsm-info ordinary) :reasoning-effort)))
              (should-not (mevedel-request-implementation-policy mevedel--current-request))
              (should-not (mevedel-request-permission-mode-override mevedel--current-request))
              (gptel--fsm-transition ordinary 'ABRT)
              (mevedel-test--await 5 "Ordinary follow-up did not settle"
                (not (mevedel-turn-busy-p buffer))))))))))

(provide 'test-mevedel-implementation-failure)
;;; test-mevedel-implementation-failure.el ends here
