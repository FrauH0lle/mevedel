;;; test-niwa-chat-model-baseline.el -- Concurrent baseline selections -*- lexical-binding: t -*-

;;; Commentary:

;; Exercise the public model setters while real request dispatch is on the
;; stack.  gptel's post-request hook and native launch run synchronously there.

;;; Code:

(require 'niwa-engine-test-support
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "niwa-engine-test-support"))
(require 'gptel-openai)
(require 'niwa-chat)
(require 'niwa-view-composer)
(require 'niwa-claude-code-session)
(require 'niwa-implementation-policy)

(defun niwa-baseline-test--change (session buffer)
  "Change SESSION's baseline through the public setters in BUFFER."
  (niwa-model-set-session-provider
   session (niwa-model-resolve-provider "Balanced:balanced-model") buffer)
  (niwa-model-set-session-effort session 'low buffer))

(defun niwa-baseline-test--assert (session buffer)
  "Check SESSION's durable and live model selections agree in BUFFER."
  (should (equal "Balanced:balanced-model" (niwa-session-model-provider session)))
  (should (eq 'low (niwa-session-reasoning-effort session)))
  (should (eq (gptel-get-backend "Balanced")
              (buffer-local-value 'gptel-backend buffer)))
  (should (eq 'balanced-model (buffer-local-value 'gptel-model buffer)))
  (should (eq 'low (buffer-local-value 'gptel-reasoning-effort buffer))))

(niwa-deftest niwa--dispatch-request/model-baseline (:quiet t)
  ,test
  (test)
  :doc "real API post-request setter changes survive scoped implementation dispatch"
  (niwa-engine-test--with-session
    (niwa-request-end)
    (niwa-skills-test--with-model-backends
      (niwa-model-set-session-provider
       session (niwa-model-resolve-provider "Fast:fast-model") buffer)
      (niwa-model-set-session-effort session 'high buffer)
      (setq-local niwa--pending-implementation-policy
                  '(:model-provider "Balanced:balanced-model"
                    :reasoning-effort nil :permission-mode edits))
      (let ((gptel-use-curl t) (gptel-use-tools nil) (gptel-use-context nil)
            (gptel-send--handlers
             (niwa-preset--build-handlers (copy-tree gptel-send--handlers)))
            (gptel-send--transitions
             (niwa-preset--build-transitions (copy-tree gptel-send--transitions)))
            (gptel-prompt-transform-functions
             '(niwa-skills--transform-apply-request-model-policy))
            (niwa-journal-enabled nil)
            sent fsm hook-owner)
        (setq-local gptel-post-request-hook
                    (list (lambda ()
                            (setq hook-owner niwa--current-request)
                            (niwa-baseline-test--change session buffer))))
        (cl-letf (((symbol-function 'gptel-curl-get-response)
                   (lambda (owner) (push owner sent)))
                  ((symbol-function 'niwa-auth-assert-ready) #'ignore))
          (setq fsm (niwa--submit-generated-turn "Implement the accepted outcome"))
          (should (equal (list fsm) sent))
          (should (eq 'WAIT (gptel-fsm-state fsm)))
          (should (eq hook-owner niwa--current-request))
          (should (eq fsm (niwa-request-fsm hook-owner)))
          (should (eq 'balanced-model (plist-get (gptel-fsm-info fsm) :model)))
          (should-not (plist-get (gptel-fsm-info fsm) :reasoning-effort))
          (should (eq 'edits (niwa-request-permission-mode-override niwa--current-request)))
          (niwa-baseline-test--assert session buffer)
          (setq-local gptel-post-request-hook nil)
          (gptel--fsm-transition fsm 'ABRT)
          (niwa-test--await 5 "API baseline fixture did not settle"
            (not (niwa-turn-busy-p buffer)))
          (niwa-baseline-test--assert session buffer)
          (should (eq 'low (plist-get (niwa-skills-request-model-policy) :effort)))
          (let ((fresh (niwa--submit-generated-turn "Ordinary follow-up")))
            (should (eq 'balanced-model (plist-get (gptel-fsm-info fresh) :model)))
            (should (eq 'low (plist-get (gptel-fsm-info fresh) :reasoning-effort)))
            (should-not (niwa-request-implementation-policy niwa--current-request))
            (gptel--fsm-transition fresh 'ABRT)
            (niwa-test--await 5 "API follow-up fixture did not settle"
              (not (niwa-turn-busy-p buffer))))))))

  :doc "native readiness, system preparation and launcher setters survive scoped dispatch"
  (dolist (stage '(readiness system launch))
    (niwa-engine-test--with-session
      (niwa-request-end)
      (niwa-skills-test--with-model-backends
        (cl-letf (((symbol-function 'niwa-claude-code-unavailable-reason) #'ignore))
          (niwa-claude-code-register)
          (niwa-model-set-session-provider
           session (niwa-model-resolve-provider "Fast:fast-model") buffer)
          (niwa-model-set-session-effort session 'high buffer)
          (setq-local gptel-system-prompt
                      (lambda ()
                        (when (eq stage 'system)
                          (niwa-baseline-test--change session buffer))
                        (should (equal "haiku"
                                       (gptel--model-name
                                        (plist-get (niwa-skills-request-model-policy) :model))))
                        "Baseline fixture")
                      gptel-tools nil
                      niwa--pending-implementation-policy
                      '(:model-provider "Claude Code:haiku"
                        :reasoning-effort nil :permission-mode edits))
          (let ((niwa-journal-enabled nil)
                (niwa-claude-code-directory (file-name-concat root "claude"))
                (check (symbol-function 'niwa-request-assert-target-ready))
                launched owner)
            (cl-letf (((symbol-function 'niwa-request-assert-target-ready)
                       (lambda (owner)
                         (prog1 (funcall check owner)
                           (when (eq stage 'readiness)
                             (niwa-baseline-test--change session buffer)))))
                      ((symbol-function 'niwa-claude-code-launch)
                       (niwa-engine-test--claude-launch
                        (lambda (_system _mcp model effort &optional _id _hook)
                          (setq launched (list model effort))
                          (when (eq stage 'launch)
                            (niwa-baseline-test--change session buffer))
                          nil))))
              (setq owner (niwa--submit-generated-turn "Implement native outcome"))
              (niwa-test--await 5 "Native baseline fixture did not settle"
                (not (niwa-turn-busy-p buffer)))
              (should (equal '("haiku" nil) launched))
              (should (equal "haiku" (gptel--model-name (plist-get (niwa-engine-info owner) :model))))
              (should-not (plist-get (niwa-engine-info owner) :reasoning-effort))
              (should (eq 'edits (niwa-request-permission-mode-override owner)))
              (niwa-baseline-test--assert session buffer)
              (should (eq 'balanced-model (plist-get (niwa-skills-request-model-policy) :model)))
              (should (eq 'low (plist-get (niwa-skills-request-model-policy) :effort))))))))))

(provide 'test-niwa-chat-model-baseline)
;;; test-niwa-chat-model-baseline.el ends here
