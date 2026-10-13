;;; test-mevedel-chat-model-baseline.el -- Concurrent baseline selections -*- lexical-binding: t -*-

;;; Commentary:

;; Exercise the public model setters while real request dispatch is on the
;; stack.  gptel's post-request hook and native launch run synchronously there.

;;; Code:

(require 'mevedel-engine-test-support
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "mevedel-engine-test-support"))
(require 'gptel-openai)
(require 'mevedel-chat)
(require 'mevedel-view-composer)
(require 'mevedel-claude-code-session)
(require 'mevedel-implementation-policy)

(defun mevedel-baseline-test--change (session buffer)
  "Change SESSION's baseline through the public setters in BUFFER."
  (mevedel-model-set-session-provider
   session (mevedel-model-resolve-provider "Balanced:balanced-model") buffer)
  (mevedel-model-set-session-effort session 'low buffer))

(defun mevedel-baseline-test--assert (session buffer)
  "Check SESSION's durable and live model selections agree in BUFFER."
  (should (equal "Balanced:balanced-model" (mevedel-session-model-provider session)))
  (should (eq 'low (mevedel-session-reasoning-effort session)))
  (should (eq (gptel-get-backend "Balanced")
              (buffer-local-value 'gptel-backend buffer)))
  (should (eq 'balanced-model (buffer-local-value 'gptel-model buffer)))
  (should (eq 'low (buffer-local-value 'gptel-reasoning-effort buffer))))

(mevedel-deftest mevedel--dispatch-request/model-baseline (:quiet t)
  ,test
  (test)
  :doc "real API post-request setter changes survive scoped implementation dispatch"
  (mevedel-engine-test--with-session
    (mevedel-request-end)
    (mevedel-skills-test--with-model-backends
      (mevedel-model-set-session-provider
       session (mevedel-model-resolve-provider "Fast:fast-model") buffer)
      (mevedel-model-set-session-effort session 'high buffer)
      (setq-local mevedel--pending-implementation-policy
                  '(:model-provider "Balanced:balanced-model"
                    :reasoning-effort nil :permission-mode edits))
      (let ((gptel-use-curl t) (gptel-use-tools nil) (gptel-use-context nil)
            (gptel-send--handlers
             (mevedel-preset--build-handlers (copy-tree gptel-send--handlers)))
            (gptel-send--transitions
             (mevedel-preset--build-transitions (copy-tree gptel-send--transitions)))
            (gptel-prompt-transform-functions
             '(mevedel-skills--transform-apply-request-model-policy))
            (mevedel-journal-enabled nil)
            sent fsm hook-owner)
        (setq-local gptel-post-request-hook
                    (list (lambda ()
                            (setq hook-owner mevedel--current-request)
                            (mevedel-baseline-test--change session buffer))))
        (cl-letf (((symbol-function 'gptel-curl-get-response)
                   (lambda (owner) (push owner sent)))
                  ((symbol-function 'mevedel-auth-assert-ready) #'ignore))
          (setq fsm (mevedel--submit-generated-turn "Implement the accepted outcome"))
          (should (equal (list fsm) sent))
          (should (eq 'WAIT (gptel-fsm-state fsm)))
          (should (eq hook-owner mevedel--current-request))
          (should (eq fsm (mevedel-request-fsm hook-owner)))
          (should (eq 'balanced-model (plist-get (gptel-fsm-info fsm) :model)))
          (should-not (plist-get (gptel-fsm-info fsm) :reasoning-effort))
          (should (eq 'edits (mevedel-request-permission-mode-override mevedel--current-request)))
          (mevedel-baseline-test--assert session buffer)
          (setq-local gptel-post-request-hook nil)
          (gptel--fsm-transition fsm 'ABRT)
          (mevedel-test--await 5 "API baseline fixture did not settle"
            (not (mevedel-turn-busy-p buffer)))
          (mevedel-baseline-test--assert session buffer)
          (should (eq 'low (plist-get (mevedel-skills-request-model-policy) :effort)))
          (let ((fresh (mevedel--submit-generated-turn "Ordinary follow-up")))
            (should (eq 'balanced-model (plist-get (gptel-fsm-info fresh) :model)))
            (should (eq 'low (plist-get (gptel-fsm-info fresh) :reasoning-effort)))
            (should-not (mevedel-request-implementation-policy mevedel--current-request))
            (gptel--fsm-transition fresh 'ABRT)
            (mevedel-test--await 5 "API follow-up fixture did not settle"
              (not (mevedel-turn-busy-p buffer))))))))

  :doc "native readiness, system preparation and launcher setters survive scoped dispatch"
  (dolist (stage '(readiness system launch))
    (mevedel-engine-test--with-session
      (mevedel-request-end)
      (mevedel-skills-test--with-model-backends
        (cl-letf (((symbol-function 'mevedel-claude-code-unavailable-reason) #'ignore))
          (mevedel-claude-code-register)
          (mevedel-model-set-session-provider
           session (mevedel-model-resolve-provider "Fast:fast-model") buffer)
          (mevedel-model-set-session-effort session 'high buffer)
          (setq-local gptel-system-prompt
                      (lambda ()
                        (when (eq stage 'system)
                          (mevedel-baseline-test--change session buffer))
                        (should (equal "haiku"
                                       (gptel--model-name
                                        (plist-get (mevedel-skills-request-model-policy) :model))))
                        "Baseline fixture")
                      gptel-tools nil
                      mevedel--pending-implementation-policy
                      '(:model-provider "Claude Code:haiku"
                        :reasoning-effort nil :permission-mode edits))
          (let ((mevedel-journal-enabled nil)
                (mevedel-claude-code-directory (file-name-concat root "claude"))
                (check (symbol-function 'mevedel-request-assert-target-ready))
                launched owner)
            (cl-letf (((symbol-function 'mevedel-request-assert-target-ready)
                       (lambda (owner)
                         (prog1 (funcall check owner)
                           (when (eq stage 'readiness)
                             (mevedel-baseline-test--change session buffer)))))
                      ((symbol-function 'mevedel-claude-code-launch)
                       (mevedel-engine-test--claude-launch
                        (lambda (_system _mcp model effort &optional _id _hook)
                          (setq launched (list model effort))
                          (when (eq stage 'launch)
                            (mevedel-baseline-test--change session buffer))
                          nil))))
              (setq owner (mevedel--submit-generated-turn "Implement native outcome"))
              (mevedel-test--await 5 "Native baseline fixture did not settle"
                (not (mevedel-turn-busy-p buffer)))
              (should (equal '("haiku" nil) launched))
              (should (equal "haiku" (gptel--model-name (plist-get (mevedel-engine-info owner) :model))))
              (should-not (plist-get (mevedel-engine-info owner) :reasoning-effort))
              (should (eq 'edits (mevedel-request-permission-mode-override owner)))
              (mevedel-baseline-test--assert session buffer)
              (should (eq 'balanced-model (plist-get (mevedel-skills-request-model-policy) :model)))
              (should (eq 'low (plist-get (mevedel-skills-request-model-policy) :effort))))))))))

(provide 'test-mevedel-chat-model-baseline)
;;; test-mevedel-chat-model-baseline.el ends here
