;;; test-mevedel-claude-code-session-admission.el -- Native startup fences -*- lexical-binding: t -*-

;;; Commentary:

;; Exercise native generated sends through real readiness/admission and ACP
;; startup.  Only the installed adapter is replaced with the fixture peer.

;;; Code:

(require 'mevedel-engine-test-support
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "mevedel-engine-test-support"))
(require 'mevedel-claude-code-session)
(require 'mevedel-goal)

(mevedel-deftest mevedel-claude-code-send/admission (:quiet t)
  ,test
  (test)
  :doc "yielding readiness rejects changed Goal authority before native launch"
  (dolist (change '(pause clear replace edit policy resume))
    (mevedel-engine-test--with-claude-session
      (mevedel-request-end)
      (mevedel-model-set-session-provider
       session (mevedel-model-resolve-provider "Claude Code:haiku") buffer)
      (mevedel-model-set-session-effort session nil buffer)
      (mevedel-permission-mode-set-raw 'ask)
      (setq-local gptel-system-prompt "Native admission fixture" gptel-tools nil)
      (let* ((policy '(:model-provider "Claude Code:sonnet"
                      :reasoning-effort high :permission-mode edits))
             (goal (mevedel-goal-create "Implement accepted outcome"
                                        :session session :implementation-policy policy))
             (check (symbol-function 'mevedel-request-assert-target-ready))
             (mevedel-journal-enabled nil)
             checked owners launches)
        (when (eq change 'resume) (mevedel-goal-pause))
        (cl-letf (((symbol-function 'mevedel-claude-code-launch)
                   (mevedel-engine-test--claude-launch
                    (lambda (_system _mcp model effort &optional _id _hook)
                      (push mevedel--current-request owners)
                      (push (list model effort) launches)
                      ;; Stop automatic continuation without changing the
                      ;; already-admitted request's model or Goal attribution.
                      (when (eq 'active (mevedel-goal-status goal))
                        (mevedel-goal-pause))
                      nil))))
          (cl-letf (((symbol-function 'mevedel-request-assert-target-ready)
                     (lambda (owner)
                       (prog1 (funcall check owner)
                         (setq checked t)
                         (should-not mevedel--current-request)
                         (mevedel-model-set-session-provider
                          session (mevedel-model-resolve-provider "Claude Code:opus") buffer)
                         (mevedel-model-set-session-effort session 'medium buffer)
                         (pcase change
                           ('pause (mevedel-goal-pause))
                           ('clear (mevedel-goal-clear))
                           ('replace
                            (mevedel-goal-clear)
                            (setq goal (mevedel-goal-create
                                        "Replacement" :session session
                                        :implementation-policy policy)))
                           ('edit (mevedel-goal-edit "Revised objective"))
                           ('policy
                            (setf (mevedel-goal-implementation-policy goal)
                                  '(:model-provider "Claude Code:sonnet"
                                    :reasoning-effort nil :permission-mode full-auto)))
                           ('resume (mevedel-goal-resume)))
                         ;; Baseline remains live even while startup is on stack.
                         (should (equal "opus" (gptel--model-name gptel-model)))
                         (should (eq 'medium gptel-reasoning-effort))))))
            (should-error (mevedel--submit-generated-turn "Start native implementation")
                          :type 'mevedel-stale-implementation-preparation))
          (should checked)
          (should-not launches)
          (should-not mevedel--current-request)
          (should-not (mevedel-turn-busy-p buffer))
          (should (eq (pcase change
                        ('clear nil) ('pause 'paused) (_ 'active))
                      (and (mevedel-session-goal session) (mevedel-goal-status goal))))
          (let* ((expected (mevedel-implementation-policy-current))
                 (goal-id (and expected (mevedel-goal-id goal)))
                 (owner (mevedel--submit-generated-turn "Submit with current authority")))
            (mevedel-test--await 5 "Fresh native admission did not settle"
              (not (mevedel-turn-busy-p buffer)))
            (should (equal (list owner) owners))
            (should (equal (list (if expected
                                    (list "sonnet" (plist-get expected :reasoning-effort))
                                  '("opus" medium)))
                           launches))
            (should (equal expected (mevedel-request-implementation-policy owner)))
            (should (eq (plist-get expected :permission-mode)
                        (mevedel-request-permission-mode-override owner)))
            (should (equal goal-id (plist-get (mevedel-engine-info owner) :mevedel-goal-id)))
            (should (eq (or (plist-get expected :permission-mode) 'ask)
                        (mevedel-permission-mode-effective session buffer nil owner))))
          (should (equal "Claude Code:opus" (mevedel-session-model-provider session)))
          (should (eq 'medium (mevedel-session-reasoning-effort session)))
          (should (equal "opus" (gptel--model-name gptel-model)))
          (should (eq 'medium gptel-reasoning-effort)))))))

(mevedel-deftest mevedel--implement-plan/goal-insertion (:quiet t)
  ,test
  (test)
  :doc "real insertion hooks cannot transfer Goal kickoff policy to inactive or replacement work"
  (dolist (case '((insertion pause) (insertion clear) (insertion replace) (insertion edit)
                 (close pause) (close clear) (close replace) (close edit)))
    (mevedel-engine-test--with-claude-session
      (mevedel-request-end)
      (mevedel-permission-mode-set-raw 'ask)
      (setq-local gptel-system-prompt "Goal insertion fixture" gptel-tools nil)
      (let* ((change (cadr case))
             (policy '(:model-provider "Claude Code:sonnet"
                      :reasoning-effort high :permission-mode full-auto))
             (goal (mevedel-goal-create "Accepted kickoff" :session session
                                        :implementation-policy policy))
             (submission (mevedel-prompt-submission-create :session session))
             (mevedel-journal-enabled nil)
             changed launches)
        (mevedel-prompt-submission-accept submission "Implement accepted outcome" nil nil nil)
        (when (eq (car case) 'close)
          (should (derived-mode-p 'org-mode))
          (insert "#+begin_reasoning\nUnfinished response\n"))
        (setq-local after-change-functions
                    (list (lambda (&rest _)
                            (when (and mevedel--pending-implementation-policy (not changed))
                              (setq changed t)
                              (pcase change
                                ('pause (mevedel-goal-pause))
                                ('clear (mevedel-goal-clear))
                                ('replace
                                 (mevedel-goal-clear)
                                 (setq goal (mevedel-goal-create
                                             "Replacement" :session session
                                             :implementation-policy policy)))
                                ('edit (mevedel-goal-edit "Revised kickoff")))))))
        (cl-letf (((symbol-function 'mevedel-claude-code-launch)
                   (mevedel-engine-test--claude-launch
                    (lambda (&rest _)
                      (push mevedel--current-request launches)
                      nil))))
          (should-error
           (mevedel--implement-plan
            (list :implementation-policy policy :prompt-submission submission
                  :implementation-goal goal :implementation-goal-id (mevedel-goal-id goal)))
           :type 'mevedel-stale-implementation-preparation))
        (should changed)
        (should-not launches)
        (should-not mevedel--current-request)
        (should-not mevedel--pending-implementation-policy)
        (should (eq (pcase change ('clear nil) ('pause 'paused) (_ 'active))
                    (and (mevedel-session-goal session) (mevedel-goal-status goal))))))))

(provide 'test-mevedel-claude-code-session-admission)
;;; test-mevedel-claude-code-session-admission.el ends here
