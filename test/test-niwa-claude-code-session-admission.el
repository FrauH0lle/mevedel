;;; test-niwa-claude-code-session-admission.el -- Native startup fences -*- lexical-binding: t -*-

;;; Commentary:

;; Exercise native generated sends through real readiness/admission and ACP
;; startup.  Only the installed adapter is replaced with the fixture peer.

;;; Code:

(require 'niwa-engine-test-support
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "niwa-engine-test-support"))
(require 'niwa-claude-code-session)
(require 'niwa-goal)

(niwa-deftest niwa-claude-code-send/admission (:quiet t)
  ,test
  (test)
  :doc "yielding readiness rejects changed Goal authority before native launch"
  (dolist (change '(pause clear replace edit policy resume))
    ;; Keep project publication for pause; the other mutations fence the same
    ;; live Goal identity before dispatch and still run the real ACP peer.
    (niwa-engine-test--with-claude-session :file-workspace (not (eq change 'pause))
      (niwa-request-end)
      (niwa-model-set-session-provider
       session (niwa-model-resolve-provider "Claude Code:haiku") buffer)
      (niwa-model-set-session-effort session nil buffer)
      (niwa-permission-mode-set-raw 'ask)
      (setq-local gptel-system-prompt "Native admission fixture" gptel-tools nil)
      (let* ((policy '(:model-provider "Claude Code:sonnet"
                      :reasoning-effort high :permission-mode edits))
             (goal (niwa-goal-create "Implement accepted outcome"
                                        :session session :implementation-policy policy))
             (check (symbol-function 'niwa-request-assert-target-ready))
             (niwa-journal-enabled nil)
             checked owners launches)
        (when (eq change 'resume) (niwa-goal-pause))
        (cl-letf (((symbol-function 'niwa-claude-code-launch)
                   (niwa-engine-test--claude-launch
                    (lambda (_system _mcp model effort &optional _id _hook)
                      (push niwa--current-request owners)
                      (push (list model effort) launches)
                      ;; Stop automatic continuation without changing the
                      ;; already-admitted request's model or Goal attribution.
                      (when (eq 'active (niwa-goal-status goal))
                        (niwa-goal-pause))
                      nil))))
          (cl-letf (((symbol-function 'niwa-request-assert-target-ready)
                     (lambda (owner)
                       (prog1 (funcall check owner)
                         (setq checked t)
                         (should-not niwa--current-request)
                         (niwa-model-set-session-provider
                          session (niwa-model-resolve-provider "Claude Code:opus") buffer)
                         (niwa-model-set-session-effort session 'medium buffer)
                         (pcase change
                           ('pause (niwa-goal-pause))
                           ('clear (niwa-goal-clear))
                           ('replace
                            (niwa-goal-clear)
                            (setq goal (niwa-goal-create
                                        "Replacement" :session session
                                        :implementation-policy policy)))
                           ('edit (niwa-goal-edit "Revised objective"))
                           ('policy
                            (setf (niwa-goal-implementation-policy goal)
                                  '(:model-provider "Claude Code:sonnet"
                                    :reasoning-effort nil :permission-mode full-auto)))
                           ('resume (niwa-goal-resume)))
                         ;; Baseline remains live even while startup is on stack.
                         (should (equal "opus" (gptel--model-name gptel-model)))
                         (should (eq 'medium gptel-reasoning-effort))))))
            (should-error (niwa--submit-generated-turn "Start native implementation")
                          :type 'niwa-stale-implementation-preparation))
          (should checked)
          (should-not launches)
          (should-not niwa--current-request)
          (should-not (niwa-turn-busy-p buffer))
          (should (eq (pcase change
                        ('clear nil) ('pause 'paused) (_ 'active))
                      (and (niwa-session-goal session) (niwa-goal-status goal))))
          (let* ((expected (niwa-implementation-policy-current))
                 (goal-id (and expected (niwa-goal-id goal)))
                 (owner (niwa--submit-generated-turn "Submit with current authority")))
            (niwa-test--await 5 "Fresh native admission did not settle"
              (not (niwa-turn-busy-p buffer)))
            (should (equal (list owner) owners))
            (should (equal (list (if expected
                                    (list "sonnet" (plist-get expected :reasoning-effort))
                                  '("opus" medium)))
                           launches))
            (should (equal expected (niwa-request-implementation-policy owner)))
            (should (eq (plist-get expected :permission-mode)
                        (niwa-request-permission-mode-override owner)))
            (should (equal goal-id (plist-get (niwa-engine-info owner) :niwa-goal-id)))
            (should (eq (or (plist-get expected :permission-mode) 'ask)
                        (niwa-permission-mode-effective session buffer nil owner))))
          (should (equal "Claude Code:opus" (niwa-session-model-provider session)))
          (should (eq 'medium (niwa-session-reasoning-effort session)))
          (should (equal "opus" (gptel--model-name gptel-model)))
          (should (eq 'medium gptel-reasoning-effort)))))))

(niwa-deftest niwa--implement-plan/goal-insertion (:quiet t)
  ,test
  (test)
  :doc "real insertion hooks cannot transfer Goal kickoff policy to inactive or replacement work"
  (dolist (case '((insertion pause) (insertion clear) (insertion replace) (insertion edit)
                 (close pause) (close clear) (close replace) (close edit)))
    ;; File saves can rewrite the leading property drawer inside the insertion
    ;; hook.  Keep insertion/pause as a portable-project control too.
    (niwa-engine-test--with-claude-session :file-workspace (not (equal case '(insertion pause)))
      (niwa-request-end)
      (niwa-permission-mode-set-raw 'ask)
      (setq-local gptel-system-prompt "Goal insertion fixture" gptel-tools nil)
      (let* ((change (cadr case))
             (policy '(:model-provider "Claude Code:sonnet"
                      :reasoning-effort high :permission-mode full-auto))
             (goal (niwa-goal-create "Accepted kickoff" :session session
                                        :implementation-policy policy))
             (submission (niwa-prompt-submission-create :session session))
             (niwa-journal-enabled nil)
             changed launches)
        (niwa-prompt-submission-accept submission "Implement accepted outcome" nil nil nil)
        (when (eq (car case) 'close)
          (should (derived-mode-p 'org-mode))
          (insert "#+begin_reasoning\nUnfinished response\n"))
        (setq-local after-change-functions
                    (list (lambda (&rest _)
                            (when (and niwa--pending-implementation-policy (not changed))
                              (setq changed t)
                              (pcase change
                                ('pause (niwa-goal-pause))
                                ('clear (niwa-goal-clear))
                                ('replace
                                 (niwa-goal-clear)
                                 (setq goal (niwa-goal-create
                                             "Replacement" :session session
                                             :implementation-policy policy)))
                                ('edit (niwa-goal-edit "Revised kickoff")))))))
        (cl-letf (((symbol-function 'niwa-claude-code-launch)
                   (niwa-engine-test--claude-launch
                    (lambda (&rest _)
                      (push niwa--current-request launches)
                      nil))))
          (should-error
           (niwa--implement-plan
            (list :implementation-policy policy :prompt-submission submission
                  :implementation-goal goal :implementation-goal-id (niwa-goal-id goal)))
           :type 'niwa-stale-implementation-preparation))
        (should changed)
        (should-not launches)
        (should-not niwa--current-request)
        (should-not niwa--pending-implementation-policy)
        (should (eq (pcase change ('clear nil) ('pause 'paused) (_ 'active))
                    (and (niwa-session-goal session) (niwa-goal-status goal))))))))

(provide 'test-niwa-claude-code-session-admission)
;;; test-niwa-claude-code-session-admission.el ends here
