;;; test-mevedel-plan-handoff-staged-owner.el -- Staged kickoff ownership -*- lexical-binding: t -*-

;;; Commentary:

;; Public composer input reenters real accepted dispatch after policy staging.
;; Ordinary input must be refused, not admitted with the kickoff's settings.
;; Exercise both insertion/closing hooks and native yielding admission checks.

;;; Code:

(require 'test-mevedel-plan-handoff-owner
         (file-name-concat
          (file-name-directory (or load-file-name buffer-file-name))
          "test-mevedel-plan-handoff-owner"))
(require 'mevedel-claude-code-session)

(mevedel-deftest mevedel--implement-plan/staged-owner (:quiet t)
  ,test
  (test)
  :doc "ordinary plain and source-backed skill sends cannot consume staged Direct policy"
  (progn
    (dolist (stage '(close insertion))
      (dolist (kind '(plain skill))
        (mevedel-handoff-owner-test--with-target
          (setf (plist-get selection :execution) 'direct)
          (let* ((view (generate-new-buffer " *staged-owner-view*"))
                 (skills-root (file-name-concat root ".mevedel" "skills"))
                 (mevedel-skill-dirs (list skills-root))
                 (mevedel-skills-include-bundled nil)
                 (mevedel-skills-check-for-modifications nil)
                 (ordinary-prompt (if (eq kind 'skill)
                                      "$alpha A separate ordinary question."
                                    "A separate ordinary question."))
                 (accepted-prompt (mevedel-prompt-submission-input submission))
                 (accepted-context (list :effort nil))
                 attempted blocked newer sent draft observations)
            (unwind-protect
                (save-window-excursion
                  (when (eq kind 'skill)
                    (mevedel-skills-test--write-skill
                     skills-root "alpha"
                     "name: alpha\ndescription: Ordinary staged ownership fixture\ncontext: inline\nallowed-tools: Read\n"
                     "Read evidence for the separate ordinary question.")
                    (mevedel-skills-install session buffer))
                  (mevedel-prompt-submission-set-outcome
                   submission (list :model-input accepted-prompt :transcript-input accepted-prompt
                                    :request-context accepted-context))
                  (mevedel-view--setup view buffer)
                  (set-window-buffer (selected-window) view)
                  (with-current-buffer view
                    (goto-char (mevedel-view--input-start))
                    (insert ordinary-prompt)
                    (setq draft (mevedel-view--input-text)))
                  (when (eq stage 'close)
                    (should (derived-mode-p 'org-mode))
                    (goto-char (point-max))
                    (insert "#+begin_reasoning\nUnfinished response\n"))
                  (add-hook
                   'after-change-functions
                   (lambda (&rest _)
                     (when (and mevedel--pending-implementation-policy (not attempted))
                       (setq attempted t)
                       (should-not mevedel--current-request)
                       (let ((context mevedel-skills--pending-request-context)
                             (pending mevedel--pending-implementation-policy)
                             (text (buffer-string)))
                         ;; No synthesized request: the actual public producer
                         ;; has a distinct ordinary composer prompt.
                         (with-current-buffer view
                           (condition-case err
                               (mevedel-view-send)
                             (user-error
                              (setq blocked (error-message-string err)))))
                         (setq newer mevedel--current-request
                               observations
                               (list :text-unchanged (equal text (buffer-string))
                                     :context-unchanged (eq context mevedel-skills--pending-request-context)
                                     :pending-unchanged (eq pending mevedel--pending-implementation-policy))))
                       ;; The user's baseline edit is not owned by kickoff.
                       (mevedel-model-set-session-effort session 'low buffer)))
                   nil t)
                  (cl-letf (((symbol-function 'mevedel-auth-assert-ready) #'ignore)
                            ((symbol-function 'gptel-curl-get-response)
                             (lambda (fsm) (push fsm sent))))
                    (unwind-protect
                        (progn
                          (mevedel-plan-handoff--dispatch-submission
                           session buffer record selection accepted
                           "Implement" submission)
                          (ert-info ((format "Staged %S/%S: refusal=%S" stage kind blocked))
                            (should attempted)
                            (should blocked)
                            (should-not newer)
                            (should (string-match-p "accepted plan implementation is pending" blocked))
                            (should (plist-get observations :text-unchanged))
                            (should (plist-get observations :context-unchanged))
                            (should (plist-get observations :pending-unchanged))
                            (should (= 1 (length sent)))
                            (with-current-buffer view (should (equal draft (mevedel-view--input-text))))
                            (let* ((fsm (car sent)) (info (gptel-fsm-info fsm))
                                   (request mevedel--current-request)
                                   (payload (prin1-to-string (plist-get info :data))))
                              (should (eq 'WAIT (gptel-fsm-state fsm)))
                              (should (eq fsm (mevedel-request-fsm request)))
                              (should (equal (mevedel-request-id request) (plist-get info :mevedel-request-id)))
                              (should (equal policy (mevedel-request-implementation-policy request)))
                              (should (eq 'full-auto (mevedel-request-permission-mode-override request)))
                              (should (eq 'balanced-model (plist-get info :model)))
                              (should (plist-member info :reasoning-effort))
                              (should-not (plist-get info :reasoning-effort))
                              (should (string-match-p (regexp-quote accepted-prompt) payload))
                              (should-not (string-match-p "A separate ordinary question" payload))
                              (gptel--fsm-transition fsm 'DONE)
                              (mevedel-test--await 5 "Staged accepted API request did not settle"
                                (not (mevedel-turn-busy-p buffer))))
                            (should-not mevedel--pending-implementation-policy)
                            (should-not mevedel-skills--pending-request-context)
                            (should (equal "Fast:fast-model" (mevedel-session-model-provider session)))
                            (should (eq 'low (mevedel-session-reasoning-effort session)))
                            (should (eq 'ask (mevedel-session-permission-mode session)))
                            ;; Actual public ordinary input works after admission
                            ;; and settlement, with the new baseline effort.
                            (with-current-buffer view
                              (mevedel-view--clear-input)
                              (insert "Ordinary baseline follow-up")
                              (mevedel-view-send))
                            (let ((fsm (car sent)) (request mevedel--current-request))
                              (should (= 2 (length sent)))
                              (should (eq 'fast-model (plist-get (gptel-fsm-info fsm) :model)))
                              (should (eq 'low (plist-get (gptel-fsm-info fsm) :reasoning-effort)))
                              (should-not (mevedel-request-implementation-policy request))
                              (should-not (mevedel-request-permission-mode-override request))
                              (gptel--fsm-transition fsm 'DONE)
                              (mevedel-test--await 5 "Staged API baseline follow-up did not settle"
                                (not (mevedel-turn-busy-p buffer))))))
                      (dolist (fsm sent)
                        (unless (eq 'DONE (gptel-fsm-state fsm))
                          (gptel--fsm-transition fsm 'ABRT)))
                      (mevedel-test--await 5 "Staged API fixture cleanup did not settle"
                        (not (mevedel-turn-busy-p buffer))))))
              (with-current-buffer view (mevedel-view-stream-stop))
              (kill-buffer view)))))))

  :doc "native readiness cannot admit public ordinary input with staged Direct authority"
  (progn
    (mevedel-handoff-owner-test--with-target
      (setf (plist-get selection :execution) 'direct
            (plist-get selection :model-provider) "Claude Code:sonnet")
      (setq policy (mevedel-implementation-policy-from-selection selection))
      (cl-letf (((symbol-function 'mevedel-claude-code-unavailable-reason) #'ignore))
        (mevedel-claude-code-register)
        (setq-local gptel-system-prompt "Staged native fixture" gptel-tools nil)
        (let* ((view (generate-new-buffer " *staged-native-owner-view*"))
               (ordinary-prompt "A separate ordinary native-startup question.")
               (check (symbol-function 'mevedel-request-assert-target-ready))
               (prompt (symbol-function 'mevedel-acp-prompt))
               (mevedel-claude-code-directory (file-name-concat root "claude"))
               attempted blocked newer prompts launches draft sent)
          (unwind-protect
              (save-window-excursion
                (mevedel-view--setup view buffer)
                (set-window-buffer (selected-window) view)
                (with-current-buffer view
                  (goto-char (mevedel-view--input-start))
                  (insert ordinary-prompt)
                  (setq draft (mevedel-view--input-text)))
                (cl-letf (((symbol-function 'mevedel-auth-assert-ready) #'ignore)
                          ((symbol-function 'gptel-curl-get-response) (lambda (fsm) (push fsm sent)))
                          ((symbol-function 'mevedel-claude-code-launch)
                           (mevedel-engine-test--claude-launch
                            (lambda (_system _mcp model effort &optional _id _hook)
                              (push (list mevedel--current-request model effort) launches)
                              nil)))
                          ((symbol-function 'mevedel-acp-prompt)
                           (lambda (connection input &rest args)
                             (push (prin1-to-string input) prompts)
                             (apply prompt connection input args)))
                          ((symbol-function 'mevedel-request-assert-target-ready)
                           (lambda (owner)
                             (prog1 (funcall check owner)
                               (when (and mevedel--pending-implementation-policy (not attempted))
                                 (setq attempted t)
                                 (should-not mevedel--current-request)
                                 (let ((pending mevedel--pending-implementation-policy)
                                       (text (buffer-string)))
                                   (with-current-buffer view
                                     (condition-case err
                                         (mevedel-view-send)
                                       (user-error
                                        (setq blocked (error-message-string err)))))
                                   (should (equal text (buffer-string)))
                                   (should (eq pending mevedel--pending-implementation-policy)))
                                 (setq newer mevedel--current-request)
                                 (mevedel-model-set-session-effort session 'low buffer))))))
                  (mevedel-plan-handoff--dispatch-submission
                   session buffer record selection accepted
                   "Implement" submission)
                  (mevedel-test--await 5 "Native staged accepted producer did not settle"
                    (not (mevedel-turn-busy-p buffer)))
                  (should attempted)
                  (should blocked)
                  (should-not newer)
                  (should (string-match-p "accepted plan implementation is pending" blocked))
                  (should (= 1 (length launches)))
                  (should (equal '("sonnet" nil) (cdar launches)))
                  (let ((request (caar launches)))
                    (should (equal policy (mevedel-request-implementation-policy request)))
                    (should (eq 'full-auto (mevedel-request-permission-mode-override request)))
                    (should (equal "sonnet" (gptel--model-name (plist-get (mevedel-engine-info request) :model))))
                    (should (plist-member (mevedel-engine-info request) :reasoning-effort))
                    (should-not (plist-get (mevedel-engine-info request) :reasoning-effort)))
                  (should (cl-some (lambda (input) (string-match-p "Implement accepted outcome" input)) prompts))
                  (should-not (cl-some (lambda (input) (string-match-p (regexp-quote ordinary-prompt) input)) prompts))
                  (with-current-buffer view (should (equal draft (mevedel-view--input-text))))
                  (should-not mevedel--pending-implementation-policy)
                  (should (eq 'low (mevedel-session-reasoning-effort session)))
                  (with-current-buffer view
                    (mevedel-view--clear-input)
                    (insert "Ordinary native-startup baseline follow-up")
                    (mevedel-view-send))
                  (let ((fsm (car sent)) (request mevedel--current-request))
                    (should (= 1 (length sent)))
                    (should (eq 'fast-model (plist-get (gptel-fsm-info fsm) :model)))
                    (should (eq 'low (plist-get (gptel-fsm-info fsm) :reasoning-effort)))
                    (should-not (mevedel-request-implementation-policy request))
                    (should-not (mevedel-request-permission-mode-override request))
                    (gptel--fsm-transition fsm 'DONE)
                    (mevedel-test--await 5 "Native staged baseline follow-up did not settle"
                      (not (mevedel-turn-busy-p buffer))))))
            (with-current-buffer view (mevedel-view-stream-stop))
            (kill-buffer view)))))))

(provide 'test-mevedel-plan-handoff-staged-owner)
;;; test-mevedel-plan-handoff-staged-owner.el ends here
