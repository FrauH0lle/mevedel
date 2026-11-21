;;; test-niwa-claude-code-usage.el --- Native usage completeness -*- lexical-binding: t -*-

;;; Commentary:
;; Missing prompt/sample counters must stay unknown across continuations.

;;; Code:

(require 'niwa-claude-code)
(require 'niwa-claude-code-context)
(require 'niwa-engine-test-support
         (file-name-concat
          (file-name-directory (or load-file-name buffer-file-name)) "niwa-engine-test-support"))

(niwa-deftest niwa--send-request/claude-continuation-unknown-usage (:quiet t)
  (niwa-engine-test--with-claude-session
    (let* ((niwa-goal-token-budget 100)
           (goal (niwa-goal-create "Complete both prompts" :session session))
           (next t) outcomes)
      (setq-local gptel-system-prompt "Continuation usage fixture" gptel-tools nil)
      (cl-letf (((symbol-function 'niwa-claude-code-launch)
                 (niwa-engine-test--claude-launch
                  (lambda (_system mcp _model _effort &optional id _hook)
                    (list :complete-prompt (lambda (owner outcome)
                                             (push (copy-tree outcome) outcomes)
                                             (niwa-claude-code--complete-prompt owner outcome))
                          :meta '((promptResponse . ((stopReason . "end_turn")
                                                     (usage . ((inputTokens . 10) (cachedWriteTokens . 0) (outputTokens . 5)))))
                                  (continuationPrompts . [((promptResponse . ((stopReason . "end_turn"))))]))))))
                ((symbol-function 'niwa-claude-code-context-next-prompt)
                 (lambda (_owner) (when next (setq next nil) [((type . "text") (text . "Continue"))])))
                ((symbol-function 'niwa-goal--schedule-continuation) #'ignore))
        (niwa--insert-user-turn "Do both prompts")
        (niwa--send-request "Do both prompts")
        (niwa-test--await 5 "Continuation usage fixture did not settle"
          (not (niwa-turn-busy-p buffer))))
      (should (eq 'success (plist-get (niwa-engine-info request) :niwa-acp-outcome)))
      (should (= 15 (niwa-goal-tokens-used goal)))
      (ert-info ((format "outcomes=%S, next=%S" outcomes next))
        (should (plist-get (niwa-engine-info request) :niwa-usage-incomplete)))
      (should (plist-get (niwa-engine-info request) :niwa-acp-prompted))
      (should (niwa-goal-tokens-incomplete-p goal))
      (should (eq 'budget-limited (niwa-goal-status goal)))
      (should (niwa-goal-tokens-incomplete-p
               (niwa-session-codec--goal-from-plist
                (plist-get (niwa-session-codec-read
                            (niwa-session-artifacts-sidecar-path (niwa-session-save-path session))) :goal)))))))

(niwa-deftest niwa-claude-code--complete-prompt/usage ()
  ,test
  (test)

  :doc "a later unknown prompt cannot inherit completeness from a prior prompt"
  (let ((owner (gptel-make-fsm :info nil)) (next t))
    (cl-letf (((symbol-function 'niwa-claude-code-context-next-prompt)
               (lambda (_owner) (when next (setq next nil) [((type . "text") (text . "Continue"))]))))
      (niwa-claude-code--complete-prompt owner '(:status success :tokens (:input 10 :output 5)))
      (should-not (plist-get (niwa-engine-info owner) :niwa-usage-incomplete))
      (should (equal '(:input 10 :output 5)
                     (plist-get (niwa-engine-info owner) :niwa-claude-usage-base)))
      (should (plist-get (niwa-engine-info owner) :niwa-usage-pending))
      (niwa-claude-code--complete-prompt owner '(:status success))
      (should (plist-get (niwa-engine-info owner) :niwa-usage-incomplete))
      (should (equal '(:input 10 :output 5) (plist-get (niwa-engine-info owner) :tokens-full)))))

  :doc "later complete counters add to known lower bounds without repairing an unknown earlier prompt"
  (let ((owner (gptel-make-fsm :info nil)) (next t))
    (cl-letf (((symbol-function 'niwa-claude-code-context-next-prompt)
               (lambda (_owner) (when next (setq next nil) [((type . "text") (text . "Continue"))]))))
      (niwa-claude-code--complete-prompt owner '(:status success :tokens (:input 12)))
      (niwa-claude-code--complete-prompt owner '(:status success :tokens (:input 20 :output 5)))
      (should (plist-get (niwa-engine-info owner) :niwa-usage-incomplete))
      (should-not (plist-get (niwa-engine-info owner) :niwa-usage-pending))
      (should (equal '(:input 32 :output 5) (plist-get (niwa-engine-info owner) :tokens-full)))))

  :doc "SDK and terminal fields jointly establish current prompt completeness"
  (dolist (terminal '((:output 15) (:input 20 :output 15)))
    (let ((owner (gptel-make-fsm :info nil)))
      (niwa-claude-code-usage-observe
       owner '((method . "_claude/sdkMessage")
               (params . ((message . ((type . "assistant")
                                     (message . ((id . "sample") (model . "sonnet")
                                                 (usage . ((input_tokens . 10) (cache_creation_input_tokens . 2)))))))))))
      (cl-letf (((symbol-function 'niwa-claude-code-context-next-prompt) (lambda (_owner) nil)))
        (niwa-claude-code--complete-prompt owner (list :status 'success :tokens terminal)))
      (should-not (plist-get (niwa-engine-info owner) :niwa-usage-incomplete))
      (should (= (if (plist-member terminal :input) 20 12)
                 (plist-get (plist-get (niwa-engine-info owner) :tokens-full) :input)))
      (should (= 15 (plist-get (plist-get (niwa-engine-info owner) :tokens-full) :output)))))

  :doc "partial SDK counters add without dropping absent known base fields"
  (let ((owner (gptel-make-fsm :info '(:tokens-full (:input 7 :output 8)
                                     :niwa-claude-usage-base (:input 7 :output 8)))))
    (niwa-claude-code-usage-observe
     owner '((method . "_claude/sdkMessage")
             (params . ((message . ((type . "assistant")
                                   (message . ((id . "partial") (model . "sonnet")
                                               (usage . ((output_tokens . 10)))))))))))
    (cl-letf (((symbol-function 'niwa-claude-code-context-next-prompt) (lambda (_owner) nil)))
      (niwa-claude-code--complete-prompt owner '(:status success)))
    (should (plist-get (niwa-engine-info owner) :niwa-usage-incomplete))
    (should (equal '(:input 7 :output 18) (plist-get (niwa-engine-info owner) :tokens-full))))

  :doc "a measured input field contributes its lower bound without inventing cache creation"
  (let ((owner (gptel-make-fsm :info nil)))
    (niwa-claude-code-usage-observe
     owner '((method . "_claude/sdkMessage")
             (params . ((message . ((type . "assistant")
                                   (message . ((id . "partial-input") (model . "sonnet")
                                               (usage . ((input_tokens . 7) (output_tokens . 5)))))))))))
    (should (= 7 (plist-get (plist-get (niwa-engine-info owner) :tokens-full) :input)))
    (should-not (plist-member (plist-get (niwa-engine-info owner) :niwa-claude-prompt-usage) :input))
    (should (plist-get (niwa-engine-info owner) :niwa-usage-pending)))

  :doc "a sample without usage preserves known lower bounds but prevents complete totals"
  (let ((owner (gptel-make-fsm :info nil)))
    (dolist (sample '(((id . "known") (model . "sonnet")
                      (usage . ((input_tokens . 10) (cache_creation_input_tokens . 0) (output_tokens . 5))))
                     ((id . "unknown") (model . "sonnet"))))
      (niwa-claude-code-usage-observe
       owner `((method . "_claude/sdkMessage")
               (params . ((message . ((type . "assistant") (message . ,sample))))))))
    (cl-letf (((symbol-function 'niwa-claude-code-context-next-prompt) (lambda (_owner) nil)))
      (niwa-claude-code--complete-prompt owner '(:status success)))
    (should (plist-get (niwa-engine-info owner) :niwa-usage-incomplete))
    (should (= 10 (plist-get (plist-get (niwa-engine-info owner) :tokens-full) :input)))
    (should (= 5 (plist-get (plist-get (niwa-engine-info owner) :tokens-full) :output)))))

(provide 'test-niwa-claude-code-usage)
;;; test-niwa-claude-code-usage.el ends here
