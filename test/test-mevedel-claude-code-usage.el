;;; test-mevedel-claude-code-usage.el --- Native usage completeness -*- lexical-binding: t -*-

;;; Commentary:
;; Missing prompt/sample counters must stay unknown across continuations.

;;; Code:

(require 'mevedel-claude-code)
(require 'mevedel-claude-code-context)
(require 'mevedel-engine-test-support
         (file-name-concat
          (file-name-directory (or load-file-name buffer-file-name)) "mevedel-engine-test-support"))

(mevedel-deftest mevedel--send-request/claude-continuation-unknown-usage (:quiet t)
  (mevedel-engine-test--with-claude-session
    (let* ((mevedel-goal-token-budget 100)
           (goal (mevedel-goal-create "Complete both prompts" session))
           (next t) outcomes)
      (setq-local gptel-system-prompt "Continuation usage fixture" gptel-tools nil)
      (cl-letf (((symbol-function 'mevedel-claude-code-launch)
                 (mevedel-engine-test--claude-launch
                  (lambda (_system mcp _model _effort &optional id _hook)
                    (list :complete-prompt (lambda (owner outcome)
                                             (push (copy-tree outcome) outcomes)
                                             (mevedel-claude-code--complete-prompt owner outcome))
                          :meta '((promptResponse . ((stopReason . "end_turn")
                                                     (usage . ((inputTokens . 10) (cachedWriteTokens . 0) (outputTokens . 5)))))
                                  (continuationPrompts . [((promptResponse . ((stopReason . "end_turn"))))]))))))
                ((symbol-function 'mevedel-claude-code-context-next-prompt)
                 (lambda (_owner) (when next (setq next nil) [((type . "text") (text . "Continue"))])))
                ((symbol-function 'mevedel-goal--schedule-continuation) #'ignore))
        (mevedel--insert-user-turn "Do both prompts")
        (mevedel--send-request "Do both prompts")
        (mevedel-test--await 5 "Continuation usage fixture did not settle"
          (not (mevedel-turn-busy-p buffer))))
      (should (eq 'success (plist-get (mevedel-engine-info request) :mevedel-acp-outcome)))
      (should (= 15 (mevedel-goal-tokens-used goal)))
      (ert-info ((format "outcomes=%S, next=%S" outcomes next))
        (should (plist-get (mevedel-engine-info request) :mevedel-usage-incomplete)))
      (should (plist-get (mevedel-engine-info request) :mevedel-acp-prompted))
      (should (mevedel-goal-tokens-incomplete-p goal))
      (should (eq 'budget-limited (mevedel-goal-status goal)))
      (should (mevedel-goal-tokens-incomplete-p
               (mevedel-session-codec--goal-from-plist
                (plist-get (mevedel-session-codec-read
                            (mevedel-session-artifacts-sidecar-path (mevedel-session-save-path session))) :goal)))))))

(mevedel-deftest mevedel-claude-code--complete-prompt/usage ()
  ,test
  (test)

  :doc "a later unknown prompt cannot inherit completeness from a prior prompt"
  (let ((owner (gptel-make-fsm :info nil)) (next t))
    (cl-letf (((symbol-function 'mevedel-claude-code-context-next-prompt)
               (lambda (_owner) (when next (setq next nil) [((type . "text") (text . "Continue"))]))))
      (mevedel-claude-code--complete-prompt owner '(:status success :tokens (:input 10 :output 5)))
      (should-not (plist-get (mevedel-engine-info owner) :mevedel-usage-incomplete))
      (should (equal '(:input 10 :output 5)
                     (plist-get (mevedel-engine-info owner) :mevedel-claude-usage-base)))
      (should (plist-get (mevedel-engine-info owner) :mevedel-usage-pending))
      (mevedel-claude-code--complete-prompt owner '(:status success))
      (should (plist-get (mevedel-engine-info owner) :mevedel-usage-incomplete))
      (should (equal '(:input 10 :output 5) (plist-get (mevedel-engine-info owner) :tokens-full)))))

  :doc "later complete counters add to known lower bounds without repairing an unknown earlier prompt"
  (let ((owner (gptel-make-fsm :info nil)) (next t))
    (cl-letf (((symbol-function 'mevedel-claude-code-context-next-prompt)
               (lambda (_owner) (when next (setq next nil) [((type . "text") (text . "Continue"))]))))
      (mevedel-claude-code--complete-prompt owner '(:status success :tokens (:input 12)))
      (mevedel-claude-code--complete-prompt owner '(:status success :tokens (:input 20 :output 5)))
      (should (plist-get (mevedel-engine-info owner) :mevedel-usage-incomplete))
      (should-not (plist-get (mevedel-engine-info owner) :mevedel-usage-pending))
      (should (equal '(:input 32 :output 5) (plist-get (mevedel-engine-info owner) :tokens-full)))))

  :doc "SDK and terminal fields jointly establish current prompt completeness"
  (dolist (terminal '((:output 15) (:input 20 :output 15)))
    (let ((owner (gptel-make-fsm :info nil)))
      (mevedel-claude-code-usage-observe
       owner '((method . "_claude/sdkMessage")
               (params . ((message . ((type . "assistant")
                                     (message . ((id . "sample") (model . "sonnet")
                                                 (usage . ((input_tokens . 10) (cache_creation_input_tokens . 2)))))))))))
      (cl-letf (((symbol-function 'mevedel-claude-code-context-next-prompt) (lambda (_owner) nil)))
        (mevedel-claude-code--complete-prompt owner (list :status 'success :tokens terminal)))
      (should-not (plist-get (mevedel-engine-info owner) :mevedel-usage-incomplete))
      (should (= (if (plist-member terminal :input) 20 12)
                 (plist-get (plist-get (mevedel-engine-info owner) :tokens-full) :input)))
      (should (= 15 (plist-get (plist-get (mevedel-engine-info owner) :tokens-full) :output)))))

  :doc "partial SDK counters add without dropping absent known base fields"
  (let ((owner (gptel-make-fsm :info '(:tokens-full (:input 7 :output 8)
                                     :mevedel-claude-usage-base (:input 7 :output 8)))))
    (mevedel-claude-code-usage-observe
     owner '((method . "_claude/sdkMessage")
             (params . ((message . ((type . "assistant")
                                   (message . ((id . "partial") (model . "sonnet")
                                               (usage . ((output_tokens . 10)))))))))))
    (cl-letf (((symbol-function 'mevedel-claude-code-context-next-prompt) (lambda (_owner) nil)))
      (mevedel-claude-code--complete-prompt owner '(:status success)))
    (should (plist-get (mevedel-engine-info owner) :mevedel-usage-incomplete))
    (should (equal '(:input 7 :output 18) (plist-get (mevedel-engine-info owner) :tokens-full))))

  :doc "a measured input field contributes its lower bound without inventing cache creation"
  (let ((owner (gptel-make-fsm :info nil)))
    (mevedel-claude-code-usage-observe
     owner '((method . "_claude/sdkMessage")
             (params . ((message . ((type . "assistant")
                                   (message . ((id . "partial-input") (model . "sonnet")
                                               (usage . ((input_tokens . 7) (output_tokens . 5)))))))))))
    (should (= 7 (plist-get (plist-get (mevedel-engine-info owner) :tokens-full) :input)))
    (should-not (plist-member (plist-get (mevedel-engine-info owner) :mevedel-claude-prompt-usage) :input))
    (should (plist-get (mevedel-engine-info owner) :mevedel-usage-pending)))

  :doc "a sample without usage preserves known lower bounds but prevents complete totals"
  (let ((owner (gptel-make-fsm :info nil)))
    (dolist (sample '(((id . "known") (model . "sonnet")
                      (usage . ((input_tokens . 10) (cache_creation_input_tokens . 0) (output_tokens . 5))))
                     ((id . "unknown") (model . "sonnet"))))
      (mevedel-claude-code-usage-observe
       owner `((method . "_claude/sdkMessage")
               (params . ((message . ((type . "assistant") (message . ,sample))))))))
    (cl-letf (((symbol-function 'mevedel-claude-code-context-next-prompt) (lambda (_owner) nil)))
      (mevedel-claude-code--complete-prompt owner '(:status success)))
    (should (plist-get (mevedel-engine-info owner) :mevedel-usage-incomplete))
    (should (= 10 (plist-get (plist-get (mevedel-engine-info owner) :tokens-full) :input)))
    (should (= 5 (plist-get (plist-get (mevedel-engine-info owner) :tokens-full) :output)))))

(provide 'test-mevedel-claude-code-usage)
;;; test-mevedel-claude-code-usage.el ends here
