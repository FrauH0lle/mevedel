;;; test-niwa-claude-code-retry-context.el --- Fresh native Retry context -*- lexical-binding: t -*-

;;; Commentary:
;; Replay the initial gptel transcript excerpt through the real ACP peer.

;;; Code:

(require 'niwa-engine-test-support
         (file-name-concat (file-name-directory (or load-file-name buffer-file-name))
                           "niwa-engine-test-support"))
(require 'niwa-claude-code)
(require 'niwa-claude-code-session)
(require 'niwa-gptel-retry)

(niwa-deftest niwa-claude-code-send/retry-initial-excerpt (:quiet t)
  (niwa-engine-test--with-claude-session
    (let ((trace (file-name-concat root "retry-context.jsonl"))
          (process-environment (copy-sequence process-environment))
          (launches 0) ids)
      ;; Canonical settled gptel turns, as in the engine-transfer fixture.
      (dotimes (index 2)
        (niwa--insert-user-turn (format "Earlier API task %d" index))
        (insert (propertize (format "\nAPI ANSWER %d RETAINED\n" index) 'gptel 'response)))
      (setf (niwa-session-turn-count session) 2
            (niwa-session-prompt-index session) '((1 (:cum-turn 1) (:cum-turn 2))))
      (niwa-request-end)
      (setq-local gptel-system-prompt "Retry excerpt fixture" gptel-tools nil)
      (setenv "NIWA_ACP_TEST_TRACE" trace)
      (cl-letf (((symbol-function 'niwa-claude-code-launch)
                 (niwa-engine-test--claude-launch
                  (lambda (_system _mcp _model _effort &optional id _hook)
                    (push id ids)
                    (cl-incf launches)
                    (list :meta `((responseText . ,(if (= launches 1) "FAILED PARTIAL" "Retried answer"))
                                  (responseMessageId . ,(format "answer-%d" launches))
                                  (promptResponse . ((stopReason . ,(if (= launches 1) "max_tokens" "end_turn"))))))))))
        (niwa--insert-local-user-turn "First native prompt")
        (niwa--send-request)
        (niwa-test--await 5 "First native failure did not settle" (not (niwa-turn-busy-p buffer)))
        (let ((id (niwa-gptel-retry-available-p buffer)))
          (should id)
          (niwa-gptel-retry-failed-turn buffer id))
        (niwa-test--await 5 "Fresh native Retry did not settle" (not (niwa-turn-busy-p buffer))))
      (should (equal '(nil nil) ids))
      (should (= 3 (niwa-session-turn-count session)))
      (should (eq 'success (niwa-session-last-outcome session)))
      (let* ((frames (with-temp-buffer
                       (insert-file-contents trace)
                       (mapcar (lambda (line) (json-parse-string line :object-type 'alist))
                               (split-string (buffer-string) "\n" t))))
             (prompts (seq-filter (lambda (frame) (equal "session/prompt" (alist-get 'method frame))) frames)))
        (should (= 2 (length prompts)))
        (dolist (frame prompts)
          (let ((text (mapconcat (lambda (part) (or (alist-get 'text part) ""))
                                (alist-get 'prompt (alist-get 'params frame)) "\n")))
            (should (string-search "Excerpt continuation from niwa" text))
            (dotimes (index 2)
              (should (string-search (format "Earlier API task %d" index) text))
              (should (string-search (format "API ANSWER %d RETAINED" index) text)))
            (should (string-search "First native prompt" text))
            (should-not (string-search "FAILED PARTIAL" text))))))))

(provide 'test-niwa-claude-code-retry-context)
;;; test-niwa-claude-code-retry-context.el ends here
