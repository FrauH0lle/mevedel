;;; test-mevedel-claude-code-retry-context.el --- Fresh native Retry context -*- lexical-binding: t -*-

;;; Commentary:
;; Replay the initial gptel transcript excerpt through the real ACP peer.

;;; Code:

(require 'mevedel-engine-test-support
         (file-name-concat (file-name-directory (or load-file-name buffer-file-name))
                           "mevedel-engine-test-support"))
(require 'mevedel-claude-code)
(require 'mevedel-claude-code-session)
(require 'mevedel-gptel-retry)

(mevedel-deftest mevedel-claude-code-send/retry-initial-excerpt (:quiet t)
  (mevedel-engine-test--with-claude-session
    (let ((trace (file-name-concat root "retry-context.jsonl"))
          (process-environment (copy-sequence process-environment))
          (launches 0) ids)
      ;; Canonical settled gptel turns, as in the engine-transfer fixture.
      (dotimes (index 2)
        (mevedel--insert-user-turn (format "Earlier API task %d" index))
        (insert (propertize (format "\nAPI ANSWER %d RETAINED\n" index) 'gptel 'response)))
      (setf (mevedel-session-turn-count session) 2
            (mevedel-session-prompt-index session) '((1 (:cum-turn 1) (:cum-turn 2))))
      (mevedel-request-end)
      (setq-local gptel-system-prompt "Retry excerpt fixture" gptel-tools nil)
      (setenv "MEVEDEL_ACP_TEST_TRACE" trace)
      (cl-letf (((symbol-function 'mevedel-claude-code-launch)
                 (mevedel-engine-test--claude-launch
                  (lambda (_system _mcp _model _effort &optional id _hook)
                    (push id ids)
                    (cl-incf launches)
                    (list :meta `((responseText . ,(if (= launches 1) "FAILED PARTIAL" "Retried answer"))
                                  (responseMessageId . ,(format "answer-%d" launches))
                                  (promptResponse . ((stopReason . ,(if (= launches 1) "max_tokens" "end_turn"))))))))))
        (mevedel--insert-local-user-turn "First native prompt")
        (mevedel--send-request)
        (mevedel-test--await 5 "First native failure did not settle" (not (mevedel-turn-busy-p buffer)))
        (let ((id (mevedel-gptel-retry-available-p buffer)))
          (should id)
          (mevedel-gptel-retry-failed-turn buffer id))
        (mevedel-test--await 5 "Fresh native Retry did not settle" (not (mevedel-turn-busy-p buffer))))
      (should (equal '(nil nil) ids))
      (should (= 3 (mevedel-session-turn-count session)))
      (should (eq 'success (mevedel-session-last-outcome session)))
      (let* ((frames (with-temp-buffer
                       (insert-file-contents trace)
                       (mapcar (lambda (line) (json-parse-string line :object-type 'alist))
                               (split-string (buffer-string) "\n" t))))
             (prompts (seq-filter (lambda (frame) (equal "session/prompt" (alist-get 'method frame))) frames)))
        (should (= 2 (length prompts)))
        (dolist (frame prompts)
          (let ((text (mapconcat (lambda (part) (or (alist-get 'text part) ""))
                                (alist-get 'prompt (alist-get 'params frame)) "\n")))
            (should (string-search "Excerpt continuation from mevedel" text))
            (dotimes (index 2)
              (should (string-search (format "Earlier API task %d" index) text))
              (should (string-search (format "API ANSWER %d RETAINED" index) text)))
            (should (string-search "First native prompt" text))
            (should-not (string-search "FAILED PARTIAL" text))))))))

(provide 'test-mevedel-claude-code-retry-context)
;;; test-mevedel-claude-code-retry-context.el ends here
