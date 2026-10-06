;;; test-mevedel-claude-code-agent.el --- Native retained agent limits -*- lexical-binding: t -*-

;;; Commentary:
;; Exercise limits and follow-ups through retained-agent control.

;;; Code:

(require 'mevedel-engine-test-support
         (file-name-concat (file-name-directory (or load-file-name buffer-file-name))
                           "mevedel-engine-test-support"))
(require 'mevedel-claude-code-agent)

(defconst mevedel-claude-code-agent-test--peer
  (file-name-concat (file-name-directory (or load-file-name buffer-file-name))
                    "fixtures" "acp-agent.py"))

(mevedel-deftest mevedel-agent-control-spawn/claude-sample-limit (:quiet t)
  (dolist (limit '(1 3 8 nil))
    (mevedel-engine-test--with-session
      (let ((gptel--known-backends nil)
            (mevedel-claude-code-directory (file-name-concat root "claude"))
            (file (file-name-concat root "evidence.txt"))
            (launches 0) record invocation)
        (write-region "Evidence" nil file nil 'silent)
        (dolist (name '("Read" "ListAgents" "SendMessage")) (mevedel-tool-ensure name))
        (mevedel-claude-code-register)
        (mevedel-model-set-session-provider
         session (mevedel-model-resolve-provider "Claude Code:sonnet") buffer)
        (unwind-protect
            (cl-letf (((symbol-function 'mevedel-claude-code-launch)
                       (lambda (_system mcp _model _effort &optional id hook)
                         (cl-incf launches)
                         (list :command (executable-find "python3")
                               :args (list mevedel-claude-code-agent-test--peer)
                               :cwd root :mcp mcp :session-id id
                               :tool-id-field :claudecode/toolUseId
                               :control #'mevedel-claude-code--control
                               :check-context #'mevedel-claude-code-context-check
                               :observe #'mevedel-claude-code-context-observe
                               :meta `((hookCommand . ,hook) (suppressResponseOnStop . t)
                                       (compactBeforeBatch . ,(when (equal limit 3) 2))
                                       (responseText . "All work completed")
                                       (toolBatches . ,(vconcat
                                                       (cl-loop for batch below 5 collect
                                                                (vconcat
                                                                 (cl-loop for tool below 2 collect
                                                                          `((name . "Read")
                                                                            (id . ,(format "%d-%d-%d" launches batch tool))
                                                                            (args . ((file_path . ,file))))))))))))))
              (dotimes (phase 2)
                (if (= phase 0)
                    (mevedel-agent-control-spawn
                     session "limited" "Read the evidence in several steps."
                     (lambda (value) (setq record (plist-get value :record)))
                     :agent (mevedel-agent--create :name "limited" :description "Read evidence"
                                                  :tools '(Read) :max-turns limit
                                                  :system-prompt "Read evidence")
                     :on-invocation (lambda (value) (setq invocation value)))
                  (mevedel-agent-control-followup session "/root/limited" "Continue the investigation.")
                  (setq invocation (mevedel-agent-record-invocation record)))
                (with-timeout (10 (ert-fail "Limited child did not settle"))
                  (while (not (mevedel-agent-invocation-runtime-settled-p invocation))
                    (accept-process-output nil 0.01)))
                (ert-info ((format "limit=%S phase=%d" limit phase))
                  (should (= (min (or limit 6) 6) (mevedel-agent-invocation-turn-count invocation)))
                  (should (eq 'completed (mevedel-agent-record-settled-outcome record)))
                  (if (and limit (<= limit 5))
                      (should (string-search (format "%d-turn limit was reached" limit)
                                             (mevedel-agent-record-settled-result record)))
                    (should (string-suffix-p "All work completed" (mevedel-agent-record-settled-result record))))
                  (with-current-buffer (mevedel-agent-record-conversation-buffer record)
                    (should (= (* (1+ phase) 2 (min (or limit 5) 5))
                               (how-many "^#\\+begin_tool" (point-min) (point-max))))
                    (when (and limit (<= limit 5))
                      (should (= (* (1+ phase) (if (equal limit 3) 2 1))
                                 (mevedel-engine-test--count-evidence "This is your final turn"))))
                    (when (equal limit 3)
                      (should (= (1+ phase) (mevedel-engine-test--count-evidence "You have used 2 of 3 turns"))))))))
          (when (and record (buffer-live-p (mevedel-agent-record-conversation-buffer record)))
            (kill-buffer (mevedel-agent-record-conversation-buffer record))))))))

(provide 'test-mevedel-claude-code-agent)
;;; test-mevedel-claude-code-agent.el ends here
