;;; test-niwa-claude-code-transfer.el --- Engine continuation -*- lexical-binding: t -*-

;;; Commentary:
;; Switch ordinary root sessions across engines and inspect actual request input.

;;; Code:

(require 'niwa-engine-test-support
         (file-name-concat (file-name-directory (or load-file-name buffer-file-name))
                           "niwa-engine-test-support"))
(require 'niwa-claude-code-session)

(niwa-deftest niwa--send-request/transfer-to-claude (:quiet t)
  (niwa-engine-test--with-claude-session
    (let (ids)
      (niwa--insert-user-turn "Earlier API task")
      (insert (propertize "\nAPI EVIDENCE RETAINED\n" 'gptel 'response))
      (setf (niwa-session-turn-count session) 1
            (niwa-session-prompt-index session) '((1 (:cum-turn 1))))
      (niwa-request-end)
      (setq request (niwa-request-begin session))
      (setq-local gptel-system-prompt "Transfer fixture" gptel-tools nil)
      (cl-letf (((symbol-function 'niwa-claude-code-launch)
                 (niwa-engine-test--claude-launch
                  (lambda (_system mcp _model _effort &optional id _hook)
                    (push id ids)
                    nil))))
        (niwa--insert-user-turn "Continue using Claude")
        (niwa--send-request "Continue using Claude")
        (niwa-test--await 5 "Excerpt continuation did not settle"
          (not (niwa-turn-busy-p buffer)))
        (should (eq 'success (plist-get (niwa-engine-info request) :niwa-acp-outcome)))
        (should (string-search "answer:Excerpt continuation" (buffer-string)))
        (should (string-search "provenance: assistant ---\nAPI EVIDENCE RETAINED" (buffer-string)))
        (should (equal '(nil) ids))
        (goto-char (point-max))
        (niwa--insert-user-turn "Second native turn")
        (niwa--send-request "Second native turn")
        (niwa-test--await 5 "Native followup did not settle" (not (niwa-turn-busy-p buffer)))
        (should (stringp (car ids)))
        (should (string-search "answer:Second native turn" (buffer-string)))))))

(niwa-deftest niwa--send-request/transfer-from-claude (:quiet t)
  (niwa-engine-test--with-session
    (let* ((gptel--known-backends nil)
           (niwa-claude-code-directory (file-name-concat root "claude"))
           (api (gptel-make-anthropic "Transfer API" :key "fixture" :models '(claude-sonnet-4-5)))
           (real-request (symbol-function 'gptel-request))
           (instructions (file-name-concat root "AGENTS.md"))
           (source (file-name-concat root "source.txt"))
           sent)
      (write-region "TRANSFER TOOL RESULT" nil source nil 'silent)
      (niwa-claude-code-register)
      (niwa-model-set-session-provider
       session (niwa-model-resolve-provider "Claude Code:sonnet") buffer)
      (setq-local gptel-system-prompt "Transfer fixture"
                  gptel-tools (list (niwa-tool-gptel-tool (niwa-tool-ensure "Read")))
                  gptel-prompt-transform-functions nil)
      (cl-letf (((symbol-function 'niwa-claude-code-launch)
                 (niwa-engine-test--claude-launch
                  (lambda (_system mcp _model _effort &optional id _hook)
                    (list :meta `((toolBatches . [[((name . "Read") (id . "transfer-read")
                                                    (args . ((file_path . ,source))))]])
                                  (compactionEvents .
                                                    [((sessionUpdate . "agent_message_chunk")
                                                      (content . ((type . "text") (text . "ARCHIVE ONLY\n"))))
                                                     ((sessionUpdate . "compaction_update") (compactionId . "switch")
                                                      (status . "completed")
                                                      (summary . [((type . "text") (text . "EFFECTIVE SUMMARY"))]))
                                                     ((sessionUpdate . "agent_thought_chunk")
                                                      (content . ((type . "text") (text . "UNSIGNED THOUGHT"))))])))))))
        (niwa--insert-user-turn "Native task")
        (niwa--send-request "Native task")
        (niwa-test--await 5 "Native source turn did not settle"
          (not (niwa-turn-busy-p buffer))))
      (ert-info ((format "Native outcome: %S / %S"
                         (plist-get (niwa-engine-info request) :niwa-acp-outcome)
                         (plist-get (niwa-engine-info request) :error)))
        (should (eq 'ready (plist-get (cdr (assoc "root" (niwa-session-external-conversations session))) :state))))
      (should (= 2 (niwa-session-current-segment session)))
      (write-region "REFRESHED PATH GUIDANCE" nil instructions nil 'silent)
      (setf (niwa-session-workspace-instruction-hashes session)
            (list (cons (list "/root" instructions) (make-string 64 ?a))
                  (cons (list "/root/child" instructions) (make-string 64 ?b))))
      (niwa-model-set-session-provider
       session (list :backend api :model 'claude-sonnet-4-5) buffer)
      ;; Selecting a provider alone does not discard its native conversation.
      (should (assoc "root" (niwa-session-external-conversations session)))
      (cl-letf (((symbol-function 'gptel-request)
                 (lambda (prompt &rest options)
                   (should (assoc (list "/root" instructions)
                                  (niwa-session-workspace-instruction-hashes session)))
                   (should-not (cdr (assoc (list "/root" instructions)
                                           (niwa-session-workspace-instruction-hashes session))))
                   (setq sent (apply real-request prompt (append options '(:dry-run t))))
                   (niwa-context-delivery-stage sent)
                   (niwa-reminders--handle-inject sent)
                   sent)))
        (goto-char (point-max))
        (niwa--insert-user-turn "Continue through the API")
        (niwa--send-request))
      (let ((data (prin1-to-string (plist-get (gptel-fsm-info sent) :data))))
        (should (string-search "EFFECTIVE SUMMARY" data))
        (ert-info ((buffer-string))
          (should (string-search "workload complete" data)))
        (should (string-search "TRANSFER TOOL RESULT" data))
        (should (string-search "\"tool_use\"" data))
        (should (string-search "Continue through the API" data))
        (should (string-search "REFRESHED PATH GUIDANCE" data))
        (should-not (string-search "ARCHIVE ONLY" data))
        (should-not (string-search ":signature" data))
        (should-not (string-search ":type \"thinking\"" data)))
      (should-not (plist-get (cdr (assoc "root" (niwa-session-external-conversations session))) :id))
      (should (equal (secure-hash 'sha256 "REFRESHED PATH GUIDANCE")
                     (cdr (assoc (list "/root" instructions)
                                 (niwa-session-workspace-instruction-hashes session)))))
      (should (equal (make-string 64 ?b)
                     (cdr (assoc (list "/root/child" instructions)
                                 (niwa-session-workspace-instruction-hashes session)))))
      ;; Returning after API dispatch starts a replacement native history,
      ;; including when that intervening API attempt did not produce an answer.
      (niwa-request-end)
      (niwa-model-set-session-provider
       session (niwa-model-resolve-provider "Claude Code:sonnet") buffer)
      (let (ids)
        (cl-letf (((symbol-function 'niwa-claude-code-launch)
                   (niwa-engine-test--claude-launch
                    (lambda (_system mcp _model _effort &optional id _hook)
                      (push id ids)
                      (list :meta '((fixtureSessionId . "replacement-session")))))))
          (setq-local gptel-tools nil)
          (goto-char (point-max))
          (niwa--insert-user-turn "Return to Claude")
          (niwa--send-request "Return to Claude")
          (niwa-test--await 5 "Replacement native turn did not settle"
            (not (niwa-turn-busy-p buffer)))
          (should (equal '(nil) ids))
          (let ((path (niwa-session-save-path session)))
            (test-niwa-session-persistence--release-and-kill buffer session)
            (setq buffer (niwa-session-persistence-restore path nil nil workspace))
            (set-buffer buffer)
            (setq session niwa--session))
          (setq-local gptel-tools nil)
          (goto-char (point-max))
          (niwa--insert-user-turn "After reopening")
          (niwa--send-request "After reopening")
          (niwa-test--await 5 "Replacement native history did not resume"
            (not (niwa-turn-busy-p buffer)))
          (should (equal '("replacement-session" nil) ids)))))))

(provide 'test-niwa-claude-code-transfer)
;;; test-niwa-claude-code-transfer.el ends here
