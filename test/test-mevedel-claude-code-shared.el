;;; test-mevedel-claude-code-shared.el --- Shared-item Claude turns -*- lexical-binding: t -*-

;;; Commentary:
;; Shared-item questions keep their own context in Claude sessions, as they
;; do through gptel's prompt transform.

;;; Code:

(require 'mevedel-engine-test-support
         (file-name-concat
          (file-name-directory (or load-file-name buffer-file-name))
          "mevedel-engine-test-support"))
(require 'mevedel-claude-code-session)
(require 'mevedel-pending-inputs)

(mevedel-deftest mevedel-claude-code-send/shared-item (:quiet t)
  ,test
  (test)
  :doc "an item question runs isolated from root native history, with item framing"
  (mevedel-engine-test--with-session
    (mevedel-request-end)
    (let ((view (generate-new-buffer " *claude-item-view*"))
          (gptel--known-backends nil)
          (mevedel-claude-code-directory (file-name-concat root "claude"))
          ids prompts launches)
      (unwind-protect
          (progn
            (mevedel-claude-code-register)
            (mevedel-model-set-session-provider
             session (mevedel-model-resolve-provider "Claude Code:sonnet") buffer)
            (setq-local gptel-system-prompt "Item fixture" gptel-tools nil)
            (mevedel-chat-install-request-hooks)
            (mevedel-view--setup view buffer)
            (advice-add 'mevedel-acp-prompt :before
                        (lambda (_c input &rest _) (push input prompts))
                        '((name . mevedel-claude-code-shared-test)))
            (advice-add 'mevedel-acp-open :before
                        (lambda (launch &rest _) (push launch launches))
                        '((name . mevedel-claude-code-shared-test)))
            (cl-letf (((symbol-function 'mevedel-claude-code-launch)
                       (mevedel-engine-test--claude-launch
                        (lambda (_system mcp _model _effort &optional id _hook)
                          (push id ids)
                          (list :meta '((echoAllText . t)))))))
              (with-current-buffer view
                (mevedel-view--forward-input-now "Room secret is PINEAPPLE"))
              (with-timeout (10 (ert-fail "Root turn did not settle"))
                (while (mevedel-turn-busy-p buffer) (accept-process-output nil 0.01)))
              (should (mevedel-view-enqueue-external-follow-up
                       buffer "What is on this board?\n\nShared content snapshot (user-provided data):\n<board/>"
                       :guest-name "Guest" :guest-id "g1" :guest-role 'full
                       :shared-question (list :questionId "q-1" :itemId "board-1" :title "Board"
                                              :revision 1 :scope "whole" :text "What is on this board?"
                                              :fingerprint "fp")))
              (mevedel-view--drain-follow-up buffer)
              (with-timeout (10 (ert-fail "Item turn did not settle"))
                (while (or (mevedel-turn-busy-p buffer)
                           (mevedel-view--pending-follow-ups session))
                  (accept-process-output nil 0.01)))
              (should (mevedel-view-enqueue-external-follow-up
                       buffer "And now?\n\nShared content snapshot (user-provided data):\n<board/>"
                       :guest-name "Guest" :guest-id "g1" :guest-role 'full
                       :shared-question (list :questionId "q-2" :itemId "board-1" :title "Board"
                                              :revision 2 :scope "whole" :text "And now?"
                                              :fingerprint "fp2")))
              (mevedel-view--drain-follow-up buffer)
              (with-timeout (10 (ert-fail "Second item turn did not settle"))
                (while (or (mevedel-turn-busy-p buffer)
                           (mevedel-view--pending-follow-ups session))
                  (accept-process-output nil 0.01))))
            ;; Item turns start their own conversations instead of resuming root.
            (should (equal '(nil nil nil) ids))
            (should (string-search "Conversation about shared item Board (board-1)"
                                   (format "%S" (cadr prompts))))
            (should-not (string-search "PINEAPPLE" (format "%S" (cadr prompts))))
            ;; Earlier item turns arrive as labelled evidence, without hidden records.
            (let ((second (format "%S" (car prompts))))
              (should (string-search "evidence item; provenance: user" second))
              (should (string-search "What is on this board?" second))
              (should-not (string-search "mevedel-hook-audit" second))
              (should-not (string-search "PINEAPPLE" second)))
            ;; They neither advertise native compaction nor replace the root record.
            (should-not (plist-get (car launches) :compaction))
            (should (plist-get (car (last launches)) :compaction))
            (should (equal "fixture-session"
                           (plist-get (alist-get "root" (mevedel-session-external-conversations session)
                                                 nil nil #'equal)
                                      :id))))
        (advice-remove 'mevedel-acp-prompt 'mevedel-claude-code-shared-test)
        (advice-remove 'mevedel-acp-open 'mevedel-claude-code-shared-test)
        (when (buffer-live-p view) (kill-buffer view))))))

(mevedel-deftest mevedel-claude-code-send/shared-context (:quiet t)
  (dolist (shared-first '(nil t))
    (mevedel-engine-test--with-session
      (mevedel-request-end)
      (let* ((view (generate-new-buffer " *claude-item-context-view*"))
             (gptel--known-backends nil)
             (mevedel-claude-code-directory (file-name-concat root "claude"))
             (directory (file-name-concat root "nested"))
             (file (file-name-concat directory "evidence.txt"))
             (instructions (file-name-concat directory "AGENTS.md"))
             (prompt (format "Discuss @file:{%s}" file))
             prompts)
        (make-directory directory t)
        (write-region "SHARED-MENTION-6392" nil file nil 'silent)
        (write-region "SHARED-PATH-GUIDANCE-7315" nil instructions nil 'silent)
        (unwind-protect
            (progn
              (mevedel-claude-code-register)
              (mevedel-model-set-session-provider
               session (mevedel-model-resolve-provider "Claude Code:sonnet") buffer)
              (setq-local gptel-system-prompt "Item context fixture"
                          gptel-tools (list (mevedel-tool-gptel-tool (mevedel-tool-ensure "Read"))))
              (mevedel-chat-install-request-hooks)
              (mevedel-view--setup view buffer)
              (advice-add 'mevedel-acp-prompt :before
                          (lambda (_connection input &rest _) (push input prompts))
                          '((name . mevedel-claude-code-shared-context-test)))
              (cl-letf (((symbol-function 'mevedel-claude-code-launch)
                         (mevedel-engine-test--claude-launch
                          (lambda (_system mcp _model _effort &optional id hook)
                            (list :meta `((hookCommand . ,hook)
                                          (responseText . "Completed")
                                          (toolBatches . [[((name . "Read") (id . "read-evidence")
                                                           (args . ((file_path . ,file))))]])))))))
                (dolist (shared (list shared-first (not shared-first)))
                  (if shared
                      (progn
                        (mevedel-agent-control--enqueue
                         session "/root"
                         (list :type 'RESULT :sender "/root/reader" :recipient "/root"
                               :outcome 'completed :payload "ROOT-MAIL-9183" :timestamp (current-time)))
                        (should (mevedel-view-enqueue-external-follow-up
                                 buffer prompt :guest-name "Guest" :guest-id "g1" :guest-role 'full
                                 :shared-question (list :questionId "q-context" :itemId "board-context"
                                                        :title "Board" :revision 1 :scope "whole"
                                                        :text prompt :fingerprint "context")))
                        (mevedel-view--drain-follow-up buffer))
                    (with-current-buffer view
                      (mevedel-view--forward-input-now prompt)))
                  (with-timeout (10 (ert-fail "Context turn did not settle"))
                    (while (or (mevedel-turn-busy-p buffer)
                               (mevedel-view--pending-follow-ups session))
                      (accept-process-output nil 0.01)))
                  ;; Both independent conversations receive the mention body.
                  (should (string-search "SHARED-MENTION-6392" (format "%S" (car prompts))))
                  (if shared
                      (progn
                        (should-not (string-search "ROOT-MAIL-9183" (format "%S" (car prompts))))
                        (should (= 1 (length (mevedel-agent-control-context-mailbox session)))))
                    (when shared-first
                      (should (string-search "ROOT-MAIL-9183" (format "%S" (car prompts))))
                      (should-not (mevedel-agent-control-context-mailbox session))))
                  (when (and shared shared-first)
                    (should (= 0 (hash-table-count (mevedel-session-mentions-shown session))))
                    (should-not (mevedel-session-workspace-instruction-hashes session)))))
              ;; Both Reads deliver their path guidance, independently of which
              ;; conversation acknowledged it first.
              (should (= 2 (mevedel-engine-test--count-evidence "SHARED-PATH-GUIDANCE-7315"))))
          (advice-remove 'mevedel-acp-prompt 'mevedel-claude-code-shared-context-test)
          (when (buffer-live-p view) (kill-buffer view)))))))

(provide 'test-mevedel-claude-code-shared)
;;; test-mevedel-claude-code-shared.el ends here
