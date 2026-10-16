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
  (mevedel-engine-test--with-claude-session
    (mevedel-request-end)
    (let ((view (generate-new-buffer " *claude-item-view*"))
          ids prompts launches)
      (unwind-protect
          (progn
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
              (mevedel-test--await 10 "Root turn did not settle" (not (mevedel-turn-busy-p buffer)))
              (should (mevedel-view-enqueue-external-follow-up
                       buffer "What is on this board?\n\nShared content snapshot (user-provided data):\n<board/>"
                       :guest-name "Guest" :guest-id "g1" :guest-role 'full
                       :shared-question (list :questionId "q-1" :itemId "board-1" :title "Board"
                                              :revision 1 :scope "whole" :text "What is on this board?"
                                              :fingerprint "fp")))
              (mevedel-view--drain-follow-up buffer)
              (mevedel-test--await 10 "Item turn did not settle"
                (not (or (mevedel-turn-busy-p buffer)
                         (mevedel-view--pending-follow-ups session))))
              (should (mevedel-view-enqueue-external-follow-up
                       buffer "And now?\n\nShared content snapshot (user-provided data):\n<board/>"
                       :guest-name "Guest" :guest-id "g1" :guest-role 'full
                       :shared-question (list :questionId "q-2" :itemId "board-1" :title "Board"
                                              :revision 2 :scope "whole" :text "And now?"
                                              :fingerprint "fp2")))
              (mevedel-view--drain-follow-up buffer)
              (mevedel-test--await 10 "Second item turn did not settle"
                (not (or (mevedel-turn-busy-p buffer)
                         (mevedel-view--pending-follow-ups session)))))
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
                          (lambda (_connection input &rest _)
                            (with-current-buffer buffer
                              (push (list :input input
                                          :read-count
                                          (how-many "^#\\+begin_tool (Read\\_>" (point-min) (point-max))
                                          :guidance-count
                                          (mevedel-engine-test--count-evidence
                                           "SHARED-PATH-GUIDANCE-7315")
                                          :mentions-count
                                          (hash-table-count (mevedel-session-mentions-shown session))
                                          :instruction-hashes
                                          (copy-tree (mevedel-session-workspace-instruction-hashes session))
                                          :attribution
                                          (mevedel-view--user-turn-attribution
                                           (last (cl-remove-if-not
                                                  (lambda (segment) (eq (car segment) 'user))
                                                  (mevedel-transcript-segments
                                                   (point-min) (point-max)))) buffer))
                                    prompts)))
                          '((name . mevedel-claude-code-shared-context-test)))
              (cl-letf (((symbol-function 'mevedel-claude-code-launch)
                         (mevedel-engine-test--claude-launch
                          (lambda (_system mcp _model _effort &optional id hook)
                            (list :meta `((hookCommand . ,hook)
                                          (responseText . "Completed")
                                          (toolBatches . [[((name . "Read") (id . "read-evidence")
                                                           (args . ((file_path . ,file))))]])))))))
                (dolist (shared (list shared-first (not shared-first)))
                  (setq prompts nil)
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
                               (mevedel-view--pending-follow-ups session)
                               (and shared (mevedel-agent-control-context-mailbox session)))
                      (accept-process-output nil 0.01)))
                  ;; Both independent conversations receive the mention body.
                  (should (= (if shared 2 1) (length prompts)))
                  (let* ((user (car (last prompts)))
                         (user-input (format "%S" (plist-get user :input)))
                         (read-count (how-many "^#\\+begin_tool (Read\\_>" (point-min) (point-max)))
                         (guidance-count (mevedel-engine-test--count-evidence
                                          "SHARED-PATH-GUIDANCE-7315")))
                    (should (string-search "SHARED-MENTION-6392" user-input))
                    (should-not (string-search "ROOT-MAIL-9183" user-input))
                    ;; Both original user turns still perform their own Read.
                    (should (= 1 (- (if shared (plist-get (car prompts) :read-count) read-count)
                                    (plist-get user :read-count))))
                    ;; Guidance is independent per conversation.  When shared
                    ;; runs first, the root notification acknowledges it before
                    ;; the later root user Read, which correctly deduplicates it.
                    (should (= (if (and shared-first (not shared)) 0 1)
                               (- (if shared
                                      (plist-get (car prompts) :guidance-count)
                                    guidance-count)
                                  (plist-get user :guidance-count))))
                    (when shared
                      (should (string-search
                               "Conversation about shared item Board (board-context)" user-input))
                      (let* ((background (car prompts))
                             (input (format "%S" (plist-get background :input))))
                        (should (string-search mevedel-pending-inputs--event-trigger input))
                        (should (string-search "ROOT-MAIL-9183" input))
                        (should (string-search "agent-result sender=\\\"/root/reader\\\" recipient=\\\"/root\\\"" input))
                        (should-not (string-search "SHARED-MENTION-6392" input))
                        (should-not (string-search "Conversation about shared item" input))
                        (should (equal '(:type background-event)
                                       (plist-get background :attribution)))
                        (should-not (mevedel-agent-control-context-mailbox session))
                        (should (= 1 (- read-count (plist-get background :read-count))))
                        (should (= (if shared-first 1 0)
                                   (- guidance-count (plist-get background :guidance-count))))
                        ;; Inspect root state before its notification runs: the
                        ;; shared turn itself acknowledges no root context.
                        (when shared-first
                          (should (= 0 (plist-get background :mentions-count)))
                          (should-not (plist-get background :instruction-hashes))))))))
              ;; Shared and root conversations acknowledge guidance independently.
              (should (= 3 (how-many "^#\\+begin_tool (Read\\_>" (point-min) (point-max))))
              (should (= 2 (mevedel-engine-test--count-evidence "SHARED-PATH-GUIDANCE-7315"))))
          (advice-remove 'mevedel-acp-prompt 'mevedel-claude-code-shared-context-test)
          (when (buffer-live-p view) (kill-buffer view)))))))

(provide 'test-mevedel-claude-code-shared)
;;; test-mevedel-claude-code-shared.el ends here
