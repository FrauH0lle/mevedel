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

(provide 'test-mevedel-claude-code-shared)
;;; test-mevedel-claude-code-shared.el ends here
