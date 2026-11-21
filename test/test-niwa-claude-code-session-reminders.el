;;; test-niwa-claude-code-session-reminders.el --- Claude prompt reminders -*- lexical-binding: t -*-

;;; Commentary:
;; Configured reminders reach a Claude prompt with the content gptel's
;; dispatch gives them.

;;; Code:

(require 'niwa-engine-test-support
         (file-name-concat
          (file-name-directory (or load-file-name buffer-file-name))
          "niwa-engine-test-support"))
(require 'niwa-claude-code)
(require 'niwa-claude-code-session)
(require 'niwa-file-state)
(require 'niwa-reminders)
(require 'niwa-view)

(niwa-deftest niwa-claude-code-send/edited-file (:quiet t)
  ,test
  (test)
  :doc "an external edit reaches the Claude prompt with its diff"
  (niwa-engine-test--with-claude-session
    (niwa-request-end)
    (let ((view (generate-new-buffer " *claude-edited-file-view*"))
          (file (file-name-concat root "notes.txt"))
          prompts)
      (unwind-protect
          (progn
            (setq-local gptel-system-prompt "Edited-file fixture" gptel-tools nil)
            (unless (cl-find 'edited-file (niwa-session-reminders session)
                             :key #'niwa-reminder-type)
              (niwa-session-add-reminder
               session (niwa-reminders-make-edited-file)))
            (write-region "hello\n" nil file nil 'silent)
            (niwa-file-cache-put (niwa-workspace-file-cache workspace)
                                    (niwa-file-state-from-file file))
            (write-region "goodbye\n" nil file nil 'silent)
            (set-file-times file (time-add (current-time) 2))
            (niwa-chat-install-request-hooks)
            (niwa-view--setup view buffer)
            (advice-add 'niwa-acp-prompt :before
                        (lambda (_c input &rest _) (push input prompts))
                        '((name . niwa-claude-code-edited-file-test)))
            (cl-letf (((symbol-function 'niwa-claude-code-launch)
                       (niwa-engine-test--claude-launch
                        (lambda (&rest _) (list :meta '((echoAllText . t)))))))
              (with-current-buffer view
                (niwa-view--forward-input-now "What changed?"))
              (niwa-test--await 10 "Turn did not settle"
                (not (niwa-turn-busy-p buffer))))
            (let ((prompt (format "%S" (car (last prompts)))))
              (should (string-search "MODIFIED: " prompt))
              (should (string-search "-hello" prompt))
              (should (string-search "+goodbye" prompt)))
            ;; Receipt consumed the change; the next turn does not repeat it.
            (should-not (niwa-file-cache-detect-external-changes
                         (niwa-workspace-file-cache workspace))))
        (advice-remove 'niwa-acp-prompt 'niwa-claude-code-edited-file-test)
        (when (buffer-live-p view) (kill-buffer view))))))

(provide 'test-niwa-claude-code-session-reminders)
;;; test-niwa-claude-code-session-reminders.el ends here
