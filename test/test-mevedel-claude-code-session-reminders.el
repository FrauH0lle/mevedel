;;; test-mevedel-claude-code-session-reminders.el --- Claude prompt reminders -*- lexical-binding: t -*-

;;; Commentary:
;; Configured reminders reach a Claude prompt with the content gptel's
;; dispatch gives them.

;;; Code:

(require 'mevedel-engine-test-support
         (file-name-concat
          (file-name-directory (or load-file-name buffer-file-name))
          "mevedel-engine-test-support"))
(require 'mevedel-claude-code)
(require 'mevedel-claude-code-session)
(require 'mevedel-file-state)
(require 'mevedel-reminders)
(require 'mevedel-view)

(mevedel-deftest mevedel-claude-code-send/edited-file (:quiet t)
  ,test
  (test)
  :doc "an external edit reaches the Claude prompt with its diff"
  (mevedel-engine-test--with-claude-session
    (mevedel-request-end)
    (let ((view (generate-new-buffer " *claude-edited-file-view*"))
          (file (file-name-concat root "notes.txt"))
          prompts)
      (unwind-protect
          (progn
            (setq-local gptel-system-prompt "Edited-file fixture" gptel-tools nil)
            (unless (cl-find 'edited-file (mevedel-session-reminders session)
                             :key #'mevedel-reminder-type)
              (mevedel-session-add-reminder
               session (mevedel-reminders-make-edited-file)))
            (write-region "hello\n" nil file nil 'silent)
            (mevedel-file-cache-put (mevedel-workspace-file-cache workspace)
                                    (mevedel-file-state-from-file file))
            (write-region "goodbye\n" nil file nil 'silent)
            (set-file-times file (time-add (current-time) 2))
            (mevedel-chat-install-request-hooks)
            (mevedel-view--setup view buffer)
            (advice-add 'mevedel-acp-prompt :before
                        (lambda (_c input &rest _) (push input prompts))
                        '((name . mevedel-claude-code-edited-file-test)))
            (cl-letf (((symbol-function 'mevedel-claude-code-launch)
                       (mevedel-engine-test--claude-launch
                        (lambda (&rest _) (list :meta '((echoAllText . t)))))))
              (with-current-buffer view
                (mevedel-view--forward-input-now "What changed?"))
              (mevedel-test--await 10 "Turn did not settle"
                (not (mevedel-turn-busy-p buffer))))
            (let ((prompt (format "%S" (car (last prompts)))))
              (should (string-search "MODIFIED: " prompt))
              (should (string-search "-hello" prompt))
              (should (string-search "+goodbye" prompt)))
            ;; Receipt consumed the change; the next turn does not repeat it.
            (should-not (mevedel-file-cache-detect-external-changes
                         (mevedel-workspace-file-cache workspace))))
        (advice-remove 'mevedel-acp-prompt 'mevedel-claude-code-edited-file-test)
        (when (buffer-live-p view) (kill-buffer view))))))

(provide 'test-mevedel-claude-code-session-reminders)
;;; test-mevedel-claude-code-session-reminders.el ends here
