;;; test-niwa-view-native-context.el --- Native receipt projection -*- lexical-binding: t -*-

;;; Commentary:
;; Native context receipts use the shared typed injection record and its view rows.

;;; Code:

(require 'helpers
         (file-name-concat (file-name-directory (or load-file-name buffer-file-name)) "helpers"))
(require 'niwa-view)
(require 'niwa-reminders)
(require 'niwa-view-render)
(require 'niwa-transcript-restore)

(niwa-deftest niwa-view--full-rerender/native-context (:quiet t)
  ,test
  (test)
  :doc "typed reminder sources stay at their initial or mid-turn delivery position"
  (niwa-view-test--with-buffers
    (niwa-view-test--insert-data data-buf "Question?\n\n" nil)
    (niwa-view-test--insert-data
     data-buf
     (niwa--format-hook-audit-record
      (niwa-reminders--injection-record
       '((:type pending-events :body "Reconcile previous effects.")
         (:type mode-constraints :body "Permission policy.")) 'turn-start))
     'niwa-hook-audit)
    (niwa-view-test--insert-data data-buf "First response.\n" 'response)
    (niwa-view-test--insert-data
     data-buf
     (niwa--format-hook-audit-record
      (niwa-reminders--injection-record
       '((:type date-change :body "Date changed.")
         (:type context-environment :body "## Environment\nOBSERVATION-BODY-5521")) 'mid-turn))
     'niwa-hook-audit)
    (niwa-view-test--insert-data data-buf "Later response.\n" 'response)
    (with-current-buffer view-buf
      (niwa-view--full-rerender)
      (let ((text (buffer-string)))
        (should (< (string-search "Question?" text)
                   (string-search "2 system reminders (pending-events, mode-constraints)" text)
                   (string-search "Assistant" text)
                   (string-search "First response." text)
                   (string-search "2 system reminders (date-change, context-environment)" text)
                   (string-search "Later response." text))))
      (should-not (string-search "Thinking" (buffer-string)))
      (should-not (string-search "OBSERVATION-BODY-5521" (buffer-string)))
      (goto-char (point-min))
      (search-forward "2 system reminders")
      (should (eq 'hook-audit (get-text-property (1- (point)) 'niwa-view-type)))
      (niwa-view-toggle-section)
      (should (string-search "1. pending-events" (buffer-string)))
      (should (string-search "2. mode-constraints" (buffer-string)))
      (should (string-search "Permission policy." (buffer-string))))))

(provide 'test-niwa-view-native-context)
;;; test-niwa-view-native-context.el ends here
