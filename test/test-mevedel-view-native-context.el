;;; test-mevedel-view-native-context.el --- Native receipt projection -*- lexical-binding: t -*-

;;; Commentary:
;; Native context receipts remain durable evidence but use compact view rows.

;;; Code:

(require 'helpers
         (file-name-concat (file-name-directory (or load-file-name buffer-file-name)) "helpers"))
(require 'mevedel-view)
(require 'mevedel-view-render)
(require 'mevedel-transcript-restore)

(mevedel-deftest mevedel-view--full-rerender/native-context (:quiet t)
  ,test
  (test)
  :doc "a receipt after authored input hides its marker and folds its reminder"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data
     data-buf
     (concat "Which model are you?\n\n"
             "<!-- mevedel-delivery:f1292edfa563f8a9b65f4ac437c28fb6 -->\n"
             "<system-reminder>\nPermission mode: `edits'.\n</system-reminder>\n\n")
     nil)
    (mevedel-view-test--insert-data data-buf "Claude Opus.\n" 'response)
    (with-current-buffer view-buf
      (goto-char (point-max))
      (insert "> Keep this draft\nand its second line")
      (mevedel-view--full-rerender)
      (let ((text (buffer-substring-no-properties (point-min) mevedel-view--input-marker)))
        (should (string-search "Which model are you?" text))
        (should (string-search "Claude Opus." text))
        (should (string-search "System reminder" text))
        (should-not (string-search "Thinking" text))
        (should-not (string-search "mevedel-delivery:" text))
        (should-not (string-search "<system-reminder>" text)))
      (goto-char (point-min))
      (search-forward "System reminder")
      (mevedel-view-toggle-section)
      (should (string-search "Permission mode: `edits'." (buffer-string)))
      (should-not (string-search "mevedel-delivery:" (buffer-string)))
      (should (equal "> Keep this draft\nand its second line" (mevedel-view--input-text)))
      (with-current-buffer data-buf (mevedel-transcript-restore-properties t))
      (mevedel-view--full-rerender)
      (should-not (string-search "mevedel-delivery:" (buffer-string)))
      (should (equal "> Keep this draft\nand its second line" (mevedel-view--input-text)))))

  :doc "adjacent reminders share a disclosure without crossing response boundaries"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data data-buf "Question?\n\n" nil)
    (mevedel-view-test--insert-data
     data-buf
     (concat "<!-- mevedel-delivery:f1292edfa563f8a9b65f4ac437c28fb6 -->\n"
             "<system-reminder>\nFirst policy.\n</system-reminder>\n\n"
             "<system-reminder>\nSecond policy.\n</system-reminder>\n")
     nil)
    (mevedel-view-test--insert-data data-buf "Answer.\n" 'response)
    (mevedel-view-test--insert-data
     data-buf "<system-reminder>\nLater policy.\n</system-reminder>\n" 'ignore)
    (with-current-buffer view-buf
      (goto-char (point-max))
      (insert "> Keep this draft\nand its second line")
      (mevedel-view--full-rerender)
      (should (= 1 (mevedel-view-test--count-substring
                    "2 system reminders" (buffer-string))))
      (should (= 1 (mevedel-view-test--count-substring
                    "System reminder (1 line)" (buffer-string))))
      (goto-char (point-min))
      (search-forward "2 system reminders")
      (mevedel-view-toggle-section)
      (should (string-search "First policy." (buffer-string)))
      (should (string-search "Second policy." (buffer-string)))
      (should-not (string-search "Later policy." (buffer-string)))
      (should-not (string-search "<system-reminder>" (buffer-string)))
      (mevedel-view--full-rerender)
      (should (string-search "Second policy." (buffer-string)))
      (goto-char (point-min))
      (search-forward "2 system reminders")
      (mevedel-view-toggle-section)
      (should-not (string-search "First policy." (buffer-string)))
      (should-not (string-search "Second policy." (buffer-string)))
      (mevedel-view-render-batched-full)
      (cl-loop repeat 100 while mevedel-view-render--batch do
               (mevedel-view-render--batch-step view-buf mevedel-view-render--batch))
      (should-not mevedel-view-render--batch)
      (should (= 1 (mevedel-view-test--count-substring
                    "2 system reminders" (buffer-string))))
      (should (equal "> Keep this draft\nand its second line"
                     (mevedel-view--input-text)))))

  :doc "typed reminder sources stay at their initial or mid-turn delivery position"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data data-buf "Question?\n\n" nil)
    (mevedel-view-test--insert-data
     data-buf
     (mevedel--format-hook-audit-record
      (mevedel-reminders--injection-record
       '((:type pending-events :body "Reconcile previous effects.")
         (:type mode-constraints :body "Permission policy.")) 'turn-start))
     'mevedel-hook-audit)
    (mevedel-view-test--insert-data data-buf "First response.\n" 'response)
    (mevedel-view-test--insert-data
     data-buf
     (mevedel--format-hook-audit-record
      (mevedel-reminders--injection-record
       '((:type date-change :body "Date changed.")) 'mid-turn))
     'mevedel-hook-audit)
    (mevedel-view-test--insert-data data-buf "Later response.\n" 'response)
    (with-current-buffer view-buf
      (mevedel-view--full-rerender)
      (let ((text (buffer-string)))
        (should (< (string-search "Question?" text)
                   (string-search "2 system reminders (pending-events, mode-constraints)" text)
                   (string-search "Assistant" text)
                   (string-search "First response." text)
                   (string-search "1 system reminder (date-change)" text)
                   (string-search "Later response." text))))
      (goto-char (point-min))
      (search-forward "2 system reminders")
      (should (eq 'hook-audit (get-text-property (1- (point)) 'mevedel-view-type)))
      (mevedel-view-toggle-section)
      (should (string-search "1. pending-events" (buffer-string)))
      (should (string-search "2. mode-constraints" (buffer-string)))
      (should (string-search "Permission policy." (buffer-string)))))

  :doc "restored ignored receipt with surrounding whitespace never becomes thinking"
  (dolist (padding '("" "\n" "\n\n" " \n"))
    (mevedel-view-test--with-buffers
      (mevedel-view-test--insert-data data-buf "Question?\n\n" nil)
      (mevedel-view-test--insert-data
       data-buf (concat padding "<!-- mevedel-delivery:f1292edfa563f8a9b65f4ac437c28fb6 -->\n" padding)
       'ignore)
      (mevedel-view-test--insert-data data-buf "<system-reminder>\nPermission mode.\n</system-reminder>\n" nil)
      (mevedel-view-test--insert-data data-buf "Answer.\n" 'response)
      (with-current-buffer view-buf
        (mevedel-view--full-rerender)
        (should-not (string-search "Thinking" (buffer-string)))
        (should-not (string-search "mevedel-delivery:" (buffer-string)))
        (should (string-search "System reminder" (buffer-string)))
        (mevedel-view-render-batched-full)
        (cl-loop repeat 100 while mevedel-view-render--batch do
                 (mevedel-view-render--batch-step
                  view-buf mevedel-view-render--batch))
        (should-not mevedel-view-render--batch)
        (should-not (string-search "Thinking" (buffer-string)))
        (should-not (string-search "mevedel-delivery:" (buffer-string))))))

  :doc "hiding receipts preserves genuine thinking and its expanded body"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data data-buf "Question?\n\n" nil)
    (mevedel-view-test--insert-data
     data-buf
     (concat "<!-- mevedel-delivery:f1292edfa563f8a9b65f4ac437c28fb6 -->\n"
             "#+begin_reasoning\nConsider the evidence.\n#+end_reasoning\n")
     'ignore)
    (mevedel-view-test--insert-data data-buf "Answer.\n" 'response)
    (with-current-buffer view-buf
      (mevedel-view--full-rerender)
      (goto-char (point-min))
      (search-forward "Thinking")
      (mevedel-view-toggle-section)
      (should (string-search "Consider the evidence." (buffer-string)))
      (should-not (string-search "mevedel-delivery:" (buffer-string)))))

  :doc "receipt markup quoted inside an assistant response remains literal"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data
     data-buf
     (concat "This is an example:\n"
             "<!-- mevedel-delivery:f1292edfa563f8a9b65f4ac437c28fb6 -->\n"
             "<system-reminder>\nLiteral reminder.\n</system-reminder>\n")
     'response)
    (with-current-buffer view-buf
      (mevedel-view--full-rerender)
      (should (string-search "mevedel-delivery:" (buffer-string)))
      (should (string-search "<system-reminder>" (buffer-string)))
      (should-not (string-search "System reminder" (buffer-string))))))

(mevedel-deftest mevedel-view--system-reminder-bodies-from-text ()
  ,test
  (test)
  :doc "parses adjacent blocks and preserves nested literal reminders"
  (should
   (equal '("First." "Nested:\n<system-reminder>\nLiteral.\n</system-reminder>")
          (mevedel-view--system-reminder-bodies-from-text
           (concat " \n<system-reminder>\nFirst.\n</system-reminder>\n \n"
                   "<system-reminder>\nNested:\n<system-reminder>\nLiteral.\n"
                   "</system-reminder>\n</system-reminder>\n"))))
  :doc "rejects ordinary prose, incomplete blocks and mixed content"
  (dolist (text '(nil "" "Literal text"
                  "<system-reminder>\nUnfinished."
                  "Example:\n<system-reminder>\nLiteral.\n</system-reminder>"
                  "<system-reminder>\nFirst.\n</system-reminder>\nProse."
                  "<system-reminder>\nFirst.\n</system-reminder>\n<system-reminder>\nIncomplete."))
    (should-not (mevedel-view--system-reminder-bodies-from-text text))))

(provide 'test-mevedel-view-native-context)
;;; test-mevedel-view-native-context.el ends here
