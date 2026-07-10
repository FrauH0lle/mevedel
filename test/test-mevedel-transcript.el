;;; test-mevedel-transcript.el --- Tests for transcript structure -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name
               load-file-name
               byte-compile-current-file))
          "helpers"))
(require 'mevedel-transcript)

(mevedel-deftest mevedel-transcript-prompt-transform-start ()
  ,test
  (test)
  :doc "returns the prompt after leading system reminders"
  (with-temp-buffer
    (insert "<system-reminder>\nremember\n</system-reminder>\n\n")
    (let ((prompt-start (point)))
      (insert "User prompt")
      (should (= prompt-start
                 (mevedel-transcript-prompt-transform-start)))))

  :doc "skips trailing ignored render data when locating prompt start"
  (with-temp-buffer
    (let ((prompt-start (point)))
      (insert (propertize "User prompt" 'gptel 'prompt))
      (insert (propertize "\n<render-data />" 'gptel 'ignore))
      (insert "\n\n")
      (should (= prompt-start
                 (mevedel-transcript-prompt-transform-start))))))

(mevedel-deftest mevedel-transcript--extract-segments/basic ()
  ,test
  (test)
  :doc "skips leading metadata and classifies transcript spans"
  (with-temp-buffer
    (insert ":PROPERTIES:\n:foo: bar\n:END:\n")
    (insert "#+begin_summary\nold summary\n#+end_summary\n\n")
    (let (user-start user-end response-start response-end tool-start tool-end)
      (setq user-start (point))
      (insert "User request\n")
      (setq user-end (point))
      (setq response-start (point))
      (insert (propertize "Assistant response\n" 'gptel 'response))
      (setq response-end (point))
      (setq tool-start (point))
      (insert (propertize "#+begin_tool\n(:name \"Read\")\n#+end_tool\n"
                          'gptel '(tool . "1")))
      (setq tool-end (point))
      (should (equal (mevedel-transcript--extract-segments
                      (point-min) (point-max))
                     `((user ,user-start ,user-end)
                       (response ,response-start ,response-end)
                       (tool ,tool-start ,tool-end))))))

  :doc "keeps restored newline-delimited prompts between responses"
  (with-temp-buffer
    (let (first-response-start first-response-end
          second-prompt-start second-prompt-end
          second-response-start second-response-end)
      (setq first-response-start (point))
      (insert "First response.\n")
      (setq first-response-end (1- (point)))
      (put-text-property first-response-start first-response-end
                         'gptel 'response)
      (setq second-prompt-start (point))
      (insert "Second prompt.\n")
      (setq second-prompt-end (point))
      (setq second-response-start (point))
      (insert "Second response.")
      (setq second-response-end (point))
      (put-text-property second-response-start second-response-end
                         'gptel 'response)
      (let ((segs (mevedel-transcript--extract-segments
                   (point-min) (point-max))))
        (should (equal '(response user response)
                       (mapcar #'car segs)))
        (should (<= (cadr (cadr segs)) second-prompt-start))
        (should (= second-prompt-end (caddr (cadr segs))))))))

(provide 'test-mevedel-transcript)
;;; test-mevedel-transcript.el ends here
