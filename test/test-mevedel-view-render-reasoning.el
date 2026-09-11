;;; test-mevedel-view-render-reasoning.el --- Reasoning audit projection -*- lexical-binding: t -*-

;;; Commentary:

;; Hidden provider history must stay out of reasoning disclosures.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-view)
(require 'mevedel-view-disclosure)
(require 'mevedel-view-render)

(mevedel-deftest mevedel-view-render-insert-expanded-disclosure
  (:before-each (setq mevedel-view--clean-reasoning-cache nil)
   :after-each (setq mevedel-view--clean-reasoning-cache nil))
  ,test
  (test)
  :doc "reasoning summaries and bodies exclude trusted provider history"
  (dolist (closed '(nil t))
    (with-temp-buffer
      (insert "#+begin_reasoning\n"
              (mevedel--format-hook-audit-record
               '(:type provider-tool-batch :id "call-1"))
              (mevedel--format-hook-audit-record
               '(:type provider-tool-batch-start :id "call-2"))
              "Now run the full suite.\n"
              (if closed "#+end_reasoning\n" ""))
      (let ((data (current-buffer))
            (start (point-min))
            (end (point-max)))
        (should (string-match-p
                 "(1 lines)"
                 (mevedel-view--thinking-summary data start end)))
        (should (equal "Now run the full suite."
                       (string-trim
                        (plist-get
                         (plist-get
                          (plist-get
                           (mevedel-view--thinking-group-entry
                            (list (list 'reasoning start end)) data)
                           :group-child)
                          :rendering)
                         :body))))
        (with-temp-buffer
          (let ((inhibit-read-only t))
            (mevedel-view-render-insert-expanded-disclosure
             data (cons start end) 'thinking-summary "Thinking..."))
          (should (string-match-p "Now run the full suite" (buffer-string)))
          (should-not (string-match-p "mevedel-hook-audit" (buffer-string))))))))

(provide 'test-mevedel-view-render-reasoning)
;;; test-mevedel-view-render-reasoning.el ends here
