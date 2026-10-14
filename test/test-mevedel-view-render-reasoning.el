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

(mevedel-deftest mevedel-view-render-toggle-child-call ()
  ,test
  (test)
  :doc "grouped thinking keeps its muted header and full inset across toggles"
  (dolist (parent-indent '("" "  " "┌ "))
    (mevedel-view-test--with-buffers
      (with-current-buffer data-buf
        (insert "#+begin_reasoning\nThought body with **emphasis**.\n#+end_reasoning\n"))
      (with-current-buffer view-buf
        (let* ((source (cons 1 (with-current-buffer data-buf (point-max))))
               (child (plist-get
                       (mevedel-view--thinking-group-entry
                        (list (list 'reasoning (car source) (cdr source))) data-buf)
                       :group-child))
               (indent (concat parent-indent "  "))
               (inset (make-string (+ (string-width indent) 4) ?\s)))
          (setq child (plist-put child :source source))
          (let ((inhibit-read-only t)
                (mevedel-view--rendering-indent parent-indent))
            (goto-char mevedel-view--input-marker)
            (mevedel-view--insert-child-call-block child source 'derive))
          (mevedel-view-test--insert-composer-draft "> keep\nsecond line" 3)
          (cl-flet ((check-header ()
                      (goto-char (point-min))
                      (should (search-forward "Thinking..." mevedel-view--input-marker t))
                      (should (eq 'mevedel-view-thinking-summary
                                  (get-text-property (match-beginning 0) 'font-lock-face)))
                      (should (equal indent (get-text-property (point) 'line-prefix)))
                      (beginning-of-line)
                      (should (looking-at-p "  … Thinking"))
                      (search-forward "…")
                      (should (eq 'mevedel-view-thinking-marker
                                  (get-text-property (1- (point)) 'font-lock-face))))
                    (check-body ()
                      (goto-char (point-min))
                      (should (search-forward "Thought body" mevedel-view--input-marker t))
                      (should (equal inset (get-text-property (point) 'line-prefix)))
                      (should (equal inset (get-text-property (point) 'wrap-prefix)))))
            (check-header)
            (mevedel-view-toggle-section)
            (check-header)
            (check-body)
            ;; Reinsert through the canonical renderer with remembered state.
            (let ((inhibit-read-only t)
                  (mevedel-view--rendering-indent parent-indent))
              (delete-region (point-min) mevedel-view--input-marker)
              (goto-char (point-min))
              (mevedel-view--insert-child-call-block child source 'derive))
            (check-header)
            (check-body)
            (mevedel-view-toggle-section)
            (check-header)
            (should-not (search-forward "Thought body" mevedel-view--input-marker t))
            (should (equal "> keep\nsecond line" (mevedel-view--input-text)))))))))

(provide 'test-mevedel-view-render-reasoning)
;;; test-mevedel-view-render-reasoning.el ends here
