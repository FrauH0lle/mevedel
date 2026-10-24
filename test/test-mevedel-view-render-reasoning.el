;;; test-mevedel-view-render-reasoning.el --- Reasoning audit projection -*- lexical-binding: t -*-

;;; Commentary:

;; Thinking remains visible independently of tool activity, while source-backed
;; manual folds and hidden provider history retain their normal semantics.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-view)
(require 'mevedel-view-disclosure)
(require 'mevedel-view-render)
(require 'mevedel-view-stream)
(require 'mevedel-tool-ptc)

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
        (with-temp-buffer
          (let ((inhibit-read-only t))
            (mevedel-view-render-insert-expanded-disclosure
             data (cons start end) 'thinking-summary "Thinking..."))
          (should (string-match-p "Now run the full suite" (buffer-string)))
          (should-not (string-match-p "mevedel-hook-audit" (buffer-string))))))))

(mevedel-deftest mevedel-view--flush-thinking-group ()
  ,test
  (test)
  :doc "standalone thinking keeps its muted header, exact body and display inset"
  (mevedel-view-test--with-buffers
    (let ((body (concat "Thought body with **emphasis**.\nA second line.\n"
                        "[Reference](https://example.test)\n\n"
                        "| Step | State |\n|---|---|\n| Read | Done |")))
      (mevedel-view-test--insert-data
       data-buf (concat "#+begin_reasoning\n" body "\n#+end_reasoning\n") 'ignore)
      (with-current-buffer view-buf
        (mevedel-view--full-rerender)
        (cl-flet ((check-header ()
                    (goto-char (point-min))
                    (should (search-forward "Thinking..." mevedel-view--input-marker t))
                    (should (eq 'mevedel-view-thinking-summary
                                (get-text-property (match-beginning 0) 'font-lock-face)))
                    (beginning-of-line)
                    (should (looking-at-p "  … Thinking"))
                    (search-forward "…")
                    (should (eq 'mevedel-view-thinking-marker
                                (get-text-property (1- (point)) 'font-lock-face))))
                  (check-body ()
                    (goto-char (point-min))
                    (should (search-forward body mevedel-view--input-marker t))
                    (should-not (invisible-p (match-beginning 0)))
                    (should (equal "    " (get-text-property (point) 'line-prefix)))
                    (should (equal "    " (get-text-property (point) 'wrap-prefix)))
                    (should (equal body (buffer-substring-no-properties
                                         (match-beginning 0) (match-end 0))))))
          (dotimes (_ 2)
            (check-body)
            (check-header)
            (mevedel-view-toggle-section)
            (check-header)
            (should-not (string-search body (buffer-string)))
            (mevedel-view-toggle-section)
            (check-body)
            (mevedel-view--full-rerender))))))
  :doc "empty and signature-only metadata create no blank thinking cards or boundaries"
  (mevedel-view-test--with-buffers
    (dotimes (i 4)
      (mevedel-view-test--insert-data
       data-buf
       (format "(:name \"Read\" :args (:file_path \"f%d\"))\n\nbody\n" i)
       (cons 'tool (format "empty-%d" i)))
      ;; Signatures are provider metadata, not transcript body text.
      (mevedel-view-test--insert-data
       data-buf
       (propertize "#+begin_reasoning\n \n#+end_reasoning\n"
                   'gptel-reasoning '(:signature "opaque-provider-signature"))
       'ignore))
    (with-current-buffer view-buf
      (mevedel-view--full-rerender)
      (should-not (string-search "Thinking" (buffer-string)))
      (should-not (string-search "opaque-provider-signature" (buffer-string)))
      (should (string-search "Read 4 files" (buffer-string)))))
  :doc "compound tools nested in reasoning retain children without hiding surrounding thoughts"
  (mevedel-view-test--with-buffers
    (mevedel-tool-ptc--register)
    (mevedel-view-test--insert-data
     data-buf "#+begin_reasoning\nBefore nested tool.\n" 'ignore)
    (mevedel-view-test--insert-data
     data-buf
     (concat
      "#+begin_tool\n(:name \"ToolCall\" :args (:expression \"(list 1 2)\"))\n\nreturned\n"
      (mevedel-tool-render-data-format
       '(:kind ptc :outcome completed
         :calls ((:id "nested/1" :tool "Read" :status success
                  :args (:file_path "a.el") :result "first output")
                 (:id "nested/2" :tool "Read" :status success
                  :args (:file_path "b.el") :result "second output")))
       "nested")
      "#+end_tool\n")
     '(tool . "nested"))
    (mevedel-view-test--insert-data
     data-buf "After nested tool.\n#+end_reasoning\n" 'ignore)
    (with-current-buffer view-buf
      (mevedel-view--full-rerender)
      (dotimes (_ 3)
        (goto-char (point-min))
        (search-forward "Before nested tool.")
        (search-forward "ToolCall: 2 calls")
        (mevedel-view-toggle-section)
        (search-forward "Read: a.el")
        (mevedel-view-toggle-section)
        (should (string-search "first output" (buffer-string)))
        (should-not (string-search "second output" (buffer-string)))
        (goto-char (point-min))
        (search-forward "Read: a.el")
        (mevedel-view-toggle-section)
        (goto-char (point-min))
        (search-forward "ToolCall: 2 calls")
        (mevedel-view-toggle-section)
        (should-not (string-search "Read: a.el" (buffer-string)))
        (should-not (string-search "first output" (buffer-string)))
        (should (string-search "Before nested tool." (buffer-string)))
        (should (string-search "After nested tool." (buffer-string)))
        (should (= 1 (mevedel-view-test--count-substring
                      "ToolCall: 2 calls" (buffer-string))))
        (mevedel-view--full-rerender)))))

(mevedel-deftest mevedel-view-render-live-update/thinking ()
  (let ((collapsed ,collapsed))
    (mevedel-view-test--with-buffers
      (with-current-buffer view-buf
        (mevedel-view-stream-begin-turn
         mevedel-view--status-marker
         (with-current-buffer data-buf (copy-marker (point-min)))))
      (mevedel-view-test--insert-data data-buf "#+begin_reasoning\nFirst thought" 'ignore)
      (with-current-buffer view-buf
        (mevedel-view-render-live-update data-buf)
        (should (string-search "First thought" (buffer-string)))
        (goto-char (point-min))
        (search-forward "Thinking...")
        ;; Exercise an explicit expansion too, not only the open default.
        (mevedel-view-toggle-section)
        (unless collapsed (mevedel-view-toggle-section))
        (mevedel-view-test--insert-composer-draft "> keep\nsecond line" 4))
      (cl-flet ((check ()
                  (with-current-buffer view-buf
                    (should (eq (not collapsed)
                                (not (null (string-search "First thought" (buffer-string))))))
                    (should (equal "> keep\nsecond line" (mevedel-view--input-text)))
                    (should (= 4 (- (point) (mevedel-view--input-start)))))))
        (dolist (chunk '(" continues" " through streaming.\n" "#+end_reasoning\n"))
          (mevedel-view-test--insert-data data-buf chunk 'ignore)
          (with-current-buffer view-buf (mevedel-view-render-live-update data-buf))
          (check))
        (mevedel-view-test--insert-data
         data-buf "(:name \"Read\" :args (:file_path \"next.el\"))\n\nfinished\n"
         '(tool . "thinking-next"))
        (with-current-buffer view-buf (mevedel-view-render-live-update data-buf))
        (check)
        ;; A distinct adjacent block gets its own default, not the first choice.
        (mevedel-view-test--insert-data
         data-buf "#+begin_reasoning\nSecond thought\n#+end_reasoning\n" 'ignore)
        (mevedel-view-test--insert-data
         data-buf "#+begin_reasoning\nThird thought\n#+end_reasoning\n" 'ignore)
        (with-current-buffer view-buf (mevedel-view-render-live-update data-buf))
        (check)
        (with-current-buffer view-buf
          (should (string-search "Second thought" (buffer-string)))
          (should (string-search "Third thought" (buffer-string)))
          (mevedel-view-render-settle
           data-buf (with-current-buffer data-buf (point-min))
           (with-current-buffer data-buf (point-max))))
        (check)
        (with-current-buffer view-buf
          (setq mevedel-view--data-turn-start nil)
          (mevedel-view-stream-set-in-flight-turn-start nil)
          (mevedel-view--full-rerender))
        (check))))
  (collapsed)
  :doc "manual collapse survives chunks, tool completion, settlement and full redraw" t
  :doc "manual expansion survives chunks, tool completion, settlement and full redraw" nil)

(provide 'test-mevedel-view-render-reasoning)
;;; test-mevedel-view-render-reasoning.el ends here
