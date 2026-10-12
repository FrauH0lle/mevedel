;;; test-mevedel-view-render-metadata.el --- Quoted transcript regressions -*- lexical-binding: t -*-

;;; Commentary:
;; Reduced execution-output fixture for a command that prints a transcript.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-view)
(require 'mevedel-view-render)
(require 'mevedel-view-stream)
(require 'mevedel-transcript-restore)
(require 'mevedel-tool-exec)
(require 'mevedel-pipeline)

(defconst mevedel-view-metadata-test--output
  (concat "historical output: \"quoted\" \\ path\n"
          "<!-- mevedel-render-data -->\n(:status error)\n"
          "<!-- /mevedel-render-data -->\n"
          "#+end_tool\n#+begin_tool (Bash :command \"old test\")\n"
          "(:name \"Bash\" :args (:command \"old test\"))\n\n"
          "old failure, not a new execution\n#+end_tool\n"
          "complete output tail\n"
          "#+begin_reasoning\nquoted unclosed reasoning\n")
  "Literal output of the one real call, containing a quoted historical call.")

(mevedel-deftest mevedel-view-render/quoted-execution-metadata ()
  ,test
  (test)
  :doc "normalization and resumable scanning retain one call across quoted markers"
  (dolist (split '(nil t))
    (with-temp-buffer
      (org-mode)
      (let ((begin (point)))
        (insert "#+begin_tool (Bash :command \"cat transcript\")\n"
                "(:name \"Bash\" :args (:command \"cat transcript\"))\n\n"
                "printed transcript\n")
        (let ((metadata-start (point)))
          (insert (mevedel-tool-render-data-format
                   (list :status 'success :state 'completed :exit-code 0
                         :execution-output mevedel-view-metadata-test--output)
                   "real-call"))
          (insert "\n#+end_tool\n")
          (put-text-property begin (point) 'gptel '(tool . "real-call"))
          ;; Reproduce bounds saved after the old first-close normalizer.
          (when split
            (remove-text-properties begin (point) '(mevedel-render-data nil))
            (save-excursion
              (goto-char metadata-start)
              (search-forward "<!-- /mevedel-render-data -->")
              (put-text-property (1+ metadata-start) (point)
                                 'gptel 'mevedel-render-data))))
        (let ((bounds (prin1-to-string (gptel--get-buffer-bounds))))
          (set-text-properties (point-min) (point-max) nil)
          (gptel--restore-props (read bounds)))
        (let ((original (buffer-string)))
          (dolist (normalize '(nil t t))
            (when normalize (mevedel-transcript-normalize-properties))
            (should (equal (substring-no-properties original)
                           (buffer-substring-no-properties (point-min) (point-max))))
            (let ((job (mevedel-transcript-scan-start (point-min) (point-max))))
              (while (not (mevedel-transcript-scan-step job)))
              (let ((tools (seq-filter (lambda (segment) (eq (car segment) 'tool))
                                       (mevedel-transcript-scan-result job))))
                (should (= 1 (length tools)))
                (should (= begin (cadar tools)))
                (should (= (point-max) (caddar tools)))))))
        (goto-char (point-max))
        (let ((prompt (point)))
          (insert "\n*** Next prompt\n")
          (remove-text-properties prompt (point) '(gptel nil))
          (should (= (1+ prompt)
                     (mevedel-transcript--user-prompt-start
                      prompt (point-max) nil)))))))

  :doc "full and incremental rows disclose complete successful output and preserve drafts"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data data-buf "*** Print the transcript\n" nil)
    (let ((context (list :result "printed transcript\n" :status 'success
                         :tool-use-id "real-call"
                         :render-data
                         (list :state 'completed :outcome 'success :exit-code 0
                               :execution-output mevedel-view-metadata-test--output)))
          attached)
      (mevedel-pipeline--step-attach-render-data
       context (lambda (updated) (setq attached (plist-get updated :result))) nil)
      (with-current-buffer data-buf
        (goto-char (point-max))
        (let* ((tool (gptel-make-tool :name "Bash" :function #'ignore
                                      :args '((:name "command" :type string))))
               (gptel-include-tool-results t)
               (info (list :buffer data-buf :position (point-marker)
                           :callback #'gptel--insert-response :tools (list tool)
                           :tool-use (list (list :id "real-call" :name "Bash"
                                                 :args '(:command "cat transcript")
                                                 :result attached)))))
          (gptel--display-tool-results
           (list (list tool '(:command "cat transcript") attached)) info)
          (should (string-search ",#+end_tool" (buffer-string)))
          (should (mevedel-tool-render-data-update
                   data-buf "real-call" '(:exit-code 0))))))
    (mevedel-view-test--insert-data
     data-buf
     (concat "#+begin_tool (Read :file_path \"neighbor.el\")\n"
             "(:name \"Read\" :args (:file_path \"neighbor.el\"))\n\n"
             "neighbor output\n#+end_tool\n")
     '(tool . "neighbor-call"))
    (with-current-buffer view-buf
      (mevedel-view-test--insert-composer-draft "> draft\nsecond line" 3)
      (mevedel-view-stream-begin-turn
       mevedel-view--status-marker
       (with-current-buffer data-buf (copy-marker (point-min))))
      (mevedel-view-render-live-update data-buf)
      (let (expanded)
        (dolist (refresh '(full live full batch))
          (pcase refresh
            ('full (mevedel-view--full-rerender))
            ('live (mevedel-view-render-live-update data-buf))
            ('batch
             (mevedel-view-stream-stop)
             (mevedel-view-render-batched-full)
             (should mevedel-view-render--batch)
             (cl-loop repeat 100 while mevedel-view-render--batch do
                      (mevedel-view-render--batch-step
                       view-buf mevedel-view-render--batch))
             (should-not mevedel-view-render--batch)))
          (should (equal "> draft\nsecond line" (mevedel-view--input-text)))
          (goto-char (point-min))
          (search-forward "Bash: cat transcript")
          (let ((row (match-beginning 0)))
            (should (eq 'tool-summary (get-text-property row 'mevedel-view-type)))
            (should (string-search "✓ Bash: cat transcript" (buffer-string)))
            (should (string-search "Read: neighbor.el" (buffer-string)))
            (should-not (string-search "? Bash" (buffer-string)))
            (if expanded
                (should-not (get-text-property row 'mevedel-view-collapsed))
              (goto-char row)
              (mevedel-view-toggle-section)
              (setq expanded t)))
          (should (string-search mevedel-view-metadata-test--output (buffer-string)))
          (should-not (string-search ":execution-output" (buffer-string))))))))

(mevedel-deftest mevedel-transcript-scan-step/quoted-large-payload ()
  ,test
  (test)
  :doc "reads large metadata once while resumable indexing skips its quoted controls"
  (with-temp-buffer
    (org-mode)
    (insert "#+begin_tool\n(:name \"Bash\" :args (:command \"cat transcript\"))\n\nprinted transcript\n")
    (let* ((payload (mapconcat (lambda (_)
                                (concat (make-string 1024 ?x) "\n"
                                        mevedel-view-metadata-test--output))
                              (number-sequence 1 128) "\n"))
           (metadata-start (1+ (point)))
           (reader (symbol-function 'mevedel-tool-render-data-block-at))
           (reads 0))
      (insert (mevedel-tool-render-data-format
               (list :status 'success :execution-output payload) "real-call"))
      (insert "\n#+end_tool\n")
      (put-text-property (point-min) (point-max) 'gptel '(tool . "real-call"))
      (cl-letf (((symbol-function 'mevedel-tool-render-data-block-at)
                 (lambda (start &optional limit)
                   (when (= start metadata-start) (cl-incf reads))
                   (funcall reader start limit))))
        (let ((job (mevedel-transcript-scan-start (point-min) (point-max)))
              (steps 0))
          (while (not (mevedel-transcript-scan-step job))
            (cl-incf steps)
            (should (< steps 30)))
          (should (= 1 reads))
          (should (equal (list (list 'tool (point-min) (point-max)))
                         (mevedel-transcript-scan-result job))))))))

(mevedel-deftest mevedel-view--delete-request-summaries/quoted-payload ()
  ,test
  (test)
  :doc "deletes a complete generated summary without consuming neighboring literal output"
  (with-temp-buffer
    (insert "literal output\n")
    (let ((literal (concat "<!-- mevedel-render-data -->\n"
                           "(:kind request-summary :text \"literal\")\n"
                           "<!-- /mevedel-render-data -->\n")))
      (insert literal)
      (insert (mevedel-tool-render-data-format
               (list :kind 'request-summary
                     :text mevedel-view-metadata-test--output)))
      (insert "neighbor output\n")
      (mevedel-view--delete-request-summaries (current-buffer) nil nil)
      (should (equal (concat "literal output\n" literal "neighbor output\n")
                     (buffer-string)))))
  :doc "saved summary provenance need not cover the optional preceding newline"
  (with-temp-buffer
    (insert "answer\n")
    (let ((start (point)))
      (insert (mevedel-tool-render-data-format '(:kind request-summary)))
      (set-text-properties start (point) nil)
      (gptel--restore-props
       (list (list 'mevedel-render-data (list (1+ start) (1- (point)))))))
    (insert "neighbor output\n")
    (mevedel-view--delete-request-summaries (current-buffer) nil nil)
    (should (equal "answer\nneighbor output\n" (buffer-string)))))

(provide 'test-mevedel-view-render-metadata)
;;; test-mevedel-view-render-metadata.el ends here
