;;; test-niwa-view-render-metadata.el --- Quoted transcript regressions -*- lexical-binding: t -*-

;;; Commentary:
;; Reduced execution-output fixture for a command that prints a transcript.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'niwa-view)
(require 'niwa-view-render)
(require 'niwa-view-stream)
(require 'niwa-transcript-restore)
(require 'niwa-tool-exec)
(require 'niwa-pipeline)

(defconst niwa-view-metadata-test--output
  (concat "historical output: \"quoted\" \\ path\n"
          "<!-- niwa-render-data -->\n(:status error)\n"
          "<!-- /niwa-render-data -->\n"
          "#+end_tool\n#+begin_tool (Bash :command \"old test\")\n"
          "(:name \"Bash\" :args (:command \"old test\"))\n\n"
          "old failure, not a new execution\n#+end_tool\n"
          "complete output tail\n"
          "#+begin_reasoning\nquoted unclosed reasoning\n")
  "Literal output of the one real call, containing a quoted historical call.")

(niwa-deftest niwa-view-render/quoted-execution-metadata ()
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
          (insert (niwa-tool-render-data-format
                   (list :status 'success :state 'completed :exit-code 0
                         :execution-output niwa-view-metadata-test--output)
                   "real-call"))
          (insert "\n#+end_tool\n")
          (put-text-property begin (point) 'gptel '(tool . "real-call"))
          ;; Reproduce bounds saved after the old first-close normalizer.
          (when split
            (remove-text-properties begin (point) '(niwa-render-data nil))
            (save-excursion
              (goto-char metadata-start)
              (search-forward "<!-- /niwa-render-data -->")
              (put-text-property (1+ metadata-start) (point)
                                 'gptel 'niwa-render-data))))
        (let ((bounds (prin1-to-string (gptel--get-buffer-bounds))))
          (set-text-properties (point-min) (point-max) nil)
          (gptel--restore-props (read bounds)))
        (let ((original (buffer-string)))
          (dolist (normalize '(nil t t))
            (when normalize (niwa-transcript-normalize-properties))
            (should (equal (substring-no-properties original)
                           (buffer-substring-no-properties (point-min) (point-max))))
            (let ((job (niwa-transcript-scan-start (point-min) (point-max))))
              (while (not (niwa-transcript-scan-step job)))
              (let ((tools (seq-filter (lambda (segment) (eq (car segment) 'tool))
                                       (niwa-transcript-scan-result job))))
                (should (= 1 (length tools)))
                (should (= begin (cadar tools)))
                (should (= (point-max) (caddar tools)))))))
        (goto-char (point-max))
        (let ((prompt (point)))
          (insert "\n*** Next prompt\n")
          (remove-text-properties prompt (point) '(gptel nil))
          (should (= (1+ prompt)
                     (niwa-transcript--user-prompt-start
                      prompt (point-max) nil)))))))

  :doc "full and incremental rows disclose complete successful output and preserve drafts"
  (niwa-view-test--with-buffers
    (niwa-view-test--insert-data data-buf "*** Print the transcript\n" nil)
    (let ((context (list :result "printed transcript\n" :status 'success
                         :tool-use-id "real-call"
                         :render-data
                         (list :state 'completed :outcome 'success :exit-code 0
                               :execution-output niwa-view-metadata-test--output)))
          attached)
      (niwa-pipeline--step-attach-render-data
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
          (should (niwa-tool-render-data-update
                   data-buf "real-call" '(:exit-code 0))))))
    (niwa-view-test--insert-data
     data-buf
     (concat "#+begin_tool (Read :file_path \"neighbor.el\")\n"
             "(:name \"Read\" :args (:file_path \"neighbor.el\"))\n\n"
             "neighbor output\n#+end_tool\n")
     '(tool . "neighbor-call"))
    (with-current-buffer view-buf
      (niwa-view-test--insert-composer-draft "> draft\nsecond line" 3)
      (niwa-view-stream-begin-turn
       niwa-view--status-marker
       (with-current-buffer data-buf (copy-marker (point-min))))
      (niwa-view-render-live-update data-buf)
      (let (expanded)
        (dolist (refresh '(full live full batch))
          (pcase refresh
            ('full (niwa-view--full-rerender))
            ('live (niwa-view-render-live-update data-buf))
            ('batch
             (niwa-view-stream-stop)
             (niwa-view-render-batched-full)
             (should niwa-view-render--batch)
             (cl-loop repeat 100 while niwa-view-render--batch do
                      (niwa-view-render--batch-step
                       view-buf niwa-view-render--batch))
             (should-not niwa-view-render--batch)))
          (should (equal "> draft\nsecond line" (niwa-view-composer-draft-text nil t)))
          (goto-char (point-min))
          (search-forward "Bash: cat transcript")
          (let ((row (match-beginning 0)))
            (should (eq 'tool-summary (get-text-property row 'niwa-view-type)))
            (should (string-search "✓ Bash: cat transcript" (buffer-string)))
            (should (string-search "Read: neighbor.el" (buffer-string)))
            (should-not (string-search "? Bash" (buffer-string)))
            (if expanded
                (should-not (get-text-property row 'niwa-view-collapsed))
              (goto-char row)
              (niwa-view-toggle-section)
              (setq expanded t)))
          (should (string-search niwa-view-metadata-test--output (buffer-string)))
          (should-not (string-search ":execution-output" (buffer-string))))))))

(niwa-deftest niwa-transcript-scan-step/quoted-large-payload ()
  ,test
  (test)
  :doc "reads large metadata once while resumable indexing skips its quoted controls"
  (with-temp-buffer
    (org-mode)
    (insert "#+begin_tool\n(:name \"Bash\" :args (:command \"cat transcript\"))\n\nprinted transcript\n")
    (let* ((payload (mapconcat (lambda (_)
                                (concat (make-string 1024 ?x) "\n"
                                        niwa-view-metadata-test--output))
                              (number-sequence 1 128) "\n"))
           (metadata-start (1+ (point)))
           (reader (symbol-function 'niwa-tool-render-data-block-at))
           (reads 0))
      (insert (niwa-tool-render-data-format
               (list :status 'success :execution-output payload) "real-call"))
      (insert "\n#+end_tool\n")
      (put-text-property (point-min) (point-max) 'gptel '(tool . "real-call"))
      (cl-letf (((symbol-function 'niwa-tool-render-data-block-at)
                 (lambda (start &optional limit)
                   (when (= start metadata-start) (cl-incf reads))
                   (funcall reader start limit))))
        (let ((job (niwa-transcript-scan-start (point-min) (point-max)))
              (steps 0))
          (while (not (niwa-transcript-scan-step job))
            (cl-incf steps)
            (should (< steps 30)))
          (should (= 1 reads))
          (should (equal (list (list 'tool (point-min) (point-max)))
                         (niwa-transcript-scan-result job))))))))

(niwa-deftest niwa-view--delete-request-summaries/quoted-payload ()
  ,test
  (test)
  :doc "deletes a complete generated summary without consuming neighboring literal output"
  (with-temp-buffer
    (insert "literal output\n")
    (let ((literal (concat "<!-- niwa-render-data -->\n"
                           "(:kind request-summary :text \"literal\")\n"
                           "<!-- /niwa-render-data -->\n")))
      (insert literal)
      (insert (niwa-tool-render-data-format
               (list :kind 'request-summary
                     :text niwa-view-metadata-test--output)))
      (insert "neighbor output\n")
      (niwa-view--delete-request-summaries (current-buffer) nil nil)
      (should (equal (concat "literal output\n" literal "neighbor output\n")
                     (buffer-string)))))
  :doc "saved summary provenance need not cover the optional preceding newline"
  (with-temp-buffer
    (insert "answer\n")
    (let ((start (point)))
      (insert (niwa-tool-render-data-format '(:kind request-summary)))
      (set-text-properties start (point) nil)
      (gptel--restore-props
       (list (list 'niwa-render-data (list (1+ start) (1- (point)))))))
    (insert "neighbor output\n")
    (niwa-view--delete-request-summaries (current-buffer) nil nil)
    (should (equal "answer\nneighbor output\n" (buffer-string)))))

(provide 'test-niwa-view-render-metadata)
;;; test-niwa-view-render-metadata.el ends here
