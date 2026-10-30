;;; test-mevedel-collaboration-projection-thinking.el --- Browser thinking -*- lexical-binding: t; -*-

;;; Commentary:
;; Thinking must reach browser transcripts as its own inspectable record.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory (or load-file-name buffer-file-name)) "helpers"))
(require 'mevedel-collaboration-projection)
(require 'mevedel-session-artifacts)
(require 'mevedel-transcript-restore)
(require 'gptel)

(mevedel-deftest mevedel-collaboration--canonical-records/thinking ()
  ,test
  (test)
  :doc "projects substantive thinking in source order, without org scaffolding"
  (with-temp-buffer
    (mevedel--transcript-org-mode)
    (insert "Question\n"
            (propertize "#+begin_reasoning\nCheck the **boundary**.\n#+end_reasoning\n"
                        'gptel 'ignore)
            (propertize "Answer." 'gptel 'response))
    (let* ((records (mevedel-collaboration--canonical-records (current-buffer)))
           (thinking (cadr records)))
      (should (equal '("user" "thinking" "assistant")
                     (mapcar (lambda (record) (plist-get record :kind)) records)))
      (should (equal "Check the **boundary**." (plist-get thinking :text)))
      (should (equal "Check the **boundary**."
                     (alist-get "text" (mevedel-collaboration--json-record thinking)
                                nil nil #'equal)))))
  :doc "empty thinking emits no card, including an empty live opener"
  (dolist (text '("#+begin_reasoning\n"
                  "#+begin_reasoning\n \n#+end_reasoning\n"))
    (with-temp-buffer
      (mevedel--transcript-org-mode)
      (insert (propertize text 'gptel 'ignore))
      (should-not (mevedel-collaboration--canonical-records (current-buffer)))))
  :doc "streaming retained projections match full ones and reuse room identities"
  (with-temp-buffer
    (mevedel--transcript-org-mode)
    (let ((cache (mevedel-collaboration--projection-cache-create)) previous)
      (dolist (chunk '("#+begin_reasoning\nFirst" " next" "\n#+end_reasoning\n"))
        (insert (propertize chunk 'gptel 'ignore))
        (let* ((full (mevedel-collaboration--canonical-records (current-buffer)))
               (cached (mevedel-collaboration--canonical-records
                        (current-buffer) nil nil cache))
               (reused (mevedel-collaboration--reuse-record-ids
                        previous (copy-tree cached))))
          (should (equal full cached))
          (ert-info ((format "Chunk %S; segments %S" chunk
                             (mevedel-transcript-segments (point-min) (point-max))))
            (should (equal "thinking" (plist-get (car full) :kind))))
          (should (= 1 (length full)))
          (when previous
            (should (equal (plist-get (car previous) :id)
                           (plist-get (car reused) :id))))
          (setq previous reused)))
      (should (equal "First next" (plist-get (car previous) :text)))))
  :doc "repeated thinking stays distinct and uses the record byte bound"
  (with-temp-buffer
    (mevedel--transcript-org-mode)
    (let ((mevedel-collaboration--max-record-text-bytes 40))
      (dotimes (_ 2)
        (insert (propertize (concat "#+begin_reasoning\n" (make-string 60 ?x)
                                    "\n#+end_reasoning\n") 'gptel 'ignore)))
      (let ((records (mevedel-collaboration--canonical-records (current-buffer))))
        (should (= 2 (length records)))
        (should-not (equal (plist-get (car records) :id)
                           (plist-get (cadr records) :id)))
        (dolist (record records)
          (should (<= (string-bytes (plist-get record :text)) 40))
          (should (string-suffix-p "[truncated]" (plist-get record :text)))))))
  :doc "native gptel streaming projects thinking before the closing marker"
  (dolist (include '(ignore t))
    (with-temp-buffer
      (mevedel--transcript-org-mode)
      (insert "Question\n")
      (let ((info (list :position (copy-marker (point)) :include-reasoning include))
            (cache (mevedel-collaboration--projection-cache-create))
            (gptel-post-stream-hook nil))
        (dolist (chunk '("First" " next" t))
          (gptel-curl--stream-insert-response
           (cons 'reasoning (if (stringp chunk) (copy-sequence chunk) chunk)) info)
          (let* ((records (mevedel-collaboration--canonical-records (current-buffer)))
                 (thinking (cadr records)))
            (ert-info ((format "Include %S; chunk %S; segments %S" include chunk
                               (mevedel-transcript-segments (point-min) (point-max))))
              (should (equal '("user" "thinking")
                             (mapcar (lambda (record) (plist-get record :kind)) records)))
              (should (equal records (mevedel-collaboration--canonical-records
                                      (current-buffer) nil nil cache)))
              (should (equal (if (equal chunk "First") "First" "First next")
                             (plist-get thinking :text)))))))))
  :doc "quoted openers in user text or hidden audit do not become thinking"
  (with-temp-buffer
    (mevedel--transcript-org-mode)
    (insert "Example:\n#+begin_reasoning\npartial\n")
    (insert (mevedel--format-hook-audit-record
             '(:type tool-context :event "PostToolUse"
                     :text "#+begin_reasoning\nprivate\n")))
    (let ((records (mevedel-collaboration--canonical-records (current-buffer))))
      (should-not (cl-find "thinking" records
                           :key (lambda (record) (plist-get record :kind))
                           :test #'equal))
      (should-not (string-match-p "private" (prin1-to-string records)))))
  :doc "native thinking resumes after an interleaved tool before the block closes"
  (dolist (configuration '((ignore "Before tool.") (t "Before tool.")
                           (ignore "") (t "")))
    (with-temp-buffer
      (mevedel--transcript-org-mode)
      (insert "Question\n")
      (let* ((include (car configuration))
             (before (cadr configuration))
             (tool (gptel--make-tool :name "Read"))
             (info (list :position (copy-marker (point))
                         :include-reasoning include :reasoning-block 'in
                         :callback #'gptel-curl--stream-insert-response
                         :tool-use '((:name "Read" :id "read-1"))))
             (cache (mevedel-collaboration--projection-cache-create))
             (gptel-include-tool-results t)
             (gptel-post-stream-hook nil))
        (gptel-curl--stream-insert-response
         (cons 'reasoning (copy-sequence before)) info)
        (gptel-curl--stream-insert-response
         (list 'tool-result (list tool '(:file_path "a.el") "contents")) info)
        (gptel-curl--stream-insert-response
         (cons 'reasoning (copy-sequence "After tool.")) info)
        (dolist (closed '(nil t))
          (when closed
            (gptel-curl--stream-insert-response '(reasoning . t) info))
          (let* ((records (mevedel-collaboration--canonical-records (current-buffer)))
                 (thinking (cl-remove-if-not
                            (lambda (record) (equal (plist-get record :kind) "thinking"))
                            records)))
            (ert-info ((format "Config %S; closed %S" configuration closed))
              (should (equal (if (string-empty-p before)
                                 '("user" "tool" "thinking")
                               '("user" "thinking" "tool" "thinking"))
                             (mapcar (lambda (record) (plist-get record :kind)) records)))
              (should (equal (if (string-empty-p before)
                                 '("After tool.")
                               '("Before tool." "After tool."))
                             (mapcar (lambda (record) (plist-get record :text)) thinking))))
            (should (equal records (mevedel-collaboration--canonical-records
                                    (current-buffer) nil nil cache))))))))
  :doc "thinking splits activity runs and never belongs to either tool fold"
  (let* ((mevedel-view-tool-group-collapse-threshold 1)
         (tool (lambda (id)
                 (list :id id :kind "tool" :status "completed"
                       :presentation (list :name "Read" :groupable t))))
         (records (mevedel-collaboration--activity-records
                   (list (funcall tool "a") (funcall tool "b")
                         (list :id "thought" :kind "thinking" :text "Consider.")
                         (funcall tool "c") (funcall tool "d")))))
    (should-not (plist-get (nth 2 records) :activity))
    (should (equal '("a" "a" "c" "c")
                   (mapcar (lambda (record)
                             (plist-get (plist-get record :activity) :id))
                           (list (nth 0 records) (nth 1 records)
                                 (nth 3 records) (nth 4 records))))))
  :doc "saved transcripts restore thinking with the same text and identity"
  (let (saved live)
    (with-temp-buffer
      (delay-mode-hooks (org-mode))
      (mevedel-transcript-enable-gptel-mode)
      (insert ":PROPERTIES:\n:GPTEL_BOUNDS: nil\n:END:\n\nQuestion\n"
              (propertize "#+begin_reasoning\nConsider.\n#+end_reasoning\n"
                          'gptel 'ignore)
              (propertize "Answer.\n" 'gptel 'response))
      (mevedel-session-artifacts-stabilize-gptel-bounds)
      (setq live (mevedel-collaboration--canonical-records (current-buffer))
            saved (buffer-substring-no-properties (point-min) (point-max))))
    (with-temp-buffer
      (insert saved)
      (delay-mode-hooks (org-mode))
      (mevedel-transcript-restore-properties)
      (let ((restored (mevedel-collaboration--canonical-records (current-buffer))))
        (should (equal '("user" "thinking" "assistant")
                       (mapcar (lambda (record) (plist-get record :kind)) restored)))
        (should (equal live restored))))))

(provide 'test-mevedel-collaboration-projection-thinking)
;;; test-mevedel-collaboration-projection-thinking.el ends here
