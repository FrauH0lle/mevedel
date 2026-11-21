;;; test-niwa-transcript-cache.el --- Projection cache contracts -*- lexical-binding: t -*-
;;; Commentary:
;; Caches retain pure decoding separately from current provenance and geometry.
;;; Code:
(require 'helpers (file-name-concat (file-name-directory (or buffer-file-name load-file-name byte-compile-current-file)) "helpers"))
(require 'niwa-transcript)
(require 'niwa-transcript-audit)

(niwa-deftest niwa--read-hook-audit-record ()
                 ,test
                 (test)
                 :doc "reuses valid and invalid decoding, without granting provenance to raw text"
                 (with-temp-buffer
                   (let* ((record '(:type tool-context :event "PostToolUse"))
                          (text (niwa--hook-audit-record-payload record))
                          (decode (symbol-function 'niwa-transcript-audit--decode))
                          (calls 0))
                     (cl-letf (((symbol-function 'niwa-transcript-audit--decode)
                                (lambda (value) (cl-incf calls) (funcall decode value))))
                              (let ((niwa-transcript-audit--decode-cache (make-hash-table :test #'equal)))
                                (dotimes (_ 2)
                                  (should (equal record (niwa--read-hook-audit-record text)))
                                  (should-not (niwa--read-hook-audit-record "bad base64")))
                                (should (= calls 2))
                                (let* ((block (niwa--format-hook-audit-record record))
                                       (plain (substring-no-properties block)))
                                  (should (niwa-transcript-audit-spans block))
                                  (should-not (niwa-transcript-audit-spans plain))))
                              (let ((before calls))
                                (dotimes (_ 2) (should (equal record (niwa--read-hook-audit-record text))))
                                (should (= calls before))))))
                 :doc "reuses bounded decoding across projections while trust and changed bytes stay current"
                 (with-temp-buffer
                   (let* ((block (niwa--format-hook-audit-record '(:type fork-point :fork-point-id "a")))
                          (decode (symbol-function 'niwa-transcript-audit--decode))
                          (calls 0))
                     (cl-letf (((symbol-function 'niwa-transcript-audit--decode)
                                (lambda (text) (cl-incf calls) (funcall decode text))))
                              (dotimes (_ 3)
                                (let ((niwa-transcript-audit--decode-cache (make-hash-table :test #'equal)))
                                  (should (equal "a" (plist-get (plist-get (car (niwa-transcript-audit-spans block)) :record) :fork-point-id)))))
                              (should (= calls 1))
                              (should-not (niwa-transcript-audit-spans (substring-no-properties block)))
                              (setq block (niwa--format-hook-audit-record '(:type fork-point :fork-point-id "b")))
                              (should (equal "b" (plist-get (plist-get (car (niwa-transcript-audit-spans block)) :record) :fork-point-id)))
                              (should (= calls 2))
                              (dotimes (n 1050)
                                (niwa--read-hook-audit-record (niwa--hook-audit-record-payload (list :type 'tool-context :number n))))
                              (should (<= (hash-table-count niwa-transcript-audit--buffer-records) 1024))
                              (should (<= niwa-transcript-audit--buffer-record-bytes (* 4 1024 1024)))
                              (let ((large (make-string (1+ (* 1024 1024)) ?x)))
                                (niwa--read-hook-audit-record large)
                                (should-not (gethash large niwa-transcript-audit--buffer-records)))))))

(niwa-deftest niwa-transcript--tool-block-bounds-for-run ()
                 ,test
                 (test)
                 :doc "a missing block does not rescan previously indexed history"
                 (with-temp-buffer
                   (org-mode)
                   (dotimes (i 8)
                     (insert "#+begin_tool\n"
                             (propertize
                              (format "(:name \"Read\" :args nil)\nresult %d\n" i)
                              'gptel (cons 'tool (format "call-%d" i)))
                             "#+end_tool\n\n"))
                   (let ((beg (point))
                         (niwa-transcript--tool-block-index
                          (make-hash-table :test #'eq)))
                     (insert (propertize "unwrapped result\n" 'gptel '(tool . "last")))
                     ;; Build the canonical index before observing recovery work.
                     (should-not (niwa-transcript--tool-block-bounds-for-run
                                  beg (point-max)))
                     (let ((scans 0)
                           (scan (symbol-function
                                  'niwa-transcript--tool-block-end-from-start)))
                       (goto-char (point-min))
                       (cl-letf (((symbol-function 'niwa-transcript--tool-block-end-from-start)
                                  (lambda (&rest args)
                                    (cl-incf scans)
                                    (apply scan args))))
                         (should-not (niwa-transcript--tool-block-bounds-for-run
                                      beg (point-max))))
                       (should (= scans 0))
                       (should (= (point) (point-min))))))
                 :doc "indexed bounds agree with anchored recovery, including nested marker text and limits"
                 (dolist (body '("result\n"
                                 "#+begin_tool\n(:name \"Fake\" :args nil)\nquoted\n#+end_tool\nresult\n"
                                 "#+end_tool\n\ncontinued result\n"))
                   (with-temp-buffer
                     (org-mode)
                     (insert "#+begin_tool\n(:name \"Read\" :args (:path \"x\"))\n")
                     (let ((beg (point)))
                       (insert (propertize body 'gptel '(tool . "one")))
                       (let ((end (point)))
                         (insert "#+end_tool\n")
                         (dolist (limit (list nil (point-max) (1- end)))
                           (let ((expected (niwa-transcript--tool-block-bounds-for-run beg end limit))
                                 (niwa-transcript--tool-block-index (make-hash-table :test #'eq)))
                             (dotimes (_ 2)
                               (should (equal expected (niwa-transcript--tool-block-bounds-for-run beg end limit))))))))))

                 :doc "text edits, property edits, narrowing and different buffers invalidate cached geometry"
                 (let ((niwa-transcript--tool-block-index (make-hash-table :test #'eq)))
                   (dotimes (_ 2)
                     (with-temp-buffer
                       (org-mode)
                       (insert "prefix\n#+begin_tool\n(:name \"Read\" :args nil)\n")
                       (let ((beg (point)))
                         (insert (propertize "result\n" 'gptel '(tool . "one")))
                         (let ((end (point)))
                           (insert "#+end_tool\n")
                           (niwa-transcript--tool-block-bounds-for-run beg end)
                           (let ((first (gethash (current-buffer) niwa-transcript--tool-block-index)))
                             (niwa-transcript--tool-block-bounds-for-run beg end)
                             (should (eq first (gethash (current-buffer) niwa-transcript--tool-block-index)))
                             (put-text-property beg end 'test-property t)
                             (niwa-transcript--tool-block-bounds-for-run beg end)
                             (should-not (eq first (gethash (current-buffer) niwa-transcript--tool-block-index))))
                           (goto-char (point-min))
                           (insert "more\n")
                           (setq beg (+ beg 5) end (+ end 5))
                           (should (equal (cons 13 (point-max))
                                          (niwa-transcript--tool-block-bounds-for-run beg end)))
                           (save-restriction
                             (narrow-to-region beg (point-max))
                             (should-not (niwa-transcript--tool-block-bounds-for-run beg end)))))))))

(niwa-deftest niwa-transcript--control-line-positions
  (:doc "indexes candidate control lines within the accessible buffer")
  (with-temp-buffer
    (insert "ordinary\n# heading\n#+BEGIN_TOOL\n(:name \"Read\")\n#+END_TOOL\n"
            "<!-- niwa-render-data -->\n:END:\n<agent-result>\n")
    (let ((case-fold-search t))
      (should (= 5 (length (niwa-transcript--control-line-positions))))
      (goto-char (point-min))
      (search-forward "#+END_TOOL")
      (beginning-of-line)
      (narrow-to-region (point) (point-max))
      (should (= 4 (length (niwa-transcript--control-line-positions)))))))

(niwa-deftest niwa-transcript--search-control-line
  (:doc "indexed search agrees with native matching, including partial line limits")
  (with-temp-buffer
    (insert "prose\n#+begin_tool\nbody\n#+end_tool suffix\n\n:END:\n")
    (dolist (regexp '("^#\\+begin_tool\\b" "^#\\+end_tool[^\n]*\n?" "^:END:[ \t]*\n?" "^<hook-context>"))
      (dolist (limit (list (point-max) 37 32 20))
        (dolist (start '(1 8 21))
          (when (<= start limit)
            (goto-char start)
            (let ((expected (re-search-forward regexp limit t))
                  (expected-point (point))
                  (expected-match (match-data))
                  (niwa-transcript--control-lines
                   (niwa-transcript--control-line-positions)))
              (goto-char start)
              (should (equal expected
                             (niwa-transcript--search-control-line regexp limit)))
              (should (= expected-point (point)))
              (when expected (should (equal expected-match (match-data)))))))))))

(provide 'test-niwa-transcript-cache)
;;; test-niwa-transcript-cache.el ends here
