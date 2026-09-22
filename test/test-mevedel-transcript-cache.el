;;; test-mevedel-transcript-cache.el --- Projection cache contracts -*- lexical-binding: t -*-
;;; Commentary:
;; Caches retain pure decoding separately from current provenance and geometry.
;;; Code:
(require 'helpers (file-name-concat (file-name-directory (or buffer-file-name load-file-name byte-compile-current-file)) "helpers"))
(require 'mevedel-transcript)
(require 'mevedel-transcript-audit)

(mevedel-deftest mevedel--read-hook-audit-record ()
                 ,test
                 (test)
                 :doc "reuses valid and invalid decoding, without granting provenance to raw text"
                 (with-temp-buffer
                   (let* ((record '(:type tool-context :event "PostToolUse"))
                          (text (mevedel--hook-audit-record-payload record))
                          (decode (symbol-function 'mevedel-transcript-audit--decode))
                          (calls 0))
                     (cl-letf (((symbol-function 'mevedel-transcript-audit--decode)
                                (lambda (value) (cl-incf calls) (funcall decode value))))
                              (let ((mevedel-transcript-audit--decode-cache (make-hash-table :test #'equal)))
                                (dotimes (_ 2)
                                  (should (equal record (mevedel--read-hook-audit-record text)))
                                  (should-not (mevedel--read-hook-audit-record "bad base64")))
                                (should (= calls 2))
                                (let* ((block (mevedel--format-hook-audit-record record))
                                       (plain (substring-no-properties block)))
                                  (should (mevedel-transcript-audit-spans block))
                                  (should-not (mevedel-transcript-audit-spans plain))))
                              (let ((before calls))
                                (dotimes (_ 2) (should (equal record (mevedel--read-hook-audit-record text))))
                                (should (= calls before))))))
                 :doc "reuses bounded decoding across projections while trust and changed bytes stay current"
                 (with-temp-buffer
                   (let* ((block (mevedel--format-hook-audit-record '(:type fork-point :fork-point-id "a")))
                          (decode (symbol-function 'mevedel-transcript-audit--decode))
                          (calls 0))
                     (cl-letf (((symbol-function 'mevedel-transcript-audit--decode)
                                (lambda (text) (cl-incf calls) (funcall decode text))))
                              (dotimes (_ 3)
                                (let ((mevedel-transcript-audit--decode-cache (make-hash-table :test #'equal)))
                                  (should (equal "a" (plist-get (plist-get (car (mevedel-transcript-audit-spans block)) :record) :fork-point-id)))))
                              (should (= calls 1))
                              (should-not (mevedel-transcript-audit-spans (substring-no-properties block)))
                              (setq block (mevedel--format-hook-audit-record '(:type fork-point :fork-point-id "b")))
                              (should (equal "b" (plist-get (plist-get (car (mevedel-transcript-audit-spans block)) :record) :fork-point-id)))
                              (should (= calls 2))
                              (dotimes (n 140)
                                (mevedel--read-hook-audit-record (mevedel--hook-audit-record-payload (list :type 'tool-context :number n))))
                              (should (<= (hash-table-count mevedel-transcript-audit--buffer-records) 128))
                              (should (<= mevedel-transcript-audit--buffer-record-bytes (* 4 1024 1024)))
                              (let ((large (make-string (1+ (* 1024 1024)) ?x)))
                                (mevedel--read-hook-audit-record large)
                                (should-not (gethash large mevedel-transcript-audit--buffer-records)))))))

(mevedel-deftest mevedel-transcript--tool-block-bounds-for-run ()
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
                         (mevedel-transcript--tool-block-index
                          (make-hash-table :test #'eq)))
                     (insert (propertize "unwrapped result\n" 'gptel '(tool . "last")))
                     ;; Build the canonical index before observing recovery work.
                     (should-not (mevedel-transcript--tool-block-bounds-for-run
                                  beg (point-max)))
                     (let ((scans 0)
                           (scan (symbol-function
                                  'mevedel-transcript--tool-block-end-from-start)))
                       (goto-char (point-min))
                       (cl-letf (((symbol-function 'mevedel-transcript--tool-block-end-from-start)
                                  (lambda (&rest args)
                                    (cl-incf scans)
                                    (apply scan args))))
                         (should-not (mevedel-transcript--tool-block-bounds-for-run
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
                           (let ((expected (mevedel-transcript--tool-block-bounds-for-run beg end limit))
                                 (mevedel-transcript--tool-block-index (make-hash-table :test #'eq)))
                             (dotimes (_ 2)
                               (should (equal expected (mevedel-transcript--tool-block-bounds-for-run beg end limit))))))))))

                 :doc "text edits, property edits, narrowing and different buffers invalidate cached geometry"
                 (let ((mevedel-transcript--tool-block-index (make-hash-table :test #'eq)))
                   (dotimes (_ 2)
                     (with-temp-buffer
                       (org-mode)
                       (insert "prefix\n#+begin_tool\n(:name \"Read\" :args nil)\n")
                       (let ((beg (point)))
                         (insert (propertize "result\n" 'gptel '(tool . "one")))
                         (let ((end (point)))
                           (insert "#+end_tool\n")
                           (mevedel-transcript--tool-block-bounds-for-run beg end)
                           (let ((first (gethash (current-buffer) mevedel-transcript--tool-block-index)))
                             (mevedel-transcript--tool-block-bounds-for-run beg end)
                             (should (eq first (gethash (current-buffer) mevedel-transcript--tool-block-index)))
                             (put-text-property beg end 'test-property t)
                             (mevedel-transcript--tool-block-bounds-for-run beg end)
                             (should-not (eq first (gethash (current-buffer) mevedel-transcript--tool-block-index))))
                           (goto-char (point-min))
                           (insert "more\n")
                           (setq beg (+ beg 5) end (+ end 5))
                           (should (equal (cons 13 (point-max))
                                          (mevedel-transcript--tool-block-bounds-for-run beg end)))
                           (save-restriction
                             (narrow-to-region beg (point-max))
                             (should-not (mevedel-transcript--tool-block-bounds-for-run beg end)))))))))

(provide 'test-mevedel-transcript-cache)
;;; test-mevedel-transcript-cache.el ends here

(mevedel-deftest mevedel-transcript--control-line-positions
  (:doc "indexes candidate control lines within the accessible buffer")
  (with-temp-buffer
    (insert "ordinary\n# heading\n#+BEGIN_TOOL\n(:name \"Read\")\n#+END_TOOL\n"
            "<!-- mevedel-render-data -->\n:END:\n<agent-result>\n")
    (let ((case-fold-search t))
      (should (= 5 (length (mevedel-transcript--control-line-positions))))
      (goto-char (point-min))
      (search-forward "#+END_TOOL")
      (beginning-of-line)
      (narrow-to-region (point) (point-max))
      (should (= 4 (length (mevedel-transcript--control-line-positions)))))))

(mevedel-deftest mevedel-transcript--search-control-line
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
                  (mevedel-transcript--control-lines
                   (mevedel-transcript--control-line-positions)))
              (goto-char start)
              (should (equal expected
                             (mevedel-transcript--search-control-line regexp limit)))
              (should (= expected-point (point)))
              (when expected (should (equal expected-match (match-data)))))))))))
