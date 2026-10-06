;;; test-mevedel-collaboration-projection.el --- Projection cost contracts -*- lexical-binding: t -*-

;;; Commentary:
;; Growing transcripts must not repeatedly recover every historical tool block.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory (or load-file-name buffer-file-name)) "helpers"))
(require 'mevedel-collaboration-projection)

(mevedel-deftest mevedel-collaboration--canonical-records/bounded-history
  (:doc "stream updates preserve tool records with linear boundary searches")
  (with-temp-buffer
    (mevedel--transcript-org-mode)
    (let ((count 32)
          (scan (symbol-function 'mevedel-transcript--tool-block-end-from-start)))
      (dotimes (index count)
        (insert "#+begin_tool\n"
                (propertize
                 (format "(:name \"Read\" :args (:file_path \"/tmp/fixture\"))\nresult %d\n" index)
                 'gptel (cons 'tool (format "call-%d" index)))
                "#+end_tool\n\n"))
      (dotimes (_ 2)
        (goto-char (point-max))
        (insert (propertize "Another streamed sentence.\n" 'gptel 'response))
        (let ((scans 0) records)
          (cl-letf (((symbol-function 'mevedel-transcript--tool-block-end-from-start)
                     (lambda (&rest args)
                       (cl-incf scans)
                       (apply scan args))))
            (setq records (mevedel-collaboration--canonical-records (current-buffer))))
          (let ((tools (seq-filter (lambda (r) (equal "tool" (plist-get r :kind))) records)))
            (should (= count (length tools)))
            (dotimes (index count)
              (should (equal (format "result %d" index)
                             (plist-get (nth index tools) :result)))))
          (should (<= scans (* 4 count))))))))

(provide 'test-mevedel-collaboration-projection)
;;; test-mevedel-collaboration-projection.el ends here
