;;; test-mevedel-view-render-directive-batch.el --- Directive projection folds -*- lexical-binding: t -*-

;;; Commentary:
;; Temporary placeholders must not become evidence of a later settled turn.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-view)
(require 'mevedel-view-render)
(require 'mevedel-transcript-audit)

(mevedel-deftest mevedel-view-render/directive-batch-preserves-newest
  (:doc "Deferred metadata placeholders never collapse the newest directive.")
  (mevedel-view-test--with-buffers
    (with-current-buffer data-buf
      (dolist (ordinal '(7 8))
        (insert (mevedel--format-hook-audit-record
                 (list :type 'directive-turn-boundary :edge 'start
                       :directive-id "batch-directive" :action 'discuss :turn ordinal))
                (format "Question %d :discuss:\n:PROMPT:\nFull directive request\n:END:\n" ordinal)
                (propertize (format "Answer %d.\n" ordinal) 'gptel 'response)
                (mevedel--format-hook-audit-record
                 (list :type 'directive-turn-boundary :edge 'end
                       :directive-id "batch-directive" :action 'discuss :turn ordinal
                       :outcome 'success :activity-kind 'discussion :sequence 1)))))
    (with-current-buffer view-buf
      (mevedel-view--full-rerender)
      (should-not (string-search "Answer 7." (buffer-string)))
      (should (string-search "Answer 8." (buffer-string)))
      (goto-char (point-min))
      (search-forward "T7")
      (mevedel-view-toggle-section)
      (mevedel-view--full-rerender)
      (let ((expected (buffer-substring-no-properties (point-min) (point-max))))
        (mevedel-view-render-batched-full)
        (cl-loop repeat 150 while mevedel-view-render--batch do
                 (mevedel-view-render--batch-step view-buf mevedel-view-render--batch))
        (should-not mevedel-view-render--batch)
        (should (equal expected (buffer-substring-no-properties (point-min) (point-max))))
        (should-not (gethash '("batch-directive" 8)
                             mevedel-view--directive-collapse-states))))))

(provide 'test-mevedel-view-render-directive-batch)
;;; test-mevedel-view-render-directive-batch.el ends here
