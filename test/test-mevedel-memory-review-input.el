;;; test-mevedel-memory-review-input.el -- Compact review evidence -*- lexical-binding: t -*-

;;; Commentary:

;; Provider-only reference grouping preserves every dated path observation.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-memory-review)

(mevedel-deftest mevedel-memory-review--reference-groups
    (:vars ((references
             '((:topic "root/a.md" :scope "path existence only" :checked "2026-10-28"
                :token "a.el" :result "found")
               (:topic "root/b.md" :scope "path existence only" :checked "2026-10-28"
                :token "unknown-symbol" :result "unknown")
               (:topic "root/a.md" :scope "path existence only" :checked "2026-10-28"
                :token "missing.el" :result "not found")
               (:topic "root/a.md" :scope "different check" :checked "2026-10-28"
                :token "a.el" :result "unknown")
               (:topic "root/a.md" :scope "path existence only" :checked "2026-10-27"
                :token "a.el" :result "not found")))))
  ,test
  (test)
  :doc "shares metadata only for equal topic, scope and timestamp, retaining all tokens and results"
  (let* ((before (copy-tree references))
         (groups (mevedel-memory-review--reference-groups references))
         (first (car groups)))
    (should (= 4 (length groups)))
    (should (equal "root/a.md" (plist-get first :topic)))
    (should (equal "path existence only" (plist-get first :scope)))
    (should (equal "2026-10-28" (plist-get first :checked)))
    (should (equal [(:token "a.el" :result "found")
                    (:token "missing.el" :result "not found")]
                   (plist-get first :checks)))
    (should (equal "root/b.md" (plist-get (nth 1 groups) :topic)))
    (should (equal "different check" (plist-get (nth 2 groups) :scope)))
    (should (equal "2026-10-27" (plist-get (nth 3 groups) :checked)))
    (should (equal before references)))
  :doc "empty observations stay empty"
  (should-not (mevedel-memory-review--reference-groups nil)))

(mevedel-deftest mevedel-memory-review--input
    (:vars ((references
             '(:references ((:topic "root/topic.md" :scope "path existence only"
                             :checked "2026-10-28T12:00:00Z" :token "code.el" :result "found")
                            (:topic "root/topic.md" :scope "path existence only"
                             :checked "2026-10-28T12:00:00Z" :token "unknown" :result "unknown"))
               :omissions ("Reference count limit reached")))))
  ,test
  (test)
  :doc "provider JSON contains complete grouped observations and omission warnings"
  (let* ((input (json-parse-string
                 (mevedel-memory-review--input nil nil nil references nil nil 0)
                 :object-type 'plist :array-type 'list))
         (groups (plist-get input :reference_checks)))
    (should (= 1 (length groups)))
    (should (= 2 (length (plist-get (car groups) :checks))))
    (should (equal "unknown" (plist-get (cadr (plist-get (car groups) :checks)) :result)))
    (should (equal '("Reference count limit reached") (plist-get input :reference_omissions))))
  :doc "chunk summaries are ordinary note evidence without a model-policy flag"
  (let* ((body "Dated candidate lesson; remaining uncertainty is not a task tracker.")
         (note (list :id "work://shared/large.md" :path "large.md" :chunked t
                     :modified "2026-10-29T00:00:00Z" :cited t :body body))
         (input (json-parse-string
                 (mevedel-memory-review--input nil nil nil nil nil nil 0 (list note))
                 :object-type 'plist :array-type 'list))
         (admitted (car (plist-get input :notes))))
    (should (equal body (plist-get admitted :body)))
    (should (eq t (plist-get admitted :cited_by_memory)))
    (should-not (plist-member admitted :sequential_summary))))

(provide 'test-mevedel-memory-review-input)
;;; test-mevedel-memory-review-input.el ends here
