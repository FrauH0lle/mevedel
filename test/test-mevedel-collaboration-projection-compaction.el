;;; test-mevedel-collaboration-projection-compaction.el --- Browser compaction -*- lexical-binding: t; -*-

;;; Commentary:
;; A compacted segment reaches browser transcripts as one summary record.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-collaboration-projection)
(require 'mevedel-session-artifacts)
(require 'mevedel-transcript-restore)
(require 'gptel)

(defun mevedel-test--compaction-kinds (records)
  "Return the record kinds of RECORDS in order."
  (mapcar (lambda (record) (plist-get record :kind)) records))

(mevedel-deftest mevedel-collaboration--canonical-records/compaction ()
  ,test
  (test)
  :doc "projects the model-visible summary first, without wrapper lines"
  (with-temp-buffer
    (mevedel--transcript-org-mode)
    (insert (mevedel-session-artifacts-summary-block "Private handoff"))
    (insert "\n" (propertize "Answer." 'gptel 'response))
    (let* ((records (mevedel-collaboration--canonical-records (current-buffer)))
           (summary (car records)))
      (should (equal "compaction" (plist-get summary :kind)))
      (should (string-search "Another language model started"
                             (plist-get summary :text)))
      (should (string-suffix-p "Private handoff" (plist-get summary :text)))
      (should-not (string-match-p "begin_summary\\|end_summary"
                                  (plist-get summary :text)))
      (should (equal "assistant" (plist-get (cadr records) :kind)))
      (should (equal (plist-get summary :text)
                     (alist-get "text" (mevedel-collaboration--json-record summary)
                                nil nil #'equal)))))
  :doc "an uncompacted transcript projects no compaction record"
  (with-temp-buffer
    (mevedel--transcript-org-mode)
    (insert "Question\n" (propertize "Answer." 'gptel 'response))
    (should-not (member "compaction"
                        (mevedel-test--compaction-kinds
                         (mevedel-collaboration--canonical-records
                          (current-buffer))))))
  :doc "the identity is stable across projections and cache use"
  (with-temp-buffer
    (mevedel--transcript-org-mode)
    (insert (mevedel-session-artifacts-summary-block "Private handoff")
            "\n" (propertize "Answer." 'gptel 'response))
    (let* ((cache (mevedel-collaboration--projection-cache-create))
           (full (mevedel-collaboration--canonical-records (current-buffer)))
           (again (mevedel-collaboration--canonical-records (current-buffer)))
           (cached (mevedel-collaboration--canonical-records
                    (current-buffer) nil nil cache))
           (retained (mevedel-collaboration--canonical-records
                      (current-buffer) nil nil cache)))
      (should (equal full again))
      (should (equal full cached))
      (should (equal full retained))
      (should (= 1 (cl-count "compaction" (mevedel-test--compaction-kinds retained)
                             :test #'equal)))))
  :doc "a different summary gets a different identity"
  (let (ids)
    (dolist (summary '("First handoff" "Second handoff"))
      (with-temp-buffer
        (mevedel--transcript-org-mode)
        (insert (mevedel-session-artifacts-summary-block summary))
        (push (plist-get (car (mevedel-collaboration--canonical-records
                               (current-buffer)))
                         :id)
              ids)))
    (should-not (equal (car ids) (cadr ids))))
  :doc "the record uses the record byte bound"
  (with-temp-buffer
    (mevedel--transcript-org-mode)
    (let ((mevedel-collaboration--max-record-text-bytes 40))
      (insert (mevedel-session-artifacts-summary-block (make-string 200 ?x)))
      (let ((summary (car (mevedel-collaboration--canonical-records
                           (current-buffer)))))
        (should (<= (string-bytes (plist-get summary :text)) 40))
        (should (string-suffix-p "[truncated]" (plist-get summary :text))))))
  :doc "a literal example in ordinary text is not a summary"
  (with-temp-buffer
    (mevedel--transcript-org-mode)
    (insert "Look:\n#+begin_summary mevedel-role=compaction-summary\nx\n#+end_summary\n"
            (propertize "Answer." 'gptel 'response))
    (should-not (member "compaction"
                        (mevedel-test--compaction-kinds
                         (mevedel-collaboration--canonical-records
                          (current-buffer))))))
  :doc "an agent's summary behind its task anchor projects only for an owner path"
  (with-temp-buffer
    (mevedel--transcript-org-mode)
    (insert "Task anchor.\n"
            (mevedel-session-artifacts-summary-block "Agent handoff")
            "\n" (propertize "Answer." 'gptel 'response))
    (should-not (member "compaction"
                        (mevedel-test--compaction-kinds
                         (mevedel-collaboration--canonical-records
                          (current-buffer)))))
    (let ((records (mevedel-collaboration--canonical-records
                    (current-buffer) nil nil nil "/root/explorer")))
      (should (= 1 (cl-count "compaction" (mevedel-test--compaction-kinds records)
                             :test #'equal)))
      (should (string-suffix-p "Agent handoff"
                               (plist-get
                                (cl-find "compaction" records
                                         :key (lambda (r) (plist-get r :kind))
                                         :test #'equal)
                                :text))))))

(provide 'test-mevedel-collaboration-projection-compaction)
;;; test-mevedel-collaboration-projection-compaction.el ends here
