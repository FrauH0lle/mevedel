;;; test-niwa-collaboration-projection-compaction.el --- Browser compaction -*- lexical-binding: t; -*-

;;; Commentary:
;; A compacted segment reaches browser transcripts as one summary record.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'niwa-collaboration-projection)
(require 'niwa-session-artifacts)
(require 'niwa-transcript-restore)
(require 'gptel)

(defun niwa-test--compaction-kinds (records)
  "Return the record kinds of RECORDS in order."
  (mapcar (lambda (record) (plist-get record :kind)) records))

(niwa-deftest niwa-collaboration--canonical-records/compaction ()
  ,test
  (test)
  :doc "projects the model-visible summary first, without wrapper lines"
  (with-temp-buffer
    (niwa--transcript-org-mode)
    (insert (niwa-session-artifacts-summary-block "Private handoff"))
    (insert "\n" (propertize "Answer." 'gptel 'response))
    (let* ((records (niwa-collaboration--canonical-records (current-buffer)))
           (summary (car records)))
      (should (equal "compaction" (plist-get summary :kind)))
      (should (string-search "Another language model started"
                             (plist-get summary :text)))
      (should (string-suffix-p "Private handoff" (plist-get summary :text)))
      (should-not (string-match-p "begin_summary\\|end_summary"
                                  (plist-get summary :text)))
      (should (equal "assistant" (plist-get (cadr records) :kind)))
      (should (equal (plist-get summary :text)
                     (alist-get "text" (niwa-collaboration--json-record summary)
                                nil nil #'equal)))))
  :doc "an uncompacted transcript projects no compaction record"
  (with-temp-buffer
    (niwa--transcript-org-mode)
    (insert "Question\n" (propertize "Answer." 'gptel 'response))
    (should-not (member "compaction"
                        (niwa-test--compaction-kinds
                         (niwa-collaboration--canonical-records
                          (current-buffer))))))
  :doc "the identity is stable across projections and cache use"
  (with-temp-buffer
    (niwa--transcript-org-mode)
    (insert (niwa-session-artifacts-summary-block "Private handoff")
            "\n" (propertize "Answer." 'gptel 'response))
    (let* ((cache (niwa-collaboration--projection-cache-create))
           (full (niwa-collaboration--canonical-records (current-buffer)))
           (again (niwa-collaboration--canonical-records (current-buffer)))
           (cached (niwa-collaboration--canonical-records
                    (current-buffer) nil nil cache))
           (retained (niwa-collaboration--canonical-records
                      (current-buffer) nil nil cache)))
      (should (equal full again))
      (should (equal full cached))
      (should (equal full retained))
      (should (= 1 (cl-count "compaction" (niwa-test--compaction-kinds retained)
                             :test #'equal)))))
  :doc "a different summary gets a different identity"
  (let (ids)
    (dolist (summary '("First handoff" "Second handoff"))
      (with-temp-buffer
        (niwa--transcript-org-mode)
        (insert (niwa-session-artifacts-summary-block summary))
        (push (plist-get (car (niwa-collaboration--canonical-records
                               (current-buffer)))
                         :id)
              ids)))
    (should-not (equal (car ids) (cadr ids))))
  :doc "the record uses the record byte bound"
  (with-temp-buffer
    (niwa--transcript-org-mode)
    (let ((niwa-collaboration--max-record-text-bytes 40))
      (insert (niwa-session-artifacts-summary-block (make-string 200 ?x)))
      (let ((summary (car (niwa-collaboration--canonical-records
                           (current-buffer)))))
        (should (<= (string-bytes (plist-get summary :text)) 40))
        (should (string-suffix-p "[truncated]" (plist-get summary :text))))))
  :doc "a literal example in ordinary text is not a summary"
  (with-temp-buffer
    (niwa--transcript-org-mode)
    (insert "Look:\n#+begin_summary niwa-role=compaction-summary\nx\n#+end_summary\n"
            (propertize "Answer." 'gptel 'response))
    (should-not (member "compaction"
                        (niwa-test--compaction-kinds
                         (niwa-collaboration--canonical-records
                          (current-buffer))))))
  :doc "an agent's summary behind its task anchor projects only for an owner path"
  (with-temp-buffer
    (niwa--transcript-org-mode)
    (insert "Task anchor.\n"
            (niwa-session-artifacts-summary-block "Agent handoff")
            "\n" (propertize "Answer." 'gptel 'response))
    (should-not (member "compaction"
                        (niwa-test--compaction-kinds
                         (niwa-collaboration--canonical-records
                          (current-buffer)))))
    (let ((records (niwa-collaboration--canonical-records
                    (current-buffer) nil nil nil "/root/explorer")))
      (should (= 1 (cl-count "compaction" (niwa-test--compaction-kinds records)
                             :test #'equal)))
      (should (string-suffix-p "Agent handoff"
                               (plist-get
                                (cl-find "compaction" records
                                         :key (lambda (r) (plist-get r :kind))
                                         :test #'equal)
                                :text))))))

(provide 'test-niwa-collaboration-projection-compaction)
;;; test-niwa-collaboration-projection-compaction.el ends here
