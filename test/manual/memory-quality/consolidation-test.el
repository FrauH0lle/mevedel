;;; consolidation-test.el -- Proposal evaluation harness checks -*- lexical-binding: t -*-

;;; Commentary:

;; Check fixture admission and collection without using a model service.

;;; Code:

(load (file-name-concat (file-name-directory load-file-name) "consolidation.el") nil t)
(ert-delete-test 'mevedel-memory-quality-consolidation-real-provider)

(mevedel-deftest mevedel-quality--consolidate ()
  ,test
  (test)
  :doc "manual and auto fixtures admit only the digest result and preserve empty-journal cases"
  (let* ((directory (make-temp-file "mevedel-quality-digests-" t))
         (process-environment (cons (concat "MEVEDEL_QUALITY_DIGESTS=" directory) process-environment))
         (gptel--known-backends nil)
         (backend (gptel-make-openai "quality-harness" :key "test-only" :models '(test-model)))
         (digest "## Done\n- Observed: Corrected the production database.\n## Learned\n- User correction: PostgreSQL 16 is production.\n## Surprised\n- none\n## Unfinished\n- none")
         (none "## Promote\n- none\n## Update\n- none\n## Merge\n- none\n## Remove\n- none\n## Instructions\n- none\n## No action\n- No supported changes."))
    (unwind-protect
        (progn
          (write-region (concat "# Metadata\nReview oracle MUST NOT ENTER INPUT\n\n## Model result\n\n" digest
                                "\n\n## Retrieval\nPrivate review notes") nil
                        (file-name-concat directory "user-correction.md") nil 'silent)
          (cl-letf (((symbol-function 'mevedel-memory-review-request)
                     (lambda (_scope entries callback &rest _)
                       (funcall callback (list :outcome 'success :entries entries :reply none
                                               :policy (list :backend backend :model 'test-model)))
                       (list :cancel #'ignore))))
            (dolist (mode '(manual auto))
              (dolist (case (list (car (mevedel-quality--cases))
                                 (seq-find (lambda (row) (equal (plist-get row :name) "journal-missing"))
                                           (mevedel-quality--cases))))
                (let ((result (mevedel-quality--consolidate case mode)))
                  (should (eq 'success (plist-get result :outcome)))
                  (should (equal none (plist-get result :reply)))
                  (should (equal (unless (plist-get case :purpose) digest) (plist-get result :digest)))
                  (should (= 2 (length (plist-get result :after))))
                  (should (equal "Never deploy. Never log raw credentials.\n" (plist-get result :instructions))))))))
      (delete-directory directory t))))

(provide 'mevedel-memory-quality-consolidation-test)
;;; consolidation-test.el ends here
