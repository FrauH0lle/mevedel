;;; test-mevedel-memory-pass-clients.el -- Independent consolidation clients -*- lexical-binding: t -*-

;;; Commentary:

;; Exercise the coordinator across two Emacs processes sharing one workspace.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory (or buffer-file-name load-file-name byte-compile-current-file)) "helpers"))
(require 'mevedel-memory-pass)
(require 'mevedel-system)

(mevedel-deftest mevedel-memory-pass-start/clients ()
  ,test
  (test)
  :doc "a live independent pass excludes a contender and its completion postpones the next automatic pass"
  (let* ((root (make-temp-file "mevedel-memory-clients-" t))
         (workspace (mevedel-workspace--create :root root))
         (identity (mevedel-workspace-identity-ensure root))
         (mevedel-memory-dirs nil)
         (mevedel-memory-consolidation-mode 'propose)
         (mevedel-memory-consolidation-min-hours 24)
         (mevedel-memory-consolidation-min-digests 5)
         (mevedel-memory-pass--inhibit-scheduling nil)
         (ready (file-name-concat root "ready"))
         (release (file-name-concat root "release"))
         (runner (file-name-concat root "holder.el"))
         (output (generate-new-buffer " *memory-pass-holder*"))
         (deadline (+ (float-time) 60)) process)
    (unwind-protect
        (progn
          (dotimes (n 5)
            (mevedel-journal-store-publish-digest
             root (list :capture-id (format "%064x" (1+ n)) :session "closed" :session-name "Closed"
                        :workspace identity :trigger 'session-end :segment 1
                        :source-revision (make-string 64 ?a) :turns '(1) :turn-ids (list (format "%064x" (1+ n)))
                        :created "2026-09-07T12:00:00Z" :model "test:model")
             "## Done\n- Observed: Tests passed.\n## Learned\n- none\n## Surprised\n- none\n## Unfinished\n- none"))
          (with-temp-file runner
            (insert ";;; -*- lexical-binding: t -*-\n")
            (prin1
             `(progn
                (setq load-path ',load-path)
                (require 'mevedel-memory-pass)
                (require 'mevedel-system)
                (require 'gptel-openai)
                (let* ((workspace (mevedel-workspace--create :root ,root))
                       (mevedel-memory-dirs nil)
                       (mevedel-memory-consolidation-mode 'manual)
                       (backend (gptel-make-openai "holder-test" :key "test-only" :models '(test-model)))
                       (deadline (+ (float-time) 55)) callback entries result)
                  (unwind-protect
                      (cl-letf (((symbol-function 'mevedel-memory-review-request)
                                 (lambda (_scope selected cb &rest _)
                                   (setq callback cb entries selected) (list :cancel #'ignore))))
                        (mevedel-memory-pass-start workspace (lambda (value) (setq result value)))
                        (unless callback (error "Pass did not reach inference"))
                        (write-region "ready" nil ,ready nil 'silent)
                        (while (and (not (file-exists-p ,release)) (< (float-time) deadline))
                          (accept-process-output nil 0.01))
                        (unless (file-exists-p ,release) (error "Release barrier timed out"))
                        (funcall callback
                                 (list :outcome 'success :entries entries :policy (list :backend backend :model 'test-model)
                                       :reply "## Promote\n- none\n## Update\n- none\n## Merge\n- none\n## Remove\n- none\n## Instructions\n- none\n## No action\n- No supported changes."))
                        (unless (eq (plist-get result :outcome) 'success) (error "Pass did not publish")))
                    (mevedel-memory-pass-cancel workspace))))
             (current-buffer)))
          (setq process (make-process :name "memory-pass-holder" :buffer output :noquery t :sentinel #'ignore
                                      :command (list (file-name-concat invocation-directory invocation-name) "--batch" "-Q" "-l" runner)))
          (while (and (process-live-p process) (not (file-exists-p ready)) (< (float-time) deadline))
            (accept-process-output nil 0.01))
          (ert-info ((with-current-buffer output (buffer-string)))
            (should (file-exists-p ready))
            (should (process-live-p process)))
          (let ((claim (mevedel-journal-claim-current (mevedel-memory-store--claim-directory workspace))))
            (should-not (mevedel-memory-pass-start workspace #'ignore :automatic t))
            (should-error (mevedel-memory-pass-start workspace #'ignore))
            (should-not (mevedel-journal-claim-outcome claim)))
          (write-region "release" nil release nil 'silent)
          (while (and (process-live-p process) (< (float-time) deadline)) (accept-process-output nil 0.01))
          (ert-info ((with-current-buffer output (buffer-string)))
            (should-not (process-live-p process))
            (should (= 0 (process-exit-status process))))
          (should-not (mevedel-memory-pass-start workspace #'ignore :automatic t))
          (let ((entries (mevedel-journal-store-entries root)))
            (should (= 6 (length entries)))
            (should-not (mevedel-journal-index-unreviewed entries))))
      (when (and process (process-live-p process)) (delete-process process))
      (mevedel-memory-pass-stop-all)
      (kill-buffer output)
      (delete-directory root t))))

(provide 'test-mevedel-memory-pass-clients)
;;; test-mevedel-memory-pass-clients.el ends here
