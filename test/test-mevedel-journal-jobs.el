;;; test-mevedel-journal-jobs.el -- Journal job controls -*- lexical-binding: t -*-

;;; Commentary:

;; Reads durable job states and exercises the human controls with a real view.

;;; Code:

(require 'mevedel-journal-test-support
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "mevedel-journal-test-support"))
(require 'mevedel-journal-jobs)

(mevedel-deftest mevedel-journal-jobs--workspace ()
  ,test
  (test)
  :doc "commands resolve ordinary workspace buffers without a session cockpit"
  (mevedel-test-journal-capture--with-session
   (lambda (session buffer)
     (let ((workspace (mevedel-session-workspace session)))
       (with-current-buffer buffer
         (should (eq workspace (mevedel-journal-jobs--workspace))))
       (with-temp-buffer
         (setq-local mevedel--workspace workspace)
         (should (eq workspace (mevedel-journal-jobs--workspace))))
       (with-temp-buffer
         (setq-local mevedel-journal-jobs--workspace workspace)
         (should (eq workspace (mevedel-journal-jobs--workspace))))))))

(mevedel-deftest mevedel-journal-jobs--records ()
  ,test
  (test)
  :doc "inspection distinguishes checkpoints, failures, exhaustion, and unavailable policy"
  (mevedel-test-journal-capture--with-session
   (lambda (session buffer)
     (mevedel-test-journal-capture--turn session buffer "Request" "Result")
     (let ((workspace (mevedel-session-workspace session)))
       (should (eq 'checkpoint (plist-get (car (mevedel-journal-jobs--records workspace)) :status)))
       (mevedel-journal-capture-seal session buffer 'session-end)
       (should (eq 'pending (plist-get (car (mevedel-journal-jobs--records workspace)) :status)))
       (cl-letf (((symbol-function 'gptel-request)
                  (lambda (_prompt &rest args)
                    (funcall (plist-get args :callback) nil '(:error "Provider failed")))))
         (mevedel-journal-process-next workspace)
         (let ((job (car (mevedel-journal-jobs--records workspace))))
           (should (eq 'failed (plist-get job :status)))
           (should (equal "Context summary request failed: Provider failed" (plist-get job :detail))))
         (dotimes (_ 2) (mevedel-journal-process-next workspace)))
       (should (eq 'exhausted (plist-get (car (mevedel-journal-jobs--records workspace)) :status)))
       (let ((gptel--known-backends nil))
         (should (eq 'unavailable (plist-get (car (mevedel-journal-jobs--records workspace)) :status)))))))

  :doc "damaged descriptors retain known attempt counts and expose accepted-discard recovery"
  (mevedel-test-journal-capture--with-session
   (lambda (session buffer)
     (mevedel-test-journal-capture--turn session buffer "Request" "Result")
     (let* ((workspace (mevedel-session-workspace session))
            (capture (car (mevedel-journal-capture-list workspace)))
            (id (plist-get capture :id))
            (payload (mevedel-journal-discard-prepare workspace id))
            (claim (mevedel-journal-claim-acquire (mevedel-journal-process--attempts workspace capture) 120)))
       (write-region "damaged descriptor" nil
                     (file-name-concat (mevedel-journal-capture--directory workspace id) "capture.json") nil 'silent)
       (let ((job (car (mevedel-journal-jobs--records workspace))))
         (should (eq 'unreadable (plist-get job :status)))
         (should (= 1 (plist-get job :attempts))))
       (should (mevedel-journal-claim-settle claim 'cancelled payload))
       (should (eq 'recovery (plist-get (car (mevedel-journal-jobs--records workspace)) :status))))))

  :doc "accepted results expose their foreign-client recovery requirement without mutation"
  (mevedel-test-journal-capture--with-session
   (lambda (session buffer)
     (mevedel-test-journal-capture--turn session buffer "Request" "Saved evidence")
     (let* ((workspace (mevedel-session-workspace session))
            (capture (car (mevedel-journal-capture-seal session buffer 'session-end)))
            (path (file-name-concat (mevedel-journal-capture--directory workspace (plist-get capture :id)) "capture.json"))
            (record (json-parse-string (mevedel-session-control-fs-read-file path)))
            (claim (mevedel-journal-claim-acquire (mevedel-journal-process--attempts workspace capture) 120)))
       (puthash "source-client" (make-string 64 ?0) record)
       (write-region (json-serialize record) nil path nil 'silent)
       (should (mevedel-journal-claim-settle claim 'completed
                                          "## Done\n- Observed: saved evidence.\n\n## Learned\n- none\n\n## Surprised\n- none\n\n## Unfinished\n- none"))
       (let ((job (car (mevedel-journal-jobs--records workspace))))
         (should (eq 'recovery (plist-get job :status)))
         (should (equal "Journal capture source belongs to another client" (plist-get job :detail))))
       (should (mevedel-journal-pins-present-p (mevedel-session-save-path session)))
       (should-not (mevedel-journal-store-entries (mevedel-workspace-root workspace))))))

  :doc "unready captures remain inspectable while retired captures stay out of the pending list"
  (mevedel-test-journal-capture--with-session
   (lambda (session buffer)
     (mevedel-test-journal-capture--turn session buffer "Request" "Saved evidence")
     (let* ((workspace (mevedel-session-workspace session))
            (capture (car (mevedel-journal-capture-list workspace)))
            (id (plist-get capture :id))
            (directory (mevedel-journal-capture--directory workspace id)))
       (delete-file (file-name-concat directory "ready"))
       (let ((job (car (mevedel-journal-jobs--records workspace))))
         (should (equal id (plist-get job :id)))
         (should (eq 'recovery (plist-get job :status)))
         (should (string-match-p "incomplete" (plist-get job :detail))))
       (write-region "damaged descriptor" nil (file-name-concat directory "capture.json") nil 'silent)
       (should (eq 'unreadable (plist-get (car (mevedel-journal-jobs--records workspace)) :status)))
       (write-region "retired" nil (file-name-concat directory "retired") nil 'silent)
       (should-not (mevedel-journal-jobs--records workspace))))))

(mevedel-deftest mevedel-journal-jobs (:quiet t)
  ,test
  (test)
  :doc "inspection and discard preserve a multiline leading-> composer draft"
  (mevedel-test-journal-capture--with-session
   (lambda (session buffer)
     (mevedel-test-journal-capture--turn session buffer "Request" "Decisive saved result")
     (let* ((workspace (mevedel-session-workspace session))
            (id (plist-get (car (mevedel-journal-capture-seal session buffer 'session-end)) :id))
            (view (generate-new-buffer " *journal-jobs-source-view*"))
            (draft "> quoted\nsecond line"))
       (unwind-protect
           (save-window-excursion
             (mevedel-view--setup view buffer)
             (with-current-buffer view
               (goto-char (mevedel-view--input-start))
               (insert draft))
             (let ((jobs (mevedel-journal-jobs workspace)))
               (should (buffer-local-value 'buffer-read-only jobs))
               (with-current-buffer jobs (should (string-match-p "pending" (buffer-string)))))
             (mevedel-journal-inspect workspace id)
             (with-current-buffer "*mevedel journal evidence*"
               (should (string-match-p "Decisive saved result" (buffer-string))))
             (mevedel-journal-discard workspace id)
             (with-current-buffer view (should (equal draft (mevedel-view--input-text))))
             (with-current-buffer "*mevedel journal jobs*"
               (should (string-match-p "No pending jobs" (buffer-string)))))
         (dolist (candidate (list view (get-buffer "*mevedel journal jobs*")
                                  (get-buffer "*mevedel journal evidence*")))
           (when (buffer-live-p candidate) (kill-buffer candidate))))))))

(mevedel-deftest mevedel-journal-jobs--retained (:quiet t)
  ,test
  (test)
  :doc "overdue evidence exposes age, unfinished review and capture reasons without changing storage"
  (mevedel-test-journal-capture--with-session
   (lambda (session buffer)
     (mevedel-test-journal-capture--turn session buffer "Request" "Saved evidence")
     (let* ((workspace (mevedel-session-workspace session))
            (root (mevedel-workspace-root workspace))
            (capture (car (mevedel-journal-capture-seal session buffer 'session-end)))
            (metadata (copy-tree (mevedel-journal-process--metadata workspace capture)))
            (mevedel-journal-max-age-days 14)
            (mevedel-memory-consolidation-mode 'manual)
            (clock (symbol-function 'float-time))
            (now (float-time (date-to-time "2026-09-22T12:00:00Z"))))
       (plist-put metadata :created "2026-09-07T12:00:00Z")
       (let* ((entry (mevedel-journal-store-publish-digest
                      root metadata
                      "## Done\n- Observed: Retained result.\n## Learned\n- none\n## Surprised\n- none\n## Unfinished\n- none"))
              (file (plist-get entry :file)))
         (unwind-protect
             (cl-letf (((symbol-function 'float-time)
                        (lambda (&optional value) (if value (funcall clock value) now))))
               (let ((row (car (mevedel-journal-jobs--retained workspace))))
                 (should (equal entry (plist-get row :entry)))
                 (should (= 15 (plist-get row :age)))
                 (should (member "Unreviewed" (plist-get row :reasons)))
                 (should (member "Capture completion pending" (plist-get row :reasons))))
               (save-window-excursion
                 (with-current-buffer (mevedel-journal-jobs workspace)
                   (should (string-search "Overdue retained evidence: 1" (buffer-string)))
                   (should (string-search "15 days" (buffer-string)))
                   (should (string-search "manual" (buffer-string)))
                   (goto-char (point-min))
                   (search-forward "Inspect retained entry")
                   (button-activate (button-at (1- (point)))))
                 (with-current-buffer "*mevedel journal evidence*"
                   (should (string-search "Retained result" (buffer-string)))))
               (should-not (mevedel-journal-index-entries workspace))
               (should (equal entry (mevedel-journal-store-read root file)))
               (let ((mevedel-journal-max-age-days nil))
                 (should-not (mevedel-journal-jobs--retained workspace))))
           (dolist (name '("*mevedel journal jobs*" "*mevedel journal evidence*"))
             (when (get-buffer name) (kill-buffer name)))))))))

(provide 'test-mevedel-journal-jobs)
;;; test-mevedel-journal-jobs.el ends here
