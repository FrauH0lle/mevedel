;;; test-niwa-journal-jobs.el -- Journal job controls -*- lexical-binding: t -*-

;;; Commentary:

;; Reads durable job states and exercises the human controls with a real view.

;;; Code:

(require 'niwa-journal-test-support
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "niwa-journal-test-support"))
(require 'niwa-journal-jobs)

(niwa-deftest niwa-journal-jobs--workspace ()
  ,test
  (test)
  :doc "commands resolve ordinary workspace buffers without a session cockpit"
  (niwa-test-journal-capture--with-session
   (lambda (session buffer)
     (let ((workspace (niwa-session-workspace session)))
       (with-current-buffer buffer
         (should (eq workspace (niwa-journal-jobs--workspace))))
       (with-temp-buffer
         (setq-local niwa--workspace workspace)
         (should (eq workspace (niwa-journal-jobs--workspace))))
       (with-temp-buffer
         (setq-local niwa-journal-jobs--workspace workspace)
         (should (eq workspace (niwa-journal-jobs--workspace))))))))

(niwa-deftest niwa-journal-jobs--records ()
  ,test
  (test)
  :doc "inspection distinguishes checkpoints, failures, exhaustion, and unavailable policy"
  (niwa-test-journal-capture--with-session
   (lambda (session buffer)
     (niwa-test-journal-capture--turn session buffer "Request" "Result")
     (let ((workspace (niwa-session-workspace session)))
       (should (eq 'checkpoint (plist-get (car (niwa-journal-jobs--records workspace)) :status)))
       (niwa-journal-capture-seal session buffer 'session-end)
       (should (eq 'pending (plist-get (car (niwa-journal-jobs--records workspace)) :status)))
       (cl-letf (((symbol-function 'gptel-request)
                  (lambda (_prompt &rest args)
                    (funcall (plist-get args :callback) nil '(:error "Provider failed")))))
         (niwa-journal-process-next workspace)
         (let ((job (car (niwa-journal-jobs--records workspace))))
           (should (eq 'failed (plist-get job :status)))
           (should (equal "Context summary request failed: Provider failed" (plist-get job :detail))))
         (dotimes (_ 2) (niwa-journal-process-next workspace)))
       (should (eq 'exhausted (plist-get (car (niwa-journal-jobs--records workspace)) :status)))
       (let ((gptel--known-backends nil))
         (should (eq 'unavailable (plist-get (car (niwa-journal-jobs--records workspace)) :status)))))))

  :doc "damaged descriptors retain known attempt counts and expose accepted-discard recovery"
  (niwa-test-journal-capture--with-session
   (lambda (session buffer)
     (niwa-test-journal-capture--turn session buffer "Request" "Result")
     (let* ((workspace (niwa-session-workspace session))
            (capture (car (niwa-journal-capture-list workspace)))
            (id (plist-get capture :id))
            (payload (niwa-journal-discard-prepare workspace id))
            (claim (niwa-journal-claim-acquire (niwa-journal-process--attempts workspace capture) 120)))
       (write-region "damaged descriptor" nil
                     (file-name-concat (niwa-journal-capture--directory workspace id) "capture.json") nil 'silent)
       (let ((job (car (niwa-journal-jobs--records workspace))))
         (should (eq 'unreadable (plist-get job :status)))
         (should (= 1 (plist-get job :attempts))))
       (should (niwa-journal-claim-settle claim 'cancelled payload))
       (should (eq 'recovery (plist-get (car (niwa-journal-jobs--records workspace)) :status))))))

  :doc "accepted results expose their foreign-client recovery requirement without mutation"
  (niwa-test-journal-capture--with-session
   (lambda (session buffer)
     (niwa-test-journal-capture--turn session buffer "Request" "Saved evidence")
     (let* ((workspace (niwa-session-workspace session))
            (capture (car (niwa-journal-capture-seal session buffer 'session-end)))
            (path (file-name-concat (niwa-journal-capture--directory workspace (plist-get capture :id)) "capture.json"))
            (record (json-parse-string (niwa-session-control-fs-read-file path)))
            (claim (niwa-journal-claim-acquire (niwa-journal-process--attempts workspace capture) 120)))
       (puthash "source-kind" "client" record)
       (puthash "source-client" (make-string 64 ?0) record)
       (write-region (json-serialize record) nil path nil 'silent)
       (should (niwa-journal-claim-settle claim 'completed
                                          "## Done\n- Observed: saved evidence.\n\n## Learned\n- none\n\n## Surprised\n- none\n\n## Unfinished\n- none"))
       (let ((job (car (niwa-journal-jobs--records workspace))))
         (should (eq 'recovery (plist-get job :status)))
         (should (equal "Journal capture source belongs to another client" (plist-get job :detail))))
       (should (niwa-journal-pins-present-p (niwa-session-save-path session)))
       (should-not (niwa-journal-store-entries (niwa-workspace-root workspace))))))

  :doc "unready captures remain inspectable while retired captures stay out of the pending list"
  (niwa-test-journal-capture--with-session
   (lambda (session buffer)
     (niwa-test-journal-capture--turn session buffer "Request" "Saved evidence")
     (let* ((workspace (niwa-session-workspace session))
            (capture (car (niwa-journal-capture-list workspace)))
            (id (plist-get capture :id))
            (directory (niwa-journal-capture--directory workspace id)))
       (delete-file (file-name-concat directory "ready"))
       (let ((job (car (niwa-journal-jobs--records workspace))))
         (should (equal id (plist-get job :id)))
         (should (eq 'recovery (plist-get job :status)))
         (should (string-match-p "incomplete" (plist-get job :detail))))
       (write-region "damaged descriptor" nil (file-name-concat directory "capture.json") nil 'silent)
       (should (eq 'unreadable (plist-get (car (niwa-journal-jobs--records workspace)) :status)))
       (write-region "retired" nil (file-name-concat directory "retired") nil 'silent)
       (should-not (niwa-journal-jobs--records workspace))))))

(niwa-deftest niwa-journal-jobs (:quiet t)
  ,test
  (test)
  :doc "inspection and discard preserve a multiline leading-> composer draft"
  (niwa-test-journal-capture--with-session
   (lambda (session buffer)
     (niwa-test-journal-capture--turn session buffer "Request" "Decisive saved result")
     (let* ((workspace (niwa-session-workspace session))
            (id (plist-get (car (niwa-journal-capture-seal session buffer 'session-end)) :id))
            (view (generate-new-buffer " *journal-jobs-source-view*"))
            (draft "> quoted\nsecond line"))
       (unwind-protect
           (save-window-excursion
             (niwa-view--setup view buffer)
             (with-current-buffer view
               (niwa-view-composer-insert draft))
             (let ((jobs (niwa-journal-jobs workspace)))
               (should (buffer-local-value 'buffer-read-only jobs))
               (with-current-buffer jobs (should (string-match-p "pending" (buffer-string)))))
             (niwa-journal-inspect workspace id)
             (with-current-buffer "*niwa journal evidence*"
			      (should (derived-mode-p 'niwa-report-mode))
			      (should (string-match-p "Frozen evidence" (buffer-string)))
			      (should (string-match-p "Decisive saved result" (buffer-string)))
			      (niwa-report-previous)
			      (should (string-match-p "Capture diagnostics" (buffer-string)))
			      (should (string-match-p "Attempts" (buffer-string)))
			      (niwa-report-refresh)
			      (should (string-match-p "Capture diagnostics" (buffer-string))))
             (niwa-journal-discard workspace id)
             (with-current-buffer view (should (equal draft (niwa-view-composer-draft-text))))
             (with-current-buffer "*niwa journal jobs*"
               (should (string-match-p "No pending jobs" (buffer-string)))))
         (dolist (candidate (list view (get-buffer "*niwa journal jobs*")
                                  (get-buffer "*niwa journal evidence*")))
           (when (buffer-live-p candidate) (kill-buffer candidate))))))))

(niwa-deftest niwa-journal-jobs--retained (:quiet t)
  ,test
  (test)
  :doc "overdue evidence exposes age, unfinished review and capture reasons without changing storage"
  (niwa-test-journal-capture--with-session
   (lambda (session buffer)
     (niwa-test-journal-capture--turn session buffer "Request" "Saved evidence")
     (let* ((workspace (niwa-session-workspace session))
            (root (niwa-workspace-root workspace))
            (capture (car (niwa-journal-capture-seal session buffer 'session-end)))
            (metadata (copy-tree (niwa-journal-process--metadata workspace capture)))
            (niwa-journal-max-age-days 14)
            (niwa-memory-consolidation-mode 'manual)
            (clock (symbol-function 'float-time))
            (now (float-time (date-to-time "2026-09-22T12:00:00Z"))))
       (plist-put metadata :created "2026-09-07T12:00:00Z")
       (let* ((entry (niwa-journal-store-publish-digest
                      root metadata
                      "## Done\n- Observed: Retained result.\n## Learned\n- none\n## Surprised\n- none\n## Unfinished\n- none"))
              (file (plist-get entry :file)))
         (unwind-protect
             (cl-letf (((symbol-function 'float-time)
                        (lambda (&optional value) (if value (funcall clock value) now))))
               (let ((row (car (niwa-journal-jobs--retained workspace))))
                 (should (equal entry (plist-get row :entry)))
                 (should (= 15 (plist-get row :age)))
                 (should (member "Unreviewed" (plist-get row :reasons)))
                 (should (member "Capture completion pending" (plist-get row :reasons))))
               (save-window-excursion
                 (with-current-buffer (niwa-journal-jobs workspace)
                   (should (string-search "Overdue retained evidence: 1" (buffer-string)))
                   (should (string-search "15 days" (buffer-string)))
                   (should (string-search "manual" (buffer-string)))
                   (goto-char (point-min))
                   (search-forward "Inspect retained entry")
                   (button-activate (button-at (1- (point)))))
                 (with-current-buffer "*niwa journal evidence*"
                   (should (string-search "Retained result" (buffer-string)))))
               (should-not (niwa-journal-index-entries workspace))
               (should (equal entry (niwa-journal-store-read root file)))
               (let ((niwa-journal-max-age-days nil))
                 (should-not (niwa-journal-jobs--retained workspace))))
           (dolist (name '("*niwa journal jobs*" "*niwa journal evidence*"))
             (when (get-buffer name) (kill-buffer name)))))))))

(provide 'test-niwa-journal-jobs)
;;; test-niwa-journal-jobs.el ends here
