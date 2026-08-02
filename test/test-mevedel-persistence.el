;;; test-mevedel-persistence.el --- Tests for mevedel-persistence.el -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'mevedel)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(mevedel-deftest mevedel--load-instructions-file
  (:vars
   ((mevedel--instruction-states (make-hash-table :test #'equal))
    (mevedel--instruction-current-state-key :global)))
  ,test
  (test)
  :doc "round-trips directive identity, execution binding, activity, and overlay"
  (let* ((root (file-name-as-directory
                (make-temp-file "mevedel-directive-persistence-" t)))
         (source (file-name-concat root "source.el"))
         (snapshot (file-name-concat root "instructions.el"))
         (workspace (mevedel-workspace--create
                     :type 'test :id root :root root :name "test"))
         source-buffer directive id)
    (unwind-protect
        (progn
          (with-temp-file source
            (insert "(message \"hello\")\n"))
          (setq source-buffer (find-file-noselect source))
          (with-current-buffer source-buffer
            (setq-local mevedel--workspace workspace)
            (setq directive
                  (mevedel--create-directive-in
                   source-buffer (point-min) (1- (point-max))
                   nil "Keep this request"))
            (setq id (mevedel-directive-id
                      (mevedel--directive-record directive)))
            (let ((record (mevedel--directive-record directive)))
              (setf (mevedel-directive-session-id record) "session-1"
                    (mevedel-directive-attempts record)
                    (list
                     (mevedel-directive-attempt--create
                      :directive-request "Keep this request"
                      :request "Exact submitted request"
                      :result "Exact answer"
                      :outcome 'success
                      :patch "diff --git a/source.el b/source.el\n"
                      :capture 'incomplete
                      :covered-files (list source)
                      :gaps (list (cons (file-name-concat root "missing.el")
                                        'not-observed))
                      :captured-at "2026-08-02T01:00:00+0200"
                      :checkpoint '(:session-id "session-1" :turn 3)))
                    (mevedel-directive-discussion record)
                    (list
                     (mevedel-directive-discussion-turn--create
                      :message "Why this change?"
                      :request "Exact discussion request"
                      :result "Because it is safer."
                      :outcome 'success
                      :attempt-index 1
                      :checkpoint '(:session-id "session-1" :turn 4)))
                    (mevedel-directive-state record) 'implemented))
            (mevedel--write-instructions-file snapshot root t t t)
            (mevedel--clear-instruction-state workspace)
            (mevedel--load-instructions-file
             snapshot root nil t workspace))
          (let* ((record (car (mevedel-workspace-directives workspace)))
                 (restored (mevedel--instruction-with-uuid id workspace)))
            (should (equal id (mevedel-directive-id record)))
            (should (equal "Keep this request"
                           (mevedel-directive-request record)))
            (should (equal source
                           (plist-get (mevedel-directive-anchor record)
                                      :file)))
            (should (equal "session-1" (mevedel-directive-session-id record)))
            (should (eq 'implemented (mevedel-directive-state record)))
            (let ((attempt (car (mevedel-directive-attempts record))))
              (should (equal "Keep this request"
                             (mevedel-directive-attempt-directive-request
                              attempt)))
              (should (equal "Exact submitted request"
                             (mevedel-directive-attempt-request attempt)))
              (should (equal "Exact answer"
                             (mevedel-directive-attempt-result attempt)))
              (should (eq 'success
                          (mevedel-directive-attempt-outcome attempt)))
              (should (eq 'incomplete
                          (mevedel-directive-attempt-capture attempt)))
              (should (equal (list source)
                             (mevedel-directive-attempt-covered-files attempt)))
              (should
               (equal (list (cons (file-name-concat root "missing.el")
                                  'not-observed))
                      (mevedel-directive-attempt-gaps attempt)))
              (should (equal '(:session-id "session-1" :turn 3)
                             (mevedel-directive-attempt-checkpoint attempt)))
              (should (equal "2026-08-02T01:00:00+0200"
                             (mevedel-directive-attempt-captured-at attempt))))
            (let ((turn (car (mevedel-directive-discussion record))))
              (should (equal "Why this change?"
                             (mevedel-directive-discussion-turn-message turn)))
              (should (equal "Exact discussion request"
                             (mevedel-directive-discussion-turn-request turn)))
              (should (equal "Because it is safer."
                             (mevedel-directive-discussion-turn-result turn)))
              (should (eq 'success
                          (mevedel-directive-discussion-turn-outcome turn)))
              (should (= 1
                         (mevedel-directive-discussion-turn-attempt-index turn)))
              (should (equal '(:session-id "session-1" :turn 4)
                             (mevedel-directive-discussion-turn-checkpoint turn))))
            (should (overlayp restored))
            (should (eq record (mevedel--directive-record restored)))
            (should (eq 'implemented (mevedel--directive-state restored)))))
      (when (buffer-live-p source-buffer)
        (with-current-buffer source-buffer
          (setq-local kill-buffer-hook nil)
          (set-buffer-modified-p nil))
        (kill-buffer source-buffer))
      (delete-directory root t)))

  :doc "rejects the superseded overlay-owned directive shape"
  (let* ((root (file-name-as-directory
                (make-temp-file "mevedel-old-directive-shape-" t)))
         (snapshot (file-name-concat root "instructions.el"))
         (workspace (mevedel-workspace--create
                     :type 'test :id root :root root :name "test")))
    (unwind-protect
        (progn
          (with-temp-file snapshot
            (prin1 (list :version (mevedel-version)
                         :ids '(:id-counter 0 :used-ids nil :retired-ids nil)
                         :files nil)
                   (current-buffer)))
          (should-error
           (mevedel--load-instructions-file snapshot root nil t workspace)
           :type 'user-error))
      (delete-directory root t))))

(provide 'test-mevedel-persistence)
;;; test-mevedel-persistence.el ends here
