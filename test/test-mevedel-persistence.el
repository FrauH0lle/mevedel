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
  :doc "round-trips directive identity, execution binding, attempts, and overlay"
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
                      :request "Exact submitted request"
                      :result "Exact answer"
                      :outcome 'success
                      :patch "diff --git a/source.el b/source.el\n"
                      :capture 'incomplete
                      :covered-files (list source)
                      :gaps (list (cons (file-name-concat root "missing.el")
                                        'not-observed))
                      :checkpoint '(:session-id "session-1" :turn 3)))
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
                             (mevedel-directive-attempt-checkpoint attempt))))
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
