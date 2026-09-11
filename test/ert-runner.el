;;; ert-runner.el --- Eask partition measurements -*- lexical-binding: t -*-

;;; Commentary:

;; Loaded by run_tests.py before the complete test-file roster.  Eask still
;; owns HOME/XDG isolation and ERT's exit status.  Each worker verifies the
;; complete inventory before running its assigned cases.

;;; Code:

(require 'ert)
(require 'json)
(require 'seq)

(defun mevedel-test-runner-dispatch (original &rest _args)
  "Discover tests or run a measured partition through ORIGINAL.
Read paths from MEVEDEL_TEST_PARTITION and MEVEDEL_TEST_RESULTS.  Without a
partition, write the complete inventory and exit without running tests."
  (let* ((input (getenv "MEVEDEL_TEST_PARTITION"))
         (output (or (getenv "MEVEDEL_TEST_RESULTS")
                     (error "Missing test results path")))
         (inventory
          (mapcar (lambda (test) (symbol-name (ert-test-name test)))
                  (ert-select-tests t t))))
    (unless input
      (with-temp-file output
        (insert (json-serialize (vconcat inventory))))
      (kill-emacs 0))
    (let* ((request (with-temp-buffer
                      (insert-file-contents input)
                      (json-parse-buffer :object-type 'plist :array-type 'list)))
           (names (plist-get request :tests))
           (start (current-time)))
      (unless (equal inventory (plist-get request :inventory))
        (error "Test inventory changed after discovery"))
      (unless (and names
                   (= (length names) (length (delete-dups (copy-sequence names))))
                   (seq-every-p (lambda (name) (member name inventory)) names))
        (error "Invalid or empty test partition"))
      (let ((stats (funcall original (cons 'member (mapcar #'intern names))))
            rows)
        (cl-loop for test across (ert--stats-tests stats)
                 for result across (ert--stats-test-results stats)
                 do (push (list :test (symbol-name (ert-test-name test))
                                :seconds (ert-test-result-duration result)
                                :status (symbol-name (type-of result))
                                :expected
                                (if (ert-test-result-expected-p test result) t :false))
                          rows))
        (with-temp-file output
          (insert (json-serialize
                   (list :ert_seconds (float-time (time-since start))
                         :tests (vconcat (nreverse rows))))))
        stats))))

(when (getenv "MEVEDEL_TEST_RESULTS")
  (advice-add 'ert-run-tests-batch :around #'mevedel-test-runner-dispatch))

(provide 'ert-runner)
;;; ert-runner.el ends here
