;;; test-niwa-test-runner.el --- Partition runner tests -*- lexical-binding: t -*-

;;; Commentary:

;; Check inventory validation and preserve real ERT failures in measurements.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'ert-runner
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "ert-runner"))

(niwa-deftest niwa-test-runner-dispatch ()
  ,test
  (test)
  :doc "discovery writes the full inventory and defining files without executing a test"
  (let* ((process-environment (copy-sequence process-environment))
         (output (make-temp-file "niwa-test-discovery-")))
    (unwind-protect
        (progn
          (setenv "NIWA_TEST_PARTITION" nil)
          (setenv "NIWA_TEST_RESULTS" output)
          (setenv "NIWA_TEST_FRESH_FILE" nil)
          (should
           (eq 'no-tests
               (niwa-test-runner-dispatch
                (lambda (selector)
                  (when (ert-select-tests selector t)
                    (error "Discovery ran tests"))
                  'no-tests))))
          (let ((discovered (with-temp-buffer
                              (insert-file-contents output)
                              (json-parse-buffer :object-type 'plist
                                                 :array-type 'list))))
            (should (equal (mapcar (lambda (test) (symbol-name (ert-test-name test)))
                                   (ert-select-tests t t))
                           (plist-get discovered :inventory)))
            (should (equal (mapcar (lambda (test) (or (ert-test-file-name test) ""))
                                   (ert-select-tests t t))
                           (plist-get discovered :files)))
            ;; This very case is defined in this file.
            (should (string-suffix-p
                     "test-niwa-test-runner.el"
                     (nth (cl-position "niwa-test-runner-dispatch/test@1"
                                       (plist-get discovered :inventory)
                                       :test #'equal)
                          (plist-get discovered :files))))))
      (delete-file output)))

  :doc "rejects changed or invalid assignments and retains unexpected failures"
  (let* ((process-environment (copy-sequence process-environment))
         (directory (make-temp-file "niwa-test-partition-" t))
         (input (file-name-concat directory "partition.json"))
         (output (file-name-concat directory "results.json")))
    (unwind-protect
        (progn
          (ert-deftest niwa-test-runner--passing-fixture () (should t))
          (ert-deftest niwa-test-runner--failing-fixture () (should nil))
          (setenv "NIWA_TEST_PARTITION" input)
          (setenv "NIWA_TEST_RESULTS" output)
          (let* ((inventory
                  (vconcat
                   (mapcar (lambda (test) (symbol-name (ert-test-name test)))
                           (ert-select-tests t t))))
                 (names ["niwa-test-runner--passing-fixture"
                         "niwa-test-runner--failing-fixture"]))
            (dolist (request
                     (list (list :inventory [] :tests names)
                           (list :inventory inventory :tests [])
                           (list :inventory inventory :tests ["missing-fixture"])
                           (list :inventory inventory
                                 :tests (vector (aref names 0) (aref names 0)))))
              (with-temp-file input (insert (json-serialize request)))
              (let (called)
                (should-error
                 (niwa-test-runner-dispatch
                  (lambda (&rest _) (setq called t))))
                (should-not called)))
            (with-temp-file input
              (insert (json-serialize (list :inventory inventory :tests names))))
            (let* ((stats
                    (niwa-test-runner-dispatch
                     (lambda (selector) (ert-run-tests selector #'ignore))))
                   (data (with-temp-buffer
                           (insert-file-contents output)
                           (json-parse-buffer :object-type 'plist :array-type 'list)))
                   (rows (plist-get data :tests)))
              (should (= 2 (length (ert--stats-tests stats))))
              (should (equal (append names nil)
                             (mapcar (lambda (row) (plist-get row :test)) rows)))
              (should (eq t (plist-get (car rows) :expected)))
              (should (eq :false (plist-get (cadr rows) :expected)))
              (should (equal "ert-test-failed" (plist-get (cadr rows) :status)))
              (should (>= (plist-get data :ert_seconds) 0)))))
      (dolist (name '(niwa-test-runner--passing-fixture
                      niwa-test-runner--failing-fixture))
        (when (ert-test-boundp name) (ert-delete-test name)))
      (delete-directory directory t))))

(niwa-deftest niwa-test-runner-fresh-file ()
  (let* ((process-environment (copy-sequence process-environment))
         (output (make-temp-file "niwa-test-fresh-file-")))
    (unwind-protect
        (progn
          (setenv "NIWA_TEST_PARTITION" nil)
          (setenv "NIWA_TEST_RESULTS" output)
          (setenv "NIWA_TEST_FRESH_FILE" "1")
          (ert-deftest niwa-test-runner--cold-fixture () (should t))
          ;; Restrict the synthetic inventory, not the production dispatcher:
          ;; running every test here would recurse into this very case.
          (let ((select (symbol-function 'ert-select-tests)))
            (cl-letf (((symbol-function 'ert-select-tests)
                       (lambda (selector universe)
                         (funcall select
                                  (if (eq selector t)
                                      '(member niwa-test-runner--cold-fixture)
                                    selector)
                                  universe))))
              (let ((stats (niwa-test-runner-dispatch
                            (lambda (selector) (ert-run-tests selector #'ignore)))))
                (should (= 1 (length (ert--stats-tests stats)))))))
          (let ((data (with-temp-buffer
                        (insert-file-contents output)
                        (json-parse-buffer :object-type 'plist :array-type 'list))))
            (should (equal '("niwa-test-runner--cold-fixture")
                           (plist-get data :inventory)))
            (should (eq t (plist-get (car (plist-get data :tests)) :expected)))))
      (when (ert-test-boundp 'niwa-test-runner--cold-fixture)
        (ert-delete-test 'niwa-test-runner--cold-fixture))
      (delete-file output))))

(provide 'test-niwa-test-runner)
;;; test-niwa-test-runner.el ends here
