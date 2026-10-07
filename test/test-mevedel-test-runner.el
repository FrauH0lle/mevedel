;;; test-mevedel-test-runner.el --- Partition runner tests -*- lexical-binding: t -*-

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

(mevedel-deftest mevedel-test-runner-dispatch ()
  ,test
  (test)
  :doc "discovery writes the full inventory without executing a test"
  (let* ((process-environment (copy-sequence process-environment))
         (output (make-temp-file "mevedel-test-discovery-")))
    (unwind-protect
        (progn
          (setenv "MEVEDEL_TEST_PARTITION" nil)
          (setenv "MEVEDEL_TEST_RESULTS" output)
          (should
           (eq 'no-tests
               (mevedel-test-runner-dispatch
                (lambda (selector)
                  (when (ert-select-tests selector t)
                    (error "Discovery ran tests"))
                  'no-tests))))
          (should
           (equal
            (mapcar (lambda (test) (symbol-name (ert-test-name test)))
                    (ert-select-tests t t))
            (with-temp-buffer
              (insert-file-contents output)
              (json-parse-buffer :array-type 'list)))))
      (delete-file output)))

  :doc "rejects changed or invalid assignments and retains unexpected failures"
  (let* ((process-environment (copy-sequence process-environment))
         (directory (make-temp-file "mevedel-test-partition-" t))
         (input (file-name-concat directory "partition.json"))
         (output (file-name-concat directory "results.json")))
    (unwind-protect
        (progn
          (ert-deftest mevedel-test-runner--passing-fixture () (should t))
          (ert-deftest mevedel-test-runner--failing-fixture () (should nil))
          (setenv "MEVEDEL_TEST_PARTITION" input)
          (setenv "MEVEDEL_TEST_RESULTS" output)
          (let* ((inventory
                  (vconcat
                   (mapcar (lambda (test) (symbol-name (ert-test-name test)))
                           (ert-select-tests t t))))
                 (names ["mevedel-test-runner--passing-fixture"
                         "mevedel-test-runner--failing-fixture"]))
            (dolist (request
                     (list (list :inventory [] :tests names)
                           (list :inventory inventory :tests [])
                           (list :inventory inventory :tests ["missing-fixture"])
                           (list :inventory inventory
                                 :tests (vector (aref names 0) (aref names 0)))))
              (with-temp-file input (insert (json-serialize request)))
              (let (called)
                (should-error
                 (mevedel-test-runner-dispatch
                  (lambda (&rest _) (setq called t))))
                (should-not called)))
            (with-temp-file input
              (insert (json-serialize (list :inventory inventory :tests names))))
            (let* ((stats
                    (mevedel-test-runner-dispatch
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
      (dolist (name '(mevedel-test-runner--passing-fixture
                      mevedel-test-runner--failing-fixture))
        (when (ert-test-boundp name) (ert-delete-test name)))
      (delete-directory directory t))))

(provide 'test-mevedel-test-runner)
;;; test-mevedel-test-runner.el ends here
