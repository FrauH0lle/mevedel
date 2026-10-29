;;; test-mevedel-memory-store-measurements.el -- Numeric evidence guards -*- lexical-binding: t -*-

;;; Commentary:

;; Conservative paragraph recognition supplements the full pass path tests.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory (or buffer-file-name load-file-name byte-compile-current-file)) "helpers"))
(require 'mevedel-memory-store)

(mevedel-deftest mevedel-memory-store--measurements ()
  ,test
  (test)
  :doc "protects operation, values, scenario and corrections rather than bare numbers"
  (let ((paragraph "Scenario: 64 decisions, one sample.\n- **Read:** 4,294 programs in 1 sample.\n- Existence: 680 programs."))
    (should (equal (list paragraph "Correction: the 2,736-program leaf is not the separate 64/192 case.")
                   (mevedel-memory-store--measurements
                    (concat "# Counts 2026-10-27\n\n" paragraph
                            "\n\nCorrection: the 2,736-program leaf is not the separate 64/192 case.\n\nOpen: batch probes.")))))
  :doc "source lines, dates, commit hashes and nonnumeric progress are not measurements"
  (should-not (mevedel-memory-store--measurements
               "# Note 2026-10-27\n\nSource: code.el:123-456, commit 5ba2d6e4.\n\nOpen: investigate the remote case."))
  :doc "raw tab-separated counts are measurements, not Read line numbers"
  (should (equal '("512\treads on target A.")
                 (mevedel-memory-store--measurements "512\treads on target A."))))

(mevedel-deftest mevedel-memory-store--contains-paragraph-p ()
  (progn
  (should (mevedel-memory-store--contains-paragraph-p
           "512 reads in 2 samples on target A." "**512 reads**\nin 2 samples on target A."))
  (should (mevedel-memory-store--contains-paragraph-p
           "512 reads in 2 samples on target A." "\"512 reads in 2 samples on target A.\""))
  (should (mevedel-memory-store--contains-paragraph-p
           "512 reads in 2 samples on target A." "\u201c512 reads in 2 samples on target A.\u201d"))
  (should-not (mevedel-memory-store--contains-paragraph-p
               "512 reads in 2 samples on target A." "1512 reads in 2 samples on target A."))
  (should-not (mevedel-memory-store--contains-paragraph-p
               "512 reads in 2 samples on target A." "-512 reads in 2 samples on target A."))
  (should-not (mevedel-memory-store--contains-paragraph-p
               "512 reads in 2 samples on target A." "512 reads in 2 samples on target B."))
  (should-not (mevedel-memory-store--contains-paragraph-p
               "512\treads in 2 samples on target A." "1512\treads in 2 samples on target A."))
  (should (mevedel-memory-store--contains-paragraph-p
           "512\treads on target A." "  352\t512\treads on target A." t))))

(mevedel-deftest mevedel-memory-store--plain ()
  (progn
    (should (equal "512 reads" (mevedel-memory-store--plain "512\treads")))
    (should (equal "512 reads" (mevedel-memory-store--plain "  352\t512\treads" t)))))

(mevedel-deftest mevedel-memory-store--anchor-p ()
  (progn
    (should (mevedel-memory-store--anchor-p "Runtime  ACTIVATION" "The runtime\nactivation completed."))
    (should-not (mevedel-memory-store--anchor-p "activation" "The deactivation completed."))
    (should-not (mevedel-memory-store--anchor-p "" "The check completed."))
    (should-not (mevedel-memory-store--anchor-p "***" "The check completed."))))

(mevedel-deftest mevedel-memory-store--resolution-anchor-p ()
  (progn
    (should (mevedel-memory-store--resolution-anchor-p "activation" "\"The activation completed.\" (lines 3-5)"))
    (should-not (mevedel-memory-store--resolution-anchor-p "activation" "\"The merge completed.\" activation"))))

(provide 'test-mevedel-memory-store-measurements)
;;; test-mevedel-memory-store-measurements.el ends here
