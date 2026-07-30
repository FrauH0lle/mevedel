;;; mevedel-profiler-optimization.el --- Profiler optimization benchmarks -*- lexical-binding: t; -*-

;;; Commentary:

;; Run deterministic workloads from a clean batch Emacs:
;;
;;   eask emacs --batch -L . -L test \
;;     -l benchmark/mevedel-profiler-optimization.el -- SCENARIO OUTPUT-DIR
;;
;; Each scenario writes metrics.el plus native CPU and memory profiler
;; artifacts below OUTPUT-DIR.

;;; Code:

(require 'benchmark)
(require 'cl-lib)
(require 'mevedel)
(require 'profiler)

(defconst mevedel-benchmark--warmups 2)
(defconst mevedel-benchmark--rounds 5)

(defun mevedel-benchmark--median (values)
  "Return the median of numeric VALUES."
  (let* ((sorted (sort (copy-sequence values) #'<))
         (middle (/ (length sorted) 2)))
    (if (cl-oddp (length sorted))
        (nth middle sorted)
      (/ (+ (nth (1- middle) sorted)
            (nth middle sorted))
         2.0))))

(defun mevedel-benchmark--profile-total (table)
  "Return the sum of numeric profiler samples in hash TABLE."
  (let ((total 0))
    (when (hash-table-p table)
      (maphash (lambda (_stack value)
                 (when (numberp value)
                   (cl-incf total value)))
               table))
    total))

(defun mevedel-benchmark--write-metrics (directory metrics)
  "Write METRICS as readable Lisp below DIRECTORY."
  (make-directory directory t)
  (let ((print-length nil)
        (print-level nil)
        (file (file-name-concat directory "metrics.el")))
    (with-temp-file file
      (prin1 metrics (current-buffer))
      (insert "\n"))
    file))

(defun mevedel-benchmark--measure (scenario parameters workload directory)
  "Measure SCENARIO WORKLOAD and write artifacts below DIRECTORY.
PARAMETERS describes the deterministic workload."
  (dotimes (_ mevedel-benchmark--warmups)
    (funcall workload))
  (let (rounds)
    (dotimes (_ mevedel-benchmark--rounds)
      (garbage-collect)
      (push (benchmark-run 1 (funcall workload)) rounds))
    (setq rounds (nreverse rounds))
    (garbage-collect)
    (profiler-start 'cpu+mem)
    (funcall workload)
    (profiler-stop)
    (make-directory directory t)
    (let* ((elapsed (mapcar #'car rounds))
           (gc-counts (mapcar #'cadr rounds))
           (gc-times (mapcar #'caddr rounds))
           (metrics
            (list :schema-version 1
                  :scenario scenario
                  :parameters parameters
                  :warmups mevedel-benchmark--warmups
                  :rounds mevedel-benchmark--rounds
                  :elapsed-seconds elapsed
                  :elapsed-median (mevedel-benchmark--median elapsed)
                  :gc-counts gc-counts
                  :gc-count-median (mevedel-benchmark--median gc-counts)
                  :gc-seconds gc-times
                  :gc-seconds-median (mevedel-benchmark--median gc-times)
                  :sampled-allocation
                  (mevedel-benchmark--profile-total profiler-memory-log)
                  :cpu-samples
                  (mevedel-benchmark--profile-total profiler-cpu-log)
                  :git-head
                  (car (process-lines "git" "rev-parse" "HEAD"))
                  :emacs-version emacs-version
                  :system-configuration system-configuration)))
      (mevedel-telemetry--write-profiler-artifacts directory)
      (mevedel-benchmark--write-metrics directory metrics)
      metrics)))

(defun mevedel-benchmark--retry-gap (directory)
  "Run the large transcript retry-gap workload below DIRECTORY."
  (let ((size (* 4 1024 1024))
        (iterations 50))
    (with-temp-buffer
      (insert (make-string (1- size) ?\s) "x")
      (put-text-property (point-min) (point-max)
                         'gptel '(tool . "benchmark"))
      (let ((workload
             (lambda ()
               (dotimes (_ iterations)
                 (unless
                     (mevedel-transcript--tool-block-retry-gap-p
                      (point-min) (point-max))
                   (error "Retry-gap benchmark fixture was rejected"))))))
        (mevedel-benchmark--measure
         'retry-gap
         (list :gap-bytes size :iterations iterations)
         workload directory)))))

(defun mevedel-benchmark-run (scenario directory)
  "Run named SCENARIO and write its artifacts below DIRECTORY."
  (pcase scenario
    ("retry-gap" (mevedel-benchmark--retry-gap directory))
    (_ (error "Unknown benchmark scenario: %s" scenario))))

(when noninteractive
  (when (equal (car command-line-args-left) "--")
    (pop command-line-args-left))
  (let ((scenario (pop command-line-args-left))
        (directory (pop command-line-args-left)))
    (unless (and scenario directory (null command-line-args-left))
      (error "Usage: ... -- SCENARIO OUTPUT-DIR"))
    (prin1 (mevedel-benchmark-run scenario (expand-file-name directory)))
    (terpri)))

(provide 'mevedel-profiler-optimization)
;;; mevedel-profiler-optimization.el ends here
