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
(require 'mevedel-file-state)
(require 'mevedel)
(require 'mevedel-structs)
(require 'mevedel-telemetry)
(require 'mevedel-view)
(require 'mevedel-view-composer)
(require 'mevedel-view-interaction)
(require 'mevedel-view-render)
(require 'mevedel-view-zone)
(require 'profiler)

(defvar mevedel-session--read-only-mode)
(defvar org-persist-directory)

(setq org-persist-directory
      (file-name-as-directory
       (file-name-concat temporary-file-directory
                         "mevedel-benchmark-org-persist")))

(defconst mevedel-benchmark--warmups 2)
(defconst mevedel-benchmark--rounds 5)
(defconst mevedel-benchmark--harness-file
  (or load-file-name buffer-file-name))

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

(defun mevedel-benchmark--file-sha256 (file)
  "Return the SHA-256 digest of FILE."
  (with-temp-buffer
    (insert-file-contents-literally file)
    (secure-hash 'sha256 (current-buffer))))

(defun mevedel-benchmark--worktree-diff-sha256 ()
  "Return the SHA-256 digest of the current tracked worktree diff."
  (with-temp-buffer
    (unless (zerop (process-file "git" nil t nil
                                 "diff" "--binary" "HEAD"))
      (error "Could not read worktree diff"))
    (secure-hash 'sha256 (current-buffer))))

(defun mevedel-benchmark--measure
    (scenario parameters workload directory &optional observe)
  "Measure SCENARIO WORKLOAD and write artifacts below DIRECTORY.
PARAMETERS describes the deterministic workload.  OBSERVE, when non-nil,
returns additional counters from one unprofiled workload run."
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
            (append
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
                   :harness-sha256
                   (mevedel-benchmark--file-sha256
                    mevedel-benchmark--harness-file)
                   :worktree-diff-sha256
                   (mevedel-benchmark--worktree-diff-sha256)
                   :dirty-files
                   (process-lines "git" "status" "--short")
                   :emacs-version emacs-version
                   :system-configuration system-configuration)
             (and observe (funcall observe)))))
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

(defun mevedel-benchmark--with-view (function)
  "Call FUNCTION with initialized data and view buffers."
  (let ((data-buf (generate-new-buffer " *mevedel-benchmark-data*"))
        (view-buf (generate-new-buffer " *mevedel-benchmark-view*")))
    (unwind-protect
        (progn
          (with-current-buffer data-buf
            (org-mode)
            (setq-local gptel-response-separator "\n\n")
            (setq-local gptel-prompt-prefix-alist '((org-mode . "*** ")))
            (setq-local mevedel-session--read-only-mode nil))
          (mevedel-view--setup view-buf data-buf)
          (save-window-excursion
            (switch-to-buffer view-buf)
            (funcall function data-buf view-buf)))
      (when (buffer-live-p view-buf)
        (kill-buffer view-buf))
      (when (buffer-live-p data-buf)
        (kill-buffer data-buf)))))

(defun mevedel-benchmark--prompt-redraw (directory)
  "Run large-view prompt lookup and redraw workload below DIRECTORY."
  (let ((transcript-bytes (* 5 1024 1024))
        (iterations 500))
    (mevedel-benchmark--with-view
     (lambda (_data-buf view-buf)
       (with-current-buffer view-buf
         (let ((inhibit-read-only t))
           (mevedel-view--call-with-render-boundaries-advancing
            (lambda ()
              (goto-char (mevedel-view--history-insertion-marker))
              (insert (make-string transcript-bytes ?x)))))
         (goto-char (mevedel-view--input-start))
         (insert "> benchmark draft\nsecond line")
         (goto-char (+ (mevedel-view--input-start) 4))
         (set-window-point (selected-window) (point))
         (let ((workload
                (lambda ()
                  (dotimes (_ iterations)
                    (unless (mevedel-view--prompt-start-position)
                      (error "Prompt benchmark lost the prompt"))))))
           (let ((metrics
                  (mevedel-benchmark--measure
                   'prompt-redraw
                   (list :transcript-bytes transcript-bytes
                         :iterations iterations)
                   workload directory
                   (lambda ()
                     (let ((original (symbol-function 'text-property-any))
                           (calls 0)
                           (scanned 0))
                       (cl-letf
                           (((symbol-function 'text-property-any)
                             (lambda (start end property value
                                            &optional object)
                               (when (eq property 'mevedel-view-prompt)
                                 (cl-incf calls)
                                 (cl-incf scanned (- end start)))
                               (funcall original start end property value
                                        object))))
                         (funcall workload))
                       (list :prompt-scan-calls calls
                             :prompt-scan-characters scanned))))))
             (mevedel-view--render-status)
             (mevedel-view--interaction-register
              (list :id 'benchmark :kind 'ask :body "benchmark\n"))
             (unless (and (= (point) (+ (mevedel-view--input-start) 4))
                          (= (window-point (selected-window)) (point))
                          (equal (mevedel-view--input-text)
                                 "> benchmark draft\nsecond line"))
               (error "Prompt benchmark moved the composer"))
             metrics)))))))

(defun mevedel-benchmark--tool-segments (directory)
  "Measure tool-segment copying during full redraws below DIRECTORY."
  (let ((block-count 2)
        (body-bytes (* 64 1024))
        (iterations 1))
    (mevedel-benchmark--with-view
     (lambda (data-buf view-buf)
       (with-current-buffer data-buf
         (dotimes (index block-count)
           (let ((start (point)))
             (insert
              (format "#+begin_tool (Bash :command \"echo %d\")\n" index)
              (format "(:name \"Bash\" :args (:command \"echo %d\"))\n\n"
                      index)
              (make-string body-bytes (+ ?a index))
              "\n#+end_tool\n")
             (if (zerop index)
                 (put-text-property
                  start (point) 'gptel
                  (cons 'tool (format "benchmark-%d" index)))
               (put-text-property
                (+ start 46) (+ start 92) 'gptel
                (cons 'tool (format "benchmark-%d" index)))))))
       (with-current-buffer view-buf
         (goto-char (mevedel-view--input-start))
         (insert "> benchmark draft\nsecond line")
         (goto-char (+ (mevedel-view--input-start) 4))
         (set-window-point (selected-window) (point))
         (let ((workload
                (lambda ()
                  (dotimes (_ iterations)
                    (mevedel-view--full-rerender data-buf t)))))
           (let ((metrics
                  (mevedel-benchmark--measure
                   'tool-segments
                   (list :block-count block-count :body-bytes body-bytes
                         :iterations iterations)
                   workload directory
                   (lambda ()
                     (let ((original-segment
                            (symbol-function
                             'mevedel-view--tool-segment-text))
                           (original-substring
                            (symbol-function
                             'buffer-substring-no-properties))
                           (in-segment nil)
                           (segment-calls 0)
                           (substring-calls 0)
                           (substring-bytes 0))
                       (cl-letf
                           (((symbol-function
                             'mevedel-view--tool-segment-text)
                             (lambda (start end)
                               (cl-incf segment-calls)
                               (let ((previous in-segment))
                                 (unwind-protect
                                     (progn
                                       (setq in-segment t)
                                       (funcall original-segment start end))
                                   (setq in-segment previous)))))
                            ((symbol-function
                              'buffer-substring-no-properties)
                             (lambda (start end)
                               (when in-segment
                                 (cl-incf substring-calls)
                                 (cl-incf substring-bytes (- end start)))
                               (funcall original-substring start end))))
                         (funcall workload))
                       (list :tool-segment-calls segment-calls
                             :substring-calls substring-calls
                             :substring-bytes substring-bytes))))))
             (unless (and (save-excursion
                            (goto-char (point-min))
                            (search-forward "Bash" nil t))
                          (= (window-point (selected-window))
                             (+ (mevedel-view--input-start) 4))
                          (equal (mevedel-view--input-text)
                                 "> benchmark draft\nsecond line"))
               (error "Tool benchmark lost its row or composer"))
             metrics)))))))

(defun mevedel-benchmark--session (root)
  "Return a benchmark session persisted below ROOT."
  (let* ((workspace
          (mevedel-workspace--create
           :type 'benchmark :id root :root root :name "benchmark"
           :file-cache
           (mevedel-file-cache--create
            :table (make-hash-table :test #'equal)
            :order nil :total-bytes 0)))
         (session (mevedel-session-create "benchmark" workspace)))
    (setf (mevedel-session-session-id session) "benchmark"
          (mevedel-session-save-path session) root)
    session))

(defun mevedel-benchmark--telemetry-perturbation (directory)
  "Compare telemetry enabled and disabled below DIRECTORY."
  (let* ((root (file-name-concat directory "session"))
         (session (mevedel-benchmark--session root))
         (events 250)
         (log-file (file-name-concat root "telemetry-log.el")))
    (cl-labels
        ((run (enabled output)
           (let ((workload
                  (lambda ()
                    (when (file-exists-p log-file)
                      (delete-file log-file))
                    (let ((mevedel-telemetry-enabled enabled))
                      (dotimes (index events)
                        (mevedel-telemetry-record
                         session 'benchmark-event
                         :index index :outcome 'success))))))
             (mevedel-benchmark--measure
              (if enabled 'telemetry-enabled 'telemetry-disabled)
              (list :events events)
              workload output
              (lambda ()
                (funcall workload)
                (list :emitted-bytes
                      (if (file-exists-p log-file)
                          (file-attribute-size
                           (file-attributes log-file))
                        0)))))))
      (let ((enabled
             (run t (file-name-concat directory "enabled")))
            (disabled
             (run nil (file-name-concat directory "disabled"))))
        (mevedel-benchmark--write-metrics
         directory (list :enabled enabled :disabled disabled))
        (list :enabled enabled :disabled disabled)))))

(defun mevedel-benchmark--status-history (directory)
  "Measure status refresh and fallback history lookup below DIRECTORY."
  (let ((transcript-bytes (* 1024 1024))
        (status-iterations 1)
        (history-iterations 1))
    (mevedel-benchmark--with-view
     (lambda (_data-buf view-buf)
       (with-current-buffer view-buf
         (let ((inhibit-read-only t))
           (mevedel-view--call-with-render-boundaries-advancing
            (lambda ()
              (goto-char (mevedel-view--history-insertion-marker))
              (let ((start (point)))
                (insert (make-string transcript-bytes ?x))
                (put-text-property start (point)
                                   'mevedel-view-type 'response)))))
         (goto-char (mevedel-view--input-start))
         (insert "> benchmark draft\nsecond line")
         (goto-char (+ (mevedel-view--input-start) 4))
         (set-window-point (selected-window) (point))
         (let ((status
                (mevedel-benchmark--measure
                 'status-refresh
                 (list :transcript-bytes transcript-bytes
                       :iterations status-iterations)
                 (lambda ()
                   (dotimes (_ status-iterations)
                     (mevedel-view--render-status)))
                 (file-name-concat directory "status")
                 (lambda ()
                   (let ((original
                          (symbol-function
                           'mevedel-view--history-tail-position))
                         (scans 0)
                         (characters 0))
                     (cl-letf
                         (((symbol-function
                            'mevedel-view--history-tail-position)
                           (lambda ()
                             (cl-incf scans)
                             (cl-incf characters
                                      (- (point-max)
                                         (mevedel-view--after-header-position)))
                             (funcall original))))
                       (dotimes (_ status-iterations)
                         (mevedel-view--render-status)))
                     (list :status-redraw-calls status-iterations
                           :history-scan-calls scans
                           :history-scan-characters characters)))))
               (history
                (mevedel-benchmark--measure
                 'history-fallback
                 (list :transcript-bytes transcript-bytes
                       :iterations history-iterations)
                 (lambda ()
                   (dotimes (_ history-iterations)
                     (mevedel-view--history-tail-position)))
                 (file-name-concat directory "history")
                 (lambda ()
                   (list :history-scan-calls history-iterations
                         :history-scan-characters
                         (* transcript-bytes history-iterations))))))
           (unless (and (= (point) (+ (mevedel-view--input-start) 4))
                        (= (window-point (selected-window)) (point))
                        (equal (mevedel-view--input-text)
                               "> benchmark draft\nsecond line"))
             (error "Status/history benchmark moved the composer"))
           (mevedel-benchmark--write-metrics
            directory (list :status status :history history))
           (list :status status :history history)))))))

(defun mevedel-benchmark-run (scenario directory)
  "Run named SCENARIO and write its artifacts below DIRECTORY."
  (pcase scenario
    ("retry-gap" (mevedel-benchmark--retry-gap directory))
    ("prompt-redraw" (mevedel-benchmark--prompt-redraw directory))
    ("status-history" (mevedel-benchmark--status-history directory))
    ("telemetry-perturbation"
     (mevedel-benchmark--telemetry-perturbation directory))
    ("tool-segments" (mevedel-benchmark--tool-segments directory))
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
