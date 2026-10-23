;;; mevedel-profiler-optimization.el --- Profiler optimization benchmarks -*- lexical-binding: t; -*-

;;; Commentary:

;; Run deterministic workloads from a clean batch Emacs:
;;
;;   eask emacs --batch -L . -L test \
;;     -l benchmark/mevedel-profiler-optimization.el -- SCENARIO OUTPUT-DIR
;;
;; Each scenario writes metrics.el plus native CPU and memory profiler
;; artifacts below OUTPUT-DIR.  full-projection accepts an optional session
;; directory, copies it privately, and measures synchronous and drained batched
;; projection.  Compile the package and this harness before measuring it.

;;; Code:

(require 'benchmark)
(require 'cl-lib)
(require 'mevedel-file-state)
(require 'mevedel)
(require 'mevedel-structs)
(require 'mevedel-session-persistence)
(require 'mevedel-telemetry)
(require 'mevedel-view)
(require 'mevedel-view-composer)
(require 'mevedel-view-interaction)
(require 'mevedel-view-render)
(require 'mevedel-view-stream)
(require 'mevedel-view-zone)
(require 'profiler)
(require 'mevedel-view-fixtures
         (file-name-concat
          (file-name-directory
           (or load-file-name buffer-file-name byte-compile-current-file))
          "mevedel-view-fixtures"))

(defvar mevedel-session--read-only-mode)
(defvar org-persist-directory)
(defvar mevedel-benchmark--no-main nil)
(defvar mevedel-benchmark--projection-gc-threshold 800000
  "GC threshold for full projection; bind to compare a tuned editor heap.")

(setq org-persist-directory
      (file-name-as-directory
       (file-name-concat temporary-file-directory
                         "mevedel-benchmark-org-persist")))

(defconst mevedel-benchmark--warmups 2)
(defconst mevedel-benchmark--rounds 5)
(defconst mevedel-benchmark--harness-file
  (or load-file-name buffer-file-name))
(defconst mevedel-benchmark--repository-directory
  (file-name-directory
   (directory-file-name (file-name-directory mevedel-benchmark--harness-file))))

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
  (let ((default-directory mevedel-benchmark--repository-directory))
    (with-temp-buffer
      (unless (zerop (process-file "git" nil t nil
                                  "diff" "--binary" "HEAD"))
        (error "Could not read worktree diff"))
      (secure-hash 'sha256 (current-buffer)))))

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
                   (let ((default-directory mevedel-benchmark--repository-directory))
                     (car (process-lines "git" "rev-parse" "HEAD")))
                   :harness-sha256
                   (mevedel-benchmark--file-sha256
                    mevedel-benchmark--harness-file)
                   :worktree-diff-sha256
                   (mevedel-benchmark--worktree-diff-sha256)
                   :dirty-files
                   (let ((default-directory mevedel-benchmark--repository-directory))
                     (process-lines "git" "status" "--short"))
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
         (mevedel-benchmark--insert-history transcript-bytes)
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
                           'mevedel-view--transcript-history-position-p))
                         (checks 0))
                     (cl-letf
                         (((symbol-function
                            'mevedel-view--transcript-history-position-p)
                           (lambda (position)
                             (cl-incf checks)
                             (funcall original position))))
                       (dotimes (_ status-iterations)
                         (mevedel-view--render-status)))
                     (list :status-redraw-calls status-iterations
                           :history-position-checks checks)))))
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
                   (let ((original
                          (symbol-function
                           'mevedel-view--transcript-history-position-p))
                         (checks 0))
                     (cl-letf
                         (((symbol-function
                            'mevedel-view--transcript-history-position-p)
                           (lambda (position)
                             (cl-incf checks)
                             (funcall original position))))
                       (dotimes (_ history-iterations)
                         (mevedel-view--history-tail-position)))
                     (list :history-position-checks checks))))))
           (unless (and (= (point) (+ (mevedel-view--input-start) 4))
                        (= (window-point (selected-window)) (point))
                        (equal (mevedel-view--input-text)
                               "> benchmark draft\nsecond line"))
             (error "Status/history benchmark moved the composer"))
           (mevedel-benchmark--write-metrics
            directory (list :status status :history history))
           (list :status status :history history)))))))

(defun mevedel-benchmark--spinner-ticks (directory)
  "Measure running-tool spinner ticks over a large view below DIRECTORY."
  (let ((transcript-bytes (* 1024 1024))
        (iterations 100))
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
                                   'mevedel-view-type 'response))
               ;; Draw the running row as the live render does.
               (setq mevedel-view--pending-tool-calls
                     '(("benchmark-call" "Read" "benchmark.el")))
               (let ((start (point)))
                 (mevedel-view--insert-pending-run
                  mevedel-view--pending-tool-calls)
                 (mevedel-view--adopt-tool-animation-targets
                  start (point) nil)))))
         (goto-char (mevedel-view--input-start))
         (insert "> benchmark draft\nsecond line")
         (goto-char (+ (mevedel-view--input-start) 4))
         (set-window-point (selected-window) (point))
         (let* ((workload
                 (lambda ()
                   (dotimes (_ iterations)
                     (mevedel-view--spinner-tick))))
                (metrics
                 (mevedel-benchmark--measure
                  'spinner-ticks
                  (list :transcript-bytes transcript-bytes
                        :iterations iterations)
                  workload directory
                  (lambda ()
                    (let ((original
                           (symbol-function
                            'mevedel-view--refresh-spinner-frame-spans))
                          (calls 0)
                          (characters 0))
                      (cl-letf
                          (((symbol-function
                             'mevedel-view--refresh-spinner-frame-spans)
                            (lambda (property start end face)
                              (when (eq property
                                        'mevedel-view-inline-spinner-frame)
                                (cl-incf calls)
                                (cl-incf characters (- end start)))
                              (funcall original property start end face))))
                        (funcall workload))
                      (list :inline-refresh-calls calls
                            :inline-refresh-characters characters))))))
           (unless (and (= (point) (+ (mevedel-view--input-start) 4))
                        (= (window-point (selected-window)) (point))
                        (equal (mevedel-view--input-text)
                               "> benchmark draft\nsecond line"))
             (error "Spinner benchmark moved the composer"))
           metrics))))))

(defun mevedel-benchmark--tool-burst-round (data-buf view-buf batches width)
  "Run one in-flight turn of BATCHES parallel tool batches of WIDTH calls.
Each batch runs gptel's hook order: every pre-tool hook, a boundary render,
the results reaching DATA-BUF, every post-tool hook, and a boundary render.
The turn is then stopped and its VIEW-BUF and DATA-BUF text removed, so
every round starts from the same state."
  (let ((data-start (with-current-buffer data-buf (copy-marker (point-max))))
        (view-start (with-current-buffer view-buf
                      (copy-marker (mevedel-view--history-insertion-marker))))
        (serial 0))
    (cl-flet ((flush ()
                (with-current-buffer view-buf
                  (when mevedel-view--pending-render-kind
                    (when (timerp mevedel-view--render-timer)
                      (cancel-timer mevedel-view--render-timer))
                    (mevedel-view--flush-scheduled-render view-buf t)))))
      (unwind-protect
          (progn
            (with-current-buffer view-buf
              (mevedel-view-stream-begin-turn view-start data-start))
            (dotimes (_ batches)
              (let (calls)
                (dotimes (index width)
                  (cl-incf serial)
                  (push (list :name (if (cl-evenp index) "Read" "Grep")
                              :args (if (cl-evenp index)
                                        (list :file_path
                                              (format "src/file-%d.el" serial))
                                      (list :pattern
                                            (format "pattern-%d" serial)))
                              :id (format "burst-%d" serial))
                        calls))
                (setq calls (nreverse calls))
                (with-current-buffer data-buf
                  (dolist (call calls)
                    (mevedel-view-stream-pre-tool call)))
                (flush)
                (with-current-buffer data-buf
                  (dolist (call calls)
                    (let ((start (point-max)))
                      (goto-char start)
                      (insert (format "(:name %S :args %S)\n\nresult %s\n"
                                      (plist-get call :name)
                                      (plist-get call :args)
                                      (plist-get call :id)))
                      (put-text-property start (point) 'gptel
                                         (cons 'tool (plist-get call :id)))))
                  (dolist (call calls)
                    (mevedel-view-stream-post-tool call)))
                (flush))))
        (with-current-buffer view-buf
          (mevedel-view-stream-stop)
          (when (timerp mevedel-view--render-timer)
            (cancel-timer mevedel-view--render-timer))
          (setq mevedel-view--pending-render-kind nil)
          (let ((inhibit-read-only t))
            (delete-region view-start (mevedel-view--history-insertion-marker))))
        (with-current-buffer data-buf
          (delete-region data-start (point-max)))
        (set-marker view-start nil)
        (set-marker data-start nil)))))

(defun mevedel-benchmark--tool-burst (directory)
  "Measure parallel tool batches in an in-flight turn below DIRECTORY.
A 1 MiB transcript precedes the turn and the composer holds a draft."
  (let ((transcript-bytes (* 1024 1024))
        (batches 3)
        (width 4))
    (mevedel-benchmark--with-view
     (lambda (data-buf view-buf)
       (with-current-buffer view-buf
         (let ((inhibit-read-only t))
           (mevedel-view--call-with-render-boundaries-advancing
            (lambda ()
              (goto-char (mevedel-view--history-insertion-marker))
              (let ((start (point)))
                (insert (make-string transcript-bytes ?x) "\n")
                (put-text-property start (point)
                                   'mevedel-view-type 'response)))))
         (goto-char (mevedel-view--input-start))
         (insert "> benchmark draft\nsecond line")
         (goto-char (+ (mevedel-view--input-start) 4))
         (set-window-point (selected-window) (point)))
       (let* ((workload
               (lambda ()
                 (mevedel-benchmark--tool-burst-round
                  data-buf view-buf batches width)))
              (metrics
               (mevedel-benchmark--measure
                'tool-burst
                (list :transcript-bytes transcript-bytes
                      :batches batches :width width)
                workload directory
                (lambda ()
                  (let ((reconcile (symbol-function 'mevedel-view-zone-reconcile))
                        (live (symbol-function 'mevedel-view--render-live-region))
                        (reconciles 0)
                        (live-renders 0)
                        (tick (with-current-buffer view-buf
                                (buffer-modified-tick))))
                    (cl-letf (((symbol-function 'mevedel-view-zone-reconcile)
                               (lambda (&rest args)
                                 (cl-incf reconciles)
                                 (apply reconcile args)))
                              ((symbol-function 'mevedel-view--render-live-region)
                               (lambda (&rest args)
                                 (cl-incf live-renders)
                                 (apply live args))))
                      (funcall workload))
                    (list :zone-reconciles reconciles
                          :live-renders live-renders
                          :view-modified-ticks
                          (- (with-current-buffer view-buf
                               (buffer-modified-tick))
                             tick)))))))
         (with-current-buffer view-buf
           (unless (and (= (point) (+ (mevedel-view--input-start) 4))
                        (equal (mevedel-view--input-text)
                               "> benchmark draft\nsecond line"))
             (error "Tool burst benchmark moved the composer")))
         metrics)))))

(defun mevedel-benchmark--projection-history ()
  "Insert forty deterministic mixed exchanges into the current data buffer."
  (dotimes (index 40)
    (insert (format "* Prompt %d\nExplain the change and check its invariants.\n\n" index))
    (let ((start (point)))
      (insert "#+begin_reasoning\nCompare the alternatives, preserve source identity, and verify the result.\n#+end_reasoning\n")
      (put-text-property start (point) 'gptel 'reasoning))
    (dotimes (tool 3)
      (let ((start (point)))
        (insert (format "#+begin_tool (Read :file_path \"src/file-%d-%d.el\")\n(:name \"Read\" :args (:file_path \"src/file-%d-%d.el\"))\n\n" index tool index tool)
                (make-string 2048 (+ ?a tool)) "\n#+end_tool\n")
        (put-text-property start (point) 'gptel
                           (cons 'tool (format "projection-%d-%d" index tool)))))
    (let ((start (point)))
      (insert (format "## Result %d\n\nThe implementation preserves **source identity** and keeps the draft intact.\n\n" index))
      (dotimes (_ 8)
        (insert "- Check the bounded scan and its _unchanged_ suffix.\n- Keep the observable result, not a second durable model.\n\n```elisp\n(+ 1 2)\n```\n\n"))
      (put-text-property start (point) 'gptel 'response))
    (insert "\n\n")))

(defun mevedel-benchmark--projection-drain ()
  "Start scheduled full projection and drive its callbacks to completion.
No sleeps or timer dispatch are needed.  Fail on recovery to synchronous work."
  (let ((steps 0))
    (cl-letf (((symbol-function 'mevedel-view-render--full-now)
               (lambda (&rest _) (error "Batched projection fell back"))))
      (mevedel-view-render-batched-full)
      (while mevedel-view-render--batch
        (when (> (cl-incf steps) 100000)
          (error "Batched projection did not finish"))
        (mevedel-view-render--batch-step
         (current-buffer) mevedel-view-render--batch)))
    steps))

(defconst mevedel-benchmark--projection-stages
  '((mevedel-transcript-restore-properties . restore)
    (mevedel-transcript-segments . scan)
    (mevedel-transcript-scan-start . scan)
    (mevedel-transcript-scan-step . scan)
    (mevedel-transcript-scan-result . scan)
    (mevedel-view--group-transcript-turns . group)
    (mevedel-view--group-into-turns . group)
    (mevedel-view-render--annotate-directive-turn . group)
    (mevedel-view--directive-ranges . group)
    (mevedel-view--directive-boundary-segment-p . group)
    (mevedel-view--render-turn . turn)
    (mevedel-view--fontify-response . response-cache)
    (mevedel-view-render--plan-fontify-step . response-cache)
    (mevedel-view--clean-reasoning-text . reasoning-cache)
    (mevedel-view--segment-rendering . tool-cache)
    (mevedel-view-render--plan-tool-step . tool-cache)
    (mevedel-view--fontify-as . markdown)
    (mevedel-view--markdown-fontify-job-start . markdown)
    (mevedel-view--markdown-fontify-job-step . markdown)
    (mevedel-view--markdown-fontify-job-result . markdown)
    (mevedel-view--insert-rendered-tool . insertion)
    (mevedel-view-render-add-display-properties . insertion))
  "Instrumented boundaries; nested times are charged only to the inner stage.
Cache categories include lookup and misses, not just successful cache hits.")

(defun mevedel-benchmark--projection-attribution (workload &optional stages)
  "Time WORKLOAD with nested exclusive timing wrappers around STAGES.
Return seconds and call counts.  Insertion helpers are measured inside turns;
inline bytecode insertion remains in turn time.  Publication is the
unattributed remainder, including preservation, reset, and batch bookkeeping."
  (let ((times (make-hash-table)) (turn-times (make-hash-table))
        (counts (make-hash-table)) (turn-depth 0) (cache-depth 0)
        (uncached-turn 0.0) (cached-turn 0.0)
        (category 'publication) (since (float-time))
        (stages (or stages mevedel-benchmark--projection-stages)))
    (cl-labels
        ((charge ()
           (let ((now (float-time)))
             (puthash category (+ (gethash category times 0) (- now since)) times)
             (when (> turn-depth 0)
               (puthash category (+ (gethash category turn-times 0) (- now since))
                        turn-times)
               (if (> cache-depth 0)
                   (cl-incf cached-turn (- now since))
                 (cl-incf uncached-turn (- now since))))
             (setq since now)))
         (install (rest)
           (if (null rest)
               (progn
                 (setq since (float-time))
                 (prog1 (funcall workload) (charge)))
             (let* ((symbol (caar rest)) (kind (cdar rest))
                    (original (symbol-function symbol)))
               (cl-letf (((symbol-function symbol)
                          (lambda (&rest args)
                            (if (and (eq kind 'insertion) (zerop turn-depth))
                                (apply original args)
                              (charge)
                              (let ((parent category)
                                    (depth turn-depth)
                                    (cached-depth cache-depth))
                                (setq category kind)
                                (when (eq kind 'turn) (cl-incf turn-depth))
                                (when (memq kind '(response-cache reasoning-cache tool-cache))
                                  (cl-incf cache-depth))
                                (puthash kind (1+ (gethash kind counts 0)) counts)
                                (unwind-protect (apply original args)
                                  (charge)
                                  (setq category parent turn-depth depth
                                        cache-depth cached-depth)))))))
                 (install (cdr rest)))))))
      ;; Installing and restoring wrappers is not part of projection time.
      (install stages))
    (let (seconds inside-turn calls)
      (maphash (lambda (key value) (push (cons key value) seconds)) times)
      (maphash (lambda (key value) (push (cons key value) inside-turn)) turn-times)
      (maphash (lambda (key value) (push (cons key value) calls)) counts)
      (list :seconds seconds :inside-turn-seconds inside-turn :calls calls
            :turn-uncached-seconds uncached-turn
            :turn-cached-seconds cached-turn))))

(defun mevedel-benchmark--projection-measure (data view directory parameters)
  "Measure DATA's projection in VIEW below DIRECTORY with PARAMETERS."
  (with-current-buffer view
    (setq parameters
          (append parameters
                  (list :gc-cons-threshold gc-cons-threshold
                        :gc-cons-percentage gc-cons-percentage
                        :markdown-mode (mevedel-view--markdown-fontify-mode)
                        :window-width (window-body-width)
                        :segment-counts
                        (let ((counts (make-hash-table)))
                          (with-current-buffer data
                            (dolist (segment (mevedel-transcript-segments
                                              (point-min) (point-max)))
                              (cl-incf (gethash (car segment) counts 0))))
                          (let (result)
                            (maphash (lambda (key value)
                                       (push (cons key value) result)) counts)
                            result)))))
    (goto-char (mevedel-view--input-start))
    (insert "> benchmark draft\nsecond line")
    (goto-char (+ (mevedel-view--input-start) 4))
    (set-window-point (selected-window) (point))
    (let ((mevedel-benchmark--rounds 7) results reference)
      (dolist (path '(synchronous batched))
        (let* ((workload (if (eq path 'synchronous)
                             (lambda () (mevedel-view--full-rerender data))
                           #'mevedel-benchmark--projection-drain))
               (metrics
                (mevedel-benchmark--measure
                 'full-projection (append parameters (list :path path))
                 workload (file-name-concat directory (symbol-name path))
                 (lambda ()
                   (let (rounds)
                     (dotimes (_ 7)
                       (garbage-collect)
                       (push (mevedel-benchmark--projection-attribution workload)
                             rounds))
                     (list :attribution-rounds (nreverse rounds)))))))
          (unless (and (equal (mevedel-view--input-text)
                              "> benchmark draft\nsecond line")
                       (= (point) (+ (mevedel-view--input-start) 4)))
            (error "Projection benchmark moved the composer"))
          (let ((text (buffer-substring-no-properties
                       (point-min) mevedel-view--input-marker)))
            (when (and reference (not (equal text reference)))
              (error "Batched and synchronous projection differ"))
            (setq reference text)
            (setq metrics (append metrics (list :history-sha256
                                               (secure-hash 'sha256 text)))))
          (mevedel-benchmark--write-metrics
           (file-name-concat directory (symbol-name path)) metrics)
          (push (cons path metrics) results)))
      (mevedel-benchmark--write-metrics directory (nreverse results)))))

(defun mevedel-benchmark--projection-restored (directory source)
  "Measure a private copy of session directory SOURCE below DIRECTORY.
Use the production restore entry point with a temporary project workspace.
Force read-only acquisition: no lease or repair writes, even on the copy."
  (let* ((root (make-temp-file "mevedel-projection-" t))
         (copy (file-name-concat root "session"))
         (workspace (mevedel-workspace--create
                     :type 'project :id root :root root :name "projection"))
         data view session)
    (unwind-protect
        (progn
          (copy-directory source copy nil t t)
          ;; A copied project keeps its durable identity, not its original root.
          (let* ((publication (mevedel-session-publication-read copy))
                 (sidecar (mevedel-session-persistence-load-sidecar
                           (plist-get publication :sidecar)))
                 (identity (plist-get (plist-get sidecar :workspace) :workspace-id))
                 (identity-path (file-name-concat root ".mevedel" "workspace-id")))
            (make-directory (file-name-directory identity-path) t)
            (with-temp-file identity-path (insert identity "\n")))
          (cl-letf (((symbol-function 'mevedel-session-persistence-lock-acquire)
                     (lambda (&rest _) nil)))
            (setq data (mevedel-session-persistence-restore
                        copy "benchmark" nil workspace)))
          (setq session (buffer-local-value 'mevedel--session data)
                view (buffer-local-value 'mevedel--view-buffer data))
          (save-window-excursion
            (switch-to-buffer view)
            (mevedel-benchmark--projection-measure
             data view directory
             (list :fixture 'restored :session-id (mevedel-session-session-id session)
                   :segment (mevedel-session-current-segment session)
                   :characters (with-current-buffer data (buffer-size))
                   :source-sha256 (with-current-buffer data
                                    (secure-hash 'sha256 (current-buffer)))))))
      (dolist (buffer (buffer-list))
        (when (or (eq buffer view)
                  (and session (eq (buffer-local-value 'mevedel--session buffer)
                                   session)))
          (with-current-buffer buffer (set-buffer-modified-p nil))
          (kill-buffer buffer)))
      (delete-directory root t))))

(defun mevedel-benchmark--full-projection (directory &optional session-directory)
  "Measure full projection below DIRECTORY, optionally from SESSION-DIRECTORY."
  (when (interpreted-function-p (symbol-function 'mevedel-view--render-turn))
    (error "Full projection requires byte-compiled package code"))
  (let ((gc-cons-threshold mevedel-benchmark--projection-gc-threshold)
        (gc-cons-percentage 0.1))
    (if session-directory
        (mevedel-benchmark--projection-restored directory session-directory)
      (mevedel-benchmark--with-view
       (lambda (data view)
         (with-current-buffer data (mevedel-benchmark--projection-history))
         (mevedel-benchmark--projection-measure
          data view directory (list :fixture 'synthetic :exchanges 40
                                    :characters (with-current-buffer data
                                                  (buffer-size)))))))))

(defun mevedel-benchmark-run (scenario directory &optional session-directory)
  "Run named SCENARIO below DIRECTORY, optionally using SESSION-DIRECTORY."
  (pcase scenario
    ("retry-gap" (mevedel-benchmark--retry-gap directory))
    ("prompt-redraw" (mevedel-benchmark--prompt-redraw directory))
    ("spinner-ticks" (mevedel-benchmark--spinner-ticks directory))
    ("status-history" (mevedel-benchmark--status-history directory))
    ("telemetry-perturbation"
     (mevedel-benchmark--telemetry-perturbation directory))
    ("tool-segments" (mevedel-benchmark--tool-segments directory))
    ("tool-burst" (mevedel-benchmark--tool-burst directory))
    ("full-projection"
     (mevedel-benchmark--full-projection directory session-directory))
    (_ (error "Unknown benchmark scenario: %s" scenario))))

(when (and noninteractive (not mevedel-benchmark--no-main))
  (when (equal (car command-line-args-left) "--")
    (pop command-line-args-left))
  (let ((scenario (pop command-line-args-left))
        (directory (pop command-line-args-left))
        (session (pop command-line-args-left)))
    (unless (and scenario directory (null command-line-args-left)
                 (or (null session) (equal scenario "full-projection")))
      (error "Usage: ... -- SCENARIO OUTPUT-DIR [SESSION-DIRECTORY]"))
    (prin1 (mevedel-benchmark-run scenario (expand-file-name directory) session))
    (terpri)))

(provide 'mevedel-profiler-optimization)
;;; mevedel-profiler-optimization.el ends here
