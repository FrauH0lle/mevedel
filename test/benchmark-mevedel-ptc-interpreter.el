;;; benchmark-mevedel-ptc-interpreter.el --- ToolScript evaluator measurements -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Repeatable, non-ERT measurements for comparing ToolScript evaluator
;; implementations.  Timing is reported rather than asserted because it
;; depends on the host and current Emacs load.
;;
;; Source-loaded, after `eask clean elc':
;;
;;   emacs --batch -Q -L . -l test/benchmark-mevedel-ptc-interpreter.el \
;;     --funcall benchmark-mevedel-ptc-run
;;
;; Byte-compiled, after `eask compile': use the same command.

;;; Code:

(require 'cl-lib)
(require 'mevedel-ptc-driver)

(defvar gc-elapsed)
(defvar gcs-done)

(defconst benchmark-mevedel-ptc--sample-count 3
  "Number of samples collected for each workload.")

(defun benchmark-mevedel-ptc--allocation-delta (before after)
  "Return named allocation-counter differences from BEFORE to AFTER."
  (let ((names '(:conses :floats :vector-cells :symbols
                 :string-chars :intervals :strings))
        result)
    (while names
      (setq result
            (append result (list (pop names) (- (pop after) (pop before))))))
    result))

(defun benchmark-mevedel-ptc--drive (script &optional roster tool-function)
  "Run SCRIPT and return timing facts using canned TOOL-FUNCTION results."
  (garbage-collect)
  (let ((state (mevedel-ptc-start script roster))
        (resume nil)
        (pauses 0)
        (max-step 0.0)
        (started (float-time))
        (started-gcs gcs-done)
        (started-gc-time gc-elapsed)
        (started-allocations (memory-use-counts))
        steps
        result)
    (unwind-protect
        (catch 'done
          (while t
            (let* ((step-started (float-time))
                   (step (mevedel-ptc-step state resume)))
              (setq max-step (max max-step (- (float-time) step-started)))
              (setq resume nil)
              (pcase (car step)
                (:pause (setq pauses (1+ pauses)))
                (:tool
                 (setq resume
                       (funcall tool-function (nth 1 step) (nth 2 step))))
                (:done (setq result (nth 1 step)) (throw 'done nil))
                (:error (error "%s" (nth 2 step)))))))
      (setq steps (mevedel-ptc-state-steps state))
      (mevedel-ptc-close state))
    (list :seconds (- (float-time) started)
          :max-step-ms (* 1000 max-step)
          :steps steps
          :pauses pauses
          :gcs (- gcs-done started-gcs)
          :gc-seconds (- gc-elapsed started-gc-time)
          :allocations
          (benchmark-mevedel-ptc--allocation-delta
           started-allocations (memory-use-counts))
          :result result)))

(defun benchmark-mevedel-ptc--event-loop-drive (script expected)
  "Run SCRIPT through the ToolScript driver, requiring EXPECTED.
Measure timer service while the driver runs."
  (garbage-collect)
  (let* ((started (float-time))
         (last-probe started)
         (max-probe-gap 0.0)
         (started-gcs gcs-done)
         (started-gc-time gc-elapsed)
         (started-allocations (memory-use-counts))
         (deadline (+ started 120.0))
         probe-timer probes done result)
    (setq probe-timer
          (run-at-time
           0.005 0.005
           (lambda ()
             (let ((now (float-time)))
               (setq max-probe-gap (max max-probe-gap (- now last-probe))
                     last-probe now
                     probes (1+ (or probes 0)))))))
    (unwind-protect
        (progn
          (mevedel-ptc-driver-run
           (lambda (value) (setq result value done t)) script nil)
          (while (and (not done) (< (float-time) deadline))
            (accept-process-output nil 0.01))
          (unless done
            (error "ToolScript driver benchmark timed out")))
      (when (timerp probe-timer)
        (cancel-timer probe-timer)))
    (setq max-probe-gap
          (max max-probe-gap (- (float-time) last-probe)))
    (unless (and (eq (plist-get result :status) 'success)
                 (equal (plist-get result :result) expected))
      (error "ToolScript driver workload returned %S" result))
    (list :seconds (- (float-time) started)
          :max-probe-gap-ms (* 1000 max-probe-gap)
          :probes (or probes 0)
          :gcs (- gcs-done started-gcs)
          :gc-seconds (- gc-elapsed started-gc-time)
          :allocations
          (benchmark-mevedel-ptc--allocation-delta
           started-allocations (memory-use-counts)))))

(defun benchmark-mevedel-ptc--samples (function)
  "Call FUNCTION repeatedly and return its measurement samples."
  (let (samples)
    (dotimes (_ benchmark-mevedel-ptc--sample-count)
      (push (funcall function) samples))
    (nreverse samples)))

(defun benchmark-mevedel-ptc--median (values)
  "Return the median number in VALUES."
  (nth (/ (length values) 2) (sort (copy-sequence values) #'<)))

(defun benchmark-mevedel-ptc--summary (samples keys)
  "Return raw SAMPLES and medians for numeric plist KEYS."
  (let ((summary (list :runs samples)))
    (dolist (key keys)
      (setq summary
            (append summary
                    (list (intern (format ":median-%s"
                                          (substring (symbol-name key) 1)))
                          (benchmark-mevedel-ptc--median
                           (mapcar (lambda (sample) (plist-get sample key))
                                   samples))))))
    summary))

(defun benchmark-mevedel-ptc--checked-drive
    (script expected &optional roster tool-function result-shape)
  "Measure SCRIPT, requiring EXPECTED after optional RESULT-SHAPE."
  (let* ((measurement
          (benchmark-mevedel-ptc--drive script roster tool-function))
         (actual (plist-get measurement :result)))
    (unless (equal expected (if result-shape (funcall result-shape actual) actual))
      (error "Workload returned %S" actual))
    (setf (plist-get measurement :result) expected)
    measurement))

(defun benchmark-mevedel-ptc--loop-script (limit)
  "Return the counter loop whose guest-form cost is 8 + 7 * LIMIT."
  (format "(let ((n 0)) (while (< n %d) (setq n (+ n 1))) n)" limit))

(defun benchmark-mevedel-ptc--recorded-workload ()
  "Return the successful defcustom-counting workload and canned Grep text."
  (let ((script
         "(let* ((root \"/workspace/\")
                 (lines (split-string
                         (Grep :pattern \"^[(]defcustom[ )]\"
                               :path \".\" :glob \"*.el\"
                               :output_mode \"count\" :head_limit 0)
                         \"\\n\" t))
                 (out nil))
            (dolist (raw lines)
              (when (string-prefix-p root raw)
                (let ((rest (substring raw (length root))))
                  (unless (string-search \"/\" rest)
                    (push rest out)))))
            (reverse out))")
        (lines nil))
    (dotimes (index 60)
      (push (format "/workspace/file-%02d.el:%d" (1+ index)
                    (1+ (% index 9)))
            lines))
    (dotimes (index 10)
      (push (format "/workspace/test/test-%02d.el:1" index) lines))
    (cons script (string-join (nreverse lines) "\n"))))

(defun benchmark-mevedel-ptc-run ()
  "Run and print the ToolScript evaluator comparison workloads."
  (let* ((small-script (benchmark-mevedel-ptc--loop-script 320))
         (stress-script (benchmark-mevedel-ptc--loop-script 71427))
         (recorded (benchmark-mevedel-ptc--recorded-workload))
         (small
          (benchmark-mevedel-ptc--samples
           (lambda ()
             (benchmark-mevedel-ptc--checked-drive small-script 320))))
         (stress
          (benchmark-mevedel-ptc--samples
           (lambda ()
             (benchmark-mevedel-ptc--checked-drive stress-script 71427))))
         (event-loop
          (benchmark-mevedel-ptc--samples
           (lambda ()
             (benchmark-mevedel-ptc--event-loop-drive stress-script "71427"))))
         (real
          (benchmark-mevedel-ptc--samples
           (lambda ()
             (benchmark-mevedel-ptc--checked-drive
              (car recorded) 60 '("Grep")
              (lambda (name _args)
                (unless (equal name "Grep")
                  (error "Unexpected tool request: %s" name))
                (cdr recorded))
              #'length)))))
    (prin1 (list :emacs-version emacs-version
                 :system system-configuration
                 :compiled (compiled-function-p
                            (symbol-function 'mevedel-ptc-step))
                 :samples benchmark-mevedel-ptc--sample-count
                 :small
                 (benchmark-mevedel-ptc--summary
                  small '(:seconds :max-step-ms :gc-seconds))
                 :stress
                 (benchmark-mevedel-ptc--summary
                  stress '(:seconds :max-step-ms :gc-seconds))
                 :event-loop
                 (benchmark-mevedel-ptc--summary
                  event-loop '(:seconds :max-probe-gap-ms :gc-seconds))
                 :recorded
                 (benchmark-mevedel-ptc--summary
                  real '(:seconds :max-step-ms :gc-seconds))))
    (terpri)))

(provide 'benchmark-mevedel-ptc-interpreter)
;;; benchmark-mevedel-ptc-interpreter.el ends here
