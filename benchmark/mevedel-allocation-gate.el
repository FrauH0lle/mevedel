;;; mevedel-allocation-gate.el --- Compiled view allocation budgets -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with `eask run script bench-gate'.  Fixture preparation and assertions
;; are outside the measured interval.  The gate is cons-based; other counters
;; are diagnostic.  Timings are not acceptance criteria.

;;; Code:

(require 'cl-lib)
(require 'mevedel-view-fixtures
         (file-name-concat
          (file-name-directory (or load-file-name buffer-file-name))
          "mevedel-view-fixtures"))
(require 'mevedel-view-markdown)
(require 'mevedel-view-zone)

(defconst mevedel-gate--budgets
  '((stream-chunk . 964) (composer-keystroke . 150)
    (zone-tick . 1832))
  "Maximum calibrated conses per operation, baseline plus fixed margin.")

(defconst mevedel-gate--draft "> benchmark draft\nsecond line"
  "Multiline composer used by every hot-path fixture.")

(defun mevedel-gate--raw (function)
  "Return allocation counters for one call to FUNCTION after a full GC."
  (garbage-collect)
  (let ((gc-before gcs-done)
        (before (memory-use-counts)))
    (funcall function)
    (let ((after (memory-use-counts)))
      (list :conses (- (nth 0 after) (nth 0 before))
            :string-chars (- (nth 4 after) (nth 4 before))
            :vector-cells (- (nth 2 after) (nth 2 before))
            :gcs (- gcs-done gc-before)))))

(defun mevedel-gate--net (sample overhead)
  "Subtract OVERHEAD from SAMPLE counters, clamping each result at zero."
  (cl-loop for key in '(:conses :string-chars :vector-cells :gcs)
           append (list key (max 0 (- (plist-get sample key)
                                     (plist-get overhead key))))))

(defun mevedel-gate-measure (function)
  "Measure FUNCTION net of the minimum of three no-op counter samples."
  (let ((calibration (cl-loop repeat 3 collect (mevedel-gate--raw #'ignore))))
    (mevedel-gate--net
     (mevedel-gate--raw function)
     (cl-loop for key in '(:conses :string-chars :vector-cells :gcs)
              append (list key (apply #'min
                                      (mapcar (lambda (sample)
                                                (plist-get sample key))
                                              calibration)))))))

(defun mevedel-gate-check (scenario sample budget)
  "Signal an error if SCENARIO's SAMPLE conses exceed BUDGET."
  (when (> (plist-get sample :conses) budget)
    (error "Allocation budget breached: %s: %d conses > %d budget"
           scenario (plist-get sample :conses) budget)))

(defun mevedel-gate--assert-compiled ()
  "Refuse interpreted hot paths, fixtures, or measurement helpers."
  (dolist (symbol '(mevedel-gate--raw mevedel-gate-measure
                    mevedel-benchmark--with-view
                    mevedel-view--render-stream-update
                    mevedel-view-render-live-update
                    mevedel-view--spinner-tick
                    mevedel-view-zone-reconcile
                    mevedel-view--realign-on-window-change))
    (unless (byte-code-function-p (symbol-function symbol))
      (error "Benchmark requires byte-compiled function: %s" symbol))))

(defun mevedel-gate--with-scenario (scenario consumer)
  "Prepare SCENARIO and call CONSUMER with its update function.
Check the actual output after CONSUMER returns, outside the measurement.
Every invocation owns a fresh view; warmups cannot grow the measured turn."
  (let* ((now (seconds-to-time 1000.25))
         (time-function (symbol-function 'current-time))
         (float (symbol-function 'float-time))
         (mevedel-view-spinner-style 'static)
         (mevedel-view-tool-spinner-style 'static)
         (mevedel-view-spinner-power-policy 'full))
    (cl-letf (((symbol-function 'current-time) (lambda () now))
              ((symbol-function 'float-time)
               (lambda (&optional time) (funcall float (or time now)))))
      (mevedel-benchmark--with-view
       (lambda (data view)
         (with-current-buffer view
           (mevedel-benchmark--insert-history (* 1024 1024))
           (goto-char (mevedel-view--input-start))
           (insert mevedel-gate--draft)
           (goto-char (+ (mevedel-view--input-start) 4))
           (set-window-point (selected-window) (point))
           (let ((offset 4)
                 (chunk " fixed stream chunk.")
                 (reconciles 0)
                 (reconcile (symbol-function 'mevedel-view-zone-reconcile))
                 update result)
             (pcase scenario
               ('stream-chunk
                (mevedel-view-stream-begin-turn
                 (mevedel-view--history-insertion-marker)
                 (with-current-buffer data (point-max)))
                (with-current-buffer data
                  (let ((start (point-max)))
                    (goto-char start)
                    (insert "Initial response.")
                    (put-text-property start (point) 'gptel 'response)))
                (mevedel-view--render-stream-update data)
                (with-current-buffer data
                  (let ((start (point-max)))
                    (goto-char start)
                    (insert chunk)
                    (put-text-property start (point) 'gptel 'response)))
                (setq update (lambda () (mevedel-view--render-stream-update data))))
               ('composer-keystroke
                (unless (memq #'mevedel-view--realign-on-window-change
                              post-command-hook)
                  (error "Composer fixture is missing its realignment hook"))
                ;; Prime the fixed window geometry before the typed command.
                (run-hooks 'post-command-hook)
                (setq update
                      (lambda ()
                        (let ((last-command-event ?x)
                              (this-command 'self-insert-command))
                          (self-insert-command 1)
                          (run-hooks 'post-command-hook))))
                (setq offset 5))
               ('zone-tick
                (mevedel-view--start-spinner "Working...")
                (setq mevedel-view--spinner-start-time (seconds-to-time 988.25))
                (mevedel-view--ensure-request-progress)
                (setq mevedel-view--spinner-last-second 1000)
                (setq now (seconds-to-time 1001.25))
                (setq update #'mevedel-view--spinner-tick))
               (_ (error "Unknown allocation scenario: %s" scenario)))
             (set-window-start
              (selected-window)
              (save-excursion
                (goto-char (mevedel-view--input-start))
                (forward-line -4)
                (point)))
             (redisplay t)
             (let ((tick (buffer-modified-tick)))
               (cl-letf (((symbol-function 'mevedel-view-zone-reconcile)
                          (lambda (&rest args)
                            (cl-incf reconciles)
                            (apply reconcile args))))
                 ;; Keep benchmark timers on the real clock.  Only the update
                 ;; itself sees the deterministic semantic-tick clock.
                 (let ((operation update))
                   (setq update
                         (lambda ()
                           (cl-letf (((symbol-function 'current-time)
                                      (lambda () now))
                                     ((symbol-function 'float-time)
                                      (lambda (&optional time)
                                        (funcall float (or time now)))))
                             (funcall operation)))))
                 (cl-letf (((symbol-function 'current-time) time-function)
                           ((symbol-function 'float-time) float))
                   (setq result (funcall consumer update))))
               (unless (> (buffer-modified-tick) tick)
                 (error "Scenario did not modify the view: %s" scenario)))
             (unless (and (= (point) (+ (mevedel-view--input-start) offset))
                          (equal (mevedel-view--input-text)
                                 (if (eq scenario 'composer-keystroke)
                                     "> bexnchmark draft\nsecond line"
                                   mevedel-gate--draft)))
               (error "Scenario corrupted the composer: %s" scenario))
             (pcase scenario
               ('stream-chunk
                (unless (save-excursion
                          (goto-char (- (mevedel-view--history-insertion-marker) 100))
                          (search-forward chunk
                                          (mevedel-view--history-insertion-marker) t))
                  (error "Stream fixture skipped the appended chunk")))
               ('zone-tick
                (unless (and (> reconciles 0)
                             (save-excursion
                               (goto-char (- (mevedel-view--input-start) 100))
                               (search-forward "13s" (mevedel-view--input-start) t)))
                  (error "Zone tick skipped elapsed metadata reconciliation"))))
             result)))))))

(defun mevedel-allocation-gate-run ()
  "Warm and measure three hot paths, failing on any cons budget breach."
  (mevedel-gate--assert-compiled)
  (let ((gc-cons-threshold (* 16 1024 1024))
        (gc-cons-percentage 0.1))
    (set-frame-size (selected-frame) 80 25)
    (dolist (entry mevedel-gate--budgets)
      (dotimes (_ 5)
        (mevedel-gate--with-scenario (car entry) #'funcall))
      (let ((sample (mevedel-gate--with-scenario
                     (car entry) #'mevedel-gate-measure)))
        (princ (format "%s %S budget=%d\n" (car entry) sample (cdr entry)))
        (mevedel-gate-check (car entry) sample (cdr entry))))))

(provide 'mevedel-allocation-gate)
;;; mevedel-allocation-gate.el ends here
