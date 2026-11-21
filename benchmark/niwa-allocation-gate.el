;;; niwa-allocation-gate.el --- Compiled view allocation budgets -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with `eask run script bench-gate'.  Fixture preparation and assertions
;; are outside the measured interval.  The gate is cons-based; other counters
;; are diagnostic.  Timings are not acceptance criteria.

;;; Code:

(require 'cl-lib)
(require 'niwa-view-fixtures
         (file-name-concat
          (file-name-directory (or load-file-name buffer-file-name))
          "niwa-view-fixtures"))
(require 'niwa-view-markdown)
(require 'niwa-view-zone)

(defconst niwa-gate--budgets
  '((stream-chunk . 964) (composer-keystroke . 150)
    (zone-tick . 1832))
  "Maximum calibrated conses per operation, baseline plus fixed margin.")

(defconst niwa-gate--draft "> benchmark draft\nsecond line"
  "Multiline composer used by every hot-path fixture.")

(defun niwa-gate--raw (function)
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

(defun niwa-gate--net (sample overhead)
  "Subtract OVERHEAD from SAMPLE counters, clamping each result at zero."
  (cl-loop for key in '(:conses :string-chars :vector-cells :gcs)
           append (list key (max 0 (- (plist-get sample key)
                                     (plist-get overhead key))))))

(defun niwa-gate-measure (function)
  "Measure FUNCTION net of the minimum of three no-op counter samples."
  (let ((calibration (cl-loop repeat 3 collect (niwa-gate--raw #'ignore))))
    (niwa-gate--net
     (niwa-gate--raw function)
     (cl-loop for key in '(:conses :string-chars :vector-cells :gcs)
              append (list key (apply #'min
                                      (mapcar (lambda (sample)
                                                (plist-get sample key))
                                              calibration)))))))

(defun niwa-gate-check (scenario sample budget)
  "Signal an error if SCENARIO's SAMPLE conses exceed BUDGET."
  (when (> (plist-get sample :conses) budget)
    (error "Allocation budget breached: %s: %d conses > %d budget"
           scenario (plist-get sample :conses) budget)))

(defun niwa-gate--assert-compiled ()
  "Refuse interpreted hot paths, fixtures, or measurement helpers."
  (dolist (symbol '(niwa-gate--raw niwa-gate-measure
                    niwa-benchmark--with-view
                    niwa-view--render-stream-update
                    niwa-view-render-live-update
                    niwa-view--spinner-tick
                    niwa-view--write-request-rail
                    niwa-view-zone-reconcile
                    niwa-view--realign-on-window-change))
    (unless (byte-code-function-p (symbol-function symbol))
      (error "Benchmark requires byte-compiled function: %s" symbol))))

(defun niwa-gate--with-scenario (scenario consumer)
  "Prepare SCENARIO and call CONSUMER with its update function.
Check the actual output after CONSUMER returns, outside the measurement.
Every invocation owns a fresh view; warmups cannot grow the measured turn."
  (let* ((now (seconds-to-time 1000.25))
         (time-function (symbol-function 'current-time))
         (float (symbol-function 'float-time))
         (niwa-view-spinner-style 'static)
         (niwa-view-tool-spinner-style 'static)
         (niwa-view-spinner-power-policy 'full))
    (cl-letf (((symbol-function 'current-time) (lambda () now))
              ((symbol-function 'float-time)
               (lambda (&optional time) (funcall float (or time now)))))
      (niwa-benchmark--with-view
       (lambda (data view)
         (with-current-buffer view
           (niwa-benchmark--insert-history (* 1024 1024))
           (niwa-view-composer-insert niwa-gate--draft)
           (let* ((composer-window (niwa-view-composer-display (selected-window)))
                  (composer (niwa-view-composer-buffer view))
                  (target (if (eq scenario 'stream-chunk) view composer))
                  (offset 4)
                  (chunk " fixed stream chunk.")
                  (writes 0)
                  (writer-symbol (if (eq scenario 'zone-tick)
                                     'niwa-view--write-request-rail
                                   'niwa-view-zone-reconcile))
                  (writer (symbol-function writer-symbol))
                  update result)
             (unless (and composer-window (eq (window-buffer composer-window) composer))
               (error "Fixture composer is not displayed"))
             (set-window-point composer-window (+ (niwa-view-composer-draft-start) offset))
             (pcase scenario
               ('stream-chunk
                (niwa-view-stream-begin-turn
                 (niwa-view--history-insertion-marker)
                 (with-current-buffer data (point-max)))
                (with-current-buffer data
                  (let ((start (point-max)))
                    (goto-char start)
                    (insert "Initial response.")
                    (put-text-property start (point) 'gptel 'response)))
                (niwa-view--render-stream-update data)
                (with-current-buffer data
                  (let ((start (point-max)))
                    (goto-char start)
                    (insert chunk)
                    (put-text-property start (point) 'gptel 'response)))
                (setq update (lambda () (niwa-view--render-stream-update data))))
               ('composer-keystroke
                (with-selected-window composer-window
                  (run-hooks 'post-command-hook))
                (setq update
                      (lambda ()
                        (let ((last-command-event ?x)
                              (this-command 'self-insert-command))
                          (self-insert-command 1)
                          (run-hooks 'post-command-hook))))
                (setq offset 5))
               ('zone-tick
                (niwa-view--start-spinner "Working...")
                (setq niwa-view--spinner-start-time (seconds-to-time 988.25))
                (niwa-view--ensure-request-progress)
                (setq niwa-view--spinner-last-second 1000)
                (setq now (seconds-to-time 1001.25))
                (setq update (lambda ()
                               (with-current-buffer view
                                 (niwa-view--spinner-tick)))))
               (_ (error "Unknown allocation scenario: %s" scenario)))
             (set-window-start
              (selected-window)
              (save-excursion
                (goto-char (niwa-view-transcript-tail))
                (forward-line -4)
                (point)))
             (redisplay t)
             (let ((tick (buffer-modified-tick target)))
               (cl-letf (((symbol-function writer-symbol)
                          (lambda (&rest args)
                            (cl-incf writes)
                            (apply writer args))))
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
                   (with-selected-window composer-window
                     (with-current-buffer target
                       (setq result (funcall consumer update))))))
               (unless (> (buffer-modified-tick target) tick)
                 (error "Scenario did not modify its target buffer: %s" scenario)))
             (unless (and (= (with-current-buffer composer (point))
                             (+ (niwa-view-composer-draft-start) offset))
                          (equal (niwa-view-composer-draft-text nil t)
                                 (if (eq scenario 'composer-keystroke)
                                     "> bexnchmark draft\nsecond line"
                                   niwa-gate--draft)))
               (error "Scenario corrupted the composer: %s" scenario))
             (pcase scenario
               ('stream-chunk
                (unless (save-excursion
                          (goto-char (- (niwa-view--history-insertion-marker) 100))
                          (search-forward chunk
                                          (niwa-view--history-insertion-marker) t))
                  (error "Stream fixture skipped the appended chunk")))
               ('zone-tick
                (unless (and (> writes 0)
                             (with-current-buffer composer
                               (equal " · 13s"
                                      (get-text-property (+ (point-min) 2) 'display))))
                  (error "Zone tick skipped elapsed rail repaint"))))
             result)))))))

(defun niwa-allocation-gate-run ()
  "Warm and measure three hot paths, failing on any cons budget breach."
  (niwa-gate--assert-compiled)
  (let ((gc-cons-threshold (* 16 1024 1024))
        (gc-cons-percentage 0.1))
    (set-frame-size (selected-frame) 80 25)
    (dolist (entry niwa-gate--budgets)
      (dotimes (_ 5)
        (niwa-gate--with-scenario (car entry) #'funcall))
      (let ((sample (niwa-gate--with-scenario
                     (car entry) #'niwa-gate-measure)))
        (princ (format "%s %S budget=%d\n" (car entry) sample (cdr entry)))
        (niwa-gate-check (car entry) sample (cdr entry))))))

(provide 'niwa-allocation-gate)
;;; niwa-allocation-gate.el ends here
