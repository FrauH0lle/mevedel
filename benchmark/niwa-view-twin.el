;;; niwa-view-twin.el --- View updates against snapshot replay -*- lexical-binding: t; -*-

;;; Commentary:

;; Report, never gate, stream and elapsed-zone updates against a contiguous
;; text/property edit.  The bootstrap runs ordinary bytecode for timings and
;; a separate generic-primitive-call build for observation.  Both use fresh
;; fixtures from the allocation gate.

;;; Code:

(require 'niwa-allocation-gate
         (file-name-concat
          (file-name-directory (or load-file-name buffer-file-name))
          "niwa-allocation-gate"))
(require 'niwa-minimal-edit-twin
         (file-name-concat
          (file-name-directory (or load-file-name buffer-file-name))
          "niwa-minimal-edit-twin"))

(defun niwa-view-twin--median (values)
  "Return the middle element of the five numeric VALUES."
  (nth 2 (sort (copy-sequence values) #'<)))

(defun niwa-view-twin-run (&optional observe)
  "Report streaming and zone updates; with OBSERVE, count writes only.
Call the observation pass only in the generic-primitive-call build.  Timings
and floor counts come from normal bytecode in a separate fresh process."
  (niwa-gate--assert-compiled)
  (let ((gc-cons-threshold (* 16 1024 1024))
        (gc-cons-percentage 0.1))
    (set-frame-size (selected-frame) 80 25)
    (dolist (scenario '(stream-chunk zone-tick))
      (dotimes (_ 5)
        (niwa-gate--with-scenario scenario #'funcall))
      (if observe
          (let ((sample (niwa-gate--with-scenario scenario #'niwa-twin-observe)))
            (let ((chars (+ (plist-get sample :real-deleted)
                            (plist-get sample :real-inserted))))
              (unless (if (eq scenario 'zone-tick)
                          (and (zerop chars) (> (plist-get sample :real-property-runs) 0))
                        (> chars 0))
                (error "Observation found unexpected compiled buffer writes: %s" scenario)))
            (princ (format "%s generic-call-counts %S\n" scenario sample)))
        (let (samples)
          (dotimes (_ 5)
            (push (niwa-gate--with-scenario
                   scenario
                   (lambda (update)
                     (garbage-collect)
                     (niwa-twin-measure update)))
                  samples))
          (princ
           (format
            "%s normal-bytecode floor-deleted=%d floor-inserted=%d floor-property-runs=%d real-ms=%.6f floor-ms=%.6f (five-sample medians)\n"
            scenario
            (plist-get (car samples) :floor-deleted)
            (plist-get (car samples) :floor-inserted)
            (plist-get (car samples) :floor-property-runs)
            (* 1000 (niwa-view-twin--median
                     (mapcar (lambda (sample) (plist-get sample :real-seconds)) samples)))
            (* 1000 (niwa-view-twin--median
                     (mapcar (lambda (sample) (plist-get sample :floor-seconds)) samples))))))))))

(provide 'niwa-view-twin)
;;; niwa-view-twin.el ends here
