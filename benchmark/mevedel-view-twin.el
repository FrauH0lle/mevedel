;;; mevedel-view-twin.el --- View updates against snapshot replay -*- lexical-binding: t; -*-

;;; Commentary:

;; Report, never gate, stream and elapsed-zone updates against a contiguous
;; text/property edit.  The bootstrap runs ordinary bytecode for timings and
;; a separate generic-primitive-call build for observation.  Both use fresh
;; fixtures from the allocation gate.

;;; Code:

(require 'mevedel-allocation-gate
         (file-name-concat
          (file-name-directory (or load-file-name buffer-file-name))
          "mevedel-allocation-gate"))
(require 'mevedel-minimal-edit-twin
         (file-name-concat
          (file-name-directory (or load-file-name buffer-file-name))
          "mevedel-minimal-edit-twin"))

(defun mevedel-view-twin--median (values)
  "Return the middle element of the five numeric VALUES."
  (nth 2 (sort (copy-sequence values) #'<)))

(defun mevedel-view-twin-run (&optional observe)
  "Report streaming and zone updates; with OBSERVE, count writes only.
Call the observation pass only in the generic-primitive-call build.  Timings
and floor counts come from normal bytecode in a separate fresh process."
  (mevedel-gate--assert-compiled)
  (let ((gc-cons-threshold (* 16 1024 1024))
        (gc-cons-percentage 0.1))
    (set-frame-size (selected-frame) 80 25)
    (dolist (scenario '(stream-chunk zone-tick))
      (dotimes (_ 5)
        (mevedel-gate--with-scenario scenario #'funcall))
      (if observe
          (let ((sample (mevedel-gate--with-scenario scenario #'mevedel-twin-observe)))
            (unless (> (+ (plist-get sample :real-deleted)
                          (plist-get sample :real-inserted)) 0)
              (error "Observation missed compiled buffer writes: %s" scenario))
            (princ (format "%s generic-call-counts %S\n" scenario sample)))
        (let (samples)
          (dotimes (_ 5)
            (push (mevedel-gate--with-scenario
                   scenario
                   (lambda (update)
                     (garbage-collect)
                     (mevedel-twin-measure update)))
                  samples))
          (princ
           (format
            "%s normal-bytecode floor-deleted=%d floor-inserted=%d floor-property-runs=%d real-ms=%.6f floor-ms=%.6f (five-sample medians)\n"
            scenario
            (plist-get (car samples) :floor-deleted)
            (plist-get (car samples) :floor-inserted)
            (plist-get (car samples) :floor-property-runs)
            (* 1000 (mevedel-view-twin--median
                     (mapcar (lambda (sample) (plist-get sample :real-seconds)) samples)))
            (* 1000 (mevedel-view-twin--median
                     (mapcar (lambda (sample) (plist-get sample :floor-seconds)) samples))))))))))

(provide 'mevedel-view-twin)
;;; mevedel-view-twin.el ends here
