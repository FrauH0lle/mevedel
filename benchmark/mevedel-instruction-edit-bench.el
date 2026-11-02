;;; mevedel-instruction-edit-bench.el --- Compiled interior edit benchmark -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with python3 scripts/bench-instruction-edits.py, not in a live editor.
;; Match the user fixture: 2000 lines, 3-line references every 10 lines,
;; and 2000 single-character inserts inside the first reference, each followed
;; by an undo boundary.  Setup, GC, and assertions are outside the timer.
;; Five fresh-fixture warmups precede five fresh-fixture samples for each N.
;; This measures batch text mutation and hooks, not interactive redisplay.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'mevedel)
(require 'mevedel-directive-source)

(defconst mevedel-instruction-bench--keys 2000
  "Number of interior inserts per fixture.")

(defun mevedel-instruction-bench--type ()
  "Insert the benchmark keystrokes with a separate undo boundary for each."
  (dotimes (_ mevedel-instruction-bench--keys)
    (insert "x")
    (undo-boundary)))

(defun mevedel-instruction-bench--sample (count)
  "Time one fresh fixture with COUNT references, asserting its final state."
  (let ((mevedel--instruction-states (make-hash-table :test #'equal))
        (mevedel-workspace--registry (make-hash-table :test #'equal))
        (undo-limit most-positive-fixnum)
        (undo-strong-limit most-positive-fixnum)
        (undo-outer-limit nil))
    (with-temp-buffer
      (dotimes (line 2000)
        (insert (format "line %d some text here\n" line)))
      (buffer-enable-undo)
      (let (references bounds)
        (dotimes (index count)
          (goto-char (point-min))
          (forward-line (* index 10))
          (let ((start (point)))
            (forward-line 3)
            (let ((overlay (mevedel--create-reference-in
                            (current-buffer) start (point))))
              (unless (= (count-lines (overlay-start overlay) (overlay-end overlay)) 3)
                (error "Fixture reference does not cover three lines"))
              (push overlay references)
              (push (cons (overlay-start overlay) (overlay-end overlay)) bounds))))
        (setq references (nreverse references) bounds (nreverse bounds))
        (unless (and (= (length (mevedel--instructions-in
                                 (point-min) (point-max))) count)
                     (memq #'mevedel--instruction-before-change before-change-functions)
                     (memq #'mevedel--instruction-after-change after-change-functions)
                     (= (count-lines (point-min) (point-max)) 2000))
          (error "Fixture did not install references and production hooks"))
        (goto-char (overlay-start (car references)))
        (forward-line 1)
        (forward-char 5)
        (setq buffer-undo-list nil)
        (let* ((position (point))
               (before (buffer-string))
               (size (buffer-size))
               (_gc (garbage-collect))
               (gc-before gcs-done)
               (start (current-time))
               (seconds (progn
                          (mevedel-instruction-bench--type)
                          (float-time (time-subtract (current-time) start))))
               (collections (- gcs-done gc-before)))
          (unless (and (= (buffer-size) (+ size mevedel-instruction-bench--keys))
                       (= (point) (+ position mevedel-instruction-bench--keys))
                       (equal (buffer-string)
                              (concat (substring before 0 (1- position))
                                      (make-string mevedel-instruction-bench--keys ?x)
                                      (substring before (1- position))))
                       (= (cl-count nil buffer-undo-list)
                          mevedel-instruction-bench--keys))
            (error "Fixture skipped inserts or undo boundaries"))
          (cl-loop for overlay in references for bound in bounds for index from 0
                   do (unless (and (eq (overlay-buffer overlay) (current-buffer))
                                   (= (overlay-start overlay)
                                      (+ (car bound)
                                         (if (zerop index) 0
                                           mevedel-instruction-bench--keys)))
                                   (= (overlay-end overlay)
                                      (+ (cdr bound) mevedel-instruction-bench--keys)))
                        (error "Interior edit corrupted reference %d" index)))
          `((us_per_key . ,(/ (* seconds 1e6) mevedel-instruction-bench--keys))
            (apply_entries . ,(cl-count-if
                              (lambda (entry) (eq (car-safe entry) 'apply))
                              buffer-undo-list))
            (gcs . ,collections)))))))

(defun mevedel-instruction-bench--provenance (directory)
  "Assert compiled hot paths originate in DIRECTORY; return their locations."
  (let ((symbols '(mevedel-instruction-bench--type
                   mevedel-instruction-bench--sample
                   mevedel-instruction-bench-run
                   mevedel--create-reference-in
                   mevedel--instruction-before-change
                   mevedel--instruction-after-change
                   mevedel--instructions-in
                   mevedel--update-instruction-overlay)))
    ;; Master predates source-presentation undo; assert its paths when present.
    (when (locate-library "mevedel-instruction-undo")
      (setq symbols (append symbols '(mevedel-instruction-undo-capture
                                      mevedel-instruction-undo-record))))
    (dolist (symbol '(mevedel--instruction-adjust-edit
                      mevedel--instruction-move-after-edit
                      mevedel--instruction-normalize-lines
                      mevedel-instruction-undo-extend))
      (when (fboundp symbol) (push symbol symbols)))
    (mapcar
     (lambda (symbol)
       (let ((file (symbol-file symbol 'defun)))
         (unless (and (byte-code-function-p (symbol-function symbol))
                      file (string-suffix-p ".elc" file)
                      (file-in-directory-p file directory))
           (error "Hot path is not expected bytecode: %s (%s)" symbol file))
         `((symbol . ,(symbol-name symbol)) (loaded . ,(file-truename file)))))
     symbols)))

(defun mevedel-instruction-bench-run (directory)
  "Measure compiled code from DIRECTORY and print one JSON report."
  (unless noninteractive (error "Benchmark requires batch Emacs"))
  (let ((provenance (mevedel-instruction-bench--provenance directory))
        (gc-cons-threshold (* 16 1024 1024))
        (gc-cons-percentage 0.1)
        scenarios libraries)
    ;; Reject source fallbacks anywhere in the loaded product, not just the
    ;; named hot paths.  Dependency locations are recorded separately below.
    (dolist (entry load-history)
      (when-let* ((file (car entry))
                  (_ (string-prefix-p "mevedel" (file-name-nondirectory file))))
        (unless (and (string-suffix-p ".elc" file)
                     (file-in-directory-p file directory))
          (error "Product library loaded outside compiled snapshot: %s" file)))
      (when (and (car entry) (file-in-directory-p
                             (car entry) (getenv "MEVEDEL_BENCH_DEPS")))
        (push (car entry) libraries)))
    (dolist (count '(1 50 200))
      (dotimes (_ 5) (mevedel-instruction-bench--sample count))
      (push `((references . ,count)
              (samples . ,(vconcat (cl-loop repeat 5 collect
                                           (mevedel-instruction-bench--sample count)))))
            scenarios))
    (princ (json-encode
            `((emacs_version . ,emacs-version)
              (keys . ,mevedel-instruction-bench--keys)
              (warmups . 5) (sample_count . 5)
              (gc_cons_threshold . ,gc-cons-threshold)
              (gc_cons_percentage . ,gc-cons-percentage)
              (hotpaths . ,(vconcat provenance))
              (dependency_libraries . ,(vconcat (sort libraries #'string-lessp)))
              (scenarios . ,(vconcat (nreverse scenarios))))))
    (terpri)))

(provide 'mevedel-instruction-edit-bench)
;;; mevedel-instruction-edit-bench.el ends here
