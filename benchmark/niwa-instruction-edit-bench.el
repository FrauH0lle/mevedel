;;; niwa-instruction-edit-bench.el --- Compiled interior edit benchmark -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with python3 scripts/bench-instruction-edits.py, not in a live editor.
;; Match the user fixture: 2000 lines, 3-line notes every 10 lines,
;; and 2000 single-character inserts inside the first note, each followed
;; by an undo boundary.  Setup, GC, and assertions are outside the timer.
;; Five fresh-fixture warmups precede five fresh-fixture samples for each N.
;; This measures batch text mutation and hooks, not interactive redisplay.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'niwa)
(require 'niwa-note-source)
(require 'niwa-notes)

(defconst niwa-instruction-bench--keys 2000
  "Number of interior inserts per fixture.")

(defun niwa-instruction-bench--type ()
  "Insert the benchmark keystrokes with a separate undo boundary for each."
  (dotimes (_ niwa-instruction-bench--keys)
    (insert "x")
    (undo-boundary)))

(defun niwa-instruction-bench--sample (count)
  "Time one fresh fixture with COUNT notes, asserting its final state."
  (let ((niwa--instruction-states (make-hash-table :test #'equal))
        (niwa-workspace--registry (make-hash-table :test #'equal))
        (undo-limit most-positive-fixnum)
        (undo-strong-limit most-positive-fixnum)
        (undo-outer-limit nil))
    (with-temp-buffer
      (dotimes (line 2000)
        (insert (format "line %d some text here\n" line)))
      (setq-local buffer-file-name (make-temp-file "niwa-edit-bench-" nil ".txt"))
      (write-region (point-min) (point-max) buffer-file-name nil 'silent)
      (let ((file buffer-file-name))
        (add-hook 'kill-buffer-hook (lambda () (delete-file file)) t t))
      (buffer-enable-undo)
      (let (references bounds)
        (dotimes (index count)
          (goto-char (point-min))
          (forward-line (* index 10))
          (let ((start (point)))
            (forward-line 3)
            (let* ((transient-mark-mode t)
                   (mark-active t)
                   (overlay (progn (set-mark start) (niwa-note-create ""))))
              (unless (= (count-lines (overlay-start overlay) (overlay-end overlay)) 3)
                (error "Fixture reference does not cover three lines"))
              (push overlay references)
              (push (cons (overlay-start overlay) (overlay-end overlay)) bounds))))
        (setq references (nreverse references) bounds (nreverse bounds))
        (unless (and (= (length (niwa--instructions-in
                                 (point-min) (point-max))) count)
                     (memq #'niwa--instruction-before-change before-change-functions)
                     (memq #'niwa--instruction-after-change after-change-functions)
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
                          (niwa-instruction-bench--type)
                          (float-time (time-subtract (current-time) start))))
               (collections (- gcs-done gc-before)))
          (unless (and (= (buffer-size) (+ size niwa-instruction-bench--keys))
                       (= (point) (+ position niwa-instruction-bench--keys))
                       (equal (buffer-string)
                              (concat (substring before 0 (1- position))
                                      (make-string niwa-instruction-bench--keys ?x)
                                      (substring before (1- position))))
                       (= (cl-count nil buffer-undo-list)
                          niwa-instruction-bench--keys))
            (error "Fixture skipped inserts or undo boundaries"))
          (cl-loop for overlay in references for bound in bounds for index from 0
                   do (unless (and (eq (overlay-buffer overlay) (current-buffer))
                                   (= (overlay-start overlay)
                                      (+ (car bound)
                                         (if (zerop index) 0
                                           niwa-instruction-bench--keys)))
                                   (= (overlay-end overlay)
                                      (+ (cdr bound) niwa-instruction-bench--keys)))
                        (error "Interior edit corrupted reference %d" index)))
          `((us_per_key . ,(/ (* seconds 1e6) niwa-instruction-bench--keys))
            (apply_entries . ,(cl-count-if
                              (lambda (entry) (eq (car-safe entry) 'apply))
                              buffer-undo-list))
            (gcs . ,collections)))))))

(defun niwa-instruction-bench--provenance (directory)
  "Assert compiled hot paths originate in DIRECTORY; return their locations."
  (let ((symbols '(niwa-instruction-bench--type
                   niwa-instruction-bench--sample
                   niwa-instruction-bench-run
                   niwa-note-create
                   niwa--instruction-before-change
                   niwa--instruction-after-change
                   niwa--instructions-in
                   niwa--update-instruction-overlay)))
    ;; Master predates source-presentation undo; assert its paths when present.
    (when (locate-library "niwa-instruction-undo")
      (setq symbols (append symbols '(niwa-instruction-undo-capture
                                      niwa-instruction-undo-record))))
    (dolist (symbol '(niwa--instruction-adjust-edit
                      niwa--instruction-move-after-edit
                      niwa--instruction-normalize-lines
                      niwa-instruction-undo-extend))
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

(defun niwa-instruction-bench-run (directory)
  "Measure compiled code from DIRECTORY and print one JSON report."
  (unless noninteractive (error "Benchmark requires batch Emacs"))
  (let ((provenance (niwa-instruction-bench--provenance directory))
        (gc-cons-threshold (* 16 1024 1024))
        (gc-cons-percentage 0.1)
        scenarios libraries)
    ;; Reject source fallbacks anywhere in the loaded product, not just the
    ;; named hot paths.  Dependency locations are recorded separately below.
    (dolist (entry load-history)
      (when-let* ((file (car entry))
                  (_ (string-prefix-p "niwa" (file-name-nondirectory file))))
        (unless (and (string-suffix-p ".elc" file)
                     (file-in-directory-p file directory))
          (error "Product library loaded outside compiled snapshot: %s" file)))
      (when (and (car entry) (file-in-directory-p
                             (car entry) (getenv "NIWA_BENCH_DEPS")))
        (push (car entry) libraries)))
    (dolist (count '(1 50 200))
      (dotimes (_ 5) (niwa-instruction-bench--sample count))
      (push `((references . ,count)
              (samples . ,(vconcat (cl-loop repeat 5 collect
                                           (niwa-instruction-bench--sample count)))))
            scenarios))
    (princ (json-encode
            `((emacs_version . ,emacs-version)
              (keys . ,niwa-instruction-bench--keys)
              (warmups . 5) (sample_count . 5)
              (gc_cons_threshold . ,gc-cons-threshold)
              (gc_cons_percentage . ,gc-cons-percentage)
              (hotpaths . ,(vconcat provenance))
              (dependency_libraries . ,(vconcat (sort libraries #'string-lessp)))
              (scenarios . ,(vconcat (nreverse scenarios))))))
    (terpri)))

(provide 'niwa-instruction-edit-bench)
;;; niwa-instruction-edit-bench.el ends here
