;;; -*- lexical-binding: t -*-
;; Bounded CPU profile: frames are reduced to short name strings before any
;; aggregation, and all printing is bounded.
(require 'profiler)

(defun cpuh-prof--name (f)
  (cond ((null f) nil)
        ((symbolp f) (symbol-name f))
        (t (let ((print-level 2) (print-length 3) (print-circle t))
             (truncate-string-to-width
              (replace-regexp-in-string "\n" " " (format "%S" f)) 80)))))

(defun cpuh-prof--dump (out &optional profile)
  (let ((log (profiler-profile-log (or profile (profiler-cpu-profile))))
        (incl (make-hash-table :test 'equal))
        (self (make-hash-table :test 'equal))
        (paths (make-hash-table :test 'equal))
        (total 0))
    (maphash
     (lambda (bt n)
       (setq total (+ total n))
       (let ((names (delq nil (mapcar #'cpuh-prof--name (append bt nil))))
             seen)
         (dolist (nm names)
           (unless (member nm seen)
             (push nm seen)
             (puthash nm (+ n (gethash nm incl 0)) incl)))
         (when names
           (puthash (car names) (+ n (gethash (car names) self 0)) self)
           ;; Leaf-to-root path, capped at 12 frames.
           (let ((p (string-join (seq-take names 12) " <- ")))
             (puthash p (+ n (gethash p paths 0)) paths)))))
     log)
    (cl-flet ((top (h k)
                (let (l)
                  (maphash (lambda (key v) (push (cons v key) l)) h)
                  (seq-take (sort l (lambda (a b) (> (car a) (car b)))) k))))
      (with-temp-file out
        (insert (format "TOTAL %d samples\n== inclusive ==\n" total))
        (dolist (e (top incl 80))
          (insert (format "%6d %5.1f%% %s\n" (car e) (/ (* 100.0 (car e)) (max total 1)) (cdr e))))
        (insert "== self ==\n")
        (dolist (e (top self 30))
          (insert (format "%6d %5.1f%% %s\n" (car e) (/ (* 100.0 (car e)) (max total 1)) (cdr e))))
        (insert "== hottest stacks (leaf <- root) ==\n")
        (dolist (e (top paths 25))
          (insert (format "%6d %5.1f%% %s\n" (car e) (/ (* 100.0 (car e)) (max total 1)) (cdr e))))))))

(defun cpuh-prof-run (secs out)
  (when (profiler-running-p) (profiler-stop))
  (profiler-start 'cpu)
  (run-with-timer secs nil
                  (lambda ()
                    (profiler-stop)
                    (condition-case err (cpuh-prof--dump out)
                      (error (with-temp-file out
                               (insert (format "ERROR %S" err))))))))

(defun cpuh-prof-begin ()
  (when (profiler-running-p) (profiler-stop))
  (profiler-start 'cpu+mem) t)

(defun cpuh-prof-end (cpu-out mem-out)
  (profiler-stop)
  (let ((cpu (profiler-cpu-profile)) (mem (profiler-memory-profile)))
    (cpuh-prof--dump cpu-out cpu)
    (cpuh-prof--dump mem-out mem))
  t)
