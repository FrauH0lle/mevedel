;;; niwa-view-fixtures.el --- Shared view benchmark fixtures -*- lexical-binding: t; -*-

;;; Commentary:

;; Synthetic view setup shared by the profiler and allocation benchmarks.
;; These are batch fixtures, not restored sessions or redisplay measurements.

;;; Code:

(require 'niwa)
(require 'niwa-view)
(require 'niwa-view-composer)
(require 'niwa-view-render)
(require 'niwa-view-stream)

(defvar niwa-session--read-only-mode)

(defun niwa-benchmark--with-view (function)
  "Call FUNCTION with initialized data and view buffers."
  (let ((data-buf (generate-new-buffer " *niwa-benchmark-data*"))
        (view-buf (generate-new-buffer " *niwa-benchmark-view*")))
    (unwind-protect
        (progn
          (with-current-buffer data-buf
            (niwa--transcript-org-mode)
            (setq-local gptel-response-separator "\n\n")
            (setq-local niwa-session--read-only-mode nil))
          (niwa-view--setup view-buf data-buf)
          (save-window-excursion
            (switch-to-buffer view-buf)
            (funcall function data-buf view-buf)))
      (when (buffer-live-p view-buf)
        (kill-buffer view-buf))
      (when (buffer-live-p data-buf)
        (kill-buffer data-buf)))))

(defun niwa-benchmark--insert-history (bytes)
  "Insert BYTES of synthetic response history in the current view."
  (let ((inhibit-read-only t))
    (niwa-view--call-with-render-boundaries-advancing
     (lambda ()
       (goto-char (niwa-view--history-insertion-marker))
       (let ((start (point)))
         (insert (make-string bytes ?x))
         (put-text-property start (point) 'niwa-view-type 'response))))))

(provide 'niwa-view-fixtures)
;;; niwa-view-fixtures.el ends here
