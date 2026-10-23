;;; mevedel-view-fixtures.el --- Shared view benchmark fixtures -*- lexical-binding: t; -*-

;;; Commentary:

;; Synthetic view setup shared by the profiler and allocation benchmarks.
;; These are batch fixtures, not restored sessions or redisplay measurements.

;;; Code:

(require 'mevedel)
(require 'mevedel-view)
(require 'mevedel-view-composer)
(require 'mevedel-view-render)
(require 'mevedel-view-stream)

(defvar mevedel-session--read-only-mode)

(defun mevedel-benchmark--with-view (function)
  "Call FUNCTION with initialized data and view buffers."
  (let ((data-buf (generate-new-buffer " *mevedel-benchmark-data*"))
        (view-buf (generate-new-buffer " *mevedel-benchmark-view*")))
    (unwind-protect
        (progn
          (with-current-buffer data-buf
            (mevedel--transcript-org-mode)
            (setq-local gptel-response-separator "\n\n")
            (setq-local mevedel-session--read-only-mode nil))
          (mevedel-view--setup view-buf data-buf)
          (save-window-excursion
            (switch-to-buffer view-buf)
            (funcall function data-buf view-buf)))
      (when (buffer-live-p view-buf)
        (kill-buffer view-buf))
      (when (buffer-live-p data-buf)
        (kill-buffer data-buf)))))

(defun mevedel-benchmark--insert-history (bytes)
  "Insert BYTES of synthetic response history in the current view."
  (let ((inhibit-read-only t))
    (mevedel-view--call-with-render-boundaries-advancing
     (lambda ()
       (goto-char (mevedel-view--history-insertion-marker))
       (let ((start (point)))
         (insert (make-string bytes ?x))
         (put-text-property start (point) 'mevedel-view-type 'response))))))

(provide 'mevedel-view-fixtures)
;;; mevedel-view-fixtures.el ends here
