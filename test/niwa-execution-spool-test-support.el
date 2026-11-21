;;; niwa-execution-spool-test-support.el --- Shared spool test helpers -*- lexical-binding: t -*-

;;; Commentary:

;; Shared waiting and public resource-tool fixtures without test registration.

;;; Code:

(require 'helpers
         (file-name-concat (file-name-directory
                            (or buffer-file-name load-file-name byte-compile-current-file))
                           "helpers"))
(require 'niwa-execution-spool)
(require 'niwa-pipeline)
(require 'niwa-resource)
(require 'niwa-structs)
(require 'niwa-tool-fs-read)
(require 'niwa-tool-fs-search)

(defun test-niwa-spool--wait (predicate)
  "Wait boundedly until PREDICATE succeeds.
The bound detects hangs only; tool calls take seconds under full-suite load."
  (let ((deadline (+ (float-time) 60)))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil 0.01))
    (should (funcall predicate))))

(defun test-niwa-spool--tool (session operation address &rest options)
  "Call public resource OPERATION on ADDRESS in SESSION with OPTIONS."
  (let* ((attempt (niwa-resource-prepare operation address (list :session session)))
         (niwa-resource-current-attempts (list (cons address attempt)))
         (niwa--session session)
         (args (append (list (if (eq operation 'read) :file_path :path) address) options))
         (handler (pcase operation
                    ('read #'niwa-tool-fs-read)
                    ('grep #'niwa-tool-fs-search-grep)
                    ('glob #'niwa-tool-fs-search-glob)))
         result done cancel)
    (unwind-protect
        (progn
          (setq cancel (funcall handler (lambda (value) (setq result value done t)) args))
          (test-niwa-spool--wait (lambda () done))
          result)
      (when (functionp cancel) (funcall cancel)))))

(provide 'niwa-execution-spool-test-support)
;;; niwa-execution-spool-test-support.el ends here
