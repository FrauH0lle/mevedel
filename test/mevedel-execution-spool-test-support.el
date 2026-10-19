;;; mevedel-execution-spool-test-support.el --- Shared spool test helpers -*- lexical-binding: t -*-

;;; Commentary:

;; Shared waiting and public resource-tool fixtures without test registration.

;;; Code:

(require 'helpers
         (file-name-concat (file-name-directory
                            (or buffer-file-name load-file-name byte-compile-current-file))
                           "helpers"))
(require 'mevedel-execution-spool)
(require 'mevedel-pipeline)
(require 'mevedel-resource)
(require 'mevedel-structs)
(require 'mevedel-tool-fs-read)
(require 'mevedel-tool-fs-search)

(defun test-mevedel-spool--wait (predicate)
  "Wait boundedly until PREDICATE succeeds."
  (let ((deadline (+ (float-time) 10)))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil 0.01))
    (should (funcall predicate))))

(defun test-mevedel-spool--tool (session operation address &rest options)
  "Call public resource OPERATION on ADDRESS in SESSION with OPTIONS."
  (let* ((attempt (mevedel-resource-prepare operation address (list :session session)))
         (mevedel-resource-current-attempts (list (cons address attempt)))
         (mevedel--session session)
         (args (append (list (if (eq operation 'read) :file_path :path) address) options))
         (handler (pcase operation
                    ('read #'mevedel-tool-fs-read)
                    ('grep #'mevedel-tool-fs-search-grep)
                    ('glob #'mevedel-tool-fs-search-glob)))
         result done cancel)
    (unwind-protect
        (progn
          (setq cancel (funcall handler (lambda (value) (setq result value done t)) args))
          (test-mevedel-spool--wait (lambda () done))
          result)
      (when (functionp cancel) (funcall cancel)))))

(provide 'mevedel-execution-spool-test-support)
;;; mevedel-execution-spool-test-support.el ends here
