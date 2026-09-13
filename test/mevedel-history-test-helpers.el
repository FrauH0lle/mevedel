;;; mevedel-history-test-helpers.el --- Native history fixtures -*- lexical-binding: t -*-

;;; Commentary:
;; Shared native saved-source fixtures without registering tests at load time.

;;; Code:

(require 'mevedel-session-test-support
         (file-name-concat
          (file-name-directory (or load-file-name buffer-file-name))
          "mevedel-session-test-support"))
(require 'mevedel-tool-fs-search)
(require 'mevedel-tool-fs-read)
(require 'mevedel-history-search)

(defun test-mevedel-history-search--source (workspace name text)
  "Save a native conversation with TEXT under NAME in WORKSPACE."
  (let ((session (mevedel-session-create name workspace))
        (buffer (generate-new-buffer " *history-source*")))
    (with-current-buffer buffer
      (mevedel--transcript-org-mode)
      (setq-local mevedel--session session)
      (mevedel-session-set-root-buffer session buffer)
      (mevedel-transcript-enable-gptel-mode)
      (insert text)
      (mevedel-session-artifacts-save session buffer))
    (cons session buffer)))

(defun test-mevedel-history-search--query (workspace operation args)
  "Await one native resource OPERATION with ARGS in WORKSPACE."
  (let* ((address (plist-get args (if (eq operation 'read) :file_path :path)))
         (attempt (mevedel-resource-prepare operation address (list :workspace workspace)))
         (mevedel-resource-current-attempts (list (cons address attempt)))
         done result cancel)
    (unwind-protect
        (progn
          (setq cancel
                (funcall (pcase operation ('read #'mevedel-tool-fs-read)
                                ('glob #'mevedel-tool-fs-search-glob)
                                ('grep #'mevedel-tool-fs-search-grep))
                         (lambda (value) (setq result value done t)) args))
          (with-timeout (10 (ert-fail "History operation timed out"))
            (while (not done) (accept-process-output nil 0.01)))
          result)
      (when (functionp cancel) (funcall cancel))
      (mevedel-resource-discard-attempts (list attempt)))))

(provide 'mevedel-history-test-helpers)
;;; mevedel-history-test-helpers.el ends here
