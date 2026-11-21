;;; niwa-history-test-helpers.el --- Native history fixtures -*- lexical-binding: t -*-

;;; Commentary:
;; Shared native saved-source fixtures without registering tests at load time.

;;; Code:

(require 'niwa-session-test-support
         (file-name-concat
          (file-name-directory (or load-file-name buffer-file-name))
          "niwa-session-test-support"))
(require 'niwa-tool-fs-search)
(require 'niwa-tool-fs-read)
(require 'niwa-history-search)

(defun test-niwa-history-search--source (workspace name text)
  "Save a native conversation with TEXT under NAME in WORKSPACE."
  (let ((session (niwa-session-create name workspace))
        (buffer (generate-new-buffer " *history-source*")))
    (with-current-buffer buffer
      (niwa--transcript-org-mode)
      (setq-local niwa--session session)
      (niwa-session-set-root-buffer session buffer)
      (niwa-transcript-enable-gptel-mode)
      (insert text)
      (niwa-session-artifacts-save session buffer))
    (cons session buffer)))

(defun test-niwa-history-search--query (workspace operation args)
  "Await one native resource OPERATION with ARGS in WORKSPACE."
  (let* ((address (plist-get args (if (eq operation 'read) :file_path :path)))
         (attempt (niwa-resource-prepare operation address (list :workspace workspace)))
         (niwa-resource-current-attempts (list (cons address attempt)))
         done result cancel)
    (unwind-protect
        (progn
          (setq cancel
                (funcall (pcase operation ('read #'niwa-tool-fs-read)
                                ('glob #'niwa-tool-fs-search-glob)
                                ('grep #'niwa-tool-fs-search-grep))
                         (lambda (value) (setq result value done t)) args))
          (with-timeout (10 (ert-fail "History operation timed out"))
            (while (not done) (accept-process-output nil 0.01)))
          result)
      (when (functionp cancel) (funcall cancel))
      (niwa-resource-discard-attempts (list attempt)))))

(provide 'niwa-history-test-helpers)
;;; niwa-history-test-helpers.el ends here
