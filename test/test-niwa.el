;;; test-niwa.el --- Tests for niwa.el entry commands -*- lexical-binding: t -*-

;;; Commentary:

;; Contract tests for the public entry commands defined in niwa.el.

;;; Code:

(require 'niwa)
(require 'niwa-chat)
(require 'niwa-structs)
(require 'niwa-workspace)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(niwa-deftest niwa-in-directory ()
  ,test
  (test)
  :doc "projects a local directory refusal but leaves the shared validator raw"
  (let* ((root (make-temp-file "niwa-command-root-" t))
         (workspace (niwa-workspace--create :root root))
         (missing (file-name-concat root "missing")))
    (unwind-protect
        (with-temp-buffer
          (cl-letf (((symbol-function 'niwa-workspace)
                     (lambda (&optional _buffer) workspace)))
            (let ((raw (error-message-string
                        (should-error (niwa--normalize-session-directory missing workspace)
                                      :type 'user-error))))
              (should-not (string-search "niwa [" raw))
              (should (equal (format "niwa [%s]: %s" root raw)
                             (error-message-string
                              (should-error (niwa-in-directory missing) :type 'user-error)))))))
      (delete-directory root t))))

(provide 'test-niwa)

;;; test-niwa.el ends here
