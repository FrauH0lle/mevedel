;;; test-mevedel.el --- Tests for mevedel.el entry commands -*- lexical-binding: t -*-

;;; Commentary:

;; Contract tests for the public entry commands defined in mevedel.el.

;;; Code:

(require 'mevedel)
(require 'mevedel-chat)
(require 'mevedel-structs)
(require 'mevedel-workspace)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(mevedel-deftest mevedel-in-directory ()
  ,test
  (test)
  :doc "projects a local directory refusal but leaves the shared validator raw"
  (let* ((root (make-temp-file "mevedel-command-root-" t))
         (workspace (mevedel-workspace--create :root root))
         (missing (file-name-concat root "missing")))
    (unwind-protect
        (with-temp-buffer
          (cl-letf (((symbol-function 'mevedel-workspace)
                     (lambda (&optional _buffer) workspace)))
            (let ((raw (error-message-string
                        (should-error (mevedel--normalize-session-directory missing workspace)
                                      :type 'user-error))))
              (should-not (string-search "mevedel [" raw))
              (should (equal (format "mevedel [%s]: %s" root raw)
                             (error-message-string
                              (should-error (mevedel-in-directory missing) :type 'user-error)))))))
      (delete-directory root t))))

(provide 'test-mevedel)

;;; test-mevedel.el ends here
