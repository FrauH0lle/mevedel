;;; mevedel-sigpipe-test-support.el --- Interactive SIGPIPE disposition -*- lexical-binding: t -*-

;;; Commentary:

;; An interactive Emacs ignores SIGPIPE, so writing to a socket whose peer has
;; closed signals "no longer connected to pipe".  Batch Emacs keeps the
;; disposition it inherited, under which that write kills the test process.
;; Cases that write to a closed peer are therefore defined only in a child
;; batch Emacs started with SIGPIPE ignored, as an interactive Emacs runs, and
;; a parent case runs them there and reports the child's failures.

;;; Code:

(require 'ert)

(defconst mevedel-test--sigpipe-variable "MEVEDEL_TEST_SIGPIPE_IGNORED"
  "Environment variable marking a child that ignores SIGPIPE.")

(defun mevedel-test-sigpipe-child-p ()
  "Return non-nil in a child batch Emacs that ignores SIGPIPE."
  (equal (getenv mevedel-test--sigpipe-variable) "1"))

(defun mevedel-test-run-sigpipe-tests (file selector)
  "Run FILE's tests matching SELECTOR where SIGPIPE is ignored, or fail.
The child inherits this Emacs's load path and isolated environment."
  (let ((output (generate-new-buffer " *sigpipe-child*"))
        (process-environment (cons (concat mevedel-test--sigpipe-variable "=1")
                                   process-environment)))
    (unwind-protect
        (let ((status
               (call-process
                "sh" nil output nil "-c" "trap '' PIPE; exec \"$@\"" "sh"
                (expand-file-name invocation-name invocation-directory)
                "--batch" "-Q"
                "--eval" (format "(setq load-path '%S)" load-path)
                "-l" file
                "--eval" (format "(ert-run-tests-batch-and-exit %S)" selector))))
          (with-current-buffer output
            ;; At least one case must have run there, and every one passed.
            (unless (and (eql status 0)
                         (save-excursion
                           (goto-char (point-min))
                           (re-search-forward
                            "^Ran [1-9][0-9]* tests?, [0-9]+ results? as expected, 0 unexpected"
                            nil t)))
              (ert-fail (list :status status :output (buffer-string))))))
      (kill-buffer output))))

(provide 'mevedel-sigpipe-test-support)
;;; mevedel-sigpipe-test-support.el ends here
