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

(defun mevedel-test-await-file-unread (file &optional timeout)
  "Wait at most TIMEOUT seconds for FILE without reading process output.
TIMEOUT defaults to ten seconds.  This preserves a closed peer's unread EOF."
  (let ((deadline (+ (float-time) (or timeout 10))))
    (while (not (file-exists-p file))
      (when (>= (float-time) deadline)
        (error "Peer marker unavailable: %s" file)))))

(defun mevedel-test-release-reply-peer (prefix)
  "Release the pending reply peer at PREFIX and await its actual close.
Never pump Emacs's sockets: the reply must discover the unread disconnect."
  (mevedel-test-await-file-unread (concat prefix "-entered"))
  (should-not (file-exists-p (concat prefix "-closed")))
  (write-region "release" nil (concat prefix "-release") nil 'silent)
  (mevedel-test-await-file-unread (concat prefix "-closed")))

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
