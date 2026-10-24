;;; test-mevedel-engine-test-support.el --- Cold fixture ownership -*- lexical-binding: t -*-

;;; Commentary:

;; A fixture must load a deferred function's owner before stubbing it, or a
;; first require inside the stub can leave the function unbound afterwards.

;;; Code:

(require 'helpers
         (file-name-concat (file-name-directory (or load-file-name buffer-file-name))
                           "helpers"))

(mevedel-deftest mevedel-engine-test--with-session/cold-load ()
  (let ((support (file-name-concat mevedel-test--worktree-root "test"
                                   "mevedel-engine-test-support.el")))
    (with-temp-buffer
      (let ((status
             (call-process
              (expand-file-name invocation-name invocation-directory)
              nil (current-buffer) nil "--batch" "-Q"
              "--eval" (format "(setq load-path '%S)" load-path)
              "--eval"
              (format
               "(progn
                  (require 'mevedel-engine-test-support %S)
                  (unless (featurep 'mevedel-readiness)
                    (error \"Fixture must load readiness before stubbing it\"))
                  (let ((before (symbol-function 'mevedel-readiness-assert)))
                    (mevedel-engine-test--with-session
                      (require 'mevedel-readiness))
                    (unless (and before
                                 (eq before (symbol-function 'mevedel-readiness-assert)))
                      (error \"Fixture did not restore readiness\"))))"
               support))))
        (unless (eql status 0)
          (ert-fail (list :status status :output (buffer-string))))))))

(provide 'test-mevedel-engine-test-support)
;;; test-mevedel-engine-test-support.el ends here
