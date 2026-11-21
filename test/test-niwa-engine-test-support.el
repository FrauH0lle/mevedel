;;; test-niwa-engine-test-support.el --- Cold fixture ownership -*- lexical-binding: t -*-

;;; Commentary:

;; A fixture must load a deferred function's owner before stubbing it, or a
;; first require inside the stub can leave the function unbound afterwards.

;;; Code:

(require 'helpers
         (file-name-concat (file-name-directory (or load-file-name buffer-file-name))
                           "helpers"))

(niwa-deftest niwa-engine-test--with-session/cold-load ()
  (let ((support (file-name-concat niwa-test--worktree-root "test"
                                   "niwa-engine-test-support.el")))
    (with-temp-buffer
      (let ((status
             (call-process
              (expand-file-name invocation-name invocation-directory)
              nil (current-buffer) nil "--batch" "-Q"
              "--eval" (format "(setq load-path '%S)" load-path)
              "--eval"
              (format
               "(progn
                  (require 'niwa-engine-test-support %S)
                  (unless (featurep 'niwa-readiness)
                    (error \"Fixture must load readiness before stubbing it\"))
                  (let ((before (symbol-function 'niwa-readiness-assert)))
                    (niwa-engine-test--with-session
                      (require 'niwa-readiness))
                    (unless (and before
                                 (eq before (symbol-function 'niwa-readiness-assert)))
                      (error \"Fixture did not restore readiness\"))))"
               support))))
        (unless (eql status 0)
          (ert-fail (list :status status :output (buffer-string))))))))

(provide 'test-niwa-engine-test-support)
;;; test-niwa-engine-test-support.el ends here
