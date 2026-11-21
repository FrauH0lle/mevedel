;;; test-niwa-view-agent-owner-load.el --- Cold agent view tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Verify observer callback routing after compilation without preloaded structs.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(niwa-deftest niwa-view-agent/cold-compile ()
  ,test
  (test)
  :doc "cold-compiled agent callbacks cannot replace the parent's live transcript"
  (let* ((root (file-name-directory (locate-library "niwa-view-agent")))
         (directory (make-temp-file "niwa-view-agent-compile-" t))
         (source (file-name-concat directory "niwa-view-agent.el"))
         (emacs (expand-file-name invocation-name invocation-directory)))
    (unwind-protect
        (progn
          (copy-file (file-name-concat root "niwa-view-agent.el") source)
          ;; Helpers already loads the session structs in this process.
          ;; Compile first in a fresh child inheriting Eask's isolated roots.
          (with-temp-buffer
            (let ((status
                   (call-process
                    emacs nil t nil "--batch" "-Q" "--eval"
                    (prin1-to-string
                     `(progn
                        (setq load-path ',(cons directory load-path))
                        (require 'bytecomp)
                        (when (special-variable-p 'niwa--view-buffer)
                          (error "View binding must start undeclared"))
                        (let ((byte-compile-verbose nil))
                          (unless (byte-compile-file ,source)
                            (error "Agent view compilation failed")))
                        (load ,(concat source "c") nil t)
                        (require 'gptel-openai)
                        (load ,(file-name-concat root "test/test-niwa-view-agent.el") nil t)
                        (let ((tests (ert-select-tests
                                      "^niwa-view--agent-live-transcript-dispatch/test" t)))
                          (unless tests (error "No agent callback routing tests"))
                          (dolist (test tests)
                            (let ((result (ert-run-test test)))
                              (unless (ert-test-passed-p result)
                                (signal (car (ert-test-result-with-condition-condition result))
                                        (cdr (ert-test-result-with-condition-condition result))))))))))))
              (ert-info ((buffer-string))
                (should (equal 0 status))
                (should (equal "" (buffer-string)))))))
      (delete-directory directory t))))

(provide 'test-niwa-view-agent-owner-load)
;;; test-niwa-view-agent-owner-load.el ends here
