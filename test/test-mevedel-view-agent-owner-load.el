;;; test-mevedel-view-agent-owner-load.el --- Cold agent view tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Verify observer callback routing after compilation without preloaded structs.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(mevedel-deftest mevedel-view-agent/cold-compile ()
  ,test
  (test)
  :doc "cold-compiled agent callbacks cannot replace the parent's live transcript"
  (let* ((root (file-name-directory (locate-library "mevedel-view-agent")))
         (directory (make-temp-file "mevedel-view-agent-compile-" t))
         (source (file-name-concat directory "mevedel-view-agent.el"))
         (emacs (expand-file-name invocation-name invocation-directory)))
    (unwind-protect
        (progn
          (copy-file (file-name-concat root "mevedel-view-agent.el") source)
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
                        (when (special-variable-p 'mevedel--view-buffer)
                          (error "View binding must start undeclared"))
                        (let ((byte-compile-verbose nil))
                          (unless (byte-compile-file ,source)
                            (error "Agent view compilation failed")))
                        (load ,(concat source "c") nil t)
                        (require 'gptel-openai)
                        (load ,(file-name-concat root "test/test-mevedel-view-agent.el") nil t)
                        (let ((result
                               (ert-run-test
                                (ert-get-test 'mevedel-view--agent-live-transcript-dispatch/test))))
                          (unless (ert-test-passed-p result)
                            (signal (car (ert-test-result-with-condition-condition result))
                                    (cdr (ert-test-result-with-condition-condition result))))))))))
              (ert-info ((buffer-string))
                (should (equal 0 status))
                (should (equal "" (buffer-string)))))))
      (delete-directory directory t))))

(provide 'test-mevedel-view-agent-owner-load)
;;; test-mevedel-view-agent-owner-load.el ends here
