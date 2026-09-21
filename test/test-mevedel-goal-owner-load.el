;;; test-mevedel-goal-owner-load.el --- Cold Goal compilation tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Exercise in-flight Goal editing after compilation without preloaded gptel.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(mevedel-deftest mevedel-goal/cold-compile ()
  ,test
  (test)
  :doc "cold-compiled Goal edits preserve in-flight request accounting"
  (let* ((root (file-name-directory (locate-library "mevedel-goal")))
         (directory (make-temp-file "mevedel-goal-compile-" t))
         (source (file-name-concat directory "mevedel-goal.el"))
         (emacs (expand-file-name invocation-name invocation-directory)))
    (unwind-protect
        (progn
          (copy-file (file-name-concat root "mevedel-goal.el") source)
          (with-temp-buffer
            (let ((status
                   (call-process
                    emacs nil t nil "--batch" "-Q" "--eval"
                    (prin1-to-string
                     `(progn
                        (setq load-path ',(cons directory load-path))
                        (require 'bytecomp)
                        (when (fboundp 'gptel-fsm-info)
                          (error "Request struct must start unloaded"))
                        (let ((byte-compile-verbose nil))
                          (unless (byte-compile-file ,source)
                            (error "Goal compilation failed")))
                        (load ,(concat source "c") nil t)
                        (require 'gptel-openai)
                        (load ,(file-name-concat root "test/test-mevedel-goal.el") nil t)
                        (let ((result
                               (ert-run-test
                                (ert-get-test 'mevedel-goal-edit/test))))
                          (unless (ert-test-passed-p result)
                            (signal (car (ert-test-result-with-condition-condition result))
                                    (cdr (ert-test-result-with-condition-condition result))))))))))
              (ert-info ((buffer-string))
                (should (equal 0 status))
                (should (equal "" (buffer-string)))))))
      (delete-directory directory t))))

(provide 'test-mevedel-goal-owner-load)
;;; test-mevedel-goal-owner-load.el ends here
