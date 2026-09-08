;;; request-test.el -- Quality harness settlement checks -*- lexical-binding: t -*-

;;; Commentary:

;; Network-free checks of quality request completion and timeout cancellation.
;; Run with Eask's ERT command. The real-provider test is excluded here.

;;; Code:

(load (file-name-concat (file-name-directory load-file-name) "run.el") nil t)
(ert-delete-test 'mevedel-memory-quality-real-provider)

(mevedel-deftest mevedel-quality--request ()
  ,test
  (test)
  :doc "completed requests return their result while timed-out requests are cancelled"
  (dolist (complete '(nil t))
    (let ((now 0) cancelled)
      (cl-letf (((symbol-function 'float-time)
                 (lambda (&rest _) (prog1 now (setq now 121))))
                ((symbol-function 'mevedel-context-summary-generate)
                 (lambda (_source _purpose callback &rest _)
                   (when complete
                     (funcall callback '(:outcome success :summary "fixture")))
                   (lambda () (setq cancelled t)))))
        (let ((result (mevedel-quality--request "fixture" 'digest nil)))
          (should (eq cancelled (not complete)))
          (should (eq (plist-get result :outcome) (if complete 'success 'error)))
          (unless complete
            (should (eq (plist-get result :error-class) 'timeout))))))))

(provide 'mevedel-memory-quality-request-test)
;;; request-test.el ends here
