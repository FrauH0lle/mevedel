;;; test-mevedel-review-owner-load.el --- Cold review compilation tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Exercise verdict storage without preloaded agent struct setters.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(mevedel-deftest mevedel-review/cold-compile ()
  ,test
  (test)
  :doc "cold-compiled verification records and clears invocation verdicts"
  (let* ((root (file-name-directory (locate-library "mevedel-review")))
         (directory (make-temp-file "mevedel-review-compile-" t))
         (source (file-name-concat directory "mevedel-review.el"))
         (agents (file-name-concat directory "mevedel-agents.el"))
         (emacs (expand-file-name invocation-name invocation-directory)))
    (unwind-protect
        (progn
          (copy-file (file-name-concat root "mevedel-review.el") source)
          (copy-file (file-name-concat root "mevedel-agents.el") agents)
          (let ((byte-compile-verbose nil))
            (should (byte-compile-file agents)))
          (with-temp-buffer
            (let ((status
                   (call-process
                    emacs nil t nil "--batch" "-Q" "--eval"
                    (prin1-to-string
                     `(progn
                        (setq load-path ',(cons directory load-path))
                        (require 'bytecomp)
                        (when (featurep 'mevedel-agents)
                          (error "Agent structs must start unloaded"))
                        (let ((byte-compile-verbose nil)
                              (byte-compile-error-on-warn t))
                          (unless (byte-compile-file ,source)
                            (error "Review compilation failed")))
                        (load ,(concat source "c") nil t)
                        (require 'mevedel-agents)
                        (let ((invocation (mevedel-agent-invocation--create)))
                          (dolist (verdict '(pass fail partial))
                            (let ((result
                                   (mevedel-review--verify-outcome
                                    (list :status 'ok :kind 'fork
                                          :result (concat "Evidence\nVERDICT: "
                                                          (upcase (symbol-name verdict))))
                                    invocation)))
                              (unless (and (eq verdict (plist-get result :verdict))
                                           (eq verdict (mevedel-agent-invocation-verdict
                                                        invocation)))
                                (error "Verifier verdict was not stored"))))
                          (let ((result
                                 (mevedel-review--verify-outcome
                                  '(:status ok :kind fork :result "Missing verdict")
                                  invocation)))
                            (unless (and (plist-get result :verification-rejected)
                                         (null (mevedel-agent-invocation-verdict invocation)))
                              (error "Invalid report did not clear the verdict")))))))))
              (ert-info ((buffer-string))
                (should (equal 0 status))
                (should (equal "" (buffer-string)))))))
      (delete-directory directory t))))

(provide 'test-mevedel-review-owner-load)
;;; test-mevedel-review-owner-load.el ends here
