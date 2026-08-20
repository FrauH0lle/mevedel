;;; test-mevedel-view-disclosure-owner-load.el -- Cold disclosure owner test -*- lexical-binding: t -*-

;;; Commentary:

;; Verifies disclosure actions without loading the mevedel umbrella.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(mevedel-deftest mevedel-view-disclosure/cold-load ()
  ,test
  (test)
  :doc "loads the compiled owner and expands a real response disclosure"
  (let* ((root
          (file-name-as-directory
           (file-name-directory (locate-library "mevedel"))))
         (compiled-root (make-temp-file "mevedel-view-disclosure-owner-" t))
         (emacs (expand-file-name invocation-name invocation-directory))
         (owner "mevedel-view-disclosure.el"))
    (unwind-protect
        (progn
          (copy-file (file-name-concat root owner)
                     (file-name-concat compiled-root owner))
          (let ((byte-compile-verbose nil))
            (byte-compile-file (file-name-concat compiled-root owner)))
          (with-temp-buffer
            (should
             (= 0
                (call-process
                 emacs nil t nil
                 "--batch" "-Q" "-L" compiled-root "-L" root
                 "--eval"
                 (prin1-to-string
                  '(progn
                     (require 'mevedel-view-disclosure)
                     (require 'mevedel-view)
                     (let ((data (generate-new-buffer " *disclosure-cold-data*"))
                           (view (generate-new-buffer " *disclosure-cold-view*")))
                       (unwind-protect
                           (progn
                             (with-current-buffer data
                               (org-mode)
                               (insert (propertize
                                        "First line\nSecond line\nThird line\n"
                                        'gptel 'response)))
                             (mevedel-view--setup view data)
                             (require 'mevedel-view-render)
                             (with-current-buffer view
                               (mevedel-view--full-rerender)
                               (goto-char (point-min))
                               (search-forward "Assistant")
                               (goto-char (match-beginning 0))
                               (mevedel-view-toggle-section)
                               (unless (string-search "First line" (buffer-string))
                                 (error "Response disclosure did not expand")))
                             (when (featurep 'mevedel)
                               (error "Owner loaded the mevedel umbrella")))
                         (when (buffer-live-p view)
                           (kill-buffer view))
                         (when (buffer-live-p data)
                           (kill-buffer data)))))))))
            (should (string-empty-p (string-trim (buffer-string))))))
      (delete-directory compiled-root t))))

(provide 'test-mevedel-view-disclosure-owner-load)
;;; test-mevedel-view-disclosure-owner-load.el ends here
