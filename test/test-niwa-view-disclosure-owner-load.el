;;; test-niwa-view-disclosure-owner-load.el -- Cold disclosure owner test -*- lexical-binding: t -*-

;;; Commentary:

;; Verifies disclosure actions without loading the niwa umbrella.

;;; Code:

(require 'bytecomp)

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(niwa-deftest niwa-view-disclosure/cold-load ()
  ,test
  (test)
  :doc "loads the compiled owner and expands a real response disclosure"
  (let* ((root
          (file-name-as-directory
           (file-name-directory (locate-library "niwa"))))
         (compiled-root (make-temp-file "niwa-view-disclosure-owner-" t))
         (emacs (expand-file-name invocation-name invocation-directory))
         (owner "niwa-view-disclosure.el"))
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
                     (require 'niwa-view-disclosure)
                     (require 'niwa-view)
                     (let ((data (generate-new-buffer " *disclosure-cold-data*"))
                           (view (generate-new-buffer " *disclosure-cold-view*")))
                       (unwind-protect
                           (progn
                             (with-current-buffer data
                               (org-mode)
                               (insert (propertize
                                        "First line\nSecond line\nThird line\n"
                                        'gptel 'response)))
                             (niwa-view--setup view data)
                             (require 'niwa-view-render)
                             (with-current-buffer view
                               (niwa-view--full-rerender)
                               (goto-char (point-min))
                               (search-forward "Assistant")
                               (goto-char (match-beginning 0))
                               (niwa-view-toggle-section)
                               (unless (string-search "First line" (buffer-string))
                                 (error "Response disclosure did not expand")))
                             (when (featurep 'niwa)
                               (error "Owner loaded the niwa umbrella")))
                         (when (buffer-live-p view)
                           (kill-buffer view))
                         (when (buffer-live-p data)
                           (kill-buffer data)))))))))
            (should (string-empty-p (string-trim (buffer-string))))))
      (delete-directory compiled-root t))))

(provide 'test-niwa-view-disclosure-owner-load)
;;; test-niwa-view-disclosure-owner-load.el ends here
