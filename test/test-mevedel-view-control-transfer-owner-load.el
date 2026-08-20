;;; test-mevedel-view-control-transfer-owner-load.el -- Cold transfer owner tests -*- lexical-binding: t -*-

;;; Commentary:

;; Verifies the view control-transfer owner without loading the umbrella.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(mevedel-deftest mevedel-view-control-transfer/cold-load ()
  ,test
  (test)
  :doc "loads the compiled owner and builds a real read-only descriptor"
  (let* ((root
          (file-name-as-directory
           (file-name-directory (locate-library "mevedel"))))
         (compiled-root (make-temp-file "mevedel-view-transfer-owner-" t))
         (source "mevedel-view-control-transfer.el")
         (emacs (expand-file-name invocation-name invocation-directory)))
    (unwind-protect
        (progn
          (copy-file (file-name-concat root source)
                     (file-name-concat compiled-root source))
          (let ((byte-compile-verbose nil))
            (byte-compile-file (file-name-concat compiled-root source)))
          (with-temp-buffer
            (should
             (= 0
                (call-process
                 emacs nil t nil
                 "--batch" "-Q" "-L" compiled-root "-L" root
                 "--eval"
                 (prin1-to-string
                  '(progn
                     (require 'mevedel-structs)
                     (require 'mevedel-view-control-transfer)
                     (let ((data (generate-new-buffer " *cold-data*"))
                           (view (generate-new-buffer " *cold-view*")))
                       (unwind-protect
                           (let ((session
                                  (mevedel-session--create :name "cold")))
                             (with-current-buffer data
                               (setq-local mevedel--session session)
                               (setq-local mevedel-session--read-only-mode t))
                             (with-current-buffer view
                               (setq-local mevedel--data-buffer data)
                               (let ((descriptor
                                      (mevedel-view-control-transfer-current-descriptor)))
                                 (unless
                                     (string-match-p
                                      "read-only" (plist-get descriptor :body))
                                   (error "Transfer descriptor was not built")))))
                         (kill-buffer data)
                         (kill-buffer view)))
                     (when (featurep 'mevedel)
                       (error "Owner loaded the mevedel umbrella")))))))
            (should (string-empty-p (string-trim (buffer-string))))))
      (delete-directory compiled-root t))))

(provide 'test-mevedel-view-control-transfer-owner-load)
;;; test-mevedel-view-control-transfer-owner-load.el ends here
