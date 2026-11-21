;;; test-niwa-view-control-transfer-owner-load.el -- Cold transfer owner tests -*- lexical-binding: t -*-

;;; Commentary:

;; Verifies the view control-transfer owner without loading the umbrella.

;;; Code:

(require 'bytecomp)

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(niwa-deftest niwa-view-control-transfer/cold-load ()
  ,test
  (test)
  :doc "loads the compiled owner and builds a real read-only descriptor"
  (let* ((root
          (file-name-as-directory
           (file-name-directory (locate-library "niwa"))))
         (compiled-root (make-temp-file "niwa-view-transfer-owner-" t))
         (source "niwa-view-control-transfer.el")
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
                     (require 'niwa-structs)
                     (require 'niwa-view-control-transfer)
                     (let ((data (generate-new-buffer " *cold-data*"))
                           (view (generate-new-buffer " *cold-view*")))
                       (unwind-protect
                           (let ((session
                                  (niwa-session--create :name "cold")))
                             (with-current-buffer data
                               (setq-local niwa--session session)
                               (setq-local niwa-session--read-only-mode t))
                             (with-current-buffer view
                               (setq-local niwa--data-buffer data)
                               (let ((descriptor
                                      (niwa-view-control-transfer-current-descriptor)))
                                 (unless
                                     (string-match-p
                                      "read-only" (plist-get descriptor :body))
                                   (error "Transfer descriptor was not built")))))
                         (kill-buffer data)
                         (kill-buffer view)))
                     (when (featurep 'niwa)
                       (error "Owner loaded the niwa umbrella")))))))
            (should (string-empty-p (string-trim (buffer-string))))))
      (delete-directory compiled-root t))))

(provide 'test-niwa-view-control-transfer-owner-load)
;;; test-niwa-view-control-transfer-owner-load.el ends here
