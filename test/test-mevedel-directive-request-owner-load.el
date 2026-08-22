;;; test-mevedel-directive-request-owner-load.el -- Cold directive-request owner test -*- lexical-binding: t -*-

;;; Commentary:

;; Verifies the directive request owner loads compiled, standalone, and
;; without loading the mevedel umbrella.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(mevedel-deftest mevedel-directive-request/cold-load ()
  ,test
  (test)
  :doc "loads the compiled owner and builds a discussion prompt"
  (let* ((root
          (file-name-as-directory
           (file-name-directory (locate-library "mevedel"))))
         (compiled-root (make-temp-file "mevedel-directive-request-owner-" t))
         (emacs (expand-file-name invocation-name invocation-directory))
         (owner "mevedel-directive-request.el"))
    (unwind-protect
        (progn
          (copy-file (file-name-concat root owner)
                     (file-name-concat compiled-root owner))
          (let ((byte-compile-verbose nil)
                (byte-compile-warnings nil))
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
                     (require 'mevedel-directive-request)
                     (let ((prompt (mevedel--discuss-directive-prompt
                                    "Where is the entry point?")))
                       (unless (and (string-search "## TASK:" prompt)
                                    (string-search
                                     "Where is the entry point?" prompt))
                         (error "Discussion prompt lost its shape")))
                     (unless (eq 'mevedel-directive-request
                                 (intern
                                  (file-name-base
                                   (symbol-file 'mevedel--process-directive))))
                       (error "Owner does not define the directive processor"))
                     (when (featurep 'mevedel)
                       (error "Owner loaded the mevedel umbrella")))))))
            (should (string-empty-p (string-trim (buffer-string))))))
      (delete-directory compiled-root t))))

(provide 'test-mevedel-directive-request-owner-load)
;;; test-mevedel-directive-request-owner-load.el ends here
