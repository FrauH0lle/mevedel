;;; test-mevedel-loading.el --- Fresh feature boundary tests -*- lexical-binding: t -*-

;;; Commentary:
;; Each source and compiled probe starts without umbrella-preloaded fixtures.

;;; Code:
(require 'ert)
(require 'cl-lib)

(ert-deftest mevedel-loading/features ()
  (should (string-match-p "mevedel-test-home-" (getenv "HOME")))
  (let* ((root (file-name-directory (locate-library "mevedel")))
         (compiled (make-temp-file "mevedel-loading-compiled-" t))
         (emacs (expand-file-name invocation-name invocation-directory)))
    (unwind-protect
        (progn
          ;; Compile in a separate process too.  Source siblings preserve the
          ;; package's ordinary prompt and resource resolution.
          (dolist (file (directory-files root t "\\`mevedel.*\\.el\\'"))
            (make-symbolic-link file (file-name-concat compiled (file-name-nondirectory file))))
          (with-temp-buffer
            (let ((status
                   (call-process
                    emacs nil t nil "--batch" "-Q" "--eval"
                    (prin1-to-string
                     `(progn
                        (setq load-path ',(cons compiled load-path))
                        (require 'bytecomp)
                        (let ((byte-compile-error-on-warn t)
                              (byte-compile-verbose nil))
                          (dolist (file (directory-files ,compiled t "\\.el\\'"))
                            (unless (byte-compile-file file)
                              (error "Compilation failed: %s" file)))))))))
              (ert-info ((buffer-string))
                (should (equal 0 status))
                (should (equal "" (buffer-string))))))
          (dolist (entry (list root compiled))
            (dolist (scenario '(installation gptel chat chat-in-directory
                               tools renderers claude lobby room
                               directive-preview-implement directive-preview-discuss
                               directive-answer))
              (with-temp-buffer
                (let ((status
                       (call-process
                        emacs nil t nil "--batch" "-Q" "--eval"
                        (prin1-to-string
                         `(progn
                            (setq load-path ',(cons entry load-path))
                            (load ,(file-name-concat root "test/fixtures/loading.el") nil t)
                            (mevedel-test-loading ',scenario))))))
                  (ert-info ((format "%s %s: %s" entry scenario (buffer-string)))
                    (should (equal 0 status))
                    (should (equal "" (buffer-string)))))))))
      (delete-directory compiled t))))

(provide 'test-mevedel-loading)
;;; test-mevedel-loading.el ends here
