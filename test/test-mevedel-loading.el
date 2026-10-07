;;; test-mevedel-loading.el --- Fresh feature boundary tests -*- lexical-binding: t -*-

;;; Commentary:
;; Each source and compiled probe starts without umbrella-preloaded fixtures.

;;; Code:
(require 'ert)
(require 'cl-lib)

(defun mevedel-loading--emacs (form)
  "Evaluate FORM in a fresh batch Emacs; return its exit status and output.
A child that outlives two minutes is killed and fails the test."
  (with-temp-buffer
    (let ((process
           (make-process
            :name "mevedel-loading" :buffer (current-buffer) :noquery t
            :connection-type 'pipe :sentinel #'ignore
            :command (list (expand-file-name invocation-name invocation-directory)
                           "--batch" "-Q" "--eval" (prin1-to-string form)))))
      (with-timeout (120 (delete-process process)
                         (ert-fail (format "Child Emacs timed out: %s" (buffer-string))))
        (while (process-live-p process) (accept-process-output process 0.05)))
      (accept-process-output process 0)
      (list (process-exit-status process) (buffer-string)))))

(ert-deftest mevedel-loading/features ()
  (should (string-match-p "mevedel-test-home-" (getenv "HOME")))
  (let* ((root (file-name-directory (locate-library "mevedel")))
         (fixture (file-name-concat root "test/fixtures/loading.el"))
         (compiled (make-temp-file "mevedel-loading-compiled-" t))
         (state (make-temp-file "mevedel-loading-session-" t)))
    (cl-flet ((run (entry scenario &optional state)
                (pcase-let ((`(,status ,output)
                             (mevedel-loading--emacs
                              `(progn
                                 (setq load-path ',(cons entry load-path))
                                 (load ,fixture nil t)
                                 (mevedel-test-loading ',scenario ,state)))))
                  (ert-info ((format "%s %s: %s" entry scenario output))
                    (should (equal 0 status))
                    (should (equal "" output))))))
      (unwind-protect
          (progn
            ;; Compile every file in one process, as package managers do, so
            ;; earlier files' definitions are known while later ones compile.
            ;; Source siblings preserve ordinary prompt and resource resolution.
            (dolist (file (directory-files root t "\\`mevedel.*\\.el\\'"))
              (make-symbolic-link file (file-name-concat compiled (file-name-nondirectory file))))
            (pcase-let ((`(,status ,output)
                         (mevedel-loading--emacs
                          `(progn
                             (setq load-path ',(cons compiled load-path))
                             (require 'bytecomp)
                             (let ((byte-compile-error-on-warn t)
                                   (byte-compile-verbose nil))
                               (dolist (file (directory-files ,compiled t "\\.el\\'"))
                                 (unless (byte-compile-file file)
                                   (error "Compilation failed: %s" file))))))))
              (ert-info (output)
                (should (equal 0 status))
                (should (equal "" output))))
            (dolist (entry (list root compiled))
              (dolist (scenario '(commands installation gptel chat chat-in-directory
                                  tools renderers claude lobby room discuss implement
                                  directive-preview-implement directive-preview-discuss
                                  directive-answer))
                (run entry scenario))
              ;; The first send saves a session that a second Emacs resumes.
              (run entry 'send state)
              (run entry 'resume state)
              (delete-directory state t)
              (make-directory state)))
        (delete-directory compiled t)
        (delete-directory state t)))))

(provide 'test-mevedel-loading)
;;; test-mevedel-loading.el ends here
