;;; mevedel-bench-runner.el --- Isolated compiled benchmark runner -*- lexical-binding: t; -*-

;;; Commentary:

;; Eask loads this bootstrap, which compiles into a disposable directory and
;; starts a fresh Emacs with those artifacts ahead of the source load path.
;; Never load this file into an interactive Emacs.

;;; Code:

(require 'bytecomp)

(unless noninteractive
  (error "Benchmarks require batch Emacs"))

(let* ((root (file-name-directory
              (directory-file-name
               (file-name-directory (or load-file-name buffer-file-name)))))
       (temporary (make-temp-file "mevedel-bench-" t))
       (compiled (file-name-concat temporary "compiled"))
       (home (file-name-concat temporary "home"))
       (mode (car (last command-line-args-left)))
       (byte-compile-error-on-warn t)
       (byte-compile-dest-file-function
        (lambda (file)
          (file-name-concat compiled (concat (file-name-base file) ".elc")))))
  (unless (equal mode "gate")
    (error "Usage: ... -l benchmark/mevedel-bench-runner.el -- gate"))
  (unwind-protect
      (progn
        (make-directory compiled t)
        (make-directory home t)
        (setq default-directory (expand-file-name default-directory))
        (setenv "HOME" home)
        (setq abbreviated-home-dir nil)
        (dolist (name '("XDG_CACHE_HOME" "XDG_CONFIG_HOME" "XDG_DATA_HOME"
                        "XDG_STATE_HOME" "XDG_RUNTIME_DIR"))
          (let ((path (file-name-concat home name)))
            (make-directory path t)
            (set-file-modes path #o700)
            (setenv name path)))
        (startup-redirect-eln-cache (file-name-concat home "eln"))
        (add-to-list 'load-path (file-name-concat root "benchmark"))
        (let ((inhibit-message t))
          (dolist (file (append (directory-files root t "\\`mevedel.*\\.el\\'")
                                (directory-files
                                 (file-name-concat root "benchmark") t
                                 "\\`mevedel-\\(?:allocation-gate\\|view-fixtures\\)\\.el\\'")))
            (unless (byte-compile-file file)
              (when (get-buffer "*Compile-Log*")
                (princ (with-current-buffer "*Compile-Log*" (buffer-string))))
              (error "Compilation failed: %s" file))))
        (with-temp-buffer
          (let ((status
               (call-process
                (expand-file-name invocation-name invocation-directory)
                nil t t "--batch" "-Q"
                "--eval" (prin1-to-string
                          `(setq load-path ',(cons compiled load-path)))
                "--eval" (prin1-to-string
                          `(startup-redirect-eln-cache
                            ,(file-name-concat home "eln")))
                "-l" (file-name-concat compiled "mevedel-allocation-gate.elc")
                "--eval" "(mevedel-allocation-gate-run)")))
            (princ (buffer-string))
            (unless (eql status 0)
              (error "Benchmark subprocess failed: %s" status)))))
    (delete-directory temporary t)))

;;; mevedel-bench-runner.el ends here
