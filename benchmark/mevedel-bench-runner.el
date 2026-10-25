;;; mevedel-bench-runner.el --- Isolated compiled benchmark runner -*- lexical-binding: t; -*-

;;; Commentary:

;; Eask loads this bootstrap, which compiles into a disposable directory and
;; starts a fresh Emacs with those artifacts ahead of the source load path.
;; Never load this file into an interactive Emacs.

;;; Code:

(require 'bytecomp)
(require 'cl-lib)

(unless noninteractive
  (error "Benchmarks require batch Emacs"))

(let* ((root (file-name-directory
              (directory-file-name
               (file-name-directory (or load-file-name buffer-file-name)))))
       (temporary (make-temp-file "mevedel-bench-" t))
       (compiled (file-name-concat temporary "compiled"))
       (home (file-name-concat temporary "home"))
       (mode (car (last command-line-args-left)))
       (byte-compile-error-on-warn t))
  (unless (member mode '("gate" "twin"))
    (error "Usage: ... -l benchmark/mevedel-bench-runner.el -- gate|twin"))
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
        (when (native-comp-available-p)
          (startup-redirect-eln-cache (file-name-concat home "eln")))
        (add-to-list 'load-path (file-name-concat root "benchmark"))
        (cl-labels
            ((compile-build (destination)
               (make-directory destination t)
               (let ((inhibit-message t)
                     (byte-compile-dest-file-function
                      (lambda (file)
                        (file-name-concat destination
                                          (concat (file-name-base file) ".elc")))))
                 (dolist (file (append (directory-files root t "\\`mevedel.*\\.el\\'")
                                      (directory-files
                                       (file-name-concat root "benchmark") t
                                       "\\`mevedel-\\(?:allocation-gate\\|bench-runner\\|view-fixtures\\|minimal-edit-twin\\|view-twin\\)\\.el\\'")))
                   (unless (byte-compile-file file)
                     (when (get-buffer "*Compile-Log*")
                       (princ (with-current-buffer "*Compile-Log*" (buffer-string))))
                     (error "Compilation failed: %s" file)))))
             (run-build (destination library expression)
               (with-temp-buffer
                 (let ((status
                        (call-process
                         (expand-file-name invocation-name invocation-directory)
                         nil t t "--batch" "-Q"
                         "--eval" (prin1-to-string
                                   `(setq load-path ',(cons destination load-path)))
                         "--eval" (prin1-to-string
                                   `(when (native-comp-available-p)
                                      (startup-redirect-eln-cache
                                       ,(file-name-concat home "eln"))))
                         "-l" (file-name-concat destination (concat library ".elc"))
                         "--eval" expression)))
                   (princ (buffer-string))
                   (unless (eql status 0)
                     (error "Benchmark subprocess failed: %s" status))))))
          (compile-build compiled)
          (if (equal mode "gate")
              (run-build compiled "mevedel-allocation-gate" "(mevedel-allocation-gate-run)")
            (run-build compiled "mevedel-view-twin" "(mevedel-view-twin-run)")
            ;; Ordinary insert/delete opcodes bypass advice.  Only the count
            ;; build uses generic calls; never time this instrumented build.
            (let ((symbols '(insert delete-region))
                  (handlers (mapcar (lambda (symbol) (get symbol 'byte-compile))
                                    '(insert delete-region)))
                  (observed (file-name-concat temporary "observed")))
              (unwind-protect
                  (progn
                    (dolist (symbol symbols) (put symbol 'byte-compile nil))
                    (compile-build observed))
                (cl-mapc (lambda (symbol handler) (put symbol 'byte-compile handler))
                         symbols handlers))
              (run-build observed "mevedel-view-twin" "(mevedel-view-twin-run t)")))))
    (delete-directory temporary t)))

(provide 'mevedel-bench-runner)
;;; mevedel-bench-runner.el ends here
