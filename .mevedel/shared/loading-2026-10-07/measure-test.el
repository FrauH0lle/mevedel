;;; measure-test.el --- Isolated loading measurement launcher -*- lexical-binding: t -*-
(require 'ert)
(ert-deftest mevedel-loading/measurement ()
  (should (string-match-p "mevedel-test-home-" (getenv "HOME")))
  (let* ((root (expand-file-name default-directory))
         (output (file-name-concat root ".scratch/loading-measurement"))
         (worker (file-name-concat root ".mevedel/shared/loading-2026-10-07/measure-worker.el"))
         (emacs (expand-file-name invocation-name invocation-directory)))
    (dolist (variant '("baseline" "current"))
      (let* ((source (if (equal variant "baseline") (file-name-concat output variant) root))
             (compiled (file-name-concat output (concat variant "-compiled"))))
        (make-directory compiled t)
        (dolist (file (directory-files source t "\\`mevedel.*\\.el\\'"))
          (make-symbolic-link file (file-name-concat compiled (file-name-nondirectory file)) t))
        (with-temp-buffer
          (let ((status
                 (call-process emacs nil t nil "-Q" "--batch" "--eval"
                               (prin1-to-string
                                `(progn
                                   (setq load-path ',(append (list compiled source) load-path))
                                   (require 'cl-lib)
                                   (require 'mevedel)
                                   (require 'bytecomp)
                                   (let ((byte-compile-error-on-warn t) (byte-compile-verbose nil))
                                     (dolist (file (cons (file-name-concat ,compiled "mevedel.el")
                                                       (delete (file-name-concat ,compiled "mevedel.el")
                                                               (directory-files ,compiled t "\\.el\\'"))))
                                       (unless (byte-compile-file file) (error "Compile failed: %s" file)))))))))
            (ert-info ((buffer-string)) (should (= status 0)) (should (equal "" (buffer-string))))))))
    (dotimes (iteration 5)
      (dolist (variant '("baseline" "current"))
        (dolist (mode '("compiled" "source"))
          (dolist (preload '(nil t))
            (dolist (scenario (if preload '(view) '(view tool)))
              (let* ((source (if (equal variant "baseline") (file-name-concat output variant) root))
                     (entry (if (equal mode "source") source
                              (file-name-concat output (concat variant "-compiled"))))
                     (stem (format "%s-%s-%s-%s-%s" variant mode preload scenario iteration))
                     (result (file-name-concat output (concat stem ".json"))))
                (with-temp-buffer
                  (let ((status
                         (call-process emacs nil t nil "-Q" "--batch" "--eval"
                                       (prin1-to-string
                                        `(progn
                                           (setq load-path ',(append (list entry source) load-path)
                                                 lazy-scan-mode ,mode lazy-scan-preload ,preload
                                                 lazy-scan-variant ,variant lazy-scan-scenario ',scenario
                                                 lazy-scan-output ,result)
                                           (load ,worker nil t))))))
                    (write-region (point-min) (point-max) (file-name-concat output (concat stem ".log")) nil 'silent)
                    (ert-info ((buffer-string)) (should (= status 0)))))))))))))
