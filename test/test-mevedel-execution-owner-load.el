;;; test-mevedel-execution-owner-load.el -- Cold execution owner tests -*- lexical-binding: t -*-

;;; Commentary:

;; Verifies the execution owners without loading the mevedel umbrella.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(mevedel-deftest mevedel-execution-owners/cold-load ()
  ,test
  (test)
  :doc "loads each compiled owner directly and exercises its public boundary"
  (let* ((root
          (file-name-as-directory
           (file-name-directory (locate-library "mevedel"))))
         (compiled-root (make-temp-file "mevedel-execution-owner-" t))
         (emacs (expand-file-name invocation-name invocation-directory))
         (owners '("mevedel-execution.el"
                   "mevedel-execution-process.el"
                   "mevedel-execution-telemetry.el"))
         (cases
          '((process
             (progn
               (require 'mevedel-execution-process)
               (let* ((directory (make-temp-file "mevedel-process-cold-" t))
                      (spool (file-name-concat directory "output"))
                      done result child)
                 (unwind-protect
                     (progn
                       (write-region "" nil spool nil 'silent)
                       (setq child
                             (mevedel-execution-process-create
                              :workdir directory :spool-path spool
                              :terminal-function
                              (lambda (_child value)
                                (setq result value done t))))
                       (mevedel-execution-process-start
                        child :name "mevedel-cold-process"
                        :command '("sh" "-c" "printf cold")
                        :coding 'utf-8-unix)
                       (while (not done)
                         (accept-process-output nil 0.02))
                       (unless (and (= 0 (plist-get result :exit-code))
                                    (equal "cold"
                                           (plist-get result :output)))
                         (error "Process owner did not settle correctly")))
                   (when child (mevedel-execution-process-release child))
                   (delete-directory directory t)))
               (when (featurep 'mevedel-execution)
                 (error "Process owner loaded the execution facade"))))
            (telemetry
             (progn
               (require 'mevedel-execution-telemetry)
               (let ((properties
                      (mevedel-execution-telemetry-command-properties
                       "eask test ert")))
                 (unless (and (eq 'eask (plist-get properties :workload))
                              (eq 'full (plist-get properties :test-scope)))
                   (error "Telemetry owner did not classify a command")))
               (when (featurep 'mevedel-execution)
                 (error "Telemetry owner loaded the execution facade"))))
            (facade
             (progn
               (require 'mevedel-execution)
               (set 'mevedel-sandbox-mode 'off)
               (let ((directory (make-temp-file "mevedel-execution-cold-" t)))
                 (unwind-protect
                     (let ((result
                            (mevedel-execution-run-one-shot
                             :name "mevedel-cold-execution"
                             :command '("sh" "-c" "printf facade")
                             :workdir directory
                             :writable-roots (list directory))))
                       (unless (equal "facade" (plist-get result :output))
                         (error "Execution facade did not run a child")))
                   (delete-directory directory t)))
               (when (featurep 'mevedel)
                 (error "Execution facade loaded the mevedel umbrella")))))))
    (unwind-protect
        (progn
          (dolist (owner owners)
            (copy-file (file-name-concat root owner)
                       (file-name-concat compiled-root owner))
            (let ((byte-compile-verbose nil))
              (byte-compile-file (file-name-concat compiled-root owner))))
          (dolist (case cases)
            (with-temp-buffer
              (ert-info ((format "cold owner: %s" (car case)))
                (should
                 (= 0
                    (call-process
                     emacs nil t nil
                     "--batch" "-Q" "-L" compiled-root "-L" root
                     "--eval" (prin1-to-string (cadr case)))))
                (should
                 (string-empty-p (string-trim (buffer-string))))))))
      (delete-directory compiled-root t))))

(provide 'test-mevedel-execution-owner-load)
;;; test-mevedel-execution-owner-load.el ends here
