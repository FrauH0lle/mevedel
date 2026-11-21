;;; test-niwa-execution-owner-load.el -- Cold execution owner tests -*- lexical-binding: t -*-

;;; Commentary:

;; Verifies the execution owners without loading the niwa umbrella.

;;; Code:

(require 'bytecomp)

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(niwa-deftest niwa-execution-owners/cold-load ()
  ,test
  (test)
  :doc "loads each compiled owner directly and exercises its public boundary"
  (let* ((root
          (file-name-as-directory
           (file-name-directory (locate-library "niwa"))))
         (compiled-root (make-temp-file "niwa-execution-owner-" t))
         (emacs (expand-file-name invocation-name invocation-directory))
         (owners '("niwa-execution.el"
                   "niwa-execution-process.el"
                   "niwa-execution-telemetry.el"))
         (cases
          '((process
             (progn
               (require 'niwa-execution-process)
               (let* ((directory (make-temp-file "niwa-process-cold-" t))
                      (spool (file-name-concat directory "output"))
                      done result child)
                 (unwind-protect
                     (progn
                       (write-region "" nil spool nil 'silent)
                       (setq child
                             (niwa-execution-process-create
                              :workdir directory :spool-path spool
                              :terminal-function
                              (lambda (_child value)
                                (setq result value done t))))
                       (niwa-execution-process-start
                        child :name "niwa-cold-process"
                        :command '("sh" "-c" "printf cold")
                        :coding 'utf-8-unix)
                       (while (not done)
                         (accept-process-output nil 0.02))
                       (unless (and (= 0 (plist-get result :exit-code))
                                    (equal "cold"
                                           (plist-get result :output)))
                         (error "Process owner did not settle correctly")))
                   (when child (niwa-execution-process-release child))
                   (delete-directory directory t)))
               (when (featurep 'niwa-execution)
                 (error "Process owner loaded the execution facade"))))
            (telemetry
             (progn
               (require 'niwa-execution-telemetry)
               (let ((properties
                      (niwa-execution-telemetry-command-properties
                       "eask test ert")))
                 (unless (and (eq 'eask (plist-get properties :workload))
                              (eq 'full (plist-get properties :test-scope)))
                   (error "Telemetry owner did not classify a command")))
               (when (featurep 'niwa-execution)
                 (error "Telemetry owner loaded the execution facade"))))
            (facade
             (progn
               (require 'niwa-execution)
               (set 'niwa-sandbox-mode 'off)
               (let ((directory (make-temp-file "niwa-execution-cold-" t)))
                 (unwind-protect
                     (let ((result
                            (niwa-execution-run-one-shot
                             :name "niwa-cold-execution"
                             :command '("sh" "-c" "printf facade")
                             :workdir directory
                             :writable-roots (list directory))))
                       (unless (equal "facade" (plist-get result :output))
                         (error "Execution facade did not run a child")))
                   (delete-directory directory t)))
               (when (featurep 'niwa)
                 (error "Execution facade loaded the niwa umbrella")))))))
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

(provide 'test-niwa-execution-owner-load)
;;; test-niwa-execution-owner-load.el ends here
