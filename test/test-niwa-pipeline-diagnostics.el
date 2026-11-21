;;; test-niwa-pipeline-diagnostics.el --- Pipeline diagnostic ownership -*- lexical-binding: t -*-

;;; Commentary:

;; Exercise emitted reports separately from canonical tool outcomes.

;;; Code:

(require 'niwa-pipeline)
(require 'niwa-tool-registry)
(require 'niwa-structs)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(niwa-deftest niwa-pipeline-run-tool-outcome/diagnostic-owner ()
  ,test
  (test)
  :doc "attributes final callback failures after origin teardown without changing outcomes"
  (let* ((root (make-temp-file "niwa-pipeline-owner-" t))
         (workspace (niwa-workspace--create :root root :type 'file :id root))
         (session (niwa-session--create :workspace workspace
                                           :working-directory root
                                           :permission-mode 'full-auto))
         (origin (generate-new-buffer " *pipeline-origin*"))
         resume outcome diagnostics)
    (unwind-protect
        (progn
          (with-current-buffer origin
            (setq-local default-directory (file-name-as-directory root)
                        niwa--session session)
            (niwa-pipeline-run-tool-outcome
             (niwa-tool--create
              :name "DiagnosticOwner" :read-only-p t :async-p t
              :handler (lambda (callback _args) (setq resume callback)))
             (lambda (value)
               (setq outcome value)
               (error "Injected callback failure"))
             nil))
          (should resume)
          (kill-buffer origin)
          (with-temp-buffer
            (setq-local niwa--session
                        (niwa-session--create
                         :workspace (niwa-workspace--create :root "/other/project")))
            (niwa-test--with-captured-diagnostics diagnostics
              (funcall resume '(:result "raw tool output" :status success))))
          (should (string-search
                   (format "niwa [%s]: Pipeline final callback signaled:" root)
                   diagnostics))
          (should (eq (plist-get outcome :status) 'success))
          (should (equal (plist-get outcome :result) "raw tool output"))
          (should-not (plist-member outcome :diagnostic-scope))
          (should-not (string-search root (prin1-to-string outcome))))
      (when (buffer-live-p origin) (kill-buffer origin))
      (delete-directory root t))))

(provide 'test-niwa-pipeline-diagnostics)
;;; test-niwa-pipeline-diagnostics.el ends here
