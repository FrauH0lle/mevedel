;;; test-mevedel-pipeline-diagnostics.el --- Pipeline diagnostic ownership -*- lexical-binding: t -*-

;;; Commentary:

;; Exercise emitted reports separately from canonical tool outcomes.

;;; Code:

(require 'mevedel-pipeline)
(require 'mevedel-tool-registry)
(require 'mevedel-structs)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(mevedel-deftest mevedel-pipeline-run-tool-outcome/diagnostic-owner ()
  ,test
  (test)
  :doc "attributes final callback failures after origin teardown without changing outcomes"
  (let* ((root (make-temp-file "mevedel-pipeline-owner-" t))
         (workspace (mevedel-workspace--create :root root :type 'file :id root))
         (session (mevedel-session--create :workspace workspace
                                           :working-directory root
                                           :permission-mode 'full-auto))
         (origin (generate-new-buffer " *pipeline-origin*"))
         resume outcome diagnostics)
    (unwind-protect
        (progn
          (with-current-buffer origin
            (setq-local default-directory (file-name-as-directory root)
                        mevedel--session session)
            (mevedel-pipeline-run-tool-outcome
             (mevedel-tool--create
              :name "DiagnosticOwner" :read-only-p t :async-p t
              :handler (lambda (callback _args) (setq resume callback)))
             (lambda (value)
               (setq outcome value)
               (error "Injected callback failure"))
             nil))
          (should resume)
          (kill-buffer origin)
          (with-temp-buffer
            (setq-local mevedel--session
                        (mevedel-session--create
                         :workspace (mevedel-workspace--create :root "/other/project")))
            (mevedel-test--with-captured-diagnostics diagnostics
              (funcall resume '(:result "raw tool output" :status success))))
          (should (string-search
                   (format "mevedel [%s]: Pipeline final callback signaled:" root)
                   diagnostics))
          (should (eq (plist-get outcome :status) 'success))
          (should (equal (plist-get outcome :result) "raw tool output"))
          (should-not (plist-member outcome :diagnostic-scope))
          (should-not (string-search root (prin1-to-string outcome))))
      (when (buffer-live-p origin) (kill-buffer origin))
      (delete-directory root t))))

(provide 'test-mevedel-pipeline-diagnostics)
;;; test-mevedel-pipeline-diagnostics.el ends here
