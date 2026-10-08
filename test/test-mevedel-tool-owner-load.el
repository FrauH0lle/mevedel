;;; test-mevedel-tool-owner-load.el -- Cold tool owner tests -*- lexical-binding: t -*-

;;; Commentary:

;; Verifies the extracted tool owners without loading the mevedel umbrella.

;;; Code:

(require 'mevedel-tool-exec-registration)

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(mevedel-deftest mevedel-tool-owners/cold-load ()
  ,test
  (test)
  :doc "loads compiled render and permission owners through real public seams"
  (let* ((root
          (file-name-as-directory
           (file-name-directory (locate-library "mevedel"))))
         (compiled-root (make-temp-file "mevedel-tool-owner-" t))
         (emacs (expand-file-name invocation-name invocation-directory))
         (owners '("mevedel-bash-policy.el"
                   "mevedel-hooks.el"
                   "mevedel-permission-review.el"
                   "mevedel-pipeline.el"
                   "mevedel-tool-exec-permission.el"
                   "mevedel-tool-exec.el"
                   "mevedel-tool-exec-registration.el"
                   "mevedel-tool-fs-read.el"
                   "mevedel-tool-fs-search.el"
                   "mevedel-tool-fs.el"
                   "mevedel-tool-patch.el"
                   "mevedel-tool-permission.el"
                   "mevedel-tool-render-data.el"))
         (cases
          '((bash-policy
             (progn
               (require 'mevedel-bash-policy)
               (unless (equal "git (2), rg"
                              (mevedel-bash-policy-commands-summary
                               '("git" "git" "rg")))
                 (error "Bash policy owner did not summarize commands"))
               (unless
                   (string-suffix-p
                    "mevedel-bash-policy.elc"
                    (or (symbol-file 'mevedel-bash-policy-commands-summary
                                     'defun)
                        ""))
                 (error "Bash policy behavior has the wrong owner"))))
            (approval-review
            (progn
               (require 'mevedel-permission-review)
               (require 'gptel-openai)
               (let ((mevedel-permission-reviewer 'auto)
                     (gptel-backend (gptel-make-openai "Cold reviewer" :key "unused"))
                     (gptel-model 'gpt-4o-mini)
                     results)
                 (with-temp-buffer
                   (org-mode)
                   (insert "Evaluate (+ 1 2) in this Emacs.\n")
                   (let* ((session (mevedel-session--create
                                    :permission-mode 'edits
                                    :root-buffer (current-buffer)))
                          (entry (list :kind 'eval :session session
                                       :data-buffer (current-buffer)
                                       :expression "(+ 1 2)" :mode "live"
                                       :callback (lambda (value) (push value results)))))
                     (cl-letf (((symbol-function 'gptel-request)
                                (lambda (_prompt &rest args)
                                  (let ((callback (plist-get args :callback)))
                                    (funcall callback
                                             "{\"decision\":\"allow-once\",\"reason\":\"Exact user request\"}"
                                             nil)
                                    (funcall callback 'abort nil)))))
                       (mevedel-permission-review-start
                        entry (lambda () (error "Unexpected human fallback")))))
                   (unless (equal '(allow-once) results)
                     (error "Cold reviewer did not settle once: %S" results)))
                 (unless (string-suffix-p
                          "mevedel-permission-review.elc"
                          (or (symbol-file 'mevedel-permission-review-start 'defun) ""))
                   (error "Reviewer did not load its compiled owner")))))
            (exec-permission
             (progn
               (require 'mevedel-structs)
               (require 'mevedel-permissions)
               ;; The mode and rule owners define the variables this case
               ;; binds.  Without them the let binds lexically and the owner's
               ;; own defcustom then errors, surfacing as a spurious deny.
               (require 'mevedel-permission-mode)
               (require 'mevedel-permission-rules)
               (require 'mevedel-tool-exec-permission)
               (let ((mevedel-permission-mode 'full-auto)
                     (mevedel-permission-rules nil)
                     outcome)
                 (mevedel-tool-exec-permission-check-eval-async
                  nil '(:expression "(+ 1 2)")
                  (lambda (result) (setq outcome result)))
                 (unless (eq 'allow outcome)
                   (error "Execution permission owner did not allow Eval")))
               (when (featurep 'mevedel-permission-queue)
                 (error "Automatic Eval loaded the permission queue"))
               (let ((mevedel-permission-rules nil)
                     (session (mevedel-session--create
                               :name "cold-prompt" :permission-mode 'ask))
                     outcome)
                 (with-temp-buffer
                   (setq-local mevedel--session session)
                   (cl-letf (((symbol-function 'mevedel-permission-prompt-render)
                              (lambda (_entry _origin cont _count)
                                (funcall cont 'deny-once))))
                     (mevedel-tool-exec-permission-check-eval-async
                      nil '(:expression "(+ 1 2)")
                      (lambda (result) (setq outcome result)))))
                 (unless (and (eq 'deny outcome)
                              (featurep 'mevedel-permission-queue)
                              (null (mevedel-session-permission-queue session)))
                   (error "Cold Eval prompt did not settle through the queue")))
               (unless
                   (string-suffix-p
                    "mevedel-tool-exec-permission.elc"
                    (or (symbol-file
                         'mevedel-tool-exec-permission-check-eval-async
                         'defun)
                        ""))
                 (error "Execution permission behavior has the wrong owner"))))
            (exec-facade
             (progn
               (require 'mevedel-execution-target)
               (require 'mevedel-structs)
               (require 'mevedel-tool-registry)
               (require 'mevedel-tool-exec)
               (unless (mevedel-tool-ensure "Bash")
                 (error "Execution tool facade did not register Bash"))
               (let* ((root (make-temp-file "mevedel-cold-exec-" t))
                      (workspace (mevedel-workspace--create
                                  :type 'test :id root :root root
                                  :name "cold-exec"))
                      (session (mevedel-session--create
                                :authority-mode 'pid-lock
                                :execution-target
                                (mevedel-execution-target-create root)
                                :save-path root
                                :workspace workspace)))
                 (unwind-protect
                     (unless
                         (equal
                          (file-name-concat root "tool-results" "executions")
                          (mevedel-tool-exec--execution-artifact-directory
                           session nil))
                       (error "Execution facade could not resolve artifacts"))
                   (delete-directory root t)))
               (unless
                   (string-suffix-p
                    "mevedel-tool-exec-registration.elc"
                    (or (symbol-file 'mevedel-tool-exec--register 'defun)
                        ""))
                 (error "Execution tool behavior has the wrong owner"))))
            (fs-facade
             (progn
               (require 'mevedel-tool-registry)
               (require 'mevedel-tool-fs)
               (mevedel-tool-fs--register)
               (unless (mevedel-tool-ensure "Read")
                 (error "File-system facade did not register Read"))
               (unless
                   (string-suffix-p
                    "mevedel-tool-fs.elc"
                    (or (symbol-file 'mevedel-tool-fs--register 'defun) ""))
                 (error "File-system registration has the wrong owner"))))
            (fs-read
             (progn
               (require 'mevedel-tool-fs-read)
               (let ((path (make-temp-file "mevedel-cold-read-")))
                 (unwind-protect
                     (progn
                       (write-region "cold\n" nil path nil 'silent)
                       (unless
                           (string-search
                            "1\tcold"
                            (plist-get
                             (let (result)
                               (mevedel-tool-fs-read
                                (lambda (value) (setq result value))
                                (list :file_path path))
                               result)
                             :result))
                         (error "Read owner did not read a real file")))
                   (delete-file path)))
               (unless
                   (string-suffix-p
                    "mevedel-tool-fs-read.elc"
                    (or (symbol-file 'mevedel-tool-fs-read 'defun) ""))
                 (error "Read behavior has the wrong owner"))))
            (fs-search
             (progn
               (require 'mevedel-tool-fs-search)
               (let ((root (make-temp-file "mevedel-cold-search-" t))
                     (deadline (+ (float-time) 5))
                     result)
                 (unwind-protect
                     (progn
                       (write-region "cold\n" nil
                                     (file-name-concat root "cold.txt")
                                     nil 'silent)
                       (mevedel-tool-fs-search-glob
                        (lambda (value) (setq result value))
                        (list :pattern "*.txt" :path root))
                       (while (and (null result)
                                   (< (float-time) deadline))
                         (accept-process-output nil 0.01))
                       (unless result
                         (error "Search owner did not settle"))
                       (unless (string-match-p
                                "cold.txt" (plist-get result :result))
                         (error "Search owner did not glob a real directory"))
                       (setq result nil)
                       (mevedel-tool-fs-search-grep
                        (lambda (value) (setq result value))
                        (list :pattern "cold" :path root
                              :output_mode "content"))
                       (while (and (null result)
                                   (< (float-time) deadline))
                         (accept-process-output nil 0.01))
                       (unless (and result
                                    (string-match-p
                                     "cold" (plist-get result :result)))
                         (error "Search owner did not grep a real directory")))
                   (delete-directory root t)))
               (unless
                   (string-suffix-p
                    "mevedel-tool-fs-search.elc"
                    (or (symbol-file 'mevedel-tool-fs-search-glob 'defun) ""))
                 (error "Search behavior has the wrong owner"))))
            (render
             (progn
               (require 'mevedel-tool-render-data)
               (unless (equal "plain"
                              (mevedel-tool-render-data-strip "plain"))
                 (error "Render owner could not use runtime CL support"))
               (let* ((block
                       (mevedel-tool-render-data-format
                        '(:status success) "call-cold"))
                      (data (cdr (mevedel-tool-render-data-extract
                                  block nil "call-cold"))))
                 (unless (eq 'success (plist-get data :status))
                   (error "Render owner did not round-trip data")))
               (unless
                   (string-suffix-p
                    "mevedel-tool-render-data.elc"
                    (or (symbol-file 'mevedel-tool-render-data-format 'defun)
                        ""))
                 (error "Render behavior has the wrong owner"))
               (when (featurep 'mevedel-pipeline)
                 (error "Render owner loaded Pipeline"))))
            (patch-paths
             (progn
               (require 'mevedel-tool-patch)
               (require 'mevedel-tool-permission)
               (when (featurep 'mevedel-pipeline)
                 (error "Patch path test started with Pipeline loaded"))
               (let* ((directory (make-temp-file "mevedel-cold-patch-paths-" t))
                      (default-directory (file-name-as-directory directory))
                      (tool (mevedel-tool--create
                             :name "ApplyPatch"
                             :get-paths #'mevedel-tool-patch--get-paths)))
                 (unwind-protect
                     (unless
                         (equal
                          (list (expand-file-name "lib/foo.el" directory)
                                (expand-file-name "README.md" directory))
                          (mevedel-tool-permission-paths
                           tool
                           '(:patch "*** Begin Patch\n*** Add File: lib/foo.el\n+x\n*** Add File: README.md\n+docs\n*** End Patch")))
                       (error "Cold patch extraction lost affected paths"))
                   (delete-directory directory t)))
               (unless (string-suffix-p
                        "mevedel-tool-patch.elc"
                        (or (symbol-file 'mevedel-tool-patch--get-paths 'defun) ""))
                 (error "Patch path extraction has the wrong owner"))))
            (permission
             (progn
               (require 'mevedel-structs)
               (require 'mevedel-tool-registry)
               (require 'mevedel-tool-permission)
               (let ((called nil)
                     (tool (mevedel-tool--create
                            :name "ColdRead" :read-only-p t))
                     (session (mevedel-session--create
                               :name "cold" :permission-mode 'ask)))
                 (mevedel-tool-permission-step
                  (list :tool tool :args nil :session session)
                 (lambda (_context) (setq called t)) #'ignore)
                 (unless called
                   (error "Permission owner did not advance"))
                 (let ((owner (current-buffer))
                       (context (list :tool tool :session session
                                      :buffer (current-buffer)))
                       denied)
                   (setq-local mevedel-permission-denied-functions
                               (list
                                (lambda (payload)
                                  (unless (and (eq owner (current-buffer))
                                               (eq 'test (plist-get payload
                                                                    :permission-provenance)))
                                    (error "Denial hook lost its caller context"))
                                  '(:additional-context "cold denial"))))
                   (with-temp-buffer
                     (mevedel-tool-permission-deny
                      context
                      (lambda (reason updated outcome)
                        (setq denied
                              (and (eq owner (current-buffer))
                                   (equal "policy denied" reason)
                                   (eq 'permission-denied outcome)
                                   (equal "cold denial"
                                          (plist-get
                                           (car (plist-get updated
                                                           :hook-additional-context))
                                           :body))
                                   (eq 'tool-context
                                       (plist-get
                                        (car (plist-get updated :hook-audit-records))
                                        :type)))))
                      "policy denied" nil 'test))
                   (unless denied
                     (error "Cold permission denial lost hook context or audit"))))
               (unless
                   (string-suffix-p
                    "mevedel-tool-permission.elc"
                    (or (symbol-file 'mevedel-tool-permission-step 'defun)
                        ""))
                 (error "Permission behavior has the wrong owner"))
               (when (featurep 'mevedel-pipeline)
                 (error "Permission step loaded Pipeline")))))))
    (unwind-protect
        (progn
          (dolist (owner owners)
            (copy-file (file-name-concat root owner)
                       (file-name-concat compiled-root owner))
            (let ((byte-compile-verbose nil)
                  (byte-compile-error-on-warn t))
              (should (byte-compile-file (file-name-concat compiled-root owner)))))
          (dolist (case cases)
            (with-temp-buffer
              (ert-info ((format "cold owner: %s" (car case)))
                        (let ((status
                               (call-process
                                emacs nil t nil
                                "--batch" "-Q" "-L" compiled-root "-L" root
                                "--eval"
                                (prin1-to-string
                                 `(progn
                                    ,(cadr case)
                                    (when (featurep 'mevedel)
                                      (error
                                       "Tool owner loaded the mevedel umbrella")))))))
                          (ert-info ((buffer-string))
                                    (should (= 0 status))))
                        (should
                         (string-empty-p (string-trim (buffer-string))))))))
      (delete-directory compiled-root t))))

(provide 'test-mevedel-tool-owner-load)
;;; test-mevedel-tool-owner-load.el ends here
