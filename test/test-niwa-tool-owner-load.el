;;; test-niwa-tool-owner-load.el -- Cold tool owner tests -*- lexical-binding: t -*-

;;; Commentary:

;; Verifies the extracted tool owners without loading the niwa umbrella.

;;; Code:

(require 'bytecomp)

(require 'niwa-tool-exec-registration)

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(niwa-deftest niwa-tool-owners/cold-load ()
  ,test
  (test)
  :doc "loads compiled render and permission owners through real public seams"
  (let* ((root
          (file-name-as-directory
           (file-name-directory (locate-library "niwa"))))
         (compiled-root (make-temp-file "niwa-tool-owner-" t))
         (emacs (expand-file-name invocation-name invocation-directory))
         (owners '("niwa-bash-policy.el"
                   "niwa-hooks.el"
                   "niwa-permission-review.el"
                   "niwa-pipeline.el"
                   "niwa-tool-exec-permission.el"
                   "niwa-tool-exec.el"
                   "niwa-tool-exec-registration.el"
                   "niwa-tool-fs-read.el"
                   "niwa-tool-fs-search.el"
                   "niwa-tool-fs.el"
                   "niwa-tool-patch.el"
                   "niwa-tool-permission.el"
                   "niwa-tool-render-data.el"))
         (cases
          '((bash-policy
             (progn
               (require 'niwa-bash-policy)
               (unless (equal "git (2), rg"
                              (niwa-bash-policy-commands-summary
                               '("git" "git" "rg")))
                 (error "Bash policy owner did not summarize commands"))
               (unless
                   (string-suffix-p
                    "niwa-bash-policy.elc"
                    (or (symbol-file 'niwa-bash-policy-commands-summary
                                     'defun)
                        ""))
                 (error "Bash policy behavior has the wrong owner"))))
            (approval-review
            (progn
               (require 'niwa-permission-review)
               (require 'gptel-openai)
               (let ((niwa-permission-reviewer 'auto)
                     (gptel-backend (gptel-make-openai "Cold reviewer" :key "unused"))
                     (gptel-model 'gpt-4o-mini)
                     results)
                 (with-temp-buffer
                   (org-mode)
                   (insert "Evaluate (+ 1 2) in this Emacs.\n")
                   (let* ((session (niwa-session--create
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
                       (niwa-permission-review-start
                        entry (lambda () (error "Unexpected human fallback")))))
                   (unless (equal '(allow-once) results)
                     (error "Cold reviewer did not settle once: %S" results)))
                 (unless (string-suffix-p
                          "niwa-permission-review.elc"
                          (or (symbol-file 'niwa-permission-review-start 'defun) ""))
                   (error "Reviewer did not load its compiled owner")))))
            (exec-permission
             (progn
               (require 'niwa-structs)
               (require 'niwa-permissions)
               ;; The mode and rule owners define the variables this case
               ;; binds.  Without them the let binds lexically and the owner's
               ;; own defcustom then errors, surfacing as a spurious deny.
               (require 'niwa-permission-mode)
               (require 'niwa-permission-rules)
               (require 'niwa-tool-exec-permission)
               (let ((niwa-permission-mode 'full-auto)
                     (niwa-permission-rules nil)
                     outcome)
                 (niwa-tool-exec-permission-check-eval-async
                  nil '(:expression "(+ 1 2)")
                  (lambda (result) (setq outcome result)))
                 (unless (eq 'allow outcome)
                   (error "Execution permission owner did not allow Eval")))
               (when (featurep 'niwa-permission-queue)
                 (error "Automatic Eval loaded the permission queue"))
               (let ((niwa-permission-rules nil)
                     (session (niwa-session--create
                               :name "cold-prompt" :permission-mode 'ask))
                     outcome)
                 (with-temp-buffer
                   (setq-local niwa--session session)
                   (cl-letf (((symbol-function 'niwa-permission-prompt-render)
                              (lambda (_entry _origin cont _count)
                                (funcall cont 'deny-once))))
                     (niwa-tool-exec-permission-check-eval-async
                      nil '(:expression "(+ 1 2)")
                      (lambda (result) (setq outcome result)))))
                 (unless (and (eq 'deny outcome)
                              (featurep 'niwa-permission-queue)
                              (null (niwa-session-permission-queue session)))
                   (error "Cold Eval prompt did not settle through the queue")))
               (unless
                   (string-suffix-p
                    "niwa-tool-exec-permission.elc"
                    (or (symbol-file
                         'niwa-tool-exec-permission-check-eval-async
                         'defun)
                        ""))
                 (error "Execution permission behavior has the wrong owner"))))
            (exec-facade
             (progn
               (require 'niwa-execution-target)
               (require 'niwa-structs)
               (require 'niwa-tool-registry)
               (require 'niwa-tool-exec)
               (unless (niwa-tool-ensure "Bash")
                 (error "Execution tool facade did not register Bash"))
               (let* ((root (make-temp-file "niwa-cold-exec-" t))
                      (workspace (niwa-workspace--create
                                  :type 'test :id root :root root
                                  :name "cold-exec"))
                      (session (niwa-session--create
                                :execution-target
                                (niwa-execution-target-create root)
                                :save-path root
                                :workspace workspace)))
                 (unwind-protect
                     (unless
                         (equal
                          (file-name-concat root "tool-results" "executions")
                          (niwa-tool-exec--execution-artifact-directory
                           session nil))
                       (error "Execution facade could not resolve artifacts"))
                   (delete-directory root t)))
               (unless
                   (string-suffix-p
                    "niwa-tool-exec-registration.elc"
                    (or (symbol-file 'niwa-tool-exec--register 'defun)
                        ""))
                 (error "Execution tool behavior has the wrong owner"))))
            (fs-facade
             (progn
               (require 'niwa-tool-registry)
               (require 'niwa-tool-fs)
               (niwa-tool-fs--register)
               (unless (niwa-tool-ensure "Read")
                 (error "File-system facade did not register Read"))
               (unless
                   (string-suffix-p
                    "niwa-tool-fs.elc"
                    (or (symbol-file 'niwa-tool-fs--register 'defun) ""))
                 (error "File-system registration has the wrong owner"))))
            (fs-read
             (progn
               (require 'niwa-tool-fs-read)
               (let ((path (make-temp-file "niwa-cold-read-")))
                 (unwind-protect
                     (progn
                       (write-region "cold\n" nil path nil 'silent)
                       (unless
                           (string-search
                            "1\tcold"
                            (plist-get
                             (let (result)
                               (niwa-tool-fs-read
                                (lambda (value) (setq result value))
                                (list :file_path path))
                               result)
                             :result))
                         (error "Read owner did not read a real file")))
                   (delete-file path)))
               (unless
                   (string-suffix-p
                    "niwa-tool-fs-read.elc"
                    (or (symbol-file 'niwa-tool-fs-read 'defun) ""))
                 (error "Read behavior has the wrong owner"))))
            (fs-search
             (progn
               (require 'niwa-tool-fs-search)
               (let ((root (make-temp-file "niwa-cold-search-" t))
                     (deadline (+ (float-time) 5))
                     result)
                 (unwind-protect
                     (progn
                       (write-region "cold\n" nil
                                     (file-name-concat root "cold.txt")
                                     nil 'silent)
                       (niwa-tool-fs-search-glob
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
                       (niwa-tool-fs-search-grep
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
                    "niwa-tool-fs-search.elc"
                    (or (symbol-file 'niwa-tool-fs-search-glob 'defun) ""))
                 (error "Search behavior has the wrong owner"))))
            (render
             (progn
               (require 'niwa-tool-render-data)
               (unless (equal "plain"
                              (niwa-tool-render-data-strip "plain"))
                 (error "Render owner could not use runtime CL support"))
               (let* ((block
                       (niwa-tool-render-data-format
                        '(:status success) "call-cold"))
                      (data (cdr (niwa-tool-render-data-extract
                                  block nil "call-cold"))))
                 (unless (eq 'success (plist-get data :status))
                   (error "Render owner did not round-trip data")))
               (unless
                   (string-suffix-p
                    "niwa-tool-render-data.elc"
                    (or (symbol-file 'niwa-tool-render-data-format 'defun)
                        ""))
                 (error "Render behavior has the wrong owner"))
               (when (featurep 'niwa-pipeline)
                 (error "Render owner loaded Pipeline"))))
            (patch-paths
             (progn
               (require 'niwa-tool-patch)
               (require 'niwa-tool-permission)
               (when (featurep 'niwa-pipeline)
                 (error "Patch path test started with Pipeline loaded"))
               (let* ((directory (make-temp-file "niwa-cold-patch-paths-" t))
                      (default-directory (file-name-as-directory directory))
                      (tool (niwa-tool--create
                             :name "ApplyPatch"
                             :get-paths #'niwa-tool-patch--get-paths)))
                 (unwind-protect
                     (unless
                         (equal
                          (list (expand-file-name "lib/foo.el" directory)
                                (expand-file-name "README.md" directory))
                          (niwa-tool-permission-paths
                           tool
                           '(:patch "*** Begin Patch\n*** Add File: lib/foo.el\n+x\n*** Add File: README.md\n+docs\n*** End Patch")))
                       (error "Cold patch extraction lost affected paths"))
                   (delete-directory directory t)))
               (unless (string-suffix-p
                        "niwa-tool-patch.elc"
                        (or (symbol-file 'niwa-tool-patch--get-paths 'defun) ""))
                 (error "Patch path extraction has the wrong owner"))))
            (permission
             (progn
               (require 'niwa-structs)
               (require 'niwa-tool-registry)
               (require 'niwa-tool-permission)
               (let ((called nil)
                     (tool (niwa-tool--create
                            :name "ColdRead" :read-only-p t))
                     (session (niwa-session--create
                               :name "cold" :permission-mode 'ask)))
                 (niwa-tool-permission-step
                  (list :tool tool :args nil :session session)
                 (lambda (_context) (setq called t)) #'ignore)
                 (unless called
                   (error "Permission owner did not advance"))
                 (let ((owner (current-buffer))
                       (context (list :tool tool :session session
                                      :buffer (current-buffer)))
                       denied)
                   (setq-local niwa-permission-denied-functions
                               (list
                                (lambda (payload)
                                  (unless (and (eq owner (current-buffer))
                                               (eq 'test (plist-get payload
                                                                    :permission-provenance)))
                                    (error "Denial hook lost its caller context"))
                                  '(:additional-context "cold denial"))))
                   (with-temp-buffer
                     (niwa-tool-permission-deny
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
                    "niwa-tool-permission.elc"
                    (or (symbol-file 'niwa-tool-permission-step 'defun)
                        ""))
                 (error "Permission behavior has the wrong owner"))
               (when (featurep 'niwa-pipeline)
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
                                    (when (featurep 'niwa)
                                      (error
                                       "Tool owner loaded the niwa umbrella")))))))
                          (ert-info ((buffer-string))
                                    (should (= 0 status))))
                        (should
                         (string-empty-p (string-trim (buffer-string))))))))
      (delete-directory compiled-root t))))

(provide 'test-niwa-tool-owner-load)
;;; test-niwa-tool-owner-load.el ends here
