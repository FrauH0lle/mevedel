;;; test-mevedel-resource-diagnostics.el --- Resource outcomes -*- lexical-binding: t -*-

;;; Commentary:

;; Exercise discovery, missing targets, and actionable public diagnostics.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-resource)
(require 'mevedel-agent-control)
(require 'mevedel-tool-fs-read)
(require 'mevedel-tool-fs-search)
(require 'mevedel-tool-patch)
(require 'mevedel-system)

(mevedel-deftest mevedel-resource-error-message ()
  ,test
  (test)
  :doc "resource condition labels are omitted and the authored address appears once"
  (progn
    (should (equal "File not found (work://note.md)"
                   (mevedel-resource-error-message
                    '(mevedel-resource-unavailable "File not found") "work://note.md")))
    (should (equal "File not found: work://note.md"
                   (mevedel-resource-error-message
                    '(error "File not found: work://note.md") "work://note.md"))))
  :doc "private local and target-native paths are replaced in nested diagnostics"
  (let ((address "memory://journal/"))
    (should (equal "Cannot open memory://journal/entry.md (memory://journal/state/lock)"
                   (mevedel-resource-error-message
                    '(error "Cannot open /ssh:host:/private/journal/entry.md (/private/journal/state/lock)")
                    address '("/ssh:host:/private/journal"))))))

(mevedel-deftest mevedel-resource--unavailable-reason ()
  ,test
  (test)
  :doc "availability distinguishes missing skill selection, missing source, and directories"
  (let ((root (make-temp-file "mevedel-resource-diagnostics-" t)))
    (unwind-protect
        (progn
          (should (string-search
                   "Skill not found"
                   (mevedel-resource--unavailable-reason
                    '(:scheme skill :source-file "old-source" :unavailable-p t))))
          (should (string-search
                   "source file is missing"
                   (mevedel-resource--unavailable-reason
                    (list :scheme 'skill :source-file (file-name-concat root "SKILL.md")
                          :physical-path root :unavailable-p t))))
          (let ((source (file-name-concat root "SKILL.md")))
            (with-temp-file source (insert "Skill body"))
            (should (string-search
                     "use Glob or Grep"
                     (mevedel-resource--unavailable-reason
                      (list :scheme 'skill :source-file source
                            :physical-path root :unavailable-p t))))))
      (delete-directory root t))))

(mevedel-deftest mevedel-resource-execute/diagnostics ()
  ,test
  (test)
  :doc "unused discovery roots return text without creating storage or counting files"
  (let* ((root (make-temp-file "mevedel-resource-diagnostics-" t))
         (workspace (mevedel-workspace--create :root root))
         (session (mevedel-session--create :workspace workspace))
         (mevedel-memory-dirs '(".mevedel/memory"))
         (context (list :session session)))
    (unwind-protect
        (dolist (address '("work://" "work://shared" "artifact://" "memory://root"))
          (dolist (operation '(read glob grep))
            (let* ((attempt (mevedel-resource-prepare operation address context))
                   (mevedel-resource-current-attempts (list (cons address attempt)))
                   result)
              (pcase operation
                ('read (setq result (mevedel-test--read (list :file_path address))))
                ('glob (mevedel-tool-fs-search-glob
                        (lambda (value) (setq result value))
                        (list :path address :pattern "**/*")))
                ('grep (mevedel-tool-fs-search-grep
                        (lambda (value) (setq result value))
                        (list :path address :pattern "note"))))
              (let ((text (plist-get result :result)))
                (should (stringp text))
                (should-not (string-prefix-p "Error:" text))
                (should-not (string-search root text))
                (when (and (eq operation 'read) (equal address "work://shared"))
                  (should (string-search "ApplyPatch" text)))
                (when (memq operation '(glob grep))
                  (should (equal 0 (plist-get (plist-get result :render-data) :count)))
                  (should (string-search
                           "(0 "
                           (plist-get
                            (funcall (if (eq operation 'glob)
                                         #'mevedel-tool-fs-search-render-glob
                                       #'mevedel-tool-fs-search-render-grep)
                                     (capitalize (symbol-name operation)) nil text
                                     (plist-get result :render-data))
                            :header)))))))
          (should-not (directory-files root nil directory-files-no-dot-files-regexp)))
      (delete-directory root t)))
  :doc "explicit missing files fail with the authored target and missing-file cause"
  (let* ((root (make-temp-file "mevedel-resource-diagnostics-" t))
         (workspace (mevedel-workspace--create :root root))
         (session (mevedel-session--create :workspace workspace
                                           :save-path (file-name-concat root "session")))
         (mevedel-memory-dirs '(".mevedel/memory")))
    (unwind-protect
        (dolist (address '("work://missing.md" "work://shared/missing.md"
                          "artifact://missing.md" "memory://local-mevedel/missing.md"))
          (dolist (operation '(read glob grep))
            (let* ((attempt (mevedel-resource-prepare operation address (list :session session)))
                   (mevedel-resource-current-attempts (list (cons address attempt)))
                   (failure
                    (should-error
                     (pcase operation
                       ('read (mevedel-test--read (list :file_path address)))
                       ('glob (mevedel-tool-fs-search-glob #'ignore (list :path address :pattern "**/*")))
                       ('grep (mevedel-tool-fs-search-grep #'ignore (list :path address :pattern "note")))))))
              (should (string-search "not found" (error-message-string failure)))
              (should (string-search address (error-message-string failure)))
              (should-not (string-search root (error-message-string failure))))))
      (delete-directory root t)))
  :doc "missing owners and selected agent records are not described as empty storage"
  (let ((session (mevedel-session--create)))
    (dolist (entry '(("work://shared" "workspace")
                     ("history://root" "conversation")
                     ("agent://root/missing" "Agent not found")
                     ("memory://global-agents/missing.md" "Memory root")))
      (let ((mevedel-memory-dirs nil))
        (let ((failure (should-error
                        (mevedel-resource-execute
                         (mevedel-resource-prepare 'read (car entry) (list :session session))
                         #'ignore))))
          (should (string-search (car entry) (error-message-string failure)))
          (should (string-search (cadr entry) (error-message-string failure))))))))

(mevedel-deftest mevedel-resource-prepare/diagnostics ()
  ,test
  (test)
  :doc "unsupported operations retain their cause without claiming the address is malformed"
  (let ((failure (should-error
                  (mevedel-resource-prepare 'apply-patch "artifact://report.txt" nil))))
    (should (string-search "ApplyPatch" (cadr failure)))
    (should (string-search "artifact://report.txt" (cadr failure)))
    (should-not (string-search "Invalid resource address" (cadr failure))))
  :doc "a trailing slash names the directory and diagnostics use its canonical spelling"
  (let* ((session (mevedel-session--create))
         (failure (should-error
                   (mevedel-resource-execute
                    (mevedel-resource-prepare 'read "work://shared/" (list :session session))
                    #'ignore))))
    (should (string-search "work://shared" (error-message-string failure)))
    (should-not (string-search "work://shared/" (error-message-string failure)))))

(mevedel-deftest mevedel-resource-execute/family-diagnostics ()
  ,test
  (test)
  :doc "documentation errors distinguish absent files, unsupported files, and directories"
  (let* ((root (make-temp-file "mevedel-resource-diagnostics-" t))
         (mevedel-resource--source-dir root)
         (docs (file-name-concat root "docs")))
    (unwind-protect
        (progn
          (make-directory (file-name-concat docs "nested") t)
          (with-temp-file (file-name-concat docs "plain.txt") (insert "Text"))
          (dolist (entry '(("mevedel://missing.md" "File not found")
                           ("mevedel://plain.txt" "Only packaged Markdown")
                           ("mevedel://nested" "use Glob or Grep")))
            (let ((failure (should-error
                            (mevedel-resource-execute
                             (mevedel-resource-prepare 'read (car entry) nil)
                             #'ignore))))
              (should (string-search (cadr entry) (cadr failure)))
              (should (string-search (car entry) (cadr failure)))
              (should-not (string-search root (cadr failure))))))
      (delete-directory root t)))
  :doc "JSON and readiness failures always identify the requested agent address"
  (let* ((record (mevedel-agent-record--create :path "/root/reviewer" :role "reviewer" :activity 'idle))
         (session (mevedel-session--create)))
    (mevedel-session--set-agent-registry session (list (cons "/root/reviewer" record)))
    (dolist (entry '((nil "agent://root/reviewer" "no settled result")
                     ("plain text" "agent://root/reviewer#/item" "not valid JSON")
                     ("{}" "agent://root/reviewer#/item" "component is missing")))
      (setf (mevedel-agent-record-settled-result record) (car entry)
            (mevedel-agent-record-settled-outcome record) (and (car entry) 'completed))
      (let ((failure (should-error
                      (mevedel-resource-execute
                       (mevedel-resource-prepare 'read (cadr entry) (list :session session))))))
        (should (string-search (cadr entry) (cadr failure)))
        (should (string-search (nth 2 entry) (cadr failure))))))
  :doc "pipeline delivers one error prefix with a specific owner failure and no nested label"
  (let* ((root (make-temp-file "mevedel-resource-diagnostics-" t))
         (workspace (mevedel-workspace--create :root root))
         (session (mevedel-session--create :workspace workspace :permission-mode 'full-auto)))
    (unwind-protect
        (with-temp-buffer
          (setq-local mevedel--session session mevedel--workspace workspace
                      default-directory (file-name-as-directory root))
          (let (result)
            (mevedel-pipeline--step-handler
             (list :tool (mevedel-tool--create :name "Read" :handler #'mevedel-tool-fs-read :async-p t)
                   :args '(:file_path "agent://root/missing")
                   :resource-attempts
                   (list (cons "agent://root/missing"
                               (mevedel-resource-prepare
                                'read "agent://root/missing" (list :session session)))))
             (lambda (context) (setq result (plist-get context :result)))
             (lambda (&rest failure) (ert-fail failure)))
            (should (string-prefix-p "Error: Agent not found" result))
            (should (string-search "agent://root/missing" result))
            (should-not (string-search "Resource unavailable:" result))))
      (delete-directory root t))))

(mevedel-deftest mevedel-tool-patch-handler/diagnostics ()
  ,test
  (test)
  :doc "missing writable owners retain their cause without redundant condition labels"
  (let ((mevedel--session nil)
        (mevedel--workspace nil)
        (mevedel--current-request nil)
        (mevedel-memory-dirs nil)
        (mevedel-tool-patch-prepared-proposal nil)
        result)
    (mevedel-tool-patch-handler
     (lambda (value) (setq result value))
     '(:patch "*** Begin Patch\n*** Add File: memory://local-mevedel/note.md\n+Note\n*** End Patch"))
    (should (eq 'error (plist-get result :status)))
    (should (string-prefix-p "Error: Memory root is not configured" (plist-get result :result)))
    (should (string-search "memory://local-mevedel/note.md" (plist-get result :result)))))

(mevedel-deftest mevedel-pipeline--run/resource-diagnostics ()
  ,test
  (test)
  :doc "resource failures before handler execution also omit redundant condition labels"
  (let (result)
    (mevedel-pipeline--run
     (list (lambda (&rest _)
             (signal 'mevedel-resource-unavailable
                     '("Shared working files require a workspace (work://shared/note.md)"))))
     (lambda (context) (setq result (plist-get context :result))) nil)
    (should (string-prefix-p "Error: Shared working files require a workspace" result))))

(provide 'test-mevedel-resource-diagnostics)
;;; test-mevedel-resource-diagnostics.el ends here
