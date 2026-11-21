;;; test-niwa-resource-diagnostics.el --- Resource outcomes -*- lexical-binding: t -*-

;;; Commentary:

;; Exercise discovery, missing targets, and actionable public diagnostics.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'niwa-resource)
(require 'niwa-agent-control)
(require 'niwa-tool-fs-read)
(require 'niwa-tool-fs-search)
(require 'niwa-tool-patch)
(require 'niwa-system)

(niwa-deftest niwa-resource-error-message ()
  ,test
  (test)
  :doc "resource condition labels are omitted and the authored address appears once"
  (progn
    (should (equal "File not found (work://note.md)"
                   (niwa-resource-error-message
                    '(niwa-resource-unavailable "File not found") "work://note.md")))
    (should (equal "File not found: work://note.md"
                   (niwa-resource-error-message
                    '(error "File not found: work://note.md") "work://note.md"))))
  :doc "private local and target-native paths are replaced in nested diagnostics"
  (let ((address "memory://journal/"))
    (should (equal "Cannot open memory://journal/entry.md (memory://journal/state/lock)"
                   (niwa-resource-error-message
                    '(error "Cannot open /ssh:host:/private/journal/entry.md (/private/journal/state/lock)")
                    address '("/ssh:host:/private/journal"))))))

(niwa-deftest niwa-resource--unavailable-reason ()
  ,test
  (test)
  :doc "availability distinguishes missing skill selection, missing source, and directories"
  (let ((root (make-temp-file "niwa-resource-diagnostics-" t)))
    (unwind-protect
        (progn
          (should (string-search
                   "Skill not found"
                   (niwa-resource--unavailable-reason
                    '(:scheme skill :source-file "old-source" :unavailable-p t))))
          (should (string-search
                   "source file is missing"
                   (niwa-resource--unavailable-reason
                    (list :scheme 'skill :source-file (file-name-concat root "SKILL.md")
                          :physical-path root :unavailable-p t))))
          (let ((source (file-name-concat root "SKILL.md")))
            (with-temp-file source (insert "Skill body"))
            (should (string-search
                     "use Glob or Grep"
                     (niwa-resource--unavailable-reason
                      (list :scheme 'skill :source-file source
                            :physical-path root :unavailable-p t))))))
      (delete-directory root t))))

(niwa-deftest niwa-resource-execute/diagnostics ()
  ,test
  (test)
  :doc "unused discovery roots return text without creating storage or counting files"
  (let* ((root (make-temp-file "niwa-resource-diagnostics-" t))
         (workspace (niwa-workspace--create :root root))
         (session (niwa-session--create :workspace workspace))
         (niwa-memory-dirs '(".niwa/memory"))
         (context (list :session session)))
    (unwind-protect
        (dolist (address '("work://" "work://shared" "artifact://" "memory://root"))
          (dolist (operation '(read glob grep))
            (let* ((attempt (niwa-resource-prepare operation address context))
                   (niwa-resource-current-attempts (list (cons address attempt)))
                   result)
              (pcase operation
                ('read (setq result (niwa-test--read (list :file_path address))))
                ('glob (niwa-tool-fs-search-glob
                        (lambda (value) (setq result value))
                        (list :path address :pattern "**/*")))
                ('grep (niwa-tool-fs-search-grep
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
                                         #'niwa-tool-fs-search-render-glob
                                       #'niwa-tool-fs-search-render-grep)
                                     (capitalize (symbol-name operation)) nil text
                                     (plist-get result :render-data))
                            :header)))))))
          (should-not (directory-files root nil directory-files-no-dot-files-regexp)))
      (delete-directory root t)))
  :doc "explicit missing files fail with the authored target and missing-file cause"
  (let* ((root (make-temp-file "niwa-resource-diagnostics-" t))
         (workspace (niwa-workspace--create :root root))
         (session (niwa-session--create :workspace workspace
                                           :save-path (file-name-concat root "session")))
         (niwa-memory-dirs '(".niwa/memory")))
    (unwind-protect
        (dolist (address '("work://missing.md" "work://shared/missing.md"
                          "artifact://missing.md" "memory://local-niwa/missing.md"))
          (dolist (operation '(read glob grep))
            (let* ((attempt (niwa-resource-prepare operation address (list :session session)))
                   (niwa-resource-current-attempts (list (cons address attempt)))
                   (failure
                    (should-error
                     (pcase operation
                       ('read (niwa-test--read (list :file_path address)))
                       ('glob (niwa-tool-fs-search-glob #'ignore (list :path address :pattern "**/*")))
                       ('grep (niwa-tool-fs-search-grep #'ignore (list :path address :pattern "note")))))))
              (should (string-search "not found" (error-message-string failure)))
              (should (string-search address (error-message-string failure)))
              (should-not (string-search root (error-message-string failure))))))
      (delete-directory root t)))
  :doc "missing owners and selected agent records are not described as empty storage"
  (let ((session (niwa-session--create)))
    (dolist (entry '(("work://shared" "workspace")
                     ("history://root" "conversation")
                     ("agent://root/missing" "Agent not found")
                     ("memory://global-agents/missing.md" "Memory root")))
      (let ((niwa-memory-dirs nil))
        (let ((failure (should-error
                        (niwa-resource-execute
                         (niwa-resource-prepare 'read (car entry) (list :session session))
                         #'ignore))))
          (should (string-search (car entry) (error-message-string failure)))
          (should (string-search (cadr entry) (error-message-string failure))))))))

(niwa-deftest niwa-resource-prepare/diagnostics ()
  ,test
  (test)
  :doc "unsupported operations retain their cause without claiming the address is malformed"
  (let ((failure (should-error
                  (niwa-resource-prepare 'apply-patch "artifact://report.txt" nil))))
    (should (string-search "ApplyPatch" (cadr failure)))
    (should (string-search "artifact://report.txt" (cadr failure)))
    (should-not (string-search "Invalid resource address" (cadr failure))))
  :doc "a trailing slash names the directory and diagnostics use its canonical spelling"
  (let* ((session (niwa-session--create))
         (failure (should-error
                   (niwa-resource-execute
                    (niwa-resource-prepare 'read "work://shared/" (list :session session))
                    #'ignore))))
    (should (string-search "work://shared" (error-message-string failure)))
    (should-not (string-search "work://shared/" (error-message-string failure)))))

(niwa-deftest niwa-resource-execute/family-diagnostics ()
  ,test
  (test)
  :doc "documentation errors distinguish absent files, unsupported files, and directories"
  (let* ((root (make-temp-file "niwa-resource-diagnostics-" t))
         (niwa-resource--source-dir root)
         (docs (file-name-concat root "docs")))
    (unwind-protect
        (progn
          (make-directory (file-name-concat docs "nested") t)
          (with-temp-file (file-name-concat docs "plain.txt") (insert "Text"))
          (dolist (entry '(("niwa://missing.md" "File not found")
                           ("niwa://plain.txt" "Only packaged Markdown")
                           ("niwa://nested" "use Glob or Grep")))
            (let ((failure (should-error
                            (niwa-resource-execute
                             (niwa-resource-prepare 'read (car entry) nil)
                             #'ignore))))
              (should (string-search (cadr entry) (cadr failure)))
              (should (string-search (car entry) (cadr failure)))
              (should-not (string-search root (cadr failure))))))
      (delete-directory root t)))
  :doc "JSON and readiness failures always identify the requested agent address"
  (let* ((record (niwa-agent-record--create :path "/root/reviewer" :role "reviewer" :activity 'idle))
         (session (niwa-session--create)))
    (niwa-session--set-agent-registry session (list (cons "/root/reviewer" record)))
    (dolist (entry '((nil "agent://root/reviewer" "no settled result")
                     ("plain text" "agent://root/reviewer#/item" "not valid JSON")
                     ("{}" "agent://root/reviewer#/item" "component is missing")))
      (setf (niwa-agent-record-settled-result record) (car entry)
            (niwa-agent-record-settled-outcome record) (and (car entry) 'completed))
      (let ((failure (should-error
                      (niwa-resource-execute
                       (niwa-resource-prepare 'read (cadr entry) (list :session session))))))
        (should (string-search (cadr entry) (cadr failure)))
        (should (string-search (nth 2 entry) (cadr failure))))))
  :doc "pipeline delivers one error prefix with a specific owner failure and no nested label"
  (let* ((root (make-temp-file "niwa-resource-diagnostics-" t))
         (workspace (niwa-workspace--create :root root))
         (session (niwa-session--create :workspace workspace :permission-mode 'full-auto)))
    (unwind-protect
        (with-temp-buffer
          (setq-local niwa--session session niwa--workspace workspace
                      default-directory (file-name-as-directory root))
          (let (result)
            (niwa-pipeline--step-handler
             (list :tool (niwa-tool--create :name "Read" :handler #'niwa-tool-fs-read :async-p t)
                   :args '(:file_path "agent://root/missing")
                   :resource-attempts
                   (list (cons "agent://root/missing"
                               (niwa-resource-prepare
                                'read "agent://root/missing" (list :session session)))))
             (lambda (context) (setq result (plist-get context :result)))
             (lambda (&rest failure) (ert-fail failure)))
            (should (string-prefix-p "Error: Agent not found" result))
            (should (string-search "agent://root/missing" result))
            (should-not (string-search "Resource unavailable:" result))))
      (delete-directory root t))))

(niwa-deftest niwa-tool-fs-read/eof-diagnostics ()
  ,test
  (test)
  :doc "file-backed and virtual history EOF give private, followable pipeline diagnostics"
  (let* ((root (make-temp-file "niwa-resource-eof-" t))
         (workspace (niwa-workspace--create :root root))
         (session (niwa-session--create :workspace workspace :permission-mode 'full-auto))
         (file (file-name-concat root ".niwa" "shared" "notes.md"))
         (niwa-tool-fs-read--default-limit 2))
    (unwind-protect
        (with-temp-buffer
          (org-mode)
          (setq-local niwa--session session niwa--workspace workspace
                      default-directory (file-name-as-directory root))
          (niwa-session-set-root-buffer session (current-buffer))
          (insert "History user request\n")
          (let ((start (point)))
            (insert "History assistant answer\nlast answer line\n")
            (put-text-property start (point) 'gptel 'response))
          (make-directory (file-name-directory file) t)
          (write-region "one\ntwo\nthree\n" nil file nil 'silent)
          (cl-labels
              ((pipeline-read (args)
                 (let* ((address (plist-get args :file_path))
                        (attempt (niwa-resource-prepare 'read address (list :session session)))
                        result)
                   (niwa-pipeline--step-handler
                    (list :tool (niwa-tool--create
                                 :name "Read" :handler #'niwa-tool-fs-read :async-p t)
                          :args args :resource-attempts (list (cons address attempt)))
                    (lambda (context) (setq result context))
                    (lambda (&rest failure) (ert-fail failure)))
                   result)))
            (dolist (address '("work://shared/notes.md" "history://root"))
              (let* ((last (if (string-prefix-p "work://" address)
                               3
                             (with-temp-buffer
                               (insert (plist-get
                                        (niwa-resource-execute
                                         (niwa-resource-prepare 'read address (list :session session)))
                                        :result))
                               (count-lines (point-min) (point-max)))))
                     (start (max 1 (1- last)))
                     (failure (pipeline-read (list :file_path address :offset 999 :limit 20)))
                     (text (plist-get failure :result)))
                (should (eq 'error (plist-get failure :handler-status)))
                (should (equal text
                               (format (concat "Error: Read offset 999 starts after the last line of %s. "
                                               "Available lines in this read: 1–%d. "
                                               "Read the last page with Read(file_path=%S, offset=%d, limit=2).")
                                       address last address start)))
                (should-not (string-search root text))
                (should-not (string-search "Resource unavailable:" text))
                (should-not (string-search "Error: Error:" text))
                ;; Parse and execute the authored call, not a reconstructed path.
                (should (string-match
                         "Read(file_path=\\(\".*\"\\), offset=\\([0-9]+\\), limit=\\([0-9]+\\))" text))
                (let* ((retry (list :file_path (car (read-from-string (match-string 1 text)))
                                    :offset (string-to-number (match-string 2 text))
                                    :limit (string-to-number (match-string 3 text))))
                       (page (pipeline-read retry)))
                  (should (eq 'success (plist-get page :handler-status)))
                  (should (string-match-p (format "%d\t" last) (plist-get page :result)))
                  (should (string-search (if (string-prefix-p "work://" address)
                                            "three" "last answer line")
                                         (plist-get page :result))))))))
      (delete-directory root t))))

(niwa-deftest niwa-tool-patch-handler/diagnostics ()
  ,test
  (test)
  :doc "missing writable owners retain their cause without redundant condition labels"
  (let ((niwa--session nil)
        (niwa--workspace nil)
        (niwa--current-request nil)
        (niwa-memory-dirs nil)
        (niwa-tool-patch-prepared-proposal nil)
        result)
    (niwa-tool-patch-handler
     (lambda (value) (setq result value))
     '(:patch "*** Begin Patch\n*** Add File: memory://local-niwa/note.md\n+Note\n*** End Patch"))
    (should (eq 'error (plist-get result :status)))
    (should (string-prefix-p "Error: Memory root is not configured" (plist-get result :result)))
    (should (string-search "memory://local-niwa/note.md" (plist-get result :result)))))

(niwa-deftest niwa-pipeline--run/resource-diagnostics ()
  ,test
  (test)
  :doc "resource failures before handler execution also omit redundant condition labels"
  (let (result)
    (niwa-pipeline--run
     (list (lambda (&rest _)
             (signal 'niwa-resource-unavailable
                     '("Shared working files require a workspace (work://shared/note.md)"))))
     (lambda (context) (setq result (plist-get context :result))) nil)
    (should (string-prefix-p "Error: Shared working files require a workspace" result))))

(provide 'test-niwa-resource-diagnostics)
;;; test-niwa-resource-diagnostics.el ends here
