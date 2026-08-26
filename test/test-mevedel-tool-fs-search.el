;;; test-mevedel-tool-fs-search.el -- Tests for search file-system tools -*- lexical-binding: t -*-

;;; Commentary:

;; Behavioral coverage for Glob/Grep execution and resource-output privacy.

;;; Code:

(require 'mevedel-tool-fs-search)
(require 'mevedel-structs)
(require 'mevedel-execution)
(require 'mevedel-system)
(require 'mevedel-resource)
(require 'mevedel-workspace)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name
               load-file-name
               byte-compile-current-file))
          "helpers"))

(mevedel-deftest mevedel-tool-fs-search--scrub-resource-search-output ()
  ,test
  (test)
  :doc "replaces nested physical roots anywhere in helper diagnostics"
  (let* ((root (make-temp-file "mevedel-resource-scrub-" t))
         (nested (file-name-concat root "nested")))
    (unwind-protect
        (progn
          (make-directory nested)
          (should
           (equal
            (format "rg: local://root/one: denied\nrg: artifact://nested/two: denied")
            (mevedel-tool-fs-search--scrub-resource-search-output
             (format "rg: %s/one: denied\nrg: %s/two: denied"
                     root nested)
             (list (list :path root :address "local://root")
                   (list :path nested :address "artifact://nested"))))))
      (delete-directory root t)))
  :doc "scrubs the target-native spelling of remote roots"
  (let ((root (make-temp-file "mevedel-resource-remote-scrub-" t)))
    (unwind-protect
        (mevedel-test--with-local-shell-tramp '("resource-scrub")
          (let* ((remote-root
                  (format "/mevedelmock:resource-scrub:%s/" root))
                 (native-root
                  (file-remote-p remote-root 'localname 'never)))
            (should
             (equal
              "rg: artifact://remote/secret.txt: denied"
              (mevedel-tool-fs-search--scrub-resource-search-output
               (format "rg: %s: denied"
                       (file-name-concat native-root "secret.txt"))
               (list (list :path remote-root
                           :address "artifact://remote")))))))
      (delete-directory root t))))

(defun test-mevedel-tool-fs-search--await-callback (fn args)
  "Call async tool handler FN with ARGS and return its callback result."
  (let ((done nil)
        result
        (deadline (+ (float-time) 5)))
    (funcall fn
             (lambda (value)
               (setq result value
                     done t))
             args)
    (while (and (not done)
                (< (float-time) deadline))
      (accept-process-output nil 0.01))
    (unless done
      (ert-fail "Timed out waiting for tool callback"))
    (should (and (proper-list-p result)
                 (plist-member result :result)))
    (plist-get result :result)))

(defun test-mevedel-tool-fs-search--target-rg-wrapper (root)
  "Create a target-only `rg' wrapper under ROOT.
Return (BIN-DIRECTORY . MARKER-PATH)."
  (let* ((rg (or (executable-find "rg")
                 (ert-skip "rg is required")))
         (bin (file-name-concat root "target-bin"))
         (marker (file-name-concat root "target-rg-ran"))
         (wrapper (file-name-concat bin "rg")))
    (make-directory bin)
    (with-temp-file wrapper
      (insert "#!/bin/sh\n"
              "touch " (shell-quote-argument marker) "\n"
              "exec " (shell-quote-argument rg) " \"$@\"\n"))
    (set-file-modes wrapper #o755)
    (cons bin marker)))

;;
;;; Glob handler

(mevedel-deftest mevedel-tool-fs-search--rg-outcome ()
  ,test
  (test)
  :doc "classifies structured termination facts before exit codes"
  (should
   (eq 'error
       (mevedel-tool-fs-search--rg-outcome
        '(:error (error "start") :timed-out-p t :exit-code 1))))
  (should
   (eq 'timeout
       (mevedel-tool-fs-search--rg-outcome
        '(:timed-out-p t :output-limit-p t :exit-code 1))))
  (should
   (eq 'output-limit
       (mevedel-tool-fs-search--rg-outcome
        '(:output-limit-p t :exit-code 1))))
  (should (eq 'success
              (mevedel-tool-fs-search--rg-outcome '(:exit-code 0))))
  (should (eq 'no-match
              (mevedel-tool-fs-search--rg-outcome '(:exit-code 1))))
  (should (eq 'failure
              (mevedel-tool-fs-search--rg-outcome '(:exit-code 2)))))

(mevedel-deftest mevedel-tool-fs-search--truncate-output-buffer ()
  ,test
  (test)
  :doc "bounds multibyte output at a complete line and appends optional guidance"
  (with-temp-buffer
    (insert "keep\n" (make-string 20 #x00e9))
    (mevedel-tool-fs-search--truncate-output-buffer 12)
    (should (equal "keep\n" (buffer-string)))
    (should (<= (string-bytes (buffer-string)) 12)))
  (with-temp-buffer
    (insert (make-string 20 ?x))
    (mevedel-tool-fs-search--truncate-output-buffer 10 "Narrow it.")
    (should (equal "\n... Outpu" (buffer-string)))
    (should (<= (string-bytes (buffer-string)) 10)))
  (with-temp-buffer
    (insert "keep\n" (make-string 200 ?x))
    (mevedel-tool-fs-search--truncate-output-buffer 80 "Narrow it.")
    (should (string-suffix-p "Narrow it." (buffer-string)))
    (should (<= (string-bytes (buffer-string)) 80))))

(mevedel-deftest mevedel-tool-fs-search--settle-rg-result ()
  ,test
  (test)
  :doc "settles every ripgrep child outcome with shared messages and metadata"
  (cl-labels
      ((settle
        (facts)
        (with-temp-buffer
          (let ((metadata
                 (mevedel-tool-fs-search--settle-rg-result
                  facts "search" "No matches" "Narrow scope.")))
            (cons (buffer-string) metadata)))))
    (let ((result (settle '(:error (error "boom") :exit-code 1))))
      (should (equal "Error: search failed to start: boom" (car result)))
      (should-not (plist-get (cdr result) :pageable-p)))
    (let ((result (settle '(:timed-out-p t :exit-code 1
                            :output "partial\r\n"))))
      (should (equal "partial\n" (car result)))
      (should (plist-get (cdr result) :pageable-p))
      (should (string-match-p
               "timed out.*partial"
               (plist-get (cdr result) :partial-warning))))
    (should
     (equal "Error: search reached its output limit; narrow the search"
            (car (settle '(:output-limit-p t :exit-code 1 :output "")))))
    (let ((result (settle '(:exit-code 0 :output "match\n"))))
      (should (equal "match\n" (car result)))
      (should (plist-get (cdr result) :pageable-p)))
    (should (equal "No matches"
                   (car (settle '(:exit-code 1 :output "ignored")))))
    (should (equal "Error: search failed (exit code 2). Narrow scope.\n\nraw"
                   (car (settle '(:exit-code 2 :output "raw")))))))

(mevedel-deftest mevedel-tool-fs-search--vcs-metadata-path-p ()
  ,test
  (test)
  :doc "recognizes direct and symlinked VCS metadata paths"
  (let* ((root (file-name-as-directory (make-temp-file "mevedel-vcs-" t)))
         (metadata (file-name-concat root ".git"))
         (alias (file-name-concat root "metadata")))
    (unwind-protect
        (progn
          (make-directory metadata)
          (make-symbolic-link metadata alias)
          (should (mevedel-tool-fs-search--vcs-metadata-path-p metadata))
          (should (mevedel-tool-fs-search--vcs-metadata-path-p alias))
          (should-not (mevedel-tool-fs-search--vcs-metadata-path-p root)))
      (delete-directory root t))))

(mevedel-deftest mevedel-tool-fs-search--normalize-rg-glob ()
  ,test
  (test)
  :doc "narrows leading literal directories and leaves the final component"
  (let* ((root (file-name-as-directory (make-temp-file "mevedel-rg-" t)))
         (docs (file-name-concat root "docs")))
    (unwind-protect
        (progn
          (make-directory docs)
          (should (equal (cons docs "**/*.md")
                         (mevedel-tool-fs-search--normalize-rg-glob
                          root "docs/**/*.md")))
          (should (equal (cons root "*.md")
                         (mevedel-tool-fs-search--normalize-rg-glob root "*.md"))))
      (delete-directory root t)))
  :doc "does not narrow through version-control metadata directories"
  (let* ((root (file-name-as-directory (make-temp-file "mevedel-rg-" t)))
         (metadata (file-name-concat root ".git"))
         (alias (file-name-concat root "metadata")))
    (unwind-protect
        (progn
          (make-directory metadata)
          (make-symbolic-link metadata alias)
          (should-not
           (mevedel-tool-fs-search--normalize-rg-glob
            root ".git/**/*.el"))
          (should-not
           (mevedel-tool-fs-search--normalize-rg-glob
            root "metadata/**/*.el")))
      (delete-directory root t)))
  :doc "rejects absolute paths, parent traversal, and symlink escapes"
  (let* ((root (file-name-as-directory (make-temp-file "mevedel-rg-" t)))
         (outside (file-name-as-directory
                   (make-temp-file "mevedel-rg-outside-" t))))
    (unwind-protect
        (progn
          (make-symbolic-link outside (file-name-concat root "escape"))
          (should-error
           (mevedel-tool-fs-search--normalize-rg-glob root "/tmp/*.el"))
          (should-error
           (mevedel-tool-fs-search--normalize-rg-glob root "../*.el"))
          (should-error
           (mevedel-tool-fs-search--normalize-rg-glob root "escape/*.el")))
      (delete-directory root t)
      (delete-directory outside t)))
  :doc "treats a missing narrowed directory as no matches"
  (let ((root (file-name-as-directory (make-temp-file "mevedel-rg-" t))))
    (unwind-protect
        (should-not
         (mevedel-tool-fs-search--normalize-rg-glob root "missing/*.el"))
      (delete-directory root t))))

(mevedel-deftest mevedel-tool-fs-search--prepend-partial-warning ()
  ,test
  (test)
  :doc "preserves normal results and bounds warning-prefixed partial results"
  (should
   (equal "result"
          (mevedel-tool-fs-search--prepend-partial-warning nil "result" 20)))
  (let ((result
         (mevedel-tool-fs-search--prepend-partial-warning
          "Warning: partial.\n\n"
          (concat "first\n" (make-string 100 ?x))
          40)))
    (should (string-prefix-p "Warning: partial." result))
    (should (<= (string-bytes result) 40))))

(mevedel-deftest mevedel-tool-fs-search--finalize-glob-buffer ()
  ,test
  (test)
  :doc "preserves no-match output"
  (with-temp-buffer
    (insert "No files found matching pattern")
    (should (equal "No files found matching pattern"
                   (mevedel-tool-fs-search--finalize-glob-buffer))))
  :doc "caps failed output by line count"
  (with-temp-buffer
    (insert "Error: glob failed (exit code 2)\n\n")
    (dotimes (i 150)
      (insert (format "/tmp/f%03d.el\n" i)))
    (let ((result (mevedel-tool-fs-search--finalize-glob-buffer)))
      (should (string-prefix-p "Error: glob failed" result))
      (should (string-match-p "Results truncated (limit: 100)" result))
      (should-not (string-match-p "f149\\.el" result))))
  :doc "caps oversized single-line output"
  (with-temp-buffer
    (insert (make-string (+ mevedel-tool-fs-search--glob-max-output-bytes 100) ?x))
    (let ((result (mevedel-tool-fs-search--finalize-glob-buffer)))
      (should (< (length result)
                 (+ mevedel-tool-fs-search--glob-max-output-bytes 100)))
      (should (string-match-p "Output truncated at 30K byte limit" result)))))

(mevedel-deftest mevedel-tool-fs-search-glob ()
  ,test
  (test)
  :doc "globs current installed docs without a session or backing paths"
  (let* ((root (make-temp-file "mevedel-glob-installed-" t))
         (docs (file-name-concat root "docs"))
         (address "mevedel://")
         (mevedel-resource--source-dir root))
    (unwind-protect
        (progn
          (make-directory docs t)
          (with-temp-file (file-name-concat root "mevedel-resource.el")
            (insert ";; Adjacent package source.\n"))
          (with-temp-file (file-name-concat docs "first.md")
            (insert "first\n"))
          (with-temp-file (file-name-concat docs "ignored.txt")
            (insert "ignored\n"))
          (let* ((attempt (mevedel-resource-prepare 'glob address nil))
                 (mevedel-resource-current-attempts
                  (list (cons address attempt))))
            (with-temp-file (file-name-concat docs "current.md")
              (insert "current\n"))
            (let ((result
                   (test-mevedel-tool-fs-search--await-callback
                    #'mevedel-tool-fs-search-glob
                    (list :pattern "*" :path address))))
              (should (string-match-p "mevedel://current\\.md" result))
              (should (string-match-p "mevedel://first\\.md" result))
              (should-not (string-match-p "ignored\\.txt" result))
              (should-not (string-match-p (regexp-quote root) result)))))
      (delete-directory root t)))
  :doc "globs a readable skill alias and preserves it in result paths"
  (let* ((root (make-temp-file "mevedel-glob-skill-alias-" t))
         (skill-dir (file-name-concat root "demo"))
         (skill-file (file-name-concat skill-dir "SKILL.md"))
         (child (file-name-concat skill-dir "prompt.md"))
         (address "skill://local-agents/demo")
         (skill (mevedel-skill--create
                 :name "demo" :raw-name "demo" :source 'project
                 :source-family 'agents :source-file skill-file
                 :source-dir skill-dir))
         (session (mevedel-session--create :skills (list skill))))
    (unwind-protect
        (progn
          (make-directory skill-dir t)
          (with-temp-file skill-file (insert "skill\n"))
          (with-temp-file child (insert "prompt\n"))
          (let* ((attempt (mevedel-resource-prepare
                           'glob address (list :session session)))
                 (mevedel-resource-current-attempts
                  (list (cons address attempt)))
                 (result
                  (test-mevedel-tool-fs-search--await-callback
                   #'mevedel-tool-fs-search-glob
                   (list :pattern "*.md" :path address))))
            (should (string-match-p
                     "skill://local-agents/demo/prompt\\.md" result))
            (should-not (string-match-p (regexp-quote root) result))))
      (delete-directory root t)))
  :doc "searches every configured memory root and rewrites paths with root identities"
  (let* ((workspace-root (make-temp-file "mevedel-memory-search-" t))
         (first-root (file-name-concat workspace-root "first"))
         (second-root (file-name-concat workspace-root "second"))
         (workspace (mevedel-workspace--create
                     :type 'test :id workspace-root :root workspace-root
                     :name "memory-search"))
         (session (mevedel-session--create
                   :workspace workspace :working-directory workspace-root))
         (address "memory://root")
         (mevedel-memory-dirs (list first-root second-root)))
    (unwind-protect
        (progn
          (make-directory first-root t)
          (make-directory second-root t)
          (with-temp-file (file-name-concat first-root "topic.md")
            (insert "needle"))
          (with-temp-file (file-name-concat second-root "topic.md")
            (insert "needle"))
          (let* ((roots (mevedel-system--memory-roots workspace))
                 (first-address
                  (concat "memory://"
                          (mevedel-resource-memory-root-key (car roots))
                          "/topic.md"))
                 (second-address
                  (concat "memory://"
                          (mevedel-resource-memory-root-key (cadr roots))
                          "/topic.md"))
                 (attempt (mevedel-resource-prepare
                           'glob address (list :session session)))
                 (mevedel-resource-current-attempts
                  (list (cons address attempt)))
                 (mevedel--session session))
            (cl-letf (((symbol-function 'mevedel-execution-start-helper)
                       (lambda (callback &rest _)
                         (funcall callback
                                  (list :exit-code 2
                                        :output
                                        (format
                                         "%s\n%s\nrg: %s: Permission denied\n"
                                         (file-name-concat
                                          first-root "topic.md")
                                         (file-name-concat
                                          second-root "topic.md")
                                         (file-name-concat
                                          first-root "topic.md"))
                                        :timed-out-p nil
                                        :output-limit-p nil)))))
              (let ((result
                     (test-mevedel-tool-fs-search--await-callback
                      #'mevedel-tool-fs-search-glob
                      (list :pattern "*.md" :path address))))
                (should (string-match-p (regexp-quote first-address) result))
                (should (string-match-p (regexp-quote second-address) result))
                (should-not (string-match-p
                             (regexp-quote workspace-root) result))))))
      (delete-directory workspace-root t)))
  :doc "runs ripgrep in a remote target and returns target-native paths"
  (let* ((root (file-name-as-directory
                (make-temp-file "mevedel-remote-glob-" t)))
         (match (file-name-concat root "found.el"))
         (wrapper (test-mevedel-tool-fs-search--target-rg-wrapper root))
         (bin (car wrapper))
         (marker (cdr wrapper)))
    (unwind-protect
        (progn
          (with-temp-file match (insert "content\n"))
          (mevedel-test--with-local-shell-tramp '("glob")
                                                (let* ((remote-root (format "/mevedelmock:glob:%s" root))
                                                       (workspace
                                                        (mevedel-workspace--create
                                                         :type 'project :id "remote-glob"
                                                         :root remote-root :name "remote-glob"))
                                                       (session (mevedel-session-create "main" workspace)))
                                                  (setf (mevedel-session-sandbox-mode session) 'off)
                                                  (let ((default-directory remote-root)
                                                        (mevedel--session session)
                                                        (tramp-remote-path (cons bin tramp-remote-path)))
                                                    (let ((result
                                                           (test-mevedel-tool-fs-search--await-callback
                                                            #'mevedel-tool-fs-search-glob
                                                            (list :pattern "*.el" :path remote-root))))
                                                      (should (string-match-p (regexp-quote match) result))
                                                      (should-not (string-match-p "/mevedelmock:" result))
                                                      (should (file-exists-p marker)))))))
      (delete-directory root t)))
  :doc "runs ripgrep through the helper layer with the search path read-only"
  (let ((session (mevedel-session--create))
        (tmp-dir (make-temp-file "mevedel-test-" t))
        captured result)
    (unwind-protect
        (progn
          (with-temp-file (file-name-concat tmp-dir "found.el")
            (insert "content"))
          (cl-letf (((symbol-function 'mevedel-execution-start-helper)
                     (lambda (&rest helper-args)
                       (setq captured helper-args)
                       (funcall (car helper-args)
                                '(:exit-code 0 :output "found.el\n"
                                             :timed-out-p nil)))))
            (let ((mevedel--session session))
              (setq result
                    (test-mevedel-tool-fs-search--await-callback
                     #'mevedel-tool-fs-search-glob
                     (list :pattern "*.el" :path tmp-dir)))))
          (should (equal (list tmp-dir) (nth 3 captured)))
          (should (eq session (nth 6 captured)))
          (should (= mevedel-tool-fs-search-timeout
                     (plist-get (nthcdr 5 captured) :timeout)))
          (let ((rg-args (cdr (nth 2 captured))))
            (should (member "--hidden" rg-args))
            (should (member "--no-ignore" rg-args))
            (should-not (member "--follow" rg-args))
            (should-not (member "--sort" rg-args))
            (dolist (directory '(".git" ".svn" ".hg" ".bzr" ".jj" ".sl"))
              (should (member (format "--glob=!**/%s" directory)
                              rg-args))))
          (should (string-match-p "found\\.el" result)))
      (delete-directory tmp-dir t)))
  :doc "finds nested and ignored hidden files while excluding VCS metadata"
  (let* ((tmp-dir (make-temp-file "mevedel-test-" t))
         (docs (file-name-concat tmp-dir "docs"))
         result)
    (unwind-protect
        (progn
          (make-directory (file-name-concat tmp-dir ".git"))
          (make-directory (file-name-concat docs ".hidden") t)
          (with-temp-file (file-name-concat tmp-dir ".gitignore")
            (insert "docs/.hidden/\n"))
          (with-temp-file (file-name-concat docs ".hidden" "found.md")
            (insert "content"))
          (dolist (directory '(".git" ".svn" ".hg" ".bzr" ".jj" ".sl"))
            (make-directory (file-name-concat docs directory) t)
            (with-temp-file
                (file-name-concat docs directory "excluded.md")
              (insert "content")))
          (setq result
                (test-mevedel-tool-fs-search--await-callback
                 #'mevedel-tool-fs-search-glob
                 (list :pattern "docs/**/*.md" :path tmp-dir)))
          (should (string-match-p "found\\.md" result))
          (should-not (string-match-p "excluded\\.md" result)))
      (delete-directory tmp-dir t)))
  :doc "reports bounded partial output for timeout and output limits"
  (let ((tmp-dir (make-temp-file "mevedel-test-" t)))
    (unwind-protect
        (cl-labels
            ((run (facts)
               (cl-letf (((symbol-function 'mevedel-execution-start-helper)
                          (lambda (callback &rest _)
                            (funcall callback facts))))
                 (test-mevedel-tool-fs-search--await-callback
                  #'mevedel-tool-fs-search-glob
                  (list :pattern "*.el" :path tmp-dir)))))
          (should (string-match-p
                   "timed out.*partial"
                   (run '(:exit-code 1 :output "partial.el\n"
                                     :timed-out-p t :output-limit-p nil))))
          (should (string-match-p
                   "output limit.*partial"
                   (run '(:exit-code 1 :output "partial.el\n"
                                     :timed-out-p nil :output-limit-p t)))))
      (delete-directory tmp-dir t)))
  :doc "finds files matching pattern"
  (let* ((tmp-dir (make-temp-file "mevedel-test-" t))
         (result nil))
    (unwind-protect
        (progn
          (with-temp-file (file-name-concat tmp-dir "foo.el")
            (insert "content"))
          (with-temp-file (file-name-concat tmp-dir "bar.el")
            (insert "content"))
          (with-temp-file (file-name-concat tmp-dir "baz.txt")
            (insert "content"))
          (setq result
                (test-mevedel-tool-fs-search--await-callback
                 #'mevedel-tool-fs-search-glob
                 (list :pattern "*.el" :path tmp-dir)))
          (should (string-match-p "foo\\.el" result))
          (should (string-match-p "bar\\.el" result))
          (should-not (string-match-p "baz\\.txt" result)))
      (delete-directory tmp-dir t)))
  :doc "returns message when no files match"
  (let* ((tmp-dir (make-temp-file "mevedel-test-" t))
         (result nil))
    (unwind-protect
        (progn
          (setq result
                (test-mevedel-tool-fs-search--await-callback
                 #'mevedel-tool-fs-search-glob
                 (list :pattern "*.xyz" :path tmp-dir)))
          (should (string-match-p "No files found" result)))
      (delete-directory tmp-dir t)))
  :doc "errors on empty pattern"
  (should-error
   (mevedel-tool-fs-search-glob #'ignore (list :pattern ""))
   :type 'error)
  :doc "errors on non-existent path"
  (should-error
   (mevedel-tool-fs-search-glob #'ignore (list :pattern "*.el"
                                               :path "/nonexistent/dir"))
   :type 'error)
  :doc "defaults path to current directory"
  (let* ((tmp-dir (make-temp-file "mevedel-test-" t))
         (default-directory tmp-dir)
         (result nil))
    (unwind-protect
        (progn
          (with-temp-file (file-name-concat tmp-dir "test.el")
            (insert "content"))
          (setq result
                (test-mevedel-tool-fs-search--await-callback
                 #'mevedel-tool-fs-search-glob
                 (list :pattern "*.el")))
          (should (string-match-p "test\\.el" result)))
      (delete-directory tmp-dir t)))
  :doc "limits output to 100 entries by default"
  (let* ((tmp-dir (make-temp-file "mevedel-test-" t))
         (result nil))
    (unwind-protect
        (progn
          (dotimes (i 101)
            (with-temp-file (file-name-concat tmp-dir (format "f%03d.el" i))
              (insert "content")))
          (setq result
                (test-mevedel-tool-fs-search--await-callback
                 #'mevedel-tool-fs-search-glob
                 (list :pattern "*.el" :path tmp-dir)))
          (should (= 101 (length (split-string result "\n" t))))
          (should (string-match-p "Results truncated (limit: 100)" result)))
      (delete-directory tmp-dir t))))

;;
;;; Grep handler

(mevedel-deftest mevedel-tool-fs-search-grep ()
  ,test
  (test)
  :doc "greps current installed docs without a session or backing paths"
  (let* ((root (make-temp-file "mevedel-grep-installed-" t))
         (docs (file-name-concat root "docs"))
         (file (file-name-concat docs "guide.md"))
         (address "mevedel://")
         (mevedel-resource--source-dir root))
    (unwind-protect
        (progn
          (make-directory docs t)
          (with-temp-file (file-name-concat root "mevedel-resource.el")
            (insert ";; Adjacent package source contains private needle.\n"))
          (with-temp-file file (insert "old text\n"))
          (with-temp-file (file-name-concat docs "ignored.txt")
            (insert "private needle\n"))
          (let* ((attempt (mevedel-resource-prepare 'grep address nil))
                 (mevedel-resource-current-attempts
                  (list (cons address attempt))))
            (with-temp-file file (insert "public needle\n"))
            (let ((result
                   (test-mevedel-tool-fs-search--await-callback
                    #'mevedel-tool-fs-search-grep
                    (list :pattern "needle" :path address
                          :output_mode "content"))))
              (should (string-match-p "mevedel://guide\\.md" result))
              (should (string-match-p "1:public needle" result))
              (should-not (string-match-p "private needle" result))
              (should-not (string-match-p "ignored\\.txt" result))
              (should-not (string-match-p (regexp-quote root) result)))))
      (delete-directory root t)))
  :doc "greps a readable skill alias and preserves it in result paths"
  (let* ((root (make-temp-file "mevedel-grep-skill-alias-" t))
         (skill-dir (file-name-concat root "demo"))
         (skill-file (file-name-concat skill-dir "SKILL.md"))
         (child (file-name-concat skill-dir "prompt.md"))
         (address "skill://local-agents/demo")
         (skill (mevedel-skill--create
                 :name "demo" :raw-name "demo" :source 'project
                 :source-family 'agents :source-file skill-file
                 :source-dir skill-dir))
         (session (mevedel-session--create :skills (list skill))))
    (unwind-protect
        (progn
          (make-directory skill-dir t)
          (with-temp-file skill-file (insert "skill\n"))
          (with-temp-file child (insert "public needle\n"))
          (let* ((attempt (mevedel-resource-prepare
                           'grep address (list :session session)))
                 (mevedel-resource-current-attempts
                  (list (cons address attempt)))
                 (result
                  (test-mevedel-tool-fs-search--await-callback
                   #'mevedel-tool-fs-search-grep
                   (list :pattern "needle" :path address
                         :output_mode "content"))))
            (should (string-match-p
                     "skill://local-agents/demo/prompt\\.md" result))
            (should-not (string-match-p (regexp-quote root) result))))
      (delete-directory root t)))
  :doc "runs ripgrep in a remote target and returns target-native paths"
  (let* ((root (file-name-as-directory
                (make-temp-file "mevedel-remote-grep-" t)))
         (match (file-name-concat root "match.el"))
         (wrapper (test-mevedel-tool-fs-search--target-rg-wrapper root))
         (bin (car wrapper))
         (marker (cdr wrapper)))
    (unwind-protect
        (progn
          (with-temp-file match (insert "remote needle\n"))
          (mevedel-test--with-local-shell-tramp '("grep")
            (let* ((remote-root (format "/mevedelmock:grep:%s" root))
                   (workspace
                    (mevedel-workspace--create
                     :type 'project :id "remote-grep"
                     :root remote-root :name "remote-grep"))
                   (session (mevedel-session-create "main" workspace)))
              (setf (mevedel-session-sandbox-mode session) 'off)
              (let ((default-directory remote-root)
                    (mevedel--session session)
                    (tramp-remote-path (cons bin tramp-remote-path)))
                (let ((result
                       (test-mevedel-tool-fs-search--await-callback
                        #'mevedel-tool-fs-search-grep
                        (list :pattern "remote needle"
                              :path remote-root))))
                  (should (string-match-p (regexp-quote match) result))
                  (should-not (string-match-p "/mevedelmock:" result))
                  (should (file-exists-p marker)))))))
      (delete-directory root t)))
  :doc "runs ripgrep through the helper layer with the search path read-only"
  (let ((session (mevedel-session--create))
        (tmp (make-temp-file "mevedel-test-"))
        captured result)
    (unwind-protect
        (progn
          (with-temp-file tmp (insert "needle\n"))
          (cl-letf (((symbol-function 'mevedel-execution-start-helper)
                     (lambda (&rest helper-args)
                       (setq captured helper-args)
                       (funcall (car helper-args)
                                (list :exit-code 0
                                      :output (concat tmp "\n")
                                      :timed-out-p nil)))))
            (let ((mevedel--session session))
              (setq result
                    (test-mevedel-tool-fs-search--await-callback
                     #'mevedel-tool-fs-search-grep
                     (list :pattern "needle" :path tmp)))))
          (should (equal (list tmp) (nth 3 captured)))
          (should (eq session (nth 6 captured)))
          (should (= mevedel-tool-fs-search-timeout
                     (plist-get (nthcdr 5 captured) :timeout)))
          (let ((rg-args (cdr (nth 2 captured))))
            (should (member "--hidden" rg-args))
            (should-not (member "--no-ignore" rg-args))
            (should-not (member "--sort=modified" rg-args))
            (dolist (directory '(".git" ".svn" ".hg" ".bzr" ".jj" ".sl"))
              (should (member (format "--glob=!**/%s" directory)
                              rg-args))))
          (should (string-match-p (regexp-quote tmp) result)))
      (delete-file tmp)))
  :doc "lets explicit scope select ignored content while ordinary traversal does not"
  (let* ((tmp-dir (make-temp-file "mevedel-test-" t))
         (docs (file-name-concat tmp-dir "docs"))
         (nested-docs (file-name-concat tmp-dir "nested" "docs"))
         result)
    (unwind-protect
        (progn
          (make-directory (file-name-concat tmp-dir ".git"))
          (make-directory (file-name-concat docs ".hidden") t)
          (make-directory (file-name-concat docs "ignored") t)
          (make-directory nested-docs t)
          (with-temp-file (file-name-concat tmp-dir ".gitignore")
            (insert "docs/ignored/\ndocs/selected.md\n"))
          (with-temp-file (file-name-concat docs ".hidden" "visible.md")
            (insert "needle\n"))
          (with-temp-file (file-name-concat docs "ignored" "ignored.md")
            (insert "needle\n"))
          (with-temp-file (file-name-concat docs "selected.md")
            (insert "needle\n"))
          (with-temp-file (file-name-concat nested-docs "wrong.md")
            (insert "needle\n"))
          (dolist (directory '(".git" ".svn" ".hg" ".bzr" ".jj" ".sl"))
            (make-directory (file-name-concat docs directory) t)
            (with-temp-file
                (file-name-concat docs directory "excluded.md")
              (insert "needle\n")))
          (setq result
                (test-mevedel-tool-fs-search--await-callback
                 #'mevedel-tool-fs-search-grep
                 (list :pattern "needle" :path tmp-dir)))
          (should (string-match-p "visible\\.md" result))
          (should-not (string-match-p "selected\\.md" result))
          (should-not (string-match-p "ignored\\.md" result))
          (setq result
                (test-mevedel-tool-fs-search--await-callback
                 #'mevedel-tool-fs-search-grep
                 (list :pattern "needle" :path tmp-dir
                       :glob "docs/**/*.md")))
          (should (string-match-p "visible\\.md" result))
          (should (string-match-p "selected\\.md" result))
          (should-not (string-match-p "ignored\\.md" result))
          (should-not (string-match-p "excluded\\.md" result))
          (should-not (string-match-p "wrong\\.md" result))
          (setq result
                (test-mevedel-tool-fs-search--await-callback
                 #'mevedel-tool-fs-search-grep
                 (list :pattern "needle"
                       :path (file-name-concat docs "ignored"))))
          (should (string-match-p "ignored\\.md" result)))
      (delete-directory tmp-dir t)))
  :doc "reports partial output before interpreting timeout exit codes"
  (let ((tmp-dir (make-temp-file "mevedel-test-" t)))
    (unwind-protect
        (cl-labels
            ((run (facts &optional offset)
               (cl-letf (((symbol-function 'mevedel-execution-start-helper)
                          (lambda (callback &rest _)
                            (funcall callback facts))))
                 (test-mevedel-tool-fs-search--await-callback
                  #'mevedel-tool-fs-search-grep
                  (list :pattern "needle" :path tmp-dir
                        :offset (or offset 0))))))
          (should (string-match-p
                   "timed out.*partial"
                   (run '(:exit-code 1 :output "partial.txt\n"
                          :timed-out-p t :output-limit-p nil))))
          (should (string-match-p
                   "output limit.*partial"
                   (run '(:exit-code 1 :output "partial.txt\n"
                          :timed-out-p nil :output-limit-p t))))
          (should (string-match-p
                   "timed out"
                   (run '(:exit-code 1 :output ""
                          :timed-out-p t :output-limit-p nil)
                        1))))
      (delete-directory tmp-dir t)))
  :doc "files_with_matches: returns matching file paths"
  (let* ((tmp-dir (make-temp-file "mevedel-test-" t))
         (result nil))
    (unwind-protect
        (progn
          (with-temp-file (file-name-concat tmp-dir "match.el")
            (insert "hello world\n"))
          (with-temp-file (file-name-concat tmp-dir "nomatch.el")
            (insert "goodbye\n"))
          (setq result
                (test-mevedel-tool-fs-search--await-callback
                 #'mevedel-tool-fs-search-grep
                 (list :pattern "hello"
                       :path tmp-dir)))
          (should (string-match-p "match\\.el" result))
          (should-not (string-match-p "nomatch\\.el" result)))
      (delete-directory tmp-dir t)))
  :doc "content mode: returns matching lines with headings"
  (let* ((tmp-dir (make-temp-file "mevedel-test-" t))
         (result nil))
    (unwind-protect
        (progn
          (with-temp-file (file-name-concat tmp-dir "code.el")
            (insert "line one\nfind me\nline three\n"))
          (setq result
                (test-mevedel-tool-fs-search--await-callback
                 #'mevedel-tool-fs-search-grep
                 (list :pattern "find me"
                       :path tmp-dir
                       :output_mode "content")))
          (should (string-match-p "2:find me" result)))
      (delete-directory tmp-dir t)))
  :doc "count mode: returns match count"
  (let* ((tmp-dir (make-temp-file "mevedel-test-" t))
         (result nil))
    (unwind-protect
        (progn
          (with-temp-file (file-name-concat tmp-dir "data.txt")
            (insert "foo\nbar\nfoo\n"))
          (setq result
                (test-mevedel-tool-fs-search--await-callback
                 #'mevedel-tool-fs-search-grep
                 (list :pattern "foo"
                       :path tmp-dir
                       :output_mode "count")))
          (should (string-match-p ":2" result)))
      (delete-directory tmp-dir t)))
  :doc "returns no-matches message for exit code 1"
  (let* ((tmp-dir (make-temp-file "mevedel-test-" t))
         (result nil))
    (unwind-protect
        (progn
          (with-temp-file (file-name-concat tmp-dir "empty.txt")
            (insert "nothing here\n"))
          (setq result
                (test-mevedel-tool-fs-search--await-callback
                 #'mevedel-tool-fs-search-grep
                 (list :pattern "zzzznotfound"
                       :path tmp-dir)))
          (should (string-match-p "No matches found" result)))
      (delete-directory tmp-dir t)))
  :doc "errors on non-readable path"
  (should-error
   (mevedel-tool-fs-search-grep #'ignore (list :pattern "test"
                                          :path "/nonexistent/dir"))
   :type 'error)
  :doc "glob filter restricts file types"
  (let* ((tmp-dir (make-temp-file "mevedel-test-" t))
         (result nil))
    (unwind-protect
        (progn
          (with-temp-file (file-name-concat tmp-dir "code.el")
            (insert "target\n"))
          (with-temp-file (file-name-concat tmp-dir "notes.txt")
            (insert "target\n"))
          (setq result
                (test-mevedel-tool-fs-search--await-callback
                 #'mevedel-tool-fs-search-grep
                 (list :pattern "target"
                       :path tmp-dir
                       :glob "*.el")))
          (should (string-match-p "code\\.el" result))
          (should-not (string-match-p "notes\\.txt" result)))
      (delete-directory tmp-dir t)))
  :doc "case insensitive search"
  (let* ((tmp-dir (make-temp-file "mevedel-test-" t))
         (result nil))
    (unwind-protect
        (progn
          (with-temp-file (file-name-concat tmp-dir "test.txt")
            (insert "Hello World\n"))
          (setq result
                (test-mevedel-tool-fs-search--await-callback
                 #'mevedel-tool-fs-search-grep
                 (list :pattern "hello"
                       :path tmp-dir
                       :-i t)))
          (should (string-match-p "test\\.txt" result)))
      (delete-directory tmp-dir t)))
  :doc "rejects a negative head_limit and offset"
  ;; A negative head_limit deleted every match and reported "truncated";
  ;; a negative offset was silently ignored.
  (progn
    (should-error
     (test-mevedel-tool-fs-search--await-callback
      #'mevedel-tool-fs-search-grep
      (list :pattern "match" :path "/tmp" :head_limit -3))
     :type 'error)
    (should-error
     (test-mevedel-tool-fs-search--await-callback
      #'mevedel-tool-fs-search-grep
      (list :pattern "match" :path "/tmp" :offset -2))
     :type 'error))
  :doc "reports an offset past the last result"
  ;; An empty success is indistinguishable from no matches.
  (let* ((tmp-dir (make-temp-file "mevedel-test-" t))
         (result nil))
    (unwind-protect
        (progn
          (dotimes (i 3)
            (with-temp-file (file-name-concat tmp-dir (format "f%d.txt" i))
              (insert "match\n")))
          (setq result
                (test-mevedel-tool-fs-search--await-callback
                 #'mevedel-tool-fs-search-grep
                 (list :pattern "match"
                       :path tmp-dir
                       :offset 99)))
          (should-not (string-empty-p result))
          (should (string-match-p "after the last" result))
          ;; A head limit alongside must not append a contradictory
          ;; truncation notice to the answer.
          (setq result
                (test-mevedel-tool-fs-search--await-callback
                 #'mevedel-tool-fs-search-grep
                 (list :pattern "match"
                       :path tmp-dir
                       :offset 99
                       :head_limit 1)))
          (should (string-match-p "after the last" result))
          (should-not (string-match-p "Results truncated" result)))
      (delete-directory tmp-dir t)))
  :doc "head_limit truncates output"
  (let* ((tmp-dir (make-temp-file "mevedel-test-" t))
         (result nil))
    (unwind-protect
        (progn
          (dotimes (i 10)
            (with-temp-file (file-name-concat tmp-dir (format "f%d.txt" i))
              (insert "match\n")))
          (setq result
                (test-mevedel-tool-fs-search--await-callback
                 #'mevedel-tool-fs-search-grep
                 (list :pattern "match"
                       :path tmp-dir
                       :head_limit 3)))
          (should (string-match-p "Results truncated" result))
          ;; Count non-empty, non-truncation lines
          (let ((lines (seq-filter
                        (lambda (l)
                          (and (not (string-empty-p l))
                               (not (string-match-p "truncated" l))))
                        (split-string result "\n"))))
            (should (= 3 (length lines)))))
      (delete-directory tmp-dir t)))
  :doc "offset skips initial results"
  (let* ((tmp-dir (make-temp-file "mevedel-test-" t))
         (result-full nil)
         (result-offset nil))
    (unwind-protect
        (progn
          (dotimes (i 5)
            (with-temp-file (file-name-concat tmp-dir (format "f%d.txt" i))
              (insert "match\n")))
          (setq result-full
                (test-mevedel-tool-fs-search--await-callback
                 #'mevedel-tool-fs-search-grep
                 (list :pattern "match"
                       :path tmp-dir
                       :head_limit 0)))
          (setq result-offset
                (test-mevedel-tool-fs-search--await-callback
                 #'mevedel-tool-fs-search-grep
                 (list :pattern "match"
                       :path tmp-dir
                       :offset 2
                       :head_limit 0)))
          (let ((full-lines (seq-filter (lambda (l) (not (string-empty-p l)))
                                        (split-string result-full "\n")))
                (offset-lines (seq-filter (lambda (l) (not (string-empty-p l)))
                                          (split-string result-offset "\n"))))
            (should (= (- (length full-lines) 2) (length offset-lines)))))
      (delete-directory tmp-dir t)))
  :doc "context lines in content mode"
  (let* ((tmp-dir (make-temp-file "mevedel-test-" t))
         (result nil))
    (unwind-protect
        (progn
          (with-temp-file (file-name-concat tmp-dir "ctx.txt")
            (insert "before\ntarget\nafter\n"))
          (setq result
                (test-mevedel-tool-fs-search--await-callback
                 #'mevedel-tool-fs-search-grep
                 (list :pattern "target"
                       :path tmp-dir
                       :output_mode "content"
                       :context 1)))
          (should (string-match-p "before" result))
          (should (string-match-p "target" result))
          (should (string-match-p "after" result)))
      (delete-directory tmp-dir t)))
  :doc "multiline mode matches across lines"
  (let* ((tmp-dir (make-temp-file "mevedel-test-" t))
         (result nil))
    (unwind-protect
        (progn
          (with-temp-file (file-name-concat tmp-dir "multi.txt")
            (insert "start\nend\n"))
          (setq result
                (test-mevedel-tool-fs-search--await-callback
                 #'mevedel-tool-fs-search-grep
                 (list :pattern "start.*end"
                       :path tmp-dir
                       :multiline t
                       :output_mode "content")))
          (should (string-match-p "start" result)))
      (delete-directory tmp-dir t)))
  :doc "single file search"
  (let* ((tmp (make-temp-file "mevedel-test-"))
         (result nil))
    (unwind-protect
        (progn
          (with-temp-file tmp (insert "alpha\nbeta\ngamma\n"))
          (setq result
                (test-mevedel-tool-fs-search--await-callback
                 #'mevedel-tool-fs-search-grep
                 (list :pattern "beta"
                       :path tmp
                       :output_mode "content")))
          (should (string-match-p "beta" result))
          (should-not (string-match-p "alpha" result)))
      (delete-file tmp)))

  :doc "empty :glob string is treated as nil, not passed to rg"
  (let* ((tmp-dir (make-temp-file "mevedel-test-" t))
         (result nil))
    (unwind-protect
        (progn
          (with-temp-file (file-name-concat tmp-dir "code.el")
            (insert "target\n"))
          (setq result
                (test-mevedel-tool-fs-search--await-callback
                 #'mevedel-tool-fs-search-grep
                 (list :pattern "target"
                       :path tmp-dir
                       :glob "")))
          (should (string-match-p "code\\.el" result))
          (should-not (string-match-p "Error" result)))
      (delete-directory tmp-dir t)))

  :doc "empty :type string is treated as nil, not passed to rg"
  (let* ((tmp-dir (make-temp-file "mevedel-test-" t))
         (result nil))
    (unwind-protect
        (progn
          (with-temp-file (file-name-concat tmp-dir "code.el")
            (insert "target\n"))
          (setq result
                (test-mevedel-tool-fs-search--await-callback
                 #'mevedel-tool-fs-search-grep
                 (list :pattern "target"
                       :path tmp-dir
                       :type "")))
          (should (string-match-p "code\\.el" result))
          (should-not (string-match-p "unrecognized file type" result)))
      (delete-directory tmp-dir t)))

  :doc "empty :output_mode falls back to default files_with_matches"
  (let* ((tmp-dir (make-temp-file "mevedel-test-" t))
         (result nil))
    (unwind-protect
        (progn
          (with-temp-file (file-name-concat tmp-dir "m.el")
            (insert "target\n"))
          (setq result
                (test-mevedel-tool-fs-search--await-callback
                 #'mevedel-tool-fs-search-grep
                 (list :pattern "target"
                       :path tmp-dir
                       :output_mode "")))
          ;; files_with_matches: prints the path, not the line content.
          (should (string-match-p "m\\.el" result))
          (should-not (string-match-p "target" result)))
      (delete-directory tmp-dir t)))

  :doc "empty :path falls back to default current directory"
  (let* ((default-directory (make-temp-file "mevedel-test-" t))
         (result nil))
    (unwind-protect
        (progn
          (with-temp-file (file-name-concat default-directory "p.el")
            (insert "target\n"))
          (setq result
                (test-mevedel-tool-fs-search--await-callback
                 #'mevedel-tool-fs-search-grep
                 (list :pattern "target"
                       :path "")))
          (should (string-match-p "p\\.el" result)))
      (delete-directory default-directory t)))

  :doc ":json-false context args are ignored, not passed as -A%d"
  (let* ((tmp-dir (make-temp-file "mevedel-test-" t))
         (result nil))
    (unwind-protect
        (progn
          (with-temp-file (file-name-concat tmp-dir "c.el")
            (insert "line one\nfind me\nline three\n"))
          ;; Without the integer guard these would crash `format'.
          (setq result
                (test-mevedel-tool-fs-search--await-callback
                 #'mevedel-tool-fs-search-grep
                 (list :pattern "find me"
                       :path tmp-dir
                       :output_mode "content"
                       :-A :json-false
                       :-B :json-false
                       :-C :json-false)))
          (should (string-match-p "find me" result))
          (should-not (string-match-p "Error" result)))
      (delete-directory tmp-dir t)))

  :doc "non-integer :context is ignored"
  (let* ((tmp-dir (make-temp-file "mevedel-test-" t))
         (result nil))
    (unwind-protect
        (progn
          (with-temp-file (file-name-concat tmp-dir "q.el")
            (insert "hit\n"))
          (setq result
                (test-mevedel-tool-fs-search--await-callback
                 #'mevedel-tool-fs-search-grep
                 (list :pattern "hit"
                       :path tmp-dir
                       :output_mode "content"
                       :context "5")))
          (should (string-match-p "hit" result))
          (should-not (string-match-p "Error" result)))
      (delete-directory tmp-dir t))))

(mevedel-deftest mevedel-tool-fs-search-render-grep ()
  ,test
  (test)
  :doc "returns nil for non-string result"
  (should (null (mevedel-tool-fs-search-render-grep "Grep" '(:pattern "p") nil nil)))

  :doc "header includes pattern and match count; body-mode is grep-mode"
  (let* ((body "file1.el:10:match1\nfile1.el:22:match2\nfile2.el:3:match3\n")
         (plist (mevedel-tool-fs-search-render-grep
                 "Grep" '(:pattern "foo") body nil)))
    (should (string-match-p "\\`Grep: foo " (plist-get plist :header)))
    (should (string-match-p "3 matches" (plist-get plist :header)))
    (should (eq 'grep-mode (plist-get plist :body-mode))))

  :doc "no matches sentinel shows 0 matches, not 1"
  (let* ((plist (mevedel-tool-fs-search-render-grep
                 "Grep" '(:pattern "foo") "No matches found" nil)))
    (should (string-match-p "0 matches" (plist-get plist :header))))

  :doc "error message shows 0 matches, not 1"
  (let* ((plist (mevedel-tool-fs-search-render-grep
                 "Grep" '(:pattern "foo") "Error: search failed (exit code 2)\n\n" nil)))
    (should (string-match-p "0 matches" (plist-get plist :header))))

  :doc "match count ignores appended system reminders"
  (let* ((body "file.el:1:foo\n\n<system-reminder>\nuse xref\n</system-reminder>")
         (plist (mevedel-tool-fs-search-render-grep
                 "Grep" '(:pattern "foo") body nil)))
    (should (string-match-p "1 match" (plist-get plist :header))))

  :doc "match count preserves literal system reminder content"
  (let* ((body "file.el:1:<system-reminder>\nfile.el:2:sample\nfile.el:3:</system-reminder>")
         (plist (mevedel-tool-fs-search-render-grep
                 "Grep" '(:pattern "system-reminder") body nil)))
    (should (string-match-p "3 matches" (plist-get plist :header)))))

(mevedel-deftest mevedel-tool-fs-search-render-glob ()
  ,test
  (test)
  :doc "returns nil for non-string result"
  (should (null (mevedel-tool-fs-search-render-glob "Glob" '(:pattern "*.el") nil nil)))

  :doc "header includes pattern and file count"
  (let* ((body "a.el\nb.el\nc.el\n")
         (plist (mevedel-tool-fs-search-render-glob
                 "Glob" '(:pattern "*.el") body nil)))
    (should (string-match-p "\\`Glob: \\*\\.el " (plist-get plist :header)))
    (should (string-match-p "3 files" (plist-get plist :header))))

  :doc "no files sentinel shows 0 files, not 1"
  (let* ((plist (mevedel-tool-fs-search-render-glob
                 "Glob" '(:pattern "*.el") "No files found matching pattern" nil)))
    (should (string-match-p "0 files" (plist-get plist :header))))

  :doc "error message shows 0 files, not 1"
  (let* ((plist (mevedel-tool-fs-search-render-glob
                 "Glob" '(:pattern "*.el") "Error: glob failed (exit code 2)\n\n" nil)))
    (should (string-match-p "0 files" (plist-get plist :header))))

  :doc "truncation marker is not counted as a file"
  (let* ((plist (mevedel-tool-fs-search-render-glob
                 "Glob" '(:pattern "*.el")
                 "a.el\nb.el\n... Results truncated (limit: 2)" nil)))
    (should (string-match-p "2 files" (plist-get plist :header)))))

(provide 'test-mevedel-tool-fs-search)
;;; test-mevedel-tool-fs-search.el ends here
