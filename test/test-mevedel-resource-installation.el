;;; test-mevedel-resource-installation.el -- Installed package assets -*- lexical-binding: t -*-

;;; Commentary:

;; Exercise real compiled owners in a fresh Emacs, including the package-manager
;; layout where compiled files and symlinked source have different directories.

;;; Code:

(require 'bytecomp)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-tool-fs-search)

(mevedel-deftest mevedel-resource-installed-package
  ()
  (let* ((repo (file-name-directory (locate-library "mevedel-resource")))
         (root (make-temp-file "mevedel-installed-package-" t))
         (source-less ,source-less)
         (build (file-name-concat root "build"))
         (source (if source-less build (file-name-concat root "source")))
         (emacs (file-name-concat invocation-directory invocation-name))
         (owners '("mevedel-resource" "mevedel-system" "mevedel-tool-registry")))
    (unwind-protect
        (progn
          (make-directory source t)
          (make-directory build t)
          (dolist (owner owners)
            (let ((name (concat owner ".el")))
              (copy-file (file-name-concat repo name)
                         (file-name-concat source name))
              (unless source-less
                (make-symbolic-link (file-name-concat source name)
                                    (file-name-concat build name)))
              (let ((byte-compile-verbose nil)
                    (byte-compile-error-on-warn t))
                (should (byte-compile-file (file-name-concat build name))))
              (when source-less (delete-file (file-name-concat build name)))))
          (dolist (dir '("prompts" "agents"))
            (copy-directory (file-name-concat repo dir)
                            (file-name-concat source dir) nil t t))
          (dolist (relative '("docs/memory.md" "docs/tools/execution.md" "docs/tools/ignored.txt"))
            (let ((file (file-name-concat source relative))
                  (link (file-name-concat build relative)))
              (make-directory (file-name-directory file) t)
              (make-directory (file-name-directory link) t)
              (with-temp-file file (insert "# Installed manual\npublic needle\n"))
              (unless source-less (make-symbolic-link file link))))
          (let ((skill (file-name-concat source "skills/fixture/SKILL.md")))
            (make-directory (file-name-directory skill) t)
            (with-temp-file skill
              (insert "---\nname: fixture\ndescription: Installed fixture\n---\nBody.\n")))
          (with-temp-file (file-name-concat source "private.md")
            (insert "private needle\n"))
          (make-symbolic-link (file-name-concat source "private.md")
                              (file-name-concat source "docs/escape.md"))
          (with-temp-file (file-name-concat source "docs/ignored.txt")
            (insert "private needle\n"))
          (with-temp-buffer
            (let ((status
                   (call-process
                    emacs nil t nil "--batch" "-Q" "--eval"
                    (prin1-to-string
                     `(progn
                        (setq load-path ',(cons build load-path)
                              native-comp-jit-compilation nil)
                        (require 'ert)
                        (require 'cl-lib)
                        (require 'gptel-request)
                        (require 'mevedel-resource)
                        (require 'mevedel-system)
                        (require 'mevedel-tool-registry)
                        (require 'mevedel-tool-fs-read)
                        (require 'mevedel-tool-fs-search)
                        (require 'mevedel-execution)
                        (require 'mevedel-skills-core)
                        ;; Do not pre-bind source dirs: exercise cold initializers.
                        (dolist (owner ',owners)
                          (should
                           (equal (symbol-file (intern owner) 'provide)
                                  (file-name-concat ,build (concat owner ".elc")))))
                        (cl-labels
                            ((invoke (operation address &rest options)
                               (let* ((attempt (and (mevedel-resource-address-like-p address)
                                                    (mevedel-resource-prepare
                                                     operation address nil)))
                                      (mevedel-resource-current-attempts
                                       (list (cons address attempt)))
                                      (args (append
                                             (list (if (eq operation 'read)
                                                       :file_path :path)
                                                   address)
                                             options))
                                      (deadline (+ (float-time) 10))
                                      done result)
                                 (funcall
                                  (pcase operation
                                    ('read #'mevedel-tool-fs-read)
                                    ('glob #'mevedel-tool-fs-search-glob)
                                    ('grep #'mevedel-tool-fs-search-grep))
                                  (lambda (value) (setq result value done t))
                                  args)
                                 (while (and (not done) (< (float-time) deadline))
                                   (accept-process-output nil 0.01))
                                 (should done)
                                 (let ((text (plist-get result :result)))
                                   (should (stringp text))
                                   (when attempt
                                     (should-not (string-match-p
                                                  (regexp-quote ,root) text)))
                                   text))))
                          (dolist (address '("mevedel://memory.md"
                                             "mevedel://tools/execution.md"))
                            (should (string-match-p
                                     "Installed manual" (invoke 'read address))))
                          (let ((listing (invoke 'read "mevedel://")))
                            (should (string-match-p "mevedel://memory.md" listing))
                            (should (string-match-p
                                     "mevedel://tools/execution.md" listing))
                            (should-not (string-match-p "escape\\|ignored" listing)))
                          (should (string-match-p
                                   "mevedel://tools/execution.md"
                                   (invoke 'glob "mevedel://tools" :pattern "*.md")))
                          (let ((listing (invoke 'glob "mevedel://tools" :pattern "*")))
                            (should (string-match-p "mevedel://tools/execution.md" listing))
                            (should-not (string-match-p "ignored" listing)))
                          (should-not (string-match-p
                                       "ignored"
                                       (invoke 'glob "mevedel://tools" :pattern "*.txt")))
                          (let ((matches (invoke 'grep "mevedel://"
                                                 :pattern "needle"
                                                 :output_mode "content")))
                            (should (string-match-p "public needle" matches))
                            (should (string-match-p "mevedel://" matches))
                            (should-not (string-match-p "private needle" matches)))
                          (dolist (address '("mevedel://" "mevedel://tools"))
                            (dolist (mode '("content" "count" "files_with_matches"))
                              (should (equal "No matches found"
                                             (invoke 'grep address :pattern "needle"
                                                     :glob "**/*.txt" :output_mode mode)))
                              (let ((matches (invoke 'grep address :pattern "needle"
                                                     :glob "**/execution.md" :type "md"
                                                     :output_mode mode)))
                                (should (string-search "mevedel://tools/execution.md" matches))
                                (should-not (string-search "memory.md" matches))
                                (should-not (string-search "ignored.txt" matches)))
                              (should (equal "No matches found"
                                             (invoke 'grep address :pattern "needle"
                                                     :type "rust" :output_mode mode)))
                              ;; Native positive globs override type filters.
                              (should (string-search
                                       "mevedel://tools/execution.md"
                                       (invoke 'grep address :pattern "needle"
                                               :glob "**/*.md" :type "rust"
                                               :output_mode mode)))))
                          ;; Ordinary-path positive globs still select text files.
                          (dolist (mode '("content" "count" "files_with_matches"))
                            (should (string-search
                                     "ignored.txt"
                                     (invoke 'grep ,(file-name-concat source "docs")
                                             :pattern "needle" :glob "**/*.txt"
                                             :output_mode mode))))
                          ;; Explicit files retain native selection semantics,
                          ;; including positive globs bypassing type selection.
                          (dolist (mode '("content" "count" "files_with_matches"))
                            (dolist (options '(nil (:glob "**/*.txt" :type "rust")))
                              (let ((matches (apply #'invoke 'grep "mevedel://memory.md"
                                                    :pattern "needle" :output_mode mode
                                                    options)))
                                (should-not (string-prefix-p "Error:" matches))
                                (should-not (equal "No matches found" matches))
                                (should (string-search
                                         (pcase mode
                                           ("content" "public needle")
                                           ("count" "1")
                                           (_ "mevedel://memory.md"))
                                         matches)))))
                          ;; Unknown types fail in both native and package searches.
                          (dolist (address (list "mevedel://" "mevedel://tools"
                                                 "mevedel://memory.md"
                                                 ,(file-name-concat source "docs")))
                            (let ((failure (invoke 'grep address :pattern "needle"
                                                   :type "mevedel-unknown-type")))
                              (should (string-prefix-p "Error:" failure))
                              (should-not (string-search "public needle" failure))))
                          (dolist (address '("mevedel://escape.md"
                                             "mevedel://../private.md"))
                            (should-error (invoke 'read address)
                                          :type 'mevedel-resource-error))
                          (dolist (address '("mevedel://ignored.txt"
                                             "mevedel://missing.md"
                                             "mevedel://../mevedel-resource.el"))
                            (should-error (invoke 'read address)))
                          (should-error
                           (mevedel-resource-prepare
                            'apply-patch "mevedel://memory.md" nil)
                           :type 'mevedel-resource-error)
                          ;; Existing asset consumers must keep working too.
                          (should (stringp (mevedel-system-render-prompt-file
                                            "prompts/system/base.md")))
                          (should (stringp (mevedel-system-render-prompt-file
                                            "agents/worker.md")))
                          (eval '(mevedel-define-tool
                                  :name "InstalledFixture" :description "Fixture"
                                  :prompt-file "prompts/tools/read.md"
                                  :args nil :handler #'ignore))
                          (should (> (length (mevedel-tool-prompt
                                              (mevedel-tool-get "InstalledFixture")))
                                     20))
                          (let ((skills (mevedel-skills-scan nil nil)))
                            (should (cl-find "fixture" skills
                                             :key #'mevedel-skill-name
                                             :test #'equal)))
                          (delete-directory (file-name-concat ,source "docs") t)
                          (should-error (invoke 'read "mevedel://")
                                        :type 'mevedel-resource-unavailable)))))))
              (ert-info ((buffer-string))
                (should (equal status 0))
                (should (string-empty-p (buffer-string)))))))
      (delete-directory root t)))
  (source-less)
  :doc "compiled owners find source assets without admitting resource symlinks"
  nil
  :doc "source-less compiled owners use assets alongside the loaded library"
  t)

(mevedel-deftest mevedel-tool-fs-search--start-grep ()
  ,test
  (test)
  :doc "public package Grep whitelists before content and retains deadline and cancellation"
  (let* ((root (make-temp-file "mevedel-package-grep-lifecycle-" t))
         (docs (file-name-concat root "docs"))
         (guide (file-name-concat docs "guide.md"))
         (ignored (file-name-concat docs "ignored.txt"))
         (escape (file-name-concat docs "escape.md"))
         (mevedel-resource--source-dir root)
         (mevedel-tool-fs-search-timeout 20)
         (now 100.0)
         calls results delivery-error)
    (unwind-protect
        (progn
          (make-directory docs t)
          (write-region "public needle\n" nil guide nil 'silent)
          (write-region "private needle\n" nil ignored nil 'silent)
          (write-region "private needle\n" nil (file-name-concat root "private.md") nil 'silent)
          (make-symbolic-link (file-name-concat root "private.md") escape)
          (cl-letf (((symbol-function 'float-time) (lambda (&optional _time) now))
                    ((symbol-function 'mevedel-execution-start-helper)
                     (lambda (callback name command _read-paths _writable-roots &rest options)
                       (let ((call (list :callback callback :name name :command command
                                         :options options :cancel-count 0)))
                         (setq calls (append calls (list call)))
                         (lambda () (cl-incf (plist-get call :cancel-count)))))))
            (cl-labels
                ((invoke (&rest options)
                   (setq calls nil results nil delivery-error nil now 100.0)
                   (let* ((address "mevedel://")
                          (attempt (mevedel-resource-prepare 'grep address nil))
                          (mevedel-resource-current-attempts (list (cons address attempt))))
                     (mevedel-tool-fs-search-grep
                      (lambda (result)
                        (push result results)
                        (when delivery-error (error "Downstream delivery failed")))
                      (append options
                              (list :path address :pattern "needle" :glob "**/*" :type "md"
                                    :output_mode "content")))))
                 (deliver (index result)
                   (funcall (plist-get (nth index calls) :callback) result)))
              (let ((cancel (invoke)))
                (should (equal "mevedel-grep-files" (plist-get (car calls) :name)))
                (should (member "--glob=**/*" (plist-get (car calls) :command)))
                (should (member "--type=md" (plist-get (car calls) :command)))
                (setq now 105.0)
                ;; Even an unexpected discovery candidate cannot reach content.
                (deliver 0 (list :exit-code 0 :output (concat ignored "\0" escape "\0" guide "\0")))
                (should (= 2 (length calls)))
                (should (member guide (plist-get (cadr calls) :command)))
                (should-not (member ignored (plist-get (cadr calls) :command)))
                (should-not (member escape (plist-get (cadr calls) :command)))
                (should (= 15.0 (plist-get (plist-get (cadr calls) :options) :timeout)))
                (funcall cancel)
                (funcall cancel)
                (dolist (call calls) (should (= 1 (plist-get call :cancel-count))))
                (deliver 1 '(:exit-code 0 :output "late result"))
                (should-not results))
              (let ((cancel (invoke)))
                (funcall cancel)
                (deliver 0 (list :exit-code 0 :output (concat guide "\0")))
                (should (= 1 (length calls)))
                (should-not results))
              ;; A downstream failure is not a second Grep result.  Both stages
              ;; remain closed to late callbacks, even before cancellation.
              (let ((cancel (invoke)))
                (deliver 0 (list :exit-code 0 :output (concat guide "\0")))
                (setq delivery-error t)
                (should (equal '(error "Downstream delivery failed")
                               (should-error (deliver 1 '(:exit-code 1 :output "")))))
                (should (= 1 (length results)))
                (deliver 1 '(:exit-code 0 :output "late result"))
                (deliver 0 (list :exit-code 0 :output (concat guide "\0")))
                (should (= 2 (length calls)))
                (should (= 1 (length results)))
                (funcall cancel)
                (funcall cancel)
                (dolist (call calls) (should (= 1 (plist-get call :cancel-count)))))
              (let ((cancel (invoke :-i t :multiline t :-A 2 :-B 1 :context 3
                                    :-n :json-false :head_limit 4 :offset 2)))
                (deliver 0 (list :exit-code 0 :output (concat guide "\0")))
                (let ((command (plist-get (cadr calls) :command)))
                  (dolist (selector '("-i" "-U" "--multiline-dotall" "-A2" "-B1"
                                      "-C3" "--max-count=7" "--with-filename"
                                      "--glob=**/*" "--type=md" "-e" "needle"))
                    (should (member selector command)))
                  (should-not (member "--line-number" command)))
                (funcall cancel))
              (dolist (failure (list
                               (list :exit-code 1 :timed-out-p t :output (concat guide "\0"))
                               (list :exit-code 1 :output-limit-p t :output (concat guide "\0"))
                               (list :exit-code 0 :output guide)
                               (list :exit-code 0 :output (make-string
                                                          (1+ mevedel-tool-fs-search--grep-max-output-bytes) ?x))
                               '(:exit-code 2 :output "private discovery failure")))
                (invoke)
                (deliver 0 failure)
                (should (= 1 (length calls)))
                (should (= 1 (length results)))
                (should (string-prefix-p "Error:" (plist-get (car results) :result)))
                (should-not (string-search root (plist-get (car results) :result)))
                (should-not (string-search "private" (plist-get (car results) :result)))
                (deliver 0 (list :exit-code 0 :output (concat guide "\0")))
                (should (= 1 (length calls)))
                (should (= 1 (length results))))
              ;; Delivered startup failures need the same privacy treatment as
              ;; signalled startup failures, including after file selection.
              (dolist (stage '(0 1))
                (invoke)
                (when (= stage 1)
                  (deliver 0 (list :exit-code 0 :output (concat guide "\0"))))
                (deliver stage
                         '(:exit-code -1 :output ""
                           :error (file-error "Cannot start helper" "/private/scratch")))
                (should (= 1 (length results)))
                (should (string-prefix-p "Error:" (plist-get (car results) :result)))
                (should-not (string-search "/private/scratch"
                                           (plist-get (car results) :result))))
              (invoke)
              (setq now 121.0)
              (deliver 0 (list :exit-code 0 :output (concat guide "\0")))
              (should (= 1 (length calls)))
              (should (string-search "timed out" (plist-get (car results) :result))))))
      (delete-directory root t)))
  :doc "public resource search settles staged or direct scratch failure once without private paths"
  (pcase-dolist (`(,operation ,address ,failure-stage)
                 '((grep "mevedel://" 1) (grep "mevedel://" 2)
                   (grep "mevedel://guide.md" 1) (glob "mevedel://" 1)))
    (let* ((root (make-temp-file "mevedel-package-grep-startup-" t))
           (docs (file-name-concat root "docs"))
           (blocked (file-name-concat root "private-scratch-parent"))
           (workspace (mevedel-workspace--create :root root))
           (session (mevedel-session--create :workspace workspace :permission-mode 'full-auto))
           (mevedel-resource--source-dir root)
           (make-temp (symbol-function 'make-temp-file))
           (scratch-attempts 0)
           scratch-failure escaped results cancel attempt)
      (unwind-protect
          (with-temp-buffer
            (setq-local mevedel--session session mevedel--workspace workspace
                        default-directory (file-name-as-directory root))
            (make-directory docs t)
            (write-region "public needle\n" nil (file-name-concat docs "guide.md") nil 'silent)
            ;; A regular file cannot be the parent of a helper scratch directory.
            (write-region "blocked" nil blocked nil 'silent)
            (cl-letf (((symbol-function 'make-temp-file)
                       (lambda (prefix &optional directory suffix text)
                         (let ((temporary-file-directory
                                (if (and directory (equal prefix "mevedel-helper-")
                                         (= failure-stage (cl-incf scratch-attempts)))
                                    (file-name-as-directory blocked)
                                  temporary-file-directory)))
                           (condition-case failure
                               (funcall make-temp prefix directory suffix text)
                             (file-error
                              (setq scratch-failure failure)
                              (signal (car failure) (cdr failure))))))))
              (mevedel-test--with-captured-diagnostics nil
                (setq attempt (mevedel-resource-prepare
                               operation address (list :session session)))
                (setq cancel
                      (mevedel-pipeline--step-handler
                       (list :tool (mevedel-tool--create
                                    :name (if (eq operation 'grep) "Grep" "Glob")
                                    :handler (if (eq operation 'grep)
                                                 #'mevedel-tool-fs-search-grep
                                               #'mevedel-tool-fs-search-glob)
                                    :async-p t)
                             :args (list :path address
                                         :pattern (if (eq operation 'grep) "needle" "*.md")
                                         :output_mode "content")
                             :resource-attempts
                             (list (cons address attempt)))
                       (lambda (context) (push context results))
                       (lambda (&rest failure) (ert-fail failure))))
                (let ((deadline (+ (float-time) 5)))
                  (condition-case failure
                      (while (and (not results) (not scratch-failure)
                                  (< (float-time) deadline))
                        (accept-process-output nil 0.01))
                    (error (setq escaped failure)))))
              (should (= failure-stage scratch-attempts))
              (should (eq 'file-error (car scratch-failure)))
              (should-not escaped)
              (should-not (gethash attempt mevedel-resource--attempt-table))
              (should-not (mevedel-execution-session-live-p session))
              (should (= 1 (length results)))
              (let ((result (car results)))
                (should (eq 'error (plist-get result :handler-status)))
                (should (string-prefix-p "Error:" (plist-get result :result)))
                (should-not (string-search root (plist-get result :result)))
                (should-not (string-search "private-scratch-parent" (plist-get result :result))))
              (funcall cancel)
              (funcall cancel)
              (accept-process-output nil 0.01)
              (should (= 1 (length results)))))
        (when (functionp cancel) (funcall cancel))
        (delete-directory root t))))
  :doc "initial startup failure propagates downstream callback errors without redelivery"
  (let ((root (make-temp-file "mevedel-package-grep-callback-" t))
        (mevedel-tool-fs-search--resource-address "mevedel://")
        (deliveries 0))
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel-execution-start-helper)
                   (lambda (&rest _)
                     (signal 'file-error '("Private startup failure" "/private/scratch")))))
          (should
           (equal '(error "Downstream delivery failed")
                  (should-error
                   (mevedel-tool-fs-search--start-grep
                    (lambda (result)
                      (cl-incf deliveries)
                      (should-not (string-search "/private/scratch" (plist-get result :output)))
                      (error "Downstream delivery failed"))
                    nil (list root) (list root) nil nil nil))))
          (should (= 1 deliveries)))
      (delete-directory root t))))

(provide 'test-mevedel-resource-installation)
;;; test-mevedel-resource-installation.el ends here
