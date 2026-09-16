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
          (dolist (relative '("docs/memory.md" "docs/tools/execution.md"))
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
                               (let* ((attempt (mevedel-resource-prepare
                                                operation address nil))
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
                                   (should-not (string-match-p
                                                (regexp-quote ,root) text))
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
                          (let ((matches (invoke 'grep "mevedel://"
                                                 :pattern "needle"
                                                 :output_mode "content")))
                            (should (string-match-p "public needle" matches))
                            (should (string-match-p "mevedel://" matches))
                            (should-not (string-match-p "private needle" matches)))
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

(provide 'test-mevedel-resource-installation)
;;; test-mevedel-resource-installation.el ends here
