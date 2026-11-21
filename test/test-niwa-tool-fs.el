;;; test-niwa-tool-fs.el -- Tests for file-system tool registration -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'niwa-tool-fs)
(require 'niwa-structs)
(require 'niwa-tool-registry)
(require 'niwa-workspace)
(require 'niwa-execution-target)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name
               load-file-name
               byte-compile-current-file))
          "helpers"))

;;
;;; External helper execution

(niwa-deftest niwa-tool-fs-executable-find ()
  ,test
  (test)
  :doc "searches in the path's local or remote execution target"
  (let ((niwa--executable-cache (make-hash-table :test #'equal))
        calls)
    (cl-letf (((symbol-function 'executable-find)
               (lambda (name &optional remote)
                 (push (list name remote) calls)
                 "/bin/rg")))
      (should (niwa-tool-fs-executable-find "rg" "/tmp/project/"))
      (should
       (niwa-tool-fs-executable-find
        "rg" "/ssh:user@host:/srv/project/")))
    (should
     (equal '(("rg" nil) ("rg" "/ssh:user@host:"))
            (seq-filter (lambda (call) (equal (car call) "rg"))
                        (nreverse calls))))))

(niwa-deftest niwa-tool-fs-model-path ()
  ,test
  (test)
  :doc "removes the current session target prefix"
  (let* ((root "/ssh:builder@example.test:/srv/project/")
         (workspace (niwa-workspace--create
                     :type 'project :id "model-path"
                     :root root :name "model-path"))
         (niwa--session (niwa-session-create "main" workspace)))
    (should (equal "/srv/project/src/main.el"
                   (niwa-tool-fs-model-path
                    "/ssh:builder@example.test:/srv/project/src/main.el")))))

;;
;;; Tool registration

(niwa-deftest niwa-tool-fs--register/path-contracts
  (:before-each (niwa-tool-clear-registry)
   :after-each (niwa-tool-clear-registry))
  ,test
  (test)

  :doc "declares paths semantically while keeping patterns and globs strings"
  (progn
    (niwa-tool-fs--register)
    (dolist (entry '(("Glob" path)
                     ("Read" file_path)
                     ("Grep" path)))
      (should
       (eq 'path-or-resource
           (cadr (assq (cadr entry)
                       (niwa-tool-args (niwa-tool-get (car entry))))))))
    (should
     (eq 'string
         (cadr (assq 'pattern
                     (niwa-tool-args (niwa-tool-get "Glob"))))))
    (should
     (eq 'string
         (cadr (assq 'glob
                     (niwa-tool-args (niwa-tool-get "Grep"))))))
    (should
     (string-match-p
      "explicit inclusion"
      (nth 3 (assq 'glob
                   (niwa-tool-args (niwa-tool-get "Grep"))))))
    (should
     (string-match-p
      "explicitly selected ignored path"
      (nth 3 (assq 'path
                   (niwa-tool-args (niwa-tool-get "Grep")))))))

  :doc "distinguishes Glob discovery from Grep on concrete live history and shared text"
  (progn
    (niwa-tool-fs--register)
    (dolist (name '("Glob" "Grep"))
      (let ((prompt (niwa-tool-prompt (niwa-tool-get name))))
        (should (string-search "`history://saved`" prompt))
        (should (string-search "selected `skill://` package" prompt))
        (if (equal name "Glob")
            (should (string-search "Live history, agent, and MCP addresses do not support Glob" prompt))
          (should (string-search "`history://root`" prompt))
          (should (string-search "`shared://`" prompt))
          (should (string-search "Bare `history://`" prompt))
          (should-not (string-search "Live history, agent, and MCP addresses do not support Grep" prompt)))))))

(niwa-deftest niwa-tool-fs-strip-system-reminders ()
  ,test
  (test)
  :doc "returns non-string results unchanged"
  (should (eq :not-string
              (niwa-tool-fs-strip-system-reminders :not-string)))

  :doc "returns strings without reminders unchanged"
  (let ((body "1->foo\n2->bar\n"))
    (should (eq body (niwa-tool-fs-strip-system-reminders body))))

  :doc "strips only trailing appended system reminders"
  (let ((body "1->foo\n\n<system-reminder>\nuse Imenu\n</system-reminder>"))
    (should (equal "1->foo"
                   (niwa-tool-fs-strip-system-reminders body))))

  :doc "preserves marker-looking file content"
  (let ((body "1-><system-reminder>\n2->sample\n3-></system-reminder>"))
    (should (equal body (niwa-tool-fs-strip-system-reminders body)))))

(niwa-deftest niwa-tool-fs-display-path ()
  ,test
  (test)

  :doc "resolves a relative header path in the selected agent directory"
  (let* ((workspace (niwa-workspace--create :root "/tmp/project/"))
         (niwa--session (niwa-session--create
                            :workspace workspace :working-directory "/tmp/project/"))
         (niwa--conversation-working-directory "/tmp/project/child/"))
    (should (equal "child/file.el" (niwa-tool-fs-display-path "file.el"))))

  :doc "shortens a path under the workspace root without touching the target"
  ;; Runs from a tool renderer, and renderers run from the redraw path:
  ;; a stat here lands inside whatever remote command the redraw
  ;; interrupted, TRAMP refuses it, and the row loses its header.
  (let ((root "/niwamock:host:/srv/project/")
        (stats 0))
    (cl-letf (((symbol-function 'niwa-tool-fs-current-workspace-root)
               (lambda () root))
              ((symbol-function 'file-in-directory-p)
               (lambda (&rest _) (setq stats (1+ stats)) t))
              ((symbol-function 'file-truename)
               (lambda (name &rest _) (setq stats (1+ stats)) name)))
      (niwa-test--with-captured-diagnostics nil
        (should (equal "src/main.py"
                       (niwa-tool-fs-display-path
                        (file-name-concat root "src/main.py"))))
        (should (= 0 stats)))))

  :doc "a path outside the root falls back to its base name"
  (let ((root "/niwamock:host:/srv/project/"))
    (cl-letf (((symbol-function 'niwa-tool-fs-current-workspace-root)
               (lambda () root)))
      (should (equal "elsewhere.py"
                     (niwa-tool-fs-display-path
                      "/niwamock:host:/other/elsewhere.py"))))))

(provide 'test-niwa-tool-fs)
;;; test-niwa-tool-fs.el ends here
