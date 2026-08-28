;;; test-mevedel-tool-fs.el -- Tests for file-system tool registration -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'mevedel-tool-fs)
(require 'mevedel-structs)
(require 'mevedel-tool-registry)
(require 'mevedel-workspace)
(require 'mevedel-execution-target)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name
               load-file-name
               byte-compile-current-file))
          "helpers"))

;;
;;; External helper execution

(mevedel-deftest mevedel-tool-fs-executable-find ()
  ,test
  (test)
  :doc "searches in the path's local or remote execution target"
  ;; The lookup is memoized per (target . name), so the cache is cleared
  ;; around the case: a leftover entry would answer without consulting the
  ;; stub and the recorded calls would be empty.
  (let (calls)
    (unwind-protect
        (progn
          (clrhash mevedel--executable-cache)
          (cl-letf (((symbol-function 'executable-find)
                     (lambda (name &optional remote)
                       (push (list name remote) calls)
                       "/bin/rg")))
            (should (mevedel-tool-fs-executable-find "rg" "/tmp/project/"))
            (should
             (mevedel-tool-fs-executable-find
              "rg" "/ssh:user@host:/srv/project/")))
          (should
           (equal '(("rg" nil) ("rg" "/ssh:user@host:"))
                  (seq-filter (lambda (call) (equal (car call) "rg"))
                              (nreverse calls)))))
      (clrhash mevedel--executable-cache))))

(mevedel-deftest mevedel-tool-fs-model-path ()
  ,test
  (test)
  :doc "removes the current session target prefix"
  (let* ((root "/ssh:builder@example.test:/srv/project/")
         (workspace (mevedel-workspace--create
                     :type 'project :id "model-path"
                     :root root :name "model-path"))
         (mevedel--session (mevedel-session-create "main" workspace)))
    (should (equal "/srv/project/src/main.el"
                   (mevedel-tool-fs-model-path
                    "/ssh:builder@example.test:/srv/project/src/main.el")))))

;;
;;; Tool registration

(mevedel-deftest mevedel-tool-fs--register/path-contracts
  (:before-each (mevedel-tool-clear-registry)
   :after-each (mevedel-tool-clear-registry))
  ,test
  (test)

  :doc "declares paths semantically while keeping patterns and globs strings"
  (progn
    (mevedel-tool-fs--register)
    (dolist (entry '(("Glob" path)
                     ("Read" file_path)
                     ("Grep" path)))
      (should
       (eq 'path-or-resource
           (cadr (assq (cadr entry)
                       (mevedel-tool-args (mevedel-tool-get (car entry))))))))
    (should
     (eq 'string
         (cadr (assq 'pattern
                     (mevedel-tool-args (mevedel-tool-get "Glob"))))))
    (should
     (eq 'string
         (cadr (assq 'glob
                     (mevedel-tool-args (mevedel-tool-get "Grep"))))))
    (should
     (string-match-p
      "explicit inclusion"
      (nth 3 (assq 'glob
                   (mevedel-tool-args (mevedel-tool-get "Grep"))))))
    (should
     (string-match-p
      "explicitly selected ignored path"
      (nth 3 (assq 'path
                   (mevedel-tool-args (mevedel-tool-get "Grep"))))))))

(mevedel-deftest mevedel-tool-fs-strip-system-reminders ()
  ,test
  (test)
  :doc "returns non-string results unchanged"
  (should (eq :not-string
              (mevedel-tool-fs-strip-system-reminders :not-string)))

  :doc "returns strings without reminders unchanged"
  (let ((body "1->foo\n2->bar\n"))
    (should (eq body (mevedel-tool-fs-strip-system-reminders body))))

  :doc "strips only trailing appended system reminders"
  (let ((body "1->foo\n\n<system-reminder>\nuse Imenu\n</system-reminder>"))
    (should (equal "1->foo"
                   (mevedel-tool-fs-strip-system-reminders body))))

  :doc "preserves marker-looking file content"
  (let ((body "1-><system-reminder>\n2->sample\n3-></system-reminder>"))
    (should (equal body (mevedel-tool-fs-strip-system-reminders body)))))

(mevedel-deftest mevedel-tool-fs-display-path ()
  ,test
  (test)

  :doc "shortens a path under the workspace root without touching the target"
  ;; Runs from a tool renderer, and renderers run from the redraw path:
  ;; a stat here lands inside whatever remote command the redraw
  ;; interrupted, TRAMP refuses it, and the row loses its header.
  (let ((root "/mevedelmock:host:/srv/project/")
        (stats 0))
    (cl-letf (((symbol-function 'mevedel-tool-fs-current-workspace-root)
               (lambda () root))
              ((symbol-function 'file-in-directory-p)
               (lambda (&rest _) (setq stats (1+ stats)) t))
              ((symbol-function 'file-truename)
               (lambda (name &rest _) (setq stats (1+ stats)) name)))
      (mevedel-test--with-captured-diagnostics nil
        (should (equal "src/main.py"
                       (mevedel-tool-fs-display-path
                        (concat root "src/main.py"))))
        (should (= 0 stats)))))

  :doc "a path outside the root falls back to its base name"
  (let ((root "/mevedelmock:host:/srv/project/"))
    (cl-letf (((symbol-function 'mevedel-tool-fs-current-workspace-root)
               (lambda () root)))
      (should (equal "elsewhere.py"
                     (mevedel-tool-fs-display-path
                      "/mevedelmock:host:/other/elsewhere.py"))))))

(provide 'test-mevedel-tool-fs)
;;; test-mevedel-tool-fs.el ends here
