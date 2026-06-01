;;; test-mevedel-tool-code.el --- Tests for code exploration tools -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'gptel)
(require 'mevedel-tool-registry)
(require 'mevedel-tool-code)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name
               load-file-name
               byte-compile-current-file))
          "helpers"))


;;
;;; Tool registration

(mevedel-deftest mevedel-tool-code--register
  (:before-each (mevedel-tool-clear-registry)
   :after-each (mevedel-tool-clear-registry))
  ,test
  (test)

  :doc "registers concise summaries for xref and imenu roster entries"
  (progn
    (mevedel-tool-code--register)
    (should (equal "LSP-aware symbol references, callers, and impact analysis."
                   (mevedel-tool-summary
                    (mevedel-tool-get "XrefReferences"))))
    (should (equal "LSP-aware symbol definitions and name discovery."
                   (mevedel-tool-summary
                    (mevedel-tool-get "XrefDefinitions"))))
    (should (equal "Fast outline of functions, classes, and variables in one file."
                   (mevedel-tool-summary
                    (mevedel-tool-get "Imenu"))))))


;;
;;; Xref references

(mevedel-deftest mevedel-tool-code--xref-references ()
  ,test
  (test)
  :doc "finds references to an elisp symbol"
  (let* ((tmp (make-temp-file "mevedel-test-" nil ".el"))
         (result nil))
    (unwind-protect
        (progn
          (with-temp-file tmp
            (insert "(defun my-test-fn-12345 () nil)\n"
                    "(my-test-fn-12345)\n"
                    "(my-test-fn-12345)\n"))
          (mevedel-tool-code--xref-references
           (lambda (r) (setq result r))
           (list :identifier "my-test-fn-12345" :file_path tmp))
          (should (stringp result))
          ;; Should find at least the two call sites
          (should (string-match-p "my-test-fn-12345" result)))
      (when-let* ((buf (find-buffer-visiting tmp)))
        (kill-buffer buf))
      (delete-file tmp)))
  :doc "returns message when no references found"
  (let* ((tmp (make-temp-file "mevedel-test-" nil ".el"))
         (identifier (format "nonexistent-symbol-%s" (file-name-base tmp)))
         (result nil))
    (unwind-protect
        (progn
          (with-temp-file tmp
            (insert "(defun some-unique-fn-99999 () nil)\n"))
          (mevedel-tool-code--xref-references
           (lambda (r) (setq result r))
           (list :identifier identifier :file_path tmp))
          (should (stringp result))
          (should (string-match-p "No references found\\|Error" result)))
      (when-let* ((buf (find-buffer-visiting tmp)))
        (kill-buffer buf))
      (delete-file tmp)))
  :doc "errors on non-existent file"
  (should-error
   (mevedel-tool-code--xref-references
    #'ignore (list :identifier "foo" :file_path "/nonexistent/file.el"))
   :type 'error))


;;
;;; Xref definitions

(mevedel-deftest mevedel-tool-code--xref-definitions ()
  ,test
  (test)
  :doc "errors on non-existent file"
  (should-error
   (mevedel-tool-code--xref-definitions
    #'ignore (list :pattern "foo" :file_path "/nonexistent/file.el"))
   :type 'error)
  :doc "returns message when no backend or no results"
  (let* ((tmp (make-temp-file "mevedel-test-" nil ".el"))
         (result nil))
    (unwind-protect
        (progn
          (with-temp-file tmp
            (insert "(defun test-fn () nil)\n"))
          (mevedel-tool-code--xref-definitions
           (lambda (r) (setq result r))
           (list :pattern "zzz-nonexistent-pattern-zzz" :file_path tmp))
          (should (stringp result))
          (should (string-match-p "No symbols found\\|No xref\\|No tags\\|Error" result)))
      (when-let* ((buf (find-buffer-visiting tmp)))
        (kill-buffer buf))
      (delete-file tmp)))
  :doc "loads apropos before regex symbol search"
  (let* ((tmp (make-temp-file "mevedel-test-" nil ".el"))
         (prefix (replace-regexp-in-string "[^[:alnum:]]" "-"
                                           (file-name-base tmp)))
         (alpha (intern (format "%s-apropos-alpha" prefix)))
         (beta (intern (format "%s-apropos-beta" prefix)))
         (result nil))
    (unwind-protect
        (progn
          (with-temp-file tmp
            (insert (format "(defun %s () nil)\n" alpha)
                    (format "(defun %s () nil)\n" beta)))
          (load tmp nil t)
          (when (featurep 'apropos)
            (unload-feature 'apropos t))
          (mevedel-tool-code--xref-definitions
           (lambda (r) (setq result r))
           (list :pattern (format "%s-apropos-.*" prefix) :file_path tmp))
          (should (stringp result))
          (should-not
           (string-match-p
            "Symbol.s function definition is void: apropos-parse-pattern"
            result))
          (should (string-match-p (symbol-name alpha) result)))
      (when (fboundp alpha)
        (fmakunbound alpha))
      (when (fboundp beta)
        (fmakunbound beta))
      (when-let* ((buf (find-buffer-visiting tmp)))
        (kill-buffer buf))
      (delete-file tmp))))


;;
;;; Imenu

(mevedel-deftest mevedel-tool-code--imenu ()
  ,test
  (test)
  :doc "lists symbols in an elisp file"
  (let* ((tmp (make-temp-file "mevedel-test-" nil ".el"))
         (result nil))
    (unwind-protect
        (progn
          (with-temp-file tmp
            (insert ";;; test -*- lexical-binding: t -*-\n"
                    "(defun my-test-alpha () nil)\n"
                    "(defun my-test-beta () nil)\n"
                    "(defvar my-test-gamma 42)\n"))
          (mevedel-tool-code--imenu
           (lambda (r) (setq result r))
           (list :file_path tmp))
          (should (stringp result))
          (should (string-match-p "my-test-alpha" result))
          (should (string-match-p "my-test-beta" result))
          (should (string-match-p "my-test-gamma" result)))
      (when-let* ((buf (find-buffer-visiting tmp)))
        (kill-buffer buf))
      (delete-file tmp)))
  :doc "returns message for empty file"
  (let* ((tmp (make-temp-file "mevedel-test-" nil ".el"))
         (result nil))
    (unwind-protect
        (progn
          (with-temp-file tmp
            (insert ";; empty file\n"))
          (mevedel-tool-code--imenu
           (lambda (r) (setq result r))
           (list :file_path tmp))
          (should (stringp result))
          (should (string-match-p "No.*symbols\\|No imenu\\|Error" result)))
      (when-let* ((buf (find-buffer-visiting tmp)))
        (kill-buffer buf))
      (delete-file tmp)))
  :doc "includes line numbers"
  (let* ((tmp (make-temp-file "mevedel-test-" nil ".el"))
         (result nil))
    (unwind-protect
        (progn
          (with-temp-file tmp
            (insert ";; line 1\n"
                    ";; line 2\n"
                    "(defun my-test-fn-line3 () nil)\n"))
          (mevedel-tool-code--imenu
           (lambda (r) (setq result r))
           (list :file_path tmp))
          (should (stringp result))
          (should (string-match-p ":3:.*my-test-fn-line3" result)))
      (when-let* ((buf (find-buffer-visiting tmp)))
        (kill-buffer buf))
      (delete-file tmp)))
  :doc "errors on non-existent file"
  (should-error
   (mevedel-tool-code--imenu
    #'ignore (list :file_path "/nonexistent/file.el"))
   :type 'error))


;;
;;; Treesitter helpers

(mevedel-deftest mevedel-tool-code--line-column-to-point ()
  ,test
  (test)
  :doc "converts line 1 column 0 to beginning of buffer"
  (with-temp-buffer
    (insert "first line\nsecond line\nthird line\n")
    (should (= (mevedel-tool-code--line-column-to-point 1 0) 1)))
  :doc "converts line 2 column 0 to beginning of second line"
  (with-temp-buffer
    (insert "first line\nsecond line\nthird line\n")
    (let ((pos (mevedel-tool-code--line-column-to-point 2 0)))
      (should (= pos 12))))
  :doc "converts line 2 column 3 correctly"
  (with-temp-buffer
    (insert "first line\nsecond line\nthird line\n")
    (let ((pos (mevedel-tool-code--line-column-to-point 2 3)))
      (should (= pos 15)))))

(mevedel-deftest mevedel-tool-code--treesitter ()
  ,test
  (test)
  :doc "errors on non-existent file"
  (should-error
   (mevedel-tool-code--treesitter
    #'ignore (list :file_path "/nonexistent/file.el"))
   :type 'error))

(provide 'test-mevedel-tool-code)
;;; test-mevedel-tool-code.el ends here
