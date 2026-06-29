;;; test-mevedel-tool-code.el --- Tests for code exploration tools -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'cl-lib)
(require 'gptel)
(require 'xref)
(require 'mevedel-tool-registry)
(require 'mevedel-tool-code)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name
               load-file-name
               byte-compile-current-file))
          "helpers"))

(defvar mevedel-tool-code-test-open-state nil
  "State captured while opening a test xref location.")
(defvar mevedel-tool-code-test-xref-backend-state nil
  "State captured while invoking the test xref backend.")

(cl-defstruct (mevedel-tool-code-test-location
               (:constructor mevedel-tool-code-test-location-create (file)))
  "Test xref location carrying a source FILE."
  file)

(cl-defmethod xref-location-group ((location mevedel-tool-code-test-location))
  "Return the xref group for test LOCATION."
  (mevedel-tool-code-test-location-file location))

(cl-defmethod xref-location-line ((_location mevedel-tool-code-test-location))
  "Return nil because test locations do not track line numbers."
  nil)

(cl-defmethod xref-location-marker ((location mevedel-tool-code-test-location))
  "Return a marker for test LOCATION while capturing open state."
  (setq mevedel-tool-code-test-open-state
        (list enable-local-variables find-file-hook hack-local-variables-hook))
  (with-current-buffer
      (find-file-noselect (mevedel-tool-code-test-location-file location))
    (goto-char (point-min))
    (forward-line 1)
    (point-marker)))

(cl-defmethod xref-backend-references
  ((_backend (eql mevedel-tool-code-test-backend)) _identifier)
  "Capture backend state and return no references."
  (setq mevedel-tool-code-test-xref-backend-state
        (list enable-local-variables find-file-hook hack-local-variables-hook))
  nil)

(cl-defmethod xref-backend-apropos
  ((_backend (eql mevedel-tool-code-test-backend)) _pattern)
  "Capture backend state and return no apropos matches."
  (setq mevedel-tool-code-test-xref-backend-state
        (list enable-local-variables find-file-hook hack-local-variables-hook))
  nil)


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
;;; Buffer lifecycle

(mevedel-deftest mevedel-tool-code--with-file-buffer ()
  ,test
  (test)

  :doc "kills buffers it opens"
  (let ((tmp (make-temp-file "mevedel-test-" nil ".el")))
    (unwind-protect
        (progn
          (with-temp-file tmp
            (insert "(defun buffer-cleanup-test () nil)\n"))
          (mevedel-tool-code--with-file-buffer
           tmp
           (lambda (_file-path _full-path target-buffer)
             (should (buffer-live-p target-buffer))))
          (should-not (find-buffer-visiting tmp)))
      (when-let* ((buf (find-buffer-visiting tmp)))
        (kill-buffer buf))
      (delete-file tmp)))

  :doc "keeps buffers that were already open"
  (let (buffer
        (tmp (make-temp-file "mevedel-test-" nil ".el")))
    (unwind-protect
        (progn
          (with-temp-file tmp
            (insert "(defun buffer-preserve-test () nil)\n"))
          (setq buffer (find-file-noselect tmp))
          (mevedel-tool-code--with-file-buffer
           tmp
           (lambda (_file-path _full-path target-buffer)
             (should (eq target-buffer buffer))))
          (should (buffer-live-p buffer))
          (should (eq (find-buffer-visiting tmp) buffer)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (delete-file tmp)))

  :doc "keeps buffers it opens when they become modified"
  (let ((tmp (make-temp-file "mevedel-test-" nil ".el")))
    (unwind-protect
        (progn
          (with-temp-file tmp
            (insert "(defun buffer-modified-test () nil)\n"))
          (mevedel-tool-code--with-file-buffer
           tmp
           (lambda (_file-path _full-path target-buffer)
             (with-current-buffer target-buffer
               (set-buffer-modified-p t))))
          (should (find-buffer-visiting tmp)))
      (when-let* ((buf (find-buffer-visiting tmp)))
        (with-current-buffer buf
          (set-buffer-modified-p nil))
        (kill-buffer buf))
      (delete-file tmp)))

  :doc "opens buffers without prompting for unsafe local variables"
  (let ((tmp (make-temp-file "mevedel-test-" nil ".el"))
        (enable-local-variables t)
        (find-file-hook '(sentinel-find-file-hook))
        (hack-local-variables-hook '(sentinel-local-variables-hook))
        captured opened)
    (unwind-protect
        (progn
          (with-temp-file tmp
            (insert "(defun buffer-quiet-open-test () nil)\n"))
          (cl-letf (((symbol-function 'find-file-noselect)
                     (lambda (file &rest _args)
                       (setq captured
                             (list enable-local-variables
                                   find-file-hook
                                   hack-local-variables-hook)
                             opened (generate-new-buffer " *mevedel-test*"))
                       (with-current-buffer opened
                         (setq buffer-file-name file))
                       opened)))
            (mevedel-tool-code--with-file-buffer
             tmp
             (lambda (_file-path _full-path target-buffer)
               (should (eq target-buffer opened)))))
          (should (equal '(:safe nil nil) captured))
          (should-not (buffer-live-p opened)))
      (when (buffer-live-p opened)
        (kill-buffer opened))
      (delete-file tmp))))

(mevedel-deftest mevedel-tool-code--xref-location-line ()
  ,test
  (test)

  :doc "runs direct xref line resolution without prompting for unsafe local variables"
  (let ((enable-local-variables t)
        (find-file-hook '(sentinel-find-file-hook))
        (hack-local-variables-hook '(sentinel-local-variables-hook))
        captured)
    (cl-letf (((symbol-function 'xref-location-line)
               (lambda (_location)
                 (setq captured
                       (list enable-local-variables
                             find-file-hook
                             hack-local-variables-hook))
                 42)))
      (should (= 42 (mevedel-tool-code--xref-location-line 'dummy)))
      (should (equal '(:safe nil nil) captured)))))

(mevedel-deftest mevedel-tool-code--format-xref-items ()
  ,test
  (test)

  :doc "formats file locations without visiting their files"
  (let* ((tmp (make-temp-file "mevedel-test-" nil ".el"))
         (location (xref-make-file-location tmp 2 0))
         (item (xref-make "matching line" location)))
    (unwind-protect
        (progn
          (with-temp-file tmp
            (insert "first line\nmatching line\n"))
          (should (equal (format "%s:2: matching line" tmp)
                         (mevedel-tool-code--format-xref-items
                          (list item))))
          (should-not (find-buffer-visiting tmp)))
      (when-let* ((buf (find-buffer-visiting tmp)))
        (kill-buffer buf))
      (delete-file tmp)))

  :doc "cleans fallback marker buffers opened during formatting"
  (let* ((tmp (make-temp-file "mevedel-test-" nil ".el"))
         (location (mevedel-tool-code-test-location-create tmp))
         (item (xref-make "matching line" location)))
    (unwind-protect
        (progn
          (with-temp-file tmp
            (insert "first line\nmatching line\n"))
          (should (equal (format "%s:2: matching line" tmp)
                         (mevedel-tool-code--format-xref-items
                          (list item))))
          (should-not (find-buffer-visiting tmp)))
      (when-let* ((buf (find-buffer-visiting tmp)))
        (kill-buffer buf))
      (delete-file tmp)))

  :doc "resolves fallback marker locations without prompting for unsafe local variables"
  (let* ((tmp (make-temp-file "mevedel-test-" nil ".el"))
         (location (mevedel-tool-code-test-location-create tmp))
         (item (xref-make "matching line" location))
         (enable-local-variables t)
         (find-file-hook '(sentinel-find-file-hook))
         (hack-local-variables-hook '(sentinel-local-variables-hook))
         mevedel-tool-code-test-open-state)
    (unwind-protect
        (progn
          (with-temp-file tmp
            (insert "first line\nmatching line\n"))
          (mevedel-tool-code--format-xref-items (list item))
          (should (equal '(:safe nil nil)
                         mevedel-tool-code-test-open-state)))
      (when-let* ((buf (find-buffer-visiting tmp)))
        (kill-buffer buf))
      (delete-file tmp)))

  :doc "formats xref items without prompting for unsafe local variables"
  (let ((item (xref-make "matching line" 'dummy-location))
        (enable-local-variables t)
        (find-file-hook '(sentinel-find-file-hook))
        (hack-local-variables-hook '(sentinel-local-variables-hook))
        group-state line-state summary-state)
    (cl-letf (((symbol-function 'xref-location-group)
               (lambda (_location)
                 (setq group-state
                       (list enable-local-variables
                             find-file-hook
                             hack-local-variables-hook))
                 "dummy.el"))
              ((symbol-function 'xref-location-line)
               (lambda (_location)
                 (setq line-state
                       (list enable-local-variables
                             find-file-hook
                             hack-local-variables-hook))
                 7))
              ((symbol-function 'xref-item-summary)
               (lambda (_item)
                 (setq summary-state
                       (list enable-local-variables
                             find-file-hook
                             hack-local-variables-hook))
                 "matching line")))
      (should (equal "dummy.el:7: matching line"
                     (mevedel-tool-code--format-xref-items (list item))))
      (should (equal '(:safe nil nil) group-state))
      (should (equal '(:safe nil nil) line-state))
      (should (equal '(:safe nil nil) summary-state)))))


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
          (should (string-match-p "my-test-fn-12345" result))
          (should-not (find-buffer-visiting tmp)))
      (when-let* ((buf (find-buffer-visiting tmp)))
        (kill-buffer buf))
      (delete-file tmp)))
  :doc "runs xref backend reference lookup without prompting for unsafe local variables"
  (let* ((tmp (make-temp-file "mevedel-test-" nil ".el"))
         (enable-local-variables t)
         (find-file-hook '(sentinel-find-file-hook))
         (hack-local-variables-hook '(sentinel-local-variables-hook))
         mevedel-tool-code-test-xref-backend-state
         xref-find-backend-state
         result)
    (unwind-protect
        (progn
          (with-temp-file tmp
            (insert "(defun xref-backend-quiet-test () nil)\n"))
          (cl-letf (((symbol-function 'xref-find-backend)
                     (lambda ()
                       (setq xref-find-backend-state
                             (list enable-local-variables
                                   find-file-hook
                                   hack-local-variables-hook))
                       'mevedel-tool-code-test-backend)))
            (mevedel-tool-code--xref-references
             (lambda (r) (setq result r))
             (list :identifier "xref-backend-quiet-test" :file_path tmp)))
          (should (string-match-p "No references found" result))
          (should (equal '(:safe nil nil) xref-find-backend-state))
          (should (equal '(:safe nil nil)
                         mevedel-tool-code-test-xref-backend-state)))
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
  :doc "runs xref backend apropos lookup without prompting for unsafe local variables"
  (let* ((tmp (make-temp-file "mevedel-test-" nil ".el"))
         (enable-local-variables t)
         (find-file-hook '(sentinel-find-file-hook))
         (hack-local-variables-hook '(sentinel-local-variables-hook))
         mevedel-tool-code-test-xref-backend-state
         xref-find-backend-state
         result)
    (unwind-protect
        (progn
          (with-temp-file tmp
            (insert "(defun xref-backend-quiet-test () nil)\n"))
          (cl-letf (((symbol-function 'xref-find-backend)
                     (lambda ()
                       (setq xref-find-backend-state
                             (list enable-local-variables
                                   find-file-hook
                                   hack-local-variables-hook))
                       'mevedel-tool-code-test-backend)))
            (mevedel-tool-code--xref-definitions
             (lambda (r) (setq result r))
             (list :pattern "xref-backend-quiet-test" :file_path tmp)))
          (should (string-match-p "No symbols found" result))
          (should (equal '(:safe nil nil) xref-find-backend-state))
          (should (equal '(:safe nil nil)
                         mevedel-tool-code-test-xref-backend-state)))
      (when-let* ((buf (find-buffer-visiting tmp)))
        (kill-buffer buf))
      (delete-file tmp)))
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
          (should (string-match-p "No symbols found\\|No xref\\|No tags\\|Error" result))
          (should-not (find-buffer-visiting tmp)))
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
          (should (string-match-p (symbol-name alpha) result))
          (should-not (find-buffer-visiting tmp)))
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
          (should (string-match-p "my-test-gamma" result))
          (should-not (find-buffer-visiting tmp)))
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
