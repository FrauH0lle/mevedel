;;; checkdoc-collect.el --- Collect batch checkdoc diagnostics -*- lexical-binding: t; -*-

;;; Commentary:
;; Loaded by check-standards.py in a clean Emacs, without evaluating file locals
;; or package sources.  Input and output are JSON files passed after `--'.

;;; Code:

(require 'checkdoc)
(require 'json)
(require 'cl-lib)

(defun mevedel-checkdoc--collect (file)
  "Return checkdoc diagnostics for FILE without loading its code."
  (with-temp-buffer
    (insert-file-contents file)
    (setq buffer-file-name file)
    (emacs-lisp-mode)
    (let ((diagnostics nil)
          (checkdoc-spellcheck-documentation-flag nil))
      (cl-letf (((symbol-function 'checkdoc-error)
                 (lambda (position text)
                   (save-match-data
                     (save-excursion
                       (goto-char (or position (point-min)))
                       (let ((line (line-number-at-pos))
                             (owner (or (lisp-current-defun-name) "<file>")))
                         ;; Pair messages with their top-level definition, not
                         ;; volatile line numbers or a tree-wide warning count.
                         (push `((line . ,line) (owner . ,owner)
                                 (message . ,text))
                               diagnostics)))))))
        (checkdoc-current-buffer t))
      (vconcat (nreverse diagnostics)))))

(let* ((args (delete "--" command-line-args-left))
       (files (json-read-file (car args)))
       (results (mapcar #'mevedel-checkdoc--collect files)))
  (with-temp-file (cadr args)
    (insert (json-encode (vconcat results))))
  (setq command-line-args-left nil))

;;; checkdoc-collect.el ends here
