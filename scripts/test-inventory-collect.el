;;; test-inventory-collect.el --- Read literal test names and strings -*- lexical-binding: t; -*-

;;; Commentary:
;; Read test snapshots without loading helpers, expanding arbitrary macros, or
;; evaluating code.  Input and output JSON paths follow `--'.

;;; Code:

(require 'cl-lib)
(require 'json)

(defun mevedel-test-inventory--collect (text)
  "Return literal test names and strings read from TEXT."
  (let (tests strings)
    (cl-labels
        ((walk
          (form line quoted)
          (cond
           ((stringp form) (push `((value . ,form) (line . ,line)) strings))
           ((vectorp form) (mapc (lambda (item) (walk item line t)) form))
           ((consp form)
            (unless quoted
              (pcase (car form)
                ('ert-deftest
                 (when (symbolp (cadr form))
                   (push `((name . ,(symbol-name (cadr form))) (line . ,line)) tests)))
                ('mevedel-deftest
                 (let* ((template (cdddr form))
                        (variables (cadr template))
                        (values (cddr template))
                        (count (if (or (null template) variables) 0 1)))
                   (when variables
                     (while values
                       (when (eq (car values) :doc) (setq values (cddr values)))
                       (setq count (1+ count)
                             values (nthcdr (length variables) values))))
                   (dotimes (index count)
                     (push `((name . ,(format "%s/test%s" (cadr form)
                                             (if (> count 1) (format "@%d" (1+ index)) "")))
                             (line . ,line)) tests))))))
            ;; Quoted data cannot define tests; strings still describe fixture
            ;; paths (including command strings passed to a child Emacs).
            (let ((quoted (or quoted (memq (car form) '(quote function \`)))))
              (while (consp form) (walk (pop form) line quoted)))))))
      (with-temp-buffer
        (insert text)
        (emacs-lisp-mode)
        (goto-char (point-min))
        (while (progn (forward-comment (point-max)) (not (eobp)))
          (let ((line (line-number-at-pos)))
            (walk (read (current-buffer)) line nil)))))
    `((tests . ,(vconcat (nreverse tests)))
      (strings . ,(vconcat (nreverse strings))))))

(let* ((args (delete "--" command-line-args-left))
       (json-array-type 'list)
       (sources (json-read-file (car args))))
  (with-temp-file (cadr args)
    (insert (json-encode
             (mapcar (lambda (source)
                       (cons (alist-get 'name source)
                             (mevedel-test-inventory--collect (alist-get 'text source))))
                     sources))))
  (setq command-line-args-left nil))

;;; test-inventory-collect.el ends here
