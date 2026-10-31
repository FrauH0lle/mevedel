;;; test-inventory-collect.el --- Read literal test names and strings -*- lexical-binding: t; -*-

;;; Commentary:
;; Read test snapshots without loading helpers, expanding arbitrary macros, or
;; evaluating code.  Input and output JSON paths follow `--'.

;;; Code:

(require 'cl-lib)
(require 'json)

(defun mevedel-test-inventory--collect (text)
  "Return literal test names, strings and fixture paths read from TEXT."
  (let (tests strings paths globals)
    (cl-labels
        ((path
          (form environment)
          ;; Interpret only literal path constructors, never arbitrary Lisp.
          ;; Unknown roots contribute no prefix; a fixture component must
          ;; still be present before the result becomes a fixture reference.
          (cond
           ((stringp form) form)
           ((symbolp form) (cdr (or (assq form environment) (assq form globals))))
           ((consp form)
            (pcase (car form)
              ((or 'file-name-concat 'expand-file-name)
               (let* ((args (if (eq (car form) 'expand-file-name)
                                (list (nth 2 form) (nth 1 form))
                              (cdr form)))
                      (parts (mapcar (lambda (arg) (path arg environment)) args)))
                 (while (and parts (null (car parts))) (pop parts))
                 (when (and parts (cl-every #'stringp parts))
                   (replace-regexp-in-string "/+" "/" (mapconcat #'identity parts "/")))))
              ('file-name-directory
               (let ((value (path (cadr form) environment)))
                 (when value (file-name-directory value))))))))
         (walk
          (form line quoted environment)
          (cond
           ((stringp form) (push `((value . ,form) (line . ,line)) strings))
           ((vectorp form) (mapc (lambda (item) (walk item line t environment)) form))
           ((consp form)
            (when (memq (car form) '(file-name-concat expand-file-name))
              (let ((value (path form environment)))
                (when (and value (string-match-p "\\(?:\\`\\|/\\)fixtures/" value))
                  (push `((value . ,value) (line . ,line)) paths))))
            (when (memq (car form) '(defconst defvar))
              (push (cons (cadr form) (path (nth 2 form) environment)) globals))
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
              (if (memq (car form) '(let let*))
                  (let ((inner environment))
                    (dolist (binding (cadr form))
                      (let* ((name (if (consp binding) (car binding) binding))
                             (init (and (consp binding) (cadr binding)))
                             (scope (if (eq (car form) 'let*) inner environment)))
                        (walk init line quoted scope)
                        (push (cons name (path init scope)) inner)))
                    (dolist (body (cddr form)) (walk body line quoted inner)))
                (while (consp form) (walk (pop form) line quoted environment))))))))
      (with-temp-buffer
        (insert text)
        (emacs-lisp-mode)
        (goto-char (point-min))
        (while (progn (forward-comment (point-max)) (not (eobp)))
          (let ((line (line-number-at-pos)))
            (walk (read (current-buffer)) line nil nil)))))
    `((tests . ,(vconcat (nreverse tests)))
      (strings . ,(vconcat (nreverse strings)))
      (paths . ,(vconcat (nreverse paths))))))

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
