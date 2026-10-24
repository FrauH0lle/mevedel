;;; test-stubs-collect.el --- Find test stubs missing owner imports -*- lexical-binding: t; -*-

;;; Commentary:
;; Read repository snapshots, never evaluating sources or file locals.  Resolve
;; function owners from definitions and provides, then lint literal function-cell
;; mutations in test libraries.  Input/output JSON paths follow `--'.

;;; Code:

(require 'cl-lib)
(require 'json)

(defun mevedel-stubs--literal (form)
  "Return the literal symbol quoted by FORM, or nil."
  (and (memq (car-safe form) '(quote function))
       (symbolp (cadr form)) (cadr form)))

(defun mevedel-stubs--forms (text)
  "Read TEXT into (LINE . FORM) pairs without evaluating it."
  (with-temp-buffer
    (insert text)
    (emacs-lisp-mode)
    (goto-char (point-min))
    (let (forms)
      (while (progn (forward-comment (point-max)) (not (eobp)))
        (let ((line (line-number-at-pos)))
          (push (cons line (read (current-buffer))) forms)))
      (nreverse forms))))

(defun mevedel-stubs--requires (form features)
  "Add unconditional literal requires in FORM to FEATURES."
  (pcase (car-safe form)
    ('require
     (let ((feature (mevedel-stubs--literal (cadr form))))
       (if (and feature (not (nth 3 form)))
           (cons feature features)
         features)))
    ((or 'progn 'eval-and-compile)
     (dolist (child (cdr form) features)
       (setq features (mevedel-stubs--requires child features))))
    (_ features)))

(defun mevedel-stubs--definitions (form &optional test-library)
  "Return function names defined by FORM, ignoring quoted data.
When TEST-LIBRARY is non-nil, treat defalias as a mutation, not ownership."
  (pcase (car-safe form)
    ((or 'defun 'cl-defun 'defmacro 'cl-defmacro 'defsubst 'cl-defsubst
         'cl-defgeneric 'cl-defmethod)
     (list (cadr form)))
    ('defalias (unless test-library (list (mevedel-stubs--literal (cadr form)))))
    ((or 'quote 'function '\`) nil)
    (_ (let (names)
         (while (consp form)
           (setq names (nconc names (mevedel-stubs--definitions
                                    (pop form) test-library))))
         names))))

(defun mevedel-stubs--fill (form bindings)
  "Substitute literal comma placeholders in FORM from template BINDINGS."
  (cond
   ((not (consp form)) form)
   ((eq (car form) '\,)
    (if-let* ((binding (assq (cadr form) bindings))) (cdr binding) form))
   ((eq (car-safe (car form)) '\,@)
    (append (cdr (assq (cadar form) bindings))
            (mevedel-stubs--fill (cdr form) bindings)))
   (t (cons (mevedel-stubs--fill (car form) bindings)
            (mevedel-stubs--fill (cdr form) bindings)))))

(defun mevedel-stubs--cases (template)
  "Split a literal `mevedel-deftest' TEMPLATE into independent case forms."
  (let ((form (car template)) (vars (cadr template)) (values (cddr template))
        cases)
    (if (not vars)
        (list form)
      (while values
        (when (eq (car values) :doc) (setq values (cddr values)))
        (let (bindings)
          (dolist (var vars) (push (cons var (pop values)) bindings))
          (push (mevedel-stubs--fill form bindings) cases)))
      (nreverse cases))))

(defun mevedel-stubs--collect (sources)
  "Return test stub diagnostics for the repository SOURCES snapshot."
  (let ((definitions (make-hash-table :test #'eq))
        (files nil)
        (results nil))
    (dolist (source sources)
      (let* ((name (alist-get 'name source))
             (forms (mevedel-stubs--forms (alist-get 'text source)))
             (features nil))
        (dolist (entry forms)
          (when (eq (car-safe (cdr entry)) 'provide)
            (push (mevedel-stubs--literal (cadr (cdr entry))) features)))
        (push (cons name forms) files)
        (dolist (entry forms)
          (dolist (symbol (mevedel-stubs--definitions
                           (cdr entry) (string-prefix-p "test/" name)))
            (when symbol
              (dolist (feature (delq nil features))
                (cl-pushnew (cons name feature) (gethash symbol definitions)
                            :test #'equal)))))))
    ;; A test redefining product code is not a second owner of that code.
    (maphash (lambda (symbol owners)
               (let ((libraries (cl-remove-if
                                 (lambda (entry)
                                   (string-prefix-p "test/" (car entry)))
                                 owners)))
                 (when libraries (puthash symbol libraries definitions))))
             definitions)
    (dolist (file (nreverse files))
      (let ((name (car file)) (forms (cdr file)) (top-features nil)
            (diagnostics nil) (owner "<file>") (line 1))
        (when (string-prefix-p "test/" name)
          ;; Deferred test/function bodies run after all top-level imports.
          (dolist (entry forms)
            (setq top-features
                  (mevedel-stubs--requires (cdr entry) top-features)))
          (cl-labels
              ((report
                (symbol features)
                (let ((owners (gethash symbol definitions)))
                  (when (and owners
                             (not (cl-some
                                   (lambda (entry)
                                     (or (equal (car entry) name)
                                         (memq (cdr entry) features)))
                                   owners)))
                    (let ((feature (cdr (car (last owners)))))
                      (push `((line . ,line) (owner . ,owner)
                              (message . ,(format
                                           "stubbing %s requires (require '%s) before installing the stub"
                                           symbol feature)))
                            diagnostics)))))
               (sequence
                (body features)
                (while (consp body)
                  (setq features (walk (pop body) features)))
                features)
               (template
                (form features depth)
                (when (consp form)
                  (pcase (car form)
                    ('\` (template (cadr form) features (1+ depth)))
                    ((or '\, '\,@)
                     (if (= depth 1)
                         (walk (cadr form) features)
                       (template (cadr form) features (1- depth))))
                    (_ (while (consp form)
                         (template (pop form) features depth))))))
               (walk
                (form features)
                (when (consp form)
                  (pcase (car form)
                    ('quote nil)
                    ('\` (template (cadr form) features 1))
                    ('function
                     (when (eq (car-safe (cadr form)) 'lambda)
                       (walk (cadr form) features)))
                    ((or 'defun 'cl-defun 'defmacro 'cl-defmacro
                         'ert-deftest)
                     (let ((previous owner))
                       (setq owner (symbol-name (cadr form)))
                       (sequence (cdddr form) (append top-features features))
                       (setq owner previous)))
                    ('mevedel-deftest
                     (let* ((previous owner)
                            (options (nth 2 form))
                            (cases (mevedel-stubs--cases (cdddr form)))
                            (before (plist-get options :before-each))
                            (after (plist-get options :after-each))
                            (vars (or (plist-get options :vars*)
                                      (plist-get options :vars)))
                            (index 0))
                       (dolist (case cases)
                         (setq owner (format "%s/test%s" (cadr form)
                                             (if (cdr cases)
                                                 (format "@%d" (cl-incf index)) "")))
                         (walk `(let ,vars
                                  ,@(if (cl-every #'listp before) before (list before))
                                  ,case)
                               (append top-features features))
                         ;; Cleanup can run even when setup or the body failed.
                         (sequence (if (cl-every #'listp after) after (list after))
                                   (append top-features features)))
                       (setq owner previous)))
                    ((or 'progn 'eval-and-compile)
                     (setq features (sequence (cdr form) features)))
                    ('require
                     (setq features (mevedel-stubs--requires form features)))
                    ('eval-when-compile nil)
                    ((or 'when 'unless)
                     (sequence (cddr form) (walk (cadr form) features)))
                    ('if
                     (let ((local (walk (cadr form) features)))
                       (walk (nth 2 form) local)
                       (sequence (cdddr form) local)))
                    ('cond
                     (dolist (clause (cdr form)) (sequence clause features)))
                    ((or 'and 'or)
                     (sequence (cdr form) features))
                    ((or 'let 'let*)
                     (let ((local features))
                       (dolist (binding (cadr form))
                         (when (consp binding)
                           (setq local (walk (cadr binding) local))))
                       (setq features (sequence (cddr form) local))))
                    ((or 'condition-case 'condition-case-unless-debug)
                     (walk (nth 2 form) features)
                     (dolist (handler (cdddr form))
                       (sequence (cdr handler) features)))
                    ('unwind-protect
                     (walk (cadr form) features)
                     (sequence (cddr form) features))
                    ((or 'cl-letf 'cl-letf*)
                     (let ((local features))
                       (when (eq (car form) 'cl-letf)
                         (dolist (binding (cadr form))
                           (setq local (walk (cadr binding) local))))
                       (dolist (binding (cadr form))
                         (when (eq (car form) 'cl-letf*)
                           (setq local (walk (cadr binding) local)))
                         (when (eq (car-safe (car-safe binding)) 'symbol-function)
                           (report (mevedel-stubs--literal (cadar binding)) local)))
                       (setq features (sequence (cddr form) local))))
                    ((or 'fset 'defalias)
                     (setq features (sequence (cddr form) features))
                     (report (mevedel-stubs--literal (cadr form)) features))
                    ((or 'setf 'cl-setf)
                     (let ((pairs (cdr form)))
                       (while (consp pairs)
                         (let ((place (pop pairs)) (value (pop pairs)))
                           (setq features (walk value features))
                           (when (eq (car-safe place) 'symbol-function)
                             (report (mevedel-stubs--literal (cadr place)) features))))))
                    (_ (sequence (cdr form) features))))
                features))
            (let (features)
              (dolist (entry forms)
                (setq line (car entry)
                      features (walk (cdr entry) features)))))
          (push (cons name (vconcat (nreverse diagnostics))) results))))
    results))

(let* ((args (delete "--" command-line-args-left))
       (json-array-type 'list)
       (sources (json-read-file (car args))))
  (with-temp-file (cadr args)
    (insert (json-encode (mevedel-stubs--collect sources))))
  (setq command-line-args-left nil))

;;; test-stubs-collect.el ends here
