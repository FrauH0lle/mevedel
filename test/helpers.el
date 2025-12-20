;;; tests/helpers.el -- Helper functions for tests -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'mevedel-instructions)


;;
;;; Test macro

;; Adapted from https://github.com/radian-software/straight.el

(defmacro mevedel-test--template (template &optional vars &rest bindings)
  "Generate multiple test templates.

TEMPLATE is an implicitly backquoted form that serves as the base
structure for the generated tests. It can contain placeholders
that will be filled with values from the bindings.

VARS is an optional list of symbols that define the destructuring
pattern for BINDINGS. Each symbol in VARS will be bound to
corresponding values from BINDINGS.

BINDINGS is a list of values that will be used to fill the
template. The number of bindings must be evenly divisible by the
number of VARS. BINDINGS can include :doc keywords followed by
documentation strings for individual test cases.

The macro returns a list of filled templates, with each template
having its placeholders replaced by the corresponding values from
BINDINGS. Each template is paired with its documentation string.

Implementation Details:
- If no VARS or BINDINGS are provided, returns just the TEMPLATE
- Normalizes BINDINGS to ensure each test case has a docstring
- Validates that BINDINGS match VARS structure
- Processes BINDINGS into environment variables
- Generates templates with proper variable bindings

Example:
  (mevedel-test--template
    (should (equal ,input ,expected))
  (input expected)
  1 1
  :doc \"First docstring\"
  2 4
  :doc \"Second docstring\"
  3 9)

This would generate:
  ((\"\"
  (should
   (equal 1 1)))
 (\"First docstring\"
  (should
   (equal 2 4)))
 (\"Second docstring\"
  (should
   (equal 3 9))))"
  (declare (indent 1) (debug t))
  ;; If no vars or bindings provided, return just the template
  (if (or (null vars) (null bindings))
      (list `("" ,template))
    ;; Ensure that each binding is preceeded by :doc "DOCSTRING" (or empty) and
    ;; assing them to docstrings and bindings
    (let ((normalized-bindings (mevedel-test--normalize-cases vars bindings))
          docstrings bindings)
      (while normalized-bindings
        (let ((item (pop normalized-bindings)))
          (if (and (keywordp item) (eq item :doc))
              (push (pop normalized-bindings) docstrings)
            (push item bindings))))
      ;; Restore order
      (setq bindings (nreverse bindings)
            docstrings (nreverse docstrings))
      ;; Check if bindings are evenly divisible by number of vars
      (let ((unbound (mod (length bindings) (length vars))))
        ;; Error if bindings don't match vars
        (unless (zerop unbound)
          (error "Uneven binding list: %S" (last bindings unbound)))
        ;; Process the bindings
        (let ((body nil)
              (bindings
               (eval
                `(cl-loop for ,vars on ',bindings
                          by (lambda (l) (nthcdr ,(length vars) l))
                          collect
                          (apply #'append
                                 (cl-mapcar #'list ',vars (list ,@vars)))))))
          ;; Iterate through bindings and generate templates
          (let ((cases (dolist (env bindings (mapcar (lambda (it) (eval it t))
                                                     (nreverse body)))
                         ;; Check if environment has even number of elements
                         (let ((even (mod (length env) 2)))
                           (unless even (error "Uneven binding list: %S" env)))
                         ;; Build the let bindings
                         (let (e)
                           (cl-loop for (var val) on env by #'cddr
                                    do (push (list var `(quote ,val)) e))
                           ;; Generate the template with bindings
                           (push `(let* ,(nreverse e) (backquote ,template)) body)))))
            ;; Combine dostrings and test cases
            (cl-loop for case in cases
                     for doc in docstrings
                     collect `(,doc . ,(list case)))))))))

(defun mevedel-test--normalize-cases (vars bindings)
  "Ensure each test case group has a docstring.
VARS is the list of variable names. BINDINGS is the list of test
inputs/outputs and optional docstrings."
  (let ((i 0)
        item
        result)
    (while bindings
      (setq item (pop bindings))
      ;; Test if the current item is at the start of a test case (number of vars +
      ;; :doc + docstring)
      (if (zerop (mod i (+ (length vars) 2)))
          ;; Test if the current item is the keyword :doc
          (if (and (keywordp item) (eq item :doc))
              ;; Add current and next item to result
              (let ((next (pop bindings)))
                (push item result)
                (push next result)
                ;; increment index +1
                (cl-incf i))
            ;; Else add :doc keyword
            (push :doc result)
            ;; Add empty docstring
            (push "" result)
            ;; Add current item
            (push item result)
            ;; Increment index by 2
            (cl-incf i 2))
        (push item result))
      (cl-incf i))
    (nreverse result)))

(cl-defmacro mevedel-deftest (object
                            (&key before-each after-each expected-result
                                  doc tags vars vars* &allow-other-keys)
                            &rest template)
  "Define one or more ERT tests for OBJECT with TEMPLATE.

OBJECT is the symbol being tested. It can be a function, macro,
or other symbol.

KEYWORD ARGUMENTS:
  :before-each - Form(s) to run before each test case
  :after-each  - Form(s) to run after each test case
  :expected-result - Expected result type (:passed, :failed, etc)
  :doc         - Documentation string for the test
  :tags        - List of tags to apply to the test
  :vars        - Variables to bind using `let'
  :vars*       - Variables to bind using `let*'

The `let'/`let*' binding introduced via :vars and :vars* will
encompass the whole test body, including the code from
:before-each and :after-each.

TEMPLATE:
TEMPLATE is a list of forms that will be expanded into test cases
using `mevedel-test--template'. There are several patterns:

1. SIMPLE TEST (no variables):
   Just provide a test body. Use ,test placeholder and (test) binding.

2. PARAMETERIZED TEST (with variables):
   Provide a template form with placeholders, variable names, and values.

3. MULTIPLE TEST CASES:
   Prefix test cases with :doc to provide individual descriptions.

AUTOMATIC TAGS:
The macro automatically adds tags based on OBJECT:
  - The object's name itself as a tag
  - \\='private if the name contains \\='--', otherwise \\='public
  - \\='macro if the object is a macro

EXAMPLES:

Example 1: Simple test (no parameters)
  (mevedel-deftest zenit-plist-map
    (:doc \"`zenit-plist-map' maps fn to plist\")
    (let ((plist \\='(:a 1 :b 2 :c 3)))
      (zenit-plist-map (lambda (key val) (1+ val)) plist)
      (should (equal \\='(:a 2 :b 3 :c 4) plist))))

Example 2: Simple test with placeholder
  (mevedel-deftest file-exists-p!
    (:doc \"`file-exists-p!' tests if one or more files exist.\")
    ,test
    (test)
    (should (file-exists-p! (file!)))
    (let ((test-file (mevedel-test-make-temp-file)))
      (should (equal (expand-file-name test-file) (file-exists-p! test-file)))
      (delete-file test-file)))

Example 3: Parameterized test with multiple cases
  (mevedel-deftest zenit-path
    (:doc \"`zenit-path' returns a path from segments\")
    (should (equal ,out (zenit-path ,@in)))
    (in out)
    (\"/tmp\" \"foo\" \"bar.txt\") \"/tmp/foo/bar.txt\"
    (\"foo\") (expand-file-name \"foo\")
    (\"/tmp\" \"foo\" nil \"bar.txt\") \"/tmp/foo/bar.txt\")

Example 4: Multiple test cases with individual docstrings
  (mevedel-deftest zenit-surrounded-p
    (:vars ((test-buffer (get-buffer-create \"test-buffer\")))
     :before-each
     (with-current-buffer test-buffer
       (erase-buffer)
       (emacs-lisp-mode))
     :after-each
     (kill-buffer test-buffer))
    ,test
    (test)
    :doc \"`zenit-surrounded-p' returns t when surrounded\"
    (with-current-buffer test-buffer
      (insert \"foo {bar} baz\")
      (goto-char 7)
      (should (zenit-surrounded-p \\='(:beg 5 :end 8 :op \"{\" :cl \"}\"))))

    :doc \"`zenit-surrounded-p' returns nil when not surrounded\"
    (with-current-buffer test-buffer
      (insert \"foo {bar} baz\")
      (goto-char 4)
      (should-not (zenit-surrounded-p \\='(:beg 5 :end 8 :op \"{\" :cl \"}\")))))

Example 5: Parameterized test with different assertions
  (mevedel-deftest zenit-file-cookie-p
    (:doc \"`zenit-file-cookie-p' returns the evaluated result\")
    (let ((test-file (mevedel-test-make-temp-file nil \".el\" ,fcookie)))
      (,assert (zenit-file-cookie-p test-file ,tcookie ,null-value))
      (delete-file test-file))
    (assert fcookie tcookie null-value)
    should \";;;###if (equal \\\"test\\\" \\\"test\\\")\" \"if\" nil
    should \";;;###foo-test (equal \\\"test\\\" \\\"test\\\")\" \"foo-test\" nil
    should \";;;###foo-test (equal \\\"test\\\" \\\"test\\\")\" \"if\" t
    should-not \";;;###foo-test (equal \\\"test\\\" \\\"test\\\")\" \"if\" nil)

Example 6: Test with setup/teardown and vars
  (mevedel-deftest zenit-syntax-ppss
    (:vars ((test-buffer (get-buffer-create \"test-buffer\")))
     :before-each
     (with-current-buffer test-buffer
       (erase-buffer)
       (emacs-lisp-mode)
       (setq zenit--sppss-memo-last-point nil
             zenit--sppss-memo-last-result nil))
     :after-each
     (kill-buffer test-buffer)
     :doc \"`zenit-syntax-ppss' parses syntax and caches state\")
    ,test
    (test)
    (with-current-buffer test-buffer
      (insert \"(hello \\\"world\\\") ; comment\")
      (goto-char 1)
      (let ((result1 (zenit-syntax-ppss 8)))
        (should (eq result1 (zenit-syntax-ppss 8))))))

PATTERN DETAILS:

For parameterized tests, the template form uses backquote syntax
where commas unquote variable values:
  - ,var    -> unquotes single variable
  - ,@var   -> unquotes and splices list variable
  - ,test   -> special placeholder for simple tests

The variable binding line lists variables that will be bound:
  (var1 var2 var3)

Then provide values in groups matching the variable count:
  value1 value2 value3     ; First test case
  value4 value5 value6     ; Second test case

Optionally prefix each case with :doc \"description\" for individual
test documentation.

See also:
  `mevedel-test--template' - Template expansion function
  `mevedel-test--normalize-cases' - Docstring normalization"
  (declare (indent defun) (debug t))
  ;; Initialize test counter and automatic tags
  (let ((counter 0)
        (autotags
         (delq nil
               (list
                object
                (if (string-match-p "--" (symbol-name object))
                    'private 'public)
                (if (macrop object) 'macro))))
        ;; Generate tests from template
        (tests (when template
                 (macroexpand `(mevedel-test--template ,@template)))))
    ;; Combine automatic and manual tags
    (setq tags (append autotags tags))
    ;; Generate the test forms
    `(progn
       ,@(mapcar
          (lambda (test)
            (let ((test-body
                   `(,@(when before-each
                         (if (cl-every #'listp before-each)
                             before-each
                           (list before-each)))
                     ,@(cdr test)
                     ,@(when after-each
                         (if (cl-every #'listp after-each)
                             after-each
                           (list after-each))))))
              `(ert-deftest
                   ,(intern (concat
                             (format "%s/test" object)
                             (when (> (length tests) 1)
                               (format "@%d" (cl-incf counter)))))
                   ()
                 ,(or (and (stringp (car test))
                           (not (string-empty-p (car test)))
                           (car test))
                      doc
                      (when (fboundp object) (documentation object)))
                 ,@(when tags `(:tags ',tags))
                 ,@(when expected-result `(:expected-result ,expected-result))
                 ,@(cond
                   (vars*  `((let* ,vars* ,@test-body)))
                   (vars   `((let ,vars ,@test-body)))
                   (t      test-body)))))
          tests))))

(defun mevedel-test-enable-fontlocking ()
  "Enable fontlocking for `mevedel-deftest'."
  (font-lock-add-keywords
   nil
   '(("(\\(\\<mevedel-deftest\\)\\>\\s *\\(\\(?:\\sw\\|\\s_\\)+\\)?"
      (1 font-lock-keyword-face nil t)
      (2 font-lock-function-name-face nil t)))))
;; Activate the font-locking
(mevedel-test-enable-fontlocking)


;;
;;; Test Functions

(defun mevedel-test-same-items-p (expected actual &rest cl-keys)
  "Verify that EXPECTED and ACTUAL have the same items.
The order of items does not matter. Returns t if lists match, nil
otherwise.
CL-KEYS as in `cl-set-difference'.
\nKeywords supported:  :test :test-not :key
\n(fn EXPECTED ACTUAL [KEYWORD VALUE]...)"
  (and (null (apply #'cl-set-difference expected actual cl-keys))
       (null (apply #'cl-set-difference actual expected cl-keys))))

(defun mevedel-test-contains-items-p (expected actual &rest cl-keys)
  "Verify that ACTUAL contains EXPECTED items.
The order of items does not matter. Returns t if lists match, nil
otherwise.
CL-KEYS as in `cl-set-difference'.
\nKeywords supported: :test :test-not :key
\n(fn EXPECTED ACTUAL [KEYWORD VALUE]...)"
  (null (apply #'cl-set-difference expected actual cl-keys)))

(provide 'helpers)
;;; helpers.el ends here
