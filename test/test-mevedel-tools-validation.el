;;; test-mevedel-tools-validation.el --- Tests for parameter validation -*- lexical-binding: t -*-

;;; Commentary:

;; Tests for the `mevedel-tools--validate-params' macro

;;; Code:

(require 'mevedel)
(load (file-name-concat (file-name-directory (or buffer-file-name load-file-name)) "helpers"))

(mevedel-deftest mevedel-tools--validate-params ()
  ,test
  (test)
  :doc "basic predicate validation:
`mevedel-tools--validate-params' accepts valid string"
  (let ((fn (lambda ()
              (let ((name "test"))
                (mevedel-tools--validate-params
                 nil nil
                 (name stringp))
                t))))
    (should (equal t (funcall fn))))
  :doc "basic predicate validation:
`mevedel-tools--validate-params' rejects non-string"
  (let ((fn (lambda ()
              (let ((name 123))
                (mevedel-tools--validate-params
                 nil nil
                 (name stringp))
                t))))
    (should-error (funcall fn)))
  :doc "basic predicate validation:
`mevedel-tools--validate-params' accepts valid integer"
  (let ((fn (lambda ()
              (let ((count 42))
                (mevedel-tools--validate-params
                 nil nil
                 (count integerp))
                t))))
    (should (equal t (funcall fn))))
  :doc "basic predicate validation:
`mevedel-tools--validate-params' rejects non-integer"
  (let ((fn (lambda ()
              (let ((count "42"))
                (mevedel-tools--validate-params
                 nil nil
                 (count integerp))
                t))))
    (should-error (funcall fn)))
  :doc "booleanp special handling:
`mevedel-tools--validate-params' accepts t as boolean"
  (let ((fn (lambda ()
              (let ((enabled t))
                (mevedel-tools--validate-params
                 nil nil
                 (enabled booleanp))
                t))))
    (should (equal t (funcall fn))))
  :doc "booleanp special handling:
`mevedel-tools--validate-params' accepts :json-false as boolean"
  (let ((fn (lambda ()
              (let ((enabled :json-false))
                (mevedel-tools--validate-params
                 nil nil
                 (enabled booleanp))
                t))))
    (should (equal t (funcall fn))))
  :doc "booleanp special handling:
`mevedel-tools--validate-params' rejects non-boolean values"
  (let ((fn (lambda ()
              (let ((enabled "yes"))
                (mevedel-tools--validate-params
                 nil nil
                 (enabled booleanp))
                t))))
    (should-error (funcall fn)))
  :doc "custom type names:
`mevedel-tools--validate-params' reports array instead of vector in error"
  (let ((fn (lambda ()
              (let ((items "not-an-array"))
                (mevedel-tools--validate-params
                 nil nil
                 (items (vectorp . "array")))
                nil))))
    (should-error (funcall fn)))
  :doc "custom type names:
`mevedel-tools--validate-params' accepts valid vector with array type name"
  (let ((fn (lambda ()
              (let ((items [1 2 3]))
                (mevedel-tools--validate-params
                 nil nil
                 (items (vectorp . "array")))
                t))))
    (should (equal t (funcall fn))))
  :doc "lambda validators:
`mevedel-tools--validate-params' accepts value passing lambda validation"
  (let ((fn (lambda ()
              (let ((score 85))
                (mevedel-tools--validate-params
                 nil nil
                 (score (lambda (x) (and (numberp x) (> x 0)))))
                t))))
    (should (equal t (funcall fn))))
  :doc "lambda validators:
`mevedel-tools--validate-params' rejects value failing lambda validation"
  (let ((fn (lambda ()
              (let ((score -10))
                (mevedel-tools--validate-params
                 nil nil
                 (score (lambda (x) (and (numberp x) (> x 0)))))
                t))))
    (should-error (funcall fn)))
  :doc "lambda validators:
`mevedel-tools--validate-params' uses custom type name with lambda"
  (let ((fn (lambda ()
              (let ((percent 150))
                (mevedel-tools--validate-params
                 nil nil
                 (percent ((lambda (x) (and (numberp x) (>= x 0) (<= x 100))) . "percentage (0-100)")))
                nil))))
    (should-error (funcall fn)))
  :doc "optional parameters:
`mevedel-tools--validate-params' allows nil for optional parameter"
  (let ((fn (lambda ()
              (let ((count nil))
                (mevedel-tools--validate-params
                 nil nil
                 (count integerp nil))
                t))))
    (should (equal t (funcall fn))))
  :doc "optional parameters:
`mevedel-tools--validate-params' validates optional parameter when provided"
  (let ((fn (lambda ()
              (let ((count "invalid"))
                (mevedel-tools--validate-params
                 nil nil
                 (count integerp nil))
                t))))
    (should-error (funcall fn)))
  :doc "optional parameters:
`mevedel-tools--validate-params' requires required parameters"
  (let ((fn (lambda ()
              (let ((name nil))
                (mevedel-tools--validate-params
                 nil nil
                 (name stringp))
                t))))
    (should-error (funcall fn)))
  :doc "async callback mode:
`mevedel-tools--validate-params' calls callback with error instead of throwing"
  (let ((error-msg nil))
    (cl-block test-func
      (let ((name 123)
            (callback (lambda (msg) (setq error-msg msg))))
        (mevedel-tools--validate-params callback test-func
                                        (name stringp))))
    (should (string-match-p "Error: 'name' must be a string" error-msg)))
  :doc "async callback mode:
`mevedel-tools--validate-params' calls callback for required parameter error"
  (let ((error-msg nil))
    (cl-block test-func
      (let ((name nil)
            (callback (lambda (msg) (setq error-msg msg))))
        (mevedel-tools--validate-params callback test-func
                                        (name stringp))))
    (should (string-match-p "Error: 'name' parameter is required" error-msg))))

(provide 'test-mevedel-tools-validation)
;;; test-mevedel-tools-validation.el ends here
