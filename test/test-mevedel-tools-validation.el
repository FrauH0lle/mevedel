;;; test-mevedel-tools-validation.el --- Tests for parameter validation -*- lexical-binding: t -*-

;;; Commentary:

;; Tests for the `mevedel-tools--validate-params' macro

;;; Code:

(require 'mevedel-tool-registry)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name
               load-file-name
               byte-compile-current-file))
          "helpers"))

(mevedel-deftest mevedel-tools--validate-params ()
  ,test
  (test)
  :doc "basic predicate validation:
`mevedel-tools--validate-params' accepts valid string"
  (should (equal t
                 (let ((name "test"))
                   (mevedel-tools--validate-params nil nil (name stringp))
                   t)))
  :doc "basic predicate validation:
`mevedel-tools--validate-params' rejects non-string"
  (should-error
   (let ((name 123))
     (mevedel-tools--validate-params nil nil (name stringp))
     t))
  :doc "basic predicate validation:
`mevedel-tools--validate-params' accepts valid integer"
  (should (equal t
                 (let ((count 42))
                   (mevedel-tools--validate-params nil nil (count integerp))
                   t)))
  :doc "basic predicate validation:
`mevedel-tools--validate-params' rejects non-integer"
  (should-error
   (let ((count "42"))
     (mevedel-tools--validate-params nil nil (count integerp))
     t))
  :doc "booleanp special handling:
`mevedel-tools--validate-params' accepts t as boolean"
  (should (equal t
                 (let ((enabled t))
                   (mevedel-tools--validate-params nil nil (enabled booleanp))
                   t)))
  :doc "booleanp special handling:
`mevedel-tools--validate-params' accepts :json-false as boolean"
  (should (equal t
                 (let ((enabled :json-false))
                   (mevedel-tools--validate-params nil nil (enabled booleanp))
                   t)))
  :doc "booleanp special handling:
`mevedel-tools--validate-params' rejects non-boolean values"
  (should-error
   (let ((enabled "yes"))
     (mevedel-tools--validate-params nil nil (enabled booleanp))
     t))
  :doc "custom type names:
`mevedel-tools--validate-params' reports array instead of vector in error"
  (should-error
   (let ((items "not-an-array"))
     (mevedel-tools--validate-params nil nil (items (vectorp . "array")))
     nil))
  :doc "custom type names:
`mevedel-tools--validate-params' accepts valid vector with array type name"
  (should (equal t
                 (let ((items [1 2 3]))
                   (mevedel-tools--validate-params nil nil (items (vectorp . "array")))
                   t)))
  :doc "optional parameters:
`mevedel-tools--validate-params' allows nil for optional parameter"
  (should (equal t
                 (let ((count nil))
                   (mevedel-tools--validate-params nil nil (count integerp nil))
                   t)))
  :doc "optional parameters:
`mevedel-tools--validate-params' validates optional parameter when provided"
  (should-error
   (let ((count "invalid"))
     (mevedel-tools--validate-params nil nil (count integerp nil))
     t))
  :doc "optional parameters:
`mevedel-tools--validate-params' requires required parameters"
  (should-error
   (let ((name nil))
     (mevedel-tools--validate-params nil nil (name stringp))
     t))
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
