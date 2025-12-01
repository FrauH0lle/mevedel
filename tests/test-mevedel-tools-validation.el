;;; test-mevedel-tools-validation.el --- Tests for parameter validation -*- lexical-binding: t -*-

;; Copyright (C) 2025

;;; Commentary:
;; Tests for the mevedel-tools--validate-params macro

;;; Code:

(require 'buttercup)
(require 'cl-lib)

;; Load the macro
(load-file "mevedel-tools.el")

(describe "mevedel-tools--validate-params"

  (describe "basic predicate validation"
    (it "accepts valid string"
      (let ((fn (lambda ()
                  (let ((name "test"))
                    (mevedel-tools--validate-params
                     nil nil
                     (name stringp))
                    t))))
        (expect (funcall fn) :to-be t)))

    (it "rejects non-string"
      (let ((fn (lambda ()
                  (let ((name 123))
                    (mevedel-tools--validate-params
                     nil nil
                     (name stringp))
                    t))))
        (expect (fn) :to-throw 'error)))

    (it "accepts valid integer"
      (let ((fn (lambda ()
                  (let ((count 42))
                    (mevedel-tools--validate-params
                     nil nil
                     (count integerp))
                    t))))
        (expect (funcall fn) :to-be t)))

    (it "rejects non-integer"
      (let ((fn (lambda ()
                  (let ((count "42"))
                    (mevedel-tools--validate-params
                     nil nil
                     (count integerp))
                    t))))
        (expect (fn) :to-throw 'error))))

  (describe "booleanp special handling"
    (it "accepts t as boolean"
      (let ((fn (lambda ()
                  (let ((enabled t))
                    (mevedel-tools--validate-params
                     nil nil
                     (enabled booleanp))
                    t))))
        (expect (funcall fn) :to-be t)))

    (it "accepts :json-false as boolean"
      (let ((fn (lambda ()
                  (let ((enabled :json-false))
                    (mevedel-tools--validate-params
                     nil nil
                     (enabled booleanp))
                    t))))
        (expect (funcall fn) :to-be t)))

    (it "rejects non-boolean values"
      (let ((fn (lambda ()
                  (let ((enabled "yes"))
                    (mevedel-tools--validate-params
                     nil nil
                     (enabled booleanp))
                    t))))
        (expect (fn) :to-throw 'error))))

  (describe "custom type names"
    (it "reports array instead of vector in error"
      (let ((fn (lambda ()
                  (let ((items "not-an-array"))
                    (mevedel-tools--validate-params
                     nil nil
                     (items (vectorp . "array")))
                    nil))))
        (expect (fn) :to-throw 'error)))

    (it "accepts valid vector with array type name"
      (let ((fn (lambda ()
                  (let ((items [1 2 3]))
                    (mevedel-tools--validate-params
                     nil nil
                     (items (vectorp . "array")))
                    t))))
        (expect (funcall fn) :to-be t))))

  (describe "lambda validators"
    (it "accepts value passing lambda validation"
      (let ((fn (lambda ()
                  (let ((score 85))
                    (mevedel-tools--validate-params
                     nil nil
                     (score (lambda (x) (and (numberp x) (> x 0)))))
                    t))))
        (expect (funcall fn) :to-be t)))

    (it "rejects value failing lambda validation"
      (let ((fn (lambda ()
                  (let ((score -10))
                    (mevedel-tools--validate-params
                     nil nil
                     (score (lambda (x) (and (numberp x) (> x 0)))))
                    t))))
        (expect (fn) :to-throw 'error)))

    (it "uses custom type name with lambda"
      (let ((fn (lambda ()
                  (let ((percent 150))
                    (mevedel-tools--validate-params
                     nil nil
                     (percent ((lambda (x) (and (numberp x) (>= x 0) (<= x 100))) . "percentage (0-100)")))
                    nil))))
        (expect (fn) :to-throw 'error))))

  (describe "optional parameters"
    (it "allows nil for optional parameter"
      (let ((fn (lambda ()
                  (let ((count nil))
                    (mevedel-tools--validate-params
                     nil nil
                     (count integerp nil))
                    t))))
        (expect (funcall fn) :to-be t)))

    (it "validates optional parameter when provided"
      (let ((fn (lambda ()
                  (let ((count "invalid"))
                    (mevedel-tools--validate-params
                     nil nil
                     (count integerp nil))
                    t))))
        (expect (fn) :to-throw 'error)))

    (it "requires required parameters"
      (let ((fn (lambda ()
                  (let ((name nil))
                    (mevedel-tools--validate-params
                     nil nil
                     (name stringp))
                    t))))
        (expect (fn) :to-throw 'error))))

  (describe "async callback mode"
    (it "calls callback with error instead of throwing"
      (let ((error-msg nil))
        (cl-block test-func
          (let ((name 123)
                (callback (lambda (msg) (setq error-msg msg))))
            (mevedel-tools--validate-params callback test-func
                                            (name stringp))))
        (expect error-msg :to-match "Error: 'name' must be a string")))

    (it "calls callback for required parameter error"
      (let ((error-msg nil))
        (cl-block test-func
          (let ((name nil)
                (callback (lambda (msg) (setq error-msg msg))))
            (mevedel-tools--validate-params callback test-func
                                            (name stringp))))
        (expect error-msg :to-match "Error: 'name' parameter is required")))))

(provide 'test-mevedel-tools-validation)
;;; test-mevedel-tools-validation.el ends here
