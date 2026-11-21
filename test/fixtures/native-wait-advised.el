;;; native-wait-advised.el --- Refuse an unsafe wait entry -*- lexical-binding: t -*-

(require 'ert)
(require 'niwa-native)
(require 'niwa-structs)

(defvar niwa-test-native-root)

(let ((niwa-user-dir niwa-test-native-root)
      (advice (lambda (original &rest arguments)
                (apply original arguments))))
  (unwind-protect
      (progn
        (advice-add 'condition-wait :around advice)
        (should (niwa-native-load))
        (should-not (niwa-native--timed-wait-available-p)))
    (advice-remove 'condition-wait advice)))
