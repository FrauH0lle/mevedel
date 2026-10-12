;;; native-wait-advised.el --- Refuse an unsafe wait entry -*- lexical-binding: t -*-

(require 'ert)
(require 'mevedel-native)
(require 'mevedel-structs)

(defvar mevedel-test-native-root)

(let ((mevedel-user-dir mevedel-test-native-root)
      (advice (lambda (original &rest arguments)
                (apply original arguments))))
  (unwind-protect
      (progn
        (advice-add 'condition-wait :around advice)
        (should (mevedel-native-load))
        (should-not (mevedel-native--timed-wait-available-p)))
    (advice-remove 'condition-wait advice)))
