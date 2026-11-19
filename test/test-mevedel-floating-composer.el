;;; test-mevedel-floating-composer.el --- Floating composer tests -*- lexical-binding: t -*-

;;; Commentary:

;; Batch cases cover the pure placement.  Child frames are not assertable in
;; batch; the native cases live in test-mevedel-floating-composer-graphical.el.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-floating-composer)

(mevedel-deftest mevedel-floating-composer--place
  (:doc "`mevedel-floating-composer--place' insets, floats and caps the frame")
  ,test
  (test)
  :doc "insets two columns per side and floats half a line above the bottom"
  (should (equal '(754 100 30 504)
                 (mevedel-floating-composer--place '(10 20 810 620) 10 20 100 3)))
  :doc "caps the height at half the host's text height"
  (should (equal '(754 300 30 304)
                 (mevedel-floating-composer--place '(10 20 810 620) 10 20 500 3)))
  :doc "never shrinks below one line"
  (should (equal '(754 20 30 584)
                 (mevedel-floating-composer--place '(10 20 810 620) 10 20 5 3)))
  :doc "a borderless frame uses the full inset width"
  (should (equal '(760 100 30 510)
                 (mevedel-floating-composer--place '(10 20 810 620) 10 20 100 0)))
  :doc "a narrow host shrinks the inset so the frame stays inside it"
  (pcase-let ((`(,width ,_ ,x ,_)
               (mevedel-floating-composer--place '(0 0 40 600) 10 20 100 3)))
    (should (= 10 width))
    (should (= 12 x))
    (should (<= (+ x width 6) 40)))
  :doc "a host narrower than one column drops the inset entirely"
  (pcase-let ((`(,width ,_ ,x ,_)
               (mevedel-floating-composer--place '(0 0 8 600) 10 20 100 3)))
    (should (= 0 x))
    (should (= 2 width))))

;;; test-mevedel-floating-composer.el ends here
