;;; test-mevedel-present.el --- PGTK presentation switch tests -*- lexical-binding: t -*-

;;; Commentary:

;; The module's effect on real frames is covered by
;; test-mevedel-present-wayland.el.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-present)

(defmacro mevedel-present-test--with-frame (type &rest body)
  "Run BODY as an interactive session whose frames are of TYPE."
  (declare (indent 1) (debug t))
  `(let ((noninteractive nil)
         (mevedel-present-enabled t)
         (mevedel-present--state nil))
     (cl-letf (((symbol-function 'framep) (lambda (_) ,type)))
       ,@body)))

(mevedel-deftest mevedel-present-ensure
  (:doc "Enables once on PGTK frames and never loads the module elsewhere.")
  (let ((loads 0) (enables 0) (result t))
    (cl-letf (((symbol-function 'mevedel-native-load) (lambda () (cl-incf loads) t))
              ((symbol-function 'mevedel-present--enable) (lambda () (cl-incf enables) result)))
      (mevedel-present-test--with-frame 'x
        (should-not (mevedel-present-ensure (selected-frame)))
        (should-not mevedel-present--state))
      (let ((noninteractive t))
        (should-not (mevedel-present-ensure (selected-frame))))
      (mevedel-present-test--with-frame 'pgtk
        (let ((mevedel-present-enabled nil))
          (should-not (mevedel-present-ensure (selected-frame))))
        (should (= loads 0))
        (should (mevedel-present-ensure (selected-frame)))
        (should (mevedel-present-active-p))
        (should (mevedel-present-ensure (selected-frame)))
        (should (= enables 1)))
      ;; A refusal is reported once and kept.
      (setq result "Emacs draws with cairo_show_text")
      (mevedel-present-test--with-frame 'pgtk
        (let (messages)
          (mevedel-test--with-captured-diagnostics messages
            (should-not (mevedel-present-ensure (selected-frame)))
            (should-not (mevedel-present-ensure (selected-frame))))
          (should (string-search "cairo_show_text" messages)))
        (should (equal mevedel-present--state result))
        (should (= enables 2))))))

(mevedel-deftest mevedel-present-ensure/unloadable
  (:doc "Leaves load failure memory to the loader so installation can recover.")
  (cl-letf (((symbol-function 'mevedel-native-load) (lambda () nil))
            ((symbol-function 'mevedel-present--enable)
             (lambda () (ert-fail "Enabled without a module"))))
    (mevedel-present-test--with-frame 'pgtk
      (let ((mevedel-native--load-state "Native module compilation failed: x"))
        (should-not (mevedel-present-ensure (selected-frame)))
        (should-not mevedel-present--state)
        (cl-letf (((symbol-function 'mevedel-native-load) (lambda () t))
                  ((symbol-function 'mevedel-present--enable) (lambda () t)))
          (should (mevedel-present-ensure (selected-frame))))))))

(provide 'test-mevedel-present)

;;; test-mevedel-present.el ends here
