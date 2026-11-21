;;; test-niwa-present.el --- PGTK presentation switch tests -*- lexical-binding: t -*-

;;; Commentary:

;; The module's effect on real frames is covered by
;; test-niwa-present-wayland.el.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'niwa-present)

(defmacro niwa-present-test--with-frame (type &rest body)
  "Run BODY as an interactive session whose frames are of TYPE."
  (declare (indent 1) (debug t))
  `(let ((noninteractive nil)
         (niwa-present-enabled t)
         (niwa-present--state nil))
     (cl-letf (((symbol-function 'framep) (lambda (_) ,type)))
       ,@body)))

(niwa-deftest niwa-present-ensure
  (:doc "Enables once on PGTK frames and never loads the module elsewhere.")
  (let ((loads 0) (enables 0) (result t))
    (cl-letf (((symbol-function 'niwa-native-load) (lambda () (cl-incf loads) t))
              ((symbol-function 'niwa-present--enable) (lambda () (cl-incf enables) result)))
      (niwa-present-test--with-frame 'x
        (should-not (niwa-present-ensure (selected-frame)))
        (should-not niwa-present--state))
      (let ((noninteractive t))
        (should-not (niwa-present-ensure (selected-frame))))
      (niwa-present-test--with-frame 'pgtk
        (let ((niwa-present-enabled nil))
          (should-not (niwa-present-ensure (selected-frame))))
        (should (= loads 0))
        (should (niwa-present-ensure (selected-frame)))
        (should (niwa-present-active-p))
        (should (niwa-present-ensure (selected-frame)))
        (should (= enables 1)))
      ;; A refusal is reported once and kept.
      (setq result "Emacs draws with cairo_show_text")
      (niwa-present-test--with-frame 'pgtk
        (let (messages)
          (niwa-test--with-captured-diagnostics messages
            (should-not (niwa-present-ensure (selected-frame)))
            (should-not (niwa-present-ensure (selected-frame))))
          (should (string-search "cairo_show_text" messages)))
        (should (equal niwa-present--state result))
        (should (= enables 2))))))

(niwa-deftest niwa-present-ensure/unloadable
  (:doc "Leaves load failure memory to the loader so installation can recover.")
  (cl-letf (((symbol-function 'niwa-native-load) (lambda () nil))
            ((symbol-function 'niwa-present--enable)
             (lambda () (ert-fail "Enabled without a module"))))
    (niwa-present-test--with-frame 'pgtk
      (let ((niwa-native--load-state "Native module compilation failed: x"))
        (should-not (niwa-present-ensure (selected-frame)))
        (should-not niwa-present--state)
        (cl-letf (((symbol-function 'niwa-native-load) (lambda () t))
                  ((symbol-function 'niwa-present--enable) (lambda () t)))
          (should (niwa-present-ensure (selected-frame))))))))

(provide 'test-niwa-present)

;;; test-niwa-present.el ends here
