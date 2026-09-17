;;; test-mevedel-test-helpers.el --- Shared fixture tests -*- lexical-binding: t -*-

;;; Commentary:

;; Exercise fixture cleanup on both successful and failing test bodies.

;;; Code:

(require 'mevedel-execution-target)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(mevedel-deftest mevedel-deftest
  (:doc "isolates tool registration and lookup caches even when a case fails")
  (let ((registry mevedel-tool--registry)
        (cache mevedel-tool--resolve-cache)
        (gptel--known-tools (list (list "fixture" (cons "probe" 'original)))))
    (puthash '("fixture" "probe") 'original registry)
    (puthash 'fixture 'original cache)
    (unwind-protect
        (dolist (fail '(nil t))
          (eval
           `(mevedel-deftest mevedel-test--nested-registry-case ()
              (progn
                (mevedel-tool-clear-registry)
                (setcdr (car gptel--known-tools) nil)
                (puthash 'fixture 'changed mevedel-tool--resolve-cache)
                (when ,fail (error "Deliberate fixture failure"))))
           t)
          (let ((body (ert-test-body
                       (ert-get-test 'mevedel-test--nested-registry-case/test))))
            (if fail
                (should-error (funcall body) :type 'error)
              (funcall body)))
          (should (eq registry mevedel-tool--registry))
          (should (eq 'original (gethash '("fixture" "probe") registry)))
          (should (equal gptel--known-tools '(("fixture" ("probe" . original)))))
          (should (eq 'original (gethash 'fixture cache)))
          (ert-delete-test 'mevedel-test--nested-registry-case/test))
      (when (ert-test-boundp 'mevedel-test--nested-registry-case/test)
        (ert-delete-test 'mevedel-test--nested-registry-case/test)))))

(mevedel-deftest mevedel-test--ensure-mock-tramp-method
  (:quiet t
   :doc "a fresh mock login establishes its own HOME under a blank client environment")
  (let* ((root (make-temp-file "mevedel-mock-login-" t))
         (remote-root (format "/mevedelmock:%s:%s/" (system-name) root))
         (expected-home (getenv "HOME")))
    (unwind-protect
        (mevedel-test--with-local-shell-tramp nil
          (tramp-cleanup-all-connections)
          (let ((default-directory remote-root)
                (process-environment nil))
            (with-temp-buffer
              (should (zerop (process-file "sh" nil t nil "-c"
                                           "printf '%s' \"$HOME\"")))
              (should (equal expected-home (buffer-string))))))
      (delete-directory root t))))

(mevedel-deftest mevedel-test--template ()
  ,test
  (test)
  :doc "rejects a second body mistaken for unbound template variables"
  (should-error
   (macroexpand '(mevedel-test--template (should t) (should nil)))
   :type 'error)
  :doc "rejects case values without template variables"
  (should-error
   (macroexpand '(mevedel-test--template (should t) nil 1))
   :type 'error)
  :doc "preserves single bodies and parameterized case values"
  (progn
    (should (equal '(("" (progn (should t) (should-not nil))))
                   (macroexpand
                    '(mevedel-test--template
                      (progn (should t) (should-not nil))))))
    (should (equal '(("" (list 1)) ("second" (list 2)))
                   (macroexpand
                    '(mevedel-test--template (list ,value) (value)
                       1 :doc "second" 2))))))

(provide 'test-mevedel-test-helpers)
;;; test-mevedel-test-helpers.el ends here
