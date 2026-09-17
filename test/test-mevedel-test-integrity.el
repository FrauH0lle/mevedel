;;; test-mevedel-test-integrity.el -- Test-suite structural guards -*- lexical-binding: t -*-

;;; Commentary:

;; Guards the test suite against structurally swallowed tests.  A paren slip
;; can nest a deftest inside its predecessor's body, where it still reads and
;; byte-compiles but is never defined and never runs; twenty durability
;; deftests were lost that way for the whole life of a feature branch.
;; Loose template keywords also identify cases accidentally moved outside
;; their deftest, where their bodies run during loading without ERT isolation.

;;; Code:

(require 'ert)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(defun test-mevedel-integrity--column-zero-deftests (file)
  "Return how many lines of FILE open a deftest at column zero."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let ((count 0))
      (while (re-search-forward
              (rx line-start "(" (or "mevedel-deftest" "ert-deftest")
                  symbol-end)
              nil t)
        (cl-incf count))
      count)))

(defun test-mevedel-integrity--top-level-deftests (file)
  "Count top-level deftests in FILE, rejecting loose template keywords."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let ((count 0))
      (condition-case nil
          (while t
            (let ((form (read (current-buffer))))
              (when (keywordp form)
                (error "Loose test template keyword %S in %s" form file))
              (when (memq (car-safe form) '(mevedel-deftest ert-deftest))
                (cl-incf count))))
        (end-of-file count)))))

(mevedel-deftest test-mevedel-integrity--top-level-deftests ()
  (let ((file (make-temp-file "mevedel-test-structure-" nil ".el")))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "(mevedel-deftest first () (should t))\n"
                    "(ert-deftest second () (should t))\n"))
          (should (= 2 (test-mevedel-integrity--top-level-deftests file)))
          (with-temp-file file
            (insert "(mevedel-deftest first () (should t))\n"
                    ":doc \"escaped case\"\n(should t)\n"))
          (should-error (test-mevedel-integrity--top-level-deftests file)))
      (delete-file file))))

(mevedel-deftest mevedel-test-file-integrity ()
  ,test
  (test)
  :doc "every column-zero deftest in every test file is a top-level form"
  (let* ((directory (file-name-directory
                     (or (bound-and-true-p buffer-file-name)
                         load-file-name
                         (expand-file-name "test/" default-directory))))
         (files (directory-files directory t "\\`test-.*\\.el\\'"))
         swallowed)
    (should files)
    (dolist (file files)
      (let ((written (test-mevedel-integrity--column-zero-deftests file))
            (defined (test-mevedel-integrity--top-level-deftests file)))
        (unless (= written defined)
          (push (format "%s writes %d deftests but defines %d"
                        (file-name-nondirectory file) written defined)
                swallowed))))
    (should-not swallowed)))

(provide 'test-mevedel-test-integrity)

;;; test-mevedel-test-integrity.el ends here
