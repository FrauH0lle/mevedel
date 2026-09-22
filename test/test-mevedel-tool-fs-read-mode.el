;;; test-mevedel-tool-fs-read-mode.el --- Display mode lookup -*- lexical-binding: t -*-
;;; Commentary:
;; Cached syntax selection follows the current rules, including in-place edits.
;;; Code:
(require 'helpers
         (file-name-concat
          (file-name-directory (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-tool-fs-read)

(mevedel-deftest mevedel-tool-fs-read--mode-for-file ()
  ,test
  (test)
  :doc "reuses mode selection but observes replacement and in-place rule changes"
  (with-temp-buffer
    (let ((auto-mode-alist (list (cons "\\.sample\\'" 'text-mode)))
          (calls 0)
          (original (symbol-function 'assoc-default)))
      (cl-letf (((symbol-function 'assoc-default)
                 (lambda (&rest args) (cl-incf calls) (apply original args))))
        (should (eq 'text-mode (mevedel-tool-fs-read--mode-for-file "a.sample")))
        (should (eq 'text-mode (mevedel-tool-fs-read--mode-for-file "a.sample")))
        (should (= calls 1))
        (setcdr (car auto-mode-alist) 'fundamental-mode)
        (should (eq 'fundamental-mode (mevedel-tool-fs-read--mode-for-file "a.sample")))
        (setq auto-mode-alist '(("\\.sample\\'" . (emacs-lisp-mode . t))))
        (should (eq 'emacs-lisp-mode (mevedel-tool-fs-read--mode-for-file "a.sample")))
        (should (= calls 3))
        (setq auto-mode-alist (list (cons (copy-sequence "a.sample") 'text-mode)))
        (should (eq 'text-mode (mevedel-tool-fs-read--mode-for-file "a.sample")))
        (aset (caar auto-mode-alist) 0 ?b)
        (should-not (mevedel-tool-fs-read--mode-for-file "a.sample")))))

  :doc "cached misses become matches after a rule is added"
  (with-temp-buffer
    (let (auto-mode-alist)
      (should-not (mevedel-tool-fs-read--mode-for-file "a.unknown"))
      (push '("\\.unknown\\'" . text-mode) auto-mode-alist)
      (should (eq 'text-mode (mevedel-tool-fs-read--mode-for-file "a.unknown")))
      (should-not (mevedel-tool-fs-read--mode-for-file nil))
      (should-not (mevedel-tool-fs-read--mode-for-file "")))))
;;; test-mevedel-tool-fs-read-mode.el ends here
