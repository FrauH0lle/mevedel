;;; test-mevedel-skills-owner-load.el -- Cold skill owner test -*- lexical-binding: t -*-

;;; Commentary:

;; Verifies preparation and input owners without loading the mevedel umbrella.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(mevedel-deftest mevedel-skills-input-scan-tokens/cold-load ()
  ,test
  (test)
  :doc "loads compiled input/preparation owners and scans a real skill token"
  (let* ((root
          (file-name-as-directory
           (file-name-directory (locate-library "mevedel"))))
         (compiled-root (make-temp-file "mevedel-skills-owner-" t))
         (emacs (expand-file-name invocation-name invocation-directory))
         (gptel-root
          (file-name-directory (locate-library "gptel-request")))
         (owners '("mevedel-skills-input.el"
                   "mevedel-skills-preparation.el")))
    (unwind-protect
        (progn
          (dolist (owner owners)
            (copy-file (file-name-concat root owner)
                       (file-name-concat compiled-root owner))
            (let ((byte-compile-verbose nil))
              (byte-compile-file (file-name-concat compiled-root owner))))
          (with-temp-buffer
            (should
             (= 0
                (call-process
                 emacs nil t nil
                 "--batch" "-Q" "-L" compiled-root "-L" root
                 "-L" gptel-root
                 "--eval"
                 (prin1-to-string
                  '(progn
                     (require 'mevedel-skills-input)
                     (require 'mevedel-skills-preparation)
                     (unless
                         (equal '("one" "two words")
                                (mevedel-skills-preparation-parse-arguments
                                 "one \"two words\""))
                       (error "Preparation owner did not parse arguments"))
                     (let ((tokens
                            (mevedel-skills-input-scan-tokens
                             "Use $demo now"
                             (lambda (name _start _end)
                               (and (equal name "demo") name))
                             t)))
                       (unless (equal "demo" (plist-get (car tokens) :name))
                         (error "Input owner did not scan the token")))
                     (with-temp-buffer
                       (mevedel-skills-input-command-delete-context 1))
                     (with-temp-buffer
                       (let ((gptel-prompt-prefix-alist
                              '((fundamental-mode . "### "))))
                         (insert "### /help")
                         (let ((context
                                (mevedel-skills-input-command-delete-context
                                 5)))
                           (unless (and (plist-get context :after-prefix)
                                        (= 5 (plist-get context :delete-start)))
                             (error "Input owner did not resolve command context")))))
                     (when (featurep 'mevedel-skills-invoke)
                       (error "Input owner loaded the invocation facade"))
                     (when (featurep 'mevedel)
                       (error "Skill owner loaded the mevedel umbrella")))))))
            (should (string-empty-p (string-trim (buffer-string))))))
      (delete-directory compiled-root t))))

(provide 'test-mevedel-skills-owner-load)
;;; test-mevedel-skills-owner-load.el ends here
