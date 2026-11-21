;;; test-niwa-skills-owner-load.el -- Cold skill owner test -*- lexical-binding: t -*-

;;; Commentary:

;; Verifies preparation and input owners without loading the niwa umbrella.

;;; Code:

(require 'bytecomp)

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(niwa-deftest niwa-skills-input-scan-tokens/cold-load ()
  ,test
  (test)
  :doc "loads compiled input/preparation owners and scans a real skill token"
  (let* ((root
          (file-name-as-directory
           (file-name-directory (locate-library "niwa"))))
         (compiled-root (make-temp-file "niwa-skills-owner-" t))
         (emacs (expand-file-name invocation-name invocation-directory))
         (gptel-root
          (file-name-directory (locate-library "gptel-request")))
         (owners '("niwa-skills-input.el"
                   "niwa-skills-preparation.el")))
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
                     (require 'niwa-skills-input)
                     (require 'niwa-skills-preparation)
                     (unless
                         (equal '("one" "two words")
                                (niwa-skills-preparation-parse-arguments
                                 "one \"two words\""))
                       (error "Preparation owner did not parse arguments"))
                     (let ((tokens
                            (niwa-skills-input-scan-tokens
                             "Use $demo now"
                             (lambda (name _start _end)
                               (and (equal name "demo") name))
                             t)))
                       (unless (equal "demo" (plist-get (car tokens) :name))
                         (error "Input owner did not scan the token")))
                     (with-temp-buffer
                       (niwa-skills-input-command-delete-start 1))
                     (with-temp-buffer
                       (insert "please /help")
                       (unless (= 8 (niwa-skills-input-command-delete-start 8))
                         (error "Input owner did not resolve command start")))
                     (when (featurep 'niwa-skills-invoke)
                       (error "Input owner loaded the invocation facade"))
                     (when (featurep 'niwa)
                       (error "Skill owner loaded the niwa umbrella")))))))
            (should (string-empty-p (string-trim (buffer-string))))))
      (delete-directory compiled-root t))))

(provide 'test-niwa-skills-owner-load)
;;; test-niwa-skills-owner-load.el ends here
