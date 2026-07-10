;;; test-mevedel-transcript-audit.el --- Audit transcript tests -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-utilities)
(require 'mevedel-transcript-audit)

(mevedel-deftest mevedel-transcript-audit-spans ()
  ,test
  (test)
  :doc "parses bounded records with source-relative spans"
  (let* ((first (mevedel--format-hook-audit-record
                 '(:type prompt-rewrite :event "UserPromptSubmit")))
         (second (mevedel--format-hook-audit-record
                  '(:type tool-context :event "PostToolUse")))
         (text (concat "before" first "middle" second "after"))
         (spans (mevedel-transcript-audit-spans text)))
    (should (equal '(prompt-rewrite tool-context)
                   (mapcar (lambda (span)
                             (plist-get (plist-get span :record) :type))
                           spans)))
    (dolist (span spans)
      (should (< (plist-get span :start) (plist-get span :end)))
      (should (string-prefix-p mevedel--hook-audit-open
                               (substring text
                                          (plist-get span :start)
                                          (plist-get span :end)))))))

(mevedel-deftest mevedel-transcript-audit-records ()
  ,test
  (test)
  :doc "filters parsed records by type"
  (let ((text (concat
               (mevedel--format-hook-audit-record '(:type prompt-rewrite))
               (mevedel--format-hook-audit-record '(:type tool-context)))))
    (should (equal '((:type tool-context))
                   (mevedel-transcript-audit-records text 'tool-context)))))

(mevedel-deftest mevedel-transcript-audit-only-p ()
  ,test
  (test)
  :doc "distinguishes audit-only scaffolding from visible transcript text"
  (let ((block (mevedel--format-hook-audit-record '(:type tool-context))))
    (should (mevedel-transcript-audit-only-p block))
    (should-not (mevedel-transcript-audit-only-p (concat "visible" block)))
    (should-not (mevedel-transcript-audit-only-p "   "))))

(provide 'test-mevedel-transcript-audit)

;;; test-mevedel-transcript-audit.el ends here
