;;; test-mevedel-memory-reference.el -- Reference evidence tests -*- lexical-binding: t -*-

;;; Commentary:

;; Reference checks report bounded observations, never semantic verification.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-memory-scope)
(require 'mevedel-memory-reference)
(require 'mevedel-journal-store)
(require 'mevedel-system)

(mevedel-deftest mevedel-memory-reference-check ()
  ,test
  (test)
  :doc "cold reference checks load their timestamp dependency without the umbrella"
  (with-temp-buffer
    (let ((status
           (call-process
            (expand-file-name invocation-name invocation-directory) nil t nil
            "--batch" "-Q" "--eval"
            (prin1-to-string
             `(progn
                (setq load-path ',load-path)
                (require 'mevedel-memory-reference)
                (unless (plist-get (mevedel-memory-reference-check '(:roots nil)) :complete)
                  (error "Empty reference scope was not complete"))
                (when (featurep 'mevedel)
                  (error "Reference check loaded the umbrella")))))))
      (ert-info ((buffer-string)) (should (= 0 status)))
      (should (string-empty-p (buffer-string)))))
  :doc "records file occurrence, scoped absence, and unsupported references without topic writes"
  (let* ((directory (make-temp-file "mevedel-memory-reference-" t))
         (workspace (mevedel-workspace--create :root directory))
         (mevedel-memory-dirs '("memory"))
         (topic (file-name-concat directory "memory" "build.md"))
         (body "Read `src/build.el` and `missing.el`. `rg -n` and `--flag` are commands. `https://example.com/a.el` and `some-function` remain unknown. Repeated `src/build.el` is one check.\n"))
    (unwind-protect
        (progn
          (make-directory (file-name-concat directory "memory"))
          (make-directory (file-name-concat directory "src"))
          (with-temp-file (file-name-concat directory "src" "build.el") (insert ";; Source exists.\n"))
          (with-temp-file topic (insert body))
          (let* ((scope (mevedel-memory-scope-capture workspace))
                 (result (mevedel-memory-reference-check scope))
                 (references (plist-get result :references)))
            (should (= 6 (length references)))
            (should (plist-get result :complete))
            (dolist (reference references)
              (should (equal (plist-get reference :result)
                             (pcase (plist-get reference :token)
                               ("src/build.el" "found")
                               ("missing.el" "not found")
                               (_ "unknown"))))
              (should (string-match-p "workspace-relative path existence" (plist-get reference :scope)))
              (should (string-match-p "\\`[0-9-]+T[0-9:]+Z\\'" (plist-get reference :checked))))
            (let* ((review (mevedel-journal-store-publish-review
                            directory
                            (list :pass-id (make-string 64 ?a) :workspace (make-string 64 ?b)
                                  :created "2026-09-07T12:00:00Z" :model "test:model"
                                  :focus "" :digests nil :proposals nil :references references)))
                   (stored (mevedel-journal-store-read directory (plist-get review :file))))
              (should (equal references (plist-get stored :references))))
          (should (equal body (with-temp-buffer (insert-file-contents topic) (buffer-string))))))
      (delete-directory directory t)))
  :doc "longer inline delimiters do not lose the following reference and private paths remain unknown"
  (let* ((directory (make-temp-file "mevedel-memory-reference-inline-" t))
         (workspace (mevedel-workspace--create :root directory))
         (mevedel-memory-dirs '("memory"))
         (topic (file-name-concat directory "memory" "refs.md")))
    (unwind-protect
        (progn
          (make-directory (file-name-directory topic))
          (with-temp-file (file-name-concat directory "source.el") (insert ";; A file.\n"))
          (with-temp-file topic
            (insert "Read ``source.el:7`` then `missing.el`. Private `.mevedel/state/journal/job.json`, `memory/refs.md`, and `../outside.el` are outside source scope.\n"))
          (let ((references (plist-get (mevedel-memory-reference-check
                                        (mevedel-memory-scope-capture workspace)) :references)))
            (should (= 5 (length references)))
            (should (equal '("found" "not found" "unknown" "unknown" "unknown")
                           (mapcar (lambda (reference) (plist-get reference :result)) references)))))
      (delete-directory directory t)))
  :doc "the fixed cap reports incomplete checks and topics without references get no certification"
  (let* ((directory (make-temp-file "mevedel-memory-reference-limit-" t))
         (workspace (mevedel-workspace--create :root directory))
         (mevedel-memory-dirs '("memory"))
         (topic (file-name-concat directory "memory" "refs.md")))
    (unwind-protect
        (progn
          (make-directory (file-name-directory topic))
          (with-temp-file topic (insert "No candidate references.\n"))
          (should-not (plist-get (mevedel-memory-reference-check
                                 (mevedel-memory-scope-capture workspace)) :references))
          (with-temp-file topic
            (dotimes (index 201) (insert (format "`some-function-%d`\n" index))))
          (let* ((scope (mevedel-memory-scope-capture workspace))
                 (result (mevedel-memory-reference-check scope)))
            (should (= 200 (length (plist-get result :references))))
            (should-not (plist-get result :complete))
            (should (plist-get result :omissions))
            (cl-letf (((symbol-function 'system-name) (lambda () "another-client")))
              (let ((unavailable (mevedel-memory-reference-check scope)))
                (should-not (plist-get unavailable :references))
                (should-not (plist-get unavailable :complete))))))
      (delete-directory directory t))))

(provide 'test-mevedel-memory-reference)
;;; test-mevedel-memory-reference.el ends here
