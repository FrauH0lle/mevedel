;;; test-mevedel-hints.el --- Tests for hint file persistence -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'mevedel)
(require 'mevedel-structs)
(require 'mevedel-tool-tutor)
(require 'helpers
         (file-name-concat (file-name-directory (or load-file-name buffer-file-name))
                           "helpers.el"))


;;
;;; Test helpers

(defun mevedel-test--hints-setup ()
  "Set up test environment for hints tests."
  ;; Set up a temporary workspace with a git repo so project-current works
  (let ((dir (mevedel-test-make-temp-dir)))
    (let ((default-directory dir))
      (call-process "git" nil nil nil "init" "-q"))
    (setq mevedel--workspace
          (mevedel-workspace-get-or-create 'project dir dir
                                           (file-name-nondirectory
                                            (directory-file-name dir)))))
  ;; Use a test-specific hints file
  (setq mevedel-hints-file ".mevedel/hints.md"))

(defun mevedel-test--hints-teardown ()
  "Clean up test environment."
  (when-let* ((workspace-root (mevedel-workspace-root mevedel--workspace))
              (hints-path (file-name-concat workspace-root mevedel-hints-file)))
    (when (file-exists-p hints-path)
      (delete-file hints-path))
    (when (file-exists-p (file-name-directory hints-path))
      (delete-directory (file-name-directory hints-path) t))
    (when (file-exists-p workspace-root)
      (delete-directory workspace-root t)))
  (setq mevedel--workspace nil))

(defun mevedel-test-make-temp-dir ()
  "Create a temporary directory and return its path."
  (let ((dir (file-name-concat temporary-file-directory
                               (make-temp-name "mevedel-hints-test-"))))
    (make-directory dir t)
    dir))


;;
;;; Timestamp tests

(mevedel-deftest mevedel-tools--format-hint-timestamp
  (:doc "Format timestamp as YYYY-MM-DD HH:MM")
  (let ((time (encode-time 0 30 14 22 3 2025 nil -1 nil)))
    (should (equal (mevedel-tools--format-hint-timestamp time) "2025-03-22 14:30"))))

(mevedel-deftest mevedel-tools--parse-hint-timestamp
  (:doc "Parse timestamp string to time value")
  (let* ((str "2025-03-22 14:30")
         (time (mevedel-tools--parse-hint-timestamp str))
         (formatted (mevedel-tools--format-hint-timestamp time)))
    (should (equal formatted str))))


;;
;;; File I/O tests

(mevedel-deftest mevedel-tools--hints-file-path
  (:doc "Return absolute path to hints file"
   :before-each (mevedel-test--hints-setup)
   :after-each (mevedel-test--hints-teardown))
  (let* ((path (mevedel-tools--hints-file-path))
         (expected (file-name-concat (mevedel-workspace-root mevedel--workspace) mevedel-hints-file)))
    (should (equal path expected))
    (should (file-name-absolute-p path))))

(mevedel-deftest mevedel-tools--read-hints-file-empty
  (:doc "Return empty alist when file doesn't exist"
   :before-each (mevedel-test--hints-setup)
   :after-each (mevedel-test--hints-teardown))
  (let ((hints (mevedel-tools--read-hints-file)))
    (should (equal hints '()))))

(mevedel-deftest mevedel-tools--write-and-read-hints-file
  (:doc "Write and read hints file correctly"
   :before-each (mevedel-test--hints-setup)
   :after-each (mevedel-test--hints-teardown))
  (let* ((timestamp (current-time))
         (hints-alist `(;; Use strings for concept keys
                        ("error-handling" . (("technique-hint" 2 "Try-catch pattern" ,timestamp)))
                        ("async-patterns" . (("socratic-question" 1 "What happens if?" ,timestamp)))))
         (_ (mevedel-tools--write-hints-file hints-alist))
         (read-hints (mevedel-tools--read-hints-file)))
    ;; Check that concepts are preserved
    (should (equal (sort (mapcar #'car read-hints) #'string<)
                   '("async-patterns" "error-handling")))
    ;; Check hint content (timestamp loses sub-minute precision in roundtrip)
    (let ((error-hints (alist-get "error-handling" read-hints nil nil #'equal)))
      (should (= (length error-hints) 1))
      (cl-destructuring-bind (type depth summary read-ts) (car error-hints)
        (should (equal type "technique-hint"))
        (should (= depth 2))
        (should (equal summary "Try-catch pattern"))
        (should (equal (format-time-string "%Y-%m-%d %H:%M" read-ts)
                       (format-time-string "%Y-%m-%d %H:%M" timestamp)))))))

(mevedel-deftest mevedel-tools--append-hint-to-file
  (:doc "Append hint to file correctly"
   :before-each (mevedel-test--hints-setup)
   :after-each (mevedel-test--hints-teardown))
  (progn
    ;; Append first hint
    (mevedel-tools--append-hint-to-file "technique-hint" "error-handling" "Try-catch pattern" 2)
    ;; Append second hint to same concept
    (mevedel-tools--append-hint-to-file "socratic-question" "error-handling" "What errors?" 1)
    ;; Append hint to different concept
    (mevedel-tools--append-hint-to-file "doc-reference" "async-patterns" "See docs" 3)
    ;; Read and verify
    (let ((hints (mevedel-tools--read-hints-file)))
      ;; Should have 2 concepts
      (should (= (length hints) 2))
      ;; error-handling should have 2 hints
      (let ((error-hints (alist-get "error-handling" hints nil nil #'equal)))
        (should (= (length error-hints) 2)))
      ;; async-patterns should have 1 hint
      (let ((async-hints (alist-get "async-patterns" hints nil nil #'equal)))
        (should (= (length async-hints) 1))))))

(mevedel-deftest mevedel-tools--count-hints-in-file
  (:doc "Count total hints correctly"
   :before-each (mevedel-test--hints-setup)
   :after-each (mevedel-test--hints-teardown))
  (progn
    ;; Initially 0
    (should (= (mevedel-tools--count-hints-in-file) 0))
    ;; Add hints
    (mevedel-tools--append-hint-to-file "technique-hint" "concept-a" "Hint 1" 1)
    (should (= (mevedel-tools--count-hints-in-file) 1))
    (mevedel-tools--append-hint-to-file "technique-hint" "concept-b" "Hint 2" 2)
    (should (= (mevedel-tools--count-hints-in-file) 2))
    (mevedel-tools--append-hint-to-file "technique-hint" "concept-a" "Hint 3" 3)
    (should (= (mevedel-tools--count-hints-in-file) 3))))

(mevedel-deftest mevedel-tools--clear-hints-file-all
  (:doc "Clear all hints from file"
   :before-each (mevedel-test--hints-setup)
   :after-each (mevedel-test--hints-teardown))
  (progn
    ;; Add hints
    (mevedel-tools--append-hint-to-file "technique-hint" "concept-a" "Hint 1" 1)
    (mevedel-tools--append-hint-to-file "technique-hint" "concept-b" "Hint 2" 2)
    ;; Clear all
    (mevedel-tools--clear-hints-file)
    ;; Verify empty
    (should (equal (mevedel-tools--read-hints-file) '()))
    (should (= (mevedel-tools--count-hints-in-file) 0))))

(mevedel-deftest mevedel-tools--clear-hints-file-concept
  (:doc "Clear hints for specific concept"
   :before-each (mevedel-test--hints-setup)
   :after-each (mevedel-test--hints-teardown))
  (progn
    ;; Add hints
    (mevedel-tools--append-hint-to-file "technique-hint" "concept-a" "Hint 1" 1)
    (mevedel-tools--append-hint-to-file "technique-hint" "concept-b" "Hint 2" 2)
    (mevedel-tools--append-hint-to-file "technique-hint" "concept-a" "Hint 3" 3)
    ;; Clear concept-a only
    (mevedel-tools--clear-hints-file "concept-a")
    ;; Verify concept-a removed, concept-b remains
    (let ((hints (mevedel-tools--read-hints-file)))
      (should (not (alist-get "concept-a" hints nil nil #'equal)))
      (should (alist-get "concept-b" hints nil nil #'equal)))
    (should (= (mevedel-tools--count-hints-in-file) 1))))


;;
;;; Markdown format tests

(mevedel-deftest mevedel-tools--write-hints-file-format
  (:doc "Write hints in correct markdown format"
   :before-each (mevedel-test--hints-setup)
   :after-each (mevedel-test--hints-teardown))
  (let* ((timestamp (encode-time 0 30 14 22 3 2025 nil -1 nil))
         (hints-alist `(;; Use strings for concept keys
                        ("error-handling" . (("technique-hint" 2 "Try-catch pattern" ,timestamp)))))
         (expected-content
          (concat "# Tutor Hints\n\n"
                  "<!-- Auto-generated by mevedel tutor mode. Do not edit manually. -->\n\n"
                  "## error-handling\n"
                  "- [technique-hint, depth 2] Try-catch pattern (2025-03-22 14:30)\n\n")))
    (mevedel-tools--write-hints-file hints-alist)
    (let ((file-path (mevedel-tools--hints-file-path)))
      (with-temp-buffer
        (insert-file-contents file-path)
        (should (equal (buffer-string) expected-content))))))

(mevedel-deftest mevedel-tools--read-hints-file-format
  (:doc "Parse markdown format correctly"
   :before-each (mevedel-test--hints-setup)
   :after-each (mevedel-test--hints-teardown))
  (let ((content
         (concat "# Tutor Hints\n\n"
                 "<!-- Auto-generated by mevedel tutor mode. Do not edit manually. -->\n\n"
                 "## error-handling\n"
                 "- [technique-hint, depth 2] Try-catch pattern (2025-03-22 14:30)\n\n"
                 "## async-patterns\n"
                 "- [socratic-question, depth 1] What happens? (2025-03-22 15:00)\n\n")))
    ;; Write content directly
    (let ((file-path (mevedel-tools--hints-file-path)))
      (make-directory (file-name-directory file-path) t)
      (with-temp-buffer
        (insert content)
        (write-region (point-min) (point-max) file-path nil 'silent)))
    ;; Read and verify
    (let ((hints (mevedel-tools--read-hints-file)))
      (should (= (length hints) 2))
      (let ((error-hints (alist-get "error-handling" hints nil nil #'equal)))
        (should (= (length error-hints) 1))
        (cl-destructuring-bind (type depth summary _timestamp) (car error-hints)
          (should (equal type "technique-hint"))
          (should (= depth 2))
          (should (equal summary "Try-catch pattern")))))))


(provide 'test-mevedel-hints)
;;; test-mevedel-hints.el ends here
