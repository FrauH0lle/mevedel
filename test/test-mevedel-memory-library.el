;;; test-mevedel-memory-library.el --- Stored memory inventory -*- lexical-binding: t -*-

;;; Commentary:
;; Exercise topic metadata and the cached, batched display inventory on real files.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory (or buffer-file-name load-file-name byte-compile-current-file)) "helpers"))
(require 'mevedel-memory-library)
(require 'mevedel-session-control-fs)

(mevedel-deftest mevedel-memory-library-copy
    (:doc "large display observations copy strings without recursive list-spine depth")
  (let* ((original (make-list 10000 (list :title (copy-sequence "Display"))))
         (copied (mevedel-memory-library-copy original)))
    (should (equal original copied))
    (aset (plist-get (car copied) :title) 0 ?X)
    (should (equal "Display" (plist-get (car original) :title)))
    (should (equal "Display" (plist-get (cadr copied) :title)))))

(mevedel-deftest mevedel-memory-library--metadata ()
  ,test
  (test)
  :doc "frontmatter exactly as the writer emits it reads without YAML and equals the YAML parse"
  (dolist (title '("Plain title" "Quote \" back\\slash/ new\nline\ttab é 😀"))
    (let* ((yaml (format "name: %s\ndescription: %s\ntype: feedback"
                         (decode-coding-string (json-serialize title) 'utf-8-unix)
                         (decode-coding-string (json-serialize "Hook \"text\"") 'utf-8-unix)))
           (expected (yaml-parse-string yaml :object-type 'plist :object-key-type 'keyword)))
      (should (equal title (plist-get expected :name)))
      (cl-letf (((symbol-function 'yaml-parse-string) (lambda (&rest _) (error "Unexpected YAML parse"))))
        (should (equal expected (mevedel-memory-library--metadata (concat "---\n" yaml "\n---\n\nBody\n")))))))
  :doc "every other frontmatter form keeps YAML's exact result, including its escape handling"
  (dolist (yaml '("name: Manual lesson\ntype: feedback" "name: \"\\x41\"" "name: \"First\"\nname: \"Second\""
                  "type: true" "type: \"user\" # comment" "name: \"Open\n  continued\"" "name: [unclosed"
                  "\"name\": \"Quoted key\"\ntype: feedback" "{name: Flow, type: feedback}"
                  "name: \"A\"\ndescription: \"B\"\ntype: other"
                  "name: \"\\u00e9\"\ndescription: \"x\"\ntype: user"
                  "name: \"\\b\\f\\r\"\ndescription: \"x\"\ntype: user"
                  "name: \"A\"\ndescription: \"line\nname: \\\"Injected\\\"\"\ntype: user"
                  "description: \"x\"\nname: \"A\"\ntype: user"))
    (let ((expected (condition-case err (yaml-parse-string yaml :object-type 'plist :object-key-type 'keyword)
                      (error (list 'signal (car err))))))
      (should (equal expected (condition-case err (mevedel-memory-library--metadata (concat "---\n" yaml "\n---\n"))
                                (error (list 'signal (car err))))))))
  :doc "text without frontmatter has no metadata"
  (should-not (mevedel-memory-library--metadata "# Heading\n\nname: \"Not frontmatter\"\n")))

(mevedel-deftest mevedel-memory-library-list
    (:vars* ((root (make-temp-file "mevedel-memory-library-" t))
             (memory (file-name-concat root "memory"))
             (workspace (mevedel-workspace--create :root root :type 'project))
             (mevedel-memory-dirs (list memory))
             (mevedel-memory-library--cache (make-hash-table :test #'equal))
             (programs 0) (reads 0))
     :after-each ((delete-directory root t)))
  (progn
    (make-directory memory)
    (write-region "- [One](one.md) - Hook\n" nil (file-name-concat memory "MEMORY.md") nil 'silent)
    (write-region "---\nname: \"One topic\"\ntype: user\n---\n\nOne\n" nil (file-name-concat memory "one.md") nil 'silent)
    (write-region "---\nname: Two topic\ntype: project\n---\n\nTwo\n" nil (file-name-concat memory "two.md") nil 'silent)
    (let ((run (symbol-function 'mevedel-session-control-fs-run-program)))
      (cl-letf (((symbol-function 'mevedel-session-control-fs-run-program)
                 (lambda (operations &rest args)
                   (cl-incf programs) (cl-incf reads (length operations))
                   (apply run operations args))))
        (cl-flet ((titles () (mapcar (lambda (row) (list (plist-get row :file) (plist-get row :title)
                                                         (plist-get row :type) (plist-get row :status)))
                                     (mevedel-memory-library-list workspace))))
          ,test))))
  (test)
  :doc "one batched program reads the index and every topic; unchanged files are not reread"
  (progn
    (should (equal '(("one.md" "One topic" "user" indexed) ("two.md" "Two topic" "project" unindexed)) (titles)))
    (should (= 1 programs))
    (should (= 3 reads))
    (titles)
    (should (= 1 programs)))
  :doc "a changed topic alone is reread, and a removed topic leaves the inventory"
  (progn
    (titles)
    (write-region "---\nname: \"Renamed\"\ntype: reference\n---\n\nOne, longer\n" nil (file-name-concat memory "one.md") nil 'silent)
    (delete-file (file-name-concat memory "two.md"))
    (should (equal '(("one.md" "Renamed" "reference" indexed)) (titles)))
    (should (= 2 programs))
    (should (= 4 reads))
    (should (equal '("MEMORY.md" "one.md")
                   (sort (mapcar #'car (gethash (file-name-as-directory (file-truename memory))
                                                mevedel-memory-library--cache))
                         #'string<))))
  :doc "an index edit updates indexed status"
  (progn
    (titles)
    (write-region "- [Two](two.md) - Hook\n" nil (file-name-concat memory "MEMORY.md") nil 'silent)
    (should (equal '(unindexed indexed) (mapcar #'cadddr (titles))))
    (should (= 4 reads)))
  :doc "consumers mutating returned rows cannot alter cached rows or metadata"
  (let ((rows (mevedel-memory-library-list workspace)))
    (aset (plist-get (car rows) :title) 0 ?X)
    (aset (plist-get (car (mevedel-memory-library-cached workspace)) :title) 1 ?X)
    (setcar rows nil)
    (should (equal "One topic" (plist-get (car (mevedel-memory-library-cached workspace)) :title)))
    (should (equal "One topic" (plist-get (car (mevedel-memory-library-list workspace)) :title)))
    (should (= 1 programs)))
  :doc "asynchronous refresh delivers rows later, exactly once, then caches them"
  (let (delivered)
    (should-not (mevedel-memory-library-cached workspace))
    (mevedel-memory-library-refresh workspace (lambda (rows) (push rows delivered)))
    (with-timeout (10 (ert-fail "Topic refresh did not deliver"))
      (while (not delivered) (accept-process-output nil .01)))
    (mevedel-session-control-fs-wait-async)
    (should (= 1 (length delivered)))
    (should (equal '("one.md" "two.md") (mapcar (lambda (row) (plist-get row :file)) (car delivered))))
    (should (equal (car delivered) (mevedel-memory-library-cached workspace))))
  :doc "oversized, non-UTF-8, linked and malformed topics stay visible with errors"
  (progn
    (write-region (make-string (1+ mevedel-memory-scope--max-file-bytes) ?x) nil (file-name-concat memory "large.md") nil 'silent)
    (let ((coding-system-for-write 'no-conversion))
      (write-region "\377\376bad" nil (file-name-concat memory "binary.md") nil 'silent))
    (make-symbolic-link (file-name-concat memory "one.md") (file-name-concat memory "link.md"))
    (write-region "---\nname: [unclosed\n---\n" nil (file-name-concat memory "broken.md") nil 'silent)
    (let ((rows (mevedel-memory-library-list workspace)))
      (dolist (file '("large.md" "binary.md" "link.md" "broken.md"))
        (let ((row (seq-find (lambda (row) (equal file (plist-get row :file))) rows)))
          (should (plist-get row :error))
          (should (equal (file-name-base file) (plist-get row :title)))))
      (should-not (plist-member (seq-find (lambda (row) (equal "one.md" (plist-get row :file))) rows) :error)))
    (should (= 1 programs)))
  :doc "an unreadable index makes its root unavailable without caching it"
  (progn
    (write-region "\0" nil (file-name-concat memory "MEMORY.md") nil 'silent)
    (let ((rows (mevedel-memory-library-list workspace)))
      (should (equal '(unavailable) (mapcar (lambda (row) (plist-get row :status)) rows)))
      (should (equal "Unavailable memory root" (plist-get (car rows) :title))))
    (should-not (gethash (file-name-as-directory (file-truename memory)) mevedel-memory-library--cache))))

(provide 'test-mevedel-memory-library)
;;; test-mevedel-memory-library.el ends here
