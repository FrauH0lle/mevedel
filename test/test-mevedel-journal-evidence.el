;;; test-mevedel-journal-evidence.el -- Bounded journal input -*- lexical-binding: t -*-

;;; Commentary:

;; Uses real local notes to verify snapshot bounds and provenance.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-journal-evidence)

(mevedel-deftest mevedel-journal-evidence-notes ()
  ,test
  (test)
  :doc "freezes notes, lists binary names, and keeps only plan first lines"
  (let* ((session-dir (make-temp-file "mevedel-journal-notes-" t))
         (local (file-name-concat session-dir "local")))
    (unwind-protect
        (progn
          (should (equal "" (mevedel-journal-evidence-notes (mevedel-session--create :save-path session-dir))))
          (make-directory (file-name-concat local "plans") t)
          (write-region "The user corrected the build command.\n" nil
                        (file-name-concat local "notes.md") nil 'silent)
          (write-region "# Plan title\nDo not include the plan body.\n" nil
                        (file-name-concat local "plans" "work.md") nil 'silent)
          (write-region "binary body must not enter the prompt" nil
                        (file-name-concat local "diagram.png") nil 'silent)
          (let ((snapshot (mevedel-journal-evidence-notes (mevedel-session--create :save-path session-dir))))
            (should (string-match-p "prior context" snapshot))
            (should (string-match-p "work://notes.md" snapshot))
            (should (string-match-p "user corrected the build command" snapshot))
            (should (string-match-p "diagram.png" snapshot))
            (should-not (string-match-p "binary body" snapshot))
            (should (string-match-p "Plan title" snapshot))
            (should-not (string-match-p "include the plan body" snapshot))
            (write-region "Changed after capture" nil (file-name-concat local "notes.md")
                          nil 'silent)
            (should-not (string-match-p "Changed after capture" snapshot))
            (should (<= (string-bytes snapshot) 32768))))
      (delete-directory session-dir t)))

  :doc "bounds multibyte notes, labels omissions, and uses deterministic filename order"
  (let* ((session-dir (make-temp-file "mevedel-journal-notes-bound-" t))
         (local (file-name-concat session-dir "local")))
    (unwind-protect
        (progn
          (make-directory local)
          (write-region (make-string 20000 #x754c) nil
                        (file-name-concat local "aaa.md") nil 'silent)
          (write-region "Other note after the input cap" nil
                        (file-name-concat local "zzz.md") nil 'silent)
          (let ((snapshot (mevedel-journal-evidence-notes (mevedel-session--create :save-path session-dir))))
            (should (<= (string-bytes snapshot) 32768))
            (should (string-match-p "work://aaa.md" snapshot))
            (should (string-match-p "omitted: byte limit" snapshot))
            (should (string-match-p "\u754c" snapshot))
            (should-not (string-match-p "Other note" snapshot))
            (should-not (cl-some (lambda (char) (eq (char-charset char) 'eight-bit))
                                 snapshot))))
      (delete-directory session-dir t)))

  :doc "lists unrecognized binary files and rejects symlink escapes"
  (let* ((session-dir (make-temp-file "mevedel-journal-notes-types-" t))
         (local (file-name-concat session-dir "local"))
         (outside (make-temp-file "mevedel-journal-notes-outside-")))
    (unwind-protect
        (progn
          (make-directory local)
          (let ((coding-system-for-write 'no-conversion))
            (write-region (unibyte-string 0 1 2 255) nil
                          (file-name-concat local "unknown.data") nil 'silent))
          (write-region "outside evidence must not be read" nil outside nil 'silent)
          (let ((snapshot (mevedel-journal-evidence-notes (mevedel-session--create :save-path session-dir))))
            (should (string-match-p "unknown.data" snapshot))
            (should (string-match-p "name only" snapshot))
            (should-not (string-match-p "\0" snapshot)))
          (make-symbolic-link outside (file-name-concat local "linked.md"))
          (should-error (mevedel-journal-evidence-notes (mevedel-session--create :save-path session-dir))))
      (delete-directory session-dir t)
      (delete-file outside))))

(provide 'test-mevedel-journal-evidence)
;;; test-mevedel-journal-evidence.el ends here
