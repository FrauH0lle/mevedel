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
(require 'mevedel-transcript)

(mevedel-deftest mevedel-journal-evidence--truncate ()
  ,test
  (test)
  :doc "bounds large multibyte strings at character boundaries, including the marker"
  (let* ((source (concat (make-string 100000 #x754c) "end"))
         (result (mevedel-journal-evidence--truncate source 32768 "[omitted]")))
    (should (<= (string-bytes result) 32768))
    (should (string-prefix-p (make-string 100 #x754c) result))
    (should (string-suffix-p "[omitted]" result))
    (should (= (% (- (string-bytes result) (string-bytes "[omitted]")) 3) 0))
    (should-not (cl-some (lambda (char) (eq (char-charset char) 'eight-bit)) result)))

  :doc "keeps exact fits unchanged and rejects a marker beyond the allowance"
  (should (equal "abc" (mevedel-journal-evidence--truncate "abc" 3 "...")))
  (should (equal "a..." (mevedel-journal-evidence--truncate "abcdef" 4 "...")))
  (should-error (mevedel-journal-evidence--truncate "abcdef" 2 "...")))

(mevedel-deftest mevedel-journal-evidence--turn ()
  ,test
  (test)
  :doc "keeps dialogue and decisions while bounding tools and excluding system observations"
  (with-temp-buffer
    (org-mode)
    (insert "User: use the private runner.\n"
            (mevedel--format-hook-audit-record
             '(:type injected-reminders :phase turn-start
               :items ((:type fixture :body "Full workspace observation must not enter the digest."))))
            (propertize "Assistant: I will isolate state.\n" 'gptel 'response))
    (dolist (name '("Read" "Skill"))
      (insert "#+begin_tool\n")
      (let ((start (point)))
        (insert (format "(:name %S :args (:name \"fixture\"))\n\n" name)
                "Observed tool result.\n" (make-string 10000 ?x))
        (put-text-property start (point) 'gptel (cons 'tool name)))
      (insert "\n#+end_tool\n"))
    (insert "User correction: batch operations first.\n"
            (propertize "Final decision: a helper is acceptable if batching fails.\n" 'gptel 'response))
    (let* ((full (mevedel-transcript-project-evidence (list (cons (point-min) (point-max)))))
           (compact (mevedel-journal-evidence--turn (point-min) (point-max)))
           (dialogue (car compact))
           (supporting (cdr compact)))
      (should (string-match-p "Full workspace observation" full))
      (should (string-match-p "User: use the private runner" dialogue))
      (should (string-match-p "User correction: batch operations first" dialogue))
      (should (string-match-p "Final decision: a helper is acceptable" dialogue))
      (should (string-match-p "provenance: assistant" dialogue))
      (should-not (string-match-p "Full workspace observation" (concat dialogue supporting)))
      (should (string-match-p "Observed tool result" supporting))
      (should (string-match-p "digest item limit" supporting))
      (should (<= (string-bytes supporting) 4097))
      (should (< (string-bytes (concat dialogue supporting)) (string-bytes full))))))

(mevedel-deftest mevedel-journal-evidence-digest-source ()
  ,test
  (test)
  :doc "keeps small evidence unchanged, without interpreting quoted delimiters"
  (let ((source (concat "Completed turn 1; source: segment-0001.chat.org\n"
                        "--- evidence item; provenance: user ---\n"
                        "Keep this quoted tool-result delimiter literally.\n"
                        "--- evidence item; provenance: tool-result ---\n"
                        "Assistant decision: use the isolated runner.\n")))
    (should (equal source (mevedel-journal-evidence-digest-source source))))

  :doc "bounds multibyte evidence and retains opening intent and final decisions"
  (let* ((opening "Completed turn 1; source: segment-0001.chat.org\nUser: keep the local runner.\n")
         (closing "\nCompleted turn 2; source: segment-0001.chat.org\nAssistant: decided to use the local runner because it isolates state.\n")
         (source (concat opening (make-string 100000 #x754c) closing))
         (copy (copy-sequence source))
         (compact (mevedel-journal-evidence-digest-source source)))
    (should (> (string-bytes source) mevedel-journal-evidence-digest-max-bytes))
    (should (<= (string-bytes compact) mevedel-journal-evidence-digest-max-bytes))
    (should (string-prefix-p opening compact))
    (should (string-suffix-p closing compact))
    (should (string-match-p "middle of capture unavailable" compact))
    (should (equal source copy))
    (should-not (cl-some (lambda (char) (eq (char-charset char) 'eight-bit)) compact))))

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
      (delete-file outside)))

  :doc "shared working files stay out of the snapshot while session notes remain"
  (let* ((root (make-temp-file "mevedel-journal-notes-shared-" t))
         (session-dir (file-name-concat root "session"))
         (shared (file-name-concat root ".mevedel" "shared")))
    (unwind-protect
        (progn
          (make-directory (file-name-concat session-dir "local") t)
          (make-directory shared t)
          (write-region "Session finding.\n" nil (file-name-concat session-dir "local" "notes.md") nil 'silent)
          (write-region "Shared finding from another session.\n" nil (file-name-concat shared "note.md") nil 'silent)
          (let ((snapshot (mevedel-journal-evidence-notes
                           (mevedel-session--create :save-path session-dir
                                                    :workspace (mevedel-workspace--create :root root)))))
            (should (string-match-p "Session finding" snapshot))
            (should-not (string-match-p "Shared finding\\|work://shared" snapshot))))
      (delete-directory root t))))

(provide 'test-mevedel-journal-evidence)
;;; test-mevedel-journal-evidence.el ends here
