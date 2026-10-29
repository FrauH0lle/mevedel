;;; test-mevedel-working-note.el --- Working-note codec tests -*- lexical-binding: t -*-

;;; Commentary:

;; Pure metadata validation, defaults, and cold loading.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-working-note)

(defconst test-mevedel-working-note--source-directory
  (file-name-directory (directory-file-name (file-name-directory
                                            (or load-file-name buffer-file-name))))
  "Source tree used by the cold codec subprocess.")

(mevedel-deftest mevedel-working-note-markdown-p ()
  ,test
  (test)
  (should (mevedel-working-note-markdown-p "topic.MD"))
  (should (mevedel-working-note-markdown-p "topic.md"))
  (dolist (path '(nil "topic.txt" "topic.md.json" "topic"))
    (should-not (mevedel-working-note-markdown-p path))))

(mevedel-deftest mevedel-working-note--timestamp-p ()
  ,test
  (test)
  :doc "UTC validation is independent of client timezone and rejects normalized dates"
  (let ((process-environment (copy-sequence process-environment)))
    (setenv "TZ" "Pacific/Honolulu")
    (should (mevedel-working-note--timestamp-p "2024-02-29T23:59:59Z"))
    (dolist (value '(nil "2025-02-29T00:00:00Z" "2026-10-29"
                         "2026-10-29T25:00:00Z" "2026-10-29T11:00:00+00:00"
                         "2026-10-29T11:00:00.1Z" "2026-10-29T11:00:00z"))
      (should-not (mevedel-working-note--timestamp-p value)))))

(mevedel-deftest mevedel-working-note--scalar ()
  ,test
  (test)
  (should (equal "a'b" (mevedel-working-note--scalar "'a''b'")))
  (should (equal "a\"b" (mevedel-working-note--scalar "\"a\\\"b\"")))
  (should (equal "/root/worker" (mevedel-working-note--scalar "/root/worker")))
  (dolist (value '("" "|" "[x]" "{x}" "'bad" "\"bad" "a #comment"))
    (should-error (mevedel-working-note--scalar value))))

(mevedel-deftest mevedel-working-note--parse ()
  ,test
  (test)
  (should (equal '(:body "---\nOrdinary Markdown\n---\nbody\n")
                 (mevedel-working-note--parse "---\nOrdinary Markdown\n---\nbody\n")))
  (should (equal '(:body "---\nNote: ordinary prose\n")
                 (mevedel-working-note--parse "---\nNote: ordinary prose\n")))
  (should (equal '(:body "---\n# Status notes\n\nstatus updates are documented below.\n")
                 (mevedel-working-note--parse "---\n# Status notes\n\nstatus updates are documented below.\n")))
  (should (eq 'handoff (plist-get (mevedel-working-note--parse
                                   "---\n# Lifecycle comment\nkind: handoff\n---\nbody\n") :kind)))
  (should-error (mevedel-working-note--parse "---\n# Lifecycle comment\nkind: invalid\n---\nbody\n"))
  (should (equal "body\n" (plist-get (mevedel-working-note--parse
                                     "---\nkind: handoff\n---\nbody\n") :body)))
  (dolist (header '("kind: invalid" "status: kept" "owner: ''" "owner: \"\\n\""
                    "created: yesterday" "title: unknown" "kind: scratch\nkind: report"
                    "status: open\nstatus: done" "owner: a\nowner: b"
                    "created: 2026-10-29T00:00:00Z\ncreated: 2026-10-29T00:00:00Z"))
    (should-error (mevedel-working-note--parse (concat "---\n" header "\n---\nbody\n"))))
  (should-error (mevedel-working-note--parse "---\nkind: scratch\n"))
  (should-error (mevedel-working-note--parse "---\nstatus: open\nkind scratch\n---\n"))
  (should-error (mevedel-working-note--parse (unibyte-string 0 255))))

(mevedel-deftest mevedel-working-note-classify ()
  ,test
  (test)
  :doc "Every lifecycle pair is managed; all non-Markdown content stays attachment"
  (progn
    (dolist (kind '(scratch handoff report))
      (dolist (status '(open done))
        (let* ((content (format "---\nkind: %s\nstatus: %s\nowner: 'a'\ncreated: '2026-10-29T00:00:00Z'\n---\n# Body\n"
                                kind status))
               (note (mevedel-working-note-classify "note.MD" content)))
          (should (eq 'managed (plist-get note :classification)))
          (should (eq kind (plist-get note :kind)))
          (should (eq status (plist-get note :status)))
          (should (equal "# Body\n" (plist-get note :body))))))
    (dolist (content '("# Headerless\n" "---\nkind: scratch\n---\n" "---\nstatus: invalid\n---\n"))
      (let ((note (mevedel-working-note-classify "x.md" content)))
        (should (eq 'unmanaged (plist-get note :classification)))
        (should (eq 'handoff (plist-get note :kind)))
        (should (eq 'open (plist-get note :status)))
        (should (stringp (plist-get note :diagnostic)))))
    (dolist (path '("x.json" "x.el" "x.txt" "x.png"))
      (dolist (content (list "UTF-8 text" (unibyte-string 0 255)))
        (should (equal '(:classification attachment)
                       (mevedel-working-note-classify path content)))))))

(mevedel-deftest mevedel-working-note-normalize ()
  ,test
  (test)
  :doc "Defaults, deliberate metadata, attribution preservation and external malformed headers"
  (let* ((date "2026-10-29T11:00:00Z")
         (plain "# Body\n")
         (default (mevedel-working-note-normalize plain "session/root" date))
         (managed (mevedel-working-note-normalize
                   "---\nkind: report\nstatus: done\nowner: author\ncreated: 2024-01-01T00:00:00Z\n---\nold\n"
                   "ignored" date)))
    (should (equal (concat "---\nkind: scratch\nstatus: open\nowner: \"session/root\"\ncreated: \"" date "\"\n---\n" plain)
                   default))
    (should (equal default (mevedel-working-note-normalize default "different" date default)))
    (let ((note (mevedel-working-note-classify
                 "x.md" (mevedel-working-note-normalize
                         "---\nstatus: open\n---\nnew\n" "new owner" date managed))))
      (should (eq 'report (plist-get note :kind)))
      (should (eq 'open (plist-get note :status)))
      (should (equal "author" (plist-get note :owner)))
      (should (equal "2024-01-01T00:00:00Z" (plist-get note :created))))
    (should (string-suffix-p "---\nOrdinary Markdown\n" (mevedel-working-note-normalize
                                                         "---\nOrdinary Markdown\n" "owner" date)))
    (should-error (mevedel-working-note-normalize "---\nkind: bad\n---\nnew\n" "owner" date))
    (should (string-suffix-p "new\n" (mevedel-working-note-normalize
                                      "---\nkind: bad\n---\nnew\n" "owner" date
                                      "---\nkind: bad\n---\nold\n")))
    (should-error (mevedel-working-note-normalize plain "" date))
    (should-error (mevedel-working-note-normalize plain "owner" "invalid"))))

(mevedel-deftest mevedel-working-note-cold-load ()
  ,test
  (test)
  :doc "The pure codec cold-loads without memory, sessions or provider runtime"
  (let ((default-directory test-mevedel-working-note--source-directory))
    (with-temp-buffer
      (should (zerop
               (call-process
                invocation-name nil t nil "-Q" "--batch" "-L" default-directory
                "--eval"
                "(progn (require 'mevedel-working-note) (when (or (featurep 'mevedel-memory-notes) (featurep 'mevedel-structs) (featurep 'gptel)) (kill-emacs 1)) (unless (eq 'managed (plist-get (mevedel-working-note-classify \"x.md\" (mevedel-working-note-normalize \"body\" \"owner\" \"2026-10-29T00:00:00Z\")) :classification)) (kill-emacs 2)))")))
      (should (string-empty-p (buffer-string))))))

(provide 'test-mevedel-working-note)
;;; test-mevedel-working-note.el ends here
