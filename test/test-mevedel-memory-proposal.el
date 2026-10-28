;;; test-mevedel-memory-proposal.el -- Consolidation output tests -*- lexical-binding: t -*-

;;; Commentary:

;; The consolidation reply must be wholly valid before it becomes proposals.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-memory-proposal)

(mevedel-deftest mevedel-memory-proposal/cold-compile-runtime-owner ()
  ,test
  (test)
  :doc "compiled proposal parser loads its runtime helper owner on first use"
  (let* ((root (file-name-directory (locate-library "mevedel-memory-proposal")))
         (directory (make-temp-file "mevedel-proposal-cold-" t))
         (source (file-name-concat directory "mevedel-memory-proposal.el"))
         (bytecode (file-name-concat directory "mevedel-memory-proposal.elc")))
    (unwind-protect
        (progn
          (copy-file (file-name-concat root "mevedel-memory-proposal.el") source)
          (with-temp-buffer
            (let ((status
                   (call-process
                    (expand-file-name invocation-name invocation-directory)
                    nil t nil "--batch" "-Q" "--eval"
                    (prin1-to-string
                     `(progn
                        (setq load-path ',load-path native-comp-jit-compilation nil)
                        (unless (and (not (featurep 'cl-lib))
                                     (not (fboundp 'cl-every)))
                          (error "Cold compiler child inherited cl-lib"))
                        (require 'bytecomp)
                        (when (or (featurep 'cl-lib) (fboundp 'cl-every))
                          (error "Compiler dependency loaded cl-lib"))
                        (let ((byte-compile-verbose nil)
                              (byte-compile-error-on-warn t))
                          (unless (byte-compile-file ,source)
                            (error "Proposal parser compilation failed"))))))))
              (ert-info ((buffer-string))
                (should (equal 0 status))
                (should (equal "" (buffer-string))))))
          (with-temp-buffer
            (let ((status
                   (call-process
                    (expand-file-name invocation-name invocation-directory)
                    nil t nil "--batch" "-Q" "--eval"
                    (prin1-to-string
                     `(progn
                        (setq load-path ',(cons directory load-path)
                              native-comp-jit-compilation nil)
                        (unless (and (not (featurep 'cl-lib))
                                     (not (fboundp 'cl-every)))
                          (error "Cold runtime child inherited cl-lib"))
                        (load ,bytecode nil t)
                        (unless (and (featurep 'cl-lib) (fboundp 'cl-every))
                          (error "Proposal parser did not load its runtime helper owner"))
                        (unless (mevedel-memory-proposal--file-p "cold.md")
                          (error "Cold parser first use failed")))))))
              (ert-info ((buffer-string))
                (should (equal 0 status))
                (should (equal "" (buffer-string)))))))
      (delete-directory directory t))))

(defconst mevedel-test-memory-proposal--roots
  '(("local-agents" :kind memory :files ("old.md" "duplicate.md")
     :existing ("MEMORY.md" "old.md" "duplicate.md" "unread.md") :complete t)
    ("instructions" :kind instructions :files ("AGENTS.md")))
  "Captured admitted files and a complete root name observation.")

(defconst mevedel-test-memory-proposal--reply
  "## Promote
````proposal
root: \"local-agents\"
file: \"retry.md\"
type: \"project\"
title: \"Retry deadlines\"
hook: \"Why retry deadlines use a monotonic clock\"
reason: \"Preserve the measured failure condition\"
evidence: [\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\"]
---
Use a monotonic deadline for elapsed-time retries.
````
## Update
- none
## Merge
- none
## Remove
- none
## Instructions
- none
## No action
- none"
  "One promotion with all required sections and an attributed source.")

(mevedel-deftest mevedel-memory-proposal-parse ()
  ,test
  (test)
  :doc "returns a complete promotion only within its captured roots and evidence"
  (let* ((id (make-string 64 ?a))
         (result (mevedel-memory-proposal-parse
                  mevedel-test-memory-proposal--reply
                  mevedel-test-memory-proposal--roots (list id)))
         (proposal (car (plist-get result :proposals))))
    (should (= 1 (length (plist-get result :proposals))))
    (should (eq 'promote (plist-get proposal :action)))
    (should (equal "retry.md" (plist-get proposal :file)))
    (should (equal (list id) (plist-get proposal :evidence)))
    (should (equal "Use a monotonic deadline for elapsed-time retries."
                   (plist-get proposal :body))))
  (dolist (change '(("local-agents" . "foreign-root")
                    ("retry.md" . "../outside.md")
                    ("retry.md" . "unread.md")
                    ("retry.md" . "MEMORY.md")
                    ("## Update" . "## Other")))
    (should-error
     (mevedel-memory-proposal-parse
      (string-replace (car change) (cdr change) mevedel-test-memory-proposal--reply)
      mevedel-test-memory-proposal--roots (list (make-string 64 ?a)))))
  (should-error
   (mevedel-memory-proposal-parse mevedel-test-memory-proposal--reply
                                 mevedel-test-memory-proposal--roots nil))

  :doc "scope diagnostics distinguish root, path, and evidence failures"
  (let* ((id (make-string 64 ?a))
         (encoded-id (json-serialize (vector id)))
         (cases `(("Unknown proposal root" "root: \"local-agents\"" "root: \"unknown-root\"")
                  ("Unsafe proposal file path" "file: \"retry.md\"" "file: \"../unsafe.md\"")
                  ("Proposal evidence must be an array" ,encoded-id "null")
                  ("Proposal evidence contains duplicate entries" ,encoded-id
                   ,(json-serialize (vector id id)))
                  ("Proposal evidence contains an invalid or unadmitted entry" ,encoded-id
                   ,(json-serialize (vector (make-string 64 ?b))))
                  ("Proposal evidence contains an invalid or unadmitted entry" ,encoded-id "[1]"))))
    (dolist (case cases)
      (let* ((changed (string-replace (nth 1 case) (nth 2 case)
                                      mevedel-test-memory-proposal--reply))
             (condition (should-error
                         (mevedel-memory-proposal-parse changed
                                                       mevedel-test-memory-proposal--roots
                                                       (list id)))))
        (should (equal (car case) (error-message-string condition))))))

  :doc "a no-action review needs no proposal targets or digest evidence"
  (let ((result
         (mevedel-memory-proposal-parse
          (concat "## Promote\n- none\n## Update\n- none\n## Merge\n- none\n"
                  "## Remove\n- none\n## Instructions\n- none\n## No action\n"
                  "- Existing memory already captures these facts.") nil nil)))
    (should-not (plist-get result :proposals))
    (should (equal "- Existing memory already captures these facts."
                   (plist-get result :no-action))))

  :doc "all actions retain complete bodies, shorter Markdown fences, and merge sources"
  (let* ((roots '(("memory" :kind memory
                  :files ("old.md" "a.md" "b.md" "gone.md")
                  :existing ("old.md" "a.md" "b.md" "gone.md") :complete t)
                 ("instructions" :kind instructions :files ("AGENTS.md"))))
         (reply
          (concat
           (mapconcat
            (lambda (item)
              (format
               (concat "## %s\n````proposal\nroot: %s\nfile: %s\ntype: \"project\"\n"
                       "title: \"Memory title\"\nhook: \"Index explanation\"\n"
                       "reason: \"Current evidence\"\nevidence: []\n%s---\n%s````\n")
               (nth 0 item) (json-serialize (nth 1 item)) (json-serialize (nth 2 item))
               (or (nth 4 item) "") (nth 3 item)))
            '(("Promote" "memory" "new.md" "New lesson.\n")
              ("Update" "memory" "old.md" "  Indented text.\n## Local detail\n```elisp\n(message \"example\")\n```\n")
              ("Merge" "memory" "combined.md" "Combined lesson.\n"
               "merged-files: [\"a.md\", \"b.md\"]\n")
              ("Remove" "memory" "gone.md" "")
              ("Instructions" "instructions" "AGENTS.md" "New contributor guidance.\n"))
            "")
           "## No action\n- none"))
         (proposals (plist-get (mevedel-memory-proposal-parse reply roots nil) :proposals)))
    (should (equal '(promote update merge remove instructions)
                   (mapcar (lambda (item) (plist-get item :action)) proposals)))
    (should (equal '("a.md" "b.md") (plist-get (nth 2 proposals) :merged-files)))
    (should (string-prefix-p "  Indented text.\n## Local detail\n```elisp"
                            (plist-get (nth 1 proposals) :body)))
    (should (equal "" (plist-get (nth 3 proposals) :body)))
    (dolist (change '(("new.md" . "old.md")
                      ("combined.md" . "old.md")
                      ("a.md" . "outside.md")
                      ("AGENTS.md" . "unrelated.md")
                      ("reason: \"Current evidence\"" . "reason: null")))
      (should-error
       (mevedel-memory-proposal-parse
        (string-replace (car change) (cdr change) reply) roots nil))))

  :doc "malformed or partially valid replies never yield partial proposals"
  (dolist (change '(("root: \"local-agents\"\n" . "")
                    ("root: \"local-agents\"" . "root: \"local-agents\"\nroot: \"local-agents\"")
                    ("reason:" . "unexpected:")
                    ("type: \"project\"" . "type: \"unknown\"")
                    ("hook: \"Why retry deadlines use a monotonic clock\"" . "hook: \"first\\nsecond\"")
                    ("---\nUse a monotonic deadline for elapsed-time retries.\n" . "---\n")
                    ("## Merge" . "## Update")
                    ("## Promote" . "## promote")
                    ("````proposal" . "````PROPOSAL")
                    ("- none\n## Merge" . "\n## Merge")
                    ("````\n## Update" . "```\n## Update")
                    ("---\nUse" . "merged-files: []\n---\nUse")))
    (should-error
     (mevedel-memory-proposal-parse
      (string-replace (car change) (cdr change) mevedel-test-memory-proposal--reply)
      mevedel-test-memory-proposal--roots (list (make-string 64 ?a)))))
  (dolist (suffix '("\nExtra prose" "\n## No action\n- duplicated"))
    (should-error
     (mevedel-memory-proposal-parse
      (concat mevedel-test-memory-proposal--reply suffix)
      mevedel-test-memory-proposal--roots (list (make-string 64 ?a)))))

  :doc "creation needs complete name coverage and targets cannot escape their roots"
  (let ((roots (copy-tree mevedel-test-memory-proposal--roots)))
    (setf (plist-get (cdar roots) :complete) nil)
    (should-error
     (mevedel-memory-proposal-parse mevedel-test-memory-proposal--reply
                                   roots (list (make-string 64 ?a)))))
  (dolist (file '("/outside.md" "~/outside.md" "a/../outside.md"
                  "a//outside.md" "./outside.md" "a\\outside.md"
                  "a:outside.md" "a\noutside.md" "topic.el"))
    (should-error
     (mevedel-memory-proposal-parse
      (string-replace "file: \"retry.md\"" (concat "file: " (json-serialize file))
                      mevedel-test-memory-proposal--reply)
      mevedel-test-memory-proposal--roots (list (make-string 64 ?a)))))

  :doc "duplicate evidence and oversized UTF-8 replies are rejected"
  (let ((id (make-string 64 ?a)))
    (dolist (encoded (list "null" "false" "\"not-an-array\""
                          (json-serialize (vector id id))))
      (should-error
       (mevedel-memory-proposal-parse
        (string-replace (json-serialize (vector id)) encoded
                        mevedel-test-memory-proposal--reply)
        mevedel-test-memory-proposal--roots (list id)))))
  (should-error
   (mevedel-memory-proposal-parse
    (string-replace "Use a monotonic deadline for elapsed-time retries."
                    (make-string 20000 ?\u00e9) mevedel-test-memory-proposal--reply)
    mevedel-test-memory-proposal--roots (list (make-string 64 ?a)))))

(mevedel-deftest mevedel-memory-proposal-relative-path-p
  (:doc "accepts plain relative paths and rejects absolute, home, control, colon and traversal spellings")
  (progn
    (dolist (path '("topic.md" "." "nested/dir/file.md"))
      (should (mevedel-memory-proposal-relative-path-p path)))
    (dolist (path '("" "/etc/passwd" "~/x" "a:b" "a\nb" "../x" "a/../b" nil))
      (should-not (mevedel-memory-proposal-relative-path-p path)))))

(mevedel-deftest mevedel-memory-proposal-parse/notes ()
  ,test
  (test)
  :doc "admitted notes need exactly one disposition and may be cited as evidence"
  (let* ((notes '(("work://shared/a.md" "notes-root" . "a.md")
                  ("work://shared/b.md" "notes-root" . "b.md")))
         (reply (lambda (dispositions)
                  (concat (string-replace
                           "[\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\"]"
                           "[\"work://shared/a.md\"]"
                           (string-remove-suffix "## No action\n- none" mevedel-test-memory-proposal--reply))
                          "## Notes\n" dispositions "## No action\n- none")))
         (block (lambda (note disposition &optional body)
                  (format "````proposal\nnote: %S\ndisposition: %S\nreason: \"Reviewed\"\n%s---\n%s````\n"
                          note disposition
                          (if (equal disposition "delete") "open-items: []\nresolved-by: []\n" "")
                          (if body (concat body "\n") ""))))
         (result (mevedel-memory-proposal-parse
                  (funcall reply (concat (funcall block "work://shared/a.md" "delete")
                                         (funcall block "work://shared/b.md" "condense" "Open: verify.")))
                  mevedel-test-memory-proposal--roots nil notes))
         (proposals (plist-get result :proposals)))
    (should (equal '(promote note-delete note-condense) (mapcar (lambda (item) (plist-get item :action)) proposals)))
    (should (equal '("work://shared/a.md") (plist-get (car proposals) :evidence)))
    (should (equal '("notes-root" . "b.md")
                   (cons (plist-get (nth 2 proposals) :root) (plist-get (nth 2 proposals) :file))))
    (should (equal "Open: verify." (plist-get (nth 2 proposals) :body)))
    (dolist (invalid (list (funcall block "work://shared/a.md" "delete")
                           (concat (funcall block "work://shared/a.md" "delete")
                                   (funcall block "work://shared/a.md" "keep")
                                   (funcall block "work://shared/b.md" "keep"))
                           (concat (funcall block "work://shared/a.md" "archive")
                                   (funcall block "work://shared/b.md" "keep"))
                           (concat (funcall block "work://shared/a.md" "delete" "Unexpected body")
                                   (funcall block "work://shared/b.md" "keep"))
                           (concat (funcall block "work://shared/a.md" "delete")
                                   (funcall block "work://shared/b.md" "condense"))
                           (concat (funcall block "work://shared/c.md" "delete")
                                   (funcall block "work://shared/b.md" "keep"))
                           "- none\n"))
      (should-error (mevedel-memory-proposal-parse (funcall reply invalid)
                                                   mevedel-test-memory-proposal--roots nil notes))))
  :doc "a delete accounts for its open items with distinct quotes from safe normalized workspace paths"
  (let* ((notes '(("work://shared/a.md" "notes-root" . "a.md")))
         (parse (lambda (disposition fields)
                  (mevedel-memory-proposal-parse
                   (concat "## Promote\n- none\n## Update\n- none\n## Merge\n- none\n## Remove\n- none\n"
                           "## Instructions\n- none\n## Notes\n````proposal\nnote: \"work://shared/a.md\"\n"
                           (format "disposition: %S\nreason: \"Reviewed\"\n%s---\n````\n" disposition fields)
                           "## No action\n- none")
                   mevedel-test-memory-proposal--roots nil notes)))
         (item (car (plist-get (funcall parse "delete"
                                        (concat "open-items: [\"Finding 2\"]\nresolved-by: "
                                                "[\"./docs/adr/x.md: Restoration keeps one anchor\", "
                                                "\"a/./c.el: (defun keep-the-row: here)\"]\n"))
                               :proposals))))
    (should (equal '("Finding 2") (plist-get item :open-items)))
    ;; Comparison with the review's reads uses one canonical path spelling;
    ;; the first colon separates, so quotes may contain colons.
    (should (equal '(("docs/adr/x.md" . "Restoration keeps one anchor")
                     ("a/c.el" . "(defun keep-the-row: here)"))
                   (plist-get item :resolved-by)))
    (should-not (plist-member (car (plist-get (funcall parse "keep" "") :proposals)) :open-items))
    ;; Missing resolution evidence is none, so this delete can only be kept.
    (should-not (plist-get (car (plist-get (funcall parse "delete" "open-items: [\"A\"]\n") :proposals))
                           :resolved-by))
    (dolist (invalid '(("delete" "")
                       ("delete" "resolved-by: []\n")
                       ("delete" "open-items: \"Finding 2\"\nresolved-by: []\n")
                       ("delete" "open-items: [\"\"]\nresolved-by: []\n")
                       ("delete" "open-items: [\"A\", \"A\"]\nresolved-by: []\n")
                       ;; A bare path is no longer evidence.
                       ("delete" "open-items: [\"A\"]\nresolved-by: [\"docs/backlog.md\"]\n")
                       ("delete" "open-items: [\"A\"]\nresolved-by: [\"docs/backlog.md: too short\"]\n")
                       ("delete" "open-items: [\"A\"]\nresolved-by: [\"/etc/passwd: root entry for the system\"]\n")
                       ("delete" "open-items: [\"A\"]\nresolved-by: [\"../outside.md: escaping quote text\"]\n")
                       ("delete" "open-items: [\"A\"]\nresolved-by: [\"a/b/../c.el: escaping quote text\"]\n")
                       ("delete" "open-items: [\"A\"]\nresolved-by: [\"~/notes.md: home quote text here\"]\n")))
      (should-error (funcall parse (car invalid) (cadr invalid)))))
  :doc "real reply shapes rejected in production parse without loosening dispositions"
  ;; Shapes from real Codex gpt-6-luna replies, 2026-10-28: keep and condense
  ;; carrying open items, a header-only delete without separator, and a reply
  ;; that ends after its last proposal fence.
  (let* ((notes '(("work://shared/a.md" "notes-root" . "a.md")
                  ("work://shared/b.md" "notes-root" . "b.md")
                  ("work://shared/c.md" "notes-root" . "c.md")))
         (head "## Promote\n- none\n## Update\n- none\n## Merge\n- none\n## Remove\n- none\n## Instructions\n- none\n## Notes\n")
         (blocks (concat "````proposal\nnote: \"work://shared/a.md\"\ndisposition: \"keep\"\nreason: \"Open handoff\"\n"
                         "open-items: [\"Duration undecided\"]\n---\n````\n"
                         "````proposal\nnote: \"work://shared/b.md\"\ndisposition: \"condense\"\nreason: \"Partly open\"\n"
                         "open-items: [\"Port the fix\"]\nresolved-by: [\"docs/backlog.md\"]\n---\nOpen: port the fix.\n````\n"
                         "````proposal\nnote: \"work://shared/c.md\"\ndisposition: \"delete\"\nreason: \"Completed\"\n"
                         "open-items: []\nresolved-by: []\n````\n"))
         (parse (lambda (text) (mevedel-memory-proposal-parse text mevedel-test-memory-proposal--roots nil notes)))
         (result (funcall parse (concat head blocks))))
    (should (equal "- none" (plist-get result :no-action)))
    (should (equal '(note-keep note-condense note-delete)
                   (mapcar (lambda (item) (plist-get item :action)) (plist-get result :proposals))))
    (should (equal '("Duration undecided") (plist-get (car (plist-get result :proposals)) :open-items)))
    (should (equal "" (plist-get (nth 2 (plist-get result :proposals)) :body)))
    (should (funcall parse (concat head blocks "## No action\n- Nothing further.\n")))
    ;; Fence spellings from real replies: a four-dash separator, a shorter
    ;; closing fence, and a missing close before the next empty-body block.
    (dolist (variant (list (string-replace "Open handoff\"\nopen-items: [\"Duration undecided\"]\n---\n````"
                                           "Open handoff\"\nopen-items: [\"Duration undecided\"]\n----\n```" blocks)
                           (string-replace "Open handoff\"\nopen-items: [\"Duration undecided\"]\n---\n````\n"
                                           "Open handoff\"\nopen-items: [\"Duration undecided\"]\n---\n" blocks)))
      (should (equal '(note-keep note-condense note-delete)
                     (mapcar (lambda (item) (plist-get item :action))
                             (plist-get (funcall parse (concat head variant)) :proposals)))))
    ;; A Markdown code block inside a condensed body survives intact.
    (let ((coded (funcall parse (concat head (string-replace "Open: port the fix.\n" "Open:\n```\n(fix)\n```\n" blocks)))))
      (should (equal "Open:\n```\n(fix)\n```" (plist-get (nth 1 (plist-get coded :proposals)) :body))))
    (dolist (invalid
             (list
              ;; An unterminated non-empty body is ambiguous.
              (concat head (string-replace "Open: port the fix.\n````\n" "Open: port the fix.\n" blocks))
              ;; Analysis between blocks remains invalid.
              (concat head "- Note a.md open items:\n  - Duration undecided\n" blocks)
              ;; Without its separator a condense has no body.
              (concat head (string-replace "---\nOpen: port the fix.\n" "" blocks))
              ;; A delete still needs its open-item accounting.
              (concat head (string-replace "open-items: []\nresolved-by: []\n" "" blocks))
              ;; Truncation before every note is disposed of.
              (concat head (substring blocks 0 (string-search "````proposal\nnote: \"work://shared/c.md\"" blocks)))
              ;; Malformed open items on keep are still rejected.
              (concat head (string-replace "[\"Duration undecided\"]" "\"Duration undecided\"" blocks))))
      (should-error (funcall parse invalid))))
  :doc "resolution quotes match delivered Read text across line numbers and wrapping only"
  (let ((read "  352\t- **One viewport anchor per window (2026-10-18).** Logical window-start\n  353\t  restoration previously overrode\n"))
    (should (mevedel-memory-proposal-quoted-p "window (2026-10-18).** Logical window-start restoration previously" read))
    (should (mevedel-memory-proposal-quoted-p "One viewport anchor per window (2026-10-18). Logical" read))
    ;; Citation decoration around quoted words is ignored; each quoted part counts.
    (should (mevedel-memory-proposal-quoted-p "\"Logical window-start restoration\" (ADR 0126, lines 352-353)" read))
    (should-not (mevedel-memory-proposal-quoted-p "\"Logical window-start\" and \"was later removed\" (ADR 0126)" read))
    (should-not (mevedel-memory-proposal-quoted-p "\"window\" (ADR 0126, lines 352-353)" read))
    (should-not (mevedel-memory-proposal-quoted-p "window start restoration was removed" read))
    (should-not (mevedel-memory-proposal-quoted-p "353 restoration previously" read)))
  :doc "without admitted notes the section is optional and may only be empty"
  (progn
    (should (mevedel-memory-proposal-parse
             (concat (string-remove-suffix "## No action\n- none" mevedel-test-memory-proposal--reply)
                     "## Notes\n- none\n## No action\n- none")
             mevedel-test-memory-proposal--roots (list (make-string 64 ?a))))
    (should-error (mevedel-memory-proposal-parse
                   (concat (string-remove-suffix "## No action\n- none" mevedel-test-memory-proposal--reply)
                           "## Notes\n````proposal\nnote: \"work://shared/a.md\"\ndisposition: \"delete\"\n"
                           "reason: \"Reviewed\"\n---\n````\n## No action\n- none")
                   mevedel-test-memory-proposal--roots (list (make-string 64 ?a))))))

(mevedel-deftest mevedel-memory-proposal-condensed-bytes
  (:doc "condensed notes end in exactly one newline and encode as UTF-8")
  (progn
    (should (equal "Open.\n" (mevedel-memory-proposal-condensed-bytes "Open.\n\n\n")))
    (should (equal (encode-coding-string "界\n" 'utf-8) (mevedel-memory-proposal-condensed-bytes "界")))))

(provide 'test-mevedel-memory-proposal)
;;; test-mevedel-memory-proposal.el ends here
