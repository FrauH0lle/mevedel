;;; test-mevedel-memory-proposal.el -- Consolidation output tests -*- lexical-binding: t -*-

;;; Commentary:

;; Note failures are isolated without weakening memory or instruction checks.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-memory-proposal)
(require 'mevedel-memory-store)

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

(mevedel-deftest mevedel-memory-proposal--note-claims
  (:doc "ownership ignores body and multiline JSON content but reserves every explicit ID")
  (progn
    (should (equal '("a" "b")
                   (mevedel-memory-proposal--note-claims
                    "future: {\n  \"note\": \"unadmitted\"\n}\nnote: \"a\"\nnote: \"a\"\nnote: \"b\"\n---\nnote: \"body\"")))
    (should (equal '("a") (mevedel-memory-proposal--note-claims "reason: invalid-json\nnote: \"a\"\n")))
    (dolist (header '("note: \"a\"\nfuture: [\n  invalid" "note: \"a\"\ninvalid header"
                      "note: \"a\"\nopen-items: [\"Finding\"]}"))
      (should (equal '("a") (mevedel-memory-proposal--note-claims header))))
    (dolist (header '("note: null" "note: invalid-json" "reason: \"No identity\""))
      (should-error (mevedel-memory-proposal--note-claims header)))))

(mevedel-deftest mevedel-memory-proposal-parse/notes ()
  ,test
  (test)
  :doc "valid dispositions survive unrelated omitted, invalid or unknown notes"
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
                          (cond ((equal disposition "delete") "open-items: []\nresolved-by: []\n")
                                ((equal disposition "keep") "open-items: [\"Verify the retry deadline under clock changes\"]\n")
                                (t ""))
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
    (dolist (case (list (list (funcall block "work://shared/a.md" "delete") '(note-delete) '("work://shared/b.md"))
                       (list (concat (funcall block "work://shared/a.md" "delete")
                                     (funcall block "work://shared/a.md" "keep")
                                     (funcall block "work://shared/b.md" "keep")) '(note-keep) '("work://shared/a.md"))
                       (list (concat (funcall block "work://shared/a.md" "archive")
                                     (funcall block "work://shared/b.md" "keep")) '(note-keep) '("work://shared/a.md"))
                       (list (concat (funcall block "work://shared/a.md" "delete" "Unexpected body")
                                     (funcall block "work://shared/b.md" "keep")) '(note-keep) '("work://shared/a.md"))
                       (list (concat (funcall block "work://shared/a.md" "delete")
                                     (funcall block "work://shared/b.md" "condense")) '(note-delete) '("work://shared/b.md"))
                       (list (concat (funcall block "work://shared/c.md" "delete")
                                     (funcall block "work://shared/b.md" "keep")) '(note-keep) '("work://shared/a.md"))
                       (list "- none\n" nil '("work://shared/a.md" "work://shared/b.md"))))
      (let ((partial (mevedel-memory-proposal-parse (funcall reply (car case))
                                                    mevedel-test-memory-proposal--roots nil notes)))
        (should (equal (cons 'promote (cadr case))
                       (mapcar (lambda (item) (plist-get item :action)) (plist-get partial :proposals))))
        (should (equal (nth 2 case) (mapcar #'car (plist-get partial :skipped-notes))))
        (should (string-search "Note NO OP" (plist-get partial :no-action))))))
  :doc "generic per-note recovery preserves unrelated proposals and store reparse"
  (let* ((notes '(("work://shared/a.md" "notes-root" . "a.md")
                  ("work://shared/b.md" "notes-root" . "b.md")))
         (head (string-remove-suffix "## No action\n- none" mevedel-test-memory-proposal--reply))
         (block (lambda (id)
                  (format "````proposal\nnote: %S\ndisposition: \"delete\"\nreason: \"Done\"\nopen-items: []\n---\n````\n" id)))
         (a (funcall block "work://shared/a.md"))
         (b (funcall block "work://shared/b.md"))
         (parse (lambda (blocks)
                  (mevedel-memory-proposal-parse (concat head "## Notes\n" blocks)
                                                mevedel-test-memory-proposal--roots (list (make-string 64 ?a)) notes))))
    (dolist (bad (list (string-replace "disposition: \"delete\"\n" "" a)
                      (string-replace "disposition: \"delete\"" "disposition: null" a)
                      (string-replace "reason: \"Done\"" "reason: null" a)
                      (string-replace "reason: \"Done\"" "reason: invalid-json" a)
                      (string-replace "reason: \"Done\"\n" "" a)
                      (string-replace "reason: \"Done\"" "future-field: {\"note\": \"not-a-target\"}" a)
                      (string-replace "reason: \"Done\"" "future_field: true" a)
                      (string-replace "reason: \"Done\"" "futureField: true" a)
                      (string-replace "open-items: []" "open-items: [7]" a)))
      (let* ((text (concat head "## Notes\n" bad b))
             (result (funcall parse (concat bad b))))
        (should (equal '(promote note-delete)
                       (mapcar (lambda (item) (plist-get item :action)) (plist-get result :proposals))))
        (should (equal "work://shared/b.md" (plist-get (nth 1 (plist-get result :proposals)) :note)))
        (should (equal '("work://shared/a.md") (mapcar #'car (plist-get result :skipped-notes))))
        (should (equal result (funcall parse (concat bad b))))
        ;; Exercise the terminal store's actual reparse boundary without
        ;; publishing a review: only persistence after parsing is intercepted.
        (let* ((scope (list :roots (append mevedel-test-memory-proposal--roots
                                           '(("notes-root" :kind notes
                                              :notes (("a.md" :id "work://shared/a.md")
                                                      ("b.md" :id "work://shared/b.md")))))))
               (prepared (list :scope scope))
               captured)
          (cl-letf (((symbol-function 'mevedel-memory-store-accept-proposals)
                     (lambda (_workspace _prepared parsed &rest _args) (setq captured parsed))))
            (mevedel-memory-store-accept nil prepared text
                                         (list (list :id (make-string 64 ?a))) "test" nil (mapcar #'car notes)))
          (should (equal (plist-get result :proposals) (plist-get captured :proposals)))
          (should (equal (plist-get result :skipped-notes) (plist-get captured :skipped-notes)))
          (should (equal (plist-get result :no-action) (plist-get captured :no-action))))))
    ;; Invalid attempts reserve their targets, independent of block order.
    (let ((bad (string-replace "reason: \"Done\"" "future-field: true" a)))
      (dolist (blocks (list (concat bad a b) (concat a bad b) (concat a a b)
                            (concat (string-replace "note: \"work://shared/a.md\""
                                                    "note: \"work://shared/a.md\"\nnote: \"unadmitted/a.md\"" a) a b)))
        (let ((result (funcall parse blocks)))
          (should (equal '(promote note-delete)
                         (mapcar (lambda (item) (plist-get item :action)) (plist-get result :proposals))))
          (should (equal "work://shared/b.md" (plist-get (nth 1 (plist-get result :proposals)) :note)))
          (should (equal '("work://shared/a.md") (mapcar #'car (plist-get result :skipped-notes)))))))
    ;; A differing admitted identity claims both notes; neither may slip
    ;; through a later valid block.  Unadmitted basename matches claim neither.
    (let ((result (funcall parse (concat (string-replace "note: \"work://shared/a.md\""
                                                        "note: \"work://shared/a.md\"\nnote: \"work://shared/b.md\"" a) a b))))
      (should (equal '(promote) (mapcar (lambda (item) (plist-get item :action)) (plist-get result :proposals))))
      (should (= 2 (length (plist-get result :skipped-notes)))))
    (let ((result (funcall parse (concat (funcall block "foreign/a.md") b))))
      (should (equal '("work://shared/a.md") (mapcar #'car (plist-get result :skipped-notes))))
      (should (equal "work://shared/b.md" (plist-get (nth 1 (plist-get result :proposals)) :note)))
      (should (= 2 (plist-get result :note-format-failure-count)))
      (should (string-search "1 unadmitted note blocks skipped" (plist-get result :no-action))))
    (let ((result (funcall parse (concat a (funcall block "foreign/a.md") b))))
      (should-not (plist-get result :skipped-notes))
      (should (= 1 (plist-get result :note-format-failure-count)))
      (should (equal '("work://shared/a.md" "work://shared/b.md")
                     (mapcar (lambda (item) (plist-get item :note)) (cdr (plist-get result :proposals))))))
    (let ((result (mevedel-memory-proposal-parse head mevedel-test-memory-proposal--roots
                                                (list (make-string 64 ?a)) notes)))
      (should (= 2 (length (plist-get result :skipped-notes))))
      (should (equal '(promote) (mapcar (lambda (item) (plist-get item :action)) (plist-get result :proposals)))))
    ;; Partial note recovery never admits a malformed memory proposal, an
    ;; unauthorized instruction target, or ambiguous structural ownership.
    (dolist (text (list (string-replace "root: \"local-agents\"" "root: \"foreign\"" (concat head "## Notes\n" a b))
                       (string-replace "## Instructions\n- none\n"
                                       "## Instructions\n````proposal\nroot: \"instructions\"\nfile: \"outside.md\"\ntype: \"project\"\ntitle: \"X\"\nhook: \"X\"\nreason: \"X\"\nevidence: []\n---\nBad.\n````\n"
                                       (concat head "## Notes\n" a b))
                       (concat head "## Notes\n" (string-replace "note: \"work://shared/a.md\"" "note: null" a) b)))
      (should-error (mevedel-memory-proposal-parse text mevedel-test-memory-proposal--roots
                                                  (list (make-string 64 ?a)) notes))))
  :doc "bounded fence recovery never promotes body-shaped metadata to another target"
  (let* ((notes '(("work://shared/a.md" "notes-root" . "a.md")
                  ("work://shared/b.md" "notes-root" . "b.md")
                  ("work://shared/c.md" "notes-root" . "c.md")))
         (head (string-remove-suffix "## No action\n- none" mevedel-test-memory-proposal--reply))
         (block (lambda (id disposition &optional body)
                  (format "````proposal\nnote: %S\ndisposition: %S\nreason: \"Reviewed\"\nopen-items: [\"Check activation\"]\n---\n%s````\n"
                          id disposition (if body (concat body "\n") ""))))
         (good (funcall block "work://shared/a.md" "keep"))
         (condense (funcall block "work://shared/b.md" "condense" "Open: confirm activation."))
         (last (funcall block "work://shared/c.md" "keep"))
         (parse (lambda (blocks &optional prefix)
                  (mevedel-memory-proposal-parse (concat (or prefix head) "## Notes\n" blocks "## No action\n- Reviewed.\n")
                                                mevedel-test-memory-proposal--roots (list (make-string 64 ?a)) notes))))
    ;; Literal trailing spaces and tabs on openers and closers carry no data.
    (let* ((blocks (replace-regexp-in-string "^````\\(proposal\\)?$" "````\\1 \t" (concat good condense last)))
           (result (funcall parse blocks)))
      (should (= 0 (plist-get result :note-format-failure-count)))
      (should (equal '(promote note-keep note-condense note-keep)
                     (mapcar (lambda (item) (plist-get item :action)) (plist-get result :proposals))))
      (should (equal "Open: confirm activation." (plist-get (nth 2 (plist-get result :proposals)) :body))))
    ;; Non-target JSON syntax failures are local, including trailing garbage
    ;; after an otherwise complete array and an array that never completes.
    (dolist (field '("open-items: [\"Check activation\"]}" "open-items: ["
                     "open-items: [\n  \"Check activation\"\n]}" "open-items: invalid-json"))
      (let ((result (funcall parse (concat (string-replace "open-items: [\"Check activation\"]" field good) condense last))))
        (should (= 1 (plist-get result :note-format-failure-count)))
        (should (equal '("work://shared/a.md") (mapcar #'car (plist-get result :skipped-notes))))
        (should (equal '(promote note-condense note-keep)
                       (mapcar (lambda (item) (plist-get item :action)) (plist-get result :proposals))))))
    ;; A final No action heading or apparent complete next proposal cannot
    ;; terminate a nonempty body.  Ambiguous same-width openers quarantine
    ;; the tail rather than borrowing another note's closer.
    (dolist (tail (list "Open: unfinished.\n"
                        "Open: unfinished.\n## No action\n- Not a boundary.\n"
                        (concat "Open: unfinished.\n" (funcall block "work://shared/c.md" "delete"))
                        (concat "Open: unfinished.\n"
                                (string-remove-suffix "````\n" (funcall block "work://shared/c.md" "delete")))))
      (let* ((unclosed (string-replace "Open: confirm activation.\n````\n" tail condense))
             (result (funcall parse (concat good unclosed))))
        (should (= 2 (plist-get result :note-format-failure-count)))
        (should (equal '("work://shared/b.md" "work://shared/c.md") (mapcar #'car (plist-get result :skipped-notes))))
        (should (equal '(promote note-keep)
                       (mapcar (lambda (item) (plist-get item :action)) (plist-get result :proposals))))))
    ;; Plain header-like lines, and correctly narrower Markdown fences, are
    ;; ordinary body bytes when an unambiguous outer closer is present.
    (dolist (body '("Open.\n## No action\nnote: \"work://shared/c.md\"\ndisposition: \"delete\"\n---"
                    "Open.\n```proposal\nnote: \"work://shared/c.md\"\ndisposition: \"delete\"\n---\n```"
                    "```\nOpen: confirm activation.\n```"
                    "\n``` \t\nOpen: confirm activation.\n```"))
      (let ((result (funcall parse (concat good (funcall block "work://shared/b.md" "condense" body) last))))
        (should (= 0 (plist-get result :note-format-failure-count)))
        (should (equal body (plist-get (nth 2 (plist-get result :proposals)) :body)))
        (should (eq 'note-keep (plist-get (nth 3 (plist-get result :proposals)) :action)))))
    ;; Reordered action sections hidden in an unclosed body cannot be
    ;; claimed as validated memory sections: structural ambiguity still fails.
    (should-error
     (funcall parse (concat good (string-remove-suffix "````\n" condense) "## Instructions\n- none\n")
              (string-replace "## Instructions\n- none\n" "" head)))
    ;; Note recovery never extends to malformed memory/instruction metadata.
    (dolist (text (list (string-replace "evidence: [" "evidence: }[" (concat head "## Notes\n" good condense last))
                       (string-replace "## Instructions\n- none\n"
                                       "## Instructions\n````proposal\nroot: \"instructions\"\nfile: \"AGENTS.md\"\ntype: \"project\"\ntitle: \"X\"\nhook: \"X\"\nreason: }\nevidence: []\n---\nBad.\n````\n"
                                       (concat head "## Notes\n" good condense last))))
      (should-error (mevedel-memory-proposal-parse text mevedel-test-memory-proposal--roots
                                                  (list (make-string 64 ?a)) notes))))
  :doc "five bodyless dispositions can omit closers before openers and final No action"
  (let* ((ids '("a" "b" "c" "d" "e"))
         (notes (mapcar (lambda (id) (cons id (cons "notes-root" (concat id ".md")))) ids))
         (head "## Promote\n- none\n## Update\n- none\n## Merge\n- none\n## Remove\n- none\n## Instructions\n- none\n## Notes\n")
         (blocks (mapconcat (lambda (id)
                              (format "````proposal\nnote: %S\ndisposition: %S\nreason: \"Reviewed\"\nopen-items: [\"Check activation\"]\n---\n"
                                      id (if (member id '("b" "d")) "delete" "keep"))) ids ""))
         (result (mevedel-memory-proposal-parse (concat head blocks "## No action\n- Reviewed.\n") nil nil notes)))
    (should (= 0 (plist-get result :note-format-failure-count)))
    (should-not (plist-get result :skipped-notes))
    (should (equal '(note-keep note-delete note-keep note-delete note-keep)
                   (mapcar (lambda (item) (plist-get item :action)) (plist-get result :proposals))))
    (should (cl-every (lambda (item) (equal "" (plist-get item :body))) (plist-get result :proposals))))
  :doc "keeps require unresolved items and dispositions retain normalized resolution quotes"
  (let* ((notes '(("work://shared/a.md" "notes-root" . "a.md")))
         (parse (lambda (disposition fields &optional body)
                  (mevedel-memory-proposal-parse
                   (concat "## Promote\n- none\n## Update\n- none\n## Merge\n- none\n## Remove\n- none\n"
                           "## Instructions\n- none\n## Notes\n````proposal\nnote: \"work://shared/a.md\"\n"
                           (format "disposition: %S\nreason: \"Reviewed\"\n%s---\n%s````\n"
                                   disposition fields (if body (concat body "\n") ""))
                           "## No action\n- none")
                   mevedel-test-memory-proposal--roots nil notes)))
         (anchors "resolution-anchors: [\"Restoration\", \"keep-the-row\"]\n")
         (fields (concat "open-items: [\"Verify restoration keeps one anchor\", \"Verify keep-the-row behavior\"]\n"
                         "resolved-by: [\"./docs/adr/x.md: Restoration keeps one anchor\", "
                         "\"a/./c.el: (defun keep-the-row: here)\"]\n"
                         anchors))
         (item (car (plist-get (funcall parse "delete" fields)
                               :proposals))))
    (should (equal '("Verify restoration keeps one anchor" "Verify keep-the-row behavior")
                   (plist-get item :open-items)))
    (should (equal '("Restoration" "keep-the-row") (plist-get item :resolution-anchors)))
    ;; Comparison with the review's reads uses one canonical path spelling;
    ;; the first colon separates, so quotes may contain colons.
    (should (equal '(("docs/adr/x.md" . "Restoration keeps one anchor")
                     ("a/c.el" . "(defun keep-the-row: here)"))
                   (plist-get item :resolved-by)))
    ;; Anchors are optional parser metadata.  Acceptance, not parsing,
    ;; downgrades a delete whose anchors cannot support its open items.
    (dolist (replacement '("" "resolution-anchors: \"Restoration\"\n"
                          "resolution-anchors: [\"Restoration\", 7]\n"))
      (let ((unanchored (car (plist-get (funcall parse "delete"
                                               (string-replace anchors replacement fields))
                                      :proposals))))
        (should (eq 'note-delete (plist-get unanchored :action)))
        (should (equal (plist-get item :open-items) (plist-get unanchored :open-items)))
        (should (equal (plist-get item :resolved-by) (plist-get unanchored :resolved-by)))
        (should-not (plist-get unanchored :resolution-anchors))))
    ;; Multiple items may name the same operation.  Preserve raw strings and
    ;; duplicates so normalization and positional binding belong to the guard.
    (let ((repeated (car (plist-get (funcall parse "delete"
                                            (concat "open-items: [\"Verify restoration after redraw\", \"Verify restoration after reentry\"]\n"
                                                    "resolved-by: [\"docs/a.md: Restoration after redraw preserves the anchor\", "
                                                    "\"docs/b.md: Restoration after reentry preserves the anchor\"]\n"
                                                    "resolution-anchors: [\"  RESTORATION  \", \"  RESTORATION  \"]\n"))
                                   :proposals))))
      (should (eq 'note-delete (plist-get repeated :action)))
      (should (equal '("  RESTORATION  " "  RESTORATION  ")
                     (plist-get repeated :resolution-anchors))))
    (dolist (fields '("" "open-items: []\n"))
      (let ((result (funcall parse "keep" fields)))
        (should-not (plist-get result :proposals))
        (should (equal '(("work://shared/a.md" . "Keep must name an open item lacking resolution evidence"))
                       (plist-get result :skipped-notes)))))
    (let ((kept (car (plist-get (funcall parse "keep"
                                        "open-items: [\"Verify the retry deadline under clock changes\"]\n")
                               :proposals))))
      (should (eq 'note-keep (plist-get kept :action)))
      (should (equal '("Verify the retry deadline under clock changes") (plist-get kept :open-items)))
      (should (equal "" (plist-get kept :body))))
    (let ((condensed (car (plist-get (funcall parse "condense"
                                             (concat "open-items: [\"Verify retry deadlines\"]\n"
                                                     "resolved-by: [\"./docs/adr/x.md: Restoration keeps one anchor\", \"docs/backlog.md\"]\n")
                                             "Open: verify retry deadlines.")
                                    :proposals))))
      (should (eq 'note-condense (plist-get condensed :action)))
      (should (equal '("Verify retry deadlines") (plist-get condensed :open-items)))
      (should (equal '(("docs/adr/x.md" . "Restoration keeps one anchor"))
                     (plist-get condensed :resolved-by)))
      (should (equal "Open: verify retry deadlines." (plist-get condensed :body))))
    ;; Missing resolution evidence is none, so this delete can only be kept.
    (should-not (plist-get (car (plist-get (funcall parse "delete" "open-items: [\"A\"]\n") :proposals))
                           :resolved-by))
    (dolist (invalid '(("delete" "")
                       ("delete" "resolved-by: []\n")
                       ("delete" "open-items: \"Finding 2\"\nresolved-by: []\n")
                       ("delete" "open-items: [\"\"]\nresolved-by: []\n")
                       ("delete" "open-items: [\"A\", \"A\"]\nresolved-by: []\n")
                       ("delete" "open-items: [\"A\"]\nresolved-by: \"docs/backlog.md: not an array\"\n")))
      (let ((result (funcall parse (car invalid) (cadr invalid))))
        (should-not (plist-get result :proposals))
        (should (equal '("work://shared/a.md") (mapcar #'car (plist-get result :skipped-notes))))))
    ;; Entries that cannot be evidence (a bare path, a short or placeholder
    ;; quote, an address, an unsafe path) are dropped, never trusted: the
    ;; delete keeps its open item without support, so acceptance keeps it.
    (dolist (entry '("docs/backlog.md" "docs/backlog.md: too short" "GIT-HISTORY: none"
                     "work://shared/a.md: quoting the note itself here"
                     "/etc/passwd: root entry for the system" "../outside.md: escaping quote text"
                     "a/b/../c.el: escaping quote text" "~/notes.md: home quote text here"))
      (let ((item (car (plist-get (funcall parse "delete" (format "open-items: [\"A\"]\nresolved-by: [%S]\n" entry))
                                  :proposals))))
        (should (equal '("A") (plist-get item :open-items)))
        (should-not (plist-get item :resolved-by))
        (should (mevedel-memory-store--unsupported-delete-p item nil)))))
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
    ;; Real 2026-10-28 shapes: Instructions after Notes, and an exactly
    ;; repeated header line.  Proposals come back in canonical order.
    (let ((moved (concat (string-replace "## Instructions\n- none\n" "" head) blocks "## Instructions\n- none\n")))
      (should (equal '(note-keep note-condense note-delete)
                     (mapcar (lambda (item) (plist-get item :action))
                             (plist-get (funcall parse moved) :proposals)))))
    ;; An omitted action section is indistinguishable from truncation.
    (should-error (funcall parse (concat (string-replace "## Instructions\n- none\n" "" head) blocks)))
    (should-error (mevedel-memory-proposal-parse "## Promote\n- none\n" mevedel-test-memory-proposal--roots nil))
    (should (funcall parse (concat head (string-replace "open-items: [\"Duration undecided\"]\n"
                                                        "open-items: [\"Duration undecided\"]\nopen-items: [\"Duration undecided\"]\n"
                                                        blocks))))
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
    ;; A condensed body may start with a Markdown heading (real 2026-10-28
    ;; replies were rejected when it was taken for the next section).
    (let ((headed (funcall parse (concat head (string-replace "Open: port the fix.\n" "## Open items\n- Port the fix.\n" blocks)))))
      (should (equal "## Open items\n- Port the fix." (plist-get (nth 1 (plist-get headed :proposals)) :body))))
    ;; A real slip: an open-items array spread over several lines.
    (should (equal '("Duration undecided" "Second")
                   (plist-get (car (plist-get (funcall parse (concat head (string-replace "open-items: [\"Duration undecided\"]\n"
                                                                                         "open-items: [\n  \"Duration undecided\",\n  \"Second\"\n]\n" blocks)))
                                              :proposals))
                              :open-items)))
    (dolist (invalid
             (list
              ;; Repeated, missing and unknown sections.
              (concat head "## Promote\n- none\n" blocks)
              (concat head blocks "## Unknown\n- none\n")
              ;; Analysis between blocks remains invalid.
              (concat head "- Note a.md open items:\n  - Duration undecided\n" blocks)))
      (should-error (funcall parse invalid)))
    ;; These bounded note failures leave exactly the affected note pending.
    (dolist (partial
             (list
              (concat head (string-replace "open-items: [\"Duration undecided\"]\n"
                                           "open-items: [\"Duration undecided\"]\nopen-items: [\"Other\"]\n" blocks))
              (concat head (string-replace "open-items: [\"Duration undecided\"]\n" "open-items: [\n  \"unterminated\n" blocks))
              (concat head (string-replace "---\nOpen: port the fix.\n" "" blocks))
              (concat head (string-replace "open-items: []\nresolved-by: []\n" "" blocks))
              (concat head (substring blocks 0 (string-search "````proposal\nnote: \"work://shared/c.md\"" blocks)))
              (concat head (string-replace "[\"Duration undecided\"]" "\"Duration undecided\"" blocks))))
      (let ((result (funcall parse partial)))
        (should (= 2 (length (plist-get result :proposals))))
        (should (= 1 (length (plist-get result :skipped-notes)))))))
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
  :doc "without admitted notes unknown dispositions never become proposals"
  (progn
    (should (mevedel-memory-proposal-parse
             (concat (string-remove-suffix "## No action\n- none" mevedel-test-memory-proposal--reply)
                     "## Notes\n- none\n## No action\n- none")
             mevedel-test-memory-proposal--roots (list (make-string 64 ?a))))
    (let ((result (mevedel-memory-proposal-parse
                   (concat (string-remove-suffix "## No action\n- none" mevedel-test-memory-proposal--reply)
                           "## Notes\n````proposal\nnote: \"work://shared/a.md\"\ndisposition: \"delete\"\n"
                           "reason: \"Reviewed\"\n---\n````\n## No action\n- none")
                   mevedel-test-memory-proposal--roots (list (make-string 64 ?a)))))
      (should (equal '(promote) (mapcar (lambda (item) (plist-get item :action)) (plist-get result :proposals))))
      (should-not (plist-get result :skipped-notes))
      (should (string-search "1 unadmitted note blocks skipped" (plist-get result :no-action))))))

(mevedel-deftest mevedel-memory-proposal-condensed-bytes
  (:doc "condensed notes end in exactly one newline and encode as UTF-8")
  (progn
    (should (equal "Open.\n" (mevedel-memory-proposal-condensed-bytes "Open.\n\n\n")))
    (should (equal (encode-coding-string "界\n" 'utf-8) (mevedel-memory-proposal-condensed-bytes "界")))))

(provide 'test-mevedel-memory-proposal)
;;; test-mevedel-memory-proposal.el ends here
