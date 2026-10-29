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

(mevedel-deftest mevedel-memory-proposal-parse/instructions ()
  ,test
  (test)
  :doc "complete instruction bodies preserve whitespace for existing and known-absent targets"
  (let* ((roots '(("instructions" :kind instructions :files ("AGENTS.md" "AGENTS.local.md")
                                :before (("AGENTS.md" :exists t :bytes "Old guidance.\n")
                                         ("AGENTS.local.md" :exists nil)))))
         (head "## Promote\n- none\n## Update\n- none\n## Merge\n- none\n## Remove\n- none\n## Instructions\n"))
    (dolist (file '("AGENTS.md" "AGENTS.local.md"))
      (dolist (body '("" "  \t" "\n" "  Rule.  \n\n" "## Policy\n```elisp\n(ignore)\n```\n"))
        (let* ((reply (concat head
                              (format "````proposal\nroot: \"instructions\"\nfile: %S\ntype: \"project\"\ntitle: \"Policy\"\nhook: \"Contributor guidance\"\nreason: \"Repair source citation\"\nevidence: []\n---\n" file)
                              body "\n````\n## No action\n- none"))
               (item (car (plist-get (mevedel-memory-proposal-parse reply roots nil) :proposals))))
          (should (eq 'instructions (plist-get item :action)))
          ;; One newline is fence structure, not part of the desired file.
          (should (equal body (plist-get item :body)))))))
  :doc "empty instruction bodies do not widen target admission or memory-body rules"
  (let* ((roots '(("instructions" :kind instructions :files ("AGENTS.md"))))
         (block "root: \"instructions\"\nfile: \"AGENTS.md\"\ntype: \"project\"\ntitle: \"Policy\"\nhook: \"Policy\"\nreason: \"Repair\"\nevidence: []\n---\n")
         (reply (concat "## Promote\n- none\n## Update\n- none\n## Merge\n- none\n## Remove\n- none\n## Instructions\n````proposal\n"
                        block "````\n## No action\n- none")))
    (should-error (mevedel-memory-proposal-parse (string-replace "AGENTS.md" "outside.md" reply) roots nil))
    (should-error (mevedel-memory-proposal-parse (string-replace "\"instructions\"" "\"unknown\"" reply) roots nil))
    (should-error (mevedel-memory-proposal-parse
                   (string-replace "Use a monotonic deadline for elapsed-time retries." ""
                                   mevedel-test-memory-proposal--reply)
                   mevedel-test-memory-proposal--roots (list (make-string 64 ?a))))))

(mevedel-deftest mevedel-memory-proposal-parse/notes
    (:vars* ((notes '(("work://shared/a.md" "notes-root" . "a.md")
                     ("work://shared/b.md" "notes-root" . "b.md")
                     ("work://shared/c.md" "notes-root" . "c.md")))
             (head (string-remove-suffix "## No action\n- none" mevedel-test-memory-proposal--reply))))
  (cl-labels ((block (id &optional body)
                (format "````proposal\nnote: %S\ndisposition: \"delete\"\nreason: \"Reviewed\"\n---\n%s````\n"
                        id (if body (concat body "\n") "")))
              (parse (blocks)
                (mevedel-memory-proposal-parse
                 (concat head "## Notes\n" blocks "## No action\n- Reviewed.\n")
                 mevedel-test-memory-proposal--roots (list (make-string 64 ?a)) notes))
              (actions (result)
                (mapcar (lambda (item) (plist-get item :action)) (plist-get result :proposals)))
              (skipped (result) (mapcar #'car (plist-get result :skipped-notes))))
    ,test)
  (test)
  :doc "delete acknowledgements retain exact admitted identities and allow no extraction"
  (let* ((result (parse (mapconcat (lambda (note) (block (car note))) notes "")))
         (items (cdr (plist-get result :proposals))))
    (should (equal '(promote note-delete note-delete note-delete) (actions result)))
    (should-not (plist-get result :skipped-notes))
    (should (equal (mapcar #'car notes) (mapcar (lambda (item) (plist-get item :note)) items)))
    (dolist (item items)
      (should (equal (list (plist-get item :note)) (plist-get item :evidence)))
      (should (equal "" (plist-get item :body))))
    (should (mevedel-memory-proposal-note-action-p 'note-delete))
    (should (mevedel-memory-proposal-note-action-p 'note-prune))
    (should-not (mevedel-memory-proposal-note-action-p 'note-keep))
    (should-not (mevedel-memory-proposal-note-action-p 'note-condense)))
  :doc "invalid one-note fields and removed actions never discard valid unrelated proposals"
  (let ((a (block "work://shared/a.md")) (b (block "work://shared/b.md")) (c (block "work://shared/c.md")))
    (dolist (bad (append
                 (mapcar (lambda (field) (string-replace "reason: \"Reviewed\"" field a))
                         '("reason: null" "reason: invalid-json" "future-field: true"
                           "open-items: []" "resolved-by: []" "resolution-anchors: []" "unresolved-items: []"
                           "reason: \"Reviewed\"\nreason: \"Other\""))
                 (mapcar (lambda (value) (string-replace "disposition: \"delete\"" value a))
                         '("disposition: \"keep\"" "disposition: \"condense\"" "disposition: null" ""))
                 (list (block "work://shared/a.md" "Unexpected replacement body."))))
      (let ((result (parse (concat bad b c))))
        (should (equal '(promote note-delete note-delete) (actions result)))
        (should (equal '("work://shared/a.md") (skipped result)))
        (should (= 1 (plist-get result :note-format-failure-count)))
        (should (string-search "Note NO OP" (plist-get result :no-action)))))
    ;; Missing or undecodable ownership cannot safely be assigned to one note.
    (dolist (bad (list (string-replace "note: \"work://shared/a.md\"" "note: null" a)
                      (string-replace "note: \"work://shared/a.md\"\n" "" a)))
      (should-error (parse (concat bad b c)))))
  :doc "missing duplicate aliased and unadmitted identities receive no service credit"
  (let ((a (block "work://shared/a.md")) (b (block "work://shared/b.md")) (c (block "work://shared/c.md")))
    (dolist (blocks (list (concat a a b c)
                          (concat (string-replace "reason: \"Reviewed\"" "unknown: true" a) a b c)
                          (concat a (string-replace "reason: \"Reviewed\"" "unknown: true" a) b c)))
      (let ((result (parse blocks)))
        (should (equal '(promote note-delete note-delete) (actions result)))
        (should (equal '("work://shared/a.md") (skipped result)))))
    (let ((result (mevedel-memory-proposal-parse
                   (concat head "## Notes\n" a (block "alias") b c)
                   mevedel-test-memory-proposal--roots (list (make-string 64 ?a))
                   (append notes '(("alias" "notes-root" . "a.md"))))))
      (should (equal '(promote note-delete note-delete) (actions result)))
      (should (equal '("work://shared/a.md" "alias") (skipped result))))
    (let ((result (parse (concat b c (block "foreign/a.md")))))
      (should (equal '("work://shared/a.md") (skipped result)))
      (should (= 2 (plist-get result :note-format-failure-count))))
    (let ((result (parse "- none\n")))
      (should (equal '(promote) (actions result)))
      (should (equal (mapcar #'car notes) (skipped result))))
    (let ((result (mevedel-memory-proposal-parse head mevedel-test-memory-proposal--roots
                                                (list (make-string 64 ?a)) notes)))
      (should (equal (mapcar #'car notes) (skipped result)))))
  :doc "store reparse retains isolated note diagnostics and rejects malformed memory"
  (let* ((text (concat head "## Notes\n"
                       (string-replace "reason: \"Reviewed\"" "future-field: true" (block "work://shared/a.md"))
                       (block "work://shared/b.md") (block "work://shared/c.md")))
         (result (parse (substring text (length (concat head "## Notes\n")))))
         (scope (list :roots (append mevedel-test-memory-proposal--roots
                                    '(("notes-root" :kind notes
                                       :notes (("a.md" :id "work://shared/a.md")
                                               ("b.md" :id "work://shared/b.md")
                                               ("c.md" :id "work://shared/c.md")))))))
         captured)
    (cl-letf (((symbol-function 'mevedel-memory-store-accept-proposals)
               (lambda (_workspace _prepared parsed &rest _args) (setq captured parsed))))
      (mevedel-memory-store-accept nil (list :scope scope) text
                                   (list (list :id (make-string 64 ?a))) "test" nil (mapcar #'car notes)))
    (should (equal (plist-get result :proposals) (plist-get captured :proposals)))
    (should (equal (plist-get result :skipped-notes) (plist-get captured :skipped-notes)))
    (should-error (parse (concat (block "work://shared/a.md") "## Unknown\n- none\n")))
    (should-error (mevedel-memory-proposal-parse
                   (string-replace "root: \"local-agents\"" "root: \"foreign\"" text)
                   mevedel-test-memory-proposal--roots (list (make-string 64 ?a)) notes)))
  :doc "bodyless delete blocks accept safe implicit boundaries and identical header repeats"
  (let* ((blocks (mapconcat (lambda (note) (string-remove-suffix "````\n" (block (car note)))) notes ""))
         (result (parse blocks)))
    (should (equal '(promote note-delete note-delete note-delete) (actions result)))
    (should-not (skipped result))
    (should-not (skipped (parse (string-replace "reason: \"Reviewed\"" "reason: \"Reviewed\"\nreason: \"Reviewed\"" blocks))))
    (should-not (skipped (parse (replace-regexp-in-string "^````\\(proposal\\)?$" "````\\1 \t"
                                                         (mapconcat (lambda (note) (block (car note))) notes ""))))))
  :doc "unexpected bodies and ambiguous unclosed tails cannot admit body-shaped targets"
  (let ((a (block "work://shared/a.md")) (c (block "work://shared/c.md")))
    (dolist (body '("## No action\nnote: \"work://shared/c.md\"\ndisposition: \"delete\"\n---"
                    "```proposal\nnote: \"work://shared/c.md\"\ndisposition: \"delete\"\n---\n```"))
      (let ((result (parse (concat a (block "work://shared/b.md" body) c))))
        (should (equal '(promote note-delete note-delete) (actions result)))
        (should (equal '("work://shared/b.md") (skipped result)))))
    (let ((unclosed (string-remove-suffix "````\n" (block "work://shared/b.md" "Unexpected body."))))
      (dolist (tail (list "" c))
        (let ((result (parse (concat a unclosed tail))))
          (should (equal '(promote note-delete) (actions result)))
          (should (equal '("work://shared/b.md" "work://shared/c.md") (skipped result)))))))
  :doc "unknown notes never become proposals without an admitted note roster"
  (let ((result (mevedel-memory-proposal-parse
                 (concat head "## Notes\n" (block "work://shared/a.md"))
                 mevedel-test-memory-proposal--roots (list (make-string 64 ?a)))))
    (should (equal '(promote) (actions result)))
    (should-not (skipped result))
    (should (= 1 (plist-get result :note-format-failure-count)))))

(provide 'test-mevedel-memory-proposal)
;;; test-mevedel-memory-proposal.el ends here
