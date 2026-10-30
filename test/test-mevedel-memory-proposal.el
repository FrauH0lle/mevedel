;;; test-mevedel-memory-proposal.el -- Consolidation output tests -*- lexical-binding: t -*-

;;; Commentary:

;; Validate memory and instruction proposals against captured scope and evidence.

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

  :doc "admitted note IDs are evidence but never authorize model note actions"
  (let* ((note "work://shared/finding.md")
         (reply (string-replace (make-string 64 ?a) note mevedel-test-memory-proposal--reply))
         (result (mevedel-memory-proposal-parse reply mevedel-test-memory-proposal--roots (list note))))
    (should (equal '(promote) (mapcar (lambda (item) (plist-get item :action))
                                    (plist-get result :proposals))))
    (should (equal (list note) (plist-get (car (plist-get result :proposals)) :evidence)))
    (should-error (mevedel-memory-proposal-parse reply mevedel-test-memory-proposal--roots nil))
    (dolist (invalid (list (string-replace "## No action" "## Notes\n- none\n## No action" reply)
                          (string-replace "root: \"local-agents\""
                                          (concat "note: " (json-serialize note) "\nroot: \"local-agents\"")
                                          reply)))
      (should-error (mevedel-memory-proposal-parse invalid mevedel-test-memory-proposal--roots (list note)))))

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

(mevedel-deftest mevedel-memory-proposal-note-action-p
  (:doc "recognizes internal disposal actions separately from model proposal actions")
  (progn
    (should (mevedel-memory-proposal-note-action-p 'note-delete))
    (should (mevedel-memory-proposal-note-action-p 'note-prune))
    (should-not (mevedel-memory-proposal-note-action-p 'promote))
    (should-not (mevedel-memory-proposal-note-action-p 'instructions))))

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

(provide 'test-mevedel-memory-proposal)
;;; test-mevedel-memory-proposal.el ends here
