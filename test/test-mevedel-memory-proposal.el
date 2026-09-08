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

(provide 'test-mevedel-memory-proposal)
;;; test-mevedel-memory-proposal.el ends here
