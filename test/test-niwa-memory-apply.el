;;; test-niwa-memory-apply.el -- Memory transaction preparation -*- lexical-binding: t -*-

;;; Commentary:

;; Prepare complete changes from captured memory and commit through ApplyPatch.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'niwa-memory-apply)
(require 'niwa-memory-store)
(require 'niwa-tool-patch)
(require 'niwa-system)

(niwa-deftest niwa-memory-apply--index-lines
    ()
  ,test
  (test)
  :doc "retains prose and order while decoding destinations and escaped labels"
  (should (equal '((nil . "# Notes") ("a b.md" . "- [A \\[label\\]](a%20b.md) - Context") (nil . ""))
                 (niwa-memory-apply--index-lines "# Notes\n- [A \\[label\\]](a%20b.md) - Context\n")))
  :doc "rejects duplicate decoded destinations, traversal, indexes, and unsupported links"
  (dolist (text '("- [A](a.md) - A\n- [Alias](%61.md) - B\n"
                  "- [Escape](../a.md) - Context\n"
                  "- [Index](MEMORY.md) - Context\n"
                  "See [Topic](a.md)\n"))
    (should-error (niwa-memory-apply--index-lines text))))

(niwa-deftest niwa-memory-apply--index-conflicts
    (:vars ((proposal '(:file "a.md" :merged-files ("b.md")))))
  ,test
  (test)
  :doc "ignores unrelated edits, deletions, prose and reordered affected entries"
  (should-not
   (niwa-memory-apply--index-conflicts
    '(:bytes "- [A](a.md) - First\n- [Other](other.md) - Old\n- [B](b.md) - Second\n")
    '(:bytes "# Manual heading\n- [B](b.md) - Second\n- [New](new.md) - Added\n- [A](a.md) - First\n") proposal))
  :doc "reports added, removed, edited and encoded affected destinations"
  (dolist (pair '(("" . "- [A](%61.md) - Added\n")
                  ("- [A](a.md) - Removed\n" . "")
                  ("- [A](a.md) - Old\n" . "- [A](a.md) - New\n")))
    (should (equal '("a.md")
                   (niwa-memory-apply--index-conflicts (list :bytes (car pair)) (list :bytes (cdr pair)) proposal))))
  :doc "includes merge sources in conflicts and treats an absent index as empty"
  (progn
    (should (equal '("b.md")
                   (niwa-memory-apply--index-conflicts nil '(:bytes "- [B](b.md) - Added\n") proposal)))
    (should-not (niwa-memory-apply--index-conflicts nil '(:bytes "# Notes\n") proposal))
    (should-not (niwa-memory-apply--index-conflicts '(:bytes "# Notes\n") nil proposal))))

(niwa-deftest niwa-memory-apply-changes
    (:vars* ((root (make-temp-file "niwa-memory-apply-" t))
             (memory (file-name-concat root ".agents" "memory"))
             (workspace (niwa-workspace--create :root root))
             (identity (niwa-workspace-identity-ensure root))
             (niwa-memory-dirs (list memory)) scope root-id)
     :before-each ((make-directory memory t)
                   (write-region "# Memory\n- [Alpha](a.md) - first\n- [Beta](b.md) - second\n" nil (file-name-concat memory "MEMORY.md") nil 'silent)
                   (write-region "Old alpha.\n" nil (file-name-concat memory "a.md") nil 'silent)
                   (write-region "Old beta.\n" nil (file-name-concat memory "b.md") nil 'silent)
                   (write-region "Existing instructions.\n" nil (file-name-concat root "AGENTS.md") nil 'silent)
                   (setq scope (niwa-memory-scope-capture workspace) root-id (caar (plist-get scope :roots))))
     :after-each ((delete-directory root t)))
  (cl-labels ((proposal (action file body &optional merged)
                (niwa-memory-store--proposal
                 scope (make-string 64 ?a)
                 (list :action action :root root-id :file file :type "project" :title "Current [context]"
                       :hook "When this matters" :reason "Retain context" :evidence nil :body body :merged-files merged)))
              (read-file (file) (niwa-session-control-fs-read-file (file-name-concat memory file))))
    ,test)
  (test)
  :doc "promotes a complete topic and escaped index entry through the shared transaction"
  (let ((changes (niwa-memory-apply-changes scope (proposal 'promote "new topic.md" "New lesson."))))
    (should-not (file-exists-p (file-name-concat memory "new topic.md")))
    (niwa-tool-patch-commit changes)
    (should (equal (format "---\nname: \"Current [context]\"\ndescription: \"When this matters\"\ntype: project\nmodified: %s\n---\n\nNew lesson.\n"
                           (format-time-string "%Y-%m-%d"))
                   (read-file "new topic.md")))
    (should (string-search "- [Current \\[context\\]](new%20topic.md) - When this matters\n" (read-file "MEMORY.md"))))
  :doc "an unchanged topic keeps its recorded date and yields no write; a changed one is redated"
  (progn
    (write-region "---\nname: \"Current [context]\"\ndescription: \"When this matters\"\ntype: project\nmodified: 2020-01-02\n---\n\nSame.\n"
                  nil (file-name-concat memory "a.md") nil 'silent)
    (write-region "# Memory\n- [Current \\[context\\]](a.md) - When this matters\n- [Beta](b.md) - second\n"
                  nil (file-name-concat memory "MEMORY.md") nil 'silent)
    (setq scope (niwa-memory-scope-capture workspace))
    (should-not (niwa-memory-apply-changes scope (proposal 'update "a.md" "Same.")))
    (let ((topic (car (niwa-memory-apply-changes scope (proposal 'update "a.md" "Changed.")))))
      (should (string-search (format "modified: %s\n" (format-time-string "%Y-%m-%d")) (plist-get topic :content)))))
  :doc "an absent topic keeps its captured mode in another editor; writing wider permissions is refused"
  (dolist (pair '((#o600 . #o644) (#o644 . #o600)))
    (cl-letf (((symbol-function 'default-file-modes) (lambda () (car pair))))
      (setq scope (niwa-memory-scope-capture workspace)))
    (cl-letf (((symbol-function 'default-file-modes) (lambda () (cdr pair))))
      (let* ((topic (car (niwa-memory-apply-changes scope (proposal 'promote "new.md" "Body"))))
             (before (plist-get topic :before))
             (after (list :path (plist-get topic :path) :exists t :mode (plist-get topic :mode))))
        (should (= (car pair) (plist-get after :mode)))
        (if (= (car pair) #o600)
            (should-not (niwa-memory-apply-check-new-mode before after))
          (let ((error (should-error (niwa-memory-apply-check-new-mode before after))))
            (should (string-match-p "0644 exceeds permitted new-file mode 0600" (error-message-string error)))))
        (should-not (file-exists-p (plist-get after :path)))
        ;; Completing a row at exact after doesn't create anything.
        (should-not (niwa-memory-apply-check-new-mode after after)))))
  :doc "Unicode titles and descriptions remain text in both patch content and UTF-8 files"
  (let ((item (proposal 'promote "unicode.md" "Body with \u754c.")))
    (setf (plist-get item :title) "\u754c topic"
          (plist-get item :hook) "caf\u00e9 context")
    (let* ((changes (niwa-memory-apply-changes scope item))
           (topic (seq-find (lambda (change) (string-suffix-p "unicode.md" (plist-get change :path))) changes)))
      (should (string-search "\u754c topic" (plist-get topic :content)))
      (should (equal (plist-get topic :content) (decode-coding-string (plist-get topic :bytes) 'utf-8-unix)))
      (niwa-tool-patch-commit changes)
      (should (equal (plist-get topic :content) (read-file "unicode.md")))))
  :doc "updates one topic and its existing index position"
  (progn
    (niwa-tool-patch-commit (niwa-memory-apply-changes scope (proposal 'update "a.md" "Updated lesson.")))
    (should (string-suffix-p "Updated lesson.\n" (read-file "a.md")))
    (should (equal "# Memory\n- [Current \\[context\\]](a.md) - When this matters\n- [Beta](b.md) - second\n" (read-file "MEMORY.md")))
    (should (equal "Old beta.\n" (read-file "b.md"))))
  :doc "merges both topics and their index entries as one checked transaction"
  (progn
    (niwa-tool-patch-commit (niwa-memory-apply-changes scope (proposal 'merge "a.md" "Merged context." '("a.md" "b.md"))))
    (should-not (file-exists-p (file-name-concat memory "b.md")))
    (should (string-suffix-p "Merged context.\n" (read-file "a.md")))
    (should (equal "# Memory\n- [Current \\[context\\]](a.md) - When this matters\n" (read-file "MEMORY.md"))))
  :doc "removes a topic and only its index entry"
  (progn
    (niwa-tool-patch-commit (niwa-memory-apply-changes scope (proposal 'remove "a.md" "")))
    (should-not (file-exists-p (file-name-concat memory "a.md")))
    (should (equal "# Memory\n- [Beta](b.md) - second\n" (read-file "MEMORY.md"))))
  :doc "replaces complete instructions without appending or changing the memory index"
  (progn
    (setq root-id (car (seq-find (lambda (row) (eq (plist-get (cdr row) :kind) 'instructions)) (plist-get scope :roots))))
    (niwa-tool-patch-commit (niwa-memory-apply-changes scope (proposal 'instructions "AGENTS.md" "Additional instruction.")))
    (should (equal "Additional instruction."
                   (niwa-session-control-fs-read-file (file-name-concat root "AGENTS.md"))))
    (should (equal "# Memory\n- [Alpha](a.md) - first\n- [Beta](b.md) - second\n" (read-file "MEMORY.md"))))
  :doc "empty instructions clear an existing file rather than deleting it"
  (progn
    (setq root-id (car (seq-find (lambda (row) (eq (plist-get (cdr row) :kind) 'instructions)) (plist-get scope :roots))))
    (let ((path (file-name-concat root "AGENTS.md")))
      (niwa-tool-patch-commit (niwa-memory-apply-changes scope (proposal 'instructions "AGENTS.md" "")))
      (should (file-exists-p path))
      (should (equal "" (niwa-session-control-fs-read-file path)))))
  :doc "empty instructions create an admitted absent file with zero bytes"
  (progn
    (setq root-id (car (seq-find (lambda (row) (eq (plist-get (cdr row) :kind) 'instructions)) (plist-get scope :roots))))
    (let ((path (file-name-concat root "AGENTS.local.md")))
      (should-not (file-exists-p path))
      (niwa-tool-patch-commit (niwa-memory-apply-changes scope (proposal 'instructions "AGENTS.local.md" "")))
      (should (file-exists-p path))
      (should (= 0 (file-attribute-size (file-attributes path))))))
  :doc "instruction whitespace is exact and an unchanged complete body emits no write"
  (progn
    (setq root-id (car (seq-find (lambda (row) (eq (plist-get (cdr row) :kind) 'instructions)) (plist-get scope :roots))))
    (should-not (niwa-memory-apply-changes scope (proposal 'instructions "AGENTS.md" "Existing instructions.\n")))
    (let ((body "  Indented instruction.\n\n "))
      (niwa-tool-patch-commit (niwa-memory-apply-changes scope (proposal 'instructions "AGENTS.md" body)))
      (should (equal body (niwa-session-control-fs-read-file (file-name-concat root "AGENTS.md"))))))
  :doc "a changed index prevents all prepared topic writes"
  (let ((changes (niwa-memory-apply-changes scope (proposal 'update "a.md" "Updated lesson."))))
    (write-region "External index edit.\n" nil (file-name-concat memory "MEMORY.md") nil 'silent)
    (should-error (niwa-tool-patch-commit changes))
    (should (equal "Old alpha.\n" (read-file "a.md")))
    (should (equal "External index edit.\n" (read-file "MEMORY.md"))))
  :doc "an index update exceeding the memory reader limit produces no writes"
  (progn
    (write-region (make-string 32760 ?x) nil (file-name-concat memory "MEMORY.md") nil 'silent)
    (setq scope (niwa-memory-scope-capture workspace))
    (should-error (niwa-memory-apply-changes scope (proposal 'promote "new.md" "New lesson.")))
    (should-not (file-exists-p (file-name-concat memory "new.md")))
    (should (= 32760 (string-bytes (read-file "MEMORY.md")))))
  :doc "duplicate index destinations are rejected before producing changes"
  (progn
    (write-region "- [One](a.md) - first\n- [Alias](%61.md) - same destination\n" nil (file-name-concat memory "MEMORY.md") nil 'silent)
    (setq scope (niwa-memory-scope-capture workspace))
    (should-error (niwa-memory-apply-changes scope (proposal 'update "a.md" "Updated lesson.")))
    (should (equal "Old alpha.\n" (read-file "a.md")))))

(provide 'test-niwa-memory-apply)
;;; test-niwa-memory-apply.el ends here
