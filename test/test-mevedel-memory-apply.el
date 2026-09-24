;;; test-mevedel-memory-apply.el -- Memory transaction preparation -*- lexical-binding: t -*-

;;; Commentary:

;; Prepare complete changes from captured memory and commit through ApplyPatch.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-memory-apply)
(require 'mevedel-memory-store)
(require 'mevedel-tool-patch)
(require 'mevedel-system)

(mevedel-deftest mevedel-memory-apply--index-lines
    ()
  ,test
  (test)
  :doc "retains prose and order while decoding destinations and escaped labels"
  (should (equal '((nil . "# Notes") ("a b.md" . "- [A \\[label\\]](a%20b.md) - Context") (nil . ""))
                 (mevedel-memory-apply--index-lines "# Notes\n- [A \\[label\\]](a%20b.md) - Context\n")))
  :doc "rejects duplicate decoded destinations, traversal, indexes, and unsupported links"
  (dolist (text '("- [A](a.md) - A\n- [Alias](%61.md) - B\n"
                  "- [Escape](../a.md) - Context\n"
                  "- [Index](MEMORY.md) - Context\n"
                  "See [Topic](a.md)\n"))
    (should-error (mevedel-memory-apply--index-lines text))))

(mevedel-deftest mevedel-memory-apply--index-conflicts
    (:vars ((proposal '(:file "a.md" :merged-files ("b.md")))))
  ,test
  (test)
  :doc "ignores unrelated edits, deletions, prose and reordered affected entries"
  (should-not
   (mevedel-memory-apply--index-conflicts
    '(:bytes "- [A](a.md) - First\n- [Other](other.md) - Old\n- [B](b.md) - Second\n")
    '(:bytes "# Manual heading\n- [B](b.md) - Second\n- [New](new.md) - Added\n- [A](a.md) - First\n") proposal))
  :doc "reports added, removed, edited and encoded affected destinations"
  (dolist (pair '(("" . "- [A](%61.md) - Added\n")
                  ("- [A](a.md) - Removed\n" . "")
                  ("- [A](a.md) - Old\n" . "- [A](a.md) - New\n")))
    (should (equal '("a.md")
                   (mevedel-memory-apply--index-conflicts (list :bytes (car pair)) (list :bytes (cdr pair)) proposal))))
  :doc "includes merge sources in conflicts and treats an absent index as empty"
  (progn
    (should (equal '("b.md")
                   (mevedel-memory-apply--index-conflicts nil '(:bytes "- [B](b.md) - Added\n") proposal)))
    (should-not (mevedel-memory-apply--index-conflicts nil '(:bytes "# Notes\n") proposal))
    (should-not (mevedel-memory-apply--index-conflicts '(:bytes "# Notes\n") nil proposal))))

(mevedel-deftest mevedel-memory-apply-changes
    (:vars* ((root (make-temp-file "mevedel-memory-apply-" t))
             (memory (file-name-concat root ".agents" "memory"))
             (workspace (mevedel-workspace--create :root root))
             (identity (mevedel-workspace-identity-ensure root))
             (mevedel-memory-dirs (list memory)) scope root-id)
     :before-each ((make-directory memory t)
                   (write-region "# Memory\n- [Alpha](a.md) - first\n- [Beta](b.md) - second\n" nil (file-name-concat memory "MEMORY.md") nil 'silent)
                   (write-region "Old alpha.\n" nil (file-name-concat memory "a.md") nil 'silent)
                   (write-region "Old beta.\n" nil (file-name-concat memory "b.md") nil 'silent)
                   (write-region "Existing instructions.\n" nil (file-name-concat root "AGENTS.md") nil 'silent)
                   (setq scope (mevedel-memory-scope-capture workspace) root-id (caar (plist-get scope :roots))))
     :after-each ((delete-directory root t)))
  (cl-labels ((proposal (action file body &optional merged)
                (mevedel-memory-store--proposal
                 scope (make-string 64 ?a)
                 (list :action action :root root-id :file file :type "project" :title "Current [context]"
                       :hook "When this matters" :reason "Retain context" :evidence nil :body body :merged-files merged)))
              (read-file (file) (mevedel-session-control-fs-read-file (file-name-concat memory file))))
    ,test)
  (test)
  :doc "promotes a complete topic and escaped index entry through the shared transaction"
  (let ((changes (mevedel-memory-apply-changes scope (proposal 'promote "new topic.md" "New lesson."))))
    (should-not (file-exists-p (file-name-concat memory "new topic.md")))
    (mevedel-tool-patch-commit changes)
    (should (equal "---\nname: \"Current [context]\"\ndescription: \"When this matters\"\ntype: project\n---\n\nNew lesson.\n"
                   (read-file "new topic.md")))
    (should (string-search "- [Current \\[context\\]](new%20topic.md) - When this matters\n" (read-file "MEMORY.md"))))
  :doc "Unicode titles and descriptions remain text in both patch content and UTF-8 files"
  (let ((item (proposal 'promote "unicode.md" "Body with \u754c.")))
    (setf (plist-get item :title) "\u754c topic"
          (plist-get item :hook) "caf\u00e9 context")
    (let* ((changes (mevedel-memory-apply-changes scope item))
           (topic (seq-find (lambda (change) (string-suffix-p "unicode.md" (plist-get change :path))) changes)))
      (should (string-search "\u754c topic" (plist-get topic :content)))
      (should (equal (plist-get topic :content) (decode-coding-string (plist-get topic :bytes) 'utf-8-unix)))
      (mevedel-tool-patch-commit changes)
      (should (equal (plist-get topic :content) (read-file "unicode.md")))))
  :doc "updates one topic and its existing index position"
  (progn
    (mevedel-tool-patch-commit (mevedel-memory-apply-changes scope (proposal 'update "a.md" "Updated lesson.")))
    (should (string-suffix-p "Updated lesson.\n" (read-file "a.md")))
    (should (equal "# Memory\n- [Current \\[context\\]](a.md) - When this matters\n- [Beta](b.md) - second\n" (read-file "MEMORY.md")))
    (should (equal "Old beta.\n" (read-file "b.md"))))
  :doc "merges both topics and their index entries as one checked transaction"
  (progn
    (mevedel-tool-patch-commit (mevedel-memory-apply-changes scope (proposal 'merge "a.md" "Merged context." '("a.md" "b.md"))))
    (should-not (file-exists-p (file-name-concat memory "b.md")))
    (should (string-suffix-p "Merged context.\n" (read-file "a.md")))
    (should (equal "# Memory\n- [Current \\[context\\]](a.md) - When this matters\n" (read-file "MEMORY.md"))))
  :doc "removes a topic and only its index entry"
  (progn
    (mevedel-tool-patch-commit (mevedel-memory-apply-changes scope (proposal 'remove "a.md" "")))
    (should-not (file-exists-p (file-name-concat memory "a.md")))
    (should (equal "# Memory\n- [Beta](b.md) - second\n" (read-file "MEMORY.md"))))
  :doc "appends instructions to their captured file without changing the memory index"
  (progn
    (setq root-id (car (seq-find (lambda (row) (eq (plist-get (cdr row) :kind) 'instructions)) (plist-get scope :roots))))
    (mevedel-tool-patch-commit (mevedel-memory-apply-changes scope (proposal 'instructions "AGENTS.md" "Additional instruction.")))
    (should (equal "Existing instructions.\n\nAdditional instruction.\n"
                   (mevedel-session-control-fs-read-file (file-name-concat root "AGENTS.md"))))
    (should (equal "# Memory\n- [Alpha](a.md) - first\n- [Beta](b.md) - second\n" (read-file "MEMORY.md"))))
  :doc "a changed index prevents all prepared topic writes"
  (let ((changes (mevedel-memory-apply-changes scope (proposal 'update "a.md" "Updated lesson."))))
    (write-region "External index edit.\n" nil (file-name-concat memory "MEMORY.md") nil 'silent)
    (should-error (mevedel-tool-patch-commit changes))
    (should (equal "Old alpha.\n" (read-file "a.md")))
    (should (equal "External index edit.\n" (read-file "MEMORY.md"))))
  :doc "an index update exceeding the memory reader limit produces no writes"
  (progn
    (write-region (make-string 32760 ?x) nil (file-name-concat memory "MEMORY.md") nil 'silent)
    (setq scope (mevedel-memory-scope-capture workspace))
    (should-error (mevedel-memory-apply-changes scope (proposal 'promote "new.md" "New lesson.")))
    (should-not (file-exists-p (file-name-concat memory "new.md")))
    (should (= 32760 (string-bytes (read-file "MEMORY.md")))))
  :doc "duplicate index destinations are rejected before producing changes"
  (progn
    (write-region "- [One](a.md) - first\n- [Alias](%61.md) - same destination\n" nil (file-name-concat memory "MEMORY.md") nil 'silent)
    (setq scope (mevedel-memory-scope-capture workspace))
    (should-error (mevedel-memory-apply-changes scope (proposal 'update "a.md" "Updated lesson.")))
    (should (equal "Old alpha.\n" (read-file "a.md")))))

(provide 'test-mevedel-memory-apply)
;;; test-mevedel-memory-apply.el ends here
