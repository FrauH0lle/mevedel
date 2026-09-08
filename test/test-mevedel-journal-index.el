;;; test-mevedel-journal-index.el -- Journal discovery tests -*- lexical-binding: t -*-

;;; Commentary:

;; Checks disposable discovery observations independently of publication owners.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-journal-index)

(mevedel-deftest mevedel-journal-index-unreviewed ()
  ,test
  (test)
  :doc "coverage is the exact union of general reviews, independent of timestamps"
  (let* ((first '(:kind digest :id "a" :created "later"))
         (second '(:kind digest :id "b" :created "earlier"))
         (third '(:kind digest :id "c"))
         (reviews '((:kind consolidation :focus "" :digests ("a"))
                    (:kind consolidation :focus "" :digests ("a" "absent"))
                    (:kind consolidation :focus "specific topic" :digests ("b" "c")))))
    (should (equal (list second third)
                   (mevedel-journal-index-unreviewed (append (list first second third) reviews)))))
  (should-not (mevedel-journal-index-unreviewed nil)))

(mevedel-deftest mevedel-journal-index-entries ()
  ,test
  (test)
  :doc "throttles scans but observes external additions, removals, and workspace root changes"
  (let* ((root (make-temp-file "mevedel-journal-index-" t))
         (workspace (mevedel-workspace--create :root root))
         (scan (symbol-function 'mevedel-journal-store-entries))
         (scans 0))
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel-journal-store-entries)
                   (lambda (path) (cl-incf scans) (funcall scan path))))
          (should-not (mevedel-journal-index-entries workspace t))
          (should (= scans 0))
          (should-not (mevedel-journal-index-entries workspace))
          (should-not (mevedel-journal-index-entries workspace))
          (should (= scans 1))
          (let* ((entry (mevedel-journal-store-publish-digest
                         root
                         (list :capture-id (make-string 64 ?a) :session "index-session"
                               :session-name "Index" :workspace (make-string 64 ?b)
                               :trigger 'session-end :segment 1 :source-revision (make-string 64 ?c)
                               :turns '(1) :turn-ids (list (make-string 64 ?d))
                               :created "2026-09-07T12:00:00Z" :model "provider:model")
                         "## Done\n- Observed: Tests passed (turn 1).\n\n## Learned\n- none\n\n## Surprised\n- none\n\n## Unfinished\n- none"))
                 (path (file-name-concat (mevedel-journal-store-directory root) (plist-get entry :file))))
            (should-not (mevedel-journal-index-entries workspace))
            (setf (plist-get (mevedel-workspace-journal-observation workspace) :time) (- (float-time) 11))
            (should (equal (list entry) (mevedel-journal-index-entries workspace)))
            (should (= scans 2))
            (delete-file path)
            (should (equal (list entry) (mevedel-journal-index-entries workspace t)))
            (setf (plist-get (mevedel-workspace-journal-observation workspace) :time) (- (float-time) 11))
            (should-not (mevedel-journal-index-entries workspace))
            (should (= scans 3))
            (setf (mevedel-workspace-root workspace) (file-name-concat root "different"))
            (should-not (mevedel-journal-index-entries workspace t))
            (should (= scans 3))
            (should-not (mevedel-journal-index-entries workspace))
            (should (= scans 4))))
      (delete-directory root t))))

(mevedel-deftest mevedel-journal-index--line ()
  ,test
  (test)
  :doc "flattens evidence and bounds UTF-8 without splitting characters"
  (should (equal "one two" (mevedel-journal-index--line "one\n\t two" 32)))
  (should (equal "abc" (mevedel-journal-index--line "abc" 3)))
  (let ((line (mevedel-journal-index--line (make-string 100 ?\u754c) 20)))
    (should (<= (string-bytes line) 20))
    (should (string-suffix-p " [...]" line))
    (should (equal line (decode-coding-string (encode-coding-string line 'utf-8) 'utf-8)))))

(mevedel-deftest mevedel-journal-index--lesson ()
  ,test
  (test)
  :doc "prefers the first Learned bullet, then Done, ignoring empty sections"
  (should (equal "User: correction" (mevedel-journal-index--lesson
                                    "## Done\n- Observed: passed\n\n## Learned\n- User: correction\n- Inferred: other\n\n## Surprised\n- none\n\n## Unfinished\n- none")))
  (should (equal "Observed: passed" (mevedel-journal-index--lesson
                                    "## Done\n- Observed: passed\n\n## Learned\n- none\n\n## Surprised\n- none\n\n## Unfinished\n- none")))
  (should (equal "No recorded lesson" (mevedel-journal-index--lesson
                                      "## Done\n- none\n\n## Learned\n- none\n\n## Surprised\n- none\n\n## Unfinished\n- none")))
  (should (equal "User: correction (turn 4)" (mevedel-journal-index--lesson
                                             "## Done\n- none\n\n## Learned\n- User: correction\n  (turn 4)\n- Inferred: other\n\n## Surprised\n- none\n\n## Unfinished\n- none"))))

(mevedel-deftest mevedel-journal-index-prompt ()
  ,test
  (test)
  :doc "external review changes refresh exact counts even when the newest digest is unchanged"
  (let* ((root (make-temp-file "mevedel-review-map-" t))
         (workspace (mevedel-workspace--create :root root)))
    (unwind-protect
        (let* ((digest (mevedel-journal-store-publish-digest
                        root
                        (list :capture-id (make-string 64 ?a) :session "reviewed"
                              :session-name "Reviewed" :workspace (make-string 64 ?b)
                              :trigger 'session-end :segment 1 :source-revision (make-string 64 ?c)
                              :turns '(1) :turn-ids (list (make-string 64 ?d))
                              :created "2026-09-07T12:00:00Z" :model "provider:model")
                        "## Done\n- Observed: passed.\n## Learned\n- none\n## Surprised\n- none\n## Unfinished\n- none"))
               (metadata (list :pass-id (make-string 64 ?e) :workspace (make-string 64 ?b)
                               :created "2026-09-06T12:00:00Z" :model "provider:model"
                               :focus "a topic" :digests (list (plist-get digest :id))
                               :proposals nil :references nil)))
          (should (string-match-p "Unreviewed digests: 1" (mevedel-journal-index-prompt workspace)))
          (mevedel-journal-store-publish-review root metadata)
          (setf (plist-get (mevedel-workspace-journal-observation workspace) :time) (- (float-time) 11))
          (should (string-match-p "Unreviewed digests: 1" (mevedel-journal-index-prompt workspace)))
          (plist-put metadata :pass-id (make-string 64 ?f))
          (plist-put metadata :focus "")
          (let ((review (mevedel-journal-store-publish-review root metadata)))
            (should (string-match-p "Unreviewed digests: 1" (mevedel-journal-index-prompt workspace)))
            (setf (plist-get (mevedel-workspace-journal-observation workspace) :time) (- (float-time) 11))
            (should (string-match-p "Unreviewed digests: 0" (mevedel-journal-index-prompt workspace)))
            (delete-file (file-name-concat (mevedel-journal-store-directory root) (plist-get review :file)))
            (setf (plist-get (mevedel-workspace-journal-observation workspace) :time) (- (float-time) 11))
            (should (string-match-p "Unreviewed digests: 1" (mevedel-journal-index-prompt workspace)))))
      (delete-directory root t)))

  :doc "renders at most five dated entries within 2 KiB, preserving canonical addresses"
  (let* ((root default-directory)
         (entries (cl-loop for n from 7 downto 1 collect
                           (list :kind 'digest :id (number-to-string n)
                                 :file (format "2026-09-0%dT12:00:00Z-%s.md" n (make-string 64 ?a))
                                 :created (format "2026-09-0%dT12:00:00Z" n)
                                 :session-name (make-string 1000 ?\u754c)
                                 :body (concat "## Done\n- none\n\n## Learned\n- User: "
                                               (make-string 3000 ?\u754c)
                                               "\n\n## Surprised\n- none\n\n## Unfinished\n- none"))))
         (workspace (mevedel-workspace--create
                     :root root :journal-observation
                     (list :root root :time (float-time) :entries entries)))
         (prompt (mevedel-journal-index-prompt workspace)))
    (should (<= (string-bytes prompt) 2048))
    (should (string-match-p "Unreviewed digests: 7" prompt))
    (should (string-match-p "dated evidence" prompt))
    (should (string-match-p "\\[\\.\\.\\.\\]" prompt))
    (should (= 6 (length (split-string prompt "journal://" t))))
    (dolist (entry (seq-take entries 5))
      (should (string-match-p
               (regexp-quote (concat "journal://" (mevedel-resource-encode-component (plist-get entry :file)))) prompt)))
    (should-not (string-match-p "2026-09-02" prompt))
    (setf (plist-get (mevedel-workspace-journal-observation workspace) :entries) nil)
    (should-not (mevedel-journal-index-prompt workspace))))

(provide 'test-mevedel-journal-index)
;;; test-mevedel-journal-index.el ends here
