;;; test-niwa-journal-index.el -- Journal discovery tests -*- lexical-binding: t -*-

;;; Commentary:

;; Checks disposable discovery observations independently of publication owners.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'niwa-journal-index)

(niwa-deftest niwa-journal-index--signature ()
  ,test
  (test)
  :doc "detects additions, same-size edits, replacements and expiry without tracking reads"
  (let* ((root (make-temp-file "journal-facts-" t))
         (directory (niwa-journal-store-directory root))
         (file (file-name-concat directory (concat "2026-09-07T12:00:00Z-" (make-string 64 ?a) ".md")))
         (marker (niwa-journal-store-expired-marker directory (file-name-nondirectory file))))
    (unwind-protect
        (progn
          (make-directory directory t)
          (let ((empty (niwa-journal-index--signature root)))
            (should empty)
            (write-region "before" nil file nil 'silent)
            (should-not (equal empty (niwa-journal-index--signature root))))
          (let ((before (niwa-journal-index--signature root))
                (mtime (file-attribute-modification-time (file-attributes file))))
            (with-temp-buffer (insert-file-contents file))
            (should (equal before (niwa-journal-index--signature root)))
            (write-region "edited" nil file nil 'silent)
            (set-file-times file mtime)
            (should-not (equal before (niwa-journal-index--signature root))))
          (let ((before (niwa-journal-index--signature root)))
            (make-directory (file-name-directory marker) t)
            (write-region "expired" nil marker nil 'silent)
            (should-not (equal before (niwa-journal-index--signature root))))
          (let ((before (niwa-journal-index--signature root)))
            (delete-file marker)
            (should-not (equal before (niwa-journal-index--signature root))))
          (let ((before (niwa-journal-index--signature root)))
            (delete-file file)
            (write-region "edited" nil file nil 'silent)
            (should-not (equal before (niwa-journal-index--signature root)))))
      (delete-directory root t)))

  :doc "declines nonphysical and remote observations"
  (let* ((root (make-temp-file "journal-link-facts-" t))
         (directory (niwa-journal-store-directory root))
         (other (file-name-concat root "elsewhere")))
    (unwind-protect
        (progn
          (make-directory (file-name-directory directory) t)
          (make-directory other)
          (make-symbolic-link other directory)
          (should-not (niwa-journal-index--signature root))
          (let ((reads 0))
            (cl-letf (((symbol-function 'file-attributes)
                       (lambda (&rest _) (cl-incf reads) nil)))
              (should-not (niwa-journal-index--signature "/ssh:unreachable:/workspace")))
            (should (= reads 0))))
      (delete-directory root t))))

(niwa-deftest niwa-journal-index-unreviewed (:vars ((niwa-journal-max-age-days nil)))
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
                   (niwa-journal-index-unreviewed (append (list first second third) reviews)))))
  (should-not (niwa-journal-index-unreviewed nil)))

(niwa-deftest niwa-journal-index-entries (:vars ((niwa-journal-max-age-days nil)))
  ,test
  (test)
  :doc "throttles scans but observes external additions, removals, and workspace root changes"
  (let* ((root (make-temp-file "niwa-journal-index-" t))
         (workspace (niwa-workspace--create :root root))
         (scan (symbol-function 'niwa-journal-store-entries))
         (scans 0))
    (unwind-protect
        (cl-letf (((symbol-function 'niwa-journal-store-entries)
                   (lambda (path) (cl-incf scans) (funcall scan path))))
          (should-not (niwa-journal-index-entries workspace t))
          (should (= scans 0))
          (should-not (niwa-journal-index-entries workspace))
          (should-not (niwa-journal-index-entries workspace))
          (should (= scans 1))
          (let* ((entry (niwa-journal-store-publish-digest
                         root
                         (list :capture-id (make-string 64 ?a) :session "index-session"
                               :session-name "Index" :workspace (make-string 64 ?b)
                               :trigger 'session-end :segment 1 :source-revision (make-string 64 ?c)
                               :turns '(1) :turn-ids (list (make-string 64 ?d))
                               :created "2026-09-07T12:00:00Z" :model "provider:model")
                         "## Done\n- Observed: Tests passed (turn 1).\n\n## Learned\n- none\n\n## Surprised\n- none\n\n## Unfinished\n- none"))
                 (path (file-name-concat (niwa-journal-store-directory root) (plist-get entry :file))))
            (should-not (niwa-journal-index-entries workspace))
            (setf (plist-get (niwa-workspace-journal-observation workspace) :time) (- (float-time) 11))
            (should (equal (list entry) (niwa-journal-index-entries workspace)))
            (should (= scans 2))
            (setf (plist-get (niwa-workspace-journal-observation workspace) :time) (- (float-time) 11))
            (should (equal (list entry) (niwa-journal-index-entries workspace)))
            (should (= scans 2))
            ;; Cached-only completion never even probes source facts.  Age
            ;; filtering still applies without another storage observation.
            (let ((probes 0))
              (cl-letf (((symbol-function 'niwa-journal-index--signature)
                         (lambda (_) (cl-incf probes) nil)))
                (should (equal (list entry) (niwa-journal-index-entries workspace t)))
                (let ((niwa-journal-max-age-days 1)
                      (clock (symbol-function 'float-time))
                      (now (float-time (date-to-time "2026-09-10T12:00:00Z"))))
                  (cl-letf (((symbol-function 'float-time)
                             (lambda (&optional value) (if value (funcall clock value) now))))
                    (should-not (niwa-journal-index-entries workspace t)))))
              (should (= probes 0)))
            (delete-file path)
            (should (equal (list entry) (niwa-journal-index-entries workspace t)))
            (setf (plist-get (niwa-workspace-journal-observation workspace) :time) (- (float-time) 11))
            (should-not (niwa-journal-index-entries workspace))
            (should (= scans 3))
            (setf (niwa-workspace-root workspace) (file-name-concat root "different"))
            (should-not (niwa-journal-index-entries workspace t))
            (should (= scans 3))
            (should-not (niwa-journal-index-entries workspace))
            (should (= scans 4))))
      (delete-directory root t)))

  :doc "fresh source facts invalidate expired and corrupted entries"
  (let* ((root (make-temp-file "journal-index-invalidation-" t))
         (workspace (niwa-workspace--create :root root)))
    (unwind-protect
        (let* ((entry (niwa-journal-store-publish-digest
                       root
                       (list :capture-id (make-string 64 ?a) :session "index-session"
                             :session-name "Index" :workspace (make-string 64 ?b)
                             :trigger 'session-end :segment 1 :source-revision (make-string 64 ?c)
                             :turns '(1) :turn-ids (list (make-string 64 ?d))
                             :created "2026-09-07T12:00:00Z" :model "provider:model")
                       "## Done\n- Observed: passed.\n## Learned\n- none\n## Surprised\n- none\n## Unfinished\n- none"))
               (directory (niwa-journal-store-directory root))
               (file (file-name-concat directory (plist-get entry :file)))
               (marker (niwa-journal-store-expired-marker directory (plist-get entry :file))))
          (should (equal (list entry) (niwa-journal-index-entries workspace)))
          (make-directory (file-name-directory marker) t)
          (write-region "expired" nil marker nil 'silent)
          (setf (plist-get (niwa-workspace-journal-observation workspace) :time) (- (float-time) 11))
          (should-not (niwa-journal-index-entries workspace))
          (delete-file marker)
          (setf (plist-get (niwa-workspace-journal-observation workspace) :time) (- (float-time) 11))
          (should (equal (list entry) (niwa-journal-index-entries workspace)))
          (write-region "invalid record" nil file nil 'silent)
          (setf (plist-get (niwa-workspace-journal-observation workspace) :time) (- (float-time) 11))
          (should-not (niwa-journal-index-entries workspace)))
      (delete-directory root t)))

  :doc "retries failed reads and does not retain a snapshot across a source race"
  (let* ((root (make-temp-file "journal-index-retry-" t))
         (workspace (niwa-workspace--create :root root))
         (scan (symbol-function 'niwa-journal-store-entries))
         (reads 0))
    (unwind-protect
        (cl-letf (((symbol-function 'niwa-journal-store-entries)
                   (lambda (path)
                     (cl-incf reads)
                     (pcase reads
                       (1 (error "Temporary read failure"))
                       (2 (prog1 (funcall scan path)
                            (make-directory (niwa-journal-store-directory path) t)))
                       (_ (funcall scan path))))))
          (dotimes (_ 4)
            (when (niwa-workspace-journal-observation workspace)
              (setf (plist-get (niwa-workspace-journal-observation workspace) :time) (- (float-time) 11)))
            (should-not (niwa-journal-index-entries workspace)))
          (should (= reads 3)))
      (delete-directory root t))))

(niwa-deftest niwa-journal-index--line (:vars ((niwa-journal-max-age-days nil)))
  ,test
  (test)
  :doc "flattens evidence and bounds UTF-8 without splitting characters"
  (should (equal "one two" (niwa-journal-index--line "one\n\t two" 32)))
  (should (equal "abc" (niwa-journal-index--line "abc" 3)))
  (let ((line (niwa-journal-index--line (make-string 100 ?\u754c) 20)))
    (should (<= (string-bytes line) 20))
    (should (string-suffix-p " [...]" line))
    (should (equal line (decode-coding-string (encode-coding-string line 'utf-8) 'utf-8)))))

(niwa-deftest niwa-journal-index--lesson (:vars ((niwa-journal-max-age-days nil)))
  ,test
  (test)
  :doc "prefers the first Learned bullet, then Done, ignoring empty sections"
  (should (equal "User: correction" (niwa-journal-index--lesson
                                    "## Done\n- Observed: passed\n\n## Learned\n- User: correction\n- Inferred: other\n\n## Surprised\n- none\n\n## Unfinished\n- none")))
  (should (equal "Observed: passed" (niwa-journal-index--lesson
                                    "## Done\n- Observed: passed\n\n## Learned\n- none\n\n## Surprised\n- none\n\n## Unfinished\n- none")))
  (should (equal "No recorded lesson" (niwa-journal-index--lesson
                                      "## Done\n- none\n\n## Learned\n- none\n\n## Surprised\n- none\n\n## Unfinished\n- none")))
  (should (equal "User: correction (turn 4)" (niwa-journal-index--lesson
                                             "## Done\n- none\n\n## Learned\n- User: correction\n  (turn 4)\n- Inferred: other\n\n## Surprised\n- none\n\n## Unfinished\n- none"))))

(niwa-deftest niwa-journal-index-prompt (:vars ((niwa-journal-max-age-days nil)))
  ,test
  (test)
  :doc "external review changes refresh exact counts even when the newest digest is unchanged"
  (let* ((root (make-temp-file "niwa-review-map-" t))
         (workspace (niwa-workspace--create :root root)))
    (unwind-protect
        (let* ((digest (niwa-journal-store-publish-digest
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
          (should (string-match-p "Unreviewed digests: 1" (niwa-journal-index-prompt workspace)))
          (niwa-journal-store-publish-review root metadata)
          (setf (plist-get (niwa-workspace-journal-observation workspace) :time) (- (float-time) 11))
          (should (string-match-p "Unreviewed digests: 1" (niwa-journal-index-prompt workspace)))
          (plist-put metadata :pass-id (make-string 64 ?f))
          (plist-put metadata :focus "")
          (let ((review (niwa-journal-store-publish-review root metadata)))
            (should (string-match-p "Unreviewed digests: 1" (niwa-journal-index-prompt workspace)))
            (setf (plist-get (niwa-workspace-journal-observation workspace) :time) (- (float-time) 11))
            (should (string-match-p "Unreviewed digests: 0" (niwa-journal-index-prompt workspace)))
            (delete-file (file-name-concat (niwa-journal-store-directory root) (plist-get review :file)))
            (setf (plist-get (niwa-workspace-journal-observation workspace) :time) (- (float-time) 11))
            (should (string-match-p "Unreviewed digests: 1" (niwa-journal-index-prompt workspace)))))
      (delete-directory root t)))

  :doc "new digests preserve surviving row bytes while growing and rolling the bounded list"
  (let* ((root (make-temp-file "journal-stable-rows-" t))
         (workspace (niwa-workspace--create :root root))
         entries previous-rows)
    (unwind-protect
        (cl-loop
         for n from 1 to 10
         do
         (push (niwa-journal-store-publish-digest
                root
                (list :capture-id (secure-hash 'sha256 (number-to-string n))
                      :session "stable-rows" :session-name (make-string 100 ?\u754c)
                      :workspace (make-string 64 ?b) :trigger 'session-end
                      :segment 1 :source-revision (make-string 64 ?c)
                      :turns (list n) :turn-ids (list (secure-hash 'sha256 (format "turn-%d" n)))
                      :created (format "2026-09-07T12:%02d:00Z" n) :model "provider:model")
                (concat "## Done\n- none\n## Learned\n- Observed: "
                        (make-string 1000 ?\u754c)
                        "\n## Surprised\n- none\n## Unfinished\n- none"))
               entries)
         (when (niwa-workspace-journal-observation workspace)
           (setf (plist-get (niwa-workspace-journal-observation workspace) :time)
                 (- (float-time) 11)))
         (let* ((prompt (niwa-journal-index-prompt workspace))
                (rows (seq-filter (lambda (line) (string-prefix-p "- " line))
                                  (split-string prompt "\n" t)))
                (row-bytes (mapcar (lambda (row) (encode-coding-string row 'utf-8)) rows)))
           (should (<= (string-bytes prompt) 2048))
           (should (string-match-p (format "Unreviewed digests: %d\n" n) prompt))
           (should (= (length rows) (min n 5)))
           (cl-loop for row in rows
                    for entry in (seq-take entries 5)
                    do (should (string-match-p
                                (regexp-quote
                                 (concat "memory://journal/"
                                         (niwa-resource-encode-component (plist-get entry :file))))
                                row)))
           (should (equal (cdr row-bytes) (seq-take previous-rows 4)))
           (setq previous-rows row-bytes)))
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
         (workspace (niwa-workspace--create
                     :root root :journal-observation
                     (list :root root :time (float-time) :entries entries)))
         (prompt (niwa-journal-index-prompt workspace)))
    (should (<= (string-bytes prompt) 2048))
    (should (string-match-p "Unreviewed digests: 7" prompt))
    (should (string-match-p "dated evidence" prompt))
    (should (string-match-p "\\[\\.\\.\\.\\]" prompt))
    (should (= 6 (length (split-string prompt "memory://journal/" t))))
    (dolist (entry (seq-take entries 5))
      (should (string-match-p
               (regexp-quote (concat "memory://journal/" (niwa-resource-encode-component (plist-get entry :file)))) prompt)))
    (should-not (string-match-p "2026-09-02" prompt))
    (setf (plist-get (niwa-workspace-journal-observation workspace) :entries) nil)
    (should-not (niwa-journal-index-prompt workspace))))

(provide 'test-niwa-journal-index)
;;; test-niwa-journal-index.el ends here
