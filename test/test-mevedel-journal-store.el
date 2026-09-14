;;; test-mevedel-journal-store.el -- Journal publication tests -*- lexical-binding: t -*-

;;; Commentary:

;; Exercises immutable digest publication and discovery on real storage.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-journal-store)

(defconst mevedel-test-journal--metadata
  '(:capture-id "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
    :session "session-123" :session-name "A \"quoted\" session\nname \u754c"
    :workspace "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
    :trigger session-end :segment 2
    :source-revision "cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc"
    :turns (3 4 7)
    :turn-ids ("1111111111111111111111111111111111111111111111111111111111111111"
               "2222222222222222222222222222222222222222222222222222222222222222"
               "3333333333333333333333333333333333333333333333333333333333333333")
    :created "2026-09-07T12:34:56Z" :model "provider/model")
  "Frozen capture metadata, including non-contiguous completed turns.")

(defconst mevedel-test-journal--body
  "## Done\n- Observed: tests passed (turn 7).\n\n## Learned\n- User: use the local runner (turn 4).\n\n## Surprised\n- none\n\n## Unfinished\n- none"
  "Valid attributed digest text.")

(mevedel-deftest mevedel-journal-store-recall-p ()
  ,test
  (test)
  :doc "ordinary recall expires at the immutable age boundary and respects configuration"
  (let ((entry '(:created "2026-09-01T12:00:00Z"))
        (mevedel-journal-max-age-days 14))
    (should (mevedel-journal-store-recall-p entry
                                          (float-time (date-to-time "2026-09-15T11:59:59Z"))))
    (should-not (mevedel-journal-store-recall-p entry
                                              (float-time (date-to-time "2026-09-15T12:00:00Z"))))
    (should-not (mevedel-journal-store-recall-p entry
                                              (float-time (date-to-time "2026-09-16T12:00:00Z"))))
    (let ((mevedel-journal-max-age-days 30))
      (should (mevedel-journal-store-recall-p entry
                                            (float-time (date-to-time "2026-09-16T12:00:00Z")))))
    (let ((mevedel-journal-max-age-days nil))
      (should (mevedel-journal-store-recall-p entry
                                            (float-time (date-to-time "2027-09-16T12:00:00Z")))))
    (let ((mevedel-journal-max-age-days 0))
      (should-not (mevedel-journal-store-recall-p entry
                                                (float-time (date-to-time "2026-09-01T12:00:00Z")))))))

(mevedel-deftest mevedel-journal-store-publish-review ()
  ,test
  (test)
  :doc "publishes immutable general and focused reviews beside their digest evidence"
  (let ((root (make-temp-file "mevedel-journal-review-" t)))
    (unwind-protect
        (let* ((digest (mevedel-journal-store-publish-digest
                        root mevedel-test-journal--metadata mevedel-test-journal--body))
               (metadata (list :pass-id (make-string 64 ?d)
                               :workspace (plist-get mevedel-test-journal--metadata :workspace)
                               :created "2026-09-07T12:35:00Z" :model "provider/model"
                               :focus "" :digests (list (plist-get digest :id))
                               :proposals nil :references nil))
               (review (mevedel-journal-store-publish-review root metadata)))
          (should (eq 'consolidation (plist-get review :kind)))
          (should (equal (list (plist-get digest :id)) (plist-get review :digests)))
          (should (equal review (mevedel-journal-store-read root (plist-get review :file))))
          (should (equal review (mevedel-journal-store-publish-review root metadata)))
          (should (= 2 (length (mevedel-journal-store-entries root))))
          (plist-put metadata :focus "retry deadlines \u2013 \u754c")
          (should-error (mevedel-journal-store-publish-review root metadata))
          (plist-put metadata :pass-id (make-string 64 ?e))
          (let ((focused (mevedel-journal-store-publish-review root metadata)))
            (should (equal "retry deadlines \u2013 \u754c" (plist-get focused :focus)))
            (should (= 3 (length (mevedel-journal-store-entries root))))))
      (delete-directory root t)))

  :doc "reference-check evidence round trips without turning textual matches into verification"
  (let* ((root (make-temp-file "mevedel-review-references-" t))
         (reference '(:topic "memory://local-agents/retry.md" :token "CLOCK_MONOTONIC \u754c"
                      :scope "workspace source, bounded literal search"
                      :result "found" :checked "2026-09-07T12:00:00Z"))
         (metadata (list :pass-id (make-string 64 ?d) :workspace (make-string 64 ?b)
                         :created "2026-09-07T12:35:00Z" :model "provider:model"
                         :focus "" :digests nil :proposals (list (make-string 64 ?e))
                         :references (list reference))))
    (unwind-protect
        (let* ((entry (mevedel-journal-store-publish-review root metadata))
               (path (file-name-concat (mevedel-journal-store-directory root) (plist-get entry :file))))
          (should (equal (list reference) (plist-get entry :references)))
          (should (equal entry (mevedel-journal-store-read root (plist-get entry :file))))
          (should (string-match-p "bounded literal search" (plist-get entry :body)))
          (dolist (change '((:result . "verified") (:checked . "2026-02-30T12:00:00Z")
                            (:scope . "") (:extra . "not allowed")))
            (let ((invalid (copy-tree metadata)))
              (plist-put (car (plist-get invalid :references)) (car change) (cdr change))
              (should-error (mevedel-journal-store-publish-review root invalid))))
          (dolist (change '(("digests: []" . "digests: null")
                            ("Fully examined digests: 0" . "Fully examined digests: 99")))
            (with-temp-file path
              (insert (string-replace (car change) (cdr change) (plist-get entry :text))))
            (should-error (mevedel-journal-store-read root (plist-get entry :file)))))
      (delete-directory root t)))

  :doc "invalid review coverage cannot create a publication"
  (let* ((root (make-temp-file "mevedel-invalid-review-" t))
         (metadata (list :pass-id (make-string 64 ?d) :workspace (make-string 64 ?b)
                         :created "2026-09-07T12:35:00Z" :model "provider:model"
                         :focus "" :digests nil :proposals nil :references nil)))
    (unwind-protect
        (progn
          (dolist (change (list (cons :pass-id "") (cons :focus nil)
                                (cons :digests '("not a digest ID"))
                                (cons :proposals (list (make-string 64 ?a) (make-string 64 ?a)))
                                (cons :digests (cl-loop for n below 21 collect (format "%064x" n)))))
            (let ((invalid (copy-tree metadata)))
              (plist-put invalid (car change) (cdr change))
              (should-error (mevedel-journal-store-publish-review root invalid))))
          (should-not (file-exists-p (mevedel-journal-store-directory root))))
      (delete-directory root t))))

(mevedel-deftest mevedel-journal-store-publish-digest ()
  ,test
  (test)
  :doc "publishes once, round trips metadata, and preserves the first result"
  (let ((root (make-temp-file "mevedel-journal-" t)))
    (unwind-protect
        (let* ((entry (mevedel-journal-store-publish-digest
                       root mevedel-test-journal--metadata
                       mevedel-test-journal--body))
               (file (plist-get entry :file)))
          (should (string-prefix-p "2026-09-07T12:34:56Z-" file))
          (should (equal entry (mevedel-journal-store-read root file)))
          (should (equal (plist-get entry :body) mevedel-test-journal--body))
          (should (equal (plist-get entry :session-name)
                         "A \"quoted\" session\nname \u754c"))
          (should (equal (plist-get entry :turns) '(3 4 7)))
          (should (equal (list entry) (mevedel-journal-store-entries root)))
          (should
           (equal entry
                  (mevedel-journal-store-publish-digest
                   root mevedel-test-journal--metadata
                   (string-replace "tests passed" "later answer"
                                   mevedel-test-journal--body))))
          (should-not (file-exists-p (file-name-concat root ".gitignore"))))
      (delete-directory root t)))

  :doc "preserves long completed-turn coverage with a maximum-sized digest body"
  (let* ((root (make-temp-file "mevedel-journal-long-" t))
         (metadata (copy-tree mevedel-test-journal--metadata))
         (body mevedel-test-journal--body))
    (unwind-protect
        (progn
          (plist-put metadata :turns (number-sequence 1 1000))
          (plist-put metadata :turn-ids
                     (mapcar (lambda (n) (secure-hash 'sha256 (number-to-string n)))
                             (plist-get metadata :turns)))
          (setq body (string-replace "tests passed"
                                     (make-string (+ 12 (- mevedel-context-summary--digest-max-bytes
                                                          (string-bytes body))) ?a)
                                     body))
          (let* ((entry (mevedel-journal-store-publish-digest root metadata body))
                 (read (mevedel-journal-store-read root (plist-get entry :file))))
            (should (> (string-bytes (plist-get entry :text)) (* 24 1024)))
            (should (equal body (plist-get read :body)))
            (should (equal (plist-get metadata :turn-ids) (plist-get read :turn-ids)))))
      (delete-directory root t)))

  :doc "refuses invalid capture metadata and bodies before creating state"
  (let ((root (make-temp-file "mevedel-journal-" t)))
    (unwind-protect
        (progn
          (dolist (change '((:capture-id . "../capture")
                            (:workspace . "") (:source-revision . "mutable")
                            (:trigger . manual) (:segment . -1)
                            (:turns . nil) (:turns . (0 1))
                            (:turns . (3 3)) (:turns . (4 3))
                            (:turns . (3 . 4))
                            (:turn-ids . nil) (:turn-ids . ("same" "same" "same"))
                            (:created . "2026-02-30T12:00:00Z")
                            (:created . "2026-09-07T12:00:00+00:00")
                            (:model . "")))
            (should-error
             (mevedel-journal-store-publish-digest
              root (plist-put (copy-tree mevedel-test-journal--metadata)
                              (car change) (cdr change))
              mevedel-test-journal--body)
             :type 'mevedel-journal-store-invalid))
          (should-error
           (mevedel-journal-store-publish-digest
            root (append mevedel-test-journal--metadata '(:unknown "value"))
            mevedel-test-journal--body)
           :type 'mevedel-journal-store-invalid)
          (should-error
           (mevedel-journal-store-publish-digest
            root mevedel-test-journal--metadata "## Done\n- Missing sections"))
          (should-not (directory-files root nil directory-files-no-dot-files-regexp)))
      (delete-directory root t)))

  :doc "does not overwrite a conflicting or corrupt publication"
  (let ((root (make-temp-file "mevedel-journal-" t)))
    (unwind-protect
        (let* ((entry (mevedel-journal-store-publish-digest
                       root mevedel-test-journal--metadata mevedel-test-journal--body))
               (path (file-name-concat (mevedel-journal-store-directory root)
                                       (plist-get entry :file))))
          (should-error
           (mevedel-journal-store-publish-digest
            root (plist-put (copy-tree mevedel-test-journal--metadata)
                            :turns '(9))
            mevedel-test-journal--body)
           :type 'mevedel-journal-store-invalid)
          (should (equal entry (mevedel-journal-store-read root (plist-get entry :file))))
          (write-region "interrupted external write" nil path nil 'silent)
          (should-error
           (mevedel-journal-store-publish-digest
            root mevedel-test-journal--metadata mevedel-test-journal--body)
           :type 'mevedel-journal-store-invalid)
          (should (equal "interrupted external write"
                         (mevedel-session-control-fs-read-file path))))
      (delete-directory root t))))

(mevedel-deftest mevedel-journal-store-directory
  (:doc "requires an absolute workspace path without creating state")
  (should-error (mevedel-journal-store-directory "relative"))
  (should (equal "/tmp/workspace/.mevedel/journal"
                 (mevedel-journal-store-directory "/tmp/workspace/"))))

(mevedel-deftest mevedel-journal-store-entries
  (:doc "discovers only valid public entries, newest first, without timestamp collisions")
  (let ((root (make-temp-file "mevedel-journal-" t)))
    (unwind-protect
        (let* ((directory (mevedel-journal-store-directory root))
               (state (file-name-concat directory "state")))
          (should-not (mevedel-journal-store-entries root))
          (make-directory state t)
          (write-region "private capture evidence" nil
                        (file-name-concat state "job.el") nil 'silent)
          (write-region "a plain Markdown file is not a publication" nil
                        (file-name-concat directory "note.md") nil 'silent)
          (should-not (mevedel-journal-store-entries root))
          (let* ((first (mevedel-journal-store-publish-digest
                         root mevedel-test-journal--metadata mevedel-test-journal--body))
                 (second (mevedel-journal-store-publish-digest
                          root (plist-put (copy-tree mevedel-test-journal--metadata)
                                          :capture-id (make-string 64 ?d))
                          mevedel-test-journal--body))
                 (third (mevedel-journal-store-publish-digest
                         root (append (list :created "2026-09-08T01:00:00Z"
                                           :capture-id (make-string 64 ?e))
                                      (cl-loop for (key value) on mevedel-test-journal--metadata
                                               by #'cddr
                                               unless (memq key '(:created :capture-id))
                                               append (list key value)))
                         mevedel-test-journal--body)))
            (should-not (equal (plist-get first :file) (plist-get second :file)))
            (should (= 3 (length (mevedel-journal-store-entries root))))
            (should (equal third (car (mevedel-journal-store-entries root))))
            ;; An external malformed entry and removal change discovery.
            (write-region "not frontmatter" nil
                          (file-name-concat directory (plist-get first :file)) nil 'silent)
            (delete-file (file-name-concat directory (plist-get second :file)))
            (should (equal (list third) (mevedel-journal-store-entries root)))))
      (delete-directory root t))))

(mevedel-deftest mevedel-journal-store-read
  (:doc "rejects private paths, traversal, mismatched identity, and symlinks")
  (let ((root (make-temp-file "mevedel-journal-" t))
        (outside (make-temp-file "mevedel-journal-outside-" t)))
    (unwind-protect
        (let* ((entry (mevedel-journal-store-publish-digest
                       root mevedel-test-journal--metadata mevedel-test-journal--body))
               (directory (mevedel-journal-store-directory root))
               (file (plist-get entry :file))
               (path (file-name-concat directory file))
               (bytes (mevedel-session-control-fs-read-file path)))
          (dolist (name (list "state" "state/job.el" "../secret" path
                              (concat "nested/" file) (concat "../" file)))
            (should-error (mevedel-journal-store-read root name)
                          :type 'mevedel-journal-store-invalid))
          (dolist (changed (list (string-replace "kind: digest" "kind: other" bytes)
                                 (string-replace "kind: digest" "kind: digest\nkind: digest" bytes)
                                 (string-replace "kind: digest" "kind: digest\nunknown: 1" bytes)
                                 (string-replace "## Done" "## Wrong" bytes)
                                 (string-replace "2026-09-07T12:34:56Z" "2026-09-08T12:34:56Z" bytes)))
            (write-region changed nil path nil 'silent)
            (should-error (mevedel-journal-store-read root file)
                          :type 'mevedel-journal-store-invalid)
            (should-not (mevedel-journal-store-entries root)))
          (delete-file path)
          (let ((external (file-name-concat outside "record.md")))
            (write-region bytes nil external nil 'silent)
            (make-symbolic-link external path)
            (should-error (mevedel-journal-store-read root file))
            (should-error (mevedel-journal-store-entries root))
            (should-error
             (mevedel-journal-store-publish-digest
              root mevedel-test-journal--metadata mevedel-test-journal--body))
            (should (equal bytes (mevedel-session-control-fs-read-file external)))
            (delete-file path))
          (delete-directory directory t)
          (make-symbolic-link outside directory)
          (should-error (mevedel-journal-store-entries root))
          (should-error
           (mevedel-journal-store-publish-digest
            root mevedel-test-journal--metadata mevedel-test-journal--body)))
      (delete-directory root t)
      (delete-directory outside t))))

(mevedel-deftest mevedel-journal-store--validate-metadata ()
  ,test
  (test)
  :doc "reserves the entire body allowance before admitting capture metadata"
  (let ((mevedel-journal-store--entry-max-bytes
         (+ (string-bytes (mevedel-journal-store--header mevedel-test-journal--metadata))
            mevedel-context-summary--digest-max-bytes)))
    (should-error (mevedel-journal-store--validate-metadata mevedel-test-journal--metadata)
                  :type 'mevedel-journal-store-invalid)
    (cl-incf mevedel-journal-store--entry-max-bytes)
    (should (equal mevedel-test-journal--metadata
                   (mevedel-journal-store--validate-metadata mevedel-test-journal--metadata)))))

(mevedel-deftest mevedel-journal-store-covered-turns ()
  ,test
  (test)
  :doc "retains only published turn identities after the public digest is removed"
  (let ((root (make-temp-file "mevedel-journal-coverage-" t)))
    (unwind-protect
        (progn
          (should-not (mevedel-journal-store-covered-turns root))
          (let ((entry (mevedel-journal-store-publish-digest
                        root mevedel-test-journal--metadata mevedel-test-journal--body)))
            (delete-file (file-name-concat (mevedel-journal-store-directory root) (plist-get entry :file)))
            (should-not (mevedel-journal-store-entries root))
            (should (equal (plist-get entry :turn-ids) (mevedel-journal-store-covered-turns root)))
            (let ((bytes (mevedel-session-control-fs-read-file
                          (file-name-concat (mevedel-journal-store-directory root)
                                            "state" "coverage"
                                            (concat (plist-get entry :capture-id) ".json")))))
              (should-not (string-match-p "tests passed\\|session-name\\|quoted" bytes)))))
      (delete-directory root t)))

  :doc "repairs publication interrupted before coverage, without replacing the first body"
  (let* ((root (make-temp-file "mevedel-journal-coverage-" t))
         (directory (mevedel-journal-store-directory root))
         (state (file-name-concat directory "state")))
    (unwind-protect
        (progn
          (make-directory directory t)
          (write-region "block control state" nil state nil 'silent)
          (should-error (mevedel-journal-store-publish-digest
                         root mevedel-test-journal--metadata mevedel-test-journal--body))
          (should (= 1 (length (mevedel-journal-store-entries root))))
          (delete-file state)
          (let ((entry (mevedel-journal-store-publish-digest
                        root mevedel-test-journal--metadata
                        (string-replace "tests passed" "later result" mevedel-test-journal--body))))
            (should (equal mevedel-test-journal--body (plist-get entry :body)))
            (should (equal (plist-get entry :turn-ids) (mevedel-journal-store-covered-turns root)))))
      (delete-directory root t)))

  :doc "rejects corrupt or conflicting coverage instead of reopening completed turns"
  (let ((root (make-temp-file "mevedel-journal-coverage-" t)))
    (unwind-protect
        (let* ((entry (mevedel-journal-store-publish-digest
                       root mevedel-test-journal--metadata mevedel-test-journal--body))
               (path (file-name-concat (mevedel-journal-store-directory root)
                                      "state" "coverage" (concat (plist-get entry :capture-id) ".json"))))
          (write-region "{}" nil path nil 'silent)
          (should-error (mevedel-journal-store-covered-turns root))
          (should-error (mevedel-journal-store-publish-digest
                         root mevedel-test-journal--metadata mevedel-test-journal--body)))
      (delete-directory root t))))

(mevedel-deftest mevedel-journal-store-publish-decision ()
  ,test
  (test)
  :doc "immutable decision metadata round-trips independently of the original review"
  (let* ((root (make-temp-file "mevedel-journal-decision-" t))
         (metadata (list :decision-id (make-string 64 ?a) :pass-id (make-string 64 ?b)
                         :proposal-id (make-string 64 ?c) :workspace (make-string 64 ?d)
                         :created "2026-09-07T12:00:00Z" :status 'rejected
                         :reason "The earlier choice was abandoned." :state-hash (make-string 64 ?e))))
    (unwind-protect
        (let ((entry (mevedel-journal-store-publish-decision root metadata)))
          (should (eq 'decision (plist-get entry :kind)))
          (should (eq 'rejected (plist-get entry :status)))
          (should (equal entry (mevedel-journal-store-publish-decision root metadata)))
          (should (equal entry (mevedel-journal-store-read root (plist-get entry :file))))
          (should (equal (list entry) (mevedel-journal-store-entries root)))
          (should-error (mevedel-journal-store-publish-decision root (plist-put (copy-sequence metadata) :reason "Different reason")))
          (should-error (mevedel-journal-store-publish-decision root (plist-put (copy-sequence metadata) :status 'unknown)))
          (should-error (mevedel-journal-store-publish-decision root (append metadata '(:before "Private content"))))
          (should-error (mevedel-journal-store--decode (concat (plist-get entry :text) "Injected body") (plist-get entry :file))))
      (delete-directory root t))))

(mevedel-deftest mevedel-journal-store-id-p
  (:doc "accepts exactly one 64-digit lowercase hex identity")
  (progn
    (should (mevedel-journal-store-id-p (make-string 64 ?a)))
    (should-not (mevedel-journal-store-id-p (make-string 63 ?a)))
    (should-not (mevedel-journal-store-id-p (concat (make-string 64 ?a) "\n")))
    (should-not (mevedel-journal-store-id-p (make-string 64 ?A)))
    (should-not (mevedel-journal-store-id-p nil))))

(mevedel-deftest mevedel-journal-store-file-name-p
  (:doc "accepts one public entry filename and rejects private or nested spellings")
  (let ((file (concat "2026-09-07T12:00:00Z-" (make-string 64 ?b) ".md")))
    (should (mevedel-journal-store-file-name-p file))
    (should-not (mevedel-journal-store-file-name-p (concat "state/" file)))
    (should-not (mevedel-journal-store-file-name-p (string-replace ".md" ".json" file)))
    (should-not (mevedel-journal-store-file-name-p 'symbol))))

(mevedel-deftest mevedel-journal-store-timestamp
  (:doc "spells canonical UTC seconds that validation round-trips")
  (progn
    (should (equal "2000-01-01T00:00:00Z" (mevedel-journal-store-timestamp 946684800)))
    (should (mevedel-journal-store--utc-time-p (mevedel-journal-store-timestamp 1757332800)))
    (should-not (mevedel-journal-store--utc-time-p "2026-09-07T12:00:00+02:00"))))

(mevedel-deftest mevedel-journal-store-claim-directory
  (:doc "names one private state directory per work scope")
  (progn
    (should (equal "/w/.mevedel/journal/state/digest-run"
                   (mevedel-journal-store-claim-directory "/w/.mevedel/journal" 'digest-run)))
    (should (equal "/w/.mevedel/journal/state/mutation"
                   (mevedel-journal-store-claim-directory "/w/.mevedel/journal" 'mutation)))))

(mevedel-deftest mevedel-journal-store-expired-marker
  (:doc "records expiry beside the private state, never at the public entry")
  (should (equal "/j/state/expired/entry.md.json"
                 (mevedel-journal-store-expired-marker "/j" "entry.md"))))

(mevedel-deftest mevedel-journal-store-entry-for-capture
  (:doc "finds the entry published from a capture identity")
  (let ((entries (list (list :id "x" :capture-id (make-string 64 ?1))
                       (list :id "y" :capture-id (make-string 64 ?2)))))
    (should (equal "y" (plist-get (mevedel-journal-store-entry-for-capture
                                   entries (make-string 64 ?2)) :id)))
    (should-not (mevedel-journal-store-entry-for-capture entries (make-string 64 ?3)))
    (should-not (mevedel-journal-store-entry-for-capture nil (make-string 64 ?1)))))

(provide 'test-mevedel-journal-store)
;;; test-mevedel-journal-store.el ends here
