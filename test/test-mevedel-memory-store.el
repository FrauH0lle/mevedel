;;; test-mevedel-memory-store.el -- Durable consolidation state -*- lexical-binding: t -*-

;;; Commentary:

;; Real journal expiry and restart reads verify evidence retention boundaries.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-journal-cleanup)
(require 'mevedel-journal-index)
(require 'mevedel-memory-scope)
(require 'mevedel-memory-store)
(require 'mevedel-system)

(defun mevedel-test-memory-store--digest (root identity number)
  "Publish one complete dated digest NUMBER under ROOT with workspace IDENTITY."
  (mevedel-journal-store-publish-digest
   root (list :capture-id (format "%064x" number) :session "closed" :session-name "Closed"
              :workspace identity :trigger 'session-end :segment 1
              :source-revision (make-string 64 ?b) :turns '(1) :turn-ids (list (format "%064x" number))
              :created "2020-01-01T00:00:00Z" :model "test:model")
   "## Done\n- Observed: Tests passed.\n\n## Learned\n- none\n\n## Surprised\n- none\n\n## Unfinished\n- none"))

(mevedel-deftest mevedel-memory-store-prepare ()
  ,test
  (test)
  :doc "fenced cancellation releases pass pins but preserves unreviewed evidence for retry"
  (let* ((root (make-temp-file "mevedel-memory-store-" t))
         (workspace (mevedel-workspace--create :root root))
         (identity (mevedel-workspace-identity-ensure root))
         (mevedel-memory-dirs '("memory"))
         (mevedel-journal-max-age-days 0)
         (journal (mevedel-journal-store-directory root))
         claim)
    (unwind-protect
        (progn
          (make-directory (file-name-concat root "memory"))
          (let ((coding-system-for-write 'utf-8-unix))
            (write-region "Gr\u00fc\u00dfe\r\n" nil (file-name-concat root "memory" "topic.md") nil 'silent))
          (let* ((entry (mevedel-test-memory-store--digest root identity 1))
                 (scope (mevedel-memory-scope-capture workspace)))
            (setq claim (mevedel-journal-claim-acquire (file-name-concat (mevedel-journal-store-state-directory journal) "consolidation") 180))
            (let* ((prepared (mevedel-memory-store-prepare workspace claim scope (list entry) ""))
                   (id (plist-get prepared :id))
                   (restored (mevedel-memory-store-read workspace id)))
              (should (equal prepared restored))
              (should (equal scope (plist-get restored :scope)))
              (should (equal prepared (mevedel-memory-store-prepare workspace claim scope (list entry) "")))
              (should-not (mevedel-journal-cleanup-expired workspace t))
              (should (= 1 (length (mevedel-journal-index-unreviewed (mevedel-journal-store-entries root)))))
              (should-not (mevedel-journal-claim-outcome claim))
              (should-error (mevedel-memory-store-release workspace id))
              (should (mevedel-journal-claim-settle claim 'cancelled ""))
              (mevedel-memory-store-release workspace id)
              (mevedel-memory-store-release workspace id)
              (should (= 0 (mevedel-journal-cleanup-expired workspace t)))
              (should (equal entry (mevedel-journal-store-read root (plist-get entry :file))))
              (should (= 1 (length (mevedel-journal-index-unreviewed
                                    (mevedel-journal-store-entries root))))))))
      (when claim (mevedel-journal-claim-settle claim 'cancelled ""))
      (delete-directory root t)))
  :doc "accepted expiry is recovered before selected evidence can acquire a new pin"
  (let* ((root (make-temp-file "mevedel-memory-expiry-recovery-" t))
         (workspace (mevedel-workspace--create :root root))
         (identity (mevedel-workspace-identity-ensure root))
         (mevedel-memory-dirs nil)
         (journal (mevedel-journal-store-directory root))
         claim mutation)
    (unwind-protect
        (let* ((entry (mevedel-test-memory-store--digest root identity 1))
               (scope (mevedel-memory-scope-capture workspace)))
          (setq claim (mevedel-journal-claim-acquire (file-name-concat (mevedel-journal-store-state-directory journal) "consolidation") 180)
                mutation (mevedel-journal-claim-acquire (file-name-concat (mevedel-journal-store-state-directory journal) "mutation") 120))
          (let* ((path (file-name-concat (mevedel-journal-store-state-directory journal) "expiry" (concat (plist-get mutation :owner) ".json")))
                 (text (json-serialize
                        (append (mevedel-journal-claim--record mutation)
                                (list :entries (vector (list :file (plist-get entry :file)
                                                             :kind "digest" :id (plist-get entry :capture-id) :private []
                                                             :sha256 (secure-hash 'sha256 (plist-get entry :text)))))))))
            (make-directory (file-name-directory path) t)
            (write-region text nil path nil 'silent)
            (should (mevedel-journal-claim-settle mutation 'completed (secure-hash 'sha256 text)))
            ;; The old owner stopped after accepting expiry, before removing its entry.
            (should (equal (list entry) (mevedel-journal-store-entries root)))
            (should-error (mevedel-memory-store-prepare workspace claim scope (list entry) ""))
            (should-not (mevedel-journal-store-entries root))
            (should-not (file-exists-p (file-name-concat (mevedel-journal-store-state-directory journal) "evidence-pins")))
            (should-not (file-exists-p (file-name-concat (mevedel-journal-store-state-directory journal) "passes" (plist-get claim :owner))))))
      (when claim (mevedel-journal-claim-settle claim 'cancelled ""))
      (when mutation (mevedel-journal-claim-settle mutation 'cancelled ""))
      (delete-directory root t))))

(mevedel-deftest mevedel-memory-store-accept ()
  ,test
  (test)
  :doc "an accepted no-action bundle survives restart before its public review advances coverage"
  (let* ((root (make-temp-file "mevedel-memory-accepted-" t))
         (workspace (mevedel-workspace--create :root root))
         (identity (mevedel-workspace-identity-ensure root))
         (mevedel-memory-dirs nil)
         (mevedel-journal-max-age-days 0)
         (reply "## Promote\n- none\n## Update\n- none\n## Merge\n- none\n## Remove\n- none\n## Instructions\n- none\n## No action\n- No supported changes.")
         claim)
    (unwind-protect
        (let* ((entry (mevedel-test-memory-store--digest root identity 1))
               (scope (mevedel-memory-scope-capture workspace)))
          (setq claim (mevedel-journal-claim-acquire
                       (file-name-concat (mevedel-journal-store-state-directory (mevedel-journal-store-directory root)) "consolidation") 180))
          (let* ((prepared (mevedel-memory-store-prepare workspace claim scope (list entry) ""))
                 (id (plist-get prepared :id)))
            (should-not (mevedel-memory-store-accepted workspace id))
            (should-error (mevedel-memory-store-accept workspace prepared reply nil "test:model" nil))
            (should-not (mevedel-journal-claim-outcome claim))
            (let ((pin (file-name-concat (mevedel-journal-store-state-directory (mevedel-journal-store-directory root)) "evidence-pins"
                                         (plist-get entry :id) (concat id ".pin"))))
              (delete-file pin)
              (should-error (mevedel-memory-store-accept workspace prepared reply (list entry) "test:model" nil))
              (should-not (mevedel-journal-claim-outcome claim))
              (write-region (plist-get prepared :hash) nil pin nil 'silent))
            (let ((accepted (mevedel-memory-store-accept workspace prepared reply (list entry) "test:model" nil)))
              (should (equal accepted (mevedel-memory-store-accepted workspace id)))
              (should-not (plist-get accepted :proposals))
              (should (equal (list (plist-get entry :id)) (plist-get (plist-get accepted :review) :digests))))
            (should (= 1 (length (mevedel-journal-index-unreviewed (mevedel-journal-store-entries root)))))
            (should (= 0 (mevedel-journal-cleanup-expired workspace t)))
            (should-error (mevedel-memory-store-release workspace id))
            (let ((published (mevedel-memory-store-publish workspace id)))
              (should (eq 'consolidation (plist-get published :kind)))
              (should-not (mevedel-journal-index-unreviewed (mevedel-journal-store-entries root)))
              (should (equal published (mevedel-memory-store-publish workspace id)))
              (should (= 1 (mevedel-journal-cleanup-expired workspace t)))
              (should (equal published (mevedel-memory-store-publish workspace id)))
              (should (= 1 (length (mevedel-journal-store-entries root)))))))
      (when claim (mevedel-journal-claim-settle claim 'cancelled ""))
      (delete-directory root t)))
  :doc "accepted proposals retain exact global before-state without publishing private bodies"
  (let* ((root (make-temp-file "mevedel-memory-proposal-store-" t))
         (memory (make-temp-file "mevedel-global-proposal-store-" t))
         (workspace (mevedel-workspace--create :root root))
         (identity (mevedel-workspace-identity-ensure root))
         (mevedel-memory-dirs (list memory))
         (mevedel-journal-max-age-days 0)
         (original (encode-coding-string "PRIVATE_GUIDANCE Gr\u00fc\u00dfe\r\n" 'utf-8-unix))
         claim)
    (unwind-protect
        (progn
          (write-region "- [Topic](topic.md): existing guide\n" nil (file-name-concat memory "MEMORY.md") nil 'silent)
          (let ((coding-system-for-write 'no-conversion))
            (write-region original nil (file-name-concat memory "topic.md") nil 'silent))
          (let* ((entry (mevedel-test-memory-store--digest root identity 1))
                 (omitted (mevedel-test-memory-store--digest root identity 2))
                 (scope (mevedel-memory-scope-capture workspace))
                 (root-id (caar (plist-get scope :roots)))
                 (reply (format (concat "## Promote\n- none\n## Update\n````proposal\nroot: %S\n"
                                        "file: \"topic.md\"\ntype: \"project\"\ntitle: \"Test conditions\"\n"
                                        "hook: \"Keep conditions with test results\"\nreason: \"Preserve context\"\nevidence: [%S]\n"
                                        "---\nProposed private replacement.\n````\n## Merge\n- none\n## Remove\n- none\n"
                                        "## Instructions\n- none\n## No action\n- none") root-id (plist-get entry :id))))
            (setq claim (mevedel-journal-claim-acquire
                         (file-name-concat (mevedel-journal-store-state-directory (mevedel-journal-store-directory root)) "consolidation") 180))
            (dotimes (_ 3)
              (let* ((parsed (mevedel-memory-proposal-parse reply (plist-get scope :roots) (list (plist-get entry :id))))
                     (bound (mevedel-memory-store--proposal scope (plist-get claim :owner) (car (plist-get parsed :proposals)))))
                (should (equal '("MEMORY.md" "topic.md") (mapcar #'car (plist-get bound :before))))))
            (let* ((prepared (mevedel-memory-store-prepare workspace claim scope (list entry omitted) "test conditions"))
                   (id (plist-get prepared :id))
                   (accepted (mevedel-memory-store-accept workspace prepared reply (list entry) "test:model" nil))
                   (proposal (car (plist-get accepted :proposals))))
              (should (string-match-p "\\`[0-9a-f]\\{64\\}\\'" (plist-get proposal :id)))
              (should (equal '("MEMORY.md" "topic.md") (mapcar #'car (plist-get proposal :before))))
              (should (equal original (plist-get (cdr (assoc "topic.md" (plist-get proposal :before))) :bytes)))
              (should (equal accepted (mevedel-memory-store-accepted workspace id)))
              (should (equal original (mevedel-session-control-fs-read-file
                                      (file-name-concat memory "topic.md") 'no-conversion)))
              (let ((published (mevedel-memory-store-publish workspace id)))
                (should (equal (list (plist-get proposal :id)) (plist-get published :proposals)))
                (should-not (string-match-p "PRIVATE_GUIDANCE\\|Proposed private replacement" (plist-get published :text)))
                (should (equal "test conditions" (plist-get published :focus)))
                (should (= 2 (length (mevedel-journal-index-unreviewed (mevedel-journal-store-entries root))))))
              (should (= 0 (mevedel-journal-cleanup-expired workspace t)))
              (should (= 2 (length (mevedel-journal-index-unreviewed
                                    (mevedel-journal-store-entries root)))))
              (should (equal omitted (mevedel-journal-store-read root (plist-get omitted :file))))
              (should-error (mevedel-memory-store-release workspace id))
              (let* ((path (file-name-concat (mevedel-journal-store-state-directory (mevedel-journal-store-directory root)) "passes" id "accepted.el"))
                     (text (mevedel-session-control-fs-read-file path)))
                (write-region (concat text "\nChanged after acceptance") nil path nil 'silent)
                (should-error (mevedel-memory-store-accepted workspace id))
                (should-error (mevedel-memory-store-publish workspace id))
                (write-region text nil path nil 'silent)))))
      (when claim (mevedel-journal-claim-settle claim 'cancelled ""))
      (delete-directory root t)
      (delete-directory memory t))))

(mevedel-deftest mevedel-memory-store-recover ()
  ,test
  (test)
  :doc "releases cancelled pins while leaving live preparation and incomplete empty directories alone"
  (let* ((root (make-temp-file "mevedel-memory-store-recover-" t))
         (workspace (mevedel-workspace--create :root root))
         (identity (mevedel-workspace-identity-ensure root))
         (mevedel-memory-dirs nil)
         (entry (mevedel-test-memory-store--digest root identity 1))
         (scope (mevedel-memory-scope-capture workspace))
         (key (mevedel-memory-store--claim-directory workspace))
         (claim (mevedel-journal-claim-acquire key 180))
         (prepared (mevedel-memory-store-prepare workspace claim scope (list entry) ""))
         successor)
    (unwind-protect
        (progn
          (should (mevedel-journal-claim-settle claim 'cancelled ""))
          (setq successor (mevedel-journal-claim-acquire key 180))
          (let* ((next (mevedel-memory-store-prepare workspace successor scope (list entry) ""))
                 (pin (mevedel-memory-store--pin workspace (plist-get successor :owner) (plist-get entry :id))))
            (make-directory (mevedel-memory-store--directory workspace (make-string 64 ?f)) t)
            (mevedel-memory-store-recover workspace)
            (mevedel-memory-store-recover workspace)
            (should-not (file-exists-p (mevedel-memory-store--pin workspace (plist-get prepared :id) (plist-get entry :id))))
            (should (equal (plist-get next :hash) (mevedel-session-control-fs-read-file pin)))
            (should-not (mevedel-journal-claim-outcome successor))
            (should (equal (list entry) (mevedel-journal-store-entries root)))))
      (when successor (mevedel-journal-claim-settle successor 'cancelled ""))
      (delete-directory root t))))

(mevedel-deftest mevedel-memory-store--read-lisp
  (:doc "returns parsed data with its exact text, and nil data for trailing garbage or unreadable records")
  (let* ((root (make-temp-file "mevedel-memory-read-lisp-" t))
         (path (file-name-concat root "record.el")))
    (unwind-protect
        (progn
          (write-region "(:id \"a\" :n 1)\n" nil path nil 'silent)
          (should (equal (cons '(:id "a" :n 1) "(:id \"a\" :n 1)\n")
                         (mevedel-memory-store--read-lisp path)))
          (write-region "(:id \"a\") trailing" nil path nil 'silent)
          (should-not (car (mevedel-memory-store--read-lisp path)))
          (write-region "(:id \"a\"" nil path nil 'silent)
          (should-not (car (mevedel-memory-store--read-lisp path)))
          (should (equal "(:id \"a\"" (cdr (mevedel-memory-store--read-lisp path)))))
      (delete-directory root t))))

(provide 'test-mevedel-memory-store)
;;; test-mevedel-memory-store.el ends here
