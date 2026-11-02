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
(require 'bytecomp)

(mevedel-deftest mevedel-memory-store/cold-compile ()
  ,test
  (test)
  :doc "compiles with compiled dependencies without inheriting a runtime cl-lib import"
  (let* ((root (file-name-directory (locate-library "mevedel-memory-store")))
         (directory (make-temp-file "mevedel-store-compile-" t))
         (source (file-name-concat directory "mevedel-memory-store.el")))
    (unwind-protect
        (progn
          ;; Source-loading dependencies executes their compile-only imports
          ;; and hides the store's own missing runtime import.  Use bytecode
          ;; throughout the package dependency graph, as package installs do.
          ;; Compiling can evaluate dependency setup.  Keep that work out of
          ;; the ERT worker as well as keeping the cold assertion in a child.
          (with-temp-buffer
            (let ((status
                   (call-process
                    (expand-file-name invocation-name invocation-directory)
                    nil t nil "--batch" "-Q" "--eval"
                    (prin1-to-string
                     `(progn
                        (setq load-path ',load-path
                              native-comp-jit-compilation nil)
                        (when (native-comp-available-p)
                          (startup-redirect-eln-cache (expand-file-name "eln" (getenv "HOME"))))
                        (require 'bytecomp)
                        (let ((byte-compile-verbose nil)
                              (byte-compile-error-on-warn t)
                              (byte-compile-dest-file-function
                               (lambda (file)
                                 (file-name-concat ,directory (concat (file-name-base file) ".elc")))))
                          (dolist (file (directory-files ,root t "\\`mevedel.*\\.el\\'"))
                            (unless (equal (file-name-nondirectory file) "mevedel-memory-store.el")
                              (unless (byte-compile-file file)
                                (error "Dependency compilation failed: %s" file))))))))))
              (ert-info ((buffer-string))
                (should (equal 0 status))
                (should (equal "" (buffer-string))))))
          (copy-file (file-name-concat root "mevedel-memory-store.el") source)
          (with-temp-buffer
            (let ((status
                   (call-process
                    (expand-file-name invocation-name invocation-directory)
                    nil t nil "--batch" "-Q" "--eval"
                    (prin1-to-string
                     `(progn
                        (setq load-path ',(cons directory load-path)
                              native-comp-jit-compilation nil)
                        (when (native-comp-available-p)
                          (startup-redirect-eln-cache (expand-file-name "eln" (getenv "HOME"))))
                        (require 'bytecomp)
                        (when (or (featurep 'cl-lib) (fboundp 'cl-find))
                          (error "Compiler dependency must start unloaded"))
                        (let ((byte-compile-verbose nil) (byte-compile-error-on-warn t))
                          (unless (byte-compile-file ,source)
                            (error "Memory store compilation failed")))
                        (load ,(concat source "c") nil t)
                        (unless (and (featurep 'cl-lib) (fboundp 'cl-find))
                          (error "Memory store lacks its runtime dependency")))))))
              (ert-info ((buffer-string))
                (should (equal 0 status))
                (should (equal "" (buffer-string)))))))
      (delete-directory directory t))))

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
              (let ((successor (mevedel-journal-claim-acquire (mevedel-memory-store--claim-directory workspace) 180))
                    (kept (mevedel-memory-store--pin workspace id (plist-get entry :id)))
                    (spare (mevedel-memory-store--pin workspace id (plist-get omitted :id))))
                (unwind-protect
                    (progn
                      (make-directory (file-name-directory spare) t)
                      (write-region (plist-get prepared :hash) nil spare nil 'silent)
                      (mevedel-memory-store-recover workspace)
                      (should (file-exists-p kept))
                      (should-not (file-exists-p spare)))
                  (mevedel-journal-claim-settle successor 'cancelled "")))
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
            ;; Only the release that actually removed pins reports a change.
            (should (mevedel-memory-store-recover workspace))
            (should-not (mevedel-memory-store-recover workspace))
            (should-not (file-exists-p (mevedel-memory-store--pin workspace (plist-get prepared :id) (plist-get entry :id))))
            (should (equal (plist-get next :hash) (mevedel-session-control-fs-read-file pin)))
            (should-not (mevedel-journal-claim-outcome successor))
            (should (equal (list entry) (mevedel-journal-store-entries root)))))
      (when successor (mevedel-journal-claim-settle successor 'cancelled ""))
      (delete-directory root t)))
  :doc "skips only proven no-op publication while recovering spare pins and failing closed on changed evidence"
  (let* ((root (make-temp-file "mevedel-memory-store-noop-" t))
         (workspace (mevedel-workspace--create :root root))
         (identity (mevedel-workspace-identity-ensure root))
         (mevedel-memory-dirs nil)
         (mevedel-journal-cleanup--inhibit-scheduling t)
         (entry (mevedel-test-memory-store--digest root identity 1))
         (scope (mevedel-memory-scope-capture workspace))
         (claim (mevedel-journal-claim-acquire (mevedel-memory-store--claim-directory workspace) 180))
         (prepared (mevedel-memory-store-prepare workspace claim scope (list entry) ""))
         (id (plist-get prepared :id))
         (accepted (mevedel-memory-store-accept
                    workspace prepared
                    "## Promote\n- none\n## Update\n- none\n## Merge\n- none\n## Remove\n- none\n## Instructions\n- none\n## No action\n- none"
                    (list entry) "test:model" nil))
         (path (file-name-concat (mevedel-memory-store--directory workspace id) "accepted.el"))
         (pin (mevedel-memory-store--pin workspace id (plist-get entry :id)))
         (publisher (symbol-function 'mevedel-memory-store-publish))
         (calls 0) successor)
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel-memory-store-publish)
                   (lambda (&rest args) (cl-incf calls) (apply publisher args))))
          (setq successor (mevedel-journal-claim-acquire (mevedel-memory-store--claim-directory workspace) 180))
          (should (mevedel-memory-store-recover workspace))
          (should (= 1 calls))
          (should-not (file-exists-p pin))
          (setq calls 0)
          (should-not (mevedel-memory-store-recover workspace))
          (should (= 0 calls))
          ;; Interrupted spare-pin release still needs a real transaction.
          (make-directory (file-name-directory pin) t)
          (write-region (plist-get prepared :hash) nil pin nil 'silent)
          (should (mevedel-memory-store-recover workspace))
          (should (= 1 calls))
          (should-not (file-exists-p pin))
          ;; A missing publication can be recovered while source pins survive.
          (let* ((review (plist-get accepted :review))
                 (public (file-name-concat (mevedel-journal-store-directory root)
                                           (mevedel-journal-store--filename review 'consolidation))))
            (write-region (plist-get prepared :hash) nil pin nil 'silent)
            (delete-file public)
            (setq calls 0)
            (mevedel-memory-store-recover workspace)
            (should (= 1 calls))
            (should (file-exists-p public))
            (let ((text (mevedel-session-control-fs-read-file public)))
              (unwind-protect
                  (progn
                    (write-region (concat text "Changed\n") nil public nil 'silent)
                    (should-error (mevedel-memory-store-recover workspace)))
                (write-region text nil public nil 'silent))))
          ;; Public equality never substitutes for immutable accepted evidence.
          (let ((text (mevedel-session-control-fs-read-file path)))
            (unwind-protect
                (progn
                  (write-region (concat text "\n") nil path nil 'silent)
                  (should-error (mevedel-memory-store-recover workspace))
                  (delete-file path)
                  (should-error (mevedel-memory-store-recover workspace)))
              (write-region text nil path nil 'silent)))
          ;; Accepted expiry is recovered even when every pass publication
          ;; itself is already a no-op.
          (let* ((expired (mevedel-test-memory-store--digest root identity 2))
                 (journal (mevedel-journal-store-directory root))
                 (mutation (mevedel-journal-claim-acquire
                            (mevedel-journal-store-claim-directory journal 'mutation) 120))
                 (manifest (file-name-concat (mevedel-journal-store-state-directory journal)
                                             "expiry" (concat (plist-get mutation :owner) ".json")))
                 (text (json-serialize
                        (append (mevedel-journal-claim--record mutation)
                                (list :entries (vector (list :file (plist-get expired :file)
                                                             :kind "digest" :id (plist-get expired :capture-id)
                                                             :private [] :sha256 (secure-hash 'sha256 (plist-get expired :text)))))))))
            (make-directory (file-name-directory manifest) t)
            (write-region text nil manifest nil 'silent)
            (should (mevedel-journal-claim-settle mutation 'completed (secure-hash 'sha256 text)))
            (mevedel-memory-store-recover workspace)
            (should-not (file-exists-p (file-name-concat journal (plist-get expired :file))))))
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
          (let ((observation (car (mevedel-session-control-fs-run-program
                                   (list (list :op 'read :path path :coding 'utf-8-unix
                                               :max-bytes (1+ mevedel-memory-store--max-bytes)))))))
            (should (equal (mevedel-memory-store--read-lisp path)
                           (mevedel-memory-store--read-lisp path observation)))
            (should-error (mevedel-memory-store--read-lisp
                           (file-name-concat root "other.el") observation)))
          (write-region "(:id \"a\") trailing" nil path nil 'silent)
          (should-not (car (mevedel-memory-store--read-lisp path)))
          (write-region "(:id \"a\"" nil path nil 'silent)
          (should-not (car (mevedel-memory-store--read-lisp path)))
          (should (equal "(:id \"a\"" (cdr (mevedel-memory-store--read-lisp path)))))
      (delete-directory root t))))

(provide 'test-mevedel-memory-store)
;;; test-mevedel-memory-store.el ends here
