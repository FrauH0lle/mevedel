;;; test-niwa-memory-store.el -- Durable consolidation state -*- lexical-binding: t -*-

;;; Commentary:

;; Real journal expiry and restart reads verify evidence retention boundaries.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'niwa-journal-cleanup)
(require 'niwa-journal-index)
(require 'niwa-memory-scope)
(require 'niwa-memory-store)
(require 'niwa-session-control-fs)
(require 'niwa-system)
(require 'bytecomp)

(niwa-deftest niwa-memory-store/cold-compile ()
  ,test
  (test)
  :doc "compiles with compiled dependencies without inheriting a runtime cl-lib import"
  (let* ((root (file-name-directory (locate-library "niwa-memory-store")))
         (directory (make-temp-file "niwa-store-compile-" t))
         (source (file-name-concat directory "niwa-memory-store.el")))
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
                          (dolist (file (directory-files ,root t "\\`niwa.*\\.el\\'"))
                            (unless (equal (file-name-nondirectory file) "niwa-memory-store.el")
                              (unless (byte-compile-file file)
                                (error "Dependency compilation failed: %s" file))))))))))
              (ert-info ((buffer-string))
                (should (equal 0 status))
                (should (equal "" (buffer-string))))))
          (copy-file (file-name-concat root "niwa-memory-store.el") source)
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

(defun niwa-test-memory-store--digest (root identity number)
  "Publish one complete dated digest NUMBER under ROOT with workspace IDENTITY."
  (niwa-journal-store-publish-digest
   root (list :capture-id (format "%064x" number) :session "closed" :session-name "Closed"
              :workspace identity :trigger 'session-end :segment 1
              :source-revision (make-string 64 ?b) :turns '(1) :turn-ids (list (format "%064x" number))
              :created "2020-01-01T00:00:00Z" :model "test:model")
   "## Done\n- Observed: Tests passed.\n\n## Learned\n- none\n\n## Surprised\n- none\n\n## Unfinished\n- none"))

(niwa-deftest niwa-memory-store-prepare ()
  ,test
  (test)
  :doc "fenced cancellation releases pass pins but preserves unreviewed evidence for retry"
  (let* ((root (make-temp-file "niwa-memory-store-" t))
         (workspace (niwa-workspace--create :root root))
         (identity (niwa-workspace-identity-ensure root))
         (niwa-memory-dirs '("memory"))
         (niwa-journal-max-age-days 0)
         (journal (niwa-journal-store-directory root))
         claim)
    (unwind-protect
        (progn
          (make-directory (file-name-concat root "memory"))
          (let ((coding-system-for-write 'utf-8-unix))
            (write-region "Gr\u00fc\u00dfe\r\n" nil (file-name-concat root "memory" "topic.md") nil 'silent))
          (let* ((entry (niwa-test-memory-store--digest root identity 1))
                 (scope (niwa-memory-scope-capture workspace)))
            (setq claim (niwa-journal-claim-acquire (file-name-concat (niwa-journal-store-state-directory journal) "consolidation") 180))
            (let* ((prepared (niwa-memory-store-prepare workspace claim scope (list entry) ""))
                   (id (plist-get prepared :id))
                   (restored (niwa-memory-store-read workspace id)))
              (should (equal prepared restored))
              (should (equal (append '(:decision-format slots) scope) (plist-get restored :scope)))
              (should (equal prepared (niwa-memory-store-prepare workspace claim scope (list entry) "")))
              (should-not (niwa-journal-cleanup-expired workspace t))
              (should (= 1 (length (niwa-journal-index-unreviewed (niwa-journal-store-entries root)))))
              (should-not (niwa-journal-claim-outcome claim))
              (should-error (niwa-memory-store-release workspace id))
              (should (niwa-journal-claim-settle claim 'cancelled ""))
              (niwa-memory-store-release workspace id)
              (niwa-memory-store-release workspace id)
              (should (= 0 (plist-get (niwa-journal-cleanup-expired workspace t) :count)))
              (should (equal entry (niwa-journal-store-read root (plist-get entry :file))))
              (should (= 1 (length (niwa-journal-index-unreviewed
                                    (niwa-journal-store-entries root))))))))
      (when claim (niwa-journal-claim-settle claim 'cancelled ""))
      (delete-directory root t)))
  :doc "accepted expiry is recovered before selected evidence can acquire a new pin"
  (let* ((root (make-temp-file "niwa-memory-expiry-recovery-" t))
         (workspace (niwa-workspace--create :root root))
         (identity (niwa-workspace-identity-ensure root))
         (niwa-memory-dirs nil)
         (journal (niwa-journal-store-directory root))
         claim mutation)
    (unwind-protect
        (let* ((entry (niwa-test-memory-store--digest root identity 1))
               (scope (niwa-memory-scope-capture workspace)))
          (setq claim (niwa-journal-claim-acquire (file-name-concat (niwa-journal-store-state-directory journal) "consolidation") 180)
                mutation (niwa-journal-claim-acquire (file-name-concat (niwa-journal-store-state-directory journal) "mutation") 120))
          (let* ((path (file-name-concat (niwa-journal-store-state-directory journal) "expiry" (concat (plist-get mutation :owner) ".json")))
                 (text (json-serialize
                        (append (niwa-journal-claim--record mutation)
                                (list :entries (vector (list :file (plist-get entry :file)
                                                             :kind "digest" :id (plist-get entry :capture-id) :private []
                                                             :sha256 (secure-hash 'sha256 (plist-get entry :text)))))))))
            (make-directory (file-name-directory path) t)
            (write-region text nil path nil 'silent)
            (should (niwa-journal-claim-settle mutation 'completed (secure-hash 'sha256 text)))
            ;; The old owner stopped after accepting expiry, before removing its entry.
            (should (equal (list entry) (niwa-journal-store-entries root)))
            (should-error (niwa-memory-store-prepare workspace claim scope (list entry) ""))
            (should-not (niwa-journal-store-entries root))
            (should-not (file-exists-p (file-name-concat (niwa-journal-store-state-directory journal) "evidence-pins")))
            (should-not (file-exists-p (file-name-concat (niwa-journal-store-state-directory journal) "passes" (plist-get claim :owner))))))
      (when claim (niwa-journal-claim-settle claim 'cancelled ""))
      (when mutation (niwa-journal-claim-settle mutation 'cancelled ""))
      (delete-directory root t))))

(niwa-deftest niwa-memory-store-accept ()
  ,test
  (test)
  :doc "an accepted no-action bundle survives restart before its public review advances coverage"
  (let* ((root (make-temp-file "niwa-memory-accepted-" t))
         (workspace (niwa-workspace--create :root root))
         (identity (niwa-workspace-identity-ensure root))
         (niwa-memory-dirs nil)
         (niwa-journal-max-age-days 0)
         (reply "## Promote\n- none\n## Update\n- none\n## Merge\n- none\n## Remove\n- none\n## Instructions\n- none\n## No action\n- No supported changes.")
         claim)
    (unwind-protect
        (let* ((entry (niwa-test-memory-store--digest root identity 1))
               (scope (niwa-memory-scope-capture workspace)))
          (setq claim (niwa-journal-claim-acquire
                       (file-name-concat (niwa-journal-store-state-directory (niwa-journal-store-directory root)) "consolidation") 180))
          (let* ((prepared (niwa-memory-store-prepare workspace claim scope (list entry) ""))
                 (id (plist-get prepared :id)))
            (should-not (niwa-memory-store-accepted workspace id))
            (should-error (niwa-memory-store-accept workspace prepared reply nil "test:model" nil))
            (should-not (niwa-journal-claim-outcome claim))
            (let ((pin (file-name-concat (niwa-journal-store-state-directory (niwa-journal-store-directory root)) "evidence-pins"
                                         (plist-get entry :id) (concat id ".pin"))))
              (delete-file pin)
              (should-error (niwa-memory-store-accept workspace prepared reply (list entry) "test:model" nil))
              (should-not (niwa-journal-claim-outcome claim))
              (write-region (plist-get prepared :hash) nil pin nil 'silent))
            (let ((accepted (niwa-memory-store-accept workspace prepared reply (list entry) "test:model" nil)))
              (should (equal accepted (niwa-memory-store-accepted workspace id)))
              (should-not (plist-get accepted :proposals))
              (should (equal (list (plist-get entry :id)) (plist-get (plist-get accepted :review) :digests))))
            (should (= 1 (length (niwa-journal-index-unreviewed (niwa-journal-store-entries root)))))
            (should (= 0 (plist-get (niwa-journal-cleanup-expired workspace t) :count)))
            (should-error (niwa-memory-store-release workspace id))
            (let ((published (niwa-memory-store-publish workspace id)))
              (should (eq 'consolidation (plist-get published :kind)))
              (should-not (niwa-journal-index-unreviewed (niwa-journal-store-entries root)))
              (should (equal published (niwa-memory-store-publish workspace id)))
              (should (= 1 (plist-get (niwa-journal-cleanup-expired workspace t) :count)))
              (should (equal published (niwa-memory-store-publish workspace id)))
              (should (= 1 (length (niwa-journal-store-entries root)))))))
      (when claim (niwa-journal-claim-settle claim 'cancelled ""))
      (delete-directory root t)))
  :doc "accepted proposals retain exact global before-state without publishing private bodies"
  (let* ((root (make-temp-file "niwa-memory-proposal-store-" t))
         (memory (make-temp-file "niwa-global-proposal-store-" t))
         (workspace (niwa-workspace--create :root root))
         (identity (niwa-workspace-identity-ensure root))
         (niwa-memory-dirs (list memory))
         (niwa-journal-max-age-days 0)
         (original (encode-coding-string "PRIVATE_GUIDANCE Gr\u00fc\u00dfe\r\n" 'utf-8-unix))
         claim)
    (unwind-protect
        (progn
          (write-region "- [Topic](topic.md): existing guide\n" nil (file-name-concat memory "MEMORY.md") nil 'silent)
          (let ((coding-system-for-write 'no-conversion))
            (write-region original nil (file-name-concat memory "topic.md") nil 'silent))
          (let* ((entry (niwa-test-memory-store--digest root identity 1))
                 (omitted (niwa-test-memory-store--digest root identity 2))
                 (scope (niwa-memory-scope-capture workspace))
                 (root-id (caar (plist-get scope :roots)))
                 (reply (format (concat "## Promote\n- none\n## Update\n````proposal\nroot: %S\n"
                                        "file: \"topic.md\"\ntype: \"project\"\ntitle: \"Test conditions\"\n"
                                        "hook: \"Keep conditions with test results\"\nreason: \"Preserve context\"\nevidence: [%S]\n"
                                        "---\nProposed private replacement.\n````\n## Merge\n- none\n## Remove\n- none\n"
                                        "## Instructions\n- none\n## No action\n- none") root-id (plist-get entry :id))))
            (setq claim (niwa-journal-claim-acquire
                         (file-name-concat (niwa-journal-store-state-directory (niwa-journal-store-directory root)) "consolidation") 180))
            (dotimes (_ 3)
              (let* ((parsed (niwa-memory-proposal-parse reply (plist-get scope :roots) (list (plist-get entry :id))))
                     (bound (niwa-memory-store--proposal scope (plist-get claim :owner) (car (plist-get parsed :proposals)))))
                (should (equal '("MEMORY.md" "topic.md") (mapcar #'car (plist-get bound :before))))))
            (let* ((prepared (niwa-memory-store-prepare workspace claim scope (list entry omitted) "test conditions"))
                   (id (plist-get prepared :id))
                   (accepted (niwa-memory-store-accept workspace prepared reply (list entry) "test:model" nil))
                   (proposal (car (plist-get accepted :proposals))))
              (should (string-match-p "\\`[0-9a-f]\\{64\\}\\'" (plist-get proposal :id)))
              (should (equal '("MEMORY.md" "topic.md") (mapcar #'car (plist-get proposal :before))))
              (should (equal original (plist-get (cdr (assoc "topic.md" (plist-get proposal :before))) :bytes)))
              (should (equal accepted (niwa-memory-store-accepted workspace id)))
              (should (equal original (niwa-session-control-fs-read-file
                                      (file-name-concat memory "topic.md") 'no-conversion)))
              (let ((published (niwa-memory-store-publish workspace id)))
                (should (equal (list (plist-get proposal :id)) (plist-get published :proposals)))
                (should-not (string-match-p "PRIVATE_GUIDANCE\\|Proposed private replacement" (plist-get published :text)))
                (should (equal "test conditions" (plist-get published :focus)))
                (should (= 2 (length (niwa-journal-index-unreviewed (niwa-journal-store-entries root))))))
              (should (= 0 (plist-get (niwa-journal-cleanup-expired workspace t) :count)))
              (should (= 2 (length (niwa-journal-index-unreviewed
                                    (niwa-journal-store-entries root)))))
              (should (equal omitted (niwa-journal-store-read root (plist-get omitted :file))))
              (let ((successor (niwa-journal-claim-acquire (niwa-memory-store--claim-directory workspace) 180))
                    (kept (niwa-memory-store--pin workspace id (plist-get entry :id)))
                    (spare (niwa-memory-store--pin workspace id (plist-get omitted :id))))
                (unwind-protect
                    (progn
                      (make-directory (file-name-directory spare) t)
                      (write-region (plist-get prepared :hash) nil spare nil 'silent)
                      (niwa-memory-store-recover workspace)
                      (should (file-exists-p kept))
                      (should-not (file-exists-p spare)))
                  (niwa-journal-claim-settle successor 'cancelled "")))
              (should-error (niwa-memory-store-release workspace id))
              (let* ((path (file-name-concat (niwa-journal-store-state-directory (niwa-journal-store-directory root)) "passes" id "accepted.el"))
                     (text (niwa-session-control-fs-read-file path)))
                (write-region (concat text "\nChanged after acceptance") nil path nil 'silent)
                (should-error (niwa-memory-store-accepted workspace id))
                (should-error (niwa-memory-store-publish workspace id))
                (write-region text nil path nil 'silent)))))
      (when claim (niwa-journal-claim-settle claim 'cancelled ""))
      (delete-directory root t)
      (delete-directory memory t))))

(niwa-deftest niwa-memory-store-recover ()
  ,test
  (test)
  :doc "releases cancelled pins while leaving live preparation and incomplete empty directories alone"
  (let* ((root (make-temp-file "niwa-memory-store-recover-" t))
         (workspace (niwa-workspace--create :root root))
         (identity (niwa-workspace-identity-ensure root))
         (niwa-memory-dirs nil)
         (entry (niwa-test-memory-store--digest root identity 1))
         (scope (niwa-memory-scope-capture workspace))
         (key (niwa-memory-store--claim-directory workspace))
         (claim (niwa-journal-claim-acquire key 180))
         (prepared (niwa-memory-store-prepare workspace claim scope (list entry) ""))
         successor)
    (unwind-protect
        (progn
          (should (niwa-journal-claim-settle claim 'cancelled ""))
          (setq successor (niwa-journal-claim-acquire key 180))
          (let* ((next (niwa-memory-store-prepare workspace successor scope (list entry) ""))
                 (pin (niwa-memory-store--pin workspace (plist-get successor :owner) (plist-get entry :id))))
            (make-directory (niwa-memory-store--directory workspace (make-string 64 ?f)) t)
            ;; Only the release that actually removed pins reports a change.
            (should (niwa-memory-store-recover workspace))
            (should-not (niwa-memory-store-recover workspace))
            (should-not (file-exists-p (niwa-memory-store--pin workspace (plist-get prepared :id) (plist-get entry :id))))
            (should (equal (plist-get next :hash) (niwa-session-control-fs-read-file pin)))
            (should-not (niwa-journal-claim-outcome successor))
            (should (equal (list entry) (niwa-journal-store-entries root)))))
      (when successor (niwa-journal-claim-settle successor 'cancelled ""))
      (delete-directory root t)))
  :doc "skips only proven no-op publication while recovering spare pins and failing closed on changed evidence"
  (let* ((root (make-temp-file "niwa-memory-store-noop-" t))
         (workspace (niwa-workspace--create :root root))
         (identity (niwa-workspace-identity-ensure root))
         (niwa-memory-dirs nil)
         (niwa-journal-cleanup--inhibit-scheduling t)
         (entry (niwa-test-memory-store--digest root identity 1))
         (scope (niwa-memory-scope-capture workspace))
         (claim (niwa-journal-claim-acquire (niwa-memory-store--claim-directory workspace) 180))
         (prepared (niwa-memory-store-prepare workspace claim scope (list entry) ""))
         (id (plist-get prepared :id))
         (accepted (niwa-memory-store-accept
                    workspace prepared
                    "## Promote\n- none\n## Update\n- none\n## Merge\n- none\n## Remove\n- none\n## Instructions\n- none\n## No action\n- none"
                    (list entry) "test:model" nil))
         (path (file-name-concat (niwa-memory-store--directory workspace id) "accepted.el"))
         (pin (niwa-memory-store--pin workspace id (plist-get entry :id)))
         (publisher (symbol-function 'niwa-memory-store-publish))
         (calls 0) successor)
    (unwind-protect
        (cl-letf (((symbol-function 'niwa-memory-store-publish)
                   (lambda (&rest args) (cl-incf calls) (apply publisher args))))
          (setq successor (niwa-journal-claim-acquire (niwa-memory-store--claim-directory workspace) 180))
          (should (niwa-memory-store-recover workspace))
          (should (= 1 calls))
          (should-not (file-exists-p pin))
          (setq calls 0)
          (should-not (niwa-memory-store-recover workspace))
          (should (= 0 calls))
          ;; Interrupted spare-pin release still needs a real transaction.
          (make-directory (file-name-directory pin) t)
          (write-region (plist-get prepared :hash) nil pin nil 'silent)
          (should (niwa-memory-store-recover workspace))
          (should (= 1 calls))
          (should-not (file-exists-p pin))
          ;; A missing publication can be recovered while source pins survive.
          (let* ((review (plist-get accepted :review))
                 (public (file-name-concat (niwa-journal-store-directory root)
                                           (niwa-journal-store--filename review 'consolidation))))
            (write-region (plist-get prepared :hash) nil pin nil 'silent)
            (delete-file public)
            (setq calls 0)
            (niwa-memory-store-recover workspace)
            (should (= 1 calls))
            (should (file-exists-p public))
            (let ((text (niwa-session-control-fs-read-file public)))
              (unwind-protect
                  (progn
                    (write-region (concat text "Changed\n") nil public nil 'silent)
                    (should-error (niwa-memory-store-recover workspace)))
                (write-region text nil public nil 'silent))))
          ;; Public equality never substitutes for immutable accepted evidence.
          (let ((text (niwa-session-control-fs-read-file path)))
            (unwind-protect
                (progn
                  (write-region (concat text "\n") nil path nil 'silent)
                  (should-error (niwa-memory-store-recover workspace))
                  (delete-file path)
                  (should-error (niwa-memory-store-recover workspace)))
              (write-region text nil path nil 'silent)))
          ;; Accepted expiry is recovered even when every pass publication
          ;; itself is already a no-op.
          (let* ((expired (niwa-test-memory-store--digest root identity 2))
                 (journal (niwa-journal-store-directory root))
                 (mutation (niwa-journal-claim-acquire
                            (niwa-journal-store-claim-directory journal 'mutation) 120))
                 (manifest (file-name-concat (niwa-journal-store-state-directory journal)
                                             "expiry" (concat (plist-get mutation :owner) ".json")))
                 (text (json-serialize
                        (append (niwa-journal-claim--record mutation)
                                (list :entries (vector (list :file (plist-get expired :file)
                                                             :kind "digest" :id (plist-get expired :capture-id)
                                                             :private [] :sha256 (secure-hash 'sha256 (plist-get expired :text)))))))))
            (make-directory (file-name-directory manifest) t)
            (write-region text nil manifest nil 'silent)
            (should (niwa-journal-claim-settle mutation 'completed (secure-hash 'sha256 text)))
            (niwa-memory-store-recover workspace)
            (should-not (file-exists-p (file-name-concat journal (plist-get expired :file))))))
      (when successor (niwa-journal-claim-settle successor 'cancelled ""))
      (delete-directory root t))))

(niwa-deftest niwa-memory-store--read-lisp
  (:doc "returns parsed data with its exact text, and nil data for trailing garbage or unreadable records")
  (let* ((root (make-temp-file "niwa-memory-read-lisp-" t))
         (path (file-name-concat root "record.el")))
    (unwind-protect
        (progn
          (write-region "(:id \"a\" :n 1)\n" nil path nil 'silent)
          (should (equal (cons '(:id "a" :n 1) "(:id \"a\" :n 1)\n")
                         (niwa-memory-store--read-lisp path)))
          (let ((observation (car (niwa-session-control-fs-run-program
                                   (list (list :op 'read :path path :coding 'utf-8-unix
                                               :max-bytes (1+ niwa-memory-store--max-bytes)))))))
            (should (equal (niwa-memory-store--read-lisp path)
                           (niwa-memory-store--read-lisp path observation)))
            (should-error (niwa-memory-store--read-lisp
                           (file-name-concat root "other.el") observation)))
          (write-region "(:id \"a\") trailing" nil path nil 'silent)
          (should-not (car (niwa-memory-store--read-lisp path)))
          (write-region "(:id \"a\"" nil path nil 'silent)
          (should-not (car (niwa-memory-store--read-lisp path)))
          (should (equal "(:id \"a\"" (cdr (niwa-memory-store--read-lisp path)))))
      (delete-directory root t))))

(defun niwa-test-memory-store--with-pass (function)
  "Prepare one real pass and call FUNCTION with its workspace and record."
  (let* ((root (make-temp-file "niwa-memory-store-read-" t))
         (workspace (niwa-workspace--create :root root))
         (identity (niwa-workspace-identity-ensure root))
         (niwa-memory-dirs nil)
         (journal (niwa-journal-store-directory root))
         claim)
    (unwind-protect
        (let ((entry (niwa-test-memory-store--digest root identity 1)))
          (setq claim (niwa-journal-claim-acquire
                       (file-name-concat (niwa-journal-store-state-directory journal) "consolidation") 180))
          (funcall function workspace
                   (niwa-memory-store-prepare
                    workspace claim (niwa-memory-scope-capture workspace) (list entry) "")))
      (when claim (niwa-journal-claim-settle claim 'cancelled ""))
      (delete-directory root t))))

(defun niwa-test-memory-store--retire (workspace id)
  "Write WORKSPACE's retirement marker for pass ID, as expiry does first."
  (let ((marker (file-name-concat (niwa-journal-store-state-directory
                                   (niwa-journal-store-directory (niwa-workspace-root workspace)))
                                  "retired-passes" id)))
    (make-directory (file-name-directory marker) t)
    (write-region "{}" nil marker nil 'silent)))

(niwa-deftest niwa-memory-store-read ()
  ,test
  (test)
  :doc "an unobserved read probes retirement and reads the record in one program, then the claim"
  (niwa-test-memory-store--with-pass
   (lambda (workspace prepared)
     (let ((run (symbol-function 'niwa-session-control-fs-run-program))
           (calls 0))
       (cl-letf (((symbol-function 'niwa-session-control-fs-run-program)
                  (lambda (&rest args) (cl-incf calls) (apply run args))))
         (should (equal prepared (niwa-memory-store-read workspace (plist-get prepared :id)))))
       (should (= 2 calls)))))

  :doc "retirement, absence and malformed records keep their results"
  (dolist (case '((:retired (error "Memory pass has expired"))
                  (:retired-absent (error "Memory pass has expired"))
                  (:absent niwa-session-control-fs-absent)
                  (:malformed (error "Invalid prepared memory pass"))))
    (niwa-test-memory-store--with-pass
     (lambda (workspace prepared)
       (let* ((id (plist-get prepared :id))
              (path (file-name-concat (niwa-memory-store--directory workspace id) "prepared.el")))
         (pcase (car case)
           (:retired (niwa-test-memory-store--retire workspace id))
           (:retired-absent (niwa-test-memory-store--retire workspace id) (delete-file path))
           (:absent (delete-file path))
           (:malformed (write-region "(:id" nil path nil 'silent)))
         (let ((err (should-error (niwa-memory-store-read workspace id))))
           (ert-info ((format "%S" case))
             (if (consp (nth 1 case))
                 (should (equal (nth 1 case) err))
               (should (eq (nth 1 case) (car err))))))))))

  :doc "a bound scan observation still decides retirement"
  (niwa-test-memory-store--with-pass
   (lambda (workspace prepared)
     (let ((id (plist-get prepared :id))
           (root (niwa-workspace-root workspace)))
       (niwa-test-memory-store--retire workspace id)
       (let ((niwa-journal-cleanup--retired-observation (list root)))
         (should (equal prepared (niwa-memory-store-read workspace id))))
       (let ((niwa-journal-cleanup--retired-observation (list root id)))
         (should (equal '(error "Memory pass has expired")
                        (should-error (niwa-memory-store-read workspace id)))))))))

(defun niwa-test-memory-store--accept (workspace prepared)
  "Accept PREPARED's journal evidence in WORKSPACE with a no-action review."
  (niwa-memory-store-accept
   workspace prepared
   "## Promote\n- none\n## Update\n- none\n## Merge\n- none\n## Remove\n- none\n## Instructions\n- none\n## No action\n- none"
   (niwa-journal-store-entries (niwa-workspace-root workspace)) "test:model" nil))

(niwa-deftest niwa-memory-store-accepted ()
  ,test
  (test)
  :doc "a batched read carries the retirement probe with the bundle reads, then the claim"
  (niwa-test-memory-store--with-pass
   (lambda (workspace prepared)
     (let* ((id (plist-get prepared :id))
            (accepted (niwa-test-memory-store--accept workspace prepared))
            (run (symbol-function 'niwa-session-control-fs-run-program))
            (calls 0))
       (cl-letf (((symbol-function 'niwa-session-control-fs-run-program)
                  (lambda (&rest args) (cl-incf calls) (apply run args))))
         (should (equal accepted (niwa-memory-store-accepted workspace id t))))
       (should (= 2 calls))
       (should (equal accepted (niwa-memory-store-accepted workspace id))))))

  :doc "batched retirement, absence and malformed records keep their results"
  (dolist (case '((:retired (error "Memory pass has expired"))
                  (:retired-absent niwa-session-control-fs-absent)
                  (:absent niwa-session-control-fs-absent)
                  ;; Fails closed before the claim read, as before batching.
                  (:malformed error)))
    (niwa-test-memory-store--with-pass
     (lambda (workspace prepared)
       (let* ((id (plist-get prepared :id))
              (path (file-name-concat (niwa-memory-store--directory workspace id) "prepared.el")))
         (niwa-test-memory-store--accept workspace prepared)
         (pcase (car case)
           (:retired (niwa-test-memory-store--retire workspace id))
           (:retired-absent (niwa-test-memory-store--retire workspace id) (delete-file path))
           (:absent (delete-file path))
           (:malformed (write-region "(:id" nil path nil 'silent)))
         (let ((err (should-error (niwa-memory-store-accepted workspace id t))))
           (ert-info ((format "%S" case))
             (if (consp (nth 1 case))
                 (should (equal (nth 1 case) err))
               (should (eq (nth 1 case) (car err)))))))))))

(provide 'test-niwa-memory-store)
;;; test-niwa-memory-store.el ends here
