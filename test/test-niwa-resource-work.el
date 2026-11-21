;;; test-niwa-resource-work.el -- Working files and memory writes -*- lexical-binding: t -*-

;;; Commentary:

;; Exercises project ownership and native patch transactions with real files.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'niwa-resource)
(require 'niwa-resource-capf)
(require 'niwa-system)
(require 'niwa-permission-queue)
(require 'niwa-permission-queue-test-support
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "niwa-permission-queue-test-support"))
(require 'niwa-tool-patch)
(require 'niwa-tool-permission)
(require 'niwa-journal-evidence)

(niwa-deftest niwa-resource-session-work-p ()
  ,test
  (test)
  :doc "distinguishes session files from shared, memory, roots and ordinary paths"
  (progn
    (should (niwa-resource-session-work-p "work://plans/current.md"))
    (should (niwa-resource-session-work-p "work://sharedness.md"))
    (dolist (address '(nil "notes.md" "work://" "work://shared"
                          "work://shared/decision.md" "memory://local-niwa/topic.md"))
      (should-not (niwa-resource-session-work-p address)))))

(niwa-deftest niwa-resource-work-shared-directory ()
  ,test
  (test)
  :doc "is workspace-owned, lazy, and preserves a remote target prefix"
  (let* ((root (make-temp-file "niwa-work-layout-" t))
         (workspace (niwa-workspace--create :root root)))
    (unwind-protect
        (progn
          (should (equal (file-name-concat root ".niwa" "shared")
                         (niwa-resource-work-shared-directory workspace)))
          (should-not (file-exists-p (file-name-concat root ".niwa")))
          (should-not (niwa-resource-work-shared-directory nil))
          (setf (niwa-workspace-root workspace) "/ssh:example.invalid:/project")
          (should (equal "/ssh:example.invalid:/project/.niwa/shared"
                         (niwa-resource-work-shared-directory workspace))))
      (delete-directory root t))))

(niwa-deftest niwa-resource-execute/shared-work ()
  ,test
  (test)
  :doc "shared files cross session working directories and survive session deletion"
  (let* ((root (make-temp-file "niwa-work-ownership-" t))
         (workspace (niwa-workspace--create :root root))
         (save-path (file-name-concat root "session-a"))
         (first (niwa-session--create :workspace workspace :save-path save-path))
         (second (niwa-session--create
                  :workspace workspace
                  :working-directory (file-name-concat root ".worktrees" "feature")))
         (shared (niwa-resource-work-shared-directory workspace))
         (address "work://shared/build-decision.md"))
    (unwind-protect
        (progn
          (make-directory (file-name-concat save-path "local" "plans") t)
          (with-temp-file (file-name-concat save-path "local" "plans" "current.md")
            (insert "# Current plan\n"))
          (make-directory shared t)
          (with-temp-file (file-name-concat shared "build-decision.md")
            (insert "User correction: never deploy this branch.\n"))
          (dolist (session (list first second))
            (should
             (equal "User correction: never deploy this branch.\n"
                    (niwa-resource-execute
                     (niwa-resource-prepare 'read address (list :session session))
                     (lambda (path _)
                       (with-temp-buffer (insert-file-contents path) (buffer-string)))))))
          (let ((listing (plist-get
                          (niwa-resource-execute
                           (niwa-resource-prepare 'read "work://" (list :session first)))
                          :result))
                (snapshot (niwa-journal-evidence-notes second)))
            (should (string-search "work://plans/current.md" listing))
            (should (string-search address listing))
            ;; Consolidation reviews shared files directly, not through capture.
            (should-not (string-search address snapshot)))
          (let ((roots (plist-get
                        (niwa-resource-execute
                         (niwa-resource-prepare 'grep "work://" (list :session first)))
                        :resource-search-roots)))
            (should (= 2 (length roots)))
            (should (equal '("work://" "work://shared")
                           (mapcar (lambda (entry) (or (plist-get entry :address-prefix) (plist-get entry :address))) roots))))
          (with-temp-buffer
            (setq-local niwa--session second)
            (insert "work://shared/")
            (should (member address (nth 2 (niwa-resource-capf)))))
          (delete-directory save-path t)
          (should (file-exists-p (file-name-concat shared "build-decision.md")))
          (let* ((other (niwa-workspace--create :root (file-name-concat root "other")))
                 (context (list :workspace other)))
            (niwa-resource-execute
             (niwa-resource-prepare 'read address context)
             (lambda (path _) (should-not (file-exists-p path))))))
      (delete-directory root t)))
  :doc "shared listings encode names into addresses that Read can follow"
  (let* ((root (make-temp-file "niwa-work-listing-" t))
         (workspace (niwa-workspace--create :root root))
         (session (niwa-session--create :workspace workspace))
         (context (list :session session))
         (shared (niwa-resource-work-shared-directory workspace))
         (path (file-name-concat shared "notes one" "a#b%\u754c.md"))
         (address "work://shared/notes%20one/a%23b%25%E7%95%8C.md"))
    (unwind-protect
        (progn
          (make-directory (file-name-directory path) t)
          (with-temp-file path (insert "Shared lesson\n"))
          (let ((listing (plist-get
                          (niwa-resource-execute
                           (niwa-resource-prepare 'read "work://" context))
                          :result)))
            (should (member address (mapcar (lambda (line) (car (split-string line "\t")))
                                           (split-string listing "\n"))))
            (should
             (equal "Shared lesson\n"
                    (niwa-resource-execute
                     (niwa-resource-prepare 'read address context)
                     (lambda (file _)
                       (with-temp-buffer
                         (insert-file-contents file)
                         (buffer-string))))))))
      (delete-directory root t))))

(niwa-deftest niwa-resource-attempt-write-path ()
  ,test
  (test)
  :doc "captures real write authority and rejects rebound roots and root-only targets"
  (let* ((root (make-temp-file "niwa-work-authority-" t))
         (workspace (niwa-workspace--create :root root))
         (session (niwa-session--create :workspace workspace))
         (context (list :session session))
         (niwa-memory-dirs '(".niwa/memory"))
         (niwa-resource-attempts-cell (list nil)))
    (unwind-protect
        (progn
          (dolist (address '("work://shared" "work://" "memory://root"
                             "memory://local-niwa" "artifact://x" "memory://journal/"))
            (should-error (niwa-resource-prepare 'apply-patch address context)))
          (dolist (entry '(("work://shared/note.md" ".niwa/shared/note.md")
                           ("memory://local-niwa/topic.md" ".niwa/memory/topic.md")))
            (let ((attempt (niwa-resource-prepare 'apply-patch (car entry) context)))
              (should (equal (file-name-concat root (cadr entry))
                             (niwa-resource-attempt-write-path attempt)))
              (setf (niwa-workspace-root workspace) (file-name-concat root "rebound"))
              (should-error (niwa-resource-execute attempt #'ignore))
              (setf (niwa-workspace-root workspace) root)))
          (should-not (file-exists-p (file-name-concat root ".niwa"))))
      (niwa-resource-discard-attempts (car niwa-resource-attempts-cell))
      (delete-directory root t))))

(niwa-deftest niwa-tool-patch-prepare-resources/shared-memory ()
  ,test
  (test)
  :doc "creates, updates, moves and deletes memory and shared files with native patches"
  (let* ((root (make-temp-file "niwa-work-patches-" t))
         (workspace (niwa-workspace--create :root root))
         (session (niwa-session--create :workspace workspace))
         (niwa-memory-dirs '(".niwa/memory"))
         (niwa-resource-attempts-cell (list nil)))
    (unwind-protect
        (with-temp-buffer
          (setq-local niwa--session session default-directory (file-name-as-directory root))
          (dolist (address '("work://shared/decision.md" "memory://local-niwa/lesson.md"))
            (let* ((proposal
                    (niwa-tool-patch-prepare-resources
                     (niwa-tool-patch-parse
                      (format "*** Begin Patch\n*** Add File: %s\n+original\n*** End Patch" address))))
                   (operation (car (plist-get proposal :operations)))
                   (physical (plist-get operation :permission-path)))
              (should-not (plist-get proposal :session-only-p))
              (should (equal (list physical)
                             (niwa-tool-permission-paths nil nil (list :patch-proposal proposal))))
              (should-not (file-exists-p physical))
              (let* ((niwa-protected-paths (list (cons physical 'inaccessible)))
                     (facts (niwa-permission--preflight
                             "ApplyPatch" :tool-struct (niwa-tool-ensure "ApplyPatch")
                             :path physical :workspace-root root :mode 'edits)))
                (should (plist-get facts :protected-path-p))
                (should (eq 'ask (plist-get (niwa-permission--resource-decision facts)
                                           :outcome))))
              (should (equal (concat "Denied: " address)
                             (niwa-tool-patch-sanitize-error
                              (concat "Denied: " physical) proposal)))
              (niwa-tool-patch--materialize-resources proposal)
              (niwa-tool-patch-commit (niwa-tool-patch-planned-changes proposal))
              (should (file-exists-p physical))
              (let* ((updated
                      (niwa-tool-patch-prepare-resources
                       (niwa-tool-patch-parse
                        (format "*** Begin Patch\n*** Update File: %s\n*** Move to: %s.moved\n@@\n-original\n+corrected\n*** End Patch"
                                address address)) t)))
                (niwa-tool-patch-commit (niwa-tool-patch-planned-changes updated))
                (should-not (file-exists-p physical))
                (should (equal "corrected\n" (with-temp-buffer
                                              (insert-file-contents (concat physical ".moved"))
                                              (buffer-string)))))
              (let ((deleted
                     (niwa-tool-patch-prepare-resources
                      (niwa-tool-patch-parse
                       (format "*** Begin Patch\n*** Delete File: %s.moved\n*** End Patch" address)) t)))
                (niwa-tool-patch-commit (niwa-tool-patch-planned-changes deleted))
                (should-not (file-exists-p (concat physical ".moved"))))))
          (should-not (niwa-session-save-path session)))
      (niwa-resource-discard-attempts (car niwa-resource-attempts-cell))
      (delete-directory root t))))

(niwa-deftest niwa-tool-permission-step/shared-memory ()
  ,test
  (test)
  :doc "reuses selected backing-directory grants without bypassing deny or Plan"
  (let* ((root (make-temp-file "niwa-work-grants-" t))
         (workspace (niwa-workspace--create :root root))
         (niwa-memory-dirs '(".niwa/memory"))
         (niwa-permission-rules nil)
         (niwa-permission-reviewer 'user)
         (niwa-resource-attempts-cell (list nil)))
    (unwind-protect
        (with-temp-buffer
          (setq-local default-directory (file-name-as-directory root))
          (dolist (address '("work://shared/note.md"
                             "memory://local-niwa/note.md"))
            (let* ((session (niwa-session--create
                             :workspace workspace :permission-mode 'edits))
                   (niwa--session session)
                   (proposal
                    (niwa-tool-patch-prepare-resources
                     (niwa-tool-patch-parse
                      (format "*** Begin Patch\n*** Add File: %s\n+note\n*** End Patch"
                              address))))
                   (path (car (niwa-tool-permission-paths
                               nil nil (list :patch-proposal proposal))))
                   (directory (directory-file-name (file-name-directory path)))
                   (niwa-protected-paths
                    (list (cons (file-name-concat directory "**") 'inaccessible)))
                   (request (niwa-request--create
                             :session session :origin "/root/worker"))
                   (niwa--current-request request)
                   (context (list :tool (niwa-tool-ensure "ApplyPatch")
                                  :patch-proposal proposal :session session
                                  :workspace workspace :request request))
                   queued allowed denied)
              (make-directory directory t)
              (niwa-pq-test--with-session-view session view
                (cl-flet ((admit ()
                            (setq allowed nil denied nil)
                            (niwa-pq-test--forget-requests session)
                            (niwa-tool-permission-step
                             context (lambda (updated) (setq allowed updated))
                             (lambda (reason &rest _) (setq denied reason)))
                            (setq queued (car (niwa-pq-test--cards view)))))
                  (admit)
                  (should queued)
                  (should-not allowed)
                  (should (equal "/root/worker"
                                 (plist-get (plist-get queued :entry) :origin)))
                  (should (equal path
                                 (plist-get (plist-get queued :entry)
                                            :specifier-value)))
                  ;; An exact approval must not authorize a sibling file.
                  (niwa-pq-test--press queued "s")
                  (should allowed)
                  (should (equal (list (list :path path :access 'write))
                                 (niwa-session-resource-grants session)))
                  (admit)
                  (should allowed)
                  (should-not queued)
                  (setq proposal
                        (niwa-tool-patch-prepare-resources
                         (niwa-tool-patch-parse
                          (format "*** Begin Patch\n*** Add File: %s.other\n+note\n*** End Patch"
                                  address)))
                        context (plist-put context :patch-proposal proposal))
                  (admit)
                  (should queued)
                  (should-not allowed)
                  ;; The person selects the directory tree on the card ("g");
                  ;; the choice is the only input replaced.  Approval storage
                  ;; and the handler's trusted context share that selection.
                  (cl-letf (((symbol-function 'completing-read)
                             (lambda (_prompt choices &rest _)
                               (car (cl-find-if
                                     (lambda (choice)
                                       (let ((grant (cdr choice)))
                                         (and (plist-get grant :recursive)
                                              (equal directory
                                                     (plist-get grant :path)))))
                                     choices)))))
                    (niwa-pq-test--press queued "g"))
                  (setq queued (car (niwa-pq-test--cards view)))
                  (niwa-pq-test--press queued "s")
                  (should allowed)
                  ;; The card builds the grant itself, so compare its fields,
                  ;; not the order they were written in.
                  (should (equal (list (list :path directory :access 'write :recursive t))
                                 (mapcar (lambda (grant)
                                           (list :path (plist-get grant :path)
                                                 :access (plist-get grant :access)
                                                 :recursive (plist-get grant :recursive)))
                                         (plist-get allowed :approved-resources))))
                  (admit)
                  (should allowed)
                  (should-not queued)
                  (setf (niwa-session-permission-rules session)
                        '(("ApplyPatch" :action deny)))
                  (admit)
                  (should denied)
                  (should-not allowed)
                  (setf (niwa-session-permission-rules session) nil
                        (niwa-request-plan-read-only request) t)
                  (admit)
                  (should denied)
                  (should-not allowed)
                  (should-not queued))))))
      (niwa-resource-discard-attempts (car niwa-resource-attempts-cell))
      (delete-directory root t))))

(niwa-deftest niwa-resource-attempt-write-check ()
  ,test
  (test)
  :doc "invalidates a reviewed patch after root rebinding, including move destinations"
  (let* ((root (make-temp-file "niwa-work-review-" t))
         (workspace (niwa-workspace--create :root root))
         (session (niwa-session--create :workspace workspace))
         (niwa-memory-dirs '(".niwa/memory")))
    (unwind-protect
        (with-temp-buffer
          (setq-local niwa--session session default-directory (file-name-as-directory root))
          (dolist (address '("work://shared/decision.md" "memory://local-niwa/lesson.md"))
            (let* ((proposal (niwa-tool-patch-prepare-resources
                              (niwa-tool-patch-parse
                               (format "*** Begin Patch\n*** Add File: %s\n+reviewed\n*** End Patch" address)) t))
                   (changes (niwa-tool-patch-planned-changes proposal)))
              (setf (niwa-workspace-root workspace) (file-name-concat root "retargeted"))
              (should-error (niwa-tool-patch-planned-changes proposal))
              (should-error (niwa-tool-patch-commit changes))
              (should-not (file-exists-p (plist-get (car changes) :path)))
              (setf (niwa-workspace-root workspace) root)))
          (with-temp-file (file-name-concat root "source.txt") (insert "source\n"))
          (let* ((proposal (niwa-tool-patch-prepare-resources
                            (niwa-tool-patch-parse
                             "*** Begin Patch\n*** Update File: source.txt\n*** Move to: work://shared/moved.txt\n*** End Patch") t))
                 (changes (niwa-tool-patch-planned-changes proposal)))
            (setf (niwa-workspace-root workspace) (file-name-concat root "retargeted"))
            (should-error (niwa-tool-patch-commit changes))
            (should (file-exists-p (file-name-concat root "source.txt")))))
      (delete-directory root t)))

  :doc "rejects a swapped ancestor symlink even when the configured root text is unchanged"
  (let* ((root (make-temp-file "niwa-memory-review-link-" t))
         (first (file-name-concat root "first"))
         (second (file-name-concat root "second"))
         (link (file-name-concat root "linked"))
         (workspace (niwa-workspace--create :root root))
         (niwa-memory-dirs (list (file-name-concat link "memory")))
         (niwa-resource-attempts-cell (list nil)))
    (unwind-protect
        (progn
          (make-directory first)
          (make-directory second)
          (make-symbolic-link first link)
          (let* ((memory-root (car (niwa-system--memory-roots workspace)))
                 (address (concat "memory://" (niwa-resource-memory-root-key memory-root) "/note.md"))
                 (attempt (niwa-resource-prepare 'apply-patch address (list :workspace workspace)))
                 (check (niwa-resource-attempt-write-check attempt)))
            (should (funcall check))
            (delete-file link)
            (make-symbolic-link second link)
            (should-error (funcall check))))
      (niwa-resource-discard-attempts (car niwa-resource-attempts-cell))
      (delete-directory root t))))

(niwa-deftest niwa-resource-capf/work ()
  ,test
  (test)
  :doc "reserves only the shared component and completes similarly named session files"
  (let* ((root (make-temp-file "niwa-work-capf-" t))
         (workspace (niwa-workspace--create :root root))
         (session (niwa-session--create :workspace workspace :save-path root)))
    (unwind-protect
        (progn
          (make-directory (file-name-concat root "local" "shared-notes") t)
          (with-temp-file (file-name-concat root "local" "sharedness.md") (insert "local"))
          (with-temp-buffer
            (setq-local niwa--session session)
            (insert "work://shared")
            (let ((candidates (nth 2 (niwa-resource-capf))))
              (should (member "work://shared" candidates))
              (should (member "work://sharedness.md" candidates))
              (should (member "work://shared-notes" candidates)))))
      (delete-directory root t))))

(provide 'test-niwa-resource-work)
;;; test-niwa-resource-work.el ends here
