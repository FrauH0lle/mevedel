;;; test-mevedel-resource-work.el -- Working files and memory writes -*- lexical-binding: t -*-

;;; Commentary:

;; Exercises project ownership and native patch transactions with real files.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-resource)
(require 'mevedel-resource-capf)
(require 'mevedel-system)
(require 'mevedel-tool-patch)
(require 'mevedel-tool-permission)
(require 'mevedel-journal-evidence)

(mevedel-deftest mevedel-resource-session-work-p ()
  ,test
  (test)
  :doc "distinguishes session files from shared, memory, roots and ordinary paths"
  (progn
    (should (mevedel-resource-session-work-p "work://plans/current.md"))
    (should (mevedel-resource-session-work-p "work://sharedness.md"))
    (dolist (address '(nil "notes.md" "work://" "work://shared"
                          "work://shared/decision.md" "memory://local-mevedel/topic.md"))
      (should-not (mevedel-resource-session-work-p address)))))

(mevedel-deftest mevedel-resource-work-shared-directory ()
  ,test
  (test)
  :doc "is workspace-owned, lazy, and preserves a remote target prefix"
  (let* ((root (make-temp-file "mevedel-work-layout-" t))
         (workspace (mevedel-workspace--create :root root)))
    (unwind-protect
        (progn
          (should (equal (file-name-concat root ".mevedel" "shared")
                         (mevedel-resource-work-shared-directory workspace)))
          (should-not (file-exists-p (file-name-concat root ".mevedel")))
          (should-not (mevedel-resource-work-shared-directory nil))
          (setf (mevedel-workspace-root workspace) "/ssh:example.invalid:/project")
          (should (equal "/ssh:example.invalid:/project/.mevedel/shared"
                         (mevedel-resource-work-shared-directory workspace))))
      (delete-directory root t))))

(mevedel-deftest mevedel-resource-execute/shared-work ()
  ,test
  (test)
  :doc "shared files survive session deletion and stay separate across workspaces"
  (let* ((root (make-temp-file "mevedel-work-ownership-" t))
         (workspace (mevedel-workspace--create :root root))
         (save-path (file-name-concat root "session-a"))
         (first (mevedel-session--create :workspace workspace :save-path save-path))
         (second (mevedel-session--create :workspace workspace))
         (shared (mevedel-resource-work-shared-directory workspace))
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
                    (mevedel-resource-execute
                     (mevedel-resource-prepare 'read address (list :session session))
                     (lambda (path _)
                       (with-temp-buffer (insert-file-contents path) (buffer-string)))))))
          (let ((listing (plist-get
                          (mevedel-resource-execute
                           (mevedel-resource-prepare 'read "work://" (list :session first)))
                          :result))
                (snapshot (mevedel-journal-evidence-notes second)))
            (should (string-search "work://plans/current.md" listing))
            (should (string-search address listing))
            (should (string-search address snapshot))
            (should (string-search "other sessions" snapshot)))
          (let ((roots (plist-get
                        (mevedel-resource-execute
                         (mevedel-resource-prepare 'grep "work://" (list :session first)))
                        :resource-search-roots)))
            (should (= 2 (length roots)))
            (should (equal '("work://" "work://shared")
                           (mapcar (lambda (entry) (or (plist-get entry :address-prefix) (plist-get entry :address))) roots))))
          (with-temp-buffer
            (setq-local mevedel--session second)
            (insert "work://shared/")
            (should (member address (nth 2 (mevedel-resource-capf)))))
          (delete-directory save-path t)
          (should (file-exists-p (file-name-concat shared "build-decision.md")))
          (let* ((other (mevedel-workspace--create :root (file-name-concat root "other")))
                 (context (list :workspace other)))
            (mevedel-resource-execute
             (mevedel-resource-prepare 'read address context)
             (lambda (path _) (should-not (file-exists-p path))))))
      (delete-directory root t)))
  :doc "shared listings encode names into addresses that Read can follow"
  (let* ((root (make-temp-file "mevedel-work-listing-" t))
         (workspace (mevedel-workspace--create :root root))
         (session (mevedel-session--create :workspace workspace))
         (context (list :session session))
         (shared (mevedel-resource-work-shared-directory workspace))
         (path (file-name-concat shared "notes one" "a#b%\u754c.md"))
         (address "work://shared/notes%20one/a%23b%25%E7%95%8C.md"))
    (unwind-protect
        (progn
          (make-directory (file-name-directory path) t)
          (with-temp-file path (insert "Shared lesson\n"))
          (let ((listing (plist-get
                          (mevedel-resource-execute
                           (mevedel-resource-prepare 'read "work://" context))
                          :result)))
            (should (member address (split-string listing "\n")))
            (should
             (equal "Shared lesson\n"
                    (mevedel-resource-execute
                     (mevedel-resource-prepare 'read address context)
                     (lambda (file _)
                       (with-temp-buffer
                         (insert-file-contents file)
                         (buffer-string))))))))
      (delete-directory root t))))

(mevedel-deftest mevedel-resource-attempt-write-path ()
  ,test
  (test)
  :doc "captures real write authority and rejects rebound roots and root-only targets"
  (let* ((root (make-temp-file "mevedel-work-authority-" t))
         (workspace (mevedel-workspace--create :root root))
         (session (mevedel-session--create :workspace workspace))
         (context (list :session session))
         (mevedel-memory-dirs '(".mevedel/memory"))
         (mevedel-resource-attempts-cell (list nil)))
    (unwind-protect
        (progn
          (dolist (address '("work://shared" "work://" "memory://root"
                             "memory://local-mevedel" "artifact://x" "memory://journal/"))
            (should-error (mevedel-resource-prepare 'apply-patch address context)))
          (dolist (entry '(("work://shared/note.md" ".mevedel/shared/note.md")
                           ("memory://local-mevedel/topic.md" ".mevedel/memory/topic.md")))
            (let ((attempt (mevedel-resource-prepare 'apply-patch (car entry) context)))
              (should (equal (file-name-concat root (cadr entry))
                             (mevedel-resource-attempt-write-path attempt)))
              (setf (mevedel-workspace-root workspace) (file-name-concat root "rebound"))
              (should-error (mevedel-resource-execute attempt #'ignore))
              (setf (mevedel-workspace-root workspace) root)))
          (should-not (file-exists-p (file-name-concat root ".mevedel"))))
      (mevedel-resource-discard-attempts (car mevedel-resource-attempts-cell))
      (delete-directory root t))))

(mevedel-deftest mevedel-tool-patch-prepare-resources/shared-memory ()
  ,test
  (test)
  :doc "creates, updates, moves and deletes memory and shared files with native patches"
  (let* ((root (make-temp-file "mevedel-work-patches-" t))
         (workspace (mevedel-workspace--create :root root))
         (session (mevedel-session--create :workspace workspace))
         (mevedel-memory-dirs '(".mevedel/memory"))
         (mevedel-resource-attempts-cell (list nil)))
    (unwind-protect
        (with-temp-buffer
          (setq-local mevedel--session session default-directory (file-name-as-directory root))
          (dolist (address '("work://shared/decision.md" "memory://local-mevedel/lesson.md"))
            (let* ((proposal
                    (mevedel-tool-patch-prepare-resources
                     (mevedel-tool-patch-parse
                      (format "*** Begin Patch\n*** Add File: %s\n+original\n*** End Patch" address))))
                   (operation (car (plist-get proposal :operations)))
                   (physical (plist-get operation :permission-path)))
              (should-not (plist-get proposal :session-only-p))
              (should (equal (list physical)
                             (mevedel-tool-permission-paths nil nil (list :patch-proposal proposal))))
              (should-not (file-exists-p physical))
              (let* ((mevedel-protected-paths (list (cons physical 'inaccessible)))
                     (facts (mevedel-permission--preflight
                             "ApplyPatch" :tool-struct (mevedel-tool-ensure "ApplyPatch")
                             :path physical :workspace-root root :mode 'full-auto)))
                (should (plist-get facts :protected-path-p))
                (should (eq 'ask (plist-get (mevedel-permission--resource-decision facts)
                                           :outcome))))
              (should (equal (concat "Denied: " address)
                             (mevedel-tool-patch-sanitize-error
                              (concat "Denied: " physical) proposal)))
              (mevedel-tool-patch--materialize-resources proposal)
              (mevedel-tool-patch-commit (mevedel-tool-patch-planned-changes proposal))
              (should (file-exists-p physical))
              (let* ((updated
                      (mevedel-tool-patch-prepare-resources
                       (mevedel-tool-patch-parse
                        (format "*** Begin Patch\n*** Update File: %s\n*** Move to: %s.moved\n@@\n-original\n+corrected\n*** End Patch"
                                address address)) t)))
                (mevedel-tool-patch-commit (mevedel-tool-patch-planned-changes updated))
                (should-not (file-exists-p physical))
                (should (equal "corrected\n" (with-temp-buffer
                                              (insert-file-contents (concat physical ".moved"))
                                              (buffer-string)))))
              (let ((deleted
                     (mevedel-tool-patch-prepare-resources
                      (mevedel-tool-patch-parse
                       (format "*** Begin Patch\n*** Delete File: %s.moved\n*** End Patch" address)) t)))
                (mevedel-tool-patch-commit (mevedel-tool-patch-planned-changes deleted))
                (should-not (file-exists-p (concat physical ".moved"))))))
          (should-not (mevedel-session-save-path session)))
      (mevedel-resource-discard-attempts (car mevedel-resource-attempts-cell))
      (delete-directory root t))))

(mevedel-deftest mevedel-tool-permission-step/shared-memory ()
  ,test
  (test)
  :doc "reuses selected backing-directory grants without bypassing deny or Plan"
  (let* ((root (make-temp-file "mevedel-work-grants-" t))
         (workspace (mevedel-workspace--create :root root))
         (mevedel-memory-dirs '(".mevedel/memory"))
         (mevedel-permission-rules nil)
         (mevedel-permission-guardian nil)
         (mevedel-resource-attempts-cell (list nil)))
    (unwind-protect
        (with-temp-buffer
          (setq-local default-directory (file-name-as-directory root))
          (dolist (address '("work://shared/note.md"
                             "memory://local-mevedel/note.md"))
            (let* ((session (mevedel-session--create
                             :workspace workspace :permission-mode 'edits))
                   (mevedel--session session)
                   (proposal
                    (mevedel-tool-patch-prepare-resources
                     (mevedel-tool-patch-parse
                      (format "*** Begin Patch\n*** Add File: %s\n+note\n*** End Patch"
                              address))))
                   (path (car (mevedel-tool-permission-paths
                               nil nil (list :patch-proposal proposal))))
                   (directory (directory-file-name (file-name-directory path)))
                   (mevedel-protected-paths
                    (list (cons (file-name-concat directory "**") 'inaccessible)))
                   (request (mevedel-request--create
                             :session session :origin "/root/worker"))
                   (mevedel--current-request request)
                   (context (list :tool (mevedel-tool-ensure "ApplyPatch")
                                  :patch-proposal proposal :session session
                                  :workspace workspace :request request))
                   queued allowed denied)
              (make-directory directory t)
              (cl-letf (((symbol-function 'mevedel-permission--enqueue)
                         (lambda (entry &optional _session) (setq queued entry))))
                (cl-flet ((admit ()
                            (setq queued nil allowed nil denied nil)
                            (mevedel-tool-permission-step
                             context (lambda (updated) (setq allowed updated))
                             (lambda (reason &rest _) (setq denied reason)))))
                  (admit)
                  (should queued)
                  (should-not allowed)
                  (should (equal "/root/worker" (plist-get queued :origin)))
                  (should (equal path (plist-get queued :specifier-value)))
                  ;; An exact approval must not authorize a sibling file.
                  (funcall (plist-get queued :callback) 'allow-session)
                  (should allowed)
                  (should (equal (list (list :path path :access 'write))
                                 (mevedel-session-resource-grants session)))
                  (admit)
                  (should allowed)
                  (should-not queued)
                  (setq proposal
                        (mevedel-tool-patch-prepare-resources
                         (mevedel-tool-patch-parse
                          (format "*** Begin Patch\n*** Add File: %s.other\n+note\n*** End Patch"
                                  address)))
                        context (plist-put context :patch-proposal proposal))
                  (admit)
                  (should queued)
                  (should-not allowed)
                  ;; The permission card's selected directory is shared by
                  ;; approval storage and the handler's trusted context.
                  (setcar (plist-get queued :resource-selection-cell)
                          (list (list :path directory :access 'write :recursive t)))
                  (funcall (plist-get queued :callback) 'allow-session)
                  (should allowed)
                  (should (equal (list (list :path directory :access 'write :recursive t))
                                 (plist-get allowed :approved-resources)))
                  (admit)
                  (should allowed)
                  (should-not queued)
                  (setf (mevedel-session-permission-rules session)
                        '(("ApplyPatch" :action deny)))
                  (admit)
                  (should denied)
                  (should-not allowed)
                  (setf (mevedel-session-permission-rules session) nil
                        (mevedel-request-plan-read-only request) t)
                  (admit)
                  (should denied)
                  (should-not allowed)
                  (should-not queued))))))
      (mevedel-resource-discard-attempts (car mevedel-resource-attempts-cell))
      (delete-directory root t))))

(mevedel-deftest mevedel-resource-attempt-write-check ()
  ,test
  (test)
  :doc "invalidates a reviewed patch after root rebinding, including move destinations"
  (let* ((root (make-temp-file "mevedel-work-review-" t))
         (workspace (mevedel-workspace--create :root root))
         (session (mevedel-session--create :workspace workspace))
         (mevedel-memory-dirs '(".mevedel/memory")))
    (unwind-protect
        (with-temp-buffer
          (setq-local mevedel--session session default-directory (file-name-as-directory root))
          (dolist (address '("work://shared/decision.md" "memory://local-mevedel/lesson.md"))
            (let* ((proposal (mevedel-tool-patch-prepare-resources
                              (mevedel-tool-patch-parse
                               (format "*** Begin Patch\n*** Add File: %s\n+reviewed\n*** End Patch" address)) t))
                   (changes (mevedel-tool-patch-planned-changes proposal)))
              (setf (mevedel-workspace-root workspace) (file-name-concat root "retargeted"))
              (should-error (mevedel-tool-patch-planned-changes proposal))
              (should-error (mevedel-tool-patch-commit changes))
              (should-not (file-exists-p (plist-get (car changes) :path)))
              (setf (mevedel-workspace-root workspace) root)))
          (with-temp-file (file-name-concat root "source.txt") (insert "source\n"))
          (let* ((proposal (mevedel-tool-patch-prepare-resources
                            (mevedel-tool-patch-parse
                             "*** Begin Patch\n*** Update File: source.txt\n*** Move to: work://shared/moved.txt\n*** End Patch") t))
                 (changes (mevedel-tool-patch-planned-changes proposal)))
            (setf (mevedel-workspace-root workspace) (file-name-concat root "retargeted"))
            (should-error (mevedel-tool-patch-commit changes))
            (should (file-exists-p (file-name-concat root "source.txt")))))
      (delete-directory root t)))

  :doc "rejects a swapped ancestor symlink even when the configured root text is unchanged"
  (let* ((root (make-temp-file "mevedel-memory-review-link-" t))
         (first (file-name-concat root "first"))
         (second (file-name-concat root "second"))
         (link (file-name-concat root "linked"))
         (workspace (mevedel-workspace--create :root root))
         (mevedel-memory-dirs (list (file-name-concat link "memory")))
         (mevedel-resource-attempts-cell (list nil)))
    (unwind-protect
        (progn
          (make-directory first)
          (make-directory second)
          (make-symbolic-link first link)
          (let* ((memory-root (car (mevedel-system--memory-roots workspace)))
                 (address (concat "memory://" (mevedel-resource-memory-root-key memory-root) "/note.md"))
                 (attempt (mevedel-resource-prepare 'apply-patch address (list :workspace workspace)))
                 (check (mevedel-resource-attempt-write-check attempt)))
            (should (funcall check))
            (delete-file link)
            (make-symbolic-link second link)
            (should-error (funcall check))))
      (mevedel-resource-discard-attempts (car mevedel-resource-attempts-cell))
      (delete-directory root t))))

(mevedel-deftest mevedel-resource-capf/work ()
  ,test
  (test)
  :doc "reserves only the shared component and completes similarly named session files"
  (let* ((root (make-temp-file "mevedel-work-capf-" t))
         (workspace (mevedel-workspace--create :root root))
         (session (mevedel-session--create :workspace workspace :save-path root)))
    (unwind-protect
        (progn
          (make-directory (file-name-concat root "local" "shared-notes") t)
          (with-temp-file (file-name-concat root "local" "sharedness.md") (insert "local"))
          (with-temp-buffer
            (setq-local mevedel--session session)
            (insert "work://shared")
            (let ((candidates (nth 2 (mevedel-resource-capf))))
              (should (member "work://shared" candidates))
              (should (member "work://sharedness.md" candidates))
              (should (member "work://shared-notes" candidates)))))
      (delete-directory root t))))

(provide 'test-mevedel-resource-work)
;;; test-mevedel-resource-work.el ends here
