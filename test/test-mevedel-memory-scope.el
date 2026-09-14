;;; test-mevedel-memory-scope.el -- Captured review scope tests -*- lexical-binding: t -*-

;;; Commentary:

;; Exercise captured before-state using real memory and instruction files.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-system)
(require 'mevedel-memory-scope)

(mevedel-deftest mevedel-memory-scope-capture ()
  ,test
  (test)
  :doc "captures exact topic/index bytes and applicable instruction absence without writes"
  (let* ((directory (make-temp-file "mevedel-memory-scope-" t))
         (workspace (mevedel-workspace--create :root directory))
         (memory (file-name-concat directory "memory"))
         (mevedel-memory-dirs '("memory")))
    (unwind-protect
        (progn
          (make-directory memory)
          (with-temp-file (file-name-concat memory "MEMORY.md")
            (insert "- [Build](build.md): build facts\n"))
          (with-temp-file (file-name-concat memory "build.md")
            (insert "Run the suite after changing requests.\n"))
          (let* ((scope (mevedel-memory-scope-capture workspace))
                 (roots (plist-get scope :roots))
                 (root (cl-find 'memory roots :key (lambda (row) (plist-get (cdr row) :kind))))
                 (instructions (cl-find 'instructions roots :key (lambda (row) (plist-get (cdr row) :kind))))
                 (before (plist-get (cdr root) :before)))
            (should (equal (plist-get (cdr root) :dir) (file-name-as-directory memory)))
            (should (plist-get (cdr root) :complete))
            (should (equal (plist-get (cdr root) :files) '("build.md")))
            (should (equal (plist-get (cdr (assoc "build.md" before)) :bytes)
                           "Run the suite after changing requests.\n"))
            (should (equal (plist-get (cdr (assoc "MEMORY.md" before)) :bytes)
                           "- [Build](build.md): build facts\n"))
            (should (member "AGENTS.md" (plist-get (cdr instructions) :files)))
            (should-not (plist-get (cdr (assoc "AGENTS.md" (plist-get (cdr instructions) :before))) :exists))
            (should-not (file-exists-p (file-name-concat directory "AGENTS.md")))
            (should-not (file-exists-p (file-name-concat directory ".mevedel")))))
      (delete-directory directory t)))
  :doc "oversized topics remain known existing files and a truncated inventory grants no creation"
  (let* ((directory (make-temp-file "mevedel-memory-bounds-" t))
         (workspace (mevedel-workspace--create :root directory))
         (mevedel-memory-dirs '("memory"))
         (memory (file-name-concat directory "memory")))
    (unwind-protect
        (progn
          (make-directory memory)
          (with-temp-file (file-name-concat memory "large.md") (insert (make-string 40000 ?x)))
          (let* ((scope (mevedel-memory-scope-capture workspace))
                 (root (car (plist-get scope :roots))))
            (should (member "large.md" (plist-get (cdr root) :existing)))
            (should-not (member "large.md" (plist-get (cdr root) :files)))
            (should-error (mevedel-memory-scope-before scope (car root) "large.md"))
            (should (plist-get scope :omissions)))
          (dotimes (index 260)
            (with-temp-file (file-name-concat memory (format "%03d.txt" index))))
          (let* ((scope (mevedel-memory-scope-capture workspace))
                 (root (car (plist-get scope :roots))))
            (should-not (plist-get (cdr root) :complete))
            (should-error (mevedel-memory-scope-before scope (car root) "new.md"))))
      (delete-directory directory t)))
  :doc "the aggregate byte budget omits whole topics and malformed text is never admitted"
  (let* ((directory (make-temp-file "mevedel-memory-byte-budget-" t))
         (workspace (mevedel-workspace--create :root directory))
         (mevedel-memory-dirs '("memory"))
         (memory (file-name-concat directory "memory")))
    (unwind-protect
        (progn
          (make-directory memory)
          (let ((coding-system-for-write 'no-conversion))
            (write-region (unibyte-string 255 254) nil
                          (file-name-concat memory "bad.md") nil 'silent))
          (dotimes (index 5)
            (with-temp-file (file-name-concat memory (format "topic-%d.md" index))
              (insert (make-string 30000 ?x))))
          (let* ((scope (mevedel-memory-scope-capture workspace))
                 (root (car (plist-get scope :roots)))
                 (before (plist-get (cdr root) :before)))
            (should-not (member "bad.md" (plist-get (cdr root) :files)))
            (should (= 3 (length (plist-get (cdr root) :files))))
            (should (= 90000 (cl-loop for (_ . snapshot) in before sum (length (plist-get snapshot :bytes)))))
            (should (member "topic-4.md" (plist-get (cdr root) :existing)))
            (should-error (mevedel-memory-scope-before scope (car root) "topic-4.md"))))
      (delete-directory directory t)))
  :doc "captures remote memory with target-native identity and preserves literal UTF-8 bytes"
  (let ((directory (make-temp-file "mevedel-memory-remote-" t)))
    (unwind-protect
        (mevedel-test--with-local-shell-tramp '("memory-scope")
          (let* ((remote (format "/mevedelmock:memory-scope:%s/" directory))
                 (workspace (mevedel-workspace--create :root remote))
                 (mevedel-memory-dirs '("memory"))
                 (memory (file-name-concat directory "memory"))
                 (bytes (encode-coding-string "A \u754c fact.\r\n" 'utf-8-unix)))
            (make-directory memory)
            (let ((coding-system-for-write 'no-conversion))
              (write-region bytes nil (file-name-concat memory "topic.md") nil 'silent))
            (let* ((scope (mevedel-memory-scope-capture workspace))
                   (root (car (plist-get scope :roots))))
              (should-not (plist-get (cdr root) :client))
              (should (eq (plist-get (plist-get (cdr root) :target) :method) 'mevedelmock))
              (should (equal bytes (plist-get (mevedel-memory-scope-before scope (car root) "topic.md") :bytes)))
              (should (eq 'fresh (plist-get (mevedel-memory-scope-check scope (car root) '("topic.md")) :status))))))
      (delete-directory directory t))))

(mevedel-deftest mevedel-memory-scope-before ()
  ,test
  (test)
  :doc "new topic absence comes from the frozen complete inventory, never a later read"
  (let* ((directory (make-temp-file "mevedel-memory-before-" t))
         (workspace (mevedel-workspace--create :root directory))
         (mevedel-memory-dirs '("memory")))
    (unwind-protect
        (let* ((scope (mevedel-memory-scope-capture workspace))
               (root (caar (plist-get scope :roots)))
               (new (file-name-concat directory "memory" "new.md")))
          (make-directory (file-name-directory new))
          (with-temp-file new (insert "Arrived after the review began.\n"))
          (should-not (plist-get (mevedel-memory-scope-before scope root "new.md") :exists))
          (should (equal new (plist-get (mevedel-memory-scope-before scope root "new.md") :path)))
          (should-error (mevedel-memory-scope-before scope root "../escape.md"))
          (should-error (mevedel-memory-scope-before scope "unknown-root" "new.md")))
      (delete-directory directory t)))
  :doc "symlinked topics and parents cannot admit external before-state or new targets"
  (let* ((directory (make-temp-file "mevedel-memory-links-" t))
         (workspace (mevedel-workspace--create :root directory))
         (mevedel-memory-dirs '("memory"))
         (memory (file-name-concat directory "memory"))
         (outside (file-name-concat directory "outside")))
    (unwind-protect
        (progn
          (make-directory memory)
          (make-directory outside)
          (with-temp-file (file-name-concat outside "secret.md") (insert "Outside scope.\n"))
          (make-symbolic-link outside (file-name-concat memory "linked"))
          (make-symbolic-link (file-name-concat outside "secret.md") (file-name-concat memory "secret.md"))
          (let* ((scope (mevedel-memory-scope-capture workspace))
                 (root (car (plist-get scope :roots))))
            (should-not (member "secret.md" (plist-get (cdr root) :files)))
            (should-error (mevedel-memory-scope-before scope (car root) "secret.md"))
            (should-error (mevedel-memory-scope-before scope (car root) "linked/new.md"))
            (should-not (file-exists-p (file-name-concat outside "new.md")))))
      (delete-directory directory t))))

(mevedel-deftest mevedel-memory-scope-check ()
  ,test
  (test)
  :doc "delayed approval detects topic/index changes and expected-absence conflicts"
  (let* ((directory (make-temp-file "mevedel-memory-freshness-" t))
         (workspace (mevedel-workspace--create :root directory))
         (mevedel-memory-dirs '("memory"))
         (memory (file-name-concat directory "memory"))
         (topic (file-name-concat memory "topic.md"))
         (index (file-name-concat memory "MEMORY.md")))
    (unwind-protect
        (progn
          (make-directory memory)
          (with-temp-file topic (insert "Original topic.\n"))
          (with-temp-file index (insert "- [Topic](topic.md): original\n"))
          (let* ((scope (mevedel-memory-scope-capture workspace))
                 (root (caar (plist-get scope :roots))))
            (should (eq 'fresh (plist-get (mevedel-memory-scope-check scope root '("topic.md" "MEMORY.md")) :status)))
            (with-temp-file topic (insert "User corrected this topic.\n"))
            (should (eq 'stale (plist-get (mevedel-memory-scope-check scope root '("topic.md" "MEMORY.md")) :status)))
            (with-temp-file topic (insert "Original topic.\n"))
            (with-temp-file index (insert "Changed index.\n"))
            (should (eq 'stale (plist-get (mevedel-memory-scope-check scope root '("topic.md" "MEMORY.md")) :status)))
            (with-temp-file (file-name-concat memory "new.md") (insert "Concurrent creation.\n"))
            (should (eq 'stale (plist-get (mevedel-memory-scope-check scope root '("new.md")) :status)))
            (delete-file topic)
            (should (eq 'stale (plist-get (mevedel-memory-scope-check scope root '("topic.md")) :status)))))
      (delete-directory directory t)))
  :doc "configuration changes never rebind roots and another client cannot inspect local bodies"
  (let* ((directory (make-temp-file "mevedel-memory-origin-" t))
         (workspace (mevedel-workspace--create :root directory))
         (memory (file-name-concat directory "global"))
         (mevedel-memory-dirs (list memory)))
    (unwind-protect
        (progn
          (make-directory memory)
          (with-temp-file (file-name-concat memory "topic.md") (insert "Original root.\n"))
          (let* ((scope (mevedel-memory-scope-capture workspace))
                 (root (caar (plist-get scope :roots)))
                 (mevedel-memory-dirs '("different")))
            (should (eq 'fresh (plist-get (mevedel-memory-scope-check scope root '("topic.md")) :status)))
            (cl-letf (((symbol-function 'system-name) (lambda () "another-client")))
              (should (eq 'unavailable (plist-get (mevedel-memory-scope-check scope root '("topic.md")) :status)))
              (should-error (mevedel-memory-scope-before scope root "topic.md")
                            :type 'mevedel-memory-scope-unavailable))))
      (delete-directory directory t)))
  :doc "retargeting a configured root symlink makes its captured proposals unavailable"
  (let* ((directory (make-temp-file "mevedel-memory-retarget-" t))
         (workspace (mevedel-workspace--create :root directory))
         (alias (file-name-concat directory "memory"))
         (original (file-name-concat directory "original"))
         (replacement (file-name-concat directory "replacement"))
         (mevedel-memory-dirs '("memory")))
    (unwind-protect
        (progn
          (make-directory original)
          (make-directory replacement)
          (make-symbolic-link original alias)
          (let* ((scope (mevedel-memory-scope-capture workspace))
                 (root (caar (plist-get scope :roots))))
            (delete-file alias)
            (make-symbolic-link replacement alias)
            (should (eq 'unavailable (plist-get (mevedel-memory-scope-check scope root '("new.md")) :status)))))
      (delete-directory directory t))))

(mevedel-deftest mevedel-memory-scope-source-path ()
  ,test
  (test)
  :doc "workspace investigation excludes private state, omitted memory roots, and symlink escapes"
  (let* ((directory (make-temp-file "mevedel-memory-source-" t))
         (workspace (mevedel-workspace--create :root directory))
         (memory (file-name-concat directory "memory"))
         (mevedel-memory-dirs '("memory"))
         (outside (make-temp-file "mevedel-memory-outside-" t)))
    (unwind-protect
        (progn
          (make-directory memory)
          ;; The unreadable-in-scope index must not make this root ordinary
          ;; source through a fallback after capture omits its contents.
          (with-temp-file (file-name-concat memory "MEMORY.md") (insert (make-string 40000 ?x)))
          (make-symbolic-link outside (file-name-concat directory "escape"))
          (let ((scope (mevedel-memory-scope-capture workspace)))
            (should (equal (file-name-concat directory "src" "new.el")
                           (mevedel-memory-scope-source-path scope "src/new.el")))
            (dolist (path '("../escape.el" "/etc/passwd" "~/.emacs" "escape/secret.el"
                            ".mevedel/journal/state/job.json" ".git/config" "memory/secret.md"
                            "memory://journal/state/job.json"))
              (should-error (mevedel-memory-scope-source-path scope path)))
            (cl-letf (((symbol-function 'system-name) (lambda () "another-client")))
              (should-error (mevedel-memory-scope-source-path scope "src/new.el")
                            :type 'mevedel-memory-scope-unavailable))))
      (delete-directory directory t)
      (delete-directory outside t))))

(mevedel-deftest mevedel-workspace-identity-client ()
  ,test
  (test)
  :doc "stable across observations, distinct across host and user identities"
  (cl-letf (((symbol-function 'system-name) (lambda () "host-one"))
            ((symbol-function 'user-login-name) (lambda (&optional _) "user-one")))
    (let ((identity (mevedel-workspace-identity-client)))
      (should (equal identity (mevedel-workspace-identity-client)))
      (should (string-match-p "\\`[0-9a-f]\\{64\\}\\'" identity))
      (cl-letf (((symbol-function 'system-name) (lambda () "host-two")))
        (should-not (equal identity (mevedel-workspace-identity-client))))
      (cl-letf (((symbol-function 'user-login-name) (lambda (&optional _) "user-two")))
        (should-not (equal identity (mevedel-workspace-identity-client)))))))

(mevedel-deftest mevedel-memory-scope--inventory ()
  ,test
  (test)
  :doc "private write coordination does not exhaust memory inventory or enter proposal/source scopes"
  (let* ((directory (make-temp-file "mevedel-memory-private-control-" t))
         (workspace (mevedel-workspace--create :root directory))
         (mevedel-memory-dirs (list directory))
         (private (file-name-concat directory ".mevedel-memory-write")))
    (unwind-protect
        (progn
          (make-directory private)
          (dotimes (n 300) (write-region "claim" nil (file-name-concat private (format "%d.claim" n)) nil 'silent))
          (write-region "Topic\n" nil (file-name-concat directory "topic.md") nil 'silent)
          (let* ((scope (mevedel-memory-scope-capture workspace))
                 (root (cdr (seq-find (lambda (row) (eq (plist-get (cdr row) :kind) 'memory)) (plist-get scope :roots)))))
            (should (plist-get root :complete))
            (should (equal '("topic.md") (plist-get root :files)))
            (should-not (mevedel-memory-proposal--file-p ".mevedel-memory-write/topic.md"))
            (should-error (mevedel-memory-scope-source-path scope ".mevedel-memory-write/1.claim"))))
      (delete-directory directory t))))

(provide 'test-mevedel-memory-scope)
;;; test-mevedel-memory-scope.el ends here
