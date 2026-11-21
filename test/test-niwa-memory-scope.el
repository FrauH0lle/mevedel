;;; test-niwa-memory-scope.el -- Captured review scope tests -*- lexical-binding: t -*-

;;; Commentary:

;; Exercise captured before-state using real memory and instruction files.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'niwa-system)
(require 'niwa-memory-scope)
(require 'niwa-session-control-fs)

(niwa-deftest niwa-memory-scope-capture ()
  ,test
  (test)
  :doc "batches bounded reads without dropping siblings after an unreadable topic"
  (let* ((directory (make-temp-file "niwa-memory-batch-" t))
         (workspace (niwa-workspace--create :root directory))
         (memory (file-name-concat directory "memory"))
         (niwa-memory-dirs '("memory"))
         (run (symbol-function 'niwa-session-control-fs-run-program))
         (reads 0))
    (unwind-protect
        (progn
          (make-directory memory)
          (make-symbolic-link "/missing-memory-topic" (file-name-concat memory "bad.md"))
          (dotimes (index 20)
            (with-temp-file (file-name-concat memory (format "topic-%02d.md" index))
              (insert "Fact\n")))
          (cl-letf (((symbol-function 'niwa-session-control-fs-run-program)
                     (lambda (operations &rest args)
                       (when (cl-some (lambda (op) (eq (plist-get op :op) 'read)) operations)
                         (cl-incf reads))
                       (apply run operations args))))
            (let* ((scope (niwa-memory-scope-capture workspace))
                   (root (car (plist-get scope :roots))))
              (should (= 20 (length (plist-get (cdr root) :files))))
              (should (plist-get scope :omissions))
              (should (< reads 10)))))
      (delete-directory directory t)))

  :doc "captures exact topic/index bytes and applicable instruction absence without writes"
  (let* ((directory (make-temp-file "niwa-memory-scope-" t))
         (workspace (niwa-workspace--create :root directory))
         (memory (file-name-concat directory "memory"))
         (niwa-memory-dirs '("memory")))
    (unwind-protect
        (progn
          (make-directory memory)
          (with-temp-file (file-name-concat memory "MEMORY.md")
            (insert "- [Build](build.md): build facts\n"))
          (with-temp-file (file-name-concat memory "build.md")
            (insert "Run the suite after changing requests.\n"))
          (let* ((scope (niwa-memory-scope-capture workspace))
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
            (should-not (file-exists-p (file-name-concat directory ".niwa")))))
      (delete-directory directory t)))
  :doc "rotates which topics fit the byte budget from day to day"
  (let* ((directory (make-temp-file "niwa-memory-rotate-" t))
         (workspace (niwa-workspace--create :root directory))
         (memory (file-name-concat directory "memory"))
         (niwa-memory-dirs '("memory"))
         (day 0))
    (unwind-protect
        (progn
          (make-directory memory)
          (with-temp-file (file-name-concat memory "MEMORY.md") (insert "- index\n"))
          ;; Twelve 30 KiB topics exceed the 256 KiB budget, so some are omitted.
          (dotimes (index 12)
            (with-temp-file (file-name-concat memory (format "topic-%02d.md" index))
              (insert (make-string (* 30 1024) ?x))))
          (cl-letf* ((rotate (symbol-function 'niwa-memory-scope--rotate))
                     ((symbol-function 'niwa-memory-scope--rotate)
                      (lambda (names &optional _day) (funcall rotate names day))))
            (let ((first (plist-get (cdr (car (plist-get (niwa-memory-scope-capture workspace) :roots)))
                                    :files))
                  second)
              (setq day 5)
              (setq second (plist-get (cdr (car (plist-get (niwa-memory-scope-capture workspace) :roots)))
                                      :files))
              (should (< (length first) 12))
              (should (equal (car first) "topic-00.md"))
              (should (equal (car second) "topic-05.md"))
              ;; Together the two days reach every topic.
              (should (= 12 (length (delete-dups (append first second))))))))
      (delete-directory directory t)))
  :doc "oversized topics remain known existing files and a truncated inventory grants no creation"
  (let* ((directory (make-temp-file "niwa-memory-bounds-" t))
         (workspace (niwa-workspace--create :root directory))
         (niwa-memory-dirs '("memory"))
         (memory (file-name-concat directory "memory")))
    (unwind-protect
        (progn
          (make-directory memory)
          (with-temp-file (file-name-concat memory "large.md") (insert (make-string 40000 ?x)))
          (let* ((scope (niwa-memory-scope-capture workspace))
                 (root (car (plist-get scope :roots))))
            (should (member "large.md" (plist-get (cdr root) :existing)))
            (should-not (member "large.md" (plist-get (cdr root) :files)))
            (should-error (niwa-memory-scope-before scope (car root) "large.md"))
            (should (plist-get scope :omissions)))
          (dotimes (index 260)
            (with-temp-file (file-name-concat memory (format "%03d.txt" index))))
          (let* ((scope (niwa-memory-scope-capture workspace))
                 (root (car (plist-get scope :roots))))
            (should-not (plist-get (cdr root) :complete))
            (should-error (niwa-memory-scope-before scope (car root) "new.md"))))
      (delete-directory directory t)))
  :doc "the aggregate byte budget omits whole topics and malformed text is never admitted"
  (let* ((directory (make-temp-file "niwa-memory-byte-budget-" t))
         (workspace (niwa-workspace--create :root directory))
         (niwa-memory-dirs '("memory"))
         (memory (file-name-concat directory "memory")))
    (unwind-protect
        (progn
          (make-directory memory)
          (let ((coding-system-for-write 'no-conversion))
            (write-region (unibyte-string 255 254) nil
                          (file-name-concat memory "bad.md") nil 'silent))
          (dotimes (index 10)
            (with-temp-file (file-name-concat memory (format "topic-%d.md" index))
              (insert (make-string 30000 ?x))))
          (let* ((rotate (symbol-function 'niwa-memory-scope--rotate))
                 (scope (cl-letf (((symbol-function 'niwa-memory-scope--rotate)
                                   (lambda (names &optional _day) (funcall rotate names 0))))
                          (niwa-memory-scope-capture workspace)))
                 (root (car (plist-get scope :roots)))
                 (before (plist-get (cdr root) :before)))
            (should-not (member "bad.md" (plist-get (cdr root) :files)))
            (should (= 8 (length (plist-get (cdr root) :files))))
            (should (= 240000 (cl-loop for (_ . snapshot) in before sum (length (plist-get snapshot :bytes)))))
            (should (member "topic-9.md" (plist-get (cdr root) :existing)))
            (should-error (niwa-memory-scope-before scope (car root) "topic-9.md"))))
      (delete-directory directory t)))
  :doc "captures remote memory with target-native identity and preserves literal UTF-8 bytes"
  (let ((directory (make-temp-file "niwa-memory-remote-" t)))
    (unwind-protect
        (niwa-test--with-local-shell-tramp '("memory-scope")
          (let* ((remote (format "/niwamock:memory-scope:%s/" directory))
                 (workspace (niwa-workspace--create :root remote))
                 (niwa-memory-dirs '("memory"))
                 (memory (file-name-concat directory "memory"))
                 (bytes (encode-coding-string "A \u754c fact.\r\n" 'utf-8-unix)))
            (make-directory memory)
            (let ((coding-system-for-write 'no-conversion))
              (write-region bytes nil (file-name-concat memory "topic.md") nil 'silent))
            (let* ((scope (niwa-memory-scope-capture workspace))
                   (root (car (plist-get scope :roots))))
              (should-not (plist-get (cdr root) :client))
              (should (eq (plist-get (plist-get (cdr root) :target) :method) 'niwamock))
              (should (equal bytes (plist-get (niwa-memory-scope-before scope (car root) "topic.md") :bytes)))
              (should (eq 'fresh (plist-get (niwa-memory-scope-check scope (car root) '("topic.md")) :status))))))
      (delete-directory directory t))))

(niwa-deftest niwa-memory-scope-before ()
  ,test
  (test)
  :doc "new topic absence comes from the frozen complete inventory, never a later read"
  (let* ((directory (make-temp-file "niwa-memory-before-" t))
         (workspace (niwa-workspace--create :root directory))
         (niwa-memory-dirs '("memory")))
    (unwind-protect
        (let* ((scope (niwa-memory-scope-capture workspace))
               (root (caar (plist-get scope :roots)))
               (new (file-name-concat directory "memory" "new.md")))
          (make-directory (file-name-directory new))
          (with-temp-file new (insert "Arrived after the review began.\n"))
          (should-not (plist-get (niwa-memory-scope-before scope root "new.md") :exists))
          (should (equal new (plist-get (niwa-memory-scope-before scope root "new.md") :path)))
          (should-error (niwa-memory-scope-before scope root "../escape.md"))
          (should-error (niwa-memory-scope-before scope "unknown-root" "new.md")))
      (delete-directory directory t)))
  :doc "symlinked topics and parents cannot admit external before-state or new targets"
  (let* ((directory (make-temp-file "niwa-memory-links-" t))
         (workspace (niwa-workspace--create :root directory))
         (niwa-memory-dirs '("memory"))
         (memory (file-name-concat directory "memory"))
         (outside (file-name-concat directory "outside")))
    (unwind-protect
        (progn
          (make-directory memory)
          (make-directory outside)
          (with-temp-file (file-name-concat outside "secret.md") (insert "Outside scope.\n"))
          (make-symbolic-link outside (file-name-concat memory "linked"))
          (make-symbolic-link (file-name-concat outside "secret.md") (file-name-concat memory "secret.md"))
          (let* ((scope (niwa-memory-scope-capture workspace))
                 (root (car (plist-get scope :roots))))
            (should-not (member "secret.md" (plist-get (cdr root) :files)))
            (should-error (niwa-memory-scope-before scope (car root) "secret.md"))
            (should-error (niwa-memory-scope-before scope (car root) "linked/new.md"))
            (should-not (file-exists-p (file-name-concat outside "new.md")))))
      (delete-directory directory t))))

(niwa-deftest niwa-memory-scope-check ()
  ,test
  (test)
  :doc "delayed approval detects topic/index changes and expected-absence conflicts"
  (let* ((directory (make-temp-file "niwa-memory-freshness-" t))
         (workspace (niwa-workspace--create :root directory))
         (niwa-memory-dirs '("memory"))
         (memory (file-name-concat directory "memory"))
         (topic (file-name-concat memory "topic.md"))
         (index (file-name-concat memory "MEMORY.md")))
    (unwind-protect
        (progn
          (make-directory memory)
          (with-temp-file topic (insert "Original topic.\n"))
          (with-temp-file index (insert "- [Topic](topic.md): original\n"))
          (let* ((scope (niwa-memory-scope-capture workspace))
                 (root (caar (plist-get scope :roots))))
            (should (eq 'fresh (plist-get (niwa-memory-scope-check scope root '("topic.md" "MEMORY.md")) :status)))
            (with-temp-file topic (insert "User corrected this topic.\n"))
            (should (eq 'stale (plist-get (niwa-memory-scope-check scope root '("topic.md" "MEMORY.md")) :status)))
            (with-temp-file topic (insert "Original topic.\n"))
            (with-temp-file index (insert "Changed index.\n"))
            (should (eq 'stale (plist-get (niwa-memory-scope-check scope root '("topic.md" "MEMORY.md")) :status)))
            (with-temp-file (file-name-concat memory "new.md") (insert "Concurrent creation.\n"))
            (should (eq 'stale (plist-get (niwa-memory-scope-check scope root '("new.md")) :status)))
            (delete-file topic)
            (should (eq 'stale (plist-get (niwa-memory-scope-check scope root '("topic.md")) :status)))))
      (delete-directory directory t)))
  :doc "an unobserved check rereads at most eight files per program and still sees a change"
  (let* ((directory (make-temp-file "niwa-memory-check-batch-" t))
         (workspace (niwa-workspace--create :root directory))
         (niwa-memory-dirs '("memory"))
         (memory (file-name-concat directory "memory"))
         (run (symbol-function 'niwa-session-control-fs-run-program))
         (calls 0))
    (unwind-protect
        (progn
          (make-directory memory)
          (with-temp-file (file-name-concat memory "MEMORY.md") (insert "- index\n"))
          (dotimes (index 10)
            (with-temp-file (file-name-concat memory (format "topic-%d.md" index))
              (insert (format "Fact %d.\n" index))))
          (let* ((scope (niwa-memory-scope-capture workspace))
                 (root (caar (plist-get scope :roots)))
                 (files (mapcar #'car (plist-get (cdar (plist-get scope :roots)) :before))))
            (should (= 11 (length files)))
            (cl-letf (((symbol-function 'niwa-session-control-fs-run-program)
                       (lambda (&rest args) (cl-incf calls) (apply run args))))
              (should (eq 'fresh (plist-get (niwa-memory-scope-check scope root files) :status)))
              (should (= 2 calls))
              (with-temp-file (file-name-concat memory "topic-9.md") (insert "Fact 9, longer now.\n"))
              (should (equal "Files changed since capture: topic-9.md"
                             (plist-get (niwa-memory-scope-check scope root files) :reason))))))
      (delete-directory directory t)))
  :doc "configuration changes never rebind roots and another client cannot inspect local bodies"
  (let* ((directory (make-temp-file "niwa-memory-origin-" t))
         (workspace (niwa-workspace--create :root directory))
         (memory (file-name-concat directory "global"))
         (niwa-memory-dirs (list memory)))
    (unwind-protect
        (progn
          (make-directory memory)
          (with-temp-file (file-name-concat memory "topic.md") (insert "Original root.\n"))
          (let* ((scope (niwa-memory-scope-capture workspace))
                 (root (caar (plist-get scope :roots)))
                 (niwa-memory-dirs '("different")))
            (should (eq 'fresh (plist-get (niwa-memory-scope-check scope root '("topic.md")) :status)))
            (cl-letf (((symbol-function 'system-name) (lambda () "another-client")))
              (should (eq 'unavailable (plist-get (niwa-memory-scope-check scope root '("topic.md")) :status)))
              (should-error (niwa-memory-scope-before scope root "topic.md")
                            :type 'niwa-memory-scope-unavailable))))
      (delete-directory directory t)))
  :doc "retargeting a configured root symlink makes its captured proposals unavailable"
  (let* ((directory (make-temp-file "niwa-memory-retarget-" t))
         (workspace (niwa-workspace--create :root directory))
         (alias (file-name-concat directory "memory"))
         (original (file-name-concat directory "original"))
         (replacement (file-name-concat directory "replacement"))
         (niwa-memory-dirs '("memory")))
    (unwind-protect
        (progn
          (make-directory original)
          (make-directory replacement)
          (make-symbolic-link original alias)
          (let* ((scope (niwa-memory-scope-capture workspace))
                 (root (caar (plist-get scope :roots))))
            (delete-file alias)
            (make-symbolic-link replacement alias)
            (should (eq 'unavailable (plist-get (niwa-memory-scope-check scope root '("new.md")) :status)))))
      (delete-directory directory t))))

(niwa-deftest niwa-memory-scope-source-path ()
  ,test
  (test)
  :doc "workspace investigation excludes private state, omitted memory roots, and symlink escapes"
  (let* ((directory (make-temp-file "niwa-memory-source-" t))
         (workspace (niwa-workspace--create :root directory))
         (memory (file-name-concat directory "memory"))
         (niwa-memory-dirs '("memory"))
         (outside (make-temp-file "niwa-memory-outside-" t)))
    (unwind-protect
        (progn
          (make-directory memory)
          ;; The unreadable-in-scope index must not make this root ordinary
          ;; source through a fallback after capture omits its contents.
          (with-temp-file (file-name-concat memory "MEMORY.md") (insert (make-string 40000 ?x)))
          (make-symbolic-link outside (file-name-concat directory "escape"))
          (let ((scope (niwa-memory-scope-capture workspace)))
            (should (equal (file-name-concat directory "src" "new.el")
                           (niwa-memory-scope-source-path scope "src/new.el")))
            (dolist (path '("../escape.el" "/etc/passwd" "~/.emacs" "escape/secret.el"
                            ".niwa/state/journal/job.json" ".git/config" "memory/secret.md"
                            "memory://journal/state/job.json"))
              (should-error (niwa-memory-scope-source-path scope path)))
            (cl-letf (((symbol-function 'system-name) (lambda () "another-client")))
              (should-error (niwa-memory-scope-source-path scope "src/new.el")
                            :type 'niwa-memory-scope-unavailable))))
      (delete-directory directory t)
      (delete-directory outside t))))

(niwa-deftest niwa-workspace-identity-client ()
  ,test
  (test)
  :doc "stable across observations, distinct across host and user identities"
  (cl-letf (((symbol-function 'system-name) (lambda () "host-one"))
            ((symbol-function 'user-login-name) (lambda (&optional _) "user-one")))
    (let ((identity (niwa-workspace-identity-client)))
      (should (equal identity (niwa-workspace-identity-client)))
      (should (string-match-p "\\`[0-9a-f]\\{64\\}\\'" identity))
      (cl-letf (((symbol-function 'system-name) (lambda () "host-two")))
        (should-not (equal identity (niwa-workspace-identity-client))))
      (cl-letf (((symbol-function 'user-login-name) (lambda (&optional _) "user-two")))
        (should-not (equal identity (niwa-workspace-identity-client)))))))

(niwa-deftest niwa-memory-scope--inventory ()
  ,test
  (test)
  :doc "private write coordination does not exhaust memory inventory or enter proposal/source scopes"
  (let* ((directory (make-temp-file "niwa-memory-private-control-" t))
         (workspace (niwa-workspace--create :root directory))
         (niwa-memory-dirs (list directory))
         (private (file-name-concat directory ".niwa" "state" "memory-write" "root")))
    (unwind-protect
        (progn
          (make-directory private t)
          (dotimes (n 300) (write-region "claim" nil (file-name-concat private (format "%d.claim" n)) nil 'silent))
          (write-region "Topic\n" nil (file-name-concat directory "topic.md") nil 'silent)
          (let* ((scope (niwa-memory-scope-capture workspace))
                 (root (cdr (seq-find (lambda (row) (eq (plist-get (cdr row) :kind) 'memory)) (plist-get scope :roots)))))
            (should (plist-get root :complete))
            (should (equal '("topic.md") (plist-get root :files)))
            (should-not (niwa-memory-proposal--file-p ".niwa/state/memory-write/topic.md"))
            (should-error (niwa-memory-scope-source-path scope ".niwa/state/memory-write/root/1.claim"))))
      (delete-directory directory t)))
  :doc "one listing reports names, directories and requested attributes without following links"
  (let ((directory (file-name-as-directory (make-temp-file "niwa-memory-inventory-" t)))
        (outside (make-temp-file "niwa-memory-outside-" t)))
    (unwind-protect
        (progn
          (make-directory (file-name-concat directory "nested"))
          (write-region "Topic\n" nil (file-name-concat directory "nested" "topic.md") nil 'silent)
          (write-region "Outside\n" nil (file-name-concat outside "hidden.md") nil 'silent)
          (make-symbolic-link outside (file-name-concat directory "linked"))
          (let ((inventory (niwa-memory-scope--inventory directory t)))
            (should (equal '("linked" "nested" "nested/topic.md") (plist-get inventory :existing)))
            (should (equal '("nested") (plist-get inventory :directories)))
            (should (plist-get inventory :complete))
            (should (= 6 (file-attribute-size (cdr (assoc "nested/topic.md" (plist-get inventory :attributes))))))
            (should (stringp (file-attribute-type (cdr (assoc "linked" (plist-get inventory :attributes)))))))
          (should-not (plist-member (niwa-memory-scope--inventory directory) :attributes)))
      (delete-directory directory t)
      (delete-directory outside t))))

(provide 'test-niwa-memory-scope)
;;; test-niwa-memory-scope.el ends here
