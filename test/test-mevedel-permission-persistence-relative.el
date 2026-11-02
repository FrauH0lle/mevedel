;;; test-mevedel-permission-persistence-relative.el -- Workspace grants -*- lexical-binding: t -*-

;;; Commentary:
;; Workspace-relative authority survives store edits without changing its scope.

;;; Code:

(require 'mevedel-permission-persistence)
(require 'mevedel-permissions)
(require 'mevedel-execution-grants)
(require 'mevedel-structs)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(mevedel-deftest mevedel-permission-persistent-authority-relative
  (:doc "loads relative grants in both locations and preserves their authored intent across edits")
  (let* ((root (make-temp-file "mevedel-relative-store-" t))
         (mevedel-user-dir (file-name-concat root "user"))
         (workspace (mevedel-workspace--create :type 'project :root root))
         (file (mevedel-permission-persistence-file workspace 'workspace))
         (git (file-name-concat root ".git")))
    (unwind-protect
        (progn
          (make-directory (file-name-directory file) t)
          (with-temp-file file
            (prin1 '(:rules (("Bash" :pattern "git commit *"
                             :file-system ((:path ".git" :access write :recursive t))
                             :action allow))
                     :resource-grants ((:path "./missing/file" :access read)
                                       (:path "." :access read)))
                   (current-buffer)))
          (let* ((authority (mevedel-permission-persistent-authority workspace 'workspace))
                 (profile (plist-get (cdar (plist-get authority :rules)) :file-system)))
            (should (equal git (plist-get (car profile) :path)))
            (should (equal (file-name-concat root "missing/file")
                           (plist-get (car (plist-get authority :resource-grants)) :path)))
            (should (mevedel-permission-rules-resource-granted-p
                     root 'read (plist-get authority :resource-grants)))
            (should (mevedel-permission-rules-resource-granted-p
                     (file-name-as-directory root) 'read
                     (plist-get authority :resource-grants))))
          (mevedel-permission-persistence-save-rule workspace '("Read" :action allow))
          (let ((stored (with-temp-buffer (insert-file-contents file) (read (current-buffer)))))
            (should (equal "./.git"
                           (plist-get (car (plist-get (cdar (plist-get stored :rules))
                                                     :file-system)) :path)))
            (should (equal '((:path "./missing/file" :access read)
                             (:path "./" :access read))
                           (plist-get stored :resource-grants)))))
      (delete-directory root t))))

(mevedel-deftest mevedel-permission-persistent-authority-relative-invalid
  (:doc "disables the whole store with a precise diagnostic for invalid relative grants")
  (let* ((root (make-temp-file "mevedel-relative-invalid-" t))
         (mevedel-user-dir (file-name-concat root "user"))
         (workspace (mevedel-workspace--create :type 'project :root root))
         (file (mevedel-permission-persistence-file workspace 'workspace)))
    (unwind-protect
        (progn
          (make-directory (file-name-directory file) t)
          (dolist (path '("" "../escape" "a/../inside" "work://shared/file"))
            (with-temp-file file
              (prin1 `(:rules (("Read" :action allow)
                               ("Bash" :pattern "build *"
                                :file-system ((:path ,path :access write))
                                :action allow))
                       :resource-grants nil)
                     (current-buffer)))
            (let ((mevedel--warn-once-table (make-hash-table :test #'equal)) text)
              (mevedel-test--with-captured-diagnostics text
                (mevedel-permission-validate-persistent-stores workspace))
              (should-not (plist-get (mevedel-permission-persistent-authority
                                      workspace 'workspace) :rules))
              (should (string-match-p "workspace" text))
              (should (string-match-p "rule 2.*Bash.*grant 1" text))
              (should (string-match-p (regexp-quote (prin1-to-string path)) text)))))
      (delete-directory root t))))

(mevedel-deftest mevedel-permission-remove-persistent-relative
  (:doc "revokes relative and absolute grants independently even after relative scope becomes unusable")
  (let* ((root (make-temp-file "mevedel-relative-revoke-" t))
         (outside (make-temp-file "mevedel-relative-revoke-outside-" t))
         (mevedel-user-dir (file-name-concat root "user"))
         (workspace (mevedel-workspace--create :type 'project :root root))
         (file (mevedel-permission-persistence-file workspace 'workspace))
         (vault (file-name-concat root "vault")))
    (unwind-protect
        (progn
          (make-directory vault)
          (make-directory (file-name-directory file) t)
          (with-temp-file file
            (prin1 `(:rules (("Read" :action deny))
                     :resource-grants ((:path "./vault" :access read :recursive t)
                                       (:path ,vault :access read :recursive t)))
                   (current-buffer)))
          (let* ((authority (mevedel-permission-persistent-authority workspace 'workspace))
                 (relative (car (plist-get authority :resource-grants))))
            (delete-directory vault)
            (make-symbolic-link outside vault)
            (mevedel-permission-remove-persistent-resource-grant
             workspace 'workspace vault 'read t (plist-get relative :workspace-origin))
            (let ((after (mevedel-permission-persistent-authority workspace 'workspace)))
              (should (equal '(("Read" :action deny)) (plist-get after :rules)))
              (should (equal `((:path ,vault :access read :recursive t))
                             (plist-get after :resource-grants))))))
      (delete-directory root t)
      (delete-directory outside t))))

(mevedel-deftest mevedel-permission-persistent-authority-worktree
  (:doc "anchors source-store grants despite a linked-worktree cwd and does not follow Git pointers")
  (let* ((root (make-temp-file "mevedel-relative-worktree-" t))
         (source (file-name-concat root "source"))
         (tree (file-name-concat root "tree"))
         (mevedel-user-dir (file-name-concat root "user"))
         (workspace (mevedel-workspace--create :type 'project :root source))
         (tree-workspace (mevedel-workspace--create :type 'project :root tree)))
    (unwind-protect
        (progn
          (skip-unless (executable-find "git"))
          (make-directory source)
          (with-temp-buffer
            (should (zerop (process-file "git" nil t nil "init" "-q" source)))
            (should (zerop (process-file "git" nil t nil "-C" source
                                         "-c" "user.name=Fixture" "-c" "user.email=fixture@example.invalid"
                                         "commit" "-q" "--allow-empty" "-m" "fixture")))
            (should (zerop (process-file "git" nil t nil "-C" source
                                         "worktree" "add" "-q" "--detach" tree "HEAD"))))
          (dolist (ws (list workspace tree-workspace))
            (let ((file (mevedel-permission-persistence-file ws 'workspace)))
              (make-directory (file-name-directory file) t)
              (with-temp-file file
                (prin1 '(:rules nil :resource-grants
                         ((:path ".git" :access write :recursive t)))
                       (current-buffer)))))
          (with-temp-buffer
            (setq-local mevedel--session (mevedel-session-create "worktree-cwd" workspace tree)
                        default-directory (file-name-as-directory tree))
            (should (equal (file-name-concat source ".git")
                           (plist-get (car (plist-get
                                            (mevedel-permission-persistent-authority workspace 'workspace)
                                            :resource-grants)) :path))))
          (let ((grants (plist-get (mevedel-permission-persistent-authority
                                   tree-workspace 'workspace) :resource-grants)))
            (should (file-regular-p (plist-get (car grants) :path)))
            (should-not (mevedel-execution-grants-covering-grant
                         (file-name-concat source ".git/worktrees/tree") 'write grants t))))
      (delete-directory root t))))

(mevedel-deftest mevedel-permission-persistent-authority-relative-scope
  (:doc "keeps relative syntax out of globals and durable session authority without changing path globs")
  (let* ((root (make-temp-file "mevedel-relative-scope-" t))
         (mevedel-user-dir (file-name-concat root "user"))
         (workspace (mevedel-workspace--create :type 'project :root root))
         (target (mevedel-execution-target-create root))
         (global (mevedel-permission-persistence-file workspace 'global))
         (relative '((:path "./vault" :access read))))
    (unwind-protect
        (progn
          (make-directory (file-name-directory global) t)
          (with-temp-file global
            (prin1 (list :rules nil :resource-grants relative) (current-buffer)))
          (should-not (plist-get (mevedel-permission-persistent-authority workspace 'global)
                                 :resource-grants))
          (should-not (mevedel-permission-serialize-authority nil relative target))
          (should-not (mevedel-permission-deserialize-authority nil relative target))
          (should (equal '(:rules (("Read" :path "**/.git/**" :action deny))
                           :resource-grants nil)
                         (mevedel-permission-deserialize-authority
                          '(("Read" :path "**/.git/**" :action deny)) nil target))))
      (delete-directory root t))))

(provide 'test-mevedel-permission-persistence-relative)
;;; test-mevedel-permission-persistence-relative.el ends here
