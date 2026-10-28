;;; test-mevedel-permission-relative-remote.el --- Real target relative grants -*- lexical-binding: t -*-

;;; Commentary:

;; Opt-in SSH/Podman journeys for target-owned project permission stores.
;; Provisioning belongs to run-remote-acceptance.sh, not these tests.

;;; Code:

(require 'mevedel-permission-persistence)
(require 'mevedel-permissions)
(require 'mevedel-session-durability)
(require 'mevedel-tool-permission)
(require 'mevedel-tools)
(require 'tramp)
(require 'tramp-container)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-execution-test-helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "mevedel-execution-test-helpers"))

(mevedel-deftest mevedel-permission-relative-real-target
  (:quiet t :tags (external remote)
          :before-each (mevedel-tools-register)
          :after-each ((tramp-cleanup-all-connections)))
  (let* ((base (test-mevedel-execution-remote--real-root ,variable ,method))
         (root (test-mevedel-execution-remote--real-temp-directory
                base "mevedel-relative-permissions-" t))
         (outside (test-mevedel-execution-remote--real-temp-directory
                   base "mevedel-relative-unapproved-" t))
         (session (test-mevedel-execution--session root))
         (workspace (mevedel-session-workspace session))
         (target (mevedel-session-execution-target session))
         (buffer (generate-new-buffer " *relative-permissions-owner*"))
         (file (mevedel-permission-persistence-file workspace 'workspace))
         (git (file-name-concat root ".git"))
         (path (file-name-concat git "state.txt"))
         (cwd (file-name-as-directory (file-name-concat root "other-cwd")))
         (mevedel-permission-rules nil)
         (mevedel-protected-paths '(("**/.git/**" . inaccessible))))
    (unwind-protect
        (with-current-buffer buffer
          (setq-local mevedel--session session mevedel--workspace workspace
                      default-directory cwd)
          (setf (mevedel-session-permission-mode session) 'ask)
          (make-directory cwd t)
          (make-directory git t)
          (with-temp-file path (insert "original\n"))
          (with-temp-file (file-name-concat outside "state.txt")
            (insert "unapproved\n"))
          (make-directory (file-name-directory file) t)
          (with-temp-file file
            (prin1 '(:rules (("Bash" :pattern "git status *" :action allow
                             :file-system ((:path ".git" :access read :recursive t))))
                     :resource-grants ((:path "./.git" :access read :recursive t)))
                   (current-buffer)))
          (should (eq 'ready (plist-get
                              (mevedel-execution-target-probe target t 'off)
                              :status)))
          (should (mevedel-session-durability-lease-acquire
                   (mevedel-session-save-path session) "relative" session))
          (let* ((authority (mevedel-permission-persistent-authority workspace 'workspace))
                 (grant (car (plist-get authority :resource-grants)))
                 (profile (car (plist-get (cdar (plist-get authority :rules)) :file-system))))
            (should (equal git (plist-get grant :path)))
            (should (equal git (plist-get profile :path)))
            (should (eq 'allow (mevedel-tool-permission-decide-now
                               "Read" (list :file_path path) buffer)))
            ;; Exercise the actual remote publication writer, not just its codec.
            (mevedel-permission-persistence-save-rule
             workspace (car (plist-get authority :rules)))
            (let* ((raw (with-temp-buffer (insert-file-contents file) (read (current-buffer))))
                   (raw-profile (car (plist-get (cdar (plist-get raw :rules)) :file-system))))
              (should (equal '(:path "./.git" :access read :recursive t)
                             (car (plist-get raw :resource-grants))))
              (should (equal '(:path "./.git" :access read :recursive t) raw-profile)))
            ;; Frozen permission configuration must not freeze filesystem safety.
            (mevedel-permission-freeze-context
             (copy-tree (plist-get authority :rules))
             (copy-tree (plist-get authority :resource-grants)))
            (delete-directory git t)
            (make-symbolic-link outside git)
            (should (eq 'ask (mevedel-tool-permission-decide-now
                             "Read" (list :file_path path) buffer)))))
      (mevedel-session-durability-lease-release
       (mevedel-session-save-path session) session)
      (kill-buffer buffer)
      (delete-directory root t)
      (delete-directory outside t)))
  (variable method)
  :doc "round-trips an SSH workspace store and rejects a changed symlink under frozen authority"
  "MEVEDEL_TEST_SSH_ROOT" 'ssh
  :doc "round-trips a Podman workspace store and rejects a changed symlink under frozen authority"
  "MEVEDEL_TEST_PODMAN_ROOT" 'podman)

(mevedel-deftest mevedel-permission-relative-real-alias
  (:quiet t :tags (external remote aliases)
          :after-each ((tramp-cleanup-all-connections)))
  ,test (test)
  :doc "reopens a project-relative store through another SSH alias of the same target"
  (let* ((base-a (test-mevedel-execution-remote--real-root
                  "MEVEDEL_TEST_SSH_ALIAS_A_ROOT" 'ssh))
         (base-b (test-mevedel-execution-remote--real-root
                  "MEVEDEL_TEST_SSH_ALIAS_B_ROOT" 'ssh))
         (root-a (test-mevedel-execution-remote--real-temp-directory
                  base-a "mevedel-relative-alias-" t))
         (root-b (file-name-as-directory
                  (file-name-concat base-b (file-relative-name root-a base-a))))
         (workspace-a (test-mevedel-execution--workspace root-a))
         (workspace-b (test-mevedel-execution--workspace root-b))
         (file-a (mevedel-permission-persistence-file workspace-a 'workspace)))
    (unwind-protect
        (progn
          (make-directory (file-name-directory file-a) t)
          (with-temp-file file-a
            (prin1 '(:rules nil :resource-grants ((:path "./.git" :access read)))
                   (current-buffer)))
          (let* ((a (mevedel-permission-persistent-authority workspace-a 'workspace))
                 (b (mevedel-permission-persistent-authority workspace-b 'workspace)))
            (should (equal (file-name-concat root-a ".git")
                           (plist-get (car (plist-get a :resource-grants)) :path)))
            (should (equal (file-name-concat root-b ".git")
                           (plist-get (car (plist-get b :resource-grants)) :path)))))
      (delete-directory root-a t))))

(provide 'test-mevedel-permission-relative-remote)
;;; test-mevedel-permission-relative-remote.el ends here
