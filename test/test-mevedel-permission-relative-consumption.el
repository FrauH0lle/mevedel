;;; test-mevedel-permission-relative-consumption.el --- Relative authority consumers -*- lexical-binding: t; -*-

;;; Commentary:
;; Exercise workspace-store authority through frozen conversations and consumers.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel)
(require 'mevedel-permission-persistence)
(require 'mevedel-sandbox)
(require 'mevedel-session-codec)
(require 'mevedel-side-conversation)
(require 'mevedel-tool-exec-permission)
(require 'mevedel-tool-fs-read)
(require 'mevedel-tool-permission)
(require 'mevedel-view)
(require 'mevedel-workspace-identity)

(mevedel-deftest mevedel-side-conversation-open/relative-authority
  (:doc "freezes store-root authority, not cwd authority or a symlink replacement")
  (let* ((root (make-temp-file "mevedel-relative-btw-" t))
         (cwd (file-name-concat root "child"))
         (vault (file-name-concat root "vault"))
         (outside (file-name-concat root "outside"))
         (path (file-name-concat vault "secret.txt"))
         (cwd-path (file-name-concat cwd "vault/secret.txt"))
         (workspace (mevedel-workspace--create
                     :type 'project :id root :root root :name "relative-btw"))
         (session (mevedel-session-create "parent" workspace cwd))
         (store (mevedel-permission-persistence-file workspace 'workspace))
         diagnostics side-view side-data)
    (unwind-protect
        (mevedel-test--with-captured-diagnostics diagnostics
          (make-directory (file-name-directory store) t)
          (dolist (directory (list vault outside (file-name-directory cwd-path)))
            (make-directory directory t)
            (with-temp-file (file-name-concat directory "secret.txt")
              (insert "private content\n")))
          (with-temp-file store
            (prin1 '(:rules nil
                     :resource-grants
                     ((:path "./vault" :access read :recursive t)))
                   (current-buffer)))
          (setf (mevedel-session-permission-mode session) 'ask)
          (mevedel-view-test--with-buffers
            (with-current-buffer data-buf
              (setq-local mevedel--workspace workspace
                          mevedel--session session
                          default-directory (file-name-as-directory cwd)
                          mevedel-permission-rules nil
                          mevedel-protected-paths
                          `((,(file-name-concat root "vault/**") . inaccessible)
                            (,(file-name-concat cwd "vault/**") . inaccessible)))
              (insert "*** Parent question\n"
                      (propertize "Parent answer\n" 'gptel 'response))
              (cl-letf (((symbol-function 'pop-to-buffer)
                         (lambda (buffer &rest _) (setq side-view buffer))))
                (mevedel-side-conversation-open)))
            (should (buffer-live-p side-view))
            (setq side-data (buffer-local-value 'mevedel--data-buffer side-view))
            (should (eq 'allow
                        (mevedel-tool-permission-decide-now
                         "Read" (list :file_path path) side-data)))
            (ert-info ("The relative store grant does not follow the side cwd")
              (should (eq 'ask
                          (mevedel-tool-permission-decide-now
                           "Read" (list :file_path cwd-path) side-data))))
            ;; Later store edits must not replace the side's captured authority.
            (with-temp-file store
              (prin1 '(:rules nil :resource-grants nil) (current-buffer)))
            (should (eq 'allow
                        (mevedel-tool-permission-decide-now
                         "Read" (list :file_path path) side-data)))
            ;; Freezing policy must not freeze the validity of its backing path.
            (delete-directory vault t)
            (make-symbolic-link outside vault)
            (ert-info ("Frozen relative authority rejects a replacement symlink")
              (should (eq 'ask
                          (mevedel-tool-permission-decide-now
                           "Read" (list :file_path path) side-data))))))
      (when (buffer-live-p side-view) (kill-buffer side-view))
      (when (buffer-live-p side-data) (kill-buffer side-data))
      (delete-directory root t))
    (should (string-empty-p diagnostics))))

(mevedel-deftest mevedel-tool-exec-permission-effective-sandbox-request/relative-authority
  (:doc "carries covering store authority into the launch grant and refuses a replaced ancestor")
  (let* ((root (make-temp-file "mevedel-relative-profile-" t))
         (mevedel-user-dir (file-name-concat root "user"))
         (mevedel-permission-rules nil)
         (mevedel-protected-paths nil)
         (mevedel-sandbox--probe-cache
          `((nil . (:available t
                    :executable ,(or (executable-find "bwrap") "bwrap")
                    :mount-proc t))))
         (workspace (mevedel-workspace--create
                     :type 'project :id root :root root :name "relative-profile"))
         (session (mevedel-session-create "profile" workspace root))
         (store (mevedel-permission-persistence-file workspace 'workspace))
         (vault (file-name-concat root "vault"))
         (child (file-name-concat vault "child"))
         (cache (file-name-concat root "cache"))
         (outside (file-name-concat root "outside"))
         (args `(:command "true"
                 :sandbox_permissions "with_additional_permissions"
                 :additional_permissions (:file_system (:write [,child]))
                 :justification "Write the selected child tree"))
         diagnostics prepared)
    (unwind-protect
        (mevedel-test--with-captured-diagnostics diagnostics
          (dolist (directory (list child cache (file-name-concat outside "child")
                                   (file-name-directory store)))
            (make-directory directory t))
          (with-temp-file store
            (prin1 '(:rules (("Bash" :pattern "true" :action allow
                             :file-system ((:path "./cache" :access write
                                            :recursive t))))
                     :resource-grants ((:path "./vault" :access write :recursive t)))
                   (current-buffer)))
          (with-temp-buffer
            (setq-local mevedel--workspace workspace
                        mevedel--session session
                        default-directory (file-name-as-directory root))
            (let* ((context (mevedel-tool-exec-permission-current-context
                             "Bash" args session))
                   (request (mevedel-tool-exec-permission-effective-sandbox-request
                             args "Bash" "true" nil context))
                   (additional (plist-get request :additional-permissions))
                   (grants (plist-get additional :file-system))
                   (children (cl-remove-if-not
                              (lambda (grant) (equal child (plist-get grant :path)))
                              grants)))
              (should (eq 'additive (plist-get request :level)))
              (should (= 2 (length grants)))
              (should (= 1 (length children)))
              (let ((grant (car children)))
                (should (eq 'write (plist-get grant :access)))
                (should (plist-get grant :recursive))
                (should (equal (list :root (file-name-as-directory root)
                                     :relative "./vault/child")
                               (plist-get grant :workspace-origin))))
              (should (cl-find-if
                       (lambda (grant)
                         (and (equal cache (plist-get grant :path))
                              (plist-get grant :workspace-origin)))
                       grants))
              ;; Preparation only: the cached backend fixture launches no process.
              (setq prepared (mevedel-sandbox-prepare
                              '("true") root (list root) additional nil 'required))
              (should (eq 'confined (plist-get prepared :state)))
              (mevedel-sandbox-cleanup prepared)
              (setq prepared nil)
              (delete-directory vault t)
              (make-symbolic-link outside vault)
              (setq prepared (mevedel-sandbox-prepare
                              '("true") root (list root) additional nil 'required))
              (should (eq 'refused (plist-get prepared :state)))
              (should-not (plist-get prepared :command))
              (should (string-match-p
                       "Workspace-relative filesystem grant is no longer contained"
                       (plist-get prepared :error))))))
      (mevedel-sandbox-cleanup prepared)
      (delete-directory root t))
    (should (string-empty-p diagnostics))))

(mevedel-deftest mevedel-session-codec-serialize/relative-authority
  (:doc "always-allow keeps bounded profiles in the workspace, not durable session authority")
  (let* ((root (make-temp-file "mevedel-relative-codec-" t))
         (mevedel-user-dir (file-name-concat root "user"))
         (mevedel-permission-rules nil)
         (workspace (mevedel-workspace--create
                     :type 'project :id root :root root :name "relative-codec"))
         (session (mevedel-session-create "codec" workspace root))
         (store (mevedel-permission-persistence-file workspace 'workspace))
         (baseline '(("WebFetch" :domain "example.test" :action allow)))
         diagnostics)
    (unwind-protect
        (mevedel-test--with-captured-diagnostics diagnostics
          (make-directory (file-name-concat root "cache") t)
          (make-directory (file-name-directory store) t)
          (mevedel-workspace-identity-ensure root)
          (setf (mevedel-session-permission-rules session) (copy-tree baseline)
                (mevedel-execution-target-incarnation
                 (mevedel-session-execution-target session))
                "relative-codec-target")
          (with-temp-file store
            (prin1 '(:rules (("Bash" :pattern "true" :action allow
                             :file-system ((:path "./cache" :access write
                                            :recursive t))))
                     :resource-grants nil)
                   (current-buffer)))
          ;; Remember an actual store-derived profile, never a fabricated origin.
          (let* ((rules (mevedel-permission-persistence-load-rules workspace))
                 (profile (plist-get (cdar rules) :file-system)))
            (should (plist-get (car profile) :workspace-origin))
            (should (eq 'allow
                        (mevedel-permission--apply-prompt-result
                         'always-allow "Bash" session workspace nil
                         :spec-key :pattern :spec-value "true"
                         :file-system profile))))
          (should (equal baseline (mevedel-session-permission-rules session)))
          (should-not (mevedel-session-resource-grants session))
          (let* ((sidecar (mevedel-session-codec-serialize session))
                 (restored (plist-get
                            (mevedel-session-codec-deserialize sidecar workspace)
                            :session)))
            (should (equal baseline (plist-get sidecar :permission-rules)))
            (should-not (plist-get sidecar :resource-grants))
            (should-not (string-search ":workspace-origin" (prin1-to-string sidecar)))
            (should (equal baseline (mevedel-session-permission-rules restored)))
            (with-temp-buffer
              (setq-local mevedel--workspace workspace
                          mevedel--session restored
                          default-directory (file-name-as-directory root))
              (let* ((args '(:command "true"))
                     (request
                      (mevedel-tool-exec-permission-effective-sandbox-request
                       args "Bash" "true" nil
                       (mevedel-tool-exec-permission-current-context
                        "Bash" args restored))))
                (should (plist-get (plist-get request :additional-permissions)
                                   :file-system))
                ;; Revoking workspace authority must also revoke it after resume.
                (with-temp-file store
                  (prin1 '(:rules nil :resource-grants nil) (current-buffer)))
                ;; Match the refresh performed before a new dispatched call.
                (mevedel-permission-validate-persistent-stores workspace)
                (setq request
                      (mevedel-tool-exec-permission-effective-sandbox-request
                       args "Bash" "true" nil
                       (mevedel-tool-exec-permission-current-context
                        "Bash" args restored)))
                (should-not (plist-get request :additional-permissions))))))
      (delete-directory root t))
    (should (string-empty-p diagnostics))))

(provide 'test-mevedel-permission-relative-consumption)
;;; test-mevedel-permission-relative-consumption.el ends here
