;;; test-niwa-permission-relative-consumption.el --- Relative authority consumers -*- lexical-binding: t; -*-

;;; Commentary:
;; Exercise workspace-store authority through frozen conversations and consumers.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'niwa)
(require 'niwa-permission-persistence)
(require 'niwa-sandbox)
(require 'niwa-session-codec)
(require 'niwa-side-conversation)
(require 'niwa-tool-exec-permission)
(require 'niwa-tool-fs-read)
(require 'niwa-tool-permission)
(require 'niwa-view)
(require 'niwa-workspace-identity)

(niwa-deftest niwa-side-conversation-open/relative-authority
  (:doc "freezes store-root authority, not cwd authority or a symlink replacement")
  (let* ((root (make-temp-file "niwa-relative-btw-" t))
         (cwd (file-name-concat root "child"))
         (vault (file-name-concat root "vault"))
         (outside (file-name-concat root "outside"))
         (path (file-name-concat vault "secret.txt"))
         (cwd-path (file-name-concat cwd "vault/secret.txt"))
         (workspace (niwa-workspace--create
                     :type 'project :id root :root root :name "relative-btw"))
         (session (niwa-session-create "parent" workspace cwd))
         (store (niwa-permission-persistence-file workspace 'workspace))
         diagnostics side-view side-data)
    (unwind-protect
        (niwa-test--with-captured-diagnostics diagnostics
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
          (setf (niwa-session-permission-mode session) 'ask)
          (niwa-view-test--with-buffers
            (with-current-buffer data-buf
              (setq-local niwa--workspace workspace
                          niwa--session session
                          default-directory (file-name-as-directory cwd)
                          niwa-permission-rules nil
                          niwa-protected-paths
                          `((,(file-name-concat root "vault/**") . inaccessible)
                            (,(file-name-concat cwd "vault/**") . inaccessible)))
              (insert "*** Parent question\n"
                      (propertize "Parent answer\n" 'gptel 'response))
              (cl-letf (((symbol-function 'pop-to-buffer)
                         (lambda (buffer &rest _) (setq side-view buffer))))
                (niwa-side-conversation-open)))
            (should (buffer-live-p side-view))
            (setq side-data (buffer-local-value 'niwa--data-buffer side-view))
            (should (eq 'allow
                        (niwa-tool-permission-decide-now
                         "Read" (list :file_path path) side-data)))
            (ert-info ("The relative store grant does not follow the side cwd")
              (should (eq 'ask
                          (niwa-tool-permission-decide-now
                           "Read" (list :file_path cwd-path) side-data))))
            ;; Later store edits must not replace the side's captured authority.
            (with-temp-file store
              (prin1 '(:rules nil :resource-grants nil) (current-buffer)))
            (should (eq 'allow
                        (niwa-tool-permission-decide-now
                         "Read" (list :file_path path) side-data)))
            ;; Freezing policy must not freeze the validity of its backing path.
            (delete-directory vault t)
            (make-symbolic-link outside vault)
            (ert-info ("Frozen relative authority rejects a replacement symlink")
              (should (eq 'ask
                          (niwa-tool-permission-decide-now
                           "Read" (list :file_path path) side-data))))))
      (when (buffer-live-p side-view) (kill-buffer side-view))
      (when (buffer-live-p side-data) (kill-buffer side-data))
      (delete-directory root t))
    (should (string-empty-p diagnostics))))

(niwa-deftest niwa-tool-exec-permission-effective-sandbox-request/relative-authority
  (:doc "carries covering store authority into the launch grant and refuses a replaced ancestor")
  (let* ((root (make-temp-file "niwa-relative-profile-" t))
         (niwa-user-dir (file-name-concat root "user"))
         (niwa-permission-rules nil)
         (niwa-protected-paths nil)
         (niwa-sandbox--probe-cache
          `((nil . (:available t
                    :executable ,(or (executable-find "bwrap") "bwrap")
                    :mount-proc t))))
         (workspace (niwa-workspace--create
                     :type 'project :id root :root root :name "relative-profile"))
         (session (niwa-session-create "profile" workspace root))
         (store (niwa-permission-persistence-file workspace 'workspace))
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
        (niwa-test--with-captured-diagnostics diagnostics
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
            (setq-local niwa--workspace workspace
                        niwa--session session
                        default-directory (file-name-as-directory root))
            (let* ((context (niwa-tool-exec-permission-current-context
                             "Bash" args session))
                   (request (niwa-tool-exec-permission-effective-sandbox-request
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
              (setq prepared (niwa-sandbox-prepare
                              '("true") root (list root) additional nil 'required))
              (should (eq 'confined (plist-get prepared :state)))
              (niwa-sandbox-cleanup prepared)
              (setq prepared nil)
              (delete-directory vault t)
              (make-symbolic-link outside vault)
              (setq prepared (niwa-sandbox-prepare
                              '("true") root (list root) additional nil 'required))
              (should (eq 'refused (plist-get prepared :state)))
              (should-not (plist-get prepared :command))
              (should (string-match-p
                       "Workspace-relative filesystem grant is no longer contained"
                       (plist-get prepared :error))))))
      (niwa-sandbox-cleanup prepared)
      (delete-directory root t))
    (should (string-empty-p diagnostics))))

(niwa-deftest niwa-session-codec-serialize/relative-authority
  (:doc "always-allow keeps bounded profiles in the workspace, not durable session authority")
  (let* ((root (make-temp-file "niwa-relative-codec-" t))
         (niwa-user-dir (file-name-concat root "user"))
         (niwa-permission-rules nil)
         (workspace (niwa-workspace--create
                     :type 'project :id root :root root :name "relative-codec"))
         (session (niwa-session-create "codec" workspace root))
         (store (niwa-permission-persistence-file workspace 'workspace))
         (baseline '(("WebFetch" :domain "example.test" :action allow)))
         diagnostics)
    (unwind-protect
        (niwa-test--with-captured-diagnostics diagnostics
          (make-directory (file-name-concat root "cache") t)
          (make-directory (file-name-directory store) t)
          (niwa-workspace-identity-ensure root)
          (setf (niwa-session-permission-rules session) (copy-tree baseline)
                (niwa-execution-target-incarnation
                 (niwa-session-execution-target session))
                "relative-codec-target")
          (with-temp-file store
            (prin1 '(:rules (("Bash" :pattern "true" :action allow
                             :file-system ((:path "./cache" :access write
                                            :recursive t))))
                     :resource-grants nil)
                   (current-buffer)))
          ;; Remember an actual store-derived profile, never a fabricated origin.
          (let* ((rules (niwa-permission-persistence-load-rules workspace))
                 (profile (plist-get (cdar rules) :file-system)))
            (should (plist-get (car profile) :workspace-origin))
            (should (eq 'allow
                        (niwa-permission--apply-prompt-result
                         'always-allow "Bash" session workspace nil
                         :spec-key :pattern :spec-value "true"
                         :file-system profile))))
          (should (equal baseline (niwa-session-permission-rules session)))
          (should-not (niwa-session-resource-grants session))
          (let* ((sidecar (niwa-session-codec-serialize session))
                 (restored (plist-get
                            (niwa-session-codec-deserialize sidecar workspace)
                            :session)))
            (should (equal baseline (plist-get sidecar :permission-rules)))
            (should-not (plist-get sidecar :resource-grants))
            (should-not (string-search ":workspace-origin" (prin1-to-string sidecar)))
            (should (equal baseline (niwa-session-permission-rules restored)))
            (with-temp-buffer
              (setq-local niwa--workspace workspace
                          niwa--session restored
                          default-directory (file-name-as-directory root))
              (let* ((args '(:command "true"))
                     (request
                      (niwa-tool-exec-permission-effective-sandbox-request
                       args "Bash" "true" nil
                       (niwa-tool-exec-permission-current-context
                        "Bash" args restored))))
                (should (plist-get (plist-get request :additional-permissions)
                                   :file-system))
                ;; Revoking workspace authority must also revoke it after resume.
                (with-temp-file store
                  (prin1 '(:rules nil :resource-grants nil) (current-buffer)))
                ;; Match the refresh performed before a new dispatched call.
                (niwa-permission-validate-persistent-stores workspace)
                (setq request
                      (niwa-tool-exec-permission-effective-sandbox-request
                       args "Bash" "true" nil
                       (niwa-tool-exec-permission-current-context
                        "Bash" args restored)))
                (should-not (plist-get request :additional-permissions))))))
      (delete-directory root t))
    (should (string-empty-p diagnostics))))

(provide 'test-niwa-permission-relative-consumption)
;;; test-niwa-permission-relative-consumption.el ends here
