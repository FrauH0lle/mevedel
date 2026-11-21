;;; test-niwa-permission-relative-resume.el -- Leased grant resume -*- lexical-binding: t -*-

;;; Commentary:
;; Real project publication and reopening must reload workspace authority rather
;; than preserving an unbounded session copy of relative permission grants.

;;; Code:

(require 'niwa-session-test-support
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "niwa-session-test-support"))
(require 'niwa-permission-persistence)
(require 'niwa-permissions)
(require 'niwa-tool-exec-permission)
(require 'niwa-tool-permission)

(niwa-deftest niwa-session-persistence-resume-id/relative-authority
  (:quiet t :doc "reopens a leased publication with revocable workspace grants")
  (let* ((root (make-temp-file "niwa-relative-resume-" t))
         (niwa-user-dir (file-name-concat root "user"))
         (workspace (test-niwa-session-persistence--make-workspace root))
         (session (niwa-session-create "relative-resume" workspace))
         (store (niwa-permission-persistence-file workspace 'workspace))
         (vault (file-name-concat root "vault"))
         (secret (file-name-concat vault "secret.txt"))
         (owner (generate-new-buffer " *relative-resume-owner*"))
         restored restored-session directory session-id)
    (unwind-protect
        (progn
          (make-directory vault)
          (make-directory (file-name-concat root "cache"))
          (make-directory (file-name-directory store) t)
          (with-temp-file secret (insert "workspace secret\n"))
          (with-temp-file store
            (prin1 '(:rules (("Bash" :pattern "true"
                             :file-system ((:path "./cache" :access write :recursive t))
                             :action allow))
                     :resource-grants ((:path "./vault" :access read :recursive t)))
                   (current-buffer)))
          (setf (niwa-session-permission-mode session) 'ask)
          (with-current-buffer owner
            (org-mode)
            (setq-local niwa--session session
                        niwa--workspace workspace
                        default-directory (file-name-as-directory root))
            (insert "Inspect workspace authority\n"
                    (propertize "Inspection complete\n" 'gptel 'response))
            (let* ((rules (niwa-permission-persistence-load-rules workspace))
                   (profile (plist-get (cdar rules) :file-system)))
              (should (plist-get (car profile) :workspace-origin))
              (should (eq 'allow
                          (niwa-permission--apply-prompt-result
                           'always-allow "Bash" session workspace nil
                           :spec-key :pattern :spec-value "true" :file-system profile))))
            ;; This writes actual transcript/sidecar artifacts under an acquired
            ;; portable lease, rather than directly serializing a session plist.
            (niwa-session-artifacts-save session owner))
          (setq directory (niwa-session-save-path session)
                session-id (niwa-session-session-id session))
          (should (eq 'portable (niwa-session-authority-mode session)))
          (should (niwa-session-durability-lease-owned-p session))
          (should (file-directory-p (file-name-concat directory ".lease")))
          (should (niwa-session-publication-read directory))
          (let* ((text (niwa-session-artifacts-read-artifact session "session.meta.el" t))
                 (sidecar (car (read-from-string text))))
            (should-not (string-search ":workspace-origin" text))
            (should-not (plist-get sidecar :permission-rules))
            (should-not (plist-get sidecar :resource-grants)))
          (test-niwa-session-persistence--release-and-kill owner session)
          (setq owner nil)
          (should-not (niwa-session-durability-lease-owned-p session))
          ;; Reopen from committed artifacts through the production resume entry
          ;; point, without passing an in-memory session override.
          (setq restored (niwa-session-persistence-resume-id workspace session-id))
          (should (buffer-live-p restored))
          (with-current-buffer restored
            (setq restored-session niwa--session)
            (should-not (eq session restored-session))
            (should (niwa-session-durability-lease-owned-p restored-session))
            (should (string-match-p "Inspection complete" (buffer-string)))
            (should-not (niwa-session-permission-rules restored-session))
            (should-not (niwa-session-resource-grants restored-session))
            (setq-local niwa-permission-rules nil
                        niwa-protected-paths
                        `((,(file-name-concat vault "**") . inaccessible)))
            (should (eq 'allow (niwa-tool-permission-decide-now
                               "Read" (list :file_path secret) restored)))
            (let* ((args '(:command "true"))
                   (request (niwa-tool-exec-permission-effective-sandbox-request
                             args "Bash" "true" nil
                             (niwa-tool-exec-permission-current-context
                              "Bash" args restored-session))))
              (should (plist-get (car (plist-get (plist-get request :additional-permissions)
                                                 :file-system)) :workspace-origin)))
            (let* ((authority (niwa-permission-persistent-authority workspace 'workspace))
                   (grant (car (plist-get authority :resource-grants)))
                   (rule (car (plist-get authority :rules))))
              (niwa-permission-remove-persistent-resource-grant
               workspace 'workspace vault 'read t (plist-get grant :workspace-origin))
              (niwa-permission-remove-persistent-rule workspace 'workspace rule))
            (should (eq 'ask (niwa-tool-permission-decide-now
                             "Read" (list :file_path secret) restored)))
            (let* ((args '(:command "true"))
                   (request (niwa-tool-exec-permission-effective-sandbox-request
                             args "Bash" "true" nil
                             (niwa-tool-exec-permission-current-context
                              "Bash" args restored-session))))
              (should-not (plist-get request :additional-permissions)))
            (should-not (niwa-session-permission-rules restored-session))
            (should-not (niwa-session-resource-grants restored-session))))
      (test-niwa-session-persistence--release-and-kill owner session)
      (test-niwa-session-persistence--release-and-kill restored restored-session)
      (delete-directory root t)
      (niwa-workspace-clear-registry))))

(provide 'test-niwa-permission-relative-resume)
;;; test-niwa-permission-relative-resume.el ends here
