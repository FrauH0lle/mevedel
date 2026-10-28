;;; test-mevedel-permission-relative-resume.el -- Leased grant resume -*- lexical-binding: t -*-

;;; Commentary:
;; Real project publication and reopening must reload workspace authority rather
;; than preserving an unbounded session copy of relative permission grants.

;;; Code:

(require 'mevedel-session-test-support
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "mevedel-session-test-support"))
(require 'mevedel-permission-persistence)
(require 'mevedel-permissions)
(require 'mevedel-tool-exec-permission)
(require 'mevedel-tool-permission)

(mevedel-deftest mevedel-session-persistence-resume-id/relative-authority
  (:quiet t :doc "reopens a leased publication with revocable workspace grants")
  (let* ((root (make-temp-file "mevedel-relative-resume-" t))
         (mevedel-user-dir (file-name-concat root "user"))
         (workspace (test-mevedel-session-persistence--make-workspace root))
         (session (mevedel-session-create "relative-resume" workspace))
         (store (mevedel-permission-persistence-file workspace 'workspace))
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
          (setf (mevedel-session-permission-mode session) 'ask)
          (with-current-buffer owner
            (org-mode)
            (setq-local mevedel--session session
                        mevedel--workspace workspace
                        default-directory (file-name-as-directory root))
            (insert "Inspect workspace authority\n"
                    (propertize "Inspection complete\n" 'gptel 'response))
            (let* ((rules (mevedel-permission-persistence-load-rules workspace))
                   (profile (plist-get (cdar rules) :file-system)))
              (should (plist-get (car profile) :workspace-origin))
              (should (eq 'allow
                          (mevedel-permission--apply-prompt-result
                           'always-allow "Bash" session workspace nil
                           :spec-key :pattern :spec-value "true" :file-system profile))))
            ;; This writes actual transcript/sidecar artifacts under an acquired
            ;; portable lease, rather than directly serializing a session plist.
            (mevedel-session-artifacts-save session owner))
          (setq directory (mevedel-session-save-path session)
                session-id (mevedel-session-session-id session))
          (should (eq 'portable (mevedel-session-authority-mode session)))
          (should (mevedel-session-durability-lease-owned-p session))
          (should (file-directory-p (file-name-concat directory ".lease")))
          (should (mevedel-session-publication-read directory))
          (let* ((text (mevedel-session-artifacts-read-artifact session "session.meta.el" t))
                 (sidecar (car (read-from-string text))))
            (should-not (string-search ":workspace-origin" text))
            (should-not (plist-get sidecar :permission-rules))
            (should-not (plist-get sidecar :resource-grants)))
          (test-mevedel-session-persistence--release-and-kill owner session)
          (setq owner nil)
          (should-not (mevedel-session-durability-lease-owned-p session))
          ;; Reopen from committed artifacts through the production resume entry
          ;; point, without passing an in-memory session override.
          (setq restored (mevedel-session-persistence-resume-id workspace session-id))
          (should (buffer-live-p restored))
          (with-current-buffer restored
            (setq restored-session mevedel--session)
            (should-not (eq session restored-session))
            (should (mevedel-session-durability-lease-owned-p restored-session))
            (should (string-match-p "Inspection complete" (buffer-string)))
            (should-not (mevedel-session-permission-rules restored-session))
            (should-not (mevedel-session-resource-grants restored-session))
            (setq-local mevedel-permission-rules nil
                        mevedel-protected-paths
                        `((,(file-name-concat vault "**") . inaccessible)))
            (should (eq 'allow (mevedel-tool-permission-decide-now
                               "Read" (list :file_path secret) restored)))
            (let* ((args '(:command "true"))
                   (request (mevedel-tool-exec-permission-effective-sandbox-request
                             args "Bash" "true" nil
                             (mevedel-tool-exec-permission-current-context
                              "Bash" args restored-session))))
              (should (plist-get (car (plist-get (plist-get request :additional-permissions)
                                                 :file-system)) :workspace-origin)))
            (let* ((authority (mevedel-permission-persistent-authority workspace 'workspace))
                   (grant (car (plist-get authority :resource-grants)))
                   (rule (car (plist-get authority :rules))))
              (mevedel-permission-remove-persistent-resource-grant
               workspace 'workspace vault 'read t (plist-get grant :workspace-origin))
              (mevedel-permission-remove-persistent-rule workspace 'workspace rule))
            (should (eq 'ask (mevedel-tool-permission-decide-now
                             "Read" (list :file_path secret) restored)))
            (let* ((args '(:command "true"))
                   (request (mevedel-tool-exec-permission-effective-sandbox-request
                             args "Bash" "true" nil
                             (mevedel-tool-exec-permission-current-context
                              "Bash" args restored-session))))
              (should-not (plist-get request :additional-permissions)))
            (should-not (mevedel-session-permission-rules restored-session))
            (should-not (mevedel-session-resource-grants restored-session))))
      (test-mevedel-session-persistence--release-and-kill owner session)
      (test-mevedel-session-persistence--release-and-kill restored restored-session)
      (delete-directory root t)
      (mevedel-workspace-clear-registry))))

(provide 'test-mevedel-permission-relative-resume)
;;; test-mevedel-permission-relative-resume.el ends here
