;;; test-niwa-session-persistence-remote-resume.el --- Remote resume tests -*- lexical-binding: t -*-

;;; Commentary:

;; Focused acceptance coverage for resuming a rebuilt execution target.

;;; Code:

(require 'niwa)
(require 'niwa-execution-target)
(require 'niwa-permissions)
(require 'niwa-session-durability)
(require 'niwa-session-publication)
(require 'niwa-session-persistence)
(require 'niwa-workspace-identity)
(require 'tramp)
(require 'tramp-container)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

;; `tramp'
(declare-function tramp-cleanup-all-connections "tramp")
(defvar tramp-histfile-override)
(defvar tramp-local-host-regexp)
(defvar tramp-methods)

(niwa-deftest niwa-session-persistence-resume-rebuilt-target ()
  ,test
  (test)
  :doc "resumes a rebuilt container and durably revokes its old exact grants"
  (let* ((host "niwa-rebuilt-container")
         (local-root
          (file-name-as-directory
           (make-temp-file "niwa-rebuilt-container-" t)))
         (remote-root (format "/docker:%s:%s" host local-root))
         ;; This Docker-shaped fixture executes on the local filesystem.
         (niwa-test--write-roots
          (cons (format "/docker:%s:%s" host temporary-file-directory)
                niwa-test--write-roots))
         (grant-path (file-name-concat remote-root "granted.el"))
         (incarnation-a (secure-hash 'sha256 "incarnation-a"))
         (client-a (make-string 64 ?a))
         (client-b (make-string 64 ?b))
         (tramp-methods (copy-tree tramp-methods))
         (tramp-histfile-override t)
         (tramp-local-host-regexp
          (concat "\\`" (regexp-quote host) "\\'"))
         (niwa-session-durability--disclosed-targets
          (make-hash-table :test #'equal))
         owner
         restored
         session-a
         session-b)
    ;; Exercise the supported container path without provisioning a real
    ;; container: this Docker method executes through a local POSIX shell.
    (setf (alist-get "docker" tramp-methods nil nil #'equal)
          `((tramp-login-program "sh")
            (tramp-login-args (("-i")))
            (tramp-remote-shell "/bin/sh")
            (tramp-remote-shell-args ("-c"))
            (tramp-tmpdir ,temporary-file-directory)
            (tramp-connection-timeout 10)))
    (unwind-protect
        (let* ((workspace-a
                (niwa-workspace-get-or-create
                 'project remote-root remote-root "rebuilt-container"))
               (workspace-identity
                (niwa-workspace-identity-ensure remote-root)))
          (setq session-a
                (niwa-session-create "main" workspace-a remote-root)
                owner (generate-new-buffer " *rebuilt-container-owner*"))
          (let ((target-a (niwa-session-execution-target session-a))
                (niwa-session-durability--client-id client-a))
            (setf (niwa-session-sandbox-mode session-a) 'off)
            (setf (niwa-execution-target-incarnation target-a) incarnation-a)
            (puthash
             (niwa-execution-target-identity target-a) t
             niwa-session-durability--disclosed-targets)
            (with-current-buffer owner
              (org-mode)
              (setq-local niwa--workspace workspace-a)
              (setq-local niwa--session session-a)
              (insert "Published under incarnation A\n")
              ;; Stage a session saved while the original container was
              ;; still live: durable mutation normally fences the target,
              ;; which would acknowledge the replacement before resume.
              (let ((niwa-session-artifacts--checking-incarnation t))
                (niwa-permission-add-session-resource-grant
                 session-a grant-path 'read)
                (niwa-session-artifacts-save session-a owner)))
            (let ((session-id (niwa-session-session-id session-a))
                  (session-dir (niwa-session-save-path session-a)))
              (should
               (equal (list (list :path grant-path :access 'read))
                      (niwa-session-resource-grants session-a)))
              (niwa-session-persistence-lock-release
               session-dir session-a)
              (with-current-buffer owner
                (set-buffer-modified-p nil))
              (kill-buffer owner)
              (setq owner nil)
              (niwa-workspace-clear-registry)
              (let* ((workspace-b
                      (niwa-workspace-get-or-create
                       'project remote-root remote-root "rebuilt-container"))
                     (niwa-session-durability--client-id client-b))
                (should
                 (equal workspace-identity
                        (niwa-workspace-identity-read remote-root)))
                (cl-letf
                    (((symbol-function 'niwa--run-session-start-hooks)
                      #'ignore)
                     ((symbol-function
                       'niwa-agent-persistence-restore-tree)
                      (lambda (&rest _) 0))
                     ((symbol-function
                       'niwa-session-artifacts-load-instructions)
                      #'ignore))
                  (setq restored
                        (niwa-session-persistence-resume-id
                         workspace-b session-id)))
                (should (buffer-live-p restored))
                (with-current-buffer restored
                  (setq session-b niwa--session)
                  (let* ((target-b
                          (niwa-session-execution-target session-b))
                         (incarnation-b
                          (niwa-execution-target-incarnation target-b)))
                    ;; Resume fences the rebuilt container before any
                    ;; request: the old exact grants are revoked and the
                    ;; replacement identity is published by its marker.
                    (should (stringp incarnation-b))
                    (should-not (equal incarnation-a incarnation-b))
                    (should-not
                     (niwa-execution-target-observed-incarnation
                      target-b))
                    (should-not
                     (niwa-execution-target-incarnation-changed-p
                      target-b))
                    (should-not
                     (niwa-session-resource-grants session-b))
                    (should (niwa-request-p
                             (niwa-request-begin session-b)))
                    (should-not
                     (niwa-session-resource-grants session-b))
                    (let* ((text
                            (niwa-session-artifacts-read-artifact
                             session-b "session.meta.el" t))
                           (sidecar
                            (with-temp-buffer
                              (insert text)
                              (goto-char (point-min))
                              (read (current-buffer)))))
                      (should
                       (equal incarnation-b
                              (plist-get sidecar :target-incarnation)))
                      (should-not
                       (plist-get sidecar :resource-grants)))
                    (niwa-request-end t)))))))
      (when (buffer-live-p restored)
        (let ((niwa-session-durability--client-id client-b))
          (with-current-buffer restored
            (niwa-request-end t)
            (set-buffer-modified-p nil))
          (cl-letf (((symbol-function 'niwa--run-session-end-hooks)
                     #'ignore))
            (kill-buffer restored))))
      (when (and session-b
                 (niwa-session-save-path session-b))
        (let ((niwa-session-durability--client-id client-b))
          (ignore-errors
            (niwa-session-persistence-lock-release
             (niwa-session-save-path session-b) session-b))))
      (when (buffer-live-p owner)
        (with-current-buffer owner
          (set-buffer-modified-p nil))
        (kill-buffer owner))
      (when (and session-a
                 (niwa-session-save-path session-a))
        (let ((niwa-session-durability--client-id client-a))
          (ignore-errors
            (niwa-session-persistence-lock-release
             (niwa-session-save-path session-a) session-a))))
      (tramp-cleanup-all-connections)
      (when (file-directory-p local-root)
        (delete-directory local-root t))
      (niwa-workspace-clear-registry))))

(provide 'test-niwa-session-persistence-remote-resume)

;;; test-niwa-session-persistence-remote-resume.el ends here
