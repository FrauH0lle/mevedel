;;; test-mevedel-session-persistence-remote-resume.el --- Remote resume tests -*- lexical-binding: t -*-

;;; Commentary:

;; Focused acceptance coverage for resuming a rebuilt execution target.

;;; Code:

(require 'mevedel)
(require 'mevedel-execution-target)
(require 'mevedel-permissions)
(require 'mevedel-session-durability)
(require 'mevedel-session-persistence)
(require 'mevedel-workspace-identity)
(require 'tramp)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

;; `tramp'
(declare-function tramp-cleanup-all-connections "tramp")
(defvar tramp-local-host-regexp)
(defvar tramp-methods)

(mevedel-deftest mevedel-session-persistence-resume-rebuilt-target ()
  ,test
  (test)
  :doc "resumes a rebuilt container and durably revokes its old exact grants"
  (let* ((host "mevedel-rebuilt-container")
         (local-root
          (file-name-as-directory
           (make-temp-file "mevedel-rebuilt-container-" t)))
         (remote-root (format "/docker:%s:%s" host local-root))
         (grant-path (file-name-concat remote-root "granted.el"))
         (incarnation-a (secure-hash 'sha256 "incarnation-a"))
         (client-a (make-string 64 ?a))
         (client-b (make-string 64 ?b))
         (tramp-methods (copy-tree tramp-methods))
         (tramp-local-host-regexp
          (concat "\\`" (regexp-quote host) "\\'"))
         (mevedel-session-durability--disclosed-targets
          (make-hash-table :test #'equal))
         owner
         restored
         session-a
         session-b)
    ;; Exercise the supported container path without provisioning a real
    ;; container: this Docker method executes through a local POSIX shell.
    (setf (alist-get "docker" tramp-methods nil nil #'equal)
          '((tramp-login-program "sh")
            (tramp-login-args (("-i")))
            (tramp-remote-shell "/bin/sh")
            (tramp-remote-shell-args ("-c"))
            (tramp-connection-timeout 10)))
    (unwind-protect
        (let* ((workspace-a
                (mevedel-workspace-get-or-create
                 'project remote-root remote-root "rebuilt-container"))
               (workspace-identity
                (mevedel-workspace-identity-ensure remote-root)))
          (setq session-a
                (mevedel-session-create "main" workspace-a remote-root)
                owner (generate-new-buffer " *rebuilt-container-owner*"))
          (let ((target-a (mevedel-session-execution-target session-a))
                (mevedel-session-durability--client-id client-a))
            (setf (mevedel-session-sandbox-mode session-a) 'off)
            (mevedel-execution-target-seed-incarnation
             target-a incarnation-a)
            (puthash
             (mevedel-execution-target-identity target-a) t
             mevedel-session-durability--disclosed-targets)
            (with-current-buffer owner
              (org-mode)
              (setq-local mevedel--workspace workspace-a)
              (setq-local mevedel--session session-a)
              (insert "Published under incarnation A\n")
              (mevedel-permission-add-session-resource-grant
               session-a grant-path 'read)
              (mevedel-session-persistence-save session-a owner))
            (let ((session-id (mevedel-session-session-id session-a))
                  (session-dir (mevedel-session-save-path session-a)))
              (should
               (equal (list (list :path grant-path :access 'read))
                      (mevedel-session-resource-grants session-a)))
              (mevedel-session-persistence-lock-release
               session-dir session-a)
              (with-current-buffer owner
                (set-buffer-modified-p nil))
              (kill-buffer owner)
              (setq owner nil)
              (mevedel-workspace-clear-registry)
              (let* ((workspace-b
                      (mevedel-workspace-get-or-create
                       'project remote-root remote-root "rebuilt-container"))
                     (mevedel-session-durability--client-id client-b))
                (should
                 (equal workspace-identity
                        (mevedel-workspace-identity-read remote-root)))
                (cl-letf
                    (((symbol-function 'mevedel--run-session-start-hooks)
                      #'ignore)
                     ((symbol-function
                       'mevedel-agent-persistence-restore-tree)
                      (lambda (&rest _) 0))
                     ((symbol-function
                       'mevedel-session-persistence--load-instructions)
                      #'ignore))
                  (setq restored
                        (mevedel-session-persistence-resume-id
                         workspace-b session-id)))
                (should (buffer-live-p restored))
                (with-current-buffer restored
                  (setq session-b mevedel--session)
                  (let* ((target-b
                          (mevedel-session-execution-target session-b))
                         (incarnation-b
                          (mevedel-execution-target-observed-incarnation
                           target-b)))
                    (should
                     (mevedel-execution-target-incarnation-changed-p
                      target-b))
                    (should
                     (equal incarnation-a
                            (mevedel-execution-target-incarnation target-b)))
                    (should (stringp incarnation-b))
                    (should-not (equal incarnation-a incarnation-b))
                    (should
                     (equal (list (list :path grant-path :access 'read))
                            (mevedel-session-resource-grants session-b)))
                    (should (mevedel-request-p
                             (mevedel-request-begin session-b)))
                    (should-not
                     (mevedel-session-resource-grants session-b))
                    (should
                     (equal incarnation-b
                            (mevedel-execution-target-incarnation target-b)))
                    (should-not
                     (mevedel-execution-target-incarnation-changed-p
                      target-b))
                    (let* ((text
                            (mevedel-session-persistence-read-artifact
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
                    (mevedel-request-end t)))))))
      (when (buffer-live-p restored)
        (let ((mevedel-session-durability--client-id client-b))
          (with-current-buffer restored
            (mevedel-request-end t)
            (set-buffer-modified-p nil))
          (cl-letf (((symbol-function 'mevedel--run-session-end-hooks)
                     #'ignore))
            (kill-buffer restored))))
      (when (and session-b
                 (mevedel-session-save-path session-b))
        (let ((mevedel-session-durability--client-id client-b))
          (ignore-errors
            (mevedel-session-persistence-lock-release
             (mevedel-session-save-path session-b) session-b))))
      (when (buffer-live-p owner)
        (with-current-buffer owner
          (set-buffer-modified-p nil))
        (kill-buffer owner))
      (when (and session-a
                 (mevedel-session-save-path session-a))
        (let ((mevedel-session-durability--client-id client-a))
          (ignore-errors
            (mevedel-session-persistence-lock-release
             (mevedel-session-save-path session-a) session-a))))
      (tramp-cleanup-all-connections)
      (when (file-directory-p local-root)
        (delete-directory local-root t))
      (mevedel-workspace-clear-registry))))

(provide 'test-mevedel-session-persistence-remote-resume)

;;; test-mevedel-session-persistence-remote-resume.el ends here
