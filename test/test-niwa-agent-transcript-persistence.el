;;; test-niwa-agent-transcript-persistence.el --- Agent transcript tests -*- lexical-binding: t -*-

;;; Commentary:

;; Durable Agent V2 transcript metadata, files, context, and root mailbox.

;;; Code:

(require 'gptel)
(require 'niwa)
(require 'niwa-agents)
(require 'niwa-session-persistence)
(require 'niwa-structs)
(require 'niwa-workspace)
(require 'niwa-workspace-identity)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))


;;
;;; Helpers

(defun test-niwa-agent-transcript--workspace ()
  "Return a fresh workspace and its temporary root."
  (let* ((root (file-name-as-directory
                (make-temp-file "niwa-agent-transcript-" t)))
         (name (file-name-nondirectory (directory-file-name root))))
    (niwa-workspace-clear-registry)
    (niwa-workspace-identity-ensure root)
    (cons (niwa-workspace-get-or-create 'project name root name) root)))

(defun test-niwa-agent-transcript--release (buffer session root)
  "Release SESSION, kill BUFFER, and delete ROOT."
  (when-let* ((save-path (niwa-session-save-path session)))
    (niwa-session-persistence-lock-release save-path))
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (set-buffer-modified-p nil)
      (setq-local kill-buffer-hook nil))
    (kill-buffer buffer))
  (when (file-directory-p root)
    (delete-directory root t))
  (niwa-workspace-clear-registry))


;;
;;; Conversation files

(niwa-deftest niwa-session-persistence-shallow-ensure-files ()
  ,test
  (test)
  :doc "creates the locked agent directory before the first root turn"
  (cl-destructuring-bind (workspace . root)
      (test-niwa-agent-transcript--workspace)
    (let ((session (niwa-session-create "main" workspace))
          (buffer (generate-new-buffer " *agent-transcript-root*")))
      (unwind-protect
          (with-current-buffer buffer
            (org-mode)
            (let ((save-path
                   (niwa-session-persistence-shallow-ensure-files
                    session buffer)))
              (should (file-directory-p
                       (file-name-concat save-path "agents")))
              ;; A project workspace is portable: it takes a renewable
              ;; lease instead of the file-workspace PID lock.
              (should (file-directory-p
                       (file-name-concat save-path ".lease")))
              (should-not (file-exists-p
                           (file-name-concat save-path ".lock")))
              (should-not (file-exists-p
                           (file-name-concat save-path "session.meta.el")))
              (should
               (equal save-path
                      (niwa-session-persistence-shallow-ensure-files
                       session buffer)))))
        (test-niwa-agent-transcript--release buffer session root)))))

(provide 'test-niwa-agent-transcript-persistence)

;;; test-niwa-agent-transcript-persistence.el ends here
