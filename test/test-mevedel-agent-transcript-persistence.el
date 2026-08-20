;;; test-mevedel-agent-transcript-persistence.el --- Agent transcript tests -*- lexical-binding: t -*-

;;; Commentary:

;; Durable Agent V2 transcript metadata, files, context, and root mailbox.

;;; Code:

(require 'gptel)
(require 'mevedel)
(require 'mevedel-agents)
(require 'mevedel-session-persistence)
(require 'mevedel-structs)
(require 'mevedel-workspace)
(require 'mevedel-workspace-identity)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))


;;
;;; Helpers

(defun test-mevedel-agent-transcript--workspace ()
  "Return a fresh workspace and its temporary root."
  (let* ((root (file-name-as-directory
                (make-temp-file "mevedel-agent-transcript-" t)))
         (name (file-name-nondirectory (directory-file-name root))))
    (mevedel-workspace-clear-registry)
    (mevedel-workspace-identity-ensure root)
    (cons (mevedel-workspace-get-or-create 'project name root name) root)))

(defun test-mevedel-agent-transcript--release (buffer session root)
  "Release SESSION, kill BUFFER, and delete ROOT."
  (when-let* ((save-path (mevedel-session-save-path session)))
    (mevedel-session-persistence-lock-release save-path))
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (set-buffer-modified-p nil)
      (setq-local kill-buffer-hook nil))
    (kill-buffer buffer))
  (when (file-directory-p root)
    (delete-directory root t))
  (mevedel-workspace-clear-registry))


;;
;;; Conversation files

(mevedel-deftest mevedel-session-persistence-shallow-ensure-files ()
  ,test
  (test)
  :doc "creates the locked agent directory before the first root turn"
  (cl-destructuring-bind (workspace . root)
      (test-mevedel-agent-transcript--workspace)
    (let ((session (mevedel-session-create "main" workspace))
          (buffer (generate-new-buffer " *agent-transcript-root*")))
      (unwind-protect
          (with-current-buffer buffer
            (org-mode)
            (let ((save-path
                   (mevedel-session-persistence-shallow-ensure-files
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
                      (mevedel-session-persistence-shallow-ensure-files
                       session buffer)))))
        (test-mevedel-agent-transcript--release buffer session root)))))

(provide 'test-mevedel-agent-transcript-persistence)

;;; test-mevedel-agent-transcript-persistence.el ends here
