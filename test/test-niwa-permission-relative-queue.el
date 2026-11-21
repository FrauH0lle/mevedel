;;; test-niwa-permission-relative-queue.el --- Relative queued approvals -*- lexical-binding: t -*-

;;; Commentary:
;; Exercise bounded workspace remembering through the real FIFO and prompt UI.

;;; Code:

(require 'niwa-interaction-prompt)
(require 'niwa-permission-persistence)
(require 'niwa-permission-prompt)
(require 'niwa-permission-queue)
(require 'niwa-tool-exec-permission)
(require 'niwa-tool-exec-registration)
(require 'niwa-view)
(require 'niwa-workspace)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(niwa-deftest niwa-permission-queue/relative-workspace-profile
  (:quiet t :doc "workspace remembering immediately clears a covered sibling without session authority")
  (let* ((root (make-temp-file "niwa-relative-queue-" t))
         (vault (file-name-concat root "vault"))
         (workspace (niwa-workspace-get-or-create 'project root root "relative-queue"))
         (session (niwa-session-create "main" workspace))
         (store (niwa-permission-persistence-file workspace 'workspace))
         (data (generate-new-buffer " *relative-queue-data*"))
         (view (generate-new-buffer " *relative-queue-view*"))
         (niwa-permission-rules nil)
         (niwa-permission-reviewer 'user)
         results)
    (unwind-protect
        (progn
          (make-directory vault)
          (make-directory (file-name-directory store) t)
          (with-temp-file store
            (prin1 '(:rules nil
                     :resource-grants ((:path "./vault" :access read :recursive t)))
                   (current-buffer)))
          (niwa-tool-exec--register)
          (setf (niwa-session-permission-mode session) 'ask
                (niwa-session-sandbox-mode session) 'off)
          (with-current-buffer data
            (org-mode)
            (setq-local default-directory (file-name-as-directory root)
                        niwa--workspace workspace
                        niwa--session session))
          (niwa-view--setup view data)
          (cl-letf (((symbol-function 'niwa--prompt-block-face) (lambda () 'ask)))
            (with-current-buffer data
              (dotimes (index 2)
                (let ((id index))
                  (niwa-tool-exec-permission-check-bash-async
                   nil
                   (list :command "make report"
                         :sandbox_permissions "with_additional_permissions"
                         :additional_permissions
                         (list :file_system (list :read (vector vault)))
                         :justification "Read the project's bounded vault")
                   (lambda (value) (push (cons id value) results))))))
            (should-not results)
            (should (= 2 (length (niwa-session-permission-queue session))))
            (with-current-buffer view
              (let* ((entry (car (niwa-session-permission-queue session)))
                     (selection (car (plist-get entry :remember-authority-cell)))
                     (grant (car (plist-get selection :file-system)))
                     (id (niwa-queue--entry-metadata-get entry :interaction-id))
                     (overlay (gethash id niwa-view--interaction-overlays)))
                (should (plist-get grant :workspace-origin))
                (goto-char (overlay-start overlay))
                (call-interactively (lookup-key (overlay-get overlay 'keymap) "A")))))
          ;; The sibling was already queued when the persistent rule was saved.
          ;; No second answer or explicit cache refresh is allowed here.
          (should (= 2 (length results)))
          (should (eq 'allow (cdr (assq 0 results))))
          (should (eq 'allow (cdr (assq 1 results))))
          (should-not (niwa-session-permission-queue session))
          (should-not (niwa-session-permission-rules session))
          (should-not (niwa-session-resource-grants session))
          (let* ((raw (with-temp-buffer
                        (insert-file-contents store)
                        (read (current-buffer))))
                 (profile (car (plist-get (cdar (plist-get raw :rules)) :file-system))))
            (should (equal "./vault" (plist-get profile :path)))
            (should-not (plist-member profile :workspace-origin))))
      (niwa-permission-queue-abort-all session)
      (when (buffer-live-p view) (kill-buffer view))
      (when (buffer-live-p data) (kill-buffer data))
      (delete-directory root t)
      (niwa-workspace-clear-registry))))

(provide 'test-niwa-permission-relative-queue)
;;; test-niwa-permission-relative-queue.el ends here
