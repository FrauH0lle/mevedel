;;; test-mevedel-permission-relative-queue.el --- Relative queued approvals -*- lexical-binding: t -*-

;;; Commentary:
;; Exercise bounded workspace remembering through the real FIFO and prompt UI.

;;; Code:

(require 'mevedel-interaction-prompt)
(require 'mevedel-permission-persistence)
(require 'mevedel-permission-prompt)
(require 'mevedel-permission-queue)
(require 'mevedel-tool-exec-permission)
(require 'mevedel-tool-exec-registration)
(require 'mevedel-view)
(require 'mevedel-workspace)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(mevedel-deftest mevedel-permission-queue/relative-workspace-profile
  (:quiet t :doc "workspace remembering immediately clears a covered sibling without session authority")
  (let* ((root (make-temp-file "mevedel-relative-queue-" t))
         (vault (file-name-concat root "vault"))
         (workspace (mevedel-workspace-get-or-create 'project root root "relative-queue"))
         (session (mevedel-session-create "main" workspace))
         (store (mevedel-permission-persistence-file workspace 'workspace))
         (data (generate-new-buffer " *relative-queue-data*"))
         (view (generate-new-buffer " *relative-queue-view*"))
         (mevedel-permission-rules nil)
         (mevedel-permission-reviewer 'user)
         results)
    (unwind-protect
        (progn
          (make-directory vault)
          (make-directory (file-name-directory store) t)
          (with-temp-file store
            (prin1 '(:rules nil
                     :resource-grants ((:path "./vault" :access read :recursive t)))
                   (current-buffer)))
          (mevedel-tool-exec--register)
          (setf (mevedel-session-permission-mode session) 'ask
                (mevedel-session-sandbox-mode session) 'off)
          (with-current-buffer data
            (org-mode)
            (setq-local default-directory (file-name-as-directory root)
                        mevedel--workspace workspace
                        mevedel--session session))
          (mevedel-view--setup view data)
          (cl-letf (((symbol-function 'mevedel--prompt-block-face) (lambda () 'ask)))
            (with-current-buffer data
              (dotimes (index 2)
                (let ((id index))
                  (mevedel-tool-exec-permission-check-bash-async
                   nil
                   (list :command "make report"
                         :sandbox_permissions "with_additional_permissions"
                         :additional_permissions
                         (list :file_system (list :read (vector vault)))
                         :justification "Read the project's bounded vault")
                   (lambda (value) (push (cons id value) results))))))
            (should-not results)
            (should (= 2 (length (mevedel-session-permission-queue session))))
            (with-current-buffer view
              (let* ((entry (car (mevedel-session-permission-queue session)))
                     (selection (car (plist-get entry :remember-authority-cell)))
                     (grant (car (plist-get selection :file-system)))
                     (id (mevedel-queue--entry-metadata-get entry :interaction-id))
                     (overlay (gethash id mevedel-view--interaction-overlays)))
                (should (plist-get grant :workspace-origin))
                (goto-char (overlay-start overlay))
                (call-interactively (lookup-key (overlay-get overlay 'keymap) "A")))))
          ;; The sibling was already queued when the persistent rule was saved.
          ;; No second answer or explicit cache refresh is allowed here.
          (should (= 2 (length results)))
          (should (eq 'allow (cdr (assq 0 results))))
          (should (eq 'allow (cdr (assq 1 results))))
          (should-not (mevedel-session-permission-queue session))
          (should-not (mevedel-session-permission-rules session))
          (should-not (mevedel-session-resource-grants session))
          (let* ((raw (with-temp-buffer
                        (insert-file-contents store)
                        (read (current-buffer))))
                 (profile (car (plist-get (cdar (plist-get raw :rules)) :file-system))))
            (should (equal "./vault" (plist-get profile :path)))
            (should-not (plist-member profile :workspace-origin))))
      (mevedel-permission-queue-abort-all session)
      (when (buffer-live-p view) (kill-buffer view))
      (when (buffer-live-p data) (kill-buffer data))
      (delete-directory root t)
      (mevedel-workspace-clear-registry))))

(provide 'test-mevedel-permission-relative-queue)
;;; test-mevedel-permission-relative-queue.el ends here
