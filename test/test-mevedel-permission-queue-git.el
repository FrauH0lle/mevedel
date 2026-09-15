;;; test-mevedel-permission-queue-git.el -- Confined Git permission tests -*- lexical-binding: t -*-

;;; Commentary:

;; Exercises protected Git metadata through the tool pipeline and prompt.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-execution)
(require 'mevedel-permission-prompt)
(require 'mevedel-permission-queue)
(require 'mevedel-pipeline)
(require 'mevedel-sandbox)
(require 'mevedel-tool-exec)
(require 'mevedel-view)

(mevedel-deftest mevedel-permission-queue--confined-git
  (:quiet t :doc "exact Git grant fails; selected metadata tree permits confined staging and commit")
  (let* ((mevedel-sandbox--probe-cache nil)
         (availability (mevedel-sandbox-probe)))
    (unless (plist-get availability :available)
      (ert-skip (plist-get availability :reason)))
    (let* ((parent (make-temp-file "mevedel-pq-git-" t))
           (root (file-name-concat parent "project"))
           (scratch (file-name-concat parent "scratch"))
           (metadata (file-name-concat root ".git"))
           (data-buf (generate-new-buffer " *test-pq-git-data*"))
           (view-buf (generate-new-buffer " *test-pq-git-view*"))
           (workspace (mevedel-workspace-get-or-create
                       'project root root "git"))
           (session (mevedel-session-create "main" workspace))
           (_register (mevedel-tool-exec--register))
           result)
      (unwind-protect
          (progn
            (make-directory root)
            (make-directory scratch)
            (should (zerop (process-file "git" nil nil nil "init" "--quiet" root)))
            (with-temp-file (file-name-concat root "example.txt") (insert "staged contents"))
            (setf (mevedel-session-permission-mode session) 'edits
                  (mevedel-session-sandbox-mode session) 'required)
            (with-current-buffer data-buf
              (org-mode)
              (setq-local mevedel--session session)
              (setq-local temporary-file-directory scratch))
            (mevedel-view--setup view-buf data-buf)
            (let ((mevedel-permission-rules nil)
                  (mevedel-permission-guardian nil)
                  (mevedel-protected-paths '(("**/.git/**" . read-only))))
              (cl-letf (((symbol-function 'mevedel--prompt-block-face) (lambda () 'ask)))
                (dotimes (index 3)
                  (setq result nil)
                  (with-current-buffer data-buf
                    (mevedel-pipeline-run-tool
                     (mevedel-tool-get "Bash")
                     (lambda (value)
                       (should-not result)
                       (setq result value))
                     (list :command
                           (if (= index 2)
                               (concat "git -c user.name=Fixture -c user.email=fixture@example.invalid "
                                       "-c commit.gpgsign=false -c core.hooksPath=/dev/null "
                                       "commit -m fixture")
                             "git add -- example.txt")
                           :sandbox_permissions "with_additional_permissions"
                           :additional_permissions
                           (list :file_system (list :write (vector metadata)))
                           :justification "Update Git metadata")))
                  (should (= 1 (length (mevedel-session-permission-queue session))))
                  (with-current-buffer view-buf
                    (let* ((entry (car (mevedel-session-permission-queue session)))
                           (id (mevedel-queue--entry-metadata-get entry :interaction-id))
                           (ov (gethash id mevedel-view--interaction-overlays))
                           (choice (format "Write %s (recursive)" metadata)))
                      (goto-char (overlay-start ov))
                      (unless (zerop index)
                        (cl-letf (((symbol-function 'completing-read)
                                   (lambda (_prompt choices &rest _)
                                     (should (assoc choice choices))
                                     choice)))
                          (call-interactively (lookup-key (overlay-get ov 'keymap) "g"))))
                      (setq ov (gethash id mevedel-view--interaction-overlays))
                      (goto-char (overlay-start ov))
                      (call-interactively (lookup-key (overlay-get ov 'keymap) "a"))))
                  (let ((deadline (+ (float-time) 15)))
                    (while (and (not result) (< (float-time) deadline))
                      (accept-process-output nil 0.01)))
                  (should result)
                  (if (zerop index)
                      (progn
                        (should (string-match-p "Exact directory access cannot be confined" result))
                        (should (string-match-p "sandbox: refused" result))
                        (should-not (string-match-p "filesystem: unrestricted" result))
                        (should-not (file-exists-p (file-name-concat metadata "index"))))
                    (should (string-match-p "sandbox: bubblewrap" result))
                    (should (string-match-p "exit_code=\"0\"" result))
                    (should (file-exists-p (file-name-concat metadata "index"))))
                  (should-not (mevedel-session-permission-queue session)))))
            (with-temp-buffer
              (should (zerop (process-file "git" nil t nil "-C" root "show" "HEAD:example.txt")))
              (should (equal "staged contents" (buffer-string)))))
        (mevedel-permission-queue-abort-all session)
        (mevedel-execution-teardown-session session)
        (when (buffer-live-p view-buf) (kill-buffer view-buf))
        (when (buffer-live-p data-buf) (kill-buffer data-buf))
        (delete-directory parent t)
        (mevedel-workspace-clear-registry)))))

(mevedel-deftest mevedel-permission-queue--confined-linked-git
  (:quiet t :doc "linked Git needs shared metadata; explicit trees permit staging and commit")
  (let* ((mevedel-sandbox--probe-cache nil)
         (availability (mevedel-sandbox-probe)))
    (unless (plist-get availability :available)
      (ert-skip (plist-get availability :reason)))
    (let* ((parent (make-temp-file "mevedel-pq-linked-git-" t))
           (main (file-name-concat parent "main"))
           (root (file-name-concat parent "linked"))
           (scratch (file-name-concat parent "scratch"))
           (common (file-name-concat main ".git"))
           (metadata (file-name-concat common "worktrees" "linked"))
           (unrelated (file-name-concat parent "unrelated"))
           (data-buf (generate-new-buffer " *test-pq-linked-git-data*"))
           (view-buf (generate-new-buffer " *test-pq-linked-git-view*"))
           (workspace (mevedel-workspace-get-or-create 'project root root "linked"))
           (session (mevedel-session-create "main" workspace))
           (_register (mevedel-tool-exec--register))
           result)
      (unwind-protect
          (progn
            (make-directory scratch)
            (dolist (repository (list main unrelated))
              (should (zerop (process-file "git" nil nil nil "init" "--quiet" repository))))
            (should (zerop
                     (process-file "git" nil nil nil "-C" main
                                   "-c" "user.name=Fixture" "-c" "user.email=fixture@example.invalid"
                                   "-c" "commit.gpgsign=false" "-c" "core.hooksPath=/dev/null"
                                   "commit" "--quiet" "--allow-empty" "-m" "initial")))
            (should (zerop (process-file "git" nil nil nil "-C" main
                                         "worktree" "add" "--quiet" "-b" "linked" root)))
            (should (file-directory-p metadata))
            (with-temp-file (file-name-concat root "example.txt") (insert "linked contents"))
            (with-temp-file (file-name-concat root "unrelated.sh")
              (insert "touch " (shell-quote-argument
                                 (file-name-concat unrelated ".git" "forbidden")) "\n"))
            (setf (mevedel-session-permission-mode session) 'edits
                  (mevedel-session-sandbox-mode session) 'required)
            (with-current-buffer data-buf
              (org-mode)
              (setq-local mevedel--session session)
              (setq-local temporary-file-directory scratch))
            (mevedel-view--setup view-buf data-buf)
            (let ((mevedel-permission-rules nil)
                  (mevedel-permission-guardian nil)
                  (mevedel-protected-paths '(("**/.git/**" . read-only))))
              (cl-letf (((symbol-function 'mevedel--prompt-block-face) (lambda () 'ask)))
                (dotimes (index 4)
                  (setq result nil)
                  (with-current-buffer data-buf
                    (mevedel-pipeline-run-tool
                     (mevedel-tool-get "Bash") (lambda (value) (should-not result) (setq result value))
                     (list :command
                           (pcase index
                             (2 (concat "git -c user.name=Fixture -c user.email=fixture@example.invalid "
                                        "-c commit.gpgsign=false -c core.hooksPath=/dev/null "
                                        "commit -m fixture"))
                             (3 "sh ./unrelated.sh")
                             (_ "git add -- example.txt"))
                           :sandbox_permissions "with_additional_permissions"
                           :additional_permissions
                           (list :file_system
                                 (list :write (if (zerop index) (vector metadata)
                                                (vector metadata common))))
                           :justification "Update linked Git metadata")))
                  (should (= 1 (length (mevedel-session-permission-queue session))))
                  (with-current-buffer view-buf
                    (let* ((entry (car (mevedel-session-permission-queue session)))
                           (id (mevedel-queue--entry-metadata-get entry :interaction-id)))
                      (dolist (path (if (zerop index) (list metadata) (list metadata common)))
                        (let ((ov (gethash id mevedel-view--interaction-overlays)))
                          (goto-char (overlay-start ov))
                          (cl-letf (((symbol-function 'completing-read)
                                     (lambda (_prompt choices &rest _)
                                       (let ((scope (format "Write %s (recursive)" path))
                                             (resource (format "Write %s (exact)" path)))
                                         (cond
                                          ((assoc scope choices) scope)
                                          ((assoc resource choices) resource)
                                          (t (ert-fail (list "Missing resource choice" path choices))))))))
                            (call-interactively (lookup-key (overlay-get ov 'keymap) "g")))))
                      (let ((ov (gethash id mevedel-view--interaction-overlays)))
                        (goto-char (overlay-start ov))
                        (call-interactively (lookup-key (overlay-get ov 'keymap) "a")))))
                  (let ((deadline (+ (float-time) 15)))
                    (while (and (not result) (< (float-time) deadline))
                      (accept-process-output nil 0.01)))
                  (should result)
                  (should (string-match-p "sandbox: bubblewrap" result))
                  (if (memq index '(1 2))
                      (should (string-match-p "exit_code=\"0\"" result))
                    (should-not (string-match-p "exit_code=\"0\"" result)))
                  (when (zerop index)
                    (should (string-match-p (regexp-quote common) result))
                    (should (string-match-p "Linked Git shared metadata" result)))
                  (should-not (mevedel-session-permission-queue session)))))
            (should-not (file-exists-p (file-name-concat unrelated ".git" "forbidden")))
            (with-temp-buffer
              (should (zerop (process-file "git" nil t nil "-C" root "show" "HEAD:example.txt")))
              (should (equal "linked contents" (buffer-string)))))
        (mevedel-permission-queue-abort-all session)
        (mevedel-execution-teardown-session session)
        (when (buffer-live-p view-buf) (kill-buffer view-buf))
        (when (buffer-live-p data-buf) (kill-buffer data-buf))
        (delete-directory parent t)
        (mevedel-workspace-clear-registry)))))

(provide 'test-mevedel-permission-queue-git)
;;; test-mevedel-permission-queue-git.el ends here
