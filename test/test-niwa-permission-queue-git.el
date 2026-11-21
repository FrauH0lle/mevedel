;;; test-niwa-permission-queue-git.el -- Confined Git permission tests -*- lexical-binding: t -*-

;;; Commentary:

;; Exercises protected Git metadata through the tool pipeline and prompt.

;;; Code:

(require 'niwa-tool-exec-registration)

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'niwa-execution)
(require 'niwa-permission-prompt)
(require 'niwa-permission-queue)
(require 'niwa-permission-review)
(require 'niwa-pipeline)
(require 'niwa-sandbox)
(require 'niwa-tool-exec)
(require 'niwa-view)

(niwa-deftest niwa-permission-queue--confined-git
  (:quiet t :doc "default and selected metadata trees permit confined staging and commit")
  (let* ((niwa-sandbox--probe-cache nil)
         (availability (niwa-sandbox-probe)))
    (unless (plist-get availability :available)
      (ert-skip (plist-get availability :reason)))
    (let* ((parent (make-temp-file "niwa-pq-git-" t))
           (root (file-name-concat parent "project"))
           (scratch (file-name-concat parent "scratch"))
           (metadata (file-name-concat root ".git"))
           (data-buf (generate-new-buffer " *test-pq-git-data*"))
           (view-buf (generate-new-buffer " *test-pq-git-view*"))
           (workspace (niwa-workspace-get-or-create
                       'project root root "git"))
           (session (niwa-session-create "main" workspace))
           (_register (niwa-tool-exec--register))
           result)
      (unwind-protect
          (progn
            (make-directory root)
            (make-directory scratch)
            (should (zerop (process-file "git" nil nil nil "init" "--quiet" root)))
            (with-temp-file (file-name-concat root "example.txt") (insert "staged contents"))
            (setf (niwa-session-permission-mode session) 'edits
                  (niwa-session-sandbox-mode session) 'required)
            (with-current-buffer data-buf
              (org-mode)
              (setq-local niwa--session session)
              (setq-local temporary-file-directory scratch))
            (niwa-view--setup view-buf data-buf)
            (let ((niwa-permission-rules nil)
                  (niwa-permission-reviewer 'user)
                  (niwa-protected-paths '(("**/.git/**" . read-only))))
              (cl-letf (((symbol-function 'niwa--prompt-block-face) (lambda () 'ask)))
                (dotimes (index 3)
                  (setq result nil)
                  (with-current-buffer data-buf
                    (niwa-pipeline-run-tool
                     (niwa-tool-get "Bash")
                     (lambda (value)
                       (should-not result)
                       (setq result value))
                     (list :description "Probe confined Git authority" :command
                           (if (= index 2)
                               (concat "git -c user.name=Fixture -c user.email=fixture@example.invalid "
                                       "-c commit.gpgsign=false -c core.hooksPath=/dev/null "
                                       "commit -m fixture")
                             "git add -- example.txt")
                           :sandbox_permissions "with_additional_permissions"
                           :additional_permissions
                           (list :file_system (list :write (vector metadata)))
                           :justification "Update Git metadata")))
                  (should (= 1 (length (niwa-session-permission-queue session))))
                  (with-current-buffer view-buf
                    (let* ((entry (car (niwa-session-permission-queue session)))
                           (id (niwa-queue--entry-metadata-get entry :interaction-id))
                           (ov (gethash id niwa-view--interaction-overlays))
                           (choice (format "Write %s (recursive)" metadata)))
                      (goto-char (overlay-start ov))
                      (unless (zerop index)
                        (cl-letf (((symbol-function 'completing-read)
                                   (lambda (_prompt choices &rest _)
                                     (should (assoc choice choices))
                                     choice)))
                          (call-interactively (lookup-key (overlay-get ov 'keymap) "g"))))
                      (setq ov (gethash id niwa-view--interaction-overlays))
                      (goto-char (overlay-start ov))
                      ;; The first card approves its default recursive tree.
                      (call-interactively (lookup-key (overlay-get ov 'keymap) "a"))))
                  (let ((deadline (+ (float-time) 15)))
                    (while (and (not result) (< (float-time) deadline))
                      (accept-process-output nil 0.01)))
                  (should result)
                  (should (string-match-p "sandbox: bubblewrap" result))
                  (should (string-match-p "exit_code=\"0\"" result))
                  (should (file-exists-p (file-name-concat metadata "index")))
                  (should-not (niwa-session-permission-queue session)))))
            (with-temp-buffer
              (should (zerop (process-file "git" nil t nil "-C" root "show" "HEAD:example.txt")))
              (should (equal "staged contents" (buffer-string)))))
        (niwa-permission-queue-abort-all session)
        (niwa-execution-teardown-session session)
        (when (buffer-live-p view-buf) (kill-buffer view-buf))
        (when (buffer-live-p data-buf) (kill-buffer data-buf))
        (delete-directory parent t)
        (niwa-workspace-clear-registry)))))

(niwa-deftest niwa-permission-queue--confined-linked-git
  (:quiet t :doc "linked Git needs shared metadata; explicit trees permit staging and commit")
  (let* ((niwa-sandbox--probe-cache nil)
         (availability (niwa-sandbox-probe)))
    (unless (plist-get availability :available)
      (ert-skip (plist-get availability :reason)))
    (let* ((parent (make-temp-file "niwa-pq-linked-git-" t))
           (main (file-name-concat parent "main"))
           (root (file-name-concat parent "linked"))
           (scratch (file-name-concat parent "scratch"))
           (common (file-name-concat main ".git"))
           (metadata (file-name-concat common "worktrees" "linked"))
           (unrelated (file-name-concat parent "unrelated"))
           (data-buf (generate-new-buffer " *test-pq-linked-git-data*"))
           (view-buf (generate-new-buffer " *test-pq-linked-git-view*"))
           (workspace (niwa-workspace-get-or-create 'project root root "linked"))
           (session (niwa-session-create "main" workspace))
           (_register (niwa-tool-exec--register))
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
            (setf (niwa-session-permission-mode session) 'edits
                  (niwa-session-sandbox-mode session) 'required)
            (with-current-buffer data-buf
              (org-mode)
              (setq-local niwa--session session)
              (setq-local temporary-file-directory scratch))
            (niwa-view--setup view-buf data-buf)
            (let ((niwa-permission-rules nil)
                  (niwa-permission-reviewer 'user)
                  (niwa-protected-paths '(("**/.git/**" . read-only))))
              (cl-letf (((symbol-function 'niwa--prompt-block-face) (lambda () 'ask)))
                (dotimes (index 4)
                  (setq result nil)
                  (with-current-buffer data-buf
                    (niwa-pipeline-run-tool
                     (niwa-tool-get "Bash") (lambda (value) (should-not result) (setq result value))
                     (list :description "Probe confined Git authority" :command
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
                  (should (= 1 (length (niwa-session-permission-queue session))))
                  (with-current-buffer view-buf
                    (let* ((entry (car (niwa-session-permission-queue session)))
                           (id (niwa-queue--entry-metadata-get entry :interaction-id)))
                      (dolist (path (if (zerop index) (list metadata) (list metadata common)))
                        (let ((ov (gethash id niwa-view--interaction-overlays)))
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
                      (let ((ov (gethash id niwa-view--interaction-overlays)))
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
                  (should-not (niwa-session-permission-queue session)))))
            (should-not (file-exists-p (file-name-concat unrelated ".git" "forbidden")))
            (with-temp-buffer
              (should (zerop (process-file "git" nil t nil "-C" root "show" "HEAD:example.txt")))
              (should (equal "linked contents" (buffer-string)))))
        (niwa-permission-queue-abort-all session)
        (niwa-execution-teardown-session session)
        (when (buffer-live-p view-buf) (kill-buffer view-buf))
        (when (buffer-live-p data-buf) (kill-buffer data-buf))
        (delete-directory parent t)
        (niwa-workspace-clear-registry)))))

(niwa-deftest niwa-permission-queue--reviewed-git
  (:quiet t :doc "automatic approval of the prepared metadata tree runs once without a card")
  (let* ((niwa-sandbox--probe-cache nil)
         (availability (niwa-sandbox-probe)))
    (unless (plist-get availability :available)
      (ert-skip (plist-get availability :reason)))
    (let* ((parent (make-temp-file "niwa-pq-reviewed-git-" t))
           (root (file-name-concat parent "project"))
           (vendor (file-name-concat root "vendor"))
           (scratch (file-name-concat parent "scratch"))
           (metadata (file-name-concat root ".git"))
           (forbidden (file-name-concat vendor ".git" "forbidden"))
           (data-buf (generate-new-buffer " *test-pq-reviewed-git-data*"))
           (view-buf (generate-new-buffer " *test-pq-reviewed-git-view*"))
           (workspace (niwa-workspace-get-or-create 'project root root "reviewed-git"))
           (session (niwa-session-create "main" workspace))
           (_register (niwa-tool-exec--register))
           (reviews 0) (results 0) evidence result)
      (unwind-protect
          (progn
            (make-directory root)
            (make-directory scratch)
            (dolist (repository (list root vendor))
              (should (zerop (process-file "git" nil nil nil "init" "--quiet" repository))))
            (with-temp-file (file-name-concat root "example.txt") (insert "reviewed contents"))
            (with-temp-file (file-name-concat root "unrelated.sh")
              (insert "touch " (shell-quote-argument forbidden) " 2>/dev/null || true\n"))
            (setf (niwa-session-permission-mode session) 'edits
                  (niwa-session-sandbox-mode session) 'required
                  (niwa-session-root-buffer session) data-buf)
            (with-current-buffer data-buf
              (org-mode)
              (insert "Stage example.txt in this repository.\n")
              (setq-local niwa--session session)
              (setq-local temporary-file-directory scratch))
            (niwa-view--setup view-buf data-buf)
            (let ((niwa-permission-rules nil)
                  (niwa-permission-reviewer 'auto)
                  (niwa-protected-paths '(("**/.git/**" . read-only))))
              (cl-letf (((symbol-function 'niwa-permission-review--model)
                         (lambda (facts callback &optional _stream)
                           (cl-incf reviews)
                           (setq evidence facts)
                           (funcall callback "{\"decision\":\"allow-once\",\"reason\":\"Requested staging\"}")
                           #'ignore))
                        ((symbol-function 'niwa-permission-queue--render-entry)
                         (lambda (_) (ert-fail "Reviewed approval displayed a card"))))
                (with-current-buffer data-buf
                  (niwa-pipeline-run-tool
                   (niwa-tool-get "Bash")
                   (lambda (value) (cl-incf results) (setq result value))
                   (list :command "git add -- example.txt; sh ./unrelated.sh"
                         :description "Probe scoped Git authority"
                         :sandbox_permissions "with_additional_permissions"
                         :additional_permissions
                         (list :file_system (list :write (vector metadata)))
                         :justification "Stage the requested file")))
                (let ((deadline (+ (float-time) 15)))
                  (while (and (not result) (< (float-time) deadline))
                    (accept-process-output nil 0.01)))))
            (should (= 1 reviews))
            (should (= 1 results))
            (let ((selected (car (plist-get evidence :selected-resources))))
              (should (equal metadata (plist-get selected :path)))
              (should (plist-get selected :recursive)))
            (should (string-match-p "sandbox: bubblewrap" result))
            (should (string-match-p "exit_code=\"0\"" result))
            (should-not (file-exists-p forbidden))
            (should-not (niwa-session-permission-queue session))
            (should-not (niwa-session-resource-grants session))
            (with-temp-buffer
              (should (zerop (process-file "git" nil t nil "-C" root "diff" "--cached" "--name-only")))
              (should (equal "example.txt\n" (buffer-string)))))
        (niwa-permission-queue-abort-all session)
        (niwa-execution-teardown-session session)
        (when (buffer-live-p view-buf) (kill-buffer view-buf))
        (when (buffer-live-p data-buf) (kill-buffer data-buf))
        (delete-directory parent t)
        (niwa-workspace-clear-registry)))))

(provide 'test-niwa-permission-queue-git)
;;; test-niwa-permission-queue-git.el ends here
