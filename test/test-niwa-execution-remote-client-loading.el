;;; test-niwa-execution-remote-client-loading.el --- Cold client tests -*- lexical-binding: t -*-

;;; Commentary:

;; Exercise the independent acceptance client's restore fixture in a fresh
;; Emacs.  An umbrella-preloaded test process hides autoloads replacing stubs.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(niwa-deftest test-niwa-execution-remote-client--session ()
  ,test
  (test)
  :doc "cold restore keeps the inert initializer and permits a control request"
  (let* ((root (file-name-as-directory
                (make-temp-file "niwa-client-loading-" t)))
         (fixture (file-name-concat
                   (file-name-directory (locate-library "niwa"))
                   "test" "test-niwa-execution-remote-client.el"))
         (form
          `(progn
             (setq load-path ',load-path)
             (load ,fixture nil t)
             (niwa-test--with-captured-diagnostics nil
               (let* ((root ,root)
                      (workspace
                       (niwa-workspace-get-or-create
                        'project root root "cold-client"))
                      (session (niwa-session-create "cold-client" workspace root))
                      (buffer (generate-new-buffer " *cold-client-owner*"))
                      (owner-id (make-string 64 ?a))
                      (requester-id (make-string 64 ?b))
                      (niwa-session-durability--client-id owner-id)
                      ;; The selected Podman journey persisted these bytes
                      ;; before chat setup installed its save-time sanitizer.
                      (transcript
                       ":PROPERTIES:\n:GPTEL_MODEL: fixture-model\n:END:\nTranscript\n")
                      requester)
                 (unwind-protect
                     (progn
                       (with-current-buffer buffer
                         (org-mode)
                         (setq-local niwa--session session
                                     niwa--workspace workspace)
                         (niwa-session-set-root-buffer session buffer)
                         (insert "Transcript\n")
                         (should (niwa-session-artifacts-save session buffer t))
                         ;; Publish the exact input independently of whichever
                         ;; save advice the client fixture has loaded.
                         (niwa-session-publication-publish
                          session
                          (list
                           (list :path (niwa-session-artifacts-segment-path
                                        (niwa-session-save-path session) 1)
                                 :content transcript)
                           (list :path (niwa-session-artifacts-sidecar-path
                                        (niwa-session-save-path session))
                                 :content (niwa-session-artifacts-printed-value
                                           (niwa-session-artifacts-build-sidecar
                                            session buffer))
                                 :commit-marker t)))
                         (setq-local kill-buffer-hook nil)
                         (set-buffer-modified-p nil))
                       (kill-buffer buffer)
                       (pcase-let ((`(,restored . ,joined)
                                    (test-niwa-execution-remote-client--session
                                     root (niwa-session-session-id session)
                                     requester-id)))
                         (setq requester restored)
                         (let ((niwa-session-durability--client-id requester-id))
                           (should (niwa-session-control-transfer-request joined))
                           ;; Reach the original modified-buffer guard, not just
                           ;; an internal load-order or initialization flag.
                           (should-not
                            (niwa-session-control-transfer-poll
                             joined restored t)))
                         (with-current-buffer restored
                           (should buffer-read-only)
                           (should-not (buffer-modified-p))
                           (should (equal transcript (buffer-string)))
                           (let ((inhibit-read-only t))
                             (goto-char (point-max))
                             (insert "Unsaved local edit\n")))
                         (let ((niwa-session-durability--client-id requester-id))
                           (should
                            (equal
                             "Read-only session changed locally; refresh before transfer"
                             (error-message-string
                              (should-error
                               (niwa-session-control-transfer-poll
                                joined restored t)
                               :type 'user-error)))))))
                   (dolist (candidate (list buffer requester))
                     (when (buffer-live-p candidate)
                       (with-current-buffer candidate
                         (setq-local kill-buffer-hook nil)
                         (set-buffer-modified-p nil))
                       (kill-buffer candidate)))
                   (niwa-session-durability-lease-release
                    (niwa-session-save-path session) session)))))))
    (unwind-protect
        (with-temp-buffer
          (let ((process
                 (make-process
                  :name "niwa-cold-transfer-client"
                  :buffer (current-buffer) :noquery t
                  :connection-type 'pipe :sentinel #'ignore
                  :command
                  (list (expand-file-name invocation-name invocation-directory)
                        "--batch" "-Q" "--eval" (prin1-to-string form)))))
            (unwind-protect
                (progn
                  (with-timeout (30 (ert-fail "Cold transfer client timed out"))
                    (while (process-live-p process)
                      (accept-process-output process 0.05)))
                  (accept-process-output process 0)
                  (ert-info ((buffer-string))
                    (should (zerop (process-exit-status process)))
                    (should (equal "" (buffer-string)))))
              (when (process-live-p process)
                (delete-process process)))))
      (delete-directory root t))))

(provide 'test-niwa-execution-remote-client-loading)
;;; test-niwa-execution-remote-client-loading.el ends here
