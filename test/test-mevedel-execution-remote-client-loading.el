;;; test-mevedel-execution-remote-client-loading.el --- Cold client tests -*- lexical-binding: t -*-

;;; Commentary:

;; Exercise the independent acceptance client's restore fixture in a fresh
;; Emacs.  An umbrella-preloaded test process hides autoloads replacing stubs.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(mevedel-deftest test-mevedel-execution-remote-client--session ()
  ,test
  (test)
  :doc "cold restore keeps the inert initializer and permits a control request"
  (let* ((root (file-name-as-directory
                (make-temp-file "mevedel-client-loading-" t)))
         (fixture (file-name-concat
                   (file-name-directory (locate-library "mevedel"))
                   "test" "test-mevedel-execution-remote-client.el"))
         (form
          `(progn
             (setq load-path ',load-path)
             (load ,fixture nil t)
             (mevedel-test--with-captured-diagnostics nil
               (let* ((root ,root)
                      (workspace
                       (mevedel-workspace-get-or-create
                        'project root root "cold-client"))
                      (session (mevedel-session-create "cold-client" workspace root))
                      (buffer (generate-new-buffer " *cold-client-owner*"))
                      (owner-id (make-string 64 ?a))
                      (requester-id (make-string 64 ?b))
                      (mevedel-session-durability--client-id owner-id)
                      ;; The selected Podman journey persisted these bytes
                      ;; before chat setup installed its save-time sanitizer.
                      (transcript
                       ":PROPERTIES:\n:GPTEL_MODEL: fixture-model\n:END:\nTranscript\n")
                      requester)
                 (unwind-protect
                     (progn
                       (with-current-buffer buffer
                         (org-mode)
                         (setq-local mevedel--session session
                                     mevedel--workspace workspace)
                         (mevedel-session-set-root-buffer session buffer)
                         (insert "Transcript\n")
                         (should (mevedel-session-artifacts-save session buffer t))
                         ;; Publish the exact input independently of whichever
                         ;; save advice the client fixture has loaded.
                         (mevedel-session-publication-publish
                          session
                          (list
                           (list :path (mevedel-session-artifacts-segment-path
                                        (mevedel-session-save-path session) 1)
                                 :content transcript)
                           (list :path (mevedel-session-artifacts-sidecar-path
                                        (mevedel-session-save-path session))
                                 :content (mevedel-session-artifacts-printed-value
                                           (mevedel-session-artifacts-build-sidecar
                                            session buffer))
                                 :commit-marker t)))
                         (setq-local kill-buffer-hook nil)
                         (set-buffer-modified-p nil))
                       (kill-buffer buffer)
                       (pcase-let ((`(,restored . ,joined)
                                    (test-mevedel-execution-remote-client--session
                                     root (mevedel-session-session-id session)
                                     requester-id)))
                         (setq requester restored)
                         (let ((mevedel-session-durability--client-id requester-id))
                           (should (mevedel-session-control-transfer-request joined))
                           ;; Reach the original modified-buffer guard, not just
                           ;; an internal load-order or initialization flag.
                           (should-not
                            (mevedel-session-control-transfer-poll
                             joined restored t)))
                         (with-current-buffer restored
                           (should buffer-read-only)
                           (should-not (buffer-modified-p))
                           (should (equal transcript (buffer-string)))
                           (let ((inhibit-read-only t))
                             (goto-char (point-max))
                             (insert "Unsaved local edit\n")))
                         (let ((mevedel-session-durability--client-id requester-id))
                           (should
                            (equal
                             "Read-only session changed locally; refresh before transfer"
                             (error-message-string
                              (should-error
                               (mevedel-session-control-transfer-poll
                                joined restored t)
                               :type 'user-error)))))))
                   (dolist (candidate (list buffer requester))
                     (when (buffer-live-p candidate)
                       (with-current-buffer candidate
                         (setq-local kill-buffer-hook nil)
                         (set-buffer-modified-p nil))
                       (kill-buffer candidate)))
                   (mevedel-session-durability-lease-release
                    (mevedel-session-save-path session) session)))))))
    (unwind-protect
        (with-temp-buffer
          (let ((process
                 (make-process
                  :name "mevedel-cold-transfer-client"
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

(provide 'test-mevedel-execution-remote-client-loading)
;;; test-mevedel-execution-remote-client-loading.el ends here
