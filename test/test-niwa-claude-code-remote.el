;;; test-niwa-claude-code-remote.el --- Native tools on real targets -*- lexical-binding: t -*-

;;; Commentary:
;; A local ACP peer invokes the real MCP bridge against provisioned targets.

;;; Code:

(require 'niwa-engine-test-support
         (file-name-concat (file-name-directory (or load-file-name buffer-file-name))
                           "niwa-engine-test-support"))
(require 'niwa-execution-test-helpers
         (file-name-concat (file-name-directory (or load-file-name buffer-file-name))
                           "niwa-execution-test-helpers"))
(require 'niwa-claude-code-session)
(require 'tramp-container)
(require 'tramp-sh)

(niwa-deftest niwa-claude-code/remote
  (:quiet t :after-each ((tramp-cleanup-all-connections)))
  (pcase-dolist (`(,variable ,method) '(("NIWA_TEST_SSH_ROOT" ssh)
                                       ("NIWA_TEST_PODMAN_ROOT" podman)))
    (let* ((tramp-use-connection-share tramp-use-connection-share)
           (tramp-ssh-controlmaster-options tramp-ssh-controlmaster-options)
           (niwa-session-durability--disclosed-targets
            (copy-hash-table niwa-session-durability--disclosed-targets))
           (base (test-niwa-execution-remote--real-root variable method))
           (root (test-niwa-execution-remote--real-temp-directory base "niwa-native-"))
           (local (make-temp-file "niwa-native-host-" t))
           (workspace (test-niwa-execution--workspace root))
           (session (niwa-session-create "native" workspace root))
           (buffer (generate-new-buffer " *native-remote*"))
           (gptel--known-backends nil)
           (niwa-claude-code-directory (file-name-concat local "claude")))
      (unwind-protect
          (with-current-buffer buffer
            (niwa-chat-prepare-transcript-buffer)
            (setq-local niwa--workspace workspace niwa--session session
                        default-directory root)
            (niwa-session-set-root-buffer session buffer)
            (test-niwa-execution-remote--accept-storage session)
            (setf (niwa-session-permission-mode session) 'full-auto
                  (niwa-session-sandbox-mode session) 'required)
            (niwa-session-artifacts-ensure-files session buffer)
            (write-region "target evidence\n" nil (file-name-concat root "input.txt") nil 'silent)
            ;; The adapter runs in the neutral conversation directory on the host.
            (make-directory (file-name-concat niwa-claude-code-directory "conversations") t)
            (write-region "host decoy\n" nil (file-name-concat niwa-claude-code-directory "conversations" "input.txt") nil 'silent)
            (niwa-claude-code-register)
            (niwa-model-set-session-provider
             session (niwa-model-resolve-provider "Claude Code:sonnet") buffer)
            (setq-local gptel-system-prompt "Use the session execution target"
                        gptel-tools (mapcar
                                     (lambda (name) (niwa-tool-gptel-tool (niwa-tool-ensure name)))
                                     '("Read" "ApplyPatch" "Bash")))
            (cl-letf (((symbol-function 'gptel-request)
                       (lambda (&rest _) (ert-fail "Native remote send reached the API")))
                      ((symbol-function 'niwa-claude-code-launch)
                       (niwa-engine-test--claude-launch
                        (lambda (&rest _)
                          (list
                           :meta '((responseText . "Remote work completed")
                                   (toolBatches . [[((name . "Read") (id . "remote-read")
                                                     (args . ((file_path . "input.txt"))))]
                                                   [((name . "ApplyPatch") (id . "remote-patch")
                                                     (args . ((patch . "*** Begin Patch\n*** Update File: input.txt\n@@\n-target evidence\n+reviewed target evidence\n*** End Patch"))))]
                                                   [((name . "Bash") (id . "remote-bash")
                                                     (args . ((command . "cat input.txt > output.txt; cat output.txt")
                                                              (description . "Verify native target changes")
                                                              (yield_time_ms . 1000))))]])))))))
              (niwa--insert-user-turn "Read and update the target file, then verify it with Bash")
              (niwa--send-request)
              (with-timeout (60 (niwa-abort buffer) (ert-fail "Native remote turn did not settle"))
                (while (niwa-turn-busy-p buffer) (accept-process-output nil 0.05))))
            (should (= 1 (niwa-session-turn-count session)))
            (should (string-search "Remote work completed" (buffer-string)))
            (should (string-search "target evidence" (buffer-string)))
            (should-not (string-search "host decoy" (buffer-string)))
            (dolist (name '("input.txt" "output.txt"))
              (should (equal "reviewed target evidence\n"
                             (with-temp-buffer
                               (insert-file-contents (file-name-concat root name)) (buffer-string)))))
            (let ((cwd (file-name-concat niwa-claude-code-directory "conversations")))
              (should (equal "host decoy\n" (with-temp-buffer
                                             (insert-file-contents (file-name-concat cwd "input.txt"))
                                             (buffer-string))))
              (should-not (file-exists-p (file-name-concat cwd "output.txt")))))
        (niwa-execution-teardown-session session)
        (test-niwa-session-persistence--release-and-kill buffer session)
        (niwa-workspace-clear-registry)
        (delete-directory root t)
        (delete-directory local t)))))

(provide 'test-niwa-claude-code-remote)
;;; test-niwa-claude-code-remote.el ends here
