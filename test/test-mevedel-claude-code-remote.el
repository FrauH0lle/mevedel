;;; test-mevedel-claude-code-remote.el --- Native tools on real targets -*- lexical-binding: t -*-

;;; Commentary:
;; A local ACP peer invokes the real MCP bridge against provisioned targets.

;;; Code:

(require 'mevedel-engine-test-support
         (file-name-concat (file-name-directory (or load-file-name buffer-file-name))
                           "mevedel-engine-test-support"))
(require 'mevedel-execution-test-helpers
         (file-name-concat (file-name-directory (or load-file-name buffer-file-name))
                           "mevedel-execution-test-helpers"))
(require 'mevedel-claude-code-session)
(require 'tramp-container)
(require 'tramp-sh)

(mevedel-deftest mevedel-claude-code/remote (:quiet t)
  (pcase-dolist (`(,variable ,method) '(("MEVEDEL_TEST_SSH_ROOT" ssh)
                                       ("MEVEDEL_TEST_PODMAN_ROOT" podman)))
    (let* ((tramp-use-connection-share tramp-use-connection-share)
           (tramp-ssh-controlmaster-options tramp-ssh-controlmaster-options)
           (mevedel-session-durability--disclosed-targets
            (copy-hash-table mevedel-session-durability--disclosed-targets))
           (base (test-mevedel-execution-remote--real-root variable method))
           (root (test-mevedel-execution-remote--real-temp-directory base "mevedel-native-"))
           (local (make-temp-file "mevedel-native-host-" t))
           (workspace (test-mevedel-execution--workspace root))
           (session (mevedel-session-create "native" workspace root))
           (buffer (generate-new-buffer " *native-remote*"))
           (gptel--known-backends nil)
           (mevedel-claude-code-directory (file-name-concat local "claude")))
      (unwind-protect
          (with-current-buffer buffer
            (mevedel-chat-prepare-transcript-buffer)
            (setq-local mevedel--workspace workspace mevedel--session session
                        default-directory root)
            (mevedel-session-set-root-buffer session buffer)
            (test-mevedel-execution-remote--accept-storage session)
            (setf (mevedel-session-permission-mode session) 'full-auto
                  (mevedel-session-sandbox-mode session) 'required)
            (mevedel-session-artifacts-ensure-files session buffer)
            (write-region "target evidence\n" nil (file-name-concat root "input.txt") nil 'silent)
            ;; The adapter runs in the neutral conversation directory on the host.
            (make-directory (file-name-concat mevedel-claude-code-directory "conversations") t)
            (write-region "host decoy\n" nil (file-name-concat mevedel-claude-code-directory "conversations" "input.txt") nil 'silent)
            (mevedel-claude-code-register)
            (mevedel-model-set-session-provider
             session (mevedel-model-resolve-provider "Claude Code:sonnet") buffer)
            (setq-local gptel-system-prompt "Use the session execution target"
                        gptel-tools (mapcar
                                     (lambda (name) (mevedel-tool-gptel-tool (mevedel-tool-ensure name)))
                                     '("Read" "ApplyPatch" "Bash")))
            (cl-letf (((symbol-function 'gptel-request)
                       (lambda (&rest _) (ert-fail "Native remote send reached the API")))
                      ((symbol-function 'mevedel-claude-code-launch)
                       (mevedel-engine-test--claude-launch
                        (lambda (&rest _)
                          (list
                           :meta '((responseText . "Remote work completed")
                                   (toolBatches . [[((name . "Read") (id . "remote-read")
                                                     (args . ((file_path . "input.txt"))))]
                                                   [((name . "ApplyPatch") (id . "remote-patch")
                                                     (args . ((patch . "*** Begin Patch\n*** Update File: input.txt\n@@\n-target evidence\n+reviewed target evidence\n*** End Patch"))))]
                                                   [((name . "Bash") (id . "remote-bash")
                                                     (args . ((command . "cat input.txt > output.txt; cat output.txt")
                                                              (yield_time_ms . 1000))))]])))))))
              (mevedel--insert-user-turn "Read and update the target file, then verify it with Bash")
              (mevedel--send-request)
              (with-timeout (60 (mevedel-abort buffer) (ert-fail "Native remote turn did not settle"))
                (while (mevedel-turn-busy-p buffer) (accept-process-output nil 0.05))))
            (should (= 1 (mevedel-session-turn-count session)))
            (should (string-search "Remote work completed" (buffer-string)))
            (should (string-search "target evidence" (buffer-string)))
            (should-not (string-search "host decoy" (buffer-string)))
            (dolist (name '("input.txt" "output.txt"))
              (should (equal "reviewed target evidence\n"
                             (with-temp-buffer
                               (insert-file-contents (file-name-concat root name)) (buffer-string)))))
            (let ((cwd (file-name-concat mevedel-claude-code-directory "conversations")))
              (should (equal "host decoy\n" (with-temp-buffer
                                             (insert-file-contents (file-name-concat cwd "input.txt"))
                                             (buffer-string))))
              (should-not (file-exists-p (file-name-concat cwd "output.txt")))))
        (mevedel-execution-teardown-session session)
        (test-mevedel-session-persistence--release-and-kill buffer session)
        (mevedel-workspace-clear-registry)
        (delete-directory root t)
        (delete-directory local t)))))

(provide 'test-mevedel-claude-code-remote)
;;; test-mevedel-claude-code-remote.el ends here
