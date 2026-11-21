;;; claude-restart.el --- Separate-editor subscription workflow -*- lexical-binding: t -*-

;;; Commentary:
;; Launched explicitly by the restart test, with only published files shared.

;;; Code:

(require 'ert)
(require 'gptel-openai)
(require 'niwa-engine-test-support
         (expand-file-name "../niwa-engine-test-support.el" (file-name-directory load-file-name)))
(require 'niwa-claude-code-session)

(defun niwa-test-claude-restart (root phase peer)
  "Run PHASE of the ROOT restart fixture using external ACP PEER."
  (let* ((workspace (test-niwa-session-persistence--make-workspace root))
         (niwa-claude-code-directory (file-name-concat root "claude"))
         (manifest (file-name-concat root "restart.el"))
         (scope (file-name-concat root "scope"))
         (instructions (file-name-concat scope "AGENTS.md"))
         (evidence (file-name-concat (if (= phase 1) scope root) "evidence.txt"))
         session buffer child launches)
    (niwa-test--with-captured-diagnostics diagnostics
      (unwind-protect
          (cl-letf (((symbol-function 'niwa-claude-code-launch)
                     (niwa-engine-test--claude-launch
                      (lambda (_system _mcp model _effort &optional id _hook)
                        (let ((native (if niwa--agent-invocation "child-history" "root-history")))
                          (push (list native id model) launches)
                          (should (equal (and (= phase 2) native) id))
                          (should (equal "sonnet" model))
                          (list :args (list peer)
                                :meta `((fixtureSessionId . ,native)
                                        (responseText . ,(format "%s phase %d complete" native phase))
                                        (toolBatches . [[((name . "Read")
                                                         (id . ,(format "%s-%d" native phase))
                                                         (args . ((file_path . ,evidence))))]]))))))))
            (niwa-claude-code-register)
            (dolist (name '("Read" "ListAgents" "SendMessage")) (niwa-tool-ensure name))
            (make-directory scope t)
            (write-region (format "Use RESTART-PATH-PHASE-%d" phase) nil instructions nil 'silent)
            (write-region "Restart evidence" nil evidence nil 'silent)
            (if (= phase 1)
                (progn
                  (setq session (niwa-session-create "restart" workspace)
                        buffer (generate-new-buffer " *restart-root*"))
                  (with-current-buffer buffer
                    (niwa-chat-prepare-transcript-buffer)
                    (setq-local niwa--session session niwa--workspace workspace
                                default-directory (file-name-as-directory root))
                    (setf (niwa-session-permission-mode session) 'full-auto)
                    (niwa-session-set-root-buffer session buffer)
                    (niwa-session-artifacts-ensure-files session buffer)
                    (niwa-model-set-session-provider
                     session (niwa-model-resolve-provider "Claude Code:sonnet") buffer)))
              (let ((saved (with-temp-buffer (insert-file-contents manifest) (read (current-buffer)))))
                (should-not (= (plist-get saved :pid) (emacs-pid)))
                (setq buffer (niwa-session-persistence-restore (plist-get saved :path) nil nil workspace)
                      session (buffer-local-value 'niwa--session buffer))
                (should (niwa-claude-code-backend-p (buffer-local-value 'gptel-backend buffer)))
                (should-not (niwa-turn-busy-p buffer))
                (should-not (niwa-agent-control-active-turn-p session))
                (should-not launches)
                (should (eq 'paused (niwa-goal-status (niwa-session-goal session))))
                (unless (assoc "/root/reader" (niwa-session-agent-registry session))
                  (error "Child restore failed: %s" niwa-test--captured))))
            (with-current-buffer buffer
              (setq-local gptel-system-prompt "Root restart fixture"
                          gptel-tools (list (niwa-tool-gptel-tool (niwa-tool-ensure "Read"))))
              (goto-char (point-max))
              (insert (format "\nRoot turn %d" phase))
              (let ((owner (niwa--send-request (format "Root turn %d" phase))))
                (with-timeout (10 (ert-fail "Restart root turn timed out"))
                  (while (niwa-turn-busy-p buffer) (accept-process-output nil 0.01)))
                (unless (eq 'success (plist-get (niwa-engine-info owner) :niwa-acp-outcome))
                  (error "Request outcome: %S; diagnostics: %S"
                         (plist-get (niwa-engine-info owner) :error) niwa-test--captured)))
              (if (= phase 1)
                  (niwa-agent-control-spawn
                   session "reader" "Read the evidence."
                   (lambda (result) (setq child (plist-get result :record)))
                   :agent (niwa-agent--create :name "reader" :description "Inspect evidence"
                                                :tools '(Read) :system-prompt "Child restart fixture"))
                (setq child (niwa-agent-control--record-at-path session "/root/reader"))
                (should child)
                (setq-local gptel-model 'haiku)
                (niwa-agent-control-send-message session "/root/reader" "Read again after restart."))
              (with-timeout (10 (ert-fail "Restart child turn timed out"))
                (while (niwa-agent-control-active-turn-p session) (accept-process-output nil 0.01)))
              (should (equal (format "child-history phase %d complete" phase)
                             (niwa-agent-record-settled-result child)))
              (should (= phase (niwa-session-turn-count session)))
              (dolist (scope '("root" "/root/reader"))
                (let ((history (alist-get scope (niwa-session-external-conversations session) nil nil #'equal)))
                  (should (eq 'ready (plist-get history :state)))))
              (dolist (conversation (list buffer (niwa-agent-record-conversation-buffer child)))
                (with-current-buffer conversation
                  (should (= 1 (niwa-engine-test--count-evidence (format "RESTART-PATH-PHASE-%d" phase))))))
              (when (= phase 2)
                (should (string-search "root-history phase 1 complete" (buffer-string)))
                (with-current-buffer (niwa-agent-record-conversation-buffer child)
                  (should (string-search "child-history phase 1 complete" (buffer-string)))))
              (when (= phase 1)
                (setf (niwa-session-goal session)
                      (niwa-goal--create :id "restart-goal" :objective "Wait for explicit resume"
                                            :status 'active :tokens-used 0 :time-used-seconds 0
                                            :turns-run 0 :created-at "now" :updated-at "now")))
              (niwa-session-artifacts-save session buffer)
              (when (= phase 1)
                (with-temp-file manifest
                  (prin1 (list :pid (emacs-pid) :path (niwa-session-save-path session)) (current-buffer)))))
            (should (= 2 (length launches))))
        (when (and child (buffer-live-p (niwa-agent-record-conversation-buffer child)))
          (kill-buffer (niwa-agent-record-conversation-buffer child)))
        (test-niwa-session-persistence--release-and-kill buffer session)
        (niwa-workspace-clear-registry)))))

;;; claude-restart.el ends here
