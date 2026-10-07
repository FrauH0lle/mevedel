;;; claude-restart.el --- Separate-editor subscription workflow -*- lexical-binding: t -*-

;;; Commentary:
;; Launched explicitly by the restart test, with only published files shared.

;;; Code:

(require 'ert)
(require 'gptel-openai)
(require 'mevedel-engine-test-support
         (expand-file-name "../mevedel-engine-test-support.el" (file-name-directory load-file-name)))
(require 'mevedel-claude-code-session)

(defun mevedel-test-claude-restart (root phase peer)
  "Run PHASE of the ROOT restart fixture using external ACP PEER."
  (let* ((workspace (test-mevedel-session-persistence--make-workspace root))
         (mevedel-claude-code-directory (file-name-concat root "claude"))
         (manifest (file-name-concat root "restart.el"))
         (scope (file-name-concat root "scope"))
         (instructions (file-name-concat scope "AGENTS.md"))
         (evidence (file-name-concat (if (= phase 1) scope root) "evidence.txt"))
         session buffer child launches)
    (mevedel-test--with-captured-diagnostics diagnostics
      (unwind-protect
          (cl-letf (((symbol-function 'mevedel-claude-code-launch)
                     (lambda (_system mcp model _effort &optional id hook)
                       (let ((native (if mevedel--agent-invocation "child-history" "root-history")))
                         (push (list native id model) launches)
                         (should (equal (and (= phase 2) native) id))
                         (should (equal "sonnet" model))
                         (list :command (executable-find "python3") :args (list peer)
                               :cwd root :mcp mcp :session-id id
                               :tool-id-field :claudecode/toolUseId
                               :observe #'mevedel-claude-code-context-observe
                               :check-context #'mevedel-claude-code-context-check
                               :control #'mevedel-claude-code--control
                               :meta `((fixtureSessionId . ,native)
                                       (hookCommand . ,hook)
                                       (responseText . ,(format "%s phase %d complete" native phase))
                                       (toolBatches . [[((name . "Read")
                                                        (id . ,(format "%s-%d" native phase))
                                                        (args . ((file_path . ,evidence))))]])))))))
            (mevedel-claude-code-register)
            (dolist (name '("Read" "ListAgents" "SendMessage")) (mevedel-tool-ensure name))
            (make-directory scope t)
            (write-region (format "Use RESTART-PATH-PHASE-%d" phase) nil instructions nil 'silent)
            (write-region "Restart evidence" nil evidence nil 'silent)
            (if (= phase 1)
                (progn
                  (setq session (mevedel-session-create "restart" workspace)
                        buffer (generate-new-buffer " *restart-root*"))
                  (with-current-buffer buffer
                    (mevedel-chat-prepare-transcript-buffer)
                    (setq-local mevedel--session session mevedel--workspace workspace
                                default-directory (file-name-as-directory root))
                    (setf (mevedel-session-permission-mode session) 'full-auto)
                    (mevedel-session-set-root-buffer session buffer)
                    (mevedel-session-artifacts-ensure-files session buffer)
                    (mevedel-model-set-session-provider
                     session (mevedel-model-resolve-provider "Claude Code:sonnet") buffer)))
              (let ((saved (with-temp-buffer (insert-file-contents manifest) (read (current-buffer)))))
                (should-not (= (plist-get saved :pid) (emacs-pid)))
                (setq buffer (mevedel-session-persistence-restore (plist-get saved :path) nil nil workspace)
                      session (buffer-local-value 'mevedel--session buffer))
                (should (mevedel-claude-code-backend-p (buffer-local-value 'gptel-backend buffer)))
                (should-not (mevedel-turn-busy-p buffer))
                (should-not (mevedel-agent-control-active-turn-p session))
                (should-not launches)
                (should (eq 'paused (mevedel-goal-status (mevedel-session-goal session))))
                (unless (assoc "/root/reader" (mevedel-session-agent-registry session))
                  (error "Child restore failed: %s" mevedel-test--captured))))
            (with-current-buffer buffer
              (setq-local gptel-system-prompt "Root restart fixture"
                          gptel-tools (list (mevedel-tool-gptel-tool (mevedel-tool-ensure "Read"))))
              (goto-char (point-max))
              (insert (format "\nRoot turn %d" phase))
              (let ((owner (mevedel--send-request (format "Root turn %d" phase))))
                (with-timeout (10 (ert-fail "Restart root turn timed out"))
                  (while (mevedel-turn-busy-p buffer) (accept-process-output nil 0.01)))
                (should (eq 'success (plist-get (mevedel-engine-info owner) :mevedel-acp-outcome))))
              (if (= phase 1)
                  (mevedel-agent-control-spawn
                   session "reader" "Read the evidence."
                   (lambda (result) (setq child (plist-get result :record)))
                   :agent (mevedel-agent--create :name "reader" :description "Inspect evidence"
                                                :tools '(Read) :system-prompt "Child restart fixture"))
                (setq child (mevedel-agent-control--record-at-path session "/root/reader"))
                (should child)
                (setq-local gptel-model 'haiku)
                (mevedel-agent-control-followup session "/root/reader" "Read again after restart."))
              (with-timeout (10 (ert-fail "Restart child turn timed out"))
                (while (mevedel-agent-control-active-turn-p session) (accept-process-output nil 0.01)))
              (should (equal (format "child-history phase %d complete" phase)
                             (mevedel-agent-record-settled-result child)))
              (should (= phase (mevedel-session-turn-count session)))
              (dolist (scope '("root" "/root/reader"))
                (let ((history (alist-get scope (mevedel-session-external-conversations session) nil nil #'equal)))
                  (should (eq 'ready (plist-get history :state)))))
              (dolist (conversation (list buffer (mevedel-agent-record-conversation-buffer child)))
                (with-current-buffer conversation
                  (should (= 1 (mevedel-engine-test--count-evidence (format "RESTART-PATH-PHASE-%d" phase))))))
              (when (= phase 2)
                (should (string-search "root-history phase 1 complete" (buffer-string)))
                (with-current-buffer (mevedel-agent-record-conversation-buffer child)
                  (should (string-search "child-history phase 1 complete" (buffer-string)))))
              (when (= phase 1)
                (setf (mevedel-session-goal session)
                      (mevedel-goal--create :id "restart-goal" :objective "Wait for explicit resume"
                                            :status 'active :tokens-used 0 :time-used-seconds 0
                                            :turns-run 0 :created-at "now" :updated-at "now")))
              (mevedel-session-artifacts-save session buffer)
              (when (= phase 1)
                (with-temp-file manifest
                  (prin1 (list :pid (emacs-pid) :path (mevedel-session-save-path session)) (current-buffer)))))
            (should (= 2 (length launches))))
        (when (and child (buffer-live-p (mevedel-agent-record-conversation-buffer child)))
          (kill-buffer (mevedel-agent-record-conversation-buffer child)))
        (test-mevedel-session-persistence--release-and-kill buffer session)
        (mevedel-workspace-clear-registry)))))

;;; claude-restart.el ends here
