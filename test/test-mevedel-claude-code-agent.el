;;; test-mevedel-claude-code-agent.el --- Native retained agent limits -*- lexical-binding: t -*-

;;; Commentary:
;; Exercise limits and follow-ups through retained-agent control.

;;; Code:

(require 'mevedel-engine-test-support
         (file-name-concat (file-name-directory (or load-file-name buffer-file-name))
                           "mevedel-engine-test-support"))
(require 'mevedel-claude-code-agent)
(require 'mevedel-claude-code-session)

(defconst mevedel-claude-code-agent-test--peer
  (file-name-concat (file-name-directory (or load-file-name buffer-file-name))
                    "fixtures" "acp-agent.py"))

(mevedel-deftest mevedel-agent-control-spawn/claude-sample-limit (:quiet t)
  (dolist (limit '(1 3 8 nil))
    (mevedel-engine-test--with-session
      (let ((gptel--known-backends nil)
            (mevedel-claude-code-directory (file-name-concat root "claude"))
            (file (file-name-concat root "evidence.txt"))
            (launches 0) record invocation)
        (write-region "Evidence" nil file nil 'silent)
        (dolist (name '("Read" "ListAgents" "SendMessage")) (mevedel-tool-ensure name))
        (mevedel-claude-code-register)
        (mevedel-model-set-session-provider
         session (mevedel-model-resolve-provider "Claude Code:sonnet") buffer)
        (unwind-protect
            (cl-letf (((symbol-function 'mevedel-claude-code-launch)
                       (lambda (_system mcp _model _effort &optional id hook)
                         (cl-incf launches)
                         (list :command (executable-find "python3")
                               :args (list mevedel-claude-code-agent-test--peer)
                               :cwd root :mcp mcp :session-id id
                               :tool-id-field :claudecode/toolUseId
                               :control #'mevedel-claude-code--control
                               :check-context #'mevedel-claude-code-context-check
                               :observe #'mevedel-claude-code-context-observe
                               :meta `((hookCommand . ,hook) (suppressResponseOnStop . t)
                                       (compactBeforeBatch . ,(when (equal limit 3) 2))
                                       (responseText . "All work completed")
                                       (toolBatches . ,(vconcat
                                                       (cl-loop for batch below 5 collect
                                                                (vconcat
                                                                 (cl-loop for tool below 2 collect
                                                                          `((name . "Read")
                                                                            (id . ,(format "%d-%d-%d" launches batch tool))
                                                                            (args . ((file_path . ,file))))))))))))))
              (dotimes (phase 2)
                (if (= phase 0)
                    (mevedel-agent-control-spawn
                     session "limited" "Read the evidence in several steps."
                     (lambda (value) (setq record (plist-get value :record)))
                     :agent (mevedel-agent--create :name "limited" :description "Read evidence"
                                                  :tools '(Read) :max-turns limit
                                                  :system-prompt "Read evidence")
                     :on-invocation (lambda (value) (setq invocation value)))
                  (mevedel-agent-control-followup session "/root/limited" "Continue the investigation.")
                  (setq invocation (mevedel-agent-record-invocation record)))
                (with-timeout (10 (ert-fail "Limited child did not settle"))
                  (while (not (mevedel-agent-invocation-runtime-settled-p invocation))
                    (accept-process-output nil 0.01)))
                (ert-info ((format "limit=%S phase=%d" limit phase))
                  (should (= (min (or limit 6) 6) (mevedel-agent-invocation-turn-count invocation)))
                  (should (eq 'completed (mevedel-agent-record-settled-outcome record)))
                  (if (and limit (<= limit 5))
                      (should (string-search (format "%d-turn limit was reached" limit)
                                             (mevedel-agent-record-settled-result record)))
                    (should (string-suffix-p "All work completed" (mevedel-agent-record-settled-result record))))
                  (with-current-buffer (mevedel-agent-record-conversation-buffer record)
                    (should (= (* (1+ phase) 2 (min (or limit 5) 5))
                               (how-many "^#\\+begin_tool" (point-min) (point-max))))
                    (when (and limit (<= limit 5))
                      (should (= (* (1+ phase) (if (equal limit 3) 2 1))
                                 (mevedel-engine-test--count-evidence "This is your final turn"))))
                    (when (equal limit 3)
                      (should (= (1+ phase) (mevedel-engine-test--count-evidence "You have used 2 of 3 turns"))))))))
          (when (and record (buffer-live-p (mevedel-agent-record-conversation-buffer record)))
            (kill-buffer (mevedel-agent-record-conversation-buffer record))))))))

(mevedel-deftest mevedel-agent-control-followup/claude-edited-history (:quiet t)
  (dolist (phase '(completed compacted active reopened))
    (mevedel-engine-test--with-session
      (mevedel-request-end)
      (let ((gptel--known-backends nil)
            (mevedel-claude-code-directory (file-name-concat root "claude"))
            (launches 0) record invocation receipt restored restored-session)
        (mevedel-claude-code-register)
        (mevedel-model-set-session-provider
         session (mevedel-model-resolve-provider "Claude Code:sonnet") buffer)
        (unwind-protect
            (cl-letf (((symbol-function 'mevedel-claude-code-launch)
                       (lambda (_system mcp _model _effort &optional id _hook)
                         (cl-incf launches)
                         (list :command (executable-find "python3")
                               :args (list mevedel-claude-code-agent-test--peer)
                               :cwd root :mcp mcp :session-id id
                               :observe (lambda (owner event)
                                          (mevedel-claude-code-context-observe owner event)
                                          (when (equal "user" (alist-get 'type (alist-get 'message (alist-get 'params event))))
                                            (setq receipt t)))
                               :check-context #'mevedel-claude-code-context-check
                               :meta `((echoAllText . t)
                                       (compactionEvents .
                                        ,(if (eq phase 'compacted)
                                             [((sessionUpdate . "agent_message_chunk")
                                               (content . ((type . "text") (text . "Prior response\n"))))
                                              ((sessionUpdate . "compaction_update") (compactionId . "child")
                                               (status . "completed")
                                               (summary . [((type . "text") (text . "RETAINED CHILD SUMMARY"))]))]
                                           [])))))))
              (mevedel-agent-control-spawn
               session "editor" (if (memq phase '(compacted active reopened)) "wait-silent" "SUBMITTED CHILD INPUT")
               (lambda (value) (setq record (plist-get value :record)))
               :agent (mevedel-agent--create :name "editor" :description "Respond"
                                            :tools nil :system-prompt "Respond")
               :on-invocation (lambda (value) (setq invocation value)))
              (with-timeout (5 (ert-fail "Child did not acknowledge submitted input"))
                (while (not receipt) (accept-process-output nil 0.01)))
              (unless (memq phase '(compacted active reopened))
                (with-timeout (5 (ert-fail "Child did not settle"))
                  (while (not (mevedel-agent-invocation-runtime-settled-p invocation))
                    (accept-process-output nil 0.01)))
                (should (eq 'completed (mevedel-agent-record-settled-outcome record))))
              (with-current-buffer (mevedel-agent-record-conversation-buffer record)
                (should-not buffer-read-only)
                (gptel--save-state)
                (gptel--save-state)
                (goto-char (point-max))
                (insert "\nnext drafX")
                (delete-char -1)
                (insert "t")
                (should-not (eq 'diverged (plist-get (alist-get "/root/editor" (mevedel-session-external-conversations session) nil nil #'equal) :state))))
              (when (eq phase 'reopened)
                (mevedel-agent-control-interrupt session "/root/editor")
                (with-timeout (5 (ert-fail "Child interrupt did not settle"))
                  (while (mevedel-agent-control-active-turn-p session) (accept-process-output nil 0.01)))
                (let ((path (mevedel-session-save-path session)))
                  (mevedel-agent-control-teardown-session session)
                  (test-mevedel-session-persistence--release-and-kill buffer session)
                  (setq restored (mevedel-session-persistence-restore path nil nil workspace)
                        restored-session (buffer-local-value 'mevedel--session restored)
                        session restored-session
                        record (mevedel-agent-control--record-at-path session "/root/editor")))
                (mevedel-agent-persistence-ensure-conversation session record restored))
              ;; A child edit must not poison the independent root scope.
              (setf (alist-get "root" (mevedel-session-external-conversations session) nil nil #'equal)
                    (list :engine 'claude-code :id "root-native" :host (system-name)
                          :directory mevedel-claude-code-directory :state 'ready))
              (with-current-buffer (mevedel-agent-record-conversation-buffer record)
                (goto-char (point-min))
                (search-forward (pcase phase ('compacted "RETAINED CHILD SUMMARY")
                                  ((or 'active 'reopened) "wait-silent") (_ "SUBMITTED CHILD INPUT")))
                (replace-match "USER EDITED EVIDENCE" t t)
                (ert-info ((format "phase=%S" phase))
                  (should (string-search "USER EDITED EVIDENCE"
                                         (if (eq phase 'compacted)
                                             (mevedel-claude-code-history-excerpt session t)
                                           (mevedel-engine-test--evidence)))))
                (should (eq 'diverged (plist-get (alist-get "/root/editor" (mevedel-session-external-conversations session) nil nil #'equal) :state)))
                (should (eq 'ready (plist-get (alist-get "root" (mevedel-session-external-conversations session) nil nil #'equal) :state))))
              (when (memq phase '(compacted active))
                (mevedel-agent-control-interrupt session "/root/editor")
                (with-timeout (5 (ert-fail "Edited child interrupt did not settle"))
                  (while (mevedel-agent-control-active-turn-p session) (accept-process-output nil 0.01))))
              (should (eq 'diverged (plist-get (alist-get "/root/editor" (mevedel-session-external-conversations session) nil nil #'equal) :state)))
              (with-current-buffer (or restored buffer)
                (should-error (mevedel-agent-control-followup session "/root/editor" "Continue using my edited evidence.") :type 'user-error))
              (should (= 1 launches))
              (with-current-buffer (or restored buffer)
                (mevedel-claude-code-recover-history "/root/editor")
                (mevedel-agent-control-followup session "/root/editor" "Continue using my edited evidence."))
              (with-timeout (5 (ert-fail "Recovered child did not settle"))
                (while (mevedel-agent-control-active-turn-p session) (accept-process-output nil 0.01)))
              (should (= 2 launches))
              (should (eq 'completed (mevedel-agent-record-settled-outcome record)))
              (should (string-search "Excerpt continuation" (mevedel-agent-record-settled-result record)))
              (should (string-search "USER EDITED EVIDENCE" (mevedel-agent-record-settled-result record))))
          (when (and record (buffer-live-p (mevedel-agent-record-conversation-buffer record)))
            (kill-buffer (mevedel-agent-record-conversation-buffer record)))
          (mevedel-agent-control-teardown-session session)
          (when (buffer-live-p restored)
            (test-mevedel-session-persistence--release-and-kill restored restored-session)))))))

(mevedel-deftest mevedel-agent-control-spawn/claude-success-preserves-divergence (:quiet t)
  (mevedel-engine-test--with-session
    (mevedel-request-end)
    (let ((gptel--known-backends nil)
          (mevedel-claude-code-directory (file-name-concat root "claude"))
          record invocation edited)
      (mevedel-claude-code-register)
      (mevedel-model-set-session-provider
       session (mevedel-model-resolve-provider "Claude Code:sonnet") buffer)
      (unwind-protect
          (cl-letf (((symbol-function 'mevedel-claude-code-launch)
                     (lambda (_system mcp _model _effort &optional id _hook)
                       (list :command (executable-find "python3")
                             :args (list mevedel-claude-code-agent-test--peer)
                             :cwd root :mcp mcp :session-id id
                             :observe
                             (lambda (owner event)
                               (mevedel-claude-code-context-observe owner event)
                               (when (and (not edited)
                                          (equal "user" (alist-get 'type (alist-get 'message (alist-get 'params event)))))
                                 (setq edited t)
                                 (with-current-buffer (mevedel-agent-invocation-buffer owner)
                                   (goto-char (point-min))
                                   (search-forward "SUBMITTED CHILD INPUT")
                                   (replace-match "USER EDITED INPUT" t t))))))))
            (mevedel-agent-control-spawn
             session "editor" "SUBMITTED CHILD INPUT"
             (lambda (value) (setq record (plist-get value :record)))
             :agent (mevedel-agent--create :name "editor" :description "Respond"
                                          :tools nil :system-prompt "Respond")
             :on-invocation (lambda (value) (setq invocation value)))
            (with-timeout (5 (ert-fail "Edited successful child did not settle"))
              (while (not (mevedel-agent-invocation-runtime-settled-p invocation))
                (accept-process-output nil 0.01)))
            (should edited)
            (should (eq 'completed (mevedel-agent-record-settled-outcome record)))
            (should (eq 'diverged (plist-get (alist-get "/root/editor" (mevedel-session-external-conversations session) nil nil #'equal) :state))))
        (when (and record (buffer-live-p (mevedel-agent-record-conversation-buffer record)))
          (kill-buffer (mevedel-agent-record-conversation-buffer record)))
        (mevedel-agent-control-teardown-session session)))))

(provide 'test-mevedel-claude-code-agent)
;;; test-mevedel-claude-code-agent.el ends here
