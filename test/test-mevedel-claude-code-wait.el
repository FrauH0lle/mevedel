;;; test-mevedel-claude-code-wait.el --- Native parent and child permission waits -*- lexical-binding: t -*-

;;; Commentary:
;; A real ACP parent waits while its retained child uses the root permission UI.

;;; Code:

(require 'mevedel-engine-test-support
         (file-name-concat (file-name-directory (or load-file-name buffer-file-name))
                           "mevedel-engine-test-support"))
(require 'mevedel-claude-code-session)
(require 'mevedel-permission-prompt)
(require 'mevedel-view)

(defconst mevedel-claude-code-wait-test--peer
  (file-name-concat (file-name-directory (or load-file-name buffer-file-name))
                    "fixtures" "acp-agent.py"))

(mevedel-deftest mevedel--send-request/claude-child-permission-wait (:quiet t)
                 (dolist (scenario '((allow) (deny) (interrupt) (allow . t) (deny . t) (interrupt . t)))
                   (mevedel-engine-test--with-session
                    (let* ((decision (car scenario))
                           (nested (cdr scenario))
                           (path (if nested "/root/reader/helper" "/root/reader"))
                           (gptel--known-backends nil)
                           (mevedel-claude-code-directory (file-name-concat root "claude"))
                           (mevedel-permission-reviewer nil)
                           (file (file-name-concat root "evidence.txt"))
                           view record permission)
                      (write-region "CHILD-EVIDENCE-72851" nil file nil 'silent)
                      (setf (mevedel-session-permission-mode session) 'edits
                            (mevedel-session-permission-rules session) '(("Read" :action ask))
                            (mevedel-session-agent-turn-capacity session) (if nested 2 1))
                      (mevedel-claude-code-register)
                      (mevedel-model-set-session-provider
                       session (mevedel-model-resolve-provider "Claude Code:sonnet") buffer)
                      (setq-local gptel-system-prompt "Retained child permission fixture"
                                  gptel-tools (mapcar (lambda (name) (mevedel-tool-gptel-tool (mevedel-tool-ensure name)))
                                                      '("Agent" "WaitAgent" "Read")))
                      (setq view (mevedel-view--ensure buffer))
                      (unwind-protect
                          (cl-letf (((symbol-function 'mevedel-claude-code-launch)
                                     (lambda (_system mcp _model _effort &optional id hook)
                                       (let* ((parent (eq (current-buffer) buffer))
                                              (branch (or parent (and nested (equal "/root/reader" (mevedel-current-origin))))))
                                         (list :command (executable-find "python3")
                                               :args (list mevedel-claude-code-wait-test--peer)
                                               :cwd root :mcp mcp :session-id id
                                               :tool-id-field :claudecode/toolUseId
                                               :control #'mevedel-claude-code--control
                                               :observe #'mevedel-claude-code--observe
                                               :check-context #'mevedel-claude-code-context-check
                                               :meta `((hookCommand . ,hook) (preToolHook . t)
                                                       (fixtureSessionId . ,(mevedel-current-origin))
                                                       (responseText . ,(if parent "Parent complete" "Child complete"))
                                                       (toolBatches . ,(if branch
                                                                           `[[((name . "Agent") (id . "spawn")
                                                                               (args . ((task_name . ,(if parent "reader" "helper")) (message . "Read the evidence."))))]
                                                                             [((name . "WaitAgent") (id . "wait") (args . ((timeout_ms . 10000))))]]
                                                                         `[[((name . "Read") (id . "read") (args . ((file_path . ,file))))]]))))))))
                            (mevedel--insert-user-turn "Delegate a read and wait for its result")
                            (mevedel--send-request "Delegate a read and wait for its result")
                            (with-timeout (8 (ert-fail "Child permission and parent wait did not become ready"))
                              (while (not (and (mevedel-agent-control-root-waiting-p session)
                                               (mevedel-session-permission-queue session)
                                               (or (not nested)
                                                   (equal "waiting"
                                                          (plist-get (cl-find "/root/reader" (mevedel-agent-control-list-agents session)
                                                                              :key (lambda (row) (plist-get row :path)) :test #'equal)
                                                                     :activity)))))
                                (accept-process-output nil 0.01)))
                            (setq record (cdr (assoc path (mevedel-session-agent-registry session))))
                            (should (mevedel-turn-busy-p buffer))
                            (should (mevedel-agent-control-active-turn-p session))
                            (should-error (mevedel-agent-control-spawn session "overflow" "No capacity remains." #'ignore)
                                          :type 'user-error)
                            (should (equal path (plist-get (car (mevedel-session-permission-queue session)) :origin)))
                            (when nested
                              (should (equal "waiting" (plist-get (cl-find "/root/reader" (mevedel-agent-control-list-agents session)
                                                                           :key (lambda (row) (plist-get row :path)) :test #'equal)
                                                                  :activity))))
                            (with-current-buffer view
                              (setq permission (seq-find (lambda (overlay) (overlay-get overlay 'mevedel-permission-prompt))
                                                         (overlays-in (point-min) (point-max))))
                              (should permission)
                              (should (string-search path (buffer-string)))
                              (goto-char (overlay-start permission))
                              (pcase decision
                                ('allow (call-interactively #'mevedel-permission--prompt-approve-once))
                                ('deny (call-interactively #'mevedel-permission--prompt-deny-once))
                                ('interrupt (mevedel-agent-control-interrupt session path))))
                            (with-timeout (8 (ert-fail "Child decision did not wake and settle its parent"))
                              (while (or (mevedel-turn-busy-p buffer) (mevedel-agent-control-active-turn-p session))
                                (accept-process-output nil 0.01)))
                            (should-not (mevedel-session-permission-queue session))
                            (should-not (mevedel-agent-control-root-waiting-p session))
                            (should-not (overlay-buffer permission))
                            (should (eq 'success (plist-get (mevedel-engine-info request) :mevedel-acp-outcome)))
                            (should (= 1 (mevedel-session-turn-count session)))
                            (should (= 2 (how-many "^#\\+begin_tool" (point-min) (point-max))))
                            (should (eq (if (eq decision 'interrupt) 'interrupted 'completed)
                                        (mevedel-agent-record-settled-outcome record)))
                            (mevedel--prompt--settle permission 'allow-once)
                            (with-current-buffer (mevedel-agent-record-conversation-buffer record)
                              (should (eq (and (string-search "CHILD-EVIDENCE-72851" (buffer-string)) t)
                                          (eq decision 'allow))))
                            (let (replacement)
                              (mevedel-agent-control-spawn session "replacement" "Use the released capacity."
                                                           (lambda (value) (setq replacement value)))
                              (with-timeout (8 (ert-fail "Released child capacity was not reusable"))
                                (while (not replacement) (accept-process-output nil 0.01)))
                              (should (eq 'success (plist-get replacement :outcome)))
                              (mevedel-agent-control-interrupt session "/root/replacement")
                              (with-timeout (8 (ert-fail "Replacement interruption did not settle"))
                                (while (mevedel-agent-control-active-turn-p session) (accept-process-output nil 0.01)))
                              (should-not (mevedel-session-permission-queue session))
                              (should (= 1 (mevedel-session-turn-count session)))))
                        (mevedel-agent-control-teardown-session session))))))

(provide 'test-mevedel-claude-code-wait)
;;; test-mevedel-claude-code-wait.el ends here
