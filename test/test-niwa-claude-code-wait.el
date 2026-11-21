;;; test-niwa-claude-code-wait.el --- Native parent and child permission waits -*- lexical-binding: t -*-

;;; Commentary:
;; A real ACP parent waits while its retained child uses the root permission UI.

;;; Code:

(require 'niwa-engine-test-support
         (file-name-concat (file-name-directory (or load-file-name buffer-file-name))
                           "niwa-engine-test-support"))
(require 'niwa-claude-code-session)
(require 'niwa-permission-prompt)
(require 'niwa-view)

(niwa-deftest niwa--send-request/claude-child-permission-wait (:quiet t)
                 (dolist (scenario '((allow) (deny) (interrupt) (allow . t) (deny . t) (interrupt . t)))
                   (niwa-engine-test--with-claude-session
                    :file-workspace (not (equal scenario '(allow)))
                    (let* ((decision (car scenario))
                           (nested (cdr scenario))
                           (path (if nested "/root/reader/helper" "/root/reader"))
                           (niwa-permission-reviewer nil)
                           (file (file-name-concat root "evidence.txt"))
                           view record permission)
                      (write-region "CHILD-EVIDENCE-72851" nil file nil 'silent)
                      (setf (niwa-session-permission-mode session) 'edits
                            (niwa-session-permission-rules session) '(("Read" :action ask))
                            (niwa-session-agent-turn-capacity session) (if nested 2 1))
                      (setq-local gptel-system-prompt "Retained child permission fixture"
                                  gptel-tools (mapcar (lambda (name) (niwa-tool-gptel-tool (niwa-tool-ensure name)))
                                                      '("Agent" "WaitAgent" "Read")))
                      (setq view (niwa-view--ensure buffer))
                      (unwind-protect
                          (cl-letf (((symbol-function 'niwa-claude-code-launch)
                                     (niwa-engine-test--claude-launch
                                      (lambda (_system mcp _model _effort &optional id hook)
                                        (let* ((parent (eq (current-buffer) buffer))
                                               (branch (or parent (and nested (equal "/root/reader" (niwa-current-origin))))))
                                          (list :meta `((hookCommand . ,hook) (preToolHook . t)
                                                        (fixtureSessionId . ,(niwa-current-origin))
                                                        (responseText . ,(if parent "Parent complete" "Child complete"))
                                                        (toolBatches . ,(if branch
                                                                            `[[((name . "Agent") (id . "spawn")
                                                                                (args . ((task_name . ,(if parent "reader" "helper")) (message . "Read the evidence."))))]
                                                                              [((name . "WaitAgent") (id . "wait") (args . ((timeout_ms . 10000))))]]
                                                                          `[[((name . "Read") (id . "read") (args . ((file_path . ,file))))]])))))))))
                            (niwa--insert-user-turn "Delegate a read and wait for its result")
                            (niwa--send-request "Delegate a read and wait for its result")
                            (niwa-test--await 8 "Child permission and parent wait did not become ready"
                              (and (niwa-agent-control-root-waiting-p session)
                                   (niwa-session-permission-queue session)
                                   (or (not nested)
                                       (equal "waiting"
                                              (plist-get (cl-find "/root/reader" (niwa-agent-control-list-agents session)
                                                                  :key (lambda (row) (plist-get row :path)) :test #'equal)
                                                         :activity)))))
                            (setq record (cdr (assoc path (niwa-session-agent-registry session))))
                            (should (niwa-turn-busy-p buffer))
                            (should (niwa-agent-control-active-turn-p session))
                            (should-error (niwa-agent-control-spawn session "overflow" "No capacity remains." #'ignore)
                                          :type 'user-error)
                            (should (equal path (plist-get (car (niwa-session-permission-queue session)) :origin)))
                            (when nested
                              (should (equal "waiting" (plist-get (cl-find "/root/reader" (niwa-agent-control-list-agents session)
                                                                           :key (lambda (row) (plist-get row :path)) :test #'equal)
                                                                  :activity))))
                            (with-current-buffer view
                              (setq permission (seq-find (lambda (overlay) (overlay-get overlay 'niwa-permission-prompt))
                                                         (overlays-in (point-min) (point-max))))
                              (should permission)
                              (should (string-search path (buffer-string)))
                              (goto-char (overlay-start permission))
                              (pcase decision
                                ('allow (call-interactively #'niwa-permission--prompt-approve-once))
                                ('deny (call-interactively #'niwa-permission--prompt-deny-once))
                                ('interrupt (niwa-agent-control-interrupt session path))))
                            (niwa-test--await 8 "Child decision did not wake and settle its parent"
                              (not (or (niwa-turn-busy-p buffer) (niwa-agent-control-active-turn-p session))))
                            (should-not (niwa-session-permission-queue session))
                            (should-not (niwa-agent-control-root-waiting-p session))
                            (should-not (overlay-buffer permission))
                            (should (eq 'success (plist-get (niwa-engine-info request) :niwa-acp-outcome)))
                            (should (= 1 (niwa-session-turn-count session)))
                            (should (= 2 (how-many "^#\\+begin_tool" (point-min) (point-max))))
                            (should (eq (if (eq decision 'interrupt) 'interrupted 'completed)
                                        (niwa-agent-record-settled-outcome record)))
                            (niwa--prompt--settle permission 'allow-once)
                            (with-current-buffer (niwa-agent-record-conversation-buffer record)
                              (should (eq (and (string-search "CHILD-EVIDENCE-72851" (buffer-string)) t)
                                          (eq decision 'allow))))
                            (let (replacement)
                              (niwa-agent-control-spawn session "replacement" "Use the released capacity."
                                                           (lambda (value) (setq replacement value)))
                              (niwa-test--await 8 "Released child capacity was not reusable"
                                replacement)
                              (should (eq 'success (plist-get replacement :outcome)))
                              (niwa-agent-control-interrupt session "/root/replacement")
                              (niwa-test--await 8 "Replacement interruption did not settle"
                                (not (niwa-agent-control-active-turn-p session)))
                              (should-not (niwa-session-permission-queue session))
                              (should (= 1 (niwa-session-turn-count session)))))
                        (niwa-agent-control-teardown-session session))))))

(provide 'test-niwa-claude-code-wait)
;;; test-niwa-claude-code-wait.el ends here
