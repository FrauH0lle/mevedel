;;; test-mevedel-acp-compaction.el --- Native compaction segments -*- lexical-binding: t -*-

;;; Commentary:
;; Exercise native summary events through ordinary sends and real publication.

;;; Code:

(require 'mevedel-engine-test-support
         (file-name-concat (file-name-directory (or load-file-name buffer-file-name))
                           "mevedel-engine-test-support"))
(require 'mevedel-claude-code-session)

(mevedel-deftest mevedel--send-request/native-compaction (:quiet t)
  (mevedel-engine-test--with-session
    (let ((gptel--known-backends nil)
          (mevedel-claude-code-directory (file-name-concat root "claude"))
          (events
           [((sessionUpdate . "agent_message_chunk")
             (content . ((type . "text") (text . "ARCHIVED RESPONSE\n"))))
            ((sessionUpdate . "compaction_update") (compactionId . "compact-1") (status . "in_progress"))
            ((sessionUpdate . "compaction_summary_chunk") (compactionId . "compact-1")
             (content . ((type . "text") (text . "STREAMED SUMMARY"))))
            ((sessionUpdate . "agent_message_chunk")
             (content . ((type . "text") (text . "RETAINED TAIL\n"))))
            ((sessionUpdate . "compaction_update") (compactionId . "compact-1") (status . "completed")
             (summary . [((type . "text") (text . "CLEANED CLAUDE SUMMARY"))]))
            ((sessionUpdate . "compaction_update") (compactionId . "compact-1") (status . "completed"))]))
      (mevedel-claude-code-register)
      (mevedel-model-set-session-provider
       session (mevedel-model-resolve-provider "Claude Code:sonnet") buffer)
      (setq-local gptel-system-prompt "Compaction fixture" gptel-tools nil)
      (cl-letf (((symbol-function 'mevedel-claude-code-launch)
                 (mevedel-engine-test--claude-launch
                  (lambda (_system mcp _model _effort &optional id _hook)
                    (list :meta `((compactionEvents . ,events)))))))
        (mevedel--insert-user-turn "Submitted before compaction")
        (mevedel--send-request "Submitted before compaction")
        (with-timeout (5 (ert-fail "Compacted turn did not settle"))
          (while (mevedel-turn-busy-p buffer) (accept-process-output nil 0.01))))
      (should (eq 'success (plist-get (mevedel-engine-info request) :mevedel-acp-outcome)))
      (should (= 2 (mevedel-session-current-segment session)))
      (should (= 1 (mevedel-session-turn-count session)))
      (should (string-search "CLEANED CLAUDE SUMMARY" (buffer-string)))
      (should-not (string-search "STREAMED SUMMARY" (buffer-string)))
      (should-not (string-search "ARCHIVED RESPONSE" (buffer-string)))
      (should (string-search "RETAINED TAIL" (buffer-string)))
      (should (string-search "answer:Submitted before compaction" (buffer-string)))
      (let ((archive (mevedel-session-artifacts-read-segment session 1)))
        (unwind-protect
            (with-current-buffer archive
              (should (string-search "ARCHIVED RESPONSE" (buffer-string))))
          (kill-buffer archive))))))

(mevedel-deftest mevedel--send-request/native-compaction-terminal (:quiet t)
  (dolist (status '("cancelled" "failed" "completed" "empty"))
    (mevedel-engine-test--with-session
      (let ((gptel--known-backends nil)
            (mevedel-claude-code-directory (file-name-concat root "claude")))
        (mevedel-claude-code-register)
        (mevedel-model-set-session-provider
         session (mevedel-model-resolve-provider "Claude Code:sonnet") buffer)
        (setq-local gptel-system-prompt "Compaction fixture" gptel-tools nil)
        (cl-letf (((symbol-function 'mevedel-claude-code-launch)
                   (mevedel-engine-test--claude-launch
                    (lambda (_system mcp _model _effort &optional id _hook)
                      (list :meta `((compactionEvents .
                                                      [((sessionUpdate . "compaction_update") (compactionId . "terminal")
                                                        (status . "in_progress"))
                                                       ((sessionUpdate . "compaction_summary_chunk") (compactionId . "terminal")
                                                        (content . ((type . "text") (text . ,(if (equal status "empty") "" "STREAMED SUMMARY")))))
                                                       ((sessionUpdate . "compaction_update") (compactionId . "terminal")
                                                        (status . ,(if (equal status "empty") "completed" status)))
                                                       ((sessionUpdate . "compaction_update") (compactionId . "terminal")
                                                        (status . "completed")
                                                        (summary . [((type . "text") (text . "LATE SUMMARY"))]))])))))))
          (mevedel--insert-user-turn "Original prompt")
          (mevedel--send-request "Original prompt")
          (with-timeout (5 (ert-fail "Terminal compaction did not settle"))
            (while (mevedel-turn-busy-p buffer) (accept-process-output nil 0.01))))
        (should (eq (if (equal status "empty") 'error 'success)
                    (plist-get (mevedel-engine-info request) :mevedel-acp-outcome)))
        (should (= (if (equal status "completed") 2 1)
                   (mevedel-session-current-segment session)))
        (should-not (string-search "LATE SUMMARY" (buffer-string)))
        (when (equal status "completed")
          (should (string-search "STREAMED SUMMARY" (buffer-string))))))))

(mevedel-deftest mevedel-view-send/native-compaction-reopen (:quiet t)
  (mevedel-engine-test--with-session
    (mevedel-request-end)
    (let ((gptel--known-backends nil)
          (view (generate-new-buffer " *native-compaction-view*"))
          (mevedel-claude-code-directory (file-name-concat root "claude"))
          (events [((sessionUpdate . "compaction_update") (compactionId . "saved")
                    (status . "completed")
                    (summary . [((type . "text") (text . "DURABLE NATIVE SUMMARY"))]))])
          restored restored-session ids)
      (unwind-protect
          (progn
            (mevedel-claude-code-register)
            (mevedel-model-set-session-provider
             session (mevedel-model-resolve-provider "Claude Code:sonnet") buffer)
            (setq-local gptel-system-prompt "Compaction fixture" gptel-tools nil)
            (mevedel-chat-install-request-hooks)
            (mevedel-view--setup view buffer)
            (cl-letf (((symbol-function 'mevedel-claude-code-launch)
                       (mevedel-engine-test--claude-launch
                        (lambda (_system mcp _model _effort &optional id _hook)
                          (push id ids)
                          (list :meta `((compactionEvents . ,events)))))))
              (with-current-buffer view
                (goto-char (mevedel-view--input-start))
                (insert "Compact this turn")
                (mevedel-view-send)
                (goto-char (mevedel-view--input-start))
                (insert "> Preserve my draft\nand its second line"))
              (with-timeout (5 (ert-fail "View compaction did not settle"))
                (while (or (zerop (mevedel-session-turn-count session))
                           (mevedel-turn-busy-p buffer))
                  (accept-process-output nil 0.01)))
              (with-current-buffer view
                (should (equal "> Preserve my draft\nand its second line"
                               (mevedel-view--input-text))))
              (kill-buffer view)
              (let ((path (mevedel-session-save-path session)))
                (test-mevedel-session-persistence--release-and-kill buffer session)
                (setq restored (mevedel-session-persistence-restore path nil nil workspace)))
              (setq events [])
              (with-current-buffer restored
                (setq restored-session mevedel--session)
                (should (string-search "DURABLE NATIVE SUMMARY" (buffer-string)))
                (should (= 2 (mevedel-session-current-segment restored-session)))
                (goto-char (point-max))
                (mevedel--insert-user-turn "Continue after reopen")
                (mevedel--send-request "Continue after reopen")
                (with-timeout (5 (ert-fail "Compacted history did not resume"))
                  (while (mevedel-turn-busy-p restored) (accept-process-output nil 0.01)))
                (should (= 2 (mevedel-session-turn-count restored-session)))
                (should (string-search "answer:Continue after reopen" (buffer-string))))
              (should (equal '("fixture-session" nil) ids))))
        (when (buffer-live-p view) (kill-buffer view))
        (when (buffer-live-p restored)
          (test-mevedel-session-persistence--release-and-kill restored restored-session))))))

(mevedel-deftest mevedel--send-request/native-compaction-tools (:quiet t)
  (mevedel-engine-test--with-session
    (let ((gptel--known-backends nil)
          (mevedel-claude-code-directory (file-name-concat root "claude"))
          (file (file-name-concat root "evidence.txt")))
      (write-region "Tool evidence" nil file nil 'silent)
      (mevedel-claude-code-register)
      (mevedel-model-set-session-provider
       session (mevedel-model-resolve-provider "Claude Code:sonnet") buffer)
      (setq-local gptel-system-prompt "Tool compaction fixture"
                  gptel-tools (list (mevedel-tool-gptel-tool (mevedel-tool-ensure "Read"))))
      (cl-letf (((symbol-function 'mevedel-claude-code-launch)
                 (mevedel-engine-test--claude-launch
                  (lambda (_system mcp _model _effort &optional id hook)
                    (list :meta `((hookCommand . ,hook) (compactBeforeBatch . 1)
                                  (compactionBeforeBatch . 1)
                                  (compactionEvents .
                                                    [((sessionUpdate . "compaction_update") (compactionId . "tools")
                                                      (status . "completed")
                                                      (summary . [((type . "text") (text . "Read evidence before compaction."))]))])
                                  (toolBatches . [[((name . "Read") (id . "before-compact")
                                                    (args . ((file_path . ,file))))]
                                                  [((name . "Read") (id . "after-compact")
                                                    (args . ((file_path . ,file))))]])))))))
        (mevedel--insert-user-turn "Read before and after compaction")
        (mevedel--send-request "Read before and after compaction")
        (with-timeout (5 (ert-fail "Tool compaction did not settle"))
          (while (mevedel-turn-busy-p buffer) (accept-process-output nil 0.01))))
      (should (eq 'success (plist-get (mevedel-engine-info request) :mevedel-acp-outcome)))
      (should (= 2 (mevedel-session-current-segment session)))
      (should (= 1 (how-many "^#\\+begin_tool" (point-min) (point-max))))
      (should (string-search "after-compact" (buffer-string)))
      (should-not (string-search "before-compact" (buffer-string)))
      ;; The summary replaced the first Read, so the second returns contents.
      (should (string-search "Tool evidence" (buffer-string)))
      (should-not (string-search "unchanged since last read" (buffer-string)))
      (should (or (cl-some (lambda (body) (string-search "Compaction omitted older transcript" body))
                           (mevedel-session-pending-reminders session))
                  (< 0 (mevedel-engine-test--count-evidence "Compaction omitted older transcript"))))
      (let ((archive (mevedel-session-artifacts-read-segment session 1)))
        (unwind-protect
            (with-current-buffer archive
              (should (= 1 (how-many "^#\\+begin_tool" (point-min) (point-max))))
              (should (string-search "before-compact" (buffer-string)))
              (should-not (string-search "after-compact" (buffer-string))))
          (kill-buffer archive))))))

(mevedel-deftest mevedel-agent-control-spawn/native-compaction (:quiet t)
  (mevedel-engine-test--with-session
    (let ((gptel--known-backends nil)
          (mevedel-claude-code-directory (file-name-concat root "claude"))
          record invocation)
      (mevedel-claude-code-register)
      (mevedel-model-set-session-provider
       session (mevedel-model-resolve-provider "Claude Code:sonnet") buffer)
      (setq-local gptel-system-prompt "Root fixture" gptel-tools nil)
      (unwind-protect
          (cl-letf (((symbol-function 'mevedel-claude-code-launch)
                     (mevedel-engine-test--claude-launch
                      (lambda (_system mcp _model _effort &optional id _hook)
                        (list :meta '((compactionEvents .
                                                        [((sessionUpdate . "agent_message_chunk")
                                                          (content . ((type . "text") (text . "CHILD OLD RESPONSE\n"))))
                                                         ((sessionUpdate . "compaction_update") (compactionId . "child")
                                                          (status . "completed")
                                                          (summary . [((type . "text") (text . "CHILD RETAINED SUMMARY"))]))])))))))
            (mevedel-agent-control-spawn
             session "compact_child" "Remember the child task."
             (lambda (value) (setq record (plist-get value :record)))
             :agent (mevedel-agent--create :name "compact_child" :description "Summarize"
                                          :tools nil :max-turns nil :system-prompt "Child fixture")
             :on-invocation (lambda (value) (setq invocation value)))
            (with-timeout (5 (ert-fail "Child compaction did not settle"))
              (while (not (mevedel-agent-invocation-runtime-settled-p invocation))
                (accept-process-output nil 0.01)))
            (should (eq 'completed (mevedel-agent-record-settled-outcome record)))
            (with-current-buffer (mevedel-agent-record-conversation-buffer record)
              (should (string-search "CHILD RETAINED SUMMARY" (buffer-string)))
              (should-not (string-search "CHILD OLD RESPONSE" (buffer-string)))
              (should (string-search "Remember the child task" (buffer-string))))
            (should (= 1 (mevedel-session-current-segment session)))
            (should-not (string-search "CHILD RETAINED SUMMARY" (buffer-string))))
        (when (and record (buffer-live-p (mevedel-agent-record-conversation-buffer record)))
          (kill-buffer (mevedel-agent-record-conversation-buffer record)))))))

(mevedel-deftest mevedel--send-request/native-compaction-busy (:quiet t)
  (dolist (abort '(nil t))
  (mevedel-engine-test--with-session
    (let ((gptel--known-backends nil)
          (mevedel-claude-code-directory (file-name-concat root "claude"))
          (file (file-name-concat root "evidence.txt"))
          busy timer snapshot hook-seen)
      (write-region "Busy target evidence" nil file nil 'silent)
      (mevedel-claude-code-register)
      (mevedel-model-set-session-provider
       session (mevedel-model-resolve-provider "Claude Code:sonnet") buffer)
      (setq-local gptel-system-prompt "Busy compaction fixture"
                  gptel-tools (list (mevedel-tool-gptel-tool (mevedel-tool-ensure "Read"))))
      (unwind-protect
          (cl-letf (((symbol-function 'mevedel-transport-busy-p) (lambda (&optional _path) busy))
                    ((symbol-function 'mevedel-claude-code-launch)
                     (mevedel-engine-test--claude-launch
                      (lambda (_system mcp _model _effort &optional id hook)
                        (list :observe (lambda (owner notification)
                                         (mevedel-claude-code-context-observe owner notification)
                                         (let ((params (alist-get 'params notification)))
                                           (when (equal "hook_response" (alist-get 'subtype (alist-get 'message params)))
                                             (setq hook-seen t))
                                           (when (equal "in_progress" (alist-get 'status (alist-get 'update params)))
                                             (setq busy t
                                                   timer (run-at-time
                                                          .15 nil
                                                          (lambda ()
                                                            (setq snapshot (list (mevedel-session-current-segment session)
                                                                                 hook-seen))
                                                            (when abort (mevedel-abort buffer))
                                                            (setq busy nil)))))))
                              :meta `((hookCommand . ,hook) (compactBeforeBatch . 0)
                                      (compactionEvents .
                                                        [((sessionUpdate . "compaction_update") (compactionId . "busy") (status . "in_progress"))
                                                         ((sessionUpdate . "compaction_update") (compactionId . "busy") (status . "completed")
                                                          (summary . [((type . "text") (text . "SUMMARY AFTER TARGET IDLE"))]))])
                                      (toolBatches . [[((name . "Read") (id . "after-busy")
                                                        (args . ((file_path . ,file))))]])))))))
            (mevedel--insert-user-turn "Wait for the target")
            (mevedel--send-request "Wait for the target")
            (with-timeout (5 (ert-fail "Deferred compaction did not settle"))
              (while (mevedel-turn-busy-p buffer) (accept-process-output nil 0.01)))
            (should (equal '(1 nil) snapshot))
            (should hook-seen)
            (should (eq (if abort 'aborted 'success)
                        (plist-get (mevedel-engine-info request) :mevedel-acp-outcome)))
            ;; Completed native compaction is history even when the turn aborts.
            (should (= 2 (mevedel-session-current-segment session)))
            (should (string-search "SUMMARY AFTER TARGET IDLE" (buffer-string)))
            ;; Cancellation still rejects new tool work.
            (should (eq (not abort) (not (null (string-search "after-busy" (buffer-string)))))))
        (when timer (cancel-timer timer)))))))

(mevedel-deftest mevedel--send-request/native-compaction-running-command (:quiet t)
  (mevedel-engine-test--with-session
    (let ((gptel--known-backends nil)
          (mevedel-execution-event-functions '(mevedel-view-stream-handle-execution-event))
          (mevedel-execution-mailbox-delivery-function #'mevedel-tool-exec-handle-execution-event)
          (mevedel-claude-code-directory (file-name-concat root "claude"))
          (unlock (file-name-concat root "unlock")))
      (mevedel-claude-code-register)
      (mevedel-model-set-session-provider
       session (mevedel-model-resolve-provider "Claude Code:sonnet") buffer)
      (setq-local gptel-system-prompt "Running command fixture"
                  gptel-tools (list (mevedel-tool-gptel-tool (mevedel-tool-ensure "Bash"))))
      (unwind-protect
          (cl-letf (((symbol-function 'mevedel-claude-code-launch)
                     (mevedel-engine-test--claude-launch
                      (lambda (_system mcp _model _effort &optional id _hook)
                        (list :meta `((compactionBeforeBatch . 1)
                                      (compactionEvents .
                                                        [((sessionUpdate . "compaction_update") (compactionId . "running")
                                                          (status . "completed")
                                                          (summary . [((type . "text") (text . "Bash is still running."))]))])
                                      (toolBatches .
                                                   [[((name . "Bash") (id . "running-bash")
                                                      (args . ((command . ,(format "while test ! -e %s; do sleep .01; done; printf 'COMPLETED ONCE'"
                                                                                   (shell-quote-argument unlock)))
                                                               (yield_time_ms . 250))))] []])))))))
            (mevedel--insert-user-turn "Start a command and compact")
            (mevedel--send-request "Start a command and compact")
            (with-timeout (5 (ert-fail "Running command turn did not settle"))
              (while (mevedel-turn-busy-p buffer) (accept-process-output nil 0.01)))
            (should (eq 'success (plist-get (mevedel-engine-info request) :mevedel-acp-outcome)))
            (should (= 2 (mevedel-session-current-segment session)))
            (ert-info ((mevedel-session-artifacts-read-artifact session "segment-0001.chat.org" t))
              (should (= 1 (length (mevedel-transcript-audit-records (buffer-string) 'execution-archive)))))
            (write-region "finish" nil unlock nil 'silent)
            (with-timeout (5 (ert-fail "Archived command did not publish completion"))
              (while (not (mevedel-transcript-audit-records (buffer-string) 'execution-completion))
                (accept-process-output nil 0.01)))
            (should-not (mevedel-transcript-audit-records (buffer-string) 'execution-archive))
            (let ((records (mevedel-transcript-audit-records (buffer-string) 'execution-completion)))
              (should (= 1 (length records)))
              (should (equal "running-bash" (plist-get (car records) :tool-use-id)))))
        (mevedel-execution-stop-all-user session)))))

(provide 'test-mevedel-acp-compaction)
;;; test-mevedel-acp-compaction.el ends here
