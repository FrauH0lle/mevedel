;;; test-niwa-acp-compaction.el --- Native compaction segments -*- lexical-binding: t -*-

;;; Commentary:
;; Exercise native summary events through ordinary sends and real publication.

;;; Code:

(require 'niwa-engine-test-support
         (file-name-concat (file-name-directory (or load-file-name buffer-file-name))
                           "niwa-engine-test-support"))
(require 'niwa-claude-code-session)

(niwa-deftest niwa--send-request/native-compaction (:quiet t)
  (niwa-engine-test--with-claude-session
    (let ((events
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
      (setq-local gptel-system-prompt "Compaction fixture" gptel-tools nil)
      (cl-letf (((symbol-function 'niwa-claude-code-launch)
                 (niwa-engine-test--claude-launch
                  (lambda (_system mcp _model _effort &optional id _hook)
                    (list :meta `((compactionEvents . ,events)))))))
        (niwa--insert-user-turn "Submitted before compaction")
        (niwa--send-request "Submitted before compaction")
        (niwa-test--await 5 "Compacted turn did not settle" (not (niwa-turn-busy-p buffer))))
      (should (eq 'success (plist-get (niwa-engine-info request) :niwa-acp-outcome)))
      (should (= 2 (niwa-session-current-segment session)))
      (should (= 1 (niwa-session-turn-count session)))
      (should (string-search "CLEANED CLAUDE SUMMARY" (buffer-string)))
      (should-not (string-search "STREAMED SUMMARY" (buffer-string)))
      (should-not (string-search "ARCHIVED RESPONSE" (buffer-string)))
      (should (string-search "RETAINED TAIL" (buffer-string)))
      (should (string-search "answer:Submitted before compaction" (buffer-string)))
      (let ((archive (niwa-session-artifacts-read-segment session 1)))
        (unwind-protect
            (with-current-buffer archive
              (should (string-search "ARCHIVED RESPONSE" (buffer-string))))
          (kill-buffer archive))))))

(niwa-deftest niwa--send-request/native-compaction-terminal (:quiet t)
  (dolist (status '("cancelled" "failed" "completed" "empty"))
    (niwa-engine-test--with-claude-session
      (setq-local gptel-system-prompt "Compaction fixture" gptel-tools nil)
      (cl-letf (((symbol-function 'niwa-claude-code-launch)
                 (niwa-engine-test--claude-launch
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
        (niwa--insert-user-turn "Original prompt")
        (niwa--send-request "Original prompt")
        (niwa-test--await 5 "Terminal compaction did not settle"
          (not (niwa-turn-busy-p buffer))))
      (should (eq (if (equal status "empty") 'error 'success)
                  (plist-get (niwa-engine-info request) :niwa-acp-outcome)))
      (should (= (if (equal status "completed") 2 1)
                 (niwa-session-current-segment session)))
      (should-not (string-search "LATE SUMMARY" (buffer-string)))
      (when (equal status "completed")
        (should (string-search "STREAMED SUMMARY" (buffer-string)))))))

(niwa-deftest niwa-view-send/native-compaction-reopen (:quiet t)
  (niwa-engine-test--with-claude-session
    (niwa-request-end)
    (let ((view (generate-new-buffer " *native-compaction-view*"))
          (events [((sessionUpdate . "compaction_update") (compactionId . "saved")
                    (status . "completed")
                    (summary . [((type . "text") (text . "DURABLE NATIVE SUMMARY"))]))])
          restored restored-session ids)
      (unwind-protect
          (progn
            (setq-local gptel-system-prompt "Compaction fixture" gptel-tools nil)
            (niwa-chat-install-request-hooks)
            (niwa-view--setup view buffer)
            (cl-letf (((symbol-function 'niwa-claude-code-launch)
                       (niwa-engine-test--claude-launch
                        (lambda (_system mcp _model _effort &optional id _hook)
                          (push id ids)
                          (list :meta `((compactionEvents . ,events)))))))
              (with-current-buffer view
                (niwa-view-composer-insert "Compact this turn")
                (with-current-buffer (niwa-view-composer-buffer)
                  (niwa-view-send))
                (niwa-view-composer-insert "> Preserve my draft\nand its second line"))
              (niwa-test--await 5 "View compaction did not settle"
                (not (or (zerop (niwa-session-turn-count session))
                         (niwa-turn-busy-p buffer))))
              (with-current-buffer view
                (should (equal "> Preserve my draft\nand its second line"
                               (niwa-view-composer-draft-text)))
                ;; The native summary is collapsed, then opens on demand with
                ;; the Claude-labelled preface the model received.
                (should (string-search "conversation compacted" (buffer-string)))
                (should-not (string-search "DURABLE NATIVE SUMMARY" (buffer-string)))
                (save-excursion
                  (goto-char (point-min))
                  (search-forward "conversation compacted")
                  (niwa-view-toggle-section))
                (should (string-search "DURABLE NATIVE SUMMARY" (buffer-string)))
                (should (string-search "retained compaction summary follows"
                                       (buffer-string)))
                (should-not (string-search "begin_summary" (buffer-string)))
                (save-excursion
                  (goto-char (point-min))
                  (search-forward "conversation compacted")
                  (niwa-view-toggle-section))
                (should-not (string-search "DURABLE NATIVE SUMMARY" (buffer-string)))
                (should (equal "> Preserve my draft\nand its second line"
                               (niwa-view-composer-draft-text))))
              (kill-buffer view)
              (let ((path (niwa-session-save-path session)))
                (test-niwa-session-persistence--release-and-kill buffer session)
                (setq restored (niwa-session-persistence-restore path nil nil workspace)))
              (setq events [])
              (with-current-buffer restored
                (setq restored-session niwa--session)
                (should (string-search "DURABLE NATIVE SUMMARY" (buffer-string)))
                (should (= 2 (niwa-session-current-segment restored-session)))
                (goto-char (point-max))
                (niwa--insert-user-turn "Continue after reopen")
                (niwa--send-request "Continue after reopen")
                (niwa-test--await 5 "Compacted history did not resume"
                  (not (niwa-turn-busy-p restored)))
                (should (= 2 (niwa-session-turn-count restored-session)))
                (should (string-search "answer:Continue after reopen" (buffer-string))))
              (should (equal '("fixture-session" nil) ids))))
        (when (buffer-live-p view) (kill-buffer view))
        (when (buffer-live-p restored)
          (test-niwa-session-persistence--release-and-kill restored restored-session))))))

(niwa-deftest niwa--send-request/native-compaction-tools (:quiet t)
  (niwa-engine-test--with-claude-session
    (let ((file (file-name-concat root "evidence.txt")))
      (write-region "Tool evidence" nil file nil 'silent)
      (setq-local gptel-system-prompt "Tool compaction fixture"
                  gptel-tools (list (niwa-tool-gptel-tool (niwa-tool-ensure "Read"))))
      (cl-letf (((symbol-function 'niwa-claude-code-launch)
                 (niwa-engine-test--claude-launch
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
        (niwa--insert-user-turn "Read before and after compaction")
        (niwa--send-request "Read before and after compaction")
        (niwa-test--await 5 "Tool compaction did not settle" (not (niwa-turn-busy-p buffer))))
      (should (eq 'success (plist-get (niwa-engine-info request) :niwa-acp-outcome)))
      (should (= 2 (niwa-session-current-segment session)))
      (should (= 1 (how-many "^#\\+begin_tool" (point-min) (point-max))))
      (should (string-search "after-compact" (buffer-string)))
      (should-not (string-search "before-compact" (buffer-string)))
      ;; The summary replaced the first Read, so the second returns contents.
      (should (string-search "Tool evidence" (buffer-string)))
      (should-not (string-search "unchanged since last read" (buffer-string)))
      (should (or (cl-some (lambda (body) (string-search "Compaction omitted older transcript" body))
                           (niwa-session-pending-reminders session))
                  (< 0 (niwa-engine-test--count-evidence "Compaction omitted older transcript"))))
      (let ((archive (niwa-session-artifacts-read-segment session 1)))
        (unwind-protect
            (with-current-buffer archive
              (should (= 1 (how-many "^#\\+begin_tool" (point-min) (point-max))))
              (should (string-search "before-compact" (buffer-string)))
              (should-not (string-search "after-compact" (buffer-string))))
          (kill-buffer archive))))))

(niwa-deftest niwa-agent-control-spawn/native-compaction (:quiet t)
  (niwa-engine-test--with-claude-session
    (let (record invocation)
      (setq-local gptel-system-prompt "Root fixture" gptel-tools nil)
      (unwind-protect
          (cl-letf (((symbol-function 'niwa-claude-code-launch)
                     (niwa-engine-test--claude-launch
                      (lambda (_system mcp _model _effort &optional id _hook)
                        (list :meta '((compactionEvents .
                                                        [((sessionUpdate . "agent_message_chunk")
                                                          (content . ((type . "text") (text . "CHILD OLD RESPONSE\n"))))
                                                         ((sessionUpdate . "compaction_update") (compactionId . "child")
                                                          (status . "completed")
                                                          (summary . [((type . "text") (text . "CHILD RETAINED SUMMARY"))]))])))))))
            (niwa-agent-control-spawn
             session "compact_child" "Remember the child task."
             (lambda (value) (setq record (plist-get value :record)))
             :agent (niwa-agent--create :name "compact_child" :description "Summarize"
                                          :tools nil :max-turns nil :system-prompt "Child fixture")
             :on-invocation (lambda (value) (setq invocation value)))
            (niwa-test--await 5 "Child compaction did not settle"
              (niwa-agent-invocation-runtime-settled-p invocation))
            (should (eq 'completed (niwa-agent-record-settled-outcome record)))
            (with-current-buffer (niwa-agent-record-conversation-buffer record)
              (should (string-search "CHILD RETAINED SUMMARY" (buffer-string)))
              (should-not (string-search "CHILD OLD RESPONSE" (buffer-string)))
              (should (string-search "Remember the child task" (buffer-string))))
            ;; The next native compaction must not leave only the spawn task.
            (niwa-agent-control-send-message
             session "/root/compact_child" "Freeze the old task. Inspect only the new checkout.")
            (setq invocation (niwa-agent-record-invocation record))
            (niwa-test--await 5 "Follow-up compaction did not settle"
              (niwa-agent-invocation-runtime-settled-p invocation))
            (should (eq 'completed (niwa-agent-record-settled-outcome record)))
            (with-current-buffer (niwa-agent-record-conversation-buffer record)
              (goto-char (point-min))
              ;; A fixture response echo is not a retained assignment.
              (should (re-search-forward
                       "^Freeze the old task\\. Inspect only the new checkout\\.$"
                       nil t)))
            (should (= 1 (niwa-session-current-segment session)))
            (should-not (string-search "CHILD RETAINED SUMMARY" (buffer-string))))
        (when (and record (buffer-live-p (niwa-agent-record-conversation-buffer record)))
          (kill-buffer (niwa-agent-record-conversation-buffer record)))))))

(niwa-deftest niwa--send-request/native-compaction-busy (:quiet t)
  (dolist (abort '(nil t))
  (niwa-engine-test--with-claude-session
    (let ((file (file-name-concat root "evidence.txt"))
          busy timer snapshot hook-seen)
      (write-region "Busy target evidence" nil file nil 'silent)
      (setq-local gptel-system-prompt "Busy compaction fixture"
                  gptel-tools (list (niwa-tool-gptel-tool (niwa-tool-ensure "Read"))))
      (unwind-protect
          (cl-letf (((symbol-function 'niwa-transport-busy-p) (lambda (&optional _path) busy))
                    ((symbol-function 'niwa-claude-code-launch)
                     (niwa-engine-test--claude-launch
                      (lambda (_system mcp _model _effort &optional id hook)
                        (list :observe (lambda (owner notification)
                                         (niwa-claude-code-context-observe owner notification)
                                         (let ((params (alist-get 'params notification)))
                                           (when (equal "hook_response" (alist-get 'subtype (alist-get 'message params)))
                                             (setq hook-seen t))
                                           (when (equal "in_progress" (alist-get 'status (alist-get 'update params)))
                                             (setq busy t
                                                   timer (run-at-time
                                                          .15 nil
                                                          (lambda ()
                                                            (setq snapshot (list (niwa-session-current-segment session)
                                                                                 hook-seen))
                                                            (when abort (niwa-abort buffer))
                                                            (setq busy nil)))))))
                              :meta `((hookCommand . ,hook) (compactBeforeBatch . 0)
                                      (compactionEvents .
                                                        [((sessionUpdate . "compaction_update") (compactionId . "busy") (status . "in_progress"))
                                                         ((sessionUpdate . "compaction_update") (compactionId . "busy") (status . "completed")
                                                          (summary . [((type . "text") (text . "SUMMARY AFTER TARGET IDLE"))]))])
                                      (toolBatches . [[((name . "Read") (id . "after-busy")
                                                        (args . ((file_path . ,file))))]])))))))
            (niwa--insert-user-turn "Wait for the target")
            (niwa--send-request "Wait for the target")
            (niwa-test--await 5 "Deferred compaction did not settle"
              (not (niwa-turn-busy-p buffer)))
            (should (equal '(1 nil) snapshot))
            (should hook-seen)
            (should (eq (if abort 'aborted 'success)
                        (plist-get (niwa-engine-info request) :niwa-acp-outcome)))
            ;; Completed native compaction is history even when the turn aborts.
            (should (= 2 (niwa-session-current-segment session)))
            (should (string-search "SUMMARY AFTER TARGET IDLE" (buffer-string)))
            ;; Cancellation still rejects new tool work.
            (should (eq (not abort) (not (null (string-search "after-busy" (buffer-string)))))))
        (when timer (cancel-timer timer)))))))

(niwa-deftest niwa--send-request/native-compaction-running-command (:quiet t)
  (niwa-engine-test--with-claude-session
    (let ((niwa-execution-event-functions '(niwa-view-stream-handle-execution-event))
          (niwa-execution-mailbox-delivery-function #'niwa-tool-exec-handle-execution-event)
          (unlock (file-name-concat root "unlock")))
      (setq-local gptel-system-prompt "Running command fixture"
                  gptel-tools (list (niwa-tool-gptel-tool (niwa-tool-ensure "Bash"))))
      (unwind-protect
          (cl-letf (((symbol-function 'niwa-claude-code-launch)
                     (niwa-engine-test--claude-launch
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
                                                               (description . "Wait across native compaction")
                                                               (yield_time_ms . 250))))] []])))))))
            (niwa--insert-user-turn "Start a command and compact")
            (niwa--send-request "Start a command and compact")
            (niwa-test--await 5 "Running command turn did not settle"
              (not (niwa-turn-busy-p buffer)))
            (should (eq 'success (plist-get (niwa-engine-info request) :niwa-acp-outcome)))
            (should (= 2 (niwa-session-current-segment session)))
            (ert-info ((niwa-session-artifacts-read-artifact session "segment-0001.chat.org" t))
              (should (= 1 (length (niwa-transcript-audit-records (buffer-string) 'execution-archive)))))
            (write-region "finish" nil unlock nil 'silent)
            (niwa-test--await 5 "Archived command did not publish completion"
              (niwa-transcript-audit-records (buffer-string) 'execution-completion))
            (should-not (niwa-transcript-audit-records (buffer-string) 'execution-archive))
            (let ((records (niwa-transcript-audit-records (buffer-string) 'execution-completion)))
              (should (= 1 (length records)))
              (should (equal "running-bash" (plist-get (car records) :tool-use-id)))))
        (niwa-execution-stop-all-user session)))))

(provide 'test-niwa-acp-compaction)
;;; test-niwa-acp-compaction.el ends here
