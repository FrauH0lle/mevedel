;;; test-niwa-claude-code-agent.el --- Native retained agent limits -*- lexical-binding: t -*-

;;; Commentary:
;; Exercise limits and follow-ups through retained-agent control.

;;; Code:

(require 'niwa-engine-test-support
         (file-name-concat (file-name-directory (or load-file-name buffer-file-name))
                           "niwa-engine-test-support"))
(require 'niwa-claude-code-agent)
(require 'niwa-claude-code-session)

(niwa-deftest niwa-agent-control-spawn/claude-sample-limit (:quiet t)
  (dolist (limit '(1 3 8 nil))
    (niwa-engine-test--with-claude-session
      :file-workspace (not (equal limit 1))
      (let ((file (file-name-concat root "evidence.txt"))
            (launches 0) record invocation)
        (write-region "Evidence" nil file nil 'silent)
        (dolist (name '("Read" "ListAgents" "SendMessage")) (niwa-tool-ensure name))
        (unwind-protect
            (cl-letf (((symbol-function 'niwa-claude-code-launch)
                       (niwa-engine-test--claude-launch
                        (lambda (_system mcp _model _effort &optional id hook)
                          (cl-incf launches)
                          (list :meta `((hookCommand . ,hook) (suppressResponseOnStop . t)
                                        (compactBeforeBatch . ,(when (equal limit 3) 2))
                                        (responseText . "All work completed")
                                        (toolBatches . ,(vconcat
                                                         (cl-loop for batch below 5 collect
                                                                  (vconcat
                                                                   (cl-loop for tool below 2 collect
                                                                            `((name . "Read")
                                                                              (id . ,(format "%d-%d-%d" launches batch tool))
                                                                              (args . ((file_path . ,file)))))))))))))))
              (dotimes (phase 2)
                (if (= phase 0)
                    (niwa-agent-control-spawn
                     session "limited" "Read the evidence in several steps."
                     (lambda (value) (setq record (plist-get value :record)))
                     :agent (niwa-agent--create :name "limited" :description "Read evidence"
                                                  :tools '(Read) :max-turns limit
                                                  :system-prompt "Read evidence")
                     :on-invocation (lambda (value) (setq invocation value)))
                  (niwa-agent-control-send-message session "/root/limited" "Continue the investigation.")
                  (setq invocation (niwa-agent-record-invocation record)))
                (niwa-test--await 10 "Limited child did not settle"
                  (niwa-agent-invocation-runtime-settled-p invocation))
                (ert-info ((format "limit=%S phase=%d" limit phase))
                  (should (= (min (or limit 6) 6) (niwa-agent-invocation-turn-count invocation)))
                  (should (eq 'completed (niwa-agent-record-settled-outcome record)))
                  (if (and limit (<= limit 5))
                      (should (string-search (format "%d-turn limit was reached" limit)
                                             (niwa-agent-record-settled-result record)))
                    (should (string-suffix-p "All work completed" (niwa-agent-record-settled-result record))))
                  (with-current-buffer (niwa-agent-record-conversation-buffer record)
                    (should (= (* (1+ phase) 2 (min (or limit 5) 5))
                               (how-many "^#\\+begin_tool" (point-min) (point-max))))
                    (when (and limit (<= limit 5))
                      (should (= (* (1+ phase) (if (equal limit 3) 2 1))
                                 (niwa-engine-test--count-evidence "This is your final turn"))))
                    (when (equal limit 3)
                      (should (= (1+ phase) (niwa-engine-test--count-evidence "You have used 2 of 3 turns"))))))))
          (when (and record (buffer-live-p (niwa-agent-record-conversation-buffer record)))
            (kill-buffer (niwa-agent-record-conversation-buffer record))))))))

(niwa-deftest niwa-agent-control-send-message/claude-edited-history (:quiet t)
  (dolist (phase '(completed compacted active reopened))
    (niwa-engine-test--with-claude-session
      (niwa-request-end)
      (let ((launches 0) record invocation receipt restored restored-session)
        (unwind-protect
            (cl-letf (((symbol-function 'niwa-claude-code-launch)
                       (niwa-engine-test--claude-launch
                        (lambda (_system mcp _model _effort &optional id _hook)
                          (cl-incf launches)
                          (list :observe (lambda (owner event)
                                           (niwa-claude-code-context-observe owner event)
                                           (when (equal "user" (alist-get 'type (alist-get 'message (alist-get 'params event))))
                                             (setq receipt t)))
                                :meta `((echoAllText . t)
                                        (compactionEvents .
                                                          ,(if (eq phase 'compacted)
                                                               [((sessionUpdate . "agent_message_chunk")
                                                                 (content . ((type . "text") (text . "Prior response\n"))))
                                                                ((sessionUpdate . "compaction_update") (compactionId . "child")
                                                                 (status . "completed")
                                                                 (summary . [((type . "text") (text . "RETAINED CHILD SUMMARY"))]))]
                                                             []))))))))
              (niwa-agent-control-spawn
               session "editor" (if (memq phase '(compacted active reopened)) "wait-silent" "SUBMITTED CHILD INPUT")
               (lambda (value) (setq record (plist-get value :record)))
               :agent (niwa-agent--create :name "editor" :description "Respond"
                                            :tools nil :system-prompt "Respond")
               :on-invocation (lambda (value) (setq invocation value)))
              (niwa-test--await 5 "Child did not acknowledge submitted input" receipt)
              (unless (memq phase '(compacted active reopened))
                (niwa-test--await 5 "Child did not settle"
                  (niwa-agent-invocation-runtime-settled-p invocation))
                (should (eq 'completed (niwa-agent-record-settled-outcome record))))
              (with-current-buffer (niwa-agent-record-conversation-buffer record)
                (should-not buffer-read-only)
                (gptel--save-state)
                (gptel--save-state)
                (goto-char (point-max))
                (insert "\nnext drafX")
                (delete-char -1)
                (insert "t")
                (should-not (eq 'diverged (plist-get (alist-get "/root/editor" (niwa-session-external-conversations session) nil nil #'equal) :state))))
              (when (eq phase 'reopened)
                (niwa-agent-control-interrupt session "/root/editor")
                (niwa-test--await 5 "Child interrupt did not settle"
                  (not (niwa-agent-control-active-turn-p session)))
                (let ((path (niwa-session-save-path session)))
                  (niwa-agent-control-teardown-session session)
                  (test-niwa-session-persistence--release-and-kill buffer session)
                  (setq restored (niwa-session-persistence-restore path nil nil workspace)
                        restored-session (buffer-local-value 'niwa--session restored)
                        session restored-session
                        record (niwa-agent-control--record-at-path session "/root/editor")))
                (niwa-agent-persistence-ensure-conversation session record restored))
              ;; A child edit must not poison the independent root scope.
              (setf (alist-get "root" (niwa-session-external-conversations session) nil nil #'equal)
                    (list :engine 'claude-code :id "root-native" :host (system-name)
                          :directory niwa-claude-code-directory :state 'ready))
              (with-current-buffer (niwa-agent-record-conversation-buffer record)
                (goto-char (point-min))
                (search-forward (pcase phase ('compacted "RETAINED CHILD SUMMARY")
                                  ((or 'active 'reopened) "wait-silent") (_ "SUBMITTED CHILD INPUT")))
                (replace-match "USER EDITED EVIDENCE" t t)
                (ert-info ((format "phase=%S" phase))
                  (should (string-search "USER EDITED EVIDENCE"
                                         (if (eq phase 'compacted)
                                             (niwa-claude-code-history-excerpt session t)
                                           (niwa-engine-test--evidence)))))
                (should (eq 'diverged (plist-get (alist-get "/root/editor" (niwa-session-external-conversations session) nil nil #'equal) :state)))
                (should (eq 'ready (plist-get (alist-get "root" (niwa-session-external-conversations session) nil nil #'equal) :state))))
              (when (memq phase '(compacted active))
                (niwa-agent-control-interrupt session "/root/editor")
                (niwa-test--await 5 "Edited child interrupt did not settle"
                  (not (niwa-agent-control-active-turn-p session))))
              (should (eq 'diverged (plist-get (alist-get "/root/editor" (niwa-session-external-conversations session) nil nil #'equal) :state)))
              (with-current-buffer (or restored buffer)
                (should-error (niwa-agent-control-send-message session "/root/editor" "Continue using my edited evidence.") :type 'user-error))
              (should (= 1 launches))
              (with-current-buffer (or restored buffer)
                (niwa-claude-code-recover-history "/root/editor")
                (niwa-agent-control-send-message session "/root/editor" "Continue using my edited evidence."))
              (niwa-test--await 5 "Recovered child did not settle"
                (not (niwa-agent-control-active-turn-p session)))
              (should (= 2 launches))
              (should (eq 'completed (niwa-agent-record-settled-outcome record)))
              (should (string-search "Excerpt continuation" (niwa-agent-record-settled-result record)))
              (should (string-search "USER EDITED EVIDENCE" (niwa-agent-record-settled-result record))))
          (when (and record (buffer-live-p (niwa-agent-record-conversation-buffer record)))
            (kill-buffer (niwa-agent-record-conversation-buffer record)))
          (niwa-agent-control-teardown-session session)
          (when (buffer-live-p restored)
            (test-niwa-session-persistence--release-and-kill restored restored-session)))))))

(niwa-deftest niwa-agent-control-spawn/claude-success-preserves-divergence (:quiet t)
  (niwa-engine-test--with-claude-session
    (niwa-request-end)
    (let (record invocation edited)
      (unwind-protect
          (cl-letf (((symbol-function 'niwa-claude-code-launch)
                     (niwa-engine-test--claude-launch
                      (lambda (_system mcp _model _effort &optional id _hook)
                        (list :observe (lambda (owner event)
                                         (niwa-claude-code-context-observe owner event)
                                         (when (and (not edited)
                                                    (equal "user" (alist-get 'type (alist-get 'message (alist-get 'params event)))))
                                           (setq edited t)
                                           (with-current-buffer (niwa-agent-invocation-buffer owner)
                                             (goto-char (point-min))
                                             (search-forward "SUBMITTED CHILD INPUT")
                                             (replace-match "USER EDITED INPUT" t t)))))))))
            (niwa-agent-control-spawn
             session "editor" "SUBMITTED CHILD INPUT"
             (lambda (value) (setq record (plist-get value :record)))
             :agent (niwa-agent--create :name "editor" :description "Respond"
                                          :tools nil :system-prompt "Respond")
             :on-invocation (lambda (value) (setq invocation value)))
            (niwa-test--await 5 "Edited successful child did not settle"
              (niwa-agent-invocation-runtime-settled-p invocation))
            (should edited)
            (should (eq 'completed (niwa-agent-record-settled-outcome record)))
            (should (eq 'diverged (plist-get (alist-get "/root/editor" (niwa-session-external-conversations session) nil nil #'equal) :state))))
        (when (and record (buffer-live-p (niwa-agent-record-conversation-buffer record)))
          (kill-buffer (niwa-agent-record-conversation-buffer record)))
        (niwa-agent-control-teardown-session session)))))

(provide 'test-niwa-claude-code-agent)
;;; test-niwa-claude-code-agent.el ends here
