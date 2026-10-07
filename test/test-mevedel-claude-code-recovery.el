;;; test-mevedel-claude-code-recovery.el --- Explicit native history recovery -*- lexical-binding: t -*-

;;; Commentary:
;; Recover missing native conversations through user controls and ordinary sends.

;;; Code:

(require 'mevedel-engine-test-support
         (file-name-concat (file-name-directory (or load-file-name buffer-file-name))
                           "mevedel-engine-test-support"))
(require 'mevedel-claude-code-session)

(mevedel-deftest mevedel-claude-code-history-assert-current ()
  (let ((mevedel-claude-code-directory (make-temp-file "mevedel-history-" t)))
    (unwind-protect
        (let ((record (list :engine 'claude-code :id "native" :host (system-name)
                            :directory (expand-file-name mevedel-claude-code-directory)
                            :state 'ready)))
          (should-not (mevedel-claude-code-history-assert-current nil))
          (should-not (mevedel-claude-code-history-assert-current
                       '(:engine claude-code :state unstarted)))
          (should-not (mevedel-claude-code-history-assert-current record))
          (should (string-search "recover-history"
                                 (cadr (should-error (mevedel-claude-code-history-assert-current
                                                      (plist-put (copy-sequence record) :state 'diverged))
                                                     :type 'user-error))))
          (dolist (foreign (list (plist-put (copy-sequence record) :host "other-machine")
                                 (plist-put (copy-sequence record) :directory "/other/installation")))
            (let ((message (cadr (should-error (mevedel-claude-code-history-assert-current foreign)
                                               :type 'user-error))))
              (should (string-search "another installation" message))
              (should (string-search "M-x mevedel-claude-code-recover-history" message)))))
      (delete-directory mevedel-claude-code-directory t))))

(mevedel-deftest mevedel-claude-code-history-open ()
  (let* ((mevedel-claude-code-directory "/tmp/mevedel-history-open")
         (owner (mevedel-request--create))
         (record (mevedel-claude-code-history-open owner "native" '(1 . 4))))
    (should (equal (list :engine 'claude-code :id "native" :host (system-name)
                         :directory "/tmp/mevedel-history-open" :state 'in-flight
                         :input-boundary '(1 . 4))
                   record))
    (should (eq record (plist-get (mevedel-engine-info owner) :mevedel-claude-history)))
    (should-not (plist-member (mevedel-claude-code-history-open owner "child") :input-boundary))))

(mevedel-deftest mevedel-claude-code-history-settle ()
  (let ((owner (mevedel-request--create)))
    (dolist (case '((success ready) (error uncertain) (interrupted uncertain)))
      (let ((record (list :id "native" :state 'in-flight)))
        (mevedel-claude-code-history-settle owner nil record (list :status (car case)))
        (should (eq (cadr case) (plist-get record :state)))))
    (let ((record (list :id "native" :state 'diverged)))
      (mevedel-claude-code-history-settle owner nil record '(:status success))
      (should (eq 'diverged (plist-get record :state))))
    ;; A retained identity that failed to start names excerpt recovery.
    (setf (mevedel-engine-info owner) (list :mevedel-acp-outcome 'error :error "Missing history"))
    (mevedel-claude-code-history-settle owner '(:id "native") nil '(:status error))
    (should (string-search "recover-history" (plist-get (mevedel-engine-info owner) :error)))
    (setf (mevedel-engine-info owner) (list :mevedel-acp-outcome 'error :error "Startup failed"))
    (mevedel-claude-code-history-settle owner nil nil '(:status error))
    (should (equal "Startup failed" (plist-get (mevedel-engine-info owner) :error)))))

(mevedel-deftest mevedel-claude-code-recover-history/root (:quiet t)
  (mevedel-engine-test--with-claude-session
    (let ((view (generate-new-buffer " *claude-recovery*"))
          (file (file-name-concat root "evidence.txt"))
          (launches 0) ids)
      (unwind-protect
          (progn
            (write-region "RETAINED TOOL EVIDENCE" nil file nil 'silent)
            (setq-local gptel-system-prompt "Recovery fixture"
                        gptel-tools (list (mevedel-tool-gptel-tool (mevedel-tool-ensure "Read"))))
            (mevedel-view--setup view buffer)
            (cl-letf (((symbol-function 'mevedel-claude-code-launch)
                       (mevedel-engine-test--claude-launch
                        (lambda (_system mcp _model _effort &optional id hook)
                          (cl-incf launches)
                          (push id ids)
                          (list :meta (append `((hookCommand . ,hook)
                                                (fixtureSessionId . ,(if (= launches 1) "original" "replacement"))
                                                (echoAllText . t))
                                              (when (= launches 1)
                                                `((toolBatches . [[((name . "Read") (id . "old-read")
                                                                    (args . ((file_path . ,file))))]])))))))))
              (mevedel--insert-user-turn "Read this evidence")
              (mevedel--send-request "Read this evidence")
              (mevedel-test--await 5 "Initial native turn did not settle"
                (not (mevedel-turn-busy-p buffer)))
              (goto-char (point-max))
              (mevedel--insert-user-turn "Continue the work")
              (setq request (mevedel--send-request "Continue the work"))
              (mevedel-test--await 5 "Missing native history did not fail"
                (not (mevedel-turn-busy-p buffer)))
              (should (eq 'error (plist-get (mevedel-engine-info request) :mevedel-acp-outcome)))
              (should (string-search "Missing history" (plist-get (mevedel-engine-info request) :error)))
              (should (string-search "mevedel-claude-code-recover-history" (plist-get (mevedel-engine-info request) :error)))
              (should (string-search "RETAINED TOOL EVIDENCE" (buffer-string)))
              (let ((before (buffer-string)))
                (with-current-buffer view (mevedel-claude-code-recover-history))
                (should (equal before (buffer-string))))
              (should-not (assoc "root" (mevedel-session-external-conversations session)))
              (let ((metadata (mevedel-session-codec-read
                               (mevedel-session-artifacts-sidecar-path (mevedel-session-save-path session)))))
                (should-not (assoc "root" (plist-get metadata :external-conversations))))
              (goto-char (point-max))
              (mevedel--insert-user-turn "Recover from retained evidence")
              (mevedel--send-request "Recover from retained evidence")
              (mevedel-test--await 5 "Recovered native turn did not settle"
                (not (mevedel-turn-busy-p buffer)))
              (should (equal '(nil "original" nil) ids))
              (should (string-search "answer:Excerpt continuation" (buffer-string)))
              (should (string-search "not an exact native resume" (buffer-string)))
              (should (= 1 (how-many "^#\\+begin_tool" (point-min) (point-max))))
              (should (equal "replacement" (plist-get (cdr (assoc "root" (mevedel-session-external-conversations session))) :id)))
              (goto-char (point-max))
              (mevedel--insert-user-turn "Continue after recovery")
              (mevedel--send-request "Continue after recovery")
              (mevedel-test--await 5 "Recovered history did not resume"
                (not (mevedel-turn-busy-p buffer)))
              (should (equal "replacement" (car ids)))))
        (when (buffer-live-p view) (kill-buffer view))))))

(mevedel-deftest mevedel-claude-code-recover-history/child (:quiet t)
  (mevedel-engine-test--with-claude-session
    (mevedel-request-end)
    (let ((launches 0) record invocation ids)
      (unwind-protect
          (cl-letf (((symbol-function 'mevedel-claude-code-launch)
                     (mevedel-engine-test--claude-launch
                      (lambda (_system mcp _model _effort &optional id _hook)
                        (cl-incf launches)
                        (push id ids)
                        (list :meta `((fixtureSessionId . ,(if (= launches 1) "original" "replacement"))
                                      (echoAllText . t)))))))
            (mevedel-agent-control-spawn
             session "reader" "Keep CHILD RECOVERY EVIDENCE for later."
             (lambda (value) (setq record (plist-get value :record)))
             :agent (mevedel-agent--create :name "reader" :description "Read evidence"
                                          :tools nil :system-prompt "Recovery fixture")
             :on-invocation (lambda (value) (setq invocation value)))
            (mevedel-test--await 5 "Child initial turn did not settle"
              (mevedel-agent-invocation-runtime-settled-p invocation))
            (mevedel-agent-control-followup session "/root/reader" "Continue the task")
            (setq invocation (mevedel-agent-record-invocation record))
            (mevedel-test--await 5 "Missing child history did not settle"
              (mevedel-agent-invocation-runtime-settled-p invocation))
            (should (eq 'errored (mevedel-agent-record-settled-outcome record)))
            (should (string-search "mevedel-claude-code-recover-history"
                                   (mevedel-agent-record-settled-result record)))
            (mevedel-claude-code-recover-history "/root/reader")
            (mevedel-agent-control-followup session "/root/reader" "Use the retained evidence")
            (setq invocation (mevedel-agent-record-invocation record))
            (mevedel-test--await 5 "Child excerpt recovery did not settle"
              (mevedel-agent-invocation-runtime-settled-p invocation))
            (should (eq 'completed (mevedel-agent-record-settled-outcome record)))
            (should (string-search "CHILD RECOVERY EVIDENCE" (mevedel-agent-record-settled-result record)))
            (should (string-search "Excerpt continuation" (mevedel-agent-record-settled-result record)))
            (should (equal '(nil "original" nil) ids))
            (should-not (assoc "root" (mevedel-session-external-conversations session)))
            (should (= 0 (mevedel-session-turn-count session))))
        (mevedel-agent-control-teardown-session session)))))

(mevedel-deftest mevedel-claude-code-recover-history/guards (:quiet t)
  (mevedel-engine-test--with-session
    (let ((history `(("root" :engine claude-code :id "retained"
                            :host ,(system-name) :directory ,root :state ready))))
      (setf (mevedel-session-external-conversations session) history)
      ;; An admitted root request must settle before detachment.
      (should-error (mevedel-claude-code-recover-history) :type 'user-error)
      (should (eq history (mevedel-session-external-conversations session)))
      (mevedel-request-end)
      (let ((mevedel-session--read-only-mode t))
        (should-error (mevedel-claude-code-recover-history) :type 'user-error))
      (should (eq history (mevedel-session-external-conversations session)))
      (mevedel-goal-create "Do not resume through recovery" session)
      (should-error (mevedel-claude-code-recover-history) :type 'user-error)
      (should (eq history (mevedel-session-external-conversations session)))
      (mevedel-goal-pause)
      (should-error (mevedel-claude-code-recover-history "/root/missing") :type 'user-error)
      (mevedel-claude-code-recover-history)
      (should (eq 'paused (mevedel-goal-status (mevedel-session-goal session))))
      (should-not (mevedel-session-external-conversations session))
      (should-not (mevedel-turn-busy-p buffer)))))

(mevedel-deftest mevedel-claude-code-recover-history/publication-failure (:quiet t)
  (mevedel-engine-test--with-session
    (mevedel-request-end)
    (let* ((history `(("root" :engine claude-code :id "retained"
                             :host ,(system-name) :directory ,root :state ready)))
           (hashes (list (cons (list "/root" (file-name-concat root "AGENTS.md")) (make-string 64 ?a))))
           (mentions (make-hash-table :test #'equal))
           (plan (mevedel-reminders-make-plan-reference))
           attempted)
      (puthash "retained mention" t mentions)
      (setf (mevedel-reminder-last-fired plan) 4)
      (setf (mevedel-session-external-conversations session) history
            (mevedel-session-workspace-instruction-hashes session) hashes
            (mevedel-session-mentions-shown session) mentions
            (mevedel-session-reminders session) (list plan))
      ;; Fault at the storage publication boundary, after the user control has
      ;; prepared detachment; failed publication must leave resume possible.
      (cl-letf (((symbol-function 'mevedel-session-publication-publish)
                 (lambda (&rest _)
                   (setq attempted t)
                   (error "Injected storage publication failure"))))
        (should-error (mevedel-claude-code-recover-history)))
      (should attempted)
      (should (eq history (mevedel-session-external-conversations session)))
      (should (eq hashes (mevedel-session-workspace-instruction-hashes session)))
      (should (eq mentions (mevedel-session-mentions-shown session)))
      (should (= 4 (mevedel-reminder-last-fired plan))))))

(mevedel-deftest mevedel-claude-code-recover-history/root-summary (:quiet t)
  (dolist (edited '(nil t))
    (mevedel-engine-test--with-claude-session
      (let ((launches 0) ids)
        (setq-local gptel-system-prompt "Summary recovery fixture" gptel-tools nil)
        (cl-letf (((symbol-function 'mevedel-claude-code-launch)
                   (mevedel-engine-test--claude-launch
                    (lambda (_system mcp _model _effort &optional id _hook)
                      (cl-incf launches)
                      (push id ids)
                      (list :meta `((echoAllText . t)
                                    (compactionEvents .
                                                      ,(if (= launches 1)
                                                           [((sessionUpdate . "agent_message_chunk")
                                                             (content . ((type . "text") (text . "ARCHIVED ORIGINAL RESPONSE\n"))))
                                                            ((sessionUpdate . "compaction_update") (compactionId . "recover")
                                                             (status . "completed")
                                                             (summary . [((type . "text") (text . "RETAINED SUMMARY EVIDENCE"))]))]
                                                         []))))))))
          (mevedel--insert-user-turn "Compact this evidence")
          (mevedel--send-request "Compact this evidence")
          (mevedel-test--await 5 "Root summary did not settle" (not (mevedel-turn-busy-p buffer)))
          (should (= 2 (mevedel-session-current-segment session)))
          (when edited
            (gptel--save-state)
            (gptel--save-state)
            (goto-char (point-min))
            (search-forward "RETAINED SUMMARY EVIDENCE")
            (replace-match "USER EDITED SUMMARY" t t)
            ;; Query-replace uses this same primitive.  The closing wrapper's
            ;; sticky ignore property must not erase authoritative summary text.
            (should (eq 'ignore (get-text-property (1- (point)) 'gptel)))
            (should (eq 'diverged (plist-get (alist-get "root" (mevedel-session-external-conversations session) nil nil #'equal) :state))))
          (mevedel-claude-code-recover-history)
          (goto-char (point-max))
          (mevedel--insert-user-turn "Use the retained summary")
          (setq request (mevedel--send-request "Use the retained summary"))
          (mevedel-test--await 5 "Root summary recovery did not settle"
            (not (mevedel-turn-busy-p buffer)))
          (should (equal '(nil nil) ids))
          (should (eq 'success (plist-get (mevedel-engine-info request) :mevedel-acp-outcome)))
          (let ((answer (mevedel-agent-conversation-project-history buffer session)))
            (ert-info ((format "edited=%S" edited))
              (should (string-search "provenance: compaction-summary" answer))
              (should (= 1 (with-temp-buffer
                              (insert answer)
                              (how-many (if edited "USER EDITED SUMMARY" "RETAINED SUMMARY EVIDENCE")
                                        (point-min) (point-max))))))
            (should-not (string-search "ARCHIVED ORIGINAL RESPONSE" answer))))))))

(provide 'test-mevedel-claude-code-recovery)
;;; test-mevedel-claude-code-recovery.el ends here
