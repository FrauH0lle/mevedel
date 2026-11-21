;;; test-niwa-claude-code-recovery.el --- Explicit native history recovery -*- lexical-binding: t -*-

;;; Commentary:
;; Recover missing native conversations through user controls and ordinary sends.

;;; Code:

(require 'niwa-engine-test-support
         (file-name-concat (file-name-directory (or load-file-name buffer-file-name))
                           "niwa-engine-test-support"))
(require 'niwa-claude-code-session)

(niwa-deftest niwa-claude-code-history-assert-current ()
  (let ((niwa-claude-code-directory (make-temp-file "niwa-history-" t)))
    (unwind-protect
        (let ((record (list :engine 'claude-code :id "native" :host (system-name)
                            :directory (expand-file-name niwa-claude-code-directory)
                            :state 'ready)))
          (should-not (niwa-claude-code-history-assert-current nil))
          (should-not (niwa-claude-code-history-assert-current
                       '(:engine claude-code :state unstarted)))
          (should-not (niwa-claude-code-history-assert-current record))
          (should (string-search "recover-history"
                                 (cadr (should-error (niwa-claude-code-history-assert-current
                                                      (plist-put (copy-sequence record) :state 'diverged))
                                                     :type 'user-error))))
          (dolist (foreign (list (plist-put (copy-sequence record) :host "other-machine")
                                 (plist-put (copy-sequence record) :directory "/other/installation")))
            (let ((message (cadr (should-error (niwa-claude-code-history-assert-current foreign)
                                               :type 'user-error))))
              (should (string-search "another installation" message))
              (should (string-search "M-x niwa-claude-code-recover-history" message)))))
      (delete-directory niwa-claude-code-directory t))))

(niwa-deftest niwa-claude-code-history-unresumable-p
  (:doc "flags edited, foreign and failed native histories, not resumable ones")
  (let* ((niwa-claude-code-directory "/tmp/niwa-history-unresumable")
         (record (list :engine 'claude-code :id "native" :host (system-name)
                       :directory (expand-file-name niwa-claude-code-directory)
                       :state 'ready)))
    (should-not (niwa-claude-code-history-unresumable-p nil))
    (should-not (niwa-claude-code-history-unresumable-p
                 '(:engine claude-code :state unstarted)))
    (should-not (niwa-claude-code-history-unresumable-p record))
    (dolist (change '((:state diverged) (:host "other-machine") (:start-failed t)))
      (should (niwa-claude-code-history-unresumable-p
               (plist-put (copy-sequence record) (car change) (cadr change)))))))

(niwa-deftest niwa-claude-code-history-open ()
  (let* ((niwa-claude-code-directory "/tmp/niwa-history-open")
         (owner (niwa-request--create))
         (record (niwa-claude-code-history-open owner "native" '(1 . 4))))
    (should (equal (list :engine 'claude-code :id "native" :host (system-name)
                         :directory "/tmp/niwa-history-open" :state 'in-flight
                         :input-boundary '(1 . 4))
                   record))
    (should (eq record (plist-get (niwa-engine-info owner) :niwa-claude-history)))
    (should-not (plist-member (niwa-claude-code-history-open owner "child") :input-boundary))))

(niwa-deftest niwa-claude-code-history-settle ()
  (let ((owner (niwa-request--create)))
    (dolist (case '((success ready) (error uncertain) (interrupted uncertain)))
      (let ((record (list :id "native" :state 'in-flight)))
        (niwa-claude-code-history-settle owner nil record (list :status (car case)))
        (should (eq (cadr case) (plist-get record :state)))))
    (let ((record (list :id "native" :state 'diverged)))
      (niwa-claude-code-history-settle owner nil record '(:status success))
      (should (eq 'diverged (plist-get record :state))))
    ;; A retained identity that failed to start names excerpt recovery.
    (setf (niwa-engine-info owner) (list :niwa-acp-outcome 'error :error "Missing history"))
    (let ((history (list :id "native")))
      (niwa-claude-code-history-settle owner history nil '(:status error))
      (should (string-search "recover-history" (plist-get (niwa-engine-info owner) :error)))
      ;; It is marked so the browser can offer recovery.
      (should (plist-get history :start-failed)))
    (setf (niwa-engine-info owner) (list :niwa-acp-outcome 'error :error "Startup failed"))
    (niwa-claude-code-history-settle owner nil nil '(:status error))
    (should (equal "Startup failed" (plist-get (niwa-engine-info owner) :error)))))

(niwa-deftest niwa-claude-code-recover-history/root (:quiet t)
  (niwa-engine-test--with-claude-session
    (let ((view (generate-new-buffer " *claude-recovery*"))
          (file (file-name-concat root "evidence.txt"))
          (launches 0) ids)
      (unwind-protect
          (progn
            (write-region "RETAINED TOOL EVIDENCE" nil file nil 'silent)
            (setq-local gptel-system-prompt "Recovery fixture"
                        gptel-tools (list (niwa-tool-gptel-tool (niwa-tool-ensure "Read"))))
            (niwa-view--setup view buffer)
            (cl-letf (((symbol-function 'niwa-claude-code-launch)
                       (niwa-engine-test--claude-launch
                        (lambda (_system mcp _model _effort &optional id hook)
                          (cl-incf launches)
                          (push id ids)
                          (list :meta (append `((hookCommand . ,hook)
                                                (fixtureSessionId . ,(if (= launches 1) "original" "replacement"))
                                                (echoAllText . t))
                                              (when (= launches 1)
                                                `((toolBatches . [[((name . "Read") (id . "old-read")
                                                                    (args . ((file_path . ,file))))]])))))))))
              (niwa--insert-user-turn "Read this evidence")
              (niwa--send-request "Read this evidence")
              (niwa-test--await 5 "Initial native turn did not settle"
                (not (niwa-turn-busy-p buffer)))
              (goto-char (point-max))
              (niwa--insert-user-turn "Continue the work")
              (setq request (niwa--send-request "Continue the work"))
              (niwa-test--await 5 "Missing native history did not fail"
                (not (niwa-turn-busy-p buffer)))
              (should (eq 'error (plist-get (niwa-engine-info request) :niwa-acp-outcome)))
              (should (string-search "Missing history" (plist-get (niwa-engine-info request) :error)))
              (should (string-search "niwa-claude-code-recover-history" (plist-get (niwa-engine-info request) :error)))
              (should (string-search "RETAINED TOOL EVIDENCE" (buffer-string)))
              (let ((before (buffer-string)))
                (with-current-buffer view (niwa-claude-code-recover-history))
                (should (equal before (buffer-string))))
              (should-not (plist-get (cdr (assoc "root" (niwa-session-external-conversations session))) :id))
              (let ((metadata (niwa-session-codec-read
                               (niwa-session-artifacts-sidecar-path (niwa-session-save-path session)))))
                (should (eq 'unstarted (plist-get (cdr (assoc "root" (plist-get metadata :external-conversations))) :state))))
              (goto-char (point-max))
              (niwa--insert-user-turn "Recover from retained evidence")
              (niwa--send-request "Recover from retained evidence")
              (niwa-test--await 5 "Recovered native turn did not settle"
                (not (niwa-turn-busy-p buffer)))
              (should (equal '(nil "original" nil) ids))
              (should (string-search "answer:Excerpt continuation" (buffer-string)))
              (should (string-search "not an exact native resume" (buffer-string)))
              (should (= 1 (how-many "^#\\+begin_tool" (point-min) (point-max))))
              (should (equal "replacement" (plist-get (cdr (assoc "root" (niwa-session-external-conversations session))) :id)))
              (goto-char (point-max))
              (niwa--insert-user-turn "Continue after recovery")
              (niwa--send-request "Continue after recovery")
              (niwa-test--await 5 "Recovered history did not resume"
                (not (niwa-turn-busy-p buffer)))
              (should (equal "replacement" (car ids)))))
        (when (buffer-live-p view) (kill-buffer view))))))

(niwa-deftest niwa-claude-code-recover-history/child (:quiet t)
  (niwa-engine-test--with-claude-session
    (niwa-request-end)
    (let ((launches 0) record invocation ids)
      (unwind-protect
          (cl-letf (((symbol-function 'niwa-claude-code-launch)
                     (niwa-engine-test--claude-launch
                      (lambda (_system mcp _model _effort &optional id _hook)
                        (cl-incf launches)
                        (push id ids)
                        (list :meta `((fixtureSessionId . ,(if (= launches 1) "original" "replacement"))
                                      (echoAllText . t)))))))
            (niwa-agent-control-spawn
             session "reader" "Keep CHILD RECOVERY EVIDENCE for later."
             (lambda (value) (setq record (plist-get value :record)))
             :agent (niwa-agent--create :name "reader" :description "Read evidence"
                                          :tools nil :system-prompt "Recovery fixture")
             :on-invocation (lambda (value) (setq invocation value)))
            (niwa-test--await 5 "Child initial turn did not settle"
              (niwa-agent-invocation-runtime-settled-p invocation))
            (niwa-agent-control-send-message session "/root/reader" "Continue the task")
            (setq invocation (niwa-agent-record-invocation record))
            (niwa-test--await 5 "Missing child history did not settle"
              (niwa-agent-invocation-runtime-settled-p invocation))
            (should (eq 'errored (niwa-agent-record-settled-outcome record)))
            (should (string-search "niwa-claude-code-recover-history"
                                   (niwa-agent-record-settled-result record)))
            (niwa-claude-code-recover-history "/root/reader")
            (niwa-agent-control-send-message session "/root/reader" "Use the retained evidence")
            (setq invocation (niwa-agent-record-invocation record))
            (niwa-test--await 5 "Child excerpt recovery did not settle"
              (niwa-agent-invocation-runtime-settled-p invocation))
            (should (eq 'completed (niwa-agent-record-settled-outcome record)))
            (should (string-search "CHILD RECOVERY EVIDENCE" (niwa-agent-record-settled-result record)))
            (should (string-search "Excerpt continuation" (niwa-agent-record-settled-result record)))
            (should (equal '(nil "original" nil) ids))
            (should-not (assoc "root" (niwa-session-external-conversations session)))
            (should (= 0 (niwa-session-turn-count session))))
        (niwa-agent-control-teardown-session session)))))

(niwa-deftest niwa-claude-code-recover-history/guards (:quiet t)
  (niwa-engine-test--with-session
    (let ((history `(("root" :engine claude-code :id "retained"
                            :host ,(system-name) :directory ,root :state ready))))
      (setf (niwa-session-external-conversations session) history)
      ;; An admitted root request must settle before detachment.
      (should-error (niwa-claude-code-recover-history) :type 'user-error)
      (should (eq history (niwa-session-external-conversations session)))
      (niwa-request-end)
      (let ((niwa-session--read-only-mode t))
        (should-error (niwa-claude-code-recover-history) :type 'user-error))
      (should (eq history (niwa-session-external-conversations session)))
      (niwa-goal-create "Do not resume through recovery" :session session)
      (should-error (niwa-claude-code-recover-history) :type 'user-error)
      (should (eq history (niwa-session-external-conversations session)))
      (niwa-goal-pause)
      (should-error (niwa-claude-code-recover-history "/root/missing") :type 'user-error)
      (niwa-claude-code-recover-history)
      (should (eq 'paused (niwa-goal-status (niwa-session-goal session))))
      (should (equal '(("root" :engine claude-code :state unstarted :boundaries ((:turn 0 :detached t))))
                     (niwa-session-external-conversations session)))
      (should-not (niwa-turn-busy-p buffer)))))

(niwa-deftest niwa-claude-code-recover-history/publication-failure (:quiet t)
  (niwa-engine-test--with-session
    (niwa-request-end)
    (let* ((history `(("root" :engine claude-code :id "retained"
                             :host ,(system-name) :directory ,root :state ready)))
           (hashes (list (cons (list "/root" (file-name-concat root "AGENTS.md")) (make-string 64 ?a))))
           (mentions (make-hash-table :test #'equal))
           (plan (niwa-reminders-make-plan-reference))
           attempted)
      (puthash "retained mention" t mentions)
      (setf (niwa-reminder-last-fired plan) 4)
      (setf (niwa-session-external-conversations session) history
            (niwa-session-workspace-instruction-hashes session) hashes
            (niwa-session-mentions-shown session) mentions
            (niwa-session-reminders session) (list plan))
      ;; Fault at the storage publication boundary, after the user control has
      ;; prepared detachment; failed publication must leave resume possible.
      (cl-letf (((symbol-function 'niwa-session-publication-publish)
                 (lambda (&rest _)
                   (setq attempted t)
                   (error "Injected storage publication failure"))))
        (should-error (niwa-claude-code-recover-history)))
      (should attempted)
      (should (eq history (niwa-session-external-conversations session)))
      (should (eq hashes (niwa-session-workspace-instruction-hashes session)))
      (should (eq mentions (niwa-session-mentions-shown session)))
      (should (= 4 (niwa-reminder-last-fired plan))))))

(niwa-deftest niwa-claude-code-recover-history/root-summary (:quiet t)
  (dolist (edited '(nil t))
    (niwa-engine-test--with-claude-session
      (let ((launches 0) ids)
        (setq-local gptel-system-prompt "Summary recovery fixture" gptel-tools nil)
        (cl-letf (((symbol-function 'niwa-claude-code-launch)
                   (niwa-engine-test--claude-launch
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
          (niwa--insert-user-turn "Compact this evidence")
          (niwa--send-request "Compact this evidence")
          (niwa-test--await 5 "Root summary did not settle" (not (niwa-turn-busy-p buffer)))
          (should (= 2 (niwa-session-current-segment session)))
          (when edited
            (gptel--save-state)
            (gptel--save-state)
            (goto-char (point-min))
            (search-forward "RETAINED SUMMARY EVIDENCE")
            (replace-match "USER EDITED SUMMARY" t t)
            ;; Query-replace uses this same primitive.  The closing wrapper's
            ;; sticky ignore property must not erase authoritative summary text.
            (should (eq 'ignore (get-text-property (1- (point)) 'gptel)))
            (should (eq 'diverged (plist-get (alist-get "root" (niwa-session-external-conversations session) nil nil #'equal) :state))))
          (niwa-claude-code-recover-history)
          (goto-char (point-max))
          (niwa--insert-user-turn "Use the retained summary")
          (setq request (niwa--send-request "Use the retained summary"))
          (niwa-test--await 5 "Root summary recovery did not settle"
            (not (niwa-turn-busy-p buffer)))
          (should (equal '(nil nil) ids))
          (should (eq 'success (plist-get (niwa-engine-info request) :niwa-acp-outcome)))
          (let ((answer (niwa-agent-conversation-project-history buffer session)))
            (ert-info ((format "edited=%S" edited))
              (should (string-search "provenance: compaction-summary" answer))
              (should (= 1 (with-temp-buffer
                              (insert answer)
                              (how-many (if edited "USER EDITED SUMMARY" "RETAINED SUMMARY EVIDENCE")
                                        (point-min) (point-max))))))
            (should-not (string-search "ARCHIVED ORIGINAL RESPONSE" answer))))))))

(provide 'test-niwa-claude-code-recovery)
;;; test-niwa-claude-code-recovery.el ends here
