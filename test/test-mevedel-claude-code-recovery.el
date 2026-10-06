;;; test-mevedel-claude-code-recovery.el --- Explicit native history recovery -*- lexical-binding: t -*-

;;; Commentary:
;; Recover missing native conversations through user controls and ordinary sends.

;;; Code:

(require 'mevedel-engine-test-support
         (file-name-concat (file-name-directory (or load-file-name buffer-file-name))
                           "mevedel-engine-test-support"))
(require 'mevedel-claude-code-session)

(defconst mevedel-claude-code-recovery-test--peer
  (file-name-concat (file-name-directory (or load-file-name buffer-file-name))
                    "fixtures" "acp-agent.py"))

(mevedel-deftest mevedel-claude-code-recover-history/root (:quiet t)
  (mevedel-engine-test--with-session
    (let ((gptel--known-backends nil)
          (mevedel-claude-code-directory (file-name-concat root "claude"))
          (view (generate-new-buffer " *claude-recovery*"))
          (file (file-name-concat root "evidence.txt"))
          (launches 0) ids)
      (unwind-protect
          (progn
            (write-region "RETAINED TOOL EVIDENCE" nil file nil 'silent)
            (mevedel-claude-code-register)
            (mevedel-model-set-session-provider
             session (mevedel-model-resolve-provider "Claude Code:sonnet") buffer)
            (setq-local gptel-system-prompt "Recovery fixture"
                        gptel-tools (list (mevedel-tool-gptel-tool (mevedel-tool-ensure "Read"))))
            (mevedel-view--setup view buffer)
            (cl-letf (((symbol-function 'mevedel-claude-code-launch)
                       (lambda (_system mcp _model _effort &optional id hook)
                         (cl-incf launches)
                         (push id ids)
                         (list :command (executable-find "python3")
                               :args (list mevedel-claude-code-recovery-test--peer)
                               :cwd root :mcp mcp :session-id id
                               :tool-id-field :claudecode/toolUseId
                               :observe #'mevedel-claude-code--observe
                               :check-context #'mevedel-claude-code-context-check
                               :control #'mevedel-claude-code--control
                               :meta (append `((hookCommand . ,hook)
                                               (fixtureSessionId . ,(if (= launches 1) "original" "replacement"))
                                               (echoAllText . t))
                                             (when (= launches 1)
                                               `((toolBatches . [[((name . "Read") (id . "old-read")
                                                                  (args . ((file_path . ,file))))]]))))))))
              (mevedel--insert-user-turn "Read this evidence")
              (mevedel--send-request "Read this evidence")
              (with-timeout (5 (ert-fail "Initial native turn did not settle"))
                (while (mevedel-turn-busy-p buffer) (accept-process-output nil 0.01)))
              (goto-char (point-max))
              (mevedel--insert-user-turn "Continue the work")
              (setq request (mevedel--send-request "Continue the work"))
              (with-timeout (5 (ert-fail "Missing native history did not fail"))
                (while (mevedel-turn-busy-p buffer) (accept-process-output nil 0.01)))
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
              (with-timeout (5 (ert-fail "Recovered native turn did not settle"))
                (while (mevedel-turn-busy-p buffer) (accept-process-output nil 0.01)))
              (should (equal '(nil "original" nil) ids))
              (should (string-search "answer:Excerpt continuation" (buffer-string)))
              (should (string-search "not an exact native resume" (buffer-string)))
              (should (= 1 (how-many "^#\\+begin_tool" (point-min) (point-max))))
              (should (equal "replacement" (plist-get (cdr (assoc "root" (mevedel-session-external-conversations session))) :id)))
              (goto-char (point-max))
              (mevedel--insert-user-turn "Continue after recovery")
              (mevedel--send-request "Continue after recovery")
              (with-timeout (5 (ert-fail "Recovered history did not resume"))
                (while (mevedel-turn-busy-p buffer) (accept-process-output nil 0.01)))
              (should (equal "replacement" (car ids)))))
        (when (buffer-live-p view) (kill-buffer view))))))

(mevedel-deftest mevedel-claude-code-recover-history/child (:quiet t)
  (mevedel-engine-test--with-session
    (mevedel-request-end)
    (let ((gptel--known-backends nil)
          (mevedel-claude-code-directory (file-name-concat root "claude"))
          (launches 0) record invocation ids)
      (mevedel-claude-code-register)
      (mevedel-model-set-session-provider
       session (mevedel-model-resolve-provider "Claude Code:sonnet") buffer)
      (unwind-protect
          (cl-letf (((symbol-function 'mevedel-claude-code-launch)
                     (lambda (_system mcp _model _effort &optional id _hook)
                       (cl-incf launches)
                       (push id ids)
                       (list :command (executable-find "python3")
                             :args (list mevedel-claude-code-recovery-test--peer)
                             :cwd root :mcp mcp :session-id id
                             :observe #'mevedel-claude-code--observe
                             :check-context #'mevedel-claude-code-context-check
                             :meta `((fixtureSessionId . ,(if (= launches 1) "original" "replacement"))
                                     (echoAllText . t))))))
            (mevedel-agent-control-spawn
             session "reader" "Keep CHILD RECOVERY EVIDENCE for later."
             (lambda (value) (setq record (plist-get value :record)))
             :agent (mevedel-agent--create :name "reader" :description "Read evidence"
                                          :tools nil :system-prompt "Recovery fixture")
             :on-invocation (lambda (value) (setq invocation value)))
            (with-timeout (5 (ert-fail "Child initial turn did not settle"))
              (while (not (mevedel-agent-invocation-runtime-settled-p invocation))
                (accept-process-output nil 0.01)))
            (mevedel-agent-control-followup session "/root/reader" "Continue the task")
            (setq invocation (mevedel-agent-record-invocation record))
            (with-timeout (5 (ert-fail "Missing child history did not settle"))
              (while (not (mevedel-agent-invocation-runtime-settled-p invocation))
                (accept-process-output nil 0.01)))
            (should (eq 'errored (mevedel-agent-record-settled-outcome record)))
            (should (string-search "mevedel-claude-code-recover-history"
                                   (mevedel-agent-record-settled-result record)))
            (mevedel-claude-code-recover-history "/root/reader")
            (mevedel-agent-control-followup session "/root/reader" "Use the retained evidence")
            (setq invocation (mevedel-agent-record-invocation record))
            (with-timeout (5 (ert-fail "Child excerpt recovery did not settle"))
              (while (not (mevedel-agent-invocation-runtime-settled-p invocation))
                (accept-process-output nil 0.01)))
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

(provide 'test-mevedel-claude-code-recovery)
;;; test-mevedel-claude-code-recovery.el ends here
