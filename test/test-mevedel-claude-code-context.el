;;; test-mevedel-claude-code-context.el --- Native context workflows -*- lexical-binding: t -*-

;;; Commentary:
;; Exercise context delivery through ordinary root and retained-agent turns.

;;; Code:

(require 'mevedel-engine-test-support
         (file-name-concat (file-name-directory (or load-file-name buffer-file-name))
                           "mevedel-engine-test-support"))
(require 'mevedel-claude-code-session)

(defconst mevedel-claude-code-context-test--peer
  (file-name-concat (file-name-directory (or load-file-name buffer-file-name))
                    "fixtures" "acp-agent.py"))

(mevedel-deftest mevedel--send-request/claude-child-roster (:quiet t)
  (dolist (receipt '(t nil compact))
    (mevedel-engine-test--with-session
      (let ((gptel--known-backends nil)
            (mevedel-claude-code-directory (file-name-concat root "claude"))
            (file (file-name-concat root "evidence.txt"))
            record root-turn)
        (write-region "Evidence" nil file nil 'silent)
        (mevedel-claude-code-register)
        (mevedel-model-set-session-provider
         session (mevedel-model-resolve-provider "Claude Code:sonnet") buffer)
        (setq-local gptel-system-prompt "Root fixture"
                    gptel-tools (list (mevedel-tool-gptel-tool (mevedel-tool-ensure "Read"))))
        (mevedel-session-add-reminder
         session (mevedel-reminders-make-user-revised-patch "ROOT-ONLY-REMINDER"))
        (unwind-protect
            (cl-letf (((symbol-function 'mevedel-claude-code-launch)
                       (lambda (_system mcp _model _effort &optional id hook)
                         (list :command (executable-find "python3")
                               :args (list mevedel-claude-code-context-test--peer)
                               :cwd root :mcp mcp :session-id id
                               :tool-id-field :claudecode/toolUseId
                               :control #'mevedel-claude-code--control
                               :check-context #'mevedel-claude-code-context-check
                               :observe #'mevedel-claude-code-context-observe
                               :meta `((hookCommand . ,hook)
                                       (compactBeforeBatch . ,(when (eq receipt 'compact) 0))
                                       (fixtureSessionId . ,(if root-turn "root" "child"))
                                       (promptAcknowledgement . ,(if (or (not root-turn) receipt) t :false))
                                       (responseText . "Completed")
                                       (toolBatches . ,(if root-turn
                                                          (vector (vector `((name . "Read") (id . "root-read")
                                                                            (args . ((file_path . ,file))))))
                                                        [])))))))
              (mevedel-agent-control-spawn
               session "reader" "Inspect this task."
               (lambda (value) (setq record (plist-get value :record)))
               :agent (mevedel-agent--create :name "reader" :description "Read evidence"
                                            :tools '(Read) :system-prompt "Child fixture"
                                            :reminders (list (mevedel-reminders-make-user-revised-patch "CHILD-ONLY-REMINDER"))))
              (with-timeout (5 (ert-fail "Roster child did not settle"))
                (while (mevedel-agent-control-active-turn-p session) (accept-process-output nil 0.01)))
              (should record)
              (with-current-buffer (mevedel-agent-record-conversation-buffer record)
                (should (= 1 (mevedel-engine-test--count-evidence "CHILD-ONLY-REMINDER")))
                (should-not (string-search "ROOT-ONLY-REMINDER" (mevedel-engine-test--evidence))))
              (setq root-turn t)
              (insert "Continue with the retained child")
              (mevedel--send-request "Continue with the retained child")
              (with-timeout (5 (ert-fail "Roster root did not settle"))
                (while (mevedel-turn-busy-p buffer) (accept-process-output nil 0.01)))
              (should (eq (if receipt 'success 'error)
                          (plist-get (mevedel-engine-info request) :mevedel-acp-outcome)))
              (should (= (pcase receipt ('compact 2) ('nil 0) (_ 1))
                         (mevedel-engine-test--count-evidence "<agent-roster>")))
              (when receipt
                (should (string-search "- `/root/reader` (`reader`)" (mevedel-engine-test--evidence)))
                (should (= 1 (mevedel-engine-test--count-evidence "ROOT-ONLY-REMINDER"))))
              (should-not (string-search "CHILD-ONLY-REMINDER" (mevedel-engine-test--evidence)))
              (should (= (if receipt 1 0) (how-many "^#\\+begin_tool" (point-min) (point-max)))))
          (when (and record (buffer-live-p (mevedel-agent-record-conversation-buffer record)))
            (kill-buffer (mevedel-agent-record-conversation-buffer record))))))))

(mevedel-deftest mevedel--send-request/claude-reminders (:quiet t)
  (dolist (receipt '(t nil late silent compact))
    (mevedel-engine-test--with-session
      (let* ((gptel--known-backends nil)
             (mevedel-claude-code-directory (file-name-concat root "claude"))
             (file (file-name-concat root "evidence.txt"))
             (reminder (mevedel-reminders-make-user-revised-patch
                        (concat "REVISED-PATCH-7194 " (make-string 11000 ?x))))
             (commits 0) queued)
        (write-region "Evidence" nil file nil 'silent)
        (mevedel-claude-code-register)
        (mevedel-model-set-session-provider
         session (mevedel-model-resolve-provider "Claude Code:sonnet") buffer)
        (setq-local gptel-system-prompt "Reminder fixture"
                    gptel-tools (list (mevedel-tool-gptel-tool (mevedel-tool-ensure "Read"))))
        (unless (eq receipt 'silent)
          (mevedel-reminders-install-defaults session)
          (mevedel-session-add-reminder session reminder)
          (mevedel-session-enqueue-pending-reminder session "QUEUED-BEFORE-2194")
          (mevedel-hooks-record-session-context
           session '(:additional-context ("INITIAL-HOOK-4813")) 'SessionStart))
        (when (eq receipt 'compact) (setf (mevedel-session-plan-mode session) t))
        (mevedel-session-add-reminder
         session (mevedel-reminder-create
                  :type 'silent :trigger (lambda (_) t) :interval 'one-shot
                  :content (lambda (_) (list :commit (lambda () (cl-incf commits))))))
        (cl-letf (((symbol-function 'mevedel-claude-code-launch)
                   (lambda (_system mcp _model _effort &optional id hook)
                     (list :command (executable-find "python3")
                           :args (list mevedel-claude-code-context-test--peer)
                           :cwd root :mcp mcp :session-id id
                           :tool-id-field :claudecode/toolUseId
                           :control #'mevedel-claude-code--control
                           :check-context #'mevedel-claude-code-context-check
                           :observe (lambda (owner notification)
                                      (when (and (eq receipt 'late) (not queued)
                                                 (equal "user" (alist-get 'type (alist-get 'message (alist-get 'params notification)))))
                                        (setq queued t)
                                        (mevedel-session-enqueue-pending-reminder session "QUEUED-LATER-4529")
                                        (mevedel-hooks-record-session-context
                                         session '(:additional-context ("LATER-HOOK-2974")) 'SessionStart))
                                      (mevedel-claude-code-context-observe owner notification))
                           :meta `((hookCommand . ,hook) (promptAcknowledgement . ,(if receipt t :false))
                                   (compactBeforeBatch . ,(when (eq receipt 'compact) 0))
                                   (responseText . "Completed")
                                   (toolBatches . [[((name . "Read") (id . "read-1") (args . ((file_path . ,file))))]
                                                  [((name . "Read") (id . "read-2") (args . ((file_path . ,file))))]]))))))
          (insert "Use current reminders")
          (mevedel--send-request "Use current reminders")
          (with-timeout (5 (ert-fail "Reminder turn did not settle"))
            (while (mevedel-turn-busy-p buffer) (accept-process-output nil 0.01)))
          (ert-info ((format "receipt=%S" receipt))
            (should (eq (if receipt 'success 'error)
                        (plist-get (mevedel-engine-info request) :mevedel-acp-outcome)))
            (should (= (if receipt 1 0) commits))
            (let* ((records (mevedel-transcript-audit-records
                             (buffer-substring (point-min) (point-max))))
                   (injections (seq-filter
                                (lambda (r) (eq (plist-get r :type) 'injected-reminders))
                                records))
                   (typed (seq-find
                           (lambda (r) (seq-find
                                        (lambda (item) (eq (plist-get item :type) 'user-revised-patch))
                                        (plist-get r :items))) injections)))
              (should (eq (and (memq receipt '(t late compact)) t) (and typed t)))
              (when typed
                (should (eq 'turn-start (plist-get typed :phase)))
                (let* ((turns (mevedel-view--group-into-turns
                               (mevedel-transcript-segments (point-min) (point-max)) buffer))
                       (audits (mevedel-view--user-turn-hook-audits
                                (plist-get (car turns) :segments) buffer)))
                  (should (eq 'user (plist-get (car turns) :role)))
                  (should (member typed (mapcar (lambda (r)
                                                 (list :type (plist-get r :type)
                                                       :phase (plist-get r :phase)
                                                       :items (plist-get r :items))) audits))))))
            (should (= (if receipt 2 0) (how-many "^#\\+begin_tool" (point-min) (point-max))))
            (should (= (if (memq receipt '(t late compact)) 1 0)
                       (mevedel-engine-test--count-evidence "REVISED-PATCH-7194")))
            (should (equal (pcase receipt ('nil '("QUEUED-BEFORE-2194")) ('late '("QUEUED-LATER-4529")))
                           (mevedel-session-pending-reminders session)))
            (should (= (if (memq receipt '(t late compact)) 1 0)
                       (mevedel-engine-test--count-evidence "INITIAL-HOOK-4813")))
            (should (equal (pcase receipt ('nil '("INITIAL-HOOK-4813")) ('late '("LATER-HOOK-2974")))
                           (mapcar (lambda (entry) (plist-get entry :body))
                                   (mevedel-session-hook-context-pending session))))
            (should (= (if (eq receipt 'compact) 2 0)
                       (mevedel-engine-test--count-evidence "Plan mode is active")))
            (should-not (string-search "Context pressure is high" (mevedel-engine-test--evidence)))))))))

(mevedel-deftest mevedel--send-request/claude-compacted-plan-reference (:quiet t)
  (dolist (receipt '(t nil))
    (mevedel-engine-test--with-session
      (let* ((gptel--known-backends nil)
             (mevedel-claude-code-directory (file-name-concat root "claude"))
             (file (file-name-concat root "evidence.txt"))
             (plan-path "local/plans/accepted-20261006-120000.md")
             (plan-file (file-name-concat (mevedel-session-save-path session) plan-path)))
        (write-region "Evidence" nil file nil 'silent)
        (mevedel-session-artifacts-publish-text
         session plan-file "# Accepted plan\n\nACCEPTED-PLAN-5192" 'utf-8-unix)
        (setf (mevedel-session-plan-metadata session)
              (list :status 'accepted :accepted-path plan-path))
        (mevedel-reminders-install-defaults session)
        (mevedel-claude-code-register)
        (mevedel-model-set-session-provider
         session (mevedel-model-resolve-provider "Claude Code:sonnet") buffer)
        (setq-local gptel-system-prompt "Plan reference fixture"
                    gptel-tools (list (mevedel-tool-gptel-tool (mevedel-tool-ensure "Read"))))
        (cl-letf (((symbol-function 'mevedel-claude-code-launch)
                   (lambda (_system mcp _model _effort &optional id hook)
                     (list :command (executable-find "python3")
                           :args (list mevedel-claude-code-context-test--peer)
                           :cwd root :mcp mcp :session-id id
                           :tool-id-field :claudecode/toolUseId
                           :control #'mevedel-claude-code--control
                           :check-context #'mevedel-claude-code-context-check
                           :observe #'mevedel-claude-code-context-observe
                           :meta `((hookCommand . ,hook) (compactBeforeBatch . 1)
                                   (compactAcknowledgement . ,(if receipt t :false))
                                   (responseText . "Completed")
                                   (toolBatches . [[((name . "Read") (id . "read-1") (args . ((file_path . ,file))))]
                                                  [((name . "Read") (id . "read-2") (args . ((file_path . ,file))))]]))))))
          (insert "Continue the accepted plan")
          (mevedel--send-request "Continue the accepted plan")
          (with-timeout (5 (ert-fail "Accepted-plan turn did not settle"))
            (while (mevedel-turn-busy-p buffer) (accept-process-output nil 0.01)))
          (should (eq (if receipt 'success 'error)
                      (plist-get (mevedel-engine-info request) :mevedel-acp-outcome)))
          (should (= (if receipt 2 1)
                     (mevedel-engine-test--count-evidence "ACCEPTED-PLAN-5192")))
          (should (= (if receipt 2 1)
                     (how-many "^#\\+begin_tool" (point-min) (point-max)))))))))

(mevedel-deftest mevedel--send-request/claude-compacted-path-instructions (:quiet t)
  (dolist (change '(unchanged updated removed missing-receipt oversized))
    (mevedel-engine-test--with-session
      (let* ((gptel--known-backends nil)
             (mevedel-claude-code-directory (file-name-concat root "claude"))
             (directory (file-name-concat root "lib"))
             (file (file-name-concat directory "evidence.txt"))
             (instructions (file-name-concat directory "AGENTS.md"))
             (other (file-name-concat root "other.txt"))
             changed)
        (make-directory directory)
        (write-region "Evidence" nil file nil 'silent)
        (write-region "Other evidence" nil other nil 'silent)
        (write-region "ORIGINAL-PATH-GUIDANCE-5341" nil instructions nil 'silent)
        (mevedel-claude-code-register)
        (mevedel-model-set-session-provider
         session (mevedel-model-resolve-provider "Claude Code:sonnet") buffer)
        (setq-local gptel-system-prompt "Compacted path context fixture"
                    gptel-tools (list (mevedel-tool-gptel-tool (mevedel-tool-ensure "Read"))))
        (cl-letf (((symbol-function 'mevedel-claude-code-launch)
                   (lambda (_system mcp _model _effort &optional id hook)
                     (list :command (executable-find "python3")
                           :args (list mevedel-claude-code-context-test--peer)
                           :cwd root :mcp mcp :session-id id
                           :tool-id-field :claudecode/toolUseId
                           :control #'mevedel-claude-code--control
                           :check-context #'mevedel-claude-code-context-check
                           :complete-prompt #'mevedel-claude-code--complete-prompt
                           :observe (lambda (owner notification)
                                      (mevedel-claude-code-context-observe owner notification)
                                      (when (and (not changed)
                                                 (equal "agent_message_chunk"
                                                        (alist-get 'sessionUpdate (alist-get 'update (alist-get 'params notification)))))
                                        (setq changed t)
                                        (pcase change
                                          ('removed (delete-file instructions))
                                          ((or 'updated 'oversized)
                                           (write-region (concat "UPDATED-PATH-GUIDANCE-6759"
                                                                 (when (eq change 'oversized) (make-string 11000 ?x)))
                                                         nil instructions nil 'silent)))))
                           :meta `((hookCommand . ,hook) (compactBeforeBatch . 1) (preToolHook . t)
                                   (compactAcknowledgement . ,(if (eq change 'missing-receipt) :false t))
                                   (responseText . "Completed")
                                   (toolBatches . [[((name . "Read") (id . "scoped-read") (args . ((file_path . ,file))))]
                                                  [((name . "Read") (id . "other-read") (args . ((file_path . ,other))))]])
                                   (continuationPrompts . [((toolBatches . [[((name . "Read") (id . "continued-other")
                                                                            (args . ((file_path . ,other))))]]))]))))))
          (insert "Continue with current path instructions after compaction")
          (mevedel--send-request "Continue with current path instructions after compaction")
          (with-timeout (5 (ert-fail "Compacted path context did not settle"))
            (while (mevedel-turn-busy-p buffer) (accept-process-output nil 0.01)))
          (ert-info ((format "change=%S" change))
            (should (eq (if (eq change 'missing-receipt) 'error 'success)
                        (plist-get (mevedel-engine-info request) :mevedel-acp-outcome)))
            (should (= (if (eq change 'missing-receipt) 1 2)
                       (how-many "^#\\+begin_tool" (point-min) (point-max))))
            (should (= (if (eq change 'unchanged) 2 1)
                       (mevedel-engine-test--count-evidence "ORIGINAL-PATH-GUIDANCE-5341")))
            (should (= (if (memq change '(updated oversized)) 1 0)
                       (mevedel-engine-test--count-evidence "UPDATED-PATH-GUIDANCE-6759")))
            (should (= (if (eq change 'removed) 1 0)
                       (mevedel-engine-test--count-evidence "no longer present"))))
          (should (= 1 (mevedel-session-turn-count session))))))))

(mevedel-deftest mevedel-agent-control-spawn/claude-compacted-path-scope (:quiet t)
  (mevedel-engine-test--with-session
    (let* ((gptel--known-backends nil)
           (mevedel-claude-code-directory (file-name-concat root "claude"))
           (first-dir (file-name-concat root "first"))
           (second-dir (file-name-concat root "second"))
           (deep-dir (file-name-concat second-dir "deep"))
           (other (file-name-concat root "other.txt"))
           records)
      (make-directory first-dir)
      (make-directory deep-dir t)
      (dolist (directory (list first-dir deep-dir))
        (write-region "Evidence" nil (file-name-concat directory "evidence.txt") nil 'silent))
      (write-region "Other evidence" nil other nil 'silent)
      (write-region "SIBLING-ONLY-7193" nil (file-name-concat first-dir "AGENTS.md") nil 'silent)
      (write-region "BROAD-2358" nil (file-name-concat second-dir "AGENTS.md") nil 'silent)
      (write-region "LOCAL-4863" nil (file-name-concat second-dir "AGENTS.local.md") nil 'silent)
      (write-region "DEEP-3297" nil (file-name-concat deep-dir "AGENTS.md") nil 'silent)
      (mevedel-claude-code-register)
      (mevedel-model-set-session-provider
       session (mevedel-model-resolve-provider "Claude Code:sonnet") buffer)
      (mevedel-tool-ensure "Read")
      (unwind-protect
          (cl-letf (((symbol-function 'mevedel-claude-code-launch)
                     (lambda (system mcp _model _effort &optional id hook)
                       (let* ((first (equal system "first"))
                              (file (file-name-concat (if first first-dir deep-dir) "evidence.txt")))
                         (list :command (executable-find "python3")
                               :args (list mevedel-claude-code-context-test--peer)
                               :cwd root :mcp mcp :session-id id
                               :tool-id-field :claudecode/toolUseId
                               :control #'mevedel-claude-code--control
                               :check-context #'mevedel-claude-code-context-check
                               :observe #'mevedel-claude-code-context-observe
                               :meta `((hookCommand . ,hook) (fixtureSessionId . ,system)
                                       (compactBeforeBatch . ,(unless first 1))
                                       (responseText . "Completed")
                                       (toolBatches . [[((name . "Read") (id . "scoped-read") (args . ((file_path . ,file))))]
                                                      [((name . "Read") (id . "other-read") (args . ((file_path . ,other))))]])))))))
            (dolist (name '("first" "second"))
              (mevedel-agent-control-spawn
               session name "Read the assigned evidence."
               (lambda (value) (should-not (plist-get value :error)) (push (plist-get value :record) records))
               :agent (mevedel-agent--create :name name :description "Read evidence" :tools '(Read) :system-prompt name))
              (with-timeout (5 (ert-fail "Scoped child did not settle"))
                (while (mevedel-agent-control-active-turn-p session) (accept-process-output nil 0.01)))
              (should (string-suffix-p "Completed" (mevedel-agent-record-settled-result (car records)))))
            (with-current-buffer (mevedel-agent-record-conversation-buffer (cadr records))
              (should (= 1 (mevedel-engine-test--count-evidence "SIBLING-ONLY-7193"))))
            (with-current-buffer (mevedel-agent-record-conversation-buffer (car records))
              (should-not (string-search "SIBLING-ONLY-7193" (mevedel-engine-test--evidence)))
              (dolist (marker '("BROAD-2358" "LOCAL-4863" "DEEP-3297"))
                (should (= 2 (mevedel-engine-test--count-evidence marker))))
              (let ((evidence (mevedel-engine-test--evidence)))
                (with-temp-buffer
                  (insert evidence)
                  (goto-char (point-min))
                  (dotimes (_ 2)
                    (should (search-forward "BROAD-2358" nil t))
                    (should (search-forward "LOCAL-4863" nil t))
                    (should (search-forward "DEEP-3297" nil t)))))))
        (dolist (record records)
          (when (buffer-live-p (mevedel-agent-record-conversation-buffer record))
            (kill-buffer (mevedel-agent-record-conversation-buffer record))))))))

(mevedel-deftest mevedel--start-directive-discussion/claude-path-scope (:quiet t)
  (mevedel-engine-test--with-session
    (let* ((gptel--known-backends nil)
           (gptel--known-presets (copy-tree gptel--known-presets))
           (mevedel-preset--registry (copy-tree mevedel-preset--registry))
           (mevedel-show-chat-buffer nil) (inhibit-interaction t)
           (mevedel-claude-code-directory (file-name-concat root "claude"))
           (directory (file-name-concat root "lib"))
           (private-directory (file-name-concat root "private"))
           (file (file-name-concat directory "evidence.txt"))
           (private-file (file-name-concat private-directory "evidence.txt"))
           (source (find-file-noselect (file-name-concat root "directive.txt")))
           directive outcome)
      (unwind-protect
          (progn
            (dolist (dir (list directory private-directory)) (make-directory dir))
            (dolist (path (list file private-file)) (write-region "Evidence" nil path nil 'silent))
            (write-region "DIRECTIVE-PATH-4361" nil (file-name-concat directory "AGENTS.md") nil 'silent)
            (write-region "ROOT-ONLY-PATH-6372" nil (file-name-concat private-directory "AGENTS.md") nil 'silent)
            (mevedel-tools-register)
            (mevedel--define-presets)
            (mevedel-claude-code-register)
            (mevedel-model-set-session-provider
             session (mevedel-model-resolve-provider "Claude Code:sonnet") buffer)
            (setq-local gptel-system-prompt "Root instructions"
                        gptel-tools (list (mevedel-tool-gptel-tool (mevedel-tool-ensure "Read"))))
            (with-current-buffer source
              (setq-local mevedel--workspace workspace)
              (insert "alpha\n") (save-buffer)
              (setq directive (mevedel--create-directive-in source (point-min) (1- (point-max)) nil "Inspect the scoped file"))
              (setf (mevedel-directive-session-id (mevedel--directive-record directive)) (mevedel-session-session-id session)))
            (cl-letf (((symbol-function 'mevedel-claude-code-launch)
                       (lambda (_system mcp _model _effort &optional id hook)
                         (let ((scoped (mevedel-request-directive-uuid mevedel--current-request)))
                           (list :command (executable-find "python3")
                                 :args (list mevedel-claude-code-context-test--peer)
                                 :cwd root :mcp mcp :session-id id
                                 :tool-id-field :claudecode/toolUseId
                                 :control #'mevedel-claude-code--control
                                 :check-context #'mevedel-claude-code-context-check
                                 :observe #'mevedel-claude-code-context-observe
                                 :meta `((hookCommand . ,hook) (fixtureSessionId . ,(if scoped "directive" "root"))
                                         (compactBeforeBatch . ,(when scoped 1)) (responseText . "Completed")
                                         (toolBatches . ,(if scoped
                                                             (vector (vector `((name . "Read") (id . "directive-read") (args . ((file_path . ,file)))))
                                                                     (vector `((name . "Read") (id . "directive-again") (args . ((file_path . ,file))))))
                                                           (vector (vector `((name . "Read") (id . "root-read") (args . ((file_path . ,file))))
                                                                           `((name . "Read") (id . "root-private") (args . ((file_path . ,private-file))))))))))))))
              (insert "Read the root's assigned files")
              (mevedel--send-request "Read the root's assigned files")
              (with-timeout (5 (ert-fail "Root path preparation did not settle"))
                (while (mevedel-turn-busy-p buffer) (accept-process-output nil 0.01)))
              (with-current-buffer source
                (mevedel--start-directive-discussion directive (lambda (err _owner) (setq outcome (if err err 'success)))))
              (with-timeout (5 (ert-fail "Directive path context did not settle"))
                (while (mevedel-turn-busy-p buffer) (accept-process-output nil 0.01)))
              (should (eq 'success outcome))
              (should (= 3 (mevedel-engine-test--count-evidence "DIRECTIVE-PATH-4361")))
              (should (= 1 (mevedel-engine-test--count-evidence "ROOT-ONLY-PATH-6372")))))
        (when (buffer-live-p source)
          (with-current-buffer source (set-buffer-modified-p nil))
          (kill-buffer source))))))

(provide 'test-mevedel-claude-code-context)
;;; test-mevedel-claude-code-context.el ends here
