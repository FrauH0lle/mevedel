;;; test-mevedel-claude-code-context.el --- Native context workflows -*- lexical-binding: t -*-

;;; Commentary:
;; Exercise context delivery through ordinary root and retained-agent turns.

;;; Code:

(require 'mevedel-engine-test-support
         (file-name-concat (file-name-directory (or load-file-name buffer-file-name))
                           "mevedel-engine-test-support"))
(require 'mevedel-claude-code-session)

(mevedel-deftest mevedel--send-request/claude-child-roster (:quiet t)
  (dolist (receipt '(t nil compact))
    (mevedel-engine-test--with-claude-session
      (let ((file (file-name-concat root "evidence.txt"))
            record root-turn)
        (write-region "Evidence" nil file nil 'silent)
        (setq-local gptel-system-prompt "Root fixture"
                    gptel-tools (list (mevedel-tool-gptel-tool (mevedel-tool-ensure "Read"))))
        (mevedel-session-add-reminder
         session (mevedel-reminders-make-user-revised-patch "ROOT-ONLY-REMINDER"))
        (unwind-protect
            (cl-letf (((symbol-function 'mevedel-claude-code-launch)
                       (mevedel-engine-test--claude-launch
                        (lambda (_system mcp _model _effort &optional id hook)
                          (list :meta `((hookCommand . ,hook)
                                        (compactBeforeBatch . ,(when (eq receipt 'compact) 0))
                                        (fixtureSessionId . ,(if root-turn "root" "child"))
                                        (promptAcknowledgement . ,(if (or (not root-turn) receipt) t :false))
                                        (responseText . "Completed")
                                        (toolBatches . ,(if root-turn
                                                            (vector (vector `((name . "Read") (id . "root-read")
                                                                              (args . ((file_path . ,file))))))
                                                          []))))))))
              (mevedel-agent-control-spawn
               session "reader" "Inspect this task."
               (lambda (value) (setq record (plist-get value :record)))
               :agent (mevedel-agent--create :name "reader" :description "Read evidence"
                                            :tools '(Read) :system-prompt "Child fixture"
                                            :reminders (list (mevedel-reminders-make-user-revised-patch "CHILD-ONLY-REMINDER"))))
              (mevedel-test--await 5 "Roster child did not settle"
                (not (mevedel-agent-control-active-turn-p session)))
              (should record)
              (with-current-buffer (mevedel-agent-record-conversation-buffer record)
                (should (= 1 (mevedel-engine-test--count-evidence "CHILD-ONLY-REMINDER")))
                (should-not (string-search "ROOT-ONLY-REMINDER" (mevedel-engine-test--evidence))))
              (setq root-turn t)
              (insert "Continue with the retained child")
              (mevedel--send-request "Continue with the retained child")
              (mevedel-test--await 5 "Roster root did not settle"
                (not (mevedel-turn-busy-p buffer)))
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
    (mevedel-engine-test--with-claude-session
      (let* ((file (file-name-concat root "evidence.txt"))
             (reminder (mevedel-reminders-make-user-revised-patch
                        (concat "REVISED-PATCH-7194 " (make-string 11000 ?x))))
             (commits 0) queued)
        (write-region "Evidence" nil file nil 'silent)
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
                   (mevedel-engine-test--claude-launch
                    (lambda (_system mcp _model _effort &optional id hook)
                      (list :observe (lambda (owner notification)
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
                                                    [((name . "Read") (id . "read-2") (args . ((file_path . ,file))))]])))))))
          (insert "Use current reminders")
          (mevedel--send-request "Use current reminders")
          (mevedel-test--await 5 "Reminder turn did not settle" (not (mevedel-turn-busy-p buffer)))
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
    (mevedel-engine-test--with-claude-session
      (let* ((file (file-name-concat root "evidence.txt"))
             (plan-path "local/plans/accepted-20261006-120000.md")
             (plan-file (file-name-concat (mevedel-session-save-path session) plan-path)))
        (write-region "Evidence" nil file nil 'silent)
        (mevedel-session-artifacts-publish-text
         session plan-file "# Accepted plan\n\nACCEPTED-PLAN-5192" 'utf-8-unix)
        (setf (mevedel-session-plan-metadata session)
              (list :status 'accepted :accepted-path plan-path))
        (mevedel-reminders-install-defaults session)
        (setq-local gptel-system-prompt "Plan reference fixture"
                    gptel-tools (list (mevedel-tool-gptel-tool (mevedel-tool-ensure "Read"))))
        (cl-letf (((symbol-function 'mevedel-claude-code-launch)
                   (mevedel-engine-test--claude-launch
                    (lambda (_system mcp _model _effort &optional id hook)
                      (list :meta `((hookCommand . ,hook) (compactBeforeBatch . 1)
                                    (compactAcknowledgement . ,(if receipt t :false))
                                    (responseText . "Completed")
                                    (toolBatches . [[((name . "Read") (id . "read-1") (args . ((file_path . ,file))))]
                                                    [((name . "Read") (id . "read-2") (args . ((file_path . ,file))))]])))))))
          (insert "Continue the accepted plan")
          (mevedel--send-request "Continue the accepted plan")
          (mevedel-test--await 5 "Accepted-plan turn did not settle"
            (not (mevedel-turn-busy-p buffer)))
          (should (eq (if receipt 'success 'error)
                      (plist-get (mevedel-engine-info request) :mevedel-acp-outcome)))
          (should (= (if receipt 2 1)
                     (mevedel-engine-test--count-evidence "ACCEPTED-PLAN-5192")))
          (should (= (if receipt 2 1)
                     (how-many "^#\\+begin_tool" (point-min) (point-max)))))))))

(mevedel-deftest mevedel--send-request/claude-compacted-path-instructions (:quiet t)
  (dolist (change '(unchanged updated removed missing-receipt oversized))
    (mevedel-engine-test--with-claude-session
      (let* ((directory (file-name-concat root "lib"))
             (file (file-name-concat directory "evidence.txt"))
             (instructions (file-name-concat directory "AGENTS.md"))
             (other (file-name-concat root "other.txt"))
             changed)
        (make-directory directory)
        (write-region "Evidence" nil file nil 'silent)
        (write-region "Other evidence" nil other nil 'silent)
        (write-region "ORIGINAL-PATH-GUIDANCE-5341" nil instructions nil 'silent)
        (setq-local gptel-system-prompt "Compacted path context fixture"
                    gptel-tools (list (mevedel-tool-gptel-tool (mevedel-tool-ensure "Read"))))
        (cl-letf (((symbol-function 'mevedel-claude-code-launch)
                   (mevedel-engine-test--claude-launch
                    (lambda (_system mcp _model _effort &optional id hook)
                      (list :observe (lambda (owner notification)
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
                                                                               (args . ((file_path . ,other))))]]))])))))))
          (insert "Continue with current path instructions after compaction")
          (mevedel--send-request "Continue with current path instructions after compaction")
          (mevedel-test--await 5 "Compacted path context did not settle"
            (not (mevedel-turn-busy-p buffer)))
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
  (mevedel-engine-test--with-claude-session
    (let* ((first-dir (file-name-concat root "first"))
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
      (mevedel-tool-ensure "Read")
      (unwind-protect
          (cl-letf (((symbol-function 'mevedel-claude-code-launch)
                     (mevedel-engine-test--claude-launch
                      (lambda (system mcp _model _effort &optional id hook)
                        (let* ((first (equal system "first"))
                               (file (file-name-concat (if first first-dir deep-dir) "evidence.txt")))
                          (list :meta `((hookCommand . ,hook) (fixtureSessionId . ,system)
                                        (compactBeforeBatch . ,(unless first 1))
                                        (responseText . "Completed")
                                        (toolBatches . [[((name . "Read") (id . "scoped-read") (args . ((file_path . ,file))))]
                                                        [((name . "Read") (id . "other-read") (args . ((file_path . ,other))))]]))))))))
            (dolist (name '("first" "second"))
              (mevedel-agent-control-spawn
               session name "Read the assigned evidence."
               (lambda (value) (should-not (plist-get value :error)) (push (plist-get value :record) records))
               :agent (mevedel-agent--create :name name :description "Read evidence" :tools '(Read) :system-prompt name))
              (mevedel-test--await 5 "Scoped child did not settle"
                (not (mevedel-agent-control-active-turn-p session)))
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
  (mevedel-engine-test--with-claude-session
    (let* ((gptel--known-presets (copy-tree gptel--known-presets))
           (mevedel-preset--registry (copy-tree mevedel-preset--registry))
           (mevedel-show-chat-buffer nil) (inhibit-interaction t)
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
            (setq-local gptel-system-prompt "Root instructions"
                        gptel-tools (list (mevedel-tool-gptel-tool (mevedel-tool-ensure "Read"))))
            (with-current-buffer source
              (setq-local mevedel--workspace workspace)
              (insert "alpha\n") (save-buffer)
              (setq directive (mevedel--create-directive-in source (point-min) (1- (point-max)) nil "Inspect the scoped file"))
              (setf (mevedel-directive-session-id (mevedel--directive-record directive)) (mevedel-session-session-id session)))
            (cl-letf (((symbol-function 'mevedel-claude-code-launch)
                       (mevedel-engine-test--claude-launch
                        (lambda (_system mcp _model _effort &optional id hook)
                          (let ((scoped (mevedel-request-directive-uuid mevedel--current-request)))
                            (list :meta `((hookCommand . ,hook) (fixtureSessionId . ,(if scoped "directive" "root"))
                                          (compactBeforeBatch . ,(when scoped 1)) (responseText . "Completed")
                                          (toolBatches . ,(if scoped
                                                              (vector (vector `((name . "Read") (id . "directive-read") (args . ((file_path . ,file)))))
                                                                      (vector `((name . "Read") (id . "directive-again") (args . ((file_path . ,file))))))
                                                            (vector (vector `((name . "Read") (id . "root-read") (args . ((file_path . ,file))))
                                                                            `((name . "Read") (id . "root-private") (args . ((file_path . ,private-file)))))))))))))))
              (insert "Read the root's assigned files")
              (mevedel--send-request "Read the root's assigned files")
              (mevedel-test--await 5 "Root path preparation did not settle"
                (not (mevedel-turn-busy-p buffer)))
              (with-current-buffer source
                (mevedel--start-directive-discussion directive (lambda (err _owner) (setq outcome (if err err 'success)))))
              (mevedel-test--await 5 "Directive path context did not settle"
                (not (mevedel-turn-busy-p buffer)))
              (should (eq 'success outcome))
              (should (= 3 (mevedel-engine-test--count-evidence "DIRECTIVE-PATH-4361")))
              (should (= 1 (mevedel-engine-test--count-evidence "ROOT-ONLY-PATH-6372")))))
        (when (buffer-live-p source)
          (with-current-buffer source (set-buffer-modified-p nil))
          (kill-buffer source))))))

;;; Direct delivery seams

(defun mevedel-claude-code-context-test--hook-receipt (body)
  "Return a successful SDK PostToolBatch receipt for hook BODY."
  `((method . "_claude/sdkMessage")
    (params (message (type . "system") (subtype . "hook_response")
                     (hook_event . "PostToolBatch") (outcome . "success") (exit_code . 0)
                     (stdout . ,(json-serialize
                                 (list :hookSpecificOutput
                                       (list :hookEventName "PostToolBatch"
                                             :additionalContext body))))))))

(defun mevedel-claude-code-context-test--prompt-text (owner history)
  "Return the text OWNER's prompt with native HISTORY submits."
  (mapconcat (lambda (part) (or (alist-get 'text part) ""))
             (mevedel-claude-code-context-prompt
              owner (vector '((type . "text") (text . "hi"))) history)
             "\n"))

(mevedel-deftest mevedel-claude-code-context-hook-fits-p ()
  ,test
  (test)
  :doc "measures the hook limit in UTF-16 units, so emoji count twice"
  (should (mevedel-claude-code-context-hook-fits-p (make-string 10000 ?x)))
  (should-not (mevedel-claude-code-context-hook-fits-p (make-string 10001 ?x)))
  (should (mevedel-claude-code-context-hook-fits-p (make-string 5000 #x1F600)))
  (should-not (mevedel-claude-code-context-hook-fits-p (make-string 5001 #x1F600))))

(mevedel-deftest mevedel-claude-code-context-prepare (:quiet t)
  ,test
  (test)
  :doc "a whole message beyond the hook limit takes the continuation route"
  (mevedel-engine-test--with-session
    (setf (mevedel-engine-info request) (list :buffer buffer))
    (mevedel-claude-code-context-system request "sys")
    (mevedel-agent-control--enqueue
     session "/root"
     (list :type 'RESULT :sender "/root/reader" :recipient "/root"
           :outcome 'completed :payload (make-string 15000 ?r) :timestamp (current-time)))
    (let ((body (mevedel-claude-code-context-prepare request)))
      (should (string-search (make-string 15000 ?r) body))
      (should (eq 'continuation (plist-get (plist-get (mevedel-engine-info request)
                                                     :mevedel-claude-context-pending)
                                          :route)))
      (should-error (mevedel-claude-code-context-check request)))
    (should (= 1 (length (mevedel-agent-control-context-mailbox session)))))

  :doc "a hook receipt records typed observations and mail without the wire marker"
  (mevedel-engine-test--with-session
    (insert "Question?\n\n")
    (setf (mevedel-engine-info request) (list :buffer buffer :position (point-max-marker)))
    (mevedel-claude-code-context-system request "sys")
    (setf (mevedel-engine-info request)
          (plist-put (mevedel-engine-info request) :mevedel-claude-observations nil))
    (mevedel-agent-control-send-message session "/root" "MAIL-5521")
    (let ((body (mevedel-claude-code-context-prepare request)))
      (should (string-prefix-p "<!-- mevedel-delivery:" body))
      (should (eq 'hook (plist-get (plist-get (mevedel-engine-info request)
                                             :mevedel-claude-context-pending)
                                  :route)))
      ;; The hook receipt can trail the next boundary, which then defers.
      (should-not (mevedel-claude-code-context-prepare request))
      (mevedel-claude-code-context-observe
       request (mevedel-claude-code-context-test--hook-receipt body)))
    (should-not (plist-get (mevedel-engine-info request) :mevedel-claude-context-pending))
    (should-not (mevedel-agent-control-context-mailbox session))
    (should-not (string-search "mevedel-delivery:" (buffer-string)))
    (should (string-search "MAIL-5521" (buffer-string)))
    (let ((types (mapcan (lambda (record)
                           (mapcar (lambda (item) (plist-get item :type)) (plist-get record :items)))
                         (mevedel-transcript-audit-records (buffer-string)))))
      (should (memq 'context-environment types)))))

(mevedel-deftest mevedel-claude-code-context-prompt (:quiet t)
  ,test
  (test)
  :doc "acknowledged unchanged path instructions are not re-sent"
  (mevedel-engine-test--with-session
    (let* ((dir (file-name-concat root "sub"))
           (file (file-name-concat dir "AGENTS.md")))
      (make-directory dir t)
      (write-region "SUBDIR-INSTRUCTIONS-8812" nil file nil 'silent)
      (setf (mevedel-session-workspace-instruction-hashes session)
            (list (cons (list "/root" file) (secure-hash 'sha256 "SUBDIR-INSTRUCTIONS-8812"))))
      (dolist (case '((nil . nil) ("SUBDIR-CHANGED-4410" . t)))
        (when (car case) (write-region (car case) nil file nil 'silent))
        (setf (mevedel-engine-info request) (list :buffer buffer))
        (mevedel-claude-code-context-system request "sys")
        (let ((text (mevedel-claude-code-context-test--prompt-text request '(:id "native" :state ready))))
          (should-not (string-search "SUBDIR-INSTRUCTIONS-8812" text))
          (should (eq (cdr case) (and (string-search "SUBDIR-CHANGED-4410" text) t)))))))

  :doc "selected user context is sent once per native conversation until it changes"
  (mevedel-engine-test--with-session
    (let ((history '(:id "native-1" :state ready))
          (count 0))
      (dolist (step '(("SELECTED-3391" t) ("SELECTED-3391" nil) ("SELECTED-4402" t)
                      (new "SELECTED-4402" t) (restore "SELECTED-4402" t)))
        (let* ((new (eq 'new (car step)))
               (restore (eq 'restore (car step)))
               (step (if (symbolp (car step)) (cdr step) step))
               (owner-history (if new '(:state ready) history)))
          (setf (mevedel-engine-info request)
                (list :buffer buffer :mevedel-claude-history history))
          (mevedel-claude-code-context-system request "sys" (list :mode 'user :text (car step)))
          (when restore
            (setf (mevedel-engine-info request)
                  (plist-put (mevedel-engine-info request) :mevedel-claude-restoration-pending nil))
            (mevedel-claude-code-context-restore request)
            (setf (mevedel-engine-info request)
                  (plist-put (mevedel-engine-info request) :mevedel-claude-restoration-pending nil)))
          (let* ((content (mevedel-claude-code-context-prompt
                           request (vector '((type . "text") (text . "hi"))) owner-history))
                 (text (mapconcat (lambda (part) (or (alist-get 'text part) "")) content "\n")))
            (cl-incf count)
            (ert-info ((format "step %d" count))
              (should (eq (cadr step) (and (string-search (car step) text) t))))
            ;; The exact user echo commits the dedup mark.
            (when (plist-get (mevedel-engine-info request) :mevedel-claude-input-pending)
              (mevedel-claude-code-context-observe
               request `((method . "_claude/sdkMessage")
                         (params (message (type . "user")
                                          (message (role . "user") (content . ,content)))))))))))))

(provide 'test-mevedel-claude-code-context)
;;; test-mevedel-claude-code-context.el ends here
