;;; test-niwa-claude-code-context.el --- Native context workflows -*- lexical-binding: t -*-

;;; Commentary:
;; Exercise context delivery through ordinary root and retained-agent turns.

;;; Code:

(require 'niwa-pending-inputs)
(require 'niwa-claude-code-context)
(require 'niwa-tools)

(require 'niwa-engine-test-support
         (file-name-concat (file-name-directory (or load-file-name buffer-file-name))
                           "niwa-engine-test-support"))
(require 'niwa-claude-code-session)
(require 'niwa-view-render)

(niwa-deftest niwa--send-request/claude-child-roster (:quiet t)
  (dolist (receipt '(t nil compact))
    (niwa-engine-test--with-claude-session
      :file-workspace (not (eq receipt t))
      (let ((file (file-name-concat root "evidence.txt"))
            record root-turn)
        (write-region "Evidence" nil file nil 'silent)
        (setq-local gptel-system-prompt "Root fixture"
                    gptel-tools (list (niwa-tool-gptel-tool (niwa-tool-ensure "Read"))))
        (niwa-session-add-reminder
         session (niwa-reminders-make-user-revised-patch "ROOT-ONLY-REMINDER"))
        (unwind-protect
            (cl-letf (((symbol-function 'niwa-claude-code-launch)
                       (niwa-engine-test--claude-launch
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
              (niwa-agent-control-spawn
               session "reader" "Inspect this task."
               (lambda (value) (setq record (plist-get value :record)))
               :agent (niwa-agent--create :name "reader" :description "Read evidence"
                                            :tools '(Read) :system-prompt "Child fixture"
                                            :reminders (list (niwa-reminders-make-user-revised-patch "CHILD-ONLY-REMINDER"))))
              (niwa-test--await 5 "Roster child did not settle"
                (not (niwa-agent-control-active-turn-p session)))
              (should record)
              (with-current-buffer (niwa-agent-record-conversation-buffer record)
                (should (= 1 (niwa-engine-test--count-evidence "CHILD-ONLY-REMINDER")))
                (should-not (string-search "ROOT-ONLY-REMINDER" (niwa-engine-test--evidence))))
              (setq root-turn t)
              (insert "Continue with the retained child")
              (niwa--send-request "Continue with the retained child")
              (niwa-test--await 5 "Roster root did not settle"
                (not (niwa-turn-busy-p buffer)))
              (ert-info ((format "receipt=%S error=%S" receipt
                                 (plist-get (niwa-engine-info request) :error)))
                (should (eq (if receipt 'success 'error)
                            (plist-get (niwa-engine-info request) :niwa-acp-outcome))))
              (should (= (pcase receipt ('compact 2) ('nil 0) (_ 1))
                         (niwa-engine-test--count-evidence "<agent-roster>")))
              (when receipt
                (should (string-search "- `/root/reader` (`reader`)" (niwa-engine-test--evidence)))
                (should (= 1 (niwa-engine-test--count-evidence "ROOT-ONLY-REMINDER"))))
              (should-not (string-search "CHILD-ONLY-REMINDER" (niwa-engine-test--evidence)))
              (should (= (if receipt 1 0) (how-many "^#\\+begin_tool" (point-min) (point-max)))))
          (when (and record (buffer-live-p (niwa-agent-record-conversation-buffer record)))
            (kill-buffer (niwa-agent-record-conversation-buffer record))))))))

(niwa-deftest niwa--send-request/claude-reminders (:quiet t)
  (dolist (receipt '(t nil late silent compact))
    (niwa-engine-test--with-claude-session
      :file-workspace (not (eq receipt t))
      (let* ((file (file-name-concat root "evidence.txt"))
             (reminder (niwa-reminders-make-user-revised-patch
                        (concat "REVISED-PATCH-7194 " (make-string 11000 ?x))))
             (commits 0) queued)
        (write-region "Evidence" nil file nil 'silent)
        (setq-local gptel-system-prompt "Reminder fixture"
                    gptel-tools (list (niwa-tool-gptel-tool (niwa-tool-ensure "Read"))))
        (unless (eq receipt 'silent)
          (niwa-reminders-install-defaults session)
          (niwa-session-add-reminder session reminder)
          (niwa-session-enqueue-pending-reminder session "QUEUED-BEFORE-2194")
          (niwa-hooks-record-session-context
           session '(:additional-context ("INITIAL-HOOK-4813")) 'SessionStart))
        (when (eq receipt 'compact) (setf (niwa-session-plan-mode session) t))
        (niwa-session-add-reminder
         session (niwa-reminder-create
                  :type 'silent :trigger (lambda (_) t) :interval 'one-shot
                  :content (lambda (_) (list :commit (lambda () (cl-incf commits))))))
        (cl-letf (((symbol-function 'niwa-claude-code-launch)
                   (niwa-engine-test--claude-launch
                    (lambda (_system mcp _model _effort &optional id hook)
                      (list :observe (lambda (owner notification)
                                       (when (and (eq receipt 'late) (not queued)
                                                  (equal "user" (alist-get 'type (alist-get 'message (alist-get 'params notification)))))
                                         (setq queued t)
                                         (niwa-session-enqueue-pending-reminder session "QUEUED-LATER-4529")
                                         (niwa-hooks-record-session-context
                                          session '(:additional-context ("LATER-HOOK-2974")) 'SessionStart))
                                       (niwa-claude-code-context-observe owner notification))
                            :meta `((hookCommand . ,hook) (promptAcknowledgement . ,(if receipt t :false))
                                    (compactBeforeBatch . ,(when (eq receipt 'compact) 0))
                                    (responseText . "Completed")
                                    (toolBatches . [[((name . "Read") (id . "read-1") (args . ((file_path . ,file))))]
                                                    [((name . "Read") (id . "read-2") (args . ((file_path . ,file))))]])))))))
          (insert "Use current reminders")
          (niwa--send-request "Use current reminders")
          (niwa-test--await 5 "Reminder turn did not settle" (not (niwa-turn-busy-p buffer)))
          (ert-info ((format "receipt=%S" receipt))
            (should (eq (if receipt 'success 'error)
                        (plist-get (niwa-engine-info request) :niwa-acp-outcome)))
            (should (= (if receipt 1 0) commits))
            (let* ((records (niwa-transcript-audit-records
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
                (let* ((turns (niwa-view--group-into-turns
                               (niwa-transcript-segments (point-min) (point-max)) buffer))
                       (audits (niwa-view--user-turn-hook-audits
                                (plist-get (car turns) :segments) buffer)))
                  (should (eq 'user (plist-get (car turns) :role)))
                  (should (member (list :type (plist-get typed :type)
                                        :phase (plist-get typed :phase)
                                        :items (plist-get typed :items))
                                  (mapcar (lambda (r)
                                            (list :type (plist-get r :type)
                                                  :phase (plist-get r :phase)
                                                  :items (plist-get r :items))) audits))))))
            (should (= (if receipt 2 0) (how-many "^#\\+begin_tool" (point-min) (point-max))))
            (should (= (if (memq receipt '(t late compact)) 1 0)
                       (niwa-engine-test--count-evidence "REVISED-PATCH-7194")))
            (should (equal (pcase receipt ('nil '("QUEUED-BEFORE-2194")) ('late '("QUEUED-LATER-4529")))
                           (niwa-session-pending-reminders session)))
            (should (= (if (memq receipt '(t late compact)) 1 0)
                       (niwa-engine-test--count-evidence "INITIAL-HOOK-4813")))
            (should (equal (pcase receipt ('nil '("INITIAL-HOOK-4813")) ('late '("LATER-HOOK-2974")))
                           (mapcar (lambda (entry) (plist-get entry :body))
                                   (niwa-session-hook-context-pending session))))
            (should (= (if (eq receipt 'compact) 1 0)
                       (niwa-engine-test--count-evidence "Plan mode is active")))
            (should (= (if (eq receipt 'compact) 1 0)
                       (niwa-engine-test--count-evidence "Plan mode remains active")))
            (when (eq receipt 'compact)
              (should (string-search "niwa://plan-mode.md"
                                     (niwa-engine-test--evidence))))
            (should-not (string-search "Context pressure is high" (niwa-engine-test--evidence)))))))))

(niwa-deftest niwa--send-request/claude-compacted-plan-reference (:quiet t)
  (dolist (receipt '(t nil))
    (niwa-engine-test--with-claude-session
      (let* ((file (file-name-concat root "evidence.txt"))
             (plan-path "local/plans/accepted-20261006-120000.md")
             (plan-file (file-name-concat (niwa-session-save-path session) plan-path)))
        (write-region "Evidence" nil file nil 'silent)
        (niwa-session-artifacts-publish-text
         session plan-file "# Accepted plan\n\nACCEPTED-PLAN-5192" 'utf-8-unix)
        (setf (niwa-session-plan-metadata session)
              (list :status 'accepted :accepted-path plan-path))
        (niwa-reminders-install-defaults session)
        (setq-local gptel-system-prompt "Plan reference fixture"
                    gptel-tools (list (niwa-tool-gptel-tool (niwa-tool-ensure "Read"))))
        (cl-letf (((symbol-function 'niwa-claude-code-launch)
                   (niwa-engine-test--claude-launch
                    (lambda (_system mcp _model _effort &optional id hook)
                      (list :meta `((hookCommand . ,hook) (compactBeforeBatch . 1)
                                    (compactAcknowledgement . ,(if receipt t :false))
                                    (responseText . "Completed")
                                    (toolBatches . [[((name . "Read") (id . "read-1") (args . ((file_path . ,file))))]
                                                    [((name . "Read") (id . "read-2") (args . ((file_path . ,file))))]])))))))
          (insert "Continue the accepted plan")
          (niwa--send-request "Continue the accepted plan")
          (niwa-test--await 5 "Accepted-plan turn did not settle"
            (not (niwa-turn-busy-p buffer)))
          (should (eq (if receipt 'success 'error)
                      (plist-get (niwa-engine-info request) :niwa-acp-outcome)))
          (should (= (if receipt 2 1)
                     (niwa-engine-test--count-evidence "ACCEPTED-PLAN-5192")))
          (should (= (if receipt 2 1)
                     (how-many "^#\\+begin_tool" (point-min) (point-max)))))))))

(niwa-deftest niwa--send-request/claude-compacted-path-instructions (:quiet t)
  (dolist (change '(unchanged updated removed missing-receipt oversized))
    (niwa-engine-test--with-claude-session
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
                    gptel-tools (list (niwa-tool-gptel-tool (niwa-tool-ensure "Read"))))
        (cl-letf (((symbol-function 'niwa-claude-code-launch)
                   (niwa-engine-test--claude-launch
                    (lambda (_system mcp _model _effort &optional id hook)
                      (list :observe (lambda (owner notification)
                                       (niwa-claude-code-context-observe owner notification)
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
          (niwa--send-request "Continue with current path instructions after compaction")
          (niwa-test--await 5 "Compacted path context did not settle"
            (not (niwa-turn-busy-p buffer)))
          (ert-info ((format "change=%S" change))
            (should (eq (if (eq change 'missing-receipt) 'error 'success)
                        (plist-get (niwa-engine-info request) :niwa-acp-outcome)))
            (should (= (if (eq change 'missing-receipt) 1 2)
                       (how-many "^#\\+begin_tool" (point-min) (point-max))))
            (should (= (if (eq change 'unchanged) 2 1)
                       (niwa-engine-test--count-evidence "ORIGINAL-PATH-GUIDANCE-5341")))
            (should (= (if (memq change '(updated oversized)) 1 0)
                       (niwa-engine-test--count-evidence "UPDATED-PATH-GUIDANCE-6759")))
            (should (= (if (eq change 'removed) 1 0)
                       (niwa-engine-test--count-evidence "no longer present"))))
          (should (= 1 (niwa-session-turn-count session))))))))

(niwa-deftest niwa-agent-control-spawn/claude-compacted-path-scope (:quiet t)
  (niwa-engine-test--with-claude-session
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
      (niwa-tool-ensure "Read")
      (unwind-protect
          (cl-letf (((symbol-function 'niwa-claude-code-launch)
                     (niwa-engine-test--claude-launch
                      (lambda (system mcp _model _effort &optional id hook)
                        (let* ((first (equal system "first"))
                               (file (file-name-concat (if first first-dir deep-dir) "evidence.txt")))
                          (list :meta `((hookCommand . ,hook) (fixtureSessionId . ,system)
                                        (compactBeforeBatch . ,(unless first 1))
                                        (responseText . "Completed")
                                        (toolBatches . [[((name . "Read") (id . "scoped-read") (args . ((file_path . ,file))))]
                                                        [((name . "Read") (id . "other-read") (args . ((file_path . ,other))))]]))))))))
            (dolist (name '("first" "second"))
              (niwa-agent-control-spawn
               session name "Read the assigned evidence."
               (lambda (value) (should-not (plist-get value :error)) (push (plist-get value :record) records))
               :agent (niwa-agent--create :name name :description "Read evidence" :tools '(Read) :system-prompt name))
              (niwa-test--await 5 "Scoped child did not settle"
                (not (niwa-agent-control-active-turn-p session)))
              (should (string-suffix-p "Completed" (niwa-agent-record-settled-result (car records)))))
            (with-current-buffer (niwa-agent-record-conversation-buffer (cadr records))
              (should (= 1 (niwa-engine-test--count-evidence "SIBLING-ONLY-7193"))))
            (with-current-buffer (niwa-agent-record-conversation-buffer (car records))
              (should-not (string-search "SIBLING-ONLY-7193" (niwa-engine-test--evidence)))
              (dolist (marker '("BROAD-2358" "LOCAL-4863" "DEEP-3297"))
                (should (= 2 (niwa-engine-test--count-evidence marker))))
              (let ((evidence (niwa-engine-test--evidence)))
                (with-temp-buffer
                  (insert evidence)
                  (goto-char (point-min))
                  (dotimes (_ 2)
                    (should (search-forward "BROAD-2358" nil t))
                    (should (search-forward "LOCAL-4863" nil t))
                    (should (search-forward "DEEP-3297" nil t)))))))
        (dolist (record records)
          (when (buffer-live-p (niwa-agent-record-conversation-buffer record))
            (kill-buffer (niwa-agent-record-conversation-buffer record))))))))

;;; Direct delivery seams

(defun niwa-claude-code-context-test--hook-receipt (body)
  "Return a successful SDK PostToolBatch receipt for hook BODY."
  `((method . "_claude/sdkMessage")
    (params (message (type . "system") (subtype . "hook_response")
                     (hook_event . "PostToolBatch") (outcome . "success") (exit_code . 0)
                     (stdout . ,(json-serialize
                                 (list :hookSpecificOutput
                                       (list :hookEventName "PostToolBatch"
                                             :additionalContext body))))))))

(defun niwa-claude-code-context-test--steer (session request text)
  "Queue TEXT in SESSION as steering for REQUEST or a request id."
  (niwa-session-enqueue-pending-input
   session 'steering
   (list :input text :model-input text :transcript-payload text
         :request-id (if (stringp request) request (niwa-request-id request)))))

(defun niwa-claude-code-context-test--echo (content)
  "Return the SDK user-message echo of submitted ACP CONTENT."
  (mapcar (lambda (part)
            (if (equal "image" (alist-get 'type part))
                `((type . "image")
                  (source (type . "base64") (data . ,(alist-get 'data part))
                          (media_type . ,(alist-get 'mimeType part))))
              part))
          content))

(defun niwa-claude-code-context-test--user-receipt (content)
  "Return a top-level SDK user receipt for submitted ACP CONTENT."
  `((method . "_claude/sdkMessage")
    (params (message (type . "user")
                     (message (role . "user")
                              (content . ,(niwa-claude-code-context-test--echo content)))))))

(defun niwa-claude-code-context-test--prompt-text (owner history)
  "Return the text OWNER's prompt with native HISTORY submits."
  (mapconcat (lambda (part) (or (alist-get 'text part) ""))
             (niwa-claude-code-context-prompt
              owner (vector '((type . "text") (text . "hi"))) history)
             "\n"))

(niwa-deftest niwa-claude-code-context-hook-fits-p ()
  ,test
  (test)
  :doc "measures the hook limit in UTF-16 units, so emoji count twice"
  (should (niwa-claude-code-context-hook-fits-p (make-string 10000 ?x)))
  (should-not (niwa-claude-code-context-hook-fits-p (make-string 10001 ?x)))
  (should (niwa-claude-code-context-hook-fits-p (make-string 5000 #x1F600)))
  (should-not (niwa-claude-code-context-hook-fits-p (make-string 5001 #x1F600))))

(niwa-deftest niwa-claude-code-context-prepare (:quiet t)
  ,test
  (test)
  :doc "a native receipt consumes captured reports but preserves later waking mail"
  (niwa-engine-test--with-session
    (insert "Question?\n\n")
    (setf (niwa-engine-info request) (list :buffer buffer :position (point-max-marker)))
    (niwa-claude-code-context-system request "sys")
    (let* ((first (list :type 'MAIL :sender "/root/reader" :recipient "/root"
                        :payload "FIRST-REPORT-7192" :wake t :timestamp (current-time)))
           (later (list :type 'RESULT :sender "/root/reader" :recipient "/root"
                        :outcome 'completed :payload "LATER-RESULT-8821"
                        :wake t :timestamp (current-time))))
      (niwa-agent-control--enqueue session "/root" first)
      (let ((body (niwa-claude-code-context-prepare request)))
        (should (string-search "FIRST-REPORT-7192" body))
        (niwa-agent-control--enqueue session "/root" later)
        (niwa-claude-code-context-observe
         request (niwa-claude-code-context-test--hook-receipt body))
        (should (equal (list later) (niwa-agent-control-context-mailbox session)))
        (should (niwa-agent-control-wake-pending-p session))
        ;; A duplicate receipt cannot acknowledge the later report.
        (niwa-claude-code-context-observe
         request (niwa-claude-code-context-test--hook-receipt body)))
      (should (string-search "FIRST-REPORT-7192" (buffer-string)))
      (should-not (string-search "LATER-RESULT-8821" (buffer-string)))
      (let ((body (niwa-claude-code-context-prepare request)))
        (should (string-search "LATER-RESULT-8821" body))
        (niwa-claude-code-context-observe
         request (niwa-claude-code-context-test--hook-receipt body)))
      (should (string-search "LATER-RESULT-8821" (buffer-string)))
      (should-not (niwa-agent-control-wake-pending-p session))))

  :doc "native redelivery skips a received durable ID but keeps distinct equal mail"
  (niwa-engine-test--with-session
    (insert "Question?\n\n")
    (setf (niwa-engine-info request) (list :buffer buffer :position (point-max-marker)))
    (niwa-claude-code-context-system request "sys")
    (niwa-agent-control-send-message session "/root" "SAME-REPORT")
    (let ((durable (copy-tree (niwa-session-messages session))))
      (let ((body (niwa-claude-code-context-prepare request)))
        (niwa-claude-code-context-observe
         request (niwa-claude-code-context-test--hook-receipt body)))
      (setf (niwa-session-messages session) durable)
      (niwa-agent-control-send-message session "/root" "SAME-REPORT")
      (let ((body (niwa-claude-code-context-prepare request)))
        (should (= 1 (length (niwa-session-messages session))))
        (should (string-search "SAME-REPORT" body))
        (niwa-claude-code-context-observe
         request (niwa-claude-code-context-test--hook-receipt body)))
      (should-not (niwa-session-messages session))
      (let ((receipts (niwa-transcript-audit-records (buffer-string) 'mailbox-receipt)))
        (should (= 2 (length receipts)))
        (should-not (equal (plist-get (car receipts) :id)
                           (plist-get (cadr receipts) :id))))))

  :doc "a whole message beyond the hook limit takes the continuation route"
  (niwa-engine-test--with-session
    (setf (niwa-engine-info request) (list :buffer buffer))
    (niwa-claude-code-context-system request "sys")
    (niwa-agent-control--enqueue
     session "/root"
     (list :type 'RESULT :sender "/root/reader" :recipient "/root"
           :outcome 'completed :payload (make-string 15000 ?r) :timestamp (current-time)))
    (let ((body (niwa-claude-code-context-prepare request)))
      (should (string-search (make-string 15000 ?r) body))
      (should (eq 'continuation (plist-get (plist-get (niwa-engine-info request)
                                                     :niwa-claude-context-pending)
                                          :route)))
      (should-error (niwa-claude-code-context-check request)))
    (should (= 1 (length (niwa-agent-control-context-mailbox session)))))

  :doc "a hook receipt records typed observations and mail without the wire marker"
  (niwa-engine-test--with-session
    (insert "Question?\n\n")
    (setf (niwa-engine-info request) (list :buffer buffer :position (point-max-marker)))
    (niwa-claude-code-context-system request "sys")
    (setf (niwa-engine-info request)
          (plist-put (niwa-engine-info request) :niwa-claude-observations nil))
    (niwa-agent-control-send-message session "/root" "MAIL-5521")
    (let ((body (niwa-claude-code-context-prepare request)))
      (should (string-prefix-p "<!-- niwa-delivery:" body))
      (should (eq 'hook (plist-get (plist-get (niwa-engine-info request)
                                             :niwa-claude-context-pending)
                                  :route)))
      ;; The hook receipt can trail the next boundary, which then defers.
      (should-not (niwa-claude-code-context-prepare request))
      (niwa-claude-code-context-observe
       request (niwa-claude-code-context-test--hook-receipt body)))
    (should-not (plist-get (niwa-engine-info request) :niwa-claude-context-pending))
    (should-not (niwa-agent-control-context-mailbox session))
    (should-not (string-search "niwa-delivery:" (buffer-string)))
    (should (string-search "MAIL-5521" (buffer-string)))
    (let ((types (mapcan (lambda (record)
                           (mapcar (lambda (item) (plist-get item :type)) (plist-get record :items)))
                         (niwa-transcript-audit-records (buffer-string)))))
      (should (memq 'context-environment types))))

  :doc "steering stops at a boundary and becomes user input only on its user echo"
  (niwa-engine-test--with-session
    (insert "Question?\n\n")
    (setf (niwa-engine-info request) (list :buffer buffer :position (point-max-marker)))
    (niwa-claude-code-context-system request "sys")
    (niwa-claude-code-context-test--steer session request "STEER-7731")
    (niwa-claude-code-context-test--steer session "another-request" "OTHER-STEER")
    (let ((body (niwa-claude-code-context-prepare request)))
      (should-not (string-search "STEER-7731" body))
      (should-not (string-search "OTHER-STEER" body))
      ;; Sending is not delivery: the entry stays queued and tools wait.
      (should (= 2 (length (niwa-session-pending-steering session))))
      (let ((entry (car (niwa-session-pending-steering session))))
        (should (plist-get entry :delivering))
        (should-error (niwa-pending-inputs--assert-removable session entry)
                      :type 'user-error))
      (should-error (niwa-claude-code-context-check request))
      (niwa-claude-code-context-observe
       request (niwa-claude-code-context-test--hook-receipt body))
      (should (= 2 (length (niwa-session-pending-steering session))))
      (should-not (string-search "STEER-7731" (buffer-string)))
      (let ((content (niwa-claude-code-context-next-prompt request)))
        (should (equal '((type . "text") (text . "STEER-7731")) (aref content (1- (length content)))))
        (niwa-claude-code-context-observe
         request (niwa-claude-code-context-test--user-receipt content))))
    (should (equal '("OTHER-STEER")
                   (mapcar (lambda (entry) (plist-get entry :input))
                           (niwa-session-pending-steering session))))
    (should-not (niwa-claude-code-context-check request))
    (should (= 1 (how-many "STEER-7731" (point-min) (point-max)))))

  :doc "steering images stop at the batch and continue the turn as a prompt"
  (niwa-engine-test--with-session
    (let ((image '((type . "image") (mimeType . "image/png") (data . "UElYRUxT"))))
      (setf (niwa-engine-info request) (list :buffer buffer :position (point-max-marker)))
      (niwa-claude-code-context-system request "sys")
      (niwa-claude-code-context-test--steer session request "IMAGE-STEER-3108")
      (cl-letf (((symbol-function 'niwa-claude-code-context--images)
                 (lambda (contexts &optional _model) (and contexts (list image))))
                ((symbol-function 'niwa-mentions-expand-user-input)
                 (lambda (text &rest _)
                   (list :text text :media-contexts '(("board.png" "image/png"))))))
        (let* ((decision (niwa-claude-code--control
                          request '(:hook_event_name "PostToolBatch")))
               (pending (plist-get (niwa-engine-info request)
                                   :niwa-claude-context-pending)))
          ;; Neither authored text nor its image rides hook context.
          (should (eq :json-false (plist-get decision :continue)))
          (should (niwa-claude-code-context-hook-fits-p (plist-get pending :body)))
          (should (eq 'continuation (plist-get pending :route)))
          (should (equal (list image) (plist-get pending :images)))
          ;; A call attempted before the continuation is denied, not fed hook text.
          (should (equal "deny"
                         (plist-get (plist-get (niwa-claude-code--control
                                                request '(:hook_event_name "PreToolUse"))
                                               :hookSpecificOutput)
                                    :permissionDecision)))
          (let ((content (niwa-claude-code-context-next-prompt request)))
            (should (equal image (aref content (1- (length content)))))
            ;; Text alone does not deliver the image.
            (niwa-claude-code-context-observe
             request `((method . "_claude/sdkMessage")
                       (params (message (type . "user")
                                        (message (role . "user")
                                                 (content . ,(seq-remove
                                                              (lambda (part) (alist-get 'mimeType part))
                                                              content)))))))
            (should (= 1 (length (niwa-session-pending-steering session))))
            (niwa-claude-code-context-observe
             request `((method . "_claude/sdkMessage")
                       (params (message (type . "user")
                                        (message (role . "user")
                                                 (content . ,(niwa-claude-code-context-test--echo
                                                              content))))))))
          (should-not (niwa-session-pending-steering session))
          (should (= 1 (how-many "IMAGE-STEER-3108" (point-min) (point-max))))))))

  :doc "steering waits for outstanding hook mail and never consumes its receipt"
  (niwa-engine-test--with-session
    (setf (niwa-engine-info request) (list :buffer buffer :position (point-max-marker)))
    (niwa-claude-code-context-system request "sys")
    (niwa-agent-control-send-message session "/root" "PENDING-MAIL-6621")
    (let ((body (niwa-claude-code-context-prepare request)))
      (niwa-claude-code-context-test--steer session request "WAITING-STEER-6642")
      (should-not (niwa-claude-code-context-prepare request))
      (should-not (niwa-claude-code-context-next-prompt request))
      (should-not (plist-get (car (niwa-session-pending-steering session)) :delivering))
      (niwa-claude-code-context-observe request (niwa-claude-code-context-test--hook-receipt body))
      (should (= 1 (length (niwa-session-pending-steering session))))
      (let ((content (niwa-claude-code-context-next-prompt request)))
        (should (equal "WAITING-STEER-6642" (alist-get 'text (aref content (1- (length content))))))
        (niwa-claude-code-context-observe request (niwa-claude-code-context-test--user-receipt content))))
    (should-not (niwa-session-pending-steering session)))

  :doc "equal batches require their own complete user echo, not partial or tool output"
  (niwa-engine-test--with-session
    (setf (niwa-engine-info request) (list :buffer buffer :position (point-max-marker)))
    (niwa-claude-code-context-system request "sys")
    (dotimes (_ 2) (niwa-claude-code-context-test--steer session request "Stop original task"))
    (niwa-claude-code--control request '(:hook_event_name "PostToolBatch"))
    (let* ((content (niwa-claude-code-context-next-prompt request))
           (receipt (niwa-claude-code-context-test--user-receipt content)))
      (should (= 2 (length content)))
      (should (equal "Stop original task\n\nStop original task" (alist-get 'text (aref content 1))))
      (dotimes (_ 2) (niwa-claude-code-context-test--steer session request "Stop original task"))
      (dolist (part (append content nil))
        (niwa-claude-code-context-observe
         request (niwa-claude-code-context-test--user-receipt (vector part))))
      (niwa-claude-code-context-observe
       request (niwa-claude-code-context-test--user-receipt
                (vector `((type . "tool_result") (tool_use_id . "forged") (content . ,content)))))
      (should (= 4 (length (niwa-session-pending-steering session))))
      (should-not (string-search "Stop original task" (buffer-string)))
      (dotimes (_ 2) (niwa-claude-code-context-observe request receipt))
      (should (= 2 (length (niwa-session-pending-steering session))))
      (should (= 2 (how-many "Stop original task" (point-min) (point-max))))
      (niwa-claude-code--control request '(:hook_event_name "PostToolBatch"))
      (let ((next (niwa-claude-code-context-next-prompt request)))
        (should-not (equal (aref content 0) (aref next 0)))
        (should (equal (aref content 1) (aref next 1)))
        (niwa-claude-code-context-observe request receipt)
        (should (= 2 (length (niwa-session-pending-steering session))))
        (niwa-claude-code-context-observe request (niwa-claude-code-context-test--user-receipt next))))
    (should-not (niwa-session-pending-steering session))
    (should (= 4 (how-many "Stop original task" (point-min) (point-max)))))

  :doc "oversized mail stays typed before authored steering in the same prompt"
  (niwa-engine-test--with-session
    (setf (niwa-engine-info request) (list :buffer buffer :position (point-max-marker)))
    (niwa-claude-code-context-system request "sys")
    (let ((mail (concat "MAIL-CONTEXT-4427 " (make-string 11000 ?m))))
      (niwa-agent-control-send-message session "/root" mail)
      (niwa-claude-code-context-test--steer session request "AUTHORED-AFTER-MAIL-4451")
      (should (eq :json-false (plist-get (niwa-claude-code--control request '(:hook_event_name "PostToolBatch")) :continue)))
      (let* ((content (niwa-claude-code-context-next-prompt request))
             (body (alist-get 'text (aref content 0)))
             (authored (alist-get 'text (aref content 1))))
        (should (string-search "<agent-message" body))
        (should (string-search mail body))
        (should-not (string-search "AUTHORED-AFTER-MAIL-4451" body))
        (should (equal "AUTHORED-AFTER-MAIL-4451" authored))
        (should (niwa-agent-control-context-mailbox session))
        (niwa-claude-code-context-observe request (niwa-claude-code-context-test--user-receipt content)))
      (should-not (niwa-session-pending-steering session))
      (should-not (niwa-agent-control-context-mailbox session))))

  :doc "ordinary budget events use hooks unless authored steering needs a prompt"
  (dolist (steering '(nil t))
    (niwa-engine-test--with-session
      (setf (niwa-engine-info request) (list :buffer buffer :position (point-max-marker)))
      (niwa-claude-code-context-system request "sys")
      (let ((commits 0))
        (niwa-reminders-queue-turn-event buffer 'goal-budget "BUDGET-WRAP-UP-7321"
                                          (lambda () (cl-incf commits)))
        (when steering
          (niwa-claude-code-context-test--steer session request "USER-BUDGET-7332"))
        (let ((decision (niwa-claude-code--control request '(:hook_event_name "PostToolBatch"))))
          (should (eq steering (eq :json-false (plist-get decision :continue))))
          (should (= 0 commits))
          (if steering
              (let ((content (niwa-claude-code-context-next-prompt request)))
                (should (string-search "BUDGET-WRAP-UP-7321" (alist-get 'text (aref content 0))))
                (should (equal "USER-BUDGET-7332" (alist-get 'text (aref content 1))))
                (niwa-claude-code-context-observe request (niwa-claude-code-context-test--user-receipt content)))
            (let ((body (plist-get (plist-get decision :hookSpecificOutput) :additionalContext)))
              (should (string-search "BUDGET-WRAP-UP-7321" body))
              (niwa-claude-code-context-observe request (niwa-claude-code-context-test--hook-receipt body)))))
        (should (= 1 commits)))))

  :doc "cockpit pause or an unresolved interaction holds steering at both boundaries"
  (dolist (hold '(pause interaction))
    (niwa-engine-test--with-session
      (setf (niwa-engine-info request) (list :buffer buffer))
      (niwa-claude-code-context-system request "sys")
      ;; Establish the normal first-prompt receipt before testing a tool boundary.
      (let ((content (niwa-claude-code-context-prompt
                      request [((type . "text") (text . "hi"))])))
        (niwa-claude-code-context-observe
         request (niwa-claude-code-context-test--user-receipt content)))
      (niwa-claude-code-context-test--steer session request "HELD-STEER")
      (let ((entry (car (niwa-session-pending-steering session))) view)
        (unwind-protect
            (progn
              (if (eq hold 'pause)
                  (niwa-session-set-pending-input-paused session t)
                (setq view (generate-new-buffer " *acp-held-interaction*"))
                (setq-local niwa--view-buffer view)
                (with-current-buffer view
                  (setq-local niwa-view--interaction-descriptors (make-hash-table :test #'equal))
                  (puthash 'ask '(:kind ask) niwa-view--interaction-descriptors)))
              (should-not (niwa-claude-code-context-prepare request))
              (should-not (niwa-claude-code-context-next-prompt request))
              (should (equal (list entry) (niwa-session-pending-steering session)))
              (should-not (plist-get entry :delivering)))
          (when (buffer-live-p view) (kill-buffer view)))))))

(niwa-deftest niwa-claude-code-context-next-prompt (:quiet t)
  ,test
  (test)
  :doc "steering left at a prompt's end continues the turn without the overflow preamble"
  (niwa-engine-test--with-session
    (setf (niwa-engine-info request) (list :buffer buffer :position (point-max-marker)))
    (niwa-claude-code-context-system request "sys")
    (niwa-claude-code-context-test--steer session request "LATE-STEER-2290")
    (let* ((content (niwa-claude-code-context-next-prompt request))
           (text (mapconcat (lambda (part) (alist-get 'text part)) content "\n")))
      (should (string-search "LATE-STEER-2290" text))
      (should-not (string-search "Continue the current task" text))
      (should (= 1 (length (niwa-session-pending-steering session))))
      (niwa-claude-code-context-observe
       request `((method . "_claude/sdkMessage")
                 (params (message (type . "user")
                                  (message (role . "user") (content . ,content)))))))
    (should-not (niwa-session-pending-steering session))
    (should (= 1 (how-many "LATE-STEER-2290" (point-min) (point-max)))))

  :doc "arrivals during expansion wait for a later snapshot and an explicit stop discards unsent content"
  (dolist (stop '(nil t))
    (niwa-engine-test--with-session
      (setf (niwa-engine-info request) (list :buffer buffer :position (point-max-marker)))
      (niwa-claude-code-context-system request "sys")
      (niwa-claude-code-context-test--steer session request "FIRST-SNAPSHOT-2186")
      (let ((expand (symbol-function 'niwa-tools-steering-expand)) queued)
        (cl-letf (((symbol-function 'niwa-tools-steering-expand)
                   (lambda (&rest args)
                     (prog1 (apply expand args)
                       (unless queued
                         (setq queued t)
                         (niwa-claude-code-context-test--steer session request "LATER-SNAPSHOT-2274")
                         (when stop
                           (setf (niwa-engine-info request)
                                 (plist-put (niwa-engine-info request) :niwa-end-turn 'user))))))))
          (let ((content (niwa-claude-code-context-next-prompt request)))
            (if stop
                (progn
                  (should-not content)
                  (should-not (plist-get (niwa-engine-info request) :niwa-claude-context-pending))
                  (should (= 2 (length (niwa-session-pending-steering session)))))
              (should (equal "FIRST-SNAPSHOT-2186" (alist-get 'text (aref content (1- (length content))))))
              (niwa-claude-code-context-observe request (niwa-claude-code-context-test--user-receipt content))
              (should (equal '("LATER-SNAPSHOT-2274")
                             (mapcar (lambda (entry) (plist-get entry :input))
                                     (niwa-session-pending-steering session))))))))))

  :doc "nothing continues a turn that already ends or has no steering"
  (niwa-engine-test--with-session
    (setf (niwa-engine-info request) (list :buffer buffer))
    (niwa-claude-code-context-system request "sys")
    (should-not (niwa-claude-code-context-next-prompt request))
    (niwa-claude-code-context-test--steer session request "ENDING-STEER")
    (setf (niwa-engine-info request)
          (plist-put (niwa-engine-info request) :niwa-end-turn 'user))
    (should-not (niwa-claude-code-context-next-prompt request))
    (should (= 1 (length (niwa-session-pending-steering session))))))

(niwa-deftest niwa-claude-code-context--prompt-parts ()
  ,test
  (test)
  :doc "keeps authored text and images after typed context without a preamble"
  (let ((image '((type . "image") (mimeType . "image/png") (data . "QUJD"))))
    (should (equal (list '((type . "text") (text . "CONTEXT"))
                        '((type . "text") (text . "Stop and do something else")) image)
                   (niwa-claude-code-context--prompt-parts
                    (list :body "CONTEXT" :steering-text "Stop and do something else" :images (list image)))))
    (should (equal '(((type . "text") (text . "CONTEXT")))
                   (niwa-claude-code-context--prompt-parts '(:body "CONTEXT"))))
    (should-not (niwa-claude-code-context--prompt-parts nil))))

(niwa-deftest niwa-claude-code-context--echoed-images ()
  ,test
  (test)
  :doc "keeps base64 image parts whatever their bytes or image MIME type"
  (let ((png '((type . "image") (mimeType . "image/png") (data . "QUJD")))
        (jpeg '((type . "image") (mimeType . "image/jpeg") (data . "QUI="))))
    (should (= 2 (length (niwa-claude-code-context--echoed-images
                          (niwa-claude-code-context-test--echo
                           (list '((type . "text") (text . "t")) png jpeg))))))
    (should-not (niwa-claude-code-context--echoed-images
                 (niwa-claude-code-context-test--echo
                  (list '((type . "image") (mimeType . "image/png") (data . ""))
                        '((type . "image") (mimeType . "text/plain") (data . "QUJD"))))))
    (should-not (niwa-claude-code-context--echoed-images
                 '(((type . "image") (source (type . "url") (url . "x"))))))))

(niwa-deftest niwa-claude-code-context--input-received-p ()
  ,test
  (test)
  :doc "accepts an image Claude Code resized and re-encoded before echoing"
  (let* ((text '((type . "text") (text . "look")))
         (sent '((type . "image") (mimeType . "image/png") (data . "T1JJRw==")))
         (resized '((type . "image") (mimeType . "image/jpeg") (data . "U01BTEw=")))
         (content (vector text sent)))
    (cl-flet ((received (parts)
                (niwa-claude-code-context--input-received-p
                 content `((type . "user")
                           (message (role . "user")
                                    (content . ,(niwa-claude-code-context-test--echo
                                                 parts)))))))
      (should (received (list text sent)))
      (should (received (list text resized)))
      ;; Text alone does not establish image delivery.
      (should-not (received (list text)))
      (should-not (received (list text sent resized)))
      (should-not (received (list '((type . "text") (text . "loo")) sent))))))

(niwa-deftest niwa-claude-code-context-prompt (:quiet t)
  ,test
  (test)
  :doc "a retained child's wake mail follows its prompt and waits for exact ACP receipt"
  (niwa-engine-test--with-claude-session
    (let* ((history '(:id "retained-child" :state ready))
           (configuration (niwa-agent-configuration--create
                           :agent (niwa-agent--create
                                   :name "default" :description "Mail test" :frozen-p t)
                           :request-locals (niwa-agent-exec-request-snapshot
                                            (list :backend gptel-backend :model gptel-model))))
           (owner (niwa-agent-invocation--create
                   :path "/root/work" :parent-session session :buffer buffer
                   :agent (niwa-agent-configuration-agent configuration)
                   :agent-id "wake-agent" :frozen-configuration configuration
                   :runtime-context (list :buffer buffer :niwa-claude-history history)))
           (record (niwa-agent-record--create
                    :id "wake-agent" :role "default" :configuration configuration
                    :conversation-location "agents/wake.chat.org"
                    :path "/root/work" :parent-path "/root"
                    :activity 'running :invocation owner))
           (wake niwa-agent-control--wake-prompt))
      (setf (niwa-session-agent-registry session) (list (cons "/root/work" record)))
      (niwa-agent-control-send-message session "/root/work" "ACP-FIRST-MAIL")
      (niwa-agent-control-send-message session "/root/work" "ACP-SECOND-MAIL")
      (let* ((content (niwa-claude-code-context-prompt
                       owner (vector `((type . "text") (text . ,wake))) history))
             (text (mapconcat (lambda (part) (alist-get 'text part)) content "\n")))
        (should (< (string-search wake text) (string-search "ACP-FIRST-MAIL" text)))
        (should (< (string-search "ACP-FIRST-MAIL" text)
                   (string-search "ACP-SECOND-MAIL" text)))
        (should (= 2 (length (niwa-agent-record-mailbox record))))
        (should (eq history (plist-get (niwa-engine-info owner) :niwa-claude-history)))
        (let ((receipt `((method . "_claude/sdkMessage")
                         (params (message (type . "user")
                                          (message (role . "user") (content . ,content)))))))
          (niwa-claude-code-context-observe owner receipt)
          (niwa-claude-code-context-observe owner receipt))
        (should-not (niwa-agent-record-mailbox record))
        (should (= 2 (length (niwa-transcript-audit-buffer-spans 'mailbox-receipt)))))))

  :doc "acknowledged unchanged path instructions are not re-sent"
  (niwa-engine-test--with-session
    (let* ((dir (file-name-concat root "sub"))
           (file (file-name-concat dir "AGENTS.md")))
      (make-directory dir t)
      (write-region "SUBDIR-INSTRUCTIONS-8812" nil file nil 'silent)
      (setf (niwa-session-workspace-instruction-hashes session)
            (list (cons (list "/root" file) (secure-hash 'sha256 "SUBDIR-INSTRUCTIONS-8812"))))
      (dolist (case '((nil . nil) ("SUBDIR-CHANGED-4410" . t)))
        (when (car case) (write-region (car case) nil file nil 'silent))
        (setf (niwa-engine-info request) (list :buffer buffer))
        (niwa-claude-code-context-system request "sys")
        (let ((text (niwa-claude-code-context-test--prompt-text request '(:id "native" :state ready))))
          (should-not (string-search "SUBDIR-INSTRUCTIONS-8812" text))
          (should (eq (cdr case) (and (string-search "SUBDIR-CHANGED-4410" text) t)))))))

  :doc "selected user context is sent once per native conversation until it changes"
  (niwa-engine-test--with-session
    (let ((history '(:id "native-1" :state ready))
          (count 0))
      (dolist (step '(("SELECTED-3391" t) ("SELECTED-3391" nil) ("SELECTED-4402" t)
                      (new "SELECTED-4402" t) (restore "SELECTED-4402" t)))
        (let* ((new (eq 'new (car step)))
               (restore (eq 'restore (car step)))
               (step (if (symbolp (car step)) (cdr step) step))
               (owner-history (if new '(:state ready) history)))
          (setf (niwa-engine-info request)
                (list :buffer buffer :niwa-claude-history history))
          (niwa-claude-code-context-system request "sys" (list :mode 'user :text (car step)))
          (when restore
            (setf (niwa-engine-info request)
                  (plist-put (niwa-engine-info request) :niwa-claude-restoration-pending nil))
            (niwa-claude-code-context-restore request)
            (setf (niwa-engine-info request)
                  (plist-put (niwa-engine-info request) :niwa-claude-restoration-pending nil)))
          (let* ((content (niwa-claude-code-context-prompt
                           request (vector '((type . "text") (text . "hi"))) owner-history))
                 (text (mapconcat (lambda (part) (or (alist-get 'text part) "")) content "\n")))
            (cl-incf count)
            (ert-info ((format "step %d" count))
              (should (eq (cadr step) (and (string-search (car step) text) t))))
            ;; The exact user echo commits the dedup mark.
            (when (plist-get (niwa-engine-info request) :niwa-claude-input-pending)
              (niwa-claude-code-context-observe
               request `((method . "_claude/sdkMessage")
                         (params (message (type . "user")
                                          (message (role . "user") (content . ,content)))))))))))))

(provide 'test-niwa-claude-code-context)
;;; test-niwa-claude-code-context.el ends here
