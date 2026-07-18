;;; test-mevedel-tool-ui.el --- Interaction tool assembly tests -*- lexical-binding: t -*-

;;; Commentary:

;; Tests the small user-interaction tool assembly boundary.  Focused Ask and
;; Ask behavior lives in its mirrored test module.

;;; Code:

(require 'gptel)
(require 'json)
(require 'mevedel-agent-exec)
(require 'mevedel-session-persistence)
(require 'mevedel-tool-registry)
(require 'mevedel-tool-ui)
(require 'mevedel-tools)
(require 'mevedel-view)
(require 'mevedel-structs)
(require 'mevedel-workspace)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

;; `mevedel-agent-control'
(declare-function mevedel-agent-record-activity
                  "mevedel-agent-control" (cl-x) t)
(declare-function mevedel-agent-record-configuration
                  "mevedel-agent-control" (cl-x) t)
(declare-function mevedel-agent-record-conversation-location
                  "mevedel-agent-control" (cl-x) t)
(declare-function mevedel-agent-record-id
                  "mevedel-agent-control" (cl-x) t)
(declare-function mevedel-agent-record-parent-path
                  "mevedel-agent-control" (cl-x) t)
(declare-function mevedel-agent-record-path
                  "mevedel-agent-control" (cl-x) t)
(declare-function mevedel-agent-record-role
                  "mevedel-agent-control" (cl-x) t)

;; `mevedel-agents'
(declare-function mevedel-agent-invocation-buffer
                  "mevedel-agents" (cl-x) t)

;; `mevedel-session-persistence'
(declare-function mevedel-session-persistence-lock-release
                  "mevedel-session-persistence" (path))

(mevedel-deftest mevedel-tool-ui--deliver-result ()
  ,test
  (test)
  :doc "wraps raw results in the canonical envelope"
  (let (delivered)
    (mevedel-tool-ui--deliver-result
     (lambda (value) (setq delivered value))
     "done")
    (should (equal '(:result "done") delivered)))

  :doc "preserves result metadata envelopes"
  (let ((envelope '(:result "done" :render-data (:kind card)
                    :media ((:type image))))
        delivered)
    (mevedel-tool-ui--deliver-result
     (lambda (value) (setq delivered value))
     envelope)
    (should (eq envelope delivered))))

(mevedel-deftest mevedel-tool-ui--register
  (:before-each (mevedel-tool-clear-registry)
   :after-each (mevedel-tool-clear-registry))
  ,test
  (test)
  :doc "assembles the user-interaction tool surface"
  (progn
    (mevedel-tool-ui--register)
    (dolist (name '("Ask" "Agent" "StopAgent"
                    "ToolSearch" "SendMessage"))
      (should (mevedel-tool-get name)))
    (should-not (mevedel-tool-get "RequestAccess"))))

(mevedel-deftest mevedel-tool-ui--agent
  (:after-each (mevedel-workspace-clear-registry))
  ,test
  (test)

  :doc "spawns, settles, and retains one path-addressed default agent"
  (let* ((root (file-name-as-directory
                (make-temp-file "mevedel-agent-v2-" t)))
         (workspace (mevedel-workspace-get-or-create
                     'project "agent-v2" root "agent-v2"))
         (session (mevedel-session-create "main" workspace))
         (parent (generate-new-buffer " *mevedel-agent-v2-parent*"))
         callbacks
         invocations
         launches)
    (unwind-protect
        (with-current-buffer parent
          (setq-local mevedel--session session)
          (setq-local mevedel--workspace workspace)
          (cl-letf (((symbol-function 'mevedel-agent-exec--run)
                     (lambda (callback _role _description message invocation
                                       _buffer &optional _configure)
                       (if (equal message "Fail initialization.")
                           (error "Provider initialization failed")
                         (push callback callbacks)
                         (push invocation invocations)
                         'provider-request))))
            (should-error
             (mevedel-tool-ui--agent
              #'ignore
              '(:task_name "retryable"
                :message "Fail initialization.")))
            (should-not (assoc "/root/retryable"
                               (mevedel-session-agent-registry session)))
            (should-not (mevedel-session-agent-transcripts session))
            (dotimes (index 3)
              (let (result)
                (mevedel-tool-ui--agent
                 (lambda (value &rest _) (setq result value))
                 (list :task_name (format "task_%d" index)
                       :message "Inspect the implementation."))
                (push result launches)))
            (should-error
             (mevedel-tool-ui--agent
              #'ignore
              '(:task_name "too_many" :message "This must not start."))
             :type 'user-error)
            (let* ((launch (car (last launches)))
                   (path (gethash "path"
                                  (json-parse-string
                                   (plist-get launch :result)))))
              (should (equal "/root/task_0" path))
              (should (equal "Started /root/task_0"
                             (substring-no-properties
                              (plist-get
                               (mevedel-tool-ui--render-agent
                                "Agent"
                                '(:task_name "task_0")
                                (plist-get launch :result)
                                (plist-get launch :render-data))
                               :header)))))
            (let* ((records (mevedel-session-agent-registry session))
                   (record (cdr (assoc "/root/task_0" records))))
              (should (= 3 (length records)))
              (should (stringp (mevedel-agent-record-id record)))
              (should (equal "/root/task_0"
                             (mevedel-agent-record-path record)))
              (should (equal "/root"
                             (mevedel-agent-record-parent-path record)))
              (should (equal "default"
                             (mevedel-agent-record-role record)))
              (should (equal '(:role "default")
                             (mevedel-agent-record-configuration record)))
              (should (eq 'running
                          (mevedel-agent-record-activity record)))
              (should (stringp
                       (mevedel-agent-record-conversation-location record))))
            (funcall (car (last callbacks)) "Completed task zero.")
            (funcall (car (last callbacks)) "Duplicate callback.")
            (let* ((records (mevedel-session-agent-registry session))
                   (record (cdr (assoc "/root/task_0" records)))
                   (messages (mevedel-session-messages session))
                   (result (car messages)))
              (should (eq 'idle (mevedel-agent-record-activity record)))
              (should (= 1 (length messages)))
              (should (eq 'RESULT (plist-get result :type)))
              (should (equal "/root/task_0" (plist-get result :sender)))
              (should (equal "/root" (plist-get result :recipient)))
              (should (eq 'completed (plist-get result :outcome)))
              (should (string-match-p "Completed task zero"
                                      (plist-get result :payload)))
              (let ((delivery (mevedel-tools--message-delivery-block result)))
                (should (string-match-p
                         "sender=\"/root/task_0\"" delivery))
                (should (string-match-p "Completed task zero" delivery))))
            (funcall
             (nth 1 callbacks)
             '(:mevedel-agent-terminal-status error
               :error-details "Provider failed"
               :fallback-partial nil))
            (funcall
             (nth 1 callbacks)
             '(:mevedel-agent-terminal-status error
               :error-details "Duplicate error"
               :fallback-partial nil))
            (let ((result (car (mevedel-session-messages session))))
              (should (= 2 (length (mevedel-session-messages session))))
              (should (eq 'errored (plist-get result :outcome)))
              (should (string-match-p "Provider failed"
                                      (plist-get result :payload)))
              (should-not (string-match-p "default--"
                                          (plist-get result :payload))))
            (funcall (car callbacks)
                     (concat "HEAD" (make-string 40000 ?x) "TAIL"))
            (let* ((result (car (mevedel-session-messages session)))
                   (payload (plist-get result :payload)))
              (should (= 3 (length (mevedel-session-messages session))))
              (should (<= (length payload) (* 32 1024)))
              (should (string-prefix-p "HEAD" payload))
              (should (string-match-p "TAIL" payload))
              (should (string-match-p "Full transcript:" payload))
              (let ((delivery
                     (mevedel-tools--message-delivery-block result)))
                (should (string-match-p "TAIL" delivery))
                (should (string-match-p "Full transcript:" delivery))))
            (let (result)
              (mevedel-tool-ui--agent
               (lambda (value &rest _) (setq result value))
               '(:task_name "after_completion"
                 :message "Use the released slot."))
              (should result))
            (should-error
             (mevedel-tool-ui--agent
              #'ignore
              '(:task_name "task_0" :message "Paths remain reserved."))
             :type 'user-error)
            (dolist (args '((:task_name "Upper" :message "bad")
                            (:task_name "two/parts" :message "bad")
                            (:task_name "legacy" :message "ok"
                             :subagent_type "explorer")
                            (:task_name "legacy_2" :message "ok"
                             :description "old")
                            (:task_name "legacy_3" :message "ok"
                             :prompt "old")
                            (:task_name "legacy_4" :message "ok"
                             :run_in_background t)))
              (should-error (mevedel-tool-ui--agent #'ignore args)))))
      (dolist (invocation invocations)
        (when-let* ((buffer (mevedel-agent-invocation-buffer invocation))
                    ((buffer-live-p buffer)))
          (with-current-buffer buffer
            (set-buffer-modified-p nil))
          (kill-buffer buffer)))
      (when (mevedel-session-save-path session)
        (mevedel-session-persistence-lock-release
         (mevedel-session-save-path session)))
      (when (buffer-live-p parent)
        (kill-buffer parent))
      (delete-directory root t))))


;;; Expand/collapse

(mevedel-deftest mevedel-tool-ui--render-agent-body
  (:doc "selects the correct Agent expanded body for foreground and background rows")
  ,test
  (test)

  :doc "Started rows preserve a multiline leading-> composer draft"
  (mevedel-view-test--with-buffers
    (let ((draft "> quoted\nsecond line"))
      (with-current-buffer view-buf
        (mevedel-view-test--insert-composer-draft draft 4)
        (let ((rendering
               (mevedel-tool-ui--render-agent
                "Agent" '(:task_name "spec_review")
                "{\"path\":\"/root/spec_review\"}"
                '(:kind collaboration-event
                  :event started
                  :path "/root/spec_review"
                  :agent-id "default--internal"
                  :status running))))
          (let ((inhibit-read-only t))
            (goto-char mevedel-view--input-marker)
            (set-marker-insertion-type mevedel-view--input-marker t)
            (unwind-protect
                (mevedel-view--insert-rendered-tool rendering (cons 1 1))
              (set-marker-insertion-type mevedel-view--input-marker nil)))
          (should (string= draft (mevedel-view--input-text)))))))

  :doc "agent description truncation never exceeds narrow widths"
  (progn
    (should (equal "" (mevedel-tool-ui--compact-agent-description
                       "long task" 0)))
    (should (equal "." (mevedel-tool-ui--compact-agent-description
                       "long task" 1)))
    (should (equal ".." (mevedel-tool-ui--compact-agent-description
                        "long task" 2)))
    (should (equal "..." (mevedel-tool-ui--compact-agent-description
                         "long task" 3)))
    (should (<= (string-width
                 (mevedel-tool-ui--compact-agent-description
                  "long task" 2))
                2)))

  :doc "foreground Agent rows still render the final response"
  (mevedel-view-test--with-buffers
    (let* ((args '(:subagent_type "explorer" :description "Task"))
           (rd '(:kind agent-transcript
                 :agent-id "explorer--fg"
                 :status completed
                 :activity ((:type tool-start :tool-name "Read"))))
           (rendering (mevedel-tool-ui--render-agent
                       "Agent" args "final response body" rd)))
      (with-current-buffer view-buf
        (let ((inhibit-read-only t))
          (goto-char mevedel-view--input-marker)
          (set-marker-insertion-type mevedel-view--input-marker t)
          (unwind-protect
              (mevedel-view--render-expanded-body rendering (cons 1 1))
            (set-marker-insertion-type mevedel-view--input-marker nil)))
        (let ((text (buffer-substring-no-properties
                     (point-min) mevedel-view--input-marker)))
          (should (string-match-p "final response body" text))
          (should-not (string-match-p "Read" text)))))))

  :doc "Agent rows group SubagentStart context handlers in execution order"
  (mevedel-view-test--with-buffers
    (let* ((args '(:subagent_type "explorer" :description "Task"))
           (rd '(:kind agent-transcript
                 :agent-id "explorer--hook"
                 :status running
                 :hook-audits
                 ((:type subagent-context
                   :event "SubagentStart"
                   :handlers
                   ((:function ponytail-subagent
                     :source plugin
                     :plugin-name "ponytail"
                     :reason "PONYTAIL:FULL")
                    (:description "Inject project conventions"
                     :source project-file))))))
           (rendering (mevedel-tool-ui--render-agent
                       "Agent" args "launch body" rd)))
      (with-current-buffer view-buf
        (let ((inhibit-read-only t))
          (goto-char mevedel-view--input-marker)
          (set-marker-insertion-type mevedel-view--input-marker t)
          (unwind-protect
              (mevedel-view--insert-rendered-tool rendering (cons 1 1))
            (set-marker-insertion-type mevedel-view--input-marker nil)))
        (let ((text (buffer-substring-no-properties
                     (point-min) mevedel-view--input-marker)))
          (should (string-match-p
                   "SubagentStart hook added context · 2 handlers" text))
          (should-not (string-match-p "ponytail plugin" text)))
        (goto-char (point-min))
        (search-forward "SubagentStart hook added context")
        (mevedel-view-toggle-section)
        (let* ((text (buffer-substring-no-properties
                      (point-min) mevedel-view--input-marker))
               (first (string-match "1\\. ponytail plugin" text))
               (second (string-match "2\\. project hook" text)))
          (should first)
          (should second)
          (should (< first second))
          (should (string-match-p "Handler: ponytail-subagent" text))
          (should (string-match-p "Reason: PONYTAIL:FULL" text))
          (should (string-match-p
                   "Handler: Inject project conventions" text))
          (should-not (string-match-p "extra start context" text))))))

  :doc "Agent handle transcript click target is the visible type label"
  (mevedel-view-test--with-buffers
    (let* ((agent-id "explorer--abcdef1234567890")
           (workspace (mevedel-workspace--create
                       :type 'project
                       :id "agent-target"
                       :root temporary-file-directory
                       :name "agent-target"))
           (session (mevedel-session-create "main" workspace))
           (save-path (file-name-as-directory
                       (file-name-concat temporary-file-directory
                                         "mevedel-agent-target-session")))
           (args '(:subagent_type "explorer" :description "Task"))
           (rd `(:kind agent-transcript
                 :agent-id ,agent-id
                 :status completed))
           rendering)
      (setf (mevedel-session-save-path session) save-path)
      (setf (mevedel-session-agent-transcripts session)
            (list (cons agent-id
                        '(:path "agents/explorer--abcdef12.chat.org"
                          :status completed))))
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (with-current-buffer view-buf
        (setq rendering (mevedel-tool-ui--render-agent
                         "Agent" args "final response body" rd))
        (let ((inhibit-read-only t)
              start)
          (goto-char mevedel-view--input-marker)
          (setq start (point))
          (mevedel-view--insert-rendered-tool rendering (cons 1 1))
          (mevedel-view--add-display-region-properties
           start (point) 'agent-handle)
          (should
           (string-search
            "Agent: explorer -- Task"
            (buffer-substring-no-properties start (point))))
          (goto-char start)
          (search-forward "Agent: explorer")
          (search-backward "explorer")
          (should (eq (get-text-property (point) 'keymap)
                      mevedel-view--agent-label-map))
          (should (equal agent-id
                         (get-text-property
                          (point) 'mevedel-view-agent-id)))
          (should
           (lookup-key (get-text-property (point) 'keymap) [mouse-1])))))

  :doc "Agent handle header normalizes long task text to one line"
  (let* ((args '(:subagent_type "coordinator"
                 :description "Run validation.\nThen repeat until green."))
         (rd '(:kind agent-transcript
               :agent-id "coordinator--abcdef123456"
               :status running
               :calls 9))
         (mevedel-tool-ui-agent-description-width 30)
         (rendering (mevedel-tool-ui--render-agent
                     "Agent" args "launch status" rd))
         (header (plist-get rendering :header)))
    (should (string-match-p
             "Agent: coordinator -- Run validation\\. Then repeat\\.\\.\\."
             header))
    (should-not (string-match-p "\n" header))
    (should (string-match-p "\\[running · 9 calls\\]" header))))

(provide 'test-mevedel-tool-ui)

;;; test-mevedel-tool-ui.el ends here
