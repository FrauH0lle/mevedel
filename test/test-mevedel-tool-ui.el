;;; test-mevedel-tool-ui.el --- Interaction tool assembly tests -*- lexical-binding: t -*-

;;; Commentary:

;; Tests the small user-interaction tool assembly boundary.  Focused Ask and
;; Ask behavior lives in its mirrored test module.

;;; Code:

(require 'gptel)
(require 'json)
(require 'mevedel-agent-control)
(require 'mevedel-agent-conversation)
(require 'mevedel-agent-exec)
(require 'mevedel-compact)
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

(defun mevedel-tool-ui-test--session ()
  "Return a fresh in-memory session for tool UI tests."
  (mevedel-session-create
   "main"
   (mevedel-workspace--create
    :type 'project
    :id "tool-ui"
    :root temporary-file-directory
    :name "tool-ui")))

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
    (dolist (name '("Ask" "Agent" "FollowupAgent" "ListAgents"
                    "InterruptAgent" "ToolSearch" "SendMessage" "WaitAgent"))
      (should (mevedel-tool-get name)))
    (should (eq #'mevedel-tool-ui--render-list-agents
                (mevedel-tool-renderer
                 (mevedel-tool-get "ListAgents"))))
    (should (mevedel-tool-async-p (mevedel-tool-get "WaitAgent")))
    (should-not (mevedel-tool-get "RequestAccess")))

  :doc "Agent exposes every optional fork and configuration control"
  (progn
    (mevedel-tool-ui--register)
    (let* ((tool (mevedel-tool-get "Agent"))
           (args (mevedel-tool-args tool)))
      (dolist (name '(role fork_turns model effort))
        (let ((arg (assq name args)))
          (should arg)
          (should (eq :optional (nth 2 arg))))))))

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
          (cl-letf (((symbol-function 'mevedel-agent-exec-run)
                     (lambda (callback _role _description invocation
                                       buffer &optional _configure)
                       (if (with-current-buffer buffer
                             (string-match-p "Fail initialization\\."
                                             (buffer-string)))
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
              (should (equal "default"
                             (mevedel-agent-name
                              (mevedel-agent-configuration-agent
                               (mevedel-agent-record-configuration record)))))
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
            (dolist
                (args '((:task_name "bad_fork" :message "bad"
                         :fork_turns "0")
                        (:task_name "bad_model" :message "bad"
                         :model "not-a-model")
                        (:task_name "bad_effort" :message "bad"
                         :effort "")
                        (:task_name "nil_effort" :message "bad"
                         :effort "nil")))
              (should-error (mevedel-tool-ui--agent #'ignore args)
                            :type 'user-error)
              (should-not
               (assoc (concat "/root/" (plist-get args :task_name))
                      (mevedel-session-agent-registry session))))
            (dolist (args '((:task_name "Upper" :message "bad")
                            (:task_name "two/parts" :message "bad")))
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

(mevedel-deftest mevedel-tool-ui--agent/frozen-conversation
  (:before-each (mevedel-tools-register)
   :after-each (mevedel-workspace-clear-registry))
  ,test
  (test)
  :doc "covers every context fork, independent compaction, and frozen follow-ups"
  (let* ((root (file-name-as-directory
                (make-temp-file "mevedel-agent-frozen-" t)))
         (workspace (mevedel-workspace-get-or-create
                     'project "agent-frozen" root "agent-frozen"))
         (session (mevedel-session-create "main" workspace))
         (parent (generate-new-buffer " *mevedel-agent-frozen-parent*"))
         (role
          (mevedel-agent--create
           :name "freeze_test"
           :description "Freeze conversation"
           :tools nil
           :system-prompt "Initial frozen instructions."))
         callbacks
         invocations
         record
         configuration
         child-buffer
         frozen-buffer
         initial-invocation)
    (push (cons "freeze_test" role) mevedel-agent--registry)
    (unwind-protect
        (with-current-buffer parent
          (org-mode)
          (setq-local mevedel--session session)
          (setq-local mevedel--workspace workspace)
          (insert "#+begin_summary\nArchived work summarized.\n#+end_summary\n"
                  "First live prompt.\n")
          (let ((response-start (point)))
            (insert "First live response.\n")
            (put-text-property response-start (point) 'gptel 'response))
          (insert "Second live prompt.\n")
          (let ((response-start (point)))
            (insert "Second live response.\n")
            (put-text-property response-start (point) 'gptel 'response))
          (let ((mevedel-agents--specs nil))
            (cl-letf (((symbol-function 'mevedel-agent-exec-run)
                       (lambda (callback _role _description invocation
                                         buffer &optional _configure)
                         (push callback callbacks)
                         (push invocation invocations)
                         (setq child-buffer buffer)
                         'provider-request)))
              (mevedel-tool-ui--agent
               #'ignore
               '(:task_name "frozen"
                 :message "Initial child task."
                 :role "freeze_test"
                 :fork_turns "1"))
              (setq record
                    (cdr (assoc "/root/frozen"
                                (mevedel-session-agent-registry session))))
              (setq configuration
                    (mevedel-agent-record-configuration record))
              (setq frozen-buffer child-buffer
                    initial-invocation (car invocations))
              (with-current-buffer child-buffer
                (let ((text (buffer-string)))
                  (should (string-match-p "Archived work summarized" text))
                  (should-not (string-match-p "First live prompt" text))
                  (should (string-match-p "Second live prompt" text))
                  (should (string-match-p "Initial child task" text))))
              (with-current-buffer frozen-buffer
                (goto-char (point-max))
                (let ((start (point)))
                  (insert "Initial child result.\n")
                  (put-text-property start (point) 'gptel 'response)))
              (funcall (car callbacks) "Initial child result.")
              (with-current-buffer frozen-buffer
                (let* ((target
                        (mevedel--compact-agent-target initial-invocation))
                       (archive
                        (mevedel--compact-agent-apply
                         target "Child work compacted." nil nil nil)))
                  (should (file-exists-p archive))
                  (should (string-match-p "Child work compacted"
                                          (buffer-string)))
                  (should (string-match-p "Initial child task"
                                          (buffer-string)))
                  (should-not (string-match-p "Archived work summarized"
                                              (buffer-string)))
                  (should-not (string-match-p "Second live prompt"
                                              (buffer-string)))))
              (should (eq record
                          (cdr (assoc
                                "/root/frozen"
                                (mevedel-session-agent-registry session)))))
              (cl-labels
                  ((fork-text (task-name fork)
                     (mevedel-tool-ui--agent
                      #'ignore
                      (append
                       (list :task_name task-name
                             :message (format "Task for %s." task-name))
                       fork))
                     (prog1
                         (with-current-buffer child-buffer (buffer-string))
                       (funcall (car callbacks) "Context fork complete."))))
                (let ((text (fork-text "explicit_all"
                                       '(:fork_turns "all"))))
                  (should (string-match-p "Archived work summarized" text))
                  (should (string-match-p "First live prompt" text))
                  (should (string-match-p "Second live prompt" text)))
                (let ((text (fork-text "default_all" nil)))
                  (should (string-match-p "Archived work summarized" text))
                  (should (string-match-p "First live prompt" text))
                  (should (string-match-p "Second live prompt" text)))
                (let ((text (fork-text "without_context"
                                       '(:fork_turns "none"))))
                  (should-not
                   (string-match-p "Archived work summarized" text))
                  (should-not (string-match-p "First live prompt" text))
                  (should-not (string-match-p "Second live prompt" text))
                  (should (string-match-p "Task for without_context" text))))
              (insert "Parent text added after spawn.\n")
              (setf (alist-get "freeze_test" mevedel-agent--registry
                               nil nil #'equal)
                    (mevedel-agent--create
                     :name "freeze_test"
                     :description "Mutated role"
                     :tools '((:tool "Eval"))
                     :system-prompt "Mutated instructions."))
              (mevedel-tool-ui--followup-agent
               '(:target "/root/frozen" :message "Follow-up child task."))
              (let* ((followup (car invocations))
                     (frozen-agent
                      (mevedel-agent-invocation-agent followup)))
                (should (eq configuration
                            (mevedel-agent-invocation-frozen-configuration
                             followup)))
                (should (equal "Initial frozen instructions."
                               (mevedel-agent-system-prompt frozen-agent)))
                (should-not (member '(:tool "Eval")
                                    (mevedel-agent-tools frozen-agent)))
                (with-current-buffer frozen-buffer
                  (let ((text (buffer-string)))
                    (should (string-match-p "Follow-up child task" text))
                    (should-not
                     (string-match-p "Parent text added after spawn" text))
                    (should
                     (= 1 (how-many
                           (format "^:%s:"
                                   mevedel-agent-task-path-property)
                           (point-min) (point-max)))))
                  (goto-char (point-max))
                  (let ((start (point)))
                    (insert "Follow-up child result.\n")
                    (put-text-property start (point) 'gptel 'response)))
                (funcall (car callbacks) "Follow-up child result.")
                (with-current-buffer frozen-buffer
                  (let ((snapshot (mevedel-compact-context-snapshot 1)))
                    (should (string-match-p "\\* Agent Task: frozen" snapshot))
                    (should (string-match-p "Child work compacted" snapshot))
                    (should (string-match-p "Follow-up child task" snapshot))
                    (should (string-match-p "Follow-up child result" snapshot)))
                  (let* ((target (mevedel--compact-agent-target followup))
                         (archive
                          (mevedel--compact-agent-apply
                           target "Follow-up work compacted." nil nil nil)))
                    (should (file-exists-p archive))
                    (should (string-match-p "Initial child task"
                                            (buffer-string)))
                    (should (string-match-p "Follow-up work compacted"
                                            (buffer-string)))
                    (should-not (string-match-p "Follow-up child task"
                                                (buffer-string)))))))))
      (setq mevedel-agent--registry
            (assoc-delete-all "freeze_test" mevedel-agent--registry))
      (dolist (invocation invocations)
        (when-let* ((buffer (mevedel-agent-invocation-buffer invocation))
                    ((buffer-live-p buffer)))
          (with-current-buffer buffer (set-buffer-modified-p nil))
          (kill-buffer buffer)))
      (when (mevedel-session-save-path session)
        (mevedel-session-persistence-lock-release
         (mevedel-session-save-path session)))
      (when (buffer-live-p parent) (kill-buffer parent))
      (delete-directory root t))))

(mevedel-deftest mevedel-tool-ui--agent/model-policy
  (:before-each (mevedel-tools-register)
   :after-each (mevedel-workspace-clear-registry))
  ,test
  (test)
  :doc "freezes parent, role, and explicit model policy through the public tool"
  (let* ((root (file-name-as-directory
                (make-temp-file "mevedel-agent-model-policy-" t)))
         (workspace (mevedel-workspace-get-or-create
                     'project "agent-model-policy" root "agent-model-policy"))
         (session (mevedel-session-create "main" workspace))
         (parent (generate-new-buffer " *mevedel-agent-model-policy*"))
         (role (mevedel-agent--create
                :name "model_test"
                :description "Model policy test"
                :system-prompt "Use the frozen model policy."))
         (old-custom (get 'gptel-reasoning-effort 'custom-type))
         (old-parent (get 'parent-model :reasoning-effort))
         (old-role (get 'role-model :reasoning-effort))
         (old-explicit (get 'explicit-model :reasoning-effort))
         invocations
         callbacks)
    (push (cons "model_test" role) mevedel-agent--registry)
    (unwind-protect
        (let ((gptel--known-backends nil))
          (gptel-make-openai "Parent" :key "test" :models '(parent-model))
          (gptel-make-openai "Role" :key "test" :models '(role-model))
          (gptel-make-openai "Explicit" :key "test" :models '(explicit-model))
          (put 'gptel-reasoning-effort 'custom-type '(choice symbol integer))
          (dolist (model '(parent-model role-model explicit-model))
            (put model :reasoning-effort '(member low medium high)))
          (with-current-buffer parent
            (setq-local mevedel--session session)
            (setq-local mevedel--workspace workspace)
            (setq-local gptel-backend (gptel-get-backend "Parent"))
            (setq-local gptel-model 'parent-model)
            (setq-local gptel-reasoning-effort 'low)
            (let ((gptel-agent-preset nil)
                  (mevedel-agents--specs nil)
                  (mevedel-model-tiers
                   '((role-tier :provider "Role:role-model" :effort medium)
                     (explicit-tier :provider "Explicit:explicit-model")))
                  (mevedel-model-workloads '((model_test :tier role-tier))))
              (should (eq 'low gptel-reasoning-effort))
              (should (eq 'low
                          (plist-get
                           (mevedel-model-resolve-workload 'default)
                           :effort)))
              (cl-letf (((symbol-function 'mevedel-agent-exec-run)
                         (lambda (callback _role _description invocation
                                           _buffer &optional _configure)
                           (push callback callbacks)
                           (push invocation invocations)
                           'provider-request)))
                (cl-labels
                    ((launch-policy (task role-name &rest controls)
                       (mevedel-tool-ui--agent
                        #'ignore
                        (append (list :task_name task
                                      :message "Inspect model policy.")
                                (and role-name (list :role role-name))
                                controls))
                       (let* ((invocation (car invocations))
                              (configuration
                               (mevedel-agent-invocation-frozen-configuration
                                invocation))
                              (snapshot
                               (mevedel-agent-configuration-request-locals
                                configuration)))
                         (funcall (car callbacks) "Model policy inspected.")
                         snapshot)))
                  (let ((snapshot (launch-policy "parent_policy" nil)))
                    (should (equal "Parent"
                                   (gptel-backend-name
                                    (alist-get 'gptel-backend snapshot))))
                    (should (eq 'parent-model
                                (alist-get 'gptel-model snapshot)))
                    (should (eq 'low
                                (alist-get 'gptel-reasoning-effort snapshot))))
                  (let ((snapshot
                         (launch-policy "role_policy" "model_test")))
                    (should (equal "Role"
                                   (gptel-backend-name
                                    (alist-get 'gptel-backend snapshot))))
                    (should (eq 'role-model
                                (alist-get 'gptel-model snapshot)))
                    (should (eq 'medium
                                (alist-get 'gptel-reasoning-effort snapshot))))
                  (let ((snapshot
                         (launch-policy
                          "explicit_policy" "model_test"
                          :model "explicit-tier" :effort "high")))
                    (should (equal "Explicit"
                                   (gptel-backend-name
                                    (alist-get 'gptel-backend snapshot))))
                    (should (eq 'explicit-model
                                (alist-get 'gptel-model snapshot)))
                    (should (eq 'high
                                (alist-get 'gptel-reasoning-effort snapshot))))))))
      (setq mevedel-agent--registry
            (assoc-delete-all "model_test" mevedel-agent--registry))
      (put 'gptel-reasoning-effort 'custom-type old-custom)
      (put 'parent-model :reasoning-effort old-parent)
      (put 'role-model :reasoning-effort old-role)
      (put 'explicit-model :reasoning-effort old-explicit)
      (dolist (invocation invocations)
        (when-let* ((buffer (mevedel-agent-invocation-buffer invocation))
                    ((buffer-live-p buffer)))
          (with-current-buffer buffer (set-buffer-modified-p nil))
          (kill-buffer buffer)))
      (when (mevedel-session-save-path session)
        (mevedel-session-persistence-lock-release
         (mevedel-session-save-path session)))
      (when (buffer-live-p parent) (kill-buffer parent))
      (delete-directory root t)))))

(mevedel-deftest mevedel-tool-ui--list-agents
  (:after-each (mevedel-workspace-clear-registry))
  ,test
  (test)
  :doc "returns the stable roster, treats an empty prefix as omitted, and filters subtrees"
  (let* ((workspace (mevedel-workspace--create
                     :type 'project :id "list-agents"
                     :root temporary-file-directory :name "list-agents"))
         (session (mevedel-session-create "main" workspace))
         (buffer (generate-new-buffer " *mevedel-list-agents*")))
    (setf (mevedel-session-agent-registry session)
          (list
           (cons "/root/zeta"
                 (mevedel-agent-record--create
                  :id "default--private-z" :path "/root/zeta"
                  :role "default" :activity 'idle
                  :conversation-location "agents/zeta.chat.org"))
           (cons "/root/alpha"
                 (mevedel-agent-record--create
                  :id "default--private-a" :path "/root/alpha"
                  :role "default" :activity 'running))))
    (unwind-protect
        (with-current-buffer buffer
          (setq-local mevedel--session session)
          (let ((result (plist-get (mevedel-tool-ui--list-agents nil)
                                   :result)))
            (should (< (string-match "/root" result)
                       (string-match "/root/alpha" result)))
            (should (< (string-match "/root/alpha" result)
                       (string-match "/root/zeta" result)))
            (should-not (string-match-p "private" result))
            (should-not (string-match-p "chat.org" result))
            (should
             (equal result
                    (plist-get
                     (mevedel-tool-ui--list-agents '(:path_prefix ""))
                     :result))))
          (let ((result
                 (plist-get
                  (mevedel-tool-ui--list-agents
                   '(:path_prefix "/root/alpha"))
                  :result)))
            (should (string-match-p "/root/alpha" result))
            (should-not (string-match-p "/root/zeta" result))))
      (kill-buffer buffer))))

(mevedel-deftest mevedel-tool-ui--followup-agent
  (:after-each (mevedel-workspace-clear-registry))
  ,test
  (test)
  :doc "continues an idle retained conversation and reports to its spawn parent"
  (let* ((root (file-name-as-directory
                (make-temp-file "mevedel-followup-idle-" t)))
         (workspace (mevedel-workspace-get-or-create
                     'project "followup-idle" root "followup-idle"))
         (session (mevedel-session-create "main" workspace))
         (parent (generate-new-buffer " *mevedel-followup-idle-parent*"))
         launches
         fail-launch)
    (unwind-protect
        (with-current-buffer parent
          (setq-local mevedel--session session)
          (setq-local mevedel--workspace workspace)
          (cl-letf (((symbol-function 'mevedel-agent-exec-run)
                     (lambda (callback _role _description invocation
                                       buffer &optional _configure)
                       (when fail-launch
                         (error "Simulated provider launch failure"))
                       (setq launches
                             (append launches
                                     (list (list :callback callback
                                                 :invocation invocation
                                                 :buffer buffer))))
                       'provider-request)))
            (let (started)
              (mevedel-tool-ui--agent
               (lambda (value &rest _) (setq started value))
               '(:task_name "worker" :message "Inspect feature one."))
              (should started))
            (let* ((initial (car launches))
                   (initial-invocation (plist-get initial :invocation))
                   (initial-buffer (plist-get initial :buffer))
                   (initial-id
                    (mevedel-agent-invocation-agent-id initial-invocation)))
              (funcall (plist-get initial :callback) "Feature one reviewed.")
              (setf (mevedel-session-messages session) nil)
              (let ((result
                     (mevedel-tool-ui--followup-agent
                      '(:target "worker"
                        :message "Now review feature two."))))
                (should (equal "" (plist-get result :result)))
                (should (equal "/root/worker"
                               (plist-get (plist-get result :render-data)
                                          :path))))
              (should (= 2 (length launches)))
              (let* ((followup (nth 1 launches))
                     (followup-invocation
                      (plist-get followup :invocation)))
                (should (eq initial-buffer (plist-get followup :buffer)))
                (should (equal initial-id
                               (mevedel-agent-invocation-agent-id
                                followup-invocation)))
                (with-current-buffer initial-buffer
                  (let ((text (buffer-string)))
                    (should (< (string-match "Inspect feature one" text)
                               (string-match "Now review feature two" text)))))
                (funcall (plist-get followup :callback)
                         "Feature two reviewed."))
              (let ((result (car (mevedel-session-messages session))))
                (should (= 1 (length (mevedel-session-messages session))))
                (should (equal "/root/worker" (plist-get result :sender)))
                (should (equal "/root" (plist-get result :recipient)))
                (should (string-match-p "Feature two reviewed"
                                        (plist-get result :payload))))
              (let* ((record
                      (cdr (assoc "/root/worker"
                                  (mevedel-session-agent-registry session))))
                     (location
                      (mevedel-agent-record-conversation-location record))
                     (text (with-current-buffer initial-buffer
                             (buffer-string))))
                (cl-letf
                    (((symbol-function
                       'mevedel-agent-conversation-save)
                      (lambda (_invocation) nil)))
                  (should-error
                   (mevedel-tool-ui--followup-agent
                    '(:target "worker"
                      :message "This task must roll back."))))
                (should (= 2 (length launches)))
                (should (eq 'idle (mevedel-agent-record-activity record)))
                (should (equal location
                               (mevedel-agent-record-conversation-location
                                record)))
                (should (equal text
                               (with-current-buffer initial-buffer
                                 (buffer-string))))
                (should
                 (eq 'completed
                     (plist-get
                      (cdr (assoc
                            (mevedel-agent-record-id record)
                            (mevedel-session-agent-transcripts session)))
                      :status)))
                (setq fail-launch t)
                (should-error
                 (mevedel-tool-ui--followup-agent
                  '(:target "worker"
                    :message "Record a failed launch.")))
                (should (= 2 (length launches)))
                (should (eq 'idle (mevedel-agent-record-activity record)))
                (should-not (mevedel-agent-record-invocation record))))))
      (dolist (launch launches)
        (when-let* ((buffer (plist-get launch :buffer))
                    ((buffer-live-p buffer)))
          (with-current-buffer buffer
            (set-buffer-modified-p nil))
          (kill-buffer buffer)))
      (when (mevedel-session-save-path session)
        (mevedel-session-persistence-lock-release
         (mevedel-session-save-path session)))
      (when (buffer-live-p parent)
        (kill-buffer parent))
      (delete-directory root t)))

  :doc "steers a running target at its next boundary when capacity is full"
  (let* ((root (file-name-as-directory
                (make-temp-file "mevedel-followup-running-" t)))
         (workspace (mevedel-workspace-get-or-create
                     'project "followup-running" root "followup-running"))
         (session (mevedel-session-create "main" workspace))
         (parent (generate-new-buffer " *mevedel-followup-running-parent*"))
         launches)
    (unwind-protect
        (with-current-buffer parent
          (setq-local mevedel--session session)
          (setq-local mevedel--workspace workspace)
          (cl-letf (((symbol-function 'mevedel-agent-exec-run)
                     (lambda (callback _role _description invocation
                                       buffer &optional _configure)
                       (setq launches
                             (append launches
                                     (list (list :callback callback
                                                 :invocation invocation
                                                 :buffer buffer))))
                       'provider-request)))
            (dolist (name '("one" "two" "three"))
              (mevedel-tool-ui--agent
               #'ignore (list :task_name name :message "Initial task.")))
            (should (= 3 (mevedel-agent-control--active-count session)))
            (let ((result
                   (mevedel-tool-ui--followup-agent
                    '(:target "/root/one"
                      :message "Incorporate this steering."))))
              (should (equal "" (plist-get result :result))))
            (should (= 3 (length launches)))
            (should (= 3 (mevedel-agent-control--active-count session)))
            (should-not (mevedel-session-messages session))
            (let* ((first (car launches))
                   (invocation (plist-get first :invocation))
                   (record (cdr (assoc "/root/one"
                                       (mevedel-session-agent-registry
                                        session))))
                   (message (car (mevedel-agent-record-mailbox record))))
              (should (eq 'MAIL (plist-get message :type)))
              (should (equal "/root" (plist-get message :sender)))
              (should (equal "/root/one" (plist-get message :recipient)))
              (should (equal "Incorporate this steering."
                             (plist-get message :payload)))
              ;; The ordinary WAIT handler drains the follow-up before the
              ;; provider's next safe request boundary.
              (mevedel-agent-control-clear-context-mailbox invocation)
              (funcall (plist-get first :callback)
                       "Steered task finished."))
            (should (= 3 (length launches)))
            (should (= 2 (mevedel-agent-control--active-count session)))
            (should (= 1 (length (mevedel-session-messages session))))))
      (dolist (launch launches)
        (when-let* ((buffer (plist-get launch :buffer))
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
  (:doc "renders canonical asynchronous Agent start events")
  ,test
  (test)

  :doc "returns one canonical Started row keyed by path"
  (let ((rendering
         (mevedel-tool-ui--render-agent
          "Agent" '(:task_name "spec_review")
          "{\"path\":\"/root/spec_review\"}"
          '(:kind collaboration-event
            :event started
            :path "/root/spec_review"
            :agent-id "default--internal"
            :status running))))
    (should (equal "Started /root/spec_review"
                   (plist-get rendering :header)))
    (should (equal "/root/spec_review"
                   (plist-get rendering :agent-path)))
    (should (equal "default--internal"
                   (plist-get rendering :agent-id)))
    (should (eq 'agent-handle (plist-get rendering :vtype))))

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

  :doc "Started paths activate their retained transcript"
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (let ((rendering
             (mevedel-tool-ui--render-agent
              "Agent" '(:task_name "spec_review")
              "{\"path\":\"/root/spec_review\"}"
              '(:kind collaboration-event
                :event started
                :path "/root/spec_review"
                :status running)))
            opened)
        (let ((inhibit-read-only t))
          (goto-char mevedel-view--input-marker)
          (mevedel-view--insert-rendered-tool rendering (cons 1 1)))
        (goto-char (point-min))
        (search-forward "/root/spec_review")
        (goto-char (match-beginning 0))
        (cl-letf (((symbol-function 'mevedel-view-open-agent-transcript)
                   (lambda (path) (setq opened path))))
          (mevedel-view-activate-at-point))
        (should (equal "/root/spec_review" opened)))))

  :doc "rejects missing and malformed collaboration event data"
  (dolist (render-data
           '(nil
             (:kind collaboration-event :event started)
             (:kind collaboration-event :event started :path 7)
             (:kind collaboration-event :event interacted
              :path "/root/spec_review")
             (:kind unrelated :event started :path "/root/spec_review")))
    (should-not
     (mevedel-tool-ui--render-agent
      "Agent" '(:task_name "spec_review") "Error: launch failed"
      render-data)))
)


(mevedel-deftest mevedel-tool-ui--render-agent-interaction
  (:doc "Renders retained-agent interaction events")
  ,test
  (test)
  :doc "renders Interacted with PATH without changing a leading-> draft"
  (mevedel-view-test--with-buffers
    (let ((draft "> quoted\nsecond line")
          (rendering
           (mevedel-tool-ui--render-agent-interaction
            "FollowupAgent"
            '(:target "/root/spec_review")
            ""
            '(:kind collaboration-event
              :event interacted
              :path "/root/spec_review"))))
      (should (equal "Interacted with /root/spec_review"
                     (plist-get rendering :header)))
      (should-not
       (mevedel-tool-ui--render-agent-interaction
        "FollowupAgent" '(:target "/root/missing")
        "Error: Unknown agent target" nil))
      (with-current-buffer view-buf
        (mevedel-view-test--insert-composer-draft draft 4)
        (let ((inhibit-read-only t))
          (goto-char mevedel-view--input-marker)
          (set-marker-insertion-type mevedel-view--input-marker t)
          (unwind-protect
              (mevedel-view--insert-rendered-tool rendering (cons 1 1))
            (set-marker-insertion-type mevedel-view--input-marker nil)))
        (should (string= draft (mevedel-view--input-text)))))))

(mevedel-deftest mevedel-tool-ui--render-list-agents
  (:doc "Renders the retained-agent roster")
  ,test
  (test)
  :doc "renders aligned columns and preserves a multiline leading-> draft"
  (mevedel-view-test--with-buffers
    (let* ((draft "> quoted\nsecond line")
           (rendering
            (mevedel-tool-ui--render-list-agents
             "ListAgents" nil
             "[{\"path\":\"/root\",\"role\":\"default\",\"activity\":\"running\"},{\"path\":\"/root/v2_smoke\",\"role\":\"explorer\",\"activity\":\"idle\"}]"
             nil)))
      (should (equal "Session agents (2)" (plist-get rendering :header)))
      (should
       (equal (concat "Path            Role      Activity\n"
                      "/root           default   running\n"
                      "/root/v2_smoke  explorer  idle")
              (plist-get rendering :body)))
      (with-current-buffer view-buf
        (mevedel-view-test--insert-composer-draft draft 4)
        (let ((inhibit-read-only t))
          (goto-char mevedel-view--input-marker)
          (set-marker-insertion-type mevedel-view--input-marker t)
          (unwind-protect
              (mevedel-view--insert-rendered-tool rendering (cons 1 1))
            (set-marker-insertion-type mevedel-view--input-marker nil)))
        (should (string= draft (mevedel-view--input-text))))))

  :doc "falls back to generic rendering for malformed results"
  (should-not
   (mevedel-tool-ui--render-list-agents
    "ListAgents" nil "not json" nil)))

(mevedel-deftest mevedel-tool-ui--interrupt-agent
  (:doc "Interrupts retained turns through canonical path events")
  ,test
  (test)
  :doc "returns the previous activity as JSON with canonical render data"
  (let* ((session (mevedel-tool-ui-test--session))
         (record
          (mevedel-agent-record--create
           :id "idle-id" :path "/root/idle" :activity 'idle))
         result)
    (setf (mevedel-session-agent-registry session)
          (list (cons "/root/idle" record)))
    (let ((mevedel--session session))
      (setq result
            (mevedel-tool-ui--interrupt-agent
             '(:target "/root/idle"))))
    (should
     (equal "idle"
            (gethash
             "previous_activity"
             (json-parse-string (plist-get result :result)))))
    (should (equal 'interrupted
                   (plist-get (plist-get result :render-data) :event)))
    (should (equal "/root/idle"
                   (plist-get (plist-get result :render-data) :path))))

  :doc "renders Interrupted PATH without changing a leading-> draft"
  (mevedel-view-test--with-buffers
    (let* ((draft "> quoted\nsecond line")
           (rendering
            (mevedel-tool-ui--render-interrupt-agent
             "InterruptAgent" '(:target "/root/spec_review")
             "{\"previous_activity\":\"running\"}"
             '(:kind collaboration-event
               :event interrupted
               :path "/root/spec_review"))))
      (should (equal "Interrupted /root/spec_review"
                     (plist-get rendering :header)))
      (with-current-buffer view-buf
        (mevedel-view-test--insert-composer-draft draft 4)
        (let ((inhibit-read-only t))
          (goto-char mevedel-view--input-marker)
          (set-marker-insertion-type mevedel-view--input-marker t)
          (unwind-protect
              (mevedel-view--insert-rendered-tool rendering (cons 1 1))
            (set-marker-insertion-type mevedel-view--input-marker nil)))
        (should (string= draft (mevedel-view--input-text)))))))

(mevedel-deftest mevedel-tool-ui--send-message ()
  ,test
  (test)
  :doc "returns empty success with canonical interaction render data"
  (let* ((session (mevedel-tool-ui-test--session))
         (record (mevedel-agent-record--create
                  :id "peer-id" :path "/root/peer" :activity 'idle))
         result)
    (setf (mevedel-session-agent-registry session)
          (list (cons "/root/peer" record)))
    (let ((mevedel--session session))
      (setq result
            (mevedel-tool-ui--send-message
             '(:target "/root/peer" :message "hello"))))
    (should (equal "" (plist-get result :result)))
    (should (equal "/root/peer"
                   (plist-get (plist-get result :render-data) :path)))))

(mevedel-deftest mevedel-tool-ui--wait-agent ()
  ,test
  (test)
  :doc "uses the ordinary async callback and returns only a wake summary"
  (let ((session (mevedel-tool-ui-test--session))
        delivered)
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (&rest _) 'fake-timer)))
      (let ((mevedel--session session))
        (mevedel-tool-ui--wait-agent
         (lambda (value) (setq delivered value))
         '(:timeout_ms 10000)))
      (mevedel-agent-control-send-message session "/root" "secret body")
      (should (equal "Mailbox activity"
                     (plist-get delivered :result)))
      (should-not (string-match-p
                   "secret body" (plist-get delivered :result)))
      (should (equal 'finished-waiting
                     (plist-get (plist-get delivered :render-data) :event)))))

  :doc "request cancellation removes the root waiter without a stale callback"
  (let* ((session (mevedel-tool-ui-test--session))
         (request (mevedel-request--create
                   :session session :origin "/root"))
         delivered
         scheduled)
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (_delay _repeat function &rest args)
                 (setq scheduled (cons function args))
                 'fake-timer)))
      (let ((mevedel--session session)
            (mevedel--current-request request))
        (mevedel-tool-ui--wait-agent
         (lambda (value) (setq delivered value))
         '(:timeout_ms 10000)))
      (should (mevedel-session-agent-root-waiter session))
      (mevedel-request-cancel request)
      (should-not (mevedel-session-agent-root-waiter session))
      (apply (car scheduled) (cdr scheduled))
      (should-not delivered)))

  :doc "renders pending and completed wait text while preserving a leading-> draft"
  (mevedel-view-test--with-buffers
    (let* ((draft "> quoted\nsecond line")
           (rendering
            (mevedel-tool-ui--render-wait-agent
             "WaitAgent" nil "Timeout elapsed"
             '(:kind collaboration-event :event finished-waiting))))
      (should (equal "Waiting for agents"
                     (mevedel-view--tool-status-string "WaitAgent" nil)))
      (should (equal "Finished waiting" (plist-get rendering :header)))
      (with-current-buffer view-buf
        (mevedel-view-test--insert-composer-draft draft 4)
        (let ((inhibit-read-only t))
          (goto-char mevedel-view--input-marker)
          (set-marker-insertion-type mevedel-view--input-marker t)
          (unwind-protect
              (mevedel-view--insert-rendered-tool rendering (cons 1 1))
            (set-marker-insertion-type mevedel-view--input-marker nil)))
        (should (string= draft (mevedel-view--input-text)))))))

(provide 'test-mevedel-tool-ui)

;;; test-mevedel-tool-ui.el ends here
