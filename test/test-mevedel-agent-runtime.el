;;; test-mevedel-agent-runtime.el --- Retained agent runner tests -*- lexical-binding: t -*-

;;; Commentary:

;; Focused Agent V2 provider-boundary, settlement, interruption, and yielded
;; execution tests.

;;; Code:

(require 'gptel-request)
(require 'mevedel-agent-control)
(require 'mevedel-agent-exec)
(require 'mevedel-agent-persistence)
(require 'mevedel-claude-code-backend)
(require 'mevedel-agent-conversation)
(require 'mevedel-agents)
(require 'mevedel-reminders)
(require 'mevedel-agent-runtime)
(require 'mevedel-execution)
(require 'mevedel-permission-queue)
(require 'mevedel-pipeline)
(require 'mevedel-sandbox)
(require 'mevedel-session-artifacts)
(require 'mevedel-session-persistence)
(require 'mevedel-structs)
(require 'mevedel-tool-task)
(require 'mevedel-tool-permission)
(require 'mevedel-tool-ptc)
(require 'mevedel-tools)
(require 'mevedel-turn)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-session-publication-test-helpers
         (file-name-concat (file-name-directory (or load-file-name buffer-file-name))
                           "mevedel-session-publication-test-helpers"))
(require 'mevedel-execution-test-helpers
         (file-name-concat (file-name-directory (or load-file-name buffer-file-name))
                           "mevedel-execution-test-helpers"))

(defun mevedel-agent-runtime-test--agent ()
  "Return a small frozen test agent."
  (mevedel-agent--create
   :name "explorer"
   :description "Explore"
   :tools nil
   :system-prompt "Explore the workspace."
   :frozen-p t))

(defun mevedel-agent-runtime-test--configuration (agent)
  "Return a frozen request configuration for AGENT."
  (mevedel-agent-configuration--create
   :agent agent
   :request-locals nil))

(defun mevedel-agent-runtime-test--invocation (&optional buffer)
  "Return a running retained invocation with optional BUFFER."
  (let* ((agent (mevedel-agent-runtime-test--agent))
         (invocation (mevedel-agent-invocation-create agent)))
    (setf (mevedel-agent-invocation-agent-id invocation) "explorer--test"
          (mevedel-agent-invocation-path invocation) "/root/explore"
          (mevedel-agent-invocation-description invocation) "Explore"
          (mevedel-agent-invocation-buffer invocation) buffer
          (mevedel-agent-invocation-transcript-status invocation) 'running)
    invocation))

(mevedel-deftest mevedel-agent-runtime--run-stop-hook ()
  ,test
  (test)
  :doc "fires SubagentStop with invocation metadata"
  (let* ((session (mevedel-session--create :name "main"))
         (agent (mevedel-agent--create :name "explorer"))
         (parent-buffer (generate-new-buffer " *mev-agent-parent-hooks*"))
         (invocation
          (mevedel-agent-invocation--create
           :path "/root/test_agent"
           :agent agent
           :parent-session session
           :parent-data-buffer parent-buffer))
         stop-event)
    (unwind-protect
        (progn
          (with-current-buffer parent-buffer
            (setq-local mevedel-subagent-stop-functions
                        (list (lambda (event)
                                (setq stop-event event)
                                nil))))
          (setf (mevedel-agent-invocation-terminal-reason invocation) "done")
          (with-temp-buffer
            (mevedel-agent-runtime--run-stop-hook invocation 'completed))
          (should (equal (plist-get stop-event :role) "explorer"))
          (should (equal (plist-get stop-event :agent-path)
                         "/root/test_agent"))
          (should (eq (plist-get stop-event :status) 'completed))
          (should (equal (plist-get stop-event :terminal-reason) "done")))
      (kill-buffer parent-buffer))))

(mevedel-deftest mevedel-agent-runtime--run-prompt-hook ()
  ,test
  (test)
  :doc "runs UserPromptSubmit with retained-agent metadata"
  (let* ((session (mevedel-session--create :name "main"))
         (invocation (mevedel-agent-runtime-test--invocation))
         event
         (mevedel-user-prompt-submit-functions
          (list (lambda (payload)
                  (setq event payload)
                  '(:updated-input "rewritten")))))
    (setf (mevedel-agent-invocation-parent-session invocation) session)
    (should
     (equal '(:updated-input "rewritten")
            (mevedel-agent-runtime--run-prompt-hook
             "Prompt body" invocation)))
    (should (equal (plist-get event :agent-path) "/root/explore"))
    (should (equal (plist-get event :prompt) "Prompt body"))))

(mevedel-deftest mevedel-agent-runtime--prepare-followup ()
  ,test
  (test)
  :doc "composes retained pending context without consuming it during preparation"
  (let* ((session (mevedel-session--create :name "main"))
         (invocation (mevedel-agent-runtime-test--invocation))
         (pending '((:event Old :source "old" :body "pending context")))
         transition
         lifecycle
         (mevedel-user-prompt-submit-functions
          (list (lambda (_event)
                  (push 'submit lifecycle)
                  '(:updated-input "rewritten prompt"
                    :additional-context ("prompt context"))))))
    (setf (mevedel-agent-invocation-parent-session invocation) session)
    (let ((turn
           (mevedel-agent-runtime--prepare-followup
            "original prompt" invocation pending
            (lambda (entries) (setq transition entries)))))
      (should (equal '(submit) (nreverse lifecycle)))
      (should (string-match-p "rewritten prompt" (plist-get turn :prompt)))
      (should (string-match-p "pending context" (plist-get turn :prompt)))
      (should (string-match-p "prompt context" (plist-get turn :prompt)))
      (should-not transition)
      (should (plist-get turn :audits)))))

(mevedel-deftest mevedel-agent-runtime--prepared-turn ()
  ,test
  (test)
  :doc "applies the accepted rewrite and ordered hook context once"
  (let ((turn
         (mevedel-agent-runtime--prepared-turn
          "Original"
          '(:additional-context ("start context"))
          '(:updated-input "Rewritten"
            :additional-context ("prompt context"))
          nil)))
    (should (string-match-p "Rewritten" (plist-get turn :prompt)))
    (should (string-match-p "start context" (plist-get turn :prompt)))
    (should (string-match-p "prompt context" (plist-get turn :prompt)))))

(mevedel-deftest mevedel-agent-runtime-prepare-task ()
  ,test
  (test)
  :doc "runs the two task hooks asynchronously once and returns one final task"
  (let ((session (mevedel-session--create :name "main"))
        (agent (mevedel-agent-runtime-test--agent))
        callbacks
        events
        outcome)
    (with-temp-buffer
      (setq-local mevedel--session session)
      (cl-letf (((symbol-function 'mevedel-hooks-run-event)
                 (lambda (event _payload callback &rest _)
                   (push event events)
                   (push callback callbacks))))
        (mevedel-agent-runtime-prepare-task
         agent "Explore" "Original task" "/root/explore"
         (lambda (value) (setq outcome value)))
        (should (equal '(SubagentStart) events))
        (funcall (car callbacks)
                 '(:additional-context ("start context")))
        (should (equal '(UserPromptSubmit SubagentStart) events))
        (funcall (car callbacks)
                 '(:updated-input "Rewritten task"
                   :additional-context ("prompt context")))
        (should (eq 'success (plist-get outcome :outcome)))
        (let ((prompt (plist-get (plist-get outcome :turn) :prompt)))
          (should (string-match-p "Rewritten task" prompt))
          (should (string-match-p "start context" prompt))
          (should (string-match-p "prompt context" prompt))))))

  :doc "suppresses a late hook result after cancellation"
  (let ((session (mevedel-session--create :name "main"))
        (agent (mevedel-agent-runtime-test--agent))
        hook-callback
        cancelled
        outcome)
    (with-temp-buffer
      (setq-local mevedel--session session)
      (cl-letf (((symbol-function 'mevedel-hooks-run-event)
                 (lambda (_event _payload callback &rest _)
                   (setq hook-callback callback))))
        (mevedel-agent-runtime-prepare-task
         agent "Explore" "Original task" "/root/explore"
         (lambda (value) (setq outcome value))
         :cancelled-p (lambda () cancelled))
        (setq cancelled t)
        (funcall hook-callback nil)
        (should-not outcome)))))

(mevedel-deftest mevedel-agent-runtime-task-background ()
  ,test
  (test)
  :doc "labels generated background as advisory before the authoritative task"
  (let ((block (mevedel-agent-runtime-task-background "  ## Scope\n- context  ")))
    (should (string-prefix-p "<task-background>\n" block))
    (should (string-match-p "may be stale or untrusted" block))
    (should (string-match-p "The following Agent Task is authoritative" block))
    (should (string-match-p "## Scope\n- context" block))
    (should (string-suffix-p "</task-background>\n" block))))

(mevedel-deftest mevedel-agent-runtime-dispatch
  (:quiet t)
  ,test
  (test)
  :doc "rejects dispatch outside an active session"
  (with-temp-buffer
    (let ((mevedel--session nil))
      (should-error
       (mevedel-agent-runtime-dispatch
        (mevedel-agent-runtime-test--agent)
        "Explore" "Inspect files" :path "/root/explore"))))

  :doc "a synchronous same-message retry failure retains its newly reported notice"
  (let* ((parent (generate-new-buffer " *agent-retry-parent*"))
         (child (generate-new-buffer " *agent-retry-child*"))
         (session (mevedel-session--create :name "main" :authority-mode 'pid-lock))
         (agent (mevedel-agent-runtime-test--agent))
         (configuration (mevedel-agent-runtime-test--configuration agent))
         retry)
    (unwind-protect
        (with-current-buffer parent
          (setq-local mevedel--session session)
          (mevedel-recovery-report
           session "agent:explorer--test" 'request
           "Agent /root/explore failed; inspect its transcript before continuing" nil)
          (cl-letf (((symbol-function 'mevedel-agent-conversation-save)
                     (lambda (&rest _) t))
                    ((symbol-function 'mevedel-agent-runtime--finalize)
                     (lambda (invocation status &optional _token)
                       (setf (mevedel-agent-invocation-transcript-status invocation)
                             status)))
                    ((symbol-function 'run-at-time)
                     (lambda (_seconds _repeat callback &rest _args)
                       (setq retry callback)))
                    ((symbol-function 'mevedel-agent-exec-run)
                     (lambda (callback role description invocation _buffer)
                       (funcall
                        (mevedel-agent-exec--make-callback
                         callback role description nil (list nil))
                        nil (list :error "Provider failed"
                                  :mevedel-agent-invocation invocation))
                       'provider-fsm)))
            (let ((invocation
                   (mevedel-agent-runtime-dispatch
                    nil "Retry" "Continue"
                    :path "/root/explore" :frozen-configuration configuration
                    :retained-id "explorer--test" :retained-buffer child
                    :retained-transcript "agents/explorer.chat.org")))
              (should retry)
              (should (eq 'error (mevedel-agent-invocation-transcript-status invocation)))
              (should (= 1 (length (mevedel-session-recovery-issues session))))
              ;; The callback retries after publication; it must not lose the
              ;; notice while that delivery is pending or after settlement.
              (funcall retry)
              (should (mevedel-agent-invocation-runtime-settled-p invocation))
              (should (= 1 (length (mevedel-session-recovery-issues session)))))))
      (kill-buffer child)
      (kill-buffer parent)))

  :doc "starts one fresh asynchronous turn and settles its callback once"
  (let* ((parent (generate-new-buffer " *agent-runtime-parent*"))
         (agent-buffer (generate-new-buffer " *agent-runtime-child*"))
         (session (mevedel-session--create :name "main"))
         (agent (mevedel-agent-runtime-test--agent))
         (configuration
          (mevedel-agent-runtime-test--configuration agent))
         (root-fsm (gptel-make-fsm
                    :info (list :mevedel-goal-accounting-id "goal-1")))
         provider-callback
         seen-invocation
         settlements
         finalizations
         events)
    (unwind-protect
        (with-current-buffer parent
          (setq-local mevedel--session session)
          (setq-local mevedel--current-request
                      (mevedel-request--create :plan-read-only t
                                               :fsm root-fsm))
          (cl-letf
              (((symbol-function
                 'mevedel-agent-exec-freeze-configuration)
                (lambda (&rest _) configuration))
               ((symbol-function
                 'mevedel-agent-conversation-open)
                (lambda (&rest _) agent-buffer))
               ((symbol-function 'mevedel-agent-runtime--setup-transcript)
                (lambda (invocation _buffer)
                  (push 'transcript-setup events)
                  (setf
                   (mevedel-agent-invocation-transcript-relative-path
                    invocation)
                   "agents/explorer.chat.org")))
               ((symbol-function 'mevedel-agent-conversation-configure)
                (lambda (&rest _)
                  (push 'conversation-configure events)))
               ((symbol-function
                 'mevedel-agent-conversation-save)
                (lambda (&rest _)
                  (push 'transcript-save events)
                  t))
               ((symbol-function 'mevedel-agent-exec-run)
                (lambda (callback _role _description invocation _buffer)
                  (setq provider-callback callback)
                  (setf (mevedel-agent-invocation-runtime-fsm invocation)
                        'provider-fsm)
                  'provider-fsm))
               ((symbol-function 'mevedel-agent-runtime--execution-live-p)
                (lambda (_invocation) nil))
               ((symbol-function 'mevedel-view-agent-live-transcript-start)
                (lambda (_invocation)
                  (push 'view-start events)))
               ((symbol-function 'mevedel-agent-runtime--finalize)
                (lambda (invocation status &optional _token)
                  (push status finalizations)
                  (setf (mevedel-agent-invocation-transcript-status invocation)
                        status))))
            (let ((invocation
                   (mevedel-agent-runtime-dispatch
                    agent "Explore" "Find the entry point."
                    :path "/root/explore"
                    :prepared-turn '(:prompt "Find the entry point.")
                    :context-snapshot "Prior context"
                    :on-invocation
                    (lambda (inv)
                      (push 'invocation-published events)
                      (setq seen-invocation inv))
                    :on-settle
                    (lambda (inv response event &optional _token)
                      (push (list inv response event) settlements)))))
              (should (eq invocation seen-invocation))
              ;; The Goal-charged root request pays for the agent.
              (should (eq root-fsm
                          (mevedel-agent-invocation-goal-owner invocation)))
              (should
               (mevedel-agent-invocation-plan-read-only invocation))
              (should (eq 'provider-fsm
                          (mevedel-agent-invocation-runtime-fsm invocation)))
              (should (equal "/root/explore"
                             (mevedel-agent-invocation-path invocation)))
              (should
               (equal '(transcript-setup conversation-configure
                         transcript-save invocation-published view-start)
                      (nreverse events)))
              (with-current-buffer agent-buffer
                (should (string-match-p "Prior context" (buffer-string)))
                (should (string-match-p "Find the entry point"
                                        (buffer-string)))
                (should (string-match-p "/root/explore" (buffer-string))))
              (funcall provider-callback "Finished")
              (funcall provider-callback "Duplicate")
              (should (equal '(completed) finalizations))
              (should (= 1 (length settlements)))
              (should (equal "Finished" (cadar settlements)))
              (should
               (mevedel-agent-invocation-runtime-settled-p invocation)))))
      (kill-buffer agent-buffer)
      (kill-buffer parent)))

  :doc "an explicit Plan stamp applies without a Plan caller"
  (let* ((parent (generate-new-buffer " *agent-runtime-plan-parent*"))
         (agent-buffer (generate-new-buffer " *agent-runtime-plan-child*"))
         (agent (mevedel-agent-runtime-test--agent))
         (configuration (mevedel-agent-runtime-test--configuration agent)))
    (unwind-protect
        (with-current-buffer parent
          (setq-local mevedel--session (mevedel-session--create :name "main"))
          (cl-letf (((symbol-function 'mevedel-agent-exec-freeze-configuration)
                     (lambda (&rest _) configuration))
                    ((symbol-function 'mevedel-agent-conversation-open)
                     (lambda (&rest _) agent-buffer))
                    ((symbol-function 'mevedel-agent-runtime--setup-transcript)
                     (lambda (invocation _buffer)
                       (setf (mevedel-agent-invocation-transcript-relative-path
                              invocation)
                             "agents/explorer.chat.org")))
                    ((symbol-function 'mevedel-agent-conversation-configure)
                     #'ignore)
                    ((symbol-function 'mevedel-agent-conversation-save)
                     (lambda (&rest _) t))
                    ((symbol-function 'mevedel-agent-exec-run)
                     (lambda (&rest _) 'provider-fsm))
                    ((symbol-function 'mevedel-view-agent-live-transcript-start)
                     #'ignore))
            (dolist (stamp '(nil t))
              (should
               (eq stamp
                   (mevedel-agent-invocation-plan-read-only
                    (mevedel-agent-runtime-dispatch
                     agent "Explore" "Look."
                     :path "/root/explore"
                     :prepared-turn '(:prompt "Look.")
                     :plan-read-only stamp)))))))
      (kill-buffer agent-buffer)
      (kill-buffer parent)))

  :doc "public dispatch shares root authority and routes child prompts to it"
  (let* ((root (file-name-as-directory
                (make-temp-file "mevedel-agent-authority-" t)))
         (workspace (mevedel-workspace--create
                     :type 'project :id root :root root :name "authority"))
         (session (mevedel-session-create "main" workspace root))
         (parent (generate-new-buffer " *agent-authority-parent*"))
         (agent (mevedel-agent-runtime-test--agent))
         (configuration
          (mevedel-agent-runtime-test--configuration agent))
         invocation
         agent-buffer
         next-called)
    (setf (mevedel-session-permission-mode session) 'ask
          (mevedel-session-sandbox-mode session) 'required)
    (unwind-protect
        (with-current-buffer parent
          (setq-local mevedel--session session
                      mevedel--workspace workspace)
          (cl-letf
              (((symbol-function 'gptel-mode) #'ignore)
               ((symbol-function 'mevedel-agent-runtime--setup-transcript)
                (lambda (inv _buffer)
                  (setf
                   (mevedel-agent-invocation-transcript-relative-path inv)
                   "agents/worker.chat.org")))
               ((symbol-function 'mevedel-agent-conversation-configure)
                #'ignore)
               ((symbol-function 'mevedel-agent-conversation-save)
                (lambda (&rest _) t))
               ((symbol-function 'mevedel-agent-exec-run)
                (lambda (&rest _) 'provider-fsm))
               ((symbol-function 'mevedel-permission-queue--render-entry)
                #'ignore))
            (setq invocation
                  (mevedel-agent-runtime-dispatch
                   nil "Work" "Edit the file."
                   :path "/root/worker"
                   :prepared-turn '(:prompt "Edit the file.")
                   :frozen-configuration configuration)
                  agent-buffer
                  (mevedel-agent-invocation-buffer invocation))
            (with-current-buffer agent-buffer
              (should (eq session mevedel--session))
              (should
               (eq 'required
                   (mevedel-sandbox-mode-effective mevedel--session)))
              (let* ((tool (mevedel-tool--create
                            :name "Edit" :read-only-p nil :groups '(edit)))
                     (request (mevedel-request--create
                               :id "child-request"
                               :session session
                               :origin "/root/worker")))
                (mevedel-tool-permission-step
                 (list :tool tool :args nil :session session :request request)
                 (lambda (_context) (setq next-called t))
                 #'ignore)))
            (let ((entry (car (mevedel-session-permission-queue session))))
              (should (equal "/root/worker" (plist-get entry :origin)))
              (should (equal "child-request" (plist-get entry :request-id)))
              (funcall (plist-get entry :callback) 'allow-session))
            (should next-called)
            (should
             (equal '(("Edit" :action allow))
                    (mevedel-session-permission-rules session)))))
      (when (buffer-live-p agent-buffer) (kill-buffer agent-buffer))
      (when (buffer-live-p parent) (kill-buffer parent))
      (delete-directory root t)))

  :doc "continues a retained buffer without replaying forked context"
  (let* ((parent (generate-new-buffer " *agent-runtime-parent*"))
         (agent-buffer (generate-new-buffer " *agent-runtime-retained*"))
         (session (mevedel-session--create :name "main"))
         (agent (mevedel-agent-runtime-test--agent))
         (configuration
          (mevedel-agent-runtime-test--configuration agent))
         saved)
    (unwind-protect
        (progn
          (with-current-buffer agent-buffer
            (insert "Existing conversation\n"))
          (with-current-buffer parent
            (setq-local mevedel--session session)
            (cl-letf
                (((symbol-function
                   'mevedel-agent-conversation-save)
                  (lambda (&rest _)
                    (setq saved t)))
                 ((symbol-function 'mevedel-agent-exec-run)
                  (lambda (_callback _role _description invocation _buffer)
                    (setf (mevedel-agent-invocation-runtime-fsm invocation)
                          'continued-fsm)
                    'continued-fsm)))
              (let ((invocation
                     (mevedel-agent-runtime-dispatch
                      nil "Continue" "Review feature two."
                      :path "/root/explore"
                      :context-snapshot "Must not be copied"
                      :frozen-configuration configuration
                      :retained-id "explorer--test"
                      :retained-buffer agent-buffer
                      :retained-transcript "agents/explorer.chat.org")))
                (should saved)
                (should
                 (eq invocation
                     (buffer-local-value 'mevedel--agent-invocation
                                         agent-buffer)))
                (with-current-buffer agent-buffer
                  (should (string-match-p "Existing conversation"
                                          (buffer-string)))
                  (should (string-match-p "Review feature two"
                                          (buffer-string)))
                  (should-not (string-match-p "Must not be copied"
                                              (buffer-string))))))))
      (kill-buffer agent-buffer)
      (kill-buffer parent)))

  :doc "stamps a turn started under sticky Plan read-only for its lifetime"
  (dolist (plan-mode '(t nil))
    (let* ((parent (generate-new-buffer " *agent-runtime-parent*"))
           (agent-buffer (generate-new-buffer " *agent-runtime-retained*"))
           (session (mevedel-session--create :name "main" :plan-mode plan-mode))
           (configuration
            (mevedel-agent-runtime-test--configuration
             (mevedel-agent-runtime-test--agent))))
      (unwind-protect
          (with-current-buffer parent
            (setq-local mevedel--session session)
            (setq-local mevedel--current-request (mevedel-request--create))
            (cl-letf (((symbol-function 'mevedel-agent-conversation-save)
                       (lambda (&rest _) t))
                      ((symbol-function 'mevedel-agent-exec-run)
                       (lambda (_callback _role _description invocation _buffer)
                         (setf (mevedel-agent-invocation-runtime-fsm invocation)
                               'continued-fsm)
                         'continued-fsm)))
              (let ((invocation
                     (mevedel-agent-runtime-dispatch
                      nil "Continue" "Inspect the parser."
                      :path "/root/explore"
                      :frozen-configuration configuration
                      :retained-id "explorer--test"
                      :retained-buffer agent-buffer
                      :retained-transcript "agents/explorer.chat.org")))
                (should (eq plan-mode
                            (mevedel-agent-invocation-plan-read-only
                             invocation)))
                ;; Leaving Plan does not widen the running turn.
                (setf (mevedel-session-plan-mode session) nil)
                (with-current-buffer agent-buffer
                  (should (eq plan-mode
                              (and (mevedel-plan-read-only-p) t)))))))
        (kill-buffer agent-buffer)
        (kill-buffer parent))))

  :doc "copies forked context into the turn text only for an external engine"
  (let* ((gptel--known-backends nil)
         (claude (mevedel-claude-code-register))
         sent)
    (dolist (case `((nil . nil)
                    (((gptel-backend . ,claude) (gptel-model . sonnet))
                     . "PARENT TRANSCRIPT\n\nInitial task")))
      (let ((parent (generate-new-buffer " *agent-runtime-fork-parent*"))
            (agent-buffer (generate-new-buffer " *agent-runtime-fork-child*"))
            (configuration
             (mevedel-agent-configuration--create
              :agent (mevedel-agent-runtime-test--agent)
              :request-locals (car case))))
        (unwind-protect
            (with-current-buffer parent
              (setq-local mevedel--session (mevedel-session--create :name "main"))
              (cl-letf
                  (((symbol-function 'mevedel-agent-conversation-open)
                    (lambda (&rest _) agent-buffer))
                   ((symbol-function 'mevedel-agent-conversation-configure) #'ignore)
                   ((symbol-function 'mevedel-agent-runtime--setup-transcript)
                    (lambda (invocation _buffer)
                      (setf (mevedel-agent-invocation-transcript-relative-path
                             invocation)
                            "agents/explorer.chat.org")))
                   ((symbol-function 'mevedel-agent-conversation-save)
                    (lambda (&rest _) t))
                   ((symbol-function 'mevedel-agent-exec-run)
                    (lambda (_callback _role _description invocation _buffer)
                      (setq sent (plist-get (mevedel-engine-info invocation)
                                            :mevedel-agent-prompt))
                      'provider-fsm)))
                (mevedel-agent-runtime-dispatch
                 (mevedel-agent-runtime-test--agent) "Explore" "Initial task"
                 :path "/root/explore"
                 :context-snapshot "PARENT TRANSCRIPT"
                 :frozen-configuration configuration
                 :prepared-turn '(:prompt "Initial task"))
                (should (equal (cdr case) sent))
                (with-current-buffer agent-buffer
                  (should (string-match-p "PARENT TRANSCRIPT" (buffer-string))))))
          (kill-buffer agent-buffer)
          (kill-buffer parent)))))

  :doc "runs SubagentStart once for a retained conversation, not per turn"
  (let* ((parent (generate-new-buffer " *agent-runtime-start-parent*"))
         (agent-buffer (generate-new-buffer " *agent-runtime-start-child*"))
         (session (mevedel-session--create :name "main"))
         (agent (mevedel-agent-runtime-test--agent))
         (configuration
          (mevedel-agent-runtime-test--configuration agent))
         (start-count 0)
         (submit-count 0)
         lifecycle
         provider-prompts
         first)
    (unwind-protect
        (with-current-buffer parent
          (setq-local mevedel--session session)
          (let ((mevedel-subagent-start-functions
                 (list (lambda (_event)
                         (cl-incf start-count)
                         (push 'start lifecycle)
                         '(:additional-context ("one startup context")))))
                (mevedel-user-prompt-submit-functions
                 (list (lambda (_event)
                         (cl-incf submit-count)
                         (push 'submit lifecycle)
                         '(:additional-context ("per-turn context"))))))
            (cl-letf
                (((symbol-function 'mevedel-agent-conversation-open)
                  (lambda (&rest _) agent-buffer))
                 ((symbol-function 'mevedel-agent-runtime--setup-transcript)
                  (lambda (invocation _buffer)
                    (setf
                     (mevedel-agent-invocation-transcript-relative-path
                      invocation)
                     "agents/explorer.chat.org")))
                 ((symbol-function 'mevedel-agent-conversation-save)
                  (lambda (&rest _) t))
                 ((symbol-function 'mevedel-agent-exec-run)
                  (lambda (_callback _role _description invocation buffer)
                    (with-current-buffer buffer
                      (push (buffer-string) provider-prompts))
                    (push 'model lifecycle)
                    (setf (mevedel-agent-invocation-runtime-fsm invocation)
                          'provider-fsm)
                    'provider-fsm)))
              (let (prepared)
                (mevedel-agent-runtime-prepare-task
                 agent "Explore" "Initial task" "/root/explore"
                 (lambda (result) (setq prepared result)))
                (should (eq 'success (plist-get prepared :outcome)))
                (setq first
                      (mevedel-agent-runtime-dispatch
                       agent "Explore" "Initial task"
                       :path "/root/explore"
                       :frozen-configuration configuration
                       :prepared-turn (plist-get prepared :turn)
                       :start-hook-audits
                       (plist-get prepared :start-hook-audits))))
              (mevedel-agent-runtime-dispatch
               nil "Continue" "Follow-up task"
               :path "/root/explore"
               :frozen-configuration configuration
               :retained-id
               (mevedel-agent-invocation-agent-id first)
               :retained-buffer agent-buffer
               :retained-transcript "agents/explorer.chat.org")
              (should (= 1 start-count))
              (should (= 2 submit-count))
              (should (equal '(start submit model submit model)
                             (nreverse lifecycle)))
              (should (string-match-p "one startup context"
                                      (car (last provider-prompts))))
              (with-current-buffer agent-buffer
                (goto-char (point-min))
                (should (= 1 (how-many "one startup context")))))))
      (kill-buffer agent-buffer)
      (kill-buffer parent)))

  :doc "keeps blocked prompt context local until the next accepted agent turn"
  (let* ((parent (generate-new-buffer " *agent-runtime-block-parent*"))
         (agent-buffer (generate-new-buffer " *agent-runtime-block-child*"))
         (session (mevedel-session--create :name "main"))
         (agent (mevedel-agent-runtime-test--agent))
         (configuration
          (mevedel-agent-runtime-test--configuration agent))
         (submit-count 0)
         pending
         provider-prompt)
    (unwind-protect
        (with-current-buffer parent
          (setq-local mevedel--session session)
          (mevedel-recovery-report session "agent:explorer--test" 'request "Old failure" nil)
          (mevedel-recovery-report session "agent:other" 'request "Other failure" nil)
          (let ((mevedel-user-prompt-submit-functions
                 (list
                  (lambda (_event)
                    (cl-incf submit-count)
                    (if (= submit-count 1)
                        '(:continue nil
                          :additional-context ("carry this context"))
                      '(:updated-input "Rewritten accepted task"
                        :additional-context ("current context")))))))
            (cl-letf
                (((symbol-function 'mevedel-agent-conversation-save)
                  (lambda (&rest _) t))
                 ((symbol-function 'mevedel-recovery-save) #'ignore)
                 ((symbol-function 'mevedel-agent-exec-run)
                  (lambda (_callback _role _description invocation buffer)
                    (with-current-buffer buffer
                      (setq provider-prompt (buffer-string)))
                    (setf (mevedel-agent-invocation-runtime-fsm invocation)
                          'provider-fsm)
                    'provider-fsm)))
              (should-error
               (mevedel-agent-runtime-dispatch
                nil "Continue" "Blocked task"
                :path "/root/explore"
                :frozen-configuration configuration
                :retained-id "explorer--test"
                :retained-buffer agent-buffer
                :retained-transcript "agents/explorer.chat.org"
                :on-hook-context (lambda (entries) (setq pending entries))))
              (should (= 2 (length (mevedel-session-recovery-issues session))))
              (with-current-buffer agent-buffer
                (should-not (string-match-p "Blocked task" (buffer-string))))
              (should pending)
              (mevedel-agent-runtime-dispatch
               nil "Continue" "Accepted task"
               :path "/root/explore"
               :frozen-configuration configuration
               :retained-id "explorer--test"
               :retained-buffer agent-buffer
               :retained-transcript "agents/explorer.chat.org"
               :pending-hook-context pending
               :on-hook-context (lambda (entries) (setq pending entries)))
              (should (equal '("agent:other")
                             (mapcar (lambda (issue) (plist-get issue :id))
                                     (mevedel-session-recovery-issues session))))
              (should-not pending)
              (should (string-match-p "Rewritten accepted task"
                                      provider-prompt))
              (should (string-match-p "carry this context" provider-prompt))
              (should (string-match-p "current context" provider-prompt))
              (with-current-buffer agent-buffer
                (should (string-match-p "Accepted task" (buffer-string)))))))
      (kill-buffer agent-buffer)
      (kill-buffer parent)))

  :doc "keeps pending context when an accepted retained turn cannot be saved"
  (let* ((parent (generate-new-buffer " *agent-runtime-save-parent*"))
         (agent-buffer (generate-new-buffer " *agent-runtime-save-child*"))
         (session (mevedel-session--create :name "main"))
         (agent (mevedel-agent-runtime-test--agent))
         (configuration
          (mevedel-agent-runtime-test--configuration agent))
         (pending '((:event UserPromptSubmit
                     :source "hook"
                     :body "keep this context")))
         (transitions 0)
         provider-called)
    (unwind-protect
        (with-current-buffer parent
          (setq-local mevedel--session session)
          (cl-letf (((symbol-function 'mevedel-agent-conversation-save)
                     (lambda (&rest _) nil))
                    ((symbol-function 'mevedel-agent-exec-run)
                     (lambda (&rest _)
                       (setq provider-called t)
                       'provider-fsm)))
            (should-error
             (mevedel-agent-runtime-dispatch
              nil "Continue" "Accepted but unsaved task"
              :path "/root/explore"
              :frozen-configuration configuration
              :retained-id "explorer--test"
              :retained-buffer agent-buffer
              :retained-transcript "agents/explorer.chat.org"
              :pending-hook-context pending
              :on-hook-context
              (lambda (_entries) (cl-incf transitions))))
            (should-not provider-called)
            (should (= 0 transitions))
            (with-current-buffer agent-buffer
              (should-not
               (string-match-p "Accepted but unsaved task"
                               (buffer-string))))))
      (kill-buffer agent-buffer)
      (kill-buffer parent))))


;;
;;; Specialist catalog inheritance

(defvar test-mevedel-agent-runtime--mcp-identity nil
  "Registered (CATEGORY NAME) of the fixture's wrapped MCP tool.")

(defun test-mevedel-agent-runtime--catalog-identities ()
  "Return the delegator's specialists.
They are communication, built-in, configured and MCP tools; the MCP
identity is the one its wrapper registered."
  (list '("mevedel" "SendMessage") '("mevedel" "Imenu")
        '("user-tools" "ConfiguredProbe")
        test-mevedel-agent-runtime--mcp-identity))

(defun test-mevedel-agent-runtime--mcp-name ()
  "Return the ToolCall name of the fixture's MCP tool."
  (mevedel-tool-call-name
   (mevedel-tool-get (cadr test-mevedel-agent-runtime--mcp-identity)
                     (car test-mevedel-agent-runtime--mcp-identity))))

(defun test-mevedel-agent-runtime--catalog-names ()
  "Return the ToolCall names of the delegator's specialists."
  (list "SendMessage" "Imenu" "user-tools/ConfiguredProbe"
        (test-mevedel-agent-runtime--mcp-name)))

(defvar test-mevedel-agent-runtime--backend nil
  "Registered provider backend of the specialist catalog fixture.")

(defvar test-mevedel-agent-runtime--buffers nil
  "Agent buffers the specialist catalog fixture must kill.")

(defun test-mevedel-agent-runtime--register-catalog-tools ()
  "Register the delegator fixture's direct and specialist tools."
  (dolist (name '("Read" "ToolSearch" "ToolCall"
                  "SendMessage" "ListAgents" "Imenu"))
    (mevedel-tool-ensure name))
  ;; A user-configured tool registered in its own category.
  (mevedel-tool-register
   (mevedel-tool--create
    :name "ConfiguredProbe" :category "user-tools"
    :summary "Configured probe" :description "Configured probe contract"
    :args '((value string :required "Probe value"))
    :handler (lambda (callback _args) (funcall callback "probe"))))
  ;; An MCP tool arrives as a gptel tool in its server's category and is
  ;; registered through the same wrapper MCP configuration uses.
  (let ((wrapped
         (mevedel-tool--register-wrap
          :source (gptel-make-tool
                   :name "search_issues" :category "probe-mcp"
                   :function (lambda (&rest _) "issues")
                   :description "Search issues on the probe MCP server"
                   :args (list '(:name "query" :type string
                                 :description "Issue query")))
          :summary "Search probe issues" :read-only-p t)))
    (setq test-mevedel-agent-runtime--mcp-identity
          (list (mevedel-tool-category wrapped) (mevedel-tool-name wrapped)))))

(defmacro test-mevedel-agent-runtime--with-delegator (identities &rest body)
  "Run BODY in a root session buffer whose specialist catalog is IDENTITIES.
The root's direct tools are Read, ToolSearch and ToolCall.  Tool, backend
and buffer state are confined to BODY."
  (declare (indent 1))
  `(let* ((mevedel-tool--registry (copy-hash-table mevedel-tool--registry))
          (gptel--known-tools (copy-tree gptel--known-tools))
          (gptel--known-backends (copy-alist gptel--known-backends))
          (test-mevedel-agent-runtime--backend
           (gptel--make-backend :name "Agent Catalog Test"))
          (test-mevedel-agent-runtime--buffers nil)
          (test-mevedel-agent-runtime--mcp-identity nil)
          (root (generate-new-buffer " *catalog-root*")))
     (unwind-protect
         (progn
           (setf (gptel-get-backend "Agent Catalog Test")
                 test-mevedel-agent-runtime--backend)
           (test-mevedel-agent-runtime--register-catalog-tools)
           (with-current-buffer root
             (setq-local mevedel--session
                         (mevedel-session--create
                          :name "main"
                          :tool-catalog
                          (mapcar (lambda (identity)
                                    (cons (copy-sequence identity)
                                          (format "%s summary" (cadr identity))))
                                  ,identities)))
             (setq-local gptel-tools
                         (mapcar (lambda (name)
                                   (mevedel-tool-gptel-tool (mevedel-tool-get name)))
                                 '("Read" "ToolSearch" "ToolCall")))
             ,@body))
       (dolist (buffer (cons root test-mevedel-agent-runtime--buffers))
         (when (buffer-live-p buffer)
           (with-current-buffer buffer (set-buffer-modified-p nil))
           (kill-buffer buffer))))))

(defun test-mevedel-agent-runtime--spawn (agent path &optional configuration)
  "Dispatch AGENT at PATH from the current buffer and return its buffer.
With CONFIGURATION, continue a retained conversation from that frozen
configuration instead, as a follow-up or a cold restore does.  Only
transcript storage and the provider are stand-ins: invocation creation,
freezing and buffer configuration are the real ones."
  (let ((session mevedel--session)
        (child (and configuration
                    (generate-new-buffer " *catalog-retained*"))))
    (when child
      (push child test-mevedel-agent-runtime--buffers)
      (with-current-buffer child (setq-local mevedel--session session)))
    (cl-letf (((symbol-function 'mevedel-agent-conversation-open)
               (lambda (&rest _)
                 (setq child (generate-new-buffer " *catalog-child*"))
                 (push child test-mevedel-agent-runtime--buffers)
                 (with-current-buffer child (setq-local mevedel--session session))
                 child))
              ((symbol-function 'mevedel-agent-runtime--setup-transcript)
               (lambda (invocation buffer)
                 (setf (mevedel-agent-invocation-transcript-relative-path
                        invocation)
                       "agents/catalog.chat.org")
                 (with-current-buffer buffer
                   (setq-local mevedel--agent-invocation invocation))))
              ((symbol-function 'mevedel-agent-conversation-save)
               (lambda (&rest _) t))
              ((symbol-function 'mevedel-agent-exec-run)
               (lambda (&rest _) 'provider-fsm))
              ((symbol-function 'mevedel-agent-runtime--execution-live-p)
               #'ignore)
              ((symbol-function 'mevedel-view-agent-live-transcript-start)
               #'ignore)
              ((symbol-function 'mevedel-telemetry-record) #'ignore))
      (if configuration
          (mevedel-agent-runtime-dispatch
           nil "Continue" "Continue the task."
           :path path :frozen-configuration configuration
           :retained-id (format "retained--%s" path)
           :retained-buffer child
           :retained-transcript "agents/catalog.chat.org")
        (mevedel-agent-runtime-dispatch
         agent "Catalog task" "Report your tools."
         :path path :prepared-turn '(:prompt "Report your tools.")
         :model-policy (list :backend test-mevedel-agent-runtime--backend
                             :model 'catalog-test-model :effort nil))))
    child))

(defun test-mevedel-agent-runtime--discovery (buffer names)
  "Return what BUFFER's agent finds for NAMES through ToolSearch and ToolCall.
Each element is (NAME SEARCH-RESULT IN-TOOLCALL-ROSTER-P)."
  (with-current-buffer buffer
    (let (roster)
      (cl-letf (((symbol-function 'mevedel-ptc-driver-run)
                 (lambda (_callback _expression names &optional _standalone)
                   (setq roster names))))
        (mevedel-tool-ptc--handler #'ignore '(:expression "nil")))
      (mapcar (lambda (name)
                (let (result)
                  (mevedel-tools--tool-search
                   (lambda (value) (setq result value)) name)
                  (list name result (and (member name roster) t))))
              names))))

(defun test-mevedel-agent-runtime--should-discover (buffer names)
  "Assert BUFFER's agent retrieves contracts for and can call every NAMES."
  (dolist (found (test-mevedel-agent-runtime--discovery buffer names))
    (ert-info ((format "discover %s" (car found)))
      (should (string-prefix-p (car found) (nth 1 found)))
      (should (string-search "Calling signature" (nth 1 found)))
      (should (nth 2 found)))))

(defun test-mevedel-agent-runtime--should-not-discover (buffer names)
  "Assert BUFFER's agent can neither find nor call any of NAMES."
  (dolist (found (test-mevedel-agent-runtime--discovery buffer names))
    (ert-info ((format "hidden %s" (car found)))
      (should (string-prefix-p "No matching tools found" (nth 1 found)))
      (should-not (nth 2 found)))))

(defun test-mevedel-agent-runtime--native-p (buffer name)
  "Return non-nil when BUFFER's agent offers NAME as a native tool."
  (cl-find name (buffer-local-value 'gptel-tools buffer)
           :key #'gptel-tool-name :test #'equal))

(defun test-mevedel-agent-runtime--named-role ()
  "Return a named role with its own direct and specialist tools."
  (mevedel-agent--create
   :name "catalog_named" :description "Named catalog role"
   :tools '((:tool "Read") (:discoverable (:tool "Imenu")))
   :system-prompt "Named role."))

(mevedel-deftest mevedel-agent-runtime-dispatch/specialist-catalog
  (:quiet t)
  ,test
  (test)
  :doc "a default agent finds and can call every delegator specialist"
  (test-mevedel-agent-runtime--with-delegator
      (test-mevedel-agent-runtime--catalog-identities)
    ;; The fixture is valid only if the delegator itself can use them all.
    (test-mevedel-agent-runtime--should-discover
     (current-buffer) (test-mevedel-agent-runtime--catalog-names))
    (let ((child (test-mevedel-agent-runtime--spawn
                  (mevedel-agent-default) "/root/helper")))
      (test-mevedel-agent-runtime--should-discover
       child (test-mevedel-agent-runtime--catalog-names))
      ;; A permitted SendMessage is native as well, so reporting needs no search.
      (should (test-mevedel-agent-runtime--native-p child "SendMessage"))
      ;; The inherited catalog is frozen with the agent, not read live.
      (push (cons (list "mevedel" "ListAgents") "late")
            (mevedel-session-tool-catalog mevedel--session))
      (test-mevedel-agent-runtime--should-not-discover child '("ListAgents"))))

  :doc "a default agent of a default agent inherits the same catalog"
  (test-mevedel-agent-runtime--with-delegator
      (test-mevedel-agent-runtime--catalog-identities)
    (let* ((child (test-mevedel-agent-runtime--spawn
                   (mevedel-agent-default) "/root/helper"))
           (grandchild (with-current-buffer child
                         (test-mevedel-agent-runtime--spawn
                          (mevedel-agent-default) "/root/helper/sub"))))
      (test-mevedel-agent-runtime--should-discover
       grandchild (test-mevedel-agent-runtime--catalog-names))))

  :doc "a retained follow-up and a cold restore keep the inherited catalog"
  (test-mevedel-agent-runtime--with-delegator
      (test-mevedel-agent-runtime--catalog-identities)
    (let* ((child (test-mevedel-agent-runtime--spawn
                   (mevedel-agent-default) "/root/helper"))
           (configuration
            (mevedel-agent-invocation-frozen-configuration
             (buffer-local-value 'mevedel--agent-invocation child)))
           (restored
            (mevedel-agent-persistence--decode-configuration
             (car (read-from-string
                   (prin1-to-string
                    (mevedel-agent-persistence--encode-configuration
                     configuration))))
             "default")))
      ;; Neither continuation may fall back to the delegator's live catalog.
      (setf (mevedel-session-tool-catalog mevedel--session) nil)
      ;; Nor may a specialist registered and catalogued after freezing appear.
      (mevedel-tool-register
       (mevedel-tool--create :name "LateProbe" :category "user-tools"
                             :summary "Late probe" :description "Late contract"))
      (push (cons (list "user-tools" "LateProbe") "late")
            (mevedel-session-tool-catalog mevedel--session))
      (test-mevedel-agent-runtime--should-discover
       (test-mevedel-agent-runtime--spawn nil "/root/helper" configuration)
       (test-mevedel-agent-runtime--catalog-names))
      (test-mevedel-agent-runtime--should-discover
       (test-mevedel-agent-runtime--spawn nil "/root/helper" restored)
       (test-mevedel-agent-runtime--catalog-names))
      (dolist (continued
               (list (test-mevedel-agent-runtime--spawn
                      nil "/root/helper" configuration)
                     (test-mevedel-agent-runtime--spawn
                      nil "/root/helper" restored)))
        (test-mevedel-agent-runtime--should-not-discover
         continued '("user-tools/LateProbe")))))

  :doc "a named role keeps its own specialists, not its delegator's"
  (test-mevedel-agent-runtime--with-delegator
      (test-mevedel-agent-runtime--catalog-identities)
    (let ((child (test-mevedel-agent-runtime--spawn
                  (test-mevedel-agent-runtime--named-role) "/root/named")))
      (should (test-mevedel-agent-runtime--native-p child "SendMessage"))
      (test-mevedel-agent-runtime--should-discover
       child '("ListAgents" "Imenu"))
      (test-mevedel-agent-runtime--should-not-discover
       child (list "user-tools/ConfiguredProbe"
                    (test-mevedel-agent-runtime--mcp-name)))))

  :doc "a default agent of a narrower delegator never gains the root catalog"
  (test-mevedel-agent-runtime--with-delegator
      (test-mevedel-agent-runtime--catalog-identities)
    (let* ((named (test-mevedel-agent-runtime--spawn
                   (test-mevedel-agent-runtime--named-role) "/root/named"))
           (grandchild (with-current-buffer named
                         (test-mevedel-agent-runtime--spawn
                          (mevedel-agent-default) "/root/named/helper"))))
      ;; The named delegator's native SendMessage is inherited natively.
      (should (test-mevedel-agent-runtime--native-p grandchild "SendMessage"))
      (test-mevedel-agent-runtime--should-discover
       grandchild '("ListAgents" "Imenu"))
      (test-mevedel-agent-runtime--should-not-discover
       grandchild (list "user-tools/ConfiguredProbe"
                    (test-mevedel-agent-runtime--mcp-name)))))

  :doc "a default agent inherits only what its delegator's request may call"
  (dolist (restricted '(some none))
    (ert-info ((format "request allows %s" restricted))
      (test-mevedel-agent-runtime--with-delegator
          (test-mevedel-agent-runtime--catalog-identities)
        (let ((allowed (and (eq restricted 'some)
                            (list "SendMessage"
                                  (test-mevedel-agent-runtime--mcp-name)))))
        ;; A skill's allowed-tools restricts the request, not the catalog.
        (setq-local mevedel--current-request
                    (mevedel-request--create :ptc-primitives allowed))
        (let ((child (test-mevedel-agent-runtime--spawn
                      (mevedel-agent-default) "/root/helper")))
          (test-mevedel-agent-runtime--should-discover child allowed)
          ;; A restricted request grants no native SendMessage either.
          (should (eq (eq restricted 'some)
                      (and (test-mevedel-agent-runtime--native-p child "SendMessage")
                           t)))
          (test-mevedel-agent-runtime--should-not-discover
           child (cl-set-difference (test-mevedel-agent-runtime--catalog-names)
                                    allowed :test #'equal)))))))

  :doc "a default agent inherits its delegator's catalog, not a bound sibling's"
  ;; Asynchronous hook callbacks restore the delegator's buffer while another
  ;; agent's engine may still be dynamically bound.
  (dolist (shape '(empty-sibling broader-sibling))
    (ert-info ((format "%s" shape))
      (test-mevedel-agent-runtime--with-delegator
          (if (eq shape 'empty-sibling)
              (test-mevedel-agent-runtime--catalog-identities)
            (list '("mevedel" "SendMessage")))
        (let* ((sibling
                (mevedel-agent-invocation--create
                 :path "/root/sibling"
                 :tool-catalog
                 (and (eq shape 'broader-sibling)
                      (mapcar (lambda (identity) (cons identity "sibling"))
                              (cons '("mevedel" "ListAgents")
                                    (test-mevedel-agent-runtime--catalog-identities))))))
               (child
                (let ((mevedel-tools--current-engine
                       (gptel-make-fsm
                        :info (list :mevedel-agent-invocation sibling))))
                  (test-mevedel-agent-runtime--spawn
                   (mevedel-agent-default) "/root/helper"))))
          (if (eq shape 'empty-sibling)
              (test-mevedel-agent-runtime--should-discover
               child (test-mevedel-agent-runtime--catalog-names))
            (test-mevedel-agent-runtime--should-discover child '("SendMessage"))
            (test-mevedel-agent-runtime--should-not-discover
             child (cons "ListAgents"
                         (cdr (test-mevedel-agent-runtime--catalog-names)))))))))

  :doc "an agent delegator's own catalog and request decide, not a bound sibling's"
  (test-mevedel-agent-runtime--with-delegator
      (test-mevedel-agent-runtime--catalog-identities)
    (let* ((child (test-mevedel-agent-runtime--spawn
                   (mevedel-agent-default) "/root/helper"))
           (allowed (list "SendMessage" (test-mevedel-agent-runtime--mcp-name)))
           (sibling
            (mevedel-agent-invocation--create
             :path "/root/sibling"
             :tool-catalog
             (mapcar (lambda (identity) (cons identity "sibling"))
                     (cons '("mevedel" "ListAgents")
                           (test-mevedel-agent-runtime--catalog-identities)))))
           (grandchild
            (with-current-buffer child
              ;; The delegating agent's own request restricts what it may call.
              ;; It also allows ListAgents, which only the sibling has, so a
              ;; sibling leak would show.
              (setq-local mevedel--current-request
                          (mevedel-request--create
                           :ptc-primitives (cons "ListAgents" allowed)))
              (let ((mevedel-tools--current-engine
                     (gptel-make-fsm
                      :info (list :mevedel-agent-invocation sibling))))
                (test-mevedel-agent-runtime--spawn
                 (mevedel-agent-default) "/root/helper/sub")))))
      (test-mevedel-agent-runtime--should-discover grandchild allowed)
      (test-mevedel-agent-runtime--should-not-discover
       grandchild '("ListAgents" "Imenu" "user-tools/ConfiguredProbe"))))

  :doc "a default agent of an empty delegator stays empty as the root grows"
  (test-mevedel-agent-runtime--with-delegator nil
    (let ((child (test-mevedel-agent-runtime--spawn
                  (mevedel-agent-default) "/root/helper")))
      (setf (mevedel-session-tool-catalog mevedel--session)
            (mapcar (lambda (identity) (cons identity "late"))
                    (test-mevedel-agent-runtime--catalog-identities)))
      (let ((grandchild (with-current-buffer child
                          (test-mevedel-agent-runtime--spawn
                           (mevedel-agent-default) "/root/helper/sub"))))
        (dolist (buffer (list child grandchild))
          (test-mevedel-agent-runtime--should-not-discover
           buffer (cons "ListAgents"
                        (test-mevedel-agent-runtime--catalog-names))))))))

(mevedel-deftest mevedel-agent-runtime-queue-execution-completion
  ()
  ,test
  (test)
  :doc "keeps the terminal answer separate from Bash output in either arrival order"
  (dolist (response-first-p '(nil t))
    (let* ((session (mevedel-session--create :authority-mode 'pid-lock))
           (invocation (mevedel-agent-runtime-test--invocation))
           (record (mevedel-agent-record--create
                    :path "/root/explore" :parent-path "/root"
                    :activity 'running :invocation invocation))
           (live-p t)
           finalizations)
      (setf (mevedel-session-agent-registry session)
            (list (cons "/root/explore" record))
            (mevedel-agent-invocation-parent-session invocation) session
            (mevedel-agent-invocation-runtime-settle-callback invocation)
            (lambda (invocation response event &optional token)
              (mevedel-agent-control--settle
               session record invocation response event token)))
      (cl-letf (((symbol-function 'mevedel-agent-runtime--execution-live-p)
                 (lambda (_invocation) live-p))
                ((symbol-function 'mevedel-agent-runtime--finalize)
                 (lambda (_invocation status &optional _token)
                   (push status finalizations))))
        (when response-first-p
          (mevedel-agent-runtime--handle-provider-result invocation "Agent answer")
          (should (equal "Agent answer"
                         (mevedel-agent-invocation-runtime-pending-response
                          invocation))))
        (should-not
         (mevedel-agent-runtime-queue-execution-completion
          invocation "/root/other" "wrong owner"))
        (should
         (mevedel-agent-runtime-queue-execution-completion
          invocation "/root/explore" "First Bash exited with code 0"))
        (should-not (mevedel-session-messages session))
        ;; A still-working agent reads its own completion first.
        (should (equal (if response-first-p nil '(EXECUTION))
                       (mapcar (lambda (msg) (plist-get msg :type))
                               (mevedel-agent-record-mailbox record))))
        (should-not finalizations)
        (setq live-p nil)
        (should
         (mevedel-agent-runtime-queue-execution-completion
          invocation "/root/explore" "Second Bash exited with code 1"))
        (unless response-first-p
          (should-not (mevedel-session-messages session))
          (mevedel-agent-runtime--handle-provider-result invocation "Agent answer"))
        (let ((messages (mevedel-agent-control-context-mailbox session)))
          (should (equal '(RESULT EXECUTION EXECUTION)
                         (mapcar (lambda (msg) (plist-get msg :type)) messages)))
          (should (equal '("Agent answer" "First Bash exited with code 0"
                           "Second Bash exited with code 1")
                         (mapcar (lambda (msg) (plist-get msg :payload)) messages))))
        (should (equal "Agent answer" (mevedel-agent-record-settled-result record)))
        (should-not (mevedel-agent-record-mailbox record))
        (should (equal '(completed) finalizations))
        (should-not
         (mevedel-agent-runtime-queue-execution-completion
          invocation "/root/explore" "duplicate"))))))

(mevedel-deftest mevedel-agent-runtime-queue-execution-completion/meta
  (:doc "forwards structural metadata to the working agent's mailbox record")
  (let* ((session (mevedel-session--create :authority-mode 'pid-lock))
         (invocation (mevedel-agent-runtime-test--invocation))
         (record (mevedel-agent-record--create
                  :path "/root/explore" :parent-path "/root"
                  :activity 'running :invocation invocation)))
    (setf (mevedel-session-agent-registry session)
          (list (cons "/root/explore" record))
          (mevedel-agent-invocation-parent-session invocation) session)
    (should (mevedel-agent-runtime-queue-execution-completion
             invocation "/root/explore" "done"
             '(:execution-id "e1" :termination "exited")))
    (should (equal '(:execution-id "e1" :termination "exited")
                   (plist-get (car (mevedel-agent-record-mailbox record))
                              :meta)))
    ;; A held response keeps the metadata for settlement.
    (setf (mevedel-agent-invocation-runtime-pending-response invocation) "ans")
    (cl-letf (((symbol-function 'mevedel-agent-runtime--execution-live-p)
               (lambda (_) t)))
      (mevedel-agent-runtime-queue-execution-completion
       invocation "/root/explore" "held" '(:execution-id "e2")))
    (should (equal '(("held" :execution-id "e2"))
                   (mevedel-agent-invocation-runtime-execution-results
                    invocation)))))

(mevedel-deftest mevedel-agent-runtime-queue-execution-completion/read
  (:doc "does not forward a completion the working agent already read")
  (let* ((session (mevedel-session--create :authority-mode 'pid-lock))
         (invocation (mevedel-agent-runtime-test--invocation))
         (record (mevedel-agent-record--create
                  :path "/root/explore" :parent-path "/root"
                  :activity 'running :invocation invocation)))
    (setf (mevedel-session-agent-registry session)
          (list (cons "/root/explore" record))
          (mevedel-agent-invocation-parent-session invocation) session
          (mevedel-agent-invocation-runtime-settle-callback invocation)
          (lambda (invocation response event &optional token)
            (mevedel-agent-control--settle
             session record invocation response event token)))
    (cl-letf (((symbol-function 'mevedel-agent-runtime--execution-live-p)
               (lambda (_invocation) nil))
              ((symbol-function 'mevedel-agent-runtime--finalize) #'ignore))
      (mevedel-agent-runtime-queue-execution-completion
       invocation "/root/explore" "Probe output")
      ;; The agent's next WAIT delivers and clears its mailbox.
      (should (mevedel-agent-control-context-mailbox invocation))
      (mevedel-agent-control-clear-context-mailbox invocation)
      (mevedel-agent-runtime--handle-provider-result invocation "Agent answer"))
    (should (equal '(RESULT)
                   (mapcar (lambda (msg) (plist-get msg :type))
                           (mevedel-agent-control-context-mailbox session))))))

(mevedel-deftest mevedel-agent-runtime-queue-execution-completion/acknowledgement
  (:quiet t)
  ,test
  (test)
  :doc "a rejected completion acknowledges failure once and returns nil"
  (let ((invocation (mevedel-agent-runtime-test--invocation))
        acks)
    (should-not
     (mevedel-agent-runtime-queue-execution-completion
      invocation "/root/other" "wrong owner" nil
      (lambda (success error) (push (list success error) acks))))
    (should (equal '((nil "Execution owner no longer accepts results")) acks)))
  :doc "a held response acknowledges success after the body joins its results"
  (let ((invocation (mevedel-agent-runtime-test--invocation))
        acks)
    (setf (mevedel-agent-invocation-runtime-pending-response invocation) "ans")
    (cl-letf (((symbol-function 'mevedel-agent-runtime--execution-live-p)
               (lambda (_) t)))
      (should
       (eq t (mevedel-agent-runtime-queue-execution-completion
              invocation (mevedel-agent-invocation-require-path invocation)
              "held" nil
              (lambda (success error)
                (push (list success error
                            (length (mevedel-agent-invocation-runtime-execution-results
                                     invocation)))
                      acks))))))
    (should (equal '((t nil 1)) acks)))
  :doc "the mailbox path forwards the acknowledgement and its pending marker"
  (dolist (marker '(pending t))
    (let* ((session (mevedel-session--create :authority-mode 'pid-lock))
           (invocation (mevedel-agent-runtime-test--invocation))
           (path (mevedel-agent-invocation-require-path invocation))
           (ack (lambda (&rest _) nil))
           captured)
      (setf (mevedel-agent-invocation-parent-session invocation) session)
      (cl-letf (((symbol-function 'mevedel-agent-control-enqueue-execution-result)
                 (lambda (&rest args) (setq captured args) marker)))
        (should (eq marker
                    (mevedel-agent-runtime-queue-execution-completion
                     invocation path "body" '(:execution-id "e1") ack))))
      (should (equal (list session path "body" nil nil '(:execution-id "e1"))
                     (seq-take captured 6)))
      (should (eq ack (nth 6 captured)))))
  :doc "the last held completion waits for durable RESULT and transcript, including rollback retry"
  (dolist (failure-mode '(nil before-commit after-commit))
    (test-mevedel-publication-async--fixture
     (lambda (session parent)
       (let* ((child (generate-new-buffer " *held-completion*"))
              (invocation (mevedel-agent-runtime-test--invocation child))
              (record (mevedel-agent-record--create
                       :id "explorer--test" :role "explorer"
                       :path "/root/explore" :parent-path "/root"
                       :activity 'running :invocation invocation))
              (publisher (symbol-function
                          'mevedel-session-artifacts-publish-agent-terminal-state))
              (deliver (symbol-function 'mevedel-agent-control--publish-result))
              (fail (eq failure-mode 'before-commit))
              (fail-delivery (eq failure-mode 'after-commit))
              acks publication-called durable)
         (setf (mevedel-agent-invocation-parent-session invocation) session
               (mevedel-agent-invocation-parent-data-buffer invocation) parent
               (mevedel-agent-invocation-runtime-pending-response invocation) "answer"
               (mevedel-session-agent-registry session)
               (list (cons "/root/explore" record))
               (mevedel-agent-invocation-runtime-settle-callback invocation)
               (lambda (inv response event token)
                 (mevedel-agent-control--settle session record inv response event token)))
         (setf (mevedel-agent-invocation-frozen-configuration invocation)
               (with-current-buffer parent
                 (mevedel-agent-exec-freeze-configuration
                  "explorer" invocation (list :backend gptel-backend :model gptel-model)))
               (mevedel-agent-record-configuration record)
               (mevedel-agent-invocation-frozen-configuration invocation))
         (with-current-buffer child
           (org-mode)
           (setq-local mevedel--session session mevedel--agent-invocation invocation)
           (mevedel-agent-runtime--setup-transcript invocation child)
           (insert "* Agent Task: explore\n\nHeld durable response\n"))
         (setf (mevedel-agent-record-conversation-location record)
               (mevedel-agent-invocation-transcript-relative-path invocation))
         ;; A real accepted preparation holds the gate before any publication
         ;; or terminal-state capture; no fabricated token opens the gate.
         (setf (mevedel-session-publication-preparations session)
               (list (cons #'ignore nil)))
         (cl-letf (((symbol-function 'mevedel-agent-runtime--execution-live-p)
                    (lambda (_) nil))
                   ((symbol-function 'mevedel-session-artifacts-publish-agent-terminal-state)
                    (lambda (&rest args)
                      (setq publication-called t)
                      (should-not acks)
                      (when fail (error "Injected deferred settlement failure"))
                      (prog1 (apply publisher args) (setq durable t))))
                   ((symbol-function 'mevedel-agent-control--publish-result)
                    (lambda (&rest args)
                      (when fail-delivery (error "Injected postcommit notification failure"))
                      (apply deliver args))))
           (let ((ack (lambda (success error)
                        (when success
                          (should durable)
                          (should
                           (equal '(RESULT EXECUTION)
                                  (mapcar (lambda (mail) (plist-get mail :type))
                                          (test-mevedel-publication-async--restored-mail session))))
                          (should
                           (string-match-p
                            "Held durable response"
                            (mevedel-session-artifacts-read-artifact
                             session (mevedel-agent-invocation-transcript-relative-path invocation)
                             t))))
                        (push (list success error) acks))))
             (should
              (eq 'pending
                  (mevedel-agent-runtime-queue-execution-completion
                   invocation "/root/explore" "execution output" '(:execution-id "held") ack)))
             (should-not acks)
             (should-not publication-called)
             (should-not (mevedel-agent-invocation-runtime-settled-p invocation))
             (should-not (test-mevedel-publication-async--restored-mail session))
             (mevedel-session-publication-barrier session)
             (should publication-called)
             (should (= 1 (length acks)))
             (when (eq failure-mode 'before-commit)
               (should (equal '((nil "Injected deferred settlement failure")) acks))
               (should-not (mevedel-agent-invocation-runtime-settled-p invocation))
               (should-not (mevedel-agent-invocation-runtime-execution-results invocation))
               (should-not (test-mevedel-publication-async--mail session))
               (should-not (test-mevedel-publication-async--restored-mail session))
               ;; The producer still owns the failed output.  Retry it through
               ;; the same sink and verify it appears exactly once on disk.
               (setq fail nil acks nil)
               (should
                (eq t (mevedel-agent-runtime-queue-execution-completion
                       invocation "/root/explore" "execution output"
                       '(:execution-id "held") ack))))
             (should (equal '((t nil)) acks))
             (when fail-delivery
               ;; Adoption already secured the result despite the notification
               ;; error. Retrying cleanup must neither reconsume nor republish it.
               (should-not (mevedel-agent-invocation-runtime-settled-p invocation))
               (setq fail-delivery nil)
               (mevedel-agent-runtime--settle invocation "answer"))
             (should (mevedel-agent-invocation-runtime-settled-p invocation))
             (mevedel-session-publication-barrier session)
             (should (equal '((t nil)) acks))))))))
  :doc "a rejected deferred preparation acknowledges failure once even if teardown repeats it"
  (let* ((session (mevedel-session--create :authority-mode 'pid-lock))
         (invocation (mevedel-agent-runtime-test--invocation))
         failure acks)
    (setf (mevedel-agent-invocation-parent-session invocation) session
          (mevedel-agent-invocation-runtime-pending-response invocation) "answer")
    (cl-letf (((symbol-function 'mevedel-agent-runtime--execution-live-p)
               (lambda (_) nil))
              ((symbol-function 'mevedel-session-publication-defer)
               (lambda (_session _preparation &optional handler _token)
                 (setq failure handler) t)))
      (should (eq 'pending
                  (mevedel-agent-runtime-queue-execution-completion
                   invocation "/root/explore" "retained output" nil
                   (lambda (success error) (push (list success error) acks)))))
      (should-not acks)
      (funcall failure '(error "Teardown rejected settlement"))
      (funcall failure '(error "Teardown rejected settlement")))
    (should (equal '((nil "Teardown rejected settlement")) acks))
    (should-not (mevedel-agent-invocation-runtime-execution-results invocation))
    (should-not (mevedel-agent-invocation-runtime-settled-p invocation)))
  :doc "synchronous settlement errors and quits acknowledge failure once before propagating"
  (dolist (condition '(error quit))
    (let ((invocation (mevedel-agent-runtime-test--invocation))
          (siblings (list (cons "older second" nil) (cons "older first" nil)))
          caught acks)
      (setf (mevedel-agent-invocation-runtime-pending-response invocation) "answer"
            (mevedel-agent-invocation-runtime-execution-results invocation) siblings)
      (cl-letf (((symbol-function 'mevedel-agent-runtime--execution-live-p)
                 (lambda (_) nil))
                ((symbol-function 'mevedel-agent-runtime--settle-1)
                 (lambda (&rest _) (signal condition '("Settlement failed")))))
        (condition-case err
            (mevedel-agent-runtime-queue-execution-completion
             invocation "/root/explore" "retained" nil
             (lambda (success error) (push (list success error) acks)))
          ((error quit) (setq caught err))))
      (should (eq condition (car caught)))
      (should (= 1 (length acks)))
      (should-not (caar acks))
      (should (stringp (cadar acks)))
      (should (equal siblings (mevedel-agent-invocation-runtime-execution-results invocation)))
      (should-not (mevedel-agent-invocation-runtime-settled-p invocation))))
  :doc "a real yielded producer survives failed held settlement and retries once with its unread spool"
  (test-mevedel-publication-async--fixture
   (lambda (session parent)
     (let* ((child (generate-new-buffer " *held-real-producer*"))
            (invocation (mevedel-agent-runtime-test--invocation child))
            (record (mevedel-agent-record--create
                     :id "explorer--test" :role "explorer" :path "/root/explore"
                     :parent-path "/root" :activity 'running :invocation invocation))
            (root (mevedel-workspace-root (mevedel-session-workspace session)))
            (go (file-name-concat root "go"))
            (mevedel-sandbox-mode 'off)
            (publisher (symbol-function 'mevedel-session-artifacts-publish-agent-terminal-state))
            (fail t)
            acks execution id)
       (unwind-protect
           (progn
             (setf (mevedel-agent-invocation-parent-session invocation) session
                   (mevedel-agent-invocation-parent-data-buffer invocation) parent
                   (mevedel-agent-invocation-runtime-pending-response invocation) "answer"
                   (mevedel-session-agent-registry session) (list (cons "/root/explore" record))
                   (mevedel-agent-invocation-runtime-settle-callback invocation)
                   (lambda (inv response event token)
                     (mevedel-agent-control--settle session record inv response event token))
                   (mevedel-agent-invocation-frozen-configuration invocation)
                   (with-current-buffer parent
                     (mevedel-agent-exec-freeze-configuration
                      "explorer" invocation (list :backend gptel-backend :model gptel-model)))
                   (mevedel-agent-record-configuration record)
                   (mevedel-agent-invocation-frozen-configuration invocation))
             (with-current-buffer child
               (org-mode)
               (setq-local mevedel--session session mevedel--agent-invocation invocation)
               (mevedel-agent-runtime--setup-transcript invocation child)
               (insert "* Agent Task: explore\n\nHeld producer transcript\n"))
             (setf (mevedel-agent-record-conversation-location record)
                   (mevedel-agent-invocation-transcript-relative-path invocation))
             (cl-letf (((symbol-function 'mevedel-session-artifacts-publish-agent-terminal-state)
                        (lambda (&rest args)
                          (when fail (error "Injected terminal publication failure"))
                          (apply publisher args))))
               (let ((mevedel-execution-mailbox-delivery-function
                      (lambda (event context ack)
                        (mevedel-tool-exec-handle-execution-event
                         event context
                         (lambda (success error)
                           (push (list success error) acks)
                           (funcall ack success error))))))
                 (let ((initial
                        (test-mevedel-execution--start-managed
                         session root
                         (list "sh" "-c"
                               "while [ ! -e \"$1\" ]; do sleep .02; done; printf RETAIN-ME"
                               "wait" go)
                         :owner "/root/explore" :owner-context invocation)))
                   (setq id (plist-get (plist-get initial :facts) :execution-id)
                         execution (mevedel-execution--owned-yielded-record
                                    session "/root/explore" id)))
                 (write-region "go" nil go nil 'silent)
                 (test-mevedel-execution--wait (lambda () acks))
                 (should (equal '((nil "Injected terminal publication failure")) acks))
                 (should (eq execution (mevedel-execution--owned-yielded-record
                                        session "/root/explore" id)))
                 (should-not (mevedel-execution--record-delivery-state execution))
                 (should-not (mevedel-execution--record-delivery-pending-p execution))
                 (should-not (mevedel-execution--record-retire-timer execution))
                 (should (file-exists-p (mevedel-execution--spool-path execution)))
                 (should (equal "RETAIN-ME" (mevedel-execution--read-output execution)))
                 (should (= 0 (mevedel-execution--record-read-offset execution)))
                 (should-not (test-mevedel-publication-async--restored-mail session))
                 (setq fail nil)
                 (mevedel-execution--deliver-independent execution)
                 (test-mevedel-execution--wait (lambda () (= 2 (length acks))))
                 (should (equal '((t nil) (nil "Injected terminal publication failure")) acks))
                 (should (timerp (mevedel-execution--record-retire-timer execution)))
                 (let ((mail (test-mevedel-publication-async--restored-mail session)))
                   (should (equal '(RESULT EXECUTION)
                                  (mapcar (lambda (entry) (plist-get entry :type)) mail)))
                   (should (string-match-p "RETAIN-ME" (plist-get (cadr mail) :payload))))
                 (should (string-match-p
                          "RETAIN-ME"
                          (plist-get (test-mevedel-execution--observe
                                      session id :owner "/root/explore" :wait-ms 0) :output)))
                 (mevedel-execution--deliver-independent execution)
                 (should (= 2 (length acks))))))
         (mevedel-execution-teardown-session session))))))

(defun test-mevedel-agent-runtime--terminal-settlement-events (artifact-p)
  "Return a local portable session's terminal persistence events.
ARTIFACT-P selects whether its sidecar counts as committed."
  (let* ((save-path
          (file-name-as-directory
           (make-temp-file "mevedel-agent-terminal-" t)))
         (parent (generate-new-buffer " *agent-terminal-parent*"))
         (child (generate-new-buffer " *agent-terminal-child*"))
         (target 'local-target)
         (session
          (mevedel-session--create
           :name "main" :execution-target target :save-path save-path
           :authority-mode 'portable))
         (invocation (mevedel-agent-runtime-test--invocation child))
         events)
    (unwind-protect
        (progn
          ;; Keep the materialized cache opposite to the authoritative
          ;; publication membership so this test detects fixed-path reads.
          (unless artifact-p
            (write-region
             "(:poisoned-cache t)" nil
             (file-name-concat save-path "session.meta.el") nil 'silent))
          (setf (mevedel-agent-invocation-parent-session invocation) session
                (mevedel-agent-invocation-parent-data-buffer invocation) parent
                (mevedel-agent-invocation-transcript-relative-path invocation)
                "agents/explorer.chat.org"
                (mevedel-agent-invocation-runtime-settle-callback invocation)
                (lambda (&rest _)
                  (push (list 'callback
                              mevedel-agent-control-suppress-persistence)
                        events)
                  (when mevedel-agent-control-suppress-persistence
                    (cons (lambda () (push 'rollback events))
                          (lambda () (push 'delivery events))))))
          (cl-letf (((symbol-function
                      'mevedel-session-artifacts-artifact-present-p)
                     (lambda (seen-session logical)
                       (push (list 'presence logical) events)
                       (and (eq seen-session session)
                            (equal logical "session.meta.el")
                            artifact-p)))
                    ((symbol-function 'mevedel-agent-runtime--finalize)
                     (lambda (inv status &optional _token)
                       (push
                        (list 'finalize
                              mevedel-agent-runtime--defer-terminal-publication-p)
                        events)
                       (setf (mevedel-agent-invocation-transcript-status inv)
                             status)))
                    ((symbol-function
                      'mevedel-session-artifacts-publish-agent-terminal-state)
                     (lambda (&rest _)
                       (push 'publication events)
                       t))
                    ((symbol-function
                      'mevedel-agent-control-commit-session)
                     (lambda (_session &optional _token)
                       (push 'commit events)
                       t)))
            (mevedel-agent-runtime--settle invocation "finished"))
          (nreverse events))
      (kill-buffer child)
      (kill-buffer parent)
      (delete-directory save-path t))))

(mevedel-deftest mevedel-agent-runtime--setup-transcript
  (:doc "allocates transcript names from authoritative artifact membership")
  (let* ((save-path
          (file-name-as-directory
           (make-temp-file "mevedel-agent-allocation-" t)))
         (parent (generate-new-buffer " *agent-allocation-parent*"))
         (child (generate-new-buffer " *agent-allocation-child*"))
         (session (mevedel-session--create :name "main" :save-path save-path))
         (invocation (mevedel-agent-runtime-test--invocation child))
         (first "agents/explorer--2026-01-02T03-04-05--test.chat.org")
         (second "agents/explorer--2026-01-02T03-04-05--test-2.chat.org")
         (second-fixed (expand-file-name second save-path))
         checked recorded)
    (unwind-protect
        (progn
          (make-directory (file-name-directory second-fixed) t)
          ;; The first fixed cache is absent and the second is poisoned: only
          ;; artifact membership may decide which logical name is free.
          (write-region "poisoned cache" nil second-fixed nil 'silent)
          (setf (mevedel-agent-invocation-parent-session invocation) session
                (mevedel-agent-invocation-parent-data-buffer invocation) parent
                (mevedel-agent-invocation-parent-turn invocation) 3)
          (with-current-buffer parent
            (setq-local mevedel-session--read-only-mode nil))
          (cl-letf (((symbol-function 'format-time-string)
                     (lambda (&rest _) "2026-01-02T03-04-05"))
                    ((symbol-function
                      'mevedel-session-persistence-shallow-ensure-files)
                     (lambda (&rest _) save-path))
                    ((symbol-function
                      'mevedel-session-artifacts-artifact-present-p)
                     (lambda (seen-session logical)
                       (should (eq seen-session session))
                       (push logical checked)
                       (equal logical first)))
                    ((symbol-function
                      'mevedel-session-persistence-record-running-transcript)
                     (lambda (_session entry) (setq recorded entry))))
            (mevedel-agent-runtime--setup-transcript invocation child))
          (should (equal second
                         (mevedel-agent-invocation-transcript-relative-path
                          invocation)))
          (should (equal second (plist-get (cdr recorded) :path)))
          (should (equal (list first second) (nreverse checked)))
          (with-current-buffer child
            (should (equal second-fixed buffer-file-name))
            (should-not (equal "poisoned cache" (buffer-string)))))
      (dolist (buffer (list child parent))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer (set-buffer-modified-p nil))
          (kill-buffer buffer)))
      (delete-directory save-path t))))

(mevedel-deftest mevedel-agent-runtime--release-settled-state ()
  ,test
  (test)
  :doc "drops the finished request payload but spares a newer turn's buffer state"
  (let ((buffer (generate-new-buffer " *agent-runtime-release*"))
        (old (mevedel-agent-runtime-test--invocation))
        (new (mevedel-agent-runtime-test--invocation)))
    (unwind-protect
        (progn
          (setf (mevedel-agent-invocation-buffer old) buffer
                (mevedel-agent-invocation-buffer new) buffer)
          (with-current-buffer buffer
            (setq-local mevedel--agent-invocation old)
            (setq-local gptel--fsm-last
                        (gptel-make-fsm :info (list :data 'old-payload))))
          (mevedel-agent-runtime--release-settled-state old)
          (with-current-buffer buffer
            (should-not (plist-get (gptel-fsm-info gptel--fsm-last) :data))
            ;; A retried settlement of OLD after NEW started there.
            (setq-local mevedel--agent-invocation new)
            (setq-local gptel--fsm-last
                        (gptel-make-fsm :info (list :data 'new-payload))))
          (mevedel-agent-runtime--release-settled-state old)
          (with-current-buffer buffer
            (should (eq 'new-payload
                        (plist-get (gptel-fsm-info gptel--fsm-last) :data)))))
      (kill-buffer buffer))))

(mevedel-deftest mevedel-agent-runtime--settle (:quiet t)
  ,test
  (test)
  :doc "successful retained-idle settlement closes owned tasks before parent RESULT"
  (let* ((session (mevedel-session--create :authority-mode 'pid-lock))
         (invocation (mevedel-agent-runtime-test--invocation))
         (record (mevedel-agent-record--create
                  :path "/root/explore" :parent-path "/root"
                  :activity 'running :invocation invocation)))
    (setf (mevedel-session-agent-registry session)
          (list (cons "/root/explore" record))
          (mevedel-agent-invocation-parent-session invocation) session
          (mevedel-agent-invocation-runtime-settle-callback invocation)
          (lambda (inv response event &optional token)
            (mevedel-agent-control--settle session record inv response event token)))
    (with-temp-buffer
      (setq-local mevedel--session session)
      (let ((mevedel--agent-invocation invocation))
        (mevedel-tool-task--handle-write
         '(:tasks [(:subject "Investigation" :status "in_progress")
                   (:subject "Verification")]
           :note "Checking")))
      (mevedel-tool-task--handle-write
       '(:tasks [(:subject "Parent follow-up" :blockedBy [1 2])]))
      ;; Exercise the real runtime and control settlement, not a direct call
      ;; to task finalization.  Retained idle is the public completion state.
      (should (equal "Done" (mevedel-agent-runtime--settle invocation "Done")))
      (should (eq 'idle (mevedel-agent-record-activity record)))
      (should-not (mevedel-agent-record-invocation record))
      (should (mevedel-agent-invocation-runtime-settled-p invocation))
      (should (equal '(completed completed pending)
                     (mapcar #'mevedel-task-status (mevedel-session-tasks session))))
      (should-not (mevedel-task-blocked-by (nth 2 (mevedel-session-tasks session))))
      (should-not (mevedel-session-task-status-notes session))
      (let ((message (car (mevedel-agent-control-context-mailbox session))))
        (should (eq 'RESULT (plist-get message :type)))
        (should (equal "/root/explore" (plist-get message :sender)))
        (should (equal "Done" (plist-get message :payload))))))

  :doc "failed or interrupted retained-idle settlement keeps owned work recoverable"
  (dolist (status '(error aborted))
    (let* ((session (mevedel-session--create :authority-mode 'pid-lock))
           (invocation (mevedel-agent-runtime-test--invocation))
           (record (mevedel-agent-record--create
                    :path "/root/explore" :parent-path "/root"
                    :activity 'running :invocation invocation)))
      (setf (mevedel-session-agent-registry session)
            (list (cons "/root/explore" record))
            (mevedel-agent-invocation-parent-session invocation) session
            (mevedel-agent-invocation-runtime-settle-callback invocation)
            (lambda (inv response event &optional token)
              (mevedel-agent-control--settle session record inv response event token)))
      (with-temp-buffer
        (setq-local mevedel--session session)
        (let ((mevedel--agent-invocation invocation))
          (mevedel-tool-task--handle-write
           '(:tasks [(:subject "Unfinished investigation" :status "in_progress")]
             :note "Still checking")))
        (mevedel-agent-runtime--settle
         invocation nil (list :mevedel-agent-terminal-status status
                              :error-details "Injected failure"))
        (should (eq 'idle (mevedel-agent-record-activity record)))
        (should (eq 'in-progress
                    (mevedel-task-status (car (mevedel-session-tasks session)))))
        (should (equal "Still checking"
                       (mevedel-tool-task--status-note session "/root/explore")))
        (let ((message (car (mevedel-agent-control-context-mailbox session))))
          (should (eq 'RESULT (plist-get message :type)))
          (should (equal "/root/explore" (plist-get message :sender)))
          (should (eq (if (eq status 'error) 'errored 'interrupted)
                      (plist-get message :outcome)))))))

  :doc "batches only materialized portable terminal state after registry update"
  (should
   (equal '((presence "session.meta.el")
            (finalize t) (callback t) publication delivery)
          (test-mevedel-agent-runtime--terminal-settlement-events t)))
  (should
   (equal '((presence "session.meta.el")
            (finalize nil) (callback t) commit delivery)
          (test-mevedel-agent-runtime--terminal-settlement-events nil)))

  :doc "a quit during portable publication rolls staged control state back"
  (let* ((session (mevedel-session--create
                   :name "main" :save-path temporary-file-directory
                   :execution-target 'local :authority-mode 'portable))
         (invocation (mevedel-agent-runtime-test--invocation))
         events)
    (setf (mevedel-agent-invocation-parent-session invocation) session
          (mevedel-agent-invocation-parent-data-buffer invocation)
          (current-buffer)
          (mevedel-agent-invocation-buffer invocation) (current-buffer)
          (mevedel-agent-invocation-transcript-relative-path invocation)
          "agents/test.chat.org"
          (mevedel-agent-invocation-runtime-settle-callback invocation)
          (lambda (&rest _)
            (cons (lambda () (push 'rollback events))
                  (lambda () (push 'delivery events)))))
    (cl-letf (((symbol-function
                'mevedel-session-artifacts-artifact-present-p)
               (lambda (&rest _) t))
              ((symbol-function 'mevedel-agent-runtime--finalize) #'ignore)
              ((symbol-function
                'mevedel-session-artifacts-publish-agent-terminal-state)
               (lambda (&rest _) (signal 'quit nil)))
              ((symbol-function 'mevedel-agent-control-settlement-finished)
               (lambda (&rest _) (push 'finished events))))
      (should (equal '(quit)
                     (condition-case err
                         (progn (mevedel-agent-runtime--settle invocation "done")
                                nil)
                       (quit err))))
      (should (equal '(rollback finished) (nreverse events)))))

  :doc "portable publication failure rolls staged control state back"
  (let* ((session (mevedel-session--create
                   :name "main" :save-path temporary-file-directory
                   :execution-target 'local :authority-mode 'portable))
         (invocation (mevedel-agent-runtime-test--invocation))
         events)
    (setf (mevedel-agent-invocation-parent-session invocation) session
          (mevedel-agent-invocation-parent-data-buffer invocation)
          (current-buffer)
          (mevedel-agent-invocation-buffer invocation) (current-buffer)
          (mevedel-agent-invocation-transcript-relative-path invocation)
          "agents/test.chat.org"
          (mevedel-agent-invocation-runtime-settle-callback invocation)
          (lambda (&rest _)
            (push 'callback events)
            (cons (lambda () (push 'rollback events))
                  (lambda () (push 'delivery events)))))
    (cl-letf (((symbol-function
                'mevedel-session-artifacts-artifact-present-p)
               (lambda (&rest _) t))
              ((symbol-function 'mevedel-agent-runtime--finalize)
               (lambda (&rest _) (push 'finalize events)))
              ((symbol-function
                'mevedel-session-artifacts-publish-agent-terminal-state)
               (lambda (&rest _)
                 (push 'publication events)
                 (error "Commit unavailable")))
              ((symbol-function 'mevedel-agent-control-settlement-finished)
               (lambda (seen-session)
                 (should (eq session seen-session))
                 (push 'finished events))))
      (should-error
       (mevedel-agent-runtime--settle invocation "finished"))
      ;; Even a failed settlement stops blocking new turns in the tree.
      (should (equal '(finalize callback publication rollback finished)
                     (nreverse events)))
      (should-not
       (mevedel-agent-invocation-runtime-settled-p invocation))
      (should
       (mevedel-agent-invocation-runtime-pending-response invocation))))

  :doc "a nested settlement of the same turn leaves the report to the outer call"
  (let* ((session (mevedel-session--create :name "main"))
         (invocation (mevedel-agent-runtime-test--invocation))
         (finished nil)
         (nested nil))
    (setf (mevedel-agent-invocation-parent-session invocation) session
          (mevedel-agent-invocation-runtime-settle-callback invocation)
          (lambda (&rest _)
            (unless nested
              (setq nested t)
              ;; A terminal timer fires while the outer call is staging.
              (should (memq invocation mevedel-agent-control-settling))
              (mevedel-agent-runtime--settle invocation "again")
              (should-not finished))
            nil))
    (cl-letf (((symbol-function 'mevedel-agent-runtime--finalize) #'ignore)
              ((symbol-function 'mevedel-agent-control-settlement-finished)
               (lambda (_session) (push t finished))))
      (mevedel-agent-runtime--settle invocation "finished"))
    (should nested)
    (should-not mevedel-agent-control-settling)
    (should (equal '(t) finished)))

  :doc "portable delivery retries without repeating its committed publication"
  (let* ((session (mevedel-session--create
                   :name "main" :save-path temporary-file-directory
                   :execution-target 'local :authority-mode 'portable))
         (invocation (mevedel-agent-runtime-test--invocation))
         (callbacks 0)
         (publications 0)
         (deliveries 0))
    (setf (mevedel-agent-invocation-parent-session invocation) session
          (mevedel-agent-invocation-parent-data-buffer invocation)
          (current-buffer)
          (mevedel-agent-invocation-buffer invocation) (current-buffer)
          (mevedel-agent-invocation-transcript-relative-path invocation)
          "agents/test.chat.org"
          (mevedel-agent-invocation-runtime-settle-callback invocation)
          (lambda (&rest _)
            (cl-incf callbacks)
            (cons #'ignore
                  (lambda ()
                    (cl-incf deliveries)
                    (when (= deliveries 1)
                      (error "Delivery unavailable"))))))
    (cl-letf (((symbol-function
                'mevedel-session-artifacts-artifact-present-p)
               (lambda (&rest _) t))
              ((symbol-function 'mevedel-agent-runtime--finalize) #'ignore)
              ((symbol-function
                'mevedel-session-artifacts-publish-agent-terminal-state)
               (lambda (&rest _)
                 (cl-incf publications)
                 t)))
      (should-error
       (mevedel-agent-runtime--settle invocation "finished"))
      (should-not
       (mevedel-agent-invocation-runtime-settled-p invocation))
      (should
       (eq 'committed
           (plist-get
            (mevedel-agent-invocation-runtime-pending-response invocation)
            :phase)))
      (should
       (equal "finished"
              (mevedel-agent-runtime--settle
               invocation "ignored duplicate"
               '(:mevedel-agent-terminal-status error))))
      (should
       (mevedel-agent-invocation-runtime-settled-p invocation))
      (should (= 1 callbacks))
      (should (= 1 publications))
      (should (= 2 deliveries))))

  :doc "releases the settled buffer's request payload and decoding memo"
  (with-temp-buffer
    (let* ((session (mevedel-session--create :name "main"))
           (invocation (mevedel-agent-runtime-test--invocation (current-buffer)))
           (marker (point-marker)))
      (setq-local gptel--fsm-last
                  (gptel-make-fsm :info (list :data '(:messages ["long"])
                                              :position marker)))
      (setq-local mevedel-transcript-audit--buffer-records
                  (make-hash-table :test #'equal))
      (setq-local mevedel-transcript-audit--buffer-record-bytes 42)
      (setf (mevedel-agent-invocation-parent-session invocation) session
            (mevedel-agent-invocation-parent-data-buffer invocation)
            (current-buffer)
            (mevedel-agent-invocation-runtime-settle-callback invocation)
            (lambda (&rest _) nil))
      (setq-local mevedel--agent-invocation invocation)
      (cl-letf (((symbol-function 'mevedel-agent-runtime--finalize) #'ignore))
        (should (equal "finished"
                       (mevedel-agent-runtime--settle invocation "finished"))))
      (should (mevedel-agent-invocation-runtime-settled-p invocation))
      ;; Markers a continuation reads survive; the payload does not.
      (should-not (plist-get (gptel-fsm-info gptel--fsm-last) :data))
      (should (eq marker (plist-get (gptel-fsm-info gptel--fsm-last) :position)))
      (should-not mevedel-transcript-audit--buffer-records)
      (should (= 0 mevedel-transcript-audit--buffer-record-bytes)))))

(mevedel-deftest mevedel-agent-runtime--transcript-path
  (:doc "returns a published remote transcript's qualified logical path")
  (let* ((save-path "/ssh:fixture:/srv/project/.mevedel/sessions/main/")
         (session (mevedel-session--create :name "main" :save-path save-path))
         (invocation (mevedel-agent-runtime-test--invocation))
         seen)
    (setf (mevedel-agent-invocation-parent-session invocation) session
          (mevedel-agent-invocation-transcript-relative-path invocation)
          "agents/explorer.chat.org")
    (cl-letf (((symbol-function
                'mevedel-session-artifacts-artifact-present-p)
               (lambda (seen-session logical)
                 (setq seen (list seen-session logical))
                 t)))
      (should
       (equal (concat save-path "agents/explorer.chat.org")
              (mevedel-agent-runtime--transcript-path invocation)))
      (should (equal (list session "agents/explorer.chat.org") seen))
      (cl-letf (((symbol-function
                  'mevedel-session-artifacts-artifact-present-p)
                 (lambda (&rest _) nil)))
        (should-not
         (mevedel-agent-runtime--transcript-path invocation))))))

(mevedel-deftest mevedel-agent-runtime-bound-turn ()
  ,test
  (test)
  :doc "cancels the deadline when a bounded turn completes in time"
  (let* ((invocation (mevedel-agent-runtime-test--invocation))
         timer-callback
         timer-args
         cancelled)
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (_seconds _repeat callback &rest args)
                 (setq timer-callback callback
                       timer-args args)
                 'budget-timer))
              ((symbol-function 'cancel-timer)
               (lambda (timer) (setq cancelled timer)))
              ((symbol-function 'mevedel-agent-runtime--finalize) #'ignore))
      (mevedel-agent-runtime-bound-turn invocation 30)
      (mevedel-agent-runtime--settle invocation "done")
      (should (eq 'budget-timer cancelled))
      (should-not
       (mevedel-agent-invocation-runtime-budget-timer invocation))
      (apply timer-callback timer-args)
      (should
       (mevedel-agent-invocation-runtime-settled-p invocation))))

  :doc "budget expiry interrupts once and preserves useful partial output"
  (let* ((buffer (generate-new-buffer " *agent-runtime-budget*"))
         (invocation (mevedel-agent-runtime-test--invocation buffer))
         (fsm (gptel-make-fsm
               :state 'WAIT :info (list :buffer buffer :callback #'ignore)))
         (gptel--request-alist
          (list (cons 'process (cons fsm #'ignore))))
         timer-callback
         timer-args
         callback-response
         abort-count)
    (unwind-protect
        (progn
          (setf (mevedel-agent-invocation-runtime-fsm invocation) fsm
                (mevedel-agent-invocation-runtime-settle-callback invocation)
                (lambda (_invocation response _event &optional _token)
                  (setq callback-response response)))
          (cl-letf (((symbol-function 'run-at-time)
                     (lambda (_seconds _repeat callback &rest args)
                       (setq timer-callback callback timer-args args)
                       'budget-timer))
                    ((symbol-function 'mevedel-agent-runtime--partial-text)
                     (lambda (&rest _) "bounded useful evidence"))
                    ((symbol-function 'mevedel-agent-runtime--finalize)
                     (lambda (inv status &optional _token)
                       (setf (mevedel-agent-invocation-transcript-status inv)
                             status)))
                    ((symbol-function 'gptel-abort)
                     (lambda (_buffer)
                       (setq abort-count (1+ (or abort-count 0))))))
            (mevedel-agent-runtime-bound-turn invocation 20)
            (apply timer-callback timer-args))
          (should (= 1 abort-count))
          (should (string-match-p "budget expired after 20 seconds"
                                  callback-response))
          (should (string-match-p "bounded useful evidence"
                                  callback-response))
          (should (eq 'aborted
                      (mevedel-agent-invocation-transcript-status invocation)))
          (apply timer-callback timer-args)
          (should (= 1 abort-count)))
      (kill-buffer buffer))))

(mevedel-deftest mevedel-agent-runtime--finalize ()
  ,test
  (test)
  :doc "request teardown failure leaves the turn active and retryable"
  (let* ((child (generate-new-buffer " *agent-finalize-child*"))
         (parent (generate-new-buffer " *agent-finalize-parent*"))
         (session (mevedel-session--create :name "main"))
         (invocation (mevedel-agent-runtime-test--invocation child))
         calls
         warnings)
    (unwind-protect
        (progn
          (setf (mevedel-agent-invocation-parent-session invocation) session
                (mevedel-agent-invocation-parent-data-buffer invocation) parent
                (mevedel-agent-invocation-activity invocation) 'working)
          (cl-letf (((symbol-function 'mevedel-request-end)
                     (lambda (&rest _) (error "Injected request failure")))
                    ((symbol-function 'mevedel-execution-stop-owner)
                     (lambda (&rest _) (push 'executions calls)))
                    ((symbol-function
                      'mevedel-session-persistence-update-transcript-entry)
                     (lambda (&rest _) (push 'transcript calls)))
                    ((symbol-function
                      'mevedel-agent-conversation-save)
                     (lambda (&rest _) (push 'save calls)))
                    ((symbol-function 'mevedel-agent-conversation-record-activity)
                     (lambda (&rest _) (push 'activity calls)))
                    ((symbol-function 'mevedel-tool-task-finalize-owner)
                     (lambda (&rest _) (push 'tasks calls) t))
                    ((symbol-function 'mevedel-tool-task-refresh-display)
                     (lambda () (push 'task-view calls)))
                    ((symbol-function 'mevedel-agent-conversation-refresh)
                     (lambda (&rest _) (push 'handle calls)))
                    ((symbol-function
                      'mevedel-session-persistence-write-sidecar-now)
                     (lambda (&rest _) (push 'sidecar calls)))
                    ((symbol-function 'mevedel-agent-runtime--run-stop-hook)
                     (lambda (&rest _) (push 'hook calls)))
                    ((symbol-function
                      'mevedel-view-agent-live-transcript-finalize)
                     (lambda (&rest _) (push 'view calls)))
                    ((symbol-function 'display-warning)
                     (lambda (&rest args) (push args warnings))))
            (should-error
             (mevedel-agent-runtime--finalize invocation 'completed)))
          (should-not calls)
          (should-not warnings)
          (should (eq 'working
                      (mevedel-agent-invocation-activity invocation)))
          (should (eq 'running
                      (mevedel-agent-invocation-transcript-status invocation))))
      (kill-buffer child)
      (kill-buffer parent)))

  :doc "execution teardown failure cannot publish later terminal effects"
  (let* ((child (generate-new-buffer " *agent-finalize-execution-child*"))
         (session (mevedel-session--create :name "main"))
         (invocation (mevedel-agent-runtime-test--invocation child))
         calls)
    (unwind-protect
        (progn
          (setf (mevedel-agent-invocation-parent-session invocation) session
                (mevedel-agent-invocation-activity invocation) 'working)
          (cl-letf (((symbol-function 'mevedel-request-end)
                     (lambda (&rest _) (push 'request calls)))
                    ((symbol-function 'mevedel-execution-stop-owner)
                     (lambda (&rest _)
                       (error "Injected execution failure")))
                    ((symbol-function
                      'mevedel-session-persistence-update-transcript-entry)
                     (lambda (&rest _) (push 'transcript calls))))
            (should-error
             (mevedel-agent-runtime--finalize invocation 'completed)))
          (should (equal '(request) calls))
          (should (eq 'running
                      (mevedel-agent-invocation-transcript-status invocation))))
      (kill-buffer child))))

(mevedel-deftest mevedel-agent-runtime-interrupt
  ()
  ,test
  (test)
  :doc "aborts a live provider turn and settles useful partial output once"
  (let* ((buffer (generate-new-buffer " *agent-runtime-interrupt*"))
         (invocation (mevedel-agent-runtime-test--invocation buffer))
         (provider-callback #'identity)
         (fsm (gptel-make-fsm
               :state 'WAIT
               :info (list :buffer buffer :callback provider-callback)))
         (gptel--request-alist
          (list (cons 'process (cons fsm #'ignore))))
         callback-count
         callback-response
         abort-count)
    (unwind-protect
        (progn
          (setf (mevedel-agent-invocation-runtime-fsm invocation) fsm
                (mevedel-agent-invocation-runtime-settle-callback invocation)
                (lambda (_invocation response _event &optional _token)
                  (setq callback-count (1+ (or callback-count 0))
                        callback-response response)))
          (cl-letf
              (((symbol-function 'mevedel-agent-runtime--partial-text)
                (lambda (&rest _) "useful partial work"))
               ((symbol-function 'mevedel-agent-runtime--finalize)
                (lambda (inv status &optional _token)
                  (setf (mevedel-agent-invocation-transcript-status inv)
                        status)))
               ((symbol-function 'gptel-abort)
                (lambda (_buffer)
                  (setq abort-count (1+ (or abort-count 0)))
                  (should
                   (eq #'ignore
                       (plist-get (gptel-fsm-info fsm) :callback))))))
            (let ((response
                   (mevedel-agent-runtime-interrupt
                    invocation "interrupted by /root")))
              (should (string-match-p "interrupted by /root" response))
              (should (string-match-p "useful partial work" response))
              (should (string-match-p "reconcile current state" response)))
            (should (= 1 abort-count))
            (should (= 1 callback-count))
            (should (string-match-p "useful partial work" callback-response))
            (should (eq provider-callback
                        (plist-get (gptel-fsm-info fsm) :callback)))
            (should (eq 'aborted
                        (mevedel-agent-invocation-transcript-status
                         invocation)))
            (mevedel-agent-runtime-interrupt invocation "duplicate")
            (should (= 1 callback-count))))
      (kill-buffer buffer)))

  :doc "lets a native cancellation acknowledgement own asynchronous settlement"
  (let* ((buffer (generate-new-buffer " *agent-runtime-deferred-abort*"))
         (invocation (mevedel-agent-runtime-test--invocation buffer))
         (cancel-count 0) settled)
    (unwind-protect
        (progn
          (setf (mevedel-agent-invocation-runtime-cancel invocation)
                (lambda () (cl-incf cancel-count) 'deferred))
          (cl-letf (((symbol-function 'mevedel-agent-runtime--settle)
                     (lambda (&rest _) (setq settled t))))
            (should (string-match-p "stop" (mevedel-agent-runtime-interrupt invocation "stop")))
            (mevedel-agent-runtime-interrupt invocation "stop again")
            (should (= 2 cancel-count))
            (should-not settled)
            (should-not (mevedel-agent-invocation-runtime-settled-p invocation))))
      (kill-buffer buffer)))

  :doc "mail queued before the interrupt no longer wakes; later mail does"
  (let* ((buffer (generate-new-buffer " *agent-runtime-interrupt-wakes*"))
         (invocation (mevedel-agent-runtime-test--invocation buffer))
         (session (mevedel-session--create :name "main"))
         (before (list :type 'MAIL :sender "/root" :payload "before" :wake t))
         (record (mevedel-agent-record--create
                  :path "/root/explore" :activity 'running
                  :invocation invocation :mailbox (list before))))
    (setf (mevedel-agent-invocation-parent-session invocation) session
          (mevedel-session-agent-registry session)
          (list (cons "/root/explore" record))
          (mevedel-agent-invocation-runtime-cancel invocation)
          (lambda () 'deferred))
    (unwind-protect
        (progn
          (mevedel-agent-runtime-interrupt invocation "stop")
          (should-not (plist-get before :wake))
          (let ((after (list :type 'MAIL :sender "/root" :payload "after"
                             :wake t)))
            (push after (mevedel-agent-record-mailbox record))
            ;; The cancellation acknowledgement settles later; mail sent
            ;; after the interrupt keeps its mark until then.
            (should (plist-get after :wake))))
      (kill-buffer buffer)))

  :doc "leaves a turn unsettled when provider abort fails"
  (let* ((buffer (generate-new-buffer " *agent-runtime-abort-fail*"))
         (invocation (mevedel-agent-runtime-test--invocation buffer))
         (fsm (gptel-make-fsm
               :state 'WAIT
               :info (list :buffer buffer :callback #'identity)))
         (gptel--request-alist
          (list (cons 'process (cons fsm #'ignore))))
         settled)
    (unwind-protect
        (progn
          (setf (mevedel-agent-invocation-runtime-fsm invocation) fsm
                (mevedel-agent-invocation-runtime-settle-callback invocation)
                (lambda (&rest _) (setq settled t)))
          (cl-letf (((symbol-function 'gptel-abort)
                     (lambda (_buffer) (error "Provider abort failed"))))
            (should
             (equal '(error "Provider abort failed")
                    (should-error
                     (mevedel-agent-runtime-interrupt invocation "stop")))))
          (should (eq #'identity (plist-get (gptel-fsm-info fsm) :callback)))
          (should-not settled)
          (should-not
           (mevedel-agent-invocation-runtime-settled-p invocation))
          (should-not
           (mevedel-agent-invocation-terminal-reason invocation)))
      (kill-buffer buffer)))

  :doc "settles active and queued parallel Bash exactly once on interruption"
  (let* ((root (make-temp-file "mevedel-agent-interrupt-bash-" t))
         (child (generate-new-buffer " *agent-interrupt-bash*"))
         (parent (generate-new-buffer " *agent-interrupt-parent*"))
         (session
          (mevedel-session-create
           "main"
           (mevedel-workspace--create
            :type 'file :id root :root root :name "interrupt")
           root))
         (invocation (mevedel-agent-runtime-test--invocation child))
         (active-path (file-name-concat root "active"))
         (queued-one-path (file-name-concat root "queued-one"))
         (queued-two-path (file-name-concat root "queued-two"))
         (artifact-directory (file-name-concat root "artifacts"))
         (mevedel-sandbox-mode 'off)
         callbacks
         settled)
    (unwind-protect
        (progn
          (setf (mevedel-agent-invocation-parent-session invocation) session
                (mevedel-agent-invocation-parent-data-buffer invocation) parent
                (mevedel-agent-invocation-runtime-settle-callback invocation)
                (lambda (&rest _)
                  (setq settled (1+ (or settled 0)))))
          (with-current-buffer child
            (setq-local mevedel--session session)
            (setq-local mevedel--agent-invocation invocation)
            (let ((request (mevedel-request-begin session)))
              (dolist (command
                       (list
                        (list "sh" "-c"
                              "touch \"$1\"; exec sleep 30"
                              "agent-active" active-path)
                        (list "sh" "-c" "touch \"$1\""
                              "agent-queued-one" queued-one-path)
                        (list "sh" "-c" "touch \"$1\""
                              "agent-queued-two" queued-two-path)))
                (let ((label (nth 3 command)))
                  (mevedel-execution-start-bash
                   (lambda (value) (push (cons label value) callbacks))
                   :session session :data-buffer child
                   :owner "/root/explore" :owner-context invocation
                   :request request :command command :workdir root
                   :writable-roots (list root)
                   :artifact-directory artifact-directory
                   :yield-time-ms nil)))))
          (with-timeout (5 (error "Timed out waiting for active Bash"))
            (while (not (file-exists-p active-path))
              (accept-process-output nil 0.01)))
          (cl-letf (((symbol-function
                      'mevedel-session-persistence-update-transcript-entry)
                     #'ignore)
                    ((symbol-function 'mevedel-agent-conversation-save)
                     #'ignore)
                    ((symbol-function
                      'mevedel-agent-conversation-record-activity)
                     #'ignore)
                    ((symbol-function 'mevedel-agent-conversation-refresh)
                     #'ignore)
                    ((symbol-function
                      'mevedel-session-persistence-write-sidecar-now)
                     #'ignore)
                    ((symbol-function 'mevedel-agent-runtime--run-stop-hook)
                     #'ignore))
            (mevedel-agent-runtime-interrupt invocation "stop batch"))
          (should (= 1 settled))
          (should (= 3 (length callbacks)))
          (should (= 3 (length (delete-dups (mapcar #'car callbacks)))))
          (dolist (entry callbacks)
            (let ((facts (plist-get (cdr entry) :facts)))
              (should (eq 'completed (plist-get facts :state)))
              (should (memq (plist-get facts :termination)
                            '(aborted owner-stopped)))
              (should (eq 'failure (plist-get facts :outcome)))))
          (should-not (file-exists-p queued-one-path))
          (should-not (file-exists-p queued-two-path))
          (should-not
           (mevedel-execution-owner-live-p session "/root/explore")))
      (mevedel-execution-teardown-session session)
      (when (buffer-live-p child) (kill-buffer child))
      (when (buffer-live-p parent) (kill-buffer parent))
      (delete-directory root t))))

(mevedel-deftest mevedel-agent-runtime--defer-settlement ()
  ,test
  (test)
  :doc "a held publication gate queues settlement and its run receives a fresh token"
  (let* ((session (mevedel-session--create :authority-mode 'pid-lock))
         (invocation (mevedel-agent-runtime-test--invocation))
         (buffer (generate-new-buffer " *settle-gate*"))
         queued settled gate-held)
    (setf (mevedel-agent-invocation-parent-session invocation) session)
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel-session-publication-defer)
                   (lambda (_session preparation &optional failure token)
                     (when (and gate-held (not token))
                       (setq queued (cons preparation failure))
                       t)))
                  ((symbol-function 'mevedel-agent-runtime--settle-1)
                   (lambda (_invocation response event token)
                     (push (list response event (current-buffer) token)
                           settled)))
                  ((symbol-function 'mevedel-agent-control-settlement-finished)
                   #'ignore))
          (setq gate-held t)
          (with-current-buffer buffer
            (mevedel-agent-runtime--settle invocation "answer" '(:event)))
          (should queued)
          (should-not settled)
          ;; The gate releases: the preparation settles in its own buffer
          ;; under the token it was handed, never one it held before.
          (setq gate-held nil)
          (funcall (car queued) 'fresh)
          (should (equal (list (list "answer" '(:event) buffer 'fresh))
                         settled)))
      (kill-buffer buffer)))
  :doc "an open gate settles in place and hands its token on"
  (let* ((session (mevedel-session--create :authority-mode 'pid-lock))
         (invocation (mevedel-agent-runtime-test--invocation))
         settled)
    (setf (mevedel-agent-invocation-parent-session invocation) session)
    (cl-letf (((symbol-function 'mevedel-session-publication-defer)
               (lambda (&rest _) nil))
              ((symbol-function 'mevedel-agent-runtime--settle-1)
               (lambda (_invocation response _event token)
                 (push (list response token) settled)))
              ((symbol-function 'mevedel-agent-control-settlement-finished)
               #'ignore))
      (mevedel-agent-runtime--settle invocation "answer" nil 'owned))
    (should (equal '(("answer" owned)) settled)))
  :doc "the token is forwarded to the publication owner's defer decision"
  (let* ((session (mevedel-session--create :authority-mode 'pid-lock))
         (invocation (mevedel-agent-runtime-test--invocation))
         seen)
    (setf (mevedel-agent-invocation-parent-session invocation) session)
    (cl-letf (((symbol-function 'mevedel-session-publication-defer)
               (lambda (_session _preparation &optional _failure token)
                 (setq seen token)
                 nil))
              ((symbol-function 'mevedel-agent-runtime--settle-1)
               #'ignore)
              ((symbol-function 'mevedel-agent-control-settlement-finished)
               #'ignore))
      (mevedel-agent-runtime--settle invocation "answer" nil 'owned))
    (should (eq 'owned seen)))
  :doc "a nested settlement of the same turn is never deferred"
  (let* ((session (mevedel-session--create :authority-mode 'pid-lock))
         (invocation (mevedel-agent-runtime-test--invocation))
         (mevedel-agent-control-settling (list invocation))
         settled)
    (setf (mevedel-agent-invocation-parent-session invocation) session)
    (cl-letf (((symbol-function 'mevedel-session-publication-defer)
               (lambda (&rest _) (error "Must not defer")))
              ((symbol-function 'mevedel-agent-runtime--settle-1)
               (lambda (_invocation response _event _token)
                 (push response settled))))
      (mevedel-agent-runtime--settle invocation "inner"))
    (should (equal '("inner") settled)))
  :doc "a deferred settlement reports no response to its caller"
  (let* ((session (mevedel-session--create :authority-mode 'pid-lock))
         (invocation (mevedel-agent-runtime-test--invocation)))
    (setf (mevedel-agent-invocation-parent-session invocation) session)
    (cl-letf (((symbol-function 'mevedel-session-publication-defer)
               (lambda (&rest _) t)))
      (should-not (mevedel-agent-runtime--settle invocation "answer"))))
  :doc "a queued failure is reported through a warning naming the agent"
  (let* ((session (mevedel-session--create :authority-mode 'pid-lock))
         (invocation (mevedel-agent-runtime-test--invocation))
         queued warning)
    (setf (mevedel-agent-invocation-parent-session invocation) session)
    (cl-letf (((symbol-function 'mevedel-session-publication-defer)
               (lambda (_session preparation &optional failure token)
                 (ignore token)
                 (setq queued (cons preparation failure))
                 t)))
      (mevedel-agent-runtime--settle invocation "answer"))
    (cl-letf (((symbol-function 'display-warning)
               (lambda (_type message &optional args) (ignore args) (setq warning message))))
      (funcall (cdr queued) '(error "Disk full")))
    (should (string-match-p "/root/explore.*Disk full" warning))
    (should-not (mevedel-agent-invocation-runtime-settled-p invocation)))
  :doc "raw deferred acknowledgements run once on precommit and postcommit errors"
  (dolist (committed '(nil t))
    (let* ((session (mevedel-session--create :authority-mode 'pid-lock))
           (invocation (mevedel-agent-runtime-test--invocation))
           queued acks)
      (setf (mevedel-agent-invocation-parent-session invocation) session)
      (mevedel-test--with-captured-diagnostics nil
        (cl-letf (((symbol-function 'mevedel-session-publication-defer)
                   (lambda (_session preparation &optional failure token)
                     (unless token (setq queued (cons preparation failure)) t)))
                  ((symbol-function 'mevedel-agent-runtime--settle-1)
                   (lambda (&rest _)
                     (when committed
                       (setf (mevedel-agent-invocation-runtime-pending-response invocation)
                             '(:phase committed)))
                     (error "Deferred step failed"))))
          (should
           (mevedel-agent-runtime--defer-settlement
            invocation "answer" nil nil
            (lambda (success error) (push (list success error) acks))))
          (should-not acks)
          (condition-case err
              (funcall (car queued) 'fresh)
            (error (funcall (cdr queued) err)))
          (funcall (cdr queued) '(error "Repeated teardown failure"))))
      (should (equal (if committed '((t nil)) '((nil "Deferred step failed"))) acks)))))

(mevedel-deftest mevedel-agent-runtime--settle-1/token ()
  ,test
  (test)
  :doc "the gate token reaches finalization, the settle callback and the control commit"
  (let* ((session (mevedel-session--create :authority-mode 'pid-lock))
         (invocation (mevedel-agent-runtime-test--invocation))
         seen)
    (setf (mevedel-agent-invocation-parent-session invocation) session
          (mevedel-agent-invocation-runtime-settle-callback invocation)
          (lambda (_invocation _visible _event token)
            (push (cons 'callback token) seen)
            (list 'transaction)))
    (cl-letf (((symbol-function 'mevedel-agent-runtime--finalize)
               (lambda (_invocation _status &optional token)
                 (push (cons 'finalize token) seen)))
              ((symbol-function 'mevedel-agent-control-commit-session)
               (lambda (_session &optional token) (push (cons 'commit token) seen))))
      (mevedel-agent-runtime--settle-1 invocation "answer" nil 'owned))
    (should (equal '((commit . owned) (callback . owned) (finalize . owned))
                   seen))))

(mevedel-deftest mevedel-agent-runtime-interrupt/gate ()
  ,test
  (test)
  :doc "a held gate defers abort and settlement together and still returns the response"
  (let* ((session (mevedel-session--create :authority-mode 'pid-lock))
         (invocation (mevedel-agent-runtime-test--invocation))
         queued log gate-held)
    (setf (mevedel-agent-invocation-parent-session invocation) session
          (mevedel-agent-invocation-runtime-cancel invocation)
          (lambda () (push 'cancel log) nil))
    (cl-letf (((symbol-function 'mevedel-session-publication-defer)
               (lambda (_session preparation &optional _failure token)
                 (when (and gate-held (not token))
                   (setq queued preparation)
                   t)))
              ((symbol-function 'mevedel-agent-control-clear-agent-wakes)
               (lambda (&rest _) (push 'clear log)))
              ((symbol-function 'mevedel-agent-runtime--settle)
               (lambda (_invocation _response _event token)
                 (push (cons 'settle token) log)
                 "settled"))
              ((symbol-function 'mevedel-agent-control-settlement-finished)
               #'ignore))
      (setq gate-held t)
      (let ((result (mevedel-agent-runtime-interrupt invocation "stop")))
        (should (stringp result))
        (should (string-match-p "stop" result)))
      ;; Nothing was aborted, cleared or settled while the gate is held.
      (should-not log)
      (should-not (mevedel-agent-invocation-terminal-reason invocation))
      (setq gate-held nil)
      ;; The queued run re-enters with its own token and hands it on.
      (funcall queued 'fresh)
      (should (equal '((settle . fresh) cancel clear) log))
      (should (equal "stop"
                     (mevedel-agent-invocation-terminal-reason invocation)))))
  :doc "an open gate interrupts in place and returns the settled response"
  (let* ((session (mevedel-session--create :authority-mode 'pid-lock))
         (invocation (mevedel-agent-runtime-test--invocation)))
    (setf (mevedel-agent-invocation-parent-session invocation) session)
    (cl-letf (((symbol-function 'mevedel-session-publication-defer)
               (lambda (&rest _) nil))
              ((symbol-function 'mevedel-agent-control-clear-agent-wakes)
               #'ignore)
              ((symbol-function 'mevedel-agent-runtime--settle)
               (lambda (&rest _) "settled")))
      (should (equal "settled"
                     (mevedel-agent-runtime-interrupt invocation "stop"))))))

(defun test-mevedel-agent-runtime--dispatch-with-token (exec-run on-settle)
  "Dispatch a retained turn under gate token `owned' using EXEC-RUN.
ON-SETTLE, when non-nil, is the settle callback.  Return the recorded
calls as (SAVES FINALIZES SETTLES) token lists, after the dispatch error."
  (let* ((parent (generate-new-buffer " *agent-runtime-token-parent*"))
         (agent-buffer (generate-new-buffer " *agent-runtime-token-child*"))
         (session (mevedel-session--create :name "main"))
         (configuration
          (mevedel-agent-runtime-test--configuration
           (mevedel-agent-runtime-test--agent)))
         saves finalizes settles)
    (unwind-protect
        (with-current-buffer parent
          (setq-local mevedel--session session)
          (cl-letf (((symbol-function 'mevedel-agent-conversation-save)
                     (lambda (_invocation &optional _deferred token)
                       (push token saves)
                       t))
                    ((symbol-function 'mevedel-agent-exec-run) exec-run)
                    ((symbol-function 'mevedel-agent-runtime--finalize)
                     (lambda (_invocation _status &optional token)
                       (push token finalizes)))
                    ((symbol-function 'mevedel-agent-runtime--settle)
                     (lambda (_invocation _response &optional _event token)
                       (push token settles)
                       nil))
                    ((symbol-function 'mevedel-telemetry-record)
                     (lambda (_session event &rest _)
                       (when (eq event 'agent-request-sent)
                         (error "Injected publication failure")))))
            (should-error
             (apply #'mevedel-agent-runtime-dispatch
                    nil "Continue" "Token task"
                    :path "/root/explore"
                    :frozen-configuration configuration
                    :retained-id "explorer--test"
                    :retained-buffer agent-buffer
                    :retained-transcript "agents/explorer.chat.org"
                    :token 'owned
                    (and on-settle (list :on-settle on-settle))))))
      (kill-buffer agent-buffer)
      (kill-buffer parent))
    (list saves finalizes settles)))

(mevedel-deftest mevedel-agent-runtime-dispatch/token ()
  ,test
  (test)
  :doc "an unpublished error carries the token through the save and the cleanup"
  (let (cancelled)
    (pcase-let
        ((`(,saves ,finalizes ,settles)
          (test-mevedel-agent-runtime--dispatch-with-token
           (lambda (_callback _type _description invocation _buffer)
             (setf (mevedel-agent-invocation-runtime-cancel invocation)
                   (lambda () (setq cancelled t)))
             nil)
           nil)))
      (should cancelled)
      (should (equal '(owned) saves))
      (should (equal '(owned) finalizes))
      (should-not settles)))
  :doc "a published error settles under the same token"
  (pcase-let
      ((`(,saves ,finalizes ,settles)
        (test-mevedel-agent-runtime--dispatch-with-token
         (lambda (&rest _) 'fsm)
         (lambda (&rest _) nil))))
    (should (equal '(owned) saves))
    (should-not finalizes)
    (should (equal '(owned) settles))))

(provide 'test-mevedel-agent-runtime)

;;; test-mevedel-agent-runtime.el ends here
