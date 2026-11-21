;;; test-niwa-agent-runtime.el --- Retained agent runner tests -*- lexical-binding: t -*-

;;; Commentary:

;; Focused Agent V2 provider-boundary, settlement, interruption, and yielded
;; execution tests.

;;; Code:

(require 'gptel-request)
(require 'niwa-agent-control)
(require 'niwa-agent-exec)
(require 'niwa-agent-persistence)
(require 'niwa-claude-code-backend)
(require 'niwa-agent-conversation)
(require 'niwa-agents)
(require 'niwa-reminders)
(require 'niwa-agent-runtime)
(require 'niwa-execution)
(require 'niwa-permission-queue)
(require 'niwa-pipeline)
(require 'niwa-sandbox)
(require 'niwa-session-artifacts)
(require 'niwa-session-control-transfer)
(require 'niwa-session-persistence)
(require 'niwa-structs)
(require 'niwa-tool-task)
(require 'niwa-tool-permission)
(require 'niwa-tool-ptc)
(require 'niwa-tools)
(require 'niwa-turn)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'niwa-session-publication-test-helpers
         (file-name-concat (file-name-directory (or load-file-name buffer-file-name))
                           "niwa-session-publication-test-helpers"))
(require 'niwa-execution-test-helpers
         (file-name-concat (file-name-directory (or load-file-name buffer-file-name))
                           "niwa-execution-test-helpers"))

(defun niwa-agent-runtime-test--agent ()
  "Return a small frozen test agent."
  (niwa-agent--create
   :name "explorer"
   :description "Explore"
   :tools nil
   :system-prompt "Explore the workspace."
   :frozen-p t))

(defun niwa-agent-runtime-test--configuration (agent)
  "Return a frozen request configuration for AGENT."
  (niwa-agent-configuration--create
   :agent agent
   :request-locals nil))

(defun niwa-agent-runtime-test--invocation (&optional buffer)
  "Return a running retained invocation with optional BUFFER."
  (let* ((agent (niwa-agent-runtime-test--agent))
         (invocation (niwa-agent-invocation-create agent)))
    (setf (niwa-agent-invocation-agent-id invocation) "explorer--test"
          (niwa-agent-invocation-path invocation) "/root/explore"
          (niwa-agent-invocation-description invocation) "Explore"
          (niwa-agent-invocation-buffer invocation) buffer
          (niwa-agent-invocation-transcript-status invocation) 'running)
    invocation))

(niwa-deftest niwa-agent-runtime--run-stop-hook ()
  ,test
  (test)
  :doc "fires SubagentStop with invocation metadata"
  (let* ((session (niwa-session--create :name "main"))
         (agent (niwa-agent--create :name "explorer"))
         (parent-buffer (generate-new-buffer " *mev-agent-parent-hooks*"))
         (invocation
          (niwa-agent-invocation--create
           :path "/root/test_agent"
           :agent agent
           :parent-session session
           :parent-data-buffer parent-buffer))
         stop-event)
    (unwind-protect
        (progn
          (with-current-buffer parent-buffer
            (setq-local niwa-subagent-stop-functions
                        (list (lambda (event)
                                (setq stop-event event)
                                nil))))
          (setf (niwa-agent-invocation-terminal-reason invocation) "done")
          (with-temp-buffer
            (niwa-agent-runtime--run-stop-hook invocation 'completed))
          (should (equal (plist-get stop-event :role) "explorer"))
          (should (equal (plist-get stop-event :agent-path)
                         "/root/test_agent"))
          (should (eq (plist-get stop-event :status) 'completed))
          (should (equal (plist-get stop-event :terminal-reason) "done")))
      (kill-buffer parent-buffer))))

(niwa-deftest niwa-agent-runtime--run-prompt-hook ()
  ,test
  (test)
  :doc "runs UserPromptSubmit with retained-agent metadata"
  (let* ((session (niwa-session--create :name "main"))
         (invocation (niwa-agent-runtime-test--invocation))
         event
         (niwa-user-prompt-submit-functions
          (list (lambda (payload)
                  (setq event payload)
                  '(:updated-input "rewritten")))))
    (setf (niwa-agent-invocation-parent-session invocation) session)
    (should
     (equal '(:updated-input "rewritten")
            (niwa-agent-runtime--run-prompt-hook
             "Prompt body" invocation)))
    (should (equal (plist-get event :agent-path) "/root/explore"))
    (should (equal (plist-get event :prompt) "Prompt body"))))

(niwa-deftest niwa-agent-runtime--prepare-followup ()
  ,test
  (test)
  :doc "composes retained pending context without consuming it during preparation"
  (let* ((session (niwa-session--create :name "main"))
         (invocation (niwa-agent-runtime-test--invocation))
         (pending '((:event Old :source "old" :body "pending context")))
         transition
         lifecycle
         (niwa-user-prompt-submit-functions
          (list (lambda (_event)
                  (push 'submit lifecycle)
                  '(:updated-input "rewritten prompt"
                    :additional-context ("prompt context"))))))
    (setf (niwa-agent-invocation-parent-session invocation) session)
    (let ((turn
           (niwa-agent-runtime--prepare-followup
            "original prompt" invocation pending
            (lambda (entries) (setq transition entries)))))
      (should (equal '(submit) (nreverse lifecycle)))
      (should (string-match-p "rewritten prompt" (plist-get turn :prompt)))
      (should (string-match-p "pending context" (plist-get turn :prompt)))
      (should (string-match-p "prompt context" (plist-get turn :prompt)))
      (should-not transition)
      (should (plist-get turn :audits)))))

(niwa-deftest niwa-agent-runtime--prepared-turn ()
  ,test
  (test)
  :doc "applies the accepted rewrite and ordered hook context once"
  (let ((turn
         (niwa-agent-runtime--prepared-turn
          "Original"
          '(:additional-context ("start context"))
          '(:updated-input "Rewritten"
            :additional-context ("prompt context"))
          nil)))
    (should (string-match-p "Rewritten" (plist-get turn :prompt)))
    (should (string-match-p "start context" (plist-get turn :prompt)))
    (should (string-match-p "prompt context" (plist-get turn :prompt)))))

(niwa-deftest niwa-agent-runtime-prepare-task ()
  ,test
  (test)
  :doc "runs the two task hooks asynchronously once and returns one final task"
  (let ((session (niwa-session--create :name "main"))
        (agent (niwa-agent-runtime-test--agent))
        callbacks
        events
        outcome)
    (with-temp-buffer
      (setq-local niwa--session session)
      (cl-letf (((symbol-function 'niwa-hooks-run-event)
                 (lambda (event _payload callback &rest _)
                   (push event events)
                   (push callback callbacks))))
        (niwa-agent-runtime-prepare-task
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
  (let ((session (niwa-session--create :name "main"))
        (agent (niwa-agent-runtime-test--agent))
        hook-callback
        cancelled
        outcome)
    (with-temp-buffer
      (setq-local niwa--session session)
      (cl-letf (((symbol-function 'niwa-hooks-run-event)
                 (lambda (_event _payload callback &rest _)
                   (setq hook-callback callback))))
        (niwa-agent-runtime-prepare-task
         agent "Explore" "Original task" "/root/explore"
         (lambda (value) (setq outcome value))
         :cancelled-p (lambda () cancelled))
        (setq cancelled t)
        (funcall hook-callback nil)
        (should-not outcome)))))

(niwa-deftest niwa-agent-runtime-task-background ()
  ,test
  (test)
  :doc "labels generated background as advisory before the authoritative task"
  (let ((block (niwa-agent-runtime-task-background "  ## Scope\n- context  ")))
    (should (string-prefix-p "<task-background>\n" block))
    (should (string-match-p "may be stale or untrusted" block))
    (should (string-match-p "The following Agent Task is authoritative" block))
    (should (string-match-p "## Scope\n- context" block))
    (should (string-suffix-p "</task-background>\n" block))))

(niwa-deftest niwa-agent-runtime-dispatch
  (:quiet t)
  ,test
  (test)
  :doc "rejects dispatch outside an active session"
  (with-temp-buffer
    (let ((niwa--session nil))
      (should-error
       (niwa-agent-runtime-dispatch
        (niwa-agent-runtime-test--agent)
        "Explore" "Inspect files" :path "/root/explore"))))

  :doc "a synchronous same-message retry failure retains its newly reported notice"
  (let* ((parent (generate-new-buffer " *agent-retry-parent*"))
         (child (generate-new-buffer " *agent-retry-child*"))
         (session (niwa-session--create :name "main"))
         (agent (niwa-agent-runtime-test--agent))
         (configuration (niwa-agent-runtime-test--configuration agent))
         retry)
    (unwind-protect
        (with-current-buffer parent
          (setq-local niwa--session session)
          (niwa-recovery-report
           session "agent:explorer--test" 'request
           "Agent /root/explore failed; inspect its transcript before continuing" nil)
          (cl-letf (((symbol-function 'niwa-agent-conversation-save)
                     (lambda (&rest _) t))
                    ((symbol-function 'niwa-agent-runtime--finalize)
                     (lambda (invocation status &optional _token)
                       (setf (niwa-agent-invocation-transcript-status invocation)
                             status)))
                    ((symbol-function 'run-at-time)
                     (lambda (_seconds _repeat callback &rest _args)
                       (setq retry callback)))
                    ((symbol-function 'niwa-agent-exec-run)
                     (lambda (callback role description invocation _buffer)
                       (funcall
                        (niwa-agent-exec--make-callback
                         callback role description nil (list nil))
                        nil (list :error "Provider failed"
                                  :niwa-agent-invocation invocation))
                       'provider-fsm)))
            (let ((invocation
                   (niwa-agent-runtime-dispatch
                    nil "Retry" "Continue"
                    :path "/root/explore" :frozen-configuration configuration
                    :retained-id "explorer--test" :retained-buffer child
                    :retained-transcript "agents/explorer.chat.org")))
              (should retry)
              (should (eq 'error (niwa-agent-invocation-transcript-status invocation)))
              (should (= 1 (length (niwa-session-recovery-issues session))))
              ;; The callback retries after publication; it must not lose the
              ;; notice while that delivery is pending or after settlement.
              (funcall retry)
              (should (niwa-agent-invocation-runtime-settled-p invocation))
              (should (= 1 (length (niwa-session-recovery-issues session)))))))
      (kill-buffer child)
      (kill-buffer parent)))

  :doc "starts one fresh asynchronous turn and settles its callback once"
  (let* ((parent (generate-new-buffer " *agent-runtime-parent*"))
         (agent-buffer (generate-new-buffer " *agent-runtime-child*"))
         (session (niwa-session--create :name "main"))
         (agent (niwa-agent-runtime-test--agent))
         (configuration
          (niwa-agent-runtime-test--configuration agent))
         (root-fsm (gptel-make-fsm
                    :info (list :niwa-goal-accounting-id "goal-1")))
         provider-callback
         seen-invocation
         settlements
         finalizations
         events)
    (unwind-protect
        (with-current-buffer parent
          (setq-local niwa--session session)
          (setq-local niwa--current-request
                      (niwa-request--create :plan-read-only t
                                               :fsm root-fsm))
          (cl-letf
              (((symbol-function
                 'niwa-agent-exec-freeze-configuration)
                (lambda (&rest _) configuration))
               ((symbol-function
                 'niwa-agent-conversation-open)
                (lambda (&rest _) agent-buffer))
               ((symbol-function 'niwa-agent-runtime--setup-transcript)
                (lambda (invocation _buffer)
                  (push 'transcript-setup events)
                  (setf
                   (niwa-agent-invocation-transcript-relative-path
                    invocation)
                   "agents/explorer.chat.org")))
               ((symbol-function 'niwa-agent-conversation-configure)
                (lambda (&rest _)
                  (push 'conversation-configure events)))
               ((symbol-function
                 'niwa-agent-conversation-save)
                (lambda (&rest _)
                  (push 'transcript-save events)
                  t))
               ((symbol-function 'niwa-agent-exec-run)
                (lambda (callback _role _description invocation _buffer)
                  (setq provider-callback callback)
                  (setf (niwa-agent-invocation-runtime-fsm invocation)
                        'provider-fsm)
                  'provider-fsm))
               ((symbol-function 'niwa-agent-runtime--execution-live-p)
                (lambda (_invocation) nil))
               ((symbol-function 'niwa-view-agent-live-transcript-start)
                (lambda (_invocation)
                  (push 'view-start events)))
               ((symbol-function 'niwa-agent-runtime--finalize)
                (lambda (invocation status &optional _token)
                  (push status finalizations)
                  (setf (niwa-agent-invocation-transcript-status invocation)
                        status))))
            (let ((invocation
                   (niwa-agent-runtime-dispatch
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
                          (niwa-agent-invocation-goal-owner invocation)))
              (should
               (niwa-agent-invocation-plan-read-only invocation))
              (should (eq 'provider-fsm
                          (niwa-agent-invocation-runtime-fsm invocation)))
              (should (equal "/root/explore"
                             (niwa-agent-invocation-path invocation)))
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
               (niwa-agent-invocation-runtime-settled-p invocation)))))
      (kill-buffer agent-buffer)
      (kill-buffer parent)))

  :doc "an explicit Plan stamp applies without a Plan caller"
  (let* ((parent (generate-new-buffer " *agent-runtime-plan-parent*"))
         (agent-buffer (generate-new-buffer " *agent-runtime-plan-child*"))
         (agent (niwa-agent-runtime-test--agent))
         (configuration (niwa-agent-runtime-test--configuration agent)))
    (unwind-protect
        (with-current-buffer parent
          (setq-local niwa--session (niwa-session--create :name "main"))
          (cl-letf (((symbol-function 'niwa-agent-exec-freeze-configuration)
                     (lambda (&rest _) configuration))
                    ((symbol-function 'niwa-agent-conversation-open)
                     (lambda (&rest _) agent-buffer))
                    ((symbol-function 'niwa-agent-runtime--setup-transcript)
                     (lambda (invocation _buffer)
                       (setf (niwa-agent-invocation-transcript-relative-path
                              invocation)
                             "agents/explorer.chat.org")))
                    ((symbol-function 'niwa-agent-conversation-configure)
                     #'ignore)
                    ((symbol-function 'niwa-agent-conversation-save)
                     (lambda (&rest _) t))
                    ((symbol-function 'niwa-agent-exec-run)
                     (lambda (&rest _) 'provider-fsm))
                    ((symbol-function 'niwa-view-agent-live-transcript-start)
                     #'ignore))
            (dolist (stamp '(nil t))
              (should
               (eq stamp
                   (niwa-agent-invocation-plan-read-only
                    (niwa-agent-runtime-dispatch
                     agent "Explore" "Look."
                     :path "/root/explore"
                     :prepared-turn '(:prompt "Look.")
                     :plan-read-only stamp)))))))
      (kill-buffer agent-buffer)
      (kill-buffer parent)))

  :doc "public dispatch shares root authority and routes child prompts to it"
  (let* ((root (file-name-as-directory
                (make-temp-file "niwa-agent-authority-" t)))
         (workspace (niwa-workspace--create
                     :type 'project :id root :root root :name "authority"))
         (session (niwa-session-create "main" workspace root))
         (parent (generate-new-buffer " *agent-authority-parent*"))
         (agent (niwa-agent-runtime-test--agent))
         (configuration
          (niwa-agent-runtime-test--configuration agent))
         invocation
         agent-buffer
         next-called)
    (setf (niwa-session-permission-mode session) 'ask
          (niwa-session-sandbox-mode session) 'required)
    (unwind-protect
        (with-current-buffer parent
          (setq-local niwa--session session
                      niwa--workspace workspace)
          (cl-letf
              (((symbol-function 'gptel-mode) #'ignore)
               ((symbol-function 'niwa-agent-runtime--setup-transcript)
                (lambda (inv _buffer)
                  (setf
                   (niwa-agent-invocation-transcript-relative-path inv)
                   "agents/worker.chat.org")))
               ((symbol-function 'niwa-agent-conversation-configure)
                #'ignore)
               ((symbol-function 'niwa-agent-conversation-save)
                (lambda (&rest _) t))
               ((symbol-function 'niwa-agent-exec-run)
                (lambda (&rest _) 'provider-fsm))
               ((symbol-function 'niwa-permission-queue--render-entry)
                #'ignore))
            (setq invocation
                  (niwa-agent-runtime-dispatch
                   nil "Work" "Edit the file."
                   :path "/root/worker"
                   :prepared-turn '(:prompt "Edit the file.")
                   :frozen-configuration configuration)
                  agent-buffer
                  (niwa-agent-invocation-buffer invocation))
            (with-current-buffer agent-buffer
              (should (eq session niwa--session))
              (should
               (eq 'required
                   (niwa-sandbox-mode-effective niwa--session)))
              (let* ((tool (niwa-tool--create
                            :name "Edit" :read-only-p nil :groups '(edit)))
                     (request (niwa-request--create
                               :id "child-request"
                               :session session
                               :origin "/root/worker")))
                (niwa-tool-permission-step
                 (list :tool tool :args nil :session session :request request)
                 (lambda (_context) (setq next-called t))
                 #'ignore)))
            (let ((entry (car (niwa-session-permission-queue session))))
              (should (equal "/root/worker" (plist-get entry :origin)))
              (should (equal "child-request" (plist-get entry :request-id)))
              (funcall (plist-get entry :callback) 'allow-session))
            (should next-called)
            (should
             (equal '(("Edit" :action allow))
                    (niwa-session-permission-rules session)))))
      (when (buffer-live-p agent-buffer) (kill-buffer agent-buffer))
      (when (buffer-live-p parent) (kill-buffer parent))
      (delete-directory root t)))

  :doc "continues a retained buffer without replaying forked context"
  (let* ((parent (generate-new-buffer " *agent-runtime-parent*"))
         (agent-buffer (generate-new-buffer " *agent-runtime-retained*"))
         (session (niwa-session--create :name "main"))
         (agent (niwa-agent-runtime-test--agent))
         (configuration
          (niwa-agent-runtime-test--configuration agent))
         saved)
    (unwind-protect
        (progn
          (with-current-buffer agent-buffer
            (insert "Existing conversation\n"))
          (with-current-buffer parent
            (setq-local niwa--session session)
            (cl-letf
                (((symbol-function
                   'niwa-agent-conversation-save)
                  (lambda (&rest _)
                    (setq saved t)))
                 ((symbol-function 'niwa-agent-exec-run)
                  (lambda (_callback _role _description invocation _buffer)
                    (setf (niwa-agent-invocation-runtime-fsm invocation)
                          'continued-fsm)
                    'continued-fsm)))
              (let ((invocation
                     (niwa-agent-runtime-dispatch
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
                     (buffer-local-value 'niwa--agent-invocation
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
           (session (niwa-session--create :name "main" :plan-mode plan-mode))
           (configuration
            (niwa-agent-runtime-test--configuration
             (niwa-agent-runtime-test--agent))))
      (unwind-protect
          (with-current-buffer parent
            (setq-local niwa--session session)
            (setq-local niwa--current-request (niwa-request--create))
            (cl-letf (((symbol-function 'niwa-agent-conversation-save)
                       (lambda (&rest _) t))
                      ((symbol-function 'niwa-agent-exec-run)
                       (lambda (_callback _role _description invocation _buffer)
                         (setf (niwa-agent-invocation-runtime-fsm invocation)
                               'continued-fsm)
                         'continued-fsm)))
              (let ((invocation
                     (niwa-agent-runtime-dispatch
                      nil "Continue" "Inspect the parser."
                      :path "/root/explore"
                      :frozen-configuration configuration
                      :retained-id "explorer--test"
                      :retained-buffer agent-buffer
                      :retained-transcript "agents/explorer.chat.org")))
                (should (eq plan-mode
                            (niwa-agent-invocation-plan-read-only
                             invocation)))
                ;; Leaving Plan does not widen the running turn.
                (setf (niwa-session-plan-mode session) nil)
                (with-current-buffer agent-buffer
                  (should (eq plan-mode
                              (and (niwa-plan-read-only-p) t)))))))
        (kill-buffer agent-buffer)
        (kill-buffer parent))))

  :doc "copies forked context into the turn text only for an external engine"
  (let* ((gptel--known-backends nil)
         (claude (niwa-claude-code-register))
         sent)
    (dolist (case `((nil . nil)
                    (((gptel-backend . ,claude) (gptel-model . sonnet))
                     . "PARENT TRANSCRIPT\n\nInitial task")))
      (let ((parent (generate-new-buffer " *agent-runtime-fork-parent*"))
            (agent-buffer (generate-new-buffer " *agent-runtime-fork-child*"))
            (configuration
             (niwa-agent-configuration--create
              :agent (niwa-agent-runtime-test--agent)
              :request-locals (car case))))
        (unwind-protect
            (with-current-buffer parent
              (setq-local niwa--session (niwa-session--create :name "main"))
              (cl-letf
                  (((symbol-function 'niwa-agent-conversation-open)
                    (lambda (&rest _) agent-buffer))
                   ((symbol-function 'niwa-agent-conversation-configure) #'ignore)
                   ((symbol-function 'niwa-agent-runtime--setup-transcript)
                    (lambda (invocation _buffer)
                      (setf (niwa-agent-invocation-transcript-relative-path
                             invocation)
                            "agents/explorer.chat.org")))
                   ((symbol-function 'niwa-agent-conversation-save)
                    (lambda (&rest _) t))
                   ((symbol-function 'niwa-agent-exec-run)
                    (lambda (_callback _role _description invocation _buffer)
                      (setq sent (plist-get (niwa-engine-info invocation)
                                            :niwa-agent-prompt))
                      'provider-fsm)))
                (niwa-agent-runtime-dispatch
                 (niwa-agent-runtime-test--agent) "Explore" "Initial task"
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
         (session (niwa-session--create :name "main"))
         (agent (niwa-agent-runtime-test--agent))
         (configuration
          (niwa-agent-runtime-test--configuration agent))
         (start-count 0)
         (submit-count 0)
         lifecycle
         provider-prompts
         first)
    (unwind-protect
        (with-current-buffer parent
          (setq-local niwa--session session)
          (let ((niwa-subagent-start-functions
                 (list (lambda (_event)
                         (cl-incf start-count)
                         (push 'start lifecycle)
                         '(:additional-context ("one startup context")))))
                (niwa-user-prompt-submit-functions
                 (list (lambda (_event)
                         (cl-incf submit-count)
                         (push 'submit lifecycle)
                         '(:additional-context ("per-turn context"))))))
            (cl-letf
                (((symbol-function 'niwa-agent-conversation-open)
                  (lambda (&rest _) agent-buffer))
                 ((symbol-function 'niwa-agent-runtime--setup-transcript)
                  (lambda (invocation _buffer)
                    (setf
                     (niwa-agent-invocation-transcript-relative-path
                      invocation)
                     "agents/explorer.chat.org")))
                 ((symbol-function 'niwa-agent-conversation-save)
                  (lambda (&rest _) t))
                 ((symbol-function 'niwa-agent-exec-run)
                  (lambda (_callback _role _description invocation buffer)
                    (with-current-buffer buffer
                      (push (buffer-string) provider-prompts))
                    (push 'model lifecycle)
                    (setf (niwa-agent-invocation-runtime-fsm invocation)
                          'provider-fsm)
                    'provider-fsm)))
              (let (prepared)
                (niwa-agent-runtime-prepare-task
                 agent "Explore" "Initial task" "/root/explore"
                 (lambda (result) (setq prepared result)))
                (should (eq 'success (plist-get prepared :outcome)))
                (setq first
                      (niwa-agent-runtime-dispatch
                       agent "Explore" "Initial task"
                       :path "/root/explore"
                       :frozen-configuration configuration
                       :prepared-turn (plist-get prepared :turn)
                       :start-hook-audits
                       (plist-get prepared :start-hook-audits))))
              (niwa-agent-runtime-dispatch
               nil "Continue" "Follow-up task"
               :path "/root/explore"
               :frozen-configuration configuration
               :retained-id
               (niwa-agent-invocation-agent-id first)
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
         (session (niwa-session--create :name "main"))
         (agent (niwa-agent-runtime-test--agent))
         (configuration
          (niwa-agent-runtime-test--configuration agent))
         (submit-count 0)
         pending
         provider-prompt)
    (unwind-protect
        (with-current-buffer parent
          (setq-local niwa--session session)
          (niwa-recovery-report session "agent:explorer--test" 'request "Old failure" nil)
          (niwa-recovery-report session "agent:other" 'request "Other failure" nil)
          (let ((niwa-user-prompt-submit-functions
                 (list
                  (lambda (_event)
                    (cl-incf submit-count)
                    (if (= submit-count 1)
                        '(:continue nil
                          :additional-context ("carry this context"))
                      '(:updated-input "Rewritten accepted task"
                        :additional-context ("current context")))))))
            (cl-letf
                (((symbol-function 'niwa-agent-conversation-save)
                  (lambda (&rest _) t))
                 ((symbol-function 'niwa-recovery-save) #'ignore)
                 ((symbol-function 'niwa-agent-exec-run)
                  (lambda (_callback _role _description invocation buffer)
                    (with-current-buffer buffer
                      (setq provider-prompt (buffer-string)))
                    (setf (niwa-agent-invocation-runtime-fsm invocation)
                          'provider-fsm)
                    'provider-fsm)))
              (should-error
               (niwa-agent-runtime-dispatch
                nil "Continue" "Blocked task"
                :path "/root/explore"
                :frozen-configuration configuration
                :retained-id "explorer--test"
                :retained-buffer agent-buffer
                :retained-transcript "agents/explorer.chat.org"
                :on-hook-context (lambda (entries) (setq pending entries))))
              (should (= 2 (length (niwa-session-recovery-issues session))))
              (with-current-buffer agent-buffer
                (should-not (string-match-p "Blocked task" (buffer-string))))
              (should pending)
              (niwa-agent-runtime-dispatch
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
                                     (niwa-session-recovery-issues session))))
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
         (session (niwa-session--create :name "main"))
         (agent (niwa-agent-runtime-test--agent))
         (configuration
          (niwa-agent-runtime-test--configuration agent))
         (pending '((:event UserPromptSubmit
                     :source "hook"
                     :body "keep this context")))
         (transitions 0)
         provider-called)
    (unwind-protect
        (with-current-buffer parent
          (setq-local niwa--session session)
          (cl-letf (((symbol-function 'niwa-agent-conversation-save)
                     (lambda (&rest _) nil))
                    ((symbol-function 'niwa-agent-exec-run)
                     (lambda (&rest _)
                       (setq provider-called t)
                       'provider-fsm)))
            (should-error
             (niwa-agent-runtime-dispatch
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

(defvar test-niwa-agent-runtime--mcp-identity nil
  "Registered (CATEGORY NAME) of the fixture's wrapped MCP tool.")

(defun test-niwa-agent-runtime--catalog-identities ()
  "Return the delegator's specialists.
They are communication, built-in, configured and MCP tools; the MCP
identity is the one its wrapper registered."
  (list '("niwa" "SendMessage") '("niwa" "Imenu")
        '("user-tools" "ConfiguredProbe")
        test-niwa-agent-runtime--mcp-identity))

(defun test-niwa-agent-runtime--mcp-name ()
  "Return the ToolCall name of the fixture's MCP tool."
  (niwa-tool-call-name
   (niwa-tool-get (cadr test-niwa-agent-runtime--mcp-identity)
                     (car test-niwa-agent-runtime--mcp-identity))))

(defun test-niwa-agent-runtime--catalog-names ()
  "Return the ToolCall names of the delegator's specialists."
  (list "SendMessage" "Imenu" "user-tools/ConfiguredProbe"
        (test-niwa-agent-runtime--mcp-name)))

(defvar test-niwa-agent-runtime--backend nil
  "Registered provider backend of the specialist catalog fixture.")

(defvar test-niwa-agent-runtime--buffers nil
  "Agent buffers the specialist catalog fixture must kill.")

(defun test-niwa-agent-runtime--register-catalog-tools ()
  "Register the delegator fixture's direct and specialist tools."
  (dolist (name '("Read" "ToolSearch" "ToolCall"
                  "SendMessage" "ListAgents" "Imenu"))
    (niwa-tool-ensure name))
  ;; A user-configured tool registered in its own category.
  (niwa-tool-register
   (niwa-tool--create
    :name "ConfiguredProbe" :category "user-tools"
    :summary "Configured probe" :description "Configured probe contract"
    :args '((value string :required "Probe value"))
    :handler (lambda (callback _args) (funcall callback "probe"))))
  ;; An MCP tool arrives as a gptel tool in its server's category and is
  ;; registered through the same wrapper MCP configuration uses.
  (let ((wrapped
         (niwa-tool--register-wrap
          :source (gptel-make-tool
                   :name "search_issues" :category "probe-mcp"
                   :function (lambda (&rest _) "issues")
                   :description "Search issues on the probe MCP server"
                   :args (list '(:name "query" :type string
                                 :description "Issue query")))
          :summary "Search probe issues" :read-only-p t)))
    (setq test-niwa-agent-runtime--mcp-identity
          (list (niwa-tool-category wrapped) (niwa-tool-name wrapped)))))

(defmacro test-niwa-agent-runtime--with-delegator (identities &rest body)
  "Run BODY in a root session buffer whose specialist catalog is IDENTITIES.
The root's direct tools are Read, ToolSearch and ToolCall.  Tool, backend
and buffer state are confined to BODY."
  (declare (indent 1))
  `(let* ((niwa-tool--registry (copy-hash-table niwa-tool--registry))
          (gptel--known-tools (copy-tree gptel--known-tools))
          (gptel--known-backends (copy-alist gptel--known-backends))
          (test-niwa-agent-runtime--backend
           (gptel--make-backend :name "Agent Catalog Test"))
          (test-niwa-agent-runtime--buffers nil)
          (test-niwa-agent-runtime--mcp-identity nil)
          (root (generate-new-buffer " *catalog-root*")))
     (unwind-protect
         (progn
           (setf (gptel-get-backend "Agent Catalog Test")
                 test-niwa-agent-runtime--backend)
           (test-niwa-agent-runtime--register-catalog-tools)
           (with-current-buffer root
             (setq-local niwa--session
                         (niwa-session--create
                          :name "main"
                          :tool-catalog
                          (mapcar (lambda (identity)
                                    (cons (copy-sequence identity)
                                          (format "%s summary" (cadr identity))))
                                  ,identities)))
             (setq-local gptel-tools
                         (mapcar (lambda (name)
                                   (niwa-tool-gptel-tool (niwa-tool-get name)))
                                 '("Read" "ToolSearch" "ToolCall")))
             ,@body))
       (dolist (buffer (cons root test-niwa-agent-runtime--buffers))
         (when (buffer-live-p buffer)
           (with-current-buffer buffer (set-buffer-modified-p nil))
           (kill-buffer buffer))))))

(defun test-niwa-agent-runtime--spawn (agent path &optional configuration)
  "Dispatch AGENT at PATH from the current buffer and return its buffer.
With CONFIGURATION, continue a retained conversation from that frozen
configuration instead, as a follow-up or a cold restore does.  Only
transcript storage and the provider are stand-ins: invocation creation,
freezing and buffer configuration are the real ones."
  (let ((session niwa--session)
        (child (and configuration
                    (generate-new-buffer " *catalog-retained*"))))
    (when child
      (push child test-niwa-agent-runtime--buffers)
      (with-current-buffer child (setq-local niwa--session session)))
    (cl-letf (((symbol-function 'niwa-agent-conversation-open)
               (lambda (&rest _)
                 (setq child (generate-new-buffer " *catalog-child*"))
                 (push child test-niwa-agent-runtime--buffers)
                 (with-current-buffer child (setq-local niwa--session session))
                 child))
              ((symbol-function 'niwa-agent-runtime--setup-transcript)
               (lambda (invocation buffer)
                 (setf (niwa-agent-invocation-transcript-relative-path
                        invocation)
                       "agents/catalog.chat.org")
                 (with-current-buffer buffer
                   (setq-local niwa--agent-invocation invocation))))
              ((symbol-function 'niwa-agent-conversation-save)
               (lambda (&rest _) t))
              ((symbol-function 'niwa-agent-exec-run)
               (lambda (&rest _) 'provider-fsm))
              ((symbol-function 'niwa-agent-runtime--execution-live-p)
               #'ignore)
              ((symbol-function 'niwa-view-agent-live-transcript-start)
               #'ignore)
              ((symbol-function 'niwa-telemetry-record) #'ignore))
      (if configuration
          (niwa-agent-runtime-dispatch
           nil "Continue" "Continue the task."
           :path path :frozen-configuration configuration
           :retained-id (format "retained--%s" path)
           :retained-buffer child
           :retained-transcript "agents/catalog.chat.org")
        (niwa-agent-runtime-dispatch
         agent "Catalog task" "Report your tools."
         :path path :prepared-turn '(:prompt "Report your tools.")
         :model-policy (list :backend test-niwa-agent-runtime--backend
                             :model 'catalog-test-model :effort nil))))
    child))

(defun test-niwa-agent-runtime--discovery (buffer names)
  "Return what BUFFER's agent finds for NAMES through ToolSearch and ToolCall.
Each element is (NAME SEARCH-RESULT IN-TOOLCALL-ROSTER-P)."
  (with-current-buffer buffer
    (let (roster)
      (cl-letf (((symbol-function 'niwa-ptc-driver-run)
                 (lambda (_callback _expression names &optional _standalone)
                   (setq roster names))))
        (niwa-tool-ptc--handler #'ignore '(:expression "nil")))
      (mapcar (lambda (name)
                (let (result)
                  (niwa-tools--tool-search
                   (lambda (value) (setq result value)) name)
                  (list name result (and (member name roster) t))))
              names))))

(defun test-niwa-agent-runtime--should-discover (buffer names)
  "Assert BUFFER's agent retrieves contracts for and can call every NAMES."
  (dolist (found (test-niwa-agent-runtime--discovery buffer names))
    (ert-info ((format "discover %s" (car found)))
      (should (string-prefix-p (car found) (nth 1 found)))
      (should (string-search "Calling signature" (nth 1 found)))
      (should (nth 2 found)))))

(defun test-niwa-agent-runtime--should-not-discover (buffer names)
  "Assert BUFFER's agent can neither find nor call any of NAMES."
  (dolist (found (test-niwa-agent-runtime--discovery buffer names))
    (ert-info ((format "hidden %s" (car found)))
      (should (string-prefix-p "No matching tools found" (nth 1 found)))
      (should-not (nth 2 found)))))

(defun test-niwa-agent-runtime--native-p (buffer name)
  "Return non-nil when BUFFER's agent offers NAME as a native tool."
  (cl-find name (buffer-local-value 'gptel-tools buffer)
           :key #'gptel-tool-name :test #'equal))

(defun test-niwa-agent-runtime--named-role ()
  "Return a named role with its own direct and specialist tools."
  (niwa-agent--create
   :name "catalog_named" :description "Named catalog role"
   :tools '((:tool "Read") (:discoverable (:tool "Imenu")))
   :system-prompt "Named role."))

(niwa-deftest niwa-agent-runtime-dispatch/specialist-catalog
  (:quiet t)
  ,test
  (test)
  :doc "a default agent finds and can call every delegator specialist"
  (test-niwa-agent-runtime--with-delegator
      (test-niwa-agent-runtime--catalog-identities)
    ;; The fixture is valid only if the delegator itself can use them all.
    (test-niwa-agent-runtime--should-discover
     (current-buffer) (test-niwa-agent-runtime--catalog-names))
    (let ((child (test-niwa-agent-runtime--spawn
                  (niwa-agent-default) "/root/helper")))
      (test-niwa-agent-runtime--should-discover
       child (test-niwa-agent-runtime--catalog-names))
      ;; A permitted SendMessage is native as well, so reporting needs no search.
      (should (test-niwa-agent-runtime--native-p child "SendMessage"))
      ;; The inherited catalog is frozen with the agent, not read live.
      (push (cons (list "niwa" "ListAgents") "late")
            (niwa-session-tool-catalog niwa--session))
      (test-niwa-agent-runtime--should-not-discover child '("ListAgents"))))

  :doc "a default agent of a default agent inherits the same catalog"
  (test-niwa-agent-runtime--with-delegator
      (test-niwa-agent-runtime--catalog-identities)
    (let* ((child (test-niwa-agent-runtime--spawn
                   (niwa-agent-default) "/root/helper"))
           (grandchild (with-current-buffer child
                         (test-niwa-agent-runtime--spawn
                          (niwa-agent-default) "/root/helper/sub"))))
      (test-niwa-agent-runtime--should-discover
       grandchild (test-niwa-agent-runtime--catalog-names))))

  :doc "a retained follow-up and a cold restore keep the inherited catalog"
  (test-niwa-agent-runtime--with-delegator
      (test-niwa-agent-runtime--catalog-identities)
    (let* ((child (test-niwa-agent-runtime--spawn
                   (niwa-agent-default) "/root/helper"))
           (configuration
            (niwa-agent-invocation-frozen-configuration
             (buffer-local-value 'niwa--agent-invocation child)))
           (restored
            (niwa-agent-persistence--decode-configuration
             (car (read-from-string
                   (prin1-to-string
                    (niwa-agent-persistence--encode-configuration
                     configuration))))
             "default")))
      ;; Neither continuation may fall back to the delegator's live catalog.
      (setf (niwa-session-tool-catalog niwa--session) nil)
      ;; Nor may a specialist registered and catalogued after freezing appear.
      (niwa-tool-register
       (niwa-tool--create :name "LateProbe" :category "user-tools"
                             :summary "Late probe" :description "Late contract"))
      (push (cons (list "user-tools" "LateProbe") "late")
            (niwa-session-tool-catalog niwa--session))
      (test-niwa-agent-runtime--should-discover
       (test-niwa-agent-runtime--spawn nil "/root/helper" configuration)
       (test-niwa-agent-runtime--catalog-names))
      (test-niwa-agent-runtime--should-discover
       (test-niwa-agent-runtime--spawn nil "/root/helper" restored)
       (test-niwa-agent-runtime--catalog-names))
      (dolist (continued
               (list (test-niwa-agent-runtime--spawn
                      nil "/root/helper" configuration)
                     (test-niwa-agent-runtime--spawn
                      nil "/root/helper" restored)))
        (test-niwa-agent-runtime--should-not-discover
         continued '("user-tools/LateProbe")))))

  :doc "a named role keeps its own specialists, not its delegator's"
  (test-niwa-agent-runtime--with-delegator
      (test-niwa-agent-runtime--catalog-identities)
    (let ((child (test-niwa-agent-runtime--spawn
                  (test-niwa-agent-runtime--named-role) "/root/named")))
      (should (test-niwa-agent-runtime--native-p child "SendMessage"))
      (test-niwa-agent-runtime--should-discover
       child '("ListAgents" "Imenu"))
      (test-niwa-agent-runtime--should-not-discover
       child (list "user-tools/ConfiguredProbe"
                    (test-niwa-agent-runtime--mcp-name)))))

  :doc "a default agent of a narrower delegator never gains the root catalog"
  (test-niwa-agent-runtime--with-delegator
      (test-niwa-agent-runtime--catalog-identities)
    (let* ((named (test-niwa-agent-runtime--spawn
                   (test-niwa-agent-runtime--named-role) "/root/named"))
           (grandchild (with-current-buffer named
                         (test-niwa-agent-runtime--spawn
                          (niwa-agent-default) "/root/named/helper"))))
      ;; The named delegator's native SendMessage is inherited natively.
      (should (test-niwa-agent-runtime--native-p grandchild "SendMessage"))
      (test-niwa-agent-runtime--should-discover
       grandchild '("ListAgents" "Imenu"))
      (test-niwa-agent-runtime--should-not-discover
       grandchild (list "user-tools/ConfiguredProbe"
                    (test-niwa-agent-runtime--mcp-name)))))

  :doc "a default agent inherits only what its delegator's request may call"
  (dolist (restricted '(some none))
    (ert-info ((format "request allows %s" restricted))
      (test-niwa-agent-runtime--with-delegator
          (test-niwa-agent-runtime--catalog-identities)
        (let ((allowed (and (eq restricted 'some)
                            (list "SendMessage"
                                  (test-niwa-agent-runtime--mcp-name)))))
        ;; A skill's allowed-tools restricts the request, not the catalog.
        (setq-local niwa--current-request
                    (niwa-request--create :ptc-primitives allowed))
        (let ((child (test-niwa-agent-runtime--spawn
                      (niwa-agent-default) "/root/helper")))
          (test-niwa-agent-runtime--should-discover child allowed)
          ;; A restricted request grants no native SendMessage either.
          (should (eq (eq restricted 'some)
                      (and (test-niwa-agent-runtime--native-p child "SendMessage")
                           t)))
          (test-niwa-agent-runtime--should-not-discover
           child (cl-set-difference (test-niwa-agent-runtime--catalog-names)
                                    allowed :test #'equal)))))))

  :doc "a default agent inherits its delegator's catalog, not a bound sibling's"
  ;; Asynchronous hook callbacks restore the delegator's buffer while another
  ;; agent's engine may still be dynamically bound.
  (dolist (shape '(empty-sibling broader-sibling))
    (ert-info ((format "%s" shape))
      (test-niwa-agent-runtime--with-delegator
          (if (eq shape 'empty-sibling)
              (test-niwa-agent-runtime--catalog-identities)
            (list '("niwa" "SendMessage")))
        (let* ((sibling
                (niwa-agent-invocation--create
                 :path "/root/sibling"
                 :tool-catalog
                 (and (eq shape 'broader-sibling)
                      (mapcar (lambda (identity) (cons identity "sibling"))
                              (cons '("niwa" "ListAgents")
                                    (test-niwa-agent-runtime--catalog-identities))))))
               (child
                (let ((niwa-tools--current-engine
                       (gptel-make-fsm
                        :info (list :niwa-agent-invocation sibling))))
                  (test-niwa-agent-runtime--spawn
                   (niwa-agent-default) "/root/helper"))))
          (if (eq shape 'empty-sibling)
              (test-niwa-agent-runtime--should-discover
               child (test-niwa-agent-runtime--catalog-names))
            (test-niwa-agent-runtime--should-discover child '("SendMessage"))
            (test-niwa-agent-runtime--should-not-discover
             child (cons "ListAgents"
                         (cdr (test-niwa-agent-runtime--catalog-names)))))))))

  :doc "an agent delegator's own catalog and request decide, not a bound sibling's"
  (test-niwa-agent-runtime--with-delegator
      (test-niwa-agent-runtime--catalog-identities)
    (let* ((child (test-niwa-agent-runtime--spawn
                   (niwa-agent-default) "/root/helper"))
           (allowed (list "SendMessage" (test-niwa-agent-runtime--mcp-name)))
           (sibling
            (niwa-agent-invocation--create
             :path "/root/sibling"
             :tool-catalog
             (mapcar (lambda (identity) (cons identity "sibling"))
                     (cons '("niwa" "ListAgents")
                           (test-niwa-agent-runtime--catalog-identities)))))
           (grandchild
            (with-current-buffer child
              ;; The delegating agent's own request restricts what it may call.
              ;; It also allows ListAgents, which only the sibling has, so a
              ;; sibling leak would show.
              (setq-local niwa--current-request
                          (niwa-request--create
                           :ptc-primitives (cons "ListAgents" allowed)))
              (let ((niwa-tools--current-engine
                     (gptel-make-fsm
                      :info (list :niwa-agent-invocation sibling))))
                (test-niwa-agent-runtime--spawn
                 (niwa-agent-default) "/root/helper/sub")))))
      (test-niwa-agent-runtime--should-discover grandchild allowed)
      (test-niwa-agent-runtime--should-not-discover
       grandchild '("ListAgents" "Imenu" "user-tools/ConfiguredProbe"))))

  :doc "a default agent of an empty delegator stays empty as the root grows"
  (test-niwa-agent-runtime--with-delegator nil
    (let ((child (test-niwa-agent-runtime--spawn
                  (niwa-agent-default) "/root/helper")))
      (setf (niwa-session-tool-catalog niwa--session)
            (mapcar (lambda (identity) (cons identity "late"))
                    (test-niwa-agent-runtime--catalog-identities)))
      (let ((grandchild (with-current-buffer child
                          (test-niwa-agent-runtime--spawn
                           (niwa-agent-default) "/root/helper/sub"))))
        (dolist (buffer (list child grandchild))
          (test-niwa-agent-runtime--should-not-discover
           buffer (cons "ListAgents"
                        (test-niwa-agent-runtime--catalog-names))))))))

(niwa-deftest niwa-agent-runtime-queue-execution-completion
  ()
  ,test
  (test)
  :doc "keeps the terminal answer separate from Bash output in either arrival order"
  (dolist (response-first-p '(nil t))
    (let* ((session (niwa-session--create))
           (invocation (niwa-agent-runtime-test--invocation))
           (record (niwa-agent-record--create
                    :path "/root/explore" :parent-path "/root"
                    :activity 'running :invocation invocation))
           (live-p t)
           finalizations)
      (setf (niwa-session-agent-registry session)
            (list (cons "/root/explore" record))
            (niwa-agent-invocation-parent-session invocation) session
            (niwa-agent-invocation-runtime-settle-callback invocation)
            (lambda (invocation response event &optional token)
              (niwa-agent-control--settle
               session record invocation response event token)))
      (cl-letf (((symbol-function 'niwa-agent-runtime--execution-live-p)
                 (lambda (_invocation) live-p))
                ((symbol-function 'niwa-agent-runtime--finalize)
                 (lambda (_invocation status &optional _token)
                   (push status finalizations))))
        (when response-first-p
          (niwa-agent-runtime--handle-provider-result invocation "Agent answer")
          (should (equal "Agent answer"
                         (niwa-agent-invocation-runtime-pending-response
                          invocation))))
        (should-not
         (niwa-agent-runtime-queue-execution-completion
          invocation "/root/other" "wrong owner"))
        (should
         (niwa-agent-runtime-queue-execution-completion
          invocation "/root/explore" "First Bash exited with code 0"))
        (should-not (niwa-session-messages session))
        ;; A still-working agent reads its own completion first.
        (should (equal (if response-first-p nil '(EXECUTION))
                       (mapcar (lambda (msg) (plist-get msg :type))
                               (niwa-agent-record-mailbox record))))
        (should-not finalizations)
        (setq live-p nil)
        (should
         (niwa-agent-runtime-queue-execution-completion
          invocation "/root/explore" "Second Bash exited with code 1"))
        (unless response-first-p
          (should-not (niwa-session-messages session))
          (niwa-agent-runtime--handle-provider-result invocation "Agent answer"))
        (let ((messages (niwa-agent-control-context-mailbox session)))
          (should (equal '(RESULT EXECUTION EXECUTION)
                         (mapcar (lambda (msg) (plist-get msg :type)) messages)))
          (should (equal '("Agent answer" "First Bash exited with code 0"
                           "Second Bash exited with code 1")
                         (mapcar (lambda (msg) (plist-get msg :payload)) messages))))
        (should (equal "Agent answer" (niwa-agent-record-settled-result record)))
        (should-not (niwa-agent-record-mailbox record))
        (should (equal '(completed) finalizations))
        (should-not
         (niwa-agent-runtime-queue-execution-completion
          invocation "/root/explore" "duplicate"))))))

(niwa-deftest niwa-agent-runtime-queue-execution-completion/meta
  (:doc "forwards structural metadata to the working agent's mailbox record")
  (let* ((session (niwa-session--create))
         (invocation (niwa-agent-runtime-test--invocation))
         (record (niwa-agent-record--create
                  :path "/root/explore" :parent-path "/root"
                  :activity 'running :invocation invocation)))
    (setf (niwa-session-agent-registry session)
          (list (cons "/root/explore" record))
          (niwa-agent-invocation-parent-session invocation) session)
    (should (niwa-agent-runtime-queue-execution-completion
             invocation "/root/explore" "done"
             '(:execution-id "e1" :termination "exited")))
    (should (equal '(:execution-id "e1" :termination "exited")
                   (plist-get (car (niwa-agent-record-mailbox record))
                              :meta)))
    ;; A held response keeps the metadata for settlement.
    (setf (niwa-agent-invocation-runtime-pending-response invocation) "ans")
    (cl-letf (((symbol-function 'niwa-agent-runtime--execution-live-p)
               (lambda (_) t)))
      (niwa-agent-runtime-queue-execution-completion
       invocation "/root/explore" "held" '(:execution-id "e2")))
    (should (equal '(("held" :execution-id "e2"))
                   (niwa-agent-invocation-runtime-execution-results
                    invocation)))))

(niwa-deftest niwa-agent-runtime-queue-execution-completion/read
  (:doc "does not forward a completion the working agent already read")
  (let* ((session (niwa-session--create))
         (invocation (niwa-agent-runtime-test--invocation))
         (record (niwa-agent-record--create
                  :path "/root/explore" :parent-path "/root"
                  :activity 'running :invocation invocation)))
    (setf (niwa-session-agent-registry session)
          (list (cons "/root/explore" record))
          (niwa-agent-invocation-parent-session invocation) session
          (niwa-agent-invocation-runtime-settle-callback invocation)
          (lambda (invocation response event &optional token)
            (niwa-agent-control--settle
             session record invocation response event token)))
    (cl-letf (((symbol-function 'niwa-agent-runtime--execution-live-p)
               (lambda (_invocation) nil))
              ((symbol-function 'niwa-agent-runtime--finalize) #'ignore))
      (niwa-agent-runtime-queue-execution-completion
       invocation "/root/explore" "Probe output")
      ;; The agent's next WAIT delivers and clears its mailbox.
      (should (niwa-agent-control-context-mailbox invocation))
      (niwa-agent-control-clear-context-mailbox invocation)
      (niwa-agent-runtime--handle-provider-result invocation "Agent answer"))
    (should (equal '(RESULT)
                   (mapcar (lambda (msg) (plist-get msg :type))
                           (niwa-agent-control-context-mailbox session))))))

(niwa-deftest niwa-agent-runtime-queue-execution-completion/acknowledgement
  (:quiet t)
  ,test
  (test)
  :doc "a rejected completion acknowledges failure once and returns nil"
  (let ((invocation (niwa-agent-runtime-test--invocation))
        acks)
    (should-not
     (niwa-agent-runtime-queue-execution-completion
      invocation "/root/other" "wrong owner" nil
      (lambda (success error) (push (list success error) acks))))
    (should (equal '((nil "Execution owner no longer accepts results")) acks)))
  :doc "a held response acknowledges success after the body joins its results"
  (let ((invocation (niwa-agent-runtime-test--invocation))
        acks)
    (setf (niwa-agent-invocation-runtime-pending-response invocation) "ans")
    (cl-letf (((symbol-function 'niwa-agent-runtime--execution-live-p)
               (lambda (_) t)))
      (should
       (eq t (niwa-agent-runtime-queue-execution-completion
              invocation (niwa-agent-invocation-require-path invocation)
              "held" nil
              (lambda (success error)
                (push (list success error
                            (length (niwa-agent-invocation-runtime-execution-results
                                     invocation)))
                      acks))))))
    (should (equal '((t nil 1)) acks)))
  :doc "the mailbox path forwards the acknowledgement and its pending marker"
  (dolist (marker '(pending t))
    (let* ((session (niwa-session--create))
           (invocation (niwa-agent-runtime-test--invocation))
           (path (niwa-agent-invocation-require-path invocation))
           (ack (lambda (&rest _) nil))
           captured)
      (setf (niwa-agent-invocation-parent-session invocation) session)
      (cl-letf (((symbol-function 'niwa-agent-control-enqueue-execution-result)
                 (lambda (&rest args) (setq captured args) marker)))
        (should (eq marker
                    (niwa-agent-runtime-queue-execution-completion
                     invocation path "body" '(:execution-id "e1") ack))))
      (should (equal (list session path "body" nil nil '(:execution-id "e1"))
                     (seq-take captured 6)))
      (should (eq ack (nth 6 captured)))))
  :doc "the last held completion waits for durable RESULT and transcript, including rollback retry"
  (dolist (failure-mode '(nil before-commit after-commit))
    (test-niwa-publication-async--fixture
     (lambda (session parent)
       (let* ((child (generate-new-buffer " *held-completion*"))
              (invocation (niwa-agent-runtime-test--invocation child))
              (record (niwa-agent-record--create
                       :id "explorer--test" :role "explorer"
                       :path "/root/explore" :parent-path "/root"
                       :activity 'running :invocation invocation))
              (publisher (symbol-function
                          'niwa-session-artifacts-publish-agent-terminal-state))
              (deliver (symbol-function 'niwa-agent-control--publish-result))
              (fail (eq failure-mode 'before-commit))
              (fail-delivery (eq failure-mode 'after-commit))
              acks publication-called durable)
         (setf (niwa-agent-invocation-parent-session invocation) session
               (niwa-agent-invocation-parent-data-buffer invocation) parent
               (niwa-agent-invocation-runtime-pending-response invocation) "answer"
               (niwa-session-agent-registry session)
               (list (cons "/root/explore" record))
               (niwa-agent-invocation-runtime-settle-callback invocation)
               (lambda (inv response event token)
                 (niwa-agent-control--settle session record inv response event token)))
         (setf (niwa-agent-invocation-frozen-configuration invocation)
               (with-current-buffer parent
                 (niwa-agent-exec-freeze-configuration
                  "explorer" invocation (list :backend gptel-backend :model gptel-model)))
               (niwa-agent-record-configuration record)
               (niwa-agent-invocation-frozen-configuration invocation))
         (with-current-buffer child
           (org-mode)
           (setq-local niwa--session session niwa--agent-invocation invocation)
           (niwa-agent-runtime--setup-transcript invocation child)
           (insert "* Agent Task: explore\n\nHeld durable response\n"))
         (setf (niwa-agent-record-conversation-location record)
               (niwa-agent-invocation-transcript-relative-path invocation))
         ;; A real accepted preparation holds the gate before any publication
         ;; or terminal-state capture; no fabricated token opens the gate.
         (setf (niwa-session-publication-preparations session)
               (list (cons #'ignore nil)))
         (cl-letf (((symbol-function 'niwa-agent-runtime--execution-live-p)
                    (lambda (_) nil))
                   ((symbol-function 'niwa-session-artifacts-publish-agent-terminal-state)
                    (lambda (&rest args)
                      (setq publication-called t)
                      (should-not acks)
                      (when fail (error "Injected deferred settlement failure"))
                      (prog1 (apply publisher args) (setq durable t))))
                   ((symbol-function 'niwa-agent-control--publish-result)
                    (lambda (&rest args)
                      (when fail-delivery (error "Injected postcommit notification failure"))
                      (apply deliver args))))
           (let ((ack (lambda (success error)
                        (when success
                          (should durable)
                          (should
                           (equal '(RESULT EXECUTION)
                                  (mapcar (lambda (mail) (plist-get mail :type))
                                          (test-niwa-publication-async--restored-mail session))))
                          (should
                           (string-match-p
                            "Held durable response"
                            (niwa-session-artifacts-read-artifact
                             session (niwa-agent-invocation-transcript-relative-path invocation)
                             t))))
                        (push (list success error) acks))))
             (should
              (eq 'pending
                  (niwa-agent-runtime-queue-execution-completion
                   invocation "/root/explore" "execution output" '(:execution-id "held") ack)))
             (should-not acks)
             (should-not publication-called)
             (should-not (niwa-agent-invocation-runtime-settled-p invocation))
             (should-not (test-niwa-publication-async--restored-mail session))
             (niwa-session-publication-barrier session)
             (should publication-called)
             (should (= 1 (length acks)))
             (when (eq failure-mode 'before-commit)
               (should (equal '((nil "Injected deferred settlement failure")) acks))
               (should-not (niwa-agent-invocation-runtime-settled-p invocation))
               (should-not (niwa-agent-invocation-runtime-execution-results invocation))
               (should-not (test-niwa-publication-async--mail session))
               (should-not (test-niwa-publication-async--restored-mail session))
               ;; The producer still owns the failed output.  Retry it through
               ;; the same sink and verify it appears exactly once on disk.
               (setq fail nil acks nil)
               (should
                (eq t (niwa-agent-runtime-queue-execution-completion
                       invocation "/root/explore" "execution output"
                       '(:execution-id "held") ack))))
             (should (equal '((t nil)) acks))
             (when fail-delivery
               ;; Adoption already secured the result despite the notification
               ;; error. Retrying cleanup must neither reconsume nor republish it.
               (should-not (niwa-agent-invocation-runtime-settled-p invocation))
               (setq fail-delivery nil)
               (niwa-agent-runtime--settle invocation "answer"))
             (should (niwa-agent-invocation-runtime-settled-p invocation))
             (niwa-session-publication-barrier session)
             (should (equal '((t nil)) acks))))))))
  :doc "a rejected deferred preparation acknowledges failure once even if teardown repeats it"
  (let* ((session (niwa-session--create))
         (invocation (niwa-agent-runtime-test--invocation))
         failure acks)
    (setf (niwa-agent-invocation-parent-session invocation) session
          (niwa-agent-invocation-runtime-pending-response invocation) "answer")
    (cl-letf (((symbol-function 'niwa-agent-runtime--execution-live-p)
               (lambda (_) nil))
              ((symbol-function 'niwa-session-publication-defer)
               (lambda (_session _preparation &optional handler _token)
                 (setq failure handler) t)))
      (should (eq 'pending
                  (niwa-agent-runtime-queue-execution-completion
                   invocation "/root/explore" "retained output" nil
                   (lambda (success error) (push (list success error) acks)))))
      (should-not acks)
      (funcall failure '(error "Teardown rejected settlement"))
      (funcall failure '(error "Teardown rejected settlement")))
    (should (equal '((nil "Teardown rejected settlement")) acks))
    (should-not (niwa-agent-invocation-runtime-execution-results invocation))
    (should-not (niwa-agent-invocation-runtime-settled-p invocation)))
  :doc "synchronous settlement errors and quits acknowledge failure once before propagating"
  (dolist (condition '(error quit))
    (let ((invocation (niwa-agent-runtime-test--invocation))
          (siblings (list (cons "older second" nil) (cons "older first" nil)))
          caught acks)
      (setf (niwa-agent-invocation-runtime-pending-response invocation) "answer"
            (niwa-agent-invocation-runtime-execution-results invocation) siblings)
      (cl-letf (((symbol-function 'niwa-agent-runtime--execution-live-p)
                 (lambda (_) nil))
                ((symbol-function 'niwa-agent-runtime--settle-1)
                 (lambda (&rest _) (signal condition '("Settlement failed")))))
        (condition-case err
            (niwa-agent-runtime-queue-execution-completion
             invocation "/root/explore" "retained" nil
             (lambda (success error) (push (list success error) acks)))
          ((error quit) (setq caught err))))
      (should (eq condition (car caught)))
      (should (= 1 (length acks)))
      (should-not (caar acks))
      (should (stringp (cadar acks)))
      (should (equal siblings (niwa-agent-invocation-runtime-execution-results invocation)))
      (should-not (niwa-agent-invocation-runtime-settled-p invocation))))
  :doc "a real yielded producer survives failed held settlement and retries once with its unread spool"
  (test-niwa-publication-async--fixture
   (lambda (session parent)
     (let* ((child (generate-new-buffer " *held-real-producer*"))
            (invocation (niwa-agent-runtime-test--invocation child))
            (record (niwa-agent-record--create
                     :id "explorer--test" :role "explorer" :path "/root/explore"
                     :parent-path "/root" :activity 'running :invocation invocation))
            (root (niwa-workspace-root (niwa-session-workspace session)))
            (go (file-name-concat root "go"))
            (niwa-sandbox-mode 'off)
            (publisher (symbol-function 'niwa-session-artifacts-publish-agent-terminal-state))
            (fail t)
            acks execution id)
       (unwind-protect
           (progn
             (setf (niwa-agent-invocation-parent-session invocation) session
                   (niwa-agent-invocation-parent-data-buffer invocation) parent
                   (niwa-agent-invocation-runtime-pending-response invocation) "answer"
                   (niwa-session-agent-registry session) (list (cons "/root/explore" record))
                   (niwa-agent-invocation-runtime-settle-callback invocation)
                   (lambda (inv response event token)
                     (niwa-agent-control--settle session record inv response event token))
                   (niwa-agent-invocation-frozen-configuration invocation)
                   (with-current-buffer parent
                     (niwa-agent-exec-freeze-configuration
                      "explorer" invocation (list :backend gptel-backend :model gptel-model)))
                   (niwa-agent-record-configuration record)
                   (niwa-agent-invocation-frozen-configuration invocation))
             (with-current-buffer child
               (org-mode)
               (setq-local niwa--session session niwa--agent-invocation invocation)
               (niwa-agent-runtime--setup-transcript invocation child)
               (insert "* Agent Task: explore\n\nHeld producer transcript\n"))
             (setf (niwa-agent-record-conversation-location record)
                   (niwa-agent-invocation-transcript-relative-path invocation))
             (cl-letf (((symbol-function 'niwa-session-artifacts-publish-agent-terminal-state)
                        (lambda (&rest args)
                          (when fail (error "Injected terminal publication failure"))
                          (apply publisher args))))
               (let ((niwa-execution-mailbox-delivery-function
                      (lambda (event context ack)
                        (niwa-tool-exec-handle-execution-event
                         event context
                         (lambda (success error)
                           (push (list success error) acks)
                           (funcall ack success error))))))
                 (let ((initial
                        (test-niwa-execution--start-managed
                         session root
                         (list "sh" "-c"
                               "while [ ! -e \"$1\" ]; do sleep .02; done; printf RETAIN-ME"
                               "wait" go)
                         :owner "/root/explore" :owner-context invocation)))
                   (setq id (plist-get (plist-get initial :facts) :execution-id)
                         execution (niwa-execution--owned-yielded-record
                                    session "/root/explore" id)))
                 (write-region "go" nil go nil 'silent)
                 (test-niwa-execution--wait (lambda () acks))
                 (should (equal '((nil "Injected terminal publication failure")) acks))
                 (should (eq execution (niwa-execution--owned-yielded-record
                                        session "/root/explore" id)))
                 (should-not (niwa-execution--record-delivery-state execution))
                 (should-not (niwa-execution--record-delivery-pending-p execution))
                 (should-not (niwa-execution--record-retire-timer execution))
                 (should (file-exists-p (niwa-execution--spool-path execution)))
                 (should (equal "RETAIN-ME" (niwa-execution--read-output execution)))
                 (should (= 0 (niwa-execution--record-read-offset execution)))
                 (should-not (test-niwa-publication-async--restored-mail session))
                 (setq fail nil)
                 (niwa-execution--deliver-independent execution)
                 (test-niwa-execution--wait (lambda () (= 2 (length acks))))
                 (should (equal '((t nil) (nil "Injected terminal publication failure")) acks))
                 (should (timerp (niwa-execution--record-retire-timer execution)))
                 (let ((mail (test-niwa-publication-async--restored-mail session)))
                   (should (equal '(RESULT EXECUTION)
                                  (mapcar (lambda (entry) (plist-get entry :type)) mail)))
                   (should (string-match-p "RETAIN-ME" (plist-get (cadr mail) :payload))))
                 (should (string-match-p
                          "RETAIN-ME"
                          (plist-get (test-niwa-execution--observe
                                      session id :owner "/root/explore" :wait-ms 0) :output)))
                 (niwa-execution--deliver-independent execution)
                 (should (= 2 (length acks))))))
         (niwa-execution-teardown-session session))))))

(defun test-niwa-agent-runtime--terminal-settlement-events (artifact-p)
  "Return a local portable session's terminal persistence events.
ARTIFACT-P selects whether its sidecar counts as committed."
  (let* ((save-path
          (file-name-as-directory
           (make-temp-file "niwa-agent-terminal-" t)))
         (parent (generate-new-buffer " *agent-terminal-parent*"))
         (child (generate-new-buffer " *agent-terminal-child*"))
         (target 'local-target)
         (session
          (niwa-session--create
           :name "main" :execution-target target :save-path save-path
           :authority-mode 'portable))
         (invocation (niwa-agent-runtime-test--invocation child))
         events)
    (unwind-protect
        (progn
          ;; Keep the materialized cache opposite to the authoritative
          ;; publication membership so this test detects fixed-path reads.
          (unless artifact-p
            (write-region
             "(:poisoned-cache t)" nil
             (file-name-concat save-path "session.meta.el") nil 'silent))
          (setf (niwa-agent-invocation-parent-session invocation) session
                (niwa-agent-invocation-parent-data-buffer invocation) parent
                (niwa-agent-invocation-transcript-relative-path invocation)
                "agents/explorer.chat.org"
                (niwa-agent-invocation-runtime-settle-callback invocation)
                (lambda (&rest _)
                  (push (list 'callback
                              niwa-agent-control-suppress-persistence)
                        events)
                  (when niwa-agent-control-suppress-persistence
                    (cons (lambda () (push 'rollback events))
                          (lambda () (push 'delivery events))))))
          (cl-letf (((symbol-function
                      'niwa-session-artifacts-artifact-present-p)
                     (lambda (seen-session logical)
                       (push (list 'presence logical) events)
                       (and (eq seen-session session)
                            (equal logical "session.meta.el")
                            artifact-p)))
                    ((symbol-function 'niwa-agent-runtime--finalize)
                     (lambda (inv status &optional _token)
                       (push
                        (list 'finalize
                              niwa-agent-runtime--defer-terminal-publication-p)
                        events)
                       (setf (niwa-agent-invocation-transcript-status inv)
                             status)))
                    ((symbol-function
                      'niwa-session-artifacts-publish-agent-terminal-state)
                     (lambda (&rest _)
                       (push 'publication events)
                       t))
                    ((symbol-function
                      'niwa-agent-control-commit-session)
                     (lambda (_session &optional _token)
                       (push 'commit events)
                       t)))
            (niwa-agent-runtime--settle invocation "finished"))
          (nreverse events))
      (kill-buffer child)
      (kill-buffer parent)
      (delete-directory save-path t))))

(niwa-deftest niwa-agent-runtime--setup-transcript
  ()
  ,test
  (test)
  :doc "allocates transcript names from authoritative artifact membership"
  (let* ((save-path
          (file-name-as-directory
           (make-temp-file "niwa-agent-allocation-" t)))
         (parent (generate-new-buffer " *agent-allocation-parent*"))
         (child (generate-new-buffer " *agent-allocation-child*"))
         (session (niwa-session--create :name "main" :save-path save-path))
         (invocation (niwa-agent-runtime-test--invocation child))
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
          (setf (niwa-agent-invocation-parent-session invocation) session
                (niwa-agent-invocation-parent-data-buffer invocation) parent
                (niwa-agent-invocation-parent-turn invocation) 3)
          (with-current-buffer parent
            (setq-local niwa-session--read-only-mode nil))
          (cl-letf (((symbol-function 'format-time-string)
                     (lambda (&rest _) "2026-01-02T03-04-05"))
                    ((symbol-function
                      'niwa-session-persistence-shallow-ensure-files)
                     (lambda (&rest _) save-path))
                    ((symbol-function
                      'niwa-session-artifacts-artifact-present-p)
                     (lambda (seen-session logical)
                       (should (eq seen-session session))
                       (push logical checked)
                       (equal logical first)))
                    ((symbol-function
                      'niwa-session-persistence-record-running-transcript)
                     (lambda (_session entry) (setq recorded entry))))
            (niwa-agent-runtime--setup-transcript invocation child))
          (should (equal second
                         (niwa-agent-invocation-transcript-relative-path
                          invocation)))
          (should (equal second (plist-get (cdr recorded) :path)))
          (should-not (niwa-agent-invocation-persistence-failure invocation))
          (should (equal (list first second) (nreverse checked)))
          (with-current-buffer child
            (should (equal second-fixed buffer-file-name))
            ;; Visiting would otherwise rename it to the visible file name.
            (should (equal " *agent-allocation-child*" (buffer-name)))
            (should-not (equal "poisoned cache" (buffer-string)))))
      (dolist (buffer (list child parent))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer (set-buffer-modified-p nil))
          (kill-buffer buffer)))
      (delete-directory save-path t)))

  :doc "records why no transcript was created instead of failing silently"
  (let* ((parent (generate-new-buffer " *agent-allocation-parent*"))
         (child (generate-new-buffer " *agent-allocation-child*"))
         (session (niwa-session--create :name "main"))
         (invocation (niwa-agent-runtime-test--invocation child))
         (reason (lambda ()
                   (niwa-agent-runtime--setup-transcript invocation child)
                   (niwa-agent-invocation-persistence-failure invocation))))
    (unwind-protect
        (progn
          (setf (niwa-agent-invocation-parent-session invocation) session
                (niwa-agent-invocation-parent-data-buffer invocation) parent)
          (with-current-buffer parent
            (setq-local niwa-session--read-only-mode t))
          (should (equal "transcript setup: parent session is read-only"
                         (funcall reason)))
          (with-current-buffer parent
            (setq-local niwa-session--read-only-mode nil))
          (let (message)
            (cl-letf (((symbol-function
                        'niwa-session-persistence-shallow-ensure-files)
                       (lambda (&rest _) (error "Lease is busy"))))
              (niwa-test--with-captured-diagnostics message
                (should (equal "transcript setup: Lease is busy"
                               (funcall reason)))))
            (should (string-search "Lease is busy" message)))
          (cl-letf (((symbol-function
                      'niwa-session-persistence-shallow-ensure-files)
                     #'ignore))
            (should (equal "transcript setup: session files could not be created"
                           (funcall reason))))
          (should-not (niwa-agent-invocation-transcript-relative-path
                       invocation)))
      (kill-buffer child)
      (kill-buffer parent))))

(niwa-deftest niwa-agent-runtime--release-settled-state ()
  ,test
  (test)
  :doc "drops the finished request payload but spares a newer turn's buffer state"
  (let ((buffer (generate-new-buffer " *agent-runtime-release*"))
        (old (niwa-agent-runtime-test--invocation))
        (new (niwa-agent-runtime-test--invocation)))
    (unwind-protect
        (progn
          (setf (niwa-agent-invocation-buffer old) buffer
                (niwa-agent-invocation-buffer new) buffer)
          (with-current-buffer buffer
            (setq-local niwa--agent-invocation old)
            (setq-local gptel--fsm-last
                        (gptel-make-fsm :info (list :data 'old-payload))))
          (niwa-agent-runtime--release-settled-state old)
          (with-current-buffer buffer
            (should-not (plist-get (gptel-fsm-info gptel--fsm-last) :data))
            ;; A retried settlement of OLD after NEW started there.
            (setq-local niwa--agent-invocation new)
            (setq-local gptel--fsm-last
                        (gptel-make-fsm :info (list :data 'new-payload))))
          (niwa-agent-runtime--release-settled-state old)
          (with-current-buffer buffer
            (should (eq 'new-payload
                        (plist-get (gptel-fsm-info gptel--fsm-last) :data)))))
      (kill-buffer buffer))))

(niwa-deftest niwa-agent-runtime--settle (:quiet t)
  ,test
  (test)
  :doc "successful retained-idle settlement closes owned tasks before parent RESULT"
  (let* ((session (niwa-session--create))
         (invocation (niwa-agent-runtime-test--invocation))
         (record (niwa-agent-record--create
                  :path "/root/explore" :parent-path "/root"
                  :activity 'running :invocation invocation)))
    (setf (niwa-session-agent-registry session)
          (list (cons "/root/explore" record))
          (niwa-agent-invocation-parent-session invocation) session
          (niwa-agent-invocation-runtime-settle-callback invocation)
          (lambda (inv response event &optional token)
            (niwa-agent-control--settle session record inv response event token)))
    (with-temp-buffer
      (setq-local niwa--session session)
      (let ((niwa--agent-invocation invocation))
        (niwa-tool-task--handle-write
         '(:tasks [(:subject "Investigation" :status "in_progress")
                   (:subject "Verification")]
           :note "Checking")))
      (niwa-tool-task--handle-write
       '(:tasks [(:subject "Parent follow-up" :blockedBy [1 2])]))
      ;; Exercise the real runtime and control settlement, not a direct call
      ;; to task finalization.  Retained idle is the public completion state.
      (should (equal "Done" (niwa-agent-runtime--settle invocation "Done")))
      (should (eq 'idle (niwa-agent-record-activity record)))
      (should-not (niwa-agent-record-invocation record))
      (should (niwa-agent-invocation-runtime-settled-p invocation))
      (should (equal '(completed completed pending)
                     (mapcar #'niwa-task-status (niwa-session-tasks session))))
      (should-not (niwa-task-blocked-by (nth 2 (niwa-session-tasks session))))
      (should-not (niwa-session-task-status-notes session))
      (let ((message (car (niwa-agent-control-context-mailbox session))))
        (should (eq 'RESULT (plist-get message :type)))
        (should (equal "/root/explore" (plist-get message :sender)))
        (should (equal "Done" (plist-get message :payload))))))

  :doc "failed or interrupted retained-idle settlement keeps owned work recoverable"
  (dolist (status '(error aborted))
    (let* ((session (niwa-session--create))
           (invocation (niwa-agent-runtime-test--invocation))
           (record (niwa-agent-record--create
                    :path "/root/explore" :parent-path "/root"
                    :activity 'running :invocation invocation)))
      (setf (niwa-session-agent-registry session)
            (list (cons "/root/explore" record))
            (niwa-agent-invocation-parent-session invocation) session
            (niwa-agent-invocation-runtime-settle-callback invocation)
            (lambda (inv response event &optional token)
              (niwa-agent-control--settle session record inv response event token)))
      (with-temp-buffer
        (setq-local niwa--session session)
        (let ((niwa--agent-invocation invocation))
          (niwa-tool-task--handle-write
           '(:tasks [(:subject "Unfinished investigation" :status "in_progress")]
             :note "Still checking")))
        (niwa-agent-runtime--settle
         invocation nil (list :niwa-agent-terminal-status status
                              :error-details "Injected failure"))
        (should (eq 'idle (niwa-agent-record-activity record)))
        (should (eq 'in-progress
                    (niwa-task-status (car (niwa-session-tasks session)))))
        (should (equal "Still checking"
                       (niwa-tool-task--status-note session "/root/explore")))
        (let ((message (car (niwa-agent-control-context-mailbox session))))
          (should (eq 'RESULT (plist-get message :type)))
          (should (equal "/root/explore" (plist-get message :sender)))
          (should (eq (if (eq status 'error) 'errored 'interrupted)
                      (plist-get message :outcome)))))))

  :doc "batches only materialized portable terminal state after registry update"
  (should
   (equal '((presence "session.meta.el")
            (finalize t) (callback t) publication delivery)
          (test-niwa-agent-runtime--terminal-settlement-events t)))
  (should
   (equal '((presence "session.meta.el")
            (finalize nil) (callback t) commit delivery)
          (test-niwa-agent-runtime--terminal-settlement-events nil)))

  :doc "a quit during portable publication rolls staged control state back"
  (let* ((session (niwa-session--create
                   :name "main" :save-path temporary-file-directory
                   :execution-target 'local :authority-mode 'portable))
         (invocation (niwa-agent-runtime-test--invocation))
         events)
    (setf (niwa-agent-invocation-parent-session invocation) session
          (niwa-agent-invocation-parent-data-buffer invocation)
          (current-buffer)
          (niwa-agent-invocation-buffer invocation) (current-buffer)
          (niwa-agent-invocation-transcript-relative-path invocation)
          "agents/test.chat.org"
          (niwa-agent-invocation-runtime-settle-callback invocation)
          (lambda (&rest _)
            (cons (lambda () (push 'rollback events))
                  (lambda () (push 'delivery events)))))
    (cl-letf (((symbol-function
                'niwa-session-artifacts-artifact-present-p)
               (lambda (&rest _) t))
              ((symbol-function 'niwa-agent-runtime--finalize) #'ignore)
              ((symbol-function
                'niwa-session-artifacts-publish-agent-terminal-state)
               (lambda (&rest _) (signal 'quit nil)))
              ((symbol-function 'niwa-agent-control-settlement-finished)
               (lambda (&rest _) (push 'finished events))))
      (should (equal '(quit)
                     (condition-case err
                         (progn (niwa-agent-runtime--settle invocation "done")
                                nil)
                       (quit err))))
      (should (equal '(rollback finished) (nreverse events)))))

  :doc "portable publication failure rolls staged control state back"
  (let* ((session (niwa-session--create
                   :name "main" :save-path temporary-file-directory
                   :execution-target 'local :authority-mode 'portable))
         (invocation (niwa-agent-runtime-test--invocation))
         events)
    (setf (niwa-agent-invocation-parent-session invocation) session
          (niwa-agent-invocation-parent-data-buffer invocation)
          (current-buffer)
          (niwa-agent-invocation-buffer invocation) (current-buffer)
          (niwa-agent-invocation-transcript-relative-path invocation)
          "agents/test.chat.org"
          (niwa-agent-invocation-runtime-settle-callback invocation)
          (lambda (&rest _)
            (push 'callback events)
            (cons (lambda () (push 'rollback events))
                  (lambda () (push 'delivery events)))))
    (cl-letf (((symbol-function
                'niwa-session-artifacts-artifact-present-p)
               (lambda (&rest _) t))
              ((symbol-function 'niwa-agent-runtime--finalize)
               (lambda (&rest _) (push 'finalize events)))
              ((symbol-function
                'niwa-session-artifacts-publish-agent-terminal-state)
               (lambda (&rest _)
                 (push 'publication events)
                 (error "Commit unavailable")))
              ((symbol-function 'niwa-agent-control-settlement-finished)
               (lambda (seen-session)
                 (should (eq session seen-session))
                 (push 'finished events))))
      (should-error
       (niwa-agent-runtime--settle invocation "finished"))
      ;; Even a failed settlement stops blocking new turns in the tree.
      (should (equal '(finalize callback publication rollback finished)
                     (nreverse events)))
      (should-not
       (niwa-agent-invocation-runtime-settled-p invocation))
      (should
       (niwa-agent-invocation-runtime-pending-response invocation))))

  :doc "a nested settlement of the same turn leaves the report to the outer call"
  (let* ((session (niwa-session--create :name "main"))
         (invocation (niwa-agent-runtime-test--invocation))
         (finished nil)
         (nested nil))
    (setf (niwa-agent-invocation-parent-session invocation) session
          (niwa-agent-invocation-runtime-settle-callback invocation)
          (lambda (&rest _)
            (unless nested
              (setq nested t)
              ;; A terminal timer fires while the outer call is staging.
              (should (memq invocation niwa-agent-control-settling))
              (niwa-agent-runtime--settle invocation "again")
              (should-not finished))
            nil))
    (cl-letf (((symbol-function 'niwa-agent-runtime--finalize) #'ignore)
              ((symbol-function 'niwa-agent-control-settlement-finished)
               (lambda (_session) (push t finished))))
      (niwa-agent-runtime--settle invocation "finished"))
    (should nested)
    (should-not niwa-agent-control-settling)
    (should (equal '(t) finished)))

  :doc "portable delivery retries without repeating its committed publication"
  (let* ((session (niwa-session--create
                   :name "main" :save-path temporary-file-directory
                   :execution-target 'local :authority-mode 'portable))
         (invocation (niwa-agent-runtime-test--invocation))
         (callbacks 0)
         (publications 0)
         (deliveries 0))
    (setf (niwa-agent-invocation-parent-session invocation) session
          (niwa-agent-invocation-parent-data-buffer invocation)
          (current-buffer)
          (niwa-agent-invocation-buffer invocation) (current-buffer)
          (niwa-agent-invocation-transcript-relative-path invocation)
          "agents/test.chat.org"
          (niwa-agent-invocation-runtime-settle-callback invocation)
          (lambda (&rest _)
            (cl-incf callbacks)
            (cons #'ignore
                  (lambda ()
                    (cl-incf deliveries)
                    (when (= deliveries 1)
                      (error "Delivery unavailable"))))))
    (cl-letf (((symbol-function
                'niwa-session-artifacts-artifact-present-p)
               (lambda (&rest _) t))
              ((symbol-function 'niwa-agent-runtime--finalize) #'ignore)
              ((symbol-function
                'niwa-session-artifacts-publish-agent-terminal-state)
               (lambda (&rest _)
                 (cl-incf publications)
                 t)))
      (should-error
       (niwa-agent-runtime--settle invocation "finished"))
      (should-not
       (niwa-agent-invocation-runtime-settled-p invocation))
      (should
       (eq 'committed
           (plist-get
            (niwa-agent-invocation-runtime-pending-response invocation)
            :phase)))
      (should
       (equal "finished"
              (niwa-agent-runtime--settle
               invocation "ignored duplicate"
               '(:niwa-agent-terminal-status error))))
      (should
       (niwa-agent-invocation-runtime-settled-p invocation))
      (should (= 1 callbacks))
      (should (= 1 publications))
      (should (= 2 deliveries))))

  :doc "releases the settled buffer's request payload and decoding memo"
  (with-temp-buffer
    (let* ((session (niwa-session--create :name "main"))
           (invocation (niwa-agent-runtime-test--invocation (current-buffer)))
           (marker (point-marker)))
      (setq-local gptel--fsm-last
                  (gptel-make-fsm :info (list :data '(:messages ["long"])
                                              :position marker)))
      (setq-local niwa-transcript-audit--buffer-records
                  (make-hash-table :test #'equal))
      (setq-local niwa-transcript-audit--buffer-record-bytes 42)
      (setf (niwa-agent-invocation-parent-session invocation) session
            (niwa-agent-invocation-parent-data-buffer invocation)
            (current-buffer)
            (niwa-agent-invocation-runtime-settle-callback invocation)
            (lambda (&rest _) nil))
      (setq-local niwa--agent-invocation invocation)
      (cl-letf (((symbol-function 'niwa-agent-runtime--finalize) #'ignore))
        (should (equal "finished"
                       (niwa-agent-runtime--settle invocation "finished"))))
      (should (niwa-agent-invocation-runtime-settled-p invocation))
      ;; Markers a continuation reads survive; the payload does not.
      (should-not (plist-get (gptel-fsm-info gptel--fsm-last) :data))
      (should (eq marker (plist-get (gptel-fsm-info gptel--fsm-last) :position)))
      (should-not niwa-transcript-audit--buffer-records)
      (should (= 0 niwa-transcript-audit--buffer-record-bytes)))))

(niwa-deftest niwa-agent-runtime--transcript-path
  (:doc "returns a published remote transcript's qualified logical path")
  (let* ((save-path "/ssh:fixture:/srv/project/.niwa/sessions/main/")
         (session (niwa-session--create :name "main" :save-path save-path))
         (invocation (niwa-agent-runtime-test--invocation))
         seen)
    (setf (niwa-agent-invocation-parent-session invocation) session
          (niwa-agent-invocation-transcript-relative-path invocation)
          "agents/explorer.chat.org")
    (cl-letf (((symbol-function
                'niwa-session-artifacts-artifact-present-p)
               (lambda (seen-session logical)
                 (setq seen (list seen-session logical))
                 t)))
      (should
       (equal (concat save-path "agents/explorer.chat.org")
              (niwa-agent-runtime--transcript-path invocation)))
      (should (equal (list session "agents/explorer.chat.org") seen))
      (cl-letf (((symbol-function
                  'niwa-session-artifacts-artifact-present-p)
                 (lambda (&rest _) nil)))
        (should-not
         (niwa-agent-runtime--transcript-path invocation))))))

(niwa-deftest niwa-agent-runtime-bound-turn ()
  ,test
  (test)
  :doc "cancels the deadline when a bounded turn completes in time"
  (let* ((invocation (niwa-agent-runtime-test--invocation))
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
              ((symbol-function 'niwa-agent-runtime--finalize) #'ignore))
      (niwa-agent-runtime-bound-turn invocation 30)
      (niwa-agent-runtime--settle invocation "done")
      (should (eq 'budget-timer cancelled))
      (should-not
       (niwa-agent-invocation-runtime-budget-timer invocation))
      (apply timer-callback timer-args)
      (should
       (niwa-agent-invocation-runtime-settled-p invocation))))

  :doc "budget expiry interrupts once and preserves useful partial output"
  (let* ((buffer (generate-new-buffer " *agent-runtime-budget*"))
         (invocation (niwa-agent-runtime-test--invocation buffer))
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
          (setf (niwa-agent-invocation-runtime-fsm invocation) fsm
                (niwa-agent-invocation-runtime-settle-callback invocation)
                (lambda (_invocation response _event &optional _token)
                  (setq callback-response response)))
          (cl-letf (((symbol-function 'run-at-time)
                     (lambda (_seconds _repeat callback &rest args)
                       (setq timer-callback callback timer-args args)
                       'budget-timer))
                    ((symbol-function 'niwa-agent-runtime--partial-text)
                     (lambda (&rest _) "bounded useful evidence"))
                    ((symbol-function 'niwa-agent-runtime--finalize)
                     (lambda (inv status &optional _token)
                       (setf (niwa-agent-invocation-transcript-status inv)
                             status)))
                    ((symbol-function 'gptel-abort)
                     (lambda (_buffer)
                       (setq abort-count (1+ (or abort-count 0))))))
            (niwa-agent-runtime-bound-turn invocation 20)
            (apply timer-callback timer-args))
          (should (= 1 abort-count))
          (should (string-match-p "budget expired after 20 seconds"
                                  callback-response))
          (should (string-match-p "bounded useful evidence"
                                  callback-response))
          (should (eq 'aborted
                      (niwa-agent-invocation-transcript-status invocation)))
          (apply timer-callback timer-args)
          (should (= 1 abort-count)))
      (kill-buffer buffer))))

(niwa-deftest niwa-agent-runtime--finalize ()
  ,test
  (test)
  :doc "request teardown failure leaves the turn active and retryable"
  (let* ((child (generate-new-buffer " *agent-finalize-child*"))
         (parent (generate-new-buffer " *agent-finalize-parent*"))
         (session (niwa-session--create :name "main"))
         (invocation (niwa-agent-runtime-test--invocation child))
         calls
         warnings)
    (unwind-protect
        (progn
          (setf (niwa-agent-invocation-parent-session invocation) session
                (niwa-agent-invocation-parent-data-buffer invocation) parent
                (niwa-agent-invocation-activity invocation) 'working)
          (cl-letf (((symbol-function 'niwa-request-end)
                     (lambda (&rest _) (error "Injected request failure")))
                    ((symbol-function 'niwa-execution-stop-owner)
                     (lambda (&rest _) (push 'executions calls)))
                    ((symbol-function
                      'niwa-session-persistence-update-transcript-entry)
                     (lambda (&rest _) (push 'transcript calls)))
                    ((symbol-function
                      'niwa-agent-conversation-save)
                     (lambda (&rest _) (push 'save calls)))
                    ((symbol-function 'niwa-agent-conversation-record-activity)
                     (lambda (&rest _) (push 'activity calls)))
                    ((symbol-function 'niwa-tool-task-finalize-owner)
                     (lambda (&rest _) (push 'tasks calls) t))
                    ((symbol-function 'niwa-tool-task-refresh-display)
                     (lambda () (push 'task-view calls)))
                    ((symbol-function 'niwa-agent-conversation-refresh)
                     (lambda (&rest _) (push 'handle calls)))
                    ((symbol-function
                      'niwa-session-persistence-write-sidecar-now)
                     (lambda (&rest _) (push 'sidecar calls)))
                    ((symbol-function 'niwa-agent-runtime--run-stop-hook)
                     (lambda (&rest _) (push 'hook calls)))
                    ((symbol-function
                      'niwa-view-agent-live-transcript-finalize)
                     (lambda (&rest _) (push 'view calls)))
                    ((symbol-function 'display-warning)
                     (lambda (&rest args) (push args warnings))))
            (should-error
             (niwa-agent-runtime--finalize invocation 'completed)))
          (should-not calls)
          (should-not warnings)
          (should (eq 'working
                      (niwa-agent-invocation-activity invocation)))
          (should (eq 'running
                      (niwa-agent-invocation-transcript-status invocation))))
      (kill-buffer child)
      (kill-buffer parent)))

  :doc "execution teardown failure cannot publish later terminal effects"
  (let* ((child (generate-new-buffer " *agent-finalize-execution-child*"))
         (session (niwa-session--create :name "main"))
         (invocation (niwa-agent-runtime-test--invocation child))
         calls)
    (unwind-protect
        (progn
          (setf (niwa-agent-invocation-parent-session invocation) session
                (niwa-agent-invocation-activity invocation) 'working)
          (cl-letf (((symbol-function 'niwa-request-end)
                     (lambda (&rest _) (push 'request calls)))
                    ((symbol-function 'niwa-execution-stop-owner)
                     (lambda (&rest _)
                       (error "Injected execution failure")))
                    ((symbol-function
                      'niwa-session-persistence-update-transcript-entry)
                     (lambda (&rest _) (push 'transcript calls))))
            (should-error
             (niwa-agent-runtime--finalize invocation 'completed)))
          (should (equal '(request) calls))
          (should (eq 'running
                      (niwa-agent-invocation-transcript-status invocation))))
      (kill-buffer child))))

(niwa-deftest niwa-agent-runtime-interrupt
  ()
  ,test
  (test)
  :doc "aborts a live provider turn and settles useful partial output once"
  (let* ((buffer (generate-new-buffer " *agent-runtime-interrupt*"))
         (invocation (niwa-agent-runtime-test--invocation buffer))
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
          (setf (niwa-agent-invocation-runtime-fsm invocation) fsm
                (niwa-agent-invocation-runtime-settle-callback invocation)
                (lambda (_invocation response _event &optional _token)
                  (setq callback-count (1+ (or callback-count 0))
                        callback-response response)))
          (cl-letf
              (((symbol-function 'niwa-agent-runtime--partial-text)
                (lambda (&rest _) "useful partial work"))
               ((symbol-function 'niwa-agent-runtime--finalize)
                (lambda (inv status &optional _token)
                  (setf (niwa-agent-invocation-transcript-status inv)
                        status)))
               ((symbol-function 'gptel-abort)
                (lambda (_buffer)
                  (setq abort-count (1+ (or abort-count 0)))
                  (should
                   (eq #'ignore
                       (plist-get (gptel-fsm-info fsm) :callback))))))
            (let ((response
                   (niwa-agent-runtime-interrupt
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
                        (niwa-agent-invocation-transcript-status
                         invocation)))
            (niwa-agent-runtime-interrupt invocation "duplicate")
            (should (= 1 callback-count))))
      (kill-buffer buffer)))

  :doc "lets a native cancellation acknowledgement own asynchronous settlement"
  (let* ((buffer (generate-new-buffer " *agent-runtime-deferred-abort*"))
         (invocation (niwa-agent-runtime-test--invocation buffer))
         (cancel-count 0) settled)
    (unwind-protect
        (progn
          (setf (niwa-agent-invocation-runtime-cancel invocation)
                (lambda () (cl-incf cancel-count) 'deferred))
          (cl-letf (((symbol-function 'niwa-agent-runtime--settle)
                     (lambda (&rest _) (setq settled t))))
            (should (string-match-p "stop" (niwa-agent-runtime-interrupt invocation "stop")))
            (niwa-agent-runtime-interrupt invocation "stop again")
            (should (= 2 cancel-count))
            (should-not settled)
            (should-not (niwa-agent-invocation-runtime-settled-p invocation))))
      (kill-buffer buffer)))

  :doc "mail queued before the interrupt no longer wakes; later mail does"
  (let* ((buffer (generate-new-buffer " *agent-runtime-interrupt-wakes*"))
         (invocation (niwa-agent-runtime-test--invocation buffer))
         (session (niwa-session--create :name "main"))
         (before (list :type 'MAIL :sender "/root" :payload "before" :wake t))
         (record (niwa-agent-record--create
                  :path "/root/explore" :activity 'running
                  :invocation invocation :mailbox (list before))))
    (setf (niwa-agent-invocation-parent-session invocation) session
          (niwa-session-agent-registry session)
          (list (cons "/root/explore" record))
          (niwa-agent-invocation-runtime-cancel invocation)
          (lambda () 'deferred))
    (unwind-protect
        (progn
          (niwa-agent-runtime-interrupt invocation "stop")
          (should-not (plist-get before :wake))
          (let ((after (list :type 'MAIL :sender "/root" :payload "after"
                             :wake t)))
            (push after (niwa-agent-record-mailbox record))
            ;; The cancellation acknowledgement settles later; mail sent
            ;; after the interrupt keeps its mark until then.
            (should (plist-get after :wake))))
      (kill-buffer buffer)))

  :doc "leaves a turn unsettled when provider abort fails"
  (let* ((buffer (generate-new-buffer " *agent-runtime-abort-fail*"))
         (invocation (niwa-agent-runtime-test--invocation buffer))
         (fsm (gptel-make-fsm
               :state 'WAIT
               :info (list :buffer buffer :callback #'identity)))
         (gptel--request-alist
          (list (cons 'process (cons fsm #'ignore))))
         settled)
    (unwind-protect
        (progn
          (setf (niwa-agent-invocation-runtime-fsm invocation) fsm
                (niwa-agent-invocation-runtime-settle-callback invocation)
                (lambda (&rest _) (setq settled t)))
          (cl-letf (((symbol-function 'gptel-abort)
                     (lambda (_buffer) (error "Provider abort failed"))))
            (should
             (equal '(error "Provider abort failed")
                    (should-error
                     (niwa-agent-runtime-interrupt invocation "stop")))))
          (should (eq #'identity (plist-get (gptel-fsm-info fsm) :callback)))
          (should-not settled)
          (should-not
           (niwa-agent-invocation-runtime-settled-p invocation))
          (should-not
           (niwa-agent-invocation-terminal-reason invocation)))
      (kill-buffer buffer)))

  :doc "stops real yielded Bash on interrupt but leaves uninterrupted work running"
  (dolist (interrupt-p '(nil t))
    (let* ((root (make-temp-file "niwa-agent-interrupt-yielded-" t))
           (buffer (generate-new-buffer " *agent-interrupt-yielded*"))
           (session (test-niwa-execution--session root))
           (invocation (niwa-agent-runtime-test--invocation buffer))
           (pid-file (file-name-concat root "sleep.pid"))
           (owner (niwa-agent-invocation-require-path invocation))
           (niwa-sandbox-mode 'off)
           pid)
      (unwind-protect
          (progn
            (setf (niwa-agent-invocation-parent-session invocation) session)
            (let* ((initial
                    (test-niwa-execution--start-managed
                     session root
                     (list "sh" "-c" "printf '%s' \"$$\" > \"$1\"; exec sleep 600"
                           "agent-yielded" pid-file)
                     :owner owner :owner-context invocation :data-buffer buffer))
                   (facts (plist-get initial :facts))
                   (id (plist-get facts :execution-id)))
              (should (eq 'running (plist-get facts :state)))
              (should (niwa-execution--owned-yielded-record session owner id)))
            (test-niwa-execution--wait
             (lambda ()
               (and (file-exists-p pid-file)
                    (> (setq pid (test-niwa-execution--read-pid pid-file)) 0))))
            (should-not (test-niwa-execution--process-gone-p pid))
            (if interrupt-p
                (progn
                  (cl-letf (((symbol-function
                             'niwa-session-persistence-update-transcript-entry)
                            #'ignore)
                           ((symbol-function 'niwa-agent-conversation-save)
                            #'ignore)
                           ((symbol-function
                             'niwa-agent-conversation-record-activity)
                            #'ignore)
                           ((symbol-function 'niwa-agent-conversation-refresh)
                            #'ignore)
                           ((symbol-function
                             'niwa-session-persistence-write-sidecar-now)
                            #'ignore)
                           ((symbol-function 'niwa-agent-runtime--run-stop-hook)
                            #'ignore))
                    (niwa-agent-runtime-interrupt invocation "stop yielded Bash"))
                  (with-timeout (5 nil)
                    (while (not (test-niwa-execution--process-gone-p pid))
                      (accept-process-output nil 0.02)))
                  (should (test-niwa-execution--process-gone-p pid))
                  (should (niwa-agent-invocation-runtime-settled-p invocation))
                  (should (eq 'aborted
                              (niwa-agent-invocation-transcript-status invocation))))
              (accept-process-output nil 0.05)
              (should-not (test-niwa-execution--process-gone-p pid))
              (should-not (niwa-agent-invocation-runtime-settled-p invocation))))
        (niwa-execution-teardown-session session)
        (when pid
          (test-niwa-execution--wait
           (lambda () (test-niwa-execution--process-gone-p pid))))
        (niwa-session-persistence-lock-release
         (niwa-session-save-path session) session)
        (when (buffer-live-p buffer) (kill-buffer buffer))
        (delete-directory root t))))

  :doc "settles active and queued parallel Bash exactly once on interruption"
  (let* ((root (make-temp-file "niwa-agent-interrupt-bash-" t))
         (child (generate-new-buffer " *agent-interrupt-bash*"))
         (parent (generate-new-buffer " *agent-interrupt-parent*"))
         (session
          (niwa-session-create
           "main"
           (niwa-workspace--create
            :type 'file :id root :root root :name "interrupt")
           root))
         (invocation (niwa-agent-runtime-test--invocation child))
         (active-path (file-name-concat root "active"))
         (queued-one-path (file-name-concat root "queued-one"))
         (queued-two-path (file-name-concat root "queued-two"))
         (artifact-directory (file-name-concat root "artifacts"))
         (niwa-sandbox-mode 'off)
         callbacks
         settled)
    (unwind-protect
        (progn
          (setf (niwa-agent-invocation-parent-session invocation) session
                (niwa-agent-invocation-parent-data-buffer invocation) parent
                (niwa-agent-invocation-runtime-settle-callback invocation)
                (lambda (&rest _)
                  (setq settled (1+ (or settled 0)))))
          (with-current-buffer parent
            (setq-local niwa--session session))
          (niwa-session-control-transfer-register-root-buffer session parent)
          (with-current-buffer child
            (setq-local niwa--session session)
            (setq-local niwa--agent-invocation invocation)
            (let ((request (niwa-request-begin session)))
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
                  (niwa-execution-start-bash
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
                      'niwa-session-persistence-update-transcript-entry)
                     #'ignore)
                    ((symbol-function 'niwa-agent-conversation-save)
                     #'ignore)
                    ((symbol-function
                      'niwa-agent-conversation-record-activity)
                     #'ignore)
                    ((symbol-function 'niwa-agent-conversation-refresh)
                     #'ignore)
                    ((symbol-function
                      'niwa-session-persistence-write-sidecar-now)
                     #'ignore)
                    ((symbol-function 'niwa-agent-runtime--run-stop-hook)
                     #'ignore))
            (niwa-agent-runtime-interrupt invocation "stop batch"))
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
           (niwa-execution-owner-live-p session "/root/explore")))
      (niwa-execution-teardown-session session)
      (niwa-session-persistence-lock-release
       (niwa-session-save-path session) session)
      (when (buffer-live-p child) (kill-buffer child))
      (when (buffer-live-p parent) (kill-buffer parent))
      (delete-directory root t))))

(niwa-deftest niwa-agent-runtime--defer-settlement ()
  ,test
  (test)
  :doc "a held publication gate queues settlement and its run receives a fresh token"
  (let* ((session (niwa-session--create))
         (invocation (niwa-agent-runtime-test--invocation))
         (buffer (generate-new-buffer " *settle-gate*"))
         queued settled gate-held)
    (setf (niwa-agent-invocation-parent-session invocation) session)
    (unwind-protect
        (cl-letf (((symbol-function 'niwa-session-publication-defer)
                   (lambda (_session preparation &optional failure token)
                     (when (and gate-held (not token))
                       (setq queued (cons preparation failure))
                       t)))
                  ((symbol-function 'niwa-agent-runtime--settle-1)
                   (lambda (_invocation response event token)
                     (push (list response event (current-buffer) token)
                           settled)))
                  ((symbol-function 'niwa-agent-control-settlement-finished)
                   #'ignore))
          (setq gate-held t)
          (with-current-buffer buffer
            (niwa-agent-runtime--settle invocation "answer" '(:event)))
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
  (let* ((session (niwa-session--create))
         (invocation (niwa-agent-runtime-test--invocation))
         settled)
    (setf (niwa-agent-invocation-parent-session invocation) session)
    (cl-letf (((symbol-function 'niwa-session-publication-defer)
               (lambda (&rest _) nil))
              ((symbol-function 'niwa-agent-runtime--settle-1)
               (lambda (_invocation response _event token)
                 (push (list response token) settled)))
              ((symbol-function 'niwa-agent-control-settlement-finished)
               #'ignore))
      (niwa-agent-runtime--settle invocation "answer" nil 'owned))
    (should (equal '(("answer" owned)) settled)))
  :doc "the token is forwarded to the publication owner's defer decision"
  (let* ((session (niwa-session--create))
         (invocation (niwa-agent-runtime-test--invocation))
         seen)
    (setf (niwa-agent-invocation-parent-session invocation) session)
    (cl-letf (((symbol-function 'niwa-session-publication-defer)
               (lambda (_session _preparation &optional _failure token)
                 (setq seen token)
                 nil))
              ((symbol-function 'niwa-agent-runtime--settle-1)
               #'ignore)
              ((symbol-function 'niwa-agent-control-settlement-finished)
               #'ignore))
      (niwa-agent-runtime--settle invocation "answer" nil 'owned))
    (should (eq 'owned seen)))
  :doc "a nested settlement of the same turn is never deferred"
  (let* ((session (niwa-session--create))
         (invocation (niwa-agent-runtime-test--invocation))
         (niwa-agent-control-settling (list invocation))
         settled)
    (setf (niwa-agent-invocation-parent-session invocation) session)
    (cl-letf (((symbol-function 'niwa-session-publication-defer)
               (lambda (&rest _) (error "Must not defer")))
              ((symbol-function 'niwa-agent-runtime--settle-1)
               (lambda (_invocation response _event _token)
                 (push response settled))))
      (niwa-agent-runtime--settle invocation "inner"))
    (should (equal '("inner") settled)))
  :doc "a deferred settlement reports no response to its caller"
  (let* ((session (niwa-session--create))
         (invocation (niwa-agent-runtime-test--invocation)))
    (setf (niwa-agent-invocation-parent-session invocation) session)
    (cl-letf (((symbol-function 'niwa-session-publication-defer)
               (lambda (&rest _) t)))
      (should-not (niwa-agent-runtime--settle invocation "answer"))))
  :doc "a queued failure keeps its workspace after the origin buffer dies"
  (let* ((workspace (niwa-workspace--create :root "/project/origin/"))
         (session (niwa-session--create :workspace workspace))
         (invocation (niwa-agent-runtime-test--invocation))
         (origin (generate-new-buffer " *agent diagnostic origin*"))
         queued warning)
    (setf (niwa-agent-invocation-parent-session invocation) session)
    (cl-letf (((symbol-function 'niwa-session-publication-defer)
               (lambda (_session preparation &optional failure token)
                 (ignore token)
                 (setq queued (cons preparation failure))
                 t)))
      (unwind-protect
          (with-current-buffer origin
            (setq-local niwa--session session)
            (niwa-agent-runtime--settle invocation "answer"))
        (kill-buffer origin)))
    (with-temp-buffer
      (setq-local niwa--session
                  (niwa-session--create
                   :workspace (niwa-workspace--create :root "/project/other/")))
      (cl-letf (((symbol-function 'display-warning)
                 (lambda (_type message &optional _level _buffer-name)
                   (setq warning message))))
        (funcall (cdr queued) '(error "Disk full"))))
    (should (string-prefix-p "niwa [/project/origin/]: " warning))
    (should (string-match-p "/root/explore.*Disk full" warning))
    (should-not (niwa-agent-invocation-runtime-settled-p invocation)))
  :doc "raw deferred acknowledgements run once on precommit and postcommit errors"
  (dolist (committed '(nil t))
    (let* ((session (niwa-session--create))
           (invocation (niwa-agent-runtime-test--invocation))
           queued acks)
      (setf (niwa-agent-invocation-parent-session invocation) session)
      (niwa-test--with-captured-diagnostics nil
        (cl-letf (((symbol-function 'niwa-session-publication-defer)
                   (lambda (_session preparation &optional failure token)
                     (unless token (setq queued (cons preparation failure)) t)))
                  ((symbol-function 'niwa-agent-runtime--settle-1)
                   (lambda (&rest _)
                     (when committed
                       (setf (niwa-agent-invocation-runtime-pending-response invocation)
                             '(:phase committed)))
                     (error "Deferred step failed"))))
          (should
           (niwa-agent-runtime--defer-settlement
            invocation "answer" nil nil
            (lambda (success error) (push (list success error) acks))))
          (should-not acks)
          (condition-case err
              (funcall (car queued) 'fresh)
            (error (funcall (cdr queued) err)))
          (funcall (cdr queued) '(error "Repeated teardown failure"))))
      (should (equal (if committed '((t nil)) '((nil "Deferred step failed"))) acks)))))

(niwa-deftest niwa-agent-runtime--settle-1/token ()
  ,test
  (test)
  :doc "the gate token reaches finalization, the settle callback and the control commit"
  (let* ((session (niwa-session--create))
         (invocation (niwa-agent-runtime-test--invocation))
         seen)
    (setf (niwa-agent-invocation-parent-session invocation) session
          (niwa-agent-invocation-runtime-settle-callback invocation)
          (lambda (_invocation _visible _event token)
            (push (cons 'callback token) seen)
            (list 'transaction)))
    (cl-letf (((symbol-function 'niwa-agent-runtime--finalize)
               (lambda (_invocation _status &optional token)
                 (push (cons 'finalize token) seen)))
              ((symbol-function 'niwa-agent-control-commit-session)
               (lambda (_session &optional token) (push (cons 'commit token) seen))))
      (niwa-agent-runtime--settle-1 invocation "answer" nil 'owned))
    (should (equal '((commit . owned) (callback . owned) (finalize . owned))
                   seen))))

(niwa-deftest niwa-agent-runtime-interrupt/gate ()
  ,test
  (test)
  :doc "a held gate defers abort and settlement together and still returns the response"
  (let* ((session (niwa-session--create))
         (invocation (niwa-agent-runtime-test--invocation))
         queued log gate-held)
    (setf (niwa-agent-invocation-parent-session invocation) session
          (niwa-agent-invocation-runtime-cancel invocation)
          (lambda () (push 'cancel log) nil))
    (cl-letf (((symbol-function 'niwa-session-publication-defer)
               (lambda (_session preparation &optional _failure token)
                 (when (and gate-held (not token))
                   (setq queued preparation)
                   t)))
              ((symbol-function 'niwa-agent-control-clear-agent-wakes)
               (lambda (&rest _) (push 'clear log)))
              ((symbol-function 'niwa-agent-runtime--settle)
               (lambda (_invocation _response _event token)
                 (push (cons 'settle token) log)
                 "settled"))
              ((symbol-function 'niwa-agent-control-settlement-finished)
               #'ignore))
      (setq gate-held t)
      (let ((result (niwa-agent-runtime-interrupt invocation "stop")))
        (should (stringp result))
        (should (string-match-p "stop" result)))
      ;; Nothing was aborted, cleared or settled while the gate is held.
      (should-not log)
      (should-not (niwa-agent-invocation-terminal-reason invocation))
      (setq gate-held nil)
      ;; The queued run re-enters with its own token and hands it on.
      (funcall queued 'fresh)
      (should (equal '((settle . fresh) cancel clear) log))
      (should (equal "stop"
                     (niwa-agent-invocation-terminal-reason invocation)))))
  :doc "an open gate interrupts in place and returns the settled response"
  (let* ((session (niwa-session--create))
         (invocation (niwa-agent-runtime-test--invocation)))
    (setf (niwa-agent-invocation-parent-session invocation) session)
    (cl-letf (((symbol-function 'niwa-session-publication-defer)
               (lambda (&rest _) nil))
              ((symbol-function 'niwa-agent-control-clear-agent-wakes)
               #'ignore)
              ((symbol-function 'niwa-agent-runtime--settle)
               (lambda (&rest _) "settled")))
      (should (equal "settled"
                     (niwa-agent-runtime-interrupt invocation "stop"))))))

(defun test-niwa-agent-runtime--dispatch-with-token (exec-run on-settle)
  "Dispatch a retained turn under gate token `owned' using EXEC-RUN.
ON-SETTLE, when non-nil, is the settle callback.  Return the recorded
calls as (SAVES FINALIZES SETTLES) token lists, after the dispatch error."
  (let* ((parent (generate-new-buffer " *agent-runtime-token-parent*"))
         (agent-buffer (generate-new-buffer " *agent-runtime-token-child*"))
         (session (niwa-session--create :name "main"))
         (configuration
          (niwa-agent-runtime-test--configuration
           (niwa-agent-runtime-test--agent)))
         saves finalizes settles)
    (unwind-protect
        (with-current-buffer parent
          (setq-local niwa--session session)
          (cl-letf (((symbol-function 'niwa-agent-conversation-save)
                     (lambda (_invocation &optional _deferred token)
                       (push token saves)
                       t))
                    ((symbol-function 'niwa-agent-exec-run) exec-run)
                    ((symbol-function 'niwa-agent-runtime--finalize)
                     (lambda (_invocation _status &optional token)
                       (push token finalizes)))
                    ((symbol-function 'niwa-agent-runtime--settle)
                     (lambda (_invocation _response &optional _event token)
                       (push token settles)
                       nil))
                    ((symbol-function 'niwa-telemetry-record)
                     (lambda (_session event &rest _)
                       (when (eq event 'agent-request-sent)
                         (error "Injected publication failure")))))
            (should-error
             (apply #'niwa-agent-runtime-dispatch
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

(niwa-deftest niwa-agent-runtime-dispatch/token ()
  ,test
  (test)
  :doc "an unpublished error carries the token through the save and the cleanup"
  (let (cancelled)
    (pcase-let
        ((`(,saves ,finalizes ,settles)
          (test-niwa-agent-runtime--dispatch-with-token
           (lambda (_callback _type _description invocation _buffer)
             (setf (niwa-agent-invocation-runtime-cancel invocation)
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
        (test-niwa-agent-runtime--dispatch-with-token
         (lambda (&rest _) 'fsm)
         (lambda (&rest _) nil))))
    (should (equal '(owned) saves))
    (should-not finalizes)
    (should (equal '(owned) settles))))

(provide 'test-niwa-agent-runtime)

;;; test-niwa-agent-runtime.el ends here
