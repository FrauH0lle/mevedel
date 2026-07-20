;;; test-mevedel-agent-runtime.el --- Retained agent runner tests -*- lexical-binding: t -*-

;;; Commentary:

;; Focused Agent V2 provider-boundary, settlement, interruption, and yielded
;; execution tests.

;;; Code:

(require 'gptel-request)
(require 'mevedel-agent-conversation)
(require 'mevedel-agents)
(require 'mevedel-reminders)
(require 'mevedel-agent-runtime)
(require 'mevedel-execution)
(require 'mevedel-session-persistence)
(require 'mevedel-structs)
(require 'mevedel-tool-task)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

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

(mevedel-deftest mevedel-agent-runtime--run-hook-sync ()
  ,test
  (test)
  :doc "waits for and returns an agent hook decision"
  (let* ((session (mevedel-session--create :name "main"))
         (invocation (mevedel-agent-runtime-test--invocation))
         (mevedel-user-prompt-submit-functions
          (list (lambda (_event) '(:updated-input "rewritten")))))
    (setf (mevedel-agent-invocation-parent-session invocation) session)
    (should
     (equal '(:updated-input "rewritten")
            (mevedel-agent-runtime--run-hook-sync
             'UserPromptSubmit
             (list :hook-event 'UserPromptSubmit)
             invocation)))))

(mevedel-deftest mevedel-agent-runtime--run-start-hook ()
  ,test
  (test)
  :doc "records SubagentStart metadata and its parent audit"
  (let* ((session (mevedel-session--create :name "main"))
         (invocation (mevedel-agent-runtime-test--invocation))
         event
         (mevedel-subagent-start-functions
          (list (lambda (payload)
                  (setq event payload)
                  '(:additional-context ("startup context"))))))
    (setf (mevedel-agent-invocation-parent-session invocation) session)
    (mevedel-agent-runtime--run-start-hook
     "explorer" "Inspect hooks" "Prompt body" invocation)
    (should (equal (plist-get event :agent-path) "/root/explore"))
    (should (equal (plist-get event :role) "explorer"))
    (should (equal (plist-get event :prompt) "Prompt body"))
    (let ((audit (car (mevedel-agent-invocation-hook-audits invocation))))
      (should (eq (plist-get audit :type) 'subagent-context))
      (should (equal (plist-get audit :event) "SubagentStart"))
      (should-not (plist-member audit :context)))))

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

(mevedel-deftest mevedel-agent-runtime--prepare-turn ()
  ,test
  (test)
  :doc "composes lifecycle context once and consumes retained pending context"
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
           (mevedel-agent-runtime--prepare-turn
            "explorer" "Explore" "original prompt" invocation t
            pending (lambda (entries) (setq transition entries)))))
      (should (equal '(submit) (nreverse lifecycle)))
      (should (string-match-p "rewritten prompt" (plist-get turn :prompt)))
      (should (string-match-p "pending context" (plist-get turn :prompt)))
      (should (string-match-p "prompt context" (plist-get turn :prompt)))
      (should-not transition)
      (should (plist-get turn :audits)))))

(mevedel-deftest mevedel-agent-runtime-dispatch
  ()
  ,test
  (test)
  :doc "rejects dispatch outside an active session"
  (with-temp-buffer
    (let ((mevedel--session nil))
      (should-error
       (mevedel-agent-runtime-dispatch
        (mevedel-agent-runtime-test--agent)
        "Explore" "Inspect files" :path "/root/explore"))))

  :doc "starts one fresh asynchronous turn and settles its callback once"
  (let* ((parent (generate-new-buffer " *agent-runtime-parent*"))
         (agent-buffer (generate-new-buffer " *agent-runtime-child*"))
         (session (mevedel-session--create :name "main"))
         (agent (mevedel-agent-runtime-test--agent))
         (configuration
          (mevedel-agent-runtime-test--configuration agent))
         provider-callback
         seen-invocation
         settlements
         finalizations
         events)
    (unwind-protect
        (with-current-buffer parent
          (setq-local mevedel--session session)
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
               ((symbol-function 'mevedel-agent-runtime--finalize)
                (lambda (invocation status)
                  (push status finalizations)
                  (setf (mevedel-agent-invocation-transcript-status invocation)
                        status))))
            (let ((invocation
                   (mevedel-agent-runtime-dispatch
                    agent "Explore" "Find the entry point."
                    :path "/root/explore"
                    :context-snapshot "Prior context"
                    :on-invocation
                    (lambda (inv)
                      (push 'invocation-published events)
                      (setq seen-invocation inv))
                    :on-settle
                    (lambda (inv response event)
                      (push (list inv response event) settlements)))))
              (should (eq invocation seen-invocation))
              (should (eq 'provider-fsm
                          (mevedel-agent-invocation-runtime-fsm invocation)))
              (should (equal "/root/explore"
                             (mevedel-agent-invocation-path invocation)))
              (should
               (equal '(transcript-setup transcript-save invocation-published)
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
              (setq first
                    (mevedel-agent-runtime-dispatch
                     agent "Explore" "Initial task"
                     :path "/root/explore"
                     :frozen-configuration configuration))
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
              (should-not pending)
              (should (string-match-p "Rewritten accepted task"
                                      provider-prompt))
              (should (string-match-p "carry this context" provider-prompt))
              (should (string-match-p "current context" provider-prompt))
              (with-current-buffer agent-buffer
                (should (string-match-p "Accepted task" (buffer-string)))))))
      (kill-buffer agent-buffer)
      (kill-buffer parent))))

(mevedel-deftest mevedel-agent-runtime-queue-execution-completion
  ()
  ,test
  (test)
  :doc "holds a terminal response until yielded Bash completion is secured"
  (let* ((invocation (mevedel-agent-runtime-test--invocation))
         (live-p t)
         settled
         finalizations)
    (setf (mevedel-agent-invocation-runtime-settle-callback invocation)
          (lambda (_invocation response _event)
            (setq settled response)))
    (cl-letf (((symbol-function 'mevedel-agent-runtime--execution-live-p)
               (lambda (_invocation) live-p))
              ((symbol-function 'mevedel-agent-runtime--finalize)
               (lambda (_invocation status)
                 (push status finalizations))))
      (mevedel-agent-runtime--handle-provider-result invocation "Agent answer")
      (should-not settled)
      (should (equal "Agent answer"
                     (mevedel-agent-invocation-runtime-pending-response
                      invocation)))
      (should-not
       (mevedel-agent-runtime-queue-execution-completion
        invocation "/root/other" "wrong owner"))
      (setq live-p nil)
      (should
       (mevedel-agent-runtime-queue-execution-completion
        invocation "/root/explore" "Bash exited with code 0"))
      (should (string-match-p "Agent answer" settled))
      (should (string-match-p "Bash exited with code 0" settled))
      (should (equal '(completed) finalizations))
      (should-not
       (mevedel-agent-runtime-queue-execution-completion
        invocation "/root/explore" "duplicate")))))

(mevedel-deftest mevedel-agent-runtime--finalize ()
  ,test
  (test)
  :doc "an early cleanup failure does not skip later terminal effects"
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
                      'mevedel-session-persistence--update-transcript-entry)
                     (lambda (&rest _) (push 'transcript calls)))
                    ((symbol-function
                      'mevedel-agent-conversation-save)
                     (lambda (&rest _) (push 'save calls)))
                    ((symbol-function 'mevedel-agent-conversation-record-activity)
                     (lambda (&rest _) (push 'activity calls)))
                    ((symbol-function
                      'mevedel-agent-conversation-final-activity)
                     (lambda (&rest _) '(:status completed)))
                    ((symbol-function 'mevedel-tool-task-finalize-owner)
                     (lambda (&rest _) (push 'tasks calls) t))
                    ((symbol-function 'mevedel-tool-task--refresh-display)
                     (lambda () (push 'task-view calls)))
                    ((symbol-function 'mevedel-agent-conversation-refresh)
                     (lambda (&rest _) (push 'handle calls)))
                    ((symbol-function
                      'mevedel-session-persistence--write-sidecar-now)
                     (lambda (&rest _) (push 'sidecar calls)))
                    ((symbol-function 'mevedel-agent-exec-run-stop-hook)
                     (lambda (&rest _) (push 'hook calls)))
                    ((symbol-function
                      'mevedel-view-agent-live-transcript-finalize)
                     (lambda (&rest _) (push 'view calls)))
                    ((symbol-function 'display-warning)
                     (lambda (&rest args) (push args warnings))))
            (mevedel-agent-runtime--finalize invocation 'completed))
          (dolist (effect '(executions transcript save activity tasks task-view
                           handle sidecar hook view))
            (should (memq effect calls)))
          (should warnings)
          (should-not (mevedel-agent-invocation-activity invocation))
          (should (eq 'completed
                      (mevedel-agent-invocation-transcript-status invocation))))
      (kill-buffer child)
      (kill-buffer parent))))

(mevedel-deftest mevedel-agent-runtime-interrupt
  ()
  ,test
  (test)
  :doc "aborts a live provider turn and settles useful partial output once"
  (let* ((buffer (generate-new-buffer " *agent-runtime-interrupt*"))
         (invocation (mevedel-agent-runtime-test--invocation buffer))
         (provider-callback #'ignore)
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
                (lambda (_invocation response _event)
                  (setq callback-count (1+ (or callback-count 0))
                        callback-response response)))
          (cl-letf
              (((symbol-function 'mevedel-agent-runtime--partial-text)
                (lambda (&rest _) "useful partial work"))
               ((symbol-function 'mevedel-agent-runtime--finalize)
                (lambda (inv status)
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
              (should (string-match-p "useful partial work" response)))
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

  :doc "leaves a turn unsettled when provider abort fails"
  (let* ((buffer (generate-new-buffer " *agent-runtime-abort-fail*"))
         (invocation (mevedel-agent-runtime-test--invocation buffer))
         (fsm (gptel-make-fsm
               :state 'WAIT
               :info (list :buffer buffer :callback #'ignore)))
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
            (should-error
             (mevedel-agent-runtime-interrupt invocation "stop")))
          (should-not settled)
          (should-not
           (mevedel-agent-invocation-runtime-settled-p invocation))
          (should-not
           (mevedel-agent-invocation-terminal-reason invocation)))
      (kill-buffer buffer))))

(provide 'test-mevedel-agent-runtime)

;;; test-mevedel-agent-runtime.el ends here
