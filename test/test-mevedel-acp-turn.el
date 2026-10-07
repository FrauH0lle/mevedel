;;; test-mevedel-acp-turn.el --- ACP turns in ordinary sessions -*- lexical-binding: t -*-

;;; Commentary:
;; Exercise publication and cancellation with a real external subprocess.

;;; Code:

(require 'mevedel-engine-test-support
         (file-name-concat
          (file-name-directory (or load-file-name buffer-file-name))
          "mevedel-engine-test-support"))
(require 'mevedel-acp-turn)
(require 'mevedel-claude-code)
(require 'mevedel-view-render)

(defconst mevedel-acp-turn-test--peer
  (file-name-concat (file-name-directory (or load-file-name buffer-file-name))
                    "fixtures" "acp-agent.py"))

(mevedel-deftest mevedel-acp-turn-start (:quiet t)
  ,test
  (test)

  :doc "streams, executes through MCP, renders full calls with the display adapter, and settles once"
  (mevedel-engine-test--with-session
    (let* ((path (file-name-concat root "source.txt"))
           (display (symbol-function 'gptel--display-tool-results))
           retained-id)
      (write-region "real tool evidence" nil path nil 'silent)
      ;; Exercise the same call-record adapter installed in the live editor.
      (cl-letf (((symbol-function 'gptel--display-tool-results)
                 (lambda (results info)
                   (mevedel-tool-render-data--display-results-advice
                    display results info))))
        (mevedel-acp-turn-start
         request
         (lambda (mcp _hook-command)
           (list :command (executable-find "python3")
                 :args (list mevedel-acp-turn-test--peer) :cwd root
                 :mcp mcp :tool-id-field :fixtureToolId))
         (vector `((type . "text") (text . ,(concat "read:" path))))
         (list (mevedel-tool-ensure "Read"))
         (lambda (id) (setq retained-id id)))
        (with-timeout (5 (ert-fail "External turn did not settle"))
          (while (mevedel-turn-busy-p buffer) (accept-process-output nil 0.01))))
      (should (equal "fixture-session" retained-id))
      (should-not (mevedel-request-fsm request))
      (should (= 1 (mevedel-session-turn-count session)))
      (should (string-search "real tool evidence" (buffer-string)))
      (let* ((seg (seq-find (lambda (s) (eq (car s) 'tool))
                            (mevedel-transcript-segments (point-min) (point-max))))
             (call (mevedel-view--tool-call-parse buffer (cadr seg) (caddr seg)))
             (rendering (mevedel-view--render-tool-call call buffer)))
        (should (equal path (plist-get (plist-get call :args) :file_path)))
        (should (string-search "source.txt" (plist-get rendering :header)))
        (should (string-search "real tool evidence" (plist-get rendering :body))))
      (should (= 1 (how-many "^#\\+begin_tool" (point-min) (point-max))))
      (should-not (string-search "foreign" (buffer-string)))
      (should (eq 'idle (mevedel-session-agent-root-activity session)))))

  :doc "cancellation retains partial text and clears admission without a successful turn"
  (mevedel-engine-test--with-session
    (mevedel-acp-turn-start
     request
     (lambda (mcp _hook-command)
       (list :command (executable-find "python3")
             :args (list mevedel-acp-turn-test--peer) :cwd root :mcp mcp))
     [((type . "text") (text . "wait"))] nil)
    (with-timeout (5 (ert-fail "External stream did not start"))
      (while (not (string-search "waiting" (buffer-string)))
        (accept-process-output nil 0.01)))
    (mevedel-abort buffer)
    (with-timeout (5 (ert-fail "External interruption did not settle"))
      (while (mevedel-turn-busy-p buffer) (accept-process-output nil 0.01)))
    (should (string-search "waiting" (buffer-string)))
    (should-not mevedel--current-request)
    (should-not mevedel--turn-settlements-pending)
    (should (= 1 (mevedel-session-turn-count session)))
    (should (eq 'aborted (plist-get (mevedel-engine-info request)
                                  :mevedel-acp-outcome))))

  :doc "cancellation accounts for acknowledged native terminal usage exactly once"
  (mevedel-engine-test--with-session
    (let (native-outcome (completed 0))
      (mevedel-acp-turn-start
       request
       (lambda (mcp _hook)
         (list :command (executable-find "python3")
               :args (list mevedel-acp-turn-test--peer) :cwd root :mcp mcp
               :observe #'mevedel-claude-code-usage-observe
               :complete-prompt (lambda (owner outcome)
                                  (cl-incf completed)
                                  (mevedel-claude-code--complete-prompt owner outcome))
               :normalize-outcome (lambda (outcome)
                                    (setq native-outcome (mevedel-claude-code--outcome outcome)))
               :meta '((sdkWhileWaiting .
                        [((type . "assistant")
                          (message . ((id . "cancel-sample") (model . "sonnet")
                                      (usage . ((input_tokens . 10) (output_tokens . 1)
                                                (cache_creation_input_tokens . 0)
                                                (cache_read_input_tokens . 0))))))])
                       (cancelResponse . ((stopReason . "cancelled")
                                          (usage . ((inputTokens . 40) (outputTokens . 20)
                                                    (cachedReadTokens . 3) (cachedWriteTokens . 5))))))))
       [((type . "text") (text . "wait"))] nil)
      (with-timeout (5 (ert-fail "Waiting peer did not start"))
        (while (not (string-search "waiting" (buffer-string)))
          (accept-process-output nil .01)))
      (should (= 10 (plist-get (plist-get (mevedel-engine-info request) :tokens-full) :input)))
      (mevedel-abort buffer)
      (with-timeout (5 (ert-fail "Cancelled turn did not settle"))
        (while (mevedel-turn-busy-p buffer) (accept-process-output nil .01)))
      (should (eq 'interrupted (plist-get native-outcome :status)))
      (should (equal (plist-get native-outcome :tokens)
                     (plist-get (mevedel-engine-info request) :tokens-full)))
      (should (= 45 (plist-get (plist-get native-outcome :tokens) :input)))
      (should (= 20 (plist-get (plist-get native-outcome :tokens) :output)))
      (should (= 1 completed))
      (should (= 1 (mevedel-session-turn-count session)))
      (should (eq 'aborted (plist-get (mevedel-engine-info request) :mevedel-acp-outcome)))))

  :doc "child interruption retains final native usage until its asynchronous terminal settlement"
  (mevedel-engine-test--with-session
    (let* ((invocation
            (mevedel-agent-invocation--create
             :buffer buffer :path "/root/usage-child" :description "Usage child"
             :runtime-context (list :buffer buffer :mevedel-request request)))
           (settled 0) terminal-status)
      (setq-local mevedel--agent-invocation invocation)
      (mevedel-acp-turn-start
       invocation
       (lambda (mcp _hook)
         (list :command (executable-find "python3")
               :args (list mevedel-acp-turn-test--peer) :cwd root :mcp mcp
               :complete-prompt #'mevedel-claude-code--complete-prompt
               :normalize-outcome #'mevedel-claude-code--outcome
               :meta '((cancelResponse . ((stopReason . "cancelled")
                                          (usage . ((inputTokens . 43) (outputTokens . 8)
                                                    (cachedReadTokens . 0) (cachedWriteTokens . 0))))))))
       [((type . "text") (text . "wait"))] nil nil nil
       (lambda (status)
         (cl-incf settled)
         (setq terminal-status status)
         (setf (mevedel-agent-invocation-runtime-settled-p invocation) t)))
      (with-timeout (5 (ert-fail "Native child did not start"))
        (while (not (string-search "waiting" (buffer-string)))
          (accept-process-output nil .01)))
      (mevedel-agent-runtime-interrupt invocation "stop child")
      (mevedel-agent-runtime-interrupt invocation "stop child again")
      (should (= 0 settled))
      (should mevedel--turn-settlements-pending)
      (with-timeout (5 (ert-fail "Native child interruption did not settle"))
        (while (= 0 settled) (accept-process-output nil .01)))
      (should (= 1 settled))
      (should (eq 'aborted terminal-status))
      (should (= 43 (plist-get (plist-get (mevedel-engine-info invocation) :tokens-full) :input)))
      (should (= 8 (plist-get (plist-get (mevedel-engine-info invocation) :tokens-full) :output)))
      (should-not mevedel--turn-settlements-pending)))

  :doc "abort preserves a terminal acknowledgement already queued behind target transport"
  (mevedel-engine-test--with-session
    (let* (native-outcome
           (connection
            (mevedel-acp-turn-start
             request
             (lambda (mcp _hook)
               (list :command (executable-find "python3")
                     :args (list mevedel-acp-turn-test--peer) :cwd root :mcp mcp
                     :complete-prompt #'mevedel-claude-code--complete-prompt
                     :normalize-outcome (lambda (outcome)
                                          (setq native-outcome (mevedel-claude-code--outcome outcome)))
                     :meta '((cancelResponse . ((stopReason . "cancelled")
                                                (usage . ((inputTokens . 41) (outputTokens . 9)
                                                          (cachedReadTokens . 0) (cachedWriteTokens . 0))))))))
             [((type . "text") (text . "wait"))] nil)))
      (with-timeout (5 (ert-fail "Waiting peer did not start"))
        (while (not (string-search "waiting" (buffer-string)))
          (accept-process-output nil .01)))
      (mevedel-acp-cancel connection)
      ;; Queue the native drain before a different target operation begins.
      (should (accept-process-output
               (alist-get :process (mevedel-acp-client connection)) 2 nil 0))
      (mevedel-transport-call-as-remote-operation
       (lambda ()
         (with-timeout (5 (ert-fail "Native acknowledgement did not arrive"))
           (while (not native-outcome) (accept-process-output nil .01)))
         (should-not (mevedel-acp-active connection))
         (should (mevedel-turn-busy-p buffer))
         (mevedel-abort buffer)))
      (with-timeout (5 (ert-fail "Queued interruption did not settle"))
        (while (mevedel-turn-busy-p buffer) (accept-process-output nil .01)))
      (should (= 41 (plist-get (plist-get (mevedel-engine-info request) :tokens-full) :input)))
      (should (= 9 (plist-get (plist-get (mevedel-engine-info request) :tokens-full) :output)))
      (should (= 1 (mevedel-session-turn-count session)))
      (should (eq 'aborted (plist-get (mevedel-engine-info request) :mevedel-acp-outcome)))))

  :doc "a failed post-response observer cannot strand successful settlement"
  (mevedel-engine-test--with-session
    (let ((gptel-post-response-functions (list (lambda (&rest _) (error "Observer broke"))))
          diagnostics)
      (mevedel-test--with-captured-diagnostics diagnostics
        (mevedel-acp-turn-start
         request
         (lambda (mcp _hook-command)
           (list :command (executable-find "python3")
                 :args (list mevedel-acp-turn-test--peer) :cwd root :mcp mcp))
         [((type . "text") (text . "hello"))] nil)
        (with-timeout (5 (ert-fail "Observer failure stranded settlement"))
          (while (mevedel-turn-busy-p buffer) (accept-process-output nil 0.01))))
      (should (string-search "Observer broke" diagnostics))
      (should (= 1 (mevedel-session-turn-count session)))
      (should-not mevedel--current-request)))

  :doc "a crashed process records a failed partial turn and releases admission"
  (mevedel-engine-test--with-session
    (mevedel-acp-turn-start
     request
     (lambda (mcp _hook-command)
       (list :command (executable-find "python3")
             :args (list mevedel-acp-turn-test--peer) :cwd root :mcp mcp))
     [((type . "text") (text . "crash"))] nil)
    (with-timeout (5 (ert-fail "Crashed turn did not settle"))
      (while (mevedel-turn-busy-p buffer) (accept-process-output nil 0.01)))
    (should (eq 'error (plist-get (mevedel-engine-info request) :mevedel-acp-outcome)))
    (should (string-search "process"
					 (plist-get (plist-get (mevedel-engine-info request) :error) :message)))
    (should-not mevedel--current-request))

  :doc "Goal pause reaches the native batch hook and settles as a successful turn"
  (mevedel-engine-test--with-session
    (let* ((path (file-name-concat root "source.txt"))
           (calls 0)
           (mevedel-post-tool-use-functions
            (list (lambda (_event)
                    (cl-incf calls)
                    (mevedel-goal-pause)
                    nil))))
      (write-region "boundary evidence" nil path nil 'silent)
      (mevedel-goal-start "Read both files")
      (mevedel-acp-turn-start
       request
       (lambda (mcp hook-command)
         (list :command (executable-find "python3")
               :args (list mevedel-acp-turn-test--peer) :cwd root :mcp mcp
               :tool-id-field :fixtureToolId
               :control #'mevedel-claude-code--control
               :normalize-outcome #'mevedel-claude-code--outcome
               :meta `((hookCommand . ,hook-command)
                       (promptResponse . ((stopReason . "end_turn")
                                          (usage . ((inputTokens . 100)
                                                    (outputTokens . 23)
                                                    (cachedReadTokens . 1000)
                                                    (cachedWriteTokens . 17))))))))
       (vector `((type . "text") (text . ,(concat "read:" path))))
       (list (mevedel-tool-ensure "Read")))
      (with-timeout (5 (ert-fail "Paused Goal did not settle"))
        (while (mevedel-turn-busy-p buffer) (accept-process-output nil 0.01)))
      (should (= 1 calls))
      (should (string-search "goal-paused" (buffer-string)))
      (should (eq 'paused (mevedel-goal-status (mevedel-session-goal session))))
      (should (eq 'success (plist-get (mevedel-engine-info request) :mevedel-acp-outcome)))
      (should (= 140 (mevedel-goal-tokens-used (mevedel-session-goal session))))
      (should (= 1 (mevedel-goal-turns-run (mevedel-session-goal session))))))

  :doc "terminal publication flushes batched text before saving the transcript"
  (mevedel-engine-test--with-session
    (let ((mevedel-gptel-stream-bridge-insert-batch-delay 60)
          (installed (advice-member-p
                      #'mevedel-gptel-stream-bridge--gptel-stream-insert-response-advice
                      'gptel-curl--stream-insert-response)))
      (unwind-protect
          (progn
            (unless installed
              (advice-add 'gptel-curl--stream-insert-response :around
                          #'mevedel-gptel-stream-bridge--gptel-stream-insert-response-advice))
            (mevedel-acp-turn-start
             request
             (lambda (mcp _hook-command)
               (list :command (executable-find "python3")
                     :args (list mevedel-acp-turn-test--peer) :cwd root :mcp mcp))
             [((type . "text") (text . "hello"))] nil)
            (with-timeout (5 (ert-fail "Batched turn did not settle"))
              (while (mevedel-turn-busy-p buffer) (accept-process-output nil 0.01)))
            (should (string-search "answer:hello" (buffer-string)))
            (should (string-search
                     "answer:hello"
                     (decode-coding-string
                      (mevedel-session-artifacts-read-artifact
                       session (format "segment-%04d.chat.org"
                                       (mevedel-session-current-segment session))) 'utf-8)))
            (should-not (plist-get (mevedel-engine-info request)
                                   :mevedel-stream-insert-timer)))
        (when-let* ((timer (plist-get (mevedel-engine-info request)
                                     :mevedel-stream-insert-timer)))
          (cancel-timer timer))
        (unless installed
          (advice-remove 'gptel-curl--stream-insert-response
                         #'mevedel-gptel-stream-bridge--gptel-stream-insert-response-advice))))))

(mevedel-deftest mevedel-acp-turn-start/mcp-after-acp-frames (:quiet t)
  ,test
  (test)
  :doc "a tool call read in the same pass as an earlier ACP receipt runs after it"
  (mevedel-engine-test--with-session
    (let ((gptel--known-backends nil)
          (mevedel-claude-code-directory (file-name-concat root "claude"))
          (file (file-name-concat root "evidence.txt"))
          dispatch content connection tool-result)
      (write-region "evidence" nil file nil 'silent)
      (mevedel-claude-code-register)
      (mevedel-model-set-session-provider
       session (mevedel-model-resolve-provider "Claude Code:sonnet") buffer)
      (setq-local gptel-system-prompt "Order fixture"
                  gptel-tools (list (mevedel-tool-gptel-tool (mevedel-tool-ensure "Read"))))
      (advice-add 'mevedel-mcp-start :around
                  (lambda (orig tools d &rest rest) (setq dispatch d) (apply orig tools d rest))
                  '((name . mevedel-acp-turn-test--capture)))
      (advice-add 'mevedel-acp-prompt :before
                  (lambda (c input &rest _) (setq connection c content input))
                  '((name . mevedel-acp-turn-test--capture)))
      (unwind-protect
          (cl-letf (((symbol-function 'mevedel-claude-code-launch)
                     (mevedel-engine-test--claude-launch
                      (lambda (_system mcp _model _effort &optional id hook)
                        (list :meta `((hookCommand . ,hook) (promptAcknowledgement . :false)))))))
            (mevedel-session-enqueue-pending-reminder session "ORDER PENDING EVENT")
            (mevedel-session-ensure-reminder session (mevedel-reminders-make-pending-events))
            (mevedel--insert-user-turn "wait")
            (mevedel--send-request "wait")
            (with-timeout (5 (ert-fail "Turn did not start"))
              (while (not (string-search "waiting" (buffer-string)))
                (accept-process-output nil 0.01)))
            (let ((process (alist-get :process (mevedel-acp-client connection))))
              ;; One read pass: the ACP filter queues the receipt, then the MCP
              ;; filter reads the tool call before any timer runs.
              (funcall (process-filter process) process
                       (concat (json-serialize
                                `((jsonrpc . "2.0") (method . "_claude/sdkMessage")
                                  (params . ((sessionId . "fixture-session")
                                             (message . ((type . "user") (uuid . "u1")
                                                         (parent_tool_use_id . :null)
                                                         (message . ((role . "user")
                                                                     (content . ,content)))))))))
                               "\n"))
              (funcall dispatch "Read" (list :file_path file)
                       (list :claudecode/toolUseId "order-1")
                       (lambda (result) (setq tool-result result))))
            (with-timeout (5 (ert-fail "Tool call was not answered"))
              (while (not tool-result) (accept-process-output nil 0.01)))
            (should (eq :json-false (plist-get tool-result :isError)))
            (should-not (plist-get (mevedel-engine-info request) :error))
            (mevedel-abort buffer)
            (with-timeout (5 (ert-fail "Turn did not settle"))
              (while (mevedel-turn-busy-p buffer) (accept-process-output nil 0.01))))
        (advice-remove 'mevedel-mcp-start 'mevedel-acp-turn-test--capture)
        (advice-remove 'mevedel-acp-prompt 'mevedel-acp-turn-test--capture)))))

(mevedel-deftest mevedel-acp-turn-start/tool-hooks (:quiet t)
  ,test
  (test)

  :doc "post-tool stop prevents subsequent calls and preserves completed evidence"
  (mevedel-engine-test--with-session
    (let* ((path (file-name-concat root "source.txt"))
           (calls 0)
           (gptel-pre-tool-call-functions (list (lambda (_) (cl-incf calls) nil)))
           (gptel-post-tool-call-functions
            (list (lambda (_) '(:stop t :stop-reason "Stop after evidence")))))
      (write-region "completed evidence" nil path nil 'silent)
      (mevedel-acp-turn-start
       request
       (lambda (mcp _hook)
         (list :command (executable-find "python3") :args (list mevedel-acp-turn-test--peer)
               :cwd root :mcp mcp :tool-id-field :claudecode/toolUseId
               :meta `((toolBatches . [[((name . "Read") (args . ((file_path . ,path))) (id . "first"))
                                       ((name . "Read") (args . ((file_path . ,path))) (id . "second"))]]))))
       [((type . "text") (text . "Read twice"))] (list (mevedel-tool-ensure "Read")))
      (with-timeout (5 (ert-fail "Hook stop did not settle"))
        (while (mevedel-turn-busy-p buffer) (accept-process-output nil .01)))
      (should (= 1 calls))
      (should (string-search "completed evidence" (buffer-string)))
      (should (eq 'error (plist-get (mevedel-engine-info request) :mevedel-acp-outcome)))))

  :doc "post-tool replacement and blocking withhold original text and media from MCP and transcript"
  (dolist (decision '((:result "redacted") (:block "withheld")))
    (mevedel-engine-test--with-session
      (let* ((path (file-name-concat root "source.txt"))
             (tool (copy-mevedel-tool (mevedel-tool-ensure "Read")))
             observed
             (gptel-post-tool-call-functions
              (list (lambda (_) decision)
                    (lambda (call) (setq observed (plist-get call :result)) '(:block nil))))
             payload
             (capture (lambda (_client line)
                        (let ((message (json-parse-string line :object-type 'plist
                                                          :false-object :json-false)))
                          (when (plist-get (plist-get message :result) :content)
                            (setq payload (plist-get message :result)))))))
        (write-region "PRIVATE EVIDENCE" nil path nil 'silent)
        (setf (mevedel-tool-async-p tool) nil
              (mevedel-tool-handler tool)
              (lambda (_args)
                '(:result "PRIVATE EVIDENCE" :media ((:mime "image/png" :data "QUJD")))))
        (advice-add 'mevedel-mcp--write :before capture)
        (unwind-protect
            (progn
              (mevedel-acp-turn-start
               request
               (lambda (mcp _hook)
                 (list :command (executable-find "python3") :args (list mevedel-acp-turn-test--peer)
                       :cwd root :mcp mcp :tool-id-field :fixtureToolId))
               (vector `((type . "text") (text . ,(concat "read:" path))))
               (list tool))
              (with-timeout (5 (ert-fail "Hook replacement did not settle"))
                (while (mevedel-turn-busy-p buffer) (accept-process-output nil .01)))
              (let ((expected (or (plist-get decision :result) (plist-get decision :block))))
                (should (equal expected (plist-get (aref (plist-get payload :content) 0) :text)))
                (should (= 1 (length (plist-get payload :content))))
                (should (equal expected observed))
                (should (eq (if (plist-get decision :block) t :json-false) (plist-get payload :isError)))
                (should (string-search expected (buffer-string)))
                (should-not (string-search "PRIVATE EVIDENCE" (buffer-string)))))
          (advice-remove 'mevedel-mcp--write capture)))))

  :doc "pre-tool arguments reach validation and later observers, while synthetic results skip execution"
  (dolist (synthetic '(nil t))
    (mevedel-engine-test--with-session
      (let* ((path (file-name-concat root "source.txt"))
             (replacement (list :file_path path))
             observed
             (gptel-pre-tool-call-functions
              (list (lambda (_) (if synthetic '(:result "synthetic evidence")
                                  (list :args replacement)))
                    (lambda (call) (setq observed (plist-get call :args))))))
        (write-region "replacement evidence" nil path nil 'silent)
        (mevedel-acp-turn-start
         request
         (lambda (mcp _hook)
           (list :command (executable-find "python3") :args (list mevedel-acp-turn-test--peer)
                 :cwd root :mcp mcp :tool-id-field :fixtureToolId))
         [((type . "text") (text . "read:/missing-original-file"))]
         (list (mevedel-tool-ensure "Read")))
        (with-timeout (5 (ert-fail "Pre-hook did not settle"))
          (while (mevedel-turn-busy-p buffer) (accept-process-output nil .01)))
        (should (string-search (if synthetic "synthetic evidence" "replacement evidence") (buffer-string)))
        (unless synthetic (should (equal replacement observed))))))

  :doc "an asynchronous result publication error closes the turn instead of stranding MCP"
  (mevedel-engine-test--with-session
    (let* ((tool (copy-mevedel-tool (mevedel-tool-ensure "Read")))
           (path (file-name-concat root "source.txt"))
           complete)
      (write-region "evidence" nil path nil 'silent)
      (setf (mevedel-tool-async-p tool) t
            (mevedel-tool-handler tool) (lambda (callback _args) (setq complete callback)))
      (cl-letf (((symbol-function 'mevedel-acp-turn--tool-result)
                 (lambda (&rest _) (error "Publication failed"))))
        (mevedel-acp-turn-start
         request
         (lambda (mcp _hook)
           (list :command (executable-find "python3") :args (list mevedel-acp-turn-test--peer)
                 :cwd root :mcp mcp :tool-id-field :fixtureToolId))
         (vector `((type . "text") (text . ,(concat "read:" path)))) (list tool))
        (with-timeout (5 (ert-fail "Async call was not admitted"))
          (while (not complete) (accept-process-output nil .01)))
        (funcall complete '(:result "async evidence"))
        (with-timeout (5 (ert-fail "Publication failure stranded the turn"))
          (while (mevedel-turn-busy-p buffer) (accept-process-output nil .01))))
      (should (eq 'error (plist-get (mevedel-engine-info request) :mevedel-acp-outcome)))))

  :doc "hook denials stay denied and unsupported controls fail before effects"
  (dolist (decision '((:block "denied") (:confirm t) (:confirm nil) (:name "Other")))
    (mevedel-engine-test--with-session
      (let* ((tool (copy-mevedel-tool (mevedel-tool-ensure "Read")))
             (calls 0)
             (gptel-pre-tool-call-functions
              (list (lambda (_) decision) (lambda (_) '(:block nil)))))
        (setf (mevedel-tool-handler tool)
              (lambda (&rest _) (cl-incf calls) '(:result "Forbidden execution")))
        (mevedel-acp-turn-start
         request
         (lambda (mcp _hook)
           (list :command (executable-find "python3") :args (list mevedel-acp-turn-test--peer)
                 :cwd root :mcp mcp :tool-id-field :fixtureToolId))
         [((type . "text") (text . "read:/missing-original-file"))] (list tool))
        (with-timeout (5 (ert-fail "Hook denial did not settle"))
          (while (mevedel-turn-busy-p buffer) (accept-process-output nil .01)))
        (should (= 0 calls))
        (if (plist-get decision :block)
            (should (string-search "denied" (buffer-string)))
          (should (eq 'error (plist-get (mevedel-engine-info request) :mevedel-acp-outcome))))))))

(provide 'test-mevedel-acp-turn)
;;; test-mevedel-acp-turn.el ends here
