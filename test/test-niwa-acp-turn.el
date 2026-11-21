;;; test-niwa-acp-turn.el --- ACP turns in ordinary sessions -*- lexical-binding: t -*-

;;; Commentary:
;; Exercise publication and cancellation with a real external subprocess.

;;; Code:

(require 'niwa-engine-test-support
         (file-name-concat
          (file-name-directory (or load-file-name buffer-file-name))
          "niwa-engine-test-support"))
(require 'niwa-acp-turn)
(require 'niwa-claude-code)
(require 'niwa-mcp)
(require 'niwa-plan-mode)
(require 'niwa-view-render)
(require 'niwa-sigpipe-test-support
         (file-name-concat
          (file-name-directory (or load-file-name buffer-file-name))
          "niwa-sigpipe-test-support"))

(defconst niwa-acp-turn-test--file (or load-file-name buffer-file-name)
  "This file, which the SIGPIPE-ignoring child loads.")

(defun niwa-acp-turn-test--hold (seconds)
  "Keep Emacs from reading any process for SECONDS, as a long command does."
  (let ((end (+ (float-time) seconds)))
    (while (< (float-time) end))))

(niwa-deftest niwa-acp-turn-start (:quiet t)
  ,test
  (test)

  :doc "streams, executes through MCP, renders full calls with the display adapter, and settles once"
  (niwa-engine-test--with-session
    (let* ((path (file-name-concat root "source.txt"))
           (display (symbol-function 'gptel--display-tool-results))
           retained-id)
      (write-region "real tool evidence" nil path nil 'silent)
      ;; Exercise the same call-record adapter installed in the live editor.
      (cl-letf (((symbol-function 'gptel--display-tool-results)
                 (lambda (results info)
                   (niwa-tool-render-data--display-results-advice
                    display results info))))
        (niwa-acp-turn-start
         request
         (lambda (mcp _hook-command)
           (list :command (executable-find "python3")
                 :args (list niwa-test--acp-peer) :cwd root
                 :mcp mcp :tool-id-field :fixtureToolId))
         (vector `((type . "text") (text . ,(concat "read:" path))))
         (list (niwa-tool-ensure "Read"))
         (lambda (id) (setq retained-id id)))
        (niwa-test--await 5 "External turn did not settle" (not (niwa-turn-busy-p buffer))))
      (should (equal "fixture-session" retained-id))
      (should-not (niwa-request-fsm request))
      (should (= 1 (niwa-session-turn-count session)))
      (should (string-search "real tool evidence" (buffer-string)))
      (let* ((seg (seq-find (lambda (s) (eq (car s) 'tool))
                            (niwa-transcript-segments (point-min) (point-max))))
             (call (niwa-view--tool-call-parse buffer (cadr seg) (caddr seg)))
             (rendering (niwa-view--render-tool-call call buffer)))
        (should (equal path (plist-get (plist-get call :args) :file_path)))
        (should (string-search "source.txt" (plist-get rendering :header)))
        (should (string-search "real tool evidence" (plist-get rendering :body))))
      (should (= 1 (how-many "^#\\+begin_tool" (point-min) (point-max))))
      (should-not (string-search "foreign" (buffer-string)))
      (should (eq 'idle (niwa-session-agent-root-activity session)))))

  :doc "cancellation retains partial text and clears admission without a successful turn"
  (niwa-engine-test--with-session
    (niwa-acp-turn-start
     request
     (lambda (mcp _hook-command)
       (list :command (executable-find "python3")
             :args (list niwa-test--acp-peer) :cwd root :mcp mcp))
     [((type . "text") (text . "wait"))] nil)
    (niwa-test--await 5 "External stream did not start"
      (string-search "waiting" (buffer-string)))
    (niwa-abort buffer)
    (niwa-test--await 5 "External interruption did not settle"
      (not (niwa-turn-busy-p buffer)))
    (should (string-search "waiting" (buffer-string)))
    (should-not niwa--current-request)
    (should-not niwa--turn-settlements-pending)
    (should (= 1 (niwa-session-turn-count session)))
    (should (eq 'aborted (plist-get (niwa-engine-info request)
                                  :niwa-acp-outcome))))

  :doc "cancellation accounts for acknowledged native terminal usage exactly once"
  (niwa-engine-test--with-session
    (let (native-outcome (completed 0))
      (niwa-acp-turn-start
       request
       (lambda (mcp _hook)
         (list :command (executable-find "python3")
               :args (list niwa-test--acp-peer) :cwd root :mcp mcp
               :observe #'niwa-claude-code-usage-observe
               :complete-prompt (lambda (owner outcome)
                                  (cl-incf completed)
                                  (niwa-claude-code--complete-prompt owner outcome))
               :normalize-outcome (lambda (outcome)
                                    (setq native-outcome (niwa-claude-code--outcome outcome)))
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
      (niwa-test--await 5 "Waiting peer did not start" (string-search "waiting" (buffer-string)))
      (should (= 10 (plist-get (plist-get (niwa-engine-info request) :tokens-full) :input)))
      (niwa-abort buffer)
      (niwa-test--await 5 "Cancelled turn did not settle" (not (niwa-turn-busy-p buffer)))
      (should (eq 'interrupted (plist-get native-outcome :status)))
      (should (equal (plist-get native-outcome :tokens)
                     (plist-get (niwa-engine-info request) :tokens-full)))
      (should (= 45 (plist-get (plist-get native-outcome :tokens) :input)))
      (should (= 20 (plist-get (plist-get native-outcome :tokens) :output)))
      (should (= 1 completed))
      (should (= 1 (niwa-session-turn-count session)))
      (should (eq 'aborted (plist-get (niwa-engine-info request) :niwa-acp-outcome)))))

  :doc "child interruption retains final native usage until its asynchronous terminal settlement"
  (niwa-engine-test--with-session
    (let* ((invocation
            (niwa-agent-invocation--create
             :buffer buffer :path "/root/usage-child" :description "Usage child"
             :runtime-context (list :buffer buffer :niwa-request request)))
           (settled 0) terminal-status)
      (setq-local niwa--agent-invocation invocation)
      (niwa-acp-turn-start
       invocation
       (lambda (mcp _hook)
         (list :command (executable-find "python3")
               :args (list niwa-test--acp-peer) :cwd root :mcp mcp
               :complete-prompt #'niwa-claude-code--complete-prompt
               :normalize-outcome #'niwa-claude-code--outcome
               :meta '((cancelResponse . ((stopReason . "cancelled")
                                          (usage . ((inputTokens . 43) (outputTokens . 8)
                                                    (cachedReadTokens . 0) (cachedWriteTokens . 0))))))))
       [((type . "text") (text . "wait"))] nil nil nil
       (lambda (status)
         (cl-incf settled)
         (setq terminal-status status)
         (setf (niwa-agent-invocation-runtime-settled-p invocation) t)))
      (niwa-test--await 5 "Native child did not start" (string-search "waiting" (buffer-string)))
      (niwa-agent-runtime-interrupt invocation "stop child")
      (niwa-agent-runtime-interrupt invocation "stop child again")
      (should (= 0 settled))
      (should niwa--turn-settlements-pending)
      (niwa-test--await 5 "Native child interruption did not settle" (not (= 0 settled)))
      (should (= 1 settled))
      (should (eq 'aborted terminal-status))
      (should (= 43 (plist-get (plist-get (niwa-engine-info invocation) :tokens-full) :input)))
      (should (= 8 (plist-get (plist-get (niwa-engine-info invocation) :tokens-full) :output)))
      (should-not niwa--turn-settlements-pending)))

  :doc "abort preserves a terminal acknowledgement already queued behind target transport"
  (niwa-engine-test--with-session
    (let* (native-outcome
           (connection
            (niwa-acp-turn-start
             request
             (lambda (mcp _hook)
               (list :command (executable-find "python3")
                     :args (list niwa-test--acp-peer) :cwd root :mcp mcp
                     :complete-prompt #'niwa-claude-code--complete-prompt
                     :normalize-outcome (lambda (outcome)
                                          (setq native-outcome (niwa-claude-code--outcome outcome)))
                     :meta '((cancelResponse . ((stopReason . "cancelled")
                                                (usage . ((inputTokens . 41) (outputTokens . 9)
                                                          (cachedReadTokens . 0) (cachedWriteTokens . 0))))))))
             [((type . "text") (text . "wait"))] nil)))
      (niwa-test--await 5 "Waiting peer did not start" (string-search "waiting" (buffer-string)))
      (niwa-acp-cancel connection)
      ;; Queue the native drain before a different target operation begins.
      (should (accept-process-output
               (alist-get :process (niwa-acp-client connection)) 2 nil 0))
      (niwa-transport-call-as-remote-operation
       (lambda ()
         (niwa-test--await 5 "Native acknowledgement did not arrive" native-outcome)
         (should-not (niwa-acp-active connection))
         (should (niwa-turn-busy-p buffer))
         (niwa-abort buffer)))
      (niwa-test--await 5 "Queued interruption did not settle"
        (not (niwa-turn-busy-p buffer)))
      (should (= 41 (plist-get (plist-get (niwa-engine-info request) :tokens-full) :input)))
      (should (= 9 (plist-get (plist-get (niwa-engine-info request) :tokens-full) :output)))
      (should (= 1 (niwa-session-turn-count session)))
      (should (eq 'aborted (plist-get (niwa-engine-info request) :niwa-acp-outcome)))))

  :doc "post-response positions keep native Plan proposals readable in published sidecars"
  (niwa-engine-test--with-session
    (setf (niwa-session-plan-mode session) t
          (niwa-session-plan-metadata session)
          '(:selection (:location here :context current :execution direct
                        :mode edits :model-provider "Claude Code:sonnet"
                        :reasoning-effort high)))
    (let* (bounds
           (gptel-post-response-functions
            (list (lambda (start end)
                    (setq bounds (list start end))
                    (niwa-plan-mode--post-response start end)))))
      (cl-letf (((symbol-function 'niwa-plan-approval-render) #'ignore))
        (niwa-acp-turn-start
         request
         (lambda (mcp _hook-command)
           (list :command (executable-find "python3")
                 :args (list niwa-test--acp-peer) :cwd root :mcp mcp
                 :meta '((responseText . "<proposed_plan>\n# Native plan\n</proposed_plan>\n"))))
         [((type . "text") (text . "hello"))] nil)
        (niwa-test--await 5 "Plan turn did not settle"
          (not (niwa-turn-busy-p buffer))))
      (should bounds)
      (should (seq-every-p #'integerp bounds))
      (should (niwa-session-pending-plan-approval session))
      (should-not (niwa-session-pending-publication session))
      (let* ((sidecar
              (read (decode-coding-string
                     (niwa-session-artifacts-read-artifact
                      session "session.meta.el" t)
                     'utf-8-unix)))
             (metadata (plist-get sidecar :plan-metadata)))
        (should (eq 'proposed (plist-get metadata :status)))
        (should (equal (append bounds (list (niwa-plan-hash "# Native plan")))
                       (plist-get metadata :proposal-id)))
        (should (equal metadata (niwa-session-plan-metadata session))))))

  :doc "a failed post-response observer cannot strand successful settlement"
  (niwa-engine-test--with-session
    (let ((gptel-post-response-functions (list (lambda (&rest _) (error "Observer broke"))))
          diagnostics)
      (niwa-test--with-captured-diagnostics diagnostics
        (niwa-acp-turn-start
         request
         (lambda (mcp _hook-command)
           (list :command (executable-find "python3")
                 :args (list niwa-test--acp-peer) :cwd root :mcp mcp))
         [((type . "text") (text . "hello"))] nil)
        (niwa-test--await 5 "Observer failure stranded settlement"
          (not (niwa-turn-busy-p buffer))))
      (should (string-search "Observer broke" diagnostics))
      (should (= 1 (niwa-session-turn-count session)))
      (should-not niwa--current-request)))

  :doc "a crashed process records a failed partial turn and releases admission"
  (niwa-engine-test--with-session
    (niwa-acp-turn-start
     request
     (lambda (mcp _hook-command)
       (list :command (executable-find "python3")
             :args (list niwa-test--acp-peer) :cwd root :mcp mcp))
     [((type . "text") (text . "crash"))] nil)
    (niwa-test--await 5 "Crashed turn did not settle" (not (niwa-turn-busy-p buffer)))
    (should (eq 'error (plist-get (niwa-engine-info request) :niwa-acp-outcome)))
    (should (string-search "process"
					 (plist-get (plist-get (niwa-engine-info request) :error) :message)))
    (should-not niwa--current-request))

  :doc "Goal pause reaches the native batch hook and settles as a successful turn"
  (niwa-engine-test--with-session
    (let* ((path (file-name-concat root "source.txt"))
           (calls 0)
           (niwa-post-tool-use-functions
            (list (lambda (_event)
                    (cl-incf calls)
                    (niwa-goal-pause)
                    nil))))
      (write-region "boundary evidence" nil path nil 'silent)
      (niwa-goal-start "Read both files")
      (niwa-acp-turn-start
       request
       (lambda (mcp hook-command)
         (list :command (executable-find "python3")
               :args (list niwa-test--acp-peer) :cwd root :mcp mcp
               :tool-id-field :fixtureToolId
               :control #'niwa-claude-code--control
               :normalize-outcome #'niwa-claude-code--outcome
               :complete-prompt #'niwa-claude-code--complete-prompt
               :meta `((hookCommand . ,hook-command)
                       (promptResponse . ((stopReason . "end_turn")
                                          (usage . ((inputTokens . 100)
                                                    (outputTokens . 23)
                                                    (cachedReadTokens . 1000)
                                                    (cachedWriteTokens . 17))))))))
       (vector `((type . "text") (text . ,(concat "read:" path))))
       (list (niwa-tool-ensure "Read")))
      (niwa-test--await 5 "Paused Goal did not settle" (not (niwa-turn-busy-p buffer)))
      (should (= 1 calls))
      (should (string-search "goal-paused" (buffer-string)))
      (should (eq 'paused (niwa-goal-status (niwa-session-goal session))))
      (should (eq 'success (plist-get (niwa-engine-info request) :niwa-acp-outcome)))
      (should (= 140 (niwa-goal-tokens-used (niwa-session-goal session))))
      (should (= 1 (niwa-goal-turns-run (niwa-session-goal session))))))

  :doc "terminal publication flushes batched text before saving the transcript"
  (niwa-engine-test--with-session
    (let ((niwa-gptel-stream-bridge-insert-batch-delay 60)
          (installed (advice-member-p
                      #'niwa-gptel-stream-bridge--gptel-stream-insert-response-advice
                      'gptel-curl--stream-insert-response)))
      (unwind-protect
          (progn
            (unless installed
              (advice-add 'gptel-curl--stream-insert-response :around
                          #'niwa-gptel-stream-bridge--gptel-stream-insert-response-advice))
            (niwa-acp-turn-start
             request
             (lambda (mcp _hook-command)
               (list :command (executable-find "python3")
                     :args (list niwa-test--acp-peer) :cwd root :mcp mcp))
             [((type . "text") (text . "hello"))] nil)
            (niwa-test--await 5 "Batched turn did not settle" (not (niwa-turn-busy-p buffer)))
            (should (string-search "answer:hello" (buffer-string)))
            (should (string-search
                     "answer:hello"
                     (decode-coding-string
                      (niwa-session-artifacts-read-artifact
                       session (format "segment-%04d.chat.org"
                                       (niwa-session-current-segment session))) 'utf-8)))
            (should-not (plist-get (niwa-engine-info request)
                                   :niwa-stream-insert-timer)))
        (when-let* ((timer (plist-get (niwa-engine-info request)
                                     :niwa-stream-insert-timer)))
          (cancel-timer timer))
        (unless installed
          (advice-remove 'gptel-curl--stream-insert-response
                         #'niwa-gptel-stream-bridge--gptel-stream-insert-response-advice))))))

(defmacro niwa-acp-turn-test--with-receipt-race (acknowledgement &rest body)
  "Run BODY in a waiting Claude turn whose prompt ACKNOWLEDGEMENT is scripted.
BODY delivers the receipt and a Read call through `connection', `content' and
`dispatch'; the call must then succeed without failing the turn."
  (declare (indent 1) (debug t))
  `(niwa-engine-test--with-claude-session
     (let ((file (file-name-concat root "evidence.txt"))
           (receipt-release (file-name-concat root "receipt-release"))
           (receipt-written (file-name-concat root "receipt-written"))
           dispatch content connection tool-result)
       (write-region "evidence" nil file nil 'silent)
       (setq-local gptel-system-prompt "Order fixture"
                   gptel-tools (list (niwa-tool-gptel-tool (niwa-tool-ensure "Read"))))
       (advice-add 'niwa-mcp-start :around
                   (lambda (orig tools d &rest rest) (setq dispatch d) (apply orig tools d rest))
                   '((name . niwa-acp-turn-test--capture)))
       (advice-add 'niwa-acp-prompt :before
                   (lambda (c input &rest _) (setq connection c content input))
                   '((name . niwa-acp-turn-test--capture)))
       (unwind-protect
           (cl-letf (((symbol-function 'niwa-claude-code-launch)
                      (niwa-engine-test--claude-launch
                       (lambda (_system _mcp _model _effort &optional _id hook)
                        ;; The test dispatches its call itself, unannounced.
                        (list :announces-tool-calls nil
                              :meta `((hookCommand . ,hook)
                                      (promptAcknowledgement . ,,acknowledgement)
                                      (promptReceiptRelease . ,receipt-release)
                                      (promptReceiptMarker . ,receipt-written)))))))
             (niwa-session-enqueue-pending-reminder session "ORDER PENDING EVENT")
             (niwa-session-ensure-reminder session (niwa-reminders-make-pending-events))
             (niwa--insert-user-turn "wait")
             (niwa--send-request "wait")
             (niwa-test--await 5 "Turn did not start" (string-search "waiting" (buffer-string)))
             ,@body
             (niwa-test--await 5 "Tool call was not answered" tool-result)
             (should (eq :json-false (plist-get tool-result :isError)))
             (should-not (plist-get (niwa-engine-info request) :error))
             (niwa-abort buffer)
             (niwa-test--await 5 "Turn did not settle" (not (niwa-turn-busy-p buffer))))
         (advice-remove 'niwa-mcp-start 'niwa-acp-turn-test--capture)
         (advice-remove 'niwa-acp-prompt 'niwa-acp-turn-test--capture)))))

(niwa-deftest niwa-acp-turn-start/mcp-after-acp-frames (:quiet t)
  ,test
  (test)
  :doc "a tool call read in the same pass after an ACP receipt runs after it"
  (niwa-acp-turn-test--with-receipt-race :false
    (let ((process (alist-get :process (niwa-acp-client connection))))
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
               (lambda (result) (setq tool-result result)))))

  :doc "a tool call read before an already written ACP receipt still runs after it"
  (niwa-acp-turn-test--with-receipt-race "late"
    ;; Release the peer only after "waiting" was read.  Observe its write
    ;; without dispatching ACP I/O, then read the tool socket first.
    (with-temp-file receipt-release)
    (let ((deadline (+ (float-time) 5)))
      (while (and (not (file-exists-p receipt-written)) (< (float-time) deadline))
        (thread-yield)))
    (should (file-exists-p receipt-written))
    (funcall dispatch "Read" (list :file_path file)
             (list :claudecode/toolUseId "order-1")
             (lambda (result) (setq tool-result result))))

  :doc "a tool call that overtakes its announcement keeps earlier text before its row"
  (niwa-engine-test--with-session
    (let ((path (file-name-concat root "source.txt")))
      (write-region "evidence" nil path nil 'silent)
      (niwa-acp-turn-start
       request
       (lambda (mcp _hook)
         (list :command (executable-find "python3") :args (list niwa-test--acp-peer)
               :cwd root :mcp mcp :tool-id-field :claudecode/toolUseId
               :announces-tool-calls t
               :meta `((lateAnnouncement . t)
                       (toolBatches . [[((name . "Read") (args . ((file_path . ,path)))
                                         (id . "first"))]]))))
       [((type . "text") (text . "Read once"))] (list (niwa-tool-ensure "Read")))
      (niwa-test--await 5 "Turn did not settle" (not (niwa-turn-busy-p buffer)))
      (let ((text (string-search "Text streamed before first." (buffer-string)))
            (row (string-search "#+begin_tool (Read" (buffer-string))))
        (should text)
        (should row)
        (should (< text row))))))

(niwa-deftest niwa-acp-turn-start/announcement-timeout
  (:quiet t :doc "missing announcements are bounded and disconnected calls stay inert")
  (dolist (disconnect '(nil t))
    (niwa-engine-test--with-session
      (let* ((path (file-name-concat root "source.txt"))
             (start (symbol-function 'niwa-mcp-start))
             (niwa-acp-turn--announce-wait 0.03)
             (calls 0) dispatch result
             (gptel-pre-tool-call-functions
              (list (lambda (_) (cl-incf calls) nil))))
        (write-region "evidence" nil path nil 'silent)
        (cl-letf (((symbol-function 'niwa-mcp-start)
                   (lambda (tools callback &rest rest)
                     (setq dispatch callback)
                     (apply start tools callback rest))))
          (niwa-acp-turn-start
           request
           (lambda (mcp _hook)
             (list :command (executable-find "python3")
                   :args (list niwa-test--acp-peer) :cwd root :mcp mcp
                   :tool-id-field :fixtureToolId :announces-tool-calls t))
           [((type . "text") (text . "wait"))]
           (list (niwa-tool-ensure "Read"))))
        (niwa-test--await 5 "Turn did not start"
          (string-search "waiting" (buffer-string)))
        (let ((cancel (funcall dispatch "Read" (list :file_path path)
                               '(:fixtureToolId "unannounced")
                               (lambda (value) (setq result value)))))
          (should (= 0 calls))
          (should-not result)
          (if disconnect
              (progn
                (funcall cancel)
                (sleep-for 0.1)
                (should (= 0 calls))
                (should-not result))
            (niwa-test--await 5 "Unannounced call did not time out" result)
            (should (= 1 calls))
            (should-not (eq t (plist-get result :isError)))))
        (niwa-abort buffer)
        (niwa-test--await 5 "Turn did not settle"
          (not (niwa-turn-busy-p buffer)))))))

(niwa-deftest niwa-acp-turn-start/progress-token
  (:quiet t :doc "a call without an MCP progress token marks its owner's waits as idle-bounded")
  (niwa-engine-test--with-session
    (let* ((path (file-name-concat root "source.txt"))
           (start (symbol-function 'niwa-mcp-start))
           (niwa-acp-turn--announce-wait 0.03)
           dispatch seen)
      (write-region "evidence" nil path nil 'silent)
      (cl-letf (((symbol-function 'niwa-mcp-start)
                 (lambda (tools callback &rest rest)
                   (setq dispatch callback)
                   (apply start tools callback rest))))
        (niwa-acp-turn-start
         request
         (lambda (mcp _hook)
           (list :command (executable-find "python3")
                 :args (list niwa-test--acp-peer) :cwd root :mcp mcp
                 :tool-id-field :fixtureToolId :announces-tool-calls t))
         [((type . "text") (text . "wait"))]
         (list (niwa-tool-ensure "Read"))))
      (niwa-test--await 5 "Turn did not start"
        (string-search "waiting" (buffer-string)))
      (dolist (meta '((:fixtureToolId "silent") (:fixtureToolId "progress" :progressToken 7)))
        (let (result)
          (funcall dispatch "Read" (list :file_path path) meta
                   (lambda (value) (setq result value)))
          (niwa-test--await 5 "Call did not finish" result)
          (push (plist-get (niwa-engine-info request) :niwa-mcp-idle-bounded) seen)))
      (should (equal '(nil t) seen))
      (niwa-abort buffer)
      (niwa-test--await 5 "Turn did not settle"
        (not (niwa-turn-busy-p buffer))))))

(niwa-deftest niwa-acp-turn-start/tool-hooks (:quiet t)
  ,test
  (test)

  :doc "post-tool stop prevents subsequent calls and preserves completed evidence"
  (niwa-engine-test--with-session
    (let* ((path (file-name-concat root "source.txt"))
           (calls 0)
           (gptel-pre-tool-call-functions (list (lambda (_) (cl-incf calls) nil)))
           (gptel-post-tool-call-functions
            (list (lambda (_) '(:stop t :stop-reason "Stop after evidence")))))
      (write-region "completed evidence" nil path nil 'silent)
      (niwa-acp-turn-start
       request
       (lambda (mcp _hook)
         (list :command (executable-find "python3") :args (list niwa-test--acp-peer)
               :cwd root :mcp mcp :tool-id-field :claudecode/toolUseId
               :meta `((toolBatches . [[((name . "Read") (args . ((file_path . ,path))) (id . "first"))
                                       ((name . "Read") (args . ((file_path . ,path))) (id . "second"))]]))))
       [((type . "text") (text . "Read twice"))] (list (niwa-tool-ensure "Read")))
      (with-timeout (5 (ert-fail "Hook stop did not settle"))
        (while (niwa-turn-busy-p buffer) (accept-process-output nil .01)))
      (should (= 1 calls))
      (should (string-search "completed evidence" (buffer-string)))
      (should (eq 'error (plist-get (niwa-engine-info request) :niwa-acp-outcome)))))

  :doc "post-tool replacement and blocking withhold original text and media from MCP and transcript"
  (dolist (decision '((:result "redacted") (:block "withheld")))
    (niwa-engine-test--with-session
      (let* ((path (file-name-concat root "source.txt"))
             (tool (copy-niwa-tool (niwa-tool-ensure "Read")))
             observed
             (gptel-post-tool-call-functions
              (list (lambda (_) decision)
                    (lambda (call) (setq observed (plist-get call :result)) '(:block nil))))
             payload
             (capture (lambda (_client line &rest _)
                        (let ((message (json-parse-string line :object-type 'plist
                                                          :false-object :json-false)))
                          (when (plist-get (plist-get message :result) :content)
                            (setq payload (plist-get message :result)))))))
        (write-region "PRIVATE EVIDENCE" nil path nil 'silent)
        (setf (niwa-tool-async-p tool) nil
              (niwa-tool-handler tool)
              (lambda (_args)
                '(:result "PRIVATE EVIDENCE" :media ((:mime "image/png" :data "QUJD")))))
        (advice-add 'niwa-mcp--write :before capture)
        (unwind-protect
            (progn
              (niwa-acp-turn-start
               request
               (lambda (mcp _hook)
                 (list :command (executable-find "python3") :args (list niwa-test--acp-peer)
                       :cwd root :mcp mcp :tool-id-field :fixtureToolId))
               (vector `((type . "text") (text . ,(concat "read:" path))))
               (list tool))
              (with-timeout (5 (ert-fail "Hook replacement did not settle"))
                (while (niwa-turn-busy-p buffer) (accept-process-output nil .01)))
              (let ((expected (or (plist-get decision :result) (plist-get decision :block))))
                (should (equal expected (plist-get (aref (plist-get payload :content) 0) :text)))
                (should (= 1 (length (plist-get payload :content))))
                (should (equal expected observed))
                (should (eq (if (plist-get decision :block) t :json-false) (plist-get payload :isError)))
                (should (string-search expected (buffer-string)))
                (should-not (string-search "PRIVATE EVIDENCE" (buffer-string)))))
          (advice-remove 'niwa-mcp--write capture)))))

  :doc "pre-tool arguments reach validation and later observers, while synthetic results skip execution"
  (dolist (synthetic '(nil t))
    (niwa-engine-test--with-session
      (let* ((path (file-name-concat root "source.txt"))
             (replacement (list :file_path path))
             observed
             (gptel-pre-tool-call-functions
              (list (lambda (_) (if synthetic '(:result "synthetic evidence")
                                  (list :args replacement)))
                    (lambda (call) (setq observed (plist-get call :args))))))
        (write-region "replacement evidence" nil path nil 'silent)
        (niwa-acp-turn-start
         request
         (lambda (mcp _hook)
           (list :command (executable-find "python3") :args (list niwa-test--acp-peer)
                 :cwd root :mcp mcp :tool-id-field :fixtureToolId))
         [((type . "text") (text . "read:/missing-original-file"))]
         (list (niwa-tool-ensure "Read")))
        (with-timeout (5 (ert-fail "Pre-hook did not settle"))
          (while (niwa-turn-busy-p buffer) (accept-process-output nil .01)))
        (should (string-search (if synthetic "synthetic evidence" "replacement evidence") (buffer-string)))
        (unless synthetic (should (equal replacement observed))))))

  :doc "an asynchronous result publication error closes the turn instead of stranding MCP"
  (niwa-engine-test--with-session
    (let* ((tool (copy-niwa-tool (niwa-tool-ensure "Read")))
           (path (file-name-concat root "source.txt"))
           complete)
      (write-region "evidence" nil path nil 'silent)
      (setf (niwa-tool-async-p tool) t
            (niwa-tool-handler tool) (lambda (callback _args) (setq complete callback)))
      (cl-letf (((symbol-function 'niwa-acp-turn--tool-result)
                 (lambda (&rest _) (error "Publication failed"))))
        (niwa-acp-turn-start
         request
         (lambda (mcp _hook)
           (list :command (executable-find "python3") :args (list niwa-test--acp-peer)
                 :cwd root :mcp mcp :tool-id-field :fixtureToolId))
         (vector `((type . "text") (text . ,(concat "read:" path)))) (list tool))
        (with-timeout (5 (ert-fail "Async call was not admitted"))
          (while (not complete) (accept-process-output nil .01)))
        (funcall complete '(:result "async evidence"))
        (with-timeout (5 (ert-fail "Publication failure stranded the turn"))
          (while (niwa-turn-busy-p buffer) (accept-process-output nil .01))))
      (should (eq 'error (plist-get (niwa-engine-info request) :niwa-acp-outcome)))))

  :doc "hook denials stay denied and unsupported controls fail before effects"
  (dolist (decision '((:block "denied") (:confirm t) (:confirm nil) (:name "Other")))
    (niwa-engine-test--with-session
      (let* ((tool (copy-niwa-tool (niwa-tool-ensure "Read")))
             (calls 0)
             (gptel-pre-tool-call-functions
              (list (lambda (_) decision) (lambda (_) '(:block nil)))))
        (setf (niwa-tool-handler tool)
              (lambda (&rest _) (cl-incf calls) '(:result "Forbidden execution")))
        (niwa-acp-turn-start
         request
         (lambda (mcp _hook)
           (list :command (executable-find "python3") :args (list niwa-test--acp-peer)
                 :cwd root :mcp mcp :tool-id-field :fixtureToolId))
         [((type . "text") (text . "read:/missing-original-file"))] (list tool))
        (with-timeout (5 (ert-fail "Hook denial did not settle"))
          (while (niwa-turn-busy-p buffer) (accept-process-output nil .01)))
        (should (= 0 calls))
        (if (plist-get decision :block)
            (should (string-search "denied" (buffer-string)))
          (should (eq 'error (plist-get (niwa-engine-info request) :niwa-acp-outcome))))))))

;; A reply written after its peer closed kills a batch Emacs that does not
;; ignore SIGPIPE; these turns run in the child the next case starts.
(when (niwa-test-sigpipe-child-p)
  (niwa-deftest niwa-acp-turn-start/lost-mcp-reply (:quiet t)
    ,test
    (test)

    :doc "an asynchronous result lost with its bridge stays local and the agent settles the turn"
    (niwa-engine-test--with-session
      (let ((tool (copy-niwa-tool (niwa-tool-ensure "Read")))
            (niwa-telemetry-enabled t)
            (barrier (file-name-concat root "reply"))
            (path (file-name-concat root "source.txt")))
        (write-region "evidence" nil path nil 'silent)
        (setf (niwa-tool-async-p tool) t
              (niwa-tool-handler tool)
              (lambda (callback _args)
                ;; The bridge dies while the tool works, and the result
                ;; arrives before Emacs reads that it closed.
                (run-at-time
                 0 nil (lambda ()
                         (niwa-test-release-reply-peer (concat barrier "-tool"))
                         (funcall callback '(:result "LOCAL-RESULT-5120"))))
                #'ignore))
        (niwa-acp-turn-start
         request
         (lambda (mcp _hook)
           (list :command (executable-find "python3") :args (list niwa-test--acp-peer)
                 :cwd root :mcp mcp :tool-id-field :claudecode/toolUseId
                 :meta `((dropBridge . t) (replyBarrier . ,barrier)
                         (responseText . "AGENT-OUTCOME-2291")
                         (toolBatches . [[((name . "Read") (id . "lost-1")
                                           (args . ((file_path . ,path))))]]))))
         [((type . "text") (text . "batch"))] (list tool))
        (niwa-test--await 10 "Lost reply turn did not settle"
          (not (niwa-turn-busy-p buffer)))
        (should (eq 'success (plist-get (niwa-engine-info request) :niwa-acp-outcome)))
        (should-not (plist-get (niwa-engine-info request) :error))
        (should (string-search "LOCAL-RESULT-5120" (buffer-string)))
        (should (string-search "AGENT-OUTCOME-2291" (buffer-string)))
        (should (equal '((:kind tool :id 2))
                       (plist-get (niwa-engine-info request) :niwa-mcp-lost)))
        (let ((events
               (with-temp-buffer
                 (insert-file-contents (niwa-telemetry-path session))
                 (goto-char (point-min))
                 (let (entries)
                   (condition-case nil
                       (while t (push (read (current-buffer)) entries))
                     (end-of-file))
                   entries))))
          (let ((event (cl-find 'mcp-reply-lost events
                                :key (lambda (entry) (plist-get entry :event)))))
            (should event)
            (should (eq 'tool (plist-get event :kind)))
            (should (equal (niwa-request-id request) (plist-get event :request-id)))
            (should-not (plist-get event :dropped-keys))))))

    :doc "a child's hook reply lost at the hook's timeout leaves its agent to settle the turn"
    (niwa-engine-test--with-session
      (let* ((path (file-name-concat root "source.txt"))
             (invocation
              (niwa-agent-invocation--create
               :buffer buffer :path "/root/lost-hook" :description "Lost hook child"
               :runtime-context (list :buffer buffer :niwa-request request)))
             (barrier (file-name-concat root "reply"))
             (controls 0) terminal-status)
        (write-region "evidence" nil path nil 'silent)
        (setq-local niwa--agent-invocation invocation)
        (niwa-acp-turn-start
         invocation
         (lambda (mcp hook)
           (list :command (executable-find "python3") :args (list niwa-test--acp-peer)
                 :cwd root :mcp mcp :tool-id-field :claudecode/toolUseId
                 :control (lambda (_owner _event)
                            (cl-incf controls)
                            ;; The native hook gives up while this answers.
                            (niwa-test-release-reply-peer
                             (format "%s-hook-%d" barrier controls))
                            '(:continue t))
                 :meta `((hookCommand . ,hook) (replyBarrier . ,barrier)
                         (responseText . "CHILD-OUTCOME-6620")
                         (toolBatches . [[((name . "Read") (id . "child-read")
                                           (args . ((file_path . ,path))))]]))))
         [((type . "text") (text . "batch"))] (list (niwa-tool-ensure "Read"))
         nil nil
         (lambda (status)
           (setq terminal-status status)
           (setf (niwa-agent-invocation-runtime-settled-p invocation) t)))
        (niwa-test--await 10 "Lost hook child did not settle" terminal-status)
        (should (= 1 controls))
        (should (eq 'success terminal-status))
        (should-not (plist-get (niwa-engine-info invocation) :error))
        (should (string-search "CHILD-OUTCOME-6620" (buffer-string)))
        (should (equal '((:kind hook :id 1))
                       (plist-get (niwa-engine-info invocation) :niwa-mcp-lost)))))

    :doc "a lost hook reply leaves its staged context undelivered, with no receipt"
    (niwa-engine-test--with-claude-session
      (let* ((directory (file-name-concat root "lib"))
             (file (file-name-concat directory "evidence.txt"))
             (barrier (file-name-concat root "reply"))
             (hooks 0)
             (hold (lambda (prepare &rest args)
                     (prog1 (apply prepare args)
                       (unless (nth 1 args)
                         (niwa-test-release-reply-peer
                          (format "%s-hook-%d" barrier (cl-incf hooks))))))))
        (make-directory directory)
        (write-region "Evidence" nil file nil 'silent)
        (write-region "PATH-GUIDANCE-4319" nil (file-name-concat directory "AGENTS.md") nil 'silent)
        (setq-local gptel-system-prompt "Lost hook fixture"
                    gptel-tools (list (niwa-tool-gptel-tool (niwa-tool-ensure "Read"))))
        (advice-add 'niwa-claude-code-context-prepare :around hold)
        (unwind-protect
            (cl-letf (((symbol-function 'niwa-claude-code-launch)
                       (niwa-engine-test--claude-launch
                        (lambda (_system _mcp _model _effort &optional _id hook)
                          (list :observe #'niwa-claude-code-context-observe
                                :meta `((hookCommand . ,hook) (replyBarrier . ,barrier)
                                        (responseText . "Done")
                                        (toolBatches . [[((name . "Read") (id . "first-read")
                                                          (args . ((file_path . ,file))))]
                                                        [((name . "Read") (id . "second-read")
                                                          (args . ((file_path . ,file))))]])))))))
              (insert "Read with applicable instructions")
              (niwa--send-request "Read with applicable instructions")
              (niwa-test--await 10 "Lost hook turn did not settle"
                (not (niwa-turn-busy-p buffer))))
          (advice-remove 'niwa-claude-code-context-prepare hold))
        (let ((info (niwa-engine-info request)))
          (ert-info ((format "outcome=%S error=%S" (plist-get info :niwa-acp-outcome)
                             (plist-get info :error)))
            (should (equal '((:kind hook :id 1)) (plist-get info :niwa-mcp-lost)))
            (should-not (string-search "no longer connected"
                                       (format "%S" (plist-get info :error))))))
        ;; Without Claude's receipt the guidance was never delivered.
        (should (= 0 (niwa-engine-test--count-evidence "PATH-GUIDANCE-4319")))
        (should-not (niwa-session-workspace-instruction-hashes session))))))

(niwa-deftest niwa-acp-turn-start/lost-mcp-reply-interactive ()
  ,test
  (test)
  :doc "turns whose MCP replies lose their client, where SIGPIPE is ignored"
  (niwa-test-run-sigpipe-tests niwa-acp-turn-test--file
                                  "\\`niwa-acp-turn-start/lost-mcp-reply/"))

(niwa-deftest niwa-acp-turn-start/adapter-exit-telemetry (:quiet t)
  ,test
  (test)

  :doc "a continuation send and the adapter's exit are recorded with timing, not content"
  (niwa-engine-test--with-session
    (let ((niwa-telemetry-enabled t) (sent 0))
      (niwa-acp-turn-start
       request
       (lambda (mcp _hook)
         (list :command (executable-find "python3") :args (list niwa-test--acp-peer)
               :cwd root :mcp mcp
               ;; The first outcome hands over a queued message; the peer dies on it.
               :complete-prompt
               (lambda (_owner outcome)
                 (if (> (cl-incf sent) 1) outcome
                   (sleep-for 0.2)
                   (append outcome '(:queued-handover t
                                     :next-prompt [((type . "text") (text . "crash"))]))))))
       [((type . "text") (text . "report-effort"))] nil)
      (niwa-test--await 5 "Turn did not settle" (not (niwa-turn-busy-p buffer)))
      (let* ((events (with-temp-buffer
                       (insert-file-contents (niwa-telemetry-path session))
                       (let (entries)
                         (condition-case nil (while t (push (read (current-buffer)) entries))
                           (end-of-file))
                         (nreverse entries))))
             (sends (seq-filter (lambda (e) (eq 'acp-prompt-send (plist-get e :event))) events))
             (exit (cl-find 'acp-process-exit events :key (lambda (e) (plist-get e :event)))))
        (should (= 2 (length sends)))
        ;; Control: the first send is no continuation and follows no end_turn.
        (should-not (plist-get (car sends) :continuation))
        (should-not (plist-get (car sends) :since-end-turn-ms))
        (should (plist-get (cadr sends) :continuation))
        (should (plist-get (cadr sends) :queued-handover))
        (should (integerp (plist-get (cadr sends) :since-end-turn-ms)))
        (should (>= (plist-get (cadr sends) :since-end-turn-ms) 200))
        (should (= 7 (plist-get exit :adapter-exit-code)))
        ;; The failure that the exit triggers must not restart the end_turn clock.
        (should (>= (plist-get exit :since-end-turn-ms)
                    (+ 200 (plist-get exit :since-send-ms))))
        (should-not (cl-some (lambda (e) (plist-get e :dropped-keys)) events))))))

(niwa-deftest niwa-acp-turn-start/resend-telemetry (:quiet t)
  ,test
  (test)

  :doc "the automatic resend of a refused prompt is recorded as its own send"
  (niwa-engine-test--with-session
    (let ((niwa-telemetry-enabled t))
      (niwa-acp-turn-start
       request
       (lambda (mcp _hook)
         (list :command (executable-find "python3") :args (list niwa-test--acp-peer)
               :cwd root :mcp mcp
               :prompt-retry-delay (lambda (failure)
                                     (and (string-search "refreshing it" (alist-get 'message failure))
                                          0.05))))
       [((type . "text") (text . "refresh-race"))] nil)
      (niwa-test--await 5 "Turn did not settle" (not (niwa-turn-busy-p buffer)))
      (let* ((events (with-temp-buffer
                       (insert-file-contents (niwa-telemetry-path session))
                       (let (entries)
                         (condition-case nil (while t (push (read (current-buffer)) entries))
                           (end-of-file))
                         (nreverse entries))))
             (sends (seq-filter (lambda (e) (eq 'acp-prompt-send (plist-get e :event))) events)))
        (should (= 2 (length sends)))
        (should-not (plist-get (car sends) :resend))
        (should (plist-get (cadr sends) :resend))
        (should (>= (plist-get (cadr sends) :since-send-ms) 50))))))

(provide 'test-niwa-acp-turn)
;;; test-niwa-acp-turn.el ends here
