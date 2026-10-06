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
                           (plist-get (mevedel-engine-info request) :error)))
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

(provide 'test-mevedel-acp-turn)
;;; test-mevedel-acp-turn.el ends here
