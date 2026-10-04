;;; test-mevedel-telemetry-usage.el --- Per-call usage tests -*- lexical-binding: t -*-

;;; Commentary:

;; Exercise normalized accounting and native gptel dispatch/response ordering
;; without contacting a provider.  Synthetic curl/URL responses still pass
;; through gptel's installed parsers and completion handlers.

;;; Code:

(require 'gptel)
(require 'gptel-anthropic)
(require 'gptel-openai)
(require 'gptel-openai-responses)
(require 'mevedel-agents)
(require 'mevedel-compact)
(require 'mevedel-telemetry-usage)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(defmacro test-mevedel-usage--with-session (&rest body)
  "Evaluate BODY with an isolated SESSION, ROOT and DATA-BUF."
  (declare (indent 0) (debug t))
  `(let* ((root (make-temp-file "mevedel-call-usage-" t))
          (workspace (mevedel-workspace--create
                      :id root :root root :name "usage" :type 'file
                      :file-cache (mevedel-test-file-cache-create)))
          (session (mevedel-session--create
                    :session-id "usage-session" :name "usage"
                    :workspace workspace :turn-count 7))
          (data-buf (generate-new-buffer " *mevedel-call-usage*"))
          (mevedel-telemetry-enabled t)
          ;; Native JSON/SSE fixtures below use Chat Completions, independently
          ;; of the backend other suites or the runner choose as their default.
          (gptel-backend (gptel--make-openai
                          :name "usage-chat" :key "synthetic"
                          :host "example.invalid" :protocol "https" :stream t
                          :endpoint "/v1/chat/completions"
                          :url "https://example.invalid/v1/chat/completions"
                          :models '(mevedel-test-model)))
          (gptel-log-level nil)
          (gptel-post-request-hook nil)
          (gptel--request-alist nil))
     (unwind-protect
         (progn
           (with-current-buffer data-buf
             (setq-local mevedel--session session
                         gptel-post-request-hook nil))
           ,@body)
       (when (buffer-live-p data-buf) (kill-buffer data-buf))
       (delete-directory root t))))

(defmacro test-mevedel-usage--with-observer (&rest body)
  "Evaluate BODY with the observer installed, restoring its prior state."
  (declare (indent 0) (debug t))
  `(let ((dispatch-installed
          (advice-member-p #'mevedel-telemetry-usage--dispatch
                           'gptel--handle-wait))
         (curl-parser-installed
          (advice-member-p #'mevedel-telemetry-usage--parse-response
                           'gptel-curl--parse-response))
         (url-parser-installed
          (advice-member-p #'mevedel-telemetry-usage--parse-response
                           'gptel--url-parse-response))
         (cleanup-installed
          (advice-member-p #'mevedel-telemetry-usage--stream-cleanup
                           'gptel-curl--stream-cleanup))
         (parser-installed
          (advice-member-p #'mevedel-telemetry-usage--parse-stream
                           'gptel-curl--parse-stream))
         (abort-installed
          (advice-member-p #'mevedel-telemetry-usage--abort
                           'gptel--fsm-transition)))
     (unwind-protect
         (progn (mevedel-telemetry-usage-install) ,@body)
       (mevedel-telemetry-usage-uninstall)
       (when dispatch-installed
         (advice-add 'gptel--handle-wait :around #'mevedel-telemetry-usage--dispatch))
       (when curl-parser-installed
         (advice-add 'gptel-curl--parse-response :around
                     #'mevedel-telemetry-usage--parse-response))
       (when url-parser-installed
         (advice-add 'gptel--url-parse-response :around
                     #'mevedel-telemetry-usage--parse-response))
       (when cleanup-installed
         (advice-add 'gptel-curl--stream-cleanup :around
                     #'mevedel-telemetry-usage--stream-cleanup))
       (when parser-installed
         (advice-add 'gptel-curl--parse-stream :around
                     #'mevedel-telemetry-usage--parse-stream))
       (when abort-installed
         (advice-add 'gptel--fsm-transition :before
                     #'mevedel-telemetry-usage--abort)))))

(defun test-mevedel-usage--events (session &optional stage)
  "Return SESSION's per-call events, oldest first, optionally for STAGE."
  (seq-filter
   (lambda (event)
     (and (eq (plist-get event :event) 'provider-call)
          (or (not stage) (eq (plist-get event :stage) stage))))
   (reverse (mevedel-session-telemetry-pending session))))

(defun test-mevedel-usage--pre-ec25a41-curl-failure (process _status)
  "Fail PROCESS the way gptel did before commit ec25a41.
That cleanup transitioned before assigning its error, so a pending tool call
reached TOOL and could dispatch the next call from inside the cleanup."
  (let* ((fsm (car (alist-get process gptel--request-alist)))
         (info (gptel-fsm-info fsm)))
    (gptel--fsm-transition fsm)
    (plist-put info :error (format "Curl failed with exit code %d."
                                   (process-exit-status process)))
    (funcall (plist-get info :callback) nil info)
    (gptel--fsm-transition fsm)
    (setf (alist-get process gptel--request-alist nil 'remove) nil)
    (kill-buffer (process-buffer process))))

(defun test-mevedel-usage--fsm (buffer &rest props)
  "Create a quiet native gptel FSM for BUFFER with additional PROPS."
  (let ((info (list :buffer buffer :backend gptel-backend
                    :model 'mevedel-test-model :data (list :messages [])
                    :callback #'ignore :tokens nil :tokens-full nil
                    :history nil :mevedel-request-id "request-1")))
    (while props
      (setq info (plist-put info (pop props) (pop props))))
    (gptel-make-fsm :state 'WAIT :handlers nil :info info)))

(defun test-mevedel-usage--call (session &rest props)
  "Create a test call owned by SESSION with additional PROPS."
  (let ((call (list :session session :turn 7 :started-at 100.0
                    :finished nil :identity '(:call-id "call-1" :workload root))))
    (while props
      (setq call (plist-put call (pop props) (pop props))))
    call))

(defun test-mevedel-usage--json-response (&optional tool-only)
  "Return an OpenAI JSON response, with only a tool call when TOOL-ONLY."
  (gptel--json-encode
   (list :choices
         (vector
          (list :finish_reason (if tool-only "tool_calls" "stop")
                :message
                (if tool-only
                    '(:role "assistant" :content :null
                      :tool_calls [(:id "tool-1" :type "function"
                                    :function (:name "Synthetic" :arguments "{}"))])
                  '(:role "assistant" :content "synthetic response"))))
         :usage '(:prompt_tokens 100 :completion_tokens 2
                  :prompt_tokens_details (:cached_tokens 80)))))

(mevedel-deftest mevedel-telemetry-usage-normalize ()
  ,test
  (test)
  :doc "normalization preserves zero and distinguishes unknown and invalid fields"
  (dolist (case '((nil missing nil nil nil nil)
                  ((:input 0 :cached 0 :output 0) complete 0 0 nil 0)
                  ((:input 20) partial 20 nil nil nil)
                  ((:cache 5) missing nil nil 5 nil)
                  ((:input 30 :cached 80 :cache 10 :output 2) complete 30 80 10 2)
                  ((:input -1 :cached 5 :output 2) invalid nil 5 nil 2)
                  ((:input 1.5 :cached "8" :cache -1 :output 2) invalid nil nil nil 2)
                  ((:input 3 :cached 0 :cache 4 :output 1) invalid 3 0 nil 1)))
    (pcase-let ((`(,tokens ,status ,input ,cached ,write ,output) case))
      (let ((usage (mevedel-telemetry-usage-normalize tokens)))
        (should (eq status (plist-get usage :usage-status)))
        (should (equal input (plist-get usage :input-tokens)))
        (should (equal cached (plist-get usage :cached-tokens)))
        (should (equal write (plist-get usage :cache-write-tokens)))
        (should (equal output (plist-get usage :output-tokens))))))

  :doc "normalization rejects malformed, odd-length, dotted and circular usage lists"
  (let ((circular (list :input 20)))
    (setcdr (last circular) circular)
    (dolist (tokens (append '(t 42 "tokens" [:input 20]
                             (:input) (:input 20 :cached)
                             (:input 20 . dangling))
                           (list circular)))
      (let ((usage (mevedel-telemetry-usage-normalize tokens)))
        (should (eq 'invalid (plist-get usage :usage-status)))
        (dolist (field '(:input-tokens :cached-tokens :cache-write-tokens :output-tokens))
          (should-not (plist-get usage field))))))

  :doc "normalization uses installed Anthropic accounting without adding writes twice"
  (let ((info (list :tokens nil :tokens-full nil)))
    (gptel--anthropic-update-tokens
     '(:input_tokens 20 :cache_creation_input_tokens 10
       :cache_read_input_tokens 80 :output_tokens 2) info)
    (let ((usage (mevedel-telemetry-usage-normalize (plist-get info :tokens))))
      (should (= 30 (plist-get usage :input-tokens)))
      (should (= 10 (plist-get usage :cache-write-tokens)))
      (should (= 80 (plist-get usage :cached-tokens)))
      (should (= 2 (plist-get usage :output-tokens)))
      (should (eq 'complete (plist-get usage :usage-status)))))

  :doc "normalization preserves installed OpenAI Responses accounting"
  (let ((info (list :tokens nil :tokens-full nil)))
    (gptel--openai-responses-update-tokens
     '(:input_tokens 100 :output_tokens 2
       :input_tokens_details (:cached_tokens 80)) info)
    (let ((usage (mevedel-telemetry-usage-normalize (plist-get info :tokens))))
      (should (eq 'complete (plist-get usage :usage-status)))
      (should (= 20 (plist-get usage :input-tokens)))
      (should (= 80 (plist-get usage :cached-tokens)))
      (should (= 2 (plist-get usage :output-tokens)))
      (should-not (plist-get usage :cache-write-tokens))))

  :doc "normalization acknowledges upstream OpenAI subtraction and omitted-field zeros"
  (let ((info (list :tokens nil :tokens-full nil)))
    (gptel--openai-update-tokens
     '(:prompt_tokens 100 :completion_tokens 2
       :prompt_tokens_details (:cached_tokens 80)) info)
    (let ((usage (mevedel-telemetry-usage-normalize (plist-get info :tokens))))
      (should (= 20 (plist-get usage :input-tokens)))
      (should (= 80 (plist-get usage :cached-tokens)))
      (should-not (plist-get usage :cache-write-tokens)))
    (gptel--openai-update-tokens '(:prompt_tokens 10) info)
    (let ((usage (mevedel-telemetry-usage-normalize (plist-get info :tokens))))
      (should (eq 'complete (plist-get usage :usage-status)))
      (should (= 0 (plist-get usage :cached-tokens)))
      (should (= 0 (plist-get usage :output-tokens))))))

(mevedel-deftest mevedel-telemetry-usage--owner ()
  ,test
  (test)
  :doc "ownership resolves root, retained-agent, and explicit helper sessions"
  (test-mevedel-usage--with-session
    (should (eq session (mevedel-telemetry-usage--owner (list :buffer data-buf))))
    (with-temp-buffer
      (setq-local mevedel--agent-invocation
                  (mevedel-agent-invocation--create
                   :parent-session session :path "/root/worker"))
      (should (eq session (mevedel-telemetry-usage--owner
                           (list :buffer (current-buffer))))))
    (should (eq session (mevedel-telemetry-usage--owner
                         (list :context
                               (list :mevedel-telemetry-session session))))))

  :doc "ownership excludes ordinary gptel, killed buffers, sessionless and btw helpers"
  (test-mevedel-usage--with-session
    (with-temp-buffer
      (should-not (mevedel-telemetry-usage--owner (list :buffer (current-buffer)))))
    (should-not (mevedel-telemetry-usage--owner nil))
    (should-not (mevedel-telemetry-usage--owner
                 '(:context (:mevedel-telemetry-session nil))))
    (let ((side (mevedel-session--create :audit-session session)))
      (with-temp-buffer
        (setq-local mevedel--session side)
        (should-not (mevedel-telemetry-usage--owner
                     (list :buffer (current-buffer)))))
      (should-not (mevedel-telemetry-usage--owner
                   (list :context (list :mevedel-telemetry-session side)))))
    (kill-buffer data-buf)
    (should-not (mevedel-telemetry-usage--owner (list :buffer data-buf)))))

(mevedel-deftest mevedel-telemetry-usage--emit ()
  ,test
  (test)
  :doc "emission freezes the turn and filters payload properties"
  (test-mevedel-usage--with-session
    (let ((call (test-mevedel-usage--call session :turn 8)))
      (setf (mevedel-session-turn-count session) 99)
      (mevedel-telemetry-usage--emit
       call 'start :command "SECRET" :response "SECRET")
      (let ((event (car (test-mevedel-usage--events session))))
        (should (= 8 (plist-get event :turn)))
        (should (equal "usage-session" (plist-get event :session-id)))
        (should (equal "call-1" (plist-get event :call-id)))
        (should (eq 'start (plist-get event :stage)))
        (should-not (string-match-p "SECRET" (prin1-to-string event))))))

  :doc "emission honors the global disable switch"
  (test-mevedel-usage--with-session
    (let ((mevedel-telemetry-enabled nil))
      (should-not (mevedel-telemetry-usage--emit
                   (test-mevedel-usage--call session) 'start)))
    (should-not (mevedel-session-telemetry-pending session))))

(mevedel-deftest mevedel-telemetry-usage--finish ()
  ,test
  (test)
  :doc "finish records per-call usage and duration once, never tokens-full"
  (test-mevedel-usage--with-session
    (let ((call (test-mevedel-usage--call session))
          (info '(:tokens (:input 30 :cached 80 :cache 10 :output 2)
                  :tokens-full (:input 900 :cached 800 :output 100)
                  :error "SECRET provider prose")))
      (cl-letf (((symbol-function 'mevedel-telemetry--monotonic-now)
                 (lambda () 100.125)))
        (mevedel-telemetry-usage--finish call info 'success)
        (mevedel-telemetry-usage--finish call nil 'error))
      (should (plist-get call :finished))
      (let ((events (test-mevedel-usage--events session 'finish)))
        (should (= 1 (length events)))
        (should (= 125 (plist-get (car events) :duration-ms)))
        (should (= 30 (plist-get (car events) :input-tokens)))
        (should (= 2 (plist-get (car events) :output-tokens)))
        (should (eq 'success (plist-get (car events) :outcome)))
        (should-not (string-match-p "SECRET" (prin1-to-string events))))))

  :doc "finish guards replacement calls sharing old completion info without blocking concurrent calls"
  (test-mevedel-usage--with-session
    (let* ((old-call (test-mevedel-usage--call session))
           (new-call (test-mevedel-usage--call
                      session :identity '(:call-id "replacement" :workload root)))
           (concurrent-call (test-mevedel-usage--call
                             session :identity '(:call-id "concurrent" :workload agent)))
           (info (list :tokens '(:input 20 :cached 80 :output 2)))
           (concurrent-info (list :tokens '(:input 3 :cached 4 :output 5)))
           (mevedel-telemetry-usage--completion-owner (cons info old-call)))
      (mevedel-telemetry-usage--finish new-call info 'success)
      (should-not (plist-get new-call :finished))
      (should-not (test-mevedel-usage--events session))
      (mevedel-telemetry-usage--finish old-call info 'error)
      (mevedel-telemetry-usage--finish concurrent-call concurrent-info 'success)
      (should (plist-get concurrent-call :finished))
      (let ((finishes (test-mevedel-usage--events session 'finish)))
        (should (= 2 (length finishes)))
        (should (equal '("call-1" "concurrent")
                       (mapcar (lambda (event) (plist-get event :call-id)) finishes)))
        (should (= 3 (plist-get (cadr finishes) :input-tokens))))))

  :doc "finish with missing usage remains unknown and clamps negative duration"
  (test-mevedel-usage--with-session
    (let ((call (test-mevedel-usage--call session)))
      (cl-letf (((symbol-function 'mevedel-telemetry--monotonic-now)
                 (lambda () 99.0)))
        (mevedel-telemetry-usage--finish call nil 'aborted))
      (let ((event (car (test-mevedel-usage--events session))))
        (should (= 0 (plist-get event :duration-ms)))
        (should (eq 'missing (plist-get event :usage-status)))
        (should-not (plist-get event :input-tokens)))))

  :doc "finish retains a failed append for ordinary retry without duplicate completion"
  (test-mevedel-usage--with-session
    (let ((call (test-mevedel-usage--call session))
          (blocked (file-name-concat root "blocked")) diagnostics)
      (write-region "not a directory" nil blocked nil 'silent)
      (setf (mevedel-session-save-path session) blocked)
      (mevedel-test--with-captured-diagnostics diagnostics
        (mevedel-telemetry-usage--finish call nil 'error))
      (should diagnostics)
      (should (plist-get call :finished))
      (mevedel-telemetry-usage--finish call nil 'error)
      (should (= 1 (length (test-mevedel-usage--events session))))
      (setf (mevedel-session-save-path session) (file-name-concat root "saved"))
      (mevedel-telemetry-flush session)
      (should-not (mevedel-session-telemetry-pending session))
      (with-temp-buffer
        (insert-file-contents
         (file-name-concat root "saved" mevedel-telemetry-file-name))
        (should (= 1 (count-lines (point-min) (point-max))))))))

(mevedel-deftest mevedel-telemetry-usage--callback ()
  ,test
  (test)
  :doc "stream callback ignores chunks, reasoning, tool notifications and raw steering"
  (test-mevedel-usage--with-session
    (let ((call (test-mevedel-usage--call session))
          (info '(:tokens (:input 20 :cached 80 :output 2))) received)
      (cl-labels ((receive (&rest args) (push args received) 'returned))
        (dolist (response '("chunk" (reasoning . "thinking")
                            (tool-call . nil) (tool-result . nil)))
          (should (eq 'returned (mevedel-telemetry-usage--callback
                                call #'receive t response info))))
        (should (eq 'returned (mevedel-telemetry-usage--callback
                              call #'receive t "steered" info 'raw)))
        (should (equal (car received) (list "steered" info 'raw)))
        (should-not (plist-get call :finished))
        (should-not (test-mevedel-usage--events session))
        (mevedel-telemetry-usage--callback call #'receive t t info)
        (mevedel-telemetry-usage--callback call #'receive t t info)
        (should (= 1 (length (test-mevedel-usage--events session 'finish)))))))

  :doc "callback always forwards an old completion routed through a replacement observer"
  (test-mevedel-usage--with-session
    (let* ((old-call (test-mevedel-usage--call session))
           (new-call (test-mevedel-usage--call session))
           (info (list :tokens '(:input 3 :cached 4 :output 5)))
           (mevedel-telemetry-usage--completion-owner (cons info old-call))
           received)
      (should (eq 'forwarded
                  (mevedel-telemetry-usage--callback
                   new-call (lambda (&rest args) (setq received args) 'forwarded)
                   t nil info)))
      (should (equal (list nil info) received))
      (should-not (plist-get new-call :finished))
      (should-not (test-mevedel-usage--events session))))

  :doc "nonstream raw steering is passed through without settling the call"
  (test-mevedel-usage--with-session
    (let ((call (test-mevedel-usage--call session)) received)
      (should (eq 'returned
                  (mevedel-telemetry-usage--callback
                   call (lambda (&rest args) (setq received args) 'returned)
                   nil "steered" nil 'raw)))
      (should (equal '("steered" nil raw) received))
      (should-not (plist-get call :finished))))

  :doc "callback settles before callbacks can kill buffers or replace normalized usage"
  (test-mevedel-usage--with-session
    (let ((call (test-mevedel-usage--call session))
          (info '(:tokens (:input 20 :cached 80 :output 2))))
      (mevedel-telemetry-usage--callback
       call (lambda (_response callback-info)
              (should (= 1 (length (test-mevedel-usage--events session 'finish))))
              (plist-put callback-info :tokens '(:input 999 :cached 0 :output 999))
              (kill-buffer data-buf)
              'callback-return)
       nil "complete" info)
      (let ((event (car (test-mevedel-usage--events session 'finish))))
        (should (= 20 (plist-get event :input-tokens)))
        (should (= 2 (plist-get event :output-tokens))))))

  :doc "callback distinguishes abort, HTTP/provider error and tool-only success"
  (test-mevedel-usage--with-session
    (dolist (case '((abort nil aborted)
                    (nil (:error "SECRET") error)
                    (nil (:http-status "429") error)
                    (nil (:http-status "200" :tool-use ((:name "Tool"))) success)))
      (pcase-let ((`(,response ,info ,outcome) case))
        (let ((call (test-mevedel-usage--call session)))
          (mevedel-telemetry-usage--callback call #'ignore nil response info)
          (should (eq outcome (plist-get (car (last (test-mevedel-usage--events
                                                     session 'finish)))
                                        :outcome))))))))

(mevedel-deftest mevedel-telemetry-usage--parse-stream ()
  ,test
  (test)
  :doc "parser observation passes unowned arguments, mutation and return value through"
  (let ((info (list :error nil)) received)
    (should (eq 'parsed
                (mevedel-telemetry-usage--parse-stream
                 (lambda (backend state)
                   (setq received (list backend state))
                   (plist-put state :error "Unowned provider error")
                   'parsed)
                 'synthetic-backend info)))
    (should (equal (list 'synthetic-backend info) received))
    (should (equal "Unowned provider error" (plist-get info :error))))

  :doc "parser observation does not adopt an unchanged inherited cleanup error"
  (test-mevedel-usage--with-session
    (let* ((call (test-mevedel-usage--call session :provider-error nil))
           (info (list :mevedel-usage-call call :error "Inherited cleanup failure")))
      (should (equal "chunk"
                     (mevedel-telemetry-usage--parse-stream
                      (lambda (_backend _info) "chunk") gptel-backend info)))
      (should-not (plist-get call :provider-error))
      (should-not (test-mevedel-usage--events session))))

  :doc "parser observation flags and rethrows a parser exception without retaining its payload"
  (test-mevedel-usage--with-session
    (let* ((call (test-mevedel-usage--call session :provider-error nil))
           (info (list :mevedel-usage-call call)))
      (should (equal '(error "Synthetic parser failure")
                     (should-error
                      (mevedel-telemetry-usage--parse-stream
                       (lambda (_backend _info) (error "Synthetic parser failure"))
                       gptel-backend info))))
      (should (eq t (plist-get call :provider-error)))
      (should-not (plist-get call :error))
      (should-not (test-mevedel-usage--events session))))

  :doc "installed Responses parser marks HTTP200 provider errors and cleanup records no payload"
  (test-mevedel-usage--with-session
    (test-mevedel-usage--with-observer
      (let* ((backend (gptel--make-openai-responses :name "responses-usage"))
             (call (test-mevedel-usage--call session :provider-error nil :stream t))
             (fsm (test-mevedel-usage--fsm
                   data-buf :backend backend :mevedel-usage-call call
                   :http-status "200" :error nil))
             (info (gptel-fsm-info fsm)))
        (with-temp-buffer
          (insert "event: response.completed\n"
                  "data: {\"response\":{\"status\":\"completed\",\"usage\":{\"input_tokens\":100,"
                  "\"output_tokens\":2,\"input_tokens_details\":{\"cached_tokens\":80}}}}\n\n"
                  "event: error\n"
                  "data: {\"error\":{\"message\":\"Private synthetic provider error\"}}\n\n")
          (goto-char (point-min))
          (gptel-curl--parse-stream backend info))
        (should (plist-get info :error))
        (should (eq t (plist-get call :provider-error)))
        (should-not (plist-get call :finished))
        (setf (alist-get 'synthetic-process gptel--request-alist) (cons fsm #'ignore))
        (cl-letf (((symbol-function 'process-exit-status) (lambda (_process) 0)))
          (mevedel-telemetry-usage--stream-cleanup #'ignore 'synthetic-process "finished"))
        (let ((finish (car (test-mevedel-usage--events session 'finish))))
          (should (eq 'error (plist-get finish :outcome)))
          (should (= 20 (plist-get finish :input-tokens)))
          (should (eq 'complete (plist-get finish :usage-status)))
          (should-not (string-match-p "Private synthetic provider error"
                                      (prin1-to-string finish))))))))

(mevedel-deftest mevedel-telemetry-usage--stream-cleanup ()
  ,test
  (test)
  :doc "cleanup passes unowned transport arguments and return value through unchanged"
  (test-mevedel-usage--with-session
    (let (received)
      (should (eq 'delegated
                  (mevedel-telemetry-usage--stream-cleanup
                   (lambda (&rest args) (setq received args) 'delegated)
                   'unowned-process "finished")))
      (should (equal '(unowned-process "finished") received))
      (should-not (test-mevedel-usage--events session))))

  :doc "abnormal cleanup freezes counts and completion ownership before original mutation"
  (test-mevedel-usage--with-session
    (let* ((call (test-mevedel-usage--call session :stream t))
           (fsm (test-mevedel-usage--fsm
                 data-buf :mevedel-usage-call call
                 :tokens '(:input 20 :cached 80 :output 2)))
           (info (gptel-fsm-info fsm))
           (outer-owner (cons 'outer-info 'outer-call))
           (mevedel-telemetry-usage--completion-owner outer-owner))
      (setf (alist-get 'synthetic-process gptel--request-alist) (cons fsm #'ignore))
      (cl-letf (((symbol-function 'process-exit-status) (lambda (_process) 18)))
        (should
         (eq 'delegated
             (mevedel-telemetry-usage--stream-cleanup
              (lambda (process status)
                (should (eq 'synthetic-process process))
                (should (equal "failed" status))
                (should (plist-get call :finished))
                (should (eq info (car mevedel-telemetry-usage--completion-owner)))
                (should (eq call (cdr mevedel-telemetry-usage--completion-owner)))
                (plist-put info :tokens '(:input 999 :cached 0 :output 999))
                (mevedel-telemetry-usage--callback call #'ignore t nil info)
                'delegated)
              'synthetic-process "failed"))))
      (should (eq outer-owner mevedel-telemetry-usage--completion-owner))
      (let ((finishes (test-mevedel-usage--events session 'finish)))
        (should (= 1 (length finishes)))
        (should (eq 'error (plist-get (car finishes) :outcome)))
        (should (= 20 (plist-get (car finishes) :input-tokens)))
        (should (= 2 (plist-get (car finishes) :output-tokens))))))

  :doc "cleanup follows native HTTP and exit criteria even when inherited errors are present"
  (test-mevedel-usage--with-session
    (dolist (case '((0 "100" nil success) (0 "200" nil success)
                    (0 "403" nil error) (0 nil nil error)
                    (18 "200" nil error) (0 "200" t error)))
      (pcase-let* ((`(,exit ,http ,provider-error ,outcome) case)
                   (call (test-mevedel-usage--call session :provider-error provider-error))
                   (fsm (test-mevedel-usage--fsm
                         data-buf :mevedel-usage-call call :http-status http
                         :error "Inherited transport error")))
        (setf (alist-get 'synthetic-process gptel--request-alist) (cons fsm #'ignore))
        (cl-letf (((symbol-function 'process-exit-status) (lambda (_process) exit)))
          (mevedel-telemetry-usage--stream-cleanup #'ignore 'synthetic-process "finished"))
        (let ((finish (car (last (test-mevedel-usage--events session 'finish)))))
          (should (eq outcome (plist-get finish :outcome)))
          (should (eq 'missing (plist-get finish :usage-status)))))))

  :doc "normal cleanup captures transport success before callback work and restores scope after an error"
  (test-mevedel-usage--with-session
    (let* ((call (test-mevedel-usage--call session :stream t))
           (fsm (test-mevedel-usage--fsm
                 data-buf :mevedel-usage-call call :http-status "200"
                 :error "Inherited previous transport error"
                 :tokens '(:input 20 :cached 80 :output 2)))
           (info (gptel-fsm-info fsm))
           (outer-owner (cons 'outer-info 'outer-call))
           (mevedel-telemetry-usage--completion-owner outer-owner))
      (setf (alist-get 'synthetic-process gptel--request-alist) (cons fsm #'ignore))
      (cl-letf (((symbol-function 'process-exit-status) (lambda (_process) 0)))
        (should (equal '(error "Synthetic cleanup handler failure")
                       (should-error
                        (mevedel-telemetry-usage--stream-cleanup
                         (lambda (_process _status)
                           (should (plist-get call :finished))
                           (should (plist-get info :error))
                           (mevedel-telemetry-usage--callback call #'ignore t t info)
                           (error "Synthetic cleanup handler failure"))
                         'synthetic-process "finished")))))
      (should (eq outer-owner mevedel-telemetry-usage--completion-owner))
      (let ((finishes (test-mevedel-usage--events session 'finish)))
        (should (= 1 (length finishes)))
        (should (eq 'success (plist-get (car finishes) :outcome)))))))

(mevedel-deftest mevedel-telemetry-usage--parse-response ()
  ,test
  (test)
  :doc "response observation forwards unowned parser arguments and exact result unchanged"
  (let ((info (list :tokens nil))
        (result (list "Private response text" "200" "Private status")) received)
    (should (eq result
                (mevedel-telemetry-usage--parse-response
                 (lambda (&rest args) (setq received args) result)
                 'synthetic-backend info)))
    (should (equal (list 'synthetic-backend info) received)))

  :doc "response observation settles tool-only nil text with normalized counts for either parser arity"
  (test-mevedel-usage--with-session
    (dolist (url-parser '(nil t))
      (let* ((call (test-mevedel-usage--call session))
             (info (list :mevedel-usage-call call :tokens nil))
             (result (list nil "200" "OK"))
             (args (if url-parser (list gptel-backend info) (list info))))
        (should (eq result
                    (apply #'mevedel-telemetry-usage--parse-response
                           (lambda (&rest forwarded)
                             (should (equal args forwarded))
                             (should-not (plist-get call :finished))
                             (plist-put info :tokens '(:input 20 :cached 80 :output 2))
                             result)
                           args)))
        (should (plist-get call :finished))
        (plist-put info :tokens '(:input 999 :output 999))
        (let ((finish (car (last (test-mevedel-usage--events session 'finish)))))
          (should (= 20 (plist-get finish :input-tokens)))
          (should (= 2 (plist-get finish :output-tokens)))
          (should (eq 'success (plist-get finish :outcome)))))))

  :doc "response observation uses parser metadata rather than inherited error or status fields"
  (test-mevedel-usage--with-session
    (dolist (case '(("100" nil success) ("200" nil success)
                    ("429" nil error) (nil nil error) ("200" "SECRET" error)))
      (pcase-let* ((`(,http ,error ,outcome) case)
                   (call (test-mevedel-usage--call session))
                   (info (list :mevedel-usage-call call :tokens nil
                               :http-status "500" :error "Inherited error"))
                   (result (list "SECRET response" http "SECRET status" error)))
        (should (eq result (mevedel-telemetry-usage--parse-response
                            (lambda (_info) result) info)))
        (let ((finish (car (last (test-mevedel-usage--events session 'finish)))))
          (should (eq outcome (plist-get finish :outcome)))
          (should (eq 'missing (plist-get finish :usage-status)))
          (should-not (string-match-p "SECRET" (prin1-to-string finish)))))))

  :doc "response observation and late callbacks settle only once before callbacks can destroy ownership"
  (test-mevedel-usage--with-session
    (let* ((call (test-mevedel-usage--call session))
           (info (list :buffer data-buf :mevedel-usage-call call
                       :tokens '(:input 20 :cached 80 :output 2)))
           (result (list "response" "200" "OK")))
      (mevedel-telemetry-usage--parse-response (lambda (_info) result) info)
      (mevedel-telemetry-usage--callback
       call (lambda (_response _info) (kill-buffer data-buf)) nil "response" info)
      (mevedel-telemetry-usage--parse-response (lambda (_info) result) info)
      (should (= 1 (length (test-mevedel-usage--events session 'finish))))))

  :doc "response observation records parser exceptions once and rethrows unchanged"
  (test-mevedel-usage--with-session
    (let* ((call (test-mevedel-usage--call session))
           (info (list :mevedel-usage-call call :tokens nil)))
      (should (equal '(error "Synthetic response parser failure")
                     (should-error
                      (mevedel-telemetry-usage--parse-response
                       (lambda (_info) (error "Synthetic response parser failure")) info))))
      (let ((finishes (test-mevedel-usage--events session 'finish)))
        (should (= 1 (length finishes)))
        (should (eq 'error (plist-get (car finishes) :outcome)))
        (should (eq 'missing (plist-get (car finishes) :usage-status)))))))

(mevedel-deftest mevedel-telemetry-usage--abort ()
  ,test
  (test)
  :doc "abort observation ignores ordinary transitions and unowned explicit cancellation"
  (test-mevedel-usage--with-session
    (let* ((call (test-mevedel-usage--call session))
           (fsm (test-mevedel-usage--fsm data-buf :mevedel-usage-call call)))
      (dolist (state '(nil TYPE DONE ERRS))
        (mevedel-telemetry-usage--abort fsm state))
      (mevedel-telemetry-usage--abort (test-mevedel-usage--fsm data-buf) 'ABRT)
      (should-not (plist-get call :finished))
      (should-not (test-mevedel-usage--events session))))

  :doc "abort observation preserves partial usage once even when completion callback is muted"
  (test-mevedel-usage--with-session
    (let* ((call (test-mevedel-usage--call session :stream t))
           (fsm (test-mevedel-usage--fsm
                 data-buf :mevedel-usage-call call :callback #'ignore
                 :tokens '(:input 20 :cached 80)))
           (info (gptel-fsm-info fsm)))
      (mevedel-telemetry-usage--abort fsm 'ABRT)
      (mevedel-telemetry-usage--abort fsm 'ABRT)
      (mevedel-telemetry-usage--callback call #'ignore t 'abort info)
      (let ((finishes (test-mevedel-usage--events session 'finish)))
        (should (= 1 (length finishes)))
        (should (eq 'aborted (plist-get (car finishes) :outcome)))
        (should (eq 'partial (plist-get (car finishes) :usage-status)))
        (should (= 20 (plist-get (car finishes) :input-tokens)))
        (should (= 80 (plist-get (car finishes) :cached-tokens)))
        (should-not (plist-get (car finishes) :output-tokens))))))

(mevedel-deftest mevedel-telemetry-usage--dispatch ()
  ,test
  (test)
  :doc "cold dispatch needs neither the umbrella module nor an agent variable binding"
  (let ((emacs (expand-file-name invocation-name invocation-directory)))
    (with-temp-buffer
      (let ((status
             (call-process
              emacs nil t nil "--batch" "-Q" "--eval"
              (prin1-to-string
               `(progn
                  (setq load-path ',load-path)
                  (require 'mevedel-telemetry-usage)
                  (when (or (featurep 'mevedel) (featurep 'mevedel-agents)
                            (boundp 'mevedel--agent-invocation))
                    (error "Cold dispatch must start without agent setup"))
                  (let ((session (mevedel-session--create
                                  :name "cold" :session-id "cold-session" :turn-count 1))
                        (gptel-use-curl t)
                        (mevedel-telemetry-enabled t))
                    (with-temp-buffer
                      (setq-local mevedel--session session
                                  gptel-post-request-hook nil)
                      (let ((fsm (gptel-make-fsm
                                  :state 'WAIT :handlers nil
                                  :info (list :buffer (current-buffer)
                                              :callback #'ignore :model 'cold-model))))
                        (unwind-protect
                            (cl-letf (((symbol-function 'gptel-curl-get-response) #'ignore))
                              (mevedel-telemetry-usage-install)
                              (gptel--handle-wait fsm)
                              (let ((info (gptel-fsm-info fsm)))
                                (funcall (plist-get info :callback) "completed" info)))
                          (mevedel-telemetry-usage-uninstall))))
                    (let ((events (reverse (mevedel-session-telemetry-pending session))))
                      (unless (and (= 2 (length events))
                                   (eq 'start (plist-get (car events) :stage))
                                   (eq 'finish (plist-get (cadr events) :stage))
                                   (eq 'root (plist-get (car events) :workload))
                                   (eq 'success (plist-get (cadr events) :outcome)))
                        (error "Cold dispatch failed to capture root call"))))
                  (when (or (featurep 'mevedel) (featurep 'mevedel-agents)
                            (boundp 'mevedel--agent-invocation))
                    (error "Root observation must not load agent setup")))))))
        (ert-info ((buffer-string))
          (should (equal 0 status))
          (should (equal "" (buffer-string)))))))

  :doc "actual compaction gate defers call identity until its continuation reaches native dispatch"
  (test-mevedel-usage--with-session
    (test-mevedel-usage--with-observer
      (let* ((gptel-use-curl t)
             (sent 0)
             (fsm (test-mevedel-usage--fsm
                   data-buf :history '(TRET)
                   :mevedel-compaction-wait-decision
                   (list :admission '(:summary-policy nil :target-pressure nil)
                         :target-policy (list :backend gptel-backend
                                              :model 'mevedel-test-model))))
             callback)
        (cl-letf (((symbol-function 'gptel-curl-get-response)
                   (lambda (_machine) (cl-incf sent)))
                  ((symbol-function 'mevedel-compact-evidence-find-boundary)
                   (lambda () 1))
                  ((symbol-function 'mevedel-compact-target-main-target)
                   (lambda ()
                     (list :resume (lambda (_target machine)
                                     (gptel--handle-wait machine)))))
                  ((symbol-function 'mevedel-compact-run-start)
                   (lambda (&rest args)
                     (setq callback (plist-get args :callback)))))
          (mevedel--compact-handle-wait fsm)
          (should callback)
          (should (= 0 sent))
          (should-not (plist-get (gptel-fsm-info fsm) :mevedel-usage-call))
          (should-not (test-mevedel-usage--events session))
          (funcall callback nil)
          (should (= 1 sent))
          (should (plist-get (gptel-fsm-info fsm) :mevedel-usage-call))
          (should (= 1 (length (test-mevedel-usage--events session 'start))))
          (let ((info (gptel-fsm-info fsm)))
            (funcall (plist-get info :callback) "post-compaction response" info))
          (should (= 1 (length (test-mevedel-usage--events session 'finish))))))))

  :doc "dispatch gives identical native usage in multiple rounds distinct identities"
  (test-mevedel-usage--with-session
    (test-mevedel-usage--with-observer
      (let ((gptel-use-curl t)
            (fsm (test-mevedel-usage--fsm data-buf))
            (dispatches 0) (callbacks 0))
        (setf (plist-get (gptel-fsm-info fsm) :callback)
              (lambda (&rest _) (cl-incf callbacks)))
        (cl-letf (((symbol-function 'gptel-curl-get-response)
                   (lambda (machine)
                     (cl-incf dispatches)
                     ;; Real handle-wait must clear prior usage before transport.
                     (should-not (plist-get (gptel-fsm-info machine) :tokens)))))
          (dotimes (_ 2)
            (gptel--handle-wait fsm)
            (let ((info (gptel-fsm-info fsm)))
              (gptel--openai-update-tokens
               '(:prompt_tokens 100 :completion_tokens 2
                 :prompt_tokens_details (:cached_tokens 80)) info)
              (funcall (plist-get info :callback) "response" info)))
          (should (= 2 dispatches))
          (should (= 2 callbacks))
          (let ((starts (test-mevedel-usage--events session 'start))
                (finishes (test-mevedel-usage--events session 'finish)))
            (should (= 2 (length starts)))
            (should (= 2 (length finishes)))
            (should-not (equal (plist-get (car starts) :call-id)
                               (plist-get (cadr starts) :call-id)))
            (dotimes (index 2)
              (should (equal (plist-get (nth index starts) :call-id)
                             (plist-get (nth index finishes) :call-id)))
              (should (= 20 (plist-get (nth index finishes) :input-tokens)))
              (should (= 2 (plist-get (nth index finishes) :output-tokens))))
            (should (= 40 (plist-get (plist-get (gptel-fsm-info fsm) :tokens-full)
                                    :input))))))))

  :doc "native tool-loop transitions clear usage only after completion and isolate late callbacks"
  (test-mevedel-usage--with-session
    (test-mevedel-usage--with-observer
      (let* ((gptel-use-curl t)
             (fsm (test-mevedel-usage--fsm data-buf))
             (sends 0)
             first-callback)
        (setf (gptel-fsm-handlers fsm)
              (list (list 'WAIT #'gptel--handle-wait)
                    (list 'TOOL
                          (lambda (machine)
                            (should (= 1 (length (test-mevedel-usage--events
                                                 session 'finish))))
                            (plist-put (gptel-fsm-info machine) :tool-result '(ready))
                            (gptel--fsm-transition machine 'TRET)))
                    (list 'TRET #'gptel--fsm-transition)))
        (cl-letf (((symbol-function 'gptel-curl-get-response)
                   (lambda (_machine) (cl-incf sends))))
          (gptel--fsm-transition fsm 'WAIT)
          (let ((info (gptel-fsm-info fsm)))
            (setq first-callback (plist-get info :callback))
            (gptel--openai-update-tokens
             '(:prompt_tokens 100 :completion_tokens 2
               :prompt_tokens_details (:cached_tokens 80)) info)
            (plist-put info :tool-use '((:name "Synthetic")))
            (gptel--fsm-transition fsm 'TYPE)
            (funcall first-callback "first response" info)
            (gptel--fsm-transition fsm)
            (should (= 2 sends))
            (should (eq 'WAIT (gptel-fsm-state fsm)))
            (should-not (plist-get info :tokens))
            ;; A delayed callback from the earlier call must not complete the
            ;; new call, even though it is passed the reused INFO object.
            (funcall first-callback "late duplicate" info)
            (should (= 1 (length (test-mevedel-usage--events session 'finish))))
            (gptel--openai-update-tokens
             '(:prompt_tokens 100 :completion_tokens 2
               :prompt_tokens_details (:cached_tokens 80)) info)
            (gptel--fsm-transition fsm 'TYPE)
            (funcall (plist-get info :callback) "second response" info)
            (gptel--fsm-transition fsm)
            (should (eq 'DONE (gptel-fsm-state fsm)))
            (should (= 2 sends))
            (should (= 2 (length (test-mevedel-usage--events session 'finish)))))))))

  :doc "dispatch freezes request turn, backend/model and duration before callback work"
  (test-mevedel-usage--with-session
    (test-mevedel-usage--with-observer
      (let* ((gptel-use-curl t)
             (clock 100.0)
             (request (mevedel-request--create :id "request-1" :turn 8))
             (fsm (test-mevedel-usage--fsm
                   data-buf :mevedel-request request :model 'initial-model
                   :callback (lambda (&rest _) (setq clock 500.0)))))
        (cl-letf (((symbol-function 'gptel-curl-get-response) #'ignore)
                  ((symbol-function 'mevedel-telemetry--monotonic-now)
                   (lambda () clock)))
          (gptel--handle-wait fsm)
          (let ((info (gptel-fsm-info fsm)))
            (setf (mevedel-session-turn-count session) 99
                  (mevedel-request-turn request) 9)
            (plist-put info :model 'changed-model)
            (plist-put info :backend (gptel--make-openai :name "changed-backend"))
            (plist-put info :tokens '(:input 1 :cached 0 :output 2))
            (setq clock 100.125)
            (funcall (plist-get info :callback) "complete" info))
          (let ((events (test-mevedel-usage--events session)))
            (should (= 2 (length events)))
            (dolist (event events)
              (should (= 8 (plist-get event :turn)))
              (should (eq 'initial-model (plist-get event :model)))
              (should (equal (gptel-backend-name gptel-backend)
                             (plist-get event :backend)))
              (should (eq 'gptel-openai (plist-get event :backend-type)))
              (should (equal "request-1" (plist-get event :request-id))))
            (should (= 125 (plist-get (cadr events) :duration-ms))))))))

  :doc "dispatch separates concurrent root and retained agents including resumed turns"
  (test-mevedel-usage--with-session
    (test-mevedel-usage--with-observer
      (let ((gptel-use-curl t)
            (agent-buf (generate-new-buffer " *mevedel-usage-agent*")))
        (unwind-protect
            (let* ((invocation (mevedel-agent-invocation--create
                                :path "/root/worker" :parent-session session
                                :parent-turn 7 :buffer agent-buf))
                   (root-fsm (test-mevedel-usage--fsm data-buf))
                   (agent-fsm (test-mevedel-usage--fsm
                               agent-buf :mevedel-agent-invocation invocation
                               :mevedel-request-id "agent-request")))
              (with-current-buffer agent-buf
                (setq-local mevedel--agent-invocation invocation
                            gptel-post-request-hook nil))
              (cl-letf (((symbol-function 'gptel-curl-get-response) #'ignore))
                (gptel--handle-wait root-fsm)
                (gptel--handle-wait agent-fsm)
                ;; Settle in a different order to catch global-call ownership.
                (dolist (fsm (list agent-fsm root-fsm))
                  (let ((info (gptel-fsm-info fsm)))
                    (plist-put info :tokens '(:input 1 :cached 2 :output 3))
                    (funcall (plist-get info :callback) "response" info)))
                (setf (mevedel-agent-invocation-turn-count invocation) 2)
                (gptel--handle-wait agent-fsm)
                (let ((info (gptel-fsm-info agent-fsm)))
                  (funcall (plist-get info :callback) "resumed response" info)))
              (let ((events (test-mevedel-usage--events session 'finish)))
                (should (= 3 (length events)))
                (should (equal '(agent root agent)
                               (mapcar (lambda (entry) (plist-get entry :workload)) events)))
                (should (equal '("/root/worker" nil "/root/worker")
                               (mapcar (lambda (entry) (plist-get entry :agent-path)) events)))
                (should (= 3 (length (delete-dups
                                     (mapcar (lambda (entry) (plist-get entry :call-id)) events)))))))
          (when (buffer-live-p agent-buf) (kill-buffer agent-buf))))))

  :doc "dispatch classifies first agent dispatch before top-level invocation metadata is installed"
  (test-mevedel-usage--with-session
    (test-mevedel-usage--with-observer
      (let* ((gptel-use-curl t)
             (invocation (mevedel-agent-invocation--create
                          :path "/root/early" :parent-session session
                          :parent-turn 7 :buffer data-buf))
             (fsm (test-mevedel-usage--fsm data-buf)))
        (with-current-buffer data-buf
          (setq-local mevedel--session nil
                      mevedel--agent-invocation invocation))
        (should-not (plist-get (gptel-fsm-info fsm) :mevedel-agent-invocation))
        (cl-letf (((symbol-function 'gptel-curl-get-response) #'ignore))
          (gptel--handle-wait fsm))
        (let ((info (gptel-fsm-info fsm)))
          (funcall (plist-get info :callback) "agent response" info))
        (dolist (event (test-mevedel-usage--events session))
          (should (eq 'agent (plist-get event :workload)))
          (should (equal "/root/early" (plist-get event :agent-path)))))))

  :doc "dispatch labels session-owned naming and summary work without adopting sessionless work"
  (test-mevedel-usage--with-session
    (test-mevedel-usage--with-observer
      (let ((gptel-use-curl t))
        (with-temp-buffer
          (cl-letf (((symbol-function 'gptel-curl-get-response) #'ignore))
            (dolist (workload '(naming context-summary))
              (let* ((fsm (test-mevedel-usage--fsm
                           (current-buffer)
                           :context (list :mevedel-telemetry-session session
                                          :mevedel-telemetry-workload workload
                                          :purpose 'compaction)))
                     (info (gptel-fsm-info fsm)))
                (gptel--handle-wait fsm)
                (funcall (plist-get info :callback) "helper response" info)))
            (gptel--handle-wait (test-mevedel-usage--fsm (current-buffer))))
          (let ((events (test-mevedel-usage--events session 'finish)))
            (should (= 2 (length events)))
            (should (equal '(naming context-summary)
                           (mapcar (lambda (entry) (plist-get entry :workload)) events))))))))

  :doc "dispatch ignores cancellation before send, disabled telemetry and transient btw work"
  (test-mevedel-usage--with-session
    (test-mevedel-usage--with-observer
      (let ((gptel-use-curl t) (sent 0)
            (fsm (test-mevedel-usage--fsm data-buf)))
        ;; Cancellation before WAIT must not manufacture a call.
        (gptel--fsm-transition fsm 'ABRT)
        (should-not (test-mevedel-usage--events session))
        (cl-letf (((symbol-function 'gptel-curl-get-response)
                   (lambda (_fsm) (cl-incf sent))))
          (let ((mevedel-telemetry-enabled nil)) (gptel--handle-wait fsm))
          (should (eq #'ignore (plist-get (gptel-fsm-info fsm) :callback)))
          (with-current-buffer data-buf
            (setq-local mevedel--session
                        (mevedel-session--create :audit-session session)))
          (gptel--handle-wait fsm))
        (should (= 2 sent))
        (should-not (test-mevedel-usage--events session))
        (should-not (mevedel-session-telemetry-pending
                     (buffer-local-value 'mevedel--session data-buf))))))

  :doc "dispatch errors preserve earlier successful usage and make a failed retry its own call"
  (test-mevedel-usage--with-session
    (test-mevedel-usage--with-observer
      (let ((gptel-use-curl t)
            (fsm (test-mevedel-usage--fsm data-buf))
            (fail nil))
        (cl-letf (((symbol-function 'gptel-curl-get-response)
                   (lambda (_fsm) (when fail (error "Synthetic dispatch failure")))))
          (gptel--handle-wait fsm)
          (let ((info (gptel-fsm-info fsm)))
            (plist-put info :tokens '(:input 20 :cached 80 :output 2))
            (funcall (plist-get info :callback) "response" info))
          (setq fail t)
          (should-error (gptel--handle-wait fsm))
          (setq fail nil)
          (gptel--handle-wait fsm)
          (let ((info (gptel-fsm-info fsm)))
            (funcall (plist-get info :callback) "retried response" info)))
        (let ((events (test-mevedel-usage--events session 'finish)))
          (should (= 3 (length events)))
          (should (equal '(success error success)
                         (mapcar (lambda (entry) (plist-get entry :outcome)) events)))
          (should (= 20 (plist-get (car events) :input-tokens)))
          (should (eq 'missing (plist-get (cadr events) :usage-status)))
          (should-not (plist-get (cadr events) :input-tokens))))))

  :doc "stale transport registration cannot hide a subsequent failed dispatch"
  (test-mevedel-usage--with-session
    (test-mevedel-usage--with-observer
      (let ((gptel-use-curl t)
            (fsm (test-mevedel-usage--fsm data-buf)))
        (cl-letf (((symbol-function 'gptel-curl-get-response)
                   (lambda (machine)
                     (setf (alist-get 'old-transport gptel--request-alist)
                           (cons machine #'ignore)))))
          (gptel--handle-wait fsm))
        (let ((info (gptel-fsm-info fsm)))
          (plist-put info :tokens '(:input 20 :cached 80 :output 2))
          (funcall (plist-get info :callback) "first response" info))
        ;; Native cleanup can dispatch again before removing the old transport.
        ;; This second dispatch never registers its own transport.
        (cl-letf (((symbol-function 'gptel-curl-get-response)
                   (lambda (_machine) (error "Synthetic next dispatch failure"))))
          (should (equal '(error "Synthetic next dispatch failure")
                         (should-error (gptel--handle-wait fsm)))))
        (let ((starts (test-mevedel-usage--events session 'start))
              (finishes (test-mevedel-usage--events session 'finish)))
          (should (= 2 (length starts)))
          (should (= 2 (length finishes)))
          (should (equal '(success error)
                         (mapcar (lambda (event) (plist-get event :outcome)) finishes)))
          (should (equal (plist-get (cadr starts) :call-id)
                         (plist-get (cadr finishes) :call-id)))
          (should (eq 'missing (plist-get (cadr finishes) :usage-status)))
          (should-not (plist-get (cadr finishes) :input-tokens))))))

  :doc "new transport registration beside a stale one remains active after a hook error"
  (test-mevedel-usage--with-session
    (test-mevedel-usage--with-observer
      (let ((gptel-use-curl t)
            (fsm (test-mevedel-usage--fsm data-buf)))
        (setf (alist-get 'old-transport gptel--request-alist) (cons fsm #'ignore))
        (with-current-buffer data-buf
          (setq-local gptel-post-request-hook
                      (list (lambda () (error "Synthetic new registered hook failure")))))
        (cl-letf (((symbol-function 'gptel-curl-get-response)
                   (lambda (machine)
                     (setf (alist-get 'new-transport gptel--request-alist)
                           (cons machine #'ignore)))))
          (should (equal '(error "Synthetic new registered hook failure")
                         (should-error (gptel--handle-wait fsm)))))
        (should-not (test-mevedel-usage--events session 'finish))
        (let ((info (gptel-fsm-info fsm)))
          (plist-put info :tokens '(:input 20 :cached 80 :output 2))
          (funcall (plist-get info :callback) "later response" info))
        (let ((finishes (test-mevedel-usage--events session 'finish)))
          (should (= 1 (length finishes)))
          (should (eq 'success (plist-get (car finishes) :outcome)))
          (should (= 20 (plist-get (car finishes) :input-tokens)))))))

  :doc "replacing a stale transport under the same key counts as a new active registration"
  (test-mevedel-usage--with-session
    (test-mevedel-usage--with-observer
      (let ((gptel-use-curl t)
            (fsm (test-mevedel-usage--fsm data-buf)))
        (setf (alist-get 'reused-key gptel--request-alist) (cons fsm #'ignore))
        (with-current-buffer data-buf
          (setq-local gptel-post-request-hook
                      (list (lambda () (error "Synthetic reused registration hook failure")))))
        (cl-letf (((symbol-function 'gptel-curl-get-response)
                   (lambda (machine)
                     (setf (alist-get 'reused-key gptel--request-alist)
                           (cons machine #'ignore)))))
          (should (equal '(error "Synthetic reused registration hook failure")
                         (should-error (gptel--handle-wait fsm)))))
        (should-not (test-mevedel-usage--events session 'finish))
        (let ((info (gptel-fsm-info fsm)))
          (plist-put info :tokens '(:input 20 :cached 80 :output 2))
          (funcall (plist-get info :callback) "later response" info))
        (let ((finishes (test-mevedel-usage--events session 'finish)))
          (should (= 1 (length finishes)))
          (should (eq 'success (plist-get (car finishes) :outcome)))
          (should (= 20 (plist-get (car finishes) :input-tokens)))))))

  :doc "dispatch reuses native transport defaults and never nests old callback wrappers"
  (test-mevedel-usage--with-session
    (test-mevedel-usage--with-observer
      (dolist (case '((t t gptel-curl--stream-insert-response)
                      (t nil gptel--insert-response)
                      (nil t gptel--insert-response)))
        (pcase-let ((`(,gptel-use-curl ,stream ,default) case))
          (let ((fsm (test-mevedel-usage--fsm data-buf :stream stream :callback nil)))
            (cl-letf (((symbol-function 'gptel-curl-get-response) #'ignore)
                      ((symbol-function 'gptel--url-get-response) #'ignore))
              (gptel--handle-wait fsm)
              (let* ((info (gptel-fsm-info fsm))
                     (old-wrapper (plist-get info :callback)))
                (should (eq default (plist-get info :mevedel-usage-original)))
                (gptel--handle-wait fsm)
                (should-not (eq old-wrapper (plist-get info :callback)))
                (should (eq default (plist-get info :mevedel-usage-original))))))))))

  :doc "native curl stream parser and cleanup settle only after the final usage chunk"
  (test-mevedel-usage--with-session
    (test-mevedel-usage--with-observer
      (let* ((gptel-use-curl t)
             (proc-buf (generate-new-buffer " *mevedel-usage-curl-stream*"))
             (process (make-pipe-process :name "usage-stream" :buffer proc-buf
                                         :noquery t :sentinel #'ignore))
             (received nil)
             (fsm (test-mevedel-usage--fsm
                   data-buf :stream t
                   :callback (lambda (response _info) (push response received)))))
        (unwind-protect
            (progn
              (cl-letf (((symbol-function 'gptel-curl-get-response) #'ignore))
                (gptel--handle-wait fsm))
              (setf (alist-get process gptel--request-alist) (cons fsm #'ignore))
              (gptel-curl--stream-filter
               process
               (concat "HTTP/1.1 200 OK\r\n\r\n"
                       "data: {\"choices\":[{\"delta\":{\"content\":\"chunk\"}}]}\n\n"))
              (should (member "chunk" received))
              (should-not (test-mevedel-usage--events session 'finish))
              (gptel-curl--stream-filter
               process
               (concat "data: {\"choices\":[],\"usage\":{\"prompt_tokens\":100,"
                       "\"completion_tokens\":2,\"prompt_tokens_details\":{\"cached_tokens\":80}}}\n\n"
                       "data: [DONE]\n\n"))
              (should-not (test-mevedel-usage--events session 'finish))
              (cl-letf (((symbol-function 'process-exit-status) (lambda (_process) 0)))
                (gptel-curl--stream-cleanup process "finished"))
              (let ((events (test-mevedel-usage--events session 'finish)))
                (should (= 1 (length events)))
                (should (= 20 (plist-get (car events) :input-tokens)))
                (should (= 80 (plist-get (car events) :cached-tokens))))
              (should (eq t (car received))))
          (when (process-live-p process) (delete-process process))
          (when (buffer-live-p proc-buf) (kill-buffer proc-buf))))))

  :doc "native curl cleanup records a transport failure without fabricating usage"
  (test-mevedel-usage--with-session
    (test-mevedel-usage--with-observer
      (let* ((gptel-use-curl t)
             (proc-buf (generate-new-buffer " *mevedel-usage-curl-error*"))
             (process (make-pipe-process :name "usage-error" :buffer proc-buf
                                         :noquery t :sentinel #'ignore))
             (fsm (test-mevedel-usage--fsm data-buf :stream t)))
        (unwind-protect
            (progn
              (cl-letf (((symbol-function 'gptel-curl-get-response) #'ignore))
                (gptel--handle-wait fsm))
              (setf (alist-get process gptel--request-alist) (cons fsm #'ignore))
              (cl-letf (((symbol-function 'process-exit-status) (lambda (_process) 7)))
                (gptel-curl--stream-cleanup process "failed"))
              (let ((events (test-mevedel-usage--events session 'finish)))
                (should (= 1 (length events)))
                (should (eq 'error (plist-get (car events) :outcome)))
                (should (eq 'missing (plist-get (car events) :usage-status)))
                (should-not (plist-get (car events) :output-tokens))))
          (when (process-live-p process) (delete-process process))
          (when (buffer-live-p proc-buf) (kill-buffer proc-buf))))))

  :doc "native streaming curl failure after HTTP200 and a chunk cannot settle as success"
  (test-mevedel-usage--with-session
    (test-mevedel-usage--with-observer
      (let* ((gptel-use-curl t)
             (proc-buf (generate-new-buffer " *mevedel-usage-partial-stream*"))
             (process (make-pipe-process :name "usage-partial" :buffer proc-buf
                                         :noquery t :sentinel #'ignore))
             (fsm (test-mevedel-usage--fsm data-buf :stream t)))
        (unwind-protect
            (progn
              (cl-letf (((symbol-function 'gptel-curl-get-response) #'ignore))
                (gptel--handle-wait fsm))
              (setf (alist-get process gptel--request-alist) (cons fsm #'ignore))
              (gptel-curl--stream-filter
               process
               (concat "HTTP/1.1 200 OK\r\n\r\n"
                       "data: {\"choices\":[{\"delta\":{\"content\":\"partial chunk\"}}]}\n\n"))
              (should (eq 'TYPE (gptel-fsm-state fsm)))
              (should-not (test-mevedel-usage--events session 'finish))
              (cl-letf (((symbol-function 'process-exit-status) (lambda (_process) 18)))
                (gptel-curl--stream-cleanup process "failed"))
              (let ((events (test-mevedel-usage--events session 'finish)))
                (should (= 1 (length events)))
                (should (eq 'error (plist-get (car events) :outcome)))
                (should (eq 'missing (plist-get (car events) :usage-status)))
                (should-not (plist-get (car events) :output-tokens))))
          (when (process-live-p process) (delete-process process))
          (when (buffer-live-p proc-buf) (kill-buffer proc-buf))))))

  :doc "pre-ec25a41 pending-tool stream failure belongs to call one despite reentrant call two dispatch"
  (test-mevedel-usage--with-session
    (test-mevedel-usage--with-observer
      (let* ((gptel-use-curl t)
             (proc-buf (generate-new-buffer " *mevedel-usage-reentrant*"))
             (process (make-pipe-process :name "usage-reentrant" :buffer proc-buf
                                         :noquery t :sentinel #'ignore))
             (next-buf (generate-new-buffer " *mevedel-usage-reentrant-next*"))
             (next-process (make-pipe-process :name "usage-reentrant-next" :buffer next-buf
                                              :noquery t :sentinel #'ignore))
             (received nil)
             (sends 0)
             (fsm (test-mevedel-usage--fsm
                   data-buf :stream t
                   :callback (lambda (response _info) (push response received)))))
        (unwind-protect
            (cl-letf (((symbol-function 'gptel-curl-get-response)
                       (lambda (machine)
                         (cl-incf sends)
                         (setf (alist-get (if (= sends 1) process next-process)
                                          gptel--request-alist)
                               (cons machine #'ignore)))))
              (setf (gptel-fsm-handlers fsm)
                    (list (list 'WAIT #'gptel--handle-wait)
                          (list 'TOOL
                                (lambda (machine)
                                  (should (= 1 (length (test-mevedel-usage--events
                                                       session 'finish))))
                                  (gptel--fsm-transition machine 'WAIT)))))
              (gptel--handle-wait fsm)
              (gptel-curl--stream-filter
               process
               (concat "HTTP/1.1 200 OK\r\n\r\n"
                       "data: {\"choices\":[{\"delta\":{\"tool_calls\":[{\"index\":0,"
                       "\"id\":\"tool-1\",\"type\":\"function\",\"function\":{"
                       "\"name\":\"Synthetic\",\"arguments\":\"{}\"}}]}}]}\n\n"
                       "data: {\"choices\":[],\"usage\":{\"prompt_tokens\":100,"
                       "\"completion_tokens\":2,\"prompt_tokens_details\":{\"cached_tokens\":80}}}\n\n"
                       "data: [DONE]\n\n"))
              (should (plist-get (gptel-fsm-info fsm) :tool-use))
              (should-not (test-mevedel-usage--events session 'finish))
              ;; Current gptel assigns the error first and never re-enters, so
              ;; drive the observer around the older ordering it still guards.
              (cl-letf (((symbol-function 'process-exit-status) (lambda (_process) 18)))
                (mevedel-telemetry-usage--stream-cleanup
                 #'test-mevedel-usage--pre-ec25a41-curl-failure process "failed"))
              (should (= 2 sends))
              ;; Callback forwarding remains native even though this old error
              ;; arrives through the newly installed callback in shared INFO.
              (should (member nil received))
              (let ((starts (test-mevedel-usage--events session 'start))
                    (finishes (test-mevedel-usage--events session 'finish)))
                (should (= 2 (length starts)))
                (should (= 1 (length finishes)))
                (should (equal (plist-get (car starts) :call-id)
                               (plist-get (car finishes) :call-id)))
                (should (eq 'error (plist-get (car finishes) :outcome)))
                (should (= 20 (plist-get (car finishes) :input-tokens)))
                (should (= 80 (plist-get (car finishes) :cached-tokens))))
              (let* ((info (gptel-fsm-info fsm))
                     (next-call (plist-get info :mevedel-usage-call)))
                (should-not (plist-get next-call :finished))
                ;; The old native cleanup wrote its error after the next
                ;; handle-wait reset.  Do not clear it or shield error reads:
                ;; the new transport's own outcome must be authoritative.
                (should (plist-get info :error))
                (should (eq fsm (cadr (assq next-process gptel--request-alist))))
                (gptel-curl--stream-filter
                 next-process
                 (concat "HTTP/1.1 200 OK\r\n\r\n"
                         "data: {\"choices\":[{\"delta\":{\"content\":\"next chunk\"}}]}\n\n"
                         "data: {\"choices\":[],\"usage\":{\"prompt_tokens\":14,"
                         "\"completion_tokens\":5,\"prompt_tokens_details\":{\"cached_tokens\":4}}}\n\n"
                         "data: [DONE]\n\n"))
                (should (plist-get info :error))
                (should-not (plist-get next-call :finished))
                (cl-letf (((symbol-function 'process-exit-status) (lambda (_process) 0)))
                  (gptel-curl--stream-cleanup next-process "finished")))
              (let ((starts (test-mevedel-usage--events session 'start))
                    (finishes (test-mevedel-usage--events session 'finish)))
                (should (= 2 (length finishes)))
                (should (equal (plist-get (cadr starts) :call-id)
                               (plist-get (cadr finishes) :call-id)))
                (should (eq 'success (plist-get (cadr finishes) :outcome)))
                (should (= 10 (plist-get (cadr finishes) :input-tokens)))
                (should (= 4 (plist-get (cadr finishes) :cached-tokens)))
                (should (= 5 (plist-get (cadr finishes) :output-tokens)))
                (should-not gptel--request-alist)))
          (when (process-live-p process) (delete-process process))
          (when (buffer-live-p proc-buf) (kill-buffer proc-buf))
          (when (process-live-p next-process) (delete-process next-process))
          (when (buffer-live-p next-buf) (kill-buffer next-buf))))))

  :doc "native pending-tool stream failure ends call one as an error without another dispatch"
  (test-mevedel-usage--with-session
    (test-mevedel-usage--with-observer
      (let* ((gptel-use-curl t)
             (proc-buf (generate-new-buffer " *mevedel-usage-pending-tool*"))
             (process (make-pipe-process :name "usage-pending-tool" :buffer proc-buf
                                         :noquery t :sentinel #'ignore))
             (sends 0)
             (fsm (test-mevedel-usage--fsm data-buf :stream t)))
        (unwind-protect
            (cl-letf (((symbol-function 'gptel-curl-get-response)
                       (lambda (machine)
                         (cl-incf sends)
                         (setf (alist-get process gptel--request-alist)
                               (cons machine #'ignore)))))
              (setf (gptel-fsm-handlers fsm)
                    (list (list 'WAIT #'gptel--handle-wait)
                          (list 'TOOL (lambda (machine)
                                        (gptel--fsm-transition machine 'WAIT)))))
              (gptel--handle-wait fsm)
              (gptel-curl--stream-filter
               process
               (concat "HTTP/1.1 200 OK\r\n\r\n"
                       "data: {\"choices\":[{\"delta\":{\"tool_calls\":[{\"index\":0,"
                       "\"id\":\"tool-1\",\"type\":\"function\",\"function\":{"
                       "\"name\":\"Synthetic\",\"arguments\":\"{}\"}}]}}]}\n\n"
                       "data: {\"choices\":[],\"usage\":{\"prompt_tokens\":100,"
                       "\"completion_tokens\":2,\"prompt_tokens_details\":{\"cached_tokens\":80}}}\n\n"
                       "data: [DONE]\n\n"))
              (should (plist-get (gptel-fsm-info fsm) :tool-use))
              (cl-letf (((symbol-function 'process-exit-status) (lambda (_process) 18)))
                (gptel-curl--stream-cleanup process "failed"))
              (should (= 1 sends))
              (let ((history (plist-get (gptel-fsm-info fsm) :history)))
                (should (memq 'ERRS history))
                (should-not (memq 'TOOL history)))
              (should-not gptel--request-alist)
              (let ((starts (test-mevedel-usage--events session 'start))
                    (finishes (test-mevedel-usage--events session 'finish)))
                (should (= 1 (length starts)))
                (should (= 1 (length finishes)))
                (should (eq 'error (plist-get (car finishes) :outcome)))
                (should (= 20 (plist-get (car finishes) :input-tokens)))
                (should (= 80 (plist-get (car finishes) :cached-tokens)))))
          (when (process-live-p process) (delete-process process))
          (when (buffer-live-p proc-buf) (kill-buffer proc-buf))))))

  :doc "native curl sentinel captures text and tool-only usage before the TOOL boundary"
  (test-mevedel-usage--with-session
    (test-mevedel-usage--with-observer
      (dolist (tool-only '(nil t))
        (let* ((gptel-use-curl t)
               (proc-buf (generate-new-buffer " *mevedel-usage-curl*"))
               (process (make-pipe-process :name "usage-curl" :buffer proc-buf
                                           :noquery t :sentinel #'ignore))
               (fsm (test-mevedel-usage--fsm data-buf :uuid "usage-footer")))
          (unwind-protect
              (progn
                (when tool-only
                  (setf (gptel-fsm-handlers fsm)
                        (list (list 'TOOL
                                    (lambda (_machine)
                                      ;; No response callback fires for this
                                      ;; transport outcome.  Still settle before
                                      ;; tool execution could clear usage.
                                      (should (= 2 (length (test-mevedel-usage--events
                                                           session 'finish)))))))))
                (cl-letf (((symbol-function 'gptel-curl-get-response) #'ignore))
                  (gptel--handle-wait fsm))
                (with-current-buffer proc-buf
                  (insert "HTTP/1.1 200 OK\r\nContent-Type: application/json\r\n\r\n")
                  (let ((header-size (1- (point))))
                    (insert (test-mevedel-usage--json-response tool-only))
                    (insert (format "\n(usage-footer . %d)" header-size))))
                (setf (alist-get process gptel--request-alist) (cons fsm #'ignore))
                (cl-letf (((symbol-function 'process-status) (lambda (_process) 'exit))
                          ((symbol-function 'process-exit-status) (lambda (_process) 0)))
                  (gptel-curl--sentinel process "finished"))
                (should (eq (if tool-only 'TOOL 'DONE) (gptel-fsm-state fsm)))
                (let ((events (test-mevedel-usage--events session 'finish)))
                  (should (= (if tool-only 2 1) (length events)))
                  (should (= 20 (plist-get (car (last events)) :input-tokens)))
                  (should (eq 'success (plist-get (car (last events)) :outcome)))))
            (when (process-live-p process) (delete-process process))
            (when (buffer-live-p proc-buf) (kill-buffer proc-buf)))))))

  :doc "native URL transport captures successful text and callback-free tool-only usage"
  (test-mevedel-usage--with-session
    (test-mevedel-usage--with-observer
      (dolist (tool-only '(nil t))
        (let* ((gptel-use-curl nil)
               (proc-buf (generate-new-buffer " *mevedel-usage-url*"))
               (fsm (test-mevedel-usage--fsm data-buf))
               callback)
          (unwind-protect
              (progn
                (when tool-only
                  (setf (gptel-fsm-handlers fsm)
                        (list (list 'TOOL
                                    (lambda (_machine)
                                      (should (= 2 (length (test-mevedel-usage--events
                                                           session 'finish)))))))))
                (cl-letf (((symbol-function 'url-retrieve)
                           (lambda (_url receive &rest _args)
                             (setq callback receive)
                             proc-buf)))
                  (gptel--handle-wait fsm))
                (with-current-buffer proc-buf
                  (insert "HTTP/1.1 200 OK\r\nContent-Type: application/json\r\n\r\n")
                  (setq-local url-http-end-of-headers (copy-marker (point))
                              url-http-response-status 200)
                  (insert (test-mevedel-usage--json-response tool-only))
                  (goto-char (point-min))
                  (funcall callback nil))
                (should (eq (if tool-only 'TOOL 'DONE) (gptel-fsm-state fsm)))
                (let ((events (test-mevedel-usage--events session 'finish)))
                  (should (= (if tool-only 2 1) (length events)))
                  (should (= 20 (plist-get (car (last events)) :input-tokens)))
                  (should (= 80 (plist-get (car (last events)) :cached-tokens)))))
            (when (buffer-live-p proc-buf) (kill-buffer proc-buf)))))))

  :doc "URL transport survives a post-request hook error and settles later provider usage"
  (test-mevedel-usage--with-session
    (test-mevedel-usage--with-observer
      (let* ((gptel-use-curl nil)
             (proc-buf (generate-new-buffer " *mevedel-usage-hook-error*"))
             (fsm (test-mevedel-usage--fsm data-buf))
             callback)
        (unwind-protect
            (progn
              (with-current-buffer data-buf
                (setq-local gptel-post-request-hook
                            (list (lambda ()
                                    (error "Synthetic post-request hook failure")))))
              (cl-letf (((symbol-function 'url-retrieve)
                         (lambda (_url receive &rest _args)
                           (setq callback receive)
                           proc-buf)))
                (should (equal '(error "Synthetic post-request hook failure")
                               (should-error (gptel--handle-wait fsm)))))
              (should callback)
              (should (eq fsm (cadr (assq proc-buf gptel--request-alist))))
              (should (= 1 (length (test-mevedel-usage--events session 'start))))
              (should-not (test-mevedel-usage--events session 'finish))
              (should-not (plist-get (plist-get (gptel-fsm-info fsm)
                                               :mevedel-usage-call)
                                     :finished))
              (with-current-buffer proc-buf
                (insert "HTTP/1.1 200 OK\r\nContent-Type: application/json\r\n\r\n")
                (setq-local url-http-end-of-headers (copy-marker (point))
                            url-http-response-status 200)
                (insert (test-mevedel-usage--json-response))
                (goto-char (point-min))
                (funcall callback nil))
              (let ((events (test-mevedel-usage--events session 'finish)))
                (should (= 1 (length events)))
                (should (eq 'success (plist-get (car events) :outcome)))
                (should (= 20 (plist-get (car events) :input-tokens)))
                (should (= 80 (plist-get (car events) :cached-tokens))))
              (should-not gptel--request-alist))
          (when (buffer-live-p proc-buf) (kill-buffer proc-buf))))))

  :doc "native abort retains partial usage after retained-agent callback muting and cancels transport"
  (test-mevedel-usage--with-session
    (test-mevedel-usage--with-observer
      (let* ((gptel-use-curl t)
             (proc-buf (generate-new-buffer " *mevedel-usage-muted-abort*"))
             (process (make-pipe-process :name "usage-muted-abort" :buffer proc-buf
                                         :noquery t :sentinel #'ignore))
             (fsm (test-mevedel-usage--fsm data-buf :stream t))
             saved-callback messages)
        (unwind-protect
            (progn
              (cl-letf (((symbol-function 'gptel-curl-get-response)
                         (lambda (machine)
                           (setf (alist-get process gptel--request-alist)
                                 (cons machine
                                       (lambda ()
                                         (set-process-sentinel process #'ignore)
                                         (delete-process process)))))))
                (gptel--handle-wait fsm))
              (let ((info (gptel-fsm-info fsm)))
                (setq saved-callback (plist-get info :callback))
                (plist-put info :tokens '(:input 20 :cached 80))
                ;; Faithfully mirror retained-agent interruption, which mutes
                ;; callbacks before native gptel-abort cancels its process.
                (plist-put info :callback #'ignore)
                (mevedel-test--with-captured-messages messages
                  (gptel-abort data-buf))
                (should messages)
                (should (eq 'ABRT (gptel-fsm-state fsm)))
                (should-not (process-live-p process))
                (should-not gptel--request-alist)
                (funcall saved-callback 'abort info)
                (let ((finishes (test-mevedel-usage--events session 'finish)))
                  (should (= 1 (length finishes)))
                  (should (eq 'aborted (plist-get (car finishes) :outcome)))
                  (should (eq 'partial (plist-get (car finishes) :usage-status)))
                  (should (= 20 (plist-get (car finishes) :input-tokens)))
                  (should (= 80 (plist-get (car finishes) :cached-tokens)))
                  (should-not (plist-get (car finishes) :output-tokens)))))
          (when (process-live-p process) (delete-process process))
          (when (buffer-live-p proc-buf) (kill-buffer proc-buf))))))

  :doc "native abort settles an in-flight call once and releases its transport"
  (test-mevedel-usage--with-session
    (test-mevedel-usage--with-observer
      (let* ((gptel-use-curl t)
             (fsm (test-mevedel-usage--fsm data-buf))
             (cancelled nil) messages)
        (cl-letf (((symbol-function 'gptel-curl-get-response) #'ignore))
          (gptel--handle-wait fsm))
        (setf (alist-get 'synthetic-transport gptel--request-alist)
              (cons fsm (lambda () (setq cancelled t))))
        (mevedel-test--with-captured-messages messages
          (gptel-abort data-buf))
        (should messages)
        (should cancelled)
        (should-not gptel--request-alist)
        (should (eq 'ABRT (gptel-fsm-state fsm)))
        (let ((info (gptel-fsm-info fsm)))
          (funcall (plist-get info :callback) 'abort info))
        (let ((events (test-mevedel-usage--events session 'finish)))
          (should (= 1 (length events)))
          (should (eq 'aborted (plist-get (car events) :outcome)))
          (should (eq 'missing (plist-get (car events) :usage-status))))))))

(mevedel-deftest mevedel-telemetry-usage-install ()
  ,test
  (test)
  :doc "installation is idempotent and can be restored after each test"
  (test-mevedel-usage--with-observer
    (mevedel-telemetry-usage-uninstall)
    (mevedel-telemetry-usage-install)
    (let ((installed (symbol-function 'gptel--handle-wait))
          (transition (symbol-function 'gptel--fsm-transition))
          (curl-parser (symbol-function 'gptel-curl--parse-response))
          (url-parser (symbol-function 'gptel--url-parse-response))
          (cleanup (symbol-function 'gptel-curl--stream-cleanup))
          (parser (symbol-function 'gptel-curl--parse-stream)))
      (mevedel-telemetry-usage-install)
      (should (eq installed (symbol-function 'gptel--handle-wait)))
      (should (eq transition (symbol-function 'gptel--fsm-transition)))
      (should (eq curl-parser (symbol-function 'gptel-curl--parse-response)))
      (should (eq url-parser (symbol-function 'gptel--url-parse-response)))
      (should (eq cleanup (symbol-function 'gptel-curl--stream-cleanup)))
      (should (eq parser (symbol-function 'gptel-curl--parse-stream)))
      (should (advice-member-p #'mevedel-telemetry-usage--dispatch
                              'gptel--handle-wait))
      (dolist (function '(gptel-curl--parse-response gptel--url-parse-response))
        (should (advice-member-p #'mevedel-telemetry-usage--parse-response function)))
      (should (advice-member-p #'mevedel-telemetry-usage--stream-cleanup
                              'gptel-curl--stream-cleanup))
      (should (advice-member-p #'mevedel-telemetry-usage--parse-stream
                              'gptel-curl--parse-stream))
      (should (advice-member-p #'mevedel-telemetry-usage--abort
                              'gptel--fsm-transition))))

  :doc "observer fixture restores each originally present advice even when its body throws"
  (test-mevedel-usage--with-observer
    (mevedel-telemetry-usage-uninstall)
    ;; Mixed presence catches restoration that accidentally installs every
    ;; observation seam if any single one was originally present.
    (advice-add 'gptel-curl--stream-cleanup :around
                #'mevedel-telemetry-usage--stream-cleanup)
    (advice-add 'gptel-curl--parse-stream :around
                #'mevedel-telemetry-usage--parse-stream)
    (advice-add 'gptel-curl--parse-response :around
                #'mevedel-telemetry-usage--parse-response)
    (should (equal '(error "Synthetic observer fixture failure")
                   (should-error
                    (test-mevedel-usage--with-observer
                      (error "Synthetic observer fixture failure")))))
    (should-not (advice-member-p #'mevedel-telemetry-usage--dispatch
                                'gptel--handle-wait))
    (should-not (advice-member-p #'mevedel-telemetry-usage--abort
                                'gptel--fsm-transition))
    (should (advice-member-p #'mevedel-telemetry-usage--parse-response
                            'gptel-curl--parse-response))
    (should-not (advice-member-p #'mevedel-telemetry-usage--parse-response
                                'gptel--url-parse-response))
    (should (advice-member-p #'mevedel-telemetry-usage--stream-cleanup
                            'gptel-curl--stream-cleanup))
    (should (advice-member-p #'mevedel-telemetry-usage--parse-stream
                            'gptel-curl--parse-stream))))

(mevedel-deftest mevedel-telemetry-usage-uninstall ()
  ,test
  (test)
  :doc "uninstallation is idempotent and does not discard already dispatched completion"
  (test-mevedel-usage--with-session
    (test-mevedel-usage--with-observer
      (let ((gptel-use-curl t)
            (fsm (test-mevedel-usage--fsm data-buf)))
        (cl-letf (((symbol-function 'gptel-curl-get-response) #'ignore))
          (gptel--handle-wait fsm))
        (mevedel-telemetry-usage-uninstall)
        (mevedel-telemetry-usage-uninstall)
        (should-not (advice-member-p #'mevedel-telemetry-usage--dispatch
                                    'gptel--handle-wait))
        (should-not (advice-member-p #'mevedel-telemetry-usage--abort
                                    'gptel--fsm-transition))
        (dolist (function '(gptel-curl--parse-response gptel--url-parse-response))
          (should-not (advice-member-p #'mevedel-telemetry-usage--parse-response function)))
        (should-not (advice-member-p #'mevedel-telemetry-usage--stream-cleanup
                                    'gptel-curl--stream-cleanup))
        (should-not (advice-member-p #'mevedel-telemetry-usage--parse-stream
                                    'gptel-curl--parse-stream))
        (let ((info (gptel-fsm-info fsm)))
          (funcall (plist-get info :callback) "finished" info))
        (should (= 1 (length (test-mevedel-usage--events session 'finish))))))))

(provide 'test-mevedel-telemetry-usage)
;;; test-mevedel-telemetry-usage.el ends here
